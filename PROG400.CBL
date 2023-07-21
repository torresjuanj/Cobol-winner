      *----------------------------------------------------------------*
      * MEMBRESIA ABR-27-2002                                          *
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG400.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\DYC\CONTREXT.DYC".
           COPY "..\DYC\CONCPTOS.DYC".
           COPY "..\DYC\PRFESION.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\AUDITE.DYC".
           COPY "..\DYC\TABLAEMP.DYC".
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\NOMISEXO.DYC".
           COPY "..\DYC\ESTCIVIL.DYC".
           COPY "..\DYC\DINOMBRE.DYC".
           COPY "..\DYC\MEMBRSIA.DYC".
           COPY "..\DYC\DCTOIDEN.DYC".
           COPY "..\DYC\TIPOVINC.DYC".
           COPY "..\DYC\PARENTCO.DYC".
           COPY "..\DYC\CONTDPTO.DYC".
           COPY "..\DYC\TIPONACI.DYC".
           COPY "..\DYC\AUDICAMP.DYC".
           COPY "..\DYC\HISNROME.DYC".
           COPY "..\DYC\NOMICIUD.DYC".
           COPY "..\SEL\INFORME2.SEL".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\CNNITCED.FD".
           COPY "..\FD\CONTREXT.FD".
           COPY "..\FD\CONCPTOS.FD".
           COPY "..\FD\PRFESION.FD".
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\AUDITE.FD".
           COPY "..\FD\TABLAEMP.FD".
           COPY "..\FD\USER-ACT.FD".
           COPY "..\FD\NOMISEXO.FD".
           COPY "..\FD\ESTCIVIL.FD".
           COPY "..\FD\DINOMBRE.FD".
           COPY "..\FD\MEMBRSIA.FD".
           COPY "..\FD\DCTOIDEN.FD".
           COPY "..\FD\TIPOVINC.FD".
           COPY "..\FD\PARENTCO.FD".
           COPY "..\FD\CONTDPTO.FD".
           COPY "..\FD\TIPONACI.FD".
           COPY "..\FD\AUDICAMP.FD".
           COPY "..\FD\HISNROME.FD".
           COPY "..\FD\NOMICIUD.FD".
           COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77  ID-PRG                     PIC X(7)  VALUE "PROG400".
       77  W-PROGRAMA                 PIC X(7)  VALUE "PROG400".
       77  WKK-DIA                    PIC S99   VALUE  ZEROS.
       77  WK-FPENSION                PIC 9(3)  VALUE  ZEROS.
       77  JD-6                       PIC ZZZZ,ZZZ,ZZZ.
       77  JD-7                       PIC ZZZ,ZZ9-.
       77  WK-PROMSALU                PIC 9(3)  VALUE  ZEROS.
       77  WK-FONCESAN                PIC 9(3)  VALUE  ZEROS.
       77  WK-CENTRIPS                PIC 9(3)  VALUE  ZEROS.
       77  WK-TIPREGIM                PIC 9(3)  VALUE  ZEROS.
       77  WK-ADMRIEPR                PIC 9(3)  VALUE  ZEROS.
       77  SW-CUENTA                  PIC 9     VALUE ZEROS.
       77  SW-HELP-UPZCAA             PIC 9     VALUE ZEROS.
       77  CLAVE-FEC-SFP              PIC X(4)  VALUE SPACES.
       77  WK-NUMCED                  PIC 9(12) VALUE ZEROS.
       77  L-BOR                      PIC 9(3)  VALUE ZEROS.
       77  W-NOMEMP-DE3               PIC X(03) VALUE SPACES.
       77  W-AUTOLIQU-CODCIU          PIC 9(03) VALUE ZEROS.
       77  W-VAL                      PIC 9(06) VALUE ZEROS.
       77  WK-IND-TABNOM       PIC 9(02)      VALUE ZEROS.
       77  WK-IND-TABNOM2      PIC 9(02)      VALUE ZEROS.
       77  WRK-APELLI-1        PIC X(15)      VALUE SPACES.
       77  WRK-APELLI-2        PIC X(15)      VALUE SPACES.
       77  WRK-NOMBRE-1        PIC X(15)      VALUE SPACES.
       77  WRK-NOMBRE-2        PIC X(15)      VALUE SPACES.
      *
       01  AREA-VARIABLE-INFORME.
        03     LINEA-DETALLE.
         05    I-CONCEPTO                PIC ZZZ.
         05    I-DESCRIPCION             PIC X(30).
         05    I-HORAS                   PIC Z,ZZZ-.
         05    I-DIAS                    PIC ZZ,ZZZ-.
         05    I-DEV                     PIC ZZZ,ZZZ,ZZZ-.
         05    I-DED                     PIC ZZZ,ZZZ,ZZZ-.
        03     VR-APORTE.
         05    I-VR-1                    PIC S9(4) VALUE ZEROS.
         05    I-VR-2                    PIC 9(02) VALUE ZEROS.
      *
           COPY "..\LBL\CNNITCED.LBL".
           COPY "..\LBL\CONTREXT.LBL".
           COPY "..\LBL\CONCPTOS.LBL".
           COPY "..\LBL\PRFESION.LBL".
           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\AUDITE.LBL".
           COPY "..\LBL\TABLAEMP.LBL".
           COPY "..\LBL\USER-ACT.LBL".
           COPY "..\LBL\NOMISEXO.LBL".
           COPY "..\LBL\ESTCIVIL.LBL".
           COPY "..\LBL\DINOMBRE.LBL".
           COPY "..\LBL\MEMBRSIA.LBL".
           COPY "..\LBL\DCTOIDEN.LBL".
           COPY "..\LBL\TIPOVINC.LBL".
           COPY "..\LBL\PARENTCO.LBL".
           COPY "..\LBL\CONTDPTO.LBL".
           COPY "..\LBL\TIPONACI.LBL".
           COPY "..\LBL\AUDICAMP.LBL".
           COPY "..\LBL\HISNROME.LBL".
           COPY "..\LBL\NOMICIUD.LBL".
           COPY "..\LBL\INFORMES.LBL".
      *
           COPY "..\WRK\TABLAGEN.WRK".
           COPY "..\WRK\CONCPTOS.WRK".
           COPY "..\WRK\USER-ACT.WRK".
           COPY "..\WRK\VARIABLE.WRK".
           COPY "..\WRK\TABLAMES.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\WK-RAYAS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\NO-ERROR.WRK".
           COPY "..\WRK\LIQUINOV.WRK".
           COPY "..\WRK\MEMBRSIA.WRK".
           COPY "..\WRK\TEMFONPE.WRK".
           COPY "..\WRK\COMANDOS.WRK".
           COPY "..\WRK\FECHFINA.WRK".
           COPY "..\WRK\COPRO200.WRK".
      *
           COPY "..\LNK\CONTROLA.LNK".
      *
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
           COPY "..\DCL\CNNITCED.DCL".
           COPY "..\DCL\CONCPTOS.DCL".
           COPY "..\DCL\PRFESION.DCL".
           COPY "..\DCL\CONTREXT.DCL".
           COPY "..\DCL\AUDITE.DCL".
           COPY "..\DCL\EMPRESAS.DCL".
           COPY "..\DCL\TABLAEMP.DCL".
           COPY "..\DCL\USER-ACT.DCL".
           COPY "..\DCL\NOMISEXO.DCL".
           COPY "..\DCL\ESTCIVIL.DCL".
           COPY "..\DCL\DCTOIDEN.DCL".
           COPY "..\DCL\TIPOVINC.DCL".
           COPY "..\DCL\PARENTCO.DCL".
           COPY "..\DCL\CONTDPTO.DCL".
           COPY "..\DCL\TIPONACI.DCL".
           COPY "..\DCL\DINOMBRE.DCL".
           COPY "..\DCL\MEMBRSIA.DCL".
           COPY "..\DCL\AUDICAMP.DCL".
           COPY "..\DCL\HISNROME.DCL".
           COPY "..\DCL\NOMICIUD.DCL".
           COPY "..\DCL\INFORMES.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           PERFORM I-REVISA-USUAPERS-ACCE THRU F-REVISA-USUAPERS-ACCE.
           PERFORM I-PROCESO-AUDITE  THRU F-PROCESO-AUDITE
           PERFORM I-LABEL-ARCHIVOS  THRU F-LABEL-ARCHIVOS.
           PERFORM I-STATUS-DINOMBRE THRU F-STATUS-DINOMBRE.
           PERFORM I-ABRE-ARCHIVOS   THRU F-ABRE-ARCHIVOS.
      *    PERFORM INICIA-VAR-602    THRU F-INICIA-VAR-602.
           PERFORM I-MENU-OPCIONES   THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE        THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           PERFORM  I-STATUS-CONTREXT1  THRU  F-STATUS-CONTREXT1.
           PERFORM  I-LEE-CONTREXT      THRU  F-LEE-CONTREXT.
           PERFORM I-LABEL-TABLAEMP  THRU  F-LABEL-TABLAEMP.
           PERFORM I-STATUS-TABLAEMP THRU  F-STATUS-TABLAEMP.
      *    PERFORM I-LABEL-MEMBRSIA  THRU  F-LABEL-MEMBRSIA.
           PERFORM I-STATUS-MEMBRSIA THRU  F-STATUS-MEMBRSIA.
           PERFORM I-STATUS-NOMISEXO THRU  F-STATUS-NOMISEXO.
           PERFORM I-STATUS-ESTCIVIL THRU  F-STATUS-ESTCIVIL.
           PERFORM I-STATUS-TIPONACI THRU  F-STATUS-TIPONACI.
           PERFORM I-STATUS-CONTDPTO THRU  F-STATUS-CONTDPTO.
           PERFORM I-STATUS-PRFESION THRU  F-STATUS-PRFESION.
           PERFORM I-STATUS-TIPOVINC THRU  F-STATUS-TIPOVINC.
           PERFORM I-STATUS-DCTOIDEN THRU  F-STATUS-DCTOIDEN.
           PERFORM I-STATUS-PARENTCO THRU  F-STATUS-PARENTCO.
           ACCEPT WK-FECHA-SYS       FROM  DATE
           ACCEPT WK-HORA-HOY        FROM  TIME
           MOVE   WK-FECHA-SYS         TO  WK-FECHA-HOY.
           ACCEPT WK-FECHA-HOY       FROM  DATE
           IF WK-ANO >= 0  ADD 2000 TO WK-ANO WK-FECHA-SYS-ANO.
           ACCEPT WK-HORA-HOY        FROM  TIME
           MOVE WK-DIA                 TO  WK-MASCARA-DIA
           IF  WK-MES                      <  01
           OR  WK-MES                      >  12
               MOVE "ERR"              TO  WK-MASCARA-MES
           ELSE
               MOVE WK-ABREVIA (WK-MES)    TO  WK-MASCARA-MES.
           MOVE WK-ANO                 TO  WK-MASCARA-ANO
           MOVE "Ä"                    TO  WK-RAYA-1
           MOVE "Ä"                    TO  WK-RAYA-2
           MOVE WK-HORAS               TO  WK-MASCARA-HORAS
           MOVE WK-MINUTOS             TO  WK-MASCARA-MINUTOS
           MOVE ":"                    TO  WK-PUNTOS
           MOVE WIN-EMPRESA            TO  WK-COMPANIA
           MOVE "¶"                    TO  WK-RAYA-U
           MOVE "Ç"                    TO  WK-RAYA-P
           MOVE "¹"                    TO  WK-RAYAS-U
           MOVE "Ì"                    TO  WK-RAYAS-P.
           MOVE 16                     TO  POS-DOWN.
       F-LABEL-ARCHIVOS. EXIT.
      *
       I-ABRE-ARCHIVOS.
           OPEN I-O   DINOMBRE.
           OPEN I-O MEMBRSIA.
       F-ABRE-ARCHIVOS. EXIT.
      *
       I-MENU-OPCIONES.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE  "*LIBRO MEMBRESIA*"  TO  TIT-APLICACION.
           MOVE W-LINEA TO I-LINEA
           MOVE 999     TO   WK-EXCEPTION
           PERFORM I-CREAR-WINDOW THRU F-CREAR-WINDOW
           MOVE ZEROS           TO   FIN-OK
           PERFORM I-SELECCIONA THRU F-SELECCIONA
                              UNTIL SALIDA OR ESC.
           DISPLAY SPACE LINE 25 POSITION 01 SIZE 80.
           IF ESC
              MOVE 19 TO I-LINEA
              MOVE 999             TO   WK-EXCEPTION
              PERFORM I-WINDOW-FIN THRU F-WINDOW-FIN
              PERFORM I-SALIR      THRU F-SALIR UNTIL SALIDA OR ESC
              IF I-LINEA = 19 AND SALIDA
                 PERFORM  I-FIN-MODE THRU F-FIN-MODE
              ELSE
                 GO I-MENU-OPCIONES
           ELSE
           IF  PROSS-XKK = "N"
               NEXT SENTENCE
           ELSE
           DISPLAY W-OPCION  LINE 24 POSITION 80
           DISPLAY PROSS-AKK LINE 24 POSITION 72
           DISPLAY PROSS-MKK LINE 24 POSITION 74
           DISPLAY PROSS-CKK LINE 24 POSITION 76
           DISPLAY PROSS-RKK LINE 24 POSITION 78
           IF  W-OPCION    =  "A" AND
               PROSS-AKK   =  "S" AND
               MOD-USR (2) =  "S"
            PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
            PERFORM I-ADICIONA    THRU  F-ADICIONA
            MOVE ZEROS            TO    WK-EXCEPTION
           ELSE
            IF  W-OPCION  =  "C" AND PROSS-CKK = "S"
             PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
             PERFORM I-CONSULTA    THRU  F-CONSULTA
             MOVE ZEROS            TO    WK-EXCEPTION
            ELSE
             IF  W-OPCION  =  "M" AND PROSS-MKK = "S"
              PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
              PERFORM I-MODIFICA    THRU  F-MODIFICA
              MOVE ZEROS            TO    WK-EXCEPTION
             ELSE
              IF  W-OPCION  =  "R" AND PROSS-RKK = "S"
               PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
               PERFORM I-RETIRA      THRU  F-RETIRA
               MOVE ZEROS            TO    WK-EXCEPTION.
       F-MENU-OPCIONES. EXIT.
      *
       I-ADICIONA.
           MOVE SPACE TO WK-REGISTRO-MEMBRSIA.
           PERFORM I-DISPLAY-OPC  THRU F-DISPLAY-OPC.
           MOVE SPACES TO REC-MEMBRSIA
           PERFORM I-CEDULA THRU F-CEDULA.
           IF CUP GO F-ADICIONA.

           PERFORM I-LEE-MEMBRSIA THRU F-LEE-MEMBRSIA.
           IF  SW-1  =  ZEROS
            PERFORM I-MUESTRA-CAB     THRU F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA  THRU F-MUESTRA-NOMINA
            PERFORM I-EXISTE-MEMBRSIA THRU F-EXISTE-MEMBRSIA
            PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW
            GO I-ADICIONA.
           PERFORM I-INICIA-REG-MEMBRSIA THRU F-INICIA-REG-MEMBRSIA.
           MOVE WIN-CODIGO-EMPRESA TO MEMBRSIA-EMPRESA.
           PERFORM I-MUESTRA-CAB    THRU F-MUESTRA-CAB
           PERFORM I-MUESTRA-NOMINA THRU F-MUESTRA-NOMINA
           PERFORM I-DATOS          THRU F-DATOS
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW
           GO I-ADICIONA.
       F-ADICIONA. EXIT.
      *
       I-CONSULTA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-DISPLAY-OPC   THRU  F-DISPLAY-OPC.
           PERFORM I-CEDULA        THRU  F-CEDULA.
           DISPLAY SPACES LINE 25 POSITION 01 SIZE 80.
           IF CUP GO F-CONSULTA.

           IF CDN
             MOVE ZEROS TO MEMBRSIA-CLAVE
             PERFORM I-START-MEMBRSIA THRU F-START-MEMBRSIA
             PERFORM I-LEER-MEMBRSIA  THRU F-LEER-MEMBRSIA
      *      MOVE CONTRATO-CEDULA TO MEMBRSIA-CEDULA
             PERFORM I-LEE-MEMBRSIA   THRU F-LEE-MEMBRSIA
             IF SW-1 = 1
              PERFORM I-NEXISTE-MEMBRSIA THRU F-NEXISTE-MEMBRSIA
              MOVE MEMBRSIA-CEDULA   TO    WK-CODIGO
              MOVE SPACES TO REC-MEMBRSIA
              MOVE WK-CODIGO      TO    MEMBRSIA-CEDULA
              GO TO I-CONSULTA
             END-IF.

           PERFORM I-LEE-MEMBRSIA  THRU  F-LEE-MEMBRSIA.
           IF SW-1 = 1
             PERFORM I-NEXISTE-MEMBRSIA   THRU  F-NEXISTE-MEMBRSIA
             MOVE MEMBRSIA-CEDULA   TO    WK-CODIGO
             MOVE SPACE          TO    REC-MEMBRSIA
             MOVE WK-CODIGO      TO    MEMBRSIA-CEDULA
             GO I-CONSULTA.
      *    MOVE CONTRATO-CEDULA TO MEMBRSIA-CEDULA.
           PERFORM I-LEE-MEMBRSIA  THRU  F-LEE-MEMBRSIA.
           IF SW-1 = 1
             PERFORM I-NEXISTE-MEMBRSIA   THRU  F-NEXISTE-MEMBRSIA
             MOVE MEMBRSIA-CEDULA   TO    WK-CODIGO
             MOVE SPACE          TO    REC-MEMBRSIA
             MOVE WK-CODIGO      TO    MEMBRSIA-CEDULA
             GO I-CONSULTA.
           PERFORM I-CONSULTAR      THRU F-CONSULTAR.
           PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW.
       F-CONSULTA. EXIT.
      *
       I-MODIFICA.
           MOVE SPACE TO WK-REGISTRO-MEMBRSIA.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
           MOVE SPACES TO REC-MEMBRSIA
           PERFORM I-CEDULA THRU  F-CEDULA.
           DISPLAY SPACES LINE 25 POSITION 01 SIZE 80.
           IF CUP GO F-MODIFICA.
           IF CDN
             MOVE ZEROS TO MEMBRSIA-CLAVE
             PERFORM I-START-MEMBRSIA THRU F-START-MEMBRSIA
             PERFORM I-LEER-MEMBRSIA  THRU F-LEER-MEMBRSIA
      *      MOVE CONTRATO-CEDULA TO MEMBRSIA-CEDULA
             PERFORM I-LEE-MEMBRSIA   THRU F-LEE-MEMBRSIA
             IF SW-1 = 1
              PERFORM I-NEXISTE-MEMBRSIA THRU F-NEXISTE-MEMBRSIA
              MOVE MEMBRSIA-CEDULA   TO    WK-CODIGO
              MOVE SPACES TO REC-MEMBRSIA
              MOVE WK-CODIGO      TO    MEMBRSIA-CEDULA
              GO TO I-MODIFICA.
           PERFORM I-LEE-MEMBRSIA  THRU  F-LEE-MEMBRSIA.
           IF SW-1 = 1
             PERFORM I-NEXISTE-MEMBRSIA   THRU  F-NEXISTE-MEMBRSIA
             MOVE MEMBRSIA-CEDULA   TO    WK-CODIGO
             MOVE SPACE          TO    REC-MEMBRSIA
             MOVE WK-CODIGO      TO    MEMBRSIA-CEDULA
             GO I-MODIFICA.
      *    MOVE CONTRATO-CEDULA TO MEMBRSIA-CEDULA.
           PERFORM I-LEE-MEMBRSIA  THRU  F-LEE-MEMBRSIA.
           IF SW-1 = 1
             PERFORM I-NEXISTE-MEMBRSIA   THRU  F-NEXISTE-MEMBRSIA
             MOVE MEMBRSIA-CEDULA   TO    WK-CODIGO
             MOVE SPACE          TO    REC-MEMBRSIA
             MOVE WK-CODIGO      TO    MEMBRSIA-CEDULA
             GO I-MODIFICA.

           PERFORM I-GUARDA-INAC-MEMBRSIA THRU F-GUARDA-INAC-MEMBRSIA.
           PERFORM I-MUESTRA-CAB    THRU F-MUESTRA-CAB
           PERFORM I-MUESTRA-NOMINA THRU F-MUESTRA-NOMINA

           IF (MEMBRSIA-SW3 = 7 OR MEMBRSIA-SW3 = 8) AND
               MOD-USR (10) = "S"
            PERFORM I-RUTINA-NO-PERMITA THRU F-RUTINA-NO-PERMITA
            PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
            GO TO I-MODIFICA.

           MOVE WIN-CODIGO-EMPRESA TO MEMBRSIA-EMPRESA.

           IF MOD-USR (9) = "S"
             PERFORM  I-DATOSCNO  THRU  F-DATOSCNO
           ELSE
             PERFORM  I-DATOS     THRU  F-DATOS.

           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
           GO I-MODIFICA.
       F-MODIFICA. EXIT.
      *
       I-RUTINA-NO-PERMITA.
           PERFORM I-VENTANA-NOSEPUEDE THRU F-VENTANA-NOSEPUEDE.
       F-RUTINA-NO-PERMITA. EXIT.
      *
       I-RETIRA.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
           PERFORM I-CEDULA THRU F-CEDULA.
           IF CUP GO F-RETIRA.
           PERFORM I-LEE-MEMBRSIA  THRU  F-LEE-MEMBRSIA.
           IF SW-1 = 1
            PERFORM I-NEXISTE-MEMBRSIA   THRU  F-NEXISTE-MEMBRSIA
            MOVE MEMBRSIA-CEDULA   TO    WK-CODIGO
            MOVE SPACE          TO    REC-MEMBRSIA
            MOVE WK-CODIGO      TO    MEMBRSIA-CEDULA
            GO I-RETIRA.
      *    MOVE CONTRATO-CEDULA TO MEMBRSIA-CEDULA.
           PERFORM I-LEE-MEMBRSIA  THRU  F-LEE-MEMBRSIA.
           IF SW-1 = 1
             PERFORM I-NEXISTE-MEMBRSIA   THRU  F-NEXISTE-MEMBRSIA
             MOVE MEMBRSIA-CEDULA   TO    WK-CODIGO
             MOVE SPACE          TO    REC-MEMBRSIA
             MOVE WK-CODIGO      TO    MEMBRSIA-CEDULA
             GO I-RETIRA.
           PERFORM I-MUESTRA-CAB    THRU F-MUESTRA-CAB.
           PERFORM I-MUESTRA-NOMINA THRU F-MUESTRA-NOMINA.
           PERFORM I-ELIMINAR       THRU F-ELIMINAR.
           PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW.
       F-RETIRA. EXIT.
      *
       I-ELIMINAR.
           MOVE SPACES TO W-TRUCO.
           PERFORM I-COLOR-1        THRU F-COLOR-1.
           DISPLAY
           "<AF2> BORRAR/DELETE              <F2> END TO PROGRAM
      -    "       <F1> CANCEL  " LINE 25 POSITION 01
           PERFORM ACEPTA-TRUCO
           IF AF2  GO I-BORRAR.
           IF F1 GO F-ELIMINAR.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF W-TRUCO NOT = "R" GO I-ELIMINAR.
           IF MEMBRSIA-SW3 = "9"
            NEXT SENTENCE
           ELSE
            DISPLAY
            "Miembro activo - No puede Borrarlo"
            LINE 25 POSITION 01
            PERFORM ACEPTA-TRUCO
            GO F-ELIMINAR.
           GO I-AUDITAR.
        I-BORRAR.
           MOVE ZEROS TO WK-CODIGO.
           MOVE MEMBRSIA-CEDULA TO WK-CODIGO.

           PERFORM I-BORRA-HISNROME THRU F-BORRA-HISNROME.

           MOVE SPACES TO WRK-AUDICAMP-CONTENIDO-ANT
                          WRK-AUDICAMP-CONTENIDO-NUE
           MOVE "R, MEMBRSIA" TO WRK-AUDICAMP-CONTENIDO-ANT
           MOVE MEMBRSIA-CEDULA TO WRK-AUDICAMP-CONTENIDO-NUE
           PERFORM I-AUDITA-BORRADOS THRU F-AUDITA-BORRADOS.
           DELETE MEMBRSIA.
           GO F-ELIMINAR.
        I-AUDITAR.
       F-ELIMINAR. EXIT.
      **
       I-BORRA-HISNROME.
           PERFORM I-LABEL-HISNROME THRU F-LABEL-HISNROME.
           PERFORM I-STATUS-HISNROME THRU F-STATUS-HISNROME.
           MOVE MEMBRSIA-CEDULA  TO HISNROME-CEDULA.
           MOVE MEMBRSIA-NROCTO  TO HISNROME-NROCTO.
           OPEN I-O HISNROME.
           READ HISNROME WITH NO LOCK INVALID KEY
            CLOSE HISNROME
            GO TO F-BORRA-HISNROME.
           DELETE HISNROME.
           CLOSE HISNROME.
       F-BORRA-HISNROME. EXIT.
      *
       I-CEDULA.
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01
           "<F1> HELP SUPERVIS "  LINE 25 POSITION 01
           "<> MENU"             LINE 25 POSITION 22
           "<F2> END"             LINE 25 POSITION 34 .
           IF W-OPCION = "C" OR W-OPCION = "M"
             DISPLAY
             "<> GO TO FIRST" LINE 25 POSITION 44 .
           MOVE ZEROS TO SW-1 WK-EXCEPTION.
           PERFORM I-COLOR-73        THRU F-COLOR-73
           DISPLAY  SPACES  SIZE 15  LINE 02 POSITION 21.
           ACCEPT MEMBRSIA-CEDULA LINE 02 POSITION 21
                      NO BEEP ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE     MEMBRSIA-CEDULA  TO WI-Z12C.
           DISPLAY  WI-Z12C  LINE 02 POSITION 21.

           IF F1
             PERFORM I-AYUDA-PROG400   THRU F-AYUDA-PROG400
             PERFORM I-COLOR-2         THRU F-COLOR-2
             PERFORM I-LLAMA-WINDOW    THRU F-LLAMA-WINDOW
             GO I-CEDULA.

           DISPLAY WK-ESPACIOS LINE 25 POSITION 1
           IF ((CDN AND W-OPCION = "C") OR (CDN AND W-OPCION = "M"))
            GO F-CEDULA.
           MOVE MEMBRSIA-CEDULA TO WI-Z12C.
           DISPLAY WI-Z12C LINE  2 POSITION 21.
           IF  CUP GO F-CEDULA
           ELSE
            IF  ERR MOVE ZEROS TO WK-EX
              DISPLAY
              " Codigo errado, de cualquier tecla para continuar..."
              LINE 25 POSITION 01
              PERFORM ACEPTA-TRUCO
              GO I-CEDULA
            ELSE
             IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE
             ELSE
              IF MEMBRSIA-CEDULA = ZEROS
                DISPLAY
              "ERROR CAMPO en ceros, OPRIMA UNA tecla para continuar.."
                LINE 25 POSITION 01
                PERFORM ACEPTA-TRUCO
                GO I-CEDULA.
       F-CEDULA. EXIT.
      *
       I-DATOS.
           PERFORM I-DATOS-CAB THRU F-DATOS-CAB.
           IF CUP  GO F-DATOS.
           IF F5   GO DAT-1.
           IF F3   GO GRABAR-YA.
         DAT-1.
      *    PERFORM I-DATOS-NOMINA THRU F-DATOS-NOMINA.
      *    IF CUP  GO I-DATOS.
      *    IF MOD-USR (2) = "N"
      *     PERFORM I-GRA-MAES-HOJA-CONT THRU F-GRA-MAES-HOJA-CONT
      *     IF F1 GO I-DATOS
      *     ELSE
      *      PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
      *      GO F-DATOS.
         DAT-2.
           IF W-OPCION = "A"
            PERFORM I-ADICIONA-602 THRU F-ADICIONA-602.
           IF W-OPCION  =  "M"
            PERFORM I-MODIFICA-602 THRU F-MODIFICA-602.
           IF CUP
            PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
            PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC
            PERFORM I-MUESTRA-CAB    THRU F-MUESTRA-CAB
            PERFORM I-MUESTRA-NOMINA THRU F-MUESTRA-NOMINA
            GO DAT-1.
         GRABAR-YA.
           IF F3
            PERFORM I-GRA-MAES-HOJA-CONT THRU F-GRA-MAES-HOJA-CONT
            IF F1 GO I-DATOS
            ELSE
             PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
             GO F-DATOS.
       F-DATOS. EXIT.
      *
      *
       I-GRA-MAES-HOJA-CONT.
           IF F3 GO GRABAR-POR-F3.
           IF W-OPCION = "A"
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "<A> ADD      "  LINE 25 POSITION 01
            "<F1> RETURN  "  LINE 25 POSITION 28
            "<F2> END"       LINE 25 POSITION 43 .
           IF W-OPCION = "M"
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "<M> MODIFICAR"    LINE 25 POSITION 01
            "<F1> REGRESAR"    LINE 25 POSITION 28
            "<F2> FIN"         LINE 25 POSITION 43 .

           MOVE  ZEROS   TO  WK-EXCEPTION.
           MOVE  SPACES  TO  W-TRUCO.

           IF W-OPCION = "A"
            PERFORM ACEPTA-TRUCO UNTIL W-TRUCO = "A" OR F1 OR F2.
           IF W-OPCION = "M"
            PERFORM ACEPTA-TRUCO UNTIL W-TRUCO = "M" OR F1 OR F2.

           IF F1  GO F-GRA-MAES-HOJA-CONT.

           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.

           IF W-OPCION = "A" AND W-TRUCO NOT = "A"
            GO TO I-GRA-MAES-HOJA-CONT.

           IF W-OPCION = "M" AND W-TRUCO NOT = "M"
            GO TO I-GRA-MAES-HOJA-CONT.
         GRABAR-POR-F3.
           PERFORM I-AUDITORIA-DE-CAMPOS THRU F-AUDITORIA-DE-CAMPOS.
           PERFORM I-AUDITORIA-INT-NOMIH THRU F-AUDITORIA-INT-NOMIH.
           IF W-OPCION = "A"
              PERFORM I-VALIDA-NROCTO  THRU F-VALIDA-NROCTO
              PERFORM I-ACTUALIZA-HISNROME THRU F-ACTUALIZA-HISNROME.
           REWRITE REC-MEMBRSIA INVALID KEY WRITE REC-MEMBRSIA.
       F-GRA-MAES-HOJA-CONT. EXIT.
      *
       I-VALIDA-NROCTO.
              MOVE     77                 TO W-COMP
              PERFORM  I-BUSCA-CONTREXT THRU F-BUSCA-CONTREXT.
              MOVE     WK-CONSEC          TO MEMBRSIA-NROCTO.
       F-VALIDA-NROCTO.
      *
       I-LLAMA-WINDOW.
           MOVE "NO" TO SG-BLINK.
           MOVE "NO" TO SG-BEEP.
           PERFORM I-COLOR-2        THRU F-COLOR-2.
           DISPLAY " " ERASE CONTROL SG-CONTROL-STRING
           PERFORM I-WINDOW-CAB THRU F-WINDOW-CAB.
           PERFORM I-WINDOW-NOM THRU F-WINDOW-NOM.
           PERFORM I-DISPLAY-OPC  THRU F-DISPLAY-OPC.
       F-LLAMA-WINDOW. EXIT.
      *
       I-DISPLAY-OPC.
           IF  W-OPCION   =  "A"
            DISPLAY " A D I C I O N " LINE  3 POSITION 32
           ELSE
           IF  W-OPCION   =  "C"
            DISPLAY "   CONSULTA    " LINE  3 POSITION 32
           ELSE
           IF  W-OPCION   =  "M"
            DISPLAY " MODIFICACION  " LINE  3 POSITION 32
           ELSE
           IF  W-OPCION   =  "R"
            DISPLAY "  R E T I R O  " LINE  3 POSITION 32 .
       F-DISPLAY-OPC. EXIT.
      *
       I-MUESTRA-CAB.
           MOVE MEMBRSIA-CEDULA TO WI-Z12C.
      *DESPLIEGA NOMBRES Y APELLIDOS
           DISPLAY
           WI-Z12C                 LINE 02 POSITION 21
           MEMBRSIA-EMPRESA           LINE 01 POSITION 72
           MEMBRSIA-NOMBRE            LINE 04 POSITION 21  SIZE 59
           MOVE MEMBRSIA-CEDULA       TO   CEDULA-DINOMBRE.
           PERFORM I-LEE-DINOMBRE  THRU F-LEE-DINOMBRE.
           DISPLAY SPACES          LINE 04 POSITION 21 SIZE 59.
           IF SW-1 = 0
              DISPLAY APELLIDO1-DINOMBRE  LINE 04 POSITION 21
              DISPLAY APELLIDO2-DINOMBRE  LINE 04 POSITION 36
              DISPLAY NOMBRE1-DINOMBRE    LINE 04 POSITION 51
              DISPLAY NOMBRE2-DINOMBRE    LINE 04 POSITION 65.
           IF W-OPCION = "A" AND
              SW-1 = 0       AND
              MEMBRSIA-NOMBRE = SPACES
             MOVE  NOMBRE-DINOMBRE  TO  MEMBRSIA-NOMBRE.
           MOVE ZEROS  TO SW-1.
      *DESPLIEGA PAIS, DPTO, CIUDADES
           DISPLAY MEMBRSIA-COD-PAIS     LINE 05 POSITION 21
           PERFORM I-LEE-TIPONACI     THRU F-LEE-TIPONACI.
           DISPLAY SPACES             LINE 05 POSITION 26 SIZE 12.
           IF SW-1 = 0
              DISPLAY NOM-NAC         LINE 05 POSITION 26 SIZE 12
           ELSE
              DISPLAY "N.N"           LINE 05 POSITION 26 SIZE 12.
           MOVE    ZEROS  TO SW-1.
           DISPLAY MEMBRSIA-COD-DPTO LINE 06 POSITION 21
           PERFORM I-LEE-CONTDPTO     THRU F-LEE-CONTDPTO.
           DISPLAY SPACES             LINE 06 POSITION 26 SIZE 12.
           IF SW-1 = 0
              DISPLAY NOM-DPTO        LINE 06 POSITION 26 SIZE 16
           ELSE
              DISPLAY "N.N"           LINE 06 POSITION 26 SIZE 16.
           MOVE    ZEROS  TO SW-1.
           DISPLAY MEMBRSIA-CIUDAD       LINE 07 POSITION 21
           MOVE    MEMBRSIA-CIUDAD          TO CIUD-CODIGO
           PERFORM I-LEE-NOMICIUD     THRU F-LEE-NOMICIUD.
           DISPLAY SPACES             LINE 07 POSITION 26 SIZE 16.
           IF SW-1 = 0
              DISPLAY CIUD-NOMBRE     LINE 07 POSITION 26 SIZE 16
           ELSE
              DISPLAY "N.N"           LINE 07 POSITION 26 SIZE 16.
           MOVE ZEROS  TO SW-1.

           MOVE MEMBRSIA-TEL          TO EDIT-TELE.
           DISPLAY MEMBRSIA-DIR-UBIC      LINE 08 POSITION 21.
           MOVE MEMBRSIA-DIR-NRO          TO WI-Z3
           DISPLAY WI-Z3               LINE 08 POSITION 24
           DISPLAY MEMBRSIA-DIR-LETRA     LINE 08 POSITION 27
           DISPLAY MEMBRSIA-DIR-SUR-NORTE LINE 08 POSITION 29
           DISPLAY MEMBRSIA-DIR-SIMB-NRO  LINE 08 POSITION 30
           MOVE MEMBRSIA-DIR-PARTE1       TO WI-Z3
           DISPLAY WI-Z3               LINE 08 POSITION 31
           DISPLAY MEMBRSIA-DIR-LETRA1 LINE 08 POSITION 34
           MOVE "-"                    TO MEMBRSIA-DIR-GUION
           DISPLAY MEMBRSIA-DIR-GUION     LINE 08 POSITION 35.
           MOVE MEMBRSIA-DIR-PARTE2       TO WI-Z3.
           DISPLAY WI-Z3                  LINE 08 POSITION 36
           DISPLAY
           MEMBRSIA-BAR                   LINE 09 POSITION 21
           EDIT-TELE                      LINE 10 POSITION 21
           MEMBRSIA-EMAIL                 LINE 11 POSITION 21
           MOVE MEMBRSIA-OFICIO           TO WI-Z6.
           DISPLAY WI-Z6                  LINE 16 POSITION 21
           OPEN INPUT PRFESION.
           MOVE MEMBRSIA-OFICIO           TO COD-PROF.
           READ PRFESION WITH NO LOCK     INVALID KEY
            DISPLAY "N.N"                 LINE 16 POSITION 27.
           CLOSE PRFESION.
           DISPLAY NOM-PROF            LINE 16 POSITION 27 SIZE 16.
           DISPLAY
           MEMBRSIA-SW3                   LINE 16 POSITION 65
           MOVE MEMBRSIA-FECDEF           TO WI-FECHA.
           DISPLAY WI-FECHA               LINE 14 POSITION 65.
      *SEGUNDA SECCION - PARTE LATERAL
           MOVE  MEMBRSIA-FECNTO          TO  W-MASCARA-FECHA.
           PERFORM  I-MASCARA-FECHA       THRU  F-MASCARA-FECHA.
           IF SW-ERROR = 0
            DISPLAY WK-MASCARA-FECHA      LINE 05 POSITION 65.
           DISPLAY
           MEMBRSIA-CIUNTO                LINE 06 POSITION 65
           MEMBRSIA-SEXO                  LINE 07 POSITION 65.
           PERFORM I-LEE-NOMISEXO      THRU F-LEE-NOMISEXO.
           DISPLAY SPACES              LINE 07 POSITION 67 SIZE 12.
           IF SW-1 = 0
              DISPLAY NOM-SEXO         LINE 07 POSITION 67 SIZE 12
           ELSE
              DISPLAY "N.N"            LINE 07 POSITION 67 SIZE 12.
           MOVE ZEROS  TO SW-1.
           DISPLAY
           MEMBRSIA-LBT-NRO               LINE 08 POSITION 65
           MEMBRSIA-LBT-DIS               LINE 09 POSITION 65
           MEMBRSIA-LBT-CLA               LINE 10 POSITION 65
           MEMBRSIA-ESTCIV                LINE 11 POSITION 65.
           PERFORM I-LEE-ESTCIVIL      THRU F-LEE-ESTCIVIL.
           DISPLAY SPACES              LINE 11 POSITION 67 SIZE 12.
           IF SW-1 = 0
              DISPLAY NOM-ESTCIV       LINE 11 POSITION 67 SIZE 12
           ELSE
              DISPLAY "N.N"            LINE 11 POSITION 67 SIZE 12.
           MOVE ZEROS  TO SW-1.
           DISPLAY MEMBRSIA-OBSERVACION1 LINE 19 POSITION 03.
           DISPLAY MEMBRSIA-OBSERVACION2 LINE 20 POSITION 03.
           DISPLAY MEMBRSIA-OBSERVACION3 LINE 21 POSITION 03.
       F-MUESTRA-CAB. EXIT.
      *
       I-DECODIFICA-CODACT.
           IF MEMBRSIA-SW3 = 0
            DISPLAY "Nuevo     " LINE 16 POSITION 72
            DISPLAY "Nuevo     " LINE 25 POSITION 72
           ELSE
            IF MEMBRSIA-SW3 = 1
             DISPLAY "Activo   " LINE 16 POSITION 69
             DISPLAY "Activo   " LINE 25 POSITION 69
            ELSE
             IF MEMBRSIA-SW3 = 7
              DISPLAY "Fallecido" LINE 16 POSITION 69
              DISPLAY "Fallecido" LINE 25 POSITION 69
            ELSE
              IF MEMBRSIA-SW3 = 8
               DISPLAY "         " LINE 16 POSITION 69
               DISPLAY "         " LINE 25 POSITION 69
              ELSE
               IF MEMBRSIA-SW3 = 9
                DISPLAY "Inactivo  " LINE 16 POSITION 69
                DISPLAY "Inactivo  " LINE 25 POSITION 69
               ELSE
                DISPLAY "N.N.      " LINE 16 POSITION 69.
       F-DECODIFICA-CODACT. EXIT.
      *
       I-DATOS-VENTANA-NOM2.
           MOVE MEMBRSIA-CEDULA TO WI-Z12C.
           DISPLAY
           WI-Z12C           LINE 02 POSITION 21
           MEMBRSIA-NOMBRE      LINE 04 POSITION 21 .
      * OTROS
           MOVE MEMBRSIA-OFICIO TO WI-Z6.
           DISPLAY WI-Z6 LINE 19 POSITION 59 .
       F-DATOS-VENTANA-NOM2. EXIT.
      *
       I-DATOS-VENTANA-NOM3.
           MOVE MEMBRSIA-CEDULA TO WI-Z12C.
           DISPLAY
           WI-Z12C           LINE 02 POSITION 21
           MEMBRSIA-NOMBRE      LINE 04 POSITION 40 .
       F-DATOS-VENTANA-NOM3. EXIT.
      *
       I-CONSULTAR.
         VENTANA-1.
           PERFORM I-LLAMADO-VENTANA1 THRU F-LLAMADO-VENTANA1.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF PGDN AND MOD-USR (2) = "S" GO VENTANA-2.
           IF PGDN AND MOD-USR (2) = "N" AND MOD-USR (4) = "S"
            GO VENTANA-4.
           IF PGDN AND MOD-USR (2) = "N" AND MOD-USR (4) = "N" AND
              MOD-USR (5) = "S"
            GO VENTANA-5.
           IF PGDN       GO VENTANA-2.
           IF CDN        GO SIGUIENTE-REGISTRO.
           IF F1 GO F-CONSULTAR.
         VENTANA-2.
      *    PERFORM I-CONSULTA-602 THRU F-CONSULTA-602.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF PGUP GO I-CONSULTAR.
           IF PGDN AND MOD-USR (4) = "S" GO VENTANA-4.
           IF PGDN AND MOD-USR (4) = "N" AND MOD-USR (5) = "S"
            GO VENTANA-5.
         VENTANA-4.
           PERFORM I-LLAMADO-VENTANA4 THRU F-LLAMADO-VENTANA4.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF PGUP AND MOD-USR (2) = "S" GO VENTANA-2.
           IF PGUP AND MOD-USR (2) = "N" GO VENTANA-1.
           IF PGDN AND MOD-USR (5) = "S" GO VENTANA-5.
         VENTANA-5.
           PERFORM I-LLAMADO-VENTANA5 THRU F-LLAMADO-VENTANA5.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY SPACES SIZE 41 LINE 02 POSITION 02.
           IF PGUP AND MOD-USR (4) = "S"  GO VENTANA-4.
           IF PGUP AND MOD-USR (2) = "S" AND MOD-USR (4) = "N"
             GO VENTANA-2.
           IF PGUP AND MOD-USR (2) = "N" AND MOD-USR (4) = "N"
             GO VENTANA-1.
         SIGUIENTE-REGISTRO.
           IF  CDN
            PERFORM I-LEER-MEMBRSIA THRU F-LEER-MEMBRSIA
            IF SW-OK = 1
             PERFORM I-FIN-CONSULTAR THRU F-FIN-CONSULTAR UNTIL F1
             GO F-CONSULTAR
            ELSE
             MOVE MEMBRSIA-CEDULA TO MEMBRSIA-CEDULA
             PERFORM I-LEE-MEMBRSIA THRU F-LEE-MEMBRSIA
             IF SW-1 = 1
              PERFORM I-NEXISTE-MEMBRSIA   THRU  F-NEXISTE-MEMBRSIA
              GO F-CONSULTAR.
           DISPLAY SPACES SIZE 80 LINE 01 POSITION 25.
           GO I-CONSULTAR.
       F-CONSULTAR.
      *
       I-LLAMADO-VENTANA1.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW.
           PERFORM I-MUESTRA-CAB    THRU F-MUESTRA-CAB.
           PERFORM I-MUESTRA-NOMINA THRU F-MUESTRA-NOMINA.
           IF MOD-USR (2) = "N" AND MOD-USR (4) = "N" AND
              MOD-USR (5) = "N"
            DISPLAY
            SPACES SIZE 80               LINE 25 POSITION 01
            "<F2> Finish"                LINE 25 POSITION 01
            "<>  Consult Next"          LINE 25 POSITION 12
            "<F1> Return "               LINE 25 POSITION 30
            PERFORM ACEPTA-TRUCO UNTIL F2 OR CDN OR F1
           ELSE
            DISPLAY
            SPACES SIZE 80               LINE 25 POSITION 01
            "<F2> Finish"               LINE 25 POSITION 01
            "<>Consult next record"     LINE 25 POSITION 12
            "<F1> Return"                LINE 25 POSITION 30
            "<PgDn> Next Screen"         LINE 25 POSITION 44
            PERFORM ACEPTA-TRUCO UNTIL F2 OR CDN OR F1 OR PGDN.
       F-LLAMADO-VENTANA1. EXIT.
      *
       I-LLAMADO-VENTANA4.
           MOVE "NO" TO SG-BLINK.
           MOVE "NO" TO SG-BEEP.
           PERFORM I-COLOR-3        THRU F-COLOR-3.
           DISPLAY " " ERASE CONTROL SG-CONTROL-STRING
           MOVE ZEROS TO WK-EXCEPTION.
      *    PERFORM I-WINDOW-1           THRU F-WINDOW-1.
      *    PERFORM I-VENTANA-NOM2       THRU F-VENTANA-NOM2.
      *    PERFORM I-DATOS-VENTANA-NOM2 THRU F-DATOS-VENTANA-NOM2.
           IF MOD-USR (5) = "N"
            DISPLAY
            SPACES SIZE 80               LINE 25 POSITION 01
            "<F2> Salir"                 LINE 25 POSITION 01
            "<PgUp> Anterior Pantalla."  LINE 25 POSITION 44
            PERFORM ACEPTA-TRUCO UNTIL F2 OR PGUP
           ELSE
            DISPLAY
            SPACES SIZE 80               LINE 25 POSITION 01
            "<F2> Salir"                 LINE 25 POSITION 01
            "<PgDn> Siguiente Pantalla." LINE 25 POSITION 12
            "<PgUp> Anterior Pantalla."  LINE 25 POSITION 44
            PERFORM ACEPTA-TRUCO UNTIL F2 OR PGUP OR PGDN.
       F-LLAMADO-VENTANA4. EXIT.
      *
       I-LLAMADO-VENTANA5.
           MOVE "NO" TO SG-BLINK.
           MOVE "NO" TO SG-BEEP.
           PERFORM I-COLOR-4        THRU F-COLOR-4.
           DISPLAY " " ERASE CONTROL SG-CONTROL-STRING.
      *    PERFORM I-WINDOW-1           THRU F-WINDOW-1.
      *    PERFORM I-VENTANA-NOM3       THRU F-VENTANA-NOM3.
      *    PERFORM I-DATOS-VENTANA-NOM3 THRU F-DATOS-VENTANA-NOM3.
       I-PIDE-TECLA.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY
           SPACES SIZE 80               LINE 25 POSITION 01
           "<F2> Salir"                 LINE 25 POSITION 01
           "<PgUp> Anterior Pantalla."  LINE 25 POSITION 30
           "<F8> Acumulados"            LINE 25 POSITION 60 .
           PERFORM ACEPTA-TRUCO UNTIL F2 OR PGUP OR F8.
      *    IF F8
      *     PERFORM I-ACUMULADOS THRU F-ACUMULADOS
      *     GO I-PIDE-TECLA.
       F-LLAMADO-VENTANA5. EXIT.
      *
       I-BORRA-LINEAS.
           DISPLAY SPACES SIZE 05 LINE L-BOR POSITION 02.
           DISPLAY SPACES SIZE 31 LINE L-BOR POSITION 08.
           DISPLAY SPACES SIZE 06 LINE L-BOR POSITION 40.
           DISPLAY SPACES SIZE 07 LINE L-BOR POSITION 47.
           DISPLAY SPACES SIZE 12 LINE L-BOR POSITION 55.
           DISPLAY SPACES SIZE 12 LINE L-BOR POSITION 68.
       F-BORRA-LINEAS. EXIT.
      *
       I-FIN-CONSULTAR.
           DISPLAY WK-ESPACIOS                  LINE 25 POSITION 01
           DISPLAY " NO HAY MAS REGISTROS "     LINE 25 POSITION 01
           DISPLAY " <F1> OPCIONES "            LINE 25 POSITION 25
           PERFORM ACEPTA-TRUCO
           IF F2
            PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-FIN-CONSULTAR.
      *
       I-EXISTE-MEMBRSIA.
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01
           "Registro ya existe en Libro Membresia..."
           LINE 25 POSITION 01
           "<ENTER> Para continuar..." LINE 25 POSITION 58.
           PERFORM ACEPTA-TRUCO.
       F-EXISTE-MEMBRSIA. EXIT.
      *
       I-NEXISTE-MEMBRSIA.
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01
           "Registro no Existe...."
           LINE 25 POSITION 01
           PERFORM ACEPTA-TRUCO.
       F-NEXISTE-MEMBRSIA. EXIT.
      *
       I-LEE-MEMBRSIA.
           MOVE ZEROS TO SW-1.
           READ MEMBRSIA WITH NO LOCK INVALID KEY
                MOVE 1 TO SW-1 GO F-LEE-MEMBRSIA.
       F-LEE-MEMBRSIA. EXIT.
      *
       I-LEER-MEMBRSIA.
           MOVE 0 TO SW-OK.
           READ MEMBRSIA NEXT RECORD WITH NO LOCK AT END
                         MOVE 1 TO SW-OK GO F-LEER-MEMBRSIA.
       F-LEER-MEMBRSIA. EXIT.
      *
       I-LEE-TIPONACI.
           OPEN INPUT TIPONACI.
           MOVE MEMBRSIA-COD-PAIS TO COD-NAC.
           READ TIPONACI WITH NO LOCK INVALID KEY
                         MOVE 1 TO SW-1.
           CLOSE TIPONACI.
       F-LEE-TIPONACI.

       I-LEE-CONTDPTO.
           OPEN INPUT CONTDPTO.
           MOVE MEMBRSIA-COD-DPTO TO COD-DPTO
           READ CONTDPTO WITH NO LOCK INVALID KEY
                         MOVE 1 TO SW-1.
           CLOSE CONTDPTO.
       F-LEE-CONTDPTO.

       I-LEE-NOMICIUD.
           OPEN INPUT NOMICIUD.
           MOVE MEMBRSIA-CIUDAD       TO   CIUD-CODIGO
           READ NOMICIUD WITH NO LOCK INVALID KEY
                         MOVE 1 TO SW-1.
           CLOSE NOMICIUD.
       F-LEE-NOMICIUD.

       I-LEE-NOMISEXO.
           OPEN INPUT NOMISEXO.
           MOVE MEMBRSIA-SEXO TO COD-SEXO.
           READ NOMISEXO WITH NO LOCK INVALID KEY
                         MOVE 1 TO SW-1.
           CLOSE NOMISEXO.
       F-LEE-NOMISEXO.

       I-LEE-ESTCIVIL.
           OPEN INPUT ESTCIVIL.
           MOVE MEMBRSIA-ESTCIV TO COD-ESTCIV.
           READ ESTCIVIL WITH NO LOCK INVALID KEY
                         MOVE 1 TO SW-1.
           CLOSE ESTCIVIL.
       F-LEE-ESTCIVIL.
      *
       I-FIN-MODE.
           CLOSE MEMBRSIA DINOMBRE.
           CANCEL "PROG400.COB".
           EXIT  PROGRAM.

           STOP  RUN.
       F-FIN-MODE.
      *
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\AUDITE.PRO".
           COPY "..\PRO\NO-ERROR.PRO".
           COPY "..\PRO\DATOMEMB.PRO".
           COPY "..\PRO\VENTMEMB.PRO".
           COPY "..\PRO\AUDITAME.PRO".
           COPY "..\PRO\TITULO1E.PRO".
           COPY "..\PRO\CONTMEMB.PRO".
           COPY "..\PRO\AUDMEMBR.PRO".
           COPY "..\PRO\USUAPERS.PRO".
           COPY "..\PRO\COPRO400.PRO".
           COPY "..\PRO\AFIFP400.PRO".
           COPY "..\PRO\MASCFECH.PRO".
           COPY "..\PRO\AYUDCIUD.PRO".
           COPY "..\PRO\AYUDPAIS.PRO".
           COPY "..\PRO\AYUDDPTO.PRO".
           COPY "..\PRO\AYUDMEMB.PRO".
           COPY "..\PRO\AYUDA400.PRO".
           COPY "..\PRO\AYUDPROF.PRO".
           COPY "..\PRO\NOMBMEMB.PRO".
           COPY "..\PRO\FECHFINA.PRO".
           COPY "..\PRO\EANO2000.PRO".
           COPY "..\PRO\COLOR200.PRO".
           COPY "..\PRO\DATOSCNO.PRO".
           COPY "..\PRO\CONTREX1.PRO".
      *
           COPY "..\STA\PRFESION.STA".
           COPY "..\STA\PARENTCO.STA".
           COPY "..\STA\DCTOIDEN.STA".
           COPY "..\STA\TIPOVINC.STA".
           COPY "..\STA\NOMISEXO.STA".
           COPY "..\STA\ESTCIVIL.STA".
           COPY "..\STA\CONTDPTO.STA".
           COPY "..\STA\TIPONACI.STA".
           COPY "..\STA\AUDICAMP.STA".
           COPY "..\STA\HISNROME.STA".
           COPY "..\STA\MEMBRSIA.STA".
           COPY "..\STA\TABLAEMP.STA".
           COPY "..\STA\NROMEMBR.STA".
      *
           COPY "..\PLB\TABLAEMP.PLB".
           COPY "..\PLB\AUDICAMP.PLB".
           COPY "..\PLB\HISNROME.PLB".
      *
      ******************************************************************
