       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG742.
      ******************************************************************
      *         CATALOGO DE NITS   ABR-27-2002                                    *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG742".
       77      ID-PRG         PIC X(7)       VALUE "PROG742".
       77      FEC-DES        PIC 9(08)      VALUE ZEROS.
       77      FEC-HAS        PIC 9(08)      VALUE ZEROS.
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE  SPACES.

       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA  PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\USER-ACT.LBL".

               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\LISTADOS.WRK".
      *******************************************************************
      *                       T  I  T  U  L  O  S                       *
      *******************************************************************
       01      AREA-INFORME.
        03     TITULO-0.
         05    TIT-CIA     PIC X(40)    VALUE SPACES.

        03     TITULO-1.
         05    FILLER      PIC X(07)    VALUE SPACES.
         05    FILLER      PIC X(17)    VALUE
               "CATALOGO  DE  NIT".

        03     TITULO-2.
         05    FILLER      PIC X(16)    VALUE SPACES.
         05    I-ORGANIZA  PIC X(31)    VALUE SPACES.

        03     TITULO-3.
         05    FILLER      PIC X(06)    VALUE "Hora: ".
         05    I-HORA      PIC X(05).
         05    FILLER      PIC X(02)    VALUE SPACES.
         05    FILLER      PIC X(06)    VALUE "Fec.: ".
         05    I-FECHA     PIC X(11).
         05    FILLER      PIC X(03)    VALUE SPACES.
         05    FILLER      PIC X(05)    VALUE "Pag. ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(03)    VALUE SPACES.
         05    FILLER      PIC X(07)    VALUE "Prog : ".
         05    I-INF       PIC X(12)    VALUE SPACES.

        03     TITULO-4.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(15)    VALUE "N.I.T / CEDULA ".
         05    FILLER      PIC X(02)    VALUE "| ".
         05    FILLER      PIC X(31)    VALUE
               "  NOMBRE  /  RAZON SOCIAL".
         05    FILLER      PIC X(02)    VALUE "| ".
         05    FILLER      PIC X(25)    VALUE "  DIRECCION".
         05    FILLER      PIC X(02)    VALUE "|".
         05    FILLER      PIC X(08)    VALUE "TELEFONO".
         05    FILLER      PIC X(02)    VALUE "|".
         05    FILLER      PIC X(05)    VALUE "OTRO".
         05    FILLER      PIC X(01)    VALUE "|".

        03     LINEA-DET-NIT.
         05    FILLER      PIC X(01)    VALUE "|".
         05    I-NIT       PIC ZZZZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(03)    VALUE " | ".
         05    I-NOMBM     PIC X(30).
         05    FILLER      PIC X(02)    VALUE " |".
         05    I-DIREM     PIC X(25).
         05    FILLER      PIC X(02)    VALUE " |".
         05    I-TELEM     PIC X(08).
         05    FILLER      PIC X(03)    VALUE " |".
         05    I-FLAG      PIC X(01).
         05    FILLER      PIC X(01)    VALUE "-".
         05    I-CIUDAD    PIC 9(02).
         05    FILLER      PIC X(02)    VALUE " |".
         05    I-SW        PIC 9(01).

        03     TITULO-TOTAL.
         05    FILLER      PIC X(17)  VALUE "TOTAL NITS   :".
         05    T-TOTAL     PIC ZZZZZ.

        03  LIN14-S.
         05 FILLER       PIC X(94)   VALUE ALL "-".
       
        03  LIN15-D.
         05 FILLER       PIC X(94)   VALUE ALL "=".

        03  L-CEROS.
         05 FILLER       PIC X(64)   VALUE SPACES.

        03  L-BLANCO.
         05    FILLER      PIC X(01)    VALUE "|".
         05    FILLER      PIC X(92)    VALUE ALL "-".
         05    FILLER      PIC X(01)    VALUE "|".

        03  L-CIERRE.
         05    FILLER      PIC X(02)    VALUE "| ".
         05    FILLER      PIC X(27)    VALUE SPACES.
         05    FILLER      PIC X(23)    VALUE "*** C O N T I N U A ***".
         05    FILLER      PIC X(40)    VALUE SPACES.
         05    FILLER      PIC X(02)    VALUE " |".

      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\INFORMES.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS        THRU F-USUARIOS.
           IF      SW-USER = 1       EXIT PROGRAM.
           PERFORM I-LABEL-ARCHIVOS   THRU F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS    THRU F-ABRE-ARCHIVOS
           PERFORM I-SELECCION        THRU F-SELECCION
           PERFORM I-TOTAL-INFORME    THRU F-TOTAL-INFORME.
           PERFORM I-FIN-MODE    THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE WEMPRESA               TO  TIT-CIA
           MOVE     1                  TO  IND-INFORMES
           PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
           MOVE LABEL-INFORMES         TO  I-INF
           MOVE "น"                    TO  WK-RAYAS-U
           MOVE "ฬ"                    TO  WK-RAYAS-P
           MOVE "ถ"                    TO  WK-RAYA-U
           MOVE "ว"                    TO  WK-RAYA-P
           MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT  CNNITCED EMPRESAS
           OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.

       I-SELECCION.
            PERFORM I-CONFIRMA-ORDEN        THRU  F-CONFIRMA-ORDEN
            MOVE SPACES                       TO  W-TRUCO.
            PERFORM I-PANTALLA-1            THRU  F-PANTALLA-1
            PERFORM ACEPTA-TRUCO
            IF F4
               PERFORM I-START-NIT          THRU  F-START-NIT
               PERFORM I-INICIA-NIT         THRU  F-INICIA-NIT
            ELSE
              IF F5
                 PERFORM I-START-NOMB-NIT   THRU  F-START-NOMB-NIT
                 PERFORM I-INICIA-NIT       THRU  F-INICIA-NIT
              ELSE
              IF F6
                 PERFORM I-START-NOMB-NIT   THRU  F-START-NOMB-NIT
                 PERFORM I-INICIA-NIT       THRU  F-INICIA-NIT
              ELSE
                  IF F2 PERFORM I-FIN-MODE  THRU  F-FIN-MODE.
       F-SELECCION.
      *
       I-INICIA-NIT.
            PERFORM I-DISPLAY      THRU   F-DISPLAY.
            PERFORM I-INICIA-VAR   THRU   F-INICIA-VAR
            PERFORM I-PROCESO-NIT  THRU   F-PROCESO-NIT
                    UNTIL FIN-OK = 1.
       F-INICIA-NIT.
      *
       I-PROCESO-NIT.
           PERFORM I-LEE-CNNITCED  THRU  F-LEE-CNNITCED
           IF FIN-OK = 1 GO F-PROCESO-NIT.
           DISPLAY LLAVM1            LINE 14 POSITION 27
           DISPLAY LLAV-NOMB         LINE 15 POSITION 27
           PERFORM I-MUEVE-INFORME   THRU F-MUEVE-INFORME
           PERFORM I-IMPRIME-NIT     THRU F-IMPRIME-NIT.
       F-PROCESO-NIT.
      *
      ***************************************************************
      *       O R G A N I Z A C I O N   D E   A R C H I V O S       *
      ***************************************************************
      *
       I-START-NIT.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE ZEROS                  TO  LLAVM1
               MOVE "INFORME ORGANIZADO POR CODIGO" TO I-ORGANIZA
               START CNNITCED KEY NOT LESS LLAVM1 INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-NIT.

       I-START-NOMB-NIT.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE ZEROS                  TO  LLAV-NOMB
               MOVE "INFORME ORGANIZADO POR ALFABETO" TO I-ORGANIZA
               START CNNITCED KEY NOT LESS LLAV-NOMB INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-NOMB-NIT.
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************
       I-LEE-CNNITCED.
              MOVE 0 TO FIN-OK
              READ CNNITCED NEXT RECORD WITH NO LOCK AT END
                                MOVE 1 TO FIN-OK
                                GO F-LEE-CNNITCED.
              IF UU = 1         GO F-LEE-CNNITCED.
              IF UU = 2 AND FLAG-NIT = "C" GO F-LEE-CNNITCED.
              IF UU = 3 AND FLAG-NIT = "E" GO F-LEE-CNNITCED.
              IF UU = 4 AND FLAG-NIT = "N" GO F-LEE-CNNITCED.
              IF UU = 5
                IF FLAG-NIT = "E" OR FLAG-NIT = "C" GO F-LEE-CNNITCED.
              GO I-LEE-CNNITCED.
       F-LEE-CNNITCED.
      ***************************************************************
      *        P R O C E S O   D E   P A N T A L L A S              *
      ***************************************************************

       I-DISPLAY.
      *JJT     PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
                                              LINE 13 POSITION 15
               DISPLAY "บ No.N.I.T  :
      -       "      บ"
                                              LINE 14 POSITION 15
               DISPLAY "บ
      -       "      บ"
                                              LINE 15 POSITION 15
               DISPLAY "บ
      -       "      บ"
                                              LINE 16 POSITION 15
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"
                                              LINE 17 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 20 POSITION 12 CONTROL
                            SG-CONTROL-STRING
               DISPLAY  SPACES LINE 20 POSITION 2 SIZE 78
               "CATALOGO DE NITS ---------->" LINE 20 POSITION 05
                LABEL-INFORMES                LINE 20 POSITION 33
                NOM-PAPEL(IND-INFORMES)       LINE 20 POSITION 66
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 24 POSITION 80 CONTROL
                            SG-CONTROL-STRING.
       F-DISPLAY.
      *
       I-PANTALLA-1.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
               DISPLAY " "  LINE 20 POSITION 12 CONTROL
                            SG-CONTROL-STRING
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 07 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 08 POSITION 20
               DISPLAY "บ   [F4]    INFORME POR CODIGOS              บ"
                                               LINE 09 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 10 POSITION 20
               DISPLAY "บ   [F5]    INFORME ALFABETICO               บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 12 POSITION 20
               DISPLAY "บ   [F2] TERMINAR                            บ"
                                               LINE 13 POSITION 20
               DISPLAY "บ                                            บ"
                                               LINE 14 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 15 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1.
      *
      ***************************************************************
      *          M O V I M I E N T O  DE   C A M P O S              *
      ***************************************************************

       I-MUEVE-INFORME.
            MOVE CODIM        TO I-NIT
            MOVE NOMBM        TO I-NOMBM.
            MOVE TELEM        TO I-TELEM.
            MOVE DIREM        TO I-DIREM.
            MOVE CIUDM        TO I-CIUDAD
            MOVE FLAG-NIT     TO I-FLAG.
            MOVE SW-CNNITCED  TO I-SW.
       F-MUEVE-INFORME.
      *
       I-INICIA-VAR.
            MOVE 0 TO FIN-OK  WK-PAGINA W-CONTA SW1.
       F-INICIA-VAR.

      ***************************************************************
      *          A R E A   D E   I M P R E S I O N                  *
      ***************************************************************
      *
       I-IMPRIME-NIT.
            IF CON-LIN > 55
                   PERFORM LINE-CIERRE   THRU F-LINE-CIERRE
                   PERFORM I-TITULO-NIT  THRU F-TITULO-NIT.
            WRITE REG-INFORMES FROM LINEA-DET-NIT AFTER 1
            ADD 1                      TO CON-LIN.
            ADD 1                      TO W-CONTA.
       F-IMPRIME-NIT.
      *
       LINE-CIERRE.
            IF CON-LIN = 66  GO F-LINE-CIERRE.
            WRITE REG-INFORMES FROM L-BLANCO AFTER 1
            WRITE REG-INFORMES FROM L-CIERRE AFTER 1
            WRITE REG-INFORMES FROM LIN14-S AFTER 1.
       F-LINE-CIERRE.
      *
      ***************************************************************
      *          I M P R E S I O N    DE   T I T U L O S            *
      ***************************************************************

       I-TITULO-NIT.
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
               WRITE REG-INFORMES  FROM  TITULO-0  AFTER PAGE

               WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1

               WRITE REG-INFORMES  FROM  TITULO-2  AFTER 1

               WRITE REG-INFORMES  FROM  TITULO-3  AFTER 1
               WRITE REG-INFORMES  FROM  LIN15-D   AFTER 1
               WRITE REG-INFORMES  FROM  TITULO-4  AFTER 1
               WRITE REG-INFORMES  FROM  LIN15-D  AFTER 1
               MOVE 7                          TO  CON-LIN.
       F-TITULO-NIT.
      *
       I-TOTAL-INFORME.
               MOVE W-CONTA        TO   T-TOTAL
               WRITE REG-INFORMES  FROM LIN15-D      AFTER 1
               WRITE REG-INFORMES  FROM L-CEROS      AFTER 2
               WRITE REG-INFORMES  FROM TITULO-TOTAL AFTER 1.
       F-TOTAL-INFORME.

      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           CLOSE EMPRESAS
                 INFORMES
                 CNNITCED.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE ZEROS                  TO  CON-PERIODO
           CANCEL "PROG742.COB"
      *     CALL "PROG737.COB" USING AREA-LINK-NOMINA.
      *     CANCEL "PROG737.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-CONFIRMA-ORDEN.
           MOVE "** CATALOGO DE N.I.T.S *** "
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           DISPLAY " " LINE 25 POSITION 79 CONTROL SG-CONTROL-STRING
           "ษออออออหอออออออออออออออออออออออออออออป" LINE 07 POSITION 25
           "บ      บ Seleccione Los NITS deseadosบ" LINE 08 POSITION 25
           "ฬออออออฮอออออออออออออออออออออออออออออน" LINE 09 POSITION 25
           "บ <F3> บ NIT - Todos                 บ" LINE 10 POSITION 25
           "บ <F4> บ  C  - Solo Contables        บ" LINE 11 POSITION 25
           "บ <F5> บ  E  - Solo Empresas         บ" LINE 12 POSITION 25
           "บ <F6> บ  N  - Solo Nomina           บ" LINE 13 POSITION 25
           "บ <F7> บ  EC - Empresas y Contables  บ" LINE 14 POSITION 25
           "ศออออออสอออออออออออออออออออออออออออออผ" LINE 15 POSITION 25.
      *
            DISPLAY " PRESIONE FUNCION DESEADA"
                                    LINE 24 POSITION 1
            PERFORM ACEPTA-TRUCO
            MOVE  1   TO UU
            IF  F2  PERFORM I-FIN-MODE  THRU  F-FIN-MODE.
            IF  F3   MOVE  1   TO UU.
            IF  F4   MOVE  2   TO UU.
            IF  F5   MOVE  3   TO UU.
            IF  F6   MOVE  4   TO UU.
            IF  F7   MOVE  5   TO UU.
       F-CONFIRMA-ORDEN.
      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLAS.PRO".
               COPY "..\PLB\INFORMES.PLB".
