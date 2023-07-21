       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG737.
      ******************************************************************
      *        IMPRESION DE TODOS LOS LISTADOS DESDE EL PROGRAMA       *
      *               CAPTURANDO LA COLA DE IMPRESION  ABR-27-2002     *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\USER-ACT.DYC".
       DATA DIVISION.
       FILE SECTION.
           COPY "..\FD\EMPRESAS.FD".
           COPY "..\FD\USER-ACT.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA              PIC X(07)  VALUE "PROG737".
       77      ID-PRG                  PIC X(7)   VALUE "PROG737".
       77      W-VARIABLES-NOMINA      PIC XX     VALUE SPACES.
       77      WK-RAYAS-U     PIC X(1) VALUE SPACES.
       77      WK-RAYAS-P     PIC X(1) VALUE SPACES.
       77      WK-RAYA-U      PIC X(1) VALUE SPACES.
       77      WK-RAYA-P      PIC X(1) VALUE SPACES.
      *
           COPY "..\LBL\EMPRESAS.LBL".
           COPY "..\LBL\USER-ACT.LBL".
      *
           COPY "..\WRK\TABLAMES.WRK".
           COPY "..\WRK\TECLADOS.WRK".
           COPY "..\WRK\IO-ERROR.WRK".
           COPY "..\WRK\VARIABLE.WRK".
      *----------------------------------------------------------------*
      * .WRK COMANDOS                                                  *
      *----------------------------------------------------------------*
       01 W-VARIABLES-COMANDO-DOS.
        03 W-COMANDO-DOS.
           05  I-COMANDO     PIC X(07)  VALUE  "NPRINT ".
           05  I-DIRECT-1    PIC X(04)  VALUE   SPACES.
           05  FILLER        PIC X(01)  VALUE  "\".
           05  I-DIRECT-2    PIC X(08)  VALUE   SPACES.
           05  FILLER        PIC X(01)  VALUE  "\".
           05  I-LETRA       PIC X(01)  VALUE  SPACES.
           05  I-EMP         PIC 9(07)  VALUE  ZEROS.
           05  FILLER        PIC X(01)  VALUE  ".".
           05  I-PROGRAMA    PIC 9(03)  VALUE  ZEROS.
      *    05  I-ANO         PIC 9(02)  VALUE  ZEROS.
      *    05  I-PER         PIC 9(02)  VALUE  ZEROS.
           05  I-Q           PIC X(03)  VALUE  " Q=".
           05  I-IMPRESOR    PIC X(12)  VALUE  SPACES.
           05  I-BANER       PIC X(04)  VALUE  " NB ".
           05  I-PAGINA      PIC X(03)  VALUE  SPACES.
      ******************************************************************
           COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION            USING AREA-LINK-NOMINA.
       DECLARATIVES.
           COPY "..\DCL\EMPRESAS.DCL".
           COPY "..\DCL\USER-ACT.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
              COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-MVTO-DATOS      THRU F-MVTO-DATOS.
           PERFORM I-PANTALLA        THRU F-PANTALLA
           PERFORM I-ACEPTA-DATOS    THRU F-ACEPTA-DATOS.
           EXIT PROGRAM.
       F-MAIN-PROCESS.
      *
       I-PANTALLA.
           MOVE "*** IMPRIMIR AL SERVIDOR DE IMPRESORAS ***"
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
           DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                           LINE 10 POSITION 20
           DISPLAY "บ  DESEA IMPRIMIR <S/N> :                    บ"
                                           LINE 11 POSITION 20
           DISPLAY "บ  NOMBRE IMPRESORA     :                    บ"
                                           LINE 12 POSITION 20
           DISPLAY "บ     *** SERVICIO SOLO PARA REDES ***       บ"
                                           LINE 13 POSITION 20
           DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                           LINE 14 POSITION 20
           DISPLAY WK-ESPACIOS             LINE 24 POSITION 1.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY " "  LINE 25 POSITION 12 CONTROL
                        SG-CONTROL-STRING
           DISPLAY W-COMANDO-DOS LINE 16 POSITION 10.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " "  LINE 25 POSITION 12 CONTROL
                        SG-CONTROL-STRING.
       F-PANTALLA.
      *
       I-MVTO-DATOS.
              MOVE CON-ANO            TO  I-DIRECT-1
              MOVE "091_2X11"            TO  I-DIRECT-2
              MOVE "I"                TO  I-LETRA
              MOVE "FF"               TO  I-PAGINA.
              MOVE CON-PROGRAMA       TO  I-PROGRAMA
      *       MOVE CON-ANO            TO  I-ANO
      *       MOVE WIN-NOMINA         TO  I-PER
              MOVE WIN-CODIGO-EMPRESA TO  I-EMP


            IF CON-PROGRAMA = 161
               MOVE CON-ANO            TO  I-DIRECT-1
               MOVE "091_2X11"            TO  I-DIRECT-2
               MOVE "R"                TO  I-LETRA
               MOVE "NFF"              TO  I-PAGINA.

            IF CON-PROGRAMA = 305 OR CON-PROGRAMA = 307 OR
               CON-PROGRAMA = 206
               MOVE CON-ANO            TO  I-DIRECT-1
               MOVE "FORMATOS"         TO  I-DIRECT-2
               MOVE "R"                TO  I-LETRA
               MOVE "NFF"              TO  I-PAGINA.

            IF CON-PROGRAMA = 325 OR CON-PROGRAMA = 326 OR
               CON-PROGRAMA = 151
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "FORMATOS"         TO  I-DIRECT-2
      *          MOVE  ZEROS             TO  I-PER
                 MOVE "I"                TO  I-LETRA
                 MOVE "NFF"              TO  I-PAGINA.

            IF CON-PROGRAMA = 743 OR CON-PROGRAMA = 744 OR
               CON-PROGRAMA = 745  OR
               CON-PROGRAMA = 741  OR
               CON-PROGRAMA = 742  OR
               CON-PROGRAMA = 748  OR
               CON-PROGRAMA = 747
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "091_2X11"            TO  I-DIRECT-2
      *          MOVE  ZEROS             TO  I-PER
                 MOVE "I"                TO  I-LETRA
                 MOVE "NFF"              TO  I-PAGINA.

            IF CON-PROGRAMA = 872 OR CON-PROGRAMA = 873
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "FORMATOS"         TO  I-DIRECT-2
      *          MOVE  W-PERIODO         TO  I-PER
                 MOVE "I"                TO  I-LETRA
                 MOVE "NFF"              TO  I-PAGINA.

            IF CON-PROGRAMA = 875
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "091_2X11"            TO  I-DIRECT-2
      *          MOVE  W-PERIODO         TO  I-PER
                 MOVE "R"                TO  I-LETRA
                 MOVE "NFF"              TO  I-PAGINA.

            IF CON-PROGRAMA = 752 OR CON-PROGRAMA = 753 OR
               CON-PROGRAMA = 754   OR
               CON-PROGRAMA = 755   OR
               CON-PROGRAMA = 756   OR
               CON-PROGRAMA = 757   OR
               CON-PROGRAMA = 758   OR
               CON-PROGRAMA = 759   OR
               CON-PROGRAMA = 770   OR
               CON-PROGRAMA = 771   OR
               CON-PROGRAMA = 772   OR
               CON-PROGRAMA = 768   OR
               CON-PROGRAMA = 773   OR
               CON-PROGRAMA = 774   OR
               CON-PROGRAMA = 775   OR
               CON-PROGRAMA = 796
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "091_2X11"            TO  I-DIRECT-2
      *          MOVE  W-PERIODO         TO  I-PER
                 MOVE "I"                TO  I-LETRA
                 MOVE "NFF"              TO  I-PAGINA.

            IF CON-PROGRAMA = 661
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "091_2X11"            TO  I-DIRECT-2
      *          MOVE  W-PERIODO         TO  I-PER
                 MOVE "I"                TO  I-LETRA
                 MOVE "NFF"              TO  I-PAGINA.

            IF CON-PROGRAMA = 663 or CON-PROGRAMA = 662
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "091_2X11"            TO  I-DIRECT-2
      *          MOVE  W-PERIODO         TO  I-PER
                 MOVE "I"                TO  I-LETRA
                 MOVE "NFF"              TO  I-PAGINA.

            IF CON-PROGRAMA = 798
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "147_8X11"           TO  I-DIRECT-2
      *          MOVE  W-PERIODO         TO  I-PER
                 MOVE "I"                TO  I-LETRA
                 MOVE "FF"               TO  I-PAGINA.

            IF CON-PROGRAMA = 651
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "147_8X11"           TO  I-DIRECT-2
                 MOVE  007               TO  I-EMP
      *          MOVE  W-PERIODO         TO  I-PER
                 MOVE "I"                TO  I-LETRA
                 MOVE "FF"               TO  I-PAGINA.

            IF CON-PROGRAMA = 769
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "FORMATOS"         TO  I-DIRECT-2
      *          MOVE  W-PERIODO         TO  I-PER
                 MOVE "R"                TO  I-LETRA
                 MOVE "NFF"              TO  I-PAGINA.

            IF CON-PROGRAMA = 864
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "FORMATOS"         TO  I-DIRECT-2
      *          MOVE  W-PERIODO         TO  I-PER
                 MOVE "R"                TO  I-LETRA
                 MOVE "NFF"              TO  I-PAGINA.

            IF CON-PROGRAMA = 385
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "147_8X11"           TO  I-DIRECT-2
      *          MOVE  CON-PERIODO       TO  I-PER
                 MOVE "L"                TO  I-LETRA
                 MOVE "NFF"              TO  I-PAGINA.

            IF CON-PROGRAMA = 835
                 MOVE CON-ANO            TO  I-DIRECT-1
                 MOVE "147_8X11"           TO  I-DIRECT-2
      *          MOVE  ZEROS             TO  I-PER
                 MOVE "I"                TO  I-LETRA
                 MOVE "FF"               TO  I-PAGINA.
       F-MVTO-DATOS.
      *
       I-ACEPTA-DATOS.
            PERFORM I-CONFIRMA-IMP  THRU F-CONFIRMA-IMP.
            IF  W-OPCION = "N"        GO F-ACEPTA.
            PERFORM I-ACEPTA-IMP    THRU F-ACEPTA-IMP.
            CALL "SYSTEM"          USING W-COMANDO-DOS.
            GO    F-ACEPTA-DATOS.
       F-ACEPTA.
            IF CON-PROGRAMA = 305 OR CON-PROGRAMA = 307 OR
               CON-PROGRAMA = 206 OR CON-PROGRAMA = 875 OR
               CON-PROGRAMA = 325 OR CON-PROGRAMA = 326 OR
               CON-PROGRAMA = 150 OR CON-PROGRAMA = 151 OR
               CON-PROGRAMA = 872 OR CON-PROGRAMA = 873 OR
               CON-PROGRAMA = 769 OR CON-PROGRAMA = 864 OR
               CON-PROGRAMA = 161
               GO  F-ACEPTA-DATOS.

              MOVE SPACES             TO  I-PAGINA.
              MOVE SPACES             TO  I-Q
              MOVE SPACES             TO  I-BANER
              MOVE "EDIT"             TO  I-COMANDO.
              DISPLAY W-COMANDO-DOS LINE 15 POSITION 1
              PERFORM ACEPTA-TRUCO
              CALL "SYSTEM"          USING W-COMANDO-DOS.
       F-ACEPTA-DATOS.
      *
       I-CONFIRMA-IMP.
               MOVE ZEROS       TO  WK-EXCEPTION
               ACCEPT W-OPCION      LINE 11 POSITION 50 REVERSE
                                    TAB NO BEEP
                                    ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
               IF  W-OPCION   = "S" GO F-CONFIRMA-IMP.
               IF  W-OPCION   = "N" GO F-CONFIRMA-IMP.
               GO I-CONFIRMA-IMP.
       F-CONFIRMA-IMP.
      *
       I-ACEPTA-IMP.
               MOVE ZEROS       TO  WK-EXCEPTION
               ACCEPT I-IMPRESOR    LINE 12 POSITION 50 REVERSE
                                    TAB NO BEEP
                                    ON EXCEPTION WK-EX
                                    PERFORM 999-EXCEPTION.
       F-ACEPTA-IMP.
      *
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\PANTALLA.PRO".
      ******************************************************************
      *

