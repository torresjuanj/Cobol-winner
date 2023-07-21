       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG769.
      ******************************************************************
      * IMPRIME TODOS LOS FORMATOS CONTABLES  ABR-27-2002              *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\CNDNOTAS.DYC".
               COPY "..\DYC\CNDOCTOS.DYC".
               COPY "..\DYC\CNCATCTA.DYC".
               COPY "..\DYC\CNCOMPTE.DYC".
               COPY "..\DYC\CNNITCED.DYC".
               COPY "..\SEL\REPORTES.SEL".
               COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDNOTAS.FD".
               COPY "..\FD\CNDOCTOS.FD".
               COPY "..\FD\CNCOMPTE.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\REPORTES.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA          PIC X(7)  VALUE "PROG769".
       77      ID-PRG              PIC X(7)  VALUE "PROG769".
       77      AA                  PIC 9     VALUE ZEROS.
       77      DIA-P               PIC 99    VALUE ZEROS.
       77      JJ                  PIC 9     VALUE ZEROS.
       77      WK-RADICA           PIC 9(6)  VALUE ZEROS.
       77      EGRE-INICIAL        PIC 9(6)  VALUE ZEROS.
       77      EGRE-FINAL          PIC 9(6)  VALUE ZEROS.
       77      WK-TIPO             PIC 9(2)  VALUE ZEROS.
       77      ANIO                PIC 9(4)  VALUE ZEROS.
       77      IMPRE               PIC 9     VALUE ZEROS.
       77      W-VARIABLES-NOMINA  PIC 9(3)  VALUE ZEROS.
         01  TABLA-REG.
           03 TAB-REG                 OCCURS    300 TIMES.
              05  TAB-CUENTA.
                  07  TAB-MAYCTA      PIC 9(04).
                  07  TAB-SUBCTA      PIC 9(02).
                  07  TAB-AUXCTA      PIC 9(04).
              05  TAB-NUMFRA          PIC 9(06).
              05  TAB-NIT             PIC 9(12).
              05  TAB-CONCEPTO        PIC X(60).
              05  TAB-VLRDOC          PIC S9(12)V99.
              05  TAB-VLRBAS          PIC 9(09).

               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\CNDOCTOS.LBL".
               COPY "..\LBL\CNCOMPTE.LBL".
               COPY "..\LBL\CNDNOTAS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\REPORTES.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\RUTMONTO.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
      *        COPY "..\WRK\IMEGRESO.WRK".
               COPY "..\WRK\FORMCONT.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\LISTADOS.WRK".
               COPY "..\LNK\CONTROLA.LNK".
      ***************************************************************
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\CNDNOTAS.DCL".
               COPY "..\DCL\CNDOCTOS.DCL".
               COPY "..\DCL\CNCOMPTE.DCL".
               COPY "..\DCL\INFORMES.DCL".
               COPY "..\DCL\REPORTES.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS            THRU F-USUARIOS.
           IF      SW-USER = 1           EXIT PROGRAM.
           PERFORM I-PANTALLA-1B         THRU F-PANTALLA-1B
           PERFORM I-ACEPTA-MES          THRU F-ACEPTA-MES.
           PERFORM I-CONFIRMA-COMP       THRU F-CONFIRMA-COMP.
       I-OTRO.
           DISPLAY JJ LINE 23 POSITION 1.
           PERFORM I-LABEL-ARCHIVOS      THRU F-LABEL-ARCHIVOS
           PERFORM I-MENU-OPCIONES       THRU F-MENU-OPCIONES
               PERFORM I-FIN-MODE            THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
           MOVE "IMPRESION DE TODOS LOS FORMATOS" TO  TIT-APLICACION
           MOVE  WIN-EMPRESA           TO  WK-COMPANIA
           MOVE   "น"                  TO  WK-RAYAS-U
           MOVE   "ฬ"                  TO  WK-RAYAS-P
           MOVE   "ถ"                  TO  WK-RAYA-U
           MOVE   "ว"                  TO  WK-RAYA-P.
           MOVE   16                   TO  POS-DOWN.
           MOVE    4                   TO  IND-REPORTES
           MOVE    3                   TO  IND-INFORMES
           PERFORM I-LABEL-REPORTES  THRU  F-LABEL-REPORTES.
           PERFORM I-LABEL-INFORMES  THRU  F-LABEL-INFORMES.
       F-LABEL-ARCHIVOS.
      *
       I-MENU-OPCIONES.
           MOVE W-LINEA TO I-LINEA
           MOVE 999             TO   WK-EXCEPTION
           MOVE ZEROS           TO   FIN-OK
           MOVE  1              TO   SW-MODE
           IF IMPRE =  2
              PERFORM I-WINDOW-DATO1     THRU  F-WINDOW-DATO1
              PERFORM I-ACEPTA-EGRESO    THRU  F-ACEPTA-EGRESO.
           PERFORM I-IMPRIME-EGRESO      THRU  F-IMPRIME-EGRESO.
       F-MENU-OPCIONES.
      *
      ***************************************************************
      *                   LECTURA  DE  ARCHIVOS                     *
      ***************************************************************
      *
       I-FIN-MODE.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE W-LONG                 TO  CON-PERIODO
           CANCEL "PROG769.COB"
           CALL "PROG737.COB" USING AREA-LINK-NOMINA.
           CANCEL "PROG737.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
       I-WINDOW-DATO1.
           DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออป"
                                        LINE 19 POSITION 20.
           DISPLAY "บ  REIMPRESION DE TODOS LOS FORMATOS       บ"
                                        LINE 20 POSITION 20.
           DISPLAY "ฬออออออออออออออออออออออออออออออออออออออออออน"
                                        LINE 21 POSITION 20.
           DISPLAY "บ   DESDE :        HASTA :                 บ"
                                        LINE 22 POSITION 20.
           DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออผ"
                                        LINE 23 POSITION 20.
       F-WINDOW-DATO1.
      *
       I-ACEPTA-EGRESO.
           DISPLAY SPACES  LINE 22  POSITION 33 SIZE 05 REVERSE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           ACCEPT  EGRE-INICIAL LINE 22 POSITION 33
                   UPDATE TAB NO BEEP PROMPT ECHO REVERSE
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO I-ACEPTA-FINAL.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY EGRE-INICIAL LINE 22 POSITION 33  REVERSE
           IF  EGRE-INICIAL = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 24 POSITION 1
                  REVERSE BEEP GO I-ACEPTA-EGRESO.
           DISPLAY WK-ESPACIOS LINE 24 POSITION 1.
       I-ACEPTA-FINAL.
           DISPLAY SPACES  LINE 22  POSITION 49 SIZE 05 REVERSE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           ACCEPT  EGRE-FINAL   LINE 22 POSITION 49
                   UPDATE TAB NO BEEP PROMPT ECHO REVERSE
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO I-ACEPTA-EGRESO.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           DISPLAY EGRE-FINAL   LINE 22 POSITION 49  REVERSE
           IF  EGRE-FINAL   = ZEROS
               DISPLAY
                 "ERROR CODIGO INCONSISTENTE" LINE 24 POSITION 1
                  REVERSE BEEP GO I-ACEPTA-FINAL.
           DISPLAY WK-ESPACIOS LINE 24 POSITION 1.

           MOVE ZEROS                  TO  WK-EXCEPTION
           ACCEPT  DIA-P        LINE 23 POSITION 49
                   UPDATE TAB NO BEEP PROMPT ECHO REVERSE
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
       F-ACEPTA-EGRESO.
      *
       I-PANTALLA-1B.
           MOVE "   IMPRESION DE TODOS LOS FORMATOS "
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
           PERFORM I-PANTA           THRU F-PANTA
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           DISPLAY " "
              LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.

               DISPLAY
                "ษอออออออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 05 POSITION 15
                "บ COMPROBANTE   :                                 บ"
                                               LINE 06 POSITION 15
                "บ MES A IMPRIMIR:                                 บ"
                                               LINE 07 POSITION 15
                "ศอออออออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 08 POSITION 15
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           DISPLAY " "
              LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY
           "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 09 POSITION 10
           "บ <01> Recibos de caja           <02> Consignaciones      บ"
                                               LINE 10 POSITION 10
           "บ <03> Comp.de Egreso            <04> Facturaciขn         บ"
                                               LINE 11 POSITION 10
           "บ <05> Nov.especiales Nomina     <06> Cuentas por pagar   บ"
                                               LINE 12 POSITION 10
           "บ <07> Costos de Nomina          <08> Costos Liquidacionesบ"
                                               LINE 13 POSITION 10
           "บ <09> Depreciaciones            <10> Amortizaciones      บ"
                                               LINE 14 POSITION 10
           "บ <11> Notas Bancarias           <12> Notas Credito/Clientบ"
                                               LINE 15 POSITION 10
           "บ <13> Ajustes                   <14> Notas Debito/Clienteบ"
                                               LINE 16 POSITION 10
           "บ <15> Prov.Ind.y Comercio       <20> Notas de Contabilid.บ"
                                               LINE 17 POSITION 10
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 18 POSITION 10 .

           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           DISPLAY " "
              LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
            DISPLAY "<  >"  LINE 10 POSITION 12
            DISPLAY "<  >"  LINE 10 POSITION 43
            DISPLAY "<  >"  LINE 11 POSITION 12
            DISPLAY "<  >"  LINE 11 POSITION 43
            DISPLAY "<  >"  LINE 12 POSITION 12
            DISPLAY "<  >"  LINE 12 POSITION 43
            DISPLAY "<  >"  LINE 13 POSITION 12
            DISPLAY "<  >"  LINE 13 POSITION 43
            DISPLAY "<  >"  LINE 14 POSITION 12
            DISPLAY "<  >"  LINE 14 POSITION 43
            DISPLAY "<  >"  LINE 15 POSITION 12
            DISPLAY "<  >"  LINE 15 POSITION 43
            DISPLAY "<  >"  LINE 16 POSITION 12
            DISPLAY "<  >"  LINE 16 POSITION 43
            DISPLAY "<  >"  LINE 17 POSITION 12
            DISPLAY "<  >"  LINE 17 POSITION 43.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           DISPLAY " "
              LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
            DISPLAY "01"  LINE 10 POSITION 13
            DISPLAY "02"  LINE 10 POSITION 44
            DISPLAY "03"  LINE 11 POSITION 13
            DISPLAY "04"  LINE 11 POSITION 44
            DISPLAY "05"  LINE 12 POSITION 13
            DISPLAY "06"  LINE 12 POSITION 44
            DISPLAY "07"  LINE 13 POSITION 13
            DISPLAY "08"  LINE 13 POSITION 44
            DISPLAY "09"  LINE 14 POSITION 13
            DISPLAY "10"  LINE 14 POSITION 44
            DISPLAY "11"  LINE 15 POSITION 13
            DISPLAY "12"  LINE 15 POSITION 44
            DISPLAY "13"  LINE 16 POSITION 13
            DISPLAY "14"  LINE 16 POSITION 44
            DISPLAY "15"  LINE 17 POSITION 13
            DISPLAY "16"  LINE 17 POSITION 44.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY " "
              LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-PANTALLA-1B.
      *
       I-ACEPTA-MES.
               MOVE ZEROS        TO  WK-EXCEPTION
               ACCEPT SW-OPCION LINE 06 POSITION 35
                                CONVERT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
               MOVE SW-OPCION TO CODIM-COMP.
               OPEN INPUT CNCOMPTE.
               READ CNCOMPTE WITH NO LOCK INVALID KEY
                    CLOSE CNCOMPTE
                    GO I-ACEPTA-MES.
               DISPLAY NOMBM-COMP  LINE 06 POSITION 38.
               CLOSE CNCOMPTE.
           IF SW-OPCION =  4
              CANCEL "PROG769.COB"
              CALL "PROG794.COB" USING AREA-LINK-NOMINA
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
        I-OPCION.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-LONG                   LINE 07 POSITION 35
                                CONVERT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               IF CUP GO I-ACEPTA-MES.
               IF  W-LONG  =  0 OR W-LONG > 12
                   DISPLAY " CODIGO DEL MES ERRADO...DIGITE NUEVAMENTE"
                                                    LINE 24 POSITION 1
                                                    BLINK
                   GO I-OPCION.
               IF CUP GO I-ACEPTA-MES.
               IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 07 POSITION 38.
       F-ACEPTA-MES.
      *
       I-CONFIRMA-COMP.
             MOVE SG-COLOR-TABLE(3) TO SG-FCOLOR.
             MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
             DISPLAY " "
                LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
             DISPLAY "PRESS--> <F4> TODO EL MES " LINE 25 POSITION 1
             DISPLAY " [F5] RANGO ESPECIAL  "   LINE 25 POSITION 35
             DISPLAY " [F2] SALIR"              LINE 25 POSITION 67
             MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
             MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
             DISPLAY " "
                    LINE   25  POSITION 80 CONTROL SG-CONTROL-STRING
             DISPLAY "<F4>" LINE 25 POSITION 10
             DISPLAY "[F5]" LINE 25 POSITION 36
             DISPLAY "[F2]" LINE 25 POSITION 68
             PERFORM ACEPTA-TRUCO
             DISPLAY  WK-ESPACIOS  LINE 25 POSITION  1.
             IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
             IF  F4  MOVE 1  TO  IMPRE
                     MOVE ZEROS  TO EGRE-INICIAL
                     MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
                     MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
                     DISPLAY " "   LINE   24  POSITION 80
                     CONTROL SG-CONTROL-STRING
                     GO F-CONFIRMA-COMP.
             IF  F8  MOVE 1  TO  IMPRE
                     MOVE 5  TO JJ
                     MOVE ZEROS  TO EGRE-INICIAL
                     MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
                     MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
                     DISPLAY " "   LINE   24  POSITION 80
                     CONTROL SG-CONTROL-STRING
                     GO F-CONFIRMA-COMP.
             IF  F5  MOVE 2  TO  IMPRE
                     MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
                     MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
                     DISPLAY " "   LINE   24  POSITION 80
                     CONTROL SG-CONTROL-STRING
                     GO F-CONFIRMA-COMP.

             IF  F12 MOVE 2  TO  IMPRE
                     MOVE  6  TO JJ
                     MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR
                     MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR
                     DISPLAY " "   LINE   24  POSITION 80
                     CONTROL SG-CONTROL-STRING
                     GO F-CONFIRMA-COMP.
             GO I-CONFIRMA-COMP.
       F-CONFIRMA-COMP.
      *
      *  ฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒ
      *  ฒฒ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ ฒฒ
      *  ฒฒณ       Diseคo de Pantalla y visualizacion datos          ณฒฒ
      *  ฒฒ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ ฒฒ
      *  ฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒ
      *
       I-PANTALLA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(6) TO SG-BCOLOR.
           DISPLAY
           " <<   IMPRESION DE TODOS LOS FORMATOS  >>"
                     LINE    8  POSITION   20 BEEP
                     CONTROL SG-CONTROL-STRING HIGH.
               DISPLAY
           "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
            LINE 19 POSITION 15 CONTROL SG-CONTROL-STRING HIGH

           "บ  COMPROBANTE No.                                        บ"
                                     LINE 20 POSITION 15

           "บ  CUENTA CONTABLE                                        บ"
                                     LINE 21 POSITION 15

           "บ  BENEFICIARIO                                           บ"
                                     LINE 22 POSITION 15

           "บ  VALOR                                                  บ"
                                     LINE 23 POSITION 15

           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
                       LINE 24 POSITION 15.
       F-PANTALLA.
      *
       I-STATUS-CNDOCTOS.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT CNDOCTOS
               MOVE 1                      TO  SW-ABRE
               IF  CNDOCTOS-STATUS              =  "35"
                   OPEN OUTPUT CNDOCTOS
               ELSE
                   IF  CNDOCTOS-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-CNDOCTOS
                       PERFORM I-FIN-MODE THRU F-FIN-MODE.
               CLOSE CNDOCTOS.
       F-STATUS-CNDOCTOS.
      ****************************************************************

               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PRO\MONTOESC.PRO".
               COPY "..\PRO\FORMCONT.PRO".

               COPY "..\PLB\CNDOCTOS.PLB".
               COPY "..\PLB\REPORTES.PLB".
               COPY "..\PLB\INFORMES.PLB".
               COPY "..\PLB\CNDNOTAS.PLB".


