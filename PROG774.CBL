       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG774.
      ******************************************************************
      * ESTADO DE PERDIDAS Y GANANCIAS ACUMULADO  ABR-27-2002          *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CNDTACUM.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG774".
       77      ID-PRG         PIC X(7)       VALUE "PROG774".
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\INFORMES.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
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
         05    FILLER      PIC X(02)    VALUE SPACES.
         05    TIT-CIA     PIC X(40)    VALUE SPACES.

        03     TITULO-1.
          05    FILLER      PIC X(03)    VALUE SPACES.
         05    FILLER      PIC X(10)    VALUE "N.I.T No: ".
         05    I-NITEMP    PIC ZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(01)    VALUE "-".
         05    I-NITEXT    PIC Z.

        03     TITULO-2.
         05    FILLER      PIC X(21)    VALUE SPACES.
         05    FILLER      PIC X(45)    VALUE
               "ESTADO DE PERDIDAS Y GANANCIAS ACUMULADO".

        03     TITULO-2A.
          05    FILLER       PIC X(29)        VALUE SPACES.
         05    I-MES        PIC X(10)        VALUE SPACES.
         05    FILLER       PIC X(4)         VALUE " de ".
         05    I-ANO        PIC X(04)        VALUE SPACES.
         05    FILLER      PIC X(45)         VALUE SPACES.

        03     TITULO-2B.
         05    FILLER      PIC X(22) VALUE "Fecha de Elaboracion: ".
         05    I-FECHA     PIC X(11).
         05    FILLER      PIC X(08)         VALUE "  Hora: ".
         05    I-HORA      PIC X(05).
         05    I-CAMPO     PIC X(035)   VALUE SPACES.
         05    FILLER       PIC X(50)   VALUE SPACES.
         05    FILLER      PIC X(08)    VALUE "Pagina: ".
         05    I-PAGINA    PIC ZZZZ     VALUE ZEROS.
         05    FILLER      PIC X(10)    VALUE SPACES.
  

        03     TITULO-3.
         05    FILLER      PIC X(233)   VALUE ALL "=".

      *
        03  LIN14-S.
           05 FILLER          PIC X(07)       VALUE " ".
           05 FILLER          PIC X(35)       VALUE SPACES.
           05 FILLER          PIC X(01)       VALUE " ".
           05 FILLER          PIC X(18)       VALUE SPACES.
           05 FILLER          PIC X(01)       VALUE " ".
           05 FILLER          PIC X(18)       VALUE ALL "-".
           05 FILLER          PIC X(01)       VALUE " ".
       
        03  LIN15-S.
           05 FILLER          PIC X(07)       VALUE " ".
           05 FILLER          PIC X(35)       VALUE SPACES.
           05 FILLER          PIC X(01)       VALUE " ".
           05 FILLER          PIC X(18)       VALUE SPACES.
           05 FILLER          PIC X(01)       VALUE " ".
           05 FILLER          PIC X(18)       VALUE ALL "=".
           05 FILLER          PIC X(01)       VALUE " ".
      *
        03 LDET1.
           05 FILLER          PIC X(07)       VALUE " ".
           05 I-NOMCTA          PIC X(35)       VALUE SPACES.
           05 FILLER          PIC X(01)       VALUE " ".
           05 VR-NIV3         PIC   ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER          PIC X(01)       VALUE " ".
           05 VR-NIV2         PIC   ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER          PIC X(01)       VALUE " ".
      *
      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\INFORMES.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
      *
       I-MAIN-PROCESS.
               COPY "..\PRO\CONTROLA.PRO".
               INITIALIZE WK-EXCEPTION
               PERFORM I-USUARIOS       THRU F-USUARIOS.
               IF      SW-USER = 1      EXIT PROGRAM.
               MOVE    1                   TO   IND-INFORMES.
               PERFORM I-PANTALLA-1B     THRU F-PANTALLA-1B
               PERFORM I-ACEPTA-MES      THRU F-ACEPTA-MES
               PERFORM I-CONFIRMA-COMP   THRU F-CONFIRMA-COMP
               PERFORM I-LABEL-ARCHIVOS  THRU F-LABEL-ARCHIVOS
               PERFORM I-ABRE-ARCHIVOS   THRU F-ABRE-ARCHIVOS
               PERFORM I-START-CNDTACUM  THRU F-START-CNDTACUM.
               PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES
               PERFORM I-PRIMER-REGISTRO THRU F-PRIMER-REGISTRO
               PERFORM I-DISPLAY         THRU F-DISPLAY.
               PERFORM I-PROCESO-INFORME THRU F-PROCESO-INFORME
                       UNTIL FIN-OK = 1.
               PERFORM I-FIN-MODE        THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE WK-MASCARA-FECHA       TO  I-FECHA
               MOVE WK-MASCARA-HORA        TO  I-HORA
               MOVE 8                      TO  X
               MOVE    1                   TO   IND-INFORMES.
               PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
               PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
               MOVE CON-ANO                TO  LAB-INFORMES-ANO
                                               I-ANO
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P
               MOVE 1                      TO  SW-ABRE.
                  MOVE W-NIT-EMP              TO  I-NITEMP
                  MOVE W-NIT-EMP-EXT          TO  I-NITEXT
                  MOVE WEMPRESA               TO  TIT-CIA.
               PERFORM I-DISPLAY-LABEL   THRU   F-DISPLAY-LABEL.
               MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
               MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
               DISPLAY " "  LINE 24 POSITION 80 CONTROL
                            SG-CONTROL-STRING.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN INPUT  CNDTACUM
               OPEN INPUT  EMPRESAS CNNITCED
                           CNCATCTA
               OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.
      *
       I-PRIMER-REGISTRO.
           PERFORM I-LEE-CNDTACUM    THRU F-LEE-CNDTACUM
           PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA
           IF NIVEL = 1
                 PERFORM I-ACUMULE-NIVEL1  THRU F-ACUMULE-NIVEL1.
           PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME
           MOVE    GRUCTA            TO   SW-INGRESO.
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-PRIMER-REGISTRO.

       I-ACUMULE-NIVEL1.
           IF CTAMAE = 1000000000
              MOVE CNDTACUM-SALDO(W-LONG) TO W-ACTIVOS.
           IF CTAMAE = 2000000000
              MOVE CNDTACUM-SALDO(W-LONG) TO W-PASIVOS.
           IF CTAMAE = 3000000000
              MOVE CNDTACUM-SALDO(W-LONG) TO W-PATRIMONIO.
           IF CTAMAE = 4000000000
              MOVE CNDTACUM-SALDO(W-LONG) TO W-INGRESOS.
           IF CTAMAE = 5000000000
              MOVE CNDTACUM-SALDO(W-LONG) TO W-GASTOS.
      *    IF CTAMAE = 6000000000
      *       MOVE CNDTACUM-SALDO(W-LONG) TO W-COSTOS.
           IF CTAMAE = 7000000000
              MOVE CNDTACUM-SALDO(W-LONG) TO W-COSTOS.
       F-ACUMULE-NIVEL1.

       I-PROCESO-INFORME.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           IF FIN-OK = 1 GO F-PROCESO-INFORME.
           IF NIVEL  > 3 GO I-PROCESO-INFORME.
           IF GRUCTA = SW-INGRESO
              PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1
              GO I-PROCESO-INFORME
           ELSE
              PERFORM I-REG-NUEVO   THRU F-REG-NUEVO
              GO I-PROCESO-INFORME.
       F-PROCESO-INFORME.
      *
       I-REG-NUEVO.
           IF NIVEL = 1
                 PERFORM I-ACUMULE-NIVEL1  THRU F-ACUMULE-NIVEL1.
           PERFORM I-TOTAL-NIVEL THRU F-TOTAL-NIVEL
           MOVE    GRUCTA            TO   SW-INGRESO.
           PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME
           PERFORM I-MUEVE-INFORME-1 THRU F-MUEVE-INFORME-1.
       F-REG-NUEVO.
      ***************************************************************
      *          M O V I M I E N T O  DE   C A M P O S              *
      ***************************************************************
       I-MUEVE-INFORME-1.
            IF CNDTACUM-SALDO(W-LONG) = 0
               GO F-MUEVE-INFORME-1.
            IF  GRUCTA > 7 GO F-MUEVE-INFORME-1.
            DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
            MOVE NOMCTA                   TO I-NOMCTA.
            IF   NIVEL = 1
                 MOVE 0   TO VR-NIV3 VR-NIV2.
            IF   NIVEL = 2
                 MOVE 0   TO VR-NIV3
                 MOVE CNDTACUM-SALDO(W-LONG) TO VR-NIV2.
            IF   NIVEL = 3
                 MOVE 0   TO VR-NIV2
                 MOVE CNDTACUM-SALDO(W-LONG) TO VR-NIV3.
            PERFORM EVALUE-IMPRESION THRU F-EVALUE-IMPRESION.
       F-MUEVE-INFORME-1.
      *
       EVALUE-IMPRESION.
            IF CON-LIN > 55
              PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
            IF   NIVEL  = 3  AND CTAMAE > 9000000000
                 GO F-EVALUE-IMPRESION.
            IF   NIVEL  = 1 OR NIVEL = 2
               MOVE SPACES TO REG-INFORMES
            
               WRITE REG-INFORMES FROM LDET1   AFTER 1
               
               MOVE SPACES TO REG-INFORMES
               ADD 1 TO CON-LIN
            ELSE
               WRITE REG-INFORMES FROM LDET1   AFTER 1
               ADD 1 TO CON-LIN.
       F-EVALUE-IMPRESION.
      ***************************************************************
      *          I M P R E S I O N    DE   T O T A L E S            *
      ***************************************************************
       I-TOTAL-NIVEL.
           IF SW-INGRESO > 7 GO F-TOTAL-NIVEL.
           IF SW-INGRESO = 4
              MOVE " T O T A L   I N G R E S O S " TO I-NOMCTA
              MOVE W-INGRESOS             TO VR-NIV2
              DISPLAY     VR-NIV2   LINE 17 POSITION 1
              MOVE 0                      TO VR-NIV3
              PERFORM I-IMPRIME-TOTAL THRU F-IMPRIME-TOTAL.
           IF SW-INGRESO = 5
              MOVE " T O T A L   G A S T O S"  TO I-NOMCTA
              MOVE W-GASTOS               TO VR-NIV2
              MOVE 0                      TO VR-NIV3
              PERFORM I-IMPRIME-TOTAL THRU F-IMPRIME-TOTAL.
      *NJ  IF SW-INGRESO = 6
           IF SW-INGRESO = 7
              MOVE " T O T A L   C O S T O S"  TO I-NOMCTA
              MOVE W-COSTOS               TO VR-NIV2
              MOVE 0                      TO VR-NIV3
              PERFORM I-IMPRIME-TOTAL THRU F-IMPRIME-TOTAL
              PERFORM I-EVALUE-UTILIDAD THRU F-EVALUE-UTILIDAD.
       F-TOTAL-NIVEL.

       I-EVALUE-UTILIDAD.
           DISPLAY "CALCULO DE UTILIDAD O PERDIDA"
               LINE 24 POSITION 1 BEEP.
           COMPUTE SALDO = W-GASTOS + W-COSTOS + W-INGRESOS
           IF SALDO IS POSITIVE
              MOVE "PERDIDA DEL PRESENTE EJERCICIO" TO I-NOMCTA.
           IF SALDO IS NEGATIVE
              MOVE "UTILIDAD DEL PRESENTE EJERCICIO" TO I-NOMCTA.
           MOVE  SALDO        TO VR-NIV2
           MOVE  0            TO VR-NIV3
           MOVE  SPACES TO  REG-INFORMES
           WRITE REG-INFORMES              AFTER 1
           WRITE REG-INFORMES FROM LDET1   AFTER 1
          
           ADD 2 TO CON-LIN.
       F-EVALUE-UTILIDAD.

       I-IMPRIME-TOTAL.
           IF CON-LIN > 55
              PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
           WRITE REG-INFORMES FROM LIN14-S  AFTER 1
           WRITE REG-INFORMES FROM LDET1    AFTER 1
           
           WRITE REG-INFORMES FROM LIN15-S  AFTER 1
           ADD 3                      TO CON-LIN.
       F-IMPRIME-TOTAL.
      ***************************************************************
      *          I M P R E S I O N    DE   T I T U L O S            *
      ***************************************************************

       I-TITULO-INFORME.
           IF SW-INGRESO > 3 GO F-TITULO-INFORME.
           ADD 1 TO WK-PAGINA
           MOVE  WK-PAGINA TO I-PAGINA.
           MOVE WK-NOMBRE-MES(W-LONG)  TO I-MES
           MOVE  SPACES        TO    REG-INFORMES
           WRITE REG-INFORMES  AFTER PAGE
           WRITE REG-INFORMES  FROM  TITULO-2  AFTER 6
          
           WRITE REG-INFORMES  FROM  TITULO-2A AFTER 1
           
           MOVE 7  TO  CON-LIN.
       F-TITULO-INFORME.
      *
       I-CEROS-VARIABLES.
               MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                         S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                         T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                         G-TOTAL-DEBITOS G-TOTAL-CREDITOS
                         W-ACTIVOS     W-PASIVOS
                         W-PATRIMONIO  W-INGRESOS
                         W-COSTOS      W-GASTOS
                         FIN-OK  WK-PAGINA  CON-LIN
               MOVE 0 TO W-TOTAL-DEBME45 W-TOTAL-CREDME45
                         S-TOTAL-DEBME45 S-TOTAL-CREDME45
                         T-TOTAL-DEBME45 T-TOTAL-CREDME45
                         G-TOTAL-DEBME45 G-TOTAL-CREDME45
               MOVE 0 TO W-TOTAL-DEBMY45 W-TOTAL-CREDMY45
                         S-TOTAL-DEBMY45 S-TOTAL-CREDMY45
                         T-TOTAL-DEBMY45 T-TOTAL-CREDMY45
                         G-TOTAL-DEBMY45 G-TOTAL-CREDMY45.
       F-CEROS-VARIABLES.
      *
       I-PANTALLA-1B.
               MOVE "*** LI774 - ESTADO DE P & G ACUMULADO  ***"
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 10 POSITION 20
               DISPLAY "บ  TECLEE EL MES A REPORTAR :                บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ   [F2] TERMINAR                            บ"
                                               LINE 12 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 13 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-ACEPTA-MES.
               MOVE ZEROS                  TO  WK-EXCEPTION
               ACCEPT W-LONG                        LINE 11 POSITION 50
                                                    REVERSE
               CONVERT
               TAB
               NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               IF  W-LONG  =  0 OR W-LONG > 14
                   DISPLAY " CODIGO DEL MES ERRADO...DIGITE NUEVAMENTE"
                                                    LINE 24 POSITION 1
                                                    REVERSE BLINK
                   GO I-ACEPTA-MES.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.

               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54
                                                    REVERSE.
               IF W-LONG = 1 OR W-LONG = 14
                  COMPUTE MESW = 14
               ELSE
                  COMPUTE MESW = W-LONG - 1.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-ACEPTA-MES.
      *
      ***************************************************************
      *        C O N F I R M A    P A N T A L L A S                 *
      ***************************************************************
       I-CONFIRMA-COMP.
                    DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                                           LINE 24 POSITION 1
                                           REVERSE
                    DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32
                                                    REVERSE
                    PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "C"
                        GO TO I-CONFIRMA-COMP
           ELSE
               DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.

      ***************************************************************
      *       O R G A N I Z A C I O N   D E   A R C H I V O S       *
      ***************************************************************

       I-START-CNDTACUM.
               MOVE ZEROS                  TO  CNDTACUM-CLAVE
               MOVE 3999                   TO  CNDTACUM-MAYOR
               START CNDTACUM  KEY >   CNDTACUM-CLAVE INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTACUM.
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************
       I-LEE-CNDTACUM.
              READ CNDTACUM NEXT RECORD WITH NO LOCK AT END
                   MOVE 1 TO FIN-OK GO F-LEE-CNDTACUM.
              IF CNDTACUM-MAYOR < 4000
                 DISPLAY "ESPERE UN MOMENTO POR FAVOR..."
                      LINE 24 POSITION 1
                 DISPLAY CNDTACUM-MAYOR LINE 24 POSITION 50
                      GO I-LEE-CNDTACUM.
       F-LEE-CNDTACUM.

       I-LEE-CNCATCTA.
      *    PERFORM I-DISPLAY1 THRU F-DISPLAY1
           DISPLAY MAYMAE                 LINE 20 POSITION 27
           DISPLAY SUBMAE                 LINE 20 POSITION 33
           DISPLAY AUXMAE                 LINE 20 POSITION 36
           DISPLAY CNDTACUM-MAYOR         LINE 20 POSITION 53
           DISPLAY CNDTACUM-SUB           LINE 20 POSITION 58
           DISPLAY CNDTACUM-AUX           LINE 20 POSITION 61.
           DISPLAY NOMCTA                 LINE 21 POSITION 16.
           DISPLAY CNDTACUM-NIT           LINE 22 POSITION 27.
           MOVE CNDTACUM-MAYOR TO MAYMAE
           MOVE CNDTACUM-SUB   TO SUBMAE
           MOVE CNDTACUM-AUX   TO AUXMAE
           MOVE 0        TO SW-EOF.
           READ CNCATCTA WITH NO LOCK INVALID KEY
                         MOVE 1 TO SW-EOF.
       F-LEE-CNCATCTA.

       I-LEE-CNNITCED.
            MOVE 0 TO SW-EOF.
            READ CNNITCED WITH NO LOCK
                 INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-CNNITCED.
      *
       I-LEE-EMPRESAS.
               MOVE 0        TO SW-EOF.
               READ EMPRESAS WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
               MOVE WIN-EMPRESA   TO  W-NOMBRE.
       F-LEE-EMPRESAS.

       I-DISPLAY.
               MOVE " ** ESTADO DE P & G ACUMULADO  ***   "
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออป"
                                              LINE 19 POSITION 15
               DISPLAY "บ CNCATCTA:                   CNACUM:
      -       "      บ"
                                              LINE 20 POSITION 15
               DISPLAY "บ
      -       "      บ"
                                              LINE 21 POSITION 15
               DISPLAY "บ N.I.T   :                   TAMANO:
      -       "      บ"
                                              LINE 22 POSITION 15
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออผ"                       LINE 23 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
               PERFORM I-DISPLAY-LABEL   THRU   F-DISPLAY-LABEL.
       F-DISPLAY.

       I-DISPLAY1.
      *        PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออออออออออออออออป"
                                              LINE 07 POSITION 05
               DISPLAY "บ
      -       "                    บ"
                                              LINE 08 POSITION 05
               DISPLAY "บ
      -       "                    บ"
                                              LINE 09 POSITION 05
               DISPLAY "บ
      -       "                    บ"
                                              LINE 10 POSITION 05
               DISPLAY "บ
      -       "                    บ"
                                              LINE 11 POSITION 05
               DISPLAY "บ
      -       "                    บ"
                                              LINE 12 POSITION 05
               DISPLAY "บ
      -       "                    บ"
                                              LINE 13 POSITION 05
               DISPLAY "บ
      -       "                    บ"
                                              LINE 14 POSITION 05
               DISPLAY "บ
      -       "                    บ"
                                              LINE 15 POSITION 05
               DISPLAY "บ
      -       "                    บ"
                                              LINE 16 POSITION 05
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออออออออออ
      -       "ออออออออออออออออออออผ"
                                              LINE 17 POSITION 05.
           PERFORM I-MUESTRE THRU F-MUESTRE.
       F-DISPLAY1.

       I-MUESTRE.
           ADD 1 TO X.
           IF X > 15
              MOVE 8 TO X
              PERFORM I-BLANQUEA THRU F-BLANQUEA
                      UNTIL X > 15
              MOVE 8 TO X.
           DISPLAY NOMCTA                LINE X  POSITION 06.
           MOVE CNDTACUM-SALDO(W-LONG)   TO WI-Z12P
           DISPLAY WI-Z12P               LINE X  POSITION 35.
       F-MUESTRE.

       I-BLANQUEA.
           ADD 1 TO X.
       F-BLANQUEA.
      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************

       I-FIN-MODE.
           CLOSE CNCATCTA CNNITCED
                 EMPRESAS
                 INFORMES
                 CNDTACUM.
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE W-LONG                 TO  CON-PERIODO
           CANCEL "PROG774.COB"
      *           CALL "PROG737.COB" USING AREA-LINK-NOMINA.
      *           CANCEL "PROG737.COB"
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.

               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".
               COPY "..\PLB\CNDTACUM.PLB".
               COPY "..\PRO\DISLABEL.PRO".
               COPY "..\PLB\INFORMES.PLB".
