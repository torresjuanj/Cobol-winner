       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG771.
      ******************************************************************
      * B A L A N C E   G E N E R A L   ABR-27-2002                    *
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
           COPY "..\SEL\INFORMES.SEL".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\INFORMES.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG771".
       77      ID-PRG         PIC X(7)       VALUE "PROG771".
       77      W-VARIABLES-NOMINA  PIC X(7)  VALUE  SPACES.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
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
         05    FILLER      PIC X(16)    VALUE SPACES.
         05    TIT-CIA     PIC X(40)    VALUE SPACES.
      *
        03     TITULO-1.
         05    FILLER      PIC X(36)    VALUE SPACES.
         05    FILLER      PIC X(10)    VALUE "N.I.T No: ".
         05    I-NITEMP    PIC ZZZ,ZZZ,ZZZ.
         05    FILLER      PIC X(01)    VALUE "-".
         05    I-NITEXT    PIC Z.
      *
        03     TITULO-2.
         05    FILLER      PIC X(15)    VALUE SPACES.
         05    FILLER      PIC X(22)    VALUE "  BALANCE GENERAL  ".
      *
        03     TITULO-2A.
         05    FILLER      PIC X(15)    VALUE SPACES.
         05    I-MES       PIC X(10)    VALUE SPACES.
         05    FILLER      PIC X(4)     VALUE " de ".
         05    I-ANO       PIC X(04)    VALUE SPACES.
      *
        03  LIN14-S.
           05 FILLER       PIC X(73)       VALUE SPACES.
           05 FILLER       PIC X(21)       VALUE ALL "-".
           05 FILLER       PIC X(01)       VALUE " ".
      *
        03  LIN15-S.
           05 FILLER       PIC X(73)       VALUE SPACES.
           05 FILLER       PIC X(21)       VALUE ALL "=".
           05 FILLER       PIC X(01)       VALUE " ".
      *
        03 LDET1.
           05 FILLER       PIC X(02)       VALUE " ".
           05 I-NOMCTA     PIC X(35)       VALUE SPACES.
           05 FILLER       PIC X(01)       VALUE " ".
           05 VR-NIV5      PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER       PIC X(01)       VALUE " ".
           05 VR-NIV3      PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER       PIC X(01)       VALUE " ".
           05 VR-NIV2      PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER       PIC X(01)       VALUE " ".
      *
        03 PRIMER.
           05 FILLER       PIC X(02)    VALUE " ".
           05 I-NOMBRE     PIC X(32)    VALUE SPACES.
            05 FILLER       PIC X(04)    VALUE SPACES.
           05 VR-SUMA      PIC ZZZZZZ,ZZZ,ZZZ.ZZ-.
           05 FILLER       PIC X(01)    VALUE " ".
      *
      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
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
               PERFORM I-PANTALLA-1B     THRU F-PANTALLA-1B
               PERFORM I-ACEPTA-MES      THRU F-ACEPTA-MES
               PERFORM I-CONFIRMA-COMP   THRU F-CONFIRMA-COMP
               PERFORM I-LABEL-ARCHIVOS  THRU F-LABEL-ARCHIVOS
               PERFORM I-ABRE-ARCHIVOS   THRU F-ABRE-ARCHIVOS
               PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES
               PERFORM I-PRIMER-REGISTRO THRU F-PRIMER-REGISTRO
               PERFORM I-DISPLAY         THRU F-DISPLAY.
               PERFORM I-PROCESO-INFORME THRU F-PROCESO-INFORME
                       UNTIL FIN-OK = 1.
               MOVE  SPACES               TO REG-INFORMES.
               WRITE REG-INFORMES  AFTER PAGE.
               PERFORM I-FIN-MODE           THRU  F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               MOVE    1                   TO   IND-INFORMES.
               PERFORM I-LABEL-CNDTACUM    THRU F-LABEL-CNDTACUM.
               PERFORM I-LABEL-INFORMES    THRU F-LABEL-INFORMES.
               MOVE CON-ANO                TO  LAB-INFORMES-ANO
                                               I-ANO
               MOVE "π"                    TO  WK-RAYAS-U
               MOVE "Ã"                    TO  WK-RAYAS-P
               MOVE "∂"                    TO  WK-RAYA-U
               MOVE "«"                    TO  WK-RAYA-P
               MOVE 1                      TO  SW-ABRE.
                  MOVE W-NIT-EMP              TO  I-NITEMP
                  MOVE W-NIT-EMP-EXT          TO  I-NITEXT
                  MOVE WEMPRESA               TO  TIT-CIA.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
               OPEN INPUT  CNDTACUM
               OPEN INPUT  CNCATCTA
               OPEN OUTPUT INFORMES.
       F-ABRE-ARCHIVOS.
      *
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥                     I-PRIMER-REGISTRO                   ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-PRIMER-REGISTRO.
           PERFORM I-LEE-CNDTACUM    THRU F-LEE-CNDTACUM
           IF      FIN-OK = 1          GO F-PRIMER-REGISTRO.
           PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA
           IF NIVEL = 1
                 PERFORM I-ACUMULE-NIVEL  THRU F-ACUMULE-NIVEL.
           MOVE    GRUCTA              TO SW-INGRESO.
           PERFORM I-MUEVE-INFORME   THRU F-MUEVE-INFORME.
       F-PRIMER-REGISTRO.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥              I-PROCESO-INFORME                          ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-PROCESO-INFORME.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           IF FIN-OK = 1            GO F-PROCESO-INFORME.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           IF NIVEL  = 5    GO I-PROCESO-INFORME.
           IF CNDTACUM-MAYOR = 2610 OR CNDTACUM-MAYOR = 2510
              NEXT SENTENCE
           ELSE
              IF NIVEL  > 3 GO I-PROCESO-INFORME.
           IF GRUCTA = SW-INGRESO
              PERFORM I-MUEVE-INFORME THRU F-MUEVE-INFORME
              GO I-PROCESO-INFORME
           ELSE
              PERFORM I-REG-NUEVO     THRU F-REG-NUEVO
              GO I-PROCESO-INFORME.
       F-PROCESO-INFORME.
      *
       I-REG-NUEVO.
           IF NIVEL = 1
                 PERFORM I-ACUMULE-NIVEL  THRU F-ACUMULE-NIVEL.
           PERFORM I-TOTAL-NIVEL     THRU F-TOTAL-NIVEL
           PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME
           PERFORM I-MUEVE-INFORME   THRU F-MUEVE-INFORME.
           MOVE    GRUCTA              TO SW-INGRESO.
       F-REG-NUEVO.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-ACUMULE-NIVEL             ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-ACUMULE-NIVEL.
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
           IF CTAMAE = 7000000000
              MOVE CNDTACUM-SALDO(W-LONG) TO W-COSTOS.
       F-ACUMULE-NIVEL.

      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-MUEVE-INFORME             ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-MUEVE-INFORME.
            MOVE SPACES       TO I-NOMBRE
            MOVE ZEROS        TO VR-SUMA.
            IF CNDTACUM-SALDO(W-LONG) = 0
               GO F-MUEVE-INFORME.
            DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
            MOVE NOMCTA                   TO I-NOMCTA I-NOMBRE.
            IF   NIVEL = 1
                 MOVE 0   TO VR-NIV5 VR-NIV3 VR-NIV2 VR-SUMA.

            IF   NIVEL = 2
                 MOVE 0   TO VR-NIV5 VR-NIV3 VR-NIV2
                 MOVE CNDTACUM-SALDO(W-LONG) TO VR-NIV2.

            IF   NIVEL = 3
                 MOVE 0   TO VR-NIV5 VR-NIV2 VR-NIV3
                 MOVE CNDTACUM-SALDO(W-LONG) TO VR-NIV3.
      
            IF   NIVEL = 6
                 MOVE 0   TO VR-NIV5 VR-NIV2 VR-NIV3
                 MOVE CNDTACUM-SALDO(W-LONG) TO VR-NIV5.

            IF   NIVEL = 4
                 MOVE 0   TO VR-NIV5 VR-NIV2 VR-NIV3
                 MOVE CNDTACUM-SALDO(W-LONG) TO VR-NIV3.

           IF   NIVEL = 3  AND CNDTACUM-MAYOR = 2610
                MOVE 0   TO VR-NIV5 VR-NIV2 VR-NIV3.
           IF   NIVEL = 3  AND CNDTACUM-MAYOR = 2510
                MOVE 0   TO VR-NIV5 VR-NIV2 VR-NIV3.

            PERFORM EVALUE-IMPRESION THRU F-EVALUE-IMPRESION.
       F-MUEVE-INFORME.
      *
       I-TOTAL-NIVEL.
           IF SW-INGRESO = 1
              MOVE " T O T A L   A C T I V O"  TO I-NOMBRE
              MOVE W-ACTIVOS              TO VR-SUMA
              MOVE 0                      TO VR-NIV3
              PERFORM I-IMPRIME-TOTAL THRU F-IMPRIME-TOTAL.
           IF SW-INGRESO = 2
              MOVE " T O T A L   P A S I V O"  TO I-NOMBRE
              MOVE W-PASIVOS              TO VR-SUMA
              MOVE 0                      TO VR-NIV3
              PERFORM I-IMPRIME-TOTAL THRU F-IMPRIME-TOTAL.
           IF SW-INGRESO = 3
              MOVE 0  TO RUT-OK
              PERFORM I-TERMINE-LEER THRU F-TERMINE-LEER  UNTIL
                      RUT-OK = 1
              PERFORM I-EVALUE-UTILIDAD THRU F-EVALUE-UTILIDAD
              MOVE " T O T A L   P A T R I M O N I O"  TO I-NOMBRE
              MOVE W-PATRIMONIO           TO VR-SUMA
              MOVE 0                      TO VR-NIV3
              PERFORM I-IMPRIME-TOTAL THRU F-IMPRIME-TOTAL
              COMPUTE W-TOTAL-CREDITOS = W-PASIVOS + W-PATRIMONIO
              MOVE " TOTAL PASIVO Y PATRIMONIO      "  TO I-NOMBRE
              MOVE W-TOTAL-CREDITOS       TO VR-SUMA
              MOVE 0                      TO VR-NIV3
              PERFORM I-IMPRIME-TOTAL   THRU F-IMPRIME-TOTAL
              MOVE     1                  TO LL.
       F-TOTAL-NIVEL.
      *
       I-TERMINE-LEER.
           IF NIVEL = 1
              PERFORM I-ACUMULE-NIVEL  THRU F-ACUMULE-NIVEL.
           PERFORM I-LEE-CNDTACUM       THRU F-LEE-CNDTACUM
           PERFORM I-LEE-CNCATCTA       THRU F-LEE-CNCATCTA
           IF CTAMAE > 7000000000
              COMPUTE SALDO = W-GASTOS + W-COSTOS + W-INGRESOS
              MOVE 1 TO RUT-OK.
       F-TERMINE-LEER.
      *
       I-EVALUE-UTILIDAD.
           IF SALDO IS POSITIVE
              MOVE "PERDIDA DEL PRESENTE EJERCICIO" TO I-NOMCTA.
           IF SALDO IS NEGATIVE
              MOVE "UTILIDAD DEL PRESENTE EJERCICIO" TO I-NOMCTA.
           COMPUTE W-PATRIMONIO = W-PATRIMONIO + SALDO.
           MOVE  SALDO        TO VR-NIV2
           MOVE  0            TO VR-NIV3
           MOVE  SPACES TO  REG-INFORMES
           WRITE REG-INFORMES              AFTER 1
           WRITE REG-INFORMES FROM LDET1   AFTER 2
           
           ADD 3 TO CON-LIN.
       F-EVALUE-UTILIDAD.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥              I-IMPRESION DEL INFORME                    ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-TITULO-INFORME.
           IF   LL       = 1 GO F-TITULO-INFORME.
           ADD 1 TO WK-PAGINA
           MOVE WK-NOMBRE-MES(W-LONG)  TO I-MES
           MOVE  SPACES          TO  REG-INFORMES
           IF  WK-PAGINA > 1
               WRITE REG-INFORMES  AFTER PAGE.
           WRITE REG-INFORMES  FROM  TITULO-0  AFTER 6.
          
           WRITE REG-INFORMES  FROM  TITULO-1  AFTER 1
           WRITE REG-INFORMES  FROM  TITULO-2  AFTER 2
           
           WRITE REG-INFORMES  FROM  TITULO-2A AFTER 1
         
           MOVE  10              TO  CON-LIN.
       F-TITULO-INFORME.
      *
       EVALUE-IMPRESION.
            IF   LL       = 1 GO F-EVALUE-IMPRESION.
            IF CON-LIN > 55  OR CON-LIN = ZEROS
              PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.

            IF NIVEL  = 1
               MOVE  SPACES TO REG-INFORMES
               WRITE REG-INFORMES              AFTER 1
               WRITE REG-INFORMES FROM PRIMER  AFTER 2
               
               MOVE  SPACES TO REG-INFORMES
               ADD    3     TO CON-LIN
               GO    F-EVALUE-IMPRESION.

            IF   NIVEL  = 1 OR NIVEL = 2
               MOVE SPACES TO REG-INFORMES
               WRITE REG-INFORMES              AFTER 1
               WRITE REG-INFORMES FROM LDET1   AFTER 2
             
               MOVE SPACES TO REG-INFORMES
               ADD 3 TO CON-LIN
            ELSE
               WRITE REG-INFORMES FROM LDET1   AFTER 1
               ADD 1 TO CON-LIN.
       F-EVALUE-IMPRESION.
      *
       I-IMPRIME-TOTAL.
                IF CON-LIN > 55
                   PERFORM I-TITULO-INFORME  THRU F-TITULO-INFORME.
                WRITE REG-INFORMES FROM LIN14-S  AFTER 3
                WRITE REG-INFORMES FROM PRIMER   AFTER 1
               
                WRITE REG-INFORMES FROM LIN15-S  AFTER 1
                ADD 5                TO CON-LIN.
       F-IMPRIME-TOTAL.
      *
      *    €ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ›
      *    €         I-REGRABACION.              ›
      *    €‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹›
      *
       I-CEROS-VARIABLES.
               MOVE 0 TO W-TOTAL-CREDITOS
                         W-ACTIVOS     W-PASIVOS
                         W-PATRIMONIO  W-INGRESOS
                         W-COSTOS      W-GASTOS LL
                         FIN-OK  WK-PAGINA CON-LIN.
       F-CEROS-VARIABLES.
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥              I-MANIPULACION DE ARCHIVOS                 ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-START-CNDTACUM.
               MOVE ZEROS                  TO  CNDTACUM-CLAVE
               START CNDTACUM  KEY >   CNDTACUM-CLAVE INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTACUM.
      *
       I-LEE-CNDTACUM.
              READ CNDTACUM NEXT RECORD WITH NO LOCK AT END
                   MOVE 1 TO FIN-OK.
           IF CNDTACUM-MAYOR > 7000 MOVE 1 TO FIN-OK.

           IF CNDTACUM-MAYOR = 2510 and CNDTACUM-NIT NOT = ZEROS
              GO I-LEE-CNDTACUM.
       F-LEE-CNDTACUM.
      *
       I-LEE-CNCATCTA.
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
      *
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≥              I-PANTALLAS Y CAPTURAS DEL PROGRAMAS       ≥≤≤
      *  ≤≤ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ≤≤
      *  ≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
      *
       I-PANTALLA-1B.
               MOVE "* B A L A N C E   G E N E R A L *"
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
                                               LINE 10 POSITION 20
               DISPLAY "∫  DIGITE EL MES A REPORTAR :                ∫"
                                               LINE 11 POSITION 20
               DISPLAY "∫   [F2] TERMINAR                            ∫"
                                               LINE 12 POSITION 20
               DISPLAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
                                               LINE 13 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-ACEPTA-MES.
               MOVE ZEROS          TO  WK-EXCEPTION
               ACCEPT W-LONG     LINE 11 POSITION 50 REVERSE
                                 CONVERT TAB NO BEEP
                                 ON EXCEPTION WK-EX
                                 PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS  LINE 24 POSITION 1
               IF  W-LONG  =  0 OR W-LONG > 14
                   DISPLAY " PERIODO ERRADO...DIGITE NUEVAMENTE"
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
       I-CONFIRMA-COMP.
           DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                     LINE 24 POSITION 1 REVERSE
           DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32 REVERSE
           PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  W-TRUCO                  NOT =  "C"
                        GO TO I-CONFIRMA-COMP
           ELSE
               DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
      *
       I-DISPLAY.
               MOVE "* B A L A N C E   G E N E R A L *"
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -       "ÕÕÕÕÕÕª"
                                              LINE 19 POSITION 15
               DISPLAY "∫ CNCATCTA:                   CNACUM:
      -       "      ∫"
                                              LINE 20 POSITION 15
               DISPLAY "∫
      -       "      ∫"
                                              LINE 21 POSITION 15
               DISPLAY "∫ N.I.T   :                   TAMANO:
      -       "      ∫"
                                              LINE 22 POSITION 15
               DISPLAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ
      -       "ÕÕÕÕÕÕº"                       LINE 23 POSITION 15
               DISPLAY WK-ESPACIOS            LINE 24 POSITION 01.
               PERFORM I-DISPLAY-LABEL THRU   F-DISPLAY-LABEL.
               MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
               MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
               DISPLAY " "  LINE 24 POSITION 80 CONTROL
                            SG-CONTROL-STRING.
       F-DISPLAY.
     *
       I-FIN-MODE.
           CLOSE CNCATCTA
                 INFORMES
                 CNDTACUM.
      *
           MOVE W-IDENT-PROGRAMA       TO  CON-PROGRAMA
           MOVE W-LONG                 TO  CON-PERIODO
           CANCEL "PROG771.COB"
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
               COPY "..\PRO\DISLABEL.PRO".
      *
               COPY "..\PLB\CNDTACUM.PLB".
               COPY "..\PLB\INFORMES.PLB".
