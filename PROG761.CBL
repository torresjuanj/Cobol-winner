       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG761.
      ****************************************************************
      *       ESTADOS DE CUENTAS CLIENTES  ABR-27-2002               *
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "..\DYC\USER-ACT.DYC".
           COPY "..\DYC\EMPRESAS.DYC".
           COPY "..\DYC\CNDTACUM.DYC".
           COPY "..\DYC\CNCONTRO.DYC".
           COPY "..\DYC\CNCATCTA.DYC".
           COPY "..\DYC\CNNITCED.DYC".
           COPY "..\DYC\WTEMHISN.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNCONTRO.FD".
               COPY "..\FD\CNDTACUM.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\WTEMHISN.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA     PIC X(07)      VALUE "PROG761".
       77      ID-PRG         PIC X(7)       VALUE "PROG761".
       77  W-TIT-PANTALLA     PIC X(38)      VALUE SPACES.
       01      CODIGO-PANTALLAS.
        03     CODIGO-PANTALLA           PIC 9(04)      OCCURS 25 TIMES.
               COPY "..\LBL\CNDTACUM.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\CNNITCED.LBL".
               COPY "..\LBL\CNCONTRO.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\WTEMHISN.LBL".
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\LIQUINOV.WRK".
               COPY "..\WRK\EMPRESAS.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\WRK\NO-ERROR.WRK".
               COPY "..\WRK\TABLAMES.WRK".
      ******************************************************************
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION               USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNDTACUM.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\CNNITCED.DCL".
               COPY "..\DCL\CNCONTRO.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\WTEMHISN.DCL".
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
               PERFORM I-PROCESO-COMP    THRU F-PROCESO-COMP UNTIL
                   F2 OR  ESC.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
               PERFORM I-LABEL-WTEMHISN THRU   F-LABEL-WTEMHISN.
               PERFORM I-LABEL-CNDTACUM THRU   F-LABEL-CNDTACUM.
               MOVE    W-CODEMP            TO  LAB-CNDTACUM-DIRCIA
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P
               MOVE 1                      TO  SW-ABRE.
       F-LABEL-ARCHIVOS. EXIT.
      *
       I-ABRE-ARCHIVOS.
              OPEN INPUT  EMPRESAS CNNITCED CNCATCTA
                          CNDTACUM.
       F-ABRE-ARCHIVOS.
      *
      ***************************************************************
      *        P R O C E S O   D E   C O M P R O B A N T E S        *
      ***************************************************************
      *
       I-PANTALLA-1.
           MOVE "E S T A D O   D E   C U E N T A   C L I E N T E S"
           TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
               DISPLAY "ษอออออออออออออออออออออออออออออออออออออออป"
                          LINE 06 POSITION 20
               DISPLAY "บ   [F4]  SALDOS EN BANCOS              บ"
                          LINE 07 POSITION 20
               DISPLAY "บ   [F5]  SALDOS EN CORPORACIONES       บ"
                          LINE 08 POSITION 20
               DISPLAY "บ   [F6]  SALDOS DE CARTERA GENERAL     บ"
                          LINE 09 POSITION 20
               DISPLAY "บ   [F7]  SALDOS DE CARTERA DETALLADA   บ"
                          LINE 10 POSITION 20
               DISPLAY "บ                                       บ"
                          LINE 11 POSITION 20
               DISPLAY "บ    [F2] TERMINAR                      บ"
                          LINE 12 POSITION 20
               DISPLAY "ศอออออออออออออออออออออออออออออออออออออออผ"
                          LINE 13 POSITION 20
               DISPLAY WK-ESPACIOS           LINE 24 POSITION 1.
       F-PANTALLA-1.

      ***************************************************************
      *    AREA DE PROCESOS PARA IMPRECION DE TIPOS DE COMPROBANTE  *
      ***************************************************************

       I-PROCESO-COMP.
            PERFORM I-PANTALLA-1   THRU F-PANTALLA-1
            PERFORM ACEPTA-TRUCO
            IF F2  PERFORM I-FIN-MODE   THRU F-FIN-MODE.
            IF F4  PERFORM I-PROCESO-F4 THRU F-PROCESO-F4.
            IF F5  PERFORM I-PROCESO-F5 THRU F-PROCESO-F5.
            IF F6  PERFORM I-PROCESO-F6 THRU F-PROCESO-F6.
            IF F7  PERFORM I-PROCESO-F7 THRU F-PROCESO-F7.
            INITIALIZE WK-EXCEPTION.
       F-PROCESO-COMP.

       I-PROCESO-F4.
            MOVE "     CONSULTA DE SALDOS EN BANCOS     "
             TO W-TIT-PANTALLA.
            PERFORM I-BLOQUE-STANDARD THRU F-BLOQUE-STANDARD
            PERFORM I-PROCESO-BANCOS  THRU F-PROCESO-BANCOS
                    UNTIL FIN-OK = 1.
            MOVE SPACES TO W-TRUCO
            PERFORM I-PARAR THRU F-PARAR UNTIL W-TRUCO = "C".
       F-PROCESO-F4.

       I-PROCESO-F5.
            MOVE " CONSULTA DE SALDOS EN COORPORACIONES "
             TO W-TIT-PANTALLA.
            PERFORM I-BLOQUE-STANDARD THRU F-BLOQUE-STANDARD
            PERFORM I-PROCESO-CAV   THRU F-PROCESO-CAV
                    UNTIL FIN-OK = 1.
            MOVE SPACES TO W-TRUCO
            PERFORM I-PARAR THRU F-PARAR UNTIL W-TRUCO = "C".
       F-PROCESO-F5.

       I-PROCESO-F6.
            MOVE "     CONSULTA DE CARTERA GENERAL      "
             TO W-TIT-PANTALLA.
            PERFORM I-BLOQUE-STANDARD THRU F-BLOQUE-STANDARD
            PERFORM I-PROCESO-CARTERA-GRAL THRU F-PROCESO-CARTERA-GRAL
                    UNTIL FIN-OK = 1.
            MOVE SPACES TO W-TRUCO
            PERFORM I-PARAR THRU F-PARAR UNTIL W-TRUCO = "C".
       F-PROCESO-F6.

       I-PROCESO-F7.
           MOVE "   E S T A D O   D E   C U E N T A    "
             TO W-TIT-PANTALLA.
           PERFORM I-BLOQUE-STANDARD-F7 THRU F-BLOQUE-STANDARD-F7
           PERFORM I-PROCESO-NITS       THRU F-PROCESO-NITS
                    UNTIL FIN-OK = 1.
           MOVE ZERO TO SW-EOF.
           PERFORM I-MUESTRA-SALDOS-F7 THRU F-MUESTRA-SALDOS-F7
                    UNTIL SW-EOF = 1.
           CLOSE WTEMHISN.
       F-PROCESO-F7.

       I-PROCESO-F8.
            PERFORM I-BLOQUE-STANDARD THRU F-BLOQUE-STANDARD
            PERFORM I-PROCESO-FRAS  THRU F-PROCESO-FRAS
                    UNTIL FIN-OK = 1.
            MOVE SPACES TO W-TRUCO
            PERFORM I-PARAR THRU F-PARAR UNTIL W-TRUCO = "C".
       F-PROCESO-F8.
      *
       I-PROCESO-BANCOS.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           IF FIN-OK = 1 GO TO F-PROCESO-BANCOS.
           IF INDICE-BCOS = "N" GO I-PROCESO-BANCOS.
           MOVE MAYMAE               TO CNDTACUM-MAYOR
           MOVE SUBMAE               TO CNDTACUM-SUB
           MOVE AUXMAE               TO CNDTACUM-AUX
           MOVE 0                    TO CNDTACUM-NIT
           MOVE 0                    TO CNDTACUM-FRA
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           PERFORM I-DISPLAY-DATOS   THRU F-DISPLAY-DATOS.
       F-PROCESO-BANCOS.
      *
       I-PROCESO-CAV.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA
           IF      FIN-OK = 1        GO   F-PROCESO-CAV.
           IF      INDICE-RET  = "N" GO   I-PROCESO-CAV.
           MOVE MAYMAE               TO CNDTACUM-MAYOR
           MOVE SUBMAE               TO CNDTACUM-SUB
           MOVE AUXMAE               TO CNDTACUM-AUX
           MOVE 0                    TO CNDTACUM-NIT
           MOVE 0                    TO CNDTACUM-FRA
           PERFORM I-LEE-CNDTACUM    THRU F-LEE-CNDTACUM
           PERFORM I-DISPLAY-DATOS   THRU F-DISPLAY-DATOS.
       F-PROCESO-CAV.

       I-PROCESO-CARTERA-GRAL.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           PERFORM I-LEE-CNCATCTA    THRU F-LEE-CNCATCTA
           IF      FIN-OK      = 1   GO   F-PROCESO-CARTERA-GRAL.
           IF      INDICE-CAR  = "N" GO   I-PROCESO-CARTERA-GRAL.
           MOVE MAYMAE               TO CNDTACUM-MAYOR
           MOVE SUBMAE               TO CNDTACUM-SUB
           MOVE AUXMAE               TO CNDTACUM-AUX
           MOVE 0                    TO CNDTACUM-NIT
           MOVE 0                    TO CNDTACUM-FRA
           PERFORM I-LEE-CNDTACUM    THRU F-LEE-CNDTACUM
           PERFORM I-DISPLAY-DATOS   THRU F-DISPLAY-DATOS.
       F-PROCESO-CARTERA-GRAL.

       I-PROCESO-NITS.
           DISPLAY
           SPACES LINE 25 POSITION 01 SIZE 80
           "PROCESANDO CUENTAS N.I.T.S..." LINE 25 POSITION 01.
           PERFORM I-LEE-SIG-CNDTACUM THRU F-LEE-SIG-CNDTACUM.
           IF FIN-OK = 1 GO F-PROCESO-NITS.
           IF WIN-NIT NOT = CNDTACUM-NIT-ALT1 MOVE 1 TO FIN-OK
              GO F-PROCESO-NITS.
           MOVE CNDTACUM-MAYOR TO MAYMAE.
           MOVE CNDTACUM-SUB   TO SUBMAE.
           MOVE CNDTACUM-AUX   TO AUXMAE.
           PERFORM I-LEE-CNCATCTAI THRU F-LEE-CNCATCTAI.
           IF SW-EOF = 1
            DISPLAY
            SPACES SIZE 80 LINE 25 POSITION 01
            CTAMAE LINE 25 POSITION 01
            "Cuenta no existe, de cualquier tecla para continuar..."
            LINE 25 POSITION 11
            PERFORM ACEPTA-TRUCO
            GO I-PROCESO-NITS.
           IF INDICE-CAR = "S"
              IF CNDTACUM-NIT > 0
                 IF CNDTACUM-FRA > 0
                          IF CNDTACUM-SALDO(W-LONG) NOT = 0
           PERFORM I-ALIMENTAR-WTEMHISN THRU F-ALIMENTAR-WTEMHISN.
       F-PROCESO-NITS. EXIT.

       I-MUESTRA-SALDOS-F7.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           PERFORM I-INI-LECTURA-WTEMHISN THRU F-INI-LECTURA-WTEMHISN.
           MOVE ZEROS TO C-TOTFAC NULOS C-TOTBAS.
           PERFORM I-LEE-WTEMHISN THRU F-LEE-WTEMHISN.
           MOVE WTEMHISN-CIUDAD TO NULOS.
           PERFORM I-DISPLAY-DAT-WTEMHISN THRU F-DISPLAY-DAT-WTEMHISN
             UNTIL SW-EOF = 1.
       F-MUESTRA-SALDOS-F7. EXIT.

       I-DISPLAY-DAT-WTEMHISN.
           IF K-LINEA = 07
            DISPLAY
            WTEMHISN-CIUDAD    LINE K-LINEA POSITION 03 REVERSE
            WTEMHISN-NOMCIU    LINE K-LINEA POSITION 07 REVERSE.
           ADD 1 TO K-LINEA.
           ADD WTEMHISN-SALDO TO C-TOTFAC.
           ADD WTEMHISN-SALDO TO C-TOTBAS.
           MOVE WTEMHISN-FRA TO WI-Z6.
           DISPLAY WI-Z6 LINE K-LINEA POSITION 24 REVERSE
           MOVE WTEMHISN-FECHA-FRA TO WK-FECHA-HOY.
           PERFORM I-MASC-NAC THRU F-MASC-NAC.
           DISPLAY WK-MASCARA-FECHA LINE K-LINEA POSITION 33 REVERSE.
           MOVE WTEMHISN-SALDO TO WI-Z9P
           DISPLAY WI-Z9P LINE K-LINEA POSITION 46 REVERSE
           PERFORM I-LEE-WTEMHISN THRU F-LEE-WTEMHISN.
           IF NULOS NOT = WTEMHISN-CIUDAD
            ADD 1 TO K-LINEA
            MOVE C-TOTFAC TO WI-Z11P
            DISPLAY WI-Z11P   LINE K-LINEA POSITION 62 REVERSE
            MOVE ZEROS TO C-TOTFAC.
           IF SW-EOF = 1
            ADD 1 TO K-LINEA
            MOVE C-TOTFAC TO WI-Z11P
            DISPLAY WI-Z11P   LINE K-LINEA POSITION 62  REVERSE
            MOVE ZEROS TO C-TOTFAC.
           IF K-LINEA > 19 OR SW-EOF = 1 OR
              NULOS NOT = WTEMHISN-CIUDAD
            IF NULOS NOT = WTEMHISN-CIUDAD
             MOVE WTEMHISN-CIUDAD TO NULOS
            END-IF
            IF SW-EOF = 1
             MOVE C-TOTBAS TO WI-Z11P
             DISPLAY WI-Z11P LINE 03 POSITION 53 REVERSE
            END-IF
            MOVE SPACES TO W-TRUCO
            PERFORM I-PARAR THRU F-PARAR UNTIL W-TRUCO = "C"
            PERFORM I-LIMPIA-PANTA-F7 THRU F-LIMPIA-PANTA-F7
              VARYING K-LINEA FROM 07 BY 1 UNTIL K-LINEA > 21
              MOVE 07 TO K-LINEA.
       F-DISPLAY-DAT-WTEMHISN. EXIT.

       I-INI-LECTURA-WTEMHISN.
           CLOSE WTEMHISN.
           OPEN INPUT WTEMHISN.
           PERFORM I-START-WTEMHISN THRU F-START-WTEMHISN.
       F-INI-LECTURA-WTEMHISN. EXIT.

       I-ALIMENTAR-WTEMHISN.
           MOVE SPACES TO REC-WTEMHISN.
           MOVE CNDTACUM-AUX TO WTEMHISN-CIUDAD
           MOVE CNDTACUM-FRA TO WTEMHISN-FRA.
           MOVE CNDTACUM-FECHA-FRA TO WTEMHISN-FECHA-FRA.
           MOVE CNDTACUM-SALDO(W-LONG) TO WTEMHISN-SALDO.
           MOVE NOMCTA TO WTEMHISN-NOMCIU.
           WRITE REC-WTEMHISN INVALID KEY
            STOP "Problemas con WTEMHISN NO GRABA OJO".
       F-ALIMENTAR-WTEMHISN. EXIT.

       I-PROCESO-FRAS.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           DISPLAY "PROCESANDO CUENTAS FACTURAS..."
                               LINE 18 POSITION 25.
           PERFORM I-LEE-CNDTACUM THRU F-LEE-CNDTACUM
           IF FIN-OK = 1 GO TO F-PROCESO-FRAS.
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA.
       F-PROCESO-FRAS.

       I-BLOQUE-STANDARD.
            MOVE 05 TO K-LINEA.
            PERFORM I-PANTALLA        THRU F-PANTALLA
            PERFORM I-START-CNDTACUM  THRU F-START-CNDTACUM
            PERFORM I-START-CNCATCTA  THRU F-START-CNCATCTA
            PERFORM I-CEROS-VARIABLES THRU F-CEROS-VARIABLES.
       F-BLOQUE-STANDARD.

       I-BLOQUE-STANDARD-F7.
           OPEN OUTPUT WTEMHISN.
           PERFORM I-PANTALLA-F7        THRU F-PANTALLA-F7
           PERFORM I-EMPRESA-CONS       THRU F-EMPRESA-CONS.
           MOVE 07 TO K-LINEA.
           PERFORM I-START-CNDTACUM-F7  THRU F-START-CNDTACUM-F7
           PERFORM I-CEROS-VARIABLES    THRU F-CEROS-VARIABLES.
       F-BLOQUE-STANDARD-F7. EXIT.

       I-EMPRESA-CONS.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR.
           DISPLAY
           SPACES SIZE 80 LINE 25 POSITION 01 REVERSE
           "<F2> TERMINAR" LINE 25 POSITION 01 REVERSE BLINK CONTROL
            SG-CONTROL-STRING.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           ACCEPT WIN-COD-EMPRESA  LINE 03 POSITION 17
                                   CONTROL SG-CONTROL-STRING
                                   UPDATE PROMPT TAB NO BEEP
                                ON EXCEPTION WK-EX
                                   PERFORM 999-EXCEPTION.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           PERFORM I-LEE-EMPRESAS THRU F-LEE-EMPRESAS.
           IF SW-EOF = 1
            DISPLAY
            SPACES SIZE 80 LINE 25 POSITION 01
            "Codigo empresa no existe, de cualquier tecla para continuar
      -    "..." LINE 25 POSITION 01 REVERSE
            PERFORM ACEPTA-TRUCO
            GO I-EMPRESA-CONS.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR.
           MOVE    SPACES TO WK-NOMBRE2.
           PERFORM I-DISPLAY-EMP THRU F-DISPLAY-EMP.
           IF WK-NOMBRE2 = SPACES
           DISPLAY WIN-EMPRESA LINE 01 POSITION 15 REVERSE CONTROL
            SG-CONTROL-STRING
            ELSE
           DISPLAY WK-NOMBRE2 LINE 01 POSITION 15 REVERSE CONTROL
            SG-CONTROL-STRING.
           MOVE WIN-NIT TO WI-Z12C
           DISPLAY
           WI-Z12C LINE 03 POSITION 24.
       F-EMPRESA-CONS. EXIT.

       I-PANTALLA-1B.
               MOVE "        SALDOS EN LINEA                "
               TO WK-SISTEMA
               PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                LINE 10 POSITION 20
               DISPLAY "บ  TECLEE EL MES A CONSULTAR:                บ"
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
      *                                             REVERSE
               CONVERT
               TAB
               NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
               IF  W-LONG  =  0 OR W-LONG > 14
                   DISPLAY " PERIODO ERRADO...DIGITE NUEVAMENTE"
                            LINE 24 POSITION 1
                                                    REVERSE BLINK
                   GO I-ACEPTA-MES.
               IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.

               DISPLAY WK-NOMBRE-MES(W-LONG)        LINE 11 POSITION 54
                                                    REVERSE.
               IF W-LONG = 1  OR W-LONG = 14
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
      *
       I-CONFIRMA-FACT.
                    DISPLAY " PRESIONE <C> PARA CONFIRMA-COMPR "
                                           LINE 24 POSITION 1
                                           REVERSE
                    DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32
                                                    REVERSE
                    PERFORM ACEPTA-TRUCO
                    IF  F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
                    IF  W-TRUCO                  NOT =  "C"
                        GO TO I-CONFIRMA-FACT
                     ELSE
                        DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-FACT.

      ***************************************************************
      *       O R G A N I Z A C I O N   D E   A R C H I V O S       *
      ***************************************************************
       I-START-CNDTACUM.
               MOVE ZEROS                  TO  CNDTACUM-CLAVE
               START CNDTACUM  KEY >   CNDTACUM-CLAVE INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTACUM. EXIT.
      *
       I-START-CNDTACUM-F7.
           MOVE ZEROS TO CNDTACUM-ALTER-1.
           MOVE WIN-NIT TO CNDTACUM-NIT-ALT1.
           START CNDTACUM KEY > CNDTACUM-ALTER-1 INVALID KEY
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
            "No hay datos cartera detallada... " LINE 25 POSITION 01
            PERFORM ACEPTA-TRUCO
            PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNDTACUM-F7. EXIT.
      *
       I-START-CNCATCTA.
               MOVE ZEROS                  TO  CTAMAE
               START CNCATCTA  KEY >   CTAMAE INVALID KEY
                     MOVE "98"      TO  STATUS-NOMINA
                     PERFORM RUTINA-ERROR-NOMINA
                     PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-CNCATCTA.
      *
       I-START-WTEMHISN.
           MOVE ZEROS TO WTEMHISN-CLAVE
           START WTEMHISN KEY > WTEMHISN-CLAVE INVALID KEY
            DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
            "No se encontro datos de cartera ... de cualquier tecla para
      -    " continuar " LINE 25 POSITION 01
            PERFORM ACEPTA-TRUCO
            PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-START-WTEMHISN. EXIT.
      ***************************************************************
      *          L E C T U R A    D E     A R C H I V O S           *
      ***************************************************************

       I-LEE-CNDTACUM.
               MOVE 0        TO SW-EOF.
               READ CNDTACUM WITH NO LOCK INVALID KEY
                             MOVE 1 TO SW-EOF.
       F-LEE-CNDTACUM.

       I-LEE-SIG-CNDTACUM.
           MOVE 0        TO FIN-OK.
           READ CNDTACUM NEXT RECORD WITH NO LOCK AT END
                         MOVE 1 TO FIN-OK.
       F-LEE-SIG-CNDTACUM. EXIT.

       I-LEE-WTEMHISN.
           MOVE 0        TO SW-EOF.
           READ WTEMHISN NEXT RECORD WITH NO LOCK AT END
                         MOVE 1 TO SW-EOF.
       F-LEE-WTEMHISN. EXIT.

       I-LEE-CNNITCED.
            MOVE 0 TO SW-EOF.
            READ CNNITCED WITH NO LOCK
                 INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-CNNITCED.

       I-LEE-CNCATCTAI.
           MOVE 0 TO SW-EOF.
           READ CNCATCTA WITH NO LOCK
                INVALID KEY MOVE 1 TO SW-EOF.
       F-LEE-CNCATCTAI.

       I-LEE-CNCATCTA.
              READ CNCATCTA NEXT RECORD WITH NO LOCK
               AT END MOVE 1 TO FIN-OK.
       F-LEE-CNCATCTA.
      *
       I-LEE-EMPRESAS.
           MOVE 0        TO SW-EOF.
           READ EMPRESAS WITH NO LOCK INVALID KEY
                         MOVE 1 TO SW-EOF.
           MOVE WIN-EMPRESA   TO  W-NOMBRE.

       F-LEE-EMPRESAS. EXIT.
      *
      ***************************************************************
      *          M O V I M I E N T O  DE   C A M P O S              *
      ***************************************************************
       I-DISPLAY-DATOS.
           IF CNDTACUM-SALDO(W-LONG) = 0 GO F-DISPLAY-DATOS.
           ADD     1               TO  K-LINEA
           MOVE    CNDTACUM-MAYOR  TO  WI-Z5
           DISPLAY WI-Z5 LINE K-LINEA POSITION 02 REVERSE
           MOVE    CNDTACUM-SUB    TO  WI-Z2
           DISPLAY WI-Z2 LINE K-LINEA POSITION 08 REVERSE
           MOVE    CNDTACUM-AUX    TO WI-Z3
           DISPLAY WI-Z3 LINE K-LINEA POSITION 11 REVERSE
           DISPLAY NOMCTA LINE K-LINEA POSITION 15 REVERSE
           MOVE CNDTACUM-SALDO(W-LONG)  TO WI-Z11P
           DISPLAY WI-Z11P LINE K-LINEA POSITION 62 REVERSE
           IF K-LINEA > 20
              MOVE SPACES TO W-TRUCO
              PERFORM I-PARAR THRU F-PARAR UNTIL W-TRUCO = "C"
              PERFORM I-LIMPIA-PANTA THRU F-LIMPIA-PANTA
                      VARYING K-LINEA FROM 06 BY 1
                      UNTIL   K-LINEA > 21
              MOVE 05 TO K-LINEA.
       F-DISPLAY-DATOS.

       I-PARAR.
           DISPLAY " PRESIONE <C> PARA CONTINUAR "
             LINE 24 POSITION 1 REVERSE BEEP BLINK
           PERFORM ACEPTA-TRUCO.
       F-PARAR.

       I-LIMPIA-PANTA.
           DISPLAY SPACE LINE K-LINEA POSITION 02 SIZE 12.
           DISPLAY SPACE LINE K-LINEA POSITION 15 SIZE 40.
           DISPLAY SPACE LINE K-LINEA POSITION 62 SIZE 17.
       F-LIMPIA-PANTA.
      *
       I-LIMPIA-PANTA-F7.
           DISPLAY SPACE LINE K-LINEA POSITION 02 SIZE 20.
           DISPLAY SPACE LINE K-LINEA POSITION 23 SIZE 08.
           DISPLAY SPACE LINE K-LINEA POSITION 32 SIZE 13.
           DISPLAY SPACE LINE K-LINEA POSITION 46 SIZE 15.
           DISPLAY SPACE LINE K-LINEA POSITION 62 SIZE 18.
       F-LIMPIA-PANTA-F7. EXIT.
      *
       I-CEROS-VARIABLES.
               MOVE 0 TO W-TOTAL-DEBITOS W-TOTAL-CREDITOS
                         S-TOTAL-DEBITOS S-TOTAL-CREDITOS
                         T-TOTAL-DEBITOS T-TOTAL-CREDITOS
                         G-TOTAL-DEBITOS G-TOTAL-CREDITOS
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
       I-PANTALLA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(7) TO SG-BCOLOR.
           DISPLAY " " ERASE CONTROL SG-CONTROL-STRING
           "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออป" LINE 01 POSITION 01 REVERSE
           "บ
      -    "                   บ" LINE 02 POSITION 01 REVERSE
           "ฬออออออออออออหออออออออออออออออออออออออออออออออออออออออออออออ
      -    "หออออออออออออออออออน" LINE 03 POSITION 01 REVERSE
           "บC O D I G O บ              D E S C R I P C I O N
      -    "บ       S A L D O  บ" LINE 04 POSITION 01 REVERSE
           "ฬออออออออออออฮออออออออออออออออออออออออออออออออออออออออออออออ
      -    "ฮออออออออออออออออออน" LINE 05 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 06 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 07 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 08 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 09 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 10 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 11 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 12 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 13 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 14 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 15 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 16 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 17 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 18 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 19 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 20 POSITION 01 REVERSE
           "บ            บ
      -    "บ                  บ" LINE 21 POSITION 01 REVERSE
           "ศออออออออออออสออออออออออออออออออออออออออออออออออออออออออออออ
      -    "สออออออออออออออออออผ" LINE 22 POSITION 01 REVERSE
      *    "Fecha :"        LINE 02 POSITION 02 LOW
      *    "Hora :"         LINE 02 POSITION 67 LOW
           ACCEPT WK-HORA-HOY        FROM  TIME
           MOVE WK-HORAS               TO  WK-MASCARA-HORAS
           MOVE WK-MINUTOS             TO  WK-MASCARA-MINUTOS
           MOVE ":"                    TO  WK-PUNTOS
           DISPLAY WK-MASCARA-HORA  LINE 02 POSITION 74 REVERSE
           W-TIT-PANTALLA LINE 02 POSITION 20 REVERSE
           WK-NOMBRE-MES(W-LONG)    LINE 02 POSITION 59 .
           ACCEPT WK-FECHA-HOY FROM DATE.
           PERFORM I-MASC-NAC THRU F-MASC-NAC.
           IF SW-ERROR = 0
            DISPLAY WK-MASCARA-FECHA LINE 02 POSITION 03 REVERSE.
           PERFORM I-TITULO-EMPRESA THRU F-TITULO-EMPRESA.
       F-PANTALLA. EXIT.
      *
       I-PANTALLA-F7.
           MOVE SG-COLOR-TABLE(7) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(8) TO SG-BCOLOR.
           DISPLAY " " ERASE CONTROL SG-CONTROL-STRING
           "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออป" LINE 01 POSITION 01
           "บ
      -    "                   บ" LINE 02 POSITION 01 REVERSE
           "บ
      -    "                   บ" LINE 03 POSITION 01 REVERSE
           "ฬออออออออออออออออออออัออออออออัอออออออออออออัอออออออออออออออ
      -    "ัออออออออออออออออออน" LINE 04 POSITION 01 REVERSE
           "บ    C I U D A D     ณ FACTURAณFECHA FACTURAณ  SALDO FACTURA
      -    "ณ   TOTAL CIUDAD   บ" LINE 05 POSITION 01 REVERSE
           "วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
      -    "ลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ" LINE 06 POSITION 01 REVERSE
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 07 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 08 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 09 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 10 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 11 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 12 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 13 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 14 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 15 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 16 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 17 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 18 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 19 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 20 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 21 POSITION 01
           "บ                    ณ        ณ             ณ
      -    "ณ                  บ" LINE 22 POSITION 01
           "ศออออออออออออออออออออฯออออออออฯอออออออออออออฯอออออออออออออออ
      -    "ฯออออออออออออออออออผ" LINE 23 POSITION 01.
           DISPLAY
           W-TIT-PANTALLA LINE 02 POSITION 20
           "Codigo Empresa" LINE 03 POSITION 02 REVERSE
           "Nit" LINE 03 POSITION 21 REVERSE
           "Saldo total" LINE 03 POSITION 41 REVERSE.
           ACCEPT WK-HORA-HOY        FROM  TIME
           MOVE WK-HORAS               TO  WK-MASCARA-HORAS
           MOVE WK-MINUTOS             TO  WK-MASCARA-MINUTOS
           MOVE ":"                    TO  WK-PUNTOS
           DISPLAY WK-MASCARA-HORA  LINE 02 POSITION 74 REVERSE
           W-TIT-PANTALLA LINE 02 POSITION 20 REVERSE
           WK-NOMBRE-MES(W-LONG)    LINE 02 POSITION 59 .
           ACCEPT WK-FECHA-HOY FROM DATE.
           PERFORM I-MASC-NAC THRU F-MASC-NAC.
           IF SW-ERROR = 0
            DISPLAY WK-MASCARA-FECHA LINE 02 POSITION 03 REVERSE.
       F-PANTALLA-F7. EXIT.
      *
       I-MASC-NAC.
           MOVE ZEROS       TO SW-ERROR.
           MOVE WK-DIA      TO WK-MASCARA-DIA
           IF  WK-MES                       <  01
           OR  WK-MES                       >  12
           OR  WK-DIA                       <  01
           OR  WK-DIA                       >  31
           OR  WK-ANO                       <  20
               MOVE "ERR"                  TO  WK-MASCARA-DIA
               MOVE "ERR"                  TO  WK-MASCARA-MES
               MOVE "ERR"                  TO  WK-MASCARA-ANO
               MOVE 1 TO SW-ERROR
               GO F-MASC-NAC
           ELSE
               MOVE WK-ABREVIA (WK-MES)    TO  WK-MASCARA-MES.
           MOVE WK-ANO                 TO  WK-MASCARA-ANO
           MOVE "-"                    TO  WK-RAYA-1
           MOVE "-"                    TO  WK-RAYA-2
           COMPUTE WK-ANO-BISIESTO      =  WK-ANO-HASTA
                                        /  4
           COMPUTE WK-ANO-BISIESTO      =  WK-ANO-BISIESTO
                                        *  4
           IF  WK-ANO-BISIESTO          =  WK-ANO-HASTA
               MOVE 29                 TO  WK-DIAS-MES (2)
           ELSE
               MOVE 28                 TO  WK-DIAS-MES  (2).
       F-MASC-NAC.
      ***************************************************************
      *          C I E R R E    D E    A R C H I V O S              *
      ***************************************************************
       I-FIN-MODE.
           CLOSE CNCATCTA CNNITCED
                 EMPRESAS
                 CNDTACUM.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
               COPY "..\PRO\USUARIOS.PRO".
               COPY "..\PRO\OPCIONES.PRO".
               COPY "..\PRO\NO-ERROR.PRO".
               COPY "..\PRO\TITULO1E.PRO".
               COPY "..\PRO\CNCONTRO.PRO".
               COPY "..\PRO\TRUQUITO.PRO".
               COPY "..\PRO\PANTALLA.PRO".

               COPY "..\PLB\CNDTACUM.PLB".
               COPY "..\PLB\WTEMHISN.PLB".
      ******************************************************************

