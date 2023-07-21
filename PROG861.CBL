       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG861.
      ******************************************************************
      *       CAPTURA Y ACTUALIZACION RECIBOS  DE PAGO ABR-27-2002     *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\TIPEGRES.DYC".
               COPY "..\DYC\CNRECPAG.DYC".
               COPY "..\DYC\NOMIBANK.DYC".
               COPY "..\DYC\CNCOMPTE.DYC".
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\CNCATCTA.DYC".
               COPY "..\DYC\CNNITCED.DYC".
               COPY "..\DYC\CNNITANT.DYC".
               COPY "..\DYC\CONTRATO.DYC".
               COPY "..\DYC\HOJAVIDA.DYC".
               COPY "..\DYC\CNCESANT.DYC".
               COPY "..\DYC\CNPRIMAS.DYC".
               COPY "..\DYC\CNVACACI.DYC".
               COPY "..\DYC\CNSUELDO.DYC".
               COPY "..\DYC\TBCONDOC.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\TIPEGRES.FD".
               COPY "..\FD\CNNITANT.FD".
               COPY "..\FD\CNRECPAG.FD".
               COPY "..\FD\NOMIBANK.FD".
               COPY "..\FD\CNCOMPTE.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\CNNITCED.FD".
               COPY "..\FD\CONTRATO.FD".
               COPY "..\FD\CNCESANT.FD".
               COPY "..\FD\HOJAVIDA.FD".
               COPY "..\FD\CNPRIMAS.FD".
               COPY "..\FD\CNVACACI.FD".
               COPY "..\FD\TBCONDOC.FD".
               COPY "..\FD\CNSUELDO.FD".
       WORKING-STORAGE SECTION.
       77  ID-PRG                     PIC X(7)  VALUE "PROG861".
       77  W-PROGRAMA                 PIC X(7)  VALUE "PROG861".
       77  WKK-DIA                    PIC S99   VALUE  ZEROS.
       77  WK-RADICA                  PIC 9(10) VALUE  ZEROS.
       77  WK-VLRDOC                  PIC S9(12) VALUE ZEROS.
       77  WK-VALDOC                  PIC S9(12) VALUE ZEROS.
       77  WK-DIFSAL                  PIC S9(15)V99 VALUE ZEROS.
       77  WK-DIFSALT                 PIC S9(15)V99 VALUE ZEROS.
       77  WK-NUMDOC                  PIC 9(6)      VALUE  ZEROS.
       77  WK-CREDITOS                PIC S9(15)V99 VALUE  ZEROS.
       77  WK-DEBITOS                 PIC S9(15)V99 VALUE  ZEROS.
       77  WK-ULTRAD                  PIC 9(6)      VALUE  ZEROS.
       77  W-EMPRESA1                 PIC X(53)  VALUE SPACES.
       77  WK-EMPRESAS                PIC X(40)  VALUE SPACES.
       77  WK-NIT                     PIC 9(12)  VALUE ZEROS.
       77  Z                          PIC 9(3)   VALUE ZEROS.
       77  FLAG-TRABAJO               PIC X(1)   VALUE ZEROS.
       77  JJ                         PIC 9(3)   VALUE ZEROS.
       77  S                          PIC 9(3)   VALUE ZEROS.
       77  WK-TIPO                    PIC 99     VALUE ZEROS.
       77  WK-CUENTAS                 PIC 9(6)   VALUE ZEROS.
       77  CREA-NIT                   PIC X      VALUE SPACES.
       77  SW-VALORES                 PIC 9      VALUE ZEROS.
       77  NUM-REG                    PIC 9(6)   VALUE ZEROS.
       77  COL1                       PIC 9      VALUE 8.
       77  COL2                       PIC 9      VALUE 2.
       77  WK-CONCEPTO                PIC X(40)  VALUE SPACES.
       77  COLTIT1                    PIC 9      VALUE 8.
       77  COLTIT2                    PIC 9      VALUE 6.
       77  BORRADOS                   PIC 9(4)   VALUE ZEROS.
       77  ZZ-8                       PIC ZZ,ZZZ,ZZZ  VALUE ZEROS.
      ****************************************************************
      *       TABLA DE REGISTROS - PARA MANEJO EN MEMORIA -          *
      ****************************************************************
        01   REGISTRO-TABLA-TRABAJO.
            03 TABLA-REGISTRO-TRAB    OCCURS  300  TIMES.
              05  TAB-CUENTA.
                  07  TAB-MAYCTA      PIC 9(04).
                  07  TAB-SUBCTA      PIC 9(02).
                  07  TAB-AUXCTA      PIC 9(04).
              05  NUMFRA              PIC 9(08).
              05  WK-VFACT            PIC S9(12)V99.
              05  TAB-NIT             PIC 9(12).
              05  TAB-FECVEN          PIC 9(08).
              05  TAB-CONCEPTO        PIC X(40).
              05  TAB-VLRDOC          PIC S9(12)V99.
              05  TAB-VLRBAS          PIC 9(09).
              05  TAB-SIGNO           PIC 9.
              05  TAB-CONDICION       PIC 9.
              05  TAB-TIPREG          PIC X.
              05  TAB-VALOR           PIC S9(12)V99.
       01  REG-TRAB-TEGRESOS.
           03 WK-TIPOE                PIC 9(0005) VALUE ZEROS.
           03 W-CED-ANTCES            PIC 9(0012) VALUE ZEROS.
           03 WK-NIT-CON              PIC 9(0012) VALUE ZEROS.
           03 WK-CONTROL              PIC X(0004) VALUE ZEROS.
           03 WK-DESCRIP              PIC X(60)   VALUE SPACES.
           03 WK-DESCRIP1             PIC X(50)   VALUE SPACES.
           03 WK-DESCRIP2             PIC X(10)   VALUE SPACES.
           03 WK-DD                   PIC 9(0002) VALUE ZEROS.
           03 WK-ANTICIPO             PIC 9(0002) VALUE ZEROS.
           03 WK-IVA                  PIC X VALUE SPACES.
           03 WK-RET                  PIC X VALUE SPACES.
           03 WK-RETP                 PIC 9(0002) VALUE ZEROS.
           03 WK-SIG                  PIC 9(0001) VALUE ZEROS.
           03 SW-ADIC                 PIC 9 VALUE ZEROS.
           03 WK-VRBAS                PIC S9(12)V99  VALUE ZEROS.
           03 WK-VRBASE               PIC S9(12)V99  VALUE ZEROS.
           03 WK-VFACT1               PIC S9(12)V99  VALUE ZEROS.
           03 WK-VFACT2               PIC S9(12)V99  VALUE ZEROS.
           03 WK-VFACT3               PIC S9(12)V99  VALUE ZEROS.
           03 WK-VFACT4               PIC S9(12)V99  VALUE ZEROS.
           03 WK-VFACT5               PIC S9(12)V99  VALUE ZEROS.
           03 FACTURAS                PIC S9(12)V99  VALUE ZEROS.
           03 WK-CHEQUEORD            PIC 9(05)  VALUE ZEROS.
           03 WK-EMPRE                PIC 9(07)  VALUE ZEROS.
           03 WK-EMPRE-INICIAL        PIC 9(07)  VALUE ZEROS.
           03 WK-NOMBRE               PIC X(18)  VALUE SPACES.
           03 WK-VRFACTURA            PIC 9(12)V99  VALUE ZEROS.
           03 WI-Z99                  PIC ZZZ,ZZZ,ZZZ.-.
       01   CONSEC-TRAB.
            03 CONSEC-LLAVE.
               05  TAB-CONSE1      PIC 9(01)  VALUE ZEROS.
               05  TAB-CONSE2      PIC 9(05)  VALUE ZEROS.
       01   REG-ANT-TRAB.
            03 TABLA-ANT           OCCURS  6    TIMES.
              05 TAB-RESOLU          PIC 9(06).
              05  TAB-FRES.
                  07 A-RES            PIC 9(04).
                  07 M-RES            PIC 99.
                  07 D-RES            PIC 99.
              05  TAB-VALANT          PIC 9(12).
              05  TAB-FDES.
                  07 A-DES            PIC 9(04).
                  07 M-DES            PIC 99.
                  07 D-DES            PIC 99.
              05  TAB-FHAS.
                  07 A-HAS            PIC 9(04).
                  07 M-HAS            PIC 99.
                  07 D-HAS            PIC 99.
              05  TAB-FPROCES         PIC 9(08).
              05  TAB-DIASPRO         PIC 99.
              05  TAB-COND            PIC 9.
            03  GG                    PIC 99 VALUE ZEROS.
              COPY "..\LBL\EMPRESAS.LBL".
              COPY "..\LBL\TIPEGRES.LBL".
              COPY "..\LBL\NOMIBANK.LBL".
              COPY "..\LBL\CNRECPAG.LBL".
              COPY "..\LBL\CNCOMPTE.LBL".
              COPY "..\LBL\CNNITCED.LBL".
              COPY "..\LBL\CNNITANT.LBL".
              COPY "..\LBL\USER-ACT.LBL".
              COPY "..\LBL\TBCONDOC.LBL".
              COPY "..\LBL\CNCATCTA.LBL".
              COPY "..\LBL\CONTRATO.LBL".
              COPY "..\LBL\CNCESANT.LBL".
              COPY "..\LBL\HOJAVIDA.LBL".
              COPY "..\LBL\CNPRIMAS.LBL".
              COPY "..\LBL\CNVACACI.LBL".
              COPY "..\LBL\CNSUELDO.LBL".
              COPY "..\WRK\USER-ACT.WRK".
              COPY "..\WRK\CNDOCTOS.WRK".
              COPY "..\WRK\TEMPCONT.WRK".
              COPY "..\WRK\VARIABLE.WRK".
              COPY "..\WRK\TABLAMES.WRK".
      ****************************************************************
      *              TABLA DE AYUDA   TIPO DE EGRESOS                *
      ****************************************************************
       01  TABLA-EGRESOS.
           03  FILLER         PIC X(30) VALUE
                                 "1. CAJA - BANCOS - INVERSIONES".
           03  FILLER         PIC X(30) VALUE
                                 "2. DEUDORES VARIOS            ".
           03  FILLER         PIC X(30) VALUE
                                 "3. PLANTA - EQUIPO y DIFERIDOS".
           03  FILLER         PIC X(30) VALUE
                                 "4. OBLIGACIONES FINANCIERAS   ".
           03  FILLER         PIC X(30) VALUE
                                 "5. CUENTAS POR PAGAR          ".
           03  FILLER         PIC X(30) VALUE
                                 "6. IMPUESTOS E INTERESES      ".
           03  FILLER         PIC X(30) VALUE
                                 "7. NOMINA - PREST.SOC. LIQ.   ".
           03  FILLER         PIC X(30) VALUE
                                 "8. GASTOS GENERALES           ".
           03  FILLER         PIC X(30) VALUE
                                 "9. COSTOS                     ".
       01  TABEGRE1 REDEFINES TABLA-EGRESOS.
           03 TAB-EGRESO OCCURS 9 TIMES  PIC X(30).
              COPY "..\WRK\TECLADOS.WRK".
              COPY "..\WRK\WK-RAYAS.WRK".
              COPY "..\WRK\IO-ERROR.WRK".
              COPY "..\WRK\LIQUINOV.WRK".
              COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION USING AREA-LINK-NOMINA.
       DECLARATIVES.
              COPY "..\DCL\CNNITCED.DCL".
              COPY "..\DCL\EMPRESAS.DCL".
              COPY "..\DCL\TBCONDOC.DCL".
              COPY "..\DCL\CNCOMPTE.DCL".
              COPY "..\DCL\USER-ACT.DCL".
              COPY "..\DCL\CNCATCTA.DCL".
              COPY "..\DCL\CNNITANT.DCL".
              COPY "..\DCL\TIPEGRES.DCL".
              COPY "..\DCL\NOMIBANK.DCL".
              COPY "..\DCL\CNRECPAG.DCL".
              COPY "..\DCL\CONTRATO.DCL".
              COPY "..\DCL\CNCESANT.DCL".
              COPY "..\DCL\HOJAVIDA.DCL".
              COPY "..\DCL\CNPRIMAS.DCL".
              COPY "..\DCL\CNVACACI.DCL".
              COPY "..\DCL\CNSUELDO.DCL".
       END DECLARATIVES.
      ***********************************************************
       PROGRAMA-PRINCIPAL SECTION.
      ***********************************************************
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           MOVE " RECIBOS DE PAGOS "  TO   TIT-APLICACION.
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF SW-USER = 1      EXIT PROGRAM.
           PERFORM I-PANTALLA-1B    THRU F-PANTALLA-1B.
           MOVE   WK-MES            TO W-LONG.
           DISPLAY WK-NOMBRE-MES(W-LONG) LINE 11 POSITION 54 REVERSE.
           DISPLAY  W-LONG LINE 11 POSITION 50 REVERSE
           PERFORM I-ACEPTA-MES     THRU F-ACEPTA-MES
           PERFORM I-CONFIRMA-COMP  THRU F-CONFIRMA-COMP.
           MOVE CON-COD-EMPRESA     TO  WK-EMPRE.
           MOVE CON-COD-EMPRESA     TO  WK-EMPRE-INICIAL.
           PERFORM I-LABEL-ARCHIVOS THRU F-LABEL-ARCHIVOS.
           PERFORM I-STATUS-CNRECPAG  THRU F-STATUS-CNRECPAG.
           PERFORM I-ABRE-ARCHIVOS  THRU F-ABRE-ARCHIVOS.
           PERFORM I-MENU-OPCIONES  THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN            THRU F-FIN.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
            PERFORM I-LABEL-CNRECPAG    THRU F-LABEL-CNRECPAG.
            PERFORM I-LABEL-CNCESANT    THRU F-LABEL-CNCESANT.
            PERFORM I-LABEL-CNPRIMAS    THRU F-LABEL-CNPRIMAS.
            PERFORM I-LABEL-CNVACACI    THRU F-LABEL-CNVACACI.
            PERFORM I-LABEL-CNSUELDO    THRU F-LABEL-CNSUELDO.
            PERFORM I-LABEL-CONTRATO    THRU F-LABEL-CONTRATO.
            MOVE WK-EMPRE               TO  LAB-CONTRATO-CIA.
            MOVE 16                     TO  POS-DOWN.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           OPEN INPUT CNCATCTA EMPRESAS TIPEGRES
           OPEN INPUT NOMIBANK CNRECPAG.
       F-ABRE-ARCHIVOS.
      *
       I-MENU-OPCIONES.
           MOVE W-LINEA TO I-LINEA
           MOVE 999     TO   WK-EXCEPTION
           PERFORM I-CREAR-WINDOW THRU F-CREAR-WINDOW
           MOVE SG-COLOR-TABLE(COLTIT1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(COLTIT2) TO SG-BCOLOR.
           DISPLAY WEMPRESA
                 LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY W-DIR-EMP
                 LINE 25 POSITION 45 CONTROL SG-CONTROL-STRING.
           MOVE SG-COLOR-TABLE(COL1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(COL2) TO SG-BCOLOR.
           MOVE ZEROS           TO   FIN-OK
           PERFORM I-SELECCIONA THRU F-SELECCIONA
                              UNTIL SALIDA OR ESC.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY SPACE LINE 24 POSITION 01 CONTROL SG-CONTROL-STRING.
           IF ESC
              MOVE 19 TO I-LINEA
              MOVE 999             TO   WK-EXCEPTION
              PERFORM I-WINDOW-FIN THRU F-WINDOW-FIN
              PERFORM I-SALIR      THRU F-SALIR UNTIL SALIDA OR ESC
              IF I-LINEA = 19 AND SALIDA
                 PERFORM  I-FIN THRU F-FIN
              ELSE
                 GO I-MENU-OPCIONES
           ELSE
           IF  PROSS-XKK = "N"
               NEXT SENTENCE
           ELSE
           MOVE ZEROS TO SW-ROMPE SW-EMPR WK-EMP-ROMPE.
           IF  W-OPCION  =  "A" AND PROSS-AKK = "S"
               PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
            
               PERFORM I-ADICIONA    THRU  F-ADICIONA
                                     UNTIL ESC
               MOVE ZEROS            TO    WK-EXCEPTION
           ELSE
             IF  W-OPCION  =  "C" AND PROSS-CKK = "S"
                 PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
                 PERFORM I-CONSULTA    THRU  F-CONSULTA
                                       UNTIL ESC
                 MOVE ZEROS            TO    WK-EXCEPTION
             ELSE
               IF  W-OPCION  =  "M" AND PROSS-MKK = "S"
                   PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
                   PERFORM I-MODIFICA    THRU  F-MODIFICA
                                         UNTIL ESC
                   MOVE ZEROS            TO    WK-EXCEPTION
               ELSE
                 IF  W-OPCION  =  "R" AND PROSS-RKK = "S"
                      PERFORM I-LLAMA-WINDOW THRU F-LLAMA-WINDOW
                      PERFORM I-RETIRA      THRU  F-RETIRA
                                            UNTIL ESC
                      MOVE ZEROS            TO    WK-EXCEPTION.
       F-MENU-OPCIONES.
      *
       I-FIN.
                CLOSE CNCATCTA
                CLOSE  EMPRESAS
                       NOMIBANK
                       CNRECPAG
                       TIPEGRES.
               EXIT    PROGRAM.
               STOP    RUN.
       F-FIN.
      *
       I-LLAMA-WINDOW.
           PERFORM I-WINDOW-CAB THRU F-WINDOW-CAB.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
       F-LLAMA-WINDOW.
      *
       I-DISPLAY-OPC.
           IF  W-OPCION                     =  "A"
               DISPLAY " A D I C I O N "            LINE  2 POSITION 66
                                                    REVERSE BLINK
           ELSE
           IF  W-OPCION                     =  "C"
               DISPLAY "   CONSULTA    "            LINE  2 POSITION 66
                                                    REVERSE  BLINK
           ELSE
           IF  W-OPCION                     =  "M"
               DISPLAY " MODIFICACION  "            LINE  2 POSITION 66
                                                    REVERSE  BLINK
           ELSE
           IF  W-OPCION                     =  "R"
               DISPLAY "  R E T I R O  "            LINE  2 POSITION 66
                                                    REVERSE BLINK.
       F-DISPLAY-OPC.
      *
       I-ADICIONA.
           PERFORM I-BORRA-TEMP   THRU  F-BORRA-TEMP.
           PERFORM I-BORRA-TABLA  THRU  F-BORRA-TABLA.
           PERFORM I-DISPLAY-OPC  THRU  F-DISPLAY-OPC.
           MOVE 1 TO SW-ADIC.
           DISPLAY W-ULTPRO      LINE 02 POSITION 13 REVERSE
           IF      CUP OR ESC     GO   F-ADICIONA.
             MOVE  SPACES  TO WK-ORIGEN.
         I-ACDOC.
               DISPLAY SPACE LINE 24 POSITION 01 SIZE 80
               PERFORM I-DATOS            THRU F-DATOS
               PERFORM I-LLAMA-WINDOW     THRU F-LLAMA-WINDOW.
       F-ADICIONA.
      *
       I-CONSULTA.
           MOVE   2     TO SW-ADIC.
           PERFORM  I-BORRA-TEMP   THRU  F-BORRA-TEMP.
           PERFORM I-DISPLAY-OPC   THRU  F-DISPLAY-OPC.
           PERFORM I-TIPCOM        THRU  F-TIPCOM.
           IF      CUP             OR
                   ESC             GO    F-CONSULTA.
            IF SW-LECTURA = 1
                  PERFORM I-CONSULTA.
               DISPLAY SPACE LINE 24 POSITION 01 SIZE 80
               PERFORM I-BORRA-TABLA    THRU  F-BORRA-TABLA
               PERFORM I-CARGA-TABLA    THRU  F-CARGA-TABLA.
               MOVE    ZEROS            TO    K
               PERFORM I-MOSTRAR-CAB    THRU  F-MOSTRAR-CAB
               PERFORM I-PANTA-7REG     THRU  F-PANTA-7REG
               PERFORM I-CONSULTAR      THRU  F-CONSULTAR
               PERFORM I-LLAMA-WINDOW   THRU  F-LLAMA-WINDOW.
       F-CONSULTA.
      *
       I-MODIFICA.
           MOVE   2     TO SW-ADIC.
           MOVE SPACES  TO RES-OK.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
           PERFORM  I-BORRA-TEMP   THRU  F-BORRA-TEMP.
           PERFORM  I-TIPCOM      THRU  F-TIPCOM.
           MOVE  SPACES  TO WK-ORIGEN.
           IF SW-LECTURA = 1
                  PERFORM I-MODIFICA.
           IF      CUP          OR
                   ESC          GO   F-MODIFICA.
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 80
           PERFORM I-BORRA-TABLA    THRU F-BORRA-TABLA
           PERFORM I-CARGA-TABLA    THRU F-CARGA-TABLA
           MOVE ZEROS TO K
           PERFORM I-MOSTRAR-CAB    THRU F-MOSTRAR-CAB
           PERFORM I-PANTA-7REG     THRU F-PANTA-7REG
           PERFORM I-MODIFICARC  THRU  F-MODIFICARC.
           IF  F1  GO I-MODIFICA.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           PERFORM I-DATOS          THRU F-DATOS
           PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW.
       F-MODIFICA.
      *
       I-RETIRA.
           MOVE   2     TO SW-ADIC.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
           CLOSE CNRECPAG.
           OPEN I-O CNRECPAG.
           PERFORM I-TIPCOM      THRU  F-TIPCOM.
           IF      CUP          OR
                   ESC          GO   F-RETIRA.
              IF SW-LECTURA = 1
                  PERFORM I-RETIRA.
               DISPLAY SPACE LINE 24 POSITION 01 SIZE 80
               PERFORM I-BORRA-TABLA    THRU F-BORRA-TABLA.
               PERFORM I-MOSTRAR-CAB    THRU F-MOSTRAR-CAB
               PERFORM I-CARGA-TABLA    THRU F-CARGA-TABLA
               PERFORM I-PANTA-7REG     THRU F-PANTA-7REG.
               PERFORM I-ELIMINAR       THRU F-ELIMINAR
               PERFORM I-LLAMA-WINDOW   THRU F-LLAMA-WINDOW.
       F-RETIRA.
      *
      *****************************************************************
      *
       I-DATOS.
           IF W-OPCION = "M"
              IF MOD-USRKK(1) = "N" GO F-DATOS.
           IF W-OPCION = "M"
              IF CONDIC-PAG  = 1
                 DISPLAY " DOCUMENTO EN CONDICION 1 *ACTUALIZADO*"
                 LINE 24   POSITION 01 REVERSE
                 ACCEPT X  LINE 24 POSITION 01
                 GO F-DATOS.
              IF CONDIC-PAG  = 3
                 DISPLAY " DOCUMENTO EN CONDICION 1 *ACTUALIZADO*"
                 LINE 24   POSITION 01 REVERSE
                 ACCEPT X  LINE 24 POSITION 01
                 GO F-DATOS.
           IF W-OPCION = "M"
                PERFORM I-FECHA-DESDE  THRU  F-FECHA-DESDE
                PERFORM I-BANCOS       THRU  F-BANCOS
                PERFORM I-CONCEPTO     THRU  F-CONCEPTO
                PERFORM I-CONCEPTO1    THRU  F-CONCEPTO1
                PERFORM I-CONCEPTO2    THRU  F-CONCEPTO2
                PERFORM I-CONTROL      THRU  F-CONTROL
                PERFORM I-NIT-CONTABLE THRU  F-NIT-CONTABLE
            ELSE
               PERFORM I-DATOS-CAB    THRU  F-DATOS-CAB.
           IF  ESC OR  CUP  GO  F-DATOS.
           MOVE    ZEROS       TO   WK-EXCEPTION
           MOVE    SPACES      TO   W-TRUCO
           IF WK-DIFSAL NOT EQUAL ZEROS
                    GO F-DATOS.
           IF W-OPCION = "L"
                    MOVE WK-ORIGEN    TO   W-OPCION.
            MOVE  ZEROS TO  XX.
           IF      W-OPCION   =   "A"
                   PERFORM I-GRABACION   THRU F-GRABACION
                   UNTIL  W-TRUCO = "A"  OR  F1 OR CUP
           ELSE
                   PERFORM I-REGRABACION THRU F-REGRABACION
                   UNTIL  W-TRUCO = "M" OR F1 OR CUP.
       F-DATOS.
      *
       I-EVAL-SALDOS.
           MOVE ZEROS TO WK-DIFSAL W-TOTAL-CREDITOS W-TOTAL-DEBITOS.
           MOVE ZEROS TO K.
           IF W-OPCION = "A"  OR RES-OK = "M"
                ADD  TAB-VALOR(6)  TO  T-TOTAL-CREDITOS.
           IF W-OPCION = "L"
               MOVE TAB-VALOR(6) TO T-TOTAL-CREDITOS.
           COMPUTE T-TOTAL-CREDITOS = T-TOTAL-CREDITOS * -1.
           COMPUTE WK-DIFSAL = T-TOTAL-DEBITOS + T-TOTAL-CREDITOS.
           MOVE    T-TOTAL-CREDITOS  TO  WI-Z99.
           DISPLAY WI-Z99 LINE 13   POSITION 65 REVERSE.
           MOVE    T-TOTAL-DEBITOS   TO  WI-Z99.
           DISPLAY WI-Z99 LINE 12   POSITION 65 REVERSE.
           MOVE    WK-DIFSAL         TO  WI-Z99
           DISPLAY WI-Z99 LINE 14   POSITION 65 LOW REVERSE.
       F-EVAL-SALDOS.
      *
      *****************************************************************
      *    MODULO  MANEJO  DE  WINDOWS CABECERA                       *
      *****************************************************************
       I-DATOS-CAB.
           MOVE  ZEROS   TO  W-SW.
           PERFORM I-BORRA-TEMP-CALC  THRU F-BORRA-TEMP-CALC.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           IF      CUP             GO  F-DATOS-CAB.
         CAB-0.
           MOVE ZEROS  TO FLAG-Z.
           PERFORM I-EMPRESA  THRU  F-EMPRESA.
                   IF             CUP  GO CAB-0.
                   IF             ESC  GO F-DATOS-CAB.

         CAB-1.
           PERFORM I-TIPEGRESOS   THRU  F-TIPEGRESOS.
                   IF             CUP  GO CAB-0.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-2.
           PERFORM I-NIT         THRU  F-NIT.
                   IF             CUP  GO CAB-1.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-21.
           PERFORM I-NIT-CONTABLE THRU  F-NIT-CONTABLE.
                   IF             CUP  GO CAB-2.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-22.
           IF ANTICIPO-EGR   =    ZEROS   GO CAB-3.
           PERFORM I-ANTICIPO    THRU  F-ANTICIPO.
           IF W-TRUCO NOT = "G"  GO CAB-1.
                   IF             CUP  GO CAB-2.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-3.
           PERFORM I-FECHA-DESDE THRU  F-FECHA-DESDE.
                   IF             CUP  GO CAB-2.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-4.
           DISPLAY SPACES LINE 23  POSITION 01 SIZE 80.
           PERFORM I-BANCOS      THRU  F-BANCOS.
                   IF             CUP  GO CAB-3.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-5.
           IF             CUP  GO CAB-4.
           IF W-SUBTOT   NOT = ZEROS  GO CAB-8.
           MOVE  ZEROS         TO  W-SW.
           PERFORM I-ESPECIAL      THRU  F-ESPECIAL.
                   IF               CUP  GO CAB-4.
                   IF               ESC  GO F-DATOS-CAB.
         CAB-05.
           IF Z = 1  GO  CAB-R.
           PERFORM I-LEE-FACTURA-CNCATCTA  THRU
                   F-LEE-FACTURA-CNCATCTA.
           IF INDDETAL NOT = "S"  GO  CAB-50.
         CAB-R.
           MOVE  ZEROS    TO  JJ.
           PERFORM I-PANTALLA-FACTURA THRU F-PANTALLA-FACTURA.
           PERFORM I-ACEPTA-FACTURAS  THRU F-ACEPTA-FACTURAS
                        UNTIL ESC OR F1 OR JJ > 8.
           PERFORM I-BLQPAN           THRU F-BLQPAN.
           PERFORM I-WIN-CTAS         THRU F-WIN-CTAS.
           GO CAB-9.
         CAB-50.
           PERFORM I-VRFACT1     THRU F-VRFACT1.
                   IF             CUP  GO CAB-4.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-61.
           IF Z = ZEROS   GO     CAB-8.
           PERFORM I-VRFACT2     THRU F-VRFACT2.
                   IF             CUP  GO CAB-50.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-71.
           PERFORM I-VRFACT3     THRU  F-VRFACT3.
                   IF             CUP  GO CAB-61.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-72.
           PERFORM I-VRFACT4     THRU  F-VRFACT4.
                   IF             CUP  GO CAB-71.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-73.
           PERFORM I-VRFACT5     THRU  F-VRFACT5.
                   IF             CUP  GO CAB-72.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-8.
           MOVE     ZEROS         TO   W-SW.
           PERFORM I-VRBASE      THRU  F-VRBASE
                     IF           CUP  GO CAB-50.
                     IF           ESC  GO F-DATOS-CAB.
           PERFORM I-WIN-CTAS    THRU  F-WIN-CTAS.
         CAB-9.
           PERFORM I-CONCEPTO    THRU  F-CONCEPTO.
                   IF             CUP  GO CAB-8.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-91.
           PERFORM I-CONCEPTO1   THRU  F-CONCEPTO1.
                   IF             CUP  GO CAB-9.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-10.
           PERFORM I-CONCEPTO2   THRU  F-CONCEPTO2.
                   IF             CUP  GO CAB-91.
                   IF             ESC  GO F-DATOS-CAB.
         CAB-18.
           IF  TAB-MAYCTA(1)  = 2205  AND
               TAB-SUBCTA(1)  = 10
     
               MOVE  SPACES               TO  WK-CONTROL
           ELSE
               MOVE  SPACES               TO  WK-CONTROL.
         CAB-11.
           MOVE    INDBASE             TO WK-RET.
           MOVE    PORC-CTA            TO WK-RETP.
           DISPLAY WK-RET              LINE 09 POSITION 75 REVERSE
           DISPLAY WK-RETP             LINE 10 POSITION 75 REVERSE.
         CAB-12.
           MOVE        8 TO I.
           MOVE    ZEROS TO J.
           MOVE    ZEROS TO WK-EXCEPTION.
           PERFORM I-CUENTAS THRU F-CAPTURA-CTAS.
           PERFORM ACEPTA-TRUCO.
           IF CUP  GO CAB-10.
       F-DATOS-CAB.
      *
       I-CUENTAS.
           PERFORM  I-BORRA-TEMP-CALC   THRU F-BORRA-TEMP-CALC.
           PERFORM I-PANTA-7REG THRU F-PANTA-7REG.
           PERFORM I-IMPRIMIR-BANCO     THRU F-IMPRIMIR-BANCO.
           PERFORM I-EVAL-SALDOS        THRU F-EVAL-SALDOS.
           IF WK-DIFSAL NOT = ZEROS
                 MOVE    WK-DIFSAL         TO  WI-Z99
                 DISPLAY WI-Z99 LINE 14 POSITION 65 REVERSE
                 DISPLAY " COMPROBANTE DESCUADRADO "
                 LINE 24 POSITION 40 REVERSE BLINK
                 PERFORM ACEPTA-TRUCO.
             DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
         F-CAPTURA-CTAS.
      *
       I-PANTA-7REG.
           PERFORM I-BORRA-TEMP-CALC  THRU F-BORRA-TEMP-CALC.
           IF W-OPCION  = "A"  OR  RES-OK = "M"
                   PERFORM I-BORRA-VECTOR  THRU F-BORRA-VECTOR
                   VARYING L FROM 1 BY 1 UNTIL L > 8.
           MOVE  0     TO K.
           MOVE  8     TO I.
           MOVE  0     TO S.
        I-LOOP-K.
           ADD   1 TO K.
           ADD   1 TO I.
           IF K > 8        GO PANTA-6REG.
           IF    TAB-MAYCTA(K) = ZEROS    GO I-LOOP-K.
           DISPLAY TAB-MAYCTA(K)     LINE I POSITION 03 REVERSE
           DISPLAY TAB-SUBCTA(K)     LINE I POSITION 09 REVERSE
           DISPLAY TAB-AUXCTA(K)     LINE I POSITION 12 REVERSE
           DISPLAY TAB-SIGNO(K)      LINE I POSITION 56 REVERSE.
           PERFORM I-IMPRIMIR-CTA    THRU F-IMPRIMIR-CTA.
           DISPLAY NOMCTA    LINE  I   POSITION  19 REVERSE  SIZE 16.
           IF W-OPCION  = "A"  OR  RES-OK = "M"
                   PERFORM I-CONTA     THRU  F-CONTA
                   MOVE    TAB-VALOR(K)  TO  WI-Z99
                   DISPLAY WI-Z99      LINE  I  POSITION 38 REVERSE
                ELSE
                   MOVE  PAG-VAL(K)   TO TAB-VALOR(K)
                   MOVE  PAG-VAL(K)   TO WI-Z99
                   DISPLAY WI-Z99        LINE I POSITION 38 REVERSE.
            IF W-OPCION NOT = "A"    AND RES-OK NOT = "M"
             IF TAB-SIGNO(K) = 1
             COMPUTE T-TOTAL-DEBITOS  = T-TOTAL-DEBITOS  + TAB-VALOR(K)
           ELSE
             COMPUTE T-TOTAL-CREDITOS = T-TOTAL-CREDITOS + TAB-VALOR(K).
           DISPLAY   " "     LINE 25 POSITION 80.
           GO        I-LOOP-K.
       PANTA-6REG.
            IF W-OPCION NOT = "A"  AND  RES-OK  NOT = "M"
                        PERFORM I-EVAL-SALDOS THRU  F-EVAL-SALDOS.
       F-PANTA-7REG.
    *
       I-BORRA-VECTOR.
           MOVE ZEROS           TO TAB-VALOR(L).
       F-BORRA-VECTOR.
      *
       I-CONTA.
           MOVE   ZEROS         TO WK-SALARIO.
           MOVE   ZEROS         TO FACTURAS.
           IF   INDBASE  = "N"  GO CONTA-1.
           IF     Z = 1         GO CONTA-1.
           IF WK-RET  =  "S"
              COMPUTE WK-SALARIO ROUNDED = ( WK-VRBAS * PORC-CTA ) / 100
              MOVE    WK-SALARIO      TO   TAB-VALOR(K).
       CONTA-1.
           IF   CTA-SIGNO(K)   =  1
           MOVE     WK-VRBAS  TO  TAB-VALOR(K).
           COMPUTE  VR-APLICA  =  VR-APLICA + WK-SALARIO.
        CONTA-2.
           IF Z = 1  MOVE 1 TO FLAG-Z.
           IF Z = 1  AND K = 1
      
                    MOVE WK-VFACT(1) TO TAB-VALOR(K).
           IF Z = 1  AND  K = 2
    
                    MOVE WK-VFACT(2) TO TAB-VALOR(K).
           IF Z = 1  AND  K = 3
     
                    MOVE WK-VFACT(3) TO TAB-VALOR(K).
           IF Z = 1  AND  K = 4
      
                    MOVE WK-VFACT(4) TO TAB-VALOR(K).
           IF Z = 1  AND  K = 5
     
                    MOVE WK-VFACT(5) TO TAB-VALOR(K).
        CONTA-3.
           IF  TAB-VALOR(K)  IS  NEGATIVE
                 COMPUTE TAB-VALOR(K) =   TAB-VALOR(K) *  -1.
           IF CTA-SIGNO(K) = 1
            COMPUTE T-TOTAL-DEBITOS = T-TOTAL-DEBITOS + TAB-VALOR(K)
             ELSE
            COMPUTE T-TOTAL-CREDITOS = T-TOTAL-CREDITOS + TAB-VALOR(K).
       F-CONTA.
      *
       I-MOSTRAR-CAB.
           MOVE    EMPRESA-PAG         TO WK-EMPRE.
           PERFORM I-LEE-EMPRESA  THRU  F-LEE-EMPRESA.
           MOVE    CHEQUEORD           TO WK-CHEQUEORD  WI-Z5P.
           DISPLAY WI-Z5P              LINE 02 POSITION 13 REVERSE.
           MOVE    CONCEPTO-PAG        TO WK-DESCRIP.
           MOVE    CONCEPTO1-PAG       TO WK-DESCRIP1.
           MOVE    CONCEPTO2-PAG       TO WK-DESCRIP2.
           DISPLAY WK-DESCRIP          LINE 17 POSITION 13 REVERSE.
           DISPLAY CHEQUE-PAG1         LINE 17 POSITION 68
           DISPLAY CHEQUE-PAG          LINE 17 POSITION 75
           MOVE    TIPO-EG-PAG         TO    WK-TIPOE WI-Z5P.
           MOVE    TIPO-EG-PAG         TO    EGRESOS-CLAVE.
           PERFORM I-LECTURA-TIPEGRES  THRU  F-LECTURA-TIPEGRES.
           DISPLAY EGR-DESCRIP LINE 3  POSITION 19 SIZE 32 REVERSE.
           DISPLAY  WK-TIPOE             LINE  03  POSITION 13  REVERSE.
           MOVE    NIT-PAGO            TO WK-NIT WI-Z9  CODIM.
           DISPLAY WI-Z9                LINE 04 POSITION 13 REVERSE.
           OPEN  INPUT  CNNITCED
           PERFORM I-LEE-CNNITCED  THRU  F-LEE-CNNITCED.
           DISPLAY NOMBRE-PAG LINE 4  POSITION 25 SIZE 26 REVERSE.
           CLOSE CNNITCED
           MOVE    NIT-CONTABLE-PAG    TO WK-NIT-CON  WI-Z9  CODIM.
           DISPLAY WI-Z9                LINE 20 POSITION 17.
           OPEN  INPUT  CNNITCED
           PERFORM I-LEE-CNNITCED      THRU  F-LEE-CNNITCED.
           DISPLAY NOMBM         LINE 20 POSITION 30 SIZE 30.
           MOVE  CONTROL-PAG       TO WK-CONTROL.
           DISPLAY  CONTROL-PAG  LINE 19 POSITION 48 REVERSE.
           CLOSE   CNNITCED.
           MOVE AA-PAG             TO AA-COMP-DOCTRA.
           MOVE MM-PAG             TO MM-COMP-DOCTRA.
           MOVE DD-PAG             TO DD-COMP-DOCTRA.
           DISPLAY AA-PAG        LINE 05 POSITION 13 REVERSE.
           DISPLAY MM-PAG        LINE 05 POSITION 16 REVERSE.
           DISPLAY DD-PAG        LINE 05 POSITION 19 REVERSE.
           MOVE    COD-BANCO       TO W-BANCO  BANC-CLAVE.
           PERFORM I-LEE-BANCOS  THRU F-LEE-BANCOS.
           DISPLAY BANC-NOMBRE   LINE 6  POSITION 24 REVERSE SIZE 27.
           DISPLAY W-BANCO       LINE 06 POSITION 13 REVERSE.
           MOVE    ZEROS           TO WI-Z99.
           MOVE    BASE-PAG        TO WI-Z99   WK-VRBAS.
           DISPLAY WI-Z99        LINE 06 POSITION 66 REVERSE.
           MOVE    ZEROS           TO WI-Z99.
           DISPLAY  CONDIC-PAG   LINE 20 POSITION 75 REVERSE.
           MOVE    INDBASE         TO WK-RET
           MOVE    PORC-CTA        TO WK-RETP.
           DISPLAY WK-RET        LINE 09 POSITION 75 REVERSE
           DISPLAY WK-RETP       LINE 10 POSITION 75 REVERSE.
           DISPLAY CONCEPTO1-PAG LINE 18 POSITION 13 REVERSE.
           DISPLAY CONCEPTO2-PAG LINE 19 POSITION 13 REVERSE.
           DISPLAY TIME-PAG      LINE 22 POSITION 70 REVERSE.
       F-MOSTRAR-CAB.
      *
       I-IMPRIMIR-CTA.
           MOVE TAB-MAYCTA(K)   TO MAYMAE.
           MOVE TAB-SUBCTA(K)   TO SUBMAE
           MOVE TAB-AUXCTA(K)   TO AUXMAE.
           READ CNCATCTA WITH NO LOCK INVALID KEY
               DISPLAY "AUXILIAR NO EXISTE"
               LINE 24 POSITION 01 REVERSE.
           IF INDBASE = "S"
           MOVE    INDBASE            TO WK-RET
           MOVE   PORC-CTA            TO WK-RETP.
           DISPLAY WK-RET              LINE 09 POSITION 75 REVERSE
           DISPLAY WK-RETP             LINE 10 POSITION 75 REVERSE.
       F-IMPRIMIR-CTA.
      *
       I-IMPRIMIR-BANCO.
           MOVE  ZEROS   TO  W-TOTAL.
           MOVE MAYCTA-BC(1)     TO TAB-MAYCTA(6)  CTA-MAYOR(6).
           MOVE SUBCTA-BC(1)     TO TAB-SUBCTA(6)  CTA-SUB(6).
           MOVE AUXCTA-BC(1)     TO TAB-AUXCTA(6)  CTA-AUX(6).
           MOVE     2            TO TAB-SIGNO(6)   CTA-SIGNO(6).
           COMPUTE  W-TOTAL       =  WK-VRBAS - VR-APLICA.
           MOVE     W-TOTAL      TO TAB-VALOR(6).
           DISPLAY TAB-MAYCTA(6)  LINE 14 POSITION 03 REVERSE
           DISPLAY TAB-SUBCTA(6)  LINE 14 POSITION 09 REVERSE
           DISPLAY TAB-AUXCTA(6)  LINE 14 POSITION 12 REVERSE
           DISPLAY TAB-SIGNO(6)   LINE 14 POSITION 56 REVERSE.
           MOVE    W-TOTAL          TO WI-Z99.
           DISPLAY WI-Z99         LINE 14 POSITION 38 REVERSE.
           DISPLAY  BANC-NOMBRE   LINE 14 POSITION 19 REVERSE  SIZE 16.
       F-IMPRIMIR-BANCO.
      *
      *****************************************************************
      *    CAPTURA DE CAMPOS MODULO PRINCIPAL                         *
      *****************************************************************
        I-EMPRESA.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY WK-EMPRE          LINE 02 POSITION 30 REVERSE
           ACCEPT  WK-EMPRE          LINE 02 POSITION 30
                                       UPDATE PROMPT REVERSE LOW
                                       NO BEEP
                                       ON EXCEPTION WK-EX
                                       PERFORM 999-EXCEPTION.
           DISPLAY WK-EMPRE        LINE 02 POSITION 30 REVERSE
           IF  F5  GO I-EMPRESA.
           IF  CUP OR ESC GO F-EMPRESA.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  WK-EMPRE = ZEROS   GO I-EMPRESA.
           PERFORM I-LEE-EMPRESA THRU F-LEE-EMPRESA.
           IF  SW-1  =  1 GO I-EMPRESA.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
        F-EMPRESA.
      *
        I-TIPCOM.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY WK-CHEQUEORD        LINE 02 POSITION 13 REVERSE
           ACCEPT  WK-CHEQUEORD        LINE 02 POSITION 13
                                       UPDATE PROMPT REVERSE LOW
                                       NO BEEP
                                       ON EXCEPTION WK-EX
                                       PERFORM 999-EXCEPTION.
           DISPLAY WK-CHEQUEORD        LINE 02 POSITION 13 REVERSE
           IF  F5  GO I-TIPCOM.
           IF  CUP OR ESC GO F-TIPCOM.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  WK-CHEQUEORD = ZEROS   GO I-TIPCOM.
           PERFORM I-LECTURA-RECPAG THRU F-LECTURA-RECPAG.
           IF CON-ANO NOT = 95
           IF FLAG-PAG NOT = FLAG-TRABAJO AND
              SW-LECTURA = 0
              DISPLAY "REGISTRO NO ES DE ESTE MODULO"
                LINE 24 POSITION 1
                GO I-TIPCOM.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
           IF SW-LECTURA = 0 AND SW-ADIC = 1
                       GO F-TIPCOM.
           IF SW-LECTURA = 1 AND SW-ADIC = 2
                       PERFORM  I-NEXISTE THRU F-NEXISTE
                       IF W-OPCION = "R"
                        GO TO I-TIPCOM.
        F-TIPCOM.
      *
        I-TIPEGRESOS.
           PERFORM I-DISPLAY-HELP      THRU   F-DISPLAY-HELP.
           MOVE SPACES                 TO  EGR-DESCRIP.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY WK-TIPOE            LINE 03 POSITION 13 REVERSE

           ACCEPT  WK-TIPOE            LINE 03 POSITION 13
                                       UPDATE PROMPT REVERSE LOW
                                       NO BEEP
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY WK-TIPOE            LINE 03 POSITION 13 REVERSE.
           IF  F5  GO I-TIPEGRESOS.
           IF  CUP OR ESC GO F-TIPEGRESOS.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  F1  PERFORM
                  I-HELP-TIPEGRES THRU F-HELP-TIPEGRES
                  PERFORM  I-WIN-CTAS   THRU  F-WIN-CTAS
                  GO      I-TIPEGRESOS.
           MOVE WK-TIPOE        TO EGRESOS-CLAVE.
           IF CONS-EGR   =   ZEROS   GO I-TIPEGRESOS.
           PERFORM I-LECTURA-TIPEGRES THRU F-LECTURA-TIPEGRES.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 80.
           DISPLAY EGR-DESCRIP LINE 03 POSITION 19 REVERSE SIZE 32.
           IF SW-LECTURA = 1
                       PERFORM  I-NEXISTE THRU F-NEXISTE
                       GO TO I-TIPEGRESOS.
            PERFORM I-TABLA-CTAEG  THRU  F-TABLA-CTAEG.
        F-TIPEGRESOS.
      *
       I-ANTICIPO.
            MOVE  SPACES  TO WK-ORIGEN.
            IF ANTICIPO-EGR   =    ZEROS   GO F-ANTICIPO.
            IF ANTICIPO-EGR = 4
                PERFORM I-CNPRIMAS  THRU  F-CNPRIMAS.
             IF ANTICIPO-EGR = 3
                 PERFORM I-CNVACACI  THRU  F-CNVACACI.
            IF ANTICIPO-EGR = 2
                 PERFORM I-CNCESANT  THRU  F-CNCESANT.
             IF ANTICIPO-EGR = 1
                 PERFORM I-CNSUELDO  THRU  F-CNSUELDO.
             MOVE  ZEROS  TO  W-RFTE.
             IF ANTICIPO-EGR = 5
                 PERFORM I-CNSUELDO   THRU   F-CNSUELDO
                 MOVE W-SUBTOT          TO   TAB-VALOR(1) PAG-VAL(1)
                 COMPUTE W-RFTE  =  W-RFTE + W-SUBTOT
                 PERFORM I-CNCESANT   THRU   F-CNCESANT
                 MOVE    W-SUBTOT       TO   TAB-VALOR(2) PAG-VAL(2)
                 COMPUTE W-RFTE  =  W-RFTE + W-SUBTOT
                 PERFORM I-CNVACACI   THRU   F-CNVACACI
                 MOVE    W-SUBTOT       TO   TAB-VALOR(3) PAG-VAL(3)
                 COMPUTE W-RFTE =   W-RFTE + W-SUBTOT
                 PERFORM I-CNPRIMAS   THRU   F-CNPRIMAS
                 MOVE    W-SUBTOT       TO   TAB-VALOR(4) PAG-VAL(4)
                 COMPUTE W-RFTE  =  W-RFTE + W-SUBTOT
                 MOVE    W-OPCION       TO   WK-ORIGEN
                 MOVE    "L"            TO   W-OPCION
                 MOVE    SPACES         TO   RES-OK
                 MOVE    W-RFTE         TO   PAG-VAL(6) TAB-VALOR(6)
                 MOVE    W-RFTE         TO   W-SUBTOT WK-VRBAS
                 MOVE    2              TO   TAB-SIGNO(6)
                 PERFORM I-WIN-CTAS   THRU   F-WIN-CTAS
                 GO F-ANTICIPO.
            PERFORM  I-WIN-CTAS  THRU   F-WIN-CTAS
            IF W-OPCION NOT = "A"
            PERFORM  I-PANTA-7REG     THRU F-PANTA-7REG.
       F-ANTICIPO.
      *
       I-FECHA-DESDE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY AA-COMP-DOCTRA    LINE 05 POSITION 13
           DISPLAY MM-COMP-DOCTRA    LINE 05 POSITION 16
           ACCEPT  DD-COMP-DOCTRA    LINE 05 POSITION 19
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO F-FECHA-DESDE.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           PERFORM I-MASC-NAC THRU F-MASC-NAC.
           IF DD-COMP-DOCTRA   =  ZEROS    GO  I-FECHA-DESDE.
           IF SW-ERROR = 1
              DISPLAY "FECHA ERRADA   "
              LINE 24 POSITION 01 REVERSE
              GO I-FECHA-DESDE.
           DISPLAY AA-COMP-DOCTRA    LINE 05 POSITION 13 REVERSE.
           DISPLAY MM-COMP-DOCTRA    LINE 05  POSITION 16 REVERSE.
           DISPLAY DD-COMP-DOCTRA    LINE 05 POSITION 19 REVERSE.
           DISPLAY SPACE LINE 24 POSITION 01 SIZE 22.
       F-FECHA-DESDE.
      *
       I-NIT.
           MOVE ZEROS  TO    SW-EOF.
           DISPLAY "N.I.T/CEDULA: " LINE 24 POSITION 60.
           MOVE WK-NIT        TO       WI-Z9.
           MOVE       ZEROS            TO    WK-EXCEPTION.
           DISPLAY WI-Z9               LINE 4 POSITION 13  REVERSE
           ACCEPT WK-NIT               LINE 4 POSITION 13
                   UPDATE PROMPT NO BEEP  REVERSE HIGH
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO F-NIT.
           IF  F1  PERFORM I-AYUDA-NIT   THRU F-AYUDA-NIT
               GO I-NIT.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  WK-NIT    < 1000
               DISPLAY "N.I.T/CEDULA no puede ser < 1000 "
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-NIT.
           IF  WK-NIT     = ZEROS
               DISPLAY "N.I.T/CEDULA no puede ser Ceros"
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-NIT.
           OPEN INPUT CNNITCED.
           MOVE WK-NIT          TO WI-Z9  CODIM
           DISPLAY WI-Z9      LINE  4   POSITION 13 REVERSE.
           PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
           CLOSE CNNITCED.
           IF ANTICIPO-EGR = ZEROS     GO NIT-2.
           IF ANTICIPO-EGR NOT = ZEROS
              PERFORM I-LEE-CONTRATO   THRU F-LEE-CONTRATO.
           IF SW-EOF = 2  OR  SW-EOF = 3
               DISPLAY
             "NIT NO EXISTE EN ESTA COMPANIA - NO PUEDE GIRAR CHEQUE"
               LINE 24 POSITION    01  REVERSE  BLINK
              GO I-NIT.
       NIT-2.
           IF SW-EOF  = 1
      
              PERFORM I-LEE-HOJAVIDA   THRU F-LEE-HOJAVIDA
              IF SW-EOF = 2
                 DISPLAY "N.I.T  -  INEXISTENTE  -                   "
                 LINE 24 POSITION    01  REVERSE
    
                 GO I-NIT.
           DISPLAY NOMBM LINE 04 POSITION 25 REVERSE SIZE 26.
           DISPLAY SPACES LINE 24 POSITION 1 SIZE 80.
           PERFORM I-VERIF-BENEFICIARIO   THRU  F-VERIF-BENEFICIARIO.
           MOVE   NOMBM     TO      NOMBRE-PAG.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 80.
           MOVE WK-NIT      TO  WK-NIT-CON.
           PERFORM I-WIN-CTAS   THRU F-WIN-CTAS.
       F-NIT.
      *
       I-VERIF-BENEFICIARIO.
           DISPLAY "BENEFICIARIO CORRECTO S/N"  LINE 24 POSITION 01
                    BLINK  BEEP.
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO NOT = "N"
                   GO F-VERIF-BENEFICIARIO.
           MOVE SPACES    TO  NOMBM.
           MOVE       ZEROS            TO    WK-EXCEPTION.
           ACCEPT  NOMBM LINE 24 POSITION 44
                   PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY NOMBM LINE 24 POSITION 44.
           DISPLAY NOMBM LINE 04 POSITION 25 REVERSE SIZE 26.
           IF  NOMBM  =  SPACES  GO  I-VERIF-BENEFICIARIO.
       F-VERIF-BENEFICIARIO.
      *
       I-NIT-CONTABLE.
           DISPLAY NOMBM LINE 20 POSITION 30 SIZE 30.
           MOVE ZEROS  TO    SW-EOF.
           MOVE WK-NIT-CON    TO       WI-Z9.
           MOVE       ZEROS            TO    WK-EXCEPTION.
           DISPLAY WI-Z9               LINE 20 POSITION 17
           MOVE       ZEROS            TO    WK-EXCEPTION.
           ACCEPT WK-NIT-CON           LINE 20 POSITION 17
                   UPDATE PROMPT NO BEEP  HIGH
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO F-NIT-CONTABLE.
           IF  WK-NIT-CON    < 1000
               DISPLAY "N.I.T/CEDULA no puede ser < 1000 "
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-NIT-CONTABLE.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  F1  PERFORM I-AYUDA-NIT   THRU F-AYUDA-NIT
               GO I-NIT-CONTABLE.
           IF  WK-NIT-CON = ZEROS
               DISPLAY "N.I.T/CEDULA no puede ser Ceros"
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-NIT-CONTABLE.
           OPEN INPUT CNNITCED.
           MOVE WK-NIT-CON      TO WI-Z9  CODIM
           DISPLAY WI-Z9      LINE  20  POSITION 17.
           PERFORM I-LEE-CNNITCED THRU F-LEE-CNNITCED
           CLOSE CNNITCED.
           IF ANTICIPO-EGR = ZEROS     GO NIT-2-CONTABLE.
           IF ANTICIPO-EGR NOT = ZEROS
              PERFORM I-LEE-CONTRATO   THRU F-LEE-CONTRATO.
           IF SW-EOF = 2  OR  SW-EOF = 3
               DISPLAY
             "NIT NO EXISTE EN ESTA COMPANIA - NO PUEDE GIRAR CHEQUE"
               LINE 24 POSITION    01  REVERSE  BLINK
              GO I-NIT-CONTABLE.
       NIT-2-CONTABLE.
           IF SW-EOF  = 1
      
              PERFORM I-LEE-HOJAVIDA   THRU F-LEE-HOJAVIDA
              IF SW-EOF = 2
              DISPLAY "N.I.T  -  INEXISTENTE  -     "
              LINE 24 POSITION    01  REVERSE
   
              GO I-NIT-CONTABLE.
           DISPLAY NOMBM  LINE 20 POSITION 30 SIZE 30.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 80.
       F-NIT-CONTABLE.
      *
       I-CONTROL.
           IF  TAB-MAYCTA(1) NOT = 2205
               MOVE  SPACES  TO WK-CONTROL
               GO F-CONTROL.
           IF  TAB-SUBCTA(1) NOT  = 10
               GO F-CONTROL.
           DISPLAY "CODIGO PROVEEDOR      "   LINE 24 POSITION 1.
           MOVE        WK-CONTROL      TO       WI-Z5P
           MOVE       ZEROS            TO    WK-EXCEPTION.
           DISPLAY WK-CONTROL          LINE 19 POSITION 48  REVERSE
           ACCEPT  WK-CONTROL          LINE 19 POSITION 48
                   UPDATE PROMPT NO BEEP  REVERSE HIGH
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO F-CONTROL.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  WK-CONTROL = SPACES OR WK-CONTROL = ZEROS
               DISPLAY "CODIGO proveedor no puede ser Ceros"
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-CONTROL.
            MOVE CTA-MAYOR(1) TO MAYMAE.
            MOVE CTA-SUB(1)   TO SUBMAE
            MOVE WK-CONTROL   TO AUXMAE.
            READ CNCATCTA WITH NO LOCK INVALID KEY
                DISPLAY "AUXILIAR NO EXISTE"
                LINE 24 POSITION 01 REVERSE
                GO I-CONTROL.
            DISPLAY NOMCTA  LINE 19 POSITION 53 SIZE 27.
            DISPLAY SPACES  LINE 24 POSITION 01 SIZE 80.
       F-CONTROL.
      *
       I-BANCOS.
           MOVE       ZEROS             TO   WK-EXCEPTION.
           PERFORM I-DISPLAY-HELP  THRU  F-DISPLAY-HELP.
           MOVE W-BANCO TO WI-Z12C.
           DISPLAY WI-Z2               LINE 6 POSITION 13 REVERSE
           ACCEPT W-BANCO              LINE 6 POSITION 13
                   UPDATE PROMPT NO BEEP REVERSE HIGH
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  CUP OR ESC GO F-BANCOS.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  F1  PERFORM
                  I-HELP-BANCOS  THRU F-HELP-BANCOS.
           IF  W-BANCO    = ZEROS
               DISPLAY "Codigo del Banco no puede ser Ceros"
               LINE 24 POSITION 01 REVERSE BEEP
               GO I-BANCOS.
           MOVE W-BANCO         TO WI-Z2   BANC-CLAVE.
           PERFORM I-LEE-BANCOS THRU F-LEE-BANCOS.
           IF SW-EOF = 1
              DISPLAY "BANCO - INEXISTENTE  -  INTENTE NUEVAMENTE :"
              LINE 24 POSITION    01  REVERSE
              GO  I-BANCOS.
           DISPLAY BANC-NOMBRE  LINE 06 POSITION 24 REVERSE SIZE 27.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 80.
       F-BANCOS.
      *
       I-CREA-NIT.
           ACCEPT  CREA-NIT LINE 24 POSITION 51.
           IF   CREA-NIT = "S" OR CREA-NIT = "N" NEXT SENTENCE
           ELSE GO I-CREA-NIT.
           IF CREA-NIT = "N" GO F-CREA-NIT.
           PERFORM I-WIN-NIT THRU F-WIN-NIT.
         I-ACNIT.
           MOVE     ZEROS      TO   WK-EXCEPTION.
           ACCEPT  CODIM LINE 22 POSITION 44
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY CODIM LINE 22 POSITION 44.
           IF      CUP   GO I-BLQPAN.
           IF      F2    GO I-BLQPAN.
           IF      CODIM = ZEROS GO I-ACNIT.
         I-ACNOM.
           ACCEPT  NOMBM LINE 23 POSITION 44
                   PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY NOMBM LINE 23 POSITION 44.
           IF      CUP   GO I-ACNIT.
           IF      F2    GO I-BLQPAN.
           IF      NOMBM =  SPACES GO I-ACNOM.
           DISPLAY " GRABA NIT  S/N : " LINE 24 POSITION 50 REVERSE.
         I-ACGRB.
           ACCEPT  RES-OK  LINE 24 POSITION 68
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF      CUP GO  I-ACNOM.
           IF      RES-OK  =  "S"  OR RES-OK = "N" NEXT SENTENCE
           ELSE    GO  I-ACGRB.
           IF      RES-OK  =  "N"  GO I-BLQPAN.
           MOVE    SPACES  TO  DIREM.
           MOVE    ZEROS   TO  CIUDM.
           MOVE    ZEROS   TO  TELEM.
           OPEN    I-O   CNNITCED.
           WRITE   REGMA1.
           CLOSE   CNNITCED.
       I-BLQPAN.
           DISPLAY SPACES LINE 21 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 22 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 23 POSITION 01 SIZE 80.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 80.
           PERFORM I-WIN-CTAS         THRU F-WIN-CTAS.
       F-BLQPAN.
      *
       F-CREA-NIT.
      *
       I-WIN-NIT.
           DISPLAY "ษอออออออออัอออออออออออออออออออออออออออออออออออออป"
                       LINE 21 POSITION 32 CONTROL SG-CONTROL-STRING
           DISPLAY "บ NIT/CED ณ                                     บ"
                                                 LINE 22 POSITION 32
           DISPLAY "บ Nombre  ณ                                     บ"
                                                 LINE 23 POSITION 32
           DISPLAY "ศอออออออออฯอออออออออออออออออออออออออออออออออออออผ"
                                                 LINE 24 POSITION 32.
       F-WIN-NIT.
      *
       I-CONCEPTO.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY WK-DESCRIP     LINE 17 POSITION 13 REVERSE
           ACCEPT  WK-DESCRIP     LINE 17 POSITION 13
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY WK-DESCRIP         LINE 17 POSITION 13 REVERSE.
           IF  CUP OR ESC GO F-CONCEPTO.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  WK-DESCRIP    = SPACE
               DISPLAY "DESCRIPCION - ERRADA -"
               LINE 24 POSITION 01 REVERSE
               GO I-CONCEPTO.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-CONCEPTO.
      *
       I-CONCEPTO1.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY WK-DESCRIP1    LINE 18 POSITION 13  REVERSE.
           ACCEPT  WK-DESCRIP1    LINE 18 POSITION 13
                   UPDATE PROMPT NO BEEP LOW REVERSE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY WK-DESCRIP1        LINE 18 POSITION 13 REVERSE.
           IF  CUP OR ESC GO F-CONCEPTO1.
           IF  F2  PERFORM I-FIN THRU F-FIN.
       F-CONCEPTO1.
      *
       I-CONCEPTO2.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY WK-DESCRIP2    LINE 19 POSITION 13 REVERSE
           ACCEPT  WK-DESCRIP2    LINE 19 POSITION 13
                   UPDATE PROMPT NO BEEP LOW  REVERSE
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           DISPLAY WK-DESCRIP2        LINE 19 POSITION 13 REVERSE.
           IF  CUP OR ESC GO F-CONCEPTO2.
           IF  F2  PERFORM I-FIN THRU F-FIN.
       F-CONCEPTO2.
      *
       I-ESPECIAL.
           PERFORM I-AYUDA-ESPECIAL  THRU F-AYUDA-ESPECIAL.
           DISPLAY "<< F8 >>  CTA.ESPECIAL :  << F9 >> CTA NORMAL"
                  LINE  17  POSITION 13 BEEP BLINK.
           MOVE  ZEROS  TO  Z.
           MOVE  ZEROS  TO  WK-EXCEPTION
           ACCEPT NUM-REG    LINE 17 POSITION 60
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F8     MOVE  1 TO Z.
           PERFORM I-BLQPAN     THRU F-BLQPAN.
           PERFORM I-WIN-CTAS   THRU F-WIN-CTAS.
           IF  CUP OR ESC GO F-ESPECIAL.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  F8  GO F-ESPECIAL.
           IF  F9  MOVE ZEROS  TO Z
                   GO F-ESPECIAL.
           GO I-ESPECIAL.
        F-ESPECIAL.
      *
       I-VRFACT1.
           MOVE ZEROS             TO  WK-EXCEPTION.
           MOVE    ZEROS          TO  WI-Z99.
           MOVE WK-VFACT1         TO  WI-Z99.
           DISPLAY WI-Z99              LINE 3 POSITION 66 REVERSE
           ACCEPT WK-VFACT1        LINE 3 POSITION 66
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE WK-VFACT1      TO  WI-Z99.
           DISPLAY WI-Z99    LINE 3 POSITION 66  REVERSE.
           IF  CUP OR ESC GO F-VRFACT1.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  WK-VFACT1  = ZEROS
                           GO  I-VRFACT1.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-VRFACT1.
      *
       I-VRFACT2.
           MOVE ZEROS             TO  WK-EXCEPTION.
           MOVE    ZEROS          TO  WI-Z99.
           MOVE WK-VFACT2         TO  WI-Z99.
           DISPLAY WI-Z99              LINE 4 POSITION 66 REVERSE
           ACCEPT WK-VFACT2        LINE 4 POSITION 66
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE WK-VFACT2      TO  WI-Z99.
           DISPLAY WI-Z99    LINE 4 POSITION 66  REVERSE
           IF  CUP OR ESC GO F-VRFACT2.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  WK-VFACT2  NOT = ZEROS
                    MOVE   1  TO  SW-1.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-VRFACT2.
      *
       I-VRFACT3.
           MOVE ZEROS             TO  WK-EXCEPTION.
           MOVE    ZEROS          TO  WI-Z99.
           MOVE WK-VFACT3         TO  WI-Z99.
           DISPLAY WI-Z99              LINE 5 POSITION 66 REVERSE
           ACCEPT WK-VFACT3        LINE 5 POSITION 66
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE WK-VFACT3      TO  WI-Z99.
           DISPLAY WI-Z99    LINE 5 POSITION 66  REVERSE
           IF  CUP OR ESC GO F-VRFACT3.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  WK-VFACT3  NOT = ZEROS
                    MOVE   1  TO  SW-1.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-VRFACT3.
      *
       I-VRFACT4.
           MOVE ZEROS             TO  WK-EXCEPTION.
           MOVE    ZEROS          TO  WI-Z99.
           DISPLAY "VALOR 4:" LINE 22    POSITION 58.
           MOVE WK-VFACT4         TO  WI-Z99.
           DISPLAY WI-Z99              LINE 22 POSITION 66 REVERSE
           ACCEPT WK-VFACT4        LINE 22 POSITION 66
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE WK-VFACT4      TO  WI-Z99.
           DISPLAY WI-Z99    LINE 22 POSITION 66  REVERSE
           IF  CUP OR ESC GO F-VRFACT4.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  WK-VFACT4  NOT = ZEROS
                    MOVE   1  TO  SW-1.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-VRFACT4.
      *
       I-VRFACT5.
           MOVE ZEROS             TO  WK-EXCEPTION.
           MOVE    ZEROS          TO  WI-Z99.
           DISPLAY "VALOR 5:" LINE 23    POSITION 58.
           MOVE WK-VFACT5         TO  WI-Z99.
           DISPLAY WI-Z99              LINE 23 POSITION 66 REVERSE
           ACCEPT WK-VFACT5        LINE 23 POSITION 66
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE WK-VFACT5      TO  WI-Z99.
           DISPLAY WI-Z99    LINE 23 POSITION 66  REVERSE
           IF  CUP OR ESC GO F-VRFACT5.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  WK-VFACT5  NOT = ZEROS
                    MOVE   1  TO  SW-1.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-VRFACT5.
      *
       I-ACEPTA-FACTURAS.
       FACT-1.
           MOVE  15    TO   I
           MOVE  1     TO   JJ
           DISPLAY JJ  LINE I    POSITION 29 REVERSE.
           PERFORM I-FACTUR    THRU F-FACTUR.
           IF ESC     GO  FACT-10.
           PERFORM I-VRFACT    THRU  F-VRFACT.
       FACT-2.
           MOVE  16    TO   I
           MOVE  2     TO   JJ
           DISPLAY JJ  LINE I    POSITION 29 REVERSE.
           PERFORM I-FACTUR    THRU F-FACTUR.
           IF CUP     GO  FACT-1.
           IF ESC     GO  FACT-10.
           PERFORM I-VRFACT    THRU  F-VRFACT.
           IF CUP     GO  FACT-1.
       FACT-3.
           MOVE  17    TO   I
           MOVE  3     TO   JJ
           DISPLAY JJ  LINE I    POSITION 29 REVERSE.
           PERFORM I-FACTUR    THRU F-FACTUR.
           IF CUP     GO  FACT-2.
           IF ESC     GO  FACT-10.
           PERFORM I-VRFACT    THRU  F-VRFACT.
           IF CUP     GO  FACT-2.
       FACT-4.
           MOVE  18    TO   I
           MOVE  4     TO   JJ
           DISPLAY JJ  LINE I    POSITION 29 REVERSE.
           PERFORM I-FACTUR    THRU F-FACTUR.
           IF CUP     GO  FACT-3.
           IF ESC     GO  FACT-10.
           PERFORM I-VRFACT    THRU  F-VRFACT.
           IF CUP     GO  FACT-3.
       FACT-5.
           MOVE  19    TO   I
           MOVE  5     TO   JJ
           DISPLAY JJ  LINE I    POSITION 29 REVERSE.
           PERFORM I-FACTUR    THRU F-FACTUR.
           IF CUP     GO  FACT-4.
           IF ESC     GO  FACT-10.
           PERFORM I-VRFACT    THRU  F-VRFACT.
           IF CUP     GO  FACT-4.
       FACT-6.
           MOVE  20    TO   I
           MOVE  6     TO   JJ
           DISPLAY JJ  LINE I    POSITION 29 REVERSE.
           PERFORM I-FACTUR    THRU F-FACTUR.
           IF CUP     GO  FACT-5.
           IF ESC     GO  FACT-10.
           PERFORM I-VRFACT    THRU  F-VRFACT.
           IF CUP     GO  FACT-5.
       FACT-7.
           MOVE  21    TO   I
           MOVE  7     TO   JJ
           DISPLAY JJ  LINE I    POSITION 29 REVERSE.
           PERFORM I-FACTUR    THRU F-FACTUR.
           IF CUP     GO  FACT-6.
           IF ESC     GO  FACT-10.
           PERFORM I-VRFACT    THRU  F-VRFACT.
           IF CUP     GO  FACT-6.
       FACT-8.
           MOVE  22    TO   I
           MOVE  8     TO   JJ
           DISPLAY JJ  LINE I    POSITION 29 REVERSE.
           PERFORM I-FACTUR    THRU F-FACTUR.
           IF CUP     GO  FACT-8.
           IF ESC     GO  FACT-10.
           PERFORM I-VRFACT    THRU  F-VRFACT.
           IF CUP     GO  FACT-7.
       FACT-10.
           MOVE ZEROS  TO  JJ.
           MOVE ZEROS  TO WK-VRBAS.
           PERFORM I-BASE  THRU F-BASE VARYING JJ FROM 1 BY 1
                          UNTIL  JJ  >  8.
           MOVE WK-VRBAS TO  WI-Z99.
           DISPLAY   WI-Z99   LINE 6  POSITION 66  REVERSE.
           PERFORM I-WIN-CTAS  THRU   F-WIN-CTAS.
           PERFORM I-BLQPAN    THRU   F-BLQPAN.
       F-ACEPTA-FACTURAS.
      *
       I-BASE.
           COMPUTE  WK-VRBAS = WK-VRBAS + WK-VFACT(JJ).
       F-BASE.
       I-FACTUR.
           MOVE ZEROS  TO  WK-EXCEPTION
           MOVE ZEROS  TO  ZZ-8.
           MOVE NUMFRA(JJ) TO ZZ-8
           DISPLAY ZZ-8   LINE I  POSITION 35 REVERSE
           ACCEPT NUMFRA(JJ)  LINE I  POSITION 35
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE NUMFRA(JJ) TO ZZ-8.
           DISPLAY ZZ-8   LINE I  POSITION 35 REVERSE
           IF  CUP OR ESC GO F-FACTUR.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF NUMFRA(JJ)  =  ZEROS GO I-FACTUR.
        F-FACTUR.
      *
       I-VRFACT.
           MOVE ZEROS             TO  WK-EXCEPTION.
           MOVE    ZEROS          TO  WI-Z99.
           MOVE WK-VFACT(JJ)      TO  WI-Z99.
           DISPLAY WI-Z99         LINE I  POSITION 49 REVERSE
           ACCEPT WK-VFACT(JJ)    LINE I  POSITION 49
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE WK-VFACT(JJ)      TO  WI-Z99.
           DISPLAY WI-Z99         LINE I  POSITION 49  REVERSE
           IF  ESC OR CUP GO F-VRFACT.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF WK-VFACT(JJ) =  ZEROS   GO I-VRFACT.
           COMPUTE  WK-VRBAS = WK-VRBAS + WK-VFACT(JJ).
           MOVE WK-VRBAS TO  WI-Z99.
           DISPLAY   WI-Z99   LINE 6  POSITION 66  REVERSE.
       F-VRFACT.
      *
       I-VRBASE.
           IF W-SUBTOT NOT = ZEROS
                 MOVE W-SUBTOT    TO WK-VRBAS
                 MOVE WK-VRBAS             TO  WI-Z99
                 DISPLAY WI-Z99       LINE 6 POSITION 66 REVERSE
                 GO F-VRBASE.
           MOVE ZEROS                  TO  WK-EXCEPTION.
           COMPUTE WK-VRBAS = WK-VFACT1 + WK-VFACT2 + WK-VFACT3 +
                              WK-VFACT4 + WK-VFACT5.
           MOVE WK-VRBAS             TO  WI-Z99.
           DISPLAY WI-Z99               LINE 6 POSITION 66 REVERSE
           ACCEPT WK-VRBAS           LINE 6 POSITION 66
                   UPDATE PROMPT NO BEEP REVERSE LOW
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE WK-VRBAS             TO  WI-Z99.
           DISPLAY WI-Z99               LINE 6 POSITION 66 REVERSE
           IF  CUP GO F-VRBASE.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  WK-VRBAS    = ZEROS
               DISPLAY "BASE  NO debe ser  0  "
               LINE 24 POSITION 01 REVERSE
               GO I-VRBASE.
           DISPLAY SPACES LINE 24 POSITION 01 SIZE 22.
       F-VRBASE.
      *
       I-GRABACION.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " PRESIONE <A> ADICIONAR "   LINE 24 POSITION 1
           DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 22
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  F1                 GO F-GRABACION.
           IF  F2                 PERFORM I-FIN THRU F-FIN.
           IF  W-TRUCO NOT = "A"  GO TO I-GRABACION.
           MOVE ZEROS TO X NUM-REG.
           PERFORM I-ACTUALIZA-CONS THRU F-ACTUALIZA-CONS.
           MOVE W-ULTPRO          TO CHEQUEORD.
           MOVE W-ULTPRO          TO SOLBA-CLAVE.
           DISPLAY  W-ULTPRO      LINE 02 POSITION 13 REVERSE BEEP
           PERFORM I-MVTO-CAMPOS-GRABAR THRU F-MVTO-CAMPOS-GRABAR.
           MOVE  ZEROS        TO  CONDIC-PAG.
           DISPLAY "GRABANDO... ESPERE UN MOMENTO POR FAVOR"
                LINE 24 POSITION 1 REVERSE
           CLOSE CNRECPAG
           OPEN  I-O   CNRECPAG.
           WRITE       RECPAG-COMP.
           CLOSE       CNRECPAG.
           OPEN INPUT CNRECPAG.
           PERFORM ACEPTA-TRUCO.
           MOVE    "A"   TO  W-TRUCO.
           COMPUTE W-ULTPRO  =   W-ULTPRO  +  1.
         F-GRABACION.

         I-LEE-EMPRESAS.
               READ EMPRESAS WITH NO LOCK INVALID KEY
               DISPLAY "ERROR LEYENDO ARCHIVO EMPRESAS"
                   LINE 23 POSITION 15
                   PERFORM I-FIN-MODE THRU F-FIN-MODE.
         F-LEE-EMPRESAS.
      ***************************************************************
      *            I-ACTUALIZA-CONSECUTIVO-SOLICITUD DE CHEQUES.    *
      ***************************************************************
       I-ACTUALIZA-CONS.
           MOVE 03 TO W-COMP
           PERFORM I-BUSCA-CONSECUTIVO THRU   F-BUSCA-CONSECUTIVO
           MOVE    WR-RADIC-CONTABLE     TO   W-ULTPRO.
           PERFORM I-ACT-TBCONDOC       THRU  F-ACT-TBCONDOC
           DISPLAY "COMPROBANTE DE EGRESO No. :" LINE 22 POSITION 10
           DISPLAY  W-ULTPRO      LINE 22 POSITION 39 BEEP.
       F-ACTUALIZA-CONS.
      *
       I-MVTO-CAMPOS-GRABAR.
           MOVE WK-TIPOE           TO TIPO-EG-PAG.
           MOVE WK-DESCRIP         TO CONCEPTO-PAG.
           MOVE WK-NIT             TO NIT-PAGO.
           MOVE W-BANCO            TO COD-BANCO
                                      BANCO-PAG.
           MOVE AA-COMP-DOCTRA     TO AA-PAG.
           MOVE DD-COMP-DOCTRA     TO DD-PAG.
           MOVE ANTICIPO-EGR       TO ANTICIPO-PAG.
           MOVE MM-COMP-DOCTRA     TO MM-PAG.
           MOVE WK-VRBAS           TO BASE-PAG.
           MOVE WK-VLRDOC          TO RET-PAG.
           MOVE WK-CONTROL         TO CONTROL-PAG.
           MOVE WK-DESCRIP         TO CONCEPTO-PAG.
           MOVE WK-DESCRIP1        TO CONCEPTO1-PAG.
           MOVE WK-DESCRIP2        TO CONCEPTO2-PAG.
           MOVE WK-SALARIO         TO IVA-PAG.
           MOVE TAB-VALOR(6)       TO NETO-PAG.
           MOVE WK-EMPRE           TO EMPRESA-PAG.
           MOVE WK-NIT-CON         TO NIT-CONTABLE-PAG.
           MOVE FLAG-TRABAJO       TO FLAG-PAG.
           MOVE CON-USERNAME       TO USUARIO-PAG.
           MOVE WK-FECHA-HOY       TO DATE-PAG.
           MOVE WK-HORA-HOY        TO TIME-PAG.
           PERFORM GRABA-TABLA THRU F-GRABA-TABLA
                       VARYING X FROM 1 BY 1 UNTIL X > 8.
        F-MVTO-CAMPOS-GRABAR.
      *
         GRABA-TABLA.
           IF Z = 1  MOVE 1 TO FLAG-Z.
           MOVE    NUMFRA(X)        TO NROFACT-PAG(X).
           MOVE    WK-VFACT(X)      TO VALFACT-PAG(X).
           IF X = 6
               MOVE MAYCTA-BC(1)  TO   PAG-MAYOR(X)
               MOVE SUBCTA-BC(1)  TO   PAG-SUB(X)
               MOVE AUXCTA-BC(1)  TO   PAG-AUX(X)
               MOVE     2         TO   PAG-SIG(X)
               MOVE TAB-VALOR(6)  TO   PAG-VAL(X)
                GO F-GRABA-TABLA.
           IF  TAB-MAYCTA(X)  = 2205  AND
               TAB-SUBCTA(X)  = 10
               MOVE    TAB-MAYCTA(X)    TO PAG-MAYOR(X)
               MOVE    TAB-SUBCTA(X)    TO PAG-SUB(X)
               MOVE    TAB-AUXCTA(X)    TO PAG-AUX(X)
      
           ELSE
               MOVE    TAB-MAYCTA(X)    TO PAG-MAYOR(X)
               MOVE    TAB-SUBCTA(X)    TO PAG-SUB(X)
               MOVE    TAB-AUXCTA(X)    TO PAG-AUX(X).

           IF   TAB-VALOR(X) IS NEGATIVE
                MOVE    2           TO PAG-SIG(X)
           ELSE
                MOVE  TAB-SIGNO(X)  TO PAG-SIG(X).
           MOVE    TAB-VALOR(X)     TO PAG-VAL(X).
       F-GRABA-TABLA.
      *
       I-REGRABACION.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
         I-REGRANOR.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " PRESIONE <M> MODIFICAR        " LINE 24 POSITION 1
           DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 27
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO
           
           IF  F8
               MOVE   1  TO  XX
               GO I-REGRABACION.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  ESC OR F1
               GO F-REGRABACION.
           IF  F2
               PERFORM I-FIN THRU F-FIN.
           IF  W-TRUCO                  NOT =  "M"
               GO I-REGRABACION.
           CLOSE CNRECPAG.
           OPEN   I-O CNRECPAG.
           MOVE WK-CHEQUEORD     TO   RECPAG-CLAVE.
           MOVE ZEROS            TO   BORRADOS.
           PERFORM I-LECTURA-RECPAG  THRU  F-LECTURA-RECPAG.
           PERFORM I-BORRA-REC   THRU F-BORRA-REC.
           PERFORM I-MVTO-CAMPOS-GRABAR.
           DISPLAY "GRABANDO MODIFICACIONES... ESPERE UN MOMENTO"
              LINE 24 POSITION  1 REVERSE.
           IF  XX =  1
               MOVE ZEROS TO CONDIC-PAG.
           REWRITE  RECPAG-COMP  INVALID KEY WRITE RECPAG-COMP.
           CLOSE    CNRECPAG.
           OPEN INPUT CNRECPAG.
       F-REGRABACION.
      *
       I-BORRA-REC.
           DELETE CNRECPAG.
           ADD    1 TO BORRADOS.
       F-BORRA-REC.
      *
       I-MODIFICARC.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " PRESIONE <M> MODIFICAR"    LINE 24 POSITION 1
           DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 25
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF  F1  GO F-MODIFICARC.
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  W-TRUCO NOT = "M" GO I-MODIFICARC.
           MOVE W-TRUCO  TO  RES-OK.
       F-MODIFICARC.
      *
       I-ELIMINAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " PRESIONE <R> RETIRAR "     LINE 24 POSITION 1
           DISPLAY " <F1> CANCELAR "            LINE 24 POSITION 25
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           IF   F1    CLOSE CNRECPAG
                      OPEN INPUT CNRECPAG
                      GO F-ELIMINAR.
           IF   F2    PERFORM I-FIN THRU F-FIN.
           IF  W-TRUCO NOT = "R"  GO I-ELIMINAR.
           IF  CONDIC-PAG  = 1
               DISPLAY "- OJO - CONDICION NO PERMITE RETIROS"
               LINE 24 POSITION 01 REVERSE  BLINK
               ACCEPT X LINE 24 POSITION 01
               CLOSE CNRECPAG
               OPEN INPUT CNRECPAG
               GO F-ELIMINAR.
           IF  CONDIC-PAG  = 3
               DISPLAY "- OJO - CONDICION NO PERMITE RETIROS"
               LINE 24 POSITION 01 REVERSE  BLINK
               ACCEPT X LINE 24 POSITION 01
               CLOSE CNRECPAG
               OPEN INPUT CNRECPAG
               GO F-ELIMINAR.
         I-BORRARA.
           MOVE ZEROS  TO K.
         I-LOOP-BO.
           ADD   1   TO   K.
           IF   K   >  8  GO         F-CARGA-BORRA.
           IF   K   =  6  
                MOVE    ZEROS    TO PAG-VAL(K)
                MOVE    ZEROS    TO PAG-SIG(K)
                MOVE    ZEROS    TO NROFACT-PAG(K)
                MOVE    ZEROS    TO VALFACT-PAG(K)
                GO         I-LOOP-BO.
           MOVE    ZEROS    TO PAG-MAYOR(K)
           MOVE    ZEROS    TO PAG-SUB(K)
           MOVE    ZEROS    TO PAG-AUX(K)
           MOVE    ZEROS    TO PAG-VAL(K)
           MOVE    ZEROS    TO PAG-SIG(K)
           MOVE    ZEROS    TO NROFACT-PAG(K)
           MOVE    ZEROS    TO VALFACT-PAG(K)
           GO      I-LOOP-BO.
       F-CARGA-BORRA.
           MOVE  ZEROS   TO  BASE-PAG.
           MOVE  ZEROS   TO  RET-PAG.
           MOVE  ZEROS   TO  IVA-PAG.
           MOVE  ZEROS   TO  NETO-PAG.
           MOVE  99999   TO  TIPO-EG-PAG.
           MOVE  ZEROS   TO  CONDIC-PAG.
           MOVE CON-USERNAME       TO USUARIO-PAG.
           MOVE WK-FECHA-HOY       TO DATE-PAG.
           MOVE WK-HORA-HOY        TO TIME-PAG.
           DISPLAY "BORRANDO MODIFICACIONES... ESPERE UN MOMENTO"
              LINE 24 POSITION  1 REVERSE.
           REWRITE    RECPAG-COMP  INVALID KEY WRITE RECPAG-COMP.
           CLOSE       CNRECPAG.
           OPEN INPUT CNRECPAG.
            PERFORM I-ANTICIPO THRU F-ANTICIPO.
            MOVE "R"  TO W-OPCION.
       F-ELIMINAR.
      *
       I-CONSULTAR.
            MOVE ZEROS TO SW-OK.
       CONSULTAR-1.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY " PRESIONE <> CONSULTAR " LINE 24 POSITION 1
           DISPLAY " <F8> FACTURAS     <F1>  SALIR " LINE 24 POSITION 27
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO.
           IF  F2   PERFORM I-FIN THRU F-FIN.
           IF  F1   DISPLAY WK-ESPACIOS LINE 24 POSITION 1
                    GO F-CONSULTAR.
           IF  F8
               PERFORM  I-PANTALLA-FACTURA   THRU  F-PANTALLA-FACTURA
               MOVE  14     TO I
               PERFORM I-MOSTRAR-FACTURAS    THRU  F-MOSTRAR-FACTURAS
                      VARYING J  FROM 1 BY 1 UNTIL J > 8
               PERFORM ACEPTA-TRUCO.
           PERFORM I-BORRA-TEMP  THRU F-BORRA-TEMP
           PERFORM I-BORRA-TABLA  THRU  F-BORRA-TABLA
           PERFORM I-LECTURA-SEC THRU F-LECTUR
           IF SW-OK   =  1
                   PERFORM I-FIN-CONSULTAR THRU F-FIN-CONSULTAR
                   GO F-CONSULTAR.
               MOVE   CHEQUEORD       TO WK-CHEQUEORD
               MOVE   EMPRESA-PAG     TO WK-EMPRE
               PERFORM I-CARGA-TABLA  THRU  F-CARGA-TABLA
               PERFORM I-WINDOW-CAB  THRU  F-WINDOW-CAB
               PERFORM I-MOSTRAR-CAB THRU F-MOSTRAR-CAB
               PERFORM I-PANTA-7REG     THRU F-PANTA-7REG.
               GO CONSULTAR-1.
       F-CONSULTAR.
      *
       I-MOSTRAR-FACTURAS.
           ADD 1  TO I.
           MOVE    NROFACT-PAG(J)      TO  ZZ-8.
           DISPLAY ZZ-8                 LINE I   POSITION 29 REVERSE.
           MOVE    VALFACT-PAG(J)      TO  WI-Z99.
           DISPLAY WI-Z99               LINE I   POSITION 35 REVERSE.
           DISPLAY J                    LINE I   POSITION 49 REVERSE.
       F-MOSTRAR-FACTURAS.
 
       I-FIN-CONSULTAR.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
           DISPLAY " NO HAY MAS REGISTROS "     LINE 24 POSITION 1
           DISPLAY " <F1> OPCIONES "            LINE 24 POSITION 25
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
           PERFORM ACEPTA-TRUCO.
           IF   F2 PERFORM I-FIN THRU F-FIN.
       F-FIN-CONSULTAR.
      *
       I-BORRA-TEMP.
                        MOVE ZEROS  TO  WK-NIT
                                        W-BANCO
                                        WK-CONTROL
                                        ANTICIPO-EGR
                                        W-TOTAL
                                        WK-DIFSAL
                                        WK-VRBAS
                                        WK-VLRDOC
                                        W-TOTAL
                                        VR-APLICA
                                        T-TOTAL-CREDITOS
                                        T-TOTAL-DEBITOS
                                        NIT-PAGO
                                        WK-VRFACTURA
                                        FACTURAS
                                        WK-CHEQUEORD
                                        WK-TIPOE
                                        WK-DD
                                        WK-SIG
                                        WK-RETP
                                        WI-Z5P
                                        WI-Z9
                                        WI-Z99
                                        W-SUBTOT
                                        WK-SALARIO
                                        WK-VFACT1
                                        WK-VFACT2
                                        WK-VFACT3.
                        MOVE SPACES TO  WK-IVA
                                        WK-RET
                                        WK-DESCRIP1
                                        WK-DESCRIP2
                                        WK-DESCRIP.
       F-BORRA-TEMP.
      *
       I-BORRA-TEMP-CALC.
                        MOVE ZEROS  TO  W-TOTAL
                                        WK-DIFSAL
                                        WK-VLRDOC                                                                       W-TOTAL
                                        VR-APLICA
                                        T-TOTAL-CREDITOS
                                        T-TOTAL-DEBITOS
                                        WI-Z5P
                                        WI-Z99
                                        WI-Z9
                                        WK-SALARIO.
       F-BORRA-TEMP-CALC.
      *
       I-EXISTE.
           DISPLAY "REGISTRO YA EXISTE" LINE 24 POSITION 01.
       F-EXISTE.
      *
       I-NEXISTE.
           DISPLAY "REGISTRO NO EXISTE" LINE 24 POSITION 01.
       F-NEXISTE.
      *
      ***********************************************************
      *       LECTURA DE ARCHIVOS                               *
      ***********************************************************
      *
       I-LEE-FACTURA-CNCATCTA.
           MOVE CTA-MAYOR(1) TO MAYMAE.
           MOVE CTA-SUB(1)   TO SUBMAE
           MOVE CTA-AUX(1)   TO AUXMAE.
           READ CNCATCTA WITH NO LOCK INVALID KEY
               DISPLAY "AUXILIAR NO EXISTE"
               LINE 24 POSITION 01 REVERSE.
       F-LEE-FACTURA-CNCATCTA.
      *
       I-LEE-EMPRESA.
           MOVE WK-EMPRE  TO  WIN-CODIGO-EMPRESA.
           MOVE ZEROS TO SW-1.
           READ EMPRESAS WITH NO LOCK INVALID KEY
               DISPLAY " EMPRESA NO EXISTE "
               LINE 24 POSITION 1  REVERSE
               MOVE 1 TO SW-1
               GO F-LEE-EMPRESA.
           DISPLAY WIN-CODIGO-EMPRESA LINE 02  POSITION 30 REVERSE.
           DISPLAY WIN-EMPRESA   LINE 02 POSITION 45 REVERSE SIZE 20.
           PERFORM I-LABEL-ARCHIVOS THRU F-LABEL-ARCHIVOS.
       F-LEE-EMPRESA.
      *
       I-LEE-CNNITCED.
           MOVE ZEROS TO SW-EOF.
           READ CNNITCED WITH NO LOCK INVALID KEY
                MOVE 1 TO SW-EOF
                GO F-LEE-CNNITCED.
           MOVE 7 TO W-SW.
       F-LEE-CNNITCED.
      *
       I-LEE-CONTRATO.
           OPEN INPUT  CONTRATO.
           IF  SW-EOF = 10
               MOVE W-CED-ANTCES   TO CONTRATO-CLAVE
            ELSE
               MOVE WK-NIT         TO CONTRATO-CLAVE.
           READ CONTRATO WITH NO LOCK INVALID KEY
                MOVE 3 TO SW-EOF
                CLOSE CONTRATO
                GO F-LEE-CONTRATO.
           IF  SW-EOF = 10
                MOVE W-CED-ANTCES  TO HOJAVIDA-CLAVE
            ELSE
                MOVE WK-NIT        TO HOJAVIDA-CLAVE.
           OPEN INPUT HOJAVIDA.
           READ HOJAVIDA WITH NO LOCK INVALID KEY
                   CLOSE HOJAVIDA
                   MOVE 2 TO SW-EOF
                   GO F-LEE-CONTRATO.
           IF W-SW   =   7   GO CONTRATO-1.
           MOVE    HOJAVIDA-EMPRESA    TO  WIN-CODIGO-EMPRESA
           PERFORM I-LEE-EMPRESAS  THRU  F-LEE-EMPRESAS.
           MOVE    HOJAVIDA-NOMBRE     TO  NOMBM.
           MOVE    WK-NIT           TO  CODIM.
           MOVE    HOJAVIDA-DIR    TO  DIREM
           MOVE    WIN-COD-CIUDAD   TO  CIUDM
           IF CIUDM > 50 COMPUTE CIUDM = CIUDM - 50.
           MOVE    HOJAVIDA-TEL    TO  TELEM.
           MOVE    "N"              TO  FLAG-NIT.
           MOVE     0               TO  SW-CNNITCED.
           MOVE    ZEROS            TO  SW-EOF.
           OPEN    I-O CNNITCED.
           WRITE   REGMA1.
           CLOSE   CNNITCED.
        CONTRATO-1.
           CLOSE HOJAVIDA.
           CLOSE CONTRATO.
       F-LEE-CONTRATO.
      *
       I-LEE-HOJAVIDA.
           MOVE WK-NIT TO HOJAVIDA-CLAVE.
           OPEN INPUT  HOJAVIDA.
           READ HOJAVIDA WITH NO LOCK INVALID KEY
                   CLOSE HOJAVIDA
                   MOVE 2 TO SW-EOF
                   GO F-LEE-HOJAVIDA.
           MOVE    HOJAVIDA-EMPRESA    TO  WIN-CODIGO-EMPRESA
           PERFORM I-LEE-EMPRESAS  THRU  F-LEE-EMPRESAS.
           MOVE    HOJAVIDA-NOMBRE     TO  NOMBM.
           MOVE    WK-NIT           TO  CODIM.
           MOVE    HOJAVIDA-DIR    TO  DIREM
           MOVE    WIN-COD-CIUDAD   TO  CIUDM
           IF CIUDM > 50 COMPUTE CIUDM = CIUDM - 50.
           MOVE    HOJAVIDA-TEL    TO  TELEM.
           MOVE    ZEROS            TO  SW-EOF.
           MOVE    "N"              TO  FLAG-NIT.
           MOVE     0               TO  SW-CNNITCED.
           OPEN    I-O CNNITCED.
           WRITE   REGMA1 INVALID KEY REWRITE REGMA1.
           CLOSE   CNNITCED.
           CLOSE HOJAVIDA.
       F-LEE-HOJAVIDA.
      *
       I-LEE-CNNITANT.
           MOVE ZEROS TO SW-EOF.
           READ CNNITANT WITH NO LOCK INVALID KEY
                MOVE 1 TO SW-EOF.
       F-LEE-CNNITANT.
      *
       I-LEE-BANCOS.
           MOVE ZEROS TO SW-EOF.
           READ NOMIBANK WITH NO LOCK INVALID KEY
                MOVE 1 TO SW-EOF.
       F-LEE-BANCOS.
      *
       I-LECTURA-TIPEGRES.
           MOVE WK-TIPOE TO EGRESOS-CLAVE.
           MOVE 0             TO  SW-LECTURA.
           READ TIPEGRES WITH NO LOCK INVALID KEY
                     MOVE 1 TO SW-LECTURA.
       F-LECTURA-TIPEGRES.
      *
       I-LECTURA-RECPAG.
           MOVE    WK-EMPRE      TO  EMPRESA-PAG.
           MOVE    WK-CHEQUEORD  TO  RECPAG-CLAVE.
           MOVE 0             TO  SW-LECTURA.
           READ CNRECPAG WITH NO LOCK INVALID KEY
                     MOVE 1 TO SW-LECTURA.
       F-LECTURA-RECPAG.
      *
       I-LECTURA-SEC.
           READ CNRECPAG NEXT RECORD WITH NO LOCK AT END
                     MOVE 1 TO SW-OK.
       F-LECTUR.
      *
       I-MASCARA.
           MOVE ZEROS       TO SW-ERROR.
           MOVE WK-DIA      TO WK-MASCARA-DIA
           IF  WK-MES                       <  01
           OR  WK-MES                       >  12
           OR  WK-DIA                       <  01
           OR  WK-DIA                       >  31
           OR  WK-ANO                       <  90
               MOVE "   "                  TO  WK-MASCARA-DIA
               MOVE "   "                  TO  WK-MASCARA-MES
               MOVE "   "                  TO  WK-MASCARA-ANO
               MOVE 1 TO SW-ERROR
               GO F-MASCARA
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
       F-MASCARA.
      *
       I-MASC-NAC.
           MOVE ZEROS       TO SW-ERROR.
           MOVE WK-DIA      TO WK-MASCARA-DIA
           IF  WK-MES                       <  01
           OR  WK-MES                       >  12
           OR  WK-DIA                       <  01
           OR  WK-DIA                       >  31
           OR  WK-ANO                       <  20
               MOVE SPACES                 TO  WK-MASCARA-DIA
               MOVE SPACES                 TO  WK-MASCARA-MES
               MOVE SPACES                 TO  WK-MASCARA-ANO
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
      *
       I-WINDOW-CAB.
           MOVE "NO" TO SG-BLINK
           MOVE "NO" TO SG-BEEP.
           MOVE WK-EMPRE TO  WIN-CODIGO-EMPRESA
           READ EMPRESAS WITH NO LOCK INVALID KEY
               DISPLAY " EMPRESA NO EXISTE "
               LINE 06 POSITION 39 REVERSE
               GO F-WINDOW-CAB.
           MOVE SG-COLOR-TABLE(COL1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(COL2) TO SG-BCOLOR.
           MOVE "LOW,"               TO SG-INTENSITY.
           DISPLAY SPACE ERASE LINE 01 POSITION 01
                         CONTROL SG-CONTROL-STRING.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY " "   CONTROL SG-CONTROL-STRING
           "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออป" LINE 01 POSITION 01
           "บ EGRESO No.:         EMPRESA:
      -    "                   บ" LINE 02 POSITION 01
           "บ TP.EGRESO:                                       VALOR 1:
      -    "   $               บ" LINE 03 POSITION 01
           "บ C.C.- NIT:                                       VALOR 2:
      -    "   $               บ" LINE 04 POSITION 01
           "บ FECHA(DD):  -  -                                 VALOR 3:
      -    "   $               บ" LINE 05 POSITION 01
           "บ COD.BANCO:                                       VR.BASE:
      -    "   $               บ" LINE 06 POSITION 01
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออน" LINE 07 POSITION 01.
           MOVE SG-COLOR-TABLE(COLTIT1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(COLTIT2) TO SG-BCOLOR.
           DISPLAY
           "บ C U E N T A S ณ NOMBRE DE CUENTA  ณ      VALOR   ณSIG.ณ
      -    "   OTROS           บ"
                          LINE 08 POSITION 01 CONTROL SG-CONTROL-STRING.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY
           "บ               ณ                   ณ              ณ    ณBAS
      -    "E GRAB. :          บ" LINE 09 POSITION 01
           "บ               ณ                   ณ              ณ    ณPOR
      -    "C. CTA. :          บ" LINE 10 POSITION 01
           "บ               ณ                   ณ              ณ    ณ
      -    "                   บ" LINE 11 POSITION 01
           "บ               ณ                   ณ              ณ    ณDEB
      -    ". $                บ" LINE 12 POSITION 01
           "บ               ณ                   ณ              ณ    ณCRE
      -    "D.$                บ" LINE 13 POSITION 01
           "บ               ณ                   ณ              ณ    ณDIF
      -    ". $                บ" LINE 14 POSITION 01
           "บ               ณ                   ณ              ณ    ณ
      -    "                   บ" LINE 15 POSITION 01
           "ฬอออออออออออออออฯอออออออออออออออออออฯออออออออออออออฯออออฯอออ
      -    "อออออออออออออออออออน" LINE 16 POSITION 01
           "บ CONCEPTO :
      -    "                   บ" LINE 17 POSITION 01
           "บ
      -    "                   บ" LINE 18 POSITION 01
           "บ                                  PROVEEDOR:
      -    "                   บ" LINE 19 POSITION 01
           "บ NIT CONTABLE:
      -    "CONDICION:         บ" LINE 20 POSITION 01
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออผ" LINE 21 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY "[ESC MENU]  [F2 SALIR]  [F5 GRABAR ] [F1 HELP] [F8 C
      -    "TA.ESP. [F10 COPIAR]"
           LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING HIGH REVERSE
           DISPLAY WIN-EMPRESA LINE 02 POSITION 45 SIZE 20
                           CONTROL SG-CONTROL-STRING.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY SPACE LINE 24 POSITION 01 CONTROL SG-CONTROL-STRING.
           IF W-OPCION = "A"
              MOVE WK-ANO  TO AA-COMP-DOCTRA
              MOVE W-LONG  TO MM-COMP-DOCTRA
              DISPLAY WK-ANO  LINE 05 POSITION 13
              DISPLAY W-LONG  LINE 05 POSITION 16.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY " "   CONTROL SG-CONTROL-STRING
            DISPLAY  "บ TP.EG" LINE 03 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY SPACE LINE 24 POSITION 01 CONTROL SG-CONTROL-STRING.
           PERFORM I-DISPLAY-OPC THRU  F-DISPLAY-OPC.
       F-WINDOW-CAB.
      *
       I-WIN-CTAS.
           MOVE SG-COLOR-TABLE(COLTIT1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(COLTIT2) TO SG-BCOLOR.
           DISPLAY
           "บ C U E N T A S ณ NOMBRE DE CUENTA  ณ      VALOR   ณSIG.ณ
      -    "   OTROS           บ"
                          LINE 08 POSITION 01 CONTROL SG-CONTROL-STRING.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY
           "บ               ณ                   ณ              ณ    ณBAS
      -    "E GRAB. :          บ" LINE 09 POSITION 01
           "บ               ณ                   ณ              ณ    ณPOR
      -    "C. CTA. :          บ" LINE 10 POSITION 01
           "บ               ณ                   ณ              ณ    ณ
      -    "                   บ" LINE 11 POSITION 01
           "บ               ณ                   ณ              ณ    ณDEB
      -    ". $                บ" LINE 12 POSITION 01
           "บ               ณ                   ณ              ณ    ณCRE
      -    "D.$                บ" LINE 13 POSITION 01
           "บ               ณ                   ณ              ณ    ณDIF
      -    ". $                บ" LINE 14 POSITION 01
           "บ               ณ                   ณ              ณ    ณ
      -    "                   บ" LINE 15 POSITION 01
           "ฬอออออออออออออออฯอออออออออออออออออออฯออออออออออออออฯออออฯอออ
      -    "อออออออออออออออออออน" LINE 16 POSITION 01
           "บ CONCEPTO :
      -    "                   บ" LINE 17 POSITION 01
           "บ
      -    "                   บ" LINE 18 POSITION 01
           "บ                                  PROVEEDOR:
      -    "                   บ" LINE 19 POSITION 01
           "บ NIT CONTABLE:
      -    "CONDICION:         บ" LINE 20 POSITION 01
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออผ" LINE 21 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY "[ESC MENU]  [F2 SALIR]  [F5 GRABAR ] [  F1 HELP  ]
      -    "    [  F10   COPIAR  ]     "
           LINE 25 POSITION 01 CONTROL SG-CONTROL-STRING HIGH REVERSE.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY SPACE LINE 24 POSITION 01 CONTROL SG-CONTROL-STRING.
           DISPLAY SPACE LINE 24 POSITION 1 SIZE 80.
           MOVE    WK-NIT-CON          TO WK-NIT-CON  WI-Z9  CODIM.
           DISPLAY WI-Z9             LINE 20 POSITION 17.
           OPEN  INPUT  CNNITCED
           PERFORM I-LEE-CNNITCED  THRU  F-LEE-CNNITCED.
           DISPLAY NOMBM           LINE  20 POSITION 30 SIZE 30.
           CLOSE   CNNITCED.
       F-WIN-CTAS.
      *
       I-PANTALLA-1B.
           MOVE "            MOVIMIENTO A MODIFICAR "
           TO WK-SISTEMA
           PERFORM I-CABECERA-WINDOW THRU F-CABECERA-WINDOW
                        
               DISPLAY "ษออออออออออออออออออออออออออออออออออออออออออออป"
                                               LINE 10 POSITION 20
               DISPLAY "บ  TECLEE EL MES A REPORTAR :                บ"
                                               LINE 11 POSITION 20
               DISPLAY "บ  DEPARTAMENTO [ S/T ]     :                บ"
                                               LINE 12 POSITION 20
               DISPLAY "บ   [F2] TERMINAR                            บ"
                                               LINE 13 POSITION 20
               DISPLAY "ศออออออออออออออออออออออออออออออออออออออออออออผ"
                                               LINE 14 POSITION 20
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1.
       F-PANTALLA-1B.
      *
       I-AYUDA-ESPECIAL.
           MOVE SG-COLOR-TABLE(4) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(1) TO SG-BCOLOR.
           DISPLAY " "  LINE 20 POSITION 12  CONTROL SG-CONTROL-STRING.
           DISPLAY
           "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออป" LINE 20 POSITION 01
                                CONTROL SG-CONTROL-STRING
           "บ CTA.ESPEC: No realiza calculo interno y el 1er valor relac
      -    "ionado corresponde บ" LINE 21 POSITION 01
           "บ            a la primera Cta. y asi sucesivamente; Ej: Paga
      -    "res, incapac.ISS   บ" LINE 22 POSITION 01
           "บ CTA.NORMAL: La Cta. es normal, Ej: pago a proveedores, ant
      -    "icipos, Gtos.Gralesบ" LINE 23 POSITION 01
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
      -    "อออออออออออออออออออผ" LINE 24 POSITION 01.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " " LINE 24 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-AYUDA-ESPECIAL.
      *
       I-ACEPTA-MES.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE   WK-MES            TO W-LONG.
           ACCEPT W-LONG LINE 11 POSITION 50 REVERSE
               UPDATE  NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
           DISPLAY WK-ESPACIOS LINE 24 POSITION 1
           IF W-LONG = 0 OR W-LONG > 12
              DISPLAY " CODIGO DEL MES ERRADO...DIGITE NUEVAMENTE"
                        LINE 24 POSITION 1 REVERSE BLINK
                        GO I-ACEPTA-MES.
           IF F2 PERFORM I-FIN THRU F-FIN.
           DISPLAY WK-NOMBRE-MES(W-LONG) LINE 11 POSITION 54 REVERSE.
           DISPLAY WK-ESPACIOS LINE 24 POSITION 1.
       I-FLAG.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE  SPACES             TO FLAG-TRABAJO.
           ACCEPT FLAG-TRABAJO  LINE 12 POSITION 50 REVERSE
               UPDATE NO BEEP
               ON EXCEPTION WK-EX
               PERFORM 999-EXCEPTION.
           DISPLAY WK-ESPACIOS LINE 24 POSITION 1
           IF FLAG-TRABAJO = "S" OR FLAG-TRABAJO = "T"
              NEXT SENTENCE
           ELSE
              DISPLAY " FLAG ERRADO...DIGITE NUEVAMENTE"
                        LINE 24 POSITION 1 REVERSE BLINK
                        GO I-FLAG.
           IF F2 PERFORM I-FIN THRU F-FIN.
           IF FLAG-TRABAJO = "S"
              DISPLAY "ESPECIAL" LINE 12 POSITION 54 REVERSE.
           IF FLAG-TRABAJO = "T"
              DISPLAY "TESORERIA" LINE 12 POSITION 54 REVERSE.
           DISPLAY WK-ESPACIOS LINE 24 POSITION 1.
       F-ACEPTA-MES.

       I-CONFIRMA-COMP.
           DISPLAY " PRESIONE <C> PARA CONFIRMAR "
                     LINE 24 POSITION 1 REVERSE
           DISPLAY " [F2] TERMINAR " LINE 24 POSITION 32 REVERSE.
           PERFORM ACEPTA-TRUCO
           IF  F2  PERFORM I-FIN THRU F-FIN.
           IF  F8  GO F-CONFIRMA-COMP.
           IF  W-TRUCO NOT = "C" GO TO I-CONFIRMA-COMP
           ELSE DISPLAY  WK-ESPACIOS  LINE 24 POSITION  1.
       F-CONFIRMA-COMP.
      *
      *
      *
       I-WINDOW-EMPRESA1S.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY "ษออออออัออออออออออออออออออออออออออออออออออออออออออป"
                        LINE  6 POSITION 25 CONTROL SG-CONTROL-STRING
                                                       REVERSE.
           DISPLAY "บ COD. ณ    EMPRESA                               บ"
                        LINE  7 POSITION 25 REVERSE.
           DISPLAY "วฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
                        LINE  8 POSITION 25 REVERSE.
           DISPLAY "บ      ณ                                          บ"
                        LINE  9 POSITION 25 REVERSE.
           DISPLAY "บ      ณ                                          บ"
                        LINE 10 POSITION 25 REVERSE.
           DISPLAY "บ      ณ                                          บ"
                        LINE 11 POSITION 25 REVERSE.
           DISPLAY "บ      ณ                                          บ"
                        LINE 12 POSITION 25 REVERSE.
           DISPLAY "บ      ณ                                         บ"
                        LINE 13 POSITION 25 REVERSE.
           DISPLAY "ศออออออฯออออออออออออออออออออออออออออออออออออออออออผ"
                        LINE 14 POSITION 25 REVERSE.
       F-WINDOW-EMPRESA1S.
      *
       I-PANTALLA-FACTURA.
           MOVE SG-COLOR-TABLE(5) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY "ษอออออออัออออออออออออัออออออออออออออออออออป"
                        LINE  12 POSITION 25 CONTROL SG-CONTROL-STRING
                                                       REVERSE.
           DISPLAY "บ SEC.  ณ  FACTURA # ณ       VALOR        บ"
                        LINE  13 POSITION 25 REVERSE.
           DISPLAY "วฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
                        LINE  14 POSITION 25 REVERSE.
           DISPLAY "บ       ณ            ณ                    บ"
                        LINE 15 POSITION 25 REVERSE.
           DISPLAY "บ       ณ            ณ                    บ"
                        LINE 16 POSITION 25 REVERSE.
           DISPLAY "บ       ณ            ณ                    บ"
                        LINE 17 POSITION 25 REVERSE.
           DISPLAY "บ       ณ            ณ                    บ"
                        LINE 18 POSITION 25 REVERSE.
           DISPLAY "บ       ณ            ณ                    บ"
                        LINE 19 POSITION 25 REVERSE.
           DISPLAY "บ       ณ            ณ                    บ"
                        LINE 20 POSITION 25 REVERSE.
           DISPLAY "บ       ณ            ณ                    บ"
                        LINE 21 POSITION 25 REVERSE.
           DISPLAY "บ       ณ            ณ                    บ"
                        LINE 22 POSITION 25 REVERSE.
           DISPLAY "ศอออออออฯออออออออออออฯออออออออออออออออออออผ"
                        LINE 23 POSITION 25 REVERSE.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " "  LINE 25 POSITION 80 CONTROL SG-CONTROL-STRING.
       F-PANTALLA-FACTURA.
      *
       I-BORRA-TABLA.
              MOVE ZEROS  TO K.
         I-LOOP-BT.
              ADD  1  TO  K.
              IF   K   >  8  GO F-BORRA-TABLA.
              MOVE ZEROS  TO TAB-MAYCTA(K).
              MOVE ZEROS  TO TAB-SUBCTA(K).
              MOVE ZEROS  TO TAB-AUXCTA(K).
              MOVE ZEROS  TO NUMFRA(K).
              MOVE ZEROS  TO WK-VFACT(K).
              MOVE ZEROS  TO TAB-NIT(K).
              MOVE ZEROS  TO TAB-FECVEN(K).
              MOVE SPACES TO TAB-CONCEPTO(K).
              MOVE ZEROS  TO TAB-VLRDOC(K).
              MOVE ZEROS  TO TAB-VLRBAS(K).
              MOVE ZEROS  TO TAB-SIGNO(K).
              MOVE ZEROS  TO TAB-VALOR(K).
              MOVE ZEROS  TO TAB-CONDICION(K).
              GO   I-LOOP-BT.
       F-BORRA-TABLA.
      *
       I-BORRA-REG.
              MOVE ZEROS  TO NUMFRA(J).
              MOVE ZEROS  TO WK-VFACT(K).
              MOVE ZEROS  TO TAB-NIT(J).
              MOVE ZEROS  TO TAB-FECVEN(J).
              MOVE SPACES TO TAB-CONCEPTO(J).
              MOVE ZEROS  TO TAB-VLRDOC(J).
              MOVE ZEROS  TO TAB-VLRBAS(J).
              MOVE ZEROS  TO TAB-SIGNO(J).
              MOVE SPACES TO TAB-TIPREG(J).
              MOVE ZEROS  TO TAB-CONDICION(J).
       F-BORRA-REG.
      *
       I-CARGA-TABLA.
           MOVE ZEROS  TO K.
         I-LOOP-CT.
           ADD   1   TO   K.
           IF   K   >  8  GO         F-CARGA-TABLA.
           MOVE    PAG-MAYOR(K)      TO TAB-MAYCTA(K).
           MOVE    PAG-SUB(K)        TO TAB-SUBCTA(K).
           MOVE    PAG-AUX(K)        TO TAB-AUXCTA(K).
           MOVE    PAG-VAL(K)        TO TAB-VALOR(K).
           MOVE    PAG-SIG(K)        TO TAB-SIGNO(K)
           MOVE    NROFACT-PAG(K)    TO NUMFRA(K).
           MOVE    VALFACT-PAG(K)    TO WK-VFACT(K).
           GO      I-LOOP-CT.
       F-CARGA-TABLA.
      *
       I-TABLA-CTAEG.
           MOVE ZEROS  TO K.
         I-LOOP-CT1.
           ADD   1   TO   K.
           IF   K   >  8  GO  F-TABLA-CTAEG.
           MOVE    CTA-MAYOR(K)      TO TAB-MAYCTA(K).
           MOVE    CTA-SUB(K)        TO TAB-SUBCTA(K).
           MOVE    CTA-AUX(K)        TO TAB-AUXCTA(K).
           MOVE    CTA-SIGNO(K)      TO TAB-SIGNO(K)
           GO      I-LOOP-CT1.
       F-TABLA-CTAEG.
      *
       I-COLOR1.
           MOVE ZEROS TO COL1 COL2.
         I-LOOPCOL1.
           ADD 1 TO COL1.
           IF  COL1 > 8 GO F-CLR1.
         I-LOOPCOL2.
           ADD 1 TO COL2.
           IF  COL2 > 8 MOVE ZEROS TO COL2
                        GO   I-LOOPCOL1.
           MOVE SG-COLOR-TABLE(COL1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(COL2) TO SG-BCOLOR.
           PERFORM I-WINDOW-CAB THRU F-WINDOW-CAB.
           DISPLAY COL1 LINE 24 POSITION 77.
           DISPLAY COL2 LINE 24 POSITION 79.
           PERFORM ACEPTA-TRUCO.
           IF   F2 GO F-COLOR1.
           GO   I-LOOPCOL2.
         F-CLR1.
           MOVE ZEROS TO COL1 COL2.
         I-LOOPCL1.
           ADD 1 TO COL1.
           IF  COL1 > 8 GO F-COLOR1.
         I-LOOPCL2.
           ADD 1 TO COL2.
           IF  COL2 > 8 MOVE ZEROS TO COL2
                        GO   I-LOOPCL1.
           MOVE SG-COLOR-TABLE(COL1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(COL2) TO SG-BCOLOR.
           PERFORM I-WINDOW-CAB THRU F-WINDOW-CAB.
           DISPLAY COL1 LINE 24 POSITION 77.
           DISPLAY COL2 LINE 24 POSITION 79.
           PERFORM ACEPTA-TRUCO.
           IF   F2 GO F-COLOR1.
           GO   I-LOOPCL2.
       F-COLOR1.
      *
       I-COLOR2.
           MOVE ZEROS TO COLTIT1 COLTIT2.
         I-LOOPCOLTIT1.
           ADD 1 TO COLTIT1.
           IF  COLTIT1 > 8 GO F-COLOR2.
         I-LOOPCOLTIT2.
           ADD 1 TO COLTIT2.
           IF  COL2 > 8 MOVE ZEROS TO COLTIT2
                        GO   I-LOOPCOLTIT1.
           MOVE SG-COLOR-TABLE(COLTIT1) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(COLTIT2) TO SG-BCOLOR.
           PERFORM I-WINDOW-CAB THRU F-WINDOW-CAB.
           DISPLAY COLTIT1 LINE 24 POSITION 77.
           DISPLAY COLTIT2 LINE 24 POSITION 79.
           PERFORM ACEPTA-TRUCO.
           IF   F2 GO F-COLOR2.
           GO   I-LOOPCOLTIT2.
       F-COLOR2.

       I-STATUS-CNRECPAG.
               MOVE ZEROS                  TO  SW-ABRE
               OPEN INPUT CNRECPAG
               MOVE 1                      TO  SW-ABRE
               IF  CNRECPAG-STATUS              =  "35"
                   OPEN OUTPUT CNRECPAG
               ELSE
                   IF  CNRECPAG-STATUS              >  "00"
                       PERFORM RUTINA-ERROR-CNRECPAG
                       PERFORM I-FIN      THRU F-FIN.
               CLOSE CNRECPAG.
       F-STATUS-CNRECPAG.
      *
       I-HELP-SIGNO.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(5) TO SG-BCOLOR.
           DISPLAY "ษออออออออออออออออป"
                        LINE  21 POSITION 40 CONTROL SG-CONTROL-STRING
                                                       REVERSE.
           DISPLAY "บ   1.   DEBITO  บ"
                        LINE 22 POSITION 40 REVERSE.
           DISPLAY "บ   2.   CREDITO บ"
                        LINE 23 POSITION 40 REVERSE.
           DISPLAY "ศออออออออออออออออผ"
                        LINE 24 POSITION 40 REVERSE.
           PERFORM ACEPTA-TRUCO.
           PERFORM I-IMPR-SIGNO THRU F-IMPR-SIGNO  VARYING N FROM 21
                   BY 1  UNTIL  N > 24.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.
           DISPLAY " " LINE 24  POSITION 80
                        CONTROL SG-CONTROL-STRING REVERSE.
       F-HELP-SIGNO.
      *
       I-IMPR-SIGNO.
           MOVE SG-COLOR-TABLE(2) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(2) TO SG-BCOLOR.

           DISPLAY "                      " LINE N  POSITION 40
                        CONTROL SG-CONTROL-STRING REVERSE.
       F-IMPR-SIGNO.
      *
       I-DISPLAY-HELP.
           DISPLAY " PRESIONE <F1> ---> TABLA DE AYUDA "
                             LINE 24 POSITION 44.
       F-DISPLAY-HELP.
      ****************************************************************
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\HELBANKO.PRO".
           COPY "..\PRO\HELPEGRE.PRO".
           COPY "..\PRO\HELPNIT.PRO".
           COPY "..\PRO\HELPANT.PRO".
           COPY "..\PRO\CNCESANT.PRO".
           COPY "..\PRO\CNPRIMAS.PRO".
           COPY "..\PRO\CNSUELDO.PRO".
           COPY "..\PRO\TBCONDOC.PRO".
           COPY "..\PRO\CNVACACI.PRO".
           COPY "..\PRO\BUSCACON.PRO".

           COPY "..\PLB\CNRECPAG.PLB".
           COPY "..\PLB\CNCESANT.PLB".
           COPY "..\PLB\CNPRIMAS.PLB".
           COPY "..\PLB\CNVACACI.PLB".
           COPY "..\PLB\CNSUELDO.PLB".
           COPY "..\PLB\CONTRATO.PLB".
           COPY "..\STA\TBCONDOC.STA".
      ****************************************************************


