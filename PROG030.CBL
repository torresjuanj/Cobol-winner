       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG030.
      ******************************************************************
      *     CITYS        MAY 1 DE  2002                                          *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               COPY "..\DYC\USER-ACT.DYC".
               COPY "..\DYC\AUDITE.DYC".
               COPY "..\DYC\EMPRESAS.DYC".
               COPY "..\DYC\NOMICIUD.DYC".
               COPY "..\DYC\CNCATCTA.DYC".
               COPY "..\DYC\CJASUBSI.DYC".
               COPY "..\DYC\CENTRIPS.DYC".
               COPY "..\DYC\TBREDECI.DYC".
               COPY "..\DYC\CONTDPTO.DYC".
       DATA DIVISION.
       FILE SECTION.
               COPY "..\FD\AUDITE.FD".
               COPY "..\FD\CJASUBSI.FD".
               COPY "..\FD\USER-ACT.FD".
               COPY "..\FD\CNCATCTA.FD".
               COPY "..\FD\EMPRESAS.FD".
               COPY "..\FD\CONTDPTO.FD".
               COPY "..\FD\NOMICIUD.FD".
               COPY "..\FD\CENTRIPS.FD".
           COPY "..\FD\TBREDECI.FD".
       WORKING-STORAGE SECTION.
       77      W-PROGRAMA           PIC X(7)       VALUE "PROG030".
       77      ID-PRG               PIC X(7)       VALUE "PROG030".
       77      WI-TEL               PIC Z(9).
       77      W-VARIABLES-NOMINA   PIC X(8)        VALUE  SPACES.
      *
               COPY "..\LBL\AUDITE.LBL".
               COPY "..\LBL\CJASUBSI.LBL".
               COPY "..\LBL\EMPRESAS.LBL".
               COPY "..\LBL\CNCATCTA.LBL".
               COPY "..\LBL\NOMICIUD.LBL".
               COPY "..\LBL\CONTDPTO.LBL".
               COPY "..\LBL\USER-ACT.LBL".
               COPY "..\LBL\CENTRIPS.LBL".
               COPY "..\LBL\TBREDECI.LBL".
      *
               COPY "..\WRK\USER-ACT.WRK".
               COPY "..\WRK\VARIABLE.WRK".
               COPY "..\WRK\TABLAMES.WRK".
               COPY "..\WRK\TECLADOS.WRK".
               COPY "..\WRK\WK-RAYAS.WRK".
               COPY "..\WRK\IO-ERROR.WRK".
               COPY "..\LNK\CONTROLA.LNK".
       PROCEDURE DIVISION USING  AREA-LINK-NOMINA.
       DECLARATIVES.
               COPY "..\DCL\AUDITE.DCL".
               COPY "..\DCL\CJASUBSI.DCL".
               COPY "..\DCL\USER-ACT.DCL".
               COPY "..\DCL\CNCATCTA.DCL".
               COPY "..\DCL\CONTDPTO.DCL".
               COPY "..\DCL\EMPRESAS.DCL".
               COPY "..\DCL\NOMICIUD.DCL".
               COPY "..\DCL\CENTRIPS.DCL".
               COPY "..\DCL\TBREDECI.DCL".
       END DECLARATIVES.
       PROGRAMA-PRINCIPAL SECTION.
       I-MAIN-PROCESS.
           COPY "..\PRO\CONTROLA.PRO".
           INITIALIZE WK-EXCEPTION
           PERFORM I-USUARIOS       THRU F-USUARIOS.
           IF      SW-USER = 1      EXIT PROGRAM.
           PERFORM I-PROCESO-AUDITE THRU F-PROCESO-AUDITE
           PERFORM I-LABEL-ARCHIVOS THRU F-LABEL-ARCHIVOS
           PERFORM I-ABRE-ARCHIVOS  THRU F-ABRE-ARCHIVOS
           PERFORM I-CREAR-PANTALLA   THRU F-CREAR-PANTALLA
           PERFORM I-MENU-OPCIONES  THRU F-MENU-OPCIONES UNTIL ESC.
           PERFORM I-FIN-MODE       THRU F-FIN-MODE.
       F-MAIN-PROCESS.
      *
       I-LABEL-ARCHIVOS.
      *        MOVE WIN-CODIGO-EMPRESA     TO  LAB-TABLAEMP-CIA
               MOVE WIN-EMPRESA            TO  WK-COMPANIA
               MOVE "น"                    TO  WK-RAYAS-U
               MOVE "ฬ"                    TO  WK-RAYAS-P
               MOVE "ถ"                    TO  WK-RAYA-U
               MOVE "ว"                    TO  WK-RAYA-P.
       F-LABEL-ARCHIVOS.
      *
       I-ABRE-ARCHIVOS.
           PERFORM I-STATUS-TBREDECI THRU F-STATUS-TBREDECI.
           PERFORM I-STATUS-CENTRIPS THRU F-STATUS-CENTRIPS.
           PERFORM I-STATUS-CJASUBSI THRU F-STATUS-CJASUBSI.
           PERFORM I-STATUS-NOMICIUD THRU F-STATUS-NOMICIUD.
           OPEN I-O NOMICIUD TBREDECI.
           OPEN INPUT CNCATCTA CJASUBSI CENTRIPS CONTDPTO.
       F-ABRE-ARCHIVOS.

       I-MENU-OPCIONES.
           PERFORM I-BLANQUEA  THRU F-BLANQUEA.
           PERFORM I-FUNCIONES THRU F-FUNCIONES.
           MOVE ZEROS        TO  WK-EXCEPTION FIN-OK
           ACCEPT W-OPCION   LINE 23 POSITION 46
                  UPDATE  NO BEEP ON EXCEPTION WK-EX
                  PERFORM 999-EXCEPTION.
           IF ESC
              PERFORM  I-FIN-MODE THRU F-FIN-MODE
           ELSE
             IF F5
                PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                PERFORM  I-CREAR          THRU F-CREAR UNTIL ESC OR CUP
                MOVE ZEROS TO WK-EXCEPTION
             ELSE
               IF F6
                  PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                  PERFORM  I-MODIFY-MODE    THRU F-MODIFY-MODE
                           UNTIL ESC OR CUP
                  MOVE ZEROS TO WK-EXCEPTION
               ELSE
                 IF F7
                   PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                   PERFORM  I-CONSUL-MODE    THRU F-CONSUL-MODE
                            UNTIL ESC OR CUP
                   MOVE ZEROS TO WK-EXCEPTION
                 ELSE
                   IF F8
                      PERFORM  I-DISPLAY-OPCION THRU F-DISPLAY-OPCION
                      PERFORM  I-ELIMINA        THRU F-ELIMINA
                               UNTIL ESC OR CUP
                      MOVE ZEROS TO WK-EXCEPTION.
       F-MENU-OPCIONES.


       I-CREAR.
           MOVE 1 TO SW-MODE.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          GO   F-CREAR.
           IF ESC          GO   F-CREAR.
           PERFORM I-LEE-NOMICIUD THRU F-LEE-NOMICIUD.
           IF  SW-1  =  ZEROS
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-EXISTE         THRU F-EXISTE
               GO I-CREAR
           ELSE
               MOVE CIUD-CODIGO TO WK-CODIGO
               MOVE SPACE TO REC-NOMICIUD
               MOVE WK-CODIGO TO CIUD-CODIGO
               PERFORM I-BLANQUEA THRU F-BLANQUEA
               PERFORM I-DATOS    THRU F-DATOS
               IF CUP
                  PERFORM I-BLANQUEA THRU F-BLANQUEA
                  GO I-CREAR
               ELSE
                  PERFORM I-BLANQUEA   THRU F-BLANQUEA.
       F-CREAR.


       I-ELIMINA.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          GO   F-ELIMINA.
           IF      ESC          GO   F-ELIMINA.
           PERFORM I-LEE-NOMICIUD THRU F-LEE-NOMICIUD.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-ELIMINA
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           MOVE SPACE TO W-TRUCO.
           PERFORM I-ELIMINAR   THRU F-ELIMINAR
                   UNTIL W-TRUCO = "E" OR CDN.
           PERFORM I-BLANQUEA THRU F-BLANQUEA.
       F-ELIMINA.

       I-MODIFY-MODE.
           MOVE 2 TO SW-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          GO   F-MODIFY-MODE.
           IF      ESC          GO   F-MODIFY-MODE.
           PERFORM I-LEE-NOMICIUD THRU F-LEE-NOMICIUD.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-MODIFY-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS
               PERFORM I-DATOS    THRU F-DATOS
               IF CUP
                  PERFORM I-BLANQUEA THRU F-BLANQUEA
                  GO I-MODIFY-MODE
               ELSE
                  PERFORM I-BLANQUEA   THRU F-BLANQUEA.
       F-MODIFY-MODE.

       I-CONSUL-MODE.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CODIGO     THRU F-CODIGO.
           IF      CUP          GO   F-CONSUL-MODE.
           IF      ESC          GO   F-CONSUL-MODE.
           PERFORM I-LEE-NOMICIUD THRU F-LEE-NOMICIUD.
           IF  SW-1  =  2
               PERFORM I-NEXISTE         THRU F-NEXISTE
               GO I-CONSUL-MODE
           ELSE
               PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
           MOVE ZEROS TO WK-EXCEPTION.
           PERFORM I-CONSULTA   THRU F-CONSULTA
                   UNTIL ESC OR F2 OR SW-1 = 1.
           PERFORM I-BLANQUEA THRU F-BLANQUEA.
       F-CONSUL-MODE.
      *
       I-GRABAR.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                                               LINE 23 POSITION 01
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
           LINE 24 POSITION 01.
           PERFORM ACEPTA-TRUCO UNTIL W-TRUCO = "G" OR CDN OR CUP.
           IF W-TRUCO = "G"
              WRITE REC-NOMICIUD INVALID KEY STOP " ".
           IF W-TRUCO = "G"
              PERFORM I-OPERA-TBREDECI THRU F-OPERA-TBREDECI.
       F-GRABAR. EXIT.
      *
       I-OPERA-TBREDECI.
           IF FIN-ARCH-TBREDECI = 1
             PERFORM I-GRABA-TBREDECI THRU F-GRABA-TBREDECI
           ELSE
             PERFORM I-REGRABA-TBREDECI THRU F-REGRABA-TBREDECI.
       F-OPERA-TBREDECI. EXIT.
      *
       I-GRABA-TBREDECI.
           PERFORM I-TBREDECI-WRITE THRU F-TBREDECI-WRITE.
           IF FIN-ARCH-TBREDECI = 1
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
             " NO GRABA TBREDECI .. OJO .. " LINE 25 POSITION 01
             MOVE ZEROS TO WK-EXCEPTION
             PERFORM ACEPTA-TRUCO.
       F-GRABA-TBREDECI. EXIT.
      *
       I-REGRABA-TBREDECI.
           PERFORM I-TBREDECI-REWRITE THRU F-TBREDECI-REWRITE.
           IF FIN-ARCH-TBREDECI = 1
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
             " NO REGRABA TBREDECI .. OJO .. " LINE 25 POSITION 01
             MOVE ZEROS TO WK-EXCEPTION
             PERFORM ACEPTA-TRUCO.
       F-REGRABA-TBREDECI. EXIT.
      *
       I-REGRABAR.
           DISPLAY
           "บ       [ G ]  GRABAR REGISTRO     [  ]  CONTINUA        บ"
                                               LINE 23 POSITION 01
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
           LINE 24 POSITION 01    .
           PERFORM ACEPTA-TRUCO UNTIL CDN OR W-TRUCO = "G" OR CUP.
           IF W-TRUCO = "G"
              REWRITE REC-NOMICIUD INVALID KEY STOP " ".
           IF W-TRUCO = "G"
              PERFORM I-OPERA-TBREDECI THRU F-OPERA-TBREDECI.
       F-REGRABAR. EXIT.
      *
       I-ELIMINAR.
           DISPLAY
           "บ       [ E ]  BORRA  REGISTRO     [  ]  CONTINUA        บ"
                                               LINE 23 POSITION 01
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
           LINE 24 POSITION 01    .
           PERFORM ACEPTA-TRUCO.
           IF W-TRUCO = "E"
              DELETE NOMICIUD INVALID KEY STOP " ".
           IF W-TRUCO = "E"
             PERFORM I-BORRA-TBREDECI THRU F-BORRA-TBREDECI.
       F-ELIMINAR. EXIT.
      *
       I-BORRA-TBREDECI.
           IF FIN-ARCH-TBREDECI = 1
             GO F-BORRA-TBREDECI.
           PERFORM I-TBREDECI-DELETE THRU F-TBREDECI-DELETE.
           IF FIN-ARCH-TBREDECI = 1
             DISPLAY SPACES SIZE 80 LINE 25 POSITION 01 BEEP
             " NO GRABA TBREDECI .. OJO .. " LINE 25 POSITION 01
             MOVE ZEROS TO WK-EXCEPTION
             PERFORM ACEPTA-TRUCO.
       F-BORRA-TBREDECI. EXIT.
      *
       I-EXISTE.
           DISPLAY "REGISTRO YA EXISTE" LINE 25 POSITION 01    .
       F-EXISTE.
      *
       I-NEXISTE.
           DISPLAY "REGISTRO NO EXISTE" LINE 25 POSITION 01 .
       F-NEXISTE.
      *
       I-DATOS.
           PERFORM I-NOMBRE     THRU F-NOMBRE.
           IF      CUP          GO   F-DATOS.
         DAT-1.
           PERFORM I-TBREDECI-DEPAR-CONTAB THRU
                   F-TBREDECI-DEPAR-CONTAB.
           IF      CUP          GO   I-DATOS.
         DAT-2.
           PERFORM I-CIUD-RFTE-MUNICIPAL THRU F-CIUD-RFTE-MUNICIPAL.
           IF      CUP          GO   DAT-1.
         DAT-3.
           PERFORM I-CIUD-DANE      THRU F-CIUD-DANE.
           IF      CUP          GO   DAT-2.
         DAT-4.
           PERFORM I-CTA-PRF    THRU F-CTA-PRF.
           IF      CUP          GO   DAT-3.
         DAT-5.
           PERFORM I-CTA-ADM    THRU F-CTA-ADM.
           IF      CUP          GO   DAT-4.
         DAT-6.
           PERFORM I-CTA-CXC    THRU F-CTA-CXC.
           IF      CUP          GO   DAT-5.
         DAT-7.
           PERFORM I-TBREDECI-CIUDA-CONTAB THRU
                   F-TBREDECI-CIUDA-CONTAB.
           IF      CUP          GO   DAT-6.
         DAT-8.
           PERFORM I-PRC-PROV-CESA THRU F-PRC-PROV-CESA.
           IF      CUP          GO   DAT-7.
         DAT-9.
           PERFORM I-PRC-PROV-INCE THRU F-PRC-PROV-INCE.
           IF      CUP          GO   DAT-8.
         DAT-10.
           PERFORM I-PRC-PROV-VACA THRU F-PRC-PROV-VACA.
           IF      CUP          GO   DAT-9.
         DAT-11.
           PERFORM I-PRC-PROV-PRIM THRU F-PRC-PROV-PRIM.
           IF      CUP          GO   DAT-10.
         DAT-12.
           PERFORM I-PRC-PROV-ICBF THRU F-PRC-PROV-ICBF.
           IF      CUP              GO   DAT-11.
         DAT-13.
           PERFORM I-PRC-PROV-SENA THRU F-PRC-PROV-SENA.
           IF      CUP              GO   DAT-12.
         DAT-130.
           PERFORM I-PRC-PROV-SUFA THRU F-PRC-PROV-SUFA.
           IF      CUP              GO   DAT-13.
         DAT-131.
           PERFORM I-PRC-PROV-RIPR THRU F-PRC-PROV-RIPR.
           IF      CUP              GO   DAT-130.
         DAT-132.
           PERFORM I-CIUD-DIRECCION THRU F-CIUD-DIRECCION.
           IF      CUP              GO   DAT-131.
           PERFORM I-CIUD-TELEFONO  THRU F-CIUD-TELEFONO.
         DAT-133.
           PERFORM I-CIUD-FAX       THRU F-CIUD-FAX.
           IF      CUP              GO   DAT-132.
         DAT-14.
           PERFORM I-PRC-PENSION-PATRONO THRU F-PRC-PENSION-PATRONO.
           IF      CUP          GO   DAT-133.
         DAT-15.
           PERFORM I-PRC-SALUD-PATRONO THRU F-PRC-SALUD-PATRONO.
      *    PERFORM I-CAJASUB      THRU F-CAJASUB.
           IF      CUP          GO   DAT-14.
         DAT-16.
           PERFORM I-PENSIONES    THRU F-PENSIONES.
           IF      CUP          GO   DAT-15.
         DAT-17.
           PERFORM I-SALUD        THRU F-SALUD.
           IF      CUP          GO   DAT-16.
         DAT-18.
           PERFORM I-PAGA-SECC  THRU  F-PAGA-SECC.
           IF      CUP          GO   DAT-17.
         DAT-19.
           PERFORM I-SECCIONAL-APORTE THRU F-SECCIONAL-APORTE.
           IF      CUP          GO   DAT-18.
         DAT-20.
           PERFORM I-NRO-PATRONAL THRU F-NRO-PATRONAL.
           IF      CUP          GO   DAT-19.

           IF      SW-MODE = 1
                   MOVE ZEROS  TO WK-EXCEPTION
                   MOVE SPACES TO W-TRUCO
                   PERFORM I-GRABAR THRU F-GRABAR
                           UNTIL W-TRUCO = "G" OR CDN OR CUP
                   IF      CUP  GO DAT-20
                   ELSE    NEXT SENTENCE
           ELSE
              IF   SW-MODE = 2
                   MOVE ZEROS  TO WK-EXCEPTION
                   MOVE SPACES TO W-TRUCO
                   PERFORM I-REGRABAR THRU F-REGRABAR
                           UNTIL W-TRUCO = "G" OR CDN OR CUP
                   IF      CUP  GO DAT-20.
       F-DATOS. EXIT.
      *
       I-MUESTRA-CAMPOS.
           DISPLAY CIUD-CODIGO               LINE 02 POSITION 10
           DISPLAY CIUD-NOMBRE               LINE 02 POSITION 16
           DISPLAY TBREDECI-COD-DANE         LINE 04 POSITION 47
           DISPLAY TBREDECI-DEPAR-CONTAB     LINE 03 POSITION 10
           PERFORM I-LEE-CONTDPTO  THRU F-LEE-CONTDPTO.
           IF SW-2 = 0
              DISPLAY  NOM-DPTO LINE 03 POSITION 16
           ELSE
              DISPLAY  SPACES   LINE 03 POSITION 16.
           DISPLAY TBREDECI-CIUDA-CONTAB LINE 11 POSITION 21.
           DISPLAY CIUD-RFTE-MUNICIPAL   LINE 04 POSITION 21
           MOVE    CIUD-CTA-PARAFISCAL       TO WI-Z10.
           DISPLAY WI-Z10                    LINE 08 POSITION 44
           MOVE    CIUD-CTA-ADMON            TO WI-Z10.
           DISPLAY WI-Z10                    LINE 09 POSITION 44
           MOVE    CIUD-CTA-CXC              TO WI-Z10.
           DISPLAY WI-Z10                    LINE 10 POSITION 27

           MOVE CIUD-PRC-PROV-CESA           TO WI-Z3P3.
           DISPLAY WI-Z3P3                   LINE 13 POSITION 22 .

           MOVE CIUD-PRC-PROV-INCE           TO WI-Z3P3
           DISPLAY WI-Z3P3                   LINE 14 POSITION 22

           MOVE    CIUD-PRC-PROV-VACA        TO WI-Z3P3
           DISPLAY WI-Z3P3                   LINE 15 POSITION 22

           MOVE    CIUD-PRC-PROV-PRIM        TO WI-Z3P3
           DISPLAY WI-Z3P3                   LINE 16 POSITION 22

           MOVE    CIUD-PRC-PROV-ICBF        TO WI-Z3P3
           DISPLAY WI-Z3P3                   LINE 13 POSITION 51

           MOVE    CIUD-PRC-PROV-SENA        TO WI-Z3P3
           DISPLAY WI-Z3P3                   LINE 14 POSITION 51

           MOVE    CIUD-PRC-PROV-SUFA        TO WI-Z3P3
           DISPLAY WI-Z3P3                   LINE 15 POSITION 51

           MOVE    CIUD-PRC-PROV-RIPR        TO WI-Z3P3
           DISPLAY WI-Z3P3                   LINE 16 POSITION 51
           DISPLAY CIUD-DIRECCION            LINE 17 POSITION 15

           MOVE    CIUD-TELEFONO             TO WI-TEL
           DISPLAY WI-TEL                    LINE 18 POSITION 15
           MOVE    TBREDECI-FAX              TO WI-TEL
           DISPLAY WI-TEL                    LINE 19 POSITION 15


      *    MOVE    CIUD-CAJASUB      TO WI-Z2 COD-CJASUBSI
      *    PERFORM I-LEE-CJASUBSI  THRU F-LEE-CJASUBSI
      *    DISPLAY NOM-CJASUBSI         LINE 21 POSITION 29 .
      *    IF SW-EOF = 1
      *       DISPLAY "N.N             "
      *       LINE 21 POSITION 38 REVERSE.
      *    DISPLAY WI-Z2                   LINE 21 POSITION 26 .
           MOVE    CIUD-PRC-PENSION-PATRONO  TO WI-Z2P4.
           DISPLAY WI-Z2P4                   LINE 08 POSITION 73 .
           MOVE    CIUD-PRC-SALUD-PATRONO    TO WI-Z2P4.
           DISPLAY WI-Z2P4                   LINE 09 POSITION 73 .

           MOVE    CIUD-PRC-PENSION          TO WI-Z2P4.
           DISPLAY WI-Z2P4                   LINE 12 POSITION 73 .
           MOVE    CIUD-PRC-SALUD            TO WI-Z2P4.
           DISPLAY WI-Z2P4                   LINE 13 POSITION 73 .

           DISPLAY CIUD-IND-PAGO-SECCIONAL LINE 16 POSITION 75 .

           DISPLAY CIUD-ISS LINE 17 POSITION 71 .
           MOVE CIUD-ISS TO LLAVM1-CENTRIPS.
           READ CENTRIPS WITH NO LOCK INVALID KEY
            MOVE "N.N." TO NOM-CENTRIPS.
           DISPLAY NOM-CENTRIPS LINE 18 POSITION 61  SIZE 18.

           MOVE CIUD-NRO-PATRONAL-ISS TO WI-Z12.
           DISPLAY WI-Z12 LINE 20 POSITION 62 .
       F-MUESTRA-CAMPOS. EXIT.
      *
       I-CODIGO.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY CIUD-CODIGO         LINE 02 POSITION 10
           ACCEPT  CIUD-CODIGO         LINE 02 POSITION 10
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP OR ESC GO F-CODIGO.
           IF  CIUD-CODIGO = ZEROS
               DISPLAY
                 "Error condigo errado " LINE 25 POSITION 1
                  REVERSE BEEP GO I-CODIGO.
           MOVE CIUD-CODIGO TO TBREDECI-CIUDAD.
           PERFORM I-LEE-TBREDECI-I00 THRU F-LEE-TBREDECI-I00.
           IF FIN-ARCH-TBREDECI = 1
            MOVE ZEROS  TO TBREDECI-DEPAR-CONTAB
                           TBREDECI-CIUDA-CONTAB
            MOVE SPACES TO TBREDECI-FILLER.
       F-CODIGO. EXIT.

       I-NOMBRE.
           MOVE ZEROS                  TO  WK-EXCEPTION
           DISPLAY CIUD-NOMBRE         LINE 02 POSITION 16
           ACCEPT  CIUD-NOMBRE         LINE 02 POSITION 16
                   UPDATE TAB NO BEEP PROMPT ECHO
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-NOMBRE.
           IF  CIUD-NOMBRE = SPACE GO I-NOMBRE.
       F-NOMBRE.
      *
       I-TBREDECI-DEPAR-CONTAB.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY TBREDECI-DEPAR-CONTAB LINE 03 POSITION 10
           ACCEPT  TBREDECI-DEPAR-CONTAB LINE 03 POSITION 10
                                         UPDATE NO BEEP PROMPT
                                         ON EXCEPTION WK-EX
                                         PERFORM 999-EXCEPTION.
           DISPLAY TBREDECI-DEPAR-CONTAB LINE 03 POSITION 10.
           PERFORM I-LEE-CONTDPTO  THRU F-LEE-CONTDPTO.
           IF SW-2 = 1    GO I-TBREDECI-DEPAR-CONTAB.
           DISPLAY  NOM-DPTO LINE 03 POSITION 16
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO F-TBREDECI-DEPAR-CONTAB.
       F-TBREDECI-DEPAR-CONTAB. EXIT.
      *
      *
       I-CIUD-RFTE-MUNICIPAL.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY CIUD-RFTE-MUNICIPAL  LINE 04 POSITION 21
           ACCEPT  CIUD-RFTE-MUNICIPAL  LINE 04 POSITION 21
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY CIUD-RFTE-MUNICIPAL LINE 04 POSITION 21
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO F-CIUD-RFTE-MUNICIPAL.
           IF CIUD-RFTE-MUNICIPAL = "S" OR
              CIUD-RFTE-MUNICIPAL = "N"
            NEXT SENTENCE
           ELSE
            GO I-CIUD-RFTE-MUNICIPAL.
       F-CIUD-RFTE-MUNICIPAL. EXIT.

       I-CIUD-DANE.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY TBREDECI-COD-DANE LINE 04 POSITION 47
           ACCEPT  TBREDECI-COD-DANE LINE 04 POSITION 47
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY TBREDECI-COD-DANE LINE 04 POSITION 47
           IF TBREDECI-COD-DANE = ZEROS GO I-CIUD-DANE.
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
       F-CIUD-DANE. EXIT.

       I-CTA-PRF.
           DISPLAY
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
           LINE 24 POSITION 01 .
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE    CIUD-CTA-PARAFISCAL          TO WI-Z10.
           DISPLAY WI-Z10                     LINE 08 POSITION 44
           ACCEPT  CIUD-CTA-PARAFISCAL        LINE 08 POSITION 44
                                              UPDATE PROMPT TAB NO BEEP
                                           ON EXCEPTION WK-EX
                                              PERFORM 999-EXCEPTION.
           MOVE    CIUD-CTA-PARAFISCAL          TO WI-Z10  CTAMAE
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           IF SW-EOF = 1 OR ESTACTA = "S"
              DISPLAY "CTA INCONSISTENTE  EN CATALOGO DE CUENTAS"
              LINE 25 POSITION 1 REVERSE BEEP
              GO I-CTA-PRF.
           DISPLAY WI-Z10                       LINE 08 POSITION 44
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CIUD-CTA-PARAFISCAL = SPACE OR
               CIUD-CTA-PARAFISCAL = ZEROS
               GO I-CTA-PRF.
       F-CTA-PRF.
      *
       I-CTA-ADM.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE    CIUD-CTA-ADMON            TO WI-Z10.
           DISPLAY WI-Z10                    LINE 09 POSITION 44
           ACCEPT  CIUD-CTA-ADMON            LINE 09 POSITION 44
                                             UPDATE NO BEEP PROMPT ECHO
                                          ON EXCEPTION WK-EX
                                             PERFORM 999-EXCEPTION.
           MOVE    CIUD-CTA-ADMON               TO WI-Z10  CTAMAE
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           IF SW-EOF = 1 OR ESTACTA = "S"
              DISPLAY "CTA INCONSISTENTE  EN CATALOGO DE CUENTAS"
              LINE 25 POSITION 1 REVERSE BEEP
              GO I-CTA-ADM.
           DISPLAY WI-Z10                       LINE 09 POSITION 44
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CIUD-CTA-ADMON = SPACE OR
               CIUD-CTA-ADMON = ZEROS
               GO I-CTA-ADM.
       F-CTA-ADM.
      *
       I-CTA-CXC.
           MOVE ZEROS                  TO  WK-EXCEPTION
           MOVE    CIUD-CTA-CXC                 TO WI-Z10.
           DISPLAY WI-Z10                       LINE 10 POSITION 27
           ACCEPT  CIUD-CTA-CXC                 LINE 10 POSITION 27
                   UPDATE NO BEEP PROMPT
                   ON EXCEPTION WK-EX
                      PERFORM 999-EXCEPTION.
           MOVE    CIUD-CTA-CXC                 TO WI-Z10
                                                   CTAMAE
           PERFORM I-LEE-CNCATCTA THRU F-LEE-CNCATCTA
           IF SW-EOF = 1 OR ESTACTA = "S"
              DISPLAY "CTA INCONSISTENTE  EN CATALOGO DE CUENTAS"
              LINE 25 POSITION 1 REVERSE BEEP
              GO I-CTA-CXC.
           DISPLAY WI-Z10                       LINE 10 POSITION 27
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-CTA-CXC.
           IF  CIUD-CTA-CXC   = SPACE OR
               CIUD-CTA-CXC   = ZEROS
               GO I-CTA-CXC.
       F-CTA-CXC.

       I-TBREDECI-CIUDA-CONTAB.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY TBREDECI-CIUDA-CONTAB LINE 11 POSITION 21
           ACCEPT  TBREDECI-CIUDA-CONTAB LINE 11 POSITION 21
                                         UPDATE NO BEEP PROMPT
                                      ON EXCEPTION WK-EX
                                         PERFORM 999-EXCEPTION.
           DISPLAY TBREDECI-CIUDA-CONTAB LINE 11 POSITION 21.

           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO F-TBREDECI-CIUDA-CONTAB.
       F-TBREDECI-CIUDA-CONTAB. EXIT.
      *
       I-PRC-PROV-CESA.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY SPACES SIZE 07 LINE 13 POSITION 22.
           DISPLAY CIUD-PRC-PROV-CESA LINE 13 POSITION 22 .
           ACCEPT CIUD-PRC-PROV-CESA LINE 13 POSITION 22
                                     UPDATE NO BEEP PROMPT
                                  ON EXCEPTION WK-EX
                                     PERFORM 999-EXCEPTION.
           MOVE CIUD-PRC-PROV-CESA TO WI-Z3P3.
           DISPLAY WI-Z3P3 LINE 13 POSITION 22 .
           IF CUP GO TO F-PRC-PROV-CESA.
           IF F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CIUD-CODIGO = 1 AND CIUD-PRC-PROV-CESA = ZEROS
            GO TO I-PRC-PROV-CESA.
       F-PRC-PROV-CESA. EXIT.
      *
       I-PRC-PROV-INCE.
           MOVE  ZEROS  TO  WK-EXCEPTION
           DISPLAY SPACES SIZE 07 LINE 14 POSITION 22
           DISPLAY CIUD-PRC-PROV-INCE LINE 14 POSITION 22
           ACCEPT  CIUD-PRC-PROV-INCE LINE 14 POSITION 22
                                      UPDATE NO BEEP PROMPT
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           MOVE    CIUD-PRC-PROV-INCE TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 14 POSITION 22
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PRC-PROV-INCE.
           IF CIUD-CODIGO = 1 AND CIUD-PRC-PROV-INCE = ZEROS
            GO TO I-PRC-PROV-INCE.
       F-PRC-PROV-INCE. EXIT.
      *
       I-PRC-PROV-VACA.
           MOVE  ZEROS  TO  WK-EXCEPTION
           DISPLAY SPACES SIZE 07 LINE 15 POSITION 22
           DISPLAY CIUD-PRC-PROV-VACA LINE 15 POSITION 22
           ACCEPT  CIUD-PRC-PROV-VACA LINE 15 POSITION 22
                                      UPDATE NO BEEP PROMPT
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           MOVE    CIUD-PRC-PROV-VACA TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 15 POSITION 22
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PRC-PROV-VACA.
           IF CIUD-CODIGO = 1 AND CIUD-PRC-PROV-VACA = ZEROS
            GO TO I-PRC-PROV-VACA.
       F-PRC-PROV-VACA. EXIT.
      *
       I-PRC-PROV-PRIM.
           MOVE  ZEROS  TO  WK-EXCEPTION
           DISPLAY SPACES SIZE 07 LINE 16 POSITION 22
           DISPLAY CIUD-PRC-PROV-PRIM LINE 16 POSITION 22
           ACCEPT  CIUD-PRC-PROV-PRIM LINE 16 POSITION 22
                                      UPDATE NO BEEP PROMPT
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           MOVE    CIUD-PRC-PROV-PRIM TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 16 POSITION 22
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PRC-PROV-PRIM.
           IF CIUD-CODIGO = 1 AND CIUD-PRC-PROV-PRIM = ZEROS
            GO TO I-PRC-PROV-PRIM.
       F-PRC-PROV-PRIM. EXIT.
      *
       I-PRC-PROV-ICBF.
           MOVE  ZEROS  TO  WK-EXCEPTION
           DISPLAY SPACES SIZE 07 LINE 13 POSITION 51
           DISPLAY CIUD-PRC-PROV-ICBF LINE 13 POSITION 51
           ACCEPT  CIUD-PRC-PROV-ICBF LINE 13 POSITION 51
                                      UPDATE NO BEEP PROMPT
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           MOVE    CIUD-PRC-PROV-ICBF TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 13 POSITION 51
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PRC-PROV-ICBF.
           IF CIUD-CODIGO = 1 AND CIUD-PRC-PROV-ICBF = ZEROS
            GO TO I-PRC-PROV-ICBF.
       F-PRC-PROV-ICBF. EXIT.
      *
       I-PRC-PROV-SENA.
           MOVE  ZEROS  TO  WK-EXCEPTION
           DISPLAY SPACES SIZE 07 LINE 14 POSITION 51
           DISPLAY CIUD-PRC-PROV-SENA LINE 14 POSITION 51
           ACCEPT  CIUD-PRC-PROV-SENA LINE 14 POSITION 51
                                      UPDATE NO BEEP PROMPT
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           MOVE    CIUD-PRC-PROV-SENA TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 14 POSITION 51
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PRC-PROV-SENA.
           IF CIUD-CODIGO = 1 AND CIUD-PRC-PROV-SENA = ZEROS
            GO TO I-PRC-PROV-SENA.
       F-PRC-PROV-SENA. EXIT.
      *
       I-PRC-PROV-SUFA.
           MOVE  ZEROS  TO  WK-EXCEPTION
           DISPLAY SPACES SIZE 07 LINE 15 POSITION 51
           DISPLAY CIUD-PRC-PROV-SUFA LINE 15 POSITION 51
           ACCEPT  CIUD-PRC-PROV-SUFA LINE 15 POSITION 51
                                      UPDATE NO BEEP PROMPT
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           MOVE    CIUD-PRC-PROV-SUFA TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 15 POSITION 51
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PRC-PROV-SUFA.
           IF CIUD-CODIGO = 1 AND CIUD-PRC-PROV-SUFA = ZEROS
            GO TO I-PRC-PROV-SUFA.
       F-PRC-PROV-SUFA. EXIT.
      *
       I-PRC-PROV-RIPR.
           MOVE  ZEROS  TO  WK-EXCEPTION
           DISPLAY SPACES SIZE 07 LINE 16 POSITION 51
           DISPLAY CIUD-PRC-PROV-RIPR LINE 16 POSITION 51
           ACCEPT  CIUD-PRC-PROV-RIPR LINE 16 POSITION 51
                                      UPDATE NO BEEP PROMPT
                                   ON EXCEPTION WK-EX
                                      PERFORM 999-EXCEPTION.
           MOVE    CIUD-PRC-PROV-RIPR TO WI-Z3P3
           DISPLAY WI-Z3P3 LINE 16 POSITION 51
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PRC-PROV-RIPR.
           IF CIUD-CODIGO = 1 AND CIUD-PRC-PROV-RIPR = ZEROS
            GO TO I-PRC-PROV-RIPR.
       F-PRC-PROV-RIPR. EXIT.
      *
       I-CIUD-DIRECCION.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY CIUD-DIRECCION LINE 17 POSITION 15
           ACCEPT  CIUD-DIRECCION LINE 17 POSITION 15
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY CIUD-DIRECCION LINE 17 POSITION 15
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO F-CIUD-DIRECCION.
       F-CIUD-DIRECCION. EXIT.
      *
       I-CIUD-TELEFONO.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY CIUD-TELEFONO  LINE 18 POSITION 15
           ACCEPT  CIUD-TELEFONO  LINE 18 POSITION 15
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE CIUD-TELEFONO TO WI-TEL
           DISPLAY WI-TEL  LINE 18 POSITION 15
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO F-CIUD-TELEFONO.
       F-CIUD-TELEFONO. EXIT.
      *
       I-CIUD-FAX.
           MOVE ZEROS TO WK-EXCEPTION.
           DISPLAY TBREDECI-FAX   LINE 19 POSITION 15
           ACCEPT  TBREDECI-FAX   LINE 19 POSITION 15
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE TBREDECI-FAX  TO WI-TEL
           DISPLAY WI-TEL  LINE 19 POSITION 15
           IF F2  PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CUP GO F-CIUD-FAX.
       F-CIUD-FAX. EXIT.
      *
      *
       I-CAJASUB.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE    CIUD-CAJASUB              TO WI-Z2.
           DISPLAY WI-Z2                   LINE 21 POSITION 26 .
           ACCEPT  CIUD-CAJASUB     LINE 21 POSITION 26
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE    CIUD-CAJASUB              TO WI-Z2
                                                COD-CJASUBSI
           PERFORM I-LEE-CJASUBSI THRU F-LEE-CJASUBSI
           DISPLAY NOM-CJASUBSI                 LINE 21 POSITION 29 .
           IF SW-EOF = 1
              DISPLAY "ERROR - CAJA SUBSIDIO FAMILIAR NO EXISTE      "
              LINE 25 POSITION 01 REVERSE BEEP GO I-CAJASUB.
           DISPLAY WI-Z2                   LINE 21 POSITION 26 .
           DISPLAY SPACES LINE 25 POSITION 1 SIZE 80
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-CAJASUB.
       F-CAJASUB.

       I-PRC-PENSION-PATRONO.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE    CIUD-PRC-PENSION-PATRONO      TO WI-Z2P3.
           DISPLAY WI-Z2P3                   LINE 08 POSITION 73 .
           ACCEPT  CIUD-PRC-PENSION-PATRONO LINE 08 POSITION 73
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE    CIUD-PRC-PENSION-PATRONO    TO WI-Z2P3.
           DISPLAY WI-Z2P3                   LINE 08 POSITION 73 .
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PRC-PENSION-PATRONO.
       F-PRC-PENSION-PATRONO. EXIT.

       I-PRC-SALUD-PATRONO.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE    CIUD-PRC-SALUD-PATRONO    TO WI-Z2P3.
           DISPLAY WI-Z2P3               LINE 09 POSITION 73 .
           ACCEPT  CIUD-PRC-SALUD-PATRONO LINE 09 POSITION 73
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE    CIUD-PRC-SALUD-PATRONO   TO WI-Z2P3.
           DISPLAY WI-Z2P3             LINE 09 POSITION 73 .
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PRC-SALUD-PATRONO.
       F-PRC-SALUD-PATRONO. EXIT.

       I-PENSIONES.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE    CIUD-PRC-PENSION          TO WI-Z2P3.
           DISPLAY WI-Z2P3                   LINE 12 POSITION 73 .
           ACCEPT  CIUD-PRC-PENSION LINE 12 POSITION 73
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE    CIUD-PRC-PENSION          TO WI-Z2P3.
           DISPLAY WI-Z2P3                   LINE 12 POSITION 73 .
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PENSIONES.
       F-PENSIONES.
      *
       I-SALUD.
           MOVE ZEROS TO WK-EXCEPTION
           MOVE    CIUD-PRC-SALUD          TO WI-Z2P3.
           DISPLAY WI-Z2P3                   LINE 13 POSITION 73 .
           ACCEPT  CIUD-PRC-SALUD LINE 13 POSITION 73
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE    CIUD-PRC-SALUD          TO WI-Z2P3.
           DISPLAY WI-Z2P3                   LINE 13 POSITION 73 .
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-SALUD.
       F-SALUD.
      *
       I-PAGA-SECC.
           MOVE ZEROS TO WK-EXCEPTION
           DISPLAY CIUD-IND-PAGO-SECCIONAL   LINE 16 POSITION 75 .
           ACCEPT  CIUD-IND-PAGO-SECCIONAL   LINE 16 POSITION 75
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY CIUD-IND-PAGO-SECCIONAL   LINE 16 POSITION 75 .
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-PAGA-SECC.
           IF CIUD-IND-PAGO-SECCIONAL = "S" OR
              CIUD-IND-PAGO-SECCIONAL = "N"
            NEXT SENTENCE
           ELSE
            GO I-PAGA-SECC.
       F-PAGA-SECC. EXIT.
      *
       I-SECCIONAL-APORTE.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01
           MOVE ZEROS TO WK-EXCEPTION
           IF CIUD-IND-PAGO-SECCIONAL = "N"
            MOVE ZEROS TO CIUD-ISS
            DISPLAY CIUD-ISS   LINE 17 POSITION 71
            DISPLAY SPACE LINE 18 POSITION 61  SIZE 19
            GO F-SECCIONAL-APORTE.

           DISPLAY CIUD-ISS   LINE 17 POSITION 71 .
           ACCEPT  CIUD-ISS LINE 17 POSITION 71
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           DISPLAY CIUD-ISS   LINE 17 POSITION 71 .
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-SECCIONAL-APORTE.
           IF CIUD-ISS = ZEROS GO I-SECCIONAL-APORTE.
           MOVE CIUD-ISS TO LLAVM1-CENTRIPS.
           READ CENTRIPS WITH NO LOCK INVALID KEY
            DISPLAY
            SPACES SIZE 80 LINE 25 POSITION 01
            "Centro de atenciขn no valido,ENTER para continuar..."
            LINE 25 POSITION 01
            PERFORM ACEPTA-TRUCO
            GO I-SECCIONAL-APORTE.
           DISPLAY NOM-CENTRIPS LINE 18 POSITION 61  SIZE 18.
       F-SECCIONAL-APORTE. EXIT.
      *
       I-NRO-PATRONAL.
           MOVE ZEROS TO WK-EXCEPTION
           IF CIUD-IND-PAGO-SECCIONAL = "N"
            MOVE ZEROS TO CIUD-NRO-PATRONAL-ISS
            MOVE CIUD-NRO-PATRONAL-ISS TO WI-Z12
            DISPLAY WI-Z12 LINE 20 POSITION 62
            GO F-NRO-PATRONAL.

           MOVE CIUD-NRO-PATRONAL-ISS TO WI-Z12.
           DISPLAY WI-Z12   LINE 20 POSITION 62 .
           ACCEPT  CIUD-NRO-PATRONAL-ISS LINE 20 POSITION 62
                   UPDATE NO BEEP PROMPT ON EXCEPTION WK-EX
                   PERFORM 999-EXCEPTION.
           MOVE CIUD-NRO-PATRONAL-ISS TO WI-Z12.
           DISPLAY WI-Z12   LINE 20 POSITION 62 .
           IF  F2 PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF  CUP GO F-NRO-PATRONAL.
           IF CIUD-NRO-PATRONAL-ISS = ZEROS GO I-NRO-PATRONAL.
       F-NRO-PATRONAL. EXIT.
      *
       I-DISPLAY-OPCION.
           DISPLAY
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
           LINE 24 POSITION 01 .
           IF  F5
               DISPLAY "ADICIONA CIUDADES"
                    LINE  1 POSITION 60  BLINK.
           IF  F6
               DISPLAY "MODIFICA CIUDADES"
                    LINE  1 POSITION 60 REVERSE BLINK.
           IF  F7
               DISPLAY "CONSULTA CIUDADES"
                    LINE  1 POSITION 60 REVERSE BLINK.
           IF  F8
               DISPLAY "ELIMINA CIUDADES"
                    LINE  1 POSITION 60 REVERSE BLINK.
       F-DISPLAY-OPCION.

       I-FUNCIONES.
      *     "บ       [ ESC ]  TERMINA           OPCION  [ _ ]          บ"
      *                                         LINE 22 POSITION 10.
      *     DISPLAY SPACE LINE 24 POSITION 01 SIZE 80.
           DISPLAY "CREA            MODIFICA           CONSULTA
      -             "  ELIMINA"      LINE 24 POSITION 14.
           DISPLAY "[ F5 ]"        LINE 24 POSITION 07 REVERSE.
           DISPLAY "[ F6 ]"        LINE 24 POSITION 23 REVERSE.
           DISPLAY "[ F7 ]"        LINE 24 POSITION 42 REVERSE.
           DISPLAY "[ F8 ]"        LINE 24 POSITION 61 REVERSE.
       F-FUNCIONES.

      ***************************************************************
      *                   LECTURA  DE  ARCHIVOS                     *
      ***************************************************************
       I-LEE-NOMICIUD.
               MOVE 0  TO   SW-1.
               READ NOMICIUD WITH NO LOCK INVALID  KEY
                    MOVE 2   TO  SW-1.
       F-LEE-NOMICIUD.
      *
       I-LEE-CONTDPTO.
               MOVE 0  TO   SW-2.
               MOVE TBREDECI-DEPAR-CONTAB TO COD-DPTO.
               READ CONTDPTO WITH NO LOCK INVALID  KEY
                    MOVE 1   TO  SW-2.
       F-LEE-CONTDPTO.

       I-LEE-CNCATCTA.
               MOVE 0  TO   SW-EOF.
               READ CNCATCTA WITH NO LOCK INVALID  KEY
                    MOVE  1  TO  SW-EOF
                    GO F-LEE-CNCATCTA.
           DISPLAY SPACES SIZE 80 LINE 25 POSITION 01.
           DISPLAY NOMCTA  LINE 25 POSITION 01 REVERSE.
       F-LEE-CNCATCTA.


       I-LEE-CJASUBSI.
               MOVE 0  TO   SW-EOF.
               READ CJASUBSI WITH NO LOCK INVALID  KEY
                    MOVE  1  TO  SW-EOF.
       F-LEE-CJASUBSI.

       I-ACTUALIZAR.
           DISPLAY " PRESIONE <M> MODIFICAR "
                   LINE 24 POSITION 1 REVERSE
           DISPLAY " <> MODIFICAR Y SEGUIR "
                   LINE 24 POSITION 27 REVERSE
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54 REVERSE
           PERFORM ACEPTA-TRUCO
               DISPLAY WK-ESPACIOS                  LINE 24 POSITION 1
      *    EVALUE   W-TRUCO
           IF W-TRUCO =  "M"
              REWRITE REC-NOMICIUD.
           IF CDN
              REWRITE REC-NOMICIUD
              PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
              PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-ACTUALIZAR.

       I-CONSULTA.
           DISPLAY " <> REGISTRO SIGUIENTE "
                   LINE 24 POSITION 01 REVERSE
           DISPLAY " <ESC> CANCELAR " LINE 24 POSITION 54 REVERSE
           PERFORM ACEPTA-TRUCO
           IF F2
              PERFORM I-FIN-MODE THRU F-FIN-MODE.
           IF CDN
              PERFORM I-LEE-SIGUIENTE THRU F-LEE-SIGUIENTE.
           IF SW-1 = 0
              PERFORM I-MUESTRA-CAMPOS THRU F-MUESTRA-CAMPOS.
       F-CONSULTA.

       I-LEE-SIGUIENTE.
           MOVE 0 TO SW-1.
           READ NOMICIUD NEXT RECORD WITH NO LOCK AT  END
                MOVE 1 TO SW-1.
           IF SW-1 = 1  GO F-LEE-SIGUIENTE.

           MOVE CIUD-CODIGO TO TBREDECI-CIUDAD.
           PERFORM I-LEE-TBREDECI-I00 THRU F-LEE-TBREDECI-I00.
           IF FIN-ARCH-TBREDECI = 1
            MOVE ZEROS TO TBREDECI-DEPAR-CONTAB
                          TBREDECI-CIUDA-CONTAB
            MOVE SPACES TO TBREDECI-FILLER.
       F-LEE-SIGUIENTE. EXIT.
      *
       120-FT.

       I-BLANQUEA.
           DISPLAY SPACE LINE 08 POSITION 25 SIZE 32.
           DISPLAY SPACE LINE 09 POSITION 25 SIZE 32.
           DISPLAY SPACE LINE 10 POSITION 25 SIZE 32.
           DISPLAY SPACE LINE 13 POSITION 22 SIZE 07.
           DISPLAY SPACE LINE 14 POSITION 22 SIZE 07.
           DISPLAY SPACE LINE 15 POSITION 22 SIZE 07.
           DISPLAY SPACE LINE 16 POSITION 22 SIZE 07.
           DISPLAY SPACE LINE 13 POSITION 51 SIZE 07.
           DISPLAY SPACE LINE 14 POSITION 51 SIZE 07.
           DISPLAY SPACE LINE 15 POSITION 51 SIZE 07.
           DISPLAY SPACE LINE 16 POSITION 51 SIZE 07.
           DISPLAY SPACE LINE 17 POSITION 15 SIZE 21.
           DISPLAY SPACE LINE 17 POSITION 49 SIZE 10.
           DISPLAY SPACE LINE 18 POSITION 15 SIZE 21.
           DISPLAY SPACE LINE 18 POSITION 52 SIZE 07.
           DISPLAY SPACE LINE 19 POSITION 15 SIZE 09.
           DISPLAY SPACE LINE 19 POSITION 50 SIZE 05.
           DISPLAY SPACE LINE 20 POSITION 52 SIZE 07.
           DISPLAY SPACE LINE 21 POSITION 23 SIZE 36.
           DISPLAY SPACE LINE 08 POSITION 73 SIZE 07.
           DISPLAY SPACE LINE 09 POSITION 73 SIZE 07.
           DISPLAY SPACE LINE 16 POSITION 75 SIZE 05.
           DISPLAY SPACE LINE 17 POSITION 71 SIZE 06.
           DISPLAY SPACE LINE 18 POSITION 61 SIZE 19.
           DISPLAY SPACE LINE 20 POSITION 61 SIZE 19.
           DISPLAY SPACE LINE 17 POSITION 15 SIZE 21.
           DISPLAY SPACE LINE 18 POSITION 15 SIZE 11.
       F-BLANQUEA.
      *
       I-CREAR-PANTALLA.
           PERFORM I-PANTA THRU F-PANTA.
           MOVE SG-COLOR-TABLE(8) TO SG-FCOLOR.
           MOVE SG-COLOR-TABLE(3) TO SG-BCOLOR.
           DISPLAY " " LINE 1 POSITION 1 CONTROL SG-CONTROL-STRING
           DISPLAY
           "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
           LINE 01 POSITION 01
           "บ Ciudad:                             Fecha :             บ"
           LINE 02 POSITION 01
           "บ Dpto  :                             Hora  :             บ"
           LINE 03 POSITION 01
           "บ Aplica ICA(S/N)  :                  DANE  :             บ"
           LINE 04 POSITION 01
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
           LINE 05 POSITION 01
           "บ                                                         บ"
           LINE 06
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
           LINE 07 POSITION 01
           "บ Cuenta Ingreso 1 :                                      บ"
           LINE 08 POSITION 01
           "บ Cuenta Ingreso 2 :                                      บ"
           LINE 09 POSITION 01
           "บ Cta Deudores Come:                                      บ"
           LINE 10 POSITION 01
           "บ Ciudad Contable  :                                      บ"
           LINE 11 POSITION 01
           "บ                                                         บ"
           LINE 12 POSITION 01
           "บ    Cesantias     :               I.C.B.F.     :         บ"
           LINE 13 POSITION 01
           "บ    Int.Cesantias :               S.E.N.A.     :         บ"
           LINE 14 POSITION 01
           "บ    Vacaciones    :               Caja Compens.:         บ"
           LINE 15 POSITION 01
           "บ    Prima Servicio:               A.R.P        :         บ"
           LINE 16 POSITION 01
           "บ Direcciขn :                                             บ"
           LINE 17 POSITION 01
           "บ Telefono  :                                             บ"
           LINE 18 POSITION 01
           "บ Fax       :                                             บ"
           LINE 19 POSITION 01
           "บ                                                         บ"
           LINE 20 POSITION 01
           "บ                                                         บ"
           LINE 21 POSITION 01
           "ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
           LINE 22 POSITION 01
           "บ       [ ESC ]  FIN               OPCION  [ _ ]          บ"
           LINE 23 POSITION 01
           "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
           LINE 24 POSITION 01 .
           DISPLAY " Debitos "     LINE 06 POSITION 28 REVERSE.
           DISPLAY " Creditos "    LINE 06 POSITION 44 REVERSE.
           DISPLAY "Ingresos"      LINE 06 POSITION 05 REVERSE.
           DISPLAY "Prov.Prestaciones Sociales"
            LINE 12 POSITION 03 REVERSE.
           DISPLAY "Prov.Aportes Parafisclaes"
            LINE 12 POSITION 33 REVERSE.
           DISPLAY WK-MASCARA-FECHA            LINE 02 POSITION 47
           DISPLAY WK-MASCARA-HORA             LINE 03 POSITION 47 .
           PERFORM I-VENTANA-PRC-SEGUROS THRU F-VENTANA-PRC-SEGUROS.
       F-CREAR-PANTALLA.
      *
       I-VENTANA-PRC-SEGUROS.
           DISPLAY
           "ออออออออออออออออออออป" LINE 03 POSITION 60
           "   Seguridad Social บ" LINE 04 POSITION 60
           "                    บ" LINE 05 POSITION 60
           "ออออออออออออออออออออน" LINE 06 POSITION 60
           "                    บ" LINE 07 POSITION 60
           "                    บ" LINE 08 POSITION 60
           "                    บ" LINE 09 POSITION 60
           "                    บ" LINE 10 POSITION 60
           "                    บ" LINE 11 POSITION 60
           "                    บ" LINE 12 POSITION 60
           "                    บ" LINE 13 POSITION 60
           "                    บ" LINE 14 POSITION 60
           "                    บ" LINE 15 POSITION 60
           "                    บ" LINE 16 POSITION 60
           "                    บ" LINE 17 POSITION 60
           "                    บ" LINE 18 POSITION 60
           "                    บ" LINE 19 POSITION 60
           "                    บ" LINE 20 POSITION 60
           "                    บ" LINE 21 POSITION 60
           "ออออออออออออออออออออผ" LINE 22 POSITION 60
           " Empleador"           LINE 07 POSITION 61 REVERSE
           "% Pension :"          LINE 08 POSITION 61
           "% Salud   :"          LINE 09 POSITION 61
           " Trabajador"          LINE 11 POSITION 61 REVERSE
           "% Pension :"          LINE 12 POSITION 61
           "% Salud   :"          LINE 13 POSITION 61
           "Paga a alguna"        LINE 15 POSITION 61 REVERSE
           "Seccional S/N"        LINE 16 POSITION 61 REVERSE
           "Cod.SECC."            LINE 17 POSITION 61
           "No. Patronal :"       LINE 19 POSITION 61 REVERSE.
       F-VENTANA-PRC-SEGUROS. EXIT.
      *
       I-FIN-MODE.
           CLOSE NOMICIUD CNCATCTA CJASUBSI CENTRIPS TBREDECI
                 CONTDPTO.
           EXIT PROGRAM.
           STOP RUN.
       F-FIN-MODE.
      *
           COPY "..\PRO\PANTALLA.PRO".
           COPY "..\PRO\OPCIONES.PRO".
           COPY "..\PRO\USUARIOS.PRO".
           COPY "..\PRO\TRUQUITO.PRO".
           COPY "..\PRO\AUDITE.PRO".
      *
           COPY "..\STA\TBREDECI.STA".
           COPY "..\STA\NOMICIUD.STA".
           COPY "..\STA\CJASUBSI.STA".
           COPY "..\STA\CENTRIPS.STA".
      *
           COPY "..\LEE\TBREDECI.I00".
      *
           COPY "..\OPE\TBREDECI.WRI".
           COPY "..\OPE\TBREDECI.REW".
           COPY "..\OPE\TBREDECI.DEL".
      ******************************************************************
