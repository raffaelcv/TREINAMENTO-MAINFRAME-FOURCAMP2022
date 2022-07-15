      *====================================================             00010000
       IDENTIFICATION                           DIVISION.               00020000
      *====================================================             00030000
       PROGRAM-ID. FR01CB16.                                            00040000
                                                                        00041000
      *====================================================             00042000
      *  AUTOR: RAFAEL                                                  00043000
      * EMPRESA: FOURSYS                                                00044000
      *====================================================             00045000
      *                                                                 00045100
      * APLICANDO CONHECIMENTO SOBRE O COMANDO COMPUTE.                 00045200
      *                                                                 00045300
      *====================================================             00045400
                                                                        00045500
      *====================================================             00045600
       ENVIRONMENT                              DIVISION.               00045700
      *====================================================             00045800
       CONFIGURATION                            SECTION.                00045900
       SPECIAL-NAMES.                                                   00046000
           DECIMAL-POINT IS COMMA.                                      00047000
                                                                        00048000
      *====================================================             00049000
       DATA                                     DIVISION.               00049100
      *====================================================             00049200
       WORKING-STORAGE                          SECTION.                00049300
       01 WRK-DADOS.                                                    00049400
          05 WRK-CODLOJA        PIC 9(03)    VALUE ZEROS.               00049510
          05 WRK-PRODUTO        PIC X(30)    VALUE SPACES.              00049610
          05 WRK-QTVENDAS       PIC 9(03)    VALUE ZEROS.               00049710
          05 WRK-VLRUNIT        PIC 9(05)V99 VALUE ZEROS.               00049810
      *===================MEDIAS===========================             00049907
                                                                        00050001
       77 WRK-MEDIADIA          PIC 9(08)V99 COMP VALUE ZEROS.          00050110
       77 WRK-MEDIASEMANA       PIC 9(08)V99 COMP VALUE ZEROS.          00050310
                                                                        00050401
      *===================MASCARAS=========================             00050500
                                                                        00050601
       77 WRK-VLRUNIT-ED        PIC ZZ.ZZ9,99.                          00050708
       77 WRK-MEDIADIA-ED       PIC ZZ.ZZZ.ZZ9,99.                      00051008
       77 WRK-MEDIASEMANA-ED    PIC ZZ.ZZZ.ZZ9,99.                      00051210
                                                                        00052001
      *====================================================             00060000
       PROCEDURE                                DIVISION.               00070000
      *====================================================             00080000
                                                                        00081011
      *----------------------------------------------------             00082011
       0000-PRINCIPAL                           SECTION.                00083011
      *----------------------------------------------------             00084011
                                                                        00085011
             PERFORM 1000-INICIALIZAR.                                  00086011
             PERFORM 2000-PROCESSAR.                                    00087011
             PERFORM 3000-FINALIZAR.                                    00088011
             STOP RUN.                                                  00089011
                                                                        00089112
       0000-99-FINAL.        EXIT.                                      00089212
                                                                        00089311
      *----------------------------------------------------             00089411
       1000-INICIALIZAR                         SECTION.                00089511
      *----------------------------------------------------             00089611
                                                                        00089711
           ACCEPT   WRK-DADOS           FROM SYSIN.                     00090000
                                                                        00091011
                                                                        00092012
       1000-99-FINAL.        EXIT.                                      00093012
                                                                        00094012
      *----------------------------------------------------             00100011
       2000-PROCESSAR                           SECTION.                00101011
      *----------------------------------------------------             00102011
                                                                        00103011
           DISPLAY 'WRK-CODLOJA .... '  WRK-CODLOJA.                    00110007
           DISPLAY 'WRK-PRODUTO......'  WRK-PRODUTO.                    00130007
           DISPLAY 'WRK-QTVENDAS.... '  WRK-QTVENDAS.                   00130107
             MOVE WRK-VLRUNIT TO WRK-VLRUNIT-ED.                        00130200
           DISPLAY 'WRK-VALOR UNIT.. '  WRK-VLRUNIT-ED.                 00130307
           MULTIPLY WRK-QTVENDAS BY WRK-VLRUNIT                         00130406
                                    GIVING WRK-MEDIASEMANA.             00130508
             MOVE WRK-MEDIASEMANA TO WRK-MEDIASEMANA-ED.                00130608
           DISPLAY 'WRK-MEDIA SEMANA.'  WRK-MEDIASEMANA-ED.             00130708
           COMPUTE WRK-MEDIADIA = WRK-QTVENDAS * WRK-VLRUNIT / 5.       00131008
             MOVE WRK-MEDIADIA TO WRK-MEDIADIA-ED.                      00132008
           DISPLAY 'WRK-MEDIA DIA....'  WRK-MEDIADIA-ED.                00133008
                                                                        00133111
                                                                        00133212
       2000-99-FINAL.        EXIT.                                      00133312
                                                                        00133412
      *----------------------------------------------------             00133511
       3000-FINALIZAR                           SECTION.                00133611
      *----------------------------------------------------             00133711
                                                                        00133811
            DISPLAY ' FIM DO PROGRAMA'.                                 00133911
                                                                        00134012
       3000-99-FINAL.        EXIT.                                      00135012
