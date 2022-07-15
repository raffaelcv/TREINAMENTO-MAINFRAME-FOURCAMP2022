      *====================================================             00010000
       IDENTIFICATION                           DIVISION.               00020000
      *====================================================             00030000
       PROGRAM-ID. FR01CB18.                                            00040000
                                                                        00041000
      *====================================================             00042000
      *  AUTOR: RAFAEL                                                  00043000
      * EMPRESA: FOURSYS                                                00044000
      *====================================================             00045000
      *                                                                 00045100
      * APLICANDO CONHECIMENTO IF COM OPERADORES LOGICOS                00045200
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
      *==============ENTRADA DE DADOS======================             00049400
                                                                        00049500
       01 WRK-DADOS.                                                    00049600
          05 WRK-CODIGO        PIC 9(05)    VALUE ZEROS.                00049700
          05 WRK-PRODUTO       PIC X(15)    VALUE SPACES.               00049800
          05 WRK-VALOR         PIC 9(06)V99 VALUE ZEROS.                00049900
          05 WRK-ESTADO        PIC X(02)    VALUE SPACES.               00050000
                                                                        00050100
      *-------------VARIAVEIS DE CALCULO------------------              00050200
                                                                        00050300
       77 WRK-FRETE            PIC 9(04)V99 VALUE ZEROS.                00050400
                                                                        00050500
      *-------------VARIAVEIS=EDITADAS---------------------             00050600
                                                                        00050700
       77 WRK-VALOR-ED         PIC ZZZ.ZZ9,99.                          00050800
       77 WRK-FRETE-ED         PIC Z.ZZ9,99.                            00050900
                                                                        00051000
      *====================================================             00051100
       PROCEDURE                                DIVISION.               00051200
      *====================================================             00051300
                                                                        00051431
      *----------------------------------------------------             00051531
       0000-PRINCIPAL                           SECTION.                00051631
      *----------------------------------------------------             00051731
                                                                        00051831
             PERFORM 1000-INICIALIZAR.                                  00051931
             PERFORM 2000-PROCESSAR.                                    00052031
             PERFORM 3000-FINALIZAR.                                    00052131
             STOP RUN.                                                  00052231
                                                                        00052331
       0000-99-FINAL.          EXIT.                                    00052432
                                                                        00052532
      *----------------------------------------------------             00052631
       1000-INICIALIZAR                         SECTION.                00052731
      *----------------------------------------------------             00052831
                                                                        00052931
           ACCEPT   WRK-DADOS  FROM SYSIN.                              00053000
                                                                        00053131
       1000-99-FINAL.          EXIT.                                    00053232
                                                                        00053332
      *----------------------------------------------------             00053431
       2000-PROCESSAR                           SECTION.                00053531
      *----------------------------------------------------             00053631
                                                                        00053731
               DISPLAY 'CODIGO DO PRODUTO...' WRK-CODIGO.               00053804
               DISPLAY 'PRODUTO.............' WRK-PRODUTO.              00053904
                                                                        00054025
                                                                        00054232
      *------------------VALIDACAO-------------------------             00054325
              IF WRK-VALOR IS NUMERIC                                   00054427
               MOVE WRK-VALOR TO WRK-VALOR-ED                           00054526
              ELSE                                                      00054626
                DISPLAY ' VALOR DA COMPRA    ' WRK-VALOR                00054726
                DISPLAY ' DIGITE UM VALOR NUMERICO'                     00054830
              END-IF.                                                   00054926
              IF WRK-ESTADO IS ALPHABETIC                               00055026
                DISPLAY ' ESTADO DESEJADO ....' WRK-ESTADO              00055126
              ELSE                                                      00055226
                DISPLAY ' ESTADO ESCOLHIDO....' WRK-ESTADO              00055329
                DISPLAY ' DIGITACAO INCORRETA.'                         00055429
              END-IF.                                                   00055526
      *-----------VERIFICANDO VALOR E ESTADO---------------             00055620
              IF WRK-VALOR GREATER OR EQUAL 1000,00                     00055713
                           AND WRK-ESTADO EQUAL 'SP' OR 'RJ' OR 'MG'    00055813
                DISPLAY ' VALOR DA COMPRA  ' WRK-VALOR-ED               00055901
                DISPLAY ' ENTREGA DISPONIVEL PARA VALOR E ESTADO '      00056030
              ELSE                                                      00056109
                IF WRK-VALOR LESS 1000,00                               00056228
                DISPLAY ' VALOR DA COMPRA.....' WRK-VALOR-ED            00056328
                DISPLAY ' VALOR NAO DISPONIVEL PARA FRETE. '            00056427
               ELSE                                                     00056527
                DISPLAY ' ESTADO NAO DISPONIVEL PARA FRETE.'            00056627
              END-IF.                                                   00056719
                                                                        00056820
      *------------------CALCULO DE FRETE------------------             00056920
              IF WRK-ESTADO EQUAL 'SP' AND WRK-VALOR GREATER 1000,00    00057118
                COMPUTE WRK-FRETE = WRK-VALOR * 0,10                    00057210
                 MOVE WRK-FRETE TO WRK-FRETE-ED                         00057310
                  DISPLAY ' O VALOR DO FRETE PARA SP:R$' WRK-FRETE-ED   00057404
              ELSE                                                      00057510
              IF WRK-ESTADO EQUAL 'RJ' AND WRK-VALOR GREATER 1000,00    00057618
                COMPUTE WRK-FRETE = WRK-VALOR * 0,15                    00057710
                 MOVE WRK-FRETE TO WRK-FRETE-ED                         00057810
                  DISPLAY ' O VALOR DO FRETE PARA RJ:R$' WRK-FRETE-ED   00057904
              ELSE                                                      00058010
              IF WRK-ESTADO EQUAL 'MG' AND WRK-VALOR GREATER 1000,00    00058118
                COMPUTE WRK-FRETE = WRK-VALOR * 0,10                    00058210
                 MOVE WRK-FRETE TO WRK-FRETE-ED                         00058310
                  DISPLAY ' O VALOR DO FRETE PARA MG:R$' WRK-FRETE-ED   00058404
              END-IF.                                                   00059001
                                                                        00059132
       2000-99-FINAL.          EXIT.                                    00059232
                                                                        00059332
      *----------------------------------------------------             00060031
       3000-FINALIZAR                           SECTION.                00070031
      *----------------------------------------------------             00080031
             DISPLAY ' FIM DO PROGRAMA'.                                00090031
                                                                        00100032
       3000-99-FINAL.          EXIT.                                    00110032
