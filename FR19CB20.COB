      *====================================================             00010000
       IDENTIFICATION                           DIVISION.               00020000
      *====================================================             00030000
       PROGRAM-ID. FR01CB20.                                            00040000
                                                                        00041000
      *====================================================             00042000
      *  AUTOR: RAFAEL                                                  00043000
      * EMPRESA: FOURSYS                                                00044000
      *====================================================             00045000
      *                                                                 00045100
      * APLICANDO CONHECIMENTO IF COM COMANDO EVALUATE.                 00045246
      *                                                                 00045400
      *====================================================             00045500
                                                                        00045600
      *====================================================             00045700
       ENVIRONMENT                              DIVISION.               00045800
      *====================================================             00045900
       CONFIGURATION                            SECTION.                00046000
       SPECIAL-NAMES.                                                   00046100
           DECIMAL-POINT IS COMMA.                                      00046200
                                                                        00046300
      *====================================================             00046400
       DATA                                     DIVISION.               00046500
      *====================================================             00046600
                                                                        00046746
       WORKING-STORAGE                          SECTION.                00046800
                                                                        00046900
       01 WRK-DADOS.                                                    00047000
          05 WRK-CODIGO        PIC 9(05)    VALUE ZEROS.                00048000
          05 WRK-PRODUTO       PIC X(15)    VALUE SPACES.               00049000
          05 WRK-VALOR         PIC 9(06)V99 VALUE ZEROS.                00049100
          05 WRK-ESTADO        PIC X(02)    VALUE SPACES.               00049200
                                                                        00049300
       77 WRK-FRETE            PIC 9(04)V99 COMP VALUE ZEROS.           00049646
      *-------------VARIAVEIS=EDITADAS---------------------             00049800
                                                                        00049900
       77 WRK-VALOR-ED         PIC ZZZ.ZZ9,99.                          00050000
       77 WRK-FRETE-ED         PIC Z.ZZ9,99.                            00050100
                                                                        00050200
      *====================================================             00050300
       PROCEDURE                                DIVISION.               00050400
      *====================================================             00050500
                                                                        00050638
      *----------------------------------------------------             00050738
       0000-PRINCIPAL                           SECTION.                00050838
      *----------------------------------------------------             00050938
                                                                        00051038
             PERFORM 1000-INICIALIZAR.                                  00051138
             PERFORM 2000-PROCESSAR.                                    00051239
             PERFORM 3000-FINALIZAR.                                    00051438
             STOP RUN.                                                  00051538
      *----------------------------------------------------             00051638
       1000-INICIALIZAR                         SECTION.                00051738
      *----------------------------------------------------             00051838
                                                                        00051938
           ACCEPT WRK-DADOS      FROM SYSIN.                            00052027
                                                                        00052141
       1000-99-FIM.          EXIT.                                      00052241
                                                                        00052338
      *----------------------------------------------------             00052438
       2000-PROCESSAR                           SECTION.                00052538
      *----------------------------------------------------             00052638
                                                                        00052738
                                                                        00053027
             EVALUATE TRUE                                              00053127
               WHEN WRK-ESTADO EQUAL 'SP' AND WRK-VALOR GREATER 1000    00053236
                 COMPUTE WRK-FRETE = WRK-VALOR * 0,10                   00053346
                  PERFORM 2100-TELA                                     00053441
               WHEN WRK-ESTADO EQUAL 'RJ' AND WRK-VALOR GREATER 1000    00054036
                 COMPUTE WRK-FRETE = WRK-VALOR * 0,15                   00057746
                  PERFORM 2100-TELA                                     00057841
               WHEN WRK-ESTADO EQUAL 'MG' AND WRK-VALOR GREATER 1000    00058436
                 COMPUTE WRK-FRETE = WRK-VALOR * 0,20                   00058546
                  PERFORM 2100-TELA                                     00058641
               WHEN OTHER                                               00059227
                 DISPLAY 'ENTREGA OU VALOR INDISPONIVEL'                00059335
             END-EVALUATE.                                              00059437
                                                                        00059543
       2000-99-FIM.          EXIT.                                      00059643
                                                                        00059741
      *----------------------------------------------------             00059841
       2100-TELA                                SECTION.                00059941
      *----------------------------------------------------             00060041
                 MOVE WRK-VALOR TO WRK-VALOR-ED.                        00060146
                 MOVE WRK-FRETE TO WRK-FRETE-ED.                        00060246
                  DISPLAY ' CODIGO.....' WRK-CODIGO.                    00060345
                  DISPLAY ' PRODUTO....' WRK-PRODUTO.                   00060445
                  DISPLAY ' VALOR......' WRK-VALOR-ED.                  00060545
                  DISPLAY ' ESTADO.....' WRK-ESTADO.                    00060645
                  DISPLAY ' FRETE......' WRK-FRETE-ED.                  00060745
                                                                        00060841
       2100-99-FIM.         EXIT.                                       00060943
                                                                        00061038
      *----------------------------------------------------             00061138
       3000-FINALIZAR                           SECTION.                00061238
      *----------------------------------------------------             00062038
             DISPLAY ' FINALIZANDO PROGRAMA '.                          00070040
                                                                        00080041
       3000-99-FIM.          EXIT.                                      00090041
                                                                        00100041
      *----------------------------------------------------             00110041
       9000-TRATA-ERROS                         SECTION.                00120041
      *----------------------------------------------------             00130041
                                                                        00140041
       9000-99-FIM.          EXIT.                                      00150041
