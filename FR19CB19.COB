      *====================================================             00010000
       IDENTIFICATION                           DIVISION.               00020000
      *====================================================             00030000
       PROGRAM-ID. FR01CB19.                                            00040000
                                                                        00041000
      *====================================================             00042000
      *  AUTOR: RAFAEL                                                  00043000
      * EMPRESA: FOURSYS                                                00044000
      *====================================================             00045000
      *                                                                 00045100
      * APLICANDO CONHECIMENTO IF COM OPERADORES LOGICOS                00045200
      * EXEMPLO DO PROFESSOR IVAN SIMPLIFICANDO                         00045304
      *                                                                 00045400
      *====================================================             00045500
                                                                        00045600
      *====================================================             00045700
       ENVIRONMENT                              DIVISION.               00045800
      *====================================================             00045900
       CONFIGURATION                            SECTION.                00046000
       SPECIAL-NAMES.                                                   00046100
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
       77 WRK-FLAG             PIC 9        VALUE 0.                    00050501
      *-------------VARIAVEIS=EDITADAS---------------------             00050600
                                                                        00050700
       77 WRK-VALOR-ED         PIC ZZZ.ZZ9,99.                          00050800
       77 WRK-FRETE-ED         PIC Z.ZZ9,99.                            00050900
                                                                        00051000
      *====================================================             00051100
       PROCEDURE                                DIVISION.               00051200
      *====================================================             00051300
      *----------------------------------------------------             00051405
       0000-PRINCIPAL                           SECTION.                00051505
      *----------------------------------------------------             00051605
                                                                        00051705
             PERFORM 1000-INICIALIZAR.                                  00051806
               IF WRK-DADOS NOT EQUAL ZEROS                             00051906
                 PERFORM 2000-PROCESSAR                                 00052005
               END-IF.                                                  00052106
             PERFORM 3000-FINALIZAR.                                    00052306
             STOP RUN.                                                  00052406
                                                                        00052507
       0000-99-FINAL.            EXIT.                                  00052607
                                                                        00052707
      *----------------------------------------------------             00052805
       1000-INICIALIZAR                         SECTION.                00052905
      *----------------------------------------------------             00053006
                                                                        00053106
           ACCEPT WRK-DADOS      FROM SYSIN.                            00053205
                                                                        00053306
       1000-99-FINAL.            EXIT.                                  00053407
                                                                        00053507
      *----------------------------------------------------             00053606
       2000-PROCESSAR                           SECTION.                00053705
      *----------------------------------------------------             00053806
           IF WRK-VALOR LESS 1000                                       00053905
             DISPLAY ' RETIRA NA LOJA'                                  00054005
      *-----------CASO VALOR MAIOR QUE 1000----------------             00054105
           ELSE                                                         00054205
               IF WRK-ESTADO EQUAL 'SP'                                 00054305
                 COMPUTE WRK-FRETE = WRK-VALOR * 0,10                   00054405
                  MOVE 1 TO WRK-FLAG                                    00054505
               END-IF                                                   00054605
               IF WRK-ESTADO EQUAL 'RJ'                                 00054705
                 COMPUTE WRK-FRETE = WRK-VALOR * 0,15                   00054805
                  MOVE 1 TO WRK-FLAG                                    00054905
               END-IF                                                   00055005
               IF WRK-ESTADO EQUAL 'MG'                                 00055105
                 COMPUTE WRK-FRETE = WRK-VALOR * 0,20                   00055205
                  MOVE 1 TO WRK-FLAG                                    00055305
                END-IF                                                  00055405
                   IF WRK-FLAG EQUAL 0                                  00055505
                     DISPLAY 'NAO ENTREGAMOS'                           00055605
                   END-IF                                               00055705
           END-IF.                                                      00055805
            MOVE WRK-VALOR TO WRK-VALOR-ED.                             00055905
            MOVE WRK-FRETE TO WRK-FRETE-ED.                             00056005
                                                                        00056105
           IF WRK-FLAG EQUAL 1                                          00056205
              DISPLAY ' CODIGO.....' WRK-CODIGO                         00056305
              DISPLAY ' PRODUTO....' WRK-PRODUTO                        00056405
              DISPLAY ' VALOR......' WRK-VALOR-ED                       00056505
              DISPLAY ' ESTADO.....' WRK-ESTADO                         00056605
              DISPLAY ' FRETE......' WRK-FRETE-ED                       00056705
           END-IF.                                                      00056805
                                                                        00056905
       2000-99-FINAL.            EXIT.                                  00057007
                                                                        00057107
      *----------------------------------------------------             00057206
       3000-FINALIZAR                           SECTION.                00057305
      *----------------------------------------------------             00057406
            DISPLAY ' FIM DO PROGRAMA'.                                 00058006
                                                                        00058204
       3000-99-FINAL.            EXIT.                                  00058307
