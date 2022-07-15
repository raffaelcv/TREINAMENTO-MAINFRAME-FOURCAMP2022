      *====================================================             00010000
       IDENTIFICATION                           DIVISION.               00020000
      *====================================================             00030000
       PROGRAM-ID. FR01CB24.                                            00040001
                                                                        00041000
      *====================================================             00042000
      *  AUTOR: RAFAEL                                                  00043000
      * EMPRESA: FOURSYS                                                00044000
      *====================================================             00045000
      *                                                                 00045100
      * OBJETIVO: APLICAR CONHECIMENTO DO COMANDO UNTIL.                00045200
      *           RECEBER LAYOUT E SOMAR QUANTOS REGISTROS              00045325
      *           SERAO LIDOS E SOMA DOS VALORES AO FINAL.              00045425
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
       WORKING-STORAGE                          SECTION.                00046700
                                                                        00046800
       01 WRK-DADOS.                                                    00046900
           05 WRK-CODIGO              PIC 9(03)    VALUE ZEROES.        00047000
           05 WRK-ITEM                PIC X(30)    VALUE SPACES.        00047100
           05 WRK-VALOR               PIC 9(05)V99 VALUE ZEROES.        00048000
           05 WRK-QUANT               PIC 9(03)    VALUE ZEROES.        00049000
                                                                        00049123
      *-----------------ACUMULADAS-------------------------             00049223
                                                                        00049300
       77 WRK-VALORTOTAL              PIC 9(08)V99 VALUE ZEROES.        00049402
       77 WRK-REGLIDOS                PIC 9(02)    VALUE 0.             00049524
       77 WRK-VALORACUM               PIC 9(08)V99 VALUE ZEROES.        00049613
                                                                        00049723
      *------------------MASCARAS--------------------------             00049823
                                                                        00049900
       77 WRK-VALOR-ED                PIC ZZ.ZZ9,99.                    00050002
       77 WRK-VALORTOTAL-ED           PIC ZZ.ZZZ.ZZ9,99.                00050104
       77 WRK-VALORACUM-ED            PIC ZZ.ZZZ.ZZ9,99.                00050213
                                                                        00050300
      *====================================================             00050400
       PROCEDURE                                DIVISION.               00050500
      *====================================================             00050623
                                                                        00050700
      *----------------------------------------------------             00050800
       0000-PRINCIPAL                           SECTION.                00050900
      *----------------------------------------------------             00051000
                                                                        00051100
             PERFORM 1000-INICIALIZAR.                                  00051223
             IF WRK-CODIGO NOT EQUAL ZEROS                              00051323
               PERFORM 2000-PROCESSAR UNTIL WRK-CODIGO EQUAL 999        00051523
             END-IF.                                                    00051623
             PERFORM 3000-FINALIZAR.                                    00051719
             STOP RUN.                                                  00051800
                                                                        00051900
       0000-99-FIM.       EXIT.                                         00052000
                                                                        00052100
      *----------------------------------------------------             00052200
       1000-INICIALIZAR                        SECTION.                 00052300
      *----------------------------------------------------             00052400
                                                                        00052500
           ACCEPT WRK-DADOS     FROM SYSIN.                             00052600
                                                                        00052700
       1000-99-FIM.       EXIT.                                         00052800
                                                                        00052900
      *----------------------------------------------------             00053000
       2000-PROCESSAR                          SECTION.                 00053100
      *----------------------------------------------------             00053200
                                                                        00053303
            DISPLAY ' CODIGO PRODUTO....' WRK-CODIGO.                   00053403
            DISPLAY ' ITEM .............' WRK-ITEM.                     00053503
            DISPLAY ' QUANTIDADE........' WRK-QUANT.                    00053603
            MOVE WRK-VALOR TO WRK-VALOR-ED.                             00053703
            DISPLAY ' VALOR...........R$' WRK-VALOR-ED.                 00053805
            COMPUTE WRK-VALORTOTAL = WRK-VALOR * WRK-QUANT.             00053905
             ADD WRK-VALORTOTAL TO WRK-VALORACUM.                       00054013
              MOVE WRK-VALORTOTAL TO WRK-VALORTOTAL-ED.                 00054114
               DISPLAY ' VALOR TOTAL ..R$' WRK-VALORTOTAL-ED.           00054214
                ADD 1 TO WRK-REGLIDOS.                                  00054324
                 DISPLAY ' -------------------------------------- '.    00054414
                  ACCEPT WRK-DADOS FROM SYSIN.                          00054514
                                                                        00054609
       2000-99-FIM.       EXIT.                                         00054700
                                                                        00054800
      *----------------------------------------------------             00054900
       3000-FINALIZAR                          SECTION.                 00055000
      *----------------------------------------------------             00055100
            MOVE WRK-VALORACUM TO WRK-VALORACUM-ED.                     00055213
                                                                        00055310
            DISPLAY ' VALOR TOTAL ACUMULADO R$' WRK-VALORACUM-ED.       00055413
            DISPLAY ' REGISTROS LIDOS ........' WRK-REGLIDOS.           00055524
                                                                        00055607
       3000-99-FIM.       EXIT.                                         00055700
                                                                        00055800
      *----------------------------------------------------             00055900
       9000-TRATAR-ERROS                       SECTION.                 00056000
      *----------------------------------------------------             00056100
                                                                        00056200
      * DISPLAY ' ERRO NOS VALORES. '.                                  00056320
                                                                        00056416
       9000-99-FIM.       EXIT.                                         00057000
