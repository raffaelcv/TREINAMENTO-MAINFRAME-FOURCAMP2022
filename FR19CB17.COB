      *====================================================             00010000
       IDENTIFICATION                           DIVISION.               00020000
      *====================================================             00030000
       PROGRAM-ID. FR01CB17.                                            00040000
                                                                        00041000
      *====================================================             00042000
      *  AUTOR: RAFAEL                                                  00043000
      * EMPRESA: FOURSYS                                                00044000
      *====================================================             00045000
      *                                                                 00045100
      * CALCULANDO VALOR PARA ABASTECIMENTO                             00045200
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
          05 WRK-GASOLINA      PIC 9(02)V99 VALUE ZEROS.                00049700
          05 WRK-ALCOOL        PIC 9(02)V99 VALUE ZEROS.                00049800
       77 WRK-MEDIA            PIC 9(03)V99 VALUE ZEROS.                00050206
                                                                        00050300
      *=============VARIAVEIS=EDITADAS=====================             00050400
                                                                        00050500
       77 WRK-MEDIA-ED         PIC ZZ9,99.                              00050600
       77 WRK-GASOLINA-ED      PIC Z9,99.                               00050700
       77 WRK-ALCOOL-ED        PIC Z9,99.                               00051014
                                                                        00052000
      *====================================================             00060000
       PROCEDURE                                DIVISION.               00070000
      *====================================================             00080000
                                                                        00081022
      *----------------------------------------------------             00082022
       0000-PRINCIPAL                           SECTION.                00083022
      *----------------------------------------------------             00084022
             PERFORM 1000-INICIALIZAR.                                  00085022
             PERFORM 2000-PROCESSAR.                                    00086022
             PERFORM 3000-FINALIZAR.                                    00087022
             STOP RUN.                                                  00087122
                                                                        00088022
       0000-99-FINAL.          EXIT.                                    00088123
                                                                        00088223
      *----------------------------------------------------             00089022
       1000-INICIALIZAR                         SECTION.                00089122
      *----------------------------------------------------             00089222
                                                                        00089322
           ACCEPT   WRK-DADOS  FROM SYSIN.                              00090000
                                                                        00090122
       1000-99-FINAL.          EXIT.                                    00090223
                                                                        00090323
      *----------------------------------------------------             00090422
       2000-PROCESSAR                           SECTION.                00090522
      *----------------------------------------------------             00090622
                                                                        00090722
            IF WRK-GASOLINA IS NUMERIC AND WRK-ALCOOL IS NUMERIC        00090819
               MOVE WRK-GASOLINA TO WRK-GASOLINA-ED                     00090920
               MOVE WRK-ALCOOL TO WRK-ALCOOL-ED                         00091020
                DISPLAY ' VALOR ALCOOL....R$  ' WRK-ALCOOL-ED           00091120
                DISPLAY ' VALOR GASOLINA..R$  ' WRK-GASOLINA-ED         00091220
              COMPUTE WRK-MEDIA = WRK-ALCOOL / WRK-GASOLINA * 100       00091310
               MOVE WRK-MEDIA TO WRK-MEDIA-ED                           00091410
                DISPLAY ' RELACAO PERCENTUAL% ' WRK-MEDIA-ED            00100215
                                                                        00100407
              IF WRK-MEDIA GREATER THAN OR EQUAL 070                    00101007
                DISPLAY 'ABASTECA GASOLINA'                             00102021
              ELSE                                                      00103000
                DISPLAY 'ABASTECA ALCOOL  '                             00104021
              END-IF                                                    00105010
                                                                        00105116
            ELSE                                                        00106010
              DISPLAY 'WRK-GASOLINA....' WRK-GASOLINA                   00106119
              DISPLAY 'WRK-ALCOOL......' WRK-ALCOOL                     00106219
              DISPLAY 'DADOS INCORRETOS, INSIRA SOMENTE NUMEROS!!'      00107012
            END-IF.                                                     00108010
                                                                        00108122
       2000-99-FINAL.          EXIT.                                    00108223
                                                                        00108323
      *-----------------------------------------------------            00109022
       3000-FINALIZAR                           SECTION.                00110022
      *-----------------------------------------------------            00120022
                                                                        00121022
             DISPLAY ' FIM DO PROGRAMA'.                                00130022
                                                                        00140023
       3000-99-FINAL.          EXIT.                                    00150023
