      *====================================================             00010000
       IDENTIFICATION                           DIVISION.               00020000
      *====================================================             00030000
       PROGRAM-ID. FR01CB13.                                            00040001
                                                                        00041000
      *====================================================             00042000
      *  AUTOR: RAFAEL                                                  00043000
      * EMPRESA: FOURSYS                                                00044000
      *====================================================             00045000
                                                                        00046000
      *====================================================             00047000
       ENVIRONMENT                              DIVISION.               00048000
      *====================================================             00049000
       CONFIGURATION                            SECTION.                00049122
       SPECIAL-NAMES.                                                   00049222
           DECIMAL-POINT IS COMMA.                                      00049322
                                                                        00050000
      *====================================================             00060000
       DATA                                     DIVISION.               00070000
      *====================================================             00080014
       WORKING-STORAGE                          SECTION.                00090000
       01 WRK-DADOS.                                                    00100000
          05 WRK-MES           PIC 9(02) VALUE ZEROS.                   00110019
          05 WRK-VENDA         PIC 9(08)V99 VALUE ZEROS.                00120022
          05 WRK-TOTFUNC       PIC 9(03)    VALUE ZEROS.                00121025
       77 WRK-MEDVENDA         PIC 9(09)V99 VALUE ZEROS.                00130022
       77 WRK-MEDVENFUNC       PIC 9(09)V99 VALUE ZEROS.                00130122
       77 WRK-MEDVENDA-ED      PIC ZZZ.ZZZ.ZZ9,99.                      00130223
       77 WRK-MEDVENFUNC-ED    PIC ZZZ.ZZZ.ZZ9,99.                      00130423
      *====================================================             00130500
       PROCEDURE                                DIVISION.               00130600
      *====================================================             00130700
                                                                        00130826
      *----------------------------------------------------             00130926
       0000-PRINCIPAL                           SECTION.                00131026
      *----------------------------------------------------             00131126
                                                                        00131226
             PERFORM 1000-INICIALIZAR.                                  00131326
             PERFORM 2000-PROCESSAR.                                    00131426
             PERFORM 3000-FINALIZAR.                                    00131526
            STOP RUN.                                                   00131626
                                                                        00131726
       0000-99-FIM.              EXIT.                                  00131826
                                                                        00131926
      *----------------------------------------------------             00132026
       1000-INICIALIZAR                         SECTION.                00132126
      *----------------------------------------------------             00132226
                                                                        00132326
           ACCEPT WRK-DADOS FROM SYSIN.                                 00132413
                                                                        00132526
       1000-99-FIM.              EXIT.                                  00132626
      *----------------------------------------------------             00132726
       2000-PROCESSAR                           SECTION.                00132826
      *----------------------------------------------------             00132926
           DISPLAY 'WRK-MES............' WRK-MES.                       00133017
           DISPLAY 'WRK-VENDA..........' WRK-VENDA.                     00133117
           DISPLAY 'WRK-TOTFUNC........' WRK-TOTFUNC.                   00133217
                                                                        00133313
            DIVIDE WRK-VENDA BY WRK-TOTFUNC GIVING WRK-MEDVENDA         00133413
              ON SIZE ERROR                                             00133513
                DISPLAY 'ERRO NA DIVISAO'                               00133613
              NOT ON SIZE ERROR                                         00134013
                MOVE WRK-MEDVENDA TO WRK-MEDVENDA-ED                    00134125
                DISPLAY 'MEDIA DE VENDA TOTAL..' WRK-MEDVENDA-ED        00135024
            DIVIDE WRK-MEDVENDA BY WRK-MES GIVING WRK-MEDVENFUNC        00135115
             MOVE WRK-MEDVENFUNC TO WRK-MEDVENFUNC-ED                   00135524
                DISPLAY 'MEDIA DE VENDA POR MES' WRK-MEDVENFUNC-ED      00135624
            END-DIVIDE.                                                 00136013
                                                                        00136127
       2000-99-FIM.              EXIT.                                  00136227
                                                                        00136327
      *----------------------------------------------------             00136427
       3000-FINALIZAR                           SECTION.                00136527
      *----------------------------------------------------             00136627
             DISPLAY ' FIM DO PROGRAMA'.                                00136727
                                                                        00136827
       3000-99-FIM.              EXIT.                                  00136927
