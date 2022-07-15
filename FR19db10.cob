      *===============================================================* 00010008
       IDENTIFICATION                                        DIVISION.  00020008
      *===============================================================* 00030008
       PROGRAM-ID. FR19DB10.                                            00040019
      *===============================================================* 00061019
      *   AUTOR....:RAFAEL VIANA                                      * 00062019
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR        * 00063019
      *   DATA ....:20/06/2022                                        * 00064025
      *---------------------------------------------------------------* 00065019
      *   OBJETIVO:SYNCPOINT E RESTART.                               * 00066019
      *                                                               * 00067019
      *                                                               * 00068019
      *                                                               * 00069019
      *---------------------------------------------------------------* 00069119
      *   BASE DE DADOS:                                              * 00069219
      *   TABELA.DB2..                                                * 00069319
      *    DDNAME              I/O                 INCLUDE/BOOK       * 00069419
      *   IVAN.FUNC             I                  #BKFUNC----        * 00080019
      *   IVAN.CHECKP           I                  #BKCHECK---        * 00090023
      *---------------------------------------------------------------* 00130019
      *   MODULOS....:                             INCLUDE/BOOK       * 00131019
      *===============================================================* 00132019
      *---------------------------------------------------------------* 00137115
       DATA                                                   DIVISION. 00137217
      *---------------------------------------------------------------* 00137317
      *---------------------------------------------------------------* 00137417
       WORKING-STORAGE                                        SECTION.  00138009
      *---------------------------------------------------------------* 00138108
                                                                        00138208
           EXEC SQL                                                     00139008
              INCLUDE #BKFUNC                                           00139108
           END-EXEC.                                                    00139208
                                                                        00139318
           EXEC SQL                                                     00139418
              INCLUDE #BKCHECK                                          00139518
           END-EXEC.                                                    00139618
                                                                        00139718
           EXEC SQL                                                     00139818
              INCLUDE SQLCA                                             00139918
           END-EXEC.                                                    00140018
                                                                        00140118
           EXEC SQL                                                     00140218
              DECLARE CFUNC CURSOR FOR                                  00140318
               SELECT ID,NOME,SETOR,SALARIO,DATAADM,EMAIL               00140418
               FROM IVAN.FUNC F  WHERE F.ID >=                          00140518
                (SELECT REGISTRO FROM IVAN.CHECKP C                     00140618
                   WHERE C.ID = 'FOUR019')                              00140719
                ORDER BY ID                                             00140818
           END-EXEC.                                                    00140918
                                                                        00141018
      *----------------VARIAVEIS DE APOIO------------------             00141118
       77 WRK-ID                        PIC 9(05) VALUE ZEROS.          00141218
       77 WRK-SQLCODE                   PIC -999.                       00142008
       77 WRK-NULL-EMAIL                PIC S9(4) COMP.                 00150008
                                                                        00152910
      *===============================================================* 00153009
       PROCEDURE                                             DIVISION.  00154009
      *===============================================================* 00155009
      *---------------------------------------------------------------* 00156009
       0000-PRINCIPAL                                         SECTION.  00157009
      *---------------------------------------------------------------* 00157109
                                                                        00157208
            PERFORM 1000-INICIALIZAR.                                   00157308
             PERFORM 2000-PROCESSAR UNTIL SQLCODE EQUAL 100.            00157508
              PERFORM 2900-ZERACHECK.                                   00157718
              PERFORM 3000-FINALIZAR.                                   00157908
               STOP RUN.                                                00158008
                                                                        00158108
       0000-99-FIM.                                              EXIT.  00158309
      *---------------------------------------------------------------* 00158809
       1000-INICIALIZAR                                       SECTION.  00158909
      *---------------------------------------------------------------* 00159009
                                                                        00159108
            EXEC SQL                                                    00159208
               OPEN CFUNC                                               00159308
            END-EXEC.                                                   00159408
             EVALUATE SQLCODE                                           00159508
              WHEN 0                                                    00159608
                PERFORM 4000-LER-FUNCIONARIO                            00159708
              WHEN 100                                                  00159808
                DISPLAY 'SEM FUNCIONARIOS'                              00159908
              WHEN OTHER                                                00160008
                MOVE SQLCODE TO WRK-SQLCODE                             00160108
                DISPLAY 'ERRO ' WRK-SQLCODE ' NO OPEN CURSOR'           00160208
                STOP RUN                                                00160308
             END-EVALUATE.                                              00160408
                                                                        00160509
       1000-99-FIM.                                              EXIT.  00160709
      *---------------------------------------------------------------* 00164909
       2000-PROCESSAR                                         SECTION.  00165009
      *---------------------------------------------------------------* 00165109
             IF DB2-SALARIO EQUAL ZEROES                                00165224
                DISPLAY 'ERRO NO REGISTRO ' DB2-ID                      00165318
               EXEC SQL                                                 00165818
                UPDATE IVAN.CHECKP SET REGISTRO = :DB2-ID               00165918
                   WHERE ID = 'FOUR019'                                 00166019
               END-EXEC                                                 00166118
                  PERFORM 3000-FINALIZAR                                00166218
                  GOBACK                                                00166318
             END-IF.                                                    00166418
                                                                        00166518
                                                                        00166618
               DISPLAY '-----------------------------'                  00166721
               DISPLAY 'ID      : ' DB2-ID                              00166818
               DISPLAY 'NOME    : ' DB2-NOME                            00166918
               DISPLAY 'SETOR   : ' DB2-SETOR                           00167018
               DISPLAY 'SALARIO : ' DB2-SALARIO                         00167118
               DISPLAY 'DATAADM : ' DB2-DATAADM                         00167218
             IF WRK-NULL-EMAIL = 0                                      00167312
               DISPLAY 'EMAIL   : ' DB2-EMAIL                           00167425
               DISPLAY ' '                                              00168025
             ELSE                                                       00168125
               DISPLAY 'EMAIL NULO'                                     00168225
               DISPLAY ' '                                              00168325
             END-IF.                                                    00168425
                  PERFORM 4000-LER-FUNCIONARIO.                         00168525
                                                                        00168608
       2000-99-FIM.                                              EXIT.  00168809
      *---------------------------------------------------------------* 00169018
       2900-ZERACHECK                                        SECTION.   00169118
            EXEC SQL                                                    00169218
             UPDATE IVAN.CHECKP SET REGISTRO = 0                        00169318
             WHERE ID = 'FOUR019'                                       00169419
            END-EXEC.                                                   00169518
                                                                        00169618
       2900-99-FIM.  EXIT.                                              00169718
      *---------------------------------------------------------------* 00169812
       3000-FINALIZAR                                         SECTION.  00169912
      *---------------------------------------------------------------* 00170012
                                                                        00170112
            EXEC SQL                                                    00170212
              CLOSE CFUNC                                               00170312
            END-EXEC.                                                   00170412
                                                                        00170512
       3000-99-FIM.                                              EXIT.  00170712
      *---------------------------------------------------------------* 00171218
       4000-LER-FUNCIONARIO                                   SECTION.  00171318
      *---------------------------------------------------------------* 00171418
                                                                        00171518
            EXEC SQL                                                    00171618
             FETCH CFUNC                                                00171718
              INTO :DB2-ID,                                             00171818
                   :DB2-NOME,                                           00171918
                   :DB2-SETOR,                                          00172018
                   :DB2-SALARIO,                                        00172118
                   :DB2-DATAADM,                                        00172218
                   :DB2-EMAIL    :WRK-NULL-EMAIL                        00172318
            END-EXEC.                                                   00172418
                                                                        00172518
            EVALUATE SQLCODE                                            00172618
             WHEN 0                                                     00172718
              CONTINUE                                                  00172818
             WHEN 100                                                   00172918
                                                                        00173018
                                                                        00173118
                DISPLAY ' '                                             00173210
                DISPLAY 'FIM DA TABELA ' DB2-ID                         00173312
             WHEN OTHER                                                 00173410
                MOVE SQLCODE TO  WRK-SQLCODE                            00173510
                DISPLAY ' '                                             00173610
                DISPLAY 'ERRO NA LEITURA' WRK-SQLCODE                   00173710
                STOP RUN                                                00173812
            END-EVALUATE.                                               00173910
                                                                        00174010
       4000-99-FIM.                                              EXIT.  00174210
