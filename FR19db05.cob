      *=============================================================*   00001001
       IDENTIFICATION                            DIVISION.              00002001
      *=============================================================*   00003001
                                                                        00004001
       PROGRAM-ID. FR19DB05.                                            00005001
                                                                        00006001
      *=============================================================*   00007001
      *   AUTOR....:RAFAEL VIANA                                    *   00007101
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007201
      *   DATA ....:07/06/2022                                      *   00007301
      *-------------------------------------------------------------*   00007401
      *   OBJETIVO:                                                 *   00007501
      *       UPDATES DE REGISTROS NA TABELA.                       *   00007612
      *                                                             *   00007701
      *                                                             *   00007801
      *-------------------------------------------------------------*   00007901
      *   BASE DE DADOS:                                            *   00008001
      *   TABELA.DB2..                                              *   00008101
      *    DDNAME              I/O                 INCLUDE/BOOK     *   00008201
      *   IVAN.FUNC             I                  #BKFUNC----      *   00008301
      *-------------------------------------------------------------*   00008401
      *   MODULOS....:                             INCLUDE/BOOK     *   00008501
      *=============================================================*   00008601
                                                                        00008701
      *=============================================================*   00008801
       ENVIRONMENT                               DIVISION.              00008901
      *=============================================================*   00009001
                                                                        00009101
      *=============================================================*   00009201
       CONFIGURATION                               SECTION.             00009301
      *=============================================================*   00009401
       SPECIAL-NAMES.                                                   00009501
           DECIMAL-POINT IS COMMA.                                      00009601
                                                                        00009701
      *=============================================================*   00009801
       DATA                                      DIVISION.              00009901
      *=============================================================*   00010001
      *=============================================================*   00020001
       WORKING-STORAGE                             SECTION.             00021001
      *=============================================================*   00021101
                                                                        00021201
           EXEC SQL                                                     00021301
              INCLUDE #BKFUNC                                           00021401
           END-EXEC.                                                    00021501
                                                                        00021601
           EXEC SQL                                                     00021701
              INCLUDE SQLCA                                             00021801
           END-EXEC.                                                    00021901
                                                                        00022001
       01 WRK-ID.                                                       00022105
          05 FILLER              PIC X(10).                             00022205
          05 WRK-ID-AC           PIC 9(05).                             00022305
                                                                        00022405
       01 WRK-NOME.                                                     00022505
          05 FILLER              PIC X(10).                             00022605
          05 WRK-NOME-AC         PIC X(30).                             00022705
                                                                        00022805
       01 WRK-SETOR.                                                    00022905
          05 FILLER              PIC X(10).                             00023005
          05 WRK-SETOR-AC        PIC X(04).                             00023105
                                                                        00023205
       01 WRK-SALARIO.                                                  00023305
          05 FILLER              PIC X(10).                             00023405
          05 WRK-SALARIO-AC      PIC 9(08)V99.                          00023505
                                                                        00023605
       01 WRK-DATAADM.                                                  00023705
          05 FILLER              PIC X(10).                             00023805
          05 WRK-DATAADM-AC      PIC X(10).                             00023905
                                                                        00024005
       01 WRK-EMAIL.                                                    00024105
          05 FILLER              PIC X(10).                             00024205
          05 WRK-EMAIL-AC        PIC X(40).                             00024310
                                                                        00024405
       77 WRK-SQLCODE         PIC -999.                                 00024601
       77 WRK-NULL-EMAIL      PIC S9(4) COMP.                           00025001
                                                                        00026001
      *=============================================================*   00026101
       PROCEDURE DIVISION.                                              00026201
      *=============================================================*   00026301
                                                                        00026401
      *-------------------------------------------------------------*   00026501
       0000-PRINCIPAL                           SECTION.                00026601
      *-------------------------------------------------------------*   00026701
                                                                        00026801
            PERFORM  1000-INICIAR.                                      00026901
             PERFORM 1100-TESTAR-ID.                                    00027026
            PERFORM  2000-PROCESSAR.                                    00027101
            PERFORM  3000-FINALIZAR.                                    00027201
            STOP RUN.                                                   00027301
                                                                        00027401
                                                                        00027501
      *-------------------------------------------------------------*   00027601
       1000-INICIAR                             SECTION.                00027701
      *-------------------------------------------------------------*   00027801
                                                                        00027901
            ACCEPT WRK-ID.                                              00028020
            ACCEPT WRK-NOME.                                            00028120
            ACCEPT WRK-SETOR.                                           00028220
            ACCEPT WRK-SALARIO.                                         00028320
            ACCEPT WRK-DATAADM.                                         00028420
            ACCEPT WRK-EMAIL.                                           00028520
                                                                        00028601
       1000-99-FIM.              EXIT.                                  00029001
                                                                        00029125
      *-------------------------------------------------------------*   00029225
       1100-TESTAR-ID                           SECTION.                00029326
      *-------------------------------------------------------------*   00029425
                                                                        00029526
             IF WRK-ID-AC NOT EQUAL 0                                   00029626
                          AND WRK-ID-AC IS NUMERIC                      00029726
               CONTINUE                                                 00029826
             ELSE                                                       00029926
              DISPLAY 'DIGITE UM ID NUMERICO OU EXISTENTE!'             00030026
               PERFORM 3000-FINALIZAR                                   00030126
                STOP RUN                                                00030226
             END-IF.                                                    00030326
                                                                        00030425
       1100-99-FIM.              EXIT.                                  00030526
      *-------------------------------------------------------------*   00030601
       2000-PROCESSAR                           SECTION.                00031001
      *-------------------------------------------------------------*   00031101
                                                                        00031201
            MOVE WRK-ID-AC TO DB2-ID.                                   00031321
                                                                        00031401
            EXEC SQL                                                    00031501
            SELECT ID,NOME,SETOR,SALARIO,DATAADM,EMAIL                  00031606
              INTO :DB2-ID,                                             00031906
                   :DB2-NOME,                                           00032006
                   :DB2-SETOR,                                          00032106
                   :DB2-SALARIO,                                        00032206
                   :DB2-DATAADM,                                        00032406
                   :DB2-EMAIL    :WRK-NULL-EMAIL                        00032506
               FROM  IVAN.FUNC                                          00032606
              WHERE ID = :DB2-ID                                        00032706
            END-EXEC.                                                   00032806
                                                                        00032913
             EVALUATE SQLCODE                                           00033032
              WHEN 0                                                    00033132
            IF WRK-NOME-AC NOT EQUAL DB2-NOME                           00033219
                     AND WRK-NOME-AC NOT EQUAL SPACES                   00033319
              MOVE WRK-NOME-AC TO DB2-NOME                              00033413
            END-IF                                                      00033532
            IF WRK-SETOR-AC NOT EQUAL DB2-SETOR                         00033619
                     AND WRK-SETOR-AC NOT EQUAL SPACES                  00033719
              MOVE WRK-SETOR-AC TO DB2-SETOR                            00033813
            END-IF                                                      00033932
            IF WRK-SALARIO-AC NOT EQUAL DB2-SALARIO                     00034019
                     AND WRK-SALARIO-AC IS NUMERIC                      00034119
              MOVE WRK-SALARIO-AC TO DB2-SALARIO                        00034213
            END-IF                                                      00034332
            IF WRK-DATAADM-AC NOT EQUAL DB2-DATAADM                     00034419
                     AND WRK-DATAADM-AC NOT EQUAL SPACES                00034519
              MOVE WRK-DATAADM-AC TO DB2-DATAADM                        00034613
            END-IF                                                      00034732
            IF WRK-NULL-EMAIL EQUAL 0                                   00034831
             IF WRK-EMAIL-AC NOT EQUAL DB2-EMAIL                        00034928
                     AND WRK-EMAIL-AC NOT EQUAL SPACES                  00035019
               MOVE WRK-EMAIL-AC TO DB2-EMAIL                           00035128
             END-IF                                                     00035231
            END-IF                                                      00035332
                                                                        00035431
              PERFORM 2100-UPDATES                                      00036114
              DISPLAY ' DADOS INSERIDOS ' DB2-ID                        00036226
              DISPLAY ' DADOS INSERIDOS ' DB2-NOME                      00036326
              DISPLAY ' DADOS INSERIDOS ' DB2-SETOR                     00036426
              DISPLAY ' DADOS INSERIDOS ' DB2-SALARIO                   00036526
              DISPLAY ' DADOS INSERIDOS ' DB2-DATAADM                   00036626
              DISPLAY ' DADOS INSERIDOS ' DB2-EMAIL                     00036726
              WHEN -181                                                 00036822
               DISPLAY 'DATA NO FORMATO ERRADO ' DB2-ID                 00036922
              WHEN OTHER                                                00037022
                MOVE SQLCODE TO WRK-SQLCODE                             00037122
                DISPLAY 'ERRO NA LEITURA ' WRK-SQLCODE                  00037222
              END-EVALUATE.                                             00037322
                                                                        00037411
                                                                        00037602
                                                                        00037702
       2000-99-FIM.              EXIT.                                  00037802
                                                                        00037902
      *-------------------------------------------------------------*   00038011
       2100-UPDATES                             SECTION.                00038112
      *-------------------------------------------------------------*   00038211
            IF WRK-NULL-EMAIL EQUAL 0                                   00038330
             EXEC SQL                                                   00040028
             UPDATE IVAN.FUNC                                           00040128
                   SET NOME    = :DB2-NOME,                             00040211
                       SETOR   = :DB2-SETOR,                            00040311
                       SALARIO = :DB2-SALARIO,                          00040411
                       DATAADM = :DB2-DATAADM,                          00040511
                       EMAIL   = :DB2-EMAIL                             00040611
                  WHERE ID     = :DB2-ID                                00040711
             END-EXEC                                                   00040828
            ELSE                                                        00040928
              EXEC SQL                                                  00041228
              UPDATE IVAN.FUNC                                          00041328
                   SET NOME    = :DB2-NOME,                             00041428
                       SETOR   = :DB2-SETOR,                            00041528
                       SALARIO = :DB2-SALARIO,                          00041628
                       DATAADM = :DB2-DATAADM,                          00041728
                       EMAIL   =  NULL                                  00041828
                  WHERE ID     = :DB2-ID                                00041928
              END-EXEC                                                  00042028
             END-IF.                                                    00042128
       2100-99-FIM.              EXIT.                                  00042211
      *-------------------------------------------------------------*   00042302
       3000-FINALIZAR                           SECTION.                00042402
      *-------------------------------------------------------------*   00042502
                                                                        00042601
              DISPLAY ' -----FIM DO PROGRAMA----- '.                    00042701
                                                                        00042801
       3000-99-FIM.              EXIT.                                  00042901
      *-------------------------------------------------------------*   00043001
       9000-TRATAERROS                              SECTION.            00043101
      *-------------------------------------------------------------*   00043201
                                                                        00044001
       9000-99-FIM.              EXIT.                                  00045001
