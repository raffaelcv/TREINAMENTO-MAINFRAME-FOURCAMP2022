      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR19DB07.                                            00005000
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:RAFAEL VIANA                                    *   00007100
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007200
      *   DATA ....:07/06/2022                                      *   00007300
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO:LER FUNCIONARIOS COM SETOR ANALITICO USANDO      *   00007501
      *          TABELAS IVAN.FUNC E IVAN.SETOR.                    *   00007601
      *                                                             *   00007701
      *                                                             *   00007801
      *-------------------------------------------------------------*   00007900
      *   BASE DE DADOS:                                            *   00008000
      *   TABELA.DB2..                                              *   00008100
      *    ------                                  INCLUDE/BOOK     *   00008201
      *   IVAN.FUNC                                #BKFUNC----      *   00008301
      *   IVAN.SETOR                               #BKSETOR---      *   00008401
      *-------------------------------------------------------------*   00008501
      *   MODULOS....:                             INCLUDE/BOOK     *   00008601
      *=============================================================*   00008701
                                                                        00008801
      *=============================================================*   00008901
       ENVIRONMENT                               DIVISION.              00009001
      *=============================================================*   00009101
                                                                        00009201
      *=============================================================*   00009301
       CONFIGURATION                               SECTION.             00009401
      *=============================================================*   00009501
       SPECIAL-NAMES.                                                   00009601
           DECIMAL-POINT IS COMMA.                                      00009701
                                                                        00009801
       INPUT-OUTPUT                                 SECTION.            00009910
       FILE-CONTROL.                                                    00010010
            SELECT RELSETOR ASSIGN TO RELSETOR                          00010110
                FILE STATUS IS WRK-FS-RELSETOR.                         00010210
                                                                        00010310
      *=============================================================*   00010401
       DATA                                      DIVISION.              00010501
      *=============================================================*   00010601
       FILE                                        SECTION.             00010707
       FD RELSETOR                                                      00010807
           RECORDING MODE IS F                                          00010907
           LABEL RECORD IS STANDARD                                     00011007
           BLOCK CONTAINS 0 RECORDS.                                    00011107
      *-----------------------LRECL 135-----------------------------*   00011207
       01 FD-RELSETOR         PIC X(135).                               00011307
                                                                        00011407
      *=============================================================*   00011501
       WORKING-STORAGE                             SECTION.             00011601
      *=============================================================*   00011701
                                                                        00011801
           EXEC SQL                                                     00011901
              INCLUDE #BKFUNC                                           00012001
           END-EXEC.                                                    00012101
                                                                        00012201
           EXEC SQL                                                     00012301
              INCLUDE #BKSETOR                                          00012401
           END-EXEC.                                                    00012501
                                                                        00012601
           EXEC SQL                                                     00012701
              INCLUDE SQLCA                                             00012801
           END-EXEC.                                                    00012900
                                                                        00013000
           EXEC SQL                                                     00014000
              DECLARE CFUNC CURSOR FOR                                  00015000
               SELECT ID,NOME,SALARIO,DATAADM,EMAIL,DESCSETOR           00016000
                FROM IVAN.FUNC F , IVAN.SETOR S                         00016100
                 WHERE F.SETOR = S.IDSETOR                              00016200
      *         ORDER BY ID                                             00017000
           END-EXEC.                                                    00018000
      *--------------LRECL 135---------------------------               00019005
       01 WRK-DADOS.                                                    00019103
          05 WRK-MID          PIC 9(05).                                00019204
          05 WRK-NOME         PIC X(30).                                00019302
          05 WRK-SALARIO      PIC 9(10).                                00019402
          05 WRK-DATAADM      PIC X(10).                                00019502
          05 WRK-EMAIL        PIC X(40).                                00019602
          05 WRK-DESCSETOR    PIC X(40).                                00019702
                                                                        00019801
       77 WRK-REGLIDOS        PIC 9(03).                                00020000
       77 WRK-MAIOR-VALOR     PIC S9(8)V9(2) COMP.                      00021000
       77 WRK-SAL-ACUM        PIC S9(8)V9(2) COMP.                      00022000
       77 WRK-MEDIA-SAL       PIC S9(8)V9(2) COMP.                      00023000
       77 WRK-ID              PIC 9(05) VALUE ZEROES.                   00024000
       77 WRK-SQLCODE         PIC -999.                                 00024100
       77 WRK-NULL-EMAIL      PIC S9(4) COMP.                           00024200
       77 WRK-FS-RELSETOR     PIC 9(02).                                00024308
                                                                        00024408
      *=============================================================*   00024508
       PROCEDURE DIVISION.                                              00024608
      *=============================================================*   00024708
                                                                        00024808
      *-------------------------------------------------------------*   00024908
       0000-PRINCIPAL                           SECTION.                00025008
      *-------------------------------------------------------------*   00025108
                                                                        00026000
            PERFORM  1000-INICIAR.                                      00026100
            PERFORM  2000-PROCESSAR UNTIL SQLCODE EQUAL 100.            00026200
            PERFORM  3000-FINALIZAR.                                    00026300
            STOP RUN.                                                   00026400
                                                                        00026500
                                                                        00026600
      *-------------------------------------------------------------*   00026700
       1000-INICIAR                             SECTION.                00026800
      *-------------------------------------------------------------*   00026900
            EXEC SQL                                                    00027000
               OPEN CFUNC                                               00027100
            END-EXEC.                                                   00027200
             EVALUATE SQLCODE                                           00027300
              WHEN 0                                                    00027400
                PERFORM 4000-LER-FUNCIONARIO                            00027500
              WHEN 100                                                  00027600
                DISPLAY 'SEM FUNCIONARIO'                               00027700
              WHEN OTHER                                                00027800
                MOVE SQLCODE TO WRK-SQLCODE                             00027900
                DISPLAY 'ERRO ' WRK-SQLCODE ' NO OPEN DO CURSOR.'       00028000
                MOVE 200 TO RETURN-CODE                                 00029000
                STOP RUN                                                00029100
             END-EVALUATE.                                              00029200
             OPEN OUTPUT RELSETOR.                                      00029311
             PERFORM 1100-TESTAR-STATUS.                                00029408
       1000-99-FIM.              EXIT.                                  00029508
      *-------------------------------------------------------------*   00029608
       1100-TESTAR-STATUS                       SECTION.                00029708
      *-------------------------------------------------------------*   00029808
            IF WRK-FS-RELSETOR NOT EQUAL 0                              00029908
               DISPLAY ' ERRO NA ABERTURA DO ARQUIVO'                   00030008
                  STOP RUN                                              00030108
            END-IF.                                                     00030208
                                                                        00030308
       1100-99-FIM.              EXIT.                                  00030408
                                                                        00030508
      *-------------------------------------------------------------*   00030600
       2000-PROCESSAR                           SECTION.                00030700
      *-------------------------------------------------------------*   00030800
                                                                        00030900
             INITIALIZE WRK-DADOS.                                      00031009
              MOVE DB2-ID TO WRK-MID.                                   00031104
              MOVE DB2-NOME TO WRK-NOME.                                00031202
              MOVE DB2-SALARIO TO WRK-SALARIO.                          00031302
              MOVE DB2-DATAADM TO WRK-DATAADM.                          00031402
              IF WRK-NULL-EMAIL = 0                                     00031502
                MOVE DB2-EMAIL TO WRK-EMAIL                             00031602
              ELSE                                                      00031702
                MOVE 'SEM EMAIL ' TO WRK-EMAIL                          00031802
              END-IF.                                                   00031902
              MOVE DB2-DESCSETOR TO WRK-DESCSETOR.                      00032002
              WRITE FD-RELSETOR FROM WRK-DADOS.                         00033008
                                                                        00034900
              PERFORM 4000-LER-FUNCIONARIO.                             00035000
                                                                        00035100
       2000-99-FIM.              EXIT.                                  00035200
                                                                        00036000
      *-------------------------------------------------------------*   00037000
       3000-FINALIZAR                               SECTION.            00038000
      *-------------------------------------------------------------*   00039000
                                                                        00039100
              EXEC SQL                                                  00039200
                CLOSE CFUNC                                             00039300
              END-EXEC.                                                 00039400
              CLOSE RELSETOR.                                           00039508
              DISPLAY ' -----FIM DO PROGRAMA----- '.                    00039600
              DISPLAY ' REGISTROS LIDOS.......' WRK-REGLIDOS.           00039700
              DISPLAY ' MAIOR SALARIO.........' WRK-MAIOR-VALOR.        00039800
              DISPLAY ' SALARIO ACUMULADO.....' WRK-SAL-ACUM.           00039900
             DIVIDE WRK-SAL-ACUM BY WRK-REGLIDOS                        00040000
                                 GIVING WRK-MEDIA-SAL.                  00040100
              DISPLAY ' MEDIA DOS SALARIOS....' WRK-MEDIA-SAL.          00041000
                                                                        00041100
       3000-99-FIM.              EXIT.                                  00041200
      *-------------------------------------------------------------*   00041300
       4000-LER-FUNCIONARIO                         SECTION.            00041400
      *-------------------------------------------------------------*   00041500
                                                                        00041600
           EXEC SQL                                                     00041700
            FETCH CFUNC                                                 00041800
             INTO :DB2-ID,                                              00041900
                  :DB2-NOME,                                            00042000
                  :DB2-SALARIO,                                         00042200
                  :DB2-DATAADM,                                         00042300
                  :DB2-EMAIL     :WRK-NULL-EMAIL,                       00042400
                  :DB2-DESCSETOR                                        00042500
            END-EXEC.                                                   00042600
            EVALUATE SQLCODE                                            00042700
             WHEN 0                                                     00042800
               ADD 1 TO WRK-REGLIDOS                                    00042900
                ADD DB2-SALARIO TO WRK-SAL-ACUM                         00043000
               CONTINUE                                                 00043100
             WHEN 100                                                   00043200
              DISPLAY ' FINAL DE ARQUIVO'                               00043300
             WHEN OTHER                                                 00043400
               MOVE SQLCODE TO WRK-SQLCODE                              00043500
               DISPLAY 'ERRO NA LEITURA ' WRK-SQLCODE                   00043600
             END-EVALUATE.                                              00043700
             PERFORM 4100-CALCULAR-MAIOR-SALARIO.                       00043800
       4000-99-FIM.              EXIT.                                  00043900
      *-------------------------------------------------------------*   00044000
       4100-CALCULAR-MAIOR-SALARIO                  SECTION.            00044100
      *-------------------------------------------------------------*   00044200
                                                                        00044300
             IF DB2-SALARIO IS GREATER WRK-MAIOR-VALOR                  00044400
              MOVE DB2-SALARIO TO WRK-MAIOR-VALOR                       00044500
             END-IF.                                                    00044600
                                                                        00044700
       4100-99-FIM.              EXIT.                                  00044800
      *-------------------------------------------------------------*   00044900
       9000-TRATAERROS                              SECTION.            00045000
      *-------------------------------------------------------------*   00045100
                                                                        00046000
       9000-99-FIM.              EXIT.                                  00047000
