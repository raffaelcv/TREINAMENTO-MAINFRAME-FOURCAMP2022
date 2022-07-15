      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR19DB02.                                            00005000
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:RAFAEL VIANA                                    *   00007100
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007200
      *   DATA ....:07/06/2022                                      *   00007332
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO:LEITURA DE TODOS OS REGISTROS DE UMA TABELA,     *   00007515
      *   EXIBIR TOTAL DE REGISTROS LIDOS, CALCULAR ACUMULADO DOS   *   00007615
      *   SALARIOS, CALCULAR A MEDIA DOS SALARIOS, E INFORMAR O     *   00007723
      *            MAIOR SALARIO.                                   *   00007823
      *-------------------------------------------------------------*   00007900
      *   BASE DE DADOS:                                            *   00008026
      *   TABELA.DB2..                                              *   00008126
      *    ------              I/O                 INCLUDE/BOOK     *   00008231
      *   IVAN.FUNC             I                  #BKFUNC----      *   00008326
      *-------------------------------------------------------------*   00008426
      *   MODULOS....:                             INCLUDE/BOOK     *   00008526
      *=============================================================*   00008626
                                                                        00008726
      *=============================================================*   00008826
       ENVIRONMENT                               DIVISION.              00008926
      *=============================================================*   00009026
                                                                        00009126
      *=============================================================*   00009226
       CONFIGURATION                               SECTION.             00009326
      *=============================================================*   00009426
       SPECIAL-NAMES.                                                   00009526
           DECIMAL-POINT IS COMMA.                                      00009626
                                                                        00009726
      *=============================================================*   00009826
       DATA                                      DIVISION.              00009926
      *=============================================================*   00010026
      *=============================================================*   00010126
       WORKING-STORAGE                             SECTION.             00011000
      *=============================================================*   00020000
                                                                        00021000
           EXEC SQL                                                     00022000
              INCLUDE #BKFUNC                                           00023000
           END-EXEC.                                                    00023100
                                                                        00023200
           EXEC SQL                                                     00023300
              INCLUDE SQLCA                                             00023400
           END-EXEC.                                                    00023500
                                                                        00023600
           EXEC SQL                                                     00023700
              DECLARE CFUNC CURSOR FOR                                  00023800
               SELECT * FROM IVAN.FUNC                                  00023901
                ORDER BY ID                                             00024018
           END-EXEC.                                                    00024100
                                                                        00024200
       77 WRK-REGLIDOS        PIC 9(03).                                00024305
       77 WRK-MAIOR-VALOR     PIC S9(8)V9(2) COMP.                      00024419
       77 WRK-SAL-ACUM        PIC S9(8)V9(2) COMP.                      00024518
       77 WRK-MEDIA-SAL       PIC S9(8)V9(2) COMP.                      00024618
       77 WRK-ID              PIC 9(05) VALUE ZEROES.                   00024718
       77 WRK-SQLCODE         PIC -999.                                 00024818
       77 WRK-NULL-EMAIL      PIC S9(4) COMP.                           00024918
                                                                        00025018
      *=============================================================*   00025118
       PROCEDURE DIVISION.                                              00026000
      *=============================================================*   00026100
                                                                        00026200
      *-------------------------------------------------------------*   00026300
       0000-PRINCIPAL                           SECTION.                00026400
      *-------------------------------------------------------------*   00026500
                                                                        00026600
            PERFORM  1000-INICIAR.                                      00026700
            PERFORM  2000-PROCESSAR UNTIL SQLCODE EQUAL 100.            00026802
            PERFORM  3000-FINALIZAR.                                    00026900
            STOP RUN.                                                   00027000
                                                                        00027100
                                                                        00027200
      *-------------------------------------------------------------*   00027300
       1000-INICIAR                             SECTION.                00027400
      *-------------------------------------------------------------*   00027500
            EXEC SQL                                                    00027600
               OPEN CFUNC                                               00027800
            END-EXEC.                                                   00027900
             EVALUATE SQLCODE                                           00028000
              WHEN 0                                                    00028100
                PERFORM 4000-LER-FUNCIONARIO                            00028400
              WHEN 100                                                  00028500
                DISPLAY 'SEM FUNCIONARIO'                               00028600
              WHEN OTHER                                                00028700
                MOVE SQLCODE TO WRK-SQLCODE                             00028800
                DISPLAY 'ERRO ' WRK-SQLCODE ' NO OPEN DO CURSOR.'       00028900
                MOVE 200 TO RETURN-CODE                                 00029000
                STOP RUN                                                00029100
             END-EVALUATE.                                              00029200
                                                                        00029300
       1000-99-FIM.              EXIT.                                  00029400
      *-------------------------------------------------------------*   00029500
       2000-PROCESSAR                           SECTION.                00029600
      *-------------------------------------------------------------*   00030000
                                                                        00031000
                                                                        00034000
              DISPLAY '------------------'                              00034103
              DISPLAY 'ID..... ' DB2-ID                                 00034203
              DISPLAY 'NOME... ' DB2-NOME                               00034303
              DISPLAY 'SETOR.. ' DB2-SETOR                              00034403
              DISPLAY 'SALARIO ' DB2-SALARIO                            00034503
              DISPLAY 'DATAADM ' DB2-DATAADM                            00034603
             IF WRK-NULL-EMAIL = 0                                      00034703
               DISPLAY 'EMAIL. ' DB2-EMAIL                              00034803
             ELSE                                                       00034903
               DISPLAY '--SEM EMAIL '                                   00035003
             END-IF                                                     00035103
                                                                        00035303
              PERFORM 4000-LER-FUNCIONARIO.                             00035403
                                                                        00035503
       2000-99-FIM.              EXIT.                                  00036002
                                                                        00037000
      *-------------------------------------------------------------*   00038000
       3000-FINALIZAR                               SECTION.            00039000
      *-------------------------------------------------------------*   00039100
                                                                        00039200
              EXEC SQL                                                  00039600
                CLOSE CFUNC                                             00039700
              END-EXEC.                                                 00039800
              DISPLAY ' -----FIM DO PROGRAMA----- '.                    00040400
              DISPLAY ' REGISTROS LIDOS.......' WRK-REGLIDOS.           00040505
              DISPLAY ' MAIOR SALARIO.........' WRK-MAIOR-VALOR.        00040619
              DISPLAY ' SALARIO ACUMULADO.....' WRK-SAL-ACUM.           00040719
             DIVIDE WRK-SAL-ACUM BY WRK-REGLIDOS                        00040819
                                 GIVING WRK-MEDIA-SAL.                  00040919
              DISPLAY ' MEDIA DOS SALARIOS....' WRK-MEDIA-SAL.          00041019
                                                                        00041119
       3000-99-FIM.              EXIT.                                  00041219
      *-------------------------------------------------------------*   00041319
       4000-LER-FUNCIONARIO                         SECTION.            00041419
      *-------------------------------------------------------------*   00041519
                                                                        00041619
           EXEC SQL                                                     00041719
            FETCH CFUNC                                                 00041819
             INTO :DB2-ID,                                              00041919
                  :DB2-NOME,                                            00042019
                  :DB2-SETOR,                                           00042119
                  :DB2-SALARIO,                                         00042219
                  :DB2-DATAADM,                                         00042319
                  :DB2-EMAIL     :WRK-NULL-EMAIL                        00042419
            END-EXEC.                                                   00042519
            EVALUATE SQLCODE                                            00042619
             WHEN 0                                                     00042719
               ADD 1 TO WRK-REGLIDOS                                    00042819
                ADD DB2-SALARIO TO WRK-SAL-ACUM                         00042919
               CONTINUE                                                 00043019
             WHEN 100                                                   00043119
              DISPLAY ' FINAL DE ARQUIVO'                               00043219
             WHEN OTHER                                                 00043319
               MOVE SQLCODE TO WRK-SQLCODE                              00043419
               DISPLAY 'ERRO NA LEITURA ' WRK-SQLCODE                   00043519
             END-EVALUATE.                                              00043619
             PERFORM 4100-CALCULAR-MAIOR-SALARIO.                       00043730
       4000-99-FIM.              EXIT.                                  00043819
      *-------------------------------------------------------------*   00043930
       4100-CALCULAR-MAIOR-SALARIO                  SECTION.            00044030
      *-------------------------------------------------------------*   00044130
                                                                        00044230
             IF DB2-SALARIO IS GREATER WRK-MAIOR-VALOR                  00044330
              MOVE DB2-SALARIO TO WRK-MAIOR-VALOR                       00044430
             END-IF.                                                    00044530
                                                                        00044630
       4100-99-FIM.              EXIT.                                  00044730
      *-------------------------------------------------------------*   00044819
       9000-TRATAERROS                              SECTION.            00044919
      *-------------------------------------------------------------*   00045019
                                                                        00046000
       9000-99-FIM.              EXIT.                                  00050000
