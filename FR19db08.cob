      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR19DB08.                                            00005000
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:RAFAEL VIANA                                    *   00007100
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007200
      *   DATA ....:14/06/2022                                      *   00007347
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO:LER FUNCIONARIOS ,COMPARAR COM TABELA DE SETOR   *   00007500
      *          E MOSTRAR FUNCIONARIOS QUE ESTAO SEM SETOR.        *   00007600
      *             (UTILIZANDO LEFT EXLUDE JOIN)                  *    00007746
      *                                                             *   00007800
      *-------------------------------------------------------------*   00007900
      *   BASE DE DADOS:                                            *   00008000
      *   TABELA.DB2..                                              *   00008100
      *    ------                                  INCLUDE/BOOK     *   00008200
      *   IVAN.FUNC                                #BKFUNC----      *   00008300
      *   IVAN.SETOR                               #BKSETOR---      *   00008400
      *-------------------------------------------------------------*   00008500
      *   MODULOS....:                             INCLUDE/BOOK     *   00008600
      *=============================================================*   00008700
                                                                        00008800
      *=============================================================*   00008900
       ENVIRONMENT                               DIVISION.              00009000
      *=============================================================*   00009100
                                                                        00009200
      *=============================================================*   00009300
       CONFIGURATION                               SECTION.             00009400
      *=============================================================*   00009500
       SPECIAL-NAMES.                                                   00009600
           DECIMAL-POINT IS COMMA.                                      00009700
                                                                        00009800
      *=============================================================*   00009900
       DATA                                      DIVISION.              00010000
      *=============================================================*   00010100
      *=============================================================*   00010200
       WORKING-STORAGE                             SECTION.             00010300
      *=============================================================*   00010400
                                                                        00010500
           EXEC SQL                                                     00010600
              INCLUDE #BKFUNC                                           00010700
           END-EXEC.                                                    00010800
                                                                        00010900
           EXEC SQL                                                     00011000
              INCLUDE #BKSETOR                                          00011100
           END-EXEC.                                                    00011200
                                                                        00011300
           EXEC SQL                                                     00011400
              INCLUDE SQLCA                                             00011500
           END-EXEC.                                                    00011600
                                                                        00011700
           EXEC SQL                                                     00011800
              DECLARE CFUNC CURSOR FOR                                  00011900
               SELECT ID, NOME, SALARIO, DATAADM, EMAIL                 00012344
               FROM IVAN.FUNC F                                         00012440
               WHERE NOT EXISTS (SELECT IDSETOR FROM IVAN.SETOR S       00012640
                                 WHERE F.SETOR = S.IDSETOR)             00012730
           END-EXEC.                                                    00016000
                                                                        00017000
       01 WRK-DADOS.                                                    00018000
          05 WRK-MID          PIC 9(05).                                00019000
          05 WRK-NOME         PIC X(30).                                00019100
          05 WRK-SALARIO      PIC 9(10).                                00019200
          05 WRK-DATAADM      PIC X(10).                                00019300
          05 WRK-EMAIL        PIC X(40).                                00019400
          05 WRK-DESCSETOR    PIC X(40).                                00019543
                                                                        00019600
       77 WRK-REGLIDOS        PIC 9(03).                                00019700
       77 WRK-MAIOR-VALOR     PIC S9(8)V9(2) COMP.                      00019800
       77 WRK-SAL-ACUM        PIC S9(8)V9(2) COMP.                      00019900
       77 WRK-MEDIA-SAL       PIC S9(8)V9(2) COMP.                      00020000
       77 WRK-ID              PIC 9(05) VALUE ZEROES.                   00021000
       77 WRK-SQLCODE         PIC -999.                                 00022000
       77 WRK-NULL-EMAIL      PIC S9(4) COMP.                           00023000
                                                                        00024000
      *=============================================================*   00024100
       PROCEDURE DIVISION.                                              00024200
      *=============================================================*   00024300
                                                                        00024400
      *-------------------------------------------------------------*   00024500
       0000-PRINCIPAL                           SECTION.                00024600
      *-------------------------------------------------------------*   00024700
                                                                        00024800
            PERFORM  1000-INICIAR.                                      00024900
            PERFORM  2000-PROCESSAR UNTIL SQLCODE EQUAL 100.            00025000
            PERFORM  3000-FINALIZAR.                                    00026000
            STOP RUN.                                                   00026100
                                                                        00026200
                                                                        00026300
      *-------------------------------------------------------------*   00026400
       1000-INICIAR                             SECTION.                00026500
      *-------------------------------------------------------------*   00026600
            EXEC SQL                                                    00026700
               OPEN CFUNC                                               00026800
            END-EXEC.                                                   00026900
             EVALUATE SQLCODE                                           00027000
              WHEN 0                                                    00027100
                PERFORM 4000-LER-FUNCIONARIO                            00027200
              WHEN 100                                                  00027300
                DISPLAY 'SEM FUNCIONARIO'                               00027400
              WHEN OTHER                                                00027500
                MOVE SQLCODE TO WRK-SQLCODE                             00027600
                DISPLAY 'ERRO ' WRK-SQLCODE ' NO OPEN DO CURSOR.'       00027700
                MOVE 200 TO RETURN-CODE                                 00027800
                STOP RUN                                                00027900
             END-EVALUATE.                                              00028000
                                                                        00029000
       1000-99-FIM.              EXIT.                                  00029100
      *-------------------------------------------------------------*   00029200
       2000-PROCESSAR                           SECTION.                00029300
      *-------------------------------------------------------------*   00029400
                                                                        00029500
                                                                        00029600
              MOVE DB2-ID TO WRK-MID.                                   00029700
              MOVE DB2-NOME TO WRK-NOME.                                00029800
              MOVE DB2-SALARIO TO WRK-SALARIO.                          00029900
              MOVE DB2-DATAADM TO WRK-DATAADM.                          00030000
              IF WRK-NULL-EMAIL = 0                                     00030100
                MOVE DB2-EMAIL TO WRK-EMAIL                             00030200
              ELSE                                                      00030300
                MOVE 'SEM EMAIL ' TO WRK-EMAIL                          00030400
              END-IF.                                                   00030500
              MOVE DB2-DESCSETOR TO WRK-DESCSETOR.                      00030643
              DISPLAY WRK-DADOS.                                        00030700
                                                                        00034700
              PERFORM 4000-LER-FUNCIONARIO.                             00034800
                                                                        00034900
       2000-99-FIM.              EXIT.                                  00035000
                                                                        00036000
      *-------------------------------------------------------------*   00037000
       3000-FINALIZAR                               SECTION.            00038000
      *-------------------------------------------------------------*   00039000
                                                                        00039100
              EXEC SQL                                                  00039200
                CLOSE CFUNC                                             00039300
              END-EXEC.                                                 00039400
              DISPLAY ' -----FIM DO PROGRAMA----- '.                    00039500
              DISPLAY ' REGISTROS LIDOS.......' WRK-REGLIDOS.           00039600
              DISPLAY ' MAIOR SALARIO.........' WRK-MAIOR-VALOR.        00039700
              DISPLAY ' SALARIO ACUMULADO.....' WRK-SAL-ACUM.           00039800
             DIVIDE WRK-SAL-ACUM BY WRK-REGLIDOS                        00039900
                                 GIVING WRK-MEDIA-SAL.                  00040000
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
                  :DB2-SALARIO,                                         00042100
                  :DB2-DATAADM,                                         00042200
                  :DB2-EMAIL     :WRK-NULL-EMAIL,                       00042342
                  :DB2-DESCSETOR                                        00042441
            END-EXEC.                                                   00042500
            EVALUATE SQLCODE                                            00042600
             WHEN 0                                                     00042700
               ADD 1 TO WRK-REGLIDOS                                    00042800
                ADD DB2-SALARIO TO WRK-SAL-ACUM                         00042900
               CONTINUE                                                 00043000
             WHEN 100                                                   00043100
              DISPLAY ' FINAL DE ARQUIVO'                               00043200
             WHEN OTHER                                                 00043300
               MOVE SQLCODE TO WRK-SQLCODE                              00043400
               DISPLAY 'ERRO NA LEITURA ' WRK-SQLCODE                   00043500
             END-EVALUATE.                                              00043600
             PERFORM 4100-CALCULAR-MAIOR-SALARIO.                       00043700
       4000-99-FIM.              EXIT.                                  00043800
      *-------------------------------------------------------------*   00043900
       4100-CALCULAR-MAIOR-SALARIO                  SECTION.            00044000
      *-------------------------------------------------------------*   00044100
                                                                        00044200
             IF DB2-SALARIO IS GREATER WRK-MAIOR-VALOR                  00044300
              MOVE DB2-SALARIO TO WRK-MAIOR-VALOR                       00044400
             END-IF.                                                    00044500
                                                                        00044600
       4100-99-FIM.              EXIT.                                  00044700
      *-------------------------------------------------------------*   00044800
       9000-TRATAERROS                              SECTION.            00044900
      *-------------------------------------------------------------*   00045000
                                                                        00046000
       9000-99-FIM.              EXIT.                                  00047000
