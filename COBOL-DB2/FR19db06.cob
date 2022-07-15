      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR19DB06.                                            00005000
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:RAFAEL VIANA                                    *   00007100
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007200
      *   DATA ....:10/06/2022                                      *   00007300
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO:LER TODOS REGISTROS DA TABELA DB2 E GERAR        *   00007501
      *          SAIDA CONSISTIDA NO ARQUIVO 'RELDB2'.              *   00007601
      *                                                             *   00007701
      *-------------------------------------------------------------*   00007900
      *   BASE DE DADOS:                                            *   00008000
      *   TABELA.DB2..                                              *   00008100
      *    ------              I/O                 INCLUDE/BOOK     *   00008200
      *   IVAN.FUNC             I                  #BKFUNC----      *   00008300
      *-------------------------------------------------------------*   00008400
      *   ARQUIVOS:                                                 *   00008501
      *    DDNAME              I/O                  COPY/BOOK       *   00008701
      *   RELDB2                O                  -----------      *   00008801
      *=============================================================*   00009000
                                                                        00009100
      *=============================================================*   00009200
       ENVIRONMENT                               DIVISION.              00009300
      *=============================================================*   00009400
                                                                        00009500
      *=============================================================*   00009600
       CONFIGURATION                               SECTION.             00009700
      *=============================================================*   00009800
       SPECIAL-NAMES.                                                   00009900
           DECIMAL-POINT IS COMMA.                                      00010000
      *-------------------------------------------------------------*   00010100
       INPUT-OUTPUT                                 SECTION.            00010200
       FILE-CONTROL.                                                    00010300
            SELECT RELDB2 ASSIGN TO RELDB2                              00010400
                FILE STATUS IS WRK-FS-RELDB2.                           00010500
                                                                        00010600
      *=============================================================*   00010700
       DATA                                      DIVISION.              00010800
      *=============================================================*   00010900
       FILE                                        SECTION.             00011000
       FD RELDB2                                                        00011100
           RECORDING MODE IS F                                          00011200
           LABEL RECORD IS STANDARD                                     00011300
           BLOCK CONTAINS 0 RECORDS.                                    00011400
      *--------LRECL 99---------------------------------------------*   00011500
       01 FD-RELDB2         PIC X(99).                                  00011612
                                                                        00013011
      *=============================================================*   00013111
       WORKING-STORAGE                             SECTION.             00013211
      *=============================================================*   00013311
                                                                        00013411
           EXEC SQL                                                     00013511
              INCLUDE #BKFUNC                                           00013611
           END-EXEC.                                                    00013711
                                                                        00013811
           EXEC SQL                                                     00013911
              INCLUDE SQLCA                                             00014011
           END-EXEC.                                                    00014111
                                                                        00014211
           EXEC SQL                                                     00014311
              DECLARE CFUNC CURSOR FOR                                  00015000
               SELECT * FROM IVAN.FUNC                                  00016000
                ORDER BY NOME                                           00017021
           END-EXEC.                                                    00018000
      *--------LRECL 99---------------------------------------------*   00019019
                                                                        00019119
       01 WRK-RELDB2.                                                   00019212
          05 WRK-ID                PIC 99999.                           00019312
          05 WRK-NOME              PIC X(30).                           00019413
          05 WRK-SETOR             PIC X(04).                           00019513
          05 WRK-SALARIO           PIC 9999999999.                      00019613
          05 WRK-DATAADM           PIC X(10).                           00019713
          05 WRK-EMAIL             PIC X(40).                           00019813
                                                                        00019920
       01 FILLER          PIC X(64) VALUE                               00020020
           '-----------VARIAVEIS DE STATUS------------------'.          00020120
                                                                        00020219
       77 WRK-FS-RELDB2       PIC 9(02).                                00020302
       77 WRK-SQLCODE         PIC -999.                                 00020419
       77 WRK-NULL-EMAIL      PIC S9(4) COMP.                           00020519
                                                                        00020620
       01 FILLER          PIC X(64) VALUE                               00020720
           '--------VARIAVEIS CONTADORES E ACUMULADOS-------'.          00020820
                                                                        00020919
       77 WRK-REGLIDOS        PIC 9(03).                                00021000
       77 WRK-MAIOR-VALOR     PIC S9(8)V9(2) COMP.                      00021100
       77 WRK-SAL-ACUM        PIC S9(8)V9(2) COMP.                      00022000
       77 WRK-MEDIA-SAL       PIC S9(8)V9(2) COMP.                      00023000
                                                                        00024300
      *=============================================================*   00024400
       PROCEDURE DIVISION.                                              00024500
      *=============================================================*   00024600
                                                                        00024700
      *-------------------------------------------------------------*   00024800
       0000-PRINCIPAL                           SECTION.                00024900
      *-------------------------------------------------------------*   00025000
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
                                                                        00029300
            OPEN OUTPUT RELDB2.                                         00029402
            PERFORM 1100-TESTAR-STATUS.                                 00029507
                                                                        00029602
       1000-99-FIM.              EXIT.                                  00029700
      *-------------------------------------------------------------*   00029802
       1100-TESTAR-STATUS                       SECTION.                00029902
      *-------------------------------------------------------------*   00030002
            IF WRK-FS-RELDB2 NOT EQUAL 0                                00030102
               DISPLAY ' ERRO NA ABERTURA DO ARQUIVO'                   00030202
                  STOP RUN                                              00030602
            END-IF.                                                     00030702
                                                                        00030802
                                                                        00030902
       1100-99-FIM.              EXIT.                                  00031002
      *-------------------------------------------------------------*   00031100
       2000-PROCESSAR                           SECTION.                00031200
      *-------------------------------------------------------------*   00031300
              INITIALIZE WRK-RELDB2.                                    00031412
                                                                        00031612
                  MOVE DB2-ID TO WRK-ID.                                00031818
                  MOVE DB2-NOME TO WRK-NOME.                            00031918
                  MOVE DB2-SETOR TO WRK-SETOR.                          00032018
                  MOVE DB2-SALARIO TO WRK-SALARIO.                      00033018
                  MOVE DB2-DATAADM TO WRK-DATAADM.                      00034018
                 IF WRK-NULL-EMAIL EQUAL -1                             00034118
                  MOVE SPACES  TO WRK-EMAIL                             00034218
                 ELSE                                                   00034318
                  MOVE DB2-EMAIL    TO WRK-EMAIL                        00034418
                 END-IF.                                                00034518
                 WRITE FD-RELDB2 FROM WRK-RELDB2.                       00034718
                 PERFORM 4000-LER-FUNCIONARIO.                          00034818
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
              CLOSE RELDB2.                                             00039502
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
                  :DB2-SETOR,                                           00042100
                  :DB2-SALARIO,                                         00042200
                  :DB2-DATAADM,                                         00042300
                  :DB2-EMAIL     :WRK-NULL-EMAIL                        00042400
            END-EXEC.                                                   00042500
           PERFORM 4050-TESTAR-CONSISTENCIA                             00042617
                                                                        00043516
            EVALUATE SQLCODE                                            00043600
             WHEN 0                                                     00043700
               MOVE DB2-ID TO WRK-ID                                    00043811
               ADD 1 TO WRK-REGLIDOS                                    00043900
                ADD DB2-SALARIO TO WRK-SAL-ACUM                         00044009
               CONTINUE                                                 00044100
             WHEN 100                                                   00044200
              DISPLAY ' FINAL DE ARQUIVO'                               00044300
             WHEN OTHER                                                 00044400
               MOVE SQLCODE TO WRK-SQLCODE                              00044500
               DISPLAY 'ERRO NA LEITURA ' WRK-SQLCODE                   00044600
             END-EVALUATE.                                              00044700
             PERFORM 4100-CALCULAR-MAIOR-SALARIO.                       00044800
       4000-99-FIM.              EXIT.                                  00044900
      *-------------------------------------------------------------*   00045017
       4050-TESTAR-CONSISTENCIA                     SECTION.            00045117
      *-------------------------------------------------------------*   00045217
                                                                        00045317
            IF DB2-ID IS NOT NUMERIC OR DB2-ID EQUAL 0                  00045424
              DISPLAY ' ID NAO NUMERICO '                               00045523
              STOP RUN                                                  00045617
            END-IF.                                                     00045717
            IF DB2-SALARIO IS NOT NUMERIC OR DB2-SALARIO EQUAL 0        00045825
              DISPLAY ' SALARIO NAO NUMERICO '                          00045923
              STOP RUN                                                  00046017
            END-IF.                                                     00046117
       4050-99-FIM.              EXIT.                                  00046217
      *-------------------------------------------------------------*   00046300
       4100-CALCULAR-MAIOR-SALARIO                  SECTION.            00046400
      *-------------------------------------------------------------*   00046500
                                                                        00046600
             IF DB2-SALARIO IS GREATER WRK-MAIOR-VALOR                  00046700
              MOVE DB2-SALARIO TO WRK-MAIOR-VALOR                       00046800
             END-IF.                                                    00046900
                                                                        00047000
       4100-99-FIM.              EXIT.                                  00047100
      *-------------------------------------------------------------*   00047200
       9000-TRATAERROS                              SECTION.            00047300
      *-------------------------------------------------------------*   00047400
                                                                        00047500
       9000-99-FIM.              EXIT.                                  00048000
