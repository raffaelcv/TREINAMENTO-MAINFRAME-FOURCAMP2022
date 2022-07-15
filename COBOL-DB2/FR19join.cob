      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR19JOIN.                                            00005000
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:RAFAEL VIANA                                    *   00007100
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007200
      *   DATA ....:14/06/2022                                      *   00007300
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO:LER FUNCIONARIOS ,COMPARAR COM TABELA DE         *   00007500
      *          BENEFICIOS E CRIAR ARQUIVO DE SAIDA (SRELBENE)     *   00007600
      *           LISTANDO FUNCIONARIOS COM BENEFICIOS.             *   00007700
      *                                                             *   00007800
      *-------------------------------------------------------------*   00007900
      *   BASE DE DADOS:                                            *   00008000
      *   TABELA.DB2..                                              *   00008100
      *    ------                                  INCLUDE/BOOK     *   00008200
      *   IVAN.FUNC                                #BKFUNC----      *   00008300
      *   IVAN.BENEF                               #BKBENEF---      *   00008400
      *-------------------------------------------------------------*   00008500
      *   ARQUIVOS...:                                              *   00008606
      *    DDNAME              I/O                 INCLUDE/BOOK     *   00008706
      *    SRELBENE             O                  -----------      *   00008806
      *-------------------------------------------------------------*   00009006
      *   MODULOS....:                             INCLUDE/BOOK     *   00009100
      *=============================================================*   00009200
                                                                        00009300
      *=============================================================*   00009400
       ENVIRONMENT                               DIVISION.              00009500
      *=============================================================*   00009600
                                                                        00009700
      *=============================================================*   00009800
       CONFIGURATION                               SECTION.             00009900
      *=============================================================*   00010000
       SPECIAL-NAMES.                                                   00010100
           DECIMAL-POINT IS COMMA.                                      00010200
                                                                        00010300
       INPUT-OUTPUT                                 SECTION.            00010400
       FILE-CONTROL.                                                    00010500
            SELECT SRELBENE ASSIGN TO SRELBENE                          00010600
                FILE STATUS IS WRK-FS-SRELBENE.                         00010700
                                                                        00010800
      *=============================================================*   00010900
       DATA                                      DIVISION.              00011000
      *=============================================================*   00011100
       FILE                                        SECTION.             00011200
       FD SRELBENE                                                      00011300
           RECORDING MODE IS F                                          00011400
           LABEL RECORD IS STANDARD                                     00011500
           BLOCK CONTAINS 0 RECORDS.                                    00011600
      *-----------------------LRECL 68------------------------------*   00011701
       01 FD-SRELBENE         PIC X(68).                                00011800
                                                                        00011900
      *=============================================================*   00012000
       WORKING-STORAGE                             SECTION.             00012100
      *=============================================================*   00012200
                                                                        00012300
           EXEC SQL                                                     00012400
              INCLUDE #BKFUNC                                           00012500
           END-EXEC.                                                    00012600
                                                                        00012700
           EXEC SQL                                                     00012800
              INCLUDE #BKBENEF                                          00012900
           END-EXEC.                                                    00013000
                                                                        00013100
           EXEC SQL                                                     00013200
              INCLUDE SQLCA                                             00013300
           END-EXEC.                                                    00013400
                                                                        00013500
           EXEC SQL                                                     00014000
              DECLARE CFUNC CURSOR FOR                                  00015000
               SELECT ID,NOME,SALARIO,PLANMED, PLANDENT, COTAS          00016000
                FROM IVAN.FUNC F , IVAN.BENEF B                         00016100
                 WHERE F.ID = B.IDFUNC                                  00016200
           END-EXEC.                                                    00016400
      *-------------------------------------------------------------*   00016601
       01 FILLER              PIC X(70) VALUE                           00016701
              '---------VARIAVEIS PARA RECEBER DADOS-(LRECL 68)----'.   00016801
      *-------------------------------------------------------------*   00016901
      *--------------LRECL 68----------------------------               00017001
       01 WRK-DADOS.                                                    00017100
          05 WRK-MID          PIC 9(05).                                00017200
          05 WRK-NOME         PIC X(30).                                00017300
          05 WRK-SALARIO      PIC 9(10).                                00017400
          05 WRK-PLANMED      PIC X(10).                                00017500
          05 WRK-PLANDENT     PIC X(10).                                00018000
          05 WRK-COTAS        PIC 9(03).                                00019000
                                                                        00019100
      *-------------------------------------------------------------*   00019201
       01 FILLER              PIC X(70) VALUE                           00019301
              '---------VARIAVEL PARA CALCULAR REGISTROS ---------'.    00019402
      *-------------------------------------------------------------*   00019501
                                                                        00019605
       77 WRK-REGLIDOS        PIC 9(03).                                00019704
       77 WRK-REGGRAVA        PIC 9(03).                                00019804
                                                                        00019905
      *-------------------------------------------------------------*   00020002
       01 FILLER              PIC X(70) VALUE                           00020102
              '---------VARIAVEIS PARA CALCULAR SALARIOS-----------'.   00020202
      *-------------------------------------------------------------*   00020302
                                                                        00020405
       77 WRK-MAIOR-VALOR     PIC S9(8)V9(2) COMP.                      00020502
       77 WRK-SAL-ACUM        PIC S9(8)V9(2) COMP.                      00020602
       77 WRK-MEDIA-SAL       PIC S9(8)V9(2) COMP.                      00020702
       77 WRK-ID              PIC 9(05) VALUE ZEROES.                   00020802
                                                                        00020905
      *-------------------------------------------------------------*   00021000
       01 FILLER              PIC X(70) VALUE                           00021100
              '---------VARIAVEL DE STATUS SRELBENE-SQLCODE----'.       00021201
      *-------------------------------------------------------------*   00021300
                                                                        00021405
       77 WRK-FS-SRELBENE     PIC 9(02).                                00021500
       77 WRK-SQLCODE         PIC -999.                                 00021601
                                                                        00021700
      *-------------------------------------------------------------*   00021804
       01 FILLER              PIC X(70) VALUE                           00021904
              '---------VARIAVEIS DE MENSAGENS SYSOUT-----'.            00022005
      *-------------------------------------------------------------*   00022104
                                                                        00022204
       77 WRK-MSGLIDOS        PIC X(22) VALUE                           00022304
                                        'REGISTROS LIDOS.......'.       00022404
       77 WRK-MSGGRAVA        PIC X(22) VALUE                           00022504
                                        'REGISTROS GRAVADOS....'.       00022604
       77 WRK-MSGMAIORSAL     PIC X(22) VALUE                           00022704
                                        'MAIOR SALARIO.........'.       00022804
       77 WRK-MSGACUMSAL      PIC X(22) VALUE                           00022904
                                        'SALARIO ACUMULADO.....'.       00023004
       77 WRK-MSGMEDIASAL     PIC X(22) VALUE                           00023104
                                        'MEDIA DOS SALARIOS....'.       00023204
                                                                        00023305
      *-------------------------------------------------------------*   00023404
       01 FILLER              PIC X(70) VALUE                           00023504
              '---------VARIAVEIS DE MENSAGENS DE ERRO----'.            00023604
      *-------------------------------------------------------------*   00023704
                                                                        00023804
       77 WRK-ERRO            PIC X(05) VALUE 'ERRO '.                  00023904
       77 WRK-ERROOPEN        PIC X(17) VALUE 'NO OPEN DO CURSOR'.      00024004
       77 WRK-SEMFUNC         PIC X(16) VALUE 'SEM FUNCIONARIOS'.       00024104
       77 WRK-ERROARQ         PIC X(28) VALUE                           00024204
                                       ' ERRO NA ABERTURA DO ARQUIVO'.  00024304
       77 WRK-FIMARQ          PIC X(16) VALUE 'FINAL DE ARQUIVO'.       00024404
       77 WRK-ERROLEI         PIC X(16) VALUE 'ERRO NA LEITURA:'.       00024504
      *=============================================================*   00024604
       PROCEDURE DIVISION.                                              00024704
      *=============================================================*   00024804
                                                                        00024900
      *-------------------------------------------------------------*   00025000
       0000-PRINCIPAL                           SECTION.                00025100
      *-------------------------------------------------------------*   00025200
                                                                        00025300
            PERFORM  1000-INICIAR.                                      00025400
            PERFORM  2000-PROCESSAR UNTIL SQLCODE EQUAL 100.            00025500
            PERFORM  3000-FINALIZAR.                                    00025600
            STOP RUN.                                                   00025700
                                                                        00025800
                                                                        00025900
      *-------------------------------------------------------------*   00026000
       1000-INICIAR                             SECTION.                00026100
      *-------------------------------------------------------------*   00026200
            EXEC SQL                                                    00026300
               OPEN CFUNC                                               00026400
            END-EXEC.                                                   00026500
             EVALUATE SQLCODE                                           00026600
              WHEN 0                                                    00026700
                PERFORM 4000-LER-FUNCIONARIO                            00026800
              WHEN 100                                                  00026900
                DISPLAY WRK-SEMFUNC                                     00027001
              WHEN OTHER                                                00027100
                MOVE SQLCODE TO WRK-SQLCODE                             00027200
                DISPLAY WRK-ERRO WRK-SQLCODE WRK-ERROOPEN               00027301
                MOVE 200 TO RETURN-CODE                                 00027400
                STOP RUN                                                00027500
             END-EVALUATE.                                              00027600
             OPEN OUTPUT SRELBENE.                                      00027700
             PERFORM 1100-TESTAR-STATUS.                                00027800
       1000-99-FIM.              EXIT.                                  00027900
      *-------------------------------------------------------------*   00028000
       1100-TESTAR-STATUS                       SECTION.                00029000
      *-------------------------------------------------------------*   00029100
            IF WRK-FS-SRELBENE NOT EQUAL 0                              00029200
               DISPLAY WRK-ERROARQ                                      00029301
                  STOP RUN                                              00029400
            END-IF.                                                     00029500
                                                                        00029600
       1100-99-FIM.              EXIT.                                  00029700
                                                                        00029800
      *-------------------------------------------------------------*   00029900
       2000-PROCESSAR                           SECTION.                00030000
      *-------------------------------------------------------------*   00030100
                                                                        00030200
             INITIALIZE WRK-DADOS.                                      00030300
              MOVE DB2-ID       TO WRK-MID.                             00030400
              MOVE DB2-NOME     TO WRK-NOME.                            00030500
              MOVE DB2-SALARIO  TO WRK-SALARIO.                         00030600
              MOVE DB2-PLANMED  TO WRK-PLANMED.                         00030700
              MOVE DB2-PLANDENT TO WRK-PLANDENT.                        00030800
              MOVE DB2-COTAS    TO WRK-COTAS.                           00031300
              WRITE FD-SRELBENE FROM WRK-DADOS.                         00031500
              ADD 1 TO WRK-REGGRAVA                                     00031603
              PERFORM 4000-LER-FUNCIONARIO.                             00031700
                                                                        00031800
       2000-99-FIM.              EXIT.                                  00031900
                                                                        00032000
      *-------------------------------------------------------------*   00032100
       3000-FINALIZAR                               SECTION.            00033000
      *-------------------------------------------------------------*   00034000
                                                                        00035000
              EXEC SQL                                                  00036000
                CLOSE CFUNC                                             00037000
              END-EXEC.                                                 00038000
              CLOSE SRELBENE.                                           00039000
              DISPLAY WRK-MSGLIDOS      WRK-REGLIDOS.                   00039204
              DISPLAY WRK-MSGGRAVA      WRK-REGGRAVA.                   00039304
              DISPLAY WRK-MSGMAIORSAL   WRK-MAIOR-VALOR.                00039404
              DISPLAY WRK-MSGACUMSAL    WRK-SAL-ACUM.                   00039504
               DIVIDE WRK-SAL-ACUM BY WRK-REGLIDOS                      00039604
                                 GIVING WRK-MEDIA-SAL.                  00039702
              DISPLAY WRK-MSGMEDIASAL   WRK-MEDIA-SAL.                  00039804
                                                                        00039902
       3000-99-FIM.              EXIT.                                  00040002
      *-------------------------------------------------------------*   00040102
       4000-LER-FUNCIONARIO                         SECTION.            00041000
      *-------------------------------------------------------------*   00041100
                                                                        00041200
           EXEC SQL                                                     00041300
            FETCH CFUNC                                                 00041400
             INTO :DB2-ID,                                              00041500
                  :DB2-NOME,                                            00041600
                  :DB2-SALARIO,                                         00041700
                  :DB2-PLANMED,                                         00041800
                  :DB2-PLANDENT,                                        00041900
                  :DB2-COTAS                                            00042000
            END-EXEC.                                                   00042100
            EVALUATE SQLCODE                                            00042200
             WHEN 0                                                     00042300
               ADD 1 TO WRK-REGLIDOS                                    00042400
                ADD DB2-SALARIO TO WRK-SAL-ACUM                         00042500
               CONTINUE                                                 00042600
             WHEN 100                                                   00042700
              DISPLAY WRK-FIMARQ                                        00042802
             WHEN OTHER                                                 00042900
               MOVE SQLCODE TO WRK-SQLCODE                              00043000
               DISPLAY WRK-ERROLEI WRK-SQLCODE                          00043102
             END-EVALUATE.                                              00043200
             PERFORM 4100-CALCULAR-MAIOR-SALARIO.                       00043300
       4000-99-FIM.              EXIT.                                  00043400
      *-------------------------------------------------------------*   00043500
       4100-CALCULAR-MAIOR-SALARIO                  SECTION.            00043600
      *-------------------------------------------------------------*   00043700
                                                                        00043800
             IF DB2-SALARIO IS GREATER WRK-MAIOR-VALOR                  00043900
              MOVE DB2-SALARIO TO WRK-MAIOR-VALOR                       00044000
             END-IF.                                                    00044100
                                                                        00044200
       4100-99-FIM.              EXIT.                                  00044300
      *-------------------------------------------------------------*   00044400
       9000-TRATAERROS                              SECTION.            00044500
      *-------------------------------------------------------------*   00044600
                                                                        00044700
       9000-99-FIM.              EXIT.                                  00044800
