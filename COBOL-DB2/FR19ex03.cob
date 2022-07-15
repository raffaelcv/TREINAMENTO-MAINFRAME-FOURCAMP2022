      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR19EX03.                                            00005015
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:RAFAEL VIANA                                    *   00007100
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007200
      *   DATA ....:30/05/2022                                      *   00007300
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO: ESTE PROGRAMA TEM A FINALIDADE DE RECEBER DADOS *   00007500
      *    DOS ARQUIVOS DE ENTRADA 'CLIENTES' E 'MOV3105',FAZER A   *   00007600
      *   RELACAO (BALANCO) ENTRE AS CHAVES E GRAVAR NO ARQUIVO DE  *   00007700
      *               SAIDA (CLI3105).                              *   00007800
      *-------------------------------------------------------------*   00007900
      *   ARQUIVOS...:                                              *   00008000
      *    DDNAME              I/O                 INCLUDE/BOOK     *   00008100
      *    CLIENTES             I                  -----------      *   00008200
      *    MOV3105              I                  -----------      *   00008300
      *    CLI3105              O                  -----------      *   00008400
      *-------------------------------------------------------------*   00008500
      *   MODULOS....:                             INCLUDE/BOOK     *   00008600
      *   GRAVALOG -   TRATAMENTO DE ERROS          #GLOG           *   00008700
      *=============================================================*   00008800
                                                                        00008900
      *=============================================================*   00009000
       ENVIRONMENT                               DIVISION.              00010000
      *=============================================================*   00011000
                                                                        00012000
      *=============================================================*   00013000
       CONFIGURATION                               SECTION.             00014000
      *=============================================================*   00015000
       SPECIAL-NAMES.                                                   00015100
           DECIMAL-POINT IS COMMA.                                      00015200
                                                                        00015300
       INPUT-OUTPUT                                SECTION.             00015400
       FILE-CONTROL.                                                    00015500
            SELECT CLIENTES ASSIGN TO CLIENTES                          00015600
                 FILE STATUS IS WRK-FS-CLIENTES.                        00015700
      *-------------------------------------------------------------*   00015800
                                                                        00015900
            SELECT MOV3105 ASSIGN TO MOV3105                            00016000
                 FILE STATUS IS WRK-FS-MOV3105.                         00016100
      *-------------------------------------------------------------*   00016200
                                                                        00016300
            SELECT CLI3105 ASSIGN TO CLI3105                            00016400
                 FILE STATUS IS WRK-FS-CLI3105.                         00016500
                                                                        00016600
      *=============================================================*   00016700
       DATA                                      DIVISION.              00016800
      *=============================================================*   00016900
       FILE                                      SECTION.               00017000
       FD CLIENTES                                                      00017100
           RECORDING MODE IS F                                          00017200
           LABEL RECORD IS STANDARD                                     00017300
           BLOCK CONTAINS 0 RECORDS.                                    00017400
      *-------------------LRECL 46----------------------------------*   00017500
       01 FD-CLIENTES.                                                  00017600
          05 FD-CHAVE.                                                  00017700
             10 FD-AGENCIA      PIC X(04).                              00017800
             10 FD-CONTA        PIC X(04).                              00017900
          05 FD-NOME            PIC X(30).                              00018000
          05 FD-SALDO           PIC 9(08).                              00018100
                                                                        00018200
       FD MOV3105                                                       00018300
           RECORDING MODE IS F                                          00018400
           LABEL RECORD IS STANDARD                                     00019000
           BLOCK CONTAINS 0 RECORDS.                                    00020000
      *-------------------LRECL 47----------------------------------*   00020100
       01 FD-MOV3105.                                                   00020200
          05 FD-MOV-CHAVE.                                              00020300
             10 FD-MOV-AGENCIA      PIC X(04).                          00020400
             10 FD-MOV-CONTA        PIC X(04).                          00020500
          05 FD-MOVIMENTO           PIC X(30).                          00020600
          05 FD-VLRMOVIMENTO        PIC 9(08).                          00020700
          05 FD-TIPOMOV             PIC X(01).                          00020800
                                                                        00020900
       FD CLI3105                                                       00021000
           RECORDING MODE IS F.                                         00021100
      *-------------------LRECL 46----------------------------------*   00021200
       01 FD-CLI3105.                                                   00021302
          05 FD-CLI-AGENCIA         PIC X(04).                          00021402
          05 FD-CLI-CONTA           PIC X(04).                          00021502
          05 FD-CLI-NOME            PIC X(30).                          00021602
          05 FD-CLI-SALDO           PIC 9(08).                          00021702
                                                                        00022100
      *=============================================================*   00022200
       WORKING-STORAGE                             SECTION.             00022300
      *=============================================================*   00022400
                                                                        00022500
       01 FILLER          PIC X(64) VALUE                               00022600
           '-----------BOOK LOGERROS------------------------'.          00022700
       77 WRK-GRAVALOG    PIC X(08) VALUE 'GRAVALOG'.                   00022800
       COPY '#GLOG'.                                                    00022900
      *-------------------------------------------------------------*   00023000
                                                                        00024000
                                                                        00024100
       01 FILLER          PIC X(64) VALUE                               00024200
           '-----------VARIAVEIS DE STATUS------------------'.          00024300
                                                                        00024400
       77 WRK-FS-CLIENTES PIC 9(02).                                    00024500
       77 WRK-FS-MOV3105  PIC 9(02).                                    00024600
       77 WRK-FS-CLI3105  PIC 9(02).                                    00024700
                                                                        00024934
                                                                        00025634
       01 FILLER          PIC X(64) VALUE                               00025745
           '-----------VARIAVEL PARA CACULAR REG.LIDOS------'.          00025845
                                                                        00025961
       77 WRK-REGLIDOS    PIC 9(02) VALUE ZEROES.                       00026034
                                                                        00026134
      *=============================================================*   00026234
       PROCEDURE DIVISION.                                              00026334
      *=============================================================*   00026434
                                                                        00026534
      *-------------------------------------------------------------*   00026634
       0000-PRINCIPAL                           SECTION.                00026734
      *-------------------------------------------------------------*   00026834
                                                                        00026934
            PERFORM  1000-INICIAR.                                      00027034
                                                                        00027134
              PERFORM 1050-VERIFICAR-VAZIO.                             00027236
                                                                        00027334
            PERFORM  2000-PROCESSAR UNTIL WRK-FS-CLIENTES EQUAL 10 AND  00027434
                                          WRK-FS-MOV3105 EQUAL 10.      00027534
            PERFORM  3000-FINALIZAR.                                    00027634
                                                                        00027734
            STOP RUN.                                                   00027834
                                                                        00027934
                                                                        00028034
      *-------------------------------------------------------------*   00028134
       1000-INICIAR                             SECTION.                00028234
      *-------------------------------------------------------------*   00028334
             OPEN INPUT  CLIENTES MOV3105                               00028434
                  OUTPUT CLI3105.                                       00028534
                                                                        00028634
               PERFORM 4000-TESTARSTATUS.                               00028734
                                                                        00028834
       1000-99-FIM.              EXIT.                                  00028934
      *-------------------------------------------------------------*   00029035
       1050-VERIFICAR-VAZIO                     SECTION.                00029135
      *-------------------------------------------------------------*   00029235
                 PERFORM 1100-VERIFICAR-VAZIO-CLIENTES.                 00029356
                 PERFORM 1200-VERIFICAR-VAZIO-MOV3105.                  00029456
      *          READ CLIENTES.                                         00029556
      *          READ MOV3105.                                          00029656
                                                                        00029735
       1100-99-FIM.              EXIT.                                  00029835
                                                                        00029935
      *-------------------------------------------------------------*   00030035
       1100-VERIFICAR-VAZIO-CLIENTES            SECTION.                00030135
      *-------------------------------------------------------------*   00030235
                 READ CLIENTES AT END MOVE HIGH-VALUES TO FD-CHAVE.     00030353
                                                                        00030453
       1100-99-FIM.              EXIT.                                  00030535
                                                                        00030635
      *-------------------------------------------------------------*   00030735
       1200-VERIFICAR-VAZIO-MOV3105             SECTION.                00030835
      *-------------------------------------------------------------*   00030935
                 READ MOV3105 AT END MOVE HIGH-VALUES TO FD-MOV-CHAVE.  00031035
                                                                        00031135
       1100-99-FIM.              EXIT.                                  00031235
      *-------------------------------------------------------------*   00031335
       2000-PROCESSAR                           SECTION.                00031435
      *-------------------------------------------------------------*   00031535
            EVALUATE TRUE                                               00031635
             WHEN FD-CHAVE LESS FD-MOV-CHAVE                            00031735
               MOVE FD-CLIENTES TO FD-CLI3105                           00031857
                DISPLAY FD-CLI3105                                      00032360
                 PERFORM 1100-VERIFICAR-VAZIO-CLIENTES                  00032535
                  ADD 1 TO WRK-REGLIDOS                                 00032635
             WHEN FD-CHAVE EQUAL FD-MOV-CHAVE                           00032735
               PERFORM 2100-AJUSTE-SALDO                                00032935
                                                                        00033535
                PERFORM 1200-VERIFICAR-VAZIO-MOV3105                    00034356
                 ADD 1 TO WRK-REGLIDOS                                  00034457
            END-EVALUATE.                                               00034900
                                                                        00035000
       2000-99-FIM.              EXIT.                                  00035100
                                                                        00035202
      *-------------------------------------------------------------*   00035302
       2100-AJUSTE-SALDO                        SECTION.                00035402
      *-------------------------------------------------------------*   00035502
           IF FD-TIPOMOV EQUAL 'C'                                      00035604
            ADD FD-VLRMOVIMENTO  TO FD-SALDO                            00036058
           ELSE                                                         00036104
              IF FD-VLRMOVIMENTO LESS OR EQUAL FD-SALDO                 00036259
               SUBTRACT FD-VLRMOVIMENTO FROM FD-SALDO                   00036759
              END-IF                                                    00036859
           END-IF.                                                      00036904
                                                                        00037002
       2100-99-FIM.              EXIT.                                  00037102
                                                                        00037202
      *-------------------------------------------------------------*   00037302
       3000-FINALIZAR                           SECTION.                00037402
      *-------------------------------------------------------------*   00037502
             CLOSE CLIENTES MOV3105                                     00037602
                   CLI3105.                                             00037702
               PERFORM 4000-TESTARSTATUS.                               00037802
              DISPLAY ' REGISTROS LIDOS ' WRK-REGLIDOS.                 00037912
                                                                        00038002
                                                                        00038102
       3000-99-FIM.              EXIT.                                  00038202
      *-------------------------------------------------------------*   00038302
       4000-TESTARSTATUS                            SECTION.            00038402
      *-------------------------------------------------------------*   00038502
                 PERFORM 4100-TESTARSTATUS-CLIENTES.                    00038602
                 PERFORM 4200-TESTARSTATUS-MOV3105.                     00038702
                 PERFORM 4300-TESTARSTATUS-CLI3105.                     00038802
                                                                        00038902
       4000-99-FIM.              EXIT.                                  00039002
      *-------------------------------------------------------------*   00039102
       4100-TESTARSTATUS-CLIENTES                   SECTION.            00039202
      *-------------------------------------------------------------*   00039302
               IF WRK-FS-CLIENTES NOT EQUAL 00                          00039402
                 MOVE 'FR19EX03'               TO WRK-PROGRAMA          00039502
                 MOVE 'ERRO NO OPEN FUNC     ' TO WRK-MSGERRO           00039602
                 MOVE '1000'                   TO WRK-SECAO             00039702
                 MOVE WRK-FS-CLIENTES          TO WRK-STATUS            00039802
                  PERFORM 9000-TRATAERROS                               00039902
               END-IF.                                                  00040002
                                                                        00040102
       4100-99-FIM.              EXIT.                                  00040202
      *-------------------------------------------------------------*   00040302
       4200-TESTARSTATUS-MOV3105                    SECTION.            00040402
      *-------------------------------------------------------------*   00040502
               IF WRK-FS-MOV3105 NOT EQUAL 00                           00040602
                 MOVE 'FR19EX03'               TO WRK-PROGRAMA          00040702
                 MOVE 'ERRO NO OPEN PROJ     ' TO WRK-MSGERRO           00040802
                 MOVE '1000'                   TO WRK-SECAO             00040902
                 MOVE WRK-FS-MOV3105           TO WRK-STATUS            00041002
                  PERFORM 9000-TRATAERROS                               00041102
               END-IF.                                                  00041202
                                                                        00041302
       4200-99-FIM.              EXIT.                                  00041402
      *-------------------------------------------------------------*   00041502
       4300-TESTARSTATUS-CLI3105                    SECTION.            00041602
      *-------------------------------------------------------------*   00041702
               IF WRK-FS-CLI3105 NOT EQUAL 00                           00041802
                 MOVE 'FR19EX03'               TO WRK-PROGRAMA          00041902
                 MOVE 'ERRO NO OPEN FUNPROJ  ' TO WRK-MSGERRO           00042002
                 MOVE '1000'                   TO WRK-SECAO             00042102
                 MOVE WRK-FS-CLI3105           TO WRK-STATUS            00042202
                  PERFORM 9000-TRATAERROS                               00042302
               END-IF.                                                  00042402
                                                                        00042502
                                                                        00042602
       4300-99-FIM.              EXIT.                                  00042702
      *-------------------------------------------------------------*   00042802
       9000-TRATAERROS                              SECTION.            00042902
      *-------------------------------------------------------------*   00043002
           CALL WRK-GRAVALOG USING WRK-DADOS-ERROS.                     00043102
           GOBACK.                                                      00043202
                                                                        00044002
       9000-99-FIM.              EXIT.                                  00050000
