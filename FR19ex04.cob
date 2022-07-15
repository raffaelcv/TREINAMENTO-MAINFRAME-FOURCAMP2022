      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR19EX04.                                            00005000
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:RAFAEL VIANA                                    *   00007100
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007200
      *   DATA ....:30/05/2022                                      *   00007300
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO: ESTE PROGRAMA TEM A FINALIDADE DE RECEBER DADOS *   00007500
      *    DOS ARQUIVOS DE ENTRADA 'CLIENTES' E 'MOV0106',FAZER A   *   00007600
      *   RELACAO (BALANCO) ENTRE AS CHAVES E GRAVAR NO ARQUIVO DE  *   00007700
      *               SAIDA (MOV0106A),(MOV0106C).                  *   00007800
      *-------------------------------------------------------------*   00007900
      *   ARQUIVOS...:                                              *   00008000
      *    DDNAME              I/O                 INCLUDE/BOOK     *   00008100
      *    CLIENTES             I                  -----------      *   00008200
      *    MOV0106              I                  -----------      *   00008300
      *    MOV0106A             O                  -----------      *   00008400
      *    MOV0106C             O                  -----------      *   00008500
      *-------------------------------------------------------------*   00008600
      *   MODULOS....:                             INCLUDE/BOOK     *   00008700
      *   GRAVALOG -   TRATAMENTO DE ERROS          #GLOG           *   00008800
      *=============================================================*   00008900
                                                                        00009000
      *=============================================================*   00009100
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
                                                                        00015900
            SELECT MOV0106 ASSIGN TO MOV0106                            00016000
                 FILE STATUS IS WRK-FS-MOV0106.                         00016100
                                                                        00016200
      *--------------ARQUIVOS SAIDAS--------------------------------*   00016300
                                                                        00016400
            SELECT MOV0106A ASSIGN TO MOV0106A                          00016500
                 FILE STATUS IS WRK-FS-MOV0106A.                        00016600
                                                                        00016700
            SELECT MOV0106C ASSIGN TO MOV0106C                          00016800
                 FILE STATUS IS WRK-FS-MOV0106C.                        00016900
      *=============================================================*   00017000
       DATA                                      DIVISION.              00017100
      *=============================================================*   00017200
       FILE                                      SECTION.               00017300
      *-------------------LRECL 46----------------------------------*   00017401
       FD CLIENTES                                                      00017500
           RECORDING MODE IS F                                          00017600
           LABEL RECORD IS STANDARD                                     00017700
           BLOCK CONTAINS 0 RECORDS.                                    00017800
       01 FD-CLIENTES.                                                  00018000
          05 FD-CHAVE.                                                  00018100
             10 FD-AGENCIA      PIC X(04).                              00018200
             10 FD-CONTA        PIC X(04).                              00018300
          05 FD-NOME            PIC X(30).                              00018400
          05 FD-SALDO           PIC 9(08).                              00018500
                                                                        00018600
      *-------------------LRECL 47----------------------------------*   00018701
       FD MOV0106                                                       00018800
           RECORDING MODE IS F                                          00018900
           LABEL RECORD IS STANDARD                                     00019000
           BLOCK CONTAINS 0 RECORDS.                                    00019100
       01 FD-MOV0106.                                                   00019300
          05 FD-MOV-CHAVE.                                              00019400
             10 FD-MOV-AGENCIA      PIC X(04).                          00019500
             10 FD-MOV-CONTA        PIC X(04).                          00020000
          05 FD-MOVIMENTO           PIC X(30).                          00020100
          05 FD-VLRMOVIMENTO        PIC 9(08).                          00020200
          05 FD-TIPOMOV             PIC X(01).                          00020300
                                                                        00020400
      *-------------------LRECL 46----------------------------------*   00020501
       FD MOV0106A                                                      00020600
           RECORDING MODE IS F.                                         00020700
       01 FD-MOV0106A.                                                  00020900
          05 FD-MOVA-AGENCIA         PIC X(04).                         00021000
          05 FD-MOVA-CONTA           PIC X(04).                         00021100
          05 FD-MOVA-NOME            PIC X(30).                         00021200
          05 FD-MOVA-SALDO           PIC 9(08).                         00021300
                                                                        00021400
      *-------------------LRECL 46----------------------------------*   00021501
       FD MOV0106C                                                      00021601
           RECORDING MODE IS F.                                         00021701
       01 FD-MOV0106C.                                                  00021901
          05 FD-MOVC-AGENCIA         PIC X(04).                         00022001
          05 FD-MOVC-CONTA           PIC X(04).                         00022101
          05 FD-MOVC-NOME            PIC X(30).                         00022201
          05 FD-MOVC-SALDO           PIC 9(08).                         00022301
      *=============================================================*   00022400
       WORKING-STORAGE                             SECTION.             00022500
      *=============================================================*   00022600
                                                                        00022700
       01 FILLER          PIC X(64) VALUE                               00022800
           '-----------BOOK LOGERROS------------------------'.          00022900
       77 WRK-GRAVALOG    PIC X(08) VALUE 'GRAVALOG'.                   00023000
       COPY '#GLOG'.                                                    00023100
      *-------------------------------------------------------------*   00023200
                                                                        00023300
                                                                        00023400
       01 FILLER          PIC X(64) VALUE                               00023500
           '-----------VARIAVEIS DE STATUS------------------'.          00023600
                                                                        00023700
       77 WRK-FS-CLIENTES  PIC 9(02).                                   00023800
       77 WRK-FS-MOV0106   PIC 9(02).                                   00023900
       77 WRK-FS-MOV0106A  PIC 9(02).                                   00024000
       77 WRK-FS-MOV0106C  PIC 9(02).                                   00024100
                                                                        00025000
                                                                        00025600
       01 FILLER          PIC X(64) VALUE                               00025700
           '-----------VARIAVEL PARA CACULAR REG.LIDOS------'.          00025800
                                                                        00025900
       77 WRK-REGLIDOS    PIC 9(02) VALUE ZEROES.                       00026000
       77 WRK-REGMOVA     PIC 9(02) VALUE ZEROES.                       00026105
       77 WRK-REGMOVC     PIC 9(02) VALUE ZEROES.                       00026205
       77 WRK-REGCOMP     PIC 9(02) VALUE ZEROES.                       00026315
                                                                        00026409
      *=============================================================*   00026509
       PROCEDURE DIVISION.                                              00026609
      *=============================================================*   00026709
                                                                        00026809
      *-------------------------------------------------------------*   00026909
       0000-PRINCIPAL                           SECTION.                00027009
      *-------------------------------------------------------------*   00027109
                                                                        00027209
            PERFORM  1000-INICIAR.                                      00027309
                                                                        00027409
              PERFORM 1050-VERIFICAR-VAZIO.                             00027509
                                                                        00027609
            PERFORM  2000-PROCESSAR UNTIL WRK-FS-CLIENTES EQUAL 10 AND  00027709
                                          WRK-FS-MOV0106 EQUAL 10.      00027809
            PERFORM  3000-FINALIZAR.                                    00027909
                                                                        00028009
            STOP RUN.                                                   00028109
                                                                        00028209
                                                                        00028309
      *-------------------------------------------------------------*   00028409
       1000-INICIAR                             SECTION.                00028509
      *-------------------------------------------------------------*   00028609
             OPEN INPUT  CLIENTES MOV0106                               00028709
                  OUTPUT MOV0106A MOV0106C.                             00028809
                                                                        00028909
               PERFORM 4000-TESTARSTATUS.                               00029009
                                                                        00029109
       1000-99-FIM.              EXIT.                                  00029209
      *-------------------------------------------------------------*   00029309
       1050-VERIFICAR-VAZIO                     SECTION.                00029409
      *-------------------------------------------------------------*   00029509
                 PERFORM 1100-VERIFICAR-VAZIO-CLIENTES.                 00029609
                 PERFORM 1200-VERIFICAR-VAZIO-MOV0106.                  00029709
                                                                        00029809
       1050-99-FIM.              EXIT.                                  00029909
                                                                        00030009
      *-------------------------------------------------------------*   00030109
       1100-VERIFICAR-VAZIO-CLIENTES            SECTION.                00030209
      *-------------------------------------------------------------*   00030309
                 READ CLIENTES AT END MOVE HIGH-VALUES TO FD-CHAVE.     00030409
                                                                        00030509
       1100-99-FIM.              EXIT.                                  00030609
                                                                        00030709
      *-------------------------------------------------------------*   00030809
       1200-VERIFICAR-VAZIO-MOV0106             SECTION.                00030909
      *-------------------------------------------------------------*   00031009
                 READ MOV0106 AT END MOVE HIGH-VALUES TO FD-MOV-CHAVE.  00031109
                                                                        00031209
       1200-99-FIM.              EXIT.                                  00031309
      *-------------------------------------------------------------*   00031409
       2000-PROCESSAR                           SECTION.                00031509
      *-------------------------------------------------------------*   00031609
            EVALUATE TRUE                                               00031709
             WHEN FD-CHAVE LESS FD-MOV-CHAVE                            00031809
              IF FD-SALDO IS GREATER THAN OR EQUAL TO 10000             00031919
               MOVE FD-CLIENTES TO FD-MOV0106A                          00032018
                WRITE FD-MOV0106A                                       00032118
                 ADD 1 TO WRK-REGMOVA                                   00032209
              ELSE                                                      00032409
                MOVE FD-CLIENTES TO FD-MOV0106C                         00032518
                 WRITE FD-MOV0106C                                      00032618
                  ADD 1 TO WRK-REGMOVC                                  00032709
              END-IF                                                    00032909
                 PERFORM 1100-VERIFICAR-VAZIO-CLIENTES                  00033109
                  ADD 1 TO WRK-REGLIDOS                                 00033209
             WHEN FD-CHAVE EQUAL FD-MOV-CHAVE                           00033309
               PERFORM 2100-AJUSTE-SALDO                                00033409
                                                                        00033509
                PERFORM 1200-VERIFICAR-VAZIO-MOV0106                    00033609
            END-EVALUATE.                                               00033809
                                                                        00033909
       2000-99-FIM.              EXIT.                                  00034009
                                                                        00034109
      *-------------------------------------------------------------*   00034209
       2100-AJUSTE-SALDO                        SECTION.                00034309
      *-------------------------------------------------------------*   00035000
           IF FD-TIPOMOV EQUAL 'C'                                      00035100
            ADD FD-VLRMOVIMENTO  TO FD-SALDO                            00035200
           ELSE                                                         00035300
              IF FD-VLRMOVIMENTO LESS THAN OR EQUAL FD-SALDO            00035417
               SUBTRACT FD-VLRMOVIMENTO FROM FD-SALDO                   00035500
              END-IF                                                    00035600
           END-IF.                                                      00035700
                                                                        00035800
       2100-99-FIM.              EXIT.                                  00035900
                                                                        00036000
      *-------------------------------------------------------------*   00036115
       2200-COMPARAR-REGISTROS                  SECTION.                00036215
      *-------------------------------------------------------------*   00036315
                                                                        00037115
            ADD WRK-REGMOVA TO WRK-REGCOMP                              00037215
            ADD WRK-REGMOVC TO WRK-REGCOMP                              00037315
             IF WRK-REGCOMP NOT EQUAL WRK-REGLIDOS                      00037416
                DISPLAY ' REGISTROS COM INCONSISTENCIA '                00037515
             END-IF.                                                    00037615
                                                                        00037715
       2200-99-FIM.              EXIT.                                  00037815
      *-------------------------------------------------------------*   00037900
       3000-FINALIZAR                           SECTION.                00038000
      *-------------------------------------------------------------*   00038100
             CLOSE CLIENTES MOV0106                                     00038201
                   MOV0106A MOV0106C.                                   00038301
               PERFORM 4000-TESTARSTATUS.                               00038400
               PERFORM 2200-COMPARAR-REGISTROS.                         00038515
              DISPLAY ' REGISTROS LIDOS............    ' WRK-REGLIDOS.  00038611
              DISPLAY ' REGISTROS GRAVADOS NO MOV0106A ' WRK-REGMOVA.   00038711
              DISPLAY ' REGISTROS GRAVADOS NO MOV0106C ' WRK-REGMOVC.   00038811
                                                                        00039010
       3000-99-FIM.              EXIT.                                  00039110
      *-------------------------------------------------------------*   00039210
       4000-TESTARSTATUS                            SECTION.            00039310
      *-------------------------------------------------------------*   00039410
                 PERFORM 4100-TESTARSTATUS-CLIENTES.                    00039510
                 PERFORM 4200-TESTARSTATUS-MOV0106.                     00039610
                 PERFORM 4300-TESTARSTATUS-MOV0106A.                    00039710
                 PERFORM 4400-TESTARSTATUS-MOV0106C.                    00039810
                                                                        00039910
       4000-99-FIM.              EXIT.                                  00040010
      *-------------------------------------------------------------*   00040110
       4100-TESTARSTATUS-CLIENTES                   SECTION.            00040210
      *-------------------------------------------------------------*   00040310
               IF WRK-FS-CLIENTES NOT EQUAL 00                          00040410
                 MOVE 'ERRO NO OPEN CLIENTES ' TO WRK-MSGERRO           00040510
                 MOVE '1000'                   TO WRK-SECAO             00040610
                 MOVE WRK-FS-CLIENTES          TO WRK-STATUS            00040710
                  PERFORM 9000-TRATAERROS                               00040810
               END-IF.                                                  00040910
                                                                        00041010
       4100-99-FIM.              EXIT.                                  00041110
      *-------------------------------------------------------------*   00041210
       4200-TESTARSTATUS-MOV0106                    SECTION.            00041310
      *-------------------------------------------------------------*   00041410
               IF WRK-FS-MOV0106 NOT EQUAL 00                           00041510
                 MOVE 'ERRO NO OPEN MOV0106  ' TO WRK-MSGERRO           00041610
                 MOVE '1000'                   TO WRK-SECAO             00041710
                 MOVE WRK-FS-MOV0106           TO WRK-STATUS            00041810
                  PERFORM 9000-TRATAERROS                               00041910
               END-IF.                                                  00042010
                                                                        00042110
       4200-99-FIM.              EXIT.                                  00042210
      *-------------------------------------------------------------*   00042310
       4300-TESTARSTATUS-MOV0106A                   SECTION.            00042410
      *-------------------------------------------------------------*   00042510
               IF WRK-FS-MOV0106A NOT EQUAL 00                          00042610
                 MOVE 'ERRO NO OPEN MOV0106A ' TO WRK-MSGERRO           00042701
                 MOVE '1000'                   TO WRK-SECAO             00042801
                 MOVE WRK-FS-MOV0106A          TO WRK-STATUS            00042901
                  PERFORM 9000-TRATAERROS                               00043001
               END-IF.                                                  00043101
                                                                        00043201
                                                                        00043301
       4300-99-FIM.              EXIT.                                  00043401
                                                                        00043501
      *-------------------------------------------------------------*   00043601
       4400-TESTARSTATUS-MOV0106C                   SECTION.            00043701
      *-------------------------------------------------------------*   00043801
               IF WRK-FS-MOV0106C NOT EQUAL 00                          00043901
                 MOVE 'ERRO NO OPEN MOV0106C ' TO WRK-MSGERRO           00044001
                 MOVE '1000'                   TO WRK-SECAO             00044101
                 MOVE WRK-FS-MOV0106C          TO WRK-STATUS            00044201
                  PERFORM 9000-TRATAERROS                               00044301
               END-IF.                                                  00044401
                                                                        00044501
                                                                        00044601
       4400-99-FIM.              EXIT.                                  00044701
      *-------------------------------------------------------------*   00044801
       9000-TRATAERROS                              SECTION.            00044901
      *-------------------------------------------------------------*   00045001
           MOVE 'FR19EX04' TO WRK-PROGRAMA                              00045104
           CALL WRK-GRAVALOG USING WRK-DADOS-ERROS.                     00045201
           GOBACK.                                                      00045301
                                                                        00045401
       9000-99-FIM.              EXIT.                                  00046000
