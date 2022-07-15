      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR19EX02.                                            00005000
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:RAFAEL VIANA                                    *   00007100
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007200
      *   DATA ....:30/05/2022                                      *   00007300
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO: ESTE PROGRAMA TEM A FINALIDADE DE RECEBER DADOS *   00007500
      *    DOS ARQUIVOS DE ENTRADA 'FUNC' E 'PROJ', FAZER A RELACAO *   00007600
      *   (BALANDO) ENTRE AS CHAVES E GRAVAR NO ARQUIVO DE SAIDA    *   00007700
      *                         (FUNPROJ).                          *   00007800
      *-------------------------------------------------------------*   00007900
      *   ARQUIVOS...:                                              *   00008000
      *    DDNAME              I/O                 INCLUDE/BOOK     *   00008100
      *    FUNC                 I                  #FUNC            *   00008200
      *    PROJ                 I                  #PROJ            *   00008300
      *    FUNPROJ              O                  -----------      *   00008400
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
            SELECT FUNC ASSIGN TO  FUNC                                 00015600
                 FILE STATUS IS WRK-FS-FUNC.                            00015700
      *-------------------------------------------------------------*   00015800
                                                                        00015900
            SELECT PROJ ASSIGN TO PROJ                                  00016000
                 FILE STATUS IS WRK-FS-PROJ.                            00016100
      *-------------------------------------------------------------*   00016200
                                                                        00016300
            SELECT FUNPROJ ASSIGN TO FUNPROJ                            00016400
                 FILE STATUS IS WRK-FS-FUNPROJ.                         00016500
                                                                        00016600
      *=============================================================*   00016700
       DATA                                      DIVISION.              00016800
      *=============================================================*   00016900
       FILE                                      SECTION.               00017000
       FD FUNC                                                          00017100
           RECORDING MODE IS F                                          00017200
           LABEL RECORD IS STANDARD                                     00017300
           BLOCK CONTAINS 0 RECORDS.                                    00017400
      *-------------------LRECL 39----------------------------------*   00017500
       COPY '#FUNC'.                                                    00017600
                                                                        00017700
                                                                        00017800
       FD PROJ                                                          00017900
           RECORDING MODE IS F                                          00018000
           LABEL RECORD IS STANDARD                                     00019000
           BLOCK CONTAINS 0 RECORDS.                                    00020000
      *-------------------LRECL 28----------------------------------*   00020100
       COPY '#PROJ'.                                                    00020200
                                                                        00020300
       FD FUNPROJ                                                       00020400
           RECORDING MODE IS F.                                         00020500
      *-------------------LRECL 58----------------------------------*   00020600
       01 FD-FUNPROJ.                                                   00020700
          05 FD-ID-FUNC  PIC 9(05).                                     00020800
          05 FD-NOMEFUNC PIC X(30).                                     00020900
          05 FD-PROJETO  PIC X(20).                                     00021000
          05 FD-QTHORAS  PIC 9(03).                                     00022000
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
       77 WRK-FS-FUNC     PIC 9(02).                                    00024500
       77 WRK-FS-PROJ     PIC 9(02).                                    00024600
       77 WRK-FS-FUNPROJ  PIC 9(02).                                    00024700
      *=============================================================*   00024800
       PROCEDURE DIVISION.                                              00024900
      *=============================================================*   00025000
                                                                        00025100
      *-------------------------------------------------------------*   00025200
       0000-PRINCIPAL                           SECTION.                00025300
      *-------------------------------------------------------------*   00025400
                                                                        00025500
            PERFORM  1000-INICIAR.                                      00025600
                                                                        00025700
              PERFORM 1100-VERIFICAR-VAZIO.                             00025800
                                                                        00025900
            PERFORM  2000-PROCESSAR UNTIL WRK-FS-FUNC EQUAL 10 AND      00026000
                                          WRK-FS-PROJ EQUAL 10.         00026100
            PERFORM  3000-FINALIZAR.                                    00026200
                                                                        00026300
            STOP RUN.                                                   00026400
                                                                        00026500
                                                                        00026600
      *-------------------------------------------------------------*   00026700
       1000-INICIAR                             SECTION.                00026800
      *-------------------------------------------------------------*   00026900
             OPEN INPUT  FUNC PROJ                                      00027000
                  OUTPUT FUNPROJ.                                       00027100
                                                                        00027200
               PERFORM 4000-TESTARSTATUS.                               00027300
                                                                        00027400
       1000-99-FIM.              EXIT.                                  00027500
      *-------------------------------------------------------------*   00027600
       1100-VERIFICAR-VAZIO                     SECTION.                00027700
      *-------------------------------------------------------------*   00027800
                 PERFORM 1200-VERIFICAR-VAZIO-FUNC.                     00027907
                 PERFORM 1300-VERIFICAR-VAZIO-PROJ.                     00028013
                                                                        00028100
       1100-99-FIM.              EXIT.                                  00028200
                                                                        00028307
      *-------------------------------------------------------------*   00028407
       1200-VERIFICAR-VAZIO-FUNC                SECTION.                00028507
      *-------------------------------------------------------------*   00028607
                 READ FUNC AT END MOVE HIGH-VALUES TO                   00028709
                                            FD-FUNC-IDFUNC.             00028809
                                                                        00028907
       1200-99-FIM.              EXIT.                                  00029007
                                                                        00029107
      *-------------------------------------------------------------*   00029207
       1300-VERIFICAR-VAZIO-PROJ                SECTION.                00029307
      *-------------------------------------------------------------*   00029407
                 READ PROJ AT END MOVE HIGH-VALUES TO                   00029609
                                            FD-PROJ-IDFUNC.             00029709
                                                                        00029807
       1200-99-FIM.              EXIT.                                  00029907
      *-------------------------------------------------------------*   00030000
       2000-PROCESSAR                           SECTION.                00030100
      *-------------------------------------------------------------*   00030200
            EVALUATE TRUE                                               00030306
             WHEN FD-FUNC-IDFUNC LESS FD-PROJ-IDFUNC                    00030409
      *       WRITE FD-FUNPROJ                                          00031010
      *        DISPLAY 'CHAVE MENOR' FD-FUNC                            00032020
                PERFORM 1200-VERIFICAR-VAZIO-FUNC                       00033010
             WHEN FD-FUNC-IDFUNC EQUAL FD-PROJ-IDFUNC                   00033218
               MOVE FD-FUNC-IDFUNC   TO FD-ID-FUNC                      00033511
               MOVE FD-FUNC-NOMEFUNC TO FD-NOMEFUNC                     00033611
               MOVE FD-PROJ-PROJETO  TO FD-PROJETO                      00033711
               MOVE FD-PROJ-QTHORAS  TO FD-QTHORAS                      00033811
      *         WRITE FD-CLI3105                                        00034006
                DISPLAY 'CHAVE IGUAL' FD-FUNPROJ                        00034119
                PERFORM 1200-VERIFICAR-VAZIO-FUNC                       00034220
      *         PERFORM 1300-VERIFICAR-VAZIO-PROJ                       00034320
             WHEN OTHER                                                 00034506
      *        DISPLAY FD-PROJ ' CHAVE NAO BATE '                       00034621
               PERFORM 1300-VERIFICAR-VAZIO-PROJ                        00034711
      *        STOP RUN                                                 00034815
             END-EVALUATE.                                              00034907
                                                                        00035000
       2000-99-FIM.              EXIT.                                  00035100
      *-------------------------------------------------------------*   00035200
       3000-FINALIZAR                           SECTION.                00035300
      *-------------------------------------------------------------*   00035400
             CLOSE FUNC PROJ                                            00035500
                   FUNPROJ.                                             00035600
               PERFORM 4000-TESTARSTATUS.                               00035700
                                                                        00035800
                                                                        00035900
       3000-99-FIM.              EXIT.                                  00036000
      *-------------------------------------------------------------*   00036100
       4000-TESTARSTATUS                            SECTION.            00036200
      *-------------------------------------------------------------*   00036300
                 PERFORM 4100-TESTARSTATUS-FUNC.                        00036400
                 PERFORM 4200-TESTARSTATUS-PROJ.                        00036500
                 PERFORM 4300-TESTARSTATUS-FUNPROJ.                     00036600
                                                                        00036700
       4000-99-FIM.              EXIT.                                  00036800
      *-------------------------------------------------------------*   00036900
       4100-TESTARSTATUS-FUNC                       SECTION.            00037000
      *-------------------------------------------------------------*   00037100
               IF WRK-FS-FUNC NOT EQUAL 00                              00037200
                 MOVE 'FR19EX02'               TO WRK-PROGRAMA          00037300
                 MOVE 'ERRO NO OPEN FUNC     ' TO WRK-MSGERRO           00037400
                 MOVE '1000'                   TO WRK-SECAO             00037500
                 MOVE WRK-FS-FUNC              TO WRK-STATUS            00037600
                  PERFORM 9000-TRATAERROS                               00037700
               END-IF.                                                  00037800
                                                                        00037900
       4100-99-FIM.              EXIT.                                  00038000
      *-------------------------------------------------------------*   00038100
       4200-TESTARSTATUS-PROJ                       SECTION.            00038200
      *-------------------------------------------------------------*   00038300
               IF WRK-FS-PROJ NOT EQUAL 00                              00038400
                 MOVE 'FR19EX02'               TO WRK-PROGRAMA          00038500
                 MOVE 'ERRO NO OPEN PROJ     ' TO WRK-MSGERRO           00038600
                 MOVE '1000'                   TO WRK-SECAO             00038700
                 MOVE WRK-FS-PROJ              TO WRK-STATUS            00038800
                  PERFORM 9000-TRATAERROS                               00038900
               END-IF.                                                  00039000
                                                                        00039100
       4200-99-FIM.              EXIT.                                  00039200
      *-------------------------------------------------------------*   00039300
       4300-TESTARSTATUS-FUNPROJ                    SECTION.            00039400
      *-------------------------------------------------------------*   00039500
               IF WRK-FS-FUNPROJ NOT EQUAL 00                           00039600
                 MOVE 'FR19EX02'               TO WRK-PROGRAMA          00039700
                 MOVE 'ERRO NO OPEN FUNPROJ  ' TO WRK-MSGERRO           00039800
                 MOVE '1000'                   TO WRK-SECAO             00039900
                 MOVE WRK-FS-FUNPROJ           TO WRK-STATUS            00040000
                  PERFORM 9000-TRATAERROS                               00040100
               END-IF.                                                  00040200
                                                                        00040300
                                                                        00040400
       4300-99-FIM.              EXIT.                                  00040500
      *-------------------------------------------------------------*   00040600
       9000-TRATAERROS                              SECTION.            00040700
      *-------------------------------------------------------------*   00040800
           CALL WRK-GRAVALOG USING WRK-DADOS-ERROS.                     00040900
           GOBACK.                                                      00041000
                                                                        00042000
       9000-99-FIM.              EXIT.                                  00050000
