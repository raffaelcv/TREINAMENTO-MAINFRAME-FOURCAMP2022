      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR19EX02.                                            00005030
                                                                        00006000
      *=============================================================*   00007101
      *   AUTOR....:RAFAEL VIANA                                    *   00007226
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00007301
      *   DATA ....:30/05/2022                                      *   00007401
      *-------------------------------------------------------------*   00007501
      *   OBJETIVO: ESTE PROGRAMA TEM A FINALIDADE DE RECEBER DADOS *   00007602
      *    DOS ARQUIVOS DE ENTRADA 'FUNC' E 'PROJ', FAZER A RELACAO *   00007702
      *   (BALANDO) ENTRE AS CHAVES E GRAVAR NO ARQUIVO DE SAIDA    *   00007802
      *                         (FUNPROJ).                          *   00007902
      *-------------------------------------------------------------*   00008001
      *   ARQUIVOS...:                                              *   00008101
      *    DDNAME              I/O                 INCLUDE/BOOK     *   00008201
      *    FUNC                 I                  #FUNC            *   00008317
      *    PROJ                 I                  #PROJ            *   00008418
      *    FUNPROJ              O                  -----------      *   00008516
      *-------------------------------------------------------------*   00008616
      *   MODULOS....:                             INCLUDE/BOOK     *   00008716
      *   GRAVALOG -   TRATAMENTO DE ERROS          #GLOG           *   00008819
      *=============================================================*   00008916
                                                                        00011800
      *=============================================================*   00011900
       ENVIRONMENT                               DIVISION.              00012000
      *=============================================================*   00013000
                                                                        00014000
      *=============================================================*   00015127
       CONFIGURATION                               SECTION.             00015200
      *=============================================================*   00015327
       SPECIAL-NAMES.                                                   00015500
           DECIMAL-POINT IS COMMA.                                      00015600
                                                                        00015700
       INPUT-OUTPUT                                SECTION.             00015800
       FILE-CONTROL.                                                    00015900
            SELECT FUNC ASSIGN TO  FUNC                                 00016002
                 FILE STATUS IS WRK-FS-FUNC.                            00016102
      *-------------------------------------------------------------*   00016227
                                                                        00016300
            SELECT PROJ ASSIGN TO PROJ                                  00016402
                 FILE STATUS IS WRK-FS-PROJ.                            00016502
      *-------------------------------------------------------------*   00016627
                                                                        00016700
            SELECT FUNPROJ ASSIGN TO FUNPROJ                            00016802
                 FILE STATUS IS WRK-FS-FUNPROJ.                         00016902
                                                                        00017000
      *=============================================================*   00017100
       DATA                                      DIVISION.              00017200
      *=============================================================*   00017300
       FILE                                      SECTION.               00017400
       FD FUNC                                                          00017502
           RECORDING MODE IS F                                          00017600
           LABEL RECORD IS STANDARD                                     00017700
           BLOCK CONTAINS 0 RECORDS.                                    00017800
      *-------------------LRECL 39----------------------------------*   00017902
       COPY '#FUNC'.                                                    00018017
                                                                        00018300
                                                                        00019100
       FD PROJ                                                          00019203
           RECORDING MODE IS F                                          00019300
           LABEL RECORD IS STANDARD                                     00019400
           BLOCK CONTAINS 0 RECORDS.                                    00020000
      *-------------------LRECL 28----------------------------------*   00020103
       COPY '#PROJ'.                                                    00020218
                                                                        00021203
       FD FUNPROJ                                                       00021303
           RECORDING MODE IS F.                                         00021403
      *-------------------LRECL 58----------------------------------*   00021603
       01 FD-FUNPROJ.                                                   00021738
          05 FD-ID-FUNC  PIC 9(05).                                     00021845
          05 FD-NOMEFUNC PIC X(30).                                     00021945
          05 FD-PROJETO  PIC X(20).                                     00022045
          05 FD-QTHORAS  PIC 9(03).                                     00022145
                                                                        00022249
      *=============================================================*   00022904
       WORKING-STORAGE                             SECTION.             00023000
      *=============================================================*   00023104
                                                                        00023204
       01 FILLER          PIC X(64) VALUE                               00023305
           '-----------BOOK LOGERROS------------------------'.          00023405
       77 WRK-GRAVALOG    PIC X(08) VALUE 'GRAVALOG'.                   00023505
       COPY '#GLOG'.                                                    00023604
      *-------------------------------------------------------------*   00023705
                                                                        00023829
                                                                        00024537
       01 FILLER          PIC X(64) VALUE                               00024637
           '-----------VARIAVEIS DE STATUS------------------'.          00024737
                                                                        00024837
       77 WRK-FS-FUNC     PIC 9(02).                                    00024937
       77 WRK-FS-PROJ     PIC 9(02).                                    00025037
       77 WRK-FS-FUNPROJ  PIC 9(02).                                    00025137
      *=============================================================*   00025229
       PROCEDURE DIVISION.                                              00025329
      *=============================================================*   00025429
                                                                        00025532
      *-------------------------------------------------------------*   00025632
       0000-PRINCIPAL                           SECTION.                00025729
      *-------------------------------------------------------------*   00025831
                                                                        00025929
            PERFORM  1000-INICIAR.                                      00026029
                                                                        00026129
              PERFORM 1100-VERIFICAR-VAZIO.                             00026229
                                                                        00026329
            PERFORM  2000-PROCESSAR UNTIL WRK-FS-FUNC EQUAL 10 AND      00026435
                                          WRK-FS-PROJ EQUAL 10.         00026535
            PERFORM  3000-FINALIZAR.                                    00026629
                                                                        00026729
            STOP RUN.                                                   00026829
                                                                        00026929
                                                                        00027029
      *-------------------------------------------------------------*   00027132
       1000-INICIAR                             SECTION.                00027229
      *-------------------------------------------------------------*   00027332
             OPEN INPUT  FUNC PROJ                                      00027429
                  OUTPUT FUNPROJ.                                       00027529
                                                                        00027629
               PERFORM 4000-TESTARSTATUS.                               00027729
                                                                        00027829
       1000-99-FIM.              EXIT.                                  00027929
      *-------------------------------------------------------------*   00028032
       1100-VERIFICAR-VAZIO                     SECTION.                00028129
      *-------------------------------------------------------------*   00028232
                 READ FUNC.                                             00028329
                 READ PROJ.                                             00028429
                                                                        00028529
       1100-99-FIM.              EXIT.                                  00028629
      *-------------------------------------------------------------*   00028732
       2000-PROCESSAR                           SECTION.                00028829
      *-------------------------------------------------------------*   00028932
      *     DISPLAY 'FUNCIONARIO' WRK-FS-FUNC                           00029078
      *     DISPLAY 'PROJETO    ' WRK-FS-PROJ                           00029178
      *     DISPLAY 'VALOR FUNC ' FD-FUNC-IDFUNC                        00029278
      *     DISPLAY 'PROJETO    ' FD-PROJ-IDFUNC                        00029378
      *     DISPLAY '-----------'                                       00029478
            EVALUATE TRUE                                               00029574
             WHEN FD-FUNC-IDFUNC LESS FD-PROJ-IDFUNC                    00029674
                DISPLAY 'CHAVE DIFERENTE' FD-FUNC                       00030062
                READ FUNC                                               00030236
                 IF FD-FUNC-IDFUNC GREATER FD-PROJ-IDFUNC               00030479
                   READ PROJ                                            00030576
                 END-IF                                                 00030673
                                                                        00030777
      *      WHEN FD-FUNC-IDFUNC GREATER FD-PROJ-IDFUNC                 00030882
      *         READ PROJ                                               00030982
                                                                        00031077
             WHEN FD-FUNC-IDFUNC EQUAL FD-PROJ-IDFUNC                   00031140
               MOVE FD-FUNC-IDFUNC   TO FD-ID-FUNC                      00031251
               MOVE FD-FUNC-NOMEFUNC TO FD-NOMEFUNC                     00031351
               MOVE FD-PROJ-PROJETO  TO FD-PROJETO                      00031451
               MOVE FD-PROJ-QTHORAS  TO FD-QTHORAS                      00031551
      *         WRITE FD-FUNPROJ                                        00031654
                DISPLAY 'CHAVE IGUAL' FD-FUNPROJ                        00031762
                READ FUNC                                               00031979
      *         READ PROJ                                               00032085
                IF FD-FUNC-IDFUNC GREATER FD-PROJ-IDFUNC                00032181
                   READ PROJ                                            00032281
                END-IF                                                  00032579
                 IF WRK-FS-PROJ EQUAL 10                                00032677
                  MOVE HIGH-VALUES TO FD-PROJ-IDFUNC                    00032777
                 END-IF                                                 00032877
             WHEN FD-FUNC-IDFUNC GREATER FD-PROJ-IDFUNC                 00032979
                WRITE FD-FUNPROJ                                        00033079
                DISPLAY 'PROJE DIFERENTE' FD-PROJ                       00033179
                READ PROJ                                               00033279
                 IF WRK-FS-PROJ EQUAL 10                                00033379
                  MOVE HIGH-VALUES TO FD-PROJ-IDFUNC                    00033479
                 END-IF                                                 00033579
             WHEN OTHER                                                 00033677
               DISPLAY ' CHAVE NAO BATE '   FD-FUNC-IDFUNC              00033777
               PERFORM 3000-FINALIZAR                                   00033877
            END-EVALUATE.                                               00033977
                                                                        00034077
       2000-99-FIM.              EXIT.                                  00034177
      *-------------------------------------------------------------*   00034277
       3000-FINALIZAR                           SECTION.                00034377
      *-------------------------------------------------------------*   00034477
             CLOSE FUNC PROJ                                            00034577
                   FUNPROJ.                                             00034677
               PERFORM 4000-TESTARSTATUS.                               00034777
                                                                        00034877
                                                                        00034977
       3000-99-FIM.              EXIT.                                  00035077
      *-------------------------------------------------------------*   00035177
       4000-TESTARSTATUS                            SECTION.            00035277
      *-------------------------------------------------------------*   00035377
                 PERFORM 4100-TESTARSTATUS-FUNC.                        00035477
                 PERFORM 4200-TESTARSTATUS-PROJ.                        00035577
                 PERFORM 4300-TESTARSTATUS-FUNPROJ.                     00035677
                                                                        00035777
       4000-99-FIM.              EXIT.                                  00035877
      *-------------------------------------------------------------*   00035977
       4100-TESTARSTATUS-FUNC                       SECTION.            00036077
      *-------------------------------------------------------------*   00036177
               IF WRK-FS-FUNC NOT EQUAL 00                              00036277
                 MOVE 'FR19EX02'               TO WRK-PROGRAMA          00036377
                 MOVE 'ERRO NO OPEN FUNC     ' TO WRK-MSGERRO           00036477
                 MOVE '1000'                   TO WRK-SECAO             00036577
                 MOVE WRK-FS-FUNC              TO WRK-STATUS            00036677
                  PERFORM 9000-TRATAERROS                               00036777
               END-IF.                                                  00036877
                                                                        00036977
       4100-99-FIM.              EXIT.                                  00037077
      *-------------------------------------------------------------*   00037177
       4200-TESTARSTATUS-PROJ                       SECTION.            00037277
      *-------------------------------------------------------------*   00037377
               IF WRK-FS-PROJ NOT EQUAL 00                              00037477
                 MOVE 'FR19EX02'               TO WRK-PROGRAMA          00037577
                 MOVE 'ERRO NO OPEN PROJ     ' TO WRK-MSGERRO           00037677
                 MOVE '1000'                   TO WRK-SECAO             00037777
                 MOVE WRK-FS-PROJ              TO WRK-STATUS            00037877
                  PERFORM 9000-TRATAERROS                               00037977
               END-IF.                                                  00038077
                                                                        00038177
       4200-99-FIM.              EXIT.                                  00038277
      *-------------------------------------------------------------*   00038377
       4300-TESTARSTATUS-FUNPROJ                    SECTION.            00038477
      *-------------------------------------------------------------*   00038577
               IF WRK-FS-FUNPROJ NOT EQUAL 00                           00038677
                 MOVE 'FR19EX02'               TO WRK-PROGRAMA          00038777
                 MOVE 'ERRO NO OPEN FUNPROJ  ' TO WRK-MSGERRO           00038877
                 MOVE '1000'                   TO WRK-SECAO             00038977
                 MOVE WRK-FS-FUNPROJ           TO WRK-STATUS            00039077
                  PERFORM 9000-TRATAERROS                               00039177
               END-IF.                                                  00039277
                                                                        00039377
                                                                        00039477
       4300-99-FIM.              EXIT.                                  00039577
      *-------------------------------------------------------------*   00039677
       9000-TRATAERROS                              SECTION.            00039777
      *-------------------------------------------------------------*   00039877
           CALL WRK-GRAVALOG USING WRK-DADOS-ERROS.                     00039977
           GOBACK.                                                      00040077
                                                                        00041077
       9000-99-FIM.              EXIT.                                  00050003
