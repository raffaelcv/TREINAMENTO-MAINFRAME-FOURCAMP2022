      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. RELACLI.                                             00005000
                                                                        00006000
      *=============================================================*   00006119
      *   AUTOR....:RAFAEL                                          *   00006219
      *   ANALISTA.:IVAN PETRUCCI                  - INSTRUTOR      *   00006319
      *   DATA ....:27/05/2022                                      *   00006419
      *-------------------------------------------------------------*   00006519
      *   OBJETIVO: LER TODOS OS REGISTROS DO ARQUIVO, CLASSIFICAR  *   00006619
      *      E SOMAR SALARIO POR SETOR, AO FINAL GERAR SOMA         *   00006719
      *   DE TODOS SALARIOS E GRAVAR EM UM ARQUIVO DE RELATORIO.    *   00006819
      *-------------------------------------------------------------*   00006919
      *   ARQUIVOS...:                                              *   00007019
      *    DDNAME              I/O                 INCLUDE/BOOK     *   00007119
      *    CLIENTES             I                  #CLIENTE         *   00007219
      *                                            -----------      *   00007319
      *-------------------------------------------------------------*   00007419
      *   MODULOS....:                             INCLUDE/BOOK     *   00007519
      *   GRAVALOG -   TRATAMENTO DE ERROS          VARERROS        *   00007619
      *=============================================================*   00007719
                                                                        00012000
      *=============================================================*   00013000
       ENVIRONMENT                               DIVISION.              00014000
      *=============================================================*   00015000
                                                                        00015100
      *=============================================================*   00015200
       CONFIGURATION                               SECTION.             00015300
      *=============================================================*   00015400
       SPECIAL-NAMES.                                                   00015500
           DECIMAL-POINT IS COMMA.                                      00015600
                                                                        00015700
                                                                        00015800
       INPUT-OUTPUT                                SECTION.             00015900
       FILE-CONTROL.                                                    00016000
           SELECT CLIENTES  ASSIGN TO CLIENTES                          00016101
               FILE STATUS  IS WRK-FS-CLIENTES.                         00016201
                                                                        00016300
      *=============================================================*   00016400
       DATA                                      DIVISION.              00016500
      *=============================================================*   00016600
       FILE                                      SECTION.               00016700
       FD CLIENTES                                                      00016801
           RECORDING  MODE IS F                                         00016900
           BLOCK CONTAINS 0 RECORDS.                                    00017000
                                                                        00017100
      *-------------------------------------------------------------*   00017218
       COPY '#CLIENTE'.                                                 00017315
                                                                        00018002
      *=============================================================*   00018100
       WORKING-STORAGE                             SECTION.             00019000
      *=============================================================*   00020000
                                                                        00020100
       01 FILLER         PIC X(64) VALUE                                00020314
           '-----------WRK-CLIENTES-TAMANHO 64--------------'.          00020417
      *-------------------------------------------------------------*   00020514
       01 WRK-CLIENTES.                                                 00020606
          05 WRK-AGENCIA     PIC 9(04).                                 00020706
          05 FILLER          PIC X(05).                                 00020813
          05 WRK-CONTA       PIC 9(05).                                 00020906
          05 FILLER          PIC X(03).                                 00021013
          05 WRK-CLIENTE     PIC X(30).                                 00021106
          05 WRK-VALOR       PIC 9(08).                                 00021206
          05 FILLER          PIC X(04).                                 00021306
          05 WRK-TIP-OP      PIC X(01).                                 00021406
          05 FILLER          PIC X(04).                                 00021506
                                                                        00021606
       01 FILLER         PIC X(64) VALUE                                00021714
           '-----------CABECALHO TAMANHO 64--------------'.             00021817
                                                                        00021906
       01 WRK-CABEC.                                                    00022006
          05 WRK-CABEC1.                                                00022106
            10 FILLER   PIC X(13).                                      00022207
            10 FILLER   PIC X(37) VALUE                                 00022306
               'RELATORIO DE FUNCIONARIOS       PAG: '.                 00022406
            10 WRK-PAG  PIC 9(02) VALUE 1.                              00022506
            10 FILLER   PIC X(12).                                      00022607
          05 WRK-CABEC2.                                                00022706
            10 FILLER   PIC X(09) VALUE 'AGENCIA  '.                    00022812
            10 FILLER   PIC X(08) VALUE 'CONTA   '.                     00022912
            10 FILLER   PIC X(07) VALUE 'CLIENTE'.                      00023008
            10 FILLER   PIC X(23).                                      00023108
            10 FILLER   PIC X(12) VALUE 'VALOR       '.                 00023210
            10 FILLER   PIC X(09) VALUE 'TIPO OP  '.                    00023308
                                                                        00023423
       01 FILLER         PIC X(64) VALUE                                00023523
           '-----------CONTADOR DE LINHA-----------------'.             00023623
       77 WRK-LINHA       PIC 9(02) VALUE 1.                            00023723
                                                                        00023808
       01 FILLER         PIC X(64) VALUE                                00023914
           '-----------TESTAR STATUS DOS ARQUIVOS--------'.             00024017
                                                                        00024108
       77 WRK-FS-CLIENTES PIC 9(02).                                    00024208
      *-------------------------------------------------------------*   00024314
                                                                        00024508
       01 FILLER         PIC X(64) VALUE                                00024614
           '-----------ACUMULAR REGISTROS LIDOS----------'.             00024717
       77 WRK-ACUM-LIDOS  PIC 9(03).                                    00024808
      *-------------------------------------------------------------*   00024914
                                                                        00025008
       01 FILLER         PIC X(64) VALUE                                00025120
           '-----------CHAMAR TRATA ERROS----------------'.             00025220
                                                                        00025320
       77 WRK-GRAVALOG   PIC X(08) VALUE 'GRAVALOG'.                    00025420
      *------------BOOK VARIAVEIS ERROS-----------------------------*   00025508
                                                                        00025608
       COPY 'VARERROS'.                                                 00025708
                                                                        00025808
      *=============================================================*   00025908
       PROCEDURE DIVISION.                                              00026008
      *=============================================================*   00026108
                                                                        00026208
      *-------------------------------------------------------------*   00026308
       0000-PRINCIPAL                         SECTION.                  00026408
      *-------------------------------------------------------------*   00026508
            PERFORM 1000-INICIAR.                                       00026608
            PERFORM 1100-TESTARSTATUS.                                  00026719
            PERFORM 2000-PROCESSAR UNTIL WRK-FS-CLIENTES EQUAL 10.      00026808
            PERFORM 3000-FINALIZAR.                                     00026908
            STOP RUN.                                                   00027008
                                                                        00027108
                                                                        00027208
                                                                        00027308
      *-------------------------------------------------------------*   00027408
       1000-INICIAR                           SECTION.                  00027508
      *-------------------------------------------------------------*   00027608
            OPEN INPUT CLIENTES.                                        00027708
                                                                        00027808
                                                                        00027908
       1000-99-FIM.               EXIT.                                 00028008
                                                                        00028108
      *-------------------------------------------------------------*   00028208
       1100-TESTARSTATUS                      SECTION.                  00028308
      *-------------------------------------------------------------*   00028408
               IF WRK-FS-CLIENTES NOT EQUAL 0                           00028508
                MOVE '1000'                  TO WRK-SECAO               00028608
                MOVE 'ERRO OPEN CLIENTES - ' TO WRK-MSGERRO             00028720
                MOVE WRK-FS-CLIENTES         TO WRK-STATUS              00028808
                   PERFORM 9000-TRATARERROS                             00028908
               END-IF.                                                  00029021
               PERFORM 1200-CABECALHO.                                  00029122
                                                                        00029508
       1100-99-FIM.               EXIT.                                 00029608
                                                                        00029708
      *-------------------------------------------------------------*   00029821
       1200-CABECALHO                         SECTION.                  00029921
      *-------------------------------------------------------------*   00030021
              IF WRK-FS-CLIENTES NOT EQUAL 10                           00030348
                DISPLAY WRK-CABEC1 '-' WRK-LINHA                        00030543
                DISPLAY WRK-CABEC2 '-' WRK-LINHA                        00030643
                ADD 2 TO WRK-LINHA                                      00030743
              ELSE                                                      00030843
              DISPLAY ' FIM DO RELATORIO '                              00030943
              STOP RUN                                                  00031043
              END-IF.                                                   00031143
               READ CLIENTES.                                           00031250
                                                                        00031343
       1000-99-FIM.               EXIT.                                 00031443
                                                                        00031543
      *-------------------------------------------------------------*   00031643
       2000-PROCESSAR                         SECTION.                  00031743
      *-------------------------------------------------------------*   00031843
              ADD 1 TO WRK-ACUM-LIDOS.                                  00031936
                IF WRK-FS-CLIENTES EQUAL 10                             00032008
                  DISPLAY ' FINAL DE ARQUIVO'                           00032108
                ELSE                                                    00032208
                 MOVE FD-AGENCIA TO WRK-AGENCIA                         00032311
                 MOVE FD-CONTA   TO WRK-CONTA                           00032411
                 MOVE FD-CLIENTE TO WRK-CLIENTE                         00032511
                 MOVE FD-VALOR   TO WRK-VALOR                           00032611
                 MOVE FD-TIP-OP  TO WRK-TIP-OP                          00032711
                 ADD 1 TO WRK-LINHA                                     00032825
                  DISPLAY WRK-CLIENTES '-' WRK-LINHA                    00032923
                END-IF.                                                 00033011
                  IF WRK-LINHA EQUAL 8                                  00033126
                   MOVE 1 TO WRK-LINHA                                  00033226
                    ADD 1 TO WRK-PAG                                    00033327
                     PERFORM 1200-CABECALHO                             00033447
                  ELSE                                                  00033550
                   READ CLIENTES                                        00033650
                  END-IF.                                               00033823
                                                                        00034111
       2000-99-FIM.               EXIT.                                 00034211
                                                                        00034311
      *-------------------------------------------------------------*   00034411
       3000-FINALIZAR                         SECTION.                  00034511
      *-------------------------------------------------------------*   00034611
               CLOSE CLIENTES.                                          00034711
               DISPLAY 'LIDOS.... ' WRK-ACUM-LIDOS.                     00034811
                                                                        00034911
       3000-99-FIM.               EXIT.                                 00035011
                                                                        00035111
      *-------------------------------------------------------------*   00035211
       9000-TRATARERROS                       SECTION.                  00035311
      *-------------------------------------------------------------*   00035411
             MOVE 'RELACLI'  TO WRK-PROGRAMA                            00035519
             CALL 'GRAVALOG' USING WRK-DADOS-ERROS.                     00035611
                 GOBACK.                                                00035711
                                                                        00036000
       9000-99-FIM.               EXIT.                                 00040000
