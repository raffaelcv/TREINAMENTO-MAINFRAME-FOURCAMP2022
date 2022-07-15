      *====================================================             00010000
       IDENTIFICATION                           DIVISION.               00020000
      *====================================================             00030000
       PROGRAM-ID. EXEDESAF.                                            00040000
                                                                        00050000
      *====================================================             00060000
      *  AUTOR: RAFAEL                                                  00070000
      * EMPRESA: FOURSYS                                                00080000
      *----------------------------------------------------             00090000
      *                                                                 00100000
      * OBJETIVO: 4 RELOGIOS BIPAR AO MESMO TEMPO                       00110000
      *====================================================             00120000
       ENVIRONMENT                              DIVISION.               00130000
      *====================================================             00140000
       CONFIGURATION                            SECTION.                00150000
       SPECIAL-NAMES.                                                   00160000
           DECIMAL-POINT IS COMMA.                                      00170000
      *====================================================             00180000
       DATA                                     DIVISION.               00190000
      *====================================================             00200000
       WORKING-STORAGE                          SECTION.                00210000
       01 WRK-RELOGIOS.                                                 00220000
          05 WRK-RA            PIC 9(05) VALUE 120.                     00230046
          05 WRK-RB            PIC 9(05) VALUE 150.                     00240046
          05 WRK-RC            PIC 9(05) VALUE 180.                     00250046
          05 WRK-RD            PIC 9(05) VALUE 270.                     00260046
       77 WRK-RESDIV           PIC 9(05) .                              00270047
       77 WRK-RESTO            PIC 9(05) .                              00271047
       77 WRK-CONTADOR         PIC 9(05) VALUE 2.                       00280044
       77 WRK-CONTRESTO        PIC 9(05) .                              00290012
      *====================================================             00300000
       PROCEDURE                                DIVISION.               00310000
      *====================================================             00320000
                                                                        00340000
      *----------------------------------------------------             00350000
       0000-INICIALIZAR                         SECTION.                00360000
      *----------------------------------------------------             00370000
                                                                        00380000
             PERFORM 1000-INICIALIZAR.                                  00390000
             PERFORM 2000-PROCESSAR UNTIL WRK-CONTRESTO EQUAL 1.        00400000
             PERFORM 3000-FINALIZAR.                                    00410000
             STOP RUN.                                                  00420000
                                                                        00430000
       0000-99-FIM.            EXIT.                                    00440000
                                                                        00450000
      *----------------------------------------------------             00460000
       1000-INICIALIZAR                         SECTION.                00470000
      *----------------------------------------------------             00480000
                                                                        00490000
      *    DISPLAY WRK-RELOGIOS.                                        00500010
                                                                        00510000
       1000-99-FIM.            EXIT.                                    00520000
                                                                        00530000
      *----------------------------------------------------             00540000
       2000-PROCESSAR                           SECTION.                00550000
      *----------------------------------------------------             00560000
            DIVIDE WRK-CONTADOR BY WRK-RA GIVING WRK-RESTO              00570047
                                REMAINDER WRK-RESDIV.                   00571047
            IF                                                          00610000
            WRK-RESDIV EQUAL 0                                          00611048
            DIVIDE WRK-CONTADOR BY WRK-RB GIVING WRK-RESTO              00611147
                                REMAINDER WRK-RESDIV                    00611247
             IF WRK-RESDIV EQUAL 0                                      00611548
            DIVIDE WRK-CONTADOR BY WRK-RC GIVING WRK-RESTO              00611647
                                REMAINDER WRK-RESDIV                    00611747
               IF WRK-RESDIV EQUAL 0                                    00672148
              DIVIDE WRK-CONTADOR BY WRK-RD GIVING WRK-RESTO            00672247
                                 REMAINDER WRK-RESDIV                   00672347
                 IF WRK-RESDIV EQUAL 0                                  00672548
                  DISPLAY 'BEPARAM JUNTOS EM: ' WRK-CONTADOR            00672647
                   ADD 1 TO WRK-CONTRESTO                               00672747
                 END-IF                                                 00672847
               END-IF                                                   00680042
             END-IF                                                     00681042
            END-IF.                                                     00682042
            ADD 1 TO WRK-CONTADOR.                                      00690023
       2000-99-FIM.            EXIT.                                    00700000
                                                                        00710000
      *----------------------------------------------------             00720000
       3000-FINALIZAR                           SECTION.                00730000
      *----------------------------------------------------             00740000
                                                                        00750000
             DISPLAY ' FIM DO PROGRAMA '.                               00760000
                                                                        00770000
       3000-99-FIM.            EXIT.                                    00780000
