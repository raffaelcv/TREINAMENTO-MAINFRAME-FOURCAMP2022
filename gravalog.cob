      *====================================================
       IDENTIFICATION                          DIVISION.
      *====================================================

      *====================================================
       PROGRAM-ID. GRAVALOG.
      *====================================================
      *  AUTOR:RAFAEL
      *  EMPRESA: FOURSYS
      *  OBJETIVO: GRAVA ERROS DOS PROGRAMAS.
      *====================================================
       ENVIRONMENT                             DIVISION.
      *====================================================
      *====================================================
       CONFIGURATION                            SECTION.
      *====================================================

       SPECIAL-NAMES.
             DECIMAL-POINT  IS COMMA.


      *====================================================
       INPUT-OUTPUT                             SECTION.
      *====================================================
       FILE-CONTROL.
            SELECT LOGERROS ASSIGN TO LOGERROS

            FILE STATUS IS WRK-FS-LOGERROS.

      *====================================================
       DATA                                    DIVISION.
      *====================================================

      *====================================================
       FILE                                     SECTION.

       FD LOGERROS
           RECORDING MODE IS F
             BLOCK CONTAINS 0 RECORDS.

      *====================================================

      *---------LRECL58-------------
       01 FD-LOGERROS PIC X(58).


      *====================================================
       WORKING-STORAGE SECTION.

       77 WRK-FS-LOGERROS PIC 9(02).
       77 WRK-SIS-DATA    PIC 9(08).
       77 WRK-SIS-HORA    PIC X(06).

      *====================================================
       LINKAGE                                  SECTION.

       01 COMMAREA.
           05 LNK-PROGRAMA PIC X(08).
           05 LNK-SECAO    PIC X(04).
           05 LNK-MSGERRO  PIC X(30).
           05 LNK-STATUS   PIC X(02).
           05 LNK-DATA     PIC 9(08).
           05 LNK-HORA     PIC 9(06).

      *====================================================
       PROCEDURE   DIVISION USING COMMAREA.

      *----------------------------------------------------
       0000-PRINCIPAL                           SECTION.
      *----------------------------------------------------
           OPEN EXTEND LOGERROS.

           ACCEPT WRK-SIS-DATA FROM DATE YYYYMMDD.
           ACCEPT WRK-SIS-HORA FROM TIME.
            MOVE WRK-SIS-DATA TO LNK-DATA.
            MOVE WRK-SIS-HORA TO LNK-HORA.

             MOVE COMMAREA TO FD-LOGERROS.

             WRITE FD-LOGERROS.
            CLOSE LOGERROS.
            GOBACK.
