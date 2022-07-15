//FOUR019X JOB 'EXEC COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010007
//STEP01   EXEC PGM=SORT                                                00011007
//SORTIN   DD   DSN=GR.FOUR019.SRELBENE,DISP=SHR                        00012007
//SORTOUT  DD   DSN=GR.FOUR019.SRELBENE,DISP=SHR                        00013007
//SYSOUT   DD   SYSOUT=*                                                00014008
  SORT FIELDS=(01,05,CH,A)                                              00016007
                                                                        00017007
//STEP02   EXEC PGM=RELBENE                                             00018007
//STEPLIB  DD   DSN=GR.GERAL.LOADLIB,DISP=SHR                           00019007
//SYSOUT   DD   SYSOUT=*                                                00019107
//SRELBENE DD   DSN=GR.FOUR019.SRELBENE,DISP=SHR                        00019210
//RELABENE DD   SYSOUT=*                                                00019309
