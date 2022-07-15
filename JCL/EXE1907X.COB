//FOUR019X JOB 'EXEC COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010004
//STEP01   EXEC PGM=SORT                                                00011003
//SORTIN   DD DSN=PR.FOUR019.FUNC2,DISP=SHR                             00012004
//SORTOUT  DD DSN=PR.FOUR019.FUNC2,DISP=OLD                             00012104
//SYSOUT   DD SYSOUT=*                                                  00012203
//SYSIN    DD *                                                         00012303
 SORT FIELDS=(36,04,CH,A)                                               00012409
//*-------------------------------                                      00012503
//* EXECUTAR RELFUNC                                                    00012603
//*-------------------------------                                      00012703
//STEP02   EXEC PGM=ARQ1907C                                            00012811
//STEPLIB  DD DSN=PR.FOUR019.OBJ,DISP=SHR                               00012903
//FUNC2    DD DSN=PR.FOUR019.FUNC2,DISP=SHR                             00013003
//LOGERROS DD DSN=PR.FOUR019.LOGERROS,DISP=SHR                          00014003
