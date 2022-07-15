//FOUR019X JOB 'COMP COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010000
//EXECUTAR EXEC PGM=ARQ1906                                             00020010
//STEPLIB  DD DSN=PR.FOUR019.OBJ,DISP=SHR                               00030000
//ALUNOS   DD DSN=PR.FOUR019.ALUNOS,DISP=SHR                            00040029
//RELREC   DD SYSOUT=*                                                  00041028
//LOGERROS DD DSN=PR.FOUR019.LOGERROS,DISP=OLD                          00050020
