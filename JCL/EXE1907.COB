//FOUR019X JOB 'COMP COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010000
//EXECUTAR EXEC PGM=RELACLI                                             00020008
//STEPLIB  DD DSN=PR.FOUR019.OBJ,DISP=SHR                               00030000
//RELREC   DD SYSOUT=*                                                  00041000
//CLIENTES DD DSN=PR.FOUR019.CLIENTES,DISP=SHR                          00041108
//LOGERROS DD DSN=PR.FOUR019.LOGERROS,DISP=OLD                          00042000
