//FOUR019X JOB 'EXEC COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010000
//EXECUTAR EXEC PGM=ARQ1905                                             00020003
//STEPLIB  DD DSN=PR.FOUR019.OBJ,DISP=SHR                               00030000
//FUNC     DD DSN=PR.FOUR019.FUNC,DISP=SHR                              00031004
//SYSIN    DD *                                                         00032000
