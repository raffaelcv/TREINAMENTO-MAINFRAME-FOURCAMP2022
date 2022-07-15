//FOUR019X JOB 'EXEC COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010001
//EXECUTAR EXEC PGM=ARQ1904                                             00020016
//STEPLIB  DD DSN=PR.FOUR019.OBJ,DISP=SHR                               00030000
//CLIENTES DD DSN=PR.FOUR019.CLIENTES,DISP=SHR                          00031006
//SYSIN    DD *                                                         00040000
