//FOUR019X JOB 'COMP COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010000
//STEP01   EXEC PGM=SORT                                                00020000
//SORTIN   DD DSN=PR.FOUR019.CLIENTES,DISP=SHR                          00030000
//SORTOUT  DD DSN=PR.FOUR019.CLIENTES,DISP=OLD                          00040000
//SYSOUT   DD SYSOUT=*                                                  00050000
//SYSIN    DD *                                                         00060000
 SORT FIELDS=(01,08,CH,A)                                               00070000
/*                                                                      00080000
//STEP02   EXEC PGM=SORT                                                00090000
//SORTIN   DD DSN=PR.FOUR019.MOV0106,DISP=SHR                           00100000
//SORTOUT  DD DSN=PR.FOUR019.MOV0106,DISP=OLD                           00110000
//SYSOUT   DD SYSOUT=*                                                  00120000
//SYSIN    DD *                                                         00130000
 SORT FIELDS=(01,08,CH,A,47,01,CH,A)                                    00140000
/*                                                                      00150000
//STEP03   EXEC PGM=FR19EX04                                            00160000
//STEPLIB  DD DSN=PR.FOUR019.OBJ,DISP=SHR                               00170000
//CLIENTES DD DSN=PR.FOUR019.CLIENTES,DISP=SHR                          00180000
//MOV0106  DD DSN=PR.FOUR019.MOV0106,DISP=SHR                           00190000
//LOGERROS DD DSN=PR.FOUR019.LOGERROS,DISP=SHR                          00200000
//MOV0106A DD DSN=PR.FOUR019.MOV0106A,DISP=OLD                          00210003
//MOV0106C DD DSN=PR.FOUR019.MOV0106C,DISP=OLD                          00220003
