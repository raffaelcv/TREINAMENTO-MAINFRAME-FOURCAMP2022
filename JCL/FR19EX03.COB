//FOUR019X JOB 'COMP COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010000
//STEP01   EXEC PGM=SORT                                                00020000
//SORTIN   DD DSN=PR.FOUR019.CLIENTES,DISP=SHR                          00030000
//SORTOUT  DD DSN=PR.FOUR019.CLIENTES,DISP=OLD                          00040000
//SYSOUT   DD SYSOUT=*                                                  00050000
//SYSIN    DD *                                                         00060000
 SORT FIELDS=(01,08,CH,A)                                               00070004
/*                                                                      00080000
//STEP02   EXEC PGM=SORT                                                00090000
//SORTIN   DD DSN=PR.FOUR019.MOV3105,DISP=SHR                           00100001
//SORTOUT  DD DSN=PR.FOUR019.MOV3105,DISP=OLD                           00110001
//SYSOUT   DD SYSOUT=*                                                  00120000
//SYSIN    DD *                                                         00130000
 SORT FIELDS=(01,08,CH,A,47,01,CH,A)                                    00140004
/*                                                                      00150000
//STEP03   EXEC PGM=FR19EX03                                            00160001
//STEPLIB  DD DSN=PR.FOUR019.OBJ,DISP=SHR                               00170000
//CLIENTES DD DSN=PR.FOUR019.CLIENTES,DISP=SHR                          00180002
//MOV3105  DD DSN=PR.FOUR019.MOV3105,DISP=SHR                           00190002
//LOGERROS DD DSN=PR.FOUR019.LOGERROS,DISP=SHR                          00200000
//CLI3105  DD SYSOUT=*                                                  00210001
