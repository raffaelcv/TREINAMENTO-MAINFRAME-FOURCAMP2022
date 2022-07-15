//FOUR019X JOB 'EXEC COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010000
//STEP01   EXEC PGM=SORT                                                00011000
//SORTIN   DD DSN=PR.FOUR019.FUNC2,DISP=SHR                             00012000
//SORTOUT  DD DSN=PR.FOUR019.FUNC2,DISP=OLD                             00012100
//SYSOUT   DD SYSOUT=*                                                  00012200
//SYSIN    DD *                                                         00012300
 SORT FIELDS=(36,04,CH,A)                                               00012400
//*-------------------------------                                      00012500
//* EXECUTAR RELFUNC                                                    00012600
//*-------------------------------                                      00012700
//STEP02   EXEC PGM=ARQ1907C                                            00012800
//STEPLIB  DD DSN=PR.FOUR019.OBJ,DISP=SHR                               00012900
//FUNC2    DD DSN=PR.FOUR019.FUNC2,DISP=SHR                             00013000
//LOGERROS DD DSN=PR.FOUR019.LOGERROS,DISP=SHR                          00014000
//RELAT    DD DSN=PR.FOUR019.RELAT,DISP=MOD                             00014102
//*RELAT    DD DSN=PR.FOUR019.RELAT,DISP=(NEW,CATLG,DELETE),            00015002
//*CRIAR ARQ SPACE=(TRK,(1,1)),                                         00016002
//*PELO JCL DCB=(RECFM=FB,LRECL=59,BLKSIZE=0)                           00017002
