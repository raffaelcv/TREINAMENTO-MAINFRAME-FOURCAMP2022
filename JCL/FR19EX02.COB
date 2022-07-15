//FOUR019X JOB 'COMP COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010003
//STEP01   EXEC PGM=SORT                                                00030002
//SORTIN   DD DSN=PR.FOUR019.FUNC,DISP=SHR                              00040004
//SORTOUT  DD DSN=PR.FOUR019.FUNC,DISP=OLD                              00050004
//SYSOUT   DD SYSOUT=*                                                  00060002
//SYSIN    DD *                                                         00070002
 SORT FIELDS=(01,05,CH,A)                                               00080002
/*                                                                      00090002
//STEP02   EXEC PGM=SORT                                                00110002
//SORTIN   DD DSN=PR.FOUR019.PROJ,DISP=SHR                              00120004
//SORTOUT  DD DSN=PR.FOUR019.PROJ,DISP=OLD                              00130004
//SYSOUT   DD SYSOUT=*                                                  00140002
//SYSIN    DD *                                                         00150002
 SORT FIELDS=(01,05,CH,A)                                               00160002
/*                                                                      00170002
//STEP03   EXEC PGM=FR19EX02                                            00190002
//STEPLIB  DD DSN=PR.FOUR019.OBJ,DISP=SHR                               00200002
//FUNC     DD DSN=PR.FOUR019.FUNC,DISP=SHR                              00210009
//PROJ     DD DSN=PR.FOUR019.PROJ,DISP=SHR                              00220009
//LOGERROS DD DSN=PR.FOUR019.LOGERROS,DISP=SHR                          00230002
//FUNPROJ  DD SYSOUT=*                                                  00240002
