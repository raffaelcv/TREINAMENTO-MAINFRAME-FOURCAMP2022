//FOUR019X JOB 'COMP COB',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010000
//EXECUTAR EXEC PGM=FR19EX02                                            00020003
//STEPLIB  DD DSN=PR.FOUR019.OBJ,DISP=SHR                               00030000
//FUNPROJ  DD SYSOUT=*                                                  00040000
//FUNC     DD DSN=PR.FOUR019.FUNC,DISP=SHR                              00041002
//PROJ     DD DSN=PR.FOUR019.PROJ,DISP=SHR                              00041102
//LOGERROS DD DSN=PR.FOUR019.LOGERROS,DISP=OLD                          00042000
