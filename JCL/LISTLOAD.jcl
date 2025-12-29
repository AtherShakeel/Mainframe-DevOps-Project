//VIBELIST JOB (VIBEGARDEN),'LIST',CLASS=A,MSGCLASS=X
//*---------------------------------------------------------
//* THIS JOB TELLS US THE TRUTH ABOUT YOUR LOAD LIBRARY
//*---------------------------------------------------------
//STEP1    EXEC PGM=IKJEFT01
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
  LISTDS 'Z88264.LOAD' MEMBERS
/*
