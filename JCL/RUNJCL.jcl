//VIBERUN  JOB (VIBEGARDEN),'RUN',CLASS=A,MSGCLASS=X
// SET LOADLIB='Z88264.ZMYPRSNL.LOAD'
// SET PGMNAME='CALCDVOP'
//*---------------------------------------------------------
//* RUN AN EXISTING LOAD MODULE FROM VIBEGARDEN LIBRARY
//*---------------------------------------------------------
//RUNSTEP  EXEC PGM=&PGMNAME
//STEPLIB  DD DSN=&LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSIN DD *
/*
