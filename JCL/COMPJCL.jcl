//VIBEJOB  JOB  (VIBEGARDEN),'COMPILE',CLASS=A,MSGCLASS=X
//* Define valiables here
// SET SRCLIB='Z88264.ZMYPRSNL.COBOL'
// SET LOADLIB='Z88264.ZMYPRSNL.LOAD'
// SET PGMNAME='CALCDVOP'
//**************************************************************
//* STEP 1: COMPILE, LINK, AND GO (IGYWCLG)
//**************************************************************
//*
//STEP01   EXEC IGYWCL
//* REFERENCE SYMBOLS USING &SYMBOL.
//COBOL.SYSIN  DD DSN=&SRCLIB(&PGMNAME),DISP=SHR
//LKED.SYSLMOD DD DSN=&LOADLIB(&PGMNAME),DISP=SHR
//*LKED.SYSLMOD DD DSN=Z88264.LOAD(HELLO),DISP=(NEW,CATLG),
//*             UNIT=SYSDA,SPACE=(CYL,(10,10,50)),
//*             DSNTYPE=LIBRARY,
//*             DCB=(RECFM=U,LRECL=0,BLKSIZE=32760)

//*SYSOUT   DD SYSOUT=*
