//COBOLGO JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//*
// SET COBPGM='FINALEX'
//**** Compile JCL ******
//*
//** Go (Run) Step. Add //DD cards when needed ******
//GO        EXEC   PGM=&COBPGM.
//STEPLIB   DD DSN=&SYSUID..FINALS.LOAD,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//PARTSUPP  DD DSN=&SYSUID..FINALS.PARTSUPP,DISP=SHR
//STATEZIP  DD DSN=&SYSUID..FINALS.STATEZIP,DISP=SHR
//ERRFILE   DD SYSOUT=*
//*WARNING   DD SYSOUT=*
//WARNING   DD DSN=&SYSUID..FINALS.WARNINGS,DISP=SHR
//PARTS     DD DSN=&SYSUID..FINALS.PARTS,DISP=SHR
//*PARTS     DD SYSOUT=*
//ADDR      DD DSN=&SYSUID..FINALS.ADDRS,DISP=SHR
//*ADDR      DD SYSOUT=*
//PURCHASE  DD DSN=&SYSUID..FINALS.PURCHASE,DISP=SHR
//*PURCHASE  DD SYSOUT=*
//CEEDUMP   DD DUMMY
//SYSUDUMP   DD DUMMY
//******* ADDITIONAL RUNTIME JCL HERE ******
//******* ADDITIONAL RUNTIME JCL HERE ******
