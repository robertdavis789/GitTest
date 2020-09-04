//PFHSXK0T JOB ('OXTBAS17DEV'),'RUN CWKTDB2X',
//       CLASS=A,MSGCLASS=R,NOTIFY=&SYSUID,REGION=0M
//*
//*********************************************************************
//*  EXECUTE CWKTDBSX
//*
//*********************************************************************
//KTDB2X   PROC SDSNLOAD='DSNC10.SDSNLOAD',
//         COBRUN='CEE.SCEERUN',
//         CXLOAD='SYS2.CW.CXR17Z.SLCXLOAD',
//         CXCNTL='SYS2.CW.CXR17Z.SLCXCNTL',
//         TSTLOAD='SALESSUP.SXK1.DEV1.LOAD'
//*****************************************************************
//* RUN THE PROGRAM TO VERIFY IT CAN EXECUTE.                     *
//*****************************************************************
//RUNDB2X      EXEC PGM=IKJEFT01,DYNAMNBR=64,TIME=0005,
//             REGION=4096K
//STEPLIB      DD  DSN=&SDSNLOAD,
//             DISP=SHR
//             DD  DSN=&COBRUN,
//             DISP=SHR
//             DD  DSN=&CXLOAD,
//             DISP=SHR
//             DD  DSN=&TSTLOAD,
//             DISP=SHR
//SYSUDUMP     DD  SYSOUT=*
//SYSOUT       DD  SYSOUT=*
//SYSTSPRT     DD  SYSOUT=*
//EMPFILE  DD  DSN=&CXCNTL(TTTDATD),
//         DISP=SHR
//RPTFILE  DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*
//KTDB2X       PEND
//KTDB2X       EXEC PROC=KTDB2X
//RUNDB2X.SYSTSIN  DD *                                                 03420006
 DSN SYSTEM(DSCC)
 RUN PROGRAM(CWKTDB2X) PLAN(CWKTSK01) PARMS('00001')
 END
/*
//