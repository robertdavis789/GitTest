
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WBCI1206.
      ******************************************************************
      *                                                                *
      ******      C O M P U W A R E   C O R P O R A T I O N       ******
      *                                                                *
      *  PROGRAM MERGES DATA FROM THE EMPLOYEE MASTER FILE AND         *
      *  THE EMPLOYEE YEAR-TO-DATE TABLE TO PRODUCE THE EMPLOYEE       *
      *  STATUS FILE.                                                  *
      *                                                                *
      *  INPUT FILE  - EMPLOYEE MASTER FILE (VSAM)                     *
      *              - EMPLOYEE YTD TABLE   (DB2)                      *
      *  OUTPUT FILE - EMPLOYEE STATUS FILE (SEQUENTIAL).              *
      *                                                                *
      *                                                                *
      *  SUBSEQUENTLY THIS PROGRAM THEN DOES SOME EXTRA WORK TO        *
      *  GENERATE TRAFFIC TO SHOW WITH THE RUNTIME VISUALIZER.         *
      *  PERTINENT FILES ARE ALL IN HSTJXL0.RVII.WEBCAST.CNTL          *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE
               ASSIGN TO EMPFILE
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS EMP-KEY
               FILE STATUS IS WS-SYSUT1-STATUS.
           SELECT REPORT-FILE   ASSIGN TO RPTFILE.
           SELECT EMPLOYEE-FILE2 ASSIGN TO EMPFILE2.
           SELECT REPORT-FILE2  ASSIGN TO RPTFILE2.
           SELECT EMPSTAT-FILE  ASSIGN TO EMPSTAT.
           SELECT INP-FILE ASSIGN TO EMPINP.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE
           RECORD CONTAINS 198 CHARACTERS.
       01  EMPLOYEE-RECORD.
           05 EMP-KEY                 PIC X(5).
           05 EMP-STUFF               PIC X(193).
       FD  EMPSTAT-FILE
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01  EMPLOYEE-RECORD.
           05 STAT-DATA               PIC X(80).
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  REPORT-RECORD              PIC X(80).
       FD  EMPLOYEE-FILE2
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  EMPLOYEE-REC2              PIC X(80).
       FD  REPORT-FILE2
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  REPORT-RECORD2             PIC X(80).
       FD  INP-FILE
           RECORD CONTAINS 60 CHARACTERS
           RECORDING MODE IS F.
       01  SALES-INPUT PIC X(60).
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
      ******************************************************************
      * DCLGEN TABLE(LAB3)                                             *
      *        LIBRARY                                                 *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE RVIIEMP TABLE
           ( EMPNO                          CHAR(5)      NOT NULL,
             LASTNAME                       CHAR(15)     NOT NULL,
             FIRSTNAME                      CHAR(10)     NOT NULL,
             MIDINIT                        CHAR(1),
             YTDSAL                         DECIMAL(10, 2),
             MAXNATL                        DECIMAL(10, 2)
           ) END-EXEC.
           EXEC SQL DECLARE RVIICHK TABLE
           ( ALTEMP                         CHAR(5)      NOT NULL,
             ALTLAST                        CHAR(15)     NOT NULL,
             ALTFIRST                       CHAR(10)     NOT NULL,
             ALTMID                         CHAR(1),
             ALTYTD                         DECIMAL(10, 2),
             ALTMAX                         DECIMAL(10, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LAB3                               *
      ******************************************************************
       01  DCLLAB3.
      *    *************************************************************
           10 EMPNO                PIC  X(5).
      *    *************************************************************
           10 LASTNAME             PIC  X(15).
      *    *************************************************************
           10 FIRSTNAME            PIC  X(10).
      *    *************************************************************
           10 MIDINIT              PIC  X(1).
      *    *************************************************************
           10 YTDSAL               PIC S99999999V99 USAGE COMP-3.
      *    *************************************************************
           10 MAXNATL              PIC S99999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LAB3ALT                            *
      ******************************************************************
       01  DCLALT3.
      *    *************************************************************
           10 ALTEMP               PIC  X(5).
      *    *************************************************************
           10 ALTLAST              PIC  X(15).
      *    *************************************************************
           10 ALTFIRST             PIC  X(10).
      *    *************************************************************
           10 ALTMID               PIC  X(1).
      *    *************************************************************
           10 ALTYTD               PIC S99999999V99 USAGE COMP-3.
      *    *************************************************************
           10 ALTMAX               PIC S99999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
      ******************************************************************
      * RECORD LAYOUTS FOR EMPFILE AND EMPSTAT FILES.                  *
      ******************************************************************
       COPY EMPC.
       COPY STATC.
 *    ************************************************************
       01  WS-SYSUT1-STATUS           PIC XX       VALUE '  '.
       01  WS-SALES-RECORD            PIC X(60).
       01  STATUS-DATA                PIC X(80).
       01  SWITCHES.
           05  FIRST-TIME-SW          PIC X        VALUE 'Y'.
               88  NOT-FIRST-TIME                  VALUE 'N'.
           05  EOF-SW                 PIC X        VALUE 'N'.
               88  END-OF-FILE                     VALUE 'Y'.
               88  NOT-END-OF-FILE                 VALUE 'N'.

       01  SQLERROR.
           05  FILLER                 PIC X(29)
                   VALUE '*** SQL ERROR DETECTED ***'.
           05  SQLECODE               PIC X(7).

       01  ACTIVE-NOTE.
           05  FILLER                 PIC X(3)
                   VALUE 'AT '.
           05  CURRENT-WITHHOLD       PIC $$$$$9.99.
           05  FILLER                 PIC X(5)
                   VALUE ' MAX '.
           05  MAX-WITHHOLD           PIC $$$$$$9.99.
           05  FILLER                 PIC X(20) VALUE SPACES.

       01  WS-RETURN-CODE          PIC 9(4) USAGE COMP.
       01  YTD-WITHHOLD            PIC S99999999V99 USAGE COMP-3.
       01  EMP-NUM                 PIC X(5).
       01  PARM-INTERFACE.
           03  PARM-IDENT             PIC X(6) VALUE 'SNPRML'.
           03  PARM-ACTION            PIC 9    VALUE ZERO.
               88  PRINT-ALL-SECTIONS          VALUE 0.
               88  PRINT-SECTION               VALUE 1.
               88  SUPPRESS-SECTION            VALUE 2.
      *            0 FULL SNAPAID REPORT (NO CUSTOMIZATION)
      *            1 PRINT SELECTED SECTIONS
      *            2 SUPPRESS SELECTED SECTIONS
           03  PARM-SECTION-SELECTION.
               05  NSI-SECTION        PIC 9    VALUE ZERO.
                   88 DO-NOT-APPLY-PARM-ACTION VALUE 0.
                   88 APPLY-PARM-ACTION        VALUE 1.
      *               0 DO NOT APPLY PARM-ACTION TO THIS SECTION
      *               1 APPLY PARM-ACTION TO THIS SECTION
               05  REGISTERS-PSW      PIC 9    VALUE ZERO.
               05  TRACE-SUMMARY      PIC 9    VALUE ZERO.
               05  PROGRAM-STORAGE    PIC 9    VALUE ZERO.
               05  FILES-SECTION      PIC 9    VALUE ZERO.
               05  IMS-SECTION        PIC 9    VALUE ZERO.
               05  IDMS-SECTION       PIC 9    VALUE ZERO.
               05  DB2-SECTION        PIC 9    VALUE ZERO.
               05  SORT-SECTION       PIC 9    VALUE ZERO.
               05  FILLER             PIC 9    VALUE ZERO.
               05  FILLER             PIC 9    VALUE ZERO.
           03  PARM-COMMENT           PIC X(10) VALUE SPACE.

       01  WS-RETURN-CODE          PIC 9(4) USAGE COMP.

       01  SWITCHES.
           05  JUMP-SW                PIC X        VALUE 'N'.
               88  JUMPING                         VALUE 'Y'.
           05  EOF-SW2                PIC X        VALUE 'N'.
               88  END-OF-FILE2                    VALUE 'Y'.
           05  REGION-ERROR-SW        PIC X        VALUE 'N'.
               88  INVALID-REGION                  VALUE 'Y'.
               88  VALID-REGION                    VALUE 'N'.
           05  PARM-ERROR-SW          PIC X        VALUE 'N'.
               88  BAD-PARM                        VALUE 'Y'.
               88  GOOD-PARM                       VALUE 'N'.
           05  END-OF-MONTH-SW        PIC X        VALUE 'N'.
               88  END-OF-MONTH                    VALUE 'Y'.
       01  COUNTERS.
           05  PAGE-COUNT             PIC 9(3)     VALUE 1.
           05  EMP-LINE-COUNT         PIC S99      VALUE +56.
           05  REG-LINE-COUNT         PIC S99      VALUE +56.
           05  START-NUMBER           PIC 999.
           05  RECORDS-READ           PIC 999      VALUE 0.
           05  NORTH-COUNT            PIC 9(2)     VALUE 0.
           05  SOUTH-COUNT            PIC 9(2)     VALUE 0.
           05  EAST-COUNT             PIC 9(2)     VALUE 0.
           05  WEST-COUNT             PIC 9(2)     VALUE 0.
           05  ALT-COUNT              PIC 999999 VALUE 0.
           05  ALT-TOTAL              PIC 9(9) COMP-3 VALUE 0.
       01  REGION-SUB                 PIC 9        VALUE 0.
       01  YRS-OF-SERVICE             PIC 99       VALUE 0.
       01  TODAYS-DATE                PIC X(6).
       01  DATE-FIELDS REDEFINES TODAYS-DATE.
           05  DATE-YY                PIC 9(2).
           05  DATE-MM                PIC 9(2).
           05  DATE-DD                PIC 9(2).
********
********  HOLD EMPLOYEE DETAIL PRINT LINES UNTIL READY TO PRINT
********  EMPLOYEE COMPENSATION REPORT.  THE DATA IS STORED BY
********  REGION AND THEN BY SEQUENCE IN EMPLOYEE FILE.
********
       01  HOLD-TABLE.
           05  HOLD-AREA        OCCURS 4 TIMES
                                INDEXED BY REG-IX.
               10  HOLD-LINE    OCCURS 20 TIMES
                                INDEXED BY HOLD-IX.
                   15  HOLD-NAME               PIC X(15).
                   15  HOLD-REGION             PIC X(5).
                   15  HOLD-TYPE               PIC X.
                   15  HOLD-YEARS              PIC 9(2).
                   15  HOLD-HIRE-DATE.
                       20  HOLD-HIRE-YY        PIC 9(2).
                       20  HOLD-HIRE-MM        PIC 9(2).
                       20  HOLD-HIRE-DD        PIC 9(2).
                   15  HOLD-WAGES              PIC 9(5)V99.
                   15  HOLD-OT                 PIC 9(5)V99.
                   15  HOLD-COMM               PIC 9(5)V99.
                   15  HOLD-TOTAL              PIC 9(5)V99.
********
********  STORES THE NAME OF EACH REGION
********
       01  REGION-NAME-TABLE.
           05  FILLER            PIC X(5)    VALUE 'NORTH'.
           05  FILLER            PIC X(5)    VALUE 'SOUTH'.
           05  FILLER            PIC X(5)    VALUE 'EAST '.
           05  FILLER            PIC X(5)    VALUE 'WEST '.
       01  REGION-TABLE     REDEFINES REGION-NAME-TABLE.
           05  REGION-ID         PIC X(5)  OCCURS 4 TIMES.
********
********  STORES REGIONAL INFORMATION THAT IS USED TO PRINT THE
********  REGIONAL SALES REPORT.  REGION SALES IS A SUM OF ALL SALES
********  FOR THE REGION AND IS USED TO CALCULATE MANAGER COMMISSION
********  THE COMMENT FIELD IS USED TO FLAG A REGION IF AN EMPLOYEE IN
********  THE REGION HAS 0 SALES.
********
       01  REGION-SALES-TABLE.
           05  REGION-DATA         OCCURS 4 TIMES.
               10  REGION-NAME       PIC X(5).
               10  REGION-MANAGER    PIC X(15).
               10  REGION-SALARY     PIC 9(4)V99.
               10  REGION-SALES      PIC 9(6)V99.
               10  REGION-COMMENT    PIC X(5).
********
********  FIELDS USED BY CALLED PROGRAM CWXTSUBC TO CALCULATE
********  COMMISSION BASED ON SALES AMOUNT
********
       01  CALC-COMMISSION-FIELDS.
           05  EMP-TYPE              PIC X.
           05  CALC-SALES            PIC 9(6)V99           VALUE 0.
           05  CALC-COMMISSION       PIC 9(5)V99  COMP-3   VALUE 0.
********
********  ACCUMULATORS USED FOR CALCULATING HOURLY EMPLOYEE WAGES,
********  TOTAL EMPLOYEE COMPENSATION (SALARY PLUS COMMISSION OR
********  HOURLY EMPLOYEE WAGES PLUS OVERTIME), AND TOTAL MANAGEMENT
********  COMPENSATION (SALARY PLUS COMMISSION BASED ON TOTAL SALES
********  FOR THE REGION)
********
       01  TOTAL-FIELDS.
           05  EMP-WAGES             PIC 9(5)V99    COMP-3.
           05  EMP-COMPENSATION      PIC 9(5)V99    COMP-3.
           05  MGMT-COMPENSATION     PIC 9(5)V99    COMP-3.
********
********  TOTAL COMPENSATION GIVEN TO ALL EMPLOYEES (HOURLY AND SALES)
********  OR MANAGEMENT.  EACH SUM IS PRINTED AT THE END OF THEIR
********  RESPECTIVE REPORTS.
********
       01  GRAND-TOTAL-FIELDS.
           05  GRAND-TOTAL-EMP       PIC 9(7)V99   COMP-3  VALUE 0.
           05  GRAND-TOTAL-MGMT      PIC 9(7)V99   COMP-3  VALUE 0.
********
********  USED FOR CALCULATING OVERTIME FOR ANY HOURLY EMPLOYEE
********  WHOSE HOURS EXCEEDS 40
********
       01  OVERTIME-FIELDS.
           05  OT-AMOUNT             PIC 9(5)V99    COMP-3.
           05  OT-HOURS              PIC 9(2).
********
********  EMPLOYEE RECORD WORK-AREA.  EMPLOYEE DATA IS REDEFINED
********  BASED ON ONE OF THE 3 EMPLOYEE TYPES, HOURLY, SALES OR
********  MANAGEMENT.
********
       01  EMPLOYEE-WORK-AREA.
           05  WA-EMP-NUM            PIC 9(5).
           05  WA-EMP-TYPE           PIC X.
               88  HOURLY            VALUE 'H'.
               88  SALES             VALUE 'S'.
               88  MANAGEMENT        VALUE 'M'.
           05  WA-EMP-REGION         PIC 9.
               88  NORTH             VALUE 1.
               88  SOUTH             VALUE 2.
               88  EAST              VALUE 3.
               88  WEST              VALUE 4.
           05  WA-EMP-NAME           PIC X(15).
           05  WA-EMP-ADDRESS.
               10  WA-EMP-STREET     PIC X(15).
               10  WA-EMP-CITY       PIC X(8).
               10  WA-EMP-STATE      PIC XX.
               10  WA-EMP-ZIP        PIC X(9).
           05  WA-HOURLY-EMPLOYEE-DATA.
               10  WA-EMP-HOURS      PIC 9(2).
               10  WA-EMP-RATE       PIC 9(3)V99     COMP-3.
               10  FILLER            PIC X(8).
           05  WA-SALES-EMPLOYEE-DATA   REDEFINES
                                        WA-HOURLY-EMPLOYEE-DATA.
               10  WA-SALES-SALARY   PIC 9(5)V99     COMP-3.
               10  WA-SALES-AMOUNT   PIC 9(5)V99.
               10  FILLER            PIC X(2).
           05  WA-MGMT-EMPLOYEE-DATA   REDEFINES
                                        WA-SALES-EMPLOYEE-DATA.
               10  WA-MGMT-SALARY    PIC 9(5)V99     COMP-3.
               10  FILLER            PIC X(9).
           05  WA-EMP-HIRE-DATE.
               10  WA-EMP-HIRE-YY    PIC 9(2).
               10  WA-EMP-HIRE-MM    PIC 9(2).
               10  WA-EMP-HIRE-DD    PIC 9(2).
           05  FILLER                PIC X(5).
*********
*********  EMPLOYEE COMPENSATION REPORT
*********
       01  EMPLOYEE-HDR1.
           05  FILLER      PIC X         VALUE SPACES.
           05  FILLER      PIC X(10)
                              VALUE  'RUN DATE  '.
           05  EMP-RUN-MM
                           PIC 99.
           05  FILLER      PIC X         VALUE '/'.
           05  EMP-RUN-DD
                           PIC 99.
           05  FILLER      PIC X         VALUE '/'.
           05  EMP-RUN-YY
                           PIC 99.
           05  FILLER      PIC X(7)      VALUE SPACES.
           05  FILLER      PIC X(28)
                              VALUE  'EMPLOYEE COMPENSATION REPORT'.
           05  FILLER      PIC X(18)     VALUE SPACES.
           05  FILLER      PIC X(5)      VALUE 'PAGE '.
           05  EMP-PAGE    PIC ZZ9.
       01  EMPLOYEE-HDR2.
           05  FILLER      PIC X(31)     VALUE SPACES.
           05  FILLER      PIC X(4)      VALUE 'HIRE'.
           05  FILLER      PIC X(2)      VALUE SPACES.
           05  FILLER      PIC X(6)      VALUE 'YRS OF'.
           05  FILLER      PIC X(37)     VALUE SPACES.
       01  EMPLOYEE-HDR3.
           05  FILLER      PIC X         VALUE SPACES.
           05  FILLER      PIC X(13)     VALUE 'EMPLOYEE NAME'.
           05  FILLER      PIC X(3)      VALUE SPACES.
           05  FILLER      PIC X(6)      VALUE 'REGION'.
           05  FILLER      PIC X         VALUE SPACES.
           05  FILLER      PIC X(4)      VALUE 'TYPE'.
           05  FILLER      PIC X(3)      VALUE SPACES.
           05  FILLER      PIC X(4)      VALUE 'DATE'.
           05  FILLER      PIC X(2)      VALUE SPACES.
           05  FILLER      PIC X(7)      VALUE 'SERVICE'.
           05  FILLER      PIC X(2)      VALUE SPACES.
           05  FILLER      PIC X(6)      VALUE 'SALARY'.
           05  FILLER      PIC X(3)      VALUE SPACES.
           05  FILLER      PIC X(3)      VALUE 'O/T'.
           05  FILLER      PIC X(3)      VALUE SPACES.
           05  FILLER      PIC X(10)     VALUE 'COMMISSION'.
           05  FILLER      PIC X(4)      VALUE SPACES.
           05  FILLER      PIC X(5)      VALUE 'TOTAL'.
       01  EMPLOYEE-DTL.
           05  FILLER                  PIC X         VALUE SPACES.
           05  EMP-DTL-NAME            PIC X(15).
           05  FILLER                  PIC X         VALUE SPACES.
           05  EMP-DTL-REGION          PIC X(5).
           05  FILLER                  PIC X(3)      VALUE SPACES.
           05  EMP-DTL-TYPE            PIC X.
           05  FILLER                  PIC X(3)      VALUE SPACES.
           05  EMP-DTL-HIRE-MM         PIC 9(2).
           05  EMP-DTL-SLASH1          PIC X         VALUE SPACES.
           05  EMP-DTL-HIRE-DD         PIC 9(2).
           05  EMP-DTL-SLASH2          PIC X         VALUE SPACES.
           05  EMP-DTL-HIRE-YY         PIC 9(2).
           05  FILLER                  PIC X(3)      VALUE SPACES.
           05  EMP-DTL-YRS-OF-SERVICE  PIC 9(2).
           05  FILLER                  PIC X(2)      VALUE SPACES.
           05  EMP-DTL-WAGES           PIC ZZZZ9.99.
           05  FILLER                  PIC X         VALUE SPACES.
           05  EMP-DTL-OT              PIC ZZZZ9.99.
           05  FILLER                  PIC X(2)      VALUE SPACES.
           05  EMP-DTL-COMM            PIC ZZZZ9.99.
           05  FILLER                  PIC X         VALUE SPACES.
           05  EMP-DTL-TOTAL           PIC ZZZZ9.99.
       01  EMP-TOTAL-DTL.
           05  FILLER            PIC X(4)      VALUE SPACES.
           05  FILLER            PIC X(5)      VALUE 'TOTAL'.
           05  FILLER            PIC X(61)     VALUE SPACES.
           05  EMP-GRAND-TOTAL   PIC ZZZZZZ9.99.
*********
*********  REGIONAL SALES REPORT
*********
       01  REGION-HDR1.
           05  FILLER      PIC X      VALUE SPACES.
           05  FILLER      PIC X(10)  VALUE 'RUN DATE  '.
           05  REG-RUN-MONTH
                           PIC 99.
           05  FILLER      PIC X      VALUE '/'.
           05  REG-RUN-DAY PIC 99.
           05  FILLER      PIC X      VALUE '/'.
           05  REG-RUN-YEAR
                           PIC 99.
           05  FILLER      PIC X(11)  VALUE SPACES.
           05  FILLER      PIC X(21)  VALUE  'REGIONAL SALES REPORT'.
           05  FILLER      PIC X(21)  VALUE SPACES.
           05  FILLER      PIC X(5)   VALUE 'PAGE '.
           05  REG-PAGE    PIC ZZ9.
       01  REGION-HDR2.
           05  FILLER      PIC X      VALUE SPACES.
           05  FILLER      PIC X(7)   VALUE 'MANAGER'.
           05  FILLER      PIC X(9)   VALUE SPACES.
           05  FILLER      PIC X(6)   VALUE 'REGION'.
           05  FILLER      PIC X(3)   VALUE SPACES.
           05  FILLER      PIC X(11)  VALUE 'TOTAL SALES'.
           05  FILLER      PIC X(5)   VALUE SPACES.
           05  FILLER      PIC X(6)   VALUE 'SALARY'.
           05  FILLER      PIC X(5)   VALUE SPACES.
           05  FILLER      PIC X(10)  VALUE 'COMMISSION'.
           05  FILLER      PIC X(3)   VALUE SPACES.
           05  FILLER      PIC X(5)   VALUE 'TOTAL'.
           05  FILLER      PIC X(9)   VALUE SPACES.
       01  REGION-DETAIL.
           05  FILLER             PIC X         VALUE SPACES.
           05  REG-DTL-MANAGER    PIC X(15).
           05  FILLER             PIC X         VALUE SPACES.
           05  REG-DTL-REGION     PIC X(5).
           05  FILLER             PIC X(5)      VALUE SPACES.
           05  REG-DTL-SALES      PIC ZZZZZ9.99.
           05  FILLER             PIC X(5)      VALUE SPACES.
           05  REG-DTL-SALARY     PIC ZZZ9.99.
           05  FILLER             PIC X(5)      VALUE SPACES.
           05  REG-DTL-COMM       PIC ZZZZ9.99.
           05  FILLER             PIC X(3)      VALUE SPACES.
           05  REG-DTL-TOTAL      PIC ZZZZ9.99.
           05  FILLER             PIC X         VALUE SPACES.
           05  REG-DTL-COMMENT    PIC X(5).
       01  MGMT-TOTAL-DTL.
           05  FILLER             PIC X(4)      VALUE SPACES.
           05  FILLER             PIC X(5)      VALUE 'TOTAL'.
           05  FILLER             PIC X(53)     VALUE SPACES.
           05  MGMT-GRAND-TOTAL   PIC ZZZZZZ9.99.
           05  FILLER             PIC X(8)      VALUE SPACES.
*********
*********  ERROR MESSAGE LINE
*********
       01  ERROR-LINE             PIC X(80).
*********
*********  BLANK LINE TO CONTROL SPACING OF REPORTS
*********
       01  BLANK-LINE             PIC X(80)   VALUE SPACES.
*********
       01  LOOP-COUNTER           PIC 99.
       01  LOOP-SO-FAR            PIC 99 VALUE 0.
*********
*********  PARM IS AN OPTIONAL FIELD USED TO START PROCESSING
*********  AT A PARTICULAR RECORD IN THE EMPLOYEE FILE.  VALID
*********  VALUES FOR PARM-DATA ARE:
*********
*********        VALUE           FUNCTION
*********     - SPACES OR 00001  BEGIN PROCESSING FROM FIRST RECORD.
*********                          CAUSES S0C7 ABEND BECAUSE FIRST
*********                          RECORD CONTAINS INVALID DATA.
*********     - 00002            BEGIN PROCESSING FROM SECOND RECORD.
*********                          CAUSES S0C7 ABEND BECAUSE SECOND
*********                          RECORD CONTAINS INVALID REGION
*********                          CREATING TABLE OVERFLOW CONDITION.
*********     - 00003 THRU 00019 BEGIN PROCESSING AT SPECIFIED RECORD
*********                          UNTIL END-OF-FILE.  BYPASSES S0C7
*********                          CAUSED BY RECORDS 00001 AND 00002.
*********     - NON NUMERIC OR   PRINTS ERROR MESSAGE AND SKIPS ALL
*********         < 5 CHARACTERS   PROCESSING.
*********
       01  NAT-DATA       PIC N(20).
       01  NAT-DATA2      PIC N(20) VALUE N"COMPUWARE".
       01  NAT-DATA3      PIC N(20) VALUE NX"00410042".
       01  NAT-DATA4      PIC N(10)
           VALUE NX"005E005F0060006100620063006400650066".
       01  NAT-DATA5      PIC N(08)
           VALUE NX"01000101010201030104010501060107".
       01  NAT-DATA6      PIC N(08)
           VALUE NX"01080109010A010B010C010D010E010F".
       01  NAT-DATA7      PIC N(08)
           VALUE NX"01700171017201730174017501760177".
       01  NAT-DATA8      PIC N(08)
           VALUE NX"01780179017A017B017C017D017E017F".
       01  NAT-DATA9      PIC N(08)
           VALUE NX"30413051306130713081309130423043".
       01  NAT-DATA10      PIC N(08)
           VALUE NX"30503051305230533054305530563057".
       01  NAT-DATA11     PIC N(08)
           VALUE NX"4E104E114E124E134E144E154E164E17".
       01  NAT-DATA12      PIC N(08)
           VALUE NX"4F014F024F03F4044F054F064F074F08".
       01  PGM-NAME.
           05  RVII0040    PIC X(8)      VALUE 'WBCI0040'.
           05  RVWKEVAL    PIC X(8)      VALUE 'RVWKEVAL'.
           05  RVII0080    PIC X(8)      VALUE 'WBCI0080'.
           05  RVII0090    PIC X(8)      VALUE 'WBCI0090'.

           EXEC SQL DECLARE ALT-CURSOR CURSOR FOR
               SELECT *
               FROM RVIICHK
               END-EXEC.

       LINKAGE SECTION.
       01  PARMINFO.
           03  PARM-LTH           PIC S9(4) COMP.
           03  LOOP-FILL          PIC X.
           03  LOOP-CNT           PIC X(2).

       PROCEDURE DIVISION USING PARMINFO.
           CALL RVII0080.
           CALL RVII0080.
           MOVE 'N' TO EOF-SW.
           COMPUTE LOOP-COUNTER = FUNCTION NUMVAL (LOOP-CNT).
           PERFORM 0900-OPEN.
           PERFORM 0800-INPUT.
           PERFORM 0100-PROCESS
               UNTIL END-OF-FILE.
           PERFORM 0900-CLOSE.
           MOVE 'N' TO EOF-SW.
*******    PERFORM 0999-OPEN.
*******    PERFORM 0888-STAT-INPUT.
           PERFORM 0111-PROCESS.
*******             UNTIL END-OF-FILE.
           PERFORM 0999-CLOSE.
           MOVE 'N' TO EOF-SW.
           CALL RVII0090 USING LOOP-COUNTER.


*********
*********  IF YOU WANT THIS DEMO TO EXECUTE CWXTCOB1
*********  UNCOMMENT THE FOLLOWING PERFORM.
*********
*********  PERFORM 0000-MAINLINE.
           GOBACK.
*********
*********

       0111-PROCESS.
           PERFORM DO-A-LOT-OF-SQL.
*********  PERFORM 0888-STAT-INPUT.
*********
*********

       DO-A-LOT-OF-SQL.
           EXEC SQL OPEN ALT-CURSOR END-EXEC.
           PERFORM 0100-FETCH
               UNTIL SQLCODE NOT = 0.
           EXEC SQL CLOSE ALT-CURSOR END-EXEC.
*********
*********

       0100-FETCH.
           ADD 1 TO ALT-TOTAL.
           EXEC SQL FETCH ALT-CURSOR INTO
               :DCLALT3
           END-EXEC.


      *                                                                *
      ******      0100-PROCESS                                    ******
      *                                                                *
      *  READ THE DB2 ROW PERTAINING TO THIS EMPLOYEE AND DETERMINE    *
      *  IF THEY HAVE REACHED THE MAXIMUM NATIONAL TAX WITHHOLDING.    *
      *                                                                *
      *  STATUS:     COMPLETE - THEY HAVE REACHED THE MAXIMUM.         *
      *              ACTIVE   - THEY HAVE NOT REACHED THE MAXIMUM.     *
      *              INVALID  - ERROR PROCESSING THE DATA.             *
      *                                                                *
      ******************************************************************
       0100-PROCESS.
           MOVE SPACES TO DCLLAB3.
           MOVE SPACES TO EMP-NOTE.
           MOVE EMP-NUMBER OF EMPLOYEE-MASTER-FILE TO
               EMP-NUMBER OF EMPLOYEE-STATUS.
           MOVE EMP-LAST-NAME OF EMPLOYEE-MASTER-FILE TO
               EMP-LAST-NAME OF EMPLOYEE-STATUS.
           MOVE EMP-FIRST-NAME OF EMPLOYEE-MASTER-FILE TO
               EMP-FIRST-NAME OF EMPLOYEE-STATUS.
           MOVE EMP-MID-INIT OF EMPLOYEE-MASTER-FILE TO
               EMP-MID-INIT OF EMPLOYEE-STATUS.
           MOVE EMP-NUMBER OF EMPLOYEE-MASTER-FILE TO
               EMP-NUM.

           EXEC SQL
               SELECT  LASTNAME,  YTDSAL,   MAXNATL
               INTO   :LASTNAME, :YTDSAL,  :MAXNATL
               FROM RVIIEMP
               WHERE EMPNO = :EMP-NUM
               FETCH FIRST ROW ONLY
           END-EXEC

           EVALUATE TRUE
              WHEN SQLCODE = 0
                 COMPUTE YTD-WITHHOLD ROUNDED = YTDSAL *
                      EMP-NATL-TAX-WITHOLD-PCT / 100
                 IF YTD-WITHHOLD < MAXNATL
                    MOVE 'ACTIVE ' TO EMP-STATUS
                    MOVE YTD-WITHHOLD TO CURRENT-WITHHOLD
                    MOVE MAXNATL TO MAX-WITHHOLD
                    MOVE ACTIVE-NOTE TO EMP-NOTE
                 ELSE
                    MOVE 'COMPLETE' TO EMP-STATUS
                 END-IF
              WHEN SQLCODE = +100
                 MOVE 'INVALID '  TO EMP-STATUS
                 MOVE '*** NO MATCHING ENTRY ***' TO EMP-NOTE
              WHEN SQLCODE NOT = 0 AND NOT = +100
                 MOVE 'INVALID' TO EMP-STATUS
                 MOVE SQLCODE TO SQLECODE
                 MOVE SQLERROR TO EMP-NOTE
           END-EVALUATE.

*********  WRITE REPORT-RECORD FROM EMPLOYEE-STATUS.
           CALL RVWKEVAL.
           CALL RVWKEVAL.
           CALL RVWKEVAL.
           READ INP-FILE INTO WS-SALES-RECORD
              AT END
                 MOVE 'Y' TO EOF-SW.
           IF NOT-END-OF-FILE
             CALL RVII0040 USING REPORT-RECORD
                              WS-SALES-RECORD
                  PERFORM 0800-INPUT
           END-IF.
*********
*********
       0800-INPUT.
           READ EMPLOYEE-FILE INTO EMPLOYEE-MASTER-FILE
               AT END
                 MOVE 'Y' TO EOF-SW.
*********
*********
       0888-STAT-INPUT.
           READ EMPSTAT-FILE INTO STATUS-DATA
               AT END
                 MOVE 'Y' TO EOF-SW.
           ADD 1 TO LOOP-SO-FAR.
           IF LOOP-SO-FAR = LOOP-COUNTER
               MOVE 'Y' TO EOF-SW.
*********
*********
       0900-OPEN.
           OPEN INPUT  EMPLOYEE-FILE.
           OPEN INPUT INP-FILE.
*********  OPEN OUTPUT REPORT-FILE.
*********
*********
       0900-CLOSE.
           CLOSE EMPLOYEE-FILE.
*********  CLOSE REPORT-FILE.
*********
*********
       0999-OPEN.
           OPEN INPUT  EMPSTAT-FILE.
*********
*********
       0999-CLOSE.
*********  CLOSE EMPSTAT-FILE.
           CLOSE INP-FILE.

       0000-MAINLINE.
           PERFORM 9000-OPEN.
           PERFORM 9100-CHECK-PARM.
           IF GOOD-PARM
              PERFORM 9200-INIT
              PERFORM 8000-READ-INPUT
                   UNTIL RECORDS-READ = START-NUMBER
                   OR END-OF-FILE2
              PERFORM 1000-PROCESS-DATA
                    UNTIL END-OF-FILE2
              PERFORM 6000-PRINT-EMPLOYEE-REPORT
              MOVE 1 TO REGION-SUB
              IF END-OF-MONTH
                  PERFORM 7000-PRINT-REGION-REPORT
                        UNTIL REGION-SUB > 4.
           PERFORM 9900-CLOSE.
           GOBACK.
*********
*********  DIFFERENT PROCESSING OCCURS BASED ON EMPLOYEE TYPE.  THERE
*********  ARE 3 VALID EMPLOYEE TYPES.  IF A RECORD IS READ CONTAINING
*********  A RECORD TYPE OTHER THAN H, S OR M, AN ERROR MESSAGE IS
*********  WRITTEN AND PROCESSING CONTINUES.
*********
       1000-PROCESS-DATA.
           IF HOURLY
               PERFORM 2000-PROCESS-HOURLY
           ELSE
               IF SALES
                   PERFORM 3000-PROCESS-SALES
               ELSE
                   IF MANAGEMENT
                       PERFORM 4000-PROCESS-MANAGEMENT
                   ELSE
                       MOVE ' INVALID EMPLOYEE TYPE ' TO ERROR-LINE
                       WRITE REPORT-RECORD2 FROM ERROR-LINE.
           PERFORM 8000-READ-INPUT.
*********
*********  CALCULATE TYPE H (HOURLY) EMPLOYEE COMPENSATION.  ANY
*********  EMPLOYEE WITH MORE THAN 40 HOURS RECEIVES OVERTIME COMPUTED
*********  AT 1.5 TIMES THEIR HOURLY RATE.  ONCE EMPLOYEE COMPENSATION
*********  IS CALCULATED, IT IS STORED IN A HOLD TABLE.  THE DATA IN
*********  THE HOLD TABLE IS USED FOR PRINTING THE EMPLOYEE COMPENSATION
*********  REPORT.
*********
       2000-PROCESS-HOURLY.
               MOVE ZERO TO OT-AMOUNT.
           IF WA-EMP-HOURS GREATER THAN 40
               COMPUTE EMP-WAGES = WA-EMP-RATE * 40
               COMPUTE OT-HOURS  = WA-EMP-HOURS - 40
               COMPUTE OT-AMOUNT = OT-HOURS * (WA-EMP-RATE * 1.5)
           ELSE
               COMPUTE EMP-WAGES = WA-EMP-HOURS * WA-EMP-RATE.
           COMPUTE EMP-COMPENSATION = EMP-WAGES + OT-AMOUNT.
           ADD EMP-COMPENSATION TO GRAND-TOTAL-EMP.
           CALL 'CWXTDATE' USING END-OF-MONTH-SW
                                 YRS-OF-SERVICE
                                 TODAYS-DATE
                                 WA-EMP-HIRE-DATE.
           PERFORM 5000-STORE-EMPLOYEE-DETAIL.
           SET HOLD-IX UP BY 1.
*********
*********  CALCULATE TYPE S (SALES) EMPLOYEE COMPENSATION.  THE TOTAL
*********  SALES FOR THE EMPLOYEE IS PASSED TO THE CALLED PROGRAM WHICH
*********  CALCULATES COMMISSION.  ONCE EMPLOYEE COMPENSATION IS
*********  RETURNED FROM CWXTSUBC, IT IS STORED IN A HOLD-TABLE.  THE
*********  DATA IN THE HOLD-TABLE IS USED FOR PRINTING THE EMPLOYEE
*********  COMPENSATION REPORT.

*********
       3000-PROCESS-SALES.
           IF WA-SALES-AMOUNT > 0
              MOVE NAT-DATA2 TO NAT-DATA
              MOVE NAT-DATA3 TO NAT-DATA
              MOVE NAT-DATA4 TO NAT-DATA
              MOVE NAT-DATA5 TO NAT-DATA
              MOVE NAT-DATA6 TO NAT-DATA
              MOVE NAT-DATA7 TO NAT-DATA
              MOVE NAT-DATA8 TO NAT-DATA
              MOVE NAT-DATA6 to NAT-DATA
              MOVE '!@#$%¬&*()' TO NAT-DATA
              ADD  WA-SALES-AMOUNT  TO REGION-SALES (WA-EMP-REGION)
              MOVE WA-SALES-AMOUNT  TO CALC-SALES
              MOVE 0 TO CALC-COMMISSION
              MOVE 'S' TO EMP-TYPE
              CALL 'CWXTSUBC' USING EMP-TYPE,
                                    CALC-SALES,
                                    CALC-COMMISSION
           ELSE
              MOVE 'UH-OH' TO REGION-COMMENT (WA-EMP-REGION).
           COMPUTE EMP-COMPENSATION = WA-SALES-SALARY +
                                      CALC-COMMISSION.
           ADD  EMP-COMPENSATION TO GRAND-TOTAL-EMP.
           CALL 'CWXTDATE' USING END-OF-MONTH-SW
                                 YRS-OF-SERVICE
                                 TODAYS-DATE
                                 WA-EMP-HIRE-DATE.
           PERFORM 5000-STORE-EMPLOYEE-DETAIL.
           SET HOLD-IX UP BY 1.
*********
*********  PROCESS TYPE M (MANAGEMENT) RECORDS.  THE MANAGER NAME AND
*********  SALARY ARE STORED IN A TABLE FOR USE DURING PRINTING OF THE
*********  REGIONAL SALES REPORT.
*********
       4000-PROCESS-MANAGEMENT.
           MOVE WA-EMP-NAME    TO REGION-MANAGER (WA-EMP-REGION).
           MOVE WA-MGMT-SALARY TO REGION-SALARY (WA-EMP-REGION).
*********
*********  SALES AND HOURLY EMPLOYEE DATA IS STORED IN A HOLD TABLE FOR
*********  PRINTING OF EMPLOYEE COMPENSATION REPORT.  THE HOLD TABLE IS
*********  A TWO-DIMENSIONAL TABLE AND HOLDS DATA FOR A MAXIMUM OF 20
*********  EMPLOYEES.
*********
       5000-STORE-EMPLOYEE-DETAIL.
           PERFORM 5100-SET-INDEX.
           IF VALID-REGION
               MOVE WA-EMP-NAME TO HOLD-NAME (REG-IX, HOLD-IX)
               MOVE REGION-ID (WA-EMP-REGION)
                                TO HOLD-REGION (REG-IX, HOLD-IX)
               MOVE WA-EMP-TYPE TO HOLD-TYPE (REG-IX, HOLD-IX)
               MOVE WA-EMP-HIRE-DATE TO HOLD-HIRE-DATE (REG-IX, HOLD-IX)
               SUBTRACT 99 FROM YRS-OF-SERVICE
               MOVE YRS-OF-SERVICE TO HOLD-YEARS (REG-IX, HOLD-IX)
               MOVE EMP-COMPENSATION
                                TO HOLD-TOTAL (REG-IX, HOLD-IX)
               IF HOURLY
                  MOVE EMP-WAGES TO HOLD-WAGES (REG-IX, HOLD-IX)
                  MOVE OT-AMOUNT TO HOLD-OT (REG-IX, HOLD-IX)
               ELSE
                  MOVE WA-SALES-SALARY
                                 TO HOLD-WAGES (REG-IX, HOLD-IX)
                  MOVE CALC-COMMISSION
                                 TO HOLD-COMM (REG-IX, HOLD-IX).
*********
*********  SET THE REGION INDEX BASED ON EMPLOYEE REGION ID AND
*********  SEQUENTIALLY INCREMENT HOLD INDEX WITHIN EACH REGION.
*********  THE EMPLOYEE COMPENSATION REPORT WILL BE GROUPED BY
*********  REGION.  IF AN INVALID REGION IS FOUND, AN ERROR MESSAGE
*********  IS WRITTEN AND PROCESSING CONTINUES.
*********
       5100-SET-INDEX.
           MOVE 'N' TO REGION-ERROR-SW.
           IF NORTH
               ADD 1 TO NORTH-COUNT
               SET HOLD-IX TO NORTH-COUNT
           ELSE
               IF SOUTH
                   ADD 1 TO SOUTH-COUNT
                   SET HOLD-IX TO SOUTH-COUNT
               ELSE
                   IF EAST
                       ADD 1 TO EAST-COUNT
                       SET HOLD-IX TO EAST-COUNT
                   ELSE
                       IF WEST
                           ADD 1 TO WEST-COUNT
                           SET HOLD-IX TO WEST-COUNT
                       ELSE
                           MOVE 'Y' TO REGION-ERROR-SW.
           IF VALID-REGION
               SET REG-IX TO WA-EMP-REGION.
*********
*********  COMPENSATION DATA FOR HOURLY AND SALES EMPLOYEES ARE PRINTED
*********  TO THE EMPLOYEE COMPENSATION REPORT FROM THE HOLD TABLE.
*********
       6000-PRINT-EMPLOYEE-REPORT.
           SET REG-IX TO 1.
           PERFORM 6100-PRINT-EMPLOYEE-DETAIL
                 VARYING HOLD-IX FROM 1 BY 1
                 UNTIL HOLD-IX > NORTH-COUNT.
           SET REG-IX TO 2.
           PERFORM 6100-PRINT-EMPLOYEE-DETAIL
                 VARYING HOLD-IX FROM 1 BY 1
                 UNTIL HOLD-IX > SOUTH-COUNT.
           SET REG-IX TO 3.
           PERFORM 6100-PRINT-EMPLOYEE-DETAIL
                 VARYING HOLD-IX FROM 1 BY 1
                 UNTIL HOLD-IX > EAST-COUNT.
           SET REG-IX TO 4.
           PERFORM 6100-PRINT-EMPLOYEE-DETAIL
                 VARYING HOLD-IX FROM 1 BY 1
                 UNTIL HOLD-IX > WEST-COUNT.
           WRITE REPORT-RECORD2 FROM BLANK-LINE.
           MOVE GRAND-TOTAL-EMP TO EMP-GRAND-TOTAL.
           WRITE REPORT-RECORD2 FROM EMP-TOTAL-DTL.
*********
*********  PRINT DETAIL LINES FOR EMPLOYEE COMPENSATION REPORT
*********
       6100-PRINT-EMPLOYEE-DETAIL.
           IF EMP-LINE-COUNT GREATER THAN 55
                 PERFORM 6200-PRINT-EMPLOYEE-HEADERS.
           MOVE HOLD-NAME   (REG-IX, HOLD-IX) TO EMP-DTL-NAME.
           MOVE HOLD-REGION (REG-IX, HOLD-IX) TO EMP-DTL-REGION.
           MOVE HOLD-TYPE   (REG-IX, HOLD-IX) TO EMP-DTL-TYPE.
           MOVE HOLD-HIRE-MM(REG-IX, HOLD-IX) TO EMP-DTL-HIRE-MM.
           MOVE '/'                           TO EMP-DTL-SLASH1.
           MOVE HOLD-HIRE-DD(REG-IX, HOLD-IX) TO EMP-DTL-HIRE-DD.
           MOVE '/'                           TO EMP-DTL-SLASH2.
           MOVE HOLD-HIRE-YY(REG-IX, HOLD-IX) TO EMP-DTL-HIRE-YY.
           MOVE HOLD-YEARS  (REG-IX, HOLD-IX) TO EMP-DTL-YRS-OF-SERVICE.
           MOVE HOLD-WAGES  (REG-IX, HOLD-IX) TO EMP-DTL-WAGES.
           MOVE HOLD-OT     (REG-IX, HOLD-IX) TO EMP-DTL-OT.
           MOVE HOLD-COMM   (REG-IX, HOLD-IX) TO EMP-DTL-COMM.
           MOVE HOLD-TOTAL  (REG-IX, HOLD-IX) TO EMP-DTL-TOTAL.
           WRITE REPORT-RECORD2 FROM EMPLOYEE-DTL
             AFTER ADVANCING 1 LINE.
           ADD  1 TO EMP-LINE-COUNT.
           MOVE SPACES TO EMPLOYEE-DTL.
*********
*********  PRINT HEADERS FOR EMPLOYEE COMPENSATION REPORT
*********
       6200-PRINT-EMPLOYEE-HEADERS.
               MOVE PAGE-COUNT TO EMP-PAGE.
               MOVE DATE-YY TO EMP-RUN-YY.
               MOVE 99 TO EMP-RUN-YY.
               MOVE DATE-MM TO EMP-RUN-MM.
               MOVE DATE-DD TO EMP-RUN-DD.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               WRITE REPORT-RECORD2 FROM EMPLOYEE-HDR1.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               WRITE REPORT-RECORD2 FROM EMPLOYEE-HDR2.
               WRITE REPORT-RECORD2 FROM EMPLOYEE-HDR3.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               ADD 1 TO PAGE-COUNT.
               MOVE 9 TO EMP-LINE-COUNT.
*********
*********  COMPENSATION DATA FOR MANAGEMENT EMPLOYEES IS PRINTED
*********  TO THE REGIONAL SALES REPORT.  THE TOTAL SALES FOR THE
*********  REGION IS PASSED TO THE CALLED PROGRAM WHICH CALCULATES
*********  COMMISSION.  THIS REPORT IS GENERATED AT END-OF-MONTH.
*********
       7000-PRINT-REGION-REPORT.
           IF REG-LINE-COUNT GREATER THAN 55
               PERFORM 7100-PRINT-REGION-HEADERS.
           MOVE SPACES TO REGION-DETAIL.
           MOVE REGION-NAME    (REGION-SUB) TO REG-DTL-REGION.
           MOVE REGION-MANAGER (REGION-SUB) TO REG-DTL-MANAGER.
           MOVE REGION-SALARY  (REGION-SUB) TO REG-DTL-SALARY.
           MOVE REGION-SALES   (REGION-SUB) TO REG-DTL-SALES.
           MOVE REGION-SALES   (REGION-SUB) TO CALC-SALES.
           MOVE REGION-COMMENT (REGION-SUB) TO REG-DTL-COMMENT.
           MOVE 'M' TO EMP-TYPE.
           CALL 'CWXTSUBC' USING EMP-TYPE,
                                 CALC-SALES,
                                 CALC-COMMISSION.
           MOVE CALC-COMMISSION TO REG-DTL-COMM.
           COMPUTE MGMT-COMPENSATION = CALC-COMMISSION +
                                      REGION-SALARY(REGION-SUB).
           ADD  MGMT-COMPENSATION TO GRAND-TOTAL-MGMT.
           MOVE MGMT-COMPENSATION TO REG-DTL-TOTAL.
           WRITE REPORT-RECORD2 FROM REGION-DETAIL.
           IF REGION-SUB = 4
              WRITE REPORT-RECORD2 FROM BLANK-LINE
              MOVE GRAND-TOTAL-MGMT TO MGMT-GRAND-TOTAL
              WRITE REPORT-RECORD2 FROM MGMT-TOTAL-DTL.
           ADD 1 TO REG-LINE-COUNT.
           ADD 1 TO REGION-SUB.
*********
*********  PRINT HEADERS FOR REGIONAL SALES REPORT
*********
       7100-PRINT-REGION-HEADERS.
               MOVE PAGE-COUNT TO REG-PAGE.
               MOVE DATE-YY TO REG-RUN-YEAR.
               MOVE DATE-MM TO REG-RUN-MONTH.
               MOVE DATE-DD TO REG-RUN-DAY.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               WRITE REPORT-RECORD2 FROM REGION-HDR1.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               WRITE REPORT-RECORD2 FROM REGION-HDR2.
               WRITE REPORT-RECORD2 FROM BLANK-LINE.
               ADD 1 TO PAGE-COUNT.
               MOVE 10 TO REG-LINE-COUNT.
*********
*********
       8000-READ-INPUT.
           CALL RVII0080.
           READ EMPLOYEE-FILE2 INTO EMPLOYEE-WORK-AREA
               AT END
                    MOVE 'Y' TO EOF-SW2.
           ADD 1 TO RECORDS-READ.
*********
*********
       9000-OPEN.
           OPEN INPUT  EMPLOYEE-FILE2.
           OPEN OUTPUT REPORT-FILE2.
*********
*********  VALID PARMS ARE BLANK OR 5 DIGIT NUMBER
*********
       9100-CHECK-PARM.
           MOVE 4 TO START-NUMBER.
*********
*********
       9200-INIT.
           MOVE 1 TO REGION-SUB.
           PERFORM 9300-INITIALIZE-REGION-TABLE
              UNTIL REGION-SUB > 4.
           SET HOLD-IX TO 1.
           PERFORM 9500-REITERATIVE 11 TIMES.
           ACCEPT TODAYS-DATE FROM DATE.
*********
*********
       9300-INITIALIZE-REGION-TABLE.
           MOVE SPACES TO REGION-MANAGER (REGION-SUB).
           MOVE SPACES TO REGION-COMMENT (REGION-SUB).
           MOVE 0 TO REGION-SALARY (REGION-SUB).
           MOVE 0 TO REGION-SALES  (REGION-SUB).
           ADD  1 TO REGION-SUB.
*********
       9500-REITERATIVE.
           MOVE 0 to CALC-SALES.
           MOVE 'S' TO EMP-TYPE.
           CALL 'CWXTSUBC' USING EMP-TYPE,
                                 CALC-SALES,
                                 CALC-COMMISSION.
*********
       9800-BAD-PARM.
           MOVE 'Y' TO PARM-ERROR-SW.
           MOVE '   PARAMETER LENGTH OR DATA IS INCORRECT   '
               TO ERROR-LINE.
           WRITE REPORT-RECORD2 FROM ERROR-LINE.
*********
*********
       9900-CLOSE.
           CLOSE EMPLOYEE-FILE2.
           CLOSE REPORT-FILE2.
*********
*********
       9999-RIP.
           DISPLAY '    ************    '.
           DISPLAY '   *            *   '.
           DISPLAY '  *   T H I S    *  '.
           DISPLAY ' *                * '.
           DISPLAY ' *      I S       * '.
           DISPLAY ' *                * '.
           DISPLAY ' *    D E A D     * '.
           DISPLAY ' *                * '.
           DISPLAY ' *    C O D E     * '.
           DISPLAY ' ****************** '.