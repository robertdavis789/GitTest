       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TSUBR16.
       AUTHOR.        BENCHMARK ISPW TRAINING.
       DATE-WRITTEN.  JANUARY 24TH, 1996.
       DATE-COMPILED.

      ********************************************************
      * THIS PROGRAM IS A TRAINING PROGRAM
      * THIS PROGRAM IS A TRAINING PROGRAM
      *      CALLED FROM    TPROG16
      *      WITH COPYLIB   TCPYB16 (LINKAGE AREA)
      * ======================================================
      *  ISPW (TM)
      *  (C) COPYRIGHT 1996, 1998  BENCHMARK TECHNOLOGIES LTD.
      *  THIS PRODUCT CONTAINS PROPRIETARY MATERIAL, AND MAY
      *  NOT BE DIVULGED OR COPIED WITHOUT THE EXPLICIT
      *  PERMISSION OF BENCHMARK TECHNOLOGIES LTD.
      * ======================================================
      *                   MODIFICATION  LOG
      *
      *  DD/MM/YY  PROGRAMMER  CHANGES
      *  ********  **********  *******
      *  96/01/24  ISPW        NEW PROGRAM
      * ======================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      ********************************************************
      *  FILES USED:
      *   INPUT   JUST SOME DUMMY DATA TO READ IN
      ********************************************************

           SELECT INFILE  ASSIGN UT-S-INPUT
             ORGANIZATION IS SEQUENTIAL
             ACCESS IS SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.

       FD  INFILE
           LABEL RECORDS OMITTED
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           DATA RECORD IS INPUT-REC.

       01  INPUT-REC         PIC X(80).

       WORKING-STORAGE SECTION.

       01   INFILE-OPEN-FLAG     PIC X(1) VALUE 'N'.
            88 INFILE-OPEN       VALUE 'Y'.

       LINKAGE SECTION.
      ********************************************************           pfhsxk0
      **** A COPY MEMBER FOR THE LINKAGE AREA                            pfhsxk0
      ********************************************************           pfhsxk0
       COPY TCPYB16.

       PROCEDURE DIVISION USING PASS-ME-AROUND.

       00000-MAIN-PROCEDURE.

           IF NOT INFILE-OPEN THEN
              MOVE 'Y' TO INFILE-OPEN-FLAG
pfhsxk        MOVE 'N' TO INFILE-EOF-FLAG
pfhsxk        OPEN INPUT INFILE.

           MOVE SPACES TO INCOMING-DATA.

pfhsxk     IF ACTION-READ THEN
pfhsxk        PERFORM GET-ME-A-RECORD THRU GET-ME-A-RECORD-X
pfhsxk        UNTIL INFILE-EOF OR KEEPER.

           IF ACTION-CLOSE THEN
              CLOSE INFILE.

           GOBACK.

       GET-ME-A-RECORD.
           READ INFILE AT END MOVE 'Y' TO INFILE-EOF-FLAG.               pfhsxk0
           IF NOT INFILE-EOF MOVE INPUT-REC TO INCOMING-DATA.            pfhsxk0
       GET-ME-A-RECORD-X.                                                pfhsxk0
           EXIT.