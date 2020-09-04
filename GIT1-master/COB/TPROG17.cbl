       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TPROG17.
       AUTHOR.        BENCHMARK ISPW TRAINING.
       DATE-WRITTEN.  JANUARY 24TH, 1996.
       DATE-COMPILED.


      ********************************************************
      * THIS PROGRAM IS A TRAINING PROGRAM checked out in DEV2
      *      WITH A LINK TO TSUBR17
      *
      *      AND COPYLIBS   TCPYA17   (HEADER)
      *                     TCPYB17   (LINK AREA)
      * ======================================================
      *  ISPW (TM)          Change for Demo
      *  ISPW (TM)          Change for Demo
      *  (C) COPYRIGHT 1996, 2004  BENCHMARK TECHNOLOGIES LTD.
      *  THIS PRODUCT CONTAINS PROPRIETARY MATERIAL, AND MAY
      *  PERMISSION OF BENCHMARK TECHNOLOGIES LTD.
      * ======================================================
      * ======================================================
      *                   MODIFICATION  LOG
      *
      *  DD/MM/YY  PROGRAMMER  CHANGES
      *  ********  **********  *******
      *  96/01/24  ISPW Peter  NEW PROGRAM
      * ======================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *****************************************************
      *  FILES USED:
      *   INPUT   JUST SOME DUMMY DATA TO READ IN
      *   OUTPUT  A COPY OF THE DUMMY DATA WRITTEN OUT
      *   Adding a line for testing
      *****************************************************
      ****
      ****
           SELECT INFILE  ASSIGN UT-S-INPUT
             ORGANIZATION IS SEQUENTIAL
             ACCESS IS SEQUENTIAL.

           SELECT OUTFILE ASSIGN UT-S-OUTPUT
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

       FD  OUTFILE
           LABEL RECORDS OMITTED
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           DATA RECORD IS OUTPUT-REC.

       01  OUTPUT-REC.
           05  CC                PIC X(1).
           05  OUTPUT-LINE       PIC X(132).


       WORKING-STORAGE SECTION.
      ****************************************************
      ****    THE DATA AREA COPYBOOK Modified by Peter
      ****************************************************
       COPY TCPYA17.



      ****************************************************
      *
      * Comment
      *
      * Comment
      *
      ***************************************************
       COPY TCPYB17 .

       PROCEDURE DIVISION.

       00000-MAIN-PROCEDURE.

           OPEN OUTPUT OUTFILE.
           MOVE 'R' TO ACTION-FLAG.
           CALL 'TSUBR17' USING PASS-ME-AROUND.

           IF NOT INFILE-EOF THEN
              MOVE TEXT-PORTION TO MESSANGER

           MOVE SPACES TO CC.
           MOVE STAR-LINE-1 TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.

           MOVE SPACES TO BODY-TEXT.
           MOVE BODY-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.

           MOVE FLASH-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.

           MOVE SPACES TO BODY-TEXT.
           MOVE BODY-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.

           MOVE MESSAGE-LINE TO BODY-TEXT.

           WRITE OUTPUT-REC.

           MOVE SPACES TO BODY-TEXT.
           MOVE BODY-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.

           PERFORM GET-MESSAGE THRU GET-MESSAGE-X
              UNTIL INFILE-EOF.

           PERFORM GET-MESSAGE THRU GET-MESSAGE-X
              UNTIL INFILE-EOF.

           PERFORM GET-MESSAGE THRU GET-MESSAGE-X
              UNTIL INFILE-EOF.

           MOVE SPACES TO BODY-TEXT.
           MOVE BODY-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.

           MOVE STAR-LINE-1 TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.

      * CLOSE THE INPUT FILE
           MOVE 'C' TO ACTION-FLAG.
           CALL 'TSUBR17' USING PASS-ME-AROUND.

           CLOSE OUTFILE.
           GOBACK.

       GET-MESSAGE.
              MOVE 'R' TO ACTION-FLAG.
              CALL 'TSUBR17' USING PASS-ME-AROUND.
                 IF NOT INFILE-EOF THEN
                   MOVE TEXT-PORTION TO BODY-TEXT
                   MOVE BODY-LINE TO OUTPUT-REC
                   WRITE OUTPUT-REC.
       GET-MESSAGE-X.
           EXIT.