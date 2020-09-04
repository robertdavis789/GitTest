       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TPROG16.
       AUTHOR.        BENCHMARK ISPW TRAINING.
       DATE-WRITTEN.  JANUARY 24TH, 1996.
       DATE-COMPILED.

      ********************************************************
      * THIS PROGRAM IS A TRAINING PROGRAM
      *      WITH A LINK TO TSUBR16.
      *
      *      AND COPYLIBS   TCPYA16   (HEADER)
      *                     TCPYB16   (LINK AREA)
      * ======================================================
      *  ISPW (TM)
      *  (C) COPYRIGHT 1996, 2004  BENCHMARK TECHNOLOGIES LTD.
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
      *
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
      *****************************************************
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
      ****    THE DATA AREA COPYBOOK
      ****************************************************
       COPY TCPYA16.

      ****************************************************
      **** A COPY MEMBER FOR THE LINKAGE AREA PARAMETERS
      ****************************************************
       COPY TCPYB16.

       PROCEDURE DIVISION.
      *****************************************************
      *  FILES USED:
      *   INPUT   JUST SOME DUMMY DATA TO READ IN
      *   OUTPUT  A COPY OF THE DUMMY DATA WRITTEN O
      *****************************************************

       00000-MAIN-PROCEDURE.

           OPEN OUTPUT OUTFILE.
           MOVE 'R' TO ACTION-FLAG.
           CALL 'TSUBR16' USING PASS-ME-AROUND.

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
           MOVE BODY-LINE TO OUTPUT-REC.           

           MOVE FLASH-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.

           MOVE SPACES TO BODY-TEXT.
           MOVE BODY-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.

           MOVE MESSAGE-LINE TO BODY-TEXT.
           MOVE BODY-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           MOVE BODY-LINE TO OUTPUT-REC.


           MOVE SPACES TO BODY-TEXT.
           MOVE BODY-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.

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
           CALL 'TSUBR16' USING PASS-ME-AROUND.

           CLOSE OUTFILE.
           GOBACK.

       GET-MESSAGE.
              MOVE 'R' TO ACTION-FLAG.

              CALL 'TSUBR16' USING PASS-ME-AROUND.
                 IF NOT INFILE-EOF THEN
                   MOVE TEXT-PORTION TO BODY-TEXT
                   MOVE BODY-LINE TO OUTPUT-REC
                   WRITE OUTPUT-REC.
       GET-MESSAGE-X.
           EXIT.