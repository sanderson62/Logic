       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FNB251.
       AUTHOR.        DOUG NELSON.
       DATE-WRITTEN.  FEBRUARY 24, 2003.

      *****************************************************************
      *                         H I S T O R Y                         *
      *****************************************************************
      * NAME    DATE    DESCRIPTION                                   *
      * ---- ---------- ----------------------------------------------*
      * DJNA 02/24/2003 REFORMAT GLS FILE                             *
082506* AJR  08/25/2006 REMOVE FROM CLAIMS SYSTEM                     *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INFILE
               ASSIGN TO EXTERNAL SYS010
               FILE STATUS IS SYS010-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT OUTFILE
               ASSIGN TO EXTERNAL SYS011
               FILE STATUS IS SYS011-STATUS
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  INFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 135 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  IN-RECORD         PIC X(135).

       FD  OUTFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 120 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  OUT-RECORD        PIC X(120).

       WORKING-STORAGE SECTION.

       01 RC                       PIC S9(9)   COMP-5 VALUE 0.
       01 LIST-REC                 PIC X(132).

       01  FILLER.
           05  SYS010-STATUS       PIC XX     VALUE '00'.
               88  EOF-SYS010                 VALUE '10'.
           05  SYS011-STATUS       PIC XX     VALUE '00'.
               88  EOF-SYS011                 VALUE '10'.

       01  FILLER.
           05  OUTPUT-COUNT       PIC S9(8)      VALUE ZERO.
           05  INPUT-COUNT        PIC S9(8)      VALUE ZERO.

       PROCEDURE DIVISION.

082506     CALL 'FNBLIST' USING 'O' LIST-REC
           OPEN INPUT INFILE
           IF SYS010-STATUS NOT = '00'
             DISPLAY '*** OPEN ERROR ' SYS010-STATUS ' ON SYS010'
                     UPON SYSERR
             MOVE SPACES TO LIST-REC
             STRING  '*** OPEN ERROR ' SYS010-STATUS ' ON SYS010'
               DELIMITED BY SIZE INTO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
082506       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 16 TO RC
082506*      GOBACK GIVING RC
082506       STOP RUN GIVING RC
           END-IF

           OPEN OUTPUT OUTFILE

           PERFORM UNTIL EOF-SYS010
             READ INFILE
               AT END EXIT PERFORM
             END-READ
             ADD +1 TO INPUT-COUNT
             MOVE IN-RECORD(01:55) TO OUT-RECORD(01:55)
             MOVE IN-RECORD(71:65) TO OUT-RECORD(56:65)
             WRITE OUT-RECORD
             ADD +1 TO OUTPUT-COUNT
           END-PERFORM

           CLOSE INFILE OUTFILE

           DISPLAY '    INPUT RECORDS READ: ' INPUT-COUNT  UPON SYSERR
           DISPLAY 'OUTPUT RECORDS WRITTEN: ' OUTPUT-COUNT UPON SYSERR

           MOVE SPACES TO LIST-REC
           STRING  '    INPUT RECORDS READ: ' INPUT-COUNT
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE SPACES TO LIST-REC
           STRING  'OUTPUT RECORDS WRITTEN: ' OUTPUT-COUNT
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
082506     CALL 'FNBLIST' USING 'C' LIST-REC

082506*    GOBACK GIVING RC.
082506     STOP RUN GIVING RC.
