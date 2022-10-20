       TITLE 'GENERAL LEDGER SUSPENSE FILE EDIT'

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FNB206.
       AUTHOR.        DAN DRYDEN.
       DATE-WRITTEN.  FEBRUARY, 1999.

      *****************************************************************
      *                         H I S T O R Y                         *
      *****************************************************************
      * NAME    DATE    DESCRIPTION                                   *
      * ---- ---------- ----------------------------------------------*
      * DANA 02/23/1999 TO PRODUCTION                                 *
      * DANA 06/19/2000 IR2000061700004                               *
      * DJNA 02/24/2003 CONVERT FROM MAINFRAME TO MICROFOCUS COBOL.   *
082506* AJR  08/25/2006 REMOVE FROM CLAIMS SYSTEM.                    *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT UPLOAD
               ASSIGN TO EXTERNAL SYS010
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS SYS010-STATUS.

           SELECT OLD-BACKUP
               ASSIGN TO EXTERNAL SYS011
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS SYS011-STATUS.

           SELECT NEW-BACKUP
               ASSIGN TO EXTERNAL SYS012
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  UPLOAD
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 120 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  UPLOAD-RECORD     PIC X(120).

       FD  OLD-BACKUP
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 120 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  OLDBU-RECORD        PIC X(120).

       FD  NEW-BACKUP
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 120 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  NEWBU-RECORD        PIC X(120).

       WORKING-STORAGE SECTION.

       01 RC                       PIC S9(9)   COMP-5 VALUE 0.
       01 LIST-REC                 PIC X(132).

       01  FILLER.
           05  SYS010-STATUS       PIC XX     VALUE '00'.
               88  EOF-SYS010                 VALUE '10'.
           05  SYS011-STATUS       PIC XX     VALUE '00'.
               88  EOF-SYS011                 VALUE '10'.
           05  FILLER              PIC X      VALUE '0'.
               88  FILES-ARE-EQUAL            VALUE '1'.
               88  FILES-ARE-NOT-EQUAL        VALUE '2'.
               88  END-OF-MATCH               VALUE '1', '2'.

       PROCEDURE DIVISION.

082506     CALL 'FNBLIST' USING 'O' LIST-REC
           OPEN INPUT UPLOAD
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

           READ UPLOAD
           IF SYS010-STATUS = '10'
             DISPLAY 'ERROR...UPLOAD FILE IS EMPTY ' UPON SYSERR
             MOVE    'ERROR...UPLOAD FILE IS EMPTY ' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
082506       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 04 TO RC
082506*      GOBACK GIVING RC
082506       STOP RUN GIVING RC
           END-IF

           OPEN INPUT OLD-BACKUP
           IF SYS011-STATUS NOT = '00'
             DISPLAY '*** OPEN ERROR ' SYS011-STATUS ' ON SYS011'
                     UPON SYSERR
             MOVE SPACES TO LIST-REC
             STRING  '*** OPEN ERROR ' SYS011-STATUS ' ON SYS011'
               DELIMITED BY SIZE INTO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
082506       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 16 TO RC
082506*      GOBACK GIVING RC
082506       STOP RUN GIVING RC
           END-IF

           READ OLD-BACKUP
           IF SYS010-STATUS NOT = '00'
061700       SET FILES-ARE-NOT-EQUAL TO TRUE
           END-IF

     *** MATCH THE CURRENT UPLOAD FILE TO THE PREVIOUS FILE (BACKUP)
     *** IF THE FILES ARE EQUAL THEN ABEND THE JOB
     *** ELSE COPY THE CURRENT UPLOAD TO THE BACKUP FILE

           PERFORM UNTIL END-OF-MATCH
             IF EOF-SYS010 AND EOF-SYS011
               SET FILES-ARE-EQUAL TO TRUE
             ELSE
               IF UPLOAD-RECORD NOT = OLDBU-RECORD
                 SET FILES-ARE-NOT-EQUAL TO TRUE
               ELSE
                 READ UPLOAD
                 READ OLD-BACKUP
               END-IF
             END-IF
           END-PERFORM

           CLOSE UPLOAD

           IF FILES-ARE-NOT-EQUAL
             OPEN INPUT  UPLOAD
             OPEN OUTPUT NEW-BACKUP
             PERFORM UNTIL EOF-SYS010
               READ UPLOAD
                 AT END EXIT PERFORM
               END-READ
               WRITE NEWBU-RECORD FROM UPLOAD-RECORD
             END-PERFORM
             CLOSE UPLOAD
             CLOSE NEW-BACKUP
           ELSE
             DISPLAY 'ERROR   *** DUPLICATE UPLOAD FILE ***' UPON SYSERR
             DISPLAY ' '                                     UPON SYSERR
             DISPLAY '*************************************' UPON SYSERR
             DISPLAY '* THE UPLOAD FILE FOR TODAY IS THE  *' UPON SYSERR
             DISPLAY '* SAME AS THE PREVIOUS DAY''S FILE. *' UPON SYSERR
             DISPLAY '*************************************' UPON SYSERR
             MOVE    'ERROR   *** DUPLICATE UPLOAD FILE ***' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             MOVE    ' '                                     TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             MOVE    '*************************************' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             MOVE    '* THE UPLOAD FILE FOR TODAY IS THE  *' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             MOVE    '* SAME AS THE PREVIOUS DAY''S FILE. *' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             MOVE    '*************************************' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
082506       CALL 'FNBLIST' USING 'C' LIST-REC
             MOVE 08 TO RC
082506*      GOBACK GIVING RC
082506       STOP RUN GIVING RC
           END-IF.

082506     CALL 'FNBLIST' USING 'C' LIST-REC
082506*    GOBACK GIVING RC.
082506     STOP RUN GIVING RC.
