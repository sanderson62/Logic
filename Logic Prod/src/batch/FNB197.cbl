       TITLE 'GENERAL LEDGER SUSPENSE FILE UPDATE'

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FNB197.
       AUTHOR.        DAN DRYDEN.
       DATE-WRITTEN.  JANUARY, 1999.

      *****************************************************************
      *                         H I S T O R Y                         *
      *****************************************************************
      * NAME    DATE    DESCRIPTION                                   *
      * ---- ---------- --------------------------------------------- *
      * DANA 01/__/1999 TO PRODUCTION                                 *
      * DJNA 02/24/2003 CONVERT FROM MAINFRAME TO MICROFOCUS COBOL.   *
082506* AJRA 08/25/2006 REMOVE FROM THE CLAIMS SYSTEM                 *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GLSUSP-IN
               ASSIGN TO EXTERNAL SYS010
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS SYS010-STATUS.

           SELECT GLSUSP-OUT
               ORGANIZATION IS LINE SEQUENTIAL
               ASSIGN TO EXTERNAL SYS011.

           SELECT GLSUSP-DELETED
               ORGANIZATION IS LINE SEQUENTIAL
               ASSIGN TO EXTERNAL SYS012.

       DATA DIVISION.
       FILE SECTION.

       FD  GLSUSP-IN
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 120 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  GLSUSP-INPUT-RECORD.
           05  GLI-KEY                   PIC X(39).
           05  FILLER                    PIC X(16).
           05  GLI-TRANS-AMOUNT          PIC S9(12)V99
                                         SIGN TRAILING SEPARATE.
           05  FILLER                    PIC X(50).

       FD  GLSUSP-OUT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 120 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  GLSUSP-OUTPUT-RECORD.
           05  FILLER                    PIC X(55).
           05  GLO-TRANS-AMOUNT          PIC S9(12)V99
                                         SIGN TRAILING SEPARATE.
           05  FILLER                    PIC X(50).

       FD  GLSUSP-DELETED
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           RECORD CONTAINS 120 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  GLSUSP-DELETED-RECORD.
           05  FILLER                    PIC X(55).
           05  GLD-TRANS-AMOUNT          PIC S9(12)V99
                                         SIGN TRAILING SEPARATE.
           05  FILLER                    PIC X(50).

       WORKING-STORAGE SECTION.

       01 RC                       PIC S9(9)   COMP-5 VALUE 0.
       01 LIST-REC                 PIC X(132).

       01  FILLER                  COMP-3.
           05  WS-TRANS-AMT        PIC S9(11)V99  VALUE +0.
           05  INPUT-CNT           PIC S9(9)      VALUE +0.
               88  FIRST-RECORD                   VALUE +1.
           05  INPUT-DR-AMT        PIC S9(11)V99  VALUE +0.
           05  INPUT-CR-AMT        PIC S9(11)V99  VALUE +0.
           05  DELETED-CNT         PIC S9(9)      VALUE +0.
           05  DELETED-DR-AMT      PIC S9(11)V99  VALUE +0.
           05  DELETED-CR-AMT      PIC S9(11)V99  VALUE +0.
           05  OUTPUT-CNT          PIC S9(9)      VALUE +0.
           05  OUTPUT-DR-AMT       PIC S9(11)V99  VALUE +0.
           05  OUTPUT-CR-AMT       PIC S9(11)V99  VALUE +0.

       01  FILLER.
           05  SYS010-STATUS       PIC XX     VALUE '00'.
               88  EOF                        VALUE '10'.
           05  PREV-KEY            PIC X(39)  VALUE LOW-VALUE.
           05  SYS-DATE            PIC 9(8).
           05  WS-DATE             PIC XX/XX/XXXX.

082506 01  WORK-DATE.
082506     05  WRK-YR    PIC 9999.
082506     05  WRK-MO    PIC 99.
082506     05  WRK-DAY   PIC 99.
082506
082506 01  SYSTEM-DATE.
082506     05  SYS-MO    PIC 99.
082506     05  SYS-DAY   PIC 99.
082506     05  SYS-YR    PIC 9999.


       01  EOJ-LINE.
           05  EOJ-MSG             PIC X(16).
           05  EOJ-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  EOJ-DR-AMT          PIC ZZZ,ZZZ,ZZZ,ZZZ.99.
           05  FILLER              PIC XXX    VALUE ' DR'.
           05  EOJ-CR-AMT          PIC ZZZ,ZZZ,ZZZ,ZZZ.99.
           05  FILLER              PIC XXX    VALUE ' CR'.

       01  TRANS-RECORD-HOLD-AREA.
           05  END-OF-TABLE INDEX.
           05  TABLE-MAX  VALUE +15000 PIC S9(8) BINARY.
           05  TRANS-HOLD OCCURS 15000 TIMES INDEXED BY TRANS-INDX.
               10  FILLER            PIC X(55).
               10  TRANS-AMOUNT      PIC S9(12)V99
                                     SIGN TRAILING SEPARATE.
               10  FILLER            PIC X(50).

       01  ED-MAX                    PIC ZZZZZZ,ZZ9.

       PROCEDURE DIVISION.

082506     CALL 'FNBLIST' USING 'O' LIST-REC
           OPEN INPUT GLSUSP-IN
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

           OPEN OUTPUT GLSUSP-DELETED
           OPEN OUTPUT GLSUSP-OUT
           SET TRANS-INDX TO +1

           PERFORM 1000-UPDATE UNTIL EOF

           PERFORM 1100-PROCESS-ENTRIES

           CLOSE GLSUSP-IN
                 GLSUSP-DELETED
                 GLSUSP-OUT

082506     ACCEPT WORK-DATE FROM DATE YYYYMMDD
082506     MOVE WRK-YR  TO SYS-YR
082506     MOVE WRK-MO  TO SYS-MO
082506     MOVE WRK-DAY TO SYS-DAY
082506
082506     MOVE SYSTEM-DATE TO SYS-DATE
082506*    CALL 'SYSDATE' USING SYS-DATE
           MOVE SYS-DATE TO WS-DATE
           DISPLAY 'SUSPENSE REPORTING FILE UPDATE ' WS-DATE UPON SYSERR
           DISPLAY ' '                                       UPON SYSERR
           MOVE SPACES TO LIST-REC
           STRING  'SUSPENSE REPORTING FILE UPDATE ' WS-DATE
             DELIMITED BY SIZE INTO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
           MOVE SPACES TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC

           MOVE 'INPUT RECORDS: ' TO EOJ-MSG
           MOVE INPUT-CNT         TO EOJ-CNT
           MOVE INPUT-DR-AMT      TO EOJ-DR-AMT
           MOVE INPUT-CR-AMT      TO EOJ-CR-AMT
           DISPLAY EOJ-LINE UPON SYSERR
           MOVE EOJ-LINE TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC

           MOVE 'DELETED RECORDS:' TO EOJ-MSG
           MOVE DELETED-CNT       TO EOJ-CNT
           MOVE DELETED-DR-AMT    TO EOJ-DR-AMT
           MOVE DELETED-CR-AMT    TO EOJ-CR-AMT
           DISPLAY EOJ-LINE UPON SYSERR
           MOVE EOJ-LINE TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC

           MOVE 'OUTPUT RECORDS:' TO EOJ-MSG
           MOVE OUTPUT-CNT        TO EOJ-CNT
           MOVE OUTPUT-DR-AMT     TO EOJ-DR-AMT
           MOVE OUTPUT-CR-AMT     TO EOJ-CR-AMT
           DISPLAY EOJ-LINE UPON SYSERR
           MOVE EOJ-LINE TO LIST-REC
           CALL 'FNBLIST' USING 'W' LIST-REC
082506     CALL 'FNBLIST' USING 'C' LIST-REC

082506*    GOBACK GIVING RC.
082506     STOP RUN GIVING RC.

       1000-UPDATE.

           READ GLSUSP-IN
             AT END EXIT PARAGRAPH
           END-READ

           ADD +1 TO INPUT-CNT
           IF GLI-TRANS-AMOUNT IS NEGATIVE
             ADD GLI-TRANS-AMOUNT TO INPUT-CR-AMT
           ELSE
             ADD GLI-TRANS-AMOUNT TO INPUT-DR-AMT
           END-IF

           IF FIRST-RECORD
             MOVE GLI-KEY TO PREV-KEY
           END-IF

           IF GLI-KEY = PREV-KEY
             CONTINUE
           ELSE
             IF GLI-KEY > PREV-KEY
               PERFORM 1100-PROCESS-ENTRIES
             ELSE
               DISPLAY 'INPUT FILE IS OUT OF SEQUENCE' UPON SYSERR
               DISPLAY '>> JOB CANCELLED <<'           UPON SYSERR
               MOVE    'INPUT FILE IS OUT OF SEQUENCE' TO LIST-REC
               CALL 'FNBLIST' USING 'W' LIST-REC
               MOVE    '>> JOB CANCELLED <<'           TO LIST-REC
               CALL 'FNBLIST' USING 'W' LIST-REC
               MOVE 16 TO RC
082506*        GOBACK GIVING RC
082506         STOP RUN GIVING RC
             END-IF
           END-IF

           MOVE GLSUSP-INPUT-RECORD TO TRANS-HOLD (TRANS-INDX)
           SET TRANS-INDX UP BY +1
           IF TRANS-INDX > TABLE-MAX
             MOVE TABLE-MAX TO ED-MAX
             DISPLAY 'TABLE OVERFLOW IN ROUTINE 1000-UPDATE' UPON SYSERR
             DISPLAY 'INPUT RECORD KEY ' GLI-KEY             UPON SYSERR
             DISPLAY 'HAS MORE THAN ' ED-MAX ' RECORDS'      UPON SYSERR
             MOVE    'TABLE OVERFLOW IN ROUTINE 1000-UPDATE' TO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             MOVE SPACES TO LIST-REC
             STRING  'INPUT RECORD KEY ' GLI-KEY
               DELIMITED BY SIZE INTO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             MOVE SPACES TO LIST-REC
             STRING  'HAS MORE THAN ' ED-MAX ' RECORDS'
               DELIMITED BY SIZE INTO LIST-REC
             CALL 'FNBLIST' USING 'W' LIST-REC
             MOVE 16 TO RC
082506*      GOBACK GIVING RC
082506       STOP RUN GIVING RC
           END-IF

           MOVE GLI-KEY TO PREV-KEY.

       1100-PROCESS-ENTRIES.

           SET END-OF-TABLE TO TRANS-INDX
           MOVE +0 TO WS-TRANS-AMT

           PERFORM VARYING TRANS-INDX FROM 1 BY 1
             UNTIL TRANS-INDX = END-OF-TABLE
             ADD TRANS-AMOUNT (TRANS-INDX) TO WS-TRANS-AMT
           END-PERFORM

      *** IF THE NET TRANSACTION AMOUNT = ZERO, IT MEANS THE
      *** THE TRANSACTION HAS "CLEARED".  THE CLEARED TRANSACTIONS
      *** ARE WRITTEN TO THE DELETED FILE WHICH WILL BE MERGED INTO
      *** AN INCEPTION-TO-DATE FILE OF DELETED TRANSACTIONS.

           IF WS-TRANS-AMT = ZERO
             PERFORM VARYING TRANS-INDX FROM 1 BY 1
               UNTIL TRANS-INDX = END-OF-TABLE
               MOVE TRANS-HOLD (TRANS-INDX) TO GLSUSP-DELETED-RECORD
               ADD +1 TO DELETED-CNT
               IF GLD-TRANS-AMOUNT IS NEGATIVE
                 ADD GLD-TRANS-AMOUNT TO DELETED-CR-AMT
               ELSE
                 ADD GLD-TRANS-AMOUNT TO DELETED-DR-AMT
               END-IF
               WRITE GLSUSP-DELETED-RECORD
             END-PERFORM
           ELSE
             PERFORM VARYING TRANS-INDX FROM 1 BY 1
               UNTIL TRANS-INDX = END-OF-TABLE
               MOVE TRANS-HOLD (TRANS-INDX) TO GLSUSP-OUTPUT-RECORD
               ADD +1 TO OUTPUT-CNT
               IF GLO-TRANS-AMOUNT IS NEGATIVE
                 ADD GLO-TRANS-AMOUNT TO OUTPUT-CR-AMT
               ELSE
                 ADD GLO-TRANS-AMOUNT TO OUTPUT-DR-AMT
               END-IF
               WRITE GLSUSP-OUTPUT-RECORD
             END-PERFORM
           END-IF

           SET TRANS-INDX TO +1.
