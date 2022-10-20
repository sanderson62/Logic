       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDUSEL3.
       AUTHOR.     AJRA.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ACCT-IN          ASSIGN TO SYS010
                                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERACCT           ASSIGN TO ERACCT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

      /

       FD  ACCT-IN
           BLOCK CONTAINS 0
           RECORDING MODE F.
           
       01  ACCT-RECORD-IN              PIC X(15).

       FD  ERACCT.
                                       COPY ERCACCT.

       FD  DISK-DATE                   COPY ELCDTEFD.
      /
       WORKING-STORAGE SECTION.
       77  WS-SUB                      PIC S9(3) COMP-3 VALUE +0.
       01  WS-STATUS-CODES.
           05  ERACCT-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-ACCT                 VALUE 'Y'.

       01  WS-PREV-ACCT-KEY            PIC X(19).
       01  WS-PREV-EXP-DT              PIC XX.
       01  WS-WORK-FIELDS.
           05  PGM-SUB                 PIC S9(4)   VALUE +548.
           05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           05  WS-ZERO                 PIC S9      VALUE ZERO.
           05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.
           05  WS-HIGH-SEQ             PIC X         VALUE ' '.
               88  FOUND-HIGH-SEQ-NO                 VALUE 'Y'.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-ERACCT-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ACCT-RECS-IN         PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-CURRENT-AM-KEY       PIC X(19)     VALUE LOW-VALUES.


       01  ACCOUNT-REC-IN.
           12  AI-CARRIER            PIC X.
           12  FILLER                PIC X.               
           12  AI-STATE              PIC XX.              
           12  FILLER                PIC X.               
           12  AI-ACCOUNT.                                
               16  AI-ACCOUNT-PREFIX PIC X(4).            
               16  AI-ACCOUNT-PRIME  PIC X(6).            

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.
       0000-LOAD-DATE-CARD.            COPY ELCDTERX.
       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 1000-PROCESS        THRU 1000-EXIT UNTIL
              END-OF-ACCT

           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

           OPEN INPUT ACCT-IN
           OPEN I-O   ERACCT

           IF ERACCT-FILE-STATUS = '00'
               CONTINUE
           ELSE
               DISPLAY ' ERROR ON ERACCT - OPEN - 0100 '
                  ERACCT-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-ERACCT-RECS-IN
                                          WS-ERACCT-RECS-FIX

           PERFORM 1050-READ-ACCT      THRU 1050-EXIT
           
           .
       0200-EXIT.
           EXIT.

       1000-PROCESS.

           PERFORM 1070-UPDATE-ERACCT  THRU 1070-EXIT
              
           PERFORM 1050-READ-ACCT      THRU 1050-EXIT

           .
       1000-EXIT.
           EXIT.
      /

       1050-READ-ACCT.
       
           READ ACCT-IN INTO ACCOUNT-REC-IN AT END
              SET END-OF-ACCT          TO TRUE
           END-READ
              
           IF NOT END-OF-ACCT
              ADD 1                    TO WS-ACCT-RECS-IN
           END-IF

           .
       1050-EXIT.
           EXIT.

       1070-UPDATE-ERACCT.
       
           PERFORM 1100-START-ERACCT   THRU 1100-EXIT
           PERFORM 1200-READ-ERACCT    THRU 1200-EXIT

           IF AM-ACCOUNT = AI-ACCOUNT
              MOVE 'PRNTDT'        TO AM-USER-SELECT-3
              PERFORM 2100-REWRITE-ERACCT THRU 2100-EXIT
           ELSE
               DISPLAY 'Account not found ' ACCOUNT-REC-IN
           END-IF
           
           .
       1070-EXIT.
           EXIT.
           
       1100-START-ERACCT.

           MOVE X'04'             TO AM-COMPANY-CD
           MOVE AI-CARRIER        TO AM-CARRIER
           MOVE '000000'          TO AM-GROUPING
           MOVE AI-STATE          TO AM-STATE
           MOVE AI-ACCOUNT        TO AM-ACCOUNT
           MOVE LOW-VALUES        TO AM-CNTRL-B

           START ERACCT KEY IS NOT < AM-CONTROL-PRIMARY
           
           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR ON ERACCT - START - 1100 '
                 ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .

       1100-EXIT.
           EXIT.

       1200-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (AM-COMPANY-CD > X'04')
              MOVE HIGH-VALUES         TO AM-CONTROL-A
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ON ERACCT - READ - 1200 '
                    ERACCT-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
      *           IF BIN-RUN-DATE >= AM-EFFECTIVE-DT AND
                 IF BIN-RUN-DATE < AM-EXPIRATION-DT
                         ADD 1      TO WS-ERACCT-RECS-IN
                 ELSE
                     GO TO 1200-READ-ERACCT
                 END-IF
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.


       2100-REWRITE-ERACCT.

           REWRITE ACCOUNT-MASTER

           IF ERACCT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACCT-RECS-FIX
           ELSE
              DISPLAY 'ERROR ON ERACCT - REWRITE - 2100 '
                 ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF
           .
       2100-EXIT.
           EXIT.


       3000-CLOSE-FILES.

           CLOSE ERACCT ACCT-IN

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ACCT-RECS-IN        TO WS-DISPLAY-CNT
           DISPLAY '***  SEQ ACCT    RECS IN         = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  ACCT MASTER RECS IN         = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  ACCT MASTER RECS FIXED      = ' WS-DISPLAY-CNT

           DISPLAY '***                                            ***'
           DISPLAY '**************************************************'
           .
       4000-EXIT.
           EXIT.

       9999-ABEND-RTN.

           DISPLAY '*** ACCOUNT MSTR CONVERSION PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                      TO WS-ABEND-FLD
           MOVE 0                      TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD
           .
       9999-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
