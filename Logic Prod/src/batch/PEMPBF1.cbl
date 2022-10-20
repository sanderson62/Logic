       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMPBF1.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ERPNDB           ASSIGN TO ERPNDB
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS PB-CONTROL-PRIMARY
                                   FILE STATUS IS ERPNDB-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.

       FD  ERPNDB.

                                       COPY ERCPNDB.

       WORKING-STORAGE SECTION.
       01  WS-STATUS-CODES.
           05  ERPNDB-FILE-STATUS         PIC XX        VALUE SPACES.
           05  WS-EOF-SW                  PIC X(3)      VALUE SPACES.
               88  END-OF-ERPNDB          VALUE 'YES'.

       01  WS-WORK-FIELDS.
           05  WS-ABEND-FLD               PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD                PIC 9         VALUE ZEROS.
           05  WS-ERPNDB-RECS-IN          PIC 9(9)      VALUE ZEROS.
           05  WS-ERPNDB-RECS-BYPASS      PIC 9(9)      VALUE ZEROS.
           05  WS-ERPNDB-RECS-FIX         PIC 9(9)      VALUE ZEROS.
           05  WS-ERPNDB-RECS-DEL         PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT             PIC Z,ZZZ,ZZ9 VALUE ZEROS.

       PROCEDURE DIVISION.
       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES      THRU 0100-EXIT
           PERFORM 0200-INITIALIZE      THRU 0200-EXIT
           PERFORM 1000-PROCESS         THRU 1000-EXIT
                   UNTIL END-OF-ERPNDB
           PERFORM 3000-CLOSE-FILES     THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS    THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

PEMTST     OPEN INPUT ERPNDB

PEMTST*    OPEN I-O   ERPNDB

           IF ERPNDB-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY '*** ERROR OPENING ERPNDB FILE ***'
              DISPLAY '*** STATUS CODE IS ' ERPNDB-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES              TO WS-EOF-SW
           MOVE ZEROS               TO WS-ERPNDB-RECS-IN
                                       WS-ERPNDB-RECS-BYPASS
                                       WS-ERPNDB-RECS-FIX

           PERFORM 1100-START-ERPNDB  THRU 1100-EXIT

           PERFORM 1200-READ-ERPNDB   THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       1000-PROCESS.

           IF PB-RECORD-TYPE = '1'
              IF PB-I-JOINT-BIRTHDAY = SPACES
                 MOVE LOW-VALUES       TO PB-I-JOINT-BIRTHDAY
                 DISPLAY ' FIXING JOINT BDAY ' PB-ENTRY-BATCH
                 ' ' PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
                 IF (PB-I-JOINT-AGE NOT NUMERIC)
                    OR (PB-I-JOINT-AGE = 22)
                    MOVE ZEROS TO PB-I-JOINT-AGE
                    DISPLAY ' FIXING JOINT AGE ' PB-ENTRY-BATCH
                    ' ' PB-STATE ' ' PB-ACCOUNT ' ' PB-CERT-NO
                 END-IF
                 PERFORM 2100-REWRITE-ERPNDB
                                       THRU 2100-EXIT
              END-IF
           END-IF

           PERFORM 1200-READ-ERPNDB      THRU 1200-EXIT

           .
       1000-EXIT.
           EXIT.

       1100-START-ERPNDB.

           MOVE LOW-VALUES TO PB-CONTROL-PRIMARY
           MOVE X'04'      TO PB-COMPANY-CD
           START ERPNDB KEY >= PB-CONTROL-PRIMARY
           IF ERPNDB-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERPNDB - START ' ERPNDB-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       1100-EXIT.
           EXIT.

       1200-READ-ERPNDB.

           READ ERPNDB NEXT RECORD

           IF ERPNDB-FILE-STATUS = '10' OR '23'
              SET END-OF-ERPNDB        TO TRUE
           ELSE
              IF ERPNDB-FILE-STATUS NOT = '00'
                 DISPLAY 'ERPNDB, BAD READ NEXT '
                      ERPNDB-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 ADD 1              TO WS-ERPNDB-RECS-IN
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.

       2100-REWRITE-ERPNDB.

      *    DISPLAY 'ERPNDB REWRITE ' PB-STATE ' ' PB-ACCOUNT ' '
      *           PB-CERT-NO
      *    DISPLAY ' '

PEMTST*    REWRITE PENDING-BUSINESS
PEMTST     MOVE ZEROS                  TO ERPNDB-FILE-STATUS

           IF ERPNDB-FILE-STATUS = '00'
               ADD 1               TO WS-ERPNDB-RECS-FIX
           ELSE
               DISPLAY 'ERPNDB, BAD REWRITE '
               DISPLAY '*** STATUS CODE IS ' ERPNDB-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF
           .
       2100-EXIT.
           EXIT.


       3000-CLOSE-FILES.

           CLOSE ERPNDB

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.
           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ERPNDB-RECS-IN        TO WS-DISPLAY-CNT
           DISPLAY '***  PNDB MASTER RECS IN         = ' WS-DISPLAY-CNT

           MOVE WS-ERPNDB-RECS-BYPASS    TO WS-DISPLAY-CNT
           DISPLAY '***  PNDB MASTER RECS BYPASSED   = ' WS-DISPLAY-CNT

           MOVE WS-ERPNDB-RECS-FIX       TO WS-DISPLAY-CNT
           DISPLAY '***  PNDB MASTER RECS FIXED      = ' WS-DISPLAY-CNT

           MOVE WS-ERPNDB-RECS-DEL       TO WS-DISPLAY-CNT
           DISPLAY '***  PNDB MASTER RECS DELETED    = ' WS-DISPLAY-CNT


           DISPLAY '***                                            ***'
           DISPLAY '**************************************************'
           .
       4000-EXIT. EXIT.

       9999-ABEND-RTN.
           DISPLAY '*** PENDING BUSINESS        PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                 TO WS-ABEND-FLD
           MOVE 0                 TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD
           .
       9999-EXIT. EXIT.
