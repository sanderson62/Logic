       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDBLDRPTC.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERACCT           ASSIGN TO ERACCT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT ERRPTC           ASSIGN TO ERRPTC
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS RA-CONTROL-PRIMARY
                                   FILE STATUS IS ERRPTC-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

      /
       FD  ERACCT.

                                       COPY ERCACCT.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERRPTC.

                                       COPY ERCRPTC.
      /
       WORKING-STORAGE SECTION.
       77  PGM-SUB                     PIC S999  COMP   VALUE +502.
       77  WS-RETURN-CODE              PIC S9(4) COMP   VALUE +0.
       77  WS-ABEND-MESSAGE            PIC X(80)        VALUE SPACES.
       77  WS-ABEND-FILE-STATUS        PIC XX           VALUE ZEROS.
       77  WS-ZERO                     PIC S9    COMP-3 VALUE +0.
       01  WS-STATUS-CODES.
           05  ERACCT-FILE-STATUS      PIC XX  VALUE SPACES.
           05  ERRPTC-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-ERACCT               VALUE 'Y'.

       01  WS-HOLD-ERACCT              PIC X(2000) VALUE LOW-VALUES.
       01  WS-WORK-FIELDS.
           05  WS-HIGH-SEQ             PIC X         VALUE ' '.
               88  FOUND-HIGH-SEQ-NO                 VALUE 'Y'.
           05  WS-ERACCT-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ERRPTC-RECS-ADD      PIC 9(9)      VALUE ZEROS.
           05  WS-ERRPTC-RECS-DUP      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-CONVERT-SW           PIC X         VALUE SPACES.
               88  WS-CONVERT                        VALUE 'Y'.
           05  SUB1                    PIC S999      VALUE +0 COMP-3.
           05  WS-PREV-ERACNT-KEY      PIC X(19)     VALUE LOW-VALUES.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.
       0000-MAINLINE.

                                       COPY ELCDTERX.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 1000-PROCESS        THRU 1000-EXIT UNTIL
              END-OF-ERACCT
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

           OPEN INPUT  ERACCT
                I-O    ERRPTC

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY '*** ERROR OPENING ERACCT FILE ***'
              DISPLAY '*** STATUS CODE IS ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERRPTC-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY '*** ERROR OPENING ERRPTC FILE ***'
              DISPLAY '*** STATUS CODE IS ' ERRPTC-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-ERACCT-RECS-IN
                                          WS-ERRPTC-RECS-ADD
                                          WS-ERRPTC-RECS-DUP

           PERFORM 1100-START-ERACCT   THRU 1100-EXIT

           PERFORM 1200-READ-ERACCT    THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       1000-PROCESS.

           IF AM-REMIT-TO NOT NUMERIC
              MOVE ZEROS               TO AM-REMIT-TO
           END-IF

           IF AM-REMIT-TO = ZEROS
              MOVE 01                  TO AM-REMIT-TO
           END-IF
           
           MOVE SPACES                 TO REPORT-CD-ACCOUNT
           MOVE 'RA'                   TO RA-RECORD-ID
           MOVE AM-COMPANY-CD          TO RA-COMPANY-CD
           MOVE AM-CARRIER             TO RA-CARRIER
           MOVE AM-GROUPING            TO RA-GROUPING
           MOVE AM-AGT (AM-REMIT-TO)   TO RA-FIN-RESP
           MOVE AM-AGT (1)             TO RA-ACCOUNT
           MOVE AM-REPORT-CODE-1       TO RA-REPORT-CODE-1
           MOVE AM-REPORT-CODE-2       TO RA-REPORT-CODE-2
           MOVE AM-EXPIRATION-DT       TO RA-EXPIRATION-DT
           
           WRITE REPORT-CD-ACCOUNT
           
           IF ERRPTC-FILE-STATUS = '00'
              ADD 1                    TO WS-ERRPTC-RECS-ADD
           ELSE
              IF ERRPTC-FILE-STATUS = '22'
                 READ ERRPTC
                 IF ERRPTC-FILE-STATUS = '00'
                    IF AM-EXPIRATION-DT > RA-EXPIRATION-DT
                       MOVE AM-REPORT-CODE-1
                                       TO RA-REPORT-CODE-1
                       MOVE AM-REPORT-CODE-2
                                       TO RA-REPORT-CODE-2
                       MOVE AM-EXPIRATION-DT
                                       TO RA-EXPIRATION-DT
                       REWRITE REPORT-CD-ACCOUNT
                       IF ERRPTC-FILE-STATUS = '00'
                          ADD 1        TO WS-ERRPTC-RECS-DUP
                       ELSE
                          DISPLAY ' BAD REWRITE ERRPTC '
                                       ERRPTC-FILE-STATUS
                          PERFORM ABEND-PGM
                       END-IF
                    END-IF
                 ELSE
                    DISPLAY ' BAD READ ERRPTC ' ERRPTC-FILE-STATUS
                    PERFORM ABEND-PGM
                 END-IF
              ELSE
                 DISPLAY ' BAD WRITE ERRPTC ' ERRPTC-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           PERFORM 1200-READ-ERACCT    THRU 1200-EXIT

           .
       1000-EXIT.
           EXIT.
      /

       1100-START-ERACCT.

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD

           START ERACCT KEY IS NOT < AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERACCT, BAD START '
                    ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       1100-EXIT.
           EXIT.

       1200-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (AM-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERACCT             TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY 'ERACCT, BAD READ NEXT '
                      ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 ADD 1                 TO WS-ERACCT-RECS-IN
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.



       3000-CLOSE-FILES.

           CLOSE ERACCT ERRPTC

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY '*** ERROR CLOSING ERACCT FILE ***'
              DISPLAY '*** STATUS CODE IS ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERRPTC-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY '*** ERROR CLOSING ERRPTC FILE ***'
              DISPLAY '*** STATUS CODE IS ' ERRPTC-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF


           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ERACCT-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  ACCT MASTER RECS IN         = ' WS-DISPLAY-CNT

           MOVE WS-ERRPTC-RECS-ADD     TO WS-DISPLAY-CNT
           DISPLAY '***  REPORT CODE RECS ADDED      = ' WS-DISPLAY-CNT

           MOVE WS-ERRPTC-RECS-DUP     TO WS-DISPLAY-CNT
           DISPLAY '***  REPORT CODE RECS DUPPED     = ' WS-DISPLAY-CNT

           DISPLAY '***                                            ***'
           DISPLAY '**************************************************'
           .
       4000-EXIT.
           EXIT.

       ABEND-PGM.

           DISPLAY '************************************************'
           DISPLAY '***          PROGRAM ABORTING                ***'
           DISPLAY '************************************************'
                                       COPY ELCABEND.

           .
       9999-END-JOB.
       
