       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCCDPF1.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ERDUEP           ASSIGN TO ERDUEP
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS DP-CONTROL-PRIMARY
                                   FILE STATUS IS ERDUEP-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

      /
       FD  ERDUEP.

                                       COPY ERCDUEP.
      /
       WORKING-STORAGE SECTION.
       77  WS-A                        PIC S9 VALUE +1.
       77  WS-B                        PIC S9 VALUE +1.
       77  WS-C                        PIC S9 VALUE +1.
       77  WS-D                        PIC S9 VALUE -1.
       77  WS-E                        PIC S9 VALUE +0.
       77  WS-DISPLAY                  PIC -Z9.
       
       01  WS-STATUS-CODES.
           05  ERDUEP-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-ERDUEP               VALUE 'Y'.
               88  MORE-ERDUEP                 VALUE ' '.

       01  WS-HOLD-ERDUEP              PIC X(512) VALUE LOW-VALUES.
       01  WS-WORK-FIELDS.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-ERDUEP-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ERDUEP-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
      /
       PROCEDURE DIVISION.
       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 1000-PROCESS        THRU 1000-EXIT UNTIL
              END-OF-ERDUEP
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

           OPEN I-O   ERDUEP

           IF ERDUEP-FILE-STATUS = '00'
               CONTINUE
           ELSE
               DISPLAY ' ERROR - ERDUEP - OPEN - '
                  ERDUEP-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-ERDUEP-RECS-IN
                                          WS-ERDUEP-RECS-FIX

           DISPLAY ' *** - BEGIN TEST - *** '
           
           PERFORM 1100-START-ERDUEP   THRU 1100-EXIT

           PERFORM 1200-READ-ERDUEP    THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE +0                     TO DP-EXP-PREM
                                          DP-REC-PREM
                                          DP-ADJUSTMENTS
                                          DP-BASE-COMM
                                          DP-GA-COMM

           PERFORM 2100-REWRITE-ERDUEP THRU 2100-EXIT

           PERFORM 1200-READ-ERDUEP    THRU 1200-EXIT

           .
       1000-EXIT.
           EXIT.
      /

       1100-START-ERDUEP.

           MOVE LOW-VALUES             TO DP-CONTROL-PRIMARY
           MOVE X'05'                  TO DP-COMPANY-CD

           START ERDUEP KEY IS NOT < DP-CONTROL-PRIMARY

           IF ERDUEP-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERDUEP - START - '
                 ERDUEP-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .

       1100-EXIT.
           EXIT.

       1200-READ-ERDUEP.

           READ ERDUEP NEXT RECORD

           IF ERDUEP-FILE-STATUS = '10' OR '23'
              SET END-OF-ERDUEP TO TRUE
           ELSE
              IF ERDUEP-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERDUEP - READ NEXT - '
                    ERDUEP-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 IF DP-COMPANY-CD > X'05'
                    SET END-OF-ERDUEP TO TRUE
                 ELSE
                    ADD 1              TO WS-ERDUEP-RECS-IN
                 END-IF
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.


       2100-REWRITE-ERDUEP.

           REWRITE DUE-PREMIUM-RECORD

           IF ERDUEP-FILE-STATUS = '00'
               ADD 1                   TO WS-ERDUEP-RECS-FIX
           ELSE
               DISPLAY ' ERROR - ERDUEP - REWRITE - '
                  ERDUEP-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF
           .
       2100-EXIT.
           EXIT.


       3000-CLOSE-FILES.

           CLOSE ERDUEP

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ERDUEP-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  DUEP MASTER RECS IN         = ' WS-DISPLAY-CNT

           MOVE WS-ERDUEP-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  DUEP MASTER RECS FIXED      = ' WS-DISPLAY-CNT


           DISPLAY '***                                            ***'
           DISPLAY '**************************************************'
           .
       4000-EXIT.
           EXIT.

       9999-ABEND-RTN.

           DISPLAY '*** DUE PREMIUM FIX - IT    PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                      TO WS-ABEND-FLD
           MOVE 0                      TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD
           .
       9999-EXIT.
           EXIT.
