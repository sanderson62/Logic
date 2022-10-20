       IDENTIFICATION DIVISION.
       PROGRAM-ID. AKCKCF01.
       AUTHOR.     AJRA.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ELCNTL           ASSIGN TO ELCNTL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CF-CONTROL-PRIMARY
                                   FILE STATUS IS ELCNTL-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

      /
       FD  ELCNTL.

           COPY ELCCNTL.
      /
       WORKING-STORAGE SECTION.
       77  SUB1                        PIC S9(3) COMP-3 VALUE +0.
       01  WS-STATUS-CODES.
           05  ELCNTL-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-ELCNTL               VALUE 'Y'.
               88  MORE-ELCNTL                 VALUE ' '.

       01  WS-HOLD-ELCNTL              PIC X(2000) VALUE LOW-VALUES.
       01  WS-WORK-FIELDS.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-ELCNTL-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ELCNTL-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-ELCNTL-RECS-DEL      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
       01  WS-INITIAL-CHECK-NUM        PIC S9(8) COMP VALUE +99999.
      /
       PROCEDURE DIVISION.
       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 1000-PROCESS        THRU 1000-EXIT UNTIL
              END-OF-ELCNTL
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

           OPEN I-O   ELCNTL
      *    OPEN INPUT ELCNTL

           IF ELCNTL-FILE-STATUS = '00'
               CONTINUE
           ELSE
               DISPLAY '*** ERROR OPENING ELCNTL FILE ***'
               DISPLAY '*** STATUS CODE IS ' ELCNTL-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-ELCNTL-RECS-IN
                                          WS-ELCNTL-RECS-FIX
                                          WS-ELCNTL-RECS-DEL

           PERFORM 1100-START-ELCNTL   THRU 1100-EXIT

           PERFORM 1200-READ-ELCNTL    THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       1000-PROCESS.

           IF CF-RECORD-TYPE = '3' AND CF-STATE-CODE = 'AK'
              MOVE WS-INITIAL-CHECK-NUM TO CF-ST-CHECK-COUNTER
              PERFORM 2100-REWRITE-ELCNTL
                                       THRU 2100-EXIT
              SET END-OF-ELCNTL TO TRUE                         
           END-IF

           PERFORM 1200-READ-ELCNTL    THRU 1200-EXIT

           .
       1000-EXIT.
           EXIT.
      /

       1100-START-ELCNTL.

           MOVE LOW-VALUES             TO CF-CONTROL-PRIMARY
           MOVE 'CID'                  TO CF-COMPANY-ID
           MOVE '3'                    TO CF-RECORD-TYPE
           MOVE 'AK  '                 TO CF-ACCESS-OF-STATE
           MOVE +0                     TO CF-SEQUENCE-NO

           START ELCNTL KEY IS NOT <
                            CF-CONTROL-PRIMARY
           IF ELCNTL-FILE-STATUS NOT = '00'
              DISPLAY ' ELCNTL, BAD START '
                    ELCNTL-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .

       1100-EXIT.
           EXIT.

       1200-READ-ELCNTL.

           READ ELCNTL NEXT RECORD

           IF ELCNTL-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCNTL TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL, BAD READ NEXT '
                      ELCNTL-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 if cf-company-id = 'DCC'
                    set end-of-elcntl to true
                 else
                    ADD 1              TO WS-ELCNTL-RECS-IN
                 end-if
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.


       2100-REWRITE-ELCNTL.

           REWRITE CONTROL-FILE

           IF ELCNTL-FILE-STATUS = '00'
               ADD 1                   TO WS-ELCNTL-RECS-FIX
           ELSE
               DISPLAY 'ELCNTL, BAD REWRITE '
               DISPLAY '*** STATUS CODE IS ' ELCNTL-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF
           .
       2100-EXIT.
           EXIT.


       3000-CLOSE-FILES.

           CLOSE ELCNTL

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ELCNTL-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  CNTL RECORDS     IN         = ' WS-DISPLAY-CNT

           MOVE WS-ELCNTL-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  CNTL RECORDS     FIXED      = ' WS-DISPLAY-CNT

           MOVE WS-ELCNTL-RECS-DEL     TO WS-DISPLAY-CNT
           DISPLAY '***  CNTL RECORDS     DELETED    = ' WS-DISPLAY-CNT


           DISPLAY '***                                            ***'
           DISPLAY '**************************************************'
           .
       4000-EXIT.
           EXIT.

       9999-ABEND-RTN.

           DISPLAY '*** ELCNTL  FILE CONVERSION PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                      TO WS-ABEND-FLD
           MOVE 0                      TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD
           .
       9999-EXIT.
           EXIT.
