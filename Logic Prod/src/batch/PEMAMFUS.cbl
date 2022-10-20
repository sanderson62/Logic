       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMAMFUS
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

           SELECT ERACNT           ASSIGN TO ERACNT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS NT-CONTROL-PRIMARY
                                   FILE STATUS IS ERACNT-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ERACCT.

                                       COPY ERCACCT.

       FD  ERACNT.

                                       COPY ERCACNT.

       FD  DISK-DATE                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  WS-TABLE-EOF-SW             PIC X  VALUE SPACES.
           88  END-OF-TABLE               VALUE 'Y'.
       77  WS-CO-PREV-KEY              PIC X(29) VALUE LOW-VALUES.
       77  ERACNT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-LINE-SEQUENCE            PIC S9(4) COMP VALUE +0.
       77  WS-PREV-ERACNT-KEY          PIC X(19)     VALUE LOW-VALUES.
       77  WS-CURRENT-BIN-DATE         PIC XX   VALUE LOW-VALUES.
       77  WS-NEW-RC1                  PIC X(10).
       77  WS-NEW-US2                  PIC X(10).
       77  WS-OLD-RC1                  PIC X(10).
       77  WS-OLD-US2                  PIC X(10).
       77  WS-SAVE-ERACNT              PIC X(120).
       77  A1                          PIC S999 COMP-3 VALUE +0.
       77  WS-FIX-GA-SW                PIC X  VALUE ' '.
           88  GA-FIXED                  VALUE 'Y'.
       01  WS-STATUS-CODES.
           05  ERACCT-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-ERACCT-SW            PIC X   VALUE SPACES.
               88  END-OF-ERACCT               VALUE 'Y'.

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
           05  WS-ERACCT-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACNT-RECS-ADD      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCTS-FOUND        PIC 9(5)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-CURRENT-AM-KEY       PIC X(19)     VALUE LOW-VALUES.


                                       COPY ELCFUNDT.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

       0000-LOAD-DATE-CARD.            COPY ELCDTERX.

       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 0500-PROCESS        THRU 0500-EXIT UNTIL
              END-OF-ERACCT
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

PEMTST     OPEN I-O ERACNT ERACCT

PEMTST*    OPEN INPUT ERACCT ERACNT

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           IF ERACNT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACNT - OPEN ' ERACNT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           DISPLAY ' FUNCTION DATE CYMD ' WS-FN-DATE
           MOVE WS-FN-DATE             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           MOVE SPACES                 TO WS-ERACCT-SW

           MOVE ZEROS                  TO WS-ERACCT-RECS-IN
                                          WS-ERACCT-RECS-FIX

           PERFORM 1100-START-ERACCT   THRU 1100-EXIT
           PERFORM 1200-READNEXT-ERACCT
                                       THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       0500-PROCESS.

      *    IF AM-REPORT-CODE-1 = 'WRO'
              IF AM-USER-SELECT-2 = 'RICK S    '
                 PERFORM 1000-PROCESS  
                                       THRU 1000-EXIT
              END-IF
      *    END-IF

           PERFORM 1200-READNEXT-ERACCT
                                       THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE SPACES TO WS-NEW-RC1 WS-NEW-US2
                          WS-OLD-RC1 WS-OLD-US2 WS-FIX-GA-SW

           MOVE 'MIKE H    '           TO WS-NEW-US2

      *    EVALUATE AM-STATE
      *       WHEN 'AK'
      *          MOVE 'RMRO-N'         TO WS-NEW-RC1
      *          MOVE 'DOMINIC M'      TO WS-NEW-US2
      *       WHEN 'HI'
      *          MOVE 'RMRO-N'         TO WS-NEW-RC1
      *          MOVE 'JERRY G  '      TO WS-NEW-US2
      *       WHEN 'ID'
      *          MOVE 'RMRO-N'         TO WS-NEW-RC1
      *          MOVE 'JERRY G  '      TO WS-NEW-US2
      *       WHEN 'WA'
      *          MOVE 'RMRO-S'         TO WS-NEW-RC1
      *          MOVE 'SCOTT S  '      TO WS-NEW-US2
      *       WHEN 'NV'
      *          MOVE 'RMRO-S'         TO WS-NEW-RC1
      *          MOVE 'SCOTT S  '      TO WS-NEW-US2
      *       WHEN 'OR'
      *          MOVE 'RMRO-S'         TO WS-NEW-RC1
      *          MOVE 'DAVE R   '      TO WS-NEW-US2
      *       WHEN OTHER
      *          DISPLAY ' NOT FOUND ' AM-STATE ' ' AM-ACCOUNT ' '
      *             WS-NEW-RC1 ' ' WS-NEW-US2
      *    END-EVALUATE
      *
      *    PERFORM VARYING A1 FROM +2 BY +1 UNTIL
      *       (A1 > +10)
      *       IF AM-AGT (A1) = '0000928900'
      *          MOVE '0000628900'     TO AM-AGT (A1)
      *          SET GA-FIXED          TO TRUE
      *          DISPLAY ' UPDATING AGT NO ' AM-STATE ' ' AM-ACCOUNT
      *             ' ' AM-REPORT-CODE-1 ' ' AM-USER-SELECT-2
      *       END-IF
      *    END-PERFORM

      *    IF GA-FIXED
      *       PERFORM 2100-REWRITE-ERACCT
      *                                THRU 2100-EXIT
      *    END-IF

      *
      *    IF (WS-NEW-RC1 NOT = SPACES)
      *       OR (WS-NEW-US2 NOT = SPACES)
      *       DISPLAY ' UPDATING ' AM-CARRIER ' ' AM-STATE ' '
      *          AM-ACCOUNT ' FROM RC1 ' AM-REPORT-CODE-1 ' TO '
      *          WS-NEW-RC1 ' FROM US2 ' AM-USER-SELECT-2 ' TO '
      *          WS-NEW-US2
      *       MOVE AM-REPORT-CODE-1    TO WS-OLD-RC1
      *       MOVE AM-USER-SELECT-2    TO WS-OLD-US2
      *       MOVE WS-NEW-RC1          TO AM-REPORT-CODE-1
      *       MOVE WS-NEW-US2          TO AM-USER-SELECT-2
      *       PERFORM 2100-REWRITE-ERACCT
      *                                THRU 2100-EXIT
      *       PERFORM 2400-BUILD-ERACNT
      *                             THRU 2400-EXIT
      *    END-IF

           IF WS-NEW-US2 NOT = SPACES
              DISPLAY ' UPDATING ' AM-CARRIER ' ' AM-STATE ' '
                 AM-ACCOUNT ' FROM US2 ' AM-USER-SELECT-2 ' TO '
                 WS-NEW-US2
              MOVE AM-USER-SELECT-2    TO WS-OLD-US2
              MOVE WS-NEW-US2          TO AM-USER-SELECT-2
              PERFORM 2100-REWRITE-ERACCT
                                       THRU 2100-EXIT
              PERFORM 2400-BUILD-ERACNT
                                    THRU 2400-EXIT
           END-IF

           .
       1000-EXIT.
           EXIT.

       1100-START-ERACCT.

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD

           START ERACCT KEY >= AM-CONTROL-PRIMARY

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACCT - START ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       1100-EXIT.
           EXIT.

       1200-READNEXT-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERACCT        TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERACCT - READNEXT '
                    ERACCT-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 ADD 1                 TO WS-ERACCT-RECS-IN
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.

       2100-REWRITE-ERACCT.

      *    DISPLAY 'ERACCT REWRITE ' AM-CONTROL-A ' ' AM-CSR-CODE
      *    DISPLAY ' '

PEMTST     REWRITE ACCOUNT-MASTER
PEMTST*    MOVE '00' TO ERACCT-FILE-STATUS

           IF ERACCT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACCT-RECS-FIX
           ELSE
              DISPLAY ' ERROR - ERACCT - REWRITE ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       2100-EXIT.
           EXIT.

       2400-BUILD-ERACNT.

           IF WS-PREV-ERACNT-KEY = AM-CNTRL-1
              CONTINUE     
           ELSE
              PERFORM 2425-FIND-SEQ-NO THRU 2425-EXIT
              MOVE AM-CNTRL-1          TO WS-PREV-ERACNT-KEY
              MOVE AM-COMPANY-CD       TO NT-COMPANY-CD
              MOVE '1'                 TO NT-RECORD-TYPE
              MOVE AM-CARRIER          TO NT-CARRIER
              MOVE AM-GROUPING         TO NT-GROUPING
              MOVE AM-STATE            TO NT-STATE
              MOVE AM-ACCOUNT          TO NT-ACCOUNT
              MOVE WS-LINE-SEQUENCE    TO NT-LINE-SEQUENCE
              MOVE 'CONV'              TO NT-LAST-MAINT-BY
              MOVE WS-CURRENT-BIN-DATE TO NT-LAST-MAINT-DT
              MOVE +220000             TO NT-LAST-MAINT-HHMMSS
              MOVE SPACES              TO NT-NOTE-LINE
              IF GA-FIXED
                 MOVE '... ALSO CHANGED GA FROM 957400 TO 928900 '
                                       TO NT-NOTE-LINE
                 DISPLAY NT-NOTE-LINE
                 MOVE NOTE-FILE        TO WS-SAVE-ERACNT
                 PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT
                 MOVE WS-SAVE-ERACNT   TO NOTE-FILE
                 SUBTRACT +1           FROM NT-LINE-SEQUENCE
                 MOVE SPACES           TO NT-NOTE-LINE
              END-IF
      *       IF GA-FIXED
      *          STRING '... XFR US2 FROM ' WS-OLD-US2 ' TO '
      *             WS-NEW-US2 ' ALL DT RANGES ...' DELIMITED BY '  '
      *                                INTO NT-NOTE-LINE
      *          END-STRING
      *       ELSE
      *          STRING '... XFR US2 FROM ' WS-OLD-US2 ' TO '
      *             WS-NEW-US2 ' ALL DT RANGES' DELIMITED BY '  '
      *                                INTO NT-NOTE-LINE
      *          END-STRING
      *       END-IF
      *       DISPLAY NT-NOTE-LINE
      *       MOVE NOTE-FILE           TO WS-SAVE-ERACNT
      *       PERFORM 2450-WRITE-ERACNT
      *                                THRU 2450-EXIT
      *       MOVE WS-SAVE-ERACNT      TO NOTE-FILE
              SUBTRACT +1              FROM NT-LINE-SEQUENCE
              MOVE SPACES              TO NT-NOTE-LINE
              STRING 'EFF 3/1/2011 XFR US2 FROM ' WS-OLD-US2 ' TO '
                 WS-NEW-US2 ' ALL DT RANGES'
                 DELIMITED BY '  ' INTO NT-NOTE-LINE
              END-STRING
              DISPLAY NT-NOTE-LINE
              PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT
      *          WITH TEST AFTER UNTIL
      *          (ERACNT-FILE-STATUS NOT = '22')
           END-IF

           .
       2400-EXIT.
           EXIT.

       2425-FIND-SEQ-NO.

           MOVE +50                    TO WS-LINE-SEQUENCE
           MOVE AM-COMPANY-CD          TO NT-COMPANY-CD
           MOVE '1'                    TO NT-RECORD-TYPE
           MOVE AM-CARRIER             TO NT-CARRIER
           MOVE AM-GROUPING            TO NT-GROUPING
           MOVE AM-STATE               TO NT-STATE
           MOVE AM-ACCOUNT             TO NT-ACCOUNT
           MOVE +0                     TO NT-LINE-SEQUENCE
           START ERACNT KEY >= NT-CONTROL-PRIMARY
           IF ERACNT-FILE-STATUS = '00'
              READ ERACNT NEXT RECORD
              IF (ERACNT-FILE-STATUS = '00')
                 AND (NT-CONTROL-PRIMARY (1:20) =
                      AM-CONTROL-PRIMARY (1:20))
                 AND (NT-RECORD-TYPE = '1')
                 MOVE NT-LINE-SEQUENCE TO WS-LINE-SEQUENCE
              ELSE
                 IF ERACNT-FILE-STATUS = '10' OR '23'
                    DISPLAY ' ERACNT NOT FOUND - READNEXT'
                    AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
                 ELSE
                    DISPLAY ' NO CURRENT NOTES FOR '
                       AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
                 END-IF
              END-IF
           ELSE
              IF ERACNT-FILE-STATUS = '10' OR '23'
                 DISPLAY ' ERACNT NOT FOUND - START'
                 AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
              ELSE
                 DISPLAY ' ERROR - ERACNT - START '
                 ERACNT-FILE-STATUS ' ' AM-CARRIER ' ' AM-STATE ' '
                 AM-ACCOUNT
              END-IF
           END-IF

           IF WS-LINE-SEQUENCE > +1
              SUBTRACT +1 FROM WS-LINE-SEQUENCE
              DISPLAY ' FOUND GOOD SEQ NO ' NT-STATE ' ' NT-ACCOUNT
                 ' ' NT-LINE-SEQUENCE
           ELSE
              DISPLAY ' ACCOUNT NOTES FULL ' AM-CARRIER ' '
                 AM-STATE ' ' AM-ACCOUNT
              MOVE +2500               TO WS-LINE-SEQUENCE
           END-IF

           .
       2425-EXIT.
           EXIT.

       2450-WRITE-ERACNT.

PEMTST     WRITE NOTE-FILE     
PEMTST*    MOVE '00'                   TO ERACNT-FILE-STATUS

           IF ERACNT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACNT-RECS-ADD
           ELSE
              DISPLAY 'ERACNT, BAD   WRITE '
              DISPLAY '*** STATUS CODE IS ' ERACNT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       2450-EXIT.
           EXIT.

       3000-CLOSE-FILES.

           CLOSE ERACCT ERACNT

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - CLOSE ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           IF ERACNT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACNT - CLOSE ' ERACNT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ERACCT-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS READ     = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS FIXED    = ' WS-DISPLAY-CNT

           MOVE WS-ERACNT-RECS-ADD     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACNT MASTER RECS ADDED    = ' WS-DISPLAY-CNT

           DISPLAY '***                                            ***'
           DISPLAY '**************************************************'

           .
       4000-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
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
