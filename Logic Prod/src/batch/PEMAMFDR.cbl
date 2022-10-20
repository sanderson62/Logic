       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMAMFDR.
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

       DATA DIVISION.
       FILE SECTION.

       FD  ERACCT.

                                       COPY ERCACCT.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  WS-SEQ-NO                   PIC S9(4)   COMP VALUE +0.
       77  WS-DIS-EFF-DT               PIC X(10)  VALUE SPACES.
       77  WS-DIS-EXP-DT               PIC X(10)  VALUE SPACES.
       77  A1                          PIC S999  VALUE +0 COMP-3.
       01  WS-STATUS-CODES.
           05  WS-CURRENT-BIN-DATE     PIC XX VALUE LOW-VALUES.
           05  ERACCT-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-RW-SW                PIC X   VALUE SPACES.
               88  REWRITE-ERACCT              VALUE 'Y'.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-ERACCT               VALUE 'Y'.
               88  MORE-ERACCT                 VALUE ' '.

       01  WS-HOLD-ERACCT              PIC X(2000) VALUE LOW-VALUES.
       01  WS-WORK-FIELDS.
           05  WS-OLD-SELECT-2         PIC X(10)  VALUE SPACES.
           05  WS-ZERO                 PIC S9  VALUE ZERO COMP-3.
           05  WS-RETURN-CODE          PIC S9(4) COMP.
           05  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.
           05  WS-ABEND-MESSAGE        PIC X(80).
           05  PGM-SUB                 PIC S9(4)  COMP VALUE +504.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-ERACCT-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-ADD      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-DEL      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.

                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
                                       COPY ELCDATE.

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

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

PEMTST     OPEN I-O   ERACCT
PEMTST*    OPEN INPUT ERACCT

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-ERACCT-RECS-IN
                                          WS-ERACCT-RECS-FIX
                                          WS-ERACCT-RECS-ADD
                                          WS-ERACCT-RECS-DEL

           MOVE WS-ACCEPT-DATE         TO DC-GREG-DATE-1-YMD
           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           PERFORM 1100-ERACCT-START   THRU 1100-EXIT

           PERFORM 1200-ERACCT-READ    THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       0500-PROCESS.

            IF (AM-STATE = 'CO')
               AND (AM-ACCOUNT = '0000925100')
               AND (AM-EXPIRATION-DT = X'A52F' OR X'FFFF')
               PERFORM 1000-PROCESS     THRU 1000-EXIT
            END-IF

      *     IF (AM-STATE = 'TX')
      *        AND (AM-ACCOUNT = '0600001970')
      *        AND (AM-EXPIRATION-DT = X'A639')
      *        PERFORM 1000-PROCESS     THRU 1000-EXIT
      *     END-IF

      *     IF ((AM-STATE = 'KS')
      *        AND (AM-ACCOUNT = '0000681200'))
      *                    OR
      *        ((AM-STATE = 'MO')
      *        AND (AM-ACCOUNT = '0000681100'))
      *                    OR
      *        ((AM-STATE = 'VA')
      *        AND (AM-ACCOUNT = '0700000884'))
      *                    OR
      *        ((AM-STATE = 'VA')
      *        AND (AM-ACCOUNT = '0700000888'))
      *                    OR
      *        ((AM-STATE = 'OH')
      *        AND (AM-ACCOUNT = '0001430200'))
      *        PERFORM 1000-PROCESS     THRU 1000-EXIT
      *     END-IF


      *    IF (AM-STATE = 'TX')
      *       AND (AM-ACCOUNT =   '0600000728'
      *                    OR     '0600000729'
      *                    OR     '0600001301'
      *                    OR     '0600001320'
      *                    OR     '0600001410'
      *                    OR     '0600001411'
      *                    OR     '0600001417'
      *                    OR     '0600001418'
      *                    OR     '0600001421'
      *                    OR     '0600001422'
      *                    OR     '0600001424'
      *                    OR     '0600001425'
      *                    OR     '0600001426'
      *                    OR     '0600001558'
      *                    OR     '0600001624'
      *                    OR     '0600001722'
      *                    OR     '0600009084'
      *                    OR     '0600009085'
      *                    OR     '0600009086'
      *                    OR     '0600009097'
      *                    OR     '0600009105'
      *                    OR     '0600990016')
      *       AND (AM-EXPIRATION-DT = X'A581')
      *       PERFORM 1000-PROCESS     THRU 1000-EXIT
      *    END-IF

           PERFORM 1200-ERACCT-READ    THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           IF AM-EXPIRATION-DT = X'FFFF'
              MOVE 20110215            TO AM-EFFECT-DT
              MOVE X'A6AF'             TO AM-EFFECTIVE-DT
              PERFORM 1300-DISPLAY-ACCOUNT-INFO
                                       THRU 1300-EXIT
              DISPLAY ' ABOUT TO REWRITE ' AM-CARRIER ' ' AM-STATE
                 ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
              PERFORM 2100-ERACCT-REWRITE
                                       THRU 2100-EXIT
           ELSE
              MOVE ACCOUNT-MASTER      TO WS-HOLD-ERACCT
              PERFORM 1300-DISPLAY-ACCOUNT-INFO
                                       THRU 1300-EXIT
              DISPLAY ' ABOUT TO DELETE ' AM-CARRIER ' ' AM-STATE
                   ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
              PERFORM 2300-DELETE-ERACCT
                                       THRU 2300-EXIT
              MOVE WS-HOLD-ERACCT      TO ACCOUNT-MASTER
              MOVE X'A6AF'             TO AM-EXPIRATION-DT
                                          AM-VG-EXPIRATION-DT
              PERFORM 1300-DISPLAY-ACCOUNT-INFO
                                       THRU 1300-EXIT
              DISPLAY ' ABOUT TO ADD ' AM-CARRIER ' ' AM-STATE
                 ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
              PERFORM 2200-WRITE-ERACCT
                                       THRU 2200-EXIT
           END-IF


      *       MOVE ACCOUNT-MASTER         TO WS-HOLD-ERACCT
      *       MOVE 20090101               TO AM-EFFECT-DT
      *    MOVE X'A381'                TO AM-EFFECTIVE-DT
      *    MOVE ZEROS                  TO AM-LO-CERT-DATE
      *
      *    PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                          THRU 1300-EXIT
      *    DISPLAY ' ABOUT TO REWRITE ' AM-CARRIER ' ' AM-STATE
      *       ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *    PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *    MOVE WS-HOLD-ERACCT         TO ACCOUNT-MASTER
      *    MOVE X'A381'                TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *    MOVE ZEROS                  TO AM-PREV-EXP-DT
      *                                   AM-PREV-EFF-DT
      *
      *    MOVE ZEROS                  TO AM-HI-CERT-DATE
      *    PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *    DISPLAY ' ABOUT TO ADD ' AM-CARRIER ' ' AM-STATE
      *       ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *    PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *    EVALUATE TRUE
      *       WHEN (AM-STATE = 'KS')
      *          AND (AM-ACCOUNT = '0000681200')
      *          AND (AM-EXPIRATION-DT = X'A201')
      *          MOVE ACCOUNT-MASTER   TO WS-HOLD-ERACCT
      *          MOVE 20061101         TO AM-EFFECT-DT
      *          MOVE X'A041'          TO AM-EFFECTIVE-DT
      *          MOVE ZEROS            TO AM-LO-CERT-DATE
      *                                   AM-HI-CERT-DATE
      *          MOVE SPACES           TO AM-REI-TABLE
      *          MOVE 'Y'              TO AM-RECALC-REIN
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO REWRITE ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *          MOVE WS-HOLD-ERACCT   TO ACCOUNT-MASTER
      *          MOVE X'A041'          TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *          MOVE ZEROS            TO AM-PREV-EXP-DT
      *                                   AM-PREV-EFF-DT
      *                                   AM-LO-CERT-DATE
      *                                   AM-HI-CERT-DATE
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO ADD ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *
      *       WHEN (AM-STATE = 'MO')
      *          AND (AM-ACCOUNT = '0000681100')
      *          AND (AM-EXPIRATION-DT = X'A401')
      *          MOVE ACCOUNT-MASTER   TO WS-HOLD-ERACCT
      *          MOVE 20061101         TO AM-EFFECT-DT
      *          MOVE X'A041'          TO AM-EFFECTIVE-DT
      *          MOVE ZEROS            TO AM-LO-CERT-DATE
      *                                   AM-HI-CERT-DATE
      *          MOVE SPACES           TO AM-REI-TABLE
      *          MOVE 'Y'              TO AM-RECALC-REIN
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO REWRITE ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *          MOVE WS-HOLD-ERACCT   TO ACCOUNT-MASTER
      *          MOVE X'A041'          TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *          MOVE ZEROS            TO AM-PREV-EXP-DT
      *                                   AM-PREV-EFF-DT
      *                                   AM-LO-CERT-DATE
      *                                   AM-HI-CERT-DATE
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO ADD ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *
      *       WHEN (AM-STATE = 'VA')
      *          AND (AM-ACCOUNT = '0700000884')
      *          AND (AM-EXPIRATION-DT = X'A441')
      *          MOVE ACCOUNT-MASTER   TO WS-HOLD-ERACCT
      *          MOVE 20080801         TO AM-EFFECT-DT
      *          MOVE X'A2E1'          TO AM-EFFECTIVE-DT
      *          MOVE ZEROS            TO AM-LO-CERT-DATE
      *                                   AM-HI-CERT-DATE
      *          MOVE 'A0901'          TO AM-REI-GROUP-A
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO REWRITE ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *          MOVE WS-HOLD-ERACCT   TO ACCOUNT-MASTER
      *          MOVE X'A2E1'          TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *          MOVE ZEROS            TO AM-PREV-EXP-DT
      *                                   AM-PREV-EFF-DT
      *                                   AM-LO-CERT-DATE
      *                                   AM-HI-CERT-DATE
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO ADD ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *
      *       WHEN (AM-STATE = 'VA')
      *          AND (AM-ACCOUNT = '0700000888')
      *          AND (AM-EXPIRATION-DT = X'A441')
      *          MOVE ACCOUNT-MASTER   TO WS-HOLD-ERACCT
      *          MOVE 20080801         TO AM-EFFECT-DT
      *          MOVE X'A2E1'          TO AM-EFFECTIVE-DT
      *          MOVE ZEROS            TO AM-LO-CERT-DATE
      *                                   AM-HI-CERT-DATE
      *          MOVE 'A0901'          TO AM-REI-GROUP-A
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO REWRITE ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *          MOVE WS-HOLD-ERACCT   TO ACCOUNT-MASTER
      *          MOVE X'A2E1'          TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *          MOVE ZEROS            TO AM-PREV-EXP-DT
      *                                   AM-PREV-EFF-DT
      *                                   AM-LO-CERT-DATE
      *                                   AM-HI-CERT-DATE
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO ADD ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *
      *       WHEN (AM-STATE = 'OH')
      *          AND (AM-ACCOUNT = '0001430200')
      *          AND (AM-EXPIRATION-DT = X'A641')
      *          MOVE ACCOUNT-MASTER   TO WS-HOLD-ERACCT
      *          MOVE 20101001         TO AM-EFFECT-DT
      *          MOVE X'A621'          TO AM-EFFECTIVE-DT
      *          MOVE ZEROS            TO AM-LO-CERT-DATE
      *                                   AM-HI-CERT-DATE
      *          MOVE 'E13'            TO AM-REI-TABLE
      *          MOVE SPACES           TO AM-REI-GROUP-A
      *                                   AM-REI-GROUP-B
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO REWRITE ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *          MOVE WS-HOLD-ERACCT   TO ACCOUNT-MASTER
      *          MOVE X'A621'          TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *          MOVE ZEROS            TO AM-PREV-EXP-DT
      *                                   AM-PREV-EFF-DT
      *                                   AM-LO-CERT-DATE
      *                                   AM-HI-CERT-DATE
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO ADD ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *
      *       WHEN OTHER
      *          DISPLAY ' BYPASS DATE RANGE ' AM-STATE ' ' AM-ACCOUNT
      *             ' ' AM-EFFECT-DT
      *    END-EVALUATE

      *    EVALUATE TRUE
      *       WHEN  AM-EXPIRATION-DT = X'A501'
      *          MOVE ACCOUNT-MASTER   TO WS-HOLD-ERACCT
      *          DISPLAY ' ABOUT TO DELETE ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2300-DELETE-ERACCT
      *                                THRU 2300-EXIT
      *          MOVE WS-HOLD-ERACCT   TO ACCOUNT-MASTER
      *          MOVE X'A4E1'          TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO ADD ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *       WHEN AM-EXPIRATION-DT = X'FFFF'
      *          MOVE 20091201         TO AM-EFFECT-DT
      *          MOVE X'A4E1'          TO AM-EFFECTIVE-DT
      *          DISPLAY ' ABOUT TO REWRITE ' AM-CARRIER ' ' AM-STATE
      *             ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *    END-EVALUATE

           .
       1000-EXIT.
           EXIT.

       1100-ERACCT-START.

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD

           START ERACCT KEY IS >= AM-CONTROL-PRIMARY

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR ERACCT - START ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       1100-EXIT.
           EXIT.

       1200-ERACCT-READ.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERACCT TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ERACCT - READNEXT '
                      ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 ADD 1                 TO WS-ERACCT-RECS-IN
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.

       1300-DISPLAY-ACCOUNT-INFO.

           MOVE AM-EFFECTIVE-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO WS-DIS-EFF-DT
           ELSE
              MOVE 'XX/XX/XXXX'        TO WS-DIS-EFF-DT
              DISPLAY ' ERROR CONVERTING EFFECT  DATE ' AM-STATE
              ' ' AM-ACCOUNT
           END-IF

           IF AM-EXPIRATION-DT = HIGH-VALUES
              MOVE '12/31/9999'        TO WS-DIS-EXP-DT
           ELSE
              MOVE AM-EXPIRATION-DT    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              MOVE +0                  TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO WS-DIS-EXP-DT
              ELSE
                 MOVE 'XX/XX/XXXX'     TO WS-DIS-EXP-DT
                 DISPLAY ' ERROR CONVERTING EXPIRE  DATE ' AM-STATE
                 ' ' AM-ACCOUNT
              END-IF
           END-IF

           .
       1300-EXIT.
           EXIT.

       2100-ERACCT-REWRITE.

PEMTST*    MOVE '00'                   TO ERACCT-FILE-STATUS
PEMTST     REWRITE ACCOUNT-MASTER

           IF ERACCT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACCT-RECS-FIX
           ELSE
              DISPLAY ' ERROR - ERACCT - REWRITE ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       2100-EXIT.
           EXIT.

       2200-WRITE-ERACCT.

PEMTST*    MOVE '00'                   TO ERACCT-FILE-STATUS
PEMTST     WRITE ACCOUNT-MASTER

           IF ERACCT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACCT-RECS-ADD
           ELSE
              DISPLAY ' ERROR - ERACCT - WRITE ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       2200-EXIT.
           EXIT.

       2300-DELETE-ERACCT.

PEMTST*    MOVE '00'                   TO ERACCT-FILE-STATUS
PEMTST     DELETE ERACCT

           IF ERACCT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACCT-RECS-DEL
           ELSE
              DISPLAY ' ERROR - ERACCT - DELETE ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       2300-EXIT.
           EXIT.


       3000-CLOSE-FILES.

           CLOSE ERACCT

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ERACCT-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  ACCT MASTER RECS IN         = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  ACCT MASTER RECS FIXED      = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-ADD     TO WS-DISPLAY-CNT
           DISPLAY '***  ACCT MASTER RECS ADDED      = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-DEL     TO WS-DISPLAY-CNT
           DISPLAY '***  ACCT MASTER RECS DELETED    = ' WS-DISPLAY-CNT

           DISPLAY '***                                            ***'
           DISPLAY '**************************************************'

           .
       4000-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX'              USING DATE-CONVERSION-DATA.

       8500-EXIT.
           EXIT.

       ABEND-PGM  SECTION.  COPY ELCABEND  SUPPRESS.
