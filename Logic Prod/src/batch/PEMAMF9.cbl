       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMAMF9.
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

       DATA DIVISION.
       FILE SECTION.

       FD  ERACCT.

                                       COPY ERCACCT.

       WORKING-STORAGE SECTION.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  S2                          PIC S999 COMP-3 VALUE +0.
       77  WS-IO-SW                    PIC X VALUE SPACES.
           88  WS-REWRITE                 VALUE 'R'.
       77  WS-WORK-CITY                PIC X(30).
       77  WS-REPORT-CODE-1            PIC X(10) VALUE SPACES.
       01  WS-STATUS-CODES.
           05  ERACCT-FILE-STATUS      PIC XX  VALUE '00'.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-ERACCT               VALUE 'Y'.

       01  WS-HOLD-ERACCT              PIC X(2000) VALUE LOW-VALUES.
       01  WS-WORK-FIELDS.
           05  WS-DCC-COMP             PIC X.
           05  WS-CID-COMP             PIC X.
           05  WS-HIGH-SEQ             PIC X         VALUE ' '.
               88  FOUND-HIGH-SEQ-NO                 VALUE 'Y'.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-ERACCT-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-BYPASS   PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-ADD      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-DEL      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-CURRENT-AM-KEY       PIC X(19)     VALUE LOW-VALUES.

       PROCEDURE DIVISION.
       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT

           PERFORM 0300-PROCESS        THRU 0300-EXIT UNTIL
              END-OF-ERACCT
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

PEMTST*    OPEN INPUT ERACCT
PEMTST     OPEN I-O   ERACCT

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-ERACCT-RECS-IN
                                          WS-ERACCT-RECS-BYPASS
                                          WS-ERACCT-RECS-FIX
                                          WS-ERACCT-RECS-ADD
                                          WS-ERACCT-RECS-DEL

           MOVE X'04'                  TO WS-CID-COMP
           MOVE X'05'                  TO WS-DCC-COMP

           PERFORM 1100-START-ERACCT   THRU 1100-EXIT
           PERFORM 1200-READ-ERACCT    THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       0300-PROCESS.

           IF (AM-REPORT-CODE-3 = 'RLIC')
              AND (AM-REPORT-CODE-2 = '00000000  ')
              PERFORM 0400-PROCESS     THRU 0400-EXIT
           ELSE
              ADD 1                    TO WS-ERACCT-RECS-BYPASS
           END-IF

      *    IF (AM-CARRIER = '9')
      *       AND (AM-ACCOUNT (1:4) = '0700' OR '7000')
      *       AND (AM-ACCOUNT (1:2) = '09')
      *       AND (AM-STATE = 'SC' OR 'NC')
      *       AND (AM-REPORT-CODE-1 = 'JM&A' OR 'UNI')
      *       PERFORM 0400-PROCESS     THRU 0400-EXIT
      *    ELSE
      *       ADD 1                    TO WS-ERACCT-RECS-BYPASS
      *    END-IF

           PERFORM 1200-READ-ERACCT    THRU 1200-EXIT

           .
       0300-EXIT.
           EXIT.

       0400-PROCESS.

      *    IF SPACES = AM-CAL-TABLE OR AM-LF-DEVIATION
      *       OR AM-AH-DEVIATION
      *       IF AM-CAL-TABLE = SPACES
      *          DISPLAY ' CHANGING CLASS ON ' AM-STATE ' ' AM-ACCOUNT
      *             ' FROM ' AM-CAL-TABLE ' TO 00 '
      *          MOVE ZEROS            TO AM-CAL-TABLE
      *       END-IF
      *       IF AM-LF-DEVIATION = SPACES
      *          MOVE ZEROS            TO AM-LF-DEVIATION
      *          DISPLAY ' CHANGING LF DEV ON ' AM-STATE ' ' AM-ACCOUNT
      *             ' TO ZEROS '
      *       END-IF
      *       IF AM-AH-DEVIATION = SPACES
      *          MOVE ZEROS            TO AM-AH-DEVIATION
      *          DISPLAY ' CHANGING AH DEV ON ' AM-STATE ' ' AM-ACCOUNT
      *             ' TO ZEROS '
      *       END-IF
      *       SET WS-REWRITE           TO TRUE
      *       PERFORM 2100-REWRITE-ERACCT THRU 2100-EXIT
      *    END-IF

      *    IF (AM-ACCOUNT (1:3) = '700')
      *       AND (AM-ACCOUNT (10:1) = 'L')
      *          DISPLAY ' SPACING OUT DLR NO ON ' AM-STATE
      *          ' ' AM-ACCOUNT
      *       MOVE SPACES              TO AM-ORIG-DEALER-NO
      *       SET WS-REWRITE           TO TRUE
      *    END-IF

      *    EVALUATE TRUE
      *       WHEN AM-STATE = 'AL' OR 'NC' OR 'PA'
      *          PERFORM 0410-FIX-COMMENT
      *                                THRU 0410-EXIT

      *       WHEN AM-STATE = 'AZ' OR 'CO' OR 'MT' OR
      *                       'NH' OR 'OR' OR 'TX'
      *          PERFORM 0420-FIX-CLASS
      *                                THRU 0420-EXIT
      *    END-EVALUATE

      *    PERFORM 0410-FIX-COMMENT    THRU 0410-EXIT

      *    PERFORM 0430-FIX-REST       THRU 0430-EXIT

      *    MOVE SPACES                 TO WS-REPORT-CODE-1
      *    EVALUATE AM-REPORT-CODE-1
      *       WHEN 'FLRO'
      *          MOVE 'SERO'           TO WS-REPORT-CODE-1
      *       WHEN 'CRO' 
      *          MOVE 'SERO'           TO WS-REPORT-CODE-1
      *    END-EVALUATE

      *    IF WS-REPORT-CODE-1 NOT = SPACES
      *       DISPLAY ' FIXING ACCT ' AM-STATE ' ' AM-ACCOUNT
      *          ' FROM ' AM-REPORT-CODE-1 ' TO ' WS-REPORT-CODE-1
      *       MOVE WS-REPORT-CODE-1    TO AM-REPORT-CODE-1
      *       PERFORM 2100-REWRITE-ERACCT
      *                                THRU 2100-EXIT
      *    END-IF

           MOVE 'RLIC'                 TO AM-REPORT-CODE-2
           PERFORM 2100-REWRITE-ERACCT THRU 2100-EXIT

           .
       0400-EXIT.
           EXIT.

       0410-FIX-COMMENT.

      *    EVALUATE AM-STATE
      *       WHEN 'AL'
      *          MOVE 'AL (COMBO $1 NO HA) 7-21-06'
      *                                TO AM-COMMENT-LINE (1)
      *          MOVE '30470C AL Rev (CID 520)'
      *                                TO AM-COMMENT-LINE (2)
      *       WHEN 'NC'
      *          MOVE '30470C NC Rev  (CID-502)      '
      *                                TO AM-COMMENT-LINE (2)
      *       WHEN 'PA'
      *          MOVE 'PA (Combo $1-BAGE-NO HA) 7-21-06'
      *                                TO AM-COMMENT-LINE (1)
      *          MOVE '30470A PA Rev/30470C PA Rev (CID-372)'
      *                                TO AM-COMMENT-LINE (2)
      *       WHEN OTHER
      *          DISPLAY ' HOW IN THE WORLD DID I GET HERE - 0410 '
      *             AM-STATE
      *    END-EVALUATE

      *    EVALUATE AM-STATE
      *       WHEN 'SC'
      *          MOVE 'SC (Combo $1-No HA) 30491C Sc 7-21-06'
      *                                TO AM-COMMENT-LINE (1)
      *          MOVE '30491C SC (CID-522)'
      *                                TO AM-COMMENT-LINE (2)
      *       WHEN 'NC'
      *          PERFORM VARYING S1 FROM +49 BY -1 UNTIL
      *             S1 < +5
      *             IF AM-COMMENT-LINE (2) (S1:2) = 'NP'
      *                MOVE SPACES     TO AM-COMMENT-LINE (2) (S1:2)
      *                MOVE +4         TO S1
      *             END-IF
      *          END-PERFORM
      *       WHEN OTHER
      *          DISPLAY ' HOW IN THE WORLD DID I GET HERE - 0410 '
      *             AM-STATE
      *    END-EVALUATE

           .
       0410-EXIT.
           EXIT.

       0420-FIX-CLASS.

           EVALUATE AM-STATE
              WHEN 'AZ'
                 MOVE '04'             TO AM-CAL-TABLE
              WHEN 'CO'
                 MOVE 'CR'             TO AM-CAL-TABLE
              WHEN 'MT'
                 MOVE '70'             TO AM-CAL-TABLE
              WHEN 'NH'
                 MOVE 'B '             TO AM-CAL-TABLE
              WHEN 'OR'
                 MOVE '02'             TO AM-CAL-TABLE
              WHEN 'TX'
                 MOVE 'EE'             TO AM-CAL-TABLE
              WHEN OTHER
                 DISPLAY ' HOW IN THE WORLD DID I GET HERE - 0420 '
                    AM-STATE
           END-EVALUATE

           .
       0420-EXIT.
           EXIT.

       0430-FIX-REST.

           MOVE +1                     TO S2
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +30)
              IF AM-CITY (S1:1) NOT = ','
                 MOVE AM-CITY (S1:1)   TO WS-WORK-CITY (S2:1)
                 ADD +1                TO S2
              END-IF
           END-PERFORM
           MOVE WS-WORK-CITY           TO AM-CITY

           .
       0430-EXIT.
           EXIT.

       1100-START-ERACCT.

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE WS-CID-COMP            TO AM-COMPANY-CD

           START ERACCT KEY >= AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACCT - START ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       1100-EXIT.
           EXIT.

       1200-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
              OR (AM-COMPANY-CD > WS-CID-COMP)
              SET END-OF-ERACCT             TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERACCT - READ NEXT '
                    ERACCT-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 ADD 1                 TO WS-ERACCT-RECS-IN
              END-IF
           END-IF

           MOVE AM-CONTROL-A           TO WS-CURRENT-AM-KEY

           .
       1200-EXIT.
           EXIT.

       2100-REWRITE-ERACCT.

           DISPLAY 'ERACCT REWRITE ' AM-CARRIER ' ' AM-STATE ' '
              AM-ACCOUNT ' ' AM-REPORT-CODE-2 ' ' AM-REPORT-CODE-3
      *    DISPLAY ' '

PEMTST*    MOVE '00' TO ERACCT-FILE-STATUS
PEMTST     REWRITE ACCOUNT-MASTER

           IF ERACCT-FILE-STATUS = '00'
               ADD 1                   TO WS-ERACCT-RECS-FIX
           ELSE
               DISPLAY 'ERACCT, BAD REWRITE '
               DISPLAY '*** STATUS CODE IS ' ERACCT-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF

           .
       2100-EXIT.
           EXIT.

       2200-WRITE-ERACCT.

           DISPLAY 'ERACCT   WRITE ' AM-CONTROL-A
           DISPLAY ' '

           WRITE ACCOUNT-MASTER

           IF ERACCT-FILE-STATUS = '00'
               ADD 1                   TO WS-ERACCT-RECS-ADD
           ELSE
               DISPLAY 'ERACCT, BAD   WRITE '
               DISPLAY '*** STATUS CODE IS ' ERACCT-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF

           .
       2200-EXIT.
           EXIT.

       2300-DELETE-ERACCT.

           DISPLAY 'ERACCT  DELETE ' AM-CONTROL-A
           DISPLAY ' '

           DELETE ERACCT

           IF ERACCT-FILE-STATUS = '00'
               ADD 1                   TO WS-ERACCT-RECS-DEL
      *        SET END-OF-ERACCT             TO TRUE
           ELSE
               DISPLAY 'ERACCT, BAD  DELETE '
               DISPLAY '*** STATUS CODE IS ' ERACCT-FILE-STATUS
               PERFORM 9999-ABEND-RTN
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

           MOVE WS-ERACCT-RECS-BYPASS  TO WS-DISPLAY-CNT
           DISPLAY '***  ACCT MASTER RECS BYPASSED   = ' WS-DISPLAY-CNT

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

       9999-ABEND-RTN.

           DISPLAY '*** ACCOUNT MSTR CONVERSION PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                      TO WS-ABEND-FLD
           MOVE 0                      TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD

           .
       9999-EXIT.
           EXIT.
