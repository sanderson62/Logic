       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMSTF1.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELCIST           ASSIGN TO ELCIST
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS ST-CONTROL-PRIMARY
                                   FILE STATUS IS ELCIST-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ELCIST.

                                       COPY ELCCIST.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  WS-SEQ-NO                   PIC S9(4)   COMP VALUE +0.
       77  WS-DIS-EFF-DT               PIC X(10)  VALUE SPACES.
       77  WS-DIS-EXP-DT               PIC X(10)  VALUE SPACES.
       77  A1                          PIC S999  VALUE +0 COMP-3.
       01  WS-STATUS-CODES.
           05  WS-CURRENT-BIN-DATE     PIC XX VALUE LOW-VALUES.
           05  ELCIST-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-RW-SW                PIC X   VALUE SPACES.
               88  REWRITE-RECORD              VALUE 'Y'.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-INPUT                VALUE 'Y'.

       01  WS-HOLD-ELCIST              PIC X(100) VALUE LOW-VALUES.
       01  WS-WORK-FIELDS.
           05  WS-OLD-SELECT-2         PIC X(10)  VALUE SPACES.
           05  WS-ZERO                 PIC S9  VALUE ZERO COMP-3.
           05  WS-RETURN-CODE          PIC S9(4) COMP.
           05  WS-ABEND-FILE-STATUS    PIC XX  VALUE ZEROS.
           05  WS-ABEND-MESSAGE        PIC X(80).
           05  PGM-SUB                 PIC S9(4)  COMP VALUE +504.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-RECS-IN              PIC 9(9)      VALUE ZEROS.
           05  WS-RECS-FIX             PIC 9(9)      VALUE ZEROS.
           05  WS-RECS-ADD             PIC 9(9)      VALUE ZEROS.
           05  WS-RECS-DEL             PIC 9(9)      VALUE ZEROS.
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
              END-OF-INPUT

           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

PEMTST     OPEN I-O   ELCIST
PEMTST*    OPEN INPUT ELCIST

           IF ELCIST-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ELCIST - OPEN ' ELCIST-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-RECS-IN
                                          WS-RECS-FIX
                                          WS-RECS-ADD
                                          WS-RECS-DEL

           MOVE WS-ACCEPT-DATE         TO DC-GREG-DATE-1-YMD
           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           PERFORM 1100-START-FILE     THRU 1100-EXIT

           PERFORM 1200-READ-NEXT      THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       0500-PROCESS.

            IF ST-END-DATE = X'A67F'
               PERFORM 1000-PROCESS     THRU 1000-EXIT
            END-IF

           PERFORM 1200-READ-NEXT      THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE CLAIM-INTEREST-ST      TO WS-HOLD-ELCIST
           PERFORM 1300-DISPLAY-FILE-INFO
                                 THRU 1300-EXIT
           DISPLAY ' ABOUT TO DELETE ' ST-SCHED-CODE ' ' WS-DIS-EXP-DT

           PERFORM 2300-DELETE-RECORD  THRU 2300-EXIT

           MOVE WS-HOLD-ELCIST         TO CLAIM-INTEREST-ST
           MOVE X'E27F'                TO ST-END-DATE

           PERFORM 1300-DISPLAY-FILE-INFO
                                 THRU 1300-EXIT
           DISPLAY ' ABOUT TO WRITE ' ST-SCHED-CODE ' ' WS-DIS-EXP-DT
           PERFORM 2200-WRITE-RECORD   THRU 2200-EXIT

           .
       1000-EXIT.
           EXIT.

       1100-START-FILE.

           MOVE LOW-VALUES             TO ST-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO ST-COMPANY-CD

           START ELCIST KEY IS >= ST-CONTROL-PRIMARY

           IF ELCIST-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR ELCIST - START ' ELCIST-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       1100-EXIT.
           EXIT.

       1200-READ-NEXT.

           READ ELCIST NEXT RECORD

           IF (ELCIST-FILE-STATUS = '10' OR '23')
              OR (ST-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT TO TRUE
           ELSE
              IF ELCIST-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ELCIST - READNEXT '
                      ELCIST-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 ADD 1                 TO WS-RECS-IN
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.

       1300-DISPLAY-FILE-INFO.

           MOVE ST-START-DATE          TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8500-DATE-CONVERT   THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO WS-DIS-EFF-DT
           ELSE
              MOVE 'XX/XX/XXXX'        TO WS-DIS-EFF-DT
              DISPLAY ' ERROR CONVERTING EFFECT  DATE ' ST-SCHED-CODE
              ' ' ST-INT-RATE-CODE
           END-IF

           IF ST-END-DATE = HIGH-VALUES
              MOVE '12/31/9999'        TO WS-DIS-EXP-DT
           ELSE
              MOVE ST-END-DATE         TO DC-BIN-DATE-1
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
                 DISPLAY ' ERROR CONVERTING EXPIRE  DATE ' ST-SCHED-CODE
                 ' ' ST-INT-RATE-CODE
              END-IF
           END-IF

           .
       1300-EXIT.
           EXIT.

       2100-REWRITE-RECORD.

PEMTST*    MOVE '00'                   TO ELCIST-FILE-STATUS
PEMTST     REWRITE CLAIM-INTEREST-ST

           IF ELCIST-FILE-STATUS = '00'
              ADD 1                    TO WS-RECS-FIX
           ELSE
              DISPLAY ' ERROR - ELCIST - REWRITE ' ELCIST-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       2100-EXIT.
           EXIT.

       2200-WRITE-RECORD.

PEMTST*    MOVE '00'                   TO ELCIST-FILE-STATUS
PEMTST     WRITE CLAIM-INTEREST-ST

           IF ELCIST-FILE-STATUS = '00'
              ADD 1                    TO WS-RECS-ADD
           ELSE
              DISPLAY ' ERROR - ELCIST - WRITE ' ELCIST-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       2200-EXIT.
           EXIT.

       2300-DELETE-RECORD.

PEMTST*    MOVE '00'                   TO ELCIST-FILE-STATUS
PEMTST     DELETE ELCIST

           IF ELCIST-FILE-STATUS = '00'
              ADD 1                    TO WS-RECS-DEL
           ELSE
              DISPLAY ' ERROR - ELCIST - DELETE ' ELCIST-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       2300-EXIT.
           EXIT.


       3000-CLOSE-FILES.

           CLOSE ELCIST

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-RECS-IN             TO WS-DISPLAY-CNT
           DISPLAY '***  RECORDS IN         = ' WS-DISPLAY-CNT

           MOVE WS-RECS-FIX            TO WS-DISPLAY-CNT
           DISPLAY '***  RECORDS FIXED      = ' WS-DISPLAY-CNT

           MOVE WS-RECS-ADD            TO WS-DISPLAY-CNT
           DISPLAY '***  RECORDS ADDED      = ' WS-DISPLAY-CNT

           MOVE WS-RECS-DEL            TO WS-DISPLAY-CNT
           DISPLAY '***  RECORDS DELETED    = ' WS-DISPLAY-CNT

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
