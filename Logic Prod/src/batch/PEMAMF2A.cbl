       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMAMF2A.
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

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERACNT.

           COPY ERCACNT.

       WORKING-STORAGE SECTION.
       77  A1                          PIC S999  VALUE +0 COMP-3.
       01  WS-STATUS-CODES.
           05  WS-CURRENT-BIN-DATE     PIC XX VALUE LOW-VALUES.
           05  ERACCT-FILE-STATUS      PIC XX  VALUE SPACES.
           05  ERACNT-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-PREV-ERACNT-KEY      PIC X(19)     VALUE LOW-VALUES.
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
           05  WS-ERACNT-RECS-ADD      PIC 9(9)      VALUE ZEROS.
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

PEMTST     OPEN I-O   ERACCT ERACNT
PEMTST*    OPEN INPUT ERACCT ERACNT

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERACNT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ERACNT - OPEN ' ERACNT-FILE-STATUS
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

           PERFORM 1000-PROCESS        THRU 1000-EXIT

           PERFORM 1200-ERACCT-READ    THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE ' '                    TO WS-RW-SW
           EVALUATE TRUE
              WHEN (AM-STATE = 'TN')
                 AND (AM-REPORT-CODE-1 = 'NERO')
                 AND (AM-ACCOUNT NOT = '0900091033' AND '0900733033'
                     AND '0900733034' AND '0990000329' AND '0990000330'
                     AND '0990000334' AND '0990000335' AND '0990000336'
                     AND '0990000337' AND '0990000391' AND '0990000407'
                     AND '0997329001' AND '0997329002' AND '0997416001'
                     AND '0997464002')
                 DISPLAY ' FOUND TN ACCOUNT ' AM-STATE ' ' AM-ACCOUNT
                 MOVE 'SERO'           TO AM-REPORT-CODE-1
                 MOVE 'MIKE D'         TO AM-USER-SELECT-2
                 SET REWRITE-ERACCT    TO TRUE
                 PERFORM VARYING A1 FROM +2 BY +1 UNTIL
                    A1 > +10
                    IF AM-AGT (A1) = '0000957900'
                       MOVE '0000957700'  TO AM-AGT (A1)
                    END-IF
                 END-PERFORM
              WHEN (AM-STATE = 'NM')
                 AND (AM-REPORT-CODE-1 = 'SWRO')
                 DISPLAY ' FOUND NM ACCOUNT ' AM-STATE ' ' AM-ACCOUNT
                 MOVE 'RMRO-S'         TO AM-REPORT-CODE-1
                 MOVE 'SCOTT S'        TO AM-USER-SELECT-2
                 SET REWRITE-ERACCT    TO TRUE
                 PERFORM VARYING A1 FROM +2 BY +1 UNTIL
                    A1 > +10
                    IF AM-AGT (A1) = '0000810200'
                       MOVE '0000628900'  TO AM-AGT (A1)
                    END-IF
                 END-PERFORM
              WHEN (AM-STATE = 'FL')
                 AND (AM-REPORT-CODE-1 = 'FLRO')
                 PERFORM VARYING A1 FROM +2 BY +1 UNTIL
                    A1 > +10
                    IF AM-AGT (A1) = '0000957700'
                       MOVE '0000907200' TO AM-AGT (A1)
                       SET REWRITE-ERACCT    TO TRUE
                       DISPLAY ' FIXING AGT FROM 957700 TO 907200 '
                          AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT ' '
                             AM-REPORT-CODE-1
                    END-IF
                 END-PERFORM
              WHEN (AM-STATE = 'IN' OR 'MI')
                 AND (AM-REPORT-CODE-1 = 'GLRO')
                 PERFORM VARYING A1 FROM +2 BY +1 UNTIL
                    A1 > +10
                    IF AM-AGT (A1) = '0000957900'
                       MOVE '0000907100' TO AM-AGT (A1)
                       SET REWRITE-ERACCT    TO TRUE
                       DISPLAY ' FIXING AGT FROM 957900 TO 907100 '
                          AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT ' '
                             AM-REPORT-CODE-1
                    END-IF
                 END-PERFORM
              WHEN (AM-STATE = 'KY')
                 AND (AM-REPORT-CODE-1 = 'GLRO')
                 PERFORM VARYING A1 FROM +2 BY +1 UNTIL
                    A1 > +10
                    IF AM-AGT (A1) = '0000957901'
                       MOVE '0000907101' TO AM-AGT (A1)
                       SET REWRITE-ERACCT    TO TRUE
                       DISPLAY ' FIXING AGT FROM 957901 TO 907101 '
                          AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT ' '
                             AM-REPORT-CODE-1
                    END-IF
                 END-PERFORM
              WHEN (AM-STATE = 'NC' OR 'SC')
                 AND (AM-REPORT-CODE-1 = 'SERO')
                 PERFORM VARYING A1 FROM +2 BY +1 UNTIL
                    A1 > +10
                    IF AM-AGT (A1) = '0000957800'
                       MOVE '0000957700' TO AM-AGT (A1)
                       SET REWRITE-ERACCT    TO TRUE
                       DISPLAY ' FIXING AGT FROM 957800 TO 957700 '
                          AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT ' '
                             AM-REPORT-CODE-1
                    END-IF
                 END-PERFORM
           END-EVALUATE

           IF AM-REPORT-CODE-2 = 'JMGOSSETT'
              IF AM-EXPIRATION-DT <= X'A121'
                 MOVE 'J99'            TO AM-REI-TABLE
                 MOVE 'Y'              TO AM-RECALC-REIN
                 SET REWRITE-ERACCT    TO TRUE
              END-IF
              IF (AM-EFFECTIVE-DT < X'A121')
                 AND (AM-EXPIRATION-DT > X'A121')
                 MOVE ACCOUNT-MASTER   TO WS-HOLD-ERACCT
                 MOVE 20070601         TO AM-EFFECT-DT
                 MOVE X'A121'          TO AM-EFFECTIVE-DT
                 PERFORM 2100-ERACCT-REWRITE
                                       THRU 2100-EXIT
                 MOVE WS-HOLD-ERACCT   TO ACCOUNT-MASTER
                 MOVE 'J99'            TO AM-REI-TABLE
                 MOVE 'Y'              TO AM-RECALC-REIN
                 MOVE X'A121'          TO AM-EXPIRATION-DT
                                          AM-VG-EXPIRATION-DT
                 PERFORM 2200-WRITE-ERACCT
                                       THRU 2200-EXIT
              END-IF
           END-IF

           IF AM-RETRO-POOL = 'JAM055'
              MOVE 'JMA055'            TO AM-RETRO-POOL
              SET REWRITE-ERACCT       TO TRUE
              DISPLAY ' FIXING POOL FROM JAM055 TO JMA055 '
           END-IF

           IF REWRITE-ERACCT
              PERFORM 2100-ERACCT-REWRITE
                                       THRU 2100-EXIT
           END-IF

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

       2100-ERACCT-REWRITE.

           DISPLAY ' ABOUT TO REWRITE ' AM-CARRIER ' ' AM-STATE
              ' ' AM-ACCOUNT ' ' AM-REPORT-CODE-1 ' ' AM-REPORT-CODE-2

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

           DISPLAY 'ERACCT   WRITE ' AM-CONTROL-A
           DISPLAY ' '

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

           DISPLAY 'ERACCT  DELETE ' AM-CONTROL-A
           DISPLAY ' '

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

       2400-BUILD-ERACNT.

           IF WS-PREV-ERACNT-KEY = AM-CNTRL-1
              CONTINUE     
           ELSE
              MOVE AM-CNTRL-1          TO WS-PREV-ERACNT-KEY
              MOVE AM-COMPANY-CD       TO NT-COMPANY-CD
              MOVE '1'                 TO NT-RECORD-TYPE
              MOVE AM-CARRIER          TO NT-CARRIER
              MOVE AM-GROUPING         TO NT-GROUPING
              MOVE AM-STATE            TO NT-STATE
              MOVE AM-ACCOUNT          TO NT-ACCOUNT
              MOVE +1                  TO NT-LINE-SEQUENCE
              MOVE 'CONV'              TO NT-LAST-MAINT-BY
              MOVE WS-CURRENT-BIN-DATE TO NT-LAST-MAINT-DT
              MOVE +220000             TO NT-LAST-MAINT-HHMMSS
              MOVE 'REMOVED REPORT CODE 2 - ALL DATE RANGES '
                                       TO NT-NOTE-LINE
              MOVE '22'                TO ERACNT-FILE-STATUS
              PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT UNTIL
                 (ERACNT-FILE-STATUS NOT = '22')
           END-IF

           .
       2400-EXIT.
           EXIT.

       2450-WRITE-ERACNT.

           WRITE NOTE-FILE     

           IF ERACNT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACNT-RECS-ADD
           ELSE
              IF ERACNT-FILE-STATUS = '22'
                 ADD +1                TO NT-LINE-SEQUENCE
              ELSE
                 DISPLAY 'ERROR - ERACNT - WRITE ' ERACNT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       2450-EXIT.
           EXIT.

       3000-CLOSE-FILES.

           CLOSE ERACCT ERACNT

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
