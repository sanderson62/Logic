       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMAMFRPC
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
       77  WS-OLD-POOL                 PIC X(6)  VALUE SPACES.
       77  WS-NEW-POOL                 PIC X(6)  VALUE SPACES.
       77  WS-OLD-LF-RET               PIC Z.ZZZZZ VALUE ZEROS.
       77  WS-NEW-LF-RET               PIC Z.ZZZZZ VALUE ZEROS.
       77  WS-OLD-AH-RET               PIC Z.ZZZZZ VALUE ZEROS.
       77  WS-NEW-AH-RET               PIC Z.ZZZZZ VALUE ZEROS.
       77  WS-CO-PREV-KEY              PIC X(29) VALUE LOW-VALUES.
       77  ERACNT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-PREV-ERACNT-KEY          PIC X(19)     VALUE LOW-VALUES.
       77  WS-ERACNT-RECS-ADD          PIC 9(9)      VALUE ZEROS.
       77  WS-CURRENT-BIN-DATE         PIC XX   VALUE LOW-VALUES.
       77  WS-DIS-EFF-DT               PIC X(10)  VALUE SPACES.
       77  WS-DIS-EXP-DT               PIC X(10)  VALUE SPACES.
       77  WS-SEQ-NO                   PIC S9(4)   COMP VALUE +0.

       01  WS-STATUS-CODES.
           05  ERACCT-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-ERACCT-SW            PIC X   VALUE SPACES.
               88  END-OF-ERACCT               VALUE 'Y'.

       01  WS-AM-KEY.
           05  WS-CARR                 PIC X.
           05  WS-STATE                PIC XX.
           05  WS-ACCOUNT              PIC X(10).

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
           05  WS-ERACCT-RECS-ADD      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-DEL      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-CURRENT-AM-KEY       PIC X(19)     VALUE LOW-VALUES.

       01  T1                          PIC S9(5)  COMP-3 VALUE +0.
       01  T1M                         PIC S9(5)  COMP-3 VALUE +0.
       01  WS-TABLE-1.
           05  WS-CON-TABLE OCCURS 101 INDEXED BY S1
              ASCENDING KEY IS WS-TBL1-KEY.
               10  WS-TBL1-KEY.
                   15  WS-TBL1-CARRIER PIC X.
                   15  WS-TBL1-STATE   PIC XX.
                   15  WS-TBL1-ACCT    PIC X(10).
               10  WS-TBL1-OLD-CD      PIC X(6).
               10  WS-TBL1-POOL-CD     PIC X(6).

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

PEMTST     OPEN I-O   ERACCT ERACNT
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
                                          WS-ERACCT-RECS-ADD
                                          WS-ERACCT-RECS-DEL

           PERFORM 1100-START-ERACCT   THRU 1100-EXIT
           PERFORM 1200-READNEXT-ERACCT
                                       THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       0500-PROCESS.

           IF (AM-STATE = 'OH')
              AND (AM-RET-GRP = 'LAWRNC')
              AND ((AM-LF-RET NOT = .15)
                          OR
                   (AM-AH-RET NOT = .15))
              PERFORM 1000-PROCESS     THRU 1000-EXIT
           END-IF

           PERFORM 1200-READNEXT-ERACCT
                                       THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE SPACES                 TO WS-NEW-POOL
                                          WS-OLD-POOL

           IF AM-EFFECT-DT > 19980930
              MOVE AM-LF-RET     TO WS-OLD-LF-RET
              MOVE AM-AH-RET     TO WS-OLD-AH-RET
              MOVE +.15          TO WS-NEW-LF-RET
                                    WS-NEW-AH-RET
                                    AM-LF-RET
                                    AM-AH-RET
      *       PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                          THRU 1300-EXIT
              PERFORM 1400-DISPLAY-ACCOUNT-INFO
                                 THRU 1400-EXIT
              PERFORM 2100-REWRITE-ERACCT
                                 THRU 2100-EXIT
              PERFORM 2400-BUILD-ERACNT
                                       THRU 2400-EXIT
           ELSE
              DISPLAY ' EFF DT PRIOR TO 10 1 1998 '                    
                 AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT ' '
                 AM-RET-GRP
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
              DISPLAY ' REACHING END ' ERACCT-FILE-STATUS
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

       1300-DISPLAY-ACCOUNT-INFO.

           MOVE AM-EFFECTIVE-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
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
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO WS-DIS-EXP-DT
              ELSE
                 MOVE 'XX/XX/XXXX'     TO WS-DIS-EXP-DT
                 DISPLAY ' ERROR CONVERTING EXPIRE  DATE ' AM-STATE
                 ' ' AM-ACCOUNT
              END-IF
           END-IF

           DISPLAY ' ABOUT TO CHG POOL CD ON '
              AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT
              ' ' WS-DIS-EXP-DT
              ' FROM ' WS-OLD-POOL ' TO ' WS-NEW-POOL

           .
       1300-EXIT.
           EXIT.

       1400-DISPLAY-ACCOUNT-INFO.

           MOVE AM-EFFECTIVE-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
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
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO WS-DIS-EXP-DT
              ELSE
                 MOVE 'XX/XX/XXXX'     TO WS-DIS-EXP-DT
                 DISPLAY ' ERROR CONVERTING EXPIRE  DATE ' AM-STATE
                 ' ' AM-ACCOUNT
              END-IF
           END-IF

           DISPLAY ' ABOUT TO CHG LIFE RET PCT ON '
              AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT
              ' ' WS-DIS-EXP-DT
              ' FROM ' WS-OLD-LF-RET ' TO ' WS-NEW-LF-RET

           DISPLAY ' ABOUT TO CHG AH RET PCT ON '
              AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT ' ' WS-DIS-EFF-DT
              ' ' WS-DIS-EXP-DT
              ' FROM ' WS-OLD-AH-RET ' TO ' WS-NEW-AH-RET

           .
       1400-EXIT.
           EXIT.

       2100-REWRITE-ERACCT.

      *    DISPLAY 'ERACCT REWRITE ' AM-CONTROL-A ' ' AM-RET-GRP
      *    DISPLAY ' '

           MOVE 'CONV'                 TO AM-LAST-MAINT-USER
           MOVE WS-CURRENT-BIN-DATE    TO AM-LAST-MAINT-DT
           MOVE +220000                TO AM-LAST-MAINT-HHMMSS

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

       2200-WRITE-ERACCT.

           DISPLAY 'ERACCT   WRITE ' AM-CONTROL-A
           DISPLAY ' '

           WRITE ACCOUNT-MASTER

           IF ERACCT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACCT-RECS-ADD
           ELSE
              DISPLAY ' ERROR - ERACCT - WRITE ' ERACCT-FILE-STATUS
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
              ADD 1                    TO WS-ERACCT-RECS-DEL
      *       SET END-OF-ERACCT        TO TRUE
           ELSE
              DISPLAY ' ERROR - ERACCT - DELETE ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       2300-EXIT.
           EXIT.

       2400-BUILD-ERACNT.

           IF WS-PREV-ERACNT-KEY = AM-CNTRL-1
              CONTINUE     
           ELSE
              PERFORM 2500-GET-ERACNT-SEQ-NO
                                       THRU 2500-EXIT
              MOVE AM-CNTRL-1          TO WS-PREV-ERACNT-KEY
              MOVE AM-COMPANY-CD       TO NT-COMPANY-CD
              MOVE '1'                 TO NT-RECORD-TYPE
              MOVE AM-CARRIER          TO NT-CARRIER
              MOVE AM-GROUPING         TO NT-GROUPING
              MOVE AM-STATE            TO NT-STATE
              MOVE AM-ACCOUNT          TO NT-ACCOUNT
              MOVE WS-SEQ-NO           TO NT-LINE-SEQUENCE
              MOVE 'CONV'              TO NT-LAST-MAINT-BY
              MOVE WS-CURRENT-BIN-DATE TO NT-LAST-MAINT-DT
              MOVE +220000             TO NT-LAST-MAINT-HHMMSS
      *       STRING 'CHANGED PCT FROM .20 TO .15' WS-OLD-POOL
      *          ' TO ' WS-NEW-POOL DELIMITED BY SIZE INTO NT-NOTE-LINE
      *       END-STRING
              MOVE 'CHANGED RET PCT FROM .20 TO .15'
                                       TO NT-NOTE-LINE
              PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT
           END-IF

           .
       2400-EXIT.
           EXIT.

       2450-WRITE-ERACNT.

PEMTST     WRITE NOTE-FILE
PEMTST*    MOVE '00' TO ERACNT-FILE-STATUS

           IF ERACNT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACNT-RECS-ADD
           ELSE
              IF ERACNT-FILE-STATUS = '22'
                 ADD +1                TO NT-LINE-SEQUENCE
              ELSE
                 DISPLAY 'ERACNT, BAD   WRITE '
                 DISPLAY '*** STATUS CODE IS ' ERACNT-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              END-IF
           END-IF

           .
       2450-EXIT.
           EXIT.

       2500-GET-ERACNT-SEQ-NO.

           MOVE +4096                  TO WS-SEQ-NO
           PERFORM 2510-START-ERACNT   THRU 2510-EXIT
           PERFORM 2520-READNEXT-ERACNT
                                       THRU 2520-EXIT
           IF (NT-CONTROL-PRIMARY (1:20) = AM-CONTROL-PRIMARY (1:20))
              AND (NT-RECORD-TYPE = '1')
              IF NT-LINE-SEQUENCE > +1
                 COMPUTE WS-SEQ-NO = NT-LINE-SEQUENCE - +1
                 DISPLAY ' GOT NOTE SEQ ' AM-ACCOUNT
                    ' ' NT-LINE-SEQUENCE
              ELSE
                 DISPLAY ' SEQUENCE NO PROBLEM ' AM-CARRIER ' '
                    AM-STATE ' ' AM-ACCOUNT
              END-IF
           ELSE
              DISPLAY ' NO CURRENT NOTES FOR ' AM-CARRIER ' '
                 AM-STATE ' ' AM-ACCOUNT
           END-IF

           .
       2500-EXIT.
           EXIT.

       2510-START-ERACNT.

           MOVE AM-COMPANY-CD       TO NT-COMPANY-CD
           MOVE '1'                 TO NT-RECORD-TYPE
           MOVE AM-CARRIER          TO NT-CARRIER
           MOVE AM-GROUPING         TO NT-GROUPING
           MOVE AM-STATE            TO NT-STATE
           MOVE AM-ACCOUNT          TO NT-ACCOUNT
           MOVE +0                  TO NT-LINE-SEQUENCE

           START ERACNT KEY >= NT-CONTROL-PRIMARY

           IF ERACNT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACNT - START ' ERACNT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       2510-EXIT.
           EXIT.

       2520-READNEXT-ERACNT.

           READ ERACNT NEXT RECORD

           IF (ERACNT-FILE-STATUS = '10' OR '23')
              OR (NT-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              DISPLAY ' REACHING END OF ERACNT' ERACNT-FILE-STATUS
           ELSE
              IF ERACNT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERACNT - READNEXT '
                    ERACNT-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              END-IF
           END-IF

           .
       2520-EXIT.
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

           MOVE WS-ERACCT-RECS-ADD     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS ADDED    = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-DEL     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS DELETED  = ' WS-DISPLAY-CNT

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
