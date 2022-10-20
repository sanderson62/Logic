       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMAMFRET.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILE-IN          ASSIGN TO SYS007
              ORGANIZATION IS LINE SEQUENTIAL.

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

       FD  FILE-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F. 

       01  RECORD-IN.
           05  IN-STATE                PIC XX.
           05  IN-ACCOUNT              PIC X(10).
           05  IN-EFF-DT               PIC X(8).
           05  IN-EXP-DT               PIC X(8).
           05  IN-AGT-NO               PIC X(10).
           05  OLD-VALUE               PIC X(5).
           05  NEW-VALUE               PIC X(5).

       WORKING-STORAGE SECTION.
       77  WS-TABLE-EOF-SW             PIC X  VALUE SPACES.
           88  END-OF-TABLE               VALUE 'Y'.
       77  WS-RET-OPT                  PIC X(6)  VALUE SPACES.
       77  WS-CO-PREV-KEY              PIC X(29) VALUE LOW-VALUES.
       77  ERACNT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-PREV-ERACNT-KEY          PIC X(19)     VALUE LOW-VALUES.
       77  WS-TABLE-HITS               PIC 9(9)  VALUE ZEROS.
       77  WS-ERACNT-RECS-ADD          PIC 9(9)      VALUE ZEROS.
       77  WS-ERACCTS-FOUND            PIC 9(9)      VALUE ZEROS.
       77  WS-CURRENT-BIN-DATE         PIC XX   VALUE LOW-VALUES.
       77  WS-DIS-EFF-DT               PIC X(10)  VALUE SPACES.
       77  WS-DIS-EXP-DT               PIC X(10)  VALUE SPACES.
       77  A1                          PIC S999 COMP-3 VALUE +0.
       77  WS-REWRITE-ERACCT-SW        PIC X  VALUE SPACES.
           88  REWRITE-ERACCT             VALUE 'Y'.

       01  WS-STATUS-CODES.
           05  ERACCT-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-ERACCT-SW            PIC X   VALUE SPACES.
               88  END-OF-ERACCT               VALUE 'Y'.

       01  WS-AM-KEY.
           05  WS-STATE                PIC XX.
           05  WS-ACCOUNT              PIC X(10).
           05  WS-EXP-DT               PIC XX.

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
           05  WS-ERACCT-LVL-UPD       PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-FIX      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-ADD      PIC 9(9)      VALUE ZEROS.
           05  WS-ERACCT-RECS-DEL      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.
           05  WS-CURRENT-AM-KEY       PIC X(19)     VALUE LOW-VALUES.

       01  T1                          PIC S9(5)  COMP-3 VALUE +0.
       01  T1M                         PIC S9(5)  COMP-3 VALUE +0.
       01  WS-TABLE-1.
           05  WS-CON-TABLE OCCURS 590 INDEXED BY S1
              ASCENDING KEY IS WS-TBL1-KEY.
               10  WS-TBL1-KEY.
                   15  WS-TBL1-STATE   PIC XX.
                   15  WS-TBL1-ACCT    PIC X(10).
                   15  WS-TBL1-EXP-DT  PIC XX.
               10  WS-TBL1-EFF-DT      PIC 9(8).
               10  WS-TBL1-AGT-NO      PIC X(10).
               10  WS-TBL1-OLD         PIC X.
               10  WS-TBL1-NEW         PIC X.

                                       COPY ELCFUNDT.
                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

       0000-LOAD-DATE-CARD.            COPY ELCDTERX.

       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 0300-LOAD-TABLES    THRU 0300-EXIT
           PERFORM 0400-TEST-TABLE     THRU 0400-EXIT
           PERFORM 0500-PROCESS        THRU 0500-EXIT UNTIL
              END-OF-ERACCT
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

PEMTST     OPEN I-O   ERACNT
           OPEN INPUT ERACCT
PEMTST*    OPEN INPUT ERACCT ERACNT
           OPEN INPUT FILE-IN

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

       0300-LOAD-TABLES.

           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              END-OF-TABLE
              READ FILE-IN AT END
                 SET END-OF-TABLE      TO TRUE
              END-READ
              IF NOT END-OF-TABLE
                 MOVE IN-STATE         TO WS-TBL1-STATE   (T1)
                 MOVE IN-ACCOUNT       TO WS-TBL1-ACCT    (T1)
                 MOVE IN-EFF-DT        TO WS-TBL1-EFF-DT  (T1)
                 MOVE IN-AGT-NO        TO WS-TBL1-AGT-NO  (T1)
                 MOVE OLD-VALUE (1:1)  TO WS-TBL1-OLD     (T1)
                 IF WS-TBL1-OLD (T1) = 'B'
                    MOVE ' '           TO WS-TBL1-OLD     (T1)
                 END-IF
                 MOVE NEW-VALUE (1:1)  TO WS-TBL1-NEW     (T1)
                 IF IN-EXP-DT (1:4) = '9999'
                    MOVE HIGH-VALUES   TO WS-TBL1-EXP-DT  (T1)
                 ELSE
                    MOVE IN-EXP-DT     TO DC-GREG-DATE-CYMD
                    MOVE 'L'           TO DC-OPTION-CODE
                    MOVE +0            TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
                    PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
                    IF NO-CONVERSION-ERROR
                       MOVE DC-BIN-DATE-1
                                       TO WS-TBL1-EXP-DT  (T1)
                    ELSE
                       DISPLAY ' ERROR CONVERTING TBL EXP DATE '
                    END-IF
                 END-IF
              ELSE
                 MOVE 'ZZZZZZZZZZZZZZ' TO WS-TBL1-KEY     (T1)
              END-IF
           END-PERFORM

           SUBTRACT +2                 FROM T1
           MOVE T1                     TO T1M
           DISPLAY ' NUMBER OF  CON  RECORDS ' T1

           MOVE ' '                    TO WS-TABLE-EOF-SW
           MOVE +1                     TO T1

           .
       0300-EXIT.
           EXIT.

       0400-TEST-TABLE.

           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              T1 > T1M
              MOVE LOW-VALUES          TO AM-CONTROL-PRIMARY
              MOVE X'04'               TO AM-COMPANY-CD
              MOVE '9'                 TO AM-CARRIER
              MOVE '000000'            TO AM-GROUPING
              MOVE WS-TBL1-STATE (T1)  TO AM-STATE
              MOVE WS-TBL1-ACCT (T1)   TO AM-ACCOUNT
              MOVE WS-TBL1-EXP-DT (T1) TO AM-EXPIRATION-DT
              READ ERACCT
              IF ERACCT-FILE-STATUS = '00'
                 ADD 1                 TO WS-ERACCTS-FOUND
              ELSE
                 DISPLAY ' NO MATCHING ERACCT ' WS-TBL1-STATE (T1) ' '
                    WS-TBL1-ACCT (T1)
              END-IF
           END-PERFORM

           CLOSE ERACCT
           OPEN I-O ERACCT
           .
       0400-EXIT.
           EXIT.

       0500-PROCESS.

           PERFORM 1000-PROCESS        THRU 1000-EXIT

           PERFORM 1200-READNEXT-ERACCT
                                       THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE SPACES                 TO WS-RET-OPT
                                          WS-REWRITE-ERACCT-SW
           MOVE AM-STATE               TO WS-STATE
           MOVE AM-ACCOUNT             TO WS-ACCOUNT
           MOVE AM-EXPIRATION-DT       TO WS-EXP-DT

           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              T1 > T1M
              IF WS-AM-KEY = WS-TBL1-KEY (T1)
                 MOVE WS-TBL1-NEW (T1) TO WS-RET-OPT
                 PERFORM 1050-SEARCH   THRU 1050-EXIT
              END-IF
           END-PERFORM

           IF REWRITE-ERACCT
              PERFORM 2100-REWRITE-ERACCT
                                    THRU 2100-EXIT
pemtst        PERFORM 2400-BUILD-ERACNT
pemtst                              THRU 2400-EXIT
           END-IF
              
           .
       1000-EXIT.
           EXIT.

       1050-SEARCH.

            ADD 1                      TO WS-TABLE-HITS

           IF WS-TBL1-EFF-DT (T1) = AM-EFFECT-DT
              PERFORM VARYING A1 FROM +1 BY +1 UNTIL
                 (A1 > +10)
                 OR (AM-AGT (A1) = WS-TBL1-AGT-NO (T1))
              END-PERFORM
              IF A1 > +10
                 DISPLAY ' CANT FIND AGT FOR ACCT '
                    AM-STATE ' ' AM-ACCOUNT
              ELSE
                 IF WS-TBL1-OLD (T1) = AM-RETRO-LV-INDIC (A1)
                    PERFORM 1300-DISPLAY-ACCOUNT-INFO
                                    THRU 1300-EXIT
                    IF WS-RET-OPT = 'B'
                       MOVE ' '     TO WS-RET-OPT
                    END-IF
                    MOVE WS-RET-OPT TO AM-RETRO-LV-INDIC (A1)
                    ADD 1              TO WS-ERACCT-LVL-UPD
                    SET REWRITE-ERACCT TO TRUE
      *             PERFORM 2100-REWRITE-ERACCT
      *                             THRU 2100-EXIT
      *             PERFORM 2400-BUILD-ERACNT
      *                             THRU 2400-EXIT
                 ELSE
                    DISPLAY ' OLD OPT DOESNT MATCH '
                       AM-STATE ' ' AM-ACCOUNT
                 END-IF
              END-IF
           ELSE
              DISPLAY ' EFFECTIVE DATE DOESNT MATCH '
                 AM-STATE ' ' AM-ACCOUNT
           END-IF

           .
       1050-EXIT.
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

           DISPLAY ' ABOUT TO CHG RET OPT ON '
              AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT ' ' AM-AGT (A1)
                 ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
              ' FROM ' AM-RETRO-LV-INDIC (A1) ' TO ' WS-RET-OPT

           .
       1300-EXIT.
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
              STRING 'CHANGED RETRO OPT CODE TO ' WS-RET-OPT
                 DELIMITED BY SIZE INTO NT-NOTE-LINE
              END-STRING
      *       MOVE 'CHANGED RETRO TAX CODE TO WS-TAX-OPT '
      *                                TO NT-NOTE-LINE
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
                 DISPLAY 'ERACNT, BAD   WRITE '
                 DISPLAY '*** STATUS CODE IS ' ERACNT-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              END-IF
           END-IF

           .
       2450-EXIT.
           EXIT.

       3000-CLOSE-FILES.

           CLOSE ERACCT FILE-IN

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - CLOSE ' ERACCT-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-TABLE-HITS          TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT TABLE HITS           = ' WS-DISPLAY-CNT

           MOVE WS-ERACCTS-FOUND       TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT RECS FOUND           = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS READ     = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-RECS-FIX     TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER RECS FIXED    = ' WS-DISPLAY-CNT

           MOVE WS-ERACCT-LVL-UPD      TO WS-DISPLAY-CNT
           DISPLAY '***  ERACCT MASTER LVLS FIXED    = ' WS-DISPLAY-CNT

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
