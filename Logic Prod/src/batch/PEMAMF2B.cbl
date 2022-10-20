       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMAMF2B.
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
       77  WS-SEQ-NO                   PIC S9(4)   COMP VALUE +0.
       77  WS-DIS-EFF-DT               PIC X(10)  VALUE SPACES.
       77  WS-DIS-EXP-DT               PIC X(10)  VALUE SPACES.
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

       01  WS-HOLD-ERACNT              PIC X(120)  VALUE SPACES.
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

           IF AM-REPORT-CODE-1 = 'ANDERSEN' OR 'ASBURY' OR 'MARTIN'
              OR 'RB SMITH' OR 'RIES' OR 'SCHEINOST' OR 'WINKLER'
              PERFORM 1000-PROCESS     THRU 1000-EXIT
           END-IF
      *    IF AM-USER-SELECT-2 = 'JEFF C'
      *       PERFORM 1000-PROCESS     THRU 1000-EXIT
      *    END-IF

      *    IF AM-USER-SELECT-2 = 'ROB C'
      *       PERFORM 1000-PROCESS     THRU 1000-EXIT
      *    END-IF

      *    IF ((AM-STATE = 'NE')
      *       AND (AM-ACCOUNT = '00006436OB')
      *       AND (AM-EXPIRATION-DT = X'A261' OR X'FFFF'))
      *                       OR
      *       ((AM-STATE = 'WY')
      *       AND (AM-ACCOUNT = '0000796400')
      *       AND (AM-EXPIRATION-DT = X'9981' OR X'A269' OR X'FFFF'))
      *                       OR
      *       ((AM-STATE = 'WY')
      *       AND (AM-ACCOUNT = '0000796402')
      *       AND (AM-EXPIRATION-DT = X'A269' OR X'FFFF'))
      *                       OR
      *       ((AM-STATE = 'WY')
      *       AND (AM-ACCOUNT = '0001165600')
      *       AND (AM-EXPIRATION-DT = X'A281' OR X'FFFF'))
      *       PERFORM 1000-PROCESS     THRU 1000-EXIT
      *    END-IF


      *    IF (AM-EFFECTIVE-DT > X'9EFF')
      *       AND (AM-REI-TABLE = '00S')
      *       PERFORM 1000-PROCESS     THRU 1000-EXIT
      *    END-IF



      *       AND (AM-ACCOUNT = '0000948500' OR '0000948700' OR
      *         '0000948800' OR '0000906300' OR '0000915000' OR
      *         '0000933200')
      *       AND (AM-EXPIRATION-DT = X'A441')
      *       PERFORM 1000-PROCESS     THRU 1000-EXIT
      *    END-IF

      *    IF ((AM-STATE = 'MD')
      *       AND (AM-ACCOUNT = '0700000486' OR '0700000490')
      *       AND (AM-EXPIRATION-DT = X'A3E1'))
      *                     OR
      *       ((AM-STATE = 'CO')
      *       AND (AM-ACCOUNT = '0900508009' OR '0900508012')
      *       AND (AM-EXPIRATION-DT = X'A3C1'))
      *       PERFORM 1000-PROCESS     THRU 1000-EXIT
      *    END-IF

           PERFORM 1200-ERACCT-READ    THRU 1200-EXIT

           .
       0500-EXIT.
           EXIT.

       1000-PROCESS.

           PERFORM 1300-DISPLAY-ACCOUNT-INFO
                                       THRU 1300-EXIT
       
           EVALUATE AM-REPORT-CODE-1
              WHEN 'ANDERSEN'
                 DISPLAY ' ABOUT TO CHG RPT1 RPT2 US2 CODE ON '
                    AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
                    ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
                    ' FROM ' AM-REPORT-CODE-1 ' ' AM-REPORT-CODE-2 ' '
                    AM-USER-SELECT-2
                 MOVE 'SMRO'           TO AM-REPORT-CODE-1
                 MOVE 'ANDERSEN'       TO AM-REPORT-CODE-2
                 MOVE 'MARK L'         TO AM-USER-SELECT-2
                 DISPLAY ' TO ' AM-REPORT-CODE-1 ' '
                    AM-REPORT-CODE-2 ' ' AM-USER-SELECT-2
              WHEN 'ASBURY'
                 DISPLAY ' ABOUT TO CHG RPT1 RPT2 US2 CODE ON '
                    AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
                    ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
                    ' FROM ' AM-REPORT-CODE-1 ' ' AM-REPORT-CODE-2 ' '
                    AM-USER-SELECT-2
                 MOVE 'SMRO'           TO AM-REPORT-CODE-1
                 MOVE 'TASBURY'        TO AM-REPORT-CODE-2
                 MOVE 'MARK L'         TO AM-USER-SELECT-2
                 DISPLAY ' TO ' AM-REPORT-CODE-1 ' '
                    AM-REPORT-CODE-2 ' ' AM-USER-SELECT-2
              WHEN 'MARTIN'
                 DISPLAY ' ABOUT TO CHG RPT1 RPT2 US2 CODE ON '
                    AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
                    ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
                    ' FROM ' AM-REPORT-CODE-1 ' ' AM-REPORT-CODE-2 ' '
                    AM-USER-SELECT-2
                 MOVE 'GLRO'           TO AM-REPORT-CODE-1
                 MOVE 'MARTIN'         TO AM-REPORT-CODE-2
                 MOVE 'GARY W'         TO AM-USER-SELECT-2
                 DISPLAY ' TO ' AM-REPORT-CODE-1 ' '
                    AM-REPORT-CODE-2 ' ' AM-USER-SELECT-2
              WHEN 'RB SMITH'
                 DISPLAY ' ABOUT TO CHG RPT1 RPT2 US2 CODE ON '
                    AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
                    ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
                    ' FROM ' AM-REPORT-CODE-1 ' ' AM-REPORT-CODE-2 ' '
                    AM-USER-SELECT-2
                 MOVE 'SMRO'           TO AM-REPORT-CODE-1
                 MOVE 'RBSMITH'        TO AM-REPORT-CODE-2
                 MOVE 'CARLOS B'       TO AM-USER-SELECT-2
                 DISPLAY ' TO ' AM-REPORT-CODE-1 ' '
                    AM-REPORT-CODE-2 ' ' AM-USER-SELECT-2
              WHEN 'RIES'
                 DISPLAY ' ABOUT TO CHG RPT1 RPT2 US2 CODE ON '
                    AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
                    ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
                    ' FROM ' AM-REPORT-CODE-1 ' ' AM-REPORT-CODE-2 ' '
                    AM-USER-SELECT-2
                 MOVE 'WRO'            TO AM-REPORT-CODE-1
                 MOVE 'RIES'           TO AM-REPORT-CODE-2
                 MOVE 'MARIDYTH M'     TO AM-USER-SELECT-2
                 DISPLAY ' TO ' AM-REPORT-CODE-1 ' '
                    AM-REPORT-CODE-2 ' ' AM-USER-SELECT-2
              WHEN 'SCHEINOST'
                 DISPLAY ' ABOUT TO CHG RPT1 RPT2 US2 CODE ON '
                    AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
                    ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
                    ' FROM ' AM-REPORT-CODE-1 ' ' AM-REPORT-CODE-2 ' '
                    AM-USER-SELECT-2
                 MOVE 'JOHN B'         TO AM-USER-SELECT-2
                 DISPLAY ' TO ' AM-REPORT-CODE-1 ' '
                    AM-REPORT-CODE-2 ' ' AM-USER-SELECT-2
              WHEN 'WINKLER'
                 DISPLAY ' ABOUT TO CHG RPT1 RPT2 US2 CODE ON '
                    AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
                    ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
                    ' FROM ' AM-REPORT-CODE-1 ' ' AM-REPORT-CODE-2 ' '
                    AM-USER-SELECT-2
                 MOVE 'NCRO'           TO AM-REPORT-CODE-1
                 MOVE 'WINKLER'        TO AM-REPORT-CODE-2
                 MOVE 'ROB T'          TO AM-USER-SELECT-2
                 DISPLAY ' TO ' AM-REPORT-CODE-1 ' '
                    AM-REPORT-CODE-2 ' ' AM-USER-SELECT-2
           END-EVALUATE

      *    MOVE 'KELLY K'              TO AM-USER-SELECT-2

      *    EVALUATE TRUE
      *       WHEN (AM-STATE = 'NE')
      *          AND (AM-ACCOUNT = '00006436OB')
      *          AND (AM-EXPIRATION-DT = X'A261')
      *          MOVE ACCOUNT-MASTER   TO WS-HOLD-ERACCT
      *          MOVE 19980430         TO AM-EFFECT-DT
      *          MOVE X'937E'          TO AM-EFFECTIVE-DT
      *          MOVE '0V3'            TO AM-REI-TABLE
      *          MOVE 'Y'              TO AM-RECALC-REIN
      *          MOVE 'CENSTAT'        TO AM-REPORT-CODE-2
      *          MOVE 'PLTVAL'         TO AM-REI-GROUP-A
      *          MOVE 'CSRMNO'         TO AM-REI-GROUP-B
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO CHG REIN STUFF ON '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *            ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT ' '
      *            AM-REI-TABLE ' ' AM-REPORT-CODE-2 ' '
      *            AM-REI-GROUP-A ' ' AM-REI-GROUP-B
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *          MOVE WS-HOLD-ERACCT   TO ACCOUNT-MASTER
      *          MOVE X'937E'          TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO ADD NEW RANGE ON '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *            ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT ' '
      *          PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *
      *       WHEN (AM-STATE = 'NE')
      *          AND (AM-ACCOUNT = '00006436OB')
      *          AND (AM-EXPIRATION-DT = X'FFFF')
      *          MOVE '0V3'            TO AM-REI-TABLE
      *          MOVE 'Y'              TO AM-RECALC-REIN
      *          MOVE 'CENSTAT'        TO AM-REPORT-CODE-2
      *          MOVE 'PLTVAL'         TO AM-REI-GROUP-A
      *          MOVE 'CSRMNO'         TO AM-REI-GROUP-B
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO CHG REIN STUFF ON '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *            ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT ' '
      *            AM-REI-TABLE ' ' AM-REPORT-CODE-2 ' '
      *            AM-REI-GROUP-A ' ' AM-REI-GROUP-B
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *       WHEN (AM-STATE = 'WY')
      *          AND (AM-ACCOUNT = '0000796400')
      *          AND (AM-EXPIRATION-DT = X'9981')
      *          MOVE ACCOUNT-MASTER   TO WS-HOLD-ERACCT
      *          MOVE 20020101         TO AM-EFFECT-DT
      *          MOVE X'9901'          TO AM-EFFECTIVE-DT
      *          MOVE '0V3'            TO AM-REI-TABLE
      *          MOVE 'Y'              TO AM-RECALC-REIN
      *          MOVE 'CENSTAT'        TO AM-REPORT-CODE-2
      *          MOVE 'PLTVAL'         TO AM-REI-GROUP-A
      *          MOVE 'CSRMNO'         TO AM-REI-GROUP-B
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO CHG REIN STUFF ON '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *            ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT ' '
      *            AM-REI-TABLE ' ' AM-REPORT-CODE-2 ' '
      *            AM-REI-GROUP-A ' ' AM-REI-GROUP-B
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *          MOVE WS-HOLD-ERACCT   TO ACCOUNT-MASTER
      *          MOVE X'9901'          TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO ADD NEW RANGE ON '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *            ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT ' '
      *          PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *       WHEN (AM-STATE = 'WY')
      *          AND (AM-ACCOUNT = '0000796400')
      *          AND (AM-EXPIRATION-DT = X'FFFF' OR X'A269')
      *          MOVE '0V3'            TO AM-REI-TABLE
      *          MOVE 'Y'              TO AM-RECALC-REIN
      *          MOVE 'CENSTAT'        TO AM-REPORT-CODE-2
      *          MOVE 'PLTVAL'         TO AM-REI-GROUP-A
      *          MOVE 'CSRMNO'         TO AM-REI-GROUP-B
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO CHG REIN STUFF ON '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *            ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT ' '
      *            AM-REI-TABLE ' ' AM-REPORT-CODE-2 ' '
      *            AM-REI-GROUP-A ' ' AM-REI-GROUP-B
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *
      *       WHEN (AM-STATE = 'WY')
      *          AND (AM-ACCOUNT = '0000796402')
      *          AND (AM-EXPIRATION-DT = X'A269')
      *          MOVE ACCOUNT-MASTER   TO WS-HOLD-ERACCT
      *          MOVE 20020101         TO AM-EFFECT-DT
      *          MOVE X'9901'          TO AM-EFFECTIVE-DT
      *          MOVE '0V3'            TO AM-REI-TABLE
      *          MOVE 'Y'              TO AM-RECALC-REIN
      *          MOVE 'CENSTAT'        TO AM-REPORT-CODE-2
      *          MOVE 'PLTVAL'         TO AM-REI-GROUP-A
      *          MOVE 'CSRMNO'         TO AM-REI-GROUP-B
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO CHG REIN STUFF ON '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *            ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT ' '
      *            AM-REI-TABLE ' ' AM-REPORT-CODE-2 ' '
      *            AM-REI-GROUP-A ' ' AM-REI-GROUP-B
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *          MOVE WS-HOLD-ERACCT   TO ACCOUNT-MASTER
      *          MOVE X'9901'          TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO ADD NEW RANGE ON '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *            ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT ' '
      *          PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *       WHEN (AM-STATE = 'WY')
      *          AND (AM-ACCOUNT = '0000796402')
      *          AND (AM-EXPIRATION-DT = X'FFFF')
      *          MOVE '0V3'            TO AM-REI-TABLE
      *          MOVE 'Y'              TO AM-RECALC-REIN
      *          MOVE 'CENSTAT'        TO AM-REPORT-CODE-2
      *          MOVE 'PLTVAL'         TO AM-REI-GROUP-A
      *          MOVE 'CSRMNO'         TO AM-REI-GROUP-B
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO CHG REIN STUFF ON '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *            ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT ' '
      *            AM-REI-TABLE ' ' AM-REPORT-CODE-2 ' '
      *            AM-REI-GROUP-A ' ' AM-REI-GROUP-B
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *
      *       WHEN (AM-STATE = 'WY')
      *          AND (AM-ACCOUNT = '0001165600')
      *          AND (AM-EXPIRATION-DT = X'A281')
      *          MOVE ACCOUNT-MASTER   TO WS-HOLD-ERACCT
      *          MOVE 20080401         TO AM-EFFECT-DT
      *          MOVE X'A261'          TO AM-EFFECTIVE-DT
      *          MOVE '0V3'            TO AM-REI-TABLE
      *          MOVE 'Y'              TO AM-RECALC-REIN
      *          MOVE 'CENSTAT'        TO AM-REPORT-CODE-2
      *          MOVE 'PLTVAL'         TO AM-REI-GROUP-A
      *          MOVE 'CSRMNO'         TO AM-REI-GROUP-B
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO CHG REIN STUFF ON '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *            ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT ' '
      *            AM-REI-TABLE ' ' AM-REPORT-CODE-2 ' '
      *            AM-REI-GROUP-A ' ' AM-REI-GROUP-B
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *          MOVE WS-HOLD-ERACCT   TO ACCOUNT-MASTER
      *          MOVE X'A261'          TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO ADD NEW RANGE ON '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *            ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT ' '
      *          PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *       WHEN (AM-STATE = 'WY')
      *          AND (AM-ACCOUNT = '0001165600')
      *          AND (AM-EXPIRATION-DT = X'FFFF')
      *          MOVE '0V3'            TO AM-REI-TABLE
      *          MOVE 'Y'              TO AM-RECALC-REIN
      *          MOVE 'CENSTAT'        TO AM-REPORT-CODE-2
      *          MOVE 'PLTVAL'         TO AM-REI-GROUP-A
      *          MOVE 'CSRMNO'         TO AM-REI-GROUP-B
      *          PERFORM 1300-DISPLAY-ACCOUNT-INFO
      *                                THRU 1300-EXIT
      *          DISPLAY ' ABOUT TO CHG REIN STUFF ON '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *            ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT ' '
      *            AM-REI-TABLE ' ' AM-REPORT-CODE-2 ' '
      *            AM-REI-GROUP-A ' ' AM-REI-GROUP-B
      *          PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *       WHEN OTHER
      *          DISPLAY ' DID I MISS ONE '
      *            AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *    END-EVALUATE

      *    MOVE ACCOUNT-MASTER         TO WS-HOLD-ERACCT
      *    MOVE 20090101               TO AM-EFFECT-DT
      *    MOVE X'A381'                TO AM-EFFECTIVE-DT
      *    MOVE 'R08'                  TO AM-REI-TABLE
      *    MOVE 'Y'                    TO AM-RECALC-REIN
      *    PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *    MOVE WS-HOLD-ERACCT         TO ACCOUNT-MASTER
      *    MOVE X'A381'                TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *    PERFORM 2200-WRITE-ERACCT
      *                             THRU 2200-EXIT
      *    IF AM-STATE = 'MD'
      *       MOVE ACCOUNT-MASTER      TO WS-HOLD-ERACCT
      *       MOVE 20090301            TO AM-EFFECT-DT
      *       MOVE X'A3C1'             TO AM-EFFECTIVE-DT
      *       PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *       MOVE WS-HOLD-ERACCT      TO ACCOUNT-MASTER
      *       MOVE X'A3C1'             TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *       PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *    ELSE
      *    IF AM-STATE = 'CO'
      *       MOVE ACCOUNT-MASTER      TO WS-HOLD-ERACCT
      *       MOVE 20090101            TO AM-EFFECT-DT
      *                                   AM-LO-CERT-DATE
      *       MOVE X'A381'             TO AM-EFFECTIVE-DT
      *       PERFORM 2100-ERACCT-REWRITE
      *                                THRU 2100-EXIT
      *       MOVE WS-HOLD-ERACCT      TO ACCOUNT-MASTER
      *       MOVE X'A381'             TO AM-EXPIRATION-DT
      *                                   AM-VG-EXPIRATION-DT
      *       PERFORM 2200-WRITE-ERACCT
      *                                THRU 2200-EXIT
      *    END-IF

           PERFORM 2100-ERACCT-REWRITE
                                       THRU 2100-EXIT
       
           PERFORM 2500-GET-ERACNT-SEQ-NO
                                       THRU 2500-EXIT
           PERFORM 2400-BUILD-ERACNT
                                       THRU 2400-EXIT

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

      *    DISPLAY ' ABOUT TO CHG US2 CODE ON '
      *       AM-CARRIER ' ' AM-STATE ' ' AM-ACCOUNT
      *          ' ' WS-DIS-EFF-DT ' ' WS-DIS-EXP-DT
      *       ' FROM ' AM-USER-SELECT-2 ' TO KELLY K  '

           .
       1300-EXIT.
           EXIT.

       2100-ERACCT-REWRITE.

      *    DISPLAY ' ABOUT TO REWRITE ' AM-CARRIER ' ' AM-STATE
      *       ' ' AM-ACCOUNT ' ' AM-REI-TABLE

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

      *    DISPLAY 'ERACCT   WRITE ' AM-CONTROL-A
      *    DISPLAY ' '

PEMTST     MOVE '00'                   TO ERACCT-FILE-STATUS
PEMTST*    WRITE ACCOUNT-MASTER

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

PEMTST     MOVE '00'                   TO ERACCT-FILE-STATUS
PEMTST*    DELETE ERACCT

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
              MOVE WS-SEQ-NO           TO NT-LINE-SEQUENCE
              MOVE 'CONV'              TO NT-LAST-MAINT-BY
              MOVE WS-CURRENT-BIN-DATE TO NT-LAST-MAINT-DT
              MOVE +220000             TO NT-LAST-MAINT-HHMMSS
              EVALUATE TRUE

               WHEN AM-REPORT-CODE-2 = 'TASBURY'
                 MOVE '...TO TASBURY. *****GA WILL STILL SERVICE ACCOUNT
      -  '*****'
                                       TO NT-NOTE-LINE
                 MOVE NOTE-FILE        TO WS-HOLD-ERACNT
                 PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT
                 MOVE WS-HOLD-ERACNT   TO NOTE-FILE
                 IF NT-LINE-SEQUENCE > +2
                    SUBTRACT +1           FROM NT-LINE-SEQUENCE
                 ELSE
                    DISPLAY ' PROBLEM WITH ERACNT ' NT-STATE ' '
                      NT-ACCOUNT ' ' NT-LINE-SEQUENCE
                    PERFORM ABEND-PGM
                 END-IF
                 MOVE '...RC1 & 2 CHGD ACCORDINGLY. RPTCD ASBURY CHGED..
      -  '.'
                                       TO NT-NOTE-LINE
                 MOVE NOTE-FILE        TO WS-HOLD-ERACNT
                 PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT
                 MOVE WS-HOLD-ERACNT   TO NOTE-FILE
                 IF NT-LINE-SEQUENCE > +2
                    SUBTRACT +1           FROM NT-LINE-SEQUENCE
                 ELSE
                    DISPLAY ' PROBLEM WITH ERACNT ' NT-STATE ' '
                      NT-ACCOUNT ' ' NT-LINE-SEQUENCE
                    PERFORM ABEND-PGM
                 END-IF
                 MOVE 'EFF 1/1/2010-ACCT CHGD TO HAVE GA ROLL UP TO HO R
      -  'EGION...'
                                       TO NT-NOTE-LINE
                 MOVE NOTE-FILE        TO WS-HOLD-ERACNT
                 PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT

               WHEN AM-REPORT-CODE-2 = 'RBSMITH'
                 MOVE '...TO RBSMITH. *****GA WILL STILL SERVICE ACCOUNT
      -  '*****'
                                       TO NT-NOTE-LINE
                 MOVE NOTE-FILE        TO WS-HOLD-ERACNT
                 PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT
                 MOVE WS-HOLD-ERACNT   TO NOTE-FILE
                 IF NT-LINE-SEQUENCE > +2
                    SUBTRACT +1           FROM NT-LINE-SEQUENCE
                 ELSE
                    DISPLAY ' PROBLEM WITH ERACNT ' NT-STATE ' '
                      NT-ACCOUNT ' ' NT-LINE-SEQUENCE
                    PERFORM ABEND-PGM
                 END-IF
                 MOVE '...RC1 & 2 CHGD ACCORDINGLY. RPTCD RB SMITH CHGED
      -  '...'
                                       TO NT-NOTE-LINE
                 MOVE NOTE-FILE        TO WS-HOLD-ERACNT
                 PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT
                 MOVE WS-HOLD-ERACNT   TO NOTE-FILE
                 IF NT-LINE-SEQUENCE > +2
                    SUBTRACT +1           FROM NT-LINE-SEQUENCE
                 ELSE
                    DISPLAY ' PROBLEM WITH ERACNT ' NT-STATE ' '
                      NT-ACCOUNT ' ' NT-LINE-SEQUENCE
                    PERFORM ABEND-PGM
                 END-IF
                 MOVE 'EFF 1/1/2010-ACCT CHGD TO HAVE GA ROLL UP TO HO R
      -  'EGION...'
                                       TO NT-NOTE-LINE
                 MOVE NOTE-FILE        TO WS-HOLD-ERACNT
                 PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT

               WHEN AM-REPORT-CODE-1 = 'SCHEINOST'
                 MOVE '...JOHN B TO US2.   *****GA WILL STILL SERVICE AC
      -  'COUNT*****'
                                       TO NT-NOTE-LINE
                 MOVE NOTE-FILE        TO WS-HOLD-ERACNT
                 PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT
                 MOVE WS-HOLD-ERACNT   TO NOTE-FILE
                 IF NT-LINE-SEQUENCE > +2
                    SUBTRACT +1        FROM NT-LINE-SEQUENCE
                 ELSE
                    DISPLAY ' PROBLEM WITH ERACNT ' NT-STATE ' '
                      NT-ACCOUNT ' ' NT-LINE-SEQUENCE
                    PERFORM ABEND-PGM
                 END-IF
                 MOVE 'EFF 1/1/2010-SCHEINOST WILL REPORT TO JOHN B.  AD
      -  'DED ...'
                                       TO NT-NOTE-LINE
                 MOVE NOTE-FILE        TO WS-HOLD-ERACNT
                 PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT
               WHEN OTHER
                 MOVE '...RC1 & 2 CHGD ACCORDINGLY ***GA WILL STILL SERV
      -  'ICE ACCT**'
                                       TO NT-NOTE-LINE
                 MOVE NOTE-FILE        TO WS-HOLD-ERACNT
                 PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT
                 MOVE WS-HOLD-ERACNT   TO NOTE-FILE
                 IF NT-LINE-SEQUENCE > +2
                    SUBTRACT +1        FROM NT-LINE-SEQUENCE
                 ELSE
                    DISPLAY ' PROBLEM WITH ERACNT ' NT-STATE ' '
                      NT-ACCOUNT ' ' NT-LINE-SEQUENCE
                    PERFORM ABEND-PGM
                 END-IF
                 MOVE 'EFF 1/1/2010-ACCT CHGD TO HAVE GA ROLL UP TO HO R
      -  'EGION...'
                                       TO NT-NOTE-LINE
                 MOVE NOTE-FILE        TO WS-HOLD-ERACNT
                 PERFORM 2450-WRITE-ERACNT
                                       THRU 2450-EXIT
              END-EVALUATE
           END-IF

           .
       2400-EXIT.
           EXIT.

       2450-WRITE-ERACNT.

PEMTST     WRITE NOTE-FILE     
PEMTST*    MOVE '00'                   TO ERACNT-FILE-STATUS

           IF ERACNT-FILE-STATUS = '00'
              ADD 1                    TO WS-ERACNT-RECS-ADD
           ELSE
              IF ERACNT-FILE-STATUS = '22'
                 DISPLAY ' PROBLEM WITH WRITE ' NT-STATE ' ' NT-ACCOUNT
                 PERFORM ABEND-PGM
                 ADD      +1           TO NT-LINE-SEQUENCE
              ELSE
                 DISPLAY 'ERROR - ERACNT - WRITE ' ERACNT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       2450-EXIT.
           EXIT.

       2500-GET-ERACNT-SEQ-NO.

           IF WS-PREV-ERACNT-KEY = AM-CNTRL-1
              GO TO 2500-EXIT
           END-IF

           MOVE +4096                  TO WS-SEQ-NO
           PERFORM 2510-START-ERACNT   THRU 2510-EXIT
           PERFORM 2520-READNEXT-ERACNT
                                       THRU 2520-EXIT
           IF (NT-CONTROL-PRIMARY (1:20) = AM-CONTROL-PRIMARY (1:20))
              AND (NT-RECORD-TYPE = '1')
              IF NT-LINE-SEQUENCE > +2
                 COMPUTE WS-SEQ-NO = NT-LINE-SEQUENCE - +1
              ELSE
                 DISPLAY ' SEQUENCE NO PROBLEM ' AM-CARRIER ' '
                    AM-STATE ' ' AM-ACCOUNT
                 PERFORM ABEND-PGM
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
              PERFORM ABEND-PGM
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
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       2520-EXIT.
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
