       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPSRTU1.
       AUTHOR.     PABLO
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RATE-FILE ASSIGN TO SYS010
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE ASSIGN TO SYS019.

           SELECT ERRATE    ASSIGN ERRATE
                            ORGANIZATION IS INDEXED
                            ACCESS IS DYNAMIC
                            RECORD KEY IS RT-CONTROL-PRIMARY
                            FILE STATUS IS ERRATE-FILE-STATUS.
           SELECT PRINTER   ASSIGN TO SYS008.


           EJECT
       DATA DIVISION.
      /
       FILE SECTION.

       FD  RATE-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 1552 CHARACTERS
           LABEL RECORDS ARE OMITTED.

       01  RATE-RECORD-IN.
           05  RR-STATE                PIC XX.
           05  RR-CLASS                PIC XX.
           05  RR-DEVIATION            PIC XXX.
           05  RR-TYPE                 PIC X.
           05  RR-BEN-CODE             PIC XX.
           05  RR-AGE                  PIC XX.
           05  RR-AMT                  PIC 999999.
           05  RR-EFF-DT               PIC X(08).
           05  RR-ATT-AGE              PIC 99.
           05  RR-MORT                 PIC X(4).
           05  FILLER OCCURS 3.
               10  RR-TAGE             PIC 99.
               10  RR-TTERM            PIC 999.
               10  RR-TBEN             PIC 9(5).
               10  RR-TAMT             PIC 9(7).
           05  RR-COMMENTS             PIC X(50).
           05  RATE-VALUES OCCURS 180.
               10  RR-RATES            PIC 99V9(05).

       FD  ERRATE
           LABEL RECORDS ARE STANDARD.
                                       COPY ERCRATE.
           EJECT

       FD  DISK-DATE
                                       COPY ELCDTEFD.
       EJECT
       FD  PRINTER
                                       COPY ELCPRTFD.
           EJECT
       WORKING-STORAGE SECTION.

       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CPSRTU1 WORKING-STORAGE      '.
       77  FILLER  PIC X(32) VALUE '************ VM 2.001 **********'.

       01  WORK-AREAS.
           12  WS-RETURN-CODE          PIC XXXX.
           12  WS-NUM-DATE             PIC 9(11).
           12  WS-WORK-DATE REDEFINES WS-NUM-DATE.
               16  FILLER              PIC XXX.
               16  WS-WORK-CCYY        PIC 9999.
               16  WS-WORK-MM          PIC 99.
               16  WS-WORK-DD          PIC 99.
           12  WS-CARD-SW              PIC X VALUE SPACES.
               88  NO-MORE-RATES            VALUE 'Y'.
           12  SUB1                   PIC S9(3) VALUE +0 COMP-3.
           12  WS-LINE-COUNT          PIC S9(3)  VALUE +99 COMP-3.
           12  WS-LINE-COUNT-MAX      PIC S9(3)  VALUE +60 COMP-3.
           12  WS-PAGE                PIC S9(3)  VALUE +1  COMP-3.
           12  WS-CURRENT-BIN-DT      PIC XX.
           12  WS-ABEND-MESSAGE       PIC X(80).
           12  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.
           12  WS-ZERO                PIC S9  VALUE ZERO COMP-3.
           12  ERRATE-FILE-STATUS     PIC XX  VALUE ZEROS.
           12  RTE REDEFINES ERRATE-FILE-STATUS.
               16  RTE1               PIC X.
               16  RTE2               PIC X.

           12  ERROR-SW               PIC X    VALUE SPACE.
               88  ERROR-OCCURRED              VALUE 'E'.
               88  NO-ERRORS                   VALUE ' '.
           12  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.

       01  WS-HOLD-ERRATE  PIC X(1765) VALUE SPACES.
       01  DTE-INTERFACE-CODES.
           05  X                 PIC X           VALUE SPACE.
           05  PGM-SUB           PIC S9(4)  COMP VALUE +504.
           05  ABEND-CODE        PIC 9999        VALUE ZERO.
           05  ABEND-OPTION      PIC X           VALUE SPACE.
           05  OLC-REPORT-NAME   PIC X(6)        VALUE 'EL504'.

           05 WS-IN              PIC 9(5) VALUE ZEROS.
           05 WS-OUT             PIC 9(5) VALUE ZEROS.

       01  WS-PRINT-LINES.

           12  HDR-1.
               16  FILLER          PIC X(45)       VALUE SPACES.
               16  H1-COMPANY-ID   PIC XXX         VALUE SPACES.
               16  FILLER          PIC X(72)       VALUE
                  '    RATE FILE UPDATE REPORT   '.
               16  FILLER          PIC X(6)        VALUE 'CPSRTU'.
               16  H1-SUFFIX       PIC X           VALUE 'A'.
           12  HDR-2.
               16  FILLER          PIC X(47)       VALUE SPACES.
               16  H2-COMP         PIC X(30).
               16  FILLER          PIC X(43)       VALUE SPACES.
               16  H2-DATE         PIC X(8).
           12  HDR-3.
               16  FILLER          PIC X(54)       VALUE SPACES.
               16  H3-DATE         PIC X(18).
               16  FILLER          PIC X(48)       VALUE SPACES.
               16  FILLER          PIC X(5)        VALUE 'PAGE '.
               16  H3-PAGE         PIC ZZ,ZZ9.
           12  HDR-4.
               16  FILLER          PIC XX          VALUE SPACES.
               16  FILLER          PIC X(52)       VALUE
               'STATE   CLASS   DEV   BEN CODE   EFF DATE   COMMENTS'.

           12  DTL-1.
               16  FILLER              PIC XXX   VALUE SPACES.
               16  DTL-STATE           PIC X(08) VALUE SPACES.
               16  DTL-CLASS           PIC X(07) VALUE SPACES.
               16  DTL-DEV             PIC X(09) VALUE SPACES.
               16  DTL-BEN-CD          PIC X(07) VALUE SPACES.
               16  DTL-EFF-DT          PIC X(13) VALUE SPACES.
               16  DTL-COMMENTS        PIC X(30) VALUE SPACES.
       01  COMP-3-WORK-AREA.
           05  K1                 PIC S9(7)  VALUE +1.
           05  K2                 PIC S9(7)  VALUE +2.
           05  WS-RATE-RECS       PIC S9(7)  VALUE +0.
           05  RECORD-COUNT       PIC S9(7)  VALUE +0.
           05  LINE-CNT           PIC S9(7)  VALUE +0.
           05  DELETE-COUNT       PIC S9(7)  VALUE +0.
           05  DATE-ERROR-COUNT   PIC S9(7)  VALUE +0.

           EJECT
                                   COPY ELCDTECX.
       EJECT
                                   COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0100-INITIALIZE     THRU 0100-EXIT

           PERFORM 0300-BUILD-RATES    THRU 0300-EXIT UNTIL
              NO-MORE-RATES

           PERFORM 0600-FINALIZE       THRU 0600-EXIT

           GOBACK

           .

       0100-INITIALIZE.

           MOVE WS-CURRENT-DATE        TO H2-DATE
           MOVE COMPANY-NAME           TO H2-COMP
           MOVE ALPH-DATE              TO H3-DATE
           MOVE ZEROS                  TO H3-PAGE
           MOVE 70                     TO LINE-CNT

           OPEN INPUT RATE-FILE
              OUTPUT PRINTER

           OPEN I-O   ERRATE

           IF ERRATE-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              MOVE ERRATE-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              MOVE ' RATE MASTER OPEN ERROR- '
                                       TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           PERFORM 0200-READ-RATES     THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.
           EJECT

       0200-READ-RATES.

           READ RATE-FILE AT END
               SET NO-MORE-RATES TO TRUE
           END-READ

           IF NOT NO-MORE-RATES
              MOVE FUNCTION UPPER-CASE(RR-STATE)
                                       TO RR-STATE
              MOVE FUNCTION UPPER-CASE(RR-BEN-CODE)
                                       TO RR-BEN-CODE
              MOVE FUNCTION UPPER-CASE(RR-MORT)
                                       TO RR-MORT
              MOVE FUNCTION UPPER-CASE(RR-COMMENTS)
                                       TO RR-COMMENTS
              ADD +1                   TO WS-RATE-RECS
           END-IF

           .
       0200-EXIT.
           EXIT.
           EJECT

       0300-BUILD-RATES.

           MOVE LOW-VALUES             TO RT-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO RT-COMPANY-CD
           MOVE RR-STATE               TO RT-ST-CODE
           MOVE RR-CLASS               TO RT-ST-CLASS
           MOVE RR-DEVIATION           TO RT-ST-DEV
           MOVE RR-TYPE                TO RT-L-AH
           MOVE RR-BEN-CODE            TO RT-LAH-NUM
           MOVE RR-AGE                 TO RT-HIGH-AGE
           MOVE '9'                    TO RT-SEX
           MOVE '99'                   TO RT-FUTURE
           MOVE RR-AMT                 TO RT-HIGH-AMT
           MOVE 99999999999            TO RT-EXPIRY-DATE

           PERFORM 0500-BUILD-PRT-LINE THRU 0500-EXIT

           MOVE RR-EFF-DT              TO WS-WORK-DATE (4:8)
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO DTL-EFF-DT
           END-STRING
           MOVE ' INPUT RATE IMAGE '   TO DTL-COMMENTS
           PERFORM 0700-WRITE-PRT-LINE THRU 0700-EXIT
           IF (RR-STATE = SPACES)
              OR (RR-CLASS = SPACES)
              OR (RR-DEVIATION = SPACES)
              OR (RR-BEN-CODE = SPACES)
              OR (RR-TYPE NOT = 'A' AND 'L')
              MOVE ' INPUT IMAGE IN ERROR RATE NOT ADDED '
                       TO DTL-COMMENTS
              PERFORM 0700-WRITE-PRT-LINE THRU 0700-EXIT
           ELSE
              READ ERRATE
              IF ERRATE-FILE-STATUS = '10' OR '23'
                 PERFORM 0400-ADD-ERRATE  THRU 0400-EXIT
              ELSE
                 IF ERRATE-FILE-STATUS NOT = '00'
                    DISPLAY 'STATUS, ERRATE, READ ' ERRATE-FILE-STATUS
                    PERFORM ABEND-PGM
                 ELSE
                    MOVE RATE-RECORD   TO WS-HOLD-ERRATE
                    PERFORM 0310-BUILD-NEW
                                       THRU 0310-EXIT
                    PERFORM 0320-EXPIRE-OLD
                                       THRU 0320-EXIT
                 END-IF
              END-IF
              ADD 1                    TO RECORD-COUNT
           end-if
           PERFORM 0200-READ-RATES     THRU 0200-EXIT

           .
       0300-EXIT.
           EXIT.

       0310-BUILD-NEW.

           MOVE RR-ATT-AGE             TO RT-MAX-AGE
           MOVE RR-COMMENTS            TO RT-RATE-COMMENT
           MOVE BIN-RUN-DATE           TO RT-LAST-MAINT-DT
           MOVE 'E504'                 TO RT-LAST-MAINT-USER
           MOVE +190000                TO RT-LAST-MAINT-HHMMSS
           MOVE 'UPLOADED RATES'       TO RT-STRUCTURE-COMMENT
           IF RR-TYPE = 'L'
              MOVE RR-MORT             TO RT-LIFE-MORT-CODE
           END-IF
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              (SUB1 > +3)
              IF RR-TYPE = 'L'
                 MOVE RR-TAGE  (SUB1)  TO RT-L-EX-AGE  (SUB1)
                 MOVE RR-TTERM (SUB1)  TO RT-L-EX-TERM (SUB1)
                 MOVE RR-TAMT  (SUB1)  TO RT-L-EX-FACE (SUB1)
              ELSE
                 MOVE RR-TAGE  (SUB1)  TO RT-AH-AGE   (SUB1)
                 MOVE RR-TTERM (SUB1)  TO RT-AH-TERM  (SUB1)
                 MOVE RR-TBEN  (SUB1)  TO RT-AH-BEN-M (SUB1)
                 MOVE RR-TAMT  (SUB1)  TO RT-AH-BEN-F (SUB1)
              END-IF
           END-PERFORM
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL SUB1 > +180
              IF RR-RATES (SUB1) NOT NUMERIC
      *          DISPLAY ' RATE NOT NUMERIC  ' RR-RATES (SUB1)
                 MOVE ZEROS            TO RR-RATES (SUB1)
              END-IF
              MOVE RR-RATES (SUB1)     TO RT-AH-RATE (SUB1)
           END-PERFORM
           PERFORM 0500-BUILD-PRT-LINE THRU 0500-EXIT

           MOVE RT-EXPIRY-DATE         TO WS-NUM-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO DTL-EFF-DT
           END-STRING
           REWRITE RATE-RECORD
           IF ERRATE-FILE-STATUS = '00'
              MOVE ' NEW RATES APPLIED   '
                                       TO DTL-COMMENTS
           ELSE
              DISPLAY 'STATUS, ERRATE, REWRITE ' ERRATE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
           PERFORM 0700-WRITE-PRT-LINE THRU 0700-EXIT

           .
       0310-EXIT.
           EXIT.
       0320-EXPIRE-OLD.

           MOVE WS-HOLD-ERRATE         TO RATE-RECORD
           MOVE RR-EFF-DT              TO RT-EXPIRY-DATE
           PERFORM 0500-BUILD-PRT-LINE THRU 0500-EXIT

           MOVE RT-EXPIRY-DATE         TO WS-NUM-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO DTL-EFF-DT
           END-STRING
           WRITE RATE-RECORD
           IF ERRATE-FILE-STATUS = '22'
              MOVE ' DUPLICATE RECORD FOUND '
                                       TO DTL-COMMENTS
           ELSE
              IF ERRATE-FILE-STATUS = '00'
                 MOVE ' RATE RECORD EXPIRED '
                                       TO DTL-COMMENTS
              ELSE
                 DISPLAY 'STATUS, ERRATE,  1WRITE ' ERRATE-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           PERFORM 0700-WRITE-PRT-LINE THRU 0700-EXIT
           .
       0320-EXIT.
           EXIT.

       0400-ADD-ERRATE.

           MOVE SPACES                 TO RATE-RECORD
           MOVE LOW-VALUES             TO RT-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO RT-COMPANY-CD
           MOVE RR-STATE               TO RT-ST-CODE
           MOVE RR-CLASS               TO RT-ST-CLASS
           MOVE RR-DEVIATION           TO RT-ST-DEV
           MOVE RR-TYPE                TO RT-L-AH
           MOVE RR-BEN-CODE            TO RT-LAH-NUM
           MOVE RR-AGE                 TO RT-HIGH-AGE
           MOVE '9'                    TO RT-SEX
           MOVE '99'                   TO RT-FUTURE
           MOVE RR-AMT                 TO RT-HIGH-AMT
           MOVE 99999999999            TO RT-EXPIRY-DATE

           MOVE BIN-RUN-DATE           TO RT-LAST-MAINT-DT
           MOVE 'E504'                 TO RT-LAST-MAINT-USER
           MOVE +190000                TO RT-LAST-MAINT-HHMMSS
           MOVE 'UPLOADED RATES'       TO RT-STRUCTURE-COMMENT
           IF RT-L-AH = 'A'
              INITIALIZE RT-AH-LIMS-FLDS
              INITIALIZE RT-AH-RATES
           ELSE
              INITIALIZE RT-LIFE-LIMS-FLDS
              INITIALIZE RT-LIFE-RATES
           END-IF

           MOVE +0                     TO RT-DAILY-RATE
                                          RT-DISCOUNT-RATE
                                          RT-DISCOUNT-OB-RATE
                                          RT-COMPOSITE-RATE
                                          RT-POLICY-FEE
                                          RTC-4
           MOVE RR-ATT-AGE             TO RT-MAX-AGE
           MOVE RR-COMMENTS            TO RT-RATE-COMMENT
           IF RR-TYPE = 'L'
              MOVE RR-MORT             TO RT-LIFE-MORT-CODE
           END-IF
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL
              (SUB1 > +3)
              IF RR-TYPE = 'L'
                 MOVE RR-TAGE  (SUB1)  TO RT-L-EX-AGE  (SUB1)
                 MOVE RR-TTERM (SUB1)  TO RT-L-EX-TERM (SUB1)
                 MOVE RR-TAMT  (SUB1)  TO RT-L-EX-FACE (SUB1)
              ELSE
                 MOVE RR-TAGE  (SUB1)  TO RT-AH-AGE   (SUB1)
                 MOVE RR-TTERM (SUB1)  TO RT-AH-TERM  (SUB1)
                 MOVE RR-TBEN  (SUB1)  TO RT-AH-BEN-M (SUB1)
                 MOVE RR-TAMT  (SUB1)  TO RT-AH-BEN-F (SUB1)
              END-IF
           END-PERFORM
           PERFORM VARYING SUB1 FROM +1 BY +1 UNTIL SUB1 > +180
              IF RR-RATES (SUB1) NOT NUMERIC
      *          DISPLAY ' ADD RATE NOT NUMERIC  ' RR-RATES (SUB1)
                 MOVE ZEROS            TO RR-RATES (SUB1)
              END-IF
              MOVE RR-RATES (SUB1)      TO RT-AH-RATE (SUB1)
           END-PERFORM

           PERFORM 0500-BUILD-PRT-LINE THRU 0500-EXIT
           MOVE RT-EXPIRY-DATE         TO WS-NUM-DATE
           STRING WS-WORK-MM '/' WS-WORK-DD '/' WS-WORK-CCYY
              DELIMITED BY SIZE INTO DTL-EFF-DT
           END-STRING

           WRITE RATE-RECORD
           IF ERRATE-FILE-STATUS = '22'
              MOVE ' DUPLICATE RECORD FOUND '
                                       TO DTL-COMMENTS
           ELSE
              IF ERRATE-FILE-STATUS = '00'

                 MOVE ' RATE RECORD   ADDED '
                                       TO DTL-COMMENTS
              ELSE
                 DISPLAY 'STATUS, ERRATE,  2WRITE ' ERRATE-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           PERFORM 0700-WRITE-PRT-LINE THRU 0700-EXIT

           .
       0400-EXIT.
           EXIT.

       0500-BUILD-PRT-LINE.

           MOVE RT-ST-CODE             TO DTL-STATE
           MOVE RT-ST-CLASS            TO DTL-CLASS
           MOVE RT-ST-DEV              TO DTL-DEV
           MOVE RT-LAH-NUM             TO DTL-BEN-CD
           MOVE SPACES                 TO DTL-EFF-DT

           .
       0500-EXIT.
           EXIT.

       0600-FINALIZE.

           CLOSE RATE-FILE
                 ERRATE
                 PRINTER

           .
       0600-EXIT.
           EXIT.
           EJECT

       0700-WRITE-PRT-LINE.

           IF LINE-CNT > +60
              PERFORM 0800-WRITE-HDR-LINES
                                       THRU 0800-EXIT
           END-IF

           MOVE DTL-1                  TO P-DATA
           MOVE ' '                    TO P-CTL
           WRITE PRT AFTER ADVANCING 1 LINES
           ADD +1                      TO LINE-CNT

           .
       0700-EXIT.
           EXIT.
           EJECT

       0800-WRITE-HDR-LINES.

           MOVE DTE-CLIENT             TO H1-COMPANY-ID
           MOVE HDR-1                  TO P-DATA
           MOVE '1'                    TO P-CTL
           WRITE PRT AFTER ADVANCING PAGE
           MOVE HDR-2                  TO P-DATA
           MOVE ' '                    TO P-CTL
           WRITE PRT AFTER ADVANCING 1 LINE
           MOVE HDR-3                  TO P-DATA
           MOVE ' '                    TO P-CTL
           WRITE PRT AFTER ADVANCING 1 LINE
           MOVE HDR-4                  TO P-DATA
           MOVE '0'                    TO P-CTL
           WRITE PRT AFTER ADVANCING 2 LINES
           MOVE ' '                    TO P-DATA
           MOVE ' '                    TO P-CTL
           WRITE PRT AFTER ADVANCING 1 LINES

           MOVE +6                     TO LINE-CNT

           .
       0800-EXIT.
           EXIT.
           EJECT


       ABEND-PGM  SECTION.  COPY ELCABEND  SUPPRESS.

