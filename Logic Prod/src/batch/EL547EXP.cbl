       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL547EXP.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.
      *    THIS PROGRAM READS THE MONTH END CERT FILE
      *    AND BUILDS AN EXTRACT BY VALUATION MONTH AND YEAR.  
      *    IT PROVIDES NEW ISSUES, CANCELLED, CLAIMED AND EXPIRED
      *    COUNTS. ALSO, TRIES TO DETERMINE HOW MANY VALID MAIL
      *    RECORDS EXIST FOR THE EXPIRED CERTS. THIS EXTRACT STARTS 
      *    WITH CERTS EFFECTIVE AFTER 12/31/2001.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERTS                ASSIGN TO SYS010.
           SELECT EXTRACT              ASSIGN TO SYS011
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DISK-DATE            ASSIGN TO SYS019.
           SELECT PRINTX               ASSIGN TO SYS008.
           SELECT ERMAIL       ASSIGN TO ERMAIL 
                               ACCESS IS DYNAMIC   
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERMAIL-FILE-STATUS
                               RECORD KEY IS MA-CONTROL-PRIMARY.

       DATA DIVISION.
       FILE SECTION.

       FD  CERTS
                                       COPY ECSCRIFD.
                                       COPY ECSCRT01.

           EJECT
       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  EXTRACT-RECORD-OUT          PIC X(125).
       01  EXTRACT-RECORD-OUT-HD       PIC X(105).


       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  PRINTX
                                       COPY ELCPRTFD.

       FD  ERMAIL.
                                       COPY ERCMAIL.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '    EL547EXP  WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
           88  THERE-ARE-MORE-RECORDS     VALUE 'N'.
       77  VYR                         PIC S999   COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  WS-CRT-CNT                  PIC S9(9)  COMP-3 VALUE +0.
       77  WS-CRT-CNT-TOT              PIC S9(9)  COMP-3 VALUE +0.
       77  INTERMED                    PIC S9(9)V9(6)  COMP-3.
       77  ERMAIL-FILE-STATUS          PIC XX   VALUE LOW-VALUES.
       77  WS-MAIL-NO-FIND             PIC 9(9) VALUE ZEROS.
       01  WS-INIT-EXT-RECORD          PIC X(125)  VALUE LOW-VALUES.

       01  WS-EXTRACT-RECORD.
           05  EXT-VALUE-CCYYMM        PIC 9(6).
           05  WS-TAB1                 PIC X.
           05  EXT-ISS-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-TAB2                 PIC X.
           05  EXT-CNC-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-TAB3                 PIC X.
           05  EXT-CLM-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  WS-TAB4                 PIC X.
           05  EXT-EXP-CNT-A           PIC ZZZ,ZZZ,ZZ9.
           05  WS-TAB5                 PIC X.
           05  EXT-EXP-CNT-B           PIC ZZZ,ZZZ,ZZ9.
           05  WS-TAB6                 PIC X.
           05  EXT-VMAIL-CNT           PIC ZZZ,ZZZ,ZZ9.

       01  WS-HEAD-RECORD.
           05  FILLER                  PIC X(8)    VALUE 'VAL DATE'.
           05  WS-HTAB1                PIC X.
           05  FILLER                  PIC X(10)   VALUE 'ISSUE CNT'.
           05  WS-HTAB2                PIC X.
           05  FILLER                  PIC X(10)   VALUE 'CANCEL CNT'.
           05  WS-HTAB3                PIC X.
           05  FILLER                  PIC X(10)   VALUE 'CLAIM CNT'.
           05  WS-HTAB4                PIC X.
           05  FILLER                  PIC X(12)   VALUE 'AUTO EXP CNT'.
           05  WS-HTAB5                PIC X.
           05  FILLER                  PIC X(12)   VALUE 'BANK EXP CNT'.
           05  WS-HTAB6                PIC X.
           05  FILLER                  PIC X(17)   VALUE
               'VALID ADDRESS CNT'.

       01  WS-ISSUE-YR-TABLE.
           10  WS-VALUATION OCCURS 600.
               12  WS-RECORD.
                   15  WS-VALUATION-CCYYMM PIC 9(6).
                   15  WS-ISS-CNT      PIC S9(9)    COMP-3.
                   15  WS-CNC-CNT      PIC S9(9)    COMP-3.
                   15  WS-CLM-CNT      PIC S9(9)    COMP-3.
                   15  WS-EXP-CNT-A    PIC S9(9)    COMP-3.
                   15  WS-EXP-CNT-B    PIC S9(9)    COMP-3.
                   15  WS-VMAIL-CNT    PIC S9(9)    COMP-3.

       01  FILLER                      PIC X(400)  VALUE LOW-VALUES.

       01  WS-DISPLAY-DATE         PIC ZZZ9(8).
       01  WS-DISPLAY-TERM         PIC Z99.
       01  WS-DISPLAY-RTERM        PIC Z99.99.
       01  WS-DISPLAY-AMT          PIC ZZZZZZZ.99.
       01  WS-DISPLAY-PRM          PIC ZZZZZZZ.99.
       01  WS-BIN-CR-DT            PIC XX  VALUE LOW-VALUES.
       01  WS-BIN-LF-END-DATE      PIC XX  VALUE LOW-VALUES.
       01  WS-BIN-AH-END-DATE      PIC XX  VALUE LOW-VALUES.
       01  FILLER. 
           05  WS-END-YEAR             PIC 9(11)  VALUE ZEROS.
           05  FILLER REDEFINES WS-END-YEAR.
               10  FILLER              PIC 999.
               10  WS-CCYYMM           PIC 9(6).
               10  FILLER REDEFINES WS-CCYYMM.
                   15  WS-CCYY         PIC 9999.
                   15  WS-MM           PIC 99.
               10  WS-DD               PIC 99.
       01  WS-DEBUG-AREA.
           12  WS-DEBUG-SW             PIC X(01) VALUE ' '.
               88  DEBUG-IS-ON                VALUES '1' '2' '3' '4'.
               88  DEBUG-LF-INFORCE-CNT       VALUE '1'.
               88  DEBUG-AH-INFORCE-CNT       VALUE '2'.
               88  DEBUG-LF-STATUTORY         VALUE '3'.
               88  DEBUG-AH-STATUTORY         VALUE '4'.

           EJECT

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WORK-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.

       01  FILLER.
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYYMM      PIC 9(6).
               10  FILLER REDEFINES WS-WORK-CCYYMM.
                   15  WS-WORK-CCYY    PIC 9999.
                   15  WS-WORK-MM      PIC 99.
               10  WS-WORK-DD          PIC 99.
       01  FILLER.
           05  WS-EFFECT-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-EFFECT-DATE.
               10  FILLER              PIC 999.
               10  WS-EFFECT-CCYYMM     PIC 9(6).
               10  WS-EFFECT-DD        PIC 99.
       01  FILLER.
           05  WS-ENTRY-DATE           PIC 9(11).
           05  FILLER REDEFINES        WS-ENTRY-DATE.
               10  FILLER              PIC 999.
               10  WS-ENTRY-CCYYMM     PIC 9(6).
               10  WS-ENTRY-DD         PIC 99.
       01  FILLER.
           05  WS-CANCEL-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-CANCEL-DATE.
               10  FILLER              PIC 999.
               10  WS-CANCEL-CCYYMM    PIC 9(6).
               10  WS-CANCEL-DD        PIC 99.
       01  FILLER.
           05  WS-CLAIM-DATE           PIC 9(11).
           05  FILLER REDEFINES WS-CLAIM-DATE.
               10  FILLER              PIC 999.
               10  WS-CLAIM-CCYYMM     PIC 9(6).
               10  WS-CLAIM-DD         PIC 99.
       01  FILLER.
           05  WS-EXPIRE-DATE          PIC 9(11).
           05  FILLER REDEFINES        WS-EXPIRE-DATE.
               10  FILLER              PIC 999.
               10  WS-EXPIRE-CCYYMM    PIC 9(6).
               10  WS-EXPIRE-DD        PIC 99.
       01  FILLER.
           05  WS-HI-ISSUE-CCYYMM      PIC 9(6) VALUE ZEROS.
           05  WS-LO-ISSUE-CCYYMM      PIC 9(6) VALUE ZEROS.
       01  FILLER.
           05  WS-HI-VALUATION-CCYYMM  PIC 9(6) VALUE ZEROS.
           05  WS-LO-VALUATION-CCYYMM  PIC 9(6) VALUE ZEROS.
           05  FILLER REDEFINES WS-LO-VALUATION-CCYYMM.
               10  WS-LO-VAL-CCYY      PIC 9999.
               10  WS-LO-VAL-MM        PIC 99.
       01  FILLER.
           05  WS-BIN-VAL-DATES OCCURS 600
                                       PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           EJECT
                                       COPY ELCCALC.
      /
                                       COPY ELCDATE.
                                       COPY ELCFUNDT.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.
      /
       PROCEDURE DIVISION.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           MOVE 'Y'                    TO DC-FORCE-EL310-DATE-SW
           MOVE FUNCTION-DATE          TO DC-EL310-DATE


           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT


           GOBACK
           .

       0010-INITIALIZE.

           DISPLAY ' MADE IT TO 0010 '
           MOVE RUN-DATE               TO WS-WORK-DATE

           MOVE 199001                 TO WS-LO-VALUATION-CCYYMM
                                          WS-WORK-CCYYMM

           PERFORM VARYING VYR FROM +1 BY +1 UNTIL
                  VYR > +600
              MOVE WS-WORK-CCYYMM TO WS-VALUATION-CCYYMM (VYR)
              MOVE WS-WORK-CCYYMM      TO WS-CCYYMM
              
              IF WS-MM = 01 OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
                 MOVE 31               TO WS-DD
              ELSE
                 IF WS-MM = 04 OR 06 OR 09 OR 11
                    MOVE 30            TO WS-DD
                 ELSE
                    MOVE 01            TO WS-DD
                    MOVE WS-END-YEAR   TO DC-GREG-DATE-CYMD
                    MOVE 'L'           TO DC-OPTION-CODE
                    PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
                    IF NO-CONVERSION-ERROR
                       DISPLAY ' FEB DAYS ' DC-DAYS-IN-MONTH
                       MOVE DC-DAYS-IN-MONTH
                                       TO WS-DD
                    ELSE
                       DISPLAY ' PROBLEMOS WITH FEB VAL DTE CONV '
                       PERFORM ABEND-PGM
                    END-IF
                 END-IF
              END-IF
              
              MOVE WS-END-YEAR         TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              MOVE DC-BIN-DATE-1       TO WS-BIN-VAL-DATES (VYR)
              ADD 1                    TO WS-WORK-CCYYMM
              IF WS-WORK-MM > 12
                 MOVE 01               TO WS-WORK-MM
                 ADD 1                 TO WS-WORK-CCYY
              END-IF
           END-PERFORM

           SUBTRACT +1 FROM WS-WORK-CCYYMM
           IF WS-WORK-MM = ZEROS
              MOVE 12                  TO WS-WORK-MM
              SUBTRACT 1               FROM WS-WORK-CCYY
           END-IF
           MOVE WS-WORK-CCYYMM         TO WS-HI-VALUATION-CCYYMM

           PERFORM VARYING VYR FROM +1 BY +1 UNTIL
              VYR > +600
              MOVE +0                  TO WS-ISS-CNT   (VYR)
                                          WS-CNC-CNT   (VYR)
                                          WS-CLM-CNT   (VYR)
                                          WS-EXP-CNT-A (VYR)
                                          WS-EXP-CNT-B (VYR)
                                          WS-VMAIL-CNT (VYR)
           END-PERFORM

           MOVE SPACES                 TO WS-EXTRACT-RECORD
           MOVE ';'                    TO WS-TAB1
                                          WS-TAB2
                                          WS-TAB3
                                          WS-TAB4
                                          WS-TAB5
                                          WS-TAB6
           MOVE WS-EXTRACT-RECORD      TO WS-INIT-EXT-RECORD
           
           MOVE ';'                    TO WS-HTAB1
                                          WS-HTAB2
                                          WS-HTAB3
                                          WS-HTAB4
                                          WS-HTAB5
                                          WS-HTAB6

           DISPLAY '  LO VALUE  ' WS-LO-VALUATION-CCYYMM
                 '    HI VALUE  ' WS-HI-VALUATION-CCYYMM

           .

       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           DISPLAY ' MADE IT TO 0020 '
           OPEN INPUT CERTS ERMAIL
               OUTPUT EXTRACT

           IF ERMAIL-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERMAIL OPEN ERROR ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.
           DISPLAY ' MADE IT TO 0030 '

           CLOSE CERTS ERMAIL
               EXTRACT

           IF ERMAIL-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERMAIL CLOSE ERROR ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           DISPLAY ' MADE IT TO 0050 '

           PERFORM 0060-READ-CERT      THRU 0060-EXIT
           PERFORM 0080-PROCESS-CERT   THRU 0080-EXIT UNTIL
              (END-OF-INPUT)
PEMTMP*       OR (WS-CRT-CNT-TOT > 100000)

           DISPLAY ' FINISHED CERTS '

           PERFORM 0500-EMPTY-TABLE    THRU 0500-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-READ-CERT.

      *    DISPLAY ' MADE IT TO 0060 '
           READ CERTS AT END
               SET END-OF-INPUT TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-CRT-CNT
                                          WS-CRT-CNT-TOT
              IF WS-CRT-CNT = +100000
                 DISPLAY ' CERTS READ ' WS-CRT-CNT-TOT
                 MOVE +0               TO WS-CRT-CNT
              END-IF
           END-IF

           .
       0060-EXIT.
           EXIT.

       0080-PROCESS-CERT.

      *    DISPLAY ' MADE IT TO 0080 '
           MOVE CR-DT                  TO WS-EFFECT-DATE
           MOVE CR-ENTRY-DATE          TO WS-ENTRY-DATE
           MOVE ZEROS                  TO WS-CANCEL-DATE
                                          WS-CLAIM-DATE
                                          WS-EXPIRE-DATE

           IF (WS-EFFECT-CCYYMM < WS-LO-VALUATION-CCYYMM)
              OR (WS-EFFECT-CCYYMM > WS-HI-VALUATION-CCYYMM)
      *       OR (WS-ENTRY-CCYYMM < WS-LO-VALUATION-CCYYMM)
              OR (CR-ENTRY-STATUS = '9' OR 'D' OR 'V' OR 'M')
              OR (CR-LF-TERM = 1)
              OR (CR-AH-TERM = 1)
              CONTINUE
           ELSE
              IF CR-DT > 20060228
                 DISPLAY ' FUT CERT ' CR-CERT-NO ' ' WS-EFFECT-CCYYMM
              END-IF
              PERFORM VARYING VYR FROM +1 BY +1 UNTIL
                 (WS-EFFECT-CCYYMM = WS-VALUATION-CCYYMM (VYR))
                 OR (VYR > 600)
              END-PERFORM
              IF VYR > 600
                 DISPLAY ' BLEW OUT VALUATION YR ' WS-EFFECT-CCYYMM
                 PERFORM ABEND-PGM
              END-IF
              IF CR-ENTRY-STATUS = '5'
                 CONTINUE
              ELSE
                 IF (CR-LFTYP NOT = '00' AND '  ')
                              OR
                    (CR-AHTYP NOT = '00' AND '  ')
                    ADD +1             TO WS-ISS-CNT (VYR)
      *             COMPUTE WS-ISS-CNT (VYR) = WS-ISS-CNT (VYR) + 1
                 END-IF
              END-IF
              IF CR-LF-CANC-DT NOT NUMERIC
                 MOVE ZEROS            TO CR-LF-CANC-DT
              END-IF
              IF CR-AH-CANC-DT NOT NUMERIC
                 MOVE ZEROS            TO CR-AH-CANC-DT
              END-IF
              MOVE CR-LF-CANC-DT       TO WS-CANCEL-DATE
              IF (WS-CANCEL-DATE = ZEROS)
                           OR
                 ((CR-AH-CANC-DT NOT = ZEROS)
                 AND (CR-AH-CANC-DT < WS-CANCEL-DATE))
                 MOVE CR-AH-CANC-DT    TO WS-CANCEL-DATE
              END-IF
              IF WS-CANCEL-DATE > 19891231
                 PERFORM VARYING VYR FROM +1 BY +1 UNTIL
                    (WS-CANCEL-CCYYMM = WS-VALUATION-CCYYMM (VYR))
                    OR (VYR > 600)
                 END-PERFORM
                 IF VYR > 600
                    DISPLAY ' BLEW OUT CANCEL ' WS-CANCEL-CCYYMM
                    PERFORM ABEND-PGM
                 END-IF
                 ADD +1                TO WS-CNC-CNT (VYR)
      *          COMPUTE WS-CNC-CNT (VYR) =
      *             WS-CNC-CNT (VYR) + 1
              END-IF
              IF CR-DTH-DT NOT NUMERIC
                 MOVE ZEROS            TO CR-DTH-DT
              END-IF
              IF CR-DTH-DT > 19891231
                 MOVE CR-DTH-DT        TO WS-CLAIM-DATE
                 PERFORM VARYING VYR FROM +1 BY +1 UNTIL
                    (WS-CLAIM-CCYYMM = WS-VALUATION-CCYYMM (VYR))
                    OR (VYR > 600)
                 END-PERFORM
                 IF VYR > 600
                    DISPLAY ' BLEW OUT CLAIM ' WS-CLAIM-CCYYMM
                    PERFORM ABEND-PGM
                 END-IF
                 ADD +1                TO WS-CLM-CNT (VYR)
      *          COMPUTE WS-CLM-CNT (VYR) = WS-CLM-CNT (VYR) + 1
              END-IF
              IF CR-LF-EXPIRE-DATE NOT NUMERIC
                 MOVE ZEROS            TO CR-LF-EXPIRE-DATE
              END-IF
              IF CR-AH-EXPIRE-DATE NOT NUMERIC
                 MOVE ZEROS            TO CR-AH-EXPIRE-DATE
              END-IF
              MOVE CR-LF-EXPIRE-DATE   TO WS-EXPIRE-DATE
              IF (WS-EXPIRE-DATE = ZEROS)
                           OR
                 ((CR-AH-EXPIRE-DATE NOT = ZEROS)
                 AND (CR-AH-EXPIRE-DATE < WS-EXPIRE-DATE))
                 MOVE CR-AH-EXPIRE-DATE
                                       TO WS-EXPIRE-DATE
              END-IF
              IF (WS-EXPIRE-DATE > 19891231)
                 AND (WS-CANCEL-DATE = ZEROS)
                 AND (WS-CLAIM-DATE = ZEROS)
                 PERFORM VARYING VYR FROM +1 BY +1 UNTIL
                    (WS-EXPIRE-CCYYMM = WS-VALUATION-CCYYMM (VYR))
                    OR (VYR > 600)
                 END-PERFORM
                 IF VYR > 600
                    DISPLAY ' BLEW OUT EXPIRE ' WS-EXPIRE-CCYYMM
                    PERFORM ABEND-PGM
                 END-IF
                 IF CR-GRPTYP = '02' OR '03' OR '04' OR '05'
                    ADD +1             TO WS-EXP-CNT-B (VYR)
                 ELSE
                    ADD +1             TO WS-EXP-CNT-A (VYR)
                 END-IF
      *          COMPUTE WS-EXP-CNT (VYR) =
      *             WS-EXP-CNT (VYR) + 1
                 PERFORM 0090-GET-ERMAIL
                                       THRU 0090-EXIT
                 IF (ERMAIL-FILE-STATUS = '00')
                    AND (MA-ADDRESS-LINE-1 NOT = SPACES)
                    AND (MA-CITY-STATE NOT = SPACES)
                    ADD +1             TO WS-VMAIL-CNT (VYR)
      *             COMPUTE WS-VMAIL-CNT (VYR) =
      *                WS-VMAIL-CNT (VYR) + 1
                 END-IF
              END-IF
           END-IF

           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-GET-ERMAIL.

           MOVE LOW-VALUES             TO MA-CONTROL-PRIMARY
           MOVE X'04'                  TO MA-COMPANY-CD
           MOVE CR-CARRIER             TO MA-CARRIER
           MOVE CR-GROUPING            TO MA-GROUPING
           MOVE CR-STATE               TO MA-STATE
           MOVE CR-ACCOUNT             TO MA-ACCOUNT
           MOVE CR-CERT-NO             TO MA-CERT-NO
           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO MA-CERT-EFF-DT
              PERFORM 0105-READ-ERMAIL THRU 0105-EXIT
           ELSE
              DISPLAY ' BAD EFF DATE ' CR-DT '  ' CR-CERT-NO   
           END-IF
           
           .
       0090-EXIT.
           EXIT.

       0105-READ-ERMAIL.

           READ ERMAIL

           IF ERMAIL-FILE-STATUS = '10' OR '23'
              ADD 1                    TO WS-MAIL-NO-FIND
           ELSE
              IF ERMAIL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERMAIL READ ERROR ' ERMAIL-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0105-EXIT.
           EXIT.

       0500-EMPTY-TABLE.

           WRITE EXTRACT-RECORD-OUT-HD FROM WS-HEAD-RECORD
           MOVE WS-INIT-EXT-RECORD     TO WS-EXTRACT-RECORD
           DISPLAY ' MADE IT TO 0500 '
           PERFORM VARYING VYR FROM +1 BY +1 UNTIL VYR > +600
              MOVE WS-VALUATION-CCYYMM (VYR)
                                       TO EXT-VALUE-CCYYMM
              MOVE WS-ISS-CNT (VYR)    TO EXT-ISS-CNT
              MOVE WS-CNC-CNT (VYR)    TO EXT-CNC-CNT
              MOVE WS-CLM-CNT (VYR)    TO EXT-CLM-CNT
              MOVE WS-EXP-CNT-A (VYR)  TO EXT-EXP-CNT-A
              MOVE WS-EXP-CNT-B (VYR)  TO EXT-EXP-CNT-B
              MOVE WS-VMAIL-CNT (VYR)  TO EXT-VMAIL-CNT
              IF (ZERO = WS-ISS-CNT (VYR)
                 AND WS-CNC-CNT   (VYR)
                 AND WS-CLM-CNT   (VYR)
                 AND WS-EXP-CNT-A (VYR)
                 AND WS-EXP-CNT-B (VYR)
                 AND WS-VMAIL-CNT (VYR))
                 CONTINUE
              ELSE
                 WRITE EXTRACT-RECORD-OUT
                                       FROM WS-EXTRACT-RECORD
              END-IF
           END-PERFORM

           .
       0500-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

      *    DISPLAY ' MADE IT TO 8510 '
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
