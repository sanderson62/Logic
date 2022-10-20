       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PEMCRX9A.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.  THAO  LAW  SUIT EXTRACT

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERTS        ASSIGN TO SYS010.
           SELECT SORT-CERTS   ASSIGN TO SORTWK1.
           SELECT ERACCT       ASSIGN TO ERACCTT
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERACCT-FILE-STATUS
                               RECORD KEY IS AM-CONTROL-PRIMARY.
           SELECT ERMAIL       ASSIGN TO ERMAIL
                               ACCESS IS RANDOM
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERMAIL-FILE-STATUS
                               RECORD KEY IS MA-CONTROL-PRIMARY.
           SELECT EXTRACT      ASSIGN TO SYS011
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DISK-DATE    ASSIGN TO SYS019.
           SELECT PRINTX       ASSIGN TO SYS008.

       DATA DIVISION.
       FILE SECTION.

       FD  CERTS
                                       COPY ECSCRIFD.
                                       COPY ECSCRT01.

       SD  SORT-CERTS.

       01  SORT-CERT-REC.
           05  SORT-STATE              PIC XX.
           05  SORT-BUS-TYPE           PIC XX.
           05  SORT-REPORT-CODE-3      PIC X(10).
           05  SORT-FIN-RESP           PIC X(10).
           05  SORT-ACCT-NO            PIC X(10).
           05  SORT-ISSUE-YEAR         PIC 9999.
           05  SORT-ACCT-NAME          PIC X(30).
           05  SORT-EXP-DATE           PIC 9(8).
           05  SORT-CERT-CNT           PIC S9(7)  COMP-3.
           05  SORT-EXP-CNT            PIC S9(7)  COMP-3.
           05  SORT-CANC-CNT           PIC S9(7)  COMP-3.
           05  SORT-ACT-CNT            PIC S9(7)  COMP-3.
           05  SORT-LIFE-PREM          PIC S9(9)V99 COMP-3.
           05  SORT-AH-PREM            PIC S9(9)V99 COMP-3.
           05  SORT-TOTAL-PREM         PIC S9(11)V99 COMP-3.
           05  SORT-LIFE-REFUNDS       PIC S9(9)V99 COMP-3.
           05  SORT-AH-REFUNDS         PIC S9(9)V99 COMP-3.
           05  SORT-TOTAL-REFUNDS      PIC S9(11)V99 COMP-3.
           05  SORT-PC-ANNIV-CNT       PIC S9(7)  COMP-3.
           05  SORT-PC-EXP-CNT         PIC S9(7)  COMP-3.
           05  SORT-PC-CRT-CNT         PIC S9(7)  COMP-3.
           05  FILLER                  PIC X(6).

       FD  ERACCT.
                                       COPY ERCACCT.

       FD  ERMAIL.
                                       COPY ERCMAIL.

       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  EXTRACT-RECORD-OUT          PIC X(275).

       FD  DISK-DATE
                                       COPY ELCDTEFD.
       FD  PRINTX
                                       COPY ELCPRTFD.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     PEMCRX9A WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  M1                          PIC S999   COMP-3 VALUE +0.
       77  STNDX                       PIC S9(03) COMP-3 VALUE +0.
       77  BUSNDX                      PIC S9(03) COMP-3 VALUE +0.
       77  ACTNDX                      PIC S9(03) COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  ERACCT-FILE-STATUS          PIC XX     VALUE '00'.
       77  ERMAIL-FILE-STATUS          PIC XX     VALUE '00'.
       77  WS-SKIP-SW                  PIC X      VALUE ' '.
           88  SKIP-CERT                          VALUE 'Y'.
       77  WS-CERT-IN                  PIC 9(9) VALUE ZEROS.
       77  WS-REC-REL                  PIC 9(9) VALUE ZEROS.
       77  WS-ISS-OUT                  PIC 9(9) VALUE ZEROS.
       77  WS-EXP-OUT                  PIC 9(9) VALUE ZEROS.
       77  WS-ACT-OUT                  PIC 9(9) VALUE ZEROS.
       77  WS-CNC-OUT                  PIC 9(9) VALUE ZEROS.
       77  WS-CANCEL-SW                PIC X    VALUE SPACES.
           88  NOT-BOTH-CANCELLED           VALUE 'Y'.

       01  WS-EXTRACT-INIT             PIC X(275).

       01  EXTRACT-RECORD.
           05  EXT-STATE               PIC XX.
           05  EXT-D1                  PIC X.
           05  EXT-BUS-TYPE            PIC XX.
           05  EXT-D1A                 PIC X.
           05  EXT-REPORT-CODE-3       PIC X(10).
           05  EXT-D2                  PIC X.
           05  EXT-FIN-RESP            PIC X(10).
           05  EXT-D3                  PIC X.
           05  EXT-ACCT-NO             PIC X(10).
           05  EXT-D4                  PIC X.
           05  EXT-ISSUE-YEAR          PIC 9999.
           05  EXT-D4A                 PIC X.
           05  EXT-ACCT-NAME           PIC X(30).
           05  EXT-D5                  PIC X.
           05  EXT-EXP-DATE            PIC 9(8).
           05  EXT-D6                  PIC X.
           05  EXT-CERT-CNT            PIC ZZZ,ZZZ,ZZ9.
           05  EXT-D7                  PIC X.
           05  EXT-EXP-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  EXT-D8                  PIC X.
           05  EXT-CANC-CNT            PIC ZZZ,ZZZ,ZZ9.
           05  EXT-D9                  PIC X.
           05  EXT-ACT-CNT             PIC ZZZ,ZZZ,ZZ9.
           05  EXT-D10                 PIC X.
           05  EXT-LF-WRITTEN          PIC ZZZ,ZZZ,ZZ9.99.
           05  EXT-D11                 PIC X.
           05  EXT-AH-WRITTEN          PIC ZZZ,ZZZ,ZZ9.99.
           05  EXT-D12                 PIC X.
           05  EXT-TOT-WRITTEN         PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  EXT-D13                 PIC X.
           05  EXT-LF-REFUNDS          PIC ZZZ,ZZZ,ZZ9.99.
           05  EXT-D14                 PIC X.
           05  EXT-AH-REFUNDS          PIC ZZZ,ZZZ,ZZ9.99.
           05  EXT-D15                 PIC X.
           05  EXT-TOT-REFUNDS         PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  EXT-D16                 PIC X.
           05  EXT-PC-ANNIV-CNT        PIC ZZZ,ZZZ,ZZ9.
           05  EXT-D17                 PIC X.
           05  EXT-PC-EXP-CNT          PIC ZZZ,ZZZ,ZZ9.
           05  EXT-D18                 PIC X.
           05  EXT-PC-CRT-CNT          PIC ZZZ,ZZZ,ZZ9.
           05  EXT-D19                 PIC X.
           05  EXT-EOR                 PIC X.

       01  WS-BIG-TABLE.
           05  WS-STATE                PIC XX.
           05  WS-BUS-TYPE             PIC XX.
           05  WS-REPORT-CODE-3        PIC X(10).
           05  WS-FIN-RESP             PIC X(10).
           05  WS-ACCT-NO              PIC X(10).
           05  WS-ISSUE-YEAR           PIC 9999.
           05  WS-ACCT-NAME            PIC X(30).
           05  WS-EXP-DATE             PIC 9(8).
           05  WS-CERT-CNT             PIC S9(7)  COMP-3.
           05  WS-EXP-CNT              PIC S9(7)  COMP-3.
           05  WS-CANC-CNT             PIC S9(7)  COMP-3.
           05  WS-ACT-CNT              PIC S9(7)  COMP-3.
           05  WS-LIFE-PREM            PIC S9(9)V99 COMP-3.
           05  WS-AH-PREM              PIC S9(9)V99 COMP-3.
           05  WS-TOTAL-PREM           PIC S9(11)V99 COMP-3.
           05  WS-LIFE-REFUNDS         PIC S9(9)V99 COMP-3.
           05  WS-AH-REFUNDS           PIC S9(9)V99 COMP-3.
           05  WS-TOTAL-REFUNDS        PIC S9(11)V99 COMP-3.
           05  WS-PC-ANNIV-CNT         PIC S9(7)  COMP-3.
           05  WS-PC-EXP-CNT           PIC S9(7)  COMP-3.
           05  WS-PC-CRT-CNT           PIC S9(7)  COMP-3.
           05  FILLER                  PIC X(6) VALUE SPACES.

       01  WS-MISC.
           05  WS-ACCT-SW              PIC X VALUE ' '.
               88  WE-FIND-AN-ACCOUNT        VALUE 'Y'.
           05  WS-DISPLAY-DATE         PIC ZZZ9(8).
           05  WS-DISPLAY-TERM         PIC Z99.
           05  WS-DISPLAY-RTERM        PIC Z99.99.
           05  WS-DISPLAY-PRM          PIC ZZZZZZZ.99.
           05  WS-DISPLAY-UEP          PIC ZZZZZZZ.99.
           05  WS-BIN-CR-DT            PIC XX  VALUE LOW-VALUES.
           05  WS-BIN-LF-END-DATE      PIC XX  VALUE LOW-VALUES.
           05  WS-BIN-AH-END-DATE      PIC XX  VALUE LOW-VALUES.
           05  WS-END-YEAR             PIC 9(11)  VALUE ZEROS.
           05  FILLER REDEFINES WS-END-YEAR.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9(4).
               10  WS-MMDD             PIC 9(4).
       01  WS-DEBUG-AREA.
           12  WS-DEBUG-SW             PIC X(01) VALUE ' '.
               88  DEBUG-IS-ON                VALUES '1' '2' '3' '4'.
               88  DEBUG-LF-INFORCE-CNT       VALUE '1'.
               88  DEBUG-AH-INFORCE-CNT       VALUE '2'.
               88  DEBUG-LF-STATUTORY         VALUE '3'.
               88  DEBUG-AH-STATUTORY         VALUE '4'.

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

       01  DATE-AREAS.
           05  WS-EXPIRE-DATE          PIC 9(11) VALUE ZEROS.
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYY        PIC 9(4).
               10  WS-WORK-MMDD        PIC 9(4).
           05  WS-ISSUE-DATE           PIC 9(11).
           05  FILLER REDEFINES WS-ISSUE-DATE.
               10  FILLER              PIC XXX.
               10  WS-ISSUE-CCYY       PIC 9(4).
               10  WS-ISSUE-MMDD       PIC 9(4).
           05  WS-CANCEL-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-CANCEL-DATE.
               10  FILLER              PIC XXX.
               10  WS-CANCEL-CCYY      PIC 9(4).
               10  WS-CANCEL-MMDD      PIC 9(4).
           05  WS-NEW-CANCEL-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-NEW-CANCEL-DATE.
               10  FILLER              PIC XXX.
               10  WS-NEW-CANCEL-CCYY      PIC 9(4).
               10  WS-NEW-CANCEL-MMDD      PIC 9(4).
           05  WS-CANCEL-EXIT-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-CANCEL-EXIT-DATE.
               10  FILLER              PIC XXX.
               10  WS-CANCEL-EXIT-CCYY      PIC 9(4).
               10  WS-CANCEL-EXIT-MMDD      PIC 9(4).
           05  WS-CLAIM-DATE           PIC 9(11).
           05  FILLER REDEFINES WS-CLAIM-DATE.
               10  FILLER              PIC XXX.
               10  WS-CLAIM-CCYY       PIC 9(4).
               10  WS-CLAIM-MMDD       PIC 9(4).
           05  WS-HI-ISSUE-CCYY        PIC 9(4) VALUE ZEROS.
           05  WS-LO-ISSUE-CCYY        PIC 9(4) VALUE ZEROS.
           05  WS-HI-VALUATION-CCYY    PIC 9(4) VALUE ZEROS.
           05  WS-LO-VALUATION-CCYY    PIC 9(4) VALUE ZEROS.
           05  WS-RE-ISSUES            PIC 9(7) VALUE ZEROS.
           05  WS-BIN-VAL-DATES OCCURS 18
                                       PIC XX.

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           SORT SORT-CERTS ON ASCENDING KEY SORT-STATE
                                            SORT-BUS-TYPE
                                            SORT-REPORT-CODE-3
                                            SORT-ACCT-NO
                                            SORT-EXP-DATE
                                            SORT-ISSUE-YEAR
                INPUT PROCEDURE 0002-INPUT THRU 0002-EXIT
                OUTPUT PROCEDURE 0003-OUTPUT THRU 0003-EXIT

           DISPLAY ' CERT IN           ' WS-CERT-IN
           DISPLAY ' CERTS RELEASED    ' WS-REC-REL
           DISPLAY ' ISS OUT           ' WS-ISS-OUT
           DISPLAY ' EXP OUT           ' WS-EXP-OUT
           DISPLAY ' ACT OUT           ' WS-ACT-OUT
           DISPLAY ' CNC OUT           ' WS-CNC-OUT

           GOBACK

             .
       0002-INPUT SECTION.

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0120-START-ERACCT THRU 0120-EXIT
           PERFORM 0110-READ-ERACCT THRU 0110-EXIT

           PERFORM 0060-READ-CERT      THRU 0060-EXIT
           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
              END-OF-INPUT

           .
       0002-EXIT.
           EXIT.

       0003-OUTPUT SECTION.

           MOVE SPACES                 TO WS-EOF-SW

           PERFORM 0070-RETURN-REC     THRU 0070-EXIT

           MOVE SORT-STATE             TO WS-STATE
           MOVE SORT-BUS-TYPE          TO WS-BUS-TYPE
           MOVE SORT-FIN-RESP          TO WS-FIN-RESP
           MOVE SORT-ACCT-NO           TO WS-ACCT-NO
           MOVE SORT-REPORT-CODE-3     TO WS-REPORT-CODE-3
           MOVE SORT-ISSUE-YEAR        TO WS-ISSUE-YEAR

           PERFORM 0075-PROCESS-RECS THRU 0075-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0076-BREAK THRU 0076-EXIT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           .
       0003-EXIT.
           EXIT.

       0010-INITIALIZE.

           MOVE SPACES                 TO WS-BUS-TYPE
                                          WS-FIN-RESP
                                          WS-ACCT-NO
                                          WS-STATE
                                          EXTRACT-RECORD
           MOVE ';'                    TO EXT-D1
                                          EXT-D1A
                                          EXT-D2
                                          EXT-D3
                                          EXT-D4
                                          EXT-D4A
                                          EXT-D5
                                          EXT-D6
                                          EXT-D7
                                          EXT-D8
                                          EXT-D9
                                          EXT-D10
                                          EXT-D11
                                          EXT-D12
                                          EXT-D13
                                          EXT-D14
                                          EXT-D15
                                          EXT-D16
                                          EXT-D17
                                          EXT-D18
                                          EXT-D19
           MOVE 'E'                    TO EXT-EOR
           MOVE EXTRACT-RECORD         TO WS-EXTRACT-INIT

           MOVE +0                     TO WS-CERT-CNT
                                          WS-EXP-CNT
                                          WS-CANC-CNT
                                          WS-ACT-CNT
                                          WS-PC-ANNIV-CNT
                                          WS-PC-EXP-CNT
                                          WS-PC-CRT-CNT
                                          WS-LIFE-PREM
                                          WS-AH-PREM
                                          WS-TOTAL-PREM
                                          WS-LIFE-REFUNDS
                                          WS-AH-REFUNDS
                                          WS-TOTAL-REFUNDS

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT CERTS ERACCT ERMAIL
               OUTPUT EXTRACT

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERACCT OPEN ERROR ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERMAIL OPEN ERROR ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY '  RE ISSUES  = ' WS-RE-ISSUES
           CLOSE CERTS ERACCT ERMAIL
               EXTRACT

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERACCT CLOSE ERROR ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERMAIL ERROR - CLOSE ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           PERFORM 0080-PROCESS-CERT   THRU 0080-EXIT

           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-READ-CERT.

           READ CERTS AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO WS-CERT-IN
           END-IF
           
           .
       0060-EXIT.
           EXIT.

       0070-RETURN-REC.

           RETURN SORT-CERTS    AT END
               SET END-OF-INPUT        TO TRUE
           END-RETURN

           .
       0070-EXIT.
           EXIT.

       0075-PROCESS-RECS.

           IF (SORT-BUS-TYPE      NOT = WS-BUS-TYPE)
              OR (SORT-FIN-RESP   NOT = WS-FIN-RESP)
              OR (SORT-REPORT-CODE-3 NOT = WS-REPORT-CODE-3)
              OR (SORT-ACCT-NO    NOT = WS-ACCT-NO)
              OR (SORT-STATE      NOT = WS-STATE)
              OR (SORT-ISSUE-YEAR NOT = WS-ISSUE-YEAR)
              PERFORM 0076-BREAK       THRU 0076-EXIT
           ELSE
              ADD SORT-CERT-CNT      TO WS-CERT-CNT
              ADD SORT-EXP-CNT       TO WS-EXP-CNT
              ADD SORT-CANC-CNT      TO WS-CANC-CNT
              ADD SORT-ACT-CNT       TO WS-ACT-CNT
              ADD SORT-PC-ANNIV-CNT  TO WS-PC-ANNIV-CNT
              ADD SORT-PC-EXP-CNT    TO WS-PC-EXP-CNT
              ADD SORT-PC-CRT-CNT    TO WS-PC-CRT-CNT
              ADD SORT-LIFE-PREM     TO WS-LIFE-PREM
              ADD SORT-AH-PREM       TO WS-AH-PREM
              ADD SORT-TOTAL-PREM    TO WS-TOTAL-PREM
              ADD SORT-LIFE-REFUNDS  TO WS-LIFE-REFUNDS
              ADD SORT-AH-REFUNDS    TO WS-AH-REFUNDS
              ADD SORT-TOTAL-REFUNDS TO WS-TOTAL-REFUNDS
           END-IF

           PERFORM 0070-RETURN-REC THRU 0070-EXIT

           .
       0075-EXIT.
           EXIT.

       0076-BREAK.

           MOVE WS-EXTRACT-INIT        TO EXTRACT-RECORD
           MOVE WS-STATE               TO EXT-STATE
           MOVE WS-BUS-TYPE            TO EXT-BUS-TYPE
           MOVE WS-FIN-RESP            TO EXT-FIN-RESP
           MOVE WS-ACCT-NO             TO EXT-ACCT-NO
           MOVE WS-ISSUE-YEAR          TO EXT-ISSUE-YEAR
           MOVE WS-REPORT-CODE-3       TO EXT-REPORT-CODE-3
           MOVE WS-ACCT-NAME           TO EXT-ACCT-NAME
           MOVE WS-EXP-DATE            TO EXT-EXP-DATE
           MOVE WS-CERT-CNT            TO EXT-CERT-CNT
           MOVE WS-EXP-CNT             TO EXT-EXP-CNT
           MOVE WS-CANC-CNT            TO EXT-CANC-CNT
           MOVE WS-ACT-CNT             TO EXT-ACT-CNT
           MOVE WS-PC-ANNIV-CNT        TO EXT-PC-ANNIV-CNT
           MOVE WS-PC-EXP-CNT          TO EXT-PC-EXP-CNT
           MOVE WS-PC-CRT-CNT          TO EXT-PC-CRT-CNT
           MOVE WS-LIFE-PREM           TO EXT-LF-WRITTEN
           MOVE WS-AH-PREM             TO EXT-AH-WRITTEN
           MOVE WS-TOTAL-PREM          TO EXT-TOT-WRITTEN
           MOVE WS-LIFE-REFUNDS        TO EXT-LF-REFUNDS
           MOVE WS-AH-REFUNDS          TO EXT-AH-REFUNDS
           MOVE WS-TOTAL-REFUNDS       TO EXT-TOT-REFUNDS

           WRITE EXTRACT-RECORD-OUT    FROM EXTRACT-RECORD

           MOVE SORT-CERT-REC          TO WS-BIG-TABLE

           .
       0076-EXIT.
           EXIT.

       0080-PROCESS-CERT.

           MOVE SPACES                 TO WS-SKIP-SW

           IF CR-ENTRY-STATUS = 'V' OR '9' OR 'D'
              SET SKIP-CERT TO TRUE
           END-IF

           IF CR-DT < 20020101
              SET SKIP-CERT TO TRUE
           END-IF

           IF CR-ENTRY-STATUS = '5'
              IF (CR-LF-CANC-DT = ZEROS) AND
                 (CR-AH-CANC-DT = ZEROS)
                 SET SKIP-CERT TO TRUE
              ELSE
                 MOVE ZEROS    TO CR-LFPRM CR-AHPRM CR-LFPRM-ALT
              END-IF
           END-IF

           IF CR-LFTYP = '18' OR '19' OR '28' OR '57' OR '58'
                      OR '78' OR '79' OR '1A' OR '1B' OR '2A'
                      OR '2B' OR '2C' OR '2D' OR '60' OR 'QL'
                      OR 'QD'
              SET SKIP-CERT TO TRUE
           END-IF

           IF CR-AHTYP = '20' OR '21' OR '22' OR '34' OR '44'
                      OR '45' OR '46' OR '47' OR '2V' OR '2X'
                      OR '5A' OR '5B' OR '7X' OR '7Y' OR '7Z'
                      OR '8A'
              SET SKIP-CERT TO TRUE
           END-IF

           IF (CR-DTHAMT NOT = ZEROS)
              OR (CR-DISAMT NOT = ZEROS)
               SET SKIP-CERT TO TRUE
           END-IF

           MOVE CR-LF-EXPIRE-DATE      TO WS-EXPIRE-DATE
           IF CR-AH-EXPIRE-DATE > WS-EXPIRE-DATE
              MOVE CR-AH-EXPIRE-DATE   TO WS-EXPIRE-DATE
           END-IF
           MOVE ZEROS TO WS-NEW-CANCEL-DATE
           MOVE CR-LF-CANCEL-EXIT-DATE TO WS-CANCEL-EXIT-DATE
           IF CR-AH-CANCEL-EXIT-DATE > WS-CANCEL-EXIT-DATE
              MOVE CR-AH-CANCEL-EXIT-DATE TO WS-CANCEL-EXIT-DATE
           END-IF

           MOVE CR-LF-CANC-DT          TO WS-CANCEL-DATE
           IF CR-AH-CANC-DT > WS-CANCEL-DATE
              MOVE CR-AH-CANC-DT       TO WS-CANCEL-DATE
           END-IF

           IF ((CR-LFTYP NOT = ZEROS AND SPACES)
              AND (CR-LF-CANC-DT = ZEROS))
                         OR
              ((CR-AHTYP NOT = ZEROS AND SPACES)
              AND (CR-AH-CANC-DT = ZEROS))
              SET NOT-BOTH-CANCELLED   TO TRUE
           END-IF

           MOVE WS-EXPIRE-DATE         TO WS-WORK-DATE
           IF (WS-CANCEL-DATE NOT = ZEROS)
              AND (WS-CANCEL-DATE < WS-EXPIRE-DATE)
              MOVE WS-CANCEL-DATE      TO WS-WORK-DATE
           END-IF

           IF NOT SKIP-CERT
              PERFORM 0090-CHECK-REST  THRU 0090-EXIT
           ELSE
              PERFORM 0130-GET-ERMAIL  THRU 0130-EXIT
              IF ERMAIL-FILE-STATUS = '00'
                 PERFORM VARYING M1 FROM +1 BY +1 UNTIL
                    M1 > +7
                    IF (MA-MAIL-TYPE (M1) = '1')
                       AND (MA-MAIL-STATUS (M1) = '1')
                       ADD  +1         TO WS-PC-ANNIV-CNT
                       MOVE +1         TO WS-PC-CRT-CNT
                    ELSE
                       IF (WS-EXP-CNT = +1)
                          AND (MA-MAIL-TYPE (M1) = '2')
                          AND (MA-MAIL-STATUS (M1) = '1')
                          ADD  +1      TO WS-PC-EXP-CNT
                          MOVE +1      TO WS-PC-CRT-CNT
                       END-IF
                    END-IF
                 END-PERFORM
                 IF WS-PC-CRT-CNT = +1
                    RELEASE SORT-CERT-REC FROM  WS-BIG-TABLE
                    ADD 1              TO WS-REC-REL
                    MOVE +0            TO WS-PC-EXP-CNT
                                          WS-PC-CRT-CNT
                                          WS-PC-ANNIV-CNT
                 END-IF
              END-IF
           END-IF

           .
       0080-EXIT.
           EXIT.

       0090-CHECK-REST.

           MOVE ' ' TO WS-ACCT-SW
           PERFORM 0100-HIT-ACCOUNT THRU 0100-EXIT UNTIL
                 WE-FIND-AN-ACCOUNT
           MOVE CR-STATE               TO WS-STATE

      *    MOVE CR-GRPTYP              TO WS-BUS-TYPE

           IF CR-REMIT-TO NOT NUMERIC
              MOVE 01                  TO CR-REMIT-TO
           END-IF
           IF CR-REMIT-TO > ZEROS
              MOVE CR-COM-AGT (CR-REMIT-TO)
                                       TO WS-FIN-RESP
           ELSE
              MOVE CR-COM-AGT (01)     TO WS-FIN-RESP
           END-IF

           MOVE CR-ACCOUNT             TO WS-ACCT-NO

           MOVE +0                     TO WS-CERT-CNT
                                          WS-PC-ANNIV-CNT
                                          WS-PC-EXP-CNT
                                          WS-TOTAL-PREM
                                          WS-LIFE-PREM
                                          WS-AH-PREM
           MOVE +0                     TO WS-CANC-CNT
                                          WS-TOTAL-REFUNDS
                                          WS-LIFE-REFUNDS
                                          WS-AH-REFUNDS

           IF CR-ENTRY-STATUS NOT = '5'
              MOVE +1                  TO WS-CERT-CNT
              ADD 1                    TO WS-ISS-OUT
              COMPUTE WS-LIFE-PREM = CR-LFPRM + CR-LFPRM-ALT
              MOVE CR-AHPRM            TO WS-AH-PREM
              COMPUTE WS-TOTAL-PREM = CR-LFPRM + CR-LFPRM-ALT
                      + CR-AHPRM
           END-IF

           IF CR-ENTRY-STATUS = '5'
              IF WS-CANCEL-DATE NOT = ZERO
                 ADD  1        TO WS-RE-ISSUES
              END-IF
           END-IF

           IF WS-CANCEL-DATE NOT = ZERO
              MOVE +1                  TO WS-CANC-CNT
              ADD 1                    TO WS-CNC-OUT
              MOVE CR-LFRFND           TO WS-LIFE-REFUNDS
              MOVE CR-AHRFND           TO WS-AH-REFUNDS
              COMPUTE WS-TOTAL-REFUNDS = CR-LFRFND + CR-AHRFND
           END-IF

           IF (WS-CANCEL-DATE = ZERO)
      *       OR (NOT-BOTH-CANCELLED)
              IF WS-EXPIRE-DATE > 20080229
                 IF CR-ACCOUNT = '0000084620'
                    DISPLAY CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 END-IF
                 ADD 1                 TO WS-ACT-OUT
                 MOVE +1               TO WS-ACT-CNT
              ELSE
                 ADD 1                 TO WS-EXP-OUT
                 MOVE +1               TO WS-EXP-CNT
              END-IF
           END-IF
           
           MOVE CR-DT                  TO WS-END-YEAR
           MOVE WS-CCYY                TO WS-ISSUE-YEAR

           PERFORM 0130-GET-ERMAIL     THRU 0130-EXIT

           IF ERMAIL-FILE-STATUS = '00'
              PERFORM VARYING M1 FROM +1 BY +1 UNTIL
                 M1 > +7
                 IF (MA-MAIL-TYPE (M1) = '1')
                    AND (MA-MAIL-STATUS (M1) = '1')
                    ADD  +1            TO WS-PC-ANNIV-CNT
                    MOVE +1            TO WS-PC-CRT-CNT
                 ELSE
                    IF (WS-EXP-CNT = +1)
                       AND (MA-MAIL-TYPE (M1) = '2')
                       AND (MA-MAIL-STATUS (M1) = '1')
                       ADD  +1         TO WS-PC-EXP-CNT
                       MOVE +1         TO WS-PC-CRT-CNT
                    END-IF
                 END-IF
              END-PERFORM
           END-IF

           RELEASE SORT-CERT-REC FROM  WS-BIG-TABLE
           ADD 1                       TO WS-REC-REL

      *    IF CR-ENTRY-STATUS NOT = '5'
      *       MOVE +1                  TO WS-CERT-CNT
      *       COMPUTE WS-LIFE-PREM = CR-LFPRM + CR-LFPRM-ALT
      *       MOVE CR-AHPRM            TO WS-AH-PREM
      *       COMPUTE WS-TOTAL-PREM = CR-LFPRM + CR-LFPRM-ALT
      *               + CR-AHPRM
      *       RELEASE SORT-CERT-REC FROM  WS-BIG-TABLE
      *    END-IF

      *    MOVE +0                     TO WS-CERT-CNT
      *                                   WS-TOTAL-PREM
      *                                   WS-LIFE-PREM
      *                                   WS-AH-PREM

      *    IF CR-ENTRY-STATUS = '5'
      *       IF WS-CANCEL-DATE NOT = ZERO
      *          ADD  1        TO WS-RE-ISSUES
      *       END-IF
      *    END-IF

      *    IF WS-CANCEL-DATE NOT = ZERO
      *       MOVE +1                  TO WS-CANC-CNT
      *       ADD 1                    TO WS-CNC-OUT
      *       MOVE CR-LFRFND           TO WS-LIFE-REFUNDS
      *       MOVE CR-AHRFND           TO WS-AH-REFUNDS
      *       COMPUTE WS-TOTAL-REFUNDS = CR-LFRFND + CR-AHRFND
      *       RELEASE SORT-CERT-REC FROM  WS-BIG-TABLE
      *    END-IF

      *    MOVE +0                     TO WS-CANC-CNT
      *                                   WS-TOTAL-REFUNDS
      *                                   WS-LIFE-REFUNDS
      *                                   WS-AH-REFUNDS

      *    IF WS-CANCEL-DATE = ZERO
      *       IF WS-EXPIRE-DATE > 20080229
      *          ADD 1                 TO WS-ACT-OUT
      *          MOVE +1               TO WS-ACT-CNT
      *          RELEASE SORT-CERT-REC FROM  WS-BIG-TABLE
      *       ELSE
      *          ADD 1                 TO WS-EXP-OUT
      *          MOVE +1               TO WS-EXP-CNT
      *          RELEASE SORT-CERT-REC FROM  WS-BIG-TABLE
      *       END-IF
      *    END-IF

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           .
       0090-EXIT.
           EXIT.

       0100-HIT-ACCOUNT.

           IF AM-CONTROL-A = CR-ACCT-CONTROL
              IF CR-DT < AM-EXPIRE-DT
                 IF CR-DT NOT < AM-EFFECT-DT
                    MOVE AM-NAME       TO WS-ACCT-NAME
                    MOVE AM-GPCD       TO WS-BUS-TYPE
                    MOVE AM-EXPIRE-DT  TO WS-EXP-DATE
                    MOVE AM-REPORT-CODE-3 TO WS-REPORT-CODE-3
                    SET WE-FIND-AN-ACCOUNT TO TRUE
                 ELSE
                    DISPLAY ' MISSING ACCOUNT MASTER '
                                 CR-FULL-CONTROL
                    MOVE '??'          TO WS-BUS-TYPE
                    SET WE-FIND-AN-ACCOUNT TO TRUE
                 END-IF
              ELSE
                 PERFORM 0110-READ-ERACCT THRU 0110-EXIT
              END-IF
           ELSE
              IF AM-CONTROL-A < CR-ACCT-CONTROL
                 PERFORM 0110-READ-ERACCT THRU 0110-EXIT
              ELSE
                 DISPLAY ' MISSING ACCOUNT MASTER '
                                 CR-FULL-CONTROL
                    MOVE '??'          TO WS-BUS-TYPE
                    SET WE-FIND-AN-ACCOUNT TO TRUE
              END-IF
           END-IF

           .
       0100-EXIT.
           EXIT.

       0110-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF ERACCT-FILE-STATUS = '10' OR '23'
              MOVE HIGH-VALUES TO AM-CONTROL-A
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERACCT READ ERROR ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ERACCT.

           MOVE LOW-VALUES TO AM-CONTROL-PRIMARY

           START ERACCT KEY NOT < AM-CONTROL-PRIMARY

           IF ERACCT-FILE-STATUS = '10' OR '23'
              MOVE HIGH-VALUES TO AM-CONTROL-A
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERACCT START ERROR ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0120-EXIT.
           EXIT.

       0130-GET-ERMAIL.

           MOVE LOW-VALUES             TO MA-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO MA-COMPANY-CD
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
              READ ERMAIL
              IF ERMAIL-FILE-STATUS = '00' OR '23' OR '22' OR '10'
                 CONTINUE
              ELSE
                 DISPLAY ' ERMAIL ERROR - READ ' ERMAIL-FILE-STATUS
                    '  ' CR-CERT-NO
           ELSE
              DISPLAY ' BAD EFF DATE ' CR-DT '  ' CR-CERT-NO   
           END-IF
           
           .
       0130-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

