       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMGRX3.
       AUTHOR.     PABLO.
       DATE-COMPILED.
011604******************************************************************
011604*                   C H A N G E   L O G
011604*
011604* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011604*-----------------------------------------------------------------
011604*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011604* EFFECTIVE    NUMBER
011604*-----------------------------------------------------------------
011604*******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GAAP-FILE-IN     ASSIGN TO GAAPIN.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT GAAP-FILE-OUT    ASSIGN TO GAAPOT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  GAAP-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY ECSGAP01.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  GAAP-FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

031704 01  GAAP-FILE-OUT-REC           PIC X(262).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMGRX4  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-GAAP                   VALUE 'Y'.
       77  GAP-RECS-IN                 PIC 9(9) VALUE ZEROS.
       77  GAP-RECS-OUT                PIC 9(9) VALUE ZEROS.
       77  S1                          PIC S9(5) VALUE +0 COMP-3.
       77  PGM-SUB                     PIC S9(5) COMP-3 VALUE +515.
       77  ELCNTL-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-ELCNTL-SW                PIC X   VALUE ' '.
           88  END-OF-ELCNTL                  VALUE 'Y'.
       77  ELCNTL-RECS-IN              PIC 9(5) VALUE ZEROS.
       77  WS-TEMP-TAX1                PIC S9(7)V99  COMP-3 VALUE +0.
       77  WS-TEMP-TAX                 PIC S9(7)V99  COMP-3 VALUE +0.
       77  WS-TAX-FACTOR               PIC S99V9(5)  COMP-3 VALUE +0.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04) COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WS-SAVE-GAAP                PIC X(262) VALUE LOW-VALUES.
       01  GAAP-DETAIL-RECORD.
           12  EX-ISS-YEAR             PIC 9999.
           12  EX-TABA                 PIC X.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-EFF                  PIC X(10).
           12  EX-TAB5                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB6                 PIC X.
           12  EX-AGE                  PIC 99.
           12  EX-TAB7                 PIC X.
           12  EX-VAGE                 PIC 99.
           12  EX-TAB8                 PIC X.
           12  EX-APR                  PIC ZZ9.9999.
           12  EX-TAB9                 PIC X.
           12  EX-LFTYP                PIC XX.
           12  EX-TAB10                PIC X.
           12  EX-LF-TERM              PIC 999.
           12  EX-TAB11                PIC X.
           12  EX-LF-REMTERM           PIC 999.
           12  EX-TAB12                PIC X.
           12  EX-LFBEN                PIC -9(9).99.
           12  EX-TAB13                PIC X.
           12  EX-LFPRM                PIC -9(7).99.
           12  EX-TAB14                PIC X.
           12  EX-REM-AMT              PIC -9(8).99.
           12  EX-TAB15                PIC X.
011604     12  EX-MORT-RESV            PIC -9(7).99.
031704     12  EX-TAB16                PIC X.
011604     12  EX-MO-DEC               PIC -9(7).99.
           12  EX-TAB17                PIC X.
           12  EX-TABLE                PIC XXXX.
           12  EX-TAB18                PIC X.
           12  EX-ALT-MORT-RESV        PIC -9(7).99.
           12  EX-TAB19                PIC X.
           12  EX-ALT-TABLE            PIC XXXX.
           12  EX-TAB20                PIC X.
           12  EX-EOR                  PIC X.

      ******************************************************************
       01  WS-MISC.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.

                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
                                       COPY ELCDATE.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-GAAP   THRU 0050-EXIT UNTIL
                 (END-OF-GAAP)
PEMTST*          OR (GAP-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' GAAP RECORDS READ    '  GAP-RECS-IN
           DISPLAY ' GAAP RECORDS WRITTEN '  GAP-RECS-OUT
           GOBACK

           .
       0050-PROCESS-GAAP.

           IF (GR-REIN = 'P')
              AND (GR-LFTYP NOT = '  ' AND '00')
              PERFORM 0100-PROCESS-GAAP
                                       THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-GAAP      THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-GAAP.

           MOVE WS-SAVE-GAAP           TO GAAP-DETAIL-RECORD
           MOVE GR-CARRIER             TO EX-CARRIER
           MOVE GR-GROUPING            TO EX-GROUPING
           MOVE GR-STATE               TO EX-STATE
           MOVE GR-ACCOUNT             TO EX-ACCOUNT
           MOVE GR-EFF                 TO WS-DATE
           MOVE ' '                    TO DC-OPTION-CODE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-EFF
           END-STRING

           MOVE WS-CCYY                TO EX-ISS-YEAR
           MOVE GR-CERT-NO             TO EX-CERT-NO
           MOVE GR-APR                 TO EX-APR
           MOVE GR-AGE                 TO EX-AGE
           MOVE GR-MORT-AGE            TO EX-VAGE
           MOVE GR-LFTYP               TO EX-LFTYP
           MOVE GR-LF-TERM             TO EX-LF-TERM
           MOVE GR-LF-REMTERM          TO EX-LF-REMTERM
           MOVE GR-LFBEN               TO EX-LFBEN
           MOVE GR-LFPRM               TO EX-LFPRM
           MOVE GR-REM-AMT             TO EX-REM-AMT
011604     MOVE GR-RESV                TO EX-MORT-RESV
           MOVE GR-MO-DEC              TO EX-MO-DEC
           MOVE GR-MORT-CODE           TO EX-TABLE
           MOVE GR-ALT-RESV            TO EX-ALT-MORT-RESV
           MOVE GR-ALT-MORT-CODE       TO EX-ALT-TABLE

           MOVE 'E'                    TO EX-EOR


           PERFORM 0300-WRITE-GAAP     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-GAAP.

           READ GAAP-FILE-IN AT END
              SET END-OF-GAAP          TO TRUE
           END-READ


           IF NOT END-OF-GAAP
              ADD 1 TO GAP-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-GAAP.

           WRITE GAAP-FILE-OUT-REC     FROM GAAP-DETAIL-RECORD
           ADD 1 TO GAP-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT GAAP-FILE-IN
               OUTPUT GAAP-FILE-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE GAAP-FILE-IN GAAP-FILE-OUT


           .
       0500-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO GAAP-DETAIL-RECORD
052704     MOVE ';'                    TO EX-TAB1
                                          EX-TABA
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
                                          EX-TAB9
                                          EX-TAB10
                                          EX-TAB11
                                          EX-TAB12
                                          EX-TAB13
                                          EX-TAB14
                                          EX-TAB15
                                          EX-TAB16
                                          EX-TAB17
                                          EX-TAB18
                                          EX-TAB19
                                          EX-TAB20

           MOVE GAAP-DETAIL-RECORD     TO WS-SAVE-GAAP


           PERFORM 0200-READ-GAAP      THRU 0200-EXIT

           MOVE +1                     TO S1

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.