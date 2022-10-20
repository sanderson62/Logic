       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDGRX1.
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
011604* 011604                   SMVA  ADD MORTALITY RESERVE TO EXTR FILE
031704* 031704                   SMVA  ADD REINSURANCE SUB TO EXTR FILE
052704* 052704 IR2004052400001   SMVA  CHG DELIMITER FROM TAB TO ; 
011604*******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GAAP-FILE-IN     ASSIGN TO GAAPIN.

           SELECT GAAP-FILE-OUT    ASSIGN TO GAAPOT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  GAAP-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSGAP01.

       FD  GAAP-FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

031704 01  GAAP-FILE-OUT-REC           PIC X(260).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDGRX1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-GAAP               VALUE 'Y'.
       77  GAP-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  GAP-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.

       01  WS-SAVE-GAAP                PIC X(260) VALUE LOW-VALUES.
       01  GAAP-DETAIL-RECORD.
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
           12  EX-TAB6A                PIC X.
           12  EX-MORT-AGE             PIC 99.
           12  EX-TAB6B                PIC X.
           12  EX-LFTYP                PIC XX.
           12  EX-TAB7                 PIC X.
           12  EX-LF-TERM              PIC 999.
           12  EX-TAB8                 PIC X.
           12  EX-LF-REMTERM           PIC 999.
           12  EX-TAB9                 PIC X.
           12  EX-LF-UP-REMTERM        PIC 999.
           12  EX-TAB10                PIC X.
           12  EX-LFBEN                PIC -9(9).99.
           12  EX-TAB11                PIC X.
           12  EX-LFPRM                PIC -9(7).99.
           12  EX-TAB12                PIC X.
           12  EX-AHTYP                PIC XX.
           12  EX-TAB13                PIC X.
           12  EX-AH-TERM              PIC 999.
           12  EX-TAB14                PIC X.
           12  EX-AH-REMTERM           PIC 999.
           12  EX-TAB15                PIC X.
           12  EX-AH-UP-REMTERM        PIC 999.
           12  EX-TAB16                PIC X.
           12  EX-AHBEN                PIC -9(7).99.
           12  EX-TAB17                PIC X.
           12  EX-AHPRM                PIC -9(7).99.
           12  EX-TAB18                PIC X.
           12  EX-REM-AMT              PIC -9(8).99.
           12  EX-TAB19                PIC X.
           12  EX-PLFPRM               PIC -9(7).99.
           12  EX-TAB20                PIC X.
           12  EX-RLFPRM               PIC -9(7).99.
           12  EX-TAB21                PIC X.
           12  EX-SLFPRM               PIC -9(7).99.
           12  EX-TAB22                PIC X.
           12  EX-PAHPRM               PIC -9(7).99.
           12  EX-TAB23                PIC X.
           12  EX-RAHPRM               PIC -9(7).99.
           12  EX-TAB24                PIC X.
           12  EX-SAHPRM               PIC -9(7).99.
           12  EX-TAB25                PIC X.
           12  EX-REIN                 PIC X.
           12  EX-TAB26                PIC X.
           12  EX-REIN-CO              PIC X(03).
           12  EX-TAB27                PIC X.
           12  EX-AH-REM-BEN           PIC -9(9).99.
011604     12  EX-TAB28                PIC X.
011604     12  EX-MORT-RESV            PIC -9(7).99.
031704     12  EX-TAB29                PIC X.
031704     12  EX-REIN-SUB             PIC X(03).
           12  EX-TAB30                PIC X.
           12  EX-MORT-CODE            PIC XXXX.
           12  EX-TAB31                PIC X.
           12  EX-EOR                  PIC X.

      ******************************************************************
       01  WS-MISC.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.

                                       COPY ELCDATE.

           EJECT
       PROCEDURE DIVISION.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
                 (END-OF-GAAP)
PEMTST*          OR (GAP-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' GAAP RECORDS READ    '  GAP-RECS-IN
           DISPLAY ' GAAP RECORDS WRITTEN '  GAP-RECS-OUT
           GOBACK

           .
       0050-PROCESS-INPUT.

           IF GR-REIN = 'P'
              AND GR-AHTYP NOT = '00' AND '  '
              PERFORM 0100-PROCESS-GAAP THRU 0100-EXIT
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

           MOVE GR-CERT-NO             TO EX-CERT-NO
           MOVE GR-AGE                 TO EX-AGE
           MOVE GR-MORT-AGE            TO EX-MORT-AGE
           MOVE GR-LFTYP               TO EX-LFTYP
           MOVE GR-LF-TERM             TO EX-LF-TERM
           MOVE GR-LF-REMTERM          TO EX-LF-REMTERM
           MOVE GR-LF-UP-REMTERM       TO EX-LF-UP-REMTERM
           MOVE GR-LFBEN               TO EX-LFBEN
           MOVE GR-LFPRM               TO EX-LFPRM
           MOVE GR-AHTYP               TO EX-AHTYP
           MOVE GR-AH-TERM             TO EX-AH-TERM
           MOVE GR-AH-REMTERM          TO EX-AH-REMTERM
           MOVE GR-AH-UP-REMTERM       TO EX-AH-UP-REMTERM
           MOVE GR-AHBEN               TO EX-AHBEN
           MOVE GR-AHPRM               TO EX-AHPRM
           MOVE GR-REM-AMT             TO EX-REM-AMT
           MOVE GRP-LFPRM              TO EX-PLFPRM
           MOVE GRR-LFPRM              TO EX-RLFPRM
           MOVE GRS-LFPRM              TO EX-SLFPRM
           MOVE GRP-AHPRM              TO EX-PAHPRM
           MOVE GRR-AHPRM              TO EX-RAHPRM
           MOVE GRS-AHPRM              TO EX-SAHPRM
           MOVE GR-REIN                TO EX-REIN
           MOVE GR-REINCO              TO EX-REIN-CO
           MOVE GR-AH-REM-BEN          TO EX-AH-REM-BEN
011604     MOVE GR-RESV                TO EX-MORT-RESV 
031704     MOVE GR-REINCO-SUB          TO EX-REIN-SUB  
           MOVE GR-MORT-CODE           TO EX-MORT-CODE

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
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB6A
                                          EX-TAB6B
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
                                          EX-TAB21
                                          EX-TAB22
                                          EX-TAB23
                                          EX-TAB24
                                          EX-TAB25
011604                                    EX-TAB26
031704                                    EX-TAB27
                                          EX-TAB28
                                          EX-TAB29
                                          EX-TAB30
                                          EX-TAB31
           MOVE 'E'                    TO EX-EOR

           MOVE GAAP-DETAIL-RECORD     TO WS-SAVE-GAAP
           PERFORM 0200-READ-GAAP      THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

