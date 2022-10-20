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
092712* 092712 CR2012092600002   PEMA ADD NEW FLDS TO EXTRACT
120816* 120816  CR2016111600001  PEMA  Calc Loaded stat uep
011604*******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GAAP-FILE-IN     ASSIGN TO GAAPIN.

           SELECT GAAP-FILE-OUT    ASSIGN TO GAAPOT
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.

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

031704 01  GAAP-FILE-OUT-REC           PIC X(600).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDGRX1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-GAAP               VALUE 'Y'.
       77  GAP-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  GAP-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.

       01  date-file-junk.
           05  pgm-sub                 pic s9(4) binary value +523.
           05  ws-return-code          pic s999  comp-3 value +0.
           05  ws-abend-message        pic x(50)        value spaces.

       01  WS-SAVE-GAAP                PIC X(600) VALUE SPACES.
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
           12  EX-LFTYP                PIC XX.
           12  EX-TAB7                 PIC X.
           12  EX-LF-TERM              PIC 999.
           12  EX-TAB8                 PIC X.
           12  EX-LF-REMTERM           PIC 999.
           12  EX-TAB9                 PIC X.
           12  EX-LFBEN                PIC -9(9).99.
           12  EX-TAB10                PIC X.
           12  EX-LFPRM                PIC -9(7).99.
           12  EX-TAB11                PIC X.
           12  EX-AHTYP                PIC XX.
           12  EX-TAB12                PIC X.
           12  EX-AH-TERM              PIC 999.
           12  EX-TAB13                PIC X.
           12  EX-AH-REMTERM           PIC 999.
           12  EX-TAB14                PIC X.
           12  EX-AHBEN                PIC -9(7).99.
           12  EX-TAB15                PIC X.
           12  EX-AHPRM                PIC -9(7).99.
           12  EX-TAB16                PIC X.
           12  EX-REM-AMT              PIC -9(8).99.
           12  EX-TAB17                PIC X.
           12  EX-PLFPRM               PIC -9(7).99.
           12  EX-TAB18                PIC X.
           12  EX-RLFPRM               PIC -9(7).99.
           12  EX-TAB19                PIC X.
           12  EX-SLFPRM               PIC -9(7).99.
           12  EX-TAB20                PIC X.
           12  EX-PAHPRM               PIC -9(7).99.
           12  EX-TAB21                PIC X.
           12  EX-RAHPRM               PIC -9(7).99.
           12  EX-TAB22                PIC X.
           12  EX-SAHPRM               PIC -9(7).99.
           12  EX-TAB23                PIC X.
           12  EX-REIN                 PIC X.
           12  EX-TAB24                PIC X.
           12  EX-REIN-CO              PIC X(03).
           12  EX-TAB25                PIC X.
           12  EX-AH-REM-BEN           PIC -9(9).99.
011604     12  EX-TAB26                PIC X.
011604     12  EX-MORT-RESV            PIC -9(7).99.
031704     12  EX-TAB27                PIC X.
031704     12  EX-REIN-SUB             PIC X(03).
           12  EX-TAB28                PIC X.
           12  EX-SEX-CODE             PIC X.
           12  EX-TAB29                PIC X.
           12  EX-LOAN-TERM            PIC 999.
           12  EX-TAB30                PIC X.
           12  EX-LF-EXP-DT            PIC X(10).
           12  EX-TAB31                PIC X.
           12  EX-AH-EXP-DT            PIC X(10).
           12  EX-TAB32                PIC X.
           12  EX-LF-UP-REMTERM        PIC 999.
           12  EX-TAB33                PIC X.
           12  EX-AH-UP-REMTERM        PIC 999.
           12  EX-TAB34                PIC X.
           12  EX-UNLD-STAT-RESV       PIC -9(7).99.
           12  EX-TAB35                PIC X.
           12  EX-LFCOM                PIC -9(5).99.
           12  EX-TAB36                PIC X.
           12  EX-PLFCOM               PIC -9(5).99.
           12  EX-TAB37                PIC X.
           12  EX-RLFCOM               PIC -9(5).99.
           12  EX-TAB38                PIC X.
           12  EX-SLFCOM               PIC -9(5).99.
           12  EX-TAB39                PIC X.
           12  EX-AHCOM                PIC -9(5).99.
           12  EX-TAB40                PIC X.
           12  EX-PAHCOM               PIC -9(5).99.
           12  EX-TAB41                PIC X.
           12  EX-RAHCOM               PIC -9(5).99.
           12  EX-TAB42                PIC X.
           12  EX-SAHCOM               PIC -9(5).99.
           12  EX-TAB43                PIC X.
           12  EX-MO-DEC               PIC -9(7).99.
           12  EX-TAB44                PIC X.
           12  EX-MORT-FACT            PIC -9(5).9(4).
           12  EX-TAB45                PIC X.
           12  EX-MORT-AGE             PIC 999.
           12  EX-TAB46                PIC X.
           12  EX-MORT-TABLE           PIC XXXX.
           12  EX-TAB47                PIC X.
           12  EX-ALT-MORT-TABLE       PIC XXXX.

      ******************************************************************
       01  WS-MISC.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-GAAP   THRU 0100-EXIT UNTIL
                 (END-OF-GAAP)
PEMTST*          OR (GAP-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' GAAP RECORDS READ    '  GAP-RECS-IN
           DISPLAY ' GAAP RECORDS WRITTEN '  GAP-RECS-OUT
           GOBACK

           .
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
           MOVE GR-LFTYP               TO EX-LFTYP
           MOVE GR-LF-TERM             TO EX-LF-TERM
           MOVE GR-LF-REMTERM          TO EX-LF-REMTERM
           MOVE GR-LFBEN               TO EX-LFBEN
           MOVE GR-LFPRM               TO EX-LFPRM
           MOVE GR-AHTYP               TO EX-AHTYP
           MOVE GR-AH-TERM             TO EX-AH-TERM
           MOVE GR-AH-REMTERM          TO EX-AH-REMTERM
           MOVE GR-AHBEN               TO EX-AHBEN
           MOVE GR-AHPRM               TO EX-AHPRM
           MOVE GR-REM-AMT             TO EX-REM-AMT
           MOVE GRP-LFPRM              TO EX-PLFPRM
           MOVE GRR-LFPRM              TO EX-RLFPRM
           MOVE GRS-LFPRM              TO EX-SLFPRM
           MOVE GRP-AHPRM              TO EX-PAHPRM
           MOVE GRR-AHPRM              TO EX-RAHPRM
120816     compute ex-sahprm =
120816        grs-ahprm + gr-loaded-stat-uep
           MOVE GR-REIN                TO EX-REIN
           MOVE GR-REINCO              TO EX-REIN-CO
           MOVE GR-AH-REM-BEN          TO EX-AH-REM-BEN
011604     MOVE GR-RESV                TO EX-MORT-RESV 
031704     MOVE GR-REINCO-SUB          TO EX-REIN-SUB  
           IF GR-SEX-CODE = 'F'
              MOVE GR-SEX-CODE         TO EX-SEX-CODE
           ELSE
              MOVE 'M'                 TO EX-SEX-CODE
           END-IF
           MOVE GR-LOAN-TERM           TO EX-LOAN-TERM
           MOVE GR-LF-UP-REMTERM       TO EX-LF-UP-REMTERM
           MOVE GR-AH-UP-REMTERM       TO EX-AH-UP-REMTERM
           IF GR-UNLD-STAT-MORT-RESV NOT NUMERIC
              MOVE ZERO                TO GR-UNLD-STAT-MORT-RESV
           END-IF
           MOVE GR-UNLD-STAT-MORT-RESV TO EX-UNLD-STAT-RESV
           MOVE GR-LFCOM               TO EX-LFCOM
           MOVE GRR-LFCOM              TO EX-RLFCOM
           MOVE GRP-LFCOM              TO EX-PLFCOM
           MOVE GRS-LFCOM              TO EX-SLFCOM
           MOVE GR-AHCOM               TO EX-AHCOM
           MOVE GRR-AHCOM              TO EX-RAHCOM
           MOVE GRP-AHCOM              TO EX-PAHCOM
           MOVE GRS-AHCOM              TO EX-SAHCOM
           MOVE GR-MO-DEC              TO EX-MO-DEC
           MOVE GR-MORT-FACT           TO EX-MORT-FACT
           MOVE GR-MORT-CODE           TO EX-MORT-TABLE
           MOVE GR-ALT-MORT-CODE       TO EX-ALT-MORT-TABLE
           MOVE GR-MORT-AGE            TO EX-MORT-AGE

           IF GR-LF-EXPIRE-DATE = ZEROS
              MOVE SPACES              TO EX-LF-EXP-DT
           ELSE
              MOVE GR-LF-EXPIRE-DATE   TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EX-LF-EXP-DT
              END-STRING
           END-IF

           IF GR-AH-EXPIRE-DATE = ZEROS
              MOVE SPACES              TO EX-AH-EXP-DT
           ELSE
              MOVE GR-AH-EXPIRE-DATE   TO WS-DATE
              MOVE ' '                 TO DC-OPTION-CODE
              STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
                                       INTO EX-AH-EXP-DT
              END-STRING
           END-IF

           PERFORM 0300-WRITE-GAAP     THRU 0300-EXIT
           PERFORM 0200-READ-GAAP      THRU 0200-EXIT

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
                                          EX-TAB32
                                          EX-TAB33
                                          EX-TAB34
                                          EX-TAB35
                                          EX-TAB36
                                          EX-TAB37
                                          EX-TAB38
                                          EX-TAB39
                                          EX-TAB40
                                          EX-TAB41
                                          EX-TAB42
                                          EX-TAB43
                                          EX-TAB44
                                          EX-TAB45
                                          EX-TAB46
                                          EX-TAB47

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

       abend-pgm.

           call 'abortme'.
           goback.
