       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDDERX1.
       AUTHOR.     CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-FILE-IN     ASSIGN TO EXTRIN.

           SELECT EXTR-FILE-OUT    ASSIGN TO EXTROT
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE        ASSIGN TO SYS019.
                                                                        
           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

           COPY ECSEXT01.
      /

       FD  EXTR-FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-FILE-OUT-HEAD          PIC X(367).
       01  EXTR-FILE-OUT-REC           PIC X(315).

       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '  CIDDERX1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-EXTR               VALUE 'Y'.
       77  EXT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  EXT-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  PGM-SUB                 PIC S999    COMP    VALUE +020.
       77  WS-EFF-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-INC-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-EXP-BIN              PIC XX    VALUE LOW-VALUES.
       77  WS-INC-AGE              PIC 99    VALUE ZEROS.
       77  WS-EXP-AGE              PIC 99    VALUE ZEROS.
       77  WS-DIS-MOS              PIC 9(3)  VALUE ZEROS.
       77  WS-WORK-TERM            PIC S9(3) VALUE +0 COMP-3.
       77  WS-REM-TERM             PIC S9(3) VALUE +0 COMP-3.
      /


       01  EXTR-DETAIL-RECORD.
           12  EX-REIN-CO              PIC XXX.
           12  EX-TABA                 PIC X.
           12  EX-REIN-SUB             PIC XXX.
           12  EX-TABB                 PIC X.
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
           12  EX-CERT                 PIC X(11).
           12  EX-TAB6                 PIC X.
           12  EX-REPORT-CODE-1        PIC X(10).
           12  EX-TAB7                 PIC X.
           12  EX-LNAME                PIC X(15).
           12  EX-TAB8                 PIC X.
           12  EX-FNAME                PIC X(10).
           12  EX-TAB9                 PIC X.
           12  EX-INIT                 PIC X.
           12  EX-TAB10                PIC X.
           12  EX-AGE                  PIC 99.
           12  EX-TAB11                PIC X.
           12  EX-LF-TYPE              PIC XX.
           12  EX-TAB12                PIC X.
           12  EX-LF-TERM              PIC 999.
           12  EX-TAB13                PIC X.
           12  EX-LF-BEN               PIC -9(9).99.
           12  EX-TAB14                PIC X.
           12  EX-AH-TYPE              PIC XX.
           12  EX-TAB15                PIC X.
           12  EX-AH-TERM              PIC 999.
           12  EX-TAB16                PIC X.
           12  EX-AH-BEN               PIC -9(7).99.
           12  EX-TAB17                PIC X.
           12  EX-LN-OFFICER           PIC XXX.
           12  EX-TAB18                PIC X.
           12  EX-TYPE                 PIC X(4).
           12  EX-TAB19                PIC X.
           12  EX-IBNR                 PIC -9(9).99.
           12  EX-TAB20                PIC X.
           12  EX-PAYCUR               PIC -9(9).99.
           12  EX-TAB21                PIC X.
           12  EX-FUTURE               PIC -9(9).99.
           12  EX-TAB22                PIC X.
           12  EX-INCUR                PIC X(10).
           12  EX-TAB23                PIC X.
           12  EX-REPORTED             PIC X(8).
           12  EX-TAB24                PIC X.
           12  EX-PAYTO                PIC X(10).
           12  EX-TAB25                PIC X.
           12  EX-CNUM                 PIC X(7).
           12  EX-TAB26                PIC X.
           12  EX-CLM-PROC-DT          PIC X(10).
           12  EX-TAB27                PIC X.
           12  EX-ACC-NAME             PIC X(30).
           12  EX-TAB28                PIC X.
           12  EX-STAR                 PIC X.
      ******************************************************************
       01  EXTR-DETAIL-HEADER.
           12  FILLER                  PIC X(07) VALUE
                                               'REIN CO'.
           12  EX-HTABA                PIC X.
           12  FILLER                  PIC X(08) VALUE
                                               'REIN SUB'.
           12  EX-HTABB                PIC X.
           12  FILLER                  PIC X(04) VALUE
                                               'CARR'.
           12  EX-HTAB1                PIC X.
           12  FILLER                  PIC X(05) VALUE
                                               'GROUP'.
           12  EX-HTAB2                PIC X.
           12  FILLER                  PIC X(05) VALUE
                                               'STATE'.
           12  EX-HTAB3                PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'ACCOUNT'.
           12  EX-HTAB4                PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'EFF DTE'.
           12  EX-HTAB5                PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'CERT NO'.
           12  EX-HTAB6                PIC X.
           12  FILLER                  PIC X(08) VALUE
                                               'RPT CDE1'.
           12  EX-HTAB7                PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'LAST NAME'.
           12  EX-HTAB8                PIC X.
           12  FILLER                  PIC X(10) VALUE
                                               'FIRST NAME'.
           12  EX-HTAB9                PIC X.
           12  FILLER                  PIC X(08) VALUE
                                               'MID INIT'.
           12  EX-HTAB10               PIC X.
           12  FILLER                  PIC X(03) VALUE
                                               'AGE'.
           12  EX-HTAB11               PIC X.
           12  FILLER                  PIC X(06) VALUE
                                               'LF TYP'.
           12  EX-HTAB12               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'LF TERM'.
           12  EX-HTAB13               PIC X.
           12  FILLER                  PIC X(06) VALUE
                                               'LF BEN'.
           12  EX-HTAB14               PIC X.
           12  FILLER                  PIC X(06) VALUE
                                               'AH TYP'.
           12  EX-HTAB15               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'AH TERM'.
           12  EX-HTAB16               PIC X.
           12  FILLER                  PIC X(06) VALUE
                                               'AH BEN'.
           12  EX-HTAB17               PIC X.
           12  FILLER                  PIC X(08) VALUE
                                               'LOAN OFF'.
           12  EX-HTAB18               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'RES TYP'.
           12  EX-HTAB19               PIC X.
           12  FILLER                  PIC X(04) VALUE
                                               'IBNR'.
           12  EX-HTAB20               PIC X.
           12  FILLER                  PIC X(03) VALUE
                                               'PTC'.
           12  EX-HTAB21               PIC X.
           12  FILLER                  PIC X(06) VALUE
                                               'FUTURE'.
           12  EX-HTAB22               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'INC DTE'.
           12  EX-HTAB23               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'RPT DTE'.
           12  EX-HTAB24               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'PD TO DTE'.
           12  EX-HTAB25               PIC X.
           12  FILLER                  PIC X(06) VALUE
                                               'CLM NO'.
           12  EX-HTAB26               PIC X.
           12  FILLER                  PIC X(07) VALUE
                                               'VAL DTE'.
           12  EX-HTAB27               PIC X.
           12  FILLER                  PIC X(09) VALUE
                                               'ACCT NAME'.
           12  EX-HTAB28               PIC X.
           12  FILLER                  PIC X     VALUE '*'.
      ******************************************************************
       01  WS-MISC.
           05  WS-SAVE-EXTR            PIC X(315) VALUE LOW-VALUES.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  FILLER REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.

       01  WS-ABEND-FIELDS.
           12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.
           12  WS-ZERO                 PIC S9          VALUE ZERO.
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.


                                       COPY ELCCALC.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

           EJECT
       PROCEDURE DIVISION.

                                       COPY ELCDTERX.                       

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-EXTR   THRU 0100-EXIT UNTIL
                 (END-OF-EXTR)
CIDTST*          OR (EXT-RECS-IN > 50000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' EXTR RECORDS READ    '  EXT-RECS-IN
           DISPLAY ' EXTR RECORDS WRITTEN '  EXT-RECS-OUT
           GOBACK
           .

       0100-PROCESS-EXTR.

           IF RUN-DATE = DE-RSV-PROC-DT
           MOVE WS-SAVE-EXTR           TO EXTR-DETAIL-RECORD
           MOVE LOW-VALUES             TO WS-EFF-BIN
                                          WS-INC-BIN
                                          WS-EXP-BIN

           MOVE ZEROS                  TO WS-INC-AGE
                                          WS-EXP-AGE
                                          WS-DIS-MOS
                                          WS-WORK-TERM
                                          WS-REM-TERM
                                          
           IF DE-LIFE-RSV
              MOVE DE-LF-TERM          TO WS-WORK-TERM
              MOVE 'LIFE'              TO EX-TYPE
           ELSE
              MOVE DE-AH-TERM          TO WS-WORK-TERM
              MOVE ' AH '              TO EX-TYPE
           END-IF
           
           MOVE DE-REINCO              TO EX-REIN-CO
           MOVE DE-REINCO-SUB          TO EX-REIN-SUB
           MOVE DE-CARRIER             TO EX-CARRIER
           MOVE DE-GROUPING            TO EX-GROUPING
           MOVE DE-STATE               TO EX-STATE
           MOVE DE-ACCOUNT             TO EX-ACCOUNT
           MOVE DE-EFF                 TO WS-DATE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-EFF
           END-STRING

           MOVE DE-CERT                TO EX-CERT
           MOVE DE-LNAME               TO EX-LNAME
           MOVE DE-FNAME               TO EX-FNAME
           MOVE DE-INIT                TO EX-INIT
           MOVE DE-AGE                 TO EX-AGE
           MOVE DE-LF-TYPE             TO EX-LF-TYPE
           MOVE DE-LF-TERM             TO EX-LF-TERM
           MOVE DE-LF-BEN              TO EX-LF-BEN
           MOVE DE-AH-TYPE             TO EX-AH-TYPE
           MOVE DE-AH-TERM             TO EX-AH-TERM
           MOVE DE-AH-BEN              TO EX-AH-BEN
           MOVE DE-LN-OFFICER          TO EX-LN-OFFICER

           IF DE-REIN = 'R'
              MOVE DE-REI-IBNR         TO EX-IBNR
              MOVE DE-REI-PAYCUR       TO EX-PAYCUR
              MOVE DE-REI-FUTRSV       TO EX-FUTURE
           ELSE
              MOVE DE-IBNR             TO EX-IBNR
              MOVE DE-PAYCUR           TO EX-PAYCUR
              MOVE DE-FUTRSV           TO EX-FUTURE
           END-IF

           MOVE DE-CLMNO               TO EX-CNUM
           MOVE DE-RSV-INCUR           TO WS-DATE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-INCUR
           END-STRING

           STRING DE-RSV-RPT-MO '/' DE-RSV-RPT-DA '/' DE-RSV-RPT-YR
                      DELIMITED BY SIZE
               INTO EX-REPORTED
           END-STRING

           MOVE DE-RSV-PAYTO           TO WS-DATE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-PAYTO
           END-STRING


           MOVE DE-RSV-PROC-DT         TO WS-DATE
           STRING WS-MM '/' WS-DD '/' WS-CCYY DELIMITED BY SIZE
               INTO EX-CLM-PROC-DT
           END-STRING
           MOVE DE-RSV-ACC-NAME        TO EX-ACC-NAME
           PERFORM 0300-WRITE-EXTR     THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-EXTR      THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

           
       0200-READ-EXTR.

           READ EXTR-FILE-IN AT END
              SET END-OF-EXTR          TO TRUE
           END-READ


           IF NOT END-OF-EXTR
              ADD 1 TO EXT-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-EXTR.

           WRITE EXTR-FILE-OUT-REC     FROM EXTR-DETAIL-RECORD
           ADD 1                       TO EXT-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT EXTR-FILE-IN
               OUTPUT EXTR-FILE-OUT

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE EXTR-FILE-IN EXTR-FILE-OUT

           .

       0500-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO EXTR-DETAIL-RECORD
           MOVE  ';'                   TO EX-TABA
                                          EX-TABB
                                          EX-TAB1
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
                                          EX-TAB26
                                          EX-TAB27
                                          EX-TAB28
           MOVE '*'                    TO EX-STAR
           MOVE  ';'                   TO EX-HTABA
                                          EX-HTABB
                                          EX-HTAB1
                                          EX-HTAB2
                                          EX-HTAB3
                                          EX-HTAB4
                                          EX-HTAB5
                                          EX-HTAB6
                                          EX-HTAB7
                                          EX-HTAB8
                                          EX-HTAB9
                                          EX-HTAB10
                                          EX-HTAB11
                                          EX-HTAB12
                                          EX-HTAB13
                                          EX-HTAB14
                                          EX-HTAB15
                                          EX-HTAB16
                                          EX-HTAB17
                                          EX-HTAB18
                                          EX-HTAB19
                                          EX-HTAB20
                                          EX-HTAB21
                                          EX-HTAB22
                                          EX-HTAB23
                                          EX-HTAB24
                                          EX-HTAB25
                                          EX-HTAB26
                                          EX-HTAB27
                                          EX-HTAB28

           MOVE EXTR-DETAIL-RECORD     TO WS-SAVE-EXTR
           MOVE EXTR-DETAIL-HEADER     TO EXTR-FILE-OUT-HEAD
           WRITE EXTR-FILE-OUT-HEAD

           PERFORM 0200-READ-EXTR      THRU 0200-EXIT
           .

       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.

       ABEND-PGM SECTION.
                                     COPY ELCABEND.

