       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ECS0561.
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.

      *REMARKS.
080309******************************************************************
080309*                   C H A N G E   L O G
080309*
080309* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080309*-----------------------------------------------------------------
080309*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080309* EFFECTIVE    NUMBER
080309*-----------------------------------------------------------------
080309* 080309  2009010500003    PEMA  CHECK FOR NON 0 B4 DIVIDE
080309******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DISK-DATE            ASSIGN TO SYS019.
           SELECT PRINTX               ASSIGN TO SYS008.
           SELECT FICH                 ASSIGN TO SYS020.
           SELECT ERDUEP               ASSIGN TO ERDUEP
                  ACCESS IS DYNAMIC
                  ORGANIZATION IS INDEXED
                  FILE STATUS IS ERDUEP-FILE-STATUS
                  RECORD KEY IS DP-CONTROL-PRIMARY.

           SELECT FREEDOM-EXTRACT      ASSIGN TO SYS012
              ORGANIZATION IS LINE SEQUENTIAL.
                                                                        
       DATA DIVISION.
       FILE SECTION.


       EJECT
       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  PRINTX
                                       COPY ELCPRTFD.
           EJECT
       FD  FICH
                                       COPY ELCFCHFD.
       EJECT
       FD  ERDUEP.
                                       COPY ERCDUEP.

       FD  FREEDOM-EXTRACT                                              
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  FREEDOM-RECORD      PIC X(250).                              
                                                                        
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '      ECS0561 WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-ERDUEP              VALUE 'Y'.
           88  MORE-ERDUEP                VALUE 'N'.
       77  WS-A                       PIC S9(03) COMP-3 VALUE +0.
       77  IYR                         PIC S9(03) COMP-3 VALUE +0.
       77  VYR                         PIC S9(03) COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  SPACE-NP                    PIC X      VALUE '1'.
       77  SPACE-1                     PIC X      VALUE ' '.
       77  SPACE-2                     PIC X      VALUE '0'.
       77  SPACE-3                     PIC X      VALUE '-'.
       77  X                           PIC X      VALUE ' '.
       77  LNCTR                       PIC S999   VALUE +60 COMP-3.
       77  PGCTR                       PIC S9(5)  VALUE +0 COMP-3.
       77  ERDUEP-IN-CNT               PIC 9(11)   VALUE ZEROS.
       77  WS-BASE-PCT                 PIC S9V9(5) COMP-3 VALUE +0.
       77  WS-OW-PCT                   PIC S9V9(5) COMP-3 VALUE +0.
       77  WS-DUE-COMM                 PIC S9(9)V99 COMP-3 VALUE +0.

       01  HD1.
           12  FILLER              PIC  X(44)      VALUE SPACES.
           12  FILLER              PIC  X(44)      VALUE
                   '        DUE PREMIUM AND COMMISSION         '.
           12  FILLER              PIC  X(31)      VALUE SPACES.
           12  FILLER              PIC  X(08)      VALUE 'ECS0561'.

       01  HD2.
           12  FILLER              PIC  X(51)      VALUE SPACES.
           12  HD-CO               PIC  X(30).
           12  FILLER              PIC  X(38)      VALUE SPACES.
           12  HD-RUN-DT           PIC  X(08)      VALUE SPACES.

       01  HD3.
           12  FILLER              PIC  X(57)      VALUE SPACES.
           12  HD-DT               PIC  X(18).
           12  FILLER              PIC  X(44)      VALUE SPACES.
           12  FILLER              PIC  X(05)      VALUE 'PAGE '.
           12  HD-PG               PIC ZZ,ZZ9.

       01  HD4.
           12  FILLER              PIC  X(45)      VALUE
                   '-- ACCOUNT TOTALS  --       BEGINNING   EXPEC'.
           12  FILLER              PIC  X(46)      VALUE
                   'TED   RECEIVED                ENDING          '.
           12  FILLER              PIC  X(41)      VALUE
                   '                    DUE      NO OF       '.

       01  HD5.
           12  FILLER              PIC  X(45)      VALUE
                   '--CAR GROUP  ST  ACCOUNT---  BALANCE    PREMI'.
           12  FILLER              PIC  X(46)      VALUE
                   'UMS   PREMIUMS  ADJUSTMENTS   BALANCE      < 9'.
           12  FILLER              PIC  X(41)      VALUE
                   '0        > 90   COMMISSION   MONTHS      '.
       01  DETAIL-LINE.
           05  FILLER                  PIC X(4).
           05  DTL1-CAR                PIC X.
           05  FILLER                  PIC X.
           05  DTL1-GRP                PIC X(6).
           05  FILLER                  PIC X.
           05  DTL1-ST                 PIC XX.
           05  FILLER                  PIC X.
           05  DTL1-ACCT               PIC X(10).
           05  FILLER                  PIC X.
           05  DTL1-BEG-BAL            PIC ZZZ,ZZ9.99-.
           05  FILLER                  PIC X.
           05  DTL1-EXP-PREM           PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-REC-PREM           PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-ADJS               PIC ZZZ,ZZ9.99-.
           05  FILLER                  PIC X.
           05  DTL1-END-BAL            PIC ZZZ,ZZ9.99-.
           05  FILLER                  PIC X.
           05  DTL1-DUE-LT90           PIC ZZZ,ZZ9.99-.
           05  FILLER                  PIC X.
           05  DTL1-DUE-GT90           PIC ZZZ,ZZ9.99-.
           05  FILLER                  PIC X.
           05  DTL1-COMM               PIC ZZZ,ZZ9.99-.
           05  FILLER                  PIC X(4).
           05  DTL1-NO-MOS             PIC ZZ9.
       EJECT
       01  WS-DUE-PREM-TABLE.
           05  WS-RECORD.
               10  WS-CAR              PIC X.   
               10  WS-GROUP            PIC X(6).
               10  WS-STATE            PIC XX.
               10  WS-ACCOUNT          PIC X(10).
               10  WS-EFF-DATE         PIC X(10).
               10  WS-CERT-NO          PIC X(11).
               10  WS-LF-BEN-CODE      PIC XX.
               10  WS-AH-BEN-CODE      PIC XX.
               10  WS-LF-TERM          PIC ZZ9.
               10  WS-AH-TERM          PIC ZZ9.
               10  WS-LF-EXP-DATE      PIC X(10).
               10  WS-AH-EXP-DATE      PIC X(10).
               10  WS-EXP-PREM         PIC S9(9)V99 COMP-3 VALUE +0.
               10  WS-REC-PREM         PIC S9(9)V99 COMP-3 VALUE +0.
               10  WS-BASE-COMM        PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-GA-COMM          PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC1            PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC2            PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC3            PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC4            PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC5            PIC S9(7)V99 COMP-3 VALUE +0.

       01  WS-MISC.
           05  ERDUEP-FILE-STATUS      PIC XX.
           05  WS-HOLD-PRT             PIC X(133).
           05  WS-DUEP-CONTROL         PIC X(19)    VALUE LOW-VALUES.
           05  WS-HOLD-CONTROL.
               10  WS-HOLD-CAR         PIC X.
               10  WS-HOLD-GRP         PIC X(6).
               10  WS-HOLD-ST          PIC XX.
               10  WS-HOLD-ACCT        PIC X(10).
           05  INTERMED                PIC S9(9)V9(6)  COMP-3.
           05  WS-REM-AMT              PIC S9(11)V99 COMP-3 VALUE +0.
           05  WS-DISPLAY-DATE         PIC ZZZ9(8).
           05  WS-DISPLAY-TERM         PIC Z99.
           05  WS-DISPLAY-RTERM        PIC Z99.99.
           05  WS-DISPLAY-AMT          PIC ZZZZZZZ.99.
           05  WS-DISPLAY-RAMT         PIC ZZZZZZZ.99.
           05  WS-DISPLAY-PRM          PIC ZZZZZZZ.99.
           05  WS-DISPLAY-UEP          PIC ZZZZZZZ.99.
           05  WS-BIN-CR-DT            PIC XX  VALUE LOW-VALUES.
           05  WS-BIN-LF-END-DATE      PIC XX  VALUE LOW-VALUES.
           05  WS-BIN-AH-END-DATE      PIC XX  VALUE LOW-VALUES.
           05  WS-END-YEAR             PIC 9(11)  VALUE ZEROS.
           05  FILLER REDEFINES WS-END-YEAR.
               10  WS-CCYY             PIC 9(7).
               10  WS-MMDD             PIC 9(4).
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

       01  WS-EXTRACT-RECORD.
                                       COPY FNC022.

       01  DATE-AREAS.
           05  WS-TEST-RUN-DATE        PIC 9(11).
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
           05  WS-EXPIRE-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-EXPIRE-DATE.
               10  FILLER              PIC XXX.
               10  WS-EXPIRE-CCYY      PIC 9(4).
               10  WS-EXPIRE-MMDD      PIC 9(4).
           05  WS-ENTRY-DATE           PIC 9(11).
           05  FILLER REDEFINES WS-ENTRY-DATE.
               10  FILLER              PIC XXX.
               10  WS-ENTRY-CCYY       PIC 9(4).
               10  WS-ENTRY-MMDD       PIC 9(4).
           05  WS-CANCEL-DATE          PIC 9(11).
           05  FILLER REDEFINES WS-CANCEL-DATE.
               10  FILLER              PIC XXX.
               10  WS-CANCEL-CCYY      PIC 9(4).
               10  WS-CANCEL-MMDD      PIC 9(4).
           05  WS-CLAIM-DATE           PIC 9(11).
           05  FILLER REDEFINES WS-CLAIM-DATE.
               10  FILLER              PIC XXX.
               10  WS-CLAIM-CCYY       PIC 9(4).
               10  WS-CLAIM-MMDD       PIC 9(4).
           05  WS-POSTING-DATE         PIC X(8).
           05  WS-HI-ISSUE-CCYY        PIC 9(4) VALUE ZEROS.
           05  WS-LO-ISSUE-CCYY        PIC 9(4) VALUE ZEROS.
           05  WS-HI-VALUATION-CCYY    PIC 9(4) VALUE ZEROS.
           05  WS-LO-VALUATION-CCYY    PIC 9(4) VALUE ZEROS.
           05  WS-BIN-VAL-DATES OCCURS 50
                                       PIC XX.

       01  WS-MISC-AREA.
           12  WS-FREEDOM-TABLE.
               16  FILLER OCCURS       55.
                   20  WS-FREE-STATE   PIC XX.
                   20  WS-FREE-PREM    PIC S9(9)V99 COMP-3 VALUE +0.
                   20  WS-FREE-COMM    PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-DP-EXP-PREM          PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-DP-REC-PREM          PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-DP-GA-COMM           PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-DP-BASE-COMM         PIC S9(9)V99 COMP-3 VALUE +0.
      *   1ST OCCURANCE           ACCOUNT TOTALS
      *   2ND                     STATE
      *   3RD                     GROUP
      *   4TH                     CARRIER
      *   5TH                     FINAL                
           12  WS-TOTAL-TABLE OCCURS 5.
               16  WS-TOT-BAL-FWD      PIC S9(9)V99 COMP-3 VALUE +0.
               16  WS-TOT-EXP-PREM     PIC S9(9)V99 COMP-3 VALUE +0.
               16  WS-TOT-REC-PREM     PIC S9(9)V99 COMP-3 VALUE +0.
               16  WS-TOT-GA-COMM      PIC S9(9)V99 COMP-3 VALUE +0.
               16  WS-TOT-BASE-COMM    PIC S9(9)V99 COMP-3 VALUE +0.
               16  WS-TOT-ADJS         PIC S9(9)V99 COMP-3 VALUE +0.
               16  WS-TOT-END-BAL      PIC S9(9)V99 COMP-3 VALUE +0.
               16  WS-TOT-DUE-COMM     PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-DISPLAY-FLD          PIC 9(9).9(5).
           12  WS-LF-PRM               PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-AH-PRM               PIC S9(7)V99 COMP-3  VALUE +0.


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +064.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           EJECT
                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.
      /
       PROCEDURE DIVISION.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           MOVE COMPANY-NAME           TO  HD-CO
      *    MOVE SPACES                 TO  SAVE-COMPANY-NAME
           MOVE ALPH-DATE              TO  HD-DT
           MOVE WS-CURRENT-DATE        TO  HD-RUN-DT
      *    MOVE SPACE-NP               TO  P-REC

           MOVE RUN-DATE               TO WS-TEST-RUN-DATE

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT


           GOBACK
           .

       0010-INITIALIZE.


           MOVE +0                     TO WS-EXP-PREM  
                                          WS-REC-PREM  
                                          WS-GA-COMM
                                          WS-BASE-COMM

           PERFORM VARYING WS-A FROM +1 BY +1 UNTIL
              (WS-A > +5)
              MOVE +0                  TO WS-TOT-EXP-PREM (WS-A)
                                          WS-TOT-REC-PREM (WS-A)
                                          WS-TOT-BASE-COMM (WS-A)
                                          WS-TOT-GA-COMM  (WS-A)
                                          WS-TOT-BAL-FWD  (WS-A)
                                          WS-TOT-ADJS     (WS-A)
                                          WS-TOT-END-BAL  (WS-A)
                                          WS-TOT-DUE-COMM (WS-A)
           END-PERFORM
           
           PERFORM VARYING WS-A FROM +1 BY +1 UNTIL
              (WS-A > +55)
              MOVE LOW-VALUES          TO WS-FREE-STATE (WS-A)
              MOVE +0                  TO WS-FREE-PREM (WS-A)
                                          WS-FREE-COMM (WS-A)
           END-PERFORM
           
           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE WS-WORK-MMDD           TO WS-POSTING-DATE (1:4)
           MOVE WS-WORK-CCYY           TO WS-POSTING-DATE (5:4)

           .

       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ERDUEP
               OUTPUT PRINTX FREEDOM-EXTRACT

           IF ERDUEP-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERDUEP OPEN  '
                 ERDUEP-FILE-STATUS
              MOVE ' ERROR ON ERDUEP OPEN  '
                                       TO WS-ABEND-MESSAGE
              MOVE ERDUEP-FILE-STATUS  TO WS-ABEND-FILE-STATUS      
              PERFORM ABEND-PGM
           END-IF
              

           .

       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' MADE IT TO CLOSE FILES '

           CLOSE PRINTX
                 ERDUEP FREEDOM-EXTRACT

           IF ERDUEP-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERDUEP CLOSE '
                 ERDUEP-FILE-STATUS
              MOVE ' ERROR ON ERDUEP OPEN  '
                                       TO WS-ABEND-MESSAGE
              MOVE ERDUEP-FILE-STATUS  TO WS-ABEND-FILE-STATUS      
              PERFORM ABEND-PGM
           END-IF
              

           .

       0030-EXIT.
           EXIT.

       0040-START-ERDUEP.

           MOVE LOW-VALUES             TO DP-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO DP-COMPANY-CD
           START ERDUEP KEY IS NOT < DP-CONTROL-PRIMARY
           
           IF ERDUEP-FILE-STATUS = '10' OR '23'
              SET END-OF-ERDUEP        TO TRUE
           ELSE
              IF ERDUEP-FILE-STATUS NOT = '00'
                 MOVE ' ERROR ON ERDUEP  START     '
                                       TO WS-ABEND-MESSAGE
                 MOVE ERDUEP-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS      
                 DISPLAY WS-ABEND-MESSAGE '  ' WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
           
           .

       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           PERFORM 0040-START-ERDUEP   THRU 0040-EXIT
           PERFORM 0060-READ-ERDUEP    THRU 0060-EXIT
           MOVE DP-ACCOUNT-CONTROL     TO WS-HOLD-CONTROL
           PERFORM 0080-PROCESS-ERDUEP THRU 0080-EXIT UNTIL
                 END-OF-ERDUEP
           DISPLAY ' END OF 0080 PROCESS '
           PERFORM 0300-ACCT-BREAK     THRU 0300-EXIT
           PERFORM 0310-ST-BREAK       THRU 0310-EXIT
           PERFORM 0320-GRP-BREAK      THRU 0320-EXIT
           PERFORM 0330-CAR-BREAK      THRU 0330-EXIT
           PERFORM 0340-FIN-BREAK      THRU 0340-EXIT
           PERFORM 0350-CREATE-FREEDOM THRU 0350-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-READ-ERDUEP.

           READ ERDUEP NEXT RECORD

           IF ERDUEP-FILE-STATUS = '10' OR '23'
              SET END-OF-ERDUEP        TO TRUE
           ELSE
              IF ERDUEP-FILE-STATUS NOT = '00'
                 MOVE ' ERROR ON ERDUEP READ NEXT  '
                                       TO WS-ABEND-MESSAGE
                 MOVE ERDUEP-FILE-STATUS
                                       TO WS-ABEND-FILE-STATUS      
                 DISPLAY WS-ABEND-MESSAGE '  ' WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 ADD +1                TO ERDUEP-IN-CNT
              END-IF
           END-IF
           
           .
       0060-EXIT.
           EXIT.

       0080-PROCESS-ERDUEP.

           IF DP-ACCOUNT-CONTROL NOT = WS-HOLD-CONTROL
              PERFORM 0250-BREAK-RTN   THRU 0250-EXIT
           END-IF

           COMPUTE WS-TOT-BAL-FWD (1) = WS-TOT-BAL-FWD (1)
              + DP-BAL-FWD

           COMPUTE WS-TOT-EXP-PREM (1) = WS-TOT-EXP-PREM (1)
              + DP-EXP-PREM

           COMPUTE WS-TOT-REC-PREM (1) = WS-TOT-REC-PREM (1)
              + DP-REC-PREM

           COMPUTE WS-TOT-GA-COMM (1) = WS-TOT-GA-COMM (1)
              + DP-GA-COMM

           COMPUTE WS-TOT-BASE-COMM (1) = WS-TOT-BASE-COMM (1)
              + DP-BASE-COMM

           COMPUTE WS-TOT-ADJS (1)    = WS-TOT-ADJS (1)
              + DP-ADJUSTMENTS

           IF DP-END-BAL NOT NUMERIC
              MOVE ZERO TO DP-END-BAL
           END-IF
           COMPUTE WS-TOT-END-BAL (1) = WS-TOT-END-BAL (1)
              + DP-END-BAL

080309     IF (DP-EXP-PREM + DP-BASE-COMM) NOT = ZEROS
              COMPUTE WS-BASE-PCT = DP-BASE-COMM
                 / (DP-EXP-PREM + DP-BASE-COMM)
              COMPUTE WS-OW-PCT   = DP-GA-COMM
                 / (DP-EXP-PREM + DP-BASE-COMM)
              COMPUTE WS-DUE-COMM = (DP-END-BAL
                 / (1 - WS-BASE-PCT)) * WS-OW-PCT
              COMPUTE WS-TOT-DUE-COMM (1) = WS-TOT-DUE-COMM (1)
                 + WS-DUE-COMM
080309     END-IF
              
           MOVE DP-ACCOUNT-CONTROL     TO WS-HOLD-CONTROL

           PERFORM 0060-READ-ERDUEP    THRU 0060-EXIT

           .

       0080-EXIT.
           EXIT.
           


       0250-BREAK-RTN.

           IF WS-HOLD-CAR NOT = DP-CARRIER
              PERFORM 0300-ACCT-BREAK  THRU 0300-EXIT
              PERFORM 0310-ST-BREAK    THRU 0310-EXIT
              PERFORM 0320-GRP-BREAK   THRU 0320-EXIT
              PERFORM 0330-CAR-BREAK   THRU 0330-EXIT
           ELSE
              IF WS-HOLD-GRP NOT = DP-GROUPING
                 PERFORM 0300-ACCT-BREAK
                                       THRU 0300-EXIT
                 PERFORM 0310-ST-BREAK THRU 0310-EXIT
                 PERFORM 0320-GRP-BREAK
                                       THRU 0320-EXIT
              ELSE
                 IF WS-HOLD-ST NOT = DP-STATE
                    PERFORM 0300-ACCT-BREAK
                                       THRU 0300-EXIT
                    PERFORM 0310-ST-BREAK
                                       THRU 0310-EXIT
                 ELSE
                    IF WS-HOLD-ACCT NOT = DP-ACCOUNT
                       PERFORM 0300-ACCT-BREAK
                                       THRU 0300-EXIT
                    END-IF
                 END-IF
              END-IF
           END-IF
           .        
       0250-EXIT.
           EXIT.

       0300-ACCT-BREAK.

           IF (WS-TOT-EXP-PREM (1) = ZEROS)
              AND (WS-TOT-REC-PREM (1) = ZEROS)
              AND (WS-TOT-GA-COMM (1)  = ZEROS)
              AND (WS-TOT-END-BAL (1)  = ZEROS)
              AND (WS-TOT-BAL-FWD (1)  = ZEROS)
              AND (WS-TOT-ADJS (1)     = ZEROS)
              DISPLAY ' ALL ZEROS '
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE WS-HOLD-CAR         TO DTL1-CAR
              MOVE WS-HOLD-GRP         TO DTL1-GRP
              MOVE WS-HOLD-ST          TO DTL1-ST
              MOVE WS-HOLD-ACCT        TO DTL1-ACCT
              MOVE WS-TOT-BAL-FWD (1)  TO DTL1-BEG-BAL
              MOVE WS-TOT-EXP-PREM (1) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (1) TO DTL1-REC-PREM
              MOVE WS-TOT-ADJS (1)     TO DTL1-ADJS
              MOVE WS-TOT-END-BAL (1)  TO DTL1-END-BAL
              MOVE WS-TOT-DUE-COMM (1) TO DTL1-COMM
              MOVE DETAIL-LINE         TO P-DATA
              MOVE SPACE-2             TO P-CTL
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

              COMPUTE WS-TOT-BAL-FWD  (2) = WS-TOT-BAL-FWD  (2)
                 + WS-TOT-BAL-FWD  (1)
              COMPUTE WS-TOT-EXP-PREM (2) = WS-TOT-EXP-PREM (2)
                 + WS-TOT-EXP-PREM (1)
              COMPUTE WS-TOT-REC-PREM (2) = WS-TOT-REC-PREM (2)
                 + WS-TOT-REC-PREM (1)
              COMPUTE WS-TOT-BASE-COMM  (2) = WS-TOT-BASE-COMM  (2)
                 + WS-TOT-BASE-COMM  (1)
              COMPUTE WS-TOT-GA-COMM  (2) = WS-TOT-GA-COMM  (2)
                 + WS-TOT-GA-COMM  (1)
              COMPUTE WS-TOT-ADJS     (2) = WS-TOT-ADJS     (2)
                 + WS-TOT-ADJS     (1)
              COMPUTE WS-TOT-END-BAL  (2) = WS-TOT-END-BAL  (2)
                 + WS-TOT-END-BAL  (1)
              COMPUTE WS-TOT-DUE-COMM (2) = WS-TOT-DUE-COMM (2)
                 + WS-TOT-DUE-COMM (1)
           END-IF
              
           MOVE +0                     TO WS-TOT-EXP-PREM (1)
                                          WS-TOT-REC-PREM (1)
                                          WS-TOT-BASE-COMM (1)
                                          WS-TOT-GA-COMM  (1)
                                          WS-TOT-BAL-FWD  (1)
                                          WS-TOT-ADJS     (1)
                                          WS-TOT-END-BAL  (1)
                                          WS-TOT-DUE-COMM (1)
           .
       0300-EXIT.
           EXIT.
           
       0310-ST-BREAK.

           IF (WS-TOT-EXP-PREM (2) = ZEROS)
              AND (WS-TOT-REC-PREM (2) = ZEROS)
              AND (WS-TOT-GA-COMM (2)  = ZEROS)
              AND (WS-TOT-END-BAL (2)  = ZEROS)
              AND (WS-TOT-BAL-FWD (2)  = ZEROS)
              AND (WS-TOT-ADJS (2)     = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE WS-HOLD-CAR         TO DTL1-CAR
              MOVE WS-HOLD-GRP         TO DTL1-GRP
              MOVE WS-HOLD-ST          TO DTL1-ST
              MOVE SPACES              TO DTL1-ACCT
              MOVE WS-TOT-BAL-FWD (2)  TO DTL1-BEG-BAL
              MOVE WS-TOT-EXP-PREM (2) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (2) TO DTL1-REC-PREM
              MOVE WS-TOT-ADJS (2)     TO DTL1-ADJS
              MOVE WS-TOT-END-BAL (2)  TO DTL1-END-BAL
              MOVE WS-TOT-DUE-COMM (2) TO DTL1-COMM
              MOVE DETAIL-LINE         TO P-DATA
              MOVE SPACE-2             TO P-CTL
              PERFORM VARYING WS-A FROM +1 BY +1 UNTIL
                 (WS-FREE-STATE (WS-A) = LOW-VALUES)
                 OR (WS-FREE-STATE (WS-A) = WS-HOLD-ST)
                 OR (WS-A > +55)
              END-PERFORM
              IF WS-A NOT > +55
                 MOVE WS-HOLD-ST       TO WS-FREE-STATE (WS-A)
                 COMPUTE WS-FREE-PREM (WS-A) = WS-FREE-PREM (WS-A)
                    + WS-TOT-END-BAL (2)
                 COMPUTE WS-FREE-COMM (WS-A) = WS-FREE-COMM (WS-A)
                    + WS-TOT-DUE-COMM (2)
              ELSE
                 DISPLAY ' FREEDOM TABLE EXCEEDED '
                 PERFORM ABEND-PGM
              END-IF
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

              COMPUTE WS-TOT-BAL-FWD (3)  = WS-TOT-BAL-FWD (3)
                 + WS-TOT-BAL-FWD (2)
              COMPUTE WS-TOT-EXP-PREM (3) = WS-TOT-EXP-PREM (3)
                 + WS-TOT-EXP-PREM (2)
              COMPUTE WS-TOT-REC-PREM (3) = WS-TOT-REC-PREM (3)
                 + WS-TOT-REC-PREM (2)
              COMPUTE WS-TOT-BASE-COMM  (3) = WS-TOT-BASE-COMM  (3)
                 + WS-TOT-BASE-COMM  (2)
              COMPUTE WS-TOT-GA-COMM (3) = WS-TOT-GA-COMM (3)
                 + WS-TOT-GA-COMM (2)
              COMPUTE WS-TOT-ADJS (3)     = WS-TOT-ADJS (3)
                 + WS-TOT-ADJS (2)
              COMPUTE WS-TOT-END-BAL (3)  = WS-TOT-END-BAL (3)
                 + WS-TOT-END-BAL (2)
              COMPUTE WS-TOT-DUE-COMM (3) = WS-TOT-DUE-COMM (3)
                 + WS-TOT-DUE-COMM (2)
           END-IF
              
           MOVE +0                     TO WS-TOT-EXP-PREM (2)
                                          WS-TOT-REC-PREM (2)
                                          WS-TOT-BASE-COMM (2)
                                          WS-TOT-GA-COMM  (2)
                                          WS-TOT-BAL-FWD  (2)
                                          WS-TOT-ADJS     (2)
                                          WS-TOT-END-BAL  (2)
                                          WS-TOT-DUE-COMM (2)
              
           .
       0310-EXIT.
           EXIT.
           
       0320-GRP-BREAK.

           IF (WS-TOT-EXP-PREM (3) = ZEROS)
              AND (WS-TOT-REC-PREM (3) = ZEROS)
              AND (WS-TOT-GA-COMM (3)  = ZEROS)
              AND (WS-TOT-END-BAL (3)  = ZEROS)
              AND (WS-TOT-BAL-FWD (3)  = ZEROS)
              AND (WS-TOT-ADJS (3)     = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE WS-HOLD-CAR         TO DTL1-CAR
              MOVE WS-HOLD-GRP         TO DTL1-GRP
              MOVE SPACES              TO DTL1-ST
              MOVE SPACES              TO DTL1-ACCT
              MOVE WS-TOT-BAL-FWD (3)  TO DTL1-BEG-BAL
              MOVE WS-TOT-EXP-PREM (3) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (3) TO DTL1-REC-PREM
              MOVE WS-TOT-ADJS (3)     TO DTL1-ADJS
              MOVE WS-TOT-END-BAL (3)  TO DTL1-END-BAL
              MOVE WS-TOT-DUE-COMM (3) TO DTL1-COMM
              MOVE DETAIL-LINE         TO P-DATA
              MOVE SPACE-2             TO P-CTL
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

              COMPUTE WS-TOT-BAL-FWD (4)  = WS-TOT-BAL-FWD (4)
                 + WS-TOT-BAL-FWD (3)
              COMPUTE WS-TOT-EXP-PREM (4) = WS-TOT-EXP-PREM (4)
                 + WS-TOT-EXP-PREM (3)
              COMPUTE WS-TOT-REC-PREM (4) = WS-TOT-REC-PREM (4)
                 + WS-TOT-REC-PREM (3)
              COMPUTE WS-TOT-BASE-COMM  (4) = WS-TOT-BASE-COMM  (4)
                 + WS-TOT-BASE-COMM  (3)
              COMPUTE WS-TOT-GA-COMM (4) = WS-TOT-GA-COMM (4)
                 + WS-TOT-GA-COMM (3)
              COMPUTE WS-TOT-ADJS (4)     = WS-TOT-ADJS (4)
                 + WS-TOT-ADJS (3)
              COMPUTE WS-TOT-END-BAL (4)  = WS-TOT-END-BAL (4)
                 + WS-TOT-END-BAL (3)
              COMPUTE WS-TOT-DUE-COMM (4) = WS-TOT-DUE-COMM (4)
                 + WS-TOT-DUE-COMM (3)
           END-IF
              
           MOVE +0                     TO WS-TOT-EXP-PREM (3)
                                          WS-TOT-REC-PREM (3)
                                          WS-TOT-BASE-COMM (3)
                                          WS-TOT-GA-COMM  (3)
                                          WS-TOT-BAL-FWD  (3)
                                          WS-TOT-ADJS     (3)
                                          WS-TOT-END-BAL  (3)
                                          WS-TOT-DUE-COMM (3)
              
           .
       0320-EXIT.
           EXIT.
           
       0330-CAR-BREAK.

           IF (WS-TOT-EXP-PREM (4) = ZEROS)
              AND (WS-TOT-REC-PREM (4) = ZEROS)
              AND (WS-TOT-GA-COMM (4)  = ZEROS)
              AND (WS-TOT-END-BAL (4)  = ZEROS)
              AND (WS-TOT-BAL-FWD (4)  = ZEROS)
              AND (WS-TOT-ADJS (4)     = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE WS-HOLD-CAR         TO DTL1-CAR
              MOVE SPACES              TO DTL1-GRP
              MOVE SPACES              TO DTL1-ST
              MOVE SPACES              TO DTL1-ACCT
              MOVE WS-TOT-BAL-FWD (4)  TO DTL1-BEG-BAL
              MOVE WS-TOT-EXP-PREM (4) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (4) TO DTL1-REC-PREM
              MOVE WS-TOT-ADJS (4)     TO DTL1-ADJS
              MOVE WS-TOT-END-BAL (4)  TO DTL1-END-BAL
              MOVE WS-TOT-DUE-COMM (4) TO DTL1-COMM
              MOVE DETAIL-LINE         TO P-DATA
              MOVE SPACE-2             TO P-CTL
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

              COMPUTE WS-TOT-BAL-FWD (5)  = WS-TOT-BAL-FWD (5)
                 + WS-TOT-BAL-FWD (4)
              COMPUTE WS-TOT-EXP-PREM (5) = WS-TOT-EXP-PREM (5)
                 + WS-TOT-EXP-PREM (4)
              COMPUTE WS-TOT-REC-PREM (5) = WS-TOT-REC-PREM (5)
                 + WS-TOT-REC-PREM (4)
              COMPUTE WS-TOT-BASE-COMM  (5) = WS-TOT-BASE-COMM  (5)
                 + WS-TOT-BASE-COMM  (4)
              COMPUTE WS-TOT-GA-COMM (5) = WS-TOT-GA-COMM (5)
                 + WS-TOT-GA-COMM (4)
              COMPUTE WS-TOT-ADJS (5)     = WS-TOT-ADJS (5)
                 + WS-TOT-ADJS (4)
              COMPUTE WS-TOT-END-BAL (5)  = WS-TOT-END-BAL (5)
                 + WS-TOT-END-BAL (4)
              COMPUTE WS-TOT-DUE-COMM (5) = WS-TOT-DUE-COMM (5)
                 + WS-TOT-DUE-COMM (4)
           END-IF
              
           MOVE +0                     TO WS-TOT-EXP-PREM (4)
                                          WS-TOT-REC-PREM (4)
                                          WS-TOT-BASE-COMM (4)
                                          WS-TOT-GA-COMM  (4)
                                          WS-TOT-BAL-FWD  (4)
                                          WS-TOT-ADJS     (4)
                                          WS-TOT-END-BAL  (4)
                                          WS-TOT-DUE-COMM (4)
              
           .
       0330-EXIT.
           EXIT.
           
       0340-FIN-BREAK.

           DISPLAY ' MADE IT TO 0340  FIN BREAK '

           IF (WS-TOT-EXP-PREM (5) = ZEROS)
              AND (WS-TOT-REC-PREM (5) = ZEROS)
              AND (WS-TOT-GA-COMM (5)  = ZEROS)
              AND (WS-TOT-END-BAL (5)  = ZEROS)
              AND (WS-TOT-BAL-FWD (5)  = ZEROS)
              AND (WS-TOT-ADJS (5)     = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE SPACES              TO DTL1-CAR
              MOVE SPACES              TO DTL1-GRP
              MOVE SPACES              TO DTL1-ST
              MOVE SPACES              TO DTL1-ACCT
              MOVE WS-TOT-BAL-FWD (5)  TO DTL1-BEG-BAL
              MOVE WS-TOT-EXP-PREM (5) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (5) TO DTL1-REC-PREM
              MOVE WS-TOT-ADJS (5)     TO DTL1-ADJS
              MOVE WS-TOT-END-BAL (5)  TO DTL1-END-BAL
              MOVE WS-TOT-DUE-COMM (5) TO DTL1-COMM
              MOVE DETAIL-LINE         TO P-DATA
              MOVE SPACE-2             TO P-CTL
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

           END-IF
              
           .
       0340-EXIT.
           EXIT.
           
       0350-CREATE-FREEDOM.

           MOVE SPACES                 TO WS-EXTRACT-RECORD
           MOVE 'LPACDUEPC'            TO FX-SYSTEM
           MOVE 'LOGIC '               TO FX-SOURCE-CODE
           MOVE '11'                   TO FX-DIVISION
           MOVE '15'                   TO FX-SUB-TYPE

           MOVE 'Y'                    TO FX-LOC-CODE
           
           MOVE WS-POSTING-DATE        TO FX-POSTING-DATE                          

           IF DTE-CLIENT = 'CID'
              MOVE 'S'                 TO FX-FY-REN
           ELSE
              MOVE ' '                 TO FX-FY-REN
           END-IF

           MOVE ZEROS                  TO FX-AMOUNT
                                                                        
           PERFORM VARYING WS-A FROM +1 BY +1 UNTIL
              (WS-A > +55)
              OR (WS-FREE-STATE (WS-A) = LOW-VALUES)
              MOVE WS-FREE-STATE (WS-A)
                                       TO FX-STATE
              MOVE 'DUE PREMIUM '      TO FX-DESCRIPTION
              MOVE WS-FREE-PREM (WS-A) TO FX-AMOUNT
              MOVE '30'                TO FX-TRAN-TYPE
              WRITE FREEDOM-RECORD     FROM WS-EXTRACT-RECORD
              MOVE 'DUE COMMISSION'    TO FX-DESCRIPTION
              MOVE WS-FREE-COMM (WS-A) TO FX-AMOUNT
              MOVE '50'                TO FX-TRAN-TYPE
              WRITE FREEDOM-RECORD     FROM WS-EXTRACT-RECORD
           END-PERFORM
           
           .
       0350-EXIT.
           EXIT.

       2900-DISPLAY-AMTS.

      *    IF (CR-DT > 19921231) AND
      *       (CR-DT < 19940101) AND
      *    IF (WS-BIN-VAL-DATES (VYR) = X'95FF')
      *       MOVE CR-DT TO WS-DISPLAY-DATE
      *       MOVE CR-LF-TERM TO WS-DISPLAY-TERM
      *       MOVE LF-REM-TRM2 TO WS-DISPLAY-RTERM
      *       MOVE CR-LFPRM TO WS-DISPLAY-PRM
      *       IF CLAS-I-EP (CLAS-INDEXL) EQUAL 'B' OR 'K' OR 'L'
      *          ADD CR-LFAMT CR-LFAMT-ALT GIVING WS-DISPLAY-AMT
      *       ELSE
      *          MOVE CR-LFAMT     TO WS-DISPLAY-AMT
      *       END-IF
      *       MOVE WS-REM-AMT   TO WS-DISPLAY-RAMT
      *       DISPLAY '  ' CR-CERT-NO '  ' WS-DISPLAY-DATE '   '
      *        WS-DISPLAY-TERM '  ' WS-DISPLAY-RTERM '   '
      *        WS-DISPLAY-AMT '   ' WS-DISPLAY-RAMT
      *    END-IF

           .
       2999-CALC-REM-AMT-X.
           EXIT.

       8600-HD-RTN.
           MOVE +0                     TO LNCTR
           MOVE HD1                    TO  P-DATA
           MOVE SPACE-NP               TO  P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           ADD +1                      TO  PGCTR
                                                
           MOVE PGCTR                  TO  HD-PG
           MOVE HD2                    TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE HD3                    TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE SPACE-2                TO  P-CTL
           MOVE HD4                    TO  P-DATA
                                                
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE HD5                    TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE SPACES                 TO  P-DATA
           MOVE SPACE-1                TO  P-CTL
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT
                                                
           MOVE +10                    TO  LNCTR
      *    MOVE SPACE-2                TO  P-CTL

           .
       8699-EXIT.
           EXIT.

       8800-PRT-RTN.

           IF LNCTR > +56
              MOVE PRT                 TO WS-HOLD-PRT
              PERFORM 8600-HD-RTN      THRU 8699-EXIT
              MOVE WS-HOLD-PRT         TO PRT
           END-IF

           MOVE P-CTL                  TO X
           IF P-CTL = SPACE-1
              ADD +1                   TO LNCTR
           ELSE
              IF P-CTL = SPACE-2
                 ADD +2                TO LNCTR
              ELSE
                 IF P-CTL = SPACE-3
                    ADD +3             TO LNCTR
                 END-IF
              END-IF
           END-IF
            
           .
       8850-COPY-PRT-RTN.
                                       COPY ELCPRT2.
                         
       8800-EXIT.        
           EXIT.         

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

