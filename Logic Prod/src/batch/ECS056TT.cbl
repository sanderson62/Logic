       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ECS056.
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE.
       DATE-COMPILED.

      *REMARKS.
      *    THIS PROGRAM READS THE MONTH END CERT FILE AND CLMS FILE
      *    AND BUILDS AN EXTRACT BY ISSUE YEAR.  THE EXTRACT IS
      *    PRIMARLIY USED BY ACTUARIAL.  THE REFUNDS SELECTED ARE
      *    PLACED IN THE VALUATION YEAR BASED ON THE CANCEL DATE.
      *    THE CLAIMS SELECTED ARE PLACED IN THE VALUATION YEAR
      *    BASED ON INCURRED DATE.  THE LOWEST ISSUE YEAR WILL ALWAYS
      *    BE 1986.  IF YOU NEED TO CHANGE THAT SIMPLY CHANGE THE
      *    HARD CODING WHERE YOU FIND MOVE 1986 TO LO-ISSUE AND
      *    LO-VALUATION.  ANY CHANGES MADE TO THE SPECIAL ECS083
      *    MUST ALSO BE MADE IN THIS PROGRAM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERTS        ASSIGN TO SYS010.
           SELECT EXTRACT      ASSIGN TO SYS011
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DISK-DATE    ASSIGN TO SYS019.
           SELECT PRINTX       ASSIGN TO SYS008.

       DATA DIVISION.
       FILE SECTION.


       EJECT
       FD  CERTS
                                       COPY ECSCRIFD.
                                       COPY ECSCRT01.

           EJECT
       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

      *         167 BYTES
       01  EXTRACT-RECORD.
           05  EXT-CAR                 PIC X.   
           05  EXT-GROUP               PIC X(6).
           05  EXT-STATE               PIC XX.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-EFF-DATE            PIC X(10).
           05  EXT-CERT-NO             PIC X(11).
           05  EXT-LF-BEN-CODE         PIC XX.
           05  EXT-AH-BEN-CODE         PIC XX.
           05  EXT-LF-TERM             PIC ZZ9.
           05  EXT-AH-TERM             PIC ZZ9.
           05  EXT-LF-EXP-DATE         PIC X(10).
           05  EXT-AH-EXP-DATE         PIC X(10).
           05  EXT-EXP-PREM            PIC ZZ,ZZZ,ZZ9.99.
           05  EXT-REC-PREM            PIC ZZ,ZZZ,ZZ9.99.
           05  EXT-GA-COMM             PIC ZZZ,ZZ9.99.
           05  EXT-MISC1               PIC ZZZ,ZZZ,ZZ9.99.
           05  EXT-MISC2               PIC ZZZ,ZZZ,ZZ9.99.
           05  EXT-MISC3               PIC ZZZ,ZZZ,ZZ9.99.
           05  EXT-MISC4               PIC ZZZ,ZZZ,ZZ9.99.
           05  EXT-MISC5               PIC ZZZ,ZZZ,ZZ9.99.


       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  PRINTX
                                       COPY ELCPRTFD.
           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '      ECS056  WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-CERT                VALUE 'Y'.
           88  MORE-CERTS                 VALUE 'N'.
       77  IYR                         PIC S9(03) COMP-3 VALUE +0.
       77  VYR                         PIC S9(03) COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  SPACE-NP                    PIC X      VALUE '1'.
       77  SPACE-1                     PIC X      VALUE ' '.
       77  SPACE-2                     PIC X      VALUE '0'.
       77  SPACE-3                     PIC X      VALUE '-'.
       77  X                           PIC X      VALUE ' '.

       01  HD1.                                                         ECS064
           12  FILLER              PIC  X(44)      VALUE SPACES.        ECS064
           12  FILLER              PIC  X(44)      VALUE                ECS064
                   '    DUE PREMIUM AND COMMISSION             '.
           12  FILLER              PIC  X(31)      VALUE SPACES.        ECS064
           12  FILLER              PIC  X(08)      VALUE 'ECS056'.      ECS064
                                                                        ECS064
       01  HD2.                                                         ECS064
           12  FILLER              PIC  X(51)      VALUE SPACES.        ECS064
           12  HD-CO               PIC  X(30).                          ECS064
           12  FILLER              PIC  X(38)      VALUE SPACES.        ECS064
           12  HD-RUN-DT           PIC  X(08)      VALUE SPACES.        ECS064
                                                                        ECS064
       01  HD3.                                                         ECS064
           12  FILLER              PIC  X(57)      VALUE SPACES.        ECS064
           12  HD-DT               PIC  X(18).                          ECS064
           12  FILLER              PIC  X(44)      VALUE SPACES.        ECS064
           12  FILLER              PIC  X(05)      VALUE 'PAGE '.       ECS064
           12  HD-PG               PIC ZZ,ZZ9.                          ECS064
                                                                        ECS064
       01  HD4.                                                         ECS064
           12  FILLER              PIC  X(44)      VALUE                ECS064
                   '-- ACCOUNT NUMBERS --                BEGINNI'.      ECS064
           12  FILLER              PIC  X(44)      VALUE                ECS064
                   'NG      CURRENT      PAYMENTS       ENDING  '.      ECS064
           12  FILLER              PIC  X(44)      VALUE                ECS064
                   '  ---------------- A G I N G ---------------'.      ECS064
                                                                        ECS064
       01  HD5.                                                         ECS064
           12  FILLER              PIC  X(44)      VALUE                ECS064
                   '-------- ACCOUNT NAME --------        BALANC'.      ECS064
           12  FILLER              PIC  X(44)      VALUE                ECS064
                   'E       CHARGES     ADJUSTMENTS     BALANCE '.      ECS064
           12  FILLER              PIC  X(44)      VALUE                ECS064
                   '      OVER 30       OVER 60       OVER 90   '.      ECS064
       01  DETAIL-LINE.
           05  FILLER                  PIC X(5).
           05  DTL1-CAR                PIC X.
           05  FILLER                  PIC X.
           05  DTL1-GRP                PIC X(6).
           05  FILLER                  PIC X.
           05  DTL1-ST                 PIC XX.
           05  FILLER                  PIC X.
           05  DTL1-ACCT               PIC X(10).
           05  FILLER                  PIC X.
           05  DTL1-BEG-BAL            PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-EXP-PREM           PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-REC-PREM           PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-ADJS               PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-END-BAL            PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-DUE-LT90           PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-DUE-GT90           PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-COMM               PIC ZZZ,ZZ9.99.
           05  FILLER                  PIC X.
           05  DTL1-NO-MOS             PIC ZZ9.
       EJECT                                                            ECS064
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
               10  WS-GA-COMM          PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC1            PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC2            PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC3            PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC4            PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-MISC5            PIC S9(7)V99 COMP-3 VALUE +0.

       01  WS-MISC.
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

       01  TEXAS-REGS-WORK-AREAS.
           12  TEX-FACT-1              PIC S9(7)V9(2)    COMP-3.
           12  TEX-FACT-2              PIC S9(3)   COMP-3.
           12  TEX-FACT-3              PIC S9(3)   COMP-3.
           12  TEX-FACT-4              PIC S9(7)   COMP-3.
           12  TEX-FACT-5              PIC S9(3)   COMP-3.
           12  TEX-FACT-6              PIC S9(3)   COMP-3.
           12  TEX-FACT-7              PIC S9(7)   COMP-3.
           12  TEX-FACT-8              PIC S9V9(6) COMP-3.
           12  TEX-FACT-9              PIC S9(4)V9(11)   COMP-3.

       01  NET-PAY-INTERFACE.
           05  NP-APR                  PIC S9(3)V9(4)    COMP-3.
           05  NP-ORIG                 PIC S9(3)         COMP-3.
           05  NP-REM                  PIC S9(3)         COMP-3.
           05  NP-OPT                  PIC X(01).
           05  NP-CAP                  PIC S9(3)         COMP-3.
           05  NP-FACTOR               PIC S9(4)V9(9)    COMP-3.
           05  NP-WORK1                PIC S9(6)V9(9)    COMP-3.
           05  NP-WORK2                PIC S9(6)V9(9)    COMP-3.

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
           05  WS-HI-ISSUE-CCYY        PIC 9(4) VALUE ZEROS.
           05  WS-LO-ISSUE-CCYY        PIC 9(4) VALUE ZEROS.
           05  WS-HI-VALUATION-CCYY    PIC 9(4) VALUE ZEROS.
           05  WS-LO-VALUATION-CCYY    PIC 9(4) VALUE ZEROS.
           05  WS-BIN-VAL-DATES OCCURS 50
                                       PIC XX.

       01  WS-MISC-AREA.
      *   1ST OCCURANCE           ACCOUNT TOTALS
      *   2ND                     STATE
      *   3RD                     GROUP
      *   4TH                     CARRIER
      *   5TH                     FINAL                
           12  WS-TOTAL-TABLE OCCURS 5.
               16  WS-TOT-EXP-PREM     PIC S9(9)V99 COMP-3 VALUE +0.
               16  WS-TOT-REC-PREM     PIC S9(9)V99 COMP-3 VALUE +0.
               16  WS-TOT-GA-COMM      PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-DISPLAY-FLD          PIC 9(9).9(5).
           12  WS-LF-PRM               PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-AH-PRM               PIC S9(7)V99 COMP-3  VALUE +0.
           12  CERT-IN-CNT             PIC 9(9)     VALUE ZEROS.
           12  WS-I                    PIC SV9(9)   COMP-3  VALUE +0.
           12  WS-J                    PIC SV9(9)   COMP-3  VALUE +0.
           12  WS-VM                   PIC S9V9(9)  COMP-3  VALUE +0.
           12  WS-P                    PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-B                    PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-R                    PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-I1                   PIC S9(7)V99 COMP-3  VALUE +0.
           12  X1                      PIC S999     COMP-3  VALUE +0.
           12  WS-A                    PIC S999     COMP-3  VALUE +0.
           12  WS-ANGLEN               PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-AMT                  PIC S9(9)V99 COMP-3  VALUE +0.
           12  WS-ANGLEM               PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-ANGLEMJ              PIC S9(7)V99 COMP-3  VALUE +0.
           12  WS-ANGLEN-M             PIC S9(7)V99 COMP-3  VALUE +0.
           12  LF-REM-TRM1             PIC S999V99  COMP-3  VALUE +0.
           12  AH-REM-TRM1             PIC S999V99  COMP-3  VALUE +0.
           12  LF-REM-TRM2             PIC S999V99  COMP-3  VALUE +0.
           12  AH-REM-TRM2             PIC S999V99  COMP-3  VALUE +0.
           12  REM-TRM1                PIC S9(3)V99    COMP-3.
           12  REM-TRM2                PIC S9(3)V99    COMP-3.
           12  WS-EARN-TERM            PIC 999              VALUE ZEROS.


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           EJECT
                                       COPY ELCCALC.
      /
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
           MOVE SPACES                 TO  SAVE-COMPANY-NAME
           MOVE ALPH-DATE              TO  HD-DT
           MOVE WS-CURRENT-DATE        TO  HD-RUN-DT
           MOVE SPACE-NP               TO  P-REC

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

           PERFORM VARYING WS-A FROM +1 BY +1 UNTIL
              (WS-A > +5)
              MOVE +0                  TO WS-TOT-EXP-PREM (WS-A)
                                          WS-TOT-REC-PREM (WS-A)
                                          WS-TOT-GA-COMM (WS-A)
           END-PERFORM
           
           .

       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT CERTS
               OUTPUT EXTRACT

           .

       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE CERTS
               EXTRACT

           .

       0030-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           PERFORM 0060-READ-CERT      THRU 0060-EXIT
           MOVE CR-ACCT-CONTROL        TO WS-HOLD-CONTROL
           PERFORM 0080-PROCESS-CERT   THRU 0080-EXIT UNTIL
                 END-OF-CERT

           .

       0050-EXIT.
           EXIT.

       0060-READ-CERT.

           READ CERTS AT END
               SET END-OF-CERT         TO TRUE
           END-READ

           IF NOT END-OF-CERT
              ADD +1                   TO CERT-IN-CNT
           END-IF
           .

       0060-EXIT.
           EXIT.

       0080-PROCESS-CERT.

           IF CR-ACCT-CONTROL NOT = WS-HOLD-CONTROL
              PERFORM 0250-BREAK-RTN   THRU 0250-EXIT
           END-IF

           MOVE CR-ACCT-CONTROL        TO WS-HOLD-CONTROL
           MOVE CR-DT                  TO WS-ISSUE-DATE

           IF (CR-ENTRY-STATUS = '9' OR 'D' OR 'V')
                       OR
              ((CR-LFTYP (1:1) NOT = 'M' AND 'N')
              AND (CR-AHTYP (1:1) NOT = 'M' AND 'N'))
              CONTINUE
           ELSE
              IF CR-ENTRY-STATUS NOT = '5'
                 PERFORM 0200-CALC-UEP THRU 0200-EXIT
              END-IF
           END-IF

           IF (WS-EXP-PREM > +0)
              OR (WS-REC-PREM > +0)
              OR (WS-GA-COMM > +0)
              PERFORM 0100-WRITE-EXTRACT
                                       THRU 0100-EXIT
           END-IF

           MOVE +0                     TO WS-EXP-PREM
                                          WS-REC-PREM
                                          WS-GA-COMM
                                          
           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .

       0080-EXIT.
           EXIT.
           
       0100-WRITE-EXTRACT.

           COMPUTE WS-TOT-EXP-PREM (1) = WS-TOT-EXP-PREM (1)
              + WS-EXP-PREM

           COMPUTE WS-TOT-REC-PREM (1) = WS-TOT-REC-PREM (1)
              + WS-REC-PREM

           COMPUTE WS-TOT-GA-COMM (1) = WS-TOT-GA-COMM (1)
              + WS-GA-COMM 

           MOVE CR-CARRIER             TO EXT-CAR
           MOVE CR-GROUPING            TO EXT-GROUP
           MOVE CR-STATE               TO EXT-STATE
           MOVE CR-ACCOUNT             TO EXT-ACCOUNT
           MOVE CR-DT                  TO WS-ISSUE-DATE
           MOVE WS-ISSUE-MMDD (1:2)    TO EXT-EFF-DATE (1:2)
           MOVE '-'                    TO EXT-EFF-DATE (3:1)
                                          EXT-EFF-DATE (6:1)
           MOVE WS-ISSUE-MMDD (3:2)    TO EXT-EFF-DATE (4:2)
           MOVE WS-ISSUE-CCYY          TO EXT-EFF-DATE (7:4)
           MOVE CR-CERT-NO             TO EXT-CERT-NO
           MOVE CR-LFTYP               TO EXT-LF-BEN-CODE
           MOVE CR-AHTYP               TO EXT-AH-BEN-CODE
           MOVE CR-LF-TERM             TO EXT-LF-TERM
           MOVE CR-AH-TERM             TO EXT-AH-TERM
           MOVE CR-LF-EXPIRE-DATE      TO WS-EXPIRE-DATE
           MOVE WS-EXPIRE-MMDD (1:2)   TO EXT-LF-EXP-DATE (1:2)
           MOVE '-'                    TO EXT-LF-EXP-DATE (3:1)
                                          EXT-LF-EXP-DATE (6:1)
           MOVE WS-EXPIRE-MMDD (3:2)   TO EXT-LF-EXP-DATE (4:2)
           MOVE WS-EXPIRE-CCYY         TO EXT-LF-EXP-DATE (7:4)

           MOVE CR-AH-EXPIRE-DATE      TO WS-EXPIRE-DATE
           MOVE WS-EXPIRE-MMDD (1:2)   TO EXT-AH-EXP-DATE (1:2)
           MOVE '-'                    TO EXT-AH-EXP-DATE (3:1)
                                          EXT-AH-EXP-DATE (6:1)
           MOVE WS-EXPIRE-MMDD (3:2)   TO EXT-AH-EXP-DATE (4:2)
           MOVE WS-EXPIRE-CCYY         TO EXT-AH-EXP-DATE (7:4)
           MOVE WS-EXP-PREM            TO EXT-EXP-PREM
           MOVE WS-REC-PREM            TO EXT-REC-PREM
           MOVE WS-GA-COMM             TO EXT-GA-COMM
           MOVE ZEROS                  TO EXT-MISC1
                                          EXT-MISC2
                                          EXT-MISC3
                                          EXT-MISC4
                                          EXT-MISC5
           
           WRITE EXTRACT-RECORD
           
           .

       0100-EXIT.
           EXIT.
           
       0200-CALC-UEP.

           IF CR-STATE = STATE-SUB (CLAS-INDEXS)
              CONTINUE
           ELSE
              PERFORM VARYING CLAS-INDEXS FROM +1 BY +1 UNTIL
                  (CLAS-INDEXS = CLAS-MAXS) OR
                  CR-STATE = STATE-SUB (CLAS-INDEXS)
              END-PERFORM
              IF CR-STATE = STATE-SUB (CLAS-INDEXS)
                 CONTINUE
              ELSE
                 DISPLAY 'STATE ' CR-STATE ' NOT IN TABLE'
                 MOVE 0302 TO WS-RETURN-CODE
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           MOVE DC-BIN-DATE-1          TO CP-CERT-EFF-DT
                                          WS-BIN-CR-DT

           IF CR-LOAN-1ST-PMT-DT NOT NUMERIC
              MOVE ZEROS               TO CR-LOAN-1ST-PMT-DT
           END-IF

           MOVE LOW-VALUES             TO CP-FIRST-PAY-DATE

           IF CR-LOAN-1ST-PMT-DT NOT = ZEROS
              MOVE CR-LOAN-1ST-PMT-DT  TO DC-GREG-DATE-1-YMD
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
              MOVE '3'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1   TO CP-FIRST-PAY-DATE
              ELSE
                 MOVE ZEROS           TO CR-LOAN-1ST-PMT-DT
              END-IF
            END-IF

           IF CP-FIRST-PAY-DATE  LESS THAN  CP-CERT-EFF-DT
              MOVE ZEROS  TO  CR-LOAN-1ST-PMT-DT
           END-IF

           IF CR-LOAN-1ST-PMT-DT = ZEROS
              MOVE CP-CERT-EFF-DT      TO DC-BIN-DATE-1
              MOVE +1                  TO DC-ELAPSED-MONTHS
              MOVE ZEROS               TO DC-ELAPSED-DAYS
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              MOVE DC-BIN-DATE-2       TO CP-FIRST-PAY-DATE
              MOVE DC-GREG-DATE-1-YMD  TO CR-LOAN-1ST-PMT-DT
           END-IF

           MOVE CR-LF-EXPIRE-DATE          TO WS-WORK-DATE

           IF (CR-LF-CANC-DT NOT = ZEROS) AND
              (CR-LF-CANC-DT < WS-WORK-DATE)
              MOVE CR-LF-CANC-DT           TO WS-WORK-DATE
           END-IF

           IF (CR-LF-CLAIM-EXIT-DATE NOT = ZEROS) AND
              (CR-LF-CLAIM-EXIT-DATE < WS-WORK-DATE)
              MOVE CR-LF-CLAIM-EXIT-DATE   TO WS-WORK-DATE
           END-IF

           MOVE WS-WORK-DATE           TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           MOVE DC-BIN-DATE-1          TO WS-BIN-LF-END-DATE

           MOVE CR-AH-EXPIRE-DATE          TO WS-WORK-DATE

           IF (CR-AH-CANC-DT NOT = ZEROS) AND
              (CR-AH-CANC-DT < WS-WORK-DATE)
              MOVE CR-AH-CANC-DT           TO WS-WORK-DATE
           END-IF

           IF (CR-AH-SETTLEMENT-EXIT-DATE NOT = ZEROS) AND
              (CR-AH-SETTLEMENT-EXIT-DATE < WS-WORK-DATE)
              MOVE CR-AH-SETTLEMENT-EXIT-DATE
                                           TO WS-WORK-DATE
           END-IF

           MOVE WS-WORK-DATE           TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           MOVE DC-BIN-DATE-1          TO WS-BIN-AH-END-DATE


           MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE
           MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV
           MOVE '3'                        TO CP-PROCESS-TYPE
           MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD
           MOVE DTE-CLIENT                 TO CP-COMPANY-ID
           MOVE SPACES                     TO CP-ACCT-FLD-5
           MOVE DTE-REM-TRM                TO CP-REM-TERM-METHOD

           IF CR-LFTYP = '00' OR '  '
              CONTINUE
           ELSE
              PERFORM 0210-CALC-LIFE-UEP   THRU 0210-EXIT
           END-IF

           .

       0200-EXIT.
           EXIT.

       0210-CALC-LIFE-UEP.

           IF CR-LFTYP = CLAS-I-BEN (CLAS-INDEXL)
              CONTINUE
           ELSE
              PERFORM VARYING CLAS-INDEXL FROM +1 BY +1 UNTIL
                  (CLAS-INDEXL = CLAS-MAXL) OR
                  (CR-LFTYP = CLAS-I-BEN (CLAS-INDEXL))
              END-PERFORM
              IF CR-LFTYP = CLAS-I-BEN (CLAS-INDEXL)
                 CONTINUE
              ELSE
                 DISPLAY 'LIFE BENEFIT ' CR-LFTYP ' NOT IN TABLE'
                 DISPLAY 'RETURN CODE - 0401'
                 MOVE 0401 TO WS-RETURN-CODE
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'
              CONTINUE
           ELSE
              PERFORM 0220-CALC-LF-REM-TERM
                                       THRU 0220-EXIT
              MOVE ZEROS               TO WS-REM-AMT
      *       IF LF-REM-TRM2 > ZERO
      *          PERFORM 2000-CALC-REM-AMT
      *                                THRU 2999-CALC-REM-AMT-X
      *       END-IF
      *       COMPUTE WS-LF-INFORCE = WS-LF-INFORCE + WS-REM-AMT
              IF CR-LFTYP = ('MG' OR 'MJ' OR 'NJ' OR 'NL')
                 IF CR-ENTRY-DATE = WS-TEST-RUN-DATE
                    PERFORM 0215-CALC-RECEIVE 
                                       THRU 0215-EXIT
                 END-IF
              ELSE
                 IF CR-LFTYP (1:1) = 'M'
                    COMPUTE WS-EXP-PREM = CR-LFPRM + CR-AHPRM
                 ELSE
                    PERFORM 0230-CALC-LF-UEP
                                       THRU 0230-EXIT
                 END-IF
              END-IF
           END-IF

           .

       0210-EXIT.
           EXIT.

       0215-CALC-RECEIVE.    

           MOVE +0                     TO WS-GA-COMM

           PERFORM VARYING X1 FROM +1 BY +1 UNTIL
              (X1 > +10)
              IF CR-AGT-TYPE (X1) = 'C' OR 'D'
                 COMPUTE WS-GA-COMM = WS-GA-COMM +
                    (CR-LFPRM * CR-LCOM-L (X1)) +
                    (CR-LFPRM-ALT * CR-LCOM-L (X1)) +
                    (CR-AHPRM * CR-LCOM-AH (X1))
              END-IF
           END-PERFORM
           
           COMPUTE WS-REC-PREM = (CR-LFPRM + CR-LFPRM-ALT + CR-AHPRM)
              - WS-GA-COMM
              
           .

       0215-EXIT.
           EXIT.

       0220-CALC-LF-REM-TERM.

           MOVE BIN-RUN-DATE           TO CP-VALUATION-DT

           MOVE CLAS-I-RL-AH (CLAS-INDEXL)
                                       TO CP-BENEFIT-TYPE
           MOVE CLAS-I-BAL (CLAS-INDEXL)
                                       TO CP-SPECIAL-CALC-CD
           MOVE CR-LF-TERM             TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM

           IF ((CLAS-I-EP (CLAS-INDEXL) EQUAL 'B' OR 'K' OR 'L') AND
              CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT EQUAL 'L')
              ADD +1                   TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM
           END-IF

           IF CP-TERM-IS-DAYS
              MOVE CR-LF-TERM-IN-DAYS  TO CP-TERM-OR-EXT-DAYS
           ELSE
              MOVE ZEROS               TO CP-TERM-OR-EXT-DAYS
           END-IF

           MOVE DTE-REM-TRM-CALC-OPTION
                                       TO CP-REM-TRM-CALC-OPTION

           PERFORM 0510-GET-REMAINING-TERM
                                       THRU 0510-EXIT

           IF ((CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND
                CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L')
      *       MOVE CP-REMAINING-TERM-2 TO LF-BAL-REMTERM
              COMPUTE CP-REMAINING-TERM-1 =
                             CP-REMAINING-TERM-1 - 1
              COMPUTE CP-REMAINING-TERM-2 =
                             CP-REMAINING-TERM-2 - 1
           END-IF

           IF CP-REMAINING-TERM-1 NEGATIVE
               MOVE ZEROS              TO CP-REMAINING-TERM-1
           END-IF

           IF CP-REMAINING-TERM-2 NEGATIVE
               MOVE ZEROS              TO CP-REMAINING-TERM-2
           END-IF

           MOVE CP-REMAINING-TERM-1    TO LF-REM-TRM1
           MOVE CP-REMAINING-TERM-2    TO LF-REM-TRM2

           .

       0220-EXIT.
           EXIT.

       0230-CALC-LF-UEP.
           
           DISPLAY ' MADE IT TO 0230 '
           COMPUTE WS-EARN-TERM = CR-LF-TERM - LF-REM-TRM2

           DISPLAY ' EARN TERM ' WS-EARN-TERM
           IF CR-LOAN-TERM NOT NUMERIC
              MOVE CR-LF-TERM          TO CR-LOAN-TERM
           END-IF
           IF CR-LOAN-TERM = ZEROS
              MOVE CR-LF-TERM          TO CR-LOAN-TERM
           END-IF
           COMPUTE WS-I = CR-APR / +1200

           MOVE WS-I                   TO WS-DISPLAY-FLD
           DISPLAY ' WS I ' WS-DISPLAY-FLD
           
           COMPUTE WS-J = WS-I + ((CR-LFPRM-RATE / +1000)
              + (CR-AHPRM-RATE / +1000))

           MOVE WS-J                   TO WS-DISPLAY-FLD
           DISPLAY ' WS J ' WS-DISPLAY-FLD

           COMPUTE WS-VM = (1 + WS-J) ** (CR-LF-TERM * -1)

           MOVE WS-VM                  TO WS-DISPLAY-FLD
           DISPLAY ' WS VM ' WS-DISPLAY-FLD
           
           COMPUTE WS-ANGLEMJ = (1 - ((1 + WS-J)
              ** (CR-LF-TERM * -1))) / WS-J

           MOVE WS-ANGLEMJ             TO WS-DISPLAY-FLD
           DISPLAY ' WS ANGLEMJ ' WS-DISPLAY-FLD
           COMPUTE WS-ANGLEN = (1 - ((1 + WS-I)
              ** (CR-LOAN-TERM * -1))) / WS-I

           MOVE WS-ANGLEN              TO WS-DISPLAY-FLD
           DISPLAY ' WS ANGLEN  ' WS-DISPLAY-FLD
           COMPUTE WS-ANGLEM = (1 - ((1 + WS-I)
              ** (CR-LF-TERM * -1))) / WS-I

           MOVE WS-ANGLEM              TO WS-DISPLAY-FLD
           DISPLAY ' WS ANGLEM  ' WS-DISPLAY-FLD
           COMPUTE WS-ANGLEN-M = (1 - ((1 + WS-I)
              ** ((CR-LOAN-TERM - CR-LF-TERM) * -1))) / WS-I

           MOVE WS-ANGLEN-M            TO WS-DISPLAY-FLD
           DISPLAY ' WS ANGLEN - M ' WS-DISPLAY-FLD
           COMPUTE WS-P = CR-LFAMT / WS-ANGLEN
           MOVE WS-P                   TO WS-DISPLAY-FLD
           DISPLAY ' WS P       ' WS-DISPLAY-FLD
           COMPUTE WS-B = WS-P * WS-ANGLEN-M
           MOVE WS-B                   TO WS-DISPLAY-FLD
           DISPLAY ' WS B       ' WS-DISPLAY-FLD
           COMPUTE WS-R = (CR-LFAMT - (WS-B * WS-VM))
              / WS-ANGLEM           
           MOVE WS-R                   TO WS-DISPLAY-FLD
           DISPLAY ' WS R       ' WS-DISPLAY-FLD

           MOVE CR-LFAMT               TO WS-AMT
           MOVE CR-LFPRM-RATE          TO WS-DISPLAY-FLD
           DISPLAY ' LF RATE ' WS-DISPLAY-FLD
           MOVE CR-AHPRM-RATE          TO WS-DISPLAY-FLD
           DISPLAY ' AH RATE ' WS-DISPLAY-FLD

           MOVE +0                     TO WS-LF-PRM
                                          WS-AH-PRM
           MOVE 2                      TO WS-EARN-TERM
           PERFORM VARYING WS-A FROM +1 BY +1 UNTIL
              (WS-A > WS-EARN-TERM)
              IF WS-A = +1
                 COMPUTE WS-I1 ROUNDED = (WS-AMT * (WS-I / +30
                    * (CR-PMT-EXTENSION-DAYS + +30)) * +100) / +100
              ELSE
                 COMPUTE WS-I1 ROUNDED = (WS-AMT * WS-I * +100)
                    / +100
              END-IF
              MOVE WS-I1               TO WS-DISPLAY-FLD
              DISPLAY ' ' WS-A '  WS-I1  ' WS-DISPLAY-FLD
              DISPLAY ' RUN DATE ' RUN-DATE
              DISPLAY ' ENTRY DATE ' CR-ENTRY-DATE
              IF RUN-DATE = CR-ENTRY-DATE
                 COMPUTE WS-LF-PRM = WS-LF-PRM
                    + (WS-AMT / +1000 * CR-LFPRM-RATE)
                 COMPUTE WS-AH-PRM = WS-AH-PRM
                    + (WS-AMT / +1000 * CR-AHPRM-RATE)
              ELSE
                 COMPUTE WS-LF-PRM = WS-AMT / +1000 * CR-LFPRM-RATE
                 COMPUTE WS-AH-PRM = WS-AMT / +1000 * CR-AHPRM-RATE
              END-IF
              COMPUTE WS-AMT ROUNDED = (.05 + (WS-AMT - (WS-R
                 - WS-I1 - (WS-LF-PRM + WS-AH-PRM))) * +100)
                 / +100
              IF WS-AMT < +0
                 COMPUTE WS-P = WS-P + WS-AMT
                 MOVE ZEROS            TO WS-AMT
              END-IF
              IF WS-A = CR-LOAN-TERM
                 IF WS-AMT > ZEROS
                    IF WS-R > WS-AMT
                       COMPUTE WS-R = WS-R - WS-AMT
                       MOVE ZEROS      TO WS-AMT
                    ELSE
                       MOVE CR-LFAMT-ALT TO WS-R
                       MOVE ZEROS      TO WS-AMT
                    END-IF
                 END-IF
              END-IF
           END-PERFORM

           COMPUTE WS-EXP-PREM = WS-LF-PRM + WS-AH-PRM              
           .

       0230-EXIT.
           EXIT.

       0250-BREAK-RTN.
       
           IF WS-HOLD-CAR NOT = CR-CARRIER
              PERFORM 0300-ACCT-BREAK  THRU 0300-EXIT
              PERFORM 0310-ST-BREAK    THRU 0310-EXIT
              PERFORM 0320-GRP-BREAK   THRU 0320-EXIT
              PERFORM 0340-CAR-BREAK   THRU 0340-EXIT
           ELSE
              IF WS-HOLD-GRP NOT = CR-GROUPING
                 PERFORM 0300-ACCT-BREAK
                                       THRU 0300-EXIT
                 PERFORM 0310-ST-BREAK THRU 0310-EXIT
                 PERFORM 0320-GRP-BREAK
                                       THRU 0320-EXIT
              ELSE
                 IF WS-HOLD-ST NOT = CR-STATE
                    PERFORM 0300-ACCT-BREAK
                                       THRU 0300-EXIT
                    PERFORM 0310-ST-BREAK
                                       THRU 0310-EXIT
                 ELSE
                    IF WS-HOLD-ACCT NOT = CR-ACCOUNT
                       PERFORM 0300-ACCT-BREAK
                                       THRU 0300-EXIT
                    END-IF
                 END-IF
              END-IF
           END-IF
                    
       0250-EXIT.
           EXIT.

       0300-ACCT-BREAK.
       
           IF (WS-TOT-EXP-PREM (1) = ZEROS)
              AND (WS-TOT-REC-PREM (1) = ZEROS)
              AND (WS-TOT-GA-COMM (1) = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE WS-HOLD-CAR         TO DTL1-CAR
              MOVE WS-HOLD-GRP         TO DTL1-GRP
              MOVE WS-HOLD-ST          TO DTL1-ST
              MOVE WS-HOLD-ACCT        TO DTL1-ACCT
              MOVE WS-TOT-EXP-PREM (1) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (1) TO DTL1-REC-PREM
              MOVE WS-TOT-GA-COMM (1)  TO DTL1-COMM
              MOVE DETAIL-LINE         TO PRT
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

              COMPUTE WS-TOT-EXP-PREM (2) = WS-TOT-EXP-PREM (2)
                 + WS-TOT-EXP-PREM (1)
              COMPUTE WS-TOT-REC-PREM (2) = WS-TOT-REC-PREM (2)
                 + WS-TOT-REC-PREM (1)
              COMPUTE WS-TOT-GA-COMM (2) = WS-TOT-GA-COMM (2)
                 + WS-TOT-GA-COMM (1)
           END-IF
           MOVE +0                     TO WS-TOT-EXP-PREM (1)
                                          WS-TOT-REC-PREM (1)
                                          WS-TOT-GA-COMM  (1)
              
           .
       0300-EXIT.
           EXIT.
           
       0310-ST-BREAK.
       
           IF (WS-TOT-EXP-PREM (2) = ZEROS)
              AND (WS-TOT-REC-PREM (2) = ZEROS)
              AND (WS-TOT-GA-COMM (2) = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE WS-HOLD-CAR         TO DTL1-CAR
              MOVE WS-HOLD-GRP         TO DTL1-GRP
              MOVE WS-HOLD-ST          TO DTL1-ST
              MOVE SPACES              TO DTL1-ACCT
              MOVE WS-TOT-EXP-PREM (2) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (2) TO DTL1-REC-PREM
              MOVE WS-TOT-GA-COMM (2)  TO DTL1-COMM
              MOVE DETAIL-LINE         TO PRT
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

              COMPUTE WS-TOT-EXP-PREM (3) = WS-TOT-EXP-PREM (3)
                 + WS-TOT-EXP-PREM (2)
              COMPUTE WS-TOT-REC-PREM (3) = WS-TOT-REC-PREM (3)
                 + WS-TOT-REC-PREM (2)
              COMPUTE WS-TOT-GA-COMM (3) = WS-TOT-GA-COMM (3)
                 + WS-TOT-GA-COMM (2)
           END-IF
              
           MOVE +0                     TO WS-TOT-EXP-PREM (2)
                                          WS-TOT-REC-PREM (2)
                                          WS-TOT-GA-COMM  (2)
              
           .
       0310-EXIT.
           EXIT.
           
       0320-GRP-BREAK.
       
           IF (WS-TOT-EXP-PREM (3) = ZEROS)
              AND (WS-TOT-REC-PREM (3) = ZEROS)
              AND (WS-TOT-GA-COMM (3) = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE WS-HOLD-CAR         TO DTL1-CAR
              MOVE WS-HOLD-GRP         TO DTL1-GRP
              MOVE SPACES              TO DTL1-ST
              MOVE SPACES              TO DTL1-ACCT
              MOVE WS-TOT-EXP-PREM (3) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (3) TO DTL1-REC-PREM
              MOVE WS-TOT-GA-COMM (3)  TO DTL1-COMM
              MOVE DETAIL-LINE         TO PRT
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

              COMPUTE WS-TOT-EXP-PREM (4) = WS-TOT-EXP-PREM (4)
                 + WS-TOT-EXP-PREM (3)
              COMPUTE WS-TOT-REC-PREM (4) = WS-TOT-REC-PREM (4)
                 + WS-TOT-REC-PREM (3)
              COMPUTE WS-TOT-GA-COMM (4) = WS-TOT-GA-COMM (4)
                 + WS-TOT-GA-COMM (3)
           END-IF
              
           MOVE +0                     TO WS-TOT-EXP-PREM (3)
                                          WS-TOT-REC-PREM (3)
                                          WS-TOT-GA-COMM  (3)
              
           .
       0320-EXIT.
           EXIT.
           
       0330-CAR-BREAK.
       
           IF (WS-TOT-EXP-PREM (4) = ZEROS)
              AND (WS-TOT-REC-PREM (4) = ZEROS)
              AND (WS-TOT-GA-COMM (4) = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE WS-HOLD-CAR         TO DTL1-CAR
              MOVE SPACES              TO DTL1-GRP
              MOVE SPACES              TO DTL1-ST
              MOVE SPACES              TO DTL1-ACCT
              MOVE WS-TOT-EXP-PREM (4) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (4) TO DTL1-REC-PREM
              MOVE WS-TOT-GA-COMM (4)  TO DTL1-COMM
              MOVE DETAIL-LINE         TO PRT
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

              COMPUTE WS-TOT-EXP-PREM (5) = WS-TOT-EXP-PREM (5)
                 + WS-TOT-EXP-PREM (4)
              COMPUTE WS-TOT-REC-PREM (5) = WS-TOT-REC-PREM (5)
                 + WS-TOT-REC-PREM (4)
              COMPUTE WS-TOT-GA-COMM (5) = WS-TOT-GA-COMM (5)
                 + WS-TOT-GA-COMM (4)
           END-IF
              
           MOVE +0                     TO WS-TOT-EXP-PREM (4)
                                          WS-TOT-REC-PREM (4)
                                          WS-TOT-GA-COMM  (4)
              
           .
       0330-EXIT.
           EXIT.
           
       0340-FIN-BREAK.
       
           IF (WS-TOT-EXP-PREM (5) = ZEROS)
              AND (WS-TOT-REC-PREM (5) = ZEROS)
              AND (WS-TOT-GA-COMM (5) = ZEROS)
              CONTINUE
           ELSE
              MOVE SPACES              TO DETAIL-LINE
              MOVE SPACES              TO DTL1-CAR
              MOVE SPACES              TO DTL1-GRP
              MOVE SPACES              TO DTL1-ST
              MOVE SPACES              TO DTL1-ACCT
              MOVE WS-TOT-EXP-PREM (5) TO DTL1-EXP-PREM
              MOVE WS-TOT-REC-PREM (5) TO DTL1-REC-PREM
              MOVE WS-TOT-GA-COMM (5)  TO DTL1-COMM
              MOVE DETAIL-LINE         TO PRT
              PERFORM 8800-PRT-RTN     THRU 8800-EXIT            

      *       COMPUTE WS-TOT-EXP-PREM (5) = WS-TOT-EXP-PREM (5)
      *          + WS-TOT-EXP-PREM (4)
      *       COMPUTE WS-TOT-REC-PREM (5) = WS-TOT-REC-PREM (5)
      *          + WS-TOT-REC-PREM (4)
      *       COMPUTE WS-TOT-GA-COMM (5) = WS-TOT-GA-COMM (5)
      *          + WS-TOT-GA-COMM (4)
           END-IF
              
           .
       0340-EXIT.
           EXIT.
           
       0420-CALC-AH-REM-TERM.

           MOVE WS-BIN-VAL-DATES (VYR) TO CP-VALUATION-DT

           MOVE CLAS-I-RL-AH (CLAS-INDEXA)
                                       TO CP-BENEFIT-TYPE
           MOVE CLAS-I-BAL (CLAS-INDEXA)
                                       TO CP-SPECIAL-CALC-CD
           MOVE CR-AH-TERM             TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM

           MOVE ZEROS                  TO CP-TERM-OR-EXT-DAYS

           MOVE DTE-REM-TRM-CALC-OPTION
                                       TO CP-REM-TRM-CALC-OPTION

           PERFORM 0510-GET-REMAINING-TERM
                                       THRU 0510-EXIT

           IF CP-REMAINING-TERM-1 NEGATIVE
               MOVE ZEROS              TO CP-REMAINING-TERM-1
           END-IF

           IF CP-REMAINING-TERM-2 NEGATIVE
               MOVE ZEROS              TO CP-REMAINING-TERM-2
           END-IF

           MOVE CP-REMAINING-TERM-1    TO AH-REM-TRM1
           MOVE CP-REMAINING-TERM-2    TO AH-REM-TRM2

           .

       0420-EXIT.
           EXIT.



       0510-GET-REMAINING-TERM.

           CALL 'ELRTRMX' USING CALCULATION-PASS-AREA

           .

       0510-EXIT.
           EXIT.


           .
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
       EJECT
       8600-HD-RTN.                                                     ECS064
           MOVE HD1                    TO  P-LINE.                      ECS064
           MOVE SPACE-NP               TO  P-CCSW.                      ECS064
                                                                        ECS064
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
                                                                        ECS064
           ADD +1                      TO  PGCTR.                       ECS064
                                                                        ECS064
           MOVE PGCTR                  TO  HD-PG.                       ECS064
           MOVE HD2                    TO  P-LINE.                      ECS064
           MOVE SPACE-1                TO  P-CCSW.                      ECS064
                                                                        ECS064
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
                                                                        ECS064
           MOVE HD3                    TO  P-LINE.                      ECS064
           MOVE SPACE-1                TO  P-CCSW.                      ECS064
                                                                        ECS064
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
                                                                        ECS064
           MOVE SPACE-2                TO  P-CCSW.                      ECS064
           MOVE 'COMPANY'              TO  P-MSG.                       ECS064
           MOVE CUR-CARR               TO  P-CARR.                      ECS064
           MOVE CUR-GROUP              TO  P-GROUP.                     ECS064
           MOVE '-'                    TO  P-DASH.                      ECS064
                                                                        ECS064
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
                                                                        ECS064
           MOVE SPACE-2                TO  P-CCSW.                      ECS064
           MOVE HD4                    TO  P-LINE.                      ECS064
                                                                        ECS064
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
                                                                        ECS064
           MOVE HD5                    TO  P-LINE.                      ECS064
                                                                        ECS064
           PERFORM 8800-PRT-RTN  THRU  8800-EXIT.                       ECS064
                                                                        ECS064
           MOVE +10                    TO  LNCTR.                       ECS064
           MOVE SPACE-2                TO  P-CCSW.                      ECS064
                                                                        ECS064
       8699-EXIT.                                                       ECS064
           EXIT.                                                        ECS064
                                                                        ECS064
       8800-PRT-RTN.                                                    ECS064

           IF LNCTR > +56
              PERFORM 8600-HD-RTN      THRU 8699-EXIT
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
                                                                        ECS064
                                                                        ECS064
       8850-COPY-PRT-RTN.                                               ECS064
                                       COPY ELCPRT2.
                                                                        ECS064
       8800-EXIT.                                                       ECS064
           EXIT.                                                        ECS064
       EJECT                                                            ECS064
       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
