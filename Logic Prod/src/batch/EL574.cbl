       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL574.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

      *             TTTTTTT     BBBBBBB     DDDDDD
      *                T        B      B    D     D
      *                T        B      B    D     D
      *                T        BBBBBBBB    D     D
      *                T        B      B    D     D
      *                T        B      B    D     D
      *                T        BBBBBBB     DDDDDD
                            

120816******************************************************************
120816*                   C H A N G E   L O G
120816*
120816* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120816*-----------------------------------------------------------------
120816*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120816* EFFECTIVE    NUMBER
120816*-----------------------------------------------------------------
120816* 120816  CR2016111600001  PEMA  Load ah stat uep
120121* 120121  IR2021120100001  PEMA  Increase size of rate table.
120816******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GAAP                 ASSIGN TO SYS010.

           SELECT ERRATE               ASSIGN TO ERRATE
              ORGANIZATION IS INDEXED
              ACCESS IS DYNAMIC
              RECORD KEY IS RT-CONTROL-PRIMARY
              FILE STATUS IS ERRATE-FILE-STATUS.

           SELECT EXTRACT              ASSIGN TO SYS011.

           SELECT DISK-DATE            ASSIGN TO SYS019.

           SELECT PRINTX               ASSIGN TO SYS008.

       DATA DIVISION.
       FILE SECTION.

       FD  GAAP
                                       COPY ECSGAPFD.
                                       COPY ECSGAP01.

       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  EXTRACT-RECORD-OUT          PIC X(320).

       FD  ERRATE.
                                       COPY ERCRATE.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  PRINTX
                                       COPY ELCPRTFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '       EL574  WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  WS-RATE-SW                  PIC X  VALUE SPACES.
           88  END-OF-RATES               VALUE 'Y'.
       77  T1                          PIC S999 VALUE +0 COMP-3.
       77  C1                          PIC S999  VALUE +0 COMP-3.
       77  IYR                         PIC S999   COMP-3 VALUE +0.
       77  VYR                         PIC S999   COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  WS-CRT-CNT                  PIC S9(9)  COMP-3 VALUE +0.
       77  EXTR-RECS-OUT               PIC S9(9)  COMP-3 VALUE +0.
       77  WS-ERRATE-IN                PIC 9(9)   VALUE ZEROS.
       77  WS-CRT-CNT-TOT              PIC S9(9)  COMP-3 VALUE +0.
       77  INTERMED                    PIC S9(9)V9(6)  COMP-3.
       77  ERRATE-FILE-STATUS          PIC XX   VALUE LOW-VALUES.
       77  WS-INIT-EXT-RECORD          PIC X(320)  VALUE LOW-VALUES.


       01  WS-NON-CREDIT-TABLES.
           12  WS-LIFE-TABLE.
               16  FILLER         PIC XX  VALUE 'QD'.
               16  FILLER         PIC XX  VALUE 'QL'.
               16  FILLER         PIC XX  VALUE '29'.
               16  FILLER         PIC XX  VALUE '32'.
               16  FILLER         PIC XX  VALUE '33'.
               16  FILLER         PIC XX  VALUE '38'.
               16  FILLER         PIC XX  VALUE '45'.
               16  FILLER         PIC XX  VALUE '46'.
               16  FILLER         PIC XX  VALUE '76'.
               16  FILLER         PIC XX  VALUE '77'.
               16  FILLER         PIC XX  VALUE '80'.
               16  FILLER         PIC XX  VALUE '81'.
           12  WS-LIFE REDEFINES WS-LIFE-TABLE OCCURS 12
                            INDEXED BY WS-LF-NDX.
               16  WS-LIFE-BEN   PIC XX.
           12  WS-AH-TABLE.
               16  FILLER         PIC XX  VALUE '38'.
               16  FILLER         PIC XX  VALUE '39'.
               16  FILLER         PIC XX  VALUE '40'.
               16  FILLER         PIC XX  VALUE '41'.
               16  FILLER         PIC XX  VALUE '42'.
               16  FILLER         PIC XX  VALUE '63'.
               16  FILLER         PIC XX  VALUE '64'.
               16  FILLER         PIC XX  VALUE '65'.
               16  FILLER         PIC XX  VALUE '66'.
               16  FILLER         PIC XX  VALUE '67'.
               16  FILLER         PIC XX  VALUE '2C'.
               16  FILLER         PIC XX  VALUE '2D'.
               16  FILLER         PIC XX  VALUE '2E'.
               16  FILLER         PIC XX  VALUE '2F'.
               16  FILLER         PIC XX  VALUE '2G'.
               16  FILLER         PIC XX  VALUE '2H'.
               16  FILLER         PIC XX  VALUE '2I'.
               16  FILLER         PIC XX  VALUE '2J'.
               16  FILLER         PIC XX  VALUE '2K'.
               16  FILLER         PIC XX  VALUE '2L'.
               16  FILLER         PIC XX  VALUE '2Q'.
               16  FILLER         PIC XX  VALUE '2R'.
               16  FILLER         PIC XX  VALUE '2S'.
               16  FILLER         PIC XX  VALUE '2T'.
               16  FILLER         PIC XX  VALUE '2U'.
               16  FILLER         PIC XX  VALUE '5U'.
               16  FILLER         PIC XX  VALUE '5V'.
               16  FILLER         PIC XX  VALUE '5W'.
               16  FILLER         PIC XX  VALUE '5X'.
               16  FILLER         PIC XX  VALUE '5Y'.
               16  FILLER         PIC XX  VALUE '5Z'.
               16  FILLER         PIC XX  VALUE '6A'.
               16  FILLER         PIC XX  VALUE '6B'.
               16  FILLER         PIC XX  VALUE '6C'.
               16  FILLER         PIC XX  VALUE '6D'.
               16  FILLER         PIC XX  VALUE '6E'.
               16  FILLER         PIC XX  VALUE '6F'.
               16  FILLER         PIC XX  VALUE '6G'.
               16  FILLER         PIC XX  VALUE '6H'.
               16  FILLER         PIC XX  VALUE '6I'.
           12  WS-AH REDEFINES WS-AH-TABLE OCCURS 40
                            INDEXED BY WS-AH-NDX.
               16  WS-AH-BEN     PIC XX.

       01  L1                          PIC S9(5) VALUE +0 COMP-3.
       01  ML1                         PIC S9(5) VALUE +0 COMP-3.
       01  FILLER.
           05  WS-LIFE-RATE-TABLE.
120121         10  WS-LIFE-RATES OCCURS 11000.
                   15  WSL-STATE-CODE.
                       20  WSL-ST-CODE  PIC XX.
                       20  WSL-ST-CLASS PIC XX.
                       20  WSL-ST-DEV   PIC XXX.
                   15  WSL-L-AH-CODE.
                       20  WSL-L-AH     PIC X.
                       20  WSL-LAH-NUM  PIC XX.
                   15  WSL-HIGH-AMT     PIC 9(9).
                   15  WSL-EXPIRY-DATE  PIC 9(11) COMP-3.
                   15  WSL-12-MO-RATE   PIC S99V9(5) COMP-3.
                   15  WSL-72-MO-RATE   PIC S99V9(5) COMP-3.
                   15  WSL-MOB-RATE     PIC S99V9(5) COMP-3.

       01  A1                          PIC S9(5) VALUE +0 COMP-3.
       01  MA1                         PIC S9(5) VALUE +0 COMP-3.
       01  FILLER.
           05  WS-AH-RATE-TABLE.
               10  WS-AH-RATES OCCURS 9000.
                   15  WSA-STATE-CODE.
                       20  WSA-ST-CODE  PIC XX.
                       20  WSA-ST-CLASS PIC XX.
                       20  WSA-ST-DEV   PIC XXX.
                   15  WSA-L-AH-CODE.
                       20  WSA-L-AH     PIC X.
                       20  WSA-LAH-NUM  PIC XX.
                   15  WSA-HIGH-AMT     PIC 9(9).
                   15  WSA-EXPIRY-DATE  PIC 9(11) COMP-3.
                   15  WSA-12-MO-RATE   PIC S99V9(5) COMP-3.
                   15  WSA-72-MO-RATE   PIC S99V9(5) COMP-3.

       01  WS-INFO-INIT                PIC X(67).

       01  WS-LIFE-INFO.
           05  WS-LF-DEV               PIC XXX.
           05  WS-LF-BEN-CODE          PIC XX.
           05  WS-LF-RATE-AMT          PIC 9(9).
           05  WS-LF-EXPIRE-DATE       PIC 9(8).
           05  WS-LF-UEP-R78           PIC S9(7)V99 COMP-3.
           05  WS-LF-UEP-PRO           PIC S9(7)V99 COMP-3.
           05  WS-LF-UEP-MEAN          PIC S9(7)V99 COMP-3.
           05  WS-LF-UEP-STAT          PIC S9(7)V99 COMP-3.
           05  WS-LF-REM-AMT           PIC S9(9)V99 COMP-3.
           05  WS-LF-MORT-RESV         PIC S9(9)V99 COMP-3.
           05  WS-LF-IBNR-RESV         PIC S9(9)V99 COMP-3.
           05  WS-LF-REM-TERM          PIC S9(9)    COMP-3.
           05  WS-LF-INF-COV           PIC 9.
           05  WS-LF-INF-CRT           PIC 9.

       01  WS-AH-INFO.
           05  WS-AH-DEV               PIC XXX.
           05  WS-AH-BEN-CODE          PIC XX.
           05  WS-AH-RATE-AMT          PIC 9(9).
           05  WS-AH-EXPIRE-DATE       PIC 9(8).
           05  WS-AH-UEP-R78           PIC S9(7)V99 COMP-3.
           05  WS-AH-UEP-PRO           PIC S9(7)V99 COMP-3.
           05  WS-AH-UEP-MEAN          PIC S9(7)V99 COMP-3.
           05  WS-AH-UEP-STAT          PIC S9(7)V99 COMP-3.
           05  WS-AH-REM-BEN           PIC S9(9)V99 COMP-3.
           05  WS-AH-MORT-RESV         PIC S9(9)V99 COMP-3.
           05  WS-AH-IBNR-RESV         PIC S9(9)V99 COMP-3.
           05  WS-AH-REM-TERM          PIC S9(9)    COMP-3.
           05  WS-AH-INF-COV           PIC 9.
           05  WS-AH-INF-CRT           PIC 9.

       01  WS-WORK-INIT                PIC X(320).

       01  WS-WORK-RECORD.
           05  OR-VAL-CCYYMM           PIC 9(6).
           05  OR-CARRIER              PIC X.
           05  OR-STATE                PIC XX.
           05  OR-ACCOUNT              PIC X(10).
           05  OR-CLASS                PIC XX.
           05  OR-DEVIATION            PIC XXX.
           05  OR-TYPE                 PIC X.
               88  OR-LIFE                 VALUE 'L'.
               88  OR-AH                   VALUE 'A'.
           05  OR-BEN-CODE             PIC XX.
           05  OR-RATE-FILE-AMT        PIC 9(9).
           05  OR-RATE-FILE-EXP-DT     PIC 9(8).
           05  OR-CREDIT-SW            PIC X.
               88  OR-CREDIT              VALUE '1'.
               88  OR-NON-CREDIT          VALUE '2'.
           05  OR-EFF-CCYY             PIC 9999.
           05  OR-TERM-GROUP           PIC 999.
           05  OR-12-MO-RATE           PIC S99V9(5) COMP-3.
           05  OR-72-MO-RATE           PIC S99V9(5) COMP-3.
           05  OR-MOB-RATE             PIC S99V9(5) COMP-3.
           05  FILLER OCCURS 4.
               10  OR-UEP-R78          PIC S9(11)V99 COMP-3.
               10  OR-UEP-PRO          PIC S9(11)V99 COMP-3.
               10  OR-UEP-MEAN         PIC S9(11)V99 COMP-3.
               10  OR-UEP-STAT         PIC S9(11)V99 COMP-3.
               10  OR-REM-BEN          PIC S9(11)V99 COMP-3.
               10  OR-MORT-RESV        PIC S9(11)V99 COMP-3.
               10  OR-IBNR-RESV        PIC S9(11)V99 COMP-3.
               10  OR-REM-TERM         PIC S9(9)     COMP-3.
               10  OR-INF-COV          PIC S9(9)     COMP-3.
               10  OR-INF-CRT          PIC S9(9)     COMP-3.

       01  WS-DISPLAY-DATE         PIC ZZZ9(8).
       01  WS-BIN-CR-DT            PIC XX  VALUE LOW-VALUES.
       01  WS-BIN-LF-END-DATE      PIC XX  VALUE LOW-VALUES.
       01  WS-BIN-AH-END-DATE      PIC XX  VALUE LOW-VALUES.
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

       01  FILLER.
           05  WS-CYE-DATE             PIC 9(11).
           05  FILLER REDEFINES WS-CYE-DATE.
               10  FILLER              PIC XXX.
               10  WS-CYE-CCYYMM       PIC 9(6).
               10  FILLER REDEFINES WS-CYE-CCYYMM.
                   15  WS-CYE-CCYY     PIC 9999.
                   15  WS-CYE-MM       PIC 99.
               10  WS-CYE-DD           PIC 99.
       01  FILLER.
           05  WS-PYE-DATE             PIC 9(11).
           05  FILLER REDEFINES WS-PYE-DATE.
               10  FILLER              PIC XXX.
               10  WS-PYE-CCYYMM       PIC 9(6).
               10  FILLER REDEFINES WS-PYE-CCYYMM.
                   15  WS-PYE-CCYY     PIC 9999.
                   15  WS-PYE-MM       PIC 99.
               10  WS-PYE-DD           PIC 99.
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

       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCCALC.
                                       COPY ELCDATE.
                                       COPY ELCFUNDT.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH             PIC S9(4)  COMP.
           05  PARM-VALUE              PIC X(100).

       PROCEDURE DIVISION USING PARM.

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' TOTAL GAAP RECORDS READ ' WS-CRT-CNT-TOT
           DISPLAY ' TOTAL EXTR RECORDS OUT  ' EXTR-RECS-OUT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT GAAP ERRATE
               OUTPUT EXTRACT

           IF ERRATE-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERRATE - ERROR - OPEN ' ERRATE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           IF PARM-LENGTH = +0
              DISPLAY ' ERROR - PARM - MISSING '
              PERFORM ABEND-PGM
           END-IF

           EVALUATE PARM-VALUE (1:3)
              WHEN 'YTD'
                 MOVE +1               TO T1
                 DISPLAY ' BEGIN PROCESS YTD '
              WHEN 'MTD'
                 MOVE +2               TO T1
                 DISPLAY ' BEGIN PROCESS MTD '
              WHEN 'L12'
                 MOVE +3               TO T1
                 DISPLAY ' BEGIN PROCESS L12 '
              WHEN 'ITD'
                 MOVE +4               TO T1
                 DISPLAY ' BEGIN PROCESS ITD '
              WHEN OTHER
                 DISPLAY ' ERROR - PARM - INVALID ' PARM-VALUE
                 PERFORM ABEND-PGM
           END-EVALUATE

           MOVE RUN-DATE               TO WS-CYE-DATE
                                          WS-PYE-DATE

           SUBTRACT +100               FROM WS-PYE-CCYYMM

           DISPLAY ' CURRENT YEAR END DATE ' WS-CYE-DATE
           DISPLAY '   PRIOR YEAR END DATE ' WS-PYE-DATE

           INITIALIZE WS-LIFE-INFO
           MOVE WS-LIFE-INFO           TO WS-AH-INFO
                                          WS-INFO-INIT
           INITIALIZE WS-WORK-RECORD
           MOVE WS-WORK-RECORD         TO WS-WORK-INIT

           PERFORM 0021-START-ERRATE   THRU 0021-EXIT
           PERFORM 0022-READ-ERRATE    THRU 0022-EXIT
           PERFORM 0025-LOAD-RATES     THRU 0025-EXIT UNTIL
              END-OF-RATES

           MOVE A1                     TO MA1
           MOVE L1                     TO ML1
           DISPLAY ' LIFE RATES ' ML1
           DISPLAY '   AH RATES ' MA1
           PERFORM 0060-READ-GAAP      THRU 0060-EXIT

           .
       0020-EXIT.
           EXIT.

       0021-START-ERRATE.

           MOVE LOW-VALUES             TO RT-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO RT-COMPANY-CD
           START ERRATE KEY IS >= RT-CONTROL-PRIMARY
           IF ERRATE-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERRROR - ERRATE - START ' ERRATE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0021-EXIT.
           EXIT.

       0022-READ-ERRATE.

           READ ERRATE NEXT RECORD
           
           IF (ERRATE-FILE-STATUS = '10' OR '23')
              OR (DTE-CLASIC-COMPANY-CD NOT = RT-COMPANY-CD)
              SET END-OF-RATES         TO TRUE
           ELSE
              IF ERRATE-FILE-STATUS = '00'
                 ADD 1                 TO WS-ERRATE-IN
              ELSE
                 DISPLAY ' ERROR - ERRATE - READNEXT '
                    ERRATE-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0022-EXIT.
           EXIT.

       0025-LOAD-RATES.

           IF RT-STATE-CODE NOT = 'BB'
              IF RT-L-AH = 'L'
                 ADD +1                TO L1
                 MOVE RT-STATE-CODE    TO WSL-STATE-CODE  (L1)
                 MOVE RT-L-AH-CODE     TO WSL-L-AH-CODE   (L1)
                 MOVE RT-HIGH-AMT      TO WSL-HIGH-AMT    (L1)
                 MOVE RT-EXPIRY-DATE   TO WSL-EXPIRY-DATE (L1)
                 MOVE RT-L-RATE (12)   TO WSL-12-MO-RATE  (L1)
                 MOVE RT-L-RATE (72)   TO WSL-72-MO-RATE  (L1)
                 IF RT-DISCOUNT-OB-RATE NOT NUMERIC
                    MOVE ZEROS         TO RT-DISCOUNT-OB-RATE
                 END-IF
                 MOVE RT-DISCOUNT-OB-RATE
                                       TO WSL-MOB-RATE    (L1)
              ELSE
                 ADD +1                TO A1
                 MOVE RT-STATE-CODE    TO WSA-STATE-CODE  (A1)
                 MOVE RT-L-AH-CODE     TO WSA-L-AH-CODE   (A1)
                 MOVE RT-HIGH-AMT      TO WSA-HIGH-AMT    (A1)
                 MOVE RT-EXPIRY-DATE   TO WSA-EXPIRY-DATE (A1)
                 MOVE RT-AH-RATE (12)  TO WSA-12-MO-RATE  (A1)
                 MOVE RT-AH-RATE (72)  TO WSA-72-MO-RATE  (A1)
              END-IF
           END-IF

           PERFORM 0022-READ-ERRATE    THRU 0022-EXIT

           .
       0025-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE GAAP EXTRACT ERRATE

           IF ERRATE-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERRATE - ERROR - CLOSE ' ERRATE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           IF GR-REIN = 'P'
              PERFORM 0080-PROCESS-GAAP
                                      THRU 0080-EXIT
           END-IF

           PERFORM 0060-READ-GAAP      THRU 0060-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-READ-GAAP.

           READ GAAP AT END
               SET END-OF-INPUT        TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-CRT-CNT
                                          WS-CRT-CNT-TOT
              IF WS-CRT-CNT = +10000
                 DISPLAY ' GAAPS READ ' WS-CRT-CNT-TOT
                 MOVE +0               TO WS-CRT-CNT
              END-IF
              IF GR-CERT-NO = '0000109489 '
                 DISPLAY ' FOUND BAD CERT '
              END-IF
           END-IF

           .
       0060-EXIT.
           EXIT.

       0080-PROCESS-GAAP.

           MOVE WS-INFO-INIT           TO WS-LIFE-INFO
                                          WS-AH-INFO

           IF GR-LFTYP NOT = '00' AND '  '
              PERFORM 0081-ACCUM-LIFE-ISS
                                       THRU 0081-EXIT
           END-IF

           IF GR-AHTYP NOT = '00' AND '  '
              PERFORM 0082-ACCUM-AH-ISS
                                       THRU 0082-EXIT
           END-IF

           IF (WS-LF-UEP-R78 > ZEROS)
              OR (WS-LF-UEP-PRO > ZEROS)
              OR (WS-LF-REM-AMT > ZEROS)
              OR (WS-LF-MORT-RESV > ZEROS)
              OR (WS-LF-INF-COV > ZEROS)
              OR (WS-LF-INF-CRT > ZEROS)
              PERFORM 0085-GET-LF-RATE-KEY
                                       THRU 0085-EXIT
           END-IF

           IF (WS-AH-UEP-R78 > ZEROS)
              OR (WS-AH-UEP-PRO > ZEROS)
              OR (WS-AH-REM-BEN > ZEROS)
              OR (WS-AH-INF-COV > ZEROS)
              OR (WS-AH-INF-CRT > ZEROS)
              PERFORM 0087-GET-AH-RATE-KEY
                                       THRU 0087-EXIT
           END-IF

           .
       0080-EXIT.
           EXIT.

       0081-ACCUM-LIFE-ISS.

           MOVE GRR-LFPRM              TO WS-LF-UEP-R78
           MOVE GRP-LFPRM              TO WS-LF-UEP-PRO
           COMPUTE WS-LF-UEP-MEAN =
              (WS-LF-UEP-R78 + WS-LF-UEP-PRO) * .5
           MOVE GRS-LFPRM              TO WS-LF-UEP-STAT
           MOVE GR-REM-AMT             TO WS-LF-REM-AMT
           MOVE GR-RESV                TO WS-LF-MORT-RESV

      *  LIFE IBNR RESERVE IS 45 CENTS PER 1,000 OF INFORCE

           COMPUTE WS-LF-IBNR-RESV ROUNDED = WS-LF-REM-AMT * .00045
           MOVE GR-LF-UP-REMTERM       TO WS-LF-REM-TERM
           MOVE 1                      TO WS-LF-INF-COV
           MOVE 1                      TO WS-LF-INF-CRT

           .
       0081-EXIT.
           EXIT.

       0082-ACCUM-AH-ISS.

           MOVE GRR-AHPRM              TO WS-AH-UEP-R78
           MOVE GRP-AHPRM              TO WS-AH-UEP-PRO
           COMPUTE WS-AH-UEP-MEAN =
              (WS-AH-UEP-R78 + WS-AH-UEP-PRO) * .5
120816     if gr-loaded-stat-uep not numeric
120816        move zeros               to gr-loaded-stat-uep
120816     end-if
120816     compute ws-ah-uep-stat =
120816        grs-ahprm + gr-loaded-stat-uep
           MOVE GR-AH-REM-BEN          TO WS-AH-REM-BEN
           MOVE ZEROS                  TO WS-AH-MORT-RESV

      *  AH IBNR RESERVE IS $35 PER 1,000 OF MEAN OF R78 AND PRO UEP

           COMPUTE WS-AH-IBNR-RESV ROUNDED = WS-AH-UEP-MEAN * .035

           MOVE GR-AH-UP-REMTERM       TO WS-AH-REM-TERM
           MOVE 1                      TO WS-AH-INF-COV
           IF WS-LF-INF-CRT = 0
              MOVE 1                   TO WS-AH-INF-CRT
           END-IF

           .
       0082-EXIT.
           EXIT.

       0085-GET-LF-RATE-KEY.

           MOVE WS-WORK-INIT           TO WS-WORK-RECORD

           MOVE WS-RUN-DATE (4:6)      TO OR-VAL-CCYYMM
           MOVE GR-CARRIER             TO OR-CARRIER
           MOVE GR-STATE               TO OR-STATE
           MOVE GR-ACCOUNT             TO OR-ACCOUNT
           MOVE GR-INS-NAME (1:2)      TO OR-CLASS
           MOVE GR-INS-NAME (3:3)      TO OR-DEVIATION
           MOVE 'L'                    TO OR-TYPE
           MOVE GR-LFTYP               TO OR-BEN-CODE
           MOVE WS-LF-UEP-R78          TO OR-UEP-R78   (T1)
           MOVE WS-LF-UEP-PRO          TO OR-UEP-PRO   (T1)
           MOVE WS-LF-UEP-MEAN         TO OR-UEP-MEAN  (T1)
           MOVE WS-LF-UEP-STAT         TO OR-UEP-STAT  (T1)
           MOVE WS-LF-REM-AMT          TO OR-REM-BEN   (T1)
           MOVE WS-LF-MORT-RESV        TO OR-MORT-RESV (T1)
           MOVE WS-LF-IBNR-RESV        TO OR-IBNR-RESV (T1)
           MOVE WS-LF-REM-TERM         TO OR-REM-TERM  (T1)
           MOVE WS-LF-INF-COV          TO OR-INF-COV   (T1)
           MOVE WS-LF-INF-CRT          TO OR-INF-CRT   (T1)

           MOVE '1'                    TO OR-CREDIT-SW

           MOVE GR-EFF                 TO WS-WORK-DATE
           MOVE WS-WORK-CCYY           TO OR-EFF-CCYY

           SET WS-LF-NDX               TO +1

           SEARCH WS-LIFE VARYING WS-LF-NDX AT END
                CONTINUE
             WHEN WS-LIFE-BEN (WS-LF-NDX) = GR-LFTYP
                SET OR-NON-CREDIT      TO TRUE
           END-SEARCH

           EVALUATE GR-LF-TERM
              WHEN < 13
                 MOVE 12               TO OR-TERM-GROUP
              WHEN < 25
                 MOVE 24               TO OR-TERM-GROUP
              WHEN < 37
                 MOVE 36               TO OR-TERM-GROUP
              WHEN < 49
                 MOVE 48               TO OR-TERM-GROUP
              WHEN < 61
                 MOVE 60               TO OR-TERM-GROUP
              WHEN < 73
                 MOVE 72               TO OR-TERM-GROUP
              WHEN < 85
                 MOVE 84               TO OR-TERM-GROUP
              WHEN < 97
                 MOVE 96               TO OR-TERM-GROUP
              WHEN < 109
                 MOVE 108              TO OR-TERM-GROUP
              WHEN < 121
                 MOVE 120              TO OR-TERM-GROUP
              WHEN OTHER
                 MOVE 999              TO OR-TERM-GROUP
           END-EVALUATE

           PERFORM VARYING L1 FROM +1 BY +1 UNTIL
             (L1 > ML1)
                  OR
             ((WSL-ST-CODE        (L1) = OR-STATE)
             AND (WSL-ST-CLASS    (L1) = OR-CLASS)
             AND (WSL-ST-DEV      (L1) = OR-DEVIATION)
             AND (WSL-L-AH        (L1) = 'L')
             AND (WSL-LAH-NUM     (L1) = GR-LFTYP)
             AND (WSL-HIGH-AMT    (L1) > GR-LFBEN)
             AND (WSL-EXPIRY-DATE (L1) > GR-EFF))
           END-PERFORM
           IF L1 <= ML1
              MOVE WSL-HIGH-AMT    (L1) TO OR-RATE-FILE-AMT
              MOVE WSL-EXPIRY-DATE (L1) TO OR-RATE-FILE-EXP-DT
              MOVE WSL-12-MO-RATE  (L1) TO OR-12-MO-RATE
              MOVE WSL-72-MO-RATE  (L1) TO OR-72-MO-RATE
              MOVE WSL-MOB-RATE    (L1) TO OR-MOB-RATE
              IF OR-RATE-FILE-EXP-DT = 99999999
                 MOVE 99991231          TO OR-RATE-FILE-EXP-DT
              END-IF
           ELSE
              MOVE 99991231            TO OR-RATE-FILE-EXP-DT
              DISPLAY ' NO LF RATE RECORD FOR ' OR-CARRIER ' '
                 OR-STATE ' ' OR-ACCOUNT ' ' GR-CERT-NO ' ' OR-CLASS
                 ' ' OR-DEVIATION ' ' GR-LFTYP
           END-IF

           WRITE EXTRACT-RECORD-OUT    FROM WS-WORK-RECORD
           ADD 1 TO EXTR-RECS-OUT

           .
       0085-EXIT.
           EXIT.

       0087-GET-AH-RATE-KEY.

           MOVE WS-WORK-INIT           TO WS-WORK-RECORD

           MOVE WS-RUN-DATE (4:6)      TO OR-VAL-CCYYMM
           MOVE GR-CARRIER             TO OR-CARRIER
           MOVE GR-STATE               TO OR-STATE
           MOVE GR-ACCOUNT             TO OR-ACCOUNT
           MOVE GR-INS-NAME (1:2)      TO OR-CLASS
           MOVE GR-INS-NAME (6:3)      TO OR-DEVIATION
           MOVE 'A'                    TO OR-TYPE
           MOVE GR-AHTYP               TO OR-BEN-CODE
           MOVE WS-AH-UEP-R78          TO OR-UEP-R78   (T1)
           MOVE WS-AH-UEP-PRO          TO OR-UEP-PRO   (T1)
           MOVE WS-AH-UEP-MEAN         TO OR-UEP-MEAN  (T1)
           MOVE WS-AH-UEP-STAT         TO OR-UEP-STAT  (T1)
           MOVE WS-AH-REM-BEN          TO OR-REM-BEN   (T1)
           MOVE WS-AH-MORT-RESV        TO OR-MORT-RESV (T1)
           MOVE WS-AH-IBNR-RESV        TO OR-IBNR-RESV (T1)
           MOVE WS-AH-REM-TERM         TO OR-REM-TERM  (T1)
           MOVE WS-AH-INF-COV          TO OR-INF-COV   (T1)
           MOVE WS-AH-INF-CRT          TO OR-INF-CRT   (T1)

           MOVE '1'                    TO OR-CREDIT-SW

           MOVE GR-EFF                 TO WS-WORK-DATE
           MOVE WS-WORK-CCYY           TO OR-EFF-CCYY

           SET WS-AH-NDX               TO +1

           SEARCH WS-AH VARYING WS-AH-NDX AT END
                CONTINUE
             WHEN WS-AH-BEN (WS-AH-NDX) = GR-AHTYP
                SET OR-NON-CREDIT      TO TRUE
           END-SEARCH

           EVALUATE GR-AH-TERM
              WHEN < 13
                 MOVE 12               TO OR-TERM-GROUP
              WHEN < 25
                 MOVE 24               TO OR-TERM-GROUP
              WHEN < 37
                 MOVE 36               TO OR-TERM-GROUP
              WHEN < 49
                 MOVE 48               TO OR-TERM-GROUP
              WHEN < 61
                 MOVE 60               TO OR-TERM-GROUP
              WHEN < 73
                 MOVE 72               TO OR-TERM-GROUP
              WHEN < 85
                 MOVE 84               TO OR-TERM-GROUP
              WHEN < 97
                 MOVE 96               TO OR-TERM-GROUP
              WHEN < 109
                 MOVE 108              TO OR-TERM-GROUP
              WHEN < 121
                 MOVE 120              TO OR-TERM-GROUP
              WHEN OTHER
                 MOVE 999              TO OR-TERM-GROUP
           END-EVALUATE

           PERFORM VARYING A1 FROM +1 BY +1 UNTIL
             (A1 > MA1)
                  OR
             ((WSA-ST-CODE        (A1) = OR-STATE)
             AND (WSA-ST-CLASS    (A1) = OR-CLASS)
             AND (WSA-ST-DEV      (A1) = OR-DEVIATION)
             AND (WSA-L-AH        (A1) = 'A')
             AND (WSA-LAH-NUM     (A1) = GR-AHTYP)
             AND (WSA-HIGH-AMT    (A1) > GR-AHBEN)
             AND (WSA-EXPIRY-DATE (A1) > GR-EFF))
           END-PERFORM
           IF A1 <= MA1
              MOVE WSA-HIGH-AMT    (A1) TO OR-RATE-FILE-AMT
              MOVE WSA-EXPIRY-DATE (A1) TO OR-RATE-FILE-EXP-DT
              MOVE WSA-12-MO-RATE  (A1) TO OR-12-MO-RATE
              MOVE WSA-72-MO-RATE  (A1) TO OR-72-MO-RATE
              MOVE ZEROS                TO OR-MOB-RATE
              IF OR-RATE-FILE-EXP-DT = 99999999
                 MOVE 99991231          TO OR-RATE-FILE-EXP-DT
              END-IF
           ELSE
              MOVE 99991231            TO OR-RATE-FILE-EXP-DT
              DISPLAY ' NO AH RATE RECORD FOR ' OR-CARRIER ' '
                 OR-STATE ' ' OR-ACCOUNT ' ' GR-CERT-NO ' ' OR-CLASS
                 ' ' OR-DEVIATION ' ' GR-AHTYP
           END-IF

           WRITE EXTRACT-RECORD-OUT    FROM WS-WORK-RECORD
           ADD 1 TO EXTR-RECS-OUT

           .
       0087-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
