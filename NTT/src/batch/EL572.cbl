       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL572.
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
                            

101415******************************************************************
101415*                   C H A N G E   L O G
101415*
101415* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101415*-----------------------------------------------------------------
101415*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101415* EFFECTIVE    NUMBER
101415*-----------------------------------------------------------------
101415* 101415  CR2014090400002  PEMA  ADD NCB PROCESSING
120121* 120121  IR2021120100001  PEMA  Increase size of rate table.
101415******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERTS                ASSIGN TO SYS010.

101415     SELECT ERACCTT              ASSIGN TO ERACCTT
101415        ORGANIZATION IS INDEXED
101415        ACCESS IS DYNAMIC
101415        RECORD KEY IS AM-CONTROL-PRIMARY
101415        FILE STATUS IS ERACCTT-FILE-STATUS.

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

       FD  CERTS
                                       COPY ECSCRIFD.
                                       COPY ECSCRT01.

       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

101415 01  EXTRACT-RECORD-OUT          PIC X(420).
101415
101415 FD  ERACCTT.
101415                                 COPY ERCACCT.

       FD  ERRATE.
                                       COPY ERCRATE.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  PRINTX
                                       COPY ELCPRTFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '       EL572  WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  WS-RATE-SW                  PIC X  VALUE SPACES.
           88  END-OF-RATES               VALUE 'Y'.
101415 77  WS-ERACCTT-SW               PIC X  VALUE SPACES.
101415     88  END-OF-ERACCTT             VALUE 'Y'.
       77  i1                          pic s999  value +0 comp-3.
       77  T1                          PIC S999  VALUE +0 COMP-3.
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
101415 77  ERACCTT-FILE-STATUS         PIC XX   VALUE LOW-VALUES.
       77  WS-INIT-EXT-RECORD          PIC X(368)  VALUE LOW-VALUES.
101415 77  ws-cr-bin-date              pic xx value low-values.
101415 77  ws-cr-lf-canc-dt            pic xx value low-values.
101415 77  ws-cr-ah-canc-dt            pic xx value low-values.
101415 77  months-diff-lf              pic s999 comp-3 value +0.
101415 77  months-diff-ah              pic s999 comp-3 value +0.

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

      *************************************************************
      *  OCCURANCE 1 = YTD, 2 = MTD, 3 = L12 AND 4 = ITD
      *************************************************************

101415 01  WS-INFO-INIT               PIC X(230).

       01  WS-LIFE-INFO.
           05  WS-LF-DEV               PIC XXX.
           05  WS-LF-BEN-CODE          PIC XX.
           05  WS-LF-RATE-AMT          PIC 9(9).
           05  WS-LF-EXPIRE-DATE       PIC 9(8).
           05  FILLER OCCURS 4.
               10  WS-LF-ORIG-BENEFIT  PIC S9(9)V99 COMP-3.
               10  WS-LF-GROSS-PREM    PIC S9(7)V99 COMP-3.
               10  WS-LF-REFUNDED-PREM PIC S9(7)V99 COMP-3.
               10  WS-LF-ACCT-COMM     PIC S9(7)V99 COMP-3.
               10  WS-LF-OW-COMM       PIC S9(7)V99 COMP-3.
               10  WS-LF-ACCT-REF-COMM PIC S9(7)V99 COMP-3.
               10  WS-LF-OW-REF-COMM   PIC S9(7)V99 COMP-3.
101415         10  WS-LF-ACCT-REF-COMM-ncb
101415                                 PIC S9(7)V99 COMP-3.
101415         10  WS-LF-OW-REF-COMM-ncb
101415                                 PIC S9(7)V99 COMP-3.
               10  WS-LF-ORIG-TERM     PIC S9(3)    COMP-3.
               10  WS-LF-ISS-CNT       PIC 9.
               10  WS-LF-COV-ISS-CNT   PIC 9.
               10  WS-LF-REF-CNT       PIC 9.
               10  WS-LF-COV-REF-CNT   PIC 9.

       01  WS-AH-INFO.
           05  WS-AH-DEV               PIC XXX.
           05  WS-AH-BEN-CODE          PIC XX.
           05  WS-AH-RATE-AMT          PIC 9(9).
           05  WS-AH-EXPIRE-DATE       PIC 9(8).
           05  FILLER OCCURS 4.
               10  WS-AH-ORIG-BENEFIT  PIC S9(9)V99 COMP-3.
               10  WS-AH-GROSS-PREM    PIC S9(7)V99 COMP-3.
               10  WS-AH-REFUNDED-PREM PIC S9(7)V99 COMP-3.
               10  WS-AH-ACCT-COMM     PIC S9(7)V99 COMP-3.
               10  WS-AH-OW-COMM       PIC S9(7)V99 COMP-3.
               10  WS-AH-ACCT-REF-COMM PIC S9(7)V99 COMP-3.
               10  WS-AH-OW-REF-COMM   PIC S9(7)V99 COMP-3.
101415         10  WS-AH-ACCT-REF-COMM-ncb
101415                                 PIC S9(7)V99 COMP-3.
101415         10  WS-AH-OW-REF-COMM-ncb
101415                                 PIC S9(7)V99 COMP-3.
               10  WS-AH-ORIG-TERM     PIC S9(3)    COMP-3.
               10  WS-AH-ISS-CNT       PIC 9.
               10  WS-AH-COV-ISS-CNT   PIC 9.
               10  WS-AH-REF-CNT       PIC 9.
               10  WS-AH-COV-REF-CNT   PIC 9.

101415 01  WS-WORK-INIT                PIC X(420).
101415
101415 01  WS-LEVELS-CHARGEBACK-SWITCHES.                               
101415     12  WS-LF-CHARGEBACK-LEVELS.                                 
101415         16  WS-LF-CHARGEBACK-SW   PIC X OCCURS 10 TIMES.     
101415     12  WS-AH-CHARGEBACK-LEVELS.                                 
101415         16  WS-AH-CHARGEBACK-SW   PIC X OCCURS 10 TIMES.     

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
               10  OR-ORIG-BENEFIT     PIC S9(13)V99 COMP-3.
               10  OR-GROSS-PREM       PIC S9(11)V99 COMP-3.
               10  OR-REFUNDED-PREM    PIC S9(11)V99 COMP-3.
               10  OR-ACCT-COMM        PIC S9(11)V99 COMP-3.
               10  OR-OW-COMM          PIC S9(11)V99 COMP-3.
               10  OR-ACCT-REF-COMM    PIC S9(11)V99 COMP-3.
               10  OR-OW-REF-COMM      PIC S9(11)V99 COMP-3.
101415         10  OR-ACCT-REF-COMM-ncb
101415                                 PIC S9(11)V99 COMP-3.
101415         10  OR-OW-REF-COMM-ncb  PIC S9(11)V99 COMP-3.
               10  OR-ORIG-TERM        PIC S9(9)     COMP-3.
               10  OR-ISS-CNT          PIC S9(9)     COMP-3.
               10  OR-COV-ISS-CNT      PIC S9(9)     COMP-3.
               10  OR-REF-CNT          PIC S9(9)     COMP-3.
               10  OR-COV-REF-CNT      PIC S9(9)     COMP-3.


       01  FILLER                      PIC X(400)  VALUE LOW-VALUES.

       01  WS-DISPLAY-DATE             PIC ZZZ9(8).
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
           05  WS-PME-DATE             PIC 9(11).
           05  FILLER REDEFINES WS-PME-DATE.
               10  FILLER              PIC XXX.
               10  WS-PME-CCYYMM       PIC 9(6).
               10  FILLER REDEFINES WS-PME-CCYYMM.
                   15  WS-PME-CCYY     PIC 9999.
                   15  WS-PME-MM       PIC 99.
               10  WS-PME-DD           PIC 99.
       01  FILLER.
           05  WS-ENTRY-DATE           PIC 9(11).
           05  FILLER REDEFINES        WS-ENTRY-DATE.
               10  FILLER              PIC 999.
               10  WS-ENTRY-CCYYMM     PIC 9(6).
               10  WS-ENTRY-DD         PIC 99.
       01  FILLER.
           05  WS-L12-DATE             PIC 9(11).
           05  FILLER REDEFINES WS-L12-DATE.
               10  FILLER              PIC XXX.
               10  WS-L12-CCYYMM       PIC 9(6).
               10  FILLER REDEFINES WS-L12-CCYYMM.
                   15  WS-L12-CCYY     PIC 9999.
                   15  WS-L12-MM       PIC 99.
               10  WS-L12-DD           PIC 99.
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

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' TOTAL CERT RECORDS READ ' WS-CRT-CNT-TOT
           DISPLAY ' TOTAL EXTR RECORDS OUT  ' EXTR-RECS-OUT
           GOBACK

           .
       0010-OPEN-FILES.

101415     OPEN INPUT CERTS ERRATE ERACCTT
               OUTPUT EXTRACT

           IF ERRATE-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERRATE - ERROR - OPEN ' ERRATE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

101415     IF ERACCTT-FILE-STATUS = '00' OR '97'
101415        CONTINUE
101415     ELSE
101415        DISPLAY ' ERACCTT - ERROR - OPEN ' ERACCTT-FILE-STATUS
101415        PERFORM ABEND-PGM
101415     END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE -1                     TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '1'                    TO DC-END-OF-MONTH
           MOVE '6'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-CYMD   TO WS-PME-DATE
           ELSE
              DISPLAY ' ERROR - DATE CONVERT ' DC-ERROR-CODE
              PERFORM ABEND-PGM
           END-IF

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE -12                    TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
           MOVE '6'                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-CYMD   TO WS-L12-DATE
                                          WS-PYE-DATE
           ELSE
              DISPLAY ' ERROR - DATE CONVERT ' DC-ERROR-CODE
              PERFORM ABEND-PGM
           END-IF

           MOVE 12                     TO WS-PYE-MM
           MOVE 31                     TO WS-PYE-DD

           DISPLAY ' CURRENT PROCESS  DATE ' WS-RUN-DATE
           DISPLAY '   PRIOR YEAR END DATE ' WS-PYE-DATE
           DISPLAY '         12 MONTHS AGO ' WS-L12-DATE
           DISPLAY '  PRIOR MONTH END DATE ' WS-PME-DATE

           INITIALIZE WS-LIFE-INFO
           MOVE WS-LIFE-INFO           TO WS-INFO-INIT
                                          WS-AH-INFO

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
101415     move low-values             to am-control-primary
101415     move dte-clasic-company-cd  to am-company-cd
101415     start eracctt key >= am-control-primary
101415     if eracctt-file-status not = '00'
101415        display ' ERROR - ERACCTT - START ' ERACCTT-FILE-STATUS
101415        PERFORM ABEND-PGM
101415     END-IF
           PERFORM 0060-READ-CERT      THRU 0060-EXIT
101415     PERFORM 0075-READ-ERACCTT   THRU 0075-EXIT

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
                                       TO WSL-MOB-RATE (L1)
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

101415     CLOSE CERTS EXTRACT ERRATE ERACCTT

           IF ERRATE-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERRATE - ERROR - CLOSE ' ERRATE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

101415     IF ERACCTT-FILE-STATUS = '00'
101415        CONTINUE
101415     ELSE
101415        DISPLAY ' ERACCTT - ERROR - CLOSE ' ERACCTT-FILE-STATUS
101415        PERFORM ABEND-PGM
101415     END-IF

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
               SET END-OF-INPUT        TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-CRT-CNT
                                          WS-CRT-CNT-TOT
              IF WS-CRT-CNT = +10000
                 DISPLAY ' CERTS READ ' WS-CRT-CNT-TOT
                 MOVE +0               TO WS-CRT-CNT
              END-IF
              IF CR-CERT-NO = '0000109489 '
                 DISPLAY ' FOUND BAD CERT '
              END-IF
           END-IF

           .
       0060-EXIT.
           EXIT.

101415 0070-sync-with-acct.
101415
101415     if am-control-a < cr-acct-control
101415        perform 0075-read-eracctt thru 0075-exit
101415        go to 0070-sync-with-acct
101415     else
101415        if am-control-a > cr-acct-control
101415           display ' mismatch between cert and account '
101415              am-control-a ' ' cr-acct-control
101415           perform abend-pgm
101415        end-if
101415     end-if
101415
101415     if am-expire-dt <= cr-dt
101415        perform 0075-read-eracctt thru 0075-exit
101415        go to 0070-sync-with-acct
101415     end-if
101415
101415     .
101415 0070-exit.
101415     exit.
101415
101415 0075-READ-ERACCTT.
101415
101415     READ ERACCTT NEXT RECORD
101415
101415     IF ERACCTT-FILE-STATUS = '10' OR '23'
101415        DISPLAY ' NON ZERO STATUS ' ERACCTT-FILE-STATUS
101415        move high-values         to am-control-a
101415        SET END-OF-ERACCTT       TO TRUE
101415        go to 0075-exit
101415     ELSE
101415        IF ERACCTT-FILE-STATUS <> '00'
101415           DISPLAY ' ERROR - ERACCTT - READ ' ERACCTT-FILE-STATUS
101415           PERFORM ABEND-PGM
101415        END-IF
101415     END-IF
101415
101415     .
101415 0075-EXIT.
101415     EXIT.

       0080-PROCESS-CERT.

           MOVE WS-INFO-INIT           TO WS-LIFE-INFO
                                          WS-AH-INFO

101415     MOVE CR-DT                  TO DC-GREG-DATE-CYMD
101415     MOVE 'L'                    TO DC-OPTION-CODE
101415     MOVE +0                     TO DC-ELAPSED-MONTHS
101415                                    DC-ELAPSED-DAYS
101415     PERFORM 8510-date-conversion
101415                                 THRU 8590-exit
101415     MOVE DC-BIN-DATE-1          TO WS-CR-BIN-DATE
101415
101415     perform 0070-sync-with-acct thru 0070-exit

      ***********   YTD   PROCESSING   HERE   **********

           IF (CR-ENTRY-DATE > WS-PYE-DATE)
              AND (CR-ENTRY-DATE <= WS-RUN-DATE-N)
              AND (CR-ENTRY-STATUS NOT = '9' AND 'D' AND 'V' AND 'M')
              MOVE +1                  TO T1
              IF CR-LFTYP NOT = '00' AND '  '
                 PERFORM 0081-ACCUM-LIFE-ISS
                                       THRU 0081-EXIT
              END-IF
              IF CR-AHTYP NOT = '00' AND '  '
                 PERFORM 0082-ACCUM-AH-ISS
                                       THRU 0082-EXIT
              END-IF
           END-IF

           IF (CR-LF-CANCEL-EXIT-DATE > WS-PYE-DATE)
              AND (CR-LF-CANCEL-EXIT-DATE <= WS-RUN-DATE-N)
              MOVE +1                  TO T1
              IF CR-LFTYP NOT = '00' AND '  '
                 PERFORM 0083-ACCUM-LIFE-REF
                                       THRU 0083-EXIT
              END-IF
           END-IF
           IF (CR-AH-CANCEL-EXIT-DATE > WS-PYE-DATE)
              AND (CR-AH-CANCEL-EXIT-DATE <= WS-RUN-DATE-N)
              MOVE +1                  TO T1
              IF CR-AHTYP NOT = '00' AND '  '
                 PERFORM 0084-ACCUM-AH-REF
                                       THRU 0084-EXIT
              END-IF
           END-IF

           IF (CR-ENTRY-DATE <= WS-PYE-DATE)
              AND (CR-ENTRY-STATUS NOT = '9' AND 'D' AND 'V' AND
                 'M' AND '5')
              IF CR-LFTYP NOT = '00' AND '  '
                 COMPUTE WS-LF-ORIG-BENEFIT (1) =
                    CR-LFAMT + CR-LFAMT-ALT
                 MOVE CR-LF-TERM    TO WS-LF-ORIG-TERM (1)
              END-IF
              IF CR-AHTYP NOT = '00' AND '  '
                 COMPUTE WS-AH-ORIG-BENEFIT (1) =
                    CR-AHAMT * CR-AH-TERM
      *          MOVE CR-AHAMT      TO WS-AH-ORIG-BENEFIT (1)
                 MOVE CR-AH-TERM    TO WS-AH-ORIG-TERM (1)
              END-IF
           END-IF

      ***********   YTD   PROCESSING   END    **********

      ***********   MTD   PROCESSING   HERE   **********

           IF (CR-ENTRY-DATE > WS-PME-DATE)
              AND (CR-ENTRY-DATE <= WS-RUN-DATE-N)
              AND (CR-ENTRY-STATUS NOT = '9' AND 'D' AND 'V' AND 'M')
              MOVE +2                  TO T1
              IF CR-LFTYP NOT = '00' AND '  '
                 PERFORM 0081-ACCUM-LIFE-ISS
                                       THRU 0081-EXIT
              END-IF
              IF CR-AHTYP NOT = '00' AND '  '
                 PERFORM 0082-ACCUM-AH-ISS
                                       THRU 0082-EXIT
              END-IF
           END-IF

           IF (CR-LF-CANCEL-EXIT-DATE > WS-PME-DATE)
              AND (CR-LF-CANCEL-EXIT-DATE <= WS-RUN-DATE-N)
              MOVE +2                  TO T1
              IF CR-LFTYP NOT = '00' AND '  '
                 PERFORM 0083-ACCUM-LIFE-REF
                                       THRU 0083-EXIT
              END-IF
           END-IF
           IF (CR-AH-CANCEL-EXIT-DATE > WS-PME-DATE)
              AND (CR-AH-CANCEL-EXIT-DATE <= WS-RUN-DATE-N)
              MOVE +2                  TO T1
              IF CR-AHTYP NOT = '00' AND '  '
                 PERFORM 0084-ACCUM-AH-REF
                                       THRU 0084-EXIT
              END-IF
           END-IF

           IF (CR-ENTRY-DATE <= WS-PME-DATE)
              AND (CR-ENTRY-STATUS NOT = '9' AND 'D' AND 'V' AND
                 'M' AND '5')
              IF CR-LFTYP NOT = '00' AND '  '
                 COMPUTE WS-LF-ORIG-BENEFIT (2) =
                    CR-LFAMT + CR-LFAMT-ALT
                 MOVE CR-LF-TERM    TO WS-LF-ORIG-TERM (2)
              END-IF
              IF CR-AHTYP NOT = '00' AND '  '
                 COMPUTE WS-AH-ORIG-BENEFIT (2) =
                    CR-AHAMT * CR-AH-TERM
      *          MOVE CR-AHAMT      TO WS-AH-ORIG-BENEFIT (2)
                 MOVE CR-AH-TERM    TO WS-AH-ORIG-TERM (2)
              END-IF
           END-IF

      ***********   MTD   PROCESSING   END    **********

      ***********   L12   PROCESSING   HERE   **********

           IF (CR-ENTRY-DATE > WS-L12-DATE)
              AND (CR-ENTRY-DATE <= WS-RUN-DATE-N)
              AND (CR-ENTRY-STATUS NOT = '9' AND 'D' AND 'V' AND 'M')
              MOVE +3                  TO T1
              IF CR-LFTYP NOT = '00' AND '  '
                 PERFORM 0081-ACCUM-LIFE-ISS
                                       THRU 0081-EXIT
              END-IF
              IF CR-AHTYP NOT = '00' AND '  '
                 PERFORM 0082-ACCUM-AH-ISS
                                       THRU 0082-EXIT
              END-IF
           END-IF

           IF (CR-LF-CANCEL-EXIT-DATE > WS-L12-DATE)
              AND (CR-LF-CANCEL-EXIT-DATE <= WS-RUN-DATE-N)
              MOVE +3                  TO T1
              IF CR-LFTYP NOT = '00' AND '  '
                 PERFORM 0083-ACCUM-LIFE-REF
                                       THRU 0083-EXIT
              END-IF
           END-IF
           IF (CR-AH-CANCEL-EXIT-DATE > WS-L12-DATE)
              AND (CR-AH-CANCEL-EXIT-DATE <= WS-RUN-DATE-N)
              MOVE +3                  TO T1
              IF CR-AHTYP NOT = '00' AND '  '
                 PERFORM 0084-ACCUM-AH-REF
                                       THRU 0084-EXIT
              END-IF
           END-IF

           IF (CR-ENTRY-DATE <= WS-L12-DATE)
              AND (CR-ENTRY-STATUS NOT = '9' AND 'D' AND 'V' AND
                 'M' AND '5')
              IF CR-LFTYP NOT = '00' AND '  '
                 COMPUTE WS-LF-ORIG-BENEFIT (3) =
                    CR-LFAMT + CR-LFAMT-ALT
                 MOVE CR-LF-TERM    TO WS-LF-ORIG-TERM (3)
              END-IF
              IF CR-AHTYP NOT = '00' AND '  '
                 COMPUTE WS-AH-ORIG-BENEFIT (3) =
                    CR-AHAMT * CR-AH-TERM
      *          MOVE CR-AHAMT      TO WS-AH-ORIG-BENEFIT (3)
                 MOVE CR-AH-TERM    TO WS-AH-ORIG-TERM (3)
              END-IF
           END-IF

      ***********   L12   PROCESSING   END    **********


      ***********   ITD   PROCESSING   HERE   **********

           IF CR-ENTRY-STATUS NOT = '9' AND 'D' AND 'V' AND 'M'
              MOVE +4                  TO T1
              IF CR-LFTYP NOT = '00' AND '  '
                 PERFORM 0081-ACCUM-LIFE-ISS
                                       THRU 0081-EXIT
              END-IF
              IF CR-AHTYP NOT = '00' AND '  '
                 PERFORM 0082-ACCUM-AH-ISS
                                       THRU 0082-EXIT
              END-IF
           END-IF

           IF (CR-LF-CANCEL-EXIT-DATE > ZEROS)
              AND (CR-LF-CANCEL-EXIT-DATE <= WS-RUN-DATE-N)
              MOVE +4                  TO T1
              IF CR-LFTYP NOT = '00' AND '  '
                 PERFORM 0083-ACCUM-LIFE-REF
                                       THRU 0083-EXIT
              END-IF
           END-IF
           IF (CR-AH-CANCEL-EXIT-DATE > ZEROS)
              AND (CR-AH-CANCEL-EXIT-DATE <= WS-RUN-DATE-N)
              MOVE +4                  TO T1
              IF CR-AHTYP NOT = '00' AND '  '
                 PERFORM 0084-ACCUM-AH-REF
                                       THRU 0084-EXIT
              END-IF
           END-IF

           IF CR-ENTRY-STATUS NOT = '9' AND 'D' AND 'V' AND 'M' AND '5'
              IF CR-LFTYP NOT = '00' AND '  '
                 COMPUTE WS-LF-ORIG-BENEFIT (4) =
                    CR-LFAMT + CR-LFAMT-ALT
                 MOVE CR-LF-TERM    TO WS-LF-ORIG-TERM (4)
              END-IF
              IF CR-AHTYP NOT = '00' AND '  '
                 COMPUTE WS-AH-ORIG-BENEFIT (4) =
                    CR-AHAMT * CR-AH-TERM
      *          MOVE CR-AHAMT      TO WS-AH-ORIG-BENEFIT (4)
                 MOVE CR-AH-TERM    TO WS-AH-ORIG-TERM (4)
              END-IF
           END-IF

      ***********   ITD   PROCESSING   END    **********

           IF ((CR-LFTYP NOT = '00')
               AND (CR-ENTRY-STATUS = '5'))
              OR (WS-LF-ORIG-BENEFIT (1) > ZEROS)
              OR (WS-LF-GROSS-PREM (1) > ZEROS)
              OR (WS-LF-REFUNDED-PREM (1) > ZEROS)
              OR (WS-LF-ORIG-BENEFIT (2) > ZEROS)
              OR (WS-LF-GROSS-PREM (2) > ZEROS)
              OR (WS-LF-REFUNDED-PREM (2) > ZEROS)
              OR (WS-LF-ORIG-BENEFIT (3) > ZEROS)
              OR (WS-LF-GROSS-PREM (3) > ZEROS)
              OR (WS-LF-REFUNDED-PREM (3) > ZEROS)
              OR (WS-LF-ORIG-BENEFIT (4) > ZEROS)
              OR (WS-LF-GROSS-PREM (4) > ZEROS)
              OR (WS-LF-REFUNDED-PREM (4) > ZEROS)
              PERFORM 0085-GET-LF-RATE-KEY
                                       THRU 0085-EXIT
           END-IF

           IF ((CR-AHTYP NOT = '00')
               AND (CR-ENTRY-STATUS = '5'))
              OR (WS-AH-ORIG-BENEFIT (1) > ZEROS)
              OR (WS-AH-GROSS-PREM (1) > ZEROS)
              OR (WS-AH-REFUNDED-PREM (1) > ZEROS)
              OR (WS-AH-ORIG-BENEFIT (2) > ZEROS)
              OR (WS-AH-GROSS-PREM (2) > ZEROS)
              OR (WS-AH-REFUNDED-PREM (2) > ZEROS)
              OR (WS-AH-ORIG-BENEFIT (3) > ZEROS)
              OR (WS-AH-GROSS-PREM (3) > ZEROS)
              OR (WS-AH-REFUNDED-PREM (3) > ZEROS)
              OR (WS-AH-ORIG-BENEFIT (4) > ZEROS)
              OR (WS-AH-GROSS-PREM (4) > ZEROS)
              OR (WS-AH-REFUNDED-PREM (4) > ZEROS)
              PERFORM 0087-GET-AH-RATE-KEY
                                       THRU 0087-EXIT
           END-IF

           .
       0080-EXIT.
           EXIT.

       0081-ACCUM-LIFE-ISS.

           IF CR-ENTRY-STATUS = '5'
              CONTINUE
           ELSE
              COMPUTE WS-LF-GROSS-PREM (T1) = CR-LFPRM + CR-LFPRM-ALT
              MOVE 1                      TO WS-LF-ISS-CNT (T1)
                                             WS-LF-COV-ISS-CNT (T1)
              
              PERFORM VARYING C1 FROM +1 BY +1 UNTIL
                 C1 > +10
                 IF CR-AGT-TYPE (C1) = 'C' OR 'D'
                    COMPUTE WS-LF-ACCT-COMM (T1) =
                      WS-LF-ACCT-COMM (T1) +
                       (WS-LF-GROSS-PREM (T1) * CR-LCOM-L (C1))
                 ELSE
                    IF CR-AGT-TYPE (C1) = 'O' OR 'P'
                       COMPUTE WS-LF-OW-COMM (T1) =
                         WS-LF-OW-COMM (T1) +
                          (WS-LF-GROSS-PREM (T1) * CR-LCOM-L (C1))
                    END-IF
                 END-IF
              END-PERFORM
           END-IF

           .
       0081-EXIT.
           EXIT.

       0082-ACCUM-AH-ISS.

           IF CR-ENTRY-STATUS = '5'
              CONTINUE
           ELSE
              MOVE CR-AHPRM            TO WS-AH-GROSS-PREM (T1)
              MOVE 1                   TO WS-AH-COV-ISS-CNT (T1)
              IF WS-LF-ISS-CNT (T1) = 0
                 MOVE 1                TO WS-AH-ISS-CNT (T1)
              END-IF
              
              PERFORM VARYING C1 FROM +1 BY +1 UNTIL
                 C1 > +10
                 IF CR-AGT-TYPE (C1) = 'C' OR 'D'
                    COMPUTE WS-AH-ACCT-COMM (T1) =
                      WS-AH-ACCT-COMM (T1) +
                       (WS-AH-GROSS-PREM (T1) * CR-LCOM-AH (C1))
                 ELSE
                    IF CR-AGT-TYPE (C1) = 'O' OR 'P'
                       COMPUTE WS-AH-OW-COMM (T1) =
                         WS-AH-OW-COMM (T1) +
                          (WS-AH-GROSS-PREM (T1) * CR-LCOM-AH (C1))
                    END-IF
                 END-IF
              END-PERFORM
           END-IF

           .
       0082-EXIT.
           EXIT.

       0083-ACCUM-LIFE-REF.

           MOVE CR-LFRFND              TO WS-LF-REFUNDED-PREM (T1)
           MOVE 1                      TO WS-LF-REF-CNT (T1)
                                          WS-LF-COV-REF-CNT (T1)

101415     if cr-lf-canc-dt not numeric
101415        move zeros                to cr-lf-canc-dt
101415     end-if
101415     if cr-lf-canc-dt = zeros
101415        display ' life cancel dte zeros ' cr-carrier ' '
101415           cr-state ' ' cr-account ' ' cr-cert-no ' ' cr-dt
101415        go to 0083-exit
101415     end-if
101415
101415     MOVE CR-lf-canc-dt          TO DC-GREG-DATE-CYMD
101415     MOVE 'L'                    TO DC-OPTION-CODE
101415     MOVE +0                     TO DC-ELAPSED-MONTHS
101415                                    DC-ELAPSED-DAYS
101415     PERFORM 8510-date-conversion
101415                                 THRU 8590-exit
101415     MOVE DC-BIN-DATE-1          TO WS-CR-lf-canc-dt
101415
101415     MOVE WS-CR-BIN-DATE         TO  DC-BIN-DATE-1
101415     MOVE ws-cr-lf-canc-dt       TO  DC-BIN-DATE-2
101415     MOVE '1'                    TO  DC-OPTION-CODE
101415     MOVE ' '                    TO  DC-CENTURY-ADJUSTMENT
101415     MOVE ZEROS                  TO  DC-ELAPSED-MONTHS
101415                                     DC-ODD-DAYS-OVER
101415                                     DC-ELAPSED-DAYS
101415     PERFORM 8510-date-conversion
101415                                 THRU 8590-exit
101415     MOVE DC-ELAPSED-MONTHS      TO  MONTHS-DIFF-LF
101415                                                                  
101415     IF DC-ODD-DAYS-OVER > 0
101415        ADD  +1                  TO  MONTHS-DIFF-LF
101415     end-if
101415
101415     MOVE  'YYYYYYYYYY'          TO  WS-LF-CHARGEBACK-LEVELS
101415
101415     perform varying i1 from +1 by +1 until i1 > +10
101415        if (am-comm-chargeback (i1) not numeric)
101415           or (am-comm-chargeback (i1) = zeros)
101415           continue
101415        else
101415           if am-comm-chargeback (i1) = '99'
101415              move 'N'           to ws-lf-chargeback-sw (i1)
101415           else
101415              if months-diff-lf > am-comm-chargeback (i1)
101415                 move 'N'        to ws-lf-chargeback-sw (i1)
101415              end-if
101415           end-if
101415        end-if
101415     end-perform
101415
           PERFORM VARYING C1 FROM +1 BY +1 UNTIL
              C1 > +10
101415        if (cr-com-agt (c1) not = am-agt (c1))
101415           or (cr-agt-type (c1) not = am-com-typ (c1))
101415           if ws-lf-chargeback-sw (c1) = 'N'
101415              display ' LF - mismatching agents ' c1 ' '
101415                 cr-carrier ' ' cr-state ' ' cr-account
101415                 ' ' cr-cert-no ' '
101415                 cr-agt-type (c1) ' ' cr-com-agt (c1) ' '
101415                 am-com-typ (c1) ' ' am-agt (c1)
101415           end-if
101415        end-if
101415
              IF CR-AGT-TYPE (C1) = 'C' OR 'D'
                 COMPUTE WS-LF-ACCT-REF-COMM (T1) =
                    WS-LF-ACCT-REF-COMM (T1) +
                    (WS-LF-REFUNDED-PREM (T1) * CR-LCOM-L (C1))
101415           if ws-lf-chargeback-sw (c1) = 'Y'
101415              COMPUTE WS-LF-ACCT-REF-COMM-ncb (T1) =
101415                 WS-LF-ACCT-REF-COMM-ncb (T1) +
101415                 (WS-LF-REFUNDED-PREM (T1) * CR-LCOM-L (C1))
101415           end-if
              ELSE
                 IF CR-AGT-TYPE (C1) = 'O' OR 'P'
                    COMPUTE WS-LF-OW-REF-COMM (T1) =
                       WS-LF-OW-REF-COMM (T1) +
                       (WS-LF-REFUNDED-PREM (T1) * CR-LCOM-L (C1))
101415              if ws-lf-chargeback-sw (c1) = 'Y'
101415                 COMPUTE WS-LF-OW-REF-COMM-ncb (T1) =
101415                    WS-LF-OW-REF-COMM-ncb (T1) +
101415                    (WS-LF-REFUNDED-PREM (T1) * CR-LCOM-L (C1))
101415              end-if
                 END-IF
              END-IF
           END-PERFORM

           .
       0083-EXIT.
           EXIT.

       0084-ACCUM-AH-REF.

           MOVE CR-AHRFND              TO WS-AH-REFUNDED-PREM (T1)
           MOVE 1                      TO WS-AH-COV-REF-CNT (T1)

           IF WS-LF-REF-CNT (T1) = 0
              MOVE 1                   TO WS-AH-REF-CNT (T1)
           END-IF

101415     if cr-ah-canc-dt not numeric
101415        move zeros                to cr-ah-canc-dt
101415     end-if
101415     if cr-ah-canc-dt = zeros
101415        display ' A&H  cancel dte zeros ' cr-carrier ' '
101415           cr-state ' ' cr-account ' ' cr-cert-no ' ' cr-dt
101415        go to 0084-exit
101415     end-if
101415
101415     MOVE CR-ah-canc-dt          TO DC-GREG-DATE-CYMD
101415     MOVE 'L'                    TO DC-OPTION-CODE
101415     MOVE +0                     TO DC-ELAPSED-MONTHS
101415                                    DC-ELAPSED-DAYS
101415     PERFORM 8510-date-conversion
101415                                 THRU 8590-exit
101415     MOVE DC-BIN-DATE-1          TO WS-CR-ah-canc-dt
101415
101415     MOVE WS-CR-BIN-DATE         TO  DC-BIN-DATE-1
101415     MOVE ws-cr-ah-canc-dt       TO  DC-BIN-DATE-2
101415     MOVE '1'                    TO  DC-OPTION-CODE
101415     MOVE ' '                    TO  DC-CENTURY-ADJUSTMENT
101415     MOVE ZEROS                  TO  DC-ELAPSED-MONTHS
101415                                     DC-ODD-DAYS-OVER
101415                                     DC-ELAPSED-DAYS
101415     PERFORM 8510-date-conversion
101415                                 THRU 8590-exit
101415     MOVE DC-ELAPSED-MONTHS      TO  MONTHS-DIFF-ah
101415                                                                  
101415     IF DC-ODD-DAYS-OVER > 0
101415        ADD  +1                  TO  MONTHS-DIFF-ah
101415     end-if
101415
101415     MOVE  'YYYYYYYYYY'          TO  WS-ah-CHARGEBACK-LEVELS
101415
101415     perform varying i1 from +1 by +1 until i1 > +10
101415        if (am-comm-chargeback (i1) not numeric)
101415           or (am-comm-chargeback (i1) = zeros)
101415           continue
101415        else
101415           if am-comm-chargeback (i1) = '99'
101415              move 'N'           to ws-ah-chargeback-sw (i1)
101415           else
101415              if months-diff-lf > am-comm-chargeback (i1)
101415                 move 'N'        to ws-ah-chargeback-sw (i1)
101415              end-if
101415           end-if
101415        end-if
101415     end-perform
101415
           PERFORM VARYING C1 FROM +1 BY +1 UNTIL
              C1 > +10
101415        IF WS-LF-REF-CNT (T1) = 0
101415           if (cr-com-agt (c1) not = am-agt (c1))
101415              or (cr-agt-type (c1) not = am-com-typ (c1))
101415              if ws-ah-chargeback-sw (c1) = 'N'
101415                 display ' AH - mismatching agents ' c1 ' '
101415                    cr-carrier ' ' cr-state ' ' cr-account
101415                    ' ' cr-cert-no ' '
101415                    cr-agt-type (c1) ' ' cr-com-agt (c1) ' '
101415                    am-com-typ (c1) ' ' am-agt (c1)
101415              end-if
101415           end-if
101415        end-if
              IF CR-AGT-TYPE (C1) = 'C' OR 'D'
                 COMPUTE WS-AH-ACCT-REF-COMM (T1) =
                    WS-AH-ACCT-REF-COMM (T1) +
                    (WS-AH-REFUNDED-PREM (T1) * CR-LCOM-AH (C1))
101415           if ws-ah-chargeback-sw (c1) = 'Y'
101415              COMPUTE WS-AH-ACCT-REF-COMM-ncb (T1) =
101415                 WS-AH-ACCT-REF-COMM-ncb (T1) +
101415                 (WS-AH-REFUNDED-PREM (T1) * CR-LCOM-AH (C1))
101415           end-if
              ELSE
                 IF CR-AGT-TYPE (C1) = 'O' OR 'P'
                    COMPUTE WS-AH-OW-REF-COMM (T1) =
                       WS-AH-OW-REF-COMM (T1) +
                       (WS-AH-REFUNDED-PREM (T1) * CR-LCOM-AH (C1))
101415              if ws-ah-chargeback-sw (c1) = 'Y'
101415                 COMPUTE WS-AH-OW-REF-COMM-ncb (T1) =
101415                    WS-AH-OW-REF-COMM-ncb (T1) +
101415                    (WS-AH-REFUNDED-PREM (T1) * CR-LCOM-AH (C1))
101415              end-if
                 END-IF
              END-IF
           END-PERFORM

           .
       0084-EXIT.
           EXIT.

       0085-GET-LF-RATE-KEY.


      *    IF CR-ENTRY-STATUS = '5'
      *       CONTINUE
      *    ELSE
      *       MOVE 1                   TO WS-LF-ISS-CNT (T1)
      *                                   WS-LF-COV-ISS-CNT (T1)
      *    END-IF

           MOVE WS-WORK-INIT           TO WS-WORK-RECORD

           MOVE WS-RUN-DATE (4:6)      TO OR-VAL-CCYYMM
           MOVE CR-CARRIER             TO OR-CARRIER
           MOVE CR-STATE               TO OR-STATE
           MOVE CR-ACCOUNT             TO OR-ACCOUNT
           MOVE CR-RATING-CLASS        TO OR-CLASS
           MOVE CR-LF-DEV-CODE         TO OR-DEVIATION
           MOVE 'L'                    TO OR-TYPE
           MOVE CR-LFTYP               TO OR-BEN-CODE
           MOVE '1'                    TO OR-CREDIT-SW

           MOVE CR-DT                  TO WS-WORK-DATE
           MOVE WS-WORK-CCYY           TO OR-EFF-CCYY
           SET WS-LF-NDX               TO +1

           SEARCH WS-LIFE VARYING WS-LF-NDX AT END
                CONTINUE
             WHEN WS-LIFE-BEN (WS-LF-NDX) = CR-LFTYP
                SET OR-NON-CREDIT      TO TRUE
           END-SEARCH

           EVALUATE CR-LF-TERM
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


           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              T1 > +4
              MOVE WS-LF-ORIG-BENEFIT  (T1) TO OR-ORIG-BENEFIT (T1)
              MOVE WS-LF-GROSS-PREM    (T1) TO OR-GROSS-PREM (T1)
              MOVE WS-LF-REFUNDED-PREM (T1) TO OR-REFUNDED-PREM (T1)
              MOVE WS-LF-ACCT-COMM     (T1) TO OR-ACCT-COMM (T1)
              MOVE WS-LF-OW-COMM       (T1) TO OR-OW-COMM (T1)
              MOVE WS-LF-ACCT-REF-COMM (T1) TO OR-ACCT-REF-COMM (T1)
              MOVE WS-LF-OW-REF-COMM   (T1) TO OR-OW-REF-COMM (T1)
101415        MOVE WS-LF-ACCT-REF-COMM-ncb (T1)
101415                                      TO OR-ACCT-REF-COMM-ncb (T1)
101415        MOVE WS-LF-OW-REF-COMM-ncb (T1)
101415                                      TO OR-OW-REF-COMM-ncb (T1)
              MOVE WS-LF-ORIG-TERM     (T1) TO OR-ORIG-TERM (T1)
              MOVE WS-LF-ISS-CNT       (T1) TO OR-ISS-CNT (T1)
              MOVE WS-LF-COV-ISS-CNT   (T1) TO OR-COV-ISS-CNT (T1)
              MOVE WS-LF-REF-CNT       (T1) TO OR-REF-CNT (T1)
              MOVE WS-LF-COV-REF-CNT   (T1) TO OR-COV-REF-CNT (T1)
           END-PERFORM

           PERFORM VARYING L1 FROM +1 BY +1 UNTIL
             (L1 > ML1)
                  OR
             ((WSL-ST-CODE        (L1) = CR-STATE)
             AND (WSL-ST-CLASS    (L1) = CR-RATING-CLASS)
             AND (WSL-ST-DEV      (L1) = CR-LF-DEV-CODE)
             AND (WSL-L-AH        (L1) = 'L')
             AND (WSL-LAH-NUM     (L1) = CR-LFTYP)
             AND (WSL-HIGH-AMT    (L1) > (CR-LFAMT + CR-LFAMT-ALT))
             AND (WSL-EXPIRY-DATE (L1) > CR-DT))
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
              MOVE 99991231             TO OR-RATE-FILE-EXP-DT
101415        if cr-account (9:2) numeric
101415           DISPLAY ' NO LF RATE RECORD FOR ' CR-CARRIER ' '
101415              CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO ' '
101415              CR-RATING-CLASS ' ' CR-LF-DEV-CODE ' ' CR-LFTYP
101415        end-if
           END-IF

           WRITE EXTRACT-RECORD-OUT    FROM WS-WORK-RECORD
           ADD 1 TO EXTR-RECS-OUT

           .
       0085-EXIT.
           EXIT.

       0087-GET-AH-RATE-KEY.

           MOVE WS-WORK-INIT           TO WS-WORK-RECORD

           MOVE WS-RUN-DATE (4:6)      TO OR-VAL-CCYYMM
           MOVE CR-CARRIER             TO OR-CARRIER
           MOVE CR-STATE               TO OR-STATE
           MOVE CR-ACCOUNT             TO OR-ACCOUNT
           MOVE CR-RATING-CLASS        TO OR-CLASS
           MOVE CR-AH-DEV-CODE         TO OR-DEVIATION
           MOVE 'A'                    TO OR-TYPE
           MOVE CR-AHTYP               TO OR-BEN-CODE
           MOVE '1'                    TO OR-CREDIT-SW

           MOVE CR-DT                  TO WS-WORK-DATE
           MOVE WS-WORK-CCYY           TO OR-EFF-CCYY

           SET WS-AH-NDX               TO +1

           SEARCH WS-AH VARYING WS-AH-NDX AT END
                CONTINUE
             WHEN WS-AH-BEN (WS-AH-NDX) = CR-AHTYP
                SET OR-NON-CREDIT      TO TRUE
           END-SEARCH

           EVALUATE CR-AH-TERM
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

           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              T1 > +4
              MOVE WS-AH-ORIG-BENEFIT  (T1) TO OR-ORIG-BENEFIT (T1)
              MOVE WS-AH-GROSS-PREM    (T1) TO OR-GROSS-PREM (T1)
              MOVE WS-AH-REFUNDED-PREM (T1) TO OR-REFUNDED-PREM (T1)
              MOVE WS-AH-ACCT-COMM     (T1) TO OR-ACCT-COMM (T1)
              MOVE WS-AH-OW-COMM       (T1) TO OR-OW-COMM (T1)
              MOVE WS-AH-ACCT-REF-COMM (T1) TO OR-ACCT-REF-COMM (T1)
              MOVE WS-AH-OW-REF-COMM   (T1) TO OR-OW-REF-COMM (T1)
101415        MOVE WS-AH-ACCT-REF-COMM-ncb (T1)
101415                                      TO OR-ACCT-REF-COMM-ncb (T1)
101415        MOVE WS-AH-OW-REF-COMM-ncb (T1)
101415                                      TO OR-OW-REF-COMM-ncb (T1)
              MOVE WS-AH-ORIG-TERM     (T1) TO OR-ORIG-TERM (T1)
              MOVE WS-AH-ISS-CNT       (T1) TO OR-ISS-CNT (T1)
              MOVE WS-AH-COV-ISS-CNT   (T1) TO OR-COV-ISS-CNT (T1)
              MOVE WS-AH-REF-CNT       (T1) TO OR-REF-CNT (T1)
              MOVE WS-AH-COV-REF-CNT   (T1) TO OR-COV-REF-CNT (T1)
           END-PERFORM

           PERFORM VARYING A1 FROM +1 BY +1 UNTIL
             (A1 > MA1)
                  OR
             ((WSA-ST-CODE        (A1) = CR-STATE)
             AND (WSA-ST-CLASS    (A1) = CR-RATING-CLASS)
             AND (WSA-ST-DEV      (A1) = CR-AH-DEV-CODE)
             AND (WSA-L-AH        (A1) = 'A')
             AND (WSA-LAH-NUM     (A1) = CR-AHTYP)
             AND (WSA-HIGH-AMT    (A1) > CR-AHAMT)
             AND (WSA-EXPIRY-DATE (A1) > CR-DT))
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
              MOVE 99991231             TO OR-RATE-FILE-EXP-DT
101415        if cr-account (9:2) numeric
101415           DISPLAY ' NO AH RATE RECORD FOR ' CR-CARRIER ' '
101415              CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO ' '
101415              CR-RATING-CLASS ' ' CR-AH-DEV-CODE ' ' CR-AHTYP
101415        end-if
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
