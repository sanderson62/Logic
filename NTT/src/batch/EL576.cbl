       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL576.
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
                            

120121******************************************************************
120121*                   C H A N G E   L O G
120121*
120121* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120121*-----------------------------------------------------------------
120121*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120121* EFFECTIVE    NUMBER
120121*-----------------------------------------------------------------
120121* 120121  IR2021120100001  PEMA  Increase size of rate table.
120121******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RESV                 ASSIGN TO SYS010.

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

       FD  RESV
                                       COPY ECSEXTFD.
                                       COPY ECSEXT01.

       FD  EXTRACT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  EXTRACT-RECORD-OUT          PIC X(120).

       FD  ERRATE.
                                       COPY ERCRATE.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  PRINTX
                                       COPY ELCPRTFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '       EL576  WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  WS-RATE-SW                  PIC X  VALUE SPACES.
           88  END-OF-RATES               VALUE 'Y'.
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

       01  WS-LIFE-INFO.
           05  WS-LF-DEV               PIC XXX.
           05  WS-LF-BEN-CODE          PIC XX.
           05  WS-LF-RATE-AMT          PIC 9(9).
           05  WS-LF-EXPIRE-DATE       PIC 9(8).
           05  FILLER OCCURS 4.
               10  WS-LF-PTC           PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-LF-FUTURE        PIC S9(7)V99 COMP-3 VALUE +0.

       01  WS-AH-INFO.
           05  WS-AH-DEV               PIC XXX.
           05  WS-AH-BEN-CODE          PIC XX.
           05  WS-AH-RATE-AMT          PIC 9(9).
           05  WS-AH-EXPIRE-DATE       PIC 9(8).
           05  FILLER OCCURS 4.
               10  WS-AH-PTC           PIC S9(7)V99 COMP-3 VALUE +0.
               10  WS-AH-FUTURE        PIC S9(7)V99 COMP-3 VALUE +0.

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
               10  OR-PTC              PIC S9(11)V99 COMP-3 VALUE +0.
               10  OR-FUTURE           PIC S9(11)V99 COMP-3 VALUE +0.

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
           05  WS-WORK-DATE            PIC 9(11).
           05  FILLER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-WORK-CCYYMM      PIC 9(6).
               10  FILLER REDEFINES WS-WORK-CCYYMM.
                   15  WS-WORK-CCYY    PIC 9999.
                   15  WS-WORK-MM      PIC 99.
               10  WS-WORK-DD          PIC 99.
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
           05  WS-PME-DATE             PIC 9(11).
           05  FILLER REDEFINES WS-PME-DATE.
               10  FILLER              PIC XXX.
               10  WS-PME-CCYYMM       PIC 9(6).
               10  FILLER REDEFINES WS-PME-CCYYMM.
                   15  WS-PME-CCYY     PIC 9999.
                   15  WS-PME-MM       PIC 99.
               10  WS-PME-DD           PIC 99.
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

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' TOTAL RESV RECORDS READ ' WS-CRT-CNT-TOT
           DISPLAY ' TOTAL EXTR RECORDS OUT  ' EXTR-RECS-OUT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT RESV ERRATE
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

           MOVE BIN-RUN-DATE           TO DC-BIN-DATE-1
           MOVE -1                     TO DC-ELAPSED-MONTHS
           MOVE +0                     TO DC-ELAPSED-DAYS
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

           INITIALIZE WS-WORK-RECORD

           PERFORM 0021-START-ERRATE   THRU 0021-EXIT
           PERFORM 0022-READ-ERRATE    THRU 0022-EXIT
           PERFORM 0025-LOAD-RATES     THRU 0025-EXIT UNTIL
              END-OF-RATES

           MOVE A1                     TO MA1
           MOVE L1                     TO ML1
           DISPLAY ' LIFE RATES ' ML1
           DISPLAY '   AH RATES ' MA1
           PERFORM 0060-READ-RESV      THRU 0060-EXIT

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

           CLOSE RESV EXTRACT ERRATE

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

           IF (DE-REIN NOT = 'R')
              AND (DE-RESERVE)
      *       AND (DE-RSV-PROC-DT = RUN-DATE)
              PERFORM 0080-PROCESS-RESV
                                      THRU 0080-EXIT
           END-IF

           PERFORM 0060-READ-RESV      THRU 0060-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-READ-RESV.

           READ RESV AT END
               SET END-OF-INPUT        TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-CRT-CNT
                                          WS-CRT-CNT-TOT
              IF WS-CRT-CNT = +10000
                 DISPLAY ' RESVS READ ' WS-CRT-CNT-TOT
                 MOVE +0               TO WS-CRT-CNT
              END-IF
              IF DE-CERT-NO = '0000109489 '
                 DISPLAY ' FOUND BAD CERT '
              END-IF
           END-IF

           .
       0060-EXIT.
           EXIT.

       0080-PROCESS-RESV.

           INITIALIZE                  WS-LIFE-INFO
                                       WS-AH-INFO

      ***********   YTD   PROCESSING   HERE   **********

           IF DE-RSV-PROC-DT = WS-PYE-DATE
              MOVE +1                  TO T1
              IF DE-LIFE-RSV
                 IF DE-LF-TYPE NOT = '00' AND '  '
                    PERFORM 0081-ACCUM-LIFE-ISS
                                       THRU 0081-EXIT
                 END-IF
              ELSE
                 IF DE-AH-TYPE NOT = '00' AND '  '
                    PERFORM 0082-ACCUM-AH-ISS
                                       THRU 0082-EXIT
                 END-IF
              END-IF
           END-IF

      ***********   YTD   PROCESSING   HERE   **********

      ***********   MTD   PROCESSING   HERE   **********

           IF DE-RSV-PROC-DT = WS-PME-DATE
              MOVE +2                  TO T1
              IF DE-LIFE-RSV
                 IF DE-LF-TYPE NOT = '00' AND '  '
                    PERFORM 0081-ACCUM-LIFE-ISS
                                       THRU 0081-EXIT
                 END-IF
              ELSE
                 IF DE-AH-TYPE NOT = '00' AND '  '
                    PERFORM 0082-ACCUM-AH-ISS
                                       THRU 0082-EXIT
                 END-IF
              END-IF
           END-IF

      ***********   MTD   PROCESSING   HERE   **********

      ***********   L12   PROCESSING   HERE   **********

           IF DE-RSV-PROC-DT = WS-L12-DATE
              MOVE +3                  TO T1
              IF DE-LIFE-RSV
                 IF DE-LF-TYPE NOT = '00' AND '  '
                    PERFORM 0081-ACCUM-LIFE-ISS
                                       THRU 0081-EXIT
                 END-IF
              ELSE
                 IF DE-AH-TYPE NOT = '00' AND '  '
                    PERFORM 0082-ACCUM-AH-ISS
                                       THRU 0082-EXIT
                 END-IF
              END-IF
           END-IF

      ***********   L12   PROCESSING   HERE   **********

      ***********   ITD   PROCESSING   HERE   **********

           IF DE-RSV-PROC-DT = WS-RUN-DATE-N
              MOVE +4                  TO T1
              IF DE-LIFE-RSV
                 IF DE-LF-TYPE NOT = '00' AND '  '
                    PERFORM 0081-ACCUM-LIFE-ISS
                                       THRU 0081-EXIT
                 END-IF
              ELSE
                 IF DE-AH-TYPE NOT = '00' AND '  '
                    PERFORM 0082-ACCUM-AH-ISS
                                       THRU 0082-EXIT
                 END-IF
              END-IF
           END-IF

      ***********   ITD   PROCESSING   HERE   **********


           IF (WS-LF-PTC (1) > ZEROS)
              OR (WS-LF-FUTURE (1) > ZEROS)
              OR (WS-LF-PTC (2) > ZEROS)
              OR (WS-LF-FUTURE (2) > ZEROS)
              OR (WS-LF-PTC (3) > ZEROS)
              OR (WS-LF-FUTURE (3) > ZEROS)
              OR (WS-LF-PTC (4) > ZEROS)
              OR (WS-LF-FUTURE (4) > ZEROS)
              PERFORM 0085-GET-LF-RATE-KEY
                                       THRU 0085-EXIT
           END-IF

           IF (WS-AH-PTC (1) > ZEROS)
              OR (WS-AH-FUTURE (1) > ZEROS)
              OR (WS-AH-PTC (2) > ZEROS)
              OR (WS-AH-FUTURE (2) > ZEROS)
              OR (WS-AH-PTC (3) > ZEROS)
              OR (WS-AH-FUTURE (3) > ZEROS)
              OR (WS-AH-PTC (4) > ZEROS)
              OR (WS-AH-FUTURE (4) > ZEROS)
              PERFORM 0087-GET-AH-RATE-KEY
                                       THRU 0087-EXIT
           END-IF

           .
       0080-EXIT.
           EXIT.

       0081-ACCUM-LIFE-ISS.

           MOVE DE-PAYCUR              TO WS-LF-PTC (T1)
           MOVE DE-FUTRSV              TO WS-LF-FUTURE (T1)

           .
       0081-EXIT.
           EXIT.

       0082-ACCUM-AH-ISS.

           MOVE DE-PAYCUR              TO WS-AH-PTC (T1)
           MOVE DE-FUTRSV              TO WS-AH-FUTURE (T1)

           .
       0082-EXIT.
           EXIT.

       0085-GET-LF-RATE-KEY.

           INITIALIZE                  WS-WORK-RECORD

           MOVE WS-RUN-DATE (4:6)      TO OR-VAL-CCYYMM
           MOVE DE-CARRIER             TO OR-CARRIER
           MOVE DE-STATE               TO OR-STATE
           MOVE DE-ACCOUNT             TO OR-ACCOUNT
           MOVE DE-RATE-CLASS          TO OR-CLASS
           MOVE DE-DEV-CODE            TO OR-DEVIATION
           MOVE 'L'                    TO OR-TYPE
           MOVE DE-LF-TYPE             TO OR-BEN-CODE

           MOVE '1'                    TO OR-CREDIT-SW
           MOVE DE-EFF                 TO WS-WORK-DATE
           MOVE WS-WORK-CCYY           TO OR-EFF-CCYY
           SET WS-LF-NDX               TO +1

           SEARCH WS-LIFE VARYING WS-LF-NDX AT END
                CONTINUE
             WHEN WS-LIFE-BEN (WS-LF-NDX) = DE-LF-TYPE
                SET OR-NON-CREDIT      TO TRUE
           END-SEARCH

           EVALUATE DE-LF-TERM
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
              MOVE WS-LF-PTC (T1)      TO OR-PTC (T1)
              MOVE WS-LF-FUTURE (T1)   TO OR-FUTURE (T1)
           END-PERFORM

           PERFORM VARYING L1 FROM +1 BY +1 UNTIL
             (L1 > ML1)
                  OR
             ((WSL-ST-CODE        (L1) = OR-STATE)
             AND (WSL-ST-CLASS    (L1) = OR-CLASS)
             AND (WSL-ST-DEV      (L1) = OR-DEVIATION)
             AND (WSL-L-AH        (L1) = 'L')
             AND (WSL-LAH-NUM     (L1) = DE-LF-TYPE)
             AND (WSL-HIGH-AMT    (L1) > DE-LF-BEN)
             AND (WSL-EXPIRY-DATE (L1) > DE-EFF))
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
                 OR-STATE ' ' OR-ACCOUNT ' ' DE-CERT-NO ' ' OR-CLASS
                 ' ' OR-DEVIATION ' ' DE-LF-TYPE
           END-IF

           WRITE EXTRACT-RECORD-OUT    FROM WS-WORK-RECORD
           ADD 1 TO EXTR-RECS-OUT

           .
       0085-EXIT.
           EXIT.

       0087-GET-AH-RATE-KEY.

           INITIALIZE                  WS-WORK-RECORD

           MOVE WS-RUN-DATE (4:6)      TO OR-VAL-CCYYMM
           MOVE DE-CARRIER             TO OR-CARRIER
           MOVE DE-STATE               TO OR-STATE
           MOVE DE-ACCOUNT             TO OR-ACCOUNT
           MOVE DE-RATE-CLASS          TO OR-CLASS
           MOVE DE-DEV-CODE            TO OR-DEVIATION
           MOVE 'A'                    TO OR-TYPE
           MOVE DE-AH-TYPE             TO OR-BEN-CODE

           MOVE '1'                    TO OR-CREDIT-SW
           MOVE DE-EFF                 TO WS-WORK-DATE
           MOVE WS-WORK-CCYY           TO OR-EFF-CCYY

           SET WS-AH-NDX               TO +1

           SEARCH WS-AH VARYING WS-AH-NDX AT END
                CONTINUE
             WHEN WS-AH-BEN (WS-AH-NDX) = DE-AH-TYPE
                SET OR-NON-CREDIT      TO TRUE
           END-SEARCH

           EVALUATE DE-AH-TERM
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
              MOVE WS-AH-PTC (T1)      TO OR-PTC (T1)
              MOVE WS-AH-FUTURE (T1)   TO OR-FUTURE (T1)
           END-PERFORM
           PERFORM VARYING A1 FROM +1 BY +1 UNTIL
             (A1 > MA1)
                  OR
             ((WSA-ST-CODE        (A1) = OR-STATE)
             AND (WSA-ST-CLASS    (A1) = OR-CLASS)
             AND (WSA-ST-DEV      (A1) = OR-DEVIATION)
             AND (WSA-L-AH        (A1) = 'A')
             AND (WSA-LAH-NUM     (A1) = DE-AH-TYPE)
             AND (WSA-HIGH-AMT    (A1) > DE-AH-BEN)
             AND (WSA-EXPIRY-DATE (A1) > DE-EFF))
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
                 OR-STATE ' ' OR-ACCOUNT ' ' DE-CERT-NO ' ' OR-CLASS
                 ' ' OR-DEVIATION ' ' DE-LF-TYPE
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
