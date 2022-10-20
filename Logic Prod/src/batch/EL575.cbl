       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL575.
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
101415******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-IN              ASSIGN TO SYS010.

           SELECT EXTR-OUT             ASSIGN TO SYS011
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE            ASSIGN TO SYS019.

           SELECT PRINTX               ASSIGN TO SYS008.

       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-IN.
       
       01  WS-IN-RECORD.
         02  WS-IN-KEY.
           05  IR-VAL-CCYYMM           PIC 9(6).
           05  IR-CARRIER              PIC X.
           05  IR-STATE                PIC XX.
           05  IR-ACCOUNT              PIC X(10).
           05  IR-CLASS                PIC XX.
           05  IR-DEVIATION            PIC XXX.
           05  IR-TYPE                 PIC X.
               88  IR-LIFE                 VALUE 'L'.
               88  IR-AH                   VALUE 'A'.
           05  IR-BEN-CODE             PIC XX.
           05  IR-RATE-FILE-AMT        PIC 9(9).
           05  IR-RATE-FILE-EXP-DT     PIC 9(8).
           05  IR-CREDIT-SW            PIC X.
           05  IR-EFF-CCYY             PIC 9999.
           05  IR-TERM-GROUP           PIC 999.
         02  WS-IN-REST.
           05  IR-12-MO-RATE           PIC S99V9(5) COMP-3.
           05  IR-72-MO-RATE           PIC S99V9(5) COMP-3.
           05  IR-MOB-RATE             PIC S99V9(5) COMP-3.
           05  FILLER OCCURS 4.
               10  IR-UEP-R78          PIC S9(11)V99 COMP-3.
               10  IR-UEP-PRO          PIC S9(11)V99 COMP-3.
               10  IR-UEP-MEAN         PIC S9(11)V99 COMP-3.
               10  IR-UEP-STAT         PIC S9(11)V99 COMP-3.
               10  IR-REM-BEN          PIC S9(11)V99 COMP-3.
               10  IR-MORT-RESV        PIC S9(11)V99 COMP-3.
               10  IR-IBNR-RESV        PIC S9(11)V99 COMP-3.
               10  IR-REM-TERM         PIC S9(9)     COMP-3.
               10  IR-INF-COV          PIC S9(9)     COMP-3.
               10  IR-INF-CRT          PIC S9(9)     COMP-3.

       FD  EXTR-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

101415 01  EXTRACT-RECORD-OUT          PIC X(671).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  PRINTX
                                       COPY ELCPRTFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '       EL575  WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  EXTR-RECS-IN                PIC S9(9)  COMP-3 VALUE +0.
       77  EXTR-RECS-OUT               PIC 9(9)   VALUE ZEROS.
       77  T1                          PIC S999 VALUE +0 COMP-3.
       01  WS-OUT-RECORD.
         02  WS-OUT-KEY.
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
           05  OR-EXP-DT REDEFINES OR-RATE-FILE-EXP-DT
                                       PIC X(8).
           05  OR-CREDIT-SW            PIC X.
           05  OR-EFF-CCYY             PIC 9999.
           05  OR-TERM-GROUP           PIC 999.
         02  WS-OUT-REST.
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


101415 01  WS-INIT-EX-RECORD           PIC X(671) VALUE SPACES.

       01  EX-RECORD.
           05  EX-VAL-CCYYMM           PIC 9(6).
           05  EX-TAB1                 PIC X.
101415     05  ex-val-yr               pic 9(4).
101415     05  ex-tab1a                pic x.
101415     05  ex-val-mo               pic 99.
101415     05  ex-tab1b                pic x.
           05  EX-CARRIER              PIC X.
           05  EX-TAB2                 PIC X.
           05  EX-STATE                PIC XX.
           05  EX-TAB3                 PIC X.
           05  EX-ACCOUNT              PIC X(10).
           05  EX-TAB4                 PIC X.
           05  EX-CLASS                PIC XX.
           05  EX-TAB5                 PIC X.
           05  EX-DEVIATION            PIC XXX.
           05  EX-TAB6                 PIC X.
           05  EX-TYPE                 PIC X.
           05  EX-TAB7                 PIC X.
           05  EX-BEN-CODE             PIC XX.
           05  EX-TAB8                 PIC X.
           05  EX-RATE-FILE-AMT        PIC 9(9).
           05  EX-TAB9                 PIC X.
           05  EX-RATE-FILE-EXP-DT     PIC X(10).
           05  EX-TAB10                PIC X.
           05  EX-CREDIT-SW            PIC X.
           05  EX-TAB11                PIC X.
           05  EX-EFF-CCYY             PIC 9999.
           05  EX-TAB12                PIC X.
           05  EX-TERM-GROUP           PIC 999.
           05  EX-TAB13                PIC X.
           05  EX-12-MO-RATE           PIC 99.9(5).
           05  EX-TAB14                PIC X.
           05  EX-72-MO-RATE           PIC 99.9(5).
           05  EX-TAB15                PIC X.
           05  EX-MOB-RATE             PIC 99.9(5).
           05  EX-TAB26                PIC X.
           05  OCCURS 4.
               10  EX-UEP-R78          PIC -9(11).99.
               10  EX-TAB16            PIC X.
               10  EX-UEP-PRO          PIC -9(11).99.
               10  EX-TAB17            PIC X.
               10  EX-UEP-MEAN         PIC -9(11).99.
               10  EX-TAB18            PIC X.
               10  EX-UEP-STAT         PIC -9(11).99.
               10  EX-TAB19            PIC X.
               10  EX-REM-BEN          PIC -9(11).99.
               10  EX-TAB20            PIC X.
               10  EX-MORT-RESV        PIC -9(11).99.
               10  EX-TAB21            PIC X.
               10  EX-IBNR-RESV        PIC -9(11).99.
               10  EX-TAB22            PIC X.
               10  EX-REM-TERM         PIC 9(9).
               10  EX-TAB23            PIC X.
               10  EX-INF-COV          PIC 9(9).
               10  EX-TAB24            PIC X.
               10  EX-INF-CRT          PIC 9(9).
               10  EX-TAB25            PIC X.
           05  EX-EOR                  PIC X.

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

           PERFORM 0100-WRITE-EXTRACT  THRU 0100-EXIT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' TOTAL EXTR RECORDS READ ' EXTR-RECS-IN
           DISPLAY ' TOTAL EXTR RECORDS OUT  ' EXTR-RECS-OUT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT EXTR-IN
               OUTPUT EXTR-OUT

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           MOVE SPACES                 TO EX-RECORD
           MOVE ZEROS                  TO EX-RATE-FILE-AMT
                                          EX-RATE-FILE-EXP-DT
           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              T1 > +4
              MOVE ZEROS               TO EX-UEP-R78   (T1)
                                          EX-UEP-PRO   (T1)
                                          EX-UEP-MEAN  (T1)
                                          EX-UEP-STAT  (T1)
                                          EX-REM-BEN   (T1)
                                          EX-MORT-RESV (T1)
                                          EX-REM-TERM  (T1)
                                          EX-INF-COV   (T1)
                                          EX-INF-CRT   (T1)
           END-PERFORM
           MOVE ';'                    TO EX-TAB1
101415                                    EX-TAB1a
101415                                    EX-TAB1b
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
                                          EX-TAB26
           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              T1 > +4
              MOVE ';'                 TO EX-TAB16 (T1)
                                          EX-TAB17 (T1)
                                          EX-TAB18 (T1)
                                          EX-TAB19 (T1)
                                          EX-TAB20 (T1)
                                          EX-TAB21 (T1)
                                          EX-TAB22 (T1)
                                          EX-TAB23 (T1)
                                          EX-TAB24 (T1)
                                          EX-TAB25 (T1)
           END-PERFORM
           MOVE 'E'                    TO EX-EOR
           MOVE EX-RECORD              TO WS-INIT-EX-RECORD

           PERFORM 0060-READ-EXTR      THRU 0060-EXIT
           MOVE WS-IN-RECORD           TO WS-OUT-RECORD

           PERFORM 0060-READ-EXTR      THRU 0060-EXIT

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE EXTR-IN EXTR-OUT

           .
       0030-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           PERFORM 0080-PROCESS-EXTR   THRU 0080-EXIT

           PERFORM 0060-READ-EXTR      THRU 0060-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-READ-EXTR.

           READ EXTR-IN AT END
               SET END-OF-INPUT        TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO EXTR-RECS-IN
           END-IF

           .
       0060-EXIT.
           EXIT.

       0080-PROCESS-EXTR.

           IF WS-IN-KEY = WS-OUT-KEY
              PERFORM VARYING T1 FROM +1 BY +1 UNTIL
                 T1 > +4
                 COMPUTE OR-UEP-R78   (T1) = OR-UEP-R78   (T1) +
                    IR-UEP-R78 (T1)
                 COMPUTE OR-UEP-PRO   (T1) = OR-UEP-PRO   (T1) +
                    IR-UEP-PRO (T1)
                 COMPUTE OR-UEP-MEAN  (T1) = OR-UEP-MEAN  (T1) +
                    IR-UEP-MEAN (T1)
                 COMPUTE OR-UEP-STAT  (T1) = OR-UEP-STAT  (T1) +
                    IR-UEP-STAT  (T1)
                 COMPUTE OR-REM-BEN   (T1) = OR-REM-BEN   (T1) +
                    IR-REM-BEN   (T1)
                 COMPUTE OR-MORT-RESV (T1) = OR-MORT-RESV (T1) +
                    IR-MORT-RESV (T1)
                 COMPUTE OR-IBNR-RESV (T1) = OR-IBNR-RESV (T1) +
                    IR-IBNR-RESV (T1)
                 COMPUTE OR-REM-TERM  (T1) = OR-REM-TERM  (T1) +
                    IR-REM-TERM (T1)
                 COMPUTE OR-INF-COV   (T1) = OR-INF-COV   (T1) +
                    IR-INF-COV   (T1)
                 COMPUTE OR-INF-CRT   (T1) = OR-INF-CRT   (T1) +
                    IR-INF-CRT   (T1)
              END-PERFORM
           ELSE
              PERFORM 0100-WRITE-EXTRACT
                                       THRU 0100-EXIT
              MOVE WS-IN-RECORD        TO WS-OUT-RECORD
           END-IF

           .
       0080-EXIT.
           EXIT.

       0100-WRITE-EXTRACT.

           MOVE WS-INIT-EX-RECORD      TO EX-RECORD

           MOVE OR-VAL-CCYYMM          TO EX-VAL-CCYYMM
101415     move ex-val-ccyymm (1:4)    to ex-val-yr
101415     move ex-val-ccyymm (5:2)    to ex-val-mo
           MOVE OR-CARRIER             TO EX-CARRIER
           MOVE OR-STATE               TO EX-STATE
           MOVE OR-ACCOUNT             TO EX-ACCOUNT
           MOVE OR-CLASS               TO EX-CLASS
           MOVE OR-DEVIATION           TO EX-DEVIATION
           MOVE OR-TYPE                TO EX-TYPE
           MOVE OR-BEN-CODE            TO EX-BEN-CODE
           MOVE OR-RATE-FILE-AMT       TO EX-RATE-FILE-AMT
           MOVE OR-EXP-DT (5:2)        TO EX-RATE-FILE-EXP-DT (1:2)
           MOVE OR-EXP-DT (7:2)        TO EX-RATE-FILE-EXP-DT (4:2)
           MOVE OR-EXP-DT (1:4)        TO EX-RATE-FILE-EXP-DT (7:4)
           MOVE '/'                    TO EX-RATE-FILE-EXP-DT (3:1)
                                          EX-RATE-FILE-EXP-DT (6:1)
           MOVE OR-CREDIT-SW           TO EX-CREDIT-SW
           MOVE OR-EFF-CCYY            TO EX-EFF-CCYY
           MOVE OR-TERM-GROUP          TO EX-TERM-GROUP
           MOVE OR-12-MO-RATE          TO EX-12-MO-RATE
           MOVE OR-72-MO-RATE          TO EX-72-MO-RATE
           MOVE OR-MOB-RATE            TO EX-MOB-RATE
           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              T1 > +4
              MOVE OR-UEP-R78   (T1)   TO EX-UEP-R78   (T1)
              MOVE OR-UEP-PRO   (T1)   TO EX-UEP-PRO   (T1)
              MOVE OR-UEP-MEAN  (T1)   TO EX-UEP-MEAN  (T1)
              MOVE OR-UEP-STAT  (T1)   TO EX-UEP-STAT  (T1)
              MOVE OR-REM-BEN   (T1)   TO EX-REM-BEN   (T1)
              MOVE OR-MORT-RESV (T1)   TO EX-MORT-RESV (T1)
              MOVE OR-IBNR-RESV (T1)   TO EX-IBNR-RESV (T1)
              MOVE OR-REM-TERM  (T1)   TO EX-REM-TERM  (T1)
              MOVE OR-INF-COV   (T1)   TO EX-INF-COV   (T1)
              MOVE OR-INF-CRT   (T1)   TO EX-INF-CRT   (T1)
           END-PERFORM

           WRITE EXTRACT-RECORD-OUT    FROM EX-RECORD
           ADD 1                       TO EXTR-RECS-OUT

           .
       0100-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
