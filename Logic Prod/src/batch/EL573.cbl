       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EL573.
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
              10  IR-ORIG-BENEFIT      PIC S9(13)V99 COMP-3.
              10  IR-GROSS-PREM        PIC S9(11)V99 COMP-3.
              10  IR-REFUNDED-PREM     PIC S9(11)V99 COMP-3.
              10  IR-ACCT-COMM         PIC S9(11)V99 COMP-3.
              10  IR-OW-COMM           PIC S9(11)V99 COMP-3.
              10  IR-ACCT-REF-COMM     PIC S9(11)V99 COMP-3.
              10  IR-OW-REF-COMM       PIC S9(11)V99 COMP-3.
101415        10  IR-ACCT-REF-COMM-ncb PIC S9(11)V99 COMP-3.
101415        10  IR-OW-REF-COMM-ncb   PIC S9(11)V99 COMP-3.
              10  IR-ORIG-TERM         PIC S9(9)     COMP-3.
              10  IR-ISS-CNT           PIC S9(9)     COMP-3.
              10  IR-COV-ISS-CNT       PIC S9(9)     COMP-3.
              10  IR-REF-CNT           PIC S9(9)     COMP-3.
              10  IR-COV-REF-CNT       PIC S9(9)     COMP-3.

       FD  EXTR-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

101415 01  EXTRACT-RECORD-OUT          PIC X(895).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  PRINTX
                                       COPY ELCPRTFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '       EL573  WORKING STORAGE   '.
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


101415 01  WS-INIT-EX-RECORD           PIC X(895) VALUE SPACES.

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
           05  EX-TAB28                PIC X.
           05  FILLER OCCURS 4.
               10  EX-ORIG-BENEFIT     PIC -9(13).
               10  EX-TAB16            PIC X.
               10  EX-GROSS-PREM       PIC -9(11).99.
               10  EX-TAB17            PIC X.
               10  EX-REFUNDED-PREM    PIC -9(11).99.
               10  EX-TAB18            PIC X.
               10  EX-ACCT-COMM        PIC -9(11).99.
               10  EX-TAB19            PIC X.
               10  EX-OW-COMM          PIC -9(11).99.
               10  EX-TAB20            PIC X.
               10  EX-ACCT-REF-COMM    PIC -9(11).99.
               10  EX-TAB21            PIC X.
               10  EX-OW-REF-COMM      PIC -9(11).99.
               10  EX-TAB22            PIC X.
101415         10  EX-ACCT-REF-COMM-ncb
101415                                 PIC -9(11).99.
101415         10  EX-TAB21a           PIC X.
101415         10  EX-OW-REF-COMM-ncb  PIC -9(11).99.
101415         10  EX-TAB22a           PIC X.
               10  EX-ORIG-TERM        PIC -9(9).
               10  EX-TAB23            PIC X.
               10  EX-ISS-CNT          PIC -9(9).
               10  EX-TAB24            PIC X.
               10  EX-COV-ISS-CNT      PIC -9(9).
               10  EX-TAB25            PIC X.
               10  EX-REF-CNT          PIC -9(9).
               10  EX-TAB26            PIC X.
               10  EX-COV-REF-CNT      PIC -9(9).
               10  EX-TAB27            PIC X.
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
                                          EX-12-MO-RATE
                                          EX-72-MO-RATE
                                          EX-MOB-RATE
           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              T1 > +4
              MOVE +0                  TO EX-ORIG-BENEFIT      (T1)
                                          EX-GROSS-PREM        (T1)
                                          EX-REFUNDED-PREM     (T1)
                                          EX-ACCT-COMM         (T1)
                                          EX-OW-COMM           (T1)
                                          EX-ACCT-REF-COMM     (T1)
                                          EX-OW-REF-COMM       (T1)
101415                                    EX-ACCT-REF-COMM-ncb (T1)
101415                                    EX-OW-REF-COMM-ncb   (T1)
                                          EX-ORIG-TERM         (T1)
                                          EX-ISS-CNT           (T1)
                                          EX-COV-ISS-CNT       (T1)
                                          EX-REF-CNT           (T1)
                                          EX-COV-REF-CNT       (T1)
           END-PERFORM
           MOVE ';'                    TO EX-TAB1
101415                                    ex-tab1a
101415                                    ex-tab1b
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
                                          EX-TAB28
           PERFORM VARYING T1 FROM +1 BY +1 UNTIL
              T1 > +4
              MOVE ';'                 TO EX-TAB16  (T1)
                                          EX-TAB17  (T1)
                                          EX-TAB18  (T1)
                                          EX-TAB19  (T1)
                                          EX-TAB20  (T1)
                                          EX-TAB21  (T1)
                                          EX-TAB22  (T1)
101415                                    EX-TAB21a (T1)
101415                                    EX-TAB22a (T1)
                                          EX-TAB23  (T1)
                                          EX-TAB24  (T1)
                                          EX-TAB25  (T1)
                                          EX-TAB26  (T1)
                                          EX-TAB27  (T1)
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
                 COMPUTE OR-ORIG-BENEFIT (T1) =
                    OR-ORIG-BENEFIT (T1) + IR-ORIG-BENEFIT (T1)

                 COMPUTE OR-GROSS-PREM (T1) =
                    OR-GROSS-PREM (T1) + IR-GROSS-PREM (T1)

                 COMPUTE OR-REFUNDED-PREM (T1) =
                    OR-REFUNDED-PREM (T1) + IR-REFUNDED-PREM (T1)

                 COMPUTE OR-ACCT-COMM (T1) =
                    OR-ACCT-COMM (T1) + IR-ACCT-COMM (T1)

                 COMPUTE OR-OW-COMM (T1) =
                    OR-OW-COMM (T1) + IR-OW-COMM (T1)

                 COMPUTE OR-ACCT-REF-COMM (T1) =
                    OR-ACCT-REF-COMM (T1) + IR-ACCT-REF-COMM (T1)

                 COMPUTE OR-OW-REF-COMM (T1) =
                    OR-OW-REF-COMM (T1) + IR-OW-REF-COMM (T1)

101415           COMPUTE OR-ACCT-REF-COMM-ncb (T1) =
101415              OR-ACCT-REF-COMM-ncb (T1) +
101415                 IR-ACCT-REF-COMM-ncb (T1)
101415
101415           COMPUTE OR-OW-REF-COMM-ncb (T1) =
101415              OR-OW-REF-COMM-ncb (T1) + IR-OW-REF-COMM-ncb (T1)

                 COMPUTE OR-ORIG-TERM (T1) =
                    OR-ORIG-TERM (T1) + IR-ORIG-TERM (T1)

                 COMPUTE OR-ISS-CNT (T1) =
                    OR-ISS-CNT (T1) + IR-ISS-CNT (T1)

                 COMPUTE OR-COV-ISS-CNT (T1) =
                    OR-COV-ISS-CNT (T1) + IR-COV-ISS-CNT (T1)

                 COMPUTE OR-REF-CNT (T1) =
                    OR-REF-CNT (T1) + IR-REF-CNT (T1)

                 COMPUTE OR-COV-REF-CNT (T1) =
                    OR-COV-REF-CNT (T1) + IR-COV-REF-CNT (T1)

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
              MOVE OR-ORIG-BENEFIT  (T1) TO EX-ORIG-BENEFIT  (T1)
              MOVE OR-GROSS-PREM    (T1) TO EX-GROSS-PREM    (T1)
              MOVE OR-REFUNDED-PREM (T1) TO EX-REFUNDED-PREM (T1)
              MOVE OR-ACCT-COMM     (T1) TO EX-ACCT-COMM     (T1)
              MOVE OR-OW-COMM       (T1) TO EX-OW-COMM       (T1)
              MOVE OR-ACCT-REF-COMM (T1) TO EX-ACCT-REF-COMM (T1)
              MOVE OR-OW-REF-COMM   (T1) TO EX-OW-REF-COMM   (T1)
101415        MOVE OR-ACCT-REF-COMM-ncb (T1)
101415                                   TO EX-ACCT-REF-COMM-ncb (T1)
101415        MOVE OR-OW-REF-COMM-ncb (T1)
101415                                   TO EX-OW-REF-COMM-ncb   (T1)
              MOVE OR-ORIG-TERM     (T1) TO EX-ORIG-TERM     (T1)
              MOVE OR-ISS-CNT       (T1) TO EX-ISS-CNT       (T1)
              MOVE OR-COV-ISS-CNT   (T1) TO EX-COV-ISS-CNT   (T1)
              MOVE OR-REF-CNT       (T1) TO EX-REF-CNT       (T1)
              MOVE OR-COV-REF-CNT   (T1) TO EX-COV-REF-CNT   (T1)
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
