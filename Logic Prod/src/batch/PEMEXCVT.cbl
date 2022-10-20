       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMEXCVT.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  FILE-IN             ASSIGN TO SYS010.
           SELECT  FILE-OUT            ASSIGN TO SYS011.

       DATA DIVISION.
       FILE SECTION.

       FD  FILE-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  IN-RECORD.
           05  IN-CARRIER              PIC X.
           05  IN-GROUP                PIC X(6).
           05  IN-STATE                PIC XX.
           05  IN-ACCOUNT              PIC X(10).
           05  IN-IG                   PIC X.
           05  IN-TYPE                 PIC XXX.
           05  FILLER                  PIC X(42).

       FD  FILE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  OUT-RECORD                  PIC X(165).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMEXCVT WORKING-STORAGE     '.
       77  FILLER  PIC X(32) VALUE '*********** VMOD=2.001. ********'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-INPUT                  VALUE 'Y'.
       77  RECS-IN                     PIC 9(9) VALUE ZEROS.
       77  RECS-OUT                    PIC 9(9) VALUE ZEROS.
       77  RECS-CVT                    PIC 9(9) VALUE ZEROS.

       01  X-REC.
           12  X-CARRIER           PIC X.
           12  X-GROUPING          PIC X(6).
           12  X-ST                PIC XX.
           12  X-ACCT              PIC X(10).
           12  X-IG                PIC X.
           12  X-TYPE.
               16  X-TYP           PIC XX.
               16  X-OB            PIC X.
           12  X-CODE              PIC 9.
      ***      VALUE 1 - ISSUE OR RECALC ISSUE
      ***      VALUE 2 - CANCEL OR RECALC CANCEL
      ***      VALUE 3 - CLAIM
           12  X-AMT               PIC S9(9)V99   COMP-3.
           12  X-BASE              PIC S9(7)V99   COMP-3.
           12  X-OVER              PIC S9(7)V99   COMP-3.
           12  X-DLR-INC           PIC S9(7)V99   COMP-3.
           12  X-LF-LMBA-FEE       PIC S9(7)V99   COMP-3.
           12  X-AH-LMBA-FEE       PIC S9(7)V99   COMP-3.
           12  X-BANK-FEE          PIC S9(7)V99   COMP-3.
           12  X-PROCESSED         PIC 9(07)      COMP-3.
           12  X-RECALC            PIC X.
           12  X-ACCT-COM-TYPE     PIC X.
           12  X-OWRT-COM-TYPE     PIC X.
           12  X-DMD-RESIDENT-ST   PIC XX.
           12  FILLER              PIC X.
           12  X-SPPDD-CLP         PIC S9(7)V99   COMP-3.
           12  FILLER              PIC X(90).

       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT

           PERFORM 0020-INITIALIZE     THRU 0020-EXIT

           PERFORM 0100-PROCESS-FILE   THRU 0100-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' RECORDS READ      ' RECS-IN
           DISPLAY ' RECORDS WRITTEN   ' RECS-OUT
           DISPLAY ' RECORDS CONVERTED ' RECS-CVT
           GOBACK

            .
       0010-OPEN-FILES.

           OPEN INPUT  FILE-IN
                OUTPUT FILE-OUT

           .
       0010-EXIT.
           EXIT.

       0020-INITIALIZE.

           PERFORM 0200-READ-INPUT     THRU 0200-EXIT

           .
       0020-EXIT.
           EXIT.

       0100-PROCESS-FILE.

           MOVE IN-RECORD              TO X-REC
           MOVE ZEROS                  TO X-SPPDD-CLP

           PERFORM 0300-WRITE-OUTPUT   THRU 0300-EXIT
           PERFORM 0200-READ-INPUT     THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-INPUT.

           READ FILE-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-OUTPUT.

           WRITE OUT-RECORD            FROM X-REC
           ADD 1                       TO RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE FILE-IN FILE-OUT

           .
       0500-EXIT.
           EXIT.

