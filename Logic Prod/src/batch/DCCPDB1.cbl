       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DCCPDB1.
       AUTHOR.        PABLO.
       DATE-COMPILED.
      *REMARKS.
      
      *   THIS PROGRAM READS AN EXTRACT FROM AN XLS FILE
      *   AND BUILDS THE ERPDEF FILE

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-IN      ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERPDEF    ASSIGN ERPDEF
                            ORGANIZATION IS INDEXED
                            ACCESS IS DYNAMIC
                            RECORD KEY IS EX-KEY-OUT
                            FILE STATUS IS ERPDEF-FILE-STATUS.

           SELECT DISK-DATE    ASSIGN TO SYS019.
                                                                        

       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-IN
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  RECORD-IN.
           05  RI-RETELM               PIC XXX.
           05  RI-FORM                 PIC X(15).
           05  RI-STATE                PIC XX.
           05  F                       PIC X(20).
           05  RI-EXP-DT               PIC X(10).
           05  RI-TERM-MIN             PIC 999.
           05  RI-TERM-MAX             PIC 999.
           05  RI-AMT-MIN              PIC 99.
           05  RI-AMT-MAX              PIC 9(9).
           05  RI-FACTORS OCCURS 8     PIC 9V99.

       FD  ERPDEF
           LABEL RECORDS ARE STANDARD.

       01  EXTR-OUT-REC.
           12 F                        PIC XX.
           12 EX-KEY-OUT               PIC X(18).
           12 F                        PIC X(1299).

       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     DCCPDB1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  ERPDEF-FILE-STATUS          PIC XX   VALUE '00'.
       77  WS-CURRENT-BIN-DATE         PIC XX   VALUE LOW-VALUES.
       77  WS-NEXT-CYCLE-BIN-DATE      PIC XX   VALUE LOW-VALUES.
       77  WS-SELECT-BIN-DATE          PIC XX   VALUE LOW-VALUES.
       77  WS-HYPHEN-CNTR              PIC S999 COMP-3 VALUE +0.
       77  I1                          PIC S999 COMP-3 VALUE +0.
       77  I2                          PIC S999 COMP-3 VALUE +0.
       77  P1                          PIC S999 COMP-3 VALUE +0.
       77  O1                          PIC S999 COMP-3 VALUE +0.

       01  WS-PREV-KEY                 PIC X(20) VALUE SPACES.
       01  WS-CURRENT-KEY.
           05  WS-CK-RETELM            PIC XXX.
           05  WS-CK-FORM              PIC X(15).
           05  WS-CK-STATE             PIC XX.
           
       01  WS-MISC.
           05  WS-CHECK-AMT            PIC X(09).
           05  WS-CHECK-AMT-N REDEFINES WS-CHECK-AMT
                                       PIC 9(07)V99.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  WS-EXTR-IN              PIC 9(7)   VALUE ZEROS.
           05  WS-EXTR-OUT             PIC 9(7)   VALUE ZEROS.
           05  WS-ELEOBC-OUT           PIC 9(7)   VALUE ZEROS.

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

                                       COPY ERCPDEF.
                                       COPY ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

       0000-BEGIN.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-INPUT)
PEMTST*         OR (WS-EXTR-IN > 1000)

           DISPLAY ' END OF FILE REACHED '
           PERFORM 0080-BUILD-EXT-OT   THRU 0080-EXIT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-EXTR-IN
           DISPLAY ' RECORDS  OUT  ' WS-EXTR-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT  EXTR-IN
                I-O    ERPDEF

           IF ERPDEF-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ERPDEF - OPEN ' ERPDEF-FILE-STATUS
              GO TO ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE EXTR-IN ERPDEF

           IF ERPDEF-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERPDEF - CLOSE '
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-INIT.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           DISPLAY ' FUNCTION DATE CYMD ' WS-FN-DATE
           MOVE WS-FN-DATE             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           PERFORM 0110-READ-INPUT     THRU 0110-EXIT
           MOVE RI-RETELM              TO WS-CK-RETELM
           MOVE RI-FORM                TO WS-CK-FORM
           MOVE RI-STATE               TO WS-CK-STATE
           MOVE WS-CURRENT-KEY         TO WS-PREV-KEY
           MOVE +1                     TO I1 I2
           PERFORM 0060-INIT-NEW-REC   THRU 0060-EXIT

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           MOVE RI-RETELM              TO WS-CK-RETELM
           MOVE RI-FORM                TO WS-CK-FORM
           MOVE RI-STATE               TO WS-CK-STATE

           IF WS-CURRENT-KEY NOT = WS-PREV-KEY
              DISPLAY ' CHANGE IN KEY ' 
              PERFORM 0080-BUILD-EXT-OT THRU 0080-EXIT
              PERFORM 0060-INIT-NEW-REC THRU 0060-EXIT
              MOVE +1                  TO I1 I2
              MOVE WS-CURRENT-KEY      TO WS-PREV-KEY
           END-IF

           MOVE RI-TERM-MIN            TO PD-LOW-TERM (I1)
           MOVE RI-TERM-MAX            TO PD-HI-TERM  (I1)
           MOVE RI-AMT-MIN             TO PD-LOW-AMT  (I1)
           MOVE RI-AMT-MAX             TO PD-HI-AMT   (I1)
           PERFORM VARYING I2 FROM +1 BY +1 UNTIL I2 > +8
              MOVE RI-FACTORS (I2)     TO PD-UEP-FACTOR (I1 I2)
           END-PERFORM

           ADD +1                      TO I1
           PERFORM 0110-READ-INPUT     THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-INIT-NEW-REC.

           MOVE 'PD'                   TO PRODUCT-MASTER
           PERFORM VARYING P1 FROM +1 BY +1 UNTIL P1 > +15
              MOVE ZEROS               TO PD-LOW-TERM (P1)
                                          PD-HI-TERM  (P1)
                                          PD-LOW-AMT  (P1)
                                          PD-HI-AMT   (P1)
              PERFORM VARYING O1 FROM +1 BY +1 UNTIL O1 > +15
                 MOVE ZEROS            TO PD-UEP-FACTOR (P1 O1)
              END-PERFORM
              IF P1 < +9
                 MOVE ZEROS            TO PD-MAX-ATT-AGE (P1)
                                          PD-MIN-ISSUE-AGE (P1)
                                          PD-MAX-ISSUE-AGE (P1)
                                          PD-MAX-TERM (P1)
                                          PD-MAX-AMT (P1)
                                          PD-PRE-EXIST-EXCL-TYPE (P1)
                                          PD-EXCLUSION-PERIOD-DAYS (P1)
                                          PD-COVERAGE-ENDS-MOS (P1)
                                          PD-ACCIDENT-ONLY-MOS (P1)
                                          PD-CRIT-PERIOD (P1)
                                          PD-RTW-MOS (P1)
              END-IF
           END-PERFORM

           MOVE DTE-CLASIC-COMPANY-CD  TO PD-COMPANY-CD
           MOVE RI-STATE               TO PD-STATE
           MOVE 'DDF'                  TO PD-PRODUCT-CD
           MOVE 'A'                    TO PD-BEN-TYPE
           EVALUATE TRUE
              WHEN RI-FORM = 'DPP 066'
                 MOVE '59'             TO PD-BEN-CODE
              WHEN RI-FORM = 'DPP 067'
                 MOVE '57'             TO PD-BEN-CODE
              WHEN RI-FORM = 'DPP 068-RV'
                 MOVE '61'             TO PD-BEN-CODE
              WHEN (RI-FORM = 'DPP 069')
                 AND (RI-RETELM = '30E')
                 MOVE '63'             TO PD-BEN-CODE
              WHEN (RI-FORM = 'DPP 069')
                 AND (RI-RETELM = '30R')
                 MOVE '65'             TO PD-BEN-CODE
              WHEN (RI-FORM = 'DPP 071-RV')
                 AND (RI-RETELM = '30E')
                 MOVE '71'             TO PD-BEN-CODE
              WHEN (RI-FORM = 'DPP 071-RV')
                 AND (RI-RETELM = '30R')
                 MOVE '72'             TO PD-BEN-CODE
              WHEN OTHER
                 DISPLAY ' FOUND STRANGE FORM ' RI-FORM ' '
                    RI-RETELM
           END-EVALUATE
           MOVE HIGH-VALUES            TO PD-PROD-EXP-DT

           PERFORM VARYING P1 FROM +1 BY +1 UNTIL P1 > +3
              EVALUATE TRUE
                 WHEN P1 = +1
                    MOVE 'L'           TO PD-PROD-CODE (P1)
                    MOVE 50000         TO PD-MAX-AMT (P1)
                    MOVE ZEROS         TO PD-EXCLUSION-PERIOD-DAYS (P1)
                                          PD-CRIT-PERIOD (P1)
                    MOVE 999           TO PD-COVERAGE-ENDS-MOS (P1)
                                          PD-ACCIDENT-ONLY-MOS (P1)
                    IF PD-BEN-CODE = '57' OR '61'
                       MOVE 24         TO PD-ACCIDENT-ONLY-MOS (P1)
                    END-IF
                 WHEN P1 = +2
                    MOVE 'A'           TO PD-PROD-CODE (P1)
                    MOVE 500           TO PD-MAX-AMT (P1)
                    MOVE ZEROS         TO PD-EXCLUSION-PERIOD-DAYS (P1)
                    MOVE 6             TO PD-CRIT-PERIOD (P1)
                    MOVE 999           TO PD-COVERAGE-ENDS-MOS (P1)
                    MOVE 24            TO PD-ACCIDENT-ONLY-MOS (P1)
                    IF PD-BEN-CODE = '59' OR '57'
                       MOVE 1000       TO PD-MAX-AMT (P1)
                    END-IF
                    IF PD-BEN-CODE = '63' OR '65' OR '71' OR '72'
                       MOVE 999        TO PD-ACCIDENT-ONLY-MOS (P1)
                    END-IF
                 WHEN (P1 = +3)
                    AND (PD-BEN-CODE = '59' OR '60')
                    MOVE 'I'           TO PD-PROD-CODE (P1)
                    MOVE 500           TO PD-MAX-AMT (P1)
                    MOVE 3             TO PD-EXCLUSION-PERIOD-DAYS (P1)
                    MOVE 3             TO PD-CRIT-PERIOD (P1)
                    MOVE 012           TO PD-COVERAGE-ENDS-MOS (P1)
                    MOVE ZEROS         TO PD-ACCIDENT-ONLY-MOS (P1)
              END-EVALUATE
              IF PD-PROD-CODE (P1) NOT = ' '
                 MOVE ZEROS            TO PD-MIN-ISSUE-AGE (P1)
                                          PD-PRE-EXIST-EXCL-TYPE (P1)
                                          PD-RTW-MOS (P1)
                 MOVE 999              TO PD-MAX-ATT-AGE (P1)
                                          PD-MAX-ISSUE-AGE (P1)
                                          PD-MAX-TERM (P1)
                 MOVE 'N'              TO PD-REC-CRIT-PERIOD (P1)
              END-IF
           END-PERFORM

           MOVE 'LOAD'                 TO PD-LAST-MAINT-BY
           MOVE WS-CURRENT-BIN-DATE    TO PD-LAST-MAINT-DT
           MOVE 220000                 TO PD-LAST-MAINT-HHMMSS
           STRING RI-FORM ' ' RI-RETELM DELIMITED BY SIZE
              INTO PD-PRODUCT-DESC
           END-STRING

           IF RI-FORM = 'DPP 066' OR 'DPP 067' OR 'DPP 068-RV'
              MOVE +30.00              TO PD-1ST-YR-ADMIN-ALLOW
           ELSE
              MOVE +15.00              TO PD-1ST-YR-ADMIN-ALLOW
           END-IF

           .
       0060-EXIT.
           EXIT.

       0080-BUILD-EXT-OT.

           PERFORM 0090-WRITE-EXTR     THRU 0090-EXIT

           EVALUATE TRUE
              WHEN PD-BEN-CODE = '59'
                 MOVE '60'             TO PD-BEN-CODE
                 PERFORM 0090-WRITE-EXTR THRU 0090-EXIT
              WHEN PD-BEN-CODE = '61'
                 MOVE '68'             TO PD-BEN-CODE
                 PERFORM 0090-WRITE-EXTR THRU 0090-EXIT
              WHEN PD-BEN-CODE = '57'
                 MOVE '58'             TO PD-BEN-CODE
                 PERFORM 0090-WRITE-EXTR THRU 0090-EXIT
              WHEN PD-BEN-CODE = '63'
                 MOVE '64'             TO PD-BEN-CODE
                 PERFORM 0090-WRITE-EXTR THRU 0090-EXIT
              WHEN PD-BEN-CODE = '65'
                 MOVE '66'             TO PD-BEN-CODE
                 PERFORM 0090-WRITE-EXTR THRU 0090-EXIT
              WHEN PD-BEN-CODE = '71'
                 MOVE '73'             TO PD-BEN-CODE
                 PERFORM 0090-WRITE-EXTR THRU 0090-EXIT
              WHEN PD-BEN-CODE = '72'
                 MOVE '74'             TO PD-BEN-CODE
                 PERFORM 0090-WRITE-EXTR THRU 0090-EXIT
           END-EVALUATE

           .
       0080-EXIT.
           EXIT.

       0090-WRITE-EXTR.

           DISPLAY ' ABOUT TO WRITE ' PD-CONTROL-PRIMARY (2:15)

           WRITE EXTR-OUT-REC    FROM PRODUCT-MASTER
           IF ERPDEF-FILE-STATUS = '22'
              DISPLAY ' FOUND DUPLICATE '
                 PD-CONTROL-PRIMARY (2:15)
           ELSE
              IF ERPDEF-FILE-STATUS NOT = ZEROS
                 DISPLAY ' ERROR - ERPDEF - WRITE '
                    ERPDEF-FILE-STATUS ' '
                     PD-CONTROL-PRIMARY (2:15)
              ELSE
                 ADD +1                TO WS-EXTR-OUT
              END-IF
           END-IF

           .
       0090-EXIT.
           EXIT.

       0110-READ-INPUT.

           READ EXTR-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-EXTR-IN
           END-IF

           .
       0110-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

