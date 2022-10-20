       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDPLX1.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.
032612******************************************************************
032612*                   C H A N G E   L O G
032612*
032612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
032612*-----------------------------------------------------------------
032612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
032612* EFFECTIVE    NUMBER
032612*-----------------------------------------------------------------
032612* 032612   2011110200001   PEMA  AHL CHANGES
032612******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERPLAN       ASSIGN TO ERPLAN
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERPLAN-FILE-STATUS
                               RECORD KEY IS PL-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.

           SELECT PLAN-OUT     ASSIGN TO PLANOT
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  PLAN-OUT.

       01  PLAN-OUT-REC                PIC X(190).

       FD  DISK-DATE                   COPY ELCDTEFD.

       FD  ERPLAN.

                                       COPY ERCPLAN.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDPLX1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-ERPLAN              VALUE 'Y'.
       77  S1                          PIC S999   VALUE +0 COMP-3.
       77  ERPLAN-FILE-STATUS          PIC XX     VALUE '00'.

       01  FILLER.
           05  WS-SAVE-PLAN-EXT        PIC X(190)   VALUE SPACES.
           05  WS-ERPLAN-IN            PIC 9(7)    VALUE ZEROS.
           05  WS-ERPLAN-OUT           PIC 9(7)    VALUE ZEROS.

       01  PLAN-EXTR-RECORD.
           05  PLAN-CARRIER            PIC X.
           05  PLAN-COL1               PIC X.
           05  PLAN-GROUPING           PIC X(6).
           05  PLAN-COL2               PIC X.
           05  PLAN-STATE              PIC XX.
           05  PLAN-COL3               PIC X.
           05  PLAN-ACCOUNT            PIC X(10).
           05  PLAN-COL4               PIC X.
           05  PLAN-BENE-TYPE          PIC X.
           05  PLAN-COL5               PIC X.
           05  PLAN-BENE-CODE          PIC XX.
           05  PLAN-COL6               PIC X.
           05  PLAN-REV                PIC XXX.
           05  PLAN-COL7               PIC X.
           05  PLAN-ATT-AGE            PIC 99.
           05  PLAN-COL8               PIC X.
           05  PLAN-LIMITS OCCURS 8.
               10  PLAN-AGE            PIC 99.
               10  PLAN-COL9           PIC X.
               10  PLAN-TERM           PIC 999.
               10  PLAN-COL10          PIC X.
               10  PLAN-AH-AMT         PIC 9(4).
               10  PLAN-COL11          PIC X.
               10  PLAN-LF-AMT         PIC 9(6).
               10  PLAN-COL12          PIC X.
           05  PLAN-EOR                PIC X.


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

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

       0000-BEGIN.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT

           PERFORM 0040-PROCESS-FILE   THRU 0040-EXIT UNTIL
              END-OF-ERPLAN

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-ERPLAN-IN
           DISPLAY ' RECORDS OUT   ' WS-ERPLAN-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0010-OPEN-FILES.

           OPEN INPUT  ERPLAN
                OUTPUT PLAN-OUT

           IF ERPLAN-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERPLAN OPEN ERROR ' ERPLAN-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           MOVE SPACES                 TO PLAN-EXTR-RECORD
           MOVE ';'                    TO PLAN-COL1
                                          PLAN-COL2
                                          PLAN-COL3
                                          PLAN-COL4
                                          PLAN-COL5
                                          PLAN-COL6
                                          PLAN-COL7
                                          PLAN-COL8
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +8
              MOVE ';'                 TO PLAN-COL9   (S1)
                                          PLAN-COL10  (S1)
                                          PLAN-COL11  (S1)
                                          PLAN-COL12  (S1)
              MOVE ZEROS               TO PLAN-AGE    (S1)
                                          PLAN-TERM   (S1)
                                          PLAN-AH-AMT (S1)
                                          PLAN-LF-AMT (S1)
           END-PERFORM
           MOVE 'E'                    TO PLAN-EOR
           MOVE PLAN-EXTR-RECORD       TO WS-SAVE-PLAN-EXT

           PERFORM 0120-START-ERPLAN   THRU 0120-EXIT
032612     if not end-of-erplan
              PERFORM 0110-READ-ERPLAN THRU 0110-EXIT
032612     end-if

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ERPLAN PLAN-OUT

           IF ERPLAN-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERPLAN CLOSE ERROR ' ERPLAN-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-PROCESS-FILE.

           PERFORM 0070-BUILD-EXTR     THRU 0070-EXIT

           PERFORM 0110-READ-ERPLAN    THRU 0110-EXIT

           .
       0040-EXIT.
           EXIT.

       0070-BUILD-EXTR.

           MOVE WS-SAVE-PLAN-EXT       TO PLAN-EXTR-RECORD

           MOVE PL-CARRIER             TO PLAN-CARRIER
           MOVE PL-GROUPING            TO PLAN-GROUPING
           MOVE PL-ACCOUNT             TO PLAN-ACCOUNT
           MOVE PL-STATE               TO PLAN-STATE
           IF PL-BENEFIT-TYPE = LOW-VALUES
              MOVE ZEROS               TO PL-BENEFIT-TYPE
           END-IF
           MOVE PL-BENEFIT-TYPE        TO PLAN-BENE-TYPE
           IF PL-BENEFIT-CODE = LOW-VALUES
              MOVE ZEROS               TO PL-BENEFIT-CODE
           END-IF
           MOVE PL-BENEFIT-CODE        TO PLAN-BENE-CODE
           IF PL-REVISION-NO = LOW-VALUES
              MOVE ZEROS               TO PL-REVISION-NO
           END-IF
           MOVE PL-REVISION-NO         TO PLAN-REV
           MOVE PL-ATT-AGE             TO PLAN-ATT-AGE
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +8
              IF PL-LM-AGE (S1) NOT NUMERIC
                 MOVE ZEROS            TO PL-LM-AGE (S1)
              END-IF
              IF PL-LM-DUR (S1) NOT NUMERIC
                 MOVE ZEROS            TO PL-LM-DUR (S1)
              END-IF
              IF PL-LM-MOA (S1) NOT NUMERIC
                 MOVE ZEROS            TO PL-LM-MOA (S1)
              END-IF
              IF PL-LM-AMT (S1) NOT NUMERIC
                 MOVE ZEROS            TO PL-LM-AMT (S1)
              END-IF
              MOVE PL-LM-AGE (S1)      TO PLAN-AGE    (S1)
              MOVE PL-LM-DUR (S1)      TO PLAN-TERM   (S1)
              MOVE PL-LM-MOA (S1)      TO PLAN-AH-AMT (S1)
              MOVE PL-LM-AMT (S1)      TO PLAN-LF-AMT (S1)
           END-PERFORM

           PERFORM 0080-WRITE-PLAN-OUT THRU 0080-EXIT

           .
       0070-EXIT.
           EXIT.

       0080-WRITE-PLAN-OUT.

           WRITE PLAN-OUT-REC          FROM PLAN-EXTR-RECORD
           ADD 1                       TO WS-ERPLAN-OUT

           .
       0080-EXIT.
           EXIT.

       0110-READ-ERPLAN.

           READ ERPLAN NEXT RECORD

           IF (ERPLAN-FILE-STATUS = '10' OR '23')
                           OR
              (PL-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERPLAN   TO TRUE
           ELSE
              IF ERPLAN-FILE-STATUS NOT = '00'
                 DISPLAY ' ERPLAN READ ERROR ' ERPLAN-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 ADD 1                 TO WS-ERPLAN-IN
              END-IF
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ERPLAN.

           MOVE LOW-VALUES             TO PL-CONTROL-PRIMARY

           MOVE DTE-CLASIC-COMPANY-CD  TO PL-COMPANY-CD
           START ERPLAN KEY >= PL-CONTROL-PRIMARY

           IF ERPLAN-FILE-STATUS = '10' OR '23'
              SET END-OF-ERPLAN TO TRUE
           ELSE
              IF ERPLAN-FILE-STATUS NOT = '00'
                 DISPLAY ' ERPLAN START ERROR ' ERPLAN-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0120-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

