       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCCAGX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ERAGTC             ASSIGN TO ERAGTC
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS AG-CONTROL-PRIMARY
                                     FILE STATUS IS ERAGTC-FILE-STATUS.

           SELECT AGTC-OUT          ASSIGN TO AGTCOUT
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE         ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ERAGTC.

                                       COPY ERCAGTC.

       FD  AGTC-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  AGTC-OUT-REC                PIC X(385).

       FD  DISK-DATE                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  S1                          PIC S999 COMP-3 VALUE +0.

       01  WS-STATUS-CODES.
           05  PGM-SUB                 PIC S9(4)   VALUE +548.
           05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           05  ERAGTC-FILE-STATUS      PIC XX  VALUE SPACES.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-ERAGTC               VALUE 'Y'.

       01  AGTC-DETAIL-RECORD.
           12  AGTC-CARRIER            PIC X.
           12  AGTC-TAB1               PIC X.
           12  AGTC-GROUPING           PIC X(6).
           12  AGTC-TAB2               PIC X.
           12  AGTC-BANK               PIC X(10).
           12  AGTC-TAB3               PIC X.
           12  AGTC-EXP-DATE           PIC X(10).
           12  AGTC-TAB4               PIC X.
           12  AGTC-TYPE               PIC X.
           12  AGTC-TAB5               PIC X.
           12  AGTC-COMM-STRUCTURE.
               16  AGTC-AGT-COMMS     OCCURS 10 TIMES.
                   20  AGTC-AGT       PIC X(10).
                   20  AGTC-TAB6      PIC X.
                   20  AGTC-COM-TYP   PIC X.
                   20  AGTC-TAB7      PIC X.
                   20  AGTC-SPP-FEE   PIC ZZZZ9.99.
                   20  AGTC-TAB8      PIC X.
                   20  AGTC-SPP-RCALC PIC X.
                   20  AGTC-TAB9      PIC X.
                   20  AGTC-LEASE-FEE PIC ZZZZ9.99.
                   20  AGTC-TAB10     PIC X.
                   20  AGTC-LEASE-RCALC
                                      PIC X.
                   20  AGTC-TAB11     PIC X.
           12  AGTC-EOR                PIC X.
       01  WS-HOLD-ERAGTC              PIC X(385) VALUE SPACES.
       01  WS-WORK-FIELDS.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-ERAGTC-RECS-IN       PIC 9(9)      VALUE ZEROS.
           05  WS-ERAGTC-RECS-OUT      PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

       0000-LOAD-DATE-CARD.            COPY ELCDTERX.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 1000-PROCESS        THRU 1000-EXIT UNTIL
              END-OF-ERAGTC
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

           OPEN INPUT  ERAGTC
                OUTPUT AGTC-OUT

           IF ERAGTC-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERAGTC - OPEN    ' ERAGTC-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-ERAGTC-RECS-IN
                                          WS-ERAGTC-RECS-OUT
           MOVE SPACES                 TO AGTC-DETAIL-RECORD
           MOVE ';'                    TO AGTC-TAB1
                                          AGTC-TAB2
                                          AGTC-TAB3
                                          AGTC-TAB4
                                          AGTC-TAB5

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +10
              MOVE ';'                 TO AGTC-TAB6 (S1)
                                          AGTC-TAB7 (S1)
                                          AGTC-TAB8 (S1)
                                          AGTC-TAB9 (S1)
                                          AGTC-TAB10 (S1)
                                          AGTC-TAB11 (S1)
              MOVE ZEROS               TO AGTC-SPP-FEE (S1)
                                          AGTC-LEASE-FEE (S1)
           END-PERFORM
           MOVE AGTC-DETAIL-RECORD     TO WS-HOLD-ERAGTC

           PERFORM 1100-START-ERAGTC   THRU 1100-EXIT
           PERFORM 1200-READ-ERAGTC    THRU 1200-EXIT

           .
       0200-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE AG-CARRIER             TO AGTC-CARRIER
           MOVE AG-GROUPING            TO AGTC-GROUPING
           MOVE AG-BANK                TO AGTC-BANK
           MOVE AG-TYPE                TO AGTC-TYPE
           IF AG-EXP-DT = HIGH-VALUES
              MOVE '12/31/9999'        TO AGTC-EXP-DATE
           ELSE
              MOVE AG-EXP-DT           TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO AGTC-EXP-DATE
              END-IF
           END-IF

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +10)
              INSPECT AG-AGT (S1)
                 REPLACING ALL LOW-VALUES BY ZEROS
              IF AG-AGT (S1) NOT = SPACES AND ZEROS
                 MOVE AG-AGT (S1)      TO AGTC-AGT (S1)
                 MOVE AG-COM-TYP (S1)  TO AGTC-COM-TYP (S1)
                 MOVE AG-SPP-FEES (S1) TO AGTC-SPP-FEE (S1)
                 MOVE AG-RECALC-LV-INDIC (S1)
                                       TO AGTC-SPP-RCALC (S1)
                 MOVE AG-SPP-LFEES (S1)
                                       TO AGTC-LEASE-FEE (S1)
                 MOVE AG-LRCALC-LV-INDIC (S1)
                                       TO AGTC-LEASE-RCALC (S1)
              END-IF
           END-PERFORM

           PERFORM 1050-WRITE-AGTC     THRU 1050-EXIT
           PERFORM 1200-READ-ERAGTC    THRU 1200-EXIT

           .
       1000-EXIT.
           EXIT.

       1050-WRITE-AGTC.

           WRITE AGTC-OUT-REC          FROM AGTC-DETAIL-RECORD
           ADD 1                       TO WS-ERAGTC-RECS-OUT

           .
       1050-EXIT.
           EXIT.

       1100-START-ERAGTC.

           MOVE LOW-VALUES             TO AG-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AG-COMPANY-CD

           START ERAGTC KEY >= AG-CONTROL-PRIMARY

           IF ERAGTC-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERAGTC - START    ' ERAGTC-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       1100-EXIT.
           EXIT.

       1200-READ-ERAGTC.

           READ ERAGTC NEXT RECORD

           IF (ERAGTC-FILE-STATUS = '10' OR '23')
              OR (AG-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERAGTC TO TRUE
           ELSE
              IF ERAGTC-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERAGTC - READ   ' ERAGTC-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT END-OF-ERAGTC
              ADD 1                    TO WS-ERAGTC-RECS-IN
           END-IF

           .
       1200-EXIT.
           EXIT.


       3000-CLOSE-FILES.

           CLOSE ERAGTC AGTC-OUT

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-ERAGTC-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  AGTC COMM   RECS IN         = ' WS-DISPLAY-CNT

           MOVE WS-ERAGTC-RECS-OUT     TO WS-DISPLAY-CNT
           DISPLAY '***  AGTC COMM   RECS WRITTEN    = ' WS-DISPLAY-CNT

           DISPLAY '***                                            ***'
           DISPLAY '**************************************************'

           .
       4000-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.

           DISPLAY '*** AGENT COMM   EXTRACT    PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                      TO WS-ABEND-FLD
           MOVE 0                      TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD

           .
       9999-EXIT.
           EXIT.
