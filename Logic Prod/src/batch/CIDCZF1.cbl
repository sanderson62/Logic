       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCZF1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERCNOT           ASSIGN TO ERCNOT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CZ-CONTROL-PRIMARY
                                   FILE STATUS IS ERCNOT-FILE-STATUS.

           SELECT DISK-DATE     ASSIGN TO SYS019-FBA1-S-SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ERCNOT.

           COPY ERCCNOT.

082603 FD  DISK-DATE                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDCZF1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  WS-RECS-IN                  PIC 9(9) VALUE ZEROS.
       77  WS-RECS-FIX                 PIC 9(9) VALUE ZEROS.
       77  WS-RECS-DEL                 PIC 9(9) VALUE ZEROS.
       77  WS-RECS-ADD                 PIC 9(9) VALUE ZEROS.
       77  WS-BAD-LINES                PIC 9(9) VALUE ZEROS.
       77  S1                          PIC S9(5) VALUE +0 COMP-3.
       77  WS-CERT-EFF-DT              PIC X(10)  VALUE SPACES.
       77  WS-REWRITE-SW               PIC X  VALUE SPACES.
           88  REWRITE-RECORD             VALUE 'Y'.


       01  WS-MISC.
           05  PGM-SUB                 PIC S9(4)   VALUE +548.
           05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           05  WS-ZERO                 PIC S9      VALUE ZERO.
           05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.

      ******************************************************************
       01  WS-HOLD-ERCNOT              PIC X(150).
       01  FILLER.
           05  WS-WORK-SEQ             PIC X.
           05  WS-NUM-SEQ REDEFINES WS-WORK-SEQ PIC 9.
           05  ERCNOT-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-INPUT  THRU 0100-EXIT UNTIL
                 (END-OF-INPUT)
PEMTST*          OR (WS-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' RECORDS READ    '  WS-RECS-IN
           DISPLAY ' RECORDS FIXED   '  WS-RECS-FIX
           DISPLAY ' RECORDS DELETED '  WS-RECS-DEL
           DISPLAY ' RECORDS ADDED   '  WS-RECS-ADD
           DISPLAY ' BAD LINES       '  WS-BAD-LINES
           GOBACK

           .
       0100-PROCESS-INPUT.

           PERFORM 0105-BUILD-EXTRACT  THRU 0105-EXIT

           PERFORM 0200-READ-ERCNOT    THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0105-BUILD-EXTRACT.

           MOVE ' '                    TO WS-REWRITE-SW

           MOVE CZ-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO WS-CERT-EFF-DT
           ELSE
              MOVE '01/01/1965'        TO WS-CERT-EFF-DT
              DISPLAY ' ERROR - EFF DT CONVERT ' CZ-CARRIER ' '
                 CZ-GROUPING ' ' CZ-STATE ' ' CZ-ACCOUNT ' '
                 CZ-CERT-NO ' DELETING RECORD '
              DELETE ERCNOT
              IF ERCNOT-FILE-STATUS = '00'
                 ADD 1 TO WS-RECS-DEL
              ELSE
                 DISPLAY ' ERROR - ERCNOT - DELETE ' ERCNOT-FILE-STATUS
              END-IF
              GO TO 0105-EXIT
           END-IF

           IF CZ-CERT-SFX = X'00'
              DISPLAY ' FIXING CERT SUFFIX ' CZ-CARRIER ' '
                 CZ-GROUPING ' ' CZ-STATE ' ' CZ-ACCOUNT ' '
                 WS-CERT-EFF-DT ' ' CZ-CERT-NO
              PERFORM 0110-CHECK-FOR-DUP THRU 0110-EXIT
              GO TO 0105-EXIT
           END-IF

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +60
              IF CZ-NOTE (S1:3) = X'434E04' OR X'434E9C'
                 DISPLAY ' REMOVING CONTENTS IN LINE ' CZ-CARRIER ' '
                    CZ-GROUPING ' ' CZ-STATE ' ' CZ-ACCOUNT ' '
                    WS-CERT-EFF-DT ' ' CZ-CERT-NO ' ' CZ-NOTE
                 MOVE SPACES        TO CZ-NOTE
                 MOVE +61           TO S1
                 ADD 1              TO WS-BAD-LINES
                 SET REWRITE-RECORD TO TRUE
              END-IF
           END-PERFORM

           IF REWRITE-RECORD
              PERFORM 0300-REWRITE-RECORD
                                       THRU 0300-EXIT
           END-IF

           .
       0105-EXIT.
           EXIT.

       0110-CHECK-FOR-DUP.

           DISPLAY ' ORIGINAL ' CZ-NOTE

           MOVE CERT-NOTE-FILE         TO WS-HOLD-ERCNOT
           DELETE ERCNOT
           IF ERCNOT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCNOT - DELETE ' ERCNOT-FILE-STATUS
              PERFORM ABEND-PGM
           ELSE
              ADD 1 TO WS-RECS-DEL
           END-IF
           MOVE WS-HOLD-ERCNOT         TO CERT-NOTE-FILE
           MOVE ' '                    TO CZ-CERT-SFX
           WRITE CERT-NOTE-FILE
           IF ERCNOT-FILE-STATUS NOT = '00' AND '22'
              DISPLAY ' ERROR - ERCNOT - WRITE ' ERCNOT-FILE-STATUS
              PERFORM ABEND-PGM
           ELSE
              ADD 1 TO WS-RECS-ADD
           END-IF

           .
       0110-EXIT.
           EXIT.

       0200-READ-ERCNOT.

           READ ERCNOT NEXT RECORD

           IF ERCNOT-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCNOT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERCNOT READ ' ERCNOT-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              ELSE
                 IF CZ-COMPANY-CD > DTE-CLASIC-COMPANY-CD
                    SET END-OF-INPUT  TO TRUE
                 END-IF
              END-IF
           END-IF

           IF NOT END-OF-INPUT
              ADD 1 TO WS-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-REWRITE-RECORD.

PEMTST     REWRITE CERT-NOTE-FILE
PEMTST*    MOVE '00'                   TO ERCNOT-FILE-STATUS

           IF ERCNOT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCNOT - REWRITE  ' ERCNOT-FILE-STATUS
              SET END-OF-INPUT         TO TRUE
           ELSE
              ADD 1                    TO WS-RECS-FIX
           END-IF

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

PEMTST     OPEN I-O ERCNOT
PEMTST*    OPEN INPUT ERCNOT

           IF ERCNOT-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERCNOT - OPEN  ' ERCNOT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERCNOT

           .
       0500-EXIT.
           EXIT.

       0550-START-ERCNOT.

           MOVE DTE-CLASIC-COMPANY-CD  TO CZ-CONTROL-PRIMARY

           START ERCNOT KEY > CZ-CONTROL-PRIMARY

           IF ERCNOT-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCNOT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERCNOT - START  ' ERCNOT-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           PERFORM 0550-START-ERCNOT   THRU 0550-EXIT
           PERFORM 0200-READ-ERCNOT    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM SECTION.              COPY ELCABEND.
