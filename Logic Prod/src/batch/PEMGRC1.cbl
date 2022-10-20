       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMGRC1.
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  GAAP-IN       ASSIGN TO GAAPIN.
           SELECT  GAAP-OUT      ASSIGN TO GAAPOT.
       EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  GAAP-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                   COPY ECSGAP01.

       FD  GAAP-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  GAAP-RECORD-OT          PIC X(365).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMGRC1   WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-INPUT-SW             PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  GAAP-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  GAAP-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  GAAP-RECS-FIX           PIC 9(9) VALUE ZEROS.
       77  WS-PREV-STATE           PIC XX   VALUE SPACES.
       77  WS-LF-CNT               PIC S999 COMP-3 VALUE +0.
       77  WS-AH-CNT               PIC S999 COMP-3 VALUE +0.
       77  WS-WRITE-SW             PIC X VALUE ' '.
           88  WRITE-REC                 VALUE 'Y'.

       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-GAAP   THRU 0100-EXIT UNTIL
              END-OF-INPUT              

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' GAAP RECORDS READ    ' GAAP-RECS-IN
           DISPLAY ' GAAP RECORDS WRITTEN ' GAAP-RECS-OUT
           DISPLAY ' GAAP RECORDS FIXED   ' GAAP-RECS-FIX
           GOBACK

           .
       0100-PROCESS-GAAP.

           IF GR-REIN NOT = 'R'
           IF GR-STATE NOT = WS-PREV-STATE
              MOVE +0                  TO WS-LF-CNT
                                          WS-AH-CNT
              MOVE GR-STATE            TO WS-PREV-STATE
           END-IF
              
           MOVE ' '                    TO WS-WRITE-SW

           IF GR-LFTYP NOT = SPACES AND ZEROS
              IF WS-LF-CNT < +5
                 SET WRITE-REC         TO TRUE
                 ADD +1                TO WS-LF-CNT
              END-IF
           END-IF

           IF GR-AHTYP NOT = SPACES AND ZEROS
              IF WS-AH-CNT < +5
                 SET WRITE-REC         TO TRUE
                 ADD +1                TO WS-AH-CNT
              END-IF
           END-IF

           IF WRITE-REC
              PERFORM 0300-WRITE-GAAP  THRU 0300-EXIT
           END-IF
           
           END-IF

           PERFORM 0200-READ-GAAP      THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-GAAP.

           READ GAAP-IN AT END
                SET END-OF-INPUT       TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO GAAP-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-GAAP.

           WRITE GAAP-RECORD-OT        FROM GAAP-RECORD
           ADD 1                       TO GAAP-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT  GAAP-IN
                OUTPUT GAAP-OUT
               
           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE GAAP-IN GAAP-OUT

           .
       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           PERFORM 0200-READ-GAAP      THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.
           
