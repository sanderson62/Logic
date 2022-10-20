       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCCVTCLP.
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  VALT-IN       ASSIGN TO VALTIN.
           SELECT  VALT-OUT      ASSIGN TO VALTOT.
       EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  VALT-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ERCEXTD.

       FD  VALT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  VALT-RECORD                 PIC X(588).
       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   DCCVTCLP WORKING-STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-VALT               VALUE 'Y'.
       77  VALT-RECS-IN                PIC 9(9) VALUE ZEROS.
       77  VALT-RECS-OUT               PIC 9(9) VALUE ZEROS.
       77  VALT-RECS-FIX               PIC 9(9) VALUE ZEROS.
       77  VALT-RECS-SKIP              PIC 9(9) VALUE ZEROS.
       77  VALT-CO-FIX                 PIC 9(9) VALUE ZEROS.

                                       COPY ERCPNDC.
                                       
       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-VALT   THRU 0100-EXIT UNTIL
                 END-OF-VALT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' VALT RECORDS READ    ' VALT-RECS-IN
           DISPLAY ' VALT RECORDS WRITTEN ' VALT-RECS-OUT
           DISPLAY ' VALT RECORDS FIXED   ' VALT-RECS-FIX
           DISPLAY ' VALT RECORDS SKIPPED ' VALT-RECS-SKIP
           DISPLAY ' VALT RECORDS CO      ' VALT-CO-FIX
           GOBACK

           .
       0100-PROCESS-VALT.

           IF ED-PENDING-CLAIMS
              MOVE ED-DATA-AREAS       TO PENDING-CLAIMS
              IF PC-RESERVES
                 IF PC-CC-CLP-STATE NOT = SPACES AND ZEROS
                    AND LOW-VALUES
                    MOVE PC-CC-CLP-STATE
                                       TO PC-STATE
                 END-IF
                 MOVE PENDING-CLAIMS   TO ED-DATA-AREAS
                 PERFORM 0300-WRITE-VALT
                                       THRU 0300-EXIT
              END-IF
           END-IF

           PERFORM 0200-READ-VALT      THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0200-READ-VALT.

           READ VALT-IN AT END
                SET END-OF-VALT        TO TRUE
           END-READ

           IF NOT END-OF-VALT
              ADD 1                    TO VALT-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-VALT.

           WRITE VALT-RECORD           FROM EXTRACT-DATA-RECORD
           ADD 1                       TO VALT-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT VALT-IN
               OUTPUT VALT-OUT

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE VALT-IN VALT-OUT

           .

       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           PERFORM 0200-READ-VALT      THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

