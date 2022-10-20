       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZAJRATC1.
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  TRLR-IN       ASSIGN TO TRLRIN.
           SELECT  TRLR-OUT      ASSIGN TO TRLROT.
       EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  TRLR-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                   COPY ELCTRLR.

       FD  TRLR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  TRLR-RECORD             PIC X(200).
       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMATC1   WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-FILE               VALUE 'Y'.
       77  TRLR-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-FIX           PIC 9(9) VALUE ZEROS.

       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-CLAIM-NUMBER           PIC X(07)     VALUE SPACES.
       

       PROCEDURE DIVISION USING PARM.
       
       0000-MAIN.
       
           DISPLAY 'PARM = ' PARM-CLAIM-NUMBER
           
           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-TRLR THRU 0100-EXIT UNTIL
                 END-OF-FILE

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' TRLR RECORDS READ    ' TRLR-RECS-IN
           DISPLAY ' TRLR RECORDS WRITTEN ' TRLR-RECS-OUT
           DISPLAY ' TRLR RECORDS FIXED   ' TRLR-RECS-FIX
           GOBACK

           .
       0100-PROCESS-TRLR.

           IF AT-CLAIM-NO = PARM-CLAIM-NUMBER
              PERFORM 0300-WRITE-TRLR THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-TRLR THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0200-READ-TRLR.

           READ TRLR-IN AT END
                SET END-OF-FILE TO TRUE
           END-READ

           IF NOT END-OF-FILE
              ADD 1 TO TRLR-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-TRLR.

           WRITE TRLR-RECORD FROM ACTIVITY-TRAILERS
           ADD 1 TO TRLR-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT TRLR-IN
               OUTPUT TRLR-OUT

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE TRLR-IN TRLR-OUT

           .

       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           PERFORM 0200-READ-TRLR THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

