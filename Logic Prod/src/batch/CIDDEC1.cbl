       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDDEC1.
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  EXTR-IN       ASSIGN TO EXTRIN.
           SELECT  EXTR-OUT      ASSIGN TO EXTROT.
       EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSEXT01.

       FD  EXTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  EXTR-RECORD                 PIC X(510).
       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDDEC1   WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-EXTR                   VALUE 'Y'.
       77  EXTR-RECS-IN                PIC 9(9) VALUE ZEROS.
       77  EXTR-RECS-OUT               PIC 9(9) VALUE ZEROS.

       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-EXTR THRU 0100-EXIT UNTIL
                 END-OF-EXTR

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' EXTR RECORDS READ    ' EXTR-RECS-IN
           DISPLAY ' EXTR RECORDS WRITTEN ' EXTR-RECS-OUT
           GOBACK

           .
       0100-PROCESS-EXTR.

           IF DE-STATE = 'CO' OR 'SC' OR 'AZ' OR 'MN' OR 'TX'
                      OR 'AK' OR 'CA' OR 'UT' OR 'ME' OR 'NH'
                      OR 'OR' OR 'RI' OR 'VT' OR 'DE' OR 'MD'
                      OR 'NM' OR 'PA' OR 'SD' OR 'WI' OR 'OH'
              PERFORM 0300-WRITE-EXTR THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-EXTR THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-EXTR.

           READ EXTR-IN AT END
                SET END-OF-EXTR TO TRUE
           END-READ

           IF NOT END-OF-EXTR
              ADD 1 TO EXTR-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-EXTR.

           WRITE EXTR-RECORD FROM DETAIL-EXTRACT
           ADD 1 TO EXTR-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT EXTR-IN
               OUTPUT EXTR-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE EXTR-IN EXTR-OUT

           .
       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           PERFORM 0200-READ-EXTR THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

