       IDENTIFICATION DIVISION.
       PROGRAM-ID. EPECFX1.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  EPEC-IN             ASSIGN TO SYS010.
           SELECT  EPEC-OUT            ASSIGN TO SYS011.
       EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  EPEC-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSEPC01.

       FD  EPEC-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  EPEC-RECORD                 PIC X(325).
       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMEPC1  WORKING-STORAGE     '.
       77  FILLER  PIC X(32) VALUE '*********** VMOD=2.001. ********'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-EPEC               VALUE 'Y'.
       77  WS-DROP-SW                  PIC X VALUE SPACES.
           88  DROP-EPEC                 VALUE 'Y'.
       77  EPEC-RECS-IN                PIC 9(9) VALUE ZEROS.
       77  EPEC-RECS-OUT               PIC 9(9) VALUE ZEROS.
       77  EPEC-RECS-DEL               PIC 9(9) VALUE ZEROS.

       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-EPEC   THRU 0100-EXIT UNTIL
                 END-OF-EPEC

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' EPEC RECORDS READ    ' EPEC-RECS-IN
           DISPLAY ' EPEC RECORDS WRITTEN ' EPEC-RECS-OUT
           DISPLAY ' EPEC RECORDS DROPPED ' EPEC-RECS-DEL
           GOBACK
            .
       0100-PROCESS-EPEC.

           PERFORM 0300-WRITE-EPEC     THRU 0300-EXIT

           IF (EP-RUN-DTE = 20030228)
              MOVE 20030331            TO EP-RUN-DTE
              PERFORM 0300-WRITE-EPEC  THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-EPEC      THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0200-READ-EPEC.

           READ EPEC-IN AT END
                SET END-OF-EPEC        TO TRUE
           END-READ

           IF NOT END-OF-EPEC
              ADD 1                    TO EPEC-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-EPEC.

           WRITE EPEC-RECORD           FROM EP-RECORD
           ADD 1                       TO EPEC-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT EPEC-IN
               OUTPUT EPEC-OUT

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE EPEC-IN EPEC-OUT

           .

       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           PERFORM 0200-READ-EPEC      THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

