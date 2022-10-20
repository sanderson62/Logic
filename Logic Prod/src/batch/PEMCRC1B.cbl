       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCRC1B.
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  CERT-IN       ASSIGN TO CERTIN.
           SELECT  CERT-OUT      ASSIGN TO CERTOT.

       DATA DIVISION.
       FILE SECTION.

       FD  CERT-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                   COPY ECSCRT01.

       FD  CERT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  CERT-RECORD             PIC X(1056).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMCRC1B  WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-CERT               VALUE 'Y'.
       77  CERT-RECS-IN            PIC 9(11) VALUE ZEROS.
       77  CERT-RECS-OUT           PIC 9(11) VALUE ZEROS.

       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-CERT   THRU 0100-EXIT UNTIL
                 END-OF-CERT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CERT RECORDS READ    ' CERT-RECS-IN
           DISPLAY ' CERT RECORDS WRITTEN ' CERT-RECS-OUT
           GOBACK

           .
       0100-PROCESS-CERT.

           IF (CR-STATE = 'MN')
              AND (CR-ACCOUNT (1:5) = '00010')
              PERFORM 0300-WRITE-CERT  THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-CERT      THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-CERT.

           READ CERT-IN AT END
              SET END-OF-CERT          TO TRUE
           END-READ

           IF NOT END-OF-CERT
              ADD 1                    TO CERT-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-CERT.

           WRITE CERT-RECORD           FROM CERTIFICATE-RECORD

           ADD 1                       TO CERT-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT CERT-IN
               OUTPUT CERT-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE CERT-IN CERT-OUT

           .
       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           PERFORM 0200-READ-CERT      THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

