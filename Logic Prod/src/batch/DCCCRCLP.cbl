       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCCCRCLP.
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  CERT-IN       ASSIGN TO CERTIN.
           SELECT  CERT-OUT      ASSIGN TO CERTOT.
           SELECT  ACCTXRF-OUT   ASSIGN TO ACCTOT.
       EJECT
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

       FD  ACCTXRF-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
      *  LRECL = 26
       01  ACCT-RECORD.
           05  ACCT-KEY.
               10  ACCT-PRIME-KEY  PIC X(19).
               10  ACCT-CLP-STATE  PIC XX.
           05  ACCT-CNTR           PIC 9(5).

       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   DCCCRCLP  WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  THERE-ARE-NO-MORE-RECORDS VALUE 'Y'.
           88  THERE-ARE-MORE-RECORDS    VALUE ' '.
       77  CERT-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  CERT-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  ACCT-RECS-OUT           PIC 9(9) VALUE ZEROS.

       01  WS-ACCT-SAVE-KEY.
           05  WS-ACCT-PRIM-KEY        PIC X(19) VALUE LOW-VALUES.
           05  WS-ACCT-CLP-STATE       PIC XX    VALUE LOW-VALUES.

       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-CERT THRU 0100-EXIT UNTIL
                 THERE-ARE-NO-MORE-RECORDS

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' CERT RECORDS READ    ' CERT-RECS-IN
           DISPLAY ' CERT RECORDS WRITTEN ' CERT-RECS-OUT
           DISPLAY ' ACCT RECORDS WRITTEN ' ACCT-RECS-OUT
           GOBACK

           .
       0100-PROCESS-CERT.

           IF CR-CLP-STATE = SPACES OR ZEROS OR LOW-VALUES
              MOVE CR-STATE            TO CR-CLP-STATE
           END-IF

           IF CR-CLP-STATE NOT = CR-STATE
              PERFORM 0150-BUILD-XRF   THRU 0150-EXIT
              MOVE CR-CLP-STATE        TO CR-STATE
           END-IF
           
           PERFORM 0300-WRITE-CERT     THRU 0300-EXIT
           PERFORM 0200-READ-CERT      THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0150-BUILD-XRF.
       
           MOVE CR-ACCT-CONTROL        TO ACCT-PRIME-KEY
           MOVE CR-CLP-STATE           TO ACCT-CLP-STATE
           MOVE 1                      TO ACCT-CNTR

      *     THIS ATTEMPTS TO AVOID MOST DUPLICATES,
      *     IT WON'T CATCH THEM ALL
      
           IF ACCT-KEY NOT = WS-ACCT-SAVE-KEY
              MOVE ACCT-KEY            TO WS-ACCT-SAVE-KEY
              WRITE ACCT-RECORD
              ADD 1                    TO ACCT-RECS-OUT
           END-IF
           
           .
       0150-EXIT.
           EXIT.
           
       0200-READ-CERT.

           READ CERT-IN AT END
                SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           END-READ

           IF THERE-ARE-MORE-RECORDS
              ADD 1 TO CERT-RECS-IN
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
               OUTPUT CERT-OUT ACCTXRF-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE CERT-IN CERT-OUT ACCTXRF-OUT

           .
       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           PERFORM 0200-READ-CERT THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

