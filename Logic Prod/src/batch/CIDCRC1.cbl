       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCRC1.
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  CERT-IN       ASSIGN TO CERTIN.
           SELECT  CERT-OUT      ASSIGN TO CERTOT.
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
       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDCRC1   WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  THERE-ARE-NO-MORE-RECORDS VALUE 'Y'.
           88  THERE-ARE-MORE-RECORDS    VALUE ' '.
       77  CERT-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  CERT-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  CERT-RECS-FIX           PIC 9(9) VALUE ZEROS.
       77  WS-LEVEL-MOB-SW         PIC X    VALUE SPACES.
           88  LEVEL-MOB                    VALUE 'Y'.
       77  WS-FIRST-ENTRY-DT       PIC 9(11)   VALUE 99999999999.

       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-CERT THRU 0100-EXIT UNTIL
                 THERE-ARE-NO-MORE-RECORDS

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' CERT RECORDS READ    ' CERT-RECS-IN
           DISPLAY ' CERT RECORDS WRITTEN ' CERT-RECS-OUT
           DISPLAY ' CERT RECORDS FIXED   ' CERT-RECS-FIX
           DISPLAY ' FIRST S RECORD ENT   ' WS-FIRST-ENTRY-DT
           GOBACK

           .
       0100-PROCESS-CERT.

      *    DISPLAY ' CERT ' CR-CERT-NO '  ' CR-LFTYP '  ' CR-AHTYP
      *       '  ' CR-ENTRY-STATUS '  ' CR-LF-CURRENT-STATUS
      *       '  ' CR-AH-CURRENT-STATUS

           IF CR-POLICY-IS-MONTHLY
              IF (CR-LFTYP (1:1) = 'S')
                 OR (CR-AHTYP (1:1) = 'S')
                 IF CR-ENTRY-DATE < WS-FIRST-ENTRY-DT
                    MOVE CR-ENTRY-DATE TO WS-FIRST-ENTRY-DT
                 END-IF
              END-IF
           END-IF
           
           IF CR-POLICY-IS-MONTHLY
              MOVE '1' TO CR-ENTRY-STATUS
              IF CR-LF-POLICY-IS-MONTHLY
                 MOVE '1' TO CR-LF-CURRENT-STATUS
              END-IF
              IF CR-AH-POLICY-IS-MONTHLY
                 MOVE '1' TO CR-AH-CURRENT-STATUS
              END-IF
              PERFORM 0250-FIX-PREMIUM THRU 0250-EXIT
              PERFORM 0300-WRITE-CERT  THRU 0300-EXIT
           END-IF
              
           PERFORM 0200-READ-CERT THRU 0200-EXIT

           .
       0100-EXIT.
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

       0250-FIX-PREMIUM.
       
           MOVE SPACES                 TO WS-LEVEL-MOB-SW
           IF CR-MOB-NET-TOT-FEES NOT NUMERIC
              MOVE ZEROS               TO CR-MOB-NET-TOT-FEES
           END-IF
           IF CR-LFTYP (1:1) = 'M'
              SET LEVEL-MOB            TO TRUE
              IF CR-LFPRM NOT = ZEROS
                 COMPUTE CR-LFPRM = CR-LFPRM * CR-LF-TERM
              END-IF
           END-IF

           IF CR-AHTYP (1:1) = 'M'
              SET LEVEL-MOB            TO TRUE
              IF CR-AHPRM NOT = ZEROS
                 COMPUTE CR-AHPRM = CR-AHPRM * CR-AH-TERM
              END-IF
           END-IF

           IF (NOT LEVEL-MOB)
              AND (CR-MOB-NET-TOT-FEES > ZEROS)
              IF CR-AHTYP (1:1) = 'N' OR 'S'
                 IF CR-LFTYP (1:1) = 'N' OR 'S'

      *  WE HAVE LIFE AND AH HERE SO PLUG AH WITH REMAINDER

                    COMPUTE CR-AHPRM = CR-MOB-NET-TOT-FEES
                    - CR-LFPRM
                 ELSE

      *  WE HAVE AH ONLY SO PLUG ALL IN AH

                    MOVE CR-MOB-NET-TOT-FEES
                                       TO CR-AHPRM
                 END-IF
              ELSE

      *  WE HAVE LF ONLY SO PLUG ALL IN LF

                 MOVE CR-MOB-NET-TOT-FEES TO CR-LFPRM
              END-IF
           END-IF

           .
       0250-EXIT.
           EXIT.
           
       0300-WRITE-CERT.

           WRITE CERT-RECORD FROM CERTIFICATE-RECORD
           ADD 1 TO CERT-RECS-OUT

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

           PERFORM 0200-READ-CERT THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

