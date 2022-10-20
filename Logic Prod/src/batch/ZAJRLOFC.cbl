       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZAJRLOFC.
       AUTHOR.     AJRA
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  MSTR-IN       ASSIGN TO MSTRIN.
           SELECT  MSTR-OUT      ASSIGN TO MSTROT.
       EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  MSTR-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                   COPY ERCLOFC.

       FD  MSTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  MSTR-RECORD.
           05  MSTR-CONTROL-PRIMARY       PIC X(25).
           05  MSTR-LOFC-NAME             PIC X(30).
           
       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMCLC1   WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-FILE               VALUE 'Y'.
       77  MSTR-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  MSTR-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  MSTR-RECS-FIX           PIC 9(9) VALUE ZEROS.

       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-MSTR THRU 0100-EXIT UNTIL
                 END-OF-FILE

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' MSTR RECORDS READ    ' MSTR-RECS-IN
           DISPLAY ' MSTR RECORDS WRITTEN ' MSTR-RECS-OUT
           DISPLAY ' MSTR RECORDS FIXED   ' MSTR-RECS-FIX
           GOBACK

           .
       0100-PROCESS-MSTR.

           PERFORM 0300-WRITE-MSTR THRU 0300-EXIT

           PERFORM 0200-READ-MSTR THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0200-READ-MSTR.

           READ MSTR-IN AT END
                SET END-OF-FILE TO TRUE
           END-READ

           IF NOT END-OF-FILE
              ADD 1 TO MSTR-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-MSTR.

           MOVE LO-CONTROL-PRIMARY TO MSTR-CONTROL-PRIMARY
           MOVE LO-OFFICER-NAME    TO MSTR-LOFC-NAME
           WRITE MSTR-RECORD 
           ADD 1 TO MSTR-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT MSTR-IN
               OUTPUT MSTR-OUT

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE MSTR-IN MSTR-OUT

           .

       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           PERFORM 0200-READ-MSTR THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

