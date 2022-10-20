       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCLC1.
       AUTHOR.     PABLO
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

                                   COPY ELCMSTR.

       FD  MSTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  MSTR-RECORD             PIC X(350).
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
       
       
       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-CLAIM-NUMBER           PIC X(07)     VALUE SPACES.


       PROCEDURE DIVISION using parm.

       0000-MAIN.
       
           DISPLAY 'PARM = ' PARM-CLAIM-NUMBER

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

           IF CL-CLAIM-NO = PARM-CLAIM-NUMBER
              PERFORM 0300-WRITE-MSTR THRU 0300-EXIT
           END-IF

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

           WRITE MSTR-RECORD FROM CLAIM-MASTER
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

