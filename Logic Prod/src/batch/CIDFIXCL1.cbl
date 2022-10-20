       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDFIXCL1.
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELMSTR           ASSIGN TO ELMSTR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CL-CONTROL-PRIMARY
                                   FILE STATUS IS ELMSTR-FILE-STATUS.
       EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ELMSTR.
                                   COPY ELCMSTR.

       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   AHLFIXCL1 WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-FILE               VALUE 'Y'.
       77  MSTR-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  MSTR-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  MSTR-RECS-FIX           PIC 9(9) VALUE ZEROS.

       01  ELMSTR-FILE-STATUS      PIC XX    VALUE ZEROS.


       PROCEDURE DIVISION.

       0000-MAIN.

           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0100-PROCESS-MSTR THRU 0100-EXIT 

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' MSTR RECORDS READ    ' MSTR-RECS-IN
      *     DISPLAY ' MSTR RECORDS WRITTEN ' MSTR-RECS-OUT
           DISPLAY ' MSTR RECORDS FIXED   ' MSTR-RECS-FIX
           GOBACK

           .
       0100-PROCESS-MSTR.

           MOVE LOW-VALUES       TO CL-CONTROL-PRIMARY
           MOVE X'04'            TO CL-COMPANY-CD
           MOVE '9'              TO CL-CARRIER
           MOVE '2365562'        TO CL-CLAIM-NO
           MOVE '0010173088 '    TO CL-CERT-NO

           PERFORM 0200-READ-MSTR THRU 0200-EXIT

           IF END-OF-FILE
               GO TO 0100-EXIT
           END-IF

           IF CL-CLAIM-NO = '2365562'
             AND CL-TOTAL-INT-PAID = 24.64
              DISPLAY 'FOUND CLAIM ' CL-CLAIM-NO 
                  ' WITH INTEREST AMT = ' CL-TOTAL-INT-PAID 
              MOVE 5.75 TO CL-TOTAL-INT-PAID
              PERFORM 0300-WRITE-MSTR THRU 0300-EXIT
           END-IF


           .

       0100-EXIT.
           EXIT.

       0200-READ-MSTR.


           READ ELMSTR 

           IF ELMSTR-FILE-STATUS = '10' OR '23'
              DISPLAY 'ELMSTR READ ' ELMSTR-FILE-STATUS
              SET END-OF-FILE        TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELMSTR READ ' ELMSTR-FILE-STATUS
                 SET END-OF-FILE     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-FILE
              ADD 1 TO MSTR-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-MSTR.

           DISPLAY 'ABOUT TO RE-WRITE CLAIM WITH INTEREST AMT = '
                     CL-TOTAL-INT-PAID
                     
           REWRITE CLAIM-MASTER
           IF ELMSTR-FILE-STATUS NOT = '00'
              DISPLAY 'ELMSTR REWRITE ' ELMSTR-FILE-STATUS
           ELSE
              ADD 1 TO MSTR-RECS-FIX
           END-IF

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN I-O ELMSTR

           IF ELMSTR-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ELMSTR OPEN ERROR  ' ELMSTR-FILE-STATUS
           END-IF

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELMSTR

           .

       0500-EXIT.
           EXIT.



