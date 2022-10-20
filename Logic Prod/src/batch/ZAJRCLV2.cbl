       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZAJRCLV1.
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
           SELECT  MSTR-OUT      ASSIGN TO MSTROT.
       EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ELMSTR.
                                   COPY ELCMSTR.

       FD  MSTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  MSTR-RECORD             PIC X(100).
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

       01  ELMSTR-FILE-STATUS      PIC XX    VALUE ZEROS.

       01  Mast-out.
           05  mast-carrier        pic x(1).
           05  mast-claim-no       pic x(7).
           05  mast-cert-no        pic x(11).
           05  mast-last-name      pic x(15).
           05  mast-name           pic x(13).
           05  mast-soc-sec-no     pic x(11).
           05  mast-last-maint-id  pic x(4).
           05  mast-processor      pic x(4).
           05  filler              pic x(34).

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

           IF cl-soc-sec-no > ' ' and CL-soc-sec-no (4:1) = ' '
              PERFORM 0300-WRITE-MSTR THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-MSTR THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0200-READ-MSTR.


           READ ELMSTR NEXT RECORD

           IF ELMSTR-FILE-STATUS = '10' OR '23'
              SET END-OF-FILE        TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELMSTR READ NEXT ' ELMSTR-FILE-STATUS
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

           move cl-carrier to mast-carrier
           move cl-claim-no to mast-claim-no
           move cl-cert-no to mast-cert-no
           move cl-insured-last-name to mast-last-name.
           move cl-insured-name to mast-name
           move cl-soc-sec-no to mast-soc-sec-no
           move cl-last-maint-user to mast-last-maint-id
           move cl-processor-id to mast-processor
           WRITE MSTR-RECORD FROM MAST-out
           ADD 1 TO MSTR-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ELMSTR
               OUTPUT MSTR-OUT

           IF ELMSTR-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ELMSTR OPEN ERROR  ' ELMSTR-FILE-STATUS
           END-IF

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELMSTR MSTR-OUT

           .

       0500-EXIT.
           EXIT.

       0550-START-ELMSTR.

           MOVE LOW-VALUES             TO CL-CONTROL-PRIMARY
           MOVE X'04'                  TO CL-COMPANY-CD

           START ELMSTR KEY IS NOT < CL-CONTROL-PRIMARY

           IF ELMSTR-FILE-STATUS = '10' OR '23'
              SET END-OF-FILE        TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELMSTR START     ' ELMSTR-FILE-STATUS
                 SET END-OF-FILE     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           PERFORM 0550-START-ELMSTR   THRU 0550-EXIT
           PERFORM 0200-READ-MSTR THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

