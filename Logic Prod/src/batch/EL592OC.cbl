       IDENTIFICATION DIVISION.
       PROGRAM-ID. EL592OC.
       AUTHOR.     
      ******************************************************************
      *REMARKS.
      *        THIS PROGRAM IS CALLED BY EL592 TO CHECK TO SEE IF
      *        THERE ARE ANY OPEN CLAIMS FOR A GIVEN CERT NUMBER.
      *
      *     INPUT:   ELMSTR5
      ******************************************************************
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELMSTR           ASSIGN TO ELMSTR5
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CL-CONTROL-BY-CERT-NO
                                   FILE STATUS IS ELMSTR-FILE-STATUS.
       EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ELMSTR.
                                   COPY ELCMSTR.

       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '          WORKING-STORAGE       '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-FILE               VALUE 'Y'.

       01  ELMSTR-FILE-STATUS      PIC XX    VALUE ZEROS.

       LINKAGE SECTION.

       01  CHECK-CLAIM-PASS.
           05  CHECK-CLAIM-KEY.
               10  CHECK-CLAIM-COMPANY       PIC X.
               10  CHECK-CLAIM-CERT-NO       PIC X(11).
           05  CHECK-CLAIM-ACCT.
               10  CHECK-CLAIM-CARRIER       PIC X.
               10  CHECK-CLAIM-GROUP         PIC X(6).
               10  CHECK-CLAIM-STATE         PIC X(2).
               10  CHECK-CLAIM-ACCOUNT       PIC X(10).
               10  CHECK-CLAIM-EFF-DT        PIC X(2).
           05  CHECK-CLAIM-OPEN              PIC X.         
       

       PROCEDURE DIVISION USING CHECK-CLAIM-PASS.

       0000-MAIN.
       
           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-MSTR THRU 0100-EXIT UNTIL
                 END-OF-FILE

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           GOBACK

           .
       0100-PROCESS-MSTR.

           READ ELMSTR NEXT RECORD

           IF ELMSTR-FILE-STATUS = '10' OR '23'
              DISPLAY 'ELMSTR READ NEXT ' ELMSTR-FILE-STATUS
              SET END-OF-FILE        TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00' AND '02'
                 DISPLAY 'ELMSTR READ NEXT ' ELMSTR-FILE-STATUS
                 SET END-OF-FILE     TO TRUE
              END-IF
           END-IF

           IF END-OF-FILE
              GO TO 0100-EXIT
           END-IF
           IF CHECK-CLAIM-COMPANY <> CL-COMPANY-CD-A4 OR
              CHECK-CLAIM-CERT-NO <> CL-CERT-NO-A4
                 SET END-OF-FILE     TO TRUE
                 GO TO 0100-EXIT
           END-IF
           
           IF CHECK-CLAIM-CARRIER NOT = CL-CARRIER       OR                  
              CHECK-CLAIM-GROUP   NOT = CL-CERT-GROUPING OR                  
              CHECK-CLAIM-STATE   NOT = CL-CERT-STATE    OR                  
              CHECK-CLAIM-ACCOUNT NOT = CL-CERT-ACCOUNT  OR                  
              CHECK-CLAIM-EFF-DT  NOT = CL-CERT-EFF-DT                       
                 GO TO 0100-EXIT
           END-IF           
           
           IF CLAIM-IS-OPEN
               MOVE 'Y'  TO  CHECK-CLAIM-OPEN
               SET END-OF-FILE  TO TRUE
           END-IF

           .

       0100-EXIT.
           EXIT.


       0400-OPEN-FILES.

           OPEN INPUT ELMSTR

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

       0600-INITIALIZE.

           MOVE SPACES TO WS-EOF-SW
           MOVE LOW-VALUES             TO CL-CONTROL-BY-CERT-NO
           MOVE CHECK-CLAIM-COMPANY    TO CL-COMPANY-CD-A4
           MOVE CHECK-CLAIM-CERT-NO    TO CL-CERT-NO-A4

           START ELMSTR KEY IS NOT < CL-CONTROL-BY-CERT-NO

           IF ELMSTR-FILE-STATUS = '10' OR '23'
              DISPLAY 'ELMSTR START     ' ELMSTR-FILE-STATUS
              SET END-OF-FILE        TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00' AND '02'
                 DISPLAY 'ELMSTR START     ' ELMSTR-FILE-STATUS
                 SET END-OF-FILE     TO TRUE
              END-IF
           END-IF

           .

       0600-EXIT.
           EXIT.

