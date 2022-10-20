       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZAJRATV2.
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELtrlr           ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS at-CONTROL-PRIMARY
                                   FILE STATUS IS ELtrlr-FILE-STATUS.

           SELECT  TRLR-OUT      ASSIGN TO TRLROT.
       EJECT
       DATA DIVISION.
       FILE SECTION.


       FD  ELtrlr.

                                   COPY ELCTRLR.

       FD  TRLR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  TRLR-RECORD             PIC X(200).
       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMATC1   WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-eltrlr             VALUE 'Y'.
       77  TRLR-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-FIX           PIC 9(9) VALUE ZEROS.
       
       01  WS-MISC.
           05  ws-work-seq             pic x.
           05  ws-num-seq redefines ws-work-seq pic 9.
           05  WS-SAVE-ELtrlr          PIC X(207) VALUE LOW-VALUES.
           05  ELtrlr-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.
       

       PROCEDURE DIVISION.

       0000-MAIN.
       
           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-TRLR THRU 0100-EXIT UNTIL
                 END-OF-eltrlr

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' TRLR RECORDS READ    ' TRLR-RECS-IN
           DISPLAY ' TRLR RECORDS WRITTEN ' TRLR-RECS-OUT
           DISPLAY ' TRLR RECORDS FIXED   ' TRLR-RECS-FIX
           GOBACK

           .
       0100-PROCESS-TRLR.

           if auto-pay-tr and
              AT-auto-cash = 'N'
      *        at-recorded-by = 'NSBA'
       	      display 'Claim # ' at-claim-no '  Cert # ' at-cert-no
       	         '   Auto Pay set up by ' 
       	         at-recorded-by       
       	         
       	      ADD 1 TO TRLR-RECS-OUT
              
      *        PERFORM 0300-WRITE-TRLR THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-TRLR THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0200-READ-TRLR.

           READ eltrlr NEXT RECORD

           IF eltrlr-FILE-STATUS = '10' OR '23'
              SET END-OF-eltrlr        TO TRUE
           ELSE
              IF eltrlr-FILE-STATUS NOT = '00'
                 DISPLAY 'eltrlr READ NEXT ' eltrlr-FILE-STATUS
                 SET END-OF-eltrlr     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-eltrlr
              ADD 1 TO trlr-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-TRLR.

           WRITE TRLR-RECORD FROM ACTIVITY-TRAILERS
           ADD 1 TO TRLR-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT eltrlr
               OUTPUT TRLR-OUT

           IF eltrlr-FILE-STATUS = '00' OR '97'
              continue
           ELSE
              DISPLAY 'eltrlr open err  ' eltrlr-FILE-STATUS
           END-IF


           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE eltrlr TRLR-OUT

           .

       0500-EXIT.
           EXIT.
       0550-START-eltrlr.

           MOVE LOW-VALUES             TO at-CONTROL-PRIMARY
           MOVE X'04'                  TO at-COMPANY-CD

           START eltrlr KEY IS NOT < at-CONTROL-PRIMARY

           IF eltrlr-FILE-STATUS = '10' OR '23'
              SET END-OF-eltrlr        TO TRUE
           ELSE
              IF eltrlr-FILE-STATUS NOT = '00'
                 DISPLAY 'eltrlr START     ' eltrlr-FILE-STATUS
                 SET END-OF-eltrlr     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           PERFORM 0550-START-eltrlr   THRU 0550-EXIT
           PERFORM 0200-READ-TRLR THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.

