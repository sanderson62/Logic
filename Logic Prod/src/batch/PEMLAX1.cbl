       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZAJRERLAEX.
       AUTHOR.     PABLO
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERARCH           ASSIGN TO ERARCH
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS LA-CONTROL-PRIMARY
                                   FILE STATUS IS ERARCH-FILE-STATUS.

           SELECT  EXTRACT-OUT      ASSIGN TO EXTROT
                                    organization is line sequential.
       EJECT
       DATA DIVISION.
       FILE SECTION.


       FD  ERARCH.

                                   COPY ERCARCH.

       FD  EXTRACT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  EXTRACT-RECORD.
           05  EXT-REC-CERT-NO       PIC X(11).
           05  EXT-REC-LTR-FORM      PIC X(04).
           05  EXT-REC-creation-DT   PIC X(08).
           05  EXT-REC-archive-no    PIC 9(08).
       EJECT
       
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMATC1   WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ERARCH             VALUE 'Y'.
       77  TRLR-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-FIX           PIC 9(9) VALUE ZEROS.
       
       01  WS-MISC.
           05  START-DT                PIC XX.
           05  END-DT                  PIC XX.
           05  ws-work-seq             pic x.
           05  ws-num-seq redefines ws-work-seq pic 9.
           05  WS-SAVE-ERARCH          PIC X(207) VALUE LOW-VALUES.
           05  ERARCH-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  WS-ZERO               PIC S9(01)    VALUE +0.
           05  WS-RETURN-CODE        PIC S9(03)    VALUE +0.
           05  WS-ABEND-MESSAGE      PIC X(80)     VALUE SPACES.
           05  WS-ABEND-FILE-STATUS  PIC X(02)     VALUE ZERO.

                                       COPY ELCDATE.
       
       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-START-DT               PIC X(08)     VALUE SPACES.
           05  PARM-END-DT                 PIC X(08)     VALUE SPACES.
       

       PROCEDURE DIVISION USING PARM.

       0000-MAIN.
           DISPLAY 'PARM Start Date = ' PARM-Start-dt
           DISPLAY 'PARM End Date = ' PARM-End-dt

           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-TRLR THRU 0100-EXIT UNTIL
                 END-OF-ERARCH

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' ARCH RECORDS READ    ' TRLR-RECS-IN
           DISPLAY ' ARCH RECORDS WRITTEN ' TRLR-RECS-OUT
           DISPLAY ' ARCH RECORDS FIXED   ' TRLR-RECS-FIX
           GOBACK

           .
       0100-PROCESS-TRLR.

           IF LA-creation-date >= start-dt  and
              la-creation-date <= end-dt
              PERFORM 0300-WRITE-TRLR THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-TRLR THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0200-READ-TRLR.

           READ ERARCH NEXT RECORD
           
           if la-company-cd not = X'04'
               set end-of-erarch       to true
           end-if

           IF ERARCH-FILE-STATUS = '10' OR '23'
              SET END-OF-ERARCH        TO TRUE
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 DISPLAY 'ERARCH READ NEXT ' ERARCH-FILE-STATUS
                 SET END-OF-ERARCH     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ERARCH
              ADD 1 TO trlr-RECS-IN
           END-IF

           .

       0200-EXIT.
           EXIT.

       0300-WRITE-TRLR.

           if la-cert-no-a2 equal low-values
               go to 0300-exit
           else
               MOVE LA-CERT-NO-A2       TO EXT-REC-CERT-NO
           end-if
           if la-form-a3 equal low-values
      *         move spaces              to ext-rec-ltr-form
               go to 0300-exit
           else 
               MOVE LA-FORM-A3          TO EXT-REC-LTR-FORM
           end-if
           
           MOVE LA-Creation-DaTe        TO DC-BIN-DATE-1
           MOVE ' '                     TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-CYMD  TO EXT-REC-creation-DT
           ELSE
               MOVE SPACES              TO EXT-REC-creation-DT
           END-IF

           move la-archive-no           to ext-rec-archive-no
           
           WRITE EXTRACT-RECORD 
           ADD 1 TO TRLR-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERARCH
               OUTPUT EXTRACT-OUT

           IF ERARCH-FILE-STATUS = '00' OR '97'
              continue
           ELSE
              DISPLAY 'ERARCH open err  ' ERARCH-FILE-STATUS
           END-IF


           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERARCH EXTRACT-OUT

           .

       0500-EXIT.
           EXIT.
       0550-START-ERARCH.

           MOVE LOW-VALUES             TO LA-CONTROL-PRIMARY
           MOVE X'04'                  TO LA-COMPANY-CD

           START ERARCH KEY IS NOT < LA-CONTROL-PRIMARY

           IF ERARCH-FILE-STATUS = '10' OR '23'
              SET END-OF-ERARCH        TO TRUE
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 DISPLAY 'ERARCH START     ' ERARCH-FILE-STATUS
                 SET END-OF-ERARCH     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE PARM-START-DT           TO DC-GREG-DATE-CYMD
           MOVE 'L'                     TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1       TO START-DT
           ELSE
               DISPLAY 'INVALID DATE '  PARM-START-DT
               PERFORM ABEND-PGM        THRU APS-EXIT
           END-IF


           MOVE PARM-END-DT             TO DC-GREG-DATE-CYMD
           MOVE 'L'                     TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1       TO END-DT
           ELSE
               DISPLAY 'INVALID DATE '  PARM-END-DT
               PERFORM ABEND-PGM        THRU APS-EXIT
           END-IF


           PERFORM 0550-START-ERARCH   THRU 0550-EXIT
           PERFORM 0200-READ-TRLR THRU 0200-EXIT

           .

       0600-EXIT.
           EXIT.



       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.


       ABEND-PGM. COPY ELCABEND.
