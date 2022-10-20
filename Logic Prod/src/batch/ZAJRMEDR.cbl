       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZAJRMEDR.
       AUTHOR.     AJRA
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DISK-DATE          ASSIGN TO SYS019-FBA1-S-SYS019.

           SELECT ELTRLR           ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AT-CONTROL-PRIMARY
                                   FILE STATUS IS ELTRLR-FILE-STATUS.

           SELECT  EXTRACT-OUT   ASSIGN TO EXTRACT.
       EJECT
       DATA DIVISION.
       FILE SECTION.
       

       FD  DISK-DATE
                                COPY ELCDTEFD.

       FD  ELTRLR.

                                   COPY ELCTRLR.

       FD  EXTRACT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  EXTRACT-RECORD.
           05  EXT-REC-CLAIM-NO      PIC X(07).
           05  EXT-REC-LTR-FORM      PIC X(04).
           05  EXT-REC-RECORDED-DT   PIC X(08).
           05  EXT-REC-RECEIVE-DT    PIC X(08).
           05  EXT-REC-SERVICE-TIME  PIC 9(05).99.
       
       
       
       
       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '        WORKING-STORAGE         '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                 PIC X VALUE SPACES.
           88  END-OF-FILE                 VALUE 'Y'.
       77  TRLR-RECS-IN              PIC 9(9) VALUE ZEROS.
       77  TRLR-RECS-OUT             PIC 9(9) VALUE ZEROS.
       
       01  WORK-AREA.
           05  START-DT              PIC XX.
           05  END-DT                PIC XX.
           05  PGM-SUB               PIC S9(04)    VALUE +586.
           05  WS-ZERO               PIC S9(01)    VALUE +0.
           05  WS-RETURN-CODE        PIC S9(03)    VALUE +0.
           05  WS-ABEND-MESSAGE      PIC X(80)     VALUE SPACES.
           05  WS-ABEND-FILE-STATUS  PIC X(02)     VALUE ZERO.
           05  ELTRLR-FILE-STATUS    PIC XX        VALUE ZEROS.
           

      *              *************
      *              ELCDTECX: LAYOUT FOR DISK-DATE FILE
                     COPY ELCDTECX.


                     COPY ELCDTEVR.

      *              *************
      *              ELCDATE: LAYOUT OF DATA PASSED TO DATE CONV RTN
                     COPY ELCDATE.
       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-START-DT               PIC X(08)     VALUE SPACES.
           05  PARM-END-DT                 PIC X(08)     VALUE SPACES.

      ******************************************************************
      ********************************
       PROCEDURE DIVISION USING PARM.


      ****************READ DISK-DATE FILE
       0000-DATE-CARD-READ. COPY ELCDTERX.

       0000-MAIN.

           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0600-INITIALIZE THRU 0600-EXIT

           PERFORM 0100-PROCESS-TRLR THRU 0100-EXIT UNTIL
                 END-OF-FILE

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' TRLR RECORDS READ    ' TRLR-RECS-IN
           DISPLAY ' TRLR RECORDS WRITTEN ' TRLR-RECS-OUT
           GOBACK

           .
       0100-PROCESS-TRLR.

           IF CORRESPONDENCE-TR AND
             AT-RECORDED-DT >= START-DT AND
              AT-RECORDED-DT <= END-DT
                PERFORM 0300-WRITE-TRLR THRU 0300-EXIT
           END-IF

           PERFORM 0200-READ-TRLR THRU 0200-EXIT
           .

       0100-EXIT.
           EXIT.

       0200-READ-TRLR.

           READ ELTRLR NEXT RECORD
           

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              SET END-OF-FILE        TO TRUE
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELTRLR READ NEXT ' ELTRLR-FILE-STATUS
                 SET END-OF-FILE     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-FILE
              ADD 1 TO TRLR-RECS-IN
           END-IF
           
           .

       0200-EXIT.
           EXIT.

       0300-WRITE-TRLR.

           IF AT-STD-LETTER-FORM = 'CI74' OR 'CI22' OR 'HOHI'
               CONTINUE
           ELSE
               GO TO 0300-EXIT
           END-IF 
           
           MOVE AT-CLAIM-NO             TO EXT-REC-CLAIM-NO    
           MOVE AT-STD-LETTER-FORM      TO EXT-REC-LTR-FORM
           
           MOVE AT-RECORDED-DT          TO DC-BIN-DATE-1
           MOVE ' '                     TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-CYMD  TO EXT-REC-RECORDED-DT
           ELSE
               MOVE SPACES              TO EXT-REC-RECORDED-DT
           END-IF

           MOVE AT-LETTER-ANSWERED-DT   TO DC-BIN-DATE-1
           MOVE ' '                     TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-CYMD  TO EXT-REC-RECEIVE-DT
           ELSE
               MOVE SPACES              TO EXT-REC-RECEIVE-DT
           END-IF

           IF AT-RECORDED-DT <= AT-LETTER-ANSWERED-DT
               MOVE AT-RECORDED-DT          TO DC-BIN-DATE-1
               MOVE AT-LETTER-ANSWERED-DT   TO DC-BIN-DATE-2
               MOVE '1'                     TO DC-OPTION-CODE
               PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT

               IF NO-CONVERSION-ERROR
                   MOVE DC-ELAPSED-DAYS     TO EXT-REC-SERVICE-TIME
               ELSE
                   DISPLAY 'CONVERSION ERROR IN SERVICE TIME CALC '
                                                DC-ERROR-CODE
                   PERFORM ABEND-PGM            THRU APS-EXIT
               END-IF
           ELSE
               MOVE 0 TO EXT-REC-SERVICE-TIME
           END-IF
           
           WRITE EXTRACT-RECORD 
           ADD 1 TO TRLR-RECS-OUT

           .

       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ELTRLR
               OUTPUT EXTRACT-OUT

           IF ELTRLR-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ELTRLR Open Err  ' ELTRLR-FILE-STATUS
           END-IF

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELTRLR EXTRACT-OUT

           .

       0500-EXIT.
           EXIT.

       0550-START-ELTRLR.

           MOVE LOW-VALUES             TO AT-CONTROL-PRIMARY
           MOVE X'04'                  TO AT-COMPANY-CD

           START ELTRLR KEY IS NOT < AT-CONTROL-PRIMARY

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              SET END-OF-FILE        TO TRUE
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELTRLR START     ' ELTRLR-FILE-STATUS
                 SET END-OF-FILE     TO TRUE
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

           PERFORM 0550-START-ELTRLR   THRU 0550-EXIT
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
