       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCAB1.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CERT-IN          ASSIGN TO CERTIN.
           SELECT ECCERT           ASSIGN TO ECCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CA-CONTROL-PRIMARY
                                   FILE STATUS IS ECCERT-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  CERT-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSCRT01.

       FD  ECCERT.

                                       COPY ECCCERT.

       WORKING-STORAGE SECTION.
       01  WS-STATUS-CODES.
           05  ECCERT-FILE-STATUS      PIC XX  VALUE SPACES.
               88  ECCERT-FOUND              VALUE '00'.
           05  WS-EOF-SW               PIC X   VALUE SPACES.
               88  END-OF-INPUT                VALUE 'Y'.

       01  WS-HOLD-ECCERT              PIC X(110)  VALUE LOW-VALUES.
       01  WS-BIN-EFF-DT               PIC XX   VALUE LOW-VALUES.
       01  WS-WORK-FIELDS.
           05  WS-ABEND-FLD            PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD             PIC 9         VALUE ZEROS.
           05  WS-CERT-RECS-IN         PIC 9(9)      VALUE ZEROS.
           05  WS-ECCERT-RECS-BUILT    PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT          PIC Z,ZZZ,ZZ9 VALUE ZEROS.

                                       COPY ELCDATE.
                                       
       PROCEDURE DIVISION.
       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES     THRU 0100-EXIT
           PERFORM 0200-INITIALIZE     THRU 0200-EXIT
           PERFORM 1000-PROCESS        THRU 1000-EXIT UNTIL
              END-OF-INPUT
           PERFORM 3000-CLOSE-FILES    THRU 3000-EXIT
           PERFORM 4000-FINAL-TOTALS   THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

           OPEN INPUT CERT-IN
           OPEN I-O   ECCERT
      *    OPEN INPUT ECCERT

           IF ECCERT-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ECCERT - OPEN ' ECCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES                 TO WS-EOF-SW
           MOVE ZEROS                  TO WS-CERT-RECS-IN
                                          WS-ECCERT-RECS-BUILT

           PERFORM 1100-READ-INPUT     THRU 1100-EXIT

           .
       0200-EXIT.
           EXIT.

       1000-PROCESS.

           MOVE SPACES                 TO BATCH-CERT-FILE-KEYS
           MOVE 'CA'                   TO CA-RECORD-ID
           MOVE X'04'                  TO CA-COMPANY-CD
           MOVE CR-ACCT-CONTROL        TO CA-CONTROL-PRIMARY (2:19)

           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8000-DATE-CONVERT   THRU 8000-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO CA-CERT-EFF-DT
           ELSE
              DISPLAY ' ERROR CONVERTING EFF DT '
                 CR-ACCT-CONTROL '  ' CR-CERT-NO
               PERFORM ABEND-PGM
           END-IF
           MOVE CR-CERT-NO             TO CA-CERT-NO
           MOVE CR-LNAME               TO CA-INSURED-LAST-NAME
           MOVE CR-FNAME               TO CA-INSURED-FIRST-NAME
           MOVE CR-INIT                TO CA-INSURED-MID-INIT
           MOVE CR-POST-CARD-IND       TO CA-POST-CARD-IND
           
           PERFORM 2100-WRITE-ECCERT   THRU 2100-EXIT

           PERFORM 1100-READ-INPUT     THRU 1100-EXIT

           .
       1000-EXIT.
           EXIT.

       1100-READ-INPUT.

           READ CERT-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ
           
           IF NOT END-OF-INPUT
              ADD +1                   TO WS-CERT-RECS-IN
           END-IF
            
           .
       1100-EXIT.
           EXIT.

       1200-READ-ECCERT.

           READ ECCERT

           IF ECCERT-FILE-STATUS = '00'
              SET ECCERT-FOUND         TO TRUE
           ELSE
              IF ECCERT-FILE-STATUS NOT = '10' AND '23'
                 DISPLAY 'ECCERT, BAD READ '
                      ECCERT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.


       2100-WRITE-ECCERT.

           WRITE BATCH-CERT-FILE-KEYS

           IF ECCERT-FILE-STATUS = '00'
              ADD 1                    TO WS-ECCERT-RECS-BUILT
           ELSE
              DISPLAY ' ERROR - ECCERT - WRITE ' ECCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       2100-EXIT.
           EXIT.

       3000-CLOSE-FILES.

           CLOSE ECCERT CERT-IN

           .
       3000-EXIT.
           EXIT.

       4000-FINAL-TOTALS.

           DISPLAY '**************************************************'
           DISPLAY '***                                            ***'

           MOVE WS-CERT-RECS-IN      TO WS-DISPLAY-CNT
           DISPLAY '***  CERT MASTER RECS IN         = ' WS-DISPLAY-CNT

           MOVE WS-ECCERT-RECS-BUILT   TO WS-DISPLAY-CNT
           DISPLAY '***  CERT MASTER RECS BUILT      = ' WS-DISPLAY-CNT


           DISPLAY '***                                            ***'
           DISPLAY '**************************************************'

           .
       4000-EXIT.
           EXIT.

11271  8000-DATE-CONVERT.
11272                                                                   
11273      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
11274                                                                   
11275  8000-EXIT.
11276      EXIT.                                                        

       ABEND-PGM.
       
            CALL 'ABORTME'

           .
       9999-EXIT.
           EXIT.
