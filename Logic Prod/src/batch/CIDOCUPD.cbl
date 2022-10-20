       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDOCUPD.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELCRTO           ASSIGN TO ELCRTO
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS OC-CONTROL-PRIMARY
                                   FILE STATUS IS ELCRTO-FILE-STATUS.

           SELECT ERPNDB           ASSIGN TO ERPNDB2
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS PB-CONTROL-BY-ACCOUNT
                                   FILE STATUS IS ERPNDB-FILE-STATUS.

           SELECT REPORT-FILE      ASSIGN TO SYS012
                                   ORGANIZATION IS LINE SEQUENTIAL. 

           SELECT DISK-DATE        ASSIGN TO SYS019.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ELCRTO.
                                   COPY ELCCRTO.


       FD  ERPNDB.
                                   COPY ERCPNDB.

       FD  REPORT-FILE
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  REPORT-RECORD                  PIC X(133).

       FD  DISK-DATE
           COPY ELCDTEFD.

           EJECT

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDOCUPD WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-ABEND-FILE-STATUS        PIC XX     VALUE ZERO.    
       77  WS-ABEND-MESSAGE            PIC X(80)  VALUE SPACES.  
       77  WS-ABEND-PROGRAM            PIC X(8)   VALUE SPACES.  
       77  WS-RETURN-CODE              PIC S9(4)  VALUE +0.      
       77  WS-ZERO                     PIC S9     COMP-3 VALUE +0.      
       77  WS-EOF-SW                   PIC X      VALUE SPACES.
           88  END-OF-ELCRTO                      VALUE 'Y'.
       77  ELCRTO-RECS-IN              PIC 9(9)   VALUE ZEROS.
       77  ELCRTO-RECS-PREV-DT         PIC 9(9)   VALUE ZEROS.
       77  ELCRTO-RECS-WITH-FLAG       PIC 9(9)   VALUE ZEROS.
       77  ELCRTO-RECS-UPDATED         PIC 9(9)   VALUE ZEROS.
       77  ELCRTO-RECS-PEND-FLAG       PIC 9(9)   VALUE ZEROS.
       77  ELCRTO-RECS-PEND-NO-FLAG    PIC 9(9)   VALUE ZEROS.
       77  SUB1                        PIC S9(5)  VALUE +0 COMP-3.
       77  PGM-SUB                     COMP-3 PIC S9(04) VALUE +585.
       77  WS-CURRENT-BIN-DT           PIC X(02)  VALUE LOW-VALUES.
       77  WS-LINE-CNT                 PIC S9(03) VALUE +0 COMP-3.
       77  WS-PAGE-CNT                 PIC S9(03) VALUE +0 COMP-3.
       77  WS-ISS-UPDATED              PIC X.
       77  WS-CAN-UPDATED              PIC X.
       77  WS-PEND-ISS-FOUND           PIC X.
       77  WS-PEND-CAN-FOUND           PIC X.

       01  BLANK-LINE.
           05  FILLER                  PIC X(133) VALUE SPACES.
           
       01  TOTAL-LINE.
           05  FILLER                  PIC X(20) VALUE SPACES.
           05  TOT-DESC                PIC X(40).
           05  TOT-TOTAL               PIC ZZZZZZ9.
           05  FILLER                  PIC X(66) VALUE SPACES.

       01  REPORT-HEAD1.
           05  FILLER                  PIC X(8)  VALUE 'CIDOCUPD'.       
           05  FILLER                  PIC X(30) VALUE SPACES.
           05  FILLER                  PIC X(23)
               VALUE 'CENTRAL STATES OF OMAHA'.
           05  FILLER                  PIC X(23) VALUE SPACES.
           05  RPT-DATE                PIC X(08) VALUE SPACES.
           05  FILLER                  PIC X(41) VALUE SPACES.

       01  REPORT-HEAD2.
           05  FILLER                  PIC X(31) VALUE SPACES.
           05  FILLER                  PIC X(36)
               VALUE 'ORIGINAL RECORD PENDING FLAG UPDATED'.
           05  FILLER                  PIC X(20) VALUE SPACES.
           05  FILLER                  PIC X(05) VALUE 'PAGE '.
           05  RPT-PAGE                PIC ZZ9   VALUE ZEROS.
           05  FILLER                  PIC X(38) VALUE SPACES.
           
       01  REPORT-HEAD3.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(7) 
               VALUE 'COMPANY'.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(11)
               VALUE 'CERT NUMBER'.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(5)
               VALUE 'STATE'.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(7)
               VALUE 'ACCOUNT'.
           05  FILLER                  PIC X(10) VALUE SPACES.
           05  FILLER                  PIC X(7)
               VALUE 'MESSAGE'.
           05  FILLER                  PIC X(59) VALUE SPACES.

       01  REPORT-REC.
           05  FILLER                  PIC X(4) VALUE SPACES.
           05  RPT-COMPANY             PIC X(3).
           05  FILLER                  PIC X(7) VALUE SPACES.
           05  RPT-CERTNO              PIC X(11).
           05  FILLER                  PIC X(6) VALUE SPACES.
           05  RPT-STATE               PIC X(2).
           05  FILLER                  PIC X(6) VALUE SPACES.
           05  RPT-ACCOUNT             PIC X(10).
           05  FILLER                  PIC X(6) VALUE SPACES.
           05  RPT-MESSAGE             PIC X(60).
           05  FILLER                  PIC X(3)  VALUE SPACES.
           05  RPT-2-PENDING           PIC X(3).
           05  FILLER                  PIC X(12) VALUE SPACES.


      ******************************************************************
       01  WS-MISC.
           05  WS-PREV-COMPANY         PIC X     VALUE X'04'.
           05  ELCRTO-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ERPNDB-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
                                       COPY ELCDATE.

       PROCEDURE DIVISION.

       0000-DATE-CARD-READ. COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-ELCRTO THRU 0050-EXIT UNTIL
                 (END-OF-ELCRTO)


           DISPLAY ' CRTO RECORDS READ             '  
                                     ELCRTO-RECS-IN
           DISPLAY ' CRTO RECS WITH PROCESSED DATE ' 
                                     ELCRTO-RECS-PREV-DT
           DISPLAY ' CRTO RECS NOT PENDING W/FLAG  '
                                     ELCRTO-RECS-WITH-FLAG
           DISPLAY ' CRTO RECS NOT PENDING UPDATED '
                                     ELCRTO-RECS-UPDATED
           DISPLAY ' CRTO RECS PENDING WITH FLAG   '
                                     ELCRTO-RECS-PEND-FLAG
           DISPLAY ' CRTO RECS PENDING UPDATED     '
                                     ELCRTO-RECS-PEND-NO-FLAG
           
           IF WS-LINE-CNT > +50
               PERFORM 0150-WRITE-HEADINGS THRU 0150-EXIT
           END-IF
           
           WRITE REPORT-RECORD FROM BLANK-LINE
           WRITE REPORT-RECORD FROM BLANK-LINE
           MOVE 'ORIGINAL RECORDS READ                   ' TO TOT-DESC
           MOVE ELCRTO-RECS-IN           TO TOT-TOTAL
           WRITE REPORT-RECORD FROM TOTAL-LINE
           MOVE 'ORIGINAL RECORDS WITH PROCESSED DATE    ' TO TOT-DESC
           MOVE ELCRTO-RECS-PREV-DT      TO TOT-TOTAL
           WRITE REPORT-RECORD FROM TOTAL-LINE
           MOVE 'ORIGINAL RECORDS NOT PENDING WITH FLAG  ' TO TOT-DESC
           MOVE ELCRTO-RECS-WITH-FLAG    TO TOT-TOTAL
           WRITE REPORT-RECORD FROM TOTAL-LINE
           MOVE 'ORIGINAL RECORDS NOT PENDING - UPDATED  ' TO TOT-DESC
           MOVE ELCRTO-RECS-UPDATED      TO TOT-TOTAL
           WRITE REPORT-RECORD FROM TOTAL-LINE
           MOVE 'ORIGINAL RECORDS PENDING WITH FLAG      ' TO TOT-DESC
           MOVE ELCRTO-RECS-PEND-FLAG    TO TOT-TOTAL
           WRITE REPORT-RECORD FROM TOTAL-LINE
           MOVE 'ORIGINAL RECORDS PENDING - UPDATED      ' TO TOT-DESC
           MOVE ELCRTO-RECS-PEND-NO-FLAG TO TOT-TOTAL
           WRITE REPORT-RECORD FROM TOTAL-LINE

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           GOBACK

           .
       0050-PROCESS-ELCRTO.

           IF OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES OR SPACES
                 PERFORM 0100-PROCESS-ELCRTO THRU 0100-EXIT
           ELSE
               ADD +1 TO ELCRTO-RECS-PREV-DT
           END-IF

           PERFORM 0200-READ-ELCRTO    THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-ELCRTO.

           MOVE LOW-VALUES             TO PENDING-BUSINESS
           MOVE OC-CONTROL-PRIMARY (1:33) TO 
                              PB-CONTROL-BY-ACCOUNT (1:33)

           START ERPNDB KEY IS NOT < PB-CONTROL-BY-ACCOUNT

           IF ERPNDB-FILE-STATUS NOT = '00'
               PERFORM 0300-UPDATE-ORIG-RECORD THRU 0300-EXIT
               GO TO 0100-EXIT
           END-IF

           READ ERPNDB NEXT RECORD

           IF ERPNDB-FILE-STATUS NOT = '00'
               PERFORM 0300-UPDATE-ORIG-RECORD THRU 0300-EXIT
               GO TO 0100-EXIT
           END-IF


           IF OC-CONTROL-PRIMARY (1:33) <>
                              PB-CONTROL-BY-ACCOUNT (1:33)
               PERFORM 0300-UPDATE-ORIG-RECORD THRU 0300-EXIT
               GO TO 0100-EXIT
           END-IF


            MOVE 'N'   TO WS-ISS-UPDATED
                          WS-CAN-UPDATED
                          WS-PEND-ISS-FOUND
                          WS-PEND-CAN-FOUND
                          
           IF PB-ISSUE
               MOVE 'Y' TO WS-PEND-ISS-FOUND
           ELSE
              IF PB-CANCELLATION
                 MOVE 'Y' TO WS-PEND-CAN-FOUND
              END-IF
           END-IF
           
           MOVE SPACES TO RPT-2-PENDING
           PERFORM 0120-CHECK-FOR-MORE THRU 0120-EXIT
                      
           IF OC-ISSUE-TRAN-IND = 'Y' AND
              WS-PEND-ISS-FOUND = 'N'
                 MOVE 'N' TO OC-ISSUE-TRAN-IND
                 MOVE 'Y' TO WS-ISS-UPDATED
           END-IF
           
           IF OC-CANCEL-TRAN-IND = 'Y' AND
              WS-PEND-CAN-FOUND = 'N'
                 MOVE 'N' TO OC-CANCEL-TRAN-IND
                 MOVE 'Y' TO WS-CAN-UPDATED
           END-IF
           
           IF (WS-ISS-UPDATED = 'N')  AND
              (WS-CAN-UPDATED = 'N')
                ADD 1 TO ELCRTO-RECS-PEND-FLAG
                GO TO 0100-EXIT
           ELSE
                ADD 1 TO ELCRTO-RECS-PEND-NO-FLAG
           END-IF


           REWRITE ORIGINAL-CERTIFICATE
           IF ELCRTO-FILE-STATUS NOT = '00' 
              MOVE ' ELCRTO REWRITE ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELCRTO-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.
           
           
           IF OC-COMPANY-CD NOT = WS-PREV-COMPANY
               MOVE OC-COMPANY-CD TO WS-PREV-COMPANY
               WRITE REPORT-RECORD FROM BLANK-LINE
               ADD +1 TO WS-LINE-CNT
           END-IF

           IF OC-COMPANY-CD = X'04'
               MOVE 'CID'   TO   RPT-COMPANY
           ELSE
             IF OC-COMPANY-CD = X'05'
                MOVE 'DCC'  TO   RPT-COMPANY
             ELSE
                MOVE 'AHL'  TO   RPT-COMPANY
             END-IF
           END-IF
           MOVE OC-CERT-NO  TO   RPT-CERTNO
           MOVE OC-STATE    TO   RPT-STATE
           MOVE OC-ACCOUNT  TO   RPT-ACCOUNT
           STRING 'ORIGINAL RECORD UPDATED - ISSUE FLAG = '
                   OC-ISSUE-TRAN-IND
                   '  CANCEL FLAG = '
                   OC-CANCEL-TRAN-IND
           		     INTO  RPT-MESSAGE
           ADD +1 TO WS-LINE-CNT

           IF WS-LINE-CNT > +58
               PERFORM 0150-WRITE-HEADINGS THRU 0150-EXIT
           END-IF
           
           WRITE REPORT-RECORD FROM REPORT-REC
           
           .
       0100-EXIT.
           EXIT.

       0120-CHECK-FOR-MORE.
           
           READ ERPNDB NEXT RECORD

           IF ERPNDB-FILE-STATUS NOT = '00'
               GO TO 0120-EXIT
           END-IF

           IF OC-CONTROL-PRIMARY (1:33) <>
                              PB-CONTROL-BY-ACCOUNT (1:33)
               GO TO 0120-EXIT
           END-IF

            MOVE '***'  TO RPT-2-PENDING
            
            IF PB-ISSUE
                MOVE 'Y' TO WS-PEND-ISS-FOUND
            ELSE
               IF PB-CANCELLATION
                  MOVE 'Y' TO WS-PEND-CAN-FOUND
               END-IF
            END-IF

           
           .
       0120-EXIT.
           EXIT.

       0150-WRITE-HEADINGS.

           ADD +1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT TO RPT-PAGE
           WRITE REPORT-RECORD FROM REPORT-HEAD1
           WRITE REPORT-RECORD FROM REPORT-HEAD2
           WRITE REPORT-RECORD FROM BLANK-LINE
           WRITE REPORT-RECORD FROM REPORT-HEAD3
           MOVE +4 TO WS-LINE-CNT

           .
       0150-EXIT.
           EXIT.

       0200-READ-ELCRTO.

           READ ELCRTO NEXT RECORD

           IF ELCRTO-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCRTO        TO TRUE
           ELSE
              IF ELCRTO-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCRTO READ NEXT ' ELCRTO-FILE-STATUS
                 SET END-OF-ELCRTO     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ELCRTO
              ADD 1 TO ELCRTO-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-UPDATE-ORIG-RECORD.

           IF (OC-ISSUE-TRAN-IND = 'N')  AND
              (OC-CANCEL-TRAN-IND = 'N')
                  ADD 1 TO ELCRTO-RECS-WITH-FLAG
                  GO TO 0300-EXIT
           END-IF

           MOVE 'N'      TO  OC-ISSUE-TRAN-IND
                             OC-CANCEL-TRAN-IND
           
           REWRITE ORIGINAL-CERTIFICATE
           IF ELCRTO-FILE-STATUS NOT = '00' 
              MOVE ' ELCRTO REWRITE ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELCRTO-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.
           
           ADD 1 TO ELCRTO-RECS-UPDATED.

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN I-O ELCRTO
           OPEN INPUT ERPNDB
               OUTPUT REPORT-FILE.

           IF ELCRTO-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCRTO OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELCRTO-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ERPNDB-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERPNDB OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERPNDB-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELCRTO ERPNDB REPORT-FILE

           .
       0500-EXIT.
           EXIT.

       0550-START-ELCRTO.

           MOVE LOW-VALUES             TO OC-CONTROL-PRIMARY
           MOVE X'04'                  TO OC-COMPANY-CD

           START ELCRTO KEY >= OC-CONTROL-PRIMARY

           IF ELCRTO-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCRTO        TO TRUE
           ELSE
              IF ELCRTO-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCRTO START     ' ELCRTO-FILE-STATUS
                 SET END-OF-ELCRTO     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE WS-CURRENT-DATE        TO  DC-GREG-DATE-1-EDIT.
           MOVE '2'                    TO  DC-OPTION-CODE.
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.
           DISPLAY 'CURRENT DATE USED FOR RUN IS - - ' WS-CURRENT-DATE.
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.

           PERFORM 8510-DATE-CONVERSION.
           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1      TO  WS-CURRENT-BIN-DT
           END-IF.
           
           MOVE WS-CURRENT-DATE TO RPT-DATE.
           MOVE +0 TO WS-PAGE-CNT.
           PERFORM 0150-WRITE-HEADINGS THRU 0150-EXIT

           PERFORM 0550-START-ELCRTO   THRU 0550-EXIT
           PERFORM 0200-READ-ELCRTO    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.


       ABEND-PGM.   COPY ELCABEND.
