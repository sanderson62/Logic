       IDENTIFICATION DIVISION.
       PROGRAM-ID.                EL513CN.
      *AUTHOR.     PABLO.
      *REMARKS.
      * THIS PROGRAM READS THE EL512 FILE FROM BOW AND FOR EACH   
      * CANCEL RECORD THAT HAS SOMETHING IN THE BOW LOAN NUMBER,
      * BUILD A CERTIFICATE NOTE RECORD AND UPDATE THE ELCERT RECORD
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
111109* 111109    2008100900003  AJRA  UPDATE CERT WITH NEW NOTE CODES
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DISK-DATE        ASSIGN TO SYS019.
           SELECT RPT-FILE         ASSIGN TO SYS008.

           SELECT PNDB-IN          ASSIGN TO SYS010.
           
           SELECT ELCERT           ASSIGN TO ELCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CM-CONTROL-PRIMARY
                                   FILE STATUS IS ELCERT-FILE-STATUS.

           SELECT ERNOTE           ASSIGN TO ERNOTE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CN-CONTROL-PRIMARY
                                   FILE STATUS IS ERNOTE-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE                   COPY ELCDTEFD.

       FD  RPT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RPT-REC-OUT.

       01  RPT-REC-OUT.
           05  RPT-REC                  PIC X(132).

       FD  PNDB-IN.
                                       COPY ERCPNDB.

       FD  ELCERT.
                                       COPY ELCCERT.                         

       FD  ERNOTE.
                                       COPY ERCNOTE.



       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  SAVE-CERT                   PIC X(11) VALUE SPACES.
       77  SAVE-EFF-DT                 PIC X(6)  VALUE SPACES.
       77  SAVE-STATE                  PIC XX    VALUE SPACES.
       77  PGM-SUB                     PIC S999  COMP   VALUE +511.    
       77  WS-ABEND-FILE-STATUS        PIC XX            VALUE ZERO.    
       77  WS-ABEND-MESSAGE            PIC X(80)         VALUE SPACES.  
       77  WS-ABEND-PROGRAM            PIC X(8)          VALUE SPACES.  
       77  WS-RETURN-CODE              PIC S9(4)         VALUE +0.      
       77  WS-ZERO                     PIC S9     COMP-3 VALUE +0.      
       77  IN-CNT                 PIC 9999999   VALUE ZEROS.
       77  CANC-CNT               PIC 9999      VALUE ZEROS.
       77  PNDB-CNT               PIC 9999      VALUE ZEROS.
       77  NOTE-SUB               PIC S999      VALUE +0 COMP-3.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  ELCERT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
           88  ELCERT-FOUND                    VALUE '00'.
       77  ERNOTE-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  AGT-SUB                     PIC S999 COMP-3 VALUE +0.

       01  W-MISC.
           05  BAL-DRP-CNT            PIC 9(7) VALUE ZEROS.
           05  BAL-FIX-CNT            PIC 9(7) VALUE ZEROS.
           05  BAL-CNT                PIC 9(7) VALUE ZEROS.
           05  DUP-CNT                PIC 9(7) VALUE ZEROS.
           05  PNDB-IN-CNT            PIC 9(7) VALUE ZEROS.
           05  ELCERT-IN-CNT          PIC 9(7) VALUE ZEROS.
           05  NOTE-CNT               PIC 9(7) VALUE ZEROS.
           05  NOTE-RCNT              PIC 9(7) VALUE ZEROS.
           05  CERT-RCNT              PIC 9(7) VALUE ZEROS.
           05  WS-INPUT-SW            PIC X VALUE ' '.
               88  END-OF-INPUT             VALUE 'Y'.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0040-OPEN-FILES     THRU 0040-EXIT
           PERFORM 0050-INIT           THRU 0050-EXIT

           PERFORM 0020-PROCESS        THRU 0020-EXIT UNTIL
              (END-OF-INPUT)
      *       OR (PNDB-IN-CNT > 10)
           PERFORM 0060-CLOSE-FILES    THRU 0060-EXIT

           DISPLAY ' PNDB RECS READ ' PNDB-IN-CNT
           DISPLAY ' NOTE REWRITE   ' NOTE-RCNT
           DISPLAY ' NOTE WRITE     ' NOTE-CNT
           DISPLAY ' CERT REWRITE   ' CERT-RCNT

           GOBACK
           .

       0015-READ-ELCERT.

           MOVE PB-COMPANY-CD          TO CM-COMPANY-CD
           MOVE PB-CARRIER             TO CM-CARRIER
           MOVE PB-GROUPING            TO CM-GROUPING
           MOVE PB-STATE               TO CM-STATE
           MOVE PB-ACCOUNT             TO CM-ACCOUNT
           MOVE PB-CERT-NO             TO CM-CERT-NO
           MOVE PB-CERT-EFF-DT         TO CM-CERT-EFF-DT

           READ ELCERT

           IF ELCERT-FILE-STATUS = '00'
              ADD 1                    TO ELCERT-IN-CNT
           ELSE
              IF (ELCERT-FILE-STATUS = '23' OR '10')
                 OR (CM-COMPANY-CD > X'04')
                 DISPLAY ' NO ELCERT    ' PB-ACCOUNT '  ' PB-CERT-NO
              ELSE
                 DISPLAY ' BAD READ ELCERT ' CM-CONTROL-PRIMARY '  '
                    ELCERT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0015-EXIT.
           EXIT.

       0017-READ-PNDB.

           READ PNDB-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO PNDB-IN-CNT
           END-IF

           .
       0017-EXIT.
           EXIT.

       0020-PROCESS.
       
           IF PB-RECORD-TYPE = '2'
              DISPLAY 'FOUND CANCEL RECORD '
                 PB-ACCOUNT ' ' PB-CERT-NO
              PERFORM 0025-PROCESS     THRU 0025-EXIT
           END-IF

           PERFORM 0017-READ-PNDB      THRU 0017-EXIT

           .
       0020-EXIT.
           EXIT.
           
       0025-PROCESS.

           PERFORM 0015-READ-ELCERT    THRU 0015-EXIT
           
           IF ELCERT-FOUND
              DISPLAY '     FOUND ELCERT '
              PERFORM 0070-BUILD-ERNOTE
                                       THRU 0070-EXIT
              PERFORM 0030-REWRITE-ELCERT
                                       THRU 0030-EXIT
           END-IF

           .
       0025-EXIT.
            EXIT.


       0030-REWRITE-ELCERT.
       
111109     IF CM-NOTE-SW = '2' OR '3' OR '6' OR '7'
              CONTINUE
           ELSE
111109        IF CM-NOTE-SW = ' '
111109             MOVE '2'            TO CM-NOTE-SW
111109        ELSE
111109          IF CM-NOTE-SW = '1'
111109              MOVE '3'           TO CM-NOTE-SW
111109          ELSE
111109            IF CM-NOTE-SW = '4'
111109                MOVE '6'         TO CM-NOTE-SW
111109            ELSE
111109                MOVE '7'         TO CM-NOTE-SW
111109            END-IF
111109          END-IF
111109        END-IF
PEMTMP        REWRITE CERTIFICATE-MASTER
              IF ELCERT-FILE-STATUS = '00'
                 DISPLAY '     REWRITE CERT ' CM-ACCOUNT
                    '  ' CM-CERT-NO
      *          DISPLAY '     CERT REWRITE ' CM-CONTROL-PRIMARY
                 ADD 1                 TO CERT-RCNT
              ELSE
                 DISPLAY '     BAD REWRITE ELCERT ' CM-CONTROL-PRIMARY
                    '  ' ELCERT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0030-EXIT.
            EXIT.


              
       0040-OPEN-FILES.
       
           OPEN INPUT PNDB-IN
PEMTMP*         ELCERT ERNOTE
PEMTMP     OPEN I-O ELCERT

           IF ELCERT-FILE-STATUS NOT = '00'
              DISPLAY ' ELCERT OPEN ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

PEMTMP     OPEN I-O ERNOTE

           IF ERNOTE-FILE-STATUS NOT = '00'
              DISPLAY ' ERNOTE OPEN ' ERNOTE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0040-EXIT.
           EXIT.

       0050-INIT.
       
           PERFORM 0017-READ-PNDB      THRU 0017-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-CLOSE-FILES.
       
           CLOSE ELCERT
                 ERNOTE
                 PNDB-IN

           IF ELCERT-FILE-STATUS NOT = '00'
              DISPLAY ' ELCERT CLOSE ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERNOTE-FILE-STATUS NOT = '00'
              DISPLAY ' ERNOTE CLOSE ' ERNOTE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-BUILD-ERNOTE.

           PERFORM 0071-BUILD-ERNOTE-KEY
                                       THRU 0071-EXIT
           READ ERNOTE

           IF ERNOTE-FILE-STATUS = '00'
              DISPLAY '     FOUND ERNOTE - WILL REWRITE '
              PERFORM 0072-BUILD-ERNOTE-BODY
                                       THRU 0072-EXIT
              PERFORM 0073-REWRITE-ERNOTE
                                       THRU 0073-EXIT

           ELSE
              IF ERNOTE-FILE-STATUS = '23' OR '10'
                 DISPLAY '     NO ERNOTE FOUND - WILL ADD '
                 MOVE SPACES           TO CERTIFICATE-NOTE
                 PERFORM 0071-BUILD-ERNOTE-KEY
                                       THRU 0071-EXIT
                 PERFORM 0072-BUILD-ERNOTE-BODY
                                       THRU 0072-EXIT
                 PERFORM 0074-WRITE-ERNOTE
                                       THRU 0074-EXIT
              ELSE
                 DISPLAY ' BAD READ ERNOTE ' CN-CONTROL-PRIMARY '  '
                    DC-GREG-DATE-CYMD '  ' ERNOTE-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0070-EXIT.
            EXIT.

       0071-BUILD-ERNOTE-KEY.
       
           MOVE CM-CONTROL-PRIMARY     TO CN-CONTROL-PRIMARY

           .
       0071-EXIT.
           EXIT.

       0072-BUILD-ERNOTE-BODY.

111109     IF CN-BILLING-START-LINE-NO NOT NUMERIC
111109        MOVE ZEROS               TO CN-BILLING-START-LINE-NO
111109     END-IF
111109     IF CN-BILLING-END-LINE-NO NOT NUMERIC
111109        MOVE ZEROS               TO CN-BILLING-END-LINE-NO
111109     END-IF
111109     PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
111109          (NOTE-SUB > +10)
111109           OR CN-LINE (NOTE-SUB) (1:14) = 'LOAN NUMBER - '
111109     END-PERFORM
111109     IF CN-LINE (NOTE-SUB) (1:14) = 'LOAN NUMBER - '
111109        DISPLAY '    ALREADY HAVE CERT NOTE FOR ' CN-ACCOUNT
111109        '  ' CN-CERT-NO
111109        MOVE +11         TO NOTE-SUB
111109        GO TO 0072-EXIT
111109     END-IF              
111109       
111109     PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
111109        (NOTE-SUB > +10)
111109        OR (CN-LINE (NOTE-SUB) = SPACES)
111109     END-PERFORM
111109
111109     IF NOTE-SUB > +10
111109        DISPLAY '     CERT NOTE FILE FULL FOR ' CN-ACCOUNT
111109        '  ' CN-CERT-NO
111109        DISPLAY '     OR MAYBE IT ALREADY GOT UPDATED '
111109     ELSE
111109        DISPLAY '     FOUND EMPTY ERNOTE SLOT '
111109        IF NOTE-SUB >= CN-BILLING-START-LINE-NO  AND
111109           NOTE-SUB <= CN-BILLING-END-LINE-NO
111109            STRING 'LOAN NUMBER - ' PB-CI-BOW-LOAN-NUMBER
111109               DELIMITED BY SIZE INTO CN-LINE (NOTE-SUB)
111109            END-STRING
111109        ELSE
111109          IF CN-BILLING-END-LINE-NO NOT = ZERO
111109            AND NOTE-SUB = (CN-BILLING-END-LINE-NO + +1)
111109              STRING 'LOAN NUMBER - ' PB-CI-BOW-LOAN-NUMBER
111109                 DELIMITED BY SIZE INTO CN-LINE (NOTE-SUB)
111109              END-STRING
111109              MOVE NOTE-SUB TO CN-BILLING-END-LINE-NO
111109          ELSE
111109            IF CN-BILLING-END-LINE-NO = ZERO
111109               STRING 'LOAN NUMBER - ' PB-CI-BOW-LOAN-NUMBER
111109                  DELIMITED BY SIZE INTO CN-LINE (NOTE-SUB)
111109               END-STRING
111109               MOVE NOTE-SUB TO CN-BILLING-START-LINE-NO
111109                                CN-BILLING-END-LINE-NO
111109            ELSE
111109              IF CN-BILLING-START-LINE-NO NOT = ZERO
111109                AND NOTE-SUB = (CN-BILLING-START-LINE-NO - +1) 
111109                   STRING 'LOAN NUMBER - ' PB-CI-BOW-LOAN-NUMBER
111109                     DELIMITED BY SIZE INTO CN-LINE (NOTE-SUB)
111109                   END-STRING
111109                   MOVE NOTE-SUB TO CN-BILLING-START-LINE-NO
111109              ELSE
111109                  PERFORM 0075-SQUEEZE-IT-IN THRU 0075-EXIT
111109              END-IF
111109            END-IF
111109          END-IF
111109        END-IF
111109     END-IF
           .
       0072-EXIT.
           EXIT.

       0073-REWRITE-ERNOTE.
       
PEMTMP*    MOVE '00'                   TO ERNOTE-FILE-STATUS

092706     IF NOTE-SUB > +10
092706        GO TO 0073-EXIT
092706     END-IF

111109     MOVE 'BOWU'                 TO CN-LAST-MAINT-USER
111109     MOVE BIN-RUN-DATE           TO CN-LAST-MAINT-DT
111109     MOVE 190000                 TO CN-LAST-MAINT-HHMMSS
           DISPLAY '     BILL LINES ' CN-BILLING-START-LINE-NO
              '   ' CN-BILLING-END-LINE-NO

PEMTMP     REWRITE CERTIFICATE-NOTE

           IF ERNOTE-FILE-STATUS = '00'
              DISPLAY '     REWRITE NOTE ' PB-ACCOUNT '  ' PB-CERT-NO
      *       DISPLAY '     REWRITE NOTE ' CN-CONTROL-PRIMARY
              ADD 1                    TO NOTE-RCNT
           ELSE
              DISPLAY '     BAD REWRITE ERNOTE ' CN-CONTROL-PRIMARY
                 '  ' ERNOTE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0073-EXIT.
           EXIT.
                      
       0074-WRITE-ERNOTE.
       
           MOVE 'CN'                   TO CN-RECORD-ID
           MOVE 1                      TO CN-BILLING-START-LINE-NO
                                          CN-BILLING-END-LINE-NO
                                          
PEMTMP*    MOVE '00'                   TO ERNOTE-FILE-STATUS
111109     MOVE 'BOWU'                 TO CN-LAST-MAINT-USER
111109     MOVE BIN-RUN-DATE           TO CN-LAST-MAINT-DT
111109     MOVE 190000                 TO CN-LAST-MAINT-HHMMSS
PEMTMP     WRITE CERTIFICATE-NOTE

           IF ERNOTE-FILE-STATUS = '00'
              DISPLAY '     WRITE NOTE   ' PB-ACCOUNT '  ' PB-CERT-NO
      *       DISPLAY '     WRITE NOTE ' CN-CONTROL-PRIMARY
              ADD 1                    TO NOTE-CNT
           ELSE
              DISPLAY '     BAD WRITE ERNOTE ' CN-CONTROL-PRIMARY
                 '  ' ERNOTE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0074-EXIT.
           EXIT.
111109                
111109 0075-SQUEEZE-IT-IN.
111109
111109     IF (NOTE-SUB > CN-BILLING-END-LINE-NO)
111109        PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY -1 UNTIL
111109           NOTE-SUB = +1
111109           MOVE CN-LINE (NOTE-SUB - 1) TO CN-LINE (NOTE-SUB)
111109           IF (NOTE-SUB - 1) = (CN-BILLING-END-LINE-NO + 1)
111109              STRING 'LOAN NUMBER - ' PB-CI-BOW-LOAN-NUMBER
111109                  DELIMITED BY SIZE INTO CN-LINE (NOTE-SUB - 1)
111109              END-STRING
111109              COMPUTE CN-BILLING-END-LINE-NO = NOTE-SUB - 1 
111109              MOVE +2            TO NOTE-SUB
111109           END-IF
111109        END-PERFORM
111109     ELSE
111109        IF (NOTE-SUB < CN-BILLING-START-LINE-NO)
111109           PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY +1 UNTIL
111109              NOTE-SUB = +10
111109              MOVE CN-LINE (NOTE-SUB + 1) TO CN-LINE (NOTE-SUB)
111109              IF (NOTE-SUB + 1) = (CN-BILLING-START-LINE-NO - 1)
111109                 STRING 'LOAN NUMBER - ' PB-CI-BOW-LOAN-NUMBER
111109                     DELIMITED BY SIZE INTO CN-LINE (NOTE-SUB + 1)
111109                 END-STRING
111109                 COMPUTE CN-BILLING-START-LINE-NO = NOTE-SUB + 1
111109                 MOVE +9 TO NOTE-SUB
111109              END-IF
111109           END-PERFORM
111109        END-IF
111109     END-IF
111109
111109     .
111109 0075-EXIT.
111109     EXIT.
111109
       8500-DATE-CONVERT.

           CALL 'ELDATCX'              USING DATE-CONVERSION-DATA.

       8500-EXIT.
           EXIT.
           EJECT

       ABEND-PGM.   COPY ELCABEND.

