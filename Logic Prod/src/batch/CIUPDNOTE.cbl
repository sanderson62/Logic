       IDENTIFICATION DIVISION.
       PROGRAM-ID.                CIUPDNOTE.
      *AUTHOR.     AJRA.
      *REMARKS.
      * THIS PROGRAM REMOVES THE BILLING NOTE LINE NUMBERS FROM ERNOTE 
      * FOR CERTS WITH NO PENDING TRANSACTIONS. 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT ERNOTE           ASSIGN TO ERNOTE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CN-CONTROL-PRIMARY
                                   FILE STATUS IS ERNOTE-FILE-STATUS.

           SELECT ERPNDB           ASSIGN TO ERPNDB2
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS PB-CONTROL-BY-ACCOUNT
                                   FILE STATUS IS ERPNDB-FILE-STATUS.

           SELECT  EXT-OUT         ASSIGN TO SYS011
                                   ORGANIZATION IS LINE SEQUENTIAL. 

       DATA DIVISION.

       FILE SECTION.

       FD  ERNOTE.
                                       COPY ERCNOTE.                         

       FD  ERPNDB.

                                       COPY ERCPNDB.

       FD  EXT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  EXT-RECORD                 PIC X(826).

         WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMEPC2  WORKING-STORAGE     '.
       77  FILLER  PIC X(32) VALUE '*********** VMOD=2.001. ********'.

       77  WS-ABEND-FILE-STATUS        PIC XX            VALUE ZERO.    
       77  WS-ABEND-MESSAGE            PIC X(80)         VALUE SPACES.  
       77  WS-ABEND-PROGRAM            PIC X(8)          VALUE SPACES.  
       77  WS-RETURN-CODE              PIC S9(4)         VALUE +0.      
       77  WS-ZERO                     PIC S9     COMP-3 VALUE +0.      
       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-NOTE               VALUE 'Y'.
       77  WS-DROP-SW                  PIC X VALUE SPACES.
           88  DROP-NOTE                 VALUE 'Y'.
       77  NOTE-RECS-IN                PIC 9(9) VALUE ZEROS.
       77  NOTE-RECS-OUT               PIC 9(9) VALUE ZEROS.
       77  NOTE-RECS-FIX               PIC 9(9) VALUE ZEROS.
       77  ERNOTE-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERPNDB-FILE-STATUS          PIC XX  VALUE ZEROS.
      
       01  misc.
           05  wrk-run-dte             pic 9(11) comp-3.
           
       01  EXT-REC.
           12  EX-CARRIER            PIC X.
           12  FILLER                PIC X    VALUE '|'.
00030      12  EX-GROUPING           PIC X(6).                                 
           12  FILLER                PIC X    VALUE '|'.
00033      12  EX-STATE              PIC XX.                
           12  FILLER                PIC X    VALUE '|'.
00034      12  EX-ACCOUNT            PIC X(10).                                      
           12  FILLER                PIC X    VALUE '|'.
00042      12  EX-CERT               PIC X(10).                                               
           12  FILLER                PIC X    VALUE '|'.
00046      12  EX-CERT-SFX           PIC X.                 
           12  FILLER                PIC X    VALUE '|'.
00056      12  EX-BILL-START         PIC Z9. 
           12  FILLER                PIC X    VALUE '|'.
00056      12  EX-BILL-END           PIC Z9. 
           12  FILLER                PIC X    VALUE '|'.
00057      12  EX-NOTE-1             PIC X(77). 
           12  FILLER                PIC X    VALUE '|'.
00057      12  EX-NOTE-2             PIC X(77). 
           12  FILLER                PIC X    VALUE '|'.
00057      12  EX-NOTE-3             PIC X(77). 
           12  FILLER                PIC X    VALUE '|'.
00057      12  EX-NOTE-4             PIC X(77). 
           12  FILLER                PIC X    VALUE '|'.
00057      12  EX-NOTE-5             PIC X(77). 
           12  FILLER                PIC X    VALUE '|'.
00057      12  EX-NOTE-6             PIC X(77). 
           12  FILLER                PIC X    VALUE '|'.
00057      12  EX-NOTE-7             PIC X(77). 
           12  FILLER                PIC X    VALUE '|'.
00057      12  EX-NOTE-8             PIC X(77). 
           12  FILLER                PIC X    VALUE '|'.
00057      12  EX-NOTE-9             PIC X(77). 
           12  FILLER                PIC X    VALUE '|'.
00057      12  EX-NOTE-10            PIC X(77). 
           12  FILLER                PIC X    VALUE '|'.
00057      12  EX-MAINT-BY           PIC X(4). 
           

       PROCEDURE DIVISION.

           PERFORM 0040-OPEN-FILES     THRU 0040-EXIT
           PERFORM 0050-INIT           THRU 0050-EXIT

           PERFORM 0020-PROCESS        THRU 0020-EXIT UNTIL
              (END-OF-NOTE)
           PERFORM 0060-CLOSE-FILES    THRU 0060-EXIT

           DISPLAY ' NOTE RECS READ  ' NOTE-RECS-IN
           DISPLAY ' NOTE RECS WRITE ' NOTE-RECS-OUT

           GOBACK
           .

       0010-START-ERNOTE.

           MOVE LOW-VALUES             TO CN-CONTROL-PRIMARY
           MOVE ZERO                   TO CN-CARRIER
           MOVE X'04'                  TO CN-COMPANY-CD

           START ERNOTE KEY IS NOT < CN-CONTROL-PRIMARY

           IF ERNOTE-FILE-STATUS = '10' OR '23'
              SET END-OF-NOTE        TO TRUE
           ELSE
              IF ERNOTE-FILE-STATUS NOT = '00'
                 DISPLAY 'ERNOTE START     ' ERNOTE-FILE-STATUS
                 SET END-OF-NOTE     TO TRUE
              END-IF
           END-IF

           .

       0010-EXIT.
           EXIT.

       0015-READ-ERNOTE.

           READ ERNOTE NEXT RECORD

           IF ERNOTE-FILE-STATUS = '00'
              ADD 1                    TO NOTE-RECS-IN
           ELSE
              IF (ERNOTE-FILE-STATUS = '23' OR '10')
                 SET END-OF-NOTE     TO TRUE
                 GO TO 0015-EXIT
              ELSE
                 DISPLAY ' BAD READ ERNOTE ' CN-CONTROL-PRIMARY '  '
                    ERNOTE-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
           
           IF CN-BILLING-START-LINE-NO NOT NUMERIC
               MOVE ZERO TO CN-BILLING-START-LINE-NO
           END-IF
           IF CN-BILLING-END-LINE-NO NOT NUMERIC
               MOVE ZERO TO CN-BILLING-END-LINE-NO
           END-IF
           
           IF CN-BILLING-START-LINE-NO = ZERO AND
              CN-BILLING-END-LINE-NO = ZERO
                  GO TO 0015-READ-ERNOTE
          END-IF.

           .
       0015-EXIT.
           EXIT.

       0020-PROCESS.
       
           PERFORM 0025-READ-PENDING THRU 0025-EXIT
       
           IF CN-CONTROL-PRIMARY = PB-CONTROL-BY-ACCOUNT (1:33)
                PERFORM 0015-READ-ERNOTE THRU 0015-EXIT
                GO TO 0020-EXIT
           END-IF



           MOVE  CN-CARRIER            TO  EX-CARRIER           
           MOVE  CN-GROUPING           TO  EX-GROUPING                      
           MOVE  CN-STATE              TO  EX-STATE                         
           MOVE  CN-ACCOUNT            TO  EX-ACCOUNT  
           MOVE  CN-CERT-PRIME         TO  EX-CERT                        
           MOVE  CN-CERT-SFX           TO  EX-CERT-SFX   
           MOVE  CN-BILLING-START-LINE-NO TO EX-BILL-START
           MOVE  CN-BILLING-END-LINE-NO TO EX-BILL-END                   
           MOVE  CN-LINE (1)           TO  EX-NOTE-1  
           MOVE  CN-LINE (2)           TO  EX-NOTE-2  
           MOVE  CN-LINE (3)           TO  EX-NOTE-3  
           MOVE  CN-LINE (4)           TO  EX-NOTE-4  
           MOVE  CN-LINE (5)           TO  EX-NOTE-5  
           MOVE  CN-LINE (6)           TO  EX-NOTE-6  
           MOVE  CN-LINE (7)           TO  EX-NOTE-7  
           MOVE  CN-LINE (8)           TO  EX-NOTE-8  
           MOVE  CN-LINE (9)           TO  EX-NOTE-9  
           MOVE  CN-LINE (10)          TO  EX-NOTE-10
           MOVE  CN-LAST-MAINT-USER    TO  EX-MAINT-BY                     
           PERFORM 0030-WRITE-EXT  THRU 0030-EXIT
           
           MOVE ZERO TO CN-BILLING-START-LINE-NO
                        CN-BILLING-END-LINE-NO
                        
           PERFORM 0035-REWRITE-ERNOTE THRU 0035-EXIT

           PERFORM 0015-READ-ERNOTE    THRU 0015-EXIT
              

           .
       0020-EXIT.
            EXIT.

       0025-READ-PENDING.
       
           MOVE LOW-VALUES             TO PB-CONTROL-BY-ACCOUNT
           MOVE CN-COMPANY-CD          TO PB-COMPANY-CD-A1
           MOVE CN-CARRIER             TO PB-CARRIER
           MOVE CN-GROUPING            TO PB-GROUPING
           MOVE CN-STATE               TO PB-STATE
           MOVE CN-ACCOUNT             TO PB-ACCOUNT
           MOVE CN-CERT-EFF-DT         TO PB-CERT-EFF-DT
           MOVE CN-CERT-NO             TO PB-CERT-NO
           MOVE ZERO                   TO PB-ALT-CHG-SEQ-NO
           MOVE SPACES                 TO PB-RECORD-TYPE

           START ERPNDB KEY >= PB-CONTROL-BY-ACCOUNT
           IF ERPNDB-FILE-STATUS NOT = '00'
               DISPLAY ' ERROR ON ERPNDB - START '                      
                  ERPNDB-FILE-STATUS 
                  '   CERT NO = ' CN-CERT-NO
               GO TO 0025-EXIT
           END-IF
 
           READ ERPNDB NEXT RECORD
           IF ERPNDB-FILE-STATUS NOT = '00'
               DISPLAY ' ERROR ON ERPNDB - READNEXT '                      
                  ERPNDB-FILE-STATUS 
                  '   CERT NO = ' CN-CERT-NO
           END-IF

           .
       0025-EXIT.
            EXIT.


       0030-WRITE-EXT.
       
    
           WRITE EXT-RECORD         FROM EXT-REC
           ADD 1                    TO NOTE-RECS-OUT

           .
       0030-EXIT.
            EXIT.


       0035-REWRITE-ERNOTE.

           REWRITE CERTIFICATE-NOTE
      *     GO TO 0035-EXIT

           IF ERNOTE-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERNOTE - REWRITE  ' ERNOTE-FILE-STATUS
              SET END-OF-NOTE          TO TRUE
           ELSE
              ADD 1                    TO NOTE-RECS-FIX
           END-IF

           .
       0035-EXIT.
           EXIT.


              
       0040-OPEN-FILES.
       
           OPEN I-O ERNOTE
           OPEN INPUT ERPNDB

           IF ERNOTE-FILE-STATUS NOT = '00'
              DISPLAY ' ERNOTE OPEN ' ERNOTE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPNDB-FILE-STATUS NOT = '00'
              DISPLAY ' ERPNDB OPEN ' ERPNDB-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           OPEN output ext-out.

           .
       0040-EXIT.
           EXIT.

       0050-INIT.
       
           PERFORM 0010-START-ERNOTE   THRU 0010-EXIT
           PERFORM 0015-READ-ERNOTE    THRU 0015-EXIT
           .
       0050-EXIT.
           EXIT.

       0060-CLOSE-FILES.
       
           CLOSE ERNOTE ERPNDB

           IF ERNOTE-FILE-STATUS NOT = '00'
              DISPLAY ' ERNOTE CLOSE ' ERNOTE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERPNDB-FILE-STATUS NOT = '00'
              DISPLAY ' ERPNDB CLOSE ' ERPNDB-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           CLOSE EXT-OUT.

           .
       0060-EXIT.
           EXIT.



       ABEND-PGM.   COPY ELCABEND.

