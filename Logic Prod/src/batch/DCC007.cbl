       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCC007
       AUTHOR.     PABLO.
       DATE-COMPILED.
                                                                        
      ***************************************************************** 
      *                                                               * 
      *  THIS PROGRAM READS THE BILLING STATEMENT PRINT LINES CREATED * 
      *  IN PROGRAM EL562 AND CREATES:                                * 
      *                                                               * 
      *   1) BILLING STATEMENTS FOR ACCOUNTS WITH A BILL/REMIT CODE   * 
      *      ON THE COMPENSATION MASTER OF "B" AND THE REMIT AMOUNT   * 
      *      IS POSITIVE.  THE PROGRAM ADDS A BARCODE TO THE STATEMENT* 
      *      AND WRITES THE STATEMENT LINES TO A FILE.  A COVER LETTER* 
      *      WITH A MATCHING BARCODE IS ADDED BEFORE EACH STATEMENT.  * 
      *      THE COVER LETTERS AND STATEMENTS ARE PRINTED BY A PRINT  * 
      *      UTILITY PROGRAM AND ARE SENT TO THE MAIL ROOM WHERE THEY * 
      *      ARE MERGED, STUFFED INTO ENVELOPES AND MAILED.           * 
      *                                                               * 
      *   2) BILLING STATEMENTS FOR ACCOUNTS WITH A BILL/REMIT CODE   * 
      *      ON THE COMPENSATION MASTER OF "B" AND THE REMIT AMOUNT   * 
      *      IS NEGATIVE (REFUND).  THE PROGRAM ADDS A COVER LETTER   * 
      *      AND A CASH DISBURSMENT VOUCHER BEFORE EACH STATEMENT.    * 
      *      THE COVER LETTERS AND STATEMENTS ARE PRINTED BY A PRINT  * 
      *      UTILITY PROGRAM AND ARE SENT TO CID.                     * 
      *                                                               * 
      *   3) BILLING STATEMENTS FOR ACCOUNTS WITH A BILL/REMIT CODE   * 
      *      ON THE COMPENSATION MASTER OF "S".   THESE ARE PRINTED   * 
      *      BY A UTILITY PROGRAM AND SENT TO CID.                    * 
      *                                                               * 
      *   4) BILLING STATEMENTS FOR ACCOUNTS WITH A BILL/REMIT CODE   * 
      *      ON THE COMPENSATION MASTER OF "T" OR " ".  THESE ARE     * 
      *      PRINTED BY A UTILITY AND SENT TO CID.                    * 
      *                                                               * 
      *                                                               * 
      *  NOTES:                                                       * 
      *  ======                                                       * 
      *   1)  CBL NOADV WAS SET BECAUSE THE INPUT RECORDS ALREADY     * 
      *       HAVE PRINTER CONTROL CHARACTERS                         * 
      *                                                               * 
      *                                                               * 
      ***************************************************************** 
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 071312    2010082600001  PEMA  NEW PROGRAM
102512* 102512  CR2012040900002  PEMA  ADD USER NOTES CAPABILITY
062513* 062513  CR2013061000003  PEMA  REMOVE CONTACT NAME FROM VOUCHERS
092513* 092513  CR2013062000003  PEMA  SPACE OUT BARCODE ON REFUND STMTS
      ******************************************************************
                                                                        
       ENVIRONMENT DIVISION.                                            
                                                                        
       INPUT-OUTPUT SECTION.                                            
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT ERCOMP               ASSIGN TO ERCOMP                          
                  ORGANIZATION IS INDEXED                               
                  ACCESS IS DYNAMIC                                     
                  RECORD KEY IS CO-CONTROL-PRIMARY                      
                  FILE STATUS IS ERCOMP-STATUS.                         
                                                                        
102512     SELECT FORMDEFS  ASSIGN TO FORMDEFS
102512            ORGANIZATION IS INDEXED
102512            ACCESS IS DYNAMIC
102512            RECORD KEY IS fd-form-key
102512            FILE STATUS IS FORMDEFS-FILE-STATUS.

           SELECT BILLING-STATEMENTS   ASSIGN TO SYS010.
      
           SELECT DISK-DATE                                             
                  ASSIGN TO SYS019.                                     
                                                                        
           SELECT REFUND-STATEMENTS1   ASSIGN TO SYS011.

           SELECT REFUND-STATEMENTS2   ASSIGN TO SYS012.

           SELECT ZERO-STATEMENTS1     ASSIGN TO SYS013.

           SELECT ZERO-STATEMENTS2     ASSIGN TO SYS014.

           SELECT KEYS-EXT             ASSIGN TO KEYSOT
              ORGANIZATION LINE SEQUENTIAL.
      
       DATA DIVISION.                                                   
      
       FILE SECTION.                                                    
                                                                        
       FD  BILLING-STATEMENTS                                           
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  STMT-RECORD       PIC X(133).                                
      
       FD  ERCOMP.                                                      
           COPY ERCCOMP.                                                
                                                                        
102512 FD  FORMDEFS.
102512 01  FORM-DEF-IN-REC.
102512     05  fd-form-key.
102512         10  fd-form-name        pic x(10).
102512         10  fd-form-month       pic 99.
102512     05  f                       pic x(888).

       FD  DISK-DATE                                                    
           COPY ELCDTEFD.                                               
      
       FD  REFUND-STATEMENTS1
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REFUND-RECORD1              PIC X(133).
      
       FD  REFUND-STATEMENTS2
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REFUND-RECORD2              PIC X(133).
      
       FD  ZERO-STATEMENTS1
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ZERO-RECORD1                PIC X(133).

       FD  ZERO-STATEMENTS2
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  ZERO-RECORD2                PIC X(133).

       FD  KEYS-EXT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.                                            
      
       01  KEYS-EXT-RECORD             PIC X(57).
      
       WORKING-STORAGE SECTION.                                         
       copy "ctypes.cpy".
                                                                        
       77  WS-REF1-ADDR-SEQ-NO         PIC 9(7)  VALUE ZEROS.
       77  WS-ZERO1-ADDR-SEQ-NO        PIC 9(7)  VALUE ZEROS.
       77  WS-REF2-ADDR-SEQ-NO         PIC 9(7)  VALUE ZEROS.
       77  WS-ZERO2-ADDR-SEQ-NO        PIC 9(7)  VALUE ZEROS.
       77  WS-REF3-ADDR-SEQ-NO         PIC 9(7)  VALUE ZEROS.
       77  WS-VOC-ADDR-SEQ-NO          PIC 9(7)  VALUE ZEROS.
       77  A1                          PIC S999  VALUE +0 COMP-3.
       77  WS-DIS-ADDR-SEQ-NO          PIC ZZZZZZ9 BLANK WHEN ZERO.
       77  WS-REF1-SEQ-NO              PIC 9(4) VALUE ZEROS.
       77  WS-ZERO1-SEQ-NO             PIC 9(4) VALUE ZEROS.
       77  WS-REF2-SEQ-NO              PIC 9(4) VALUE ZEROS.
       77  WS-ZERO2-SEQ-NO             PIC 9(4) VALUE ZEROS.
       77  WS-END-BAL                  PIC S9(9)V99  VALUE +0 COMP-3.
102512 77  FORMDEFS-FILE-STATUS        PIC XX  VALUE LOW-VALUES.
       77  ws-eof-sw                   pic x  value ' '.
           88  end-of-input              value 'Y'.
      
       01  FILLER.
           05  ws-ahl-pd-date1         PIC X(18)   VALUE SPACE.
           05  ws-ahl-vr-date1         PIC X(18)   VALUE SPACE.
           05  ws-ahl-vr-date2         PIC X(08)   VALUE SPACE.
           05  WS-SUMMARY-SW           PIC X   VALUE SPACES.
               88  LAST-PAGE-SUMMARY          VALUE 'Y'.
               88  LAST-PAGE-DETAIL           VALUE 'D'.
           05  WS-WORK-DATE            PIC 9(11)  VALUE ZEROS.
           05  WS-WORK-DATER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-SCAN-CCYYMM      PIC X(6).
               10  FILLER              PIC XX.
           05  WS-RETURN-CODE          PIC S9(4)  COMP  VALUE ZEROS.    
           05  WS-ZERO                 PIC S9     VALUE +0  COMP-3.
           05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         
           05  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZEROS.          
           05  PGM-SUB             PIC S999   COMP-3  VALUE +061.   
           05  S0C7                PIC X       VALUE SPACE.             
           05  FORCE-DUMP REDEFINES S0C7 PIC S9 COMP-3.                 
           05  SYS010-STATUS       PIC X(2)    VALUE SPACE.             
               88  EOF                         VALUE '10'.              
           05  ERCOMP-STATUS       PIC X(2)    VALUE SPACE.             
           05  SYS030-STATUS       PIC X(2)    VALUE SPACE.             
           05  SYS031-STATUS       PIC X(2)    VALUE SPACE.             
           05  SYS032-STATUS       PIC X(2)    VALUE SPACE.             
           05  SAVE-BARCODE1       PIC X(50)   VALUE SPACE.             
           05  SAVE-BARCODE2       PIC X(25)   VALUE SPACE.             
           05  PREV-CO-CONTROL     PIC X(29)   VALUE SPACE.             
           05  TYPE-OF-STATEMENT   PIC X(7)    VALUE SPACE.             
           05  REMIT-PAGE-NO       PIC 9(6)    VALUE ZERO.              
           05  WS-DATE             PIC 9(8)    VALUE ZERO.              
           05  WS-HDG  OCCURS 4 TIMES PIC X(133).                       
           05  WK-ADDR OCCURS 7 TIMES PIC X(30).
           05  WK-AMT              PIC ZZZ,ZZZ,ZZZ.99/.                 

       01  WS-PREV-KEYS-KEY.
           05  WS-PREV-KEY-CARRIER     PIC X.
           05  WS-PREV-KEY-GROUP       PIC X(6).
           05  WS-PREV-KEY-FIN-RESP    PIC X(10).
           05  WS-PREV-KEY-ACCOUNT     PIC X(10).
      
       01  WS-SAVE-KEY-REC         PIC X(57).
       01  CID-KEY-REC.
           05  SAV-CID-CC              PIC 99.
           05  SAV-CID-YY              PIC 99.
           05  SAV-CID-MM              PIC 99.
           05  CID-KEY-SEQ-NO          PIC 9(7).
           05  SR-DEL1                 PIC X.
           05  CID-STMT-TYPE           PIC X.
           05  SR-DEL2                 PIC X.
           05  CID-CARRIER             PIC X.
           05  SR-DEL3                 PIC X.
           05  CID-GROUP               PIC X(6).
           05  SR-DEL4                 PIC X.
           05  CID-FIN-RESP            PIC X(10).
           05  SR-DEL5                 PIC X.
           05  CID-ACCOUNT             PIC X(10).
           05  SR-DEL6                 PIC X.
           05  CID-AMT-DUE             PIC 9(7).99.
      
       01  FILLER               COMP-3.                                 
           05  STRT             PIC S9(3)   VALUE +0.                   
           05  SUB              PIC S9(3)   VALUE +0.                   
           05  WK1              PIC S9(7)   VALUE +0.                   
           05  WK2              PIC S9(7)   VALUE +0.                   
      
      
102512                                 COPY FORMREC.                                                                        
102512
102512 01  refund-wso111.
102512     03  wso111-key.
102512         05  wso111-name         pic x(10).
102512         05  wso111-month        pic 99.
102512     03  wso111-desc             pic x(30).
102512     03  wso111-special-notes occurs 8
102512                                 pic x(75).
102512     03  wso111-comment-1        pic x(95).
102512     03  wso111-comment-2        pic x(95).
102512     03  filler                  pic x(68).
102512
102512 01  refund-wso112.
102512     03  wso112-key.
102512         05  wso112-name         pic x(10).
102512         05  wso112-month        pic 99.
102512     03  wso112-desc             pic x(30).
102512     03  wso112-special-notes occurs 8
102512                                 pic x(75).
102512     03  wso112-comment-1        pic x(95).
102512     03  wso112-comment-2        pic x(95).
102512     03  filler                  pic x(68).
102512
102512 01  zerobal-wso121.
102512     03  wso121-key.
102512         05  wso121-name         pic x(10).
102512         05  wso121-month        pic 99.
102512     03  wso121-desc             pic x(30).
102512     03  wso121-special-notes occurs 8
102512                                 pic x(75).
102512     03  wso121-comment-1        pic x(95).
102512     03  wso121-comment-2        pic x(95).
102512     03  filler                  pic x(68).
102512
102512 01  zerobal-wso122.
102512     03  wso122-key.
102512         05  wso122-name         pic x(10).
102512         05  wso122-month        pic 99.
102512     03  wso122-desc             pic x(30).
102512     03  wso122-special-notes occurs 8
102512                                 pic x(75).
102512     03  wso122-comment-1        pic x(95).
102512     03  wso122-comment-2        pic x(95).
102512     03  filler                  pic x(68).

       01  WS-COVER-SHEET.
           05  PD-CC               PIC X.
           05  PD-ID               PIC X(4).
           05  PD-CARRIER          PIC X.
           05  PD-DATE1            PIC X(18).
           05  PD-CUR              PIC ---,--9.99.
           05  PD-OV30             PIC ----,--9.99.
           05  PD-OV60             PIC ----,--9.99.
           05  PD-OV90             PIC ----,--9.99.
           05  PD-END-BAL          PIC ----,--9.99.
           05  PD-ACCOUNT          PIC X(10).
           05  PD-PMT              PIC ----,--9.99.
           05  PD-BAL-FWD          PIC ----,--9.99.
           05  PD-USER-SELECT-2    PIC X(13).
           05  PD-REPORT-CODE-1    PIC X(10).
       01  REDEFINES WS-COVER-SHEET.
           05  FILLER              PIC X.
           05  PD-BARCODE1         PIC X(50).
           05  FILLER              PIC X(10).
           05  PD-BARCODE2         PIC X(25).
       01  REDEFINES WS-COVER-SHEET.
           05  FILLER           PIC X.
           05  PD-ADDRESS       PIC X(132).
102512 01  REDEFINES WS-COVER-SHEET.
102512     05  FILLER           PIC X.
102512     05  PD-SPEC-NOTES    PIC X(132).
                                                                        
       01  WS-VOUCHER-RECORD.                                           
           05  VR-CC            PIC X       VALUE SPACE.                
           05  VR-ID.
               10  VR-VOUCHER-CONST    PIC X(7) VALUE SPACES.
               10  VR-CARRIER          PIC X VALUE SPACES.
               10  FILLER              PIC X(7) VALUE SPACES.
           05  VR-DATE1         PIC X(18)   VALUE SPACE.                
           05  VR-DATE2         PIC X(08)   VALUE SPACE.                
           05  VR-ACCOUNT.                                              
               10  VR-CARR      PIC X(07)   VALUE SPACE.                
               10  VR-ACCT      PIC X(10)   VALUE SPACE.                
           05  VR-AMT           PIC ZZZZ,ZZZ,ZZZ.99.
           05  FILLER           PIC X(36)   VALUE SPACES.
           05  VR-CSR           PIC X(4)    VALUE SPACES.
           05  FILLER           PIC X(19)   VALUE SPACES.
      *    05  FILLER           PIC X(59)   VALUE SPACE.                
       01  REDEFINES WS-VOUCHER-RECORD.                                 
           05  FILLER           PIC X.                                  
           05  VR-ADDRESS       PIC X(132).                             
                                                                        
       01  LETTER-TABLES          VALUE IS SPACE.                       
           05  REFUND-LETTER-LINE OCCURS 10 TIMES PIC X(132).           
           05  ZERO-LETTER-LINE   OCCURS 10 TIMES PIC X(132).           
      
      ***************************************************************** 
      *  BARCODE ROUTINE                                              * 
      ***************************************************************** 
       01  AGEB16-WORKAREA.                                             
      *     05  AGEB16-LEN       PIC 9(4)    VALUE ZERO.                 
           05  AGEB16-LEN       short.                 
           05  AGEB16-INPUT     PIC X(28)   VALUE SPACE.                
           05  AGEB16-OUTPUT    PIC X(128)  VALUE SPACE.                
                                                                        
       01  BARCODE1.                                                     
           05  BC-ENCL-CODE        PIC X(2)    VALUE ZERO.              
           05  BC-MAIL-CODE        PIC X       VALUE '1'.               
           05  BC-DIV-CODE         PIC X       VALUE '1'.               
           05  BC-ACCT-NO.                                              
               10  BC-CARR         PIC X(07)   VALUE ZERO.              
               10  BC-ACCT         PIC X(10)   VALUE ZERO.              
           05  BC-SEQ-NO           PIC 9(4)    VALUE ZERO.              
                                                                        
       01  BARCODE2.
           05  BC1-DATE            PIC X(6).                             
           05  BC1-SEQ-NO          PIC 9(7)    VALUE ZERO.              
                                                                        
                                       copy ELCDATE.
                                       COPY ELCDTECX.
      
                                       COPY ELCDTEVR.
      
       PROCEDURE DIVISION.                                              
                                                                        
                                       COPY ELCDTERX.
           PERFORM 0000-INIT-ROUTINE   THRU 0000-EXIT                     
           PERFORM 1000-PROCESS-FILE   THRU 1000-EXIT
              UNTIL end-of-input
           PERFORM 9000-END-OF-JOB     THRU 9000-EXIT                     
           GOBACK
      
           .
       0000-INIT-ROUTINE.

           OPEN INPUT BILLING-STATEMENTS
                                                                        
           OPEN INPUT ERCOMP                                            
           IF ERCOMP-STATUS NOT = '00'                                  
              DISPLAY 'OPEN ERROR ' ERCOMP-STATUS ' ON ERCOMP'          
              MOVE +16 TO RETURN-CODE                                   
              ADD +1 TO FORCE-DUMP.                                     
                                                                        
102512     OPEN INPUT FORMDEFS
102512     IF FORMDEFS-FILE-STATUS NOT = '00' AND '97'
102512        DISPLAY ' ERROR - FORMDEFS - OPEN ' FORMDEFS-FILE-STATUS
102512        PERFORM ABEND-PGM
102512     END-IF

     
           OPEN OUTPUT REFUND-STATEMENTS1 refund-statements2
                       ZERO-STATEMENTS1 zero-statements2
                       KEYS-EXT
      
           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE WS-SCAN-CCYYMM         TO BC1-DATE
      
      
           MOVE SPACES                 TO CID-KEY-REC
           MOVE WS-SCAN-CCYYMM         TO CID-KEY-REC (1:6)
           MOVE ZEROS                  TO CID-KEY-SEQ-NO
           MOVE ZEROS                  TO CID-AMT-DUE
           MOVE ';'                    TO SR-DEL1
                                          SR-DEL2
                                          SR-DEL3
                                          SR-DEL4
                                          SR-DEL5
                                          SR-DEL6
           MOVE CID-KEY-REC            TO WS-SAVE-KEY-REC

102512     move 'DCO111'               to fd-form-name
102512
102512     move run-mo                 to fd-form-month
102512
102512     read FORMDEFS
102512     evaluate true
102512        when formdefs-file-status = '00'
102512           move form-def-in-rec  to refund-wso111
102512        when formdefs-file-status = '23'
102512           move 01               to fd-form-month
102512           read FORMDEFS
102512           if formdefs-file-status = '00'
102512              move form-def-in-rec
102512                                 to refund-wso111
102512           else
102512              move spaces        to refund-wso111
102512           end-if
102512        when other
102512           move spaces           to refund-wso111
102512     end-evaluate
102512
102512     move 'DCO112'               to fd-form-name
102512
102512     move run-mo                 to fd-form-month
102512
102512     read FORMDEFS
102512     evaluate true
102512        when formdefs-file-status = '00'
102512           move form-def-in-rec  to refund-wso112
102512        when formdefs-file-status = '23'
102512           move 01               to fd-form-month
102512           read FORMDEFS
102512           if formdefs-file-status = '00'
102512              move form-def-in-rec
102512                                 to refund-wso112
102512           else
102512              move spaces        to refund-wso112
102512           end-if
102512        when other
102512           move spaces           to refund-wso112
102512     end-evaluate
102512
102512     move 'DCO121'               to fd-form-name
102512
102512     move run-mo                 to fd-form-month
102512
102512     read FORMDEFS
102512     evaluate true
102512        when formdefs-file-status = '00'
102512           move form-def-in-rec  to zerobal-wso121
102512        when formdefs-file-status = '23'
102512           move 01               to fd-form-month
102512           read FORMDEFS
102512           if formdefs-file-status = '00'
102512              move form-def-in-rec
102512                                 to zerobal-wso121
102512           else
102512              move spaces        to zerobal-wso121
102512           end-if
102512        when other
102512           move spaces           to zerobal-wso121
102512     end-evaluate
102512
102512     move 'DCO122'               to fd-form-name
102512
102512     move run-mo                 to fd-form-month
102512
102512     read FORMDEFS
102512     evaluate true
102512        when formdefs-file-status = '00'
102512           move form-def-in-rec  to zerobal-wso122
102512        when formdefs-file-status = '23'
102512           move 01               to fd-form-month
102512           read FORMDEFS
102512           if formdefs-file-status = '00'
102512              move form-def-in-rec
102512                                 to zerobal-wso122
102512           else
102512              move spaces        to zerobal-wso122
102512           end-if
102512        when other
102512           move spaces           to zerobal-wso122
102512     end-evaluate
102512
102512     close FORMDEFS              

           perform 0050-read-input     thru 0050-exit

           .                                                            
       0000-EXIT.                                                       
           EXIT.                                                        

       0050-read-input.      

           READ BILLING-STATEMENTS at end
              set end-of-input to true                                      
           end-read

           .
       0050-exit.
           exit.

       1000-PROCESS-FILE.                                               
                                                                        
           IF STMT-RECORD(1:1) = '1'
              PERFORM 2000-NEW-PAGE    THRU 2000-EXIT
              GO TO 1000-CONTINUE
           END-IF
      
           evaluate true
              when type-of-statement = 'REFUND'
                 if co-carrier = '1' or '3' or '5'
                    write refund-record1 from stmt-record
                 else
                    write refund-record2 from stmt-record
                 end-if
              when type-of-statement = 'ZERO'
                 if co-carrier = '1' or '3' or '5'
                    WRITE ZERO-RECORD1 FROM STMT-RECORD
                 else
                    WRITE ZERO-RECORD2 FROM STMT-RECORD
                 end-if
           end-evaluate

           .
       1000-continue.

           perform 0050-read-input     thru 0050-exit

           .                                                            
       1000-EXIT.                                                       
           EXIT.                                                        
      
       2000-NEW-PAGE.

      ***--------------------------------------------------***          
      ***  CHECK TO SEE IF A BARCODE LINE IS NEEDED                     
      ***  ON THE PREVIOUS PAGE BEFORE PRINTING THIS PAGE.              
      ***  NOTE:  BARCODES ARE ONLY ON ODD NUMBERED PAGES.              
      ***--------------------------------------------------***          
      
           MOVE SPACE        TO TYPE-OF-STATEMENT                              
                                                                        
           MOVE STMT-RECORD            TO WS-HDG (1)
           PERFORM VARYING SUB FROM 2 BY 1 UNTIL
              SUB > 4                
              READ BILLING-STATEMENTS INTO WS-HDG(SUB)             
           END-PERFORM                                                  
                                                                        
           IF WS-HDG (1)(61:13) = 'OVERALL RECAP'                        
              GO TO 2000-EXIT
           end-if
      
           PERFORM 2200-READ-ERCOMP    THRU 2200-EXIT                      
           IF ERCOMP-STATUS NOT = '00'                                  
              GO TO 2000-EXIT.                                          
                                                                        
           IF CO-TYPE NOT = 'A'                                         
              GO TO 2000-EXIT.                                          
      
           IF CO-END-BAL < ZEROS
              MOVE 'REFUND'            TO TYPE-OF-STATEMENT
              PERFORM 2400-PRINT-VOUCHER
                                       THRU 2400-EXIT
              PERFORM 2500-COVER-SHEET THRU 2500-EXIT
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                 if co-carrier = '1' or '3' or '5'
                    write refund-record1 from ws-hdg (sub)
                 else
                    write refund-record2 from ws-hdg (sub)
                 end-if
              END-PERFORM
           ELSE
              IF CO-CURRENT-END-BAL = ZEROS
                 MOVE 'ZERO'  TO TYPE-OF-STATEMENT
                 IF CO-CONTROL NOT = WS-PREV-KEYS-KEY
                    MOVE WS-SAVE-KEY-REC  TO CID-KEY-REC
                    MOVE ZEROS            TO CID-KEY-SEQ-NO
                    MOVE '2'              TO CID-STMT-TYPE
                    MOVE CO-CARRIER       TO CID-CARRIER
                    MOVE CO-GROUPING      TO CID-GROUP
                    MOVE CO-RESP-NO       TO CID-FIN-RESP
                    MOVE CO-ACCOUNT       TO CID-ACCOUNT
                    MOVE CO-CURRENT-END-BAL
                                          TO CID-AMT-DUE
                    WRITE KEYS-EXT-RECORD FROM CID-KEY-REC
                    MOVE CO-CONTROL       TO WS-PREV-KEYS-KEY
                 END-IF
                 PERFORM 2500-COVER-SHEET THRU 2500-EXIT
                 PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                    if co-carrier = '1' or '3' or '5'
                       WRITE ZERO-RECORD1 FROM ws-hdg (sub)
                    else
                       WRITE ZERO-RECORD2 FROM ws-hdg (sub)
                    end-if
                 END-PERFORM
              END-IF
           END-IF                                                       
      
           MOVE CO-CONTROL-PRIMARY TO PREV-CO-CONTROL                   
      
           .                                                            
       2000-EXIT.                                                       
           EXIT.                                                        
      
       2100-BUILD-BARCODE1.                                              
      
           MOVE CO-CARR-GROUP          TO BC-CARR
           MOVE CO-ACCOUNT             TO BC-ACCT
           add 1 to bc-seq-no

           MOVE  28                    TO AGEB16-LEN
      
           MOVE BARCODE1               TO AGEB16-INPUT
           MOVE SPACES                 TO AGEB16-OUTPUT
           CALL 'AGEB16' USING AGEB16-LEN
                               AGEB16-INPUT
                               AGEB16-OUTPUT
           IF AGEB16-LEN = +128
              DISPLAY 'AGEB16 - BARCODE1 ROUTINE ERROR'
           ELSE
              MOVE AGEB16-OUTPUT       TO SAVE-BARCODE1
           END-IF

           .
       2100-EXIT.                                                       
           EXIT.                                                        

       2200-READ-ERCOMP.                                                
      
           MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD
      
           MOVE WS-HDG(4)(16:01) TO CO-CARRIER                          
           MOVE WS-HDG(4)(17:06) TO CO-GROUPING                         
           MOVE WS-HDG(4)(90:10) TO CO-RESP-NO                          
           MOVE WS-HDG(4)(24:10) TO CO-ACCOUNT                          
           MOVE 'A'              TO CO-TYPE                             
      
           IF CO-RESP-NO = SPACES                                       
              MOVE CO-ACCOUNT TO CO-RESP-NO                             
           END-IF                                                       
                                                                        
           IF CO-CONTROL-PRIMARY = PREV-CO-CONTROL                      
              GO TO 2200-EXIT.                                          
                                                                        
           READ ERCOMP                                                  
           IF ERCOMP-STATUS NOT = '00'                                  
              DISPLAY 'READ ERROR ' ERCOMP-STATUS ' ON ERCOMP.   '      
                      'KEY=' CO-CONTROL-PRIMARY                         
           END-IF                                                       

           .                                                            
       2200-EXIT.                                                       
           EXIT.                                                        
      
       2500-COVER-SHEET.
      
           IF CO-CONTROL-PRIMARY = PREV-CO-CONTROL                      
              GO TO 2500-EXIT
           END-IF
      
           MOVE '1'                    TO PD-CC
           MOVE 'PDUE'                 TO PD-ID
      
           MOVE +0                  TO STRT
           INSPECT ALPH-DATE TALLYING STRT FOR LEADING ' '
           MOVE ALPH-DATE(STRT + 1:)   TO PD-DATE1
      
           IF CO-CARRIER = '1' OR '3' OR '5'
              MOVE '1'                 TO PD-CARRIER
           ELSE
              MOVE '2'                 TO PD-CARRIER
           END-IF
           MOVE CO-ACCOUNT             TO PD-ACCOUNT
           MOVE CO-CUR                 TO PD-CUR
           MOVE CO-OV30                TO PD-OV30
           MOVE CO-OV60                TO PD-OV60
           MOVE CO-OV90                TO PD-OV90
           MOVE CO-END-BAL             TO PD-END-BAL
           MOVE CO-CUR-PMT             TO PD-PMT
           MOVE CO-BAL-FWD             TO PD-BAL-FWD
           MOVE SPACES                 TO PD-USER-SELECT-2
           MOVE SPACES                 TO PD-REPORT-CODE-1

           evaluate true
              when type-of-statement = 'REFUND'
                 if co-carrier = '1' or '3' or '5'
                    write refund-record1 from ws-cover-sheet
                 else
                    write refund-record2 from ws-cover-sheet
                 end-if
              when type-of-statement = 'ZERO'
                 if co-carrier = '1' or '3' or '5'
                    WRITE ZERO-RECORD1 FROM ws-cover-sheet
                 else
                    WRITE ZERO-RECORD2 FROM ws-cover-sheet
                 end-if
           end-evaluate

           MOVE SPACES                 TO WS-COVER-SHEET
           perform 2100-BUILD-BARCODE1 thru 2100-exit
           MOVE SAVE-BARCODE1          TO PD-BARCODE1
           evaluate true
              when type-of-statement = 'REFUND'
092513           move spaces           to pd-barcode1
                 if co-carrier = '1' or '3' or '5'
                    write refund-record1 from ws-cover-sheet
                    add 1              to ws-ref1-addr-seq-no
                    move ws-ref1-addr-seq-no
                                       to ws-dis-addr-seq-no
                 else
                    write refund-record2 from ws-cover-sheet
                    add 1              to ws-ref2-addr-seq-no
                    move ws-ref2-addr-seq-no
                                       to ws-dis-addr-seq-no
                 end-if
              when type-of-statement = 'ZERO'
                 if co-carrier = '1' or '3' or '5'
                    WRITE ZERO-RECORD1 FROM ws-cover-sheet
                    add 1              to ws-zero1-addr-seq-no
                    move ws-zero1-addr-seq-no
                                       to ws-dis-addr-seq-no
                 else
                    WRITE ZERO-RECORD2 FROM ws-cover-sheet
                    add 1              to ws-zero2-addr-seq-no
                    move ws-zero2-addr-seq-no
                                       to ws-dis-addr-seq-no
                 end-if
           end-evaluate
      
           MOVE WS-DIS-ADDR-SEQ-NO     TO WK-ADDR (1)
           MOVE CO-CONTROL-NAME        TO WK-ADDR(2)
           MOVE CO-ACCT-NAME           TO WK-ADDR(3)
           MOVE CO-ADDR-1              TO WK-ADDR(4)
           MOVE CO-ADDR-2              TO WK-ADDR(5)
           MOVE SPACES                 TO WK-ADDR(6)
           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
              DELIMITED BY '  ' INTO WK-ADDR (6)
           END-STRING
           IF CO-ZIP-PLUS4 = SPACE
              MOVE CO-ZIP-PRIME        TO WK-ADDR(6)(26:5)
           ELSE
              MOVE CO-ZIP              TO WK-ADDR(6)(22:9)
           END-IF
      
           IF WK-ADDR(2) = WK-ADDR(3)
              MOVE SPACES TO WK-ADDR(3)                                 
           END-IF                                                       
      
           PERFORM 2 TIMES
      
           IF WK-ADDR(1) = SPACES                                       
              MOVE WK-ADDR(2) TO WK-ADDR(1)                             
              MOVE WK-ADDR(3) TO WK-ADDR(2)                             
              MOVE WK-ADDR(4) TO WK-ADDR(3)                             
              MOVE WK-ADDR(5) TO WK-ADDR(4)                             
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
                                                                        
           IF WK-ADDR(2) = SPACES                                       
              MOVE WK-ADDR(3) TO WK-ADDR(2)                             
              MOVE WK-ADDR(4) TO WK-ADDR(3)                             
              MOVE WK-ADDR(5) TO WK-ADDR(4)                             
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
                                                                        
           IF WK-ADDR(3) = SPACES                                       
              MOVE WK-ADDR(4) TO WK-ADDR(3)                             
              MOVE WK-ADDR(5) TO WK-ADDR(4)                             
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
                                                                        
           IF WK-ADDR(4) = SPACES                                       
              MOVE WK-ADDR(5) TO WK-ADDR(4)                             
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
                                                                        
           IF WK-ADDR(5) = SPACES                                       
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
      
           END-PERFORM
      
           MOVE SPACES                 TO WS-COVER-SHEET
           PERFORM VARYING A1 FROM 1 BY 1 UNTIL A1 > 7
              MOVE WK-ADDR(A1)         TO PD-ADDRESS                           
              evaluate true
                 when type-of-statement = 'REFUND'
                    if co-carrier = '1' or '3' or '5'
                       write refund-record1 from ws-cover-sheet
                    else
                       write refund-record2 from ws-cover-sheet
                    end-if
                 when type-of-statement = 'ZERO'
                    if co-carrier = '1' or '3' or '5'
                       WRITE ZERO-RECORD1 FROM ws-cover-sheet
                    else
                       WRITE ZERO-RECORD2 FROM ws-cover-sheet
                    end-if
              end-evaluate
           END-PERFORM                                                  

102512     move spaces                 to ws-cover-sheet
102512     PERFORM VARYING A1 FROM 1 BY 1 UNTIL A1 > 8
102512        evaluate true
102512           when type-of-statement = 'REFUND'
102512              if co-carrier = '1' or '3' or '5'
102512                 move wso111-special-notes (a1)
102512                                 to pd-spec-notes
102512                 write refund-record1 from ws-cover-sheet
102512              else
102512                 move wso112-special-notes (a1)
102512                                 to pd-spec-notes
102512                 write refund-record2 from ws-cover-sheet
102512              end-if
102512           when type-of-statement = 'ZERO'
102512              if co-carrier = '1' or '3' or '5'
102512                 move wso121-special-notes (a1)
102512                                 to pd-spec-notes
102512                 WRITE ZERO-RECORD1 FROM ws-cover-sheet
102512              else
102512                 move wso122-special-notes (a1)
102512                                 to pd-spec-notes
102512                 WRITE ZERO-RECORD2 FROM ws-cover-sheet
102512              end-if
102512        end-evaluate
102512     END-PERFORM                                                  
102512
102512     move spaces                 to ws-cover-sheet
102512     evaluate true
102512        when type-of-statement = 'REFUND'
102512           if co-carrier = '1' or '3' or '5'
102512              move wso111-comment-1
102512                                 to pd-spec-notes
102512              write refund-record1 from ws-cover-sheet
102512              move wso111-comment-2
102512                                 to pd-spec-notes
102512              write refund-record1 from ws-cover-sheet
102512           else
102512              move wso112-comment-1
102512                                 to pd-spec-notes
102512              write refund-record2 from ws-cover-sheet
102512              move wso112-comment-2
102512                                 to pd-spec-notes
102512              write refund-record2 from ws-cover-sheet
102512           end-if
102512        when type-of-statement = 'ZERO'
102512           if co-carrier = '1' or '3' or '5'
102512              move wso121-comment-1
102512                                 to pd-spec-notes
102512              WRITE ZERO-RECORD1 FROM ws-cover-sheet
102512              move wso121-comment-2
102512                                 to pd-spec-notes
102512              WRITE ZERO-RECORD1 FROM ws-cover-sheet
102512           else
102512              move wso122-comment-1
102512                                 to pd-spec-notes
102512              WRITE ZERO-RECORD2 FROM ws-cover-sheet
102512              move wso122-comment-2
102512                                 to pd-spec-notes
102512              WRITE ZERO-RECORD2 FROM ws-cover-sheet
102512           end-if
102512     end-evaluate

           .                                                            
       2500-EXIT.                                                       
           EXIT.                                                        
      
       2310-ADJ-ADDRESS.                                                
      
           MOVE WS-DIS-ADDR-SEQ-NO     TO WK-ADDR (1)
062513*    MOVE CO-CONTROL-NAME TO WK-ADDR(2)                           
062513     MOVE CO-MAIL-NAME    TO WK-ADDR(2)                           
062513     MOVE CO-ACCT-NAME    TO WK-ADDR(3)                           
           IF CO-MAIL-NAME(1:3) = 'C/O'                                 
062513        MOVE CO-ACCT-NAME TO WK-ADDR(2)                           
062513        MOVE CO-MAIL-NAME TO WK-ADDR(3)                           
           END-IF                                                       
062513     MOVE CO-ADDR-1 TO WK-ADDR(4)                                 
062513     MOVE CO-ADDR-2 TO WK-ADDR(5)                                 
062513     MOVE SPACES    TO WK-ADDR(6)                                 
           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
062513        DELIMITED BY '  ' INTO WK-ADDR (6)
           END-STRING
           IF CO-ZIP-PLUS4 = SPACE                                      
062513        MOVE CO-ZIP-PRIME TO WK-ADDR(6)(26:5)                     
           ELSE                                                         
062513        MOVE CO-ZIP TO WK-ADDR(6)(22:9)                           
           END-IF                                                       
                                                                        
062513     IF WK-ADDR(2) = WK-ADDR(3)                                   
062513        MOVE SPACES TO WK-ADDR(3)                                 
           END-IF                                                       
                                                                        
           PERFORM 2 TIMES
      
           IF WK-ADDR(1) = SPACES                                       
              MOVE WK-ADDR(2) TO WK-ADDR(1)                             
              MOVE WK-ADDR(3) TO WK-ADDR(2)                             
              MOVE WK-ADDR(4) TO WK-ADDR(3)                             
              MOVE WK-ADDR(5) TO WK-ADDR(4)                             
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
                                                                        
           IF WK-ADDR(2) = SPACES                                       
              MOVE WK-ADDR(3) TO WK-ADDR(2)                             
              MOVE WK-ADDR(4) TO WK-ADDR(3)                             
              MOVE WK-ADDR(5) TO WK-ADDR(4)                             
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
                                                                        
           IF WK-ADDR(3) = SPACES                                       
              MOVE WK-ADDR(4) TO WK-ADDR(3)                             
              MOVE WK-ADDR(5) TO WK-ADDR(4)                             
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
                                                                        
           IF WK-ADDR(4) = SPACES                                       
              MOVE WK-ADDR(5) TO WK-ADDR(4)                             
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
                                                                        
           IF WK-ADDR(5) = SPACES                                       
              MOVE WK-ADDR(6) TO WK-ADDR(5)                             
              MOVE WK-ADDR(7) TO WK-ADDR(6)
              MOVE SPACES     TO WK-ADDR(7)
           END-IF                                                       
      
           END-PERFORM
      
           .                                                            
       2310-EXIT.                                                       
           EXIT.                                                        
      
       2400-PRINT-VOUCHER.                                              

           IF CO-CONTROL-PRIMARY = PREV-CO-CONTROL                      
              GO TO 2400-EXIT.                                          
                                                                        
           MOVE '1' TO VR-CC                                            
           MOVE 'VOUCHER' TO VR-ID

      ****  The following tells the formdef (DCC011)
      ****  what voucher to use
      ****  Carrier 1 and 2 (LPAC)  O1DCV111
      ****  Carrier 3 and 4 (CSI)   O1DCV113
      ****  Carrier 5 and 6 (CCC)   O1DCV115

           IF CO-CARRIER = '1' OR '2'
              MOVE '1'                 TO VR-CARRIER
           ELSE
              IF CO-CARRIER = '3' OR '4'
                 MOVE '3'              TO VR-CARRIER
              ELSE
                 MOVE '5'              TO VR-CARRIER
              END-IF
           END-IF
      
           MOVE +0 TO STRT                                              
           INSPECT ALPH-DATE TALLYING STRT FOR LEADING ' '              
           MOVE ALPH-DATE(STRT + 1:) TO VR-DATE1                        
           MOVE RUN-DATE TO WS-DATE                                     
           STRING WS-DATE(5:2) '/' WS-DATE(7:2) '/' WS-DATE(3:2)        
              DELIMITED BY SIZE INTO VR-DATE2                       
      
           MOVE CO-CARR-GROUP TO VR-CARR                                
           MOVE CO-ACCOUNT TO VR-ACCT                                   
           MOVE CO-CURRENT-END-BAL TO VR-AMT
           MOVE CO-CSR-CODE        TO VR-CSR

           if co-carrier = '1' or '3' or '5'
              write refund-record1 from ws-voucher-record
           else
              write refund-record2 from ws-voucher-record
           end-if
                                                                        
           MOVE ' ' TO VR-CC                                            
      *    ADD 1                       TO WS-VOC-ADDR-SEQ-NO
           MOVE WS-VOC-ADDR-SEQ-NO     TO WS-DIS-ADDR-SEQ-NO
           PERFORM 2310-ADJ-ADDRESS THRU 2310-EXIT                      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              MOVE WK-ADDR(SUB) TO VR-ADDRESS                           
              if co-carrier = '1' or '3' or '5'
                 write refund-record1 from ws-voucher-record
              else
                 write refund-record2 from ws-voucher-record
              end-if
           END-PERFORM                                                  

           .                                                            
       2400-EXIT.                                                       
           EXIT.                                                        
      
       8500-DATE-CONVERT.
      
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA
      
           .
       8500-EXIT.
           EXIT.
      
       9000-END-OF-JOB.                                                 
      
           DISPLAY 'EOJ - BILLING STATEMENT PRINT JOB'                  
                                                                        
           CLOSE BILLING-STATEMENTS
                 ERCOMP
                 REFUND-STATEMENTS1 refund-statements2
                 ZERO-STATEMENTS1 zero-statements2
                 KEYS-EXT
      
           .
       9000-EXIT.                                                       
           EXIT.                                                        
       ABEND-PGM.
                           COPY ELCABEND.
