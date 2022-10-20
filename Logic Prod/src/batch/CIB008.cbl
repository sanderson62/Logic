       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. CIB008.                                              
                                                                        
      ***************************************************************** 
      *                                                               * 
      *  THIS PROGRAM READS THE COMPENSATION MASTER AND CREATES:      * 
      *                                                               * 
      *  1)  A FILE OF FREEDOM ACCOUNTS PAYABLE TRANSACTIONS          * 
      *  2)  A FILE OF LOGIC PAYMENT TRANSACTIONS                     * 
      *  3)  REPORT OF VOUCHERS LESS THAN $5000                       * 
      *  4)  REPORT OF VOUCHERS GREATER THAN OR EQUAL TO $5000        * 
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
      * 052303    2001061800003  SMVA  FIX - FIRST CHAR GETTING CUT OFF
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
      ****************************************************************** 
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT ERCOMP                                                
               ASSIGN TO ERCOMP                                         
               ORGANIZATION IS INDEXED                                  
               ACCESS IS SEQUENTIAL                                     
               RECORD KEY IS CO-CONTROL-PRIMARY                         
               FILE STATUS IS ERCOMP-STATUS.                            
                                                                        
           SELECT DISK-DATE                                             
               ASSIGN TO SYS019                                         
               FILE STATUS IS SYS019-STATUS.                            
                                                                        
           SELECT AP-TRANS       ASSIGN TO SYS010.                      
                                                                        
           SELECT LOGIC-TRANS    ASSIGN TO SYS011.                      
                                                                        
           SELECT REPORT-1       ASSIGN TO SYS012.                      
                                                                        
           SELECT REPORT-2       ASSIGN TO SYS013.                      
                                                                        
                                                                        
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  ERCOMP.                                                      
           COPY ERCCOMP.                                                
                                                                        
       FD  DISK-DATE                                                    
           COPY ELCDTEFD.                                               
                                                                        
       FD  AP-TRANS                                                     
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  AP-RECORD         PIC X(400).                                
                                                                        
       FD  LOGIC-TRANS                                                  
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  LOGIC-RECORD      PIC X(200).                                
                                                                        
       FD  REPORT-1                                                     
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  REPORT-REC1       PIC X(80).                                 
                                                                        
       FD  REPORT-2                                                     
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  REPORT-REC2       PIC X(80).                                 
                                                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  BINARY.                                                      
           05  SUB             PIC S9(4)      VALUE +0.                 
           05  MAX-LINE        PIC S9(4)      VALUE +0.                 
                                                                        
       01  PACKED-DECIMAL.                                              
           05  LINE-CNT1       PIC S9(3)      VALUE +0.                 
           05  LINE-CNT2       PIC S9(3)      VALUE +0.                 
           05  TOT1-CNT        PIC S9(5)      VALUE +0.                 
           05  TOT1-AMT        PIC S9(9)V99   VALUE +0.                 
           05  TOT2-CNT        PIC S9(5)      VALUE +0.                 
           05  TOT2-AMT        PIC S9(9)V99   VALUE +0.                 
           05  WS-PAYMENT-AMT  PIC S9(7)V99   VALUE +0.                 
                                                                        
       01  FILLER.                                                      
           05  DUMP                PIC X      VALUE SPACE.              
           05  FORCE-DUMP REDEFINES DUMP PIC S9 COMP-3.                 
           05  SYS019-STATUS       PIC XX     VALUE '00'.               
           05  ERCOMP-STATUS       PIC XX     VALUE '00'.               
               88  EOF                        VALUE '10'.               
           05  BLANK-LINE          PIC X      VALUE SPACE.              
           05  WS-GROUP-CODE       PIC 9(4)   VALUE ZERO.               
           05  WS-INV.                                                  
               10  WS-INV-MO       PIC X      VALUE SPACE.              
               10  WS-INV-YR       PIC XX     VALUE SPACE.              
               10  WS-INV-CARR     PIC X      VALUE SPACE.              
               10  WS-INV-ACCT     PIC X(8)   JUSTIFIED RIGHT.          
           05  WS-SUSP.                                                 
               10  WS-SUSP-CARR    PIC X      VALUE SPACE.              
               10  WS-SUSP-RESP    PIC X(7)   JUSTIFIED RIGHT.          
               10  WS-SUSP-PYMTLVL PIC X(8)   JUSTIFIED RIGHT.          
           05  WS-EXP-ACCT                                              
                  PIC X(24) VALUE '110812010000000000000000'.           
           05  WS-PAY-ACCT                                              
                  PIC X(24) VALUE '182501130002000000000000'.           
           05  WS-ACTV-DATE        PIC 9(6).                            
           05  WS-SELECT-DT        PIC X(2).                            
           05  WS-TIME-HMS         PIC 9(8).                            
           05  REDEFINES WS-TIME-HMS.                                   
               10  WS-TIME-HHMMS   PIC 9(5).                            
               10  FILLER          PIC X(3).                            
           05  REDEFINES WS-TIME-HMS.                                   
               10  WS-TIME-HHMMSS  PIC 9(6).                            
               10  FILLER          PIC X(2).                            
                                                                        
           EJECT                                                        
      ** FREEDOM A/P RECORD                                             
       COPY FNC006.                                                     
                                                                        
           EJECT                                                        
      ** LOGIC PAYMENT RECORD                                           
       COPY ERCPYAJ.                                                    
                                                                        
           EJECT                                                        
      ** LOGIC DATE CONVERSION                                          
       COPY ELCDATE.                                                    
                                                                        
           EJECT                                                        
      ** LOGIC DATE CARD                                                
       COPY ELCDTECX.                                                   
                                                                        
           EJECT                                                        
       01  HDG-1A PIC X(40) VALUE                                       
                  'CID BILLING VOUCHERS LESS THAN $5000'.               
       01  HDG-1B PIC X(40) VALUE                                       
                  'CID BILLING VOUCHERS $5000 OR GREATER'.              
       01  HDG-1.                                                       
052303     05  FILLER          PIC X(01)  VALUE SPACE.
           05  REPORT-TITLE    PIC X(40)  VALUE SPACES.                 
           05  FILLER          PIC X(9)   VALUE SPACES.                 
           05  REPORT-DATE     PIC X(18)  VALUE SPACES.                 
       01  HDG-2.                                                       
052303     05  FILLER          PIC X(01)  VALUE SPACE.
           05  PIC X(17) VALUE 'ACCOUNT          '.                     
           05  PIC X(03) VALUE SPACES.                                  
           05  PIC X(30) VALUE 'NAME                          '.        
           05  PIC X(03) VALUE SPACES.                                  
           05  PIC X(14) VALUE '        AMOUNT'.                        
       01  HDG-3.                                                       
052303     05  FILLER          PIC X(01)  VALUE SPACE.
           05  PIC X(17) VALUE '-----------------'.                     
           05  PIC X(03) VALUE SPACES.                                  
           05  PIC X(30) VALUE '------------------------------'.        
           05  PIC X(03) VALUE SPACES.                                  
           05  PIC X(14) VALUE '--------------'.                        
       01  REPORT-DETAIL.                                               
052303     05  FILLER            PIC X(01)   VALUE SPACE.
           05  REPORT-ACCOUNT.                                          
               10  REPORT-CARR   PIC X(07)   VALUE SPACE.               
               10  REPORT-ACCT   PIC X(10)   VALUE SPACE.               
           05  FILLER            PIC X(03)   VALUE SPACE.               
           05  REPORT-NAME       PIC X(30)   VALUE SPACE.               
           05  FILLER            PIC X(03)   VALUE SPACE.               
           05  REPORT-AMOUNT     PIC ZZZ,ZZZ,ZZZ.99.                    
       01  REPORT-TOTAL.                                                
052303     05  FILLER            PIC X(01)   VALUE SPACE.
           05  FILLER            PIC X(6)    VALUE 'COUNT:'.            
           05  REPORT-TOT-CNT    PIC ZZ,ZZ9.                            
           05  FILLER            PIC X(31)   VALUE SPACE.               
           05  FILLER            PIC X(10)   VALUE 'TOTAL $   '.        
           05  REPORT-TOT-AMT    PIC ZZZ,ZZZ,ZZZ.99.                    
                                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       PROCEDURE DIVISION.                                              
      *                                                                 
           PERFORM 000-INITIALIZE THRU 000-EXIT                         
                                                                        
           PERFORM 100-MAINLINE THRU 100-EXIT                           
             UNTIL EOF                                                  
                                                                        
           PERFORM 900-PRINT-TOTALS THRU 900-EXIT                       
                                                                        
           STOP RUN.                                                    
                                                                        
                                                                        
                                                                        
      *                                                                 
       000-INITIALIZE.                                                  
      *                                                                 
           OPEN INPUT DISK-DATE                                         
           IF SYS019-STATUS = '00'                                      
              READ DISK-DATE INTO DATE-CARD                             
              CLOSE DISK-DATE                                           
           ELSE                                                         
              DISPLAY 'OPEN ERROR ' SYS019-STATUS ' ON SYS019'          
              ADD +1 TO FORCE-DUMP                                      
           END-IF                                                       
                                                                        
           MOVE ALPH-DATE TO REPORT-DATE                                
           MOVE RUN-DATE  TO WS-ACTV-DATE                               
           MOVE RUN-DATE  TO DC-GREG-DATE-1-YMD                         
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA                    
           MOVE DC-BIN-DATE-1 TO WS-SELECT-DT                           
                                                                        
           OPEN INPUT ERCOMP                                            
           IF ERCOMP-STATUS NOT = '00'                                  
              DISPLAY 'OPEN ERROR ' ERCOMP-STATUS ' ON ERCOMP'          
              ADD +1 TO FORCE-DUMP.                                     
                                                                        
           OPEN OUTPUT AP-TRANS                                         
                       LOGIC-TRANS                                      
                       REPORT-1                                         
                       REPORT-2.                                        
                                                                        
           PERFORM 410-HEADING-1 THRU 410-EXIT                          
           PERFORM 510-HEADING-2 THRU 510-EXIT                          
           .                                                            
       000-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
      *                                                                 
       100-MAINLINE.                                                    
      *                                                                 
           READ ERCOMP                                                  
           IF ERCOMP-STATUS = '10'                                      
              GO TO 100-EXIT.                                           
                                                                        
           IF ( CO-TYPE = 'A' )                                         
              AND ( CO-CURRENT-END-BAL IS NEGATIVE )                    
              AND ( CO-BILL-SW = 'B' )                                  
                 CONTINUE                                               
           ELSE                                                         
              GO TO 100-EXIT.                                           
                                                                        
           MULTIPLY CO-CURRENT-END-BAL BY -1                            
             GIVING WS-PAYMENT-AMT                                      
                                                                        
           PERFORM 200-WRITE-AP-TRANS THRU 200-EXIT                     
           PERFORM 300-WRITE-LOGIC-TRANS THRU 300-EXIT                  
           IF WS-PAYMENT-AMT < 5000.00                                  
              PERFORM 400-REPORT-1 THRU 400-EXIT                        
           ELSE                                                         
              PERFORM 500-REPORT-2 THRU 500-EXIT                        
           END-IF                                                       
           .                                                            
       100-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
      *                                                                 
       200-WRITE-AP-TRANS.                                              
      *                                                                 
           PERFORM 210-WRITE-TRANS-REC   THRU 210-EXIT                  
           PERFORM 220-WRITE-EXPENSE-REC THRU 220-EXIT                  
           PERFORM 230-WRITE-PAYABLE-REC THRU 230-EXIT                  
           PERFORM 240-WRITE-CHECK-REC   THRU 240-EXIT                  
           PERFORM 250-WRITE-ADDR-REC    THRU 250-EXIT                  
           .                                                            
       200-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       210-WRITE-TRANS-REC.                                             
      *                                                                 
           ADD 1 TO WS-GROUP-CODE                                       
           MOVE WS-GROUP-CODE TO VR-GROUP-CODE                          
           MOVE 'PSEUDO'      TO VR-VENDOR-ID                           
           MOVE SPACES        TO VR-DATA                                
           MOVE 'T'           TO VR-RECORD-ID                           
           MOVE 'F'           TO VTR-CHECK-TYPE                         
           MOVE 'R'           TO VTR-TRANS-TYPE                         
           EVALUATE CO-ACT-MONTH                                        
               WHEN '01'  MOVE 'A' TO WS-INV-MO                         
               WHEN '02'  MOVE 'B' TO WS-INV-MO                         
               WHEN '03'  MOVE 'C' TO WS-INV-MO                         
               WHEN '04'  MOVE 'D' TO WS-INV-MO                         
               WHEN '05'  MOVE 'E' TO WS-INV-MO                         
               WHEN '06'  MOVE 'F' TO WS-INV-MO                         
               WHEN '07'  MOVE 'G' TO WS-INV-MO                         
               WHEN '08'  MOVE 'H' TO WS-INV-MO                         
               WHEN '09'  MOVE 'I' TO WS-INV-MO                         
               WHEN '10'  MOVE 'J' TO WS-INV-MO                         
               WHEN '11'  MOVE 'K' TO WS-INV-MO                         
               WHEN OTHER MOVE 'L' TO WS-INV-MO                         
           END-EVALUATE                                                 
           MOVE CO-ACT-YEAR    TO WS-INV-YR                             
           MOVE CO-CARRIER     TO WS-INV-CARR                           
           IF CO-TYPE = 'A'                                             
              MOVE CO-ACCOUNT  TO WS-INV-ACCT                           
           ELSE                                                         
              MOVE CO-RESP-NO  TO WS-INV-ACCT                           
           END-IF                                                       
           MOVE WS-INV         TO VTR-INVOICE-NO                        
           STRING CO-ACT-MONTH CO-ACT-DAY '20' CO-ACT-YEAR              
                  DELIMITED BY SIZE INTO VTR-INVOICE-DATE               
           MOVE 'IMM'          TO VTR-TERMS                             
           WRITE AP-RECORD FROM VOUCHER-RECORD                          
           .                                                            
       210-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       220-WRITE-EXPENSE-REC.                                           
      *                                                                 
           MOVE SPACES      TO VR-DATA                                  
           MOVE 'D'         TO VR-RECORD-ID                             
           MOVE WS-INV      TO VDR-INVOICE-NO                           
           MOVE 001         TO VDR-SEQ-NO                               
           MOVE 'E'         TO VDR-DISTR-TYPE (1)                       
           MOVE WS-EXP-ACCT TO VDR-ACCT-NO (1)                          
           MOVE 'AP'        TO VDR-SOURCE (1)                           
           MOVE CO-CARRIER  TO WS-SUSP-CARR                             
           MOVE CO-RESP-NO  TO WS-SUSP-RESP                             
           MOVE CO-ACCOUNT  TO WS-SUSP-PYMTLVL                          
           MOVE WS-SUSP     TO VDR-SUSPENSE (1)                         
           MOVE WS-PAYMENT-AMT TO VDR-INVOICE-AMT (1)                   
           MOVE 'CID LOGIC 562 PMT' TO VDR-DESCR (1)                    
           WRITE AP-RECORD FROM VOUCHER-RECORD                          
           .                                                            
       220-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       230-WRITE-PAYABLE-REC.                                           
      *                                                                 
           MOVE SPACES      TO VR-DATA                                  
           MOVE 'D'         TO VR-RECORD-ID                             
           MOVE WS-INV      TO VDR-INVOICE-NO                           
           MOVE 002         TO VDR-SEQ-NO                               
           MOVE 'P'         TO VDR-DISTR-TYPE (1)                       
           MOVE WS-PAY-ACCT TO VDR-ACCT-NO (1)                          
           MOVE 'AP'        TO VDR-SOURCE (1)                           
           MOVE WS-SUSP     TO VDR-SUSPENSE (1)                         
           MOVE WS-PAYMENT-AMT TO VDR-INVOICE-AMT (1)                   
           MOVE 'CID LOGIC 562 PMT' TO VDR-DESCR (1)                    
           WRITE AP-RECORD FROM VOUCHER-RECORD                          
           .                                                            
       230-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       240-WRITE-CHECK-REC.                                             
      *                                                                 
           MOVE SPACES TO VR-DATA                                       
           MOVE 'F'    TO VR-RECORD-ID                                  
           MOVE WS-INV TO VCR-INVOICE-NO                                
           MOVE 001    TO VCR-SEQ-NO                                    
           MOVE 'CID LOGIC 562 PMT' TO VCR-DESCR(1)                     
           MOVE WS-PAYMENT-AMT      TO VCR-CHECK-AMT(1)                 
           WRITE AP-RECORD FROM VOUCHER-RECORD                          
           .                                                            
       240-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       250-WRITE-ADDR-REC.                                              
      *                                                                 
           MOVE SPACES    TO VR-DATA                                    
           MOVE 'A'       TO VR-RECORD-ID                               
           MOVE WS-INV    TO VPR-INVOICE-NO                             
           MOVE CO-ACCT-NAME TO VPR-NAME                                
           MOVE CO-ADDR-1 TO VPR-ADDRESS (1)                            
           MOVE CO-ADDR-2 TO VPR-ADDRESS (2)                            
051810     MOVE SPACES    TO VPR-ADDRESS (3)                            
051810     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810        DELIMITED BY '  ' INTO VPR-ADDRESS (3)
051810     END-STRING
           WRITE AP-RECORD FROM VOUCHER-RECORD                          
           .                                                            
       250-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       300-WRITE-LOGIC-TRANS.                                           
      *                                                                 
           MOVE SPACES  TO  PENDING-PAY-ADJ                             
                                                                        
           ACCEPT WS-TIME-HMS FROM TIME                                 
           ACCEPT DC-GREG-DATE-1-YMD FROM DATE                          
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA                    
           COMPUTE PY-FILE-SEQ-NO                                       
                 = (DC-JULIAN-DAYS * 100000) + WS-TIME-HHMMS.           
                                                                        
           MOVE 'PY'            TO  PY-RECORD-ID                        
           MOVE CO-COMPANY-CD   TO  PY-COMPANY-CD                       
           MOVE CO-CARRIER      TO  PY-CARRIER                          
           MOVE CO-GROUPING     TO  PY-GROUPING                         
           MOVE CO-RESP-NO      TO  PY-FIN-RESP                         
           MOVE CO-ACCOUNT      TO  PY-ACCOUNT                          
           MOVE 'R'             TO  PY-RECORD-TYPE                      
           MOVE DC-BIN-DATE-1   TO  PY-INPUT-DT                         
           MOVE DC-BIN-DATE-1   TO  PY-LAST-MAINT-DT                    
           MOVE 'STMT'          TO  PY-LAST-MAINT-BY                    
           MOVE WS-TIME-HHMMSS  TO  PY-LAST-MAINT-HHMMSS                
           MOVE WS-PAYMENT-AMT  TO  PY-ENTRY-AMT                        
           MOVE 'STATEMENT'     TO  PY-ENTRY-COMMENT                    
           MOVE WS-SELECT-DT    TO  PY-CREDIT-SELECT-DT                 
           MOVE +0              TO  PY-CHECK-QUE-CONTROL                
           MOVE +0              TO  PY-CHECK-QUE-SEQUENCE               
           MOVE LOW-VALUES      TO  PY-CREDIT-ACCEPT-DT                 
                                    PY-BILLED-DATE                      
                                    PY-REPORTED-DT                      
                                    PY-CHECK-WRITTEN-DT                 
                                    PY-AR-DATE                          
                                                                        
           WRITE LOGIC-RECORD FROM PENDING-PAY-ADJ                      
           .                                                            
       300-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       400-REPORT-1.                                                    
      *                                                                 
           MOVE CO-CARR-GROUP TO REPORT-CARR                            
           IF CO-TYPE = 'A'                                             
              MOVE CO-ACCOUNT TO REPORT-ACCT                            
           ELSE                                                         
              MOVE CO-RESP-NO TO REPORT-ACCT                            
           END-IF                                                       
                                                                        
           MOVE CO-ACCT-NAME   TO REPORT-NAME                           
           MOVE WS-PAYMENT-AMT TO REPORT-AMOUNT                         
           ADD +1  TO TOT1-CNT                                          
           ADD WS-PAYMENT-AMT TO TOT1-AMT                               
           WRITE REPORT-REC1 FROM REPORT-DETAIL                         
           ADD +1 TO LINE-CNT1                                          
           IF LINE-CNT1 > +75                                           
              PERFORM 410-HEADING-1 THRU 410-EXIT                       
           END-IF                                                       
           .                                                            
       400-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       410-HEADING-1.                                                   
      *                                                                 
           MOVE HDG-1A TO REPORT-TITLE                                  
           WRITE REPORT-REC1 FROM HDG-1 AFTER ADVANCING PAGE            
           WRITE REPORT-REC1 FROM HDG-2 AFTER ADVANCING 2 LINES         
           WRITE REPORT-REC1 FROM HDG-3 AFTER ADVANCING 1 LINE          
           WRITE REPORT-REC1 FROM BLANK-LINE AFTER ADVANCING 2 LINES    
           MOVE +6 TO LINE-CNT1                                         
           .                                                            
       410-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       500-REPORT-2.                                                    
      *                                                                 
           MOVE CO-CARR-GROUP TO REPORT-CARR                            
           IF CO-TYPE = 'A'                                             
              MOVE CO-ACCOUNT TO REPORT-ACCT                            
           ELSE                                                         
              MOVE CO-RESP-NO TO REPORT-ACCT                            
           END-IF                                                       
                                                                        
           MOVE CO-ACCT-NAME   TO REPORT-NAME                           
           MOVE WS-PAYMENT-AMT TO REPORT-AMOUNT                         
           ADD +1  TO TOT2-CNT                                          
           ADD WS-PAYMENT-AMT TO TOT2-AMT                               
           WRITE REPORT-REC2 FROM REPORT-DETAIL                         
           ADD +1 TO LINE-CNT2                                          
           IF LINE-CNT2 > +75                                           
              PERFORM 510-HEADING-2 THRU 510-EXIT                       
           END-IF                                                       
           .                                                            
       500-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       510-HEADING-2.                                                   
      *                                                                 
           MOVE HDG-1B TO REPORT-TITLE                                  
           WRITE REPORT-REC2 FROM HDG-1 AFTER ADVANCING PAGE            
           WRITE REPORT-REC2 FROM HDG-2 AFTER ADVANCING 2 LINES         
           WRITE REPORT-REC2 FROM HDG-3 AFTER ADVANCING 1 LINE          
           WRITE REPORT-REC2 FROM BLANK-LINE AFTER ADVANCING 2 LINES    
           MOVE +6 TO LINE-CNT2                                         
           .                                                            
       510-EXIT.                                                        
           EXIT.                                                        
                                                                        
                                                                        
           EJECT                                                        
      *                                                                 
       900-PRINT-TOTALS.                                                
      *                                                                 
           MOVE TOT1-CNT TO REPORT-TOT-CNT                              
           MOVE TOT1-AMT TO REPORT-TOT-AMT                              
           WRITE REPORT-REC1 FROM REPORT-TOTAL                          
                 AFTER ADVANCING 3 LINES                                
                                                                        
           MOVE TOT2-CNT TO REPORT-TOT-CNT                              
           MOVE TOT2-AMT TO REPORT-TOT-AMT                              
           WRITE REPORT-REC2 FROM REPORT-TOTAL                          
                 AFTER ADVANCING 3 LINES                                
           .                                                            
       900-EXIT.                                                        
           EXIT.                                                        
                                                                        
