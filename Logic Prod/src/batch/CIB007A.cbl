       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIB007A.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERACCT  ASSIGN TO ERACCTT                             
                  ORGANIZATION IS INDEXED                               
                  ACCESS IS DYNAMIC                                     
                  RECORD KEY IS AM-CONTROL-PRIMARY                      
                  FILE STATUS IS ERACCT-FILE-STATUS.                         
                                                                        
           SELECT BILLING-STATEMENTS                                    
                  ASSIGN TO SYS010                                      
                  FILE STATUS IS SYS010-STATUS.                         

           SELECT DISK-DATE                                             
                  ASSIGN TO SYS019.                                     
                                                                        
           SELECT A-STATES-STATEMENTS
                  ASSIGN TO SYS011.
                                                                        
           SELECT B-STATES-STATEMENTS
                  ASSIGN TO SYS012.

           SELECT C-STATES-STATEMENTS
                  ASSIGN TO SYS013.

       DATA DIVISION.                                                   

       FILE SECTION.                                                    
                                                                        
       FD  BILLING-STATEMENTS                                           
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  STMT-RECORD       PIC X(133).                                
                                                                        
       FD  ERACCT.                                                      
                                       COPY ERCACCT.
                                                                        
       FD  DISK-DATE                                                    
           COPY ELCDTEFD.                                               
                                                                        
       FD  A-STATES-STATEMENTS                                             
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  A-STATES-RECORD             PIC X(133).
                                                                        
       FD  B-STATES-STATEMENTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  B-STATES-RECORD             PIC X(133).

       FD  C-STATES-STATEMENTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  C-STATES-RECORD             PIC X(133).

       WORKING-STORAGE SECTION.                                         
       77  S1                          PIC S9(5) VALUE +0 COMP-3.
       77  WS-HOLD-AM-KEY              PIC X(19) VALUE SPACES.
       77  WS-STATE-SW                 PIC X     VALUE SPACES.
           88  VALID-STATE                       VALUE 'Y'.
       77  WS-INPUT-CNT                PIC 9(7)  VALUE ZEROS COMP-3.
       77  WS-OUTPUT-CNT               PIC 9(7)  VALUE ZEROS COMP-3.
       copy "ctypes.cpy".
                                                                        
       01  WS-TYPE-R-RECORD.
           05  WS-TYPE-R-TYPE          PIC X      VALUE ' '.
           05  WS-TYPE-R-END-BAL       PIC S9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-TYPE-R-SEQ-NO        PIC 9(9)   VALUE ZEROS.
           05  WS-TYPE-R-REC           PIC X(133) VALUE SPACES.

       01  WS-WK-ADD                   PIC X(30).

       01  VOUCHER-RECORDS.
           05  VOUCHER-REC OCCURS 7    PIC X(133).
       01  LETTER-RECORDS.
           05  LETTER-REC OCCURS 17    PIC X(133).
       01  STMT-RECORDS.
           05  STMT-REC OCCURS 8       PIC X(133).
       01  WS-STATE-TABLE.
           05  WS-STATES.
               10  FILLER              PIC XXX   VALUE 'AZA'.
               10  FILLER              PIC XXX   VALUE 'FLA'.
               10  FILLER              PIC XXX   VALUE 'GUA'.
               10  FILLER              PIC XXX   VALUE 'IDA'.
               10  FILLER              PIC XXX   VALUE 'MDA'.
               10  FILLER              PIC XXX   VALUE 'MIA'.
               10  FILLER              PIC XXX   VALUE 'MTA'.
               10  FILLER              PIC XXX   VALUE 'OHA'.
               10  FILLER              PIC XXX   VALUE 'SDA'.
               10  FILLER              PIC XXX   VALUE 'WYA'.
               10  FILLER              PIC XXX   VALUE 'HIA'.
               10  FILLER              PIC XXX   VALUE 'ILA'.
               10  FILLER              PIC XXX   VALUE 'KYA'.
               10  FILLER              PIC XXX   VALUE 'MSA'.
               10  FILLER              PIC XXX   VALUE 'NCA'.
               10  FILLER              PIC XXX   VALUE 'SCA'.
               10  FILLER              PIC XXX   VALUE 'VTA'.
               10  FILLER              PIC XXX   VALUE 'CAA'.
               10  FILLER              PIC XXX   VALUE 'DEB'.
               10  FILLER              PIC XXX   VALUE 'IAB'.
               10  FILLER              PIC XXX   VALUE 'OKB'.
               10  FILLER              PIC XXX   VALUE 'ORB'.
               10  FILLER              PIC XXX   VALUE 'TNB'.
               10  FILLER              PIC XXX   VALUE 'TXB'.
               10  FILLER              PIC XXX   VALUE 'UTB'.
               10  FILLER              PIC XXX   VALUE 'WAB'.
               10  FILLER              PIC XXX   VALUE 'WVB'.
               10  FILLER              PIC XXX   VALUE 'ALB'.
               10  FILLER              PIC XXX   VALUE 'GAB'.
               10  FILLER              PIC XXX   VALUE 'INB'.
               10  FILLER              PIC XXX   VALUE 'KSB'.
               10  FILLER              PIC XXX   VALUE 'LAB'.
               10  FILLER              PIC XXX   VALUE 'MEB'.
               10  FILLER              PIC XXX   VALUE 'MNB'.
               10  FILLER              PIC XXX   VALUE 'NDB'.
               10  FILLER              PIC XXX   VALUE 'CTC'.
               10  FILLER              PIC XXX   VALUE 'MAC'.
               10  FILLER              PIC XXX   VALUE 'MOC'.
               10  FILLER              PIC XXX   VALUE 'NEC'.
               10  FILLER              PIC XXX   VALUE 'NHC'.
               10  FILLER              PIC XXX   VALUE 'NJC'.
               10  FILLER              PIC XXX   VALUE 'PAC'.
               10  FILLER              PIC XXX   VALUE 'COC'.
               10  FILLER              PIC XXX   VALUE 'NMC'.
               10  FILLER              PIC XXX   VALUE 'RIC'.
               10  FILLER              PIC XXX   VALUE 'VAC'.
               10  FILLER              PIC XXX   VALUE 'WIC'.
               10  FILLER              PIC XXX   VALUE 'ARC'.
           05  FILLER REDEFINES WS-STATES OCCURS 48.
               10  WS-STATE            PIC XX.
               10  WS-REPORT           PIC X.
       01  FILLER.
           05  WS-SUMMARY-SW           PIC X   VALUE SPACES.
               88  LAST-PAGE-SUMMARY          VALUE 'Y'.
               88  LAST-PAGE-DETAIL           VALUE 'D'.
           05  WS-WORK-DATE            PIC 9(11)  VALUE ZEROS.
           05  WS-WORK-DATER REDEFINES WS-WORK-DATE.
               10  FILLER              PIC XXX.
               10  WS-SCAN-CCYYMM      PIC X(6).
               10  FILLER              PIC XX.
092903     05  WS-RETURN-CODE          PIC S9(4)  COMP  VALUE ZEROS.    
092903     05  WS-ZERO                 PIC S9     VALUE +0  COMP-3.
092903     05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         
092903     05  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZEROS.          
092903     05  PGM-SUB             PIC S999   COMP-3  VALUE +061.   
           05  S0C7                PIC X       VALUE SPACE.             
           05  FORCE-DUMP REDEFINES S0C7 PIC S9 COMP-3.                 
           05  SYS010-STATUS       PIC X(2)    VALUE SPACE.             
               88  EOF                         VALUE '10'.              
           05  WS-INPUT-SW             PIC X   VALUE ' '.
               88  END-OF-INPUT                VALUE 'Y'.
           05  ERACCT-FILE-STATUS      PIC XX  VALUE ZEROS.
           05  WS-DATE             PIC 9(8)    VALUE ZERO.              
           05  WS-HDG  OCCURS 4 TIMES PIC X(133).                       
DAN01      05  WK-ADDR OCCURS 6 TIMES PIC X(30).                        
           05  WK-AMT              PIC ZZZ,ZZZ,ZZZ.99/.                 
                                                                        
       01  FILLER               COMP-3.                                 
           05  STRT             PIC S9(3)   VALUE +0.                   
           05  SUB              PIC S9(3)   VALUE +0.                   
           05  WK1              PIC S9(7)   VALUE +0.                   
           05  WK2              PIC S9(7)   VALUE +0.                   
                                                                        
                                                                        
092903                                 COPY ELCDTECX.

092903                                 COPY ELCDTEVR.
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
092903                                 COPY ELCDTERX.

           PERFORM 0000-INIT-ROUTINE THRU 0000-EXIT                     
           PERFORM 0100-PROCESS-FILE THRU 0100-EXIT UNTIL
              END-OF-INPUT
           PERFORM 9000-END-OF-JOB   THRU 9000-EXIT                     
           GOBACK

           .
       0000-INIT-ROUTINE.                                               

           OPEN INPUT BILLING-STATEMENTS                                
           IF SYS010-STATUS NOT = '00'                                  
              DISPLAY 'OPEN ERROR ' SYS010-STATUS ' ON SYS010'          
              MOVE +16 TO RETURN-CODE                                   
              ADD +1 TO FORCE-DUMP.                                     
                                                                        
           OPEN INPUT ERACCT                                            
           IF ERACCT-FILE-STATUS NOT = '00'                                  
              DISPLAY 'OPEN ERROR ' ERACCT-FILE-STATUS ' ON ERACCT'
              MOVE +16 TO RETURN-CODE                                   
              ADD +1 TO FORCE-DUMP.                                     
                                                                        
           OPEN OUTPUT A-STATES-STATEMENTS
                       B-STATES-STATEMENTS
                       C-STATES-STATEMENTS
                                                                        

           MOVE RUN-DATE               TO WS-WORK-DATE
           PERFORM 0050-READ-INPUT     THRU 0050-EXIT

           .                                                            
       0000-EXIT.                                                       
           EXIT.                                                        
                                                                        
       0050-READ-INPUT.

           READ BILLING-STATEMENTS AT END
              SET END-OF-INPUT         TO TRUE
           END-READ
           
           IF NOT END-OF-INPUT
              ADD 1                    TO WS-INPUT-CNT
           END-IF

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-FILE.

           IF STMT-RECORD(1:1) = '1'
              IF STMT-RECORD (2:7) = 'VOUCHER'
                 PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                    (S1 > +7)
                    OR (END-OF-INPUT)
                    IF END-OF-INPUT
                       DISPLAY ' SOMETHING WRONG HERE VOUCHER EOF '
                       PERFORM ABEND-PGM
                    END-IF
                    MOVE STMT-RECORD   TO VOUCHER-REC (S1)
                    PERFORM 0050-READ-INPUT
                                       THRU 0050-EXIT
                 END-PERFORM
              ELSE
                 IF STMT-RECORD (2:13) = 'REFUND LETTER'
                    PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                       (S1 > +17)
                       OR (END-OF-INPUT)
                       IF END-OF-INPUT
                          DISPLAY ' SOMETHING WRONG HERE LETTER EOF '
                          PERFORM ABEND-PGM
                       END-IF
                       MOVE STMT-RECORD
                                       TO LETTER-REC (S1)
                       PERFORM 0050-READ-INPUT
                                       THRU 0050-EXIT
                    END-PERFORM
                 ELSE
                    PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                       (S1 > +3)
                       OR (END-OF-INPUT)
                       IF END-OF-INPUT
                          DISPLAY ' SOMETHING WRONG HERE HEADING EOF '
                          PERFORM ABEND-PGM
                       END-IF
                       MOVE STMT-RECORD
                                       TO STMT-REC (S1)
                       PERFORM 0050-READ-INPUT
                                       THRU 0050-EXIT
                    END-PERFORM
                    IF STMT-REC (3) (130:2) = ' 1'
                       PERFORM 0125-SET-STATE
                                       THRU 0125-EXIT
                 END-IF
              END-IF
           END-IF

      *    PERFORM 0050-READ-INPUT     THRU 0050-EXIT

           .                                                            
       0100-EXIT.                                                       
           EXIT.                                                        

       0125-SET-STATE.

           MOVE SPACES                 TO WS-STATE-SW
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD
           MOVE STMT-RECORD (16:1)     TO AM-CARRIER
           MOVE STMT-RECORD (17:6)     TO AM-GROUPING
           MOVE STMT-RECORD (24:10)    TO AM-ACCOUNT
           MOVE ZEROS                  TO AM-EXPIRE-DT
           MOVE STMT-RECORD            TO STMT-REC (4)
           PERFORM 0050-READ-INPUT     THRU 0050-EXIT
           MOVE STMT-RECORD            TO STMT-REC (5)
           PERFORM 0050-READ-INPUT     THRU 0050-EXIT
           MOVE STMT-RECORD            TO STMT-REC (6)
           PERFORM 0050-READ-INPUT     THRU 0050-EXIT
           MOVE STMT-RECORD            TO STMT-REC (7)
           PERFORM 0050-READ-INPUT     THRU 0050-EXIT
           MOVE STMT-RECORD            TO STMT-REC (8)
           PERFORM 0050-READ-INPUT     THRU 0050-EXIT
           IF STMT-REC (8) (36:3) = SPACES
              MOVE STMT-REC (7) (36:30) TO WS-WK-ADD
              IF STMT-REC (7) (36:3) = SPACES
                 MOVE STMT-REC (6) (36:30) TO WS-WK-ADD
              END-IF
           ELSE
              MOVE STMT-REC (8) (36:30) TO WS-WK-ADD
           END-IF
           PERFORM VARYING S1 FROM +30 BY -1 UNTIL
              (S1 < +1)
              OR (WS-WK-ADD (S1:1) NOT = SPACES)
           END-PERFORM
           IF S1 < +1
              DISPLAY 'PROBLEM FINDING STATE ' WS-WK-ADD
              PERFORM ABEND-PGM
           END-IF
           MOVE WS-WK-ADD ((S1 - 1):2)   TO AM-STATE
           DISPLAY ' ACCOUNT KEY IS ' AM-CONTROL-A
           MOVE AM-CONTROL-A           TO WS-HOLD-AM-KEY
           START ERACCT KEY IS >= AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS = '00'
              READ ERACCT NEXT RECORD
              IF ERACCT-FILE-STATUS = '00'
                 IF AM-CONTROL-A = WS-HOLD-AM-KEY
                    SET VALID-STATE    TO TRUE
                 ELSE
                    DISPLAY ' NO ACCOUNT MASTER ' WS-HOLD-AM-KEY
                 END-IF
              ELSE
                 DISPLAY ' ERROR - ERACCT - READ ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           ELSE
              DISPLAY ' ERROR - ERACCT - START ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              (S1 > +48)
              OR (WS-STATE (S1)        = AM-STATE)
           END-PERFORM
           IF S1 > +48
              DISPLAY ' STATE MISSING FROM TABLE ' AM-STATE
              PERFORM ABEND-PGM
           END-IF
           
           EVALUATE WS-REPORT (S1)
              WHEN 'A'
                 PERFORM 0150-PROCESS-A-STMT
                                       THRU 0150-EXIT
              WHEN 'B'
                 PERFORM 0160-PROCESS-B-STMT
                                       THRU 0160-EXIT
              WHEN 'C'
                 PERFORM 0170-PROCESS-C-STMT
                                       THRU 0170-EXIT
              WHEN OTHER
                 DISPLAY ' INVALID REPORT ID ' WS-REPORT (S1)
                 PERFORM ABEND-PGM
           END-EVALUATE

           MOVE SPACES                 TO VOUCHER-RECORDS
                                          LETTER-RECORDS
                                          STMT-RECORDS

           .
       0125-EXIT.
           EXIT.

       0150-PROCESS-A-STMT.

           IF VOUCHER-RECORDS NOT = SPACES
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 S1 > +7
                 WRITE A-STATES-RECORD FROM VOUCHER-REC (S1)
              END-PERFORM
           END-IF

           IF LETTER-RECORDS NOT = SPACES
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 S1 > +17
                 WRITE A-STATES-RECORD    FROM LETTER-REC (S1)
              END-PERFORM
           END-IF

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +8
              WRITE A-STATES-RECORD    FROM STMT-REC (S1)
           END-PERFORM

           PERFORM UNTIL
              (STMT-RECORD (1:8) = '1VOUCHER' OR '1REFUND ')
              OR (END-OF-INPUT)
              WRITE A-STATES-RECORD    FROM STMT-RECORD
              PERFORM 0050-READ-INPUT  THRU 0050-EXIT
           END-PERFORM

           .
       0150-EXIT.
           EXIT.

       0160-PROCESS-B-STMT.

           IF VOUCHER-RECORDS NOT = SPACES
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 S1 > +7
                 WRITE B-STATES-RECORD FROM VOUCHER-REC (S1)
              END-PERFORM
           END-IF

           IF LETTER-RECORDS NOT = SPACES
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 S1 > +17
                 WRITE B-STATES-RECORD    FROM LETTER-REC (S1)
              END-PERFORM
           END-IF

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +8
              WRITE B-STATES-RECORD    FROM STMT-REC (S1)
           END-PERFORM

           PERFORM UNTIL
              (STMT-RECORD (1:8) = '1VOUCHER' OR '1REFUND ')
              OR (END-OF-INPUT)
              WRITE B-STATES-RECORD    FROM STMT-RECORD
              PERFORM 0050-READ-INPUT  THRU 0050-EXIT
           END-PERFORM

           .
       0160-EXIT.
           EXIT.

       0170-PROCESS-C-STMT.

           IF VOUCHER-RECORDS NOT = SPACES
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 S1 > +7
                 WRITE C-STATES-RECORD FROM VOUCHER-REC (S1)
              END-PERFORM
           END-IF

           IF LETTER-RECORDS NOT = SPACES
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 S1 > +17
                 WRITE C-STATES-RECORD    FROM LETTER-REC (S1)
              END-PERFORM
           END-IF

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +8
              WRITE C-STATES-RECORD    FROM STMT-REC (S1)
           END-PERFORM

           PERFORM UNTIL
              (STMT-RECORD (1:8) = '1VOUCHER' OR '1REFUND ')
              OR (END-OF-INPUT)
              WRITE C-STATES-RECORD    FROM STMT-RECORD
              PERFORM 0050-READ-INPUT  THRU 0050-EXIT
           END-PERFORM

           .
       0170-EXIT.
           EXIT.

       9000-END-OF-JOB.                                                 

           CLOSE BILLING-STATEMENTS
                 ERACCT
                 A-STATES-STATEMENTS
                 B-STATES-STATEMENTS
                 C-STATES-STATEMENTS

           .
       9000-EXIT.                                                       
           EXIT.                                                        
       ABEND-PGM.
                           COPY ELCABEND.
                                                                        
