       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCC002
       AUTHOR.     PABLO.
       DATE-COMPILED.
      *REMARKS.                                                         
      *       THIS PROGRAM READS THE EL562 FICHE/REPORT FILE            
      *       DIRECTLY OUT OF EL562 AND CREATES 3 SEPARATE FICHE FILES. 
      *       1 PENDING FOR CARRIER 1,3,5 other for 2,4,6
      *       SYS012 (PENDING STATEMENTS) IS CREATED FOR ALL THE EL562
      *       STATEMENTS WHERE THE BILL SWITCH ON THE ERCOMP FILE IS    
      *       A 'B' OR 'C' AND THE BALANCE IS NEGATIVE AND THE          
      *       NON PROCESSED NET PREMIUMS LESS THE NON PROCESSED NET
      *       COMMISSIONS IS EQUAL TO THE ENDING BALANCE (WE WILL REFUND).
      *       SINCE THE ERCOMP FILE DOES NOT MAINTAIN THE NON PROCESSED
      *       TOTALS, THIS PROGRAM MUST GET THE INFORMATION OFF OF THE
      *       SUMMARY PAGE. THAT IS WHY WE STORE THE ENTIRE STATEMENT
      *       IN A TABLE BEFORE WE CAN DECIDE WHAT FILE TO WRITE IT OUT
      *       TO.  SYS011 (OLD STATMENTS) IS ALL THE OTHER STATEMENTS.
      *       THIS PROGRAM WRITES THE SCAN FILE OUT THAT IS READ BY CIDCOX2
      *       TO NOT PASS THOSE TO THE AUTO VOUCHER SYSTEM. IT'S NOT REALLY
      *       A SCAN FILE JUST A FILE THAT CONTAINS ERCOMP KEYS.
      *                                                                 
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 071312  CR2010082600001  PEMA  NEW PROGRAM
102512* 102512  CR2012040900002  PEMA  ADD USER NOTES CAPABILITY
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                                                                        
           SELECT ERCOMP                                                
                  ASSIGN TO ERCOMP
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS CO-CONTROL-PRIMARY
                  FILE STATUS IS ERCOMP-FILE-STATUS.

102512     SELECT FORMDEFS  ASSIGN TO FORMDEFS
102512            ORGANIZATION IS INDEXED
102512            ACCESS IS DYNAMIC
102512            RECORD KEY IS fd-form-key
102512            FILE STATUS IS FORMDEFS-FILE-STATUS.

           SELECT BILLING-STATEMENTS ASSIGN TO SYS010.

           SELECT DISK-DATE          ASSIGN TO SYS019.

           SELECT OTHER-STATEMENTS   ASSIGN TO SYS011.

           SELECT PEND-STATEMENTS1   ASSIGN TO SYS012.

           SELECT PEND-STATEMENTS2   ASSIGN TO SYS013.

           SELECT KEYS-EXT           ASSIGN TO KEYSOT
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
                                                                        
       FD  OTHER-STATEMENTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  OTHER-STMT-REC    PIC X(133).

       FD  PEND-STATEMENTS1
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PEND-STMT-REC1     PIC X(133).

       FD  PEND-STATEMENTS2
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  PEND-STMT-REC2              PIC X(133).

       FD  KEYS-EXT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.                                            

       01  KEYS-EXT-RECORD             PIC X(57).

       WORKING-STORAGE SECTION.                                         
       copy "ctypes.cpy".

       77  WS-PEND-STMT-SW              PIC X   VALUE SPACES.
           88  THIS-IS-PEND-STMT             VALUE 'Y'.
       77  WS-SO-FAR-SW                PIC X   VALUE SPACES.
           88  SO-FAR-SO-GOOD               VALUE 'Y'.
       77  WS-EOF-SW                   PIC X   VALUE SPACES.
           88  END-OF-INPUT                 VALUE 'Y'.
       77  WS-PAGE-SW                  PIC X   VALUE SPACES.
           88  SUMM-PAGE                    VALUE 'Y'.
       77  WS-NEW-ACCT-SW              PIC X   VALUE SPACES.
           88  NEW-ACCT                     VALUE 'Y'.
       77  WS-PREV-PAGE-ODD-SW         PIC X  VALUE ' '.
           88  PREV-PAGE-ODD              VALUE 'Y'.
       77  ERCOMP-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
102512 77  FORMDEFS-FILE-STATUS        PIC XX  VALUE LOW-VALUES.
       77  WS-IN-CNT                   PIC 9(7)  VALUE ZEROS.
       77  WS-OTHER-OUT                PIC 9(7)  VALUE ZEROS.
       77  WS-PEND-OUT                 PIC 9(7)  VALUE ZEROS.
       77  WS-OUT-CNT                  PIC 9(7)  VALUE ZEROS.
       77  S1                         PIC S9(5)  VALUE +0.
       77  S2                         PIC S9(5)  VALUE +0.
       77  W1                         PIC S9(3)  VALUE +0.
       77  W2                         PIC S9(3)  VALUE +0.
       77  WS-END-BAL                 PIC S9(9)V99  VALUE +0 COMP-3.
       77  WS-ADDR-SEQ-NO-c1          PIC 9(7)  VALUE ZEROS.
       77  WS-ADDR-SEQ-NO-c2          PIC 9(7)  VALUE ZEROS.
       77  WS-DIS-ADDR-SEQ-NO         PIC ZZZZZZ9.
       77  A1                          PIC S999  VALUE +0 COMP-3.
       77  B1                          PIC S9(5) VALUE +0 COMP-3.

       01  WS-COMPARE-KEY.
           05  WS-CK-CARRIER           PIC X.
           05  WS-CK-GROUP             PIC X(6).
           05  WS-CK-RESP              PIC X(10).
           05  WS-CK-ACCOUNT           PIC X(10).
       01  WS-PREV-KEY                 PIC X(27)  VALUE LOW-VALUES.
       01  WS-MISC.
           05  WS-REPORT-TABLE.
010410         10  WS-STMT-TABLE OCCURS 25000 PIC X(133).
           05  WS-HOLD-HEAD  OCCURS 4  PIC X(133).
           05  WS-WORK-AMT            PIC X(17)  VALUE SPACES.
           05  WS-PREM-X              PIC X(11)  VALUE ZEROS.
           05  WS-NET-PREM REDEFINES WS-PREM-X
                                      PIC S9(9)V99.
           05  WS-WORK-COMP           PIC X(17)  VALUE SPACES.
           05  WS-COMP-X              PIC X(11)  VALUE ZEROS.
           05  WS-TOT-COMP REDEFINES WS-COMP-X
                                      PIC S9(9)V99.

       01  WS-TYPE-R-RECORD.
           05  WS-TYPE-R-TYPE          PIC X      VALUE ' '.
           05  WS-TYPE-R-CSR           PIC X(4).
           05  WS-TYPE-R-END-BAL       PIC S9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-TYPE-R-SEQ-NO        PIC 9(9)   VALUE ZEROS.
           05  WS-TYPE-R-REC           PIC X(133) VALUE SPACES.

       01  FILLER.
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
           05  ERCOMP-STATUS       PIC X(2)    VALUE SPACE.             
           05  SAVE-BARCODE1       PIC X(50)   VALUE SPACE.             
           05  SAVE-BARCODE2       PIC X(25)   VALUE SPACE.             
           05  PREV-CO-CONTROL     PIC X(29)   VALUE SPACE.             
           05  TYPE-OF-STATEMENT   PIC X(7)    VALUE SPACE.             
           05  REMIT-PAGE-NO       PIC 9(6)    VALUE ZERO.              
           05  WS-DATE             PIC 9(8)    VALUE ZERO.              
           05  WS-HDG  OCCURS 4 TIMES PIC X(133).                       
           05  WK-ADDR OCCURS 7 TIMES PIC X(30).
           05  WK-AMT              PIC ZZZ,ZZZ,ZZZ.99/.                 
                                                                        
       01  WS-CSR-RECORD.
           05  WS-CSR-CODE             PIC X(4)   VALUE SPACES.
           05  WS-CSR-SEQ-NO           PIC 9(7)   VALUE ZEROS.
           05  WS-CSR-STMT-RECORD      PIC X(133) VALUE SPACES.

       01  FILLER               COMP-3.                                 
           05  STRT             PIC S9(3)   VALUE +0.                   
           05  SUB              PIC S9(3)   VALUE +0.                   
           05  WK1              PIC S9(7)   VALUE +0.                   
           05  WK2              PIC S9(7)   VALUE +0.                   

102512                                 COPY FORMREC.                                                                        
102512
102512 01  pending-wso131.
102512     03  wso131-key.
102512         05  wso131-name         pic x(10).
102512         05  wso131-month        pic 99.
102512     03  wso131-desc             pic x(30).
102512     03  wso131-special-notes occurs 8
102512                                 pic x(75).
102512     03  wso131-comment-1        pic x(95).
102512     03  wso131-comment-2        pic x(95).
102512     03  filler                  pic x(68).
102512
102512 01  pending-wso132.
102512     03  wso132-key.
102512         05  wso132-name         pic x(10).
102512         05  wso132-month        pic 99.
102512     03  wso132-desc             pic x(30).
102512     03  wso132-special-notes occurs 8
102512                                 pic x(75).
102512     03  wso132-comment-1        pic x(95).
102512     03  wso132-comment-2        pic x(95).
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

       01  LETTER-TABLES          VALUE IS SPACE.                       
           05  REMIT-LETTER-LINE  OCCURS 10 TIMES PIC X(132).           
                                                                        
       01  WS-SAVE-SCAN-REC        PIC X(57).
       01  CID-SCAN-REC.
           05  SAV-CID-CC              PIC 99.
           05  SAV-CID-YY              PIC 99.
           05  SAV-CID-MM              PIC 99.
           05  CID-SCAN-SEQ-NO         PIC 9(7).
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
                                                                        
                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.                                              
                                                                        
                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              END-OF-INPUT

           PERFORM 0080-GO-FIGURE      THRU 0080-EXIT

           DISPLAY ' BILLING RECS READ ' WS-IN-CNT
           DISPLAY ' PEND STMT RECS    ' WS-PEND-OUT
           DISPLAY ' OTHER STMT RECS   ' WS-OTHER-OUT

           PERFORM 4000-CLOSE-FILES    THRU 4000-EXIT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT BILLING-STATEMENTS
                                                                        
           OPEN INPUT ERCOMP                                            
           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ERCOMP - OPEN ' ERCOMP-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
                                                                        
102512     OPEN INPUT FORMDEFS
102512     IF FORMDEFS-FILE-STATUS NOT = '00' AND '97'
102512        DISPLAY ' ERROR - FORMDEFS - OPEN ' FORMDEFS-FILE-STATUS
102512        PERFORM ABEND-PGM
102512     END-IF

           OPEN OUTPUT OTHER-STATEMENTS
                       PEND-STATEMENTS1 pend-statements2
                       KEYS-EXT
           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           MOVE SPACES                 TO CID-SCAN-REC
           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE WS-SCAN-CCYYMM         TO BC1-DATE
                                          CID-SCAN-REC (1:6)
           MOVE ZEROS                  TO CID-SCAN-SEQ-NO
           MOVE ZEROS                  TO CID-AMT-DUE
           MOVE ';'                    TO SR-DEL1
                                          SR-DEL2
                                          SR-DEL3
                                          SR-DEL4
                                          SR-DEL5
                                          SR-DEL6
           MOVE CID-SCAN-REC           TO WS-SAVE-SCAN-REC

102512     move 'DCO131'               to fd-form-name
102512
102512     move run-mo                 to fd-form-month
102512*    move 01                     to fd-form-month
102512
102512     read FORMDEFS
102512     evaluate true
102512        when formdefs-file-status = '00'
102512           move form-def-in-rec  to pending-wso131
102512        when formdefs-file-status = '23'
102512           move 01               to fd-form-month
102512           read FORMDEFS
102512           if formdefs-file-status = '00'
102512              move form-def-in-rec
102512                                 to pending-wso131
102512           else
102512              move spaces        to pending-wso131
102512           end-if
102512        when other
102512           move spaces           to pending-wso131
102512     end-evaluate
102512
102512     move 'DCO132'               to fd-form-name
102512
102512     move run-mo                 to fd-form-month
102512*    move 01                     to fd-form-month
102512
102512     read FORMDEFS
102512     evaluate true
102512        when formdefs-file-status = '00'
102512           move form-def-in-rec  to pending-wso132
102512        when formdefs-file-status = '23'
102512           move 01               to fd-form-month
102512           read FORMDEFS
102512           if formdefs-file-status = '00'
102512              move form-def-in-rec
102512                                 to pending-wso132
102512           else
102512              move spaces        to pending-wso132
102512           end-if
102512        when other
102512           move spaces           to pending-wso132
102512     end-evaluate
102512
102512     close FORMDEFS              

           MOVE +1                     TO S1

           PERFORM 0040-READ-INPUT     THRU 0040-EXIT

           .                                                            
       0020-EXIT.                                                       
           EXIT.                                                        

       0040-READ-INPUT.

           READ BILLING-STATEMENTS AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO WS-IN-CNT
           END-IF

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           IF STMT-RECORD (1:1) = '1'
              PERFORM 0060-PROCESS-HEADING
                                       THRU 0060-EXIT
              IF END-OF-INPUT
                 GO TO 0050-EXIT
              END-IF
              IF NEW-ACCT
                 PERFORM 0080-GO-FIGURE
                                       THRU 0080-EXIT
                 MOVE WS-COMPARE-KEY   TO WS-PREV-KEY
                 MOVE +1               TO S1
                 MOVE SPACES           TO WS-REPORT-TABLE
                                          WS-PEND-STMT-SW
                                          WS-NEW-ACCT-SW
                                          WS-SO-FAR-SW
                                          WS-PAGE-SW
                 PERFORM 0070-READ-ERCOMP
                                       THRU 0070-EXIT
              END-IF
              MOVE WS-HOLD-HEAD (1)    TO WS-STMT-TABLE (S1)
              ADD +1                   TO S1
              MOVE WS-HOLD-HEAD (2)    TO WS-STMT-TABLE (S1)
              ADD +1                   TO S1
              MOVE WS-HOLD-HEAD (3)    TO WS-STMT-TABLE (S1)
              ADD +1                   TO S1
              MOVE WS-HOLD-HEAD (4)    TO WS-STMT-TABLE (S1)
              ADD +1                   TO S1
           END-IF
                             
           IF STMT-RECORD (17:18) = 'TOTAL NET PREMIUMS'
              MOVE STMT-RECORD (71:17) TO WS-WORK-AMT
              MOVE ZEROS               TO WS-PREM-X
              MOVE +11        TO W2
              PERFORM VARYING W1 FROM +16 BY -1 UNTIL
                 W1 < +1
                 IF WS-WORK-AMT (W1:1) NUMERIC
                    MOVE WS-WORK-AMT (W1:1) TO WS-PREM-X (W2:1)
                    SUBTRACT +1 FROM W2
                 END-IF
              END-PERFORM
              IF WS-WORK-AMT (17:1) = '-'
                 COMPUTE WS-NET-PREM = WS-NET-PREM * -1
              ELSE
                 COMPUTE WS-NET-PREM = WS-NET-PREM * +1
              END-IF
      *       DISPLAY ' NET PREM ' WS-COMPARE-KEY ' ' WS-NET-PREM
           END-IF

           IF STMT-RECORD (17:18) = 'TOTAL COMPENSATION'
              MOVE STMT-RECORD (71:17) TO WS-WORK-AMT
              MOVE ZEROS               TO WS-COMP-X
              MOVE +11        TO W2
              PERFORM VARYING W1 FROM +16 BY -1 UNTIL
                 W1 < +1
                 IF WS-WORK-AMT (W1:1) NUMERIC
                    MOVE WS-WORK-AMT (W1:1) TO WS-COMP-X (W2:1)
                    SUBTRACT +1 FROM W2
                 END-IF
              END-PERFORM
              IF WS-WORK-AMT (17:1) = '-'
                 COMPUTE WS-TOT-COMP = WS-TOT-COMP * -1
              ELSE
                 COMPUTE WS-TOT-COMP = WS-TOT-COMP * +1
              END-IF
      *       DISPLAY ' TOT COMP ' WS-COMPARE-KEY ' ' WS-TOT-COMP
           END-IF

           MOVE STMT-RECORD            TO WS-STMT-TABLE (S1)
           ADD +1                      TO S1

010410     IF S1 > 25000
              DISPLAY ' YOU NEED TO INCREASE SIZE OF STMT TABLE '
010410        DISPLAY ' COMPARE KEY ' WS-COMPARE-KEY
010410        DISPLAY ' PREV KEY    ' WS-PREV-KEY
              PERFORM ABEND-PGM
           END-IF

           PERFORM 0040-READ-INPUT     THRU 0040-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-PROCESS-HEADING.

           MOVE STMT-RECORD            TO WS-HOLD-HEAD (1)
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT
           MOVE STMT-RECORD            TO WS-HOLD-HEAD (2)
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT
           MOVE STMT-RECORD            TO WS-HOLD-HEAD (3)
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT
           MOVE STMT-RECORD            TO WS-HOLD-HEAD (4)
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT

           IF WS-HOLD-HEAD (1) (61:13) = 'OVERALL RECAP'
              SET END-OF-INPUT         TO TRUE
              GO TO 0060-EXIT
           END-IF

           MOVE WS-HOLD-HEAD (4)(16:01)
                                       TO WS-CK-CARRIER
           MOVE WS-HOLD-HEAD (4)(17:06)
                                       TO WS-CK-GROUP
           MOVE WS-HOLD-HEAD (4)(90:10)
                                       TO WS-CK-RESP
           MOVE WS-HOLD-HEAD (4)(24:10)
                                       TO WS-CK-ACCOUNT

           IF (WS-COMPARE-KEY NOT = WS-PREV-KEY)
              SET NEW-ACCT             TO TRUE
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-READ-ERCOMP.                                                

           MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD

           MOVE WS-HOLD-HEAD (4)(16:01) TO CO-CARRIER                          
           MOVE WS-HOLD-HEAD (4)(17:06) TO CO-GROUPING                         
           MOVE WS-HOLD-HEAD (4)(90:10) TO CO-RESP-NO                          
           MOVE WS-HOLD-HEAD (4)(24:10) TO CO-ACCOUNT                          
           MOVE 'A'                    TO CO-TYPE                             

           IF CO-RESP-NO = SPACES                                       
              MOVE CO-ACCOUNT          TO CO-RESP-NO
           END-IF

           IF CO-CONTROL-PRIMARY = PREV-CO-CONTROL
              GO TO 0070-EXIT
           END-IF
                                                                        
           IF (CO-CARRIER = SPACES)
              AND (CO-GROUPING = SPACES)
              GO TO 0070-EXIT
           END-IF

           READ ERCOMP
           IF ERCOMP-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCOMP - READ ' ERCOMP-FILE-STATUS
                 ' KEY=' CO-CONTROL-PRIMARY (2:27)
              PERFORM ABEND-PGM
           END-IF

           MOVE CO-CONTROL-PRIMARY     TO PREV-CO-CONTROL

           .
       0070-EXIT.                                                       
           EXIT.                                                        

       0080-GO-FIGURE.

           IF WS-PREV-KEY = LOW-VALUES
              GO TO 0080-EXIT
           END-IF

           IF (CO-BILL-SW = 'B' OR 'C')
              AND (CO-END-BAL < +0)
              SET SO-FAR-SO-GOOD TO TRUE
           END-IF

           IF SO-FAR-SO-GOOD
              COMPUTE WS-END-BAL = (WS-NET-PREM - WS-TOT-COMP) * -1
              IF (CO-END-BAL <= (WS-END-BAL + .10))
                 AND (CO-END-BAL >= (WS-END-BAL - .10))
                 SET THIS-IS-PEND-STMT  TO TRUE
              END-IF
           END-IF

           IF THIS-IS-PEND-STMT
              PERFORM 0150-PRINT-COVER-SHEET THRU 0150-EXIT
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                 S2 > S1
                 IF WS-STMT-TABLE (S2) (1:1) = '1'
                    PERFORM 0085-CHECK-BAR THRU 0085-EXIT
                 END-IF
                 if co-carrier = '1' or '3' or '5'
                    WRITE PEND-STMT-REC1 FROM WS-STMT-TABLE (S2)
                 else
                    WRITE PEND-STMT-REC2 FROM WS-STMT-TABLE (S2)
                 end-if
                 ADD 1 TO WS-PEND-OUT
              END-PERFORM
           ELSE
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                 S2 > S1
                 WRITE OTHER-STMT-REC  FROM WS-STMT-TABLE (S2)
                 ADD 1 TO WS-OTHER-OUT
              END-PERFORM
           END-IF

           .
       0080-EXIT.
           EXIT.

       0085-CHECK-BAR.

           DISPLAY ' MADE CHECK BAR ' CO-CARRIER ' ' CO-ACCOUNT
           DISPLAY ' PAGE NUM ' WS-STMT-TABLE (S2 + 2) (126:6)

           MOVE WS-STMT-TABLE (S2 + 2) (126:6)
                                       TO REMIT-PAGE-NO
           DIVIDE REMIT-PAGE-NO BY 2 GIVING WK1 REMAINDER WK2
           IF WK2 = 1
              SET PREV-PAGE-ODD        TO TRUE
           END-IF

           .
       0085-EXIT.
           EXIT.

       0090-BUILD-BARCODE1.                                             

           MOVE CO-CARR-GROUP          TO BC-CARR
           MOVE CO-ACCOUNT             TO BC-ACCT
           ADD 1                       TO BC-SEQ-NO
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
      *    DISPLAY  BARCODE1

           .
       0090-EXIT.                                                       
           EXIT.                                                        

       0150-PRINT-COVER-SHEET.

           MOVE '1'                    TO PD-CC
           MOVE 'PDUE'                 TO PD-ID

           MOVE +0                     TO STRT
           INSPECT ALPH-DATE TALLYING STRT FOR LEADING ' '
           MOVE ALPH-DATE(STRT + 1:)   TO PD-DATE1
           if co-carrier = '1' or '3' or '5'
              move '1'                 to pd-carrier
           else
              move '2'                 to pd-carrier
           end-if
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

           if co-carrier = '1' or '3' or '5'
              WRITE PEND-STMT-REC1      FROM WS-COVER-SHEET
           else
              WRITE PEND-STMT-REC2      FROM WS-COVER-SHEET
           end-if

           MOVE SPACES                 TO WS-COVER-SHEET
           PERFORM 0090-BUILD-BARCODE1 THRU 0090-EXIT
           MOVE SAVE-BARCODE1          TO PD-BARCODE1

           if co-carrier = '1' or '3' or '5'
              WRITE PEND-STMT-REC1      FROM WS-COVER-SHEET
           else
              WRITE PEND-STMT-REC2      FROM WS-COVER-SHEET
           end-if

           MOVE SPACES                 TO WS-COVER-SHEET
           if co-carrier = '1' or '3' or '5'
              ADD 1                    TO WS-ADDR-SEQ-NO-c1
              MOVE WS-ADDR-SEQ-NO-c1   TO WS-DIS-ADDR-SEQ-NO
           else
              ADD 1                    TO WS-ADDR-SEQ-NO-c2
              MOVE WS-ADDR-SEQ-NO-c2   TO WS-DIS-ADDR-SEQ-NO
           end-if

           MOVE WS-DIS-ADDR-SEQ-NO     TO WK-ADDR (1)
           MOVE CO-CONTROL-NAME        TO WK-ADDR(2)
           MOVE CO-ACCT-NAME           TO WK-ADDR(3)
           MOVE CO-ADDR-1              TO WK-ADDR(4)
           MOVE CO-ADDR-2              TO WK-ADDR(5)
051810     MOVE SPACES                 TO WK-ADDR(6)
051810     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
051810        DELIMITED BY '  ' INTO WK-ADDR (6)
051810     END-STRING
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

           PERFORM VARYING A1 FROM 1 BY 1 UNTIL A1 > 7
              MOVE WK-ADDR(A1)         TO PD-ADDRESS                           
              if co-carrier = '1' or '3' or '5'
                 WRITE PEND-STMT-REC1  FROM WS-COVER-SHEET
              else
                 WRITE PEND-STMT-REC2  FROM WS-COVER-SHEET
              end-if
           END-PERFORM                                                  

102512     move spaces                 to ws-cover-sheet
102512     PERFORM VARYING A1 FROM 1 BY 1 UNTIL A1 > 8
102512        if co-carrier = '1' or '3' or '5'
102512           move wso131-special-notes (a1)
102512                                 to pd-spec-notes
102512           WRITE PEND-STMT-REC1  FROM WS-COVER-SHEET
102512        else
102512           move wso132-special-notes (a1)
102512                                 to pd-spec-notes
102512           WRITE PEND-STMT-REC2  FROM WS-COVER-SHEET
102512        end-if
102512     END-PERFORM                                                  
102512
102512     move spaces                 to ws-cover-sheet
102512     if co-carrier = '1' or '3' or '5'
102512        move wso131-comment-1    to pd-spec-notes
102512        WRITE PEND-STMT-REC1     FROM WS-COVER-SHEET
102512        move wso131-comment-2    to pd-spec-notes
102512        WRITE PEND-STMT-REC1     FROM WS-COVER-SHEET
102512     else
102512        move wso132-comment-1    to pd-spec-notes
102512        WRITE PEND-STMT-REC2     FROM WS-COVER-SHEET
102512        move wso132-comment-2    to pd-spec-notes
102512        WRITE PEND-STMT-REC2     FROM WS-COVER-SHEET
102512     end-if

           MOVE WS-SAVE-SCAN-REC       TO CID-SCAN-REC
           ADD 1                       TO BC1-SEQ-NO
           MOVE BC1-SEQ-NO             TO CID-SCAN-SEQ-NO
           MOVE 'P'                    TO CID-STMT-TYPE
           MOVE CO-CARRIER             TO CID-CARRIER
           MOVE CO-GROUPING            TO CID-GROUP
           MOVE CO-RESP-NO             TO CID-FIN-RESP
           MOVE CO-ACCOUNT             TO CID-ACCOUNT
           MOVE CO-CURRENT-END-BAL     TO CID-AMT-DUE
           WRITE KEYS-EXT-RECORD       FROM CID-SCAN-REC

           .                                                            
       0150-EXIT.                                                       
           EXIT.                                                        

       4000-CLOSE-FILES.

           CLOSE BILLING-STATEMENTS ERCOMP
              OTHER-STATEMENTS PEND-STATEMENTS1
              pend-statements2 KEYS-EXT                                                                     

           .
       4000-EXIT.
           EXIT.

       ABEND-PGM.
                           COPY ELCABEND.
                                                                        
