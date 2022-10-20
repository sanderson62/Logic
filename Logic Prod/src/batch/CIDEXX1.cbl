       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    CIDEXX1.                             
      *AUTHOR.        PABLO.
      *               COLLEYVILLE, TEXAS.
      *DATE-COMPILED.                                                   
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.                                                         
      *        THIS PROGRAM RUNS AFTER EL521 AND EXTRACTS ALL THE
      *        NEW ISSUES, NEW CANCELS, ISSUES AND CANCELS IN ERROR AND 
      *        ON HOLD. THE PROGRAM CREATES EXTRACT FILES THAT ARE READ
      *        INTO SSFPBMonBillRpt.pl which creates an xls file that is   
      *        sent to First Premier Bank by Amy.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 032311    2011031600001  PEMA  NEW PROGRAM
      ******************************************************************

       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT EXTRACT-IN           ASSIGN TO SYS010.
                                                                        
           SELECT ERLOFC           ASSIGN TO ERLOFC
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS LO-CONTROL-PRIMARY
                                   FILE STATUS IS ERLOFC-FILE-STATUS.

           SELECT EXTR-GOOD-ISS        ASSIGN TO SYS011
              ORGANIZATION LINE SEQUENTIAL.

           SELECT EXTR-GOOD-CNC        ASSIGN TO SYS012
              ORGANIZATION LINE SEQUENTIAL.

           SELECT EXTR-BAD-ISS         ASSIGN TO SYS013
              ORGANIZATION LINE SEQUENTIAL.

           SELECT EXTR-BAD-CNC         ASSIGN TO SYS014
              ORGANIZATION LINE SEQUENTIAL.

           SELECT EXTR-HOLD-ISS        ASSIGN TO SYS015
              ORGANIZATION LINE SEQUENTIAL.
                                                                        
           SELECT DISK-DATE            ASSIGN TO SYS019.

       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD  EXTRACT-IN
                                       COPY ERCEXTFD.
                                                                        
       01  EXTRACT-IN-REC              PIC X(629).
                                                                        
       FD  ERLOFC.
                                       COPY ERCLOFC.

       FD  EXTR-GOOD-ISS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  EXTR-GOOD-ISS-REC           PIC X(588).

       FD  EXTR-BAD-ISS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  EXTR-BAD-ISS-REC            PIC X(588).

       FD  EXTR-GOOD-CNC
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  EXTR-GOOD-CNC-REC           PIC X(588).

       FD  EXTR-BAD-CNC
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  EXTR-BAD-CNC-REC            PIC X(588).

       FD  EXTR-HOLD-ISS
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  EXTR-HOLD-ISS-REC           PIC X(588).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*   CIDEXX1  WORKING STORAGE   *'.
       77  FILLER  PIC X(32)   VALUE '********* VMOD=2.001 ***********'.
       77  ERLOFC-FILE-STATUS          PIC XX    VALUE LOW-VALUES.
       77  WS-EOF-SW                   PIC X  VALUE ' '.
           88  END-OF-INPUT                VALUE 'Y'.

       01  WS-HOLD-ISSUE-REC           PIC X(200).
       01  FPB-ISSUE-REC.
           05  FPBI-ACCOUNT-NO         PIC X(10).
           05  FPBI-D1                 PIC X.
           05  FPBI-MONTH              PIC X(30).
           05  FPBI-D2                 PIC X.
           05  FPBI-LNAME              PIC X(30).
           05  FPBI-D3                 PIC X.
           05  FPBI-FNAME              PIC X(15).
           05  FPBI-D4                 PIC X.
           05  FPBI-INIT               PIC X.
           05  FPBI-D5                 PIC X.
           05  FPBI-CERT-NO            PIC X(11).
           05  FPBI-D6                 PIC X.
           05  FPBI-EFF-DT             PIC X(10).
           05  FPBI-D7                 PIC X.
           05  FPBI-TERM               PIC Z99.
           05  FPBI-D8                 PIC X.
           05  FPBI-BEN-AMT            PIC ZZZZ,ZZ9.99.
           05  FPBI-D9                 PIC X.
           05  FPBI-PREM               PIC ZZZ,ZZ9.99.
           05  FPBI-D10                PIC X.
           05  FPBI-BEN-TYPE           PIC XX.
           05  FPBI-DASH               PIC X.
           05  FPBI-BEN-DESC           PIC XXX.
           05  FPBI-D13                PIC X.
           05  FPBI-EFF-YR             PIC XXXX.
           05  FPBI-D14                PIC X.
           05  FPBI-RPT-MO             PIC Z9.
           05  FPBI-D15                PIC X.
           05  FPBI-RPT-YR             PIC XXXX.
           05  FPBI-D16                PIC X.
           05  FPBI-COMM               PIC ZZZ,ZZ9.99.
           05  FPBI-D17                PIC X.
           05  FPBI-BRANCH             PIC XX.
           05  FPBI-D18                PIC X.
           05  FPBI-LOAN-OFF           PIC XXX.
           05  FPBI-D19                PIC X.
           05  FPBI-LOAN-OFF-NAME      PIC X(30).

       01  WS-HOLD-CANCEL-REC          PIC X(200).
       01  FPB-CANCEL-REC.
           05  FPBC-ACCOUNT-NO         PIC X(10).
           05  FPBC-D1                 PIC X.
           05  FPBC-MONTH              PIC X(30).
           05  FPBC-D2                 PIC X.
           05  FPBC-LNAME              PIC X(30).
           05  FPBC-D3                 PIC X.
           05  FPBC-CERT-NO            PIC X(11).
           05  FPBC-D4                 PIC X.
           05  FPBC-EFF-DT             PIC X(10).
           05  FPBC-D5                 PIC X.
           05  FPBC-CNC-DT             PIC X(10).
           05  FPBC-D6                 PIC X.
           05  FPBC-BEN-TYPE           PIC XX.
           05  FPBC-D7                 PIC X.
           05  FPBC-REFUND             PIC ZZZ,ZZ9.99.
           05  FPBC-D8                 PIC X.
           05  FPBC-RPT-MO             PIC Z9.
           05  FPBC-D9                 PIC X.
           05  FPBC-RPT-YR             PIC XXXX.
           05  FPBC-D10                PIC X.
           05  FPBC-COMM               PIC ZZZ,ZZ9.99.
           05  FPBC-D11                PIC X.
           05  FPBC-BRANCH             PIC XX.
           05  FPBC-D12                PIC X.
           05  FPBC-LOAN-OFF           PIC XXX.
           05  FPBC-D13                PIC X.
           05  FPBC-LOAN-OFF-NAME      PIC X(30).

                                                                        
       01  FILLER              COMP   SYNC.                             
           12  PGM-SUB             PIC S9(4)           VALUE +522.      
           12  WS-INDEX            PIC S9(4)           VALUE ZERO.      
           12  SUB1                PIC S9(4)           VALUE ZERO.      
                                                                        
       01  FILLER.                                                      
           12  ABEND-CODE              PIC X(4).                        
           12  ABEND-OPTION            PIC X.                           
           12  OLC-REPORT-NAME         PIC X(5)        VALUE 'EL522'.   
           12  X                       PIC X           VALUE SPACE.     
           12  WS-SAVE-PRINT-RECORD    PIC X(133)      VALUE SPACES.    
           12  WS-DAYS-DISAB           PIC 9(3)        VALUE ZERO.      
           12  WS-LAST-CARRIER         PIC X           VALUE SPACES.    
           12  WS-ZERO                 PIC S9    COMP-3  VALUE ZERO.
           12  WS-RETURN-CODE          PIC S9(3) COMP-3  VALUE ZERO.
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      
           12  WS-RUN-DATE-BIN         PIC XX.                          
           12  WS-FILE-ERROR-MESSAGE.                                   
               16  FILLER              PIC X(24)       VALUE            
                       'ERROR OCCURED OPENING - '.                      
               16  WS-FEM-FILE-NAME    PIC X(8).                        

                                       COPY ERCEXTR.
                                       COPY ERCPNDB.

                                       COPY ELCDTECX.               

                                       COPY ELCDTEVR.               

                                       COPY ELCDATE.                

       PROCEDURE DIVISION.                                              
                                                                        
                                       COPY ELCDTERX.                   

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT
           PERFORM 0030-PROCESS-INPUT  THRU 0030-EXIT UNTIL
              END-OF-INPUT
           PERFORM 0300-CLOSE-FILES    THRU 0300-EXIT
           GOBACK

           .
       0010-OPEN-FILES.

           OPEN INPUT EXTRACT-IN ERLOFC
           OPEN OUTPUT EXTR-GOOD-ISS EXTR-GOOD-CNC
                       EXTR-HOLD-ISS EXTR-BAD-ISS EXTR-BAD-CNC

           IF ERLOFC-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERROR - ERLOFC - OPEN '
                                       TO WS-ABEND-MESSAGE
              MOVE ERLOFC-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              GO TO ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           INITIALIZE FPB-ISSUE-REC
           MOVE ';'                    TO FPBI-D1
                                          FPBI-D2
                                          FPBI-D3
                                          FPBI-D4
                                          FPBI-D5
                                          FPBI-D6
                                          FPBI-D7
                                          FPBI-D8
                                          FPBI-D9
                                          FPBI-D10
                                          FPBI-D13
                                          FPBI-D14
                                          FPBI-D15
                                          FPBI-D16
                                          FPBI-D17
                                          FPBI-D18
                                          FPBI-D19
           MOVE FPB-ISSUE-REC          TO WS-HOLD-ISSUE-REC

           INITIALIZE FPB-CANCEL-REC
           MOVE ';'                    TO FPBC-D1
                                          FPBC-D2
                                          FPBC-D3
                                          FPBC-D4
                                          FPBC-D5
                                          FPBC-D6
                                          FPBC-D7
                                          FPBC-D8
                                          FPBC-D9
                                          FPBC-D10
                                          FPBC-D11
                                          FPBC-D12
                                          FPBC-D13

           MOVE FPB-CANCEL-REC         TO WS-HOLD-CANCEL-REC

           PERFORM 0035-READ-INPUT     THRU 0035-EXIT

           .
       0020-EXIT.
           EXIT.

       0030-PROCESS-INPUT.

           IF (EX-EXTRACT-CODE = 'A')
              AND (EX-RECORD-TYPE = 'A')
              PERFORM 0040-PROCESS-DATA THRU 0040-EXIT
           END-IF

           PERFORM 0035-READ-INPUT     THRU 0035-EXIT

           .
       0030-EXIT.
           EXIT.

       0035-READ-INPUT.

           READ EXTRACT-IN INTO EXTRACT-INTERFACE-RECORD AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           .
       0035-EXIT.
           EXIT.

       0040-PROCESS-DATA.

           MOVE EX-DATA-AREAS          TO PENDING-BUSINESS

           IF (PB-CREDIT-ACCEPT-DT = WS-RUN-DATE-BIN)
              OR (PB-CREDIT-ACCEPT-DT = LOW-VALUES)
              CONTINUE
           ELSE
              GO TO 0040-EXIT
           END-IF
                                                                        
           IF (PB-BATCH-TRAILER)                                          
              OR (PB-ALT-CHG-SEQ-NO NOT = ZEROS)
              GO TO 0040-EXIT
           END-IF
                                                                        
           IF (PB-RECORD-RETURNED)
              OR (PB-REIN-ONLY-CERT)
              OR (PB-REISSUED-CERT)
              OR (PB-MONTHLY-CERT)
              OR (CLASIC-CREATED-CERT)
              OR (PB-POLICY-IS-VOIDED)
              OR (PB-POLICY-IS-DECLINED)
              GO TO 0040-EXIT
           END-IF

           IF PB-ISSUE
              PERFORM 0050-PROCESS-ISS THRU 0050-EXIT
           ELSE
              IF PB-CANCELLATION
                 PERFORM 0070-PROCESS-CNC
                                       THRU 0070-EXIT
              ELSE
                 DISPLAY ' INVALID TYPE ' PB-RECORD-TYPE
              END-IF
           END-IF

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-ISS.

           MOVE WS-HOLD-ISSUE-REC      TO FPB-ISSUE-REC
           MOVE PB-ACCOUNT             TO FPBI-ACCOUNT-NO
           MOVE ALPH-DATE              TO FPBI-MONTH
           MOVE PB-I-INSURED-LAST-NAME   TO FPBI-LNAME
           MOVE PB-I-INSURED-FIRST-NAME  TO FPBI-FNAME
           MOVE PB-I-INSURED-MIDDLE-INIT TO FPBI-INIT
           MOVE PB-CERT-NO             TO FPBI-CERT-NO

           MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO FPBI-EFF-DT
              MOVE DC-EDITA-CCYY       TO FPBI-EFF-YR
           ELSE
              MOVE ZEROS               TO FPBI-EFF-DT
              DISPLAY ' ERROR CONVERTING EFF DATE '
           END-IF

           MOVE RUN-MO                 TO FPBI-RPT-MO
           MOVE RUN-CCYR               TO FPBI-RPT-YR
           MOVE '-'                    TO FPBI-DASH
           MOVE PB-I-LOAN-OFFICER (1:2) TO FPBI-BRANCH
           MOVE PB-I-LOAN-OFFICER (3:3) TO FPBI-LOAN-OFF

           MOVE PB-CONTROL-BY-ACCOUNT (1:20)
                                       TO LO-CONTROL-PRIMARY (1:20)
           MOVE PB-I-LOAN-OFFICER      TO LO-OFFICER-CODE
           READ ERLOFC
           IF ERLOFC-FILE-STATUS = '00'
              MOVE LO-OFFICER-NAME    TO FPBI-LOAN-OFF-NAME
           ELSE
              MOVE 'UNKNOWN '         TO FPBI-LOAN-OFF-NAME
           END-IF

           IF PB-I-LF-BENEFIT-CD = '00' OR '  '
              GO TO 0050-PROCESS-AH
           END-IF

           MOVE PB-I-LF-TERM           TO FPBI-TERM
           MOVE PB-I-LF-BENEFIT-AMT    TO FPBI-BEN-AMT
           MOVE PB-I-LF-PREMIUM-AMT    TO FPBI-PREM
           MOVE 'LF'                   TO FPBI-BEN-TYPE
           MOVE PB-I-LF-ABBR           TO FPBI-BEN-DESC

           IF (PB-FATAL-ERRORS)
              OR (PB-UNFORCED-ERRORS)
              COMPUTE FPBI-COMM = PB-I-LF-PREMIUM-AMT *
                 PB-I-LIFE-COMMISSION
           ELSE
              COMPUTE FPBI-COMM ROUNDED = PB-I-LF-PREMIUM-AMT *
                 PB-I-LIFE-COMMISSION
           END-IF

           IF (PB-I-LF-ALT-BENEFIT-AMT NOT = ZERO)
              OR (PB-I-LF-ALT-PREMIUM-AMT NOT = ZERO)
              IF (PB-FATAL-ERRORS)
                 OR (PB-UNFORCED-ERRORS)
                 COMPUTE FPBI-COMM = (PB-I-LF-PREMIUM-AMT +
                    PB-I-LF-ALT-PREMIUM-AMT) *
                    PB-I-LIFE-COMMISSION
              END-IF
           END-IF

           PERFORM 0060-WRITE-ISSUE    THRU 0060-EXIT

           IF (PB-I-LF-ALT-BENEFIT-AMT NOT = ZERO)
              OR (PB-I-LF-ALT-PREMIUM-AMT NOT = ZERO)
              CONTINUE
           ELSE
              GO TO 0050-PROCESS-AH
           END-IF

           MOVE PB-I-LF-ALT-BENEFIT-AMT TO FPBI-BEN-AMT
           MOVE PB-I-LF-ALT-PREMIUM-AMT TO FPBI-PREM

           IF (PB-FATAL-ERRORS)
              OR (PB-UNFORCED-ERRORS)
              MOVE ZEROS               TO FPBI-COMM
           ELSE
              COMPUTE FPBI-COMM ROUNDED = PB-I-LF-ALT-PREMIUM-AMT *
                 PB-I-LIFE-COMMISSION
           END-IF

           PERFORM 0060-WRITE-ISSUE    THRU 0060-EXIT

           .
       0050-PROCESS-AH.

           IF PB-I-AH-BENEFIT-CD = '00' OR '  '
              GO TO 0050-EXIT
           END-IF

           MOVE PB-I-AH-TERM           TO FPBI-TERM
           MOVE PB-I-AH-BENEFIT-AMT    TO FPBI-BEN-AMT
           MOVE PB-I-AH-PREMIUM-AMT    TO FPBI-PREM
           MOVE 'AH'                   TO FPBI-BEN-TYPE
           MOVE PB-I-AH-ABBR           TO FPBI-BEN-DESC
           IF (PB-FATAL-ERRORS)
              OR (PB-UNFORCED-ERRORS)
              COMPUTE FPBI-COMM = PB-I-AH-PREMIUM-AMT *
                 PB-I-AH-COMMISSION
           ELSE
              COMPUTE FPBI-COMM ROUNDED = PB-I-AH-PREMIUM-AMT *
                 PB-I-AH-COMMISSION
           END-IF

           PERFORM 0060-WRITE-ISSUE    THRU 0060-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-WRITE-ISSUE.

           IF PB-RECORD-ON-HOLD
              WRITE EXTR-HOLD-ISS-REC  FROM FPB-ISSUE-REC
           ELSE
              IF (PB-FATAL-ERRORS)
                 OR (PB-UNFORCED-ERRORS)
                 WRITE EXTR-BAD-ISS-REC
                                       FROM FPB-ISSUE-REC
              ELSE
                 WRITE EXTR-GOOD-ISS-REC
                                       FROM FPB-ISSUE-REC
              END-IF
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-PROCESS-CNC.

           MOVE WS-HOLD-CANCEL-REC     TO FPB-CANCEL-REC
           MOVE PB-ACCOUNT             TO FPBC-ACCOUNT-NO
           MOVE ALPH-DATE              TO FPBC-MONTH
           MOVE PB-CI-LAST-NAME        TO FPBC-LNAME
           MOVE PB-CERT-NO             TO FPBC-CERT-NO

           MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO FPBC-EFF-DT
           ELSE
              MOVE ZEROS               TO FPBC-EFF-DT
              DISPLAY ' ERROR CONVERTING EFF DATE '
           END-IF

           MOVE RUN-MO                 TO FPBC-RPT-MO
           MOVE RUN-CCYR               TO FPBC-RPT-YR

           MOVE PB-CI-LOAN-OFFICER (1:2)TO FPBC-BRANCH
           MOVE PB-CI-LOAN-OFFICER (3:3)TO FPBC-LOAN-OFF

           MOVE PB-CONTROL-BY-ACCOUNT (1:20)
                                       TO LO-CONTROL-PRIMARY (1:20)
           MOVE PB-CI-LOAN-OFFICER     TO LO-OFFICER-CODE
           READ ERLOFC
           IF ERLOFC-FILE-STATUS = '00'
              MOVE LO-OFFICER-NAME    TO FPBC-LOAN-OFF-NAME
           ELSE
              MOVE 'UNKNOWN '         TO FPBC-LOAN-OFF-NAME
           END-IF

           IF (PB-C-LF-CANCEL-AMT = ZEROS)
              AND (PB-C-LF-CANCEL-DT = LOW-VALUES)
              GO TO 0070-PROCESS-AH
           END-IF

           MOVE PB-C-LF-CANCEL-DT      TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO FPBC-CNC-DT
           ELSE
              MOVE ZEROS               TO FPBC-CNC-DT
              DISPLAY ' ERROR CONVERTING LF CNC DATE '
           END-IF

           MOVE PB-C-LF-CANCEL-AMT     TO FPBC-REFUND
           MOVE 'LF'                   TO FPBC-BEN-TYPE

           IF (PB-FATAL-ERRORS)
              OR (PB-UNFORCED-ERRORS)
              COMPUTE FPBC-COMM = PB-C-LF-CANCEL-AMT *
                 PB-CI-LIFE-COMMISSION
           ELSE
              COMPUTE FPBC-COMM ROUNDED = PB-C-LF-CANCEL-AMT *
                 PB-CI-LIFE-COMMISSION
           END-IF

           PERFORM 0080-WRITE-CANCEL   THRU 0080-EXIT

           .
       0070-PROCESS-AH.

           IF (PB-C-AH-CANCEL-AMT = ZEROS)
      *       AND (PB-C-AH-CANCEL-DT = LOW-VALUES)
              GO TO 0070-EXIT
           END-IF

           MOVE PB-C-AH-CANCEL-DT      TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO FPBC-CNC-DT
           ELSE
              MOVE ZEROS               TO FPBC-CNC-DT
              DISPLAY ' ERROR CONVERTING AH CNC DATE '
           END-IF

           MOVE PB-C-AH-CANCEL-AMT     TO FPBC-REFUND
           MOVE 'AH'                   TO FPBC-BEN-TYPE

           IF (PB-FATAL-ERRORS)
              OR (PB-UNFORCED-ERRORS)
              COMPUTE FPBC-COMM = PB-C-AH-CANCEL-AMT *
                 PB-CI-AH-COMMISSION
           ELSE
              COMPUTE FPBC-COMM ROUNDED = PB-C-AH-CANCEL-AMT *
                 PB-CI-AH-COMMISSION
           END-IF

           PERFORM 0080-WRITE-CANCEL   THRU 0080-EXIT

           .
       0070-EXIT.
           EXIT.

       0080-WRITE-CANCEL.

           IF (PB-FATAL-ERRORS)
              OR (PB-UNFORCED-ERRORS)
              WRITE EXTR-BAD-CNC-REC   FROM FPB-CANCEL-REC
           ELSE
              WRITE EXTR-GOOD-CNC-REC  FROM FPB-CANCEL-REC
           END-IF

           .
       0080-EXIT.
           EXIT.

       0300-CLOSE-FILES.

           CLOSE EXTRACT-IN ERLOFC EXTR-GOOD-ISS EXTR-GOOD-CNC
              EXTR-HOLD-ISS EXTR-BAD-ISS EXTR-BAD-CNC

           IF ERLOFC-FILE-STATUS NOT = '00'
              MOVE ' ERROR - ERLOFC - CLOSE '
                                       TO WS-ABEND-MESSAGE
              MOVE ERLOFC-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              GO TO ABEND-PGM
           END-IF

           .
       0300-EXIT.
           EXIT.

       8500-DATE-CONVERSION SECTION. COPY ELCDCS.


       ABEND-PGM SECTION.              COPY ELCABEND.                   
