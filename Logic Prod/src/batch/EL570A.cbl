       IDENTIFICATION DIVISION.
       PROGRAM-ID. EL570A.
      *AUTHOR.     PABLO.
      *            COLLEYVILLE, TX.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF    CSO      IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************

      *REMARKS.
      *        THIS PROGRAM CREATES A FREEDOM INTERFACE FILE OF
      *    ALL REFUNDS MADE IN NH THAT IS CONSIDERED PROCESSABLE
      *    THEN FLAGS THE ELCERT RECORD.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 052008    2007110500003  PEMA  NEW PROGRAM
111109* 111109    2008100900003  AJRA  CHANGE CERT NOTE TO NEW FILE      
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT CERT-IN         ASSIGN TO SYS010.

           SELECT ERACCT          ASSIGN TO ERACCT
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS AM-CONTROL-PRIMARY
                                  FILE STATUS IS ERACCT-FILE-STATUS.

111109     SELECT ERCNOT          ASSIGN TO ERCNOT
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
111109                            RECORD KEY IS CZ-CONTROL-PRIMARY
111109                            FILE STATUS IS ERCNOT-FILE-STATUS.

           SELECT ERMAIL          ASSIGN TO ERMAIL
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS MA-CONTROL-PRIMARY
                                  FILE STATUS IS ERMAIL-FILE-STATUS.

           SELECT ELCNTL          ASSIGN TO ELCNTL
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS CF-CONTROL-PRIMARY
                                  FILE STATUS IS ELCNTL-FILE-STATUS.

           SELECT ELCERT          ASSIGN TO ELCERT
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS CM-CONTROL-PRIMARY
                                  FILE STATUS IS ELCERT-FILE-STATUS.

           SELECT DISK-DATE       ASSIGN TO SYS019.
           SELECT PRNTR           ASSIGN TO SYS008.
           SELECT FICH            ASSIGN TO SYS020.
           SELECT MAIL-MERGE-FILE
                                  ASSIGN TO SYS011
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT FREEDOM-INT-FILE
                                  ASSIGN TO SYS021
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  ERACCT.
                                       COPY ERCACCT.

       FD  CERT-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.
                                       COPY ECSCRT01.

111109 FD  ERCNOT.
111109                                 COPY ERCCNOT.

       FD  ELCNTL.
                                       COPY ELCCNTL.

       FD  ELCERT.
                                       COPY ELCCERT.

       FD  ERMAIL.
                                       COPY ERCMAIL.

       FD  MAIL-MERGE-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  MAIL-MERGE-RECORD.
           05  MM-PAYEE-NAME           PIC X(30).
           05  MM-PAYEE-ADDR1          PIC X(30).
           05  MM-PAYEE-ADDR2          PIC X(30).
           05  MM-PAYEE-ADDR3          PIC X(30).
           05  MM-ACCOUNT-NAME         PIC X(30).
           05  MM-EFF-DATE             PIC X(10).
           05  MM-CAN-DATE             PIC X(10).
           05  MM-REF-AMT              PIC $$,$$$9.99.
           05  MM-INT-AMT              PIC $$$9.99.

       FD  FREEDOM-INT-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  FREEDOM-INT-RECORD          PIC X(400).

       FD  DISK-DATE                   COPY ELCDTEFD.

       FD  PRNTR                       COPY ELCPRTFD.

       FD  FICH                        COPY ELCFCHFD.

       WORKING-STORAGE SECTION.
       01  LCP-ABND-CODE               PIC S999 COMP VALUE +519.
       77  LCP-ONCTR-01                PIC S9(8) COMP-3 VALUE ZERO.
       77  LCP-ASA                     PIC X.

       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*     EL570A WORKING STORAGE   *'.
       77  FILLER  PIC X(32)   VALUE '********* VMOD=2.001 ***********'.
       77  ERACCT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
111109 77  ERCNOT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELCNTL-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELCERT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERMAIL-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-CURRENT-BIN-DT           PIC XX  VALUE LOW-VALUES.
       77  WS-EOF-SW                   PIC X   VALUE SPACES.
           88  END-OF-INPUT                    VALUE 'Y'.

       77  WS-RECS-IN                  PIC 9(7) VALUE ZEROS.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  N1                          PIC S999 COMP-3 VALUE +0.
       77  WS-REF-AMT                  PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-INT-AMT                  PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-SKIP-CNT                 PIC 9(5) VALUE ZEROS.
       77  WS-PROCESS-PMT-SW           PIC X.
           88  PROCESS-PMT                VALUE 'Y'.
       77  WS-PROCESS-RERUN-SW         PIC X.
           88  PROCESS-RERUN              VALUE 'Y'.
       77  WS-DAYS                     PIC S9(5)    COMP-3 VALUE +0.
       77  WS-BIN-CANC-DT              PIC XX  VALUE LOW-VALUES.
       77  WS-BIN-EFF-DT               PIC XX  VALUE LOW-VALUES.
       77  WS-BIN-ENT-DT               PIC XX  VALUE LOW-VALUES.
       77  WS-ARCH-NUM                 PIC S9(9)  COMP-3 VALUE +0.
       77  WS-CERT-NOTE-LINE           PIC X(77).
       77  WS-ERNOTE-AMT               PIC $,$$$.99 VALUE ZEROS.
       77  WS-CANC-DT                  PIC 9(11)  COMP-3 VALUE ZEROS.
       77  WS-ENT-DT                   PIC 9(11)  COMP-3 VALUE ZEROS.

       01  WS-WORK-SSN.
           05  FILLER                  PIC 99999 VALUE 00008.
           05  WS-SSN-SEQ-NO           PIC 9(4) VALUE ZEROS.

       01  WS-TRANSACTION-RECORD       PIC X(400).
       01  WS-CHECK-DES-RECORD         PIC X(400).
       01  WS-DISTRIBUTION-RECORD      PIC X(400).
       01  WS-PAYEE-ADDRESS-RECORD     PIC X(400).
       01  WS-ALPHA-1099-RECORD        PIC X(400).
       01  WS-VOUCH-ADDR-1099-RECORD   PIC X(400).

       01  WS-INVOICE-NO.
           05  WS-INVOICE-MM           PIC 99.
           05  WS-INVOICE-DD           PIC 99.
           05  WS-INVOICE-CLM-NO       PIC X(7).
           05  FILLER                  PIC XX    VALUE SPACES.
       01  WS-REC-GRP-CD.
           05  WS-REC-GRP-DA           PIC X.
           05  WS-REC-GRP-SEQ-NO       PIC 999    VALUE 000.
       01  WS-USER-DEFINED.
           05  WS-USER-CARRIER         PIC X.
           05  WS-USER-CLAIM-NO        PIC X(7).
           05  WS-USER-CERT-NO         PIC X(11).
           05  WS-USER-TRLR-SEQ-NO     PIC X(5).
       01  FILLER                          COMP-3.                      
           05  WS-LINE-COUNT               PIC S9(3)       VALUE +99.   
           05  WS-LINE-COUNT-MAX           PIC S9(3)       VALUE +60.   
           05  WS-PAGE                     PIC S9(5)       VALUE ZERO.  
           05  WS-REPORT-SW                PIC S9          VALUE ZERO.  
           05  WS-HEADING-SW               PIC S9          VALUE ZERO.  
           05  WS-PRINT-SW                 PIC S9          VALUE ZERO.  
           05  WS-RECORD-COUNT             PIC S9(9)       VALUE ZERO.  
           05  WS-RETURN-CODE              PIC S9(3)       VALUE ZERO.  
           05  WS-ZERO                     PIC S9          VALUE ZERO.  
                                                                        
           05  WS-INCURRED-AGE             PIC S9(3)       VALUE ZERO.  
           05  WS-YEAR REDEFINES                                        
               WS-INCURRED-AGE             PIC S9(3).                   
                                                                        
           05  WS-AMOUNT                   PIC S9(7)V99 VALUE ZERO.     

       01  FILLER                          COMP SYNC.                   
           05  PGM-SUB                     PIC S9(4)       VALUE +317.  
           05  WS-INDEX                    PIC S9(4)       VALUE ZERO.  
                                                                        
       01  FILLER.                                                      
           05  ABEND-CODE                  PIC X(4).                    
           05  ABEND-OPTION                PIC X.                       
           05  OLC-REPORT-NAME             PIC X(5)      VALUE 'EL570'. 
           05  X                           PIC X           VALUE SPACE. 
                                                                        
           05  WS-SAVE-PRINT-RECORD        PIC X(133)      VALUE SPACES.
                                                                        
           05  WS-LAST-CARRIER             PIC X           VALUE SPACES.
                                                                        
           05  WS-ABEND-MESSAGE            PIC X(80)       VALUE SPACES.
                                                                        
           05  WS-LAST-MONTH               PIC 99          VALUE ZERO.  
           05  WS-LAST-MONTH-X REDEFINES                                
               WS-LAST-MONTH               PIC XX.                      
                                                                        
           05  WS-MONTH                    PIC XX          VALUE ZERO.  
                                                                        
           05  WS-ABEND-FILE-STATUS        PIC XX          VALUE ZERO.  
           05  WS-FIRST-TIME-SWITCH        PIC X           VALUE 'Y'.   
               88  WS-FIRST-TIME                           VALUE 'Y'.   
                                                                        
           05  WS-DATE-WORK.                                            
               10  WS-DW-MONTH             PIC 99.                      
               10  FILLER                  PIC X.                       
               10  WS-DW-DAY               PIC 99.                      
               10  FILLER                  PIC X.                       
               10  WS-DW-YEAR              PIC 99.                      
                                                                        
           EJECT                                                        
                                       COPY ELCVOCH.
       01  FILLER.                                                      
           05  WS-TA-LIFE-OVERIDE-L6       PIC X(6)        VALUE SPACE. 
           05  FILLER                      PIC X(24)       VALUE        
               ' PAYMENTS'.                                             
           05  FILLER                      COMP-3.                      
               10  CT-CURR-LF-PMTS-AMT           PIC S9(9)V99  VALUE +0.
               10  CT-CURR-LF-PMTS-CNT           PIC S9(7)     VALUE +0.
                                                                        
               10  CT-CURR-LF-PMTS-AMT-BM        PIC S9(9)V99  VALUE +0.
               10  CT-CURR-LF-PMTS-CNT-BM        PIC S9(7)     VALUE +0.
                                                                        
       01  WS-HEADING1.                                                 
           05  FILLER                      PIC X(44)       VALUE '1'.   
           05  WS-H1-TITLE                 PIC X(80)       VALUE
               ' ITD  REFUND INTEREST PAYMENTS'.
           05  WS-H1-REPORT-NUMBER         PIC X(06)  VALUE 'EL570A'.
                                                                        
       01  WS-HEADING2.                                                 
           05  FILLER                      PIC X(12)       VALUE SPACES.
           05  FILLER                      PIC X           VALUE SPACES.
           05  FILLER                      PIC X(32)       VALUE SPACES.
           05  WS-H2-CLIENT-NAME           PIC X(75)       VALUE SPACES.
           05  WS-H2-DATE                  PIC X(8).                    
                                                                        
       01  WS-HEADING3.                                                 
           05  FILLER                      PIC XX          VALUE SPACES.
           05  FILLER                      PIC X(30)       VALUE SPACES.
           05  FILLER                      PIC X(21)       VALUE SPACES.
           05  WS-H3-DATE                  PIC X(67)       VALUE SPACES.
           05  FILLER                      PIC X(5)        VALUE 'PAGE'.
           05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  
           05  FILLER                      PIC X(02)       VALUE SPACES.
                                                                        
       01  WS-HEADING4.
           05  FILLER                      PIC X(133)     VALUE
               '0 CARR GROUP   ST  ACCOUNT     EFF DATE   CERT NO      I
      -'NT AMOUNT    SOC SEC NO   CANCEL DT  ENTRY DT    CSR    REF AMOU
      -'NT     DAYS'.

       01  WS-HEADING5.
           05  FILLER                      PIC X(41)      VALUE
               '   /PAYEE NAME AND ADDRESS  /ACCOUNT NAME'.

       01  WS-HEADING6.
           05  FILLER                      PIC X(26)      VALUE SPACES.

       01  WS-HEADING8.
           05  FILLER                      PIC X(26)      VALUE SPACES.

       01  WS-HEADING9.                                                 
           05  FILLER                      PIC X(43)    VALUE SPACES.   
           05  FILLER                      PIC X(5)     VALUE 'COUNT'.  
           05  FILLER                      PIC X(10)    VALUE SPACES.   
           05  FILLER                      PIC X(06)    VALUE 'AMOUNT'. 
           05  FILLER                      PIC X(69)    VALUE SPACES.   

       01  WS-DETAIL1.                                                  
           05  FILLER                      PIC X(4)  VALUE SPACES.
           05  WS-D1-CARRIER               PIC X.
           05  FILLER                      PIC XX    VALUE SPACES.
           05  WS-D1-GROUP                 PIC X(6).
           05  FILLER                      PIC XX    VALUE SPACES.
           05  WS-D1-STATE                 PIC XX.
           05  FILLER                      PIC X     VALUE SPACES.
           05  WS-D1-ACCOUNT               PIC X(10) VALUE SPACES.
           05  FILLER                      PIC XX.
           05  WS-D1-EFFECTIVE-DATE        PIC X(10).
           05  FILLER                      PIC X      VALUE SPACES.
           05  WS-D1-CERT-NO               PIC X(11).
           05  FILLER                      PIC XX     VALUE SPACES.
           05  WS-D1-AMOUNT                PIC Z,ZZZ,ZZ9.99-.
           05  FILLER                      PIC XX.
           05  WS-D1-SOC-SEC-NO            PIC X(11).
           05  FILLER                      PIC XX     VALUE SPACES.
           05  WS-D1-CANCEL-DT             PIC X(10).
           05  FILLER                      PIC X      VALUE SPACES.
           05  WS-D1-ENTRY-DT              PIC X(10).
           05  FILLER                      PIC XX     VALUE SPACES.
           05  WS-D1-CSR-CODE              PIC XXXX.
           05  FILLER                      PIC XX     VALUE SPACES.
           05  WS-D1-CANC-AMOUNT           PIC Z,ZZZ,ZZ9.99-.
           05  FILLER                      PIC XXX    VALUE SPACES.
           05  WS-D1-DAYS                  PIC ZZ99.
                                                                        
       01  WS-DETAIL2.
           05  FILLER                      PIC XXX.
           05  WS-D2-PAYEE-NAME            PIC X(30).
           05  FILLER                      PIC XX.
           05  WS-D2-PAYEE-ADDR1           PIC X(30).
           05  FILLER                      PIC XX.
           05  WS-D2-PAYEE-ADDR2           PIC X(30).
           05  FILLER                      PIC XX.
           05  WS-D2-PAYEE-ADDR3           PIC X(30).

       01  WS-DETAIL3.
           05  FILLER                      PIC XXX.
           05  WS-D3-ACCOUNT-NAME          PIC X(30).

                                                                        
       01  WS-TOTAL-LINE1.
           05  FILLER                      PIC XX.                      
           05  WS-T1-DESCRIPTION.                                       
               10  WS-T1-BENEFIT-CODE      PIC XX.                      
               10  FILLER                  PIC X(4).                    
               10  WS-T1-BENEFIT-DESC      PIC X(10).                   
               10  FILLER                  PIC X(16).                   
                                                                        
           05  FILLER REDEFINES                                         
               WS-T1-DESCRIPTION.                                       
               10  FILLER                  PIC X(6).                    
               10  WS-T1-TOTAL-DESCRIPTION PIC X(26).                   
                                                                        
           05  FILLER REDEFINES                                         
               WS-T1-DESCRIPTION.                                       
               10  FILLER                  PIC X(8).                    
               10  WS-T1-VOIDED-DESCRIPTION PIC X(24).                  
                                                                        
           05  FILLER REDEFINES                                         
               WS-T1-DESCRIPTION.                                       
               10  FILLER                  PIC X(4).                    
               10  WS-T1-NET-DESCRIPTION PIC X(28).                     
                                                                        
           05  FILLER                      PIC X.                       
           05  WS-T1-DASH                  PIC X.                       
           05  FILLER                      PIC X.                       
           05  WS-T1-CURR-COUNT            PIC ZZZ,ZZZ,ZZ9-.            
           05  FILLER                      PIC X.                       
           05  WS-T1-CURR-AMOUNT           PIC ZZZ,ZZZ,ZZ9.99-.         
           05  FILLER                      PIC X.                       
                                                                        
       01  WS-TOTAL-LINE2.
           05  FILLER                      PIC XX.                      
           05  WS-T2-DESCRIPTION.                                       
               10  FILLER                  PIC X(6).                    
               10  WS-T2-PAYEE-DESC        PIC X(11).                   
               10  FILLER                  PIC X(15).                   
                                                                        
           05  FILLER REDEFINES                                         
               WS-T2-DESCRIPTION.                                       
               10  FILLER                  PIC X(6).                    
               10  WS-T2-TOTAL-DESCRIPTION PIC X(26).                   
                                                                        
           05  FILLER REDEFINES                                         
               WS-T2-DESCRIPTION.                                       
               10  FILLER                  PIC X(8).                    
               10  WS-T2-VOIDED-DESCRIPTION PIC X(24).                  
                                                                        
           05  FILLER REDEFINES                                         
               WS-T2-DESCRIPTION.                                       
               10  FILLER                  PIC X(4).                    
               10  WS-T2-NET-DESCRIPTION PIC X(28).                     
                                                                        
           05  FILLER                      PIC X.                       
           05  WS-T2-DASH                  PIC X.                       
           05  FILLER                      PIC X.                       
           05  WS-T2-CURR-COUNT            PIC ZZZ,ZZZ,ZZ9-.            
           05  FILLER                      PIC X.                       
           05  WS-T2-CURR-AMOUNT           PIC ZZZ,ZZZ,ZZ9.99-.         
           05  FILLER                      PIC X.                       
                                                                        
       01  WS-NO-ACTIVITY.                                              
           05  FILLER                      PIC X(30).                   
           05  FILLER                      PIC X(30) VALUE              
                 'NO ACTIVITY WAS FOUND FOR THIS'.                      
           05  FILLER                      PIC X(30) VALUE              
                 ' DATE RANGE                   '.                      

                                       COPY ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

       0000-DATE-CARD-READ SECTION.

                                       COPY ELCDTERX.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              END-OF-INPUT

           PERFORM 4000-PRINT-TOTALS   THRU 4900-EXIT

           PERFORM CLOSE-FILES         THRU CFS-EXIT.

           GOBACK.

       0010-OPEN-FILES.

           OPEN INPUT CERT-IN ERMAIL ERACCT
      *            ELCNTL
111109     OPEN I-O   ELCNTL ERCNOT ELCERT
           OPEN OUTPUT FREEDOM-INT-FILE PRNTR MAIL-MERGE-FILE

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

111109     IF ERCNOT-FILE-STATUS NOT = '00' AND '97'
111109        DISPLAY ' ERROR - ERCNOT - OPEN ' ERCNOT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCERT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ELCERT - OPEN ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCNTL-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ELCNTL - OPEN ' ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ERMAIL - OPEN ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           MOVE RUN-MO                 TO DC-MDY-MONTH
           MOVE RUN-DA                 TO DC-MDY-DAY
           MOVE RUN-YR                 TO DC-MDY-YEAR
           MOVE '4'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION

           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           DISPLAY ' WS-FN-DATE ' WS-FN-DATE
           MOVE WS-FN-DATE             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DT
           ELSE
              DISPLAY 'BAD CURRENT DATE ' WS-FN-DATE
              PERFORM ABEND-PGM
           END-IF
           MOVE LIFE-OVERRIDE-L6       TO WS-TA-LIFE-OVERIDE-L6.

           MOVE SPACES                 TO TRANSACTION-RECORD
                                          CHECK-DES-RECORD
                                          DISTRIBUTION-RECORD
                                          PAYEE-ADDRESS-RECORD
                                          ALPHA-1099-RECORD
                                          VOUCH-ADDR-1099-RECORD
                                          
           MOVE 'T'                    TO TR-RECORD-ID
           MOVE 'F'                    TO CD-RECORD-ID
           MOVE 'D'                    TO DR-RECORD-ID
           MOVE 'A'                    TO PR-RECORD-ID
           MOVE '1'                    TO AR-RECORD-ID
           MOVE '2'                    TO VR-RECORD-ID
           MOVE WS-FN-DA (2:1)         TO WS-REC-GRP-DA
           MOVE 'PSEUDO-INT'           TO TR-VENDOR-ID
                                          CD-VENDOR-ID
                                          DR-VENDOR-ID
                                          PR-VENDOR-ID
                                          AR-VENDOR-ID
                                          VR-VENDOR-ID
           MOVE 'P'                    TO TR-CHECK-TYPE
           MOVE 'R'                    TO TR-TRAN-TYPE
           MOVE 'CSO'                  TO TR-CSO

           STRING WS-FN-MO WS-FN-DA WS-FN-CCYR DELIMITED BY SIZE
              INTO TR-INVOICE-DATE
           END-STRING
              
           MOVE 'IMM'                  TO TR-TERMS-CODE
           MOVE 001                    TO CD-SEQ-NO
                                          DR-SEQ-NO
           MOVE 'E'                    TO DR-DIST-TYPE (1)
           MOVE '6146000100026620CRLIFS'
                                       TO DR-ACCT-NO (1)
           MOVE 'AP'                   TO DR-SOURCE (1)
           MOVE 'S'                    TO DR-SUSPENSE (1) (1:1)
           MOVE 'INT'                  TO AR-CATEGORY
                                          VR-CATEGORY
           MOVE 'S'                    TO AR-TAX-ID-TYPE
           MOVE 'Y'                    TO AR-ACCUM-BY-ACCT
           MOVE 'U'                    TO VR-ADDRESS-TYPE
           MOVE ZEROS                  TO TR-NON-DIS-AMT
                                          TR-DIS-PCT
                                          TR-DIS-AMT
                                          TR-TAX-PCT
                                          TR-TAX-ADJ
                                          TR-TAX-AMT
                                          DR-USE-TAX-AMT (1)
           
           MOVE '+'                    TO TR-NON-DIS-AMT-SIGN
                                          TR-DIS-AMT-SIGN
                                          TR-TAX-ADJ-SIGN
                                          TR-TAX-AMT-SIGN
                                          DR-USE-TAX-AMT-SIGN (1)

           MOVE TRANSACTION-RECORD     TO WS-TRANSACTION-RECORD
           MOVE CHECK-DES-RECORD       TO WS-CHECK-DES-RECORD
           MOVE DISTRIBUTION-RECORD    TO WS-DISTRIBUTION-RECORD
           MOVE PAYEE-ADDRESS-RECORD   TO WS-PAYEE-ADDRESS-RECORD
           MOVE ALPHA-1099-RECORD      TO WS-ALPHA-1099-RECORD
           MOVE VOUCH-ADDR-1099-RECORD TO WS-VOUCH-ADDR-1099-RECORD

      *    MOVE ZEROS                  TO WS-LINE-COUNT
      *    MOVE '1<HTML>'              TO PRT
      *    PERFORM WRITE-A-LINE
      *    MOVE ' <HEAD>'              TO PRT
      *    PERFORM WRITE-A-LINE
      *    MOVE ' <PRE>'               TO PRT
      *    PERFORM WRITE-A-LINE
           MOVE +99                    TO WS-LINE-COUNT

           PERFORM 0210-GET-ARCHNO     THRU 0210-EXIT

           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .
       0020-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           IF (CR-STATE = 'NH')
              AND (CR-CERT-NO NOT = '0008883177 ')
                    AND
              ((CR-LF-CANC-DT NOT = ZEROS)
              OR (CR-AH-CANC-DT NOT = ZEROS))
              PERFORM 0100-CHECK-REF   THRU 0100-EXIT
           END-IF

           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .
       0050-EXIT.
           EXIT.
           
       0060-READ-CERT.

           READ CERT-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ
           
           IF NOT END-OF-INPUT
              ADD 1                    TO WS-RECS-IN
           END-IF

           .
       0060-EXIT.
           EXIT.
           
       0100-CHECK-REF.

           MOVE +0                     TO WS-INT-AMT WS-REF-AMT

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD
           MOVE CR-ACCT-CONTROL        TO AM-CONTROL-A
           MOVE WS-BIN-EFF-DT          TO AM-EXPIRATION-DT

           START ERACCT KEY > AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS = '10' OR '23'
              DISPLAY ' NO ERACCT FOUND '
              CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
              GO TO 0100-EXIT
           END-IF

           READ ERACCT NEXT RECORD 
           IF ERACCT-FILE-STATUS = '00'
              IF AM-GPCD NOT = 02 AND 03 AND 04 AND 05
                 CONTINUE
              ELSE
                 DISPLAY ' REFUND BYPASSED, BUS TYPE '
                    CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO ' ' AM-GPCD
                 GO TO 0100-EXIT
              END-IF
           ELSE
              DISPLAY ' BAD READ ON ERACCT '
                 CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
           END-IF

           IF CR-DT = CR-LF-CANC-DT OR CR-AH-CANC-DT
              GO TO 0100-DO-ELCERT
           END-IF

           IF (CR-LFRFND + CR-AHRFND) = ZEROS
              DISPLAY ' REF BYPASSED, ZERO REFUND '
                 CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
              GO TO 0100-DO-ELCERT
           END-IF

           MOVE CR-LF-CANC-DT          TO WS-CANC-DT
           IF CR-AH-CANC-DT > WS-CANC-DT
              MOVE CR-AH-CANC-DT       TO WS-CANC-DT
           END-IF

           MOVE WS-CANC-DT             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-CANC-DT
           ELSE
              DISPLAY ' ERROR - DATE CONVERT - CANC DT '
                 CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO ' ' WS-CANC-DT
           END-IF

           MOVE CR-DT                  TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-EFF-DT
           ELSE
              DISPLAY ' ERROR - DATE CONVERT - EFF  DT '
                 CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO ' '
                 DC-GREG-DATE-CYMD
           END-IF

           MOVE ZEROS                  TO WS-ENT-DT
           IF CR-LF-CNC-ENT-DT NOT NUMERIC
              MOVE ZEROS               TO CR-LF-CNC-ENT-DT
           END-IF
           IF CR-AH-CNC-ENT-DT NOT NUMERIC
              MOVE ZEROS               TO CR-AH-CNC-ENT-DT
           END-IF

           MOVE CR-LF-CNC-ENT-DT       TO WS-ENT-DT
           IF CR-AH-CNC-ENT-DT > WS-ENT-DT
              MOVE CR-AH-CNC-ENT-DT    TO WS-ENT-DT
           END-IF
           IF WS-ENT-DT = ZEROS
              MOVE CR-LF-CANCEL-EXIT-DATE
                                       TO WS-ENT-DT
              IF CR-AH-CANCEL-EXIT-DATE > WS-ENT-DT
                 MOVE CR-AH-CANCEL-EXIT-DATE
                                       TO WS-ENT-DT
              END-IF
           END-IF

           MOVE 20080815               TO WS-ENT-DT
           MOVE WS-ENT-DT              TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-ENT-DT
           ELSE
              DISPLAY ' ERROR - DATE CONVERT - ENT  DT '
                 CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO ' '
                 DC-GREG-DATE-CYMD
           END-IF

           MOVE WS-BIN-CANC-DT         TO DC-BIN-DATE-1
           MOVE WS-BIN-ENT-DT          TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              IF DC-ELAPSED-DAYS > +30
                 COMPUTE WS-DAYS = (DC-ELAPSED-DAYS - +30) + +15
              ELSE
                 GO TO 0100-DO-ELCERT
              END-IF
           ELSE
              DISPLAY ' CANCEL AND ENTRY DATE CONVERSION ERROR '
                 CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
              PERFORM ABEND-PGM
           END-IF

           MOVE WS-BIN-CANC-DT         TO DC-BIN-DATE-2
           MOVE WS-BIN-EFF-DT          TO DC-BIN-DATE-1

           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              IF DC-ELAPSED-DAYS < +16
                 DISPLAY ' REFUND WITHIN 15 DAYS OF EFFECT DATE '
                    CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 GO TO 0100-DO-ELCERT
              END-IF
           ELSE
              DISPLAY ' CANCEL AND EFFECT DATE CONVERSION ERROR '
                 CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
              PERFORM ABEND-PGM
           END-IF

           COMPUTE WS-REF-AMT = CR-LFRFND + CR-AHRFND

           COMPUTE WS-INT-AMT ROUNDED = (WS-REF-AMT * 
                 (1.10 ** (WS-DAYS / 365) - 1))

           IF WS-INT-AMT = ZEROS
              DISPLAY ' REF BYPASSED, ZERO INTEREST '
                 CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
              GO TO 0100-EXIT
           END-IF

           IF WS-INT-AMT < 1.00
              ADD 1                    TO CT-CURR-LF-PMTS-CNT-BM
              ADD WS-INT-AMT           TO CT-CURR-LF-PMTS-AMT-BM
              DISPLAY ' REF BYPASSED, BELOW MINIMUM '
                 CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
           END-IF

           PERFORM 0120-BUILD-GL-DATA THRU 0120-EXIT

           PERFORM 0150-PROCESS-ERMAIL THRU 0150-EXIT

           IF (AR-TAX-ID-NO = ZEROS)
              AND (WS-INT-AMT < 600.01)
              AND (WS-INT-AMT > .99)
              ADD 1                    TO WS-SSN-SEQ-NO
              MOVE WS-WORK-SSN         TO AR-TAX-ID-NO
                                          DR-SUSPENSE (1) (2:9)
           ELSE
              DISPLAY ' DIDNT ADD 1 TO SSN COUNT '
                 CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
              DISPLAY ' TAX ID ' AR-TAX-ID-NO ' INT AMT ' WS-INT-AMT
           END-IF

111109     PERFORM 0160-PROCESS-ERCNOT THRU 0160-EXIT

           PERFORM 0130-PRINT-REPORT   THRU 0130-EXIT

           IF WS-INT-AMT < 1.00
              CONTINUE
           ELSE
              PERFORM 0140-CREATE-FREEDOM
                                       THRU 0140-EXIT
           END-IF

           .
       0100-DO-ELCERT.

           MOVE LOW-VALUES             TO CM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO CM-COMPANY-CD
           MOVE CR-ACCT-CONTROL        TO CM-CONTROL-PRIMARY (2:19)
           MOVE WS-BIN-EFF-DT          TO CM-CERT-EFF-DT
           MOVE CR-CERT-NO             TO CM-CERT-NO

           READ ELCERT
           IF ELCERT-FILE-STATUS = '00'
              MOVE 'Y'                 TO CM-NH-INTERFACE-SW
              MOVE WS-INT-AMT          TO CM-NH-INT-ON-REFS
              REWRITE CERTIFICATE-MASTER
              IF ELCERT-FILE-STATUS = '00'
                 CONTINUE
              ELSE
                 DISPLAY ' ERROR - ELCERT - REWRITE '
                    ELCERT-FILE-STATUS ' ' CM-STATE ' ' CM-ACCOUNT ' '
                    CM-CERT-NO
              END-IF
           ELSE
              DISPLAY ' ELCERT NOT FOUND '
                 ELCERT-FILE-STATUS ' ' CM-STATE ' ' CM-ACCOUNT ' '
                 CM-CERT-NO
           END-IF

           .
       0100-EXIT.
           EXIT.

       0120-BUILD-GL-DATA.

           MOVE SPACES                 TO WS-USER-DEFINED
           MOVE CR-CARRIER             TO WS-USER-CARRIER
           MOVE CR-ACCOUNT             TO WS-USER-CLAIM-NO
           MOVE CR-CERT-NO             TO WS-USER-CERT-NO
           MOVE WS-USER-DEFINED        TO TR-USER-DEFINED

           MOVE WS-FN-MO               TO WS-INVOICE-MM
           MOVE WS-FN-DA               TO WS-INVOICE-DD
           MOVE CR-ACCOUNT             TO WS-INVOICE-CLM-NO
           MOVE CR-STATE               TO DR-ACCT-STATE (1)
           MOVE WS-INVOICE-NO          TO TR-INVOICE-NO
                                          CD-INVOICE-NO
                                          DR-INVOICE-NO
                                          PR-INVOICE-NO
                                          AR-INVOICE-NO
                                          VR-INVOICE-NO

           MOVE WS-INT-AMT             TO TR-INVOICE-AMT
                                          CD-INVOICE-AMT (1)
                                          DR-INVOICE-AMT (1)
                                          AR-INVOICE-AMT (1)
                                          
           IF WS-INT-AMT < ZEROS
              MOVE '-'                 TO TR-INVOICE-AMT-SIGN
                                          CD-INVOICE-AMT-SIGN (1)
                                          DR-INVOICE-AMT-SIGN (1)
                                          DR-INVOICE-AMT-SIGN (2)
           ELSE
              MOVE '+'                 TO TR-INVOICE-AMT-SIGN
                                          CD-INVOICE-AMT-SIGN (1)
                                          DR-INVOICE-AMT-SIGN (1)
                                          AR-INVOICE-AMT-SIGN (1)
           END-IF

           STRING 'CK ' 'CC ' CR-CSR-CODE ' INT ON REF'
              DELIMITED BY SIZE INTO TR-VOUCHER-REF
           END-STRING

           MOVE SPACES                 TO CD-DESC (1)
           STRING ' INSURED : ' CR-LNAME
              DELIMITED BY SIZE INTO CD-DESC (1)
           END-STRING

           MOVE SPACES                 TO PR-PAYEE-NAME
           STRING CR-FNAME ' ' CR-LNAME DELIMITED BY '  '
              INTO PR-PAYEE-NAME
           END-STRING
           MOVE PR-PAYEE-NAME          TO VR-VENDOR-NAME (1)

           .
       0120-EXIT.
           EXIT.

       0130-PRINT-REPORT.

           MOVE SPACES                 TO MAIL-MERGE-RECORD
           MOVE '0'                    TO WS-DETAIL1
      *    MOVE SPACES                 TO WS-D1-CHECK-NUMBER

           MOVE WS-INT-AMT             TO WS-D1-AMOUNT
                                          MM-INT-AMT
           MOVE WS-REF-AMT             TO WS-D1-CANC-AMOUNT
                                          MM-REF-AMT
           MOVE WS-DAYS                TO WS-D1-DAYS

           MOVE WS-BIN-CANC-DT         TO DC-BIN-DATE-1
           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO WS-D1-CANCEL-DT
                                          MM-CAN-DATE
           ELSE
              MOVE 'ERROR'             TO WS-D1-CANCEL-DT
                                          MM-CAN-DATE
           END-IF

           MOVE WS-BIN-ENT-DT          TO DC-BIN-DATE-1
           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO WS-D1-ENTRY-DT
           ELSE
              MOVE 'ERROR'             TO WS-D1-ENTRY-DT
           END-IF

           MOVE CR-CSR-CODE            TO WS-D1-CSR-CODE
           MOVE CR-CARRIER             TO WS-D1-CARRIER
           MOVE CR-CERT-NO             TO WS-D1-CERT-NO
           MOVE WS-BIN-EFF-DT          TO DC-BIN-DATE-1
           MOVE SPACES                 TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
           MOVE DC-GREG-DATE-A-EDIT    TO WS-D1-EFFECTIVE-DATE
                                          MM-EFF-DATE
           MOVE CR-STATE               TO WS-D1-STATE
           MOVE CR-ACCOUNT             TO WS-D1-ACCOUNT
           MOVE CR-GROUPING            TO WS-D1-GROUP

           MOVE SPACES                 TO WS-DETAIL2

           MOVE PR-PAYEE-NAME          TO WS-D2-PAYEE-NAME
                                          MM-PAYEE-NAME
           MOVE PR-ADDRESS (1)         TO WS-D2-PAYEE-ADDR1
                                          MM-PAYEE-ADDR1
           MOVE PR-ADDRESS (2)         TO WS-D2-PAYEE-ADDR2
                                          MM-PAYEE-ADDR2
           MOVE PR-ADDRESS (3)         TO WS-D2-PAYEE-ADDR3
                                          MM-PAYEE-ADDR3

           IF ERMAIL-FILE-STATUS NOT = '00'
              MOVE ' **** MISSING ****'
                                       TO WS-D2-PAYEE-NAME
                                          WS-D2-PAYEE-ADDR1
                                          WS-D2-PAYEE-ADDR2
                                          WS-D2-PAYEE-ADDR3
           END-IF

           MOVE AR-TAX-ID-NO           TO WS-D1-SOC-SEC-NO

           IF WS-INT-AMT < 1.00
              MOVE ' ** BELOW MINIMUM ** '
                                       TO WS-D2-PAYEE-ADDR2
           END-IF

           MOVE SPACES                 TO WS-DETAIL3

           MOVE AM-NAME                TO WS-D3-ACCOUNT-NAME
                                          MM-ACCOUNT-NAME

           IF WS-LINE-COUNT > (WS-LINE-COUNT-MAX - 2)
              MOVE 61                  TO WS-LINE-COUNT
           END-IF
           MOVE WS-DETAIL1             TO PRT
           PERFORM WRITE-A-LINE

           MOVE WS-DETAIL2             TO PRT
           PERFORM WRITE-A-LINE

           MOVE WS-DETAIL3             TO PRT
           PERFORM WRITE-A-LINE
           
           WRITE MAIL-MERGE-RECORD

           .
       0130-EXIT.
           EXIT.
           
       0140-CREATE-FREEDOM.

           ADD 1                       TO WS-REC-GRP-SEQ-NO
           MOVE WS-REC-GRP-CD          TO TR-REC-GRP-CODE
                                          CD-REC-GRP-CODE
                                          DR-REC-GRP-CODE
                                          PR-REC-GRP-CODE
                                          AR-REC-GRP-CODE
                                          VR-REC-GRP-CODE

           WRITE FREEDOM-INT-RECORD    FROM TRANSACTION-RECORD
      *    WRITE FREEDOM-INT-RECORD    FROM CHECK-DES-RECORD
           WRITE FREEDOM-INT-RECORD    FROM DISTRIBUTION-RECORD
           MOVE 'P'                    TO DR-DIST-TYPE (1)
           MOVE '1108124500000000000000'
                                       TO DR-ACCT-NO (1)
           MOVE '00'                   TO DR-ACCT-STATE (1)
           MOVE SPACES                 TO DR-SUSPENSE (1)
           WRITE FREEDOM-INT-RECORD    FROM DISTRIBUTION-RECORD
           WRITE FREEDOM-INT-RECORD    FROM PAYEE-ADDRESS-RECORD
           WRITE FREEDOM-INT-RECORD    FROM ALPHA-1099-RECORD
           WRITE FREEDOM-INT-RECORD    FROM VOUCH-ADDR-1099-RECORD

           MOVE WS-TRANSACTION-RECORD     TO TRANSACTION-RECORD
           MOVE WS-CHECK-DES-RECORD       TO CHECK-DES-RECORD
           MOVE WS-DISTRIBUTION-RECORD    TO DISTRIBUTION-RECORD
           MOVE WS-PAYEE-ADDRESS-RECORD   TO PAYEE-ADDRESS-RECORD
           MOVE WS-ALPHA-1099-RECORD      TO ALPHA-1099-RECORD
           MOVE WS-VOUCH-ADDR-1099-RECORD TO VOUCH-ADDR-1099-RECORD

           ADD +1                      TO CT-CURR-LF-PMTS-CNT
           ADD WS-INT-AMT              TO CT-CURR-LF-PMTS-AMT

      *    IF (WS-PMT-AMT < +1.00)
      *       AND (WS-PMT-AMT NOT = ZEROS)
      *       MOVE ' ** BELOW MINIMUM ** '
      *                                TO WS-D2-PAYEE-ADDR2
      *       MOVE ' PMT VOIDED  '     TO WS-D2-SSN-COMMENT
      *       ADD +1                   TO CT-CURR-LF-PMTS-CNT-BM
      *       ADD WS-PMT-AMT           TO CT-CURR-LF-PMTS-AMT-BM
      *       PERFORM 3610-VOID-BELOW-MINIMUM
      *                                THRU 3610-EXIT
      *       PERFORM 3620-BUILD-NOTE-TRLR
      *                                THRU 3620-EXIT
      *    END-IF

      *    MOVE +0                     TO WS-INT-AMT WS-REF-AMT

           .
       0140-EXIT.
           EXIT.

       0150-PROCESS-ERMAIL.

           MOVE LOW-VALUES             TO MA-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO MA-COMPANY-CD
           MOVE CR-ACCT-CONTROL        TO MA-CONTROL-PRIMARY (2:19)
           MOVE WS-BIN-EFF-DT          TO MA-CERT-EFF-DT
           MOVE CR-CERT-NO             TO MA-CERT-NO

           READ ERMAIL
           IF ERMAIL-FILE-STATUS = '10' OR '23'
              DISPLAY ' ERMAIL  RECORD NOT FOUND ' CR-STATE ' '
                 CR-ACCOUNT ' ' CR-CERT-NO
              GO TO 0150-EXIT
           ELSE
              IF ERMAIL-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERMAIL - READ ' ERMAIL-FILE-STATUS
                 ' ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           MOVE MA-ADDRESS-LINE-1      TO PR-ADDRESS (1)
                                          VR-VENDOR-ADDRESS (1)
           MOVE MA-ADDRESS-LINE-2      TO PR-ADDRESS (2)
           PERFORM VARYING S1 FROM +30 BY -1 UNTIL
              (S1 < +2)
              OR (MA-CITY-STATE (S1:1) NOT = SPACES)
           END-PERFORM
           IF S1 > +1
              IF S1 < +30
                 ADD +1                TO S1
              END-IF
              STRING MA-CITY-STATE (1:S1) MA-ZIP DELIMITED BY SIZE
                 INTO PR-ADDRESS (3)
              END-STRING
           ELSE
              MOVE MA-ZIP              TO PR-ADDRESS (3)
           END-IF
           MOVE SPACES                 TO PR-ADDRESS (4)
           STRING MA-CITY-STATE MA-ZIP DELIMITED BY SIZE
              INTO VR-VENDOR-ADDRESS (2)
           END-STRING
           IF PR-PAYEE-NAME = SPACES
              STRING MA-INSURED-FIRST-NAME ' '
                 MA-INSURED-LAST-NAME DELIMITED BY '  '
                 INTO PR-PAYEE-NAME
              END-STRING
              MOVE PR-PAYEE-NAME       TO VR-VENDOR-NAME (1)
           END-IF
           IF PR-ADDRESS (2) = SPACES
              MOVE PR-ADDRESS (3)      TO PR-ADDRESS (2)
              MOVE SPACES              TO PR-ADDRESS (3)
           END-IF

           IF (MA-INSURED-SOC-SEC-NO (4:1) = '-')
              AND (MA-INSURED-SOC-SEC-NO (7:1) = '-')
              DISPLAY ' SOC SEC FIX BEFORE ' MA-INSURED-SOC-SEC-NO
              MOVE MA-INSURED-SOC-SEC-NO (5:2)
                                       TO MA-INSURED-SOC-SEC-NO (4:2)
              MOVE MA-INSURED-SOC-SEC-NO (8:4)
                                       TO MA-INSURED-SOC-SEC-NO (6:4)
              MOVE ZEROS               TO MA-INSURED-SOC-SEC-NO (10:2)
              DISPLAY ' SOC SEC FIX AFTER ' MA-INSURED-SOC-SEC-NO
           END-IF
           IF MA-INSURED-SOC-SEC-NO (1:9) NOT NUMERIC
              MOVE ZEROS               TO MA-INSURED-SOC-SEC-NO
           END-IF
           
           MOVE MA-INSURED-SOC-SEC-NO (1:9)
                                       TO AR-TAX-ID-NO
                                          DR-SUSPENSE (1) (2:9)
      *    IF AR-TAX-ID-NO = ZEROS
      *       MOVE '123456789'         TO AR-TAX-ID-NO
      *                                   DR-SUSPENSE (1) (2:9)
      *    END-IF

           .
       0150-EXIT.
           EXIT.

111109 0160-PROCESS-ERCNOT.

           MOVE WS-INT-AMT             TO WS-ERNOTE-AMT
           MOVE SPACES                 TO WS-CERT-NOTE-LINE
           IF WS-INT-AMT < 1.00
              STRING 'NH INTEREST ON CANCELLATION LESS THAN MINIMUM '
                 WS-CURRENT-DATE ' ' WS-ERNOTE-AMT DELIMITED BY '     '
                 INTO WS-CERT-NOTE-LINE
              END-STRING
           ELSE
              STRING WS-CURRENT-DATE
                 ' NH INT ON RFD, THRU 08/15/08 AMT '
                 WS-ERNOTE-AMT ' TO ' PR-PAYEE-NAME DELIMITED BY '     '
                 INTO WS-CERT-NOTE-LINE
              END-STRING
      *       STRING 'NH INTEREST ON CANCELLATION ' WS-CURRENT-DATE ' '
      *          WS-ERNOTE-AMT ' ' PR-PAYEE-NAME DELIMITED BY '     '
      *          INTO WS-CERT-NOTE-LINE
      *       END-STRING
           END-IF

111109     MOVE SPACES                 TO CERT-NOTE-FILE
111109     MOVE 'CZ'                   TO CZ-RECORD-ID
111109     MOVE DTE-CLASIC-COMPANY-CD  TO CZ-COMPANY-CD
111109     MOVE CR-ACCT-CONTROL        TO CZ-CONTROL-PRIMARY (2:19)
111109     MOVE WS-BIN-EFF-DT          TO CZ-CERT-EFF-DT
111109     MOVE CR-CERT-NO             TO CZ-CERT-NO
111109     MOVE '1'                    TO CZ-RECORD-TYPE
111109     MOVE +1                     TO CZ-NOTE-SEQUENCE
111109     MOVE WS-CURRENT-BIN-DT      TO CZ-LAST-MAINT-DT
111109     MOVE +190000                TO CZ-LAST-MAINT-HHMMSS
111109     MOVE 'E570'                 TO CZ-LAST-MAINT-USER
111109     MOVE WS-CERT-NOTE-LINE      TO CZ-NOTE
111109     MOVE '22'                   TO ERCNOT-FILE-STATUS
111109     PERFORM 0170-WRITE-CERT-NOTE THRU 0170-EXIT
111109            UNTIL ERCNOT-FILE-STATUS NOT EQUAL '22'
111109
111109     IF ERCNOT-FILE-STATUS = '00'
              CONTINUE
           ELSE
111109        DISPLAY ' ERROR - ERCNOT - WRITE ' ERCNOT-FILE-STATUS
              ' ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
           END-IF

           .
       0160-EXIT.
           EXIT.

111109 0170-WRITE-CERT-NOTE.
111109 
111109      WRITE CERT-NOTE-FILE
111109      
111109      IF ERCNOT-FILE-STATUS = '22'
111109          ADD +1             TO CZ-NOTE-SEQUENCE
111109      END-IF
111109
111109      .
111109 0170-EXIT.
111109     EXIT.

       0210-GET-ARCHNO.

           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '1'                    TO CF-RECORD-TYPE
           MOVE SPACES                 TO CF-ACCESS-CD-GENL
           MOVE ZEROS                  TO CF-SEQUENCE-NO
           
           READ ELCNTL
           IF ELCNTL-FILE-STATUS = '00'
              MOVE +0                  TO CF-CREDIT-REF-SSN-CNT
              MOVE CF-CREDIT-REF-SSN-CNT
                                       TO WS-SSN-SEQ-NO
           ELSE
              DISPLAY ' ERROR - ELCNTL - READ ' ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0210-EXIT.
           EXIT.

       0220-UPDATE-ARCHNO.

           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '1'                    TO CF-RECORD-TYPE
           MOVE SPACES                 TO CF-ACCESS-CD-GENL
           MOVE ZEROS                  TO CF-SEQUENCE-NO

           READ ELCNTL
           IF ELCNTL-FILE-STATUS = '00'
              DISPLAY ' UPDATING SSN SEQ NO TO ' WS-SSN-SEQ-NO
              MOVE WS-SSN-SEQ-NO       TO CF-CREDIT-REF-SSN-CNT
              REWRITE CONTROL-FILE
           ELSE
              DISPLAY ' ERROR - ELCNTL - READU ' ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0220-EXIT.
           EXIT.

       4000-PRINT-TOTALS.

           PERFORM 0220-UPDATE-ARCHNO  THRU 0220-EXIT

           MOVE +99                    TO WS-LINE-COUNT
           MOVE +1                     TO WS-HEADING-SW

           IF WS-REPORT-SW = +1
               MOVE '0 * * * * * * * GRAND TOTALS * * * * * * *'
                                       TO  WS-HEADING6
           END-IF

           MOVE '0 TOTAL PAYMENTS INTERFACED '
                                       TO WS-TOTAL-LINE1
      *    MOVE SPACES                 TO WS-T1-TOTAL-DESCRIPTION
           MOVE CT-CURR-LF-PMTS-AMT    TO WS-T1-CURR-AMOUNT
           MOVE CT-CURR-LF-PMTS-CNT    TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

           MOVE '  TOTAL PAYMENTS BELOW MINIMUM '
                                       TO WS-TOTAL-LINE1
           MOVE CT-CURR-LF-PMTS-AMT-BM TO WS-T1-CURR-AMOUNT
           MOVE CT-CURR-LF-PMTS-CNT-BM TO WS-T1-CURR-COUNT
           MOVE WS-TOTAL-LINE1         TO PRT
           PERFORM WRITE-A-LINE

      *    MOVE ' </PRE>'              TO PRT
      *    PERFORM WRITE-A-LINE
      *    MOVE ' </HEAD>'             TO PRT
      *    PERFORM WRITE-A-LINE
      *    MOVE ' </HTML>'             TO PRT
      *    PERFORM WRITE-A-LINE

           .
       4900-EXIT.                                                       
           EXIT.                                                        

       8500-DATE-CONVERSION. COPY ELCDCS.

       WRITE-A-LINE SECTION.         COPY ELCWAL.                       

       WRITE-HEADINGS SECTION.                                          
       WHS-010.                                                         
           IF LCP-ONCTR-01 =  0                                         
               ADD 1 TO LCP-ONCTR-01                                    
               MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   
               MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            
               MOVE ALPH-DATE          TO  WS-H3-DATE.                  
                                                                        
           ADD +1  TO  WS-PAGE.                                         
           MOVE WS-PAGE                TO  WS-H3-PAGE.                  
           MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        
           MOVE ZERO                   TO  WS-LINE-COUNT.               
                                                                        
           MOVE WS-HEADING1            TO  PRT.                         
           MOVE '1'                    TO  X.                           
           PERFORM WRITE-PRINTER.                                       
                                                                        
           MOVE WS-HEADING2            TO  PRT.                         
           MOVE ' '                    TO  X.                           
           PERFORM WRITE-PRINTER.                                       
                                                                        
           MOVE WS-HEADING3            TO  PRT.                         
           MOVE ' '                    TO  X.                           
           PERFORM WRITE-PRINTER.                                       
                                                                        
           MOVE WS-HEADING4            TO  PRT.                         
           MOVE ' '                    TO  X.                           
           PERFORM WRITE-PRINTER.                                       
                                                                        
           MOVE WS-HEADING5            TO  PRT.                         
           PERFORM WRITE-PRINTER.                                       
                                                                        
           MOVE +10                    TO  WS-LINE-COUNT.               
                                                                        
           IF WS-HEADING-SW NOT = ZERO                                  
               MOVE WS-HEADING6        TO  PRT                          
               PERFORM WRITE-PRINTER.                                   
                                                                        
           IF WS-HEADING-SW NOT = ZERO                                  
               MOVE WS-HEADING8    TO  PRT                              
               PERFORM WRITE-PRINTER                                    
               MOVE WS-HEADING9    TO  PRT                              
               PERFORM WRITE-PRINTER                                    
               MOVE +15            TO  WS-LINE-COUNT.                   
                                                                        
           IF WS-HEADING-SW = +2 OR +3                                  
               ADD +5  TO  WS-LINE-COUNT.                               
                                                                        
       WHS-020. COPY ELCWHS2.                                           
                                                                        
       WRITE-PRINTER SECTION. COPY ELCWPS.                              
                                                                        
       WPS-020. COPY ELCPRT2.
                                                                        
       CLOSE-FILES SECTION.
                                                                        
       CFS-010. COPY ELCPRTC.

111109     CLOSE ERMAIL MAIL-MERGE-FILE ERCNOT ELCERT
              FREEDOM-INT-FILE PRNTR ERACCT ELCNTL

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACCT - CLOSE ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERMAIL-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERMAIL - CLOSE ' ERMAIL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

111109     IF ERCNOT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCNOT - CLOSE ' ERCNOT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCERT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ELCERT - CLOSE ' ELCERT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELCNTL-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ELCNTL - CLOSE ' ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

          .
       CFS-EXIT.                                                        
           EXIT.                                                        
                                                                        
       ABEND-PGM SECTION. COPY ELCABEND.                                
