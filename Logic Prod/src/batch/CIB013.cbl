       CBL NOADV                                                        
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    CIB013.
       DATE-WRITTEN.  APRIL, 2000.                                      
                                                                        
      *REMARKS.

      *             TTTTTTT     BBBBBBB     DDDDDD
      *                T        B      B    D     D
      *                T        B      B    D     D
      *                T        BBBBBBBB    D     D
      *                T        B      B    D     D
      *                T        B      B    D     D
      *                T        BBBBBBB     DDDDDD
                            
060506******************************************************************
060506*                   C H A N G E   L O G
060506*
060506* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
060506*-----------------------------------------------------------------
060506*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
060506* EFFECTIVE    NUMBER
060506*-----------------------------------------------------------------
020310* 020310  CR2008100900004  PEMA  NEW PROGRAM
012711* 012711  CR2011010400001  PEMA  CORRECT ADDRESS LINE 3
060812* 060812  CR2012050400001  PEMA  SORTING BARCD ON COVERSHEET ONLY
082012* 082012  CR2012042700005  PEMA  ADD OVER 120 DAYS TO AGEING
101101******************************************************************
                                                                        
       ENVIRONMENT DIVISION.                                            
                                                                        
       INPUT-OUTPUT SECTION.                                            
                                                                        
       FILE-CONTROL.                                                    
                                                                        
           SELECT ERCOMP                                                
                  ASSIGN TO SYS018-FBA1-ERCOMP                          
                  ORGANIZATION IS INDEXED                               
                  ACCESS IS DYNAMIC                                     
                  RECORD KEY IS CO-CONTROL-PRIMARY                      
                  FILE STATUS IS ERCOMP-STATUS.                         

           SELECT FORMDEFS  ASSIGN TO FORMDEFS
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS fd-form-key
                  FILE STATUS IS FORMDEFS-STATUS.

           SELECT BILLING-STATEMENTS                                    
                  ASSIGN TO SYS010                                      
                  FILE STATUS IS SYS010-STATUS.                         

           SELECT DISK-DATE                                             
                  ASSIGN TO SYS019.                                     
                                                                        
           SELECT REFUND1-STATEMENTS
                  ASSIGN TO SYS011.

           SELECT REST-OF-STATEMENTS
                  ASSIGN TO SYS012.

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

       FD  FORMDEFS.
       01  FORM-DEF-IN-REC.
           05  fd-form-key.
               10  fd-form-name        pic x(10).
               10  fd-form-month       pic 99.
           05  f                       pic x(888).

       FD  DISK-DATE                                                    
           COPY ELCDTEFD.                                               
                                                                        
       FD  REFUND1-STATEMENTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REFUND1-RECORD     PIC X(133).

       FD  REST-OF-STATEMENTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REST-OF-STMTS-REC  PIC X(133).

       FD  KEYS-EXT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.                                            

       01  KEYS-EXT-RECORD             PIC X(57).

       WORKING-STORAGE SECTION.                                         
       copy "ctypes.cpy".
                                                                        
       77  WS-REF1-ADDR-SEQ-NO         PIC 9(7)  VALUE ZEROS.
       77  WS-VOC-ADDR-SEQ-NO         PIC 9(7)  VALUE ZEROS.
       77  A1                          PIC S999  VALUE +0 COMP-3.
       77  WS-DIS-ADDR-SEQ-NO         PIC ZZZZZZ9 BLANK WHEN ZERO.
       77  WS-REFUND1-SEQ-NO          PIC 9(4) VALUE ZEROS.
       77  WS-END-BAL                 PIC S9(9)V99  VALUE +0 COMP-3.
       01  WS-TYPE-R-RECORD.
           05  WS-TYPE-R-TYPE          PIC X      VALUE ' '.
           05  WS-TYPE-R-CSR           PIC X(4).
           05  WS-TYPE-R-END-BAL       PIC S9(7)V99 COMP-3 VALUE ZEROS.
           05  WS-TYPE-R-SEQ-NO        PIC 9(9)   VALUE ZEROS.
           05  WS-TYPE-R-REC           PIC X(133) VALUE SPACES.

       01  FILLER.
           05  ws-ahl-pd-date1         PIC X(18)   VALUE SPACE.
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
           05  ERCOMP-STATUS       PIC X(2)    VALUE SPACE.             
           05  FORMDEFS-STATUS     PIC X(2)    VALUE SPACE.             
           05  SYS030-STATUS       PIC X(2)    VALUE SPACE.             
           05  SYS031-STATUS       PIC X(2)    VALUE SPACE.             
           05  SYS032-STATUS       PIC X(2)    VALUE SPACE.             
           05  SAVE-BARCODE1       PIC X(50)   VALUE SPACE.             
           05  SAVE-BARCODE2       PIC X(25)   VALUE SPACE.             
           05  PREV-CO-CONTROL     PIC X(29)   VALUE SPACE.             
080305     05  TYPE-OF-STATEMENT   PIC X(7)    VALUE SPACE.             
           05  REMIT-PAGE-NO       PIC 9(6)    VALUE ZERO.              
           05  WS-DATE             PIC 9(8)    VALUE ZERO.              
           05  WS-HDG  OCCURS 4 TIMES PIC X(133).                       
DAN01      05  WK-ADDR OCCURS 7 TIMES PIC X(30).
           05  WK-AMT              PIC ZZZ,ZZZ,ZZZ.99/.                 
                                                                        
       01  WS-CSR-RECORD.
           05  WS-CSR-CODE             PIC X(4)   VALUE SPACES.
           05  WS-CSR-SEQ-NO           PIC 9(7)   VALUE ZEROS.
           05  WS-CSR-STMT-RECORD      PIC X(133) VALUE SPACES.

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

                                       copy FORMREC.

       01  due-agt-wso015.
           03  due-agt-key.
               05  due-agt-name        pic x(10).
               05  due-agt-month       pic 99.
           03  due-agt-desc            pic x(30).
           03  due-agt-special-notes occurs 8
                                       pic x(75).
           03  due-agt-comment-1       pic x(95).
           03  due-agt-comment-2       pic x(95).
           03  filler                  pic x(68).

       01  WS-COVER-SHEET.
           05  PD-CC               PIC X.
           05  PD-ID               PIC X(4).
           05  PD-DATE1            PIC X(18).
           05  PD-CUR              PIC ---,--9.99.
           05  PD-OV30             PIC ----,--9.99.
           05  PD-OV60             PIC ----,--9.99.
           05  PD-OV90             PIC ----,--9.99.
           05  PD-END-BAL          PIC ----,--9.99.
           05  PD-ACCOUNT          PIC X(10).
           05  PD-PMT              PIC ----,--9.99.
           05  PD-BAL-FWD          PIC ----,--9.99.
           05  PD-USER-SELECT-2    PIC X(14).
082012     05  filler redefines pd-user-select-2.
082012         10  pd-ahl-ov120    PIC ----,--9.99.
082012         10  filler          pic xxx.
           05  PD-REPORT-CODE-1    PIC X(10).
       01  REDEFINES WS-COVER-SHEET.
           05  FILLER              PIC X.
           05  PD-BARCODE1         PIC X(50).
           05  FILLER              PIC X(10).
           05  PD-BARCODE2         PIC X(25).
       01  REDEFINES WS-COVER-SHEET.
           05  FILLER           PIC X.
           05  PD-ADDRESS       PIC X(132).
       01  REDEFINES WS-COVER-SHEET.
           05  FILLER           PIC X.
           05  PD-SPEC-NOTES    PIC X(132).
                                                                        
       01  WS-VOUCHER-RECORD.                                           
           05  VR-CC            PIC X       VALUE SPACE.                
           05  VR-ID            PIC X(15)   VALUE SPACE.                
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
                                                                        
                                       COPY ELCDATE.
092903                                 COPY ELCDTECX.

092903                                 COPY ELCDTEVR.

       PROCEDURE DIVISION.                                              
                                                                        
092903                                 COPY ELCDTERX.
           PERFORM 0000-INIT-ROUTINE THRU 0000-EXIT                     
           PERFORM 1000-PROCESS-FILE THRU 1000-EXIT UNTIL EOF           
           PERFORM 9000-END-OF-JOB   THRU 9000-EXIT                     
           GOBACK

           .
       0000-INIT-ROUTINE.                                               
      *                                                                 
           OPEN INPUT BILLING-STATEMENTS
           IF SYS010-STATUS NOT = '00'                                  
              DISPLAY 'OPEN ERROR ' SYS010-STATUS ' ON SYS010'          
              MOVE +16 TO RETURN-CODE                                   
              ADD +1 TO FORCE-DUMP.                                     
                                                                        
           OPEN INPUT ERCOMP                                            
           IF ERCOMP-STATUS NOT = '00'                                  
              DISPLAY 'OPEN ERROR ' ERCOMP-STATUS ' ON ERCOMP'          
              MOVE +16 TO RETURN-CODE                                   
              ADD +1 TO FORCE-DUMP.                                     

           OPEN INPUT FORMDEFS
           IF FORMDEFS-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - FORMDEFS - OPEN ' FORMDEFS-STATUS
              PERFORM ABEND-PGM
           END-IF

           OPEN OUTPUT REFUND1-STATEMENTS REST-OF-STATEMENTS
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

           if dte-client = 'AHL'
              move 'AHO015'            to fd-form-name
           else
              move 'CIO015'            to fd-form-name
           end-if
           move run-mo                 to fd-form-month
      *    move 01                     to fd-form-month

           read FORMDEFS
           evaluate true
              when formdefs-status = '00'
                 move form-def-in-rec  to due-agt-wso015
              when formdefs-status = '23'
                 move 01               to fd-form-month
                 read FORMDEFS
                 if formdefs-status = '00'
                    move form-def-in-rec
                                       to due-agt-wso015
                 else
                    move spaces        to due-agt-wso015
                 end-if
              when other
                 move spaces           to due-agt-wso015
           end-evaluate

           close FORMDEFS              

053012     if dte-client not = 'AHL'
053012        go to 0000-exit
053012     end-if
053012
053012     move +0                     to strt

053012     move ws-accept-date         to dc-greg-date-1-ymd-r
053012     move '3'                    to dc-option-code
053012     perform 8500-date-convert   thru 8500-exit
053012     if not no-conversion-error
053012        display ' error - dtecnvrt - accept date ' ws-accept-date
053012           ' ' dc-error-code
053012        perform abend-pgm
053012     end-if
053012     inspect dc-greg-date-1-alpha tallying strt for leading ' '              
053012     move dc-greg-date-1-alpha(strt + 1:)
053012                                 to ws-ahl-pd-date1

           .
       0000-EXIT.                                                       
           EXIT.                                                        

       1000-PROCESS-FILE.                                               

           READ BILLING-STATEMENTS                                      
                AT END GO TO 1000-EXIT.                                 
                                                                        
           IF STMT-RECORD(1:1) = '1'
              PERFORM 2000-NEW-PAGE    THRU 2000-EXIT
              GO TO 1000-EXIT
           END-IF

           IF TYPE-OF-STATEMENT = 'REFUND1'
              WRITE REFUND1-RECORD FROM STMT-RECORD
           ELSE
              WRITE REST-OF-STMTS-REC FROM STMT-RECORD
           END-IF

           .                                                            
       1000-EXIT.                                                       
           EXIT.                                                        

       2000-NEW-PAGE.                                                   

      ***--------------------------------------------------***          
      ***  CHECK TO SEE IF A BARCODE LINE IS NEEDED                     
      ***  ON THE PREVIOUS PAGE BEFORE PRINTING THIS PAGE.              
      ***  NOTE:  BARCODES ARE ONLY ON ODD NUMBERED PAGES.              
      ***--------------------------------------------------***          
                                                                        

060812*    IF TYPE-OF-STATEMENT = 'REFUND1'
060812*       MOVE WS-HDG (3) (126:6)  TO REMIT-PAGE-NO
060812*       DIVIDE REMIT-PAGE-NO BY 2 GIVING WK1 REMAINDER WK2        
060812*       IF WK2 = 1
060812*          MOVE SPACES           TO WS-COVER-SHEET
060812*          MOVE 'C'              TO PD-CC
060812*          ADD 1                 TO WS-REFUND1-SEQ-NO
060812*          MOVE WS-REFUND1-SEQ-NO
060812*                                TO BC-SEQ-NO
060812*          PERFORM 2100-BUILD-BARCODE1
060812*                                THRU 2100-EXIT
060812*          MOVE SAVE-BARCODE1    TO PD-BARCODE1
060812*          WRITE REFUND1-RECORD  FROM WS-COVER-SHEET
060812*       END-IF
060812*    END-IF

           MOVE SPACE TO TYPE-OF-STATEMENT                              
                                                                        
           MOVE STMT-RECORD            TO WS-HDG (1)
           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > 4                
              READ BILLING-STATEMENTS NEXT INTO WS-HDG(SUB)             
           END-PERFORM                                                  
                                                                        
           IF WS-HDG (1)(61:13) = 'OVERALL RECAP'                        
              GO TO 2000-EXIT.                                          

           PERFORM 2200-READ-ERCOMP THRU 2200-EXIT                      
           IF ERCOMP-STATUS NOT = '00'                                  
              GO TO 2000-EXIT.                                          
                                                                        
           IF CO-TYPE NOT = 'A'                                         
              GO TO 2000-EXIT.                                          
                                                                        
100307     IF CO-CSR-CODE = SPACES OR ZEROS OR LOW-VALUES
              DISPLAY ' BAD CSR CODE ' CO-CONTROL CO-TYPE
              MOVE 'JJVA'              TO CO-CSR-CODE
           END-IF

           IF (CO-BILL-SW = 'B' OR 'C' OR 'E')
              AND (CO-END-BAL < ZEROS)
              AND (CO-STMT-TYPE = 'RF4')
              MOVE 'REFUND1' TO TYPE-OF-STATEMENT
              IF CO-CONTROL NOT = WS-PREV-KEYS-KEY
                 MOVE WS-SAVE-KEY-REC  TO CID-KEY-REC
                 MOVE ZEROS            TO CID-KEY-SEQ-NO
                 MOVE '4'              TO CID-STMT-TYPE
                 MOVE CO-CARRIER       TO CID-CARRIER
                 MOVE CO-GROUPING      TO CID-GROUP
                 MOVE CO-RESP-NO       TO CID-FIN-RESP
                 MOVE CO-ACCOUNT       TO CID-ACCOUNT
                 MOVE CO-CURRENT-END-BAL
                                       TO CID-AMT-DUE
                 WRITE KEYS-EXT-RECORD FROM CID-KEY-REC
                 MOVE CO-CONTROL       TO WS-PREV-KEYS-KEY
              END-IF

      *       PERFORM 2400-PRINT-VOUCHER THRU 2400-EXIT
              PERFORM 2500-REFUND-COVER-SHEET THRU 2500-EXIT
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
                 WRITE REFUND1-RECORD FROM WS-HDG(SUB)
              END-PERFORM
           ELSE
              MOVE '      ' TO TYPE-OF-STATEMENT                        
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4             
                WRITE REST-OF-STMTS-REC FROM WS-HDG(SUB)                    
              END-PERFORM                                               
           END-IF                                                       
                                                                        
           MOVE CO-CONTROL-PRIMARY TO PREV-CO-CONTROL                   

           .                                                            
       2000-EXIT.                                                       
           EXIT.                                                        

       2100-BUILD-BARCODE1.                                              

           MOVE CO-CARR-GROUP          TO BC-CARR
           MOVE CO-ACCOUNT             TO BC-ACCT
      *    ADD 1                       TO BC-SEQ-NO
042607     MOVE  28                    TO AGEB16-LEN

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

PEMTST*    MOVE BARCODE1               TO SAVE-BARCODE1

           .
       2100-EXIT.                                                       
           EXIT.                                                        

       2200-READ-ERCOMP.                                                

092903     MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD

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

PEMTST*    IF (CO-CARRIER = '9')
PEMTST*       AND (CO-RESP-NO = '0000019110')
PEMTST*       SET EOF                  TO TRUE
PEMTST*    END-IF

           .                                                            
       2200-EXIT.                                                       
           EXIT.                                                        

       2500-REFUND-COVER-SHEET.

           IF CO-CONTROL-PRIMARY = PREV-CO-CONTROL                      
              GO TO 2500-EXIT
           END-IF

           MOVE '1'                    TO PD-CC
           MOVE 'PDUE'                 TO PD-ID

           if dte-client = 'AHL'
              move ws-ahl-pd-date1     to pd-date1
           else
              MOVE +0                  TO STRT
              INSPECT ALPH-DATE TALLYING STRT FOR LEADING ' '
              MOVE ALPH-DATE(STRT + 1:)
                                       TO PD-DATE1
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
082012     if dte-client = 'AHL'
082012        move co-ov120            to pd-ahl-ov120
082012     end-if
           MOVE SPACES                 TO PD-REPORT-CODE-1

           WRITE REFUND1-RECORD  FROM WS-COVER-SHEET

           MOVE SPACES           TO WS-COVER-SHEET
           ADD 1                 TO WS-REFUND1-SEQ-NO
           MOVE WS-REFUND1-SEQ-NO
                                 TO BC-SEQ-NO
           PERFORM 2100-BUILD-BARCODE1
                                 THRU 2100-EXIT
           MOVE SAVE-BARCODE1    TO PD-BARCODE1
           WRITE REFUND1-RECORD  FROM WS-COVER-SHEET
           ADD 1                 TO WS-REF1-ADDR-SEQ-NO
           MOVE WS-REF1-ADDR-SEQ-NO
                                       TO WS-DIS-ADDR-SEQ-NO
           MOVE SPACES                 TO WS-COVER-SHEET

           MOVE WS-DIS-ADDR-SEQ-NO     TO WK-ADDR (1)
           MOVE CO-CONTROL-NAME        TO WK-ADDR(2)
           MOVE CO-ACCT-NAME           TO WK-ADDR(3)
           MOVE CO-ADDR-1              TO WK-ADDR(4)
           MOVE CO-ADDR-2              TO WK-ADDR(5)

012711     MOVE SPACES                 TO WK-ADDR(6)
012711     STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
012711        DELIMITED BY '  ' INTO WK-ADDR (6)
012711     END-STRING
012711
012711*    MOVE CO-ADDR-3              TO WK-ADDR(6)
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
              WRITE REFUND1-RECORD     FROM WS-COVER-SHEET
           END-PERFORM                                                  

           move spaces                 to ws-cover-sheet
           PERFORM VARYING A1 FROM 1 BY 1 UNTIL A1 > 8
              move due-agt-special-notes (a1)
                                       to pd-spec-notes
              WRITE REFUND1-RECORD     FROM WS-COVER-SHEET
           END-PERFORM                                                  

           move spaces                 to ws-cover-sheet
           move due-agt-comment-1      to pd-spec-notes
           WRITE REFUND1-RECORD        FROM WS-COVER-SHEET
           move due-agt-comment-2      to pd-spec-notes
           WRITE REFUND1-RECORD        FROM WS-COVER-SHEET

           .                                                            
       2500-EXIT.                                                       
           EXIT.                                                        

       2310-ADJ-ADDRESS.                                                

           MOVE WS-DIS-ADDR-SEQ-NO     TO WK-ADDR (1)
           MOVE CO-CONTROL-NAME TO WK-ADDR(2)                           
           MOVE CO-MAIL-NAME    TO WK-ADDR(3)                           
           MOVE CO-ACCT-NAME    TO WK-ADDR(4)                           
           IF CO-MAIL-NAME(1:3) = 'C/O'                                 
              MOVE CO-ACCT-NAME TO WK-ADDR(3)                           
              MOVE CO-MAIL-NAME TO WK-ADDR(4)                           
           END-IF                                                       
           MOVE CO-ADDR-1 TO WK-ADDR(5)                                 
           MOVE CO-ADDR-2 TO WK-ADDR(6)                                 
           MOVE CO-ADDR-3 TO WK-ADDR(7)                                 
           IF CO-ZIP-PLUS4 = SPACE                                      
              MOVE CO-ZIP-PRIME TO WK-ADDR(7)(26:5)                     
           ELSE                                                         
              MOVE CO-ZIP TO WK-ADDR(7)(22:9)                           
           END-IF                                                       
                                                                        
           IF WK-ADDR(3) = WK-ADDR(4)                                   
              MOVE SPACES TO WK-ADDR(4)                                 
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
              GO TO 2400-EXIT
           END-IF
                                                                        
           MOVE '1' TO VR-CC                                            
           MOVE 'VOUCHER' TO VR-ID                                      
           MOVE +0 TO STRT                                              
           INSPECT ALPH-DATE TALLYING STRT FOR LEADING ' '              
           MOVE ALPH-DATE(STRT + 1:) TO VR-DATE1                        
           MOVE RUN-DATE TO WS-DATE                                     
           STRING WS-DATE(5:2) '/' WS-DATE(7:2) '/' WS-DATE(3:2)        
                  DELIMITED BY SIZE INTO VR-DATE2                       
                                                                        
           MOVE CO-CARR-GROUP TO VR-CARR                                
           MOVE CO-ACCOUNT TO VR-ACCT                                   
           MOVE CO-CURRENT-END-BAL TO VR-AMT
100307     MOVE CO-CSR-CODE        TO VR-CSR
           ADD 1                       TO WS-CSR-SEQ-NO
           MOVE WS-VOUCHER-RECORD      TO WS-CSR-STMT-RECORD
100307     MOVE CO-CSR-CODE            TO WS-CSR-CODE
           WRITE REFUND1-RECORD        FROM WS-CSR-RECORD
                                                                        
           MOVE ' ' TO VR-CC                                            
      *    ADD 1                       TO WS-VOC-ADDR-SEQ-NO
           MOVE WS-VOC-ADDR-SEQ-NO     TO WS-DIS-ADDR-SEQ-NO
           PERFORM 2310-ADJ-ADDRESS THRU 2310-EXIT                      
DAN01      PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              MOVE WK-ADDR(SUB) TO VR-ADDRESS                           
              ADD 1                    TO WS-CSR-SEQ-NO
              MOVE WS-VOUCHER-RECORD   TO WS-CSR-STMT-RECORD
100307        MOVE CO-CSR-CODE         TO WS-CSR-CODE
              WRITE REFUND1-RECORD     FROM WS-CSR-RECORD
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
                 REFUND1-STATEMENTS
                 KEYS-EXT

           .
       9000-EXIT.                                                       
           EXIT.                                                        
       ABEND-PGM.
                           COPY ELCABEND.
                                                                        
