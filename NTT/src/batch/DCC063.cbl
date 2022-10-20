       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCC063.
       AUTHOR.     PABLO.
       DATE-COMPILED.

      *REMARKS.
      *        THIS PROGRAM READS THE FICHE FILE OUT OF ECS063 (SYS020)
      *        AND SEPARATES THE STATEMENTS INTO 2 GROUPS.
      *        ONE GROUP WHERE WE OWE THE AGENT (BALANCE IS < 0)
      *        AND THE OTHER GROUP IS WHERE THE AGENT OWES CSO.
      
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 043012  CR2011041100003  PEMA  NEW PROGRAM
052912* 052912  CR2012042700004  PEMA  CORRECT ACCT NUMBER
060712* 060712  CR2012050400001  PEMA  SORTING BARCD ON COVERSHEET ONLY
082012* 082012  CR2012042700005  PEMA  ADD OVER 120 DAYS TO AGEING
100413* 100413  IR2013100100001  PEMA  LOOK FOR B REC IF NO G
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                                                                        
           SELECT ERCOMP    ASSIGN TO ERCOMP
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS CO-CONTROL-PRIMARY
                  FILE STATUS IS ERCOMP-FILE-STATUS.

           SELECT FORMDEFS  ASSIGN TO FORMDEFS
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS fd-form-key
                  FILE STATUS IS FORMDEFS-FILE-STATUS.

           SELECT BILLING-STATEMENTS ASSIGN TO SYS010.
      
           SELECT DISK-DATE          ASSIGN TO SYS019.
      
           SELECT DUE-AGENT-STMT     ASSIGN TO SYS011.
      
           SELECT REMIT-STMT         ASSIGN TO SYS012.
           SELECT OTHER-STMT         ASSIGN TO SYS013.
      
           SELECT SCAN-EXT           ASSIGN TO SCANOT
                                     ORGANIZATION LINE SEQUENTIAL.
      
       DATA DIVISION.                                                   
      
       FILE SECTION.                                                    
                                                                        
       FD  BILLING-STATEMENTS                                           
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  STMT-RECORD                 PIC X(133).
                                                                        
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
                                                                        
       FD  DUE-AGENT-STMT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  DUE-AGENT-STMT-REC          PIC X(133).
      
       FD  REMIT-STMT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REMIT-STMT-REC              PIC X(133).

       FD  OTHER-STMT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  OTHER-STMT-REC              PIC X(133).

       FD  SCAN-EXT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.                                            
      
       01  SCAN-EXT-RECORD             PIC X(55).
      
      
       WORKING-STORAGE SECTION.                                         
       copy "ctypes.cpy".
      
       77  WS-HEADING-SW               PIC X   VALUE SPACES.
           88  WS-HEADING                  VALUE 'Y'.
       77  WS-REMIT-STMT-SW            PIC X   VALUE SPACES.
           88  WS-REMIT-STMT               VALUE 'Y'.
       77  WS-DUE-AGENT-STMT-SW        PIC X   VALUE SPACES.
           88  WS-DUE-AGENT-STMT           VALUE 'Y'.
       77  WS-NEW-ACCT-SW              PIC X   VALUE SPACES.
           88  WS-NEW-ACCT                 VALUE 'Y'.
       77  WS-EOF-SW                   PIC X   VALUE SPACES.
           88  END-OF-INPUT                VALUE 'Y'.
       77  ERCOMP-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  FORMDEFS-FILE-STATUS        PIC XX  VALUE LOW-VALUES.
       77  WS-IN-CNT                   PIC 9(7)  VALUE ZEROS.
       77  WS-OUT-CNT                  PIC 9(7)  VALUE ZEROS.
       77  A1                          PIC S999  VALUE +0 COMP-3.
       77  WS-SAVE-STATE               PIC XX   VALUE SPACES.
       77  WS-USER-SELECT-2            PIC X(10)  VALUE SPACES.
       77  WS-REPORT-CODE-1            PIC X(10)  VALUE SPACES.
       77  WS-ACCOUNT-FOUND-SW         PIC X    VALUE SPACES.
           88  FOUND-ACCOUNT              VALUE 'Y'.
       77  WS-SAVE-ERACCT-KEY          PIC X(19)  VALUE LOW-VALUES.
       77  WS-DATE-RANGE-FOUND-SW      PIC X   VALUE SPACES.
          88  DATE-RANGE-FOUND            VALUE 'Y'.
       77  WS-ADDR-SEQ-NO              PIC 9(7)  VALUE ZEROS.
060712 77  ws-remit-addr-seq-no        pic 9(7)  value zeros.
060712 77  ws-due-addr-seq-no          pic 9(7)  value zeros.
       77  WS-DIS-ADDR-SEQ-NO          PIC ZZZZZZ9  BLANK WHEN ZERO.
       77  WS-DISPLAY-AMT              PIC ---,--9.99.
       01  WS-COMPARE-KEY.
           05  WS-CK-CARRIER           PIC X.
           05  WS-CK-GROUP             PIC X(6).
           05  WS-CK-RESP              PIC X(10).
           05  WS-CK-ACCOUNT           PIC X(10).
       01  WS-PREV-KEY                 PIC X(27)  VALUE LOW-VALUES.
       01  WS-MISC.
           05  WS-SUMMARY-SW           PIC X   VALUE SPACES.
               88  LAST-PAGE-SUMMARY          VALUE 'Y'.
               88  LAST-PAGE-DETAIL           VALUE 'D'.
           05  WS-SRCH-STATE           PIC X(30)  VALUE SPACES.
           05  WS-HOLD-HEAD  OCCURS 4  PIC X(133).
      
                                       copy FORMREC.

       01  due-cso-wso020.
           03  due-cso-key.
               05  due-cso-name        pic x(10).
               05  due-cso-month       pic 99.
           03  due-cso-desc            pic x(30).
           03  due-cso-special-notes occurs 8
                                       pic x(75).
           03  due-cso-comment-1       pic x(95).
           03  due-cso-comment-2       pic x(95).
           03  filler                  pic x(68).

       01  due-agt-wso021.
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
           05  PD-CARRIER          PIC X.
           05  PD-DATE1            PIC X(18).
           05  PD-CUR              PIC ---,--9.99.
           05  PD-OV30             PIC ----,--9.99.
           05  PD-OV60             PIC ----,--9.99.
           05  PD-OV90             PIC ----,--9.99.
           05  PD-OV120            PIC ----,--9.99.
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
       01  REDEFINES WS-COVER-SHEET.
           05  FILLER           PIC X.
           05  PD-SPEC-NOTES    PIC X(132).

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

       01  FILLER.
           05  SAVE-BARCODE1       PIC X(50)   VALUE SPACE.
           05  SAVE-BARCODE2       PIC X(25)   VALUE SPACE.
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
           05  PREV-CO-CONTROL     PIC X(29)   VALUE SPACE.             
           05  REMIT-PAGE-NO       PIC 9(6)    VALUE ZERO.              
           05  WS-DATE             PIC 9(8)    VALUE ZERO.              
           05  WS-HDG  OCCURS 4 TIMES PIC X(133).                       
           05  WK-ADDR OCCURS 7 TIMES PIC X(30).                        
           05  WK-AMT              PIC ZZZ,ZZZ,ZZZ.99/.                 
                                                                        
       01  FILLER               COMP-3.                                 
           05  STRT             PIC S9(3)   VALUE +0.                   
           05  S1               PIC S9(3)   VALUE +0.                   
           05  WK1              PIC S9(7)   VALUE +0.                   
           05  WK2              PIC S9(7)   VALUE +0.                   
      
       01  WS-SAVE-SCAN-REC        PIC X(55).
       01  CID-SCAN-REC.
           05  SAV-CID-CC              PIC 99.
           05  SAV-CID-YY              PIC 99.
           05  SAV-CID-MM              PIC 99.
           05  sav-comp-indicator      pic 9.
           05  CID-SCAN-SEQ-NO         PIC 9(6).
           05  SR-DEL1                 PIC X.
           05  CID-CARRIER             PIC X.
           05  SR-DEL2                 PIC X.
           05  CID-GROUP               PIC X(6).
           05  SR-DEL3                 PIC X.
           05  CID-FIN-RESP            PIC X(10).
           05  SR-DEL4                 PIC X.
           05  CID-ACCOUNT             PIC X(10).
           05  SR-DEL5                 PIC X.
           05  CID-AMT-DUE             PIC 9(7).99.
      
      ***************************************************************** 
      *  BARCODE ROUTINE                                              * 
      ***************************************************************** 
       01  AGEB16-WORKAREA.
      *     05  AGEB16-LEN       PIC 9(4)    VALUE ZERO.
NTTDel*    05  AGEB16-LEN       short.
NTTIns     05  AGEB16-LEN       PIC S9(4) COMP.
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
           05  bc2-fill            pic x value '0'.
           05  BC1-SEQ-NO          PIC 9(6)    VALUE ZERO.
      
      
                                       COPY ELCDTECX.
      
                                       COPY ELCDTEVR.
      
       PROCEDURE DIVISION.                                              
                                                                        
                                       COPY ELCDTERX.
      
           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT
      
           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              END-OF-INPUT
      
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

           OPEN INPUT FORMDEFS
           IF FORMDEFS-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - FORMDEFS - OPEN ' FORMDEFS-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           OPEN OUTPUT DUE-AGENT-STMT SCAN-EXT
                       REMIT-STMT OTHER-STMT

           .
       0010-EXIT.
           EXIT.
      
       0020-INIT.
      
           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE SPACES                 TO WS-COVER-SHEET
      
           MOVE SPACES                 TO CID-SCAN-REC
           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE WS-SCAN-CCYYMM         TO BC1-DATE
                                          CID-SCAN-REC (1:6)
           move 5                      to sav-comp-indicator

           MOVE ZEROS                  TO CID-SCAN-SEQ-NO
           MOVE ZEROS                  TO CID-AMT-DUE
           MOVE ';'                    TO SR-DEL1
                                          SR-DEL2
                                          SR-DEL3
                                          SR-DEL4
                                          SR-DEL5
           MOVE CID-SCAN-REC           TO WS-SAVE-SCAN-REC
      
           move '5'                    to bc2-fill
      
           move 'DCO020'               to fd-form-name

           move run-mo                 to fd-form-month
      *    move 01                     to fd-form-month

           read FORMDEFS
           evaluate true
              when formdefs-file-status = '00'
                 move form-def-in-rec  to due-cso-wso020
              when formdefs-file-status = '23'
                 move 01               to fd-form-month
                 read FORMDEFS
                 if formdefs-file-status = '00'
                    move form-def-in-rec
                                       to due-cso-wso020
                 else
                    move spaces        to due-cso-wso020
                 end-if
              when other
                 move spaces           to due-cso-wso020
           end-evaluate


           move 'DCO021'               to fd-form-name

           move run-mo                 to fd-form-month
      *    move 01                     to fd-form-month

           read FORMDEFS
           evaluate true
              when formdefs-file-status = '00'
                 move form-def-in-rec  to due-agt-wso021
              when formdefs-file-status = '23'
                 move 01               to fd-form-month
                 read FORMDEFS
                 if formdefs-file-status = '00'
                    move form-def-in-rec
                                       to due-agt-wso021
                 else
                    move spaces        to due-agt-wso021
                 end-if
              when other
                 move spaces           to due-agt-wso021
           end-evaluate

           close FORMDEFS              

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
              SET WS-HEADING TO TRUE
              PERFORM 0060-PROCESS-HEADING THRU 0060-EXIT
           END-IF
      
           IF STMT-RECORD (18:16) = 'DUE COMPANY     '
              OR 'DUE FROM COMPANY'
              SET LAST-PAGE-SUMMARY    TO TRUE
           END-IF
      
           IF WS-REMIT-STMT
              PERFORM 0080-WRITE-REMIT-STMT
                                       THRU 0080-EXIT
           ELSE
              IF WS-DUE-AGENT-STMT
                 PERFORM 0085-WRITE-DUE-AGENT
                                       THRU 0085-EXIT
              ELSE
                 PERFORM 0090-WRITE-OTHER-STMT
                                       THRU 0090-EXIT
              END-IF
           END-IF
      
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT
      
           .
       0050-EXIT.
           EXIT.
      
       0060-PROCESS-HEADING.
      
           IF WS-REMIT-STMT
               DISPLAY ' YES REMIT STMT ' CO-CARRIER ' ' CO-RESP-NO
              MOVE SPACES              TO WS-COVER-SHEET
              IF LAST-PAGE-SUMMARY
                 DISPLAY ' YES LAST PAGE SUMMARY ' CO-CARRIER ' '
                    CO-RESP-NO
                 SET LAST-PAGE-DETAIL  TO TRUE
                 MOVE 'C'              TO PD-CC
                 MOVE SAVE-BARCODE2    TO PD-BARCODE2
              END-IF
060712*       MOVE WS-HOLD-HEAD (3)(119:6)
060712*                                TO REMIT-PAGE-NO
060712*       DIVIDE REMIT-PAGE-NO BY 2 GIVING WK1 REMAINDER WK2        
060712*       IF WK2 = 1
060712*          MOVE 'C'              TO PD-CC
060712*          PERFORM 2100-BUILD-BARCODE1
060712*                                THRU 2100-EXIT              
060712*          MOVE SAVE-BARCODE1    TO PD-BARCODE1
060712*       END-IF
              IF WS-COVER-SHEET NOT = SPACES
                 WRITE REMIT-STMT-REC  FROM WS-COVER-SHEET
              END-IF
           END-IF
      
           MOVE STMT-RECORD            TO WS-HOLD-HEAD (1)
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT
           MOVE STMT-RECORD            TO WS-HOLD-HEAD (2)
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT
           MOVE STMT-RECORD            TO WS-HOLD-HEAD (3)
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT
           MOVE STMT-RECORD            TO WS-HOLD-HEAD (4)
           PERFORM 0040-READ-INPUT     THRU 0040-EXIT

           IF WS-HOLD-HEAD (4) (22:18) = 'MASTER RECORDS IN '
              display ' found master recs in '
              move ' ' to ws-remit-stmt-sw ws-due-agent-stmt-sw
              GO TO 0060-EXIT
           END-IF
      
           MOVE WS-HOLD-HEAD (4)(02:01)
                                       TO WS-CK-CARRIER
           MOVE WS-HOLD-HEAD (4)(04:06)
                                       TO WS-CK-GROUP
           MOVE WS-HOLD-HEAD (4)(11:10)
                                       TO WS-CK-RESP
           MOVE SPACES                 TO WS-CK-ACCOUNT
      
           IF WS-COMPARE-KEY NOT = WS-PREV-KEY
              SET WS-NEW-ACCT          TO TRUE
              PERFORM 0070-READ-ERCOMP THRU 0070-EXIT
              MOVE WS-COMPARE-KEY      TO WS-PREV-KEY
           ELSE
              MOVE ' '                 TO WS-NEW-ACCT-SW
           END-IF
      
           .
       0060-EXIT.
           EXIT.
      
       0070-READ-ERCOMP.                                                
      
           MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD
      
           MOVE WS-HOLD-HEAD (4)(02:01) TO CO-CARRIER                          
           MOVE WS-HOLD-HEAD (4)(04:06) TO CO-GROUPING                         
           MOVE WS-HOLD-HEAD (4)(11:10) TO CO-RESP-NO                          
           MOVE LOW-VALUES              TO CO-ACCOUNT                          
           MOVE 'G'                    TO CO-TYPE                             
      
      *    IF CO-RESP-NO = SPACES                                       
      *       MOVE CO-ACCOUNT          TO CO-RESP-NO
      *    END-IF
      
           IF CO-CONTROL-PRIMARY = PREV-CO-CONTROL
              GO TO 0070-EXIT
           END-IF
                                                                        
           IF (CO-CARRIER = SPACES)
              AND (CO-GROUPING = SPACES)
              GO TO 0070-EXIT
           END-IF

100413     .      
100413 0070-READ-NEXT.

           READ ERCOMP
100413     IF ERCOMP-FILE-STATUS = '23'
100413        AND CO-TYPE = 'G'
100413        MOVE 'B' TO CO-TYPE
100413        GO TO 0070-READ-NEXT
100413     END-IF

           IF ERCOMP-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCOMP - READ ' ERCOMP-FILE-STATUS
                 ' KEY=' CO-CONTROL-PRIMARY (2:27)
              PERFORM ABEND-PGM
           END-IF
      
           MOVE CO-CONTROL-PRIMARY     TO PREV-CO-CONTROL
      
           move ' '                    to ws-due-agent-stmt-sw
                                          ws-remit-stmt-sw

           IF (CO-BILL-SW = 'B' OR 'C' OR 'E')
              or (dte-client = 'DCC')
              IF (CO-CURRENT-END-BAL IS POSITIVE)
                 or (co-current-end-bal = zeros)
                 move ' '              to ws-due-agent-stmt-sw
                 MOVE CO-CURRENT-END-BAL TO WS-DISPLAY-AMT
                 DISPLAY ' REMIT STMT ' CO-CARRIER ' ' CO-RESP-NO ' '
                    CO-BILL-SW ' ' WS-DISPLAY-AMT
                 SET WS-REMIT-STMT  TO TRUE
              ELSE
                 MOVE ' '                 TO WS-REMIT-STMT-SW
                 MOVE CO-CURRENT-END-BAL TO WS-DISPLAY-AMT
                 DISPLAY ' NOT REMIT STMT ' CO-CARRIER ' '
                    CO-RESP-NO ' ' CO-BILL-SW ' ' WS-DISPLAY-AMT
                 SET WS-DUE-AGENT-STMT  TO TRUE
              END-IF
           END-IF
      
      *    IF (CO-CARRIER = '9')
      *       AND (CO-RESP-NO = '0000016480')
      *       SET END-OF-INPUT TO TRUE
      *    END-IF
      
           .
       0070-EXIT.                                                       
           EXIT.                                                        
      
       0080-WRITE-REMIT-STMT.
       
           IF WS-HEADING
              IF WS-NEW-ACCT
                 PERFORM 0100-PRINT-COVER-SHEET
                                       THRU 0100-EXIT
              END-IF
              WRITE REMIT-STMT-REC         FROM WS-HOLD-HEAD (1)
              WRITE REMIT-STMT-REC         FROM WS-HOLD-HEAD (2)
              WRITE REMIT-STMT-REC         FROM WS-HOLD-HEAD (3)
              WRITE REMIT-STMT-REC         FROM WS-HOLD-HEAD (4)
              MOVE ' ' TO WS-HEADING-SW
           END-IF
      
           WRITE REMIT-STMT-REC            FROM STMT-RECORD
      
           .
       0080-EXIT.                                                       
           EXIT.                                                        
      
       0085-WRITE-DUE-AGENT.
       
           IF WS-HEADING
              IF WS-NEW-ACCT
                 PERFORM 2400-PRINT-VOUCHER
                                       THRU 2400-EXIT
                 PERFORM 0100-PRINT-COVER-SHEET
                                       THRU 0100-EXIT
              END-IF
              WRITE DUE-AGENT-STMT-REC FROM WS-HOLD-HEAD (1)
              WRITE DUE-AGENT-STMT-REC FROM WS-HOLD-HEAD (2)
              WRITE DUE-AGENT-STMT-REC FROM WS-HOLD-HEAD (3)
              WRITE DUE-AGENT-STMT-REC FROM WS-HOLD-HEAD (4)
              MOVE ' '                 TO WS-HEADING-SW
           END-IF
      
           WRITE DUE-AGENT-STMT-REC    FROM STMT-RECORD
      
           .
       0085-EXIT.
           EXIT.                                                        
      
       0090-WRITE-OTHER-STMT.
              
           IF WS-HEADING
              WRITE OTHER-STMT-REC FROM WS-HOLD-HEAD (1)
              WRITE OTHER-STMT-REC FROM WS-HOLD-HEAD (2)
              WRITE OTHER-STMT-REC FROM WS-HOLD-HEAD (3)
              WRITE OTHER-STMT-REC FROM WS-HOLD-HEAD (4)
              MOVE ' ' TO WS-HEADING-SW
           END-IF
      
           WRITE OTHER-STMT-REC FROM STMT-RECORD
      
           .
       0090-EXIT.                                                       
           EXIT.                                                        
      
       0100-PRINT-COVER-SHEET.
      
           SET LAST-PAGE-DETAIL TO TRUE
      
           MOVE '1'                    TO PD-CC
           MOVE 'PDUE'                 TO PD-ID
      
           MOVE +0                     TO STRT
           INSPECT ALPH-DATE TALLYING STRT FOR LEADING ' '

           MOVE ALPH-DATE(STRT + 1:)   TO PD-DATE1
           MOVE CO-CARRIER             TO PD-CARRIER
           MOVE CO-RESP-NO             TO PD-ACCOUNT
           MOVE CO-CUR                 TO PD-CUR
           MOVE CO-OV30                TO PD-OV30
           MOVE CO-OV60                TO PD-OV60
           MOVE CO-OV90                TO PD-OV90
           MOVE ZEROS                  TO PD-OV120
           MOVE CO-END-BAL             TO PD-END-BAL
           MOVE CO-CUR-PMT             TO PD-PMT
           MOVE CO-BAL-FWD             TO PD-BAL-FWD
           MOVE SPACES                 TO PD-USER-SELECT-2
           MOVE SPACES                 TO PD-REPORT-CODE-1
      
           if ws-remit-stmt
              WRITE REMIT-STMT-REC     FROM WS-COVER-SHEET
           else
              write due-agent-stmt-rec from ws-cover-sheet
           end-if

           if not ws-remit-stmt
              go to 0100-bypass-barcode
           end-if

           MOVE SPACES                 TO WS-COVER-SHEET
           PERFORM 2100-BUILD-BARCODE1 THRU 2100-EXIT
           MOVE SAVE-BARCODE1          TO PD-BARCODE1
           PERFORM 2150-BUILD-BARCODE2 THRU 2150-EXIT
           MOVE SAVE-BARCODE2          TO PD-BARCODE2
           if ws-remit-stmt
              WRITE REMIT-STMT-REC     FROM WS-COVER-SHEET
           else
              write due-agent-stmt-rec from ws-cover-sheet
           end-if
      
      *    MOVE SPACES                 TO WS-COVER-SHEET
      *    PERFORM 2150-BUILD-BARCODE2 THRU 2150-EXIT
      *    MOVE SAVE-BARCODE2          TO PD-BARCODE2
      *    WRITE REMIT-STMT-REC            FROM WS-COVER-SHEET
      
           .      
       0100-bypass-barcode.

           MOVE SPACES                 TO WS-COVER-SHEET
060712     if ws-remit-stmt
060712        add 1                    to ws-remit-addr-seq-no
060712        move ws-remit-addr-seq-no
060712                                 to ws-dis-addr-seq-no
060712     else
060712        add 1                    to ws-due-addr-seq-no
060712        move ws-due-addr-seq-no  to ws-dis-addr-seq-no
060712     end-if
060712*    ADD 1                       TO WS-ADDR-SEQ-NO
060712*    MOVE WS-ADDR-SEQ-NO         TO WS-DIS-ADDR-SEQ-NO
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
      

           PERFORM VARYING A1 FROM 1 BY 1 UNTIL A1 > 7
              MOVE WK-ADDR(A1)         TO PD-ADDRESS                           
              if ws-remit-stmt
                 WRITE REMIT-STMT-REC     FROM WS-COVER-SHEET
              else
                 write due-agent-stmt-rec from ws-cover-sheet
              end-if

           END-PERFORM                                                  

           move spaces                 to ws-cover-sheet
           perform varying a1 from 1 by 1 until a1 > +8
              if ws-remit-stmt
                 move due-cso-special-notes (a1)
                                       to pd-spec-notes
                 write remit-stmt-rec  from ws-cover-sheet
              else
                 move due-agt-special-notes (a1)
                                       to pd-spec-notes
                 write due-agent-stmt-rec
                                       from ws-cover-sheet
              end-if
           end-perform                                                  

           if ws-remit-stmt
              move due-cso-comment-1   to pd-spec-notes
              write remit-stmt-rec     from ws-cover-sheet
              move due-cso-comment-2   to pd-spec-notes
              write remit-stmt-rec     from ws-cover-sheet
           else
              move due-agt-comment-1   to pd-spec-notes
              write due-agent-stmt-rec from ws-cover-sheet
              move due-agt-comment-2   to pd-spec-notes
              write due-agent-stmt-rec from ws-cover-sheet
           end-if

           MOVE WS-SAVE-SCAN-REC       TO CID-SCAN-REC
           MOVE BC1-SEQ-NO             TO CID-SCAN-SEQ-NO
           MOVE CO-CARRIER             TO CID-CARRIER
           MOVE CO-GROUPING            TO CID-GROUP
           MOVE CO-RESP-NO             TO CID-FIN-RESP
052912     MOVE SPACES                 TO CID-ACCOUNT
           MOVE CO-CURRENT-END-BAL     TO CID-AMT-DUE
           if ws-remit-stmt
              WRITE SCAN-EXT-RECORD    FROM CID-SCAN-REC
           end-if
      
           .                                                            
       0100-EXIT.                                                       
           EXIT.                                                        
      
       0210-GET-STATE.
      
      *  WE REALLY DON'T NEED THIS PARA BECAUSE THE STATE
      *  IS SEPARATE NOW, BUT I LEFT IT IN ANYWAY
           MOVE CO-ADDR-3              TO WS-SRCH-STATE
      
           PERFORM VARYING S1 FROM +29 BY -1 UNTIL
              (S1 < +1) OR
              ((WS-SRCH-STATE (S1:2) ALPHABETIC) AND
              (WS-SRCH-STATE (S1:2) NOT = SPACES AND LOW-VALUES)
              AND
              (WS-SRCH-STATE (S1 + 1:1) NOT = ' ' AND ',' AND
                       '.' AND LOW-VALUES) AND
              (WS-SRCH-STATE (S1:1) NOT = ' ' AND ',' AND
                       '.' AND LOW-VALUES))
           END-PERFORM
      
           IF S1 NOT < +1
              MOVE WS-SRCH-STATE (S1:2)
                                       TO WS-SAVE-STATE
           ELSE
              MOVE SPACES              TO WS-SAVE-STATE
           END-IF
      
           .
       0210-EXIT.
           EXIT.
      
      
       2100-BUILD-BARCODE1.                                              
      
           MOVE CO-CARR-GROUP          TO BC-CARR
060712     MOVE CO-RESP-NO             TO BC-ACCT
           ADD 1                       TO BC-SEQ-NO
           MOVE 28                     TO AGEB16-LEN
           MOVE BARCODE1               TO AGEB16-INPUT
           MOVE SPACES                 TO AGEB16-OUTPUT
      
           CALL 'AGEB16'   USING AGEB16-LEN
                                 AGEB16-INPUT
                                 AGEB16-OUTPUT
      
           IF AGEB16-LEN = +128
              DISPLAY 'AGEB16 - BARCODE ROUTINE ERROR'
           ELSE
              MOVE AGEB16-OUTPUT       TO SAVE-BARCODE1
           END-IF
      *    DISPLAY  BARCODE1
      
           .
       2100-EXIT.
           EXIT.
      
       2150-BUILD-BARCODE2.
      
           ADD 1                       TO BC1-SEQ-NO
           MOVE 13                     TO AGEB16-LEN
           MOVE BARCODE2               TO AGEB16-INPUT
           MOVE SPACES                 TO AGEB16-OUTPUT
      
           display ' barcode 2 ahl ' barcode2
      
           CALL 'AGEB16' USING AGEB16-LEN
                               AGEB16-INPUT
                               AGEB16-OUTPUT
           IF AGEB16-LEN = +128
              DISPLAY 'AGEB16 - BARCODE ROUTINE ERROR'
           ELSE
              MOVE AGEB16-OUTPUT       TO SAVE-BARCODE2
           END-IF
      
           .
       2150-EXIT.
           EXIT.
      
       2400-PRINT-VOUCHER.

      *    IF CO-CONTROL-PRIMARY = PREV-CO-CONTROL                      
      *       GO TO 2400-EXIT.                                          
                                                                        
           MOVE '1' TO VR-CC                                            
           MOVE 'VOUCHER' TO VR-ID

      ****  The following tells the formdef (DCC021)
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
      
           MOVE CO-CARR-GROUP          TO VR-CARR
           MOVE CO-RESP-NO             TO VR-ACCT
           MOVE CO-CURRENT-END-BAL     TO VR-AMT
           MOVE CO-CSR-CODE            TO VR-CSR

           write DUE-AGENT-STMT-REC    from ws-voucher-record
                                                                        
           MOVE SPACES                 TO ws-voucher-record
           MOVE CO-CONTROL-NAME        TO WK-ADDR(1)
           MOVE CO-ACCT-NAME           TO WK-ADDR(2)
           MOVE CO-ADDR-1              TO WK-ADDR(3)
           MOVE CO-ADDR-2              TO WK-ADDR(4)
           MOVE SPACES                 TO WK-ADDR(5)
           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
              DELIMITED BY '  ' INTO WK-ADDR (5)
           END-STRING
           IF CO-ZIP-PLUS4 = SPACE
              MOVE CO-ZIP-PRIME        TO WK-ADDR(5)(26:5)
           ELSE
              MOVE CO-ZIP              TO WK-ADDR(5)(22:9)
           END-IF
      
           IF WK-ADDR(1) = WK-ADDR(2)
              MOVE SPACES TO WK-ADDR(2)                                 
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
              MOVE WK-ADDR(A1)         TO VR-ADDRESS
              write DUE-AGENT-STMT-REC from ws-voucher-record
           END-PERFORM                                                  

           .                                                            
       2400-EXIT.                                                       
           EXIT.                                                        
      
       4000-CLOSE-FILES.
      
           CLOSE BILLING-STATEMENTS ERCOMP SCAN-EXT
              OTHER-STMT REMIT-STMT DUE-AGENT-STMT
      
           .
       4000-EXIT.
           EXIT.
      
       ABEND-PGM.
                           COPY ELCABEND.
                                                                        
