       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCC012.
       AUTHOR.     PABLO.
       DATE-COMPILED.

      *REMARKS.
      *        This program is a copy of cib012 and is for dcc
      *        THIS PROGRAM READS THE FICHE FILE OUT OF EL562
      *        AND SEPARATES THE REMIT STATEMENTS FROM ALL THE OTHERS
      *        AND PLACES COVER SHEET INFO  IN FRONT OF EACH REMIT
      *        STMT. THE NON REMIT STATEMENTS JUST GET PASSED TO
      *        THE NEXT STEP IN THE JOB.
      *        REMIT STMTS ARE DEFINED AS FOLLOWS
      *            IF (CO-BILL-SW = 'B' OR 'C')
      *                AND (CO-CURRENT-END-BAL IS POSITIVE)
      
071312******************************************************************
071312*                   C H A N G E   L O G
071312*
071312* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
071312*-----------------------------------------------------------------
071312*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
071312* EFFECTIVE    NUMBER
071312*-----------------------------------------------------------------
071312* 071312  CR2010082600001  PEMA  NEW PROGRAM
101212* 101212  IR2012100500001  PEMA  ADD CARRIER CODE TO COVERSHEET
102512* 102512  CR2012040900002  PEMA  ADD USER NOTES CAPABILITY
071312******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                                                                        
           SELECT ERCOMP    ASSIGN TO ERCOMP
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS CO-CONTROL-PRIMARY
                  FILE STATUS IS ERCOMP-FILE-STATUS.

102512     SELECT FORMDEFS  ASSIGN TO FORMDEFS
102512            ORGANIZATION IS INDEXED
102512            ACCESS IS DYNAMIC
102512            RECORD KEY IS fd-form-key
102512            FILE STATUS IS FORMDEFS-FILE-STATUS.

           SELECT ERACCT    ASSIGN TO ERACCT
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS AM-CONTROL-PRIMARY
                  FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT BILLING-STATEMENTS ASSIGN TO SYS010.

           SELECT DISK-DATE          ASSIGN TO SYS019.

           SELECT OTHER-STATEMENTS   ASSIGN TO SYS011.

           SELECT REMIT-STMTS1       ASSIGN TO SYS012.

           SELECT REMIT-STMTS2       ASSIGN TO SYS013.

           SELECT SCAN-EXT         ASSIGN TO SCANOT
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

102512 FD  FORMDEFS.
102512 01  FORM-DEF-IN-REC.
102512     05  fd-form-key.
102512         10  fd-form-name        pic x(10).
102512         10  fd-form-month       pic 99.
102512     05  f                       pic x(888).

       FD  ERACCT.
                                       COPY ERCACCT.

       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.
                                                                        
       FD  OTHER-STATEMENTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  OTHER-STMT-REC              PIC X(133).

       FD  REMIT-STMTS1
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REMIT-STMT-REC1                 PIC X(133).

       FD  REMIT-STMTS2
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  REMIT-STMT-REC2                 PIC X(133).

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
       77  WS-NEW-ACCT-SW              PIC X   VALUE SPACES.
           88  WS-NEW-ACCT                 VALUE 'Y'.
       77  WS-EOF-SW                   PIC X   VALUE SPACES.
           88  END-OF-INPUT                VALUE 'Y'.
       77  ERCOMP-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
102512 77  FORMDEFS-FILE-STATUS        PIC XX  VALUE LOW-VALUES.
       77  ERACCT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
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
       77  WS-ADDR-SEQ-NO-C135         PIC 9(7)  VALUE ZEROS.
       77  WS-ADDR-SEQ-NO-C246         PIC 9(7)  VALUE ZEROS.
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

102512                                 COPY FORMREC.                                                                        
102512
102512 01  remit-wso101.
102512     03  wso101-key.
102512         05  wso101-name         pic x(10).
102512         05  wso101-month        pic 99.
102512     03  wso101-desc             pic x(30).
102512     03  wso101-special-notes occurs 8
102512                                 pic x(75).
102512     03  wso101-comment-1        pic x(95).
102512     03  wso101-comment-2        pic x(95).
102512     03  filler                  pic x(68).
102512
102512 01  remit-wso102.
102512     03  wso102-key.
102512         05  wso102-name         pic x(10).
102512         05  wso102-month        pic 99.
102512     03  wso102-desc             pic x(30).
102512     03  wso102-special-notes occurs 8
102512                                 pic x(75).
102512     03  wso102-comment-1        pic x(95).
102512     03  wso102-comment-2        pic x(95).
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

       01  FILLER.
           05  ws-ahl-pd-date1         PIC X(18)   VALUE SPACE.
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
           05  bc2-fill            pic x value '0'.
           05  BC1-SEQ-NO          PIC 9(6)    VALUE ZERO.


                                       COPY ELCDATE.
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

102512     OPEN INPUT FORMDEFS
102512     IF FORMDEFS-FILE-STATUS NOT = '00' AND '97'
102512        DISPLAY ' ERROR - FORMDEFS - OPEN ' FORMDEFS-FILE-STATUS
102512        PERFORM ABEND-PGM
102512     END-IF

           OPEN INPUT ERACCT
           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           OPEN OUTPUT OTHER-STATEMENTS SCAN-EXT
                       REMIT-STMTS1 REMIT-STMTS2

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
           move 1                      to sav-comp-indicator
                                          bc2-fill

           MOVE ZEROS                  TO CID-SCAN-SEQ-NO
           MOVE ZEROS                  TO CID-AMT-DUE
           MOVE ';'                    TO SR-DEL1
                                          SR-DEL2
                                          SR-DEL3
                                          SR-DEL4
                                          SR-DEL5
           MOVE CID-SCAN-REC           TO WS-SAVE-SCAN-REC

102512     move 'DCO101'               to fd-form-name
102512
102512     move run-mo                 to fd-form-month
102512*    move 01                     to fd-form-month
102512
102512     read FORMDEFS
102512     evaluate true
102512        when formdefs-file-status = '00'
102512           move form-def-in-rec  to remit-wso101
102512        when formdefs-file-status = '23'
102512           move 01               to fd-form-month
102512           read FORMDEFS
102512           if formdefs-file-status = '00'
102512              move form-def-in-rec
102512                                 to remit-wso101
102512           else
102512              move spaces        to remit-wso101
102512           end-if
102512        when other
102512           move spaces           to remit-wso101
102512     end-evaluate
102512
102512     move 'DCO102'               to fd-form-name
102512
102512     move run-mo                 to fd-form-month
102512*    move 01                     to fd-form-month
102512
102512     read FORMDEFS
102512     evaluate true
102512        when formdefs-file-status = '00'
102512           move form-def-in-rec  to remit-wso102
102512        when formdefs-file-status = '23'
102512           move 01               to fd-form-month
102512           read FORMDEFS
102512           if formdefs-file-status = '00'
102512              move form-def-in-rec
102512                                 to remit-wso102
102512           else
102512              move spaces        to remit-wso102
102512           end-if
102512        when other
102512           move spaces           to remit-wso102
102512     end-evaluate
102512
102512     close FORMDEFS              

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

           IF STMT-RECORD (4:17) = 'PLEASE RETURN ONE'
              SET LAST-PAGE-SUMMARY    TO TRUE
           END-IF

           IF WS-REMIT-STMT
              PERFORM 0080-WRITE-REMIT-STMT THRU 0080-EXIT
           ELSE
              PERFORM 0090-WRITE-OTHER-STMT THRU 0090-EXIT
           END-IF

           PERFORM 0040-READ-INPUT     THRU 0040-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-PROCESS-HEADING.

           IF WS-REMIT-STMT
               DISPLAY ' YES REMIT STMT ' CO-CARRIER ' ' CO-RESP-NO
                  ' ' CO-ACCOUNT
              MOVE SPACES              TO WS-COVER-SHEET
              IF LAST-PAGE-SUMMARY
                 DISPLAY ' YES LAST PAGE SUMMARY ' CO-CARRIER ' '
                   CO-RESP-NO ' ' CO-ACCOUNT
                 SET LAST-PAGE-DETAIL  TO TRUE
                 MOVE 'C'              TO PD-CC
                 MOVE SAVE-BARCODE2    TO PD-BARCODE2
              END-IF
              IF WS-COVER-SHEET NOT = SPACES
                 if co-carrier = '1' or '3' or '5'
                    WRITE REMIT-STMT-REC1 FROM WS-COVER-SHEET
                 else
                    WRITE REMIT-STMT-REC2 FROM WS-COVER-SHEET
                 end-if
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

           IF WS-HOLD-HEAD (1) (61:13) = 'OVERALL RECAP'
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

           IF (CO-BILL-SW = 'B' OR 'C')
              AND (CO-CURRENT-END-BAL IS POSITIVE)
              MOVE CO-CURRENT-END-BAL TO WS-DISPLAY-AMT
              DISPLAY ' REMIT STMT ' CO-CARRIER ' ' CO-RESP-NO ' '
                 CO-ACCOUNT ' ' CO-BILL-SW ' ' WS-DISPLAY-AMT
              SET WS-REMIT-STMT  TO TRUE
           ELSE
              MOVE ' '                 TO WS-REMIT-STMT-SW
              MOVE CO-CURRENT-END-BAL TO WS-DISPLAY-AMT
              DISPLAY ' NOT REMIT STMT ' CO-CARRIER ' ' CO-RESP-NO ' '
                 CO-ACCOUNT ' ' CO-BILL-SW ' ' WS-DISPLAY-AMT
           END-IF

           .
       0070-EXIT.                                                       
           EXIT.                                                        

       0080-WRITE-REMIT-STMT.
       
           IF WS-HEADING
              IF WS-NEW-ACCT
                 PERFORM 0100-PRINT-COVER-SHEET
                                       THRU 0100-EXIT
              END-IF
              if co-carrier = '1' or '3' or '5'
                 WRITE REMIT-STMT-REC1      FROM WS-HOLD-HEAD (1)
                 WRITE REMIT-STMT-REC1      FROM WS-HOLD-HEAD (2)
                 WRITE REMIT-STMT-REC1      FROM WS-HOLD-HEAD (3)
                 WRITE REMIT-STMT-REC1      FROM WS-HOLD-HEAD (4)
              else
                 WRITE REMIT-STMT-REC2      FROM WS-HOLD-HEAD (1)
                 WRITE REMIT-STMT-REC2      FROM WS-HOLD-HEAD (2)
                 WRITE REMIT-STMT-REC2      FROM WS-HOLD-HEAD (3)
                 WRITE REMIT-STMT-REC2      FROM WS-HOLD-HEAD (4)
              end-if
              MOVE ' ' TO WS-HEADING-SW
           END-IF

           if co-carrier = '1' or '3' or '5'
              WRITE REMIT-STMT-REC1        FROM STMT-RECORD
           else
              WRITE REMIT-STMT-REC2        FROM STMT-RECORD
           end-if

           .
       0080-EXIT.                                                       
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
           PERFORM 0200-GET-ERACCT     THRU 0200-EXIT

           MOVE '1'                    TO PD-CC
           MOVE 'PDUE'                 TO PD-ID

           MOVE +0                     TO STRT
           INSPECT ALPH-DATE TALLYING STRT FOR LEADING ' '
           MOVE ALPH-DATE(STRT + 1:)   TO PD-DATE1

101212*    if co-carrier = '1' or '3' or '5'
101212*       move '1'                 to pd-carrier
101212*    else
101212*       move '2'                 to pd-carrier
101212*    end-if
           move co-carrier             to pd-carrier
           MOVE CO-ACCOUNT             TO PD-ACCOUNT
           MOVE CO-CUR                 TO PD-CUR
           MOVE CO-OV30                TO PD-OV30
           MOVE CO-OV60                TO PD-OV60
           MOVE CO-OV90                TO PD-OV90
           MOVE CO-END-BAL             TO PD-END-BAL
           MOVE CO-CUR-PMT             TO PD-PMT
           MOVE CO-BAL-FWD             TO PD-BAL-FWD
           IF WS-USER-SELECT-2 NOT = SPACES
              STRING 'CC: ' WS-USER-SELECT-2 DELIMITED BY SIZE
                 INTO PD-USER-SELECT-2
              END-STRING
           ELSE
              MOVE SPACES              TO PD-USER-SELECT-2
           END-IF
           IF WS-REPORT-CODE-1 NOT = SPACES
              MOVE WS-REPORT-CODE-1    TO PD-REPORT-CODE-1
           ELSE
              MOVE SPACES              TO PD-REPORT-CODE-1
           END-IF

           if co-carrier = '1' or '3' or '5'
              WRITE REMIT-STMT-REC1    FROM WS-COVER-SHEET
           else
              WRITE REMIT-STMT-REC2    FROM WS-COVER-SHEET
           end-if

           MOVE SPACES                 TO WS-COVER-SHEET
           PERFORM 2100-BUILD-BARCODE1 THRU 2100-EXIT
           MOVE SAVE-BARCODE1          TO PD-BARCODE1
           PERFORM 2150-BUILD-BARCODE2 THRU 2150-EXIT
           MOVE SAVE-BARCODE2          TO PD-BARCODE2
           if co-carrier = '1' or '3' or '5'
              WRITE REMIT-STMT-REC1    FROM WS-COVER-SHEET
           else
              WRITE REMIT-STMT-REC2    FROM WS-COVER-SHEET
           end-if

           MOVE SPACES                 TO WS-COVER-SHEET
           if co-carrier = '1' or '3' or '5'
              ADD 1                    TO WS-ADDR-SEQ-NO-C135
              MOVE WS-ADDR-SEQ-NO-c135 TO WS-DIS-ADDR-SEQ-NO
           else
              ADD 1                    TO WS-ADDR-SEQ-NO-C246
              MOVE WS-ADDR-SEQ-NO-c246 TO WS-DIS-ADDR-SEQ-NO
           end-if
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
              if co-carrier = '1' or '3' or '5'
                 WRITE REMIT-STMT-REC1 FROM WS-COVER-SHEET
              else
                 WRITE REMIT-STMT-REC2 FROM WS-COVER-SHEET
              end-if
           END-PERFORM                                                  

102512     move spaces                 to ws-cover-sheet
102512     PERFORM VARYING A1 FROM 1 BY 1 UNTIL A1 > 8
102512        if co-carrier = '1' or '3' or '5'
102512           move wso101-special-notes (a1)
102512                                 to pd-spec-notes
102512           WRITE REMIT-STMT-REC1 FROM WS-COVER-SHEET
102512        else
102512           move wso102-special-notes (a1)
102512                                 to pd-spec-notes
102512           WRITE REMIT-STMT-REC2 FROM WS-COVER-SHEET
102512        end-if
102512     END-PERFORM                                                  
102512
102512     move spaces                 to ws-cover-sheet
102512     if co-carrier = '1' or '3' or '5'
102512        move wso101-comment-1    to pd-spec-notes
102512        WRITE REMIT-STMT-REC1    FROM WS-COVER-SHEET
102512        move wso101-comment-2    to pd-spec-notes
102512        WRITE REMIT-STMT-REC1    FROM WS-COVER-SHEET
102512     else
102512        move wso102-comment-1    to pd-spec-notes
102512        WRITE REMIT-STMT-REC2    FROM WS-COVER-SHEET
102512        move wso102-comment-2    to pd-spec-notes
102512        WRITE REMIT-STMT-REC2    FROM WS-COVER-SHEET
102512     end-if

           MOVE WS-SAVE-SCAN-REC       TO CID-SCAN-REC
           MOVE BC1-SEQ-NO             TO CID-SCAN-SEQ-NO
           MOVE CO-CARRIER             TO CID-CARRIER
           MOVE CO-GROUPING            TO CID-GROUP
           MOVE CO-RESP-NO             TO CID-FIN-RESP
           MOVE CO-ACCOUNT             TO CID-ACCOUNT
           MOVE CO-CURRENT-END-BAL     TO CID-AMT-DUE
           WRITE SCAN-EXT-RECORD       FROM CID-SCAN-REC

           .                                                            
       0100-EXIT.                                                       
           EXIT.                                                        

       0200-GET-ERACCT.

           MOVE SPACES                 TO WS-ACCOUNT-FOUND-SW
                                          WS-USER-SELECT-2
                                          WS-REPORT-CODE-1

           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY        
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD
           MOVE CO-CARRIER             TO AM-CARRIER
           MOVE CO-GROUPING            TO AM-GROUPING
           MOVE CO-ACCOUNT             TO AM-ACCOUNT
           MOVE LOW-VALUES             TO AM-EXPIRATION-DT

           MOVE CO-ADDR-STATE          TO AM-STATE

           MOVE AM-CONTROL-A           TO WS-SAVE-ERACCT-KEY
           START ERACCT KEY >= AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACCT - START ' ERACCT-FILE-STATUS
                 ' KEY=' AM-CONTROL-PRIMARY (2:19)
              PERFORM ABEND-PGM
           END-IF

           MOVE SPACES                 TO WS-DATE-RANGE-FOUND-SW
           PERFORM UNTIL DATE-RANGE-FOUND
              READ ERACCT NEXT
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERACCT - READ ' ERACCT-FILE-STATUS
                    ' KEY=' AM-CONTROL-PRIMARY (2:19)
                 PERFORM ABEND-PGM
              END-IF
              
              IF (AM-CONTROL-A = WS-SAVE-ERACCT-KEY)
                 SET FOUND-ACCOUNT        TO TRUE
                 MOVE AM-USER-SELECT-2    TO WS-USER-SELECT-2
                 MOVE AM-REPORT-CODE-1    TO WS-REPORT-CODE-1
              ELSE
                 SET DATE-RANGE-FOUND     TO TRUE
                 DISPLAY ' ERACCT = ' AM-CONTROL-PRIMARY (2:19)
                    ' ERCOMP = ' CO-CONTROL-PRIMARY (2:27)
                    ' STATE = ' WS-SAVE-STATE
              END-IF
           END-PERFORM

           .
       0200-EXIT.
           EXIT.

       2100-BUILD-BARCODE1.                                              

           MOVE CO-CARR-GROUP          TO BC-CARR
           MOVE CO-ACCOUNT             TO BC-ACCT
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
PEMTST*    DISPLAY  BARCODE1

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

       4000-CLOSE-FILES.

           CLOSE BILLING-STATEMENTS ERCOMP ERACCT SCAN-EXT
              OTHER-STATEMENTS REMIT-STMTS1 REMIT-STMTS2

           .
       4000-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.

       ABEND-PGM.
                           COPY ELCABEND.
                                                                        
