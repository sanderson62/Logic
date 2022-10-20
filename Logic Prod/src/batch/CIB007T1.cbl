       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIB007T1
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                                                                        
           SELECT ERCOMP    ASSIGN TO ERCOMP
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS CO-CONTROL-PRIMARY
                  FILE STATUS IS ERCOMP-FILE-STATUS.

           SELECT ERCOBI    ASSIGN TO ERCOBI
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS BL-CONTROL-PRIMARY
                  FILE STATUS IS ERCOBI-FILE-STATUS.

           SELECT BILLING-STATEMENTS ASSIGN TO SYS010.

           SELECT DISK-DATE          ASSIGN TO SYS019.

           SELECT OTHER-STATEMENTS   ASSIGN TO SYS011.

           SELECT S-STATEMENTS       ASSIGN TO SYS012.

       DATA DIVISION.                                                   

       FILE SECTION.                                                    
                                                                        
       FD  BILLING-STATEMENTS                                           
           LABEL RECORDS ARE STANDARD                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS.                                    
       01  STMT-RECORD       PIC X(133).                                
                                                                        
       FD  ERCOMP.                                                      
           COPY ERCCOMP.                                                

       FD  ERCOBI.
           COPY ERCCOBI.

       FD  DISK-DATE                                                    
           COPY ELCDTEFD.                                               
                                                                        
       FD  OTHER-STATEMENTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  OTHER-STMT-REC    PIC X(133).

       FD  S-STATEMENTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  S-STMT-REC      PIC X(133).
                                                                        

       WORKING-STORAGE SECTION.                                         
       copy "ctypes.cpy".

       77  WS-HEADING-SW             PIC X   VALUE SPACES.
           88  WS-HEADING                 VALUE 'Y'.
       77  WS-S-STMT-SW              PIC X   VALUE SPACES.
           88  WS-S-STMT                  VALUE 'Y'.
       77  WS-NEW-ACCT-SW            PIC X   VALUE SPACES.
           88  WS-NEW-ACCT                 VALUE 'Y'.
       77  WS-EOF-SW                   PIC X   VALUE SPACES.
           88  END-OF-INPUT                 VALUE 'Y'.
       77  ERCOMP-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERCOBI-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-IN-CNT                   PIC 9(7)  VALUE ZEROS.
       77  WS-OUT-CNT                  PIC 9(7)  VALUE ZEROS.
       77  S1                         PIC S9(5)  VALUE +0.
       01  WS-COMPARE-KEY.
           05  WS-CK-CARRIER           PIC X.
           05  WS-CK-GROUP             PIC X(6).
           05  WS-CK-RESP              PIC X(10).
           05  WS-CK-ACCOUNT           PIC X(10).
       01  WS-PREV-KEY                 PIC X(27)  VALUE LOW-VALUES.
       01  WS-MISC.
           05  WS-HOLD-HEAD  OCCURS 4  PIC X(133).

       01  WS-COVER-PAGES.
           05  WS-COVER-PAGE-1.
               10  CP1-CC              PIC X.
               10  CP1-ID              PIC X(10).
               10  CP1-UID             PIC X(4).
               10  CP1-RGID            PIC X(12).
               10  CP1-CHECK-HANDLE    PIC X(8).
           05  CP1-SPECIAL-INSTRUCTIONS OCCURS 5.
               10  FILLER              PIC X.
               10  CP1-SI              PIC X(70).
           05  CP1-ADDRESS OCCURS 4.
               10  FILLER              PIC X.
               10  CP1-ADDR            PIC X(40).

       01  FILLER.
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
           05  WK-ADDR OCCURS 6 TIMES PIC X(30).                        
           05  WK-AMT              PIC ZZZ,ZZZ,ZZZ.99/.                 
                                                                        
       01  FILLER               COMP-3.                                 
           05  STRT             PIC S9(3)   VALUE +0.                   
           05  S1               PIC S9(3)   VALUE +0.                   
           05  WK1              PIC S9(7)   VALUE +0.                   
           05  WK2              PIC S9(7)   VALUE +0.                   
                                                                       
                                                                        
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
                                                                        
           OPEN OUTPUT OTHER-STATEMENTS
                       S-STATEMENTS

           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE SPACES                 TO WS-COVER-PAGES

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
              SET WS-HEADING TO TRUE
              PERFORM 0060-PROCESS-HEADING THRU 0060-EXIT
           END-IF

           IF WS-S-STMT
              PERFORM 0080-WRITE-S-STMT THRU 0080-EXIT
           ELSE
              PERFORM 0090-WRITE-OTHER-STMT THRU 0090-EXIT
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

           IF CO-BILL-SW = 'S'
              SET WS-S-STMT  TO TRUE
           ELSE
              MOVE ' '                 TO WS-S-STMT-SW
           END-IF

           .
       0070-EXIT.                                                       
           EXIT.                                                        

       0080-WRITE-S-STMT.
       
           IF WS-HEADING
              IF WS-NEW-ACCT
                 PERFORM 0100-PRINT-COVER-PAGES
                                       THRU 0100-EXIT
              END-IF
              WRITE S-STMT-REC FROM WS-HOLD-HEAD (1)
              WRITE S-STMT-REC FROM WS-HOLD-HEAD (2)
              WRITE S-STMT-REC FROM WS-HOLD-HEAD (3)
              WRITE S-STMT-REC FROM WS-HOLD-HEAD (4)
              MOVE ' ' TO WS-HEADING-SW
           END-IF

           WRITE S-STMT-REC FROM STMT-RECORD

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


       0100-PRINT-COVER-PAGES.                                          

           PERFORM 0110-GET-ERCOBI     THRU 0110-EXIT
                                                                        
           MOVE '1'                    TO CP1-CC
           MOVE 'COVER PAGE'           TO CP1-ID
           MOVE BL-STMT-OWNER          TO CP1-UID
           MOVE BL-REPORT-GROUP-ID     TO CP1-RGID
           IF BL-CHECK-HANDLING = '2'
              MOVE 'SEPARATE'          TO CP1-CHECK-HANDLE
           ELSE
              MOVE 'NET'               TO CP1-CHECK-HANDLE
           END-IF

           WRITE NEW-STMT-REC          FROM WS-COVER-PAGE1

           MOVE BL-SI-LINE-1           TO CP1-SI (1)
           MOVE BL-SI-LINE-2           TO CP1-SI (2)
           MOVE BL-SI-LINE-3           TO CP1-SI (3)
           MOVE BL-SI-LINE-4           TO CP1-SI (4)
           MOVE BL-SI-LINE-5           TO CP1-SI (5)

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > 5
              WRITE NEW-STMT-REC   FROM CP1-SPECIAL-INSTRUCTIONS (S1)
           END-PERFORM


           MOVE BL-CONTACT-NAME        TO CP1-ADDR (1)
           MOVE BL-ADDR1               TO CP1-ADDR (2)
           MOVE BL-ADDR2               TO CP1-ADDR (3)
           STRING BL-CITY ' ' BL-STATE ' ' BL-ZIP
              DELIMITED BY '  ' INTO CP1-ADDR (4)

           PERFORM 2 TIMES
              IF CP1-ADDR (1) = SPACES
                 MOVE CP1-ADDR (2)     TO CP1-ADDR (1)
                 MOVE CP1-ADDR (3)     TO CP1-ADDR (2)
                 MOVE CP1-ADDR (4)     TO CP1-ADDR (3)
                 MOVE SPACES           TO CP1-ADDR (4)
              END-IF

              IF CP1-ADDR (2) = SPACES
                 MOVE CP1-ADDR (3)     TO CP1-ADDR (2)
                 MOVE CP1-ADDR (4)     TO CP1-ADDR (3)
                 MOVE SPACES           TO CP1-ADDR (4)
              END-IF

              IF CP1-ADDR (3) = SPACES
                 MOVE CP1-ADDR (4)     TO CP1-ADDR (3)
                 MOVE SPACES           TO CP1-ADDR (4)
              END-IF
           END-PERFORM

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > 4
              WRITE NEW-STMT-REC       FROM CP1-ADDRESS (S1)
           END-PERFORM

           .                                                            
       0100-EXIT.                                                       
           EXIT.                                                        

       0110-GET-ERCOBI.

           MOVE DTE-CLASIC-COMPANY-CD  TO BL-COMPANY-CD

           MOVE CO-STMT-OWNER          TO BL-STMT-OWNER
           MOVE CO-REPORT-GROUP-ID     TO BL-REPORT-GROUP-ID

           READ ERCOBI
           IF ERCOBI-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCOBI - READ ' ERCOBI-FILE-STATUS
                 ' KEY=' BL-CONTROL-PRIMARY (2:16)
              MOVE SPACES              TO WS-STMT-OWNER
                                          WS-REPORT-GROUP-ID
      *       PERFORM ABEND-PGM
           ELSE
              MOVE BL-STMT-OWNER       TO WS-STMT-OWNER
              MOVE BL-REPORT-GROUP-ID  TO WS-
           END-IF

           MOVE CO-CONTROL-PRIMARY     TO PREV-CO-CONTROL

           IF CO-BILL-SW = 'S'
              SET WS-S-STMT            TO TRUE
           ELSE
              MOVE ' '                 TO WS-S-STMT-SW
           END-IF

           .
       0110-EXIT.                                                       
           EXIT.                                                        

       4000-CLOSE-FILES.

           CLOSE BILLING-STATEMENTS ERCOMP
              OTHER-STATEMENTS S-STATEMENTS

           .
       4000-EXIT.
           EXIT.

       ABEND-PGM.
                           COPY ELCABEND.
                                                                        
