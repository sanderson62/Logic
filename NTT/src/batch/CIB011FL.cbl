       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIB011FL.
       AUTHOR.     PABLO.
       DATE-COMPILED.

      *REMARKS.
      *        THIS PROGRAM READS THE REPORT FILE OUT OF EL562 
      *    AND PLACES NAMD AND ADDRESSES IN FRONT OF THE STATEMENTS.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 091421  CR2021090900001  PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                                                                        
           SELECT ERCOMP    ASSIGN TO ERCOMP
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS CO-CONTROL-PRIMARY
                  FILE STATUS IS ERCOMP-FILE-STATUS.

           SELECT BILLING-STATEMENTS ASSIGN TO SYS010.

           SELECT DISK-DATE          ASSIGN TO SYS019.

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

       FD  DISK-DATE                                                    
           COPY ELCDTEFD.                                               
                                                                        
       FD  S-STATEMENTS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  S-STMT-REC      PIC X(133).
                                                                        
       WORKING-STORAGE SECTION.                                         
       copy "ctypes.cpy".

       77  WS-HEADING-SW               PIC X   VALUE SPACES.
           88  WS-HEADING                  VALUE 'Y'.
       77  WS-S-STMT-SW                PIC X   VALUE SPACES.
           88  WS-S-STMT                   VALUE 'Y'.
       77  WS-NEW-ACCT-SW              PIC X   VALUE SPACES.
           88  WS-NEW-ACCT                 VALUE 'Y'.
       77  WS-EOF-SW                   PIC X   VALUE SPACES.
           88  END-OF-INPUT                VALUE 'Y'.
       77  ERCOMP-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-IN-CNT                   PIC 9(7)  VALUE ZEROS.
       77  WS-OUT-CNT                  PIC 9(7)  VALUE ZEROS.
       77  WS-STMT-OWNER               PIC X(4)  VALUE SPACES.
       77  WS-REPORT-GID               PIC X(12) VALUE SPACES.

       01  WS-COMPARE-KEY.
           05  WS-CK-CARRIER           PIC X.
           05  WS-CK-GROUP             PIC X(6).
           05  WS-CK-RESP              PIC X(10).
           05  WS-CK-ACCOUNT           PIC X(10).
       01  WS-PREV-KEY                 PIC X(27)  VALUE LOW-VALUES.
       01  WS-MISC.
           05  WS-HOLD-HEAD  OCCURS 4  PIC X(133).

       01  WS-RGID-RECORD.
           05  WS-USER-ID              PIC X(4)   VALUE SPACES.
           05  WS-REPORT-GROUP-ID      PIC X(12)  VALUE SPACES.
           05  WS-SEQ-NO               PIC 9(7)   VALUE ZEROS.
           05  WS-RGID-STMT-RECORD     PIC X(133) VALUE SPACES.

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
           05  CP1-ADDRESS OCCURS 5.
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

           OPEN OUTPUT S-STATEMENTS

           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           MOVE RUN-DATE               TO WS-WORK-DATE
           MOVE SPACES                 TO WS-COVER-PAGES

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

           PERFORM 0080-WRITE-S-STMT   THRU 0080-EXIT

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

           IF CO-BILL-SW = 'S'
              SET WS-S-STMT  TO TRUE
           ELSE
              MOVE ' '                 TO WS-S-STMT-SW
           END-IF

PEMTST     IF (CO-STMT-OWNER = SPACES OR LOW-VALUES)
PEMTST        AND (CO-REPORT-GROUP-ID = SPACES OR LOW-VALUES)
PEMTST        MOVE ' '                 TO WS-S-STMT-SW
PEMTST     END-IF

           .
       0070-EXIT.                                                       
           EXIT.                                                        

       0080-WRITE-S-STMT.
       
           IF WS-HEADING
              IF WS-NEW-ACCT
                 PERFORM 0100-PRINT-COVER-PAGES
                                       THRU 0100-EXIT
              END-IF
              ADD 1                    TO WS-SEQ-NO
              MOVE WS-STMT-OWNER       TO WS-USER-ID
              MOVE WS-REPORT-GID       TO WS-REPORT-GROUP-ID
              MOVE WS-HOLD-HEAD (1)    TO WS-RGID-STMT-RECORD
              WRITE S-STMT-REC         FROM WS-RGID-STMT-RECORD
              ADD 1                    TO WS-SEQ-NO
              MOVE WS-HOLD-HEAD (2)    TO WS-RGID-STMT-RECORD
              WRITE S-STMT-REC         FROM WS-RGID-STMT-RECORD
              ADD 1                    TO WS-SEQ-NO
              MOVE WS-HOLD-HEAD (3)    TO WS-RGID-STMT-RECORD
              WRITE S-STMT-REC         FROM WS-RGID-STMT-RECORD
              ADD 1                    TO WS-SEQ-NO
              MOVE WS-HOLD-HEAD (4)    TO WS-RGID-STMT-RECORD
              WRITE S-STMT-REC         FROM WS-RGID-STMT-RECORD
              MOVE ' ' TO WS-HEADING-SW
           END-IF

           ADD 1                       TO WS-SEQ-NO
           MOVE WS-STMT-OWNER          TO WS-USER-ID
           MOVE WS-REPORT-GID          TO WS-REPORT-GROUP-ID
           MOVE STMT-RECORD            TO WS-RGID-STMT-RECORD
           WRITE S-STMT-REC            FROM WS-RGID-STMT-RECORD

           .
       0080-EXIT.                                                       
           EXIT.                                                        

       0100-PRINT-COVER-PAGES.                                          

           MOVE SPACES                 TO WS-COVER-PAGE-1
           MOVE '1'                    TO CP1-CC
           MOVE 'ADDRESS'              TO CP1-ID
           ADD 1 TO WS-SEQ-NO
           MOVE WS-COVER-PAGE-1        TO WS-RGID-STMT-RECORD
           WRITE S-STMT-REC            FROM WS-RGID-STMT-RECORD

           MOVE CO-ACCT-NAME           TO CP1-ADDR (1)
           MOVE CO-MAIL-NAME           TO CP1-ADDR (2)
           MOVE CO-ADDR-1              TO CP1-ADDR (3)
           MOVE CO-ADDR-2              TO CP1-ADDR (4)
           STRING
              CO-ADDR-CITY ', '
              CO-ADDR-STATE ' '
              CO-ZIP
              DELIMITED BY '  ' INTO CP1-ADDR (5)
           END-STRING

           PERFORM 2 TIMES
              IF CP1-ADDR (1) = SPACES
                 MOVE CP1-ADDR (2)     TO CP1-ADDR (1)
                 MOVE CP1-ADDR (3)     TO CP1-ADDR (2)
                 MOVE CP1-ADDR (4)     TO CP1-ADDR (3)
                 MOVE CP1-ADDR (5)     TO CP1-ADDR (4)
                 MOVE SPACES           TO CP1-ADDR (5)
              END-IF

              IF CP1-ADDR (2) = SPACES
                 MOVE CP1-ADDR (3)     TO CP1-ADDR (2)
                 MOVE CP1-ADDR (4)     TO CP1-ADDR (3)
                 MOVE CP1-ADDR (5)     TO CP1-ADDR (4)
                 MOVE SPACES           TO CP1-ADDR (5)
              END-IF

              IF CP1-ADDR (3) = SPACES
                 MOVE CP1-ADDR (4)     TO CP1-ADDR (3)
                 MOVE CP1-ADDR (5)     TO CP1-ADDR (4)
                 MOVE SPACES           TO CP1-ADDR (5)
              END-IF

              IF CP1-ADDR (4) = SPACES
                 MOVE CP1-ADDR (5)     TO CP1-ADDR (4)
                 MOVE SPACES           TO CP1-ADDR (5)
              END-IF
           END-PERFORM

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > 5
              ADD 1                    TO WS-SEQ-NO
              MOVE CP1-ADDRESS (S1)    TO WS-RGID-STMT-RECORD
              WRITE S-STMT-REC         FROM WS-RGID-STMT-RECORD
           END-PERFORM

           .                                                            
       0100-EXIT.                                                       
           EXIT.                                                        

       4000-CLOSE-FILES.

           CLOSE BILLING-STATEMENTS ERCOMP
              S-STATEMENTS

           .
       4000-EXIT.
           EXIT.

       ABEND-PGM.
                           COPY ELCABEND.
                                                                        
