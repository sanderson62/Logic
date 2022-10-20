       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIB011G
       AUTHOR.     PABLO.
       DATE-COMPILED.

      *REMARKS.
      *        THIS PROGRAM READS THE REPORT FILE OUT OF ECS063 (SYS020)
      *    AND SEPARATES THE BILL SW "S" STATEMENTS FROM ALL THE OTHERS
      *        AND PLACES 2 COVER SHEETS IN FRONT OF EACH S STATEMENT.
      *        THE NON S STATEMENTS JUST GET PASSED TO THE NEXT STEP
      *   THIS PROGRAM WAS COPIED FROM CIB011, IT DOES THE SAME THING
      *    WITH THE EL562 STATEMENTS. SO IF YOU MAKE CHANGES TO THIS
      *    YOU MAY WANT TO CHECK CIB011
030510******************************************************************
030510*                   C H A N G E   L O G
030510*
030510* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
030510*-----------------------------------------------------------------
030510*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
030510* EFFECTIVE    NUMBER
030510*-----------------------------------------------------------------
030510* 030510  CR2009061500001  PEMA  NEW PROGRAM
030510******************************************************************
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
       01  S-STMT-REC      PIC X(156).
                                                                        

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
       77  ERCOBI-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-IN-CNT                   PIC 9(7)  VALUE ZEROS.
       77  WS-OUT-CNT                  PIC 9(7)  VALUE ZEROS.
       77  WS-STMT-OWNER               PIC X(4)  VALUE SPACES.
       77  WS-REPORT-GID               PIC X(12) VALUE SPACES.
       77  WS-PAGES-READ               PIC 9(5)  VALUE ZEROS.
       77  WS-PAGES-WRITTEN            PIC 9(5)  VALUE ZEROS.

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

           OPEN INPUT ERCOBI                                            
           IF ERCOBI-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERROR - ERCOBI - OPEN ' ERCOBI-FILE-STATUS
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
              ADD 1 TO WS-PAGES-READ
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

           IF WS-HOLD-HEAD (4) (22:14) = 'MASTER RECORDS'
              DISPLAY ' FOUND MASTER RECORD AT END '
              GO TO 0060-EXIT
           END-IF

           MOVE WS-HOLD-HEAD (4)(2:01)
                                       TO WS-CK-CARRIER
           MOVE WS-HOLD-HEAD (4)(4:06)
                                       TO WS-CK-GROUP
           MOVE WS-HOLD-HEAD (4)(11:10)
                                       TO WS-CK-RESP
           MOVE LOW-VALUES             TO WS-CK-ACCOUNT

           DISPLAY ' COMPARE KEY ' WS-COMPARE-KEY
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

           MOVE WS-CK-CARRIER          TO CO-CARRIER                          
           MOVE WS-CK-GROUP            TO CO-GROUPING                         
           MOVE WS-CK-RESP             TO CO-RESP-NO                          
           MOVE WS-CK-ACCOUNT          TO CO-ACCOUNT                          
           MOVE 'G'                    TO CO-TYPE                             

           IF CO-CONTROL-PRIMARY = PREV-CO-CONTROL
              GO TO 0070-EXIT
           END-IF
                                                                        
           IF (CO-CARRIER = SPACES)
              AND (CO-GROUPING = SPACES)
              DISPLAY ' BYPASS READ, CARR AND GRP ARE SPACES '
              DISPLAY WS-HOLD-HEAD (1)
              DISPLAY WS-HOLD-HEAD (2)
              DISPLAY WS-HOLD-HEAD (3)
              DISPLAY WS-HOLD-HEAD (4)
              GO TO 0070-EXIT
           END-IF

           READ ERCOMP
           IF ERCOMP-FILE-STATUS = '10' OR '23'
              DISPLAY ' GA NOT FOUND ' CO-CARRIER ' ' CO-GROUPING
                 ' ' CO-RESP-NO ' ' CO-TYPE
              MOVE ' '                 TO WS-S-STMT-SW
              GO TO 0070-EXIT
           ELSE
              IF ERCOMP-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERCOMP - READ ' ERCOMP-FILE-STATUS
                    ' KEY=' CO-CONTROL-PRIMARY (2:27)
                 PERFORM ABEND-PGM
              END-IF
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

PEMTST*    IF (CO-CARRIER = '9')
PEMTST*       AND (CO-RESP-NO = '0000013640')
PEMTST*       SET END-OF-INPUT TO TRUE
PEMTST*    END-IF

           .
       0070-EXIT.                                                       
           EXIT.                                                        

       0080-WRITE-S-STMT.
       
           IF WS-HEADING
              ADD 1 TO WS-PAGES-WRITTEN
              IF WS-NEW-ACCT
                 PERFORM 0100-PRINT-COVER-PAGES
                                       THRU 0100-EXIT
              END-IF
              ADD 1                    TO WS-SEQ-NO
              MOVE WS-STMT-OWNER       TO WS-USER-ID
              MOVE WS-REPORT-GID       TO WS-REPORT-GROUP-ID
              MOVE WS-HOLD-HEAD (1)    TO WS-RGID-STMT-RECORD
              WRITE S-STMT-REC         FROM WS-RGID-RECORD
              ADD 1                    TO WS-SEQ-NO
              MOVE WS-HOLD-HEAD (2)    TO WS-RGID-STMT-RECORD
              WRITE S-STMT-REC         FROM WS-RGID-RECORD
              ADD 1                    TO WS-SEQ-NO
              MOVE WS-HOLD-HEAD (3)    TO WS-RGID-STMT-RECORD
              WRITE S-STMT-REC         FROM WS-RGID-RECORD
              ADD 1                    TO WS-SEQ-NO
              MOVE WS-HOLD-HEAD (4)    TO WS-RGID-STMT-RECORD
              WRITE S-STMT-REC         FROM WS-RGID-RECORD
              MOVE ' ' TO WS-HEADING-SW
           END-IF

           ADD 1                       TO WS-SEQ-NO
           MOVE WS-STMT-OWNER          TO WS-USER-ID
           MOVE WS-REPORT-GID          TO WS-REPORT-GROUP-ID
           MOVE STMT-RECORD            TO WS-RGID-STMT-RECORD
           WRITE S-STMT-REC            FROM WS-RGID-RECORD

           .
       0080-EXIT.                                                       
           EXIT.                                                        

       0090-WRITE-OTHER-STMT.
              
           IF WS-HEADING
              ADD 1 TO WS-PAGES-WRITTEN
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
           MOVE 'SPECIAL IN'           TO CP1-ID
           MOVE WS-STMT-OWNER          TO CP1-UID
           MOVE WS-REPORT-GID          TO CP1-RGID
           IF BL-CHECK-HANDLING = '2'
              MOVE 'SEPARATE'          TO CP1-CHECK-HANDLE
           ELSE
              MOVE 'NET'               TO CP1-CHECK-HANDLE
           END-IF

           ADD 1                       TO WS-SEQ-NO
           MOVE WS-STMT-OWNER          TO WS-USER-ID
           MOVE WS-REPORT-GID          TO WS-REPORT-GROUP-ID
           MOVE WS-COVER-PAGE-1        TO WS-RGID-STMT-RECORD
           WRITE S-STMT-REC            FROM WS-RGID-RECORD

           MOVE BL-SI-LINE-1           TO CP1-SI (1)
           MOVE BL-SI-LINE-2           TO CP1-SI (2)
           MOVE BL-SI-LINE-3           TO CP1-SI (3)
           MOVE BL-SI-LINE-4           TO CP1-SI (4)
           MOVE BL-SI-LINE-5           TO CP1-SI (5)

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > 5
              ADD 1                    TO WS-SEQ-NO
              MOVE CP1-SPECIAL-INSTRUCTIONS (S1)
                                       TO WS-RGID-STMT-RECORD
              WRITE S-STMT-REC         FROM WS-RGID-RECORD
           END-PERFORM

           MOVE SPACES                 TO WS-COVER-PAGE-1
           MOVE '1'                    TO CP1-CC
           MOVE 'ADDRESS'              TO CP1-ID
           ADD 1 TO WS-SEQ-NO
           MOVE WS-COVER-PAGE-1        TO WS-RGID-STMT-RECORD
           WRITE S-STMT-REC            FROM WS-RGID-RECORD

           MOVE BL-ACCOUNT-NAME        TO CP1-ADDR (1)
           MOVE BL-CONTACT-NAME        TO CP1-ADDR (2)
           MOVE BL-ADDR1               TO CP1-ADDR (3)
           MOVE BL-ADDR2               TO CP1-ADDR (4)
           IF SPACES NOT = BL-CITY AND BL-STATE
              STRING BL-CITY ', ' BL-STATE ' ' BL-ZIP
                 DELIMITED BY '  ' INTO CP1-ADDR (5)
           END-IF

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
              WRITE S-STMT-REC         FROM WS-RGID-RECORD
           END-PERFORM

           .                                                            
       0100-EXIT.                                                       
           EXIT.                                                        

       0110-GET-ERCOBI.

           MOVE DTE-CLASIC-COMPANY-CD  TO BL-COMPANY-CD

           MOVE CO-STMT-OWNER          TO BL-STMT-OWNER
           MOVE CO-REPORT-GROUP-ID     TO BL-REPORT-GROUP-ID

           MOVE BL-CONTROL-PRIMARY     TO WS-PREV-KEY

           READ ERCOBI
           IF ERCOBI-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERCOBI - READ ' ERCOBI-FILE-STATUS
                 ' KEY=' BL-CONTROL-PRIMARY (2:16)
              MOVE SPACES              TO COMP-BILLING-INSTRUCTIONS
              MOVE CO-STMT-OWNER       TO WS-STMT-OWNER
              MOVE CO-REPORT-GROUP-ID  TO WS-REPORT-GID
           ELSE
              MOVE BL-STMT-OWNER       TO WS-STMT-OWNER
              MOVE BL-REPORT-GROUP-ID  TO WS-REPORT-GID
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

           DISPLAY ' ECS063 PAGES READ    ' WS-PAGES-READ
           DISPLAY ' ECS063 PAGES WRITTEN ' WS-PAGES-WRITTEN
           CLOSE BILLING-STATEMENTS ERCOMP ERCOBI
              OTHER-STATEMENTS S-STATEMENTS

           .
       4000-EXIT.
           EXIT.

       ABEND-PGM.
                           COPY ELCABEND.
                                                                        
