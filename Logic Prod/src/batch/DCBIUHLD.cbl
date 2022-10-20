       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCBIUHLD.
       AUTHOR.     AJRA.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ERPNDB           ASSIGN TO ERPNDB
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS PB-CONTROL-PRIMARY
                                   FILE STATUS IS ERPNDB-FILE-STATUS.
                                   
           SELECT BATCHIN          ASSIGN TO SYS010.
           SELECT RESULTS          ASSIGN TO SYS011
                                   ORGANIZATION IS LINE SEQUENTIAL.
                                              
       DATA DIVISION.
       FILE SECTION.

      /
       FD  ERPNDB.

           COPY ERCPNDB.
           
       FD  BATCHIN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  BATCHIN-REC             PIC X(6).
       
       
       FD  RESULTS.
       01  RESULTS-REC          PIC X(132).
       
       
      /
       WORKING-STORAGE SECTION.
       01  WS-STATUS-CODES.
           05  ERPNDB-FILE-STATUS         PIC XX        VALUE SPACES.
           05  WS-EOF-SW                  PIC X(3)      VALUE SPACES.
               88  END-OF-ERPNDB          VALUE 'YES'.

       01  WS-WORK-FIELDS.
           05  WS-ABEND-FLD               PIC 9         VALUE ZEROS.
           05  WS-ZERO-FLD                PIC 9         VALUE ZEROS.
           05  WS-ERPNDB-RECS-IN          PIC 9(9)      VALUE ZEROS.
           05  WS-ERPNDB-RECS-BYPASS      PIC 9(9)      VALUE ZEROS.
           05  WS-ERPNDB-RECS-FIX         PIC 9(9)      VALUE ZEROS.
           05  WS-DISPLAY-CNT             PIC Z,ZZZ,ZZ9 VALUE ZEROS.
      
       01  BATCH-ID                       PIC X(06).
       
       01  PRINT-REC                      PIC X(132).

       PROCEDURE DIVISION.
       0000-MAINLINE.

           PERFORM 0100-OPEN-FILES      THRU 0100-EXIT
           PERFORM 0200-INITIALIZE      THRU 0200-EXIT
           PERFORM 1000-PROCESS         THRU 1000-EXIT
                   UNTIL END-OF-ERPNDB
           PERFORM 3000-FINAL-TOTALS    THRU 3000-EXIT
           PERFORM 4000-CLOSE-FILES     THRU 4000-EXIT
           GOBACK

           .
       0000-EXIT.
           EXIT.

       0100-OPEN-FILES.

           OPEN I-O   ERPNDB

           IF ERPNDB-FILE-STATUS = '00'
               CONTINUE
           ELSE
               DISPLAY '*** ERROR OPENING ERPNDB FILE ***'
               DISPLAY '*** STATUS CODE IS ' ERPNDB-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF

           OPEN INPUT BATCHIN.
           OPEN OUTPUT RESULTS.
           .
       0100-EXIT.
           EXIT.

       0200-INITIALIZE.

           MOVE SPACES              TO WS-EOF-SW
           MOVE ZEROS               TO WS-ERPNDB-RECS-IN
                                       WS-ERPNDB-RECS-BYPASS
                                       WS-ERPNDB-RECS-FIX

           PERFORM 1100-START-ERPNDB  THRU 1100-EXIT

           PERFORM 1200-READ-ERPNDB   THRU 1200-EXIT
           
           READ BATCHIN INTO BATCH-ID.
           
           .
       0200-EXIT.
           EXIT.

       1000-PROCESS.

           IF PB-RECORD-TYPE = '1' AND
              PB-ENTRY-BATCH = BATCH-ID
              IF PB-RECORD-BILL = 'H'
                 MOVE SPACES           TO PB-RECORD-BILL
                 PERFORM 2100-REWRITE-ERPNDB
                                       THRU 2100-EXIT
              ELSE
                 ADD  1   TO  WS-ERPNDB-RECS-BYPASS                 
              END-IF
           ELSE
              ADD  1   TO  WS-ERPNDB-RECS-BYPASS                 
           END-IF

           PERFORM 1200-READ-ERPNDB      THRU 1200-EXIT

           .
       1000-EXIT. 
           EXIT.
      
       1100-START-ERPNDB.

           MOVE LOW-VALUES TO PB-CONTROL-PRIMARY
           MOVE X'05'      TO PB-COMPANY-CD
           START ERPNDB KEY IS NOT LESS THAN
                            PB-CONTROL-PRIMARY
           IF ERPNDB-FILE-STATUS NOT = '00'
              DISPLAY ' ERPNDB, BAD START '
                    ERPNDB-FILE-STATUS
              PERFORM 9999-ABEND-RTN
           END-IF

           .

       1100-EXIT.
           EXIT.

       1200-READ-ERPNDB.

           READ ERPNDB NEXT RECORD

           IF ERPNDB-FILE-STATUS = '10' OR '23'
              SET END-OF-ERPNDB        TO TRUE
           ELSE
              IF ERPNDB-FILE-STATUS NOT = '00'
                 DISPLAY 'ERPNDB, BAD READ NEXT '
                      ERPNDB-FILE-STATUS
                 PERFORM 9999-ABEND-RTN
              ELSE
                 IF PB-COMPANY-CD > X'05'
                    SET END-OF-ERPNDB  TO TRUE
                 ELSE
                    ADD 1              TO WS-ERPNDB-RECS-IN
                 END-IF
              END-IF
           END-IF

           .
       1200-EXIT.
           EXIT.

       2100-REWRITE-ERPNDB.

           REWRITE PENDING-BUSINESS

           IF ERPNDB-FILE-STATUS = '00'
               ADD 1               TO WS-ERPNDB-RECS-FIX
           ELSE
               DISPLAY 'ERPNDB, BAD REWRITE '
               DISPLAY '*** STATUS CODE IS ' ERPNDB-FILE-STATUS
               PERFORM 9999-ABEND-RTN
           END-IF
           .
       2100-EXIT.
           EXIT.


       3000-FINAL-TOTALS.
           MOVE '**************************************************'
               TO PRINT-REC.
           WRITE RESULTS-REC FROM PRINT-REC.    
           MOVE '***                                            ***'
               TO PRINT-REC.
           WRITE RESULTS-REC FROM PRINT-REC.    
           STRING '***  BATCH NUMBER                =    ', 
                  BATCH-ID,  '   ***'
               INTO PRINT-REC.
           WRITE RESULTS-REC FROM PRINT-REC.    

           MOVE WS-ERPNDB-RECS-IN        TO WS-DISPLAY-CNT
           STRING '***  PNDB MASTER RECS IN         = ',
                  WS-DISPLAY-CNT, '   ***'
               INTO PRINT-REC.
           WRITE RESULTS-REC FROM PRINT-REC.    

           MOVE WS-ERPNDB-RECS-BYPASS    TO WS-DISPLAY-CNT
           STRING '***  PNDB MASTER RECS BYPASSED   = ',
                  WS-DISPLAY-CNT, '   ***'
               INTO PRINT-REC.
           WRITE RESULTS-REC FROM PRINT-REC.    

           MOVE WS-ERPNDB-RECS-FIX       TO WS-DISPLAY-CNT
           STRING '***  PNDB MASTER RECS UPDATED    = ',
                   WS-DISPLAY-CNT, '   ***'
               INTO PRINT-REC.
           WRITE RESULTS-REC FROM PRINT-REC.    

           MOVE '***                                            ***'
               TO PRINT-REC.
           WRITE RESULTS-REC FROM PRINT-REC.    
           MOVE '**************************************************'
               TO PRINT-REC.
           WRITE RESULTS-REC FROM PRINT-REC.    
           .
       3000-EXIT. 
           EXIT.


       4000-CLOSE-FILES.

           CLOSE ERPNDB.
           CLOSE BATCHIN.
           CLOSE RESULTS.

           
       4000-EXIT.
           EXIT.


       9999-ABEND-RTN.
           DISPLAY '*** PENDING BUSINESS        PROGRAM ABENDING ***'
           DISPLAY '************************************************'

           MOVE 1                 TO WS-ABEND-FLD
           MOVE 0                 TO WS-ZERO-FLD

           COMPUTE WS-ABEND-FLD = WS-ABEND-FLD / WS-ZERO-FLD
           .
       9999-EXIT. 
           EXIT.