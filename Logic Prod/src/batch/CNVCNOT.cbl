       IDENTIFICATION DIVISION.
       PROGRAM-ID.                CNVCNOT.
      *AUTHOR.     AJRA.
      *REMARKS.
      * THIS PROGRAM READS THE ERNOTE FILE AND MOVES THE NON
      * BILLING NOTES TO FILE ERCNOT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT ERCNOT           ASSIGN TO ERCNOT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CZ-CONTROL-PRIMARY
                                   FILE STATUS IS ERCNOT-FILE-STATUS.

           SELECT ERNOTE           ASSIGN TO ERNOTE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS SEQUENTIAL
                                   RECORD KEY IS CN-CONTROL-PRIMARY
                                   FILE STATUS IS ERNOTE-FILE-STATUS.

       DATA DIVISION.

       FILE SECTION.

       FD  ERCNOT.
                                       COPY ERCCNOT.                         

       FD  ERNOTE.
                                       COPY ERCNOTE.



       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-ABEND-FILE-STATUS        PIC XX            VALUE ZERO.    
       77  WS-ABEND-MESSAGE            PIC X(80)         VALUE SPACES.  
       77  WS-ABEND-PROGRAM            PIC X(8)          VALUE SPACES.  
       77  WS-RETURN-CODE              PIC S9(4)         VALUE +0.      
       77  WS-ZERO                     PIC S9     COMP-3 VALUE +0.      
       77  IN-CNT                      PIC 9999999   VALUE ZEROS.
       77  WS-SUB                      PIC S999      VALUE +0 COMP-3.
       77  WS-SEQ                      PIC S9(4)     VALUE +0 COMP.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  ERCNOT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERNOTE-FILE-STATUS          PIC XX  VALUE LOW-VALUES.

       01  W-MISC.
           05  WS-NOTE-CHANGED        PIC X   VALUE 'N'.
               88  ERNOTE-NOT-CHANGED         VALUE 'N'.
               88  ERNOTE-CHANGED             VALUE 'Y'.
           05  WS-WORK-TERM           PIC 999 VALUE ZEROS.
           05  WS-BIN-EFF             PIC XX VALUE X'A4ED'.
           05  WS-BIN-CNC             PIC XX VALUE LOW-VALUES.
           05  BAL-DRP-CNT            PIC 9(7) VALUE ZEROS.
           05  BAL-FIX-CNT            PIC 9(7) VALUE ZEROS.
           05  BAL-CNT                PIC 9(7) VALUE ZEROS.
           05  NOTE-IN-CNT            PIC 9(7) VALUE ZEROS.
           05  CNOT-CNT               PIC 9(7) VALUE ZEROS.
           05  NOTE-RCNT              PIC 9(7) VALUE ZEROS.
           05  CERT-RCNT              PIC 9(7) VALUE ZEROS.
           05  WS-CRT-SW              PIC X VALUE ' '.
               88  END-OF-ERNOTE            VALUE 'Y'.
           05  W-PREM-N               PIC 9(7)V99.
           05  W-PREM-X      REDEFINES  W-PREM-N.
               10  W-PREM-DOL.
                   15  W-PREM-DOL2    PIC X(02).
                   15  W-PREM-DOL5    PIC X(05).
               10  W-PREM-CEN         PIC X(02).


       PROCEDURE DIVISION.

           PERFORM 0040-OPEN-FILES     THRU 0040-EXIT
           PERFORM 0050-INIT           THRU 0050-EXIT

           PERFORM 0020-PROCESS        THRU 0020-EXIT UNTIL
              (END-OF-ERNOTE)
           PERFORM 0060-CLOSE-FILES    THRU 0060-EXIT

           DISPLAY ' NOTE RECS READ ' NOTE-IN-CNT
           DISPLAY ' NOTE REWRITE   ' NOTE-RCNT
           DISPLAY ' C-NOTE WRITE   ' CNOT-CNT

           GOBACK
           .

       0010-START-ERNOTE.

           MOVE LOW-VALUES             TO CN-CONTROL-PRIMARY
           MOVE ZERO                   TO CN-CARRIER
           MOVE X'04'                  TO CN-COMPANY-CD

           START ERNOTE KEY IS NOT < CN-CONTROL-PRIMARY

           IF ERNOTE-FILE-STATUS = '10' OR '23'
              SET END-OF-ERNOTE        TO TRUE
           ELSE
              IF ERNOTE-FILE-STATUS NOT = '00'
                 DISPLAY 'ERNOTE START     ' ERNOTE-FILE-STATUS
                 SET END-OF-ERNOTE     TO TRUE
              END-IF
           END-IF

           .

       0010-EXIT.
           EXIT.

       0015-READ-ERNOTE.


           READ ERNOTE NEXT RECORD

           IF ERNOTE-FILE-STATUS = '00'
              ADD 1                    TO NOTE-IN-CNT
           ELSE
              IF (ERNOTE-FILE-STATUS = '23' OR '10')
                 SET END-OF-ERNOTE     TO TRUE
              ELSE
                 DISPLAY ' BAD READ ERNOTE ' CN-CONTROL-PRIMARY '  '
                    ERNOTE-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0015-EXIT.
           EXIT.

       0020-PROCESS.


           IF CN-LINES > SPACES
              MOVE +0                TO WS-SUB
              MOVE +0                TO WS-SEQ
              MOVE 'N'               TO WS-NOTE-CHANGED
              PERFORM UNTIL WS-SUB = 10
                  ADD +1             TO WS-SUB
                  IF WS-SUB < CN-BILLING-START-LINE-NO OR
                     WS-SUB > CN-BILLING-END-LINE-NO
                     IF CN-LINE (WS-SUB) > SPACES
                         MOVE 'Y'      TO WS-NOTE-CHANGED
                         ADD +1        TO WS-SEQ
                         PERFORM 0070-BUILD-ERCNOT
                                       THRU 0070-EXIT
                         MOVE SPACES TO CN-LINE (WS-SUB)
                     END-IF
                  END-IF
              END-PERFORM
              PERFORM 0030-REWRITE-ERNOTE
                                       THRU 0030-EXIT
           END-IF
           PERFORM 0015-READ-ERNOTE    THRU 0015-EXIT
              

           .
       0020-EXIT.
            EXIT.


       0030-REWRITE-ERNOTE.
       
           IF ERNOTE-NOT-CHANGED
              CONTINUE
           ELSE
              REWRITE CERTIFICATE-NOTE
              IF ERNOTE-FILE-STATUS = '00'
                 ADD 1                 TO NOTE-RCNT
              ELSE
                 DISPLAY ' BAD REWRITE ERNOTE ' CN-CONTROL-PRIMARY
                    '  ' ERNOTE-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0030-EXIT.
            EXIT.


              
       0040-OPEN-FILES.
       
           OPEN I-O ERCNOT

           IF ERCNOT-FILE-STATUS NOT = '00'
              DISPLAY ' ERCNOT OPEN ' ERCNOT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           OPEN I-O ERNOTE

           IF ERNOTE-FILE-STATUS NOT = '00'
              DISPLAY ' ERNOTE OPEN ' ERNOTE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0040-EXIT.
           EXIT.

       0050-INIT.
       
           PERFORM 0010-START-ERNOTE   THRU 0010-EXIT
           PERFORM 0015-READ-ERNOTE    THRU 0015-EXIT
           .
       0050-EXIT.
           EXIT.

       0060-CLOSE-FILES.
       
           CLOSE ERCNOT

           IF ERCNOT-FILE-STATUS NOT = '00'
              DISPLAY ' ERCNOT CLOSE ' ERCNOT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           CLOSE ERNOTE

           IF ERNOTE-FILE-STATUS NOT = '00'
              DISPLAY ' ERNOTE CLOSE ' ERNOTE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-BUILD-ERCNOT.

           PERFORM 0071-BUILD-ERCNOT-KEY THRU 0071-EXIT
           PERFORM 0072-BUILD-ERCNOT-BODY THRU 0072-EXIT

           .
       0070-EXIT.
            EXIT.

       0071-BUILD-ERCNOT-KEY.
       
           MOVE SPACES                 TO CERT-NOTE-FILE
           MOVE 'CZ'                   TO CZ-RECORD-ID
           MOVE CN-CONTROL-PRIMARY     TO CZ-CONTROL-PRIMARY
           MOVE '1'                    TO CZ-RECORD-TYPE
           MOVE WS-SEQ                 TO CZ-NOTE-SEQUENCE

           .
       0071-EXIT.
           EXIT.

       0072-BUILD-ERCNOT-BODY.
       
           MOVE CN-LINE (WS-SUB) (1:63) TO CZ-NOTE
           MOVE 'CONV'                 TO CZ-LAST-MAINT-USER
           MOVE WS-BIN-EFF             TO CZ-LAST-MAINT-DT
           MOVE 0                      TO CZ-LAST-MAINT-HHMMSS
           PERFORM 0073-WRITE-ERCNOT THRU 0073-EXIT
           IF CN-LINE (WS-SUB) (64:14) > SPACES
               ADD +1        TO WS-SEQ
               MOVE WS-SEQ             TO CZ-NOTE-SEQUENCE
               MOVE CN-LINE (WS-SUB) (64:14) TO CZ-NOTE
               MOVE 'CONV'             TO CZ-LAST-MAINT-USER
               MOVE WS-BIN-EFF         TO CZ-LAST-MAINT-DT
               MOVE 0                  TO CZ-LAST-MAINT-HHMMSS
               PERFORM 0073-WRITE-ERCNOT THRU 0073-EXIT
           END-IF
               
           .
       0072-EXIT.
           EXIT.

       0073-WRITE-ERCNOT.
       
           WRITE CERT-NOTE-FILE

           IF ERCNOT-FILE-STATUS = '00'
              ADD 1                    TO CNOT-CNT
           ELSE
              DISPLAY ' BAD WRITE ERCNOT ' CZ-CONTROL-PRIMARY '  '
                  'STATUS =   ' ERCNOT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
 
           .
       0073-EXIT.
           EXIT.
                      


       ABEND-PGM.   COPY ELCABEND.

