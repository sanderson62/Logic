       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PEMPYF1.
       AUTHOR.        PABLO.
       DATEILED.
      *REMARKS.
      
      *   THIS PROGRAM READS THE ERPYAJ FILE AND CHANGES THE 
      *   CODE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERPYAJ       ASSIGN TO ERPYAJ
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERPYAJ-FILE-STATUS
                               RECORD KEY IS PY-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.
                                                                        

       DATA DIVISION.
       FILE SECTION.

       FD  ERPYAJ.
                                       COPY ERCPYAJ.
       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     PEMPYF1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +20.
       77  ERPYAJ-FILE-STATUS          PIC XX   VALUE '00'.
       77  WS-TOTAL-AMT                PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-DIS-AMT                  PIC Z,ZZZ,ZZ9.99  VALUE ZEROS.

       01  WS-HOLD-ERPYAJ              PIC X(200) VALUE SPACES.
       01  WS-MISC.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-ERPYAJ                  VALUE 'Y'.
           05  WS-ERPYAJ-IN            PIC 9(7)   VALUE ZEROS.
           05  WS-ERPYAJ-OUT           PIC 9(7)   VALUE ZEROS.
           05  WS-ERPYAJ-DEL           PIC 9(7)   VALUE ZEROS.

       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WORK-ABEND-CODE.
           12  WAC-1                   PIC X.
           12  WAC-2                   PIC X.
           12  WAC-3-4.
               16  WAC-3               PIC X.
               16  WAC-4               PIC X.


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

       0000-BEGIN.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-ERPYAJ)
PEMTST*         OR (WS-ERPYAJ-IN > 1000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-ERPYAJ-IN
           DISPLAY ' RECORDS  OUT  ' WS-ERPYAJ-OUT
           DISPLAY ' RECORDS  DEL  ' WS-ERPYAJ-DEL
           MOVE WS-TOTAL-AMT           TO WS-DIS-AMT
           DISPLAY ' TOTAL AMOUNT  ' WS-DIS-AMT
           GOBACK
           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN I-O    ERPYAJ
               
           IF ERPYAJ-FILE-STATUS  = '00'  OR  '97'                     
              DISPLAY ' SUCCESSFUL OPEN ' ERPYAJ-FILE-STATUS
      *       CONTINUE
           ELSE                                                       
              DISPLAY ' BAD OPEN FOR ERPYAJ ' ERPYAJ-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ERPYAJ

           IF ERPYAJ-FILE-STATUS  = '00'
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD CLOSE FOR ERPYAJ ' ERPYAJ-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF
               
           .
       0030-EXIT.
           EXIT.

       0040-INIT.

           PERFORM 0100-START-ERPYAJ   THRU 0100-EXIT
           PERFORM 0110-READ-ERPYAJ    THRU 0110-EXIT
           .

       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           IF (PY-LAST-MAINT-BY = 'AUTO')
              AND (PY-GL-COMMENT = 'MONTHLY CK')
              AND (PY-INPUT-DT = X'A464')
              AND (PY-RECORD-TYPE = 'C')
              AND (PY-GL-ACCOUNT = '1825011300')
              DISPLAY ' FOUND ONE '    PY-CONTROL-PRIMARY
              COMPUTE WS-TOTAL-AMT = WS-TOTAL-AMT + PY-ENTRY-AMT
              MOVE 'R'                 TO PY-RECORD-TYPE
              MOVE X'A46C'             TO PY-INPUT-DT
                                          PY-LAST-MAINT-DT
              MOVE WS-WORK-SEQ         TO PY-FILE-SEQ-NO
              ADD +1                   TO WS-WORK-SEQ
              PERFORM 0080-WRITE-ERPYAJ
                                       THRU 0080-EXIT
           END-IF

           PERFORM 0110-READ-ERPYAJ    THRU 0110-EXIT

           .

       0050-EXIT.
           EXIT.

       0070-DELETE-ERPYAJ.

           DELETE ERPYAJ

           IF ERPYAJ-FILE-STATUS  = '00'
              ADD 1                    TO WS-ERPYAJ-DEL
           ELSE
              DISPLAY ' BAD DELET FOR ERPYAJ ' ERPYAJ-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0070-EXIT.
           EXIT.

       0080-WRITE-ERPYAJ.

PEMTST*    MOVE '00' TO ERPYAJ-FILE-STATUS
PEMTST     WRITE PENDING-PAY-ADJ

           IF ERPYAJ-FILE-STATUS  = '00'
              ADD 1                    TO WS-ERPYAJ-OUT
           ELSE
              DISPLAY ' BAD WRITE FOR ERPYAJ ' ERPYAJ-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0080-EXIT.
           EXIT.

       0100-START-ERPYAJ.

           MOVE LOW-VALUES             TO PY-CONTROL-PRIMARY
           MOVE X'04'                  TO PY-COMPANY-CD
           START ERPYAJ KEY IS NOT < PY-CONTROL-PRIMARY

           IF ERPYAJ-FILE-STATUS NOT = '00'
              DISPLAY ' ERPYAJ, BAD START ' ERPYAJ-FILE-STATUS
              PERFORM ABEND-PGM
           ELSE
              DISPLAY ' SUCCESSFUL START ' ERPYAJ-FILE-STATUS
           END-IF
           .
       0100-EXIT.
           EXIT.
           
       0110-READ-ERPYAJ.

           READ ERPYAJ NEXT RECORD

           IF (ERPYAJ-FILE-STATUS = '10' OR '23')
              OR (PY-COMPANY-CD > X'04')
              SET END-OF-ERPYAJ        TO TRUE
           ELSE
              IF ERPYAJ-FILE-STATUS NOT = '00'
                 DISPLAY ' BAD READ FOR ERPYAJ ' ERPYAJ-FILE-STATUS
                 PERFORM ABEND-PGM
      *       ELSE
      *          DISPLAY ' SUCCESSFUL READ ' ERPYAJ-FILE-STATUS
      *          ' KEY = ' PY-CONTROL-PRIMARY
              END-IF
           END-IF
           
           IF NOT END-OF-ERPYAJ
              ADD +1                   TO WS-ERPYAJ-IN
           END-IF

           .
       0110-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
