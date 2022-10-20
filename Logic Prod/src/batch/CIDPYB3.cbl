       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDPYB3.
       AUTHOR.        PABLO.
       DATE-COMPILED.
      *REMARKS.
      
      *   THIS PROGRAM READS AN EXTRACT FILE FROM AN APPLICATION    
      *   THAT SCANS THE EL562 SUMMARY PAGE BAR CODE THAT IS 
      *   ACCOMPANIED BY A CHECK/REMITTANCE FROM THE PRODUCER.

113017******************************************************************
113017*                   C H A N G E   L O G
113017*
113017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
113017*-----------------------------------------------------------------
113017*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
113017* EFFECTIVE    NUMBER
113017*-----------------------------------------------------------------
113017* 113017  IR2017112200001  PEMA  Correct possible duplicate
052218* 052218  CR2015012900002  PEMA  Use CC date instead of CPU date

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-IN      ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERPYAJ       ASSIGN TO ERPYAJ
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERPYAJ-FILE-STATUS
                               RECORD KEY IS PY-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.
                                                                        

       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-IN
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-IN-REC                 PIC X(40).

       FD  ERPYAJ.
                                       COPY ERCPYAJ.
       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDPYB3 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  ERPYAJ-FILE-STATUS          PIC XX   VALUE '00'.
       77  WS-CURRENT-BIN-DATE         PIC XX   VALUE LOW-VALUES.
       77  WS-NEXT-CYCLE-BIN-DATE      PIC XX   VALUE LOW-VALUES.
       77  WS-SELECT-BIN-DATE          PIC XX   VALUE LOW-VALUES.

       01  WS-MISC.
           05  WS-REMIT-AMT            PIC X(09).
           05  WS-REMIT-AMT-N REDEFINES WS-REMIT-AMT
                                       PIC 9(07)V99.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  WS-EXTR-IN              PIC 9(7)   VALUE ZEROS.
           05  WS-ERPYAJ-OUT           PIC 9(7)   VALUE ZEROS.

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


       01  EXTRACT-RECORD.
           12  EX-CARR                 PIC X.
           12  EX-GROUP                PIC X(6).
           12  EX-FINRESP              PIC X(10).
           12  EX-ACCOUNT              PIC X(10).
           12  EX-REMIT-AMT            PIC X(9).
           12  EX-USER                 PIC X(4).


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.


052218 LINKAGE SECTION.
052218
052218 01  PARM.
052218     05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
052218     05  PARM-CYCLE-DT               PIC X(08)     VALUE SPACES.
052218
052218 PROCEDURE DIVISION using PARM.
                                       COPY ELCDTERX.

       0000-BEGIN.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-INPUT)
PEMTST*         OR (WS-EXTR-IN > 1000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-EXTR-IN
           DISPLAY ' RECORDS  OUT  ' WS-ERPYAJ-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EXTR-IN
               I-O    ERPYAJ
               
           IF ERPYAJ-FILE-STATUS  = '00'  OR  '97'                     
              CONTINUE
           ELSE                                                       
              DISPLAY ' ERROR - ERPYAJ - OPEN  ' ERPYAJ-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE EXTR-IN ERPYAJ

           IF ERPYAJ-FILE-STATUS  = '00'
              CONTINUE
           ELSE                                                       
              DISPLAY ' ERROR - ERPYAJ - CLOSE ' ERPYAJ-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
               
           .
       0030-EXIT.
           EXIT.

       0040-INIT.

052218     if parm-length = +0
052218        display ' CYCLE DATE INPUT PARM MISSING'
052218        perform abend-pgm
052218     end-if
052218
052218     MOVE PARM-CYCLE-DT          TO DC-GREG-DATE-CYMD
052218     MOVE 'L'                    TO DC-OPTION-CODE
052218     move +0                     to dc-elapsed-months
052218                                    dc-elapsed-days
052218     PERFORM 8510-DATE-CONVERSION
052218                                 THRU 8590-EXIT
052218     if no-conversion-error
052218        move dc-bin-date-1       to ws-current-bin-date
052218     else
052218        display ' INVALID PARM DATE ' parm-cycle-dt
052218        perform abend-pgm
052218     end-if

052218*    MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
052218*    DISPLAY ' FUNCTION DATE CYMD ' WS-FN-DATE
052218*    MOVE WS-FN-DATE             TO DC-GREG-DATE-CYMD
052218*    MOVE 'L'                    TO DC-OPTION-CODE
052218*    MOVE +0                     TO DC-ELAPSED-MONTHS
052218*                                   DC-ELAPSED-DAYS
052218*    PERFORM 8510-DATE-CONVERSION
052218*                                THRU 8590-EXIT
052218*    IF NO-CONVERSION-ERROR
052218*       MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
052218*       DISPLAY ' DAY OF WEEK = ' DC-DAY-OF-WEEK
052218*    ELSE
052218*       DISPLAY ' PROBLEMS CONVERTING CURRENT DATE '
052218*       PERFORM ABEND-PGM
052218*    END-IF

           MOVE CLASIC-CREDIT-EOM-DT   TO WS-SELECT-BIN-DATE
           PERFORM 0110-READ-INPUT     THRU 0110-EXIT

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           ADD +1                      TO WS-WORK-SEQ
           MOVE SPACES                 TO PENDING-PAY-ADJ
           MOVE 'PY'                   TO PY-RECORD-ID
           MOVE DTE-CLASIC-COMPANY-CD  TO PY-COMPANY-CD
           MOVE EX-CARR                TO PY-CARRIER
           MOVE EX-GROUP               TO PY-GROUPING
           MOVE EX-FINRESP             TO PY-FIN-RESP
           MOVE EX-ACCOUNT             TO PY-ACCOUNT
           MOVE 'R'                    TO PY-RECORD-TYPE
           INSPECT EX-REMIT-AMT
              REPLACING ALL ' ' BY ZEROS
           MOVE EX-REMIT-AMT           TO WS-REMIT-AMT
           MOVE WS-REMIT-AMT-N         TO PY-ENTRY-AMT

           MOVE WS-CURRENT-BIN-DATE    TO PY-LAST-MAINT-DT
                                          PY-INPUT-DT
                                          
           MOVE EX-USER                TO PY-LAST-MAINT-BY
           MOVE +174500                TO PY-LAST-MAINT-HHMMSS
           MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT
                                          PY-BILLED-DATE
                                          PY-AR-DATE
                                          PY-REPORTED-DT
                                          PY-CHECK-WRITTEN-DT
           MOVE ZEROS                  TO PY-CHECK-QUE-CONTROL
                                          PY-CHECK-QUE-SEQUENCE
           MOVE '1825099050'           TO PY-GL-ACCOUNT
           MOVE 'SCAN REMIT'           TO PY-GL-COMMENT
           MOVE WS-SELECT-BIN-DATE     TO PY-CREDIT-SELECT-DT
           MOVE WS-WORK-SEQ            TO PY-FILE-SEQ-NO
           PERFORM 0080-WRITE-ERPYAJ-OUT
                                       THRU 0080-EXIT

           PERFORM 0110-READ-INPUT     THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0080-WRITE-ERPYAJ-OUT.

           WRITE PENDING-PAY-ADJ

113017     if erpyaj-file-status = '22'
113017        add +1 to ws-work-seq
113017        move ws-work-seq         to py-file-seq-no
113017        go to 0080-write-erpyaj-out
113017     end-if

           IF ERPYAJ-FILE-STATUS  = '00'
              ADD 1                    TO WS-ERPYAJ-OUT
           ELSE
              DISPLAY ' ERROR - ERPYAJ - WRITE  ' ERPYAJ-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0080-EXIT.
           EXIT.

       0110-READ-INPUT.

           READ EXTR-IN INTO EXTRACT-RECORD AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
052218*       DISPLAY ' INPUT RECORD ' EXTRACT-RECORD
              ADD +1                   TO WS-EXTR-IN
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
