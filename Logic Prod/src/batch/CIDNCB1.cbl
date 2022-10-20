       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDENC1.
       AUTHOR.        PABLO.
       DATE-COMPILED.
      *REMARKS.
      
      *   THIS PROGRAM READS AN EXTRACT FILE FROM A SQL EXTRACT
      *   FROM NAPERSOFT AND POPULATES THE ELENCC FILE.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-IN      ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ELENCC       ASSIGN TO ELENCC
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELENCC-FILE-STATUS
                               RECORD KEY IS NC-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.
                                                                        

       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-IN
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-IN-REC                 PIC X(213).

       FD  ELENCC.
                                       COPY ELCENCC.
       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDENC1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  ELENCC-FILE-STATUS          PIC XX   VALUE '00'.
       77  WS-CURRENT-BIN-DATE         PIC XX   VALUE LOW-VALUES.
       77  WS-NEXT-CYCLE-BIN-DATE      PIC XX   VALUE LOW-VALUES.
       77  WS-SELECT-BIN-DATE          PIC XX   VALUE LOW-VALUES.
       77  WS-HYPHEN-CNTR              PIC S999 COMP-3 VALUE +0.
       77  I1                          PIC S999 COMP-3 VALUE +0.
       77  O1                          PIC S999 COMP-3 VALUE +0.
       01  WS-MISC.
           05  WS-CHECK-AMT            PIC X(09).
           05  WS-CHECK-AMT-N REDEFINES WS-CHECK-AMT
                                       PIC 9(07)V99.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  WS-EXTR-IN              PIC 9(7)   VALUE ZEROS.
           05  WS-ELENCC-OUT           PIC 9(7)   VALUE ZEROS.

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
           12  F                       PIC X.
           12  EX-REC-TYPE             PIC X.
           12  EX-ENC-CODE             PIC X(5).
           12  EX-OP-STACK             PIC XXX.
           12  EX-ENC-LINE             PIC X(100).
           12  EX-ATTACHMENTS          PIC X(100).
           12  F                       PIC XXX.


       01  ABEND-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

                                       COPY ELCFUNDT.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.
                                       COPY ELCDTERX.

       0000-BEGIN.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-INPUT)
PEMTST*         OR (WS-EXTR-IN > 1000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-EXTR-IN
           DISPLAY ' RECORDS  OUT  ' WS-ELENCC-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EXTR-IN
               I-O    ELENCC
               
           IF ELENCC-FILE-STATUS  = '00'  OR  '97'                     
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD OPEN FOR ELENCC ' ELENCC-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE EXTR-IN ELENCC

           IF ELENCC-FILE-STATUS  = '00'
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD CLOSE FOR ELENCC ' ELENCC-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF
               
           .
       0030-EXIT.
           EXIT.

       0040-INIT.

           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           DISPLAY ' FUNCTION DATE CYMD ' WS-FN-DATE
           MOVE WS-FN-DATE             TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           MOVE +0                     TO DC-ELAPSED-MONTHS
                                          DC-ELAPSED-DAYS
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DATE
           ELSE
              DISPLAY ' ERROR CONVERTING CURRENT DATE '
           END-IF

           PERFORM 0110-READ-INPUT     THRU 0110-EXIT

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           MOVE 'NC'                   TO ENCLOSURE-CODES
           MOVE DTE-CLASIC-COMPANY-CD  TO NC-COMPANY-CD
           MOVE EX-REC-TYPE            TO NC-REC-TYPE
           MOVE FUNCTION UPPER-CASE(EX-ENC-CODE)
                                       TO NC-ENC-CODE
           MOVE EX-OP-STACK            TO NC-OUTPUT-STACK
           MOVE EX-ENC-LINE            TO NC-ENCLOSURE-LINE
           MOVE EX-ATTACHMENTS         TO NC-ATTACHMENTS
                                          
           MOVE 'AUTO'                 TO NC-LAST-MAINT-USER
           MOVE +180000                TO NC-LAST-MAINT-HHMMSS
           MOVE WS-CURRENT-BIN-DATE    TO NC-LAST-MAINT-DT
          
           PERFORM 0080-WRITE-ELENCC   THRU 0080-EXIT

           PERFORM 0110-READ-INPUT     THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0080-WRITE-ELENCC.

           WRITE ENCLOSURE-CODES

           IF ELENCC-FILE-STATUS  = '00'
              ADD 1                    TO WS-ELENCC-OUT
           ELSE
              DISPLAY ' BAD WRITE FOR ELENCC ' ELENCC-FILE-STATUS
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

