       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDEOB1.
       AUTHOR.        PABLO.
       DATE-COMPILED.
      *REMARKS.
      
      *   THIS PROGRAM READS AN EXTRACT FILE FROM A SQL EXTRACT
      *   FROM NAPERSOFT AND POPULATES THE ELEOBC FILE.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTR-IN      ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ELEOBC       ASSIGN TO ELEOBC
                               ACCESS IS DYNAMIC
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ELEOBC-FILE-STATUS
                               RECORD KEY IS EO-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.
                                                                        

       DATA DIVISION.
       FILE SECTION.

       FD  EXTR-IN
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  EXTR-IN-REC                 PIC X(270).

       FD  ELEOBC.
                                       COPY ELCEOBC.
       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDEOB1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  WS-WORK-SEQ                 PIC S9(7) COMP-3 VALUE +0.
       77  ELEOBC-FILE-STATUS          PIC XX   VALUE '00'.
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
           05  WS-ELEOBC-OUT           PIC 9(7)   VALUE ZEROS.

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
011612     12  EX-EOB-REC-TYPE         PIC X.
           12  EX-EOB-CODE             PIC XXXX.
011612     12  EX-EOB-DESC             PIC X(265).


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
           DISPLAY ' RECORDS  OUT  ' WS-ELEOBC-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT EXTR-IN
               I-O    ELEOBC
               
           IF ELEOBC-FILE-STATUS  = '00'  OR  '97'                     
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD OPEN FOR ELEOBC ' ELEOBC-FILE-STATUS       
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE EXTR-IN ELEOBC

           IF ELEOBC-FILE-STATUS  = '00'
              CONTINUE
           ELSE                                                       
              DISPLAY ' BAD CLOSE FOR ELEOBC ' ELEOBC-FILE-STATUS       
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

           MOVE 'EO'                   TO EO-RECORD-ID
           MOVE DTE-CLASIC-COMPANY-CD  TO EO-COMPANY-CD
011612     MOVE EX-EOB-REC-TYPE        TO EO-RECORD-TYPE
           MOVE EX-EOB-CODE            TO EO-EOB-CODE
           MOVE EX-EOB-DESC            TO EO-DESCRIPTION
                                          
           MOVE 'AUTO'                 TO EO-LAST-MAINT-USER
           MOVE +180000                TO EO-LAST-MAINT-HHMMSS
           MOVE WS-CURRENT-BIN-DATE    TO EO-LAST-MAINT-DT
          
           PERFORM 0080-WRITE-ELEOBC   THRU 0080-EXIT

           PERFORM 0110-READ-INPUT     THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0080-WRITE-ELEOBC.

           WRITE EOB-CODES

           IF ELEOBC-FILE-STATUS  = '00'
              ADD 1                    TO WS-ELEOBC-OUT
           ELSE
              DISPLAY ' BAD WRITE FOR ELEOBC ' ELEOBC-FILE-STATUS
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

