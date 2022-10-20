       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDNTX1.
       AUTHOR.        PABLO.
       DATE-COMPILED.
      *REMARKS.
100208******************************************************************
100208*                   C H A N G E   L O G
100208*
100208* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100208*-----------------------------------------------------------------
100208*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100208* EFFECTIVE    NUMBER
100208*-----------------------------------------------------------------
100208* 100208                   PEMA  NEW PROGRAM
032612* 032612   2011110200001   PEMA  AHL CHANGES
100208******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERACNT       ASSIGN TO ERACNT
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERACNT-FILE-STATUS
                               RECORD KEY IS NT-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.

           SELECT ACNT-OUT     ASSIGN TO ACNTOT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ACNT-OUT
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  ACNT-OUT-REC                PIC X(110).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERACNT.

                                       COPY ERCACNT.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDNTX1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       01  WS-MISC.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  ERACNT-FILE-STATUS      PIC XX     VALUE '00'.
           05  SUB1            COMP-3  PIC S9(3)  VALUE +0.

           05  WS-ERACNT-IN            PIC 9(7)   VALUE ZEROS.
           05  WS-ERACNT-OUT           PIC 9(7)   VALUE ZEROS.

       01  WS-SAVE-ERACNT              PIC X(110) VALUE LOW-VALUES.
       01  ERACNT-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-SEQ-NO               PIC 9(7).
           12  EX-TAB5                 PIC X.
           12  EX-LAST-MAINT-DT        PIC X(10).
           12  EX-TAB6                 PIC X.
           12  EX-LAST-MAINT-BY        PIC XXXX.
           12  EX-TAB7                 PIC X.
           12  EX-NOTE                 PIC X(60).
           12  EX-TAB8                 PIC X.
           12  EX-EOR                  PIC X.

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

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0040-INIT           THRU 0040-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                (END-OF-INPUT)
PEMTST*         OR (WS-ERACNT-IN > 1000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-ERACNT-IN
           DISPLAY ' RECORDS  OUT  ' WS-ERACNT-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ERACNT
               OUTPUT ACNT-OUT

           IF ERACNT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERACNT OPEN ERROR ' ERACNT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ERACNT ACNT-OUT

           IF ERACNT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERACNT CLOSE ERROR ' ERACNT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-INIT.

           MOVE SPACES                 TO ERACNT-DETAIL-RECORD
052704     MOVE X'09'                  TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
           MOVE 'E'                    TO EX-EOR

           MOVE ERACNT-DETAIL-RECORD   TO WS-SAVE-ERACNT
           PERFORM 0120-START-ERACNT   THRU 0120-EXIT
032612     if not end-of-input
              PERFORM 0110-READ-ERACNT THRU 0110-EXIT
032612     end-if

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           IF NT-RECORD-TYPE = '1'
              PERFORM 0060-PROCESS-FILE
                                       THRU 0060-EXIT
           END-IF
           
           PERFORM 0110-READ-ERACNT    THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-PROCESS-FILE.

           MOVE WS-SAVE-ERACNT         TO ERACNT-DETAIL-RECORD

           MOVE NT-CARRIER             TO EX-CARRIER
           MOVE NT-GROUPING            TO EX-GROUPING
           MOVE NT-STATE               TO EX-STATE
           MOVE NT-ACCOUNT             TO EX-ACCOUNT
           MOVE NT-NOTE-LINE           TO EX-NOTE
           MOVE NT-LAST-MAINT-BY       TO EX-LAST-MAINT-BY
           MOVE NT-LINE-SEQUENCE       TO EX-SEQ-NO

           MOVE NT-LAST-MAINT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-MAINT-DT
           ELSE
              MOVE SPACES              TO EX-LAST-MAINT-DT
              DISPLAY ' BYPASSING NOTE ' NT-CARRIER ' ' NT-STATE ' '
                 NT-ACCOUNT ' ' NT-NOTE-LINE
              GO TO 0060-EXIT
           END-IF

           PERFORM 0080-WRITE-ACNT-OUT THRU 0080-EXIT

           .
       0060-EXIT.
           EXIT.

       0080-WRITE-ACNT-OUT.

           INSPECT ERACNT-DETAIL-RECORD REPLACING
              ALL ';'                  BY ' '
              ALL X'00'                BY ' '
              ALL X'1E'                BY ' '
              ALL X'09'                BY ';'

           WRITE ACNT-OUT-REC          FROM ERACNT-DETAIL-RECORD
           ADD 1 TO WS-ERACNT-OUT

           .
       0080-EXIT.
           EXIT.

       0110-READ-ERACNT.

           READ ERACNT NEXT RECORD

           IF (ERACNT-FILE-STATUS = '10' OR '23')
              OR (NT-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERACNT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERACNT READ ERROR ' ERACNT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT END-OF-INPUT 
              ADD +1                   TO WS-ERACNT-IN
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ERACNT.

           MOVE LOW-VALUES             TO NT-CONTROL-PRIMARY

           MOVE DTE-CLASIC-COMPANY-CD  TO NT-COMPANY-CD

           START ERACNT KEY >= NT-CONTROL-PRIMARY

           IF ERACNT-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERACNT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERACNT START ERROR ' ERACNT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0120-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.

