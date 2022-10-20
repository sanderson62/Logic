       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDNFX1.
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
100208* 100208  CR2008090200004  PEMA  NEW PROGRAM
032612* 032612   2011110200001   PEMA  AHL CHANGES
100208******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERCONT       ASSIGN TO ERCONT
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERCONT-FILE-STATUS
                               RECORD KEY IS NF-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.

           SELECT CONT-OUT     ASSIGN TO CONTOT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  CONT-OUT
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  CONT-OUT-REC                PIC X(120).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERCONT.

                                       COPY ERCCONT.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDNFX1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       01  WS-MISC.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  ERCONT-FILE-STATUS      PIC XX     VALUE '00'.
           05  SUB1            COMP-3  PIC S9(3)  VALUE +0.

           05  WS-ERCONT-IN            PIC 9(7)   VALUE ZEROS.
           05  WS-ERCONT-OUT           PIC 9(7)   VALUE ZEROS.

       01  WS-SAVE-ERCONT              PIC X(120) VALUE LOW-VALUES.
       01  ERCONT-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-RESP-NO              PIC X(10).
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-NOTE-TYPE            PIC X.
           12  EX-TAB5                 PIC X.
           12  EX-SEQ-NO               PIC 9(7).
           12  EX-TAB6                 PIC X.
           12  EX-LAST-MAINT-DT        PIC X(10).
           12  EX-TAB7                 PIC X.
           12  EX-LAST-MAINT-BY        PIC XXXX.
           12  EX-TAB8                 PIC X.
           12  EX-NOTE                 PIC X(60).
           12  EX-TAB9                 PIC X.
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
PEMTST*         OR (WS-ERCONT-IN > 1000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-ERCONT-IN
           DISPLAY ' RECORDS  OUT  ' WS-ERCONT-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ERCONT
               OUTPUT CONT-OUT

           IF ERCONT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCONT OPEN ERROR ' ERCONT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ERCONT CONT-OUT

           IF ERCONT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCONT CLOSE ERROR ' ERCONT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-INIT.

           MOVE SPACES                 TO ERCONT-DETAIL-RECORD
052704     MOVE X'09'                  TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
                                          EX-TAB9
           MOVE 'E'                    TO EX-EOR

           MOVE ERCONT-DETAIL-RECORD   TO WS-SAVE-ERCONT
           PERFORM 0120-START-ERCONT   THRU 0120-EXIT
032612     if not end-of-input
              PERFORM 0110-READ-ERCONT THRU 0110-EXIT
032612     end-if

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           MOVE WS-SAVE-ERCONT         TO ERCONT-DETAIL-RECORD

           MOVE NF-CARRIER             TO EX-CARRIER
           MOVE NF-GROUPING            TO EX-GROUPING
           MOVE NF-FIN-RESP-NO         TO EX-RESP-NO
           MOVE NF-ACCOUNT             TO EX-ACCOUNT
           MOVE NF-RECORD-TYPE         TO EX-NOTE-TYPE
           MOVE NF-NOTE-LINE           TO EX-NOTE
           MOVE NF-LAST-MAINT-BY       TO EX-LAST-MAINT-BY
           MOVE NF-LINE-SEQUENCE       TO EX-SEQ-NO

           MOVE NF-LAST-MAINT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-MAINT-DT
           ELSE
              MOVE SPACES              TO EX-LAST-MAINT-DT
           END-IF

           PERFORM 0080-WRITE-CONT-OUT THRU 0080-EXIT

           PERFORM 0110-READ-ERCONT    THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0080-WRITE-CONT-OUT.

           INSPECT ERCONT-DETAIL-RECORD REPLACING
              ALL ';'                  BY ' '
              ALL X'00'                BY ' '
              ALL X'09'                BY ';'

           WRITE CONT-OUT-REC          FROM ERCONT-DETAIL-RECORD
           ADD 1                       TO WS-ERCONT-OUT

           .
       0080-EXIT.
           EXIT.

       0110-READ-ERCONT.

           READ ERCONT NEXT RECORD

           IF (ERCONT-FILE-STATUS = '10' OR '23')
              OR (NF-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCONT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERCONT READ ERROR ' ERCONT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT END-OF-INPUT 
              ADD +1                   TO WS-ERCONT-IN
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ERCONT.

           MOVE LOW-VALUES             TO NF-CONTROL-PRIMARY

           MOVE DTE-CLASIC-COMPANY-CD  TO NF-COMPANY-CD

           START ERCONT KEY >= NF-CONTROL-PRIMARY

           IF ERCONT-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCONT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERCONT START ERROR ' ERCONT-FILE-STATUS
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

