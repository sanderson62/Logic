       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDBLX1.
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
121508* 121508  CR2008111000003  PEMA  ADD ACCOUNT NAME
032612* 032612   2011110200001   PEMA  AHL CHANGES
100208******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERCOBI       ASSIGN TO ERCOBI
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERCOBI-FILE-STATUS
                               RECORD KEY IS BL-CONTROL-PRIMARY.

           SELECT DISK-DATE    ASSIGN TO SYS019.

           SELECT COBI-OUT     ASSIGN TO COBIOT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  COBI-OUT
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.

       01  COBI-OUT-REC                PIC X(571).

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERCOBI.

                                       COPY ERCCOBI.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDBLX1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       01  WS-MISC.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-INPUT                   VALUE 'Y'.
           05  ERCOBI-FILE-STATUS      PIC XX     VALUE '00'.
           05  SUB1            COMP-3  PIC S9(3)  VALUE +0.

           05  WS-ERCOBI-IN            PIC 9(7)   VALUE ZEROS.
           05  WS-ERCOBI-OUT           PIC 9(7)   VALUE ZEROS.

       01  WS-SAVE-ERCOBI              PIC X(571) VALUE LOW-VALUES.
       01  ERCOBI-DETAIL-RECORD.
           12  EX-STMT-OWNER           PIC X(4).
           12  EX-TAB1                 PIC X.
           12  EX-REPORT-GROUP-ID      PIC X(12).
           12  EX-TAB2                 PIC X.
           12  EX-ACCT-NAME            PIC X(35).
           12  EX-TAB3                 PIC X.
           12  EX-CONTACT-NAME         PIC X(35).
           12  EX-TAB4                 PIC X.
           12  EX-CONTACT-ADDR1        PIC X(30).
           12  EX-TAB5                 PIC X.
           12  EX-CONTACT-ADDR2        PIC X(30).
           12  EX-TAB6                 PIC X.
           12  EX-CONTACT-CITY         PIC X(30).
           12  EX-TAB7                 PIC X.
           12  EX-CONTACT-STATE        PIC XX.
           12  EX-TAB8                 PIC X.
           12  EX-CONTACT-ZIP          PIC X(9).
           12  EX-TAB9                 PIC X.
           12  EX-CHECK-HANDLING       PIC X.
           12  EX-TAB10                PIC X.
           12  EX-SPEC-INSTRUCTIONS-L1 PIC X(70).
           12  EX-TAB11                PIC X.
           12  EX-SPEC-INSTRUCTIONS-L2 PIC X(70).
           12  EX-TAB12                PIC X.
           12  EX-SPEC-INSTRUCTIONS-L3 PIC X(70).
           12  EX-TAB13                PIC X.
           12  EX-SPEC-INSTRUCTIONS-L4 PIC X(70).
           12  EX-TAB14                PIC X.
           12  EX-SPEC-INSTRUCTIONS-L5 PIC X(70).
           12  EX-TAB15                PIC X.
           12  EX-LAST-MAINT-DT        PIC X(10).
           12  EX-TAB16                PIC X.
           12  EX-LAST-MAINT-BY        PIC XXXX.
           12  EX-TAB17                PIC X.
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
PEMTST*         OR (WS-ERCOBI-IN > 1000)

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-ERCOBI-IN
           DISPLAY ' RECORDS  OUT  ' WS-ERCOBI-OUT
           GOBACK

           .
       0002-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ERCOBI
               OUTPUT COBI-OUT

           IF ERCOBI-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCOBI OPEN ERROR ' ERCOBI-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ERCOBI COBI-OUT

           IF ERCOBI-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERCOBI CLOSE ERROR ' ERCOBI-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0030-EXIT.
           EXIT.

       0040-INIT.

           MOVE SPACES                 TO ERCOBI-DETAIL-RECORD
052704     MOVE X'09'                  TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
                                          EX-TAB9
                                          EX-TAB10
                                          EX-TAB11
                                          EX-TAB12
                                          EX-TAB13
                                          EX-TAB14
                                          EX-TAB15
                                          EX-TAB16
                                          EX-TAB17
           MOVE 'E'                    TO EX-EOR

           MOVE ERCOBI-DETAIL-RECORD   TO WS-SAVE-ERCOBI
           PERFORM 0120-START-ERCOBI   THRU 0120-EXIT
032612     if not end-of-input
              PERFORM 0110-READ-ERCOBI THRU 0110-EXIT
032612     end-if

           .
       0040-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           MOVE WS-SAVE-ERCOBI         TO ERCOBI-DETAIL-RECORD

           MOVE BL-STMT-OWNER          TO EX-STMT-OWNER
           MOVE BL-REPORT-GROUP-ID     TO EX-REPORT-GROUP-ID
           MOVE BL-ACCOUNT-NAME        TO EX-ACCT-NAME
           MOVE BL-CONTACT-NAME        TO EX-CONTACT-NAME
           MOVE BL-ADDR1               TO EX-CONTACT-ADDR1
           MOVE BL-ADDR2               TO EX-CONTACT-ADDR2
           MOVE BL-CITY                TO EX-CONTACT-CITY
           MOVE BL-STATE               TO EX-CONTACT-STATE
           MOVE BL-ZIP                 TO EX-CONTACT-ZIP
           MOVE BL-CHECK-HANDLING      TO EX-CHECK-HANDLING
           MOVE BL-SI-LINE-1           TO EX-SPEC-INSTRUCTIONS-L1
           MOVE BL-SI-LINE-2           TO EX-SPEC-INSTRUCTIONS-L2
           MOVE BL-SI-LINE-3           TO EX-SPEC-INSTRUCTIONS-L3
           MOVE BL-SI-LINE-4           TO EX-SPEC-INSTRUCTIONS-L4
           MOVE BL-SI-LINE-5           TO EX-SPEC-INSTRUCTIONS-L5

           MOVE BL-LAST-MAINT-USER     TO EX-LAST-MAINT-BY

           MOVE BL-LAST-MAINT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-MAINT-DT
           ELSE
              MOVE SPACES              TO EX-LAST-MAINT-DT
           END-IF

           PERFORM 0080-WRITE-COBI-OUT THRU 0080-EXIT

           PERFORM 0110-READ-ERCOBI    THRU 0110-EXIT

           .
       0050-EXIT.
           EXIT.

       0080-WRITE-COBI-OUT.

           INSPECT ERCOBI-DETAIL-RECORD REPLACING
              ALL ';'                  BY ' '
              ALL X'00'                BY ' '
              ALL X'09'                BY ';'

           WRITE COBI-OUT-REC          FROM ERCOBI-DETAIL-RECORD
           ADD 1                       TO WS-ERCOBI-OUT

           .
       0080-EXIT.
           EXIT.

       0110-READ-ERCOBI.

           READ ERCOBI NEXT RECORD

           IF (ERCOBI-FILE-STATUS = '10' OR '23')
              OR (BL-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCOBI-FILE-STATUS NOT = '00'
                 DISPLAY ' ERCOBI READ ERROR ' ERCOBI-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT END-OF-INPUT 
              ADD +1                   TO WS-ERCOBI-IN
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ERCOBI.

           MOVE LOW-VALUES             TO BL-CONTROL-PRIMARY

           MOVE DTE-CLASIC-COMPANY-CD  TO BL-COMPANY-CD

           START ERCOBI KEY >= BL-CONTROL-PRIMARY

           IF ERCOBI-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERCOBI-FILE-STATUS NOT = '00'
                 DISPLAY ' ERCOBI START ERROR ' ERCOBI-FILE-STATUS
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

