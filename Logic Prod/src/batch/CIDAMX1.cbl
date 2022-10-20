       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDAMX1.
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE, TX DIV.
       DATE-COMPILED.

      *REMARKS.

062107******************************************************************
062107*                   C H A N G E   L O G
062107*
062107* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062107*-----------------------------------------------------------------
062107*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062107* EFFECTIVE    NUMBER
062107*-----------------------------------------------------------------
062107* 062107   2007010300001   PEMA  CORRECT NAME AND ACCTNO   
062107******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SORT-FILE    ASSIGN TO SORTWK1.
           SELECT ERACCT       ASSIGN TO ERACCT
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERACCT-FILE-STATUS
                               RECORD KEY IS AM-CONTROL-PRIMARY.

           SELECT ACCT-OUT     ASSIGN TO ACCTOT
                   ORGANIZATION IS LINE SEQUENTIAL.

110303     SELECT DISK-DATE        ASSIGN TO SYS019.
110303                                                                  

       DATA DIVISION.
       FILE SECTION.

       SD  SORT-FILE.

       01  SORT-REC.
           12  FILLER                  PIC X(9).
           12  SR-ACCOUNT              PIC X(10).
           12  FILLER                  PIC X(51).

       EJECT
       FD  ACCT-OUT.

       01  ACCT-OUT-REC                PIC X(70).
       FD  ERACCT.

                                       COPY ERCACCT.

110303 FD  DISK-DATE                                                    
110303                             COPY ELCDTEFD.                       
           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDAMX1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-ERACCT              VALUE 'Y'.
       77  ERACCT-FILE-STATUS          PIC XX     VALUE '00'.

       01  FILLER.
           05  WS-SAVE-ACCT-EXT        PIC X(70)   VALUE SPACES.
           05  WS-ERACCT-IN            PIC 9(7)    VALUE ZEROS.
           05  WS-ERACCT-OUT           PIC 9(7)    VALUE ZEROS.

       01  ACCT-EXTR-RECORD.
           05  ACCT-CARRIER            PIC X.
           05  ACCT-COL1               PIC X.
           05  ACCT-GROUPING           PIC X(6).
           05  ACCT-COL2               PIC X.
           05  ACCT-NUMBER             PIC X(10).
           05  ACCT-COL3               PIC X.
           05  ACCT-NAME               PIC X(30).
           05  ACCT-COL4               PIC X.
           05  ACCT-STATE              PIC XX.
           05  ACCT-COL5               PIC X.
           05  ACCT-EFF-JUL            PIC X(7).
           05  ACCT-COL6               PIC X.
           05  ACCT-EXP-JUL            PIC X(7).
           05  ACCT-COL7               PIC X.


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

110303                                 COPY ELCDTECX.
110303
110303                                 COPY ELCDTEVR.
      /
       PROCEDURE DIVISION.

       0000-BEGIN.

110303*0003-LOAD-DATE-CARD.
110303                             COPY ELCDTERX.

           SORT SORT-FILE ASCENDING KEY SR-ACCOUNT
              INPUT PROCEDURE 0010-SORT-IN
                                       THRU 0010-EXIT
              OUTPUT PROCEDURE 0020-SORT-OUT
                                       THRU 0020-EXIT

           IF SORT-RETURN NOT = (ZEROS AND 4)
               MOVE  0101              TO WS-RETURN-CODE
               GO TO ABEND-PGM.

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-ERACCT-IN
           DISPLAY ' ACCOUNTS OUT  ' WS-ERACCT-OUT
           GOBACK
           .
       0002-EXIT.
           EXIT.

       0010-SORT-IN SECTION.

           OPEN INPUT ERACCT
           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERACCT OPEN ERROR ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           PERFORM 0025-INIT           THRU 0025-EXIT

           PERFORM 0040-PROCESS-FILE   THRU 0040-EXIT UNTIL
                 END-OF-ERACCT

           .

       0010-EXIT.
           EXIT.

       0020-SORT-OUT SECTION.

           OPEN OUTPUT ACCT-OUT

           MOVE SPACES                 TO WS-EOF-SW
           PERFORM 0060-RETURN-EXTR    THRU 0060-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                 END-OF-ERACCT

           .

       0020-EXIT.
           EXIT.


       0025-INIT.

           MOVE SPACES                 TO ACCT-EXTR-RECORD
           MOVE ':'                    TO ACCT-COL1
                                          ACCT-COL2
                                          ACCT-COL3
                                          ACCT-COL4
                                          ACCT-COL5
                                          ACCT-COL6
                                          ACCT-COL7
           MOVE ACCT-EXTR-RECORD       TO WS-SAVE-ACCT-EXT

           PERFORM 0120-START-ERACCT   THRU 0120-EXIT
           PERFORM 0110-READ-ERACCT    THRU 0110-EXIT


           .

       0025-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           CLOSE ERACCT ACCT-OUT

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERACCT CLOSE ERROR ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .

       0030-EXIT.
           EXIT.

       0040-PROCESS-FILE.

           PERFORM 0070-BUILD-EXTR     THRU 0070-EXIT
           PERFORM 0047-RELEASE-ACCT   THRU 0047-EXIT

           PERFORM 0110-READ-ERACCT    THRU 0110-EXIT

           .

       0040-EXIT.
           EXIT.

       0047-RELEASE-ACCT.

           RELEASE SORT-REC            FROM ACCT-EXTR-RECORD
      *    ADD 1 TO WS-ERACCT-REL

           .

       0047-EXIT.
           EXIT.

       0050-PROCESS-FILE.

           PERFORM 0080-WRITE-ACCT-OUT THRU 0080-EXIT

           PERFORM 0060-RETURN-EXTR    THRU 0060-EXIT

           .

       0050-EXIT.
           EXIT.

       0060-RETURN-EXTR.

           RETURN SORT-FILE AT END
               SET END-OF-ERACCT       TO TRUE

           .
       0060-EXIT.
           EXIT.

       0070-BUILD-EXTR.

           MOVE WS-SAVE-ACCT-EXT       TO ACCT-EXTR-RECORD

           MOVE AM-CARRIER             TO ACCT-CARRIER
           MOVE AM-GROUPING            TO ACCT-GROUPING
           MOVE AM-ACCOUNT             TO ACCT-NUMBER
           MOVE AM-STATE               TO ACCT-STATE
           MOVE AM-NAME                TO ACCT-NAME

062107     IF ACCT-NUMBER (1:1) NOT = '0'
062107        IF ACCT-NUMBER NUMERIC
062107           MOVE '0'              TO ACCT-NUMBER (1:1)
062107        END-IF
062107     END-IF

062107     INSPECT ACCT-NAME
062107        REPLACING ALL ':' BY SPACES

           IF AM-EXPIRATION-DT = HIGH-VALUES
              MOVE '9999999'           TO ACCT-EXP-JUL
           ELSE
              MOVE AM-EXPIRATION-DT    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-JULIAN-DATE-1
                                       TO ACCT-EXP-JUL
              ELSE
                 MOVE 'ERROR'          TO ACCT-EXP-JUL
              END-IF
           END-IF

           MOVE AM-EFFECTIVE-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-JULIAN-DATE-1    TO ACCT-EFF-JUL
           ELSE
              MOVE 'ERROR'             TO ACCT-EFF-JUL
           END-IF

           .

       0070-EXIT.
           EXIT.

       0080-WRITE-ACCT-OUT.

           WRITE ACCT-OUT-REC          FROM SORT-REC
           ADD 1                       TO WS-ERACCT-OUT

           .

       0080-EXIT.
           EXIT.

       0110-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
                           OR
110303        (AM-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERACCT   TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERACCT READ ERROR ' ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              ELSE
                 ADD 1 TO WS-ERACCT-IN
              END-IF
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-START-ERACCT.

           MOVE LOW-VALUES TO AM-CONTROL-PRIMARY

110303*    MOVE X'04'       TO AM-COMPANY-CD
110303     MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD

           START ERACCT KEY NOT < AM-CONTROL-PRIMARY

           IF (ERACCT-FILE-STATUS = '10' OR '23')
                           OR
110303        (AM-COMPANY-CD > DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERACCT TO TRUE
           ELSE
              IF ERACCT-FILE-STATUS NOT = '00'
                 DISPLAY ' ERACCT START ERROR ' ERACCT-FILE-STATUS
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

           EJECT
