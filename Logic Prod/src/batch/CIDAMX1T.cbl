       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CIDAMX1.
       AUTHOR.        CENTRAL STATES HEALTH AND LIFE, TX DIV.
       DATE-COMPILED.

      *REMARKS.

062107******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERACCT       ASSIGN TO ERACCT
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS INDEXED
                               FILE STATUS IS ERACCT-FILE-STATUS
                               RECORD KEY IS ERACCT-KEY.

           SELECT ACCT-OUT     ASSIGN TO ACCTOT
                   ORGANIZATION IS LINE SEQUENTIAL.

110303     SELECT DISK-DATE        ASSIGN TO SYS019.
110303                                                                  

       DATA DIVISION.
       FILE SECTION.

       FD  ACCT-OUT.

       01  ACCT-OUT-REC                PIC X(200).
       01  ACCT-HEAD-REC                PIC X(250).

       FD  ERACCT.

       01  ERACCT-REC-IN.
           05  F                       PIC XX.
           05  ERACCT-KEY.
               10  ERACCT-COMP-CD      PIC X.
               10  ERACCT-CGSA         PIC X(19).
               10  ERACCT-EXP-DT       PIC XX.
               10  F                   PIC XXXX.
           05  F                       PIC X(1972).

110303 FD  DISK-DATE                                                    
110303                                 COPY ELCDTEFD.
           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     CIDAMX1 WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-ERACCT              VALUE 'Y'.
       77  ERACCT-FILE-STATUS          PIC XX     VALUE '00'.
       77  WS-LO-DT                    PIC XX.
       77  WS-HI-DT                    PIC XX.

                                       COPY ERCACCT.

       01  WS-CONTROL-A                PIC X(19)  VALUE SPACES.
       01  WS-HOLD-ERACCT              PIC X(2000).
       01  WS-HOLD-ERACCT-2            PIC X(2000).
       01  FILLER.
           05  WS-SAVE-ACCT-EXT        PIC X(200)   VALUE SPACES.
           05  WS-ERACCT-IN            PIC 9(7)    VALUE ZEROS.
           05  WS-ERACCT-OUT           PIC 9(7)    VALUE ZEROS.




       01  ACCT-EXTR-HEAD.
           05  F                       PIC X(16)
                                  VALUE 'Identifying Code'.
           05  EX-HTAB1                PIC X.
           05  F                       PIC X(12)
                                  VALUE 'Company Name'.
           05  EX-HTAB2                PIC X.
           05  F                       PIC X(15)
                                  VALUE 'Individual Name'.
           05  EX-HTAB3                PIC X.
           05  F                       PIC X(14)
                                  VALUE 'Address Line 1'.
           05  EX-HTAB4                PIC X.
           05  F                       PIC X(14)
                                  VALUE 'Address Line 2'.
           05  EX-HTAB5                PIC X.
           05  F                       PIC X(4)
                                  VALUE 'City'.
           05  EX-HTAB6                PIC X.
           05  F                       PIC X(5)
                                  VALUE 'State'.
           05  EX-HTAB7                PIC X.
           05  F                       PIC X(3)
                                  VALUE 'Zip'.
           05  EX-HTAB8                PIC X.
           05  F                       PIC X(16)
                                  VALUE 'Telephone Number'.




       01  ACCT-EXTR-RECORD.
           05  ACCT-NUMBER             PIC X(10).
           05  ACCT-COL1               PIC X.
           05  ACCT-NAME               PIC X(30).
           05  ACCT-COL2               PIC X.
           05  ACCT-PERSON             PIC X(30).
           05  ACCT-COL3               PIC X.
           05  ACCT-ADDR1              PIC X(30).
           05  ACCT-COL4               PIC X.
           05  ACCT-ADDR2              PIC X(30).
           05  ACCT-COL5               PIC X.
           05  ACCT-CITY               PIC X(28).
           05  ACCT-COL6               PIC X.
           05  ACCT-STATE              PIC XX.
           05  ACCT-COL7               PIC X.
           05  ACCT-ZIP                PIC X(12).
           05  ACCT-COL8               PIC X.
           05  ACCT-TELE               PIC X(12).


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

       PROCEDURE DIVISION.

       0000-BEGIN.

110303*0003-LOAD-DATE-CARD.
110303                             COPY ELCDTERX.

           OPEN INPUT ERACCT OUTPUT ACCT-OUT
           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' ERACCT OPEN ERROR ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           PERFORM 0025-INIT           THRU 0025-EXIT

           PERFORM 0040-PROCESS-FILE   THRU 0040-EXIT UNTIL
                 END-OF-ERACCT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           DISPLAY ' RECORDS IN    ' WS-ERACCT-IN
           DISPLAY ' ACCOUNTS OUT  ' WS-ERACCT-OUT
           GOBACK
           .
       0002-EXIT.
           EXIT.

       0010-PROCESS-INPUT.


           .
       0010-EXIT.
           EXIT.


       0025-INIT.

           MOVE SPACES                 TO ACCT-EXTR-RECORD
           MOVE ';'                    TO ACCT-COL1
                                          ACCT-COL2
                                          ACCT-COL3
                                          ACCT-COL4
                                          ACCT-COL5
                                          ACCT-COL6
                                          ACCT-COL7
                                          ACCT-COL8
                                          EX-HTAB1
                                          EX-HTAB2
                                          EX-HTAB3
                                          EX-HTAB4
                                          EX-HTAB5
                                          EX-HTAB6
                                          EX-HTAB7
                                          EX-HTAB8

           MOVE ACCT-EXTR-RECORD       TO WS-SAVE-ACCT-EXT

           PERFORM 0120-START-ERACCT   THRU 0120-EXIT
           PERFORM 0110-READ-ERACCT    THRU 0110-EXIT

           WRITE ACCT-HEAD-REC         FROM ACCT-EXTR-HEAD


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

           IF ERACCT-CGSA NOT = AM-CONTROL-A
              IF AM-STATE = 'MN'
                 IF WS-HI-DT > X'9D81'
      *          IF (WS-LO-DT < X'9D81')
      *             AND (WS-HI-DT > X'9D81')
                    PERFORM 0070-BUILD-EXTR
                                       THRU 0070-EXIT
                 END-IF
              END-IF
              MOVE ERACCT-REC-IN       TO ACCOUNT-MASTER
              MOVE AM-EFFECTIVE-DT     TO WS-LO-DT
              MOVE AM-EXPIRATION-DT    TO WS-HI-DT
           ELSE
              MOVE ERACCT-REC-IN       TO ACCOUNT-MASTER
              MOVE AM-EXPIRATION-DT    TO WS-HI-DT
           END-IF

           PERFORM 0110-READ-ERACCT    THRU 0110-EXIT

           .
       0040-EXIT.
           EXIT.


       0070-BUILD-EXTR.

           IF (AM-ACCOUNT (10:1) NOT NUMERIC)
              OR (AM-ACCOUNT = '0780MN1579')
              DISPLAY ' BYPASSING ACCOUNT ' AM-STATE ' '
                 AM-ACCOUNT
              GO TO 0070-EXIT
           END-IF

           IF AM-ADDR-CITY (1:13) = 'REFUND DIRECT'
              DISPLAY ' FIXING CITY ON ACCOUNT ' AM-STATE ' '
                 AM-ACCOUNT ' ' AM-ADDR-CITY
              MOVE SPACES              TO AM-ADDR-CITY
           END-IF
           MOVE WS-SAVE-ACCT-EXT       TO ACCT-EXTR-RECORD

           MOVE AM-NAME                TO ACCT-NAME
           MOVE AM-CONTROL-NAME        TO ACCT-PERSON
           MOVE AM-ADDRS               TO ACCT-ADDR1
           MOVE SPACES                 TO ACCT-ADDR2
           MOVE AM-ADDR-CITY           TO ACCT-CITY
           MOVE AM-ADDR-STATE          TO ACCT-STATE
           MOVE AM-ZIP                 TO ACCT-ZIP
           MOVE AM-TEL-NO              TO ACCT-TELE
           MOVE AM-ACCOUNT             TO ACCT-NUMBER

           PERFORM 0080-WRITE-ACCT-OUT THRU 0080-EXIT

           .
       0070-EXIT.
           EXIT.

       0080-WRITE-ACCT-OUT.

           WRITE ACCT-OUT-REC          FROM ACCT-EXTR-RECORD
           ADD 1                       TO WS-ERACCT-OUT

           .
       0080-EXIT.
           EXIT.

       0110-READ-ERACCT.

           READ ERACCT NEXT RECORD

           IF (ERACCT-FILE-STATUS = '10' OR '23')
                           OR
110303        (ERACCT-COMP-CD > DTE-CLASIC-COMPANY-CD)
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

           MOVE LOW-VALUES             TO ERACCT-KEY

110303*    MOVE X'04'       TO AM-COMPANY-CD
110303     MOVE DTE-CLASIC-COMPANY-CD  TO ERACCT-COMP-CD

           START ERACCT KEY >= ERACCT-KEY

           IF (ERACCT-FILE-STATUS = '10' OR '23')
                           OR
110303        (ERACCT-COMP-CD > DTE-CLASIC-COMPANY-CD)
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

