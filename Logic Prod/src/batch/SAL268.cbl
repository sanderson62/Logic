       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SALACCT.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ACCT-FILE-IN         ASSIGN TO SYS010.
           SELECT SORT-FILE            ASSIGN TO SORTWK1.
           SELECT COMP-FILE-OUT        ASSIGN TO SYS011.
           SELECT DISK-DATE            ASSIGN TO SYS019.
           SELECT PRINTX               ASSIGN TO SYS008.

       DATA DIVISION.
       FILE SECTION.

       FD  ACCT-FILE-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

                                       COPY ERCACCT.
           

           EJECT
       SD  SORT-FILE. 

       01  SORT-RECORD.
           05  FILLER                  PIC XX.
           05  SORT-KEY                PIC X(29).
           05  FILLER                  PIC X(669).

       FD  COMP-FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  COMP-RECORD-OUT             PIC X(700).

       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  PRINTX
                                       COPY ELCPRTFD.
           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     SALACCT  WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  SUB1                        PIC S999   COMP-3 VALUE +0.
       77  BUSNDX                      PIC S9(03) COMP-3 VALUE +0.
       77  ACTNDX                      PIC S9(03) COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  WS-INPUT-CNT                PIC 9(9)   VALUE ZEROS.
       77  WS-OUTPUT-CNT               PIC 9(9)   VALUE ZEROS.
       77  WS-INIT-COMP                PIC X(700).
       77  WS-SAVE-COMP                PIC X(700).
       77  WS-SAVE-KEY                 PIC X(29)  VALUE LOW-VALUES.

       01  WS-MISC.
           05  WS-WORK-DATE            PIC X(8).
           05  WS-WORK-DATE-N REDEFINES WS-WORK-DATE
                                       PIC 9(8).
           05  WS-DISPLAY-DATE         PIC ZZZ9(8).
           05  WS-DISPLAY-TERM         PIC Z99.
           05  WS-DISPLAY-RTERM        PIC Z99.99.
           05  WS-DISPLAY-PRM          PIC ZZZZZZZ.99.
           05  WS-DISPLAY-UEP          PIC ZZZZZZZ.99.

           EJECT

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

           EJECT
                                       COPY ERCCOMP.
                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.
      /
       PROCEDURE DIVISION.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           SORT SORT-FILE ON ASCENDING KEY SORT-KEY
                INPUT PROCEDURE 0002-INPUT
                                       THRU 0002-EXIT
                OUTPUT PROCEDURE 0003-OUTPUT
                                       THRU 0003-EXIT

           GOBACK
             .

       0002-INPUT SECTION.

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT

           .
       0002-EXIT.
           EXIT.

       0003-OUTPUT SECTION.

           MOVE ' '                    TO WS-EOF-SW

           PERFORM 0070-RETURN-REC     THRU 0070-EXIT

           PERFORM 0075-PROCESS-RECS THRU 0075-EXIT UNTIL
                END-OF-INPUT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           .
       0003-EXIT.
           EXIT.


       0010-INITIALIZE.

           MOVE SPACES                 TO COMPENSATION-MASTER
           INITIALIZE                     COMPENSATION-MASTER
           MOVE 'CO'                   TO CO-RECORD-ID
           MOVE X'04'                  TO CO-COMPANY-CD
           MOVE 'CONV'                 TO CO-LAST-MAINT-USER
           MOVE +200000                TO CO-LAST-MAINT-HHMMSS
           MOVE X'9B41'                TO CO-LAST-MAINT-DT
           MOVE LOW-VALUES             TO CO-ROLADEX-PRINT-DT
                                          CO-LAST-EOM-STMT-DT
                                          CO-GA-EFFECTIVE-DT
                                          CO-GA-TERMINATION-DT
           MOVE ZEROS                  TO CO-LAST-ACTIVITY-DATE
                                          CO-LAST-STMT-DT
                                          CO-CURRENT-LAST-STMT-DT
                                          CO-FAXNO
                                          CO-TELEPHONE
           MOVE 'B'                    TO CO-BILL-SW
           MOVE COMPENSATION-MASTER    TO WS-INIT-COMP

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT ACCT-FILE-IN 
               OUTPUT COMP-FILE-OUT

           .

       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' ACCT RECORDS INPUT    ' WS-INPUT-CNT
           DISPLAY ' COMP RECORDS OUTPUT   ' WS-OUTPUT-CNT

           CLOSE ACCT-FILE-IN COMP-FILE-OUT

           .
       0030-EXIT.
           EXIT.

       0050-PROCESS-INPUT.

           PERFORM 0080-PROCESS-INPUT  THRU 0080-EXIT UNTIL
                 END-OF-INPUT

           .
       0050-EXIT.
           EXIT.

       0060-READ-INPUT.

           READ ACCT-FILE-IN AT END
               SET END-OF-INPUT        TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-INPUT-CNT
           END-IF

           .
       0060-EXIT.
           EXIT.

       0070-RETURN-REC.

           RETURN SORT-FILE     AT END
               SET END-OF-INPUT        TO TRUE
           END-RETURN

           .

       0070-EXIT.
           EXIT.

       0075-PROCESS-RECS.

           IF SORT-KEY > WS-SAVE-KEY
              MOVE SORT-RECORD         TO COMP-RECORD-OUT
              WRITE COMP-RECORD-OUT
              ADD 1                    TO WS-OUTPUT-CNT
              MOVE SORT-KEY            TO WS-SAVE-KEY
           END-IF

           PERFORM 0070-RETURN-REC THRU 0070-EXIT

           .
       0075-EXIT.
           EXIT.

       0080-PROCESS-INPUT.

           MOVE WS-INIT-COMP           TO COMPENSATION-MASTER
           
           MOVE AM-CARRIER             TO CO-CARRIER
           MOVE AM-GROUPING            TO CO-GROUPING

           IF AM-REMIT-TO = ZEROS OR SPACES OR LOW-VALUES
              MOVE 01                  TO AM-REMIT-TO
           END-IF

           MOVE AM-AGT (AM-REMIT-TO)   TO CO-RESP-NO
           MOVE AM-AGT (01)            TO CO-ACCOUNT
           MOVE 'A'                    TO CO-TYPE
           MOVE 'Y'                    TO CO-BALANCE-CONTROL
           MOVE AM-NAME                TO CO-ACCT-NAME 
           MOVE AM-PERSON              TO CO-MAIL-NAME 
           MOVE AM-ADDRS               TO CO-ADDR-1 
           MOVE AM-CITY                TO CO-ADDR-3 
           MOVE AM-ZIP                 TO CO-ZIP 
           MOVE AM-CSR-CODE            TO CO-CSR-CODE 
           MOVE AM-TEL-NO              TO CO-TELEPHONE 
           MOVE AM-ID-NO               TO CO-SOC-SEC 
           IF AM-ACCOUNT = '0001013600'
              MOVE 'Y'                 TO CO-CSO-1099
           END-IF
           MOVE COMPENSATION-MASTER    TO WS-SAVE-COMP

           PERFORM 0090-RELEASE        THRU 0090-EXIT

           IF AM-AGT (2) NOT = SPACES AND ZEROS AND LOW-VALUES
              MOVE ' '                 TO CO-CSO-1099
              MOVE WS-SAVE-COMP        TO COMPENSATION-MASTER
              MOVE LOW-VALUES          TO CO-ACCOUNT
              MOVE AM-AGT (2)          TO CO-RESP-NO
              MOVE 'G'                 TO CO-TYPE
              MOVE 'N'                 TO CO-BALANCE-CONTROL
              MOVE COMPENSATION-MASTER TO WS-SAVE-COMP
              PERFORM 0090-RELEASE     THRU 0090-EXIT
           END-IF

           IF AM-AGT (3) NOT = SPACES AND ZEROS AND LOW-VALUES
              MOVE ' '                 TO CO-CSO-1099
              MOVE WS-SAVE-COMP        TO COMPENSATION-MASTER
              MOVE LOW-VALUES          TO CO-ACCOUNT
              MOVE AM-AGT (3)          TO CO-RESP-NO
              MOVE 'G'                 TO CO-TYPE
              MOVE 'N'                 TO CO-BALANCE-CONTROL
              MOVE COMPENSATION-MASTER TO WS-SAVE-COMP
              PERFORM 0090-RELEASE     THRU 0090-EXIT
           END-IF

           IF AM-AGT (4) NOT = SPACES AND ZEROS AND LOW-VALUES
              MOVE ' '                 TO CO-CSO-1099
              MOVE WS-SAVE-COMP        TO COMPENSATION-MASTER
              MOVE LOW-VALUES          TO CO-ACCOUNT
              MOVE AM-AGT (4)          TO CO-RESP-NO
              MOVE 'G'                 TO CO-TYPE
              MOVE 'N'                 TO CO-BALANCE-CONTROL
              MOVE COMPENSATION-MASTER TO WS-SAVE-COMP
              PERFORM 0090-RELEASE     THRU 0090-EXIT
           END-IF

           IF AM-AGT (5) NOT = SPACES AND ZEROS AND LOW-VALUES
              MOVE ' '                 TO CO-CSO-1099
              MOVE WS-SAVE-COMP        TO COMPENSATION-MASTER
              MOVE LOW-VALUES          TO CO-ACCOUNT
              MOVE AM-AGT (5)          TO CO-RESP-NO
              MOVE 'G'                 TO CO-TYPE
              MOVE 'N'                 TO CO-BALANCE-CONTROL
              MOVE COMPENSATION-MASTER TO WS-SAVE-COMP
              PERFORM 0090-RELEASE     THRU 0090-EXIT
           END-IF

           PERFORM 0060-READ-INPUT     THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-RELEASE.
       
           IF (AM-ACCOUNT (1:5) = '00010')
              AND (AM-LAST-MAINT-USER = 'CONV')
              AND (AM-STATE = 'MN')
              AND (AM-CARRIER = '9')
              RELEASE SORT-RECORD      FROM COMPENSATION-MASTER
           END-IF

           .
       0090-EXIT.
           EXIT.
           
       0110-READ-INPUT.

           READ ACCT-FILE-IN AT END
              SET END-OF-INPUT TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD +1                   TO WS-INPUT-CNT
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
