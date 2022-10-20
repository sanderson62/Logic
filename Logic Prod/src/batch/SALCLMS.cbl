       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SALCLMS.
       AUTHOR.        SUZAN VUKOV.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SAL-FILE-IN          ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CID-FILE-OUT         ASSIGN TO SYS011.
           SELECT DISK-DATE            ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  SAL-FILE-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  SAL-CLAIM-RECORD.
           05  SAL-ACCT-NUM            PIC X(10).
           05  SAL-CERT-EFF-DT         PIC X(08).
           05  SAL-CERT-NUM            PIC X(10).
           05  SAL-CLM-TYPE            PIC X(01).
           05  SAL-CLM-PAYMENT         PIC 9(09)V99.
           05  SAL-INCUR-DT            PIC X(08).
           05  SAL-REPORT-DT           PIC X(06).
           05  SAL-CLM-PAID-DT.
               10  SAL-CLM-PAID-CCYY   PIC X(04).
               10  SAL-CLM-PAID-MM     PIC X(02).
               10  SAL-CLM-PAID-DD     PIC X(02).
           05  SAL-CLM-PAID-TO-DT      PIC X(08).
           05  SAL-CLM-NUM             PIC X(07).
           05  SAL-CHK-NUM             PIC X(07).
           05  SAL-DAYS-DISABLE        PIC X(03).
           05  SAL-PAY-TYPE            PIC X(01).
           05  SAL-CLMNT-LAST-NAME     PIC X(15).
           05  SAL-CLMNT-FIRST-NAME    PIC X(10).
           05  SAL-CLMNT-SSN           PIC X(11).
           05  SAL-CLM-CAUSE           PIC X(06).
           05  SAL-DENY-DT             PIC X(08).
           05  SAL-DENY-REASON         PIC X(01).
           

       FD  CID-FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  CID-RECORD-OUT              PIC X(510).

       FD  DISK-DATE
                                       COPY ELCDTEFD.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     SALCLMS  WORKING STORAGE   '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X  VALUE SPACES.
           88  END-OF-INPUT               VALUE 'Y'.
       77  SUB1                        PIC S999   COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  WS-INPUT-CNT                PIC 9(9)   VALUE ZEROS.
       77  WS-OUTPUT-CNT               PIC 9(9)   VALUE ZEROS.

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


       01  MISC-FIELDS.
           12  PGM-SUB                 PIC S999 COMP  VALUE +158.
           12  FIRST-TIME-SW           PIC X  VALUE 'Y'.
               88  FIRST-TIME                 VALUE 'Y'.

           12  WS-DATE-X               PIC X(08).
           12  WS-DATE-9  REDEFINES WS-DATE-X
                                       PIC 9(08).
           EJECT
                                       COPY ECSEXT01.

                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.
      /
       PROCEDURE DIVISION.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

       0010-MAIN.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT
           PERFORM 0100-READ-INPUT     THRU 0100-EXIT
           DISPLAY 'INITIAL READ'
           PERFORM 0080-PROCESS-INPUT  THRU 0080-EXIT 
               UNTIL END-OF-INPUT
           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT
           GOBACK

             .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT SAL-FILE-IN 
               OUTPUT CID-FILE-OUT 

           .
       0020-EXIT.
           EXIT.


       0030-CLOSE-FILES.

           DISPLAY ' SALA RECORDS INPUT    ' WS-INPUT-CNT
           DISPLAY ' CLM RECORDS OUTPUT   ' WS-OUTPUT-CNT

           CLOSE SAL-FILE-IN 
                 CID-FILE-OUT

           .
       0030-EXIT.
           EXIT.


       0080-PROCESS-INPUT.

           DISPLAY 'Into process input ' SAL-CLAIM-RECORD
           PERFORM 0085-INITIALIZE     THRU 0085-EXIT

           MOVE SAL-CLM-TYPE           TO DE-TYPE
           MOVE SAL-ACCT-NUM           TO DE-ACCOUNT

           MOVE SAL-CERT-EFF-DT        TO WS-DATE-X
           MOVE WS-DATE-9              TO DE-EFF

           MOVE SAL-CERT-NUM           TO DE-CERT-NO
           MOVE SAL-CLMNT-LAST-NAME    TO DE-LNAME
           MOVE SAL-CLMNT-FIRST-NAME   TO DE-FNAME
           MOVE SAL-CLMNT-SSN          TO DE-SOC-SEC-NO
           MOVE SAL-CLM-PAYMENT        TO DE-CLAIM-AMT

           MOVE SAL-INCUR-DT           TO WS-DATE-X
           MOVE WS-DATE-9              TO DE-INCUR

           MOVE SAL-REPORT-DT          TO WS-DATE-X
           MOVE WS-DATE-9              TO DE-REPORTED

           MOVE SAL-CLM-PAID-DT        TO WS-DATE-X
           MOVE WS-DATE-9              TO DE-PAY


           IF SAL-CLM-PAID-DT NOT = ZEROS
              AND SPACES
               MOVE SAL-CLM-PAID-DT        TO DC-GREG-DATE-CYMD
               MOVE 'L'                    TO DC-OPTION-CODE
               PERFORM 8510-DATE-CONVERSION
               IF NO-CONVERSION-ERROR
                   MOVE DC-DAYS-IN-MONTH   TO SAL-CLM-PAID-DD
                   DISPLAY 'DAYS in mo ' DC-DAYS-IN-MONTH
               ELSE
                   DISPLAY 'DATE CONVERSION ERR' SAL-CLM-PAID-DT
                   PERFORM ABEND-PGM    
               END-IF
           ELSE
               DISPLAY ' PAID DT was blank ' SAL-ACCT-NUM ' '
                           SAL-CERT-NUM SAL-CLAIM-RECORD
           END-IF

           MOVE SAL-CLM-PAID-DT        TO WS-DATE-X
           MOVE WS-DATE-9              TO DE-CLM-PROC-DT

           MOVE SAL-CLM-PAID-TO-DT     TO WS-DATE-X 
           MOVE WS-DATE-9              TO DE-PAID-TO

           MOVE SAL-CLM-NUM            TO DE-CNUM
           MOVE SAL-CHK-NUM            TO DE-CHECK
           MOVE SAL-DAYS-DISABLE       TO DE-DAYS-DISAB
           MOVE SAL-PAY-TYPE           TO DE-PAY-CODE
           MOVE SAL-CLM-CAUSE          TO DE-CLM-CAUSE
           
           IF SAL-CERT-NUM = ZEROS 
              OR  SAL-ACCT-NUM = '0001000100'
              OR  SAL-CERT-NUM = '000000none'
              OR  SAL-DENY-REASON = '2'
               DISPLAY 'NOT WRITING CLAIM REC ' SAL-ACCT-NUM ' '
                        SAL-CERT-NUM ' ' SAL-DENY-REASON
           ELSE 
               WRITE CID-RECORD-OUT    FROM DETAIL-EXTRACT
               DISPLAY 'WRITE'
               ADD +1                  TO WS-OUTPUT-CNT
           END-IF

           PERFORM 0100-READ-INPUT     THRU 0100-EXIT

           .
       0080-EXIT.
           EXIT.

       0085-INITIALIZE.

           INITIALIZE DETAIL-EXTRACT
           MOVE 'DE'                   TO DE-RECORD-ID
           MOVE X'04'                  TO DE-COMPANY-CD
           MOVE '9'                    TO DE-CARRIER
           MOVE '000000'               TO DE-GROUPING
           MOVE 'MN'                   TO DE-STATE
           MOVE 'X'                    TO DE-TRANS

           .
       0085-EXIT.
           EXIT.


       0100-READ-INPUT.

           DISPLAY 'READ'
           READ SAL-FILE-IN AT END
               SET END-OF-INPUT        TO TRUE
               GO TO 0100-EXIT
           END-READ

           ADD +1                      TO WS-INPUT-CNT

           .
       0100-EXIT.
           EXIT.


       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8510-EXIT.
           EXIT.


       ABEND-PGM.
                                       COPY ELCABEND.

