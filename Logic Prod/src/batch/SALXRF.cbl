       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SALXRF.
       AUTHOR.        PABLO.
       DATE-COMPILED.

      *REMARKS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SAL-FILE-IN          ASSIGN TO SYS010.
           SELECT SAL-ACCT             ASSIGN TO SYS012.
           SELECT CID-FILE-OUT         ASSIGN TO SYS011.
           SELECT DISK-DATE            ASSIGN TO SYS019.
           SELECT PRINTX               ASSIGN TO SYS008.

       DATA DIVISION.
       FILE SECTION.

       FD  SAL-FILE-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

                                       COPY ECSCRT01.

       FD  SAL-ACCT 
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

                                       COPY ERCACCT.

       FD  CID-FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  CID-RECORD-OUT              PIC X(1056).

       FD  DISK-DATE
                                       COPY ELCDTEFD.
           EJECT
       FD  PRINTX
                                       COPY ELCPRTFD.
           EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     SALXRF  WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW1                  PIC X  VALUE SPACES.
           88  END-OF-CERT                VALUE 'Y'.
       77  WS-EOF-SW2                  PIC X  VALUE SPACES.
           88  END-OF-ACCT                VALUE 'Y'.
       77  WS-MATCH-SW                 PIC X  VALUE SPACES.
           88  FOUND-MATCH                VALUE 'Y'.
       77  SUB1                        PIC S999   COMP-3 VALUE +0.
       77  ACTNDX                      PIC S9(03) COMP-3 VALUE +0.
       77  PAGE-CTR                    PIC S9(07) VALUE +0  COMP-3.
       77  LINE-CTR                    PIC S9(03) VALUE +99 COMP-3.
       77  X                           PIC X      VALUE ' '.
       77  WS-ACCT-CNT                 PIC 9(9)   VALUE ZEROS.
       77  WS-INPUT-CNT                PIC 9(9)   VALUE ZEROS.
       77  WS-OUTPUT-CNT               PIC 9(9)   VALUE ZEROS.
       77  WS-SAVE-CERT                PIC X(1056).

       01  WS-MISC.
           05  WS-FULL-CONTROL.
               10  FILLER              PIC X(35) VALUE LOW-VALUES.
               10  WS-FULL-SFX         PIC X     VALUE LOW-VALUES.
           05  WS-SFX-TABLE            PIC X(36)   VALUE
               '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
           05  FILLER REDEFINES WS-SFX-TABLE.
               10  WS-CERT-SFX OCCURS 36
                                       PIC X.
           05  SUBSFX                  PIC S999  COMP-3 VALUE +0.
           05  WS-WORK-DAYS            PIC 999   VALUE ZEROS.
           05  WS-WORK-DAYS-X REDEFINES WS-WORK-DAYS.
               10  FILLER              PIC X.
               10  WS-WORK-DAYS-C      PIC XX.
           05  WS-WORK-AMT.
               10  WS-AMT1             PIC 9(8).
               10  FILLER              PIC X.
               10  WS-AMT2             PIC 99.
           05  WS-WORK-AMT-X           PIC X(10).
           05  WS-WORK-AMT-N REDEFINES WS-WORK-AMT-X
                                       PIC 9(8)V99.
           05  WS-WORK-APR-X           PIC X(7).
           05  WS-WORK-APR-N REDEFINES WS-WORK-APR-X
                                       PIC 9(3)V9(4).
           05  WS-WORK-COMM-X          PIC X(8).
           05  WS-WORK-COMM-N REDEFINES WS-WORK-COMM-X
                                       PIC 9(3)V9(5).
           05  WS-BIN-EFF-DT           PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-LF-EXP-DT        PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-AH-EXP-DT        PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-LF-CANC-EXIT-DT  PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-AH-CANC-EXIT-DT  PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-LF-CANC-DT       PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-AH-CANC-DT       PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-LF-CLM-EXIT-DT   PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-AH-CLM-EXIT-DT   PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-1ST-PMT-DT       PIC XX   VALUE LOW-VALUES.
           05  WS-BIN-ENTRY-DT         PIC XX   VALUE LOW-VALUES.
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
                                       COPY ELCDATE.
      /
                                       COPY ELCDTECX.
      /
                                       COPY ELCDTEVR.
      /
       PROCEDURE DIVISION.

       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

           PERFORM 0020-OPEN-FILES     THRU 0020-EXIT

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0080-PROCESS-INPUT  THRU 0080-EXIT UNTIL
                 END-OF-CERT

           PERFORM 0030-CLOSE-FILES    THRU 0030-EXIT

           GOBACK

           .
       0010-INITIALIZE.

           PERFORM 0060-READ-CERT      THRU 0060-EXIT
           PERFORM 0070-READ-ACCT      THRU 0070-EXIT

           .
       0010-EXIT.
           EXIT.

       0020-OPEN-FILES.

           OPEN INPUT SAL-FILE-IN SAL-ACCT
               OUTPUT CID-FILE-OUT

           .
       0020-EXIT.
           EXIT.

       0030-CLOSE-FILES.

           DISPLAY ' ACCT RECS READ        ' WS-ACCT-CNT
           DISPLAY ' SALA RECORDS INPUT    ' WS-INPUT-CNT
           DISPLAY ' CERT RECORDS OUTPUT   ' WS-OUTPUT-CNT
      *    DISPLAY ' NOTE RECORDS OUTPUT   ' WS-NOTE-OUTPUT-CNT


           CLOSE SAL-FILE-IN CID-FILE-OUT SAL-ACCT

           .
       0030-EXIT.
           EXIT.

       0060-READ-CERT.

           READ SAL-FILE-IN AT END
               SET END-OF-CERT         TO TRUE
           END-READ

           IF NOT END-OF-CERT 
              ADD +1                   TO WS-INPUT-CNT
           END-IF

           .

       0060-EXIT.
           EXIT.

       0070-READ-ACCT.

           READ SAL-ACCT AT END
               SET END-OF-ACCT         TO TRUE
           END-READ

           IF NOT END-OF-ACCT
              ADD +1                   TO WS-ACCT-CNT
           END-IF

           .

       0070-EXIT.
           EXIT.

       0075-MATCH-FILES.
        
           IF CR-ACCT-CONTROL > AM-CONTROL-A
              PERFORM 0070-READ-ACCT   THRU 0070-EXIT
           ELSE
              IF AM-CONTROL-A > CR-ACCT-CONTROL
                 DISPLAY 'NO ACCT FOR CERT ' CR-ACCT-CONTROL
                    '  ' CR-DT '   ' CR-CERT-NO ' BYPASSING '
      *          PERFORM 0085-WRITE-CERT
      *                                THRU 0085-EXIT
                 PERFORM 0060-READ-CERT
                                       THRU 0060-EXIT
              ELSE
                 IF CR-DT NOT < AM-EXPIRE-DT
                    PERFORM 0070-READ-ACCT
                                       THRU 0070-EXIT
                 ELSE
                    IF AM-EFFECT-DT > CR-DT
                       DISPLAY 'NO ACCT FOR CERT ' CR-ACCT-CONTROL
                          '  ' CR-DT '   ' CR-CERT-NO ' BYPASSING '
      *                PERFORM 0085-WRITE-CERT
      *                                THRU 0085-EXIT
                       PERFORM 0060-READ-CERT
                                       THRU 0060-EXIT
                    ELSE
                       SET FOUND-MATCH  TO TRUE
                    END-IF
                 END-IF
              END-IF
           END-IF

           .
       0075-EXIT.
           EXIT.
           
       0080-PROCESS-INPUT.

           PERFORM 0075-MATCH-FILES    THRU 0075-EXIT UNTIL
              FOUND-MATCH

           MOVE AM-REI-TABLE           TO CR-REIN-TABLE
           
           PERFORM 0085-WRITE-CERT     THRU 0085-EXIT

           MOVE ' '                    TO WS-MATCH-SW
           PERFORM 0060-READ-CERT      THRU 0060-EXIT

           .
       0080-EXIT.
           EXIT.

       0085-WRITE-CERT.
       
           WRITE CID-RECORD-OUT        FROM CERTIFICATE-RECORD
           ADD +1                      TO WS-OUTPUT-CNT

           .
       0085-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.




       ABEND-PGM.
                                       COPY ELCABEND.

           EJECT
