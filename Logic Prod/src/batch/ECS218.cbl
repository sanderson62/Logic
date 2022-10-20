       IDENTIFICATION DIVISION.
       PROGRAM-ID. ECS218.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CREDIT         ASSIGN TO SYS011.
           SELECT NON-CREDIT     ASSIGN TO SYS012.
           SELECT PRNTR          ASSIGN TO SYS008.
           SELECT CERT-FILE      ASSIGN TO SYS010.
           SELECT DISK-DATE      ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  CREDIT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  CREDIT-REC              PIC X(1056).

       FD  NON-CREDIT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  NON-CREDIT-REC          PIC X(1056).


       FD  PRNTR
                                   COPY ELCPRTFD.

       FD  CERT-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

                                   COPY ECSCRT01.
       EJECT
       FD  DISK-DATE
                                   COPY ELCDTEFD.
       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '    ECS218 WORKING STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  PGM-SUB                 PIC S999    COMP    VALUE +158.
       77  WS-SPLIT-CNT            PIC 9(5)  VALUE ZEROS.

       01  WS-NON-CREDIT-TABLES.
           12  WS-LIFE-TABLE.
               16  FILLER         PIC XX  VALUE 'QD'.
               16  FILLER         PIC XX  VALUE 'QL'.
               16  FILLER         PIC XX  VALUE '29'.
               16  FILLER         PIC XX  VALUE '32'.
               16  FILLER         PIC XX  VALUE '33'.
               16  FILLER         PIC XX  VALUE '38'.
               16  FILLER         PIC XX  VALUE '45'.
               16  FILLER         PIC XX  VALUE '46'.
               16  FILLER         PIC XX  VALUE '76'.
               16  FILLER         PIC XX  VALUE '77'.
               16  FILLER         PIC XX  VALUE '80'.
               16  FILLER         PIC XX  VALUE '81'.
           12  WS-LIFE REDEFINES WS-LIFE-TABLE OCCURS 12
                            INDEXED BY WS-LF-NDX.
               16  WS-LIFE-BEN   PIC XX.
           12  WS-AH-TABLE.
               16  FILLER         PIC XX  VALUE '38'.
               16  FILLER         PIC XX  VALUE '39'.
               16  FILLER         PIC XX  VALUE '40'.
               16  FILLER         PIC XX  VALUE '41'.
               16  FILLER         PIC XX  VALUE '42'.
               16  FILLER         PIC XX  VALUE '63'.
               16  FILLER         PIC XX  VALUE '64'.
               16  FILLER         PIC XX  VALUE '65'.
               16  FILLER         PIC XX  VALUE '66'.
               16  FILLER         PIC XX  VALUE '67'.
               16  FILLER         PIC XX  VALUE '2C'.
               16  FILLER         PIC XX  VALUE '2D'.
               16  FILLER         PIC XX  VALUE '2E'.
               16  FILLER         PIC XX  VALUE '2F'.
               16  FILLER         PIC XX  VALUE '2G'.
               16  FILLER         PIC XX  VALUE '2H'.
               16  FILLER         PIC XX  VALUE '2I'.
               16  FILLER         PIC XX  VALUE '2J'.
               16  FILLER         PIC XX  VALUE '2K'.
               16  FILLER         PIC XX  VALUE '2L'.
               16  FILLER         PIC XX  VALUE '2Q'.
               16  FILLER         PIC XX  VALUE '2R'.
               16  FILLER         PIC XX  VALUE '2S'.
               16  FILLER         PIC XX  VALUE '2T'.
               16  FILLER         PIC XX  VALUE '2U'.
               16  FILLER         PIC XX  VALUE '5U'.
               16  FILLER         PIC XX  VALUE '5V'.
               16  FILLER         PIC XX  VALUE '5W'.
               16  FILLER         PIC XX  VALUE '5X'.
               16  FILLER         PIC XX  VALUE '5Y'.
               16  FILLER         PIC XX  VALUE '5Z'.
               16  FILLER         PIC XX  VALUE '6A'.
               16  FILLER         PIC XX  VALUE '6B'.
               16  FILLER         PIC XX  VALUE '6C'.
               16  FILLER         PIC XX  VALUE '6D'.
               16  FILLER         PIC XX  VALUE '6E'.
               16  FILLER         PIC XX  VALUE '6F'.
               16  FILLER         PIC XX  VALUE '6G'.
               16  FILLER         PIC XX  VALUE '6H'.
               16  FILLER         PIC XX  VALUE '6I'.
           12  WS-AH REDEFINES WS-AH-TABLE OCCURS 40
                            INDEXED BY WS-AH-NDX.
               16  WS-AH-BEN     PIC XX.

       01  MISC-WS.
           12  WS-HOLD-CERT        PIC X(1056) VALUE SPACES.
           12  X                   PIC X   VALUE ' '.
           12  WS-EOF-SW           PIC X   VALUE ' '.
               88  THERE-ARE-NO-MORE-RECORDS  VALUE 'Y'.
           12  WS-NON-CREDIT       PIC X   VALUE 'N'.
               88  RECORD-NON-CREDIT       VALUE 'Y'.
           12  WS-SPLIT-CERT       PIC X   VALUE 'N'.
               88  SPLIT-RECORD            VALUE 'Y'.
      *    12  WS-CREDIT-120       PIC X   VALUE 'N'.
      *        88  RECORD-CREDIT-120       VALUE 'Y'.
           12  WS-RETURN-CODE      PIC S9(4)   COMP.
           12  WS-ABEND-MESSAGE    PIC X(80).
           12  WS-ABEND-FILE-STATUS PIC XX     VALUE ZEROS.
           12  WS-ZERO             PIC S9       VALUE +0 COMP-3.

       01  HEAD-1.
           12  FILLER              PIC X(42)           VALUE SPACES.
           12  FILLER              PIC X(39)           VALUE
                   'CERTIFICATE MASTER FILE SPLIT          '.
           12  FILLER              PIC X(37)           VALUE SPACES.
           12  FILLER              PIC X(8)         VALUE 'ECS-218 '.

       01  HEAD-2.
           12  FILLER              PIC X(47)           VALUE SPACES.
           12  HD-CLIENT           PIC X(30)           VALUE SPACES.
           12  FILLER              PIC X(42)           VALUE SPACES.
           12  HD-DATE             PIC X(8)            VALUE SPACES.

       01  HEAD-3.
           12  FILLER              PIC X(53)           VALUE SPACES.
           12  HD-ALF-DTE          PIC X(18)           VALUE SPACES.
           12  FILLER              PIC X(48)           VALUE SPACES.
           12  FILLER              PIC X(5)            VALUE 'PAGE'.
           12  HD-PAGE             PIC ZZ,ZZZ.

                                   COPY ELCDTECX.

                                   COPY ELCDTEVR.

       PROCEDURE DIVISION.

       0100-SET-START.
                                   COPY ELCDTERX.

       0110-OPEN-RTN.

           OPEN INPUT  CERT-FILE
                OUTPUT CREDIT NON-CREDIT
                       PRNTR

           PERFORM 0500-PRINT-HEADINGS THRU 0510-HEADING-EXIT

           PERFORM 0130-READ-CERT  THRU 0130-EXIT

           PERFORM 0120-PROCESS-CERT THRU 0120-EXIT UNTIL
              THERE-ARE-NO-MORE-RECORDS

           PERFORM 0600-CLOSE-FILES THRU 0600-EXIT

           DISPLAY ' CERTS SPLIT ' WS-SPLIT-CNT
           GOBACK

           .
       0120-PROCESS-CERT.

           MOVE 'N'                    TO WS-NON-CREDIT
                                          WS-SPLIT-CERT

           SET WS-LF-NDX TO +1

           SEARCH WS-LIFE VARYING WS-LF-NDX AT END
                CONTINUE
             WHEN WS-LIFE-BEN (WS-LF-NDX) = CR-LFTYP
                SET RECORD-NON-CREDIT TO TRUE
           END-SEARCH

           SET WS-AH-NDX TO +1

           SEARCH WS-AH VARYING WS-AH-NDX AT END
                CONTINUE
             WHEN WS-AH-BEN (WS-AH-NDX) = CR-AHTYP
                SET RECORD-NON-CREDIT TO TRUE
           END-SEARCH

           IF ((CR-LF-TERM = 0) OR
              (CR-LF-TERM > 120))
                    AND
              ((CR-AH-TERM = 0) OR
              (CR-AH-TERM > 120))
                SET RECORD-NON-CREDIT TO TRUE
           END-IF

           IF RECORD-NON-CREDIT
              CONTINUE
           ELSE
              IF ((CR-LF-TERM > 120) AND
                 (CR-AH-TERM NOT = 0) AND
                 (CR-AH-TERM < 121))
                       OR
                 ((CR-AH-TERM > 120) AND
                 (CR-LF-TERM NOT = 0) AND
                 (CR-LF-TERM < 121))
                 SET SPLIT-RECORD TO TRUE
              END-IF
           END-IF

           IF SPLIT-RECORD
              ADD 1 TO WS-SPLIT-CNT
              MOVE CERTIFICATE-RECORD  TO WS-HOLD-CERT
              IF CR-LF-TERM > 120
                 MOVE ZEROS            TO CR-AHTYP
                 WRITE NON-CREDIT-REC  FROM CERTIFICATE-RECORD
                 MOVE WS-HOLD-CERT     TO CERTIFICATE-RECORD
                 MOVE ZEROS            TO CR-LFTYP
                 WRITE CREDIT-REC      FROM CERTIFICATE-RECORD
              ELSE
                 MOVE ZEROS            TO CR-LFTYP
                 WRITE NON-CREDIT-REC  FROM CERTIFICATE-RECORD
                 MOVE WS-HOLD-CERT     TO CERTIFICATE-RECORD
                 MOVE ZEROS            TO CR-AHTYP
                 WRITE CREDIT-REC      FROM CERTIFICATE-RECORD
              END-IF
           ELSE
              IF RECORD-NON-CREDIT
                 WRITE NON-CREDIT-REC  FROM CERTIFICATE-RECORD
              ELSE
                 WRITE CREDIT-REC      FROM CERTIFICATE-RECORD
              END-IF
           END-IF

           PERFORM 0130-READ-CERT      THRU 0130-EXIT

           .
       0120-EXIT.
           EXIT.

       0130-READ-CERT.

           READ CERT-FILE AT END
               SET THERE-ARE-NO-MORE-RECORDS TO TRUE
           END-READ

           .
       0130-EXIT.
           EXIT.

       0480-PRT-RTN.

           IF DTE-FICH = SPACE OR '2'
             MOVE X                    TO  P-CTL
             IF P-CTL = ' '
               WRITE PRT AFTER ADVANCING 1 LINE
             ELSE
               IF P-CTL = '0'
                 WRITE PRT AFTER ADVANCING 2 LINES
               ELSE
                 IF P-CTL = '-'
                   WRITE PRT AFTER ADVANCING 3 LINES
                 ELSE
                   WRITE PRT AFTER ADVANCING PAGE.

      ******************************************************************

       0490-E-PRT-RTN.
           EXIT.

       0500-PRINT-HEADINGS.

           MOVE '1'                    TO X
           MOVE HEAD-1                 TO P-DATA
           PERFORM 0480-PRT-RTN        THRU 0490-E-PRT-RTN

           MOVE ' '                    TO X
           MOVE WS-CURRENT-DATE        TO HD-DATE
           MOVE COMPANY-NAME           TO HD-CLIENT
           MOVE HEAD-2                 TO P-DATA
           PERFORM 0480-PRT-RTN        THRU 0490-E-PRT-RTN

           MOVE 1                      TO HD-PAGE
           MOVE ALPH-DATE              TO HD-ALF-DTE
           MOVE HEAD-3                 TO P-DATA
           PERFORM 0480-PRT-RTN        THRU 0490-E-PRT-RTN

           .
       0510-HEADING-EXIT.
           EXIT.

       0600-CLOSE-FILES.

           CLOSE CERT-FILE CREDIT NON-CREDIT PRNTR

           .
       0600-EXIT.
           EXIT.

       ABEND-PGM.
                           COPY ELCABEND SUPPRESS.
