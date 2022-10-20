       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PEMLACR1.
      *AUTHOR.        PABLO.
      *               COLLEYVILE TEXAS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT REPORT-EXT           ASSIGN TO SYS011
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ERARCH-IN ASSIGN TO ARCHIN.

       DATA DIVISION.
       FILE SECTION.

       FD  REPORT-EXT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  REPORT-RECORD               PIC X(90).

       FD  ERARCH-IN
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

                                       COPY ERCARCH.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMLACR1  WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '*********VMOD=2.001*************'.
       77  WS-NOV-PRINT                PIC 9(7) VALUE ZEROS.
       77  WS-NOV-REPRINT              PIC 9(7) VALUE ZEROS.
       77  WS-DEC-PRINT                PIC 9(7) VALUE ZEROS.
       77  WS-DEC-REPRINT              PIC 9(7) VALUE ZEROS.
       77  WS-JAN-PRINT                PIC 9(7) VALUE ZEROS.
       77  WS-JAN-REPRINT              PIC 9(7) VALUE ZEROS.

       01  WS-HEAD-LINE.
           05  FILLER                  PIC X(25) VALUE 
            ' CAR  CLM NO   CERT NO   '.
           05  FILLER                  PIC X(35) VALUE
            ' NC  RE DT   PROC   CR DT   PRT DT '.
           05  FILLER                  PIC X(29) VALUE
            '  RES PRT   TRLR ARCH NO  CNT'.


       01  WS-DETAIL-LINE.
           05  FILLER                  PIC XX   VALUE SPACES.
           05  D1-CARRIER              PIC XXX.
           05  D1-ACCT-NO              PIC X(8).
           05  D1-CERT-NO              PIC X(12).
           05  D1-NOC                  PIC ZZ9.
           05  FILLER                  PIC XX.
           05  D1-RESENT1-DATE         PIC X(9).
           05  D1-PROCESSOR-CD         PIC X(5).
           05  D1-CREATION-DT          PIC X(9).
           05  D1-INITIAL-PRINT-DATE   PIC X(9).
           05  D1-RESEND-PRINT-DATE    PIC X(9).
           05  D1-CORR-TRLR-SEQ        PIC Z9999.
           05  D1-ARCH-NO              PIC Z9999999.
           05  D1-ARCH-CNT             PIC ZZ99.

       01  WS-FORM-INFO.
           05  WS-CARRIER              PIC X.
           05  WS-CLAIM-NO             PIC X(7).
           05  WS-CERT-NO              PIC X(11).
           05  WS-NO-OF-COPIES         PIC S9.
           05  WS-RESEND-DATE          PIC XX.
           05  WS-PROCESSOR-CD         PIC X(4).
           05  WS-CREATION-DT          PIC XX.
           05  WS-INITIAL-PRINT-DATE   PIC XX.
           05  WS-RESEND-PRINT-DATE    PIC XX.
           05  WS-CORR-TRLR-SEQ        PIC S9(4)    COMP.
           05  WS-1ST-RESEND-PRINT-DT  PIC XX.

       01  SV-FORM-INFO.
           05  SV-CARRIER              PIC X.
           05  SV-CLAIM-NO             PIC X(7).
           05  SV-CERT-NO              PIC X(11).
           05  SV-NO-OF-COPIES         PIC S9.
           05  SV-RESEND-DATE          PIC XX.
           05  SV-PROCESSOR-CD         PIC X(4).
           05  SV-CREATION-DT          PIC XX.
           05  SV-INITIAL-PRINT-DATE   PIC XX.
           05  SV-RESEND-PRINT-DATE    PIC XX.
           05  SV-CORR-TRLR-SEQ        PIC S9(4)    COMP.
           05  SV-1ST-RESEND-PRINT-DT  PIC XX.

       01  MISC.
           12  WS-SPACES               PIC X(5)  VALUE SPACES.
           12  QSAM-IN-CNT             PIC 9(11) VALUE ZEROS.
           12  WS-ARCH-CNT             PIC S9(5) COMP-3 VALUE +0.
           12  REC-1-CNT               PIC 9(9) VALUE ZEROS.
           12  REC-2-CNT               PIC 9(9) VALUE ZEROS.
           12  REC-3-CNT               PIC 9(9) VALUE ZEROS.
           12  REC-4-CNT               PIC 9(9) VALUE ZEROS.
           12  REC-O-CNT               PIC 9(9) VALUE ZEROS.
           12  WS-DISPLAY-ARCH-CNT     PIC ZZ,Z99 VALUE ZEROS.
           12  WS-DISPLAY-ARCH-NO      PIC 9(7) VALUE ZEROS.
           12  WS-SAVE-ARCH-NO         PIC S9(8) COMP VALUE +0.
           12  WS-ERARCH-SW            PIC X VALUE ' '.
               88  END-OF-ERARCH             VALUE 'Y'.

           12  WS-RETURN-CODE      PIC X(4)            VALUE ZEROS.     EL545
           12  ABEND-OPTION        PIC X               VALUE 'Y'.       EL545
           12  WS-ABEND-MESSAGE    PIC X(80)           VALUE SPACES.    EL545
           12  WS-ABEND-FILE-STATUS PIC XX             VALUE SPACES.    EL545
           12  WS-ZERO             PIC S9              VALUE ZERO.      EL545
           12  PGM-SUB             PIC S999    COMP-3  VALUE +545.      EL545

                                       COPY ELCDATE.
                                       
       PROCEDURE DIVISION.

       0000-BEGIN-PROGRAM.

           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT

           PERFORM 0140-PROCESS        THRU 0140-EXIT UNTIL
              (END-OF-ERARCH)
      *       OR (REC-3-CNT > 1000000)

           PERFORM 0200-CLOSE-FILES    THRU 0200-EXIT

           DISPLAY ' TOTAL RECORDS READ   ' QSAM-IN-CNT
           DISPLAY ' HEADER RECORDS   1   ' REC-1-CNT
           DISPLAY ' ADDRESS RECORDS  2   ' REC-2-CNT
           DISPLAY ' TEXT RECORDS     3   ' REC-3-CNT
           DISPLAY ' FORM CNTRL HDR   4   ' REC-4-CNT
           DISPLAY ' ULAR             ?   ' REC-O-CNT

           DISPLAY SPACES
           DISPLAY ' NOV 08 PRINT         ' WS-NOV-PRINT
           DISPLAY ' NOV 08 REPRINTS      ' WS-NOV-REPRINT
           DISPLAY ' DEC 08 PRINT         ' WS-DEC-PRINT
           DISPLAY ' DEC 08 REPRINTS      ' WS-DEC-REPRINT
           DISPLAY ' JAN 09 PRINT         ' WS-JAN-PRINT
           DISPLAY ' JAN 09 REPRINTS      ' WS-JAN-REPRINT

           GOBACK

           .
       0010-OPEN-FILES.

           OPEN  INPUT ERARCH-IN
                OUTPUT REPORT-EXT

           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           MOVE ZEROS                  TO QSAM-IN-CNT

           WRITE REPORT-RECORD         FROM WS-HEAD-LINE

           WRITE REPORT-RECORD         FROM WS-SPACES
           WRITE REPORT-RECORD         FROM WS-SPACES
           PERFORM 0146-READ-ERARCH    THRU 0146-EXIT

           .
       0020-EXIT.
           EXIT.
           
       0140-PROCESS.

           IF ((LA-INITIAL-PRINT-DATE >= X'A341')
              AND (LA-INITIAL-PRINT-DATE < X'A3A1'))
                             OR
              ((LA-SENT-DATE-1 >= X'A341')
              AND (LA-SENT-DATE-1 < X'A3A1'))
                             OR
              ((LA-SENT-DATE-2 >= X'A341')
              AND (LA-SENT-DATE-2 < X'A3A1'))
                             OR
              ((LA-SENT-DATE-3 >= X'A341')
              AND (LA-SENT-DATE-3 < X'A3A1'))
              PERFORM 0150-PRINT-REPORT
                                    THRU 0150-EXIT
           END-IF

           PERFORM 0146-READ-ERARCH    THRU 0146-EXIT

           .
       0140-EXIT.
           EXIT.

       0146-READ-ERARCH.

           READ ERARCH-IN              AT END
              SET END-OF-ERARCH        TO TRUE
           END-READ

           IF NOT END-OF-ERARCH
              ADD 1                    TO QSAM-IN-CNT
           END-IF

           .
       0146-EXIT.
           EXIT.

       0150-PRINT-REPORT.

           EVALUATE TRUE
              WHEN (LA-INITIAL-PRINT-DATE >= X'A341')
                 AND (LA-INITIAL-PRINT-DATE < X'A361')
                 ADD 1                 TO WS-NOV-PRINT
              WHEN (LA-INITIAL-PRINT-DATE >= X'A361')
                 AND (LA-INITIAL-PRINT-DATE < X'A381')
                 ADD 1                 TO WS-DEC-PRINT
              WHEN (LA-INITIAL-PRINT-DATE >= X'A381')
                 AND (LA-INITIAL-PRINT-DATE < X'A3A1')
                 ADD 1                 TO WS-JAN-PRINT
           END-EVALUATE

           EVALUATE TRUE
              WHEN (LA-SENT-DATE-1 >= X'A341')
                 AND (LA-SENT-DATE-1 < X'A361')
                 ADD 1                 TO WS-NOV-REPRINT
              WHEN (LA-SENT-DATE-1 >= X'A361')
                 AND (LA-SENT-DATE-1 < X'A381')
                 ADD 1                 TO WS-DEC-REPRINT
              WHEN (LA-SENT-DATE-1 >= X'A381')
                 AND (LA-SENT-DATE-1 < X'A3A1')
                 ADD 1                 TO WS-JAN-REPRINT
           END-EVALUATE

           EVALUATE TRUE
              WHEN (LA-SENT-DATE-2 >= X'A341')
                 AND (LA-SENT-DATE-2 < X'A361')
                 ADD 1                 TO WS-NOV-REPRINT
              WHEN (LA-SENT-DATE-2 >= X'A361')
                 AND (LA-SENT-DATE-2 < X'A381')
                 ADD 1                 TO WS-DEC-REPRINT
              WHEN (LA-SENT-DATE-2 >= X'A381')
                 AND (LA-SENT-DATE-2 < X'A3A1')
                 ADD 1                 TO WS-JAN-REPRINT
           END-EVALUATE

           EVALUATE TRUE
              WHEN (LA-SENT-DATE-3 >= X'A341')
                 AND (LA-SENT-DATE-3 < X'A361')
                 ADD 1                 TO WS-NOV-REPRINT
              WHEN (LA-SENT-DATE-3 >= X'A361')
                 AND (LA-SENT-DATE-3 < X'A381')
                 ADD 1                 TO WS-DEC-REPRINT
              WHEN (LA-SENT-DATE-3 >= X'A381')
                 AND (LA-SENT-DATE-3 < X'A3A1')
                 ADD 1                 TO WS-JAN-REPRINT
           END-EVALUATE

           MOVE ZEROS                  TO D1-CORR-TRLR-SEQ
           MOVE LA-CARRIER-A2          TO D1-CARRIER
           MOVE LA-ACCOUNT-A2          TO D1-ACCT-NO
           MOVE LA-CERT-NO-A2          TO D1-CERT-NO
           MOVE LA-ARCHIVE-NO          TO D1-ARCH-NO
           MOVE WS-ARCH-CNT            TO D1-ARCH-CNT
           MOVE LA-NO-OF-COPIES        TO D1-NOC
           MOVE LA-PROCESSOR-CD        TO D1-PROCESSOR-CD

           IF LA-SENT-DATE-1 NOT = LOW-VALUES
              MOVE LA-SENT-DATE-1      TO DC-BIN-DATE-1
              PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO D1-RESENT1-DATE
              ELSE
                 MOVE '00/00/00'       TO D1-RESENT1-DATE
              END-IF
           END-IF
           
           IF LA-CREATION-DATE NOT = LOW-VALUES
              MOVE LA-CREATION-DATE    TO DC-BIN-DATE-1
              PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO D1-CREATION-DT
              ELSE
                 MOVE '00/00/00'       TO D1-CREATION-DT
              END-IF
           END-IF
           
           IF LA-INITIAL-PRINT-DATE NOT = LOW-VALUES
              MOVE LA-INITIAL-PRINT-DATE
                                       TO DC-BIN-DATE-1
              PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO D1-INITIAL-PRINT-DATE
              ELSE
                 MOVE '00/00/00'       TO D1-INITIAL-PRINT-DATE
              END-IF
           END-IF
           
           IF LA-SENT-DATE-2 NOT = LOW-VALUES
              MOVE LA-SENT-DATE-2      TO DC-BIN-DATE-1
              PERFORM 8500-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO D1-RESEND-PRINT-DATE
              ELSE
                 MOVE '00/00/00'       TO D1-RESEND-PRINT-DATE
              END-IF
           END-IF

           WRITE REPORT-RECORD         FROM WS-DETAIL-LINE
           WRITE REPORT-RECORD         FROM WS-SPACES
           
           MOVE ZEROS                  TO WS-ARCH-CNT

           .
       0150-EXIT.
           EXIT.
           
       0200-CLOSE-FILES.

           CLOSE ERARCH-IN
                 REPORT-EXT

           .
       0200-EXIT.
           EXIT.
           
       8500-DATE-CONVERSION. COPY ELCDCS.                       

       ABEND-PGM SECTION.
                                COPY ELCABEND.

