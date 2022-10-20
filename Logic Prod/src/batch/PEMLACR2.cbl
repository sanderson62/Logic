       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PEMLACR2.
      *AUTHOR.        PABLO.
      *               COLLEYVILE TEXAS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT REPORT-EXT           ASSIGN TO SYS011
                  ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERARCH               ASSIGN TO ERARCH2
                  ACCESS IS DYNAMIC
                  ORGANIZATION IS INDEXED
                  FILE STATUS IS ERARCH-FILE-STATUS
                  RECORD KEY IS LA-CONTROL-BY-CERT-RESP.

       DATA DIVISION.
       FILE SECTION.

       FD  REPORT-EXT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  REPORT-RECORD               PIC X(110).

       FD  ERARCH.
                                       COPY ERCARCH.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMLACR2  WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '*********VMOD=2.001*************'.
       77  ERARCH-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  WS-NOV-PRINT                PIC 9(7) VALUE ZEROS.
       77  WS-NOV-REPRINT              PIC 9(7) VALUE ZEROS.
       77  WS-DEC-PRINT                PIC 9(7) VALUE ZEROS.
       77  WS-DEC-REPRINT              PIC 9(7) VALUE ZEROS.
       77  WS-JAN-PRINT                PIC 9(7) VALUE ZEROS.
       77  WS-JAN-REPRINT              PIC 9(7) VALUE ZEROS.
       77  WS-PREV-CERT                PIC X(11) VALUE SPACES.

       01  WS-HEAD-LINE.
           05  FILLER                  PIC X(99) VALUE 
              ' CAR  ACT NO      CERT NO    NC     RE DT   PROC  CR DT  
      -       ' PRT DT   RES PRT  LETR ARCH NO  DUP  LETR'.

       01  WS-DETAIL-LINE.
           05  FILLER                  PIC XX   VALUE SPACES.
           05  D1-CARRIER              PIC XXX.
           05  D1-ACCT-NO              PIC X(11).
           05  D1-CERT-NO              PIC X(12).
           05  D1-NOC                  PIC ZZ9.
           05  FILLER                  PIC XXXX.
           05  D1-RESENT1-DATE         PIC X(9).
           05  D1-PROCESSOR-CD         PIC X(5).
           05  D1-CREATION-DT          PIC X(9).
           05  D1-INITIAL-PRINT-DATE   PIC X(9).
           05  D1-RESEND-PRINT-DATE    PIC X(9).
           05  D1-FORM-NO              PIC XXXXX.
           05  D1-ARCH-NO              PIC 9(7).
           05  D1-ARCH-CNT             PIC ZZ99.
           05  F                       PIC XX.
           05  D1-RFORM-NO             PIC X(4).

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

           OPEN  INPUT ERARCH
                OUTPUT REPORT-EXT

           IF ERARCH-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' BAD OPEN ERARCH ' ERARCH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF           

           .
       0010-EXIT.
           EXIT.

       0020-INIT.

           MOVE ZEROS                  TO QSAM-IN-CNT

           WRITE REPORT-RECORD         FROM WS-HEAD-LINE

           WRITE REPORT-RECORD         FROM WS-SPACES
           WRITE REPORT-RECORD         FROM WS-SPACES
           MOVE LOW-VALUES TO LA-CONTROL-BY-CERT-RESP
           START ERARCH KEY > LA-CONTROL-BY-CERT-RESP

           IF ERARCH-FILE-STATUS NOT = '00'
              DISPLAY ' BAD START ERARCH ' ERARCH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF           

           PERFORM 0146-READ-ERARCH    THRU 0146-EXIT

           .
       0020-EXIT.
           EXIT.
           
       0140-PROCESS.

           IF (LA-STATUS = 'A')
              AND (LA-PURGED-DATE = LOW-VALUES OR SPACES)
              AND (LA-VOIDED-DATE = LOW-VALUES OR SPACES)
              AND (LA-REPLY-DATE  = LOW-VALUES OR SPACES)
              CONTINUE
           ELSE
              GO TO 0140-CONTINUE
           END-IF

           DISPLAY ' MADE IT PAST FIRST SELECTION '

           IF (LA-INITIAL-PRINT-DATE = LOW-VALUES OR SPACES)
                         OR
              ((LA-SENT-DATE = SPACES OR LOW-VALUES)
               AND (LA-RESEND-DATE NOT = SPACES AND LOW-VALUES))
               CONTINUE
           ELSE
              GO TO 0140-CONTINUE
           END-IF

           PERFORM 0150-PRINT-REPORT
                                    THRU 0150-EXIT

           .
       0140-CONTINUE.

           PERFORM 0146-READ-ERARCH    THRU 0146-EXIT

           .
       0140-EXIT.
           EXIT.

       0146-READ-ERARCH.

           READ ERARCH NEXT RECORD

           IF (ERARCH-FILE-STATUS = '10')
              OR (LA-COMPANY-CD-A2 > X'04')
              SET END-OF-ERARCH        TO TRUE
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 DISPLAY ' BAD READ NEXT ERARCH ' ERARCH-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT END-OF-ERARCH
              ADD 1                    TO QSAM-IN-CNT
           END-IF

           .
       0146-EXIT.
           EXIT.

       0150-PRINT-REPORT.

           IF WS-PREV-CERT = LA-CERT-NO-A2
              MOVE 55                  TO D1-ARCH-CNT
           ELSE
              MOVE ZEROS               TO D1-ARCH-CNT
           END-IF
           MOVE LA-CERT-NO-A2          TO WS-PREV-CERT
           MOVE LA-CARRIER-A2          TO D1-CARRIER
           MOVE LA-ACCOUNT-A2          TO D1-ACCT-NO
           MOVE LA-CERT-NO-A2          TO D1-CERT-NO
           MOVE LA-ARCHIVE-NO          TO D1-ARCH-NO

           MOVE LA-NO-OF-COPIES        TO D1-NOC
           MOVE LA-PROCESSOR-CD        TO D1-PROCESSOR-CD
           MOVE LA-FORM-A3             TO D1-FORM-NO
           MOVE LA-RESEND-LETR         TO D1-RFORM-NO

           IF LA-RESEND-DATE NOT = LOW-VALUES
              MOVE LA-RESEND-DATE      TO DC-BIN-DATE-1
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
           
           IF LA-RESEND-DATE NOT = LOW-VALUES
              MOVE LA-RESEND-DATE        TO DC-BIN-DATE-1
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

           CLOSE ERARCH
                 REPORT-EXT

           .
       0200-EXIT.
           EXIT.
           
       8500-DATE-CONVERSION. COPY ELCDCS.                       

       ABEND-PGM SECTION.
                                COPY ELCABEND.

