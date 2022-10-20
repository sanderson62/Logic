       IDENTIFICATION DIVISION.
       PROGRAM-ID. EL324P.
      *AUTHOR.     PABLO.
      *DATE-COMPILED.
      *REMARKS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DISK-DATE        ASSIGN TO SYS019.
           SELECT PRNTR            ASSIGN TO SYS008.
           SELECT FICH             ASSIGN TO SYS020.
           SELECT ELREPT           ASSIGN TO ELREPT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS RF-CONTROL-PRIMARY
                                   FILE STATUS IS DTE-VSAM-FLAGS.

           SELECT ELMSTR           ASSIGN TO ELMSTR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CL-CONTROL-PRIMARY
                                   FILE STATUS IS ELMSTR-FILE-STATUS.

           SELECT ELTRLR           ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AT-CONTROL-PRIMARY
                                   FILE STATUS IS ELTRLR-FILE-STATUS.

           EJECT
       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE               COPY ELCDTEFD.

       FD  PRNTR                   COPY ELCPRTFD.

       FD  FICH                    COPY ELCFCHFD.

       FD  ELREPT.
                                   COPY ELCREPT.

           EJECT
       FD  ELMSTR.
                                   COPY ELCMSTR.

           EJECT
       FD  ELTRLR.
                                   COPY ELCTRLR.

           EJECT
       WORKING-STORAGE SECTION.
       01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.

       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*     EL324P WORKING STORAGE   *'.
       77  FILLER  PIC X(32)   VALUE '********* VMOD=2.001 ***********'.

       01  FILLER                      COMP-3.
           05  WS-LINE-COUNT           PIC S9(3)       VALUE +99.
           05  WS-LINE-COUNT-MAX       PIC S9(3)       VALUE +59.
           05  WS-PAGE                 PIC S9(5)       VALUE ZERO.
           05  WS-REPORT-SW            PIC S9          VALUE +1.
           05  WS-PRINT-SW             PIC S9          VALUE ZERO.
           05  WS-RECORD-COUNT         PIC S9(9)       VALUE ZERO.
           05  WS-RETURN-CODE          PIC S9(3)       VALUE ZERO.
           05  WS-ZERO                 PIC S9          VALUE ZERO.

           EJECT
       01  FILLER                      COMP SYNC.
           05  PGM-SUB                 PIC S9(4)       VALUE +324.
           05  WS-INDEX                PIC S9(4)       VALUE ZERO.

       01  FILLER.
           05  WS-RESEND-IND           PIC XX VALUE SPACES.
           05  WS-PREV-CYCLE-DT        PIC XX VALUE LOW-VALUES.
           05  WS-CURR-CYCLE-DT        PIC XX VALUE LOW-VALUES.
           05  WS-PROC-DT              PIC XX VALUE LOW-VALUES.
           05  WS-EOF-SW               PIC X VALUE SPACES.
               88  END-OF-ELTRLR             VALUE 'Y'.
           05  WS-WANT-CLAIM           PIC X VALUE SPACES.
               88  WANT-CLAIM                    VALUE 'Y'.
           05  ABEND-CODE              PIC X(4).
           05  ABEND-OPTION            PIC X.
           05  OLC-REPORT-NAME         PIC X(5) VALUE 'EL324'.
           05  X                       PIC X           VALUE SPACE.

           05  WS-SAVE-PRINT-RECORD    PIC X(133)      VALUE SPACES.

           05  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.

           05  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.
           05  ELTRLR-FILE-STATUS      PIC XX          VALUE ZERO.
           05  ELMSTR-FILE-STATUS      PIC XX          VALUE ZERO.

           05  WS-FILE-ERROR-MESSAGE.
               10  FILLER              PIC X(24)       VALUE
                   'ERROR OCCURED OPENING - '.
               10  WS-FEM-FILE-NAME    PIC X(8).

           EJECT
       01  WS-HEADING1.
           05  FILLER                  PIC X(49)       VALUE '1'.
           05  WS-H1-TITLE             PIC X(71)       VALUE
               'CORRESPONDENCE REGISTER'.
           05  WS-H1-REPORT-NUMBER     PIC X(9) VALUE 'EL324'.

       01  WS-HEADING2.
           05  FILLER                  PIC X(45)       VALUE SPACES.
           05  WS-H2-CLIENT-NAME       PIC X(75)       VALUE SPACES.
           05  WS-H2-DATE              PIC X(8)        VALUE SPACES.
           05  FILLER                  PIC X           VALUE SPACES.

       01  WS-HEADING3.
           05  FILLER                  PIC X(51)       VALUE SPACES.
           05  WS-H3-DATE              PIC X(69)       VALUE SPACES.
           05  FILLER                  PIC X(5)        VALUE 'PAGE'.
           05  WS-H3-PAGE              PIC ZZ,ZZ9.
           05  FILLER                  PIC X(11)       VALUE SPACES.

       01  WS-HEADING4.
           05  FILLER                  PIC X(47)       VALUE
               '-    CLAIM      CERT     INSURED'.
           05  FILLER                  PIC X(86)       VALUE
               'ARCHIVE    SEND    INITIAL  ANSWER   RE-SEND  RE-SEND  A
      -        'DDRESSEE NAME/'.

       01  WS-HEADING5.
           05  FILLER                  PIC X(41)       VALUE
               ' CAR NUMBER    NUMBER    LAST NAME'.
           05  FILLER                  PIC X(92)       VALUE
               'FORM   NUMBER    DATE     PRINT  RECEIVED   DATE    PRIN
      -        'TED            TYPE'.

           EJECT
       01  WS-DETAIL1.
           05  FILLER                  PIC X.
           05  RESEND-IND              PIC XX.
           05  WS-D1-CARRIER           PIC X.
           05  FILLER                  PIC X.
           05  WS-D1-CLAIM-NO          PIC X(7).
           05  FILLER                  PIC X.
           05  WS-D1-CERT-NO           PIC X(11).
           05  FILLER                  PIC X.
           05  WS-D1-INSURED-LAST-NAME PIC X(15).
           05  FILLER                  PIC XX.
           05  WS-D1-FORM              PIC X(4).
           05  FILLER                  PIC X.
           05  WS-D1-ARCHIVE-NUMBER    PIC Z(7)9.

           05  WS-D1-ARCHIVE-NUMBER-X  REDEFINES
               WS-D1-ARCHIVE-NUMBER    PIC X(8).
           05  FILLER                  PIC XX.
           05  WS-D1-SEND-DATE         PIC X(8).
           05  FILLER                  PIC X.
           05  WS-D1-INITIAL-PRINT     PIC X(8).
           05  FILLER                  PIC X.
           05  WS-D1-ANSWER-RECEIVED   PIC X(8).
           05  FILLER                  PIC X.
           05  WS-D1-RESEND-DATE       PIC X(8).
           05  FILLER                  PIC X.
           05  WS-D1-RESEND-PRINTED    PIC X(8).
           05  FILLER                  PIC X.
           05  WS-D1-ADDRESSEE-NAME    PIC X(30).
           05  FILLER                  PIC X.

           EJECT
       01  WS-DETAIL2                      REDEFINES
           WS-DETAIL1.
           05  FILLER                  PIC X(24).
           05  WS-D2-DESCRIPTION       PIC X(7).
           05  WS-D2-REASON            PIC X(70).
           05  FILLER                  PIC X.
           05  WS-D2-ADDRESSEE-TYPE    PIC X(11).
           05  FILLER                  PIC X(10).
           05  WS-D2-BY-DESC           PIC X(5).
           05  WS-D2-BY                PIC X(4).
           05  FILLER                  PIC X.

           EJECT
                                       COPY ELCDTECX.
           EJECT
                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

           EJECT
       LINKAGE SECTION.
       01  PARM.
           05  PARM-LEN      PIC S9(04) COMP.
           05  PARM-VALUE    PIC X(100).

       PROCEDURE DIVISION USING PARM.

       0000-DATE-CARD-READ. COPY ELCDTERX.

           PERFORM OPEN-FILES

           PERFORM 1010-INIT           THRU 1010-EXIT
           PERFORM 1040-PROCESS        THRU 1040-EXIT UNTIL
              END-OF-ELTRLR

           IF WS-RECORD-COUNT  =  ZERO
              PERFORM WRITE-HEADINGS
              MOVE '**** NO TRANSACTION FOR THIS REPORT ****'
                                       TO P-DATA
              MOVE '-'                 TO X
              PERFORM WRITE-A-LINE
           END-IF

           PERFORM CLOSE-FILES

           GOBACK

           .
       1010-INIT.

           IF PARM-LEN = 0
              DISPLAY ' INVALID PARM '
              PERFORM ABEND-PGM
           END-IF

           MOVE PARM-VALUE (1:6)       TO DC-GREG-DATE-1-MDY
           MOVE '4'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-PREV-CYCLE-DT
           ELSE
              DISPLAY ' INVALID PREV DATE ' PARM-VALUE (1:6)
              PERFORM ABEND-PGM
           END-IF

           MOVE PARM-VALUE (7:6)       TO DC-GREG-DATE-1-MDY
           MOVE '4'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-CURR-CYCLE-DT
           ELSE
              DISPLAY ' INVALID CURR DATE ' PARM-VALUE (7:6)
              PERFORM ABEND-PGM
           END-IF

           IF DC-DAY-OF-WEEK > +5
              MOVE +3                  TO DC-ELAPSED-DAYS
              MOVE ZERO                TO DC-ELAPSED-MONTHS
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION
              MOVE DC-BIN-DATE-2       TO WS-PROC-DT
           ELSE
              MOVE +1                  TO DC-ELAPSED-DAYS
              MOVE ZERO                TO DC-ELAPSED-MONTHS
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION
              MOVE DC-BIN-DATE-2       TO WS-PROC-DT
           END-IF

           IF AT-LETTER-SENT-DT NOT = LOW-VALUES
              MOVE AT-LETTER-SENT-DT   TO DC-BIN-DATE-1
              MOVE SPACES              TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION
              MOVE DC-GREG-DATE-1-EDIT TO WS-D1-SEND-DATE
           END-IF

           MOVE LOW-VALUES TO AT-CONTROL-PRIMARY

           MOVE DTE-CLASIC-COMPANY-CD TO AT-COMPANY-CD
           START ELTRLR KEY IS NOT < AT-CONTROL-PRIMARY
           IF ELTRLR-FILE-STATUS = '00'
              CONTINUE
           ELSE
              IF ELTRLR-FILE-STATUS = '23'
                 DISPLAY ' NO RECORDS FOR COMPANY '
                 SET END-OF-ELTRLR TO TRUE
              ELSE
                 DISPLAY ' ELTRLR START ' ELTRLR-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF
           IF NOT END-OF-ELTRLR
              PERFORM 1020-READ-ELTRLR    THRU 1020-EXIT
           END-IF

           .
       1010-EXIT.
           EXIT.
       1020-READ-ELTRLR.

           READ ELTRLR NEXT RECORD

           IF ELTRLR-FILE-STATUS = '00'
              CONTINUE
           ELSE
              IF ELTRLR-FILE-STATUS = '10' OR '23'
                 SET END-OF-ELTRLR TO TRUE
              ELSE
                 DISPLAY ' ELTRLR READ  ' ELTRLR-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF AT-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD
              SET END-OF-ELTRLR TO TRUE
           END-IF

           .
       1020-EXIT.
           EXIT.
       1030-READ-ELMSTR.

           IF AT-CONTROL-PRIMARY (1:20) = CL-CONTROL-PRIMARY
              CONTINUE
           ELSE
              MOVE AT-CONTROL-PRIMARY  TO CL-CONTROL-PRIMARY
              READ ELMSTR
              IF ELMSTR-FILE-STATUS = '00'
                 CONTINUE
              ELSE
                 DISPLAY ' ELMSTR READ  ' ELMSTR-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF


           .
       1030-EXIT.
           EXIT.
           EJECT

       1040-PROCESS.

           PERFORM 3100-CHECK-ELTRLR   THRU 3100-EXIT

           IF WANT-CLAIM
              PERFORM 1030-READ-ELMSTR THRU 1030-EXIT
              PERFORM 3200-PRINT-REPORT
                                       THRU 3200-EXIT
           END-IF

           PERFORM 1020-READ-ELTRLR    THRU 1020-EXIT
           .
       1040-EXIT.
           EXIT.

       3100-CHECK-ELTRLR.

           MOVE SPACES                 TO WS-WANT-CLAIM
                                          WS-RESEND-IND

           IF     (AT-TRAILER-TYPE = '4')
              AND (AT-LETTER-ORIGIN = '1' or '2')
              AND (AT-LETTER-ARCHIVE-NO NOT = 0)

              IF     ((AT-AUTO-RE-SEND-DT > WS-PREV-CYCLE-DT)
                 AND (AT-AUTO-RE-SEND-DT NOT > WS-CURR-CYCLE-DT))
                              AND
                    ((AT-RESEND-PRINT-DATE = LOW-VALUES)
                 AND (AT-LETTER-ANSWERED-DT = LOW-VALUES))
                 MOVE '*' TO WS-RESEND-IND
                 SET WANT-CLAIM TO TRUE
              END-IF

              IF     ((AT-RESEND-PRINT-DATE > WS-PREV-CYCLE-DT)
                 AND (AT-RESEND-PRINT-DATE NOT > WS-CURR-CYCLE-DT))
                              AND
                     (AT-LETTER-ANSWERED-DT = LOW-VALUES)
                 MOVE '*' TO WS-RESEND-IND
                 SET WANT-CLAIM TO TRUE
              END-IF

              IF     ((AT-LETTER-SENT-DT > WS-PREV-CYCLE-DT)
                 AND (AT-LETTER-SENT-DT NOT > WS-CURR-CYCLE-DT))
                                OR
                     ((AT-INITIAL-PRINT-DATE > WS-CURR-CYCLE-DT)
                 AND (AT-INITIAL-PRINT-DATE NOT > WS-PROC-DT))
                 SET WANT-CLAIM TO TRUE
              END-IF
           END-IF

           if want-claim and at-reason-text (1:8) = 'AUTO PAY'
              move '**'     to ws-resend-ind
           end-if

           .
       3100-EXIT.
           EXIT.

       3200-PRINT-REPORT.

           MOVE '0'                    TO WS-DETAIL1.
           MOVE WS-RESEND-IND          TO RESEND-IND
           MOVE AT-CARRIER             TO WS-D1-CARRIER.
           MOVE AT-CLAIM-NO            TO WS-D1-CLAIM-NO.
           MOVE AT-CERT-NO             TO WS-D1-CERT-NO.
           MOVE CL-INSURED-LAST-NAME   TO WS-D1-INSURED-LAST-NAME.

           IF AT-LETTER-ARCHIVE-NO > ZERO
              MOVE AT-STD-LETTER-FORM  TO WS-D1-FORM
              MOVE AT-LETTER-ARCHIVE-NO
                                       TO WS-D1-ARCHIVE-NUMBER
           ELSE
              MOVE 'FORM'              TO WS-D1-ARCHIVE-NUMBER-X
              IF AT-STD-LETTER-FORM = '1'
                 MOVE 'INIT'           TO WS-D1-FORM
              ELSE
                 IF AT-STD-LETTER-FORM = '2'
                    MOVE 'INIT'        TO WS-D1-FORM
                 ELSE
                    MOVE AT-STD-LETTER-FORM
                                       TO WS-D1-FORM
                 END-IF
              END-IF
           END-IF

           IF AT-LETTER-SENT-DT NOT = LOW-VALUES
              MOVE AT-LETTER-SENT-DT   TO DC-BIN-DATE-1
              MOVE SPACES              TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION
              MOVE DC-GREG-DATE-1-EDIT TO WS-D1-SEND-DATE
           END-IF

           IF AT-INITIAL-PRINT-DATE NOT = LOW-VALUES
              MOVE AT-INITIAL-PRINT-DATE
                                       TO DC-BIN-DATE-1
              MOVE SPACES              TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION
              MOVE DC-GREG-DATE-1-EDIT TO WS-D1-INITIAL-PRINT
           END-IF

           IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES
              MOVE AT-LETTER-ANSWERED-DT
                                       TO DC-BIN-DATE-1
              MOVE SPACES              TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION
              MOVE DC-GREG-DATE-1-EDIT TO WS-D1-ANSWER-RECEIVED
           END-IF

           IF AT-AUTO-RE-SEND-DT NOT = LOW-VALUES
              MOVE AT-AUTO-RE-SEND-DT  TO DC-BIN-DATE-1
              MOVE SPACES              TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION
              MOVE DC-GREG-DATE-1-EDIT TO WS-D1-RESEND-DATE
           END-IF

           IF AT-RESEND-PRINT-DATE NOT = LOW-VALUES
              MOVE AT-RESEND-PRINT-DATE
                                       TO DC-BIN-DATE-1
              MOVE SPACES              TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION
              MOVE DC-GREG-DATE-1-EDIT TO WS-D1-RESEND-PRINTED
           END-IF

           MOVE AT-ADDRESSEE-NAME      TO WS-D1-ADDRESSEE-NAME
           MOVE WS-DETAIL1             TO PRT
           PERFORM WRITE-A-LINE
           MOVE SPACES                 TO WS-DETAIL2
           MOVE 'REASON-'              TO WS-D2-DESCRIPTION
           MOVE AT-REASON-TEXT         TO WS-D2-REASON

           IF AT-ADDRESEE-TYPE = 'I'
              MOVE 'INSURED'           TO WS-D2-ADDRESSEE-TYPE
           ELSE
              IF AT-ADDRESEE-TYPE = 'B'
                 MOVE 'BENEFICIARY'    TO WS-D2-ADDRESSEE-TYPE
              ELSE
                 IF AT-ADDRESEE-TYPE = 'A'
                    MOVE 'ACCOUNT'     TO WS-D2-ADDRESSEE-TYPE
                 ELSE
                    IF AT-ADDRESEE-TYPE = 'P'
                       MOVE 'PHYSICIAN'
                                       TO WS-D2-ADDRESSEE-TYPE
                    ELSE
                       IF AT-ADDRESEE-TYPE = 'E'
                          MOVE 'EMPLOYER'
                                       TO WS-D2-ADDRESSEE-TYPE
                       ELSE
                          IF AT-ADDRESEE-TYPE = 'O'
                             MOVE 'OTHER 1'
                                       TO WS-D2-ADDRESSEE-TYPE
                          ELSE
                             IF AT-ADDRESEE-TYPE = 'Q'
                                MOVE 'OTHER 2'
                                       TO WS-D2-ADDRESSEE-TYPE
                             ELSE
                                MOVE AT-ADDRESEE-TYPE
                                       TO WS-D2-ADDRESSEE-TYPE
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF

           ADD +1                      TO WS-RECORD-COUNT
           MOVE 'BY - '                TO WS-D2-BY-DESC
           MOVE AT-RECORDED-BY         TO WS-D2-BY
           MOVE WS-DETAIL2             TO PRT
           PERFORM WRITE-A-LINE
           .
       3200-EXIT.
           EXIT.

           EJECT
       8500-DATE-CONVERSION SECTION.   COPY ELCDCS.

           EJECT
       WRITE-A-LINE SECTION.           COPY ELCWAL.

       WRITE-HEADINGS SECTION.

       WHS-010.
           IF WS-H2-DATE = SPACES
              MOVE WS-CURRENT-DATE     TO WS-H2-DATE
              MOVE COMPANY-NAME        TO WS-H2-CLIENT-NAME
              MOVE ALPH-DATE           TO WS-H3-DATE
           END-IF

           ADD +1                      TO WS-PAGE
           MOVE WS-PAGE                TO WS-H3-PAGE
           MOVE PRT                    TO WS-SAVE-PRINT-RECORD
           MOVE ZERO                   TO WS-LINE-COUNT

           MOVE WS-HEADING1            TO PRT
           MOVE '1'                    TO X
           PERFORM WRITE-PRINTER

           MOVE WS-HEADING2            TO PRT
           MOVE ' '                    TO X
           PERFORM WRITE-PRINTER

           MOVE WS-HEADING3            TO PRT
           MOVE ' '                    TO X
           PERFORM WRITE-PRINTER

           MOVE WS-HEADING4            TO PRT
           MOVE ' '                    TO X
           PERFORM WRITE-PRINTER


           MOVE WS-HEADING5            TO PRT

           PERFORM WRITE-PRINTER

           MOVE +7                     TO WS-LINE-COUNT
           .
       WHS-020. COPY ELCWHS2.

       WRITE-PRINTER SECTION.          COPY ELCWPS.


           IF DTE-FICH NOT = SPACE AND
               FICH-OPEN   = SPACE
               MOVE 'X'                TO FICH-OPEN
               OPEN OUTPUT FICH
           END-IF

           IF DTE-PRT-OPT = 'S' OR 'T'
              IF (REPT-OPEN = SPACE) AND (DTE-ABEND-CD-1 = SPACE)
                 OPEN I-O ELREPT
                 IF DTE-F-1 NOT = ZERO AND
                    DTE-VSAM-FLAGS NOT = '97'
                    MOVE DTE-VSAM-FLAGS
                                       TO WS-ABEND-FILE-STATUS
                    MOVE 'ERROR OCCURED OPEN - ELREPT'
                                       TO WS-ABEND-MESSAGE
                    PERFORM ABEND-PGM
                 ELSE
                    MOVE '1'           TO REPT-OPEN
                    MOVE DTE-CLASIC-COMPANY-CD
                                       TO RF-COMPANY-CD
                    MOVE '1'           TO RF-RECORD-TYPE
                    MOVE OLC-REPORT-NAME
                                       TO RF-REPORT-ID
                    MOVE ZERO          TO RF-LINE-NUMBER
                    START ELREPT  KEY NOT < RF-CONTROL-PRIMARY
                    PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT
                    MOVE DTE-CLASIC-COMPANY-CD
                                       TO RF-COMPANY-CD
                    MOVE '2'           TO RF-RECORD-TYPE
                    MOVE OLC-REPORT-NAME
                                       TO RF-REPORT-ID
                    MOVE ZERO          TO RF-LINE-NUMBER
                    START ELREPT  KEY NOT < RF-CONTROL-PRIMARY
                    PERFORM DTE-REPORT-DELETE THRU DTE-DELETE-EXIT
                    MOVE DTE-CLASIC-COMPANY-CD
                                       TO RF-COMPANY-CD
                    MOVE '1'           TO RF-RECORD-TYPE
                    MOVE OLC-REPORT-NAME
                                       TO RF-REPORT-ID
                    MOVE SPACES        TO RF-REPORT-LINE-133
                 END-IF
              END-IF
           END-IF

           IF DTE-ABEND-CD-1 = '81' AND
              DTE-PRT-OPT    = 'S'
              MOVE +0302               TO WS-RETURN-CODE
              PERFORM ABEND-PGM
           END-IF

           IF DTE-PRT-OPT = 'S' OR 'T'
              MOVE X                   TO RF-CTL-CHAR-133
              MOVE P-DATA              TO RF-DATA-133
              IF DTE-ABEND-CD-1 = SPACES
                 ADD +1                TO DTE-TOT-LINES
                 MOVE DTE-TOT-LINES    TO RF-LINE-NUMBER
                 WRITE REPORT-SAVE-FILE
                           INVALID KEY
                    MOVE '88'          TO DTE-ABEND-CD-1
                    CLOSE ELREPT
                    MOVE SPACE         TO REPT-OPEN
              END-IF
           END-IF

           IF DTE-FICH NOT = SPACE
              MOVE X                   TO P-CTL
              WRITE FICH-REC FROM PRT
           END-IF

           IF DTE-PRT-OPT = 'P' OR 'B' OR 'T'
              MOVE X                   TO P-CTL
              WRITE PRT
           END-IF

           GO TO DTE-PRINT-EXIT
           .
       DTE-REPORT-DELETE.
           IF DTE-F-1 NOT = ZERO
              MOVE ZERO                TO DTE-VSAM-FLAGS
              GO TO DTE-DELETE-EXIT
           END-IF

           READ ELREPT NEXT RECORD AT END
              GO TO DTE-DELETE-EXIT
           END-READ

           IF DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD  AND
              OLC-REPORT-NAME       = RF-REPORT-ID
              DELETE ELREPT RECORD
              GO TO DTE-REPORT-DELETE
           END-IF

          .
       DTE-DELETE-EXIT.
           EXIT.

       DTE-PRINT-EXIT.
           EXIT.
      ******************************************************************

           EJECT
       OPEN-FILES SECTION.

       OFS-010.
           OPEN INPUT ELMSTR ELTRLR
                OUTPUT PRNTR
           .
       OFS-EXIT.
           EXIT.

           EJECT
       CLOSE-FILES SECTION.

       CFS-010. COPY ELCPRTCX.
           CLOSE ELMSTR ELTRLR
                 PRNTR
           .
       CFS-EXIT.
           EXIT.


       ABEND-PGM SECTION. COPY ELCABEND.

