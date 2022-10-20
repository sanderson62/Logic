       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCFX3.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELCNTL           ASSIGN TO ELCNTL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CF-CONTROL-PRIMARY
                                   FILE STATUS IS ELCNTL-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT ELCNTL-OUT       ASSIGN TO ELCNTLOT
               ORGANIZATION IS LINE SEQUENTIAL.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ELCNTL.

           COPY ELCCNTL.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ELCNTL-OUT
           RECORDING MODE V
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  CF-USER-REC                 PIC X(92).
       01  CF-STATE-REC                PIC X(92).
       01  CF-BENEFIT-CD-REC           PIC X(42).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDCFX3  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ELCNTL                 VALUE 'Y'.
       77  ELCNTL-RECS-IN              PIC 9(9) VALUE ZEROS.
       77  ELCNTL-RECS-OUT             PIC 9(9) VALUE ZEROS.
       77  S1                          PIC S9(5) VALUE +0 COMP-3.
       77  PGM-SUB                     PIC S9(5) COMP-3 VALUE +515.
       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       01  WS-EX2-INIT                 PIC X(92) VALUE LOW-VALUES.
       01  EX-USER-REC.
           12  EX2-REC-TYPE            PIC X.
           12  EX2-TAB1                PIC X.
           12  EX2-USER-ID             PIC X(4).
           12  EX2-TAB2                PIC X.
           12  EX2-USER-PASSWORD       PIC X(11).
           12  EX2-TAB3                PIC X.
           12  EX2-USER-NAME           PIC X(30).
           12  EX2-TAB4                PIC X.
           12  EX2-USER-TITLE          PIC X(26).
           12  EX2-TAB5                PIC X.
           12  EX2-USER-TERM-ID        PIC X(4).
           12  EX2-CNT REDEFINES EX2-USER-TERM-ID
                                       PIC 9(4).
           12  EX2-TAB6                PIC X.
           12  EX2-LAST-MAINT-DT       PIC X(10).
           12  EX2-TAB7                PIC X.
           12  EX2-EOR                 PIC X.

       01  WS-EX3-INIT                 PIC X(92) VALUE LOW-VALUES.
       01  EX-STATE-REC.
           12  EX3-REC-TYPE            PIC X.
           12  EX3-TAB1                PIC X.
           12  EX3-STATE               PIC XX.
           12  EX3-TAB2                PIC X.
           12  EX3-STATE-NAME          PIC X(25).
           12  EX3-TAB3                PIC X.
           12  EX3-SL-COMM-CAP         PIC 9.9999.
           12  EX3-TAB4                PIC X.
           12  EX3-JL-COMM-CAP         PIC 9.9999.
           12  EX3-TAB5                PIC X.
           12  EX3-SA-COMM-CAP         PIC 9.9999.
           12  EX3-TAB6                PIC X.
           12  EX3-JA-COMM-CAP         PIC 9.9999.
           12  EX3-TAB7                PIC X.
           12  EX3-LF-PREM-TAX         PIC 9.9999.
           12  EX3-TAB8                PIC X.
           12  EX3-AH-PREM-TAX-I       PIC 9.9999.
           12  EX3-TAB9                PIC X.
           12  EX3-AH-PREM-TAX-G       PIC 9.9999.
           12  EX3-TAB10               PIC X.
           12  EX3-LAST-MAINT-DT       PIC X(10).
           12  EX3-TAB11               PIC X.
           12  EX3-EOR                 PIC X.

       01  WS-EX4-INIT                 PIC X(42).
       01  EX-BENEFIT-CD-REC.
           12  EX4-REC-TYPE            PIC X.
           12  EX4-TAB1                PIC X.
           12  EX4-LF-AH               PIC X.
           12  EX4-TAB1A               PIC X.
           12  EX4-BENEFIT-CD          PIC XX.
           12  EX4-TAB2                PIC X.
           12  EX4-ALPHA               PIC XXX.
           12  EX4-TAB3                PIC X.
           12  EX4-DESC                PIC X(10).
           12  EX4-TAB4                PIC X.
           12  EX4-COMMENT             PIC X(10).
           12  EX4-TAB5                PIC X.
           12  EX4-COV-TYPE            PIC X.
           12  EX4-TAB6                PIC X.
           12  EX4-SPEC-CALC-CD        PIC X.
           12  EX4-TAB7                PIC X.
           12  EX4-JNT-IND             PIC X.
           12  EX4-TAB8                PIC X.
           12  EX4-BEN-CAT             PIC X.
           12  EX4-TAB9                PIC X.
           12  EX4-EOR                 PIC X.

      ******************************************************************
       01  WS-MISC.
           05  WS-CNTR                 PIC 9(4) VALUE ZEROS.
           05  ELCNTL-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 4000-OPEN-FILES     THRU 4000-EXIT

           PERFORM 5020-INITIALIZE     THRU 5020-EXIT

           PERFORM 0100-PROCESS-ELCNTL THRU 0100-EXIT UNTIL
              (END-OF-ELCNTL)
PEMTST*       OR (CLM-RECS-IN > 1000)

      *    MOVE WS-EX2-INIT            TO EX-USER-REC
      *    MOVE 'TOTAL COUNT '         TO EX2-USER-NAME
      *    MOVE WS-CNTR                TO EX2-CNT
      *    PERFORM 0300-WRITE-ELCNTL   THRU 0300-EXIT

           PERFORM 5000-CLOSE-FILES    THRU 5000-EXIT

           DISPLAY ' ELCNTL RECORDS READ    '  ELCNTL-RECS-IN
           DISPLAY ' ELCNTL RECORDS WRITTEN '  ELCNTL-RECS-OUT
           GOBACK

           .
       0100-PROCESS-ELCNTL.

           EVALUATE CF-RECORD-TYPE
              WHEN '2'
                 PERFORM 0400-BUILD-USER-REC
                                       THRU 0400-EXIT
              WHEN '3'
                 PERFORM 0600-BUILD-STATE-REC
                                       THRU 0600-EXIT
              WHEN '4'
                 PERFORM 0500-BUILD-BENEFIT-CD
                                       THRU 0500-EXIT
              WHEN '5'
                 PERFORM 0500-BUILD-BENEFIT-CD
                                       THRU 0500-EXIT
           END-EVALUATE
                            
           PERFORM 0200-READ-ELCNTL    THRU 0200-EXIT

           .

       0100-EXIT.
           EXIT.

       0200-READ-ELCNTL.

           READ ELCNTL NEXT RECORD

           IF ELCNTL-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCNTL        TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL READ NEXT ' ELCNTL-FILE-STATUS
                 SET END-OF-ELCNTL     TO TRUE
              ELSE
                 IF (CF-COMPANY-ID NOT = DTE-CLIENT)
                    OR (CF-RECORD-TYPE > '5')
                    SET END-OF-ELCNTL TO TRUE
                 END-IF
              END-IF
           END-IF

           IF NOT END-OF-ELCNTL
              ADD 1 TO ELCNTL-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-ELCNTL.

           WRITE CF-USER-REC           FROM EX-USER-REC
           ADD 1 TO ELCNTL-RECS-OUT

           .
       0300-EXIT.
           EXIT.


       0400-BUILD-USER-REC.

      *    IF CF-CURRENT-TERM-ON = SPACES OR LOW-VALUES
      *       CONTINUE
      *    ELSE
              ADD 1                    TO WS-CNTR
              MOVE WS-EX2-INIT         TO EX-USER-REC
              MOVE CF-PROCESSOR        TO EX2-USER-ID
              MOVE CF-PROCESSOR-PASSWORD
                                       TO EX2-USER-PASSWORD
              MOVE CF-PROCESSOR-NAME   TO EX2-USER-NAME
              MOVE CF-PROCESSOR-TITLE  TO EX2-USER-TITLE
              MOVE CF-CURRENT-TERM-ON  TO EX2-USER-TERM-ID
              MOVE CF-LAST-MAINT-DT    TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EX2-LAST-MAINT-DT
              END-IF
              INSPECT EX-USER-REC REPLACING
                 ALL ';'               BY ' '
                 ALL X'00'             BY ' '
                 ALL X'09'             BY ';'

              WRITE CF-USER-REC        FROM EX-USER-REC
              ADD 1 TO ELCNTL-RECS-OUT
      *    END-IF

           .
       0400-EXIT.
           EXIT.

       0500-BUILD-BENEFIT-CD.

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +8
              MOVE WS-EX4-INIT         TO EX-BENEFIT-CD-REC
              IF CF-BENEFIT-CODE (S1) NOT = SPACES AND ZEROS
               MOVE CF-BENEFIT-CODE (S1)
                                       TO EX4-BENEFIT-CD
               IF CF-RECORD-TYPE = '4'
                  MOVE 'L'             TO EX4-LF-AH
               ELSE
                  MOVE 'A'             TO EX4-LF-AH
               END-IF
               MOVE CF-BENEFIT-ALPHA (S1)
                                       TO EX4-ALPHA
               MOVE CF-BENEFIT-DESCRIP (S1)
                                       TO EX4-DESC
               MOVE CF-BENEFIT-COMMENT (S1)
                                       TO EX4-COMMENT
               MOVE CF-LF-COVERAGE-TYPE (S1)
                                       TO EX4-COV-TYPE
               MOVE CF-SPECIAL-CALC-CD (S1)
                                       TO EX4-SPEC-CALC-CD
               MOVE CF-JOINT-INDICATOR (S1)
                                       TO EX4-JNT-IND
               MOVE CF-BENEFIT-CATEGORY (S1)
                                       TO EX4-BEN-CAT

               INSPECT EX-BENEFIT-CD-REC REPLACING
                 ALL ';'               BY ' '
                 ALL X'00'             BY ' '
                 ALL X'09'             BY ';'

               WRITE CF-BENEFIT-CD-REC FROM EX-BENEFIT-CD-REC
               ADD 1 TO ELCNTL-RECS-OUT
              END-IF
           END-PERFORM

           .
       0500-EXIT.
           EXIT.

       0600-BUILD-STATE-REC.

           MOVE WS-EX3-INIT         TO EX-STATE-REC
           MOVE CF-STATE-CODE       TO EX3-STATE
           MOVE CF-STATE-NAME       TO EX3-STATE-NAME
           MOVE CF-ST-COMM-CAP-SL   TO EX3-SL-COMM-CAP
           MOVE CF-ST-COMM-CAP-JL   TO EX3-JL-COMM-CAP
           MOVE CF-ST-COMM-CAP-SA   TO EX3-SA-COMM-CAP
           MOVE CF-ST-COMM-CAP-JA   TO EX3-JA-COMM-CAP
           MOVE CF-ST-LF-PREM-TAX   TO EX3-LF-PREM-TAX
           MOVE CF-ST-AH-PREM-TAX-I TO EX3-AH-PREM-TAX-I
           MOVE CF-ST-AH-PREM-TAX-G TO EX3-AH-PREM-TAX-G
           MOVE CF-LAST-MAINT-DT    TO DC-BIN-DATE-1
           MOVE ' '                 TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX3-LAST-MAINT-DT
           END-IF
           INSPECT EX-STATE-REC REPLACING
              ALL ';'               BY ' '
              ALL X'00'             BY ' '
              ALL X'09'             BY ';'

           WRITE CF-STATE-REC       FROM EX-STATE-REC
           ADD 1 TO ELCNTL-RECS-OUT

           .
       0600-EXIT.
           EXIT.

       4000-OPEN-FILES.

           OPEN INPUT ELCNTL
               OUTPUT ELCNTL-OUT

           .
       4000-EXIT.
           EXIT.

       5000-CLOSE-FILES.

           CLOSE ELCNTL ELCNTL-OUT

           .
       5000-EXIT.
           EXIT.

       5010-START-ELCNTL.

           MOVE LOW-VALUES             TO CF-CONTROL-PRIMARY
           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '2'                    TO CF-RECORD-TYPE
           START ELCNTL KEY >= CF-CONTROL-PRIMARY

           IF ELCNTL-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCNTL        TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL START     ' ELCNTL-FILE-STATUS
                 SET END-OF-ELCNTL     TO TRUE
              END-IF
           END-IF

           .
       5010-EXIT.
           EXIT.


       5020-INITIALIZE.

           MOVE SPACES                 TO EX-USER-REC
           MOVE X'09'                  TO EX2-TAB1
                                          EX2-TAB2
                                          EX2-TAB3
                                          EX2-TAB4
                                          EX2-TAB5
                                          EX2-TAB6
                                          EX2-TAB7
           MOVE '2'                    TO EX2-REC-TYPE
           MOVE 'E'                    TO EX2-EOR
           MOVE EX-USER-REC            TO WS-EX2-INIT

           MOVE SPACES                 TO EX-STATE-REC
           MOVE X'09'                  TO EX3-TAB1
                                          EX3-TAB2
                                          EX3-TAB3
                                          EX3-TAB4
                                          EX3-TAB5
                                          EX3-TAB6
                                          EX3-TAB7
                                          EX3-TAB8
                                          EX3-TAB9
                                          EX3-TAB10
                                          EX3-TAB11
           MOVE '3'                    TO EX3-REC-TYPE
           MOVE 'E'                    TO EX3-EOR
           MOVE EX-STATE-REC           TO WS-EX3-INIT

           MOVE SPACES                 TO EX-BENEFIT-CD-REC
           MOVE X'09'                  TO EX4-TAB1
                                          EX4-TAB1A
                                          EX4-TAB2
                                          EX4-TAB3
                                          EX4-TAB4
                                          EX4-TAB5
                                          EX4-TAB6
                                          EX4-TAB7
                                          EX4-TAB8
                                          EX4-TAB9
           MOVE '4'                    TO EX4-REC-TYPE
           MOVE 'E'                    TO EX4-EOR
           MOVE EX-BENEFIT-CD-REC      TO WS-EX4-INIT

           PERFORM 5010-START-ELCNTL   THRU 5010-EXIT
           PERFORM 0200-READ-ELCNTL    THRU 0200-EXIT

           .
       5020-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
