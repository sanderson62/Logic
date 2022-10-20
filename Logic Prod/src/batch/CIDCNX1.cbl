       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCNX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
032612******************************************************************
032612*                   C H A N G E   L O G
032612*
032612* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
032612*-----------------------------------------------------------------
032612*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
032612* EFFECTIVE    NUMBER
032612*-----------------------------------------------------------------
032612* 032612   2011110200001   PEMA  AHL CHANGES
072619* 072619  CR2019072600001  PEMA  Comment out display statements.
041320* 041320  CR2020030500002  PEMA  Issue, cancel billing notes
032612******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERNOTE           ASSIGN TO ERNOTE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CN-CONTROL-PRIMARY
                                   FILE STATUS IS ERNOTE-FILE-STATUS.

           SELECT ERNOTE-OUT       ASSIGN TO ERNOTEOT
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DISK-DATE     ASSIGN TO SYS019-FBA1-S-SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ERNOTE.

                                       COPY ERCNOTE.

       FD  ERNOTE-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

041320 01  ERNOTE-OUT-REC              PIC X(881).

082603 FD  DISK-DATE                   COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDCNX1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  WS-RECS-IN                  PIC 9(9) VALUE ZEROS.
       77  WS-RECS-OUT                 PIC 9(9) VALUE ZEROS.
       77  WS-BAD-LINES                PIC 9(9) VALUE ZEROS.
       77  S1                          PIC S999 VALUE +0 COMP-3.
       77  S2                          PIC S999 VALUE +0 COMP-3.

       01  WS-MISC.
           05  PGM-SUB                 PIC S9(4)   VALUE +548.
           05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           05  WS-ZERO                 PIC S9      VALUE ZERO.
           05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.

041320 01  WS-SAVE-ERNOTE              PIC X(881) VALUE SPACES.

       01  ERNOTE-DETAIL-RECORD.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-GROUPING             PIC X(6).
           12  EX-TAB2                 PIC X.
           12  EX-STATE                PIC XX.
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-CERT-EFF-DT          PIC X(10).
           12  EX-TAB5                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB6                 PIC X.
041320     12  EX-NOTE-TYPE            PIC X.
           12  EX-TAB7                 PIC X.
           12  EX-BILL-START-LINE      PIC 99.
           12  EX-TAB8                 PIC X.
           12  EX-BILL-END-LINE        PIC 99.
           12  FILLER OCCURS 10.
               16  EX-TAB9             PIC X.
               16  EX-BILL-LINE        PIC X(80).
041320     12  EX-TAB10                PIC X.
041320     12  EX-LAST-MAINT-DT        PIC X(10).
041320     12  EX-TAB11                PIC X.
041320     12  EX-LAST-MAINT-BY        PIC X(4).
041320     12  EX-TAB12                PIC X.
           12  EX-EOR                  PIC X.

      ******************************************************************
       01  WS-MISC.
           05  WS-WORK-SEQ             PIC X.
           05  WS-NUM-SEQ REDEFINES WS-WORK-SEQ PIC 9.
           05  ERNOTE-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-INPUT  THRU 0100-EXIT UNTIL
                 (END-OF-INPUT)
PEMTST*          OR (WS-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' RECORDS READ    '  WS-RECS-IN
           DISPLAY ' RECORDS WRITTEN '  WS-RECS-OUT
           DISPLAY ' BILL LINES WITH GARBAGE ' WS-BAD-LINES
           GOBACK

           .
       0100-PROCESS-INPUT.

           PERFORM 0105-BUILD-EXTRACT  THRU 0105-EXIT

           PERFORM 0200-READ-ERNOTE    THRU 0200-EXIT

           .
       0100-EXIT.
           EXIT.

       0105-BUILD-EXTRACT.

           MOVE WS-SAVE-ERNOTE         TO ERNOTE-DETAIL-RECORD
           MOVE CN-CARRIER             TO EX-CARRIER
           MOVE CN-GROUPING            TO EX-GROUPING
           MOVE CN-STATE               TO EX-STATE
           MOVE CN-ACCOUNT             TO EX-ACCOUNT

           MOVE CN-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-CERT-EFF-DT
           ELSE
              DISPLAY ' ERROR - EFF DT CONVERT ' CN-CARRIER ' '
                 CN-GROUPING ' ' CN-STATE ' ' CN-ACCOUNT ' '
041320           CN-CERT-NO '*** BYPASSING ***'
041320        GO TO 0105-EXIT
           END-IF

           IF CN-CERT-SFX = X'00'
              MOVE 'U'                 TO CN-CERT-SFX
072619*       DISPLAY ' FIXING CERT SUFFIX ' CN-CARRIER ' '
072619*          CN-GROUPING ' ' CN-STATE ' ' CN-ACCOUNT ' '
072619*          EX-CERT-EFF-DT ' ' CN-CERT-NO
           END-IF
           MOVE CN-CERT-NO             TO EX-CERT-NO
041320     MOVE CN-RECORD-TYPE         TO EX-note-type
           MOVE CN-BILLING-START-LINE-NO
                                       TO EX-BILL-START-LINE
           MOVE CN-BILLING-END-LINE-NO TO EX-BILL-END-LINE

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +2
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                 S2 > +75
                 IF CN-LINE (S1) (S2:3) = X'434E04' OR X'434E9C'
072619*             DISPLAY ' REMOVING BILL LINE ' CN-CARRIER ' '
072619*                CN-GROUPING ' ' CN-STATE ' ' CN-ACCOUNT ' '
072619*                EX-CERT-EFF-DT ' ' CN-CERT-NO ' ' CN-LINE (S1)
                    MOVE SPACES        TO CN-LINE (S1)
                    MOVE +76           TO S2
                    ADD 1              TO WS-BAD-LINES
                 END-IF
              END-PERFORM
           END-PERFORM

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +10
              MOVE CN-LINE (S1)        TO EX-BILL-LINE (S1)
           END-PERFORM

041320     MOVE CN-LAST-MAINT-DT       TO DC-BIN-DATE-1
041320     MOVE ' '                    TO DC-OPTION-CODE
041320     PERFORM 8510-DATE-CONVERSION
041320                                 THRU 8590-EXIT
041320     IF NO-CONVERSION-ERROR
041320        MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-MAINT-DT
041320     ELSE
041320        MOVE EX-CERT-EFF-DT      TO EX-LAST-MAINT-DT
041320     END-IF
041320     MOVE CN-LAST-MAINT-USER     TO EX-LAST-MAINT-BY

           PERFORM 0300-WRITE-OUTPUT   THRU 0300-EXIT

           .
       0105-EXIT.
           EXIT.

       0200-READ-ERNOTE.

           READ ERNOTE NEXT RECORD

           IF ERNOTE-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERNOTE-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERNOTE READ ' ERNOTE-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              ELSE
                 IF CN-COMPANY-CD > DTE-CLASIC-COMPANY-CD
                    SET END-OF-INPUT  TO TRUE
                 END-IF
              END-IF
           END-IF

           IF NOT END-OF-INPUT
              ADD 1 TO WS-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-OUTPUT.

           INSPECT ERNOTE-DETAIL-RECORD REPLACING ALL ';' BY ' '
              ALL X'00' BY ' '
              ALL X'09' BY ';'
           WRITE ERNOTE-OUT-REC        FROM ERNOTE-DETAIL-RECORD
           ADD 1 TO WS-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERNOTE
               OUTPUT ERNOTE-OUT

           IF ERNOTE-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERNOTE - OPEN  ' ERNOTE-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERNOTE ERNOTE-OUT

           .
       0500-EXIT.
           EXIT.

       0550-START-ERNOTE.

           MOVE DTE-CLASIC-COMPANY-CD  TO CN-CONTROL-PRIMARY

           START ERNOTE KEY > CN-CONTROL-PRIMARY

           IF ERNOTE-FILE-STATUS = '10' OR '23'
              SET END-OF-INPUT         TO TRUE
           ELSE
              IF ERNOTE-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR - ERNOTE - START  ' ERNOTE-FILE-STATUS
                 SET END-OF-INPUT      TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO ERNOTE-DETAIL-RECORD

           MOVE X'09'                  TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
041320                                    ex-tab10
041320                                    ex-tab11
041320                                    ex-tab12

           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
              S1 > +10
041320        MOVE X'09'               TO EX-TAB9 (S1)
           END-PERFORM

           MOVE 'E'                    TO EX-EOR

           MOVE ERNOTE-DETAIL-RECORD   TO WS-SAVE-ERNOTE
           PERFORM 0550-START-ERNOTE   THRU 0550-EXIT
032612     if not end-of-input
              PERFORM 0200-READ-ERNOTE THRU 0200-EXIT
032612     end-if

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM SECTION.              COPY ELCABEND.
