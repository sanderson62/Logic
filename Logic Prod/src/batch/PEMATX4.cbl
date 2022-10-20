       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMATX4.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELTRLR           ASSIGN TO ELTRLR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AT-CONTROL-PRIMARY
                                   FILE STATUS IS ELTRLR-FILE-STATUS.

           SELECT ELTRLR-OUT       ASSIGN TO ELTRLROT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ELTRLR.

           COPY ELCTRLR.

       FD  ELTRLR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  ELTRLR-OUT-REC              PIC X(212).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMATX4  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ELTRLR             VALUE 'Y'.
       77  TRL-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  TRL-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.

       01  WS-SAVE-ELTRLR              PIC X(212) VALUE SPACES.

       01  ELTRLR-DETAIL-RECORD.
           12  EX-CLAIM-TYPE           PIC X.
           12  EX-TABA                 PIC X.
           12  EX-CHECK-NO             PIC X(7).
           12  EX-TAB1                 PIC X.
           12  EX-CLAIM-NO             PIC X(7).
           12  EX-TAB2                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB3                 PIC X.
           12  EX-PAID-THRU-DT         PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-PMT-SELECT-DT        PIC X(10).
           12  EX-TAB5                 PIC X.
           12  EX-PMT-ACCEPT-DT        PIC X(10).
           12  EX-TAB6                 PIC X.
           12  EX-PAYEE-NAME           PIC X(30).
           12  EX-TAB7                 PIC X.
           12  EX-VOID-DT              PIC X(10).
           12  EX-TAB8                 PIC X.
           12  EX-PAYEE-ADDR2          PIC X(30).
           12  EX-TAB9                 PIC X.
           12  EX-PAYEE-CITY-ST        PIC X(30).
           12  EX-TAB10                PIC X.
           12  EX-PAYEE-ZIP            PIC X(9).
           12  EX-TAB11                PIC X.
           12  EX-CHECK-AMT            PIC -9(7).99.
           12  EX-TAB12                PIC X.
           12  EX-CHECK-DT             PIC X(10).
           12  EX-TAB13                PIC X.
           12  EX-SSN                  PIC X(9).
           12  EX-TAB14                PIC X.
           12  EX-EOR                  PIC X.

      ******************************************************************
       01  WS-MISC.
           05  WS-WORK-SEQ             PIC X.
           05  WS-NUM-SEQ REDEFINES WS-WORK-SEQ PIC 9.
           05  ELTRLR-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.

           EJECT
       PROCEDURE DIVISION.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0100-PROCESS-ELTRLR THRU 0100-EXIT UNTIL
                 (END-OF-ELTRLR)
PEMTST*          OR (TRL-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' TRLR  RECORDS READ    '  TRL-RECS-IN
           DISPLAY ' TRLR  RECORDS WRITTEN '  TRL-RECS-OUT
           GOBACK
           .

       0100-PROCESS-ELTRLR.

           IF (AT-TRAILER-TYPE = '2')
              AND (AT-CLAIM-TYPE = 'L')
              AND (AT-PAYEE-TYPE = 'B')
              AND (AT-CHECK-WRITTEN-DT > X'A37F')
              AND (AT-CHECK-WRITTEN-DT <= X'A4FF')
              PERFORM 0105-BUILD-EXTRACT THRU 0105-EXIT
           END-IF

           PERFORM 0200-READ-ELTRLR    THRU 0200-EXIT
           .
       0100-EXIT.
           EXIT.

       0105-BUILD-EXTRACT.

           MOVE WS-SAVE-ELTRLR         TO ELTRLR-DETAIL-RECORD
           MOVE AT-CLAIM-TYPE          TO EX-CLAIM-TYPE
           MOVE AT-CHECK-NO            TO EX-CHECK-NO
           MOVE AT-CLAIM-NO            TO EX-CLAIM-NO
           MOVE AT-CERT-NO             TO EX-CERT-NO
           MOVE AT-PAYEES-NAME         TO EX-PAYEE-NAME
           MOVE AT-AMOUNT-PAID         TO EX-CHECK-AMT

           MOVE AT-CHECK-WRITTEN-DT    TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-CHECK-DT
           END-IF

           MOVE AT-PAID-THRU-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-PAID-THRU-DT
           END-IF

           MOVE AT-PMT-SELECT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-PMT-SELECT-DT
           END-IF

           MOVE AT-PMT-ACCEPT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-PMT-ACCEPT-DT
           END-IF

           PERFORM 0300-WRITE-TRLR     THRU 0300-EXIT

           .
       0105-EXIT.
           EXIT.

       0200-READ-ELTRLR.

           READ ELTRLR NEXT RECORD

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              SET END-OF-ELTRLR        TO TRUE
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELTRLR READ NEXT ' ELTRLR-FILE-STATUS
                 SET END-OF-ELTRLR     TO TRUE
              ELSE
                 IF AT-COMPANY-CD > X'04'
                    SET END-OF-ELTRLR TO TRUE
                 END-IF
              END-IF
           END-IF

           IF NOT END-OF-ELTRLR
              ADD 1 TO TRL-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-TRLR.

           WRITE ELTRLR-OUT-REC        FROM ELTRLR-DETAIL-RECORD
           ADD 1 TO TRL-RECS-OUT

           .
       0300-EXIT.
           EXIT.


       0400-OPEN-FILES.

           OPEN INPUT ELTRLR
               OUTPUT ELTRLR-OUT

           IF ELTRLR-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY 'ELTRLR OPEN ERR  ' ELTRLR-FILE-STATUS
           END-IF

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELTRLR ELTRLR-OUT

           .

       0500-EXIT.
           EXIT.

       0550-START-ELTRLR.

           MOVE LOW-VALUES             TO AT-CONTROL-PRIMARY
           MOVE X'04'                  TO AT-COMPANY-CD

           START ELTRLR KEY >= AT-CONTROL-PRIMARY

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              SET END-OF-ELTRLR        TO TRUE
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELTRLR START     ' ELTRLR-FILE-STATUS
                 SET END-OF-ELTRLR     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO ELTRLR-DETAIL-RECORD

           MOVE ';'                    TO EX-TAB1
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB8
                                          EX-TAB9
                                          EX-TAB10
                                          EX-TAB11
                                          EX-TAB12
                                          EX-TAB13
                                          EX-TAB14
                                          EX-TABA
           MOVE 'E'                    TO EX-EOR

           MOVE ELTRLR-DETAIL-RECORD   TO WS-SAVE-ELTRLR
           PERFORM 0550-START-ELTRLR   THRU 0550-EXIT
           PERFORM 0200-READ-ELTRLR    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .

       8590-EXIT.
           EXIT.

