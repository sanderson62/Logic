       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMPBX2.
       AUTHOR.     PABLO.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERPNDB           ASSIGN TO ERPNDB
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS PB-CONTROL-PRIMARY
                                   FILE STATUS IS ERPNDB-FILE-STATUS.

           SELECT ERPNDB-OUT       ASSIGN TO ERPNDBOT
               organization is line sequential.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ERPNDB.

                                       COPY ERCPNDB.

       FD  ERPNDB-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  ERPNDB-OUT-REC              PIC X(591).

           EJECT

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMPBX2  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ERPNDB             VALUE 'Y'.
       77  ERPNDB-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  ERPNDB-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.

       01  ERPNDB-DETAIL-RECORD.
           12  EX-BATCH-NO             PIC X(6).
           12  EX-TABA                 PIC X.
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
           12  EX-INSURED-LAST-NAME    PIC X(15).
           12  EX-TAB7                 PIC X.
           12  EX-LF-BENEFIT-CD        PIC XX.
           12  EX-TAB8                 PIC X.
           12  EX-LF-ORIG-TERM         PIC 999.
           12  EX-TAB9                 PIC X.
           12  EX-LF-BENEFIT-AMT       PIC -9(9).99.
           12  EX-TAB10                PIC X.
           12  EX-LF-PREMIUM-AMT       PIC -9(7).99.
           12  EX-TAB11                PIC X.
           12  EX-LF-ITD-CANCEL-AMT    PIC -9(7).99.
           12  EX-TAB12                PIC X.
           12  EX-AH-BENEFIT-CD        PIC XX.
           12  EX-TAB13                PIC X.
           12  EX-AH-ORIG-TERM         PIC 999.
           12  EX-TAB14                PIC X.
           12  EX-AH-BENEFIT-AMT       PIC -9(7).99.
           12  EX-TAB15                PIC X.
           12  EX-AH-PREMIUM-AMT       PIC -9(7).99.
           12  EX-TAB16                PIC X.
           12  EX-AH-ITD-CANCEL-AMT    PIC -9(7).99.
           12  EX-TAB17                PIC X.
           12  EX-LF-LOAN-EXPIRE-DT    PIC X(10).
           12  EX-TAB18                PIC X.
           12  EX-AH-LOAN-EXPIRE-DT    PIC X(10).
           12  EX-TAB19                PIC X.
           12  EX-ENTRY-DT             PIC X(10).
           12  EX-TAB20                PIC X.
           12  EX-LF-CANCEL-DT         PIC X(10).
           12  EX-TAB21                PIC X.
           12  EX-AH-CANCEL-DT         PIC X(10).
           12  EX-TAB22                PIC X.
           12  EX-JOINT-BIRTHDATE      PIC X(10).
           12  EX-TAB23                PIC X.
           12  EX-JOINT-AGE            PIC 99.
           12  EX-TAB24                PIC X.
           12  EX-FORCE                PIC X(10).
           12  EX-TAB25                PIC X.
           12  EX-ERRORS1              PIC X(4).
           12  EX-COM1                 PIC X.
           12  EX-ERRORS2              PIC X(4).
           12  EX-COM2                 PIC X.
           12  EX-ERRORS3              PIC X(4).
           12  EX-COM3                 PIC X.
           12  EX-ERRORS4              PIC X(4).
           12  EX-COM4                 PIC X.
           12  EX-ERRORS5              PIC X(4).
           12  EX-COM5                 PIC X.
           12  EX-ERRORS6              PIC X(4).
           12  EX-COM6                 PIC X.
           12  EX-ERRORS7              PIC X(4).
           12  EX-COM7                 PIC X.
           12  EX-ERRORS8              PIC X(4).
           12  EX-COM8                 PIC X.
           12  EX-ERRORS9              PIC X(4).
           12  EX-COM9                 PIC X.
           12  EX-ERRORS10             PIC X(4).
           12  EX-TAB26                PIC X.
           12  EX-EOR                  PIC X.

      ******************************************************************
       01  WS-MISC.
           05  WS-SAVE-ERPNDB          PIC X(300) VALUE LOW-VALUES.
           05  ERPNDB-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-ERPNDB THRU 0050-EXIT UNTIL
                 (END-OF-ERPNDB)
PEMTST*          OR (CRT-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' PNDB RECORDS READ    '  ERPNDB-RECS-IN
           DISPLAY ' PNDB RECORDS WRITTEN '  ERPNDB-RECS-OUT
           GOBACK

           .
       0050-PROCESS-ERPNDB.

           IF PB-RECORD-TYPE = '1'
              IF (PB-COMMON-ERROR (1) = 9503 OR 2686 OR 2685 OR 2627
                 OR 2696)
                 OR (PB-COMMON-ERROR (2) = 9503 OR 2686 OR 2685 OR 2627
                 OR 2696)
                 OR (PB-COMMON-ERROR (3) = 9503 OR 2686 OR 2685 OR 2627
                 OR 2696)
                 OR (PB-COMMON-ERROR (4) = 9503 OR 2686 OR 2685 OR 2627
                 OR 2696)
                 OR (PB-COMMON-ERROR (5) = 9503 OR 2686 OR 2685 OR 2627
                 OR 2696)
                 OR (PB-COMMON-ERROR (6) = 9503 OR 2686 OR 2685 OR 2627
                 OR 2696)
                 OR (PB-COMMON-ERROR (7) = 9503 OR 2686 OR 2685 OR 2627
                 OR 2696)
                 OR (PB-COMMON-ERROR (8) = 9503 OR 2686 OR 2685 OR 2627
                 OR 2696)
                 OR (PB-COMMON-ERROR (9) = 9503 OR 2686 OR 2685 OR 2627
                 OR 2696)
                 OR (PB-COMMON-ERROR (10) = 9503 OR 2686 OR 2685 OR 2627
                 OR 2696)
                 PERFORM 0100-PROCESS-ERPNDB THRU 0100-EXIT
              END-IF
           END-IF

           PERFORM 0200-READ-ERPNDB    THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-ERPNDB.

           MOVE WS-SAVE-ERPNDB         TO ERPNDB-DETAIL-RECORD
           MOVE PB-ENTRY-BATCH         TO EX-BATCH-NO
           MOVE PB-CARRIER             TO EX-CARRIER
           MOVE PB-GROUPING            TO EX-GROUPING
           MOVE PB-STATE               TO EX-STATE
           MOVE PB-ACCOUNT             TO EX-ACCOUNT
           MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-CERT-EFF-DT
           END-IF

           MOVE PB-CERT-NO             TO EX-CERT-NO
           MOVE PB-I-INSURED-LAST-NAME TO EX-INSURED-LAST-NAME
           MOVE PB-I-LF-BENEFIT-CD    TO EX-LF-BENEFIT-CD
           MOVE PB-I-LF-TERM          TO EX-LF-ORIG-TERM
           MOVE PB-I-LF-BENEFIT-AMT   TO EX-LF-BENEFIT-AMT
           MOVE PB-I-LF-PREMIUM-AMT   TO EX-LF-PREMIUM-AMT
           MOVE ZEROS                  TO EX-LF-ITD-CANCEL-AMT
           MOVE PB-I-AH-BENEFIT-CD    TO EX-AH-BENEFIT-CD
           MOVE PB-I-AH-TERM          TO EX-AH-ORIG-TERM
           MOVE PB-I-AH-BENEFIT-AMT   TO EX-AH-BENEFIT-AMT
           MOVE PB-I-AH-PREMIUM-AMT   TO EX-AH-PREMIUM-AMT
           MOVE ZEROS                  TO EX-AH-ITD-CANCEL-AMT

           MOVE PB-I-LF-EXPIRE-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LF-LOAN-EXPIRE-DT
           END-IF

           MOVE PB-I-AH-EXPIRE-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-AH-LOAN-EXPIRE-DT
           END-IF

           MOVE SPACES              TO EX-ENTRY-DT

           MOVE SPACES              TO EX-LF-CANCEL-DT

           MOVE SPACES              TO EX-AH-CANCEL-DT

           
           MOVE PB-I-JOINT-BIRTHDAY TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-JOINT-BIRTHDATE
           END-IF

           IF PB-I-JOINT-BIRTHDAY = LOW-VALUES OR SPACES
              MOVE 'SPACES'            TO EX-JOINT-BIRTHDATE
           END-IF
           MOVE PB-I-JOINT-AGE         TO EX-JOINT-AGE
           IF PB-FATAL-ERRORS
              MOVE 'FATAL'             TO EX-FORCE
           ELSE
              IF PB-FORCE-ERRORS
                 MOVE 'FORCED'         TO EX-FORCE
              ELSE
                 IF PB-UNFORCED-ERRORS
                    MOVE 'UNFORCED'    TO EX-FORCE
                 ELSE
                    MOVE SPACES        TO EX-FORCE
                 END-IF
              END-IF
           END-IF
           MOVE PB-COMMON-ERROR (1)    TO EX-ERRORS1
           MOVE PB-COMMON-ERROR (2)    TO EX-ERRORS2
           MOVE PB-COMMON-ERROR (3)    TO EX-ERRORS3
           MOVE PB-COMMON-ERROR (4)    TO EX-ERRORS4
           MOVE PB-COMMON-ERROR (5)    TO EX-ERRORS5
           MOVE PB-COMMON-ERROR (6)    TO EX-ERRORS6
           MOVE PB-COMMON-ERROR (7)    TO EX-ERRORS7
           MOVE PB-COMMON-ERROR (8)    TO EX-ERRORS8
           MOVE PB-COMMON-ERROR (9)    TO EX-ERRORS9
           MOVE PB-COMMON-ERROR (10)   TO EX-ERRORS10
           MOVE 'E'                    TO EX-EOR
           PERFORM 0300-WRITE-PNDB     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-ERPNDB.

           READ ERPNDB NEXT RECORD

           IF ERPNDB-FILE-STATUS = '10' OR '23'
              SET END-OF-ERPNDB        TO TRUE
           ELSE
              IF ERPNDB-FILE-STATUS NOT = '00'
                 DISPLAY 'ERPNDB READ NEXT ' ERPNDB-FILE-STATUS
                 SET END-OF-ERPNDB     TO TRUE
              else
                 if PB-company-cd > x'04'
                    set end-of-ERPNDB        to true
                 end-if
              END-IF
           END-IF

           IF NOT END-OF-ERPNDB
              ADD 1 TO ERPNDB-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-PNDB.

           WRITE ERPNDB-OUT-REC        FROM ERPNDB-DETAIL-RECORD
           ADD 1 TO ERPNDB-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERPNDB
               OUTPUT ERPNDB-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERPNDB ERPNDB-OUT

           .
       0500-EXIT.
           EXIT.

       0550-START-ERPNDB.

           MOVE LOW-VALUES             TO PB-CONTROL-PRIMARY
           MOVE X'04'                  TO PB-COMPANY-CD

           START ERPNDB KEY IS NOT < PB-CONTROL-PRIMARY

           IF ERPNDB-FILE-STATUS = '10' OR '23'
              SET END-OF-ERPNDB        TO TRUE
           ELSE
              IF ERPNDB-FILE-STATUS NOT = '00'
                 DISPLAY 'ERPNDB START     ' ERPNDB-FILE-STATUS
                 SET END-OF-ERPNDB     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO ERPNDB-DETAIL-RECORD
           MOVE ','                    TO EX-COM1
                                          EX-COM2
                                          EX-COM3
                                          EX-COM4
                                          EX-COM5
                                          EX-COM6
                                          EX-COM7
                                          EX-COM8
                                          EX-COM9
           MOVE ';'                    TO EX-TABA
                                          EX-TAB1
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
                                          EX-TAB15
                                          EX-TAB16
                                          EX-TAB17
                                          EX-TAB18
                                          EX-TAB19
                                          EX-TAB20
                                          EX-TAB21
                                          EX-TAB22
                                          EX-TAB23
                                          EX-TAB24
                                          EX-TAB25
                                          EX-TAB26

           MOVE ERPNDB-DETAIL-RECORD   TO WS-SAVE-ERPNDB
           PERFORM 0550-START-ERPNDB   THRU 0550-EXIT
           PERFORM 0200-READ-ERPNDB    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

