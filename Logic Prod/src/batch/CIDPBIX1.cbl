       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDPBIX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.
060608*SECURITY.   *****************************************************
060608*            *                                                   *
060608*            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
060608*            *                                                   *
060608*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
060608*            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
060608*            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
060608*            *                                                   *
060608*            *****************************************************

060608******************************************************************
060608*REMARKS.                                                        *
060608*        THIS PROGRAM RUNS DAILY AND CREATES AN EXTRACT          *
060608*        OF PENDING BUSINESS (ERPNDB) ISSUE RECORDS, BYPASSING   *
060608*        ANY CLAIM CREATED ISSUES.                               *
060608******************************************************************
060608*                   C H A N G E   L O G
060608*
060608* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
060608*-----------------------------------------------------------------
060608*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
060608* EFFECTIVE    NUMBER
060608*-----------------------------------------------------------------
060608* 060608   2008010200006   PEMA  REMOVE POSSIBLE DELIMITERS
060608******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERPNDB           ASSIGN TO ERPNDB
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS PB-CONTROL-PRIMARY
                                   FILE STATUS IS ERPNDB-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT ERPNDB-OUT       ASSIGN TO ERPNDBOT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ERPNDB.

                                       COPY ERCPNDB.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERPNDB-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  ERPNDB-OUT-REC              PIC X(240).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDPBIX1  WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                  PIC X VALUE SPACES.
           88  END-OF-ERPNDB                VALUE 'Y'.
       77  ERPNDB-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  ERPNDB-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                       PIC S9(5) VALUE +0 COMP-3.

       01  ERPNDB-DETAIL-RECORD.
           12  EX-BATCH-NO             PIC X(6).
           12  EX-TAB1                 PIC X.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1A                PIC X.
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
           12  EX-INSURED-FIRST-NAME   PIC X(15).
           12  EX-TAB7A                PIC X.
           12  EX-INSURED-MID-INIT     PIC X.
           12  EX-TAB7B                PIC X.
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
           12  EX-LOAN-OFFICER         PIC X(5).
           12  EX-TAB23                PIC X.
           12  EX-SIG-SW               PIC X.
           12  EX-TAB24                PIC X.
           12  EX-EOR                  PIC X.

      ******************************************************************
       01  WS-MISC.
           05  PGM-SUB                 PIC S9(4)   VALUE +515.
           05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           05  WS-ZERO                 PIC S9      VALUE ZERO.
           05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.
           05  WS-SAVE-ERPNDB          PIC X(240) VALUE LOW-VALUES.
           05  ERPNDB-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
              (END-OF-ERPNDB)
PEMTST*       OR (CRT-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' PNDB RECORDS READ    '  ERPNDB-RECS-IN
           DISPLAY ' PNDB RECORDS WRITTEN '  ERPNDB-RECS-OUT
           GOBACK

           .
       0050-PROCESS-INPUT.

           IF (PB-RECORD-TYPE = '1')
              AND (PB-ENTRY-BATCH (1:3) NOT = '#CL')
              PERFORM 0100-PROCESS-ERPNDB
                                       THRU 0100-EXIT
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
           MOVE PB-I-INSURED-FIRST-NAME TO EX-INSURED-FIRST-NAME
           MOVE PB-I-INSURED-MIDDLE-INIT  TO EX-INSURED-MID-INIT
           MOVE PB-I-SIG-SW            TO EX-SIG-SW
           MOVE PB-I-LF-BENEFIT-CD     TO EX-LF-BENEFIT-CD
           MOVE PB-I-LF-TERM           TO EX-LF-ORIG-TERM
           MOVE PB-I-LF-BENEFIT-AMT    TO EX-LF-BENEFIT-AMT
           MOVE PB-I-LF-PREMIUM-AMT    TO EX-LF-PREMIUM-AMT
           MOVE ZEROS                  TO EX-LF-ITD-CANCEL-AMT
           MOVE PB-I-AH-BENEFIT-CD     TO EX-AH-BENEFIT-CD
           MOVE PB-I-AH-TERM           TO EX-AH-ORIG-TERM
           MOVE PB-I-AH-BENEFIT-AMT    TO EX-AH-BENEFIT-AMT
           MOVE PB-I-AH-PREMIUM-AMT    TO EX-AH-PREMIUM-AMT
           MOVE ZEROS                  TO EX-AH-ITD-CANCEL-AMT

           MOVE PB-I-LF-EXPIRE-DT      TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LF-LOAN-EXPIRE-DT
           END-IF

           MOVE PB-I-AH-EXPIRE-DT      TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-AH-LOAN-EXPIRE-DT
           END-IF
           MOVE PB-INPUT-DT            TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-ENTRY-DT
           END-IF

      *    MOVE PB-C-LF-CANCEL-DT      TO DC-BIN-DATE-1
      *    MOVE ' '                    TO DC-OPTION-CODE
      *    PERFORM 8510-DATE-CONVERSION
      *                                THRU 8590-EXIT
      *    IF NO-CONVERSION-ERROR
      *       MOVE DC-GREG-DATE-A-EDIT TO EX-LF-CANCEL-DT
      *    END-IF

      *    MOVE PB-C-AH-CANCEL-DT      TO DC-BIN-DATE-1
      *    MOVE ' '                    TO DC-OPTION-CODE
      *    PERFORM 8510-DATE-CONVERSION
      *                                THRU 8590-EXIT
      *    IF NO-CONVERSION-ERROR
      *       MOVE DC-GREG-DATE-A-EDIT TO EX-AH-CANCEL-DT
      *    END-IF
           MOVE PB-I-LOAN-OFFICER      TO EX-LOAN-OFFICER
           PERFORM 0300-WRITE-PNDB     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-ERPNDB.

           READ ERPNDB NEXT RECORD

           IF (ERPNDB-FILE-STATUS = '10' OR '23')
              OR (PB-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ERPNDB        TO TRUE
           ELSE
              IF ERPNDB-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ON ERPNDB - READNEXT '
                    ERPNDB-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT END-OF-ERPNDB
              ADD 1                    TO ERPNDB-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-PNDB.

           INSPECT ERPNDB-DETAIL-RECORD REPLACING
              ALL ';'     BY ' '
              ALL X'00'   BY ' '
              ALL X'09'   BY ';'

           WRITE ERPNDB-OUT-REC        FROM ERPNDB-DETAIL-RECORD
           ADD 1 TO ERPNDB-RECS-OUT

           .
       0300-EXIT.
           EXIT.


       0350-START-ERPNDB.

           MOVE LOW-VALUES             TO PB-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO PB-COMPANY-CD

           START ERPNDB KEY >= PB-CONTROL-PRIMARY

           IF ERPNDB-FILE-STATUS = '10' OR '23'
              SET END-OF-ERPNDB        TO TRUE
           ELSE
              IF ERPNDB-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ON ERPNDB - START  '
                    ERPNDB-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0350-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT ERPNDB
               OUTPUT ERPNDB-OUT

           IF ERPNDB-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERPNDB - OPEN   '
                 ERPNDB-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERPNDB ERPNDB-OUT

           .
       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO ERPNDB-DETAIL-RECORD
           MOVE X'09'                  TO EX-TAB1
                                          EX-TAB1A
                                          EX-TAB2
                                          EX-TAB3
                                          EX-TAB4
                                          EX-TAB5
                                          EX-TAB6
                                          EX-TAB7
                                          EX-TAB7A
                                          EX-TAB7B
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
           MOVE 'E'                    TO EX-EOR

           MOVE ERPNDB-DETAIL-RECORD   TO WS-SAVE-ERPNDB
           PERFORM 0350-START-ERPNDB   THRU 0350-EXIT
           PERFORM 0200-READ-ERPNDB    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
