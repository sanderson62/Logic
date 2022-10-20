       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDPBX1.
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
060608*        OF PENDING BUSINESS (ERPNDB)       RECORDS, BYPASSING   *
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
041210* 041210   2010010400006   PEMA  ADD 4 NEW COLUMNS
040312* 040312  IR2012040300004  PEMA  GO TO EOJ IF NO RECORDS ON FILE
012115* 012115  IR2015012000001  PEMA  Process last record in file.
013118* 013118  CR2017062000002  PEMA  ADD CITY AND STATE TO EXTRACT
012220* 012220  CR2018092700002  TANA  ADD PRINT REQUIRED TO EXTRACT
072122* 072122  CR2022071900001  PEMA  Add cancel reason to extract
060608******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERPNDB           ASSIGN TO ERPNDB2
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS PB-CONTROL-BY-ACCOUNT
                                   FILE STATUS IS ERPNDB-FILE-STATUS.

013118     SELECT ERPNDM           ASSIGN TO ERPNDM
013118                             ORGANIZATION IS INDEXED
013118                             ACCESS IS DYNAMIC
013118                             RECORD KEY IS PM-CONTROL-PRIMARY
013118                             FILE STATUS IS ERPNDM-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT ERPNDB-OUT       ASSIGN TO ERPNDBOT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ERPNDB.

                                       COPY ERCPNDB.

013118 FD  ERPNDM.
013118
013118                                 COPY ERCPNDM.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ERPNDB-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

072122 01  ERPNDB-OUT-REC              PIC X(347).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDPBX1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ERPNDB                VALUE 'Y'.
       77  ERPNDB-RECS-IN              PIC 9(9) VALUE ZEROS.
       77  ERPNDB-RECS-OUT             PIC 9(9) VALUE ZEROS.
       77  SUB1                        PIC S9(5) VALUE +0 COMP-3.
       77  WS-PREV-PNDB-KEY            PIC X(23) VALUE LOW-VALUES.
013118 77  ws-current-bin-dt           pic xx    value low-values.
013118 77  ws-work-age                 pic s999 comp-3 value zeros.

072122 01  WS-SAVE-ERPNDB              PIC X(347) VALUE LOW-VALUES.
       01  ERPNDB-DETAIL-RECORD.
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
           12  EX-BATCH-NO             PIC X(6).
           12  EX-TAB7                 PIC X.
           12  EX-INSURED-LAST-NAME    PIC X(15).
           12  EX-TAB8                 PIC X.
           12  EX-INSURED-FIRST-NAME   PIC X(15).
           12  EX-TAB9                 PIC X.
           12  EX-INSURED-MID-INIT     PIC X.
           12  EX-TAB10                PIC X.
           12  EX-LF-BENEFIT-CD        PIC XX.
           12  EX-TAB11                PIC X.
           12  EX-LF-ORIG-TERM         PIC 999.
           12  EX-TAB12                PIC X.
           12  EX-LF-BENEFIT-AMT       PIC -9(9).99.
           12  EX-TAB13                PIC X.
           12  EX-LF-PREMIUM-AMT       PIC -9(7).99.
           12  EX-TAB14                PIC X.
           12  EX-LF-ITD-CANCEL-AMT    PIC -9(7).99.
           12  EX-TAB15                PIC X.
           12  EX-AH-BENEFIT-CD        PIC XX.
           12  EX-TAB16                PIC X.
           12  EX-AH-ORIG-TERM         PIC 999.
           12  EX-TAB17                PIC X.
           12  EX-AH-BENEFIT-AMT       PIC -9(7).99.
           12  EX-TAB18                PIC X.
           12  EX-AH-PREMIUM-AMT       PIC -9(7).99.
           12  EX-TAB19                PIC X.
           12  EX-AH-ITD-CANCEL-AMT    PIC -9(7).99.
           12  EX-TAB20                PIC X.
           12  EX-LF-LOAN-EXPIRE-DT    PIC X(10).
           12  EX-TAB21                PIC X.
           12  EX-AH-LOAN-EXPIRE-DT    PIC X(10).
           12  EX-TAB22                PIC X.
           12  EX-INPUT-DT             PIC X(10).
           12  EX-TAB23                PIC X.
           12  EX-LF-CANCEL-DT         PIC X(10).
           12  EX-TAB24                PIC X.
           12  EX-AH-CANCEL-DT         PIC X(10).
           12  EX-TAB25                PIC X.
           12  EX-LOAN-OFFICER         PIC X(5).
           12  EX-TAB26                PIC X.
           12  EX-SIG-SW               PIC X.
           12  EX-TAB27                PIC X.
           12  EX-ENTRY-STATUS         PIC X.
           12  EX-TAB28                PIC X.
           12  EX-RECORD-TYPE          PIC X.
           12  EX-TAB29                PIC X.
           12  EX-PROCESS-SW           PIC X.
           12  EX-TAB30                PIC X.
041210     12  EX-MONTH-END-DT         PIC X(10).
041210     12  EX-TAB31                PIC X.
041210     12  EX-CSR                  PIC X(4).
041210     12  EX-TAB32                PIC X.
041210     12  EX-LAST-MAINT-BY        PIC X(4).
041210     12  EX-TAB33                PIC X.
041210     12  EX-INPUT-BY             PIC X(4).
           12  EX-TAB34                PIC X.
013118     12  ex-ins-city             pic x(30).
013118     12  ex-tab35                pic x.
013118     12  ex-ins-state            pic xx.
013118     12  ex-tab36                pic x.
013118     12  ex-curr-age             pic 99.
013118     12  ex-tab37                pic x.
013118     12  EX-JOINT-LAST-NAME      PIC X(15).
013118     12  EX-TAB38                PIC X.
013118     12  EX-JOINT-FIRST-NAME     PIC X(15).
013118     12  EX-TAB39                PIC X.
013118     12  ex-curr-jnt-age         pic 99.
013118     12  ex-tab40                pic x.
012220     12  EX-LETTER-REQD          PIC X.
012220     12  EX-TAB41                PIC X.
072122     12  ex-cancel-reason        pic x.
072122     12  ex-tab42                pic x.
           12  EX-EOR                  PIC X.

      ******************************************************************
       01  WS-MISC.
           05  PGM-SUB                 PIC S9(4)   VALUE +515.
           05  WS-RETURN-CODE          PIC S9(3)   VALUE ZERO.
           05  WS-ABEND-MESSAGE        PIC X(80)   VALUE SPACES.
           05  WS-ZERO                 PIC S9      VALUE ZERO.
           05  WS-ABEND-FILE-STATUS    PIC XX      VALUE ZERO.
           05  ERPNDB-FILE-STATUS      PIC XX    VALUE ZEROS.
013118     05  ERPNDM-FILE-STATUS      PIC XX    VALUE ZEROS.
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

           PERFORM 0300-WRITE-PNDB     THRU 0300-EXIT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' PNDB RECORDS READ    '  ERPNDB-RECS-IN
           DISPLAY ' PNDB RECORDS WRITTEN '  ERPNDB-RECS-OUT
           GOBACK

           .
       0050-PROCESS-INPUT.

           IF (PB-RECORD-TYPE = '1' OR '2')
              AND (PB-ENTRY-BATCH (1:3) NOT = '#CL')
              PERFORM 0100-PROCESS-ERPNDB
                                       THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-ERPNDB    THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-ERPNDB.

           IF WS-PREV-PNDB-KEY NOT = PB-CONTROL-BY-ACCOUNT (2:32)
              IF WS-PREV-PNDB-KEY NOT = LOW-VALUES
                 PERFORM 0300-WRITE-PNDB
                                       THRU 0300-EXIT
              END-IF
              MOVE WS-SAVE-ERPNDB      TO ERPNDB-DETAIL-RECORD
              PERFORM 0110-BUILD-COMMON
                                       THRU 0110-EXIT
           END-IF

           MOVE PB-CONTROL-BY-ACCOUNT (2:32)
                                       TO WS-PREV-PNDB-KEY

           IF (PB-RECORD-TYPE = '1')
              AND (PB-I-ENTRY-STATUS NOT = 'D' AND 'V' AND '9')
              PERFORM 0120-BUILD-ISSUE THRU 0120-EXIT
           ELSE
              IF PB-RECORD-TYPE = '2'
                 PERFORM 0130-BUILD-CANCEL
                                       THRU 0130-EXIT
              END-IF
           END-IF

           .
       0100-EXIT.
           EXIT.

       0110-BUILD-COMMON.

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

           MOVE PB-INPUT-DT            TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-INPUT-DT
           END-IF

041210     MOVE PB-CREDIT-SELECT-DT    TO DC-BIN-DATE-1
041210     MOVE ' '                    TO DC-OPTION-CODE
041210     PERFORM 8510-DATE-CONVERSION
041210                                 THRU 8590-EXIT
041210     IF NO-CONVERSION-ERROR
041210        MOVE DC-GREG-DATE-A-EDIT TO EX-MONTH-END-DT
041210     END-IF
041210
041210     MOVE PB-INPUT-BY            TO EX-INPUT-BY
           if pb-csr-id = low-values or high-values
              move spaces              to pb-csr-id
           end-if
041210     MOVE PB-CSR-ID              TO EX-CSR
041210     MOVE PB-LAST-MAINT-BY       TO EX-LAST-MAINT-BY

           .
       0110-EXIT.
           EXIT.

       0120-BUILD-ISSUE.

           MOVE PB-I-INSURED-LAST-NAME
                                       TO EX-INSURED-LAST-NAME
           MOVE PB-I-INSURED-FIRST-NAME
                                       TO EX-INSURED-FIRST-NAME
           MOVE PB-I-INSURED-MIDDLE-INIT
                                       TO EX-INSURED-MID-INIT
           MOVE PB-I-SIG-SW            TO EX-SIG-SW
           MOVE PB-I-ENTRY-STATUS      TO EX-ENTRY-STATUS
           MOVE '1'                    TO EX-RECORD-TYPE
           MOVE PB-I-LF-BENEFIT-CD     TO EX-LF-BENEFIT-CD
           MOVE PB-I-LF-TERM           TO EX-LF-ORIG-TERM
           MOVE PB-I-LF-BENEFIT-AMT    TO EX-LF-BENEFIT-AMT

           IF PB-I-ENTRY-STATUS NOT = '5'
              COMPUTE EX-LF-PREMIUM-AMT = PB-I-LF-PREMIUM-AMT +
                 PB-I-LF-ALT-PREMIUM-AMT
              MOVE PB-I-AH-PREMIUM-AMT
                                       TO EX-AH-PREMIUM-AMT
           END-IF

           MOVE PB-I-AH-BENEFIT-CD     TO EX-AH-BENEFIT-CD
           MOVE PB-I-AH-TERM           TO EX-AH-ORIG-TERM
           MOVE PB-I-AH-BENEFIT-AMT    TO EX-AH-BENEFIT-AMT

           IF PB-I-LF-EXPIRE-DT NOT = LOW-VALUES
              MOVE PB-I-LF-EXPIRE-DT   TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-LF-LOAN-EXPIRE-DT
              END-IF
           END-IF

           IF PB-I-AH-EXPIRE-DT NOT = LOW-VALUES
              MOVE PB-I-AH-EXPIRE-DT   TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-AH-LOAN-EXPIRE-DT
              END-IF
           END-IF

           MOVE PB-I-LOAN-OFFICER      TO EX-LOAN-OFFICER

           IF (PB-FATAL-ERRORS)
              OR (PB-UNFORCED-ERRORS)
              OR (PB-RECORD-ON-HOLD)
              OR (PB-RECORD-RETURNED)
              MOVE 'N'                 TO EX-PROCESS-SW
           ELSE
              MOVE 'Y'                 TO EX-PROCESS-SW
           END-IF

012220     MOVE PB-I-LETTER-REQD       TO EX-LETTER-REQD

013118     if (pb-i-age not numeric)
013118               or
013118        (pb-i-age = zeros)
013118        move 42                  to pb-i-age
013118     end-if
013118
013118     move pb-i-age               to ex-curr-age
013118
013118     if pb-i-birthday <> low-values
013118        move pb-i-birthday       to dc-bin-date-1
013118        move ws-current-bin-dt   to dc-bin-date-2
013118        move '1'                 to dc-option-code
013118        perform 8510-date-conversion
013118                                 thru 8590-exit
013118        if no-conversion-error
013118*          display ' did dob ' dc-elapsed-months
013118           compute ws-work-age = dc-elapsed-months / 12
013118           move ws-work-age      to ex-curr-age
013118           go to 0120-joint-stuff
013118        end-if
013118     end-if
013118
013118     move pb-cert-eff-dt         to dc-bin-date-1
013118     move ws-current-bin-dt      to dc-bin-date-2
013118     move '1'                    to dc-option-code
013118     perform 8510-date-conversion
013118                                 thru 8590-exit
013118     if no-conversion-error
013118*       display ' did age ' pb-i-age ' ' dc-elapsed-months
013118        compute ws-work-age = pb-i-age + (dc-elapsed-months / 12)
013118        move ws-work-age         to ex-curr-age
013118     end-if
013118
013118     .
013118 0120-joint-stuff.
013118
013118     if pb-i-joint-last-name = spaces
013118        and pb-i-joint-first-name = spaces
013118        go to 0120-continue
013118     end-if
013118
013118     MOVE PB-I-JOINT-LAST-NAME   TO EX-JOINT-LAST-NAME
013118     MOVE PB-I-JOINT-FIRST-NAME  TO EX-JOINT-FIRST-NAME
013118
013118     if pb-i-joint-birthday <> low-values
013118        move pb-i-joint-birthday to dc-bin-date-1
013118        move ws-current-bin-dt   to dc-bin-date-2
013118        move '1'                 to dc-option-code
013118        perform 8510-date-conversion
013118                                 thru 8590-exit
013118        if no-conversion-error
013118           display ' did jdob ' dc-elapsed-months
013118           compute ws-work-age = dc-elapsed-months / 12
013118           move ws-work-age      to ex-curr-jnt-age
013118           go to 0120-continue
013118        end-if
013118     end-if
013118
013118     move pb-cert-eff-dt         to dc-bin-date-1
013118     move ws-current-bin-dt      to dc-bin-date-2
013118     move '1'                    to dc-option-code
013118     perform 8510-date-conversion
013118                                 thru 8590-exit
013118     if no-conversion-error
013118        display ' did age ' pb-i-joint-age ' ' dc-elapsed-months
013118        compute ws-work-age =
013118           pb-i-joint-age + (dc-elapsed-months / 12)
013118        move ws-work-age         to ex-curr-jnt-age
013118     end-if
013118
013118     .
013118 0120-continue.
013118
013118     move pb-control-primary     to pm-control-primary
013118     read erpndm
013118     if erpndm-file-status = '00'
013118        move pm-city             to ex-ins-city
013118        move pm-state            to ex-ins-state
013118     end-if

           .
       0120-EXIT.
           EXIT.

       0130-BUILD-CANCEL.

           MOVE PB-CI-LAST-NAME        TO EX-INSURED-LAST-NAME
           MOVE PB-CI-FIRST-NAME       TO EX-INSURED-FIRST-NAME
           MOVE PB-CI-INITIALS (2:1)   TO EX-INSURED-MID-INIT
           MOVE PB-CI-ENTRY-STATUS     TO EX-ENTRY-STATUS
           IF EX-RECORD-TYPE = '1'
              MOVE '3'                 TO EX-RECORD-TYPE
           ELSE
              MOVE '2'                 TO EX-RECORD-TYPE
           END-IF

           MOVE PB-CI-LF-BENEFIT-CD    TO EX-LF-BENEFIT-CD
           MOVE PB-CI-LF-TERM          TO EX-LF-ORIG-TERM
           MOVE PB-CI-LF-BENEFIT-AMT   TO EX-LF-BENEFIT-AMT

           MOVE PB-CI-AH-BENEFIT-CD    TO EX-AH-BENEFIT-CD
           MOVE PB-CI-AH-TERM          TO EX-AH-ORIG-TERM
           MOVE PB-CI-AH-BENEFIT-AMT   TO EX-AH-BENEFIT-AMT

           IF PB-CI-LF-EXPIRE-DT NOT = LOW-VALUES
              MOVE PB-CI-LF-EXPIRE-DT  TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-LF-LOAN-EXPIRE-DT
              END-IF
           END-IF
   
           IF PB-CI-AH-EXPIRE-DT NOT = LOW-VALUES
              MOVE PB-CI-AH-EXPIRE-DT  TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-AH-LOAN-EXPIRE-DT
              END-IF
           END-IF

           IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES
              MOVE PB-C-LF-CANCEL-DT   TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-LF-CANCEL-DT
              END-IF
           END-IF
          
           IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES
              MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-AH-CANCEL-DT
              END-IF
           END-IF

           MOVE PB-CI-LOAN-OFFICER     TO EX-LOAN-OFFICER
           MOVE PB-C-LF-CANCEL-AMT     TO EX-LF-ITD-CANCEL-AMT
           MOVE PB-C-AH-CANCEL-AMT     TO EX-AH-ITD-CANCEL-AMT

           IF (PB-FATAL-ERRORS)
              OR (PB-UNFORCED-ERRORS)
              OR (PB-RECORD-ON-HOLD)
              OR (PB-RECORD-RETURNED)
              MOVE 'N'                 TO EX-PROCESS-SW
           ELSE
              MOVE 'Y'                 TO EX-PROCESS-SW
           END-IF

072122     if pb-c-cancel-reason <> spaces and low-values and
072122        high-values
072122        move pb-c-cancel-reason     to ex-cancel-reason
072122     end-if

           .
       0130-EXIT.
           EXIT.

       0200-READ-ERPNDB.

           READ ERPNDB NEXT RECORD

           IF (ERPNDB-FILE-STATUS = '10' OR '23')
              OR (PB-COMPANY-CD-A1 NOT = DTE-CLASIC-COMPANY-CD)
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

041210     IF (EX-LF-ORIG-TERM NOT NUMERIC)
041210        AND (EX-AH-ORIG-TERM NOT NUMERIC)
041210        DISPLAY ' BYPASSING PENDING RECORD '
041210           ERPNDB-DETAIL-RECORD (1:50)
041210     ELSE
041210        WRITE ERPNDB-OUT-REC        FROM ERPNDB-DETAIL-RECORD
041210        ADD 1 TO ERPNDB-RECS-OUT
041210     END-IF

           .
       0300-EXIT.
           EXIT.


       0350-START-ERPNDB.

           MOVE LOW-VALUES             TO PB-CONTROL-BY-ACCOUNT
           MOVE DTE-CLASIC-COMPANY-CD  TO PB-COMPANY-CD-A1

           START ERPNDB KEY >= PB-CONTROL-BY-ACCOUNT

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

013118     OPEN INPUT ERPNDB ERPNDM
               OUTPUT ERPNDB-OUT

           IF ERPNDB-FILE-STATUS = '00' OR '97'
              CONTINUE
           ELSE
              DISPLAY ' ERROR ON ERPNDB - OPEN   '
                 ERPNDB-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

013118     IF ERPNDM-FILE-STATUS = '00' OR '97'
013118        CONTINUE
013118     ELSE
013118        DISPLAY ' ERROR ON ERPNDM - OPEN   '
013118           ERPNDM-FILE-STATUS
013118        PERFORM ABEND-PGM
013118     END-IF

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

013118     CLOSE ERPNDB ERPNDM ERPNDB-OUT

           .
       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO ERPNDB-DETAIL-RECORD
           MOVE X'09'                  TO EX-TAB1
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
                                          EX-TAB27
                                          EX-TAB28
                                          EX-TAB29
                                          EX-TAB30
041210                                    EX-TAB31
041210                                    EX-TAB32
041210                                    EX-TAB33
041210                                    EX-TAB34
013118                                    EX-TAB35
013118                                    EX-TAB36
013118                                    EX-TAB37
013118                                    EX-TAB38
013118                                    EX-TAB39
013118                                    EX-TAB40
012220                                    EX-TAB41
072122                                    ex-tab42

           MOVE 'E'                    TO EX-EOR

           MOVE ZEROS                  TO EX-LF-BENEFIT-AMT
                                          EX-LF-PREMIUM-AMT
                                          EX-LF-ITD-CANCEL-AMT
                                          EX-AH-BENEFIT-AMT
                                          EX-AH-PREMIUM-AMT
                                          EX-AH-ITD-CANCEL-AMT
013118                                    ex-curr-age
013118                                    ex-curr-jnt-age

           MOVE ERPNDB-DETAIL-RECORD   TO WS-SAVE-ERPNDB

013118     display ' current date ' ws-current-date
013118
013118     MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT
013118     MOVE '2'                    TO DC-OPTION-CODE
013118     PERFORM 8510-date-conversion thru 8590-exit
013118     MOVE DC-BIN-DATE-1          TO ws-current-bin-dt
        
           PERFORM 0350-START-ERPNDB   THRU 0350-EXIT
040312     if not end-of-erpndb
040312        PERFORM 0200-READ-ERPNDB THRU 0200-EXIT
040312     end-if

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
