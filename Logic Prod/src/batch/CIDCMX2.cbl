       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDCMX2.
       AUTHOR.     SUZAN DOWNING.
       DATE-COMPILED.
072403******************************************************************
072403*                   C H A N G E   L O G
072403*
072403* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
072403*-----------------------------------------------------------------
072403*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
072403* EFFECTIVE    NUMBER
072403*-----------------------------------------------------------------
041604* 041604                   SMVA  NEW PGM FOR OAK HILLS BANK CERT
041604*                                DETAIL MODELED AFTER PEMCMX1
061104* 061104                   SMVA  CHG DELIMITER FROM TAB TO SEMI-COLON
091814* 091814  IR2014091800001  PEMA  INIT INVALID AMOUNTS
050420* 050420  IR2020042900001  PEMA  Fix garbage data
121020* 121020  CR2020120200004  PEMA  Add birth dates (PLUS) to extract.
072403******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELCERT           ASSIGN TO ELCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CM-CONTROL-PRIMARY
                                   FILE STATUS IS ELCERT-FILE-STATUS.

072403     SELECT ERMAIL           ASSIGN TO ERMAIL
072403                             ORGANIZATION IS INDEXED
072403                             ACCESS IS DYNAMIC
072403                             RECORD KEY IS MA-CONTROL-PRIMARY
072403                             FILE STATUS IS ERMAIL-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

           SELECT ELCERT-OUT       ASSIGN TO ELCERTOT
               ORGANIZATION IS LINE SEQUENTIAL.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ELCERT.

                                       COPY ELCCERT.

072403 FD  ERMAIL.
072403
072403     COPY ERCMAIL.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       FD  ELCERT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

072403 01  ELCERT-OUT-REC              PIC X(802).

           EJECT

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMCMX1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       01  PGM-SUB                     PIC S999 COMP  VALUE +515.
030404 01  WS-MISC.
           05  WS-EOF-SW               PIC X      VALUE SPACES.
               88  END-OF-ELCERT                  VALUE 'Y'.
           05  CRT-RECS-IN             PIC 9(9)   VALUE ZEROS.
           05  CRT-RECS-OUT            PIC 9(9)   VALUE ZEROS.
           05  SUB1                    PIC S9(5)  VALUE +0 COMP-3.
030404     05  WS-COMPANY-CD           PIC X(01)  VALUE SPACE.
030404     05  WS-CARRIER              PIC X(01)  VALUE SPACE.
030404     05  WS-STATE                PIC X(02)  VALUE SPACE.
030404     05  WS-ACCOUNT              PIC X(10)  VALUE SPACE.

           05  ELCERT-FILE-STATUS      PIC XX     VALUE ZEROS.
072403     05  ERMAIL-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)  VALUE ZEROS.

030404**** PROGRAM ABEND FIELDS
030404     05  WS-RETURN-CODE          PIC S9(03) VALUE +0.
030404     05  WS-ZERO                 PIC S9(01) VALUE +0.
030404     05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
030404     05  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZERO.

       01  elcert-detail-init          pic x(789) value spaces.
       01  ELCERT-DETAIL-RECORD.
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
           12  EX-INSURED-INITIAL1     PIC X.
           12  EX-TAB8                 PIC X.
           12  EX-INSURED-INITIAL2     PIC X.
           12  EX-TAB9                 PIC X.
           12  EX-SOC-SEC-NO           PIC X(11).
           12  EX-TAB10                PIC X.
           12  EX-MEMBER-NO            PIC X(12).
           12  EX-TAB11                PIC X.
           12  EX-INSURED-FIRST-NAME   PIC X(10).
           12  EX-TAB12                PIC X.
           12  EX-INSURED-ISSUE-AGE    PIC 99.
           12  EX-TAB13                PIC X.
121020     12  EX-INSURED-BIRTH-DT     PIC X(10).
121020     12  EX-TAB13A               PIC X.
           12  EX-INSURED-SEX          PIC X.
           12  EX-TAB14                PIC X.
           12  EX-INSURED-JOINT-AGE    PIC 99.
           12  EX-TAB15                PIC X.
           12  EX-JT-LAST-NAME         PIC X(15).
           12  EX-TAB16                PIC X.
           12  EX-JT-FIRST-NAME        PIC X(10).
           12  EX-TAB17                PIC X.
           12  EX-JT-INITIAL           PIC X.
           12  EX-TAB18                PIC X.
121020     12  EX-JT-BIRTH-DT          PIC X(10).
121020     12  EX-TAB18A               PIC X.
           12  EX-LF-BENEFIT-CD        PIC XX.
           12  EX-TAB19                PIC X.
           12  EX-LF-ORIG-TERM         PIC 999.
           12  EX-TAB20                PIC X.
           12  EX-LF-DEV-CODE          PIC XXX.
           12  EX-TAB21                PIC X.
           12  EX-LF-DEV-PCT           PIC -9.9(6).
           12  EX-TAB22                PIC X.
           12  EX-LF-BENEFIT-AMT       PIC -9(9).99.
           12  EX-TAB23                PIC X.
           12  EX-LF-PREMIUM-AMT       PIC -9(7).99.
           12  EX-TAB24                PIC X.
           12  EX-LF-ALT-BENEFIT-AMT   PIC -9(9).99.
           12  EX-TAB25                PIC X.
           12  EX-LF-ALT-PREMIUM-AMT   PIC -9(7).99.
           12  EX-TAB26                PIC X.
           12  EX-LF-NSP-PREMIUM-AMT   PIC -9(7).99.
           12  EX-TAB27                PIC X.
           12  EX-LF-REMAINING-AMT     PIC -9(9).99.
           12  EX-TAB28                PIC X.
           12  EX-LF-ITD-CANCEL-AMT    PIC -9(7).99.
           12  EX-TAB29                PIC X.
           12  EX-LF-ITD-DEATH-AMT     PIC -9(9).99.
           12  EX-TAB30                PIC X.
           12  EX-LF-PREMIUM-RATE      PIC -99.9(5).
           12  EX-TAB31                PIC X.
           12  EX-LF-ALT-PREMIUM-RATE  PIC -99.9(5).
           12  EX-TAB32                PIC X.
           12  EX-AH-BENEFIT-CD        PIC XX.
           12  EX-TAB33                PIC X.
           12  EX-AH-ORIG-TERM         PIC 999.
           12  EX-TAB34                PIC X.
           12  EX-AH-CRITICAL-PERIOD   PIC 999.
           12  EX-TAB35                PIC X.
           12  EX-AH-DEV-CODE          PIC XXX.
           12  EX-TAB36                PIC X.
           12  EX-AH-DEV-PCT           PIC -9.9(6).
           12  EX-TAB37                PIC X.
           12  EX-AH-BENEFIT-AMT       PIC -9(7).99.
           12  EX-TAB38                PIC X.
           12  EX-AH-PREMIUM-AMT       PIC -9(7).99.
           12  EX-TAB39                PIC X.
           12  EX-AH-NSP-PREMIUM-AMT   PIC -9(7).99.
           12  EX-TAB40                PIC X.
           12  EX-AH-ITD-CANCEL-AMT    PIC -9(7).99.
           12  EX-TAB41                PIC X.
           12  EX-AH-ITD-AH-PMT        PIC -9(9).99.
           12  EX-TAB42                PIC X.
           12  EX-AH-PAID-THRU-DT      PIC X(10).
           12  EX-TAB43                PIC X.
           12  EX-AH-PREMIUM-RATE      PIC -99.9(5).
           12  EX-TAB44                PIC X.
           12  EX-LOAN-APR             PIC -999.9(4).
           12  EX-TAB45                PIC X.
121020     12  EX-PMT-FREQ             PIC 99.
121020     12  EX-TAB45A               PIC X.
           12  EX-LOAN-TERM            PIC 999.
           12  EX-TAB46                PIC X.
           12  EX-RATE-CLASS           PIC XX.
           12  EX-TAB47                PIC X.
121020     12  EX-BENEFICIARY          PIC X(25).
121020     12  EX-TAB47A               PIC X.
           12  EX-PMT-EXTENSION-DAYS   PIC 999.
           12  EX-TAB48                PIC X.
           12  EX-CSR-CODE             PIC XXX.
           12  EX-TAB49                PIC X.
           12  EX-UNDERWRITING-CODE    PIC X.
           12  EX-TAB50                PIC X.
           12  EX-PREMIUM-TYPE         PIC X.
           12  EX-TAB51                PIC X.
           12  EX-IND-GRP-TYPE         PIC X.
           12  EX-TAB52                PIC X.
           12  EX-SKIP-CODE            PIC X.
           12  EX-TAB53                PIC X.
           12  EX-PAYMENT-MODE         PIC X.
           12  EX-TAB54                PIC X.
121020     12  EX-LOAN-NUMBER          PIC X(8).
121020     12  EX-TAB54A               PIC X.
           12  EX-LOAN-OFFICER         PIC X(5).
           12  EX-TAB55                PIC X.
           12  EX-REIN-TABLE           PIC XXX.
           12  EX-TAB56                PIC X.
           12  EX-SPECIAL-REIN-CODE    PIC X.
           12  EX-TAB57                PIC X.
           12  EX-LF-LOAN-EXPIRE-DT    PIC X(10).
           12  EX-TAB58                PIC X.
           12  EX-AH-LOAN-EXPIRE-DT    PIC X(10).
           12  EX-TAB59                PIC X.
           12  EX-LOAN-1ST-PMT-DT      PIC X(10).
           12  EX-TAB60                PIC X.
           12  EX-ENTRY-STATUS         PIC X.
           12  EX-TAB61                PIC X.
           12  EX-ENTRY-DT             PIC X(10).
           12  EX-TAB62                PIC X.
121020     12  EX-LF-STATUS-AT-CANCEL  PIC X.
121020     12  EX-TAB62A               PIC X.
           12  EX-LF-CANCEL-DT         PIC X(10).
           12  EX-TAB63                PIC X.
           12  EX-LF-CANCEL-EXIT-DT    PIC X(10).
           12  EX-TAB64                PIC X.
121020     12  EX-LF-STATUS-AT-DEATH   PIC X.
121020     12  EX-TAB64A               PIC X.
           12  EX-LF-DEATH-DT          PIC X(10).
           12  EX-TAB65                PIC X.
           12  EX-LF-DEATH-EXIT-DT     PIC X(10).
           12  EX-TAB66                PIC X.
           12  EX-LF-CURRENT-STATUS    PIC X.
           12  EX-TAB67                PIC X.
121020     12  EX-AH-STATUS-AT-CANCEL  PIC X.
121020     12  EX-TAB67A               PIC X.
           12  EX-AH-CANCEL-DT         PIC X(10).
           12  EX-TAB68                PIC X.
           12  EX-AH-CANCEL-EXIT-DT    PIC X(10).
           12  EX-TAB69                PIC X.
           12  EX-AH-CURRENT-STATUS    PIC X.
           12  EX-TAB70                PIC X.
121020     12  EX-CLAIM-INTERFACE-SW   PIC X.
121020     12  EX-TAB70A               PIC X.
121020     12  EX-CLAIM-ATTACHED-CNT   PIC 999.
121020     12  EX-TAB70B               PIC X.
           12  EX-ENTRY-BATCH          PIC X(6).
           12  EX-TAB71                PIC X.
           12  EX-LF-EXIT-BATCH        PIC X(6).
           12  EX-TAB72                PIC X.
           12  EX-AH-EXIT-BATCH        PIC X(6).
           12  EX-TAB73                PIC X.
121020     12  EX-INT-ON-REFS          PIC -9(7).99.
121020     12  EX-TAB73A               PIC X.
121020     12  EX-CRED-INT-SW-1        PIC X.
121020     12  EX-TAB73B               PIC X.
121020     12  EX-CRED-INT-SW-2        PIC X.
121020     12  EX-TAB73C               PIC X.
           12  EX-LIFE-COMM-PCT        PIC -.9(5).
           12  EX-TAB74                PIC X.
           12  EX-AH-COMM-PCT          PIC -.9(5).
           12  EX-TAB75                PIC X.
121020     12  EX-AH-CLASS-CD          PIC XX.
121020     12  EX-TAB75A               PIC X.
           12  EX-CERT-SFX             PIC X.
072403     12  EX-TAB76                PIC X.
072403     12  EX-ADDR1                PIC X(30).
072403     12  EX-TAB77                PIC X.
072403     12  EX-ADDR2                PIC X(30).
072403     12  EX-TAB78                PIC X.
072403     12  EX-CITY-ST              PIC X(30).
072403     12  EX-TAB79                PIC X.
072403     12  EX-ZIP                  PIC X(9).
           12  EX-TAB80                PIC X.
           12  EX-EOR                  PIC X.
      ******************************************************************

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-ELCERT THRU 0050-EXIT UNTIL
              (END-OF-ELCERT)
PEMTST*       OR (CRT-RECS-IN > 70000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CERT RECORDS READ    '  CRT-RECS-IN
           DISPLAY ' CERT RECORDS WRITTEN '  CRT-RECS-OUT
           GOBACK

           .
       0050-PROCESS-ELCERT.

           PERFORM 0100-PROCESS-ELCERT THRU 0100-EXIT
           PERFORM 0200-READ-ELCERT    THRU 0200-EXIT
           
           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-ELCERT.

050420     move elcert-detail-init     to elcert-detail-record
           MOVE CM-CARRIER             TO EX-CARRIER
           MOVE CM-GROUPING            TO EX-GROUPING
           MOVE CM-STATE               TO EX-STATE
           MOVE CM-ACCOUNT             TO EX-ACCOUNT
           MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-CERT-EFF-DT
           END-IF

           MOVE CM-CERT-NO             TO EX-CERT-NO
           MOVE CM-INSURED-LAST-NAME   TO EX-INSURED-LAST-NAME
           MOVE CM-INSURED-INITIAL1    TO EX-INSURED-INITIAL1
           MOVE CM-INSURED-INITIAL2    TO EX-INSURED-INITIAL2
           MOVE CM-SOC-SEC-NO          TO EX-SOC-SEC-NO
           MOVE CM-MEMBER-NO           TO EX-MEMBER-NO
           MOVE CM-INSURED-FIRST-NAME  TO EX-INSURED-FIRST-NAME
           MOVE CM-INSURED-ISSUE-AGE   TO EX-INSURED-ISSUE-AGE
           MOVE CM-INSURED-SEX         TO EX-INSURED-SEX
           MOVE CM-INSURED-JOINT-AGE   TO EX-INSURED-JOINT-AGE
           MOVE CM-JT-LAST-NAME        TO EX-JT-LAST-NAME
           MOVE CM-JT-FIRST-NAME       TO EX-JT-FIRST-NAME
           MOVE CM-JT-INITIAL          TO EX-JT-INITIAL
           MOVE CM-LF-BENEFIT-CD       TO EX-LF-BENEFIT-CD
           MOVE CM-LF-ORIG-TERM        TO EX-LF-ORIG-TERM
           MOVE CM-LF-DEV-CODE         TO EX-LF-DEV-CODE
           IF CM-LF-DEV-PCT NOT NUMERIC
              MOVE ZEROS               TO CM-LF-DEV-PCT
           END-IF
           MOVE CM-LF-DEV-PCT          TO EX-LF-DEV-PCT
091814     if cm-lf-benefit-amt not numeric
091814        move zeros               to cm-lf-benefit-amt
091814     end-if
           MOVE CM-LF-BENEFIT-AMT      TO EX-LF-BENEFIT-AMT
091814     if cm-lf-premium-amt not numeric
091814        move zeros               to cm-lf-premium-amt
091814     end-if
           MOVE CM-LF-PREMIUM-AMT      TO EX-LF-PREMIUM-AMT
           IF CM-LF-ALT-BENEFIT-AMT NOT NUMERIC
              MOVE ZEROS               TO CM-LF-ALT-BENEFIT-AMT
           END-IF
           MOVE CM-LF-ALT-BENEFIT-AMT  TO EX-LF-ALT-BENEFIT-AMT
           IF CM-LF-ALT-PREMIUM-AMT NOT NUMERIC
              MOVE ZEROS               TO CM-LF-ALT-PREMIUM-AMT
           END-IF
           MOVE CM-LF-ALT-PREMIUM-AMT  TO EX-LF-ALT-PREMIUM-AMT
           IF CM-LF-NSP-PREMIUM-AMT NOT NUMERIC
              MOVE ZEROS               TO CM-LF-NSP-PREMIUM-AMT
           END-IF
           MOVE CM-LF-NSP-PREMIUM-AMT  TO EX-LF-NSP-PREMIUM-AMT
           MOVE CM-LF-REMAINING-AMT    TO EX-LF-REMAINING-AMT
           MOVE CM-LF-ITD-CANCEL-AMT   TO EX-LF-ITD-CANCEL-AMT
           MOVE CM-LF-ITD-DEATH-AMT    TO EX-LF-ITD-DEATH-AMT
           IF CM-LF-PREMIUM-RATE NOT NUMERIC
              MOVE ZEROS               TO CM-LF-PREMIUM-RATE
           END-IF
           MOVE CM-LF-PREMIUM-RATE     TO EX-LF-PREMIUM-RATE
           IF CM-LF-ALT-PREMIUM-RATE NOT NUMERIC
              MOVE ZEROS               TO CM-LF-ALT-PREMIUM-RATE
           END-IF
           MOVE CM-LF-ALT-PREMIUM-RATE TO EX-LF-ALT-PREMIUM-RATE
           MOVE CM-AH-BENEFIT-CD       TO EX-AH-BENEFIT-CD
           MOVE CM-AH-ORIG-TERM        TO EX-AH-ORIG-TERM
           MOVE CM-AH-CRITICAL-PERIOD  TO EX-AH-CRITICAL-PERIOD
           MOVE CM-AH-DEV-CODE         TO EX-AH-DEV-CODE
           IF CM-AH-DEV-PCT NOT NUMERIC
              MOVE ZEROS               TO CM-AH-DEV-PCT
           END-IF
           MOVE CM-AH-DEV-PCT          TO EX-AH-DEV-PCT
           MOVE CM-AH-BENEFIT-AMT      TO EX-AH-BENEFIT-AMT
           MOVE CM-AH-PREMIUM-AMT      TO EX-AH-PREMIUM-AMT
           MOVE CM-AH-NSP-PREMIUM-AMT  TO EX-AH-NSP-PREMIUM-AMT
           MOVE CM-AH-ITD-CANCEL-AMT   TO EX-AH-ITD-CANCEL-AMT
           MOVE CM-AH-ITD-AH-PMT       TO EX-AH-ITD-AH-PMT

           MOVE CM-AH-PAID-THRU-DT     TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-AH-PAID-THRU-DT
           END-IF

           IF CM-AH-PREMIUM-RATE NOT NUMERIC
              MOVE ZEROS               TO CM-AH-PREMIUM-RATE
           END-IF
           MOVE CM-AH-PREMIUM-RATE     TO EX-AH-PREMIUM-RATE
           IF CM-LOAN-APR NOT NUMERIC
              MOVE ZEROS               TO CM-LOAN-APR
           END-IF
           MOVE CM-LOAN-APR            TO EX-LOAN-APR
121020     IF CM-PAY-FREQUENCY NOT NUMERIC
121020        MOVE ZEROS               TO CM-PAY-FREQUENCY
121020     END-IF
121020     MOVE CM-PAY-FREQUENCY       TO EX-PMT-FREQ
121020     MOVE CM-BENEFICIARY         TO EX-BENEFICIARY
121020     MOVE CM-LOAN-NUMBER         TO EX-LOAN-NUMBER
121020     MOVE CM-LF-STATUS-AT-CANCEL TO EX-LF-STATUS-AT-CANCEL
121020     MOVE CM-LF-STATUS-AT-DEATH  TO EX-LF-STATUS-AT-DEATH
121020     MOVE CM-AH-STATUS-AT-CANCEL TO EX-AH-STATUS-AT-CANCEL
121020     MOVE CM-CLAIM-INTERFACE-SW  TO EX-CLAIM-INTERFACE-SW
121020     IF CM-CLAIM-ATTACHED-COUNT NOT NUMERIC
121020        MOVE ZEROS               TO CM-CLAIM-ATTACHED-COUNT
121020     END-IF
121020     MOVE CM-CLAIM-ATTACHED-COUNT TO EX-CLAIM-ATTACHED-CNT
121020     IF CM-INT-ON-REFS NOT NUMERIC
121020        MOVE ZEROS               TO CM-INT-ON-REFS
121020     END-IF
121020     MOVE CM-INT-ON-REFS         TO EX-INT-ON-REFS
121020     MOVE CM-CREDIT-INTERFACE-SW-1 TO EX-CRED-INT-SW-1
121020     MOVE CM-CREDIT-INTERFACE-SW-2 TO EX-CRED-INT-SW-2
121020     MOVE CM-AH-CLASS-CD         TO EX-AH-CLASS-CD
           MOVE CM-LOAN-TERM           TO EX-LOAN-TERM
           MOVE CM-RATE-CLASS          TO EX-RATE-CLASS
           MOVE CM-PMT-EXTENSION-DAYS  TO EX-PMT-EXTENSION-DAYS
           MOVE CM-CSR-CODE            TO EX-CSR-CODE
           MOVE CM-UNDERWRITING-CODE   TO EX-UNDERWRITING-CODE
           MOVE CM-PREMIUM-TYPE        TO EX-PREMIUM-TYPE
           MOVE CM-IND-GRP-TYPE        TO EX-IND-GRP-TYPE
           MOVE CM-LOAN-OFFICER        TO EX-LOAN-OFFICER
           MOVE CM-REIN-TABLE          TO EX-REIN-TABLE
           MOVE CM-SPECIAL-REIN-CODE   TO EX-SPECIAL-REIN-CODE

           MOVE CM-LF-LOAN-EXPIRE-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LF-LOAN-EXPIRE-DT
           END-IF

           MOVE CM-AH-LOAN-EXPIRE-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-AH-LOAN-EXPIRE-DT
           END-IF

           MOVE CM-LOAN-1ST-PMT-DT     TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LOAN-1ST-PMT-DT
           END-IF

           MOVE CM-ENTRY-STATUS        TO EX-ENTRY-STATUS

           MOVE CM-ENTRY-DT            TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-ENTRY-DT
           END-IF

           MOVE CM-LF-CANCEL-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LF-CANCEL-DT
           END-IF

           MOVE CM-LF-CANCEL-EXIT-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LF-CANCEL-EXIT-DT
           END-IF

           MOVE CM-LF-DEATH-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LF-DEATH-DT
           END-IF

           MOVE CM-LF-DEATH-EXIT-DT    TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LF-DEATH-EXIT-DT
           END-IF

           MOVE CM-LF-CURRENT-STATUS   TO EX-LF-CURRENT-STATUS
           MOVE CM-AH-CANCEL-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-AH-CANCEL-DT
           END-IF

           MOVE CM-AH-CANCEL-EXIT-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-AH-CANCEL-EXIT-DT
           END-IF

           MOVE CM-AH-CURRENT-STATUS   TO EX-AH-CURRENT-STATUS
080603     IF CM-ENTRY-BATCH = LOW-VALUES
080603        MOVE SPACES              TO CM-ENTRY-BATCH
080603     END-IF
           MOVE CM-ENTRY-BATCH         TO EX-ENTRY-BATCH
           IF CM-LF-EXIT-BATCH = LOW-VALUES
              MOVE SPACES              TO CM-LF-EXIT-BATCH
           END-IF
           MOVE CM-LF-EXIT-BATCH       TO EX-LF-EXIT-BATCH
           IF CM-AH-EXIT-BATCH = LOW-VALUES
              MOVE SPACES              TO CM-AH-EXIT-BATCH
           END-IF
           MOVE CM-AH-EXIT-BATCH       TO EX-AH-EXIT-BATCH
           MOVE CM-LIFE-COMM-PCT       TO EX-LIFE-COMM-PCT
           MOVE CM-AH-COMM-PCT         TO EX-AH-COMM-PCT
           MOVE CM-CERT-SFX            TO EX-CERT-SFX
072403     MOVE CM-CONTROL-PRIMARY     TO MA-CONTROL-PRIMARY
072403     PERFORM 0350-READ-ERMAIL    THRU 0350-EXIT

           PERFORM 0300-WRITE-CERT     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-ELCERT.

           READ ELCERT NEXT RECORD

           IF (ELCERT-FILE-STATUS = '10' OR '23')
              OR (CM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ELCERT        TO TRUE
           ELSE
              IF ELCERT-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCERT READ NEXT ' ELCERT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           IF NOT END-OF-ELCERT
              ADD +1 TO CRT-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-CERT.

           INSPECT ELCERT-DETAIL-RECORD
              REPLACING ALL X'00'   BY SPACES
                        ALL X'01'   BY SPACES
                        ALL X'02'   BY SPACES
                        ALL X'03'   BY SPACES
                        ALL X'04'   BY SPACES
                        ALL X'05'   BY SPACES
                        ALL X'06'   BY SPACES
                        ALL X'07'   BY SPACES
                        ALL X'08'   BY SPACES
                        ALL X'09'   BY SPACES
                        ALL X'0C'   BY SPACES
                        ALL X'1C'   BY SPACES
050420                  ALL X'7C'   BY SPACES
050420                  ALL X'7E'   BY SPACES
                        ALL ';'     BY SPACES

050420     perform 0310-delimite-extract thru 0310-exit

           WRITE ELCERT-OUT-REC        FROM ELCERT-DETAIL-RECORD
           ADD 1 TO CRT-RECS-OUT

           .
       0300-EXIT.
           EXIT.

050420 0310-delimite-extract.
050420
050420     MOVE ';'                    TO
050420         EX-TAB1  EX-TAB2  EX-TAB3  EX-TAB4
050420         EX-TAB5  EX-TAB6  EX-TAB7  EX-TAB8
050420         EX-TAB9  EX-TAB10 EX-TAB11 EX-TAB12
050420         EX-TAB13 EX-TAB13A EX-TAB14 EX-TAB15 EX-TAB16
050420         EX-TAB17 EX-TAB18 EX-TAB18A EX-TAB19 EX-TAB20
050420         EX-TAB21 EX-TAB22 EX-TAB23 EX-TAB24
050420         EX-TAB25 EX-TAB26 EX-TAB27 EX-TAB28
050420         EX-TAB29 EX-TAB30 EX-TAB31 EX-TAB32
050420         EX-TAB33 EX-TAB34 EX-TAB35 EX-TAB36
050420         EX-TAB37 EX-TAB38 EX-TAB39 EX-TAB40
050420         EX-TAB41 EX-TAB42 EX-TAB43 EX-TAB44
050420         EX-TAB45 EX-TAB45A EX-TAB46 EX-TAB47 EX-TAB47A EX-TAB48
050420         EX-TAB49 EX-TAB50 EX-TAB51 EX-TAB52
050420         EX-TAB53 EX-TAB54 EX-TAB54A EX-TAB55 EX-TAB56
050420         EX-TAB57 EX-TAB58 EX-TAB59 EX-TAB60
050420         EX-TAB61 EX-TAB62 EX-TAB62A EX-TAB63 EX-TAB64 EX-TAB64A
050420         EX-TAB65 EX-TAB66 EX-TAB67 EX-TAB67A EX-TAB68
050420         EX-TAB69 EX-TAB70 EX-TAB70A EX-TAB70B EX-TAB71 EX-TAB72
050420         EX-TAB73 EX-TAB73A EX-TAB73B EX-TAB73C EX-TAB74 EX-TAB75
               EX-TAB75A EX-TAB76
050420         EX-TAB77 EX-TAB78 EX-TAB79 EX-TAB80
050420
050420     MOVE 'E'                    TO EX-EOR
050420
050420     .
050420 0310-exit.
050420     exit.

072403 0350-READ-ERMAIL.
072403
072403     READ ERMAIL
072403
121020     IF ERMAIL-FILE-STATUS <> '00'
121020        GO TO 0350-EXIT
121020     END-IF

072403     MOVE MA-ADDRESS-LINE-1      TO EX-ADDR1
072403     MOVE MA-ADDRESS-LINE-2      TO EX-ADDR2
072403     MOVE MA-CITY-STATE          TO EX-CITY-ST
072403     MOVE MA-ZIP                 TO EX-ZIP

121020     IF MA-INSURED-BIRTH-DT <> SPACES AND LOW-VALUES
121020        MOVE MA-INSURED-BIRTH-DT TO DC-BIN-DATE-1
121020        MOVE ' '                 TO DC-OPTION-CODE
121020        PERFORM 8510-DATE-CONVERSION
121020                                 THRU 8590-EXIT
121020        IF NO-CONVERSION-ERROR
121020           MOVE DC-GREG-DATE-A-EDIT
121020                                 TO EX-INSURED-BIRTH-DT
121020        END-IF
121020     END-IF
121020     IF MA-JOINT-BIRTH-DT <> SPACES AND LOW-VALUES
121020        MOVE MA-JOINT-BIRTH-DT   TO DC-BIN-DATE-1
121020        MOVE ' '                 TO DC-OPTION-CODE
121020        PERFORM 8510-DATE-CONVERSION
121020                                 THRU 8590-EXIT
121020        IF NO-CONVERSION-ERROR
121020           MOVE DC-GREG-DATE-A-EDIT
121020                                 TO EX-JT-BIRTH-DT
121020        END-IF
121020     END-IF
072403
072403     .
072403 0350-EXIT.
072403     EXIT.

       0400-OPEN-FILES.

072403     OPEN INPUT ELCERT ERMAIL
               OUTPUT ELCERT-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

072403     CLOSE ELCERT ELCERT-OUT ERMAIL

           .
       0500-EXIT.
           EXIT.

       0550-START-ELCERT.

           MOVE LOW-VALUES             TO CM-CONTROL-PRIMARY

           MOVE DTE-CLASIC-COMPANY-CD  TO CM-COMPANY-CD

           START ELCERT KEY IS >= CM-CONTROL-PRIMARY

           IF (ELCERT-FILE-STATUS = '10' OR '23')
              SET END-OF-ELCERT        TO TRUE
           ELSE
              IF ELCERT-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCERT START     ' ELCERT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.


       0600-INITIALIZE.

           MOVE SPACES                 TO ELCERT-DETAIL-RECORD
121020     move zeros                  to EX-INSURED-ISSUE-AGE  
121020                                    EX-INSURED-JOINT-AGE  
121020                                    EX-LF-ORIG-TERM       
121020                                    EX-LF-DEV-PCT         
121020                                    EX-LF-BENEFIT-AMT     
121020                                    EX-LF-PREMIUM-AMT     
121020                                    EX-LF-ALT-BENEFIT-AMT 
121020                                    EX-LF-ALT-PREMIUM-AMT 
121020                                    EX-LF-NSP-PREMIUM-AMT 
121020                                    EX-LF-REMAINING-AMT   
121020                                    EX-LF-ITD-CANCEL-AMT  
121020                                    EX-LF-ITD-DEATH-AMT   
121020                                    EX-LF-PREMIUM-RATE    
121020                                    EX-LF-ALT-PREMIUM-RATE
121020                                    EX-AH-ORIG-TERM       
121020                                    EX-AH-CRITICAL-PERIOD 
121020                                    EX-AH-DEV-PCT         
121020                                    EX-AH-BENEFIT-AMT     
121020                                    EX-AH-PREMIUM-AMT     
121020                                    EX-AH-NSP-PREMIUM-AMT 
121020                                    EX-AH-ITD-CANCEL-AMT  
121020                                    EX-AH-ITD-AH-PMT      
121020                                    EX-AH-PREMIUM-RATE    
121020                                    EX-LOAN-APR           
121020                                    EX-PMT-FREQ           
121020                                    EX-LOAN-TERM          
121020                                    EX-PMT-EXTENSION-DAYS 
121020                                    EX-CLAIM-ATTACHED-CNT 
121020                                    EX-INT-ON-REFS        
121020                                    EX-LIFE-COMM-PCT      
121020                                    EX-AH-COMM-PCT        
121020
121020     move elcert-detail-record   to elcert-detail-init
           PERFORM 0550-START-ELCERT   THRU 0550-EXIT
           PERFORM 0200-READ-ELCERT    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

030404 ABEND-PGM. COPY ELCABEND.