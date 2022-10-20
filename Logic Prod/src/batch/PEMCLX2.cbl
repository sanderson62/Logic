       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCLX2.
       AUTHOR.     PABLO.
       DATE-COMPILED.
021704******************************************************************
021704*                   C H A N G E   L O G
021704*
021704* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
021704*-----------------------------------------------------------------
021704*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
021704* EFFECTIVE    NUMBER
021704*-----------------------------------------------------------------
021704******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ERACCT           ASSIGN TO ERACCT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

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

           SELECT ELCERT           ASSIGN TO ELCERT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CM-CONTROL-PRIMARY
                                   FILE STATUS IS ELCERT-FILE-STATUS.

           SELECT ELBENE           ASSIGN TO ELBENE
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS BE-CONTROL-PRIMARY
                                   FILE STATUS IS ELBENE-FILE-STATUS.

           SELECT ELMSTR-OUT       ASSIGN TO ELMSTROT
               ORGANIZATION IS LINE SEQUENTIAL.

021704     SELECT DISK-DATE        ASSIGN TO SYS019.

           EJECT
       DATA DIVISION.
       FILE SECTION.

       FD  ERACCT.

           COPY ERCACCT.

       FD  ELMSTR.

           COPY ELCMSTR.

       FD  ELTRLR.

           COPY ELCTRLR.

       FD  ELCERT.

           COPY ELCCERT.

       FD  ELBENE.

           COPY ELCBENE.

       FD  ELMSTR-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

092204 01  ELMSTR-OUT-REC              PIC X(710).


021704 FD  DISK-DATE
021704     COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMCLX2  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ELMSTR             VALUE 'Y'.
       77  CLM-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  CRT-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  TRL-RECS-IN             PIC 9(9) VALUE ZEROS.
       77  CLM-RECS-OUT            PIC 9(9) VALUE ZEROS.
       77  SUB1                    PIC S9(5) VALUE +0 COMP-3.
       77  WS-DIAGNOSIS            PIC X(60) VALUE SPACES.
       77  WS-LAST-ACT-DT          PIC XX    VALUE LOW-VALUES.
       77  WS-ACTIVITY-DT          PIC XX    VALUE LOW-VALUES.
       77  WS-LAST-ACT-TYPE        PIC X(15) VALUE SPACES.
       77  WS-ACTIVITY-TYPE        PIC X(15) VALUE SPACES.
       77  WS-FORM                 PIC X(4)  VALUE SPACES.
       77  WS-GOT-IT-SW            PIC X     VALUE ' '.
           88  WE-GOT-IT-ALL                 VALUE 'Y'.
       77  WS-AM-NAME                  PIC X(30)  VALUE SPACES.

       01  WS-LOAN-NO                  PIC X(25).
       01  WS-INS-NAME-AND-ADDRESS.
           05  WS-INS-NAME             PIC X(30).
           05  WS-INS-ADDR1            PIC X(30).
           05  WS-INS-ADDR2            PIC X(30).
           05  WS-INS-CITY-ST          PIC X(30).
           05  WS-INS-ZIP              PIC X(9).
           05  WS-INS-PHONE            PIC X(13).
           05  WS-WORK-PHONE           PIC X(11).

       01  WS-BEN-NAME-AND-ADDRESS.
           05  WS-BEN-NAME             PIC X(30).
           05  WS-BEN-ADDR1            PIC X(30).
           05  WS-BEN-ADDR2            PIC X(30).
           05  WS-BEN-CITY-ST          PIC X(30).
           05  WS-BEN-ZIP              PIC X(9).
           05  WS-BEN-PHONE            PIC X(13).

       01  WS-SAVE-ELMSTR              PIC X(710) VALUE LOW-VALUES.
       01  ELMSTR-DETAIL-RECORD.
           12  EX-LETTER-ID            PIC X(4).
           12  EX-TABA                 PIC X.
           12  EX-CARRIER              PIC X.
           12  EX-TAB1                 PIC X.
           12  EX-CLAIM-NO             PIC X(7).
           12  EX-TAB2                 PIC X.
           12  EX-CERT-NO              PIC X(11).
           12  EX-TAB3                 PIC X.
           12  EX-ACCOUNT              PIC X(10).
           12  EX-TAB4                 PIC X.
           12  EX-INSURED-LAST-NAME    PIC X(15).
           12  EX-TAB5                 PIC X.
           12  EX-INSURED-1ST-NAME     PIC X(11).
           12  EX-TAB6                 PIC X.
           12  EX-CLAIM-STATUS         PIC X.
           12  EX-TAB7                 PIC X.
           12  EX-CLAIM-TYPE           PIC X.
           12  EX-TAB8                 PIC X.
           12  EX-FILE-ESTABLISH-DT    PIC X(10).
           12  EX-TAB9                 PIC X.
           12  EX-TOTAL-PAID-AMT       PIC -9(7).99.
           12  EX-TAB10                PIC X.
           12  EX-INSURED-SEX-CD       PIC X.
           12  EX-TAB11                PIC X.
           12  EX-ACCOUNT-ADDR-CNT     PIC 9.
           12  EX-TAB12                PIC X.
           12  EX-CERT-STATE           PIC XX.
           12  EX-TAB13                PIC X.
           12  EX-CERT-ACCOUNT         PIC X(10).
           12  EX-TAB14                PIC X.
           12  EX-CERT-EFF-DT          PIC X(10).
           12  EX-TAB15                PIC X.
           12  EX-INCURRED-DT          PIC X(10).
           12  EX-TAB16                PIC X.
           12  EX-REPORTED-DT          PIC X(10).
           12  EX-TAB17                PIC X.
           12  EX-PAID-THRU-DT         PIC X(10).
           12  EX-TAB18                PIC X.
           12  EX-LAST-PMT-DT          PIC X(10).
           12  EX-TAB19                PIC X.
           12  EX-LAST-MAINT-DT        PIC X(10).
           12  EX-TAB20                PIC X.
           12  EX-LAST-MAINT-USER      PIC X(4).
           12  EX-TAB21                PIC X.
           12  EX-LAST-MAINT-TYPE      PIC X.
           12  EX-TAB22                PIC X.
           12  EX-DIAG                 PIC X(60).
           12  EX-TAB23                PIC X.
           12  EX-EXP-DT               PIC X(10).
           12  EX-TAB24                PIC X.
           12  EX-LAST-CLOSE-REASON    PIC X.
           12  EX-TAB25                PIC X.
092204     12  EX-BIRTH-DT             PIC X(10).
092204     12  EX-TAB26                PIC X.
091207     12  EX-NO-OF-PMTS           PIC ZZ9.
091207     12  EX-TAB27                PIC X.
           12  EX-LAST-ACT-DT          PIC X(10).
           12  EX-TAB28                PIC X.
           12  EX-ACTIVITY-DT          PIC X(10).
           12  EX-TAB29                PIC X.
           12  EX-LAST-ACT-TYPE        PIC X(15).
           12  EX-TAB30                PIC X.
           12  EX-ACTIVITY-TYPE        PIC X(15).
           12  EX-TAB31                PIC X.
           12  EX-FORM                 PIC X(4).
           12  EX-TAB32                PIC X.
           12  EX-ACCT-NAME            PIC X(30).
           12  EX-TAB33                PIC X.
           12  EX-LAST-PAID-AMT        PIC -9(7).99.
           12  EX-TAB34                PIC X.
           12  EX-TERM                 PIC 999.
           12  EX-TAB35                PIC X.
           12  EX-BEN-CODE             PIC XX.
           12  EX-TAB36                PIC X.
           12  EX-INS-NAME             PIC X(30).
           12  EX-TAB37                PIC X.
           12  EX-INS-ADDR1            PIC X(30).
           12  EX-TAB38                PIC X.
           12  EX-INS-ADDR2            PIC X(30).
           12  EX-TAB39                PIC X.
           12  EX-INS-CITY-ST          PIC X(30).
           12  EX-TAB40                PIC X.
           12  EX-INS-ZIP              PIC X(9).
           12  EX-TAB41                PIC X.
           12  EX-INS-PHONE            PIC X(13).
           12  EX-TAB42                PIC X.
           12  EX-BEN-NAME             PIC X(30).
           12  EX-TAB43                PIC X.
           12  EX-BEN-ADDR1            PIC X(30).
           12  EX-TAB44                PIC X.
           12  EX-BEN-ADDR2            PIC X(30).
           12  EX-TAB45                PIC X.
           12  EX-BEN-CITY-ST          PIC X(30).
           12  EX-TAB46                PIC X.
           12  EX-BEN-ZIP              PIC X(9).
           12  EX-TAB47                PIC X.
           12  EX-BEN-PHONE            PIC X(13).
           12  EX-TAB48                PIC X.
           12  EX-LOAN-NO              PIC X(25).
           12  EX-TAB49                PIC X.
           12  EX-LAST-BYTE            PIC X.

      ******************************************************************
       01  WS-MISC.
           05  ERACCT-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELMSTR-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELTRLR-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELCERT-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  ELBENE-FILE-STATUS      PIC XX     VALUE ZEROS.
           05  WS-DATE                 PIC 9(11)  VALUE ZEROS.
021704     05  PGM-SUB          COMP-3 PIC S9(04) VALUE +585.
021704     05  WS-RETURN-CODE   COMP   PIC S9(03) VALUE +0.
021704     05  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.
021704     05  WS-ZERO          COMP-3 PIC S9(01) VALUE +0.
021704     05  WS-ABEND-FILE-STATUS    PIC X(02)  VALUE ZERO.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

                                       COPY ELCDATE.

       PROCEDURE DIVISION.

021704 0000-DATE-CARD-READ. COPY ELCDTERX.

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-FILE   THRU 0050-EXIT UNTIL
                 (END-OF-ELMSTR)
PEMTST*          OR (CLM-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CLAIM RECORDS READ    '  CLM-RECS-IN
           DISPLAY ' TRLR  RECORDS READ    '  TRL-RECS-IN
           DISPLAY ' CLAIM RECORDS WRITTEN '  CLM-RECS-OUT
           GOBACK

           .
       0050-PROCESS-FILE.

           IF CL-INCURRED-DT > X'A4FF'
              PERFORM 0100-PROCESS-ELMSTR THRU 0100-EXIT
           END-IF

           PERFORM 0200-READ-ELMSTR    THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-ELMSTR.

           MOVE WS-SAVE-ELMSTR         TO ELMSTR-DETAIL-RECORD

           MOVE CL-CARRIER             TO EX-CARRIER
           MOVE CL-CLAIM-NO            TO EX-CLAIM-NO
           MOVE CL-CERT-NO             TO EX-CERT-NO
           MOVE CL-CERT-ACCOUNT        TO EX-ACCOUNT
           MOVE CL-INSURED-LAST-NAME   TO EX-INSURED-LAST-NAME
           MOVE CL-INSURED-1ST-NAME    TO EX-INSURED-1ST-NAME
           MOVE CL-CLAIM-STATUS        TO EX-CLAIM-STATUS
           MOVE CL-CLAIM-TYPE          TO EX-CLAIM-TYPE
      *    MOVE CL-FILE-ESTABLISH-DT   TO EX-FILE-ESTABLISH-DT
           MOVE CL-TOTAL-PAID-AMT      TO EX-TOTAL-PAID-AMT
           MOVE CL-NO-OF-PMTS-MADE     TO EX-NO-OF-PMTS
           MOVE CL-LAST-PMT-AMT        TO EX-LAST-PAID-AMT
           MOVE CL-INSURED-SEX-CD      TO EX-INSURED-SEX-CD
           MOVE CL-ACCOUNT-ADDR-CNT    TO EX-ACCOUNT-ADDR-CNT
           MOVE CL-CERT-STATE          TO EX-CERT-STATE
           MOVE CL-CERT-ACCOUNT        TO EX-CERT-ACCOUNT
           MOVE CL-LAST-MAINT-USER     TO EX-LAST-MAINT-USER
           MOVE CL-LAST-MAINT-TYPE     TO EX-LAST-MAINT-TYPE
           MOVE CL-LAST-CLOSE-REASON   TO EX-LAST-CLOSE-REASON

           MOVE CL-FILE-ESTABLISH-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-FILE-ESTABLISH-DT
           END-IF

           MOVE CL-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-CERT-EFF-DT
           END-IF

           MOVE CL-PAID-THRU-DT        TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-PAID-THRU-DT
           END-IF

           MOVE CL-LAST-PMT-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-PMT-DT
           END-IF

           MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-INCURRED-DT
           END-IF

           MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-REPORTED-DT
           END-IF

           MOVE CL-LAST-MAINT-DT       TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-MAINT-DT
           END-IF

092204     MOVE CL-INSURED-BIRTH-DT    TO DC-BIN-DATE-1
092204     MOVE ' '                    TO DC-OPTION-CODE
092204     PERFORM 8510-DATE-CONVERSION
092204                                 THRU 8590-EXIT
092204     IF NO-CONVERSION-ERROR
092204        MOVE DC-GREG-DATE-A-EDIT TO EX-BIRTH-DT
092204     END-IF

           INSPECT WS-DIAGNOSIS
              REPLACING ALL X'00' BY SPACES

           MOVE WS-DIAGNOSIS           TO EX-DIAG

           MOVE WS-LAST-ACT-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-LAST-ACT-DT
           END-IF

           MOVE WS-ACTIVITY-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-A-EDIT TO EX-ACTIVITY-DT
           END-IF

           MOVE WS-LAST-ACT-TYPE       TO EX-LAST-ACT-TYPE
           MOVE WS-ACTIVITY-TYPE       TO EX-ACTIVITY-TYPE
           MOVE WS-FORM                TO EX-FORM
           MOVE WS-AM-NAME             TO EX-ACCT-NAME
           IF WS-LOAN-NO NOT = SPACES
              MOVE WS-LOAN-NO      TO EX-LOAN-NO
           END-IF

           IF WS-INS-NAME-AND-ADDRESS NOT = SPACES
              MOVE WS-INS-NAME         TO EX-INS-NAME
              MOVE WS-INS-ADDR1        TO EX-INS-ADDR1
              MOVE WS-INS-ADDR2        TO EX-INS-ADDR2
              MOVE WS-INS-CITY-ST      TO EX-INS-CITY-ST
              MOVE WS-INS-ZIP          TO EX-INS-ZIP
              MOVE WS-INS-PHONE        TO EX-INS-PHONE
           END-IF

           IF WS-BEN-NAME-AND-ADDRESS NOT = SPACES
              MOVE WS-BEN-NAME         TO EX-BEN-NAME
              MOVE WS-BEN-ADDR1        TO EX-BEN-ADDR1
              MOVE WS-BEN-ADDR2        TO EX-BEN-ADDR2
              MOVE WS-BEN-CITY-ST      TO EX-BEN-CITY-ST
              MOVE WS-BEN-ZIP          TO EX-BEN-ZIP
              MOVE WS-BEN-PHONE        TO EX-BEN-PHONE
           ELSE
              IF CL-BENEFICIARY NOT = SPACES
                 MOVE CL-COMPANY-CD    TO BE-COMPANY-CD
                 MOVE 'B'              TO BE-RECORD-TYPE
                 MOVE CL-BENEFICIARY   TO BE-BENEFICIARY
                 READ ELBENE
                 IF ELBENE-FILE-STATUS = '00'
                    MOVE BE-MAIL-TO-NAME TO EX-BEN-NAME
                    MOVE BE-ADDRESS-LINE-1 TO EX-BEN-ADDR1
                    MOVE BE-ADDRESS-LINE-2 TO EX-BEN-ADDR2
                    MOVE BE-CITY-STATE TO EX-BEN-CITY-ST
                    MOVE BE-ZIP-CODE   TO EX-BEN-ZIP
                    MOVE BE-PHONE-NO   TO EX-BEN-PHONE
                 ELSE
                    DISPLAY ' ELBENE NOT FOUND ' CL-BENEFICIARY
                    ' ' ELBENE-FILE-STATUS
                 END-IF
              END-IF
           END-IF

      *    IF ELTRLR-FILE-STATUS = '00'
      *       INSPECT AT-INFO-LINE-1
      *          REPLACING ALL X'00' BY SPACES
      *       MOVE AT-INFO-LINE-1      TO EX-DIAG
      *    ELSE
      *       MOVE SPACES              TO EX-DIAG
      *    END-IF

           IF ELCERT-FILE-STATUS = '00'
              IF CL-CLAIM-TYPE = 'L'
                 MOVE CM-LF-LOAN-EXPIRE-DT
                                       TO DC-BIN-DATE-1
                 MOVE CM-LF-ORIG-TERM  TO EX-TERM
                 MOVE CM-LF-BENEFIT-CD TO EX-BEN-CODE
              ELSE
                 MOVE CM-AH-LOAN-EXPIRE-DT 
                                       TO DC-BIN-DATE-1
                 MOVE CM-AH-ORIG-TERM  TO EX-TERM
                 MOVE CM-AH-BENEFIT-CD TO EX-BEN-CODE
              END-IF
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 8510-DATE-CONVERSION
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EX-EXP-DT
              END-IF
           END-IF

           PERFORM 0300-WRITE-MSTR     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-ELMSTR.

           READ ELMSTR NEXT RECORD

           IF (ELMSTR-FILE-STATUS = '10' OR '23')
              OR (CL-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD)
              SET END-OF-ELMSTR        TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELMSTR READ NEXT ' ELMSTR-FILE-STATUS
                 SET END-OF-ELMSTR     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ELMSTR
              ADD 1 TO CLM-RECS-IN
              PERFORM 0210-GET-ELTRLR  THRU 0210-EXIT
      *       PERFORM 0250-READ-ELTRLR THRU 0250-EXIT
              PERFORM 0275-READ-ELCERT THRU 0275-EXIT
           END-IF

           .
       0200-EXIT.
           EXIT.

       0210-GET-ELTRLR.

           MOVE SPACES                 TO WS-DIAGNOSIS
                                          WS-LAST-ACT-TYPE
                                          WS-ACTIVITY-TYPE
                                          WS-FORM
                                          WS-GOT-IT-SW
                                          WS-LOAN-NO
           MOVE LOW-VALUES             TO WS-LAST-ACT-DT
                                          WS-ACTIVITY-DT

           PERFORM 0215-GET-INS-ADDR-TRLR
                                       THRU 0215-EXIT

           PERFORM 0216-GET-BEN-ADDR-TRLR
                                       THRU 0216-EXIT

           PERFORM 0220-START-ELTRLR   THRU 0220-EXIT
           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              PERFORM 0230-READ-ELTRLR THRU 0230-EXIT UNTIL
                 WE-GOT-IT-ALL
           END-IF

           .
       0210-EXIT.
           EXIT.

       0215-GET-INS-ADDR-TRLR.

           MOVE SPACES                 TO WS-INS-NAME-AND-ADDRESS
           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +1                     TO AT-SEQUENCE-NO

           READ ELTRLR

           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              IF AT-SEQUENCE-NO = +1
                 MOVE AT-MAIL-TO-NAME  TO WS-INS-NAME
                 MOVE AT-ADDRESS-LINE-1
                                       TO WS-INS-ADDR1
                 MOVE AT-ADDRESS-LINE-2
                                       TO WS-INS-ADDR2
                 MOVE AT-CITY-STATE    TO WS-INS-CITY-ST
                 MOVE AT-ZIP           TO WS-INS-ZIP
                 IF AT-PHONE-NO NOT = ZEROS
                    MOVE AT-PHONE-NO   TO WS-WORK-PHONE
                    STRING '(' WS-WORK-PHONE (2:3) ')'
                       WS-WORK-PHONE (5:3) '-' WS-WORK-PHONE (8:4)
                       DELIMITED BY SIZE INTO WS-INS-PHONE
                    END-STRING
                 END-IF
              END-IF
           END-IF

           .
       0215-EXIT.
           EXIT.

       0216-GET-BEN-ADDR-TRLR.

           MOVE SPACES                 TO WS-BEN-NAME-AND-ADDRESS
           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +11                    TO AT-SEQUENCE-NO

           READ ELTRLR

           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              IF AT-SEQUENCE-NO = +11
                 MOVE AT-MAIL-TO-NAME  TO WS-BEN-NAME
                 MOVE AT-ADDRESS-LINE-1
                                       TO WS-BEN-ADDR1
                 MOVE AT-ADDRESS-LINE-2
                                       TO WS-BEN-ADDR2
                 MOVE AT-CITY-STATE    TO WS-BEN-CITY-ST
                 MOVE AT-ZIP           TO WS-BEN-ZIP
                 IF AT-PHONE-NO NOT = ZEROS
                    MOVE AT-PHONE-NO   TO WS-WORK-PHONE
                    STRING '(' WS-WORK-PHONE (2:3) ')'
                       WS-WORK-PHONE (5:3) '-' WS-WORK-PHONE (8:4)
                       DELIMITED BY SIZE INTO WS-BEN-PHONE
                    END-STRING
                 END-IF
              END-IF
           END-IF

           .
       0216-EXIT.
           EXIT.

       0220-START-ELTRLR.

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +90                    TO AT-SEQUENCE-NO
           START ELTRLR KEY >= AT-CONTROL-PRIMARY
           IF ELTRLR-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ELTRLR - START ' ELTRLR-FILE-STATUS
                 ' ' CL-CONTROL-PRIMARY (2:19)
           END-IF

           .
       0220-EXIT.
           EXIT.

           
       0230-READ-ELTRLR.

           READ ELTRLR NEXT RECORD

           IF (ELTRLR-FILE-STATUS = '00')
              AND (CL-CONTROL-PRIMARY = AT-CONTROL-PRIMARY (1:20))
              IF AT-SEQUENCE-NO = +90
                 MOVE AT-INFO-LINE-1   TO WS-DIAGNOSIS
              ELSE
                 EVALUATE TRUE
                    WHEN AT-SEQUENCE-NO = +91
                       MOVE AT-INFO-LINE-1 TO WS-LOAN-NO
                    WHEN AT-TRAILER-TYPE = '2'
                       IF AT-CHECK-WRITTEN-DT > WS-LAST-ACT-DT
                          MOVE 'PAYMT' TO WS-LAST-ACT-TYPE
                          MOVE AT-CHECK-WRITTEN-DT
                                       TO WS-LAST-ACT-DT
                       END-IF
                       IF AT-PAID-THRU-DT > WS-ACTIVITY-DT
                          MOVE AT-PAID-THRU-DT
                                       TO WS-ACTIVITY-DT
                          IF FINAL-PAYMENT
                             MOVE '*FINAL*'
                                       TO WS-ACTIVITY-TYPE
                          END-IF
                          IF PARTIAL-PAYMENT
                             MOVE 'PARTIAL'
                                       TO WS-ACTIVITY-TYPE
                          END-IF
                          IF LUMP-SUM-PAYMENT
                             MOVE 'LUMP-S*'
                                       TO WS-ACTIVITY-TYPE
                          END-IF
                          IF (ONLINE-AUTO-PMT)
                             AND (PARTIAL-PAYMENT)
                             MOVE 'AUTO-P'
                                       TO WS-ACTIVITY-TYPE
                          END-IF
                          IF (ONLINE-AUTO-PMT)
                             AND (FINAL-PAYMENT)
                             MOVE 'AUTO-F'
                                       TO WS-ACTIVITY-TYPE
                          END-IF
                          IF ADDITIONAL-PAYMENT
                             MOVE 'ADDITIONAL'
                                       TO WS-ACTIVITY-TYPE
                          END-IF
                          IF CHARGEABLE-EXPENSE
                             MOVE 'EXPENSE'
                                       TO WS-ACTIVITY-TYPE
                          END-IF
                       END-IF
                    WHEN AT-TRAILER-TYPE = '4'
                       IF AT-RECORDED-DT > WS-LAST-ACT-DT
                          MOVE AT-RECORDED-DT
                                       TO WS-LAST-ACT-DT
                          MOVE 'LETTER'
                                       TO WS-LAST-ACT-TYPE
                          MOVE AT-STD-LETTER-FORM
                                       TO WS-FORM
                       END-IF
                       IF AT-LETTER-SENT-DT > WS-ACTIVITY-DT
                          MOVE AT-LETTER-SENT-DT
                                       TO WS-ACTIVITY-DT
                          MOVE 'SENT'  TO WS-ACTIVITY-TYPE
                       END-IF
                       IF AT-RESEND-PRINT-DATE > WS-ACTIVITY-DT
                          MOVE AT-RESEND-PRINT-DATE
                                       TO WS-ACTIVITY-DT
                          MOVE 'RESENT'
                                       TO WS-ACTIVITY-TYPE
                       END-IF
                       IF AT-LETTER-ANSWERED-DT > WS-ACTIVITY-DT
                          MOVE AT-LETTER-ANSWERED-DT
                                       TO WS-ACTIVITY-DT
                          MOVE 'RECVD' TO WS-ACTIVITY-TYPE
                       END-IF
                    WHEN AT-TRAILER-TYPE = '8'
                       IF AT-RECORDED-DT > WS-LAST-ACT-DT
                          MOVE AT-RECORDED-DT
                                       TO WS-LAST-ACT-DT
                          MOVE 'DENIAL'
                                       TO WS-LAST-ACT-TYPE
                       END-IF
                    WHEN AT-TRAILER-TYPE = 'A'
                       IF AT-RECORDED-DT > WS-LAST-ACT-DT
                          MOVE AT-RECORDED-DT
                                       TO WS-LAST-ACT-DT
                          MOVE 'FORMS' TO WS-LAST-ACT-TYPE
                          IF INITIAL-FORM
                             MOVE 'INIT'
                                       TO WS-FORM
                          END-IF
                          IF PROGRESS-FORM
                             MOVE 'PROG'
                                       TO WS-FORM
                          END-IF
                       END-IF
                       IF AT-FORM-SEND-ON-DT > WS-ACTIVITY-DT
                          MOVE AT-FORM-SEND-ON-DT
                                       TO WS-ACTIVITY-DT
                       END-IF
                       IF AT-FORM-REPRINT-DT > WS-ACTIVITY-DT
                          MOVE AT-FORM-REPRINT-DT
                                       TO WS-ACTIVITY-DT
                       END-IF
                       IF AT-FORM-ANSWERED-DT > WS-ACTIVITY-DT
                          MOVE AT-FORM-ANSWERED-DT
                                       TO WS-ACTIVITY-DT
                       END-IF
                 END-EVALUATE
              END-IF
           ELSE
              SET WE-GOT-IT-ALL        TO TRUE
           END-IF

           IF WS-LAST-ACT-DT NOT = LOW-VALUES
              SET WE-GOT-IT-ALL        TO TRUE
           END-IF

           .
       0230-EXIT.
           EXIT.

       0250-READ-ELTRLR.

           MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY
           MOVE +90                    TO AT-SEQUENCE-NO
           
           READ ELTRLR

           IF ELTRLR-FILE-STATUS = '10' OR '23'
              CONTINUE
           ELSE
              IF ELTRLR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELTRLR READ      ' ELTRLR-FILE-STATUS
              ELSE
                 ADD 1 TO TRL-RECS-IN   
              END-IF
           END-IF

           .
       0250-EXIT.
           EXIT.

       0275-READ-ELCERT.

           MOVE CL-CERT-KEY-DATA       TO CM-CONTROL-PRIMARY (2:21)
021704     MOVE DTE-CLASIC-COMPANY-CD  TO CM-COMPANY-CD
           MOVE CL-CERT-NO             TO CM-CERT-NO
           
           READ ELCERT

           IF ELCERT-FILE-STATUS = '10' OR '23'
              CONTINUE
           ELSE
              IF ELCERT-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCERT READ      ' ELCERT-FILE-STATUS
              ELSE
                 PERFORM 0280-GET-ERACCT
                                       THRU 0280-EXIT
                 ADD 1 TO CRT-RECS-IN   
              END-IF
           END-IF

           .
       0275-EXIT.
           EXIT.

       0280-GET-ERACCT.

           MOVE SPACES                 TO WS-AM-NAME
           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE CM-CONTROL-PRIMARY (1:20)
                                       TO AM-CONTROL-PRIMARY (1:20)
           MOVE CM-CERT-EFF-DT         TO AM-EXPIRATION-DT
           START ERACCT KEY >= AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS = '00'
              READ ERACCT NEXT RECORD
              IF ERACCT-FILE-STATUS = '00'
                 IF CM-CONTROL-PRIMARY (1:20) =
                    AM-CONTROL-PRIMARY (1:20)
                    MOVE AM-NAME       TO WS-AM-NAME
                 END-IF
              END-IF
           END-IF

           .
       0280-EXIT.
           EXIT.

       0300-WRITE-MSTR.

           INSPECT ELMSTR-DETAIL-RECORD REPLACING ALL ';' BY ' '
           INSPECT ELMSTR-DETAIL-RECORD REPLACING ALL X'A2' BY ';'
           MOVE 'DE50'                 TO EX-LETTER-ID
           WRITE ELMSTR-OUT-REC        FROM ELMSTR-DETAIL-RECORD
           ADD 1 TO CLM-RECS-OUT
           MOVE 'DEPR'                 TO EX-LETTER-ID
           WRITE ELMSTR-OUT-REC        FROM ELMSTR-DETAIL-RECORD
           ADD 1 TO CLM-RECS-OUT
           MOVE 'DER1'                 TO EX-LETTER-ID
           WRITE ELMSTR-OUT-REC        FROM ELMSTR-DETAIL-RECORD
           ADD 1 TO CLM-RECS-OUT
           MOVE 'DETH'                 TO EX-LETTER-ID
           WRITE ELMSTR-OUT-REC        FROM ELMSTR-DETAIL-RECORD
           ADD 1 TO CLM-RECS-OUT
           MOVE 'PRRU'                 TO EX-LETTER-ID
           WRITE ELMSTR-OUT-REC        FROM ELMSTR-DETAIL-RECORD
           ADD 1 TO CLM-RECS-OUT
           MOVE 'CICM'                 TO EX-LETTER-ID
           WRITE ELMSTR-OUT-REC        FROM ELMSTR-DETAIL-RECORD
           ADD 1 TO CLM-RECS-OUT

           .
       0300-EXIT.
           EXIT.


       0400-OPEN-FILES.

           OPEN INPUT ELMSTR ELTRLR ELCERT ERACCT ELBENE
               OUTPUT ELMSTR-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ELMSTR ELMSTR-OUT ELTRLR ELCERT ERACCT ELBENE

           .
       0500-EXIT.
           EXIT.

       0550-START-ELMSTR.

           MOVE LOW-VALUES             TO CL-CONTROL-PRIMARY
021704     MOVE DTE-CLASIC-COMPANY-CD  TO CL-COMPANY-CD

           START ELMSTR KEY IS NOT < CL-CONTROL-PRIMARY

           IF ELMSTR-FILE-STATUS = '10' OR '23'
              SET END-OF-ELMSTR        TO TRUE
           ELSE
              IF ELMSTR-FILE-STATUS NOT = '00'
                 DISPLAY 'ELMSTR START     ' ELMSTR-FILE-STATUS
                 SET END-OF-ELMSTR     TO TRUE
              END-IF
           END-IF

           .
       0550-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO ELMSTR-DETAIL-RECORD
052704     MOVE X'A2'                  TO EX-TABA
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
                                          EX-TAB27
                                          EX-TAB28
                                          EX-TAB29
                                          EX-TAB30
                                          EX-TAB31
                                          EX-TAB32
                                          EX-TAB33
                                          EX-TAB34
                                          EX-TAB35
                                          EX-TAB36
                                          EX-TAB37
                                          EX-TAB38
                                          EX-TAB39
                                          EX-TAB40
                                          EX-TAB41
                                          EX-TAB42
                                          EX-TAB43
                                          EX-TAB44
                                          EX-TAB45
                                          EX-TAB46
                                          EX-TAB47
                                          EX-TAB48
                                          EX-TAB49
           MOVE '*'                    TO EX-LAST-BYTE

           MOVE ELMSTR-DETAIL-RECORD   TO WS-SAVE-ELMSTR
           PERFORM 0550-START-ELMSTR   THRU 0550-EXIT
           PERFORM 0200-READ-ELMSTR    THRU 0200-EXIT

           .
       0600-EXIT.
           EXIT.

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
