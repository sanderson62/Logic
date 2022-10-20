       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCCGRX1.
       AUTHOR.     PABLO.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EXTRACT-IN       ASSIGN TO EXTRIN.

           SELECT EXTRACT-OUT      ASSIGN TO EXTROT
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ERACCT          ASSIGN TO ERACCTT
                   ORGANIZATION INDEXED
                   ACCESS DYNAMIC
                   RECORD KEY AM-CONTROL-PRIMARY
                   FILE STATUS ERACCT-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  EXTRACT-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSGAP01.

       FD  EXTRACT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  EXTRACT-OUT-REC             PIC X(238).

       FD  ERACCT.

                                       COPY ERCACCT.

       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   DCCGRX1  WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-INPUT                 VALUE 'Y'.
       77  GAP-RECS-IN                 PIC 9(9) VALUE ZEROS.
       77  GAP-RECS-OUT                PIC 9(9) VALUE ZEROS.
       77  SUB1                        PIC S9(5) VALUE +0 COMP-3.
       77  ERACCT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  PGM-SUB                     PIC S999 COMP  VALUE +158.

       01  WS-INIT-EXTRACT             PIC X(900).
       01  EXTRACT-RECORD.
           05  EXT-RECORD-KEY          PIC X(38).
           05  EXT-TABA                PIC X.
           05  EXT-PT-CODE             PIC X.
           05  EXT-TAB1                PIC X.
           05  EXT-COV-CODE            PIC XX.
           05  EXT-TAB2                PIC X.
           05  EXT-LOB                 PIC X(4).
           05  EXT-TAB3                PIC X.
           05  EXT-ACCOUNT             PIC X(10).
           05  EXT-TAB4                PIC X.
           05  EXT-REG-CODE            PIC X.
           05  EXT-TAB5                PIC X.
           05  EXT-CARRIER             PIC X.
           05  EXT-TAB6                PIC X.
           05  EXT-INST-NO             PIC X.
           05  EXT-TAB7                PIC X.
           05  EXT-INST-DT             PIC X.
           05  EXT-TAB8                PIC X.
           05  EXT-PLAN-CODE           PIC X.
           05  EXT-TAB9                PIC X.
           05  EXT-TRANS-ACC-DT        PIC X(10).
           05  EXT-TAB10               PIC X.
           05  EXT-TRANS-EFF-DT        PIC X(10).
           05  EXT-TAB11               PIC X.
           05  EXT-BILL-DT             PIC X.
           05  EXT-TAB12               PIC X.
           05  EXT-AGENT-NO            PIC X(10).
           05  EXT-TAB13               PIC X.
           05  EXT-SUB-AGENT-NO        PIC X.
           05  EXT-TAB14               PIC X.
           05  EXT-POL-EFF-DT          PIC X(10).
           05  EXT-TAB15               PIC X.
           05  EXT-POL-EXP-DT          PIC X(10).
           05  EXT-TAB16               PIC X.
           05  EXT-CASH-REC-AMT        PIC X.
           05  EXT-TAB17               PIC X.
           05  EXT-CASH-TRAN-DT        PIC X.
           05  EXT-TAB18               PIC X.
           05  EXT-PREM-WRIT           PIC 99999999.99.
           05  EXT-TAB19               PIC X.
           05  EXT-CLP-ST              PIC XX.
           05  EXT-TAB20               PIC X.
           05  EXT-ASS-WRIT-AMT        PIC X.
           05  EXT-TAB21               PIC X.
           05  EXT-ASS-REIN-COMP       PIC X.
           05  EXT-TAB22               PIC X.
           05  EXT-CEDED-WRIT-AMT      PIC X.
           05  EXT-TAB23               PIC X.
           05  EXT-CEDED-REIN-COMP     PIC XXX.
           05  EXT-TAB24               PIC X.
           05  EXT-COMM-AMT            PIC 99999999.99.
           05  EXT-TAB25               PIC X.
           05  EXT-UEP-AMT             PIC 99999999.99.
           05  EXT-TAB26               PIC X.
           05  EXT-GROSS-PREM          PIC 99999999.99.
           05  EXT-TAB27               PIC X.
           05  EXT-BASE-COMM           PIC 99999999.99.
           05  EXT-TAB28               PIC X.
           05  EXT-REFUNDS             PIC 99999999.99.
           05  EXT-TAB29               PIC X.
           05  EXT-NSP-PREM            PIC 99999999.99.
           05  EXT-TAB30               PIC X.
           05  EXT-EOR                 PIC X.

       01  WS-ABEND-FIELDS.
           12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.
           12  WS-ZERO                 PIC S9          VALUE ZERO.
           12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.
           12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.

       01  WS-RECORD-KEY.
           05  WS-RC-CARRIER           PIC X.
           05  WS-RC-GROUP             PIC X(6).
           05  WS-RC-STATE             PIC XX.
           05  WS-RC-ACCOUNT           PIC X(10).
           05  WS-RC-EFF-DT            PIC X(8).
           05  WS-RC-CERT-NO           PIC X(11).

       01  WS-MISC.
           05  WS-ERACCT-SW            PIC X VALUE ' '.
               88  ERACCT-FINISHED           VALUE 'Y'.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  WS-DATE-R REDEFINES WS-DATE.
               10  FILLER              PIC 999.
               10  WS-CCYY             PIC 9999.
               10  WS-MM               PIC 99.
               10  WS-DD               PIC 99.

                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.                       

           PERFORM 0400-OPEN-FILES     THRU 0400-EXIT

           PERFORM 0600-INITIALIZE     THRU 0600-EXIT

           PERFORM 0050-PROCESS-INPUT  THRU 0050-EXIT UNTIL
                 (END-OF-INPUT)
PEMTST*          OR (GAP-RECS-IN > 1000)

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' GAAP RECORDS READ    '  GAP-RECS-IN
           DISPLAY ' GAAP RECORDS WRITTEN '  GAP-RECS-OUT
           GOBACK

           .
       0050-PROCESS-INPUT.

           IF GR-REINCO = '300' OR '500' OR '700'
              PERFORM 0100-PROCESS-GAAP
                                       THRU 0100-EXIT
           END-IF
           PERFORM 0200-READ-GAAP      THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-GAAP.

           MOVE WS-INIT-EXTRACT        TO EXTRACT-RECORD

           MOVE GR-CARRIER             TO WS-RC-CARRIER
           MOVE GR-GROUPING            TO WS-RC-GROUP
           MOVE GR-STATE               TO WS-RC-STATE
           MOVE GR-ACCOUNT             TO WS-RC-ACCOUNT
           MOVE GR-EFF                 TO WS-DATE
           MOVE WS-DATE-R (4:8)        TO WS-RC-EFF-DT
           MOVE GR-CERT-NO             TO WS-RC-CERT-NO
           MOVE WS-RECORD-KEY          TO EXT-RECORD-KEY
           
           MOVE 'AH'                   TO EXT-COV-CODE
           MOVE '0171'                 TO EXT-LOB
           MOVE GR-ACCOUNT             TO EXT-ACCOUNT
           MOVE GR-CARRIER             TO EXT-CARRIER
           MOVE '20071231'             TO EXT-TRANS-ACC-DT
           MOVE GR-ACCOUNT             TO EXT-AGENT-NO
           MOVE GR-STATE               TO EXT-CLP-ST

           IF GR-DCC-CLP-STATE NOT = SPACES AND ZEROS AND LOW-VALUES
              MOVE GR-DCC-CLP-STATE    TO EXT-CLP-ST
           END-IF

           COMPUTE EXT-UEP-AMT = GRS-LFPRM + GRS-AHPRM

           IF (GR-CONTROL (1:19) = AM-CONTROL-A)
              AND ((GR-EFF < AM-EXPIRE-DT)
              AND (GR-EFF >= AM-EFFECT-DT))
              CONTINUE
           ELSE
              MOVE DTE-CLASIC-COMPANY-CD
                                       TO AM-COMPANY-CD
              MOVE GR-CONTROL (1:25)   TO AM-CONTROL-PRIMARY (2:25)
              PERFORM 0360-STARTBR-ERACCT
                                       THRU 0360-EXIT
              PERFORM 0370-READNEXT-ERACCT
                                       THRU 0370-EXIT
              PERFORM 0350-MATCH-TO-ERACCT
                                       THRU 0350-EXIT UNTIL
                 ERACCT-FINISHED
           END-IF

           MOVE AM-REI-TABLE           TO EXT-CEDED-REIN-COMP
           MOVE 'E'                    TO EXT-EOR

           PERFORM 0300-WRITE-GAAP     THRU 0300-EXIT

           .
       0100-EXIT.
           EXIT.

       0200-READ-GAAP.

           READ EXTRACT-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO GAP-RECS-IN
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-WRITE-GAAP.

           WRITE EXTRACT-OUT-REC       FROM EXTRACT-RECORD
           ADD 1                       TO GAP-RECS-OUT

           .
       0300-EXIT.
           EXIT.

       0350-MATCH-TO-ERACCT.

           IF (GR-CONTROL (1:19) > AM-CONTROL-A)
              PERFORM 0370-READNEXT-ERACCT
                                       THRU 0370-EXIT
           ELSE
              IF (GR-CONTROL (1:19) = AM-CONTROL-A) AND
                 ((GR-EFF < AM-EXPIRE-DT)   AND
                 (GR-EFF >= AM-EFFECT-DT))
                 SET ERACCT-FINISHED TO TRUE
              ELSE
                 IF GR-CONTROL (1:19) < AM-CONTROL-A
                    MOVE '???'         TO AM-REI-TABLE
                    SET ERACCT-FINISHED TO TRUE
                 ELSE
                    PERFORM 0370-READNEXT-ERACCT
                                       THRU 0370-EXIT
                 END-IF
              END-IF
           END-IF

           .
       0350-EXIT.
           EXIT.

       0360-STARTBR-ERACCT.

           MOVE ' '                    TO WS-ERACCT-SW
           START ERACCT KEY >= AM-CONTROL-PRIMARY

           IF ERACCT-FILE-STATUS = '00'
              CONTINUE
           ELSE
              DISPLAY ' ERROR - ERACCT - STARTBR ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           .
       0360-EXIT.
           EXIT.

       0370-READNEXT-ERACCT.

           READ ERACCT NEXT RECORD

           IF ERACCT-FILE-STATUS = '00'
              IF AM-COMPANY-CD > DTE-CLASIC-COMPANY-CD
                 MOVE HIGH-VALUES      TO AM-CONTROL-PRIMARY
              END-IF
           ELSE
              IF ERACCT-FILE-STATUS = '10' OR '23'
                 MOVE HIGH-VALUES      TO AM-CONTROL-PRIMARY
              ELSE
                 DISPLAY ' ERROR - ERACCT - READNEXT '
                    ERACCT-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

           .
       0370-EXIT.
           EXIT.

       0400-OPEN-FILES.

           OPEN INPUT EXTRACT-IN ERACCT
               OUTPUT EXTRACT-OUT

           .
       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE EXTRACT-IN EXTRACT-OUT ERACCT

           .
       0500-EXIT.
           EXIT.

       0600-INITIALIZE.

           MOVE SPACES                 TO EXTRACT-RECORD
           MOVE ';'                    TO EXT-TAB1
                                          EXT-TABA
                                          EXT-TAB2
                                          EXT-TAB3
                                          EXT-TAB4
                                          EXT-TAB5
                                          EXT-TAB6
                                          EXT-TAB7
                                          EXT-TAB8
                                          EXT-TAB9
                                          EXT-TAB10
                                          EXT-TAB11
                                          EXT-TAB12
                                          EXT-TAB13
                                          EXT-TAB14
                                          EXT-TAB15
                                          EXT-TAB16
                                          EXT-TAB17
                                          EXT-TAB18
                                          EXT-TAB19
                                          EXT-TAB20
                                          EXT-TAB21
                                          EXT-TAB22
                                          EXT-TAB23
                                          EXT-TAB24
                                          EXT-TAB25
                                          EXT-TAB26
                                          EXT-TAB27
                                          EXT-TAB28
                                          EXT-TAB29
                                          EXT-TAB30

           MOVE EXTRACT-RECORD         TO WS-INIT-EXTRACT
           MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD
           PERFORM 0360-STARTBR-ERACCT THRU 0360-EXIT
           PERFORM 0370-READNEXT-ERACCT
                                       THRU 0370-EXIT
           PERFORM 0200-READ-GAAP      THRU 0200-EXIT

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
