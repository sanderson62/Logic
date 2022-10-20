       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEMCRREF.
       AUTHOR.     PABLO
       DATE-COMPILED.
      *REMARKS
      *  PROGRAM READS THE CERT FILE AND PROCESSES ALL THE
      *  MN CERTS THAT WERE CANCELLED IN 05,06,07 AND 08
      *  THEN RECOMPUTES THE REFUNDS. MEAN - A&H  ROA - LIFE 
      *  AND CREATES A NEW CERT FILE PLACING THE NEW REFUND IN
      *  THE CALC FIELD
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  CERT-IN       ASSIGN TO SYS010.

           SELECT ELCNTL         ASSIGN TO ELCNTL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CF-CONTROL-PRIMARY
                                   FILE STATUS IS ELCNTL-FILE-STATUS.

           SELECT ERACCT         ASSIGN TO ERACCT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS AM-CONTROL-PRIMARY
                                   FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT DISK-DATE      ASSIGN TO SYS019.
           SELECT CERT-OUT       ASSIGN TO SYS011.

       DATA DIVISION.
       FILE SECTION.

       FD  ELCNTL.
                                       COPY ELCCNTL.

       FD  ERACCT.
                                       COPY ERCACCT.

       FD  CERT-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

                                       COPY ECSCRT01.

       FD  CERT-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  CERT-OUT-RECORD             PIC X(1056).

       FD  DISK-DATE                                                    
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   PEMCRREF WORKING-STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

00197  77  WS-ZERO                PIC S9        COMP-3   VALUE ZERO.    EL517
00198  77  WS-RETURN-CODE         PIC S9(4)     COMP     VALUE ZERO.    EL517
       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-INPUT              VALUE 'Y'.
       77  SUB3                        PIC S999 COMP-3 VALUE +0.
00199  77  WS-ABEND-MESSAGE            PIC X(80) VALUE SPACES.          EL517
00200  77  WS-ABEND-FILE-STATUS        PIC XX    VALUE ZERO.            EL517
       77  CERT-RECS-IN            PIC 9(9) VALUE ZEROS.
       77  CERT-RECS-SEL           PIC 9(9) VALUE ZEROS.
       77  CERT-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  WS-AH-R78-REF               PIC 9(7)V99 VALUE ZEROS.
       77  WS-AH-PRO-REF               PIC 9(7)V99 VALUE ZEROS.
       77  WS-AH-MEAN-REF              PIC 9(7)V99 VALUE ZEROS.
       77  WS-AH-R78-FACT              PIC 9V9(7)  VALUE ZEROS.
       77  WS-AH-PRO-FACT              PIC 9V9(7)  VALUE ZEROS.
       01  TEXAS-REGS-WORK-AREAS.
           12  TEX-FACT-1              PIC S9(7)V9(2)    COMP-3.
           12  TEX-FACT-2              PIC S9(3)   COMP-3.
           12  TEX-FACT-3              PIC S9(3)   COMP-3.
           12  TEX-FACT-4              PIC S9(7)   COMP-3.
           12  TEX-FACT-5              PIC S9(3)   COMP-3.
           12  TEX-FACT-6              PIC S9(3)   COMP-3.
           12  TEX-FACT-7              PIC S9(7)   COMP-3.
           12  TEX-FACT-8              PIC S9V9(6) COMP-3.
           12  TEX-FACT-9              PIC S9(4)V9(11)   COMP-3.

       01  NET-PAY-INTERFACE.
           05  NP-APR                  PIC S9(3)V9(4)    COMP-3.
           05  NP-ORIG                 PIC S9(3)         COMP-3.
           05  NP-REM                  PIC S9(3)         COMP-3.
           05  NP-OPT                  PIC X(01).
           05  NP-CAP                  PIC S9(3)         COMP-3.
           05  NP-FACTOR               PIC S9(4)V9(9)    COMP-3.
           05  NP-WORK1                PIC S9(6)V9(9)    COMP-3.
           05  NP-WORK2                PIC S9(6)V9(9)    COMP-3.

       01  WS-MISC-AREA.
00584      12  PGM-SUB         PIC S999 COMP  VALUE +158.               
082103     12  LF-BAL-REMTERM1         PIC S999V99  COMP-3  VALUE +0.
082103     12  LF-BAL-REMTERM2         PIC S999V99  COMP-3  VALUE +0.
           12  LF-REM-TRM1             PIC S999V99  COMP-3  VALUE +0.
           12  AH-REM-TRM1             PIC S999V99  COMP-3  VALUE +0.
           12  LF-REM-TRM2             PIC S999V99  COMP-3  VALUE +0.
           12  AH-REM-TRM2             PIC S999V99  COMP-3  VALUE +0.
           12  REM-TRM1                PIC S9(3)V99    COMP-3.
           12  REM-TRM2                PIC S9(3)V99    COMP-3.

       01  MISC-WS.
           12  WS-LF-RFND              PIC S9(7)V99 VALUE +0 COMP-3.
           12  WS-AH-RFND              PIC S9(7)V99 VALUE +0 COMP-3.
           12  INTERMED                PIC S9(9)V9(6)  COMP-3.
           12  WS-REM-AMT              PIC S9(11)V99 COMP-3 VALUE +0.
           12  WS-LF-REFUND            PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-DATE-ALPH.
               16  FILLER             PIC XXX VALUE '000'.
               16  WS-WORK-CENT       PIC XX.
               16  WS-WORK-YR         PIC XX.
               16  WS-WORK-MO         PIC XX.
               16  WS-WORK-DA         PIC XX.
           12  WS-DATE-NUM REDEFINES WS-DATE-ALPH
                                      PIC 9(11).
           12  WS-VAL-BIN-DATE          PIC XX      VALUE LOW-VALUES.
           12  WS-VAL-DATE              PIC X(8).
           12  WS-VAL-DATE-NUM REDEFINES WS-VAL-DATE PIC 9(8).
           12  WS-BIN-CR-DT             PIC XX  VALUE LOW-VALUES.
           12  ELCNTL-KEY.
               16  CNTL-COMP-ID         PIC XXX     VALUE SPACES.
               16  CNTL-REC-TYPE        PIC X       VALUE SPACE.
               16  CNTL-ACCESS          PIC X(4)    VALUE SPACES.
               16  CNTL-SEQ-NO          PIC S9(4)    COMP VALUE +0.

           12  ERACCT-KEY.
               16  ACCT-COMP-CD         PIC X     VALUE LOW-VALUES.
               16  ACCT-CARRIER         PIC X     VALUE SPACES.
               16  ACCT-GROUPING        PIC X(06) VALUE SPACES.
               16  ACCT-STATE           PIC X(02) VALUE SPACES.
               16  ACCT-ACCOUNT         PIC X(10) VALUE SPACES.
               16  ACCT-EXP-DT          PIC X(02) VALUE LOW-VALUES.

           12  WS-LF-SPECIAL-CALC-CD   PIC X   VALUE SPACES.
           12  WS-LF-EARNINGS-CALC     PIC X   VALUE SPACES.
           12  WS-LF-REFUND-CALC       PIC X   VALUE SPACES.
           12  WS-LF-COVERAGE-TYPE     PIC X   VALUE SPACES.
           12  WS-LF-REM-TERM          PIC X   VALUE SPACES.
           12  WS-AH-SPECIAL-CALC-CD   PIC X   VALUE SPACES.
           12  WS-AH-EARNINGS-CALC     PIC X   VALUE SPACES.
           12  WS-AH-REFUND-CALC       PIC X   VALUE SPACES.
           12  WS-AH-COVERAGE-TYPE     PIC X   VALUE SPACES.
           12  WS-AH-REM-TERM          PIC X   VALUE SPACES.
           12  ERACCT-FILE-STATUS      PIC XX  VALUE LOW-VALUES.
           12  ELCNTL-FILE-STATUS      PIC XX  VALUE LOW-VALUES.
           12  WS-CRT-KEY.
               16  WS-CRT-STATE        PIC XX.   
               16  WS-CRT-EFF-DT       PIC 9(11).
               16  WS-CRT-CRT-NO       PIC X(11).
           12  WS-BAL-KEY.
               16  WS-BAL-STATE        PIC XX.   
               16  WS-BAL-EFF-DT       PIC 9(11).
               16  WS-BAL-CRT-NO       PIC X(11).
           12  WS-WORK-DATE            PIC 9(11).
           12  FILLER REDEFINES WS-WORK-DATE.
               16  FILLER              PIC XXX.
               16  WS-WORK-CCYY        PIC X(4).
               16  WS-WORK-MM          PIC XX.
               16  WS-WORK-DD          PIC XX.

                                       COPY ELCCALC.

                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.

       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           PERFORM 0000-OPEN-FILES     THRU 0000-EXIT

           PERFORM 0010-INITIALIZE     THRU 0010-EXIT

           PERFORM 0050-PROCESS-CERT   THRU 0050-EXIT UNTIL

                 END-OF-INPUT

           PERFORM 0500-CLOSE-FILES    THRU 0500-EXIT

           DISPLAY ' CERT RECORDS READ     ' CERT-RECS-IN
           DISPLAY ' CERT RECORDS WRITTEN  ' CERT-RECS-OUT
           DISPLAY ' CERT RECORDS SELECTED ' CERT-RECS-SEL

           GOBACK

           .
       0000-OPEN-FILES.

           MOVE 'R' TO CP-RATE-FILE
           MOVE 'O' TO CP-IO-FUNCTION
           CALL 'ELRATEX' USING CALCULATION-PASS-AREA

           IF IO-ERROR
              MOVE +0302   TO WS-RETURN-CODE
              MOVE ' ERROR ON ERRATE OPEN ' TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF


           OPEN INPUT CERT-IN ELCNTL ERACCT
               OUTPUT CERT-OUT
           IF ELCNTL-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ELCNTL - OPEN ' ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERROR - ERACCT - OPEN ' ERACCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF
                                                                        
           .
       0000-EXIT.
           EXIT.

       0010-INITIALIZE.

           PERFORM 0200-READ-CERT      THRU 0200-EXIT

           .
       0010-EXIT.
           EXIT.

       0050-PROCESS-CERT.

           IF CR-STATE = 'MN'
              IF ((CR-LFRFND > 0)
                 AND (CR-LF-CANCEL-EXIT-DATE > 20041231)
                 AND (CR-LF-CANCEL-EXIT-DATE < 20090101))
                               OR
                 ((CR-AHRFND > 0)
                 AND (CR-AH-CANCEL-EXIT-DATE > 20041231)
                 AND (CR-AH-CANCEL-EXIT-DATE < 20090101))
                 PERFORM 0100-PROCESS-CERT
                                       THRU 0100-EXIT
                 PERFORM 3500-WRITE-CERT
                                       THRU 3500-EXIT
              END-IF
           END-IF

           PERFORM 0200-READ-CERT      THRU 0200-EXIT

           .
       0050-EXIT.
           EXIT.

       0100-PROCESS-CERT.

           ADD 1 TO CERT-RECS-SEL
           IF CR-LFTYP NOT = '  ' AND '00'
              PERFORM 0276-DO-LIFE     THRU 0276-EXIT
           END-IF

           IF CR-AHTYP NOT = '  ' AND '00'
              PERFORM 0277-DO-AH       THRU 0277-EXIT
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-READ-CERT.

           READ CERT-IN AT END
              SET END-OF-INPUT         TO TRUE
           END-READ

           IF NOT END-OF-INPUT
              ADD 1                    TO CERT-RECS-IN
      *       IF CERT-RECS-SEL > 50
      *          SET END-OF-INPUT      TO TRUE
      *       END-IF
           END-IF

           .
       0200-EXIT.
           EXIT.

       0276-DO-LIFE.

           PERFORM VARYING CLAS-INDEXL FROM CLAS-STARTL BY +1 UNTIL
              (CLAS-INDEXL > CLAS-MAXL)
              OR (CR-LFTYP = CLAS-I-BEN (CLAS-INDEXL))
           END-PERFORM

           IF CLAS-INDEXL > CLAS-MAXL
              DISPLAY ' ERROR - LF TYP NOT FOUND ' CR-LFTYP ' '
                 CR-CARRIER ' ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
              PERFORM ABEND-PGM
           END-IF

           MOVE CLAS-I-EP (CLAS-INDEXL)
                                       TO WS-LF-EARNINGS-CALC
                                          WS-LF-REFUND-CALC
           MOVE CLAS-I-RL-AH (CLAS-INDEXL)
                                       TO WS-LF-COVERAGE-TYPE
           MOVE CLAS-I-BAL (CLAS-INDEXL)
                                       TO WS-LF-SPECIAL-CALC-CD
           IF CLAS-I-REFUND-METHOD (CLAS-INDEXL) > '0'
              MOVE CLAS-I-REFUND-METHOD (CLAS-INDEXL)
                                       TO WS-LF-REFUND-CALC
           END-IF
           MOVE CLAS-CO-REM-TERM-CALC (CLAS-INDEXL)
                                       TO WS-LF-REM-TERM

           IF WS-LF-EARNINGS-CALC NOT = 'N' AND '5' AND '2'
              IF CERT-RECS-SEL < 500
                 DISPLAY ' NOT NET PAY AND LEVEL '
                    WS-LF-EARNINGS-CALC
              END-IF
           END-IF

           PERFORM 0300-FIND-STATE     THRU 0300-EXIT

           IF CF-ST-RT-CALC > '0'
              MOVE CF-ST-RT-CALC       TO WS-LF-REM-TERM
           END-IF

           IF WS-LF-COVERAGE-TYPE = 'R'
              IF CF-ST-RF-LR-CALC > '0'
                 MOVE CF-ST-RF-LR-CALC TO WS-LF-REFUND-CALC
              END-IF
              IF WS-LF-EARNINGS-CALC = 'N' OR '5'
                 IF CF-ST-RF-LN-CALC > '0'
                    MOVE CF-ST-RF-LN-CALC
                                       TO WS-LF-REFUND-CALC
                 END-IF
              END-IF
           ELSE
              IF CF-ST-RF-LL-CALC > '0'
                 MOVE CF-ST-RF-LL-CALC TO WS-LF-REFUND-CALC
              END-IF
           END-IF

           PERFORM VARYING SUB3 FROM 1 BY 1 UNTIL
              (SUB3 > 50) OR
              ((CF-ST-BENEFIT-CD (SUB3) = CR-LFTYP) AND
               (CF-ST-BENEFIT-KIND (SUB3) = 'L'))
           END-PERFORM

           IF SUB3 > 50
              CONTINUE
           ELSE
              IF CF-ST-REFUND-CALC  (SUB3) > '0'
                 MOVE CF-ST-REFUND-CALC (SUB3)   TO WS-LF-REFUND-CALC
              END-IF
              IF CF-ST-REM-TERM-CALC (SUB3) > '0'
                 MOVE CF-ST-REM-TERM-CALC (SUB3) TO WS-LF-REM-TERM
              END-IF
PEMTMP        IF CR-STATE = 'OR'
                 DISPLAY 'REF AFTER STATEBEN- ' WS-LF-REFUND-CALC
              END-IF
           END-IF

           PERFORM 0400-GET-ERACCT     THRU 0400-EXIT

           PERFORM 3720-GET-LF-REFUND  THRU 3720-EXIT
           
      *    DISPLAY ' RETURN CODE ' CP-RETURN-CODE

           MOVE CP-CALC-REFUND         TO CR-LFRFND-CALC

           IF CERT-RECS-SEL < 500
              DISPLAY CR-ACCOUNT ' ' CR-CERT-NO ' '
                ' LIFE ORIG ' CR-LFRFND ' RECALC ' CR-LFRFND-CALC
              ' RTRM ' LF-REM-TRM1 ' PRM ' CR-LFPRM ' TRM ' CR-LF-TERM
           END-IF



           .
       0276-EXIT.
           EXIT.

       0277-DO-AH.

           PERFORM VARYING CLAS-INDEXA FROM CLAS-STARTA BY +1 UNTIL
              (CLAS-INDEXA > CLAS-MAXA)
              OR (CR-AHTYP = CLAS-I-BEN (CLAS-INDEXA))
           END-PERFORM

           IF CLAS-INDEXA > CLAS-MAXA
              DISPLAY ' ERROR - AH TYP NOT FOUND ' CR-AHTYP ' '
                 CR-CARRIER ' ' CR-STATE ' ' CR-ACCOUNT ' ' CR-CERT-NO
              PERFORM ABEND-PGM
           END-IF

           MOVE CLAS-I-EP (CLAS-INDEXA)
                                       TO WS-AH-EARNINGS-CALC
                                          WS-AH-REFUND-CALC

           MOVE CLAS-I-BAL (CLAS-INDEXA)
                                       TO WS-AH-SPECIAL-CALC-CD

           IF CLAS-I-REFUND-METHOD (CLAS-INDEXA) > '0'
              MOVE CLAS-I-REFUND-METHOD (CLAS-INDEXA)
                                       TO WS-AH-REFUND-CALC
           END-IF
           MOVE CLAS-CO-REM-TERM-CALC (CLAS-INDEXA)
                                       TO WS-AH-REM-TERM

           PERFORM 0300-FIND-STATE     THRU 0300-EXIT
           IF CF-ST-RT-CALC > '0'
              MOVE CF-ST-RT-CALC       TO WS-AH-REM-TERM
           END-IF

           IF CF-ST-RF-AH-CALC > '0'
              MOVE CF-ST-RF-AH-CALC    TO WS-AH-REFUND-CALC
           END-IF
           IF WS-AH-SPECIAL-CALC-CD = 'C'
              IF CF-ST-RF-CP-CALC > '0'
                 MOVE CF-ST-RF-CP-CALC TO WS-AH-REFUND-CALC
              END-IF
           END-IF

           PERFORM VARYING SUB3 FROM 1 BY 1 UNTIL
              (SUB3 > 50) OR
              ((CF-ST-BENEFIT-CD (SUB3) = CR-AHTYP) AND
               (CF-ST-BENEFIT-KIND (SUB3) = 'A'))
           END-PERFORM

           IF SUB3 > 50
              CONTINUE
           ELSE
              IF CF-ST-REFUND-CALC  (SUB3) > '0'
                 MOVE CF-ST-REFUND-CALC (SUB3)   TO WS-AH-REFUND-CALC
              END-IF
              IF CF-ST-REM-TERM-CALC (SUB3) > '0'
                 MOVE CF-ST-REM-TERM-CALC (SUB3) TO WS-AH-REM-TERM
              END-IF
           END-IF

           IF CR-LFTYP NOT = '  ' AND '00'
              CONTINUE
           ELSE
              PERFORM 0400-GET-ERACCT THRU 0400-EXIT
           END-IF

           PERFORM 3740-GET-AH-REFUND THRU 3740-EXIT

           MOVE CP-CALC-REFUND         TO CR-AHRFND-CALC

           IF CERT-RECS-SEL < 500
              DISPLAY ' AH MANUAL ' WS-AH-MEAN-REF ' AH LOGIC '
                 CR-AHRFND-CALC ' RTRM ' AH-REM-TRM1 ' PREM ' CR-AHPRM
                 ' TERM ' CR-AH-TERM
           END-IF

           .
       0277-EXIT.
           EXIT.

       0300-FIND-STATE.

           MOVE 'CID'                  TO CNTL-COMP-ID.
           MOVE SPACES                 TO CNTL-ACCESS.
           MOVE '3'                    TO CNTL-REC-TYPE.
           MOVE +0                     TO CNTL-SEQ-NO.
           MOVE CR-STATE               TO CNTL-ACCESS

           PERFORM 9000-READ-CONTROL   THRU 9000-EXIT

           .
       0300-EXIT.
           EXIT.
       0400-GET-ERACCT.

           MOVE X'04'         TO AM-COMPANY-CD
           MOVE CR-CARRIER    TO AM-CARRIER
           MOVE '000000'      TO AM-GROUPING
           MOVE CR-STATE      TO AM-STATE
           MOVE CR-ACCOUNT    TO AM-ACCOUNT
           MOVE CR-DT         TO AM-EXPIRE-DT
           START ERACCT KEY >= AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS NOT = '00'
              DISPLAY 'ERACCT START ' ERACCT-FILE-STATUS
           END-IF
           READ ERACCT NEXT RECORD

           .

       0400-EXIT.
           EXIT.

       3720-GET-LF-REFUND.

02906      MOVE CR-DT                 TO  DC-GREG-DATE-CYMD.            ECS010
02907      MOVE 'L'                   TO  DC-OPTION-CODE.               ECS010
02908      PERFORM 8500-DATE-CONVERT  THRU 8500-EXIT                    ECS010
02909      MOVE DC-BIN-DATE-1         TO  CP-CERT-EFF-DT                ECS010
02910                                                                   ECS010
02906      MOVE CR-LF-CANC-DT         TO  DC-GREG-DATE-CYMD.            ECS010
02907      MOVE 'L'                   TO  DC-OPTION-CODE.               ECS010
02908      PERFORM 8500-DATE-CONVERT  THRU 8500-EXIT                    ECS010
02909      MOVE DC-BIN-DATE-1         TO  CP-VALUATION-DT               ECS010
                                          WS-VAL-BIN-DATE
02910                                                                   ECS010
           IF CR-LOAN-1ST-PMT-DT IS NOT = ZEROS
              MOVE CR-LOAN-1ST-PMT-DT  TO  DC-GREG-DATE-1-YMD
              MOVE '3'                 TO  DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-BIN-DATE-1       TO  CP-FIRST-PAY-DATE
           ELSE
              MOVE CP-CERT-EFF-DT      TO  DC-BIN-DATE-1
              MOVE +1                  TO  DC-ELAPSED-MONTHS
              MOVE +0                  TO  DC-ELAPSED-DAYS
              MOVE '6'                 TO  DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-BIN-DATE-2       TO  CP-FIRST-PAY-DATE
           END-IF
           MOVE CR-LF-TERM             TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM

           MOVE WS-LF-COVERAGE-TYPE    TO CP-BENEFIT-TYPE
           PERFORM 0220-CALC-LF-REM-TERM THRU 0220-EXIT
           MOVE '2'                    TO CP-PROCESS-TYPE
           MOVE WS-LF-EARNINGS-CALC    TO CP-EARNING-METHOD.
           MOVE CR-LFAMT               TO CP-ORIGINAL-BENEFIT
                                          CP-RATING-BENEFIT-AMT
           MOVE CR-APR                 TO CP-LOAN-APR.
           MOVE CR-PMT-FREQ            TO CP-PAY-FREQUENCY.

082103     IF (WS-LF-EARNINGS-CALC = 'B')
082103        AND (WS-LF-SPECIAL-CALC-CD NOT = 'L')
082103        MOVE LF-BAL-REMTERM2     TO CP-REMAINING-TERM
082103     ELSE
082103        MOVE LF-REM-TRM2         TO CP-REMAINING-TERM
082103     END-IF
           IF CR-CERT-NO = '0007059837 '
              DISPLAY ' RTRM1      ' LF-BAL-REMTERM1
              DISPLAY ' RTRM2      ' LF-BAL-REMTERM2
           END-IF

           IF CP-REMAINING-TERM > ZERO
              PERFORM 2000-CALC-REM-AMT
                                       THRU 2999-CALC-REM-AMT-X
      *       MOVE WS-REM-AMT          TO WS-BALL-LF-RAMT
           END-IF

           MOVE CR-STATE               TO CP-STATE-STD-ABBRV
                                          CP-STATE
           MOVE WS-LF-REFUND-CALC      TO CP-EARNING-METHOD.
           MOVE WS-LF-EARNINGS-CALC    TO CP-RATING-METHOD.
           MOVE CR-RATING-CLASS        TO CP-CLASS-CODE.
           MOVE CR-LFTYP               TO CP-BENEFIT-CD.
           MOVE CR-LFAMT               TO CP-RATING-BENEFIT-AMT

           IF CP-STATE-STD-ABBRV = 'OR'
              COMPUTE CP-RATING-BENEFIT-AMT = CR-LFAMT +
                                            CR-LFAMT-ALT
           END-IF
           MOVE WS-REM-AMT             TO CP-REMAINING-BENEFIT
           MOVE CR-LFPRM               TO CP-ORIGINAL-PREMIUM.
           MOVE CR-LFPRM-ALT           TO CP-ALTERNATE-PREMIUM.
           MOVE CR-AGE                 TO CP-ISSUE-AGE.
           MOVE CR-LF-DEV-CODE         TO CP-DEVIATION-CODE.
           MOVE CR-LF-DEV-PCT          TO CP-RATE-DEV-PCT.

082103     IF (WS-LF-EARNINGS-CALC = 'B')
082103        AND (WS-LF-SPECIAL-CALC-CD NOT = 'L')
082103        MOVE LF-BAL-REMTERM1     TO CP-REMAINING-TERM
082103     ELSE
082103        MOVE LF-REM-TRM1         TO CP-REMAINING-TERM
082103     END-IF

082103*    MOVE LF-REM-TRM1            TO CP-REMAINING-TERM
           IF CR-STATE = 'OH'
              MOVE WS-LF-EARNINGS-CALC
                                       TO CP-EARNING-METHOD
           END-IF

           MOVE '6'                    TO CP-EARNING-METHOD
           MOVE '3'                    TO CP-PROCESS-TYPE

082103*    MOVE CP-REMAINING-TERM      TO WS-BALL-LF-RTRM
           MOVE 'CID'                  TO CP-COMPANY-ID
           MOVE 'L'                    TO CP-LIFE-OVERRIDE-CODE
           PERFORM 7600-GET-REFUND THRU 7600-EXIT
           MOVE CP-CALC-REFUND         TO WS-LF-REFUND
06168      IF WS-LF-EARNINGS-CALC = 'B'                                 ELC50PD
082103*      IF WS-LF-SPECIAL-CALC-CD = 'L'                             ELC50PD
082103*         ADD +1                   TO                             ELC50PD
082103*                                   CP-REMAINING-TERM             ELC50PD
082103*                                   CP-ORIGINAL-TERM              ELC50PD
082103*      END-IF                                                     ELC50PD
06176        MOVE 'L'                    TO CP-BENEFIT-TYPE             ELC50PD
06177        MOVE '2'                    TO CP-EARNING-METHOD           ELC50PD
06178                                     CP-RATING-METHOD              ELC50PD
06179        MOVE CR-LFAMT-ALT           TO CP-ORIGINAL-BENEFIT         ELC50PD
06180                                     CP-REMAINING-BENEFIT          ELC50PD
06181                                     CP-RATING-BENEFIT-AMT         ELC50PD
06182        IF CP-STATE-STD-ABBRV = 'OR'                               ELC50PD
06183            COMPUTE CP-RATING-BENEFIT-AMT = CR-LFAMT +             ELC50PD
06184                                            CR-LFAMT-ALT
             END-IF
06185        MOVE CR-LFPRM-ALT           TO CP-ORIGINAL-PREMIUM         ELC50PD
06186        MOVE 'LEV'                  TO CP-DEVIATION-CODE           ELC50PD
06188        PERFORM 7600-GET-REFUND THRU 7600-EXIT                     ELC50PD
06189        COMPUTE CP-CALC-REFUND = CP-CALC-REFUND + WS-LF-REFUND     ELC50PD
06200      END-IF                                                       ELC50PD

           .
       3720-EXIT.
           EXIT.

       0220-CALC-LF-REM-TERM.

           MOVE WS-VAL-BIN-DATE        TO CP-VALUATION-DT

           MOVE WS-LF-COVERAGE-TYPE       
                                       TO CP-BENEFIT-TYPE
           MOVE WS-LF-SPECIAL-CALC-CD   
                                       TO CP-SPECIAL-CALC-CD
           MOVE CR-LF-TERM             TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM

           IF ((WS-LF-EARNINGS-CALC = 'B') AND
              WS-LF-SPECIAL-CALC-CD NOT = 'L')
              ADD +1                   TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM
           END-IF
           MOVE WS-LF-REM-TERM         TO CP-REM-TERM-METHOD
           IF CP-TERM-IS-DAYS
              MOVE CR-LF-TERM-IN-DAYS  TO CP-TERM-OR-EXT-DAYS
           ELSE
              MOVE ZEROS               TO CP-TERM-OR-EXT-DAYS
           END-IF

           MOVE '1'                    TO CP-REM-TRM-CALC-OPTION

           PERFORM 0510-GET-REMAINING-TERM
                                       THRU 0510-EXIT

           IF ((WS-LF-EARNINGS-CALC = 'B') AND
              WS-LF-SPECIAL-CALC-CD NOT = 'L')
082103        MOVE CP-REMAINING-TERM-1 TO LF-BAL-REMTERM1
082103        MOVE CP-REMAINING-TERM-2 TO LF-BAL-REMTERM2
              COMPUTE CP-REMAINING-TERM-1 =
                             CP-REMAINING-TERM-1 - 1
              COMPUTE CP-REMAINING-TERM-2 =
                             CP-REMAINING-TERM-2 - 1
           END-IF

           IF CP-REMAINING-TERM-1 NEGATIVE
               MOVE ZEROS              TO CP-REMAINING-TERM-1
           END-IF

           IF CP-REMAINING-TERM-2 NEGATIVE
               MOVE ZEROS              TO CP-REMAINING-TERM-2
           END-IF

           MOVE CP-REMAINING-TERM-1    TO LF-REM-TRM1
           MOVE CP-REMAINING-TERM-2    TO LF-REM-TRM2

           .

       0220-EXIT.
           EXIT.
       2000-CALC-REM-AMT.

           IF CR-LFTYP = 'QD'
              MOVE +15.0               TO CR-APR
           END-IF

           MOVE +0                     TO WS-REM-AMT
           IF WS-LF-COVERAGE-TYPE  = 'L' OR 'P'
              IF WS-LF-EARNINGS-CALC = 'B'
                 COMPUTE WS-REM-AMT =
                   CR-LFAMT + CR-LFAMT-ALT
              ELSE
                 MOVE CR-LFAMT         TO WS-REM-AMT
              END-IF
      *       GO TO 2999-CALC-REM-AMT-X
              GO TO 2900-DISPLAY-AMTS
           END-IF

           COMPUTE INTERMED ROUNDED = CR-LFAMT / CR-LF-TERM

           IF LF-REM-TRM2 = CR-LF-TERM
              IF WS-LF-EARNINGS-CALC = 'B'
                 COMPUTE WS-REM-AMT =
                   CR-LFAMT + CR-LFAMT-ALT
              ELSE
                 MOVE CR-LFAMT         TO WS-REM-AMT
              END-IF
      *       GO TO 2999-CALC-REM-AMT-X
              GO TO 2900-DISPLAY-AMTS
           END-IF

           IF WS-LF-EARNINGS-CALC = 'B'
              GO TO 2010-ORDINARY-REM
           END-IF

           IF WS-LF-EARNINGS-CALC = 'T' OR '4'
              GO TO 2020-CALC-TEXAS-REM
           END-IF

           IF CR-LFTYP = 'QD'
              MOVE +15.0               TO CR-APR
              MOVE 'N'                 TO
                             WS-LF-EARNINGS-CALC            
              GO TO 2030-CALC-NET-PAY-REM
           END-IF

           IF WS-LF-EARNINGS-CALC = 'N' OR '5'
              GO TO 2030-CALC-NET-PAY-REM
           END-IF

           IF CF-STATE-ABBREVIATION = 'OH'
               IF (CR-LF-TERM GREATER THAN +60) AND
                  (CR-DT GREATER THAN 19831031) AND
                  (CR-APR GREATER THAN ZERO)
                   GO TO 2030-CALC-NET-PAY-REM
              END-IF
           END-IF

           IF CF-STATE-ABBREVIATION = 'MT'
               IF (CR-LF-TERM GREATER THAN +61) AND
                  (CR-DT  GREATER THAN 19830318) AND
                  (CR-APR GREATER THAN ZERO)
                   GO TO 2030-CALC-NET-PAY-REM
              END-IF
           END-IF

           IF CF-STATE-ABBREVIATION = 'UT'
               IF (CR-LF-TERM GREATER THAN +62) AND
                  (CR-DT  GREATER THAN 19810831) AND
                  (CR-DT  LESS THAN 19830901) AND
                  (CR-APR GREATER THAN ZERO)
                   GO TO 2030-CALC-NET-PAY-REM
              END-IF
           END-IF

           IF CF-STATE-ABBREVIATION = 'RI'
               IF (CR-LF-TERM GREATER THAN +60) AND
                  (CR-DT  GREATER THAN 19831231) AND
                  (CR-APR GREATER THAN ZERO)
                   GO TO 2030-CALC-NET-PAY-REM
              END-IF
           END-IF
           .
       2010-ORDINARY-REM.

           IF WS-LF-EARNINGS-CALC = 'B'
               COMPUTE WS-REM-AMT ROUNDED =
                        (INTERMED * LF-REM-TRM2) + CR-LFAMT-ALT
           ELSE
               COMPUTE WS-REM-AMT ROUNDED = INTERMED * LF-REM-TRM2
           END-IF

      *    GO TO 2999-CALC-REM-AMT-X
           GO TO 2900-DISPLAY-AMTS

           .
       2020-CALC-TEXAS-REM.

           DIVIDE CR-LFAMT BY CR-LF-TERM
               GIVING TEX-FACT-1.
           DIVIDE LF-REM-TRM2 BY CR-PMT-FREQ
               GIVING TEX-FACT-2
               REMAINDER TEX-FACT-3.

           IF TEX-FACT-3 NOT = ZERO
               ADD +1                  TO TEX-FACT-2
           END-IF

           IF (TEX-FACT-2 * CR-PMT-FREQ) = CR-LF-TERM
               MOVE CR-LFAMT           TO WS-REM-AMT
           ELSE
               COMPUTE WS-REM-AMT ROUNDED =
                   (TEX-FACT-1 * (TEX-FACT-2 * CR-PMT-FREQ))
           END-IF

      *    GO TO 2999-CALC-REM-AMT-X
           GO TO 2900-DISPLAY-AMTS

           .
       2030-CALC-NET-PAY-REM.

           MOVE CR-DT                  TO  DC-GREG-DATE-CYMD
           MOVE 'L'                    TO  DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
           MOVE DC-BIN-DATE-1          TO  CP-CERT-EFF-DT
      *    MOVE WS-BIN-CR-DT           TO  CP-CERT-EFF-DT

           IF CR-LOAN-1ST-PMT-DT IS NOT = ZEROS
              MOVE CR-LOAN-1ST-PMT-DT  TO  DC-GREG-DATE-1-YMD
              MOVE '3'                 TO  DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-BIN-DATE-1       TO  CP-FIRST-PAY-DATE
           ELSE
              MOVE CP-CERT-EFF-DT      TO  DC-BIN-DATE-1
              MOVE +1                  TO  DC-ELAPSED-MONTHS
              MOVE +0                  TO  DC-ELAPSED-DAYS
              MOVE '6'                 TO  DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-BIN-DATE-2       TO  CP-FIRST-PAY-DATE
           END-IF

           IF WS-LF-EARNINGS-CALC = 'B' OR 'K' OR 'L'
              COMPUTE CP-ORIGINAL-BENEFIT = CR-LFAMT + CR-LFAMT-ALT
           ELSE
              MOVE CR-LFAMT            TO  CP-ORIGINAL-BENEFIT
           END-IF
           MOVE CR-APR                 TO  CP-LOAN-APR
           MOVE CR-LF-TERM             TO  CP-ORIGINAL-TERM
           MOVE CR-LOAN-TERM           TO  CP-LOAN-TERM
           MOVE LF-REM-TRM2            TO  CP-REMAINING-TERM
           MOVE WS-LF-SPECIAL-CALC-CD         
                                       TO CP-SPECIAL-CALC-CD
           MOVE WS-LF-COVERAGE-TYPE       
                                       TO  CP-BENEFIT-TYPE
           MOVE WS-LF-EARNINGS-CALC      
                                       TO  CP-EARNING-METHOD

           CALL 'ELRAMTX' USING CALCULATION-PASS-AREA

           MOVE CP-REMAINING-AMT       TO  WS-REM-AMT

           .
       2900-DISPLAY-AMTS.

      *    IF (CR-DT > 19921231) AND
      *       (CR-DT < 19940101) AND
      *    IF (WS-BIN-VAL-DATES (VYR) = X'95FF')
      *       MOVE CR-DT TO WS-DISPLAY-DATE
      *       MOVE CR-LF-TERM TO WS-DISPLAY-TERM
      *       MOVE LF-REM-TRM2 TO WS-DISPLAY-RTERM
      *       MOVE CR-LFPRM TO WS-DISPLAY-PRM
      *       IF CLAS-I-EP (CLAS-INDEXL) EQUAL 'B' OR 'K' OR 'L'
      *          ADD CR-LFAMT CR-LFAMT-ALT GIVING WS-DISPLAY-AMT
      *       ELSE
      *          MOVE CR-LFAMT     TO WS-DISPLAY-AMT
      *       END-IF
      *       MOVE WS-REM-AMT   TO WS-DISPLAY-RAMT
      *       DISPLAY '  ' CR-CERT-NO '  ' WS-DISPLAY-DATE '   '
      *        WS-DISPLAY-TERM '  ' WS-DISPLAY-RTERM '   '
      *        WS-DISPLAY-AMT '   ' WS-DISPLAY-RAMT
      *    END-IF

           .
       2999-CALC-REM-AMT-X.
           EXIT.

       3740-GET-AH-REFUND.

02906      MOVE CR-DT                 TO  DC-GREG-DATE-CYMD.            ECS010
02907      MOVE 'L'                   TO  DC-OPTION-CODE.               ECS010
02908      PERFORM 8500-DATE-CONVERT  THRU 8500-EXIT                    ECS010
02909      MOVE DC-BIN-DATE-1         TO  CP-CERT-EFF-DT                ECS010

02906      MOVE CR-AH-CANC-DT         TO  DC-GREG-DATE-CYMD.            ECS010
02907      MOVE 'L'                   TO  DC-OPTION-CODE.               ECS010
02908      PERFORM 8500-DATE-CONVERT  THRU 8500-EXIT                    ECS010
02909      MOVE DC-BIN-DATE-1         TO  CP-VALUATION-DT               ECS010
                                          WS-VAL-BIN-DATE
02910                                                                   ECS010
           IF CR-LOAN-1ST-PMT-DT IS NOT = ZEROS
              MOVE CR-LOAN-1ST-PMT-DT  TO  DC-GREG-DATE-1-YMD
              MOVE '3'                 TO  DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-BIN-DATE-1       TO  CP-FIRST-PAY-DATE
           ELSE
              MOVE CP-CERT-EFF-DT      TO  DC-BIN-DATE-1
              MOVE +1                  TO  DC-ELAPSED-MONTHS
              MOVE +0                  TO  DC-ELAPSED-DAYS
              MOVE '6'                 TO  DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8500-EXIT
              MOVE DC-BIN-DATE-2       TO  CP-FIRST-PAY-DATE
           END-IF
           MOVE CR-AH-TERM             TO CP-ORIGINAL-TERM
           PERFORM 0420-CALC-AH-REM-TERM THRU 0420-EXIT
      *    COMPUTE CP-REMAINING-TERM = CR-AH-TERM - 1
           MOVE WS-AH-REFUND-CALC      TO CP-EARNING-METHOD.
           MOVE WS-AH-EARNINGS-CALC    TO CP-RATING-METHOD.
           MOVE '8'                    TO CP-EARNING-METHOD
           
           MOVE '2'                    TO CP-PROCESS-TYPE
           MOVE CR-RATING-CLASS        TO CP-CLASS-CODE.
           MOVE CR-AHTYP               TO CP-BENEFIT-CD.
           MOVE CR-STATE               TO CP-STATE-STD-ABBRV
                                          CP-STATE
           MOVE CR-AHAMT               TO CP-ORIGINAL-BENEFIT
                                          CP-RATING-BENEFIT-AMT.
           IF CP-STATE-STD-ABBRV = 'OR'
               COMPUTE CP-RATING-BENEFIT-AMT = CR-AHAMT * CR-AH-TERM
           END-IF    

           MOVE CR-AHPRM               TO CP-ORIGINAL-PREMIUM.
           MOVE CR-PMT-FREQ            TO CP-PAY-FREQUENCY.
           MOVE CR-AGE                 TO CP-ISSUE-AGE.
           MOVE CR-AH-DEV-CODE         TO CP-DEVIATION-CODE.
           MOVE CR-AH-DEV-PCT          TO CP-RATE-DEV-PCT.
           MOVE CR-APR                 TO CP-LOAN-APR.

           COMPUTE WS-AH-R78-FACT ROUNDED =
              (AH-REM-TRM1 * (AH-REM-TRM1 + 1)) /
              (CR-AH-TERM * (CR-AH-TERM + 1))
           COMPUTE WS-AH-R78-REF = WS-AH-R78-FACT * CR-AHPRM
           COMPUTE WS-AH-PRO-REF ROUNDED =
              CR-AHPRM / CR-AH-TERM * AH-REM-TRM1
           COMPUTE WS-AH-MEAN-REF = (WS-AH-R78-REF + WS-AH-PRO-REF)
              * .5

           MOVE AH-REM-TRM1        TO CP-REMAINING-TERM
           MOVE 'CID'                  TO CP-COMPANY-ID
      *    MOVE 'A'                    TO CP-AH-OVERRIDE-CODE
           PERFORM 7600-GET-REFUND THRU 7600-EXIT.


           .
       3740-EXIT.
           EXIT.
       0420-CALC-AH-REM-TERM.

           MOVE WS-VAL-BIN-DATE        TO CP-VALUATION-DT

           MOVE 'A'                    TO CP-BENEFIT-TYPE
           MOVE WS-AH-SPECIAL-CALC-CD  TO CP-SPECIAL-CALC-CD
           MOVE CR-AH-TERM             TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM

           MOVE WS-AH-REM-TERM         TO CP-REM-TERM-METHOD
           MOVE ZEROS                  TO CP-TERM-OR-EXT-DAYS

           MOVE '1'                    TO CP-REM-TRM-CALC-OPTION

           PERFORM 0510-GET-REMAINING-TERM
                                       THRU 0510-EXIT

           IF CP-REMAINING-TERM-1 NEGATIVE
               MOVE ZEROS              TO CP-REMAINING-TERM-1
           END-IF

           IF CP-REMAINING-TERM-2 NEGATIVE
               MOVE ZEROS              TO CP-REMAINING-TERM-2
           END-IF

           MOVE CP-REMAINING-TERM-1    TO AH-REM-TRM1
      *                                   WS-BALL-AH-RTRM
           MOVE CP-REMAINING-TERM-2    TO AH-REM-TRM2

           .
       0420-EXIT.
           EXIT.


       3500-WRITE-CERT.

           WRITE CERT-OUT-RECORD       FROM CERTIFICATE-RECORD
           ADD 1                       TO CERT-RECS-OUT

           .
       3500-EXIT.
           EXIT.

       0500-CLOSE-FILES.

04666      MOVE 'R' TO CP-RATE-FILE.                                    
04667      MOVE 'C' TO CP-IO-FUNCTION.                                  
           CALL 'ELRATEX' USING CALCULATION-PASS-AREA
04669                                                                   
04670      IF IO-ERROR                                                  
04671         MOVE +0303   TO WS-RETURN-CODE                            
04672         MOVE ' ERROR ON ERRATE CLOSE' TO WS-ABEND-MESSAGE         
04673         PERFORM ABEND-PGM
           END-IF
           CLOSE CERT-IN CERT-OUT ELCNTL ERACCT

           .
       0500-EXIT.
           EXIT.

       0510-GET-REMAINING-TERM.

           CALL 'ELRTRMX' USING CALCULATION-PASS-AREA

           .
       0510-EXIT.
           EXIT.

       7600-GET-REFUND.
           MOVE X'04'                  TO CP-COMPANY-CD
           MOVE 'CID'                  TO CP-COMPANY-ID
           MOVE 'R'                    TO CP-RATE-FILE
           CALL 'ELRFNDX' USING CALCULATION-PASS-AREA.

       7600-EXIT.
           EXIT.
           EJECT

       8100-GET-RATE.

           MOVE 'R'                    TO CP-RATE-FILE
           CALL 'ELRATEX' USING CALCULATION-PASS-AREA.

       8100-EXIT.
           EXIT.

       9000-READ-CONTROL.

           MOVE ELCNTL-KEY TO CF-CONTROL-PRIMARY.
           START ELCNTL KEY NOT LESS THAN CF-CONTROL-PRIMARY.

           IF ELCNTL-FILE-STATUS NOT = '00'
              GO TO 9000-NOTFND.

           READ ELCNTL NEXT.

           IF ELCNTL-FILE-STATUS NOT = '00'
              GO TO 9000-NOTFND.

           GO TO 9000-EXIT.

       9000-NOTFND.

           DISPLAY ' ELCNTL NOT FOUND ' ELCNTL-FILE-STATUS
           .
       9000-EXIT.
           EXIT.
       8500-DATE-CONVERT.
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA.

       8500-EXIT.
           EXIT.
           EJECT

       ABEND-PGM.   COPY ELCABEND.

