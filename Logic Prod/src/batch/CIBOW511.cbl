       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIBOW511.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      ****************************************************************  00000900
      *                                                                 00001000
      * REMARKS.                                                        00001100
      *                                                                 00001200
      * COMFED PROCESSING.                                              00001300
      *   (USING THE 800 CHARACTER INPUT RECORDS FROM COMFED)           00001400
      *                                                                 00001500
      *   1)  THIS PROGRAM WILL EXTRACT RECORDS FROM THE COPY FILE      00001600
      *        OF COMFED INPUT AND CREATE INDIVIDUAL RECORDS            00001700
      *         FOR INPUT TO LOGIC FROM THEM.                           00001800
      *                                                                 00001900
      *   2)  IT ALSO BUILDS THE 115 LRECL PPA FILE FOR TIM WOOD. IT    00001900
      *        IS DOWNLOADED TO A PC FILE FOR COMFED TO USE.            00001900
      *                                                                 00001900
      * NOTE: AS OF 7-01-99 THE PPA FILE IS NO LONGER BEING BUILT.      00001900
      * ===== COMMERCIAL FEDERAL IS PROVIDING TIM WOOD WITH A DISKETT   00001900
      *       FILE.  THIS LOGIC HAS BEEN COMMENTED OUT.                 00001900
      *                                                                 00001900
      ****************************************************************  00002800
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 092805   CR2005080300009 PEMA  NEW PROGRAM
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  COMFED-IN           ASSIGN TO SYS010.
      *       ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  DISK-DATE           ASSIGN TO SYS019.
           SELECT  COMFED-OUT          ASSIGN TO SYS012.

       DATA DIVISION.
       FILE SECTION.

       FD  COMFED-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  COMFED-RECORD               PIC X(480).

       FD  DISK-DATE
                                       COPY ELCDTEFD.
       FD  COMFED-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  CARD-RECORD                 PIC X(101).

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.  00133388
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  00133389
       77  FILLER  PIC X(32) VALUE '********************************'.  00133390
       77  W-DISPLAY-DATE        PIC 9(9) VALUE ZEROS.
                                                                        00133391
       77  V-AH-BEN-AMT-WORK      PIC S9(7)V99  VALUE ZEROS.            00134107
       77  LAST-REC-SW            PIC X         VALUE 'N'.              00133393
       77  LETTER-SW              PIC X         VALUE 'N'.              00133393
       77  SAVE-SUB               PIC 99        VALUE ZERO.             00133393
       77  SAVE-1                 PIC X         VALUE SPACE.            00133393
       77  CANCL-SW               PIC X         VALUE 'N'.              00133394
       77  SW-20                  PIC X         VALUE ' '.              00133395
       77  ERROR-CODE             PIC X(25)     VALUE SPACES.           00133396
       77  SAVE-SEQ               PIC X(30)     VALUE SPACES.           00133397
       77  SAVE-CERT              PIC X(10)     VALUE SPACES.           00133398
       77  SAVE-STATE             PIC X(02)     VALUE SPACES.           00133399
       77  HDR-CNT                PIC 9(07)     VALUE ZEROS.            00133400
       77  NAME-LINE-CNT          PIC 99        VALUE ZEROS.            00133403
       77  RPT-CNT                PIC 9999999   VALUE ZEROS.            00133404
       77  SPACE-CNT              PIC 9         VALUE ZEROS.            00133404
       77  DETAIL-CNT             PIC 9999      VALUE ZEROS.            00133405
       77  NINES-IN               PIC 9(12)     VALUE 999999999999.     00133406
       77  DETAIL-CNT-TOT         PIC 9999999   VALUE ZEROS.            00133407
       77  PPA-CNT                PIC 9999999   VALUE ZEROS.            00133407
       77  LOGIC-CNT              PIC 9999999   VALUE ZEROS.            00133407
       77  LOGIC-CANC-CNT         PIC 9999999   VALUE ZEROS.            00133407
       77  IN-CNT                 PIC 9999999   VALUE ZEROS.            00133408
       77  TOT-LIFE-WRITTEN       PIC S9(8)V99  VALUE ZEROS.            00133409
       77  TOT-LIFE-CANC          PIC S9(8)V99  VALUE ZEROS.            00133410
       77  TOT-AH-WRITTEN         PIC S9(8)V99  VALUE ZEROS.            00133411
       77  TOT-AH-CANC            PIC S9(8)V99  VALUE ZEROS.            00133412
       77  TOT-CERT-ISS           PIC S9(4)     VALUE ZEROS.            00133413
       77  TOT-CERT-CANC          PIC S9(4)     VALUE ZEROS.            00133414
       77  CANC-CNT               PIC 9999      VALUE ZEROS.            00133415
       77  CERT-CNT               PIC 9999      VALUE ZEROS.            00133416
       77  FIRST-SW               PIC X         VALUE 'Y'.              00133418
       77  WRITTEN-PREM-SW        PIC X         VALUE 'N'.              00133419
       77  JR-SUB                 PIC 99        VALUE ZEROS.            00133421
       77  SP-SUB                 PIC 99        VALUE ZEROS.            00133421
       77  JR-SW                  PIC X         VALUE 'N'.              00133422
       77  SAVE-JNT-FIRST-INIT    PIC X(01)     VALUE SPACES.           00133423
       77  SAVE-SUB-LT-SUB1       PIC 9999999   VALUE ZEROS.            00133424
       77  SUB1                   PIC 99        VALUE ZEROS.            00133425
       77  SUB2                   PIC 99        VALUE ZEROS.            00133426
       77  CODE-SUB               PIC 99        VALUE ZEROS.            00133427
       77  G-SUB                  PIC 99        VALUE ZEROS.            00133428
       77  SAVE-G-SUB             PIC 99        VALUE ZEROS.            00133429
       77  PGM-SUB                PIC S9(4) COMP  VALUE +511.
       77  WS-EOF-SW              PIC X         VALUE ' '.
           88  END-OF-BOW                       VALUE 'Y'.
       77  WS-RETURN-CODE         PIC S9(3)     VALUE ZERO.
       77  WS-ABEND-MESSAGE       PIC X(80)     VALUE SPACES.
       77  SAVE-EFF-DATE          PIC X(6)      VALUE SPACES.
       77  WS-BIN-EFF-DT          PIC XX        VALUE LOW-VALUES.
       77  WS-BIN-BIRTH-DT        PIC XX        VALUE LOW-VALUES.
       77  WS-INSURED-AGE         PIC 999       VALUE ZEROS.
       77  RECS-IN                PIC 9(7)      VALUE ZEROS.
       77  WS-ABEND-FILE-STATUS   PIC XX        VALUE ZERO.
       77  WS-ZERO                PIC S9        VALUE ZERO.
       77  WS-ISS-SW              PIC X         VALUE ' '.
           88  ISSUED-CERT                      VALUE 'Y'.
       77  WS-CAN-SW              PIC X         VALUE ' '.
           88  CANCELLED-CERT                   VALUE 'Y'.
                                                                        00133352
       01  WS-CERT-WORK1.                                               00133352
           05  WS-CERT-WK1-NUM-6         PIC X(06).                     00133352
           05  WS-CERT-WK1-ZEROS-4       PIC X(04).                     00133352
       01  WS-CERT-WORK1-R  REDEFINES                                   00133352
           WS-CERT-WORK1.                                               00133352
           05  WS-CERT-WK1-NUM-7         PIC X(07).                     00133352
           05  WS-CERT-WK1-ZEROS-3       PIC X(03).                     00133352
                                                                        00133352
       01  WS-CERT-WORK2.                                               00133352
           05  WS-CERT-WK2-ZEROS-4       PIC X(04).                     00133352
           05  WS-CERT-WK2-NUM-6         PIC X(06).                     00133352
       01  WS-CERT-WORK2-R  REDEFINES                                   00133352
           WS-CERT-WORK2.                                               00133352
           05  WS-CERT-WK2-ZEROS-3       PIC X(03).                     00133352
           05  WS-CERT-WK2-NUM-7         PIC X(07).                     00133352
                                                                        00133352
                                                                        00133446
       01  SPACE-CK.                                                    00133447
           05  SP-CK               PIC X    OCCURS 26 TIMES.            00133448

       01  CANC-NAME.                                                   00133461
           05   A-NAME             PIC X(11) VALUE SPACE.               00133462
           05   A-INIT             PIC X     VALUE SPACE.               00133463

       01  W-PROC-YR-CEN.                                               00133497
           05  W-PROC-MO          PIC 99        VALUE ZEROS.            00133417
           05  W-PROC-SLASH       PIC X         VALUE '/'.              00133417
           05  W-PROC-CEN         PIC X(02)     VALUE SPACES.           00133498
           05  W-PROC-YR          PIC 9(02)     VALUE ZEROS.            00133499
                                                                        00133500












       01  OUT-RECORD.                                                  00133501
           05  OUT-CERT-NO        PIC X(10)     VALUE SPACES.           00133502
           05  OUT-CERT-SFX       PIC X(01)     VALUE SPACES.           00133503
           05  OUT-EFF-DATE       PIC X(06)     VALUE SPACES.           00133504
           05  OUT-INFO           PIC X(61)     VALUE SPACES.           00133505
           05  OUT-TRAN-TYPE      PIC X         VALUE SPACES.           00133506
           05  OUT-SEQUENCE       PIC X         VALUE SPACES.
           05  SR-ACCT-NO         PIC X(10)     VALUE SPACES.
           05  SR-CERT-NO         PIC X(10)     VALUE ZEROS.
           05  SR-CERT-SFX        PIC X         VALUE ZEROS.
           
      *    05  SR-BATCH-NUMB.                                           00133508
      *        10  SR-BATCH-ID    PIC XX        VALUE ZEROS.            00133508
      *        10  SR-BATCH-DA    PIC XX        VALUE ZEROS.            00133508
      *        10  SR-BATCH-NO    PIC 99        VALUE ZEROS.            00133508
      *    05  SR-REC-CNT         PIC 9(4)      VALUE ZEROS.            00133509
                                                                        00133510
       01  SAVE-FULL-ACCT.                                              00133511
           05 SAVE-ACCT-7         PIC X(07)     VALUE SPACES.           00133512
           05 SAVE-ACCT           PIC XXX       VALUE SPACES.           00133513
                                                                        00133514
       01  COMFED-BATCH-HDR.                                            00133515
           05 CF-CARR-CO.                                               00133516
               10 CF-CARR          PIC X         VALUE '9'.             00133517
               10 CF-COMP          PIC X(6)      VALUE '000000'.        00133518
           05 CF-STATE             PIC XX        VALUE '  '.            00133519
           05 CF-ACCT-NO           PIC X(10)     VALUE SPACES.          00133520
           05 CF-BATCH-NUMB.                                            00133521
               10 CF-BATCH-ID      PIC XX        VALUE ZEROS.           00133521
               10 CF-BATCH-DA      PIC XX        VALUE ZEROS.           00133521
               10 CF-BATCH-NO      PIC 99        VALUE ZEROS.           00133521
           05 CF-HD-EFF-DT.                                             00133522
               10 CF-HD-EFF-MO     PIC XX        VALUE SPACES.          00133523
               10 CF-HD-EFF-DA     PIC XX        VALUE SPACES.          00133524
               10 CF-HD-EFF-YR     PIC XX        VALUE SPACES.          00133525
           05 CF-CERT-ISS          PIC S9(4)     VALUE ZEROS.           00133526
           05 CF-LF-WRITTEN        PIC S9(7)V99  VALUE ZEROS.           00133527
           05 CF-AH-WRITTEN        PIC S9(7)V99  VALUE ZEROS.           00133528
           05 CF-CERT-CANC         PIC S9(4)     VALUE ZEROS.           00133529
           05 CF-LF-CANC           PIC S9(7)V99  VALUE ZEROS.           00133530
           05 CF-AH-CANC           PIC S9(7)V99  VALUE ZEROS.           00133531
           05 CF-CLIENT-ID         PIC X(3)      VALUE 'CSO'.           00133532
           05 CF-TRANS-TYPE        PIC X         VALUE '1'.             00133533
           05 CF-SEQUENCE-NO       PIC X         VALUE '0'.             00133534
                                                                        00133535
                                                                        00133621
       01  WK-DATE-N.                                                   00133622
           05  WK-MO-X             PIC XX     VALUE SPACES.             00133623
           05  WK-MO-N REDEFINES WK-MO-X PIC 99.                        00133624
                                                                        00133625
       01  WK-DATE.                                                     00133626
           05  WK-MO               PIC XX     VALUE SPACES.             00133627
           05  FILLER              PIC X      VALUE SPACES.             00133628
           05  WK-DA               PIC XX     VALUE SPACES.             00133629
           05  FILLER              PIC X      VALUE SPACES.             00133630
           05  WK-YR               PIC XX     VALUE SPACES.             00133631
                                                                        00133632
       01  WORK-DATE.                                                   00133633
           12  WORK-BIRTH-DATE.
               15  WB-CC               PIC XX.
               15  WB-YY               PIC XX.
               15  WB-MM               PIC XX.
               15  WB-DD               PIC XX.
           12  WORK-BIRTH-DATE-N REDEFINES WORK-BIRTH-DATE
                                       PIC 9(8).
           12  WORK-DATE-X.                                             00133634
               15  WORK-MO-X       PIC XX.                              00133635
               15  WORK-DA-X       PIC XX.                              00133636
               15  WORK-YR-X       PIC XX.                              00133637
           12  WORK-DATE-N    REDEFINES   WORK-DATE-X.                  00133638
               15  WORK-MO-N       PIC 99.                              00133639
               15  WORK-DA-N       PIC 99.                              00133640
               15  WORK-YR-N       PIC 99.                              00133641
                                                                        00133642
           12  WORK-DATE-IN.                                            00133638
               15  WORK-YR-IN      PIC 99.                              00133641
               15  WORK-MO-IN      PIC 99.                              00133639
               15  WORK-DA-IN      PIC 99.                              00133640
                                                                        00133642
       01  WORK-ISS-DATE.                                               00133642
           05  WRK-ISS-YR          PIC XX.                              00133642
           05  WRK-ISS-MO          PIC XX.                              00133642
           05  WRK-ISS-DA          PIC XX.                              00133642
                                                                        00133642
       01  WORK-ISS-DTE-R          REDEFINES                            00133638
           WORK-ISS-DATE           PIC 9(06).                           00133638
                                                                        00133642
       01  WORK-ISS-DTE-N          REDEFINES                            00133638
           WORK-ISS-DATE.                                               00133638
           05  WRK-ISS-YR-N        PIC 99.                              00133642
           05  WRK-ISS-MO-N        PIC 99.                              00133642
           05  WRK-ISS-DA-N        PIC 99.                              00133642
                                                                        00133642
                                                                        00133383
      ******************************************************************00133384
                                                                        00133379
                                                                        00133662
                                       COPY ERCPNDBI.
                                       COPY ERCBOWRL.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
                                                                        00133669
      ******************************************************************00133673
       PROCEDURE DIVISION.

                                       COPY ELCDTERX.
                                       
           PERFORM 0010-OPEN-FILES     THRU 0010-EXIT
           PERFORM 0020-INIT           THRU 0020-EXIT
           PERFORM 0030-PROCESS-BOW    THRU 0030-EXIT UNTIL
              END-OF-BOW
           PERFORM 0015-FINALIZE       THRU 0015-EXIT
           
      *    DISPLAY STUFF HERE
           
           GOBACK
           .
       0010-OPEN-FILES.

           OPEN INPUT  COMFED-IN
                OUTPUT COMFED-OUT

           .
       0010-EXIT.
           EXIT.

       0015-FINALIZE.

           PERFORM 0160-ACCOUNT-BREAK  THRU 0160-EXIT
           CLOSE COMFED-IN COMFED-OUT

           .
       0015-EXIT.
           EXIT.

       0020-INIT.
       
           ACCEPT WORK-DATE-IN         FROM DATE
           DISPLAY ' WORK DATE IN ' WORK-DATE-IN
           
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'
           DISPLAY '*   CURRENT DATE IS -- ' WS-CURRENT-DATE
           MOVE RUN-DATE               TO W-DISPLAY-DATE
           DISPLAY ' RUN DATE IS ' W-DISPLAY-DATE
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'

           PERFORM 0040-READ-COMFED    THRU 0040-EXIT
           MOVE 'CF'                   TO CF-BATCH-ID
           MOVE WORK-MO-IN             TO CF-BATCH-DA
           ADD  1                      TO CF-BATCH-NO
           
           .
       0020-EXIT.
           EXIT.

       0030-PROCESS-BOW.

           IF VALID-HDR-ID
              CONTINUE
           ELSE
              IF VALID-DETAIL-ID
                 IF BWD-ACCOUNT-NO NOT = SAVE-FULL-ACCT
                    PERFORM 0160-ACCOUNT-BREAK
                                       THRU 0160-EXIT
                 END-IF
                 PERFORM 0070-BUILD-COMMON
                                       THRU 0070-EXIT
                 IF ISSUED-CERT OR CANCELLED-CERT
                    IF BWD-CANCEL-DATE NOT = SPACES AND ZEROS
                       PERFORM 0060-BUILD-CANCEL-REC
                                       THRU 0060-EXIT
                    ELSE
                       PERFORM 0050-BUILD-ISSUE-REC
                                       THRU 0050-EXIT
                    END-IF
                 END-IF
              ELSE
                 IF VALID-TRLR-ID
                    DISPLAY ' FOUND TRLR '
                 ELSE
                    DISPLAY ' INVALID REC ID ' BW-RECORD-ID
                 END-IF
              END-IF
           END-IF

           PERFORM 0040-READ-COMFED    THRU 0040-EXIT

           .
       0030-EXIT.
           EXIT.

       0040-READ-COMFED.

           READ COMFED-IN INTO BANK-OF-THE-WEST-INPUT AT END
                SET END-OF-BOW         TO TRUE
           END-READ

           IF NOT END-OF-BOW
              ADD 1                    TO RECS-IN
           END-IF

           .
       0040-EXIT.
           EXIT.

       0050-BUILD-ISSUE-REC.

           PERFORM 0080-BUILD-SEQ-1    THRU 0080-EXIT
           PERFORM 0090-BUILD-SEQ-2    THRU 0090-EXIT
           PERFORM 0100-BUILD-SEQ-3    THRU 0100-EXIT
           PERFORM 0110-BUILD-SEQ-4    THRU 0110-EXIT
           PERFORM 0120-BUILD-SEQ-5    THRU 0120-EXIT
           PERFORM 0122-BUILD-SEQ-6    THRU 0122-EXIT
           PERFORM 0125-BUILD-SEQ-7    THRU 0125-EXIT

           .
       0050-EXIT.
           EXIT.

       0060-BUILD-CANCEL-REC.

           PERFORM 0130-BUILD-CANCEL   THRU 0130-EXIT

           .
       0060-EXIT.
           EXIT.

       0070-BUILD-COMMON.

           MOVE ' '                    TO WS-ISS-SW
                                          WS-CAN-SW
           MOVE BWD-ACCOUNT-NO         TO SAVE-FULL-ACCT
           MOVE BWD-CERT-EFF-DATE (5:2) TO WORK-YR-X
                                          WRK-ISS-YR
           MOVE BWD-CERT-EFF-DATE (1:2) TO WORK-MO-X
                                          WRK-ISS-MO
           MOVE BWD-CERT-EFF-DATE (3:2) TO WORK-DA-X
                                          WRK-ISS-DA
           MOVE WORK-DATE-X            TO SAVE-EFF-DATE                                          

           MOVE WORK-ISS-DTE-R         TO DC-GREG-DATE-1-YMD

           MOVE '3'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-EFF-DT
           ELSE
              DISPLAY ' BAD ISSUE DATE ' WORK-ISS-DTE-R '  '
              'CERT = ' BWD-LOAN-NO
           END-IF

           MOVE BWD-DOB (5:2)          TO WB-YY
           MOVE BWD-DOB (1:2)          TO WB-MM
           MOVE BWD-DOB (3:2)          TO WB-DD
           IF WB-YY NOT < WORK-YR-IN
              MOVE 19                  TO WB-CC
           ELSE
              MOVE 20                  TO WB-CC
           END-IF
           
           MOVE WORK-BIRTH-DATE-N      TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE

           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-BIRTH-DT
           ELSE
              DISPLAY ' BAD BIRTH DATE ' BWD-DOB    '  '
              'CERT = ' BWD-LOAN-NO
           END-IF

           MOVE 40                     TO WS-INSURED-AGE
           
           MOVE WS-BIN-BIRTH-DT        TO DC-BIN-DATE-1
           MOVE WS-BIN-EFF-DT          TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              COMPUTE WS-INSURED-AGE = DC-ELAPSED-MONTHS / +12
           ELSE
              DISPLAY ' BAD AGE CALC ' BWD-DOB ' '
              BWD-CERT-EFF-DATE  ' CERT = ' BWD-LOAN-NO
              ' ' DC-ERROR-CODE
           END-IF

           MOVE  BWD-LOAN-NO           TO SAVE-CERT

           IF BWD-LF-PREM NOT NUMERIC
              MOVE ZEROS               TO BWD-LF-PREM
           END-IF

           IF BWD-LEV-LF-PREM NOT NUMERIC
              MOVE ZEROS               TO BWD-LEV-LF-PREM
           END-IF

           IF BWD-AH-PREM NOT NUMERIC
              MOVE ZEROS               TO BWD-AH-PREM
           END-IF

           IF BWD-IUI-PREM NOT NUMERIC
              MOVE ZEROS               TO BWD-IUI-PREM
           END-IF

           IF BWD-LF-REFUND NOT NUMERIC
              MOVE ZEROS               TO BWD-LF-REFUND
           END-IF

           IF BWD-LEV-LF-REFUND NOT NUMERIC
              MOVE ZEROS               TO BWD-LEV-LF-REFUND
           END-IF

           IF BWD-AH-REFUND NOT NUMERIC
              MOVE ZEROS               TO BWD-AH-REFUND
           END-IF

           IF BWD-IUI-REFUND NOT NUMERIC
              MOVE ZEROS               TO BWD-IUI-REFUND
           END-IF

           IF BWD-INS-TERM NOT NUMERIC
              MOVE ZEROS               TO BWD-INS-TERM
           END-IF

           IF BWD-LOAN-TERM NOT NUMERIC
              MOVE ZEROS               TO BWD-LOAN-TERM
           END-IF

           IF BWD-LOAN-TERM = ZEROS
              MOVE BWD-INS-TERM        TO BWD-LOAN-TERM
           END-IF
           
           IF (BWD-LF-PREM             > ZEROS)
              OR (BWD-LEV-LF-PREM      > ZEROS)
              OR (BWD-AH-PREM          > ZEROS)
              OR (BWD-IUI-PREM         > ZEROS)
              SET ISSUED-CERT          TO TRUE
           END-IF

           IF (BWD-LF-REFUND           > ZEROS)
              OR (BWD-LEV-LF-REFUND    > ZEROS)
              OR (BWD-AH-REFUND        > ZEROS)
              OR (BWD-IUI-REFUND       > ZEROS)
              SET CANCELLED-CERT       TO TRUE
              IF BWD-CANCEL-DATE = SPACES OR ZEROS
                 MOVE '051505'         TO BWD-CANCEL-DATE
              END-IF
           END-IF

           .
       0070-EXIT.
           EXIT.
           
       0080-BUILD-SEQ-1.
       
           ADD 1                       TO LOGIC-CNT
           ADD 1                       TO CERT-CNT
           ADD 1                       TO CF-CERT-ISS
           ADD 1                       TO TOT-CERT-ISS

           MOVE SPACES                 TO PBI-RECORD-BODY
           
           MOVE SAVE-CERT              TO PBI-CERT-PRIME
           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X

           MOVE BWD-FIRST-NAME         TO PBI-I-INS-1ST-NAME
           MOVE BWD-MID-INIT           TO PBI-I-INS-MIDDLE-INIT
           MOVE BWD-LAST-NAME          TO PBI-I-INS-LAST-NAME
           MOVE WS-INSURED-AGE         TO PBI-I-INSURED-AGE
           MOVE 'M'                    TO PBI-I-INSURED-SEX

           STRING BWD-SSN (1:3) '-' BWD-SSN (4:2) '-'
              BWD-SSN (6:4) DELIMITED BY SIZE INTO PBI-I-SOC-SEC-NO
           END-STRING

           MOVE BWD-DOB                TO PBI-I-BIRTHDAY-X

           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '1'                    TO PBI-SEQUENCE
           MOVE BWD-ACCOUNT-NO         TO SR-ACCT-NO
           MOVE PBI-CERT-PRIME         TO SR-CERT-NO
           MOVE ' '                    TO SR-CERT-SFX

           PERFORM 0150-WRITE-DATA-CARD
                                      THRU 0150-EXIT
           .
       0080-EXIT.
           EXIT.

       0090-BUILD-SEQ-2.

           MOVE SPACES                 TO PBI-RECORD-BODY
           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME

           MOVE BWD-JT-LAST-NAME       TO PBI-I-JNT-LAST-NAME
           MOVE BWD-JT-MID-INIT        TO PBI-I-JNT-MIDDLE-INIT
           MOVE BWD-JT-FIRST-NAME      TO PBI-I-JNT-1ST-NAME

           IF BWD-JT-DOB NOT = ZEROS AND SPACES
              MOVE BWD-JT-DOB (5:2)    TO WB-YY
              MOVE BWD-JT-DOB (1:2)    TO WB-MM
              MOVE BWD-JT-DOB (3:2)    TO WB-DD
              IF WB-YY NOT < WORK-YR-IN
                 MOVE 19               TO WB-CC
              ELSE
                 MOVE 20               TO WB-CC
              END-IF
              MOVE WORK-BIRTH-DATE-N   TO DC-GREG-DATE-CYMD
              MOVE 'L'                 TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT
                                       THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE WS-BIN-EFF-DT    TO DC-BIN-DATE-2
                 MOVE '1'              TO DC-OPTION-CODE
                 PERFORM 8500-DATE-CONVERT
                                       THRU 8590-EXIT
                 IF NO-CONVERSION-ERROR
                    COMPUTE PBI-I-JOINT-AGE = 
                       DC-ELAPSED-MONTHS / +12
                 ELSE
                    DISPLAY ' BAD JT AGE CONVERT ' BWD-JT-DOB
                    '  ' BWD-LOAN-NO
                 END-IF
              ELSE
                 DISPLAY ' BAD JT BIRTH DATE ' BWD-JT-DOB '  '
                 'CERT = ' BWD-LOAN-NO
              END-IF
           END-IF

           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '2'                    TO PBI-SEQUENCE
           MOVE BWD-ACCOUNT-NO         TO SR-ACCT-NO
           MOVE PBI-CERT-PRIME         TO SR-CERT-NO
           MOVE ' '                    TO SR-CERT-SFX

           IF PBI-I-ISSUE-REC-SEQ-2 NOT = SPACES
              PERFORM 0150-WRITE-DATA-CARD
                                       THRU 0150-EXIT
           END-IF

           .
       0090-EXIT.
           EXIT.

       0100-BUILD-SEQ-3.

           MOVE SPACES                 TO PBI-RECORD-BODY

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME

           IF BWD-STATE = 'CO' OR 'IA'
              GO TO LIFE-TYPE-CHG-CO-IA
           END-IF

           IF BWD-STATE = 'KS' OR 'NE' OR 'OK' OR 'AZ' OR 'IL' OR
                              'MN' OR 'MO' OR 'SD' OR 'WI'
              GO TO LIFE-TYPE-CHG-OTHER
           END-IF

           GO TO LIFE-TYPE-CHG-EXIT

           .
       LIFE-TYPE-CHG-CO-IA.

           EVALUATE BWD-INS-CODE
              WHEN '01'
                 MOVE '86'             TO PBI-I-LF-BENEFIT-TYPE
              WHEN '02'
                 MOVE '84'             TO PBI-I-LF-BENEFIT-TYPE
              WHEN '03'
                 MOVE '85'             TO PBI-I-LF-BENEFIT-TYPE
              WHEN '04'
                 MOVE '87'             TO PBI-I-LF-BENEFIT-TYPE
              WHEN OTHER
                 MOVE '86'             TO PBI-I-LF-BENEFIT-TYPE
           END-EVALUATE

           GO TO LIFE-TYPE-CHG-EXIT

           .
       LIFE-TYPE-CHG-OTHER.

           EVALUATE BWD-INS-CODE
              WHEN '01'
                 MOVE 'NP'             TO PBI-I-LF-BENEFIT-TYPE
              WHEN '02'
                 MOVE 'SL'             TO PBI-I-LF-BENEFIT-TYPE
              WHEN '03'
                 MOVE 'JL'             TO PBI-I-LF-BENEFIT-TYPE
              WHEN '04'
                 MOVE 'JP'             TO PBI-I-LF-BENEFIT-TYPE
              WHEN OTHER
                 MOVE 'NP'             TO PBI-I-LF-BENEFIT-TYPE
           END-EVALUATE

           .
       LIFE-TYPE-CHG-EXIT.

           IF BWD-LF-BENEFIT-AMT NOT NUMERIC
              MOVE ZEROS               TO BWD-LF-BENEFIT-AMT
           END-IF
           
           IF BWD-FINAL-PMT NOT NUMERIC
              MOVE ZEROS               TO BWD-FINAL-PMT
           END-IF
           
      *    MOVE BWD-INS-CODE           TO PBI-I-LF-BENEFIT-TYPE
           MOVE BWD-INS-TERM           TO PBI-I-LF-TERM-X
           MOVE BWD-LF-BENEFIT-AMT     TO PBI-I-LF-BENEFIT-AMT
           MOVE BWD-LF-PREM            TO PBI-I-LF-PREM-AMT
      *    MOVE BWD-CRIT-PER           TO PBI-I-LF-CRIT-PERIOD

           MOVE BWD-LOAN-EXP-DATE      TO PBI-I-LF-EXPIRE-DT-X

           IF BWD-LEV-LF-PREM > ZEROS
              MOVE BWD-FINAL-PMT       TO PBI-I-LF-ALT-BENEFIT-AMT
              MOVE BWD-LEV-LF-PREM     TO PBI-I-LF-ALT-PREM-AMT
           END-IF
           
           ADD BWD-LF-PREM             TO TOT-LIFE-WRITTEN
                                          CF-LF-WRITTEN

           ADD BWD-LEV-LF-PREM         TO TOT-LIFE-WRITTEN
                                          CF-LF-WRITTEN

           IF PBI-I-LF-PREM-AMT = ZEROS
              MOVE ZEROS               TO PBI-I-LF-BENEFIT-AMT
           END-IF
      
           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '3'                    TO PBI-SEQUENCE
           MOVE BWD-ACCOUNT-NO         TO SR-ACCT-NO
           MOVE PBI-CERT-PRIME         TO SR-CERT-NO
           MOVE ' '                    TO SR-CERT-SFX

           IF (PBI-I-LF-PREM-AMT > ZEROS)
              AND (PBI-I-LF-BENEFIT-AMT > ZEROS)
              PERFORM 0150-WRITE-DATA-CARD
                                       THRU 0150-EXIT
           END-IF

           .
       0100-EXIT.
           EXIT.

       0110-BUILD-SEQ-4.

           MOVE SPACES                 TO PBI-RECORD-BODY

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME

           IF BWD-AH-BENEFIT-AMT NOT NUMERIC
              MOVE ZEROS               TO BWD-AH-BENEFIT-AMT
           END-IF
           
           MOVE BWD-AH-PREM            TO PBI-I-AH-PREM-AMT
           ADD BWD-AH-PREM             TO CF-AH-WRITTEN
                                          TOT-AH-WRITTEN

           MOVE BWD-AH-BENEFIT-AMT     TO PBI-I-AH-BENEFIT-AMT

           MOVE BWD-INS-TERM           TO PBI-I-AH-TERM-X

           IF BWD-STATE = 'CO'
              CONTINUE
           ELSE
              GO TO AH-TYPE-CHG-EXIT
           END-IF

           EVALUATE BWD-INS-CODE
              WHEN '001'
                 MOVE '11'             TO PBI-I-AH-BENEFIT-TYPE
              WHEN '054'
                 MOVE '59'             TO PBI-I-AH-BENEFIT-TYPE
              WHEN OTHER
                 MOVE '11'             TO PBI-I-AH-BENEFIT-TYPE
           END-EVALUATE

           .
       AH-TYPE-CHG-EXIT.

      *    MOVE BWD-INS-CODE           TO PBI-I-AH-BENEFIT-TYPE
      *    MOVE BWD-CRIT-PER           TO PBI-I-AH-CRIT-PERIOD

           EVALUATE BWD-INS-CODE
              WHEN '001'
                 MOVE '11'             TO PBI-I-AH-BENEFIT-TYPE
              WHEN '054'
                 MOVE '59'             TO PBI-I-AH-BENEFIT-TYPE
              WHEN OTHER
                 MOVE '01'             TO PBI-I-AH-BENEFIT-TYPE
           END-EVALUATE

           MOVE BWD-LOAN-EXP-DATE      TO PBI-I-AH-EXPIRE-DT-X

           MOVE '2'                       TO  PBI-TRANS-TYPE.
           MOVE '4'                       TO  PBI-SEQUENCE.
           MOVE BWD-ACCOUNT-NO         TO SR-ACCT-NO
           MOVE PBI-CERT-PRIME         TO SR-CERT-NO
           MOVE ' '                    TO SR-CERT-SFX

           IF (BWD-AH-PREM > ZEROS)
              AND (BWD-AH-BENEFIT-AMT > ZEROS)
              PERFORM 0150-WRITE-DATA-CARD
                                       THRU 0150-EXIT
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-BUILD-SEQ-5.

           MOVE SPACES                 TO PBI-RECORD-BODY

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME

           MOVE BWD-ENROLLER-CODE      TO PBI-I-LOAN-OFFICER

           MOVE BWD-LOAN-TERM          TO PBI-I-LOAN-TERM

           MOVE BWD-1ST-PMT-DATE       TO PBI-I-1ST-PMT-DT

           IF BWD-APR1 NOT NUMERIC
              MOVE ZEROS               TO BWD-APR1
           END-IF

           MOVE BWD-APR1               TO PBI-I-LOAN-APR

           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '5'                    TO PBI-SEQUENCE
           MOVE BWD-ACCOUNT-NO         TO SR-ACCT-NO
           MOVE PBI-CERT-PRIME         TO SR-CERT-NO
           MOVE ' '                    TO SR-CERT-SFX

           PERFORM 0150-WRITE-DATA-CARD
                                       THRU 0150-EXIT

           .
       0120-EXIT.
           EXIT.

       0122-BUILD-SEQ-6.

           MOVE SPACES                 TO PBI-RECORD-BODY

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME

           MOVE BWD-ADDR1              TO PBI-I-INSURED-ADDRESS-1

           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '6'                    TO PBI-SEQUENCE
           MOVE BWD-ACCOUNT-NO         TO SR-ACCT-NO
           MOVE PBI-CERT-PRIME         TO SR-CERT-NO
           MOVE ' '                    TO SR-CERT-SFX

           IF PBI-I-ISSUE-REC-SEQ-6 NOT = SPACES
              PERFORM 0150-WRITE-DATA-CARD
                                       THRU 0150-EXIT
           END-IF

           .
       0122-EXIT.
           EXIT.

       0125-BUILD-SEQ-7.

           MOVE SPACES                 TO PBI-RECORD-BODY

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME

051810     MOVE BWD-CITY               TO PBI-I-INSURED-CITY
051810     MOVE BWD-ADDR-STATE         TO PBI-I-INSURED-STATE
051810*    STRING BWD-CITY ', ' BWD-ADDR-STATE DELIMITED BY SIZE
051810*       INTO PBI-I-INSURED-CITY-STATE
051810*    END-STRING
           MOVE BWD-ZIP                TO PBI-I-INSURED-ZIP-CODE

           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '7'                    TO PBI-SEQUENCE
           MOVE BWD-ACCOUNT-NO         TO SR-ACCT-NO
           MOVE PBI-CERT-PRIME         TO SR-CERT-NO
           MOVE ' '                    TO SR-CERT-SFX

           IF PBI-I-ISSUE-REC-SEQ-7 NOT = SPACES
              PERFORM 0150-WRITE-DATA-CARD
                                       THRU 0150-EXIT
           END-IF

           .
       0125-EXIT.
           EXIT.

       0130-BUILD-CANCEL.

           MOVE SPACES                 TO PBI-RECORD-BODY
           ADD 1                       TO LOGIC-CANC-CNT
           ADD 1                       TO CANC-CNT

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE BWD-PRIOR-CERT-NO      TO PBI-CERT-PRIME
      *    MOVE SAVE-CERT              TO PBI-CERT-PRIME

           MOVE BWD-LAST-NAME          TO PBI-C-INSURED-NAME

           COMPUTE PBI-C-LF-PREM-REFUND = BWD-LF-REFUND +
              BWD-LEV-LF-REFUND
              
           MOVE BWD-AH-REFUND          TO PBI-C-AH-PREM-REFUND
           
           IF PBI-C-LF-PREM-REFUND > ZEROS
              MOVE BWD-CANCEL-DATE     TO PBI-C-LF-CANCEL-DATE
           END-IF

           IF PBI-C-AH-PREM-REFUND > ZEROS
              MOVE BWD-CANCEL-DATE     TO PBI-C-AH-CANCEL-DATE
           END-IF

           MOVE '3'                    TO PBI-C-TRANS-TYPE
           MOVE '1'                    TO PBI-C-SEQUENCE
           MOVE BWD-ACCOUNT-NO         TO SR-ACCT-NO
           MOVE PBI-CERT-PRIME         TO SR-CERT-NO
           MOVE ' '                    TO SR-CERT-SFX

      *    ADD 1                       TO CF-CERT-CANC
           ADD PBI-C-LF-PREM-REFUND    TO CF-LF-CANC
           ADD PBI-C-AH-PREM-REFUND    TO CF-AH-CANC

           ADD 1                       TO TOT-CERT-CANC
           ADD PBI-C-LF-PREM-REFUND    TO TOT-LIFE-CANC
           ADD PBI-C-AH-PREM-REFUND    TO TOT-AH-CANC

           IF (PBI-C-LF-PREM-REFUND > ZEROS)
              OR (PBI-C-AH-PREM-REFUND > ZEROS)
              ADD 1                       TO CF-CERT-CANC
              PERFORM 0150-WRITE-DATA-CARD
                                       THRU 0150-EXIT

              MOVE SPACES              TO PBI-RECORD-BODY

              MOVE SAVE-EFF-DATE       TO PBI-CERT-EFF-DT-X
              MOVE BWD-PRIOR-CERT-NO   TO PBI-CERT-PRIME

              MOVE BWD-ENROLLER-NAME (1:14)
                                       TO PBI-C-BOW-LOAN-NUMBER

              MOVE '3'                 TO PBI-C-TRANS-TYPE
              MOVE '2'                 TO PBI-C-SEQUENCE
              MOVE BWD-ACCOUNT-NO      TO SR-ACCT-NO
              MOVE PBI-CERT-PRIME      TO SR-CERT-NO
              MOVE ' '                 TO SR-CERT-SFX

              IF PBI-C-BOW-LOAN-NUMBER NOT = SPACES
                 PERFORM 0150-WRITE-DATA-CARD
                                       THRU 0150-EXIT
              END-IF
           END-IF

           .
       0130-EXIT.
           EXIT.
           

       0150-WRITE-DATA-CARD.

           ADD 1                       TO DETAIL-CNT
                                          DETAIL-CNT-TOT

           MOVE PBI-RECORD-BODY        TO OUT-RECORD (1:80)
      *    MOVE  DETAIL-CNT            TO SR-REC-CNT
      *    MOVE ZEROS                  TO CF-BATCH-NUMB
      *    MOVE CF-BATCH-NUMB          TO SR-BATCH-NUMB

           WRITE CARD-RECORD           FROM OUT-RECORD
           .
       0150-EXIT.
           EXIT.

       0160-ACCOUNT-BREAK.

            MOVE '0'                   TO CF-SEQUENCE-NO
            MOVE SAVE-FULL-ACCT        TO CF-ACCT-NO
            MOVE SAVE-STATE            TO CF-STATE
            MOVE '1'                   TO CF-TRANS-TYPE
            MOVE '0'                   TO CF-SEQUENCE-NO
            MOVE 'CSO'                 TO CF-CLIENT-ID
            MOVE '9000000'             TO CF-CARR-CO

            IF SAVE-FULL-ACCT = ZEROS OR SPACES
               MOVE ZEROS              TO CF-BATCH-NUMB
               DISPLAY ' '
               DISPLAY 'COMFED-BATCH-HDR IS ZEROS  ' COMFED-BATCH-HDR
               GO TO SKIP-WRITE
            END-IF

            MOVE ZEROS                 TO CF-BATCH-NUMB
            MOVE COMFED-BATCH-HDR      TO OUT-RECORD
      *     MOVE CF-BATCH-NUMB         TO SR-BATCH-NUMB
      *     MOVE ZEROS                 TO SR-REC-CNT
            MOVE SAVE-FULL-ACCT        TO SR-ACCT-NO
            MOVE ZEROS                 TO SR-CERT-NO

            ADD 1                      TO HDR-CNT

            IF (CF-LF-WRITTEN          > ZEROS)
               OR (CF-LF-CANC          > ZEROS)
               OR (CF-AH-WRITTEN       > ZEROS)
               OR (CF-AH-CANC          > ZEROS)
               WRITE CARD-RECORD       FROM OUT-RECORD
            END-IF

           .
       SKIP-WRITE.

           DISPLAY ' '
           DISPLAY 'COMFED-BATCH-HDR  ' COMFED-BATCH-HDR

      *    ADD 1                      TO CF-BATCH-NO
           MOVE ZEROS                 TO CF-LF-WRITTEN
                                          CF-LF-CANC
                                          CF-AH-WRITTEN
                                          CF-AH-CANC
                                          CF-CERT-ISS
                                          CF-CERT-CANC
                                          CERT-CNT
                                          CANC-CNT
                                          DETAIL-CNT

           MOVE  BWD-STATE             TO SAVE-STATE

           MOVE  BWD-ACCOUNT-NO        TO SAVE-FULL-ACCT

           .
       0160-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX'              USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.

