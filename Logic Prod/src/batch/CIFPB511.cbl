       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIFPB511.
       AUTHOR.     PABLO.
       DATE-COMPILED.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 051606   CR2006050500001 PEMA  NEW PROGRAM
021109* 021109   CR2009010500002 PEMA  REMOVE LOAN TERM MOVE STMT
032109* 032109     2009021700002 PEMA  ADD CRED BENE TO PNDB REC
030612* 030612   IR2012030600001 PEMA  ADD A&H ONLY BENEFIT CODES
110712* 110712   CR2011071400001 PEMA  A&H ONLY IN ACCT 0001107201
120312* 120312   IR2012120300004 PEMA  PASS DOB TO OUTPUT
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  FPB-IN              ASSIGN TO SYS010.
      *       ORGANIZATION IS LINE SEQUENTIAL.
           SELECT  DISK-DATE           ASSIGN TO SYS019.
           SELECT  FPB-OUT             ASSIGN TO SYS012.
           SELECT SORT-FILE        ASSIGN TO SYS001-UT-3380-S-SORTWK1.

       DATA DIVISION.
       FILE SECTION.

       FD  FPB-IN
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  FPB-IN-RECORD.
           05  FPB-RECORD-TYPE         PIC X.
               88  FPB-VALID-REC-1        VALUE '1'.      
               88  FPB-VALID-REC-2        VALUE '2'.
               88  FPB-VALID-REC-3        VALUE '3'.
               88  FPB-VALID-REC-4        VALUE '4'.
               88  FPB-VALID-REC-5        VALUE '5'.
               88  FPB-VALID-REC-7        VALUE '7'.
               88  FPB-VALID-REC-9        VALUE '9'.
           05  FILLER                  PIC XXX.
           05  FPB-INST-NO             PIC 99.
           05  FPB-ACCT-NO             PIC X(10).
           05  FILLER                  PIC X(920).

       FD  DISK-DATE
                                       COPY ELCDTEFD.
       FD  FPB-OUT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.

       01  CARD-RECORD                 PIC X(91).

       SD  SORT-FILE.                                               
                                                                    
       01  SORT-RECORD.
           12  SR-PENDING-RECORD.
               16  SR-CERT-NO              PIC X(11).
               16  F                       PIC X(67).
               16  SR-REC-TYPE             PIC XX.
           12  SR-CONTROL                  PIC X(10).
           12  SR-BATCH-HDR-IND            PIC X.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.  00133388
       77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  00133389
       77  FILLER  PIC X(32) VALUE '********************************'.  00133390
       77  W-DISPLAY-DATE        PIC 9(9) VALUE ZEROS.
       77  WS-SAVE-ACCT-NO             PIC X(10) VALUE LOW-VALUES.
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
           88  END-OF-FPB                       VALUE 'Y'.
       77  WS-RETURN-CODE         PIC S9(3)     VALUE ZERO.
       77  WS-ABEND-MESSAGE       PIC X(80)     VALUE SPACES.
       77  SAVE-EFF-DATE          PIC X(6)      VALUE SPACES.
       77  WS-BIN-1ST-PAY-DT      PIC XX        VALUE LOW-VALUES.
       77  WS-BIN-EFF-DT          PIC XX        VALUE LOW-VALUES.
       77  WS-BIN-BIRTH-DT        PIC XX        VALUE LOW-VALUES.
       77  WS-INSURED-AGE         PIC 999       VALUE ZEROS.
       77  RECS-IN                PIC 9(7)      VALUE ZEROS.
       77  RECS-RET               PIC 9(7)      VALUE ZEROS.
       77  RECS-REL               PIC 9(7)      VALUE ZEROS.
       77  WS-ABEND-FILE-STATUS   PIC XX        VALUE ZERO.
       77  WS-ZERO                PIC S9        VALUE ZERO.
       77  WS-ISS-SW              PIC X         VALUE ' '.
           88  ISSUED-CERT                      VALUE 'Y'.
       77  WS-CAN-SW              PIC X         VALUE ' '.
           88  CANCELLED-CERT                   VALUE 'Y'.

       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  S2                          PIC S999 COMP-3 VALUE +0.
       77  WS-LF-BENEFIT-CD            PIC XX        VALUE '  '.
       77  WS-AH-BENEFIT-CD            PIC XX        VALUE '  '.
       77  WS-LF-PREM                  PIC 9(9)V99   VALUE ZEROS.
       77  WS-AH-PREM                  PIC 9(9)V99   VALUE ZEROS.
       77  WS-LF-PREM-ALT              PIC 9(9)V99   VALUE ZEROS.
       77  WS-LF-BALLOON               PIC 9(11)V99  VALUE ZEROS.
       77  WS-LF-REF                   PIC 9(9)V99   VALUE ZEROS.
       77  WS-AH-REF                   PIC 9(9)V99   VALUE ZEROS.
       77  WS-LF-REF-DATE              PIC X(6)      VALUE ZEROS.
       77  WS-AH-REF-DATE              PIC X(6)      VALUE ZEROS.
       77  WS-PREV-SR-ACCOUNT          PIC X(10)     VALUE LOW-VALUES.
       77  WS-AH-ONLY-SW               PIC X         VALUE ' '.
           88  FOUND-AH-ONLY                   VALUE 'Y'.

       01  BRANCH-RESP-TABLE.
           05  LOAN-OFFICERS.
               10  FILLER              PIC X(5)  VALUE '11001'.
               10  FILLER              PIC X(5)  VALUE '13445'.
               10  FILLER              PIC X(5)  VALUE '13645'.
               10  FILLER              PIC X(5)  VALUE '13745'.
               10  FILLER              PIC X(5)  VALUE '13917'.
               10  FILLER              PIC X(5)  VALUE '14312'.
               10  FILLER              PIC X(5)  VALUE '14413'.
               10  FILLER              PIC X(5)  VALUE '14518'.
               10  FILLER              PIC X(5)  VALUE '14821'.
               10  FILLER              PIC X(5)  VALUE '15117'.
               10  FILLER              PIC X(5)  VALUE '15319'.
               10  FILLER              PIC X(5)  VALUE '20117'.
               10  FILLER              PIC X(5)  VALUE '21118'.
               10  FILLER              PIC X(5)  VALUE '22417'.
               10  FILLER              PIC X(5)  VALUE '22617'.
               10  FILLER              PIC X(5)  VALUE '22714'.
               10  FILLER              PIC X(5)  VALUE '23217'.
               10  FILLER              PIC X(5)  VALUE '23616'.
               10  FILLER              PIC X(5)  VALUE '23720'.
               10  FILLER              PIC X(5)  VALUE '24207'.
               10  FILLER              PIC X(5)  VALUE '24605'.
               10  FILLER              PIC X(5)  VALUE '25217'.
               10  FILLER              PIC X(5)  VALUE '26818'.
               10  FILLER              PIC X(5)  VALUE '26918'.
               10  FILLER              PIC X(5)  VALUE '27014'.
               10  FILLER              PIC X(5)  VALUE '27117'.
               10  FILLER              PIC X(5)  VALUE '27501'.
               10  FILLER              PIC X(5)  VALUE '27620'.
               10  FILLER              PIC X(5)  VALUE '27712'.
               10  FILLER              PIC X(5)  VALUE '30513'.
               10  FILLER              PIC X(5)  VALUE '31017'.
               10  FILLER              PIC X(5)  VALUE '31521'.
               10  FILLER              PIC X(5)  VALUE '32013'.
               10  FILLER              PIC X(5)  VALUE '36001'.
               10  FILLER              PIC X(5)  VALUE '42516'.
               10  FILLER              PIC X(5)  VALUE '43016'.
               10  FILLER              PIC X(5)  VALUE '48116'.
               10  FILLER              PIC X(5)  VALUE '48512'.
               10  FILLER              PIC X(5)  VALUE '50019'.
               10  FILLER              PIC X(5)  VALUE '50503'.
               10  FILLER              PIC X(5)  VALUE '50710'.
               10  FILLER              PIC X(5)  VALUE '51521'.
               10  FILLER              PIC X(5)  VALUE '53806'.
               10  FILLER              PIC X(5)  VALUE '54206'.
               10  FILLER              PIC X(5)  VALUE '54805'.
               10  FILLER              PIC X(5)  VALUE '60718'.
               10  FILLER              PIC X(5)  VALUE '60821'.
               10  FILLER              PIC X(5)  VALUE '60901'.
               10  FILLER              PIC X(5)  VALUE '63407'.
               10  FILLER              PIC X(5)  VALUE '63801'.
               10  FILLER              PIC X(5)  VALUE '64101'.
               10  FILLER              PIC X(5)  VALUE '64614'.
               10  FILLER              PIC X(5)  VALUE '70105'.
               10  FILLER              PIC X(5)  VALUE '73505'.
               10  FILLER              PIC X(5)  VALUE '74105'.
               10  FILLER              PIC X(5)  VALUE '74405'.
               10  FILLER              PIC X(5)  VALUE '81005'.
               10  FILLER              PIC X(5)  VALUE '90501'.
           05  LOAN-OFFS REDEFINES LOAN-OFFICERS OCCURS 58
                         ASCENDING KEY RESP-CODE
                         INDEXED BY S3.
               10  RESP-CODE           PIC XXX.
               10  BRANCH-CODE         PIC XX.


       01  WS-WORK-APR.
           05  WS-WORK-APR-DOLLARS       PIC 9(6).
           05  WS-WORK-APR-CENTS         PIC 9(6).
       01  WS-WORK-APR-N REDEFINES WS-WORK-APR
                                         PIC 9(6)V9(6).

       01  WS-WORK-PREM-1.
           05  WS-WORK-PREM-DOLLARS-1    PIC 9(9).
           05  WS-WORK-PREM-CENTS-1      PIC 99.
       01  WS-WORK-PREM-N-1 REDEFINES WS-WORK-PREM-1
                                         PIC 9(9)V99.

       01  WS-WORK-PREM-2.
           05  WS-WORK-PREM-DOLLARS-2    PIC 9(9).
           05  WS-WORK-PREM-CENTS-2      PIC 99.
       01  WS-WORK-PREM-N-2 REDEFINES WS-WORK-PREM-2
                                         PIC 9(9)V99.

       01  WS-WORK-AMT.
           05  WS-WORK-AMT-DOLLARS       PIC 9(9).
           05  WS-WORK-AMT-CENTS         PIC 99.
       01  WS-WORK-AMT-N   REDEFINES WS-WORK-AMT
                                         PIC 9(9)V99.

       01  WS-WORK-AMT-2.
           05  WS-WORK-AMT-DOLLARS-2     PIC 9(9).
           05  WS-WORK-AMT-CENTS-2       PIC 99.
       01  WS-WORK-AMT-N-2 REDEFINES WS-WORK-AMT-2
                                         PIC 9(9)V99.

       01  WS-WORK-REFUND-1.
           05  WS-WORK-REF-DOLLARS-1   PIC 9(9).
           05  WS-WORK-REF-CENTS-1     PIC 99.
       01  WS-WORK-REFUND-N-1 REDEFINES WS-WORK-REFUND-1
                                         PIC 9(9)V99.

       01  WS-WORK-REFUND-2.
           05  WS-WORK-REF-DOLLARS-2   PIC 9(9).
           05  WS-WORK-REF-CENTS-2     PIC 99.
       01  WS-WORK-REFUND-N-2 REDEFINES WS-WORK-REFUND-2
                                         PIC 9(9)V99.

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
           05  OUT-SEQUENCE       PIC X         VALUE SPACES.           00133507
           05  OUT-ACCOUNT-NO     PIC X(10)     VALUE SPACES.
           05  OUT-BATCH-HDR-IND  PIC X         VALUE SPACES.

       01  FPB-BATCH-NO.
           05  SR-BATCH-NUMB.                                           00133508
               10  SR-BATCH-ID    PIC XX        VALUE ZEROS.            00133508
               10  SR-BATCH-DA    PIC XX        VALUE ZEROS.            00133508
               10  SR-BATCH-NO    PIC 99        VALUE ZEROS.            00133508
           05  SR-REC-CNT         PIC 9(4)      VALUE ZEROS.            00133509
                                                                        00133510
       01  SAVE-FULL-ACCT.                                              00133511
           05 SAVE-ACCT-7         PIC X(07)     VALUE SPACES.           00133512
           05 SAVE-ACCT           PIC XXX       VALUE SPACES.           00133513
                                                                        00133514
       01  FPB-BATCH-HDR.
           05 FP-CARR-CO.                                               00133516
               10 FP-CARR          PIC X         VALUE '9'.             00133517
               10 FP-COMP          PIC X(6)      VALUE '000000'.        00133518
           05 FP-STATE             PIC XX        VALUE 'SD'.            00133519
           05 FP-ACCT-NO           PIC X(10)     VALUE SPACES.
           05 FP-BATCH-NUMB.                                            00133521
               10 FP-BATCH-ID      PIC XX        VALUE ZEROS.           00133521
               10 FP-BATCH-DA      PIC XX        VALUE ZEROS.           00133521
               10 FP-BATCH-NO      PIC 99        VALUE ZEROS.           00133521
           05 FP-HD-EFF-DT.                                             00133522
               10 FP-HD-EFF-MO     PIC XX        VALUE SPACES.          00133523
               10 FP-HD-EFF-DA     PIC XX        VALUE SPACES.          00133524
               10 FP-HD-EFF-YR     PIC XX        VALUE SPACES.          00133525
           05 FP-CERT-ISS          PIC S9(4)     VALUE ZEROS.           00133526
           05 FP-LF-WRITTEN        PIC S9(7)V99  VALUE ZEROS.           00133527
           05 FP-AH-WRITTEN        PIC S9(7)V99  VALUE ZEROS.           00133528
           05 FP-CERT-CANC         PIC S9(4)     VALUE ZEROS.           00133529
           05 FP-LF-CANC           PIC S9(7)V99  VALUE ZEROS.           00133530
           05 FP-AH-CANC           PIC S9(7)V99  VALUE ZEROS.           00133531
           05 FP-CLIENT-ID         PIC X(3)      VALUE 'CID'.           00133532
           05 FP-TRANS-TYPE        PIC X         VALUE '1'.             00133533
           05 FP-SEQUENCE-NO       PIC X         VALUE '0'.             00133534
           05 FP-ACCOUNT-NO        PIC X(10)  VALUE SPACES.
           05 FP-BATCH-HDR-IND     PIC X      VALUE ' '.
                                                                        00133535
                                                                        00133621
       01  WS-BIRTH-DT                 PIC 9(6).
120312 01  ws-birth-dt-x redefines ws-birth-dt pic x(6).
       01  WS-1ST-PMT-DT               PIC 9(6).

       01  WS-WORK-DATE            PIC X(8).
       01  WS-WORK-DATE-N REDEFINES WS-WORK-DATE
                                   PIC 9(8).
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
                                       COPY ERCFPBRL.
                                       COPY ELCDATE.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
                                                                        00133669
      ******************************************************************00133673
       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

           OPEN INPUT FPB-IN

           ACCEPT WORK-DATE-IN         FROM DATE
           DISPLAY ' WORK DATE IN ' WORK-DATE-IN
           
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'
           DISPLAY '*   CURRENT DATE IS -- ' WS-CURRENT-DATE
           MOVE RUN-DATE               TO W-DISPLAY-DATE
           DISPLAY ' RUN DATE IS ' W-DISPLAY-DATE
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'

           SORT SORT-FILE
               ON ASCENDING KEY SR-CONTROL SR-CERT-NO SR-REC-TYPE
                   INPUT PROCEDURE  0000-SORT-INPUT-PROCEDURE
                   OUTPUT PROCEDURE 0001-SORT-OUTPUT-PROCEDURE.

           PERFORM 0015-FINALIZE       THRU 0015-EXIT

           DISPLAY ' RECORDS INPUT      ' RECS-IN
           DISPLAY ' RECORDS RELEASED   ' RECS-REL
           DISPLAY ' RECORDS RETURNED   ' RECS-RET

           GOBACK

           .
       0000-SORT-INPUT-PROCEDURE SECTION.

           PERFORM 0040-READ-FPB       THRU 0040-EXIT

           PERFORM 0030-PROCESS-FPB    THRU 0030-EXIT UNTIL
              END-OF-FPB

           PERFORM 0045-WRITE-PENDING-RECS
                                       THRU 0045-EXIT

           .
       0001-SORT-OUTPUT-PROCEDURE SECTION.

           OPEN OUTPUT FPB-OUT
           PERFORM 0025-RETURN-FPB     THRU 0025-EXIT
           MOVE ' ' TO WS-EOF-SW
           MOVE SR-CONTROL             TO WS-PREV-SR-ACCOUNT
           PERFORM 0020-PROCESS        THRU 0020-EXIT UNTIL
              END-OF-FPB

           .
       0015-FINALIZE SECTION.

           PERFORM 0160-ACCOUNT-BREAK  THRU 0160-EXIT
           CLOSE FPB-IN FPB-OUT

           .
       0015-EXIT.
           EXIT.

       0020-PROCESS.

           IF SR-CONTROL NOT = WS-PREV-SR-ACCOUNT
              PERFORM 0160-ACCOUNT-BREAK THRU 0160-EXIT
              MOVE SR-CONTROL          TO WS-PREV-SR-ACCOUNT
           END-IF
           EVALUATE TRUE
              WHEN SR-REC-TYPE = '21'
                 ADD +1 TO FP-CERT-ISS
              WHEN SR-REC-TYPE = '23'
                 MOVE SR-PENDING-RECORD TO PBI-RECORD-BODY
                 COMPUTE FP-LF-WRITTEN = FP-LF-WRITTEN +
                    PBI-I-LF-PREM-AMT + PBI-I-LF-ALT-PREM-AMT
              WHEN SR-REC-TYPE = '24'
                 MOVE SR-PENDING-RECORD TO PBI-RECORD-BODY
                 COMPUTE FP-AH-WRITTEN = FP-AH-WRITTEN +
                    PBI-I-AH-PREM-AMT
              WHEN SR-REC-TYPE = '31'
                 ADD +1 TO FP-CERT-CANC
                 MOVE SR-PENDING-RECORD TO PBI-RECORD-BODY
                 COMPUTE FP-LF-CANC = FP-LF-CANC +
                    PBI-C-LF-PREM-REFUND
                 COMPUTE FP-AH-CANC = FP-AH-CANC +
                    PBI-C-AH-PREM-REFUND
           END-EVALUATE

           WRITE CARD-RECORD           FROM SORT-RECORD

           PERFORM 0025-RETURN-FPB     THRU 0025-EXIT

           .
       0020-EXIT.
           EXIT.

       0025-RETURN-FPB.

           RETURN SORT-FILE AT END
                SET END-OF-FPB         TO TRUE
           END-RETURN

           IF NOT END-OF-FPB
              ADD 1                    TO RECS-RET
           END-IF

           .
       0025-EXIT.
           EXIT.

       0030-PROCESS-FPB.

           IF NOT FPB-VALID-REC-7
              CONTINUE
           ELSE
              IF FPB-ACCT-NO NOT = WS-SAVE-ACCT-NO
                 PERFORM 0045-WRITE-PENDING-RECS
                                       THRU 0045-EXIT
                 MOVE FPB-IN-RECORD    TO FIRST-PREMIER-INPUT
                 MOVE FPB-ACCT-NO      TO WS-SAVE-ACCT-NO
                 PERFORM 0070-BUILD-COMMON
                                       THRU 0070-EXIT
              END-IF
              MOVE FPB-IN-RECORD       TO FIRST-PREMIER-INPUT
              IF FP7-CANC-DATE NOT = SPACES
                 PERFORM 0060-PROCESS-CANCELS
                                       THRU 0060-EXIT
              ELSE
                 PERFORM 0055-PROCESS-ISSUES
                                       THRU 0055-EXIT
              END-IF
           END-IF

           PERFORM 0040-READ-FPB       THRU 0040-EXIT

           .
       0030-EXIT.
           EXIT.

       0040-READ-FPB.

           READ FPB-IN AT END
                SET END-OF-FPB         TO TRUE
           END-READ

           IF NOT END-OF-FPB
              ADD 1                    TO RECS-IN
           END-IF

           .
       0040-EXIT.
           EXIT.

       0045-WRITE-PENDING-RECS.

           IF (WS-LF-PREM > ZEROS)
              OR (WS-AH-PREM > ZEROS)
              PERFORM 0050-BUILD-ISSUE-REC
                                       THRU 0050-EXIT
           ELSE
              IF (WS-LF-REF > ZEROS)
                 OR (WS-AH-REF > ZEROS)
                 PERFORM 0065-BUILD-CANCEL-REC
                                       THRU 0065-EXIT
              END-IF
           END-IF

           .
       0045-EXIT.
           EXIT.
           
           
       0050-BUILD-ISSUE-REC.

      *    IF (WS-LF-PREM = ZEROS)
      *       AND (WS-AH-PREM > ZEROS)
      *       DISPLAY ' FOUND A&H ONLY USING 1107201 '
      *       MOVE '0001107201'        TO OUT-ACCOUNT-NO
      *    ELSE
      *       MOVE '0001107200'        TO OUT-ACCOUNT-NO
      *    END-IF
           move '0001107200'           to out-account-no
           if found-ah-only
              move '0001107201'        to out-account-no
              DISPLAY ' FOUND A&H ONLY USING 1107201 ' save-cert
           end-if
           PERFORM 0080-BUILD-SEQ-1    THRU 0080-EXIT
           PERFORM 0090-BUILD-SEQ-2    THRU 0090-EXIT
           PERFORM 0100-BUILD-SEQ-3    THRU 0100-EXIT
           PERFORM 0110-BUILD-SEQ-4    THRU 0110-EXIT
           PERFORM 0120-BUILD-SEQ-5    THRU 0120-EXIT
           PERFORM 0122-BUILD-SEQ-6    THRU 0122-EXIT
           PERFORM 0125-BUILD-SEQ-7    THRU 0125-EXIT
           PERFORM 0127-BUILD-SEQ-8    THRU 0127-EXIT

           .
       0050-EXIT.
           EXIT.

       0055-PROCESS-ISSUES.

           DISPLAY ' FOUND ISSUE '
           IF FP7-INS-TABLE NOT NUMERIC
              MOVE ZEROS               TO FP7-INS-TABLE
           END-IF

           IF FP7-BALLOON-AMT NOT NUMERIC
              MOVE ZEROS               TO FP7-BALLOON-AMT
           END-IF

           IF FP7-PREMIUM NOT NUMERIC
              MOVE ZEROS               TO FP7-PREMIUM
           END-IF

           EVALUATE TRUE
              WHEN FP7-INS-TABLE = 51
                 MOVE FP7-PREMIUM      TO WS-LF-PREM
                 MOVE '22'             TO WS-LF-BENEFIT-CD
              WHEN FP7-INS-TABLE = 52
                 MOVE FP7-PREMIUM      TO WS-LF-PREM
                 MOVE '23'             TO WS-LF-BENEFIT-CD
              WHEN (FP7-INS-TABLE = 53)
                   AND (WS-LF-PREM NOT = ZEROS)
                 MOVE FP7-PREMIUM      TO WS-LF-PREM-ALT
                 MOVE FP7-BALLOON-AMT  TO WS-LF-BALLOON
                 MOVE '62'             TO WS-LF-BENEFIT-CD
              WHEN (FP7-INS-TABLE = 54)
                   AND (WS-LF-PREM NOT = ZEROS)
                 MOVE FP7-PREMIUM      TO WS-LF-PREM-ALT
                 MOVE FP7-BALLOON-AMT  TO WS-LF-BALLOON
                 MOVE '63'             TO WS-LF-BENEFIT-CD
              WHEN (FP7-INS-TABLE = 53)
                   AND (WS-LF-PREM = ZEROS)
                 DISPLAY ' FOUND PURE LEVEL ' FP7-ACCOUNT-NUMBER
                 MOVE FP7-PREMIUM      TO WS-LF-PREM
      *          MOVE FP7-BALLOON-AMT  TO WS-LF-BALLOON
                 MOVE '24'             TO WS-LF-BENEFIT-CD
              WHEN (FP7-INS-TABLE = 54)
                   AND (WS-LF-PREM = ZEROS)
                 DISPLAY ' FOUND PURE LEVEL ' FP7-ACCOUNT-NUMBER
                 MOVE FP7-PREMIUM      TO WS-LF-PREM
      *          MOVE FP7-BALLOON-AMT  TO WS-LF-BALLOON
                 MOVE '25'             TO WS-LF-BENEFIT-CD
              WHEN FP7-INS-TABLE = 55
                 MOVE FP7-PREMIUM      TO WS-AH-PREM
                 MOVE '03'             TO WS-AH-BENEFIT-CD
              WHEN FP7-INS-TABLE = 57
                 MOVE FP7-PREMIUM      TO WS-AH-PREM
                 MOVE '03'             TO WS-AH-BENEFIT-CD
                 set found-ah-only to true
              WHEN FP7-INS-TABLE = 56
                 MOVE FP7-PREMIUM      TO WS-AH-PREM
                 MOVE '53'             TO WS-AH-BENEFIT-CD
              WHEN FP7-INS-TABLE = 58
                 MOVE FP7-PREMIUM      TO WS-AH-PREM
                 MOVE '53'             TO WS-AH-BENEFIT-CD
                 set found-ah-only to true
              WHEN OTHER
                 DISPLAY ' INVALID OR UNKNOWN INS TABLE '
                    FP7-INS-TABLE
                 PERFORM ABEND-PGM
           END-EVALUATE                 

           .
       0055-EXIT.
           EXIT.

       0060-PROCESS-CANCELS.

           IF FP7-INS-TABLE NOT NUMERIC
              MOVE ZEROS               TO FP7-INS-TABLE
           END-IF

           MOVE FP7-CANC-DATE (7:4)    TO WS-WORK-DATE (1:4)
           MOVE FP7-CANC-DATE (1:2)    TO WS-WORK-DATE (5:2)
           MOVE FP7-CANC-DATE (4:2)    TO WS-WORK-DATE (7:2)
           IF WS-WORK-DATE-N NOT = ZEROS
              MOVE WS-WORK-DATE-N         TO DC-GREG-DATE-CYMD
              MOVE 'L'                    TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 CONTINUE
      *          MOVE DC-GREG-DATE-1-MDY  TO WS-REF-DATE
              ELSE
                 DISPLAY ' BAD REFUND DATE ' FP7-CANC-DATE
                 ' CERT = ' FP7-ACCOUNT-NUMBER
              END-IF
           END-IF

           EVALUATE TRUE
              WHEN FP7-INS-TABLE = 51 OR 52 OR 53 OR 54
                 COMPUTE WS-LF-REF = WS-LF-REF + FP7-CANC-AMT
                 MOVE DC-GREG-DATE-1-MDY
                                       TO WS-LF-REF-DATE
              WHEN FP7-INS-TABLE = 55 OR 56
                 MOVE FP7-CANC-AMT     TO WS-AH-REF
                 MOVE DC-GREG-DATE-1-MDY
                                       TO WS-AH-REF-DATE
              WHEN FP7-INS-TABLE = 57 or 58
                 set found-ah-only to true
                 MOVE FP7-CANC-AMT     TO WS-AH-REF
                 MOVE DC-GREG-DATE-1-MDY
                                       TO WS-AH-REF-DATE
              WHEN OTHER
                 DISPLAY ' INVALID OR UNKNOWN INS TABLE '
                    FP7-INS-TABLE
                 PERFORM ABEND-PGM
           END-EVALUATE

           .
       0060-EXIT.
           EXIT.

       0065-BUILD-CANCEL-REC.

           move '0001107200'           to out-account-no
           if found-ah-only
              move '0001107201'        to out-account-no
              DISPLAY ' FOUND A&H ONLY CAN USING 1107201 ' save-cert
           end-if
           PERFORM 0130-BUILD-CANCEL   THRU 0130-EXIT

           .
       0065-EXIT.
           EXIT.

       0070-BUILD-COMMON.

      *    DISPLAY FIRST-PREMIER-INPUT
           MOVE ' '                    TO WS-ISS-SW
                                          WS-CAN-SW

           MOVE ZEROS                  TO WS-BIRTH-DT
                                          WS-1ST-PMT-DT

      *    DISPLAY ' NOTE DATE ' FP7-NOTE-DATE
           MOVE FP7-NOTE-DATE (7:4)    TO WS-WORK-DATE (1:4)
           MOVE FP7-NOTE-DATE (1:2)    TO WS-WORK-DATE (5:2)
           MOVE FP7-NOTE-DATE (4:2)    TO WS-WORK-DATE (7:2)
           MOVE WS-WORK-DATE-N         TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-EFF-DT
              MOVE DC-GREG-DATE-1-MDY  TO SAVE-EFF-DATE
           ELSE
              DISPLAY ' BAD EFFECT  DT ' WS-WORK-DATE '  '
              'CERT = ' FP7-ACCOUNT-NUMBER
           END-IF

           MOVE FP7-LASN-ORIG-1ST-PMT-DT (7:4)
                                       TO WS-WORK-DATE (1:4)
           MOVE FP7-LASN-ORIG-1ST-PMT-DT (1:2)
                                       TO WS-WORK-DATE (5:2)
           MOVE FP7-LASN-ORIG-1ST-PMT-DT (4:2)
                                       TO WS-WORK-DATE (7:2)
           MOVE WS-WORK-DATE-N         TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-1ST-PAY-DT
              MOVE DC-GREG-DATE-1-MDY  TO WS-1ST-PMT-DT
           ELSE
              DISPLAY ' BAD 1ST PMT DT ' WS-WORK-DATE '  '
              'CERT = ' FP7-ACCOUNT-NUMBER
           END-IF

           MOVE FP7-CISM-BIRTH-DT (7:4)
                                       TO WS-WORK-DATE (1:4)
                                          WORK-BIRTH-DATE (1:4)
           MOVE FP7-CISM-BIRTH-DT (1:2)
                                       TO WS-WORK-DATE (5:2)
                                          WB-MM
120312                                    ws-birth-dt-x (1:2)
           MOVE FP7-CISM-BIRTH-DT (4:2)
                                       TO WS-WORK-DATE (7:2)
                                          WB-DD
120312                                    ws-birth-dt-x (3:2)
120312     move fp7-cism-birth-dt (9:2) to ws-birth-dt-x (5:2)
           MOVE WS-WORK-DATE-N         TO DC-GREG-DATE-CYMD
           MOVE 'L'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              MOVE DC-BIN-DATE-1       TO WS-BIN-BIRTH-DT
           ELSE
              DISPLAY ' BAD BIRTH DATE ' FP7-CISM-BIRTH-DT
              ' CERT = ' FP7-ACCOUNT-NUMBER
           END-IF

           MOVE 40                     TO WS-INSURED-AGE
           
           MOVE WS-BIN-BIRTH-DT        TO DC-BIN-DATE-1
           MOVE WS-BIN-EFF-DT          TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           IF NO-CONVERSION-ERROR
              COMPUTE WS-INSURED-AGE = DC-ELAPSED-MONTHS / +12
           ELSE
              DISPLAY ' BAD AGE CALC CERT = ' FP7-ACCOUNT-NUMBER
              ' ' DC-ERROR-CODE
           END-IF

           MOVE  FP7-ACCOUNT-NUMBER    TO SAVE-CERT

           IF FP7-PMT-AMT NOT NUMERIC
              MOVE ZEROS               TO FP7-PMT-AMT
           END-IF

           IF FP7-CANC-AMT NOT NUMERIC
              MOVE ZEROS               TO FP7-CANC-AMT
           END-IF

           IF FP7-INS-MOS NOT NUMERIC
              MOVE ZEROS               TO FP7-INS-MOS
           END-IF

           IF FP7-AMT-DOLLARS NOT NUMERIC
              MOVE ZEROS               TO FP7-AMT-DOLLARS
           END-IF

           IF FP7-AMT-CENTS NOT NUMERIC
              MOVE ZEROS               TO FP7-AMT-CENTS
           END-IF

           MOVE FP7-AMT-DOLLARS        TO WS-WORK-AMT-DOLLARS
           MOVE FP7-AMT-CENTS          TO WS-WORK-AMT-CENTS

           DISPLAY ' FP7 APR =' FP7-APR
           IF FP7-APR-WHOLE NOT NUMERIC
              MOVE ZEROS               TO FP7-APR-WHOLE
           END-IF
           IF FP7-APR-DEC NOT NUMERIC
              MOVE ZEROS               TO FP7-APR-DEC
           END-IF
           MOVE FP7-APR-WHOLE          TO WS-WORK-APR-DOLLARS
           MOVE FP7-APR-DEC            TO WS-WORK-APR-CENTS
           DISPLAY ' APR = **' WS-WORK-APR-N '**'

           IF FP7-LASN-LOAN-TERM NOT NUMERIC
              MOVE ZEROS               TO FP7-LASN-LOAN-TERM
           END-IF

           MOVE SPACES                 TO WS-LF-BENEFIT-CD
                                          WS-AH-BENEFIT-CD
                                          WS-AH-ONLY-SW
           MOVE ZEROS                  TO WS-LF-PREM
                                          WS-AH-PREM
                                          WS-LF-REF
                                          WS-AH-REF
                                          WS-LF-PREM-ALT
                                          WS-LF-BALLOON
                                          WS-LF-REF-DATE
                                          WS-AH-REF-DATE

           .
       0070-EXIT.
           EXIT.
           
       0080-BUILD-SEQ-1.
       
           ADD 1                       TO LOGIC-CNT
           ADD 1                       TO CERT-CNT
      *    ADD 1                       TO FP-CERT-ISS
           ADD 1                       TO TOT-CERT-ISS

           MOVE SPACES                 TO PBI-RECORD-BODY
           
           MOVE SAVE-CERT              TO PBI-CERT-PRIME
           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X

           MOVE FP7-SHORT-FNAME        TO PBI-I-INS-1ST-NAME
           MOVE FP7-MID-INIT           TO PBI-I-INS-MIDDLE-INIT
           MOVE FP7-SHORT-LNAME        TO PBI-I-INS-LAST-NAME

           MOVE WS-INSURED-AGE         TO PBI-I-INSURED-AGE
           MOVE 'M'                    TO PBI-I-INSURED-SEX

           STRING FP7-TAX-ID (1:3) '-' FP7-TAX-ID (4:2) '-'
              FP7-TAX-ID (6:4) DELIMITED BY SIZE INTO PBI-I-SOC-SEC-NO
           END-STRING

           MOVE WS-BIRTH-DT            TO PBI-I-BIRTHDAY

           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '1'                    TO PBI-SEQUENCE

           MOVE PBI-ISSUE-RECORD       TO OUT-RECORD (1:80)
           PERFORM 0140-RELEASE-REC    THRU 0140-EXIT

           .
       0080-EXIT.
           EXIT.

       0090-BUILD-SEQ-2.

           MOVE SPACES                 TO PBI-RECORD-BODY
           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME

           MOVE +1                     TO S1
           IF FP7-NAME-2 NOT = SPACES
              PERFORM VARYING S2 FROM +1 BY +1 UNTIL
                 (S2 > +30)
                 OR (FP7-NAME-2 (S2:1) = ' ')
              END-PERFORM
              IF (S2 < +30)
                 AND (S2 > +1)
                 MOVE FP7-NAME-2 (S1:(S2 - 1))
                                       TO PBI-I-JNT-1ST-NAME
              END-IF
              IF (FP7-NAME-2 (S2:1) = SPACES)
                 AND (FP7-NAME-2 ((S2 + 2):1) = SPACES)
                 MOVE FP7-NAME-2 ((S2 + 1):1)
                                       TO PBI-I-JNT-MIDDLE-INIT
                 COMPUTE S2 = S2 + 3
              ELSE
                 COMPUTE S2 = S2 + 1
              END-IF
              MOVE S2                  TO S1
              COMPUTE S2 = 31 - S2
              MOVE FP7-NAME-2 (S1:S2)  TO PBI-I-JNT-LAST-NAME
           END-IF

           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '2'                    TO PBI-SEQUENCE

           IF PBI-I-ISSUE-REC-SEQ-2 NOT = SPACES
              MOVE PBI-ISSUE-RECORD    TO OUT-RECORD (1:80)
              PERFORM 0140-RELEASE-REC THRU 0140-EXIT
           END-IF

           .
       0090-EXIT.
           EXIT.

       0100-BUILD-SEQ-3.

           MOVE SPACES                 TO PBI-RECORD-BODY

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME
           
           MOVE WS-LF-BENEFIT-CD       TO PBI-I-LF-BENEFIT-TYPE
           MOVE WS-LF-PREM             TO PBI-I-LF-PREM-AMT
           MOVE WS-LF-PREM-ALT         TO PBI-I-LF-ALT-PREM-AMT
           MOVE WS-LF-BALLOON          TO PBI-I-LF-ALT-BENEFIT-AMT

           COMPUTE PBI-I-LF-BENEFIT-AMT = FP7-PMT-AMT * FP7-INS-MOS
      *    IF WS-WORK-AMT-N NOT = ZEROS
      *       MOVE WS-WORK-AMT-N       TO PBI-I-LF-BENEFIT-AMT
      *    END-IF

           MOVE FP7-INS-MOS            TO PBI-I-LF-TERM

           ADD WS-LF-PREM              TO TOT-LIFE-WRITTEN
      *                                   FP-LF-WRITTEN

           ADD WS-LF-PREM-ALT          TO TOT-LIFE-WRITTEN
      *                                   FP-LF-WRITTEN

           IF PBI-I-LF-PREM-AMT = ZEROS
              MOVE ZEROS               TO PBI-I-LF-BENEFIT-AMT
           END-IF
      
           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '3'                    TO PBI-SEQUENCE

           IF (PBI-I-LF-PREM-AMT > ZEROS)
      *       AND (PBI-I-LF-BENEFIT-AMT > ZEROS)
              MOVE PBI-ISSUE-RECORD    TO OUT-RECORD (1:80)
              PERFORM 0140-RELEASE-REC THRU 0140-EXIT
           END-IF

           .
       0100-EXIT.
           EXIT.

       0110-BUILD-SEQ-4.

           MOVE SPACES                 TO PBI-RECORD-BODY

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME

           DISPLAY ' BENEFIT CD **' WS-AH-BENEFIT-CD '**'
           DISPLAY '  PMT AMT   **' FP7-PMT-AMT '**'
           DISPLAY '  PREM AMT  **' WS-AH-PREM '**'
           MOVE WS-AH-BENEFIT-CD       TO PBI-I-AH-BENEFIT-TYPE
           MOVE FP7-PMT-AMT            TO PBI-I-AH-BENEFIT-AMT
           MOVE FP7-INS-MOS            TO PBI-I-AH-TERM
           MOVE WS-AH-PREM             TO PBI-I-AH-PREM-AMT           


           ADD WS-AH-PREM              TO TOT-AH-WRITTEN
      *                                   FP-AH-WRITTEN

           MOVE '2'                       TO  PBI-TRANS-TYPE.
           MOVE '4'                       TO  PBI-SEQUENCE.

           IF (PBI-I-AH-BENEFIT-AMT > ZEROS)
              AND (PBI-I-AH-PREM-AMT > ZEROS)
              MOVE PBI-ISSUE-RECORD    TO OUT-RECORD (1:80)
              PERFORM 0140-RELEASE-REC THRU 0140-EXIT
           END-IF

           .
       0110-EXIT.
           EXIT.

       0120-BUILD-SEQ-5.

           MOVE SPACES                 TO PBI-RECORD-BODY

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME

           MOVE FP7-RESP-CODE          TO PBI-I-LOAN-OFFICER
           
           SET S3                      TO +1
      *    SEARCH ALL LOAN-OFFS AT END
      *       MOVE 'XX'                TO PBI-I-LOAN-OFFICER (1:2)
      *         WHEN RESP-CODE (S3) = PBI-I-LOAN-OFFICER (3:3)
      *            MOVE BRANCH-CODE (S3) TO PBI-I-LOAN-OFFICER (1:2)
      *    END-SEARCH
           MOVE FP7-BRANCH-NO (11:2)   TO PBI-I-LOAN-OFFICER (1:2)

021109*    MOVE FP7-LASN-LOAN-TERM     TO PBI-I-LOAN-TERM

           MOVE WS-1ST-PMT-DT          TO PBI-I-1ST-PMT-DT

           MOVE WS-WORK-APR-N          TO PBI-I-LOAN-APR

           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '5'                    TO PBI-SEQUENCE

           MOVE PBI-ISSUE-RECORD       TO OUT-RECORD (1:80)
           PERFORM 0140-RELEASE-REC    THRU 0140-EXIT

           .
       0120-EXIT.
           EXIT.

       0122-BUILD-SEQ-6.

           MOVE SPACES                 TO PBI-RECORD-BODY

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME

           MOVE FP7-ADDRESS            TO PBI-I-INSURED-ADDRESS-1

           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '6'                    TO PBI-SEQUENCE

           IF PBI-I-ISSUE-REC-SEQ-6 NOT = SPACES
              MOVE PBI-ISSUE-RECORD    TO OUT-RECORD (1:80)
              PERFORM 0140-RELEASE-REC THRU 0140-EXIT
           END-IF

           .
       0122-EXIT.
           EXIT.

       0125-BUILD-SEQ-7.

           MOVE SPACES                 TO PBI-RECORD-BODY

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME

051810     MOVE FP7-CISM-CITY          TO PBI-I-INSURED-CITY
051810     MOVE FP7-CISM-STATE         TO PBI-I-INSURED-STATE
051810*    STRING FP7-CISM-CITY ', ' FP7-CISM-STATE DELIMITED BY SIZE
051810*       INTO PBI-I-INSURED-CITY-STATE
051810*    END-STRING
           MOVE FP7-CISM-ZIP-9 (4:9)   TO PBI-I-INSURED-ZIP-CODE
           MOVE FP7-CISM-HOME-PHONE (3:10)
                                       TO PBI-I-INSURED-PHONE-NO
          
           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '7'                    TO PBI-SEQUENCE

           IF PBI-I-ISSUE-REC-SEQ-7 NOT = SPACES
              MOVE PBI-ISSUE-RECORD    TO OUT-RECORD (1:80)
              PERFORM 0140-RELEASE-REC THRU 0140-EXIT
           END-IF

           .
       0125-EXIT.
           EXIT.

       0127-BUILD-SEQ-8.

           MOVE SPACES                 TO PBI-RECORD-BODY

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE SAVE-CERT              TO PBI-CERT-PRIME

           MOVE 'FIRST PREMIER BANK'   TO  PBI-I-CRED-BENE-NAME      
          
           MOVE '2'                    TO PBI-TRANS-TYPE
           MOVE '8'                    TO PBI-SEQUENCE

           IF PBI-I-ISSUE-REC-SEQ-8 NOT = SPACES
              MOVE PBI-ISSUE-RECORD    TO OUT-RECORD (1:80)
              PERFORM 0140-RELEASE-REC THRU 0140-EXIT
           END-IF

           .
       0127-EXIT.
           EXIT.

       0130-BUILD-CANCEL.

           MOVE SPACES                 TO PBI-RECORD-BODY
           ADD 1                       TO LOGIC-CANC-CNT
           ADD 1                       TO CANC-CNT

           MOVE SAVE-EFF-DATE          TO PBI-CERT-EFF-DT-X
           MOVE WS-SAVE-ACCT-NO        TO PBI-CERT-PRIME

           MOVE FP7-SHORT-LNAME        TO PBI-C-INSURED-NAME

           MOVE WS-LF-REF              TO PBI-C-LF-PREM-REFUND
           MOVE WS-AH-REF              TO PBI-C-AH-PREM-REFUND
              
           IF PBI-C-LF-PREM-REFUND > ZEROS
              MOVE WS-LF-REF-DATE      TO PBI-C-LF-CANCEL-DATE
           END-IF

           IF PBI-C-AH-PREM-REFUND > ZEROS
              MOVE WS-AH-REF-DATE      TO PBI-C-AH-CANCEL-DATE
           END-IF

           MOVE '3'                    TO PBI-C-TRANS-TYPE
           MOVE '1'                    TO PBI-C-SEQUENCE

      *    ADD 1                       TO FP-CERT-CANC
      *    ADD PBI-C-LF-PREM-REFUND    TO FP-LF-CANC
      *    ADD PBI-C-AH-PREM-REFUND    TO FP-AH-CANC

           ADD 1                       TO TOT-CERT-CANC
           ADD PBI-C-LF-PREM-REFUND    TO TOT-LIFE-CANC
           ADD PBI-C-AH-PREM-REFUND    TO TOT-AH-CANC

           IF (PBI-C-LF-PREM-REFUND > ZEROS)
              OR (PBI-C-AH-PREM-REFUND > ZEROS)
              MOVE PBI-ISSUE-RECORD    TO OUT-RECORD (1:80)
              PERFORM 0140-RELEASE-REC THRU 0140-EXIT
           END-IF

           .
       0130-EXIT.
           EXIT.

       0140-RELEASE-REC.

           MOVE '1'                 TO OUT-BATCH-HDR-IND
           RELEASE SORT-RECORD      FROM OUT-RECORD
           ADD +1 TO RECS-REL

           .
       0140-EXIT.
           EXIT.

       0160-ACCOUNT-BREAK.

           MOVE '9'                    TO FP-CARR
           MOVE '000000'               TO FP-COMP
           MOVE 'SD'                   TO FP-STATE
           MOVE WS-PREV-SR-ACCOUNT     TO FP-ACCT-NO
           MOVE 'FP'                   TO FP-BATCH-ID
           MOVE WORK-MO-IN             TO FP-BATCH-DA
           ADD  1                      TO FP-BATCH-NO
           MOVE WORK-YR-IN             TO FP-HD-EFF-YR
           MOVE WORK-MO-IN             TO FP-HD-EFF-MO
           MOVE WORK-DA-IN             TO FP-HD-EFF-DA
           MOVE 'CID'                  TO FP-CLIENT-ID
           MOVE '1'                    TO FP-TRANS-TYPE
           MOVE '0'                    TO FP-SEQUENCE-NO
           MOVE WS-PREV-SR-ACCOUNT     TO FP-ACCOUNT-NO
           MOVE '0'                    TO FP-BATCH-HDR-IND
           ADD 1                       TO HDR-CNT

           WRITE CARD-RECORD           FROM FPB-BATCH-HDR

           MOVE ZEROS                 TO FP-LF-WRITTEN
                                         FP-LF-CANC
                                         FP-AH-WRITTEN
                                         FP-AH-CANC
                                         FP-CERT-ISS
                                         FP-CERT-CANC
                                         CERT-CNT
                                         CANC-CNT
                                         DETAIL-CNT

           .
       0160-EXIT.
           EXIT.

       8500-DATE-CONVERT.

           CALL 'ELDATCX'              USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.

