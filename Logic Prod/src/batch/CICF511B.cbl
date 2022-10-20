000100 IDENTIFICATION DIVISION.                                         00000100
000200                                                                  00000200
000300 PROGRAM-ID.                CICF511.                              00000300
000400                                                                  00000400
000500*AUTHOR.     CSO.                                                 00000500
000600                                                                  00000600
000700*DATE-COMPILED.                                                   00000700
000800                                                                  00000800
000900****************************************************************  00000900
001000*                                                                 00001000
001100* REMARKS.                                                        00001100
001200*                                                                 00001200
001300* COMFED PROCESSING.                                              00001300
001400*   (USING THE 800 CHARACTER INPUT RECORDS FROM COMFED)           00001400
001500*                                                                 00001500
001600*   1)  THIS PROGRAM WILL EXTRACT RECORDS FROM THE COPY FILE      00001600
001700*        OF COMFED INPUT AND CREATE INDIVIDUAL RECORDS            00001700
001800*         FOR INPUT TO LOGIC FROM THEM.                           00001800
002300*                                                                 00001900
002300*   2)  IT ALSO BUILDS THE 115 LRECL PPA FILE FOR TIM WOOD. IT    00001900
002300*        IS DOWNLOADED TO A PC FILE FOR COMFED TO USE.            00001900
002300*                                                                 00001900
002300* NOTE: AS OF 7-01-99 THE PPA FILE IS NO LONGER BEING BUILT.      00001900
002300* ===== COMMERCIAL FEDERAL IS PROVIDING TIM WOOD WITH A DISKETT   00001900
002300*       FILE.  THIS LOGIC HAS BEEN COMMENTED OUT.                 00001900
002300*                                                                 00001900
002800****************************************************************  00002800
060503******************************************************************
060503*                   C H A N G E   L O G
060503*
060503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
060503*-----------------------------------------------------------------
060503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
060503* EFFECTIVE    NUMBER
060503*-----------------------------------------------------------------
060503* 060503                   PEMA  CHANGE TO NET PAY      
060503******************************************************************
002900*                                                                 00002900
001500 ENVIRONMENT DIVISION.                                            00133315
001600 INPUT-OUTPUT SECTION.                                            00133316
001700 FILE-CONTROL.                                                    00133317
001800     SELECT  COMFED-IN       ASSIGN TO SYS010-UT-2400-S-SYS010.   00133318
001900     SELECT  COMFED-OUT      ASSIGN TO SYS012-UT-2400-S-SYS012.   00133319
001900*    SELECT  PPA-OUT         ASSIGN TO SYS014-UT-2400-S-SYS014.   00133319
002000     SELECT  RPT-FILE        ASSIGN TO SYS013-UR-1403-S-SYS013.   00133321
002000     SELECT  NAME-OUT        ASSIGN TO SYS053-UR-1403-S-SYS053.   00133321
002100*    SELECT  REPORT-FILE     ASSIGN TO SYS008-UR-1403-S-SYS008.   00133322
002200                                                                  00133323
002300 SKIP3                                                            00133324
002400 DATA DIVISION.                                                   00133325
002500                                                                  00133326
002600 FILE SECTION.                                                    00133327
002700                                                                  00133328
002800******************************************************************00133329
002900**        INPUT FILE FROM COMFED                                **00133330
003000******************************************************************00133331
003100                                                                  00133332
003200 FD  COMFED-IN                                                    00133333
003400     RECORDING MODE IS F                                          00133334
003500     LABEL RECORDS ARE STANDARD                                   00133335
003600     RECORD CONTAINS 800 CHARACTERS                               00133336
003700     BLOCK CONTAINS 0 RECORDS                                     00133337
003800     DATA RECORD IS COMFED-RECORD.                                00133338
003900                                                                  00133339
004000 01  COMFED-RECORD              PIC X(800).                       00133340
004100                                                                  00133341
004200******************************************************************00133342
004300**       OUTPUT CARD FILE FOR INPUT TO PROGRAM 'CL512'          **00133343
004400******************************************************************00133344
004500                                                                  00133345
004600 FD  COMFED-OUT                                                   00133346
004700     RECORDING MODE IS F                                          00133347
004800     LABEL RECORDS ARE STANDARD                                   00133348
004900     RECORD CONTAINS 90 CHARACTERS                                00133349
003700     BLOCK CONTAINS 0 RECORDS                                     00133350
005000     DATA RECORD IS CARD-RECORD.                                  00133351
005100                                                                  00133352
005200 01  CARD-RECORD             PIC X(90).                           00133353
005300                                                                  00133354
004100                                                                  00133355
004100                                                                  00133341
004200******************************************************************00133342
004300**       OUTPUT EXCEPTION NAME FORMAT RECORDS FOR CHECKING      **00133343
004400******************************************************************00133344
004500                                                                  00133345
004600 FD  NAME-OUT                                                     00133346
004700     RECORDING MODE IS F                                          00133347
004800     LABEL RECORDS ARE STANDARD                                   00133348
004900     RECORD CONTAINS 132 CHARACTERS                               00133349
003700     BLOCK CONTAINS 0 RECORDS                                     00133350
005000     DATA RECORD IS NAME-REC.                                     00133351
005100                                                                  00133352
005200 01  NAME-REC                       PIC X(132).                   00133353
004100                                                                  00133355
004200******************************************************************00005320
004300**       OUTPUT DISK FILE FOR INPUT TO CID PC PP&A PROGRAM      **00005330
004400******************************************************************00005340
004500                                                                  00005350
004600*FD  PPA-OUT                                                      00005360
004700*    RECORDING MODE IS F                                          00005370
004800*    LABEL RECORDS ARE STANDARD                                   00005380
004900*    RECORD CONTAINS 115 CHARACTERS                               00005390
003700*    BLOCK CONTAINS 0 RECORDS                                     00005391
005000*    DATA RECORD IS PPA-RECORD.                                   00005392
005100*                                                                 00005393
005200*01  PPA-RECORD              PIC X(115).                          00005394
005300                                                                  00005395
005400******************************************************************00133369
005500**                 OUTPUT ERROR REPORT                          **00133370
005600******************************************************************00133371
005700                                                                  00133372
005800 FD  RPT-FILE                                                     00133373
005900     RECORDING MODE IS F                                          00133374
006000     LABEL RECORDS ARE STANDARD                                   00133375
006100     RECORD CONTAINS 133 CHARACTERS                               00133376
006200     BLOCK CONTAINS 0 RECORDS                                     00133377
006300     DATA RECORD IS RPT-REC-OUT.                                  00133378
006400                                                                  00133379
006500 01  RPT-REC-OUT.                                                 00133380
006700     05  RPT-REC                  PIC X(132).                     00133382
006800                                                                  00133383
006900******************************************************************00133384
007000                                                                  00133385
007100 SKIP3                                                            00133386
007200 WORKING-STORAGE SECTION.                                         00133387
007300 77  FILLER  PIC X(32) VALUE '********************************'.  00133388
007400 77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  00133389
007500 77  FILLER  PIC X(32) VALUE '********************************'.  00133390
007600                                                                  00133391
007700 77  V-AH-BEN-AMT-WORK      PIC S9(7)V99  VALUE ZEROS.            00134107
062100 77  LAST-REC-SW            PIC X         VALUE 'N'.              00133393
062100 77  LETTER-SW              PIC X         VALUE 'N'.              00133393
062100 77  SAVE-SUB               PIC 99        VALUE ZERO.             00133393
062100 77  SAVE-1                 PIC X         VALUE SPACE.            00133393
007700 77  CANCL-SW               PIC X         VALUE 'N'.              00133394
007700 77  SW-20                  PIC X         VALUE ' '.              00133395
007700 77  ERROR-CODE             PIC X(25)     VALUE SPACES.           00133396
007700 77  SAVE-SEQ               PIC X(30)     VALUE SPACES.           00133397
007800 77  SAVE-CERT              PIC X(10)     VALUE SPACES.           00133398
007800 77  SAVE-STATE             PIC X(02)     VALUE SPACES.           00133399
008300 77  HDR-CNT                PIC 9(07)     VALUE ZEROS.            00133400
008300 77  RPT-PAGE-CNT           PIC 9999      VALUE ZEROS.            00133400
008400 77  RPT-CANCL-CNT          PIC 99999     VALUE ZEROS.            00133401
008500 77  RPT-ISS-CNT            PIC 99999     VALUE ZEROS.            00133402
008600 77  RPT-LINE-CNT           PIC 99        VALUE ZEROS.            00133403
008600 77  NAME-LINE-CNT          PIC 99        VALUE ZEROS.            00133403
008800 77  RPT-CNT                PIC 9999999   VALUE ZEROS.            00133404
008800 77  SPACE-CNT              PIC 9         VALUE ZEROS.            00133404
008900 77  DETAIL-CNT             PIC 9999      VALUE ZEROS.            00133405
008900 77  NINES-IN               PIC 9(12)     VALUE 999999999999.     00133406
008900 77  DETAIL-CNT-TOT         PIC 9999999   VALUE ZEROS.            00133407
008900 77  PPA-CNT                PIC 9999999   VALUE ZEROS.            00133407
008900 77  LOGIC-CNT              PIC 9999999   VALUE ZEROS.            00133407
008900 77  LOGIC-CANC-CNT         PIC 9999999   VALUE ZEROS.            00133407
009000 77  IN-CNT                 PIC 9999999   VALUE ZEROS.            00133408
009100 77  TOT-LIFE-WRITTEN       PIC S9(8)V99  VALUE ZEROS.            00133409
009200 77  TOT-LIFE-CANC          PIC S9(8)V99  VALUE ZEROS.            00133410
009300 77  TOT-AH-WRITTEN         PIC S9(8)V99  VALUE ZEROS.            00133411
009400 77  TOT-AH-CANC            PIC S9(8)V99  VALUE ZEROS.            00133412
009500 77  TOT-CERT-ISS           PIC S9(4)     VALUE ZEROS.            00133413
009600 77  TOT-CERT-CANC          PIC S9(4)     VALUE ZEROS.            00133414
010200 77  CANC-CNT               PIC 9999      VALUE ZEROS.            00133415
010200 77  CERT-CNT               PIC 9999      VALUE ZEROS.            00133416
010300 77  FIRST-SW               PIC X         VALUE 'Y'.              00133418
010300 77  WRITTEN-PREM-SW        PIC X         VALUE 'N'.              00133419
010400 77  JR-SUB                 PIC 99        VALUE ZEROS.            00133421
010400 77  SP-SUB                 PIC 99        VALUE ZEROS.            00133421
010500 77  JR-SW                  PIC X         VALUE 'N'.              00133422
010700 77  SAVE-JNT-FIRST-INIT    PIC X(01)     VALUE SPACES.           00133423
012200 77  SAVE-SUB-LT-SUB1       PIC 9999999   VALUE ZEROS.            00133424
014900 77  SUB1                   PIC 99        VALUE ZEROS.            00133425
015000 77  SUB2                   PIC 99        VALUE ZEROS.            00133426
015100 77  CODE-SUB               PIC 99        VALUE ZEROS.            00133427
015200 77  G-SUB                  PIC 99        VALUE ZEROS.            00133428
015300 77  SAVE-G-SUB             PIC 99        VALUE ZEROS.            00133429
005100                                                                  00133352
005100 01  WS-CERT-WORK1.                                               00133352
005100     05  WS-CERT-WK1-NUM-6         PIC X(06).                     00133352
005100     05  WS-CERT-WK1-ZEROS-4       PIC X(04).                     00133352
005100 01  WS-CERT-WORK1-R  REDEFINES                                   00133352
005100     WS-CERT-WORK1.                                               00133352
005100     05  WS-CERT-WK1-NUM-7         PIC X(07).                     00133352
005100     05  WS-CERT-WK1-ZEROS-3       PIC X(03).                     00133352
005100                                                                  00133352
005100 01  WS-CERT-WORK2.                                               00133352
005100     05  WS-CERT-WK2-ZEROS-4       PIC X(04).                     00133352
005100     05  WS-CERT-WK2-NUM-6         PIC X(06).                     00133352
005100 01  WS-CERT-WORK2-R  REDEFINES                                   00133352
005100     WS-CERT-WORK2.                                               00133352
005100     05  WS-CERT-WK2-ZEROS-3       PIC X(03).                     00133352
005100     05  WS-CERT-WK2-NUM-7         PIC X(07).                     00133352
005100                                                                  00133352
005200 01  N-R-WORK.                                                    00133353
005200     05  NAME-CC                    PIC X.                        00133353
005200     05  FILLER                     PIC XXX.                      00133353
005200     05  FILLER                     PIC X(17)                     00133353
005200                                     VALUE 'CERT NUMBER IS = '.   00133353
005200     05  CERT-NUM                   PIC X(10).                    00133353
005200     05  FILLER                     PIC X(08).                    00133353
005200     05  FILLER                     PIC X(10)  VALUE 'NAME IS = '.00133353
005200     05  NAME-IS                    PIC X(27).                    00133353
005200     05  FILLER                     PIC X(56).                    00133353
005300                                                                  00133354
005200 01  NAME-RPT-HDNG.                                               00133353
005200     05  NAME-CC                    PIC X.                        00133353
005200     05  FILLER                     PIC X(13).                    00133353
005200     05  FILLER                     PIC X(33)                     00133353
005200         VALUE 'COMFED NAME EXCEPTION REPORT FOR '.               00133353
030500     05  NAME-REPORT-DATE    PIC X(14).                           00133587
005200     05  FILLER                     PIC X(49).                    00133353
005200     05  FILLER                     PIC X(15)                     00133353
005200         VALUE 'PGM = CICF511'.                                   00133353
005200     05  FILLER                     PIC X(06).                    00133353
084800                                                                  00133446
084900 01  BUILD-APR.                                                   00133447
085000     05  W-APR-N                       PIC 9(06).                 00133448
085000     05  W-APR-X  REDEFINES W-APR-N.                              00133448
085000         10  W-APR-ALL.                                           00133448
085000             15  W-APR-WHOLE-POS1      PIC X(01).                 00133448
085000             15  W-APR-WHOLE           PIC X(02).                 00133448
085000             15  W-APR-DEC.                                       00133448
085000                 20  W-APR-DEC-FIRST2  PIC X(02).                 00133448
085000                 20  W-APR-DEC-LAST1   PIC X(01).                 00133448
085100                                                                  00133449
085100 01  APR-WORK.                                                    00133449
085100     05  WS-APR              PIC 99V9999.                         00133449
085100     05  WS-APR-R REDEFINES                                       00133449
085100         WS-APR              PIC 999V999.                         00133449
085100                                                                  00133446
085100 01  SPACE-CK.                                                    00133447
085100     05  SP-CK               PIC X    OCCURS 26 TIMES.            00133448
085100                                                                  00133449
085100                                                                  00133449
084800                                                                  00133446
084900 01  NAME-IN.                                                     00133447
085000     05  ILN-SHT-NAME-IN     PIC X    OCCURS 26 TIMES.            00133448
085100                                                                  00133449
085200 01  F-NAME.                                                      00133450
085300     05   FN-1               PIC X.                               00133451
085400                                                                  00133452
085500                                                                  00133453
085600 01  M-INIT.                                                      00133454
085700     05   MI-1               PIC X.                               00133455
085800                                                                  00133456
085900 01  L-NAME.                                                      00133457
086000     05   LN-15              PIC X     OCCURS 15 TIMES.           00133458
086200                                                                  00133460
086300 01  CANC-NAME.                                                   00133461
086400     05   A-NAME             PIC X(11) VALUE SPACE.               00133462
086500     05   A-INIT             PIC X     VALUE SPACE.               00133463
086600                                                                  00133464
024900                                                                  00133465
010400                                                                  00133466
010400 01  WORK-TERM.                                                   00133467
010400     05  V-TERM-BOTH.                                             00133468
010400         10  V-TERM-X       PIC X(03).                            00133469
010400         10  V-TERM-N      REDEFINES                              00133470
010400             V-TERM-X       PIC 9(03).                            00133471
010400                                                                  00133466
010500 01  WORK-ST2.                                                    00133467
010600     05  WORK-STATE-X2.                                           00133468
010600         10  WORK-ST-X2     PIC XX.                               00133469
010500         10  WORK-ST-N     REDEFINES                              00133470
010600             WORK-ST-X2     PIC 99.                               00133471
010400                                                                  00133472
010500 01  W-LREFUND.                                                   00133473
010600     05  W-LREFUND-N            PIC 9(09).                        00133474
010500     05  W-LREFUND-X   REDEFINES  W-LREFUND-N.                    00133475
010600         10  W-LREFUND-DOL.                                       00133476
010600             15  W-LREFUND-DOL2 PIC X(02).                        00133477
010600             15  W-LREFUND-DOL5 PIC X(05).                        00133478
010600         10  W-LREFUND-CEN      PIC X(02).                        00133479
010400                                                                  00133480
010500 01  W-AREFUND.                                                   00133481
010600     05  W-AREFUND-N            PIC 9(09).                        00133482
010500     05  W-AREFUND-X   REDEFINES  W-AREFUND-N.                    00133483
010600         10  W-AREFUND-DOL.                                       00133484
010600             15  W-AREFUND-DOL2 PIC X(02).                        00133485
010600             15  W-AREFUND-DOL5 PIC X(05).                        00133486
010600         10  W-AREFUND-CEN      PIC X(02).                        00133487
010400                                                                  00133488
010400                                                                  00133489
010500 01  W-PR-NOTE-AMT.                                               00133490
010600     05  W-PN-AMT-N            PIC 9(08).                         00133491
010500     05  W-PN-AMT-X   REDEFINES  W-PN-AMT-N.                      00133492
010600         10  W-PN-AMT-DOL      PIC X(06).                         00133493
010600         10  W-PN-AMT-CEN      PIC X(02).                         00133494
010400                                                                  00133495
010400                                                                  00133496
010500 01  W-PROC-YR-CEN.                                               00133497
010200     05  W-PROC-MO          PIC 99        VALUE ZEROS.            00133417
010200     05  W-PROC-SLASH       PIC X         VALUE '/'.              00133417
010600     05  W-PROC-CEN         PIC X(02)     VALUE SPACES.           00133498
010600     05  W-PROC-YR          PIC 9(02)     VALUE ZEROS.            00133499
010300                                                                  00133500
010500 01  OUT-RECORD.                                                  00133501
010600     05  OUT-CERT-NO        PIC X(10)     VALUE SPACES.           00133502
010600     05  OUT-CERT-SFX       PIC X(01)     VALUE SPACES.           00133503
010700     05  OUT-EFF-DATE       PIC X(06)     VALUE SPACES.           00133504
010800     05  OUT-INFO           PIC X(61)     VALUE SPACES.           00133505
010800     05  OUT-TRAN-TYPE      PIC X         VALUE SPACES.           00133506
010800     05  OUT-SEQUENCE       PIC X         VALUE SPACES.           00133507
010600     05  SR-BATCH-NUMB.                                           00133508
010600         10  SR-BATCH-ID    PIC XX        VALUE ZEROS.            00133508
010600         10  SR-BATCH-DA    PIC XX        VALUE ZEROS.            00133508
010600         10  SR-BATCH-NO    PIC 99        VALUE ZEROS.            00133508
010600     05  SR-REC-CNT         PIC 9(4)      VALUE ZEROS.            00133509
010900                                                                  00133510
124600 01  SAVE-FULL-ACCT.                                              00133511
124700     05 SAVE-ACCT-7         PIC X(07)     VALUE SPACES.           00133512
124600     05 SAVE-ACCT           PIC XXX       VALUE SPACES.           00133513
013600                                                                  00133514
019500 01  COMFED-BATCH-HDR.                                            00133515
020500     05 CF-CARR-CO.                                               00133516
020600         10 CF-CARR          PIC X         VALUE '9'.             00133517
020700         10 CF-COMP          PIC X(6)      VALUE '000000'.        00133518
020800     05 CF-STATE             PIC XX        VALUE '  '.            00133519
020900     05 CF-ACCT-NO           PIC X(10)     VALUE SPACES.          00133520
020200     05 CF-BATCH-NUMB.                                            00133521
020200         10 CF-BATCH-ID      PIC XX        VALUE ZEROS.           00133521
020200         10 CF-BATCH-DA      PIC XX        VALUE ZEROS.           00133521
020200         10 CF-BATCH-NO      PIC 99        VALUE 12.              00133521
021000     05 CF-HD-EFF-DT.                                             00133522
021100         10 CF-HD-EFF-MO     PIC XX        VALUE SPACES.          00133523
021200         10 CF-HD-EFF-DA     PIC XX        VALUE SPACES.          00133524
021300         10 CF-HD-EFF-YR     PIC XX        VALUE SPACES.          00133525
020000     05 CF-CERT-ISS          PIC S9(4)     VALUE ZEROS.           00133526
019600     05 CF-LF-WRITTEN        PIC S9(7)V99  VALUE ZEROS.           00133527
019800     05 CF-AH-WRITTEN        PIC S9(7)V99  VALUE ZEROS.           00133528
020100     05 CF-CERT-CANC         PIC S9(4)     VALUE ZEROS.           00133529
019700     05 CF-LF-CANC           PIC S9(7)V99  VALUE ZEROS.           00133530
019900     05 CF-AH-CANC           PIC S9(7)V99  VALUE ZEROS.           00133531
020300     05 CF-CLIENT-ID         PIC X(3)      VALUE 'CSO'.           00133532
021400     05 CF-TRANS-TYPE        PIC X         VALUE '1'.             00133533
020400     05 CF-SEQUENCE-NO       PIC X         VALUE '0'.             00133534
024900                                                                  00133535
025000 01  REPORT-HD-LINE.                                              00133536
025100     05  HD-LIFE-WRITTEN     PIC X(14)  VALUE ' LIFE WRITTEN '.   00133537
025200     05  FILLER              PIC XX     VALUE SPACES.             00133538
025300     05  HD-LIFE-CANC        PIC X(13)  VALUE '    LIFE CANC'.    00133539
025400     05  FILLER              PIC XXX    VALUE SPACES.             00133540
025500     05  HD-AH-WRITTEN       PIC X(14)  VALUE '  A&H WRITTEN '.   00133541
025600     05  FILLER              PIC XX     VALUE SPACES.             00133542
025700     05  HD-AH-CANC          PIC X(13)  VALUE '     A&H CANC'.    00133543
025800     05  FILLER              PIC XX     VALUE SPACES.             00133544
025900     05  HD-CERT-ISS         PIC X(10)  VALUE '   ISS CNT'.       00133545
026000     05  FILLER              PIC X(06)  VALUE SPACES.             00133546
026100     05  HD-CERT-CANC        PIC X(8)   VALUE 'CANC CNT'.         00133547
026200     05  FILLER              PIC X(06)  VALUE SPACES.             00133548
026300     05  HD-PPA-CNT          PIC X(7)   VALUE 'PPA CNT'.          00133549
026400     05  FILLER              PIC XX     VALUE SPACES.             00133550
026500     05  HD-STATE            PIC XXXXX  VALUE SPACES.             00133551
026600     05  FILLER              PIC XX     VALUE SPACES.             00133552
026700     05  HD-ACCT-NO          PIC X(10)  VALUE SPACES.             00133553
026800     05  FILLER              PIC XX     VALUE SPACES.             00133554
026900     05  HD-EFF-DT           PIC X(8)   VALUE SPACES.             00133555
027000                                                                  00133556
027100 01  TOTAL-LINE.                                                  00133557
027200     05  T-LIFE-WRITTEN      PIC ZZ,ZZZ,ZZ9.99-.                  00133558
027300     05  FILLER              PIC XX              VALUE SPACES.    00133559
027400     05  T-LIFE-CANC         PIC ZZ,ZZZ,ZZ9.99-.                  00133560
027500     05  FILLER              PIC XX              VALUE SPACES.    00133561
027600     05  T-AH-WRITTEN        PIC ZZ,ZZZ,ZZ9.99-.                  00133562
027700     05  FILLER              PIC XX              VALUE SPACES.    00133563
027800     05  T-AH-CANC           PIC ZZ,ZZZ,ZZ9.99-.                  00133564
027900     05  FILLER              PIC XXXX            VALUE SPACES.    00133565
028000     05  T-CERT-ISS          PIC ZZZ,ZZ9.                         00133566
028100     05  FILLER              PIC X(09)           VALUE SPACES.    00133567
028200     05  T-CERT-CANC         PIC Z,ZZ9.                           00133568
028100     05  FILLER              PIC X(08)           VALUE SPACES.    00133567
028200     05  T-PPA-CNT           PIC Z,ZZ9.                           00133568
028600     05  FILLER              PIC X(26)           VALUE SPACES.    00133569
028800                                                                  00133570
028900 01  RPT-TOT-LINE.                                                00133571
029000     05  FILLER              PIC X(10)           VALUE SPACES.    00133572
029100     05  FILLER              PIC X(35)           VALUE            00133573
029200         ' NUMBER OF ISSUES WRITTEN IS - '.                       00133574
029300     05  P-ISS-CNT           PIC ZZ,ZZ9.                          00133575
029400     05  FILLER              PIC X(10)           VALUE SPACES.    00133576
029500     05  FILLER              PIC X(35)           VALUE            00133577
029600         'NUMBER OF CANCELS WRITTEN IS - '.                       00133578
029700     05  P-CANCL-CNT         PIC ZZ,ZZ9.                          00133579
029800     05  FILLER              PIC X(10)            VALUE SPACES.   00133580
029900     05  FILLER              PIC X(10)            VALUE SPACES.   00133581
030000                                                                  00133582
030100 01  RPT-HD-LINE1.                                                00133583
030200     05  FILLER              PIC X(26)           VALUE SPACES.    00133584
030300     05 CF-RPT-ID            PIC X(32)           VALUE            00133585
030400          '        COMFED BANK REPORT FOR '.                      00133586
030500     05  RPT-REPORT-DATE     PIC X(14)           VALUE SPACES.    00133587
030600     05  FILLER              PIC X(24)           VALUE SPACES.    00133588
030600     05  FILLER              PIC X(16)           VALUE            00133589
030400         'PGM = CICF511  '.                                       00133590
030600     05  FILLER              PIC X(04)           VALUE SPACES.    00133591
030700     05  FILLER              PIC X(5)            VALUE  'PAGE '.  00133592
030800     05  RPT-PAGE            PIC Z,ZZ9.                           00133593
030900     05  FILLER              PIC X(5)            VALUE SPACES.    00133594
031000                                                                  00133595
031100 01  RPT-HD-LINE2.                                                00133596
031300     05  FILLER              PIC X(10)  VALUE 'CERT #  '.         00133597
031500     05  FILLER              PIC X(12)  VALUE ' DETAIL INFO'.     00133598
031600     05  FILLER              PIC X(09)  VALUE ' '.                00133599
031700     05  FILLER              PIC X(5)   VALUE SPACES.             00133600
031800     05  FILLER              PIC X(20)  VALUE SPACES.             00133601
032000     05  FILLER              PIC X(20)  VALUE SPACES.             00133602
032100     05  FILLER              PIC X(40)  VALUE                     00133603
032200                 '              RECORD TYPE    '.                 00133604
032300     05  FILLER              PIC X(16)  VALUE SPACES.             00133605
032400                                                                  00133606
032500 01  RPT-PT-LINE.                                                 00133607
032600     05  PRT-REC             PIC X(80)  VALUE SPACES.             00133608
032600     05  FILLER              PIC X(02)  VALUE SPACES.             00133609
033600     05  FULL-ERROR.                                              00133610
033600         10  PT-ERROR        PIC X(22)  VALUE SPACES.             00133611
033600         10  PRNT-ERROR      PIC X(28)  VALUE SPACES.             00133612
032400                                                                  00133613
032500 01  ERR-PT-LINE.                                                 00133614
032600     05  ERR-ID              PIC X(11)  VALUE ' * ERROR * '.      00133615
032600     05  ERR-REC             PIC X(80)  VALUE SPACES.             00133616
032600     05  FILLER              PIC X(02)  VALUE SPACES.             00133617
033600     05  ERR-LINE.                                                00133618
033600         10  ERR-REASON      PIC X(30)  VALUE SPACES.             00133619
033600         10  ERR-CODE        PIC X(09)  VALUE SPACES.             00133620
033800                                                                  00133621
034800 01  WK-DATE-N.                                                   00133622
034900     05  WK-MO-X             PIC XX     VALUE SPACES.             00133623
034900     05  WK-MO-N REDEFINES WK-MO-X PIC 99.                        00133624
033800                                                                  00133625
034800 01  WK-DATE.                                                     00133626
034900     05  WK-MO               PIC XX     VALUE SPACES.             00133627
035000     05  FILLER              PIC X      VALUE SPACES.             00133628
035100     05  WK-DA               PIC XX     VALUE SPACES.             00133629
035200     05  FILLER              PIC X      VALUE SPACES.             00133630
035300     05  WK-YR               PIC XX     VALUE SPACES.             00133631
037600                                                                  00133632
038600 01  WORK-DATE.                                                   00133633
038700     12  WORK-DATE-X.                                             00133634
038800         15  WORK-MO-X       PIC XX.                              00133635
038900         15  WORK-DA-X       PIC XX.                              00133636
039000         15  WORK-YR-X       PIC XX.                              00133637
039100     12  WORK-DATE-N    REDEFINES   WORK-DATE-X.                  00133638
039200         15  WORK-MO-N       PIC 99.                              00133639
039300         15  WORK-DA-N       PIC 99.                              00133640
039400         15  WORK-YR-N       PIC 99.                              00133641
039500                                                                  00133642
039100     12  WORK-DATE-IN.                                            00133638
039400         15  WORK-YR-IN      PIC 99.                              00133641
039200         15  WORK-MO-IN      PIC 99.                              00133639
039300         15  WORK-DA-IN      PIC 99.                              00133640
039500                                                                  00133642
039500 01  WORK-ISS-DATE.                                               00133642
039500     05  WRK-ISS-YR          PIC XX.                              00133642
039500     05  WRK-ISS-MO          PIC XX.                              00133642
039500     05  WRK-ISS-DA          PIC XX.                              00133642
039500                                                                  00133642
039500 01  WORK-ISS-DTE-R          REDEFINES                            00133638
039500     WORK-ISS-DATE           PIC 9(06).                           00133638
039500                                                                  00133642
039500 01  WORK-ISS-DTE-N          REDEFINES                            00133638
039500     WORK-ISS-DATE.                                               00133638
039500     05  WRK-ISS-YR-N        PIC 99.                              00133642
039500     05  WRK-ISS-MO-N        PIC 99.                              00133642
039500     05  WRK-ISS-DA-N        PIC 99.                              00133642
039500                                                                  00133642
040900 01  PRNT-RPT-DATES.                                              00133643
041000     05  RPT-DT-ID.                                               00133644
041200         10  FILLER          PIC X(14)   VALUE 'JANUARY       '.  00133646
041300         10  FILLER          PIC X(14)   VALUE 'FEBRUARY      '.  00133647
041400         10  FILLER          PIC X(14)   VALUE 'MARCH         '.  00133648
041500         10  FILLER          PIC X(14)   VALUE 'APRIL         '.  00133649
041600         10  FILLER          PIC X(14)   VALUE 'MAY           '.  00133650
041700         10  FILLER          PIC X(14)   VALUE 'JUNE          '.  00133651
041800         10  FILLER          PIC X(14)   VALUE 'JULY          '.  00133652
041900         10  FILLER          PIC X(14)   VALUE 'AUGUST        '.  00133653
042000         10  FILLER          PIC X(14)   VALUE 'SEPTEMBER     '.  00133654
042100         10  FILLER          PIC X(14)   VALUE 'OCTOBER       '.  00133655
042200         10  FILLER          PIC X(14)   VALUE 'NOVEMBER      '.  00133656
041100         10  FILLER          PIC X(14)   VALUE 'DECEMBER      '.  00133645
042300                                                                  00133657
042400     05  RPT-DT-05     REDEFINES   RPT-DT-ID.                     00133658
042500         10  RPT-DT        OCCURS 12    PIC X(14).                00133659
006400                                                                  00133660
045600                                                                  00133671
006500 01  DISP-LINE.                                                   00133380
006600     05  DISP-DESC                PIC X(22)    VALUE SPACES.      00133381
006700     05  DISP-CNT                 PIC X(07)    VALUE SPACES.      00133382
006700     05  DISP-FILLER              PIC X(103)   VALUE SPACES.      00133382
006800                                                                  00133383
006900******************************************************************00133384
006400                                                                  00133379
006800                                                                  00133662
042700 01  FILLER   PIC X(24) VALUE '** CARD COPY BOOK  **'.            00133663
042800                                                                  00133664
042900*01  INPUT-COPYBOOK          COPY ERCPNDBI.                       00133665
042800                                                                  00133664
042900     COPY ERCPNDBI.                                               00133665
043000                                                                  00133666
043300 EJECT                                                            00133669
043100                                                                  00133667
043200 01  FILLER   PIC X(24) VALUE '**COMFED-RECORD-IN**'.             00133668
043300                                                                  00133669
043400*01  VENDOR-REC              COPY VENDREC8.                       00133670
043300                                                                  00133669
043400     COPY VENDREC8.                                               00133670
043300                                                                  00133669
043300 EJECT                                                            00133669
043300                                                                  00133669
043200 01  FILLER   PIC X(24) VALUE '**PP&A-RECORD-OUT**'.              00133668
043300                                                                  00133669
043400*01  PPA-REC                 COPY PPAREC.                         00133670
043300                                                                  00133669
043400*    COPY PPAREC.                                                 00133670
045600                                                                  00133672
043300 EJECT                                                            00133669
043300                                                                  00133669
054900******************************************************************00133673
055000 SKIP3                                                            00133674
055100 PROCEDURE DIVISION.                                              00133675
055200 SKIP3                                                            00133676
055400                                                                  00133677
055500 INPUT-ROUTINE SECTION.                                           00133678
055600                                                                  00133679
055700     ACCEPT  WORK-DATE-IN  FROM  DATE.                            00133680
055700     MOVE  WORK-MO-IN    TO  WORK-MO-X.                           00133681
055700     MOVE  WORK-MO-IN    TO  WK-MO-X.                             00133681
055700     MOVE  WORK-DA-IN    TO  WORK-DA-X.                           00133681
055700     MOVE  WORK-YR-IN    TO  WORK-YR-X.                           00133681
055600                                                                  00133679
055700     MOVE  WORK-MO-IN    TO  WK-MO.                               00133681
055700     MOVE  WORK-DA-IN    TO  WK-DA.                               00133681
055700     MOVE  WORK-YR-IN    TO  WK-YR.                               00133681
055600                                                                  00133679
055900     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00133683
056000     DISPLAY '*   CURRENT DATE IS -- ' WK-DATE.                   00133684
056100     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00133685
056200                                                                  00133686
056500     OPEN INPUT    COMFED-IN.                                     00133687
056600                                                                  00133688
056700     OPEN  OUTPUT  COMFED-OUT                                     00133689
056700*                  PPA-OUT                                        00133689
056700                   NAME-OUT                                       00133689
056800                   RPT-FILE.                                      00133690
056900                                                                  00133691
082100     MOVE  SPACES                 TO  PBI-RECORD-BODY.            00133692
057100                                                                  00133693
057500     MOVE  ZEROS                  TO  CF-LF-CANC                  00133694
057600                                      CF-AH-CANC                  00133695
057700                                      CF-CERT-CANC.               00133696
119500     MOVE  ZEROS                  TO  PBI-I-AH-PREM-AMT           00133697
119500     MOVE  ZEROS                  TO  PBI-I-AH-BENEFIT-AMT        00133698
094000     MOVE  ZEROS                  TO  PBI-I-AH-BENEFIT-POS1       00133699
094000     MOVE  ZEROS                  TO  PBI-I-AH-BENEFIT-NO         00133700
119500     MOVE  ZEROS                  TO  PBI-I-AH-TERM               00133701
119500     MOVE  ZEROS                  TO  PBI-I-AH-CRIT-PERIOD        00133702
119500     MOVE  ZEROS                  TO  PBI-I-AH-TERM               00133703
119500     MOVE  ZEROS                  TO  PBI-I-AH-CRIT-PERIOD        00133704
093500     MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-YR          00133705
093500     MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-MO          00133706
093500     MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-DA          00133707
119500     MOVE  ZEROS                  TO  PBI-I-AH-PAYMENT            00133708
010600     MOVE  SPACES                 TO  OUT-CERT-NO                 00133709
010600     MOVE  SPACES                 TO  OUT-CERT-SFX                00133710
088500     MOVE  ZEROS                  TO  PBI-C-LF-CANCEL-DATE.       00133711
088500     MOVE  ZEROS                  TO  PBI-C-AH-CANCEL-DATE.       00133712
088500     MOVE  ZEROS                  TO  PBI-C-LF-PREM-REFUND.       00133713
088500     MOVE  ZEROS                  TO  PBI-C-AH-PREM-REFUND.       00133714
057100                                                                  00133715
057200     MOVE  WK-MO                  TO  CF-HD-EFF-MO.               00133716
057400     MOVE  WK-DA                  TO  CF-HD-EFF-DA.               00133717
057300     MOVE  WK-YR                  TO  CF-HD-EFF-YR                00133718
057900     MOVE  WK-MO                  TO  WORK-MO-X.                  00133719
058000     MOVE  WK-YR                  TO  WORK-YR-X.                  00133720
058100     MOVE  WK-DA                  TO  WORK-DA-X.                  00133721
058200                                                                  00133722
058300     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00133723
058400     DISPLAY '*   WORK-DATE-X  IS -- ' WORK-DATE-X.               00133724
058500     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00133725
158100                                                                  00133726
058700     MOVE  RPT-DT (WK-MO-N)       TO NAME-REPORT-DATE             00133835
058700     MOVE  RPT-DT (WK-MO-N)       TO RPT-REPORT-DATE.             00133835
158100                                                                  00133726
146600     PERFORM      NAME-RPT-HD   THRU NAME-RPT-HD-EXIT.            00134252
183800                                                                  00136679
062100 BUILD-PROCESS-DATE.                                              00134334
122000                                                                  00134352
122000     IF    WORK-YR-N  GREATER THAN 20                             00134353
122000           MOVE   19              TO  W-PROC-CEN                  00134354
122000        ELSE                                                      00134355
122000           MOVE   20              TO  W-PROC-CEN.                 00134351
122000                                                                  00134355
122000     MOVE  WORK-YR-N              TO  W-PROC-YR.                  00119505
122000                                                                  00119504
122000     MOVE  WORK-MO-N              TO  W-PROC-MO.                  00119505
122000                                                                  00119506
122000     MOVE  '/'                    TO  W-PROC-SLASH.               00119505
010400                                                                  00133496
122000                                                                  00119506
062100 BUILD-PROCESS-DATE-EXIT.                                         00119511
062100      EXIT.                                                       00119512
122000                                                                  00119513
058900 005-ERR-RPT-HD.                                                  00133727
059000                                                                  00133728
059100     ADD   1                      TO RPT-PAGE-CNT.                00133729
059200     MOVE  RPT-PAGE-CNT           TO RPT-PAGE.                    00133730
059300     MOVE  6                      TO RPT-LINE-CNT.                00133731
059500     MOVE  RPT-HD-LINE1           TO RPT-REC.                     00133733
059600     WRITE RPT-REC-OUT  AFTER ADVANCING PAGE.                     00133734
059800     MOVE  SPACES                 TO RPT-REC.                     00133736
059900     WRITE RPT-REC-OUT  AFTER ADVANCING 1  LINE.                  00133737
060100     MOVE  RPT-HD-LINE2           TO RPT-REC.                     00133739
060200     WRITE RPT-REC-OUT  AFTER ADVANCING 1  LINE.                  00133740
060400     MOVE  SPACES                 TO RPT-REC.                     00133742
060500     WRITE RPT-REC-OUT  AFTER ADVANCING 1  LINE.                  00133743
060600                                                                  00133744
060700 ERR-RPT-HD-EXIT.                                                 00133745
060800     EXIT.                                                        00133746
060900                                                                  00133747
061400 010-READ-INPUT-FILE.                                             00133748
122000                                                                  00133749
061600     IF  RPT-CNT  GREATER THAN  10                                00133750
061700         GO TO 0200-ERRORS.                                       00133751
061800                                                                  00133752
061900     READ COMFED-IN                                               00133753
062000       INTO VENDOR-REC                                            00133754
062100         AT END                                                   00133755
062100           MOVE 'Y'  TO  LAST-REC-SW                              00133756
062100*            CLOSE PPA-OUT                                        00133756
122000               PERFORM  ACCT-BREAK   THRU   ACCT-BREAK-EXIT       00133757
061700                 GO TO END-OF-JOB.                                00133758
061800                                                                  00133752
064500     GO TO ACCT-NO-CHANGED.                                       00133764
122000                                                                  00133759
122000*****************************************************             00133759
122000*                                                                 00133759
122000*  THE FOLLOWING ACCT NUMBER PROCESSING WAS PUT INTO              00133759
122000*  ANOTHER PROGRAM "CICFCNV".                                     00133759
122000*                                                                 00133759
122000*****************************************************             00133759
122000                                                                  00133759
064500 MOVE-CSO-ACCT-NO.                                                00133760
064500                                                                  00133761
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'NE'               00133762
064500                            AND V-FIN-INST-ID = '00052'           00133762
064500         MOVE  '0000606537' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'NE'               00133762
064500                            AND V-FIN-INST-ID = '00058'           00133762
064500         MOVE  '0000606554' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'NE'               00133762
064500                            AND V-FIN-INST-ID = '00055'           00133762
064500         MOVE  '0000606586' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'NE'               00133762
064500                            AND V-FIN-INST-ID = '00057'           00133762
064500         MOVE  '0000606591' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'NE'               00133762
064500                            AND V-FIN-INST-ID = '00059'           00133762
064500         MOVE  '0000606592' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'NE'               00133762
064500                            AND V-FIN-INST-ID = '00056'           00133762
064500         MOVE  '0000606593' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-STATE-CODE = 'NE'                                      00133762
064500         MOVE  '0000015860' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-STATE-CODE = 'CO'                                      00133766
064500         MOVE  '0000017500' TO V-ACCOUNT-ID                       00133767
064500         GO TO ACCT-NO-CHANGED.                                   00133768
064500                                                                  00133769
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'KS'               00133762
064500                            AND V-FIN-INST-ID = '00552'           00133762
064500         MOVE  '0000606556' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'KS'               00133762
064500                            AND V-FIN-INST-ID = '00549'           00133762
064500         MOVE  '0000606566' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'KS'               00133762
064500                            AND V-FIN-INST-ID = '00551'           00133762
064500         MOVE  '0000606581' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'KS'               00133762
064500                            AND V-FIN-INST-ID = '00550'           00133762
064500         MOVE  '0000606583' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-STATE-CODE = 'KS'                                      00133770
064500         MOVE  '0000019580' TO V-ACCOUNT-ID                       00133771
064500         GO TO ACCT-NO-CHANGED.                                   00133772
064500                                                                  00133773
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00347'           00133762
064500         MOVE  '0000606501' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00349'           00133762
064500         MOVE  '0000606503' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00334'           00133762
064500         MOVE  '0000606504' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00756'           00133762
064500         MOVE  '0000606505' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00343'           00133762
064500         MOVE  '0000606506' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00338'           00133762
064500         MOVE  '0000606507' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00345'           00133762
064500         MOVE  '0000606510' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00356'           00133762
064500         MOVE  '0000606512' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00357'           00133762
064500         MOVE  '0000606514' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00353'           00133762
064500         MOVE  '0000606515' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00774'           00133762
064500         MOVE  '0000606527' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00352'           00133762
064500         MOVE  '0000606528' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00337'           00133762
064500         MOVE  '0000606529' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00335'           00133762
064500         MOVE  '0000606530' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00361'           00133762
064500         MOVE  '0000606535' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00354'           00133762
064500         MOVE  '0000606536' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00358'           00133762
064500         MOVE  '0000606539' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00362'           00133762
064500         MOVE  '0000606549' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00339'           00133762
064500         MOVE  '0000606559' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00360'           00133762
064500         MOVE  '0000606560' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00350'           00133762
064500         MOVE  '0000606561' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'IA'               00133762
064500                            AND V-FIN-INST-ID = '00342'           00133762
064500         MOVE  '0000606573' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-STATE-CODE = 'IA'                                      00133774
064500         MOVE  '0000610400' TO V-ACCOUNT-ID                       00133775
064500         GO TO ACCT-NO-CHANGED.                                   00133776
064500                                                                  00133777
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'MO'               00133762
064500                            AND V-FIN-INST-ID = '00388'           00133762
064500         MOVE  '0000606562' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'MO'               00133762
064500                            AND V-FIN-INST-ID = '00380'           00133762
064500         MOVE  '0000606567' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'MO'               00133762
064500                            AND V-FIN-INST-ID = '00386'           00133762
064500         MOVE  '0000606568' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'MO'               00133762
064500                            AND V-FIN-INST-ID = '00387'           00133762
064500         MOVE  '0000606569' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'MO'               00133762
064500                            AND V-FIN-INST-ID = '00385'           00133762
064500         MOVE  '0000606572' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'MO'               00133762
064500                            AND V-FIN-INST-ID = '00381'           00133762
064500         MOVE  '0000606582' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-STATE-CODE = 'MO'                                      00133778
064500         MOVE  '0000610500' TO V-ACCOUNT-ID                       00133779
064500         GO TO ACCT-NO-CHANGED.                                   00133780
064500                                                                  00133781
064500     IF  V-STATE-CODE = 'OK'                                      00133782
064500         MOVE  '0000490700' TO V-ACCOUNT-ID                       00133783
064500         GO TO ACCT-NO-CHANGED.                                   00133784
064500                                                                  00133785
064500     IF  V-STATE-CODE = 'AZ'                                      00133786
064500         MOVE  '0000596100' TO V-ACCOUNT-ID                       00133787
064500         GO TO ACCT-NO-CHANGED.                                   00133788
064500                                                                  00133789
064500     IF  V-STATE-CODE = 'IL'                                      00133790
064500         MOVE  '0000596200' TO V-ACCOUNT-ID                       00133791
064500         GO TO ACCT-NO-CHANGED.                                   00133792
064500                                                                  00133793
064500     IF  V-STATE-CODE = 'IN'                                      00133794
064500         MOVE  '0000596300' TO V-ACCOUNT-ID                       00133795
064500         GO TO ACCT-NO-CHANGED.                                   00133796
064500                                                                  00133797
064500     IF  V-STATE-CODE = 'MN'                                      00133798
064500         MOVE  '0000596400' TO V-ACCOUNT-ID                       00133799
064500         GO TO ACCT-NO-CHANGED.                                   00133800
064500                                                                  00133801
064500     IF  V-TRANS-TYPE = '3' AND V-STATE-CODE = 'SD'               00133762
064500                            AND V-FIN-INST-ID = '00390'           00133762
064500         MOVE  '0000606579' TO V-ACCOUNT-ID                       00133763
064500         GO TO ACCT-NO-CHANGED.                                   00133764
064500                                                                  00133765
064500     IF  V-STATE-CODE = 'SD'                                      00133802
064500         MOVE  '0000596500' TO V-ACCOUNT-ID                       00133803
064500         GO TO ACCT-NO-CHANGED.                                   00133804
064500                                                                  00133805
064500     IF  V-STATE-CODE = 'WY'                                      00133806
064500         MOVE  '0000596600' TO V-ACCOUNT-ID                       00133807
064500         GO TO ACCT-NO-CHANGED.                                   00133808
122000                                                                  00133809
064500 INVALID-STATE-CODE.                                              00133810
064000                                                                  00133811
064000     DISPLAY ' '.                                                 00133811
064000     DISPLAY 'INVALID STATE CODE - CODE  = ' V-STATE-CODE.        00133811
064000     DISPLAY 'NO CSO ACCT NUMBER CAN BE ASSIGNED'.                00133811
064000     DISPLAY 'COMFED ACCOUNT NUMBER IS   = ' V-ACCOUNT-ID.        00133811
064000     DISPLAY 'INSURED NAME IS            = ' V-FULL-NAME.         00133811
064000     DISPLAY 'TRANS TYPE CODE IS         = ' V-TRANS-TYPE.        00133811
062100     DISPLAY 'V-WRITTEN-PREM-INDIC       = ' V-WRITTEN-PREM-INDIC.00133827
064000     DISPLAY ' '.                                                 00133811
062100     IF  V-WRITTEN-PREM-INDIC  =  'Y'                             00133827
062100         GO TO BUILD-PPA                                          00054142
062100       ELSE                                                       00054142
062100         PERFORM BUILD-PPA THRU PPA-EXIT                          00054142
061400         GO TO 010-READ-INPUT-FILE.                               00133748
064000                                                                  00133811
122000                                                                  00133809
064500 ACCT-NO-CHANGED.                                                 00133810
064000                                                                  00133811
063900     ADD  1    TO IN-CNT.                                         00133812
062100     MOVE 'N'  TO  LAST-REC-SW.                                   00133813
064000                                                                  00133814
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00133815
057100                                                                  00133816
064300                                                                  00133817
064400 PROCESS-INPUT.                                                   00133818
064500                                                                  00133819
064600*    DISPLAY 'PROCESS-INPUT   ENTERED  '.                         00133820
122000                                                                  00133821
062100     IF  FIRST-SW  =  'N'                                         00133822
062100         GO TO BUILD-PPA.                                         00133823
064300                                                                  00133824
064500      MOVE   V-STATE-CODE      TO  SAVE-STATE.                    00133825
062100                                                                  00133826
062100     IF  V-WRITTEN-PREM-INDIC  =  'Y'                             00133827
062100         MOVE 'Y'  TO  WRITTEN-PREM-SW                            00133832
062100       ELSE                                                       00133829
062100         MOVE 'N'  TO  WRITTEN-PREM-SW.                           00133832
064500                                                                  00133831
064500     MOVE   'CF'              TO  CF-BATCH-ID.                    00133833
064500     MOVE   WORK-MO-IN        TO  CF-BATCH-DA.                    00133834
167600     ADD    1                 TO  CF-BATCH-NO.                    00133834
058700     MOVE  RPT-DT (WK-MO-N)   TO RPT-REPORT-DATE                  00133835
122000     MOVE  V-ACCOUNT-ID       TO  SAVE-FULL-ACCT                  00133836
062100     MOVE 'N'  TO  FIRST-SW.                                      00133837
167600                                                                  00133838
122000                                                                  00054141
062100 BUILD-PPA.                                                       00054142
122000                                                                  00054143
122000     GO TO PPA-EXIT.                                              00054143
122000                                                                  00054143
122000*  NOTE: ALL PP&A PROCESSING IS BEING SKIPPED.                    00054143
122000*  =====                                                          00054143
122000                                                                  00054143
064800*    IF  V-TRANS-TYPE =  '1' OR '2'                               00133865
122000*        NEXT SENTENCE                                            00054143
122000*      ELSE                                                       00054143
062100*        GO  TO  PPA-EXIT.                                        00054169
062100*                                                                 00054143
062100*                                                                 00054143
062100*    MOVE  V-COMPANY-ID          TO    P-COMPANY-ID.              00054144
062100*    MOVE  V-BANK-ID             TO    P-BANK-ID.                 00054145
062100*    MOVE  V-DISTRICT-ID         TO    P-DISTRICT-ID.             00054146
062100*    MOVE  V-FIN-INST-ID         TO    P-FIN-INST-ID.             00054147
122000*                                                                 00119504
122000*    MOVE  W-PROC-YR-CEN         TO  P-PROCESS-DATE.              00119507
122000*                                                                 00119510
062100*    MOVE  V-NOTE-MO             TO    P-NOTE-MO.                 00054149
062100*    MOVE  '/'                   TO    P-NOTE-SLASH1.             00054150
062100*    MOVE  V-NOTE-DA             TO    P-NOTE-DA.                 00054151
062100*    MOVE  '/'                   TO    P-NOTE-SLASH2.             00054152
062100*    MOVE  V-NOTE-YR             TO    P-NOTE-YR.                 00054153
062100*    MOVE  V-LOAN-NUMBER         TO    P-LAST-NAME.               00054154
062100*    IF    V-ELIG-FOR-LIFE = 'Y' OR 'N'                           00054155
062100*        MOVE  V-ELIG-FOR-LIFE   TO    P-ELIG-FOR-LIFE            00054155
062100*      ELSE                                                       00054155
062100*        MOVE  'N'               TO    P-ELIG-FOR-LIFE.           00054155
062100*    IF    V-ELIG-FOR-AH   = 'Y' OR 'N'                           00054155
062100*        MOVE  V-ELIG-FOR-AH     TO    P-ELIG-FOR-AH              00054156
062100*      ELSE                                                       00054156
062100*        MOVE  'N'               TO    P-ELIG-FOR-AH.             00054156
062100*    PERFORM BUILD-DOLLARS       THRU  BUILD-DOLLARS-EXIT.        00054157
122000*    MOVE  V-LOAN-APR            TO    BUILD-APR.                 00054158
122000*    MOVE  W-APR-WHOLE           TO    P-LOAN-APR-DOL.            00054159
122000*    MOVE  W-APR-DEC-FIRST2      TO    P-LOAN-APR-CEN.            00054160
122000*    MOVE  '.'                   TO    P-LOAN-APR-DEC.            00054161
122000*    MOVE  V-LOAN-OFFICER-ID     TO    P-LOAN-OFFICER-ID.         00054162
122000*    MOVE  V-LOAN-TERM           TO    P-LOAN-TERM.               00054163
122000*                                                                 00054165
122000*    ADD  1  TO  PPA-CNT.                                         00054166
122000*                                                                 00054166
122000*    WRITE  PPA-RECORD           FROM  PPA-REC.                   00054167
122000                                                                  00054168
122000 PPA-EXIT.                                                        00054168
122000     EXIT.                                                        00054168
122000                                                                  00054168
122000                                                                  00133840
062100 CK-FOR-LOGIC-BUILD.                                              00133841
122000                                                                  00133842
062100     IF  V-WRITTEN-PREM-INDIC  =  'Y'                             00133843
062100         NEXT SENTENCE                                            00133844
062100       ELSE                                                       00133845
061400         GO TO 010-READ-INPUT-FILE.                               00133846
064300                                                                  00133847
122000                                                                  00133848
122000     IF  V-ACCOUNT-ID  =  SAVE-FULL-ACCT                          00133849
064400         GO TO CK-ISS-CANC.                                       00133850
122000                                                                  00133851
122000                                                                  00133852
122000     PERFORM  ACCT-BREAK   THRU   ACCT-BREAK-EXIT.                00133853
122000                                                                  00133854
122000                                                                  00133855
064400 CK-ISS-CANC.                                                     00133856
064400                                                                  00133857
064500     MOVE 'Y'  TO  WRITTEN-PREM-SW.                               00133858
TSTMOD                                                                  00133859
TSTMOD* PULL NAMES TO INDIVIDUAL FIELDS FROM COMFED INPUT FILE.         00133860
TSTMOD                                                                  00133861
TSTMOD     PERFORM SPLIT-NAME-FIELD                                     00133862
TSTMOD        THRU SPLIT-NAME-FIELD-EXIT.                               00133863
064700                                                                  00133864
064800     IF  V-TRANS-TYPE =  '2'                                      00133865
062100         NEXT SENTENCE                                            00133866
062100       ELSE                                                       00133867
064800         GO  TO  CANCEL-BLD.                                      00133868
064500                                                                  00133869
080500 PROCESS-DETAILS.                                                 00133870
080600                                                                  00133871
080700*    DISPLAY 'PROCESS-DETAILS      ENTERED '.                     00133872
080900                                                                  00133873
152200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00133874
152300* * * * * * * *      START ISSUE PROCESSING     * * * * * * * *   00133875
152400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00133876
152900                                                                  00133877
082800*                ICARD                                            00133878
082900 ISSUE-BLD.                                                       00133879
083000                                                                  00133880
083100*    DISPLAY 'ISSUE-BLD      ENTERED  '.                          00133881
083200                                                                  00133882
083300* * * * * * *    BUILD THE CSO INPUT RECORDS   * * * * * * * *    00133883
083200                                                                  00133884
082900 SEQ1-BLD.                                                        00133885
083000                                                                  00133886
083300* *  BUILD THE SEQUENCE 1 RECORD   * * * * * * * *                00133887
093000                                                                  00133991
092400*    DISPLAY '* * SEQ1 - INSUREDS  FIELDS     * * '.              00133992
083200                                                                  00133888
057100     ADD  1                        TO  LOGIC-CNT.                 00133891
083200                                                                  00133888
083300     MOVE   'ISSUE -  SEQUENCE 1 ' TO  SAVE-SEQ.                  00133889
057100                                                                  00133890
057100     ADD  1                        TO  CERT-CNT.                  00133891
057100     ADD  1                        TO  CF-CERT-ISS.               00133892
057100     ADD  1                        TO  TOT-CERT-ISS.              00133893
057100                                                                  00133894
082100     MOVE   SPACES                 TO  PBI-RECORD-BODY.           00133895
088300                                                                  00133896
088500     MOVE  V-ISS-YR                TO  WORK-YR-X                  00133897
088500                                       WRK-ISS-YR.                00133897
088600     MOVE  V-ISS-MO                TO  WORK-MO-X                  00133898
088600                                       WRK-ISS-MO.                00133897
088700     MOVE  V-ISS-DA                TO  WORK-DA-X                  00133899
088700                                       WRK-ISS-DA.                00133897
088800     MOVE  WORK-DATE-X             TO  PBI-CERT-EFF-DT-X.         00133900
088800
088800**
088800**  THE FOLLOWING IF STATEMENT REFORMATS THE CERT NUMBER
088800**  IN ISSUE RECORDS EFFECTIVE PRIOR TO 06-01-99 SHIFTING
088800**  TRAILING ZEROS TO LEADING ZEROS IN CERT NUMBER.
088800**  PRIOR TO THIS DATE COMMERCIAL FEDERAL PROVIDED THE CERT
088800**  NUMBER RIGHT JUSTIFIED, NOW THEY ARE SENDING IT LEFT
088800**  JUSTIFIED.
088800**
088800     IF WORK-ISS-DTE-R LESS THAN 990601  AND
088800            WRK-ISS-YR-N GREATER THAN  50
088800        MOVE V-ISS-CERT-NUMBER       TO  WS-CERT-WORK1            00134174
088800        IF WS-CERT-WK1-ZEROS-4 = '0000'                           00134174
088800           MOVE WS-CERT-WK1-ZEROS-4 TO WS-CERT-WK2-ZEROS-4        00134174
088800           MOVE WS-CERT-WK1-NUM-6   TO WS-CERT-WK2-NUM-6          00134174
088800           MOVE WS-CERT-WORK2       TO SAVE-CERT                  00134174
088800        ELSE
088800           IF WS-CERT-WK1-ZEROS-3 = '000'                         00134174
088800              MOVE WS-CERT-WK1-ZEROS-3 TO WS-CERT-WK2-ZEROS-3     00134174
088800              MOVE WS-CERT-WK1-NUM-7   TO WS-CERT-WK2-NUM-7       00134174
088800              MOVE WS-CERT-WORK2       TO SAVE-CERT               00134174
088800           ELSE
088800              MOVE V-ISS-CERT-NUMBER TO SAVE-CERT                 00134174
088800           END-IF
088800        END-IF
088800     ELSE
088800        MOVE  V-ISS-CERT-NUMBER   TO SAVE-CERT                    00134174
088800     END-IF.
089000                                                                  00133901
090600**   MOVE  V-ISS-CERT-NUMBER       TO  SAVE-CERT.                 00133902
090600     MOVE  SAVE-CERT               TO  PBI-CERT-PRIME.            00133903
090600     MOVE  SPACES                  TO  PBI-CERT-SFX.              00133904
089000                                                                  00133906
TSTMOD     MOVE  V-FIRST-NAME            TO  PBI-I-INS-1ST-NAME.        00133907
TSTMOD     MOVE  SPACE                   TO  PBI-I-INS-MIDDLE-INIT.     00133908
TSTMOD*    MOVE  V-MID-INIT              TO  PBI-I-INS-MIDDLE-INIT.     00133908
TSTMOD     MOVE  V-LAST-NAME             TO  PBI-I-INS-LAST-NAME.       00133909
084800                                                                  00133910
083900     IF V-AGE NOT NUMERIC MOVE 40  TO  V-AGE.                     00133911
083700     MOVE V-AGE                    TO  PBI-I-INSURED-AGE-X.       00133912
083700     MOVE 'M'                      TO  PBI-I-INSURED-SEX.         00133913
083700     MOVE SPACES                   TO  PBI-I-SOC-SEC-NO.          00133914
083700     MOVE SPACES                   TO  PBI-I-MEMBER-NO.           00133915
083700     MOVE SPACES                   TO  PBI-I-BIRTHDAY-X.          00133916
083700     MOVE SPACES                   TO  PBI-I-ENTRY-CD.            00133917
083700     MOVE ' '                      TO  PBI-I-FORCE-CD.            00133918
083700     MOVE '2'                      TO  PBI-TRANS-TYPE.            00133919
083700     MOVE '1'                      TO  PBI-SEQUENCE.              00133920
083200                                                                  00133921
010800     MOVE  PBI-I-ISSUE-REC-SEQ-1   TO  OUT-INFO.                  00133922
010600     MOVE  SAVE-CERT               TO  OUT-CERT-NO.               00133923
010600     MOVE  SPACES                  TO  OUT-CERT-SFX.              00133924
088500     MOVE  WORK-DATE-X             TO  OUT-EFF-DATE.              00133925
010800     MOVE  '2'                     TO  OUT-TRAN-TYPE              00133926
010800     MOVE  '1'                     TO  OUT-SEQUENCE.              00133927
088300                                                                  00133928
088300                                                                  00133929
121900     PERFORM 0150-WRITE-DATA-CARD  THRU 0150-EXIT.                00133930
122000                                                                  00133931
083300* *  END OF  SEQUENCE 1 RECORD BUILD  * * * * * * * *.            00133932
083200                                                                  00133933
082900 SEQ1-EXIT.                                                       00133934
088000     EXIT.                                                        00133935
083200                                                                  00133936
083300* *  BUILD THE SEQUENCE 2 RECORD   * * * * * * * *.               00133937
083200                                                                  00133938
082900 SEQ2-BLD.                                                        00133939
093000                                                                  00133991
092400*    DISPLAY '* * SEQ2 - JOINT  FIELDS     * * '.                 00133992
057100                                                                  00133940
082100     MOVE   SPACES                 TO  PBI-RECORD-BODY.           00133941
083200                                                                  00133942
083300     MOVE   'ISSUE -  SEQUENCE 2 ' TO  SAVE-SEQ.                  00133943
088300                                                                  00133944
088500     MOVE   V-ISS-YR               TO  WORK-YR-X.                 00133945
088600     MOVE   V-ISS-MO               TO  WORK-MO-X.                 00133946
088700     MOVE   V-ISS-DA               TO  WORK-DA-X.                 00133947
088500     MOVE   WORK-DATE-N            TO  PBI-CERT-EFF-DT-X.         00133948
089000                                                                  00133949
090600     MOVE   SAVE-CERT              TO  PBI-CERT-PRIME.            00133950
090600     MOVE   SPACES                 TO  PBI-CERT-SFX.              00133951
084000                                                                  00133952
084000     IF  V-LF-COVG-TYPE = 'J'                                     00133953
084000         NEXT SENTENCE                                            00133954
084000       ELSE                                                       00133955
082900         GO TO SEQ3-BLD.                                          00133956
084000                                                                  00133957
084000     MOVE  SPACES  TO V-JT-FIRST-NAME                             00133958
084000     MOVE  'JOINT' TO V-JT-LAST-NAME                              00133959
084000     MOVE  SPACES  TO V-JT-MID-INIT                               00133960
084000     MOVE  '40'    TO V-JT-AGE.                                   00133961
084000                                                                  00133965
084700     MOVE   V-JT-LAST-NAME         TO  PBI-I-JNT-LAST-NAME.       00133966
084700     MOVE   V-JT-FIRST-NAME        TO  PBI-I-JNT-1ST-NAME.        00133967
084700     MOVE   V-JT-MID-INIT          TO  PBI-I-JNT-MIDDLE-INIT.     00133968
084800     MOVE   V-JT-AGE               TO  PBI-I-JOINT-AGE-X.         00133969
084800     MOVE   SPACES                 TO  PBI-I-POLICY-FORM-NO.      00133970
084000                                                                  00133971
083700     MOVE   '2'                    TO  PBI-TRANS-TYPE.            00133972
083700     MOVE   '2'                    TO  PBI-SEQUENCE.              00133973
083200                                                                  00133974
010800     MOVE   PBI-I-ISSUE-REC-SEQ-2  TO  OUT-INFO.                  00133975
010600     MOVE   SAVE-CERT              TO  OUT-CERT-NO.               00133976
010600     MOVE   SPACES                 TO  OUT-CERT-SFX.              00133977
088500     MOVE   WORK-DATE-X            TO  OUT-EFF-DATE.              00133978
010800     MOVE   '2'                    TO  OUT-TRAN-TYPE.             00133979
010800     MOVE   '2'                    TO  OUT-SEQUENCE.              00133980
088300                                                                  00133981
121900     PERFORM 0150-WRITE-DATA-CARD  THRU 0150-EXIT.                00133982
122000                                                                  00133983
083300* *  END OF  SEQUENCE 2 RECORD BUILD  * * * * * * * *.            00133984
088300                                                                  00133985
083300* *  BUILD THE SEQUENCE 3 RECORD   * * * * * * * *.               00133986
088300                                                                  00133987
082900 SEQ3-BLD.                                                        00133988
088300                                                                  00133989
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00133990
093000                                                                  00133991
092400*    DISPLAY '* * SEQ3 - FILL THE LIFE FIELDS     * * '.          00133992
083200                                                                  00133993
083300     MOVE  'ISSUE -  SEQUENCE 3 '  TO  SAVE-SEQ.                  00133994
092500                                                                  00133995
088500     MOVE   V-ISS-YR               TO  WORK-YR-X.                 00133996
088600     MOVE   V-ISS-MO               TO  WORK-MO-X.                 00133997
088700     MOVE   V-ISS-DA               TO  WORK-DA-X.                 00133998
088500     MOVE   WORK-DATE-X            TO  PBI-CERT-EFF-DT-X.         00133999
089000                                                                  00134000
090600     MOVE   SAVE-CERT              TO  PBI-CERT-PRIME.            00134001
090600     MOVE   SPACES                 TO  PBI-CERT-SFX.              00134002
093900                                                                  00134003
094000****************************************************************  00134003
094000****************************************************************  00134003
094000                                                                  00134003
094000     IF  V-STATE-CODE = 'CO' OR 'IA'                              00133766
094000         GO TO LIFE-TYPE-CHG-CO-IA.                               00134003
094000                                                                  00133768
094000     IF  V-STATE-CODE = 'KS' OR 'NE' OR 'OK' OR 'AZ' OR 'IL' OR   00133766
094000                        'MN' OR 'MO' OR 'SD' OR 'WI'              00133766
094000         GO TO LIFE-TYPE-CHG-OTHER.                               00134003
094000                                                                  00134003
094000     GO TO LIFE-TYPE-CHG-EXIT.                                    00134003
094000                                                                  00133765
094000 LIFE-TYPE-CHG-CO-IA.                                             00134003
094000                                                                  00133765
094000     IF  V-LF-BEN-CODE = '01'                                     00133766
094000*        MOVE  '82'  TO  V-LF-BEN-CODE                            00134004
060503         MOVE  '86'  TO  V-LF-BEN-CODE                            00134004
094000         GO TO LIFE-TYPE-CHG-EXIT.                                00134003
094000                                                                  00133765
094000     IF  V-LF-BEN-CODE = '02'                                     00133766
094000         MOVE  '84'  TO  V-LF-BEN-CODE                            00134004
094000         GO TO LIFE-TYPE-CHG-EXIT.                                00134003
094000                                                                  00133765
094000     IF  V-LF-BEN-CODE = '03'                                     00133766
094000         MOVE  '85'  TO  V-LF-BEN-CODE                            00134004
060503*        MOVE  '87'  TO  V-LF-BEN-CODE                            00134004
094000         GO TO LIFE-TYPE-CHG-EXIT.                                00134003
094000                                                                  00133765
094000     IF  V-LF-BEN-CODE = '04'                                     00133766
094000*        MOVE  '83'  TO  V-LF-BEN-CODE                            00134004
060503         MOVE  '87'  TO  V-LF-BEN-CODE                            00134004
094000         GO TO LIFE-TYPE-CHG-EXIT.                                00134003
094000                                                                  00133765
094000     GO TO LIFE-TYPE-CHG-EXIT.                                    00134003
094000                                                                  00134003
094000****************************************************************  00134003
094000****************************************************************  00134003
094000                                                                  00134003
094000 LIFE-TYPE-CHG-OTHER.                                             00134003
094000                                                                  00134003
094000     IF  V-LF-BEN-CODE = '01'                                     00133766
094000*        MOVE  'SR'  TO  V-LF-BEN-CODE                            00134004
060503         MOVE  'NP'  TO  V-LF-BEN-CODE                            00134004
094000         GO TO LIFE-TYPE-CHG-EXIT.                                00134003
094000                                                                  00133765
094000     IF  V-LF-BEN-CODE = '02'                                     00133766
094000         MOVE  'SL'  TO  V-LF-BEN-CODE                            00134004
094000         GO TO LIFE-TYPE-CHG-EXIT.                                00134003
094000                                                                  00133765
094000     IF  V-LF-BEN-CODE = '03'                                     00133766
094000         MOVE  'JL'  TO  V-LF-BEN-CODE                            00134004
060503*        MOVE  'JP'  TO  V-LF-BEN-CODE                            00134004
094000         GO TO LIFE-TYPE-CHG-EXIT.                                00134003
094000                                                                  00133765
094000     IF  V-LF-BEN-CODE = '04'                                     00133766
060503         MOVE 'JP'   TO  V-LF-BEN-CODE
094000*        MOVE  'JR'  TO  V-LF-BEN-CODE                            00134004
094000         GO TO LIFE-TYPE-CHG-EXIT.                                00134003
094000                                                                  00133765
094000     GO TO LIFE-TYPE-CHG-EXIT.                                    00134003
094000                                                                  00133765
094000****************************************************************  00134003
094000                                                                  00133765
094000 LIFE-TYPE-CHG-EXIT.                                              00134003
094000                                                                  00133765
094000****************************************************************  00134003
094000                                                                  00133765
094000     MOVE   V-LF-BEN-CODE          TO PBI-I-LF-BENEFIT-TYPE.      00134004
093900                                                                  00134005
092000     MOVE  V-LF-TERM               TO  PBI-I-LF-TERM-X.           00134006
089000                                                                  00134007
092800*    IF     V-LF-BEN-AMT NOT NUMERIC                              00134008
092800*                      MOVE ZEROS  TO V-LF-BEN-AMT.               00134009
092800*    IF     PBI-I-LF-BENEFIT-AMT NOT NUMERIC                      00134010
092800*                      MOVE ZEROS  TO PBI-I-LF-BENEFIT-AMT.       00134011
092800*    ADD    V-LF-BEN-AMT           TO  PBI-I-LF-BENEFIT-AMT.      00134012

060503     IF V-PRINC-AMT NOT NUMERIC
060503        MOVE ZEROS               TO V-PRINC-AMT
060503     END-IF
060503     IF PBI-I-LF-BENEFIT-AMT NOT NUMERIC
060503        MOVE ZEROS               TO PBI-I-LF-BENEFIT-AMT
060503     END-IF
060503     ADD V-PRINC-AMT             TO PBI-I-LF-BENEFIT-AMT

093500     IF    V-LIFE-PREM-WRITTEN NOT NUMERIC                        00134014
093500                      MOVE ZEROS   TO V-LIFE-PREM-WRITTEN.        00134018
057100                                                                  00134013
092800     IF     PBI-I-LF-PREM-AMT      NOT NUMERIC                    00134019
092800                       MOVE ZEROS  TO PBI-I-LF-PREM-AMT.          00134020
119800                                                                  00134021
093500     ADD   V-LIFE-PREM-WRITTEN     TO  PBI-I-LF-PREM-AMT          00134022
176600                                        TOT-LIFE-WRITTEN          00134023
157400                                        CF-LF-WRITTEN.            00134024
119800                                                                  00134025
060503     IF PBI-I-LF-PREM-AMT = ZEROS
060503        MOVE ZEROS               TO PBI-I-LF-BENEFIT-AMT
060503     END-IF
060503
093500     MOVE  ZEROS                   TO  PBI-I-LF-CRIT-PERIOD.      00134026
119800                                                                  00134027
093500     MOVE  ZEROS                   TO  PBI-I-LF-EXPIRE-YR.        00134028
093500     MOVE  ZEROS                   TO  PBI-I-LF-EXPIRE-MO.        00134029
093500     MOVE  ZEROS                   TO  PBI-I-LF-EXPIRE-DA.        00134030
120100                                                                  00134031
093500     MOVE V-LF-ALT-BEN-AMT         TO  PBI-I-LF-ALT-BENEFIT-AMT.  00134032
093500     MOVE V-LF-ALT-PREM-AMT        TO  PBI-I-LF-ALT-PREM-AMT.     00134033
120100                                                                  00134034
093500     MOVE  V-LF-TERM               TO  PBI-I-TERM-AS-DAYS.        00134035
084000                                                                  00134036
083700     MOVE '2'                      TO  PBI-TRANS-TYPE.            00134037
083700     MOVE '3'                      TO  PBI-SEQUENCE.              00134038
083200                                                                  00134039
088300                                                                  00134040
010800     MOVE  PBI-I-ISSUE-REC-SEQ-3   TO  OUT-INFO.                  00134041
010600     MOVE  SAVE-CERT               TO  OUT-CERT-NO.               00134042
010600     MOVE  SPACES                  TO  OUT-CERT-SFX.              00134043
088500     MOVE  WORK-DATE-X             TO  OUT-EFF-DATE.              00134044
010800     MOVE  '2'                     TO  OUT-TRAN-TYPE              00134045
010800     MOVE  '3'                     TO  OUT-SEQUENCE.              00134046
088300                                                                  00134047
121900     PERFORM  0150-WRITE-DATA-CARD THRU 0150-EXIT.                00134048
122000                                                                  00134049
093500     MOVE ZEROS                    TO  PBI-I-LF-PREM-AMT          00134050
092800                                       PBI-I-LF-BENEFIT-AMT.      00134051
122000                                                                  00134052
083300* *  END OF  SEQUENCE 3 RECORD BUILD  * * * * * * * *             00134053
088300                                                                  00134054
083300* *  BUILD THE SEQUENCE 4 RECORD    * * * * * * * * *             00134055
083200                                                                  00134056
082900 SEQ4-BLD.                                                        00134057
088300                                                                  00134058
106900*   PROCESS-A-H.                                                  00134059
088300                                                                  00134060
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00134061
107000                                                                  00134062
107100*    DISPLAY '* *  SEQ4 -  FILL THE AH FIELDS     * * '.          00134063
083200                                                                  00134064
083300     MOVE  'ISSUE -  SEQUENCE 4 '   TO  SAVE-SEQ.                 00134065
092500                                                                  00134066
088500     MOVE   V-ISS-YR                TO  WORK-YR-X.                00134067
088600     MOVE   V-ISS-MO                TO  WORK-MO-X.                00134068
088700     MOVE   V-ISS-DA                TO  WORK-DA-X.                00134069
088500     MOVE   WORK-DATE-X             TO  PBI-CERT-EFF-DT-X.        00134070
089000                                                                  00134071
090600     MOVE   SAVE-CERT               TO  PBI-CERT-PRIME.           00134072
090600     MOVE   SPACES                  TO  PBI-CERT-SFX.             00134073
107200                                                                  00134074
093900                                                                  00134075
119500     IF   V-AH-PREM-WRITTEN    GREATER THAN ZEROS                 00134076
119500           NEXT  SENTENCE                                         00134077
119500       ELSE                                                       00134078
119500           MOVE  ZEROS              TO  PBI-I-AH-PREM-AMT         00134079
119500           MOVE  ZEROS              TO  PBI-I-AH-BENEFIT-AMT      00134080
094000           MOVE  ZEROS              TO  PBI-I-AH-BENEFIT-POS1     00134081
094000           MOVE  ZEROS              TO  PBI-I-AH-BENEFIT-NO       00134082
119500           MOVE  ZEROS              TO  PBI-I-AH-TERM             00134083
119500           MOVE  ZEROS              TO  PBI-I-AH-CRIT-PERIOD      00134084
119500           MOVE  ZEROS              TO  PBI-I-AH-TERM             00134085
119500           MOVE  ZEROS              TO  PBI-I-AH-CRIT-PERIOD      00134086
093500           MOVE  ZEROS              TO  PBI-I-AH-EXPIRE-YR        00134087
093500           MOVE  ZEROS              TO  PBI-I-AH-EXPIRE-MO        00134088
093500           MOVE  ZEROS              TO  PBI-I-AH-EXPIRE-DA        00134089
119500           MOVE  ZEROS              TO  PBI-I-AH-PAYMENT          00134090
010600           MOVE  SAVE-CERT          TO  OUT-CERT-NO               00134091
010600           MOVE  SPACES             TO  OUT-CERT-SFX              00134092
010600           MOVE  WORK-DATE-N        TO  OUT-EFF-DATE              00134093
010800           MOVE  PBI-I-ISSUE-REC-SEQ-4  TO  OUT-INFO              00134094
010800           MOVE  '2'                TO  OUT-TRAN-TYPE             00134095
010800           MOVE  '4'                TO  OUT-SEQUENCE              00134096
010800           GO  TO  AH-WRITE.                                      00134097
093900                                                                  00134098
119500     IF   V-AH-PREM-WRITTEN  NOT NUMERIC                          00134099
119500                         MOVE ZEROS TO V-AH-PREM-WRITTEN.         00134100
119500     IF   PBI-I-AH-PREM-AMT         NOT NUMERIC                   00134101
119500                        MOVE  ZEROS TO  PBI-I-AH-PREM-AMT.        00134102
119500     ADD  V-AH-PREM-WRITTEN         TO  PBI-I-AH-PREM-AMT         00134103
119500                                        CF-AH-WRITTEN             00134104
119500                                        TOT-AH-WRITTEN.           00134105
119500                                                                  00134106
119500     IF   V-AH-BEN-AMT NOT NUMERIC                                00134107
119500                         MOVE ZEROS TO  V-AH-BEN-AMT.             00134108
119500     IF   PBI-I-AH-BENEFIT-AMT      NOT NUMERIC                   00134109
119500                        MOVE  ZEROS TO  PBI-I-AH-BENEFIT-AMT.     00134110
119600     ADD  V-AH-BEN-AMT              TO  PBI-I-AH-BENEFIT-AMT.     00134111
119600                                                                  00134106
119600     IF   V-AH-BEN-CODE   EQUAL    TO  '000'
119600          MOVE  '   '              TO  V-AH-BEN-CODE.
119600                                                                  00134106
119600     MOVE V-AH-TERM                 TO  PBI-I-AH-TERM-X.          00134115
119600     GO TO SKIP-BENE-CALC.                                        00134112
119600                                                                  00134106
119600****************************************************************  00134003
119600***  THE FOLLOWING LOGIC IS BEING BYPASSED, PER KAREN HENSON ***  00134003
119600****************************************************************  00134003
119600                                                                  00134116
119600     MOVE V-AH-TERM                 TO  V-TERM-X.                 00134115
119600     IF   V-TERM-N                  NOT NUMERIC                   00134115
119600          MOVE ZEROS                TO V-TERM-N.                  00134115
119600     MOVE V-AH-TERM                 TO  PBI-I-AH-TERM-X.          00134115
119600                                                                  00134116
119600     IF   V-AH-BEN-CODE      EQUAL  '000'                         00134113
119600          GO TO SKIP-BENE-CALC.                                   00134112
119600                                                                  00134116
119600     IF   V-TERM-N                  = ZEROS                       00134115
119600          DISPLAY ' '                                             00134115
119600          DISPLAY                                                 00134115
119600            'A&H TERM NOT PROVIDED FOR CERT #  ' SAVE-CERT        00134115
119600            '  -  NO AH BENEFIT AMOUNT CALCULATED '               00134115
119600          DISPLAY ' '                                             00134115
119600          GO TO SKIP-BENE-CALC.                                   00134112
119600                                                                  00134116
119600     DIVIDE V-TERM-N  INTO  V-AH-BEN-AMT                          00134107
119600            GIVING V-AH-BEN-AMT-WORK.                             00134107
119600                                                                  00134112
119600     ADD  V-AH-BEN-AMT-WORK         TO  PBI-I-AH-BENEFIT-AMT.     00134111
119600                                                                  00134112
119600 SKIP-BENE-CALC.                                                  00134112
119600                                                                  00134112
119600     IF  V-STATE-CODE = 'CO'                                      00133766
119600         NEXT SENTENCE                                            00133767
119600       ELSE                                                       00133768
119600         GO TO AH-TYPE-CHG-EXIT.                                  00134003
119600                                                                  00134003
119600 AH-TYPE-CHG.                                                     00134003
119600                                                                  00134003
119600     IF  V-AH-BEN-CODE = '001'                                    00133766
119600         MOVE '11'    TO   V-AH-BEN-CODE2                         00134004
119600         GO TO AH-TYPE-CHG-EXIT.                                  00134003
119600                                                                  00134003
119600     IF  V-AH-BEN-CODE = '054'                                    00133766
119600         MOVE '59'    TO   V-AH-BEN-CODE2                         00134004
119600         GO TO AH-TYPE-CHG-EXIT.                                  00134003
119600                                                                  00134003
119600 AH-TYPE-CHG-EXIT.                                                00134003
119600                                                                  00134003
119600     MOVE SPACES                    TO V-AH-BEN-CODE1.            00134113
119600                                                                  00134112
119600****************************************************************  00134003
119600****************************************************************  00134003
193000                                                                  00134112
193000     MOVE V-AH-BEN-CODE             TO PBI-I-AH-BENEFIT-TYPE.     00134113
193000                                                                  00134114
193000     MOVE V-AH-CRIT-PERIOD          TO  PBI-I-AH-CRIT-PERIOD.     00134117
193000                                                                  00134118
193500     MOVE ZEROS                     TO  PBI-I-AH-EXPIRE-YR.       00134119
193500     MOVE ZEROS                     TO  PBI-I-AH-EXPIRE-MO.       00134120
193500     MOVE ZEROS                     TO  PBI-I-AH-EXPIRE-DA.       00134121
193500                                                                  00134122
193500     MOVE ZEROS                     TO  PBI-I-AH-PAYMENT.         00134123
193500                                                                  00134124
083700     MOVE '2'                       TO  PBI-TRANS-TYPE.           00134125
083700     MOVE '4'                       TO  PBI-SEQUENCE.             00134126
083200                                                                  00134127
010800     MOVE  PBI-I-ISSUE-REC-SEQ-4    TO  OUT-INFO.                 00134128
010600     MOVE  SAVE-CERT                TO  OUT-CERT-NO.              00134129
010600     MOVE  SPACES                   TO  OUT-CERT-SFX.             00134130
088500     MOVE  WORK-DATE-X              TO  OUT-EFF-DATE.             00134131
010800     MOVE  '2'                      TO  OUT-TRAN-TYPE             00134132
010800     MOVE  '4'                      TO  OUT-SEQUENCE.             00134133
088300                                                                  00134134
088300 AH-WRITE.                                                        00134134
088300                                                                  00134134
121900     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00134140
122000                                                                  00134141
093500     MOVE ZEROS                     TO  PBI-I-AH-PREM-AMT         00134142
092800                                        PBI-I-AH-BENEFIT-AMT.     00134143
122000                                                                  00134144
122000                                                                  00134145
083300* *  END OF  SEQUENCE 4 RECORD BUILD  * * * * * * * *             00134146
122000                                                                  00134147
083300* *  BUILD THE SEQUENCE 5 RECORD   * * * * * * * *.               00095920
083200                                                                  00095930
082900 SEQ5-BLD.                                                        00095940
057100                                                                  00095950
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00095960
083200                                                                  00095970
083300     MOVE  'ISSUE -  SEQUENCE 5 '  TO  SAVE-SEQ.                  00095980
088300                                                                  00095990
088500     MOVE   V-ISS-YR               TO  WORK-YR-X.                 00095991
088600     MOVE   V-ISS-MO               TO  WORK-MO-X.                 00095992
088700     MOVE   V-ISS-DA               TO  WORK-DA-X.                 00095993
088500     MOVE   WORK-DATE-X            TO  PBI-CERT-EFF-DT-X.         00095994
089000                                                                  00095995
090600     MOVE   SAVE-CERT              TO  PBI-CERT-PRIME.            00095996
090600     MOVE   SPACES                 TO  PBI-CERT-SFX.              00095997
084000                                                                  00095998
084700     MOVE   SPACES                 TO  PBI-I-LOAN-OFFICER.        00095999
084700     MOVE   V-LOAN-TERM            TO  PBI-I-LOAN-TERM-X.         00096000
084700     MOVE   V-PAYMENT-DATE         TO  PBI-I-1ST-PMT-DT.          00096010
084800     MOVE   SPACES                 TO  PBI-I-EXT-DAYS-X           00096020
084800                                       PBI-I-INDV-GRP-CD          00096030
084800                                       PBI-I-REIN-CODE            00096031
084800                                       PBI-I-SIG-SW               00096032
084800                                       PBI-I-LIVES-X              00096033
084800                                       PBI-I-PAY-FREQUENCY-X      00096034
084800                                       PBI-I-RATE-CLASS           00096035
084800                                       PBI-I-SKIP-CODE            00096036
084800                                       PBI-I-BENEFICIARY          00096037
084800                                       PBI-I-TERM-TYPE.           00096038
084000                                                                  00096039
084700     IF     V-LOAN-APR   NOT NUMERIC                              00096040
084700            MOVE  ZEROS            TO  V-LOAN-APR.                00096041
084700                                                                  00096042
084700     MOVE   V-LOAN-APR             TO  WS-APR.                    00096046
084700     MOVE   WS-APR-R               TO  PBI-I-LOAN-APR.            00096046
084700                                                                  00096047
085000     MOVE   '2'                    TO  PBI-TRANS-TYPE.            00096050
085000     MOVE   '5'                    TO  PBI-SEQUENCE.              00096060
085000                                                                  00096070
086000     MOVE   PBI-I-ISSUE-REC-SEQ-5  TO  OUT-INFO.                  00096080
086000     MOVE   SAVE-CERT              TO  OUT-CERT-NO.               00096090
086000     MOVE   SPACES                 TO  OUT-CERT-SFX.              00096091
086000     MOVE   WORK-DATE-X            TO  OUT-EFF-DATE.              00096092
086000     MOVE   '2'                    TO  OUT-TRAN-TYPE.             00096093
086000     MOVE   '5'                    TO  OUT-SEQUENCE.              00096094
088300                                                                  00096095
121900     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00096110
122000                                                                  00096120
122300 I-CARD-END.                                                      00134148
122400                                                                  00134149
122100*    DISPLAY 'GOING TO 010-READ-INPUT-FILE FROM I-CARD-END'.      00134150
122400                                                                  00134149
122100     GO  TO  010-READ-INPUT-FILE.                                 00134150
092500                                                                  00134151
122500                                                                  00134152
122600* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00134153
122700* * * * * * * * *   END  ISSUE   PROCESSING   * * * * * * * * *   00134154
122800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00134155
122900                                                                  00134156
151300                                                                  00134157
152200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00134158
152300* * * * * * * *   START  CANCELLATION PROCESSING  * * * * * * *   00134159
152400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00134160
152100                                                                  00134161
122900*                   ACARD                                         00134162
123000 CANCEL-BLD.                                                      00134163
088300                                                                  00134164
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00134165
123100                                                                  00134166
123200*    DISPLAY 'CANCEL-BLD  ENTERED '.                              00134167
092500                                                                  00134168
057100     ADD  1                          TO  LOGIC-CANC-CNT.          00133891
088500     ADD  1                          TO  CANC-CNT.                00134169
092500                                                                  00134170
088500     MOVE   V-ISS-YR                 TO  WORK-YR-X                00134171
088500                                         WRK-ISS-YR.
088600     MOVE   V-ISS-MO                 TO  WORK-MO-X                00134172
088600                                         WRK-ISS-MO.
088700     MOVE   V-ISS-DA                 TO  WORK-DA-X                00134173
088700                                         WRK-ISS-DA.
088800**
088800**  THE FOLLOWING IF STATEMENT REFORMATS THE CERT NUMBER
088800**  IN CANCEL RECORDS ON ISSUES PRIOR TO 06-01-99 TO
088800**  SHIFT TRAILING ZEROS TO LEADING ZEROS IN CERT NUMBER,
088800**  IN ORDER TO MATCH THE PRIOR ISSUE RECORD CERT NUMBER.
088800**
088800     IF WORK-ISS-DTE-R LESS THAN 990601  AND
088800            WRK-ISS-YR-N GREATER THAN  50
088800        MOVE V-ISS-CERT-NUMBER       TO  WS-CERT-WORK1            00134174
088800        IF WS-CERT-WK1-ZEROS-4 = '0000'                           00134174
088800           MOVE WS-CERT-WK1-ZEROS-4 TO WS-CERT-WK2-ZEROS-4        00134174
088800           MOVE WS-CERT-WK1-NUM-6   TO WS-CERT-WK2-NUM-6          00134174
088800           MOVE WS-CERT-WORK2       TO SAVE-CERT                  00134174
088800        ELSE
088800           IF WS-CERT-WK1-ZEROS-3 = '000'                         00134174
088800              MOVE WS-CERT-WK1-ZEROS-3 TO WS-CERT-WK2-ZEROS-3     00134174
088800              MOVE WS-CERT-WK1-NUM-7   TO WS-CERT-WK2-NUM-7       00134174
088800              MOVE WS-CERT-WORK2       TO SAVE-CERT               00134174
088800           ELSE
088800              MOVE V-ISS-CERT-NUMBER TO SAVE-CERT                 00134174
088800           END-IF
088800        END-IF
088800     ELSE
088800        MOVE  V-ISS-CERT-NUMBER   TO SAVE-CERT                    00134174
088800     END-IF.
088800     MOVE   WORK-DATE-X              TO  PBI-CERT-EFF-DT-X.       00134175
089000                                                                  00134176
090600     MOVE   SAVE-CERT                TO  PBI-CERT-PRIME.          00134177
090600     MOVE   SPACES                   TO  PBI-CERT-SFX.            00134178
090600                                                                  00134179
090600     MOVE   SPACES                   TO  PBI-C-LIVES-X.           00134180
090600     MOVE   SPACES                   TO  PBI-C-FORCE-CD.          00134181
084000                                                                  00134182
125400     MOVE   V-LAST-NAME              TO   PBI-C-INSURED-NAME.     00134183
125500                                                                  00134184
134300* *     FILL THE CANCEL LIFE FIELDS     * * * * * * * *           00134185
088300                                                                  00134186
134500     IF     V-LF-COVG-CANC-DATE-N    NOT NUMERIC                  00134187
134500       MOVE ZEROS                    TO  PBI-C-LF-CANCEL-DATE     00134188
088700      ELSE                                                        00134189
088700       MOVE V-LF-COVG-CANC-DATE-N    TO  PBI-C-LF-CANCEL-DATE.    00134190
134900                                                                  00134191
134500     IF     V-LF-CANC-REFUND         NOT  NUMERIC                 00134192
134500            MOVE  ZEROS              TO  V-LF-CANC-REFUND.        00134193
134500     IF     PBI-C-LF-PREM-REFUND     NOT  NUMERIC                 00134194
134500            MOVE  ZEROS              TO  PBI-C-LF-PREM-REFUND.    00134195
134500     ADD    V-LF-CANC-REFUND         TO PBI-C-LF-PREM-REFUND.     00134196
128600                                                                  00134197
134300* *     FILL THE CANCEL A-H  FIELDS     * * * * * * * *           00134198
088300                                                                  00134199
088500     IF     V-AH-COVG-CANC-DATE-N    NOT NUMERIC                  00134200
088600      MOVE ZEROS                     TO  PBI-C-AH-CANCEL-DATE     00134201
088700     ELSE                                                         00134202
088700      MOVE V-AH-COVG-CANC-DATE-N     TO PBI-C-AH-CANCEL-DATE.     00134203
134900                                                                  00134204
134500     IF    V-AH-CANC-REFUND          NOT NUMERIC                  00134205
134500           MOVE  ZEROS               TO  V-AH-CANC-REFUND.        00134206
134500     IF    PBI-C-AH-PREM-REFUND      NOT  NUMERIC                 00134207
134500           MOVE  ZEROS               TO  PBI-C-AH-PREM-REFUND.    00134208
134500     ADD  V-AH-CANC-REFUND           TO PBI-C-AH-PREM-REFUND.     00134209
093900                                                                  00134210
083700     MOVE '3'                        TO  PBI-C-TRANS-TYPE.        00134211
083700     MOVE '1'                        TO  PBI-C-SEQUENCE.          00134212
137000                                                                  00134213
140200 WRITE-CANCL-REC.                                                 00134214
137000                                                                  00134215
140200*    DISPLAY 'WRITE-CANCL-REC   ENTERED    '.                     00134216
148000*    DISPLAY  OUT-RECORD.                                         00134217
088300                                                                  00134218
088500     MOVE   V-ISS-YR                 TO  WORK-YR-X.               00134219
088600     MOVE   V-ISS-MO                 TO  WORK-MO-X.               00134220
088700     MOVE   V-ISS-DA                 TO  WORK-DA-X.               00134221
088700     MOVE   WORK-DATE-X              TO  PBI-CERT-EFF-DT-X.       00134222
089000                                                                  00134223
090600     MOVE   SAVE-CERT                TO  PBI-CERT-PRIME.          00134224
090600     MOVE   SPACES                   TO  PBI-CERT-SFX.            00134225
084000                                                                  00134226
010800     MOVE  PBI-C-CANCEL-REC-SEQ-1    TO  OUT-INFO.                00134227
010600     MOVE  SAVE-CERT                 TO  OUT-CERT-NO.             00134228
010600     MOVE  SPACES                    TO  OUT-CERT-SFX.            00134229
010700     MOVE  WORK-DATE-X               TO  OUT-EFF-DATE.            00134230
010800     MOVE  '3'                       TO  OUT-TRAN-TYPE.           00134231
010800     MOVE  '1'                       TO  OUT-SEQUENCE.            00134232
088300                                                                  00134233
144900     ADD   1                         TO  DETAIL-CNT               00134234
144900                                         DETAIL-CNT-TOT.          00134235
144900     MOVE  DETAIL-CNT                TO  SR-REC-CNT.              00134236
144900     MOVE CF-BATCH-NUMB              TO  SR-BATCH-NUMB.           00134237
088300                                                                  00134238
148000     WRITE CARD-RECORD               FROM OUT-RECORD.             00134239
140300                                                                  00134240
143500     ADD  1                          TO CF-CERT-CANC.             00134241
143600     ADD  PBI-C-LF-PREM-REFUND       TO CF-LF-CANC.               00134242
143700     ADD  PBI-C-AH-PREM-REFUND       TO CF-AH-CANC.               00134243
176500                                                                  00134244
143500     ADD  1                          TO TOT-CERT-CANC.            00134245
143600     ADD  PBI-C-LF-PREM-REFUND       TO TOT-LIFE-CANC.            00134246
143700     ADD  PBI-C-AH-PREM-REFUND       TO TOT-AH-CANC.              00134247
146200                                                                  00134248
146300     ADD   1                         TO RPT-CANCL-CNT.            00134249
146400     ADD   1                         TO RPT-LINE-CNT.             00134250
146500     IF   RPT-LINE-CNT               GREATER THAN  60             00134251
146600          PERFORM   005-ERR-RPT-HD   THRU ERR-RPT-HD-EXIT.        00134252
146800     MOVE  OUT-RECORD                TO  RPT-PT-LINE.             00134254
147100     MOVE  'CANCEL '                 TO  PT-ERROR.                00134255
147200                                                                  00134256
147900     MOVE  RPT-PT-LINE               TO  RPT-REC.                 00134257
148000     WRITE RPT-REC-OUT               AFTER ADVANCING 1  LINE.     00134258
148100     ADD   1                         TO  RPT-LINE-CNT.            00134259
148300     MOVE  SPACES                    TO RPT-REC.                  00134261
148400     WRITE RPT-REC-OUT               AFTER ADVANCING 1  LINE.     00134262
148500     MOVE  SPACES                    TO RPT-PT-LINE.              00134263
148600                                                                  00134264
143600     MOVE ZEROS                      TO PBI-C-LF-PREM-REFUND      00134265
143700                                        PBI-C-AH-PREM-REFUND.     00134266
137200                                                                  00134267
137200*    DISPLAY 'GOING TO 010-READ-INPUT-FILE FROM A-CARD-END '.     00134268
137200                                                                  00134267
137200     GO  TO  010-READ-INPUT-FILE.                                 00134268
137200                                                                  00134269
137300 A-CARD-END.                                                      00134270
137400     EXIT.                                                        00134271
151300                                                                  00134272
152200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00134273
152300* * * * * * * *   END  CANCELLATION PROCESSING  * * * * * * * *   00134274
152400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00134275
152100                                                                  00134276
153000******* * * * * * * * * * * * * * * * * * * * * * * * * * * *     00134277
152100                                                                  00134278
153200 0150-WRITE-DATA-CARD.                                            00134279
153300                                                                  00134280
153400*    DISPLAY '0150-WRITE-DATA-CARD ENTERED  '.                    00134281
152600                                                                  00134282
168300*    DISPLAY  OUT-RECORD.                                         00134287
158100                                                                  00134288
144900     ADD  1                    TO  DETAIL-CNT                     00134289
144900                                   DETAIL-CNT-TOT.                00134290
144900     MOVE  DETAIL-CNT          TO  SR-REC-CNT.                    00134291
144900     MOVE CF-BATCH-NUMB        TO  SR-BATCH-NUMB.                 00134292
168300     WRITE CARD-RECORD         FROM OUT-RECORD.                   00134293
158100                                                                  00134294
159800     ADD   1                   TO RPT-ISS-CNT.                    00134299
159900     ADD   1                   TO RPT-LINE-CNT.                   00134300
160000     IF   RPT-LINE-CNT         GREATER THAN  60                   00134301
160100        PERFORM 005-ERR-RPT-HD THRU ERR-RPT-HD-EXIT.              00134302
160300     MOVE  OUT-RECORD          TO  RPT-PT-LINE.                   00134304
160300     MOVE  SAVE-SEQ            TO  PT-ERROR.                      00134305
161400     MOVE  RPT-PT-LINE         TO  RPT-REC.                       00134306
161500     WRITE RPT-REC-OUT         AFTER ADVANCING 1  LINE.           00134307
161600     ADD  1                    TO  RPT-LINE-CNT.                  00134308
161800     MOVE  SPACES              TO RPT-REC.                        00134310
161900     WRITE RPT-REC-OUT         AFTER ADVANCING 1  LINE.           00134311
162000     MOVE  SPACES              TO RPT-PT-LINE.                    00134312
162100                                                                  00134313
165600 0150-EXIT.                                                       00134314
165700     EXIT.                                                        00134315
165800                                                                  00134316
165900 0200-ERRORS.                                                     00134317
166000                                                                  00134318
166100*    DISPLAY '0200-ERRORS  ENTERED '.                             00134319
166200                                                                  00134320
166300     DISPLAY '******************************************'.        00134321
166400     DISPLAY 'INPUT FILE HAS MORE THAN 10 ERRORS '.               00134322
166500     DISPLAY ' '.                                                 00134323
166600     DISPLAY '******************************************'.        00134324
167100                                                                  00134328
167200     GO TO END-OF-JOB.                                            00134329
167300                                                                  00134330
167400 0200-EXIT.                                                       00134331
167500      EXIT.                                                       00134332
122000                                                                  00134333
062100*BUILD-DOLLARS.                                                   00119514
122000*                                                                 00119515
122000*    MOVE  V-LF-CANC-REFUND    TO  W-LREFUND.                     00119516
122000*    MOVE  W-LREFUND-DOL5      TO  P-LF-CANC-REFUND-DOL.          00119517
122000*    MOVE  W-LREFUND-CEN       TO  P-LF-CANC-REFUND-CEN.          00119518
122000*    MOVE  '.'                 TO  P-LF-CANC-REFUND-DEC.          00119519
122000*                                                                 00119520
122000*    MOVE  V-AH-CANC-REFUND    TO  W-AREFUND.                     00119521
122000*    MOVE  W-AREFUND-DOL5      TO  P-AH-CANC-REFUND-DOL.          00119522
122000*    MOVE  W-AREFUND-CEN       TO  P-AH-CANC-REFUND-CEN.          00119523
122000*    MOVE  '.'                 TO  P-AH-CANC-REFUND-DEC.          00119524
122000*                                                                 00119525
122000*    MOVE  V-PRINC-AMT         TO  W-PR-NOTE-AMT.                 00119526
122000*    MOVE  W-PN-AMT-DOL        TO  P-PRINC-AMT-DOL.               00119527
122000*    MOVE  W-PN-AMT-CEN        TO  P-PRINC-AMT-CEN.               00119528
122000*    MOVE  '.'                 TO  P-PRINC-AMT-DEC.               00119529
122000*                                                                 00119530
122000*    MOVE  V-TOT-NOTE-AMT      TO  W-PR-NOTE-AMT.                 00119531
122000*    MOVE  W-PN-AMT-DOL        TO  P-TOT-NOTE-AMT-DOL.            00119532
122000*    MOVE  W-PN-AMT-CEN        TO  P-TOT-NOTE-AMT-CEN.            00119533
122000*    MOVE  '.'                 TO  P-TOT-NOTE-AMT-DEC.            00119534
122000*                                                                 00119535
122000*    MOVE  V-LIFE-PREM-WRITTEN TO  W-LREFUND.                     00119536
122000*    MOVE  W-LREFUND-DOL5      TO  P-LIFE-PREM-WRITTEN-DOL.       00119537
122000*    MOVE  W-LREFUND-CEN       TO  P-LIFE-PREM-WRITTEN-CEN.       00119538
122000*    MOVE  '.'                 TO  P-LIFE-PREM-WRITTEN-DEC.       00119539
122000*                                                                 00119540
122000*    MOVE  V-AH-PREM-WRITTEN   TO  W-AREFUND.                     00119541
122000*    MOVE  W-AREFUND-DOL5      TO  P-AH-PREM-WRITTEN-DOL.         00119542
122000*    MOVE  W-AREFUND-CEN       TO  P-AH-PREM-WRITTEN-CEN.         00119543
122000*    MOVE  '.'                 TO  P-AH-PREM-WRITTEN-DEC.         00119544
122000*                                                                 00119545
122000*                                                                 00134356
062100*BUILD-DOLLARS-EXIT.                                              00134357
062100*    EXIT.                                                        00134358
122000*                                                                 00134359
167600                                                                  00134360
064500 ACCT-BREAK.                                                      00134361
167600                                                                  00134362
158000*     DISPLAY 'ACCT-BREAK  ENTERED '.                             00134363
158100                                                                  00134364
167600      MOVE   '0'               TO   CF-SEQUENCE-NO.               00134365
167600      MOVE   SAVE-FULL-ACCT    TO   CF-ACCT-NO                    00134366
064500      MOVE   SAVE-STATE        TO   CF-STATE                      00134367
064500      MOVE   '1'               TO   CF-TRANS-TYPE.                00134368
064500      MOVE   '0'               TO   CF-SEQUENCE-NO.               00134369
064500      MOVE   'CSO'             TO   CF-CLIENT-ID.                 00134370
064500      MOVE   '9000000'         TO   CF-CARR-CO.                   00134371
158000                                                                  00134372
167600      IF  SAVE-FULL-ACCT  =    ZEROS OR SPACES                    00134373
167600        MOVE  ZEROS            TO   CF-BATCH-NO                   00134374
158000        DISPLAY ' '                                               00134375
158000        DISPLAY 'COMFED-BATCH-HDR IS ZEROS  '   COMFED-BATCH-HDR  00134376
158000         GO TO SKIP-WRITE.                                        00134377
158000                                                                  00134378
158000      MOVE    COMFED-BATCH-HDR TO OUT-RECORD.                     00134379
167600      MOVE    CF-BATCH-NUMB    TO   SR-BATCH-NUMB.                00134380
157900      MOVE    ZEROS            TO   SR-REC-CNT.                   00134381
167600                                                                  00134382
168300      ADD  1  TO  HDR-CNT.                                        00134383
167600                                                                  00134384
168300      WRITE CARD-RECORD        FROM OUT-RECORD.                   00134383
167600                                                                  00134384
064500 SKIP-WRITE.                                                      00134385
167600                                                                  00134386
158000      DISPLAY ' '.                                                00134387
158000      DISPLAY 'COMFED-BATCH-HDR  ' COMFED-BATCH-HDR.              00134388
158100                                                                  00134389
167600      ADD    1                 TO   CF-BATCH-NO.                  00134390
167600      MOVE  ZEROS              TO   CF-LF-WRITTEN                 00134391
167600                                    CF-LF-CANC                    00134392
167600                                    CF-AH-WRITTEN                 00134393
167600                                    CF-AH-CANC                    00134394
167600                                    CF-CERT-ISS                   00134395
167600                                    CF-CERT-CANC                  00134396
167600                                    CERT-CNT                      00134397
167600                                    CANC-CNT                      00134398
167600                                    DETAIL-CNT.                   00134399
167600                                                                  00134400
062100      IF  LAST-REC-SW = 'Y'                                       00134401
064500          GO TO ACCT-BREAK-EXIT.                                  00134402
167600                                                                  00134403
058700      MOVE  RPT-DT (WK-MO-N)   TO RPT-REPORT-DATE.                00134404
167600                                                                  00134405
064500      IF    V-STATE-CODE  =  ZEROS  OR  SPACES                    00134406
122000         MOVE  V-ACCOUNT-ID TO  SAVE-FULL-ACCT                    00134407
064500            GO TO ACCT-BREAK-EXIT.                                00134408
167600                                                                  00134409
064500      MOVE  V-STATE-CODE       TO  SAVE-STATE.                    00134410
167600                                                                  00134411
122000      MOVE  V-ACCOUNT-ID       TO  SAVE-FULL-ACCT.                00134412
167600                                                                  00134413
064500 ACCT-BREAK-EXIT.                                                 00134414
064500      EXIT.                                                       00134415
167600                                                                  00134416
064500                                                                  00134417
170500 END-OF-JOB SECTION.                                              00134418
158100                                                                  00134419
167900*    DISPLAY 'END-OF-JOB   ENTERED '.                             00134420
158100                                                                  00134421
063500      CLOSE COMFED-IN                                             00134422
063500            NAME-OUT                                              00134423
063500            COMFED-OUT.                                           00134423
171800                                                                  00134433
172200     MOVE  RPT-PAGE-CNT     TO RPT-PAGE.                          00134436
172300     MOVE  4                TO RPT-LINE-CNT.                      00134437
172500     MOVE '       COMFED INPUT TOTALS FOR ' TO CF-RPT-ID.         00134439
172600     MOVE  RPT-HD-LINE1     TO RPT-REC.                           00134440
172700     WRITE RPT-REC-OUT      AFTER ADVANCING PAGE.                 00134441
172900     MOVE  SPACES           TO RPT-REC.                           00134443
173000     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00134444
173200     MOVE  REPORT-HD-LINE   TO RPT-REC.                           00134446
173300     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00134447
173400     ADD  1                 TO  RPT-LINE-CNT.                     00134448
173600     MOVE  SPACES           TO RPT-REC.                           00134450
173700     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00134451
177200                                                                  00134452
177300     MOVE  TOT-LIFE-WRITTEN TO T-LIFE-WRITTEN.                    00134453
177400     MOVE  TOT-LIFE-CANC    TO T-LIFE-CANC.                       00134454
177500     MOVE  TOT-AH-WRITTEN   TO T-AH-WRITTEN.                      00134455
177600     MOVE  TOT-AH-CANC      TO T-AH-CANC.                         00134456
177700     MOVE  TOT-CERT-ISS     TO T-CERT-ISS.                        00134457
177800     MOVE  TOT-CERT-CANC    TO T-CERT-CANC.                       00134458
177800     MOVE  PPA-CNT          TO T-PPA-CNT.                         00134458
177900                                                                  00134459
178100     MOVE  TOTAL-LINE       TO RPT-REC.                           00134461
178200     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00134462
178300     ADD  1                 TO  RPT-LINE-CNT.                     00134463
178500     MOVE  SPACES           TO RPT-REC.                           00134465
178600     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00134466
178700                                                                  00134467
172000                                                                  00134435
171600     MOVE SPACES TO DISP-LINE.                                    00134431
172200     MOVE  RPT-PAGE-CNT     TO RPT-PAGE.                          00134436
172300     MOVE  4                TO RPT-LINE-CNT.                      00134437
172500     MOVE '       COMFED OUTPUT TOTALS  ' TO DISP-LINE.           00134439
172600     MOVE  DISP-LINE        TO RPT-REC.                           00134440
172700     WRITE RPT-REC-OUT      AFTER ADVANCING PAGE.                 00134441
172900     MOVE  SPACES           TO RPT-REC.                           00134443
173000     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00134444
171700     MOVE  '************************************************'     00134432
172500                                                    TO DISP-LINE. 00134439
173000     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00134444
172900     MOVE  SPACES           TO RPT-REC.                           00134443
173000     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00134444
171100     MOVE  'INPUT RECORDS      -- ' TO DISP-DESC.                 00134426
171100     MOVE  IN-CNT                   TO DISP-CNT.                  00134426
172600     MOVE  SPACES                   TO DISP-FILLER.               00134440
172600     MOVE  DISP-LINE                TO RPT-REC.                   00134440
172600     MOVE  SPACES                   TO DISP-FILLER.               00134440
172700     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134441
172900     MOVE  SPACES                   TO RPT-REC.                   00134443
173000     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134444
171300     MOVE  'RECS WRITTEN COUNT -- ' TO DISP-DESC.                 00134428
171400     MOVE  DETAIL-CNT-TOT           TO DISP-CNT.                  00134429
172600     MOVE  SPACES                   TO DISP-FILLER.               00134440
172600     MOVE  DISP-LINE                TO RPT-REC.                   00134440
172700     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134441
172900     MOVE  SPACES                   TO RPT-REC.                   00134443
173000     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134444
171300     MOVE  'HDRS WRITTEN COUNT -- ' TO DISP-DESC.                 00134428
171400     MOVE  HDR-CNT                  TO DISP-CNT.                  00134429
172600     MOVE  SPACES                   TO DISP-FILLER.               00134440
172600     MOVE  DISP-LINE                TO RPT-REC.                   00134440
172700     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134441
172900     MOVE  SPACES                   TO RPT-REC.                   00134443
173000     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134444
R71500     MOVE  'ERROR RECORD COUNT -- ' TO DISP-DESC.                 00134430
R71500     MOVE  RPT-CNT                  TO DISP-CNT.                  00134430
172600     MOVE  SPACES                   TO DISP-FILLER.               00134440
172600     MOVE  DISP-LINE                TO RPT-REC.                   00134440
172700     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134441
172900     MOVE  SPACES                   TO RPT-REC.                   00134443
173000     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134444
171500     MOVE  'LOGIC ISSUE  COUNT -- ' TO DISP-DESC.                 00134430
171500     MOVE  LOGIC-CNT                TO DISP-CNT.                  00134430
172600     MOVE  SPACES                   TO DISP-FILLER.               00134440
172600     MOVE  DISP-LINE                TO RPT-REC.                   00134440
172700     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134441
172900     MOVE  SPACES                   TO RPT-REC.                   00134443
173000     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134444
171500     MOVE  'LOGIC CANCEL COUNT -- ' TO DISP-DESC.                 00134430
171500     MOVE  LOGIC-CANC-CNT           TO DISP-CNT.                  00134430
172600     MOVE  SPACES                   TO DISP-FILLER.               00134440
172600     MOVE  DISP-LINE                TO RPT-REC.                   00134440
172700     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134441
172900     MOVE  SPACES                   TO RPT-REC.                   00134443
173000     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134444
171500     MOVE  'PP&A  RECORD COUNT -- ' TO DISP-DESC.                 00134430
171500     MOVE  PPA-CNT                  TO DISP-CNT.                  00134430
172600     MOVE  SPACES                   TO DISP-FILLER.               00134440
172600     MOVE  DISP-LINE                TO RPT-REC.                   00134440
172700     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134441
172900     MOVE  SPACES                   TO RPT-REC.                   00134443
173000     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134444
172900     MOVE  SPACES                   TO RPT-REC.                   00134443
173000     WRITE RPT-REC-OUT              AFTER ADVANCING 1  LINE.      00134444
171700     MOVE  '************************************************'     00134432
172500                                                    TO DISP-LINE. 00134439
173000     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00134444
172900     MOVE  SPACES           TO RPT-REC.                           00134443
173000     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00134444
062100                                                                  00134424
178800     CLOSE RPT-FILE.                                              00134468
178900                                                                  00134469
179000     GOBACK.                                                      00134470
179100                                                                  00134480
002900*                                                                 00134500
005400******************************************************************00134569
054900******************************************************************00134819
183800                                                                  00136679
146600 NAME-RPT-HD.                                                     00134252
183800                                                                  00136679
173000     WRITE NAME-REC  FROM  NAME-RPT-HDNG  AFTER ADVANCING PAGE.   00134444
173000     MOVE SPACES TO NAME-REC.                                     00134444
173000     WRITE NAME-REC                       AFTER ADVANCING 1 LINE. 00134444
173000     MOVE  2  TO  NAME-LINE-CNT.                                  00134444
183800                                                                  00136679
146600 NAME-RPT-HD-EXIT.                                                00134252
002900                                                                  00134500
005400******************************************************************00134569
054900******************************************************************00134819
TSTMOD                                                                  00134820
TSTMOD SPLIT-NAME-FIELD.                                                00134830
084800                                                                  00134890
175000                                                                  00136590
175100* * * * * * * * *   GET INSURED NAME    * * * * * * * * * * * * * 00136591
175200                                                                  00136592
175300     MOVE   'N'                TO  LETTER-SW.                     00136593
175300     MOVE   ZEROS              TO  SP-SUB.                        00136593
175300     MOVE   ZEROS              TO  SUB1.                          00136593
175400     MOVE   ZEROS              TO  SUB2.                          00136594
175400     MOVE   ZEROS              TO  SPACE-CNT.                     00136594
175500     MOVE   SPACES             TO  F-NAME.                        00136595
175600     MOVE   SPACES             TO  M-INIT.                        00136596
175700     MOVE   SPACES             TO  L-NAME.                        00136597
NEWMOD     MOVE   V-FULL-NAME        TO  NAME-IN.                       00136602
NEWMOD     MOVE   V-FULL-NAME        TO  SPACE-CK.                      00136602
DMBMOD*    DISPLAY  'ILN-SHT-NAME = ' NAME-IN.                          00136603
NEWMOD     MOVE   SPACES  TO         V-FULL-NAME.                       00136602
176300                                                                  00136604
176400*DISPLAY ' '.                                                     00136605
176500*DISPLAY ' START NAME PROCESSING '.                               00136606
176300                                                                  00136604
176300     MOVE  27  TO SP-SUB.                                         00136604
176300                                                                  00136604
176300 COUNT-SPACES.                                                    00136604
176300                                                                  00136604
176300     SUBTRACT  1  FROM SP-SUB.                                    00136604
176300                                                                  00136604
176300     IF SP-SUB  = ZERO                                            00136604
176300        GO TO CHECK-SPACE-COUNT.                                  00136604
176300                                                                  00136604
176300     IF SP-CK (SP-SUB) = ' '                                      00136604
176300       AND LETTER-SW = 'N'                                        00136604
176300        GO TO COUNT-SPACES.                                       00136604
176300                                                                  00136604
176300     MOVE 'Y' TO LETTER-SW.                                       00136604
176300                                                                  00136604
176300     IF SP-CK (SP-SUB) = ' '                                      00136604
176300       AND LETTER-SW = 'Y'                                        00136604
176300        ADD  1  TO SPACE-CNT.                                     00136604
176300                                                                  00136604
176300     GO TO COUNT-SPACES.                                          00136604
176300                                                                  00136604
176300 CHECK-SPACE-COUNT.                                               00136604
176300                                                                  00136604
176300     IF  SPACE-CNT > 2                                            00136604
183900         PERFORM FORMAT-ERROR-PRINT                               00136680
183900      THRU                                                        00136680
183900         FORMAT-ERROR-PRINT-EXIT.                                 00136680
176300                                                                  00136604
177100 BUILD-LAST-NAME.                                                 00136612
177100                                                                  00136612
177200     ADD    1   TO  SUB1.                                         00136613
177300                                                                  00136614
177800     IF     SUB1   GREATER THAN  26                               00136619
177800            GO TO NAME-DONE.                                      00136619
177300                                                                  00136614
177800     IF     ILN-SHT-NAME-IN (SUB1)   =   ' '                      00136619
177100            GO TO BUILD-FIRST-INIT.                               00136612
177300                                                                  00136614
177100                                                                  00136612
177200     ADD    1   TO  SUB2.                                         00136613
177300                                                                  00136614
177800     IF     SUB2   GREATER THAN  15                               00136619
177100            GO TO BUILD-FIRST-INIT.                               00136612
177300                                                                  00136614
177300                                                                  00136614
177400     MOVE   ILN-SHT-NAME-IN (SUB1)   TO  LN-15 (SUB2).            00136615
177100                                                                  00136612
177100     GO TO BUILD-LAST-NAME.                                       00136612
177300                                                                  00136614
177500                                                                  00136616
176600                                                                  00136607
177100 BUILD-FIRST-INIT.                                                00136612
177100                                                                  00136612
177200     ADD    1   TO  SUB1.                                         00136613
177300                                                                  00136614
177400     MOVE   ILN-SHT-NAME-IN (SUB1)   TO  FN-1.                    00136615
177100                                                                  00136612
177100                                                                  00136612
177100 GET-MID-INIT-ONLY.                                               00136612
177100                                                                  00136612
177200     ADD    1   TO  SUB1.                                         00136613
177300                                                                  00136614
177800     IF     ILN-SHT-NAME-IN (SUB1)   =   ' '                      00136619
177200            ADD    1   TO  SUB1                                   00136613
177400            MOVE   ILN-SHT-NAME-IN (SUB1)   TO  MI-1              00136615
183900            GO TO NAME-DONE.                                      00136680
177100                                                                  00136612
177100     GO TO GET-MID-INIT-ONLY.                                     00136612
177600                                                                  00136617
183800                                                                  00136679
183900 FORMAT-ERROR-PRINT.                                              00136680
183800                                                                  00136679
183900*     DISPLAY  ' '.                                               00136680
183900*     DISPLAY 'FORMAT-ERROR-PRINT'.                               00136680
183900*     DISPLAY                                                     00136680
183900*     'CERT NUMBER = ' V-ISS-CERT-NUMBER ' NAME = ' ILN-SHT-NAME  00136680
183900*     DISPLAY  ' '.                                               00136680
183800                                                                  00136679
146400     ADD   1                         TO NAME-LINE-CNT.            00134250
146500     IF   NAME-LINE-CNT              GREATER THAN  56             00134251
146600          PERFORM      NAME-RPT-HD   THRU NAME-RPT-HD-EXIT.       00134252
183800                                                                  00136679
183800      MOVE V-ISS-CERT-NUMBER TO CERT-NUM.                         00136679
183800      MOVE NAME-IN           TO NAME-IS.                          00136679
183800                                                                  00136679
148000     WRITE NAME-REC  FROM N-R-WORK   AFTER ADVANCING 2  LINES.    00134258
148600                                                                  00134264
183700                                                                  00136678
183900 FORMAT-ERROR-PRINT-EXIT.                                         00136680
183900     EXIT.                                                        00136680
183800                                                                  00136679
183900 NAME-DONE.                                                       00136680
183800                                                                  00136679
183800     MOVE   FN-1           TO  V-FIRST-NAME.                      00136679
183800     MOVE   MI-1           TO  V-MID-INIT.                        00136679
183800     MOVE   L-NAME         TO  V-LAST-NAME.                       00136679
183700                                                                  00136678
TSTMOD SPLIT-NAME-FIELD-EXIT.                                           00136694
TSTMOD     EXIT.                                                        00136700
999999                                                                  00138438
005400******************************************************************00138440
184000                                                                  00136681
184200                                                                  00136683
089000                                                                  00136688
054900******************************************************************00138500

