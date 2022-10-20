00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 EL9301.                              00000030
00004 *                            VMOD=2.006                           00000031
00005                                                                   00000050
00006  AUTHOR.     LOGIC,INC.                                           00000060
00007              DALLAS, TEXAS.                                       00000070
00008                                                                   00000080
00009  DATE-COMPILED.                                                   00000090
00010  SECURITY.   *****************************************************00000100
00011              *                                                   *00000110
00012              *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *00000120
00013              *                                                   *00000130
00014              *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *00000140
00015              *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *00000150
00016              *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *00000160
00017              *                                                   *00000170
00018              *****************************************************00000180
00019                                                                   00000190
00020  REMARKS. TRANSACTION - EXI2 - NEW BUSINESS - DATA ENTRY (ISSUES).00000200
00021                                                                   00000210
00022  ENVIRONMENT DIVISION.                                            00000220
00023                                                                   00000230
00024      EJECT                                                        00000240
00025  DATA DIVISION.                                                   00000250
00026  WORKING-STORAGE SECTION.                                         00000260
00027  77  FILLER  PIC X(32)  VALUE '*** PAN EL9301    PANLVL 012 ***'. 00000261
00028  77  FILLER  PIC X(32)  VALUE '********************************'. 00000262
00029  77  FILLER  PIC X(32)  VALUE '*    EL9301 WORKING STORAGE    *'. 00000263
00030  77  FILLER  PIC X(32)  VALUE '******VMOD=2.006****************'. 00000264
00031                                                                   00000265
00032                              COPY ELCSCTM.                        00000266
00033                                                                   00000267
00034                              COPY ELCSCRTY.                       00000268
00035                                                                   00000269
00036  01  STANDARD-AREAS.                                              00000270
00037      12  GETMAIN-SPACE       PIC X       VALUE SPACE.             00000271
00038      12  EL930B              PIC X(8)    VALUE 'EL930B  '.        00000272
00039      12  EL930C              PIC X(8)    VALUE 'EL930C  '.        00000273
00040      12  MAPSET-EL9301S      PIC X(8)    VALUE 'EL9301S '.        00000274
00041      12  SCREEN-NUMBER       PIC X(6)    VALUE 'EL930B'.          00000275
00042      12  TRANS-EXI2          PIC X(4)    VALUE 'EXI2'.            00000276
00043      12  THIS-PGM            PIC X(8)    VALUE 'EL9301  '.        00000277
00044      12  PGM-NAME            PIC X(8).                            00000278
00045      12  TIME-IN             PIC S9(7).                           00000279
00046      12  TIME-OUT-R  REDEFINES TIME-IN.                           00000280
00047          16  FILLER          PIC X.                               00000281
00048          16  TIME-OUT        PIC 99V99.                           00000282
00049          16  FILLER          PIC X(2).                            00000283
00050      12  LINK-EL001          PIC X(8)    VALUE 'EL001   '.        00000284
00051      12  LINK-EL004          PIC X(8)    VALUE 'EL004   '.        00000285
00052      12  XCTL-EL005          PIC X(8)    VALUE 'EL005   '.        00000286
00053      12  XCTL-EL010          PIC X(8)    VALUE 'EL010   '.        00000287
00054      12  XCTL-EL626          PIC X(8)    VALUE 'EL626   '.        00000288
00055      12  XCTL-EL930          PIC X(8)    VALUE 'EL930   '.        00000289
00056      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV '.        00000290
00057      12  FILE-ID-ERPNDT      PIC X(8)    VALUE 'ERPNDT  '.        00000291
00058      12  FILE-ID-ERPNDM      PIC X(8)    VALUE 'ERPNDM  '.        00000292
00059      12  FILE-ID-ELCERT      PIC X(8)    VALUE 'ELCERT  '.        00000293
00060      12  FILE-ID-ELCNTL      PIC X(8)    VALUE 'ELCNTL  '.        00000294
00061      12  FILE-ID-ERACCT2     PIC X(8)    VALUE 'ERACCT2 '.        00000295
00062      12  WS-CURRENT-DT       PIC X(8)    VALUE SPACES.            00000296
00063      12  WS-CURRENT-BIN-DT   PIC XX      VALUE SPACES.            00000297
00064      12  WS-TERM-IN-DAYS-SW  PIC X.                               00000298
00065          88  WS-TERM-IN-DAYS-FOUND       VALUE 'Y'.               00000299
00066      12  W-TEST-ZIP          PIC X.                               00000300
00067          88  W-CANADIAN-POST-CODE   VALUE 'A' THRU 'Z'.           00000301
00068                                                                   00000302
00069      EJECT                                                        00000303
00070                                                                   00000304
00071  01  ERROR-MESSAGES.                                              00000305
00072      12  ER-0000                 PIC X(4)  VALUE '0000'.          00000306
00073      12  ER-0004                 PIC X(4)  VALUE '0004'.          00000307
00074      12  ER-0008                 PIC X(4)  VALUE '0008'.          00000308
00075      12  ER-0029                 PIC X(4)  VALUE '0029'.          00000309
00076      12  ER-0070                 PIC X(4)  VALUE '0070'.          00000310
00077      12  ER-0194                 PIC X(4)  VALUE '0194'.          00000311
00078      12  ER-0195                 PIC X(4)  VALUE '0195'.          00000312
00079      12  ER-0196                 PIC X(4)  VALUE '0196'.          00000313
00080      12  ER-0197                 PIC X(4)  VALUE '0197'.          00000314
00081      12  ER-2208                 PIC X(4)  VALUE '2208'.          00000315
00082      12  ER-2209                 PIC X(4)  VALUE '2209'.          00000316
00083      12  ER-2210                 PIC X(4)  VALUE '2210'.          00000317
00084      12  ER-2212                 PIC X(4)  VALUE '2212'.          00000318
00085      12  ER-2217                 PIC X(4)  VALUE '2217'.          00000319
00086      12  ER-2218                 PIC X(4)  VALUE '2218'.          00000320
00087      12  ER-2200                 PIC X(4)  VALUE '2200'.          00000321
00088      12  ER-2220                 PIC X(4)  VALUE '2220'.          00000322
00089      12  ER-2222                 PIC X(4)  VALUE '2222'.          00000323
00090      12  ER-2223                 PIC X(4)  VALUE '2223'.          00000324
00091      12  ER-2226                 PIC X(4)  VALUE '2226'.          00000325
00092      12  ER-2227                 PIC X(4)  VALUE '2227'.          00000326
00093      12  ER-2228                 PIC X(4)  VALUE '2228'.          00000327
00094      12  ER-2240                 PIC X(4)  VALUE '2240'.          00000328
00095      12  ER-2241                 PIC X(4)  VALUE '2241'.          00000329
00096      12  ER-2247                 PIC X(4)  VALUE '2247'.          00000330
00097      12  ER-2423                 PIC X(4)  VALUE '2423'.          00000331
00098      12  ER-2424                 PIC X(4)  VALUE '2424'.          00000332
00099      12  ER-2425                 PIC X(4)  VALUE '2425'.          00000333
00100      12  ER-2426                 PIC X(4)  VALUE '2426'.          00000334
00101      12  ER-2427                 PIC X(4)  VALUE '2427'.          00000335
00102      12  ER-2428                 PIC X(4)  VALUE '2428'.          00000336
00103      12  ER-2431                 PIC X(4)  VALUE '2431'.          00000337
00104      12  ER-2433                 PIC X(4)  VALUE '2433'.          00000338
00105      12  ER-2437                 PIC X(4)  VALUE '2437'.          00000339
00106      12  ER-2429                 PIC X(4)  VALUE '2429'.          00000340
00107      12  ER-2442                 PIC X(4)  VALUE '2442'.          00000341
00108      12  ER-2471                 PIC X(4)  VALUE '2471'.          00000342
00109      12  ER-2526                 PIC X(4)  VALUE '2526'.          00000343
00110      12  ER-2529                 PIC X(4)  VALUE '2529'.          00000344
00111      12  ER-2531                 PIC X(4)  VALUE '2531'.          00000345
00112      12  ER-2532                 PIC X(4)  VALUE '2532'.          00000346
00113      12  ER-2541                 PIC X(4)  VALUE '2541'.          00000347
00114      12  ER-2542                 PIC X(4)  VALUE '2542'.          00000348
00115      12  ER-2589                 PIC X(4)  VALUE '2589'.          00000349
00116      12  ER-2591                 PIC X(4)  VALUE '2591'.          00000350
00117      12  ER-2592                 PIC X(4)  VALUE '2592'.          00000351
00118      12  ER-2593                 PIC X(4)  VALUE '2593'.          00000352
00119      12  ER-2594                 PIC X(4)  VALUE '2594'.          00000353
00120      12  ER-2629                 PIC X(4)  VALUE '2629'.          00000354
00121      12  ER-2630                 PIC X(4)  VALUE '2630'.          00000355
00122      12  ER-2635                 PIC X(4)  VALUE '2635'.          00000356
00123      12  ER-2636                 PIC X(4)  VALUE '2636'.          00000357
00124      12  ER-2651                 PIC X(4)  VALUE '2651'.          00000358
00125      12  ER-2901                 PIC X(4)  VALUE '2901'.          00000359
00126      12  ER-7400                 PIC X(4)  VALUE '7400'.          00000360
00127      12  ER-7403                 PIC X(4)  VALUE '7403'.          00000361
00128      12  ER-7404                 PIC X(4)  VALUE '7404'.          00000362
00129      12  ER-7405                 PIC X(4)  VALUE '7405'.          00000363
00130      12  ER-7423                 PIC X(4)  VALUE '7423'.          00000364
00131      12  ER-7424                 PIC X(4)  VALUE '7424'.          00000365
00132      12  ER-7530                 PIC X(4)  VALUE '7530'.          00000366
00133      12  ER-7632                 PIC X(4)  VALUE '7632'.          00000367
00134      12  ER-7573                 PIC X(4)  VALUE '7573'.          00000368
00135      12  ER-7630                 PIC X(4)  VALUE '7630'.          00000369
00136      12  ER-7631                 PIC X(4)  VALUE '7631'.          00000370
00137      12  ER-7633                 PIC X(4)  VALUE '7633'.          00000371
00138      12  ER-7997                 PIC X(4)  VALUE '7997'.          00000372
00139      12  ER-7998                 PIC X(4)  VALUE '7998'.          00000373
00140      12  ER-9999                 PIC X(4)  VALUE '9999'.          00000374
00141                                                                   00001410
00142      EJECT                                                        00001411
00143                                                                   00001430
00144                                                                   00001431
00145  01  ACCESS-KEYS.                                                 00001432
00146                                                                   00001433
00147      12  ERPNDT-KEY.                                              00001434
00148          16  ERPNDT-COMP-CD          PIC X     VALUE SPACE.       00001435
00149          16  ERPNDT-ENTRY-BATCH      PIC X(6)  VALUE SPACES.      00001436
00150          16  ERPNDT-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.     00001437
00151          16  ERPNDT-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.     00001438
00152                                                                   00001520
00153      12  ERPNDT-RECORD-LENGTH        PIC S9(4) COMP VALUE +585.   00001521
00154      12  ERPNDT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +608.   00001522
00155                                                                   00001523
00156      12  ELCNTL-KEY.                                              00001524
00157          16  ELCNTL-COMPANY-ID       PIC X(3)  VALUE SPACES.      00001525
00158          16  ELCNTL-REC-TYPE         PIC X     VALUE SPACES.      00001526
00159          16  ELCNTL-ACCESS.                                       00001527
00160              20  FILLER              PIC XX.                      00001528
00161              20  ELCNTL-HI-BEN       PIC XX.                      00001529
00162          16  ELCNTL-ST-ACCESS REDEFINES ELCNTL-ACCESS.            00001530
00163              20  ELCNTL-STATE        PIC XX.                      00001531
00164              20  FILLER              PIC X.                       00001532
00165              20  ELCNTL-CARRIER        PIC X.                     00001533
00166          16  ELCNTL-SEQ              PIC S9(4) VALUE +0 COMP.     00001534
00167                                                                   00001670
00168      12  ELCNTL-RECORD-LENGTH        PIC S9(4) COMP VALUE +504.   00001671
00169      12  ELCNTL-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +527.   00001672
00170                                                                   00001673
00171      12  ELCERT-KEY.                                              00001674
00172          16  ELCERT-COMPANY-CD       PIC X.                       00001675
00173          16  ELCERT-CARRIER          PIC X.                       00001676
00174          16  ELCERT-GROUPING         PIC X(6).                    00001677
00175          16  ELCERT-STATE            PIC XX.                      00001678
00176          16  ELCERT-ACCOUNT          PIC X(10).                   00001679
00177          16  ELCERT-CERT-EFF-DT      PIC XX.                      00001680
00178          16  ELCERT-CERT-NO.                                      00001681
00179              20  ELCERT-CERT-PRIME   PIC X(10).                   00001682
00180              20  ELCERT-CERT-SFX     PIC X.                       00001683
00181                                                                   00001810
00182      12  ELCERT-RECORD-LENGTH        PIC S9(4) COMP VALUE +450.   00001811
00183      12  ELCERT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +473.   00001812
00184                                                                   00001813
00185      12  ERPNDM-KEY.                                              00001814
00186          16  ERPNDM-COMP-CD          PIC X     VALUE SPACE.       00001815
00187          16  ERPNDM-ENTRY-BATCH      PIC X(6)  VALUE SPACES.      00001816
00188          16  ERPNDM-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.     00001817
00189          16  ERPNDM-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.     00001818
00190                                                                   00001900
00191      12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +250.   00001901
00192      12  ERPNDM-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +273.   00001902
00193                                                                   00001903
00194      12  ERACCT-KEY.                                              00001904
00195          16  ERACCT-COMP-KEY.                                     00001905
00196              20  ERACCT-CO       PIC X     VALUE SPACES.          00001906
00197              20  ERACCT-CARRIER  PIC X     VALUE SPACES.          00001907
00198              20  ERACCT-GROUPING PIC X(6)  VALUE SPACES.          00001908
00199              20  ERACCT-STATE    PIC XX    VALUE SPACES.          00001909
00200              20  ERACCT-ACCOUNT  PIC X(10) VALUE SPACES.          00001910
00201          16  ERACCT-EXP-DATE     PIC XX    VALUE SPACES.          00001911
00202          16  FILLER              PIC X(4)  VALUE LOW-VALUES.      00001912
00203      12  ERACCT-SAVE-KEY         PIC X(20) VALUE SPACES.          00001913
00204                                                                   00001914
00205      EJECT                                                        00001915
00206                                                                   00001916
00207  01  WORK-AREA.                                                   00001917
00208                                                                   00001918
00209      12  DEEDIT-FIELD            PIC X(15).                       00001919
00210      12  FILLER REDEFINES DEEDIT-FIELD.                           00001920
00211          16  FILLER              PIC X(4).                        00001921
00212          16  DEEDIT-FIELD-X11    PIC X(11).                       00001922
00213      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(15).       00001923
00214      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(13)V99.    00001924
00215      12  DEEDIT-FIELD-V3 REDEFINES DEEDIT-FIELD PIC S9(12)V9(3).  00001925
00216      12  DEEDIT-FIELD-V4 REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).  00001926
00217      12  DEEDIT-FIELD-V5 REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).  00001927
00218                                                                   00001928
00219      12  WS-SUB                  PIC S9(4) VALUE +0  COMP.        00001929
00220      12  WS-SUB1                 PIC S9(4) VALUE +0  COMP.        00001930
00221      12  WS-SUB2                 PIC S9(4) VALUE +0  COMP.        00001931
00222      12  WS-SUB3                 PIC S9(4) VALUE +0  COMP.        00001932
00223      12  WS-COV-SUB              PIC S9(4) VALUE +0  COMP.        00001933
00224      12  WS-EDIT-SUB             PIC S9(4) VALUE +0  COMP.        00001934
00225                                                                   00001935
00226      12  WS-CALC-TERM            PIC S999V9(5) VALUE ZEROS.       00001936
00227      12  WS-CALC-TERM-R REDEFINES WS-CALC-TERM.                   00001937
00228          16  WS-CALC-TERM-WHOLE  PIC S999.                        00001938
00229          16  WS-CALC-TERM-REMAIN PIC SV9(5).                      00001939
00230                                                                   00001940
00231      12  ERROR-SW                PIC X     VALUE SPACE.           00001941
00232          88  NO-ERROR                VALUE SPACE.                 00001942
00233          88  ERRORS                  VALUE 'Y'.                   00001943
00234                                                                   00001944
00235      12  WS-BROWSE-SW            PIC X     VALUE SPACE.           00001945
00236          88  BROWSE-NOT-STARTED      VALUE SPACE.                 00001946
00237          88  BROWSE-STARTED          VALUE 'Y'.                   00001947
00238      12  WS-FIRST-TIME-SW        PIC X     VALUE SPACE.           00001948
00239          88 FIRST-TIME               VALUE '0'.                   00001949
00240          88 NOT-FIRST-TIME           VALUE '1'.                   00001950
00241      12  WS-DATA-KEYED-SW        PIC X     VALUE SPACE.           00001951
00242          88  WS-DATA-NOT-KEYED       VALUE SPACE.                 00001952
00243          88  WS-DATA-KEYED           VALUE 'Y'.                   00001953
00244                                                                   00002440
00245      12  WS-EDITED-LF-CODE       PIC XX   VALUE ZEROS.            00002441
00246      12  WS-LF-ABBR-DESC         PIC XXX  VALUE SPACES.           00002442
00247                                                                   00002470
00248      12  WS-EDITED-AH-CODE       PIC XX   VALUE ZEROS.            00002471
00249      12  WS-AH-ABBR-DESC         PIC XXX  VALUE SPACES.           00002472
00250                                                                   00002473
00251      12  WS-BEN-CD               PIC XX   VALUE SPACES.           00002474
00252                                                                   00002520
00253      12  WS-ENTRY-CODE           PIC X     VALUE SPACE.           00002521
00254          88  WS-ENTRY-CODE-VALID   VALUE ' ' 'E' 'R' 'P'.         00002522
00255                                                                   00002523
00256      12  WS-ALL-NINES            PIC S9(7)V99 VALUE +9999999.99.  00002524
00257                                                                   00002570
00258      12  WS-MODE-CODE            PIC X     VALUE SPACE.           00002571
00259          88 WS-MODE-CODE-VALID     VALUE ' ' 'M' 'W' 'S' 'B'.     00002572
00260                                                                   00002573
00261      12  WS-KIND                 PIC XX    VALUE SPACE.           00002574
00262          88 WS-KIND-LF             VALUE 'LF'.                    00002575
00263          88 WS-KIND-AH             VALUE 'AH'.                    00002576
00264          88 WS-KIND-PR             VALUE 'PR'.                    00002577
00265          88 WS-KIND-UE             VALUE 'UE'.                    00002578
00266          88 WS-KIND-DI             VALUE 'DI'.                    00002579
00267          88 WS-KIND-MONTHLY        VALUE 'AH' 'UE'.               00002580
00268                                                                   00002581
00269      12  WS-JOURNAL-RECORD-LENGTH   PIC S9(4) COMP VALUE +0000.   00002582
00270                                                                   00002583
00271      12  WS-EDIT-CODE            PIC X(4)  VALUE SPACES.          00002584
00272                                                                   00002585
00273      12  WS-SAVE-INPUT-FIELDS.                                    00002586
00274                                                                   00002587
00275          16  WS-BAGE                 PIC 99       VALUE ZERO.     00002588
00276          16  WS-BJNT-AGE             PIC 99       VALUE ZERO.     00002589
00277          16  WS-BDAYS                PIC 999      VALUE ZERO.     00002590
00278          16  WS-BLN-TERM             PIC 999      VALUE ZERO.     00002591
00279          16  WS-BFREQ                PIC 99       VALUE ZERO.     00002592
00280          16  WS-BPHONE               PIC 9(12)    VALUE 0 COMP-3. 00002593
00281          16  WS-BAPR                 PIC S99V9(4) VALUE +0 COMP-3.00002594
00282          16  WS-BPMT                 PIC S9(6)V99 VALUE +0 COMP-3.00002595
00283          16  WS-BPMTS                PIC S999     VALUE +0 COMP-3.00002596
00284          16  WS-BLIVES               PIC 9(3)      COMP-3.        00002597
00285                                                                   00002598
00286          16  WS-B-COVERAGE OCCURS 2 TIMES.                        00002599
00287              20  WS-BTERM            PIC 999       COMP-3.        00002600
00288              20  WS-BCRIT-PERD       PIC 99        COMP-3.        00002601
00289              20  WS-BBEN             PIC S9(10)V99 COMP-3.        00002602
00290              20  WS-BALT-BEN     PIC S9(10)V99 COMP-3.            00002603
00291              20  WS-BPREM        PIC S9(10)V99 COMP-3.            00002604
00292              20  WS-BALT-PREM    PIC S9(07)V99 COMP-3.            00002605
00293                                                                   00002606
00294          16  WS-C-FIELDS   OCCURS 4 TIMES.                        00002607
00295              20  WS-CLIVES      PIC 9(3)          COMP-3.         00002608
00296              20  WS-CREFUND1    PIC S9(7)V99      COMP-3.         00002609
00297              20  WS-CREFUND2    PIC S9(7)V99      COMP-3.         00002610
00298                                                                   00002611
00299      12  WS-CONVERTED-BIRTH  PIC XX    VALUE SPACE.               00002612
00300      12  WS-CONVERTED-EFFDT      PIC XX    VALUE SPACES.          00002613
00301      12  WS-CONVERTED-1ST-PMT-DT PIC XX    VALUE SPACES.          00002614
00302      12  WS-CONVERTED-EXPIRDT      OCCURS 2 TIMES PIC XX.         00002615
00303      12  WS-CONVERTED-CANCEL-DATES OCCURS 4 TIMES.                00002616
00304          16  WS-CONVERTED-CANDT1 PIC XX.                          00002617
00305          16  WS-CONVERTED-CANDT2 PIC XX.                          00002618
00306                                                                   00002619
00307      12  WS-FIRST-NAME.                                           00002620
00308          16  WS-1ST-INIT         PIC X.                           00002621
00309          16  FILLER              PIC X(9).                        00002622
00310                                                                   00002623
00311      12  WS-INITIALS.                                             00002624
00312          16  WS-INITIAL-1        PIC X.                           00002625
00313          16  WS-INITIAL-2        PIC X.                           00002626
00314                                                                   00002627
00315      EJECT                                                        00002628
00316                                                                   00002629
00317  01  CLASIC-WARNING.                                              00002630
00318      12  WARNING-LENGTH              PIC S9(4)  VALUE +124 COMP.  00002631
00319      12  WARNING-TEXT.                                            00002632
00320          16  FILLER                  PIC X(80)  VALUE             00002633
00321              'THIS DATA MAY HAVE PREVIOUSLY BEEN PROCESSED'.      00002634
00322          16  FILLER                  PIC X(44)  VALUE             00002635
00323              'CONTACT LOGIC INC. FOR FURTHER INFORMATION'.        00002636
00324                                                                   00002637
00325      EJECT                                                        00002638
00326                                                                   00002639
00327                              COPY ELCDATE.                        00002640
00328                                                                   00003280
00329      EJECT                                                        00003290
00330                              COPY ELCLOGOF.                       00003291
00331                                                                   00003310
00332      EJECT                                                        00003320
00333                              COPY ELCATTR.                        00003321
00334                                                                   00003340
00335      EJECT                                                        00003350
00336                                    COPY ELCEMIB.                  00003351
00337                                                                   00003352
00338      EJECT                                                        00003380
00339                              COPY ELCINTF.                        00003381
00340      COPY ELC930PI.                                               00003382
00341      EJECT                                                        00003383
00342                              COPY ELCJPFX.                        00003384
00343                              PIC X(608).                          00003385
00344                                                                   00003386
00345      EJECT                                                        00003387
00346                              COPY ELCAID.                         00003388
00347  01  FILLER    REDEFINES DFHAID.                                  00003389
00348      12  FILLER              PIC X(8).                            00003390
00349      12  PF-VALUES           PIC X       OCCURS 2.                00003391
00350                                                                   00003392
00351      EJECT                                                        00003393
00352  01  DATA-ENTRY-MAP              PIC X(1116).                     00003394
00353                                        COPY EL9301S.              00003395
00354                                                                   00003396
00355      EJECT                                                        00003397
00356                                                                   00003398
00357  01  MAP-B REDEFINES EL930BI.                                     00003399
00358      12  FILLER                      PIC X(42).                   00003400
00359      12  DATA-AREA-B.                                             00003401
00360          16  BSEQ-LEN                PIC S9(4)  COMP.             00003402
00361          16  BSEQ-ATTRB              PIC X.                       00003403
00362          16  BSEQ                    PIC 9(4).                    00003404
00363          16  FILLER                  PIC X(8).                    00003405
00364          16  BCAR-LEN                PIC S9(4)  COMP.             00003406
00365          16  BCAR-ATTRB              PIC X.                       00003407
00366          16  BCAR                    PIC X.                       00003408
00367          16  FILLER                  PIC X(8).                    00003409
00368          16  BGRP-LEN                PIC S9(4)  COMP.             00003410
00369          16  BGRP-ATTRB              PIC X.                       00003411
00370          16  BGRP                    PIC X(6).                    00003412
00371          16  FILLER                  PIC X(7).                    00003413
00372          16  BST-LEN                 PIC S9(4)  COMP.             00003414
00373          16  BST-ATTRB               PIC X.                       00003415
00374          16  BST                     PIC XX.                      00003416
00375          16  FILLER                  PIC X(9).                    00003417
00376          16  BACCT-LEN               PIC S9(4)  COMP.             00003418
00377          16  BACCT-ATTRB             PIC X.                       00003419
00378          16  BACCT                   PIC X(10).                   00003420
00379          16  BCERT-LEN               PIC S9(4)  COMP.             00003421
00380          16  BCERT-ATTRB             PIC X.                       00003422
00381          16  BCERT                   PIC X(10).                   00003423
00382          16  BSFX-LEN                PIC S9(4)  COMP.             00003424
00383          16  BSFX-ATTRB              PIC X.                       00003425
00384          16  BSFX                    PIC X.                       00003426
00385          16  BEFFDT-LEN              PIC S9(4)  COMP.             00003427
00386          16  BEFFDT-ATTRB            PIC X.                       00003428
00387          16  BEFFDT                  PIC 9(6).                    00003429
00388          16  BLAST-NAME-LEN          PIC S9(4)  COMP.             00003430
00389          16  BLAST-NAME-ATTRB        PIC X.                       00003431
00390          16  BLAST-NAME              PIC X(15).                   00003432
00391          16  B1ST-NAME-LEN           PIC S9(4)  COMP.             00003433
00392          16  B1ST-NAME-ATTRB         PIC X.                       00003434
00393          16  B1ST-NAME               PIC X(10).                   00003435
00394          16  BINIT-LEN               PIC S9(4)  COMP.             00003436
00395          16  BINIT-ATTRB             PIC X.                       00003437
00396          16  BINIT                   PIC X.                       00003438
00397          16  BSEX-LEN                PIC S9(4)  COMP.             00003439
00398          16  BSEX-ATTRB              PIC X.                       00003440
00399          16  BSEX                    PIC X.                       00003441
00400          16  BAGE-LEN                PIC S9(4)  COMP.             00003442
00401          16  BAGE-ATTRB              PIC X.                       00003443
00402          16  BAGE                    PIC 99.                      00003444
00403          16  BSSNUM-LEN              PIC S9(4)  COMP.             00003445
00404          16  BSSNUM-ATTRB            PIC X.                       00003446
00405          16  BSSNUM                  PIC X(11).                   00003447
00406                                                                   00003448
00407          16  MAP-B-COVERAGE OCCURS 2 TIMES.                       00003449
00408                                                                   00003450
00409              20  BKIND-LEN           PIC S9(4)  COMP.             00003451
00410              20  BKIND-ATTRB         PIC X.                       00003452
00411              20  BKIND               PIC XX.                      00003453
00412              20  BTYPE-LEN           PIC S9(4)  COMP.             00003454
00413              20  BTYPE-ATTRB         PIC X.                       00003455
00414              20  BTYPE               PIC X(3).                    00003456
00415              20  BTERM-LEN           PIC S9(4)  COMP.             00003457
00416              20  BTERM-ATTRB         PIC X.                       00003458
00417              20  BTERMI              PIC 999.                     00003459
00418              20  BTERMO REDEFINES                                 00003460
00419                             BTERMI   PIC ZZZ.                     00003461
00420              20  BBEN-LEN            PIC S9(4)  COMP.             00003462
00421              20  BBEN-ATTRB          PIC X.                       00003463
00422              20  BBENI               PIC 9(12).                   00003464
00423              20  BBENO REDEFINES                                  00003465
00424                                BBENI PIC Z(9).99.                 00003466
00425              20  BPREM-LEN           PIC S9(4)  COMP.             00003467
00426              20  BPREM-ATTRB         PIC X.                       00003468
00427              20  BPREMI              PIC 9(11).                   00003469
00428              20  BPREMO REDEFINES                                 00003470
00429                               BPREMI PIC Z(7).99-.                00003471
00430              20  BEXPIRE-LEN         PIC S9(4)  COMP.             00003472
00431              20  BEXPIRE-ATTRB       PIC X.                       00003473
00432              20  BEXPIRE             PIC 9(6).                    00003474
00433              20  BCRIT-PERD-LEN      PIC S9(4)  COMP.             00003475
00434              20  BCRIT-PERD-ATTRB    PIC X.                       00003476
00435              20  BCRIT-PERDI         PIC 99.                      00003477
00436              20  BCRIT-PERDO REDEFINES                            00003478
00437                          BCRIT-PERDI PIC ZZ.                      00003479
00438              20  BALT-BEN-LEN        PIC S9(4)  COMP.             00003480
00439              20  BALT-BEN-ATTRB      PIC X.                       00003481
00440              20  BALT-BENI           PIC 9(12).                   00003482
00441              20  BALT-BENO REDEFINES                              00003483
00442                                BALT-BENI PIC Z(9).ZZ.             00003484
00443              20  BALT-PREM-LEN       PIC S9(4)  COMP.             00003485
00444              20  BALT-PREM-ATTRB     PIC X.                       00003486
00445              20  BALT-PREMI          PIC 9(9).                    00003487
00446              20  BALT-PREMO REDEFINES                             00003488
00447                                BALT-PREMI PIC Z(6).ZZ.            00003489
00448                                                                   00003490
00449          16  BLIVES-LEN              PIC S9(4)  COMP.             00003491
00450          16  BLIVES-ATTRB            PIC X.                       00003492
00451          16  BLIVESI                 PIC 9(3).                    00003493
00452          16  BLIVESO    REDEFINES                                 00003494
00453                           BLIVESI    PIC Z(3).                    00003495
00454          16  BJNT-1ST-NAME-LEN       PIC S9(4)   COMP.            00003496
00455          16  BJNT-1ST-NAME-ATTRB     PIC X.                       00003497
00456          16  BJNT-1ST-NAME           PIC X(10).                   00003498
00457          16  BJNT-INIT-LEN           PIC S9(4)   COMP.            00003499
00458          16  BJNT-INIT-ATTRB         PIC X.                       00003500
00459          16  BJNT-INIT               PIC X.                       00003501
00460          16  BJNT-LST-NAME-LEN       PIC S9(4)   COMP.            00003502
00461          16  BJNT-LST-NAME-ATTRB     PIC X.                       00003503
00462          16  BJNT-LST-NAME           PIC X(15).                   00003504
00463          16  BJNT-AGE-LEN            PIC S9(4)   COMP.            00003505
00464          16  BJNT-AGE-ATTRB          PIC X.                       00003506
00465          16  BJNT-AGE                PIC 99.                      00003507
00466          16  BBENEFICIARY-LEN        PIC S9(4)   COMP.            00003508
00467          16  BBENEFICIARY-ATTRB      PIC X.                       00003509
00468          16  BBENEFICIARY            PIC X(25).                   00003510
00469          16  B1ST-PMT-LEN            PIC S9(4)  COMP.             00003511
00470          16  B1ST-PMT-ATTRB          PIC X.                       00003512
00471          16  B1ST-PMT                PIC 9(6).                    00003513
00472          16  BDAYS-LEN               PIC S9(4)  COMP.             00003514
00473          16  BDAYS-ATTRB             PIC X.                       00003515
00474          16  BDAYSI                  PIC 9(3).                    00003516
00475          16  BDAYSO REDEFINES                                     00003517
00476                           BDAYSI     PIC ZZZ.                     00003518
00477          16  BLN-TERM-LEN            PIC S9(4)  COMP.             00003519
00478          16  BLN-TERM-ATTRB          PIC X.                       00003520
00479          16  BLN-TERMI               PIC 9(3).                    00003521
00480          16  BLN-TERMO REDEFINES                                  00003522
00481                           BLN-TERMI  PIC ZZZ.                     00003523
00482          16  BLN-OFFICER-LEN         PIC S9(4)  COMP.             00003524
00483          16  BLN-OFFICER-ATTRB       PIC X.                       00003525
00484          16  BLN-OFFICER             PIC XXX.                     00003526
00485          16  BMODE-LEN               PIC S9(4)  COMP.             00003527
00486          16  BMODE-ATTRB             PIC X.                       00003528
00487          16  BMODE                   PIC X.                       00003529
00488          16  BFREQ-LEN               PIC S9(4)  COMP.             00003530
00489          16  BFREQ-ATTRB             PIC X.                       00003531
00490          16  BFREQI                  PIC 99.                      00003532
00491          16  BFREQO REDEFINES BFREQI PIC ZZ.                      00003533
00492          16  BPMTS-LEN               PIC S9(4)  COMP.             00003534
00493          16  BPMTS-ATTRB             PIC X.                       00003535
00494          16  BPMTS-IN                PIC 999.                     00003536
00495          16  BPMTS-OUT REDEFINES BPMTS-IN                         00003537
00496                                      PIC ZZZ.                     00003538
00497          16  BPMT-LEN                PIC S9(4)  COMP.             00003539
00498          16  BPMT-ATTRB              PIC X.                       00003540
00499          16  BPMTI                   PIC 9(9).                    00003541
00500          16  BPMTO REDEFINES BPMTI   PIC Z(6).ZZ.                 00003542
00501          16  BCAPTN-LEN              PIC S9(4)  COMP.             00003543
00502          16  BCAPTN-ATTRB            PIC X.                       00003544
00503          16  BCAPTN                  PIC X(10).                   00003545
00504          16  BRINCD-LEN              PIC S9(4)  COMP.             00003546
00505          16  BRINCD-ATTRB            PIC X.                       00003547
00506          16  BRINCD                  PIC X.                       00003548
00507          16  BENTRY-LEN              PIC S9(4)  COMP.             00003549
00508          16  BENTRY-ATTRB            PIC X.                       00003550
00509          16  BENTRY                  PIC X.                       00003551
00510          16  BSKPCD-LEN              PIC S9(4)  COMP.             00003552
00511          16  BSKPCD-ATTRB            PIC X.                       00003553
00512          16  BSKPCD                  PIC X.                       00003554
00513          16  BIND-GRP-LEN            PIC S9(4)  COMP.             00003555
00514          16  BIND-GRP-ATTRB          PIC X.                       00003556
00515          16  BIND-GRP                PIC X.                       00003557
00516          16  BSIG-LEN                PIC S9(4)  COMP.             00003558
00517          16  BSIG-ATTRB              PIC X.                       00003559
00518          16  BSIG                    PIC X.                       00003560
00519          16  BPOLICY-LEN             PIC S9(4)  COMP.             00003561
00520          16  BPOLICY-ATTRB           PIC X.                       00003562
00521          16  BPOLICY                 PIC X(12).                   00003563
00522          16  BRTCLS-LEN              PIC S9(4)  COMP.             00003564
00523          16  BRTCLS-ATTRB            PIC X.                       00003565
00524          16  BRTCLS                  PIC XX.                      00003566
00525          16  BAPR-LEN                PIC S9(4)  COMP.             00003567
00526          16  BAPR-ATTRB              PIC X.                       00003568
00527          16  BAPR-IN                 PIC 9(7).                    00003569
00528          16  BAPR-OUT REDEFINES BAPR-IN                           00003570
00529                                      PIC 99.9999.                 00003571
00530          16  BBIRTH-LEN              PIC S9(4)  COMP.             00003572
00531          16  BBIRTH-ATTRB            PIC X.                       00003573
00532          16  BBIRTH-DT               PIC 9(6).                    00003574
00533          16  BMEM-NO-LEN             PIC S9(4)   COMP.            00003575
00534          16  BMEM-NO-ATTRB           PIC X.                       00003576
00535          16  BMEM-NO                 PIC X(12).                   00003577
00536          16  BADDRS1-LEN             PIC S9(4)  COMP.             00003578
00537          16  BADDRS1-ATTRB           PIC X.                       00003579
00538          16  BADDRS1                 PIC X(30).                   00003580
00539          16  BADDRS2-LEN             PIC S9(4)  COMP.             00003581
00540          16  BADDRS2-ATTRB           PIC X.                       00003582
00541          16  BADDRS2                 PIC X(30).                   00003583
00542          16  BCITYST-LEN             PIC S9(4)  COMP.             00003584
00543          16  BCITYST-ATTRB           PIC X.                       00003585
00544          16  BCITYST                 PIC X(30).                   00003586
00545          16  BZIP5-LEN               PIC S9(4)  COMP.             00003587
00546          16  BZIP5-ATTRB             PIC X.                       00003588
00547          16  BZIP5                   PIC X(5).                    00003589
00548          16  BDASH-LEN               PIC S9(4)  COMP.             00003590
00549          16  BDASH-ATTRB             PIC X.                       00003591
00550          16  BDASH                   PIC X.                       00003592
00551          16  BZIP4-LEN               PIC S9(4)  COMP.             00003593
00552          16  BZIP4-ATTRB             PIC X.                       00003594
00553          16  BZIP4                   PIC X(4).                    00003595
00554          16  BPHONE-LEN              PIC S9(4)  COMP.             00003596
00555          16  BPHONE-ATTRB            PIC X.                       00003597
00556          16  BPHONE                  PIC 9(12).                   00003598
00557          16  BPHONE-NO REDEFINES                                  00003599
00558                                BPHONE PIC 999B999B9999.           00003600
00559                                                                   00003601
00560                                                                   00003602
00561  01  MAP-C REDEFINES EL930BI.                                     00003603
00562      12  FILLER                  PIC X(42).                       00003604
00563      12  DATA-AREA-C             OCCURS 2 TIMES.                  00003605
00564          16  CSEQ-LEN                PIC S9(4)  COMP.             00003606
00565          16  CSEQ-ATTRB              PIC X.                       00003607
00566          16  CSEQ                    PIC 9(4).                    00003608
00567          16  FILLER                  PIC X(8).                    00003609
00568          16  CCAR-LEN                PIC S9(4)  COMP.             00003610
00569          16  CCAR-ATTRB              PIC X.                       00003611
00570          16  CCAR                    PIC X.                       00003612
00571          16  FILLER                  PIC X(8).                    00003613
00572          16  CGRP-LEN                PIC S9(4)  COMP.             00003614
00573          16  CGRP-ATTRB              PIC X.                       00003615
00574          16  CGRP                    PIC X(6).                    00003616
00575          16  FILLER                  PIC X(7).                    00003617
00576          16  CST-LEN                 PIC S9(4)  COMP.             00003618
00577          16  CST-ATTRB               PIC X.                       00003619
00578          16  CST                     PIC XX.                      00003620
00579          16  FILLER                  PIC X(9).                    00003621
00580          16  CACCT-LEN               PIC S9(4)  COMP.             00003622
00581          16  CACCT-ATTRB             PIC X.                       00003623
00582          16  CACCT                   PIC X(10).                   00003624
00583          16  CCERT-LEN               PIC S9(4)  COMP.             00003625
00584          16  CCERT-ATTRB             PIC X.                       00003626
00585          16  CCERT                   PIC X(10).                   00003627
00586          16  CSFX-LEN                PIC S9(4)  COMP.             00003628
00587          16  CSFX-ATTRB              PIC X.                       00003629
00588          16  CSFX                    PIC X.                       00003630
00589          16  CEFFDT-LEN              PIC S9(4)  COMP.             00003631
00590          16  CEFFDT-ATTRB            PIC X.                       00003632
00591          16  CEFFDT                  PIC 9(6).                    00003633
00592          16  CLAST-NAME-LEN          PIC S9(4)  COMP.             00003634
00593          16  CLAST-NAME-ATTRB        PIC X.                       00003635
00594          16  CLAST-NAME              PIC X(15).                   00003636
00595          16  CANCEL-INFO.                                         00003637
00596              20  CKIND1-LEN          PIC S9(4)  COMP.             00003638
00597              20  CKIND1-ATTRB        PIC X.                       00003639
00598              20  CKIND1              PIC XX.                      00003640
00599              20  CCANDT1-LEN         PIC S9(4)  COMP.             00003641
00600              20  CCANDT1-ATTRB       PIC X.                       00003642
00601              20  CCANDT1             PIC 9(6).                    00003643
00602              20  CREFUND1-LEN        PIC S9(4)  COMP.             00003644
00603              20  CREFUND1-ATTRB      PIC X.                       00003645
00604              20  CREFUND1I           PIC X(11).                   00003646
00605              20  CREFUND1O REDEFINES                              00003647
00606                            CREFUND1I PIC Z(7).99-.                00003648
00607              20  CKIND2-LEN          PIC S9(4)  COMP.             00003649
00608              20  CKIND2-ATTRB        PIC X.                       00003650
00609              20  CKIND2              PIC XX.                      00003651
00610              20  CCANDT2-LEN         PIC S9(4)  COMP.             00003652
00611              20  CCANDT2-ATTRB       PIC X.                       00003653
00612              20  CCANDT2             PIC 9(6).                    00003654
00613              20  CREFUND2-LEN        PIC S9(4)  COMP.             00003655
00614              20  CREFUND2-ATTRB      PIC X.                       00003656
00615              20  CREFUND2I           PIC X(11).                   00003657
00616              20  CREFUND2O REDEFINES                              00003658
00617                            CREFUND2I PIC Z(7).99-.                00003659
00618              20  CCHK-LEN            PIC S9(4)  COMP.             00003660
00619              20  CCHK-ATTRB          PIC X.                       00003661
00620              20  CCHK                PIC X.                       00003662
00621              20  CPAYEE-LEN          PIC S9(4)  COMP.             00003663
00622              20  CPAYEE-ATTRB        PIC X.                       00003664
00623              20  CPAYEE              PIC X(6).                    00003665
00624              20  CLIVES-LEN          PIC S9(4)  COMP.             00003666
00625              20  CLIVES-ATTRB        PIC X.                       00003667
00626              20  CLIVESI             PIC 999.                     00003668
00627              20  CLIVESO REDEFINES                                00003669
00628                           CLIVESI    PIC ZZZ.                     00003670
00629                                                                   00003671
00630      EJECT                                                        00003672
00631  LINKAGE SECTION.                                                 00003673
00632  01  DFHCOMMAREA             PIC X(1024).                         00003674
00633                                                                   00003675
00634      EJECT                                                        00003676
00635  01  PARMLIST.                                                    00003677
00636      02  FILLER              PIC S9(8)   COMP.                    00003678
00637      02  ERPNDT-POINTER      PIC S9(8)   COMP.                    00003679
00638      02  ELCNTL-POINTER      PIC S9(8)   COMP.                    00003680
00639      02  ELCERT-POINTER      PIC S9(8)   COMP.                    00003681
00640      02  ERPNDM-POINTER      PIC S9(8)   COMP.                    00003682
00641      02  ERACCT-POINTER      PIC S9(8)   COMP.                    00003683
00642                                                                   00003684
00643      EJECT                                                        00006430
00644                                                                   00006440
00645                                  COPY ERCPNDB.                    00006441
00646      EJECT                                                        00006460
00647                                                                   00006470
00648                                  COPY ELCCNTL.                    00006471
00649      EJECT                                                        00006490
00650                                                                   00006500
00651                                  COPY ELCCERT.                    00006501
00652      EJECT                                                        00006520
00653                                                                   00006530
00654                                  COPY ERCPNDM.                    00006531
00655      EJECT                                                        00006550
00656                                                                   00006560
00657                                  COPY ERCACCT.                    00006561
00658      EJECT                                                        00006562
00659                                                                   00006590
00660                                                                   00006591
00661  PROCEDURE DIVISION.                                              00006592
00662                                                                   00006593
00663      SERVICE RELOAD PARMLIST.                                     00006594
00664                                                                   00006640
00665      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      00006641
00666      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            00006642
00667                                                                   00006643
00668      MOVE +2                     TO EMI-NUMBER-OF-LINES.          00006644
00669                                                                   00006690
00670      IF EIBCALEN = 0                                              00006691
00671          GO TO 8800-UNAUTHORIZED-ACCESS.                          00006692
00672                                                                   00006693
00673      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              00006694
00674      MOVE '5'                    TO DC-OPTION-CODE.               00006695
00675      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    00006696
00676      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            00006697
00677      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                00006698
00678                                                                   00006699
00679      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         00006700
00680          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   00006701
00681              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      00006702
00682              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      00006703
00683              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      00006704
00684              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      00006705
00685              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      00006706
00686              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      00006707
00687              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    00006708
00688              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      00006709
00689          ELSE                                                     00006710
00690              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      00006711
00691              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    00006712
00692              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      00006713
00693              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      00006714
00694              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      00006715
00695              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      00006716
00696              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      00006717
00697              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     00006718
00698                                                                   00006719
00699      MOVE LOW-VALUES             TO EL930BI.                      00006720
00700                                                                   00006721
00701      IF EIBTRNID NOT = TRANS-EXI2                                 00006722
00702          MOVE ZEROS              TO PI-LF-ISS-ENTERED             00006723
00703                                     PI-LF-CAN-ENTERED             00006724
00704                                     PI-AH-ISS-ENTERED             00006725
00705                                     PI-AH-CAN-ENTERED             00006726
00706                                     PI-ISS-CNT-ENTERED            00006727
00707                                     PI-CAN-CNT-ENTERED            00006728
00708          IF PI-MAINT-FUNC = 'N'                                   00006729
00709             MOVE +0              TO PI-LAST-SEQ-NO-ADDED          00006730
00710             MOVE +1              TO PI-NEXT-DISPLAY-SEQ-NO        00006731
00711             IF PI-MAP-NAME = EL930B                               00006732
00712                PERFORM 8550-SET-MAP-SEQ-NOS                       00007120
00713                GO TO 8100-SEND-INITIAL-MAP                        00007121
00714             ELSE                                                  00007122
00715                PERFORM 8550-SET-MAP-SEQ-NOS                       00007123
00716                        VARYING WS-SUB2 FROM 1 BY 1                00007124
00717                        UNTIL WS-SUB2   GREATER THAN +2            00007125
00718                  GO TO 8100-SEND-INITIAL-MAP                      00007126
00719          ELSE                                                     00007127
00720              GO TO 3000-CONTINUE-ENTRY.                           00007128
00721                                                                   00007129
00722      EXEC CICS HANDLE CONDITION                                   00007130
00723          PGMIDERR  (9600-PGMID-ERROR)                             00007131
00724          ERROR     (9990-ABEND)                                   00007132
00725          END-EXEC.                                                00007133
00726                                                                   00007260
00727      IF EIBAID = DFHCLEAR                                         00007261
00728          GO TO 9400-CLEAR.                                        00007262
00729                                                                   00007263
00730      EJECT                                                        00007264
00731                                                                   00007265
00732 ******************************************************************00007266
00733 *                                                                *00007267
00734 *                R E C E I V E   M A P S                         *00007268
00735 *                                                                *00007269
00736 ******************************************************************00007270
00737                                                                   00007271
00738  0200-RECEIVE.                                                    00007272
00739                                                                   00007273
00740      IF EIBAID = DFHPA1 OR                                        00007274
00741                  DFHPA2 OR                                        00007275
00742                  DFHPA3                                           00007276
00743          MOVE ER-0008            TO EMI-ERROR                     00007277
00744          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00007278
00745          IF PI-MAP-NAME = EL930B                                  00007279
00746              MOVE -1             TO BPFENTRL                      00007280
00747              GO TO 8200-SEND-DATAONLY                             00007281
00748          ELSE                                                     00007282
00749              MOVE -1             TO CPFENTRL                      00007283
00750              GO TO 8200-SEND-DATAONLY.                            00007284
00751                                                                   00007285
00752      EXEC CICS RECEIVE                                            00007286
00753          MAP      (PI-MAP-NAME)                                   00007287
00754          MAPSET   (MAPSET-EL9301S)                                00007288
00755          INTO     (DATA-ENTRY-MAP)                                00007289
00756          END-EXEC.                                                00007290
00757                                                                   00007291
uktdel*    TRANSFORM DATA-ENTRY-MAP FROM '_' TO ' '.                    00007292
uktins     INSPECT DATA-ENTRY-MAP REPLACING ALL '_' BY ' '.
00759                                                                   00007293
00760      IF PI-MAP-NAME = EL930B                                      00007294
00761          IF BPFENTRL GREATER THAN ZERO                            00007295
00762              IF EIBAID NOT = DFHENTER                             00007296
00763                  MOVE ER-0004    TO EMI-ERROR                     00007297
00764                  MOVE AL-UNBOF   TO BPFENTRA                      00007298
00765                  MOVE -1         TO BPFENTRL                      00007299
00766                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         00007300
00767                  GO TO 8200-SEND-DATAONLY                         00007301
00768              ELSE                                                 00007302
00769                  IF BPFENTRI NUMERIC                              00007303
00770                    AND BPFENTRI GREATER THAN 0                    00007304
00771                    AND BPFENTRI LESS THAN 23                      00007305
00772                      MOVE PF-VALUES (BPFENTRI) TO EIBAID          00007306
00773                  ELSE                                             00007307
00774                      MOVE ER-0029  TO EMI-ERROR                   00007308
00775                      MOVE AL-UNBOF TO BPFENTRA                    00007309
00776                      MOVE -1       TO BPFENTRL                    00007310
00777                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     00007311
00778                      GO TO 8200-SEND-DATAONLY                     00007312
00779          ELSE                                                     00007313
00780              NEXT SENTENCE                                        00007314
00781                                                                   00007315
00782      ELSE                                                         00007316
00783                                                                   00007317
00784      IF PI-MAP-NAME = EL930C                                      00007318
00785          IF CPFENTRL GREATER THAN ZERO                            00007319
00786              IF EIBAID NOT = DFHENTER                             00007320
00787                  MOVE ER-0004    TO EMI-ERROR                     00007321
00788                  MOVE AL-UNBOF   TO CPFENTRA                      00007322
00789                  MOVE -1         TO CPFENTRL                      00007323
00790                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         00007324
00791                  GO TO 8200-SEND-DATAONLY                         00007325
00792              ELSE                                                 00007326
00793                  IF CPFENTRI NUMERIC                              00007327
00794                    AND CPFENTRI GREATER THAN 0                    00007328
00795                    AND CPFENTRI LESS THAN 23                      00007329
00796                      MOVE PF-VALUES (CPFENTRI) TO EIBAID          00007330
00797                  ELSE                                             00007331
00798                      MOVE ER-0029  TO EMI-ERROR                   00007332
00799                      MOVE AL-UNBOF TO BPFENTRA                    00007333
00800                      MOVE -1       TO BPFENTRL                    00007334
00801                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     00007335
00802                      GO TO 8200-SEND-DATAONLY.                    00007336
00803                                                                   00007337
00804                                                                   00007338
00805      EJECT                                                        00007339
00806 ******************************************************************00007340
00807 *                                                                *00007341
00808 *                  C H E C K   PF   K E Y S                      *00007342
00809 *                                                                *00007343
00810 ******************************************************************00008100
00811                                                                   00008101
00812                                                                   00008102
00813 ******************************************************************00008103
00814 *                                                                *00008104
00815 *   PF KEY FUNCTIONS:                                            *00008105
00816 *                                                                *00008106
00817 *   PF1 = BROWSE FOWARD                                          *00008107
00818 *   PF2 = BROWSE BACKWARD                                        *00008108
00819 *   PF3 = ADD ISSUE RECORD                                       *00008109
00820 *   PF4 = ADD CANCEL RECORD                                      *00008110
00821 *   PF5 = RESET TABS (OPEN PRTECTED FIELDS)                      *00008111
00822 *   PF6 = DELETE ENTRY                                           *00008112
00823 *                                                                *00008113
00824 ******************************************************************00008114
00825                                                                   00008115
00826  0300-CHECK-PFKEYS.                                               00008116
00827                                                                   00008270
00828      IF EIBAID = DFHPF12                                          00008271
00829          GO TO 9500-PF12.                                         00008272
00830                                                                   00008300
00831      IF EIBAID = DFHENTER                                         00008301
00832          GO TO 1000-EDIT-MAPB.                                    00008302
00833                                                                   00008330
00834      IF EIBAID = DFHPF1                                           00008331
00835          GO TO 2000-BROWSE-FWD.                                   00008332
00836                                                                   00008360
00837      IF EIBAID = DFHPF2                                           00008361
00838          GO TO 2100-BROWSE-BKWD.                                  00008362
00839                                                                   00008363
00840      IF EIBAID = DFHPF3                                           00008364
00841          MOVE SPACE              TO PI-DISPLAY-SW                 00008365
00842          MOVE LOW-VALUES         TO DATA-ENTRY-MAP                00008366
00843          ADD +1, PI-LAST-SEQ-NO-ADDED                             00008367
00844                GIVING PI-NEXT-DISPLAY-SEQ-NO                      00008368
00845          MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ          00008369
00846          MOVE EL930B             TO PI-MAP-NAME                   00008370
00847          PERFORM 8550-SET-MAP-SEQ-NOS                             00008371
00848          GO TO 8100-SEND-INITIAL-MAP.                             00008372
00849                                                                   00008373
00850      IF EIBAID = DFHPF4                                           00008374
00851          MOVE SPACE              TO PI-DISPLAY-SW                 00008375
00852          MOVE LOW-VALUES         TO MAP-C                         00008376
00853          ADD +1, PI-LAST-SEQ-NO-ADDED                             00008377
00854                GIVING PI-NEXT-DISPLAY-SEQ-NO                      00008378
00855          MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ          00008379
00856          MOVE EL930C             TO PI-MAP-NAME                   00008380
00857          PERFORM 8550-SET-MAP-SEQ-NOS                             00008381
00858                 VARYING WS-SUB2 FROM 1 BY 1                       00008382
00859                 UNTIL WS-SUB2 GREATER THAN +2                     00008383
00860          GO TO 8100-SEND-INITIAL-MAP.                             00008384
00861                                                                   00008385
00862      IF EIBAID = DFHPF5                                           00008386
00863         IF PI-MAP-NAME = EL930B                                   00008387
00864            PERFORM 0610-UNPROTECT-FIELDS THRU 0610-EXIT           00008388
00865            ADD +1, PI-LAST-SEQ-NO-ADDED                           00008389
00866                   GIVING PI-NEXT-DISPLAY-SEQ-NO                   00008390
00867            PERFORM 8550-SET-MAP-SEQ-NOS                           00008391
00868            MOVE -1             TO BCERTL                          00008392
00869            GO TO 8200-SEND-DATAONLY                               00008393
00870         ELSE                                                      00008394
00871            PERFORM 0710-UNPROTECT-FIELDS THRU 0710-EXIT           00008395
00872            ADD +1, PI-LAST-SEQ-NO-ADDED                           00008396
00873                   GIVING PI-NEXT-DISPLAY-SEQ-NO                   00008397
00874            PERFORM 8550-SET-MAP-SEQ-NOS                           00008398
00875                    VARYING WS-SUB2 FROM +1 BY +1                  00008399
00876                    UNTIL WS-SUB2 GREATER THAN +2                  00008400
00877           MOVE -1             TO CCERT-LEN  (1)                   00008401
00878           GO TO 8200-SEND-DATAONLY.                               00008402
00879                                                                   00008403
00880      IF EIBAID = DFHPF6                                           00008404
00881          IF PI-LAST-FUNC-DISPLAY                                  00008405
00882              GO TO 6000-DELETE-PEND-BUS-RECORD                    00008406
00883          ELSE                                                     00008407
00884              MOVE ER-2594        TO EMI-ERROR                     00008408
00885              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             00008409
00886              IF PI-MAP-NAME = EL930B                              00008410
00887                  MOVE -1         TO BPFENTRL                      00008411
00888                  GO TO 8200-SEND-DATAONLY                         00008412
00889              ELSE                                                 00008413
00890                  MOVE -1         TO CPFENTRL                      00008414
00891                  GO TO 8200-SEND-DATAONLY.                        00008415
00892                                                                   00008920
00893      MOVE ER-0008 TO EMI-ERROR.                                   00008921
00894      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00008922
00895                                                                   00008923
00896      IF PI-MAP-NAME = EL930B                                      00008924
00897          MOVE -1                 TO BPFENTRL                      00008925
00898      ELSE                                                         00008926
00899          MOVE -1                 TO CPFENTRL.                     00008927
00900                                                                   00008928
00901      GO TO 8200-SEND-DATAONLY.                                    00008929
00902                                                                   00009020
00903      EJECT                                                        00009021
00904                                                                   00009022
00905                                                                   00009023
00906 ******************************************************************00009024
00907 *                                                                *00009025
00908 *         P R O T E C T   I S S U E   F I E L D S                *00009026
00909 *                                                                *00009027
00910 ******************************************************************00009028
00911                                                                   00009029
00912  0600-PROTECT-FIELDS.                                             00009030
00913                                                                   00009130
00914      IF NOT PI-ISS-SUFFIX-KEYED                                   00009131
00915          MOVE AL-SANOF           TO BSFX-ATTRB.                   00009132
00916                                                                   00009160
00917      IF NOT PI-IG-KEYED                                           00009161
00918          MOVE AL-SANOF           TO BIND-GRP-ATTRB.               00009162
00919                                                                   00009190
00920      IF NOT PI-1ST-PMT-KEYED                                      00009191
00921          MOVE AL-SANOF           TO B1ST-PMT-ATTRB.               00009192
00922                                                                   00009220
00923      IF NOT PI-DAYS-KEYED                                         00009221
00924          MOVE AL-SANOF           TO BDAYS-ATTRB.                  00009222
00925                                                                   00009250
00926      IF NOT PI-APR-KEYED                                          00009251
00927          MOVE AL-SANOF           TO BAPR-ATTRB.                   00009252
00928                                                                   00009280
00929      IF NOT PI-FREQ-KEYED                                         00009281
00930          MOVE AL-SANOF           TO BFREQ-ATTRB.                  00009282
00931                                                                   00009310
00932      IF NOT PI-SIG-KEYED                                          00009311
00933          MOVE AL-SANOF           TO BSIG-ATTRB.                   00009312
00934                                                                   00009340
00935      IF NOT PI-ISS-LIVES-KEYED                                    00009341
00936          MOVE AL-SANOF           TO BLIVES-ATTRB.                 00009342
00937                                                                   00009370
00938      IF NOT PI-SSNUM-KEYED                                        00009371
00939          MOVE AL-SANOF           TO BSSNUM-ATTRB.                 00009372
00940                                                                   00009400
00941      IF NOT PI-MEMBER-KEYED                                       00009401
00942          MOVE AL-SANOF           TO BMEM-NO-ATTRB.                00009402
00943                                                                   00009430
00944      IF NOT PI-MODE-KEYED                                         00009431
00945          MOVE AL-SANOF           TO BMODE-ATTRB.                  00009432
00946                                                                   00009460
00947      IF NOT PI-PMTS-KEYED                                         00009461
00948          MOVE AL-SANOF           TO BPMTS-ATTRB.                  00009462
00949                                                                   00009490
00950      IF NOT PI-LN-OFFICER-KEYED                                   00009491
00951          MOVE AL-SANOF           TO BLN-OFFICER-ATTRB.            00009492
00952                                                                   00009520
00953      IF NOT PI-LNTRM-KEYED                                        00009521
00954          MOVE AL-SANOF           TO BLN-TERM-ATTRB.               00009522
00955                                                                   00009550
00956      IF NOT PI-ENTRY-KEYED                                        00009551
00957          MOVE AL-SANOF           TO BENTRY-ATTRB.                 00009552
00958                                                                   00009580
00959 *    IF NOT PI-ZIP4-KEYED                                         00009581
00960 *        MOVE AL-SANOF           TO BZIP4-ATTRB.                  00009582
00961                                                                   00009610
00962      IF NOT PI-POLICY-KEYED                                       00009611
00963          MOVE AL-SANOF           TO BPOLICY-ATTRB.                00009612
00964                                                                   00009640
00965      IF NOT PI-RINCD-KEYED                                        00009641
00966          MOVE AL-SANOF           TO BRINCD-ATTRB.                 00009642
00967                                                                   00009670
00968      IF NOT PI-RTCLS-KEYED                                        00009671
00969          MOVE AL-SANOF           TO BRTCLS-ATTRB.                 00009672
00970                                                                   00009673
00971                                                                   00009674
00972      IF NOT PI-EXPIRE-KEYED                                       00009675
00973          MOVE AL-SANOF           TO BEXPIRE-ATTRB (1)             00009676
00974                                     BEXPIRE-ATTRB (2).            00009677
00975                                                                   00009678
00976      IF NOT PI-CRIT-PERD-KEYED                                    00009679
00977          MOVE AL-SANOF           TO BCRIT-PERD-ATTRB (1)          00009680
00978                                     BCRIT-PERD-ATTRB (2).         00009681
00979                                                                   00009790
00980      IF NOT PI-PMT-KEYED                                          00009791
00981          MOVE AL-SANOF           TO BPMT-ATTRB.                   00009792
00982                                                                   00009820
00983      IF NOT PI-SKPCD-KEYED                                        00009821
00984          MOVE AL-SANOF           TO BSKPCD-ATTRB.                 00009822
00985                                                                   00009823
00986      IF PI-BIRTH-DATE-IS-INPUT                                    00009824
00987          MOVE AL-SANOF           TO BAGE-ATTRB                    00009825
00988      ELSE                                                         00009826
00989          MOVE AL-SANOF           TO BBIRTH-ATTRB.                 00009827
00990                                                                   00009900
00991      IF NOT PI-JNT-AGE-KEYED                                      00009901
00992          MOVE AL-SANOF           TO BJNT-AGE-ATTRB.               00009902
00993                                                                   00009903
00994      IF NOT PI-JNT-NAME-KEYED                                     00009904
00995          MOVE AL-SANOF           TO BJNT-INIT-ATTRB               00009905
00996                                     BJNT-LST-NAME-ATTRB           00009906
00997                                     BJNT-1ST-NAME-ATTRB.          00009907
00998                                                                   00009980
00999      IF NOT PI-BENEFICIARY-KEYED                                  00009981
01000          MOVE AL-SANOF           TO BBENEFICIARY-ATTRB.           00009982
01001                                                                   00010010
01002      IF NOT PI-PHONE-KEYED                                        00010011
01003          MOVE AL-SANOF           TO BPHONE-ATTRB.                 00010012
01004                                                                   00010040
01005      IF NOT PI-ALT-BEN-KEYED                                      00010041
01006          MOVE AL-SANOF           TO BALT-BEN-ATTRB (1).           00010042
01007                                                                   00010070
01008      IF NOT PI-ALT-PREM-KEYED                                     00010071
01009          MOVE AL-SANOF           TO BALT-PREM-ATTRB (1).          00010072
01010                                                                   00010100
01011  0600-EXIT.                                                       00010101
01012      EXIT.                                                        00010102
01013                                                                   00010103
01014      EJECT                                                        00010104
01015                                                                   00010105
01016 ******************************************************************00010106
01017 *                                                                *00010107
01018 *         U N P R O T E C T   I S S U E   F I E L D S            *00010108
01019 *                                                                *00010109
01020 ******************************************************************00010110
01021                                                                   00010111
01022  0610-UNPROTECT-FIELDS.                                           00010112
01023                                                                   00010230
01024      IF NOT PI-ISS-SUFFIX-KEYED                                   00010231
01025          MOVE AL-UANOF           TO BSFX-ATTRB.                   00010232
01026                                                                   00010260
01027      IF NOT PI-IG-KEYED                                           00010261
01028          MOVE AL-UANOF           TO BIND-GRP-ATTRB.               00010262
01029                                                                   00010290
01030      IF NOT PI-1ST-PMT-KEYED                                      00010291
01031          MOVE AL-UANOF           TO B1ST-PMT-ATTRB.               00010292
01032                                                                   00010320
01033      IF NOT PI-DAYS-KEYED                                         00010321
01034          MOVE AL-UANOF           TO BDAYS-ATTRB.                  00010322
01035                                                                   00010350
01036      IF NOT PI-APR-KEYED                                          00010351
01037          MOVE AL-UANOF           TO BAPR-ATTRB.                   00010352
01038                                                                   00010380
01039      IF NOT PI-FREQ-KEYED                                         00010381
01040          MOVE AL-UANOF           TO BFREQ-ATTRB.                  00010382
01041                                                                   00010410
01042      IF NOT PI-SIG-KEYED                                          00010411
01043          MOVE AL-UANOF           TO BSIG-ATTRB.                   00010412
01044                                                                   00010440
01045      IF NOT PI-ISS-LIVES-KEYED                                    00010441
01046          MOVE AL-UANOF           TO BLIVES-ATTRB.                 00010442
01047                                                                   00010470
01048      IF NOT PI-SSNUM-KEYED                                        00010471
01049          MOVE AL-UANOF           TO BSSNUM-ATTRB.                 00010472
01050                                                                   00010500
01051      IF NOT PI-MEMBER-KEYED                                       00010501
01052          MOVE AL-UANOF           TO BMEM-NO-ATTRB.                00010502
01053                                                                   00010530
01054      IF NOT PI-MODE-KEYED                                         00010531
01055          MOVE AL-UANOF           TO BMODE-ATTRB.                  00010532
01056                                                                   00010560
01057      IF NOT PI-PMTS-KEYED                                         00010561
01058          MOVE AL-UANOF           TO BPMTS-ATTRB.                  00010562
01059                                                                   00010590
01060      IF NOT PI-LN-OFFICER-KEYED                                   00010591
01061          MOVE AL-UANOF           TO BLN-OFFICER-ATTRB.            00010592
01062                                                                   00010620
01063      IF NOT PI-ENTRY-KEYED                                        00010621
01064          MOVE AL-UANOF           TO BENTRY-ATTRB.                 00010622
01065                                                                   00010650
01066 *    IF NOT PI-ZIP4-KEYED                                         00010651
01067 *        MOVE AL-UANOF           TO BZIP4-ATTRB.                  00010652
01068                                                                   00010680
01069      IF NOT PI-POLICY-KEYED                                       00010681
01070          MOVE AL-UANOF           TO BPOLICY-ATTRB.                00010682
01071                                                                   00010710
01072      IF NOT PI-RINCD-KEYED                                        00010711
01073          MOVE AL-UANOF           TO BRINCD-ATTRB.                 00010712
01074                                                                   00010740
01075      IF NOT PI-RTCLS-KEYED                                        00010741
01076          MOVE AL-UANOF           TO BRTCLS-ATTRB.                 00010742
01077                                                                   00010743
01078                                                                   00010780
01079      IF NOT PI-LNTRM-KEYED                                        00010781
01080          MOVE AL-UANOF           TO BLN-TERM-ATTRB.               00010782
01081                                                                   00010783
01082                                                                   00010784
01083      IF NOT PI-EXPIRE-KEYED                                       00010785
01084          MOVE AL-UANOF           TO BEXPIRE-ATTRB (1)             00010786
01085                                     BEXPIRE-ATTRB (2).            00010787
01086                                                                   00010788
01087      IF NOT PI-CRIT-PERD-KEYED                                    00010789
01088          MOVE AL-UANOF           TO BCRIT-PERD-ATTRB (1)          00010790
01089          MOVE AL-UANOF           TO BCRIT-PERD-ATTRB (2).         00010791
01090                                                                   00010900
01091      IF NOT PI-PMT-KEYED                                          00010901
01092          MOVE AL-UANOF           TO BPMT-ATTRB.                   00010902
01093                                                                   00010930
01094      IF NOT PI-SKPCD-KEYED                                        00010931
01095          MOVE AL-UANOF           TO BSKPCD-ATTRB.                 00010932
01096                                                                   00010933
01097      IF PI-BIRTH-DATE-IS-INPUT                                    00010934
01098          MOVE AL-UANOF           TO BAGE-ATTRB                    00010935
01099      ELSE                                                         00010936
01100          MOVE AL-UANOF           TO BBIRTH-ATTRB.                 00010937
01101                                                                   00011010
01102      IF NOT PI-JNT-AGE-KEYED                                      00011011
01103          MOVE AL-UANOF           TO BJNT-AGE-ATTRB.               00011012
01104                                                                   00011013
01105      IF NOT PI-JNT-NAME-KEYED                                     00011014
01106          MOVE AL-UANOF           TO BJNT-INIT-ATTRB               00011015
01107                                     BJNT-LST-NAME-ATTRB           00011016
01108                                     BJNT-1ST-NAME-ATTRB.          00011017
01109                                                                   00011090
01110      IF NOT PI-BENEFICIARY-KEYED                                  00011091
01111          MOVE AL-UANOF           TO BBENEFICIARY-ATTRB.           00011092
01112                                                                   00011120
01113      IF NOT PI-PHONE-KEYED                                        00011121
01114          MOVE AL-UANOF           TO BPHONE-ATTRB.                 00011122
01115                                                                   00011150
01116      IF NOT PI-ALT-BEN-KEYED                                      00011151
01117          MOVE AL-UANOF           TO BALT-BEN-ATTRB (1).           00011152
01118                                                                   00011180
01119      IF NOT PI-ALT-PREM-KEYED                                     00011181
01120          MOVE AL-UANOF           TO BALT-PREM-ATTRB (1).          00011182
01121                                                                   00011183
01122  0610-EXIT.                                                       00011184
01123                                                                   00011185
01124      EXIT.                                                        00011186
01125                                                                   00011187
01126      EJECT                                                        00011188
01127                                                                   00011189
01128 ******************************************************************00011190
01129 *                                                                *00011191
01130 *         P R O T E C T   C A N C E L   F I E L D S              *00011192
01131 *                                                                *00011193
01132 ******************************************************************00011194
01133                                                                   00011195
01134  0700-PROTECT-FIELDS.                                             00011196
01135                                                                   00011197
01136      IF NOT PI-CAN-SUFFIX-KEYED                                   00011198
01137          MOVE AL-SANOF           TO CSFX-ATTRB (1)                00011199
01138                                     CSFX-ATTRB (2)                00011200
01139                                     CSFX-ATTRB (3)                00011201
01140                                     CSFX-ATTRB (4).               00011202
01141                                                                   00011203
01142      IF NOT PI-CAN-LIVES-KEYED                                    00011204
01143          MOVE AL-SANOF           TO CLIVES-ATTRB (1)              00011205
01144                                     CLIVES-ATTRB (2)              00011206
01145                                     CLIVES-ATTRB (3)              00011207
01146                                     CLIVES-ATTRB (4).             00011208
01147      IF NOT PI-PAYEE-KEYED                                        00011209
01148          MOVE AL-SANOF           TO CPAYEE-ATTRB (1)              00011210
01149                                     CPAYEE-ATTRB (2)              00011211
01150                                     CPAYEE-ATTRB (3)              00011212
01151                                     CPAYEE-ATTRB (4).             00011213
01152      IF NOT PI-CHK-REQ-KEYED                                      00011214
01153          MOVE AL-SANOF           TO CCHK-ATTRB   (1)              00011215
01154                                     CCHK-ATTRB   (2)              00011216
01155                                     CCHK-ATTRB   (3)              00011217
01156                                     CCHK-ATTRB   (4).             00011218
01157  0700-EXIT.                                                       00011219
01158      EXIT.                                                        00011220
01159                                                                   00011221
01160      EJECT                                                        00011222
01161                                                                   00011223
01162 ******************************************************************00011224
01163 *                                                                *00011225
01164 *         U N P R O T E C T   C A N C E L   F I E L D S          *00011226
01165 *                                                                *00011227
01166 ******************************************************************00011228
01167                                                                   00011229
01168  0710-UNPROTECT-FIELDS.                                           00011230
01169                                                                   00011231
01170      IF NOT PI-CAN-SUFFIX-KEYED                                   00011232
01171          MOVE AL-UANOF           TO CSFX-ATTRB (1)                00011233
01172                                     CSFX-ATTRB (2)                00011234
01173                                     CSFX-ATTRB (3)                00011235
01174                                     CSFX-ATTRB (4).               00011236
01175                                                                   00011237
01176      IF NOT PI-CAN-LIVES-KEYED                                    00011238
01177          MOVE AL-UANOF           TO CLIVES-ATTRB (1)              00011239
01178                                     CLIVES-ATTRB (2)              00011240
01179                                     CLIVES-ATTRB (3)              00011241
01180                                     CLIVES-ATTRB (4).             00011242
01181      IF NOT PI-PAYEE-KEYED                                        00011243
01182          MOVE AL-UANOF           TO CPAYEE-ATTRB (1)              00011244
01183                                     CPAYEE-ATTRB (2)              00011245
01184                                     CPAYEE-ATTRB (3)              00011246
01185                                     CPAYEE-ATTRB (4).             00011247
01186      IF NOT PI-CHK-REQ-KEYED                                      00011248
01187          MOVE AL-UANOF           TO CCHK-ATTRB   (1)              00011249
01188                                     CCHK-ATTRB   (2)              00011250
01189                                     CCHK-ATTRB   (3)              00011251
01190                                     CCHK-ATTRB   (4).             00011252
01191  0710-EXIT.                                                       00011253
01192      EXIT.                                                        00011254
01193                                                                   00011255
01194      EJECT                                                        00011256
01195                                                                   00011257
01196 ******************************************************************00011258
01197 *                                                                *00011259
01198 *     E D I T   D A T A   E N T R Y   I S S U E    S C R E EN    *00011260
01199 *                                                                *00011261
01200 ******************************************************************00011262
01201                                                                   00011263
01202  1000-EDIT-MAPB.                                                  00011264
01203                                                                   00012030
01204      IF PI-MAP-NAME NOT = EL930B                                  00012031
01205          GO TO 1100-EDIT-MAPC.                                    00012032
01206                                                                   00012033
01207      IF PI-LAST-FUNC-DISPLAY                                      00012034
01208        AND BSFX-LEN           = ZEROS                             00012035
01209        AND B1ST-NAME-LEN      = ZEROS                             00012036
01210        AND BLAST-NAME-LEN     = ZEROS                             00012037
01211        AND BINIT-LEN          = ZEROS                             00012038
01212        AND BJNT-1ST-NAME-LEN  = ZEROS                             00012039
01213        AND BJNT-INIT-LEN      = ZEROS                             00012040
01214        AND BJNT-LST-NAME-LEN  = ZEROS                             00012041
01215        AND BSEX-LEN           = ZEROS                             00012042
01216        AND BAGE-LEN           = ZEROS                             00012043
01217        AND BSSNUM-LEN         = ZEROS                             00012044
01218        AND BIND-GRP-LEN       = ZEROS                             00012045
01219        AND BAPR-LEN           = ZEROS                             00012046
01220        AND BFREQ-LEN          = ZEROS                             00012047
01221        AND BSIG-LEN           = ZEROS                             00012048
01222        AND BTERM-LEN     (1)  = ZEROS                             00012049
01223        AND BTERM-LEN     (2)  = ZEROS                             00012050
01224        AND BTYPE-LEN     (1)  = ZEROS                             00012051
01225        AND BTYPE-LEN     (2)  = ZEROS                             00012052
01226        AND BBEN-LEN      (1)  = ZEROS                             00012053
01227        AND BBEN-LEN      (2)  = ZEROS                             00012054
01228        AND BALT-BEN-LEN  (1)  = ZEROS                             00012055
01229        AND BPREM-LEN     (1)  = ZEROS                             00012056
01230        AND BPREM-LEN     (2)  = ZEROS                             00012057
01231        AND BALT-PREM-LEN (1)  = ZEROS                             00012058
01232        AND BLIVES-LEN         = ZEROS                             00012059
01233        AND BPOLICY-LEN        = ZEROS                             00012060
01234        AND BENTRY-LEN         = ZEROS                             00012061
01235        AND BRINCD-LEN         = ZEROS                             00012062
01236        AND BSSNUM-LEN         = ZEROS                             00012063
01237        AND BMEM-NO-LEN        = ZEROS                             00012064
01238        AND BJNT-AGE-LEN       = ZEROS                             00012065
01239        AND BBENEFICIARY-LEN   = ZEROS                             00012066
01240        AND BBIRTH-LEN         = ZEROS                             00012067
01241        AND BMODE-LEN          = ZEROS                             00012068
01242        AND BPMTS-LEN          = ZEROS                             00012069
01243        AND BLN-OFFICER-LEN    = ZEROS                             00012070
01244        AND BDAYS-LEN          = ZEROS                             00012071
01245        AND BLN-TERM-LEN       = ZEROS                             00012072
01246        AND BEXPIRE-LEN (1)    = ZEROS                             00012073
01247        AND BEXPIRE-LEN (2)    = ZEROS                             00012074
01248        AND BPMT-LEN           = ZEROS                             00012075
01249        AND B1ST-PMT-LEN       = ZEROS                             00012076
01250        AND BSKPCD-LEN         = ZEROS                             00012077
01251        AND BADDRS1-LEN        = ZEROS                             00012078
01252        AND BADDRS2-LEN        = ZEROS                             00012079
01253        AND BCITYST-LEN        = ZEROS                             00012080
01254        AND BZIP5-LEN          = ZEROS                             00012081
01255        AND BAGE-LEN           = ZEROS                             00012082
01256        AND BZIP4-LEN          = ZEROS                             00012083
01257        AND BPHONE-LEN         = ZEROS                             00012084
01258          MOVE SPACE              TO PI-DISPLAY-SW                 00012085
01259          GO TO 1030-NOTHING-TO-EDIT.                              00012086
01260                                                                   00012087
01261  1010-EDIT-MAPB.                                                  00012088
01262                                                                   00012089
01263      IF BCERT-LEN             = ZEROS                             00012090
01264        AND BLAST-NAME-LEN     = ZEROS                             00012091
01265        AND BEFFDT-LEN         = ZEROS                             00012092
01266        AND NOT PI-LAST-FUNC-DISPLAY                               00012093
01267          GO TO 1030-NOTHING-TO-EDIT.                              00012094
01268                                                                   00012680
01269      MOVE AL-SABON               TO BSEQ-ATTRB.                   00012681
01270                                                                   00012682
01271                                                                   00012683
01272      IF NOT PI-LAST-FUNC-DISPLAY                                  00012684
01273         IF BCAR-LEN GREATER THAN ZEROS                            00012685
01274            MOVE AL-UANON           TO BCAR-ATTRB                  00012686
01275            PERFORM 1500-VERIFY-CARRIER-ID THRU 1590-EXIT          00012687
01276         ELSE                                                      00012688
01277            IF NOT ST-ACCNT-CNTL AND NOT ACCNT-CNTL                00012689
01278                MOVE -1             TO BCAR-LEN                    00012690
01279                MOVE AL-UABON       TO BCAR-ATTRB                  00012691
01280                MOVE ER-0194        TO EMI-ERROR                   00012692
01281                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.          00012693
01282                                                                   00012694
01283      IF NOT PI-LAST-FUNC-DISPLAY                                  00012695
01284         IF BGRP-LEN GREATER THAN ZEROS                            00012696
01285            MOVE AL-UANON           TO BGRP-ATTRB                  00012697
01286         ELSE                                                      00012698
01287            IF CARR-GROUP-ST-ACCNT-CNTL                            00012699
01288                MOVE -1 TO          BGRP-LEN                       00012700
01289                MOVE AL-UABON       TO BGRP-ATTRB                  00012701
01290                MOVE ER-0195        TO EMI-ERROR                   00012702
01291                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.          00012703
01292                                                                   00012704
01293      IF NOT PI-LAST-FUNC-DISPLAY                                  00012705
01294        IF BST-LEN GREATER THAN ZEROS                              00012706
01295            MOVE AL-UANON           TO BST-ATTRB                   00012707
01296            PERFORM 1600-VERIFY-STATE-ID THRU 1690-EXIT            00012708
01297        ELSE                                                       00012709
01298            IF NOT ACCNT-CNTL AND NOT CARR-ACCNT-CNTL              00012710
01299                MOVE -1             TO BST-LEN                     00012711
01300                MOVE AL-UABON       TO BST-ATTRB                   00012712
01301                MOVE ER-0196        TO EMI-ERROR                   00012713
01302                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.          00012714
01303                                                                   00012715
01304      IF NOT PI-LAST-FUNC-DISPLAY                                  00012716
01305         IF BACCT-LEN GREATER THAN ZEROS                           00012717
01306            MOVE AL-UANON           TO BACCT-ATTRB                 00012718
01307            PERFORM 1700-VERIFY-ACCOUNT THRU 1790-EXIT             00012719
01308         ELSE                                                      00012720
01309            MOVE -1 TO BACCT-LEN                                   00012721
01310            MOVE AL-UABON           TO BACCT-ATTRB                 00012722
01311            MOVE ER-0197            TO EMI-ERROR                   00012723
01312            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              00012724
01313                                                                   00012725
01314      IF BCERT-LEN  GREATER THAN ZEROS                             00012726
01315        AND PI-LAST-FUNC-DISPLAY                                   00012727
01316          NEXT SENTENCE                                            00012728
01317      ELSE                                                         00012729
01318          IF BCERT-LEN  GREATER THAN ZEROS                         00012730
01319              MOVE AL-UANON       TO BCERT-ATTRB                   00012731
01320          ELSE                                                     00012732
01321              MOVE -1             TO BCERT-LEN                     00012733
01322              MOVE ER-2218        TO EMI-ERROR                     00012734
01323              MOVE AL-UABON       TO BCERT-ATTRB                   00012735
01324              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            00012736
01325                                                                   00012737
01326      IF BSFX-LEN  NOT = ZEROS                                     00012738
01327          MOVE 'Y'                TO PI-ISS-SUFFIX-KEYED-SW        00012739
01328          MOVE AL-UANON           TO BSFX-ATTRB.                   00012740
01329                                                                   00012741
01330      IF BEFFDT-LEN  = ZEROS                                       00012742
01331        AND PI-LAST-FUNC-DISPLAY                                   00012743
01332          NEXT SENTENCE                                            00012744
01333      ELSE                                                         00012745
01334          IF BEFFDT-LEN   GREATER THAN ZEROS                       00012746
01335              MOVE AL-UNNON           TO BEFFDT-ATTRB              00012747
01336              IF BEFFDT   NUMERIC                                  00012748
01337                  MOVE 4              TO DC-OPTION-CODE            00012749
01338                  MOVE BEFFDT    TO DC-GREG-DATE-1-MDY             00012750
01339                  PERFORM 8500-DATE-CONVERT THRU 8500-EXIT         00012751
01340                  MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EFFDT        00012752
01341                  IF NO-CONVERSION-ERROR                           00012753
01342                      IF WS-CONVERTED-EFFDT NOT LESS THAN          00012754
01343                        PI-ACCT-LOW-EFF-DT  AND LESS THAN          00012755
01344                        PI-ACCT-HIGH-EXP-DT                        00012756
01345                          NEXT SENTENCE                            00012757
01346                      ELSE                                         00012758
01347                          MOVE -1       TO BEFFDT-LEN              00012759
01348                          MOVE ER-2589  TO EMI-ERROR               00012760
01349                          MOVE AL-UNBON TO BEFFDT-ATTRB            00012761
01350                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT 00012762
01351                  ELSE                                             00012763
01352                      MOVE -1         TO BEFFDT-LEN                00012764
01353                      MOVE ER-2226    TO EMI-ERROR                 00012765
01354                      MOVE AL-UNBON   TO BEFFDT-ATTRB              00012766
01355                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     00012767
01356              ELSE                                                 00012768
01357                  MOVE -1             TO BEFFDT-LEN                00012769
01358                  MOVE ER-2223        TO EMI-ERROR                 00012770
01359                  MOVE AL-UNBON       TO BEFFDT-ATTRB              00012771
01360                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         00012772
01361          ELSE                                                     00012773
01362              MOVE -1                 TO BEFFDT-LEN                00012774
01363              MOVE ER-2220            TO EMI-ERROR                 00012775
01364              MOVE AL-UNBON           TO BEFFDT-ATTRB              00012776
01365              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            00012777
01366                                                                   00013660
01367      IF BLAST-NAME-LEN   GREATER THAN ZEROS                       00013661
01368          MOVE AL-UANON           TO BLAST-NAME-ATTRB.             00013662
01369                                                                   00013690
01370      IF B1ST-NAME-LEN    GREATER THAN ZEROS                       00013691
01371          MOVE AL-UANON           TO B1ST-NAME-ATTRB.              00013692
01372                                                                   00013720
01373      IF BINIT-LEN        GREATER THAN ZEROS                       00013721
01374          MOVE AL-UANON           TO BINIT-ATTRB.                  00013722
01375                                                                   00013723
01376      IF BSEX-LEN         GREATER THAN ZEROS                       00013724
01377          IF BSEX   = 'M' OR 'F'                                   00013725
01378              MOVE AL-UANON       TO BSEX-ATTRB                    00013726
01379          ELSE                                                     00013727
01380              MOVE -1             TO BSEX-LEN                      00013728
01381              MOVE ER-2629        TO EMI-ERROR                     00013729
01382              MOVE AL-UABON       TO BSEX-ATTRB                    00013730
01383              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            00013731
01384                                                                   00013732
01385      IF BAGE-LEN        GREATER THAN ZEROS                        00013733
01386         IF BAGE NUMERIC                                           00013734
01387            MOVE BAGE             TO WS-BAGE                       00013735
01388            MOVE AL-UNNON         TO BAGE-ATTRB                    00013736
01389         ELSE                                                      00013737
01390            MOVE -1             TO BAGE-LEN                        00013738
01391            MOVE ER-2223        TO EMI-ERROR                       00013739
01392            MOVE AL-UNBON       TO BAGE-ATTRB                      00013740
01393            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              00013741
01394                                                                   00013742
01395                                                                   00013743
01396      IF BSSNUM-LEN          GREATER THAN ZEROS                    00013744
01397          MOVE 'Y'                TO PI-SSNUM-KEYED-SW             00013745
01398          MOVE AL-UANON           TO BSSNUM-ATTRB.                 00013746
01399                                                                   00013747
01400      MOVE +0                     TO WS-SUB1.                      00013748
01401                                                                   00013749
01402      EJECT                                                        00013750
01403                                                                   00013751
01404 ******************************************************************00013752
01405 *                                                                *00013753
01406 *           E D I T   I S S U E   C O V E R A G E S              *00013754
01407 *                                                                *00013755
01408 ******************************************************************00013756
01409                                                                   00013757
01410  1020-EDIT-COVERAGES.                                             00013758
01411                                                                   00013759
01412      IF NOT MODIFY-CAP                                            00013760
01413            MOVE 'UPDATE'       TO SM-READ                         00013761
01414            PERFORM 9995-SECURITY-VIOLATION                        00013762
01415            MOVE ER-0070        TO EMI-ERROR                       00013763
01416            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               00013764
01417            GO TO 8100-SEND-INITIAL-MAP.                           00013765
01418                                                                   00013766
01419      ADD +1                      TO WS-SUB1.                      00013767
01420                                                                   00014200
01421      IF WS-SUB1 GREATER THAN +2                                   00014201
01422         GO TO 1025-CONT-EDIT.                                     00014202
01423                                                                   00014203
01424      IF BTYPE-LEN          (WS-SUB1)  GREATER THAN ZEROS          00014204
01425         OR  BTERM-LEN      (WS-SUB1)  GREATER THAN ZEROS          00014205
01426         OR  BBEN-LEN       (WS-SUB1)  GREATER THAN ZEROS          00014206
01427         OR  BPREM-LEN      (WS-SUB1)  GREATER THAN ZEROS          00014207
01428         OR  BCRIT-PERD-LEN (WS-SUB1)  GREATER THAN ZEROS          00014208
01429         OR  BEXPIRE-LEN    (WS-SUB1)  GREATER THAN ZEROS          00014209
01430         OR  BALT-PREM-LEN  (WS-SUB1)  GREATER THAN ZEROS          00014210
01431         OR  BALT-BEN-LEN   (WS-SUB1)  GREATER THAN ZEROS          00014211
01432             MOVE 'Y'             TO WS-DATA-KEYED-SW              00014212
01433      ELSE                                                         00014213
01434             GO TO 1020-EDIT-COVERAGES.                            00014214
01435                                                                   00014215
01436      IF NOT PI-LAST-FUNC-DISPLAY                                  00014216
01437         IF BTYPE-LEN  (WS-SUB1) GREATER THAN ZEROS                00014217
01438              MOVE AL-UANON         TO BTYPE-ATTRB       (WS-SUB1) 00014218
01439              PERFORM 1040-EDIT-INPUT-CODE THRU 1059-EXIT          00014219
01440         ELSE                                                      00014220
01441            NEXT SENTENCE                                          00014221
01442      ELSE                                                         00014222
01443         IF BTYPE-LEN  (WS-SUB1) GREATER THAN ZEROS                00014223
01444            IF BTYPE   (WS-SUB1) = SPACES OR ZEROS                 00014224
01445               MOVE AL-UANON      TO BTYPE-ATTRB       (WS-SUB1)   00014225
01446            ELSE                                                   00014226
01447               MOVE AL-UANON      TO BTYPE-ATTRB       (WS-SUB1)   00014227
01448               PERFORM 1040-EDIT-INPUT-CODE THRU 1059-EXIT.        00014228
01449                                                                   00014229
01450      IF BPMTS-LEN           GREATER THAN ZEROS                    00014230
01451          MOVE 'Y'                TO PI-PMTS-KEYED-SW              00014231
01452          MOVE BPMTS-IN           TO DEEDIT-FIELD                  00014232
01453          PERFORM 8600-DEEDIT                                      00014233
01454          IF DEEDIT-FIELD-V0 NUMERIC                               00014234
01455             MOVE DEEDIT-FIELD-V0 TO WS-BPMTS                      00014235
01456             MOVE AL-UNNON        TO BPMTS-ATTRB.                  00014236
01457                                                                   00014237
01458      IF BPMT-LEN              GREATER THAN ZEROS                  00014238
01459          MOVE 'Y'                TO PI-PMT-KEYED-SW               00014239
01460          MOVE BPMTI              TO DEEDIT-FIELD                  00014240
01461          PERFORM 8600-DEEDIT                                      00014241
01462          IF DEEDIT-FIELD-V2     NUMERIC                           00014242
01463            MOVE DEEDIT-FIELD-V2 TO WS-BPMT                        00014243
01464            MOVE AL-UNNON           TO BPMT-ATTRB.                 00014244
01465                                                                   00014245
01466      IF BTERM-LEN (WS-SUB1) GREATER THAN ZEROS                    00014246
01467         NEXT SENTENCE                                             00014247
01468      ELSE                                                         00014248
01469         IF BLN-TERM-LEN GREATER THAN ZERO                         00014249
01470            AND WS-BLN-TERM NUMERIC                                00014250
01471            MOVE WS-BLN-TERM      TO BTERMI      (WS-SUB1)         00014251
01472            MOVE +3               TO BTERM-LEN   (WS-SUB1).        00014252
01473                                                                   00014253
01474      IF  WS-TERM-IN-DAYS-FOUND                                    00014254
01475          AND BMODE-LEN   GREATER THAN ZERO                        00014255
01476          PERFORM 1090-CALCULATE-MONTHLY-TERM THRU 1094-EXIT       00014256
01477      ELSE                                                         00014257
01478          IF BMODE-LEN   GREATER THAN ZEROS                        00014258
01479             AND BPMTS-LEN   GREATER THAN ZEROS                    00014259
01480             PERFORM 1080-TERM-CONVERSION THRU 1089-EXIT.          00014260
01481                                                                   00014261
01482      IF PI-LAST-FUNC-DISPLAY                                      00014262
01483         IF BTERM-LEN (WS-SUB1)  = ZEROS                           00014263
01484            NEXT SENTENCE                                          00014264
01485         ELSE                                                      00014265
01486            MOVE BTERMI    (WS-SUB1)   TO DEEDIT-FIELD             00014266
01487            PERFORM 8600-DEEDIT                                    00014267
01488            IF DEEDIT-FIELD-V0 NUMERIC                             00014268
01489               MOVE DEEDIT-FIELD-V0    TO WS-BTERM    (WS-SUB1)    00014269
01490               IF WS-BTERM (WS-SUB1)  GREATER THAN ZERO            00014270
01491                     MOVE AL-UNNON     TO BTERM-ATTRB (WS-SUB1)    00014271
01492               ELSE                                                00014272
01493                     MOVE ER-2241      TO EMI-ERROR                00014273
01494                     MOVE -1           TO BTERM-LEN   (WS-SUB1)    00014274
01495                     MOVE AL-UNBOF     TO BTERM-ATTRB (WS-SUB1)    00014275
01496                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT      00014276
01497            ELSE                                                   00014277
01498               MOVE ER-2223          TO EMI-ERROR                  00014278
01499               MOVE -1               TO BTERM-LEN   (WS-SUB1)      00014279
01500               MOVE AL-UNBON         TO BTERM-ATTRB (WS-SUB1)      00014280
01501               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            00014281
01502      ELSE                                                         00014282
01503         IF  BTERM-LEN      (WS-SUB1)   GREATER THAN ZEROS         00014283
01504             MOVE BTERMI    (WS-SUB1)   TO DEEDIT-FIELD            00014284
01505             PERFORM 8600-DEEDIT                                   00014285
01506             IF DEEDIT-FIELD-V0      NUMERIC                       00014286
01507                IF DEEDIT-FIELD-V0     GREATER THAN ZERO           00014287
01508                   MOVE DEEDIT-FIELD-V0 TO WS-BTERM    (WS-SUB1)   00014288
01509                   MOVE AL-UNNON        TO BTERM-ATTRB (WS-SUB1)   00014289
01510                ELSE                                               00014290
01511                   MOVE ER-2241         TO EMI-ERROR               00014291
01512                   MOVE -1              TO BTERM-LEN   (WS-SUB1)   00014292
01513                   MOVE AL-UNBOF        TO BTERM-ATTRB (WS-SUB1)   00014293
01514                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        00014294
01515             ELSE                                                  00014295
01516               MOVE ER-2223             TO EMI-ERROR               00014296
01517               MOVE -1                  TO BTERM-LEN   (WS-SUB1)   00014297
01518               MOVE AL-UNBON            TO BTERM-ATTRB (WS-SUB1)   00014298
01519               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            00014299
01520         ELSE                                                      00014300
01521             MOVE ER-2240               TO EMI-ERROR               00014301
01522             MOVE -1                    TO BTERM-LEN   (WS-SUB1)   00014302
01523             MOVE AL-UNBOF              TO BTERM-ATTRB (WS-SUB1)   00014303
01524             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             00014304
01525                                                                   00014305
01526      IF BBEN-LEN (WS-SUB1) = ZEROS                                00014306
01527         AND PI-LAST-FUNC-DISPLAY                                  00014307
01528            NEXT SENTENCE                                          00014308
01529      ELSE                                                         00014309
01530         IF BBEN-LEN (WS-SUB1) GREATER THAN ZEROS                  00014310
01531            MOVE AL-UNNON           TO BBEN-ATTRB (WS-SUB1)        00014311
01532            MOVE BBENI (WS-SUB1)    TO DEEDIT-FIELD                00014312
01533            PERFORM 8600-DEEDIT                                    00014313
01534            IF DEEDIT-FIELD-V2  NUMERIC                            00014314
01535               IF DEEDIT-FIELD-V2 GREATER THAN ZEROS               00014315
01536                  MOVE DEEDIT-FIELD-V2 TO WS-BBEN    (WS-SUB1)     00014316
01537               ELSE                                                00014317
01538                  MOVE ER-7632    TO EMI-ERROR                     00014318
01539                  MOVE -1         TO BBEN-LEN   (WS-SUB1)          00014319
01540                  MOVE AL-UNBOF   TO BBEN-ATTRB (WS-SUB1)          00014320
01541                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         00014321
01542            ELSE                                                   00014322
01543               MOVE ER-2223         TO EMI-ERROR                   00014323
01544               MOVE AL-UNBON        TO BBEN-ATTRB (WS-SUB1)        00014324
01545               MOVE -1              TO BBEN-LEN   (WS-SUB1)        00014325
01546         ELSE                                                      00014326
01547            MOVE ER-7632    TO EMI-ERROR                           00014327
01548            MOVE -1         TO BBEN-LEN   (WS-SUB1)                00014328
01549            MOVE AL-UNBOF   TO BBEN-ATTRB (WS-SUB1)                00014329
01550            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              00014330
01551                                                                   00014331
01552      IF BPREM-LEN    (WS-SUB1) = ZEROS                            00014332
01553         AND PI-LAST-FUNC-DISPLAY                                  00014333
01554            NEXT SENTENCE                                          00014334
01555      ELSE                                                         00014335
01556         IF BPREM-LEN (WS-SUB1) GREATER THAN ZEROS                 00014336
01557            MOVE AL-UNNON           TO BPREM-ATTRB (WS-SUB1)       00014337
01558            MOVE BPREMI (WS-SUB1)   TO DEEDIT-FIELD                00014338
01559            PERFORM 8600-DEEDIT                                    00014339
01560            IF DEEDIT-FIELD-V2  NUMERIC                            00014340
01561               IF DEEDIT-FIELD-V2 GREATER THAN ZEROS               00014341
01562                  MOVE DEEDIT-FIELD-V2 TO WS-BPREM   (WS-SUB1)     00014342
01563               ELSE                                                00014343
01564                  MOVE ER-7633    TO EMI-ERROR                     00014344
01565                  MOVE -1         TO BPREM-LEN   (WS-SUB1)         00014345
01566                  MOVE AL-UNBOF   TO BPREM-ATTRB (WS-SUB1)         00014346
01567                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         00014347
01568            ELSE                                                   00014348
01569               MOVE AL-UNBON   TO BPREM-ATTRB (WS-SUB1)            00014349
01570               MOVE ER-2223         TO EMI-ERROR                   00014350
01571               MOVE -1              TO BPREM-LEN  (WS-SUB1)        00014351
01572         ELSE                                                      00014352
01573            MOVE ER-7633    TO EMI-ERROR                           00014353
01574            MOVE -1         TO BPREM-LEN   (WS-SUB1)               00014354
01575            MOVE AL-UNBOF   TO BPREM-ATTRB (WS-SUB1)               00014355
01576            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              00014356
01577                                                                   00014357
01578      IF BEXPIRE-LEN (WS-SUB1)   GREATER THAN ZEROS                00014358
01579         MOVE 'Y'                 TO PI-EXPIRE-KEYED-SW            00014359
01580          IF BEXPIRE (WS-SUB1)    NUMERIC                          00014360
01581              MOVE AL-UNNON       TO BEXPIRE-ATTRB (WS-SUB1)       00014361
01582              MOVE 4              TO DC-OPTION-CODE                00014362
01583              MOVE BEXPIRE (WS-SUB1)   TO DC-GREG-DATE-1-MDY       00014363
01584              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT             00014364
01585              MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EXPIRDT (WS-SUB1)00014365
01586              IF NO-CONVERSION-ERROR                               00014366
01587                  NEXT SENTENCE                                    00014367
01588              ELSE                                                 00014368
01589                  MOVE -1         TO BEXPIRE-LEN   (WS-SUB1)       00014369
01590                  MOVE ER-2531    TO EMI-ERROR                     00014370
01591                  MOVE AL-UNBON   TO BEXPIRE-ATTRB (WS-SUB1)       00014371
01592                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         00014372
01593          ELSE                                                     00014373
01594              MOVE -1             TO BEXPIRE-LEN   (WS-SUB1)       00014374
01595              MOVE ER-2532        TO EMI-ERROR                     00014375
01596              MOVE AL-UNBON       TO BEXPIRE-ATTRB (WS-SUB1)       00014376
01597              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            00014377
01598                                                                   00014378
01599      IF BCRIT-PERD-LEN  (WS-SUB1)    GREATER THAN ZEROS           00014379
01600         MOVE 'Y'                     TO PI-CRIT-PERD-KEYED-SW     00014380
01601         MOVE BCRIT-PERDI (WS-SUB1)   TO DEEDIT-FIELD              00014381
01602         PERFORM 8600-DEEDIT                                       00014382
01603         IF DEEDIT-FIELD-V0 NUMERIC                                00014383
01604            MOVE DEEDIT-FIELD-V0      TO WS-BCRIT-PERD    (WS-SUB1)00014384
01605            MOVE AL-UNNON             TO BCRIT-PERD-ATTRB (WS-SUB1)00014385
01606          ELSE                                                     00014386
01607            MOVE -1                   TO BCRIT-PERD-LEN   (WS-SUB1)00014387
01608            MOVE AL-UNBON             TO BCRIT-PERD-ATTRB (WS-SUB1)00014388
01609            MOVE ER-2223              TO EMI-ERROR                 00014389
01610            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              00014390
01611                                                                   00014391
01612                                                                   00014392
01613      IF BALT-BEN-LEN (WS-SUB1) = ZEROS                            00014393
01614         AND PI-LAST-FUNC-DISPLAY                                  00014394
01615            NEXT SENTENCE                                          00014395
01616      ELSE                                                         00014396
01617         IF BALT-BEN-LEN (WS-SUB1) GREATER THAN ZEROS              00014397
01618            MOVE 'Y'              TO PI-ALT-BEN-KEYED-SW           00014398
01619            MOVE AL-UNNON         TO BALT-BEN-ATTRB (WS-SUB1)      00014399
01620            MOVE BALT-BENI (WS-SUB1)    TO DEEDIT-FIELD            00014400
01621            PERFORM 8600-DEEDIT                                    00014401
01622            IF DEEDIT-FIELD-V2  NUMERIC                            00014402
01623               MOVE DEEDIT-FIELD-V2 TO WS-BALT-BEN (WS-SUB1)       00014403
01624            ELSE                                                   00014404
01625               MOVE ER-2223       TO EMI-ERROR                     00014405
01626               MOVE AL-UNBON      TO BALT-BEN-ATTRB (WS-SUB1)      00014406
01627               MOVE -1            TO BALT-BEN-LEN   (WS-SUB1).     00014407
01628                                                                   00014408
01629      IF BALT-PREM-LEN    (WS-SUB1) = ZEROS                        00014409
01630         AND PI-LAST-FUNC-DISPLAY                                  00014410
01631            NEXT SENTENCE                                          00014411
01632      ELSE                                                         00014412
01633         IF BALT-PREM-LEN   (WS-SUB1) GREATER THAN ZEROS           00014413
01634            MOVE 'Y'              TO PI-ALT-PREM-KEYED-SW          00014414
01635            MOVE AL-UNNON              TO BALT-PREM-ATTRB (WS-SUB1)00014415
01636            MOVE BALT-PREMI (WS-SUB1)  TO DEEDIT-FIELD             00014416
01637            PERFORM 8600-DEEDIT                                    00014417
01638            IF DEEDIT-FIELD-V2  NUMERIC                            00014418
01639               MOVE DEEDIT-FIELD-V2 TO WS-BALT-PREM   (WS-SUB1)    00014419
01640            ELSE                                                   00014420
01641               MOVE AL-UNBON      TO BALT-PREM-ATTRB (WS-SUB1)     00014421
01642               MOVE ER-2223       TO EMI-ERROR                     00014422
01643               MOVE -1            TO BALT-PREM-LEN  (WS-SUB1).     00014423
01644                                                                   00014424
01645      GO TO 1020-EDIT-COVERAGES.                                   00014425
01646                                                                   00014426
01647  1025-CONT-EDIT.                                                  00014427
01648                                                                   00014428
01649      IF BLIVES-LEN               GREATER THAN ZEROS               00014429
01650         MOVE 'Y'                   TO PI-ISS-LIVES-KEYED-SW       00014430
01651         MOVE BLIVESI               TO DEEDIT-FIELD                00014431
01652         PERFORM 8600-DEEDIT                                       00014432
01653         IF DEEDIT-FIELD-V0 NUMERIC                                00014433
01654            MOVE DEEDIT-FIELD-V0    TO WS-BLIVES                   00014434
01655            MOVE AL-UNNON           TO BLIVES-ATTRB                00014435
01656         ELSE                                                      00014436
01657            MOVE -1                 TO BLIVES-LEN                  00014437
01658            MOVE AL-UNBON           TO BLIVES-ATTRB                00014438
01659            MOVE ER-2223            TO EMI-ERROR                   00014439
01660            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              00014440
01661                                                                   00014441
01662      IF BJNT-1ST-NAME-LEN     GREATER THAN ZEROS OR               00014442
01663         BJNT-INIT-LEN         GREATER THAN ZEROS OR               00014443
01664         BJNT-LST-NAME-LEN     GREATER THAN ZEROS                  00014444
01665          MOVE 'Y'                TO PI-JNT-NAME-KEYED-SW          00014445
01666          MOVE AL-UANON           TO BJNT-1ST-NAME-ATTRB           00014446
01667                                     BJNT-INIT-ATTRB               00014447
01668                                     BJNT-LST-NAME-ATTRB.          00014448
01669                                                                   00014449
01670      IF BJNT-AGE-LEN        GREATER THAN ZEROS                    00014450
01671         MOVE 'Y'                 TO PI-JNT-AGE-KEYED-SW           00014451
01672         IF BJNT-AGE NUMERIC                                       00014452
01673            MOVE BJNT-AGE         TO WS-BJNT-AGE                   00014453
01674            MOVE AL-UNNON         TO BJNT-AGE-ATTRB                00014454
01675         ELSE                                                      00014455
01676            MOVE -1             TO BJNT-AGE-LEN                    00014456
01677            MOVE ER-2223        TO EMI-ERROR                       00014457
01678            MOVE AL-UNBON       TO BJNT-AGE-ATTRB                  00014458
01679            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              00014459
01680                                                                   00014460
01681      IF BBENEFICIARY-LEN    GREATER THAN ZEROS                    00014461
01682          MOVE 'Y'                TO PI-BENEFICIARY-KEYED-SW       00014462
01683          MOVE AL-UANON           TO BBENEFICIARY-ATTRB.           00014463
01684                                                                   00014464
01685      IF B1ST-PMT-LEN GREATER THAN ZEROS                           00014465
01686         MOVE 'Y'                     TO PI-1ST-PMT-KEYED-SW       00014466
01687         IF B1ST-PMT = SPACES                                      00014467
01688            MOVE LOW-VALUES           TO WS-CONVERTED-1ST-PMT-DT   00014468
01689         ELSE                                                      00014469
01690            MOVE B1ST-PMT             TO DEEDIT-FIELD              00014470
01691            PERFORM 8600-DEEDIT                                    00014471
01692            MOVE DEEDIT-FIELD-V0      TO DC-GREG-DATE-1-MDY        00014472
01693            MOVE AL-UNNON             TO B1ST-PMT-ATTRB            00014473
01694            MOVE 4                    TO DC-OPTION-CODE            00014474
01695            PERFORM 8500-DATE-CONVERT                              00014475
01696            IF NO-CONVERSION-ERROR                                 00014476
01697               MOVE DC-BIN-DATE-1     TO WS-CONVERTED-1ST-PMT-DT   00014477
01698               MOVE AL-UANON          TO B1ST-PMT-ATTRB            00014478
01699            ELSE                                                   00014479
01700               MOVE -1                TO B1ST-PMT-LEN              00014480
01701               MOVE ER-2200           TO EMI-ERROR                 00014481
01702               MOVE AL-UNBON          TO B1ST-PMT-ATTRB            00014482
01703               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           00014483
01704                                                                   00014484
01705      IF BDAYS-LEN       GREATER THAN ZEROS                        00014485
01706         MOVE 'Y'                TO PI-DAYS-KEYED-SW               00014486
01707         MOVE BDAYSI             TO DEEDIT-FIELD                   00014487
01708         PERFORM 8600-DEEDIT                                       00014488
01709         IF DEEDIT-FIELD-V0 NUMERIC                                00014489
01710            MOVE DEEDIT-FIELD-V0  TO WS-BDAYS                      00014490
01711            MOVE AL-UNNON        TO BDAYS-ATTRB                    00014491
01712         ELSE                                                      00014492
01713            MOVE -1              TO BDAYS-LEN                      00014493
01714            MOVE ER-7530         TO EMI-ERROR                      00014494
01715            MOVE AL-UNBON        TO BDAYS-ATTRB                    00014495
01716            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              00014496
01717                                                                   00014497
01718      IF BLN-TERM-LEN   = ZEROS                                    00014498
01719          NEXT SENTENCE                                            00014499
01720      ELSE                                                         00014500
01721         MOVE 'Y'                 TO PI-LNTRM-KEYED-SW             00014501
01722         MOVE BLN-TERMI           TO DEEDIT-FIELD                  00014502
01723         PERFORM 8600-DEEDIT                                       00014503
01724         IF DEEDIT-FIELD-V0 NUMERIC                                00014504
01725            MOVE DEEDIT-FIELD-V0  TO WS-BLN-TERM                   00014505
01726            IF WS-BLN-TERM  GREATER THAN ZERO                      00014506
01727               MOVE AL-UNNON TO BLN-TERM-ATTRB                     00014507
01728                  ELSE                                             00014508
01729                      MOVE ER-2241  TO EMI-ERROR                   00014509
01730                      MOVE -1       TO BLN-TERM-LEN                00014510
01731                      MOVE AL-UNBOF TO BLN-TERM-ATTRB              00014511
01732                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     00014512
01733              ELSE                                                 00014513
01734                  MOVE ER-2223      TO EMI-ERROR                   00014514
01735                  MOVE -1           TO BLN-TERM-LEN                00014515
01736                  MOVE AL-UNBON     TO BLN-TERM-ATTRB              00014516
01737                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        00014517
01738                                                                   00014518
01739      IF BLN-OFFICER-LEN      GREATER THAN ZEROS                   00014519
01740          MOVE 'Y'                TO PI-LN-OFFICER-KEYED-SW        00014520
01741          MOVE AL-UANON           TO BLN-OFFICER-ATTRB.            00014521
01742                                                                   00014522
01743      IF BMODE-LEN            GREATER THAN ZEROS                   00014523
01744          MOVE BMODE              TO WS-MODE-CODE                  00014524
01745          MOVE 'Y'                TO PI-MODE-KEYED-SW              00014525
01746          IF WS-MODE-CODE-VALID                                    00014526
01747              MOVE AL-UANON       TO BMODE-ATTRB                   00014527
01748          ELSE                                                     00014528
01749              MOVE -1             TO BMODE-LEN                     00014529
01750              MOVE ER-2591        TO EMI-ERROR                     00014530
01751              MOVE AL-UABON       TO BMODE-ATTRB                   00014531
01752              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            00014532
01753                                                                   00014533
01754      IF BFREQ-LEN             GREATER THAN ZEROS                  00014534
01755         MOVE 'Y'                 TO PI-FREQ-KEYED-SW              00014535
01756         MOVE BFREQI              TO DEEDIT-FIELD                  00014536
01757         PERFORM 8600-DEEDIT                                       00014537
01758         IF DEEDIT-FIELD-V0 NUMERIC                                00014538
01759            MOVE DEEDIT-FIELD-V0  TO WS-BFREQ                      00014539
01760            MOVE AL-UNNON         TO BFREQ-ATTRB                   00014540
01761         ELSE                                                      00014541
01762            MOVE -1             TO BFREQ-LEN                       00014542
01763            MOVE ER-2223        TO EMI-ERROR                       00014543
01764            MOVE AL-UNBON       TO BFREQ-ATTRB                     00014544
01765            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              00014545
01766                                                                   00014546
01767      IF BPMTS-LEN           GREATER THAN ZEROS                    00014547
01768          MOVE 'Y'                TO PI-PMTS-KEYED-SW              00014548
01769          MOVE BPMTS-IN           TO DEEDIT-FIELD                  00014549
01770          PERFORM 8600-DEEDIT                                      00014550
01771          IF DEEDIT-FIELD-V0 NUMERIC                               00014551
01772             MOVE DEEDIT-FIELD-V0 TO WS-BPMTS                      00014552
01773             MOVE AL-UNNON        TO BPMTS-ATTRB                   00014553
01774          ELSE                                                     00014554
01775              MOVE -1             TO BPMTS-LEN                     00014555
01776              MOVE ER-2592        TO EMI-ERROR                     00014556
01777              MOVE AL-UNBON       TO BPMTS-ATTRB                   00014557
01778              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            00014558
01779                                                                   00014559
01780      IF BPMT-LEN              GREATER THAN ZEROS                  00014560
01781          MOVE 'Y'                TO PI-PMT-KEYED-SW               00014561
01782          MOVE BPMTI              TO DEEDIT-FIELD                  00014562
01783          PERFORM 8600-DEEDIT                                      00014563
01784          IF DEEDIT-FIELD-V2     NUMERIC                           00014564
01785            MOVE DEEDIT-FIELD-V2 TO WS-BPMT                        00014565
01786            MOVE AL-UNNON           TO BPMT-ATTRB                  00014566
01787         ELSE                                                      00014567
01788            MOVE -1                 TO BPMT-LEN                    00014568
01789            MOVE ER-2529            TO EMI-ERROR                   00014569
01790            MOVE AL-UNBON           TO BPMT-ATTRB                  00014570
01791            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              00014571
01792                                                                   00014572
01793      IF BRINCD-LEN              GREATER THAN ZEROS                00014573
01794          MOVE 'Y'                TO PI-RINCD-KEYED-SW             00014574
01795          MOVE AL-UANON           TO BRINCD-ATTRB.                 00014575
01796                                                                   00014576
01797      IF BENTRY-LEN           GREATER THAN ZEROS                   00014577
01798          MOVE 'Y'                TO PI-ENTRY-KEYED-SW             00014578
01799          MOVE BENTRY             TO WS-ENTRY-CODE                 00014579
01800          IF WS-ENTRY-CODE-VALID                                   00014580
01801             MOVE AL-UANON       TO BENTRY-ATTRB                   00014581
01802          ELSE                                                     00014582
01803             MOVE ER-7573        TO EMI-ERROR                      00014583
01804             MOVE -1             TO BENTRY-LEN                     00014584
01805             MOVE AL-UABON       TO BENTRY-ATTRB                   00014585
01806             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             00014586
01807                                                                   00014587
01808      IF  BSKPCD-LEN          GREATER THAN ZEROS                   00014588
01809          MOVE 'Y'                TO PI-SKPCD-KEYED-SW             00014589
01810          MOVE AL-UANON           TO BSKPCD-ATTRB.                 00014590
01811                                                                   00014591
01812      IF BIND-GRP-LEN        GREATER THAN ZEROS                    00014592
01813          MOVE 'Y'                TO PI-IG-KEYED-SW                00014593
01814          MOVE AL-UANON           TO BIND-GRP-ATTRB.               00014594
01815                                                                   00014595
01816      IF BSIG-LEN            GREATER THAN ZEROS                    00014596
01817          MOVE 'Y'                TO PI-SIG-KEYED-SW               00014597
01818          IF BSIG   = 'Y'                                          00014598
01819              MOVE AL-UANON       TO BSIG-ATTRB                    00014599
01820          ELSE                                                     00014600
01821              MOVE -1             TO BSIG-LEN                      00014601
01822              MOVE ER-2651        TO EMI-ERROR                     00014602
01823              MOVE AL-UABON       TO BSIG-ATTRB                    00014603
01824              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            00014604
01825                                                                   00014605
01826      IF BPOLICY-LEN          GREATER THAN ZEROS                   00014606
01827          MOVE 'Y'                TO PI-POLICY-KEYED-SW            00014607
01828          MOVE AL-UANON           TO BPOLICY-ATTRB.                00014608
01829                                                                   00014609
01830      IF BRTCLS-LEN           GREATER THAN ZEROS                   00014610
01831          MOVE 'Y'                TO PI-RTCLS-KEYED-SW             00014611
01832          MOVE AL-UANON           TO BRTCLS-ATTRB.                 00014612
01833                                                                   00014613
01834      IF BAPR-LEN            GREATER THAN ZEROS                    00014614
01835          MOVE 'Y'                TO PI-APR-KEYED-SW               00014615
01836          MOVE BAPR-IN            TO DEEDIT-FIELD                  00014616
01837          PERFORM 8600-DEEDIT                                      00014617
01838          IF DEEDIT-FIELD-V4  NUMERIC                              00014618
01839              MOVE AL-UNNON       TO BAPR-ATTRB                    00014619
01840              MOVE DEEDIT-FIELD-V4 TO WS-BAPR                      00014620
01841          ELSE                                                     00014621
01842              MOVE ER-2471        TO EMI-ERROR                     00014622
01843              MOVE -1             TO BAPR-LEN                      00014623
01844              MOVE AL-UNBON       TO BAPR-ATTRB                    00014624
01845              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            00014625
01846                                                                   00014626
01847      IF BBIRTH-LEN   GREATER THAN ZEROS                           00014627
01848          IF BBIRTH-DT  NUMERIC                                    00014628
01849              MOVE AL-UNNON       TO BBIRTH-ATTRB                  00014629
01850              MOVE 4              TO DC-OPTION-CODE                00014630
01851              MOVE BBIRTH-DT      TO DC-GREG-DATE-1-MDY            00014631
01852              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT             00014632
01853              MOVE DC-BIN-DATE-1  TO WS-CONVERTED-BIRTH            00014633
01854              IF NO-CONVERSION-ERROR                               00014634
01855                  IF BAGE-LEN   = ZERO                             00014635
01856                      PERFORM 1095-CALC-AGE THRU 1099-EXIT         00014636
01857                  ELSE                                             00014637
01858                      NEXT SENTENCE                                00014638
01859              ELSE                                                 00014639
01860                  MOVE -1         TO BBIRTH-LEN                    00014640
01861                  MOVE ER-2228    TO EMI-ERROR                     00014641
01862                  MOVE AL-UNBON   TO BBIRTH-ATTRB                  00014642
01863                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         00014643
01864          ELSE                                                     00014644
01865              MOVE -1             TO BBIRTH-LEN                    00014645
01866              MOVE ER-2223        TO EMI-ERROR                     00014646
01867              MOVE AL-UNBON       TO BBIRTH-ATTRB                  00014647
01868              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             00014648
01869      ELSE                                                         00014649
01870          IF PI-LAST-FUNC-DISPLAY                                  00014650
01871              NEXT SENTENCE                                        00014651
01872          ELSE                                                     00014652
01873              IF PI-BIRTH-DATE-IS-INPUT                            00014653
01874                  MOVE ER-2442    TO EMI-ERROR                     00014654
01875                  MOVE -1         TO BBIRTH-LEN                    00014655
01876                  MOVE AL-UNBON   TO BBIRTH-ATTRB                  00014656
01877                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         00014657
01878              ELSE                                                 00014658
01879                  MOVE AL-SANOF   TO BBIRTH-ATTRB.                 00014659
01880                                                                   00014660
01881      IF BMEM-NO-LEN      GREATER THAN ZEROS                       00014661
01882          MOVE 'Y'                TO PI-MEMBER-KEYED-SW            00014662
01883          MOVE AL-UANON           TO BMEM-NO-ATTRB.                00014663
01884                                                                   00018840
01885      IF  BADDRS1-LEN        GREATER THAN ZEROS                    00018841
01886          MOVE AL-UANON           TO BADDRS1-ATTRB.                00018842
01887                                                                   00018870
01888      IF  BADDRS2-LEN        GREATER THAN ZEROS                    00018871
01889          MOVE AL-UANON           TO BADDRS2-ATTRB.                00018872
01890                                                                   00018900
01891      IF  BCITYST-LEN        GREATER THAN ZEROS                    00018901
01892          MOVE AL-UANON           TO BCITYST-ATTRB.                00018902
01893                                                                   00018930
01894      IF  BZIP5-LEN          GREATER THAN ZEROS                    00018931
01895          MOVE AL-UANON           TO BZIP5-ATTRB.                  00018932
01896                                                                   00018933
01897      IF  BZIP4-LEN          GREATER THAN ZEROS                    00018934
01898 *        MOVE 'Y'                TO PI-ZIP4-KEYED-SW              00018935
01899          MOVE AL-UANON           TO BZIP4-ATTRB.                  00018936
01900                                                                   00018937
01901      IF  BPHONE-LEN         GREATER THAN ZERO                     00018938
01902          MOVE 'Y'                TO PI-PHONE-KEYED-SW             00018939
01903          MOVE BPHONE             TO DEEDIT-FIELD                  00018940
01904          PERFORM 8600-DEEDIT                                      00018941
01905          MOVE DEEDIT-FIELD-V0 TO WS-BPHONE                        00018942
01906          MOVE AL-UANON       TO BPHONE-ATTRB.                     00018943
01907                                                                   00018944
01908      IF NOT PI-LAST-FUNC-DISPLAY                                  00018945
01909         AND WS-DATA-NOT-KEYED                                     00018946
01910         MOVE ER-7400             TO EMI-ERROR                     00018947
01911         MOVE -1                  TO BTYPE-LEN (1)                 00018948
01912         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  00018949
01913         MOVE 'Y'                 TO PI-ERROR-SW.                  00018950
01914                                                                   00018951
01915      IF EMI-ERROR = ZEROS                                         00018952
01916          MOVE 'Y'                TO PI-UPDATE-SW                  00018953
01917          MOVE SPACE              TO PI-DISPLAY-SW                 00018954
01918          PERFORM 4000-BUILD-ISSUE-RECORD THRU 4900-EXIT           00018955
01919      ELSE                                                         00018956
01920          MOVE ZEROS              TO EMI-ERROR                     00018957
01921          MOVE 'Y'                TO PI-ERROR-SW.                  00018958
01922                                                                   00018959
01923  1030-NOTHING-TO-EDIT.                                            00018960
01924                                                                   00018961
01925      IF PI-DATA-ERRORS                                            00018962
01926          MOVE AL-SABON           TO BSEQ-ATTRB                    00018963
01927          GO TO 8200-SEND-DATAONLY.                                00018964
01928                                                                   00018965
01929                                                                   00018966
01930      IF PI-SAV-BATCH-SEQ LESS THAN PI-LAST-SEQ-NO-ADDED           00018967
01931          SUBTRACT 1 FROM PI-SAV-BATCH-SEQ                         00018968
01932          MOVE ER-0000        TO EMI-ERROR                         00018969
01933          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00018970
01934          GO TO 2000-BROWSE-FWD.                                   00018971
01935                                                                   00018972
01936      MOVE LOW-VALUES     TO DATA-ENTRY-MAP.                       00018973
01937      ADD +1, PI-LAST-SEQ-NO-ADDED                                 00018974
01938                          GIVING PI-NEXT-DISPLAY-SEQ-NO.           00018975
01939      MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ.             00018976
01940                                                                   00018977
01941      PERFORM 8550-SET-MAP-SEQ-NOS.                                00018978
01942                                                                   00018979
01943      MOVE ER-0000        TO EMI-ERROR.                            00018980
01944      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00018981
01945      GO TO 8100-SEND-INITIAL-MAP.                                 00018982
01946                                                                   00018983
01947      EJECT                                                        00018984
01948                                                                   00018985
01949 ******************************************************************00018986
01950 *                                                                *00018987
01951 *            E D I T   I N P U T   C O D E S                     *00018988
01952 *                                                                *00018989
01953 ******************************************************************00018990
01954                                                                   00018991
01955  1040-EDIT-INPUT-CODE.                                            00018992
01956                                                                   00019560
01957      IF WS-SUB1 = +2                                              00019561
01958         GO TO 1050-EDIT-INPUT-AH-CODE.                            00019562
01959                                                                   00019563
01960      MOVE SPACES                 TO ELCNTL-ACCESS.                00019564
01961      MOVE 'L'                    TO ELCNTL-REC-TYPE.              00019565
01962      PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.                     00019566
01963                                                                   00019630
01964      IF EMI-ERROR = 9999                                          00019631
01965          GO TO 1048-NO-RECORD.                                    00019632
01966                                                                   00019633
01967      MOVE +1 TO WS-EDIT-SUB.                                      00019634
01968                                                                   00019635
01969  1041-SEARCH-LOOP.                                                00019636
01970                                                                   00019700
01971      IF CF-LIFE-CODE-OUT (WS-EDIT-SUB) = ZEROS                    00019701
01972          GO TO 1047-NO-MATCH-FOUND.                               00019702
01973                                                                   00019703
01974      IF BTYPE (1) = CF-LIFE-CODE-IN (WS-EDIT-SUB)                 00019704
01975          MOVE CF-LIFE-CODE-OUT (WS-EDIT-SUB) TO WS-EDITED-LF-CODE 00019705
01976          PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT          00019706
01977          GO TO 1059-EXIT.                                         00019707
01978                                                                   00019708
01979      ADD 1   TO WS-EDIT-SUB.                                      00019709
01980                                                                   00019710
01981      IF WS-EDIT-SUB GREATER THAN 120                              00019711
01982          GO TO 1047-NO-MATCH-FOUND.                               00019712
01983      GO TO 1041-SEARCH-LOOP.                                      00019713
01984                                                                   00019714
01985  1047-NO-MATCH-FOUND.                                             00019715
01986                                                                   00019716
01987      MOVE ER-2424                TO EMI-ERROR.                    00019717
01988      MOVE AL-UABON               TO BTYPE-ATTRB (1).              00019718
01989      MOVE -1                     TO BTYPE-LEN   (1).              00019719
01990      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00019720
01991      MOVE 'Y'                    TO ERROR-SW.                     00019721
01992      GO TO 1059-EXIT.                                             00019722
01993                                                                   00019723
01994  1048-NO-RECORD.                                                  00019724
01995                                                                   00019725
01996      MOVE ER-2423                TO EMI-ERROR.                    00019726
01997      MOVE AL-UABON               TO BTYPE-ATTRB (1).              00019727
01998      MOVE -1                     TO BTYPE-LEN   (1).              00019728
01999      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00019729
02000      MOVE 'Y'                    TO ERROR-SW.                     00019730
02001      GO TO 1059-EXIT.                                             00019731
02002                                                                   00020020
02003                                                                   00020021
02004  1050-EDIT-INPUT-AH-CODE.                                         00020022
02005                                                                   00020050
02006      MOVE SPACES                 TO ELCNTL-ACCESS.                00020051
02007      MOVE 'A'                    TO ELCNTL-REC-TYPE.              00020052
02008                                                                   00020053
02009      PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.                     00020054
02010                                                                   00020100
02011      IF EMI-ERROR = 9999                                          00020101
02012          GO TO 1058-NO-RECORD.                                    00020102
02013                                                                   00020103
02014      MOVE +1 TO WS-EDIT-SUB.                                      00020104
02015                                                                   00020105
02016  1051-SEARCH-LOOP.                                                00020106
02017                                                                   00020170
02018      IF CF-AH-CODE-OUT (WS-EDIT-SUB) = ZEROS                      00020171
02019          GO TO 1057-NO-MATCH-FOUND.                               00020172
02020                                                                   00020173
02021      IF BTYPE (2) = CF-AH-CODE-IN (WS-EDIT-SUB)                   00020174
02022          MOVE CF-AH-CODE-OUT (WS-EDIT-SUB) TO WS-EDITED-AH-CODE   00020175
02023          PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT          00020176
02024          GO TO 1059-EXIT.                                         00020177
02025                                                                   00020178
02026      ADD +1  TO WS-EDIT-SUB.                                      00020179
02027                                                                   00020270
02028      IF WS-EDIT-SUB GREATER THAN +96                              00020271
02029          GO TO 1057-NO-MATCH-FOUND.                               00020272
02030                                                                   00020273
02031      GO TO 1051-SEARCH-LOOP.                                      00020274
02032                                                                   00020275
02033  1057-NO-MATCH-FOUND.                                             00020276
02034                                                                   00020277
02035      MOVE ER-2428                TO EMI-ERROR.                    00020278
02036      MOVE AL-UABON               TO BTYPE-ATTRB (2).              00020279
02037      MOVE -1                     TO BTYPE-LEN   (2).              00020280
02038      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00020281
02039      MOVE 'Y'                    TO ERROR-SW.                     00020282
02040      GO TO 1059-EXIT.                                             00020283
02041                                                                   00020284
02042  1058-NO-RECORD.                                                  00020285
02043                                                                   00020286
02044      MOVE ER-2427                TO EMI-ERROR.                    00020287
02045      MOVE AL-UABON               TO BTYPE-ATTRB (2).              00020288
02046      MOVE -1                     TO BTYPE-LEN   (2).              00020289
02047      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00020290
02048      MOVE 'Y'                    TO ERROR-SW.                     00020291
02049                                                                   00020490
02050  1059-EXIT.                                                       00020491
02051      EXIT.                                                        00020492
02052                                                                   00020493
02053      EJECT                                                        00020494
02054                                                                   00020495
02055 ******************************************************************00020496
02056 *                                                                *00020497
02057 *         B E N E F I T   M A S T E R   R E A D                  *00020498
02058 *                                                                *00020499
02059 ******************************************************************00020500
02060                                                                   00020501
02061  1060-BENEFIT-MASTER-READ.                                        00020502
02062                                                                   00020503
02063      MOVE SPACES                 TO ELCNTL-ACCESS.                00020504
02064                                                                   00020505
02065      IF ELCNTL-REC-TYPE = 'L'                                     00020506
02066          MOVE WS-EDITED-LF-CODE  TO WS-BEN-CD                     00020507
02067                                     ELCNTL-HI-BEN                 00020508
02068          MOVE '4'                TO ELCNTL-REC-TYPE               00020509
02069      ELSE                                                         00020510
02070          MOVE WS-EDITED-AH-CODE  TO WS-BEN-CD                     00020511
02071                                     ELCNTL-HI-BEN                 00020512
02072          MOVE '5'                TO ELCNTL-REC-TYPE.              00020513
02073                                                                   00020514
02074      PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.                     00020515
02075                                                                   00020750
02076      IF EMI-ERROR = 9999                                          00020751
02077          GO TO 1062-NO-RECORD.                                    00020752
02078                                                                   00020753
02079      IF (ELCNTL-COMPANY-ID NOT  = CF-COMPANY-ID) OR               00020754
02080         (ELCNTL-REC-TYPE NOT = CF-RECORD-TYPE)                    00020755
02081            GO TO 1062-NO-RECORD.                                  00020756
02082                                                                   00020757
02083      PERFORM 1061-BENEFIT-DUMMY THRU 1061-DUMMY-EXIT              00020758
02084          VARYING WS-SUB FROM +1 BY +1 UNTIL                       00020759
02085             ((WS-SUB GREATER +8) OR                               00020760
02086             (CF-BENEFIT-CODE (WS-SUB) = WS-BEN-CD)).              00020761
02087                                                                   00020762
02088      IF WS-SUB NOT = +9                                           00020763
02089          IF ELCNTL-REC-TYPE = '4'                                 00020764
02090              MOVE CF-BENEFIT-ALPHA (WS-SUB) TO WS-LF-ABBR-DESC    00020765
02091          ELSE                                                     00020766
02092              MOVE CF-BENEFIT-ALPHA (WS-SUB) TO WS-AH-ABBR-DESC    00020767
02093      ELSE                                                         00020768
02094          GO TO 1063-NO-MATCH-FOUND.                               00020769
02095                                                                   00020950
02096      IF  CF-TERM-IN-DAYS (WS-SUB)                                 00020951
02097          MOVE 'Y'                TO WS-TERM-IN-DAYS-SW.           00020952
02098                                                                   00020953
02099      GO TO 1069-EXIT.                                             00020954
02100                                                                   00020955
02101  1061-BENEFIT-DUMMY.                                              00020956
02102                                                                   00021020
02103  1061-DUMMY-EXIT.                                                 00021021
02104      EXIT.                                                        00021022
02105                                                                   00021023
02106  1062-NO-RECORD.                                                  00021024
02107                                                                   00021025
02108      MOVE ER-2426                TO EMI-ERROR.                    00021026
02109                                                                   00021027
02110      IF ELCNTL-REC-TYPE = '4'                                     00021028
02111          MOVE AL-UABON           TO BTYPE-ATTRB (1)               00021029
02112          MOVE -1                 TO BTYPE-LEN   (1)               00021030
02113      ELSE                                                         00021031
02114          MOVE AL-UABON           TO BTYPE-ATTRB (2)               00021032
02115          MOVE -1                 TO BTYPE-LEN   (2).              00021033
02116                                                                   00021160
02117      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00021161
02118      MOVE 'Y'                    TO ERROR-SW.                     00021162
02119                                                                   00021163
02120      GO TO 1069-EXIT.                                             00021164
02121                                                                   00021165
02122  1063-NO-MATCH-FOUND.                                             00021166
02123                                                                   00021167
02124      IF ELCNTL-REC-TYPE = '4'                                     00021168
02125          MOVE ER-2425            TO EMI-ERROR                     00021169
02126          MOVE AL-UABON           TO BTYPE-ATTRB (1)               00021170
02127          MOVE -1                 TO BTYPE-LEN   (1)               00021171
02128      ELSE                                                         00021172
02129          MOVE ER-2429            TO EMI-ERROR                     00021173
02130          MOVE AL-UABON           TO BTYPE-ATTRB (2)               00021174
02131          MOVE -1                 TO BTYPE-LEN   (2).              00021175
02132                                                                   00021176
02133      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00021177
02134      MOVE 'Y'                    TO ERROR-SW.                     00021178
02135      GO TO 1069-EXIT.                                             00021179
02136                                                                   00021360
02137  1069-EXIT.                                                       00021361
02138      EXIT.                                                        00021362
02139                                                                   00021390
02140      EJECT                                                        00021391
02141                                                                   00021392
02142                                                                   00021393
02143 ******************************************************************00021394
02144 *                                                                *00021395
02145 *             C O N T R O L   F I L E   R E A D                  *00021396
02146 *                                                                *00021397
02147 ******************************************************************00021398
02148                                                                   00021399
02149  1070-ELCNTL-READ.                                                00021400
02150                                                                   00021401
02151      EXEC CICS HANDLE CONDITION                                   00021402
02152          NOTFND  (1078-NO-RECORD)                                 00021403
02153          ENDFILE (1078-NO-RECORD)                                 00021404
02154          END-EXEC.                                                00021405
02155                                                                   00021406
02156      IF ELCNTL-REC-TYPE = '1' OR '4' OR '5'                       00021407
02157          EXEC CICS READ                                           00021408
02158              DATASET (FILE-ID-ELCNTL)                             00021409
02159              SET     (ELCNTL-POINTER)                             00021410
02160              RIDFLD  (ELCNTL-KEY)                                 00021411
02161              GTEQ                                                 00021412
02162              END-EXEC                                             00021413
02163      ELSE                                                         00021414
02164          EXEC CICS READ                                           00021415
02165              DATASET (FILE-ID-ELCNTL)                             00021416
02166              SET     (ELCNTL-POINTER)                             00021417
02167              RIDFLD  (ELCNTL-KEY)                                 00021418
02168              END-EXEC.                                            00021419
02169                                                                   00021420
02170      SERVICE RELOAD CONTROL-FILE.                                 00021421
02171                                                                   00021422
02172      GO TO 1079-EXIT.                                             00021423
02173                                                                   00021424
02174  1078-NO-RECORD.                                                  00021425
02175                                                                   00021426
02176      MOVE ER-9999                TO EMI-ERROR.                    00021427
02177                                                                   00021770
02178  1079-EXIT.                                                       00021771
02179      EXIT.                                                        00021772
02180                                                                   00021800
02181      EJECT                                                        00021801
02182                                                                   00021802
02183                                                                   00021803
02184 ******************************************************************00021804
02185 *                                                                *00021805
02186 *   T E R M   C O N V E R S I O N   FOR   T E R M  IN  D A Y S   *00021806
02187 *                                                                *00021807
02188 ******************************************************************00021808
02189                                                                   00021809
02190  1080-TERM-CONVERSION.                                            00021810
02191                                                                   00021811
02192      IF BMODE   = ' ' OR 'M'                                      00021812
02193          MOVE WS-BPMTS      TO WS-CALC-TERM-WHOLE                 00021813
02194          GO TO 1085-ROUND-TERM.                                   00021814
02195                                                                   00021815
02196      IF BMODE   = 'S'                                             00021816
02197          COMPUTE WS-CALC-TERM = WS-BPMTS   / 2                    00021817
02198          GO TO 1085-ROUND-TERM.                                   00021818
02199                                                                   00021819
02200      IF BMODE   = 'W'                                             00021820
02201          COMPUTE WS-CALC-TERM = WS-BPMTS   / 4.33333              00021821
02202          GO TO 1085-ROUND-TERM.                                   00021822
02203                                                                   00022030
02204      IF BMODE   = 'B'                                             00022031
02205          COMPUTE WS-CALC-TERM = WS-BPMTS   / 2.16667.             00022032
02206                                                                   00022033
02207  1085-ROUND-TERM.                                                 00022034
02208                                                                   00022035
02209      IF WS-CALC-TERM-REMAIN GREATER THAN .00000                   00022036
02210          ADD +1                  TO WS-CALC-TERM-WHOLE.           00022037
02211      MOVE ZEROS                  TO WS-CALC-TERM-REMAIN.          00022038
02212                                                                   00022039
02213      IF  BTYPE-LEN       (WS-SUB1)  GREATER THAN ZEROS            00022040
02214          IF  WS-KIND-MONTHLY                                      00022041
02215              IF BTERM-LEN    (WS-SUB1)  EQUAL ZEROS               00022042
02216                 IF BPREM-LEN (WS-SUB1)  GREATER THAN ZEROS        00022043
02217                    IF  BPMT-LEN   GREATER THAN ZEROS              00022044
02218                        NEXT SENTENCE                              00022045
02219                    ELSE                                           00022046
02220                        GO TO 1087-EDIT-TERM.                      00022047
02221                                                                   00022048
02222      IF  BMODE   = 'M'                                            00022049
02223          MOVE WS-BPMT            TO  BBENO      (WS-SUB1)         00022050
02224                                      WS-BBEN    (WS-SUB1)         00022051
02225          MOVE AL-UNNON           TO  BBEN-ATTRB (WS-SUB1)         00022052
02226          MOVE +12                TO  BBEN-LEN   (WS-SUB1)         00022053
02227          GO TO 1087-EDIT-TERM.                                    00022054
02228                                                                   00022055
02229      IF  BMODE   = 'W'                                            00022056
02230          COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 4.33333    00022057
02231          MOVE    WS-BBEN (WS-SUB1) TO BBENO       (WS-SUB1)       00022058
02232          MOVE AL-UNNON             TO  BBEN-ATTRB (WS-SUB1)       00022059
02233          MOVE +12                  TO  BBEN-LEN   (WS-SUB1)       00022060
02234          GO TO 1087-EDIT-TERM.                                    00022061
02235                                                                   00022062
02236      IF  BMODE   = 'S'                                            00022063
02237          COMPUTE WS-BBEN (WS-SUB1)  ROUNDED = WS-BPMT * 2         00022064
02238          MOVE    WS-BBEN (WS-SUB1)  TO BBENO      (WS-SUB1)       00022065
02239          MOVE AL-UNNON              TO BBEN-ATTRB (WS-SUB1)       00022066
02240          MOVE +12                   TO BBEN-LEN   (WS-SUB1)       00022067
02241          GO TO 1087-EDIT-TERM.                                    00022068
02242                                                                   00022069
02243      IF  BMODE   = 'B'                                            00022070
02244          COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 2.16667    00022071
02245          MOVE    WS-BBEN (WS-SUB1) TO BBENO       (WS-SUB1)       00022072
02246          MOVE +12                  TO BBEN-LEN    (WS-SUB1)       00022073
02247          MOVE AL-UNNON             TO BBEN-ATTRB  (WS-SUB1).      00022074
02248                                                                   00022075
02249  1087-EDIT-TERM.                                                  00022076
02250                                                                   00022077
02251      IF BTERM-LEN (WS-SUB1)  = ZEROS                              00022078
02252          MOVE WS-CALC-TERM-WHOLE   TO BTERMI      (WS-SUB1)       00022079
02253                                       WS-BTERM    (WS-SUB1)       00022080
02254          MOVE +3                   TO BTERM-LEN   (WS-SUB1)       00022081
02255          GO TO 1089-EXIT.                                         00022082
02256                                                                   00022083
02257      MOVE BTERMI    (WS-SUB1)   TO DEEDIT-FIELD.                  00022084
02258      PERFORM 8600-DEEDIT.                                         00022085
02259      IF DEEDIT-FIELD-V0 NUMERIC                                   00022086
02260         MOVE DEEDIT-FIELD-V0    TO WS-BTERM    (WS-SUB1)          00022087
02261         IF WS-BTERM (WS-SUB1)  GREATER THAN ZERO                  00022088
02262            MOVE AL-UNNON        TO BTERM-ATTRB (WS-SUB1)          00022089
02263         ELSE                                                      00022090
02264            MOVE ER-2241         TO EMI-ERROR                      00022091
02265            MOVE -1              TO BTERM-LEN   (WS-SUB1)          00022092
02266            MOVE AL-UNBOF        TO BTERM-ATTRB (WS-SUB1)          00022093
02267            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               00022094
02268            GO TO 1089-EXIT.                                       00022095
02269                                                                   00022096
02270      IF WS-BTERM (WS-SUB1)   NOT = WS-CALC-TERM-WHOLE             00022097
02271          MOVE -1                   TO BTERM-LEN   (WS-SUB1)       00022098
02272          MOVE ER-2593              TO EMI-ERROR                   00022099
02273          MOVE AL-UNBON             TO BTERM-ATTRB (WS-SUB1)       00022100
02274          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                00022101
02275                                                                   00022750
02276  1089-EXIT.                                                       00022751
02277      EXIT.                                                        00022752
02278                                                                   00022753
02279      EJECT                                                        00022754
02280                                                                   00022755
02281 ******************************************************************00022756
02282 *                                                                *00022757
02283 *         CALCULATE MONTHLY TERM AND BENEFIT                     *00022758
02284 *                                                                *00022759
02285 ******************************************************************00022760
02286                                                                   00022761
02287  1090-CALCULATE-MONTHLY-TERM.                                     00022762
02288                                                                   00022763
02289      COMPUTE  WS-CALC-TERM = WS-BDAYS / 31.                       00022764
02290                                                                   00022900
02291      IF WS-CALC-TERM-REMAIN GREATER THAN .00000                   00022901
02292         ADD +1 TO WS-CALC-TERM.                                   00022902
02293                                                                   00022903
02294      MOVE ZEROS                  TO  WS-CALC-TERM-REMAIN.         00022904
02295      MOVE WS-CALC-TERM           TO  BTERMO    (2).               00022905
02296      MOVE +3                     TO  BTERM-LEN (2).               00022906
02297                                                                   00022907
02298      IF  BTYPE-LEN  (2)  GREATER THAN ZEROS                       00022908
02299          IF BTERM-LEN (2)  EQUAL ZEROS                            00022909
02300             IF BPREM-LEN (2) GREATER THAN ZEROS                   00022910
02301                IF  BPMT-LEN   GREATER THAN ZEROS                  00022911
02302                    NEXT SENTENCE                                  00022912
02303                ELSE                                               00022913
02304                    GO TO 1094-EXIT.                               00022914
02305                                                                   00022915
02306      IF  BMODE   = 'M'                                            00022916
02307          MOVE WS-BPMT            TO  WS-BBEN     (2)              00022917
02308                                      BBENO       (2)              00022918
02309          MOVE AL-UNNON           TO  BBEN-ATTRB  (2)              00022919
02310          MOVE +12                TO  BBEN-LEN    (2)              00022920
02311          GO TO 1094-EXIT.                                         00022921
02312                                                                   00022922
02313      IF  BMODE   = 'W'                                            00022923
02314          COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 4.33333    00022924
02315          MOVE WS-BBEN    (WS-SUB1)   TO BBENO       (WS-SUB1)     00022925
02316          MOVE  AL-UNNON              TO  BBEN-ATTRB (WS-SUB1)     00022926
02317          MOVE  +12                   TO  BBEN-LEN   (WS-SUB1)     00022927
02318          GO TO 1094-EXIT.                                         00022928
02319                                                                   00022929
02320      IF  BMODE   = 'S'                                            00022930
02321          COMPUTE WS-BBEN (2)        ROUNDED = WS-BPMT * 2         00022931
02322          MOVE WS-BBEN    (2)         TO  BBENO      (2)           00022932
02323          MOVE  AL-UNNON              TO  BBEN-ATTRB (2)           00022933
02324          MOVE  +12                   TO  BBEN-LEN   (2)           00022934
02325          GO TO 1094-EXIT.                                         00022935
02326                                                                   00022936
02327      IF  BMODE   = 'B'                                            00022937
02328          COMPUTE WS-BBEN (2)       ROUNDED = WS-BPMT * 2.16667    00022938
02329          MOVE WS-BBEN    (2)         TO  BBENO      (2)           00022939
02330          MOVE +12                    TO  BBEN-LEN   (2)           00022940
02331          MOVE AL-UNNON               TO  BBEN-ATTRB (2).          00022941
02332                                                                   00022942
02333                                                                   00023330
02334  1094-EXIT.                                                       00023331
02335      EXIT.                                                        00023332
02336                                                                   00023360
02337      EJECT                                                        00023361
02338                                                                   00023362
02339                                                                   00023363
02340 ******************************************************************00023364
02341 *                                                                *00023365
02342 *               CALCULATE INSURED'S AGE                          *00023366
02343 *                                                                *00023367
02344 ******************************************************************00023368
02345                                                                   00023369
02346  1095-CALC-AGE.                                                   00023370
02347                                                                   00023371
02348      MOVE WS-CONVERTED-BIRTH TO DC-BIN-DATE-1.                    00023372
02349      MOVE WS-CURRENT-BIN-DT TO  DC-BIN-DATE-2.                    00023373
02350      MOVE 1 TO DC-OPTION-CODE.                                    00023374
02351      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    00023375
02352      IF NO-CONVERSION-ERROR                                       00023376
02353          COMPUTE WS-BAGE = DC-ELAPSED-MONTHS / 12                 00023377
02354          MOVE WS-BAGE            TO BAGE                          00023378
02355          MOVE +2                 TO BAGE-LEN                      00023379
02356          MOVE AL-UNNON TO BAGE-ATTRB .                            00023380
02357                                                                   00023570
02358  1099-EXIT.                                                       00023571
02359      EXIT.                                                        00023572
02360                                                                   00023573
02361      EJECT                                                        00023574
02362                                                                   00023575
02363 ******************************************************************00023576
02364 *                                                                *00023577
02365 *    E D I T   D A T A   E N T R Y   C A N C E L   S C R E E N   *00023578
02366 *                                                                *00023579
02367 ******************************************************************00023580
02368                                                                   00023680
02369                                                                   00023681
02370  1100-EDIT-MAPC.                                                  00023682
02371                                                                   00023683
02372      MOVE +1                     TO WS-SUB2.                      00023684
02373                                                                   00023685
02374      IF PI-LAST-FUNC-DISPLAY                                      00023686
02375         AND CLAST-NAME-LEN (1)   = ZEROS                          00023687
02376         AND CLAST-NAME-LEN (2)   = ZEROS                          00023688
02377         AND CLAST-NAME-LEN (3)   = ZEROS                          00023689
02378         AND CLAST-NAME-LEN (4)   = ZEROS                          00023690
02379         AND CCANDT1-LEN     (1) = ZEROS                           00023691
02380         AND CCANDT2-LEN     (1) = ZEROS                           00023692
02381         AND CCANDT1-LEN     (2) = ZEROS                           00023693
02382         AND CCANDT2-LEN     (2) = ZEROS                           00023694
02383         AND CCANDT1-LEN     (3) = ZEROS                           00023695
02384         AND CCANDT2-LEN     (3) = ZEROS                           00023696
02385         AND CCANDT1-LEN     (4) = ZEROS                           00023697
02386         AND CCANDT2-LEN     (4) = ZEROS                           00023698
02387         AND CREFUND1-LEN    (1) = ZEROS                           00023699
02388         AND CREFUND2-LEN    (1) = ZEROS                           00023700
02389         AND CREFUND1-LEN    (2) = ZEROS                           00023701
02390         AND CREFUND2-LEN    (2) = ZEROS                           00023702
02391         AND CREFUND1-LEN    (3) = ZEROS                           00023703
02392         AND CREFUND2-LEN    (3) = ZEROS                           00023704
02393         AND CREFUND1-LEN    (4) = ZEROS                           00023705
02394         AND CREFUND2-LEN    (4) = ZEROS                           00023706
02395         AND CLIVES-LEN     (1) = ZEROS                            00023707
02396         AND CLIVES-LEN     (2) = ZEROS                            00023708
02397         AND CLIVES-LEN     (3) = ZEROS                            00023709
02398         AND CLIVES-LEN     (4) = ZEROS                            00023710
02399         AND CPAYEE-LEN     (1) = ZEROS                            00023711
02400         AND CPAYEE-LEN     (2) = ZEROS                            00023712
02401         AND CPAYEE-LEN     (3) = ZEROS                            00023713
02402         AND CPAYEE-LEN     (4) = ZEROS                            00023714
02403         AND CCHK-LEN       (1) = ZEROS                            00023715
02404         AND CCHK-LEN       (2) = ZEROS                            00023716
02405         AND CCHK-LEN       (3) = ZEROS                            00023717
02406         AND CCHK-LEN       (4) = ZEROS                            00023718
02407          GO TO 1130-NOTHING-TO-EDIT.                              00023719
02408                                                                   00023720
02409  1110-EDIT-MAPC-LOOP.                                             00023721
02410                                                                   00023722
02411      IF CCERT-LEN       (WS-SUB2) = ZEROS                         00023723
02412        AND CEFFDT-LEN   (WS-SUB2) = ZEROS                         00023724
02413        AND CCANDT1-LEN  (WS-SUB2) = ZEROS                         00023725
02414        AND CCANDT2-LEN  (WS-SUB2) = ZEROS                         00023726
02415        AND CREFUND1-LEN (WS-SUB2) = ZEROS                         00023727
02416        AND CREFUND2-LEN (WS-SUB2) = ZEROS                         00023728
02417        AND NOT PI-LAST-FUNC-DISPLAY                               00023729
02418          GO TO 1120-INCREMENT-OCCURANCE.                          00023730
02419                                                                   00023731
02420      MOVE 'Y'                    TO WS-DATA-KEYED-SW.             00023732
02421                                                                   00023733
02422      MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).         00023734
02423                                                                   00023735
02424      IF NOT PI-LAST-FUNC-DISPLAY                                  00023736
02425         IF CCAR-LEN (WS-SUB2) GREATER THAN ZEROS                  00023737
02426            MOVE AL-UANON           TO CCAR-ATTRB (WS-SUB2)        00023738
02427            PERFORM 1500-VERIFY-CARRIER-ID THRU 1590-EXIT          00023739
02428         ELSE                                                      00023740
02429            IF NOT ST-ACCNT-CNTL AND NOT ACCNT-CNTL                00023741
02430               MOVE -1             TO CCAR-LEN   (WS-SUB2)         00023742
02431               MOVE AL-UABON       TO CCAR-ATTRB (WS-SUB2)         00023743
02432               MOVE ER-0194        TO EMI-ERROR                    00023744
02433               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           00023745
02434                                                                   00023746
02435      IF NOT PI-LAST-FUNC-DISPLAY                                  00023747
02436         IF CGRP-LEN (WS-SUB2) GREATER THAN ZEROS                  00023748
02437            MOVE AL-UANON           TO CGRP-ATTRB (WS-SUB2)        00023749
02438         ELSE                                                      00023750
02439            IF CARR-GROUP-ST-ACCNT-CNTL                            00023751
02440               MOVE -1             TO CGRP-LEN      (WS-SUB2)      00023752
02441               MOVE AL-UABON       TO CGRP-ATTRB    (WS-SUB2)      00023753
02442               MOVE ER-0195        TO EMI-ERROR                    00023754
02443               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           00023755
02444                                                                   00023756
02445      IF NOT PI-LAST-FUNC-DISPLAY                                  00023757
02446         IF CST-LEN (WS-SUB2) GREATER THAN ZEROS                   00023758
02447            MOVE AL-UANON           TO CST-ATTRB  (WS-SUB2)        00023759
02448            PERFORM 1600-VERIFY-STATE-ID THRU 1690-EXIT            00023760
02449         ELSE                                                      00023761
02450            IF NOT ACCNT-CNTL AND NOT CARR-ACCNT-CNTL              00023762
02451               MOVE -1             TO CST-LEN    (WS-SUB2)         00023763
02452               MOVE AL-UABON       TO CST-ATTRB  (WS-SUB2)         00023764
02453               MOVE ER-0196        TO EMI-ERROR                    00023765
02454               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           00023766
02455                                                                   00023767
02456      IF NOT PI-LAST-FUNC-DISPLAY                                  00023768
02457         IF CACCT-LEN (WS-SUB2) GREATER THAN ZEROS                 00023769
02458            MOVE AL-UANON           TO CACCT-ATTRB (WS-SUB2)       00023770
02459            PERFORM 1700-VERIFY-ACCOUNT THRU 1790-EXIT             00023771
02460         ELSE                                                      00023772
02461            MOVE -1 TO CACCT-LEN      (WS-SUB2)                    00023773
02462            MOVE AL-UABON           TO CACCT-ATTRB (WS-SUB2)       00023774
02463            MOVE ER-0197            TO EMI-ERROR                   00023775
02464            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              00023776
02465                                                                   00023777
02466                                                                   00023778
02467      IF CCERT-LEN (WS-SUB2) = ZEROS                               00023779
02468        AND PI-LAST-FUNC-DISPLAY                                   00023780
02469          NEXT SENTENCE                                            00023781
02470      ELSE                                                         00023782
02471          IF CCERT-LEN (WS-SUB2)  NOT = ZEROS                      00023783
02472              MOVE AL-UANON       TO CCERT-ATTRB (WS-SUB2)         00023784
02473          ELSE                                                     00023785
02474              MOVE -1             TO CCERT-LEN   (WS-SUB2)         00023786
02475              MOVE ER-2218        TO EMI-ERROR                     00023787
02476              MOVE AL-UABON       TO CCERT-ATTRB (WS-SUB2)         00023788
02477              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            00023789
02478                                                                   00023790
02479      IF CSFX-LEN (WS-SUB1)      GREATER THAN ZEROS                00023791
02480          MOVE 'Y'                TO PI-CAN-SUFFIX-KEYED-SW        00023792
02481          MOVE AL-UANON           TO CSFX-ATTRB (WS-SUB2).         00023793
02482                                                                   00023794
02483      IF CEFFDT-LEN (WS-SUB2) = ZEROS                              00023795
02484        AND PI-LAST-FUNC-DISPLAY                                   00023796
02485          NEXT SENTENCE                                            00023797
02486      ELSE                                                         00023798
02487          IF CEFFDT-LEN (WS-SUB2) GREATER THAN ZEROS               00023799
02488              MOVE AL-UNNON           TO CEFFDT-ATTRB (WS-SUB2)    00023800
02489              IF CEFFDT (WS-SUB2) NUMERIC                          00023801
02490                  MOVE 4              TO DC-OPTION-CODE            00023802
02491                  MOVE CEFFDT (WS-SUB2)  TO DC-GREG-DATE-1-MDY     00023803
02492                  PERFORM 8500-DATE-CONVERT THRU 8500-EXIT         00023804
02493                  MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EFFDT        00023805
02494                  IF NO-CONVERSION-ERROR                           00023806
02495                      IF WS-CONVERTED-EFFDT NOT LESS THAN          00023807
02496                        PI-ACCT-LOW-EFF-DT AND LESS THAN           00023808
02497                        PI-ACCT-HIGH-EXP-DT                        00023809
02498                          NEXT SENTENCE                            00023810
02499                      ELSE                                         00023811
02500                          MOVE -1       TO CEFFDT-LEN (WS-SUB2)    00023812
02501                          MOVE ER-2589  TO EMI-ERROR               00023813
02502                          MOVE AL-UNBON TO CEFFDT-ATTRB (WS-SUB2)  00023814
02503                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT 00023815
02504                  ELSE                                             00023816
02505                      MOVE -1         TO CEFFDT-LEN (WS-SUB2)      00023817
02506                      MOVE ER-2226    TO EMI-ERROR                 00023818
02507                      MOVE AL-UNBON   TO CEFFDT-ATTRB (WS-SUB2)    00023819
02508                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     00023820
02509              ELSE                                                 00023821
02510                  MOVE -1             TO CEFFDT-LEN (WS-SUB2)      00023822
02511                  MOVE ER-2223        TO EMI-ERROR                 00023823
02512                  MOVE AL-UNBON       TO CEFFDT-ATTRB (WS-SUB2)    00023824
02513                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         00023825
02514          ELSE                                                     00023826
02515              MOVE -1                 TO CEFFDT-LEN (WS-SUB2)      00023827
02516              MOVE ER-2220            TO EMI-ERROR                 00023828
02517              MOVE AL-UNBON           TO CEFFDT-ATTRB (WS-SUB2)    00023829
02518              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            00023830
02519                                                                   00025190
02520      IF CLAST-NAME-LEN (WS-SUB2) GREATER THAN ZEROS               00025191
02521          MOVE AL-UANON           TO CLAST-NAME-ATTRB (WS-SUB2).   00025192
02522                                                                   00025193
02523                                                                   00025230
02524                                                                   00025231
02525      EJECT                                                        00025232
02526                                                                   00025233
02527 ******************************************************************00025234
02528 *                                                                *00025235
02529 *           E D I T   C A N C E L   C O V E R A G E S            *00025236
02530 *                                                                *00025237
02531 ******************************************************************00025238
02532                                                                   00025239
02533  1115-EDIT-COVERAGES.                                             00025240
02534                                                                   00025241
02535      IF CCANDT1-LEN (WS-SUB2) = ZEROS                             00025242
02536        AND PI-LAST-FUNC-DISPLAY                                   00025243
02537          NEXT SENTENCE                                            00025244
02538      ELSE                                                         00025245
02539          IF CCANDT1-LEN (WS-SUB2) GREATER THAN ZEROS              00025246
02540              MOVE AL-UNNON       TO CCANDT1-ATTRB (WS-SUB2)       00025247
02541              IF PI-LAST-FUNC-DISPLAY AND                          00025248
02542                 CCANDT1 (WS-SUB2) = SPACES                        00025249
02543                  MOVE LOW-VALUES TO WS-CONVERTED-CANDT1 (WS-SUB2) 00025250
02544              ELSE                                                 00025251
02545                 IF CCANDT1 (WS-SUB2) NUMERIC                      00025252
02546                    MOVE 4              TO DC-OPTION-CODE          00025253
02547                    MOVE CCANDT1 (WS-SUB2) TO                      00025254
02548                                       DC-GREG-DATE-1-MDY          00025255
02549                    PERFORM 8500-DATE-CONVERT THRU 8500-EXIT       00025256
02550                    MOVE DC-BIN-DATE-1  TO WS-CONVERTED-CANDT1     00025257
02551                                                          (WS-SUB2)00025258
02552                    IF NO-CONVERSION-ERROR                         00025259
02553                       NEXT SENTENCE                               00025260
02554                    ELSE                                           00025261
02555                       MOVE -1       TO CCANDT1-LEN   (WS-SUB2)    00025262
02556                       MOVE ER-2227  TO EMI-ERROR                  00025263
02557                       MOVE AL-UNBON TO CCANDT1-ATTRB (WS-SUB2)    00025264
02558                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    00025265
02559                 ELSE                                              00025266
02560                    MOVE -1         TO CCANDT1-LEN   (WS-SUB2)     00025267
02561                    MOVE ER-2223    TO EMI-ERROR                   00025268
02562                    MOVE AL-UNBON   TO CCANDT1-ATTRB (WS-SUB2)     00025269
02563                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT       00025270
02564          ELSE                                                     00025271
02565              IF CREFUND1-LEN (WS-SUB2) GREATER THAN ZEROS         00025272
02566                 MOVE -1             TO CCANDT1-LEN   (WS-SUB2)    00025273
02567                 MOVE ER-2222        TO EMI-ERROR                  00025274
02568                 MOVE AL-UNBOF       TO CCANDT1-ATTRB (WS-SUB2)    00025275
02569                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.         00025276
02570                                                                   00025277
02571                                                                   00025278
02572      IF CREFUND1-LEN (WS-SUB2) = ZEROS                            00025279
02573        AND PI-LAST-FUNC-DISPLAY                                   00025280
02574          NEXT SENTENCE                                            00025281
02575      ELSE                                                         00025282
02576          IF CREFUND1-LEN (WS-SUB2) NOT = ZEROS                    00025283
02577             MOVE AL-UNNON       TO CREFUND1-ATTRB (WS-SUB2)       00025284
02578             MOVE CREFUND1I (WS-SUB2) TO DEEDIT-FIELD-X11          00025285
02579             PERFORM 8600-DEEDIT                                   00025286
02580             MOVE DEEDIT-FIELD-V2  TO WS-CREFUND1 (WS-SUB2).       00025287
02581                                                                   00025288
02582      IF CCANDT2-LEN (WS-SUB2) = ZEROS                             00025289
02583        AND PI-LAST-FUNC-DISPLAY                                   00025290
02584          NEXT SENTENCE                                            00025291
02585      ELSE                                                         00025292
02586          IF CCANDT2-LEN (WS-SUB2) GREATER THAN ZEROS              00025293
02587              MOVE AL-UNNON       TO CCANDT2-ATTRB (WS-SUB2)       00025294
02588              IF PI-LAST-FUNC-DISPLAY AND                          00025295
02589                 CCANDT2 (WS-SUB2) = SPACES                        00025296
02590                  MOVE LOW-VALUES TO WS-CONVERTED-CANDT2 (WS-SUB2) 00025297
02591              ELSE                                                 00025298
02592                 IF CCANDT2 (WS-SUB2) NUMERIC                      00025299
02593                    MOVE 4              TO DC-OPTION-CODE          00025300
02594                    MOVE CCANDT2 (WS-SUB2) TO                      00025301
02595                                       DC-GREG-DATE-1-MDY          00025302
02596                    PERFORM 8500-DATE-CONVERT THRU 8500-EXIT       00025303
02597                    MOVE DC-BIN-DATE-1  TO WS-CONVERTED-CANDT2     00025304
02598                                                          (WS-SUB2)00025305
02599                    IF NO-CONVERSION-ERROR                         00025306
02600                       NEXT SENTENCE                               00025307
02601                    ELSE                                           00025308
02602                       MOVE -1       TO CCANDT2-LEN   (WS-SUB2)    00025309
02603                       MOVE ER-2227  TO EMI-ERROR                  00025310
02604                       MOVE AL-UNBON TO CCANDT2-ATTRB (WS-SUB2)    00025311
02605                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    00025312
02606                 ELSE                                              00025313
02607                    MOVE -1         TO CCANDT2-LEN   (WS-SUB2)     00025314
02608                    MOVE ER-2223    TO EMI-ERROR                   00025315
02609                    MOVE AL-UNBON   TO CCANDT2-ATTRB (WS-SUB2)     00025316
02610                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT       00025317
02611          ELSE                                                     00025318
02612              IF CREFUND2-LEN (WS-SUB2) GREATER THAN ZEROS         00025319
02613                 MOVE -1             TO CCANDT2-LEN   (WS-SUB2)    00025320
02614                 MOVE ER-2222        TO EMI-ERROR                  00025321
02615                 MOVE AL-UNBOF       TO CCANDT2-ATTRB (WS-SUB2)    00025322
02616                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.         00025323
02617                                                                   00025324
02618                                                                   00025325
02619      IF CREFUND2-LEN (WS-SUB2) = ZEROS                            00025326
02620        AND PI-LAST-FUNC-DISPLAY                                   00025327
02621          NEXT SENTENCE                                            00025328
02622      ELSE                                                         00025329
02623          IF CREFUND2-LEN (WS-SUB2) NOT = ZEROS                    00025330
02624             MOVE AL-UNNON       TO CREFUND2-ATTRB (WS-SUB2)       00025331
02625             MOVE CREFUND2I (WS-SUB2) TO DEEDIT-FIELD-X11          00025332
02626             PERFORM 8600-DEEDIT                                   00025333
02627             MOVE DEEDIT-FIELD-V2  TO WS-CREFUND2 (WS-SUB2).       00025334
02628                                                                   00025335
02629      IF CLIVES-LEN  (WS-SUB2)  GREATER THAN ZEROS                 00025336
02630          MOVE 'Y'                TO PI-CAN-LIVES-KEYED-SW         00025337
02631          MOVE CLIVESI (WS-SUB2 ) TO DEEDIT-FIELD                  00025338
02632          PERFORM 8600-DEEDIT                                      00025339
02633          IF DEEDIT-FIELD-V0 NUMERIC                               00025340
02634              MOVE DEEDIT-FIELD-V0 TO WS-CLIVES   (WS-SUB2)        00025341
02635              MOVE AL-UNNON       TO CLIVES-ATTRB (WS-SUB2)        00025342
02636          ELSE                                                     00025343
02637              MOVE -1             TO CLIVES-LEN   (WS-SUB2)        00025344
02638              MOVE AL-UNBON       TO CLIVES-ATTRB (WS-SUB2)        00025345
02639              MOVE ER-2223        TO EMI-ERROR                     00025346
02640              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            00025347
02641                                                                   00026410
02642      IF CPAYEE-LEN  (WS-SUB2)  GREATER THAN ZEROS                 00026411
02643         MOVE 'Y'                 TO PI-PAYEE-KEYED-SW.            00026412
02644                                                                   00026413
02645      IF CCHK-LEN    (WS-SUB2)  GREATER THAN ZEROS                 00026414
02646         MOVE 'Y'                 TO PI-CHK-REQ-KEYED-SW           00026415
02647         IF  CCHK    (WS-SUB2)  = 'R' OR ' '                       00026416
02648             NEXT SENTENCE                                         00026417
02649         ELSE                                                      00026418
02650             MOVE ER-7405         TO EMI-ERROR                     00026419
02651             MOVE -1              TO CCHK-LEN      (WS-SUB2)       00026420
02652             MOVE AL-UABON        TO CCHK-ATTRB    (WS-SUB2)       00026421
02653             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             00026422
02654                                                                   00026423
02655      IF PI-LAST-FUNC-DISPLAY                                      00026424
02656         NEXT SENTENCE                                             00026425
02657      ELSE                                                         00026426
02658         IF CCANDT1-LEN (WS-SUB2) GREATER THAN ZEROS OR            00026427
02659            CCANDT2-LEN (WS-SUB2) GREATER THAN ZEROS               00026428
02660            NEXT SENTENCE                                          00026429
02661         ELSE                                                      00026430
02662            MOVE ER-2222          TO EMI-ERROR                     00026431
02663            MOVE -1               TO CCANDT1-LEN   (WS-SUB2)       00026432
02664            MOVE AL-UNBOF         TO CCANDT1-ATTRB (WS-SUB2)       00026433
02665            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              00026434
02666                                                                   00026435
02667                                                                   00026436
02668      IF  EMI-ERROR = ZEROS                                        00026437
02669          MOVE 'Y'                TO PI-UPDATE-SW                  00026438
02670          MOVE SPACE              TO PI-DISPLAY-SW                 00026439
02671          PERFORM 5000-BUILD-CANCEL-RECORD THRU 5900-EXIT          00026440
02672      ELSE                                                         00026441
02673          MOVE ZEROS              TO EMI-ERROR                     00026442
02674          MOVE 'Y'                TO PI-ERROR-SW.                  00026443
02675                                                                   00026444
02676  1120-INCREMENT-OCCURANCE.                                        00026445
02677                                                                   00026446
02678      ADD +1                      TO WS-SUB2.                      00026447
02679                                                                   00026448
02680      IF WS-SUB2 GREATER THAN +2 OR PI-LAST-FUNC-DISPLAY           00026449
02681          NEXT SENTENCE                                            00026450
02682      ELSE                                                         00026451
02683          GO TO 1110-EDIT-MAPC-LOOP.                               00026452
02684                                                                   00026840
02685                                                                   00026850
02686  1130-NOTHING-TO-EDIT.                                            00026851
02687                                                                   00026852
02688                                                                   00026853
02689      IF PI-DATA-ERRORS                                            00026854
02690          MOVE AL-SABON           TO CSEQ-ATTRB (1) CSEQ-ATTRB (2) 00026855
02691                                     CSEQ-ATTRB (3) CSEQ-ATTRB (4) 00026856
02692         GO TO 8100-SEND-INITIAL-MAP.                              00026857
02693                                                                   00026858
02694      IF NOT PI-LAST-FUNC-DISPLAY                                  00026859
02695         AND WS-DATA-NOT-KEYED                                     00026860
02696         MOVE ER-7400             TO EMI-ERROR                     00026861
02697         MOVE -1                  TO CCANDT1-LEN (1)               00026862
02698         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  00026863
02699         MOVE 'Y'                 TO PI-ERROR-SW                   00026864
02700         GO TO 8200-SEND-DATAONLY.                                 00026865
02701                                                                   00026866
02702      IF PI-SAV-BATCH-SEQ LESS THAN PI-LAST-SEQ-NO-ADDED           00026867
02703         SUBTRACT 1 FROM PI-SAV-BATCH-SEQ                          00026868
02704         MOVE ER-0000            TO EMI-ERROR                      00026869
02705         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  00026870
02706         GO TO 2000-BROWSE-FWD.                                    00026871
02707                                                                   00026872
02708      MOVE LOW-VALUES     TO DATA-ENTRY-MAP.                       00026873
02709                                                                   00027090
02710      ADD +1, PI-LAST-SEQ-NO-ADDED                                 00027091
02711                             GIVING PI-NEXT-DISPLAY-SEQ-NO.        00027092
02712                                                                   00027093
02713      MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ.             00027094
02714                                                                   00027095
02715      PERFORM 8550-SET-MAP-SEQ-NOS                                 00027096
02716                   VARYING WS-SUB2 FROM +1 BY +1                   00027097
02717                   UNTIL WS-SUB2 GREATER THAN +2.                  00027098
02718                                                                   00027099
02719      MOVE ER-0000            TO EMI-ERROR.                        00027100
02720      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00027101
02721      GO TO 8100-SEND-INITIAL-MAP.                                 00027102
02722                                                                   00027103
02723      EJECT                                                        00027104
02724                                                                   00027105
02725 ******************************************************************00027106
02726 *                                                                *00027107
02727 *           V E R I F Y   C A R R I E R   I D                    *00027108
02728 *                                                                *00027109
02729 ******************************************************************00027110
02730                                                                   00027111
02731  1500-VERIFY-CARRIER-ID.                                          00027112
02732                                                                   00027113
02733      MOVE SPACES                 TO ELCNTL-KEY.                   00027114
02734      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            00027115
02735      MOVE '6'                    TO ELCNTL-REC-TYPE.              00027116
02736                                                                   00027117
02737      IF PI-MAP-NAME = EL930B                                      00027118
02738         MOVE BCAR                TO ELCNTL-CARRIER                00027119
02739      ELSE                                                         00027120
02740         MOVE CCAR (WS-SUB2)      TO ELCNTL-CARRIER.               00027121
02741                                                                   00027122
02742      MOVE +0                     TO ELCNTL-SEQ.                   00027123
02743                                                                   00027124
02744      EXEC CICS HANDLE CONDITION                                   00027125
02745          NOTFND   (1580-NO-CARRIER)                               00027126
02746          END-EXEC.                                                00027127
02747                                                                   00027128
02748      EXEC CICS READ                                               00027129
02749          DATASET   (FILE-ID-ELCNTL)                               00027130
02750          SET       (ELCNTL-POINTER)                               00027131
02751          RIDFLD    (ELCNTL-KEY)                                   00027132
02752          END-EXEC.                                                00027133
02753      SERVICE RELOAD CONTROL-FILE.                                 00027134
02754      GO TO 1590-EXIT.                                             00027135
02755                                                                   00027136
02756  1580-NO-CARRIER.                                                 00027137
02757                                                                   00027138
02758      IF PI-MAP-NAME = EL930B                                      00027139
02759         MOVE -1                  TO BCARL                         00027140
02760         MOVE AL-UABON            TO BCARA                         00027141
02761      ELSE                                                         00027142
02762         MOVE AL-UABON            TO CCAR-ATTRB (WS-SUB2)          00027143
02763         MOVE -1                  TO CCAR-LEN   (WS-SUB2).         00027144
02764                                                                   00027640
02765      MOVE ER-2208                TO EMI-ERROR.                    00027641
02766      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00027642
02767                                                                   00027670
02768  1590-EXIT.                                                       00027671
02769      EXIT.                                                        00027672
02770                                                                   00027673
02771      EJECT                                                        00027674
02772                                                                   00027675
02773 ******************************************************************00027676
02774 *                                                                *00027677
02775 *               V E R I F Y   S T A T E  I D                     *00027678
02776 *                                                                *00027679
02777 ******************************************************************00027680
02778                                                                   00027681
02779  1600-VERIFY-STATE-ID.                                            00027682
02780                                                                   00027683
02781      MOVE SPACES                 TO ELCNTL-KEY.                   00027684
02782      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            00027685
02783      MOVE '3'                    TO ELCNTL-REC-TYPE.              00027686
02784                                                                   00027687
02785      IF PI-MAP-NAME = EL930B                                      00027688
02786         MOVE BST                 TO ELCNTL-STATE                  00027689
02787      ELSE                                                         00027690
02788         MOVE CST  (WS-SUB2)      TO ELCNTL-STATE.                 00027691
02789                                                                   00027692
02790      MOVE +0                     TO ELCNTL-SEQ.                   00027693
02791                                                                   00027694
02792      EXEC CICS HANDLE CONDITION                                   00027695
02793          NOTFND   (1680-NO-STATE)                                 00027696
02794          END-EXEC.                                                00027697
02795                                                                   00027698
02796      EXEC CICS READ                                               00027699
02797          DATASET   (FILE-ID-ELCNTL)                               00027700
02798          SET       (ELCNTL-POINTER)                               00027701
02799          RIDFLD    (ELCNTL-KEY)                                   00027702
02800          END-EXEC.                                                00027703
02801                                                                   00027704
02802      SERVICE RELOAD CONTROL-FILE.                                 00027705
02803                                                                   00027706
02804      GO TO 1690-EXIT.                                             00027707
02805                                                                   00027708
02806  1680-NO-STATE.                                                   00027709
02807                                                                   00027710
02808      IF PI-MAP-NAME = EL930B                                      00027711
02809         MOVE -1                  TO BST-LEN                       00027712
02810         MOVE AL-UABON            TO BST-ATTRB                     00027713
02811      ELSE                                                         00027714
02812         MOVE AL-UABON            TO CST-LEN (WS-SUB2)             00027715
02813         MOVE -1                  TO CST-LEN (WS-SUB2).            00027716
02814                                                                   00028140
02815      MOVE ER-2209                TO EMI-ERROR.                    00028141
02816      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00028142
02817                                                                   00028143
02818  1690-EXIT.                                                       00028144
02819                                                                   00028145
02820      EXIT.                                                        00028146
02821                                                                   00028147
02822      EJECT                                                        00028148
02823                                                                   00028149
02824 ******************************************************************00028150
02825 *                                                                *00028151
02826 *               V E R I F Y   A C C O U N T                      *00028152
02827 *                                                                *00028153
02828 ******************************************************************00028154
02829                                                                   00028155
02830  1700-VERIFY-ACCOUNT.                                             00028156
02831                                                                   00028310
02832      IF PI-MAP-NAME NOT = EL930B                                  00028311
02833         GO TO 1710-VERIFY-CANCEL-ACCOUNT.                         00028312
02834                                                                   00028313
02835      IF BCAR-LEN GREATER THAN ZEROS                               00028314
02836         MOVE BCAR                TO ERACCT-CARRIER                00028315
02837      ELSE                                                         00028316
02838          MOVE SPACES             TO ERACCT-CARRIER.               00028317
02839                                                                   00028318
02840      IF BGRP-LEN GREATER THAN ZEROS                               00028319
02841          MOVE BGRP               TO ERACCT-GROUPING               00028320
02842      ELSE                                                         00028321
02843          MOVE SPACES             TO ERACCT-GROUPING.              00028322
02844                                                                   00028323
02845      IF BST-LEN GREATER THAN ZEROS                                00028324
02846          MOVE BST                TO ERACCT-STATE                  00028325
02847      ELSE                                                         00028326
02848          MOVE SPACES             TO ERACCT-STATE.                 00028327
02849                                                                   00028328
02850      MOVE BACCT                  TO ERACCT-ACCOUNT.               00028329
02851                                                                   00028330
02852      GO TO 1720-READ-ACCOUNT.                                     00028331
02853                                                                   00028332
02854  1710-VERIFY-CANCEL-ACCOUNT.                                      00028333
02855                                                                   00028334
02856      IF CCAR-LEN  (WS-SUB2) GREATER THAN ZEROS                    00028335
02857         MOVE CCAR (WS-SUB2)      TO ERACCT-CARRIER                00028336
02858      ELSE                                                         00028337
02859          MOVE SPACES             TO ERACCT-CARRIER.               00028338
02860                                                                   00028339
02861      IF CGRP-LEN   (WS-SUB2) GREATER THAN ZEROS                   00028340
02862          MOVE CGRP (WS-SUB2)     TO ERACCT-GROUPING               00028341
02863      ELSE                                                         00028342
02864          MOVE SPACES             TO ERACCT-GROUPING.              00028343
02865                                                                   00028344
02866      IF CST-LEN    (WS-SUB2) GREATER THAN ZEROS                   00028345
02867          MOVE CST  (WS-SUB2)     TO ERACCT-STATE                  00028346
02868      ELSE                                                         00028347
02869          MOVE SPACES             TO ERACCT-STATE.                 00028348
02870                                                                   00028700
02871      MOVE CACCT (WS-SUB2)        TO ERACCT-ACCOUNT.               00028701
02872                                                                   00028720
02873                                                                   00028721
02874  1720-READ-ACCOUNT.                                               00028722
02875                                                                   00028723
02876      MOVE PI-COMPANY-CD          TO ERACCT-CO.                    00028724
02877      MOVE '0'                    TO WS-FIRST-TIME-SW.             00028725
02878      MOVE LOW-VALUES             TO ERACCT-EXP-DATE               00028726
02879                                     PI-ACCT-LOW-EFF-DT            00028727
02880                                     PI-ACCT-HIGH-EXP-DT.          00028728
02881                                                                   00028729
02882      EXEC CICS HANDLE CONDITION                                   00028730
02883          NOTFND   (1780-ACCOUNT-INVALID)                          00028731
02884          ENDFILE  (1780-ACCOUNT-INVALID)                          00028732
02885          END-EXEC.                                                00028733
02886                                                                   00028734
02887      EXEC CICS STARTBR                                            00028735
02888          DATASET (FILE-ID-ERACCT2)                                00028736
02889          RIDFLD  (ERACCT-KEY)                                     00028737
02890          END-EXEC.                                                00028738
02891                                                                   00028739
02892      MOVE 'Y'                    TO WS-BROWSE-SW.                 00028740
02893                                                                   00028741
02894      MOVE ERACCT-KEY             TO ERACCT-SAVE-KEY.              00028742
02895                                                                   00028743
02896  1750-READ-LOOP.                                                  00028744
02897                                                                   00028745
02898      EXEC CICS READNEXT                                           00028746
02899          DATASET   (FILE-ID-ERACCT2)                              00028747
02900          SET       (ERACCT-POINTER)                               00028748
02901          RIDFLD    (ERACCT-KEY)                                   00028749
02902          END-EXEC.                                                00028750
02903                                                                   00028751
02904      SERVICE RELOAD ACCOUNT-MASTER.                               00028752
02905                                                                   00029050
02906      IF ERACCT-COMP-KEY NOT = ERACCT-SAVE-KEY                     00029051
02907          GO TO 1780-ACCOUNT-INVALID.                              00029052
02908                                                                   00029053
02909      IF FIRST-TIME                                                00029054
02910          MOVE '1'                TO WS-FIRST-TIME-SW              00029055
02911          MOVE AM-VG-CARRIER      TO PI-SAV-CARRIER                00029056
02912          MOVE AM-VG-GROUPING     TO PI-SAV-GROUPING               00029057
02913          MOVE AM-VG-STATE        TO PI-SAV-STATE                  00029058
02914          MOVE AM-VG-ACCOUNT      TO PI-SAV-ACCOUNT                00029059
02915          MOVE AM-EFFECTIVE-DT    TO PI-ACCT-LOW-EFF-DT.           00029060
02916                                                                   00029061
02917      MOVE AM-EXPIRATION-DT       TO PI-ACCT-HIGH-EXP-DT.          00029062
02918                                                                   00029063
02919      GO TO 1750-READ-LOOP.                                        00029064
02920                                                                   00029065
02921  1780-ACCOUNT-INVALID.                                            00029066
02922                                                                   00029067
02923      IF BROWSE-STARTED                                            00029068
02924         MOVE SPACE  TO WS-BROWSE-SW                               00029069
02925         EXEC CICS ENDBR                                           00029070
02926              DATASET (FILE-ID-ERACCT2)                            00029071
02927              END-EXEC.                                            00029072
02928                                                                   00029280
02929      IF PI-ACCT-LOW-EFF-DT NOT = LOW-VALUES                       00029281
02930         GO TO 1790-EXIT.                                          00029282
02931                                                                   00029310
02932      IF PI-MAP-NAME NOT = EL930B                                  00029311
02933         GO TO 1785-SET-CANCEL-CONTROL.                            00029312
02934                                                                   00029313
02935      IF CARR-GROUP-ST-ACCNT-CNTL                                  00029314
02936          MOVE -1                     TO BCAR-LEN                  00029315
02937          MOVE AL-UABON               TO BCAR-ATTRB                00029316
02938                                         BGRP-ATTRB                00029317
02939                                         BST-ATTRB                 00029318
02940                                         BACCT-ATTRB               00029319
02941      ELSE                                                         00029320
02942          IF ST-ACCNT-CNTL                                         00029321
02943              MOVE -1                 TO BST-LEN                   00029322
02944              MOVE AL-UABON           TO BST-ATTRB                 00029323
02945                                         BACCT-ATTRB               00029324
02946          ELSE                                                     00029325
02947              IF CARR-ST-ACCNT-CNTL                                00029326
02948                  MOVE -1             TO BCAR-LEN                  00029327
02949                  MOVE AL-UABON       TO BCAR-ATTRB                00029328
02950                                         BST-ATTRB                 00029329
02951                                         BACCT-ATTRB               00029330
02952              ELSE                                                 00029331
02953                  IF ACCNT-CNTL                                    00029332
02954                      MOVE -1         TO BACCT-LEN                 00029333
02955                      MOVE AL-UABON   TO BACCT-ATTRB               00029334
02956                  ELSE                                             00029335
02957                      MOVE -1         TO BCAR-LEN                  00029336
02958                      MOVE AL-UABON   TO BCAR-ATTRB                00029337
02959                                         BACCT-ATTRB.              00029338
02960                                                                   00029339
02961      MOVE ER-2210                TO EMI-ERROR.                    00029340
02962                                                                   00029341
02963      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00029342
02964                                                                   00029343
02965      GO TO 8200-SEND-DATAONLY.                                    00029344
02966                                                                   00029345
02967  1785-SET-CANCEL-CONTROL.                                         00029346
02968                                                                   00029347
02969      IF CARR-GROUP-ST-ACCNT-CNTL                                  00029348
02970          MOVE -1                     TO CCAR-LEN       (WS-SUB2)  00029349
02971          MOVE AL-UABON               TO CCAR-ATTRB     (WS-SUB2)  00029350
02972                                         CGRP-ATTRB     (WS-SUB2)  00029351
02973                                         CST-ATTRB      (WS-SUB2)  00029352
02974                                         CACCT-ATTRB (WS-SUB2)     00029353
02975      ELSE                                                         00029354
02976          IF ST-ACCNT-CNTL                                         00029355
02977              MOVE -1                 TO CST-LEN        (WS-SUB2)  00029356
02978              MOVE AL-UABON           TO CST-ATTRB      (WS-SUB2)  00029357
02979                                         CACCT-ATTRB (WS-SUB2)     00029358
02980          ELSE                                                     00029359
02981              IF CARR-ST-ACCNT-CNTL                                00029360
02982                  MOVE -1             TO CCAR-LEN       (WS-SUB2)  00029361
02983                  MOVE AL-UABON       TO CCAR-ATTRB     (WS-SUB2)  00029362
02984                                         CST-ATTRB      (WS-SUB2)  00029363
02985                                         CACCT-ATTRB (WS-SUB2)     00029364
02986              ELSE                                                 00029365
02987                  IF ACCNT-CNTL                                    00029366
02988                      MOVE -1         TO CACCT-LEN      (WS-SUB2)  00029367
02989                      MOVE AL-UABON   TO CACCT-ATTRB (WS-SUB2)     00029368
02990                  ELSE                                             00029369
02991                      MOVE -1         TO CCAR-LEN     (WS-SUB2)    00029370
02992                      MOVE AL-UABON   TO CCAR-ATTRB     (WS-SUB2)  00029371
02993                                         CACCT-ATTRB (WS-SUB2).    00029372
02994                                                                   00029373
02995      MOVE ER-2210                TO EMI-ERROR.                    00029374
02996                                                                   00029375
02997      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00029376
02998                                                                   00029377
02999      GO TO 8200-SEND-DATAONLY.                                    00029378
03000                                                                   00029379
03001  1790-EXIT.                                                       00029380
03002                                                                   00029381
03003      EXIT.                                                        00029382
03004                                                                   00029383
03005      EJECT                                                        00029384
03006                                                                   00029385
03007 ******************************************************************00029386
03008 *                                                                *00029387
03009 *                   B R O W S E   F O R W A R D                  *00029388
03010 *                                                                *00029389
03011 ******************************************************************00029390
03012                                                                   00029391
03013  2000-BROWSE-FWD.                                                 00029392
03014                                                                   00029393
03015      MOVE LOW-VALUES             TO DATA-ENTRY-MAP.               00029394
03016                                                                   00029395
03017      ADD +1                      TO PI-SAV-BATCH-SEQ.             00029396
03018                                                                   00029397
03019      EXEC CICS HANDLE CONDITION                                   00029398
03020          NOTFND (2020-END-FILE)                                   00029399
03021          END-EXEC.                                                00029400
03022                                                                   00029401
03023      EXEC CICS READ                                               00029402
03024          SET     (ERPNDT-POINTER)                                 00029403
03025          DATASET (FILE-ID-ERPNDT)                                 00029404
03026          RIDFLD  (PI-SAV-ENDING-ERPNDT-KEY)                       00029405
03027          GTEQ                                                     00029406
03028          END-EXEC.                                                00029407
03029                                                                   00029408
03030      SERVICE RELOAD PENDING-BUSINESS.                             00029409
03031                                                                   00029410
03032      IF PB-COMPANY-CD = PI-SAV-COMP-CD                            00029411
03033        AND PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                    00029412
03034          NEXT SENTENCE                                            00029413
03035      ELSE                                                         00029414
03036          GO TO 2020-END-FILE.                                     00030360
03037                                                                   00030370
03038      IF PB-BATCH-TRAILER                                          00030371
03039          GO TO 2020-END-FILE.                                     00030372
03040                                                                   00030373
03041      MOVE PB-BATCH-SEQ-NO        TO PI-SAV-BATCH-SEQ.             00030374
03042                                                                   00030375
03043      IF PB-ISSUE                                                  00030376
03044          MOVE EL930B             TO PI-MAP-NAME                   00030377
03045          MOVE AL-SANOF           TO BDELHDGA                      00030378
03046          PERFORM 7000-FORMAT-ISSUE-SCREEN THRU 7090-EXIT          00030379
03047      ELSE                                                         00030380
03048          MOVE EL930C             TO PI-MAP-NAME                   00030381
03049          MOVE AL-SANOF           TO CDELHDGA                      00030382
03050          PERFORM 7100-FORMAT-CANCEL-SCREEN THRU 7190-EXIT.        00030383
03051                                                                   00030384
03052  2010-SEND-MAP.                                                   00030385
03053                                                                   00030386
03054      GO TO 8100-SEND-INITIAL-MAP.                                 00030387
03055                                                                   00030388
03056  2020-END-FILE.                                                   00030389
03057                                                                   00030390
03058      MOVE SPACE                  TO PI-DISPLAY-SW.                00030391
03059                                                                   00030392
03060      IF PI-MAP-NAME = EL930B                                      00030393
03061          MOVE LOW-VALUES         TO DATA-ENTRY-MAP                00030394
03062          MOVE -1                 TO BPFENTRL                      00030395
03063          ADD +1, PI-LAST-SEQ-NO-ADDED                             00030396
03064                GIVING PI-NEXT-DISPLAY-SEQ-NO                      00030397
03065          PERFORM 8550-SET-MAP-SEQ-NOS                             00030398
03066      ELSE                                                         00030399
03067          MOVE LOW-VALUES         TO DATA-ENTRY-MAP                00030400
03068          MOVE -1                 TO CPFENTRL                      00030401
03069          ADD +1, PI-LAST-SEQ-NO-ADDED                             00030402
03070                GIVING PI-NEXT-DISPLAY-SEQ-NO                      00030403
03071          PERFORM 8550-SET-MAP-SEQ-NOS                             00030404
03072                  VARYING WS-SUB2 FROM +1 BY +1                    00030405
03073                  UNTIL WS-SUB2 GREATER THAN +2.                   00030406
03074                                                                   00030407
03075      MOVE ER-2217                TO EMI-ERROR.                    00030408
03076      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00030409
03077      GO TO 2010-SEND-MAP.                                         00030410
03078                                                                   00030411
03079      EJECT                                                        00030412
03080                                                                   00030413
03081 ******************************************************************00030414
03082 *                                                                *00030415
03083 *                B R O W S E   B A C K W A R D                   *00030416
03084 *                                                                *00030417
03085 ******************************************************************00030418
03086                                                                   00030419
03087  2100-BROWSE-BKWD.                                                00030420
03088                                                                   00030421
03089      MOVE LOW-VALUES             TO DATA-ENTRY-MAP.               00030422
03090                                                                   00030423
03091      SUBTRACT +1             FROM PI-SAV-BATCH-SEQ.               00030424
03092                                                                   00030920
03093      IF PI-SAV-BATCH-SEQ NOT GREATER THAN +0                      00030921
03094          GO TO 2120-END-FILE.                                     00030922
03095                                                                   00030923
03096      EXEC CICS HANDLE CONDITION                                   00030924
03097          NOTFND (2100-BROWSE-BKWD)                                00030925
03098          END-EXEC.                                                00030926
03099                                                                   00030927
03100      EXEC CICS READ                                               00030928
03101          SET     (ERPNDT-POINTER)                                 00030929
03102          DATASET (FILE-ID-ERPNDT)                                 00030930
03103          RIDFLD  (PI-SAV-ENDING-ERPNDT-KEY)                       00030931
03104          END-EXEC.                                                00030932
03105                                                                   00030933
03106      SERVICE RELOAD PENDING-BUSINESS.                             00030934
03107                                                                   00030935
03108      IF PB-COMPANY-CD     = PI-SAV-COMP-CD                        00030936
03109        AND PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                    00030937
03110          NEXT SENTENCE                                            00030938
03111      ELSE                                                         00030939
03112          GO TO 2120-END-FILE.                                     00031120
03113                                                                   00031130
03114      IF PB-BATCH-TRAILER                                          00031131
03115          GO TO 2120-END-FILE.                                     00031132
03116                                                                   00031133
03117      IF PB-ISSUE                                                  00031134
03118          MOVE EL930B             TO PI-MAP-NAME                   00031135
03119          MOVE AL-SANOF           TO BDELHDGA                      00031136
03120          PERFORM 7000-FORMAT-ISSUE-SCREEN THRU 7090-EXIT          00031137
03121      ELSE                                                         00031138
03122          MOVE EL930C             TO PI-MAP-NAME                   00031139
03123          MOVE AL-SANOF           TO CDELHDGA                      00031140
03124          PERFORM 7100-FORMAT-CANCEL-SCREEN THRU 7190-EXIT.        00031141
03125                                                                   00031250
03126  2110-SEND-MAP.                                                   00031251
03127      GO TO 8100-SEND-INITIAL-MAP.                                 00031252
03128                                                                   00031253
03129  2120-END-FILE.                                                   00031254
03130                                                                   00031255
03131      MOVE ER-2431                TO EMI-ERROR.                    00031256
03132      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00031257
03133      MOVE ER-0000                TO EMI-ERROR.                    00031258
03134      MOVE ZEROS                  TO PI-SAV-BATCH-SEQ.             00031259
03135      GO TO 2000-BROWSE-FWD.                                       00031260
03136                                                                   00031261
03137      EJECT                                                        00031262
03138                                                                   00031263
03139 ******************************************************************00031264
03140 *                                                                *00031265
03141 *            C O N T I U E   B A T C H   E N T R Y               *00031266
03142 *                                                                *00031267
03143 ******************************************************************00031268
03144                                                                   00031269
03145  3000-CONTINUE-ENTRY.                                             00031270
03146                                                                   00031271
03147      MOVE PI-SAV-ENDING-ERPNDT-KEY TO ERPNDT-KEY.                 00031272
03148                                                                   00031273
03149      EXEC CICS HANDLE CONDITION                                   00031274
03150          NOTFND (3300-REC-NOT-FND)                                00031275
03151          END-EXEC.                                                00031276
03152                                                                   00031277
03153      EXEC CICS STARTBR                                            00031278
03154          DATASET (FILE-ID-ERPNDT)                                 00031279
03155          RIDFLD  (ERPNDT-KEY)                                     00031280
03156          GTEQ                                                     00031281
03157          END-EXEC.                                                00031282
03158                                                                   00031283
03159  3100-READ-LOOP.                                                  00031284
03160                                                                   00031285
03161      EXEC CICS HANDLE CONDITION                                   00031286
03162          ENDFILE (3200-END-BROWSE)                                00031287
03163          END-EXEC.                                                00031288
03164                                                                   00031289
03165      EXEC CICS READNEXT                                           00031290
03166          SET     (ERPNDT-POINTER)                                 00031291
03167          DATASET (FILE-ID-ERPNDT)                                 00031292
03168          RIDFLD  (ERPNDT-KEY)                                     00031293
03169          END-EXEC.                                                00031294
03170                                                                   00031295
03171      SERVICE RELOAD PENDING-BUSINESS.                             00031296
03172                                                                   00031297
03173      IF PB-COMPANY-CD     = PI-SAV-COMP-CD                        00031298
03174        AND PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH                    00031299
03175          NEXT SENTENCE                                            00031300
03176      ELSE                                                         00031301
03177          GO TO 3200-END-BROWSE.                                   00031302
03178                                                                   00031780
03179      IF NOT PB-BATCH-TRAILER                                      00031781
03180          GO TO 3110-NOT-BATCH-TRAILER.                            00031782
03181                                                                   00031783
03182  3105-PRIME-PI-COUNTS.                                            00031784
03183                                                                   00031830
03184      IF PI-LF-ISS-REMITTED = ZEROS                                00031831
03185          MOVE PB-B-LF-ISS-PRM-REMITTED  TO PI-LF-ISS-REMITTED.    00031832
03186                                                                   00031860
03187      IF PI-AH-ISS-REMITTED = ZEROS                                00031861
03188          MOVE PB-B-AH-ISS-PRM-REMITTED  TO PI-AH-ISS-REMITTED.    00031862
03189                                                                   00031890
03190      IF PI-ISS-CNT-REMITTED = ZEROS                               00031891
03191          MOVE PB-B-ISSUE-CNT-REMITTED   TO PI-ISS-CNT-REMITTED.   00031892
03192                                                                   00031920
03193      IF PI-CAN-CNT-REMITTED = ZEROS                               00031921
03194          MOVE PB-B-CANCEL-CNT-REMITTED  TO PI-CAN-CNT-REMITTED.   00031922
03195                                                                   00031950
03196      IF PI-LF-CAN-REMITTED = ZEROS                                00031951
03197          MOVE PB-B-LF-CAN-PRM-REMITTED  TO PI-LF-CAN-REMITTED.    00031952
03198                                                                   00031980
03199      IF PI-AH-CAN-REMITTED = ZEROS                                00031981
03200          MOVE PB-B-AH-CAN-PRM-REMITTED  TO PI-AH-CAN-REMITTED.    00031982
03201                                                                   00031983
03202      GO TO 3200-END-BROWSE.                                       00031984
03203                                                                   00031985
03204  3110-NOT-BATCH-TRAILER.                                          00031986
03205                                                                   00031987
03206      IF PB-ISSUE                                                  00031988
03207          ADD PB-I-LF-PREMIUM-AMT     TO PI-LF-ISS-ENTERED         00031989
03208          ADD PB-I-LF-ALT-PREMIUM-AMT TO PI-LF-ISS-ENTERED         00031990
03209          ADD PB-I-AH-PREMIUM-AMT     TO PI-AH-ISS-ENTERED         00031991
03210          ADD +1                      TO PI-ISS-CNT-ENTERED        00031992
03211      ELSE                                                         00031993
03212          ADD PB-C-LF-CANCEL-AMT      TO PI-LF-CAN-ENTERED         00031994
03213          ADD PB-C-AH-CANCEL-AMT      TO PI-AH-CAN-ENTERED         00031995
03214          ADD +1                      TO PI-CAN-CNT-ENTERED.       00031996
03215      MOVE PB-BATCH-SEQ-NO            TO PI-LAST-SEQ-NO-ADDED      00031997
03216                                         PI-SAV-BATCH-SEQ.         00031998
03217      GO TO 3100-READ-LOOP.                                        00031999
03218                                                                   00032000
03219  3200-END-BROWSE.                                                 00032001
03220                                                                   00032002
03221      EXEC CICS ENDBR                                              00032003
03222          DATASET (FILE-ID-ERPNDT)                                 00032004
03223          END-EXEC.                                                00032005
03224                                                                   00032240
03225      ADD +1, PI-LAST-SEQ-NO-ADDED                                 00032241
03226                             GIVING PI-NEXT-DISPLAY-SEQ-NO.        00032242
03227                                                                   00032243
03228      IF PI-MAINT-FUNC = 'B'                                       00032244
03229          MOVE ZEROS              TO PI-SAV-BATCH-SEQ              00032245
03230          GO TO 2000-BROWSE-FWD                                    00032246
03231      ELSE                                                         00032247
03232          ADD +1                  TO PI-SAV-BATCH-SEQ.             00032248
03233                                                                   00032249
03234      IF PI-LF-ISS-REMITTED = ZEROS                                00032250
03235        AND PI-AH-ISS-REMITTED = ZEROS                             00032251
03236        AND PI-LF-CAN-REMITTED = ZEROS                             00032252
03237        AND PI-AH-CAN-REMITTED = ZEROS                             00032253
03238        AND PI-ISS-CNT-REMITTED = ZEROS                            00032254
03239        AND PI-CAN-CNT-REMITTED = ZEROS                            00032255
03240          MOVE  EL930B            TO PI-MAP-NAME                   00032256
03241      ELSE                                                         00032257
03242          IF PI-LF-ISS-REMITTED = ZEROS                            00032258
03243            AND PI-AH-ISS-REMITTED = ZEROS                         00032259
03244            AND PI-ISS-CNT-REMITTED = ZEROS                        00032260
03245              MOVE  EL930C        TO PI-MAP-NAME                   00032261
03246          ELSE                                                     00032262
03247              MOVE  EL930B        TO PI-MAP-NAME.                  00032263
03248                                                                   00032264
03249      IF PI-MAP-NAME = EL930B                                      00032265
03250          PERFORM 8550-SET-MAP-SEQ-NOS                             00032266
03251      ELSE                                                         00032267
03252          PERFORM 8550-SET-MAP-SEQ-NOS                             00032268
03253                  VARYING WS-SUB2 FROM +1 BY +1                    00032269
03254                  UNTIL WS-SUB2 GREATER THAN +2.                   00032270
03255                                                                   00032271
03256      GO TO 8100-SEND-INITIAL-MAP.                                 00032272
03257                                                                   00032273
03258  3300-REC-NOT-FND.                                                00032274
03259                                                                   00032275
03260      MOVE ER-2212                TO EMI-ERROR.                    00032276
03261      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00032277
03262      GO TO 8100-SEND-INITIAL-MAP.                                 00032278
03263                                                                   00032279
03264      EJECT                                                        00032280
03265                                                                   00032281
03266 ******************************************************************00032282
03267 *                                                                *00032283
03268 *            B U I L D   I S S U E   R E C O R D                 *00032284
03269 *                                                                *00032285
03270 ******************************************************************00032286
03271                                                                   00032287
03272  4000-BUILD-ISSUE-RECORD.                                         00032288
03273                                                                   00032730
03274      IF BSEQ   GREATER THAN PI-LAST-SEQ-NO-ADDED                  00032731
03275          GO TO 4100-ADD-ISSUE-RECORD.                             00032732
03276                                                                   00032733
03277 ******************************************************************00032734
03278 *                                                                *00032735
03279 *   THE DATA ENTRY SYSTEM ALLOWS BROWSING OF THE CURRENT BUS.    *00032736
03280 *   FILE. THE DATA ENTRY SYS. DOES NOT HAVE A MAINT. FUNCTION.   *00032737
03281 *   THE PROGRAM ASSUMES THAT IF A MATCH ON THE READ FOR UPDATE   *00032738
03282 *   IS SUCCESSFUL, THE RECORD HAS PREVIOUSLY BEEN DISPLAYED      *00032739
03283 *   THRUOUGH A BROWSE.  CHANGES ARE APPLIED AND THE PB-RECORD IS *00032740
03284 *   REWRITTEN, ELSE A NEW PENDING BUS. RECORD IS ADDED.          *00032741
03285 *                                                                *00032742
03286 ******************************************************************00032743
03287                                                                   00032744
03288      MOVE PI-COMPANY-CD          TO ERPNDT-COMP-CD.               00032745
03289      MOVE PI-SAV-ENTRY-BATCH     TO ERPNDT-ENTRY-BATCH.           00032746
03290      MOVE BSEQ                   TO ERPNDT-BATCH-SEQ.             00032747
03291                                                                   00032748
03292      EXEC CICS HANDLE CONDITION                                   00032749
03293          NOTFND (4100-ADD-ISSUE-RECORD)                           00032750
03294          END-EXEC.                                                00032751
03295                                                                   00032752
03296      EXEC CICS READ                                               00032753
03297          SET     (ERPNDT-POINTER)                                 00032754
03298          DATASET (FILE-ID-ERPNDT)                                 00032755
03299          RIDFLD  (ERPNDT-KEY)                                     00032756
03300          UPDATE                                                   00032757
03301          END-EXEC.                                                00032758
03302                                                                   00032759
03303      SERVICE RELOAD PENDING-BUSINESS.                             00032760
03304                                                                   00032761
03305      MOVE PB-CONTROL-PRIMARY     TO ERPNDM-KEY.                   00032762
03306                                                                   00032763
03307      MOVE 'B'                    TO JP-RECORD-TYPE                00032764
03308      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00032765
03309      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     00032766
03310      MOVE FILE-ID-ERPNDT         TO JP-FILE-ID.                   00032767
03311      PERFORM 8400-LOG-JOURNAL-RECORD.                             00032768
03312                                                                   00032769
03313      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             00032770
03314      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         00032771
03315      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             00032772
03316                                                                   00033160
03317      IF BLAST-NAME-LEN      GREATER THAN ZEROS                    00033161
03318          MOVE BLAST-NAME         TO PB-I-INSURED-LAST-NAME.       00033162
03319                                                                   00033190
03320      IF B1ST-NAME-LEN       GREATER THAN ZEROS                    00033191
03321          MOVE B1ST-NAME          TO PB-I-INSURED-FIRST-NAME.      00033192
03322                                                                   00033220
03323      IF BINIT-LEN           GREATER THAN ZEROS                    00033221
03324          MOVE BINIT              TO PB-I-INSURED-MIDDLE-INIT.     00033222
03325                                                                   00033250
03326      IF BAGE-LEN            GREATER THAN ZEROS                    00033251
03327          MOVE WS-BAGE            TO PB-I-AGE.                     00033252
03328                                                                   00033280
03329      IF BJNT-AGE-LEN        GREATER THAN ZEROS                    00033281
03330          MOVE WS-BJNT-AGE        TO PB-I-JOINT-AGE.               00033282
03331                                                                   00033310
03332      IF BBIRTH-LEN          GREATER THAN ZEROS                    00033311
03333          MOVE WS-CONVERTED-BIRTH TO PB-I-BIRTHDAY.                00033312
03334                                                                   00033340
03335      IF BSEX-LEN            GREATER THAN ZEROS                    00033341
03336          MOVE BSEX               TO PB-I-INSURED-SEX.             00033342
03337                                                                   00033370
03338      IF BTERM-LEN  (1)      GREATER THAN ZEROS                    00033371
03339         MOVE WS-BTERM (1)        TO PB-I-LF-TERM.                 00033372
03340                                                                   00033400
03341      IF BTERM-LEN  (2)      GREATER THAN ZEROS                    00033401
03342         MOVE WS-BTERM (2)         TO PB-I-AH-TERM.                00033402
03343                                                                   00033430
03344      IF BLN-TERM-LEN        GREATER THAN ZEROS                    00033431
03345          MOVE WS-BLN-TERM        TO PB-I-LOAN-TERM.               00033432
03346                                                                   00033460
03347      IF BFREQ-LEN           GREATER THAN ZEROS                    00033461
03348          MOVE WS-BFREQ           TO PB-I-PAY-FREQUENCY.           00033462
03349                                                                   00033490
03350      IF BSKPCD-LEN          GREATER THAN ZEROS                    00033491
03351          MOVE BSKPCD             TO PB-I-SKIP-CODE.               00033492
03352                                                                   00033520
03353      IF BMODE-LEN           GREATER THAN ZEROS                    00033521
03354          MOVE BMODE              TO PB-I-TERM-TYPE.               00033522
03355                                                                   00033550
03356      IF BPMTS-LEN           GREATER THAN ZEROS                    00033551
03357          MOVE WS-BPMTS           TO PB-I-NO-OF-PAYMENTS.          00033552
03358                                                                   00033580
03359      IF BPOLICY-LEN         GREATER THAN ZEROS                    00033581
03360          MOVE BPOLICY            TO PB-I-POLICY-FORM-NO.          00033582
03361                                                                   00033583
03362 ******************************************************************00033584
03363 *          IF BTYPE = ZEROS DELETE LIFE COVERAGE.                *00033585
03364 *                                                                *00033586
03365 *          IF BTYPE = SPACES ZERO OUT BENEFIT CODE.              *00033587
03366 ******************************************************************00033588
03367                                                                   00033589
03368      IF BTYPE-LEN     (1)   GREATER THAN ZEROS                    00033590
03369         IF BTYPE      (1)   NOT = SPACES AND ZEROS                00033591
03370            MOVE BTYPE (1)         TO PB-I-LF-INPUT-CD             00033592
03371            MOVE WS-EDITED-LF-CODE TO PB-I-LIFE-BENEFIT-CD         00033593
03372            MOVE WS-LF-ABBR-DESC   TO PB-I-LF-ABBR                 00033594
03373         ELSE                                                      00033595
03374            IF BTYPE   (1) = SPACES                                00033596
03375               MOVE SPACES         TO PB-I-LF-INPUT-CD             00033597
03376                                      PB-I-LF-ABBR                 00033598
03377               MOVE ZEROS          TO PB-I-LIFE-BENEFIT-CD         00033599
03378            ELSE                                                   00033600
03379               SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED 00033601
03380               SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM               00033602
03381                        PI-LF-ISS-ENTERED                          00033603
03382               MOVE SPACES         TO PB-I-LF-INPUT-CD             00033604
03383                                      PB-I-LF-ABBR                 00033605
03384               MOVE ZEROS          TO PB-I-LF-TERM                 00033606
03385                                      PB-I-LF-BENEFIT-AMT          00033607
03386                                      PB-I-LF-PREMIUM-AMT          00033608
03387                                      PB-I-LF-BENEFIT-CD           00033609
03388                                      PB-I-LF-PREM-CALC            00033610
03389                                      PB-I-LF-ALT-BENEFIT-AMT      00033611
03390                                      PB-I-LF-ALT-PREMIUM-AMT      00033612
03391                                      PB-I-LF-CRIT-PER             00033613
03392               MOVE LOW-VALUES     TO PB-I-LF-EXPIRE-DT.           00033614
03393                                                                   00033930
03394      IF  BBEN-LEN      (1)  GREATER THAN ZEROS                    00033931
03395          MOVE WS-BBEN  (1)       TO PB-I-LF-BENEFIT-AMT.          00033932
03396                                                                   00033960
03397      IF  BALT-BEN-LEN     (1)  GREATER THAN ZEROS                 00033961
03398          MOVE WS-BALT-BEN (1)       TO PB-I-LF-ALT-BENEFIT-AMT.   00033962
03399                                                                   00033963
03400      IF  BPREM-LEN     (1)  GREATER THAN ZEROS                    00033964
03401          IF WS-BPREM   (1)  = WS-ALL-NINES OR                     00033965
03402             WS-BPREM   (1)  GREATER THAN WS-ALL-NINES             00033966
03403             SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED   00033967
03404             MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT           00033968
03405             MOVE '?'             TO PB-I-LF-CALC-FLAG             00033969
03406          ELSE                                                     00033970
03407             SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED   00033971
03408             MOVE WS-BPREM (1)    TO PB-I-LF-PREMIUM-AMT           00033972
03409             ADD  WS-BPREM (1)    TO PI-LF-ISS-ENTERED             00033973
03410             MOVE SPACE           TO PB-I-LF-CALC-FLAG.            00033974
03411                                                                   00033975
03412      IF  BALT-PREM-LEN     (1) GREATER THAN ZEROS                 00033976
03413          SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED  00033977
03414          MOVE WS-BALT-PREM (1)   TO PB-I-LF-ALT-PREMIUM-AMT       00033978
03415          ADD  WS-BALT-PREM (1)   TO PI-LF-ISS-ENTERED.            00033979
03416                                                                   00033980
03417 ******************************************************************00033981
03418 *          IF BTYPE = ZEROS DELETE A&H COVERAGE.                 *00033982
03419 *                                                                *00033983
03420 *          IF BTYPE = SPACES ZERO OUT BENEFIT CODE.              *00033984
03421 ******************************************************************00033985
03422                                                                   00033986
03423      IF BTYPE-LEN     (2)   GREATER THAN ZEROS                    00033987
03424         IF BTYPE      (2)   NOT = SPACES AND ZEROS                00033988
03425            MOVE BTYPE (2)         TO PB-I-AH-INPUT-CD             00033989
03426            MOVE WS-EDITED-AH-CODE TO PB-I-AH-BENEFIT-CD           00033990
03427            MOVE WS-AH-ABBR-DESC   TO PB-I-AH-ABBR                 00033991
03428         ELSE                                                      00033992
03429            IF BTYPE   (2) = SPACES                                00033993
03430               MOVE SPACES         TO PB-I-AH-INPUT-CD             00033994
03431                                      PB-I-AH-ABBR                 00033995
03432               MOVE ZEROS          TO PB-I-AH-BENEFIT-CD           00033996
03433            ELSE                                                   00033997
03434               SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED 00033998
03435               MOVE SPACES         TO PB-I-AH-INPUT-CD             00033999
03436                                      PB-I-AH-ABBR                 00034000
03437               MOVE ZEROS          TO PB-I-AH-TERM                 00034001
03438                                      PB-I-AH-BENEFIT-AMT          00034002
03439                                      PB-I-AH-PREMIUM-AMT          00034003
03440                                      PB-I-AH-BENEFIT-CD           00034004
03441                                      PB-I-AH-PREM-CALC            00034005
03442                                      PB-I-AH-CRIT-PER             00034006
03443               MOVE LOW-VALUES     TO PB-I-AH-EXPIRE-DT.           00034007
03444                                                                   00034440
03445      IF  BBEN-LEN      (2)  GREATER THAN ZEROS                    00034441
03446          MOVE WS-BBEN  (2)       TO PB-I-AH-BENEFIT-AMT.          00034442
03447                                                                   00034443
03448                                                                   00034444
03449      IF  BPREM-LEN     (2)  GREATER THAN ZEROS                    00034445
03450          IF WS-BPREM   (2)  = WS-ALL-NINES OR                     00034446
03451             WS-BPREM   (2)  GREATER THAN WS-ALL-NINES             00034447
03452             SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED   00034448
03453             MOVE ZEROS              TO PB-I-AH-PREMIUM-AMT        00034449
03454             MOVE '?'                TO PB-I-AH-CALC-FLAG          00034450
03455          ELSE                                                     00034451
03456             SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED   00034452
03457             MOVE WS-BPREM (2)    TO PB-I-AH-PREMIUM-AMT           00034453
03458             ADD  WS-BPREM (2)    TO PI-AH-ISS-ENTERED             00034454
03459             MOVE SPACE           TO PB-I-AH-CALC-FLAG.            00034455
03460                                                                   00034600
03461      IF BCRIT-PERD-LEN      (1)   GREATER THAN ZEROS              00034601
03462         MOVE WS-BCRIT-PERD  (1)   TO PB-I-LF-CRIT-PER.            00034602
03463                                                                   00034630
03464      IF BCRIT-PERD-LEN      (2)   GREATER THAN ZEROS              00034631
03465         MOVE WS-BCRIT-PERD  (2)   TO PB-I-AH-CRIT-PER.            00034632
03466                                                                   00034660
03467      IF BIND-GRP-LEN        GREATER THAN ZEROS                    00034661
03468          MOVE BIND-GRP           TO PB-I-INDV-GRP-OVRD.           00034662
03469                                                                   00034690
03470      IF BRTCLS-LEN          GREATER THAN ZEROS                    00034691
03471          MOVE BRTCLS             TO PB-I-RATE-CLASS-OVRD.         00034692
03472                                                                   00034720
03473      IF BSIG-LEN            GREATER THAN ZEROS                    00034721
03474          MOVE BSIG               TO PB-I-SIG-SW.                  00034722
03475                                                                   00034750
03476      IF BAPR-LEN            GREATER THAN ZEROS                    00034751
03477          MOVE WS-BAPR            TO PB-I-LOAN-APR.                00034752
03478                                                                   00034780
03479      IF BSSNUM-LEN          GREATER THAN ZEROS                    00034781
03480          MOVE BSSNUM             TO PB-I-SOC-SEC-NO.              00034782
03481                                                                   00034810
03482      IF BMEM-NO-LEN         GREATER THAN ZEROS                    00034811
03483          MOVE BMEM-NO        TO PB-I-MEMBER-NO.                   00034812
03484                                                                   00034840
03485      IF BLN-OFFICER-LEN     GREATER THAN ZEROS                    00034841
03486          MOVE BLN-OFFICER        TO PB-I-LOAN-OFFICER.            00034842
03487                                                                   00034870
03488      IF BEXPIRE-LEN    (1)  GREATER THAN ZEROS                    00034871
03489         MOVE WS-CONVERTED-EXPIRDT (1) TO PB-I-LF-EXPIRE-DT.       00034872
03490                                                                   00034900
03491      IF BEXPIRE-LEN    (2)  GREATER THAN ZEROS                    00034901
03492         MOVE WS-CONVERTED-EXPIRDT (2) TO PB-I-AH-EXPIRE-DT.       00034902
03493                                                                   00034930
03494      IF B1ST-PMT-LEN        GREATER THAN ZEROS                    00034931
03495         MOVE WS-CONVERTED-1ST-PMT-DT TO  PB-I-1ST-PMT-DT.         00034932
03496                                                                   00034933
03497      IF BDAYS-LEN           GREATER THAN ZEROS                    00034934
03498         MOVE WS-BDAYS            TO PB-I-TERM-IN-DAYS             00034935
03499                                     PB-I-EXTENTION-DAYS.          00034936
03500                                                                   00035000
03501      IF BRINCD-LEN               GREATER THAN ZEROS               00035001
03502         MOVE BRINCD              TO PB-I-SPECIAL-REIN-CODE.       00035002
03503                                                                   00035003
03504      IF BENTRY-LEN   NOT = ZEROS                                  00035004
03505         IF BENTRY = 'E' OR 'R' OR 'P'                             00035005
03506            MOVE BENTRY           TO PB-BATCH-ENTRY                00035006
03507      ELSE                                                         00035007
03508         IF BENTRY = 'H'                                           00035008
03509            MOVE BENTRY           TO PB-RECORD-BILL                00035009
03510         ELSE                                                      00035010
03511            MOVE BENTRY           TO PB-FORCE-CODE.                00035011
03512                                                                   00035120
03513      IF BLIVES-LEN          GREATER THAN ZEROS                    00035121
03514         MOVE WS-BLIVES           TO PB-I-LIVES.                   00035122
03515                                                                   00035150
03516      IF BJNT-1ST-NAME-LEN   GREATER THAN ZEROS                    00035151
03517          MOVE BJNT-1ST-NAME      TO PB-I-JOINT-FIRST-NAME.        00035152
03518                                                                   00035180
03519      IF BJNT-INIT-LEN       GREATER THAN ZEROS                    00035181
03520          MOVE BJNT-INIT          TO PB-I-JOINT-MIDDLE-INIT.       00035182
03521                                                                   00035210
03522      IF BJNT-LST-NAME-LEN   GREATER THAN ZEROS                    00035211
03523          MOVE BJNT-LST-NAME      TO PB-I-JOINT-LAST-NAME.         00035212
03524                                                                   00035240
03525      IF BBENEFICIARY-LEN    GREATER THAN ZEROS                    00035241
03526          MOVE BBENEFICIARY       TO PB-I-BENEFICIARY-NAME.        00035242
03527                                                                   00035270
03528      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          00035271
03529      MOVE PI-AH-OVERRIDE-L1      TO PB-LIFE-OVERRIDE-L1.          00035272
03530                                                                   00035300
03531      IF PI-MAIL-YES                                               00035301
03532         MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.           00035302
03533                                                                   00035303
03534      MOVE 'C'                    TO JP-RECORD-TYPE.               00035304
03535      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00035305
03536      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     00035306
03537                                                                   00035307
03538      EXEC CICS REWRITE                                            00035308
03539          DATASET (FILE-ID-ERPNDT)                                 00035309
03540          FROM    (PENDING-BUSINESS)                               00035310
03541          END-EXEC.                                                00035311
03542                                                                   00035312
03543      MOVE ERPNDT-KEY             TO PI-SAV-ENDING-ERPNDT-KEY.     00035313
03544                                                                   00035314
03545      PERFORM 8400-LOG-JOURNAL-RECORD.                             00035315
03546                                                                   00035316
03547      IF EIBAID = DFHENTER                                         00035317
03548          MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ                      00035318
03549          ADD +1                      TO PI-NEXT-DISPLAY-SEQ-NO    00035319
03550          MOVE AL-SABON               TO BSEQ-ATTRB.               00035320
03551                                                                   00035321
03552      IF PI-MAIL-YES                                               00035322
03553         NEXT SENTENCE                                             00035323
03554      ELSE                                                         00035324
03555         GO TO 4900-EXIT.                                          00035325
03556                                                                   00035326
03557      IF BLAST-NAME-LEN = ZEROS AND                                00035327
03558         B1ST-NAME-LEN  = ZEROS AND                                00035328
03559         BINIT-LEN      = ZEROS AND                                00035329
03560         BADDRS1-LEN    = ZEROS AND                                00035330
03561         BADDRS2-LEN    = ZEROS AND                                00035331
03562         BCITYST-LEN    = ZEROS AND                                00035332
03563         BZIP5-LEN      = ZEROS AND                                00035333
03564         BZIP4-LEN      = ZEROS AND                                00035334
03565         BPHONE-LEN     = ZEROS AND                                00035335
03566         BSSNUM-LEN     = ZEROS                                    00035336
03567         GO TO 4900-EXIT.                                          00035337
03568                                                                   00035338
03569      EXEC CICS HANDLE CONDITION                                   00035339
03570          NOTFND (4185-ADD-MAILING-RECORD)                         00035340
03571          END-EXEC.                                                00035341
03572                                                                   00035342
03573      EXEC CICS READ                                               00035343
03574          SET     (ERPNDM-POINTER)                                 00035344
03575          DATASET (FILE-ID-ERPNDM)                                 00035345
03576          RIDFLD  (ERPNDM-KEY)                                     00035346
03577          UPDATE                                                   00035347
03578          END-EXEC.                                                00035348
03579                                                                   00035790
03580      SERVICE RELOAD PENDING-MAILING-DATA.                         00035791
03581                                                                   00035792
03582      MOVE 'B'                    TO JP-RECORD-TYPE.               00035793
03583      MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.               00035794
03584      MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     00035795
03585      MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.                   00035796
03586      PERFORM 8400-LOG-JOURNAL-RECORD.                             00035797
03587                                                                   00035798
03588      MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY.             00035799
03589      MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.         00035800
03590      MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT.             00035801
03591                                                                   00035802
03592      IF BLAST-NAME-LEN      GREATER THAN ZEROS                    00035803
03593          MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.         00035804
03594                                                                   00035805
03595      IF B1ST-NAME-LEN       GREATER THAN ZEROS                    00035806
03596          MOVE B1ST-NAME          TO PM-INSURED-FIRST-NAME.        00035807
03597                                                                   00035808
03598      IF BINIT-LEN           GREATER THAN ZEROS                    00035809
03599          MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.       00035810
03600                                                                   00035811
03601      IF BAGE-LEN            GREATER THAN ZEROS                    00035812
03602          MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE.         00035813
03603                                                                   00035814
03604      IF BBIRTH-LEN          GREATER THAN ZEROS                    00035815
03605         MOVE  WS-CONVERTED-BIRTH TO PM-INSURED-BIRTH-DT.          00035816
03606                                                                   00035817
03607      IF BLAST-NAME-LEN      GREATER THAN ZEROS                    00035818
03608          MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.         00035819
03609                                                                   00035820
03610      IF BINIT-LEN           GREATER THAN ZEROS                    00035821
03611          MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.       00035822
03612                                                                   00035823
03613      IF BADDRS1-LEN         GREATER THAN ZERO                     00035824
03614          MOVE BADDRS1            TO PM-ADDRESS-LINE-1.            00035825
03615                                                                   00035826
03616      IF BADDRS2-LEN         GREATER THAN ZERO                     00035827
03617          MOVE BADDRS2            TO PM-ADDRESS-LINE-2.            00035828
03618                                                                   00035829
03619      IF BCITYST-LEN         GREATER THAN ZERO                     00035830
03620          MOVE BCITYST            TO PM-CITY-STATE.                00035831
03621                                                                   00035832
03622      IF BZIP5-LEN           GREATER THAN ZERO                     00035833
03623          MOVE BZIP5              TO W-TEST-ZIP                    00035834
03624                                                                   00035835
03625          IF  W-CANADIAN-POST-CODE                                 00035836
03626              MOVE BZIP5          TO PM-CAN-POST1                  00035837
03627                                                                   00035838
03628          ELSE                                                     00035839
03629              MOVE BZIP5          TO PM-ZIP-CODE.                  00035840
03630                                                                   00035841
03631                                                                   00036310
03632      IF BZIP4-LEN           GREATER THAN ZERO                     00036311
03633                                                                   00036312
03634          IF  PM-CANADIAN-POST-CODE                                00036313
03635              MOVE BZIP4          TO PM-CAN-POST2                  00036314
03636                                                                   00036360
03637          ELSE                                                     00036361
03638              MOVE BZIP4          TO PM-ZIP-PLUS4.                 00036362
03639                                                                   00036363
03640      IF BPHONE-LEN          GREATER THAN ZERO                     00036364
03641          MOVE WS-BPHONE          TO PM-PHONE-NO.                  00036365
03642                                                                   00036366
03643      IF BSSNUM-LEN          GREATER THAN ZEROS                    00036367
03644          MOVE BSSNUM             TO PM-INSURED-SOC-SEC-NO.        00036368
03645                                                                   00036450
03646      MOVE 'C'                    TO JP-RECORD-TYPE.               00036451
03647      MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.               00036452
03648      MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     00036453
03649      MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.                   00036454
03650                                                                   00036455
03651      EXEC CICS REWRITE                                            00036456
03652          DATASET (FILE-ID-ERPNDM)                                 00036457
03653          FROM    (PENDING-MAILING-DATA)                           00036458
03654          END-EXEC.                                                00036459
03655                                                                   00036550
03656      PERFORM 8400-LOG-JOURNAL-RECORD.                             00036551
03657                                                                   00036552
03658      GO TO 4900-EXIT.                                             00036553
03659                                                                   00036554
03660      EJECT                                                        00036555
03661                                                                   00036610
03662 ******************************************************************00036611
03663 *                                                                *00036612
03664 *               A D D  I S S U E   R E C O R D                   *00036613
03665 *                                                                *00036614
03666 ******************************************************************00036615
03667                                                                   00036616
03668  4100-ADD-ISSUE-RECORD.                                           00036617
03669                                                                   00036618
03670      EXEC CICS GETMAIN                                            00036619
03671          SET     (ERPNDT-POINTER)                                 00036620
03672          LENGTH  (ERPNDT-RECORD-LENGTH)                           00036621
03673          INITIMG (GETMAIN-SPACE)                                  00036622
03674          END-EXEC.                                                00036623
03675                                                                   00036624
03676      SERVICE RELOAD PENDING-BUSINESS.                             00036625
03677                                                                   00036626
03678      MOVE 'PB'                   TO PB-RECORD-ID.                 00036627
03679      MOVE PI-COMPANY-CD          TO PB-COMPANY-CD                 00036628
03680                                     PB-COMPANY-CD-A1.             00036629
03681      MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.                00036630
03682      MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH.               00036631
03683      MOVE BSEQ                   TO PB-BATCH-SEQ-NO.              00036632
03684                                                                   00036633
03685      IF BSEQ   GREATER THAN PI-LAST-SEQ-NO-ADDED                  00036634
03686          MOVE BSEQ          TO PI-LAST-SEQ-NO-ADDED.              00036635
03687                                                                   00036636
03688      IF BCAR-LEN GREATER THAN ZEROS                               00036637
03689         MOVE BCAR                TO PB-CARRIER.                   00036638
03690                                                                   00036639
03691      IF BGRP-LEN GREATER THAN ZEROS                               00036640
03692         MOVE BGRP                TO PB-GROUPING.                  00036641
03693                                                                   00036642
03694      IF BST-LEN  GREATER THAN ZEROS                               00036643
03695         MOVE BST                 TO PB-STATE.                     00036644
03696                                                                   00036645
03697      IF BACCT-LEN GREATER THAN ZEROS                              00036646
03698         MOVE BACCT               TO PB-ACCOUNT.                   00036647
03699                                                                   00036648
03700      MOVE '1'                    TO PB-RECORD-TYPE.               00036649
03701      MOVE BCERT                  TO PB-CERT-PRIME.                00036650
03702      MOVE WS-CONVERTED-EFFDT     TO PB-CERT-EFF-DT.               00036651
03703      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           00036652
03704                                     PB-ALT-CHG-SEQ-NO.            00036653
03705                                                                   00036654
03706      MOVE ZEROS                  TO PB-I-LOAN-TERM                00036655
03707                                     PB-I-LF-PREM-CALC             00036656
03708                                     PB-I-LF-ALT-PREM-CALC         00036657
03709                                     PB-I-LF-RATE                  00036658
03710                                     PB-I-LF-ALT-RATE              00036659
03711                                     PB-I-LF-REI-RATE              00036660
03712                                     PB-I-LF-ALT-REI-RATE          00036661
03713                                     PB-I-RATE-DEV-PCT-LF          00036662
03714                                     PB-I-AH-PREM-CALC             00036663
03715                                     PB-I-AH-RATE                  00036664
03716                                     PB-I-AH-REI-RATE              00036665
03717                                     PB-I-AH-RATE-TRM              00036666
03718                                     PB-I-RATE-DEV-PCT-AH          00036667
03719                                     PB-I-BUSINESS-TYPE            00036668
03720                                     PB-I-LIFE-COMMISSION          00036669
03721                                     PB-I-JOINT-COMMISSION         00036670
03722                                     PB-I-AH-COMMISSION            00036671
03723                                     PB-I-CURR-SEQ                 00036672
03724                                     PB-CHG-COUNT                  00036673
03725                                     PB-LF-BILLED-AMTS             00036674
03726                                     PB-AH-BILLED-AMTS             00036675
03727                                     PB-CALC-TOLERANCE             00036676
03728                                     PB-I-EXTENTION-DAYS           00036677
03729                                     PB-I-TERM-IN-DAYS.            00036678
03730                                                                   00037300
03731      MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT           00037301
03732                                     PB-I-LF-EXPIRE-DT             00037302
03733                                     PB-I-AH-EXPIRE-DT             00037303
03734                                     PB-I-1ST-PMT-DT               00037304
03735                                     PB-BILLED-DT                  00037305
03736                                     PB-ACCT-EFF-DT                00037306
03737                                     PB-ACCT-EXP-DT.               00037307
03738                                                                   00037380
03739                                                                   00037381
03740      MOVE 'X'                    TO PB-FATAL-FLAG.                00037382
03741                                                                   00037410
03742      IF PI-MAIL-YES                                               00037411
03743         MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.           00037412
03744                                                                   00037440
03745      IF PI-NB-MONTH-END-DT NOT = SPACES                           00037441
03746         MOVE PI-NB-MONTH-END-DT  TO PB-CREDIT-SELECT-DT           00037442
03747        ELSE                                                       00037443
03748         MOVE PI-CR-MONTH-END-DT     TO PB-CREDIT-SELECT-DT.       00037444
03749                                                                   00037445
03750      IF BSFX-LEN            GREATER THAN ZEROS                    00037446
03751          MOVE BSFX               TO PB-CERT-SFX.                  00037447
03752                                                                   00037520
03753      IF BLAST-NAME-LEN      GREATER THAN ZEROS                    00037521
03754          MOVE BLAST-NAME         TO PB-I-INSURED-LAST-NAME.       00037522
03755                                                                   00037523
03756      IF B1ST-NAME-LEN       GREATER THAN ZEROS                    00037524
03757          MOVE B1ST-NAME          TO PB-I-INSURED-FIRST-NAME.      00037525
03758                                                                   00037580
03759      IF BINIT-LEN           GREATER THAN ZEROS                    00037581
03760          MOVE BINIT              TO PB-I-INSURED-MIDDLE-INIT.     00037582
03761                                                                   00037583
03762      IF BAGE-LEN            GREATER THAN ZEROS                    00037584
03763          MOVE WS-BAGE            TO PB-I-AGE                      00037585
03764      ELSE                                                         00037586
03765          MOVE ZEROS              TO PB-I-AGE.                     00037587
03766                                                                   00037660
03767                                                                   00037661
03768      IF BJNT-AGE-LEN        GREATER THAN ZEROS                    00037662
03769          MOVE WS-BJNT-AGE        TO PB-I-JOINT-AGE                00037663
03770      ELSE                                                         00037664
03771          MOVE ZEROS              TO PB-I-JOINT-AGE.               00037665
03772                                                                   00037666
03773      IF BBIRTH-LEN          GREATER THAN ZEROS                    00037667
03774          MOVE WS-CONVERTED-BIRTH TO PB-I-BIRTHDAY                 00037668
03775      ELSE                                                         00037669
03776          MOVE LOW-VALUES          TO PB-I-BIRTHDAY.               00037670
03777                                                                   00037671
03778      IF BSEX-LEN            GREATER THAN ZEROS                    00037672
03779          MOVE BSEX               TO PB-I-INSURED-SEX.             00037673
03780                                                                   00037674
03781      IF BTERM-LEN  (1)      GREATER THAN ZEROS                    00037675
03782         MOVE WS-BTERM (1)           TO PB-I-LF-TERM               00037676
03783      ELSE                                                         00037677
03784         MOVE ZEROS               TO PB-I-LF-TERM.                 00037678
03785                                                                   00037679
03786      IF BTERM-LEN  (2)      GREATER THAN ZEROS                    00037680
03787         MOVE WS-BTERM (2)           TO PB-I-AH-TERM               00037681
03788      ELSE                                                         00037682
03789         MOVE ZEROS              TO PB-I-AH-TERM.                  00037683
03790                                                                   00037684
03791      IF BLN-TERM-LEN        GREATER THAN ZEROS                    00037685
03792          MOVE WS-BLN-TERM        TO PB-I-LOAN-TERM                00037686
03793      ELSE                                                         00037687
03794          MOVE ZEROS              TO PB-I-LOAN-TERM.               00037688
03795                                                                   00037689
03796      IF BFREQ-LEN           GREATER THAN ZEROS                    00037690
03797          MOVE WS-BFREQ           TO PB-I-PAY-FREQUENCY            00037691
03798      ELSE                                                         00037692
03799          MOVE ZEROS              TO PB-I-PAY-FREQUENCY.           00037693
03800                                                                   00038000
03801      IF BSKPCD-LEN          GREATER THAN ZEROS                    00038001
03802          MOVE BSKPCD             TO PB-I-SKIP-CODE.               00038002
03803                                                                   00038003
03804      IF BMODE-LEN           GREATER THAN ZEROS                    00038004
03805          MOVE BMODE              TO PB-I-TERM-TYPE.               00038005
03806                                                                   00038006
03807      IF BPMTS-LEN           GREATER THAN ZEROS                    00038007
03808          MOVE WS-BPMTS           TO PB-I-NO-OF-PAYMENTS           00038008
03809      ELSE                                                         00038009
03810          MOVE ZEROS              TO PB-I-NO-OF-PAYMENTS.          00038010
03811                                                                   00038011
03812      IF BPOLICY-LEN         GREATER THAN ZEROS                    00038012
03813          MOVE BPOLICY            TO PB-I-POLICY-FORM-NO.          00038013
03814                                                                   00038014
03815      IF BTYPE-LEN     (1)   GREATER THAN ZEROS                    00038015
03816         IF BTYPE      (1)   NOT = ZEROS  AND  SPACES              00038016
03817            MOVE BTYPE (1)         TO PB-I-LF-INPUT-CD             00038017
03818            MOVE WS-EDITED-LF-CODE TO PB-I-LIFE-BENEFIT-CD         00038018
03819            MOVE WS-LF-ABBR-DESC  TO PB-I-LF-ABBR                  00038019
03820         ELSE                                                      00038020
03821            MOVE ZEROS             TO PB-I-LIFE-BENEFIT-CD         00038021
03822      ELSE                                                         00038022
03823            MOVE ZEROS             TO PB-I-LIFE-BENEFIT-CD.        00038023
03824                                                                   00038024
03825      IF  BBEN-LEN      (1)  GREATER THAN ZEROS                    00038025
03826          MOVE WS-BBEN  (1)       TO PB-I-LF-BENEFIT-AMT           00038026
03827      ELSE                                                         00038027
03828          MOVE ZEROS              TO PB-I-LF-BENEFIT-AMT.          00038028
03829                                                                   00038029
03830      IF  BALT-BEN-LEN     (1)  GREATER THAN ZEROS                 00038030
03831          MOVE WS-BALT-BEN (1)    TO PB-I-LF-ALT-BENEFIT-AMT       00038031
03832      ELSE                                                         00038032
03833          MOVE ZEROS              TO PB-I-LF-ALT-BENEFIT-AMT.      00038033
03834                                                                   00038034
03835      IF  BPREM-LEN     (1)  GREATER THAN ZEROS                    00038035
03836          IF WS-BPREM   (1) = WS-ALL-NINES OR                      00038036
03837             WS-BPREM   (1) GREATER THAN WS-ALL-NINES              00038037
03838             MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT           00038038
03839             MOVE '?'             TO PB-I-LF-CALC-FLAG             00038039
03840          ELSE                                                     00038040
03841             ADD  WS-BPREM (1)    TO PI-LF-ISS-ENTERED             00038041
03842             MOVE WS-BPREM (1)    TO PB-I-LF-PREMIUM-AMT           00038042
03843      ELSE                                                         00038043
03844          MOVE ZEROS              TO PB-I-LF-PREMIUM-AMT.          00038044
03845                                                                   00038045
03846      IF  BALT-PREM-LEN     (1) GREATER THAN ZEROS                 00038046
03847          MOVE WS-BALT-PREM (1)   TO PB-I-LF-ALT-PREMIUM-AMT       00038047
03848          ADD  WS-BALT-PREM (1)   TO PI-LF-ISS-ENTERED             00038048
03849      ELSE                                                         00038049
03850          MOVE ZEROS              TO PB-I-LF-ALT-PREMIUM-AMT.      00038050
03851                                                                   00038051
03852      IF BTYPE-LEN     (2)   GREATER THAN ZEROS                    00038052
03853         IF BTYPE      (2)   NOT = ZEROS  AND  SPACES              00038053
03854            MOVE BTYPE (2)         TO PB-I-AH-INPUT-CD             00038054
03855            MOVE WS-EDITED-AH-CODE TO PB-I-AH-BENEFIT-CD           00038055
03856            MOVE WS-AH-ABBR-DESC   TO PB-I-AH-ABBR                 00038056
03857         ELSE                                                      00038057
03858            MOVE ZEROS             TO PB-I-AH-BENEFIT-CD           00038058
03859      ELSE                                                         00038059
03860            MOVE ZEROS             TO PB-I-AH-BENEFIT-CD.          00038060
03861                                                                   00038061
03862      IF  BBEN-LEN         (2)  GREATER THAN ZEROS                 00038062
03863          MOVE WS-BBEN     (2)    TO PB-I-AH-BENEFIT-AMT           00038063
03864      ELSE                                                         00038064
03865          MOVE ZEROS              TO PB-I-AH-BENEFIT-AMT.          00038065
03866                                                                   00038066
03867                                                                   00038067
03868      IF  BPREM-LEN     (2)  GREATER THAN ZEROS                    00038068
03869          IF WS-BPREM   (2) = WS-ALL-NINES OR                      00038069
03870             WS-BPREM   (2) GREATER THAN WS-ALL-NINES              00038070
03871             MOVE ZEROS            TO PB-I-AH-PREMIUM-AMT          00038071
03872             MOVE '?'              TO PB-I-AH-CALC-FLAG            00038072
03873          ELSE                                                     00038073
03874             ADD  WS-BPREM (2)     TO PI-AH-ISS-ENTERED            00038074
03875             MOVE WS-BPREM (2)     TO PB-I-AH-PREMIUM-AMT          00038075
03876      ELSE                                                         00038076
03877          MOVE ZEROS               TO PB-I-AH-PREMIUM-AMT.         00038077
03878                                                                   00038078
03879      IF BCRIT-PERD-LEN      (1)   GREATER THAN ZEROS              00038079
03880         MOVE WS-BCRIT-PERD  (1)   TO PB-I-LF-CRIT-PER             00038080
03881      ELSE                                                         00038081
03882         MOVE ZEROS                TO PB-I-LF-CRIT-PER.            00038082
03883                                                                   00038083
03884      IF BCRIT-PERD-LEN      (2)   GREATER THAN ZEROS              00038084
03885         MOVE WS-BCRIT-PERD  (2)   TO PB-I-AH-CRIT-PER             00038085
03886      ELSE                                                         00038086
03887         MOVE ZEROS                TO PB-I-AH-CRIT-PER.            00038087
03888      IF BIND-GRP-LEN        GREATER THAN ZEROS                    00038088
03889          MOVE BIND-GRP           TO PB-I-INDV-GRP-OVRD.           00038089
03890                                                                   00038900
03891      IF BRTCLS-LEN          GREATER THAN ZEROS                    00038901
03892          MOVE BRTCLS             TO PB-I-RATE-CLASS-OVRD.         00038902
03893                                                                   00038930
03894      IF BSIG-LEN            GREATER THAN ZEROS                    00038931
03895          MOVE BSIG               TO PB-I-SIG-SW.                  00038932
03896                                                                   00038960
03897      IF BAPR-LEN            GREATER THAN ZEROS                    00038961
03898          MOVE WS-BAPR            TO PB-I-LOAN-APR                 00038962
03899      ELSE                                                         00038990
03900          MOVE ZEROS              TO PB-I-LOAN-APR.                00038991
03901                                                                   00039010
03902      IF BSSNUM-LEN          GREATER THAN ZEROS                    00039011
03903          MOVE BSSNUM             TO PB-I-SOC-SEC-NO.              00039012
03904                                                                   00039013
03905      IF BMEM-NO-LEN         GREATER THAN ZEROS                    00039014
03906          MOVE BMEM-NO        TO PB-I-MEMBER-NO.                   00039015
03907                                                                   00039016
03908      IF BLN-OFFICER-LEN     GREATER THAN ZEROS                    00039017
03909          MOVE BLN-OFFICER        TO PB-I-LOAN-OFFICER.            00039018
03910                                                                   00039019
03911      IF BEXPIRE-LEN    (1)  GREATER THAN ZEROS                    00039020
03912         MOVE WS-CONVERTED-EXPIRDT (1) TO PB-I-LF-EXPIRE-DT        00039021
03913      ELSE                                                         00039130
03914         MOVE LOW-VALUES               TO PB-I-LF-EXPIRE-DT.       00039131
03915                                                                   00039132
03916      IF BEXPIRE-LEN    (2)  GREATER THAN ZEROS                    00039133
03917         MOVE WS-CONVERTED-EXPIRDT (2) TO PB-I-AH-EXPIRE-DT        00039134
03918      ELSE                                                         00039135
03919         MOVE LOW-VALUES               TO PB-I-AH-EXPIRE-DT.       00039136
03920                                                                   00039137
03921      IF B1ST-PMT-LEN        GREATER THAN ZEROS                    00039138
03922         MOVE WS-CONVERTED-1ST-PMT-DT TO  PB-I-1ST-PMT-DT.         00039139
03923                                                                   00039140
03924      IF BDAYS-LEN           GREATER THAN ZEROS                    00039141
03925         MOVE WS-BDAYS            TO PB-I-TERM-IN-DAYS             00039142
03926                                     PB-I-EXTENTION-DAYS           00039143
03927      ELSE                                                         00039144
03928         MOVE ZEROS               TO PB-I-TERM-IN-DAYS             00039145
03929                                     PB-I-EXTENTION-DAYS.          00039146
03930                                                                   00039147
03931      IF BRINCD-LEN               GREATER THAN ZEROS               00039148
03932         MOVE BRINCD              TO PB-I-SPECIAL-REIN-CODE.       00039149
03933                                                                   00039330
03934      IF BENTRY-LEN   NOT = ZEROS                                  00039331
03935         IF BENTRY = 'E' OR 'R' OR 'P'                             00039332
03936            MOVE BENTRY           TO PB-BATCH-ENTRY                00039333
03937      ELSE                                                         00039334
03938         IF BENTRY = 'H'                                           00039335
03939            MOVE BENTRY           TO PB-RECORD-BILL                00039336
03940         ELSE                                                      00039337
03941            MOVE BENTRY           TO PB-FORCE-CODE.                00039338
03942                                                                   00039420
03943      IF BLIVES-LEN          GREATER THAN ZEROS                    00039421
03944         MOVE WS-BLIVES           TO PB-I-LIVES                    00039422
03945      ELSE                                                         00039423
03946         MOVE ZEROS               TO PB-I-LIVES.                   00039424
03947                                                                   00039425
03948      IF BJNT-1ST-NAME-LEN   GREATER THAN ZEROS                    00039426
03949          MOVE BJNT-1ST-NAME      TO PB-I-JOINT-FIRST-NAME.        00039427
03950                                                                   00039428
03951      IF BJNT-INIT-LEN       GREATER THAN ZEROS                    00039429
03952          MOVE BJNT-INIT          TO PB-I-JOINT-MIDDLE-INIT.       00039430
03953                                                                   00039431
03954      IF BJNT-LST-NAME-LEN   GREATER THAN ZEROS                    00039432
03955          MOVE BJNT-LST-NAME      TO PB-I-JOINT-LAST-NAME.         00039433
03956                                                                   00039434
03957      IF BBENEFICIARY-LEN    GREATER THAN ZEROS                    00039435
03958          MOVE BBENEFICIARY       TO PB-I-BENEFICIARY-NAME.        00039436
03959                                                                   00039437
03960      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          00039438
03961      MOVE PI-AH-OVERRIDE-L1      TO PB-LIFE-OVERRIDE-L1.          00039439
03962                                                                   00039440
03963                                                                   00039630
03964  4175-WRITE-PB-RECORD.                                            00039631
03965                                                                   00039632
03966      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY              00039633
03967                                     PB-INPUT-BY.                  00039634
03968      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         00039635
03969      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT              00039636
03970                                     PB-INPUT-DT.                  00039637
03971      MOVE 'A'                    TO JP-RECORD-TYPE.               00039638
03972      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00039639
03973      MOVE PB-CONTROL-PRIMARY     TO PI-SAV-ENDING-ERPNDT-KEY      00039640
03974                                     ERPNDM-KEY.                   00039641
03975                                                                   00039750
03976      ADD +1                      TO PI-SAV-BATCH-SEQ.             00039751
03977                                                                   00039752
03978      EXEC CICS HANDLE CONDITION                                   00039753
03979          DUPREC (4200-DUPLICATE-ALT-INDEX)                        00039754
03980          END-EXEC.                                                00039755
03981                                                                   00039756
03982      EXEC CICS WRITE                                              00039757
03983          DATASET (FILE-ID-ERPNDT)                                 00039758
03984          FROM    (PENDING-BUSINESS)                               00039759
03985          RIDFLD  (PB-CONTROL-PRIMARY)                             00039760
03986          END-EXEC.                                                00039761
03987                                                                   00039762
03988      ADD +1                      TO PI-ISS-CNT-ENTERED.           00039763
03989                                                                   00039764
03990      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     00039765
03991      MOVE FILE-ID-ERPNDT         TO JP-FILE-ID.                   00039766
03992      PERFORM 8400-LOG-JOURNAL-RECORD.                             00039767
03993                                                                   00039768
03994      MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ.                         00039769
03995      MOVE AL-SABON               TO BSEQ-ATTRB.                   00039770
03996                                                                   00039771
03997      ADD +1                      TO PI-NEXT-DISPLAY-SEQ-NO.       00039772
03998                                                                   00039773
03999      EJECT                                                        00039774
04000                                                                   00039775
04001 ******************************************************************00039776
04002 *                                                                *00039777
04003 *    CHECK THE FIRST ISSUE RECORD IN EVERY NEW BATCH.  VERIFY    *00039778
04004 *    THAT THE CERTIFICATE DOES NOT EXIST ON THE CERT. MASTER     *00039779
04005 *    FILE.  IF IT DOES DISPLAY WARNING MESSAGE ON BLANK SCREEN.  *00039780
04006 *                                                                *00039781
04007 ******************************************************************00039782
04008                                                                   00040080
04009                                                                   00040081
04010      IF  PI-MAINT-FUNC = 'N' NEXT SENTENCE                        00040082
04011         ELSE                                                      00040083
04012          GO TO 4185-ADD-MAILING-RECORD.                           00040084
04013                                                                   00040085
04014      IF  PI-ISSUE-ADDED                                           00040086
04015          GO TO 4185-ADD-MAILING-RECORD.                           00040087
04016                                                                   00040088
04017      MOVE 'Y'                    TO  PI-ISSUE-ADDED-SW.           00040089
04018                                                                   00040180
04019      EXEC CICS HANDLE CONDITION                                   00040181
04020          NOTFND (4185-ADD-MAILING-RECORD)                         00040182
04021          END-EXEC.                                                00040183
04022                                                                   00040184
04023      MOVE PB-CONTROL-BY-ACCOUNT  TO  ELCERT-KEY.                  00040185
04024                                                                   00040186
04025      EXEC CICS READ                                               00040187
04026          SET     (ELCERT-POINTER)                                 00040188
04027          DATASET (FILE-ID-ELCERT)                                 00040189
04028          RIDFLD  (ELCERT-KEY)                                     00040190
04029          LENGTH  (ELCERT-RECORD-LENGTH)                           00040191
04030          UPDATE                                                   00040192
04031          END-EXEC.                                                00040193
04032                                                                   00040194
04033      SERVICE RELOAD CERTIFICATE-MASTER.                           00040195
04034                                                                   00040196
04035      IF  CERT-WAS-CREATED-FOR-CLAIM                               00040197
04036          GO TO 4185-ADD-MAILING-RECORD.                           00040198
04037                                                                   00040199
04038      GO TO 8350-SEND-WARNING.                                     00040200
04039                                                                   00040201
04040      EJECT                                                        00040202
04041                                                                   00040203
04042 ******************************************************************00040204
04043 *                                                                *00040205
04044 *          A D D   M A I L I N G   R E C O R D                   *00040206
04045 *                                                                *00040207
04046 ******************************************************************00040208
04047                                                                   00040209
04048  4185-ADD-MAILING-RECORD.                                         00040210
04049                                                                   00040211
04050      IF PI-MAIL-YES                                               00040212
04051         NEXT SENTENCE                                             00040213
04052      ELSE                                                         00040214
04053         GO TO 4900-EXIT.                                          00040215
04054                                                                   00040216
04055      EXEC CICS GETMAIN                                            00040217
04056          SET     (ERPNDM-POINTER)                                 00040218
04057          LENGTH  (ERPNDM-RECORD-LENGTH)                           00040219
04058          INITIMG (GETMAIN-SPACE)                                  00040220
04059          END-EXEC.                                                00040221
04060                                                                   00040222
04061      SERVICE RELOAD PENDING-MAILING-DATA.                         00040223
04062                                                                   00040620
04063      MOVE 'PM'                   TO PM-RECORD-ID.                 00040621
04064      MOVE 'ER'                   TO PM-SOURCE-SYSTEM.             00040622
04065                                                                   00040650
04066      MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY              00040651
04067                                     PM-RECORD-ADDED-BY.           00040652
04068      MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.         00040653
04069      MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT              00040654
04070                                     PM-RECORD-ADD-DT.             00040655
04071                                                                   00040656
04072      MOVE ERPNDM-KEY             TO PM-CONTROL-PRIMARY.           00040657
04073                                                                   00040658
04074      IF BLAST-NAME-LEN      GREATER THAN ZEROS                    00040659
04075          MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.         00040660
04076                                                                   00040661
04077      IF B1ST-NAME-LEN       GREATER THAN ZEROS                    00040662
04078          MOVE B1ST-NAME          TO PM-INSURED-FIRST-NAME.        00040663
04079                                                                   00040790
04080      IF BINIT-LEN           GREATER THAN ZEROS                    00040791
04081          MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.       00040792
04082                                                                   00040820
04083                                                                   00040821
04084      IF BAGE-LEN            GREATER THAN ZEROS                    00040822
04085          MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE          00040823
04086      ELSE                                                         00040824
04087          MOVE ZEROS              TO PM-INSURED-ISSUE-AGE.         00040825
04088                                                                   00040880
04089      IF BBIRTH-LEN          GREATER THAN ZEROS                    00040881
04090         MOVE  WS-CONVERTED-BIRTH TO PM-INSURED-BIRTH-DT           00040882
04091      ELSE                                                         00040883
04092         MOVE LOW-VALUES          TO PM-INSURED-BIRTH-DT.          00040884
04093                                                                   00040885
04094      IF BLAST-NAME-LEN      GREATER THAN ZEROS                    00040886
04095          MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.         00040887
04096                                                                   00040888
04097      IF BINIT-LEN           GREATER THAN ZEROS                    00040889
04098          MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.       00040890
04099                                                                   00040891
04100      IF BADDRS1-LEN         GREATER THAN ZERO                     00040892
04101          MOVE BADDRS1            TO PM-ADDRESS-LINE-1.            00040893
04102                                                                   00040894
04103      IF BADDRS2-LEN         GREATER THAN ZERO                     00040895
04104          MOVE BADDRS2            TO PM-ADDRESS-LINE-2.            00040896
04105                                                                   00041050
04106      IF BCITYST-LEN         GREATER THAN ZERO                     00041051
04107          MOVE BCITYST            TO PM-CITY-STATE.                00041052
04108                                                                   00041080
04109      IF BZIP5-LEN           GREATER THAN ZERO                     00041081
04110          MOVE BZIP5              TO W-TEST-ZIP                    00041082
04111                                                                   00041083
04112          IF  W-CANADIAN-POST-CODE                                 00041084
04113              MOVE BZIP5          TO PM-CAN-POST1                  00041085
04114                                                                   00041086
04115          ELSE                                                     00041087
04116              MOVE BZIP5          TO PM-ZIP-CODE.                  00041088
04117                                                                   00041089
04118      IF BZIP4-LEN           GREATER THAN ZERO                     00041090
04119                                                                   00041190
04120          IF  PM-CANADIAN-POST-CODE                                00041191
04121              MOVE BZIP4          TO PM-CAN-POST2                  00041192
04122                                                                   00041193
04123          ELSE                                                     00041194
04124              MOVE BZIP4          TO PM-ZIP-PLUS4.                 00041195
04125                                                                   00041250
04126      IF BPHONE-LEN          GREATER THAN ZERO                     00041251
04127          MOVE WS-BPHONE          TO PM-PHONE-NO                   00041252
04128      ELSE                                                         00041253
04129          MOVE ZEROS              TO PM-PHONE-NO.                  00041254
04130                                                                   00041255
04131      IF BSSNUM-LEN          GREATER THAN ZEROS                    00041256
04132          MOVE BSSNUM             TO PM-INSURED-SOC-SEC-NO.        00041257
04133                                                                   00041330
04134      MOVE 'A'                    TO JP-RECORD-TYPE.               00041331
04135      MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.               00041332
04136      MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     00041333
04137      MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.                   00041334
04138                                                                   00041335
04139      EXEC CICS WRITE                                              00041336
04140          DATASET (FILE-ID-ERPNDM)                                 00041337
04141          FROM    (PENDING-MAILING-DATA)                           00041338
04142          RIDFLD  (PM-CONTROL-PRIMARY)                             00041339
04143          END-EXEC.                                                00041340
04144                                                                   00041341
04145      PERFORM 8400-LOG-JOURNAL-RECORD.                             00041342
04146                                                                   00041343
04147      MOVE LOW-VALUES             TO MAP-B.                        00041344
04148                                                                   00041345
04149      GO TO 4900-EXIT.                                             00041346
04150                                                                   00041500
04151  4200-DUPLICATE-ALT-INDEX.                                        00041501
04152                                                                   00041502
04153      MOVE ER-2247                TO EMI-ERROR.                    00041503
04154      MOVE -1                     TO BCERT-LEN.                    00041504
04155      MOVE AL-UABON               TO BCERT-ATTRB.                  00041505
04156      MOVE AL-UNBON               TO BEFFDT-ATTRB.                 00041506
04157      MOVE 'Y'                    TO PI-ERROR-SW.                  00041507
04158                                                                   00041508
04159      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     00041509
04160                                                                   00041510
04161      IF BPREM-LEN (1) GREATER THAN ZEROS                          00041511
04162          SUBTRACT WS-BPREM (1)                                    00041512
04163                                  FROM PI-LF-ISS-ENTERED.          00041513
04164                                                                   00041514
04165      IF BALT-PREM-LEN (1) GREATER THAN ZEROS                      00041515
04166          SUBTRACT WS-BALT-PREM (1)                                00041516
04167                                  FROM PI-LF-ISS-ENTERED.          00041517
04168                                                                   00041518
04169      IF BPREM-LEN (2) GREATER THAN ZEROS                          00041519
04170          SUBTRACT WS-BPREM (2)                                    00041520
04171                                  FROM PI-AH-ISS-ENTERED.          00041521
04172                                                                   00041522
04173      SUBTRACT +1                 FROM PI-LAST-SEQ-NO-ADDED.       00041523
04174      SUBTRACT +1                 FROM PI-SAV-BATCH-SEQ.           00041524
04175                                                                   00041525
04176  4900-EXIT.                                                       00041526
04177      EXIT.                                                        00041527
04178                                                                   00041528
04179      EJECT                                                        00041529
04180                                                                   00041530
04181 ******************************************************************00041531
04182 *                                                                *00041532
04183 *            B U I L D   C A N C E L   R E C O R D               *00041533
04184 *                                                                *00041534
04185 ******************************************************************00041535
04186                                                                   00041536
04187  5000-BUILD-CANCEL-RECORD.                                        00041537
04188                                                                   00041538
04189      IF CSEQ (WS-SUB2) GREATER THAN PI-LAST-SEQ-NO-ADDED          00041539
04190          GO TO 5100-ADD-CANCEL-RECORD.                            00041540
04191                                                                   00041541
04192 ******************************************************************00041542
04193 *                                                                *00041543
04194 *   THE DATA ENTRY SYSTEM ALLOWS BROWSING OF THE CURRENT BUS.    *00041544
04195 *   FILE. THE DATA ENTRY SYS. DOES NOT HAVE A MAINT. FUNCTION.   *00041545
04196 *   THE PROGRAM ASSUMES THAT IF A MATCH ON THE READ FOR UPDATE   *00041546
04197 *   IS SUCCESSFUL, THE RECORD HAS PREVIOUSLY BEEN DISPLAYED      *00041547
04198 *   THRUOUGH A BROWSE.  CHANGES ARE APPLIED AND THE PB-RECORD IS *00041548
04199 *   REWRITEN, ELSE A NEW PB-RECORD IS ADDED.                     *00041549
04200 *                                                                *00041550
04201 ******************************************************************00041551
04202                                                                   00041552
04203      MOVE PI-COMPANY-CD          TO ERPNDT-COMP-CD.               00041553
04204      MOVE PI-SAV-ENTRY-BATCH     TO ERPNDT-ENTRY-BATCH.           00041554
04205      MOVE CSEQ (WS-SUB2)         TO ERPNDT-BATCH-SEQ.             00041555
04206                                                                   00041556
04207      EXEC CICS HANDLE CONDITION                                   00041557
04208          NOTFND (5100-ADD-CANCEL-RECORD)                          00041558
04209          END-EXEC.                                                00041559
04210                                                                   00041560
04211      EXEC CICS READ                                               00041561
04212          SET     (ERPNDT-POINTER)                                 00041562
04213          DATASET (FILE-ID-ERPNDT)                                 00041563
04214          RIDFLD  (ERPNDT-KEY)                                     00041564
04215          UPDATE                                                   00041565
04216          END-EXEC.                                                00041566
04217                                                                   00041567
04218      SERVICE RELOAD PENDING-BUSINESS.                             00041568
04219                                                                   00041569
04220      MOVE 'B'                    TO JP-RECORD-TYPE                00041570
04221      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00041571
04222      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     00041572
04223      MOVE FILE-ID-ERPNDT         TO JP-FILE-ID.                   00041573
04224                                                                   00041574
04225      PERFORM 8400-LOG-JOURNAL-RECORD.                             00041575
04226                                                                   00041576
04227      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             00041577
04228      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         00041578
04229      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.             00041579
04230                                                                   00041580
04231      IF CSFX-LEN  (WS-SUB2) GREATER THAN ZEROS                    00041581
04232         MOVE CSFX (WS-SUB2)      TO PB-CERT-SFX.                  00041582
04233                                                                   00041583
04234      IF CLAST-NAME-LEN (WS-SUB2) GREATER THAN ZEROS               00041584
04235          MOVE CLAST-NAME (WS-SUB2)  TO PB-C-LAST-NAME.            00041585
04236                                                                   00041586
04237      IF CREFUND1-LEN   (WS-SUB2) GREATER THAN ZEROS               00041587
04238          IF WS-CREFUND1 (WS-SUB2) = WS-ALL-NINES OR               00041588
04239             WS-CREFUND1 (WS-SUB2) GREATER THAN WS-ALL-NINES       00041589
04240             SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED    00041590
04241             MOVE ZEROS            TO PB-C-LF-CANCEL-AMT           00041591
04242             MOVE '?'              TO PB-C-LF-CALC-REQ             00041592
04243          ELSE                                                     00041593
04244             SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED    00041594
04245             ADD WS-CREFUND1  (WS-SUB2) TO PI-LF-CAN-ENTERED       00041595
04246             MOVE WS-CREFUND1 (WS-SUB2) TO PB-C-LF-CANCEL-AMT      00041596
04247             MOVE SPACE                 TO PB-C-LF-CALC-REQ.       00041597
04248                                                                   00041598
04249      IF CREFUND2-LEN   (WS-SUB2) GREATER THAN ZEROS               00041599
04250          IF WS-CREFUND2 (WS-SUB2) = WS-ALL-NINES OR               00041600
04251             WS-CREFUND2 (WS-SUB2) GREATER THAN WS-ALL-NINES       00041601
04252             SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED    00041602
04253             MOVE ZEROS            TO PB-C-AH-CANCEL-AMT           00041603
04254             MOVE '?'              TO PB-C-AH-CALC-REQ             00041604
04255          ELSE                                                     00041605
04256             SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED    00041606
04257             ADD WS-CREFUND2  (WS-SUB2) TO PI-AH-CAN-ENTERED       00041607
04258             MOVE WS-CREFUND2 (WS-SUB2) TO PB-C-AH-CANCEL-AMT      00041608
04259             MOVE SPACE                 TO PB-C-AH-CALC-REQ.       00041609
04260                                                                   00041610
04261 ******************************************************************00041611
04262 *      IF CANCEL DATE = SPACES (LOW-VALUES) DELETE COVERAGE.     *00041612
04263 ******************************************************************00041613
04264                                                                   00042640
04265      IF CCANDT1-LEN (WS-SUB2) GREATER THAN ZEROS                  00042641
04266         MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO PB-C-LF-CANCEL-DT   00042642
04267         IF   WS-CONVERTED-CANDT1 (WS-SUB2) = LOW-VALUES           00042643
04268              SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED   00042644
04269              MOVE ZEROS          TO PB-C-LF-REF-CALC              00042645
04270                                     PB-C-LF-CANCEL-AMT.           00042646
04271                                                                   00042647
04272      IF CCANDT2-LEN (WS-SUB2) GREATER THAN ZEROS                  00042648
04273         MOVE WS-CONVERTED-CANDT2 (WS-SUB2) TO PB-C-AH-CANCEL-DT   00042649
04274         IF   WS-CONVERTED-CANDT2 (WS-SUB2) = LOW-VALUES           00042650
04275              SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED   00042651
04276              MOVE ZEROS          TO PB-C-AH-REF-CALC              00042652
04277                                     PB-C-AH-CANCEL-AMT.           00042653
04278                                                                   00042780
04279      IF CLIVES-LEN  (WS-SUB2) GREATER THAN ZEROS                  00042781
04280         MOVE WS-CLIVES (WS-SUB2) TO PB-C-LIVES.                   00042782
04281                                                                   00042783
04282      IF CPAYEE-LEN  (WS-SUB2) GREATER THAN ZEROS                  00042784
04283         MOVE CPAYEE (WS-SUB2)    TO PB-C-PAYEE-CODE.              00042785
04284                                                                   00042786
04285      IF CCHK-LEN    (WS-SUB2) GREATER THAN ZEROS                  00042787
04286         MOVE CCHK   (WS-SUB2)    TO PB-C-REFUND-SW.               00042788
04287      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          00042789
04288      MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.            00042790
04289                                                                   00042890
04290      MOVE 'C'                    TO JP-RECORD-TYPE.               00042891
04291      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00042892
04292      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     00042893
04293      MOVE FILE-ID-ERPNDT         TO JP-FILE-ID.                   00042894
04294                                                                   00042895
04295      EXEC CICS REWRITE                                            00042896
04296          DATASET (FILE-ID-ERPNDT)                                 00042897
04297          FROM    (PENDING-BUSINESS)                               00042898
04298          END-EXEC.                                                00042899
04299                                                                   00042900
04300      MOVE ERPNDT-KEY             TO PI-SAV-ENDING-ERPNDT-KEY.     00042901
04301                                                                   00042902
04302      PERFORM 8400-LOG-JOURNAL-RECORD.                             00042903
04303                                                                   00043030
04304      MOVE LOW-VALUES             TO DATA-AREA-C (WS-SUB2).        00043031
04305                                                                   00043050
04306      IF EIBAID = DFHENTER                                         00043051
04307          MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ (WS-SUB2)            00043052
04308          ADD +1 TO               PI-NEXT-DISPLAY-SEQ-NO           00043053
04309          MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).     00043054
04310                                                                   00043055
04311      GO TO 5900-EXIT.                                             00043056
04312                                                                   00043120
04313      EJECT                                                        00043121
04314                                                                   00043122
04315 ******************************************************************00043123
04316 *                                                                *00043124
04317 *            B U I L D   C A N C E L   R E C O R D               *00043125
04318 *                                                                *00043126
04319 ******************************************************************00043127
04320                                                                   00043128
04321  5100-ADD-CANCEL-RECORD.                                          00043129
04322                                                                   00043130
04323      EXEC CICS GETMAIN                                            00043131
04324          SET     (ERPNDT-POINTER)                                 00043132
04325          LENGTH  (ERPNDT-RECORD-LENGTH)                           00043133
04326          INITIMG (GETMAIN-SPACE)                                  00043134
04327          END-EXEC.                                                00043135
04328                                                                   00043136
04329      SERVICE RELOAD PENDING-BUSINESS.                             00043137
04330                                                                   00043138
04331      MOVE 'PB'                   TO PB-RECORD-ID.                 00043139
04332      MOVE PI-COMPANY-CD          TO PB-COMPANY-CD                 00043140
04333                                     PB-COMPANY-CD-A1.             00043141
04334      MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.                00043142
04335      MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH.               00043143
04336      MOVE CSEQ (WS-SUB2)            TO PB-BATCH-SEQ-NO.           00043144
04337                                                                   00043145
04338      IF CSEQ (WS-SUB2) GREATER THAN PI-LAST-SEQ-NO-ADDED          00043146
04339         MOVE CSEQ (WS-SUB2)      TO PI-LAST-SEQ-NO-ADDED.         00043147
04340                                                                   00043148
04341      IF CCAR-LEN  (WS-SUB2) GREATER THAN ZEROS                    00043149
04342         MOVE CCAR (WS-SUB2)      TO PB-CARRIER.                   00043150
04343                                                                   00043151
04344      IF CGRP-LEN  (WS-SUB2) GREATER THAN ZEROS                    00043152
04345         MOVE CGRP (WS-SUB2)      TO PB-GROUPING.                  00043153
04346                                                                   00043154
04347      IF CST-LEN  (WS-SUB2) GREATER THAN ZEROS                     00043155
04348         MOVE CST (WS-SUB2)       TO PB-STATE.                     00043156
04349                                                                   00043157
04350      IF CACCT-LEN     (WS-SUB2) GREATER THAN ZEROS                00043158
04351         MOVE CACCT    (WS-SUB2)  TO PB-ACCOUNT.                   00043159
04352                                                                   00043160
04353      MOVE '2'                    TO PB-RECORD-TYPE.               00043161
04354      MOVE CCERT (WS-SUB2)        TO PB-CERT-PRIME.                00043162
04355      MOVE WS-CONVERTED-EFFDT     TO PB-CERT-EFF-DT.               00043163
04356                                                                   00043164
04357      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO           00043165
04358                                     PB-ALT-CHG-SEQ-NO.            00043166
04359                                                                   00043167
04360                                                                   00043168
04361      MOVE ZEROS                  TO PB-C-LF-REF-CALC              00043169
04362                                     PB-C-AH-REF-CALC              00043170
04363                                     PB-CI-INSURED-AGE             00043171
04364                                     PB-CI-LF-TERM                 00043172
04365                                     PB-CI-AH-TERM                 00043173
04366                                     PB-CI-LF-BENEFIT-CD           00043174
04367                                     PB-CI-LF-BENEFIT-AMT          00043175
04368                                     PB-CI-LF-ALT-BENEFIT-AMT      00043176
04369                                     PB-CI-LF-PREMIUM-AMT          00043177
04370                                     PB-CI-LF-ALT-PREMIUM-AMT      00043178
04371                                     PB-CI-AH-BENEFIT-CD           00043179
04372                                     PB-CI-AH-BENEFIT-AMT          00043180
04373                                     PB-CI-AH-PREMIUM-AMT          00043181
04374                                     PB-CI-PAY-FREQUENCY           00043182
04375                                     PB-CI-LOAN-APR                00043183
04376                                     PB-CI-LOAN-TERM               00043184
04377                                     PB-CI-LIFE-COMMISSION         00043185
04378                                     PB-CI-AH-COMMISSION           00043186
04379                                     PB-CI-CURR-SEQ                00043187
04380                                     PB-CI-AH-CANCEL-AMT           00043188
04381                                     PB-CI-LF-CANCEL-AMT           00043189
04382                                     PB-CI-RATE-DEV-PCT-LF         00043190
04383                                     PB-CI-RATE-DEV-PCT-AH         00043191
04384                                     PB-CI-EXTENTION-DAYS          00043192
04385                                     PB-CI-TERM-IN-DAYS            00043193
04386                                     PB-CI-LIVES                   00043194
04387                                     PB-CI-LF-CRIT-PER             00043195
04388                                     PB-CI-AH-CRIT-PER             00043196
04389                                     PB-C-LF-REM-TERM              00043197
04390                                     PB-C-AH-REM-TERM              00043198
04391                                     PB-CHG-COUNT                  00043199
04392                                     PB-LF-BILLED-AMTS             00043200
04393                                     PB-AH-BILLED-AMTS             00043201
04394                                     PB-CALC-TOLERANCE.            00043202
04395                                                                   00043203
04396      MOVE LOW-VALUES             TO PB-CI-AH-PAID-THRU-DT         00043204
04397                                     PB-CI-AH-SETTLEMENT-DT        00043205
04398                                     PB-CI-DEATH-DT                00043206
04399                                     PB-CI-LF-PRIOR-CANCEL-DT      00043207
04400                                     PB-CI-AH-PRIOR-CANCEL-DT      00043208
04401                                     PB-CI-ENTRY-DT                00043209
04402                                     PB-CI-LF-EXPIRE-DT            00043210
04403                                     PB-CI-AH-EXPIRE-DT            00043211
04404                                     PB-CI-LOAN-1ST-PMT-DT         00043212
04405                                     PB-C-LF-CANCEL-DT             00043213
04406                                     PB-C-AH-CANCEL-DT             00043214
04407                                     PB-CREDIT-ACCEPT-DT           00043215
04408                                     PB-BILLED-DT                  00043216
04409                                     PB-ACCT-EFF-DT                00043217
04410                                     PB-ACCT-EXP-DT.               00043218
04411                                                                   00043219
04412      IF PI-NB-MONTH-END-DT NOT = SPACES                           00043220
04413         MOVE PI-NB-MONTH-END-DT  TO PB-CREDIT-SELECT-DT           00043221
04414        ELSE                                                       00043222
04415         MOVE PI-CR-MONTH-END-DT  TO PB-CREDIT-SELECT-DT.          00043223
04416                                                                   00043224
04417      MOVE 'X'                    TO PB-FATAL-FLAG.                00043225
04418                                                                   00043226
04419                                                                   00043227
04420      IF CSFX-LEN  (WS-SUB2) GREATER THAN ZEROS                    00043228
04421         MOVE CSFX (WS-SUB2)      TO PB-CERT-SFX.                  00043229
04422                                                                   00043230
04423      IF CLAST-NAME-LEN (WS-SUB2) GREATER THAN ZEROS               00043231
04424          MOVE CLAST-NAME (WS-SUB2)  TO PB-C-LAST-NAME.            00043232
04425                                                                   00043233
04426      IF CCANDT1-LEN (WS-SUB2) GREATER THAN ZEROS                  00043234
04427         MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO PB-C-LF-CANCEL-DT.  00043235
04428                                                                   00044280
04429      IF CCANDT2-LEN (WS-SUB2) GREATER THAN ZEROS                  00044281
04430         MOVE WS-CONVERTED-CANDT2 (WS-SUB2) TO PB-C-AH-CANCEL-DT.  00044282
04431                                                                   00044283
04432      IF CREFUND1-LEN   (WS-SUB2) GREATER THAN ZEROS               00044284
04433          IF WS-CREFUND1 (WS-SUB2) = WS-ALL-NINES OR               00044285
04434             WS-CREFUND1 (WS-SUB2) GREATER THAN WS-ALL-NINES       00044286
04435             MOVE ZEROS            TO PB-C-LF-CANCEL-AMT           00044287
04436             MOVE '?'              TO PB-C-LF-CALC-REQ             00044288
04437          ELSE                                                     00044289
04438             ADD  WS-CREFUND1  (WS-SUB2) TO PI-LF-CAN-ENTERED      00044290
04439             MOVE WS-CREFUND1  (WS-SUB2) TO PB-C-LF-CANCEL-AMT     00044291
04440      ELSE                                                         00044292
04441          MOVE ZEROS            TO PB-C-LF-CANCEL-AMT.             00044293
04442                                                                   00044294
04443      IF CREFUND2-LEN   (WS-SUB2) GREATER THAN ZEROS               00044295
04444          IF WS-CREFUND2 (WS-SUB2) = WS-ALL-NINES OR               00044296
04445             WS-CREFUND2 (WS-SUB2) GREATER THAN WS-ALL-NINES       00044297
04446             MOVE ZEROS            TO PB-C-AH-CANCEL-AMT           00044298
04447             MOVE '?'              TO PB-C-AH-CALC-REQ             00044299
04448          ELSE                                                     00044300
04449             ADD  WS-CREFUND2  (WS-SUB2) TO PI-AH-CAN-ENTERED      00044301
04450             MOVE WS-CREFUND2  (WS-SUB2) TO PB-C-AH-CANCEL-AMT     00044302
04451      ELSE                                                         00044303
04452          MOVE ZEROS              TO PB-C-AH-CANCEL-AMT.           00044304
04453                                                                   00044305
04454                                                                   00044306
04455      IF CLIVES-LEN  (WS-SUB2) GREATER THAN ZEROS                  00044307
04456         MOVE WS-CLIVES (WS-SUB2) TO PB-C-LIVES                    00044308
04457      ELSE                                                         00044309
04458         MOVE ZEROS               TO PB-C-LIVES.                   00044310
04459                                                                   00044590
04460      IF CPAYEE-LEN  (WS-SUB2) GREATER THAN ZEROS                  00044591
04461         MOVE CPAYEE (WS-SUB2)    TO PB-C-PAYEE-CODE.              00044592
04462                                                                   00044593
04463      IF CCHK-LEN    (WS-SUB2) GREATER THAN ZEROS                  00044594
04464         MOVE CCHK   (WS-SUB2)    TO PB-C-REFUND-SW.               00044595
04465                                                                   00044650
04466      MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.          00044651
04467      MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.            00044652
04468                                                                   00044653
04469      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY              00044654
04470                                     PB-INPUT-BY.                  00044655
04471      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         00044656
04472      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT              00044657
04473                                     PB-INPUT-DT.                  00044658
04474      MOVE 'A'                    TO JP-RECORD-TYPE.               00044659
04475      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00044660
04476      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     00044661
04477      MOVE FILE-ID-ERPNDT         TO JP-FILE-ID.                   00044662
04478      MOVE PB-CONTROL-PRIMARY     TO PI-SAV-ENDING-ERPNDT-KEY.     00044663
04479      ADD +1                      TO PI-SAV-BATCH-SEQ.             00044664
04480                                                                   00044665
04481      EXEC CICS HANDLE CONDITION                                   00044666
04482          DUPREC (5200-DUPLICATE-ALT-INDEX)                        00044667
04483          END-EXEC.                                                00044668
04484                                                                   00044669
04485      EXEC CICS WRITE                                              00044670
04486          DATASET (FILE-ID-ERPNDT)                                 00044671
04487          FROM    (PENDING-BUSINESS)                               00044672
04488          RIDFLD  (PB-CONTROL-PRIMARY)                             00044673
04489          END-EXEC.                                                00044674
04490                                                                   00044900
04491      ADD +1                      TO PI-CAN-CNT-ENTERED.           00044901
04492                                                                   00044902
04493      PERFORM 8400-LOG-JOURNAL-RECORD.                             00044903
04494                                                                   00044904
04495      MOVE LOW-VALUES             TO DATA-AREA-C (WS-SUB2).        00044905
04496      MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ        (WS-SUB2).        00044906
04497      MOVE AL-SABON               TO CSEQ-ATTRB  (WS-SUB2).        00044907
04498                                                                   00044908
04499      ADD +1 TO                   PI-NEXT-DISPLAY-SEQ-NO.          00044909
04500                                                                   00044910
04501      GO TO 5900-EXIT.                                             00044911
04502                                                                   00044912
04503  5200-DUPLICATE-ALT-INDEX.                                        00044913
04504                                                                   00044914
04505      MOVE ER-2247                TO EMI-ERROR.                    00044915
04506      MOVE -1                     TO CCERT-LEN    (WS-SUB2).       00044916
04507      MOVE AL-UABON               TO CCERT-ATTRB  (WS-SUB2).       00044917
04508      MOVE AL-UNBON               TO CEFFDT-ATTRB (WS-SUB2).       00044918
04509      MOVE 'Y'                    TO PI-ERROR-SW.                  00044919
04510                                                                   00045100
04511      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00045101
04512                                                                   00045102
04513      IF CREFUND1-LEN (WS-SUB2) GREATER THAN ZEROS                 00045103
04514          SUBTRACT WS-CREFUND1 (WS-SUB2) FROM PI-LF-CAN-ENTERED.   00045104
04515                                                                   00045150
04516      IF CREFUND2-LEN (WS-SUB2) GREATER THAN ZEROS                 00045151
04517          SUBTRACT WS-CREFUND2 (WS-SUB2) FROM PI-AH-CAN-ENTERED.   00045152
04518                                                                   00045153
04519  5900-EXIT.                                                       00045154
04520                                                                   00045155
04521      EXIT.                                                        00045156
04522                                                                   00045157
04523      EJECT                                                        00045158
04524                                                                   00045159
04525 ******************************************************************00045160
04526 *                                                                *00045161
04527 *   D E L E T E  P E N D I N G   B U S I N E S S    R E C O R D  *00045162
04528 *                                                                *00045163
04529 ******************************************************************00045290
04530                                                                   00045291
04531  6000-DELETE-PEND-BUS-RECORD.                                     00045292
04532                                                                   00045293
04533      MOVE PI-COMPANY-CD          TO ERPNDT-COMP-CD.               00045294
04534      MOVE PI-SAV-ENTRY-BATCH     TO ERPNDT-ENTRY-BATCH.           00045295
04535                                                                   00045296
04536      IF PI-MAP-NAME = EL930B                                      00045297
04537          MOVE BSEQ               TO ERPNDT-BATCH-SEQ              00045298
04538      ELSE                                                         00045299
04539          MOVE CSEQ (1)           TO ERPNDT-BATCH-SEQ.             00045300
04540                                                                   00045301
04541      EXEC CICS HANDLE CONDITION                                   00045302
04542          NOTFND (6990-REC-NOTFND)                                 00045303
04543          END-EXEC.                                                00045304
04544                                                                   00045305
04545      EXEC CICS READ                                               00045306
04546          SET     (ERPNDT-POINTER)                                 00045307
04547          DATASET (FILE-ID-ERPNDT)                                 00045308
04548          RIDFLD  (ERPNDT-KEY)                                     00045309
04549          UPDATE                                                   00045310
04550          END-EXEC.                                                00045311
04551                                                                   00045510
04552      SERVICE RELOAD PENDING-BUSINESS.                             00045511
04553                                                                   00045512
04554 ******************************************************************00045513
04555 *                                                                *00045514
04556 *    PENDING BUSINESS RECORD CAN NOT BE DELETED THROUGH DATA     *00045515
04557 *    ENTRY IF THE RECORD HAS BEEN EDITED.  IF THE RECORD HAS     *00045516
04558 *    BEEN EDITED, THE CURRENT BUSINESS RECORD CAN ONLY BE DELETED*00045517
04559 *    THRUOUGH REVIEW AND CORRECTION.                             *00045518
04560 *                                                                *00045519
04561 ******************************************************************00045520
04562                                                                   00045521
04563      IF  PB-ACCT-EFF-DT = LOW-VALUES NEXT SENTENCE                00045522
04564         ELSE                                                      00045523
04565          GO TO 6880-DELETE-ERROR.                                 00045524
04566                                                                   00045525
04567      IF PB-ISSUE                                                  00045526
04568          SUBTRACT PB-I-LF-PREMIUM-AMT     FROM PI-LF-ISS-ENTERED  00045527
04569          SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED  00045528
04570          SUBTRACT PB-I-AH-PREMIUM-AMT     FROM PI-AH-ISS-ENTERED  00045529
04571          SUBTRACT +1 FROM PI-ISS-CNT-ENTERED                      00045530
04572      ELSE                                                         00045531
04573          SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED       00045532
04574          SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED       00045533
04575          SUBTRACT +1 FROM PI-CAN-CNT-ENTERED.                     00045534
04576                                                                   00045535
04577                                                                   00045536
04578  6300-DELETE-PB-RECORD.                                           00045537
04579                                                                   00045538
04580      MOVE 'D'                    TO JP-RECORD-TYPE.               00045539
04581      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.               00045540
04582      MOVE ERPNDT-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.     00045541
04583      MOVE FILE-ID-ERPNDT         TO JP-FILE-ID.                   00045542
04584                                                                   00045840
04585      EXEC CICS DELETE                                             00045841
04586          DATASET (FILE-ID-ERPNDT)                                 00045842
04587          END-EXEC.                                                00045843
04588                                                                   00045844
04589      PERFORM 8400-LOG-JOURNAL-RECORD.                             00045845
04590                                                                   00045900
04591      MOVE 'Y'                    TO PI-UPDATE-SW.                 00045901
04592      MOVE ER-0000                TO EMI-ERROR                     00045902
04593                                                                   00045903
04594      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00045904
04595                                                                   00045905
04596      ADD +1, PI-LAST-SEQ-NO-ADDED                                 00045906
04597                      GIVING PI-NEXT-DISPLAY-SEQ-NO.               00045907
04598                                                                   00045908
04599      IF PI-MAP-NAME = EL930B                                      00045909
04600          MOVE LOW-VALUES         TO MAP-B                         00045910
04601          PERFORM 8550-SET-MAP-SEQ-NOS                             00045911
04602      ELSE                                                         00045912
04603          MOVE LOW-VALUES         TO MAP-C                         00045913
04604          PERFORM 8550-SET-MAP-SEQ-NOS                             00045914
04605                  VARYING WS-SUB2 FROM +1 BY +1                    00045915
04606                  UNTIL WS-SUB2 GREATER THAN +2.                   00045916
04607                                                                   00045917
04608      GO TO 8100-SEND-INITIAL-MAP.                                 00045918
04609                                                                   00045919
04610  6880-DELETE-ERROR.                                               00045920
04611                                                                   00045921
04612      EXEC CICS UNLOCK                                             00045922
04613           DATASET (FILE-ID-ERPNDT)                                00045923
04614           END-EXEC.                                               00045924
04615                                                                   00045925
04616      MOVE ER-2901        TO EMI-ERROR.                            00045926
04617      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00045927
04618      IF PI-MAP-NAME = EL930B                                      00045928
04619          MOVE -1                 TO BPFENTRL                      00045929
04620      ELSE                                                         00045930
04621          MOVE -1                 TO CPFENTRL.                     00045931
04622                                                                   00046220
04623      GO TO 8200-SEND-DATAONLY.                                    00046221
04624                                                                   00046222
04625  6990-REC-NOTFND.                                                 00046223
04626                                                                   00046224
04627      MOVE ER-2433                TO EMI-ERROR                     00046225
04628                                                                   00046280
04629      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00046281
04630                                                                   00046282
04631      IF PI-MAP-NAME = EL930B                                      00046283
04632          MOVE -1                 TO BPFENTRL                      00046284
04633      ELSE                                                         00046285
04634          MOVE -1                 TO CPFENTRL.                     00046286
04635                                                                   00046287
04636      GO TO 8200-SEND-DATAONLY.                                    00046288
04637                                                                   00046370
04638      EJECT                                                        00046371
04639                                                                   00046372
04640 ******************************************************************00046373
04641 *                                                                *00046374
04642 *           F O R M A T   I S S U E   S C R E E N                *00046375
04643 *                                                                *00046376
04644 ******************************************************************00046377
04645                                                                   00046378
04646  7000-FORMAT-ISSUE-SCREEN.                                        00046379
04647                                                                   00046470
04648      MOVE 'Y'                        TO PI-DISPLAY-SW.            00046471
04649      MOVE LOW-VALUES                 TO DATA-AREA-B.              00046472
04650      MOVE -1                         TO BPFENTRL.                 00046473
04651      MOVE PB-BATCH-SEQ-NO            TO BSEQ.                     00046474
04652      MOVE AL-SABON                   TO BSEQ-ATTRB.               00046475
04653                                                                   00046530
04654      MOVE PB-CARRIER                 TO BCAR.                     00046531
04655      MOVE AL-SANOF                   TO BCAR-ATTRB.               00046532
04656      MOVE PB-GROUPING                TO BGRP.                     00046533
04657      MOVE AL-SANOF                   TO BGRP-ATTRB.               00046534
04658      MOVE PB-STATE                   TO BST.                      00046535
04659      MOVE AL-SANOF                   TO BST-ATTRB.                00046536
04660      MOVE PB-ACCOUNT                 TO BACCT.                    00046537
04661      MOVE AL-SANOF                   TO BACCT-ATTRB.              00046538
04662                                                                   00046620
04663      MOVE PB-CERT-PRIME              TO BCERT.                    00046621
04664      MOVE AL-SANON                   TO BCERT-ATTRB.              00046622
04665      MOVE PB-CERT-SFX                TO BSFX.                     00046623
04666      MOVE AL-SANOF                   TO BSFX-ATTRB.               00046624
04667      MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.            00046625
04668      MOVE SPACE                      TO DC-OPTION-CODE.           00046626
04669      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    00046627
04670      MOVE DC-GREG-DATE-1-MDY         TO BEFFDT.                   00046628
04671      MOVE AL-SANOF                   TO BEFFDT-ATTRB.             00046629
04672                                                                   00046630
04673      MOVE PI-LIFE-OVERRIDE-L2        TO BKIND (1).                00046631
04674      MOVE PI-AH-OVERRIDE-L2          TO BKIND(2).                 00046632
04675                                                                   00046633
04676      IF PB-I-INSURED-LAST-NAME GREATER THAN SPACES                00046634
04677         MOVE PB-I-INSURED-LAST-NAME  TO BLAST-NAME.               00046635
04678                                                                   00046636
04679      IF PB-I-INSURED-FIRST-NAME GREATER THAN SPACES               00046637
04680         MOVE PB-I-INSURED-FIRST-NAME TO B1ST-NAME.                00046638
04681                                                                   00046639
04682      IF PB-I-INSURED-MIDDLE-INIT GREATER THAN SPACES              00046640
04683         MOVE PB-I-INSURED-MIDDLE-INIT TO BINIT.                   00046641
04684                                                                   00046642
04685      IF PB-I-AGE GREATER THAN ZEROS                               00046643
04686         MOVE PB-I-AGE                TO BAGE.                     00046644
04687                                                                   00046645
04688      IF PB-I-JOINT-AGE GREATER THAN ZEROS                         00046646
04689         MOVE PB-I-JOINT-AGE          TO BJNT-AGE.                 00046647
04690                                                                   00046648
04691      IF PB-I-BIRTHDAY NOT = LOW-VALUES                            00046649
04692         MOVE PB-I-BIRTHDAY           TO DC-BIN-DATE-1             00046650
04693         MOVE SPACE                   TO DC-OPTION-CODE            00046651
04694         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  00046652
04695         MOVE DC-GREG-DATE-1-MDY      TO BBIRTH-DT.                00046653
04696                                                                   00046654
04697      IF PB-I-INSURED-SEX GREATER THAN SPACES                      00046655
04698         MOVE PB-I-INSURED-SEX        TO BSEX.                     00046656
04699                                                                   00046657
04700      IF PB-I-LF-TERM GREATER THAN ZEROS                           00046658
04701         MOVE PB-I-LF-TERM            TO BTERMO (1).               00046659
04702                                                                   00046660
04703      IF PB-I-AH-TERM GREATER THAN ZEROS                           00046661
04704         MOVE PB-I-AH-TERM            TO BTERMO (2).               00046662
04705                                                                   00046663
04706      IF PB-I-LOAN-TERM GREATER THAN ZEROS                         00046664
04707         MOVE PB-I-LOAN-TERM          TO BLN-TERMO.                00046665
04708                                                                   00046666
04709      IF PB-I-PAY-FREQUENCY GREATER THAN ZEROS                     00046667
04710         MOVE PB-I-PAY-FREQUENCY      TO BFREQO.                   00046668
04711                                                                   00046669
04712      IF PB-I-SKIP-CODE GREATER THAN SPACES                        00046670
04713         MOVE PB-I-SKIP-CODE          TO BSKPCD.                   00046671
04714                                                                   00046672
04715      IF PB-I-TERM-TYPE GREATER THAN SPACES                        00046673
04716         MOVE PB-I-TERM-TYPE          TO BMODE.                    00046674
04717                                                                   00046675
04718      IF PB-I-NO-OF-PAYMENTS GREATER THAN ZEROS                    00046676
04719         MOVE PB-I-NO-OF-PAYMENTS     TO BPMTS-OUT.                00046677
04720                                                                   00046678
04721      IF PB-I-POLICY-FORM-NO GREATER THAN SPACES                   00046679
04722         MOVE PB-I-POLICY-FORM-NO     TO BPOLICY.                  00046680
04723                                                                   00046681
04724      IF PB-I-LF-INPUT-CD GREATER THAN SPACES                      00046682
04725         MOVE PB-I-LF-INPUT-CD        TO BTYPE (1).                00046683
04726                                                                   00046684
04727      IF PB-I-LF-BENEFIT-AMT GREATER THAN ZEROS                    00046685
04728         MOVE PB-I-LF-BENEFIT-AMT     TO BBENO (1).                00046686
04729                                                                   00046687
04730      IF PB-I-LF-ALT-BENEFIT-AMT GREATER THAN ZEROS                00046688
04731         MOVE PB-I-LF-ALT-BENEFIT-AMT TO BALT-BENO (1).            00046689
04732                                                                   00046690
04733      IF PB-I-LF-PREMIUM-AMT GREATER THAN ZEROS                    00046691
04734         MOVE PB-I-LF-PREMIUM-AMT     TO BPREMO (1).               00046692
04735                                                                   00046693
04736      IF PB-I-LF-ALT-PREMIUM-AMT GREATER THAN ZEROS                00046694
04737         MOVE PB-I-LF-ALT-PREMIUM-AMT TO BALT-PREMO (1).           00046695
04738                                                                   00046696
04739      IF PB-I-AH-INPUT-CD GREATER THAN SPACES                      00046697
04740         MOVE PB-I-AH-INPUT-CD        TO BTYPE (2).                00046698
04741                                                                   00046699
04742      IF PB-I-AH-BENEFIT-AMT GREATER THAN ZEROS                    00046700
04743         MOVE PB-I-AH-BENEFIT-AMT     TO BBENO (2).                00046701
04744                                                                   00046702
04745      IF PB-I-AH-PREMIUM-AMT GREATER THAN ZEROS                    00046703
04746         MOVE PB-I-AH-PREMIUM-AMT     TO BPREMO (2).               00046704
04747                                                                   00046705
04748      IF PB-I-LF-CRIT-PER GREATER THAN ZEROS                       00046706
04749         MOVE PB-I-LF-CRIT-PER        TO BCRIT-PERDO (1).          00046707
04750                                                                   00046708
04751      IF PB-I-AH-CRIT-PER GREATER THAN ZEROS                       00046709
04752         MOVE PB-I-AH-CRIT-PER           TO BCRIT-PERDO (2).       00046710
04753                                                                   00046711
04754      IF PB-BATCH-ENTRY GREATER THAN SPACES                        00046712
04755         MOVE PB-BATCH-ENTRY          TO BENTRY.                   00046713
04756                                                                   00046714
04757      IF PB-I-SPECIAL-REIN-CODE GREATER THAN SPACE                 00046715
04758        MOVE PB-I-SPECIAL-REIN-CODE   TO BRINCD.                   00046716
04759                                                                   00046717
04760      IF PB-I-INDV-GRP-OVRD GREATER THAN SPACES                    00046718
04761         MOVE PB-I-INDV-GRP-OVRD      TO BIND-GRP.                 00046719
04762                                                                   00046720
04763      IF PB-I-RATE-CLASS-OVRD GREATER THAN SPACES                  00046721
04764         MOVE PB-I-RATE-CLASS-OVRD    TO BRTCLS.                   00046722
04765                                                                   00046723
04766      IF PB-I-SIG-SW GREATER THAN SPACES                           00046724
04767          MOVE PB-I-SIG-SW            TO BSIG.                     00046725
04768                                                                   00046726
04769      IF PB-I-LOAN-APR GREATER THAN ZEROS                          00046727
04770         MOVE PB-I-LOAN-APR           TO BAPR-OUT.                 00046728
04771                                                                   00046729
04772      IF PB-I-SOC-SEC-NO GREATER THAN SPACES                       00046730
04773         MOVE PB-I-SOC-SEC-NO         TO BSSNUM.                   00046731
04774                                                                   00046732
04775      IF PB-I-MEMBER-NO GREATER THAN SPACES                        00046733
04776         MOVE PB-I-MEMBER-NO          TO BMEM-NO.                  00046734
04777                                                                   00046735
04778      IF PB-I-LOAN-OFFICER GREATER THAN SPACES                     00046736
04779         MOVE PB-I-LOAN-OFFICER       TO BLN-OFFICER.              00046737
04780                                                                   00046738
04781      IF PB-I-LF-EXPIRE-DT NOT = LOW-VALUES                        00046739
04782         MOVE PB-I-LF-EXPIRE-DT   TO DC-BIN-DATE-1                 00046740
04783         MOVE SPACE               TO DC-OPTION-CODE                00046741
04784         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  00046742
04785         MOVE DC-GREG-DATE-1-MDY  TO BEXPIRE (1).                  00046743
04786                                                                   00046744
04787      IF PB-I-AH-EXPIRE-DT NOT = LOW-VALUES                        00046745
04788         MOVE PB-I-AH-EXPIRE-DT   TO DC-BIN-DATE-1                 00046746
04789         MOVE SPACE               TO DC-OPTION-CODE                00046747
04790         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  00046748
04791         MOVE DC-GREG-DATE-1-MDY  TO BEXPIRE (2).                  00046749
04792                                                                   00046750
04793      IF PB-I-1ST-PMT-DT GREATER THAN LOW-VALUES                   00046751
04794         MOVE PB-I-1ST-PMT-DT     TO DC-BIN-DATE-1                 00046752
04795         MOVE SPACE               TO DC-OPTION-CODE                00046753
04796         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  00046754
04797         MOVE DC-GREG-DATE-1-MDY  TO B1ST-PMT.                     00046755
04798                                                                   00047980
04799      IF PB-I-EXTENTION-DAYS NUMERIC                               00047981
04800         IF PB-I-EXTENTION-DAYS NOT = ZEROS                        00047982
04801            MOVE PB-I-EXTENTION-DAYS  TO BDAYSO                    00047983
04802         ELSE                                                      00047984
04803            IF PB-I-TERM-IN-DAYS NUMERIC                           00047985
04804               IF PB-I-TERM-IN-DAYS NOT = ZEROS                    00047986
04805                  MOVE PB-I-TERM-IN-DAYS TO BDAYSO.                00047987
04806                                                                   00047988
04807      IF PB-I-LIVES GREATER THAN ZEROS                             00047989
04808         MOVE PB-I-LIVES              TO BLIVESO.                  00047990
04809                                                                   00048090
04810      IF PB-I-JOINT-FIRST-NAME GREATER THAN SPACES                 00048091
04811         MOVE PB-I-JOINT-FIRST-NAME   TO BJNT-1ST-NAME.            00048092
04812                                                                   00048093
04813      IF PB-I-JOINT-MIDDLE-INIT GREATER THAN SPACES                00048094
04814         MOVE PB-I-JOINT-MIDDLE-INIT  TO BJNT-INIT.                00048095
04815                                                                   00048096
04816      IF PB-I-JOINT-LAST-NAME GREATER THAN SPACES                  00048097
04817         MOVE PB-I-JOINT-LAST-NAME    TO BJNT-LST-NAME.            00048098
04818                                                                   00048099
04819      IF PB-I-BENEFICIARY-NAME GREATER THAN SPACES                 00048100
04820         MOVE PB-I-BENEFICIARY-NAME   TO BBENEFICIARY.             00048101
04821                                                                   00048102
04822      MOVE PB-CONTROL-PRIMARY         TO ERPNDM-KEY.               00048103
04823                                                                   00048230
04824      EXEC CICS HANDLE CONDITION                                   00048231
04825          NOTFND (7090-EXIT)                                       00048232
04826          END-EXEC.                                                00048233
04827                                                                   00048270
04828      EXEC CICS READ                                               00048271
04829          SET     (ERPNDM-POINTER)                                 00048272
04830          DATASET (FILE-ID-ERPNDM)                                 00048273
04831          RIDFLD  (ERPNDM-KEY)                                     00048274
04832          UPDATE                                                   00048275
04833          END-EXEC.                                                00048276
04834                                                                   00048340
04835      SERVICE RELOAD PENDING-MAILING-DATA.                         00048341
04836                                                                   00048360
04837      IF PM-ADDRESS-LINE-1 GREATER THAN SPACES                     00048361
04838         MOVE PM-ADDRESS-LINE-1       TO BADDRS1.                  00048362
04839                                                                   00048363
04840      IF PM-ADDRESS-LINE-2 GREATER THAN SPACES                     00048364
04841         MOVE PM-ADDRESS-LINE-2       TO BADDRS2.                  00048365
04842                                                                   00048366
04843      IF PM-CITY-STATE GREATER THAN SPACES                         00048367
04844         MOVE PM-CITY-STATE           TO BCITYST.                  00048368
04845                                                                   00048369
04846      IF PM-CANADIAN-POST-CODE                                     00048370
04847         MOVE PM-CAN-POST1            TO BZIP5                     00048371
04848         MOVE PM-CAN-POST2            TO BZIP4                     00048372
04849         MOVE SPACES                  TO BDASH                     00048373
04850                                                                   00048374
04851      ELSE                                                         00048375
04852          IF PM-ZIP-CODE GREATER THAN SPACES                       00048376
04853             MOVE PM-ZIP-CODE         TO BZIP5                     00048377
04854                                                                   00048378
04855             IF  PM-ZIP-PLUS4 GREATER THAN SPACES                  00048379
04856                 MOVE '-'             TO BDASH                     00048380
04857                 MOVE PM-ZIP-PLUS4    TO BZIP4                     00048381
04858                                                                   00048382
04859             ELSE                                                  00048383
04860                 MOVE SPACES          TO BDASH                     00048384
04861                                         BZIP4                     00048385
04862                                                                   00048386
04863         ELSE                                                      00048387
04864             MOVE SPACES              TO BDASH                     00048388
04865                                         BZIP4                     00048389
04866                                         BZIP5.                    00048390
04867                                                                   00048391
04868      IF PM-PHONE-NO NUMERIC                                       00048392
04869         IF PM-PHONE-NO GREATER THAN ZEROS                         00048393
04870            MOVE PM-PHONE-NO          TO  BPHONE-NO                00048394
uktdel*          TRANSFORM BPHONE-NO FROM ' ' TO '-'.                   00048395
uktins           INSPECT BPHONE-NO REPLACING ALL ' ' BY '-'.
04872                                                                   00048396
04873  7090-EXIT.                                                       00048397
04874                                                                   00048398
04875      EXIT.                                                        00048399
04876                                                                   00048400
04877      EJECT                                                        00048401
04878                                                                   00048780
04879 ******************************************************************00048781
04880 *                                                                *00048782
04881 *           F O R M A T   C A N C E L   S C R E E N              *00048783
04882 *                                                                *00048784
04883 ******************************************************************00048785
04884                                                                   00048786
04885  7100-FORMAT-CANCEL-SCREEN.                                       00048787
04886                                                                   00048788
04887      MOVE 'Y'                    TO PI-DISPLAY-SW.                00048789
04888                                                                   00048790
04889      MOVE LOW-VALUES             TO DATA-AREA-C (2)               00048791
uktdel*                                TO DATA-AREA-C (3).              00048792
uktins                                    DATA-AREA-C (3).              00048792
04891                                                                   00048793
04892      MOVE -1                     TO CPFENTRL.                     00048794
04893                                                                   00048795
04894      MOVE PB-BATCH-SEQ-NO        TO CSEQ        (1).              00048796
04895      MOVE AL-SABON               TO CSEQ-ATTRB  (1).              00048797
04896                                                                   00048960
04897      MOVE PB-CARRIER             TO CCAR        (1).              00048961
04898      MOVE AL-SANOF               TO CCAR-ATTRB  (1).              00048962
04899      MOVE PB-GROUPING            TO CGRP        (1).              00048963
04900      MOVE AL-SANOF               TO CGRP-ATTRB  (1).              00048964
04901      MOVE PB-STATE               TO CST         (1).              00048965
04902      MOVE AL-SANOF               TO CST-ATTRB   (1).              00048966
04903      MOVE PB-ACCOUNT             TO CACCT       (1).              00048967
04904      MOVE AL-SANOF               TO CACCT-ATTRB (1).              00048968
04905                                                                   00048969
04906      MOVE PB-CERT-PRIME          TO CCERT       (1).              00048970
04907      MOVE AL-SANON               TO CCERT-ATTRB (1).              00048971
04908      MOVE PB-CERT-SFX            TO CSFX        (1).              00048972
04909      MOVE AL-SANON               TO CSFX-ATTRB  (1).              00048973
04910                                                                   00048974
04911      MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1.                00048975
04912      MOVE SPACE                  TO DC-OPTION-CODE.               00048976
04913      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    00048977
04914      MOVE AL-SANON               TO CEFFDT-ATTRB (1).             00048978
04915      MOVE DC-GREG-DATE-1-MDY     TO CEFFDT       (1).             00048979
04916      MOVE PB-C-LAST-NAME         TO CLAST-NAME   (1).             00048980
04917                                                                   00048981
04918                                                                   00048982
04919      IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES                        00048983
04920         MOVE PB-C-LF-CANCEL-DT   TO DC-BIN-DATE-1                 00048984
04921         MOVE SPACE               TO DC-OPTION-CODE                00048985
04922         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  00048986
04923         MOVE DC-GREG-DATE-1-MDY  TO CCANDT1 (1)                   00048987
04924         MOVE AL-UANON            TO CCANDT1-ATTRB (1).            00048988
04925                                                                   00048989
04926      IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES                        00048990
04927         MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-1                 00048991
04928         MOVE SPACE               TO DC-OPTION-CODE                00048992
04929         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  00048993
04930         MOVE DC-GREG-DATE-1-MDY  TO CCANDT2 (1)                   00048994
04931         MOVE AL-UANON            TO CCANDT2-ATTRB (1).            00048995
04932                                                                   00048996
04933      IF PB-C-LF-CANCEL-AMT NOT =  ZEROS                           00048997
04934         MOVE PB-C-LF-CANCEL-AMT  TO CREFUND1O (1)                 00048998
04935         MOVE AL-UNNON            TO CREFUND1-ATTRB (1).           00048999
04936                                                                   00049360
04937      IF PB-C-AH-CANCEL-AMT NOT =  ZEROS                           00049361
04938         MOVE PB-C-AH-CANCEL-AMT  TO CREFUND2O      (1)            00049362
04939         MOVE AL-UNNON            TO CREFUND2-ATTRB (1).           00049363
04940                                                                   00049364
04941      IF PB-C-LIVES GREATER THAN ZEROS                             00049365
04942         MOVE PB-C-LIVES          TO CLIVESO      (1)              00049366
04943         MOVE AL-UNNON            TO CLIVES-ATTRB (1).             00049367
04944                                                                   00049440
04945      IF PB-C-PAYEE-CODE GREATER THAN SPACES                       00049441
04946         MOVE PB-C-PAYEE-CODE     TO CPAYEE       (1)              00049442
04947         MOVE AL-UANON            TO CPAYEE-ATTRB (1).             00049443
04948                                                                   00049444
04949      IF PB-C-REFUND-SW  GREATER THAN SPACES                       00049445
04950         MOVE PB-C-REFUND-SW      TO CCHK         (1)              00049446
04951         MOVE AL-UANON            TO CCHK-ATTRB   (1).             00049447
04952                                                                   00049520
04953      IF PB-C-LAST-NAME GREATER THAN SPACES                        00049521
04954         MOVE PB-C-LAST-NAME      TO CLAST-NAME       (1)          00049522
04955         MOVE AL-UANON            TO CLAST-NAME-ATTRB (1).         00049523
04956                                                                   00049524
04957      PERFORM 7180-PROTECT-FIELDS VARYING WS-SUB2 FROM +2 BY +1    00049525
04958                                  UNTIL WS-SUB2 GREATER THAN +2.   00049526
04959                                                                   00049527
04960      GO TO 7190-EXIT.                                             00049528
04961                                                                   00049529
04962                                                                   00049530
04963  7180-PROTECT-FIELDS.                                             00049531
04964                                                                   00049532
04965      MOVE AL-SANOF               TO CCERT-ATTRB      (WS-SUB2)    00049533
04966                                     CSFX-ATTRB       (WS-SUB2)    00049534
04967                                     CEFFDT-ATTRB     (WS-SUB2)    00049535
04968                                     CLAST-NAME-ATTRB (WS-SUB2)    00049536
04969                                     CCANDT1-ATTRB    (WS-SUB2)    00049537
04970                                     CCANDT2-ATTRB    (WS-SUB2)    00049538
04971                                     CREFUND1-ATTRB   (WS-SUB2)    00049539
04972                                     CREFUND2-ATTRB   (WS-SUB2)    00049540
04973                                     CLIVES-ATTRB     (WS-SUB2).   00049541
04974                                                                   00049542
04975  7190-EXIT.                                                       00049543
04976                                                                   00049544
04977      EJECT                                                        00049545
04978                                                                   00049546
04979 ******************************************************************00049547
04980 *                                                                *00049548
04981 *    S  E N D    I N I T I A L   M A P   FOR    I S S U E S      *00049549
04982 *                                                                *00049550
04983 ******************************************************************00049551
04984                                                                   00049552
04985  8100-SEND-INITIAL-MAP.                                           00049553
04986      IF PI-MAP-NAME = EL930B                                      00049554
04987          NEXT SENTENCE                                            00049555
04988      ELSE                                                         00049556
04989          GO TO 8110-SEND-INITIAL-CANCEL-MAP.                      00049557
04990                                                                   00049558
04991      MOVE PI-MEMBER-CAPTION        TO BCAPTNO.                    00049559
04992                                                                   00049560
04993      IF EIBAID NOT = DFHPF5                                       00049561
04994        AND EIBAID NOT = DFHPF1                                    00049562
04995        AND EIBAID NOT = DFHPF2                                    00049563
04996        AND PI-MAINT-FUNC NOT = 'B'                                00049564
04997          PERFORM 0600-PROTECT-FIELDS THRU 0600-EXIT.              00049565
04998                                                                   00049566
04999      MOVE PI-SAV-ENTRY-BATCH     TO BBATCHO.                      00049567
05000      MOVE PI-CR-MONTH-END-DT     TO DC-BIN-DATE-1.                00049568
05001      MOVE SPACE                  TO DC-OPTION-CODE.               00049569
05002      PERFORM 8500-DATE-CONVERT.                                   00049570
05003      MOVE WS-CURRENT-DT          TO BDATEO.                       00049571
05004      MOVE EIBTIME                TO TIME-IN.                      00049572
05005      MOVE TIME-OUT               TO BTIMEO.                       00049573
05006                                                                   00049574
05007      MOVE PI-LIFE-OVERRIDE-L2    TO BKIND (1).                    00049575
05008      MOVE AL-SABOF               TO BKIND-ATTRB (1).              00049576
05009      MOVE PI-AH-OVERRIDE-L2      TO BKIND (2).                    00049577
05010      MOVE AL-SABOF               TO BKIND-ATTRB (2).              00049578
05011                                                                   00049579
05012      IF PI-DATA-ERRORS                                            00049580
05013          MOVE SPACE              TO PI-ERROR-SW                   00049581
05014      ELSE                                                         00049582
05015          IF EIBAID = DFHPF1 OR DFHPF2                             00049583
05016              NEXT SENTENCE                                        00049584
05017          ELSE                                                     00049585
05018              IF CARR-GROUP-ST-ACCNT-CNTL                          00049586
05019                  MOVE -1                     TO BCAR-LEN          00049587
05020              ELSE                                                 00049588
05021                  IF ST-ACCNT-CNTL                                 00049589
05022                     MOVE -1                  TO BST-LEN           00049590
05023                     MOVE AL-SADOF            TO BCARHDA           00049591
05024                                                 BGRPHDA           00049592
05025                     MOVE AL-SANOF            TO BCARA             00049593
05026                                                 BGRPA             00049594
05027                  ELSE                                             00049595
05028                     IF CARR-ST-ACCNT-CNTL                         00049596
05029                        MOVE -1               TO BCAR-LEN          00049597
05030                        MOVE AL-SADOF         TO BGRPHDA           00049598
05031                        MOVE AL-SANOF         TO BGRPA             00049599
05032                     ELSE                                          00049600
05033                        IF ACCNT-CNTL                              00049601
05034                           MOVE -1            TO BACCT-LEN         00049602
05035                           MOVE AL-SADOF      TO BCARHDA           00049603
05036                                                 BGRPHDA           00049604
05037                                                 BSTHDA            00049605
05038                           MOVE AL-SANOF      TO BCARA             00049606
05039                                                 BGRPA             00049607
05040                                                 BSTA              00049608
05041                        ELSE                                       00049609
05042                           IF CARR-ACCNT-CNTL                      00049610
05043                              MOVE AL-SADOF   TO BGRPHDA           00049611
05044                                                 BSTHDA            00049612
05045                              MOVE AL-SANOF   TO BGRPA             00049613
05046                                                 BSTA              00049614
05047                              MOVE -1         TO BCAR-LEN.         00049615
05048                                                                   00049616
05049                                                                   00049617
05050      MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O.                     00049618
05051      MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O.                     00049619
05052                                                                   00049620
05053      EXEC CICS SEND                                               00049621
05054          MAP      (PI-MAP-NAME)                                   00049622
05055          MAPSET   (MAPSET-EL9301S)                                00049623
05056          FROM     (DATA-ENTRY-MAP)                                00049624
05057          ERASE                                                    00049625
05058          CURSOR                                                   00049626
05059          END-EXEC.                                                00049627
05060                                                                   00049628
05061      GO TO 9100-RETURN-TRAN.                                      00049629
05062                                                                   00049630
05063      EJECT                                                        00049631
05064                                                                   00049632
05065 ******************************************************************00049633
05066 *                                                                *00049634
05067 *    S  E N D    I N I T I A L   M A P   FOR    C A N C E L S    *00049635
05068 *                                                                *00049636
05069 ******************************************************************00049637
05070                                                                   00049638
05071  8110-SEND-INITIAL-CANCEL-MAP.                                    00049639
05072                                                                   00049640
05073      IF EIBAID NOT = DFHPF5                                       00049641
05074        AND EIBAID NOT = DFHPF1                                    00049642
05075        AND EIBAID NOT = DFHPF2                                    00049643
05076        AND PI-MAINT-FUNC NOT = 'B'                                00049644
05077          PERFORM 0700-PROTECT-FIELDS THRU 0700-EXIT.              00049645
05078                                                                   00049646
05079      MOVE PI-SAV-ENTRY-BATCH     TO CBATCHO.                      00049647
05080      MOVE PI-CR-MONTH-END-DT     TO DC-BIN-DATE-1.                00049648
05081      MOVE SPACE                  TO DC-OPTION-CODE.               00049649
05082      PERFORM 8500-DATE-CONVERT.                                   00049650
05083      MOVE WS-CURRENT-DT          TO CDATEO.                       00049651
05084      MOVE EIBTIME                TO TIME-IN.                      00049652
05085      MOVE TIME-OUT               TO CTIMEO.                       00049653
05086                                                                   00049654
05087      MOVE PI-LIFE-OVERRIDE-L2    TO CKIND1 (1).                   00049655
05088      MOVE AL-SABOF               TO CKIND1-ATTRB (1).             00049656
05089      MOVE PI-AH-OVERRIDE-L2      TO CKIND2 (1).                   00049657
05090      MOVE AL-SABOF               TO CKIND2-ATTRB (1).             00049658
05091      MOVE PI-LIFE-OVERRIDE-L2    TO CKIND1 (2).                   00049659
05092      MOVE AL-SABOF               TO CKIND1-ATTRB (2).             00049660
05093      MOVE PI-AH-OVERRIDE-L2      TO CKIND2 (2).                   00049661
05094      MOVE AL-SABOF               TO CKIND2-ATTRB (2).             00049662
05095                                                                   00049663
05096      IF PI-DATA-ERRORS                                            00049664
05097          MOVE SPACE              TO PI-ERROR-SW                   00049665
05098      ELSE                                                         00049666
05099          IF EIBAID = DFHPF1 OR DFHPF2                             00049667
05100              NEXT SENTENCE                                        00049668
05101          ELSE                                                     00049669
05102              IF CARR-GROUP-ST-ACCNT-CNTL                          00049670
05103                  MOVE -1                     TO CCAR1L            00049671
05104              ELSE                                                 00049672
05105                  IF ST-ACCNT-CNTL                                 00049673
05106                     MOVE -1                  TO CST1L             00049674
05107                     MOVE AL-SADOF            TO CCARHD1A          00049675
05108                                                 CGRPHD1A          00049676
05109                     MOVE AL-SANOF            TO CCAR1A            00049677
05110                                                 CGRP1A            00049678
05111                     MOVE AL-SADOF            TO CCARHD2A          00049679
05112                                                 CGRPHD2A          00049680
05113                     MOVE AL-SANOF            TO CCAR2A            00049681
05114                                                 CGRP2A            00049682
05115                  ELSE                                             00049683
05116                     IF CARR-ST-ACCNT-CNTL                         00049684
05117                        MOVE -1               TO CCAR1L            00049685
05118                        MOVE AL-SADOF         TO CGRPHD1A          00049686
05119                        MOVE AL-SANOF         TO CGRP1A            00049687
05120                        MOVE AL-SADOF         TO CGRPHD2A          00049688
05121                        MOVE AL-SANOF         TO CGRP2A            00049689
05122                     ELSE                                          00049690
05123                        IF ACCNT-CNTL                              00049691
05124                           MOVE -1            TO CACCT1L           00049692
05125                           MOVE AL-SADOF      TO CCARHD1A          00049693
05126                                                 CGRPHD1A          00049694
05127                                                 CSTHD1A           00049695
05128                           MOVE AL-SANOF      TO CCAR1A            00049696
05129                           MOVE AL-SADOF      TO CCARHD2A          00049697
05130                                                 CGRPHD2A          00049698
05131                                                 CSTHD2A           00049699
05132                           MOVE AL-SANOF      TO CCAR2A            00049700
05133                                                 CGRP2A            00049701
05134                                                 CST2A             00049702
05135                        ELSE                                       00049703
05136                           IF CARR-ACCNT-CNTL                      00049704
05137                              MOVE AL-SADOF   TO CGRPHD1A          00049705
05138                                                 CSTHD1A           00049706
05139                              MOVE AL-SANOF   TO CGRP1A            00049707
05140                                                 CST1A             00049708
05141                              MOVE AL-SADOF   TO CGRPHD2A          00049709
05142                                                 CSTHD2A           00049710
05143                              MOVE AL-SANOF   TO CGRP2A            00049711
05144                                                 CST2A             00049712
05145                              MOVE -1         TO CCAR1L.           00049713
05146                                                                   00049714
05147                                                                   00049715
05148                                                                   00049716
05149      MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O.                     00049717
05150      MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O.                     00049718
05151                                                                   00049719
05152      EXEC CICS SEND                                               00049720
05153          MAP      (PI-MAP-NAME)                                   00049721
05154          MAPSET   (MAPSET-EL9301S)                                00049722
05155          FROM     (DATA-ENTRY-MAP)                                00049723
05156          ERASE                                                    00049724
05157          CURSOR                                                   00049725
05158          END-EXEC.                                                00049726
05159                                                                   00049727
05160      GO TO 9100-RETURN-TRAN.                                      00049728
05161                                                                   00051610
05162      EJECT                                                        00051611
05163                                                                   00051630
05164 ******************************************************************00051631
05165 *                                                                *00051632
05166 *              S E N D    D A T A O N L Y                        *00051633
05167 *                                                                *00051670
05168 ******************************************************************00051671
05169                                                                   00051672
05170  8200-SEND-DATAONLY.                                              00051673
05171                                                                   00051710
05172      MOVE SPACE              TO PI-ERROR-SW.                      00051711
05173                                                                   00051712
05174      IF PI-MAP-NAME = EL930B                                      00051713
05175          MOVE PI-MEMBER-CAPTION      TO BCAPTNO                   00051714
05176          MOVE WS-CURRENT-DT          TO BDATEO                    00051715
05177          MOVE EIBTIME                TO TIME-IN                   00051716
05178          MOVE TIME-OUT               TO BTIMEO                    00051717
05179          MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O                  00051718
05180          MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O                  00051719
05181          EXEC CICS SEND                                           00051720
05182              MAP      (PI-MAP-NAME)                               00051721
05183              MAPSET   (MAPSET-EL9301S)                            00051722
05184              FROM     (DATA-ENTRY-MAP)                            00051723
05185              DATAONLY                                             00051724
05186              CURSOR                                               00051725
05187              END-EXEC                                             00051726
05188      ELSE                                                         00051727
05189          MOVE WS-CURRENT-DT          TO CDATEO                    00051728
05190          MOVE EIBTIME                TO TIME-IN                   00051729
05191          MOVE TIME-OUT               TO CTIMEO                    00051730
05192          MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O                  00051731
05193          MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O                  00051732
05194          EXEC CICS SEND                                           00051733
05195              MAP      (PI-MAP-NAME)                               00051734
05196              MAPSET   (MAPSET-EL9301S)                            00051735
05197              FROM     (DATA-ENTRY-MAP)                            00051736
05198              DATAONLY                                             00051737
05199              CURSOR                                               00051738
05200              END-EXEC.                                            00051739
05201                                                                   00051740
05202      GO TO 9100-RETURN-TRAN.                                      00051741
05203                                                                   00052030
05204      EJECT                                                        00052031
05205                                                                   00052050
05206 ******************************************************************00052051
05207 *                                                                *00052052
05208 *                U T I L I T Y   R O U T I N E S                 *00052053
05209 *                                                                *00052054
05210 ******************************************************************00052055
05211                                                                   00052056
05212  8300-SEND-TEXT.                                                  00052057
05213      EXEC CICS SEND TEXT                                          00052058
05214          FROM     (LOGOFF-TEXT)                                   00052059
05215          LENGTH   (LOGOFF-LENGTH)                                 00052060
05216          ERASE                                                    00052061
05217          FREEKB                                                   00052062
05218          END-EXEC.                                                00052063
05219      EXEC CICS RETURN                                             00052064
05220          END-EXEC.                                                00052065
05221                                                                   00052066
05222  8350-SEND-WARNING.                                               00052067
05223      EXEC CICS SEND TEXT                                          00052068
05224          FROM     (WARNING-TEXT)                                  00052069
05225          LENGTH   (WARNING-LENGTH)                                00052070
05226          ERASE                                                    00052071
05227          FREEKB                                                   00052072
05228          END-EXEC.                                                00052073
05229                                                                   00052290
05230      GO TO 9100-RETURN-TRAN.                                      00052291
05231                                                                   00052310
05232                                                                   00052311
05233  8400-LOG-JOURNAL-RECORD.                                         00052312
05234      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   00052313
05235      MOVE THIS-PGM                TO JP-PROGRAM-ID.               00052314
05236                                                                   00052360
05237 *    EXEC CICS JOURNAL                                            00052361
05238 *        JFILEID     (PI-JOURNAL-FILE-ID)                         00052362
05239 *        JTYPEID     ('EL')                                       00052363
05240 *        FROM        (JOURNAL-RECORD)                             00052364
05241 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   00052365
05242 *        END-EXEC.                                                00052366
05243                                                                   00052430
05244  8500-DATE-CONVERT.                                               00052431
05245                                                                   00052432
05246      EXEC CICS LINK                                               00052433
05247          PROGRAM  (LINK-ELDATCV)                                  00052434
05248          COMMAREA (DATE-CONVERSION-DATA)                          00052435
05249          LENGTH   (DC-COMM-LENGTH) END-EXEC.                      00052436
05250                                                                   00052437
05251  8500-EXIT.                                                       00052438
05252      EXIT.                                                        00052439
05253                                                                   00052440
05254      EJECT                                                        00052441
05255                                                                   00052442
05256  8550-SET-MAP-SEQ-NOS.                                            00052443
05257                                                                   00052444
05258      IF PI-MAP-NAME = EL930B                                      00052445
05259          MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ                      00052446
05260          MOVE AL-SABON               TO BSEQ-ATTRB                00052447
05261      ELSE                                                         00052448
05262          MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ (WS-SUB2)            00052449
05263          MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).     00052450
05264                                                                   00052640
05265      ADD +1  TO PI-NEXT-DISPLAY-SEQ-NO.                           00052641
05266                                                                   00052660
05267  8555-EXIT.                                                       00052661
05268      EXIT.                                                        00052662
05269                                                                   00052690
05270  8600-DEEDIT.                                                     00052691
05271                                                                   00052710
05272      EXEC CICS BIF DEEDIT                                         00052711
05273          FIELD   (DEEDIT-FIELD)                                   00052712
05274          LENGTH  (15)                                             00052713
05275          END-EXEC.                                                00052714
05276                                                                   00052760
05277  8600-EXIT.                                                       00052761
05278      EXIT.                                                        00052762
05279                                                                   00052763
05280  8800-UNAUTHORIZED-ACCESS.                                        00052764
05281      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   00052765
05282      GO TO 8300-SEND-TEXT.                                        00052766
05283                                                                   00052767
05284  9000-RETURN-CICS.                                                00052768
05285      EXEC CICS RETURN                                             00052769
05286          END-EXEC.                                                00052770
05287                                                                   00052771
05288  9100-RETURN-TRAN.                                                00052772
05289      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             00052773
05290      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         00052774
05291      EXEC CICS RETURN                                             00052775
05292          TRANSID    (TRANS-EXI2)                                  00052776
05293          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     00052777
05294          LENGTH     (PI-COMM-LENGTH)                              00052778
05295          END-EXEC.                                                00052779
05296                                                                   00052780
05297  9300-XCTL.                                                       00052781
05298                                                                   00052782
05299      EXEC CICS XCTL                                               00052783
05300          PROGRAM    (PGM-NAME)                                    00052784
05301          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     00052785
05302          LENGTH     (PI-COMM-LENGTH)                              00052786
05303          END-EXEC.                                                00052787
05304                                                                   00052788
05305  9400-CLEAR.                                                      00052789
05306                                                                   00052790
05307      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      00052791
05308      GO TO 9300-XCTL.                                             00052792
05309                                                                   00052793
05310  9500-PF12.                                                       00052794
05311                                                                   00052795
05312      MOVE XCTL-EL010             TO PGM-NAME.                     00052796
05313      GO TO 9300-XCTL.                                             00052797
05314                                                                   00052798
05315  9600-PGMID-ERROR.                                                00052799
05316                                                                   00052800
05317      EXEC CICS HANDLE CONDITION                                   00052801
05318          PGMIDERR    (8300-SEND-TEXT)                             00052802
05319          END-EXEC.                                                00052803
05320      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           00052804
05321      MOVE ' '                    TO PI-ENTRY-CD-1.                00052805
05322      MOVE XCTL-EL005            TO PGM-NAME.                      00052806
05323      MOVE PGM-NAME               TO LOGOFF-PGM.                   00052807
05324      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  00052808
05325      GO TO 9300-XCTL.                                             00052809
05326                                                                   00052810
05327  9900-ERROR-FORMAT.                                               00052811
05328                                                                   00052812
05329      IF PI-MAP-NAME = EL930B                                      00052813
05330         MOVE 2                   TO EMI-NUMBER-OF-LINES           00052814
05331        ELSE                                                       00052815
05332         MOVE 1                   TO EMI-NUMBER-OF-LINES.          00052816
05333                                                                   00052817
05334      IF NOT EMI-ERRORS-COMPLETE                                   00052818
05335          MOVE LINK-EL001         TO PGM-NAME                      00052819
05336          EXEC CICS LINK                                           00052820
05337              PROGRAM    (PGM-NAME)                                00052821
05338              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           00052822
05339              LENGTH     (EMI-COMM-LENGTH)                         00052823
05340              END-EXEC.                                            00052824
05341  9900-EXIT.                                                       00052825
05342      EXIT.                                                        00052826
05343                                                                   00052827
05344  9990-ABEND.                                                      00052828
05345                                                                   00052829
05346      MOVE LINK-EL004             TO PGM-NAME.                     00052830
05347      MOVE DFHEIBLK               TO EMI-LINE1                     00052831
05348      EXEC CICS LINK                                               00052832
05349          PROGRAM   (PGM-NAME)                                     00052833
05350          COMMAREA  (EMI-LINE1)                                    00052834
05351          LENGTH    (72)                                           00052835
05352          END-EXEC.                                                00052836
05353      IF PI-MAP-NAME = EL930B                                      00052837
05354          MOVE -1 TO BPFENTRL                                      00052838
05355      ELSE                                                         00052839
05356          MOVE -1 TO CPFENTRL.                                     00052840
05357      GO TO 8200-SEND-DATAONLY.                                    00052841
05358      GOBACK.                                                      00052842
05359                                                                   00052843
05360  9995-SECURITY-VIOLATION.                                         00052844
05361                              COPY ELCSCTP.                        00052845
05362                                                                   00052846
05363  9995-EXIT.                                                       00052847
05364      EXIT.                                                        00052848
05365                                                                   00052849
