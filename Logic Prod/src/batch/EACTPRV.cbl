00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 EACTPRV.                             00000030
00004 *                            VMOD=2.001.                          00000031
00005  AUTHOR.        LOGIC, INC.                                       00000050
00006                 DALLAS, TEXAS.                                    00000060
00007                                                                   00000070
00008  DATE-COMPILED.                                                   00000080
00009  SKIP1                                                            00000090
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
00020  REMARKS.                                                         00000200
00021          THIS PROGRAM PRINTS THE ACCOUNT MASTER.                  00000210
00022                                                                   00000220
00023  ENVIRONMENT DIVISION.                                            00000230
00024  INPUT-OUTPUT SECTION.                                            00000240
00025  FILE-CONTROL.                                                    00000250
00026                                                                   00000260
00027      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   00000270
00028      SELECT ACCT-MASTER      ASSIGN TO SYS010-UT-2400-S-SYS010.   00000280
00029  EJECT                                                            00000290
00030  DATA DIVISION.                                                   00000300
00031  FILE SECTION.                                                    00000310
00032                                                                   00000320
00033  FD  PRNTR                                                        00000330
00034      RECORDING MODE IS F                                          00000340
00035      LABEL RECORDS ARE OMITTED                                    00000350
00036      BLOCK CONTAINS 0 RECORDS                                     00000351
00037      RECORD CONTAINS 133 CHARACTERS.                              00000352
00038                                                                   00000353
00039  01  PRT.                                                         00000354
00040      12  P-CTL               PIC  X.                              00000355
00041      12  P-DATA              PIC  X(132).                         00000356
00042  EJECT                                                            00000357
00043  FD  ACCT-MASTER                                                  00000358
00044      RECORDING MODE IS F                                          00000359
00045      LABEL RECORDS ARE STANDARD                                   00000360
00046      RECORD CONTAINS 2000 CHARACTERS                              00000361
00047      BLOCK CONTAINS 0 RECORDS                                     00000362
00048      DATA RECORD IS ACT-MSTR-REC.                                 00000363
00049                                                                   00000364
00050  01  ACT-MSTR-REC            PIC  X(2000).                        00000365
00051  EJECT                                                            00000366
00052  WORKING-STORAGE SECTION.                                         00000367
00053  77  FILLER  PIC  X(32) VALUE '********************************'. 00000368
00054  77  FILLER  PIC  X(32) VALUE '    EACTPRV WORKING-STORAGE     '. 00000369
00055  77  FILLER  PIC  X(32) VALUE '*********VMOD=2.001*************'. 00000370
00056                                                                   00000371
00057  77  CTR                     PIC S99 COMP        VALUE +0.        00000372
00058  77  PGM-SUB                 PIC S9(3) COMP-3    VALUE +77.       00000373
00059  77  SETS                    PIC S9(3) COMP-3    VALUE +80.       00000374
00060  77  PAGER                   PIC S9(5) COMP-3    VALUE +1.        00000375
00061  77  X                       PIC  X.                              00000376
00062  77  SAV-CARRIER             PIC  X              VALUE LOW-VALUE. 00000377
00063                                                                   00000630
00064  01  WORK-PREV-DTE.                                               00000640
00065      12  WRK-PRV-YR          PIC  XX.                             00000641
00066      12  WRK-PRV-MO          PIC  XX.                             00000642
00067      12  WRK-PRV-DA          PIC  XX.                             00000643
00068  EJECT                                                            00000680
00069  01  HEAD-1.                                                      00000690
00070      12  FILLER              PIC  X(48)          VALUE SPACES.    00000700
00071      12  FILLER              PIC  X(27)          VALUE            00000710
00072              'ACCOUNT MASTER FILE SUMMARY'.                       00000720
00073      12  FILLER              PIC  X(49)          VALUE SPACES.    00000730
00074      12  FILLER              PIC  X(8)           VALUE 'EACTPRV'. 00000731
00075                                                                   00000750
00076  01  HEAD-2.                                                      00000760
00077      12  FILLER              PIC  X(47)          VALUE SPACES.    00000770
00078      12  HA-NAME             PIC  X(30).                          00000780
00079      12  FILLER              PIC  X(47)          VALUE SPACES.    00000790
00080      12  HB-IPL              PIC  X(8).                           00000791
00081                                                                   00000810
00082  01  HEAD-3.                                                      00000820
00083      12  FILLER              PIC  X(53)          VALUE SPACES.    00000830
00084      12  HC-DATE             PIC  X(18).                          00000840
00085      12  FILLER              PIC  X(41)          VALUE SPACES.    00000850
00086      12  FILLER              PIC  X(5)           VALUE 'PAGE'.    00000851
00087      12  HC-PAGE             PIC ZZ,ZZ9.                          00000870
00088                                                                   00000880
00089  01  HEAD-4.                                                      00000890
00090      12  FILLER              PIC  X              VALUE SPACES.    00000891
00091      12  FILLER              PIC  X(44)          VALUE            00000910
00092              '                                            '.      00000920
00093      12  FILLER              PIC  X(44)          VALUE            00000930
00094              '           EFFECTIVE DATES      CERTIFICATE '.      00000940
00095      12  FILLER              PIC  X(44)          VALUE            00000950
00096              'DATES      PREVIOUS DATES       LAST        '.      00000960
00097                                                                   00000970
00098  01  HEAD-5.                                                      00000980
00099      12  FILLER              PIC  X              VALUE SPACES.    00000981
00100      12  FILLER              PIC  X(44)          VALUE            00001000
00101              '    ACCOUNT CONTROL             ACCOUNT NAME'.      00001010
00102      12  FILLER              PIC  X(44)          VALUE            00001020
00103              '            FROM      TO           HI       '.      00001030
00104      12  FILLER              PIC  X(44)          VALUE            00001040
00105              'LO          FROM      TO        MAINT       '.      00001050
00106                                                                   00001060
00107  01  DATA-1.                                                      00001070
00108      12  FILLER              PIC  X              VALUE SPACES.    00001071
00109      12  D-CARRIER           PIC  X.                              00001072
00110      12  FILLER              PIC  X              VALUE '-'.       00001073
00111      12  D-GROUPING          PIC  X(6).                           00001074
00112      12  FILLER              PIC  X              VALUE '-'.       00001075
00113      12  D-STATE             PIC  XX.                             00001076
00114      12  FILLER              PIC  X              VALUE '-'.       00001077
00115      12  D-ACCOUNT           PIC  X(10).                          00001150
00116      12  FILLER              PIC  X              VALUE SPACES.    00001151
00117      12  D-NAME              PIC  X(30).                          00001170
00118      12  FILLER              PIC  X              VALUE SPACES.    00001171
00119      12  D-F-MO              PIC  XX.                             00001172
00120      12  FILLER              PIC  X              VALUE '-'.       00001173
00121      12  D-F-DA              PIC  XX.                             00001174
00122      12  FILLER              PIC  X              VALUE '-'.       00001175
00123      12  D-F-YR              PIC  XX.                             00001176
00124      12  FILLER              PIC  X              VALUE SPACES.    00001177
00125      12  D-EXP.                                                   00001250
00126          16  D-T-MO          PIC  XX.                             00001251
00127          16  D-D-1           PIC  X              VALUE '-'.       00001252
00128          16  D-T-DA          PIC  XX.                             00001253
00129          16  D-D-2           PIC  X              VALUE '-'.       00001254
00130          16  D-T-YR          PIC  XX.                             00001255
00131      12  FILLER              PIC  X(5)           VALUE SPACES.    00001256
00132      12  D-HIGHS.                                                 00001320
00133          16  D-H-MO          PIC  XX.                             00001321
00134          16  D-H-D1          PIC  X              VALUE '-'.       00001322
00135          16  D-H-DA          PIC  XX.                             00001323
00136          16  D-H-D2          PIC  X              VALUE '-'.       00001324
00137          16  D-H-YR          PIC  XX.                             00001325
00138      12  FILLER              PIC  X              VALUE SPACES.    00001326
00139      12  D-LOWS.                                                  00001390
00140          16  D-L-MO          PIC  XX.                             00001391
00141          16  D-L-D1          PIC  X              VALUE '-'.       00001392
00142          16  D-L-DA          PIC  XX.                             00001393
00143          16  D-L-D2          PIC  X              VALUE '-'.       00001394
00144          16  D-L-YR          PIC  XX.                             00001395
00145      12  FILLER              PIC  X(5)           VALUE SPACES.    00001396
00146      12  D-PF-MO             PIC  XX.                             00001397
00147      12  FILLER              PIC  X              VALUE '-'.       00001398
00148      12  D-PF-DA             PIC  XX.                             00001399
00149      12  FILLER              PIC  X              VALUE '-'.       00001400
00150      12  D-PF-YR             PIC  XX.                             00001401
00151      12  FILLER              PIC  X              VALUE SPACES.    00001402
00152      12  D-PEXP.                                                  00001520
00153          16  D-PT-MO         PIC  XX.                             00001521
00154          16  D-PD-1          PIC  X              VALUE '-'.       00001522
00155          16  D-PT-DA         PIC  XX.                             00001523
00156          16  D-PD-2          PIC  X              VALUE '-'.       00001524
00157          16  D-PT-YR         PIC  XX.                             00001525
00158      12  FILLER              PIC  X(5)           VALUE SPACES.    00001526
00159      12  CB-MAINT.                                                00001590
00160          16  CB-M-MO         PIC  XX.                             00001591
00161          16  CB-M-D1         PIC  X              VALUE '-'.       00001592
00162          16  CB-M-YR         PIC  XX.                             00001593
00163      12  FILLER              PIC  X(7)           VALUE SPACES.    00001594
00164  EJECT                                                            00001640
00165                              COPY ERCACCT.                        00001641
00166  EJECT                                                            00001660
00167                              COPY ELCDATE.                        00001661
00168  EJECT                                                            00001680
00169  PROCEDURE DIVISION.                                              00001690
00170      MOVE SPACES                 TO  HA-NAME.                     00001700
00171      MOVE CURRENT-DATE           TO  HB-IPL.                      00001710
00172      MOVE SPACES                 TO  HC-DATE.                     00001720
00173                                                                   00001730
00174      OPEN INPUT   ACCT-MASTER                                     00001740
00175           OUTPUT  PRNTR.                                          00001750
00176                                                                   00001760
00177  1000-READ-LOOP.                                                  00001770
00178      READ ACCT-MASTER  INTO  ACCOUNT-MASTER  AT END               00001780
00179          GO TO 9999-END-JOB.                                      00001790
00180                                                                   00001800
00181      ADD +1                      TO  SETS.                        00001810
00182                                                                   00001820
00183      MOVE AM-CARRIER             TO  D-CARRIER.                   00001830
00184      MOVE AM-GROUPING            TO  D-GROUPING.                  00001840
00185      MOVE AM-STATE               TO  D-STATE.                     00001850
00186      MOVE AM-ACCOUNT             TO  D-ACCOUNT.                   00001860
00187      MOVE AM-NAME                TO  D-NAME.                      00001870
00188      MOVE AM-EFF-YR              TO  D-F-YR.                      00001880
00189      MOVE AM-EFF-MO              TO  D-F-MO.                      00001890
00190      MOVE AM-EFF-DA              TO  D-F-DA.                      00001900
00191                                                                   00001910
00192      IF AM-EXPIRE-DT  IS EQUAL TO  ALL '9'                        00001920
00193          MOVE ' CURRENT'         TO  D-EXP                        00001930
00194      ELSE                                                         00001940
00195          MOVE '-'                TO  D-D-1  D-D-2                 00001950
00196          MOVE AM-EXP-YR          TO  D-T-YR                       00001960
00197          MOVE AM-EXP-MO          TO  D-T-MO                       00001970
00198          MOVE AM-EXP-DA          TO  D-T-DA.                      00001980
00199                                                                   00001990
00200      MOVE AM-HI-YR               TO  D-H-YR.                      00002000
00201      MOVE AM-HI-MO               TO  D-H-MO.                      00002010
00202      MOVE AM-HI-DA               TO  D-H-DA.                      00002020
00203                                                                   00002030
00204      IF AM-HI-CERT-DATE  IS EQUAL TO  ZEROS  OR  SPACES           00002040
00205          MOVE '  NONE'           TO  D-HIGHS                      00002050
00206      ELSE                                                         00002060
00207          MOVE '-'                TO  D-H-D1  D-H-D2.              00002070
00208                                                                   00002080
00209      MOVE AM-LO-YR               TO  D-L-YR.                      00002090
00210      MOVE AM-LO-MO               TO  D-L-MO.                      00002100
00211      MOVE AM-LO-DA               TO  D-L-DA.                      00002110
00212                                                                   00002120
00213      IF AM-LO-CERT-DATE  IS EQUAL TO  ZEROS  OR  SPACES           00002130
00214          MOVE '  NONE'           TO  D-LOWS                       00002140
00215      ELSE                                                         00002150
00216          MOVE '-'                TO  D-L-D1  D-L-D2.              00002160
00217                                                                   00002170
00218      MOVE AM-PREV-EFF-DT         TO  WORK-PREV-DTE.               00002180
00219      MOVE WRK-PRV-YR             TO  D-PF-YR.                     00002190
00220      MOVE WRK-PRV-MO             TO  D-PF-MO.                     00002200
00221      MOVE WRK-PRV-DA             TO  D-PF-DA.                     00002210
00222                                                                   00002220
00223      IF AM-PREV-EXP-DT  IS EQUAL TO  ALL '9'                      00002230
00224          MOVE ' CURRENT'         TO  D-PEXP                       00002240
00225      ELSE                                                         00002250
00226          MOVE AM-PREV-EXP-DT     TO  WORK-PREV-DTE                00002260
00227          MOVE '-'                TO  D-PD-1  D-PD-2               00002270
00228          MOVE WRK-PRV-YR         TO  D-PT-YR                      00002280
00229          MOVE WRK-PRV-MO         TO  D-PT-MO                      00002290
00230          MOVE WRK-PRV-DA         TO  D-PT-DA.                     00002300
00231                                                                   00002310
00232      IF AM-LAST-MAINT-DT  IS EQUAL TO  LOW-VALUES  OR  SPACES     00002320
00233          MOVE 'NONE'             TO  CB-MAINT                     00002330
00234      ELSE                                                         00002340
00235          MOVE AM-LAST-MAINT-DT   TO  DC-BIN-DATE-1                00002350
00236          MOVE ' '                TO  DC-OPTION-CODE               00002360
00237          PERFORM 8500-DATE-CONVERSION  THRU  8599-EXIT            00002370
00238          IF DATE-CONVERSION-ERROR                                 00002380
00239              MOVE 'NONE'         TO  CB-MAINT                     00002390
00240          ELSE                                                     00002400
00241              MOVE DC-GREG-DATE-1-YMD                              00002410
00242                                  TO  WORK-PREV-DTE                00002420
00243              MOVE WRK-PRV-MO     TO  CB-M-MO                      00002430
00244              MOVE '-'            TO  CB-M-D1                      00002440
00245              MOVE WRK-PRV-YR     TO  CB-M-YR.                     00002450
00246                                                                   00002460
00247      IF SETS  IS GREATER THAN  +58                                00002470
00248          MOVE PAGER              TO  HC-PAGE                      00002480
00249          MOVE HEAD-1             TO  PRT                          00002490
00250          MOVE '1'                TO  X                            00002500
00251          PERFORM 9000-PRINT  THRU  9000-PRINT-EXIT                00002510
00252          MOVE HEAD-2             TO  PRT                          00002520
00253          MOVE ' '                TO  X                            00002530
00254          PERFORM 9000-PRINT  THRU  9000-PRINT-EXIT                00002540
00255          MOVE HEAD-3             TO  PRT                          00002550
00256          MOVE ' '                TO  X                            00002560
00257          PERFORM 9000-PRINT  THRU  9000-PRINT-EXIT                00002570
00258          MOVE HEAD-4             TO  PRT                          00002580
00259          MOVE '0'                TO  X                            00002590
00260          PERFORM 9000-PRINT  THRU  9000-PRINT-EXIT                00002600
00261          MOVE HEAD-5             TO  PRT                          00002610
00262          MOVE ' '                TO  X                            00002620
00263          PERFORM 9000-PRINT  THRU  9000-PRINT-EXIT                00002630
00264          MOVE SPACES             TO  PRT                          00002640
00265          MOVE ' '                TO  X                            00002650
00266          PERFORM 9000-PRINT  THRU  9000-PRINT-EXIT                00002660
00267          MOVE +10                TO  SETS                         00002670
00268          ADD +1                  TO  PAGER.                       00002680
00269                                                                   00002690
00270      MOVE AM-CARRIER             TO  SAV-CARRIER.                 00002700
00271      MOVE DATA-1                 TO  PRT.                         00002710
00272      MOVE ' '                    TO  X.                           00002720
00273                                                                   00002730
00274      PERFORM 9000-PRINT  THRU  9000-PRINT-EXIT.                   00002740
00275                                                                   00002750
00276      ADD +1                      TO  SETS.                        00002760
00277                                                                   00002770
00278      MOVE SPACES                 TO  PRT.                         00002780
00279                                                                   00002790
00280      PERFORM 9000-PRINT  THRU  9000-PRINT-EXIT.                   00002800
00281                                                                   00002810
00282      GO TO 1000-READ-LOOP.                                        00002820
00283                                                                   00002830
00284  8500-DATE-CONVERSION.                                            00002840
00285                              COPY ELCDCS.                         00002850
00286                                                                   00002860
00287  8599-EXIT.                                                       00002870
00288      EXIT.                                                        00002880
00289                                                                   00002890
00290  9000-PRINT.                                                      00002900
00291      WRITE PRT  AFTER  POSITIONING  X  LINES.                     00002910
00292                                                                   00002920
00293  9000-PRINT-EXIT.                                                 00002930
00294      EXIT.                                                        00002940
00295                                                                   00002950
00296  9999-END-JOB.                                                    00002960
00297      CLOSE  ACCT-MASTER  PRNTR.                                   00002970
00298                                                                   00002980
00299      GOBACK.                                                      00002990
