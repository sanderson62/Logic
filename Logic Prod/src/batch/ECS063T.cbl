00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 ECS063.                              00000030
00004 *                            VMOD=2.007.                          00000031
00005                                                                   00000050
00006  AUTHOR.        LOGIC, INC.                                       00000060
00007                 DALLAS, TEXAS.                                    00000070
00008                                                                   00000080
00009  DATE-COMPILED.                                                   00000090
00010                                                                   00000100
00011  SECURITY.   *****************************************************00000110
00012              *                                                   *00000120
00013              *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *00000130
00014              *                                                   *00000140
00015              *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *00000150
00016              *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *00000160
00017              *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *00000170
00018              *                                                   *00000180
00019              *****************************************************00000190
00020                                                                   00000200
00021 *REMARKS.                                                         00000201
00022 *        THIS PROGRAM PRINTS STATEMENT OF ACCOUNTS AND UPDATES    00000202
00023 *        THE GENERAL AGENT RECORDS ON THE COMPENSATION MASTER.    00000203
00024                                                                   00000240
120804******************************************************************
120804*                   C H A N G E   L O G
120804*
120804* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120804*-----------------------------------------------------------------
120804*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120804* EFFECTIVE    NUMBER
120804*-----------------------------------------------------------------
120804* 120804                   PEMA  FIX REFUND NOT PRINTING ON STMT
122804* 122804  CR2004101500006  PEMA  DROP DECIMALS IF SPP DCC
032406* 032406  CR2006022800001  AJRA  SET CO-FIRST-WRITTEN-DT
120804******************************************************************
00025  ENVIRONMENT DIVISION.                                            00000250
00026  INPUT-OUTPUT SECTION.                                            00000260
00027  FILE-CONTROL.                                                    00000270
00028                                                                   00000280
00029      SELECT SORT-FILE        ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.  00000290
00030      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   00000300
00031      SELECT COMM-MSTR-OUT    ASSIGN TO SYS010-UT-2400-S-SYS010.   00000310
00032      SELECT COMM-MSTR-IN     ASSIGN TO SYS015-UT-FBA1-S-SYS015.   00000320
00033      SELECT SUMM-TRAN-IN     ASSIGN TO SYS017-UT-FBA1-S-SYS017.   00000330
00034      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   00000340
00035      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   00000350
00036  EJECT                                                            00000360
00037  DATA DIVISION.                                                   00000370
00038  FILE SECTION.                                                    00000380
00039                                                                   00000390
00040  SD  SORT-FILE.                                                   00000400
00043  01  SORT-REC.                                                    00000430
00044      12  SORT-ID             PIC  XX.                             00000440
00045      12  SORT-CONTROL-74.                                         00000450
00046          16  SORT-CARRIER    PIC  X.                              00000460
00047          16  SORT-GROUPING   PIC  X(6).                           00000470
00048          16  SORT-RESP-NO    PIC  X(10).                          00000480
00049          16  SORT-ACCOUNT    PIC  X(10).                          00000490
00050          16  SORT-AM-NO      PIC  X(10).                          00000500
00051      12  SORT-TYPE           PIC  X.                              00000510
00052      12  SORT-BAL-CTL        PIC  X.                              00000520
00053      12  SORT-NAME           PIC  X(30).                          00000530
00054      12  SORT-PREM           PIC S9(7)V99               COMP-3.   00000540
00055      12  SORT-COMM           PIC S9(7)V99               COMP-3.   00000550
00056      12  SORT-PMTS           PIC S9(7)V99               COMP-3.   00000560
00057      12  SORT-BEG-BAL        PIC S9(7)V99               COMP-3.   00000570
00058      12  SORT-END-BAL        PIC S9(7)V99               COMP-3.   00000580
00059      12  SORT-OV-L-PREM      PIC S9(7)V99               COMP-3.   00000590
00060      12  SORT-OV-A-PREM      PIC S9(7)V99               COMP-3.   00000600
00061      12  SORT-OV-LIFE        PIC S9(7)V99               COMP-3.   00000610
00062      12  SORT-OV-AH          PIC S9(7)V99               COMP-3.   00000620
011904     12  SORT-OV-AH-RFND     PIC S9(7)V99               COMP-3.
040504     12  SORT-DLR-INC        PIC S9(7)V99               COMP-3.
           12  SORT-LMBA-FEE       PIC S9(7)V99               COMP-3.
00063      12  SORT-OV-B-L-PREM    PIC S9(7)V99               COMP-3.   00000630
00064      12  SORT-OV-B-A-PREM    PIC S9(7)V99               COMP-3.   00000640
00065      12  SORT-OV-B-LIFE      PIC S9(7)V99               COMP-3.   00000650
00066      12  SORT-OV-B-AH        PIC S9(7)V99               COMP-3.   00000660
00067  EJECT                                                            00000670
00068  FD  PRNTR                                                        00000680
00069                              COPY ELCPRTFD.                       00000690
00070  EJECT                                                            00000700
00071  FD  COMM-MSTR-OUT                                                00000710
00072                              COPY ECSCOOFD.                       00000720
00073  EJECT                                                            00000730
00074  FD  COMM-MSTR-IN                                                 00000740
00075                              COPY ECSCOIFD.                       00000750
00076  EJECT                                                            00000760
00077  FD  SUMM-TRAN-IN                                                 00000770
00080      BLOCK CONTAINS 0 RECORDS                                     00000800
00078      RECORDING MODE F.                                            00000780
00082  01  CCM-WK.                                                      00000820
00083      12  COMM-TRAN-WORK-REC.                                      00000830
00084          16  CCW-ID              PIC  XX.                         00000840
00085          16  CCW-CARR-GROUP      PIC  X(7).                       00000850
00086          16  CCW-RESP-NO         PIC  X(10).                      00000860
00087          16  CCW-ACCOUNT         PIC  X(10).                      00000870
00088          16  CCW-AM-NO           PIC  X(10).                      00000880
00089          16  CCW-TYPE            PIC  X.                          00000890
00090              88  CCW-ACCTG                   VALUE '5'.           00000900
00091              88  CCW-OVERWT                  VALUE '6'.           00000910
00092              88  CCW-SUMMARY                 VALUE '7'.           00000920
00093          16  CCW-BAL-CTL         PIC  X.                          00000930
00094          16  CCW-NAME            PIC  X(30).                      00000940
00095          16  CCW-PREM            PIC S9(7)V99           COMP-3.   00000950
00096          16  CCW-COMM            PIC S9(7)V99           COMP-3.   00000960
00097          16  CCW-PMTS            PIC S9(7)V99           COMP-3.   00000970
00098          16  CCW-BEG-BAL         PIC S9(7)V99           COMP-3.   00000980
00099          16  CCW-END-BAL         PIC S9(7)V99           COMP-3.   00000990
00100          16  CCW-OV-L-PREM       PIC S9(7)V99           COMP-3.   00001000
00101          16  CCW-OV-A-PREM       PIC S9(7)V99           COMP-3.   00001010
00102          16  CCW-OV-LIFE         PIC S9(7)V99           COMP-3.   00001020
00103          16  CCW-OV-AH           PIC S9(7)V99           COMP-3.   00001030
011904         16  CCW-OV-AH-RFND      PIC S9(7)V99           COMP-3.
040504         16  CCW-DLR-INC         PIC S9(7)V99           COMP-3.
               16  CCW-LMBA-FEE        PIC S9(7)V99           COMP-3.
00104          16  CCW-OV-B-L-PREM     PIC S9(7)V99           COMP-3.   00001040
00105          16  CCW-OV-B-A-PREM     PIC S9(7)V99           COMP-3.   00001050
00106          16  CCW-OV-B-LIFE       PIC S9(7)V99           COMP-3.   00001060
00107          16  CCW-OV-B-AH         PIC S9(7)V99           COMP-3.   00001070
00108                                                                   00001080
00109  FD  DISK-DATE                                                    00001090
00110                              COPY ELCDTEFD.                       00001100
00111  EJECT                                                            00001110
00112  FD  FICH                                                         00001120
00113                              COPY ELCFCHFD.                       00001130
00114  EJECT                                                            00001140
00115  WORKING-STORAGE SECTION.                                         00001150
00116  77  FILLER PIC X(32) VALUE '********************************'.   00001160
00117  77  FILLER PIC X(32) VALUE '*            ECS063            *'.   00001170
00118  77  FILLER PIC X(32) VALUE '*********** VMOD=2.007 *********'.   00001171
00119                                                                   00001190
00120  77  PGM-SUB                 PIC S9(3)       VALUE +63  COMP-3.   00001200
00121  77  SPACE-NP                PIC  X          VALUE '1'.           00001210
00122  77  SPACE-1                 PIC  X          VALUE ' '.           00001220
00123  77  SPACE-2                 PIC  X          VALUE '0'.           00001230
00124  77  SPACE-3                 PIC  X          VALUE '-'.           00001240
00125  77  X                       PIC  X          VALUE '1'.           00001250
00126  77  CMI-END-OF-FILE-SW      PIC  X          VALUE 'N'.           00001260
00127      88  CMI-END-OF-FILE                     VALUE 'Y'.           00001270
00128                                                                   00001280
00129  01  REQUIRED-STORAGE.                                            00001290
00130      12  WS-RETURN-CODE          PIC S9(4)              COMP.     00001300
00131      12  WS-ABEND-MESSAGE        PIC  X(80).                      00001310
00132      12  WS-ABEND-FILE-STATUS    PIC  XX     VALUE ZEROS.         00001320
00133      12  WS-ZERO                 PIC S9      VALUE ZERO COMP-3.   00001330
00134  EJECT                                                            00001340
00135  01  MISC-WORKING-STORAGE.                                        00001350
00136      12  COMP-3-AREA     COMP-3.                                  00001360
00137          16  PGCTR           PIC S9(5)           VALUE +0.        00001370
00138          16  LNCTR           PIC S9(3)           VALUE +0.        00001380
00139          16  AST-SW          PIC S9              VALUE +0.        00001390
00140          16  WORK-PERC       PIC S99V9(3)        VALUE +0.        00001400
00141          16  X-ZERO          PIC S9(9)V99        VALUE +0.        00001410
00142          16  T-COMP          PIC S9(9)V99        VALUE +0.        00001420
00143          16  T-DUE-ACCT      PIC S9(9)V99        VALUE +0.        00001430
00144          16  T-OVERW         PIC S9(9)V99        VALUE +0.        00001440
00145          16  T-DUE           PIC S9(9)V99        VALUE +0.        00001450
00146      12  STMT-OF-ACCT-TOTALS     COMP-3.                          00001460
00147          16  S-PREM          PIC S9(7)V99.                        00001470
00148          16  S-L-PREM        PIC S9(7)V99.                        00001480
00149          16  S-A-PREM        PIC S9(7)V99.                        00001490
00150          16  S-COMM          PIC S9(7)V99.                        00001500
00151          16  S-PYMT          PIC S9(7)V99.                        00001510
00152          16  S-BEG-BAL       PIC S9(7)V99.                        00001520
00153          16  S-END-BAL       PIC S9(7)V99.                        00001530
00154          16  S-OV-LIFE       PIC S9(7)V99.                        00001540
00155          16  S-OV-AH         PIC S9(7)V99.                        00001550
011904         16  S-OV-AH-RFND    PIC S9(7)V99.
040504         16  S-DLR-INC       PIC S9(7)V99.
               16  S-LMBA-FEE      PIC S9(7)V99.
00156          16  S-OV-PYMT       PIC S9(7)V99.                        00001560
00157          16  S-OV-BEG-BAL    PIC S9(7)V99.                        00001570
00158          16  S-OV-END-BAL    PIC S9(7)V99.                        00001580
00159          16  S-DUE           PIC S9(7)V99.                        00001590
00160          16  S-OV-B-PREM     PIC S9(7)V99.                        00001600
00161          16  S-OV-B-L-PREM   PIC S9(7)V99.                        00001610
00162          16  S-OV-B-A-PREM   PIC S9(7)V99.                        00001620
00163          16  S-OV-B-LIFE     PIC S9(7)V99.                        00001630
00164          16  S-OV-B-AH       PIC S9(7)V99.                        00001640
00165      12  OVERWRITE-UPDATE-TOTALS     COMP-3.                      00001650
00166          16  S-UP-PREM       PIC S9(7)V99.                        00001660
00167          16  S-UP-COMM       PIC S9(7)V99.                        00001670
00168          16  S-UP-PYMT       PIC S9(7)V99.                        00001680
00169      12  TOT-OV-COMM         PIC S9(7)V99        VALUE +0 COMP-3. 00001690
           12  TOT-MISC-FEES       PIC S9(7)V99        VALUE +0 COMP-3.
00170      12  MSTR-CTL-RECS-IN    PIC S9(9)           VALUE +0 COMP-3. 00001700
00171      12  MSTR-CTL-RECS-OUT   PIC S9(9)           VALUE +0 COMP-3. 00001710
00172      12  PRE-CONTROL.                                             00001720
00173          16  PRE-CTL-1.                                           00001730
00174              20  PRE-CARR-GROUP.                                  00001740
00175                  24  PRE-CARRIER     PIC  X.                      00001750
00176                  24  PRE-GROUPING    PIC  X(6).                   00001760
00177              20  PRE-RESP-NO         PIC  X(10).                  00001770
00178          16  PRE-CTL-2.                                           00001780
00179              20  PRE-ACCOUNT         PIC  X(10).                  00001790
00180      12  FIRST-ACTIVITY-SW   PIC  X.                              00001800
00181          88  FIRST-ACTIVITY                  VALUE 'Y'.           00001810
00182          88  NOT-FIRST-ACTIVITY              VALUE 'N'.           00001820
00183      12  SAVE-COMPANY-NAME   PIC  X(30)  OCCURS 3 TIMES.          00001830
00184      12  SAVE-CARR-GROUP.                                         00001840
00185          16  SAVE-CARRIER    PIC  X          VALUE LOW-VALUE.     00001850
00186          16  SAVE-GROUPING   PIC  X(6)       VALUE LOW-VALUE.     00001860
00187      12  STMT-SW             PIC S9          VALUE +0   COMP-3.   00001870
00188      12  OVERWRITE-ONLY-SW   PIC S9                     COMP-3.   00001880
00189          88  OVERWRITE-ONLY                  VALUE +0.            00001890
00190          88  ALSO-SUMMARY                    VALUE +1.            00001900
00191                                                                   00001910
00192      12  ZIP-CODE.                                                00001911
00193          16  ZIP-FIVE            PIC  X(5).                       00001912
00194          16  ZIP-DASH            PIC  X.                          00001913
00195          16  ZIP-FOUR            PIC  X(4).                       00001914
00196      12  POSTAL-CODE  REDEFINES  ZIP-CODE.                        00001915
00197          16  POST-CODE1          PIC  XXX.                        00001916
00198          16  FILLER              PIC  X.                          00001917
00199          16  POST-CODE2          PIC  XXX.                        00001918
00200          16  FILLER              PIC  XXX.                        00001919
00201                                                                   00001920
00202      12  WORK-ZIP-CODE.                                           00001921
00203          16  WZC-PRIME.                                           00001922
00204              20  WZC-POS-1       PIC  X.                          00001923
00205              20  FILLER          PIC  X(4).                       00001924
00206          16  WZC-PLUS4           PIC  X(4).                       00001925
00207      12  WORK-POSTAL-CD  REDEFINES  WORK-ZIP-CODE.                00001926
00208          16  WZC-POST-CD1        PIC  XXX.                        00001927
00209          16  WZC-POST-CD2        PIC  XXX.                        00001928
00210          16  FILLER              PIC  XXX.                        00001929
00211                                                                   00001930
00212  01  HD1.                                                         00001931
00213      12  FILLER              PIC  X(44)      VALUE SPACES.        00001932
00214      12  FILLER              PIC  X(44)      VALUE                00002140
00215              '            STATEMENT OF ACCOUNT            '.      00002141
00216      12  FILLER              PIC  X(36)      VALUE SPACES.        00002142
00217      12  FILLER              PIC  X(44)      VALUE 'ECS-063'.     00002143
00218                                                                   00002144
00219  01  HD2.                                                         00002145
00220      12  FILLER              PIC  X(51)      VALUE SPACES.        00002146
00221      12  HD-CO               PIC  X(30).                          00002147
00222      12  FILLER              PIC  X(43)      VALUE SPACES.        00002148
00223      12  HD-RUN-DT           PIC  X(8)       VALUE SPACES.        00002149
00224                                                                   00002150
00225  01  HD3.                                                         00002151
00226      12  FILLER              PIC  X(57)      VALUE SPACES.        00002152
00227      12  HD-DT               PIC  X(18).                          00002153
00228      12  FILLER              PIC  X(37)      VALUE SPACES.        00002154
00229      12  FILLER              PIC  X(5)       VALUE 'PAGE '.       00002155
00230      12  HD-PG               PIC ZZ,ZZ9.                          00002156
00231      12  FILLER              PIC  X(9)       VALUE SPACES.        00002157
00232                                                                   00002158
00233  01  HD4.                                                         00002159
00234      12  FILLER              PIC  X(44)      VALUE                00002160
00235              '----------------- ACCOUNT ----------------- '.      00002161
00236      12  FILLER              PIC  X(44)      VALUE                00002162
00237              '                        NET         ACCOUNT '.      00002163
00238      12  FILLER              PIC  X(44)      VALUE                00002164
00239              '    PAYMENTS/                    AMOUNT     '.      00002165
00240                                                                   00002166
00241  01  HD5.                                                         00002167
00242      12  FILLER              PIC  X(44)      VALUE                00002168
00243              '  NUMBER                  NAME              '.      00002169
042904     12  FILLER              PIC  X(21)      VALUE
                   '                     '.
           12  HD5-PRM-CNT         PIC  X(07)  VALUE '       '.
00244      12  FILLER              PIC  X(15)      VALUE
00245              '    COMPENSATIO'.
00246      12  FILLER              PIC  X(15)      VALUE
00247              'N  ADJUSTMENTS '.
           12  HD5-OW-HD           PIC  X(14) VALUE ' CONTRACT FEE '.

00246      12  FILLER              PIC  X(14)      VALUE
00247              '     DUE      '.
00248  EJECT                                                            00002174
00249  01  P-REC.                                                       00002175
00250      12  P-CCSW              PIC  X.                              00002176
00251      12  P-LINE.                                                  00002177
00252          16  FILLER          PIC  X(132).                         00002178
00253      12  P-LINE-1  REDEFINES  P-LINE.                             00002179
00254          16  P-CARRIER       PIC  X.                              00002180
00255          16  P-DASH-1        PIC  X.                              00002181
00256          16  P-GROUPING      PIC  X(6).                           00002182
00257          16  P-DASH-2        PIC  X.                              00002183
00258          16  P-RESP-NO       PIC  X(10).                          00002184
00259          16  FILLER          PIC  X.                              00002185
00260          16  P-NAME          PIC  X(30).                          00002186
00261          16  FILLER          PIC  X(8).                           00002187
00262          16  P-ZIP           PIC  X(10).                          00002188
00263          16  FILLER          PIC  X.                              00002189
00264          16  P-CTR           PIC ZZZ,ZZZ,ZZZ-.                    00002190
00265          16  FILLER          PIC  X(51).                          00002191
00266      12  P-LINE-2  REDEFINES  P-LINE.                             00002192
00267          16  P-ACCT          PIC  X(10).                          00002193
00268          16  FILLER          PIC  X.                              00002194
00269          16  P-AST           PIC  X.                              00002195
00270          16  FILLER          PIC  X.                              00002196
00271          16  P-DESC          PIC  X(30).                          00002197
00272          16  FILLER          PIC  X.                              00002198
00273          16  P-BEG           PIC Z,ZZZ,ZZZ.ZZ-.                   00002199
00274          16  FILLER          PIC  X.                              00002200
00275          16  P-BEN           PIC  X(3).                           00002201
00276          16  FILLER          PIC  X.                              00002202
00277          16  P-PRM           PIC Z,ZZZ,ZZZ.ZZ-.                   00002203
122804         16  P-CNT REDEFINES P-PRM
122804                             PIC ZZZZ,ZZZ,ZZZ-.
00278          16  FILLER          PIC  X.                              00002204
00279          16  P-COM           PIC Z,ZZZ,ZZZ.ZZ-.                   00002205
00280          16  FILLER          PIC  X.                              00002206
00281          16  P-PMT           PIC Z,ZZZ,ZZZ.ZZ-.                   00002207
00282          16  FILLER          PIC  X.                              00002208
00283          16  P-LOV           PIC Z,ZZZ,ZZZ.ZZ-.                   00002209
00284          16  FILLER          PIC  X.                              00002210
00285          16  P-DUE           PIC Z,ZZZ,ZZZ.99-.                   00002211
00286          16  FILLER          PIC  X.                              00002212
00287      12  P-LINE-3  REDEFINES  P-LINE.                             00002213
00288          16  FILLER          PIC  X(20).                          00002214
00289          16  P-TOT-NAME      PIC  X(38).                          00002215
00290          16  FILLER          PIC  X(04).                          00002216
00291          16  P-TOT-PRM       PIC Z,ZZZ,ZZ9.99-.                   00002217
00292          16  FILLER          PIC  X(57).                          00002218
00293      12  P-LINE-4  REDEFINES  P-LINE.                             00002219
00294          16  FILLER          PIC  X(16).                          00002220
00295          16  T-DESC          PIC  X(35).                          00002221
00296          16  T-AMT           PIC  ZZZ,ZZZ,ZZZ.99-.                00002222
00297          16  T-AMTR  REDEFINES                                    00002223
00298              T-AMT           PIC  X(15).                          00002224
00299          16  FILLER          PIC  X(18).                          00002225
00300          16  T-EXPL          PIC  X(11).                          00002226
00301          16  FILLER          PIC  X(37).                          00002227
00302  EJECT                                                            00002228
00303                              COPY ERCCOMP.                        00002229
00304  EJECT                                                            00002230
00305  01  SUMM-REC.                                                    00002231
00306      12  SUMM-ID                 PIC  XX.                         00002232
00307      12  SUMM-CONTROL.                                            00002233
00308          16  SUMM-CTL-1.                                          00002234
00309              20  SUMM-CARRIER    PIC  X.                          00002235
00310              20  SUMM-GROUING    PIC  X(6).                       00002236
00311              20  SUMM-RESP-NO    PIC  X(10).                      00002237
00312              20  SUMM-ACCOUNT    PIC  X(10).                      00002238
00313          16  SUMM-CTL-2.                                          00002239
00314              20  SUMM-AM-NO      PIC  X(10).                      00002240
00315      12  SUMM-TYPE               PIC  X.                          00002241
00316          88  SUMM-ACCTG                      VALUE '5'.           00002242
00317          88  SUMM-OVRWT                      VALUE '6'.           00002243
00318          88  SUMM-SUMMR                      VALUE '7'.           00002244
00319      12  SUMM-BAL-CTL            PIC  X.                          00002245
00320      12  SUMM-NAME               PIC  X(30).                      00002246
00321      12  SUMM-PREM               PIC S9(7)V99           COMP-3.   00002247
00322      12  SUMM-COMM               PIC S9(7)V99           COMP-3.   00002248
00323      12  SUMM-PMTS               PIC S9(7)V99           COMP-3.   00002249
00324      12  SUMM-BEG-BAL            PIC S9(7)V99           COMP-3.   00002250
00325      12  SUMM-END-BAL            PIC S9(7)V99           COMP-3.   00002251
00326      12  SUMM-OV-L-PREM          PIC S9(7)V99           COMP-3.   00002252
00327      12  SUMM-OV-A-PREM          PIC S9(7)V99           COMP-3.   00002253
00328      12  SUMM-OV-LIFE            PIC S9(7)V99           COMP-3.   00002254
00329      12  SUMM-OV-AH              PIC S9(7)V99           COMP-3.   00002255
011904     12  SUMM-OV-AH-RFND         PIC S9(7)V99           COMP-3.
040504     12  SUMM-DLR-INC            PIC S9(7)V99           COMP-3.
           12  SUMM-LMBA-FEE           PIC S9(7)V99           COMP-3.
00330      12  SUMM-OV-B-L-PREM        PIC S9(7)V99           COMP-3.   00002256
00331      12  SUMM-OV-B-A-PREM        PIC S9(7)V99           COMP-3.   00002257
00332      12  SUMM-OV-B-LIFE          PIC S9(7)V99           COMP-3.   00002258
00333      12  SUMM-OV-B-AH            PIC S9(7)V99           COMP-3.   00002259
00334  EJECT                                                            00002260
00335                              COPY ELCDTECX.                       00002261
00335                              COPY ELCDTEVR.                       00002261
00336  EJECT                                                            00002262
00337  PROCEDURE DIVISION.                                              00002263
00338                                                                   00002264
00339  0000-STANDARD-RTN.                                               00002265
00340                              COPY ELCDTERX.                       00002266
00341                                                                   00003410
00342      GO TO 1100-SORT-RTN.                                         00003411
00343                                                                   00003412
00344  1000-SORT-ROUTINE SECTION.                                       00003413
00345                                                                   00003450
00346  1100-SORT-RTN.                                                   00003451
00347      SORT SORT-FILE  ON ASCENDING  SORT-CARRIER                   00003452
00348                                    SORT-GROUPING                  00003453
00349                                    SORT-RESP-NO                   00003454
00350                                    SORT-ACCOUNT                   00003455
00351                                    SORT-AM-NO                     00003456
00352                                    SORT-TYPE                      00003457
00353          INPUT  PROCEDURE 2000-INPUT-RTN   THRU  2999-EXIT        00003458
00354          OUTPUT PROCEDURE 3000-OUTPUT-RTN  THRU  8999-EXIT.       00003459
00355                                                                   00003550
00356      IF SORT-RETURN NOT = ZERO                                    00003551
00357          MOVE '0101'             TO  WS-RETURN-CODE               00003552
00358          MOVE 'INTERNAL SORT 01 ABORTED'                          00003553
00359                                  TO  WS-ABEND-MESSAGE             00003554
00360          GO TO ABEND-PGM.                                         00003555
00361                                                                   00003556
00362      GO TO 9990-E-O-J.                                            00003557
00363  EJECT                                                            00003558
00364  2000-INPUT-RTN SECTION.                                          00003559
00365                                                                   00003560
00366  2000-INTIALIZE-INPUT.                                            00003561
00367      OPEN INPUT SUMM-TRAN-IN.                                     00003562
00368                                                                   00003563
00369  2100-RD-CCM.                                                     00003564
00370      READ SUMM-TRAN-IN                                            00003565
00371          AT END                                                   00003566
00372              GO TO 2990-END-INPUT.                                00003567
00373                                                                   00003568
00374      MOVE CCM-WK                 TO  SORT-REC.                    00003569
00375                                                                   00003570
00376      RELEASE SORT-REC.                                            00003571
00377                                                                   00003572
00378      GO TO 2100-RD-CCM.                                           00003573
00379                                                                   00003574
00380  2990-END-INPUT.                                                  00003575
00381      CLOSE SUMM-TRAN-IN.                                          00003576
00382                                                                   00003820
00383  2999-EXIT.                                                       00003821
00384      EXIT.                                                        00003822
00385  EJECT                                                            00003823
00386  3000-OUTPUT-RTN SECTION.                                         00003824
00387                                                                   00003825
00388  3000-INITIALIZE-OUTPUT.                                          00003826
00389      OPEN INPUT  COMM-MSTR-IN                                     00003827
00390           OUTPUT COMM-MSTR-OUT                                    00003828
00391                  PRNTR.                                           00003829
00392                                                                   00003830
00393      MOVE LOW-VALUES             TO  COMPENSATION-MASTER          00003831
00394                                      COMP-IN-RECORD               00003832
00395                                      COMP-OUT-RECORD              00003833
00396                                      PRE-CONTROL.                 00003834
00397      MOVE COMPANY-NAME           TO  SAVE-COMPANY-NAME (1)        00003835
00398                                      SAVE-COMPANY-NAME (2)        00003836
00399                                      SAVE-COMPANY-NAME (3).       00003837
00400      MOVE ALPH-DATE              TO  HD-DT.                       00003838
00401      MOVE WS-CURRENT-DATE        TO  HD-RUN-DT.                   00003839
00402                                                                   00003840
00403      PERFORM 8000-MSTR-CONTROL-RTN  THRU  8099-EXIT.              00003841
00404                                                                   00003842
00405      PERFORM 8200-RETURN-SORT-RTN   THRU  8299-EXIT.              00003843
00406                                                                   00003844
00407      PERFORM 8300-COMBINE-CCM-RTN   THRU  8399-EXIT.              00003845
00408                                                                   00003846
00409      GO TO 4000-PROCESS-RTN.                                      00003847
00410  EJECT                                                            00003848
00411  4000-PROCESS-RTN.                                                00003849
00412      IF SUMM-CTL-1 LESS CO-CONTROL                                00003850
00413          GO TO 4999-PROCESS-ERR.                                  00003851
00414                                                                   00003852
00415      IF SUMM-CTL-1 GREATER CO-CONTROL                             00003853
00416          PERFORM 6000-UPDATE-MSTR-RTN  THRU  6299-EXIT            00003854
00417          PERFORM 5400-PRT-TOTAL-RTN    THRU  5599-EXIT            00003855
00418          PERFORM 8000-MSTR-CONTROL-RTN THRU  8099-EXIT            00003856
00419          GO TO 4000-PROCESS-RTN.                                  00003857
00420                                                                   00003858
00421      IF CO-CONTROL = HIGH-VALUE                                   00003859
00422          GO TO 8990-END-OUTPUT.                                   00003860
00423                                                                   00003861
00424      PERFORM 5000-PRT-DETAIL-RTN   THRU  5199-EXIT.               00003862
00425                                                                   00003863
00426      PERFORM 8300-COMBINE-CCM-RTN  THRU  8399-EXIT.               00003864
00427                                                                   00004270
00428      GO TO 4000-PROCESS-RTN.                                      00004271
00429                                                                   00004272
00430  4999-PROCESS-ERR.                                                00004273
00431      DISPLAY 'SUMM-CTL-1  = ' SUMM-CTL-1.                         00004274
00432      DISPLAY 'CO-CONTROL = ' CO-CONTROL.                          00004275
00433                                                                   00004276
00434      MOVE '0301'                 TO  WS-RETURN-CODE.              00004277
00435      MOVE 'FATAL DATA ERROR'     TO  WS-ABEND-MESSAGE.            00004278
00436                                                                   00004279
00437      GO TO ABEND-PGM.                                             00004280
00438  EJECT                                                            00004281
00439  5000-PRT-DETAIL-RTN.                                             00004282
00440      MOVE +1                     TO  STMT-SW.                     00004283
00441      MOVE RUN-MO                 TO  CO-ACT-MONTH.                00004284
00442      MOVE RUN-YR                 TO  CO-ACT-YEAR.                 00004285
00443      MOVE RUN-DA                 TO  CO-ACT-DAY.                  00004286
00444                                                                   00004287
00445      IF LNCTR GREATER +43                                         00004288
00446          PERFORM 8600-HD-RTN  THRU  8699-EXIT.                    00004289
00447                                                                   00004290
00448      IF SUMM-ACCTG                                                00004291
00449          GO TO 5120-PRT-ACCTG.                                    00004292
00450                                                                   00004500
00451      IF FIRST-ACTIVITY                                            00004501
00452          NEXT SENTENCE                                            00004502
00453      ELSE                                                         00004503
00454          GO TO 5020-PRT-SUMMARY.                                  00004504
00455                                                                   00004505
00456      MOVE 'N'                    TO  FIRST-ACTIVITY-SW.           00004506
00457                                                                   00004507
00458      IF S-OV-PYMT = +0                                            00004508
00459          GO TO 5020-PRT-SUMMARY.                                  00004509
00460                                                                   00004600
00461      SUBTRACT S-OV-PYMT          FROM  S-OV-END-BAL.              00004601
00462                                                                   00004602
00463      MOVE 'PRELIMINARY BALANCE'  TO  P-DESC.                      00004603
00464      MOVE S-OV-END-BAL           TO  P-DUE.                       00004604
00465      MOVE S-OV-END-BAL           TO  P-BEG.                       00004605
00466                                                                   00004606
00467      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00004607
00468                                                                   00004680
00469      MOVE SPACE-2                TO  P-CCSW.                      00004681
00470                                                                   00004682
00471  5020-PRT-SUMMARY.                                                00004683
00472      ADD SUMM-COMM               TO  S-COMM.                      00004684
00473      ADD SUMM-PMTS               TO  S-PYMT.                      00004685
00474      ADD SUMM-BEG-BAL            TO  S-BEG-BAL.                   00004686
00475      ADD SUMM-END-BAL            TO  S-END-BAL.                   00004687
00476      ADD SUMM-OV-LIFE            TO  S-OV-LIFE.                   00004688
00477      ADD SUMM-OV-AH              TO  S-OV-AH.                     00004689
011904     ADD SUMM-OV-AH-RFND         TO  S-OV-AH-RFND
040504     ADD SUMM-DLR-INC            TO  S-DLR-INC
           ADD SUMM-LMBA-FEE           TO  S-LMBA-FEE
00478      ADD SUMM-OV-B-LIFE          TO  S-OV-B-LIFE.                 00004690
00479      ADD SUMM-OV-B-AH            TO  S-OV-B-AH.                   00004691
00480                                                                   00004800
00481      IF SUMM-SUMMR                                                00004801
00482          MOVE +1                 TO  OVERWRITE-ONLY-SW.           00004802
00483                                                                   00004830
00484      ADD SUMM-OV-L-PREM          TO  S-L-PREM  S-PREM.            00004831
00485      ADD SUMM-OV-A-PREM          TO  S-A-PREM  S-PREM.            00004832
00486      ADD SUMM-OV-B-L-PREM        TO  S-OV-B-L-PREM.               00004833
00487      ADD SUMM-OV-B-A-PREM        TO  S-OV-B-A-PREM.               00004834
00488                                                                   00004835
00489      COMPUTE S-DUE  =  SUMM-END-BAL  -  SUMM-OV-LIFE              00004836
00490                     -  SUMM-OV-AH - SUMM-OV-AH-RFND
040504                    -  SUMM-DLR-INC - SUMM-LMBA-FEE
00491                                                                   00004910
00492      ADD S-DUE                   TO  S-OV-END-BAL.                00004911
00493                                                                   00004912
00494  5040-GA-BALANCE.                                                 00004913
00495      ADD SUMM-OV-LIFE            TO  S-UP-COMM.                   00004914
00496      ADD SUMM-OV-AH              TO  S-UP-COMM.                   00004915
100604     ADD SUMM-DLR-INC            TO  S-UP-COMM
100604     ADD SUMM-LMBA-FEE           TO  S-UP-COMM
00497                                                                   00004916
00498      IF SUMM-OVRWT                                                00004917
00499          GO TO 5060-ACCT-BALANCE.                                 00004918
00500                                                                   00004919
00501      IF SUMM-BAL-CTL = 'Y'                                        00004920
00502          GO TO 5060-ACCT-BALANCE.                                 00004921
00503                                                                   00004922
00504      ADD SUMM-PREM               TO  S-UP-PREM.                   00004923
00505      ADD SUMM-COMM               TO  S-UP-COMM.                   00004924
00506      ADD SUMM-PMTS               TO  S-UP-PYMT.                   00004925
00507                                                                   00005070
00508  5060-ACCT-BALANCE.                                               00005071
00509      IF SUMM-OVRWT                                                00005072
00510          GO TO 5080-PRT-OVRWT.                                    00005073
00511                                                                   00005074
00512  5070-PRT-SUMMR.                                                  00005075
00513      MOVE SUMM-AM-NO             TO  P-ACCT.                      00005076
00514                                                                   00005077
00515      IF SUMM-BAL-CTL = 'Y'                                        00005078
00516          MOVE +1                 TO  AST-SW                       00005079
00517          MOVE '*'                TO  P-AST.                       00005080
00518                                                                   00005081
00519      MOVE SPACE-2                TO  P-CCSW.                      00005082
00520      MOVE SUMM-NAME              TO  P-DESC.                      00005083
00521      MOVE SUMM-BEG-BAL           TO  P-BEG.                       00005084

122804     IF SEC-PAY-CARRIER (CLAS-INDEXCN)
122804        MOVE SUMM-PREM           TO  P-CNT
122804     ELSE
122804        MOVE SUMM-PREM           TO  P-PRM
122804     END-IF
00522 *    MOVE SUMM-PREM              TO  P-PRM.                       00005085
00523      MOVE SUMM-COMM              TO  P-COM.                       00005086
00524      MOVE SUMM-PMTS              TO  P-PMT.                       00005087
00525                                                                   00005088
00526      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00005089
00527                                                                   00005270
00528      MOVE 'ENDING BALANCE'       TO  P-DESC.                      00005271
00529      MOVE SUMM-END-BAL           TO  P-BEG.                       00005272
00530                                                                   00005273
00531      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00005274
00532                                                                   00005275
00533  5080-PRT-OVRWT.                                                  00005276
00534      MOVE SPACE-2                TO  P-CCSW.                      00005277
00535      MOVE SUMM-AM-NO             TO  P-ACCT.                      00005278
00536      MOVE SUMM-NAME              TO  P-DESC.                      00005279
00537      MOVE LIFE-OVERRIDE-L2       TO  P-BEN.                       00005280
00538      MOVE SUMM-OV-L-PREM         TO  P-PRM.                       00005281
00539      MOVE SUMM-OV-LIFE           TO  P-LOV.                       00005282
00540                                                                   00005283
00541      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00005284
00542                                                                   00005285
00543      MOVE AH-OVERRIDE-L2         TO  P-BEN.                       00005286
122804     IF SEC-PAY-CARRIER (CLAS-INDEXCN)
122804        MOVE SUMM-OV-A-PREM      TO  P-CNT
122804     ELSE
122804        MOVE SUMM-OV-A-PREM      TO  P-PRM
122804     END-IF
00544 *    MOVE SUMM-OV-A-PREM         TO  P-PRM.                       00005287
032905     COMPUTE P-LOV = SUMM-OV-AH + SUMM-LMBA-FEE
032905*    COMPUTE P-LOV = SUMM-OV-AH + SUMM-LMBA-FEE
032905*       + SUMM-DLR-INC
00545 *    MOVE SUMM-OV-AH             TO  P-LOV.                       00005288
011904     MOVE SUMM-OV-AH-RFND        TO  P-PMT
011904     ADD SUMM-OV-AH-RFND         TO  S-UP-PYMT
00546      MOVE S-DUE                  TO  P-DUE.                       00005289
040504
040504     IF SUMM-DLR-INC NOT = ZEROS
040504        PERFORM 8800-PRT-RTN     THRU 8899-EXIT
040504        MOVE SPACES              TO P-BEN
040504        MOVE ZEROS               TO P-PRM
040504        MOVE SUMM-DLR-INC        TO P-LOV
040504        MOVE ZEROS               TO P-PMT
040504        MOVE 'DEALER INCENTIVE ' TO P-DESC
040504     END-IF
00547                                                                   00005290
00548      GO TO 5190-PRT-DETAIL.                                       00005291
00549                                                                   00005292
00550  5120-PRT-ACCTG.                                                  00005293
00551      ADD SUMM-PMTS               TO  S-OV-PYMT.                   00005294
00552      ADD SUMM-PMTS               TO  S-UP-PYMT.                   00005295
00553      ADD SUMM-PMTS               TO  S-PYMT.                      00005296
00554                                                                   00005540
00555      SUBTRACT SUMM-PMTS          FROM  S-END-BAL.                 00005541
00556                                                                   00005542
00557      MOVE SUMM-NAME              TO  P-DESC.                      00005543
00558      MOVE SUMM-PMTS              TO  P-PMT.                       00005544
00559                                                                   00005545
00560      GO TO 5190-PRT-DETAIL.                                       00005546
00561                                                                   00005547
00562  5190-PRT-DETAIL.                                                 00005548
00563      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00005549
00564                                                                   00005550
00565  5199-EXIT.                                                       00005551
00566      EXIT.                                                        00005552
00567  EJECT                                                            00005553
00568  5400-PRT-TOTAL-RTN.                                              00005554
00569      IF STMT-SW = +0                                              00005555
00570          GO TO 5599-EXIT.                                         00005556
00571                                                                   00005557
00572      IF LNCTR GREATER +40                                         00005558
00573          PERFORM 8600-HD-RTN  THRU  8699-EXIT.                    00005559
00574                                                                   00005560
00575      MOVE 'COLUMN TOTALS '       TO  P-DESC.                      00005561
00576      MOVE S-BEG-BAL              TO  P-BEG.                       00005562
122804     IF SEC-PAY-CARRIER (CLAS-INDEXCN)
122804        MOVE S-PREM              TO  P-CNT
122804     ELSE
122804        MOVE S-PREM              TO  P-PRM
122804     END-IF
00577 *    MOVE S-PREM                 TO  P-PRM.                       00005563
00578      MOVE S-PYMT                 TO  P-PMT.                       00005564
00579      MOVE S-COMM                 TO  P-COM.                       00005565
00580                                                                   00005800
00581      IF S-OV-LIFE = ZERO                                          00005801
00582          MOVE S-OV-LIFE          TO  P-LOV                        00005802
00583      ELSE                                                         00005803
00584          MOVE LIFE-OVERRIDE-L2   TO  P-BEN                        00005804
00585          MOVE S-OV-LIFE          TO  P-LOV.                       00005805
00586                                                                   00005806
00587      MOVE SPACE-2                TO  P-CCSW.                      00005807
00588                                                                   00005808
00589      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00005809
00590                                                                   00005810
00591 *    IF S-OV-AH = ZERO                                            00005811
100604     IF (S-OV-AH + S-DLR-INC + S-LMBA-FEE) = ZERO
00592          MOVE S-OV-AH            TO  P-LOV                        00005812
00593      ELSE                                                         00005813
00594          MOVE AH-OVERRIDE-L2     TO  P-BEN                        00005814
040504         COMPUTE P-LOV = S-OV-AH + S-DLR-INC + S-LMBA-FEE
040504     END-IF
040504*        MOVE S-OV-AH            TO  P-LOV.                       00005815
00596                                                                   00005960
00597      MOVE S-OV-END-BAL           TO  P-DUE.                       00005961
00598                                                                   00005962
00599      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00005963
00600                                                                   00005964
00601      IF S-OV-B-PREM GREATER ZERO  OR                              00005965
00602         S-OV-B-LIFE GREATER ZERO  OR                              00005966
00603         S-OV-B-AH GREATER ZERO                                    00005967
00604          MOVE 'PREVIOUSLY BILLED'  TO  P-DESC                     00005968
00605          MOVE S-OV-B-PREM          TO  P-PRM                      00005969
00606          MOVE LIFE-OVERRIDE-L2     TO  P-BEN                      00005970
00607          MOVE S-OV-B-LIFE          TO  P-LOV                      00005971
00608          MOVE SPACE-2              TO  P-CCSW                     00005972
00609          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    00005973
00610          MOVE AH-OVERRIDE-L2       TO  P-BEN                      00005974
00611          MOVE S-OV-B-AH            TO  P-LOV                      00005975
00612          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   00005976
00613                                                                   00005977
00614      ADD S-OV-AH                 TO  TOT-OV-COMM.                 00005978
00615      ADD S-OV-LIFE               TO  TOT-OV-COMM.                 00005979
           ADD S-DLR-INC               TO  TOT-MISC-FEES
           ADD S-LMBA-FEE              TO  TOT-MISC-FEES
00616                                                                   00005980
00617      IF LNCTR GREATER +29                                         00005981
00618          PERFORM 8600-HD-RTN  THRU  8699-EXIT.                    00005982
00619                                                                   00005983
00620      MOVE SPACE-2                TO  P-REC.                       00005984
00621                                                                   00005985
00622      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00005986
00623                                                                   00005987
00624      MOVE 'TOTAL NET PREMIUM'    TO  T-DESC.                      00005988
00625      MOVE S-UP-PREM              TO  T-AMT.                       00005989
00626      MOVE 'A'                    TO  T-EXPL.                      00005990
00627      MOVE SPACE-2                TO  P-CCSW.                      00005991
00628                                                                   00005992
00629      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00005993
00630                                                                   00005994
00631      COMPUTE T-COMP  =  S-UP-COMM  -  S-OV-AH  -  S-OV-LIFE
100604        - S-DLR-INC - S-LMBA-FEE
00632                                                                   00005996
00633      MOVE 'ACCOUNT COMPENSATION' TO  T-DESC.                      00005997
00634      MOVE T-COMP                 TO  T-AMT.                       00005998
00635      MOVE 'B'                    TO  T-EXPL.                      00005999
00636      MOVE SPACE-2                TO  P-CCSW.                      00006000
00637                                                                   00006001
00638      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00006002
00639                                                                   00006003
00640      COMPUTE T-DUE-ACCT  =  S-UP-PREM  -  T-COMP.                 00006004
00641                                                                   00006410
00642      IF T-DUE-ACCT LESS ZERO                                      00006411
00643          MOVE 'DUE TO ACCOUNT THIS MONTH'                         00006412
00644                                  TO  T-DESC                       00006413
00645      ELSE                                                         00006414
00646          MOVE 'DUE FROM ACCOUNT THIS MONTH'                       00006415
00647                                  TO  T-DESC.                      00006416
00648                                                                   00006417
00649      MOVE T-DUE-ACCT             TO  T-AMT.                       00006418
00650      MOVE 'C=A-B'                TO  T-EXPL.                      00006419
00651      MOVE SPACE-2                TO  P-CCSW.                      00006420
00652                                                                   00006421
00653      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00006422
00654                                                                   00006423
00655      MOVE 'BALANCE BROUGHT FORWARD'                               00006424
00656                                  TO  T-DESC.                      00006425
00657      MOVE S-OV-BEG-BAL           TO  T-AMT.                       00006426
00658      MOVE 'D'                    TO  T-EXPL.                      00006427
00659      MOVE SPACE-2                TO  P-CCSW.                      00006428
00660                                                                   00006429
00661      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00006430
00662                                                                   00006431
00663      MOVE 'PAYMENTS & ADJUSTMENTS'                                00006432
00664                                  TO  T-DESC.                      00006433
00665      MOVE S-UP-PYMT              TO  T-AMT.                       00006434
00666      MOVE 'E'                    TO  T-EXPL.                      00006435
00667      MOVE SPACE-2                TO  P-CCSW.                      00006436
00668                                                                   00006437
00669      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00006438
00670                                                                   00006700
00671      COMPUTE T-OVERW  =  S-OV-LIFE  +  S-OV-AH
040504        + S-DLR-INC + S-LMBA-FEE 
00672                                                                   00006702
00673      MOVE 'OVERWRITE COMPENSATION'                                00006703
00674                                  TO  T-DESC.                      00006704
00675      MOVE T-OVERW                TO  T-AMT.                       00006705
00676      MOVE 'F'                    TO  T-EXPL.                      00006706
00677      MOVE SPACE-2                TO  P-CCSW.                      00006707
00678                                                                   00006708
00679      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00006709
00680                                                                   00006800
00681      MOVE SPACES                 TO  P-REC.                       00006801
00682      MOVE ALL '-'                TO  T-AMTR.                      00006802
00683      MOVE SPACE-2                TO  P-CCSW.                      00006803
00684                                                                   00006804
00685      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00006805
00686                                                                   00006806
00687      COMPUTE T-DUE  =  T-DUE-ACCT  +  S-OV-BEG-BAL                00006807
00688                     -  S-UP-PYMT   -  T-OVERW.                    00006808
00689                                                                   00006809
00690      IF T-DUE LESS ZERO                                           00006810
00691          MOVE 'DUE FROM COMPANY' TO  T-DESC                       00006811
00692      ELSE                                                         00006812
00693          MOVE 'DUE COMPANY'      TO  T-DESC.                      00006813
00694                                                                   00006814
00695      MOVE T-DUE                  TO  T-AMT.                       00006815
00696      MOVE 'G=C+D-E-F'            TO  T-EXPL.                      00006816
00697      MOVE SPACE-2                TO  P-CCSW.                      00006817
00698                                                                   00006818
00699      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00006819
00700                                                                   00007000
00701      IF AST-SW = +0                                               00007001
00702          GO TO 5599-EXIT.                                         00007002
00703                                                                   00007003
00704  5500-FIND-BOTTOM.                                                00007004
00705      IF LNCTR LESS +42                                            00007005
00706          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    00007006
00707          GO TO 5500-FIND-BOTTOM.                                  00007007
00708                                                                   00007080
00709      MOVE '              BALANCE CARRIED BY THE ACCOUNT'          00007081
00710                                  TO  P-LINE.                      00007082
00711      MOVE '*'                    TO  P-AST.                       00007083
00712      MOVE SPACE-3                TO  P-CCSW.                      00007084
00713                                                                   00007130
00714      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00007131
00715                                                                   00007132
00716  5599-EXIT.                                                       00007133
00717      EXIT.                                                        00007134
00718                                                                   00007180
00719  6000-UPDATE-MSTR-RTN.                                            00007181
00720      ADD S-OV-LIFE               TO  CO-YTD-OV.                   00007182
00721      ADD S-OV-AH                 TO  CO-YTD-OV.                   00007183
100604     ADD S-DLR-INC               TO  CO-YTD-OV
100604     ADD S-LMBA-FEE              TO  CO-YTD-OV
00722                                                                   00007184
00723      MOVE S-UP-COMM              TO  CO-CUR-COM.                  00007185
00724      MOVE S-UP-PREM              TO  CO-CUR-CHG.                  00007186
00725      MOVE S-UP-PYMT              TO  CO-CUR-PMT.                  00007187
00726                                                                   00007188
00727      COMPUTE CO-CUR  =  S-UP-PREM  -  S-UP-COMM.                  00007189
00728                                                                   00007190
00729      COMPUTE CO-END-BAL  =  CO-BAL-FWD  +  CO-CUR-CHG             00007191
00730                          -  CO-CUR-PMT  -  CO-CUR-COM.            00007192
00731                                                                   00007193
00732      IF STMT-SW = +1                                              00007194
00733          MOVE 'Y'                TO  CO-INTERNAL-CONTROL-2        00007195
00734      ELSE                                                         00007196
00735          MOVE 'N'                TO  CO-INTERNAL-CONTROL-2.       00007197
00736                                                                   00007198
00737      IF CO-CUR-PMT NEGATIVE                                       00007199
00738          SUBTRACT CO-CUR-PMT     FROM  CO-CUR                     00007200
00739      ELSE                                                         00007201
00740          SUBTRACT CO-CUR-PMT     FROM  CO-OV90.                   00007202
00741                                                                   00007203
00742      IF CO-CUR NEGATIVE                                           00007204
00743          ADD CO-CUR              TO  CO-OV90                      00007205
00744          MOVE +0                 TO  CO-CUR.                      00007206
00745                                                                   00007207
00746      IF CO-OV90 NEGATIVE                                          00007208
00747          ADD CO-OV90             TO  CO-OV60                      00007209
00748          MOVE +0                 TO  CO-OV90.                     00007210
00749                                                                   00007211
00750      IF CO-OV60 NEGATIVE                                          00007212
00751          ADD CO-OV60             TO  CO-OV30                      00007213
00752          MOVE +0                 TO  CO-OV60.                     00007214
00753                                                                   00007215
00754      IF CO-OV30 NEGATIVE                                          00007216
00755          ADD CO-OV30             TO  CO-CUR                       00007217
00756          MOVE +0                 TO  CO-OV30.                     00007218
00757                                                                   00007219
00758      COMPUTE X-ZERO  =  CO-CUR  +  CO-OV30  +  CO-OV60            00007220
00759                      +  CO-OV90.                                  00007221
00760                                                                   00007222
00761      IF CO-END-BAL NOT = X-ZERO  OR                               00007223
00762         CO-END-BAL LESS ZERO                                      00007224
00763          MOVE CO-END-BAL         TO  CO-CUR                       00007225
00764          MOVE +0                 TO  CO-OV30                      00007226
00765                                      CO-OV60                      00007227
00766                                      CO-OV90.                     00007228
00767                                                                   00007229
00768  6299-EXIT.                                                       00007230
00769      EXIT.                                                        00007231
00770  EJECT                                                            00007232
00771  8000-MSTR-CONTROL-RTN.                                           00007233
032406     IF (CO-CUR-COM <> 0) AND 
080707        (CO-FIRST-WRITTEN-DT = LOW-VALUES OR SPACES)
032406         MOVE BIN-RUN-DATE TO CO-FIRST-WRITTEN-DT
032406     END-IF.
032406
00772      MOVE COMPENSATION-MASTER    TO  COMP-OUT-RECORD.             00007234
00773                                                                   00007235
00774      IF COO-ID NOT = 'CO'                                         00007236
00775          GO TO 8010-READ-MSTR.                                    00007237
00776                                                                   00007238
00777      WRITE COMP-OUT-RECORD.                                       00007239
00778                                                                   00007240
00779      ADD +1                      TO  MSTR-CTL-RECS-IN.            00007241
00780                                                                   00007242
00781  8010-READ-MSTR.                                                  00007243
00782      IF CMI-END-OF-FILE                                           00007244
00783          MOVE COMP-IN-RECORD     TO  COMPENSATION-MASTER          00007245
00784          GO TO 8099-EXIT.                                         00007246
00785                                                                   00007247
00786      READ COMM-MSTR-IN                                            00007248
00787          AT END                                                   00007249
00788              MOVE 'Y'            TO  CMI-END-OF-FILE-SW           00007250
00789              MOVE HIGH-VALUE     TO  COMP-IN-RECORD               00007251
00790                                      COMPENSATION-MASTER          00007252
00791              GO TO 8099-EXIT.                                     00007253
00792                                                                   00007920
00793      ADD +1                      TO  MSTR-CTL-RECS-OUT.           00007921
00794                                                                   00007922
00795      MOVE COMP-IN-RECORD         TO  COMPENSATION-MASTER.         00007923
00796                                                                   00007924
00797      IF CO-CARRIER NOT = SAVE-CARRIER                             00007925
00798          MOVE CO-CARR-GROUP          TO  SAVE-CARR-GROUP          00007926
00799          MOVE SAVE-COMPANY-NAME (1)  TO  SAVE-COMPANY-NAME (2)    00007927
00800                                          SAVE-COMPANY-NAME (3).   00007928
00801                                                                   00007929
00802      IF CO-CARR-GROUP NOT = SAVE-CARR-GROUP                       00007930
00803          MOVE CO-CARR-GROUP          TO  SAVE-CARR-GROUP          00007931
00804          MOVE SAVE-COMPANY-NAME (2)  TO  SAVE-COMPANY-NAME (3).   00007932
00805                                                                   00007933
00806      IF CO-COMPANY-TYPE                                           00007934
00807          MOVE CO-ACCT-NAME       TO  SAVE-COMPANY-NAME (2)        00007935
00808                                      SAVE-COMPANY-NAME (3).       00007936
00809                                                                   00007937
00810      IF NOT CO-GEN-AGENT-TYPE                                     00007938
00811          GO TO 8000-MSTR-CONTROL-RTN.                             00007939
00812                                                                   00008120
00813      MOVE +0                     TO  S-OV-BEG-BAL   S-OV-END-BAL  00008121
00814                                      S-DUE          S-PREM        00008122
00815                                      S-COMM         S-PYMT        00008123
00816                                      S-OV-LIFE      S-OV-AH       00008124
011904                                     S-OV-AH-RFND
040504                                     S-DLR-INC      S-LMBA-FEE
00817                                      S-OV-PYMT      S-BEG-BAL     00008125
00818                                      S-END-BAL      S-OV-B-PREM   00008126
00819                                      S-L-PREM       S-A-PREM      00008127
00820                                      S-OV-B-L-PREM  S-OV-B-A-PREM 00008128
00821                                      S-OV-B-LIFE    S-OV-B-AH     00008129
00822                                      S-UP-COMM     S-UP-PREM      00008130
00823                                      S-UP-PYMT.                   00008131
00824      MOVE CO-BAL-FWD             TO  S-OV-BEG-BAL  S-OV-END-BAL   00008132
00825                                      S-BEG-BAL     S-END-BAL.     00008133
00826      MOVE +0                     TO  STMT-SW                      00008134
00827                                      OVERWRITE-ONLY-SW            00008135
00828                                      AST-SW.                      00008136
00829                                                                   00008290
00830      IF CO-BAL-FWD NOT = +0                                       00008291
00831          MOVE +1                 TO  STMT-SW.                     00008292
00832                                                                   00008320
00833      MOVE +0                     TO  PGCTR.                       00008321
00834      MOVE +66                    TO  LNCTR.                       00008322
00835      MOVE 'Y'                    TO  FIRST-ACTIVITY-SW.           00008323
00836      MOVE CO-CONTROL             TO  PRE-CONTROL.                 00008324
00837                                                                   00008325
00838  8099-EXIT.                                                       00008326
00839      EXIT.                                                        00008327
00840  EJECT                                                            00008328
00841  8200-RETURN-SORT-RTN.                                            00008329
00842      RETURN SORT-FILE                                             00008330
00843          AT END                                                   00008331
00844              MOVE HIGH-VALUE     TO  SORT-REC                     00008332
00845              GO TO 8299-EXIT.                                     00008333
00846                                                                   00008334
00847  8299-EXIT.                                                       00008335
00848      EXIT.                                                        00008336
00849                                                                   00008337
00850  8300-COMBINE-CCM-RTN.                                            00008338
00851      MOVE SORT-REC               TO  SUMM-REC.                    00008339
00852                                                                   00008340
00853  8320-COMBINE-CCM.                                                00008341
00854      IF SORT-CONTROL-74 = HIGH-VALUE                              00008342
00855          GO TO 8399-EXIT.                                         00008343
00856                                                                   00008344
00857      PERFORM 8200-RETURN-SORT-RTN  THRU  8299-EXIT.               00008345
00858                                                                   00008346
00859      IF SUMM-ACCTG                                                00008347
00860          GO TO 8399-EXIT.                                         00008348
00861                                                                   00008349
00862      IF SUMM-CONTROL NOT = SORT-CONTROL-74                        00008350
00863          GO TO 8390-CHECK-FOR-ZERO.                               00008351
00864                                                                   00008640
00865      MOVE SORT-BAL-CTL           TO  SUMM-BAL-CTL.                00008641
00866      MOVE SORT-NAME              TO  SUMM-NAME.                   00008642
00867      MOVE SORT-TYPE              TO  SUMM-TYPE.                   00008643
00868      ADD SORT-PREM               TO  SUMM-PREM.                   00008644
00869      ADD SORT-COMM               TO  SUMM-COMM.                   00008645
00870      ADD SORT-PMTS               TO  SUMM-PMTS.                   00008646
00871      ADD SORT-BEG-BAL            TO  SUMM-BEG-BAL.                00008647
00872      ADD SORT-END-BAL            TO  SUMM-END-BAL.                00008648
00873      ADD SORT-OV-L-PREM          TO  SUMM-OV-L-PREM.              00008649
00874      ADD SORT-OV-A-PREM          TO  SUMM-OV-A-PREM.              00008650
00875      ADD SORT-OV-LIFE            TO  SUMM-OV-LIFE.                00008651
00876      ADD SORT-OV-AH              TO  SUMM-OV-AH.                  00008652
011904     ADD SORT-OV-AH-RFND         TO  SUMM-OV-AH-RFND
040504     ADD SORT-DLR-INC            TO  SUMM-DLR-INC
           ADD SORT-LMBA-FEE           TO  SUMM-LMBA-FEE
00877      ADD SORT-OV-B-L-PREM        TO  SUMM-OV-B-L-PREM.            00008653
00878      ADD SORT-OV-B-A-PREM        TO  SUMM-OV-B-A-PREM.            00008654
00879      ADD SORT-OV-B-LIFE          TO  SUMM-OV-B-LIFE.              00008655
00880      ADD SORT-OV-B-AH            TO  SUMM-OV-B-AH.                00008656
00881                                                                   00008657
00882      GO TO 8320-COMBINE-CCM.                                      00008658
00883                                                                   00008659
00884  8390-CHECK-FOR-ZERO.                                             00008660
00885      IF SUMM-PREM        = +0 AND                                 00008661
00886         SUMM-COMM        = +0 AND                                 00008662
00887         SUMM-PMTS        = +0 AND                                 00008663
00888         SUMM-BEG-BAL     = +0 AND                                 00008664
00889         SUMM-END-BAL     = +0 AND                                 00008665
00890         SUMM-OV-L-PREM   = +0 AND                                 00008666
00891         SUMM-OV-A-PREM   = +0 AND                                 00008667
00892         SUMM-OV-LIFE     = +0 AND                                 00008668
00893         SUMM-OV-AH       = +0 AND                                 00008669
100604        SUMM-DLR-INC     = +0 AND
100604        SUMM-LMBA-FEE    = +0 AND
00894         SUMM-OV-B-L-PREM = +0 AND                                 00008670
00895         SUMM-OV-B-A-PREM = +0 AND                                 00008671
00896         SUMM-OV-B-LIFE   = +0 AND                                 00008672
120804        SUMM-OV-AH-RFND  = +0 AND
00897         SUMM-OV-B-AH     = +0                                     00008673
00898          GO TO 8300-COMBINE-CCM-RTN.                              00008674
00899                                                                   00008990
00900  8399-EXIT.                                                       00008991
00901      EXIT.                                                        00008992
00902  EJECT                                                            00008993
00903  8600-HD-RTN.                                                     00008994
00904      IF PGCTR = +0                                                00008995
00905          GO TO 8620-PRT-FIRST-PAGE.                               00008996
00906                                                                   00008997
00907      MOVE SPACE-2                             TO  P-CCSW.         00008998
00908      MOVE '          CONTINUED ON NEXT PAGE'  TO  P-LINE.         00008999
00909                                                                   00009090
00910      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00009100
00911                                                                   00009110
00912  8620-PRT-FIRST-PAGE.                                             00009111
00913      MOVE SAVE-COMPANY-NAME (3)  TO  HD-CO.                       00009112
00914      MOVE HD1                    TO  P-LINE.                      00009113
00915      MOVE SPACE-NP               TO  P-CCSW.                      00009114
00916                                                                   00009160
00917      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00009161
00918                                                                   00009162
00919      MOVE +1                     TO  LNCTR.                       00009163
00920                                                                   00009164
00921      ADD +1                      TO  PGCTR.                       00009165
00922                                                                   00009166
00923      MOVE PGCTR                  TO  HD-PG.                       00009167
00924      MOVE HD2                    TO  P-LINE.                      00009168
00925      MOVE SPACE-1                TO  P-CCSW.                      00009169
00926                                                                   00009170
00927      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00009171
00928                                                                   00009280
00929      MOVE HD3                    TO  P-LINE.                      00009281
00930      MOVE SPACE-1                TO  P-CCSW.                      00009282
00931                                                                   00009283
00932      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00009284
00933                                                                   00009285
00934  8640-PRT-RESP-HD.                                                00009286

           PERFORM VARYING CLAS-INDEXCN FROM +1 BY +1 UNTIL
              (CLAS-INDEXCN > CLAS-MAXCN)
              OR (CO-CARRIER = CARRIER-SUB (CLAS-INDEXCN))
           END-PERFORM
           IF CLAS-INDEXCN > CLAS-MAXCN
              DISPLAY ' PROBLEM FINDING CARRIER ' CO-CARRIER
           END-IF

      *    IF CO-BANK-FEE NOT NUMERIC
      *       MOVE +0                  TO CO-BANK-FEE
      *    END-IF
           IF (SEC-PAY-CARRIER (CLAS-INDEXCN))
032905*       AND (CO-BANK-FEE > +0)
              MOVE '  COUNT'           TO HD5-PRM-CNT
              MOVE '  CONTRACT FEE'    TO HD5-OW-HD
           ELSE
              MOVE 'PREMIUM'           TO HD5-PRM-CNT
              MOVE '  OVERWRITE   '    TO HD5-OW-HD
           END-IF

00935      MOVE SPACE-2                TO  P-CCSW.                      00009287
00936      MOVE CO-CARRIER             TO  P-CARRIER.                   00009288
00937      MOVE '-'                    TO  P-DASH-1.                    00009289
00938      MOVE CO-GROUPING            TO  P-GROUPING.                  00009290
00939      MOVE '-'                    TO  P-DASH-2.                    00009291
00940      MOVE CO-RESP-NO             TO  P-RESP-NO.                   00009292
00941                                                                   00009293
00942      IF CO-MAIL-NAME NOT = SPACES                                 00009294
00943          MOVE CO-MAIL-NAME       TO  P-NAME                       00009295
00944          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   00009296
00945                                                                   00009297
00946      IF CO-ACCT-NAME = SPACES                                     00009298
00947        OR  CO-ACCT-NAME = CO-MAIL-NAME                            00009299
00948          NEXT SENTENCE                                            00009300
00949      ELSE                                                         00009301
00950          MOVE CO-ACCT-NAME       TO  P-NAME                       00009302
00951          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   00009303
00952                                                                   00009304
00953      IF CO-ADDR-1 NOT = SPACES                                    00009305
00954          MOVE CO-ADDR-1          TO  P-NAME                       00009306
00955          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   00009307
00956                                                                   00009308
00957      IF CO-ADDR-2 NOT = SPACES                                    00009309
00958          MOVE CO-ADDR-2          TO  P-NAME                       00009310
00959          PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                   00009311
00960                                                                   00009312
00961      IF CO-ADDR-3 = SPACES  AND                                   00009313
00962         CO-ZIP    = SPACES                                        00009314
00963            GO TO 8650-CONTINUE.                                   00009315
00964                                                                   00009640
00965      MOVE CO-ADDR-3              TO  P-NAME.                      00009641
00966                                                                   00009642
00967      IF CO-ZIP = SPACES                                           00009643
00968          PERFORM 8800-PRT-RTN  THRU  8899-EXIT                    00009644
00969          GO TO 8650-CONTINUE.                                     00009645
00970                                                                   00009646
00971      MOVE CO-ZIP                 TO  WORK-ZIP-CODE.               00009647
00972      MOVE SPACES                 TO  ZIP-CODE.                    00009648
00973                                                                   00009649
00974 *******   CHECK FOR CANADIAN ZIP CODE                             00009650
00975      IF WZC-POS-1 NOT NUMERIC                                     00009651
00976          MOVE WZC-POST-CD1       TO  POST-CODE1                   00009652
00977          MOVE WZC-POST-CD2       TO  POST-CODE2                   00009653
00978       ELSE                                                        00009654
00979          MOVE WZC-PRIME          TO  ZIP-FIVE                     00009655
00980          IF WZC-PLUS4 = '0000'  OR  SPACES                        00009656
00981              MOVE SPACES         TO  ZIP-DASH                     00009657
00982                                      ZIP-FOUR                     00009658
00983           ELSE                                                    00009659
00984              MOVE '-'            TO  ZIP-DASH                     00009660
00985              MOVE WZC-PLUS4      TO  ZIP-FOUR.                    00009661
00986                                                                   00009662
00987      MOVE ZIP-CODE               TO  P-ZIP.                       00009663
00988      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00009664
00989                                                                   00009665
00990  8650-CONTINUE.                                                   00009666
00991      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00009667
00992                                                                   00009668
00993      MOVE HD4                    TO  P-LINE.                      00009669
00994                                                                   00009670
00995      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00009671
00996                                                                   00009960
00997      MOVE HD5                    TO  P-LINE.                      00009961
00998                                                                   00009962
00999      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00009963
01000                                                                   00009964
01001      MOVE SPACE-2                TO  P-CCSW.                      00009965
01002                                                                   00009966
01003      IF PGCTR NOT = +1                                            00009967
01004          GO TO 8699-EXIT.                                         00009968
01005                                                                   00010050
01006      MOVE 'BEGINNING BALANCE'    TO  P-DESC.                      00010051
01007      MOVE S-OV-BEG-BAL           TO  P-BEG.                       00010052
01008                                                                   00010053
01009      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00010054
01010                                                                   00010100
01011      MOVE SPACE-2                TO  P-CCSW.                      00010101
01012                                                                   00010120
01013  8699-EXIT.                                                       00010121
01014      EXIT.                                                        00010122
01015                                                                   00010123
01016  8800-PRT-RTN.                                                    00010124
01017      MOVE P-REC                  TO  PRT.                         00010125
01018      MOVE P-CCSW                 TO  X.                           00010126
01019                                                                   00010190
01020      IF P-CCSW = SPACE-1                                          00010191
01021          ADD +1                  TO  LNCTR                        00010192
01022      ELSE                                                         00010193
01023          IF P-CCSW = SPACE-2                                      00010194
01024              ADD +2              TO  LNCTR                        00010195
01025          ELSE                                                     00010196
01026              IF P-CCSW = SPACE-3                                  00010197
01027                  ADD +3          TO  LNCTR.                       00010198
01028                                                                   00010280
01029      MOVE SPACES                 TO  P-REC.                       00010281
01030      MOVE SPACE-1                TO  P-CCSW.                      00010282
01031                                                                   00010310
01032  8850-COPY-PRT-RTN.                                               00010311
01033                              COPY ELCPRT2.                        00010312
01034                                                                   00010313
01035  8899-EXIT.                                                       00010314
01036      EXIT.                                                        00010315
01037                                                                   00010316
01038  8990-END-OUTPUT.                                                 00010317
01039      MOVE +0                     TO  PGCTR.                       00010318
01040                                                                   00010319
01041      PERFORM 8620-PRT-FIRST-PAGE.                                 00010320
01042                                                                   00010321
01043      MOVE 'MASTER RECORDS IN'    TO  P-NAME.                      00010322
01044      MOVE MSTR-CTL-RECS-IN       TO  P-CTR.                       00010323
01045      MOVE SPACE-2                TO  P-CCSW.                      00010324
01046                                                                   00010325
01047      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00010326
01048                                                                   00010327
01049      MOVE 'MASTER RECORDS OUT'   TO  P-NAME.                      00010328
01050      MOVE MSTR-CTL-RECS-OUT      TO  P-CTR.                       00010329
01051                                                                   00010330
01052      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00010331
01053                                                                   00010332
01054      MOVE 'TOTAL OVERWRITE COMPENSATION PROCESSED'                00010333
01055                                  TO  P-TOT-NAME.                  00010334
01056      MOVE TOT-OV-COMM            TO  P-TOT-PRM.                   00010335
01057      MOVE SPACE-2                TO  P-CCSW.                      00010336
01058                                                                   00010337
01059      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00010338
01060                                                                   00010339
01054      MOVE 'TOTAL MISC FEES                       '                00010333
01055                                  TO  P-TOT-NAME.                  00010334
01056      MOVE TOT-MISC-FEES          TO  P-TOT-PRM.                   00010335
01057      MOVE SPACE-2                TO  P-CCSW.                      00010336
01058                                                                   00010337
01059      PERFORM 8800-PRT-RTN  THRU  8899-EXIT.                       00010338
01060                                                                   00010339
01061      CLOSE COMM-MSTR-IN                                           00010340
01062            COMM-MSTR-OUT                                          00010341
01063            PRNTR.                                                 00010342
01064                                                                   00010343
01065  8999-EXIT.                                                       00010344
01066      EXIT.                                                        00010345
01067  EJECT                                                            00010346
01068  9000-END-OF-JOB SECTION.                                         00010347
01069                                                                   00010348
01070  9990-E-O-J.                                                      00010349
01071                              COPY ELCPRTC.                        00010350
01072                                                                   00010351
01073      GOBACK.                                                      00010352
01074                                                                   00010353
01075  ABEND-PGM SECTION.                                               00010354
01076                              COPY ELCABEND.                       00010355
