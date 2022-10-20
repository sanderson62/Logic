00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 ECS999.                              00000030
00004 *                            VMOD=2.013                           00000031
00005  AUTHOR.     LOGIC, INC.                                          00000050
00006              DALLAS, TEXAS.                                       00000060
00007                                                                   00000070
00008  DATE-COMPILED.                                                   00000080
00009                                                                   00000090
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
00020 *REMARKS.                                                         00000200
00021                                                                   00000210
00022 *    THIS PROGRAM CONVERTS THE SCS LEVEL 6.0 'BATCH' FILES TO     00000220
00023 *                 TO THE 'ENHANCED' LEVEL 2.001 FORMATS.          00000230
00024                                                                   00000240
00025                                                                   00000250
00026  EJECT                                                            00000260
00027  ENVIRONMENT DIVISION.                                            00000270
00028  INPUT-OUTPUT SECTION.                                            00000280
00029  FILE-CONTROL.                                                    00000290
00030                                                                   00000300
00031      SELECT CONTROL-CARD  ASSIGN TO SYS006-UR-2540R-S-SYS006.     00000310
00032                                                                   00000320
00033      SELECT DISK-DATE     ASSIGN TO SYS007-UT-FBA1-S-SYS007.      00000330
00034                                                                   00000340
00035      SELECT ACCT-IN       ASSIGN TO SYS010-UT-2400-S-SYS010.      00000350
00036      SELECT ACCT-OUT      ASSIGN TO SYS011-FBA1-ERVACCT           00000360
00037                     ORGANIZATION IS INDEXED                       00000370
00038                     ACCESS IS SEQUENTIAL                          00000380
00039                     RECORD KEY IS ERVACCT-KEY                     00000390
00040                     FILE STATUS IS ERVACCT-FILE-STATUS.           00000400
00041                                                                   00000410
00042      SELECT CERT-IN       ASSIGN TO SYS012-UT-2400-S-SYS012.      00000420
00043      SELECT CERT-OUT      ASSIGN TO SYS013-UT-2400-S-SYS013.      00000430
00044                                                                   00000440
00045      SELECT CLMS-IN       ASSIGN TO SYS014-UT-2400-S-SYS014.      00000450
00046      SELECT CLMS-OUT      ASSIGN TO SYS015-UT-2400-S-SYS015.      00000460
00047                                                                   00000470
00048      SELECT COMP-IN       ASSIGN TO SYS016-UT-2400-S-SYS016.      00000480
00049      SELECT COMP-OUT      ASSIGN TO SYS017-UT-2400-S-SYS017.      00000490
00050                                                                   00000500
00051      SELECT CTBL-IN       ASSIGN TO SYS018-UT-2400-S-SYS018.      00000510
00052      SELECT CTBL-OUT      ASSIGN TO SYS019-FBA1-ERVCTBL           00000520
00053                     ORGANIZATION IS INDEXED                       00000530
00054                     ACCESS IS SEQUENTIAL                          00000540
00055                     RECORD KEY IS ERVCTBL-KEY                     00000550
00056                     FILE STATUS IS ERVCTBL-FILE-STATUS.           00000560
00057                                                                   00000570
00058      SELECT EPEC-IN       ASSIGN TO SYS020-UT-2400-S-SYS020.      00000580
00059      SELECT EPEC-OUT      ASSIGN TO SYS021-UT-2400-S-SYS021.      00000590
00060                                                                   00000600
00061      SELECT RTBL-IN       ASSIGN TO SYS022-UT-2400-S-SYS022.      00000610
00062      SELECT RTBL-OUT      ASSIGN TO SYS023-FBA1-ERVRTBL           00000620
00063                     ORGANIZATION IS INDEXED                       00000630
00064                     ACCESS IS SEQUENTIAL                          00000640
00065                     RECORD KEY IS ERVRTBL-KEY                     00000650
00066                     FILE STATUS IS ERVRTBL-FILE-STATUS.           00000660
00067                                                                   00000670
00068      SELECT RATE-IN       ASSIGN TO SYS024-UT-2400-S-SYS024.      00000680
00069      SELECT RATE-OUT      ASSIGN TO SYS025-FBA1-ERVRATE           00000690
00070                     ORGANIZATION IS INDEXED                       00000700
00071                     ACCESS IS SEQUENTIAL                          00000710
00072                     RECORD KEY IS ERVRATE-KEY                     00000720
00073                     FILE STATUS IS ERVRATE-FILE-STATUS.           00000730
00074                                                                   00000740
00075      SELECT ALPH-IN       ASSIGN TO SYS026-UT-2400-S-SYS026.      00000750
00076      SELECT ALPH-OUT      ASSIGN TO SYS027-UT-2400-S-SYS027.      00000760
00077                                                                   00000770
00078      SELECT GAAP-IN       ASSIGN TO SYS028-UT-2400-S-SYS028.      00000780
00079      SELECT GAAP-OUT      ASSIGN TO SYS029-UT-2400-S-SYS029.      00000790
00080                                                                   00000800
00081  EJECT                                                            00000810
00082  DATA DIVISION.                                                   00000820
00083  FILE SECTION.                                                    00000830
00084                                                                   00000840
00085  FD  CONTROL-CARD                                                 00000850
00086      RECORDING MODE F                                             00000860
00087      LABEL RECORDS OMITTED                                        00000870
00088      RECORD CONTAINS 80 CHARACTERS.                               00000880
00089  01  CONTROL-CARD-RECORD.                                         00000890
00090      12  FILE-OPTION         PIC X(4).                            00000900
00091          88  CONVERTING-ALL-FILES     VALUE 'ALL '.               00000910
00092          88  CONVERTING-ACCT          VALUE 'ACCT'.               00000920
00093          88  CONVERTING-CERT          VALUE 'CERT'.               00000930
00094          88  CONVERTING-CLMS          VALUE 'CLMS'.               00000940
00095          88  CONVERTING-COMP          VALUE 'COMP'.               00000950
00096          88  CONVERTING-CTBL          VALUE 'CTBL'.               00000960
00097          88  CONVERTING-EPEC          VALUE 'EPEC'.               00000970
00098          88  CONVERTING-EXTR          VALUE 'EXTR'.               00000980
00099          88  CONVERTING-RTBL          VALUE 'RTBL'.               00000990
00100          88  CONVERTING-RATE          VALUE 'RATE'.               00001000
00101          88  CONVERTING-ALPH          VALUE 'ALPH'.               00001010
00102          88  CONVERTING-GAAP          VALUE 'GAAP'.               00001020
00103      12  FILLER                   PIC X(76).                      00001030
00104                                                                   00001040
00105  FD  DISK-DATE                                                    00001050
00106                              COPY ELCDTEFD.                       00001060
00107                                                                   00001070
00108  FD  ACCT-IN                                                      00001080
00109      RECORDING MODE F                                             00001090
00110      LABEL RECORDS STANDARD                                       00001100
00111      BLOCK CONTAINS 0 RECORDS                                     00001110
00112      RECORD CONTAINS 1400 CHARACTERS.                             00001120
00113  01  SCS-ACC-MSTR-REC        PIC X(1400).                         00001130
00114                                                                   00001140
00115  FD  ACCT-OUT                                                     00001150
00116      LABEL RECORDS ARE STANDARD                                   00001160
00117      RECORD CONTAINS 2000 CHARACTERS.                             00001170
00118  01  ECS-ACCT-RECORD.                                             00001180
00119      12  FILLER              PIC XX.                              00001190
00120      12  ERVACCT-KEY         PIC X(26).                           00001200
00121      12  FILLER              PIC X(1972).                         00001210
00122                                                                   00001220
00123  FD  CERT-IN                                                      00001230
00124      RECORDING MODE F                                             00001240
00125      LABEL RECORDS STANDARD                                       00001250
00126      BLOCK CONTAINS 0 RECORDS                                     00001260
00127      RECORD CONTAINS 400 CHARACTERS.                              00001270
00128  01  SCS-CERTIFICATE-MASTER  PIC X(400).                          00001280
00129                                                                   00001290
00130  FD  CERT-OUT                                                     00001300
00131      RECORDING MODE F                                             00001310
00132      LABEL RECORDS STANDARD                                       00001320
00133      BLOCK CONTAINS 0 RECORDS                                     00001330
00134      RECORD CONTAINS 700 CHARACTERS.                              00001340
00135  01  ECS-CERTIFICATE-MASTER  PIC X(700).                          00001350
00136                                                                   00001360
00137  FD  COMP-IN                                                      00001370
00138      RECORDING MODE F                                             00001380
00139      LABEL RECORDS STANDARD                                       00001390
00140      BLOCK CONTAINS 0 RECORDS                                     00001400
00141      RECORD CONTAINS 300 CHARACTERS.                              00001410
00142  01  SCS-COMP-RECORD         PIC X(300).                          00001420
00143                                                                   00001430
00144  FD  COMP-OUT                                                     00001440
00145      RECORDING MODE F                                             00001450
00146      LABEL RECORDS STANDARD                                       00001460
00147      BLOCK CONTAINS 0 RECORDS                                     00001470
00148      RECORD CONTAINS 700 CHARACTERS.                              00001471
00149  01  ECS-COMP-RECORD         PIC X(700).                          00001472
00150                                                                   00001500
00151  FD  CLMS-IN                                                      00001510
00152      RECORDING MODE F                                             00001520
00153      LABEL RECORDS STANDARD                                       00001530
00154      BLOCK CONTAINS 0 RECORDS                                     00001540
00155      RECORD CONTAINS 310 CHARACTERS.                              00001550
00156  01  SCS-CLMS-RECORD         PIC X(310).                          00001560
00157                                                                   00001570
00158  FD  CLMS-OUT                                                     00001580
00159      RECORDING MODE F                                             00001590
00160      LABEL RECORDS STANDARD                                       00001600
00161      BLOCK CONTAINS 0 RECORDS                                     00001610
00162      RECORD CONTAINS 510 CHARACTERS.                              00001620
00163  01  ECS-CLMS-RECORD         PIC X(510).                          00001630
00164                                                                   00001640
00165  FD  CTBL-IN                                                      00001650
00166      RECORDING MODE F                                             00001660
00167      LABEL RECORDS STANDARD                                       00001670
00168      BLOCK CONTAINS 0 RECORDS                                     00001680
00169      RECORD CONTAINS 150 CHARACTERS.                              00001690
00170  01  SCS-CTBL-RECORD         PIC X(150).                          00001700
00171                                                                   00001710
00172  FD  CTBL-OUT                                                     00001720
00173      LABEL RECORDS STANDARD                                       00001730
00174      RECORD CONTAINS 200 CHARACTERS.                              00001740
00175  01  ECS-CTBL-RECORD.                                             00001750
00176      12  FILLER              PIC XX.                              00001760
00177      12  ERVCTBL-KEY         PIC X(7).                            00001770
00178      12  FILLER              PIC X(191).                          00001780
00179                                                                   00001790
00180  FD  EPEC-IN                                                      00001800
00181      RECORDING MODE F                                             00001810
00182      LABEL RECORDS STANDARD                                       00001820
00183      BLOCK CONTAINS 0 RECORDS                                     00001830
00184      RECORD CONTAINS 310 CHARACTERS.                              00001840
00185  01  SCS-EPEC-RECORD         PIC X(310).                          00001850
00186                                                                   00001860
00187  FD  EPEC-OUT                                                     00001870
00188      RECORDING MODE F                                             00001880
00189      LABEL RECORDS STANDARD                                       00001890
00190      BLOCK CONTAINS 0 RECORDS                                     00001900
00191      RECORD CONTAINS 325 CHARACTERS.                              00001910
00192  01  ECS-EPEC-RECORD         PIC X(325).                          00001920
00193                                                                   00001930
00194  FD  RTBL-IN                                                      00001940
00195      RECORDING MODE F                                             00001950
00196      LABEL RECORDS STANDARD                                       00001960
00197      BLOCK CONTAINS 0 RECORDS                                     00001970
00198      RECORD CONTAINS 2100 CHARACTERS.                             00001980
00199  01  SCS-RTBL-RECORD         PIC X(2100).                         00001990
00200                                                                   00002000
00201  FD  RTBL-OUT                                                     00002010
00202      LABEL RECORDS STANDARD                                       00002020
00203      RECORD CONTAINS 4000 CHARACTERS.                             00002030
00204  01  ECS-RTBL-RECORD.                                             00002040
00205      12  FILLER              PIC XX.                              00002050
00206      12  ERVRTBL-KEY         PIC X(8).                            00002060
00207      12  FILLER              PIC X(3990).                         00002070
00208                                                                   00002080
00209  FD  RATE-IN                                                      00002090
00210      RECORDING MODE F                                             00002100
00211      LABEL RECORDS STANDARD                                       00002110
00212      BLOCK CONTAINS 0 RECORDS                                     00002120
00213      RECORD CONTAINS 1240 CHARACTERS.                             00002130
00214  01  SCS-RATE-RECORD         PIC X(1240).                         00002140
00215                                                                   00002150
00216  FD  RATE-OUT                                                     00002160
00217      LABEL RECORDS STANDARD                                       00002170
00218      RECORD CONTAINS 1765 CHARACTERS.                             00002180
00219                                                                   00002190
00220  01  ECS-RATE-RECORD.                                             00002200
00221      12  FILLER              PIC XX.                              00002210
00222      12  ERVRATE-KEY         PIC X(28).                           00002220
00223      12  FILLER              PIC X(1735).                         00002230
00224                                                                   00002240
00225  FD  ALPH-IN                                                      00002250
00226      RECORDING MODE F                                             00002260
00227      LABEL RECORDS STANDARD                                       00002270
00228      BLOCK CONTAINS 0 RECORDS                                     00002280
00229      RECORD CONTAINS 150 CHARACTERS.                              00002290
00230  01  SCS-ALPH-RECORD         PIC X(150).                          00002300
00231                                                                   00002310
00232  FD  ALPH-OUT                                                     00002320
00233      RECORDING MODE F                                             00002330
00234      LABEL RECORDS STANDARD                                       00002340
00235      BLOCK CONTAINS 0 RECORDS                                     00002350
00236      RECORD CONTAINS 250 CHARACTERS.                              00002360
00237  01  ECS-ALPH-RECORD         PIC X(250).                          00002370
00238                                                                   00002380
00239  FD  GAAP-IN                                                      00002390
00240      RECORDING MODE F                                             00002400
00241      LABEL RECORDS STANDARD                                       00002410
00242      BLOCK CONTAINS 0 RECORDS                                     00002420
00243      RECORD CONTAINS 325 CHARACTERS.                              00002430
00244  01  SCS-GAAP-RECORD         PIC X(325).                          00002440
00245                                                                   00002450
00246  FD  GAAP-OUT                                                     00002460
00247      RECORDING MODE F                                             00002470
00248      LABEL RECORDS STANDARD                                       00002480
00249      BLOCK CONTAINS 0 RECORDS                                     00002490
00250      RECORD CONTAINS 365 CHARACTERS.                              00002500
00251  01  ECS-GAAP-RECORD         PIC X(365).                          00002510
00252  EJECT                                                            00002520
00253  WORKING-STORAGE SECTION.                                         00002530
00254  77  FILLER  PIC X(32) VALUE '********************************'.  00002540
00255  77  FILLER  PIC X(32) VALUE '     ECS999  WORKING-STORAGE    '.  00002550
00256  77  FILLER  PIC X(32) VALUE '********** VMOD=2.013 **********'.  00002551
00257                                                                   00002570
00258  77  PGM-SUB                          PIC S9(4)     VALUE +009.   00002580
00259                                                                   00002590
00260  77  X                                PIC S999   VALUE ZERO.      00002600
00261  77  WS-ZERO                          PIC S9     VALUE ZERO.      00002610
00262  77  WS-RETURN-CODE                   PIC S9(4)  VALUE +0  COMP.  00002620
00263  77  WS-ABEND-FILE-STATUS             PIC XX     VALUE ZEROS.     00002630
00264  77  WS-ABEND-MESSAGE                 PIC X(80)  VALUE SPACES.    00002640
00265                                                                   00002651
00266  77  WS-BIN-MAINT-DT                  PIC XX.                     00002660
00267  77  WS-BIN-EFF-DT                    PIC XX.                     00002670
00268  77  PRIOR-CNTRL-A                    PIC X(12)  VALUE SPACES.    00002680
00269                                                                   00002690
00270  77  ERVACCT-FILE-STATUS              PIC XX     VALUE ZEROS.     00002700
00271  77  ERVCTBL-FILE-STATUS              PIC XX     VALUE ZEROS.     00002710
00272  77  ERVRATE-FILE-STATUS              PIC XX     VALUE ZEROS.     00002720
00273  77  ERVRTBL-FILE-STATUS              PIC XX     VALUE ZEROS.     00002730
00274                                                                   00002740
00275  01  WS-REIN-AGENT.                                               00002750
00276      12  WS-REINCO-NO                 PIC XXX.                    00002760
00277      12  WS-REINCO-SUB                PIC XXX.                    00002770
00278                                                                   00002780
00279  01  WS-1ST-PROD-DATE.                                            00002790
00280      12  WS-1ST-PROD-YR               PIC 99.                     00002800
00281      12  WS-1ST-PROD-MO               PIC 99.                     00002810
00282      12  WS-1ST-PROD-DA               PIC 99.                     00002820
00283                                                                   00002830
00284  01  WS-INITIALS.                                                 00002840
00285      12  WS-1ST-INIT                  PIC X.                      00002850
00286      12  WS-MID-INIT                  PIC X.                      00002860
00287                                                                   00002870
00288  01  WS-COUNTY-PARISH.                                            00002871
00289      12  WS-COUNTY                    PIC XXX.                    00002872
00290      12  WS-PARISH                    PIC XXX.                    00002873
00291                                                                   00002910
00292  01  WS-CHECK-NUMBER.                                             00002911
00293      12  FILLER                       PIC XX        VALUE '00'.   00002912
00294      12  WS-CHECK-NO                  PIC X(5).                   00002913
00295                                                                   00002914
00296  01  CONVERSION-TOTALS.                                           00002915
00297      12  DISPLAY-RCDS                 PIC Z,ZZZ,ZZZ.              00002916
00298      12  ACCT-RCDS                    PIC 9(7)      VALUE ZERO.   00002917
00299      12  CERT-RCDS                    PIC 9(7)      VALUE ZERO.   00002918
00300      12  CLMS-RCDS                    PIC 9(7)      VALUE ZERO.   00002919
00301      12  COMP-RCDS                    PIC 9(7)      VALUE ZERO.   00002920
00302      12  CTBL-RCDS                    PIC 9(7)      VALUE ZERO.   00002921
00303      12  EPEC-RCDS                    PIC 9(7)      VALUE ZERO.   00002922
00304      12  RTBL-RCDS                    PIC 9(7)      VALUE ZERO.   00002923
00305      12  RATE-RCDS                    PIC 9(7)      VALUE ZERO.   00002924
00306      12  ALPH-RCDS                    PIC 9(7)      VALUE ZERO.   00002925
00307      12  GAAP-RCDS                    PIC 9(7)      VALUE ZERO.   00002926
00308                                                                   00002927
00309      12  DTH-AT-LUMP                  PIC 9(7)      VALUE ZERO.   00002928
00310      12  CAN-AT-LUMP                  PIC 9(7)      VALUE ZERO.   00002929
00311      12  LUMP-AT-DTH                  PIC 9(7)      VALUE ZERO.   00002930
00312      12  CAN-AT-DTH                   PIC 9(7)      VALUE ZERO.   00002931
00313      12  DTH-AT-CAN                   PIC 9(7)      VALUE ZERO.   00002932
00314      12  LUMP-AT-CAN                  PIC 9(7)      VALUE ZERO.   00002933
00315                                                                   00002934
00316  EJECT                                                            00003160
00317                              COPY ELCDATE.                        00003161
00318  EJECT                                                            00003180
00319                              COPY ELCDTECX.                       00003181
00320  EJECT                                                            00003200
00321 *01  ACCOUNT-MASTER          COPY ERCACCT.                        00003201
00322  01  ACCOUNT-MASTER.                                              00003202
00323      12  AM-RECORD-ID                      PIC XX.                00003203
00324          88  VALID-AM-ID                      VALUE 'AM'.         00003204
00325                                                                   00003205
00326      12  AM-CONTROL-PRIMARY.                                      00003206
00327          16  AM-COMPANY-CD                 PIC X.                 00003207
00328          16  AM-MSTR-CNTRL.                                       00003208
00329              20  AM-CONTROL-A.                                    00003209
00330                  24  AM-CARRIER            PIC X.                 00003210
00331                  24  AM-GROUPING.                                 00003211
00332                      28 AM-GROUPING-PREFIX PIC XXX.               00003212
00333                      28 AM-GROUPING-PRIME  PIC XXX.               00003213
00334                  24  AM-STATE              PIC XX.                00003214
00335                  24  AM-ACCOUNT.                                  00003215
00336                      28  AM-ACCOUNT-PREFIX PIC X(4).              00003216
00337                      28  AM-ACCOUNT-PRIME  PIC X(6).              00003217
00338              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A            00003218
00339                                            PIC X(19).             00003219
00340              20  AM-CNTRL-B.                                      00003220
00341                  24  AM-EXPIRATION-DT      PIC XX.                00003221
00342                  24  FILLER                PIC X(4).              00003222
00343              20  AM-CNTRL-2   REDEFINES   AM-CNTRL-B.             00003223
00344                  24  AM-EXPIRE-DT.                                00003224
00345                      28  AM-EXP-YR         PIC 99.                00003225
00346                      28  AM-EXP-MO         PIC 99.                00003226
00347                      28  AM-EXP-DA         PIC 99.                00003227
00348                                                                   00003228
00349      12  AM-CONTROL-BY-VAR-GRP.                                   00003229
00350          16  AM-COMPANY-CD-A1              PIC X.                 00003230
00351          16  AM-VG-CARRIER                 PIC X.                 00003231
00352          16  AM-VG-GROUPING                PIC X(6).              00003232
00353          16  AM-VG-STATE                   PIC XX.                00003233
00354          16  AM-VG-ACCOUNT                 PIC X(10).             00003234
00355          16  AM-VG-DATE.                                          00003235
00356              20  AM-VG-EXPIRATION-DT       PIC XX.                00003236
00357              20  FILLER                    PIC X(4).              00003237
00358          16  AM-VG-EXP-DATE   REDEFINES   AM-VG-DATE.             00003238
00359              20  AM-VG-EXP-YR              PIC 99.                00003239
00360              20  AM-VG-EXP-MO              PIC 99.                00003240
00361              20  AM-VG-EXP-DA              PIC 99.                00003241
00362                                                                   00003242
00363      12  AM-MAINT-INFORMATION.                                    00003243
00364          16  AM-LAST-MAINT-DT              PIC XX.                00003244
00365          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3. 00003245
00366          16  AM-LAST-MAINT-USER            PIC X(4).              00003246
00367          16  FILLER                        PIC XX.                00003247
00368                                                                   00003248
00369      12  AM-EFFECTIVE-DT                   PIC XX.                00003249
00370      12  AM-EFFECT-DT.                                            00003250
00371          16  AM-EFF-YR                     PIC 99.                00003251
00372          16  AM-EFF-MO                     PIC 99.                00003252
00373          16  AM-EFF-DA                     PIC 99.                00003253
00374                                                                   00003254
00375      12  AM-PREV-DATES.                                           00003255
00376          16  AM-PREV-EXP-DT                PIC X(6).              00003256
00377          16  AM-PREV-EFF-DT                PIC X(6).              00003257
00378                                                                   00003258
00379      12  AM-REPORT-CODE-1                  PIC X(10).             00003259
00380      12  AM-REPORT-CODE-2                  PIC X(10).             00003260
00381                                                                   00003261
00382      12  AM-CITY-CODE                      PIC X(4).              00003262
00383      12  AM-COUNTY-PARISH                  PIC X(6).              00003263
00384                                                                   00003264
00385      12  AM-NAME                           PIC X(30).             00003265
00386      12  AM-PERSON                         PIC X(30).             00003266
00387      12  AM-ADDRS                          PIC X(30).             00003267
00388      12  AM-CITY                           PIC X(30).             00003268
00389      12  AM-ZIP.                                                  00003269
00390          16  AM-ZIP-PRIME                  PIC X(5).              00003270
00391          16  AM-ZIP-PLUS4                  PIC X(4).              00003271
00392      12  AM-TEL-NO.                                               00003272
00393          16  AM-AREA-CODE                  PIC 999.               00003273
00394          16  AM-TEL-PRE                    PIC 999.               00003274
00395          16  AM-TEL-NBR                    PIC 9(4).              00003275
00396      12  AM-TEL-LOC                        PIC X.                 00003276
00397          88  AM-TEL-AT-HOME                   VALUE 'H'.          00003277
00398          88  AM-TEL-AT-BUSINESS               VALUE 'B'.          00003278
00399                                                                   00003279
00400      12  AM-COMM-STRUCTURE.                                       00003280
00401          16  AM-DEFN-1.                                           00003281
00402              20  AM-AGT-COMMS       OCCURS 10 TIMES.              00003282
00403                  24  AM-AGT.                                      00003283
00404                      28  AM-AGT-PREFIX     PIC X(4).              00003284
00405                      28  AM-AGT-PRIME      PIC X(6).              00003285
00406                  24  AM-COM-TYP            PIC X.                 00003286
00407                  24  AM-L-COM              PIC SV9(5)     COMP-3. 00003287
00408                  24  AM-J-COM              PIC SV9(5)     COMP-3. 00003288
00409                  24  AM-A-COM              PIC SV9(5)     COMP-3. 00003289
00410                  24  AM-RECALC-LV-INDIC    PIC X.                 00003290
00411                  24  AM-RETRO-LV-INDIC     PIC X.                 00003291
00412                  24  AM-GL-CODES           PIC X.                 00003292
00413                  24  FILLER                PIC XXX.               00003293
00414          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.                   00003294
00415              20  AM-COM-TBLS        OCCURS 10 TIMES.              00003295
00416                  24  FILLER                PIC X(11).             00003296
00417                  24  AM-L-COMA             PIC XXX.               00003297
00418                  24  AM-J-COMA             PIC XXX.               00003298
00419                  24  AM-A-COMA             PIC XXX.               00003299
00420                  24  FILLER                PIC X(6).              00003300
00421                                                                   00003301
00422      12  AM-COMM-CHANGE-STATUS             PIC X.                 00003302
00423          88  AM-COMMISSIONS-CHANGED           VALUE '*'.          00003303
00424                                                                   00003304
00425      12  FILLER                            PIC X(4).              00003305
00426                                                                   00003306
00427      12  AM-BILLING-STATUS                 PIC X.                 00003307
00428          88  AM-ACCOUNT-BILLED                VALUE 'B'.          00003308
00429          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.          00003309
00430      12  AM-AUTO-REFUND-SW                 PIC X.                 00003310
00431          88  AUTO-REFUNDS-USED                VALUE 'Y'.          00003311
00432          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.      00003312
00433      12  AM-GPCD                           PIC 99.                00003313
00434      12  AM-IG                             PIC X.                 00003314
00435          88  AM-HAS-INDIVIDUAL                VALUE 'I'.          00003315
00436          88  AM-HAS-GROUP                     VALUE 'G'.          00003316
00437      12  AM-STATUS                         PIC X.                 00003317
00438      12  AM-REMIT-TO                       PIC 99.                00003318
00439      12  AM-ID-NO                          PIC X(11).             00003319
00440                                                                   00003320
00441      12  AM-CAL-TABLE                      PIC XX.                00003321
00442      12  AM-LF-DEVIATION                   PIC XXX.               00003322
00443      12  AM-AH-DEVIATION                   PIC XXX.               00003323
00444      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3. 00003324
00445      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3. 00003325
00446      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3. 00003326
00447      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3. 00003327
00448      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3. 00003328
00449      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3. 00003329
00450                                                                   00003330
00451      12  AM-USER-FIELDS.                                          00003331
00452          16  AM-FLD-1                      PIC XX.                00003332
00453          16  AM-FLD-2                      PIC XX.                00003333
00454          16  AM-FLD-3                      PIC XX.                00003334
00455          16  AM-FLD-4                      PIC XX.                00003335
00456          16  AM-FLD-5                      PIC XX.                00003336
00457                                                                   00004570
00458      12  AM-1ST-PROD-DATE.                                        00004571
00459          16  AM-1ST-PROD-YR                PIC XX.                00004572
00460          16  AM-1ST-PROD-MO                PIC XX.                00004573
00461          16  AM-1ST-PROD-DA                PIC XX.                00004574
00462      12  AM-ANNIVERSARY-DATE.                                     00004575
00463          16  AM-AN-MO                      PIC 99.                00004576
00464          16  AM-AN-DA                      PIC 99.                00004577
00465          16  AM-AN-YR                      PIC 99.                00004578
00466      12  AM-CERTS-PURGED-DATE.                                    00004579
00467          16  AM-PUR-YR                     PIC XX.                00004580
00468          16  AM-PUR-MO                     PIC XX.                00004581
00469          16  AM-PUR-DA                     PIC XX.                00004582
00470      12  AM-HI-CERT-DATE.                                         00004583
00471          16  AM-HI-YR                      PIC XX.                00004584
00472          16  AM-HI-MO                      PIC XX.                00004585
00473          16  AM-HI-DA                      PIC XX.                00004586
00474      12  AM-LO-CERT-DATE.                                         00004587
00475          16  AM-LO-YR                      PIC XX.                00004588
00476          16  AM-LO-MO                      PIC XX.                00004589
00477          16  AM-LO-DA                      PIC XX.                00004590
00478      12  AM-ENTRY-DATE.                                           00004591
00479          16  AM-ENT-MO                     PIC 99.                00004592
00480          16  AM-ENT-DA                     PIC 99.                00004593
00481          16  AM-ENT-YR                     PIC 99.                00004594
00482      12  AM-INACTIVE-DATE.                                        00004595
00483          16  AM-INA-MO                     PIC 99.                00004596
00484          16  AM-INA-DA                     PIC 99.                00004597
00485          16  AM-INA-YR                     PIC 99.                00004598
00486      12  AM-AR-HI-CERT-DATE                PIC XX.                00004599
00487                                                                   00004600
00488      12  AM-LF-PSI-FACTOR                  PIC S9V9(06)   COMP-3. 00004601
00489      12  AM-AH-PSI-FACTOR                  PIC S9V9(06)   COMP-3. 00004602
00490      12  FILLER                            PIC X(14).             00004603
00491                                                                   00004604
00492      12  AM-RECALC-COMM                    PIC X.                 00004605
00493      12  AM-RECALC-REIN                    PIC X.                 00004606
00494                                                                   00004607
00495      12  AM-REI-TABLE                      PIC XXX.               00004608
00496      12  AM-REI-ET-LF                      PIC X.                 00004609
00497      12  AM-REI-ET-AH                      PIC X.                 00004610
00498      12  AM-REI-PE-LF                      PIC X.                 00004611
00499      12  AM-REI-PE-AH                      PIC X.                 00004612
00500      12  AM-REI-PRT-ST                     PIC X.                 00004613
00501      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3. 00004614
00502      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3. 00004615
00503      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3. 00004616
00504      12  AM-REI-GROUP-A                    PIC X(6).              00004617
00505      12  AM-REI-MORT                       PIC X(4).              00004618
00506      12  AM-REI-PRT-OW                     PIC X.                 00004619
00507      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3. 00004620
00508      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3. 00004621
00509      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3. 00004622
00510      12  AM-REI-GROUP-B                    PIC X(6).              00004623
00511                                                                   00004624
00512      12  FILLER                            PIC X(2).              00004625
00513      12  AM-EMPLOYER-STMT-USED             PIC X.                 00004626
00514      12  AM-GROUPED-CHECKS-Y-N             PIC X.                 00004627
00515                                                                   00004628
00516      12  AM-STD-AH-TYPE                    PIC XX.                00004629
00517      12  AM-EARN-METHODS.                                         00004630
00518          16  AM-EARN-METHOD-R              PIC X.                 00004631
00519              88 AM-REF-RL-R78                 VALUE 'R'.          00004632
00520              88 AM-REF-RL-PR                  VALUE 'P'.          00004633
00521              88 AM-REF-RL-MEAN                VALUE 'M'.          00004634
00522              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.          00004635
00523          16  AM-EARN-METHOD-L              PIC X.                 00004636
00524              88 AM-REF-LL-R78                 VALUE 'R'.          00004637
00525              88 AM-REF-LL-PR                  VALUE 'P'.          00004638
00526              88 AM-REF-LL-MEAN                VALUE 'M'.          00004639
00527              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.          00004640
00528          16  AM-EARN-METHOD-A              PIC X.                 00004641
00529              88 AM-REF-AH-R78                 VALUE 'R'.          00004642
00530              88 AM-REF-AH-PR                  VALUE 'P'.          00004643
00531              88 AM-REF-AH-MEAN                VALUE 'M'.          00004644
00532              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.          00004645
00533              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.          00004646
00534                                                                   00004647
00535      12  AM-TOL-PREM                       PIC S999V99    COMP-3. 00004648
00536      12  AM-TOL-REF                        PIC S999V99    COMP-3. 00004649
00537      12  AM-TOL-CLM                        PIC S999V99    COMP-3. 00004650
00538                                                                   00005380
00539      12  AM-RET-Y-N                        PIC X.                 00005381
00540      12  AM-RET-P-E                        PIC X.                 00005382
00541      12  AM-LF-RET                         PIC S9V9999    COMP-3. 00005383
00542      12  AM-AH-RET                         PIC S9V9999    COMP-3. 00005384
00543      12  AM-RET-GRP                        PIC X(6).              00005385
00544      12  AM-RETRO-EARNINGS.                                       00005386
00545          16  AM-RET-EARN-R                 PIC X.                 00005387
00546          16  AM-RET-EARN-L                 PIC X.                 00005388
00547          16  AM-RET-EARN-A                 PIC X.                 00005389
00548      12  AM-RET-ST-TAX-USE                 PIC X.                 00005390
00549          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.  00005391
00550          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.      00005392
00551      12  AM-RETRO-BEG-EARNINGS.                                   00005393
00552          16  AM-RET-BEG-EARN-R             PIC X.                 00005394
00553          16  AM-RET-BEG-EARN-L             PIC X.                 00005395
00554          16  AM-RET-BEG-EARN-A             PIC X.                 00005396
00555      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3. 00005397
00556      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3. 00005398
00557                                                                   00005399
00558      12  AM-USER-SELECT-OPTIONS.                                  00005400
00559          16  AM-USER-SELECT-1              PIC X(10).             00005401
00560          16  AM-USER-SELECT-2              PIC X(10).             00005402
00561          16  AM-USER-SELECT-3              PIC X(10).             00005403
00562          16  AM-USER-SELECT-4              PIC X(10).             00005404
00563          16  AM-USER-SELECT-5              PIC X(10).             00005405
00564                                                                   00005406
00565      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3. 00005407
00566                                                                   00005408
00567      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3. 00005409
00568                                                                   00005410
00569      12  AM-RPT045A-SWITCH                 PIC X.                 00005411
00570          88  RPT045A-OFF                   VALUE 'N'.             00005412
00571                                                                   00005413
00572      12  AM-INSURANCE-LIMITS.                                     00005414
00573          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3. 00005415
00574          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3. 00005416
00575                                                                   00005417
00576      12  FILLER                            PIC X(329).            00005418
00577                                                                   00005419
00578                                                                   00005420
00579      12  AM-TRANSFER-DATA.                                        00005421
00580          16  AM-TRANSFERRED-FROM.                                 00005422
00581              20  AM-TRNFROM-CARRIER        PIC X.                 00005423
00582              20  AM-TRNFROM-GROUPING.                             00005424
00583                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.               00005425
00584                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.               00005426
00585              20  AM-TRNFROM-STATE          PIC XX.                00005427
00586              20  AM-TRNFROM-ACCOUNT.                              00005428
00587                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).             00005429
00588                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).              00005430
00589              20  AM-TRNFROM-DTE            PIC XX.                00005431
00590          16  AM-TRANSFERRED-TO.                                   00005432
00591              20  AM-TRNTO-CARRIER          PIC X.                 00005433
00592              20  AM-TRNTO-GROUPING.                               00005434
00593                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.               00005435
00594                  24  AM-TRNTO-GRP-PRIME    PIC XXX.               00005436
00595              20  AM-TRNTO-STATE            PIC XX.                00005437
00596              20  AM-TRNTO-ACCOUNT.                                00005438
00597                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).              00005439
00598                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).              00005440
00599              20  AM-TRNTO-DTE              PIC XX.                00005441
00600          16  FILLER                        PIC X(12).             00005442
00601                                                                   00005443
00602      12  AM-LIABILITY-LIMITS.                                     00005444
00603          16  AM-LF-LIMITS              OCCURS 5 TIMES.            00005445
00604              20  AM-L-TYPE                 PIC 99.                00005446
00605              20  AM-L-ATT-AGE              PIC S99        COMP-3. 00005447
00606              20  AM-L-LIMITS           OCCURS 4 TIMES.            00005448
00607                  24  AM-LF-LM-AGE          PIC S99        COMP-3. 00005449
00608                  24  AM-LF-LM-DUR          PIC S999       COMP-3. 00005450
00609                  24  AM-LF-LM-AMT          PIC S9(6)      COMP-3. 00005451
00610          16  AM-AH-LIMITS              OCCURS 8 TIMES.            00005452
00611              20  AM-A-TYPE                 PIC 99.                00005453
00612              20  AM-A-ATT-AGE              PIC S99        COMP-3. 00005454
00613              20  AM-A-LIMITS           OCCURS 4 TIMES.            00005455
00614                  24  AM-AH-LM-AGE          PIC S99        COMP-3. 00005456
00615                  24  AM-AH-LM-DUR          PIC S999       COMP-3. 00005457
00616                  24  AM-AH-LM-MOA          PIC S9(4)      COMP-3. 00005458
00617                  24  AM-AH-LM-AMT          PIC S9(6)      COMP-3. 00005459
00618                                                                   00005460
00619      12  AM-COMMENTS.                                             00005461
00620          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.00005462
00621                                                                   00005463
00622      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.         00005464
00623          16  AM-FLI-RETRO-SHARE-CODE       PIC X.                 00005465
00624          16  AM-FLI-BILLING-CODE           PIC X.                 00005466
00625          16  AM-FLI-ALT-STATE-CODE         PIC XX.                00005467
00626          16  AM-FLI-UNITED-IDENT           PIC X.                 00005468
00627          16  AM-FLI-INTEREST-LOST-DATA.                           00005469
00628              20  AM-FLI-BANK-NO            PIC X(5).              00005470
00629              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3. 00005471
00630              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3. 00005472
00631              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3. 00005473
00632          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.            00005474
00633              20  AM-FLI-AGT                PIC X(9).              00005475
00634              20  AM-FLI-AGT-COMM-ACC       PIC X.                 00005476
00635              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3. 00005477
00636          16  FILLER                        PIC X(102).            00005478
00637                                                                   00006370
00638 ******************************************************************00006371
00639  EJECT                                                            00006372
00640                              COPY SCSACC01.                       00006373
00641  EJECT                                                            00006374
00642                              COPY ECSCRT01.                       00006375
00643  EJECT                                                            00006376
00644                              COPY SCSCRT01.                       00006377
00645  EJECT                                                            00006378
00646                              COPY ECSEXT01.                       00006379
00647  EJECT                                                            00006380
00648 ******************************************************************00006381
00649 *                                                                *00006382
00650 *                            SCSEXT01                            *00006383
00651 *                                                                *00006384
00652 ******************************************************************00006385
00653  01  CAC-DETAIL-RECORD.                                           00006386
00654      12  CAC-ID                      PIC  X.                      00006387
00655      12  CAC-REIN                    PIC  X.                      00006388
00656      12  CAC-CONTROL.                                             00006389
00657          16  CAC-CNTRL1.                                          00006390
00658              20  CAC-COMP.                                        00006391
00659                  24  CAC-CARR        PIC  X.                      00006392
00660                  24  CAC-CMP         PIC  XXX.                    00006393
00661              20  CAC-STATE           PIC  XX.                     00006394
00662              20  CAC-ACCT            PIC  X(6).                   00006395
00663          16  CAC-CNTRL2.                                          00006396
00664              20  CAC-EFF.                                         00006397
00665                  24  CAC-EF-YR       PIC  99.                     00006398
00666                  24  CAC-EF-MO       PIC  99.                     00006399
00667                  24  CAC-EF-DA       PIC  99.                     00006400
00668              20  CAC-CERT.                                        00006401
00669                  24  CAC-CRT-NO      PIC  X(7).                   00006402
00670                  24  CAC-CRT-SUF     PIC  X.                      00006403
00671      12  CAC-TRANS                   PIC  X.                      00006404
00672          88  CAC-ISSUE               VALUE 'I'.                   00006405
00673          88  CAC-RC-ISSUE            VALUE '8'.                   00006406
00674          88  CAC-CANCEL              VALUE 'C'.                   00006407
00675          88  CAC-RC-CANCEL           VALUE '7'.                   00006408
00676          88  CAC-CLAIM               VALUE 'X'.                   00006409
00677          88  CAC-RESTORE             VALUE 'R'.                   00006410
00678          88  CAC-RESERVE             VALUE 'Y'.                   00006411
00679          88  CAC-RR-RC-ISS           VALUE 'J'.                   00006412
00680          88  CAC-RR-RC-CNC           VALUE 'K'.                   00006413
00681          88  CAC-RR-RC-CLM           VALUE 'L'.                   00006414
00682      12  CAC-NAME.                                                00006415
00683          16  CAC-LNAME               PIC  X(12).                  00006416
00684          16  CAC-INIT                PIC  XX.                     00006417
00685      12  CAC-AGE                     PIC  99.                     00006418
00686      12  CAC-SEX                     PIC  X.                      00006419
00687      12  CAC-TERM                    PIC  999        COMP-3.      00006420
00688      12  CAC-LF-TYPE                 PIC  99.                     00006421
00689      12  CAC-LF-BEN                  PIC S9(7)V99    COMP-3.      00006422
00690      12  CAC-LF-PRM                  PIC S9(5)V99    COMP-3.      00006423
00691      12  CAC-AH-TYPE                 PIC  99.                     00006424
00692      12  CAC-AH-BEN                  PIC S9(5)V99    COMP-3.      00006425
00693      12  CAC-AH-PRM                  PIC S9(5)V99    COMP-3.      00006426
00694      12  CAC-ACC-GPCD                PIC  XX.                     00006427
00695      12  CAC-IG                      PIC  X.                      00006428
00696      12  CAC-REMIT-TO                PIC  XX.                     00006429
00697      12  CAC-MEMBER-NO               PIC  X(12).                  00006430
00698      12  CAC-SUMMARY-CNTS REDEFINES CAC-MEMBER-NO.                00006431
00699          16  CAC-LIVES               PIC S999.                    00006432
00700          16  FILLER                  PIC S999.                    00006433
00701          16  CAC-CANCEL-CNT-ITD      PIC S999.                    00006434
00702          16  CAC-CANCEL-CNT-YTD      PIC S999.                    00006435
00703      12  CAC-SSN                     PIC  X(11).                  00006436
00704      12  CAC-SSN-REDEFINED  REDEFINES  CAC-SSN.                   00006437
00705          16  CAC-APR                 PIC  S999V9999   COMP-3.     00006438
00706          16  FILLER                  PIC  X(7).                   00006439
00707      12  CAC-STAT-CDE                PIC  X.                      00006440
00708      12  CAC-PREV-STAT               PIC  X.                      00006441
00709      12  CAC-ENTRY-DTE.                                           00006442
00710          16  CAC-ENTRY-YR            PIC  99.                     00006443
00711          16  CAC-ENTRY-MO            PIC  99.                     00006444
00712          16  CAC-ENTRY-DA            PIC  99.                     00006445
00713      12  CAC-CANC-DTE.                                            00006446
00714          16  CAC-CANC-MO             PIC  99.                     00006447
00715          16  CAC-CANC-DA             PIC  99.                     00006448
00716          16  CAC-CANC-YR             PIC  99.                     00006449
00717      12  CAC-LF-RFND                 PIC S9(5)V99    COMP-3.      00006450
00718      12  CAC-AH-RFND                 PIC S9(5)V99    COMP-3.      00006451
00719      12  CAC-ERROR                   PIC  XX.                     00006452
00720      12  CAC-REI-AREA.                                            00006453
00721          16  CAC-REI-COMP            PIC  XXX.                    00006454
00722          16  CAC-REI-LFAMT           PIC S9(7)V99    COMP-3.      00006455
00723          16  CAC-REI-LFPRM           PIC S9(5)V99    COMP-3.      00006456
00724          16  CAC-REI-LFRFND          PIC S9(5)V99    COMP-3.      00006457
00725          16  CAC-REI-AHAMT           PIC S9(5)V99    COMP-3.      00006458
00726          16  CAC-REI-AHPRM           PIC S9(5)V99    COMP-3.      00006459
00727          16  CAC-REI-AHRFND          PIC S9(5)V99    COMP-3.      00006460
00728          16  CAC-REI-CNAMT           PIC S9(7)V99    COMP-3.      00006461
00729      12  CAC-BILLED-AREA  REDEFINES  CAC-REI-AREA.                00006462
00730          16  CAC-GL-INTERFACE-CODES.                              00006463
00731              20  CAC-GL-CODES OCCURS 10 TIMES                     00006464
00732                                      PIC  X.                      00006465
00733          16  CAC-BILL-SW             PIC  X.                      00006466
00734              88  CAC-RECORD-ON-HOLD              VALUE 'H'.       00006467
00735              88  CAC-RECORD-RETURNED             VALUE 'R'.       00006468
00736              88  CAC-RECORD-ENDORSED             VALUE 'E'.       00006469
00737              88  CAC-OVERRIDE-LIFE               VALUE 'L'.       00006470
00738              88  CAC-OVERRIDE-AH                 VALUE 'A'.       00006471
00739              88  CAC-OVERRIDE-BOTH               VALUE 'B'.       00006472
00740          16  CAC-REFUND-SW           PIC  X.                      00006473
00741              88  CAC-REFUND-CREATED              VALUE 'Y'.       00006474
00742          16  CAC-BILLED-LFPRM        PIC S9(5)V99    COMP-3.      00006475
00743          16  CAC-BILLED-LFRFND       PIC S9(5)V99    COMP-3.      00006476
00744          16  CAC-BILLED-AHPRM        PIC S9(5)V99    COMP-3.      00006477
00745          16  CAC-BILLED-AHRFND       PIC S9(5)V99    COMP-3.      00006478
00746          16  CAC-GA-BILL-STATUS OCCURS 5 TIMES                    00006479
00747                                      PIC  X.                      00006480
00748 *            88  CAC-NO-PRE-BILL-GA              VALUE ' '.       00006481
00749 *            88  CAC-BILLED-GA                   VALUE 'B'.       00006482
00750      12  CAC-LF-CNBEN                PIC S9(7)V99    COMP-3.      00006483
00751      12  CAC-PMT-FREQ                PIC  99.                     00006484
00752      12  CAC-BILL-STATUS             PIC  X.                      00006485
00753          88  CAC-NO-PRE-BILL                     VALUE ' '.       00006486
00754          88  CAC-BILLED                          VALUE 'B'.       00006487
00755          88  CAC-REVERSE                         VALUE 'R'.       00006488
00756      12  CAC-REVERSE-REASONS.                                     00006489
00757          16  CAC-REVERSE-REASON-1    PIC  X.                      00006490
00758          16  CAC-REVERSE-REASON-2    PIC  X.                      00006491
00759          16  CAC-REVERSE-REASON-3    PIC  X.                      00006492
00760      12  CAC-RECALC-CODE             PIC  X.                      00006493
00761      12  CAC-CANC-EXIT-DT.                                        00006494
00762          16  CAC-CANC-EXIT-MO        PIC  99.                     00006495
00763          16  CAC-CANC-EXIT-DA        PIC  99.                     00006496
00764          16  CAC-CANC-EXIT-YR        PIC  99.                     00006497
00765      12  CAC-ENTRY-PLST              PIC  X.                      00006498
00766      12  CAC-RECALC-TYPE             PIC  X.                      00006499
00767      12  CAC-COMM-LEVELS.                                         00006500
00768          16  CAC-AGT-LEVELS          OCCURS 10 TIMES.             00006501
00769              20  CAC-AGT             PIC  X(6).                   00006502
00770              20  CAC-AGT-TYPE        PIC  X.                      00006503
00771              20  CAC-L-PC            PIC SV9(5)      COMP-3.      00006504
00772              20  CAC-A-PC            PIC SV9(5)      COMP-3.      00006505
00773      12  CAC-PROC-DT.                                             00006506
00774          16  CAC-PD-YR               PIC  99.                     00006507
00775          16  CAC-PD-MO               PIC  99.                     00006508
00776          16  CAC-PD-DA               PIC  99.                     00006509
00777                                                                   00006510
00778  01  CAC-CLAIM-EXTR   REDEFINES   CAC-DETAIL-RECORD.              00006511
00779      12  FILLER                      PIC  X(174).                 00006512
00780      12  CAC-TYPE                    PIC  X.                      00006513
00781          88  CAC-DTH                 VALUE '1'.                   00006514
00782          88  CAC-AH                  VALUE '2'.                   00006515
00783          88  CAC-OB-DTH              VALUE '3'.                   00006516
00784          88  CAC-OB-AH               VALUE '4'.                   00006517
00785      12  CAC-AMT                     PIC S9(7)V99    COMP-3.      00006518
00786      12  CAC-INCUR.                                               00006519
00787          16  CAC-INCUR-MO            PIC  99.                     00006520
00788          16  CAC-INCUR-DA            PIC  99.                     00006521
00789          16  CAC-INCUR-YR            PIC  99.                     00006522
00790      12  CAC-REPORTED.                                            00006523
00791          16  CAC-RPT-MO              PIC  99.                     00006524
00792          16  CAC-RPT-DA              PIC  99.                     00006525
00793          16  CAC-RPT-YR              PIC  99.                     00006526
00794      12  CAC-PAY.                                                 00006527
00795          16  CAC-PAY-MO              PIC  99.                     00006528
00796          16  CAC-PAY-DA              PIC  99.                     00006529
00797          16  CAC-PAY-YR              PIC  99.                     00006530
00798      12  CAC-PAID-TO.                                             00006531
00799          16  CAC-PTO-MO              PIC  99.                     00006532
00800          16  CAC-PTO-DA              PIC  99.                     00006533
00801          16  CAC-PTO-YR              PIC  99.                     00006534
00802      12  CAC-CNUM                    PIC  X(7).                   00006535
00803      12  CAC-CHECK                   PIC  X(5).                   00006536
00804      12  CAC-DAYS-DISAB              PIC  XXX.                    00006537
00805      12  CAC-CLM-AGE                 PIC  99.                     00006538
00806      12  CAC-PAY-CODE                PIC  X.                      00006539
00807      12  CAC-CLM-ERR                 PIC  XX.                     00006540
00808      12  CAC-REI-CLAIM               PIC S9(7)V99    COMP-3.      00006541
00809      12  CAC-ACC-NAME                PIC  X(20).                  00006542
00810      12  CAC-ACC-EXP-DTE             PIC  X(6).                   00006543
00811      12  CAC-ACC-EFF-DTE             PIC  X(6).                   00006544
00812      12  CAC-CLM-CAUSE               PIC  XX.                     00006545
00813      12  CAC-CM-LN-OFFICER           PIC  X(4).                   00006546
00814      12  FILLER                      PIC  X(37).                  00006547
00815      12  CAC-CLM-PROC-DT.                                         00006548
00816          16  CAC-CP-YR               PIC  99.                     00006549
00817          16  CAC-CP-MO               PIC  99.                     00006550
00818          16  CAC-CP-DA               PIC  99.                     00006551
00819                                                                   00006552
00820  01  CAC-RESERVE-EXTR   REDEFINES   CAC-DETAIL-RECORD.            00006553
00821      12  FILLER                      PIC X(174).                  00006554
00822      12  CAC-RESERVE-TYPE            PIC X.                       00006555
00823          88  CAC-LIFE-RSV              VALUE '1'.                 00006556
00824          88  CAC-AH-RSV                VALUE '2'.                 00006557
00825      12  CAC-IBNR                    PIC S9(7)V99    COMP-3.      00006558
00826      12  CAC-PAYCUR                  PIC S9(7)V99    COMP-3.      00006559
00827      12  CAC-FUTRSV                  PIC S9(7)V99    COMP-3.      00006560
00828      12  CAC-CLMNO                   PIC X(7).                    00006561
00829      12  CAC-REI-IBNR                PIC S9(7)V99    COMP-3.      00006562
00830      12  CAC-REI-PAYCUR              PIC S9(7)V99    COMP-3.      00006563
00831      12  CAC-REI-FUTRSV              PIC S9(7)V99    COMP-3.      00006564
00832      12  CAC-RSV-ACC-NAME            PIC X(20).                   00006565
00833      12  FILLER                      PIC X(78).                   00006566
00834 ******************************************************************00006567
00835  EJECT                                                            00006568
00836                              COPY ERCCOMP.                        00006569
00837  EJECT                                                            00006570
00838  01  COMP-RECORD.                                                 00006571
00839      12  CMR-ID                      PIC  X.                      00006572
00840          88  CMR-BEG-OF-FILE                    VALUE LOW-VALUE.  00006573
00841          88  CMR-COMPENSATION-MASTER            VALUE '$'.        00006574
00842          88  CMR-NULL-RECORD                    VALUE ' '.        00006575
00843          88  CMR-END-OF-FILE                    VALUE HIGH-VALUE. 00006576
00844                                                                   00006577
00845      12  CMR-TYPE                    PIC  X.                      00006578
00846          88  CMR-COMPANY-RECORD                 VALUE 'C'.        00006579
00847          88  CMR-GENERAL-AGENT-RECORD           VALUE 'G'.        00006580
00848          88  CMR-ACCOUNT-RECORD                 VALUE 'A'.        00006581
00849                                                                   00006582
00850      12  CMR-BALANCE-CONTROL         PIC  X.                      00006583
00851          88  CMR-CARRY-BALANCE                  VALUE 'Y'.        00006584
00852          88  CMR-NO-BALANCE                     VALUE 'N'.        00006585
00853                                                                   00006586
00854      12  CMR-INTERNAL-CONTROL-1      PIC  X.                      00006587
00855          88  CMR-AUTO-GENERATED-THIS-RUN        VALUE 'X'.        00006588
00856          88  CMR-AUTO-GENERATED                 VALUE 'Y'.        00006589
00857          88  CMR-NOT-AUTO-GENERATED             VALUE 'N'.        00006590
00858                                                                   00006591
00859      12  CMR-INTERNAL-CONTROL-2      PIC  X.                      00006592
00860          88  CMR-STATEMENT-THIS-RUN             VALUE 'Y'.        00006593
00861          88  CMR-NO-STATEMENT-THIS-RUN          VALUE 'N'.        00006594
00862                                                                   00006595
00863      12  CMR-FUTURE-SPACE            PIC  X.                      00006596
00864          88  CMR-FUTURE-NOT-USED                VALUE ' '.        00006597
00865                                                                   00006598
00866      12  CMR-CONTROL.                                             00006599
00867          16  CMR-CTL-1.                                           00006600
00868              20  CMR-CARR-COMP.                                   00006601
00869                  24  CMR-CARR        PIC  X.                      00006602
00870                  24  CMR-COMP        PIC  XXX.                    00006603
00871              20  CMR-RESP            PIC  X(6).                   00006604
00872          16  CMR-CTL-2.                                           00006605
00873              20  CMR-ACCT            PIC  X(6).                   00006606
00874                                                                   00006607
00875      12  CMR-ACCT-NAME               PIC  X(30).                  00006608
00876      12  CMR-MAIL-NAME               PIC  X(30).                  00006609
00877      12  CMR-ADDR-1                  PIC  X(30).                  00006610
00878      12  CMR-ADDR-2                  PIC  X(30).                  00006611
00879      12  CMR-ADDR-3                  PIC  X(30).                  00006612
00880      12  CMR-ZIP                     PIC  X(9).                   00006613
00881      12  CMR-SOC-SEC                 PIC  X(13).                  00006614
00882      12  CMR-LAST-UPDATE-DATE.                                    00006615
00883          16  CMR-MONTH               PIC  XX.                     00006616
00884          16  CMR-YEAR                PIC  XX.                     00006617
00885                                                                   00006618
00886      12  CMR-TELEPHONE.                                           00006619
00887          16  CMR-AREA-CODE           PIC  XXX.                    00006620
00888          16  CMR-PREFIX              PIC  XXX.                    00006621
00889          16  CMR-PHONE               PIC  X(4).                   00006622
00890                                                                   00006623
00891      12  CMR-LAST-ACTIVITY-DATE.                                  00006624
00892          16  CMR-ACT-MONTH           PIC  XX.                     00006625
00893          16  CMR-ACT-YEAR            PIC  XX.                     00006626
00894                                                                   00006627
00895      12  CMR-FUTURE                  PIC  X.                      00006628
00896                                                                   00006629
00897      12  CMR-LAST-STMT-DT.                                        00006630
00898          16  CMR-LAST-STMT-YEAR      PIC 99.                      00006631
00899          16  CMR-LAST-STMT-MONTH     PIC 99.                      00006632
00900          16  CMR-LAST-STMT-DAY       PIC 99.                      00006633
00901                                                                   00006634
00902      12  CMR-CLAIM-TOTALS.                                        00006635
00903          16  CMR-LF-CLM-AMT          PIC S9(5)V99    COMP-3.      00006636
00904          16  CMR-AH-CLM-AMT          PIC S9(5)V99    COMP-3.      00006637
00905                                                                   00006638
00906      12  CMR-FICA-TOTALS.                                         00006639
00907          16  CMR-CUR-FICA            PIC S9(5)V99    COMP-3.      00006640
00908          16  CMR-YTD-FICA            PIC S9(5)V99    COMP-3.      00006641
00909                                                                   00006642
00910      12  CMR-OVER-UNDER-TOTALS.                                   00006643
00911          16  CMR-CUR-OVR-UNDR        PIC S9(7)V99    COMP-3.      00006644
00912          16  CMR-YTD-OVR-UNDR        PIC S9(7)V99    COMP-3.      00006645
00913                                                                   00006646
00914      12  CMR-TOTALS.                                              00006647
00915                                                                   00006648
00916          16  CMR-MONTHLY-TOTALS.                                  00006649
00917              20  CMR-BAL-FWD         PIC S9(7)V99    COMP-3.      00006650
00918              20  CMR-CUR-COM         PIC S9(7)V99    COMP-3.      00006651
00919              20  CMR-CUR-CHG         PIC S9(7)V99    COMP-3.      00006652
00920              20  CMR-CUR-PMT         PIC S9(7)V99    COMP-3.      00006653
00921              20  CMR-END-BAL         PIC S9(7)V99    COMP-3.      00006654
00922                                                                   00006655
00923          16  CMR-AGING-TOTALS.                                    00006656
00924              20  CMR-CUR             PIC S9(7)V99    COMP-3.      00006657
00925              20  CMR-OV30            PIC S9(7)V99    COMP-3.      00006658
00926              20  CMR-OV60            PIC S9(7)V99    COMP-3.      00006659
00927              20  CMR-OV90            PIC S9(7)V99    COMP-3.      00006660
00928                                                                   00006661
00929          16  CMR-YTD-TOTALS.                                      00006662
00930              20  CMR-YTD-COM         PIC S9(7)V99    COMP-3.      00006663
00931              20  CMR-YTD-OV          PIC S9(7)V99    COMP-3.      00006664
00932                                                                   00006665
00933 ******************************************************************00006666
00934  EJECT                                                            00006667
00935                              COPY ERCCTBL.                        00006668
00936  EJECT                                                            00006669
00937 ******************************************************************00006670
00938 *                                                                *00006671
00939 *                            SCSCOMTB                            *00006672
00940 *                                                                *00006673
00941 ******************************************************************00006674
00942  01  CTBL-RECORD.                                                 00006675
00943                                                                   00006676
00944      12  XCT-FLAG                    PIC  X.                      00006677
00945                                                                   00006678
00946      12  XCT-CNTRL.                                               00006679
00947          16  XCT-CNTRL-1.                                         00006680
00948              20  XCT-REC-ID          PIC  XX.                     00006681
00949              20  XCT-CLIENT          PIC  XXX.                    00006682
00950              20  XCT-TABLE           PIC  XXX.                    00006683
00951          16  XCT-CNTRL-2.                                         00006684
00952              20  XCT-BEN-TYPE        PIC  X.                      00006685
00953              20  XCT-BEN-CODE        PIC  XX.                     00006686
00954                                                                   00006687
00955      12  XCT-LIMITS.                                              00006688
00956                                                                   00006689
00957          16  XCT-TBF             OCCURS 3 TIMES                   00006690
00958                                      PIC S9(6)V99   COMP-3.       00006691
00959                                                                   00006692
00960          16  XCT-AGE             OCCURS 3 TIMES                   00006693
00961                                      PIC S99         COMP-3.      00006694
00962                                                                   00006695
00963          16  XCT-TRM             OCCURS 3 TIMES                   00006696
00964                                      PIC S999        COMP-3.      00006697
00965                                                                   00006698
00966      12  XCT-RATES.                                               00006699
00967          16  XCT-RTX             OCCURS 27 TIMES.                 00006700
00968              20  XCT-RT              PIC SV9(5)      COMP-3.      00006701
00969                                                                   00006702
00970      12  FILLER                      PIC  X(30).                  00006703
00971                                                                   00006704
00972 ******************************************************************00006705
00973  EJECT                                                            00006706
00974                              COPY ECSEPC01.                       00006707
00975  EJECT                                                            00006708
00976 ******************************************************************00006709
00977 *                                                                *00006710
00978 *                            SCSEXT01                            *00006711
00979 *                                                                *00006712
00980 ******************************************************************00006713
00981  01  EPEC-RECORD.                                                 00006714
00982      12  XP-CODE                     PIC  X.                      00006715
00983      12  XP-REIN                     PIC  X.                      00006716
00984      12  XP-CONTROL.                                              00006717
00985          16  XP-COMP.                                             00006718
00986              20  XP-CARR             PIC  X.                      00006719
00987              20  XP-CMP              PIC  XXX.                    00006720
00988          16  XP-STATE                PIC  XX.                     00006721
00989          16  XP-ACCT                 PIC  X(6).                   00006722
00990          16  XP-DATES.                                            00006723
00991              20  XP-EXP-DTE.                                      00006724
00992                  24  XP-EXP-YR       PIC  99.                     00006725
00993                  24  XP-EXP-MO       PIC  99.                     00006726
00994                  24  XP-EXP-DA       PIC  99.                     00006727
00995              20  XP-EFF-DTE.                                      00006728
00996                  24  XP-EFF-YR       PIC  99.                     00006729
00997                  24  XP-EFF-MO       PIC  99.                     00006730
00998                  24  XP-EFF-DA       PIC  99.                     00006731
00999      12  XP-LF-AH-CODE               PIC  X.                      00006732
01000      12  XP-BEN-TYPE                 PIC  99.                     00006733
01001      12  FILLER                      PIC  X.                      00006734
01002      12  XP-ISS-BEN                  PIC S9(11)V99   COMP-3.      00006735
01003      12  XP-CNC-BEN                  PIC S9(11)V99   COMP-3.      00006736
01004      12  XP-ISS-PRM                  PIC S9(9)V99    COMP-3.      00006737
01005      12  XP-CNC-PRM                  PIC S9(9)V99    COMP-3.      00006738
01006      12  XP-PRM-78                   PIC S9(9)V99    COMP-3.      00006739
01007      12  XP-PRM-PR                   PIC S9(9)V99    COMP-3.      00006740
01008      12  XP-CLM-AMT                  PIC S9(9)V99    COMP-3.      00006741
01009      12  XP-CLM-CNT                  PIC S9(7)       COMP-3.      00006742
01010      12  XP-CLM-CRT                  PIC S9(7)       COMP-3.      00006743
01011      12  XP-ISS-CNT                  PIC S9(7)       COMP-3.      00006744
01012      12  XP-CNC-CNT                  PIC S9(7)       COMP-3.      00006745
01013      12  XP-CLM-DU                   PIC S9(7)V99    COMP-3.      00006746
01014      12  XP-CLM-PV                   PIC S9(7)V99    COMP-3.      00006747
01015      12  XP-CLM-IBNR                 PIC S9(7)V99    COMP-3.      00006748
01016      12  XP-FILL-3                   PIC  XXX.                    00006749
01017      12  XP-LOSS-RESV                PIC S9(7)V99    COMP-3.      00006750
01018      12  XP-CLAIM-ADJ                PIC S9(7)V99    COMP-3.      00006751
01019      12  XP-RETRO-EXPENSES           PIC S9(7)V99    COMP-3.      00006752
01020      12  XP-RETRO-PAYMENTS           PIC S9(7)V99    COMP-3.      00006753
01021      12  XP-RETRO-OTH-COMM           PIC S9(7)V99    COMP-3.      00006754
01022      12  XP-MORT-RESV                PIC S9(11)V9(6) COMP-3.      00006755
01023      12  XP-IN-FORCE                 PIC S9(11)V99   COMP-3.      00006756
01024      12  XP-ADJUST                   PIC S9(7)V99    COMP-3.      00006757
01025      12  XP-PRM-SP                   PIC S9(9)V99    COMP-3.      00006758
01026      12  FILLER                      PIC  X(122).                 00006759
01027      12  XP-HI-COV-DT.                                            00006760
01028          16  XP-COV-YR               PIC  99.                     00006761
01029          16  XP-COV-MO               PIC  99.                     00006762
01030          16  XP-COV-DA               PIC  99.                     00006763
01031      12  XP-REI-CO                   PIC  XXX.                    00006764
01032      12  XP-HI-CERT.                                              00006765
01033          16  XP-HI-YR                PIC  99.                     00006766
01034          16  XP-HI-MO                PIC  99.                     00006767
01035          16  XP-HI-DA                PIC  99.                     00006768
01036      12  XP-LO-CERT.                                              00006769
01037          16  XP-LO-YR                PIC  99.                     00006770
01038          16  XP-LO-MO                PIC  99.                     00006771
01039          16  XP-LO-DA                PIC  99.                     00006772
01040      12  XP-PURGE                    PIC  X.                      00006773
01041      12  XP-RUN-DTE.                                              00006774
01042          16  XP-RUN-YR               PIC  99.                     00006775
01043          16  XP-RUN-MO               PIC  99.                     00006776
01044          16  XP-RUN-DA               PIC  99.                     00006777
01045                                                                   00006778
01046  01  XC-RECORD   REDEFINES   EPEC-RECORD.                         00006779
01047      12  XC-CODE                     PIC  X.                      00006780
01048      12  XC-REIN                     PIC  X.                      00006781
01049      12  XC-CONTROL.                                              00006782
01050          16  XC-COMP.                                             00006783
01051              20  XC-CARR             PIC  X.                      00006784
01052              20  XC-CMP              PIC  XXX.                    00006785
01053          16  XC-STATE                PIC  XX.                     00006786
01054          16  XC-ACCT                 PIC  X(6).                   00006787
01055          16  XC-DATES.                                            00006788
01056              20  XC-EXP-DTE.                                      00006789
01057                  24  XC-EXP-YR       PIC  99.                     00006790
01058                  24  XC-EXP-MO       PIC  99.                     00006791
01059                  24  XC-EXP-DA       PIC  99.                     00006792
01060              20  XC-EFF-DTE.                                      00006793
01061                  24  XC-EFF-YR       PIC  99.                     00006794
01062                  24  XC-EFF-MO       PIC  99.                     00006795
01063                  24  XC-EFF-DA       PIC  99.                     00006796
01064      12  XC-LF-AH-CODE               PIC  X.                      00006797
01065      12  XC-BEN-TYPE                 PIC  99.                     00006798
01066      12  XC-SEQ-NBR                  PIC  X.                      00006799
01067      12  XC-AGENTS-DATA.                                          00006800
01068          16  XC-AGENTS-LEVEL         OCCURS 5 TIMES.              00006801
01069              20  XC-AGT-NO           PIC  X(6).                   00006802
01070              20  XC-AGT-TYPE         PIC  X.                      00006803
01071              20  XC-ISS-COMM         PIC S9(9)V99    COMP-3.      00006804
01072              20  XC-CNC-COMM         PIC S9(9)V99    COMP-3.      00006805
01073              20  XC-COMM-78          PIC S9(9)V99    COMP-3.      00006806
01074              20  XC-COMM-PR          PIC S9(9)V99    COMP-3.      00006807
01075          16  XC-AGENTS-LEVEL-1       OCCURS 5 TIMES.              00006808
01076              20  XC-COMM-SP          PIC S9(9)V99    COMP-3.      00006809
01077      12  FILLER                      PIC  X(73).                  00006810
01078      12  XC-REI-CO                   PIC  XXX.                    00006811
01079      12  FILLER                      PIC  X(12).                  00006812
01080      12  XC-PURGE                    PIC  X.                      00006813
01081      12  XC-RUN-DTE.                                              00006814
01082          16  XC-RUN-YR               PIC  99.                     00006815
01083          16  XC-RUN-MO               PIC  99.                     00006816
01084          16  XC-RUN-DA               PIC  99.                     00006817
01085  EJECT                                                            00006818
01086                              COPY ERCREIN.                        00006819
01087  EJECT                                                            00006820
01088                              COPY SCSRTB01.                       00006821
01089  EJECT                                                            00006822
01090                              COPY ERCRATE.                        00006823
01091  EJECT                                                            00006824
01092 ******************************************************************00006825
01093 *                                                                *00006826
01094 *                            SCSRAT01                            *00006827
01095 *                            VMOD 6.1                            *00006828
01096 *                                                                *00006829
01097 ******************************************************************00006830
01098  01  RATE-RCD-AREA.                                               00006831
01099      12  XR-CONTROL.                                              00006832
01100          16  XR-STATE-CODE.                                       00006833
01101              20  XR-ST-CODE           PIC  XX.                    00006834
01102              20  XR-ST-CLASS          PIC  XX.                    00006835
01103              20  XR-ST-DEV            PIC  XXX.                   00006836
01104          16  XR-LIMITS.                                           00006837
01105              20  XR-HIGH-AGE          PIC  99.                    00006838
01106              20  XR-HIGH-AMT          PIC  9(6).                  00006839
01107              20  XR-FUTURE            PIC  XX.                    00006840
01108              20  XR-SEX               PIC  X.                     00006841
01109          16  XR-L-AH-CODE.                                        00006842
01110              20  XR-L-AH              PIC  X.                     00006843
01111              20  XR-LAH-NUM           PIC  99.                    00006844
01112          16  XR-EXPIRY-DATE.                                      00006845
01113              20  XR-EXP-YR            PIC  99.                    00006846
01114              20  XR-EXP-MO            PIC  99.                    00006847
01115              20  XR-EXP-DA            PIC  99.                    00006848
01116                                                                   00006849
01117      12  XR-STRUCT-COMNT              PIC  X(50).                 00006850
01118      12  XR-RATE-COMNT                PIC  X(50).                 00006851
01119      12  XR-MAX-AGE                   PIC  99.                    00006852
01120                                                                   00006853
01121      12  XR-LIFE-LIMS-FLDS.                                       00006854
01122          16  XR-LIFE-MORT-CODE        PIC  X(4).                  00006855
01123          16  XR-LIFE-EXCEPTIONS       OCCURS 8 TIMES.             00006856
01124              20  XR-L-EX-AGE          PIC 99.                     00006857
01125              20  XR-L-EX-TERM         PIC S999        COMP-3.     00006858
01126              20  XR-L-EX-FACE         PIC S9(7)       COMP-3.     00006859
01127          16  FILLER                   PIC  X(20).                 00006860
01128      12  XR-AH-LIMS-FLDS   REDEFINES   XR-LIFE-LIMS-FLDS.         00006861
01129          16  XR-AH-EXCEPTIONS         OCCURS 8 TIMES.             00006862
01130              20  XR-AH-AGE            PIC 99.                     00006863
01131              20  XR-AH-TERM           PIC S999        COMP-3.     00006864
01132              20  XR-AH-BEN-M          PIC S9(5)       COMP-3.     00006865
01133              20  XR-AH-BEN-F          PIC S9(7)       COMP-3.     00006866
01134                                                                   00006867
01135      12  XR-LIFE-RATES.                                           00006868
01136          16  XR-L-RATE   OCCURS 240 TIMES                         00006869
01137                                       PIC S99V9(5) COMP-3.        00006870
01138      12  XR-AH-RATES                  REDEFINES XR-LIFE-RATES.    00006871
01139          16  XR-AH-RATE  OCCURS 240 TIMES                         00006872
01140                                       PIC S99V9(5) COMP-3.        00006873
01141                                                                   00006874
01142      12  FILLER                       PIC  X(35).                 00006875
01143                                                                   00006876
01144      12  XR-SRT-ALPHA                 PIC  X.                     00006877
01145                                                                   00006878
01146      12  XR-CONTROL-2.                                            00006879
01147          16  XR-CTL-1                 PIC  X(7).                  00006880
01148          16  XR-CTL-3                 PIC  X(11).                 00006881
01149          16  XR-CTL-4                 PIC  X(6).                  00006882
01150          16  XR-CTL-2                 PIC  XXX.                   00006883
01151 ******************************************************************00006884
01152  EJECT                                                            00006885
01153                              COPY ECSAEX01.                       00006886
01154  EJECT                                                            00006887
01155 ******************************************************************00006888
01156 *                                                                *00006889
01157 *                            SCSAEX01                            *00006890
01158 *                                                                *00006891
01159 ******************************************************************00006892
01160  01  ALPH-EXT-REC.                                                00006893
01161      12  AER-AREA-1.                                              00006894
01162          16  AER-ID                  PIC  X.                      00006895
01163          16  AER-COMP.                                            00006896
01164              20  AER-CARR            PIC  X.                      00006897
01165              20  AER-CO              PIC  XXX.                    00006898
01166          16  AER-ST                  PIC  XX.                     00006899
01167          16  AER-ACCT                PIC  X(6).                   00006900
01168          16  AER-DT.                                              00006901
01169              20  AER-YR              PIC  99.                     00006902
01170              20  AER-MO              PIC  99.                     00006903
01171              20  AER-DA              PIC  99.                     00006904
01172      12  AER-AREA-2.                                              00006905
01173          16  AER-CERT.                                            00006906
01174              20  AER-CERT-NO         PIC  X(7).                   00006907
01175              20  AER-CERT-SUF        PIC  X.                      00006908
01176          16  AER-COMP-3.                                          00006909
01177              20  AER-TRM             PIC S999        COMP-3.      00006910
01178              20  AER-R-TRM           PIC S999        COMP-3.      00006911
01179              20  AER-LF-AMT          PIC S9(7)V99    COMP-3.      00006912
01180              20  AER-AH-AMT          PIC S9(5)V99    COMP-3.      00006913
01181              20  AER-LF-PRM          PIC S9(5)V99    COMP-3.      00006914
01182              20  AER-AH-PRM          PIC S9(5)V99    COMP-3.      00006915
01183              20  AER-LF-REM          PIC S9(7)V99    COMP-3.      00006916
01184          16  AER-LF-TYP              PIC  XX.                     00006917
01185          16  AER-AH-TYP              PIC  XX.                     00006918
01186          16  AER-AGE                 PIC  99.                     00006919
01187          16  AER-SEX                 PIC  X.                      00006920
01188          16  AER-STATUS              PIC  X.                      00006921
01189          16  AER-NAME.                                            00006922
01190              20  AER-LNAME           PIC  X(12).                  00006923
01191              20  AER-INIT-F          PIC  X.                      00006924
01192              20  AER-INIT-M          PIC  X.                      00006925
01193          16  AER-CNCL.                                            00006926
01194              20  AER-C-MO            PIC  99.                     00006927
01195              20  AER-C-DA            PIC  99.                     00006928
01196              20  AER-C-YR            PIC  99.                     00006929
01197          16  AER-DEATH.                                           00006930
01198              20  AER-D-MO            PIC  99.                     00006931
01199              20  AER-D-DA            PIC  99.                     00006932
01200              20  AER-D-YR            PIC  99.                     00006933
01201          16  AER-ENTRY.                                           00006934
01202              20  AER-E-MO            PIC  99.                     00006935
01203              20  AER-E-DA            PIC  99.                     00006936
01204              20  AER-E-YR            PIC  99.                     00006937
01205          16  AER-EXIT.                                            00006938
01206              20  AER-X-MO            PIC  99.                     00006939
01207              20  AER-X-DA            PIC  99.                     00006940
01208              20  AER-X-YR            PIC  99.                     00006941
01209          16  AER-SPEC-REIN           PIC  X.                      00006942
01210          16  FILLER                  PIC  X.                      00006943
01211          16  AER-MEM-NO              PIC  X(12).                  00006944
01212          16  AER-SOC-NO              PIC  X(11).                  00006945
01213          16  AER-AH-REM              PIC S9(7)V99    COMP-3.      00006946
01214          16  AER-PRE-PLST            PIC  X.                      00006947
01215          16  AER-PMT-FREQ            PIC  99.                     00006948
01216          16  AER-APR                 PIC S999V9(4).               00006949
01217          16  AER-IND-GRP             PIC  X.                      00006950
01218          16  FILLER                  PIC  X(10).                  00006951
01219 ******************************************************************00006952
01220  EJECT                                                            00006953
01221                              COPY ECSGAP01.                       00006954
01222  EJECT                                                            00006955
01223 ******************************************************************00006956
01224 *                                                                *00006957
01225 *                            SCSGAP01                            *00006958
01226 *                                                                *00006959
01227 ******************************************************************00006960
01228  01  GAAP-EXT-REC.                                                00006961
01229      12  GER-ID                      PIC  XXX.                    00006962
01230      12  GER-TYPE                    PIC  X.                      00006963
01231      12  GER-CNTL.                                                00006964
01232          16  GER-CMP-ST.                                          00006965
01233              20  GER-COMP.                                        00006966
01234                  24  GER-CO          PIC  X.                      00006967
01235                  24  GER-SUBCO       PIC  XXX.                    00006968
01236              20  GER-ST              PIC  XX.                     00006969
01237          16  GER-ACCT                PIC  X(6).                   00006970
01238          16  GER-EFF.                                             00006971
01239              20  GER-YR              PIC  99.                     00006972
01240              20  GER-MO              PIC  99.                     00006973
01241              20  GER-DA              PIC  99.                     00006974
01242          16  GER-CERT-NO.                                         00006975
01243              20  GER-CERT            PIC  X(7).                   00006976
01244              20  GER-CERT-SFX        PIC  X.                      00006977
01245      12  GER-LFTYP                   PIC  99.                     00006978
01246      12  GER-AHTYP                   PIC  99.                     00006979
01247      12  GER-BASIC.                                               00006980
01248          16  GER-TRM                 PIC S999        COMP-3.      00006981
01249          16  GER-REM                 PIC S999V99     COMP-3.      00006982
01250          16  GER-LFBEN               PIC S9(7)V99    COMP-3.      00006983
01251          16  GER-LFPRM               PIC S9(5)V99    COMP-3.      00006984
01252          16  GER-LFCOM               PIC S9(5)V99    COMP-3.      00006985
01253          16  GER-LFTAX               PIC S9(5)V99    COMP-3.      00006986
01254          16  GER-AHBEN               PIC S9(5)V99    COMP-3.      00006987
01255          16  GER-AHPRM               PIC S9(5)V99    COMP-3.      00006988
01256          16  GER-AHCOM               PIC S9(5)V99    COMP-3.      00006989
01257          16  GER-AHTAX               PIC S9(5)V99    COMP-3.      00006990
01258      12  GER-PRORATA.                                             00006991
01259          16  GXP-LFPRM               PIC S9(5)V99    COMP-3.      00006992
01260          16  GXP-LFCOM               PIC S9(5)V99    COMP-3.      00006993
01261          16  GXP-LFTAX               PIC S9(5)V99    COMP-3.      00006994
01262          16  GXP-AHPRM               PIC S9(5)V99    COMP-3.      00006995
01263          16  GXP-AHCOM               PIC S9(5)V99    COMP-3.      00006996
01264          16  GXP-AHTAX               PIC S9(5)V99    COMP-3.      00006997
01265      12  GER-R78.                                                 00006998
01266          16  GXR-LFPRM               PIC S9(5)V99    COMP-3.      00006999
01267          16  GXR-LFCOM               PIC S9(5)V99    COMP-3.      00007000
01268          16  GXR-LFTAX               PIC S9(5)V99    COMP-3.      00007001
01269          16  GXR-AHPRM               PIC S9(5)V99    COMP-3.      00007002
01270          16  GXR-AHCOM               PIC S9(5)V99    COMP-3.      00007003
01271          16  GXR-AHTAX               PIC S9(5)V99    COMP-3.      00007004
01272      12  GER-DOMICILE-STAT.                                       00007005
01273          16  GXD-LFPRM               PIC S9(5)V99    COMP-3.      00007006
01274          16  GXD-LFCOM               PIC S9(5)V99    COMP-3.      00007007
01275          16  GXD-AHPRM               PIC S9(5)V99    COMP-3.      00007008
01276          16  GXD-AHCOM               PIC S9(5)V99    COMP-3.      00007009
01277      12  GER-STATE-STAT.                                          00007010
01278          16  GXS-LFPRM               PIC S9(5)V99    COMP-3.      00007011
01279          16  GXS-LFCOM               PIC S9(5)V99    COMP-3.      00007012
01280          16  GXS-AHPRM               PIC S9(5)V99    COMP-3.      00007013
01281          16  GXS-AHCOM               PIC S9(5)V99    COMP-3.      00007014
01282      12  GER-MORTALITY-DATA.                                      00007015
01283          16  GER-MORT-CTL.                                        00007016
01284              20  GER-MORT-CODE.                                   00007017
01285                  24  GER-MORT-TBL    PIC  X.                      00007018
01286                  24  GER-MORT-INT    PIC  XX.                     00007019
01287                  24  GER-MORT-TYP    PIC  X.                      00007020
01288              20  GER-MORT-AGE        PIC  99.                     00007021
01289          16  GER-MORT-DATA.                                       00007022
01290              20  GER-REM-AMT         PIC S9(7)V99    COMP-3.      00007023
01291              20  GER-MO-DEC          PIC S9(7)V99    COMP-3.      00007024
01292              20  GER-MORT-FACT       PIC S9(5)V9(4)  COMP-3.      00007025
01293              20  GER-RESV            PIC S9(5)V99    COMP-3.      00007026
01294          16  GER-FLAG                PIC  X.                      00007027
01295      12  GER-ALT-MORTALITY-DATA.                                  00007028
01296          16  GER-ALT-MORT-CODE.                                   00007029
01297              20  GER-ALT-MORT-TBL    PIC  X.                      00007030
01298              20  GER-ALT-MORT-INT    PIC  XX.                     00007031
01299              20  GER-ALT-MORT-TYP    PIC  X.                      00007032
01300          16  GER-ALT-MORT-DATA.                                   00007033
01301              20  GER-ALT-RESV        PIC S9(5)V99    COMP-3.      00007034
01302      12  GER-AGE                     PIC  99.                     00007035
01303      12  GER-CNT                     PIC S999        COMP-3.      00007036
01304      12  GER-CNT-LF                  PIC S999        COMP-3.      00007037
01305      12  GER-CNT-AH                  PIC S999        COMP-3.      00007038
01306      12  GER-UP-REM                  PIC S999V99     COMP-3.      00007039
01307      12  GER-REIN-COMP               PIC  XXX.                    00007040
01308      12  GER-IG                      PIC  X.                      00007041
01309      12  GER-ENT-DT.                                              00007042
01310          16  GER-ENT-YR              PIC  99.                     00007043
01311          16  GER-ENT-MO              PIC  99.                     00007044
01312          16  GER-ENT-DA              PIC  99.                     00007045
01313      12  GER-AH-REM-BEN              PIC S9(7)V99    COMP-3.      00007046
01314      12  GER-PMT-FREQ                PIC  99.                     00007047
01315      12  GER-APR                     PIC S999V9(4)   COMP-3.      00007048
01316      12  GER-ACC-EXPIRES             PIC  X(6).                   00007049
01317      12  GER-RATE-TERM               PIC  S999       COMP-3.      00007050
01318      12  GER-SUMMARY-FLAG            PIC  X.                      00007051
01319          88  GER-SUMMARY-REC             VALUE 'Z'.               00007052
01320      12  FILLER                      PIC  X(98).                  00007053
01321 ******************************************************************00007054
01322  EJECT                                                            00007055
01323  PROCEDURE DIVISION.                                              00007056
01324  0000-LOAD-DATE-CARD.                                             00007057
01325                              COPY ELCDTERX  SUPPRESS.             00007058
01326                                                                   00007059
01327  0100-INITIALIZATION-ROUTINE.                                     00007060
01328      OPEN INPUT CONTROL-CARD.                                     00007061
01329                                                                   00007062
01330      MOVE CURRENT-DATE           TO DC-GREG-DATE-1-EDIT.          00007063
01331      MOVE '2'                    TO DC-OPTION-CODE.               00007064
01332      PERFORM DATE-CONVERSION-ROUTINE THRU CONVERT-EXIT.           00007065
01333      MOVE DC-BIN-DATE-1          TO WS-BIN-MAINT-DT.              00007066
01334                                                                   00007067
01335      DISPLAY ' '.                                                 00007068
01336      DISPLAY ' '.                                                 00007069
01337      DISPLAY ' '.                                                 00007070
01338                                                                   00007071
01339  0200-READ-CONTROL-CARD.                                          00007072
01340      READ CONTROL-CARD  AT END  GO TO END-OF-JOB.                 00007073
01341                                                                   00007074
01342  0300-SELECT-FILE.                                                00007075
01343      IF CONVERTING-ALL-FILES  GO TO 1000-ACCT-CONVERSION-ROUTINE. 00007076
01344                                                                   00007077
01345      IF CONVERTING-ACCT  GO TO  1000-ACCT-CONVERSION-ROUTINE.     00007078
01346      IF CONVERTING-CERT  GO TO  2000-CERT-CONVERSION-ROUTINE.     00007079
01347      IF CONVERTING-CLMS  GO TO  3000-CLMS-CONVERSION-ROUTINE.     00007080
01348      IF CONVERTING-COMP  GO TO  4000-COMP-CONVERSION-ROUTINE.     00007081
01349      IF CONVERTING-CTBL  GO TO  5000-CTBL-CONVERSION-ROUTINE.     00007082
01350      IF CONVERTING-EPEC  OR                                       00007083
01351         CONVERTING-EXTR                                           00007084
01352          GO TO  6000-EPEC-CONVERSION-ROUTINE.                     00007085
01353      IF CONVERTING-RTBL  GO TO  7000-RTBL-CONVERSION-ROUTINE.     00007086
01354      IF CONVERTING-RATE  GO TO  8000-RATE-CONVERSION-ROUTINE.     00007087
01355      IF CONVERTING-ALPH  GO TO  9000-ALPH-CONVERSION-ROUTINE.     00007088
01356      IF CONVERTING-GAAP  GO TO 10000-GAAP-CONVERSION-ROUTINE.     00007089
01357                                                                   00007090
01358      GO TO 0200-READ-CONTROL-CARD.                                00007091
01359  EJECT                                                            00007092
01360  1000-ACCT-CONVERSION-ROUTINE.                                    00007093
01361      OPEN INPUT ACCT-IN                                           00007094
01362          OUTPUT ACCT-OUT.                                         00007095
01363                                                                   00007096
01364      IF ERVACCT-FILE-STATUS NOT = '00'  AND  '97'                 00007097
01365          MOVE ERVACCT-FILE-STATUS     TO WS-ABEND-FILE-STATUS     00007098
01366          MOVE 'OPEN ERROR - ERVACCT ' TO WS-ABEND-MESSAGE         00007099
01367          GO TO ABEND-PGM.                                         00007100
01368                                                                   00007101
01369  1010-READ-ACCT-MASTER.                                           00007102
01370      READ ACCT-IN INTO ACC-MSTR-REC                               00007103
01371                  AT END GO TO 1099-CLOSE-ACCT-MASTER.             00007104
01372                                                                   00007105
01373  1030-EDIT-ACCT-FIELDS.                                           00007106
01374      IF ACC-LF-RET     NOT NUMERIC  MOVE ZEROS TO ACC-LF-RET.     00007107
01375      IF ACC-AH-RET     NOT NUMERIC  MOVE ZEROS TO ACC-AH-RET.     00007108
01376      IF ACC-REI-FEE-LF NOT NUMERIC  MOVE ZEROS TO ACC-REI-FEE-LF. 00007109
01377      IF ACC-REI-FEE-AH NOT NUMERIC  MOVE ZEROS TO ACC-REI-FEE-AH. 00007110
01378      IF ACC-REI-LF-TAX NOT NUMERIC  MOVE ZEROS TO ACC-REI-LF-TAX. 00007111
01379      IF ACC-REI-AH-TAX NOT NUMERIC  MOVE ZEROS TO ACC-REI-AH-TAX. 00007112
01380      IF ACC-REI-PR-PCT NOT NUMERIC  MOVE ZEROS TO ACC-REI-PR-PCT. 00007113
01381      IF ACC-REI-78-PCT NOT NUMERIC  MOVE ZEROS TO ACC-REI-78-PCT. 00007114
01382      IF ACC-TOL-PREM   NOT NUMERIC  MOVE ZEROS TO ACC-TOL-PREM.   00007115
01383      IF ACC-TOL-REF    NOT NUMERIC  MOVE ZEROS TO ACC-TOL-REF.    00007116
01384      IF ACC-TOL-CLM    NOT NUMERIC  MOVE ZEROS TO ACC-TOL-CLM.    00007117
01385      IF ACC-RET-MIN-LOSS-L NOT NUMERIC                            00007118
01386                          MOVE ZEROS TO ACC-RET-MIN-LOSS-L.        00007119
01387      IF ACC-RET-MIN-LOSS-A NOT NUMERIC                            00007120
01388                          MOVE ZEROS TO ACC-RET-MIN-LOSS-A.        00007121
01389      IF ACC-LF-OB-RATE NOT NUMERIC  MOVE ZEROS TO ACC-LF-OB-RATE. 00007122
01390      IF ACC-AH-OB-RATE NOT NUMERIC  MOVE ZEROS TO ACC-AH-OB-RATE. 00007123
01391      IF ACC-JNT-LF-OB-RATE NOT NUMERIC                            00007124
01392                          MOVE ZEROS TO ACC-JNT-LF-OB-RATE.        00007125
01393                                                                   00007126
01394  1050-CONVERT-ACCT-MASTER.                                        00007127
01395      MOVE SPACES                 TO ACCOUNT-MASTER.               00007128
01396      MOVE 'AM'                   TO AM-RECORD-ID.                 00007129
01397      MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD.                00007130
01398                                                                   00007131
01399      MOVE ACC-CO-PRE             TO AM-CARRIER.                   00007132
01400      MOVE ACC-CO-NO              TO AM-GROUPING-PRIME.            00007133
01401      MOVE ACC-ST                 TO AM-STATE.                     00007134
01402      MOVE ACC-NO                 TO AM-ACCOUNT-PRIME.             00007135
01403      MOVE ACC-EXPIRES            TO AM-EXPIRE-DT.                 00007136
01404      MOVE ZEROS                  TO AM-GROUPING-PREFIX            00007137
01405                                     AM-ACCOUNT-PREFIX.            00007138
01406                                                                   00007139
01407      IF DTE-CLIENT = 'FIA'                                        00007140
01408         MOVE AM-ACCOUNT          TO AM-REPORT-CODE-1.             00007141
01409                                                                   00007142
01410      MOVE AM-CONTROL-PRIMARY     TO AM-CONTROL-BY-VAR-GRP.        00007143
01411      IF DTE-COMP-VG = ' '                                         00007144
01412          MOVE SPACES             TO AM-VG-CARRIER                 00007145
01413                                     AM-VG-GROUPING.               00007146
01414      IF DTE-COMP-VG = '2'                                         00007147
01415          MOVE SPACES             TO AM-VG-GROUPING.               00007148
01416      IF DTE-COMP-VG = '3'                                         00007149
01417          MOVE SPACES             TO AM-VG-CARRIER                 00007150
01418                                     AM-VG-GROUPING                00007151
01419                                     AM-VG-STATE.                  00007152
01420      IF DTE-COMP-VG = '4'                                         00007153
01421          MOVE SPACES             TO AM-VG-GROUPING                00007154
01422                                     AM-VG-STATE.                  00007155
01423                                                                   00014230
01424      MOVE WS-BIN-MAINT-DT        TO AM-LAST-MAINT-DT.             00014231
01425      MOVE ZEROS                  TO AM-LAST-MAINT-HHMMSS.         00014232
01426      MOVE 'LVL6'                 TO AM-LAST-MAINT-USER.           00014233
01427                                                                   00014234
01428      MOVE ACC-EFFECT             TO AM-EFFECT-DT                  00014235
01429                                     DC-GREG-DATE-1-YMD.           00014236
01430      MOVE '3'                    TO DC-OPTION-CODE.               00014237
01431      PERFORM DATE-CONVERSION-ROUTINE THRU CONVERT-EXIT.           00014238
01432      MOVE DC-BIN-DATE-1          TO AM-EFFECTIVE-DT.              00014239
01433                                                                   00014330
01434      MOVE ACC-PREV-DATES         TO AM-PREV-DATES.                00014331
01435                                                                   00014332
01436      MOVE ACC-CITY-CODE          TO AM-CITY-CODE.                 00014333
01437      MOVE ACC-COUNTY-CODE        TO WS-COUNTY.                    00014334
01438      MOVE ACC-PARRISH-CODE       TO WS-PARISH.                    00014335
01439      MOVE WS-COUNTY-PARISH       TO AM-COUNTY-PARISH.             00014336
01440                                                                   00014337
01441      MOVE ACC-NAME               TO AM-NAME.                      00014338
01442      MOVE ACC-PERSON             TO AM-PERSON.                    00014339
01443      MOVE ACC-ADDRS              TO AM-ADDRS                      00014340
01444      MOVE ACC-CITY               TO AM-CITY.                      00014341
01445      MOVE ACC-ZIP                TO AM-ZIP.                       00014342
01446 *    MOVE ACC-ZIP                TO AM-ZIP-PRIME.                 00014343
01447 *    MOVE SPACES                 TO AM-ZIP-PLUS4.                 00014344
01448      MOVE ACC-TEL-NO             TO AM-TEL-NO.                    00014345
01449      MOVE ACC-TEL-LOC            TO AM-TEL-LOC.                   00014346
01450                                                                   00014347
01451      MOVE ACC-BILLING-STATUS     TO AM-BILLING-STATUS.            00014348
01452      MOVE ACC-AUTO-REFUND-SW     TO AM-AUTO-REFUND-SW.            00014349
01453      MOVE ACC-GPCD               TO AM-GPCD.                      00014350
01454      MOVE ACC-IG                 TO AM-IG.                        00014351
01455      MOVE ACC-STATUS             TO AM-STATUS.                    00014352
01456      MOVE ACC-REMIT-TO           TO AM-REMIT-TO.                  00014353
01457      MOVE ACC-ID-NO              TO AM-ID-NO.                     00014354
01458                                                                   00014355
01459      MOVE ACC-CAL-TABLE          TO AM-CAL-TABLE.                 00014356
01460      MOVE ACC-DEVIATION          TO AM-LF-DEVIATION               00014357
01461                                     AM-AH-DEVIATION.              00014358
01462      MOVE ZEROS                  TO AM-LF-DEVIATION-PCT           00014359
01463                                     AM-AH-DEVIATION-PCT.          00014360
01464      MOVE ACC-LF-OB-RATE         TO AM-LF-OB-RATE.                00014361
01465      MOVE ACC-AH-OB-RATE         TO AM-AH-OB-RATE.                00014362
01466      MOVE ACC-JNT-LF-OB-RATE     TO AM-LF-OB-RATE-JNT.            00014363
01467      MOVE ZEROS                  TO AM-AH-OB-RATE-JNT.            00014364
01468                                                                   00014365
01469      MOVE ACC-USER-FIELDS        TO AM-USER-FIELDS.               00014366
01470      MOVE ACC-ANVR-DT            TO AM-ANNIVERSARY-DATE.          00014367
01471      MOVE ACC-LST-91-PURG        TO AM-CERTS-PURGED-DATE.         00014368
01472      MOVE ACC-35-HI-CERT         TO AM-HI-CERT-DATE.              00014369
01473      MOVE ACC-35-LO-CERT         TO AM-LO-CERT-DATE.              00014370
01474      MOVE ACC-ADD-DATE           TO AM-ENTRY-DATE.                00014371
01475      MOVE ACC-INA-DATE           TO AM-INACTIVE-DATE.             00014372
01476                                                                   00014373
01477      MOVE ACC-RECALC-COMM        TO AM-RECALC-COMM.               00014374
01478      MOVE ACC-RECALC-REIN        TO AM-RECALC-REIN.               00014375
01479                                                                   00014376
01480      MOVE ACC-REI-TABLE          TO AM-REI-TABLE.                 00014377
01481      MOVE ACC-REI-ET-LF          TO AM-REI-ET-LF.                 00014378
01482      MOVE ACC-REI-ET-AH          TO AM-REI-ET-AH.                 00014379
01483      MOVE ACC-REI-PE-LF          TO AM-REI-PE-LF.                 00014380
01484      MOVE ACC-REI-PE-AH          TO AM-REI-PE-AH.                 00014381
01485      MOVE ACC-REI-PRT-ST         TO AM-REI-PRT-ST.                00014382
01486      MOVE ACC-REI-FEE-LF         TO AM-REI-FEE-LF.                00014383
01487      MOVE ACC-REI-FEE-AH         TO AM-REI-FEE-AH.                00014384
01488      MOVE ACC-REI-LF-TAX         TO AM-REI-LF-TAX.                00014385
01489      MOVE ACC-REI-GROUP          TO AM-REI-GROUP-A.               00014386
01490      MOVE SPACES                 TO AM-REI-GROUP-B.               00014387
01491      MOVE ACC-REI-MORT           TO AM-REI-MORT.                  00014388
01492      MOVE ACC-REI-PRT-OW         TO AM-REI-PRT-OW.                00014389
01493      MOVE ACC-REI-PR-PCT         TO AM-REI-PR-PCT.                00014390
01494      MOVE ACC-REI-78-PCT         TO AM-REI-78-PCT.                00014391
01495      MOVE ACC-REI-AH-TAX         TO AM-REI-AH-TAX.                00014392
01496                                                                   00014960
01497      MOVE ACC-STD-AH-TYPE        TO AM-STD-AH-TYPE.               00014961
01498      MOVE ACC-EARN-METHODS       TO AM-EARN-METHODS.              00014962
01499                                                                   00014963
01500      MOVE ACC-TOL-PREM           TO AM-TOL-PREM.                  00014964
01501      MOVE ACC-TOL-REF            TO AM-TOL-REF.                   00014965
01502      MOVE ACC-TOL-CLM            TO AM-TOL-CLM.                   00014966
01503                                                                   00014967
01504      MOVE ACC-RET-Y-N            TO AM-RET-Y-N.                   00014968
01505      MOVE ACC-RET-P-E            TO AM-RET-P-E.                   00014969
01506      MOVE ACC-LF-RET             TO AM-LF-RET.                    00014970
01507      MOVE ACC-AH-RET             TO AM-AH-RET.                    00014971
01508      MOVE ACC-RET-GRP            TO AM-RET-GRP.                   00014972
01509      MOVE ACC-RETRO-EARNINGS     TO AM-RETRO-EARNINGS.            00014973
01510      MOVE ACC-RET-ST-TAX-USE     TO AM-RET-ST-TAX-USE.            00014974
01511      MOVE ACC-RETRO-BEG-EARNINGS TO AM-RETRO-BEG-EARNINGS.        00014975
01512      MOVE ACC-RET-MIN-LOSS-L     TO AM-RET-MIN-LOSS-L.            00014976
01513      MOVE ACC-RET-MIN-LOSS-A     TO AM-RET-MIN-LOSS-A.            00014977
01514                                                                   00015140
01515      IF DTE-CLIENT = 'MIC'  OR  'MCC'                             00015141
01516        IF ACC-TRANSFERRED-FROM NOT = SPACES                       00015142
01517            MOVE ACC-TRNFROM-CARR  TO AM-TRNFROM-CARRIER           00015143
01518            MOVE ZEROS             TO AM-TRNFROM-GRP-PREFIX        00015144
01519            MOVE ACC-TRNFROM-COMP  TO AM-TRNFROM-GRP-PRIME         00015145
01520            MOVE ACC-TRNFROM-STATE TO AM-TRNFROM-STATE             00015146
01521            MOVE ZEROS             TO AM-TRNFROM-ACCT-PREFIX       00015147
01522            MOVE ACC-TRNFROM-ACCT  TO AM-TRNFROM-ACCT-PRIME        00015148
01523            MOVE ACC-TRNFROM-DTE   TO AM-TRNFROM-DTE.              00015149
01524                                                                   00015150
01525      IF DTE-CLIENT = 'MIC'  OR  'MCC'                             00015151
01526        IF ACC-TRANSFERRED-TO NOT = SPACES                         00015152
01527            MOVE ACC-TRNTO-CARR   TO AM-TRNTO-CARRIER              00015153
01528            MOVE ZEROS            TO AM-TRNTO-GRP-PREFIX           00015154
01529            MOVE ACC-TRNTO-COMP   TO AM-TRNTO-GRP-PRIME            00015155
01530            MOVE ACC-TRNTO-STATE  TO AM-TRNTO-STATE                00015156
01531            MOVE ZEROS            TO AM-TRNTO-ACCT-PREFIX          00015157
01532            MOVE ACC-TRNTO-ACCT   TO AM-TRNTO-ACCT-PRIME           00015158
01533            MOVE ACC-TRNTO-DTE    TO AM-TRNTO-DTE.                 00015159
01534                                                                   00015340
01535      MOVE ACC-LINE-LIAB-LIMITS   TO AM-LIABILITY-LIMITS.          00015341
01536                                                                   00015342
01537      MOVE ACC-COMMENTS           TO AM-COMMENTS.                  00015343
01538                                                                   00015344
01539      MOVE +1                     TO X.                            00015345
01540  1070-MOVE-IN-COMMISSION-DATA.                                    00015346
01541      MOVE ZEROS                  TO AM-AGT-PREFIX (X).            00015347
01542      IF ACC-COM-TYP (X) = 'W'                                     00015348
01543          MOVE ACC-AGT (X)        TO WS-REIN-AGENT                 00015349
01544          MOVE WS-REINCO-SUB      TO WS-REINCO-NO                  00015350
01545          MOVE ZEROS              TO WS-REINCO-SUB                 00015351
01546          MOVE WS-REIN-AGENT      TO AM-AGT-PRIME (X)              00015352
01547      ELSE                                                         00015353
01548          MOVE ACC-AGT (X)        TO AM-AGT-PRIME (X).             00015354
01549      MOVE ACC-COM-TYP (X)        TO AM-COM-TYP (X).               00015355
01550      MOVE ACC-L-COMA (X)         TO AM-L-COMA (X).                00015356
01551      MOVE ACC-J-COMA (X)         TO AM-J-COMA (X).                00015357
01552      MOVE ACC-A-COMA (X)         TO AM-A-COMA (X).                00015358
01553      MOVE ACC-RET-LV-INDIC (X)   TO AM-RETRO-LV-INDIC (X).        00015359
01554      MOVE ACC-GL-CODES (X)       TO AM-GL-CODES (X).              00015360
01555                                                                   00015361
01556      IF X LESS THAN +10                                           00015362
01557          ADD +1 TO X                                              00015363
01558          GO TO 1070-MOVE-IN-COMMISSION-DATA.                      00015364
01559                                                                   00015365
01560  1080-SET-1ST-PRODUCTION-DATE.                                    00015366
01561      IF ACC-CNTRL-A = PRIOR-CNTRL-A                               00015367
01562          IF WS-1ST-PROD-DATE NOT = ZEROS                          00015368
01563              MOVE WS-1ST-PROD-DATE TO AM-1ST-PROD-DATE            00015369
01564              GO TO 1090-WRITE-ACC-MSTR-REC                        00015370
01565          ELSE                                                     00015371
01566              NEXT SENTENCE                                        00015372
01567      ELSE                                                         00015373
01568          MOVE ACC-CNTRL-A TO PRIOR-CNTRL-A                        00015374
01569          MOVE ZEROS TO WS-1ST-PROD-DATE.                          00015375
01570                                                                   00015376
01571      IF ACC-35-LO-CERT NOT NUMERIC                                00015377
01572          MOVE ZEROS TO WS-1ST-PROD-DATE                           00015378
01573      ELSE                                                         00015379
01574          MOVE ACC-35-LO-CERT TO WS-1ST-PROD-DATE.                 00015380
01575                                                                   00015381
01576      IF WS-1ST-PROD-DATE = ZEROS                                  00015382
01577          MOVE ZEROS TO AM-1ST-PROD-DATE                           00015383
01578          GO TO 1090-WRITE-ACC-MSTR-REC.                           00015384
01579                                                                   00015385
01580      ADD 1 TO WS-1ST-PROD-MO.                                     00015386
01581                                                                   00015810
01582      IF WS-1ST-PROD-MO GREATER 12                                 00015811
01583          MOVE 01 TO WS-1ST-PROD-MO                                00015812
01584          ADD 1 TO WS-1ST-PROD-YR.                                 00015813
01585                                                                   00015814
01586      MOVE 31 TO WS-1ST-PROD-DA.                                   00015815
01587      IF WS-1ST-PROD-MO = 02                                       00015816
01588          MOVE 28 TO WS-1ST-PROD-DA.                               00015817
01589      IF WS-1ST-PROD-MO = 04  OR  06  OR  09  OR  11               00015818
01590          MOVE 30 TO WS-1ST-PROD-DA.                               00015819
01591                                                                   00015820
01592      MOVE WS-1ST-PROD-DATE TO AM-1ST-PROD-DATE.                   00015821
01593                                                                   00015822
01594  1090-WRITE-ACC-MSTR-REC.                                         00015823
01595      ADD +1 TO ACCT-RCDS.                                         00015824
01596                                                                   00015825
01597      MOVE ACCOUNT-MASTER               TO ECS-ACCT-RECORD.        00015826
01598                                                                   00015827
01599      WRITE ECS-ACCT-RECORD.                                       00015828
01600                                                                   00015829
01601      IF ERVACCT-FILE-STATUS NOT = '00'                            00015830
01602          MOVE ERVACCT-FILE-STATUS      TO WS-ABEND-FILE-STATUS    00015831
01603          MOVE 'WRITE ERROR - ERVACCT ' TO WS-ABEND-MESSAGE        00015832
01604          GO TO ABEND-PGM.                                         00015833
01605                                                                   00015834
01606      GO TO 1010-READ-ACCT-MASTER.                                 00015835
01607                                                                   00015836
01608  1099-CLOSE-ACCT-MASTER.                                          00015837
01609      DISPLAY '     '.                                             00015838
01610      MOVE ACCT-RCDS              TO DISPLAY-RCDS.                 00015839
01611      DISPLAY 'ACCOUNT MASTER RECORDS CONVERTED....' DISPLAY-RCDS. 00015840
01612                                                                   00015841
01613      CLOSE ACCT-IN  ACCT-OUT.                                     00015842
01614                                                                   00015843
01615      IF NOT CONVERTING-ALL-FILES                                  00015844
01616          GO TO 0200-READ-CONTROL-CARD.                            00015845
01617  EJECT                                                            00015846
01618  2000-CERT-CONVERSION-ROUTINE.                                    00015847
01619      OPEN INPUT CERT-IN                                           00015848
01620          OUTPUT CERT-OUT.                                         00015849
01621                                                                   00015850
01622  2010-READ-CERT-MASTER.                                           00015851
01623      READ CERT-IN INTO CERTIFICATE-MASTER                         00015852
01624                  AT END GO TO 2099-CLOSE-CERT-MASTER.             00015853
01625                                                                   00015854
01626      IF CM-APR         NOT NUMERIC  MOVE ZEROS TO CM-APR.         00015855
01627      IF CM-PMT-FREQ    NOT NUMERIC  MOVE ZEROS TO CM-PMT-FREQ.    00015856
01628      IF CM-JOINT-AGE   NOT NUMERIC  MOVE ZEROS TO CM-JOINT-AGE.   00015857
01629      IF CM-SKIP        NOT NUMERIC  MOVE ZEROS TO CM-SKIP.        00015858
01630      IF CM-TERM-DAYS   NOT NUMERIC  MOVE ZEROS TO CM-TERM-DAYS.   00015859
01631      IF CM-RATE-TERM   NOT NUMERIC  MOVE ZEROS TO CM-RATE-TERM.   00015860
01632      IF CM-LFPRM-CALC  NOT NUMERIC  MOVE ZEROS TO CM-LFPRM-CALC.  00015861
01633      IF CM-AHPRM-CALC  NOT NUMERIC  MOVE ZEROS TO CM-AHPRM-CALC.  00015862
01634      IF CM-LFRFND      NOT NUMERIC  MOVE ZEROS TO CM-LFRFND.      00015863
01635      IF CM-AHRFND      NOT NUMERIC  MOVE ZEROS TO CM-AHRFND.      00015864
01636      IF CM-LFRFND-CALC NOT NUMERIC  MOVE ZEROS TO CM-LFRFND-CALC. 00015865
01637      IF CM-AHRFND-CALC NOT NUMERIC  MOVE ZEROS TO CM-AHRFND-CALC. 00015866
01638      IF CM-NUM-DTH-CLM NOT NUMERIC  MOVE ZEROS TO CM-NUM-DTH-CLM. 00015867
01639      IF CM-DTHAMT      NOT NUMERIC  MOVE ZEROS TO CM-DTHAMT.      00015868
01640      IF CM-DTHAMT-Y    NOT NUMERIC  MOVE ZEROS TO CM-DTHAMT-Y.    00015869
01641      IF CM-DTHAMT-LST  NOT NUMERIC  MOVE ZEROS TO CM-DTHAMT-LST.  00015870
01642      IF CM-DTH-AGE     NOT NUMERIC  MOVE ZEROS TO CM-DTH-AGE.     00015871
01643      IF CM-NUM-DIS-CLM NOT NUMERIC  MOVE ZEROS TO CM-NUM-DIS-CLM. 00015872
01644      IF CM-DSPY-AMT    NOT NUMERIC  MOVE ZEROS TO CM-DSPY-AMT.    00015873
01645      IF CM-DSPY-YTD    NOT NUMERIC  MOVE ZEROS TO CM-DSPY-YTD.    00015874
01646      IF CM-DSPY-LAST   NOT NUMERIC  MOVE ZEROS TO CM-DSPY-LAST.   00015875
01647      IF CM-DAYS-DISAB  NOT NUMERIC  MOVE ZEROS TO CM-DAYS-DISAB.  00015876
01648      IF CM-CANC-DT     NOT NUMERIC  MOVE ZEROS TO CM-CANC-DT.     00015877
01649      IF CM-DTH-DT      NOT NUMERIC  MOVE ZEROS TO CM-DTH-DT.      00015878
01650      IF CM-DTH-RPT-DT  NOT NUMERIC  MOVE ZEROS TO CM-DTH-RPT-DT.  00015879
01651      IF CM-DTH-PAY-DT  NOT NUMERIC  MOVE ZEROS TO CM-DTH-PAY-DT.  00015880
01652      IF CM-DIS-DT      NOT NUMERIC  MOVE ZEROS TO CM-DIS-DT.      00015881
01653      IF CM-DIS-RPT-DT  NOT NUMERIC  MOVE ZEROS TO CM-DIS-RPT-DT.  00015882
01654      IF CM-DIS-PAY-DT  NOT NUMERIC  MOVE ZEROS TO CM-DIS-PAY-DT.  00015883
01655      IF CM-DIS-PTO-DT  NOT NUMERIC  MOVE ZEROS TO CM-DIS-PTO-DT.  00015884
01656      IF CM-EXPIRE-DATE NOT NUMERIC  MOVE ZEROS TO CM-EXPIRE-DATE. 00015885
01657                                                                   00015886
01658      IF CM-CANCEL-EXIT-DATE NOT NUMERIC                           00015887
01659          MOVE ZEROS TO CM-CANCEL-EXIT-DATE.                       00015888
01660                                                                   00015889
01661      IF CM-CLAIM-EXIT-DATE NOT NUMERIC                            00015890
01662          MOVE ZEROS TO CM-CLAIM-EXIT-DATE.                        00015891
01663                                                                   00015892
01664      MOVE CM-DT                  TO DC-GREG-DATE-1-YMD.           00015893
01665      MOVE '3'                    TO DC-OPTION-CODE.               00015894
01666      PERFORM DATE-CONVERSION-ROUTINE THRU CONVERT-EXIT.           00015895
01667      MOVE DC-BIN-DATE-1          TO WS-BIN-EFF-DT.                00015896
01668      MOVE CM-TRM                 TO DC-ELAPSED-MONTHS.            00015897
01669      MOVE CM-TERM-DAYS           TO DC-ELAPSED-DAYS.              00015898
01670      MOVE '6'                    TO DC-OPTION-CODE.               00015899
01671      PERFORM DATE-CONVERSION-ROUTINE THRU CONVERT-EXIT.           00015900
01672      MOVE DC-GREG-DATE-1-YMD     TO CM-EXPIRE-DATE.               00015901
01673                                                                   00015902
01674      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  00015903
01675      MOVE CLAS-STARTA            TO CLAS-INDEXA.                  00015904
01676                                                                   00016760
01677      IF CM-LFTYP = ZERO                                           00016761
01678          GO TO 2020-FIND-AH-BENEFIT.                              00016762
01679                                                                   00016763
01680  2015-FIND-LIFE-BENEFIT.                                          00016764
01681      IF CLAS-INDEXL GREATER THAN CLAS-MAXL  OR  CLAS-STARTL = ZERO00016765
01682          DISPLAY 'LIFE BENEFIT ' CM-LFTYP ' NOT IN TABLE'         00016766
01683          MOVE 0401               TO WS-RETURN-CODE                00016767
01684          GO TO ABEND-PGM.                                         00016768
01685                                                                   00016769
01686      IF CM-LFTYP NOT = CLAS-I-BEN (CLAS-INDEXL)                   00016770
01687          ADD +1 TO CLAS-INDEXL                                    00016771
01688          GO TO 2015-FIND-LIFE-BENEFIT.                            00016772
01689                                                                   00016773
01690  2020-FIND-AH-BENEFIT.                                            00016774
01691      IF CM-AHTYP = ZERO                                           00016775
01692          GO TO 2025-CONVERT-CERT-MASTER.                          00016776
01693                                                                   00016777
01694  2022-FIND-AH-LOOP.                                               00016778
01695      IF CLAS-INDEXA GREATER THAN CLAS-MAXA  OR  CLAS-STARTA = ZERO00016779
01696          DISPLAY 'A&H BENEFIT ' CM-AHTYP ' NOT IN TABLE'          00016780
01697          MOVE 0402               TO WS-RETURN-CODE                00016781
01698          GO TO ABEND-PGM.                                         00016782
01699                                                                   00016783
01700      IF CM-AHTYP NOT = CLAS-I-BEN (CLAS-INDEXA)                   00016784
01701          ADD +1 TO CLAS-INDEXA                                    00016785
01702          GO TO 2022-FIND-AH-LOOP.                                 00016786
01703                                                                   00016787
01704  2025-CONVERT-CERT-MASTER.                                        00016788
01705      MOVE SPACES                 TO CERTIFICATE-RECORD.           00016789
01706      MOVE 'CR'                   TO CR-RECORD-ID.                 00016790
01707      MOVE DTE-CLASIC-COMPANY-CD  TO CR-COMPANY-CD.                00016791
01708                                                                   00016792
01709      MOVE CM-C-PRF               TO CR-CARRIER.                   00016793
01710      MOVE CM-C-NO                TO CR-GROUP-PRIME.               00016794
01711      MOVE CM-ST                  TO CR-STATE.                     00016795
01712      MOVE CM-ACC                 TO CR-ACCT-PRIME.                00016796
01713      MOVE CM-DT                  TO CR-DT.                        00016797
01714      MOVE CM-CRT                 TO CR-CERT-PRIME.                00016798
01715      MOVE CM-SUF                 TO CR-CERT-SFX.                  00016799
01716      MOVE ZEROS                  TO CR-GROUP-PREFIX               00016800
01717                                     CR-ACCT-PREFIX                00016801
01718                                     CR-CERT-PREFIX.               00016802
01719                                                                   00016803
01720      MOVE CM-LNAME               TO CR-LNAME.                     00016804
01721      MOVE CM-INIT                TO WS-INITIALS.                  00016805
01722      MOVE WS-1ST-INIT            TO CR-1ST-INITIAL.               00016806
01723      MOVE WS-MID-INIT            TO CR-INIT.                      00016807
01724      MOVE CM-AGE                 TO CR-AGE.                       00016808
01725      MOVE CM-SEX                 TO CR-SEX.                       00016809
01726      MOVE CM-SOC-SEC             TO CR-SOC-SEC.                   00016810
01727      MOVE CM-JOINT-AGE           TO CR-JOINT-AGE.                 00016811
01728                                                                   00016812
01729      MOVE CM-LFTYP               TO CR-LFTYP.                     00016813
01730      MOVE CM-TRM                 TO CR-LF-TERM.                   00016814
01731      MOVE ZEROS                  TO CR-LF-CRIT-PERIOD             00016815
01732                                     CR-LF-TERM-IN-DAYS.           00016816
01733      MOVE CM-DEV                 TO CR-LF-DEV-CODE.               00016817
01734      MOVE ZEROS                  TO CR-LF-DEV-PCT.                00016818
01735                                                                   00016819
01736      IF CM-LFTYP NOT = ZERO                                       00016820
01737        IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'B'  OR  'Z'           00016821
01738            MOVE CM-OB-LFAMT      TO CR-LFAMT                      00016822
01739        ELSE                                                       00016823
01740            MOVE CM-LFAMT         TO CR-LFAMT.                     00016824
01741                                                                   00016825
01742      MOVE CM-LFPRM               TO CR-LFPRM.                     00016826
01743      MOVE CM-LFPRM-CALC          TO CR-LFPRM-CALC.                00016827
01744      MOVE ZEROS                  TO CR-LFPRM-RATE                 00016828
01745                                     CR-LFAMT-ALT                  00016829
01746                                     CR-LFPRM-ALT                  00016830
01747                                     CR-LFPRM-CALC-ALT             00016831
01748                                     CR-LFPRM-RATE-ALT.            00016832
01749      MOVE CM-LFRFND              TO CR-LFRFND.                    00016833
01750      MOVE CM-LFRFND-CALC         TO CR-LFRFND-CALC.               00016834
01751                                                                   00016835
01752      IF CM-REIN-TABLE = SPACES  OR  ZEROS                         00016836
01753          MOVE ZEROS              TO CR-LF-NSP-PRM                 00016837
01754      ELSE                                                         00016838
01755          MOVE CM-LFPRM-CALC      TO CR-LF-NSP-PRM.                00016839
01756                                                                   00016840
01757      MOVE ZEROS                  TO CR-LF-NSP-PRM-RATE.           00016841
01758      MOVE CM-EXPIRE-DATE         TO CR-LF-EXPIRE-DATE.            00016842
01759                                                                   00016843
01760      MOVE CM-AHTYP               TO CR-AHTYP.                     00016844
01761      MOVE CM-TRM                 TO CR-AH-TERM.                   00016845
01762      MOVE ZEROS                  TO CR-AH-CRIT-PERIOD.            00016846
01763      MOVE CM-DEV                 TO CR-AH-DEV-CODE.               00016847
01764      MOVE ZEROS                  TO CR-AH-DEV-PCT.                00016848
01765                                                                   00016849
01766      IF CM-AHTYP NOT = ZERO                                       00016850
01767        IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'B'  OR  'Z'           00016851
01768            MOVE CM-OB-AHAMT      TO CR-AHAMT                      00016852
01769        ELSE                                                       00016853
01770            MOVE CM-AHAMT         TO CR-AHAMT.                     00016854
01771                                                                   00016855
01772      MOVE CM-AHPRM               TO CR-AHPRM.                     00016856
01773      MOVE CM-AHPRM-CALC          TO CR-AHPRM-CALC.                00016857
01774      MOVE ZEROS                  TO CR-AHPRM-RATE.                00016858
01775      MOVE CM-AHRFND              TO CR-AHRFND.                    00016859
01776      MOVE CM-AHRFND-CALC         TO CR-AHRFND-CALC.               00016860
01777                                                                   00016861
01778      IF CM-REIN-TABLE = SPACES  OR  ZEROS                         00016862
01779          MOVE ZEROS              TO CR-AH-NSP-PRM                 00016863
01780      ELSE                                                         00016864
01781          MOVE CM-AHPRM-CALC      TO CR-AH-NSP-PRM.                00016865
01782                                                                   00016866
01783      MOVE ZEROS                  TO CR-AH-NSP-PRM-RATE.           00016867
01784      MOVE CM-EXPIRE-DATE         TO CR-AH-EXPIRE-DATE.            00016868
01785                                                                   00016869
01786      IF DTE-CLIENT = 'FUL'                                        00016870
01787          MOVE +17.5213           TO CR-APR                        00016871
01788      ELSE                                                         00016872
01789          MOVE CM-APR             TO CR-APR.                       00016873
01790                                                                   00016874
01791      MOVE CM-PMT-FREQ            TO CR-PMT-FREQ.                  00016875
01792      MOVE CM-RATE-TERM           TO CR-LOAN-TERM.                 00016876
01793      MOVE CM-CLASS               TO CR-RATING-CLASS.              00016877
01794      MOVE CM-GRPTYP              TO CR-GRPTYP.                    00016878
01795                                                                   00016879
01796      IF CM-IND-GRP = 'I'                                          00016880
01797          MOVE '1'                TO CM-IND-GRP.                   00016881
01798                                                                   00016882
01799      IF CM-IND-GRP = 'G'                                          00016883
01800          MOVE '2'                TO CM-IND-GRP.                   00016884
01801                                                                   00016885
01802      IF CM-IND-GRP = '1'  OR  '2'                                 00016886
01803          MOVE CM-IND-GRP         TO CR-IND-GRP                    00016887
01804      ELSE                                                         00016888
01805          MOVE '2'                TO CR-IND-GRP.                   00016889
01806                                                                   00016890
01807      MOVE CM-SKIP                TO CR-SKIP.                      00016891
01808      MOVE CM-MORT                TO CR-MORT.                      00016892
01809      MOVE CM-MEMBER-NO           TO CR-MEMBER-NO.                 00016893
01810      MOVE CM-LN-OFFICER          TO CR-LOAN-OFFICER.              00016894
01811      MOVE CM-REIN-TABLE          TO CR-REIN-TABLE.                00016895
01812      MOVE CM-REIN-SPEC           TO CR-REIN-SPEC                  00016896
01813      MOVE WS-BIN-EFF-DT          TO DC-BIN-DATE-1.                00016897
01814                                                                   00016898
01815      IF CM-TERM-DAYS GREATER +30                                  00016899
01816          MOVE ZEROS              TO DC-ELAPSED-MONTHS             00016900
01817      ELSE                                                         00016901
01818          MOVE +1                 TO DC-ELAPSED-MONTHS.            00016902
01819                                                                   00018190
01820      MOVE CM-TERM-DAYS           TO DC-ELAPSED-DAYS.              00018191
01821      MOVE '6'                    TO DC-OPTION-CODE.               00018192
01822      PERFORM DATE-CONVERSION-ROUTINE THRU CONVERT-EXIT.           00018193
01823      MOVE DC-GREG-DATE-1-YMD     TO CR-LOAN-1ST-PMT-DT.           00018194
01824      MOVE CM-TERM-DAYS           TO CR-PMT-EXTENSION-DAYS.        00018195
01825                                                                   00018196
01826      MOVE ZEROS                  TO CR-LF-CANC-DT                 00018197
01827                                     CR-LF-CANCEL-EXIT-DATE        00018198
01828                                     CR-LF-CLAIM-EXIT-DATE         00018199
01829                                     CR-AH-CANC-DT                 00018200
01830                                     CR-AH-CANCEL-EXIT-DATE        00018201
01831                                     CR-AH-SETTLEMENT-EXIT-DATE.   00018202
01832                                                                   00018203
01833      MOVE CM-ENTRY-PLST          TO CR-ENTRY-STATUS.              00018204
01834      MOVE CM-ENTRY-YR            TO CR-ENTRY-YR.                  00018205
01835      MOVE CM-ENTRY-MO            TO CR-ENTRY-MO.                  00018206
01836      MOVE CM-ENTRY-DA            TO CR-ENTRY-DA.                  00018207
01837                                                                   00018208
01838      MOVE CM-PLST                TO CR-LF-CURRENT-STATUS          00018209
01839                                     CR-AH-CURRENT-STATUS.         00018210
01840                                                                   00018211
01841      IF CM-PLST = '6'  GO TO 2030-LUMP-SUM-CERT.                  00018212
01842      IF CM-PLST = '7'  GO TO 2040-DEATH-CERT.                     00018213
01843      IF CM-PLST = '8'  GO TO 2050-CANCELLED-CERT.                 00018214
01844                                                                   00018215
01845      GO TO 2060-CONTINUE-CONVERTING.                              00018216
01846                                                                   00018217
01847  2030-LUMP-SUM-CERT.                                              00018218
01848      MOVE CM-CLAIM-PLST          TO CR-AH-STATUS-AT-SETTLEMENT.   00018219
01849      MOVE CM-DEX-YR              TO CR-AH-DEX-YR.                 00018220
01850      MOVE CM-DEX-MO              TO CR-AH-DEX-MO.                 00018221
01851      MOVE CM-DEX-DA              TO CR-AH-DEX-DA.                 00018222
01852                                                                   00018223
01853      IF CM-CLAIM-PLST = '7'                                       00018224
01854          ADD +1                  TO DTH-AT-LUMP                   00018225
01855          MOVE CM-ENTRY-PLST      TO CR-LF-STATUS-AT-DEATH         00018226
01856          MOVE CM-DEX-YR          TO CR-LF-DEX-YR                  00018227
01857          MOVE CM-DEX-MO          TO CR-LF-DEX-MO                  00018228
01858          MOVE CM-DEX-DA          TO CR-LF-DEX-DA                  00018229
01859          MOVE '7'                TO CR-LF-CURRENT-STATUS          00018230
01860          GO TO 2060-CONTINUE-CONVERTING.                          00018231
01861                                                                   00018232
01862      IF CM-CLAIM-PLST = '8'                                       00018233
01863          ADD +1                  TO CAN-AT-LUMP                   00018234
01864          MOVE CM-CANCEL-PLST     TO CR-LF-STATUS-AT-CANCEL        00018235
01865                                     CR-AH-STATUS-AT-CANCEL        00018236
01866          MOVE CM-CNC-YR          TO CR-LF-CNC-YR  CR-AH-CNC-YR    00018237
01867          MOVE CM-CNC-MO          TO CR-LF-CNC-MO  CR-AH-CNC-MO    00018238
01868          MOVE CM-CNC-DA          TO CR-LF-CNC-DA  CR-AH-CNC-DA    00018239
01869          MOVE CM-CEX-YR          TO CR-LF-CEX-YR  CR-AH-CEX-YR    00018240
01870          MOVE CM-CEX-MO          TO CR-LF-CEX-MO  CR-AH-CEX-MO    00018241
01871          MOVE CM-CEX-DA          TO CR-LF-CEX-DA  CR-AH-CEX-DA    00018242
01872          MOVE '8'                TO CR-LF-CURRENT-STATUS          00018243
01873      ELSE                                                         00018244
01874          MOVE CM-CLAIM-PLST      TO CR-LF-STATUS-AT-CANCEL        00018245
01875          MOVE CM-DEX-YR          TO CR-LF-CEX-YR                  00018246
01876          MOVE CM-DEX-MO          TO CR-LF-CEX-MO                  00018247
01877          MOVE CM-DEX-DA          TO CR-LF-CEX-DA                  00018248
01878          MOVE CM-DIS-YR          TO CR-LF-CNC-YR                  00018249
01879          MOVE CM-DIS-MO          TO CR-LF-CNC-MO                  00018250
01880          MOVE CM-DIS-DA          TO CR-LF-CNC-DA                  00018251
01881          MOVE '8'                TO CR-LF-CURRENT-STATUS.         00018252
01882                                                                   00018253
01883      GO TO 2060-CONTINUE-CONVERTING.                              00018254
01884                                                                   00018255
01885  2040-DEATH-CERT.                                                 00018256
01886      MOVE CM-CLAIM-PLST          TO CR-LF-STATUS-AT-DEATH.        00018257
01887      MOVE CM-DEX-YR              TO CR-LF-DEX-YR.                 00018258
01888      MOVE CM-DEX-MO              TO CR-LF-DEX-MO.                 00018259
01889      MOVE CM-DEX-DA              TO CR-LF-DEX-DA.                 00018260
01890                                                                   00018261
01891      IF CM-CLAIM-PLST = '6'                                       00018262
01892          ADD +1                  TO LUMP-AT-DTH                   00018263
01893          MOVE CM-ENTRY-PLST      TO CR-AH-STATUS-AT-SETTLEMENT    00018264
01894          MOVE CM-DEX-YR          TO CR-AH-DEX-YR                  00018265
01895          MOVE CM-DEX-MO          TO CR-AH-DEX-MO                  00018266
01896          MOVE CM-DEX-DA          TO CR-AH-DEX-DA                  00018267
01897          MOVE '6'                TO CR-AH-CURRENT-STATUS          00018268
01898          GO TO 2060-CONTINUE-CONVERTING.                          00018269
01899                                                                   00018990
01900      IF CM-CLAIM-PLST = '8'                                       00018991
01901          ADD +1                  TO CAN-AT-DTH                    00018992
01902          MOVE CM-CANCEL-PLST     TO CR-LF-STATUS-AT-CANCEL        00018993
01903                                     CR-AH-STATUS-AT-CANCEL        00018994
01904          MOVE CM-CEX-YR          TO CR-LF-CEX-YR  CR-AH-CEX-YR    00018995
01905          MOVE CM-CEX-MO          TO CR-LF-CEX-MO  CR-AH-CEX-MO    00018996
01906          MOVE CM-CEX-DA          TO CR-LF-CEX-DA  CR-AH-CEX-DA    00018997
01907          MOVE CM-CNC-YR          TO CR-LF-CNC-YR  CR-AH-CNC-YR    00018998
01908          MOVE CM-CNC-MO          TO CR-LF-CNC-MO  CR-AH-CNC-MO    00018999
01909          MOVE CM-CNC-DA          TO CR-LF-CNC-DA  CR-AH-CNC-DA    00019000
01910          MOVE '8'                TO CR-AH-CURRENT-STATUS          00019001
01911      ELSE                                                         00019002
01912          MOVE CM-CLAIM-PLST      TO CR-AH-STATUS-AT-CANCEL        00019003
01913          MOVE CM-DEX-YR          TO CR-AH-CEX-YR                  00019004
01914          MOVE CM-DEX-MO          TO CR-AH-CEX-MO                  00019005
01915          MOVE CM-DEX-DA          TO CR-AH-CEX-DA                  00019006
01916          MOVE CM-DTH-YR          TO CR-AH-CNC-YR                  00019007
01917          MOVE CM-DTH-MO          TO CR-AH-CNC-MO                  00019008
01918          MOVE CM-DTH-DA          TO CR-AH-CNC-DA                  00019009
01919          MOVE '8'                TO CR-AH-CURRENT-STATUS.         00019010
01920                                                                   00019011
01921      GO TO 2060-CONTINUE-CONVERTING.                              00019012
01922                                                                   00019013
01923  2050-CANCELLED-CERT.                                             00019014
01924      MOVE CM-CANCEL-PLST         TO CR-LF-STATUS-AT-CANCEL        00019015
01925                                     CR-AH-STATUS-AT-CANCEL.       00019016
01926                                                                   00019017
01927      MOVE CM-CNC-YR              TO CR-LF-CNC-YR  CR-AH-CNC-YR.   00019018
01928      MOVE CM-CNC-MO              TO CR-LF-CNC-MO  CR-AH-CNC-MO.   00019019
01929      MOVE CM-CNC-DA              TO CR-LF-CNC-DA  CR-AH-CNC-DA.   00019020
01930      MOVE CM-CEX-YR              TO CR-LF-CEX-YR  CR-AH-CEX-YR.   00019021
01931      MOVE CM-CEX-MO              TO CR-LF-CEX-MO  CR-AH-CEX-MO.   00019022
01932      MOVE CM-CEX-DA              TO CR-LF-CEX-DA  CR-AH-CEX-DA.   00019023
01933                                                                   00019024
01934      IF CM-CANCEL-PLST = '6'                                      00019025
01935          ADD +1                  TO LUMP-AT-CAN                   00019026
01936          MOVE CM-CLAIM-PLST      TO CR-AH-STATUS-AT-SETTLEMENT    00019027
01937          MOVE CM-DEX-YR          TO CR-AH-DEX-YR                  00019028
01938          MOVE CM-DEX-MO          TO CR-AH-DEX-MO                  00019029
01939          MOVE CM-DEX-DA          TO CR-AH-DEX-DA                  00019030
01940            GO TO 2060-CONTINUE-CONVERTING.                        00019031
01941                                                                   00019032
01942      IF CM-CANCEL-PLST = '7'                                      00019033
01943          ADD +1                  TO DTH-AT-CAN                    00019034
01944          MOVE CM-CLAIM-PLST      TO CR-LF-STATUS-AT-DEATH         00019035
01945          MOVE CM-DEX-YR          TO CR-LF-DEX-YR                  00019036
01946          MOVE CM-DEX-MO          TO CR-LF-DEX-MO                  00019037
01947          MOVE CM-DEX-DA          TO CR-LF-DEX-DA.                 00019038
01948                                                                   00019039
01949  2060-CONTINUE-CONVERTING.                                        00019040
01950      MOVE CM-NUM-DTH-CLM         TO CR-NUM-DTH-CLM.               00019041
01951      MOVE CM-DTH-YR              TO CR-DTH-YR.                    00019042
01952      MOVE CM-DTH-MO              TO CR-DTH-MO.                    00019043
01953      MOVE CM-DTH-DA              TO CR-DTH-DA.                    00019044
01954      MOVE CM-DTH-RPT-YR          TO CR-DTH-RPT-YR.                00019045
01955      MOVE CM-DTH-RPT-MO          TO CR-DTH-RPT-MO.                00019046
01956      MOVE CM-DTH-RPT-DA          TO CR-DTH-RPT-DA.                00019047
01957      MOVE CM-DTH-PAY-YR          TO CR-DTH-PAY-YR.                00019048
01958      MOVE CM-DTH-PAY-MO          TO CR-DTH-PAY-MO.                00019049
01959      MOVE CM-DTH-PAY-DA          TO CR-DTH-PAY-DA.                00019050
01960                                                                   00019051
01961      MOVE CM-DTHAMT              TO CR-DTHAMT.                    00019052
01962      MOVE CM-DTHAMT-Y            TO CR-DTHAMT-YTD.                00019053
01963      MOVE CM-DTHAMT-LST          TO CR-DTHAMT-LAST.               00019054
01964      MOVE ZEROS                  TO CR-DTHEXP                     00019055
01965                                     CR-DTHEXP-YTD.                00019056
01966                                                                   00019057
01967      MOVE CM-DTH-AGE             TO CR-DTH-AGE.                   00019058
01968      MOVE CM-DTH-PAY-CD          TO CR-DTH-PAY-CD.                00019059
01969      MOVE CM-CLAIM-CAUSE         TO CR-DEATH-CAUSE.               00019060
01970                                                                   00019700
01971      MOVE CM-NUM-DIS-CLM         TO CR-NUM-DIS-CLM.               00019701
01972      MOVE CM-DIS-YR              TO CR-DIS-YR.                    00019702
01973      MOVE CM-DIS-MO              TO CR-DIS-MO.                    00019703
01974      MOVE CM-DIS-DA              TO CR-DIS-DA.                    00019704
01975      MOVE CM-DIS-RPT-YR          TO CR-DIS-RPT-YR.                00019705
01976      MOVE CM-DIS-RPT-MO          TO CR-DIS-RPT-MO.                00019706
01977      MOVE CM-DIS-RPT-DA          TO CR-DIS-RPT-DA.                00019707
01978      MOVE CM-DIS-PAY-YR          TO CR-DIS-PAY-YR.                00019708
01979      MOVE CM-DIS-PAY-MO          TO CR-DIS-PAY-MO.                00019709
01980      MOVE CM-DIS-PAY-DA          TO CR-DIS-PAY-DA.                00019710
01981      MOVE CM-DIS-PTO-YR          TO CR-DIS-PTO-YR.                00019711
01982      MOVE CM-DIS-PTO-MO          TO CR-DIS-PTO-MO.                00019712
01983      MOVE CM-DIS-PTO-DA          TO CR-DIS-PTO-DA.                00019713
01984                                                                   00019714
01985      MOVE CM-DSPY-AMT            TO CR-DISAMT.                    00019715
01986      MOVE CM-DSPY-YTD            TO CR-DISAMT-YTD.                00019716
01987      MOVE CM-DSPY-LAST           TO CR-DISAMT-LAST.               00019717
01988      MOVE ZEROS                  TO CR-DISEXP                     00019718
01989                                     CR-DISEXP-YTD.                00019719
01990                                                                   00019720
01991      MOVE CM-DAYS-DISAB          TO CR-DAYS-DISAB.                00019721
01992      MOVE CM-DIS-PAY-CD          TO CR-DIS-PAY-CD.                00019722
01993      MOVE CM-CLAIM-CAUSE         TO CR-DISAB-CAUSE.               00019723
01994                                                                   00019724
01995      MOVE CM-USER-CODE           TO CR-USER-CODE.                 00019725
01996                                                                   00019726
01997      MOVE +1                     TO X.                            00019727
01998  2070-MOVE-IN-COMMISSION-DATA.                                    00019728
01999      MOVE ZEROS                  TO CR-COM-AGT-PREFIX (X).        00019729
02000                                                                   00019730
02001      IF CM-AGT-TYPE (X) = 'W'                                     00019731
02002          MOVE CM-COM-AGT (X)     TO WS-REIN-AGENT                 00019732
02003          MOVE WS-REINCO-SUB      TO WS-REINCO-NO                  00019733
02004          MOVE ZEROS              TO WS-REINCO-SUB                 00019734
02005          MOVE WS-REIN-AGENT      TO CR-COM-AGT-PRIME (X)          00019735
02006      ELSE                                                         00019736
02007          MOVE CM-COM-AGT (X)     TO CR-COM-AGT-PRIME (X).         00019737
02008                                                                   00019738
02009      MOVE CM-AGT-TYPE (X)        TO CR-AGT-TYPE (X).              00019739
02010                                                                   00019740
02011      IF CM-LCOM-L (X) NUMERIC                                     00019741
02012          MOVE CM-LCOM-L (X)      TO CR-LCOM-L (X)                 00019742
02013      ELSE                                                         00019743
02014          MOVE ZEROS              TO CR-LCOM-L (X).                00019744
02015                                                                   00019745
02016      IF CM-LCOM-AH (X) NUMERIC                                    00019746
02017          MOVE CM-LCOM-AH (X)     TO CR-LCOM-AH (X)                00019747
02018      ELSE                                                         00019748
02019          MOVE ZEROS              TO CR-LCOM-AH (X).               00019749
02020                                                                   00020200
02021      IF X LESS +10                                                00020201
02022          ADD +1 TO X                                              00020202
02023          GO TO 2070-MOVE-IN-COMMISSION-DATA.                      00020203
02024                                                                   00020204
02025      IF (CM-LFTYP NOT = 00  AND                                   00020205
02026          CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z')  OR                00020206
02027         (CM-AHTYP NOT = 00  AND                                   00020207
02028          CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z')                    00020208
02029          NEXT SENTENCE                                            00020209
02030      ELSE                                                         00020210
02031          MOVE ZEROS              TO CR-LIVES                      00020211
02032                                     CR-SUM-CAN-CNT-ITD            00020212
02033                                     CR-SUM-CAN-CNT-YTD            00020213
02034          GO TO 2080-CHECK-FOR-COVERAGE.                           00020214
02035                                                                   00020350
02036      IF CM-LIVES NUMERIC                                          00020351
02037          MOVE CM-LIVES           TO CR-LIVES                      00020352
02038      ELSE                                                         00020353
02039          MOVE ZEROS              TO CR-LIVES.                     00020354
02040                                                                   00020355
02041      IF CM-CANCEL-CNT-ITD NUMERIC                                 00020356
02042          MOVE CM-CANCEL-CNT-ITD  TO CR-SUM-CAN-CNT-ITD            00020357
02043      ELSE                                                         00020358
02044          MOVE ZEROS              TO CR-SUM-CAN-CNT-ITD.           00020359
02045                                                                   00020360
02046      IF CM-CANCEL-CNT-YTD NUMERIC                                 00020361
02047          MOVE CM-CANCEL-CNT-YTD  TO CR-SUM-CAN-CNT-YTD            00020362
02048      ELSE                                                         00020363
02049          MOVE ZEROS              TO CR-SUM-CAN-CNT-YTD.           00020364
02050                                                                   00020365
02051      MOVE SPACES                 TO CR-MEMBER-NO.                 00020366
02052                                                                   00020520
02053  2080-CHECK-FOR-COVERAGE.                                         00020521
02054      IF CR-LFTYP = ZEROS                                          00020522
02055          MOVE ZEROS              TO CR-LF-TERM                    00020523
02056                                     CR-LF-TERM-IN-DAYS            00020524
02057                                     CR-LF-DEV-CODE                00020525
02058                                     CR-LFAMT                      00020526
02059                                     CR-LFPRM                      00020527
02060                                     CR-LFPRM-CALC                 00020528
02061                                     CR-LFRFND                     00020529
02062                                     CR-LFRFND-CALC                00020530
02063                                     CR-LF-EXPIRE-DATE             00020531
02064                                     CR-LF-CANC-DT                 00020532
02065                                     CR-LF-CANCEL-EXIT-DATE        00020533
02066                                     CR-LF-CLAIM-EXIT-DATE         00020534
02067                                     CR-NUM-DTH-CLM                00020535
02068                                     CR-DTH-DT                     00020536
02069                                     CR-DTH-RPT-DT                 00020537
02070                                     CR-DTH-PAY-DT                 00020538
02071                                     CR-DTHAMT                     00020539
02072                                     CR-DTHAMT-YTD                 00020540
02073                                     CR-DTHAMT-LAST                00020541
02074                                     CR-DTH-AGE                    00020542
02075          MOVE SPACES             TO CR-LF-CURRENT-STATUS          00020543
02076                                     CR-LF-STATUS-AT-CANCEL        00020544
02077                                     CR-LF-STATUS-AT-DEATH         00020545
02078                                     CR-DTH-PAY-CD                 00020546
02079                                     CR-DEATH-CAUSE.               00020547
02080                                                                   00020800
02081      IF CR-AHTYP = ZEROS                                          00020801
02082          MOVE ZEROS              TO CR-AH-TERM                    00020802
02083                                     CR-AH-DEV-CODE                00020803
02084                                     CR-AHAMT                      00020804
02085                                     CR-AHPRM                      00020805
02086                                     CR-AHPRM-CALC                 00020806
02087                                     CR-AHRFND                     00020807
02088                                     CR-AHRFND-CALC                00020808
02089                                     CR-AH-EXPIRE-DATE             00020809
02090                                     CR-AH-CANC-DT                 00020810
02091                                     CR-AH-CANCEL-EXIT-DATE        00020811
02092                                     CR-AH-SETTLEMENT-EXIT-DATE    00020812
02093                                     CR-NUM-DIS-CLM                00020813
02094                                     CR-DIS-DT                     00020814
02095                                     CR-DIS-RPT-DT                 00020815
02096                                     CR-DIS-PAY-DT                 00020816
02097                                     CR-DIS-PTO-DT                 00020817
02098                                     CR-DISAMT                     00020818
02099                                     CR-DISAMT-YTD                 00020819
02100                                     CR-DISAMT-LAST                00020820
02101                                     CR-DAYS-DISAB                 00020821
02102          MOVE SPACES             TO CR-AH-CURRENT-STATUS          00020822
02103                                     CR-AH-STATUS-AT-CANCEL        00020823
02104                                     CR-AH-STATUS-AT-SETTLEMENT    00020824
02105                                     CR-DIS-PAY-CD                 00020825
02106                                     CR-DISAB-CAUSE.               00020826
02107                                                                   00021070
02108  2090-WRITE-CERTIFICATE-MASTER.                                   00021071
02109      ADD +1 TO CERT-RCDS.                                         00021072
02110                                                                   00021073
02111      WRITE ECS-CERTIFICATE-MASTER FROM CERTIFICATE-RECORD.        00021074
02112                                                                   00021075
02113      GO TO 2010-READ-CERT-MASTER.                                 00021076
02114                                                                   00021077
02115  2099-CLOSE-CERT-MASTER.                                          00021078
02116      DISPLAY ' '.                                                 00021079
02117      DISPLAY ' '.                                                 00021080
02118      MOVE DTH-AT-LUMP            TO DISPLAY-RCDS.                 00021081
02119      DISPLAY 'LUMP SUM WITH DEATH STATUS..........' DISPLAY-RCDS. 00021082
02120      MOVE CAN-AT-LUMP            TO DISPLAY-RCDS.                 00021083
02121      DISPLAY 'LUMP SUM WITH CANCEL STATUS.........' DISPLAY-RCDS. 00021084
02122      MOVE LUMP-AT-DTH            TO DISPLAY-RCDS.                 00021085
02123      DISPLAY 'DEATH WITH LUMP SUM STATUS..........' DISPLAY-RCDS. 00021086
02124      MOVE CAN-AT-DTH             TO DISPLAY-RCDS.                 00021087
02125      DISPLAY 'DEATH WITH CANCEL STATUS............' DISPLAY-RCDS. 00021088
02126      MOVE LUMP-AT-CAN            TO DISPLAY-RCDS.                 00021089
02127      DISPLAY 'CANCEL WITH LUMP SUM STATUS.........' DISPLAY-RCDS. 00021090
02128      MOVE DTH-AT-CAN             TO DISPLAY-RCDS.                 00021091
02129      DISPLAY 'CANCEL WITH DEATH STATUS............' DISPLAY-RCDS. 00021092
02130      DISPLAY ' '.                                                 00021093
02131      DISPLAY ' '.                                                 00021094
02132      DISPLAY ' '.                                                 00021095
02133      MOVE CERT-RCDS              TO DISPLAY-RCDS.                 00021096
02134      DISPLAY 'CERT MASTER RECORDS CONVERTED.......' DISPLAY-RCDS. 00021097
02135                                                                   00021098
02136      CLOSE CERT-IN  CERT-OUT.                                     00021099
02137                                                                   00021100
02138      IF NOT CONVERTING-ALL-FILES                                  00021101
02139          GO TO 0200-READ-CONTROL-CARD.                            00021102
02140  EJECT                                                            00021103
02141  3000-CLMS-CONVERSION-ROUTINE.                                    00021104
02142      OPEN INPUT CLMS-IN                                           00021105
02143          OUTPUT CLMS-OUT.                                         00021106
02144                                                                   00021107
02145  3010-READ-CLMS-MASTER.                                           00021108
02146      READ CLMS-IN INTO CAC-CLAIM-EXTR                             00021109
02147                  AT END GO TO 3099-CLOSE-CLMS-MASTER.             00021110
02148                                                                   00021480
02149  3030-EDIT-CLMS-FIELDS.                                           00021481
02150      IF CAC-AMT        NOT NUMERIC  MOVE ZEROS TO CAC-AMT.        00021482
02151      IF CAC-REI-CLAIM  NOT NUMERIC  MOVE ZEROS TO CAC-REI-CLAIM.  00021483
02152      IF CAC-CLM-AGE    NOT NUMERIC  MOVE ZEROS TO CAC-CLM-AGE.    00021484
02153      IF CAC-DAYS-DISAB NOT NUMERIC  MOVE ZEROS TO CAC-DAYS-DISAB. 00021485
02154      IF CAC-LF-RFND    NOT NUMERIC  MOVE ZEROS TO CAC-LF-RFND.    00021486
02155      IF CAC-AH-RFND    NOT NUMERIC  MOVE ZEROS TO CAC-AH-RFND.    00021487
02156      IF CAC-LF-CNBEN   NOT NUMERIC  MOVE ZEROS TO CAC-LF-CNBEN.   00021488
02157      IF CAC-PMT-FREQ   NOT NUMERIC  MOVE ZEROS TO CAC-PMT-FREQ.   00021489
02158      IF CAC-REI-LFAMT  NOT NUMERIC  MOVE ZEROS TO CAC-REI-LFAMT.  00021490
02159      IF CAC-REI-LFPRM  NOT NUMERIC  MOVE ZEROS TO CAC-REI-LFPRM.  00021491
02160      IF CAC-REI-LFRFND NOT NUMERIC  MOVE ZEROS TO CAC-REI-LFRFND. 00021492
02161      IF CAC-REI-AHAMT  NOT NUMERIC  MOVE ZEROS TO CAC-REI-AHAMT.  00021493
02162      IF CAC-REI-AHPRM  NOT NUMERIC  MOVE ZEROS TO CAC-REI-AHPRM.  00021494
02163      IF CAC-REI-AHRFND NOT NUMERIC  MOVE ZEROS TO CAC-REI-AHRFND. 00021495
02164      IF CAC-REI-CNAMT  NOT NUMERIC  MOVE ZEROS TO CAC-REI-CNAMT.  00021496
02165                                                                   00021497
02166  3050-CONVERT-CLMS-MASTER.                                        00021498
02167      MOVE SPACES                 TO DE-CLAIM-EXTRACT.             00021499
02168      MOVE 'DE'                   TO DE-RECORD-ID.                 00021500
02169      MOVE DTE-CLASIC-COMPANY-CD  TO DE-COMPANY-CD.                00021501
02170      MOVE CAC-REIN               TO DE-REIN.                      00021502
02171                                                                   00021503
02172      MOVE CAC-CARR               TO DE-CARRIER.                   00021504
02173      MOVE CAC-CMP                TO DE-GROUP-PRIME.               00021505
02174      MOVE CAC-STATE              TO DE-STATE.                     00021506
02175      MOVE CAC-ACCT               TO DE-ACCT-PRIME.                00021507
02176      MOVE CAC-EFF                TO DE-EFF.                       00021508
02177      MOVE CAC-CRT-NO             TO DE-CRT-PRIME.                 00021509
02178      MOVE CAC-CRT-SUF            TO DE-CRT-SUF.                   00021510
02179      MOVE ZEROS                  TO DE-GROUP-PREFIX               00021511
02180                                     DE-ACCT-PREFIX                00021512
02181                                     DE-CRT-PREFIX.                00021513
02182                                                                   00021514
02183      MOVE CAC-TRANS              TO DE-TRANS.                     00021515
02184      MOVE CAC-LNAME              TO DE-LNAME.                     00021516
02185      MOVE CAC-INIT               TO WS-INITIALS.                  00021517
02186      MOVE WS-1ST-INIT            TO DE-1ST-INIT-FNAME.            00021518
02187      MOVE WS-MID-INIT            TO DE-INIT.                      00021519
02188      MOVE CAC-AGE                TO DE-AGE.                       00021520
02189      MOVE CAC-SEX                TO DE-SEX.                       00021521
02190      MOVE CAC-SSN                TO DE-SOC-SEC-NO.                00021522
02191                                                                   00021523
02192      MOVE CAC-LF-TYPE            TO DE-LF-TYPE.                   00021524
02193      MOVE CAC-TERM               TO DE-LF-TERM.                   00021525
02194      MOVE CAC-LF-BEN             TO DE-LF-BEN.                    00021526
02195      MOVE CAC-LF-CNBEN           TO DE-LF-CNBEN.                  00021527
02196      MOVE CAC-LF-PRM             TO DE-LF-PRM.                    00021528
02197      MOVE ZEROS                  TO DE-LF-BEN-ALT                 00021529
02198                                     DE-LF-CNBEN-ALT               00021530
02199                                     DE-LF-PRM-ALT                 00021531
02200      MOVE CAC-LF-RFND            TO DE-LF-RFND.                   00021532
02201                                                                   00021533
02202      MOVE CAC-STAT-CDE           TO DE-LF-STAT-CDE.               00021534
02203      MOVE CAC-PREV-STAT          TO DE-LF-PREV-STAT.              00021535
02204      MOVE ZEROS                  TO DE-LF-CANC-DTE                00021536
02205                                     DE-LF-CANC-EXIT-DT.           00021537
02206                                                                   00021538
02207      MOVE CAC-AH-TYPE            TO DE-AH-TYPE.                   00021539
02208      MOVE CAC-TERM               TO DE-AH-TERM.                   00021540
02209      MOVE CAC-AH-BEN             TO DE-AH-BEN.                    00021541
02210      MOVE CAC-AH-PRM             TO DE-AH-PRM.                    00021542
02211      MOVE CAC-AH-RFND            TO DE-AH-RFND.                   00021543
02212                                                                   00021544
02213      MOVE CAC-STAT-CDE           TO DE-AH-STAT-CDE.               00021545
02214      MOVE CAC-PREV-STAT          TO DE-AH-PREV-STAT.              00021546
02215      MOVE ZEROS                  TO DE-AH-CANC-DTE                00021547
02216                                     DE-AH-CANC-EXIT-DT.           00021548
02217                                                                   00021549
02218      MOVE ZEROS                  TO DE-LIVES                      00021550
02219                                     DE-CANCEL-CNT-ITD             00021551
02220                                     DE-CANCEL-CNT-YTD.            00021552
02221                                                                   00021553
02222      MOVE ZEROS                  TO DE-APR.                       00021554
02223      MOVE CAC-PMT-FREQ           TO DE-PMT-FREQ.                  00021555
02224      MOVE CAC-ACC-GPCD           TO DE-ACC-GPCD.                  00021556
02225      MOVE CAC-IG                 TO DE-IG.                        00021557
02226      MOVE CAC-REMIT-TO           TO DE-REMIT-TO.                  00021558
02227      MOVE CAC-MEMBER-NO          TO DE-MEMBER-NO.                 00021559
02228                                                                   00021560
02229      MOVE CAC-ENTRY-PLST         TO DE-ENTRY-STATUS.              00021561
02230      MOVE CAC-ENTRY-DTE          TO DE-ENTRY-DTE.                 00021562
02231                                                                   00022310
02232      IF DE-REIN = 'R'                                             00022311
02233          MOVE CAC-REI-COMP       TO DE-REINCO                     00022312
02234          MOVE ZEROS              TO DE-REINCO-SUB                 00022313
02235          MOVE CAC-REI-LFAMT      TO DE-REI-LFAMT                  00022314
02236          MOVE CAC-REI-LFPRM      TO DE-REI-LFPRM                  00022315
02237          MOVE CAC-REI-LFRFND     TO DE-REI-LFRFND                 00022316
02238          MOVE CAC-REI-AHAMT      TO DE-REI-AHAMT                  00022317
02239          MOVE CAC-REI-AHPRM      TO DE-REI-AHPRM                  00022318
02240          MOVE CAC-REI-AHRFND     TO DE-REI-AHRFND                 00022319
02241          MOVE CAC-REI-CNAMT      TO DE-REI-CNAMT                  00022320
02242      ELSE                                                         00022321
02243          MOVE ZEROS              TO DE-REI-LFAMT                  00022322
02244                                     DE-REI-LFPRM                  00022323
02245                                     DE-REI-LFRFND                 00022324
02246                                     DE-REI-AHAMT                  00022325
02247                                     DE-REI-AHPRM                  00022326
02248                                     DE-REI-AHRFND                 00022327
02249                                     DE-REI-CNAMT.                 00022328
02250                                                                   00022329
02251      MOVE CAC-TYPE               TO DE-TYPE.                      00022330
02252      MOVE CAC-AMT                TO DE-CLAIM-AMT.                 00022331
02253      MOVE CAC-REI-CLAIM          TO DE-REI-CLAIM-AMT.             00022332
02254                                                                   00022333
02255      MOVE CAC-INCUR-YR           TO DE-INCUR-YR.                  00022334
02256      MOVE CAC-INCUR-MO           TO DE-INCUR-MO.                  00022335
02257      MOVE CAC-INCUR-DA           TO DE-INCUR-DA.                  00022336
02258      MOVE CAC-RPT-YR             TO DE-RPT-YR.                    00022337
02259      MOVE CAC-RPT-MO             TO DE-RPT-MO.                    00022338
02260      MOVE CAC-RPT-DA             TO DE-RPT-DA.                    00022339
02261      MOVE CAC-PAY-YR             TO DE-PAY-YR.                    00022340
02262      MOVE CAC-PAY-MO             TO DE-PAY-MO.                    00022341
02263      MOVE CAC-PAY-DA             TO DE-PAY-DA.                    00022342
02264      MOVE CAC-PTO-YR             TO DE-PTO-YR.                    00022343
02265      MOVE CAC-PTO-MO             TO DE-PTO-MO.                    00022344
02266      MOVE CAC-PTO-DA             TO DE-PTO-DA.                    00022345
02267                                                                   00022346
02268      MOVE CAC-CNUM               TO DE-CNUM.                      00022347
02269      MOVE CAC-CHECK              TO WS-CHECK-NO.                  00022348
02270      MOVE WS-CHECK-NUMBER        TO DE-CHECK.                     00022349
02271      MOVE ZEROS                  TO DE-PMT-TRAILER-SEQ.           00022350
02272      MOVE CAC-DAYS-DISAB         TO DE-DAYS-DISAB.                00022351
02273      MOVE CAC-CLM-AGE            TO DE-CLM-AGE.                   00022352
02274      MOVE CAC-PAY-CODE           TO DE-PAY-CODE.                  00022353
02275      MOVE CAC-CLM-ERR            TO DE-CLM-ERR.                   00022354
02276                                                                   00022355
02277      MOVE CAC-ACC-NAME           TO DE-ACC-NAME.                  00022356
02278      MOVE CAC-ACC-EXP-DTE        TO DE-ACC-EXP-DTE.               00022357
02279      MOVE CAC-ACC-EFF-DTE        TO DE-ACC-EFF-DTE.               00022358
02280                                                                   00022359
02281      MOVE CAC-CLM-CAUSE          TO DE-CLM-CAUSE.                 00022360
02282      MOVE CAC-CM-LN-OFFICER      TO DE-LOAN-OFFICER.              00022361
02283                                                                   00022362
02284      MOVE CAC-CLM-PROC-DT        TO DE-CLM-PROC-DT.               00022363
02285                                                                   00022364
02286  3090-WRITE-CLMS-RECORD.                                          00022365
02287      ADD +1 TO CLMS-RCDS.                                         00022366
02288                                                                   00022367
02289      WRITE ECS-CLMS-RECORD FROM DE-CLAIM-EXTRACT.                 00022368
02290                                                                   00022900
02291      GO TO 3010-READ-CLMS-MASTER.                                 00022901
02292                                                                   00022902
02293  3099-CLOSE-CLMS-MASTER.                                          00022903
02294      DISPLAY ' '.                                                 00022904
02295      MOVE CLMS-RCDS              TO DISPLAY-RCDS.                 00022905
02296      DISPLAY 'CLAIMS HISTORY RECORDS CONVERTED....' DISPLAY-RCDS. 00022906
02297                                                                   00022970
02298      CLOSE CLMS-IN  CLMS-OUT.                                     00022971
02299                                                                   00022972
02300      IF NOT CONVERTING-ALL-FILES                                  00022973
02301          GO TO 0200-READ-CONTROL-CARD.                            00022974
02302  EJECT                                                            00022975
02303  4000-COMP-CONVERSION-ROUTINE.                                    00022976
02304      OPEN INPUT COMP-IN                                           00022977
02305          OUTPUT COMP-OUT.                                         00022978
02306                                                                   00023060
02307  4010-READ-COMP-MASTER.                                           00023061
02308      READ COMP-IN INTO COMP-RECORD                                00023062
02309                  AT END GO TO 4099-CLOSE-COMP-MASTER.             00023063
02310                                                                   00023064
02311  4050-CONVERT-COMP-MASTER.                                        00023065
02312      MOVE SPACES                 TO COMPENSATION-MASTER.          00023066
02313      MOVE 'CO'                   TO CO-RECORD-ID.                 00023067
02314      MOVE DTE-CLASIC-COMPANY-CD  TO CO-COMPANY-CD.                00023068
02315                                                                   00023150
02316      MOVE CMR-CARR               TO CO-CARRIER.                   00023151
02317      MOVE ZEROS                  TO CO-GROUP-PREFIX.              00023152
02318      MOVE CMR-COMP               TO CO-GROUP-PRIME.               00023153
02319      MOVE CMR-TYPE               TO CO-TYPE.                      00023154
02320                                                                   00023155
02321      IF CO-COMPANY-TYPE                                           00023156
02322          MOVE LOW-VALUES         TO CO-RESP-NO                    00023157
02323                                     CO-ACCOUNT.                   00023158
02324                                                                   00023159
02325      IF CO-GEN-AGENT-TYPE                                         00023160
02326          MOVE ZEROS              TO CO-RESP-PREFIX                00023161
02327          MOVE CMR-RESP           TO CO-RESP-PRIME                 00023162
02328          MOVE LOW-VALUES         TO CO-ACCOUNT.                   00023163
02329                                                                   00023164
02330      IF CO-ACCOUNT-TYPE                                           00023165
02331          MOVE ZEROS              TO CO-RESP-PREFIX                00023166
02332          MOVE CMR-RESP           TO CO-RESP-PRIME                 00023167
02333          MOVE ZEROS              TO CO-ACCT-PREFIX                00023168
02334          MOVE CMR-ACCT           TO CO-ACCT-PRIME.                00023169
02335                                                                   00023170
02336      MOVE CMR-YEAR               TO CO-ACT-YEAR.                  00023171
02337      MOVE CMR-MONTH              TO CO-ACT-MONTH.                 00023172
02338                                                                   00023173
02339      IF CMR-MONTH = '02'                                          00023174
02340          MOVE '28'               TO CO-ACT-DAY                    00023175
02341      ELSE                                                         00023176
02342          IF CMR-MONTH = '04' OR '06' OR '09' OR '11'              00023177
02343              MOVE '30'           TO CO-ACT-DAY                    00023178
02344          ELSE                                                     00023179
02345              MOVE '31'           TO CO-ACT-DAY.                   00023180
02346                                                                   00023460
02347      MOVE CO-LAST-ACTIVITY-DATE  TO DC-GREG-DATE-1-YMD.           00023461
02348      MOVE '3'                    TO DC-OPTION-CODE.               00023462
02349      PERFORM DATE-CONVERSION-ROUTINE THRU CONVERT-EXIT.           00023463
02350      MOVE DC-BIN-DATE-1          TO CO-LAST-MAINT-DT.             00023464
02351      MOVE ZEROS                  TO CO-LAST-MAINT-HHMMSS.         00023465
02352      MOVE 'LVL6'                 TO CO-LAST-MAINT-USER.           00023466
02353                                                                   00023467
02354      MOVE CMR-BALANCE-CONTROL    TO CO-BALANCE-CONTROL.           00023468
02355      MOVE CMR-INTERNAL-CONTROL-1 TO CO-INTERNAL-CONTROL-1.        00023469
02356      MOVE CMR-INTERNAL-CONTROL-2 TO CO-INTERNAL-CONTROL-2.        00023470
02357                                                                   00023471
02358      MOVE CMR-ACCT-NAME          TO CO-ACCT-NAME.                 00023472
02359      MOVE CMR-MAIL-NAME          TO CO-MAIL-NAME.                 00023473
02360      MOVE CMR-ADDR-1             TO CO-ADDR-1.                    00023474
02361      MOVE CMR-ADDR-2             TO CO-ADDR-2.                    00023475
02362      MOVE CMR-ADDR-3             TO CO-ADDR-3.                    00023476
02363      MOVE CMR-ZIP                TO CO-ZIP.                       00023477
02364      MOVE CMR-SOC-SEC            TO CO-SOC-SEC.                   00023478
02365      MOVE CMR-TELEPHONE          TO CO-TELEPHONE.                 00023479
02366                                                                   00023660
02367      MOVE CMR-FUTURE             TO CO-USER-CODE.                 00023661
02368      MOVE CMR-ACT-YEAR           TO CO-ACT-YEAR.                  00023662
02369      MOVE CMR-ACT-MONTH          TO CO-ACT-MONTH.                 00023663
02370                                                                   00023664
02371      IF CMR-ACT-MONTH = '02'                                      00023665
02372          MOVE '28'               TO CO-ACT-DAY                    00023666
02373      ELSE                                                         00023667
02374          IF CMR-ACT-MONTH = '04' OR '06' OR '09' OR '11'          00023668
02375              MOVE '30'           TO CO-ACT-DAY                    00023669
02376          ELSE                                                     00023670
02377              MOVE '31'           TO CO-ACT-DAY.                   00023671
02378                                                                   00023672
02379      IF CMR-LAST-STMT-DT IS NOT NUMERIC                           00023673
02380          MOVE ZEROS              TO CO-LAST-STMT-DT               00023674
02381      ELSE                                                         00023675
02382          MOVE CMR-LAST-STMT-DT   TO CO-LAST-STMT-DT.              00023676
02383                                                                   00023677
02384      MOVE CMR-MONTHLY-TOTALS     TO CO-MONTHLY-TOTALS.            00023678
02385      MOVE CMR-AGING-TOTALS       TO CO-AGING-TOTALS.              00023679
02386      MOVE CMR-YTD-TOTALS         TO CO-YTD-TOTALS.                00023680
02387      MOVE CMR-OVER-UNDER-TOTALS  TO CO-OVER-UNDER-TOTALS.         00023681
02388                                                                   00023682
02389      IF CMR-CUR-FICA IS NOT NUMERIC                               00023683
02390          MOVE ZEROS              TO CO-CUR-FICA                   00023684
02391      ELSE                                                         00023685
02392          MOVE CMR-CUR-FICA       TO CO-CUR-FICA.                  00023686
02393                                                                   00023687
02394      IF CMR-YTD-FICA IS NOT NUMERIC                               00023688
02395          MOVE ZEROS              TO CO-YTD-FICA                   00023689
02396      ELSE                                                         00023690
02397          MOVE CMR-YTD-FICA       TO CO-YTD-FICA.                  00023691
02398                                                                   00023692
02399      IF CMR-LF-CLM-AMT IS NOT NUMERIC                             00023693
02400          MOVE ZEROS              TO CO-LF-CLM-AMT                 00023694
02401      ELSE                                                         00023695
02402          MOVE CMR-LF-CLM-AMT     TO CO-LF-CLM-AMT.                00023696
02403                                                                   00023697
02404      IF CMR-AH-CLM-AMT IS NOT NUMERIC                             00023698
02405          MOVE ZEROS              TO CO-AH-CLM-AMT                 00023699
02406      ELSE                                                         00023700
02407          MOVE CMR-AH-CLM-AMT     TO CO-AH-CLM-AMT.                00023701
02408                                                                   00023702
02409      MOVE CO-LAST-STMT-DT        TO CO-CURRENT-LAST-STMT-DT.      00023703
02410      MOVE CO-MONTHLY-TOTALS      TO CO-CURRENT-MONTHLY-TOTALS.    00023704
02411      MOVE CO-AGING-TOTALS        TO CO-CURRENT-AGING-TOTALS.      00023705
02412      MOVE CO-YTD-TOTALS          TO CO-CURRENT-YTD-TOTALS.        00023706
02413                                                                   00023707
02414  4090-WRITE-COMP-RECORD.                                          00023708
02415      ADD +1 TO COMP-RCDS.                                         00023709
02416                                                                   00023710
02417      WRITE ECS-COMP-RECORD FROM COMPENSATION-MASTER.              00023711
02418                                                                   00023712
02419      GO TO 4010-READ-COMP-MASTER.                                 00023713
02420                                                                   00023714
02421  4099-CLOSE-COMP-MASTER.                                          00023715
02422      DISPLAY ' '.                                                 00023716
02423      MOVE COMP-RCDS              TO DISPLAY-RCDS.                 00023717
02424      DISPLAY 'COMPENSATION MASTER RCDS CONVERTED..' DISPLAY-RCDS. 00023718
02425                                                                   00023719
02426      CLOSE COMP-IN  COMP-OUT.                                     00023720
02427                                                                   00023721
02428      IF NOT CONVERTING-ALL-FILES                                  00023722
02429          GO TO 0200-READ-CONTROL-CARD.                            00023723
02430  EJECT                                                            00023724
02431  5000-CTBL-CONVERSION-ROUTINE.                                    00023725
02432      OPEN INPUT CTBL-IN                                           00023726
02433          OUTPUT CTBL-OUT.                                         00023727
02434                                                                   00023728
02435      IF ERVCTBL-FILE-STATUS NOT = '00'  AND  '97'                 00023729
02436          MOVE ERVCTBL-FILE-STATUS      TO WS-ABEND-FILE-STATUS    00023730
02437          MOVE 'OPEN ERROR - ERVCTBL ' TO WS-ABEND-MESSAGE         00023731
02438          GO TO ABEND-PGM.                                         00023732
02439                                                                   00023733
02440  5010-READ-CTBL-MASTER.                                           00023734
02441      READ CTBL-IN INTO CTBL-RECORD                                00023735
02442                  AT END GO TO 5099-CLOSE-CTBL-MASTER.             00023736
02443                                                                   00024430
02444  5050-CONVERT-CTBL-MASTER.                                        00024431
02445      MOVE SPACES                 TO COMM-TABLE-RECORD.            00024432
02446      MOVE 'CT'                   TO CT-RECORD-ID.                 00024433
02447      MOVE DTE-CLASIC-COMPANY-CD  TO CT-COMPANY-CD.                00024434
02448      MOVE XCT-TABLE              TO CT-TABLE.                     00024435
02449                                                                   00024436
02450      IF XCT-BEN-TYPE = 'L'                                        00024437
02451          MOVE LIFE-OVERRIDE-L1   TO CT-BEN-TYPE                   00024438
02452      ELSE                                                         00024439
02453          MOVE AH-OVERRIDE-L1     TO CT-BEN-TYPE.                  00024440
02454                                                                   00024441
02455      MOVE XCT-BEN-CODE           TO CT-BEN-CODE.                  00024442
02456                                                                   00024443
02457      MOVE WS-BIN-MAINT-DT        TO CT-LAST-MAINT-DT.             00024444
02458      MOVE ZEROS                  TO CT-LAST-MAINT-HHMMSS.         00024445
02459      MOVE 'LVL6'                 TO CT-LAST-MAINT-USER.           00024446
02460                                                                   00024447
02461      MOVE XCT-LIMITS             TO CT-LIMITS.                    00024448
02462      MOVE XCT-RATES              TO CT-RATES.                     00024449
02463                                                                   00024450
02464  5090-WRITE-CTBL-RECORD.                                          00024451
02465      ADD +1 TO CTBL-RCDS.                                         00024452
02466                                                                   00024453
02467      MOVE COMM-TABLE-RECORD            TO ECS-CTBL-RECORD.        00024454
02468                                                                   00024455
02469      WRITE ECS-CTBL-RECORD.                                       00024456
02470                                                                   00024457
02471      IF ERVCTBL-FILE-STATUS NOT = '00'                            00024458
02472          MOVE ERVCTBL-FILE-STATUS      TO WS-ABEND-FILE-STATUS    00024459
02473          MOVE 'WRITE ERROR - ERVCTBL ' TO WS-ABEND-MESSAGE        00024460
02474          GO TO ABEND-PGM.                                         00024461
02475                                                                   00024462
02476      GO TO 5010-READ-CTBL-MASTER.                                 00024463
02477                                                                   00024464
02478  5099-CLOSE-CTBL-MASTER.                                          00024465
02479      DISPLAY ' '.                                                 00024466
02480      MOVE CTBL-RCDS              TO DISPLAY-RCDS.                 00024467
02481      DISPLAY 'COMMISSION TABLE RCDS CONVERTED.....' DISPLAY-RCDS. 00024468
02482                                                                   00024469
02483      CLOSE CTBL-IN  CTBL-OUT.                                     00024470
02484                                                                   00024471
02485      IF NOT CONVERTING-ALL-FILES                                  00024472
02486          GO TO 0200-READ-CONTROL-CARD.                            00024473
02487  EJECT                                                            00024474
02488  6000-EPEC-CONVERSION-ROUTINE.                                    00024475
02489      OPEN INPUT EPEC-IN                                           00024476
02490          OUTPUT EPEC-OUT.                                         00024477
02491                                                                   00024478
02492  6010-READ-EPEC-MASTER.                                           00024479
02493      READ EPEC-IN INTO EPEC-RECORD                                00024480
02494                  AT END GO TO 6099-CLOSE-EPEC-MASTER.             00024481
02495                                                                   00024482
02496      IF CONVERTING-EXTR                                           00024483
02497          IF XP-CODE = 'C'                                         00024484
02498              GO TO 6010-READ-EPEC-MASTER.                         00024485
02499                                                                   00024486
02500  6050-CONVERT-EPEC-RECORD.                                        00024487
02501      MOVE SPACES                 TO EP-RECORD.                    00024488
02502                                                                   00024489
02503      MOVE DTE-CLASIC-COMPANY-CD  TO EP-COMPANY-CD.                00024490
02504      MOVE XP-REIN                TO EP-REIN.                      00024491
02505      MOVE XP-CARR                TO EP-CARRIER.                   00024492
02506      MOVE XP-CMP                 TO EP-GROUP-PRIME.               00024493
02507      MOVE XP-STATE               TO EP-STATE.                     00024494
02508      MOVE XP-ACCT                TO EP-ACCT-PRIME.                00024495
02509      MOVE XP-EXP-DTE             TO EP-EXP-DTE.                   00024496
02510      MOVE XP-EFF-DTE             TO EP-EFF-DTE.                   00024497
02511      MOVE ZEROS                  TO EP-GROUP-PREFIX               00024498
02512                                     EP-ACCT-PREFIX.               00024499
02513                                                                   00024500
02514      IF EP-REIN = 'R'                                             00024501
02515          MOVE XP-REI-CO          TO EP-REINCO                     00024502
02516          MOVE ZEROS              TO EP-REINCO-SUB.                00024503
02517                                                                   00024504
02518      IF XP-LF-AH-CODE = 'L'                                       00024505
02519          MOVE LIFE-OVERRIDE-L1   TO EP-RCD-TYPE                   00024506
02520      ELSE                                                         00024507
02521          MOVE AH-OVERRIDE-L1     TO EP-RCD-TYPE.                  00024508
02522                                                                   00024509
02523      MOVE XP-BEN-TYPE            TO EP-BEN-CODE.                  00024510
02524      MOVE XP-PURGE               TO EP-PURGE.                     00024511
02525      MOVE XP-RUN-DTE             TO EP-RUN-DTE.                   00024512
02526                                                                   00024513
02527      IF XP-CODE = 'U'                                             00024514
02528          GO TO 6080-CONVERT-EC-RECORD.                            00024515
02529                                                                   00025290
02530  6070-CONVERT-EP-RECORD.                                          00025291
02531      MOVE 'EP'                   TO EP-RECORD-ID.                 00025292
02532                                                                   00025293
02533      MOVE XP-ISS-CNT             TO EP-ISS-CNT.                   00025294
02534      MOVE XP-ISS-BEN             TO EP-ISS-BEN.                   00025295
02535      MOVE ZEROS                  TO EP-ISS-BEN-GROSS.             00025296
02536                                                                   00025297
02537      MOVE XP-CNC-CNT             TO EP-CNC-CNT.                   00025298
02538      MOVE XP-CNC-BEN             TO EP-CNC-BEN.                   00025299
02539      MOVE ZEROS                  TO EP-CNC-BEN-GROSS.             00025300
02540                                                                   00025301
02541      MOVE XP-ISS-PRM             TO EP-ISS-PRM.                   00025302
02542      MOVE ZEROS                  TO EP-ISS-PRM-GROSS.             00025303
02543      MOVE XP-CNC-PRM             TO EP-CNC-PRM.                   00025304
02544      MOVE ZEROS                  TO EP-CNC-PRM-GROSS.             00025305
02545                                                                   00025306
02546      MOVE XP-PRM-78              TO EP-PRM-78.                    00025307
02547      MOVE XP-PRM-PR              TO EP-PRM-PR.                    00025308
02548      MOVE ZEROS                  TO EP-PRM-ST.                    00025309
02549                                                                   00025310
02550      MOVE XP-CLM-AMT             TO EP-CLM-AMT.                   00025311
02551      MOVE XP-CLM-CNT             TO EP-CLM-CNT.                   00025312
02552      MOVE XP-CLM-CRT             TO EP-CLM-CRT.                   00025313
02553                                                                   00025530
02554      MOVE XP-CLM-DU              TO EP-CLM-DU.                    00025531
02555      MOVE XP-CLM-PV              TO EP-CLM-PV.                    00025532
02556      MOVE XP-CLM-IBNR            TO EP-CLM-IBNR.                  00025533
02557      MOVE XP-LOSS-RESV           TO EP-LOSS-RESV.                 00025534
02558      MOVE XP-CLAIM-ADJ           TO EP-CLAIM-ADJ.                 00025535
02559                                                                   00025536
02560      MOVE XP-RETRO-EXPENSES      TO EP-RETRO-EXPENSES.            00025537
02561      MOVE XP-RETRO-PAYMENTS      TO EP-RETRO-PAYMENTS.            00025538
02562      MOVE XP-RETRO-OTH-COMM      TO EP-RETRO-OTH-COMM.            00025539
02563                                                                   00025540
02564      MOVE XP-MORT-RESV           TO EP-MORT-RESV.                 00025541
02565      MOVE XP-IN-FORCE            TO EP-IN-FORCE.                  00025542
02566      MOVE XP-ADJUST              TO EP-ADJUST.                    00025543
02567                                                                   00025670
02568      MOVE ZEROS                  TO EP-AVG-AGE                    00025671
02569                                     EP-AVG-ORIG-TERM              00025672
02570                                     EP-WEIGHTED-AGE               00025673
02571                                     EP-WEIGHTED-ORIG-TERM         00025674
02572                                     EP-AVG-REM-TERM               00025675
02573                                     EP-INFORCE-CNT.               00025676
02574                                                                   00025740
02575      MOVE XP-HI-COV-DT           TO EP-HI-COV-DT.                 00025741
02576      MOVE XP-HI-CERT             TO EP-HI-CERT.                   00025742
02577      MOVE XP-LO-CERT             TO EP-LO-CERT.                   00025743
02578                                                                   00025744
02579      GO TO 6090-WRITE-EPEC-RECORD.                                00025745
02580                                                                   00025746
02581  6080-CONVERT-EC-RECORD.                                          00025747
02582      MOVE 'EC'                   TO EC-RECORD-ID.                 00025748
02583                                                                   00025749
02584      MOVE XC-SEQ-NBR             TO EC-SEQ-NBR.                   00025750
02585                                                                   00025751
02586      MOVE +1                     TO X.                            00025752
02587                                                                   00025753
02588  6085-MOVE-IN-COMMISSION-DATA.                                    00025754
02589      MOVE ZEROS                  TO EC-AGT-PREFIX (X).            00025755
02590                                                                   00025900
02591      IF XC-AGT-TYPE (X) = 'W'                                     00025901
02592          MOVE XC-AGT-NO (X)      TO WS-REIN-AGENT                 00025902
02593          MOVE WS-REINCO-SUB      TO WS-REINCO-NO                  00025903
02594          MOVE ZEROS              TO WS-REINCO-SUB                 00025904
02595          MOVE WS-REIN-AGENT      TO EC-AGT-PRIME (X)              00025905
02596      ELSE                                                         00025906
02597          MOVE XC-AGT-NO (X)      TO EC-AGT-PRIME (X).             00025907
02598                                                                   00025908
02599      MOVE XC-AGT-TYPE (X)        TO EC-AGT-TYPE (X).              00025909
02600      MOVE XC-ISS-COMM (X)        TO EC-ISS-COMM (X).              00025910
02601      MOVE XC-CNC-COMM (X)        TO EC-CNC-COMM (X).              00025911
02602      MOVE XC-COMM-78 (X)         TO EC-COMM-78 (X).               00025912
02603      MOVE XC-COMM-PR (X)         TO EC-COMM-PR (X).               00025913
02604      MOVE ZEROS                  TO EC-COMM-ST (X).               00025914
02605                                                                   00025915
02606      IF X LESS THAN +5                                            00025916
02607          ADD +1 TO X                                              00025917
02608          GO TO 6085-MOVE-IN-COMMISSION-DATA.                      00025918
02609                                                                   00025919
02610  6090-WRITE-EPEC-RECORD.                                          00025920
02611      ADD +1 TO EPEC-RCDS.                                         00025921
02612                                                                   00025922
02613      WRITE ECS-EPEC-RECORD FROM EP-RECORD.                        00025923
02614                                                                   00026140
02615      GO TO 6010-READ-EPEC-MASTER.                                 00026141
02616                                                                   00026160
02617  6099-CLOSE-EPEC-MASTER.                                          00026161
02618      DISPLAY ' '.                                                 00026162
02619      MOVE EPEC-RCDS              TO DISPLAY-RCDS.                 00026163
02620      DISPLAY 'E.P.E.C. FILE RECORDS CONVERTED.....' DISPLAY-RCDS. 00026164
02621                                                                   00026165
02622      CLOSE EPEC-IN  EPEC-OUT.                                     00026166
02623                                                                   00026167
02624      IF NOT CONVERTING-ALL-FILES                                  00026168
02625          GO TO 0200-READ-CONTROL-CARD.                            00026169
02626  EJECT                                                            00026170
02627  7000-RTBL-CONVERSION-ROUTINE.                                    00026171
02628      OPEN INPUT RTBL-IN                                           00026172
02629          OUTPUT RTBL-OUT.                                         00026173
02630                                                                   00026174
02631      IF ERVRTBL-FILE-STATUS NOT = '00'  AND  '97'                 00026175
02632          MOVE ERVRTBL-FILE-STATUS     TO WS-ABEND-FILE-STATUS     00026176
02633          MOVE 'OPEN ERROR - ERVRTBL ' TO WS-ABEND-MESSAGE         00026177
02634          GO TO ABEND-PGM.                                         00026178
02635                                                                   00026179
02636  7010-READ-RTBL-MASTER.                                           00026180
02637      READ RTBL-IN INTO RT-TAB-REC                                 00026181
02638                  AT END GO TO 7099-CLOSE-RTBL-MASTER.             00026182
02639                                                                   00026390
02640  7050-CONVERT-RTBL-MASTER.                                        00026391
02641      MOVE SPACES                 TO REINSURANCE-RECORD.           00026392
02642      MOVE 'RE'                   TO RE-RECORD-ID.                 00026393
02643      MOVE DTE-CLASIC-COMPANY-CD  TO RE-COMPANY-CD.                00026394
02644                                                                   00026395
02645      MOVE LOW-VALUES             TO RE-KEY.                       00026396
02646      MOVE RT-CODE                TO RE-CODE.                      00026397
02647      MOVE RT-TABLE               TO RE-TABLE.                     00026398
02648                                                                   00026480
02649      MOVE WS-BIN-MAINT-DT        TO RE-LAST-MAINT-DT.             00026481
02650      MOVE ZEROS                  TO RE-LAST-MAINT-HHMMSS.         00026482
02651      MOVE 'LVL6'                 TO RE-LAST-MAINT-USER.           00026483
02652                                                                   00026484
02653      MOVE +1                     TO X.                            00026485
02654                                                                   00026486
02655      IF RE-COMPANY-RECORD                                         00026487
02656          MOVE ZEROS              TO RE-COMP-SUB                   00026488
02657          GO TO 7080-CONVERT-B-RECORD.                             00026489
02658                                                                   00026490
02659  7070-CONVERT-A-RECORD.                                           00026491
02660      MOVE ZEROS        TO RE-LFAGE-LO (X)     RE-LFAGE-HI (X)     00026492
02661                           RE-AHAGE-LO (X)     RE-AHAGE-HI (X)     00026493
02662                           RE-LFTRM-LO (X)     RE-LFTRM-HI (X)     00026494
02663                           RE-AHTRM-LO (X)     RE-AHTRM-HI (X)     00026495
02664                           RE-LF-PCT (X)       RE-AH-PCT (X)       00026496
02665                           RE-LF-LIM-LO (X)    RE-LF-LIM-HI (X)    00026497
02666                           RE-LF-LO (X)        RE-LF-HI (X)        00026498
02667                           RE-AHBEN-LIM-LO (X) RE-AHBEN-LIM-HI (X) 00026499
02668                           RE-AHBEN-LO (X)     RE-AHBEN-HI (X)     00026500
02669                           RE-AHMOA-LIM-LO (X) RE-AHMOA-LIM-HI (X) 00026501
02670                           RE-AHMOA-LO (X)     RE-AHMOA-HI (X).    00026502
02671                                                                   00026503
02672      IF X GREATER THAN +20                                        00026504
02673          GO TO 7075-CONTINUE-A-RECORD.                            00026505
02674                                                                   00026506
02675      IF RT-REI-COMP (X) = SPACES                                  00026507
02676          GO TO 7075-CONTINUE-A-RECORD                             00026508
02677      ELSE                                                         00026509
02678          MOVE RT-REI-COMP (X)      TO RE-REI-COMP (X)             00026510
02679          MOVE ZEROS                TO RE-REI-COMP-SUB (X).        00026511
02680                                                                   00026512
02681      MOVE RT-LF-QC (X)             TO RE-LF-QC (X).               00026513
02682      MOVE RT-AH-QC (X)             TO RE-AH-QC (X).               00026514
02683      MOVE RT-LO-DATE (X)           TO RE-LO-DATE (X).             00026515
02684      MOVE RT-HI-DATE (X)           TO RE-HI-DATE (X).             00026516
02685                                                                   00026850
02686      IF RT-LFAGE-LO (X) NUMERIC                                   00026851
02687          MOVE RT-LFAGE-LO (X)      TO RE-LFAGE-LO (X).            00026852
02688      IF RT-LFAGE-HI (X) NUMERIC                                   00026853
02689          MOVE RT-LFAGE-HI (X)      TO RE-LFAGE-HI (X).            00026854
02690      IF RT-AHAGE-LO (X) NUMERIC                                   00026855
02691          MOVE RT-AHAGE-LO (X)      TO RE-AHAGE-LO (X).            00026856
02692      IF RT-AHAGE-HI (X) NUMERIC                                   00026857
02693          MOVE RT-AHAGE-HI (X)      TO RE-AHAGE-HI (X).            00026858
02694      IF RT-LFTRM-LO (X) NUMERIC                                   00026859
02695          MOVE RT-LFTRM-LO (X)      TO RE-LFTRM-LO (X).            00026860
02696      IF RT-LFTRM-HI (X) NUMERIC                                   00026861
02697          MOVE RT-LFTRM-HI (X)      TO RE-LFTRM-HI (X).            00026862
02698      IF RT-AHTRM-LO (X) NUMERIC                                   00026863
02699          MOVE RT-AHTRM-LO (X)      TO RE-AHTRM-LO (X).            00026864
02700      IF RT-AHTRM-HI (X) NUMERIC                                   00026865
02701          MOVE RT-AHTRM-HI (X)      TO RE-AHTRM-HI (X).            00026866
02702      IF RT-LF-PCT (X) NUMERIC                                     00026867
02703          MOVE RT-LF-PCT (X)        TO RE-LF-PCT (X).              00026868
02704      IF RT-AH-PCT (X) NUMERIC                                     00026869
02705          MOVE RT-AH-PCT (X)        TO RE-AH-PCT (X).              00026870
02706      IF RT-LF-LIM-LO (X) NUMERIC                                  00026871
02707          MOVE RT-LF-LIM-LO (X)     TO RE-LF-LIM-LO (X).           00026872
02708      IF RT-LF-LIM-HI (X) NUMERIC                                  00026873
02709          MOVE RT-LF-LIM-HI (X)     TO RE-LF-LIM-HI (X).           00026874
02710      IF RT-LF-LIM-HI (X) = +9999999.99                            00026875
02711          MOVE +999999999.99        TO RE-LF-LIM-HI (X).           00026876
02712      IF RT-LF-LO (X) NUMERIC                                      00026877
02713          MOVE RT-LF-LO (X)         TO RE-LF-LO (X).               00026878
02714      IF RT-LF-HI (X) NUMERIC                                      00026879
02715          MOVE RT-LF-HI (X)         TO RE-LF-HI (X).               00026880
02716      IF RT-LF-HI (X) = +9999999.99                                00026881
02717          MOVE +999999999.99        TO RE-LF-HI (X).               00026882
02718      IF RT-AHBEN-LIM-LO (X) NUMERIC                               00026883
02719          MOVE RT-AHBEN-LIM-LO (X)  TO RE-AHBEN-LIM-LO (X).        00026884
02720      IF RT-AHBEN-LIM-HI (X) NUMERIC                               00026885
02721          MOVE RT-AHBEN-LIM-HI (X)  TO RE-AHBEN-LIM-HI (X).        00026886
02722      IF RT-AHBEN-LO (X) NUMERIC                                   00026887
02723          MOVE RT-AHBEN-LO (X)      TO RE-AHBEN-LO (X).            00026888
02724      IF RT-AHBEN-HI (X) NUMERIC                                   00026889
02725          MOVE RT-AHBEN-HI (X)      TO RE-AHBEN-HI (X).            00026890
02726      IF RT-AHMOA-LIM-LO (X) NUMERIC                               00026891
02727          MOVE RT-AHMOA-LIM-LO (X)  TO RE-AHMOA-LIM-LO (X).        00026892
02728      IF RT-AHMOA-LIM-HI (X) NUMERIC                               00026893
02729          MOVE RT-AHMOA-LIM-HI (X)  TO RE-AHMOA-LIM-HI (X).        00026894
02730      IF RT-AHMOA-LO (X) NUMERIC                                   00026895
02731          MOVE RT-AHMOA-LO (X)      TO RE-AHMOA-LO (X).            00026896
02732      IF RT-AHMOA-HI (X) NUMERIC                                   00026897
02733          MOVE RT-AHMOA-HI (X)      TO RE-AHMOA-HI (X).            00026898
02734                                                                   00026899
02735      MOVE RT-LF-BEN-CODE (X)       TO RE-LF-BEN-CODE (X).         00026900
02736      MOVE RT-AH-BEN-CODE (X)       TO RE-AH-BEN-CODE (X).         00026901
02737      MOVE RT-INTERACTIVE (X)       TO RE-INTERACTIVE (X).         00026902
02738      MOVE RT-REMAINING (X)         TO RE-REMAINING (X).           00026903
02739                                                                   00026904
02740  7075-CONTINUE-A-RECORD.                                          00026905
02741      IF X LESS +30                                                00026906
02742          ADD +1 TO X                                              00026907
02743          GO TO 7070-CONVERT-A-RECORD.                             00026908
02744                                                                   00026909
02745      MOVE RT-100-COMP            TO RE-100-COMP.                  00026910
02746      MOVE RT-COMP-INFO-END       TO RE-COMP-INFO-END.             00026911
02747      MOVE RT-NSP-ST-CD-LF        TO RE-NSP-ST-CD-LF.              00026912
02748      MOVE RT-NSP-ST-CD-AH        TO RE-NSP-ST-CD-AH.              00026913
02749      MOVE RT-TABLE-CARRIER-SECURITY                               00026914
02750                                  TO RE-TABLE-CARRIER-SECURITY.    00026915
02751                                                                   00026916
02752      GO TO 7090-WRITE-RTBL-RECORD.                                00026917
02753                                                                   00026918
02754  7080-CONVERT-B-RECORD.                                           00026919
02755      MOVE RT-DESC (X)            TO RE-DESC (X).                  00026920
02756                                                                   00026921
02757      IF X LESS +18                                                00026922
02758          ADD +1 TO X                                              00026923
02759          GO TO 7080-CONVERT-B-RECORD.                             00026924
02760                                                                   00026925
02761      IF RT-LF-FEE      NOT NUMERIC  MOVE ZEROS TO RT-LF-FEE.      00026926
02762      IF RT-AH-FEE      NOT NUMERIC  MOVE ZEROS TO RT-AH-FEE.      00026927
02763      IF RT-AH-PR-PCT   NOT NUMERIC  MOVE ZEROS TO RT-AH-PR-PCT.   00026928
02764      IF RT-AH-78-PCT   NOT NUMERIC  MOVE ZEROS TO RT-AH-78-PCT.   00026929
02765      IF RT-LF-IBNR-PCT NOT NUMERIC  MOVE ZEROS TO RT-LF-IBNR-PCT. 00026930
02766      IF RT-AH-IBNR-PCT NOT NUMERIC  MOVE ZEROS TO RT-AH-IBNR-PCT. 00026931
02767      IF RT-LF-CLM-PCT  NOT NUMERIC  MOVE ZEROS TO RT-LF-CLM-PCT.  00026932
02768      IF RT-AH-CLM-PCT  NOT NUMERIC  MOVE ZEROS TO RT-AH-CLM-PCT.  00026933
02769      IF RT-LF-CLM-MAX  NOT NUMERIC  MOVE ZEROS TO RT-LF-CLM-MAX.  00026934
02770      IF RT-AH-CLM-MAX  NOT NUMERIC  MOVE ZEROS TO RT-AH-CLM-MAX.  00026935
02771      IF RT-LF-PR-PCT   NOT NUMERIC  MOVE ZEROS TO RT-LF-PR-PCT.   00026936
02772      IF RT-LF-78-PCT   NOT NUMERIC  MOVE ZEROS TO RT-LF-78-PCT.   00026937
02773                                                                   00027730
02774      MOVE RT-NAME                TO RE-NAME.                      00027731
02775      MOVE RT-LF-PE               TO RE-LF-PE.                     00027732
02776      MOVE RT-AH-PE               TO RE-AH-PE.                     00027733
02777      MOVE RT-LF-FEE              TO RE-LF-FEE.                    00027734
02778      MOVE RT-AH-FEE              TO RE-AH-FEE.                    00027735
02779      MOVE RT-AH-PR-PCT           TO RE-AH-PR-PCT.                 00027736
02780      MOVE RT-AH-78-PCT           TO RE-AH-78-PCT.                 00027737
02781      MOVE RT-PRT-ST              TO RE-PRT-ST.                    00027738
02782      MOVE RT-PRT-OW              TO RE-PRT-OW.                    00027739
02783      MOVE RT-MORT-CODE           TO RE-MORT-CODE.                 00027740
02784      MOVE RT-CLAIM-CODE          TO RE-CLAIM-CODE.                00027741
02785      MOVE RT-ZERO-LF-FEE         TO RE-ZERO-LF-FEE.               00027742
02786      MOVE RT-ZERO-AH-FEE         TO RE-ZERO-AH-FEE.               00027743
02787      MOVE RT-CEDE-NAME           TO RE-CEDE-NAME.                 00027744
02788      MOVE RT-LF-COMM             TO RE-LF-COMM.                   00027745
02789      MOVE RT-AH-COMM             TO RE-AH-COMM.                   00027746
02790      MOVE RT-LF-TAX              TO RE-LF-TAX.                    00027747
02791      MOVE RT-AH-TAX              TO RE-AH-TAX.                    00027748
02792      MOVE RT-CLM-INCURRED-LIM    TO RE-CLM-INCURRED-LIM.          00027749
02793      MOVE RT-LF-IBNR-PCT         TO RE-LF-IBNR-PCT.               00027750
02794      MOVE RT-AH-IBNR-PCT         TO RE-AH-IBNR-PCT.               00027751
02795      MOVE RT-COMP-CARRIER-SECURITY TO RE-COMP-CARRIER-SECURITY.   00027752
02796      MOVE 'P'                    TO RE-LF-FEE-METHOD.             00027753
02797      MOVE 'Y'                    TO RE-MORT-SW.                   00027754
02798      MOVE SPACE                  TO RE-CEDING-TYPE-FLAG.          00027755
02799      MOVE '2'                    TO RE-LF-FEE-BASIS               00027756
02800                                     RE-AH-FEE-BASIS.              00027757
02801                                                                   00027758
02802      MOVE ZEROS TO RE-LF-FEE-RANGE-PCT (1) RE-LF-FEE-THRU-AMT (1) 00027759
02803                    RE-LF-FEE-RANGE-PCT (2) RE-LF-FEE-THRU-AMT (2) 00027760
02804                    RE-LF-FEE-RANGE-PCT (3) RE-LF-FEE-THRU-AMT (3) 00027761
02805                    RE-LF-FEE-RANGE-PCT (4) RE-LF-FEE-THRU-AMT (4) 00027762
02806                    RE-LF-FEE-RANGE-PCT (5) RE-LF-FEE-THRU-AMT (5) 00027763
02807                    RE-LF-FEE-RANGE-PCT (6) RE-LF-FEE-THRU-AMT (6).00027764
02808                                                                   00027765
02809      MOVE RE-LF-CEDING-FEE-BRACKETS TO RE-AH-CEDING-FEE-BRACKETS. 00027766
02810      MOVE RT-EARNING-START-DT    TO RE-EARNING-START-DT.          00027767
02811      MOVE RT-CEDING-STMT-OPTION  TO RE-OLD-CEDING-STMT.           00027768
02812                                                                   00027769
02813      IF RE-OLD-CEDING-STMT = ' ' OR 'Y' OR '6'                    00027770
02814          MOVE 'Y'                TO RE-CEDING-STMT-OPT-A          00027771
02815          MOVE 'Y'                TO RE-CEDING-STMT-OPT-B          00027772
02816          MOVE 'Y'                TO RE-CEDING-STMT-OPT-C.         00027773
02817                                                                   00027774
02818      IF RE-OLD-CEDING-STMT = '1'                                  00027775
02819          MOVE 'Y'                TO RE-CEDING-STMT-OPT-A          00027776
02820          MOVE 'N'                TO RE-CEDING-STMT-OPT-B          00027777
02821          MOVE 'N'                TO RE-CEDING-STMT-OPT-C.         00027778
02822                                                                   00028220
02823      IF RE-OLD-CEDING-STMT = '2'                                  00028221
02824          MOVE 'N'                TO RE-CEDING-STMT-OPT-A          00028222
02825          MOVE 'Y'                TO RE-CEDING-STMT-OPT-B          00028223
02826          MOVE 'N'                TO RE-CEDING-STMT-OPT-C.         00028224
02827                                                                   00028225
02828      IF RE-OLD-CEDING-STMT = '3'                                  00028226
02829          MOVE 'N'                TO RE-CEDING-STMT-OPT-A          00028227
02830          MOVE 'N'                TO RE-CEDING-STMT-OPT-B          00028228
02831          MOVE 'Y'                TO RE-CEDING-STMT-OPT-C.         00028229
02832                                                                   00028230
02833      IF RE-OLD-CEDING-STMT = '4'                                  00028231
02834          MOVE 'Y'                TO RE-CEDING-STMT-OPT-A          00028232
02835          MOVE 'N'                TO RE-CEDING-STMT-OPT-B          00028233
02836          MOVE 'Y'                TO RE-CEDING-STMT-OPT-C.         00028234
02837                                                                   00028235
02838      IF RE-OLD-CEDING-STMT = '5'                                  00028236
02839          MOVE 'N'                TO RE-CEDING-STMT-OPT-A          00028237
02840          MOVE 'Y'                TO RE-CEDING-STMT-OPT-B          00028238
02841          MOVE 'Y'                TO RE-CEDING-STMT-OPT-C.         00028239
02842                                                                   00028240
02843      IF RE-OLD-CEDING-STMT = '7'                                  00028241
02844          MOVE 'Y'                TO RE-CEDING-STMT-OPT-A          00028242
02845          MOVE 'Y'                TO RE-CEDING-STMT-OPT-B          00028243
02846          MOVE 'N'                TO RE-CEDING-STMT-OPT-C.         00028244
02847                                                                   00028245
02848      MOVE 'Y'                    TO RE-CEDING-STMT-OPT-D.         00028246
02849      MOVE 'Y'                    TO RE-CEDING-STMT-OPT-E.         00028247
02850      MOVE RT-LF-CLM-PCT          TO RE-LF-CLM-PCT.                00028248
02851      MOVE RT-AH-CLM-PCT          TO RE-AH-CLM-PCT.                00028249
02852      MOVE RT-LF-CLM-MAX          TO RE-LF-CLM-MAX.                00028250
02853      MOVE RT-AH-CLM-MAX          TO RE-AH-CLM-MAX.                00028251
02854      MOVE RT-LF-PR-PCT           TO RE-LF-PR-PCT.                 00028252
02855      MOVE RT-LF-78-PCT           TO RE-LF-78-PCT.                 00028253
02856                                                                   00028254
02857  7090-WRITE-RTBL-RECORD.                                          00028255
02858      ADD +1 TO RTBL-RCDS.                                         00028256
02859                                                                   00028257
02860      MOVE REINSURANCE-RECORD TO ECS-RTBL-RECORD.                  00028258
02861                                                                   00028259
02862      WRITE ECS-RTBL-RECORD.                                       00028260
02863                                                                   00028261
02864      IF ERVRTBL-FILE-STATUS NOT = '00'                            00028262
02865          MOVE ERVRTBL-FILE-STATUS      TO WS-ABEND-FILE-STATUS    00028263
02866          MOVE 'WRITE ERROR - ERVRTBL ' TO WS-ABEND-MESSAGE        00028264
02867          GO TO ABEND-PGM.                                         00028265
02868                                                                   00028266
02869      GO TO 7010-READ-RTBL-MASTER.                                 00028267
02870                                                                   00028268
02871  7099-CLOSE-RTBL-MASTER.                                          00028269
02872      DISPLAY ' '.                                                 00028270
02873      MOVE RTBL-RCDS              TO DISPLAY-RCDS.                 00028271
02874      DISPLAY 'REINSURANCE TABLE RCDS CONVERTED....' DISPLAY-RCDS. 00028272
02875                                                                   00028273
02876      CLOSE RTBL-IN  RTBL-OUT.                                     00028274
02877                                                                   00028275
02878      IF ERVRTBL-FILE-STATUS NOT = '00'                            00028276
02879          MOVE ERVRTBL-FILE-STATUS      TO WS-ABEND-FILE-STATUS    00028277
02880          MOVE 'CLOSE ERROR - ERVRTBL ' TO WS-ABEND-MESSAGE        00028278
02881          GO TO ABEND-PGM.                                         00028279
02882                                                                   00028280
02883      IF NOT CONVERTING-ALL-FILES                                  00028281
02884          GO TO 0200-READ-CONTROL-CARD.                            00028282
02885  EJECT                                                            00028283
02886  8000-RATE-CONVERSION-ROUTINE.                                    00028284
02887      OPEN INPUT RATE-IN                                           00028285
02888          OUTPUT RATE-OUT.                                         00028286
02889                                                                   00028287
02890      IF ERVRATE-FILE-STATUS NOT = '00'  AND  '97'                 00028288
02891          MOVE ERVRATE-FILE-STATUS      TO WS-ABEND-FILE-STATUS    00028289
02892          MOVE 'OPEN ERROR - ERVRATE ' TO WS-ABEND-MESSAGE         00028290
02893          GO TO ABEND-PGM.                                         00028291
02894                                                                   00028292
02895  8010-READ-RATE-MASTER.                                           00028293
02896      READ RATE-IN INTO RATE-RCD-AREA                              00028294
02897                  AT END GO TO 8099-CLOSE-RATE-MASTER.             00028295
02898                                                                   00028296
02899  8050-CONVERT-RATE-MASTER.                                        00028297
02900      MOVE SPACES                 TO RATE-RECORD.                  00028298
02901      MOVE 'RT'                   TO RT-RECORD-ID.                 00028299
02902      MOVE DTE-CLASIC-COMPANY-CD  TO RT-COMPANY-CD.                00028300
02903                                                                   00028301
02904      MOVE XR-STATE-CODE          TO RT-STATE-CODE.                00028302
02905                                                                   00028303
02906      IF XR-L-AH = 'L'                                             00028304
02907          MOVE LIFE-OVERRIDE-L1   TO RT-L-AH                       00028305
02908      ELSE                                                         00028306
02909          MOVE AH-OVERRIDE-L1     TO RT-L-AH.                      00028307
02910                                                                   00028308
02911      MOVE XR-LAH-NUM             TO RT-LAH-NUM.                   00028309
02912      MOVE XR-LIMITS              TO RT-LIMITS.                    00028310
02913      MOVE XR-EXPIRY-DATE         TO RT-EXPIRY-DATE.               00028311
02914                                                                   00028312
02915      MOVE '99'                   TO RT-FUTURE.                    00028313
02916      MOVE 999999                 TO RT-HIGH-AMT.                  00028314
02917      MOVE '9'                    TO RT-SEX.                       00028315
02918                                                                   00028316
02919      MOVE WS-BIN-MAINT-DT        TO RT-LAST-MAINT-DT.             00028317
02920      MOVE ZEROS                  TO RT-LAST-MAINT-HHMMSS.         00028318
02921      MOVE 'LVL6'                 TO RT-LAST-MAINT-USER.           00028319
02922                                                                   00028320
02923      MOVE XR-STRUCT-COMNT        TO RT-STRUCTURE-COMMENT.         00028321
02924      MOVE XR-RATE-COMNT          TO RT-RATE-COMMENT.              00028322
02925      MOVE XR-MAX-AGE             TO RT-MAX-AGE.                   00028323
02926                                                                   00028324
02927      MOVE XR-LIFE-LIMS-FLDS      TO RT-LIFE-LIMS-FLDS.            00028325
02928                                                                   00028326
02929      MOVE +1                     TO X.                            00028327
02930                                                                   00029300
02931  8070-MOVE-THE-RATES.                                             00029301
02932      IF X GREATER THAN +240                                       00029302
02933          MOVE ZEROS              TO RT-L-RATE (X)                 00029303
02934      ELSE                                                         00029304
02935          IF XR-L-RATE (X) NUMERIC                                 00029305
02936              MOVE XR-L-RATE (X)  TO RT-L-RATE (X)                 00029306
02937          ELSE                                                     00029307
02938              MOVE ZEROS          TO RT-L-RATE (X).                00029308
02939                                                                   00029309
02940      IF X LESS THAN +360                                          00029310
02941          ADD +1 TO X                                              00029311
02942          GO TO 8070-MOVE-THE-RATES.                               00029312
02943                                                                   00029313
02944      MOVE XR-SRT-ALPHA           TO RT-SRT-ALPHA.                 00029314
02945      MOVE XR-CONTROL-2           TO RT-CONTROL-2.                 00029315
02946                                                                   00029316
02947  8090-WRITE-RATE-RECORD.                                          00029317
02948      ADD +1 TO RATE-RCDS.                                         00029318
02949                                                                   00029319
02950      MOVE RATE-RECORD                  TO ECS-RATE-RECORD.        00029320
02951                                                                   00029321
02952      WRITE ECS-RATE-RECORD.                                       00029322
02953                                                                   00029323
02954      IF ERVRATE-FILE-STATUS NOT = '00'                            00029324
02955          MOVE ERVRATE-FILE-STATUS      TO WS-ABEND-FILE-STATUS    00029325
02956          MOVE 'WRITE ERROR - ERVRATE ' TO WS-ABEND-MESSAGE        00029326
02957          GO TO ABEND-PGM.                                         00029327
02958                                                                   00029328
02959      GO TO 8010-READ-RATE-MASTER.                                 00029329
02960                                                                   00029330
02961  8099-CLOSE-RATE-MASTER.                                          00029331
02962      DISPLAY ' '.                                                 00029332
02963      MOVE RATE-RCDS              TO DISPLAY-RCDS.                 00029333
02964      DISPLAY 'RATE FILE RECORDS CONVERTED.........' DISPLAY-RCDS. 00029334
02965                                                                   00029335
02966      CLOSE RATE-IN  RATE-OUT.                                     00029336
02967                                                                   00029337
02968      IF NOT CONVERTING-ALL-FILES                                  00029338
02969          GO TO 0200-READ-CONTROL-CARD.                            00029339
02970  EJECT                                                            00029340
02971  9000-ALPH-CONVERSION-ROUTINE.                                    00029341
02972      OPEN INPUT ALPH-IN                                           00029342
02973          OUTPUT ALPH-OUT.                                         00029343
02974                                                                   00029344
02975  9010-READ-ALPH-MASTER.                                           00029345
02976      READ ALPH-IN INTO ALPH-EXT-REC                               00029346
02977                  AT END GO TO 9099-CLOSE-ALPH-MASTER.             00029347
02978                                                                   00029348
02979  9050-CONVERT-ALPH-MASTER.                                        00029349
02980      MOVE SPACES                 TO ALPHA-RECORD.                 00029350
02981                                                                   00029351
02982      MOVE ZEROS             TO AX-LF-TYP          AX-AH-TYP       00029352
02983                                AX-LF-TERM         AX-AH-TERM      00029353
02984                                AX-LF-REMTERM      AX-AH-REMTERM   00029354
02985                                AX-LF-AMT          AX-AH-AMT       00029355
02986                                AX-LF-REMAMT       AX-AH-REMAMT    00029356
02987                                AX-LF-PRM          AX-AH-PRM       00029357
02988                                AX-LF-REFUND       AX-AH-REFUND    00029358
02989                                AX-LF-CLAIM-PMTS   AX-AH-CLAIM-PMTS00029359
02990                                AX-LF-AMT-ALT                      00029360
02991                                AX-LF-REMAMT-ALT                   00029361
02992                                AX-LF-PRM-ALT.                     00029362
02993                                                                   00029363
02994                                                                   00029364
02995      MOVE 'AX'                   TO AX-RECORD-ID.                 00029365
02996      MOVE DTE-CLASIC-COMPANY-CD  TO AX-COMPANY-CD.                00029366
02997                                                                   00029367
02998      MOVE AER-CARR               TO AX-CARRIER.                   00029368
02999      MOVE AER-CO                 TO AX-GRP-PRIME.                 00029369
03000      MOVE AER-ST                 TO AX-STATE.                     00029370
03001      MOVE AER-ACCT               TO AX-ACCT-PRIME.                00029371
03002      MOVE AER-DT                 TO AX-DT.                        00029372
03003      MOVE AER-CERT-NO            TO AX-CERT-PRIME.                00029373
03004      MOVE AER-CERT-SUF           TO AX-CERT-SUFFIX.               00029374
03005      MOVE ZEROS                  TO AX-GRP-PREFIX                 00029375
03006                                     AX-ACCT-PREFIX                00029376
03007                                     AX-CERT-PREFIX.               00029377
03008                                                                   00029378
03009      MOVE AER-LNAME              TO AX-LNAME.                     00029379
03010      MOVE AER-INIT-F             TO AX-1ST-INIT-FNAME.            00029380
03011      MOVE AER-INIT-M             TO AX-INIT.                      00029381
03012      MOVE AER-AGE                TO AX-AGE.                       00029382
03013      MOVE AER-SEX                TO AX-SEX.                       00029383
03014                                                                   00029384
03015      IF AER-LF-TYP NOT = ZEROS  AND  NOT = SPACES                 00029385
03016          MOVE AER-LF-TYP         TO AX-LF-TYP                     00029386
03017          MOVE AER-TRM            TO AX-LF-TERM                    00029387
03018          MOVE AER-R-TRM          TO AX-LF-REMTERM                 00029388
03019          MOVE AER-LF-AMT         TO AX-LF-AMT                     00029389
03020          MOVE AER-LF-REM         TO AX-LF-REMAMT                  00029390
03021          MOVE AER-LF-PRM         TO AX-LF-PRM.                    00029391
03022                                                                   00029392
03023      IF AER-LF-TYP NOT = ZEROS  AND  NOT = SPACES                 00029393
03024          MOVE AER-AH-TYP         TO AX-AH-TYP                     00029394
03025          MOVE AER-TRM            TO AX-AH-TERM                    00029395
03026          MOVE AER-R-TRM          TO AX-AH-REMTERM                 00029396
03027          MOVE AER-AH-AMT         TO AX-AH-AMT                     00029397
03028          MOVE AER-AH-REM         TO AX-AH-REMAMT                  00029398
03029          MOVE AER-AH-PRM         TO AX-AH-PRM.                    00029399
03030                                                                   00029400
03031      MOVE AER-APR                TO AX-APR.                       00029401
03032      MOVE AER-IND-GRP            TO AX-IND-GRP.                   00029402
03033      MOVE AER-PMT-FREQ           TO AX-PMT-FREQ.                  00029403
03034      MOVE AER-SPEC-REIN          TO AX-SPEC-REIN.                 00029404
03035      MOVE AER-MEM-NO             TO AX-MEM-NO.                    00029405
03036      MOVE AER-SOC-NO             TO AX-SOC-NO.                    00029406
03037                                                                   00029407
03038      MOVE AER-E-YR               TO AX-E-YR.                      00029408
03039      MOVE AER-E-MO               TO AX-E-MO.                      00029409
03040      MOVE AER-E-DA               TO AX-E-DA.                      00029410
03041                                                                   00029411
03042      MOVE AER-STATUS             TO AX-LF-STATUS  AX-AH-STATUS.   00029412
03043      MOVE AER-PRE-PLST           TO AX-AH-PRE-PLST.               00029413
03044                                                                   00029414
03045      MOVE ZEROS TO               AX-LF-CNCL     AX-AH-CNCL        00029415
03046                                  AX-DEATH       AX-LUMP-SUM       00029416
03047                                  AX-LF-EXIT     AX-AH-EXIT        00029417
03048                                  AX-LF-EXPIRES  AX-AH-EXPIRES.    00029418
03049                                                                   00029419
03050      IF AER-STATUS = '6'  OR  '7'  OR  '8'                        00029420
03051          MOVE AER-X-YR           TO AX-LX-YR    AX-AX-YR          00029421
03052          MOVE AER-X-MO           TO AX-LX-MO    AX-AX-MO          00029422
03053          MOVE AER-X-DA           TO AX-LX-DA    AX-AX-DA.         00029423
03054                                                                   00029424
03055      IF AER-STATUS = '6'                                          00029425
03056          MOVE '8'                TO AX-LF-STATUS                  00029426
03057          MOVE AER-D-YR           TO AX-LF-C-YR  AX-LS-YR          00029427
03058          MOVE AER-D-MO           TO AX-LF-C-MO  AX-LS-MO          00029428
03059          MOVE AER-D-DA           TO AX-LF-C-DA  AX-LS-DA          00029429
03060          MOVE ZEROS              TO AX-DEATH    AX-AH-CNCL        00029430
03061          GO TO 9090-WRITE-ALPH-RECORD.                            00029431
03062                                                                   00029432
03063      IF AER-STATUS = '7'                                          00029433
03064          MOVE '8'                TO AX-AH-STATUS                  00029434
03065          MOVE AER-D-YR           TO AX-D-YR     AX-AH-C-YR        00029435
03066          MOVE AER-D-MO           TO AX-D-MO     AX-AH-C-MO        00029436
03067          MOVE AER-D-DA           TO AX-D-DA     AX-AH-C-DA        00029437
03068          MOVE ZEROS              TO AX-LF-CNCL  AX-LUMP-SUM       00029438
03069          GO TO 9090-WRITE-ALPH-RECORD.                            00029439
03070                                                                   00029440
03071      IF AER-STATUS = '8'                                          00029441
03072          MOVE AER-C-YR           TO AX-LF-C-YR  AX-AH-C-YR        00029442
03073          MOVE AER-C-MO           TO AX-LF-C-MO  AX-AH-C-MO        00029443
03074          MOVE AER-C-DA           TO AX-LF-C-DA  AX-AH-C-DA        00029444
03075          MOVE ZEROS              TO AX-DEATH    AX-LUMP-SUM       00029445
03076          GO TO 9090-WRITE-ALPH-RECORD.                            00029446
03077                                                                   00029447
03078      IF AER-PRE-PLST = '6'                                        00029448
03079          MOVE AER-D-YR           TO AX-LS-YR                      00029449
03080          MOVE AER-D-MO           TO AX-LS-MO                      00029450
03081          MOVE AER-D-DA           TO AX-LS-DA.                     00029451
03082                                                                   00029452
03083  9090-WRITE-ALPH-RECORD.                                          00029453
03084      ADD +1 TO ALPH-RCDS.                                         00029454
03085                                                                   00029455
03086      WRITE ECS-ALPH-RECORD FROM ALPHA-RECORD.                     00029456
03087                                                                   00029457
03088      GO TO 9010-READ-ALPH-MASTER.                                 00029458
03089                                                                   00029459
03090  9099-CLOSE-ALPH-MASTER.                                          00029460
03091      DISPLAY ' '.                                                 00029461
03092      MOVE ALPH-RCDS              TO DISPLAY-RCDS.                 00029462
03093      DISPLAY 'ALPHA EXTRACT RECORDS CONVERTED.....' DISPLAY-RCDS. 00029463
03094                                                                   00029464
03095      CLOSE ALPH-IN  ALPH-OUT.                                     00029465
03096                                                                   00029466
03097      IF NOT CONVERTING-ALL-FILES                                  00029467
03098          GO TO 0200-READ-CONTROL-CARD.                            00029468
03099  EJECT                                                            00029469
03100  10000-GAAP-CONVERSION-ROUTINE.                                   00029470
03101      OPEN INPUT GAAP-IN                                           00029471
03102          OUTPUT GAAP-OUT.                                         00029472
03103                                                                   00029473
03104  10010-READ-GAAP-MASTER.                                          00029474
03105      READ GAAP-IN INTO GAAP-EXT-REC                               00029475
03106                  AT END GO TO 10099-CLOSE-GAAP-MASTER.            00029476
03107                                                                   00029477
03108  10050-CONVERT-GAAP-MASTER.                                       00029478
03109      MOVE SPACES                 TO GAAP-RECORD.                  00029479
03110                                                                   00029480
03111      MOVE ZEROS                  TO GR-LFTYP       GR-AHTYP       00029481
03112                                     GR-LF-TERM     GR-AH-TERM     00029482
03113                                     GR-LF-REMTERM  GR-AH-REMTERM. 00029483
03114                                                                   00029484
03115      MOVE 'GR'                   TO GR-RECORD-ID.                 00029485
03116      MOVE DTE-CLASIC-COMPANY-CD  TO GR-COMPANY-CD.                00029486
03117                                                                   00029487
03118      MOVE GER-TYPE               TO GR-REIN.                      00029488
03119                                                                   00029489
03120      MOVE GER-CO                 TO GR-CARRIER.                   00029490
03121      MOVE GER-SUBCO              TO GR-GROUP-PRIME.               00029491
03122      MOVE GER-ST                 TO GR-STATE.                     00029492
03123      MOVE GER-ACCT               TO GR-ACCT-PRIME.                00029493
03124      MOVE GER-EFF                TO GR-EFF.                       00029494
03125      MOVE GER-CERT               TO GR-CERT-PRIME.                00029495
03126      MOVE GER-CERT-SFX           TO GR-CERT-SUFFIX.               00029496
03127      MOVE ZEROS                  TO GR-GROUP-PREFIX               00029497
03128                                     GR-ACCT-PREFIX                00029498
03129                                     GR-CERT-PREFIX.               00029499
03130                                                                   00029500
03131      IF GR-REIN = 'R'                                             00029501
03132          MOVE GER-REIN-COMP      TO GR-REINCO                     00029502
03133          MOVE ZEROS              TO GR-REINCO-SUB.                00029503
03134                                                                   00029504
03135      MOVE GER-IG                 TO GR-IG.                        00029505
03136      MOVE GER-APR                TO GR-APR.                       00029506
03137      MOVE GER-PMT-FREQ           TO GR-PMT-FREQ.                  00029507
03138      MOVE GER-RATE-TERM          TO GR-LOAN-TERM.                 00029508
03139      MOVE GER-AGE                TO GR-AGE.                       00029509
03140      MOVE GER-ACC-EXPIRES        TO GR-ACC-EXPIRES.               00029510
03141                                                                   00029511
03142      IF GER-LFTYP NOT = ZEROS                                     00029512
03143          MOVE GER-LFTYP          TO GR-LFTYP                      00029513
03144          MOVE GER-TRM            TO GR-LF-TERM                    00029514
03145          MOVE GER-REM            TO GR-LF-REMTERM.                00029515
03146                                                                   00029516
03147      MOVE GER-LFBEN              TO GR-LFBEN.                     00029517
03148      MOVE GER-LFPRM              TO GR-LFPRM.                     00029518
03149      MOVE GER-LFCOM              TO GR-LFCOM.                     00029519
03150      MOVE ZEROS                  TO GR-LFEXP.                     00029520
03151      MOVE GER-LFTAX              TO GR-LFTAX.                     00029521
03152                                                                   00029522
03153      IF GER-AHTYP NOT = ZEROS                                     00029523
03154          MOVE GER-AHTYP          TO GR-AHTYP                      00029524
03155          MOVE GER-TRM            TO GR-AH-TERM                    00029525
03156          MOVE GER-REM            TO GR-AH-REMTERM.                00029526
03157                                                                   00029527
03158      MOVE GER-AHBEN              TO GR-AHBEN.                     00029528
03159      MOVE GER-AHPRM              TO GR-AHPRM.                     00029529
03160      MOVE GER-AHCOM              TO GR-AHCOM.                     00029530
03161      MOVE ZEROS                  TO GR-AHEXP.                     00029531
03162      MOVE GER-AHTAX              TO GR-AHTAX.                     00029532
03163                                                                   00029533
03164      MOVE GXP-LFPRM              TO GRP-LFPRM.                    00029534
03165      MOVE GXP-LFCOM              TO GRP-LFCOM.                    00029535
03166      MOVE ZEROS                  TO GRP-LFEXP.                    00029536
03167      MOVE GXP-LFTAX              TO GRP-LFTAX.                    00029537
03168      MOVE GXP-AHPRM              TO GRP-AHPRM.                    00029538
03169      MOVE GXP-AHCOM              TO GRP-AHCOM.                    00029539
03170      MOVE ZEROS                  TO GRP-AHEXP.                    00029540
03171      MOVE GXP-AHTAX              TO GRP-AHTAX.                    00029541
03172                                                                   00029542
03173      MOVE GXR-LFPRM              TO GRR-LFPRM.                    00029543
03174      MOVE GXR-LFCOM              TO GRR-LFCOM.                    00029544
03175      MOVE ZEROS                  TO GRR-LFEXP.                    00029545
03176      MOVE GXR-LFTAX              TO GRR-LFTAX.                    00029546
03177      MOVE GXR-AHPRM              TO GRR-AHPRM.                    00029547
03178      MOVE GXR-AHCOM              TO GRR-AHCOM.                    00029548
03179      MOVE ZEROS                  TO GRR-AHEXP.                    00029549
03180      MOVE GXR-AHTAX              TO GRR-AHTAX.                    00029550
03181                                                                   00029551
03182      MOVE GXD-LFPRM              TO GRD-LFPRM.                    00029552
03183      MOVE GXD-LFCOM              TO GRD-LFCOM.                    00029553
03184      MOVE GXD-AHPRM              TO GRD-AHPRM.                    00029554
03185      MOVE GXD-AHCOM              TO GRD-AHCOM.                    00029555
03186                                                                   00029556
03187      MOVE GXS-LFPRM              TO GRS-LFPRM.                    00029557
03188      MOVE GXS-LFCOM              TO GRS-LFCOM.                    00029558
03189      MOVE GXS-AHPRM              TO GRS-AHPRM.                    00029559
03190      MOVE GXS-AHCOM              TO GRS-AHCOM.                    00029560
03191                                                                   00029561
03192      MOVE GER-MORT-CTL           TO GR-MORT-CONTROL.              00029562
03193      MOVE GER-REM-AMT            TO GR-REM-AMT.                   00029563
03194      MOVE GER-MO-DEC             TO GR-MO-DEC.                    00029564
03195      MOVE GER-MORT-FACT          TO GR-MORT-FACT.                 00029565
03196      MOVE GER-RESV               TO GR-RESV.                      00029566
03197      MOVE GER-FLAG               TO GR-FLAG.                      00029567
03198      MOVE GER-ALT-MORT-CODE      TO GR-ALT-MORT-CODE.             00029568
03199      MOVE GER-ALT-RESV           TO GR-ALT-RESV.                  00029569
03200                                                                   00029570
03201      MOVE GER-CNT                TO GR-CNT.                       00029571
03202      MOVE GER-CNT-LF             TO GR-CNT-LF.                    00029572
03203      MOVE GER-CNT-AH             TO GR-CNT-AH.                    00029573
03204      MOVE GER-UP-REM             TO GR-LF-UP-REMTERM              00029574
03205                                     GR-AH-UP-REMTERM.             00029575
03206      MOVE GER-AH-REM-BEN         TO GR-AH-REM-BEN.                00029576
03207                                                                   00029577
03208      MOVE GER-ENT-DT             TO GR-ENT-DT.                    00029578
03209                                                                   00029579
03210      MOVE GER-SUMMARY-FLAG       TO GR-SUMMARY-FLAG.              00029580
03211                                                                   00029581
03212  10090-WRITE-GAAP-RECORD.                                         00029582
03213      ADD +1 TO GAAP-RCDS.                                         00029583
03214                                                                   00029584
03215      WRITE ECS-GAAP-RECORD FROM GAAP-RECORD.                      00029585
03216                                                                   00029586
03217      GO TO 10010-READ-GAAP-MASTER.                                00029587
03218                                                                   00029588
03219  10099-CLOSE-GAAP-MASTER.                                         00029589
03220      DISPLAY ' '.                                                 00029590
03221      MOVE GAAP-RCDS              TO DISPLAY-RCDS.                 00029591
03222      DISPLAY 'G.A.A.P. EXTRACT RECORDS CONVERTED..' DISPLAY-RCDS. 00029592
03223                                                                   00029593
03224      CLOSE GAAP-IN  GAAP-OUT.                                     00029594
03225                                                                   00029595
03226      IF CONVERTING-ALL-FILES                                      00029596
03227          GO TO END-OF-JOB.                                        00029597
03228                                                                   00029598
03229      GO TO 0200-READ-CONTROL-CARD.                                00029599
03230  EJECT                                                            00029600
03231  ABEND-PGM.                                                       00029601
03232                              COPY ELCABEND SUPPRESS.              00029602
03233                                                                   00029603
03234  DATE-CONVERSION-ROUTINE.                                         00029604
03235      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   00029605
03236                                                                   00029606
03237  CONVERT-EXIT.                                                    00029607
03238      EXIT.                                                        00029608
03239                                                                   00029609
03240  END-OF-JOB.                                                      00029610
03241      CLOSE CONTROL-CARD.                                          00029611
03242                                                                   00029612
03243      GOBACK.                                                      00029613
