00001  ID DIVISION.                                                     00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 EL113.                               00000030
00004 *                            VMOD=2.002.                          00000031
00005                                                                   00000050
00006  AUTHOR.     LOGIC,INC.                                           00000060
00007              DALLAS, TEXAS.                                       00000070
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
00021  REMARKS.    TRANSACTION - EX38 - ACCOUNT MASTER DISPLAY.         00000210
00022                                                                   00000220
00023      EJECT                                                        00000230
00024  ENVIRONMENT DIVISION.                                            00000240
00025  DATA DIVISION.                                                   00000250
00026  WORKING-STORAGE SECTION.                                         00000260
00027  77  FILLER  PIC X(32)  VALUE '*** PAN EL113     PANLVL 006 ***'. 00000261
00028  77  FILLER  PIC X(32)  VALUE '********************************'. 00000262
00029  77  FILLER  PIC X(32)  VALUE '*    EL113 WORKING STORAGE     *'. 00000263
00030  77  FILLER  PIC X(32)  VALUE '********** V/M 2.002 ***********'. 00000264
00031                                                                   00000265
00032  01  WS-DATE-AREA.                                                00000266
00033      05  SAVE-DATE           PIC X(8)    VALUE SPACES.            00000267
00034      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            00000268
00035                                                                   00000269
00036  01  STANDARD-AREAS.                                              00000270
00037      12  GETMAIN-SPACE       PIC X       VALUE SPACE.             00000271
00038      12  MAP-NAME            PIC X(8)    VALUE 'EL113A'.          00000272
00039      12  MAPSET-NAME         PIC X(8)    VALUE 'EL113S'.          00000273
00040      12  TRANS-ID            PIC X(4)    VALUE 'EX38'.            00000274
00041      12  PGM-NAME            PIC X(8).                            00000275
00042      12  TIME-IN             PIC S9(7).                           00000276
00043      12  TIME-OUT-R  REDEFINES TIME-IN.                           00000277
00044          16  FILLER          PIC X.                               00000278
00045          16  TIME-OUT        PIC 99V99.                           00000279
00046          16  FILLER          PIC X(2).                            00000280
00047      12  XCTL-005            PIC X(5)    VALUE 'EL005'.           00000281
00048      12  XCTL-010            PIC X(5)    VALUE 'EL010'.           00000282
00049      12  XCTL-126            PIC X(5)    VALUE 'EL126'.           00000283
00050      12  LINK-001            PIC X(5)    VALUE 'EL001'.           00000284
00051      12  LINK-004            PIC X(5)    VALUE 'EL004'.           00000285
00052      12  LINK-CLDATCV        PIC X(7)    VALUE 'ELDATCV'.         00000286
00053      12  PGM-EL113           PIC X(8)    VALUE 'EL113'.           00000287
00054      12  ACCT-ID             PIC X(8)    VALUE 'ERACCT'.          00000288
00055      12  ACCT-VG-ID          PIC X(8)    VALUE 'ERACCT2'.         00000289
00056      12  SAVE-ACCT-PARTIAL-KEY   PIC X(20)  VALUE LOW-VALUES.     00000290
00057      12  WS-ZIP.                                                  00000291
00058          16  WS-ZIP-CODE         PIC X(5).                        00000292
00059          16  WS-DASH             PIC X(1)   VALUE '-'.            00000293
00060          16  WS-ZIP-PLUS4        PIC X(4).                        00000294
00061      12  WS-CANADIAN-POSTAL-CODES REDEFINES WS-ZIP.               00000295
00062          16  WS-CAN-POSTAL-CD-1  PIC X(3).                        00000296
00063          16  WS-DASH-CAN         PIC X(1).                        00000297
00064          16  WS-CAN-POSTAL-CD-2  PIC X(3).                        00000298
00065          16  WS-CAN-FILLER       PIC X(3).                        00000299
00066      12  WS-WORK-PHONE           PIC X(10)  VALUE ZEROS.          00000300
00067      12  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE                 00000301
00068                                  PIC 9(10).                       00000302
00069      EJECT                                                        00000303
00070                                                                   00000304
00071  01  WORK-AREAS.                                                  00000305
00072      12  BROWSE-STARTED-SW       PIC X       VALUE ' '.           00000306
00073          88  BROWSE-STARTED      VALUE 'Y'.                       00000307
00074      12  FIRST-TIME-SW           PIC X       VALUE ' '.           00000308
00075          88  FIRST-TIME          VALUE 'Y'.                       00000309
00076      12  DEEDIT-FIELD            PIC X(15).                       00000310
00077      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).     00000311
00078      12  DEEDIT-FIELD-V1 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.  00000312
00079      12  EXPDTE-SAVE             PIC XX.                          00000313
00080      12  EFFDTE-SAVE             PIC XX.                          00000314
00081      12  ALLOW-SAVE              PIC S9(3)V99  VALUE ZEROS.       00000315
00082      12  E023                    PIC X(4)    VALUE '0023'.        00000316
00083      12  E042                    PIC X(4)    VALUE '0042'.        00000317
00084      12  E052                    PIC X(4)    VALUE '0052'.        00000318
00085      12  E066                    PIC X(4)    VALUE '0066'.        00000319
00086      12  E067                    PIC X(4)    VALUE '0067'.        00000320
00087      12  E144                    PIC X(4)    VALUE '0144'.        00000321
00088      12  E168                    PIC X(4)    VALUE '0168'.        00000322
00089      12  E193                    PIC X(4)    VALUE '0193'.        00000323
00090      12  E195                    PIC X(4)    VALUE '0195'.        00000324
00091      12  E226                    PIC X(4)    VALUE '0226'.        00000325
00092      12  E348                    PIC X(4)    VALUE '0348'.        00000326
00093      12  E453                    PIC X(4)    VALUE '0453'.        00000327
00094      12  E454                    PIC X(4)    VALUE '0454'.        00000328
00095      12  E455                    PIC X(4)    VALUE '0455'.        00000329
00096      12  E456                    PIC X(4)    VALUE '0456'.        00000330
00097      12  E457                    PIC X(4)    VALUE '0457'.        00000331
00098      12  E470                    PIC X(4)    VALUE '0470'.        00000332
00099      12  E471                    PIC X(4)    VALUE '0471'.        00000333
00100      12  E472                    PIC X(4)    VALUE '0472'.        00000334
00101      12  E482                    PIC X(4)    VALUE '0482'.        00000335
00102                                                                   00000336
00103      EJECT                                                        00000337
00104                              COPY ELCDATE.                        00000338
00105      EJECT                                                        00000339
00106                              COPY ELCLOGOF.                       00000340
00107      EJECT                                                        00000341
00108                              COPY ELCATTR.                        00000342
00109      EJECT                                                        00001090
00110                                    COPY ELCEMIB.                  00001091
00111      EJECT                                                        00001092
00112                              COPY ELCJPFX.                        00001093
00113                                   PIC X(2000).                    00001094
00114      EJECT                                                        00001095
00115                              COPY ELCINTF.                        00001096
00116      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                00001097
00117          16  PI-PREV-ACCOUNT     PIC X(26).                       00001098
00118          16  PI-PREV-VG-ACCOUNT  PIC X(26).                       00001099
00119                                                                   00001100
00120          16  ACCT-KEY.                                            00001101
00121            18  ACCT-PARTIAL-KEY.                                  00001102
00122              20  ACCT-CO             PIC X.                       00001103
00123              20  ACCT-CARRIER        PIC X.                       00001104
00124              20  ACCT-GROUPING       PIC X(6).                    00001105
00125              20  ACCT-STATE          PIC XX.                      00001106
00126              20  ACCT-ACCOUNT        PIC X(10).                   00001107
00127            18  ACCT-EXP-DT           PIC XX.                      00001108
00128            18  ACCT-FILLER           PIC X(04).                   00001109
00129      EJECT                                                        00001290
00130                              COPY ELCAID.                         00001291
00131  01  FILLER    REDEFINES DFHAID.                                  00001292
00132      12  FILLER              PIC X(8).                            00001293
00133      12  PF-VALUES           PIC X       OCCURS 2.                00001294
00134      EJECT                                                        00001340
00135                              COPY EL113S.                         00001341
00136 *01  MAP-REDEF REDEFINES EL113AI.                                 00001342
00137 *    12  FILLER                  PIC X(70).                       00001343
00138 *    12  MAP-RECORD-AREA         PIC X(214).                      00001344
00139      EJECT                                                        00001345
00140  LINKAGE SECTION.                                                 00001346
00141  01  DFHCOMMAREA             PIC X(1024).                         00001347
00142                                                                   00001348
00143  01  PARMLIST.                                                    00001349
00144      02  FILLER              PIC S9(8)   COMP.                    00001350
00145      02  ACCT-POINTER        PIC S9(8)   COMP.                    00001351
00146      EJECT                                                        00001352
00147                              COPY ERCACCT.                        00001353
00148      EJECT                                                        00001354
00149  PROCEDURE DIVISION.                                              00001355
00150      SERVICE RELOAD PARMLIST.                                     00001356
00151      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 00001357
00152      MOVE 3  TO EMI-NUMBER-OF-LINES.                              00001358
00153      IF EIBCALEN = 0                                              00001359
00154          GO TO 8800-UNAUTHORIZED-ACCESS.                          00001360
00155                                                                   00001361
00156      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               00001362
00157      MOVE '5'                   TO DC-OPTION-CODE.                00001363
00158      PERFORM 9700-DATE-LINK.                                      00001364
00159      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    00001365
00160      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                00001366
00161                                                                   00001367
00162                                                                   00001368
00163      IF PI-CALLING-PROGRAM NOT = PGM-EL113                        00001369
00164          IF PI-RETURN-TO-PROGRAM NOT = PGM-EL113                  00001370
00165              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      00001371
00166              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      00001372
00167              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      00001373
00168              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      00001374
00169              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      00001375
00170              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      00001376
00171              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    00001377
00172              MOVE PGM-EL113 TO PI-CALLING-PROGRAM                 00001378
00173              MOVE LOW-VALUES TO EL113AO                           00001379
00174                                 PI-PROGRAM-WORK-AREA              00001380
00175              GO TO 8100-SEND-INITIAL-MAP.                         00001381
00176                                                                   00001382
00177                                                                   00001770
00178      EXEC CICS HANDLE CONDITION                                   00001771
00179          PGMIDERR(9600-PGMID-ERROR)                               00001772
00180          ERROR   (9990-ABEND)                                     00001773
00181      END-EXEC.                                                    00001774
00182                                                                   00001775
00183      IF EIBAID = DFHCLEAR                                         00001776
00184          GO TO 9400-CLEAR.                                        00001777
00185                                                                   00001778
00186      EJECT                                                        00001779
00187  0200-RECEIVE.                                                    00001780
00188      MOVE LOW-VALUES             TO EL113AI.                      00001781
00189      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       00001782
00190          MOVE 0008               TO EMI-ERROR                     00001783
00191          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 00001784
00192          MOVE -1                 TO AMAINTL                       00001785
00193          GO TO 8200-SEND-DATAONLY.                                00001786
00194                                                                   00001787
00195      EXEC CICS RECEIVE                                            00001788
00196          MAP   (MAP-NAME)                                         00001789
00197          MAPSET(MAPSET-NAME)                                      00001790
00198          INTO  (EL113AI)                                          00001791
00199      END-EXEC.                                                    00001792
00200                                                                   00001793
00201      IF APFKL = 0                                                 00001794
00202          GO TO 0300-CHECK-PFKEYS.                                 00001795
00203                                                                   00001796
00204      IF EIBAID NOT = DFHENTER                                     00001797
00205          MOVE 0004               TO EMI-ERROR                     00001798
00206          GO TO 0320-INPUT-ERROR.                                  00002060
00207      IF (APFKI NUMERIC) AND (APFKI > 0 AND < 25)                  00002061
00208          MOVE PF-VALUES (APFKI) TO EIBAID                         00002062
00209      ELSE                                                         00002063
00210          MOVE 0029               TO EMI-ERROR                     00002064
00211          GO TO 0320-INPUT-ERROR.                                  00002065
00212                                                                   00002066
00213  0300-CHECK-PFKEYS.                                               00002067
00214      IF EIBAID = DFHPF23                                          00002068
00215          GO TO 8810-PF23.                                         00002069
00216      IF EIBAID = DFHPF24                                          00002070
00217          GO TO 9200-RETURN-MAIN-MENU.                             00002071
00218      IF EIBAID = DFHPF12                                          00002072
00219          GO TO 9500-PF12.                                         00002073
00220      IF (AMAINTL NOT = 0 AND AMAINTI NOT = SPACE) AND             00002074
00221         EIBAID NOT = DFHENTER                                     00002075
00222          MOVE 0050               TO EMI-ERROR                     00002076
00223          GO TO 0320-INPUT-ERROR.                                  00002077
00224      IF EIBAID = DFHPF1                                           00002078
00225          GO TO 5000-FIND-NEXT-ACCOUNT.                            00002079
00226      IF EIBAID = DFHPF2                                           00002080
00227          GO TO 5100-FIND-PREV-ACCOUNT.                            00002081
00228      IF EIBAID = DFHENTER                                         00002082
00229          GO TO 0330-EDIT-DATA.                                    00002083
00230                                                                   00002084
00231      MOVE 0029                   TO EMI-ERROR.                    00002085
00232  0320-INPUT-ERROR.                                                00002086
00233      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00002087
00234                                                                   00002088
00235      MOVE AL-UNBON               TO APFKA.                        00002089
00236                                                                   00002360
00237      IF APFKL = 0                                                 00002361
00238          MOVE -1                 TO AMAINTL                       00002362
00239         ELSE                                                      00002363
00240          MOVE -1                 TO APFKL.                        00002364
00241                                                                   00002365
00242      GO TO 8200-SEND-DATAONLY.                                    00002366
00243                                                                   00002367
00244      EJECT                                                        00002368
00245  0330-EDIT-DATA.                                                  00002369
00246      MOVE AL-UANON               TO AMAINTA.                      00002370
00247                                                                   00002371
00248      IF AMAINTI = 'S'                                             00002372
00249          GO TO 1000-SHOW-ACCOUNT.                                 00002373
00250                                                                   00002374
00251      MOVE E023                   TO EMI-ERROR.                    00002375
00252      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00002376
00253      MOVE -1                     TO AMAINTL.                      00002377
00254      MOVE AL-UABON               TO AMAINTA.                      00002378
00255      GO TO 8200-SEND-DATAONLY.                                    00002379
00256                                                                   00002560
00257      EJECT                                                        00002561
00258  1000-SHOW-ACCOUNT.                                               00002562
00259      PERFORM 6100-BUILD-KEY THRU 6199-EXIT.                       00002563
00260      MOVE ACCT-PARTIAL-KEY TO SAVE-ACCT-PARTIAL-KEY.              00002564
00261                                                                   00002565
00262      EXEC CICS HANDLE CONDITION                                   00002566
00263           NOTFND(1010-NOT-FOUND)                                  00002567
00264      END-EXEC.                                                    00002568
00265                                                                   00002569
00266  1005-READ-ACCT.                                                  00002570
00267      MOVE LOW-VALUES             TO ACCT-FILLER.                  00002571
00268                                                                   00002572
00269      IF ACCT-EXP-DT = LOW-VALUES                                  00002573
00270         EXEC CICS READ                                            00002574
00271             DATASET(ACCT-ID)                                      00002575
00272             SET    (ACCT-POINTER)                                 00002576
00273             RIDFLD (ACCT-KEY)                                     00002577
00274             GTEQ                                                  00002578
00275         END-EXEC                                                  00002579
00276        ELSE                                                       00002580
00277         EXEC CICS READ                                            00002581
00278             DATASET(ACCT-ID)                                      00002582
00279             SET    (ACCT-POINTER)                                 00002583
00280             RIDFLD (ACCT-KEY)                                     00002584
00281         END-EXEC.                                                 00002585
00282                                                                   00002586
00283      SERVICE RELOAD ACCOUNT-MASTER.                               00002587
00284                                                                   00002840
00285      MOVE AM-CONTROL-PRIMARY     TO PI-PREV-ACCOUNT               00002841
00286                                     ACCT-KEY.                     00002842
00287      IF ACCT-PARTIAL-KEY NOT = SAVE-ACCT-PARTIAL-KEY              00002843
00288         GO TO 1010-NOT-FOUND.                                     00002844
00289                                                                   00002845
00290      MOVE AM-CONTROL-BY-VAR-GRP  TO PI-PREV-VG-ACCOUNT.           00002846
00291      GO TO 7000-BUILD-OUTPUT-MAP.                                 00002847
00292                                                                   00002848
00293                                                                   00002849
00294  1010-NOT-FOUND.                                                  00002850
00295      MOVE E226                   TO EMI-ERROR.                    00002851
00296      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00002852
00297      MOVE -1                     TO AMAINTL.                      00002853
00298      MOVE AL-UABON               TO ACARIERA                      00002854
00299                                     AGROUPA                       00002855
00300                                     ASTATEA                       00002856
00301                                     AACCTA                        00002857
00302                                     AEXPDTA.                      00002858
00303                                                                   00002859
00304      MOVE LOW-VALUES             TO PI-PROGRAM-WORK-AREA.         00002860
00305      GO TO 8100-SEND-INITIAL-MAP.                                 00002861
00306                                                                   00002862
00307      EJECT                                                        00002863
00308  5000-FIND-NEXT-ACCOUNT.                                          00002864
00309      MOVE PI-PREV-ACCOUNT        TO ACCT-KEY.                     00002865
00310                                                                   00003100
00311      IF PI-PREV-ACCOUNT = LOW-VALUES                              00003101
00312         MOVE PI-COMPANY-CD       TO ACCT-CO                       00003102
00313       ELSE                                                        00003103
00314         MOVE 'Y' TO FIRST-TIME-SW.                                00003104
00315                                                                   00003150
00316      EXEC CICS HANDLE CONDITION                                   00003151
00317           NOTFND (5030-NOT-FOUND)                                 00003152
00318           ENDFILE(5030-NOT-FOUND)                                 00003153
00319      END-EXEC.                                                    00003154
00320                                                                   00003155
00321  5005-START-BR.                                                   00003156
00322      MOVE LOW-VALUES             TO ACCT-FILLER.                  00003157
00323                                                                   00003230
00324      EXEC CICS STARTBR                                            00003231
00325           DATASET(ACCT-ID)                                        00003232
00326           RIDFLD (ACCT-KEY)                                       00003233
00327      END-EXEC.                                                    00003234
00328                                                                   00003235
00329      MOVE 'Y'                    TO BROWSE-STARTED-SW.            00003236
00330                                                                   00003237
00331  5010-READ-NEXT.                                                  00003238
00332      EXEC CICS READNEXT                                           00003239
00333           DATASET(ACCT-ID)                                        00003240
00334           SET    (ACCT-POINTER)                                   00003241
00335           RIDFLD (ACCT-KEY)                                       00003242
00336      END-EXEC.                                                    00003243
00337                                                                   00003244
00338      SERVICE RELOAD ACCOUNT-MASTER.                               00003245
00339                                                                   00003246
00340      IF PI-COMPANY-CD  NOT = ACCT-CO                              00003247
00341         IF PI-PREV-ACCOUNT = LOW-VALUES                           00003248
00342            MOVE E472             TO EMI-ERROR                     00003249
00343            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               00003430
00344            MOVE -1               TO AMAINTL                       00003440
00345            GO TO 8200-SEND-DATAONLY                               00003441
00346         ELSE                                                      00003442
00347            MOVE E066                   TO EMI-ERROR               00003443
00348            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               00003444
00349            MOVE -1               TO AMAINTL                       00003445
00350            GO TO 8200-SEND-DATAONLY.                              00003446
00351                                                                   00003447
00352      IF FIRST-TIME                                                00003448
00353         MOVE SPACES TO FIRST-TIME-SW                              00003449
00354         GO TO 5010-READ-NEXT.                                     00003450
00355                                                                   00003550
00356  5020-DISPLAY-RECORD.                                             00003551
00357      MOVE AM-CONTROL-PRIMARY     TO PI-PREV-ACCOUNT.              00003552
00358      MOVE AM-CONTROL-BY-VAR-GRP  TO PI-PREV-VG-ACCOUNT.           00003553
00359      GO TO 7000-BUILD-OUTPUT-MAP.                                 00003554
00360                                                                   00003555
00361  5030-NOT-FOUND.                                                  00003556
00362      IF BROWSE-STARTED                                            00003557
00363         MOVE SPACE               TO BROWSE-STARTED-SW             00003558
00364         EXEC CICS ENDBR                                           00003559
00365              DATASET(ACCT-ID)                                     00003560
00366         END-EXEC.                                                 00003561
00367                                                                   00003562
00368      IF ACCT-ACCOUNT = LOW-VALUE                                  00003563
00369         MOVE E472                   TO EMI-ERROR                  00003564
00370         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  00003565
00371         MOVE -1 TO AMAINTL                                        00003566
00372         GO TO 8200-SEND-DATAONLY.                                 00003567
00373                                                                   00003730
00374      MOVE E066                   TO EMI-ERROR.                    00003731
00375      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00003732
00376      MOVE PI-PREV-ACCOUNT        TO ACCT-KEY.                     00003733
00377      GO TO 5005-START-BR.                                         00003734
00378                                                                   00003735
00379      EJECT                                                        00003736
00380  5100-FIND-PREV-ACCOUNT.                                          00003737
00381      IF PI-PREV-ACCOUNT = LOW-VALUES                              00003738
00382         GO TO 5130-NOT-FOUND                                      00003739
00383      ELSE                                                         00003740
00384         MOVE PI-PREV-ACCOUNT TO ACCT-KEY.                         00003741
00385                                                                   00003850
00386      EXEC CICS HANDLE CONDITION                                   00003851
00387           NOTFND (5130-NOT-FOUND)                                 00003852
00388           ENDFILE(5130-NOT-FOUND)                                 00003853
00389      END-EXEC.                                                    00003854
00390                                                                   00003855
00391      EXEC CICS STARTBR                                            00003856
00392           DATASET(ACCT-ID)                                        00003857
00393           RIDFLD (ACCT-KEY)                                       00003858
00394           EQUAL                                                   00003859
00395      END-EXEC.                                                    00003860
00396                                                                   00003861
00397      MOVE 'Y'                    TO BROWSE-STARTED-SW             00003862
00398                                     FIRST-TIME-SW.                00003863
00399                                                                   00003864
00400  5110-READ-PREV.                                                  00003865
00401      EXEC CICS READPREV                                           00003866
00402           DATASET(ACCT-ID)                                        00003867
00403           SET    (ACCT-POINTER)                                   00003868
00404           RIDFLD (ACCT-KEY)                                       00003869
00405      END-EXEC.                                                    00003870
00406                                                                   00004060
00407      SERVICE RELOAD ACCOUNT-MASTER.                               00004061
00408                                                                   00004062
00409      IF ACCT-CO NOT = PI-COMPANY-CD                               00004063
00410         GO TO 5130-NOT-FOUND.                                     00004064
00411                                                                   00004065
00412      IF FIRST-TIME                                                00004066
00413         MOVE SPACES TO FIRST-TIME-SW                              00004067
00414         GO TO 5110-READ-PREV.                                     00004068
00415                                                                   00004069
00416      MOVE AM-CONTROL-PRIMARY     TO PI-PREV-ACCOUNT.              00004070
00417      MOVE AM-CONTROL-BY-VAR-GRP  TO PI-PREV-VG-ACCOUNT.           00004071
00418      GO TO 7000-BUILD-OUTPUT-MAP.                                 00004072
00419                                                                   00004073
00420  5130-NOT-FOUND.                                                  00004074
00421      IF BROWSE-STARTED                                            00004075
00422         MOVE SPACE               TO BROWSE-STARTED-SW             00004076
00423         EXEC CICS ENDBR                                           00004077
00424              DATASET(ACCT-ID)                                     00004078
00425          END-EXEC.                                                00004079
00426                                                                   00004080
00427      MOVE E067                   TO EMI-ERROR.                    00004081
00428      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00004082
00429      MOVE -1                     TO AMAINTL.                      00004083
00430                                                                   00004084
00431      IF PI-PREV-ACCOUNT  =  LOW-VALUES                            00004085
00432         GO TO 5000-FIND-NEXT-ACCOUNT.                             00004086
00433                                                                   00004330
00434      MOVE PI-PREV-ACCOUNT        TO ACCT-KEY.                     00004331
00435      GO TO 1005-READ-ACCT.                                        00004332
00436                                                                   00004333
00437      EJECT                                                        00004334
00438                                                                   00004380
00439      EJECT                                                        00004381
00440  6100-BUILD-KEY.                                                  00004382
00441      MOVE LOW-VALUES             TO ACCT-KEY.                     00004383
00442      MOVE PI-COMPANY-CD          TO ACCT-CO.                      00004384
00443                                                                   00004385
00444      IF ACARIERI = SPACES OR LOW-VALUES                           00004386
00445          MOVE SPACES             TO ACCT-CARRIER                  00004387
00446      ELSE                                                         00004388
00447          MOVE AL-UANON           TO ACARIERA                      00004389
00448          MOVE ACARIERI           TO ACCT-CARRIER.                 00004390
00449                                                                   00004391
00450      IF AGROUPI = SPACES OR LOW-VALUES                            00004392
00451          MOVE SPACES             TO ACCT-GROUPING                 00004393
00452      ELSE                                                         00004394
00453          MOVE AL-UANON           TO AGROUPA                       00004395
00454          MOVE AGROUPI            TO ACCT-GROUPING.                00004396
00455                                                                   00004397
00456      IF ASTATEI = SPACES OR LOW-VALUES                            00004398
00457          MOVE SPACES             TO ACCT-STATE                    00004399
00458      ELSE                                                         00004400
00459          MOVE AL-UANON           TO ASTATEA                       00004401
00460          MOVE ASTATEI            TO ACCT-STATE.                   00004402
00461                                                                   00004403
00462      IF AACCTI = SPACES OR LOW-VALUES                             00004404
00463          MOVE E471               TO EMI-ERROR                     00004405
00464          MOVE -1                 TO AACCTL                        00004406
00465          PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT                00004407
00466      ELSE                                                         00004408
00467          MOVE AL-UANON           TO AACCTA                        00004409
00468          MOVE AACCTI             TO ACCT-ACCOUNT.                 00004410
00469                                                                   00004411
00470      IF ACCT-STATE = SPACES AND                                   00004412
00471         (ST-ACCNT-CNTL         OR                                 00004413
00472          CARR-ST-ACCNT-CNTL    OR                                 00004414
00473          CARR-GROUP-ST-ACCNT-CNTL)                                00004415
00474         MOVE E144                TO EMI-ERROR                     00004416
00475         MOVE -1                  TO ASTATEL                       00004417
00476         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                00004418
00477                                                                   00004419
00478      IF ACCT-CARRIER = SPACES AND                                 00004420
00479         (CARR-ACCNT-CNTL       OR                                 00004421
00480          CARR-ST-ACCNT-CNTL    OR                                 00004422
00481          CARR-GROUP-ST-ACCNT-CNTL)                                00004423
00482         MOVE E193                TO EMI-ERROR                     00004424
00483         MOVE -1                  TO ACARIERL                      00004425
00484         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                00004426
00485                                                                   00004427
00486      IF ACCT-GROUPING = SPACES AND                                00004428
00487          CARR-GROUP-ST-ACCNT-CNTL                                 00004429
00488         MOVE E195                TO EMI-ERROR                     00004430
00489         MOVE -1                  TO AGROUPL                       00004431
00490         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                00004432
00491                                                                   00004433
00492      IF AEXPDTL = ZEROS                                           00004434
00493         GO TO 6150-CHECK-ERRORS.                                  00004435
00494                                                                   00004436
00495      MOVE AEXPDTI                TO DEEDIT-FIELD.                 00004437
00496      PERFORM 8600-DEEDIT.                                         00004438
00497      IF DEEDIT-FIELD-V0 NOT LESS 999999                           00004439
00498         MOVE DEEDIT-FIELD-V0     TO AEXPDTO                       00004440
uktdel*       TRANSFORM AEXPDTI FROM SPACES TO '/'                      00004441
uktins        INSPECT AEXPDTI REPLACING ALL ' ' BY '/'
00500         MOVE HIGH-VALUES         TO EXPDTE-SAVE                   00004442
00501                                     ACCT-EXP-DT                   00004443
00502         GO TO 6150-CHECK-ERRORS.                                  00004444
00503                                                                   00004445
00504      MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY.           00004446
00505      MOVE '4'                    TO DC-OPTION-CODE.               00004447
00506      PERFORM 9700-DATE-LINK.                                      00004448
00507      IF DATE-CONVERSION-ERROR                                     00004449
00508         MOVE E454                TO EMI-ERROR                     00004450
00509         MOVE -1                  TO AEXPDTL                       00004451
00510         MOVE AL-UABON            TO AEXPDTA                       00004452
00511         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  00004453
00512      ELSE                                                         00004454
00513         MOVE AL-UANON            TO AEXPDTA                       00004455
00514         MOVE DC-GREG-DATE-1-EDIT TO AEXPDTI                       00004456
00515         MOVE AL-UANON            TO AEXPDTA                       00004457
00516         MOVE DC-BIN-DATE-1       TO EXPDTE-SAVE                   00004458
00517                                     ACCT-EXP-DT.                  00004459
00518                                                                   00004460
00519  6150-CHECK-ERRORS.                                               00004461
00520      IF NOT EMI-NO-ERRORS                                         00004462
00521         GO TO 8200-SEND-DATAONLY.                                 00004463
00522                                                                   00005220
00523      IF ACCT-CARRIER  = SPACES  OR                                00005221
00524         ACCT-STATE    = SPACES  OR                                00005222
00525         ACCT-GROUPING = SPACES                                    00005223
00526         MOVE ACCT-VG-ID          TO ACCT-ID.                      00005224
00527                                                                   00005225
00528  6199-EXIT.                                                       00005226
00529      EXIT.                                                        00005227
00530                                                                   00005228
00531      EJECT                                                        00005229
00532  7000-BUILD-OUTPUT-MAP.                                           00005230
00533      MOVE AL-UANON               TO ACARIERA                      00005231
00534                                     AGROUPA                       00005232
00535                                     ASTATEA                       00005233
00536                                     AACCTA                        00005234
00537                                     AEXPDTA.                      00005235
00538      MOVE AM-CARRIER             TO ACARIERO.                     00005236
00539      MOVE AM-GROUPING            TO AGROUPO.                      00005237
00540      MOVE AM-STATE               TO ASTATEO.                      00005238
00541      MOVE AM-ACCOUNT             TO AACCTO.                       00005239
00542                                                                   00005240
00543      IF AM-EXPIRATION-DT = HIGH-VALUES                            00005241
00544         MOVE 999999              TO AEXPDTO                       00005242
uktdel*       TRANSFORM AEXPDTI FROM SPACES TO '/'                      00005243
uktins        INSPECT AEXPDTI REPLACING ALL ' ' BY '/'                  00005243
00546        ELSE                                                       00005244
00547         MOVE AM-EXPIRATION-DT       TO DC-BIN-DATE-1              00005245
00548         MOVE SPACE                  TO DC-OPTION-CODE             00005246
00549         PERFORM 9700-DATE-LINK                                    00005247
00550         IF DATE-CONVERSION-ERROR                                  00005248
00551             MOVE SPACES             TO AEXPDTI                    00005249
00552            ELSE                                                   00005250
00553             MOVE DC-GREG-DATE-1-EDIT TO AEXPDTI.                  00005251
00554                                                                   00005252
00555      MOVE AM-EFFECTIVE-DT        TO DC-BIN-DATE-1.                00005253
00556      MOVE SPACE                  TO DC-OPTION-CODE.               00005254
00557      PERFORM 9700-DATE-LINK.                                      00005255
00558      IF DATE-CONVERSION-ERROR                                     00005256
00559          MOVE SPACES             TO AEFFDTI                       00005257
00560         ELSE                                                      00005258
00561          MOVE DC-GREG-DATE-1-EDIT TO AEFFDTI.                     00005259
00562                                                                   00005260
00563      MOVE AM-NAME                TO ANAMEO.                       00005261
00564      MOVE AM-PERSON              TO ACAREOFO.                     00005262
00565      MOVE AM-ADDRS               TO AADDR1O.                      00005263
00566 *    MOVE AM-CITY                TO ACITYSTO.                     00005264
           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
              DELIMITED BY '  ' INTO ACITYSTO
           END-STRING
00567      MOVE AM-TEL-NO              TO WS-WORK-PHONE.                00005265
uktdel*    TRANSFORM WS-WORK-PHONE FROM SPACES TO '0'.                  00005266
uktins     INSPECT WS-WORK-PHONE REPLACING ALL ' ' BY '0'.
00569      MOVE WS-NUMERIC-PHONE       TO APHONEO.                      00005267
uktdel*    TRANSFORM APHONEI FROM SPACES TO '-'.                        00005268
uktins     INSPECT APHONEI REPLACING ALL ' ' BY '/'                     00005268
00571                                                                   00005269
00572      IF  AM-CANADIAN-POST-CODE                                    00005270
00573          MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1            00005271
00574          MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2            00005272
00575          MOVE SPACES             TO WS-DASH-CAN                   00005273
uktdel*                                TO WS-CAN-FILLER                 00005274
uktins                                    WS-CAN-FILLER                 00005274
00577          MOVE WS-CANADIAN-POSTAL-CODES                            00005275
00578                                  TO AZIPO                         00005276
00579                                                                   00005277
00580      ELSE                                                         00005278
00581          MOVE AM-ZIP-PRIME       TO WS-ZIP-CODE                   00005279
00582                                                                   00005820
00583          IF  AM-ZIP-PLUS4 = SPACES OR ZEROS                       00005821
00584              MOVE SPACES         TO WS-ZIP-PLUS4                  00005822
00585                                     WS-DASH                       00005823
00586              MOVE WS-ZIP         TO AZIPO                         00005824
00587                                                                   00005825
00588          ELSE                                                     00005826
00589              MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4                  00005827
00590              MOVE '-'            TO WS-DASH                       00005828
00591              MOVE WS-ZIP         TO AZIPO.                        00005829
00592                                                                   00005830
00593      MOVE AM-EARN-METHOD-A       TO ADECAHO.                      00005831
00594      MOVE AM-EARN-METHOD-L       TO ALIFLEVO.                     00005832
00595      MOVE AM-EARN-METHOD-R       TO ADECLIFO.                     00005833
00596      MOVE AM-TOL-CLM             TO AALLOWO.                      00005834
00597                                                                   00005835
00598      MOVE -1 TO AMAINTL.                                          00005836
00599      IF BROWSE-STARTED                                            00005837
00600         MOVE SPACE               TO BROWSE-STARTED-SW             00005838
00601         EXEC CICS ENDBR                                           00005839
00602              DATASET(ACCT-ID)                                     00005840
00603         END-EXEC.                                                 00005841
00604                                                                   00005842
00605      GO TO 8100-SEND-INITIAL-MAP.                                 00005843
00606                                                                   00005844
00607      EJECT                                                        00005845
00608  8100-SEND-INITIAL-MAP.                                           00005846
00609      MOVE SAVE-DATE    TO ADATEO.                                 00005847
00610      MOVE EIBTIME      TO TIME-IN.                                00005848
00611      MOVE TIME-OUT     TO ATIMEO.                                 00005849
00612      MOVE -1           TO AMAINTL.                                00005850
00613      MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O.                        00005851
00614      MOVE EMI-MESSAGE-AREA (2) TO AEMSG2O.                        00005852
00615      EXEC CICS SEND                                               00005853
00616          MAP   (MAP-NAME)                                         00005854
00617          MAPSET(MAPSET-NAME)                                      00005855
00618          FROM  (EL113AO)                                          00005856
00619          ERASE                                                    00005857
00620          CURSOR                                                   00005858
00621      END-EXEC.                                                    00005859
00622                                                                   00005860
00623      GO TO 9100-RETURN-TRAN.                                      00005861
00624                                                                   00005862
00625  8200-SEND-DATAONLY.                                              00005863
00626      MOVE SAVE-DATE    TO ADATEO.                                 00005864
00627      MOVE EIBTIME TO TIME-IN.                                     00005865
00628      MOVE TIME-OUT TO ATIMEO.                                     00005866
00629      MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O.                        00005867
00630      MOVE EMI-MESSAGE-AREA (2) TO AEMSG2O.                        00005868
00631                                                                   00005869
00632      EXEC CICS SEND                                               00005870
00633          MAP   (MAP-NAME)                                         00005871
00634          MAPSET(MAPSET-NAME)                                      00005872
00635          FROM  (EL113AO)                                          00005873
00636          DATAONLY                                                 00005874
00637          CURSOR                                                   00005875
00638      END-EXEC.                                                    00005876
00639                                                                   00005877
00640      GO TO 9100-RETURN-TRAN.                                      00005878
00641                                                                   00005879
00642  8300-SEND-TEXT.                                                  00005880
00643      EXEC CICS SEND TEXT                                          00005881
00644          FROM  (LOGOFF-TEXT)                                      00005882
00645          LENGTH(LOGOFF-LENGTH)                                    00005883
00646          ERASE                                                    00005884
00647          FREEKB                                                   00005885
00648      END-EXEC.                                                    00006480
00649                                                                   00006490
00650      EXEC CICS RETURN                                             00006491
00651      END-EXEC.                                                    00006492
00652                                                                   00006493
00653      EJECT                                                        00006494
00654  8400-LOG-JOURNAL-RECORD.                                         00006495
00655      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   00006496
00656      MOVE ACCT-ID                TO JP-FILE-ID.                   00006497
00657      MOVE PGM-EL113              TO JP-PROGRAM-ID.                00006498
pemuni*    EXEC CICS JOURNAL                                            00006499
pemuni*         JFILEID(PI-JOURNAL-FILE-ID)                             00006500
pemuni*         JTYPEID('CL')                                           00006501
pemuni*         FROM   (JOURNAL-RECORD)                                 00006502
pemuni*         LENGTH (273)                                            00006503
pemuni*    END-EXEC.                                                    00006504
00664                                                                   00006505
00665                                                                   00006650
00666  8600-DEEDIT.                                                     00006651
00667      EXEC CICS BIF DEEDIT                                         00006652
00668           FIELD (DEEDIT-FIELD)                                    00006653
00669           LENGTH(15)                                              00006654
00670      END-EXEC.                                                    00006655
00671                                                                   00006710
00672  8800-UNAUTHORIZED-ACCESS.                                        00006711
00673      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   00006712
00674      GO TO 8300-SEND-TEXT.                                        00006713
00675                                                                   00006714
00676  8810-PF23.                                                       00006715
00677      MOVE EIBAID                 TO PI-ENTRY-CD-1.                00006716
00678      MOVE XCTL-005               TO PGM-NAME.                     00006717
00679      GO TO 9300-XCTL.                                             00006718
00680                                                                   00006719
00681  8850-DUPREC.                                                     00006720
00682      MOVE 0147                   TO EMI-ERROR.                    00006721
00683      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00006722
00684      MOVE -1                     TO AACCTL.                       00006723
00685      MOVE AL-UABON               TO AACCTA.                       00006724
00686      GO TO 8200-SEND-DATAONLY.                                    00006725
00687                                                                   00006726
00688  8870-CNTL-NOTOPEN.                                               00006727
00689      MOVE E042                   TO EMI-ERROR.                    00006728
00690      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00006729
00691      MOVE -1                     TO AMAINTL.                      00006730
00692      GO TO 8200-SEND-DATAONLY.                                    00006731
00693                                                                   00006732
00694  8880-ACCT-NOTOPEN.                                               00006733
00695      MOVE E168                   TO EMI-ERROR.                    00006734
00696      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    00006735
00697      MOVE -1                     TO AMAINTL.                      00006736
00698      GO TO 8200-SEND-DATAONLY.                                    00006737
00699                                                                   00006738
00700  9000-RETURN-CICS.                                                00006739
00701      EXEC CICS RETURN                                             00006740
00702      END-EXEC.                                                    00006741
00703                                                                   00006742
00704  9100-RETURN-TRAN.                                                00006743
00705      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.               00006744
00706      MOVE '106A' TO PI-CURRENT-SCREEN-NO.                         00006745
00707      EXEC CICS RETURN                                             00006746
00708          TRANSID(TRANS-ID)                                        00006747
00709          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        00006748
00710          LENGTH(PI-COMM-LENGTH)                                   00006749
00711      END-EXEC.                                                    00006750
00712                                                                   00006751
00713  9200-RETURN-MAIN-MENU.                                           00006752
00714      MOVE XCTL-126 TO PGM-NAME.                                   00006753
00715      GO TO 9300-XCTL.                                             00006754
00716                                                                   00006755
00717  9300-XCTL.                                                       00006756
00718      EXEC CICS XCTL                                               00006757
00719          PROGRAM(PGM-NAME)                                        00006758
00720          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        00006759
00721          LENGTH(PI-COMM-LENGTH)                                   00006760
00722      END-EXEC.                                                    00006761
00723                                                                   00006762
00724  9400-CLEAR.                                                      00006763
00725      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       00006764
00726      GO TO 9300-XCTL.                                             00006765
00727                                                                   00006766
00728  9500-PF12.                                                       00006767
00729      MOVE XCTL-010 TO PGM-NAME.                                   00006768
00730      GO TO 9300-XCTL.                                             00006769
00731                                                                   00006770
00732  9700-DATE-LINK.                                                  00006771
00733      MOVE LINK-CLDATCV           TO PGM-NAME                      00006772
00734      EXEC CICS LINK                                               00006773
00735          PROGRAM   (PGM-NAME)                                     00006774
00736          COMMAREA  (DATE-CONVERSION-DATA)                         00006775
00737          LENGTH    (DC-COMM-LENGTH)                               00006776
00738      END-EXEC.                                                    00006777
00739                                                                   00006778
00740  9600-PGMID-ERROR.                                                00006779
00741      EXEC CICS HANDLE CONDITION                                   00006780
00742          PGMIDERR(8300-SEND-TEXT)                                 00006781
00743      END-EXEC.                                                    00006782
00744                                                                   00006783
00745      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                     00006784
00746      MOVE ' '          TO PI-ENTRY-CD-1.                          00006785
00747      MOVE XCTL-005     TO PGM-NAME.                               00006786
00748      MOVE PGM-NAME     TO LOGOFF-PGM.                             00006787
00749      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            00006788
00750      GO TO 9300-XCTL.                                             00006789
00751                                                                   00006790
00752  9900-ERROR-FORMAT.                                               00006791
00753      IF NOT EMI-ERRORS-COMPLETE                                   00006792
00754          MOVE LINK-001 TO PGM-NAME                                00006793
00755          EXEC CICS LINK                                           00006794
00756              PROGRAM(PGM-NAME)                                    00006795
00757              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              00006796
00758              LENGTH(EMI-COMM-LENGTH)                              00006797
00759          END-EXEC.                                                00006798
00760                                                                   00006799
00761  9900-EXIT.                                                       00006800
00762      EXIT.                                                        00006801
00763                                                                   00006802
00764  9990-ABEND.                                                      00006803
00765      MOVE LINK-004 TO PGM-NAME.                                   00006804
00766      MOVE DFHEIBLK TO EMI-LINE1.                                  00006805
00767      EXEC CICS LINK                                               00006806
00768          PROGRAM  (PGM-NAME)                                      00006807
00769          COMMAREA (EMI-LINE1)                                     00006808
00770          LENGTH   (72)                                            00006809
00771      END-EXEC.                                                    00006810
00772                                                                   00006811
00773      GO TO 8200-SEND-DATAONLY.                                    00006812
00774      GOBACK.                                                      00006813
