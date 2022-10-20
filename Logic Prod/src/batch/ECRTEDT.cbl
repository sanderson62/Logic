      $set osvs
00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 ECRTEDT.                             00000030
00004 *                            VMOD=2.001.                          00000031
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
00021  REMARKS.                                                         00000210
uktdel         MATCH CERTIFICATE MASTER TO ACCOUNT MASTER FOR           00000220
uktdel         STATISTICS AND ERRORS.                                   00000230
00024  EJECT                                                            00000240
00025  ENVIRONMENT DIVISION.                                            00000250
00026  INPUT-OUTPUT SECTION.                                            00000260
00027  FILE-CONTROL.                                                    00000270
00028                                                                   00000280
00029      SELECT PRINTR           ASSIGN TO SYS008-UR-1403-S-SYS008.   00000290
00030      SELECT CERT-IN          ASSIGN TO SYS010-UT-2400-S-SYS010.   00000300
00031      SELECT ACCT-IN          ASSIGN TO SYS013-UT-2400-S-SYS013.   00000310
00032  EJECT                                                            00000320
00033  DATA DIVISION.                                                   00000330
00034  FILE SECTION.                                                    00000340
00035                                                                   00000350
00036  FD  PRINTR                                                       00000360
00037      RECORDING MODE F                                             00000370
00038      LABEL RECORDS OMITTED                                        00000380
00039      RECORD CONTAINS 133 CHARACTERS.                              00000390
00040  01  PRT-REC.                                                     00000400
00041      12  FILLER              PIC  X(133).                         00000410
00042                                                                   00000420
00043  FD  CERT-IN                                                      00000430
00044      RECORDING MODE F                                             00000440
00045      LABEL RECORDS STANDARD                                       00000450
00046      BLOCK CONTAINS 0 RECORDS                                     00000460
00047      RECORD CONTAINS 700 CHARACTERS.                              00000470
00048  01  CERT-IN-REC             PIC  X(700).                         00000480
00049                                                                   00000490
00050  FD  ACCT-IN                                                      00000500
00051      RECORDING MODE F                                             00000510
00052      LABEL RECORDS STANDARD                                       00000520
00053      BLOCK CONTAINS 0 RECORDS                                     00000530
00054      RECORD CONTAINS 2000 CHARACTERS.                             00000540
00055  01  ACCT-IN-REC             PIC  X(2000).                        00000550
00056  EJECT                                                            00000560
00057  WORKING-STORAGE SECTION.                                         00000570
00058  77  FILLER  PIC  X(32) VALUE '********************************'. 00000580
00059  77  FILLER  PIC  X(32) VALUE '*          ECRTEDT             *'. 00000590
00060  77  FILLER  PIC  X(32) VALUE '**********V/M=2.001*************'. 00000600
00061                                                                   00000610
00062  77  SAVE-OVFLO              PIC S999    COMP-3  VALUE +66.       00000620
00063  77  PGCTR                   PIC S9(5)   COMP-3  VALUE +0.        00000630
00064  77  C-CNT                   PIC S9(5)   COMP-3  VALUE +0.        00000640
00065  77  SPACE-NP                PIC  X              VALUE '1'.       00000650
00066  77  SPACE-1                 PIC  X              VALUE ' '.       00000660
00067  77  SPACE-2                 PIC  X              VALUE '0'.       00000670
00068  77  SPACE-3                 PIC  X              VALUE '-'.       00000680
00069  77  ABEND-SW                PIC  X              VALUE 'N'.       00000690
00070      88  ABEND-ON                                VALUE 'Y'.       00000700
00071                                                                   00000710
00072  01  WS.                                                          00000720
00073      12  WS-RETURN-CODE       PIC S9(4)    COMP    VALUE +0.      00000730
00074      12  WS-ABEND-MESSAGE     PIC X(80)            VALUE SPACES.  00000740
00075      12  WS-ABEND-FILE-STATUS PIC X(80)            VALUE SPACES.  00000750
00076      12  WS-ZERO              PIC S9               VALUE +0.      00000760
00077  01  MISC-WORK-AREA.                                              00000770
00078      12  OLD-DATE.                                                00000780
00079          16  O-YR            PIC  99             VALUE 99.        00000790
00080          16  O-MO            PIC  99             VALUE 99.        00000800
00081          16  O-DA            PIC  99             VALUE 99.        00000810
00082      12  NEW-DATE.                                                00000820
00083          16  N-YR            PIC  99             VALUE 00.        00000830
00084          16  N-MO            PIC  99             VALUE 00.        00000840
00085          16  N-DA            PIC  99             VALUE 00.        00000850
00086  EJECT                                                            00000860
00087  01  HD1.                                                         00000870
00088      12  FILLER              PIC  X(109)         VALUE            00000880
00089              'LOGIC CREDIT INSURANCE SYSTEM'.                     00000890
00090      12  FILLER              PIC  X(5)           VALUE 'PAGE'.    00000900
00091      12  HD-PG               PIC ZZZ,ZZ9.                         00000910
00092      12  FILLER              PIC  X(11)          VALUE            00000920
00093              '   CERTEDIT'.                                       00000930
00094                                                                   00000940
00095  01  HD2.                                                         00000950
00096      12  FILLER              PIC  X(124)         VALUE            00000960
00097              'CERTIFICATE VS. ACCOUNT MASTER EDIT'.               00000970
00098      12  HD-RUN              PIC  X(8).                           00000980
00099                                                                   00000990
00100  01  HD3.                                                         00001000
00101      12  FILLER              PIC  X(44)          VALUE            00001010
00102              ' CARR.       ACCOUNT                        '.      00001020
00103      12  FILLER              PIC  X(44)          VALUE            00001030
00104              '           EFF.     EXP.    OLDEST   NEWEST '.      00001040
00105      12  FILLER              PIC  X(44)          VALUE            00001050
00106              '   NO.                   CERT       CERT    '.      00001060
00107                                                                   00001070
00108  01  HD4.                                                         00001080
00109      12  FILLER              PIC  X(44)          VALUE            00001090
00110              ' GROUP  ST   NUMBER            ACCOUNT NAME '.      00001100
00111      12  FILLER              PIC  X(44)          VALUE            00001110
00112              '           DATE     DATE     DATE     DATE  '.      00001120
00113      12  FILLER              PIC  X(44)          VALUE            00001130
00114              '  CERTS                  DATE      NUMBER   '.      00001140
00115                                                                   00001150
00116  01  P-REC.                                                       00001160
00117      12  CCSW                PIC  X.                              00001170
00118      12  P-LN.                                                    00001180
00119          16  P-CARRIER       PIC  X.                              00001190
00120          16  P-GROUPING      PIC  X(6).                           00001200
00121          16  FILLER          PIC  X.                              00001210
00122          16  P-STATE         PIC  XX.                             00001220
00123          16  FILLER          PIC  X.                              00001230
00124          16  P-ACCOUNT       PIC  X(10).                          00001240
00125          16  FILLER          PIC  X.                              00001250
00126          16  P-NAME          PIC  X(30).                          00001260
00127          16  FILLER          PIC  X.                              00001270
00128          16  P-EMO           PIC  XX.                             00001280
00129          16  P-EMOD          PIC  X.                              00001290
00130          16  P-EDA           PIC  XX.                             00001300
00131          16  P-EDAD          PIC  X.                              00001310
00132          16  P-EYR           PIC  XX.                             00001320
00133          16  FILLER          PIC  X.                              00001330
00134          16  P-XMO           PIC  XX.                             00001340
00135          16  P-XMOD          PIC  X.                              00001350
00136          16  P-XDA           PIC  XX.                             00001360
00137          16  P-XDAD          PIC  X.                              00001370
00138          16  P-XYR           PIC  XX.                             00001380
00139          16  FILLER          PIC  X.                              00001390
00140          16  P-OMO           PIC  XX.                             00001400
00141          16  P-OMOD          PIC  X.                              00001410
00142          16  P-ODA           PIC  XX.                             00001420
00143          16  P-ODAD          PIC  X.                              00001430
00144          16  P-OYR           PIC  XX.                             00001440
00145          16  FILLER          PIC  X.                              00001450
00146          16  P-NMO           PIC  XX.                             00001460
00147          16  P-NMOD          PIC  X.                              00001470
00148          16  P-NDA           PIC  XX.                             00001480
00149          16  P-NDAD          PIC  X.                              00001490
00150          16  P-NYR           PIC  XX.                             00001500
00151          16  FILLER          PIC  X.                              00001510
00152          16  P-COUNT         PIC ZZZ,ZZZ.                         00001520
00153          16  FILLER          PIC  X.                              00001530
00154          16  P-MESS          PIC  X(13).                          00001540
00155          16  FILLER          PIC  X.                              00001550
00156          16  P-CMO           PIC  XX.                             00001560
00157          16  P-CMOD          PIC  X.                              00001570
00158          16  P-CDA           PIC  XX.                             00001580
00159          16  P-CDAD          PIC  X.                              00001590
00160          16  P-CYR           PIC  XX.                             00001600
00161          16  FILLER          PIC  X.                              00001610
00162          16  P-CERT          PIC  X(11).                          00001620
00163          16  FILLER          PIC  X.                              00001630
00164  EJECT                                                            00001640
00165                              COPY ECSCRT01.                       00001641
00166  EJECT                                                            00001660
00167                              COPY ERCACCT.                        00001661
00168  EJECT                                                            00001680
00169  PROCEDURE DIVISION.                                              00001690
00170                                                                   00001700
00171  0100-INITIALIZATION.                                             00001710
00172      OPEN INPUT   CERT-IN  ACCT-IN                                00001720
00173           OUTPUT  PRINTR.                                         00001730
00174                                                                   00001740
00175      MOVE CURRENT-DATE           TO  HD-RUN.                      00001750
00176                                                                   00001760
00177      PERFORM 1800-HD-RTN  THRU  1899-EXIT.                        00001770
00178                                                                   00001780
00179  0200-READ-ACCT.                                                  00001790
00180      READ ACCT-IN  INTO  ACCOUNT-MASTER  AT END                   00001800
00181          MOVE HIGH-VALUES        TO  ACCOUNT-MASTER.              00001810
00182                                                                   00001820
00183  0299-EXIT.                                                       00001830
00184      EXIT.                                                        00001840
00185                                                                   00001850
00186  0300-READ-CERT.                                                  00001860
00187      READ CERT-IN  INTO  CERTIFICATE-RECORD  AT END               00001870
00188          MOVE HIGH-VALUES        TO  CERTIFICATE-RECORD.          00001880
00189                                                                   00001890
00190  0399-EXIT.                                                       00001900
00191      EXIT.                                                        00001910
00192                                                                   00001920
00193  0400-MAIN-LOOP.                                                  00001930
00194      IF CR-ACCT-CONTROL = AM-CONTROL-A                            00001940
00195          GO TO 0800-CHECK-DATE.                                   00001950
00196                                                                   00001960
00197      IF CR-ACCT-CONTROL GREATER AM-CONTROL-A                      00001970
00198          GO TO 0700-END-ACCT.                                     00001980
00199                                                                   00001990
00200  0500-NO-ACCT.                                                    00002000
00201      MOVE CR-CARRIER             TO  P-CARRIER.                   00002010
00202      MOVE CR-GROUPING            TO  P-GROUPING.                  00002020
00203      MOVE CR-STATE               TO  P-STATE.                     00002030
00204      MOVE CR-ACCOUNT             TO  P-ACCOUNT.                   00002040
00205      MOVE CR-YR                  TO  P-CYR.                       00002050
00206      MOVE CR-MO                  TO  P-CMO.                       00002060
00207      MOVE CR-DA                  TO  P-CDA.                       00002070
00208      MOVE CR-CERT-NO             TO  P-CERT.                      00002080
00209      MOVE '-'                    TO  P-CMOD  P-CDAD.              00002090
00210      MOVE 'NO ACCT MSTR'         TO  P-MESS.                      00002100
00211      MOVE 'Y'                    TO  ABEND-SW.                    00002110
00212      MOVE SPACE-1                TO  CCSW.                        00002120
00213                                                                   00002130
00214      PERFORM 1700-PRT-RTN  THRU  1799-EXIT.                       00002140
00215                                                                   00002150
00216  0600-N-A-X.                                                      00002160
00217      PERFORM 0300-READ-CERT  THRU  0399-EXIT.                     00002170
00218                                                                   00002180
00219      GO TO 0400-MAIN-LOOP.                                        00002190
00220                                                                   00002200
00221  0700-END-ACCT.                                                   00002210
00222      MOVE AM-CARRIER             TO  P-CARRIER.                   00002220
00223      MOVE AM-GROUPING            TO  P-GROUPING.                  00002230
00224      MOVE AM-STATE               TO  P-STATE.                     00002240
00225      MOVE AM-ACCOUNT             TO  P-ACCOUNT.                   00002250
00226      MOVE AM-NAME                TO  P-NAME.                      00002260
00227      MOVE AM-EXP-YR              TO  P-XYR.                       00002270
00228      MOVE AM-EXP-MO              TO  P-XMO.                       00002280
00229      MOVE AM-EXP-DA              TO  P-XDA.                       00002290
00230      MOVE AM-EFF-MO              TO  P-EMO.                       00002300
00231      MOVE AM-EFF-DA              TO  P-EDA.                       00002310
00232      MOVE AM-EFF-YR              TO  P-EYR.                       00002320
00233      MOVE '-'                    TO  P-EMOD  P-EDAD.              00002330
00234      MOVE '-'                    TO  P-XMOD  P-XDAD.              00002340
00235      MOVE C-CNT                  TO  P-COUNT.                     00002350
00236                                                                   00002360
00237      IF C-CNT = ZERO                                              00002370
00238          MOVE ' NO CERTS'        TO  P-MESS.                      00002380
00239                                                                   00002390
00240      IF OLD-DATE NOT = ALL '9'                                    00002400
00241          MOVE O-YR               TO  P-OYR                        00002410
00242          MOVE O-MO               TO  P-OMO                        00002420
00243          MOVE O-DA               TO  P-ODA                        00002430
00244          MOVE '-'                TO  P-ODAD  P-OMOD.              00002440
00245                                                                   00002450
00246      IF NEW-DATE NOT = ALL '0'                                    00002460
00247          MOVE N-YR               TO  P-NYR                        00002470
00248          MOVE N-MO               TO  P-NMO                        00002480
00249          MOVE N-DA               TO  P-NDA                        00002490
00250          MOVE '-'                TO  P-NDAD  P-NMOD.              00002500
00251                                                                   00002510
00252      MOVE SPACE-1                TO  CCSW.                        00002520
00253                                                                   00002530
00254      PERFORM 1700-PRT-RTN  THRU  1799-EXIT.                       00002540
00255                                                                   00002550
00256      PERFORM 0200-READ-ACCT  THRU  0299-EXIT.                     00002560
00257                                                                   00002570
00258      MOVE +0                     TO  C-CNT.                       00002580
00259      MOVE ALL '9'                TO  OLD-DATE.                    00002590
00260      MOVE ALL '0'                TO  NEW-DATE.                    00002600
00261                                                                   00002610
00262      GO TO 0400-MAIN-LOOP.                                        00002620
00263                                                                   00002630
00264  0800-CHECK-DATE.                                                 00002640
00265      IF CR-DT = HIGH-VALUES                                       00002650
00266          GO TO 9000-END-OF-JOB.                                   00002660
00267                                                                   00002670
00268      IF CR-DT LESS AM-EFFECT-DT                                   00002680
00269          GO TO 0500-NO-ACCT.                                      00002690
00270                                                                   00002700
00271      IF CR-DT NOT LESS AM-EXPIRE-DT                               00002710
00272          GO TO 0700-END-ACCT.                                     00002720
00273                                                                   00002730
00274      ADD +1                      TO  C-CNT.                       00002740
00275                                                                   00002750
00276      IF CR-DT LESS OLD-DATE                                       00002760
00277          MOVE CR-DT              TO  OLD-DATE.                    00002770
00278                                                                   00002780
00279      IF CR-DT GREATER NEW-DATE                                    00002790
00280          MOVE CR-DT              TO  NEW-DATE.                    00002800
00281                                                                   00002810
00282      PERFORM 0300-READ-CERT  THRU  0399-EXIT.                     00002820
00283                                                                   00002830
00284      GO TO 0400-MAIN-LOOP.                                        00002840
00285                                                                   00002850
00286  1700-PRT-RTN.                                                    00002860
00287      IF CCSW = SPACE-1                                            00002870
00288          ADD 1                   TO  SAVE-OVFLO                   00002880
00289      ELSE                                                         00002890
00290          IF CCSW = SPACE-2                                        00002900
00291              ADD 2               TO  SAVE-OVFLO                   00002910
00292          ELSE                                                     00002920
00293              IF CCSW = SPACE-3                                    00002930
00294                  ADD 3           TO  SAVE-OVFLO                   00002940
00295              ELSE                                                 00002950
00296                  IF CCSW = SPACE-NP                               00002960
00297                      MOVE ZEROS  TO  SAVE-OVFLO.                  00002970
00298                                                                   00002980
00299      WRITE PRT-REC  FROM  P-REC  AFTER POSITIONING  CCSW.         00002990
00300                                                                   00003000
00301      MOVE SPACE-1                TO  P-REC.                       00003010
00302                                                                   00003020
00303      IF SAVE-OVFLO GREATER +52                                    00003030
00304          PERFORM 1800-HD-RTN  THRU  1899-EXIT.                    00003040
00305                                                                   00003050
00306  1799-EXIT.                                                       00003060
00307      EXIT.                                                        00003070
00308                                                                   00003080
00309  1800-HD-RTN.                                                     00003090
00310      MOVE SPACE-NP               TO  CCSW.                        00003100
00311                                                                   00003110
00312      ADD 1                       TO  PGCTR.                       00003120
00313                                                                   00003130
00314      MOVE PGCTR                  TO  HD-PG.                       00003140
00315      MOVE HD1                    TO  P-LN.                        00003150
00316                                                                   00003160
00317      PERFORM 1700-PRT-RTN  THRU  1799-EXIT.                       00003170
00318                                                                   00003180
00319      MOVE HD2                    TO  P-LN.                        00003190
00320                                                                   00003200
00321      PERFORM 1700-PRT-RTN  THRU  1799-EXIT.                       00003210
00322                                                                   00003220
00323      MOVE SPACE-2                TO  CCSW.                        00003230
00324      MOVE HD3                    TO  P-LN.                        00003240
00325                                                                   00003250
00326      PERFORM 1700-PRT-RTN  THRU  1799-EXIT.                       00003260
00327                                                                   00003270
00328      MOVE HD4                    TO  P-LN.                        00003280
00329                                                                   00003290
00330      PERFORM 1700-PRT-RTN  THRU  1799-EXIT.                       00003300
00331                                                                   00003310
00332      MOVE SPACE-1                TO  P-REC.                       00003320
00333                                                                   00003330
00334      PERFORM 1700-PRT-RTN  THRU  1799-EXIT.                       00003340
00335                                                                   00003350
00336  1899-EXIT.                                                       00003360
00337      EXIT.                                                        00003370
00338                                                                   00003380
00339  9000-END-OF-JOB.                                                 00003390
00340      PERFORM 1800-HD-RTN  THRU  1899-EXIT.                        00003400
00341                                                                   00003410
00342      CLOSE  PRINTR  ACCT-IN  CERT-IN.                             00003420
00343                                                                   00003430
00344      GOBACK.                                                      00003440
00345                                                                   00003450
00346      IF ABEND-ON                                                  00003460
00347          MOVE +2000              TO  WS-RETURN-CODE               00003470
00348          MOVE 'NO ACCT MASTER'   TO  WS-ABEND-MESSAGE             00003480
00349          GO TO ABEND-PGM.                                         00003490
00350                                                                   00003500
00351      GOBACK.                                                      00003510
00352                                                                   00003520
00353  ABEND-PGM.                                                       00003530
00354                 COPY ELCABEND.                                    00003540
00355                                                                   00003550
