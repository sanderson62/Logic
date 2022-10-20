      $SET ALTER
00001  ID DIVISION.                                                     00000010
00002                                                                   00000020
00003  PROGRAM-ID.                     ECS201.                          00000030
00004 *                            VMOD=2.001.                          00000040
00005                                                                   00000050
00006                                                                   00000060
00007  AUTHOR.     LOGIC, INC.                                          00000070
00008              DALLAS, TEXAS.                                       00000080
00009                                                                   00000090
00010  DATE-COMPILED.                                                   00000100
00011                                                                   00000110
00012  SECURITY.   *****************************************************00000120
00013              *                                                   *00000130
00014              *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *00000140
00015              *                                                   *00000150
00016              *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *00000160
00017              *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *00000170
00018              *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *00000180
00019              *                                                   *00000190
00020              *****************************************************00000200
00021                                                                   00000210
00022  REMARKS.                                                         00000220
00023                                                                   00000230
00024      PRINT DOCUMENTATION MANUAL FROM CARD INPUT AND TAPE          00000240
00025              MANUAL SELECTION IS VIA A FINDER CARD                00000250
00026                  (MANUAL NAME IN COL 1-8 OF FINDER).              00000260
00027      PRINT DOCUMENTATION MANUAL FROM ICCF CARD IMAGES.            00000270
00028                                                                   00000280
00029      INPUT CARD FORMAT - COL. 1-2 = TT                            00000290
00030                          COL. 3-72 = DATA                         00000300
00031                          COL. 73-80 = UNUSED                      00000310
00032                                                                   00000320
00033      CONTROL FIELD - ** = MANUAL TITLE                            00000330
00034                      *C = CHAPTER TITLE                           00000340
00035                      *S = SECTION TITLE                           00000350
00036                      *P = PARAGRAPH TITLE                         00000360
00037                      *T = INSERT TABLE OF CONTENTS                00000370
00038                      NN = SKIP LINES AFTER PRINT                  00000380
00039                            IF NN = 01 SPACE 1 LINE AFTER          00000390
00040                            IF NN = 02 SPACE 2 LINES AFTER         00000400
00041                            IF NN = 99 SPACE 99 LINES AFTER        00000410
00042                            IF NN NOT NUMERIC, IGNORED.            00000420
00043                                                                   00000430
00044  EJECT                                                            00000440
00045  ENVIRONMENT DIVISION.                                            00000450
00046  INPUT-OUTPUT SECTION.                                            00000460
00047  FILE-CONTROL.                                                    00000470
00048                                                                   00000480
00049      SELECT SORT-FILE     ASSIGN TO 2 SYS001-UT-FBA1-S-SORTWK1.   00000490
00050                                                                   00000500
00051      SELECT DOCU-CARD     ASSIGN TO SYS006-UR-2540R-S-SYS006.     00000510
00052                                                                   00000520
00053      SELECT PRNT-FILE     ASSIGN TO SYS008-UR-1403-S-SYS008.      00000530
00054                                                                   00000540
00055      SELECT INDX-FILE     ASSIGN TO SYS016-UT-FBA1-S-SYS016.      00000550
00056                                                                   00000560
00057      SELECT CNTR-FILE     ASSIGN TO SYS017-UT-FBA1-S-SYS017.      00000570
00058                                                                   00000580
00059      SELECT CNTR-FILE-R   ASSIGN TO SYS014-UT-FBA1-S-SYS014.      00000590
00060                                                                   00000600
00061      SELECT WORK-FILE     ASSIGN TO SYS018-UT-FBA1-S-SYS018.      00000610
00062                                                                   00000620
00063      SELECT FICH          ASSIGN TO SYS020-UT-2400-S-SYS020.      00000630
00064                                                                   00000640
00065      SELECT DOCU-TAPE     ASSIGN TO SYS010-UT-2400-S-SYS010.      00000650
00066  EJECT                                                            00000660
00067  DATA DIVISION.                                                   00000670
00068  FILE SECTION.                                                    00000680
00069  FD  PRNT-FILE         COPY ELCPRTFD.                             00000690
00070                                                                   00000700
00071  01  PRT-1.                                                       00000710
00072      12  FILLER                  PIC XXXX.                        00000720
00073      12  P-DESC                  PIC X(73).                       00000730
00074      12  FILLER                  PIC X(56).                       00000740
00075                                                                   00000750
00076  01  PRT-2.                                                       00000760
00077      12  FILLER                  PIC XXXX.                        00000770
00078      12  PRT-2-DTL.                                               00000780
00079          16  PRT-2-TD            PIC X       OCCURS 70 TIMES.     00000790
00080      12  PRT-2-PGNO              PIC ZZZZZ.                       00000800
00081      12  FILLER                  PIC X(54).                       00000810
00082                                                                   00000820
00083  01  PRT-2A.                                                      00000830
00084      12  FILLER                  PIC XXXX.                        00000840
00085      12  FILLER                  PIC X(46).                       00000850
00086      12  PRT-2E-PGNO             PIC ZZZZZ.                       00000860
00087      12  PRT-2D-PGNO             PIC ZZZZZ.                       00000870
00088      12  PRT-2C-PGNO             PIC ZZZZZ.                       00000880
00089      12  PRT-2B-PGNO             PIC ZZZZZ.                       00000890
00090      12  PRT-2A-PGNO             PIC ZZZZZ.                       00000900
00091      12  FILLER                  PIC X(58).                       00000910
00092                                                                   00000920
00093  01  PRT-3.                                                       00000930
00094      12  FILLER                  PIC X(8).                        00000940
00095      12  PRT-3-DTL               PIC X(66).                       00000950
00096      12  FILLER                  PIC X(59).                       00000960
00097                                                                   00000970
00098  FD  DOCU-CARD                                                    00000980
00099      RECORDING MODE F                                             00000990
00100      LABEL RECORDS OMITTED.                                       00001000
00101  01  DOCU-CARD-REC               PIC X(80).                       00001010
00102                                                                   00001020
00103  01  D-FINDER.                                                    00001030
00104      12  D-MANUAL                PIC X(8).                        00001040
00105      12  FILLER                  PIC X(72).                       00001050
00106                                                                   00001060
00107  FD  INDX-FILE                                                    00001070
00108      RECORDING MODE F                                             00001080
00109      LABEL RECORDS STANDARD                                       00001090
uktdel*    BLOCK CONTAINS 0 RECORDS                                     00001100
uktins     BLOCK CONTAINS 0 RECORDS.                                    00001100
00111  01  INDX-REC.                                                    00001110
00112      12  INDX-IND.                                                00001120
00113          16  INDX-CTL            PIC XX.                          00001130
00114          16  INDX-DATA           PIC X(70).                       00001140
00115      12  INDX-PG                 PIC S9(5)   COMP-3.              00001150
00116                                                                   00001160
00117  FD  CNTR-FILE                                                    00001170
00118      RECORDING MODE F                                             00001180
00119      LABEL RECORDS STANDARD                                       00001190
00120      BLOCK CONTAINS 0 RECORDS                                     00001200
00121      RECORD CONTAINS 83 CHARACTERS.                               00001210
00122  01  CNTR-REC.                                                    00001220
00123      12  CNTR-NAME               PIC X(80).                       00001230
00124      12  CNTR-PG                 PIC S9(5)   COMP-3.              00001240
00125                                                                   00001250
00126  FD  CNTR-FILE-R                                                  00001260
00127      RECORDING MODE F                                             00001270
00128      LABEL RECORDS STANDARD                                       00001280
00129      BLOCK CONTAINS 0 RECORDS                                     00001290
00130      RECORD CONTAINS 83 CHARACTERS.                               00001300
00131  01  CNTR-REC-R.                                                  00001310
00132      12  CNTR-NAME-R             PIC X(80).                       00001320
00133      12  CNTR-PG-R               PIC S9(5)   COMP-3.              00001330
00134                                                                   00001340
00135  SD  SORT-FILE                                                    00001350
00136      RECORDING MODE F                                             00001360
00137      LABEL RECORDS STANDARD.                                      00001370
00138  01  SORT-REC.                                                    00001380
00139      12  SORT-CTL                PIC X(80).                       00001390
00140      12  SORT-PG                 PIC S9(5)   COMP-3.              00001400
00141                                                                   00001410
00142  FD  WORK-FILE                                                    00001420
00143      RECORDING MODE F                                             00001430
00144      LABEL RECORDS STANDARD                                       00001440
00145      BLOCK CONTAINS 0 RECORDS                                     00001450
00146      RECORD CONTAINS 133 CHARACTERS.                              00001460
00147  01  WORK-FILE-REC.                                               00001470
00148      12  WF-CTL                  PIC X.                           00001480
00149      12  WF-DATA.                                                 00001490
00150          16  WFX-CTL             PIC XX.                          00001500
00151          16  WF-DESC             PIC X(78).                       00001510
00152          16  FILLER              PIC X(52).                       00001520
00153  EJECT                                                            00001530
00154  FD  FICH                                                         00001540
00155                              COPY ECSFICH.                        00001550
00156  FD  DOCU-TAPE                                                    00001560
00157      RECORDING MODE F                                             00001570
00158      LABEL RECORDS STANDARD                                       00001580
uktdel*    BLOCK CONTAINS 0 RECORDS                                     00001590
uktins     BLOCK CONTAINS 0 RECORDS.                                    00001590
00160  01  DOCU-TAPE-REC               PIC X(80).                       00001600
00161                                                                   00001610
00162  EJECT                                                            00001620
00163  WORKING-STORAGE SECTION.                                         00001630
00164  77  FILLER  PIC X(32) VALUE '********************************'.  00001640
00165  77  FILLER  PIC X(32) VALUE '     ECS201 WORKING STARAGE     '.  00001650
00166  77  FILLER  PIC X(32) VALUE '*****V/M=2.001******************'.  00001660
00167                                                                   00001670
00168  77  FOUND-SW                    PIC X      VALUE SPACE.          00001680
00169      88  FOUND                              VALUE '1'.            00001690
00170  77  FINDER-SW                   PIC X      VALUE SPACE.          00001700
00171      88  FINDER                             VALUE '1'.            00001710
00172                                                                   00001720
00173  77  NO-COLONS                   PIC S9999  COMP.                 00001730
00174                                                                   00001740
00175  01  WS-ACCEPT-DATE.                                              00001750
00176      12  WS-AD-YY                    PIC 99.                      00001760
00177      12  WS-AD-MM                    PIC 99.                      00001770
00178      12  WS-AD-DD                    PIC 99.                      00001780
00179                                                                   00001790
00180  01  WS-CURRENT-DATE.                                             00001800
00181      12  WS-CD-MM                    PIC 99.                      00001810
00182      12  FILLER                      PIC X      VALUE '/'.        00001820
00183      12  WS-CD-DD                    PIC 99.                      00001830
00184      12  FILLER                      PIC X      VALUE '/'.        00001840
00185      12  WS-CD-YY                    PIC 99.                      00001850
00186                                                                   00001860
00187  01  HD1.                                                         00001870
00188      12  HD-MAJ                  PIC X(66)   VALUE SPACES.        00001880
00189      12  FILLER                  PIC X(6)    VALUE ' PAGE '.      00001890
00190      12  HD-PG                   PIC ZZ,ZZZ.                      00001900
00191                                                                   00001910
00192  01  HD2.                                                         00001920
00193      12  HD-SS                   PIC X(70)   VALUE SPACES.        00001930
00194      12  HD-DT                   PIC X(8)    VALUE SPACES.        00001940
00195                                                                   00001950
00196  01  HD3.                                                         00001960
00197      12  HD-HD                   PIC X(76)   VALUE SPACES.        00001970
00198                                                                   00001980
00199  01  HD4.                                                         00001990
00200      12  HD-MAJ2                 PIC X(73).                       00002000
00201                                                                   00002010
00202  01  HD5.                                                         00002020
00203      12  HD-TOC                  PIC X(18)   VALUE                00002030
00204         'TABLE OF CONTENTS'.                                      00002040
00205      12  HD-CNT                  PIC X(11)   VALUE                00002050
00206         '(CONTINUED)'.                                            00002060
00207                                                                   00002070
00208  01  WS-DOCU-REC.                                                 00002080
00209      12  WSD-ID.                                                  00002090
00210          16  WSD-ID-R            PIC 99.                          00002100
00211      12  WSD-DESC.                                                00002110
00212          16  WSD-SS              PIC X(57).                       00002120
00213          16  FILLER              PIC X(21).                       00002130
00214                                                                   00002140
00215  01  WS-DOCU-REC-R REDEFINES WS-DOCU-REC.                         00002150
00216      12  XSD-ID                  PIC XXX.                         00002160
00217      12  XSD-SEQ                 PIC XXXX.                        00002170
00218      12  XSD-DESC.                                                00002180
00219          16  XSD-SS              PIC X(57).                       00002190
00220          16  FILLER              PIC X(16).                       00002200
00221                                                                   00002210
00222  01  WS-DOCU-REC-F REDEFINES WS-DOCU-REC.                         00002220
00223      12  F-FINDER                PIC X(8).                        00002230
00224      12  FILLER                  PIC X(72).                       00002240
00225                                                                   00002250
00226  01  TAPE-REC-1    REDEFINES WS-DOCU-REC.                         00002260
00227      12  DOC-ID.                                                  00002270
00228          16  DOC-TAPE            PIC XX.                          00002280
00229          16  DOC-NAME            PIC X(8).                        00002290
00230      12  DOC-MANUAL              PIC X(8).                        00002300
00231      12  FILLER                  PIC X(62).                       00002310
00232                                                                   00002320
00233                                                                   00002330
00234  01  TEST-PRT.                                                    00002340
00235      12  FILLER                  PIC X(24)   VALUE                00002350
00236         'LINEUP RIGHT PERFORATION'.                               00002360
00237      12  FILLER                  PIC X(26)   VALUE                00002370
00238         ' BETWEEN PERIODS #########'.                             00002380
00239      12  FILLER                  PIC X(33)   VALUE                00002390
00240         '############################. .'.                        00002400
00241                                                                   00002410
00242  01  INDEX-WORK-AREA.                                             00002420
00243      12  WS-INDX.                                                 00002430
00244          16  WS-INDX-O           PIC X       OCCURS 80 TIMES.     00002440
00245                                                                   00002450
00246  01  INPUT-INDEX-AREA.                                            00002460
00247      12  I-I-A.                                                   00002470
00248          16  I-A                 PIC X       OCCURS 73 TIMES.     00002480
00249                                                                   00002490
00250  01  BINARY-WORK-AREA    COMP.                                    00002500
00251      12  X1                      PIC S999    VALUE +0.            00002510
00252      12  X2                      PIC S999    VALUE +0.            00002520
00253      12  X3                      PIC S999    VALUE +0.            00002530
00254      12  X4                      PIC S999    VALUE +0.            00002540
00255      12  B1                      PIC S999    VALUE +1.            00002550
00256      12  B2                      PIC S999    VALUE +2.            00002560
00257      12  B69                     PIC S999    VALUE +69.           00002570
00258      12  B70                     PIC S999    VALUE +70.           00002580
00259      12  B80                     PIC S999    VALUE +80.           00002590
00260      12  B12                     PIC S999    VALUE +12.           00002600
00261                                                                   00002610
00262  01  MISC-COMP-3-AREA    COMP-3.                                  00002620
00263      12  LN-CT                   PIC S999    VALUE +99.           00002630
00264      12  PG-NO                   PIC S9(5)   VALUE +0.            00002640
00265      12  K1                      PIC S999    VALUE +1.            00002650
00266      12  K99                     PIC S999    VALUE +99.           00002660
00267      12  LN-MX                   PIC S999    VALUE +53.           00002670
00268      12  K0                      PIC S999    VALUE +0.            00002680
00269      12  SAVE-PAGE               PIC S9(5)   VALUE +0.            00002690
00270      12  NEW-PAGE-SW             PIC S9      VALUE +0.            00002700
00271                                                                   00002710
00272  01  MISC-WORK-AREA.                                              00002720
00273      12  SV-CTL                  PIC XX.                          00002730
00274      12  K-CNTD                  PIC X(11)   VALUE '(CONTINUED)'. 00002740
00275      12  K-CNTD-R REDEFINES K-CNTD.                               00002750
00276          16  K-C                 PIC X       OCCURS 11 TIMES.     00002760
00277      12  KEY-WORD-SWITCH         PIC X       VALUE ' '.           00002770
00278      12  K-COLON                 PIC X       VALUE ':'.           00002780
00279      12  PG-ONE                  PIC X       VALUE ' '.           00002790
00280                                                                   00002800
00281      12  FICH-SW                 PIC X       VALUE SPACE.         00002810
00282                                                                   00002820
00283          88  FICHE     VALUE 'X'.                                 00002830
00284                                                                   00002840
00285      12  SAVE-TOC.                                                00002850
00286                                                                   00002860
00287          16  SV-TOC      PIC X     OCCURS 80.                     00002870
00288                                                                   00002880
00289      12  TAPE-INPUT      PIC X(3)       VALUE 'NO '.              00002890
00290      12  ABEND-CODE      PIC X(4)       VALUE SPACE.              00002900
00291      12  ABEND-OPTION    PIC X          VALUE 'Y'.                00002910
00292      12  PGM-SUB         PIC S999    COMP  VALUE +201.            00002920
00293      12  MANUAL-NAME     PIC X(8)       VALUE SPACES.             00002930
00294                                                                   00002940
00295  EJECT                                                            00002950
00296  PROCEDURE DIVISION.                                              00002960
00297  HSKPG.                                                           00002970
00298      OPEN INPUT DOCU-CARD                                         00002980
00299           OUTPUT INDX-FILE                                        00002990
00300                  CNTR-FILE                                        00003000
00301                  WORK-FILE                                        00003010
00302                  PRNT-FILE.                                       00003020
00303                                                                   00003030
00304      MOVE '1' TO P-CTL.                                           00003040
00305      MOVE +0 TO LN-CT.                                            00003050
00306                                                                   00003060
00307      ACCEPT WS-ACCEPT-DATE FROM DATE.                             00003070
00308      MOVE WS-AD-YY               TO WS-CD-YY.                     00003080
00309      MOVE WS-AD-MM               TO WS-CD-MM.                     00003090
00310      MOVE WS-AD-DD               TO WS-CD-DD.                     00003100
00311                                                                   00003110
00312      MOVE WS-CURRENT-DATE TO HD-DT.                               00003120
00313                                                                   00003130
00314  HSKPG-READ.                                                      00003140
00315      READ DOCU-CARD INTO WS-DOCU-REC                              00003150
00316              AT END GO TO BOJ-ABORT.                              00003160
00317                                                                   00003170
00318      IF DOC-TAPE = 'TT'                                           00003180
00319          MOVE DOC-NAME TO MANUAL-NAME                             00003190
00320          ALTER MAIN-READ TO PROCEED TO READ-TAPE                  00003200
00321          MOVE 'YES' TO TAPE-INPUT                                 00003210
00322          OPEN INPUT DOCU-TAPE                                     00003220
00323          GO TO SCAN-FOR-MANUAL.                                   00003230
00324                                                                   00003240
00325  HSK-2.                                                           00003250
00326      IF WSD-ID NOT = '**'                                         00003260
00327         IF XSD-ID NOT = '*K*' AND '*M*'                           00003270
00328            GO TO BOJ-ABORT.                                       00003280
00329                                                                   00003290
00330      DISPLAY WSD-DESC UPON CONSOLE.                               00003300
00331                                                                   00003310
00332      IF WSD-ID = '**'                                             00003320
00333          ALTER MAIN-LOOP TO PROCEED TO CHECK-NEW-CTL.             00003330
00334                                                                   00003340
00335  AGAIN-TST.                                                       00003350
00336      MOVE TEST-PRT TO P-DATA.                                     00003360
00337                                                                   00003370
00338      PERFORM PRINT-RTN-2.                                         00003380
00339                                                                   00003390
00340      IF LN-CT LESS LN-MX                                          00003400
00341          GO TO AGAIN-TST.                                         00003410
00342                                                                   00003420
00343      MOVE +99 TO LN-CT.                                           00003430
00344                                                                   00003440
00345      IF XSD-ID = '*M*' OR '*K*'                                   00003450
00346           MOVE XSD-DESC TO WSD-DESC                               00003460
00347           MOVE '**'     TO WSD-ID.                                00003470
00348                                                                   00003480
00349      MOVE WSD-DESC TO HD-MAJ                                      00003490
00350                       HD-MAJ2.                                    00003500
00351                                                                   00003510
00352  MAIN-LOOP.                                                       00003520
00353      GO TO CHECK-OLD-CTL.                                         00003530
00354                                                                   00003540
00355  CHECK-OLD-CTL.                                                   00003550
00356      IF WSD-ID = '**'                                             00003560
00357          GO TO CHECK-NEW-CTL.                                     00003570
00358                                                                   00003580
00359      IF XSD-ID = '*H*'                                            00003590
00360          MOVE '*C' TO WSD-ID                                      00003600
00361          GO TO SHIFT-OLD.                                         00003610
00362                                                                   00003620
00363      IF XSD-ID = '*HH'                                            00003630
00364          MOVE '*S' TO WSD-ID                                      00003640
00365          GO TO SHIFT-OLD.                                         00003650
00366                                                                   00003660
00367      IF XSD-ID = '*T*'                                            00003670
00368          MOVE '*T' TO WSD-ID                                      00003680
00369          GO TO SHIFT-OLD.                                         00003690
00370                                                                   00003700
00371      IF XSD-ID = '*S1'                                            00003710
00372          MOVE '01' TO WSD-ID                                      00003720
00373          GO TO SHIFT-OLD.                                         00003730
00374                                                                   00003740
00375      IF XSD-ID = '*S2'                                            00003750
00376          MOVE '02' TO WSD-ID                                      00003760
00377          GO TO SHIFT-OLD.                                         00003770
00378                                                                   00003780
00379      IF XSD-ID = '*S3'                                            00003790
00380          MOVE '03' TO WSD-ID                                      00003800
00381          GO TO SHIFT-OLD.                                         00003810
00382                                                                   00003820
00383      IF XSD-ID = '*S4'                                            00003830
00384          MOVE '04' TO WSD-ID                                      00003840
00385          GO TO SHIFT-OLD.                                         00003850
00386                                                                   00003860
00387      IF XSD-ID = '*S5'                                            00003870
00388          MOVE '05' TO WSD-ID                                      00003880
00389          GO TO SHIFT-OLD.                                         00003890
00390                                                                   00003900
00391      IF XSD-ID = '*S6'                                            00003910
00392          MOVE '06' TO WSD-ID                                      00003920
00393          GO TO SHIFT-OLD.                                         00003930
00394                                                                   00003940
00395      IF XSD-ID = '*S7'                                            00003950
00396          MOVE '07' TO WSD-ID                                      00003960
00397          GO TO SHIFT-OLD.                                         00003970
00398                                                                   00003980
00399      IF XSD-ID = '*S8'                                            00003990
00400          MOVE '08' TO WSD-ID                                      00004000
00401          GO TO SHIFT-OLD.                                         00004010
00402                                                                   00004020
00403      IF XSD-ID = '*S9'                                            00004030
00404          MOVE '09' TO WSD-ID                                      00004040
00405          GO TO SHIFT-OLD.                                         00004050
00406                                                                   00004060
00407      IF XSD-ID = '*S0'                                            00004070
00408          MOVE '10' TO WSD-ID                                      00004080
00409          GO TO SHIFT-OLD.                                         00004090
00410                                                                   00004100
00411      IF XSD-ID = '*SA'                                            00004110
00412          MOVE '11' TO WSD-ID                                      00004120
00413          GO TO SHIFT-OLD.                                         00004130
00414                                                                   00004140
00415      IF XSD-ID = '*SB'                                            00004150
00416          MOVE '12' TO WSD-ID                                      00004160
00417          GO TO SHIFT-OLD.                                         00004170
00418                                                                   00004180
00419      IF XSD-ID = '*SC'                                            00004190
00420          MOVE '13' TO WSD-ID                                      00004200
00421          GO TO SHIFT-OLD.                                         00004210
00422                                                                   00004220
00423      IF XSD-ID = '*SD'                                            00004230
00424          MOVE '14' TO WSD-ID                                      00004240
00425          GO TO SHIFT-OLD.                                         00004250
00426                                                                   00004260
00427      IF XSD-ID = '*SE'                                            00004270
00428          MOVE '15' TO WSD-ID                                      00004280
00429          GO TO SHIFT-OLD.                                         00004290
00430                                                                   00004300
00431      IF XSD-ID = '*SF'                                            00004310
00432          MOVE '16' TO WSD-ID                                      00004320
00433          GO TO SHIFT-OLD.                                         00004330
00434                                                                   00004340
00435      IF XSD-ID = '*SG'                                            00004350
00436          MOVE '17' TO WSD-ID                                      00004360
00437          GO TO SHIFT-OLD.                                         00004370
00438                                                                   00004380
00439      IF XSD-ID = '*SH'                                            00004390
00440          MOVE '18' TO WSD-ID                                      00004400
00441          GO TO SHIFT-OLD.                                         00004410
00442                                                                   00004420
00443      IF XSD-ID = '*SI'                                            00004430
00444          MOVE '19' TO WSD-ID                                      00004440
00445          GO TO SHIFT-OLD.                                         00004450
00446                                                                   00004460
00447      MOVE SPACES TO WSD-ID.                                       00004470
00448                                                                   00004480
00449  SHIFT-OLD.                                                       00004490
00450      MOVE XSD-DESC TO WSD-DESC.                                   00004500
00451                                                                   00004510
00452  CHECK-NEW-CTL.                                                   00004520
00453      IF WSD-ID = '**'                                             00004530
00454          GO TO SET-MANUAL-TITLE.                                  00004540
00455                                                                   00004550
00456      IF WSD-ID = '*C'                                             00004560
00457          GO TO SET-CHAPTER.                                       00004570
00458                                                                   00004580
00459      IF WSD-ID = '*S'                                             00004590
00460          GO TO SET-SECTION.                                       00004600
00461                                                                   00004610
00462      IF WSD-ID = '*P'                                             00004620
00463          GO TO SET-PARA.                                          00004630
00464                                                                   00004640
00465      IF WSD-ID = '*T'                                             00004650
00466          GO TO SET-TOX.                                           00004660
00467                                                                   00004670
00468      IF LN-CT GREATER LN-MX                                       00004680
00469          PERFORM PT-HEAD.                                         00004690
00470                                                                   00004700
00471      IF WSD-ID = '*/'                                             00004710
00472          MOVE SPACES             TO  WSD-ID                       00004720
00473          MOVE +1                 TO  NEW-PAGE-SW.                 00004730
00474                                                                   00004740
00475      MOVE SPACES   TO WORK-FILE-REC.                              00004750
00476      MOVE WSD-DESC TO WF-DESC.                                    00004760
00477                                                                   00004770
00478  PRINT-RTN-1.                                                     00004780
00479      IF LN-CT GREATER LN-MX                                       00004790
00480          PERFORM PT-HEAD.                                         00004800
00481                                                                   00004810
00482      MOVE ZEROS TO NO-COLONS.                                     00004820
00483                                                                   00004830
00484      INSPECT WORK-FILE-REC                                        00004840
00485              TALLYING NO-COLONS FOR ALL ':'.                      00004850
00486                                                                   00004860
00487      IF NO-COLONS GREATER ZERO                                    00004870
00488          PERFORM KEY-WORD-XTRCT  THRU K-W-X.                      00004880
00489                                                                   00004890
00490      TRANSFORM WORK-FILE-REC CHARACTERS FROM ':' TO ' '.          00004900
00491                                                                   00004910
00492      WRITE WORK-FILE-REC                                          00004920
00493            INVALID KEY GO TO SPOOL-FULL.                          00004930
00494                                                                   00004940
00495      MOVE SPACES TO WORK-FILE-REC.                                00004950
00496                                                                   00004960
00497      ADD K1 TO LN-CT.                                             00004970
00498                                                                   00004980
00499      IF NEW-PAGE-SW = +1                                          00004990
00500          MOVE ZERO               TO  NEW-PAGE-SW                  00005000
00501          PERFORM PT-HEAD.                                         00005010
00502                                                                   00005020
00503  CHECK-KEYWORD.                                                   00005030
00504      IF WSD-ID-R NUMERIC                                          00005040
00505          PERFORM PRINT-RTN-1 WSD-ID-R TIMES.                      00005050
00506                                                                   00005060
00507  EJECT                                                            00005070
00508  MAIN-READ.                                                       00005080
00509      GO TO READ-CARDS.                                            00005090
00510                                                                   00005100
00511  READ-CARDS.                                                      00005110
00512      READ DOCU-CARD INTO WS-DOCU-REC                              00005120
00513              AT END GO TO END-SPOOL.                              00005130
00514                                                                   00005140
00515      GO TO M-R-X.                                                 00005150
00516                                                                   00005160
00517  READ-TAPE.                                                       00005170
00518      READ DOCU-TAPE INTO WS-DOCU-REC AT END                       00005180
00519              GO TO END-SPOOL.                                     00005190
00520  RT-CHECK.                                                        00005200
00521      IF XSD-ID = '$$$'                                            00005210
00522         GO TO READ-TAPE.                                          00005220
00523                                                                   00005230
00524      IF DOC-ID = ' *MANUAL* '                                     00005240
00525 *       IF MANUAL-NAME = 'ALL'                                    00005250
00526 *          GO TO READ-TAPE                                        00005260
00527 *     ELSE                                                        00005270
00528           GO TO END-SPOOL.                                        00005280
00529                                                                   00005290
00530      GO TO M-R-X.                                                 00005300
00531                                                                   00005310
00532  M-R-X.                                                           00005320
00533      GO TO MAIN-LOOP.                                             00005330
00534  EJECT                                                            00005340
00535  SET-TOX.                                                         00005350
00536      MOVE SPACES TO WORK-FILE-REC.                                00005360
00537      MOVE '*T' TO WFX-CTL.                                        00005370
00538                                                                   00005380
00539      GO TO PRINT-RTN-1.                                           00005390
00540                                                                   00005400
00541  SET-MANUAL-TITLE.                                                00005410
00542      MOVE K99 TO LN-CT.                                           00005420
00543      MOVE K1 TO PG-NO.                                            00005430
00544      MOVE WSD-DESC TO HD-MAJ HD-MAJ2.                             00005440
00545      GO TO MAIN-READ.                                             00005450
00546                                                                   00005460
00547  SET-CHAPTER.                                                     00005470
00548      MOVE K99 TO LN-CT.                                           00005480
00549      MOVE WSD-SS TO HD-SS.                                        00005490
00550      MOVE  PG-NO TO INDX-PG.                                      00005500
00551                                                                   00005510
00552      IF WSD-SS = SPACES GO TO MAIN-READ.                          00005520
00553                                                                   00005530
00554  BUILD-TOC.                                                       00005540
00555      MOVE WS-DOCU-REC TO INDX-IND.                                00005550
00556                                                                   00005560
00557      PERFORM BUILD-INDEX THRU B-I-X.                              00005570
00558                                                                   00005580
00559      WRITE INDX-REC INVALID KEY GO TO TOC-FULL.                   00005590
00560                                                                   00005600
00561  B-T-X.                                                           00005610
00562      EXIT.                                                        00005620
00563                                                                   00005630
00564  S-S-X.                                                           00005640
00565      GO TO MAIN-READ.                                             00005650
00566  EJECT                                                            00005660
00567  SET-SECTION.                                                     00005670
00568      MOVE WSD-DESC TO HD-HD.                                      00005680
00569                                                                   00005690
00570      IF WSD-DESC = SPACES                                         00005700
00571          GO TO MAIN-READ.                                         00005710
00572                                                                   00005720
00573      MOVE 'X' TO PG-ONE.                                          00005730
00574                                                                   00005740
00575  PT-HEAD.                                                         00005750
00576      MOVE K0 TO LN-CT.                                            00005760
00577                                                                   00005770
00578      PERFORM PT-HDNG.                                             00005780
00579                                                                   00005790
00580      MOVE HD3 TO WF-DATA.                                         00005800
00581      MOVE '-' TO WF-CTL.                                          00005810
00582                                                                   00005820
00583      IF PG-ONE = ' '   MOVE SPACES TO WF-DATA                     00005830
00584                        MOVE 'X' TO PG-ONE.                        00005840
00585                                                                   00005850
00586      PERFORM PRINT-RTN-1.                                         00005860
00587                                                                   00005870
00588      MOVE '0' TO WF-CTL.                                          00005880
00589                                                                   00005890
00590      PERFORM PRINT-RTN-1.                                         00005900
00591                                                                   00005910
00592  INSERT-CONTD.                                                    00005920
00593      MOVE HD3 TO WS-INDX.                                         00005930
00594      MOVE B80 TO X1.                                              00005940
00595                                                                   00005950
00596  I-C-LP.                                                          00005960
00597      IF WS-INDX-O (X1) NOT = ' '                                  00005970
00598          GO TO INS-CT.                                            00005980
00599                                                                   00005990
00600      SUBTRACT B1 FROM X1.                                         00006000
00601                                                                   00006010
00602      IF X1 NOT = B2                                               00006020
00603          GO TO I-C-LP.                                            00006030
00604                                                                   00006040
00605  INS-CT.                                                          00006050
00606      ADD B2 TO X1.                                                00006060
00607      MOVE B1 TO X2.                                               00006070
00608                                                                   00006080
00609  I-C-MV.                                                          00006090
00610      MOVE K-C (X2) TO WS-INDX-O (X1).                             00006100
00611                                                                   00006110
00612      ADD B1 TO X2.                                                00006120
00613      ADD B1 TO X1.                                                00006130
00614                                                                   00006140
00615      IF X2 = B12 OR                                               00006150
00616         X1 GREATER B80                                            00006160
00617              GO TO I-C-X.                                         00006170
00618                                                                   00006180
00619      GO TO I-C-MV.                                                00006190
00620                                                                   00006200
00621  I-C-X.                                                           00006210
00622      EXIT.                                                        00006220
00623                                                                   00006230
00624  I-C-XX.                                                          00006240
00625      MOVE WS-INDX TO HD3.                                         00006250
00626                                                                   00006260
00627  S-P-2.                                                           00006270
00628      COMPUTE INDX-PG = PG-NO - K1.                                00006280
00629                                                                   00006290
00630      PERFORM BUILD-TOC THRU B-T-X.                                00006300
00631                                                                   00006310
00632      GO TO MAIN-READ.                                             00006320
00633                                                                   00006330
00634  SET-PARA.                                                        00006340
00635      MOVE WSD-DESC TO HD-HD.                                      00006350
00636                                                                   00006360
00637      IF WSD-DESC = SPACES                                         00006370
00638          GO TO MAIN-READ.                                         00006380
00639                                                                   00006390
00640      MOVE HD3 TO WF-DATA.                                         00006400
00641      MOVE ' ' TO WF-CTL.                                          00006410
00642                                                                   00006420
00643      PERFORM PRINT-RTN-1.                                         00006430
00644                                                                   00006440
00645      MOVE SPACES TO WF-DATA.                                      00006450
00646                                                                   00006460
00647      PERFORM PRINT-RTN-1.                                         00006470
00648                                                                   00006480
00649      GO TO INSERT-CONTD.                                          00006490
00650                                                                   00006500
00651  EJECT                                                            00006510
00652  PT-HDNG.                                                         00006520
00653      MOVE PG-NO TO HD-PG.                                         00006530
00654      MOVE HD1 TO WF-DATA.                                         00006540
00655      MOVE '1' TO WF-CTL.                                          00006550
00656                                                                   00006560
00657      IF PG-ONE = ' '                                              00006570
00658          MOVE SPACES TO WF-DATA.                                  00006580
00659                                                                   00006590
00660      PERFORM PRINT-RTN-1.                                         00006600
00661                                                                   00006610
00662      MOVE HD2 TO WF-DATA.                                         00006620
00663                                                                   00006630
00664      IF PG-ONE = ' '                                              00006640
00665          MOVE SPACES TO WF-DATA.                                  00006650
00666                                                                   00006660
00667      PERFORM PRINT-RTN-1.                                         00006670
00668                                                                   00006680
00669      ADD K1 TO PG-NO.                                             00006690
00670                                                                   00006700
00671  SET-TOC.                                                         00006710
00672      PERFORM BUILD-TOC THRU B-T-X.                                00006720
00673                                                                   00006730
00674      GO TO MAIN-READ.                                             00006740
00675                                                                   00006750
00676  BOJ-ABORT.                                                       00006760
00677                                                                   00006770
00678      IF WSD-ID = 'FI'                                             00006780
00679          MOVE 'X' TO FICH-SW                                      00006790
00680          GO TO HSKPG-READ.                                        00006800
00681                                                                   00006810
00682      DISPLAY 'ECS201 - MISSING ** RECORD' UPON CONSOLE.           00006820
00683      DISPLAY 'ECS201 - MISSING ** RECORD'.                        00006830
00684                                                                   00006840
00685      GOBACK.                                                      00006850
00686                                                                   00006860
00687  SPOOL-FULL.                                                      00006870
00688      DISPLAY 'ECS201-EXTENTS FOR SYS018 EXCEEDED' UPON CONSOLE.   00006880
00689      DISPLAY 'ECS201-EXTENTS FOR SYS018 EXCEEDED'.                00006890
00690                                                                   00006900
00691      GOBACK.                                                      00006910
00692                                                                   00006920
00693  TOC-FULL.                                                        00006930
00694      DISPLAY 'ECS201-EXTENTS FOR SYS016 EXCEEDED' UPON CONSOLE.   00006940
00695      DISPLAY 'ECS201-EXTENTS FOR SYS016 EXCEEDED'.                00006950
00696                                                                   00006960
00697      GOBACK.                                                      00006970
00698                                                                   00006980
00699  BUILD-INDEX.                                                     00006990
00700      MOVE INDX-CTL TO SV-CTL.                                     00007000
00701      MOVE SPACES   TO INDX-CTL.                                   00007010
00702      MOVE INDX-PG  TO CNTR-PG.                                    00007020
00703      MOVE INDX-IND TO WS-INDX.                                    00007030
00704      MOVE B1       TO X1.                                         00007040
00705      MOVE B2       TO X2.                                         00007050
00706                                                                   00007060
00707  B-I-LOOP.                                                        00007070
00708      IF WS-INDX-O (X1) NOT = ' '                                  00007080
00709          GO TO B-I-END.                                           00007090
00710                                                                   00007100
00711  B-I-MOVE.                                                        00007110
00712      MOVE WS-INDX-O (X2) TO WS-INDX-O (X1).                       00007120
00713                                                                   00007130
00714      ADD B1 TO X1.                                                00007140
00715      ADD B1 TO X2.                                                00007150
00716                                                                   00007160
00717      IF X1 NOT = B80                                              00007170
00718          GO TO B-I-MOVE.                                          00007180
00719                                                                   00007190
00720      MOVE ' ' TO WS-INDX-O (80).                                  00007200
00721      MOVE B1 TO X1.                                               00007210
00722      MOVE B2 TO X2.                                               00007220
00723                                                                   00007230
00724      GO TO B-I-LOOP.                                              00007240
00725                                                                   00007250
00726  B-I-END.                                                         00007260
00727      MOVE WS-INDX TO CNTR-NAME.                                   00007270
00728      MOVE SV-CTL TO INDX-CTL.                                     00007280
00729                                                                   00007290
00730      IF WS-INDX = SPACES                                          00007300
00731          GO TO B-I-X.                                             00007310
00732                                                                   00007320
00733      WRITE CNTR-REC                                               00007330
00734          INVALID KEY GO TO INDEX-FULL.                            00007340
00735                                                                   00007350
00736  B-I-X.                                                           00007360
00737      EXIT.                                                        00007370
00738                                                                   00007380
00739  INDEX-FULL.                                                      00007390
00740      DISPLAY 'ECS201-EXTENTS FOR SYS017 EXCEEDED' UPON CONSOLE.   00007400
00741      DISPLAY 'ECS201-EXTENTS FOR SYS017 EXCEEDED'.                00007410
00742                                                                   00007420
00743      GOBACK.                                                      00007430
00744                                                                   00007440
00745  END-SPOOL.                                                       00007450
00746      CLOSE DOCU-CARD                                              00007460
00747            INDX-FILE                                              00007470
00748            WORK-FILE                                              00007480
00749            CNTR-FILE.                                             00007490
00750                                                                   00007500
00751  SORT-INDEX SECTION.                                              00007510
00752      MOVE +5000 TO SORT-FILE-SIZE.                                00007520
00753                                                                   00007530
00754      SORT SORT-FILE ON ASCENDING KEY SORT-CTL                     00007540
00755                                      SORT-PG                      00007550
00756          USING CNTR-FILE                                          00007560
00757          GIVING CNTR-FILE-R.                                      00007570
00758                                                                   00007580
00759  PRINT-MANUAL SECTION.                                            00007590
00760      OPEN INPUT INDX-FILE                                         00007600
00761                 CNTR-FILE-R                                       00007610
00762                 WORK-FILE.                                        00007620
00763                                                                   00007630
00764      IF FICHE                                                     00007640
00765           OPEN OUTPUT FICH.                                       00007650
00766                                                                   00007660
00767      MOVE SPACES TO SAVE-TOC.                                     00007670
00768                                                                   00007680
00769  PRINT-READ.                                                      00007690
00770      READ WORK-FILE AT END                                        00007700
00771          GO TO PRINT-INDEX.                                       00007710
00772                                                                   00007720
00773      MOVE WORK-FILE-REC TO PRT.                                   00007730
00774                                                                   00007740
00775      IF WFX-CTL = '*T'                                            00007750
00776          GO TO INSERT-TOC.                                        00007760
00777                                                                   00007770
00778      PERFORM PRINT-RTN-2.                                         00007780
00779                                                                   00007790
00780      GO TO PRINT-READ.                                            00007800
00781                                                                   00007810
00782  INSERT-TOC.                                                      00007820
00783      MOVE SPACES TO HD-CNT.                                       00007830
00784                                                                   00007840
00785      PERFORM REAL-PRT-HDNG THRU R-P-H-X.                          00007850
00786                                                                   00007860
00787      MOVE '(CONTINUED)' TO HD-CNT.                                00007870
00788                                                                   00007880
00789  TOC-LOOP.                                                        00007890
00790      READ INDX-FILE AT END                                        00007900
00791          GO TO END-TOC.                                           00007910
00792                                                                   00007920
00793      IF INDX-CTL = '*C'                                           00007930
00794          MOVE INDX-DATA TO PRT-2-DTL.                             00007940
00795                                                                   00007950
00796      IF INDX-CTL = '*S'                                           00007960
00797          MOVE INDX-DATA TO PRT-2-DTL                              00007970
00798          GO TO INSERT-AST.                                        00007980
00799                                                                   00007990
00800      IF INDX-CTL = '*P'                                           00008000
00801          MOVE INDX-DATA TO PRT-3-DTL                              00008010
00802          GO TO INSERT-AST.                                        00008020
00803                                                                   00008030
00804      MOVE INDX-DATA TO P-DATA.                                    00008040
00805                                                                   00008050
00806      ADD K1 TO LN-CT.                                             00008060
00807                                                                   00008070
00808      MOVE '0' TO P-CTL.                                           00008080
00809      MOVE INDX-DATA TO WS-INDX.                                   00008090
00810      MOVE B80 TO X1.                                              00008100
00811                                                                   00008110
00812      PERFORM I-C-LP THRU I-C-X.                                   00008120
00813                                                                   00008130
00814  TOC-PRNT.                                                        00008140
00815      PERFORM PRINT-RTN-2.                                         00008150
00816                                                                   00008160
00817      IF LN-CT LESS LN-MX                                          00008170
00818          GO TO TOC-LOOP.                                          00008180
00819                                                                   00008190
00820      PERFORM REAL-PRT-HDNG THRU R-P-H-X.                          00008200
00821                                                                   00008210
00822      MOVE WS-INDX TO P-DATA.                                      00008220
00823                                                                   00008230
00824      GO TO TOC-PRNT.                                              00008240
00825                                                                   00008250
00826      IF LN-CT GREATER LN-MX                                       00008260
00827          PERFORM REAL-PRT-HDNG THRU R-P-H-X.                      00008270
00828                                                                   00008280
00829      GO TO TOC-LOOP.                                              00008290
00830  EJECT                                                            00008300
00831  REAL-PRT-HDNG.                                                   00008310
00832      MOVE HD4 TO P-DATA.                                          00008320
00833      MOVE '1' TO P-CTL.                                           00008330
00834                                                                   00008340
00835  PRINT-RTN-2.                                                     00008350
00836      IF FICHE                                                     00008360
00837          WRITE FICH-REC FROM PRT                                  00008370
00838          ELSE                                                     00008380
00839          WRITE PRT AFTER POSITIONING P-CTL.                       00008390
00840                                                                   00008400
00841      MOVE SPACES TO PRT.                                          00008410
00842                                                                   00008420
00843      ADD K1 TO LN-CT.                                             00008430
00844                                                                   00008440
00845  R-P-H-2.                                                         00008450
00846      MOVE HD5 TO P-DATA.                                          00008460
00847                                                                   00008470
00848      PERFORM PRINT-RTN-2.                                         00008480
00849                                                                   00008490
00850      MOVE '-' TO P-CTL.                                           00008500
00851                                                                   00008510
00852      PERFORM PRINT-RTN-2.                                         00008520
00853                                                                   00008530
00854      MOVE K0 TO LN-CT.                                            00008540
00855                                                                   00008550
00856  R-P-H-X.                                                         00008560
00857      EXIT.                                                        00008570
00858                                                                   00008580
00859  END-TOC.                                                         00008590
00860      GO TO PRINT-READ.                                            00008600
00861                                                                   00008610
00862  INSERT-AST.                                                      00008620
00863      MOVE INDX-PG TO PRT-2-PGNO.                                  00008630
00864      MOVE B70 TO X2.                                              00008640
00865      MOVE B69 TO X1.                                              00008650
00866                                                                   00008660
00867  AST-LOOP.                                                        00008670
00868      IF PRT-2-TD (X1) NOT = ' '                                   00008680
00869          MOVE ' ' TO PRT-2-TD (X2),                               00008690
00870              GO TO A-LP-X.                                        00008700
00871                                                                   00008710
00872      MOVE '.' TO PRT-2-TD (X1).                                   00008720
00873                                                                   00008730
00874      SUBTRACT B1 FROM X1.                                         00008740
00875      SUBTRACT B1 FROM X2.                                         00008750
00876                                                                   00008760
00877      IF X1 NOT = B1                                               00008770
00878          GO TO AST-LOOP.                                          00008780
00879                                                                   00008790
00880  A-LP-X.                                                          00008800
00881      EXIT.                                                        00008810
00882                                                                   00008820
00883  A-LP-XX.                                                         00008830
00884      GO TO TOC-PRNT.                                              00008840
00885                                                                   00008850
00886  PRINT-INDEX.                                                     00008860
00887      MOVE SPACES TO HD-CNT.                                       00008870
00888      MOVE 'INDEX' TO HD-TOC.                                      00008880
00889                                                                   00008890
00890      PERFORM REAL-PRT-HDNG THRU R-P-H-X.                          00008900
00891                                                                   00008910
00892      MOVE 'INDEX (CONTINUED)' TO HD-TOC.                          00008920
00893                                                                   00008930
00894  INDEX-LOOP.                                                      00008940
00895      READ CNTR-FILE-R AT END GO TO END-JOB.                       00008950
00896                                                                   00008960
00897      IF LN-CT GREATER LN-MX                                       00008970
00898          PERFORM REAL-PRT-HDNG THRU R-P-H-X.                      00008980
00899                                                                   00008990
00900      IF CNTR-NAME-R = SAVE-TOC                                    00009000
00901              GO TO ADDL-PAGES.                                    00009010
00902                                                                   00009020
00903  INDEX-PRINT.                                                     00009030
00904      PERFORM PRINT-RTN-2.                                         00009040
00905                                                                   00009050
00906      IF LN-CT GREATER LN-MX                                       00009060
00907          PERFORM REAL-PRT-HDNG THRU R-P-H-X.                      00009070
00908                                                                   00009080
00909      MOVE CNTR-NAME-R TO PRT-2-DTL, SAVE-TOC.                     00009090
00910      MOVE CNTR-PG-R TO PRT-2-PGNO, SAVE-PAGE.                     00009100
00911      MOVE B70 TO X2.                                              00009110
00912      MOVE B69 TO X1.                                              00009120
00913                                                                   00009130
00914      PERFORM AST-LOOP THRU A-LP-X.                                00009140
00915                                                                   00009150
00916      GO TO INDEX-LOOP.                                            00009160
00917                                                                   00009170
00918  ADDL-PAGES.                                                      00009180
00919      IF SAVE-PAGE = CNTR-PG-R                                     00009190
00920               GO TO INDEX-LOOP.                                   00009200
00921                                                                   00009210
00922      IF PRT-2A-PGNO = '.....'                                     00009220
00923           MOVE CNTR-PG-R TO PRT-2A-PGNO, SAVE-PAGE                00009230
00924           GO TO INDEX-LOOP.                                       00009240
00925                                                                   00009250
00926      IF PRT-2B-PGNO = '.....'                                     00009260
00927           MOVE CNTR-PG-R TO PRT-2B-PGNO, SAVE-PAGE                00009270
00928           GO TO INDEX-LOOP.                                       00009280
00929                                                                   00009290
00930      IF PRT-2C-PGNO = '.....'                                     00009300
00931           MOVE CNTR-PG-R TO PRT-2C-PGNO, SAVE-PAGE                00009310
00932           GO TO INDEX-LOOP.                                       00009320
00933                                                                   00009330
00934      IF PRT-2D-PGNO = '.....'                                     00009340
00935           MOVE CNTR-PG-R TO PRT-2D-PGNO, SAVE-PAGE                00009350
00936           GO TO INDEX-LOOP.                                       00009360
00937                                                                   00009370
00938      MOVE CNTR-PG-R TO PRT-2E-PGNO.                               00009380
00939      MOVE SPACES TO SAVE-TOC.                                     00009390
00940      MOVE +0 TO SAVE-PAGE.                                        00009400
00941                                                                   00009410
00942      GO TO INDEX-LOOP.                                            00009420
00943                                                                   00009430
00944  KEY-WORD-XTRCT.                                                  00009440
00945      MOVE B1 TO X1.                                               00009450
00946                                                                   00009460
00947  K-W-SEARCH-AGAIN.                                                00009470
00948      MOVE SPACES TO CNTR-REC                                      00009480
00949                     WS-INDX.                                      00009490
00950                                                                   00009500
00951      COMPUTE CNTR-PG = PG-NO - K1.                                00009510
00952                                                                   00009520
00953      MOVE WSD-DESC TO I-I-A.                                      00009530
00954      MOVE B1 TO X3.                                               00009540
00955                                                                   00009550
00956  K-W-LOOP.                                                        00009560
00957      IF I-A (X1) = ':'                                            00009570
00958          GO TO K-W-FOUND.                                         00009580
00959                                                                   00009590
00960      ADD B1 TO X1.                                                00009600
00961                                                                   00009610
00962      IF X1 LESS THAN B70                                          00009620
00963          GO TO K-W-LOOP.                                          00009630
00964                                                                   00009640
00965      GO TO K-W-X.                                                 00009650
00966                                                                   00009660
00967  K-W-FOUND.                                                       00009670
00968      ADD B1 TO X1.                                                00009680
00969                                                                   00009690
00970  K-W-MVE.                                                         00009700
00971      MOVE I-A (X1) TO WS-INDX-O (X3).                             00009710
00972                                                                   00009720
00973      ADD B1 TO X1.                                                00009730
00974      ADD B1 TO X3.                                                00009740
00975                                                                   00009750
00976      IF X3 = +74                                                  00009760
00977          GO TO K-W-X.                                             00009770
00978                                                                   00009780
00979      IF I-A (X1) NOT = ':'                                        00009790
00980          GO TO K-W-MVE.                                           00009800
00981                                                                   00009810
00982      GO TO K-W-END.                                               00009820
00983                                                                   00009830
00984  K-W-END.                                                         00009840
00985      PERFORM B-I-END THRU B-I-X.                                  00009850
00986                                                                   00009860
00987      ADD B1 TO X1.                                                00009870
00988                                                                   00009880
00989      IF X1 LESS B70                                               00009890
00990          GO TO K-W-SEARCH-AGAIN.                                  00009900
00991                                                                   00009910
00992  K-W-X.                                                           00009920
00993      EXIT.                                                        00009930
00994  EJECT                                                            00009940
00995  END-JOB.                                                         00009950
00996      PERFORM PRINT-RTN-2.                                         00009960
00997                                                                   00009970
00998      CLOSE INDX-FILE                                              00009980
00999            CNTR-FILE-R                                            00009990
01000            WORK-FILE                                              00010000
01001            PRNT-FILE.                                             00010010
01002                                                                   00010020
01003      IF MANUAL-NAME NOT = SPACES                                  00010030
01004         CLOSE DOCU-TAPE.                                          00010040
01005                                                                   00010050
01006      IF FICHE                                                     00010060
01007         CLOSE FICH.                                               00010070
01008                                                                   00010080
01009      GOBACK.                                                      00010090
01010                                                                   00010100
01011  SCAN-FOR-MANUAL.                                                 00010110
01012      PERFORM READ-TAPE.                                           00010120
01013      IF DOC-ID NOT = ' *MANUAL* ' GO TO SCAN-FOR-MANUAL.          00010130
01014 *    IF MANUAL-NAME = 'ALL'                                       00010140
01015 *       PERFORM READ-TAPE                                         00010150
01016 *          GO TO HSK-2.                                           00010160
01017                                                                   00010170
01018      IF DOC-MANUAL NOT = MANUAL-NAME                              00010180
01019         GO TO SCAN-FOR-MANUAL.                                    00010190
01020      PERFORM READ-TAPE.                                           00010200
01021      GO TO HSK-2.                                                 00010210
01022 *                                                                 00010220
01023 *   END OF PGM ECS201                                             00010230
