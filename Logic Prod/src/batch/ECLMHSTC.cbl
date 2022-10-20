00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 ECLMHSTC.                            00000030
00004 *                            VMOD=2.001.                          00000031
00005                                                                   00000050
00006  AUTHOR.     LOGIC, INC.                                          00000060
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
00021 *REMARKS.                                                         00000210
00022 *        THIS PROGRAM CONVERTS THE NAME ALTERNATE INDEX ON THE    00000220
00023 *    CLAIM MASTER RECORD.                                         00000230
00024                                                                   00000240
00025      EJECT                                                        00000250
00026  ENVIRONMENT DIVISION.                                            00000260
00027                                                                   00000270
00028  INPUT-OUTPUT SECTION.                                            00000280
00029                                                                   00000290
00030  FILE-CONTROL.                                                    00000300
00031                                                                   00000310
00032      SELECT HISTORY-INPUT-FILE                                    00000320
00033          ASSIGN TO SYS010-UT-2400-S-SYS010.                       00000330
00034                                                                   00000340
00035      SELECT HISTORY-OUTPUT-FILE                                   00000350
00036          ASSIGN TO SYS011-UT-2400-S-SYS011.                       00000360
00037                                                                   00000370
00038      EJECT                                                        00000380
00039  DATA DIVISION.                                                   00000390
00040                                                                   00000400
00041  FILE SECTION.                                                    00000410
00042                                                                   00000420
00043  FD  HISTORY-INPUT-FILE          COPY ELCHAF.                     00000430
00044                                                                   00000440
00045      EJECT                                                        00000450
00046  FD  HISTORY-OUTPUT-FILE         COPY ELCHAF REPLACING            00000460
00047      HISTORY-INPUT-RECORD        BY  HISTORY-OUTPUT-RECORD        00000470
00048      HISTORY-INPUT-CLAIM-RECORD  BY  HISTORY-OUTPUT-CLAIM-RECORD  00000480
00049      HISTORY-INPUT-CERT-RECORD   BY  HISTORY-OUTPUT-CERT-RECORD   00000490
00050      HISTORY-INPUT-TRAILER-RECORD BY HISTORY-OUTPUT-TRAILER-RECORD00000500
00051      HISTORY-INPUT-LETTER-RECORD BY  HISTORY-OUTPUT-LETTER-RECORD 00000510
00052      HIR-COMPANY-ID              BY  HOR-COMPANY-ID               00000520
00053      HIR-CLAIM-KEY               BY  HOR-CLAIM-KEY                00000530
00054      HIR-DATE-ARCHIVED           BY  HOR-DATE-ARCHIVED            00000540
00055      HIR-PURGED-CLAIM            BY  HOR-PURGED-CLAIM             00000550
00056      HIR-RECORD-ID               BY  HOR-RECORD-ID                00000560
00057      HIR-CLAIM-RECORD            BY  HOR-CLAIM-RECORD             00000570
00058      HIR-CERTIFICATE-RECORD      BY  HOR-CERTIFICATE-RECORD       00000580
00059      HIR-ACTIVITY-TRAILER-RECORD BY  HOR-ACTIVITY-TRAILER-RECORD  00000590
00060      HIR-LETTER-ARCHIVE-RECORD   BY  HOR-LETTER-ARCHIVE-RECORD.   00000600
00061                                                                   00000610
00062  01  HISTORY-OUTPUT-RECORD2          PIC X(480).                  00000620
00063                                                                   00000630
00064      EJECT                                                        00000640
00065  WORKING-STORAGE SECTION.                                         00000650
00066                                                                   00000660
00067  77  FILLER  PIC X(32)   VALUE '********************************'.00000670
00068  77  FILLER  PIC X(32)   VALUE '*   ECLMHSTC WORKING STORAGE   *'.00000680
00069  77  FILLER   PIC X(32) VALUE  '******** VMOD=2.001 ************'.00000690
00070                                                                   00000700
00071  01  FILLER.                                                      00000710
00072      12  CLAIM-IN-CNT                PIC S9(07) COMP-3 VALUE +0.  00000720
00073      12  CLAIM-OUT-CNT               PIC S9(07) COMP-3 VALUE +0.  00000730
00074      12  HISTORY-IN-CNT              PIC S9(07) COMP-3 VALUE +0.  00000740
00075      12  HISTORY-OUT-CNT             PIC S9(07) COMP-3 VALUE +0.  00000750
00076      12  WS-ZERO                     PIC S9 COMP-3 VALUE +0.      00000760
00077      12  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.  00000770
00078      12  WS-ABEND-FILE-STATUS        PIC X(02)     VALUE SPACES.  00000780
00079      12  WS-RETURN-CODE              PIC S9(04) COMP VALUE +0.    00000790
00080      EJECT                                                        00000800
00081                           COPY ELCMSTR.                           00000810
00082      EJECT                                                        00000820
00083                                                                   00000830
00084 ******************************************************************00000840
00085 *                                                                *00000850
00086 *                            ELCMSTR.                            *00000860
00087 *                            VMOD=2.003                          *00000870
00088 *                                                                *00000880
00089 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *00000890
00090 *                                                                *00000900
00091 *   FILE TYPE = VSAM,KSDS                                        *00000910
00092 *   RECORD SIZE = 270  RECFORM = FIXED                           *00000920
00093 *                                                                *00000930
00094 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *00000940
00095 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=16  *00000950
00096 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=38,LEN=12  *00000960
00097 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=50,LEN=12  *00000970
00098 *                                                                *00000980
00099 *   LOG = YES                                                    *00000990
00100 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *00001000
00101 ******************************************************************00001010
00102  01  OLD-CLAIM-MASTER.                                            00001020
00103      12  CL-RECORD-ID                PIC XX.                      00001030
00104          88  VALID-CL-ID         VALUE 'CL'.                      00001040
00105                                                                   00001050
00106      12  CL-CONTROL-PRIMARY.                                      00001060
00107          16  CL-COMPANY-CD           PIC X.                       00001070
00108          16  CL-CARRIER              PIC X.                       00001080
00109          16  CL-CLAIM-NO             PIC X(7).                    00001090
00110          16  CL-CERT-NO.                                          00001100
00111              20  CL-CERT-PRIME       PIC X(10).                   00001110
00112              20  CL-CERT-SFX         PIC X.                       00001120
00113                                                                   00001130
00114      12  CL-CONTROL-BY-NAME.                                      00001140
00115          16  CL-COMPANY-CD-A1        PIC X.                       00001150
00116          16  CL-INSURED-LAST-NAME    PIC X(15).                   00001160
00117                                                                   00001170
00118      12  CL-CONTROL-BY-SSN.                                       00001180
00119          16  CL-COMPANY-CD-A2        PIC X.                       00001190
00120          16  CL-SOC-SEC-NO.                                       00001200
00121              20  CL-SSN-STATE        PIC XX.                      00001210
00122              20  CL-SSN-ACCOUNT      PIC X(6).                    00001220
00123              20  CL-SSN-LN3          PIC X(3).                    00001230
00124                                                                   00001240
00125      12  CL-CONTROL-BY-CERT-NO.                                   00001250
00126          16  CL-COMPANY-CD-A4        PIC X.                       00001260
00127          16  CL-CERT-NO-A4.                                       00001270
00128              20  CL-CERT-A4-PRIME    PIC X(10).                   00001280
00129              20  CL-CERT-A4-SFX      PIC X.                       00001290
00130                                                                   00001300
00131      12  CL-INSURED-PROFILE-DATA.                                 00001310
00132          16  CL-INSURED-NAME.                                     00001320
00133              20  CL-INSURED-1ST-NAME PIC X(12).                   00001330
00134              20  CL-INSURED-MID-INIT PIC X.                       00001340
00135          16  CL-INSURED-BIRTH-DT     PIC XX.                      00001350
00136          16  CL-INSURED-SEX-CD       PIC X.                       00001360
00137              88  INSURED-IS-MALE        VALUE 'M'.                00001370
00138              88  INSURED-IS-FEMALE      VALUE 'F'.                00001380
00139              88  INSURED-SEX-UNKNOWN    VALUE ' '.                00001390
00140          16  CL-INSURED-OCC-CD       PIC XX.                      00001400
00141          16  FILLER                  PIC X(5).                    00001410
00142                                                                   00001420
00143      12  CL-PROCESSING-INFO.                                      00001430
00144          16  CL-PROCESSOR-ID         PIC X(4).                    00001440
00145          16  CL-CLAIM-STATUS         PIC X.                       00001450
00146              88  CLAIM-IS-OPEN          VALUE 'O'.                00001460
00147              88  CLAIM-IS-CLOSED        VALUE 'C'.                00001470
00148          16  CL-CLAIM-TYPE           PIC X.                       00001480
00149 *            88  AH-CLAIM               VALUE 'A'.                00001490
00150 *            88  LIFE-CLAIM             VALUE 'L'.                00001500
00151 *            88  PROPERTY-CLAIM         VALUE 'P'.                00001510
00152 *            88  UNEMPLOYMENT-CLAIM     VALUE 'U'.                00001520
00153          16  CL-CLAIM-PREM-TYPE      PIC X.                       00001530
00154              88  SINGLE-PREMIUM         VALUE '1'.                00001540
00155              88  O-B-COVERAGE           VALUE '2'.                00001550
00156              88  OPEN-END-COVERAGE      VALUE '3'.                00001560
00157          16  CL-INCURRED-DT          PIC XX.                      00001570
00158          16  CL-REPORTED-DT          PIC XX.                      00001580
00159          16  CL-FILE-ESTABLISH-DT    PIC XX.                      00001590
00160          16  CL-EST-END-OF-DISAB-DT  PIC XX.                      00001600
00161          16  CL-LAST-PMT-DT          PIC XX.                      00001610
00162          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.        00001620
00163          16  CL-PAID-THRU-DT         PIC XX.                      00001630
00164          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.        00001640
00165          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.        00001650
00166          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.          00001660
00167          16  CL-PMT-CALC-METHOD      PIC X.                       00001670
00168              88  CL-360-DAY-YR          VALUE '1'.                00001680
00169              88  CL-365-DAY-YR          VALUE '2'.                00001690
00170              88  CL-FULL-MONTHS         VALUE '3'.                00001700
00171          16  CL-CAUSE-CD             PIC X(6).                    00001710
00172                                                                   00001720
00173          16  CL-PRIME-CERT-NO.                                    00001730
00174              20  CL-PRIME-CERT-PRIME PIC X(10).                   00001740
00175              20  CL-PRIME-CERT-SFX   PIC X.                       00001750
00176                                                                   00001760
00177          16  FILLER                  PIC X(12).                   00001770
00178                                                                   00001780
00179          16  CL-PROG-FORM-TYPE       PIC X.                       00001790
00180          16  CL-LAST-ADD-ON-DT       PIC XX.                      00001800
00181                                                                   00001810
00182          16  CL-LAST-REOPEN-DT       PIC XX.                      00001820
00183          16  CL-LAST-CLOSE-DT        PIC XX.                      00001830
00184          16  CL-LAST-CLOSE-REASON    PIC X.                       00001840
00185              88  FINAL-PAID             VALUE '1'.                00001850
00186              88  CLAIM-DENIED           VALUE '2'.                00001860
00187              88  AUTO-CLOSE             VALUE '3'.                00001870
00188              88  MANUAL-CLOSE           VALUE '4'.                00001880
00189          16  CL-ASSOC-CERT-SEQU      PIC S99.                     00001890
00190          16  CL-ASSOC-CERT-TOTAL     PIC S99.                     00001900
00191          16  CL-CLAIM-PAYMENT-STATUS PIC 9.                       00001910
00192              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.           00001920
00193          16  FILLER                  PIC X(05).                   00001930
00194                                                                   00001940
00195      12  CL-CERTIFICATE-DATA.                                     00001950
00196          16  CL-CERT-ORIGIN          PIC X.                       00001960
00197              88  CERT-WAS-ONLINE        VALUE '1'.                00001970
00198              88  CERT-WAS-CREATED       VALUE '2'.                00001980
00199              88  COVERAGE-WAS-ADDED     VALUE '3'.                00001990
00200          16  CL-CERT-KEY-DATA.                                    00002000
00201              20  CL-CERT-CARRIER     PIC X.                       00002010
00202              20  CL-CERT-GROUPING    PIC X(6).                    00002020
00203              20  CL-CERT-STATE       PIC XX.                      00002030
00204              20  CL-CERT-ACCOUNT.                                 00002040
00205                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).             00002050
00206                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).             00002060
00207              20  CL-CERT-EFF-DT      PIC XX.                      00002070
00208                                                                   00002080
00209      12  CL-STATUS-CONTROLS.                                      00002090
00210          16  CL-PRIORITY-CD          PIC X.                       00002100
00211              88  HIGHEST-PRIORITY       VALUE '9'.                00002110
00212          16  CL-SUPV-ATTN-CD         PIC X.                       00002120
00213              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.            00002130
00214              88  SUPV-IS-REQUIRED       VALUE 'Y'.                00002140
00215          16  CL-PURGED-DT            PIC XX.                      00002150
00216          16  CL-RESTORED-DT          PIC XX.                      00002160
00217          16  CL-NEXT-AUTO-PAY-DT     PIC XX.                      00002170
00218          16  CL-NEXT-RESEND-DT       PIC XX.                      00002180
00219          16  CL-NEXT-FOLLOWUP-DT     PIC XX.                      00002190
00220          16  FILLER                  PIC XX.                      00002200
00221          16  CL-LAST-MAINT-DT        PIC XX.                      00002210
00222          16  CL-LAST-MAINT-USER      PIC X(4).                    00002220
00223          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.        00002230
00224          16  CL-LAST-MAINT-TYPE      PIC X.                       00002240
00225              88  CLAIM-SET-UP           VALUE ' '.                00002250
00226              88  PAYMENT-MADE           VALUE '1'.                00002260
00227              88  LETTER-SENT            VALUE '2'.                00002270
00228              88  MASTER-WAS-ALTERED     VALUE '3'.                00002280
00229              88  MASTER-WAS-RESTORED    VALUE '4'.                00002290
00230              88  INCURRED-DATE-CHANGED  VALUE '5'.                00002300
00231              88  FILE-CONVERTED         VALUE '6'.                00002310
00232          16  CL-RELATED-CLAIM-NO     PIC X(7).                    00002320
00233          16  CL-HISTORY-ARCHIVE-DT   PIC XX.                      00002330
00234          16  CL-BENEFICIARY          PIC X(10).                   00002340
00235                                                                   00002350
00236      12  CL-TRAILER-CONTROLS.                                     00002360
00237          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.          00002370
00238              88  CL-1ST-TRL-AVAIL       VALUE +4095.              00002380
00239              88  CL-LAST-TRL-AVAIL      VALUE +100.               00002390
00240              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.                 00002400
00241          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.          00002410
00242          16  FILLER                  PIC XX.                      00002420
00243          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.          00002430
00244          16  CL-ADDRESS-TRAILER-CNT.                              00002440
00245              20  CL-INSURED-ADDR-CNT  PIC S9(1).                  00002450
00246                  88  NO-INSURED-AVAILABLE    VALUE ZERO.          00002460
00247              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).                  00002470
00248                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.          00002480
00249              20  CL-BENIF-ADDR-CNT    PIC S9(1).                  00002490
00250                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.          00002500
00251              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).                  00002510
00252                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.          00002520
00253              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).                  00002530
00254                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.          00002540
00255              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).                  00002550
00256                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.          00002560
00257              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).                  00002570
00258                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.          00002580
00259                                                                   00002590
00260      12  FILLER                      PIC X(7).                    00002600
00261                                                                   00002610
00262      12  CL-FILE-LOCATION            PIC X(4).                    00002620
00263                                                                   00002630
00264      12  CL-PROCESS-ERRORS.                                       00002640
00265          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.          00002650
00266              88  NO-FATAL-ERRORS        VALUE ZERO.               00002660
00267          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.          00002670
00268              88  NO-FORCABLE-ERRORS     VALUE ZERO.               00002680
00269                                                                   00002690
00270      12  FILLER                      PIC X(8).                    00002700
00271                                                                   00002710
00272 ******************************************************************00002720
00273  PROCEDURE DIVISION.                                              00002730
00274                                                                   00002740
00275  0000-OPEN-FILES.                                                 00002750
00276                                                                   00002760
00277      OPEN INPUT HISTORY-INPUT-FILE                                00002770
00278          OUTPUT HISTORY-OUTPUT-FILE.                              00002780
00279                                                                   00002790
00280  1000-READ-HISTORY-FILE.                                          00002800
00281                                                                   00002810
00282      READ HISTORY-INPUT-FILE AT END                               00002820
00283         GO TO 9999-END-JOB.                                       00002830
00284                                                                   00002840
00285      ADD +1 TO HISTORY-IN-CNT.                                    00002850
00286                                                                   00002860
00287      IF HIR-RECORD-ID NOT EQUAL 'CL'                              00002870
00288         GO TO 5000-WRITE-HISTORY-FILE.                            00002880
00289                                                                   00002890
00290      ADD +1                  TO CLAIM-IN-CNT.                     00002900
00291      MOVE HIR-CLAIM-RECORD   TO OLD-CLAIM-MASTER.                 00002910
00292                                                                   00002920
00293      MOVE SPACES             TO CLAIM-MASTER.                     00002930
00294                                                                   00002940
00295      MOVE 'CL'               TO CL-RECORD-ID                      00002950
00296                              OF CLAIM-MASTER.                     00002960
00297                                                                   00002970
00298      MOVE CL-CONTROL-PRIMARY OF OLD-CLAIM-MASTER                  00002980
00299                              TO CL-CONTROL-PRIMARY                00002990
00300                              OF CLAIM-MASTER.                     00003000
00301                                                                   00003010
00302      MOVE CL-INSURED-LAST-NAME                                    00003020
00303                              OF OLD-CLAIM-MASTER                  00003030
00304                              TO CL-INSURED-LAST-NAME              00003040
00305                              OF CLAIM-MASTER.                     00003050
00306                                                                   00003060
00307      MOVE CL-COMPANY-CD-A1   OF OLD-CLAIM-MASTER                  00003070
00308                              TO CL-COMPANY-CD-A1                  00003080
00309                              OF CLAIM-MASTER.                     00003090
00310                                                                   00003100
00311      MOVE CL-INSURED-NAME    OF OLD-CLAIM-MASTER                  00003110
00312                              TO CL-INSURED-NAME                   00003120
00313                              OF CLAIM-MASTER.                     00003130
00314                                                                   00003140
00315      MOVE CL-CONTROL-BY-SSN  OF OLD-CLAIM-MASTER                  00003150
00316                              TO CL-CONTROL-BY-SSN                 00003160
00317                              OF CLAIM-MASTER.                     00003170
00318                                                                   00003180
00319      MOVE CL-CONTROL-BY-CERT-NO                                   00003190
00320                              OF OLD-CLAIM-MASTER                  00003200
00321                              TO CL-CONTROL-BY-CERT-NO             00003210
00322                              OF CLAIM-MASTER.                     00003220
00323                                                                   00003230
00324      MOVE CL-INSURED-BIRTH-DT                                     00003240
00325                              OF OLD-CLAIM-MASTER                  00003250
00326                              TO CL-INSURED-BIRTH-DT               00003260
00327                              OF CLAIM-MASTER.                     00003270
00328                                                                   00003280
00329      MOVE CL-INSURED-SEX-CD  OF OLD-CLAIM-MASTER                  00003290
00330                              TO CL-INSURED-SEX-CD                 00003300
00331                              OF CLAIM-MASTER.                     00003310
00332                                                                   00003320
00333      MOVE CL-INSURED-OCC-CD  OF OLD-CLAIM-MASTER                  00003330
00334                              TO CL-INSURED-OCC-CD                 00003340
00335                              OF CLAIM-MASTER.                     00003350
00336                                                                   00003360
00337      MOVE CL-PROCESSING-INFO OF OLD-CLAIM-MASTER                  00003370
00338                              TO CL-PROCESSING-INFO                00003380
00339                              OF CLAIM-MASTER.                     00003390
00340                                                                   00003400
00341      MOVE CL-CERTIFICATE-DATA                                     00003410
00342                              OF OLD-CLAIM-MASTER                  00003420
00343                              TO CL-CERTIFICATE-DATA               00003430
00344                              OF CLAIM-MASTER.                     00003440
00345                                                                   00003450
00346      MOVE CL-STATUS-CONTROLS OF OLD-CLAIM-MASTER                  00003460
00347                              TO CL-STATUS-CONTROLS                00003470
00348                              OF CLAIM-MASTER.                     00003480
00349                                                                   00003490
00350      MOVE CL-TRAILER-CONTROLS                                     00003500
00351                              OF OLD-CLAIM-MASTER                  00003510
00352                              TO CL-TRAILER-CONTROLS               00003520
00353                              OF CLAIM-MASTER.                     00003530
00354                                                                   00003540
00355      MOVE CL-FILE-LOCATION   OF OLD-CLAIM-MASTER                  00003550
00356                              TO CL-FILE-LOCATION                  00003560
00357                              OF CLAIM-MASTER.                     00003570
00358                                                                   00003580
00359      MOVE CL-PROCESS-ERRORS  OF OLD-CLAIM-MASTER                  00003590
00360                              TO CL-PROCESS-ERRORS                 00003600
00361                              OF CLAIM-MASTER.                     00003610
00362                                                                   00003620
00363      MOVE CLAIM-MASTER    TO HIR-CLAIM-RECORD.                    00003630
00364                                                                   00003640
00365      EJECT                                                        00003650
00366  5000-WRITE-HISTORY-FILE.                                         00003660
00367                                                                   00003670
00368      MOVE HISTORY-INPUT-RECORD TO HISTORY-OUTPUT-RECORD.          00003680
00369                                                                   00003690
00370      IF HOR-RECORD-ID EQUAL 'AT'                                  00003700
00371         ADD +1 TO HISTORY-OUT-CNT                                 00003710
00372         WRITE HISTORY-OUTPUT-TRAILER-RECORD                       00003720
00373      ELSE                                                         00003730
00374      IF HOR-RECORD-ID EQUAL 'LA'                                  00003740
00375         ADD +1 TO HISTORY-OUT-CNT                                 00003750
00376         WRITE HISTORY-OUTPUT-LETTER-RECORD                        00003760
00377      ELSE                                                         00003770
00378      IF HOR-RECORD-ID EQUAL 'CM'                                  00003780
00379         ADD +1 TO HISTORY-OUT-CNT                                 00003790
00380         WRITE HISTORY-OUTPUT-CERT-RECORD                          00003800
00381      ELSE                                                         00003810
00382      IF HOR-RECORD-ID EQUAL 'CL'                                  00003820
00383         ADD +1 TO HISTORY-OUT-CNT                                 00003830
00384         ADD +1                  TO CLAIM-OUT-CNT                  00003840
00385         WRITE HISTORY-OUTPUT-CLAIM-RECORD.                        00003850
00386                                                                   00003860
00387      GO TO 1000-READ-HISTORY-FILE.                                00003870
00388                                                                   00003880
00389      EJECT                                                        00003890
00390  9999-END-JOB.                                                    00003900
00391                                                                   00003910
00392      CLOSE HISTORY-INPUT-FILE                                     00003920
00393           HISTORY-OUTPUT-FILE.                                    00003930
00394                                                                   00003940
00395      DISPLAY '*******************************************'.       00003950
00396      DISPLAY ' HISTORY IN     ...... ' HISTORY-IN-CNT             00003960
00397      DISPLAY ' HISTORY OUT    ...... ' HISTORY-OUT-CNT            00003970
00398      DISPLAY ' CLAIMS  IN     ...... ' CLAIM-IN-CNT               00003980
00399      DISPLAY ' CLAIMS  OUT    ...... ' CLAIM-OUT-CNT              00003990
00400      DISPLAY '*******************************************'.       00004000
00401                                                                   00004010
00402      GOBACK.                                                      00004020
00403                                                                   00004030
