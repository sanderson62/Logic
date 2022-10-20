00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 ELCLHCNV.                            00000030
00004 *                            VMOD=2.001.                          00000031
00005                                                                   00000050
00006  AUTHOR.        LOGIC, INC.                                       00000060
00007                 DALLAS, TEXAS.                                    00000070
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
00020 *REMARKS.                                                         00000200
00021 *          CONVERSION OF CLAIMS FILES REQUIRING CHANGES.          00000210
00022                                                                   00000220
00023  ENVIRONMENT DIVISION.                                            00000230
00024  INPUT-OUTPUT SECTION.                                            00000240
00025  FILE-CONTROL.                                                    00000250
00026                                                                   00000260
00027      SELECT  HISTORY-IN      ASSIGN TO SYS010-UT-2400-S-SYS010.   00000270
00028      SELECT  HISTORY-OT      ASSIGN TO SYS011-UT-2400-S-SYS011.   00000280
00029                                                                   00000290
00030                                                                   00000300
00031      EJECT                                                        00000310
00032  DATA DIVISION.                                                   00000320
00033  FILE SECTION.                                                    00000330
00034                                                                   00000340
00035      EJECT                                                        00000350
00036  FD  HISTORY-IN                  COPY ELCHAF.                     00000360
00037                                                                   00000370
00038      EJECT                                                        00000380
00039  FD  HISTORY-OT          COPY ELCHAF REPLACING                    00000390
00040      HISTORY-INPUT-RECORD        BY  HISTORY-OUTPUT-RECORD        00000400
00041      HISTORY-INPUT-CLAIM-RECORD  BY  HISTORY-OUTPUT-CLAIM-RECORD  00000410
00042      HISTORY-INPUT-CERT-RECORD   BY  HISTORY-OUTPUT-CERT-RECORD   00000420
00043      HISTORY-INPUT-TRAILER-RECORD BY HISTORY-OUTPUT-TRAILER-RECORD00000430
00044      HISTORY-INPUT-LETTER-RECORD BY  HISTORY-OUTPUT-LETTER-RECORD 00000440
00045      HIR-COMPANY-ID              BY  HOR-COMPANY-ID               00000450
00046      HIR-CLAIM-KEY               BY  HOR-CLAIM-KEY                00000460
00047      HIR-DATE-ARCHIVED           BY  HOR-DATE-ARCHIVED            00000470
00048      HIR-PURGED-CLAIM            BY  HOR-PURGED-CLAIM             00000480
00049      HIR-RECORD-ID               BY  HOR-RECORD-ID                00000490
00050      HIR-CLAIM-RECORD            BY  HOR-CLAIM-RECORD             00000500
00051      HIR-CERTIFICATE-RECORD      BY  HOR-CERTIFICATE-RECORD       00000510
00052      HIR-ACTIVITY-TRAILER-RECORD BY  HOR-ACTIVITY-TRAILER-RECORD  00000520
00053      HIR-LETTER-ARCHIVE-RECORD   BY  HOR-LETTER-ARCHIVE-RECORD.   00000530
00054                                                                   00000540
00055      EJECT                                                        00000550
00056                                                                   00000560
00057  WORKING-STORAGE SECTION.                                         00000570
00058                                                                   00000580
00059  77  FILLER  PIC X(32)  VALUE '********************************'. 00000590
00060  77  FILLER  PIC X(32)  VALUE '* ELCLHCNV WORKING STORAGE     *'. 00000600
00061  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.001 **********'. 00000610
00062                                                                   00000620
00063  77  WS-LAST-RECORD-ID           PIC XX      VALUE SPACES.        00000630
00064  77  WS-LAST-TRLR-TYPE           PIC X       VALUE SPACES.        00000640
00065  77  WS-RUN-DATE                 PIC X(06).                       00000650
00066  77  WS-BIN-RUN-DATE             PIC X(02)   VALUE LOW-VALUES.    00000660
00067                                                                   00000670
00068  77  ABEND-CODE                  PIC X(4).                        00000680
00069  77  ABEND-OPTION                PIC X.                           00000690
00070  77  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.        00000700
00071  77  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.          00000710
00072  77  WS-CURRENT-DATE             PIC XX      VALUE ZERO.          00000720
00073  77  WS-RETURN-CODE              PIC S9(4)   VALUE ZERO.          00000730
00074  77  WS-ZERO                     PIC S9      VALUE ZERO.          00000740
00075                                                                   00000750
00076  01  MISC-TOTALS.                                                 00000760
00077      12  TOTAL-CL-RECORDS-IN         PIC 9(7)    VALUE ZERO.      00000770
00078      12  TOTAL-AT-RECORDS-IN         PIC 9(7)    VALUE ZERO.      00000780
00079      12  TOTAL-LA-RECORDS-IN         PIC 9(7)    VALUE ZERO.      00000790
00080      12  TOTAL-CM-RECORDS-IN         PIC 9(7)    VALUE ZERO.      00000800
00081      12  TOTAL-CL-RECORDS-OT         PIC 9(7)    VALUE ZERO.      00000810
00082      12  TOTAL-AT-RECORDS-OT         PIC 9(7)    VALUE ZERO.      00000820
00083      12  TOTAL-LA-RECORDS-OT         PIC 9(7)    VALUE ZERO.      00000830
00084      12  TOTAL-CM-RECORDS-OT         PIC 9(7)    VALUE ZERO.      00000840
00085      12  TOTAL-AT-RECORDS-DROP       PIC 9(7)    VALUE ZERO.      00000850
00086      12  TOTAL-LA-RECORDS-DROP       PIC 9(7)    VALUE ZERO.      00000860
00087      12  TOTAL-1-TRLRS               PIC 9(7)    VALUE ZEROS.     00000870
00088      12  TOTAL-2-TRLRS               PIC 9(7)    VALUE ZEROS.     00000880
00089      12  TOTAL-3-TRLRS               PIC 9(7)    VALUE ZEROS.     00000890
00090      12  TOTAL-4-TRLRS               PIC 9(7)    VALUE ZEROS.     00000900
00091      12  TOTAL-5-TRLRS               PIC 9(7)    VALUE ZEROS.     00000910
00092      12  TOTAL-6-TRLRS               PIC 9(7)    VALUE ZEROS.     00000920
00093      12  TOTAL-7-TRLRS               PIC 9(7)    VALUE ZEROS.     00000930
00094      12  TOTAL-8-TRLRS               PIC 9(7)    VALUE ZEROS.     00000940
00095      12  TOTAL-9-TRLRS               PIC 9(7)    VALUE ZEROS.     00000950
00096      12  TOTAL-A-TRLRS               PIC 9(7)    VALUE ZEROS.     00000960
00097      12  TOTAL-BAD-LA1-RECS          PIC 9(7)    VALUE ZEROS.     00000970
00098      12  TOTAL-BAD-LA4-RECS          PIC 9(7)    VALUE ZEROS.     00000980
00099                                                                   00000990
00100      12  G-CL-RECORDS-IN             PIC 9(7)    VALUE ZERO.      00001000
00101      12  G-AT-RECORDS-IN             PIC 9(7)    VALUE ZERO.      00001010
00102      12  G-LA-RECORDS-IN             PIC 9(7)    VALUE ZERO.      00001020
00103      12  G-CM-RECORDS-IN             PIC 9(7)    VALUE ZERO.      00001030
00104      12  G-CL-RECORDS-OT             PIC 9(7)    VALUE ZERO.      00001040
00105      12  G-AT-RECORDS-OT             PIC 9(7)    VALUE ZERO.      00001050
00106      12  G-LA-RECORDS-OT             PIC 9(7)    VALUE ZERO.      00001060
00107      12  G-CM-RECORDS-OT             PIC 9(7)    VALUE ZERO.      00001070
00108      12  G-AT-RECORDS-DROP           PIC 9(7)    VALUE ZERO.      00001080
00109      12  G-LA-RECORDS-DROP           PIC 9(7)    VALUE ZERO.      00001090
00110      12  G-TOTAL-1-TRLRS             PIC 9(7)    VALUE ZEROS.     00001100
00111      12  G-TOTAL-2-TRLRS             PIC 9(7)    VALUE ZEROS.     00001110
00112      12  G-TOTAL-3-TRLRS             PIC 9(7)    VALUE ZEROS.     00001120
00113      12  G-TOTAL-4-TRLRS             PIC 9(7)    VALUE ZEROS.     00001130
00114      12  G-TOTAL-5-TRLRS             PIC 9(7)    VALUE ZEROS.     00001140
00115      12  G-TOTAL-6-TRLRS             PIC 9(7)    VALUE ZEROS.     00001150
00116      12  G-TOTAL-7-TRLRS             PIC 9(7)    VALUE ZEROS.     00001160
00117      12  G-TOTAL-8-TRLRS             PIC 9(7)    VALUE ZEROS.     00001170
00118      12  G-TOTAL-9-TRLRS             PIC 9(7)    VALUE ZEROS.     00001180
00119      12  G-TOTAL-A-TRLRS             PIC 9(7)    VALUE ZEROS.     00001190
00120      12  G-TOTAL-BAD-LA1-RECS        PIC 9(7)    VALUE ZEROS.     00001200
00121      12  G-TOTAL-BAD-LA4-RECS        PIC 9(7)    VALUE ZEROS.     00001210
00122                                                                   00001220
00123  01  WS-HIR-CLAIM-KEY.                                            00001230
00124      12  WS-HIR-COMPANY-CD           PIC X.                       00001240
00125      12  WS-HIR-CARRIER              PIC X.                       00001250
00126      12  WS-HIR-CLAIM-NO             PIC X(7).                    00001260
00127      12  WS-HIR-CERT-NO.                                          00001270
00128          16  WS-HIR-CERT-PREFIX      PIC X(3).                    00001280
00129          16  WS-HIR-CERT-PRIME       PIC X(7).                    00001290
00130          16  WS-HIR-CERT-SFX         PIC X.                       00001300
00131                                                                   00001310
00132  01  WS-HOR-CLAIM-KEY.                                            00001320
00133      12  WS-HOR-COMPANY-CD           PIC X.                       00001330
00134      12  WS-HOR-CARRIER              PIC X.                       00001340
00135      12  WS-HOR-CLAIM-NO             PIC X(7).                    00001350
00136      12  WS-HOR-CERT-NO.                                          00001360
00137          16  WS-HOR-CERT-PREFIX      PIC X(3).                    00001370
00138          16  WS-HOR-CERT-PRIME       PIC X(7).                    00001380
00139          16  WS-HOR-CERT-SFX         PIC X.                       00001390
00140                                                                   00001400
00141      EJECT                                                        00001410
00142  01  OLD-CLAIM-MASTER.                                            00001420
00143      12  CL-RECORD-ID                PIC XX.                      00001430
00144          88  VALID-CL-ID         VALUE 'CL'.                      00001440
00145                                                                   00001450
00146      12  CL-CONTROL-PRIMARY.                                      00001460
00147          16  CL-COMPANY-CD           PIC X.                       00001470
00148          16  CL-CARRIER              PIC X.                       00001480
00149          16  CL-CLAIM-NO             PIC X(7).                    00001490
00150          16  CL-CERT-NO.                                          00001500
00151              20  CL-CERT-PRIME       PIC X(10).                   00001510
00152              20  CL-CERT-SFX         PIC X.                       00001520
00153                                                                   00001530
00154      12  CL-CONTROL-BY-NAME.                                      00001540
00155          16  CL-COMPANY-CD-A1        PIC X.                       00001550
00156          16  CL-INSURED-LAST-NAME    PIC X(15).                   00001560
00157                                                                   00001570
00158      12  CL-CONTROL-BY-SSN.                                       00001580
00159          16  CL-COMPANY-CD-A2        PIC X.                       00001590
00160          16  CL-SOC-SEC-NO.                                       00001600
00161              20  CL-SSN-STATE        PIC XX.                      00001610
00162              20  CL-SSN-ACCOUNT      PIC X(6).                    00001620
00163              20  CL-SSN-LN3          PIC X(3).                    00001630
00164                                                                   00001640
00165      12  CL-CONTROL-BY-CERT-NO.                                   00001650
00166          16  CL-COMPANY-CD-A4        PIC X.                       00001660
00167          16  CL-CERT-NO-A4.                                       00001670
00168              20  CL-CERT-A4-PRIME    PIC X(10).                   00001680
00169              20  CL-CERT-A4-SFX      PIC X.                       00001690
00170                                                                   00001700
00171      12  CL-INSURED-PROFILE-DATA.                                 00001710
00172          16  CL-INSURED-NAME.                                     00001720
00173              20  CL-INSURED-1ST-NAME PIC X(12).                   00001730
00174              20  CL-INSURED-MID-INIT PIC X.                       00001740
00175          16  CL-INSURED-BIRTH-DT     PIC XX.                      00001750
00176          16  CL-INSURED-SEX-CD       PIC X.                       00001760
00177              88  INSURED-IS-MALE        VALUE 'M'.                00001770
00178              88  INSURED-IS-FEMALE      VALUE 'F'.                00001780
00179              88  INSURED-SEX-UNKNOWN    VALUE ' '.                00001790
00180          16  CL-INSURED-OCC-CD       PIC XX.                      00001800
00181          16  FILLER                  PIC X(5).                    00001810
00182                                                                   00001820
00183      12  CL-PROCESSING-INFO.                                      00001830
00184          16  CL-PROCESSOR-ID         PIC X(4).                    00001840
00185          16  CL-CLAIM-STATUS         PIC X.                       00001850
00186              88  CLAIM-IS-OPEN          VALUE 'O'.                00001860
00187              88  CLAIM-IS-CLOSED        VALUE 'C'.                00001870
00188          16  CL-CLAIM-TYPE           PIC X.                       00001880
00189 *            88  AH-CLAIM               VALUE 'A'.                00001890
00190 *            88  LIFE-CLAIM             VALUE 'L'.                00001900
00191          16  CL-CLAIM-PREM-TYPE      PIC X.                       00001910
00192              88  SINGLE-PREMIUM         VALUE '1'.                00001920
00193              88  O-B-COVERAGE           VALUE '2'.                00001930
00194              88  OPEN-END-COVERAGE      VALUE '3'.                00001940
00195          16  CL-INCURRED-DT          PIC XX.                      00001950
00196          16  CL-REPORTED-DT          PIC XX.                      00001960
00197          16  CL-FILE-ESTABLISH-DT    PIC XX.                      00001970
00198          16  CL-EST-END-OF-DISAB-DT  PIC XX.                      00001980
00199          16  CL-LAST-PMT-DT          PIC XX.                      00001990
00200          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.        00002000
00201          16  CL-PAID-THRU-DT         PIC XX.                      00002010
00202          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.        00002020
00203          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.        00002030
00204          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.          00002040
00205          16  CL-PMT-CALC-METHOD      PIC X.                       00002050
00206              88  CL-360-DAY-YR          VALUE '1'.                00002060
00207              88  CL-365-DAY-YR          VALUE '2'.                00002070
00208              88  CL-FULL-MONTHS         VALUE '3'.                00002080
00209          16  CL-CAUSE-CD             PIC X(6).                    00002090
00210          16  CL-DIAGNOSIS-DESCRIP    PIC X(26).                   00002100
00211          16  CL-LAST-REOPEN-DT       PIC XX.                      00002110
00212          16  CL-LAST-CLOSE-DT        PIC XX.                      00002120
00213          16  CL-LAST-CLOSE-REASON    PIC X.                       00002130
00214              88  FINAL-PAID             VALUE '1'.                00002140
00215              88  CLAIM-DENIED           VALUE '2'.                00002150
00216              88  AUTO-CLOSE             VALUE '3'.                00002160
00217              88  MANUAL-CLOSE           VALUE '4'.                00002170
00218          16  FILLER                  PIC X(10).                   00002180
00219                                                                   00002190
00220      12  CL-CERTIFICATE-DATA.                                     00002200
00221          16  CL-CERT-ORIGIN          PIC X.                       00002210
00222              88  CERT-WAS-ONLINE        VALUE '1'.                00002220
00223              88  CERT-WAS-CREATED       VALUE '2'.                00002230
00224              88  COVERAGE-WAS-ADDED     VALUE '3'.                00002240
00225          16  CL-CERT-KEY-DATA.                                    00002250
00226              20  CL-CERT-CARRIER     PIC X.                       00002260
00227              20  CL-CERT-GROUPING    PIC X(6).                    00002270
00228              20  CL-CERT-STATE       PIC XX.                      00002280
00229              20  CL-CERT-ACCOUNT.                                 00002290
00230                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).             00002300
00231                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).             00002310
00232              20  CL-CERT-EFF-DT      PIC XX.                      00002320
00233                                                                   00002330
00234      12  CL-STATUS-CONTROLS.                                      00002340
00235          16  CL-PRIORITY-CD          PIC X.                       00002350
00236              88  HIGHEST-PRIORITY       VALUE '9'.                00002360
00237          16  CL-SUPV-ATTN-CD         PIC X.                       00002370
00238              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.            00002380
00239              88  SUPV-IS-REQUIRED       VALUE 'Y'.                00002390
00240          16  CL-PURGED-DT            PIC XX.                      00002400
00241          16  CL-RESTORED-DT          PIC XX.                      00002410
00242          16  CL-NEXT-AUTO-PAY-DT     PIC XX.                      00002420
00243          16  CL-NEXT-RESEND-DT       PIC XX.                      00002430
00244          16  CL-NEXT-FOLLOWUP-DT     PIC XX.                      00002440
00245          16  FILLER                  PIC XX.                      00002450
00246          16  CL-LAST-MAINT-DT        PIC XX.                      00002460
00247          16  CL-LAST-MAINT-USER      PIC X(4).                    00002470
00248          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.        00002480
00249          16  CL-LAST-MAINT-TYPE      PIC X.                       00002490
00250              88  CLAIM-SET-UP           VALUE ' '.                00002500
00251              88  PAYMENT-MADE           VALUE '1'.                00002510
00252              88  LETTER-SENT            VALUE '2'.                00002520
00253              88  MASTER-WAS-ALTERED     VALUE '3'.                00002530
00254              88  MASTER-WAS-RESTORED    VALUE '4'.                00002540
00255              88  INCURRED-DATE-CHANGED  VALUE '5'.                00002550
00256              88  FILE-CONVERTED         VALUE '6'.                00002560
00257          16  CL-RELATED-CLAIM-NO     PIC X(7).                    00002570
00258          16  CL-HISTORY-ARCHIVE-DT   PIC XX.                      00002580
00259          16  FILLER                  PIC X(10).                   00002590
00260                                                                   00002600
00261      12  CL-TRAILER-CONTROLS.                                     00002610
00262          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.          00002620
00263              88  CL-1ST-TRL-AVAIL       VALUE +4095.              00002630
00264              88  CL-LAST-TRL-AVAIL      VALUE +1.                 00002640
00265              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.                 00002650
00266          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.          00002660
00267          16  FILLER                  PIC XX.                      00002670
00268          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.          00002680
00269          16  CL-ADDRESS-TRAILER-SEQ.                              00002690
00270              20  CL-INSURED-ADDR-SEQ PIC S9(4)     COMP.          00002700
00271              20  CL-ACCOUNT-ADDR-SEQ PIC S9(4)     COMP.          00002710
00272                  88  ACCOUNT-IS-ONLINE  VALUE ZERO.               00002720
00273              20  CL-BENIF-ADDR-SEQ   PIC S9(4)     COMP.          00002730
00274              20  CL-EMPLOYER-ADDR-SEQ PIC S9(4)    COMP.          00002740
00275              20  CL-DOCTOR-ADDR-SEQ  PIC S9(4)     COMP.          00002750
00276              20  CL-OTHER-1-ADDR-SEQ PIC S9(4)     COMP.          00002760
00277              20  CL-OTHER-2-ADDR-SEQ PIC S9(4)     COMP.          00002770
00278                                                                   00002780
00279      12  CL-FILE-LOCATION            PIC X(4).                    00002790
00280                                                                   00002800
00281      12  CL-PROCESS-ERRORS.                                       00002810
00282          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.          00002820
00283              88  NO-FATAL-ERRORS        VALUE ZERO.               00002830
00284          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.          00002840
00285              88  NO-FORCABLE-ERRORS     VALUE ZERO.               00002850
00286                                                                   00002860
00287      12  CL-BENEFICIARY              PIC X(6).                    00002870
00288                                                                   00002880
00289      12  FILLER                      PIC X(2).                    00002890
00290                                                                   00002900
00291 ******************************************************************00002910
00292                                                                   00002920
00293      EJECT                                                        00002930
00294                                  COPY ELCMSTR.                    00002931
00295                                                                   00002950
00296      EJECT                                                        00002960
00297                                  COPY ELCCERT.                    00002961
00298                                                                   00002980
00299      EJECT                                                        00002990
00300  01  OLD-TRLR-RECORD.                                             00003000
00301      12  AT-RECORD-ID                PIC XX.                      00003010
00302          88  VALID-AT-ID                VALUE 'AT'.               00003020
00303                                                                   00003030
00304      12  AT-CONTROL-PRIMARY.                                      00003040
00305          16  AT-COMPANY-CD           PIC X.                       00003050
00306          16  AT-CARRIER              PIC X.                       00003060
00307          16  AT-CLAIM-NO             PIC X(7).                    00003070
00308          16  AT-CERT-NO.                                          00003080
00309              20  AT-CERT-PRIME       PIC X(10).                   00003090
00310              20  AT-CERT-SFX         PIC X.                       00003100
00311          16  AT-SEQUENCE-NO          PIC S9(4)     COMP.          00003110
00312                                                                   00003120
00313      12  AT-TRAILER-TYPE             PIC X.                       00003130
00314          88  RESERVE-EXPENSE-TR         VALUE '1'.                00003140
00315          88  PAYMENT-TR                 VALUE '2'.                00003150
00316          88  AUTO-PAY-TR                VALUE '3'.                00003160
00317          88  CORRESPONDENCE-TR          VALUE '4'.                00003170
00318          88  ADDRESS-TR                 VALUE '5'.                00003180
00319          88  GENERAL-INFO-TR            VALUE '6'.                00003190
00320          88  AUTO-PROMPT-TR             VALUE '7'.                00003200
00321          88  DENIAL-TR                  VALUE '8'.                00003210
00322          88  INCURRED-CHG-TR            VALUE '9'.                00003220
00323          88  FORM-CONTROL-TR            VALUE 'A'.                00003230
00324                                                                   00003240
00325      12  AT-RECORDED-DT              PIC XX.                      00003250
00326      12  AT-RECORDED-BY              PIC X(4).                    00003260
00327      12  AT-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.        00003270
00328                                                                   00003280
00329      12  AT-TRAILER-BODY             PIC X(165).                  00003290
00330                                                                   00003300
00331      12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.       00003310
00332          16  AT-RESERVE-CONTROLS.                                 00003320
00333              20  AT-MANUAL-SW        PIC X.                       00003330
00334                  88  AT-MANUAL-RESERVES-USED VALUE '1'.           00003340
00335              20  AT-FUTURE-SW        PIC X.                       00003350
00336                  88  AT-FUTURE-RESERVES-USED VALUE '1'.           00003360
00337              20  AT-PTC-SW           PIC X.                       00003370
00338                  88  AT-PAY-TO-CURRENT-USED  VALUE '1'.           00003380
00339              20  AT-IBNR-SW          PIC X.                       00003390
00340                  88  AT-IBNR-RESERVES-USED   VALUE '1'.           00003400
00341              20  AT-PTC-LF-SW        PIC X.                       00003410
00342                  88  AT-LF-PTC-USED          VALUE '1'.           00003420
00343              20  AT-CDT-ACCESS-METHOD PIC X.                      00003430
00344                  88  AT-CDT-ROUND-NEAR       VALUE '1'.           00003440
00345                  88  AT-CDT-ROUND-HIGH       VALUE '2'.           00003450
00346                  88  AT-CDT-INTERPOLATED     VALUE '3'.           00003460
00347              20  AT-PERCENT-OF-CDT   PIC S9(3)V99    COMP-3.      00003470
00348          16  AT-LAST-COMPUTED-DT     PIC XX.                      00003480
00349          16  AT-FUTURE-RESERVE       PIC S9(5)V99    COMP-3.      00003490
00350          16  AT-PAY-CURRENT-RESERVE  PIC S9(5)V99    COMP-3.      00003500
00351          16  AT-IBNR-RESERVE         PIC S9(5)V99    COMP-3.      00003510
00352          16  AT-INITIAL-MANUAL-RESERVE PIC S9(5)V99  COMP-3.      00003520
00353          16  AT-CURRENT-MANUAL-RESERVE PIC S9(5)V99  COMP-3.      00003530
00354          16  AT-ITD-ADDITIONAL-RESERVE PIC S9(5)V99  COMP-3.      00003540
00355          16  AT-EXPENSE-CONTROLS.                                 00003550
00356              20  AT-EXPENSE-METHOD   PIC X.                       00003560
00357                  88  NO-EXPENSE-CALCULATED   VALUE '1'.           00003570
00358                  88  FLAT-DOLLAR-PER-PMT     VALUE '2'.           00003580
00359                  88  PERCENT-OF-PMT          VALUE '3'.           00003590
00360                  88  DOLLAR-PER-OPEN-MONTH   VALUE '4'.           00003600
00361              20  AT-EXPENSE-PERCENT  PIC S9(3)V99    COMP-3.      00003610
00362              20  AT-EXPENSE-DOLLAR   PIC S9(3)V99    COMP-3.      00003620
00363          16  AT-ITD-PAID-EXPENSES    PIC S9(5)V99    COMP-3.      00003630
00364          16  AT-ITD-CHARGEABLE-EXPENSE PIC S9(5)V99  COMP-3.      00003640
00365                                                                   00003650
00366          16  AT-ITD-LIFE-REFUNDS     PIC S9(5)V99    COMP-3.      00003660
00367          16  AT-ITD-AH-REFUNDS       PIC S9(5)V99    COMP-3.      00003670
00368                                                                   00003680
00369          16  FILLER                  PIC X(57).                   00003690
00370                                                                   00003700
00371          16  AT-RESERVES-LAST-UPDATED-BY PIC S9(4)   COMP.        00003710
00372                                                                   00003720
00373          16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.                00003730
00374              20  AT-OPEN-CLOSE-DATE  PIC XX.                      00003740
00375              20  AT-OPEN-CLOSE-TYPE  PIC X.                       00003750
00376 *                    C = CLOSED                                   00003760
00377 *                    O = OPEN                                     00003770
00378              20  AT-OPEN-CLOSE-REASON PIC X(5).                   00003780
00379 *                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE      00003790
00380                                                                   00003800
00381      12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.               00003810
00382          16  AT-PAYMENT-TYPE         PIC X.                       00003820
00383              88  PARTIAL-PAYMENT        VALUE '1'.                00003830
00384              88  FINAL-PAYMENT          VALUE '2'.                00003840
00385              88  LUMP-SUM-PAYMENT       VALUE '3'.                00003850
00386              88  ADDITIONAL-PAYMENT     VALUE '4'.                00003860
00387              88  CHARGEABLE-EXPENSE     VALUE '5'.                00003870
00388              88  NON-CHARGEABLE-EXPENSE VALUE '6'.                00003880
00389                                                                   00003890
00390          16  AT-CLAIM-TYPE           PIC X.                       00003900
00391              88  PAID-FOR-AH            VALUE 'A'.                00003910
00392              88  PAID-FOR-LIFE          VALUE 'L'.                00003920
00393          16  AT-CLAIM-PREM-TYPE      PIC X.                       00003930
00394              88  AT-SINGLE-PREMIUM      VALUE '1'.                00003940
00395              88  AT-O-B-COVERAGE        VALUE '2'.                00003950
00396              88  AT-OPEN-END-COVERAGE   VALUE '3'.                00003960
00397          16  AT-AMOUNT-PAID          PIC S9(7)V99  COMP-3.        00003970
00398          16  AT-CHECK-NO             PIC X(7).                    00003980
00399          16  AT-PAID-FROM-DT         PIC XX.                      00003990
00400          16  AT-PAID-THRU-DT         PIC XX.                      00004000
00401          16  AT-DAYS-IN-PERIOD       PIC S9(4)     COMP.          00004010
00402          16  AT-PAYEE-TYPE-CD        PIC X.                       00004020
00403              88  INSURED-PAID           VALUE '1'.                00004030
00404              88  BENEFICIARY-PAID       VALUE '2'.                00004040
00405              88  ACCOUNT-PAID           VALUE '3'.                00004050
00406              88  OTHER-1-PAID           VALUE '4'.                00004060
00407              88  OTHER-2-PAID           VALUE '5'.                00004070
00408              88  DOCTOR-PAID            VALUE '6'.                00004080
00409          16  AT-PAYEES-NAME          PIC X(30).                   00004090
00410          16  AT-PAYMENT-ORIGIN       PIC X.                       00004100
00411              88  ONLINE-MANUAL-PMT      VALUE '1'.                00004110
00412              88  ONLINE-AUTO-PMT        VALUE '2'.                00004120
00413              88  OFFLINE-PMT            VALUE '3'.                00004130
00414          16  AT-CHECK-WRITTEN-DT     PIC XX.                      00004140
00415          16  AT-TO-BE-WRITTEN-DT     PIC XX.                      00004150
00416          16  AT-VOID-DATA.                                        00004160
00417              20  AT-VOID-DT          PIC XX.                      00004170
00418              20  AT-VOID-REASON      PIC X(30).                   00004180
00419          16  AT-ADDL-RESERVE         PIC S9(5)V99  COMP-3.        00004190
00420          16  AT-EXPENSE-PER-PMT      PIC S9(5)V99  COMP-3.        00004200
00421          16  AT-CREDIT-INTERFACE.                                 00004210
00422              20  AT-PMT-SELECT-DT    PIC XX.                      00004220
00423                  88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.       00004230
00424              20  AT-PMT-ACCEPT-DT    PIC XX.                      00004240
00425                  88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.       00004250
00426              20  AT-VOID-SELECT-DT   PIC XX.                      00004260
00427                  88  VOID-NOT-SELECTED     VALUE LOW-VALUE.       00004270
00428              20  AT-VOID-ACCEPT-DT   PIC XX.                      00004280
00429                  88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.       00004290
00430                                                                   00004300
00431          16  AT-CHECK-QUE-CONTROL    PIC S9(8)     COMP.          00004310
00432                  88  PAYMENT-NOT-QUEUED    VALUE ZERO.            00004320
00433                  88  CONVERSION-PAYMENT    VALUE +99999999.       00004330
00434          16  AT-CHECK-QUE-SEQUENCE   PIC S9(4)     COMP.          00004340
00435                                                                   00004350
00436          16  AT-FORCE-CONTROL        PIC X.                       00004360
00437              88  PAYMENT-WAS-FORCED        VALUE '1'.             00004370
00438          16  AT-PREV-LAST-PMT-DT     PIC XX.                      00004380
00439          16  AT-PREV-PAID-THRU-DT    PIC XX.                      00004390
00440          16  AT-PREV-LAST-PMT-AMT    PIC S9(7)V99  COMP-3.        00004400
00441          16  AT-ELIMINATION-DAYS     PIC S999      COMP-3.        00004410
00442          16  AT-DAILY-RATE           PIC S9(3)V99  COMP-3.        00004420
00443          16  AT-BENEFIT-TYPE         PIC X.                       00004430
00444                                                                   00004440
00445          16  AT-EXPENSE-TYPE         PIC X.                       00004450
00446          16  AT-PAYMENT-APPROVAL-SW  PIC X.                       00004460
00447                                                                   00004470
00448          16  FILLER                  PIC X(34).                   00004480
00449                                                                   00004490
00450          16  AT-PAYMENT-LAST-UPDATED-BY  PIC S9(4)   COMP.        00004500
00451                                                                   00004510
00452      12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.              00004520
00453          16  AT-SCHEDULE-START-DT    PIC XX.                      00004530
00454          16  AT-SCHEDULE-END-DT      PIC XX.                      00004540
00455          16  AT-TERMINATED-DT        PIC XX.                      00004550
00456          16  AT-LAST-PMT-TYPE        PIC X.                       00004560
00457              88  LAST-PMT-IS-FINAL      VALUE 'F'.                00004570
00458              88  LAST-PMT-IS-PARTIAL    VALUE 'P'.                00004580
00459          16  AT-FIRST-PMT-DATA.                                   00004590
00460              20  AT-FIRST-PMT-AMT    PIC S9(7)V99  COMP-3.        00004600
00461              20  AT-DAYS-IN-1ST-PMT  PIC S9(4)     COMP.          00004610
00462              20  AT-1ST-PAY-THRU-DT  PIC XX.                      00004620
00463          16  AT-REGULAR-PMT-DATA.                                 00004630
00464              20  AT-REGULAR-PMT-AMT  PIC S9(7)V99  COMP-3.        00004640
00465              20  AT-DAYS-IN-REG-PMT  PIC S9(4)     COMP.          00004650
00466              20  AT-INTERVAL-MONTHS  PIC S9(4)     COMP.          00004660
00467          16  AT-AUTO-PAYEE-CD        PIC X.                       00004670
00468              88  INSURED-PAID-AUTO      VALUE '1'.                00004680
00469              88  BENEFICIARY-PAID-AUTO  VALUE '2'.                00004690
00470              88  ACCOUNT-PAID-AUTO      VALUE '3'.                00004700
00471              88  OTHER-1-PAID-AUTO      VALUE '4'.                00004710
00472              88  OTHER-2-PAID-AUTO      VALUE '5'.                00004720
00473          16  FILLER                  PIC X(137).                  00004730
00474                                                                   00004740
00475          16  AT-AUTO-PAY-LAST-UPDATED-BY PIC S9(4)   COMP.        00004750
00476                                                                   00004760
00477      12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.        00004770
00478          16  AT-LETTER-SENT-DT       PIC XX.                      00004780
00479          16  AT-RECEIPT-FOLLOW-UP    PIC XX.                      00004790
00480          16  AT-AUTO-RE-SEND-DT      PIC XX.                      00004800
00481          16  AT-LETTER-ANSWERED-DT   PIC XX.                      00004810
00482          16  AT-LETTER-ARCHIVE-NO    PIC S9(8)     COMP.          00004820
00483          16  AT-LETTER-ORIGIN        PIC X.                       00004830
00484              88  ONLINE-CREATION        VALUE '1'.                00004840
00485              88  OFFLINE-CREATION       VALUE '2'.                00004850
00486          16  AT-STD-LETTER-FORM      PIC X(4).                    00004860
00487          16  AT-REASON-TEXT          PIC X(70).                   00004870
00488          16  AT-ADDRESS-REC-SEQ-NO   PIC S9(4)     COMP.          00004880
00489          16  AT-ADDRESEE-TYPE        PIC X.                       00004890
00490              88  INSURED-ADDRESEE       VALUE '1'.                00004900
00491              88  BENEFICIARY-ADDRESEE   VALUE '2'.                00004910
00492              88  ACCOUNT-ADDRESEE       VALUE '3'.                00004920
00493              88  PHYSICIAN-ADDRESEE     VALUE '4'.                00004930
00494              88  EMPLOYER-ADDRESEE      VALUE '5'.                00004940
00495              88  OTHER-ADDRESEE-1       VALUE '6'.                00004950
00496              88  OTHER-ADDRESEE-2       VALUE '7'.                00004960
00497          16  AT-ADDRESSEE-NAME       PIC X(30).                   00004970
00498          16  AT-INITIAL-PRINT-DATE   PIC XX.                      00004980
00499          16  AT-RESEND-PRINT-DATE    PIC XX.                      00004990
00500          16  AT-CORR-SOL-UNSOL       PIC X(01).                   00005000
00501          16  FILLER                  PIC X(38).                   00005010
00502          16  AT-CORR-LAST-UPDATED-BY PIC S9(4)   COMP.            00005020
00503                                                                   00005030
00504      12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.               00005040
00505          16  AT-ADDRESS-TYPE         PIC X.                       00005050
00506              88  INSURED-ADDRESS        VALUE '1'.                00005060
00507              88  BENEFICIARY-ADDRESS    VALUE '2'.                00005070
00508              88  ACCOUNT-ADDRESS        VALUE '3'.                00005080
00509              88  PHYSICIAN-ADDRESS      VALUE '4'.                00005090
00510              88  EMPLOYER-ADDRESS       VALUE '5'.                00005100
00511              88  OTHER-ADDRESS-1        VALUE '6'.                00005110
00512              88  OTHER-ADDRESS-2        VALUE '7'.                00005120
00513          16  AT-MAIL-TO-NAME         PIC X(30).                   00005130
00514          16  AT-ADDRESS-LINE-1       PIC X(30).                   00005140
00515          16  AT-ADDRESS-LINE-2       PIC X(30).                   00005150
00516          16  AT-CITY-STATE           PIC X(30).                   00005160
00517          16  AT-ZIP.                                              00005170
00518             18  AT-ZIP-CODE          PIC X(5).                    00005180
00519             18  AT-ZIP-PLUS4         PIC X(4).                    00005190
00520          16  AT-PHONE-NO             PIC 9(11)     COMP-3.        00005200
00521          16  FILLER                  PIC X(27).                   00005210
00522          16  AT-ADDRESS-LAST-UPDATED-BY PIC S9(4)  COMP.          00005220
00523                                                                   00005230
00524      12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.          00005240
00525          16  AT-INFO-LINE-1          PIC X(60).                   00005250
00526          16  AT-INFO-LINE-2          PIC X(60).                   00005260
00527          16  FILLER                  PIC X(43).                   00005270
00528          16  AT-GEN-INFO-LAST-UPDATED-BY PIC S9(4)  COMP.         00005280
00529                                                                   00005290
00530      12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.           00005300
00531          16  AT-PROMPT-LINE-1        PIC X(60).                   00005310
00532          16  AT-PROMPT-LINE-2        PIC X(60).                   00005320
00533          16  AT-PROMPT-START-DT      PIC XX.                      00005330
00534          16  AT-PROMPT-END-DT        PIC XX.                      00005340
00535          16  FILLER                  PIC X(39).                   00005350
00536          16  AT-PROMPT-LAST-UPDATED-BY PIC S9(4)  COMP.           00005360
00537                                                                   00005370
00538      12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.           00005380
00539          16  AT-DENIAL-INFO-1        PIC X(60).                   00005390
00540          16  AT-DENIAL-INFO-2        PIC X(60).                   00005400
00541          16  AT-DENIAL-DT            PIC XX.                      00005410
00542          16  AT-RETRACTION-DT        PIC XX.                      00005420
00543          16  AT-DENIAL-REASON-CODE   PIC X(4).                    00005430
00544          16  FILLER                  PIC X(35).                   00005440
00545          16  AT-DENIAL-LAST-UPDATED-BY PIC S9(4)  COMP.           00005450
00546                                                                   00005460
00547      12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.          00005470
00548          16  AT-OLD-INCURRED-DT      PIC XX.                      00005480
00549          16  AT-OLD-REPORTED-DT      PIC XX.                      00005490
00550          16  AT-OLD-ESTABLISHED-DT   PIC XX.                      00005500
00551          16  AT-OLD-TOTAL-PAID       PIC S9(7)V99     COMP-3.     00005510
00552          16  AT-OLD-DAYS-PAID        PIC S9(4)        COMP.       00005520
00553          16  AT-OLD-NO-OF-PMTS       PIC S9(3)        COMP-3.     00005530
00554          16  AT-OLD-PAID-THRU-DT     PIC XX.                      00005540
00555          16  AT-LAST-PMT-MADE-DT     PIC XX.                      00005550
00556          16  AT-OLD-DIAG-DESCRIP     PIC X(26).                   00005560
00557          16  AT-OLD-DIAG-CODE        PIC X(6).                    00005570
00558          16  AT-TRAILER-CNT-AT-CHG   PIC S9(4)        COMP.       00005580
00559          16  AT-OLD-ITD-PAID-EXPENSE PIC S9(5)V99     COMP-3.     00005590
00560          16  AT-OLD-CHARGABLE-EXPENSE PIC S9(5)V99    COMP-3.     00005600
00561          16  AT-OLD-INIT-MAN-RESV    PIC S9(7)V99     COMP-3.     00005610
00562          16  AT-OLD-CURRENT-MAN-RESV PIC S9(7)V99     COMP-3.     00005620
00563          16  AT-OLD-ADDL-MAN-RESV    PIC S9(7)V99     COMP-3.     00005630
00564          16  FILLER                  PIC X(87).                   00005640
00565          16  AT-INCURRED-LAST-UPDATED-BY PIC S9(4)  COMP.         00005650
00566                                                                   00005660
00567      12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.          00005670
00568          16  AT-FORM-SEND-ON-DT      PIC XX.                      00005680
00569          16  AT-FORM-FOLLOW-UP-DT    PIC XX.                      00005690
00570          16  AT-FORM-RE-SEND-DT      PIC XX.                      00005700
00571          16  AT-FORM-ANSWERED-DT     PIC XX.                      00005710
00572          16  AT-FORM-PRINTED-DT      PIC XX.                      00005720
00573          16  AT-FORM-REPRINT-DT      PIC XX.                      00005730
00574          16  AT-FORM-TYPE            PIC X.                       00005740
00575              88  INITIAL-FORM           VALUE '1'.                00005750
00576              88  PROGRESS-FORM          VALUE '2'.                00005760
00577          16  AT-INSTRUCT-LN-1        PIC X(28).                   00005770
00578          16  AT-INSTRUCT-LN-2        PIC X(28).                   00005780
00579          16  AT-INSTRUCT-LN-3        PIC X(28).                   00005790
00580          16  AT-FORM-ADDR-SEQ-NO     PIC S9(4)      COMP.         00005800
00581          16  AT-FORM-ADDRESS         PIC X.                       00005810
00582              88  FORM-TO-INSURED        VALUE '1'.                00005820
00583              88  FORM-TO-ACCOUNT        VALUE '3'.                00005830
00584              88  FORM-TO-OTHER-1        VALUE '6'.                00005840
00585              88  FORM-TO-OTHER-2        VALUE '7'.                00005850
00586          16  AT-RELATED-1.                                        00005860
00587              20 AT-REL-CARR-1        PIC X.                       00005870
00588              20 AT-REL-CLAIM-1       PIC X(7).                    00005880
00589              20 AT-REL-CERT-1        PIC X(11).                   00005890
00590          16  AT-RELATED-2.                                        00005900
00591              20 AT-REL-CARR-2        PIC X.                       00005910
00592              20 AT-REL-CLAIM-2       PIC X(7).                    00005920
00593              20 AT-REL-CERT-2        PIC X(11).                   00005930
00594                                                                   00005940
00595          16  FILLER                  PIC X(25).                   00005950
00596          16  AT-FORM-LAST-UPDATED-BY PIC S9(4)  COMP.             00005960
00597                                                                   00005970
00598 ******************************************************************00005980
00599                                                                   00005990
00600      EJECT                                                        00006000
00601                                  COPY ELCTRLR.                    00006001
00602                                                                   00006020
00603      EJECT                                                        00006030
00604  01  OLD-ARCH-RECORD.                                             00006040
00605      12  LA-RECORD-ID                PIC XX.                      00006050
00606          88  VALID-LA-ID                VALUE 'LA'.               00006060
00607                                                                   00006070
00608      12  LA-CONTROL-PRIMARY.                                      00006080
00609          16  LA-COMPANY-CD           PIC X.                       00006090
00610          16  LA-ARCHIVE-NO           PIC S9(8)     COMP.          00006100
00611          16  LA-RECORD-TYPE          PIC X.                       00006110
00612              88  LA-HEADER-DATA         VALUE '1'.                00006120
00613              88  LA-ADDRESS-DATA        VALUE '2'.                00006130
00614              88  LA-TEXT-DATA           VALUE '3'.                00006140
00615              88  LA-FORM-CONTROL-HDR    VALUE '4'.                00006150
00616          16  LA-LINE-SEQ-NO          PIC S9(4)     COMP.          00006160
00617                                                                   00006170
00618      12  LA-CONTROL-BY-TYPE.                                      00006180
00619          16  LA-COMPANY-CD-A1        PIC X.                       00006190
00620          16  LA-RECORD-TYPE-A1       PIC X.                       00006200
00621          16  LA-ARCHIVE-NO-A1        PIC S9(8)     COMP.          00006210
00622          16  LA-LINE-SEQ-NO-A1       PIC S9(4)     COMP.          00006220
00623                                                                   00006230
00624      12  LA-TEXT-RECORD.                                          00006240
00625          16  LA-SKIP-CONTROL         PIC XX.                      00006250
00626              88  NO-LINES-SKIPPED       VALUE SPACES.             00006260
00627              88  SKIP-TO-NEXT-PAGE      VALUE '99'.               00006270
00628          16  LA-TEXT-LINE            PIC X(70).                   00006280
00629                                                                   00006290
00630      12  LA-ADDRESS-RECORD  REDEFINES  LA-TEXT-RECORD.            00006300
00631          16  FILLER                  PIC XX.                      00006310
00632          16  LA-ADDRESS-LINE         PIC X(30).                   00006320
00633          16  FILLER                  PIC X(40).                   00006330
00634                                                                   00006340
00635      12  LA-HEADER-RECORD  REDEFINES  LA-TEXT-RECORD.             00006350
00636          16  FILLER                  PIC XX.                      00006360
00637          16  LA-CARRIER              PIC X.                       00006370
00638          16  LA-CLAIM-NO             PIC X(7).                    00006380
00639          16  LA-CERT-NO.                                          00006390
00640              20  LA-CERT-PRIME       PIC X(10).                   00006400
00641              20  LA-CERT-SFX         PIC X.                       00006410
00642          16  LA-NO-OF-COPIES         PIC S9.                      00006420
00643          16  LA-RESEND-DATE          PIC XX.                      00006430
00644          16  LA-PROCESSOR-CD         PIC X(4).                    00006440
00645          16  LA-CREATION-DT          PIC XX.                      00006450
00646          16  LA-INITIAL-PRINT-DATE   PIC XX.                      00006460
00647          16  LA-RESEND-PRINT-DATE    PIC XX.                      00006470
00648          16  LA-CORR-TRLR-SEQ        PIC S9(4)    COMP.           00006480
00649          16  FILLER                  PIC X(36).                   00006490
00650                                                                   00006500
00651      12  LA-FORM-CONTROL-HEADER REDEFINES  LA-TEXT-RECORD.        00006510
00652          16  FILLER                  PIC XX.                      00006520
00653          16  LA4-CARRIER             PIC X.                       00006530
00654          16  LA4-CLAIM-NO            PIC X(7).                    00006540
00655          16  LA4-CERT-NO.                                         00006550
00656              20  LA4-CERT-PRIME      PIC X(10).                   00006560
00657              20  LA4-CERT-SFX        PIC X.                       00006570
00658          16  LA4-NO-OF-COPIES        PIC S9.                      00006580
00659          16  LA4-RESEND-DATE         PIC XX.                      00006590
00660          16  LA4-PROCESSOR-CD        PIC X(4).                    00006600
00661          16  LA4-CREATION-DT         PIC XX.                      00006610
00662          16  LA4-INITIAL-PRINT-DATE  PIC XX.                      00006620
00663          16  LA4-RESEND-PRINT-DATE   PIC XX.                      00006630
00664          16  LA4-FORM-TRLR-SEQ       PIC S9(4)    COMP.           00006640
00665          16  LA4-FORM-TYPE           PIC X.                       00006650
00666              88  LA4-INITIAL-FORM    VALUE '1'.                   00006660
00667              88  LA4-PROGRESS-FORM   VALUE '2'.                   00006670
00668          16  FILLER                  PIC X(35).                   00006680
00669 ******************************************************************00006690
00670                                                                   00006700
00671      EJECT                                                        00006710
00672                                  COPY ELCARCH.                    00006711
00673                                                                   00006730
00674      EJECT                                                        00006740
00675                                  COPY ELCDATE.                    00006741
00676                                                                   00006760
00677      EJECT                                                        00006770
00678  PROCEDURE DIVISION.                                              00006780
00679                                                                   00006790
00680  0010-OPEN-FILES.                                                 00006800
00681                                                                   00006810
00682      OPEN INPUT HISTORY-IN                                        00006820
00683           OUTPUT HISTORY-OT.                                      00006830
00684                                                                   00006840
00685      ACCEPT WS-RUN-DATE FROM DATE.                                00006850
00686      MOVE WS-RUN-DATE            TO  DC-GREG-DATE-1-YMD.          00006860
00687      MOVE '3'                    TO  DC-OPTION-CODE.              00006870
00688      PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT.                 00006880
00689      IF NO-CONVERSION-ERROR                                       00006890
00690          MOVE DC-BIN-DATE-1      TO  WS-BIN-RUN-DATE              00006900
00691      ELSE                                                         00006910
00692          MOVE LOW-VALUES         TO  WS-BIN-RUN-DATE.             00006920
00693                                                                   00006930
00694  0020-READ-HISTORY.                                               00006940
00695                                                                   00006950
00696      READ HISTORY-IN AT END                                       00006960
00697         GO TO 9000-EOJ-ROUTINE.                                   00006970
00698                                                                   00006980
00699  0025-RETURN.                                                     00006990
00700                                                                   00007000
00701      IF HIR-RECORD-ID NOT EQUAL 'CL'                              00007010
00702         GO TO 0030-PROCESS-CM.                                    00007020
00703                                                                   00007030
00704      ADD 1 TO TOTAL-CL-RECORDS-IN.                                00007040
00705      MOVE 'CL'                   TO WS-LAST-RECORD-ID.            00007050
00706                                                                   00007060
00707      MOVE HIR-COMPANY-ID         TO HOR-COMPANY-ID.               00007070
00708      MOVE HIR-DATE-ARCHIVED      TO HOR-DATE-ARCHIVED.            00007080
00709      MOVE HIR-PURGED-CLAIM       TO HOR-PURGED-CLAIM.             00007090
00710                                                                   00007100
00711      MOVE HIR-CLAIM-RECORD       TO OLD-CLAIM-MASTER.             00007110
00712                                                                   00007120
00713      MOVE SPACES TO CLAIM-MASTER.                                 00007130
00714                                                                   00007140
00715      MOVE CL-RECORD-ID OF OLD-CLAIM-MASTER TO                     00007150
00716           CL-RECORD-ID OF CLAIM-MASTER.                           00007160
00717      MOVE CL-COMPANY-CD OF OLD-CLAIM-MASTER TO                    00007170
00718           CL-COMPANY-CD OF CLAIM-MASTER.                          00007180
00719      MOVE CL-CARRIER OF OLD-CLAIM-MASTER TO                       00007190
00720           CL-CARRIER OF CLAIM-MASTER.                             00007200
00721      MOVE CL-CLAIM-NO OF OLD-CLAIM-MASTER TO                      00007210
00722           CL-CLAIM-NO OF CLAIM-MASTER.                            00007220
00723      MOVE CL-CERT-NO OF OLD-CLAIM-MASTER TO                       00007230
00724           CL-CERT-NO OF CLAIM-MASTER.                             00007240
00725      MOVE CL-COMPANY-CD-A1 OF OLD-CLAIM-MASTER TO                 00007250
00726           CL-COMPANY-CD-A1 OF CLAIM-MASTER.                       00007260
00727      MOVE CL-INSURED-LAST-NAME OF OLD-CLAIM-MASTER TO             00007270
00728           CL-INSURED-LAST-NAME OF CLAIM-MASTER.                   00007280
00729      MOVE CL-COMPANY-CD-A2 OF OLD-CLAIM-MASTER TO                 00007290
00730           CL-COMPANY-CD-A2 OF CLAIM-MASTER.                       00007300
00731      MOVE CL-SSN-STATE OF OLD-CLAIM-MASTER TO                     00007310
00732           CL-SSN-STATE OF CLAIM-MASTER.                           00007320
00733      MOVE CL-SSN-ACCOUNT OF OLD-CLAIM-MASTER TO                   00007330
00734           CL-SSN-ACCOUNT OF CLAIM-MASTER.                         00007340
00735      MOVE CL-SSN-LN3 OF OLD-CLAIM-MASTER TO                       00007350
00736           CL-SSN-LN3 OF CLAIM-MASTER.                             00007360
00737      MOVE CL-COMPANY-CD-A4 OF OLD-CLAIM-MASTER TO                 00007370
00738           CL-COMPANY-CD-A4 OF CLAIM-MASTER.                       00007380
00739      MOVE CL-CERT-NO-A4 OF OLD-CLAIM-MASTER TO                    00007390
00740           CL-CERT-NO-A4 OF CLAIM-MASTER.                          00007400
00741      MOVE CL-INSURED-1ST-NAME OF OLD-CLAIM-MASTER TO              00007410
00742           CL-INSURED-1ST-NAME OF CLAIM-MASTER.                    00007420
00743      MOVE CL-INSURED-MID-INIT OF OLD-CLAIM-MASTER TO              00007430
00744           CL-INSURED-MID-INIT OF CLAIM-MASTER.                    00007440
00745      MOVE CL-INSURED-BIRTH-DT OF OLD-CLAIM-MASTER TO              00007450
00746           CL-INSURED-BIRTH-DT OF CLAIM-MASTER.                    00007460
00747      MOVE CL-INSURED-SEX-CD OF OLD-CLAIM-MASTER TO                00007470
00748           CL-INSURED-SEX-CD OF CLAIM-MASTER.                      00007480
00749      MOVE CL-INSURED-OCC-CD OF OLD-CLAIM-MASTER TO                00007490
00750           CL-INSURED-OCC-CD OF CLAIM-MASTER.                      00007500
00751      MOVE CL-PROCESSOR-ID OF OLD-CLAIM-MASTER TO                  00007510
00752           CL-PROCESSOR-ID OF CLAIM-MASTER.                        00007520
00753      MOVE CL-CLAIM-STATUS OF OLD-CLAIM-MASTER TO                  00007530
00754           CL-CLAIM-STATUS OF CLAIM-MASTER.                        00007540
00755      MOVE CL-CLAIM-TYPE OF OLD-CLAIM-MASTER TO                    00007550
00756           CL-CLAIM-TYPE OF CLAIM-MASTER.                          00007560
00757      MOVE CL-CLAIM-PREM-TYPE OF OLD-CLAIM-MASTER TO               00007570
00758           CL-CLAIM-PREM-TYPE OF CLAIM-MASTER.                     00007580
00759      MOVE CL-INCURRED-DT OF OLD-CLAIM-MASTER TO                   00007590
00760           CL-INCURRED-DT OF CLAIM-MASTER.                         00007600
00761      MOVE CL-REPORTED-DT OF OLD-CLAIM-MASTER TO                   00007610
00762           CL-REPORTED-DT OF CLAIM-MASTER.                         00007620
00763      MOVE CL-FILE-ESTABLISH-DT OF OLD-CLAIM-MASTER TO             00007630
00764           CL-FILE-ESTABLISH-DT OF CLAIM-MASTER.                   00007640
00765      MOVE CL-EST-END-OF-DISAB-DT OF OLD-CLAIM-MASTER TO           00007650
00766           CL-EST-END-OF-DISAB-DT OF CLAIM-MASTER.                 00007660
00767      MOVE CL-LAST-PMT-DT OF OLD-CLAIM-MASTER TO                   00007670
00768           CL-LAST-PMT-DT OF CLAIM-MASTER.                         00007680
00769      MOVE CL-LAST-PMT-AMT OF OLD-CLAIM-MASTER TO                  00007690
00770           CL-LAST-PMT-AMT OF CLAIM-MASTER.                        00007700
00771      MOVE CL-PAID-THRU-DT OF OLD-CLAIM-MASTER TO                  00007710
00772           CL-PAID-THRU-DT OF CLAIM-MASTER.                        00007720
00773      MOVE CL-TOTAL-PAID-AMT OF OLD-CLAIM-MASTER TO                00007730
00774           CL-TOTAL-PAID-AMT OF CLAIM-MASTER.                      00007740
00775      MOVE CL-NO-OF-PMTS-MADE OF OLD-CLAIM-MASTER TO               00007750
00776           CL-NO-OF-PMTS-MADE OF CLAIM-MASTER.                     00007760
00777      MOVE CL-NO-OF-DAYS-PAID OF OLD-CLAIM-MASTER TO               00007770
00778           CL-NO-OF-DAYS-PAID OF CLAIM-MASTER.                     00007780
00779      MOVE CL-PMT-CALC-METHOD OF OLD-CLAIM-MASTER TO               00007790
00780           CL-PMT-CALC-METHOD OF CLAIM-MASTER.                     00007800
00781      MOVE CL-CAUSE-CD OF OLD-CLAIM-MASTER TO                      00007810
00782           CL-CAUSE-CD OF CLAIM-MASTER.                            00007820
00783      MOVE CL-CERT-NO OF OLD-CLAIM-MASTER  TO                      00007830
00784           CL-PRIME-CERT-NO OF CLAIM-MASTER.                       00007840
00785      MOVE CL-LAST-REOPEN-DT OF OLD-CLAIM-MASTER TO                00007850
00786           CL-LAST-REOPEN-DT OF CLAIM-MASTER.                      00007860
00787      MOVE CL-LAST-CLOSE-DT OF OLD-CLAIM-MASTER TO                 00007870
00788           CL-LAST-CLOSE-DT OF CLAIM-MASTER.                       00007880
00789      MOVE CL-LAST-CLOSE-REASON OF OLD-CLAIM-MASTER TO             00007890
00790           CL-LAST-CLOSE-REASON OF CLAIM-MASTER.                   00007900
00791      MOVE +1             TO CL-ASSOC-CERT-SEQU                    00007910
00792                          OF CLAIM-MASTER                          00007920
00793                             CL-ASSOC-CERT-TOTAL                   00007930
00794                          OF CLAIM-MASTER.                         00007940
00795 *    MOVE 0              TO CL-CLAIM-PAYMENT-STATUS               00007950
00796      MOVE 1              TO CL-CLAIM-PAYMENT-STATUS               00007960
00797                          OF CLAIM-MASTER.                         00007970
00798      MOVE CL-CERT-ORIGIN OF OLD-CLAIM-MASTER TO                   00007980
00799           CL-CERT-ORIGIN OF CLAIM-MASTER.                         00007990
00800      MOVE CL-CERT-CARRIER OF OLD-CLAIM-MASTER TO                  00008000
00801           CL-CERT-CARRIER OF CLAIM-MASTER.                        00008010
00802      MOVE CL-CERT-GROUPING OF OLD-CLAIM-MASTER TO                 00008020
00803           CL-CERT-GROUPING OF CLAIM-MASTER.                       00008030
00804      MOVE CL-CERT-STATE OF OLD-CLAIM-MASTER TO                    00008040
00805           CL-CERT-STATE OF CLAIM-MASTER.                          00008050
00806      MOVE CL-CERT-ACCOUNT OF OLD-CLAIM-MASTER TO                  00008060
00807           CL-CERT-ACCOUNT OF CLAIM-MASTER.                        00008070
00808      MOVE CL-CERT-EFF-DT OF OLD-CLAIM-MASTER TO                   00008080
00809           CL-CERT-EFF-DT OF CLAIM-MASTER.                         00008090
00810      MOVE CL-PRIORITY-CD OF OLD-CLAIM-MASTER TO                   00008100
00811           CL-PRIORITY-CD OF CLAIM-MASTER.                         00008110
00812      MOVE CL-SUPV-ATTN-CD OF OLD-CLAIM-MASTER TO                  00008120
00813           CL-SUPV-ATTN-CD OF CLAIM-MASTER.                        00008130
00814      MOVE CL-PURGED-DT OF OLD-CLAIM-MASTER TO                     00008140
00815           CL-PURGED-DT OF CLAIM-MASTER.                           00008150
00816      MOVE CL-RESTORED-DT OF OLD-CLAIM-MASTER TO                   00008160
00817           CL-RESTORED-DT OF CLAIM-MASTER.                         00008170
00818      MOVE CL-NEXT-AUTO-PAY-DT OF OLD-CLAIM-MASTER TO              00008180
00819           CL-NEXT-AUTO-PAY-DT OF CLAIM-MASTER.                    00008190
00820      MOVE CL-NEXT-RESEND-DT OF OLD-CLAIM-MASTER TO                00008200
00821           CL-NEXT-RESEND-DT OF CLAIM-MASTER.                      00008210
00822      MOVE CL-NEXT-FOLLOWUP-DT OF OLD-CLAIM-MASTER TO              00008220
00823           CL-NEXT-FOLLOWUP-DT OF CLAIM-MASTER.                    00008230
00824      MOVE CL-LAST-MAINT-DT OF OLD-CLAIM-MASTER TO                 00008240
00825           CL-LAST-MAINT-DT OF CLAIM-MASTER.                       00008250
00826      MOVE CL-LAST-MAINT-USER OF OLD-CLAIM-MASTER TO               00008260
00827           CL-LAST-MAINT-USER OF CLAIM-MASTER.                     00008270
00828      MOVE CL-LAST-MAINT-HHMMSS OF OLD-CLAIM-MASTER TO             00008280
00829           CL-LAST-MAINT-HHMMSS OF CLAIM-MASTER.                   00008290
00830      MOVE CL-LAST-MAINT-TYPE OF OLD-CLAIM-MASTER TO               00008300
00831           CL-LAST-MAINT-TYPE OF CLAIM-MASTER.                     00008310
00832      MOVE CL-RELATED-CLAIM-NO OF OLD-CLAIM-MASTER TO              00008320
00833           CL-RELATED-CLAIM-NO OF CLAIM-MASTER.                    00008330
00834      MOVE CL-HISTORY-ARCHIVE-DT OF OLD-CLAIM-MASTER TO            00008340
00835           CL-HISTORY-ARCHIVE-DT OF CLAIM-MASTER.                  00008350
00836      MOVE CL-TRAILER-SEQ-CNT OF OLD-CLAIM-MASTER TO               00008360
00837           CL-TRAILER-SEQ-CNT OF CLAIM-MASTER.                     00008370
00838      MOVE CL-LAST-INC-DT-CHANGE OF OLD-CLAIM-MASTER TO            00008380
00839           CL-LAST-INC-DT-CHANGE OF CLAIM-MASTER.                  00008390
00840      MOVE CL-AUTO-PAY-SEQ OF OLD-CLAIM-MASTER TO                  00008400
00841           CL-AUTO-PAY-SEQ OF CLAIM-MASTER.                        00008410
00842      IF CL-INSURED-ADDR-SEQ OF OLD-CLAIM-MASTER GREATER +0        00008420
00843          MOVE +1           TO  CL-INSURED-ADDR-CNT                00008430
00844                            OF  CLAIM-MASTER                       00008440
00845      ELSE                                                         00008450
00846          MOVE +0           TO  CL-INSURED-ADDR-CNT                00008460
00847                            OF CLAIM-MASTER.                       00008470
00848      IF CL-ACCOUNT-ADDR-SEQ OF OLD-CLAIM-MASTER GREATER +0        00008480
00849          MOVE +1           TO  CL-ACCOUNT-ADDR-CNT                00008490
00850                            OF  CLAIM-MASTER                       00008500
00851      ELSE                                                         00008510
00852          MOVE +0           TO  CL-ACCOUNT-ADDR-CNT                00008520
00853                            OF CLAIM-MASTER.                       00008530
00854      IF CL-BENIF-ADDR-SEQ OF OLD-CLAIM-MASTER GREATER +0          00008540
00855          MOVE +1           TO  CL-BENIF-ADDR-CNT                  00008550
00856                            OF  CLAIM-MASTER                       00008560
00857      ELSE                                                         00008570
00858          MOVE +0           TO  CL-BENIF-ADDR-CNT                  00008580
00859                            OF CLAIM-MASTER.                       00008590
00860      IF CL-EMPLOYER-ADDR-SEQ OF OLD-CLAIM-MASTER GREATER +0       00008600
00861          MOVE +1           TO  CL-EMPLOYER-ADDR-CNT               00008610
00862                            OF  CLAIM-MASTER                       00008620
00863      ELSE                                                         00008630
00864          MOVE +0           TO  CL-EMPLOYER-ADDR-CNT               00008640
00865                            OF CLAIM-MASTER.                       00008650
00866      IF CL-DOCTOR-ADDR-SEQ OF OLD-CLAIM-MASTER GREATER +0         00008660
00867          MOVE +1           TO  CL-DOCTOR-ADDR-CNT                 00008670
00868                            OF  CLAIM-MASTER                       00008680
00869      ELSE                                                         00008690
00870          MOVE +0           TO  CL-DOCTOR-ADDR-CNT                 00008700
00871                            OF CLAIM-MASTER.                       00008710
00872      IF CL-OTHER-1-ADDR-SEQ OF OLD-CLAIM-MASTER GREATER +0        00008720
00873          MOVE +1           TO  CL-OTHER-1-ADDR-CNT                00008730
00874                            OF  CLAIM-MASTER                       00008740
00875      ELSE                                                         00008750
00876          MOVE +0           TO  CL-OTHER-1-ADDR-CNT                00008760
00877                            OF CLAIM-MASTER.                       00008770
00878      IF CL-OTHER-2-ADDR-SEQ OF OLD-CLAIM-MASTER GREATER +0        00008780
00879          MOVE +1           TO  CL-OTHER-2-ADDR-CNT                00008790
00880                            OF  CLAIM-MASTER                       00008800
00881      ELSE                                                         00008810
00882          MOVE +0           TO  CL-OTHER-2-ADDR-CNT                00008820
00883                            OF CLAIM-MASTER.                       00008830
00884                                                                   00008840
00885      MOVE CL-FILE-LOCATION OF OLD-CLAIM-MASTER TO                 00008850
00886           CL-FILE-LOCATION OF CLAIM-MASTER.                       00008860
00887      MOVE CL-FATAL-ERROR-CNT OF OLD-CLAIM-MASTER TO               00008870
00888           CL-FATAL-ERROR-CNT OF CLAIM-MASTER.                     00008880
00889      MOVE CL-FORCEABLE-ERROR-CNT OF OLD-CLAIM-MASTER TO           00008890
00890           CL-FORCEABLE-ERROR-CNT OF CLAIM-MASTER.                 00008900
00891      MOVE CL-BENEFICIARY OF OLD-CLAIM-MASTER TO                   00008910
00892           CL-BENEFICIARY OF CLAIM-MASTER.                         00008920
00893                                                                   00008930
00894      MOVE HIR-CLAIM-KEY          TO  WS-HIR-CLAIM-KEY.            00008940
00895      MOVE WS-HIR-COMPANY-CD      TO  WS-HOR-COMPANY-CD.           00008950
00896      MOVE WS-HIR-CARRIER         TO  WS-HOR-CARRIER.              00008960
00897      MOVE WS-HIR-CLAIM-NO        TO  WS-HOR-CLAIM-NO.             00008970
00898      MOVE WS-HIR-CERT-NO         TO  WS-HOR-CERT-NO.              00008980
00899      MOVE WS-HOR-CLAIM-KEY       TO  HOR-CLAIM-KEY.               00008990
00900                                                                   00009000
00901      MOVE CLAIM-MASTER           TO  HOR-CLAIM-RECORD.            00009010
00902                                                                   00009020
00903      WRITE HISTORY-OUTPUT-CLAIM-RECORD.                           00009030
00904                                                                   00009040
00905      ADD 1                       TO  TOTAL-CL-RECORDS-OT.         00009050
00906                                                                   00009060
00907      GO TO 0020-READ-HISTORY.                                     00009070
00908                                                                   00009080
00909  0030-PROCESS-CM.                                                 00009090
00910                                                                   00009100
00911      IF HIR-RECORD-ID NOT EQUAL 'CM'                              00009110
00912         GO TO 0040-PROCESS-AT.                                    00009120
00913                                                                   00009130
00914      ADD 1 TO TOTAL-CM-RECORDS-IN.                                00009140
00915                                                                   00009150
00916      IF WS-LAST-RECORD-ID IS NOT EQUAL TO 'CL'                    00009160
00917          DISPLAY 'CERT RECORD HAS NO CLAIM REC ' HIR-CLAIM-KEY    00009170
00918          MOVE 'CERT RECORD HAS NO CLAIM REC'                      00009180
00919                                  TO  WS-ABEND-MESSAGE             00009190
00920          GO TO ABEND-PGM.                                         00009200
00921                                                                   00009210
00922      MOVE 'CM'                   TO  WS-LAST-RECORD-ID.           00009220
00923                                                                   00009230
00924      MOVE HIR-COMPANY-ID         TO  HOR-COMPANY-ID.              00009240
00925      MOVE HIR-DATE-ARCHIVED      TO  HOR-DATE-ARCHIVED.           00009250
00926      MOVE HIR-PURGED-CLAIM       TO  HOR-PURGED-CLAIM.            00009260
00927                                                                   00009270
00928      MOVE HIR-CLAIM-KEY          TO  WS-HIR-CLAIM-KEY.            00009280
00929      MOVE WS-HIR-COMPANY-CD      TO  WS-HOR-COMPANY-CD.           00009290
00930      MOVE WS-HIR-CARRIER         TO  WS-HOR-CARRIER.              00009300
00931      MOVE WS-HIR-CLAIM-NO        TO  WS-HOR-CLAIM-NO.             00009310
00932      MOVE WS-HIR-CERT-NO         TO  WS-HOR-CERT-NO.              00009320
00933      MOVE WS-HOR-CLAIM-KEY       TO  HOR-CLAIM-KEY.               00009330
00934                                                                   00009340
00935      MOVE HIR-CERTIFICATE-RECORD TO HOR-CERTIFICATE-RECORD.       00009350
00936                                                                   00009360
00937      WRITE HISTORY-OUTPUT-CERT-RECORD.                            00009370
00938                                                                   00009380
00939      ADD 1 TO TOTAL-CM-RECORDS-OT.                                00009390
00940                                                                   00009400
00941      GO TO 0020-READ-HISTORY.                                     00009410
00942                                                                   00009420
00943      EJECT                                                        00009430
00944                                                                   00009440
00945  0040-PROCESS-AT.                                                 00009450
00946                                                                   00009460
00947      IF HIR-RECORD-ID NOT EQUAL 'AT'                              00009470
00948         GO TO 0050-PROCESS-LA.                                    00009480
00949                                                                   00009490
00950      ADD 1 TO TOTAL-AT-RECORDS-IN.                                00009500
00951                                                                   00009510
00952      IF WS-LAST-RECORD-ID NOT EQUAL 'CM' AND 'AT' AND 'LA'        00009520
00953         ADD 1 TO TOTAL-AT-RECORDS-DROP                            00009530
00954         GO TO 0020-READ-HISTORY.                                  00009540
00955                                                                   00009550
00956      MOVE 'AT'                   TO WS-LAST-RECORD-ID.            00009560
00957                                                                   00009570
00958      MOVE HIR-ACTIVITY-TRAILER-RECORD TO OLD-TRLR-RECORD.         00009580
00959                                                                   00009590
00960      MOVE AT-TRAILER-TYPE OF OLD-TRLR-RECORD TO                   00009600
00961           WS-LAST-TRLR-TYPE.                                      00009610
00962      MOVE SPACES TO ACTIVITY-TRAILERS.                            00009620
00963      MOVE AT-RECORD-ID OF OLD-TRLR-RECORD TO                      00009630
00964           AT-RECORD-ID OF ACTIVITY-TRAILERS.                      00009640
00965      MOVE AT-COMPANY-CD OF OLD-TRLR-RECORD TO                     00009650
00966           AT-COMPANY-CD OF ACTIVITY-TRAILERS.                     00009660
00967      MOVE AT-CARRIER OF OLD-TRLR-RECORD TO                        00009670
00968           AT-CARRIER OF ACTIVITY-TRAILERS.                        00009680
00969      MOVE AT-CLAIM-NO OF OLD-TRLR-RECORD TO                       00009690
00970           AT-CLAIM-NO OF ACTIVITY-TRAILERS.                       00009700
00971      MOVE AT-CERT-NO OF OLD-TRLR-RECORD                           00009710
00972          TO AT-CERT-NO OF ACTIVITY-TRAILERS.                      00009720
00973      MOVE AT-SEQUENCE-NO OF OLD-TRLR-RECORD TO                    00009730
00974           AT-SEQUENCE-NO OF ACTIVITY-TRAILERS.                    00009740
00975      MOVE AT-TRAILER-TYPE OF OLD-TRLR-RECORD TO                   00009750
00976           AT-TRAILER-TYPE OF ACTIVITY-TRAILERS.                   00009760
00977      MOVE AT-RECORDED-DT OF OLD-TRLR-RECORD TO                    00009770
00978           AT-RECORDED-DT OF ACTIVITY-TRAILERS.                    00009780
00979      MOVE AT-RECORDED-BY OF OLD-TRLR-RECORD TO                    00009790
00980           AT-RECORDED-BY OF ACTIVITY-TRAILERS.                    00009800
00981      MOVE AT-LAST-MAINT-HHMMSS OF OLD-TRLR-RECORD TO              00009810
00982           AT-LAST-MAINT-HHMMSS OF ACTIVITY-TRAILERS.              00009820
00983                                                                   00009830
00984                                                                   00009840
00985                                                                   00009850
00986      MOVE SPACES                                                  00009860
00987          TO AT-TRAILER-BODY OF ACTIVITY-TRAILERS.                 00009870
00988                                                                   00009880
00989      IF PAYMENT-TR OF OLD-TRLR-RECORD                             00009890
00990         IF AT-PREV-LAST-PMT-AMT OF OLD-TRLR-RECORD NOT NUMERIC    00009900
00991            MOVE ZEROS TO AT-PREV-LAST-PMT-AMT OF OLD-TRLR-RECORD. 00009910
00992                                                                   00009920
00993      IF PAYMENT-TR OF OLD-TRLR-RECORD                             00009930
00994         IF AT-ADDL-RESERVE OF OLD-TRLR-RECORD NOT NUMERIC         00009940
00995            MOVE ZEROS TO AT-ADDL-RESERVE OF OLD-TRLR-RECORD.      00009950
00996                                                                   00009960
00997      IF PAYMENT-TR OF OLD-TRLR-RECORD                             00009970
00998         IF AT-EXPENSE-PER-PMT OF OLD-TRLR-RECORD NOT NUMERIC      00009980
00999            MOVE ZEROS TO AT-EXPENSE-PER-PMT OF OLD-TRLR-RECORD.   00009990
01000                                                                   00010000
01001      IF PAYMENT-TR OF OLD-TRLR-RECORD                             00010010
01002         IF AT-ELIMINATION-DAYS OF OLD-TRLR-RECORD NOT NUMERIC     00010020
01003              MOVE ZEROS TO AT-ELIMINATION-DAYS OF OLD-TRLR-RECORD.00010030
01004                                                                   00010040
01005      IF PAYMENT-TR OF OLD-TRLR-RECORD                             00010050
01006         IF AT-DAILY-RATE OF OLD-TRLR-RECORD NOT NUMERIC           00010060
01007            MOVE ZEROS TO AT-DAILY-RATE OF OLD-TRLR-RECORD.        00010070
01008                                                                   00010080
01009      IF PAYMENT-TR OF OLD-TRLR-RECORD                             00010090
01010         IF AT-AMOUNT-PAID OF OLD-TRLR-RECORD NOT NUMERIC          00010100
01011            MOVE ZEROS TO AT-AMOUNT-PAID OF OLD-TRLR-RECORD.       00010110
01012                                                                   00010120
01013      IF ADDRESS-TR OF OLD-TRLR-RECORD                             00010130
01014         IF AT-ZIP-CODE OF OLD-TRLR-RECORD NOT NUMERIC             00010140
01015            MOVE ZEROS TO AT-ZIP-CODE OF OLD-TRLR-RECORD.          00010150
01016                                                                   00010160
01017      IF ADDRESS-TR OF OLD-TRLR-RECORD                             00010170
01018         IF AT-PHONE-NO OF OLD-TRLR-RECORD NOT NUMERIC             00010180
01019            MOVE ZEROS TO AT-PHONE-NO OF OLD-TRLR-RECORD.          00010190
01020                                                                   00010200
01021      IF RESERVE-EXPENSE-TR OF OLD-TRLR-RECORD                     00010210
01022         ADD 1 TO TOTAL-1-TRLRS                                    00010220
01023         MOVE AT-MANUAL-SW OF OLD-TRLR-RECORD TO                   00010230
01024              AT-MANUAL-SW OF ACTIVITY-TRAILERS                    00010240
01025         MOVE AT-FUTURE-SW OF OLD-TRLR-RECORD TO                   00010250
01026              AT-FUTURE-SW OF ACTIVITY-TRAILERS                    00010260
01027         MOVE AT-PTC-SW OF OLD-TRLR-RECORD TO                      00010270
01028              AT-PTC-SW OF ACTIVITY-TRAILERS                       00010280
01029         MOVE AT-IBNR-SW OF OLD-TRLR-RECORD TO                     00010290
01030              AT-IBNR-SW OF ACTIVITY-TRAILERS                      00010300
01031         MOVE AT-PTC-LF-SW OF OLD-TRLR-RECORD TO                   00010310
01032              AT-PTC-LF-SW OF ACTIVITY-TRAILERS                    00010320
01033         MOVE AT-CDT-ACCESS-METHOD OF OLD-TRLR-RECORD TO           00010330
01034              AT-CDT-ACCESS-METHOD OF ACTIVITY-TRAILERS            00010340
01035         MOVE AT-PERCENT-OF-CDT OF OLD-TRLR-RECORD TO              00010350
01036              AT-PERCENT-OF-CDT OF ACTIVITY-TRAILERS               00010360
01037         MOVE AT-LAST-COMPUTED-DT OF OLD-TRLR-RECORD TO            00010370
01038              AT-LAST-COMPUTED-DT OF ACTIVITY-TRAILERS             00010380
01039         MOVE AT-FUTURE-RESERVE OF OLD-TRLR-RECORD TO              00010390
01040              AT-FUTURE-RESERVE OF ACTIVITY-TRAILERS               00010400
01041         MOVE AT-PAY-CURRENT-RESERVE OF OLD-TRLR-RECORD TO         00010410
01042              AT-PAY-CURRENT-RESERVE OF ACTIVITY-TRAILERS          00010420
01043         MOVE AT-IBNR-RESERVE OF OLD-TRLR-RECORD TO                00010430
01044              AT-IBNR-RESERVE OF ACTIVITY-TRAILERS                 00010440
01045         MOVE AT-INITIAL-MANUAL-RESERVE OF OLD-TRLR-RECORD TO      00010450
01046              AT-INITIAL-MANUAL-RESERVE OF ACTIVITY-TRAILERS       00010460
01047         MOVE AT-CURRENT-MANUAL-RESERVE OF OLD-TRLR-RECORD TO      00010470
01048              AT-CURRENT-MANUAL-RESERVE OF ACTIVITY-TRAILERS       00010480
01049         MOVE AT-ITD-ADDITIONAL-RESERVE OF OLD-TRLR-RECORD TO      00010490
01050              AT-ITD-ADDITIONAL-RESERVE OF ACTIVITY-TRAILERS       00010500
01051         MOVE AT-EXPENSE-METHOD OF OLD-TRLR-RECORD TO              00010510
01052              AT-EXPENSE-METHOD OF ACTIVITY-TRAILERS               00010520
01053         MOVE AT-EXPENSE-PERCENT OF OLD-TRLR-RECORD TO             00010530
01054              AT-EXPENSE-PERCENT OF ACTIVITY-TRAILERS              00010540
01055         MOVE AT-EXPENSE-DOLLAR OF OLD-TRLR-RECORD TO              00010550
01056              AT-EXPENSE-DOLLAR OF ACTIVITY-TRAILERS               00010560
01057         MOVE AT-ITD-PAID-EXPENSES OF OLD-TRLR-RECORD TO           00010570
01058              AT-ITD-PAID-EXPENSES OF ACTIVITY-TRAILERS            00010580
01059         MOVE AT-ITD-CHARGEABLE-EXPENSE OF OLD-TRLR-RECORD TO      00010590
01060              AT-ITD-CHARGEABLE-EXPENSE OF ACTIVITY-TRAILERS       00010600
01061         MOVE AT-ITD-LIFE-REFUNDS OF OLD-TRLR-RECORD TO            00010610
01062              AT-ITD-LIFE-REFUNDS OF ACTIVITY-TRAILERS             00010620
01063         MOVE AT-ITD-AH-REFUNDS OF OLD-TRLR-RECORD TO              00010630
01064              AT-ITD-AH-REFUNDS OF ACTIVITY-TRAILERS               00010640
01065         MOVE WS-BIN-RUN-DATE TO  AT-RESERVES-LAST-MAINT-DT        00010650
01066                              OF  ACTIVITY-TRAILERS                00010660
01067         MOVE AT-RESERVES-LAST-UPDATED-BY OF OLD-TRLR-RECORD TO    00010670
01068              AT-RESERVES-LAST-UPDATED-BY OF ACTIVITY-TRAILERS     00010680
01069                                                                   00010690
01070         MOVE AT-OPEN-CLOSE-HISTORY OF OLD-TRLR-RECORD (1) TO      00010700
01071              AT-OPEN-CLOSE-HISTORY OF ACTIVITY-TRAILERS (1)       00010710
01072         MOVE AT-OPEN-CLOSE-HISTORY OF OLD-TRLR-RECORD (2) TO      00010720
01073              AT-OPEN-CLOSE-HISTORY OF ACTIVITY-TRAILERS (2)       00010730
01074         MOVE AT-OPEN-CLOSE-HISTORY OF OLD-TRLR-RECORD (3) TO      00010740
01075              AT-OPEN-CLOSE-HISTORY OF ACTIVITY-TRAILERS (3)       00010750
01076         MOVE AT-OPEN-CLOSE-HISTORY OF OLD-TRLR-RECORD (4) TO      00010760
01077              AT-OPEN-CLOSE-HISTORY OF ACTIVITY-TRAILERS (4)       00010770
01078         MOVE AT-OPEN-CLOSE-HISTORY OF OLD-TRLR-RECORD (5) TO      00010780
01079              AT-OPEN-CLOSE-HISTORY OF ACTIVITY-TRAILERS (5)       00010790
01080         MOVE AT-OPEN-CLOSE-HISTORY OF OLD-TRLR-RECORD (6) TO      00010800
01081              AT-OPEN-CLOSE-HISTORY OF ACTIVITY-TRAILERS (6)       00010810
01082                                                                   00010820
01083      ELSE                                                         00010830
01084                                                                   00010840
01085      IF PAYMENT-TR OF OLD-TRLR-RECORD                             00010850
01086         ADD 1 TO TOTAL-2-TRLRS                                    00010860
01087         MOVE AT-PAYMENT-TYPE OF OLD-TRLR-RECORD TO                00010870
01088              AT-PAYMENT-TYPE OF ACTIVITY-TRAILERS                 00010880
01089         MOVE AT-CLAIM-TYPE OF OLD-TRLR-RECORD TO                  00010890
01090              AT-CLAIM-TYPE OF ACTIVITY-TRAILERS                   00010900
01091         MOVE AT-CLAIM-PREM-TYPE OF OLD-TRLR-RECORD TO             00010910
01092              AT-CLAIM-PREM-TYPE OF ACTIVITY-TRAILERS              00010920
01093         MOVE AT-AMOUNT-PAID OF OLD-TRLR-RECORD TO                 00010930
01094              AT-AMOUNT-PAID OF ACTIVITY-TRAILERS                  00010940
01095         MOVE AT-CHECK-NO OF OLD-TRLR-RECORD TO                    00010950
01096              AT-CHECK-NO OF ACTIVITY-TRAILERS                     00010960
01097         MOVE AT-PAID-FROM-DT OF OLD-TRLR-RECORD TO                00010970
01098              AT-PAID-FROM-DT OF ACTIVITY-TRAILERS                 00010980
01099         MOVE AT-PAID-THRU-DT OF OLD-TRLR-RECORD TO                00010990
01100              AT-PAID-THRU-DT OF ACTIVITY-TRAILERS                 00011000
01101         MOVE AT-DAYS-IN-PERIOD OF OLD-TRLR-RECORD TO              00011010
01102              AT-DAYS-IN-PERIOD OF ACTIVITY-TRAILERS               00011020
01103 *       MOVE AT-PAYEE-TYPE-CD OF OLD-TRLR-RECORD TO               00011030
01104 *            AT-PAYEE-TYPE-CD OF ACTIVITY-TRAILERS                00011040
01105         MOVE AT-PAYEES-NAME OF OLD-TRLR-RECORD TO                 00011050
01106              AT-PAYEES-NAME OF ACTIVITY-TRAILERS                  00011060
01107         MOVE AT-PAYMENT-ORIGIN OF OLD-TRLR-RECORD TO              00011070
01108              AT-PAYMENT-ORIGIN OF ACTIVITY-TRAILERS               00011080
01109         MOVE AT-CHECK-WRITTEN-DT OF OLD-TRLR-RECORD TO            00011090
01110              AT-CHECK-WRITTEN-DT OF ACTIVITY-TRAILERS             00011100
01111         MOVE AT-TO-BE-WRITTEN-DT OF OLD-TRLR-RECORD TO            00011110
01112              AT-TO-BE-WRITTEN-DT OF ACTIVITY-TRAILERS             00011120
01113         MOVE AT-VOID-DT OF OLD-TRLR-RECORD TO                     00011130
01114              AT-VOID-DT OF ACTIVITY-TRAILERS                      00011140
01115         MOVE AT-VOID-REASON OF OLD-TRLR-RECORD TO                 00011150
01116              AT-VOID-REASON OF ACTIVITY-TRAILERS                  00011160
01117         MOVE AT-ADDL-RESERVE OF OLD-TRLR-RECORD TO                00011170
01118              AT-ADDL-RESERVE OF ACTIVITY-TRAILERS                 00011180
01119         MOVE AT-EXPENSE-PER-PMT OF OLD-TRLR-RECORD TO             00011190
01120              AT-EXPENSE-PER-PMT OF ACTIVITY-TRAILERS              00011200
01121         MOVE AT-PMT-SELECT-DT OF OLD-TRLR-RECORD TO               00011210
01122              AT-PMT-SELECT-DT OF ACTIVITY-TRAILERS                00011220
01123         MOVE AT-PMT-ACCEPT-DT OF OLD-TRLR-RECORD TO               00011230
01124              AT-PMT-ACCEPT-DT OF ACTIVITY-TRAILERS                00011240
01125         MOVE AT-VOID-SELECT-DT OF OLD-TRLR-RECORD TO              00011250
01126              AT-VOID-SELECT-DT OF ACTIVITY-TRAILERS               00011260
01127         MOVE AT-VOID-ACCEPT-DT OF OLD-TRLR-RECORD TO              00011270
01128              AT-VOID-ACCEPT-DT OF ACTIVITY-TRAILERS               00011280
01129         MOVE AT-CHECK-QUE-CONTROL OF OLD-TRLR-RECORD TO           00011290
01130              AT-CHECK-QUE-CONTROL OF ACTIVITY-TRAILERS            00011300
01131         MOVE AT-CHECK-QUE-SEQUENCE OF OLD-TRLR-RECORD TO          00011310
01132              AT-CHECK-QUE-SEQUENCE OF ACTIVITY-TRAILERS           00011320
01133         MOVE AT-FORCE-CONTROL OF OLD-TRLR-RECORD TO               00011330
01134              AT-FORCE-CONTROL OF ACTIVITY-TRAILERS                00011340
01135         MOVE AT-PREV-LAST-PMT-DT OF OLD-TRLR-RECORD TO            00011350
01136              AT-PREV-LAST-PMT-DT OF ACTIVITY-TRAILERS             00011360
01137         MOVE AT-PREV-PAID-THRU-DT OF OLD-TRLR-RECORD TO           00011370
01138              AT-PREV-PAID-THRU-DT OF ACTIVITY-TRAILERS            00011380
01139         MOVE AT-PREV-LAST-PMT-AMT OF OLD-TRLR-RECORD TO           00011390
01140              AT-PREV-LAST-PMT-AMT OF ACTIVITY-TRAILERS            00011400
01141         MOVE AT-ELIMINATION-DAYS OF OLD-TRLR-RECORD TO            00011410
01142              AT-ELIMINATION-DAYS OF ACTIVITY-TRAILERS             00011420
01143         MOVE AT-DAILY-RATE OF OLD-TRLR-RECORD TO                  00011430
01144              AT-DAILY-RATE OF ACTIVITY-TRAILERS                   00011440
01145         MOVE AT-BENEFIT-TYPE OF OLD-TRLR-RECORD TO                00011450
01146              AT-BENEFIT-TYPE OF ACTIVITY-TRAILERS                 00011460
01147         MOVE AT-EXPENSE-TYPE OF OLD-TRLR-RECORD TO                00011470
01148              AT-EXPENSE-TYPE OF ACTIVITY-TRAILERS                 00011480
01149         MOVE AT-PAYMENT-LAST-UPDATED-BY OF OLD-TRLR-RECORD TO     00011490
01150              AT-PAYMENT-LAST-UPDATED-BY OF ACTIVITY-TRAILERS      00011500
01151         MOVE WS-BIN-RUN-DATE    TO  AT-PAYMENT-LAST-MAINT-DT      00011510
01152                                 OF  ACTIVITY-TRAILERS             00011520
01153         MOVE 'Y'                TO  AT-CASH-PAYMENT               00011530
01154                                 OF  ACTIVITY-TRAILERS             00011540
01155         MOVE 'N'                TO  AT-GROUPED-PAYMENT            00011550
01156                                 OF  ACTIVITY-TRAILERS             00011560
01157         MOVE +0                 TO  AT-PAYMENT-NOTE-SEQ-NO        00011570
01158                                 OF  ACTIVITY-TRAILERS             00011580
01159         IF INSURED-PAID OF OLD-TRLR-RECORD                        00011590
01160           MOVE 'I'              TO  AT-PAYEE-TYPE                 00011600
01161                                 OF  ACTIVITY-TRAILERS             00011610
01162           MOVE '1'              TO  AT-PAYEE-SEQ                  00011620
01163                                 OF  ACTIVITY-TRAILERS             00011630
01164         ELSE                                                      00011640
01165           IF BENEFICIARY-PAID OF OLD-TRLR-RECORD                  00011650
01166             MOVE 'B'            TO  AT-PAYEE-TYPE                 00011660
01167                                 OF  ACTIVITY-TRAILERS             00011670
01168             MOVE '1'            TO  AT-PAYEE-SEQ                  00011680
01169                                 OF  ACTIVITY-TRAILERS             00011690
01170           ELSE                                                    00011700
01171             IF ACCOUNT-PAID OF OLD-TRLR-RECORD                    00011710
01172               MOVE 'A'          TO  AT-PAYEE-TYPE                 00011720
01173                                 OF  ACTIVITY-TRAILERS             00011730
01174               MOVE '1'          TO  AT-PAYEE-SEQ                  00011740
01175                                 OF  ACTIVITY-TRAILERS             00011750
01176             ELSE                                                  00011760
01177               IF OTHER-1-PAID OF OLD-TRLR-RECORD                  00011770
01178                 MOVE 'O'        TO  AT-PAYEE-TYPE                 00011780
01179                                 OF  ACTIVITY-TRAILERS             00011790
01180                 MOVE '1'        TO  AT-PAYEE-SEQ                  00011800
01181                                 OF  ACTIVITY-TRAILERS             00011810
01182               ELSE                                                00011820
01183                 IF OTHER-2-PAID OF OLD-TRLR-RECORD                00011830
01184                   MOVE 'Q'      TO  AT-PAYEE-TYPE                 00011840
01185                                 OF  ACTIVITY-TRAILERS             00011850
01186                   MOVE '1'      TO  AT-PAYEE-SEQ                  00011860
01187                                 OF  ACTIVITY-TRAILERS             00011870
01188                 ELSE                                              00011880
01189                   IF DOCTOR-PAID OF OLD-TRLR-RECORD               00011890
01190                     MOVE 'P'    TO  AT-PAYEE-TYPE                 00011900
01191                                 OF  ACTIVITY-TRAILERS             00011910
01192                     MOVE '1'    TO  AT-PAYEE-SEQ                  00011920
01193                                 OF  ACTIVITY-TRAILERS             00011930
01194                   ELSE                                            00011940
01195                     NEXT SENTENCE                                 00011950
01196      ELSE                                                         00011960
01197                                                                   00011970
01198      IF AUTO-PAY-TR OF OLD-TRLR-RECORD                            00011980
01199         ADD 1 TO TOTAL-3-TRLRS                                    00011990
01200         MOVE AT-SCHEDULE-START-DT OF OLD-TRLR-RECORD TO           00012000
01201              AT-SCHEDULE-START-DT OF ACTIVITY-TRAILERS            00012010
01202         MOVE AT-SCHEDULE-END-DT OF OLD-TRLR-RECORD TO             00012020
01203              AT-SCHEDULE-END-DT OF ACTIVITY-TRAILERS              00012030
01204         MOVE AT-TERMINATED-DT OF OLD-TRLR-RECORD TO               00012040
01205              AT-TERMINATED-DT OF ACTIVITY-TRAILERS                00012050
01206         MOVE AT-LAST-PMT-TYPE OF OLD-TRLR-RECORD TO               00012060
01207              AT-LAST-PMT-TYPE OF ACTIVITY-TRAILERS                00012070
01208         MOVE AT-FIRST-PMT-AMT OF OLD-TRLR-RECORD TO               00012080
01209              AT-FIRST-PMT-AMT OF ACTIVITY-TRAILERS                00012090
01210         MOVE AT-DAYS-IN-1ST-PMT OF OLD-TRLR-RECORD TO             00012100
01211              AT-DAYS-IN-1ST-PMT OF ACTIVITY-TRAILERS              00012110
01212         MOVE AT-1ST-PAY-THRU-DT OF OLD-TRLR-RECORD TO             00012120
01213              AT-1ST-PAY-THRU-DT OF ACTIVITY-TRAILERS              00012130
01214         MOVE AT-REGULAR-PMT-AMT OF OLD-TRLR-RECORD TO             00012140
01215              AT-REGULAR-PMT-AMT OF ACTIVITY-TRAILERS              00012150
01216         MOVE AT-DAYS-IN-REG-PMT OF OLD-TRLR-RECORD TO             00012160
01217              AT-DAYS-IN-REG-PMT OF ACTIVITY-TRAILERS              00012170
01218         MOVE AT-INTERVAL-MONTHS OF OLD-TRLR-RECORD TO             00012180
01219              AT-INTERVAL-MONTHS OF ACTIVITY-TRAILERS              00012190
01220 *       MOVE AT-AUTO-PAYEE-CD OF OLD-TRLR-RECORD TO               00012200
01221 *            AT-AUTO-PAYEE-CD OF ACTIVITY-TRAILERS                00012210
01222         MOVE AT-AUTO-PAY-LAST-UPDATED-BY OF OLD-TRLR-RECORD TO    00012220
01223              AT-AUTO-PAY-LAST-UPDATED-BY OF ACTIVITY-TRAILERS     00012230
01224         MOVE ZEROS               TO  AT-AUTO-PAY-DAY              00012240
01225         MOVE WS-BIN-RUN-DATE     TO  AT-AUTO-PAY-LAST-MAINT-DT    00012250
01226         IF INSURED-PAID-AUTO OF OLD-TRLR-RECORD                   00012260
01227           MOVE 'I'              TO  AT-AUTO-PAYEE-TYPE            00012270
01228                                 OF  ACTIVITY-TRAILERS             00012280
01229           MOVE '1'              TO  AT-AUTO-PAYEE-SEQ             00012290
01230                                 OF  ACTIVITY-TRAILERS             00012300
01231         ELSE                                                      00012310
01232           IF BENEFICIARY-PAID-AUTO OF OLD-TRLR-RECORD             00012320
01233             MOVE 'B'            TO  AT-AUTO-PAYEE-TYPE            00012330
01234                                 OF  ACTIVITY-TRAILERS             00012340
01235             MOVE '1'            TO  AT-AUTO-PAYEE-SEQ             00012350
01236                                 OF  ACTIVITY-TRAILERS             00012360
01237           ELSE                                                    00012370
01238             IF ACCOUNT-PAID-AUTO OF OLD-TRLR-RECORD               00012380
01239               MOVE 'A'          TO  AT-AUTO-PAYEE-TYPE            00012390
01240                                 OF  ACTIVITY-TRAILERS             00012400
01241               MOVE '1'          TO  AT-AUTO-PAYEE-SEQ             00012410
01242                                 OF  ACTIVITY-TRAILERS             00012420
01243             ELSE                                                  00012430
01244               IF OTHER-1-PAID-AUTO OF OLD-TRLR-RECORD             00012440
01245                 MOVE 'O'        TO  AT-AUTO-PAYEE-TYPE            00012450
01246                                 OF  ACTIVITY-TRAILERS             00012460
01247                 MOVE '1'        TO  AT-AUTO-PAYEE-SEQ             00012470
01248                                 OF  ACTIVITY-TRAILERS             00012480
01249               ELSE                                                00012490
01250                 IF OTHER-2-PAID-AUTO OF OLD-TRLR-RECORD           00012500
01251                   MOVE 'Q'      TO  AT-AUTO-PAYEE-TYPE            00012510
01252                                 OF  ACTIVITY-TRAILERS             00012520
01253                   MOVE '1'      TO  AT-AUTO-PAYEE-SEQ             00012530
01254                                 OF  ACTIVITY-TRAILERS             00012540
01255                 ELSE                                              00012550
01256                   NEXT SENTENCE                                   00012560
01257                                                                   00012570
01258      ELSE                                                         00012580
01259                                                                   00012590
01260      IF CORRESPONDENCE-TR OF OLD-TRLR-RECORD                      00012600
01261         ADD 1 TO TOTAL-4-TRLRS                                    00012610
01262         MOVE AT-LETTER-SENT-DT OF OLD-TRLR-RECORD TO              00012620
01263              AT-LETTER-SENT-DT OF ACTIVITY-TRAILERS               00012630
01264         MOVE AT-RECEIPT-FOLLOW-UP OF OLD-TRLR-RECORD TO           00012640
01265              AT-RECEIPT-FOLLOW-UP OF ACTIVITY-TRAILERS            00012650
01266         MOVE AT-AUTO-RE-SEND-DT OF OLD-TRLR-RECORD TO             00012660
01267              AT-AUTO-RE-SEND-DT OF ACTIVITY-TRAILERS              00012670
01268         MOVE AT-LETTER-ANSWERED-DT OF OLD-TRLR-RECORD TO          00012680
01269              AT-LETTER-ANSWERED-DT OF ACTIVITY-TRAILERS           00012690
01270         MOVE AT-LETTER-ARCHIVE-NO OF OLD-TRLR-RECORD TO           00012700
01271              AT-LETTER-ARCHIVE-NO OF ACTIVITY-TRAILERS            00012710
01272         MOVE AT-LETTER-ORIGIN OF OLD-TRLR-RECORD TO               00012720
01273              AT-LETTER-ORIGIN OF ACTIVITY-TRAILERS                00012730
01274         MOVE AT-STD-LETTER-FORM OF OLD-TRLR-RECORD TO             00012740
01275              AT-STD-LETTER-FORM OF ACTIVITY-TRAILERS              00012750
01276         MOVE AT-REASON-TEXT OF OLD-TRLR-RECORD TO                 00012760
01277              AT-REASON-TEXT OF ACTIVITY-TRAILERS                  00012770
01278         MOVE AT-ADDRESS-REC-SEQ-NO OF OLD-TRLR-RECORD TO          00012780
01279              AT-ADDRESS-REC-SEQ-NO OF ACTIVITY-TRAILERS           00012790
01280 *       MOVE AT-ADDRESEE-TYPE OF OLD-TRLR-RECORD TO               00012800
01281 *            AT-ADDRESEE-TYPE OF ACTIVITY-TRAILERS                00012810
01282         MOVE AT-ADDRESSEE-NAME OF OLD-TRLR-RECORD TO              00012820
01283              AT-ADDRESSEE-NAME OF ACTIVITY-TRAILERS               00012830
01284         MOVE AT-INITIAL-PRINT-DATE OF OLD-TRLR-RECORD TO          00012840
01285              AT-INITIAL-PRINT-DATE OF ACTIVITY-TRAILERS           00012850
01286         MOVE AT-RESEND-PRINT-DATE OF OLD-TRLR-RECORD TO           00012860
01287              AT-RESEND-PRINT-DATE OF ACTIVITY-TRAILERS            00012870
01288         MOVE AT-CORR-SOL-UNSOL       OF OLD-TRLR-RECORD TO        00012880
01289              AT-CORR-SOL-UNSOL       OF ACTIVITY-TRAILERS         00012890
01290         MOVE AT-CORR-LAST-UPDATED-BY OF OLD-TRLR-RECORD TO        00012900
01291              AT-CORR-LAST-UPDATED-BY OF ACTIVITY-TRAILERS         00012910
01292         MOVE WS-BIN-RUN-DATE     TO  AT-CORR-LAST-MAINT-DT        00012920
01293         IF INSURED-ADDRESEE OF OLD-TRLR-RECORD                    00012930
01294           MOVE 'I'               TO  AT-ADDRESEE-TYPE             00012940
01295                                  OF  ACTIVITY-TRAILERS            00012950
01296         ELSE                                                      00012960
01297           IF BENEFICIARY-ADDRESEE OF OLD-TRLR-RECORD              00012970
01298             MOVE 'B'             TO  AT-ADDRESEE-TYPE             00012980
01299                                  OF  ACTIVITY-TRAILERS            00012990
01300           ELSE                                                    00013000
01301             IF ACCOUNT-ADDRESEE OF OLD-TRLR-RECORD                00013010
01302               MOVE 'A'           TO  AT-ADDRESEE-TYPE             00013020
01303                                  OF  ACTIVITY-TRAILERS            00013030
01304             ELSE                                                  00013040
01305               IF PHYSICIAN-ADDRESEE OF OLD-TRLR-RECORD            00013050
01306                 MOVE 'P'         TO  AT-ADDRESEE-TYPE             00013060
01307                                  OF  ACTIVITY-TRAILERS            00013070
01308               ELSE                                                00013080
01309                 IF EMPLOYER-ADDRESEE OF OLD-TRLR-RECORD           00013090
01310                   MOVE 'E'       TO  AT-ADDRESEE-TYPE             00013100
01311                                  OF  ACTIVITY-TRAILERS            00013110
01312                 ELSE                                              00013120
01313                   IF OTHER-ADDRESEE-1 OF OLD-TRLR-RECORD          00013130
01314                     MOVE 'O'     TO  AT-ADDRESEE-TYPE             00013140
01315                                  OF  ACTIVITY-TRAILERS            00013150
01316                   ELSE                                            00013160
01317                     IF OTHER-ADDRESEE-2 OF OLD-TRLR-RECORD        00013170
01318                       MOVE 'Q'   TO  AT-ADDRESEE-TYPE             00013180
01319                                  OF  ACTIVITY-TRAILERS            00013190
01320                     ELSE                                          00013200
01321                       NEXT SENTENCE                               00013210
01322                                                                   00013220
01323      ELSE                                                         00013230
01324                                                                   00013240
01325      IF ADDRESS-TR OF OLD-TRLR-RECORD                             00013250
01326         ADD 1 TO TOTAL-5-TRLRS                                    00013260
01327 *       MOVE AT-ADDRESS-TYPE OF OLD-TRLR-RECORD TO                00013270
01328 *            AT-ADDRESS-TYPE OF ACTIVITY-TRAILERS                 00013280
01329         MOVE AT-MAIL-TO-NAME OF OLD-TRLR-RECORD TO                00013290
01330              AT-MAIL-TO-NAME OF ACTIVITY-TRAILERS                 00013300
01331         MOVE AT-ADDRESS-LINE-1 OF OLD-TRLR-RECORD TO              00013310
01332              AT-ADDRESS-LINE-1 OF ACTIVITY-TRAILERS               00013320
01333         MOVE AT-ADDRESS-LINE-2 OF OLD-TRLR-RECORD TO              00013330
01334              AT-ADDRESS-LINE-2 OF ACTIVITY-TRAILERS               00013340
01335         MOVE AT-CITY-STATE OF OLD-TRLR-RECORD TO                  00013350
01336              AT-CITY-STATE OF ACTIVITY-TRAILERS                   00013360
01337         MOVE AT-ZIP OF OLD-TRLR-RECORD TO                         00013370
01338              AT-ZIP OF ACTIVITY-TRAILERS                          00013380
01339         MOVE AT-PHONE-NO OF OLD-TRLR-RECORD TO                    00013390
01340              AT-PHONE-NO OF ACTIVITY-TRAILERS                     00013400
01341         MOVE AT-ADDRESS-LAST-UPDATED-BY OF OLD-TRLR-RECORD TO     00013410
01342              AT-ADDRESS-LAST-UPDATED-BY OF ACTIVITY-TRAILERS      00013420
01343         MOVE WS-BIN-RUN-DATE TO AT-ADDRESS-LAST-MAINT-DT          00013430
01344         IF INSURED-ADDRESS OF OLD-TRLR-RECORD                     00013440
01345           MOVE 'I'               TO  AT-ADDRESS-TYPE              00013450
01346                                  OF  ACTIVITY-TRAILERS            00013460
01347           MOVE +1                TO  AT-SEQUENCE-NO               00013470
01348                                  OF  ACTIVITY-TRAILERS            00013480
01349         ELSE                                                      00013490
01350           IF BENEFICIARY-ADDRESS OF OLD-TRLR-RECORD               00013500
01351             MOVE 'B'             TO  AT-ADDRESS-TYPE              00013510
01352                                  OF  ACTIVITY-TRAILERS            00013520
01353             MOVE +11             TO  AT-SEQUENCE-NO               00013530
01354                                  OF  ACTIVITY-TRAILERS            00013540
01355           ELSE                                                    00013550
01356             IF ACCOUNT-ADDRESS OF OLD-TRLR-RECORD                 00013560
01357               MOVE 'A'           TO  AT-ADDRESS-TYPE              00013570
01358                                  OF  ACTIVITY-TRAILERS            00013580
01359               MOVE +21           TO  AT-SEQUENCE-NO               00013590
01360                                  OF  ACTIVITY-TRAILERS            00013600
01361             ELSE                                                  00013610
01362               IF PHYSICIAN-ADDRESS OF OLD-TRLR-RECORD             00013620
01363                 MOVE 'P'         TO  AT-ADDRESS-TYPE              00013630
01364                                  OF  ACTIVITY-TRAILERS            00013640
01365                 MOVE +31         TO  AT-SEQUENCE-NO               00013650
01366                                  OF  ACTIVITY-TRAILERS            00013660
01367               ELSE                                                00013670
01368                 IF EMPLOYER-ADDRESS OF OLD-TRLR-RECORD            00013680
01369                   MOVE 'E'       TO  AT-ADDRESS-TYPE              00013690
01370                                  OF  ACTIVITY-TRAILERS            00013700
01371                   MOVE +41       TO  AT-SEQUENCE-NO               00013710
01372                                  OF  ACTIVITY-TRAILERS            00013720
01373                 ELSE                                              00013730
01374                   IF OTHER-ADDRESS-1 OF OLD-TRLR-RECORD           00013740
01375                     MOVE 'O'     TO  AT-ADDRESS-TYPE              00013750
01376                                  OF  ACTIVITY-TRAILERS            00013760
01377                     MOVE +51     TO  AT-SEQUENCE-NO               00013770
01378                                  OF  ACTIVITY-TRAILERS            00013780
01379                   ELSE                                            00013790
01380                     IF OTHER-ADDRESS-2 OF OLD-TRLR-RECORD         00013800
01381                       MOVE 'Q'   TO  AT-ADDRESS-TYPE              00013810
01382                                  OF  ACTIVITY-TRAILERS            00013820
01383                       MOVE +61   TO  AT-SEQUENCE-NO               00013830
01384                                  OF  ACTIVITY-TRAILERS            00013840
01385                     ELSE                                          00013850
01386                       NEXT SENTENCE                               00013860
01387      ELSE                                                         00013870
01388                                                                   00013880
01389      IF GENERAL-INFO-TR OF OLD-TRLR-RECORD                        00013890
01390         ADD 1 TO TOTAL-6-TRLRS                                    00013900
01391         MOVE AT-INFO-LINE-1 OF OLD-TRLR-RECORD TO                 00013910
01392              AT-INFO-LINE-1 OF ACTIVITY-TRAILERS                  00013920
01393         MOVE AT-INFO-LINE-2 OF OLD-TRLR-RECORD TO                 00013930
01394              AT-INFO-LINE-2 OF ACTIVITY-TRAILERS                  00013940
01395         MOVE AT-GEN-INFO-LAST-UPDATED-BY OF OLD-TRLR-RECORD TO    00013950
01396              AT-GEN-INFO-LAST-UPDATED-BY OF ACTIVITY-TRAILERS     00013960
01397         MOVE WS-BIN-RUN-DATE TO AT-GEN-INFO-LAST-MAINT-DT         00013970
01398                                                                   00013980
01399      ELSE                                                         00013990
01400                                                                   00014000
01401      IF AUTO-PROMPT-TR OF OLD-TRLR-RECORD                         00014010
01402         ADD 1 TO TOTAL-7-TRLRS                                    00014020
01403         MOVE AT-PROMPT-LINE-1 OF OLD-TRLR-RECORD TO               00014030
01404              AT-PROMPT-LINE-1 OF ACTIVITY-TRAILERS                00014040
01405         MOVE AT-PROMPT-LINE-2 OF OLD-TRLR-RECORD TO               00014050
01406              AT-PROMPT-LINE-2 OF ACTIVITY-TRAILERS                00014060
01407         MOVE AT-PROMPT-START-DT OF OLD-TRLR-RECORD TO             00014070
01408              AT-PROMPT-START-DT OF ACTIVITY-TRAILERS              00014080
01409         MOVE AT-PROMPT-END-DT OF OLD-TRLR-RECORD TO               00014090
01410              AT-PROMPT-END-DT OF ACTIVITY-TRAILERS                00014100
01411         MOVE AT-PROMPT-LAST-UPDATED-BY OF OLD-TRLR-RECORD TO      00014110
01412              AT-PROMPT-LAST-UPDATED-BY OF ACTIVITY-TRAILERS       00014120
01413         MOVE WS-BIN-RUN-DATE TO AT-PROMPT-LAST-MAINT-DT           00014130
01414                                                                   00014140
01415      ELSE                                                         00014150
01416                                                                   00014160
01417      IF DENIAL-TR OF OLD-TRLR-RECORD                              00014170
01418         ADD 1 TO TOTAL-8-TRLRS                                    00014180
01419         MOVE AT-DENIAL-INFO-1 OF OLD-TRLR-RECORD TO               00014190
01420              AT-DENIAL-INFO-1 OF ACTIVITY-TRAILERS                00014200
01421         MOVE AT-DENIAL-INFO-2 OF OLD-TRLR-RECORD TO               00014210
01422              AT-DENIAL-INFO-2 OF ACTIVITY-TRAILERS                00014220
01423         MOVE AT-DENIAL-DT OF OLD-TRLR-RECORD TO                   00014230
01424              AT-DENIAL-DT OF ACTIVITY-TRAILERS                    00014240
01425         MOVE AT-RETRACTION-DT OF OLD-TRLR-RECORD TO               00014250
01426              AT-RETRACTION-DT OF ACTIVITY-TRAILERS                00014260
01427         MOVE AT-DENIAL-REASON-CODE OF OLD-TRLR-RECORD TO          00014270
01428              AT-DENIAL-REASON-CODE OF ACTIVITY-TRAILERS           00014280
01429         MOVE AT-DENIAL-LAST-UPDATED-BY OF OLD-TRLR-RECORD TO      00014290
01430              AT-DENIAL-LAST-UPDATED-BY OF ACTIVITY-TRAILERS       00014300
01431         MOVE WS-BIN-RUN-DATE TO AT-DENIAL-LAST-MAINT-DT           00014310
01432                                                                   00014320
01433      ELSE                                                         00014330
01434                                                                   00014340
01435      IF INCURRED-CHG-TR OF OLD-TRLR-RECORD                        00014350
01436         ADD 1 TO TOTAL-9-TRLRS                                    00014360
01437         MOVE AT-OLD-INCURRED-DT OF OLD-TRLR-RECORD TO             00014370
01438              AT-OLD-INCURRED-DT OF ACTIVITY-TRAILERS              00014380
01439         MOVE AT-OLD-REPORTED-DT OF OLD-TRLR-RECORD TO             00014390
01440              AT-OLD-REPORTED-DT OF ACTIVITY-TRAILERS              00014400
01441         MOVE AT-OLD-ESTABLISHED-DT OF OLD-TRLR-RECORD TO          00014410
01442              AT-OLD-ESTABLISHED-DT OF ACTIVITY-TRAILERS           00014420
01443         MOVE AT-OLD-TOTAL-PAID OF OLD-TRLR-RECORD TO              00014430
01444              AT-OLD-TOTAL-PAID OF ACTIVITY-TRAILERS               00014440
01445         MOVE AT-OLD-DAYS-PAID OF OLD-TRLR-RECORD TO               00014450
01446              AT-OLD-DAYS-PAID OF ACTIVITY-TRAILERS                00014460
01447         MOVE AT-OLD-NO-OF-PMTS OF OLD-TRLR-RECORD TO              00014470
01448              AT-OLD-NO-OF-PMTS OF ACTIVITY-TRAILERS               00014480
01449         MOVE AT-OLD-PAID-THRU-DT OF OLD-TRLR-RECORD TO            00014490
01450              AT-OLD-PAID-THRU-DT OF ACTIVITY-TRAILERS             00014500
01451         MOVE AT-LAST-PMT-MADE-DT OF OLD-TRLR-RECORD TO            00014510
01452              AT-LAST-PMT-MADE-DT OF ACTIVITY-TRAILERS             00014520
01453         MOVE AT-OLD-DIAG-DESCRIP OF OLD-TRLR-RECORD TO            00014530
01454              AT-OLD-DIAG-DESCRIP OF ACTIVITY-TRAILERS             00014540
01455         MOVE AT-OLD-DIAG-CODE OF OLD-TRLR-RECORD TO               00014550
01456              AT-OLD-DIAG-CODE OF ACTIVITY-TRAILERS                00014560
01457         MOVE AT-TRAILER-CNT-AT-CHG OF OLD-TRLR-RECORD TO          00014570
01458              AT-TRAILER-CNT-AT-CHG OF ACTIVITY-TRAILERS           00014580
01459         MOVE AT-OLD-ITD-PAID-EXPENSE OF OLD-TRLR-RECORD TO        00014590
01460              AT-OLD-ITD-PAID-EXPENSE OF ACTIVITY-TRAILERS         00014600
01461         MOVE AT-OLD-CHARGABLE-EXPENSE OF OLD-TRLR-RECORD TO       00014610
01462              AT-OLD-CHARGABLE-EXPENSE OF ACTIVITY-TRAILERS        00014620
01463         MOVE AT-OLD-INIT-MAN-RESV OF OLD-TRLR-RECORD TO           00014630
01464              AT-OLD-INIT-MAN-RESV OF ACTIVITY-TRAILERS            00014640
01465         MOVE AT-OLD-CURRENT-MAN-RESV OF OLD-TRLR-RECORD TO        00014650
01466              AT-OLD-CURRENT-MAN-RESV OF ACTIVITY-TRAILERS         00014660
01467         MOVE AT-OLD-ADDL-MAN-RESV OF OLD-TRLR-RECORD TO           00014670
01468              AT-OLD-ADDL-MAN-RESV OF ACTIVITY-TRAILERS            00014680
01469         MOVE AT-INCURRED-LAST-UPDATED-BY OF OLD-TRLR-RECORD TO    00014690
01470              AT-INCURRED-LAST-UPDATED-BY OF ACTIVITY-TRAILERS     00014700
01471                                                                   00014710
01472      ELSE                                                         00014720
01473                                                                   00014730
01474      IF FORM-CONTROL-TR OF OLD-TRLR-RECORD                        00014740
01475         ADD 1 TO TOTAL-A-TRLRS                                    00014750
01476         MOVE AT-FORM-SEND-ON-DT OF OLD-TRLR-RECORD TO             00014760
01477              AT-FORM-SEND-ON-DT OF ACTIVITY-TRAILERS              00014770
01478         MOVE AT-FORM-FOLLOW-UP-DT OF OLD-TRLR-RECORD TO           00014780
01479              AT-FORM-FOLLOW-UP-DT OF ACTIVITY-TRAILERS            00014790
01480         MOVE AT-FORM-RE-SEND-DT OF OLD-TRLR-RECORD TO             00014800
01481              AT-FORM-RE-SEND-DT OF ACTIVITY-TRAILERS              00014810
01482         MOVE AT-FORM-ANSWERED-DT OF OLD-TRLR-RECORD TO            00014820
01483              AT-FORM-ANSWERED-DT OF ACTIVITY-TRAILERS             00014830
01484         MOVE AT-FORM-PRINTED-DT OF OLD-TRLR-RECORD TO             00014840
01485              AT-FORM-PRINTED-DT OF ACTIVITY-TRAILERS              00014850
01486         MOVE AT-FORM-REPRINT-DT OF OLD-TRLR-RECORD TO             00014860
01487              AT-FORM-REPRINT-DT OF ACTIVITY-TRAILERS              00014870
01488         MOVE AT-FORM-TYPE OF OLD-TRLR-RECORD TO                   00014880
01489              AT-FORM-TYPE OF ACTIVITY-TRAILERS                    00014890
01490         MOVE AT-INSTRUCT-LN-1 OF OLD-TRLR-RECORD TO               00014900
01491              AT-INSTRUCT-LN-1 OF ACTIVITY-TRAILERS                00014910
01492         MOVE AT-INSTRUCT-LN-2 OF OLD-TRLR-RECORD TO               00014920
01493              AT-INSTRUCT-LN-2 OF ACTIVITY-TRAILERS                00014930
01494         MOVE AT-INSTRUCT-LN-3 OF OLD-TRLR-RECORD TO               00014940
01495              AT-INSTRUCT-LN-3 OF ACTIVITY-TRAILERS                00014950
01496         MOVE AT-FORM-ADDR-SEQ-NO OF OLD-TRLR-RECORD TO            00014960
01497              AT-FORM-ADDR-SEQ-NO OF ACTIVITY-TRAILERS             00014970
01498 *       MOVE AT-FORM-ADDRESS OF OLD-TRLR-RECORD TO                00014980
01499 *            AT-FORM-ADDRESS OF ACTIVITY-TRAILERS                 00014990
01500         MOVE AT-REL-CARR-1 OF OLD-TRLR-RECORD TO                  00015000
01501              AT-REL-CARR-1 OF ACTIVITY-TRAILERS                   00015010
01502         MOVE AT-REL-CLAIM-1 OF OLD-TRLR-RECORD TO                 00015020
01503              AT-REL-CLAIM-1 OF ACTIVITY-TRAILERS                  00015030
01504         MOVE AT-REL-CERT-1 OF OLD-TRLR-RECORD TO                  00015040
01505              AT-REL-CERT-1 OF ACTIVITY-TRAILERS                   00015050
01506         MOVE AT-REL-CARR-2 OF OLD-TRLR-RECORD TO                  00015060
01507              AT-REL-CARR-2 OF ACTIVITY-TRAILERS                   00015070
01508         MOVE AT-REL-CLAIM-2 OF OLD-TRLR-RECORD TO                 00015080
01509              AT-REL-CLAIM-2 OF ACTIVITY-TRAILERS                  00015090
01510         MOVE AT-REL-CERT-2 OF OLD-TRLR-RECORD TO                  00015100
01511              AT-REL-CERT-2 OF ACTIVITY-TRAILERS                   00015110
01512         MOVE AT-FORM-LAST-UPDATED-BY OF OLD-TRLR-RECORD TO        00015120
01513              AT-FORM-LAST-UPDATED-BY OF ACTIVITY-TRAILERS         00015130
01514         MOVE WS-BIN-RUN-DATE     TO  AT-FORM-LAST-MAINT-DT        00015140
01515                                  OF  ACTIVITY-TRAILERS            00015150
01516         MOVE LOW-VALUES          TO  AT-EMP-FORM-SEND-ON-DT       00015160
01517                                      AT-PHY-FORM-SEND-ON-DT       00015170
01518                                      AT-EMP-FORM-ANSWERED-DT      00015180
01519                                      AT-PHY-FORM-ANSWERED-DT      00015190
01520                                      AT-FORM-REM-PRINT-DT         00015200
01521         IF FORM-TO-INSURED OF OLD-TRLR-RECORD                     00015210
01522           MOVE 'I'               TO  AT-FORM-ADDRESS              00015220
01523                                  OF  ACTIVITY-TRAILERS            00015230
01524         ELSE                                                      00015240
01525           IF FORM-TO-ACCOUNT OF OLD-TRLR-RECORD                   00015250
01526             MOVE 'A'             TO  AT-FORM-ADDRESS              00015260
01527                                  OF  ACTIVITY-TRAILERS            00015270
01528           ELSE                                                    00015280
01529             IF FORM-TO-OTHER-1 OF OLD-TRLR-RECORD                 00015290
01530               MOVE 'O'           TO  AT-FORM-ADDRESS              00015300
01531                                  OF  ACTIVITY-TRAILERS            00015310
01532             ELSE                                                  00015320
01533               IF FORM-TO-OTHER-2 OF OLD-TRLR-RECORD               00015330
01534                 MOVE 'Q'         TO  AT-FORM-ADDRESS              00015340
01535                                  OF  ACTIVITY-TRAILERS            00015350
01536               ELSE                                                00015360
01537                 NEXT SENTENCE.                                    00015370
01538                                                                   00015380
01539      MOVE HIR-COMPANY-ID         TO HOR-COMPANY-ID.               00015390
01540      MOVE HIR-DATE-ARCHIVED      TO HOR-DATE-ARCHIVED.            00015400
01541      MOVE HIR-PURGED-CLAIM       TO HOR-PURGED-CLAIM.             00015410
01542                                                                   00015420
01543 *    MOVE CL-CONTROL-PRIMARY OF CLAIM-MASTER TO                   00015430
01544 *         HOR-CLAIM-KEY.                                          00015440
01545                                                                   00015450
01546      MOVE HIR-CLAIM-KEY          TO WS-HIR-CLAIM-KEY.             00015460
01547      MOVE WS-HIR-COMPANY-CD      TO WS-HOR-COMPANY-CD.            00015470
01548      MOVE WS-HIR-CARRIER         TO WS-HOR-CARRIER.               00015480
01549      MOVE WS-HIR-CLAIM-NO        TO WS-HOR-CLAIM-NO.              00015490
01550      MOVE WS-HIR-CERT-NO         TO WS-HOR-CERT-NO.               00015500
01551      MOVE WS-HOR-CLAIM-KEY       TO HOR-CLAIM-KEY.                00015510
01552                                                                   00015520
01553      MOVE ACTIVITY-TRAILERS      TO HOR-ACTIVITY-TRAILER-RECORD.  00015530
01554                                                                   00015540
01555      WRITE HISTORY-OUTPUT-TRAILER-RECORD.                         00015550
01556                                                                   00015560
01557      ADD 1 TO TOTAL-AT-RECORDS-OT.                                00015570
01558                                                                   00015580
01559      GO TO 0020-READ-HISTORY.                                     00015590
01560                                                                   00015600
01561      EJECT                                                        00015610
01562                                                                   00015620
01563  0050-PROCESS-LA.                                                 00015630
01564                                                                   00015640
01565      IF HIR-RECORD-ID NOT EQUAL 'LA'                              00015650
01566         DISPLAY 'INVALID  TRAILER TYPE ' HIR-CLAIM-KEY            00015660
01567         DISPLAY 'INVALID  TRAILER TYPE ' HIR-CLAIM-KEY            00015670
01568                 UPON CONSOLE.                                     00015680
01569                                                                   00015690
01570      ADD 1 TO TOTAL-LA-RECORDS-IN.                                00015700
01571                                                                   00015710
01572      IF WS-LAST-RECORD-ID NOT EQUAL 'CM' AND 'AT' AND 'LA'        00015720
01573         ADD 1 TO TOTAL-LA-RECORDS-DROP                            00015730
01574         GO TO 0020-READ-HISTORY.                                  00015740
01575 *       DISPLAY 'LETT OUT OF SEQUENCE ' HIR-CLAIM-KEY             00015750
01576 *       DISPLAY 'LETT OUT OF SEQUENCE ' HIR-CLAIM-KEY             00015760
01577 *                 UPON CONSOLE.                                   00015770
01578                                                                   00015780
01579      MOVE 'LA'                   TO WS-LAST-RECORD-ID.            00015790
01580                                                                   00015800
01581      MOVE HIR-LETTER-ARCHIVE-RECORD TO OLD-ARCH-RECORD.           00015810
01582                                                                   00015820
01583      MOVE SPACES TO LETTER-ARCHIVE.                               00015830
01584                                                                   00015840
01585      MOVE LA-RECORD-ID OF OLD-ARCH-RECORD TO                      00015850
01586           LA-RECORD-ID OF LETTER-ARCHIVE.                         00015860
01587      MOVE LA-COMPANY-CD OF OLD-ARCH-RECORD TO                     00015870
01588           LA-COMPANY-CD OF LETTER-ARCHIVE.                        00015880
01589      MOVE LA-ARCHIVE-NO OF OLD-ARCH-RECORD TO                     00015890
01590           LA-ARCHIVE-NO OF LETTER-ARCHIVE.                        00015900
01591      MOVE LA-RECORD-TYPE OF OLD-ARCH-RECORD TO                    00015910
01592           LA-RECORD-TYPE OF LETTER-ARCHIVE.                       00015920
01593      MOVE LA-LINE-SEQ-NO OF OLD-ARCH-RECORD TO                    00015930
01594           LA-LINE-SEQ-NO OF LETTER-ARCHIVE.                       00015940
01595      MOVE LA-COMPANY-CD-A1 OF OLD-ARCH-RECORD TO                  00015950
01596           LA-COMPANY-CD-A1 OF LETTER-ARCHIVE.                     00015960
01597      MOVE LA-RECORD-TYPE-A1 OF OLD-ARCH-RECORD TO                 00015970
01598           LA-RECORD-TYPE-A1 OF LETTER-ARCHIVE.                    00015980
01599      MOVE LA-ARCHIVE-NO-A1 OF OLD-ARCH-RECORD TO                  00015990
01600           LA-ARCHIVE-NO-A1 OF LETTER-ARCHIVE.                     00016000
01601      MOVE LA-LINE-SEQ-NO-A1 OF OLD-ARCH-RECORD TO                 00016010
01602           LA-LINE-SEQ-NO-A1 OF LETTER-ARCHIVE.                    00016020
01603                                                                   00016030
01604      IF LA-RECORD-TYPE OF OLD-ARCH-RECORD EQUAL '3'               00016040
01605         MOVE LA-TEXT-LINE OF OLD-ARCH-RECORD TO                   00016050
01606              LA-TEXT-LINE OF LETTER-ARCHIVE                       00016060
01607         MOVE LA-SKIP-CONTROL OF OLD-ARCH-RECORD TO                00016070
01608              LA-SKIP-CONTROL OF LETTER-ARCHIVE                    00016080
01609                                                                   00016090
01610      ELSE                                                         00016100
01611                                                                   00016110
01612      IF LA-RECORD-TYPE OF OLD-ARCH-RECORD EQUAL '2'               00016120
01613         MOVE LA-ADDRESS-LINE OF OLD-ARCH-RECORD TO                00016130
01614              LA-ADDRESS-LINE OF LETTER-ARCHIVE                    00016140
01615                                                                   00016150
01616      ELSE                                                         00016160
01617                                                                   00016170
01618      IF LA-RECORD-TYPE OF OLD-ARCH-RECORD EQUAL '1'               00016180
01619         MOVE LA-CARRIER OF OLD-ARCH-RECORD TO                     00016190
01620              LA-CARRIER OF LETTER-ARCHIVE                         00016200
01621         MOVE LA-CLAIM-NO OF OLD-ARCH-RECORD TO                    00016210
01622              LA-CLAIM-NO OF LETTER-ARCHIVE                        00016220
01623         MOVE LA-CERT-NO OF OLD-ARCH-RECORD TO                     00016230
01624              LA-CERT-NO OF LETTER-ARCHIVE                         00016240
01625         MOVE LA-NO-OF-COPIES OF OLD-ARCH-RECORD TO                00016250
01626              LA-NO-OF-COPIES OF LETTER-ARCHIVE                    00016260
01627         MOVE LA-RESEND-DATE OF OLD-ARCH-RECORD TO                 00016270
01628              LA-RESEND-DATE OF LETTER-ARCHIVE                     00016280
01629         MOVE LA-PROCESSOR-CD OF OLD-ARCH-RECORD TO                00016290
01630              LA-PROCESSOR-CD OF LETTER-ARCHIVE                    00016300
01631         MOVE LA-CREATION-DT OF OLD-ARCH-RECORD TO                 00016310
01632              LA-CREATION-DT OF LETTER-ARCHIVE                     00016320
01633         MOVE LA-INITIAL-PRINT-DATE OF OLD-ARCH-RECORD TO          00016330
01634              LA-INITIAL-PRINT-DATE OF LETTER-ARCHIVE              00016340
01635         MOVE LA-RESEND-PRINT-DATE OF OLD-ARCH-RECORD TO           00016350
01636              LA-RESEND-PRINT-DATE OF LETTER-ARCHIVE               00016360
01637         MOVE LA-CORR-TRLR-SEQ OF OLD-ARCH-RECORD TO               00016370
01638              LA-CORR-TRLR-SEQ OF LETTER-ARCHIVE                   00016380
01639         IF WS-LAST-TRLR-TYPE NOT EQUAL '4'                        00016390
01640            ADD 1 TO TOTAL-BAD-LA1-RECS                            00016400
01641         ELSE                                                      00016410
01642            NEXT SENTENCE                                          00016420
01643                                                                   00016430
01644      ELSE                                                         00016440
01645                                                                   00016450
01646      IF LA-RECORD-TYPE OF OLD-ARCH-RECORD EQUAL '4'               00016460
01647         MOVE LA4-CARRIER OF OLD-ARCH-RECORD TO                    00016470
01648              LA4-CARRIER OF LETTER-ARCHIVE                        00016480
01649         MOVE LA4-CLAIM-NO OF OLD-ARCH-RECORD TO                   00016490
01650              LA4-CLAIM-NO OF LETTER-ARCHIVE                       00016500
01651         MOVE LA4-CERT-NO OF OLD-ARCH-RECORD TO                    00016510
01652              LA4-CERT-NO OF LETTER-ARCHIVE                        00016520
01653         MOVE LA4-NO-OF-COPIES OF OLD-ARCH-RECORD TO               00016530
01654              LA4-NO-OF-COPIES OF LETTER-ARCHIVE                   00016540
01655         MOVE LA4-RESEND-DATE OF OLD-ARCH-RECORD TO                00016550
01656              LA4-RESEND-DATE OF LETTER-ARCHIVE                    00016560
01657         MOVE LA4-PROCESSOR-CD OF OLD-ARCH-RECORD TO               00016570
01658              LA4-PROCESSOR-CD OF LETTER-ARCHIVE                   00016580
01659         MOVE LA4-CREATION-DT OF OLD-ARCH-RECORD TO                00016590
01660              LA4-CREATION-DT OF LETTER-ARCHIVE                    00016600
01661         MOVE LA4-INITIAL-PRINT-DATE OF OLD-ARCH-RECORD TO         00016610
01662              LA4-INITIAL-PRINT-DATE OF LETTER-ARCHIVE             00016620
01663         MOVE LA4-RESEND-PRINT-DATE OF OLD-ARCH-RECORD TO          00016630
01664              LA4-RESEND-PRINT-DATE OF LETTER-ARCHIVE              00016640
01665         MOVE LA4-FORM-TRLR-SEQ OF OLD-ARCH-RECORD TO              00016650
01666              LA4-FORM-TRLR-SEQ OF LETTER-ARCHIVE                  00016660
01667         MOVE LA4-FORM-TYPE OF OLD-ARCH-RECORD TO                  00016670
01668              LA4-FORM-TYPE OF LETTER-ARCHIVE                      00016680
01669         MOVE LOW-VALUES      TO LA4-FORM-REM-PRINT-DT             00016690
01670         IF WS-LAST-TRLR-TYPE NOT EQUAL 'A'                        00016700
01671            ADD 1 TO TOTAL-BAD-LA4-RECS.                           00016710
01672                                                                   00016720
01673      MOVE HIR-COMPANY-ID         TO HOR-COMPANY-ID.               00016730
01674      MOVE HIR-DATE-ARCHIVED      TO HOR-DATE-ARCHIVED.            00016740
01675      MOVE HIR-PURGED-CLAIM       TO HOR-PURGED-CLAIM.             00016750
01676                                                                   00016760
01677 *    MOVE CL-CONTROL-PRIMARY OF CLAIM-MASTER TO                   00016770
01678 *         HOR-CLAIM-KEY.                                          00016780
01679                                                                   00016790
01680      MOVE HIR-CLAIM-KEY          TO WS-HIR-CLAIM-KEY.             00016800
01681      MOVE WS-HIR-COMPANY-CD      TO WS-HOR-COMPANY-CD.            00016810
01682      MOVE WS-HIR-CARRIER         TO WS-HOR-CARRIER.               00016820
01683      MOVE WS-HIR-CLAIM-NO        TO WS-HOR-CLAIM-NO.              00016830
01684      MOVE WS-HIR-CERT-NO         TO WS-HOR-CERT-NO.               00016840
01685      MOVE WS-HOR-CLAIM-KEY       TO HOR-CLAIM-KEY.                00016850
01686                                                                   00016860
01687      MOVE LETTER-ARCHIVE         TO HOR-LETTER-ARCHIVE-RECORD.    00016870
01688                                                                   00016880
01689      WRITE HISTORY-OUTPUT-LETTER-RECORD.                          00016890
01690                                                                   00016900
01691      ADD 1 TO TOTAL-LA-RECORDS-OT.                                00016910
01692                                                                   00016920
01693      GO TO 0020-READ-HISTORY.                                     00016930
01694                                                                   00016940
01695      EJECT                                                        00016950
01696  8500-DATE-CONVERSION.           COPY ELCDCS.                     00016960
01697                                                                   00016970
01698      EJECT                                                        00016980
01699                                                                   00016990
01700  9000-EOJ-ROUTINE.                                                00017000
01701         DISPLAY SPACES                                            00017010
01702         DISPLAY '*************************************'           00017020
01703         DISPLAY ' CL IN RECS ' TOTAL-CL-RECORDS-IN                00017030
01704         DISPLAY ' CL OT RECS ' TOTAL-CL-RECORDS-OT                00017040
01705         DISPLAY ' CM IN RECS ' TOTAL-CM-RECORDS-IN                00017050
01706         DISPLAY ' CM OT RECS ' TOTAL-CM-RECORDS-OT                00017060
01707         DISPLAY ' AT IN RECS ' TOTAL-AT-RECORDS-IN                00017070
01708         DISPLAY ' AT OT RECS ' TOTAL-AT-RECORDS-OT                00017080
01709         DISPLAY ' AT DP RECS ' TOTAL-AT-RECORDS-DROP              00017090
01710         DISPLAY ' LA IN RECS ' TOTAL-LA-RECORDS-IN                00017100
01711         DISPLAY ' LA OT RECS ' TOTAL-LA-RECORDS-OT                00017110
01712         DISPLAY ' LA DP RECS ' TOTAL-LA-RECORDS-DROP              00017120
01713         DISPLAY ' 1 TRAILERS ' TOTAL-1-TRLRS                      00017130
01714         DISPLAY ' 2 TRAILERS ' TOTAL-2-TRLRS                      00017140
01715         DISPLAY ' 3 TRAILERS ' TOTAL-3-TRLRS                      00017150
01716         DISPLAY ' 4 TRAILERS ' TOTAL-4-TRLRS                      00017160
01717         DISPLAY ' 5 TRAILERS ' TOTAL-5-TRLRS                      00017170
01718         DISPLAY ' 6 TRAILERS ' TOTAL-6-TRLRS                      00017180
01719         DISPLAY ' 7 TRAILERS ' TOTAL-7-TRLRS                      00017190
01720         DISPLAY ' 8 TRAILERS ' TOTAL-8-TRLRS                      00017200
01721         DISPLAY ' 9 TRAILERS ' TOTAL-9-TRLRS                      00017210
01722         DISPLAY ' A TRAILERS ' TOTAL-A-TRLRS                      00017220
01723         DISPLAY ' LA HDR W/O CORRES TRLR ' TOTAL-BAD-LA1-RECS     00017230
01724         DISPLAY ' LA FRM W/O FORMS  TRLR ' TOTAL-BAD-LA4-RECS     00017240
01725         DISPLAY '*************************************'           00017250
01726         DISPLAY SPACES.                                           00017260
01727                                                                   00017270
01728      ADD TOTAL-CL-RECORDS-IN TO G-CL-RECORDS-IN                   00017280
01729      ADD TOTAL-CL-RECORDS-OT TO G-CL-RECORDS-OT                   00017290
01730      ADD TOTAL-CM-RECORDS-IN TO G-CM-RECORDS-IN                   00017300
01731      ADD TOTAL-CM-RECORDS-OT TO G-CM-RECORDS-OT                   00017310
01732      ADD TOTAL-AT-RECORDS-IN TO G-AT-RECORDS-IN                   00017320
01733      ADD TOTAL-AT-RECORDS-OT TO G-AT-RECORDS-OT                   00017330
01734      ADD TOTAL-LA-RECORDS-IN TO G-LA-RECORDS-IN                   00017340
01735      ADD TOTAL-LA-RECORDS-OT TO G-LA-RECORDS-OT                   00017350
01736      ADD TOTAL-AT-RECORDS-DROP TO G-AT-RECORDS-DROP               00017360
01737      ADD TOTAL-LA-RECORDS-DROP TO G-LA-RECORDS-DROP.              00017370
01738      ADD TOTAL-1-TRLRS       TO G-TOTAL-1-TRLRS                   00017380
01739      ADD TOTAL-2-TRLRS       TO G-TOTAL-2-TRLRS                   00017390
01740      ADD TOTAL-3-TRLRS       TO G-TOTAL-3-TRLRS                   00017400
01741      ADD TOTAL-4-TRLRS       TO G-TOTAL-4-TRLRS                   00017410
01742      ADD TOTAL-5-TRLRS       TO G-TOTAL-5-TRLRS                   00017420
01743      ADD TOTAL-6-TRLRS       TO G-TOTAL-6-TRLRS                   00017430
01744      ADD TOTAL-7-TRLRS       TO G-TOTAL-7-TRLRS                   00017440
01745      ADD TOTAL-8-TRLRS       TO G-TOTAL-8-TRLRS                   00017450
01746      ADD TOTAL-9-TRLRS       TO G-TOTAL-9-TRLRS                   00017460
01747      ADD TOTAL-A-TRLRS       TO G-TOTAL-A-TRLRS                   00017470
01748      ADD TOTAL-BAD-LA1-RECS  TO G-TOTAL-BAD-LA1-RECS              00017480
01749      ADD TOTAL-BAD-LA4-RECS  TO G-TOTAL-BAD-LA4-RECS              00017490
01750                                                                   00017500
01751      MOVE ZEROS TO TOTAL-CL-RECORDS-IN                            00017510
01752      MOVE ZEROS TO TOTAL-CL-RECORDS-OT                            00017520
01753      MOVE ZEROS TO TOTAL-CM-RECORDS-IN                            00017530
01754      MOVE ZEROS TO TOTAL-CM-RECORDS-OT                            00017540
01755      MOVE ZEROS TO TOTAL-AT-RECORDS-IN                            00017550
01756      MOVE ZEROS TO TOTAL-AT-RECORDS-OT                            00017560
01757      MOVE ZEROS TO TOTAL-LA-RECORDS-IN                            00017570
01758      MOVE ZEROS TO TOTAL-LA-RECORDS-OT                            00017580
01759      MOVE ZEROS TO TOTAL-AT-RECORDS-DROP                          00017590
01760      MOVE ZEROS TO TOTAL-LA-RECORDS-DROP.                         00017600
01761      MOVE ZEROS TO TOTAL-1-TRLRS                                  00017610
01762      MOVE ZEROS TO TOTAL-2-TRLRS                                  00017620
01763      MOVE ZEROS TO TOTAL-3-TRLRS                                  00017630
01764      MOVE ZEROS TO TOTAL-4-TRLRS                                  00017640
01765      MOVE ZEROS TO TOTAL-5-TRLRS                                  00017650
01766      MOVE ZEROS TO TOTAL-6-TRLRS                                  00017660
01767      MOVE ZEROS TO TOTAL-7-TRLRS                                  00017670
01768      MOVE ZEROS TO TOTAL-8-TRLRS                                  00017680
01769      MOVE ZEROS TO TOTAL-9-TRLRS                                  00017690
01770      MOVE ZEROS TO TOTAL-A-TRLRS                                  00017700
01771      MOVE ZEROS TO TOTAL-BAD-LA1-RECS                             00017710
01772      MOVE ZEROS TO TOTAL-BAD-LA4-RECS.                            00017720
01773                                                                   00017730
01774      DISPLAY SPACES.                                              00017740
01775      DISPLAY '  G R A N D   T O T A L S '.                        00017750
01776      DISPLAY '***********************************************'.   00017760
01777      DISPLAY '  INPUT CL RECORDS ....... ' G-CL-RECORDS-IN.       00017770
01778      DISPLAY '  INPUT CM RECORDS ....... ' G-CM-RECORDS-IN.       00017780
01779      DISPLAY '  INPUT AT RECORDS ....... ' G-AT-RECORDS-IN.       00017790
01780      DISPLAY '  INPUT LA RECORDS ....... ' G-LA-RECORDS-IN.       00017800
01781                                                                   00017810
01782      DISPLAY '  OUTPUT CL RECORDS ....... ' G-CL-RECORDS-OT.      00017820
01783      DISPLAY '  OUTPUT CM RECORDS ....... ' G-CM-RECORDS-OT.      00017830
01784      DISPLAY '  OUTPUT AT RECORDS ....... ' G-AT-RECORDS-OT.      00017840
01785      DISPLAY '  OUTPUT LA RECORDS ....... ' G-LA-RECORDS-OT.      00017850
01786      DISPLAY '  DROPED AT RECORDS ....... ' G-AT-RECORDS-DROP.    00017860
01787      DISPLAY '  DROPED LA RECORDS ....... ' G-LA-RECORDS-DROP.    00017870
01788      DISPLAY '  1 TRAILERS        ....... ' G-TOTAL-1-TRLRS.      00017880
01789      DISPLAY '  2 TRAILERS        ....... ' G-TOTAL-2-TRLRS.      00017890
01790      DISPLAY '  3 TRAILERS        ....... ' G-TOTAL-3-TRLRS.      00017900
01791      DISPLAY '  4 TRAILERS        ....... ' G-TOTAL-4-TRLRS.      00017910
01792      DISPLAY '  5 TRAILERS        ....... ' G-TOTAL-5-TRLRS.      00017920
01793      DISPLAY '  6 TRAILERS        ....... ' G-TOTAL-6-TRLRS.      00017930
01794      DISPLAY '  7 TRAILERS        ....... ' G-TOTAL-7-TRLRS.      00017940
01795      DISPLAY '  8 TRAILERS        ....... ' G-TOTAL-8-TRLRS.      00017950
01796      DISPLAY '  9 TRAILERS        ....... ' G-TOTAL-9-TRLRS.      00017960
01797      DISPLAY '  A TRAILERS        ....... ' G-TOTAL-A-TRLRS.      00017970
01798      DISPLAY '  HDR LA W/O CORRS TLRL ... ' G-TOTAL-BAD-LA1-RECS. 00017980
01799      DISPLAY '  FRM LA W/O FORMS TLRL ... ' G-TOTAL-BAD-LA4-RECS. 00017990
01800      DISPLAY '***********************************************'.   00018000
01801                                                                   00018010
01802      CLOSE HISTORY-IN                                             00018020
01803            HISTORY-OT.                                            00018030
01804                                                                   00018040
01805      GOBACK.                                                      00018050
01806      EJECT                                                        00018060
01807                                                                   00018070
01808  ABEND-PGM SECTION. COPY ELCABEND.                                00018080
01809                                                                   00018090
