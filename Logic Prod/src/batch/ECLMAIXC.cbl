00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 ECLMAIXC.                            00000030
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
00021 *REMARKS.                                                         00000210
00022 *        ADD FIRST NAME AND MIDDLE INITIAL TO NAME ALTERNATE      00000220
00023 *        INDEX.                                                   00000230
00024                                                                   00000240
00025  EJECT                                                            00000250
00026  ENVIRONMENT DIVISION.                                            00000260
00027  CONFIGURATION SECTION.                                           00000270
00028  INPUT-OUTPUT SECTION.                                            00000280
00029  FILE-CONTROL.                                                    00000290
00030                                                                   00000300
00031      SELECT CLAIM-MASTER-IN  ASSIGN TO SYS010-UT-2400-S-SYS010.   00000310
00032                                                                   00000320
00033      SELECT CLAIM-MASTER-OUT ASSIGN TO SYS011-UT-2400-S-SYS011.   00000330
00034                                                                   00000340
00035  EJECT                                                            00000350
00036  DATA DIVISION.                                                   00000360
00037  FILE SECTION.                                                    00000370
00038                                                                   00000380
00039  FD  CLAIM-MASTER-IN                                              00000390
00040      LABEL RECORDS ARE STANDARD                                   00000400
00041      BLOCK CONTAINS 0 RECORDS                                     00000410
00042      RECORD CONTAINS 270 CHARACTERS.                              00000420
00043                                                                   00000430
00044 ******************************************************************00000440
00045 *                                                                *00000450
00046 *                            ELCMSTR.                            *00000460
00047 *                            VMOD=2.003                          *00000470
00048 *                                                                *00000480
00049 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *00000490
00050 *                                                                *00000500
00051 *   FILE TYPE = VSAM,KSDS                                        *00000510
00052 *   RECORD SIZE = 270  RECFORM = FIXED                           *00000520
00053 *                                                                *00000530
00054 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *00000540
00055 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=16  *00000550
00056 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=38,LEN=12  *00000560
00057 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=50,LEN=12  *00000570
00058 *                                                                *00000580
00059 *   LOG = YES                                                    *00000590
00060 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *00000600
00061 ******************************************************************00000610
00062  01  OLD-CLAIM-MASTER.                                            00000620
00063      12  CL-RECORD-ID                PIC XX.                      00000630
00064          88  VALID-CL-ID         VALUE 'CL'.                      00000640
00065                                                                   00000650
00066      12  CL-CONTROL-PRIMARY.                                      00000660
00067          16  CL-COMPANY-CD           PIC X.                       00000670
00068          16  CL-CARRIER              PIC X.                       00000680
00069          16  CL-CLAIM-NO             PIC X(7).                    00000690
00070          16  CL-CERT-NO.                                          00000700
00071              20  CL-CERT-PRIME       PIC X(10).                   00000710
00072              20  CL-CERT-SFX         PIC X.                       00000720
00073                                                                   00000730
00074      12  CL-CONTROL-BY-NAME.                                      00000740
00075          16  CL-COMPANY-CD-A1        PIC X.                       00000750
00076          16  CL-INSURED-LAST-NAME    PIC X(15).                   00000760
00077                                                                   00000770
00078      12  CL-CONTROL-BY-SSN.                                       00000780
00079          16  CL-COMPANY-CD-A2        PIC X.                       00000790
00080          16  CL-SOC-SEC-NO.                                       00000800
00081              20  CL-SSN-STATE        PIC XX.                      00000810
00082              20  CL-SSN-ACCOUNT      PIC X(6).                    00000820
00083              20  CL-SSN-LN3          PIC X(3).                    00000830
00084                                                                   00000840
00085      12  CL-CONTROL-BY-CERT-NO.                                   00000850
00086          16  CL-COMPANY-CD-A4        PIC X.                       00000860
00087          16  CL-CERT-NO-A4.                                       00000870
00088              20  CL-CERT-A4-PRIME    PIC X(10).                   00000880
00089              20  CL-CERT-A4-SFX      PIC X.                       00000890
00090                                                                   00000900
00091      12  CL-INSURED-PROFILE-DATA.                                 00000910
00092          16  CL-INSURED-NAME.                                     00000920
00093              20  CL-INSURED-1ST-NAME PIC X(12).                   00000930
00094              20  CL-INSURED-MID-INIT PIC X.                       00000940
00095          16  CL-INSURED-BIRTH-DT     PIC XX.                      00000950
00096          16  CL-INSURED-SEX-CD       PIC X.                       00000960
00097              88  INSURED-IS-MALE        VALUE 'M'.                00000970
00098              88  INSURED-IS-FEMALE      VALUE 'F'.                00000980
00099              88  INSURED-SEX-UNKNOWN    VALUE ' '.                00000990
00100          16  CL-INSURED-OCC-CD       PIC XX.                      00001000
00101          16  FILLER                  PIC X(5).                    00001010
00102                                                                   00001020
00103      12  CL-PROCESSING-INFO.                                      00001030
00104          16  CL-PROCESSOR-ID         PIC X(4).                    00001040
00105          16  CL-CLAIM-STATUS         PIC X.                       00001050
00106              88  CLAIM-IS-OPEN          VALUE 'O'.                00001060
00107              88  CLAIM-IS-CLOSED        VALUE 'C'.                00001070
00108          16  CL-CLAIM-TYPE           PIC X.                       00001080
00109 *            88  AH-CLAIM               VALUE 'A'.                00001090
00110 *            88  LIFE-CLAIM             VALUE 'L'.                00001100
00111 *            88  PROPERTY-CLAIM         VALUE 'P'.                00001110
00112 *            88  UNEMPLOYMENT-CLAIM     VALUE 'U'.                00001120
00113          16  CL-CLAIM-PREM-TYPE      PIC X.                       00001130
00114              88  SINGLE-PREMIUM         VALUE '1'.                00001140
00115              88  O-B-COVERAGE           VALUE '2'.                00001150
00116              88  OPEN-END-COVERAGE      VALUE '3'.                00001160
00117          16  CL-INCURRED-DT          PIC XX.                      00001170
00118          16  CL-REPORTED-DT          PIC XX.                      00001180
00119          16  CL-FILE-ESTABLISH-DT    PIC XX.                      00001190
00120          16  CL-EST-END-OF-DISAB-DT  PIC XX.                      00001200
00121          16  CL-LAST-PMT-DT          PIC XX.                      00001210
00122          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.        00001220
00123          16  CL-PAID-THRU-DT         PIC XX.                      00001230
00124          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.        00001240
00125          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.        00001250
00126          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.          00001260
00127          16  CL-PMT-CALC-METHOD      PIC X.                       00001270
00128              88  CL-360-DAY-YR          VALUE '1'.                00001280
00129              88  CL-365-DAY-YR          VALUE '2'.                00001290
00130              88  CL-FULL-MONTHS         VALUE '3'.                00001300
00131          16  CL-CAUSE-CD             PIC X(6).                    00001310
00132                                                                   00001320
00133          16  CL-PRIME-CERT-NO.                                    00001330
00134              20  CL-PRIME-CERT-PRIME PIC X(10).                   00001340
00135              20  CL-PRIME-CERT-SFX   PIC X.                       00001350
00136                                                                   00001360
00137          16  FILLER                  PIC X(12).                   00001370
00138                                                                   00001380
00139          16  CL-PROG-FORM-TYPE       PIC X.                       00001390
00140          16  CL-LAST-ADD-ON-DT       PIC XX.                      00001400
00141                                                                   00001410
00142          16  CL-LAST-REOPEN-DT       PIC XX.                      00001420
00143          16  CL-LAST-CLOSE-DT        PIC XX.                      00001430
00144          16  CL-LAST-CLOSE-REASON    PIC X.                       00001440
00145              88  FINAL-PAID             VALUE '1'.                00001450
00146              88  CLAIM-DENIED           VALUE '2'.                00001460
00147              88  AUTO-CLOSE             VALUE '3'.                00001470
00148              88  MANUAL-CLOSE           VALUE '4'.                00001480
00149          16  CL-ASSOC-CERT-SEQU      PIC S99.                     00001490
00150          16  CL-ASSOC-CERT-TOTAL     PIC S99.                     00001500
00151          16  CL-CLAIM-PAYMENT-STATUS PIC 9.                       00001510
00152              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.           00001520
00153          16  FILLER                  PIC X(05).                   00001530
00154                                                                   00001540
00155      12  CL-CERTIFICATE-DATA.                                     00001550
00156          16  CL-CERT-ORIGIN          PIC X.                       00001560
00157              88  CERT-WAS-ONLINE        VALUE '1'.                00001570
00158              88  CERT-WAS-CREATED       VALUE '2'.                00001580
00159              88  COVERAGE-WAS-ADDED     VALUE '3'.                00001590
00160          16  CL-CERT-KEY-DATA.                                    00001600
00161              20  CL-CERT-CARRIER     PIC X.                       00001610
00162              20  CL-CERT-GROUPING    PIC X(6).                    00001620
00163              20  CL-CERT-STATE       PIC XX.                      00001630
00164              20  CL-CERT-ACCOUNT.                                 00001640
00165                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).             00001650
00166                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).             00001660
00167              20  CL-CERT-EFF-DT      PIC XX.                      00001670
00168                                                                   00001680
00169      12  CL-STATUS-CONTROLS.                                      00001690
00170          16  CL-PRIORITY-CD          PIC X.                       00001700
00171              88  HIGHEST-PRIORITY       VALUE '9'.                00001710
00172          16  CL-SUPV-ATTN-CD         PIC X.                       00001720
00173              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.            00001730
00174              88  SUPV-IS-REQUIRED       VALUE 'Y'.                00001740
00175          16  CL-PURGED-DT            PIC XX.                      00001750
00176          16  CL-RESTORED-DT          PIC XX.                      00001760
00177          16  CL-NEXT-AUTO-PAY-DT     PIC XX.                      00001770
00178          16  CL-NEXT-RESEND-DT       PIC XX.                      00001780
00179          16  CL-NEXT-FOLLOWUP-DT     PIC XX.                      00001790
00180          16  FILLER                  PIC XX.                      00001800
00181          16  CL-LAST-MAINT-DT        PIC XX.                      00001810
00182          16  CL-LAST-MAINT-USER      PIC X(4).                    00001820
00183          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.        00001830
00184          16  CL-LAST-MAINT-TYPE      PIC X.                       00001840
00185              88  CLAIM-SET-UP           VALUE ' '.                00001850
00186              88  PAYMENT-MADE           VALUE '1'.                00001860
00187              88  LETTER-SENT            VALUE '2'.                00001870
00188              88  MASTER-WAS-ALTERED     VALUE '3'.                00001880
00189              88  MASTER-WAS-RESTORED    VALUE '4'.                00001890
00190              88  INCURRED-DATE-CHANGED  VALUE '5'.                00001900
00191              88  FILE-CONVERTED         VALUE '6'.                00001910
00192          16  CL-RELATED-CLAIM-NO     PIC X(7).                    00001920
00193          16  CL-HISTORY-ARCHIVE-DT   PIC XX.                      00001930
00194          16  CL-BENEFICIARY          PIC X(10).                   00001940
00195                                                                   00001950
00196      12  CL-TRAILER-CONTROLS.                                     00001960
00197          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.          00001970
00198              88  CL-1ST-TRL-AVAIL       VALUE +4095.              00001980
00199              88  CL-LAST-TRL-AVAIL      VALUE +100.               00001990
00200              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.                 00002000
00201          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.          00002010
00202          16  FILLER                  PIC XX.                      00002020
00203          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.          00002030
00204          16  CL-ADDRESS-TRAILER-CNT.                              00002040
00205              20  CL-INSURED-ADDR-CNT  PIC S9(1).                  00002050
00206                  88  NO-INSURED-AVAILABLE    VALUE ZERO.          00002060
00207              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).                  00002070
00208                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.          00002080
00209              20  CL-BENIF-ADDR-CNT    PIC S9(1).                  00002090
00210                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.          00002100
00211              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).                  00002110
00212                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.          00002120
00213              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).                  00002130
00214                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.          00002140
00215              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).                  00002150
00216                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.          00002160
00217              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).                  00002170
00218                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.          00002180
00219                                                                   00002190
00220      12  FILLER                      PIC X(7).                    00002200
00221                                                                   00002210
00222      12  CL-FILE-LOCATION            PIC X(4).                    00002220
00223                                                                   00002230
00224      12  CL-PROCESS-ERRORS.                                       00002240
00225          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.          00002250
00226              88  NO-FATAL-ERRORS        VALUE ZERO.               00002260
00227          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.          00002270
00228              88  NO-FORCABLE-ERRORS     VALUE ZERO.               00002280
00229                                                                   00002290
00230      12  FILLER                      PIC X(8).                    00002300
00231                                                                   00002310
00232 ******************************************************************00002320
00233      EJECT                                                        00002330
00234  FD  CLAIM-MASTER-OUT                                             00002340
00235      LABEL RECORDS ARE STANDARD                                   00002350
00236      BLOCK CONTAINS 0 RECORDS                                     00002360
00237      RECORD CONTAINS 270 CHARACTERS.                              00002370
00238                                                                   00002380
00239                              COPY ELCMSTR.                        00002390
00240                                                                   00002400
00241      EJECT                                                        00002410
00242  WORKING-STORAGE SECTION.                                         00002420
00243  77  FILLER  PIC X(32)  VALUE '********************************'. 00002430
00244  77  FILLER  PIC X(32)  VALUE '*   ECLMAIXC WORKING-STORAGE    '. 00002440
00245  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'. 00002441
00246                                                                   00002460
00247  77  CLAIM-IN-CNT            PIC S9(7)   COMP-3  VALUE +0.        00002470
00248  77  CLAIM-OUT-CNT           PIC S9(7)   COMP-3  VALUE +0.        00002480
00249                                                                   00002490
00250  01  WS.                                                          00002500
00251      12  WS-RETURN-CODE        PIC S9(4)   COMP   VALUE +0.       00002510
00252      12  WS-ABEND-MESSAGE      PIC X(80)          VALUE SPACES.   00002520
00253      12  WS-ABEND-FILE-STATUS  PIC XX             VALUE ZEROS.    00002530
00254      12  WS-ZERO               PIC S9      COMP-3 VALUE +0.       00002540
00255                                                                   00002550
00256                                                                   00002560
00257      EJECT                                                        00002570
00258  PROCEDURE DIVISION.                                              00002580
00259                                                                   00002590
00260 ******************************************************************00002600
00261 ***           O P E N   F I L E S   R O U T I N E              ***00002610
00262 ******************************************************************00002620
00263                                                                   00002630
00264  0000-OPEN-FILES.                                                 00002640
00265                                                                   00002650
00266      OPEN INPUT  CLAIM-MASTER-IN                                  00002660
00267           OUTPUT CLAIM-MASTER-OUT.                                00002670
00268                                                                   00002680
00269  0100-READ-CLAIM.                                                 00002690
00270                                                                   00002700
00271      READ CLAIM-MASTER-IN AT END                                  00002710
00272         GO TO 9999-EOJ-ROUTINE.                                   00002720
00273                                                                   00002730
00274      ADD +1                  TO CLAIM-IN-CNT.                     00002740
00275      MOVE SPACES             TO CLAIM-MASTER.                     00002750
00276                                                                   00002760
00277      MOVE 'CL'               TO CL-RECORD-ID                      00002770
00278                              OF CLAIM-MASTER.                     00002780
00279                                                                   00002790
00280      MOVE CL-CONTROL-PRIMARY OF OLD-CLAIM-MASTER                  00002800
00281                              TO CL-CONTROL-PRIMARY                00002810
00282                              OF CLAIM-MASTER.                     00002820
00283                                                                   00002830
00284      MOVE CL-INSURED-LAST-NAME                                    00002840
00285                              OF OLD-CLAIM-MASTER                  00002850
00286                              TO CL-INSURED-LAST-NAME              00002860
00287                              OF CLAIM-MASTER.                     00002870
00288                                                                   00002880
00289      MOVE CL-COMPANY-CD-A1   OF OLD-CLAIM-MASTER                  00002890
00290                              TO CL-COMPANY-CD-A1                  00002900
00291                              OF CLAIM-MASTER.                     00002910
00292                                                                   00002920
00293      MOVE CL-INSURED-NAME    OF OLD-CLAIM-MASTER                  00002930
00294                              TO CL-INSURED-NAME                   00002940
00295                              OF CLAIM-MASTER.                     00002950
00296                                                                   00002960
00297      MOVE CL-CONTROL-BY-SSN  OF OLD-CLAIM-MASTER                  00002970
00298                              TO CL-CONTROL-BY-SSN                 00002980
00299                              OF CLAIM-MASTER.                     00002990
00300                                                                   00003000
00301      MOVE CL-CONTROL-BY-CERT-NO                                   00003010
00302                              OF OLD-CLAIM-MASTER                  00003020
00303                              TO CL-CONTROL-BY-CERT-NO             00003030
00304                              OF CLAIM-MASTER.                     00003040
00305                                                                   00003050
00306      MOVE CL-INSURED-BIRTH-DT                                     00003060
00307                              OF OLD-CLAIM-MASTER                  00003070
00308                              TO CL-INSURED-BIRTH-DT               00003080
00309                              OF CLAIM-MASTER.                     00003090
00310                                                                   00003100
00311      MOVE CL-INSURED-SEX-CD  OF OLD-CLAIM-MASTER                  00003110
00312                              TO CL-INSURED-SEX-CD                 00003120
00313                              OF CLAIM-MASTER.                     00003130
00314                                                                   00003140
00315      MOVE CL-INSURED-OCC-CD  OF OLD-CLAIM-MASTER                  00003150
00316                              TO CL-INSURED-OCC-CD                 00003160
00317                              OF CLAIM-MASTER.                     00003170
00318                                                                   00003180
00319      MOVE CL-PROCESSING-INFO OF OLD-CLAIM-MASTER                  00003190
00320                              TO CL-PROCESSING-INFO                00003200
00321                              OF CLAIM-MASTER.                     00003210
00322                                                                   00003220
00323      MOVE CL-CERTIFICATE-DATA                                     00003230
00324                              OF OLD-CLAIM-MASTER                  00003240
00325                              TO CL-CERTIFICATE-DATA               00003250
00326                              OF CLAIM-MASTER.                     00003260
00327                                                                   00003270
00328      MOVE CL-STATUS-CONTROLS OF OLD-CLAIM-MASTER                  00003280
00329                              TO CL-STATUS-CONTROLS                00003290
00330                              OF CLAIM-MASTER.                     00003300
00331                                                                   00003310
00332      MOVE CL-TRAILER-CONTROLS                                     00003320
00333                              OF OLD-CLAIM-MASTER                  00003330
00334                              TO CL-TRAILER-CONTROLS               00003340
00335                              OF CLAIM-MASTER.                     00003350
00336                                                                   00003360
00337      MOVE CL-FILE-LOCATION   OF OLD-CLAIM-MASTER                  00003370
00338                              TO CL-FILE-LOCATION                  00003380
00339                              OF CLAIM-MASTER.                     00003390
00340                                                                   00003400
00341      MOVE CL-PROCESS-ERRORS  OF OLD-CLAIM-MASTER                  00003410
00342                              TO CL-PROCESS-ERRORS                 00003420
00343                              OF CLAIM-MASTER.                     00003430
00344                                                                   00003440
00345      EJECT                                                        00003450
00346  0200-WRITE-NEW-CLAIM.                                            00003460
00347                                                                   00003470
00348      WRITE CLAIM-MASTER.                                          00003480
00349                                                                   00003490
00350      ADD +1                  TO CLAIM-OUT-CNT.                    00003500
00351                                                                   00003510
00352      GO TO 0100-READ-CLAIM.                                       00003520
00353                                                                   00003530
00354      EJECT                                                        00003540
00355 ******************************************************************00003550
00356 ***          E N D   O F   J O B   P R O C E S S I N G         ***00003560
00357 ******************************************************************00003570
00358                                                                   00003580
00359  9999-EOJ-ROUTINE.                                                00003590
00360                                                                   00003600
00361      DISPLAY '****   CLAIM MASTER    READ = ' CLAIM-IN-CNT.       00003610
00362      DISPLAY '****   CLAIM MASTER WRITTEN = ' CLAIM-OUT-CNT.      00003620
00363                                                                   00003630
00364      CLOSE CLAIM-MASTER-IN                                        00003640
00365            CLAIM-MASTER-OUT.                                      00003650
00366                                                                   00003660
00367      GOBACK.                                                      00003670
00368                                                                   00003680
00369      EJECT                                                        00003690
00370  ABEND-PGM.                                                       00003700
00371                                  COPY ELCABEND.                   00003710
