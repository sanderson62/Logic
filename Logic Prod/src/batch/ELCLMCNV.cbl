00001  IDENTIFICATION DIVISION.                                         00000010
00002                                                                   00000020
00003  PROGRAM-ID.                 ELCLMCNV.                            00000030
00004 *                            VMOD=2.004.                          00000031
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
00022 *        CONVERT OLD CLAIM MASTER RECORD INTO REVISED             00000220
00023 *        RECORD LAYOUT WITH CLAIM / CERTIFICATE SEQUENCING.       00000230
00024                                                                   00000240
00025  EJECT                                                            00000250
00026  ENVIRONMENT DIVISION.                                            00000260
00027  CONFIGURATION SECTION.                                           00000270
00028  INPUT-OUTPUT SECTION.                                            00000280
00029  FILE-CONTROL.                                                    00000290
00030                                                                   00000300
00031      SELECT SORT-WORK        ASSIGN TO SORTWORK-S-SORTWK1.        00000310
00032                                                                   00000320
00033      SELECT CLAIM-MASTER-IN  ASSIGN TO INPUT-S-ELMSTRI.           00000330
00034                                                                   00000340
00035      SELECT CLAIM-MASTER-OUT ASSIGN TO OUTPUT-S-ELMSTRO.          00000350
00036                                                                   00000360
00037      SELECT TRAIL-MASTER-IN  ASSIGN TO INPUT-S-ELTRLRI.           00000370
00038                                                                   00000380
00039      SELECT TRAIL-MASTER-OUT ASSIGN TO OUTPUT-S-ELTRLRO.          00000390
00040                                                                   00000400
00041  EJECT                                                            00000410
00042  DATA DIVISION.                                                   00000420
00043  FILE SECTION.                                                    00000430
00044                                                                   00000440
00045  SD  SORT-WORK                                                    00000450
00046      RECORDING MODE IS F                                          00000460
00047      RECORD CONTAINS 200 CHARACTERS.                              00000470
00048  01  SW-RECORD.                                                   00000480
00049      12  FILLER                  PIC X(02).                       00000490
00050      12  SW-CONTROL-PRIMARY.                                      00000500
00051          16  SW-COMPANY-CD       PIC X.                           00000510
00052          16  SW-CARRIER          PIC X.                           00000520
00053          16  SW-CLAIM-NO         PIC X(7).                        00000530
00054          16  SW-CERT-NO.                                          00000540
00055              20  SW-CERT-PRIME   PIC X(10).                       00000550
00056              20  SW-CERT-SFX     PIC X(1).                        00000560
00057          16  SW-SEQUENCE-NO      PIC S9(4)         COMP.          00000570
00058      12  FILLER                  PIC X(176).                      00000580
00059                                                                   00000590
00060      EJECT                                                        00000600
00061  FD  CLAIM-MASTER-IN                                              00000610
00062      LABEL RECORDS ARE STANDARD                                   00000620
00063      RECORD CONTAINS 270 CHARACTERS                               00000630
00064      BLOCK CONTAINS 0 RECORDS                                     00000640
00065      DATA RECORD IS CLAIM-MASTER.                                 00000650
00066                                                                   00000660
00067  01  CLAIM-MASTER.                                                00000670
00068      12  CL-RECORD-ID                PIC XX.                      00000680
00069          88  VALID-CL-ID         VALUE 'CL'.                      00000690
00070                                                                   00000700
00071      12  CL-CONTROL-PRIMARY.                                      00000710
00072          16  CL-COMPANY-CD           PIC X.                       00000720
00073          16  CL-CARRIER              PIC X.                       00000730
00074          16  CL-CLAIM-NO             PIC X(7).                    00000740
00075          16  CL-CERT-NO.                                          00000750
00076              20  CL-CERT-PRIME       PIC X(10).                   00000760
00077              20  CL-CERT-SFX         PIC X.                       00000770
00078                                                                   00000780
00079      12  CL-CONTROL-BY-NAME.                                      00000790
00080          16  CL-COMPANY-CD-A1        PIC X.                       00000800
00081          16  CL-INSURED-LAST-NAME    PIC X(15).                   00000810
00082          16  CL-INSURED-NAME.                                     00000811
00083              20  CL-INSURED-1ST-NAME PIC X(12).                   00000812
00084              20  CL-INSURED-MID-INIT PIC X.                       00000813
00085                                                                   00000814
00086      12  CL-CONTROL-BY-SSN.                                       00000815
00087          16  CL-COMPANY-CD-A2        PIC X.                       00000816
00088          16  CL-SOC-SEC-NO.                                       00000817
00089              20  CL-SSN-STATE        PIC XX.                      00000818
00090              20  CL-SSN-ACCOUNT      PIC X(6).                    00000819
00091              20  CL-SSN-LN3          PIC X(3).                    00000820
00092                                                                   00000821
00093      12  CL-CONTROL-BY-CERT-NO.                                   00000822
00094          16  CL-COMPANY-CD-A4        PIC X.                       00000823
00095          16  CL-CERT-NO-A4.                                       00000824
00096              20  CL-CERT-A4-PRIME    PIC X(10).                   00000825
00097              20  CL-CERT-A4-SFX      PIC X.                       00000826
00098                                                                   00000827
00099      12  CL-INSURED-PROFILE-DATA.                                 00000828
00100          16  CL-INSURED-BIRTH-DT     PIC XX.                      00001000
00101          16  CL-INSURED-SEX-CD       PIC X.                       00001010
00102              88  INSURED-IS-MALE        VALUE 'M'.                00001020
00103              88  INSURED-IS-FEMALE      VALUE 'F'.                00001030
00104              88  INSURED-SEX-UNKNOWN    VALUE ' '.                00001040
00105          16  CL-INSURED-OCC-CD       PIC XX.                      00001050
00106          16  FILLER                  PIC X(5).                    00001060
00107                                                                   00001070
00108      12  CL-PROCESSING-INFO.                                      00001080
00109          16  CL-PROCESSOR-ID         PIC X(4).                    00001090
00110          16  CL-CLAIM-STATUS         PIC X.                       00001100
00111              88  CLAIM-IS-OPEN          VALUE 'O'.                00001110
00112              88  CLAIM-IS-CLOSED        VALUE 'C'.                00001120
00113          16  CL-CLAIM-TYPE           PIC X.                       00001130
00114 *            88  AH-CLAIM               VALUE 'A'.                00001140
00115 *            88  LIFE-CLAIM             VALUE 'L'.                00001150
00116          16  CL-CLAIM-PREM-TYPE      PIC X.                       00001160
00117              88  SINGLE-PREMIUM         VALUE '1'.                00001170
00118              88  O-B-COVERAGE           VALUE '2'.                00001180
00119              88  OPEN-END-COVERAGE      VALUE '3'.                00001190
00120          16  CL-INCURRED-DT          PIC XX.                      00001200
00121          16  CL-REPORTED-DT          PIC XX.                      00001210
00122          16  CL-FILE-ESTABLISH-DT    PIC XX.                      00001220
00123          16  CL-EST-END-OF-DISAB-DT  PIC XX.                      00001230
00124          16  CL-LAST-PMT-DT          PIC XX.                      00001240
00125          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.        00001250
00126          16  CL-PAID-THRU-DT         PIC XX.                      00001260
00127          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.        00001270
00128          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.        00001280
00129          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.          00001290
00130          16  CL-PMT-CALC-METHOD      PIC X.                       00001300
00131              88  CL-360-DAY-YR          VALUE '1'.                00001310
00132              88  CL-365-DAY-YR          VALUE '2'.                00001320
00133              88  CL-FULL-MONTHS         VALUE '3'.                00001330
00134          16  CL-CAUSE-CD             PIC X(6).                    00001340
00135          16  CL-DIAGNOSIS-DESCRIP    PIC X(26).                   00001350
00136          16  CL-LAST-REOPEN-DT       PIC XX.                      00001360
00137          16  CL-LAST-CLOSE-DT        PIC XX.                      00001370
00138          16  CL-LAST-CLOSE-REASON    PIC X.                       00001380
00139              88  FINAL-PAID             VALUE '1'.                00001390
00140              88  CLAIM-DENIED           VALUE '2'.                00001400
00141              88  AUTO-CLOSE             VALUE '3'.                00001410
00142              88  MANUAL-CLOSE           VALUE '4'.                00001420
00143          16  FILLER                  PIC X(10).                   00001430
00144                                                                   00001440
00145      12  CL-CERTIFICATE-DATA.                                     00001450
00146          16  CL-CERT-ORIGIN          PIC X.                       00001460
00147              88  CERT-WAS-ONLINE        VALUE '1'.                00001470
00148              88  CERT-WAS-CREATED       VALUE '2'.                00001480
00149              88  COVERAGE-WAS-ADDED     VALUE '3'.                00001490
00150          16  CL-CERT-KEY-DATA.                                    00001500
00151              20  CL-CERT-CARRIER     PIC X.                       00001510
00152              20  CL-CERT-GROUPING    PIC X(6).                    00001520
00153              20  CL-CERT-STATE       PIC XX.                      00001530
00154              20  CL-CERT-ACCOUNT.                                 00001540
00155                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).             00001550
00156                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).             00001560
00157              20  CL-CERT-EFF-DT      PIC XX.                      00001570
00158                                                                   00001580
00159      12  CL-STATUS-CONTROLS.                                      00001590
00160          16  CL-PRIORITY-CD          PIC X.                       00001600
00161              88  HIGHEST-PRIORITY       VALUE '9'.                00001610
00162          16  CL-SUPV-ATTN-CD         PIC X.                       00001620
00163              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.            00001630
00164              88  SUPV-IS-REQUIRED       VALUE 'Y'.                00001640
00165          16  CL-PURGED-DT            PIC XX.                      00001650
00166          16  CL-RESTORED-DT          PIC XX.                      00001660
00167          16  CL-NEXT-AUTO-PAY-DT     PIC XX.                      00001670
00168          16  CL-NEXT-RESEND-DT       PIC XX.                      00001680
00169          16  CL-NEXT-FOLLOWUP-DT     PIC XX.                      00001690
00170          16  FILLER                  PIC XX.                      00001700
00171          16  CL-LAST-MAINT-DT        PIC XX.                      00001710
00172          16  CL-LAST-MAINT-USER      PIC X(4).                    00001720
00173          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.        00001730
00174          16  CL-LAST-MAINT-TYPE      PIC X.                       00001740
00175              88  CLAIM-SET-UP           VALUE ' '.                00001750
00176              88  PAYMENT-MADE           VALUE '1'.                00001760
00177              88  LETTER-SENT            VALUE '2'.                00001770
00178              88  MASTER-WAS-ALTERED     VALUE '3'.                00001780
00179              88  MASTER-WAS-RESTORED    VALUE '4'.                00001790
00180              88  INCURRED-DATE-CHANGED  VALUE '5'.                00001800
00181              88  FILE-CONVERTED         VALUE '6'.                00001810
00182          16  CL-RELATED-CLAIM-NO     PIC X(7).                    00001820
00183          16  CL-HISTORY-ARCHIVE-DT   PIC XX.                      00001830
00184          16  FILLER                  PIC X(10).                   00001840
00185                                                                   00001850
00186      12  CL-TRAILER-CONTROLS.                                     00001860
00187          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.          00001870
00188              88  CL-1ST-TRL-AVAIL       VALUE +4095.              00001880
00189              88  CL-LAST-TRL-AVAIL      VALUE +1.                 00001890
00190              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.                 00001900
00191          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.          00001910
00192          16  FILLER                  PIC XX.                      00001920
00193          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.          00001930
00194          16  CL-ADDRESS-TRAILER-SEQ.                              00001940
00195              20  CL-INSURED-ADDR-SEQ PIC S9(4)     COMP.          00001950
00196              20  CL-ACCOUNT-ADDR-SEQ PIC S9(4)     COMP.          00001960
00197                  88  ACCOUNT-IS-ONLINE  VALUE ZERO.               00001970
00198              20  CL-BENIF-ADDR-SEQ   PIC S9(4)     COMP.          00001980
00199              20  CL-EMPLOYER-ADDR-SEQ PIC S9(4)    COMP.          00001990
00200              20  CL-DOCTOR-ADDR-SEQ  PIC S9(4)     COMP.          00002000
00201              20  CL-OTHER-1-ADDR-SEQ PIC S9(4)     COMP.          00002010
00202              20  CL-OTHER-2-ADDR-SEQ PIC S9(4)     COMP.          00002020
00203                                                                   00002030
00204      12  CL-FILE-LOCATION            PIC X(4).                    00002040
00205                                                                   00002050
00206      12  CL-PROCESS-ERRORS.                                       00002060
00207          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.          00002070
00208              88  NO-FATAL-ERRORS        VALUE ZERO.               00002080
00209          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.          00002090
00210              88  NO-FORCABLE-ERRORS     VALUE ZERO.               00002100
00211                                                                   00002110
00212      12  CL-BENEFICIARY              PIC X(6).                    00002120
00213                                                                   00002130
00214      12  FILLER                      PIC X(2).                    00002140
00215                                                                   00002150
00216 ******************************************************************00002160
00217      EJECT                                                        00002170
00218  FD  CLAIM-MASTER-OUT                                             00002180
00219      LABEL RECORDS ARE STANDARD                                   00002190
00220      RECORD CONTAINS 270 CHARACTERS                               00002200
00221      BLOCK CONTAINS 0 RECORDS                                     00002210
00222      DATA RECORD IS CLAIM-MASTER-RCD.                             00002220
00223                                                                   00002230
00224                              COPY ELCMSTR                         00002240
00225      REPLACING CLAIM-MASTER BY CLAIM-MASTER-RCD.                  00002250
00226                                                                   00002260
00227      EJECT                                                        00002270
00228  FD  TRAIL-MASTER-IN                                              00002280
00229      LABEL RECORDS ARE STANDARD                                   00002290
00230      RECORD CONTAINS 200 CHARACTERS                               00002300
00231      BLOCK CONTAINS 0 RECORDS                                     00002310
00232      DATA RECORD IS ACTIVITY-TRAILERS.                            00002320
00233                                                                   00002330
00234  01  ACTIVITY-TRAILERS.                                           00002340
00235      12  AT-RECORD-ID                PIC XX.                      00002350
00236          88  VALID-AT-ID                VALUE 'AT'.               00002360
00237                                                                   00002370
00238      12  AT-CONTROL-PRIMARY.                                      00002380
00239          16  AT-COMPANY-CD           PIC X.                       00002390
00240          16  AT-CARRIER              PIC X.                       00002400
00241          16  AT-CLAIM-NO             PIC X(7).                    00002410
00242          16  AT-CERT-NO.                                          00002420
00243              20  AT-CERT-PRIME       PIC X(10).                   00002430
00244              20  AT-CERT-SFX         PIC X.                       00002440
00245          16  AT-SEQUENCE-NO          PIC S9(4)     COMP.          00002450
00246                                                                   00002460
00247      12  AT-TRAILER-TYPE             PIC X.                       00002470
00248          88  RESERVE-EXPENSE-TR         VALUE '1'.                00002480
00249          88  PAYMENT-TR                 VALUE '2'.                00002490
00250          88  AUTO-PAY-TR                VALUE '3'.                00002500
00251          88  CORRESPONDENCE-TR          VALUE '4'.                00002510
00252          88  ADDRESS-TR                 VALUE '5'.                00002520
00253          88  GENERAL-INFO-TR            VALUE '6'.                00002530
00254          88  AUTO-PROMPT-TR             VALUE '7'.                00002540
00255          88  DENIAL-TR                  VALUE '8'.                00002550
00256          88  INCURRED-CHG-TR            VALUE '9'.                00002560
00257          88  FORM-CONTROL-TR            VALUE 'A'.                00002570
00258                                                                   00002580
00259      12  AT-RECORDED-DT              PIC XX.                      00002590
00260      12  AT-RECORDED-BY              PIC X(4).                    00002600
00261      12  AT-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.        00002610
00262                                                                   00002620
00263      12  AT-TRAILER-BODY             PIC X(165).                  00002630
00264                                                                   00002640
00265      12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.       00002650
00266          16  AT-RESERVE-CONTROLS.                                 00002660
00267              20  AT-MANUAL-SW        PIC X.                       00002670
00268                  88  AT-MANUAL-RESERVES-USED VALUE '1'.           00002680
00269              20  AT-FUTURE-SW        PIC X.                       00002690
00270                  88  AT-FUTURE-RESERVES-USED VALUE '1'.           00002700
00271              20  AT-PTC-SW           PIC X.                       00002710
00272                  88  AT-PAY-TO-CURRENT-USED  VALUE '1'.           00002720
00273              20  AT-IBNR-SW          PIC X.                       00002730
00274                  88  AT-IBNR-RESERVES-USED   VALUE '1'.           00002740
00275              20  AT-PTC-LF-SW        PIC X.                       00002750
00276                  88  AT-LF-PTC-USED          VALUE '1'.           00002760
00277              20  AT-CDT-ACCESS-METHOD PIC X.                      00002770
00278                  88  AT-CDT-ROUND-NEAR       VALUE '1'.           00002780
00279                  88  AT-CDT-ROUND-HIGH       VALUE '2'.           00002790
00280                  88  AT-CDT-INTERPOLATED     VALUE '3'.           00002800
00281              20  AT-PERCENT-OF-CDT   PIC S9(3)V99    COMP-3.      00002810
00282          16  AT-LAST-COMPUTED-DT     PIC XX.                      00002820
00283          16  AT-FUTURE-RESERVE       PIC S9(5)V99    COMP-3.      00002830
00284          16  AT-PAY-CURRENT-RESERVE  PIC S9(5)V99    COMP-3.      00002840
00285          16  AT-IBNR-RESERVE         PIC S9(5)V99    COMP-3.      00002850
00286          16  AT-INITIAL-MANUAL-RESERVE PIC S9(5)V99  COMP-3.      00002860
00287          16  AT-CURRENT-MANUAL-RESERVE PIC S9(5)V99  COMP-3.      00002870
00288          16  AT-ITD-ADDITIONAL-RESERVE PIC S9(5)V99  COMP-3.      00002880
00289          16  AT-EXPENSE-CONTROLS.                                 00002890
00290              20  AT-EXPENSE-METHOD   PIC X.                       00002900
00291                  88  NO-EXPENSE-CALCULATED   VALUE '1'.           00002910
00292                  88  FLAT-DOLLAR-PER-PMT     VALUE '2'.           00002920
00293                  88  PERCENT-OF-PMT          VALUE '3'.           00002930
00294                  88  DOLLAR-PER-OPEN-MONTH   VALUE '4'.           00002940
00295              20  AT-EXPENSE-PERCENT  PIC S9(3)V99    COMP-3.      00002950
00296              20  AT-EXPENSE-DOLLAR   PIC S9(3)V99    COMP-3.      00002960
00297          16  AT-ITD-PAID-EXPENSES    PIC S9(5)V99    COMP-3.      00002970
00298          16  AT-ITD-CHARGEABLE-EXPENSE PIC S9(5)V99  COMP-3.      00002980
00299                                                                   00002990
00300          16  AT-ITD-LIFE-REFUNDS     PIC S9(5)V99    COMP-3.      00003000
00301          16  AT-ITD-AH-REFUNDS       PIC S9(5)V99    COMP-3.      00003010
00302                                                                   00003020
00303          16  FILLER                  PIC X(57).                   00003030
00304                                                                   00003040
00305          16  AT-RESERVES-LAST-UPDATED-BY PIC S9(4)   COMP.        00003050
00306                                                                   00003060
00307          16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.                00003070
00308              20  AT-OPEN-CLOSE-DATE  PIC XX.                      00003080
00309              20  AT-OPEN-CLOSE-TYPE  PIC X.                       00003090
00310 *                    C = CLOSED                                   00003100
00311 *                    O = OPEN                                     00003110
00312              20  AT-OPEN-CLOSE-REASON PIC X(5).                   00003120
00313 *                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE      00003130
00314                                                                   00003140
00315      12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.               00003150
00316          16  AT-PAYMENT-TYPE         PIC X.                       00003160
00317              88  PARTIAL-PAYMENT        VALUE '1'.                00003170
00318              88  FINAL-PAYMENT          VALUE '2'.                00003180
00319              88  LUMP-SUM-PAYMENT       VALUE '3'.                00003190
00320              88  ADDITIONAL-PAYMENT     VALUE '4'.                00003200
00321              88  CHARGEABLE-EXPENSE     VALUE '5'.                00003210
00322              88  NON-CHARGEABLE-EXPENSE VALUE '6'.                00003220
00323                                                                   00003230
00324          16  AT-CLAIM-TYPE           PIC X.                       00003240
00325              88  PAID-FOR-AH            VALUE 'A'.                00003250
00326              88  PAID-FOR-LIFE          VALUE 'L'.                00003260
00327          16  AT-CLAIM-PREM-TYPE      PIC X.                       00003270
00328              88  AT-SINGLE-PREMIUM      VALUE '1'.                00003280
00329              88  AT-O-B-COVERAGE        VALUE '2'.                00003290
00330              88  AT-OPEN-END-COVERAGE   VALUE '3'.                00003300
00331          16  AT-AMOUNT-PAID          PIC S9(7)V99  COMP-3.        00003310
00332          16  AT-CHECK-NO             PIC X(7).                    00003320
00333          16  AT-PAID-FROM-DT         PIC XX.                      00003330
00334          16  AT-PAID-THRU-DT         PIC XX.                      00003340
00335          16  AT-DAYS-IN-PERIOD       PIC S9(4)     COMP.          00003350
00336          16  AT-PAYEE-TYPE-CD        PIC X.                       00003360
00337              88  INSURED-PAID           VALUE '1'.                00003370
00338              88  BENEFICIARY-PAID       VALUE '2'.                00003380
00339              88  ACCOUNT-PAID           VALUE '3'.                00003390
00340              88  OTHER-1-PAID           VALUE '4'.                00003400
00341              88  OTHER-2-PAID           VALUE '5'.                00003410
00342              88  DOCTOR-PAID            VALUE '6'.                00003420
00343          16  AT-PAYEES-NAME          PIC X(30).                   00003430
00344          16  AT-PAYMENT-ORIGIN       PIC X.                       00003440
00345              88  ONLINE-MANUAL-PMT      VALUE '1'.                00003450
00346              88  ONLINE-AUTO-PMT        VALUE '2'.                00003460
00347              88  OFFLINE-PMT            VALUE '3'.                00003470
00348          16  AT-CHECK-WRITTEN-DT     PIC XX.                      00003480
00349          16  AT-TO-BE-WRITTEN-DT     PIC XX.                      00003490
00350          16  AT-VOID-DATA.                                        00003500
00351              20  AT-VOID-DT          PIC XX.                      00003510
00352              20  AT-VOID-REASON      PIC X(30).                   00003520
00353          16  AT-ADDL-RESERVE         PIC S9(5)V99  COMP-3.        00003530
00354          16  AT-EXPENSE-PER-PMT      PIC S9(5)V99  COMP-3.        00003540
00355          16  AT-CREDIT-INTERFACE.                                 00003550
00356              20  AT-PMT-SELECT-DT    PIC XX.                      00003560
00357                  88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.       00003570
00358              20  AT-PMT-ACCEPT-DT    PIC XX.                      00003580
00359                  88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.       00003590
00360              20  AT-VOID-SELECT-DT   PIC XX.                      00003600
00361                  88  VOID-NOT-SELECTED     VALUE LOW-VALUE.       00003610
00362              20  AT-VOID-ACCEPT-DT   PIC XX.                      00003620
00363                  88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.       00003630
00364                                                                   00003640
00365          16  AT-CHECK-QUE-CONTROL    PIC S9(8)     COMP.          00003650
00366                  88  PAYMENT-NOT-QUEUED    VALUE ZERO.            00003660
00367                  88  CONVERSION-PAYMENT    VALUE +99999999.       00003670
00368          16  AT-CHECK-QUE-SEQUENCE   PIC S9(4)     COMP.          00003680
00369                                                                   00003690
00370          16  AT-FORCE-CONTROL        PIC X.                       00003700
00371              88  PAYMENT-WAS-FORCED        VALUE '1'.             00003710
00372          16  AT-PREV-LAST-PMT-DT     PIC XX.                      00003720
00373          16  AT-PREV-PAID-THRU-DT    PIC XX.                      00003730
00374          16  AT-PREV-LAST-PMT-AMT    PIC S9(7)V99  COMP-3.        00003740
00375          16  AT-ELIMINATION-DAYS     PIC S999      COMP-3.        00003750
00376          16  AT-DAILY-RATE           PIC S9(3)V99  COMP-3.        00003760
00377          16  AT-BENEFIT-TYPE         PIC X.                       00003770
00378                                                                   00003780
00379          16  AT-EXPENSE-TYPE         PIC X.                       00003790
00380          16  AT-PAYMENT-APPROVAL-SW  PIC X.                       00003800
00381                                                                   00003810
00382          16  FILLER                  PIC X(34).                   00003820
00383                                                                   00003830
00384          16  AT-PAYMENT-LAST-UPDATED-BY  PIC S9(4)   COMP.        00003840
00385                                                                   00003850
00386      12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.              00003860
00387          16  AT-SCHEDULE-START-DT    PIC XX.                      00003870
00388          16  AT-SCHEDULE-END-DT      PIC XX.                      00003880
00389          16  AT-TERMINATED-DT        PIC XX.                      00003890
00390          16  AT-LAST-PMT-TYPE        PIC X.                       00003900
00391              88  LAST-PMT-IS-FINAL      VALUE 'F'.                00003910
00392              88  LAST-PMT-IS-PARTIAL    VALUE 'P'.                00003920
00393          16  AT-FIRST-PMT-DATA.                                   00003930
00394              20  AT-FIRST-PMT-AMT    PIC S9(7)V99  COMP-3.        00003940
00395              20  AT-DAYS-IN-1ST-PMT  PIC S9(4)     COMP.          00003950
00396              20  AT-1ST-PAY-THRU-DT  PIC XX.                      00003960
00397          16  AT-REGULAR-PMT-DATA.                                 00003970
00398              20  AT-REGULAR-PMT-AMT  PIC S9(7)V99  COMP-3.        00003980
00399              20  AT-DAYS-IN-REG-PMT  PIC S9(4)     COMP.          00003990
00400              20  AT-INTERVAL-MONTHS  PIC S9(4)     COMP.          00004000
00401          16  AT-AUTO-PAYEE-CD        PIC X.                       00004010
00402              88  INSURED-PAID-AUTO      VALUE '1'.                00004020
00403              88  BENEFICIARY-PAID-AUTO  VALUE '2'.                00004030
00404              88  ACCOUNT-PAID-AUTO      VALUE '3'.                00004040
00405              88  OTHER-1-PAID-AUTO      VALUE '4'.                00004050
00406              88  OTHER-2-PAID-AUTO      VALUE '5'.                00004060
00407          16  FILLER                  PIC X(137).                  00004070
00408                                                                   00004080
00409          16  AT-AUTO-PAY-LAST-UPDATED-BY PIC S9(4)   COMP.        00004090
00410                                                                   00004100
00411      12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.        00004110
00412          16  AT-LETTER-SENT-DT       PIC XX.                      00004120
00413          16  AT-RECEIPT-FOLLOW-UP    PIC XX.                      00004130
00414          16  AT-AUTO-RE-SEND-DT      PIC XX.                      00004140
00415          16  AT-LETTER-ANSWERED-DT   PIC XX.                      00004150
00416          16  AT-LETTER-ARCHIVE-NO    PIC S9(8)     COMP.          00004160
00417          16  AT-LETTER-ORIGIN        PIC X.                       00004170
00418              88  ONLINE-CREATION        VALUE '1'.                00004180
00419              88  OFFLINE-CREATION       VALUE '2'.                00004190
00420          16  AT-STD-LETTER-FORM      PIC X(4).                    00004200
00421          16  AT-REASON-TEXT          PIC X(70).                   00004210
00422          16  AT-ADDRESS-REC-SEQ-NO   PIC S9(4)     COMP.          00004220
00423          16  AT-ADDRESEE-TYPE        PIC X.                       00004230
00424              88  INSURED-ADDRESEE       VALUE '1'.                00004240
00425              88  BENEFICIARY-ADDRESEE   VALUE '2'.                00004250
00426              88  ACCOUNT-ADDRESEE       VALUE '3'.                00004260
00427              88  PHYSICIAN-ADDRESEE     VALUE '4'.                00004270
00428              88  EMPLOYER-ADDRESEE      VALUE '5'.                00004280
00429              88  OTHER-ADDRESEE-1       VALUE '6'.                00004290
00430              88  OTHER-ADDRESEE-2       VALUE '7'.                00004300
00431          16  AT-ADDRESSEE-NAME       PIC X(30).                   00004310
00432          16  AT-INITIAL-PRINT-DATE   PIC XX.                      00004320
00433          16  AT-RESEND-PRINT-DATE    PIC XX.                      00004330
00434          16  AT-CORR-SOL-UNSOL       PIC X(01).                   00004340
00435          16  FILLER                  PIC X(38).                   00004350
00436          16  AT-CORR-LAST-UPDATED-BY PIC S9(4)   COMP.            00004360
00437                                                                   00004370
00438      12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.               00004380
00439          16  AT-ADDRESS-TYPE         PIC X.                       00004390
00440              88  INSURED-ADDRESS        VALUE '1'.                00004400
00441              88  BENEFICIARY-ADDRESS    VALUE '2'.                00004410
00442              88  ACCOUNT-ADDRESS        VALUE '3'.                00004420
00443              88  PHYSICIAN-ADDRESS      VALUE '4'.                00004430
00444              88  EMPLOYER-ADDRESS       VALUE '5'.                00004440
00445              88  OTHER-ADDRESS-1        VALUE '6'.                00004450
00446              88  OTHER-ADDRESS-2        VALUE '7'.                00004460
00447          16  AT-MAIL-TO-NAME         PIC X(30).                   00004470
00448          16  AT-ADDRESS-LINE-1       PIC X(30).                   00004480
00449          16  AT-ADDRESS-LINE-2       PIC X(30).                   00004490
00450          16  AT-CITY-STATE           PIC X(30).                   00004500
00451          16  AT-ZIP.                                              00004510
00452             18  AT-ZIP-CODE          PIC X(5).                    00004520
00453             18  AT-ZIP-PLUS4         PIC X(4).                    00004530
00454          16  AT-PHONE-NO             PIC 9(11)     COMP-3.        00004540
00455          16  FILLER                  PIC X(27).                   00004550
00456          16  AT-ADDRESS-LAST-UPDATED-BY PIC S9(4)  COMP.          00004560
00457                                                                   00004570
00458      12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.          00004580
00459          16  AT-INFO-LINE-1          PIC X(60).                   00004590
00460          16  AT-INFO-LINE-2          PIC X(60).                   00004600
00461          16  FILLER                  PIC X(43).                   00004610
00462          16  AT-GEN-INFO-LAST-UPDATED-BY PIC S9(4)  COMP.         00004620
00463                                                                   00004630
00464      12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.           00004640
00465          16  AT-PROMPT-LINE-1        PIC X(60).                   00004650
00466          16  AT-PROMPT-LINE-2        PIC X(60).                   00004660
00467          16  AT-PROMPT-START-DT      PIC XX.                      00004670
00468          16  AT-PROMPT-END-DT        PIC XX.                      00004680
00469          16  FILLER                  PIC X(39).                   00004690
00470          16  AT-PROMPT-LAST-UPDATED-BY PIC S9(4)  COMP.           00004700
00471                                                                   00004710
00472      12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.           00004720
00473          16  AT-DENIAL-INFO-1        PIC X(60).                   00004730
00474          16  AT-DENIAL-INFO-2        PIC X(60).                   00004740
00475          16  AT-DENIAL-DT            PIC XX.                      00004750
00476          16  AT-RETRACTION-DT        PIC XX.                      00004760
00477          16  AT-DENIAL-REASON-CODE   PIC X(4).                    00004770
00478          16  FILLER                  PIC X(35).                   00004780
00479          16  AT-DENIAL-LAST-UPDATED-BY PIC S9(4)  COMP.           00004790
00480                                                                   00004800
00481      12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.          00004810
00482          16  AT-OLD-INCURRED-DT      PIC XX.                      00004820
00483          16  AT-OLD-REPORTED-DT      PIC XX.                      00004830
00484          16  AT-OLD-ESTABLISHED-DT   PIC XX.                      00004840
00485          16  AT-OLD-TOTAL-PAID       PIC S9(7)V99     COMP-3.     00004850
00486          16  AT-OLD-DAYS-PAID        PIC S9(4)        COMP.       00004860
00487          16  AT-OLD-NO-OF-PMTS       PIC S9(3)        COMP-3.     00004870
00488          16  AT-OLD-PAID-THRU-DT     PIC XX.                      00004880
00489          16  AT-LAST-PMT-MADE-DT     PIC XX.                      00004890
00490          16  AT-OLD-DIAG-DESCRIP     PIC X(26).                   00004900
00491          16  AT-OLD-DIAG-CODE        PIC X(6).                    00004910
00492          16  AT-TRAILER-CNT-AT-CHG   PIC S9(4)        COMP.       00004920
00493          16  AT-OLD-ITD-PAID-EXPENSE PIC S9(5)V99     COMP-3.     00004930
00494          16  AT-OLD-CHARGABLE-EXPENSE PIC S9(5)V99    COMP-3.     00004940
00495          16  AT-OLD-INIT-MAN-RESV    PIC S9(7)V99     COMP-3.     00004950
00496          16  AT-OLD-CURRENT-MAN-RESV PIC S9(7)V99     COMP-3.     00004960
00497          16  AT-OLD-ADDL-MAN-RESV    PIC S9(7)V99     COMP-3.     00004970
00498          16  FILLER                  PIC X(87).                   00004980
00499          16  AT-INCURRED-LAST-UPDATED-BY PIC S9(4)  COMP.         00004990
00500                                                                   00005000
00501      12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.          00005010
00502          16  AT-FORM-SEND-ON-DT      PIC XX.                      00005020
00503          16  AT-FORM-FOLLOW-UP-DT    PIC XX.                      00005030
00504          16  AT-FORM-RE-SEND-DT      PIC XX.                      00005040
00505          16  AT-FORM-ANSWERED-DT     PIC XX.                      00005050
00506          16  AT-FORM-PRINTED-DT      PIC XX.                      00005060
00507          16  AT-FORM-REPRINT-DT      PIC XX.                      00005070
00508          16  AT-FORM-TYPE            PIC X.                       00005080
00509              88  INITIAL-FORM           VALUE '1'.                00005090
00510              88  PROGRESS-FORM          VALUE '2'.                00005100
00511          16  AT-INSTRUCT-LN-1        PIC X(28).                   00005110
00512          16  AT-INSTRUCT-LN-2        PIC X(28).                   00005120
00513          16  AT-INSTRUCT-LN-3        PIC X(28).                   00005130
00514          16  AT-FORM-ADDR-SEQ-NO     PIC S9(4)      COMP.         00005140
00515          16  AT-FORM-ADDRESS         PIC X.                       00005150
00516              88  FORM-TO-INSURED        VALUE '1'.                00005160
00517              88  FORM-TO-ACCOUNT        VALUE '3'.                00005170
00518              88  FORM-TO-OTHER-1        VALUE '6'.                00005180
00519              88  FORM-TO-OTHER-2        VALUE '7'.                00005190
00520          16  AT-RELATED-1.                                        00005200
00521              20 AT-REL-CARR-1        PIC X.                       00005210
00522              20 AT-REL-CLAIM-1       PIC X(7).                    00005220
00523              20 AT-REL-CERT-1        PIC X(11).                   00005230
00524          16  AT-RELATED-2.                                        00005240
00525              20 AT-REL-CARR-2        PIC X.                       00005250
00526              20 AT-REL-CLAIM-2       PIC X(7).                    00005260
00527              20 AT-REL-CERT-2        PIC X(11).                   00005270
00528                                                                   00005280
00529          16  FILLER                  PIC X(25).                   00005290
00530          16  AT-FORM-LAST-UPDATED-BY PIC S9(4)  COMP.             00005300
00531                                                                   00005310
00532 ******************************************************************00005320
00533      EJECT                                                        00005330
00534  FD  TRAIL-MASTER-OUT                                             00005340
00535      LABEL RECORDS ARE STANDARD                                   00005350
00536      RECORD CONTAINS 200 CHARACTERS                               00005360
00537      BLOCK CONTAINS 0 RECORDS                                     00005370
00538      DATA RECORD IS ACTIVITY-TRAILERS-RCD.                        00005380
00539                                                                   00005390
00540                              COPY ELCTRLR                         00005400
00541      REPLACING ACTIVITY-TRAILERS BY ACTIVITY-TRAILERS-RCD.        00005410
00542      EJECT                                                        00005420
00543  WORKING-STORAGE SECTION.                                         00005430
00544  77  FILLER  PIC X(32)  VALUE '********************************'. 00005440
00545  77  FILLER  PIC X(32)  VALUE '*   ELCLMCNV WORKING-STORAGE    '. 00005450
00546  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.004 ************'. 00005451
00547                                                                   00005470
00548  77  CLAIM-IN-CNT            PIC S9(7)   COMP-3  VALUE +0.        00005480
00549  77  CLAIM-OUT-CNT           PIC S9(7)   COMP-3  VALUE +0.        00005490
00550  77  TRLR-IN-CNT             PIC S9(7)   COMP-3  VALUE +0.        00005500
00551  77  TRLR-OUT-CNT            PIC S9(7)   COMP-3  VALUE +0.        00005510
00552  77  DIAGNOSIS-TRLRS-CNT     PIC S9(7)   COMP-3  VALUE +0.        00005520
00553  77  SORT-RELEASE-CNT        PIC S9(7)   COMP-3  VALUE +0.        00005530
00554  77  SORT-RETURN-CNT         PIC S9(7)   COMP-3  VALUE +0.        00005540
00555  77  HIGH-TABLE-CNT          PIC S9(7)   COMP-3  VALUE +0.        00005550
00556  77  SEQUENCE-NINETY         PIC S9(4)   COMP    VALUE +90.       00005560
00557  77  CURRENT-CLAIM-CNT       PIC S9(4)   COMP    VALUE +0.        00005570
00558  77  ONE                     PIC S9(4)   COMP    VALUE +1.        00005580
00559                                                                   00005590
00560  01  WS.                                                          00005600
00561      12  WS-RETURN-CODE        PIC S9(4)   COMP   VALUE +0.       00005610
00562      12  WS-ABEND-MESSAGE      PIC X(80)          VALUE SPACES.   00005620
00563      12  WS-ABEND-FILE-STATUS  PIC XX             VALUE ZEROS.    00005630
00564      12  WS-ZERO               PIC S9      COMP-3 VALUE +0.       00005640
00565      12  WS-OLD-DIAGNOSIS.                                        00005650
00566          16  WS-OLD-DIAG-DESCRIP PIC X(26).                       00005660
00567          16  FILLER              PIC X(34)  VALUE SPACES.         00005670
00568                                                                   00005680
00569      12  WS-ABEND-CODE         PIC 9(4).                          00005690
00570      12  ABEND-CODE  REDEFINES  WS-ABEND-CODE.                    00005700
00571          16  ABEND-CODE-1      PIC XX.                            00005710
00572          16  ABEND-CODE-2      PIC XX.                            00005720
00573      12  WS-DIAGNOSIS.                                            00005730
00574          16  WS-DIAGNOSIS-DESCRIP PIC X(26).                      00005740
00575          16  FILLER               PIC X(34).                      00005750
00576      12  WS-PRIME-CERT.                                           00005760
00577          16  PRIME-CERT-PRIME     PIC X(10).                      00005770
00578          16  PRIME-CERT-SUFX      PIC X(01).                      00005780
00579      12  WS-TRAILER-PRIMARY.                                      00005790
00580          16  WS-COMPANY-CD        PIC X(1).                       00005800
00581          16  WS-CARRIER           PIC X(1).                       00005810
00582          16  WS-CLAIM-NO          PIC X(7).                       00005820
00583          16  WS-CERT-NO.                                          00005830
00584              20  WS-CERT-PRIME    PIC X(10).                      00005840
00585              20  WS-CERT-SUB      PIC X(1).                       00005850
00586                                                                   00005860
00587  01  CLAIM-MASTERS-TABLE.                                         00005870
00588      12  TBL-CLAIM-CNT         PIC S9(4)   COMP   VALUE +0.       00005880
00589      12  TBL-SEQ-CNT           PIC S9(4)   COMP   VALUE +0.       00005890
00590      12  TBL-COMPANY-CD        PIC X              VALUE SPACES.   00005900
00591      12  TBL-CARRIER           PIC X              VALUE SPACES.   00005910
00592      12  TBL-CLAIM-NO          PIC X(7)           VALUE SPACES.   00005920
00593      12  TBL-CLAIM-RCDS        OCCURS  25  TIMES.                 00005930
00594          16  FILLER            PIC X(11).                         00005940
00595          16  TBL-CERT-NO       PIC X(11).                         00005950
00596          16  FILLER            PIC X(248).                        00005960
00597      12  TBL-MAX-CLAIM-CNT     PIC S9(4)   COMP   VALUE +25.      00005970
00598      12  TBL-SAVE-CLAIM-RCD.                                      00005980
00599          16  FILLER                  PIC X(2).                    00005990
00600          16  TBL-SAVE-COMPANY-CD     PIC X(1).                    00006000
00601          16  TBL-SAVE-CARRIER        PIC X(1).                    00006010
00602          16  TBL-SAVE-CLAIM-NO       PIC X(7).                    00006020
00603          16  TBL-SAVE-CERT-NO.                                    00006030
00604              20  TBL-SAVE-CERT-PRIME PIC X(10).                   00006040
00605              20  TBL-SAVE-CERT-SUFX  PIC X(1).                    00006050
00606          16  FILLER                  PIC X(248).                  00006060
00607                                                                   00006070
00608  01  WS-ADDRESS-SEQU-CNTS.                                        00006080
00609      12  WS-INIT-ADDRESS-CNT.                                     00006090
00610          16  FILLER            PIC S9(1)          VALUE ZERO.     00006100
00611          16  FILLER            PIC S9(1)          VALUE ZERO.     00006110
00612          16  FILLER            PIC S9(1)          VALUE ZERO.     00006120
00613          16  FILLER            PIC S9(1)          VALUE ZERO.     00006130
00614          16  FILLER            PIC S9(1)          VALUE ZERO.     00006140
00615          16  FILLER            PIC S9(1)          VALUE ZERO.     00006150
00616          16  FILLER            PIC S9(1)          VALUE ZERO.     00006160
00617      12  WS-INIT-ADDRESS-SEQU.                                    00006170
00618          16  FILLER            PIC S9(4)  COMP    VALUE +0.       00006180
00619          16  FILLER            PIC S9(4)  COMP    VALUE +10.      00006190
00620          16  FILLER            PIC S9(4)  COMP    VALUE +20.      00006200
00621          16  FILLER            PIC S9(4)  COMP    VALUE +30.      00006210
00622          16  FILLER            PIC S9(4)  COMP    VALUE +40.      00006220
00623          16  FILLER            PIC S9(4)  COMP    VALUE +50.      00006230
00624          16  FILLER            PIC S9(4)  COMP    VALUE +60.      00006240
00625      12  WS-BUILD-ADDRESS-SEQU.                                   00006250
00626          16  INSURED-SEQU      PIC S9(4)  COMP    VALUE +0.       00006260
00627          16  BENEFIC-SEQU      PIC S9(4)  COMP    VALUE +10.      00006270
00628          16  ACCOUNT-SEQU      PIC S9(4)  COMP    VALUE +20.      00006280
00629          16  DOCTOR-SEQU       PIC S9(4)  COMP    VALUE +30.      00006290
00630          16  EMPLOYER-SEQU     PIC S9(4)  COMP    VALUE +40.      00006300
00631          16  OTHER-1-SEQU      PIC S9(4)  COMP    VALUE +50.      00006310
00632          16  OTHER-2-SEQU      PIC S9(4)  COMP    VALUE +60.      00006320
00633                                                                   00006330
00634      EJECT                                                        00006340
00635  PROCEDURE DIVISION.                                              00006350
00636                                                                   00006360
00637 ******************************************************************00006370
00638 ***           O P E N   F I L E S   R O U T I N E              ***00006380
00639 ******************************************************************00006390
00640                                                                   00006400
00641  0080-OPEN-FILES.                                                 00006410
00642                                                                   00006420
00643      OPEN INPUT  CLAIM-MASTER-IN                                  00006430
00644                  TRAIL-MASTER-IN                                  00006440
00645           OUTPUT CLAIM-MASTER-OUT                                 00006450
00646                  TRAIL-MASTER-OUT.                                00006460
00647                                                                   00006470
00648  0100-SORT-CONTROL-STMTS.                                         00006480
00649                                                                   00006490
00650      SORT SORT-WORK ON ASCENDING SW-CONTROL-PRIMARY               00006500
00651          INPUT PROCEDURE                                          00006510
00652              0150-SORT-INPUT-PROCEDURE THRU 1000-EXIT             00006520
00653          OUTPUT PROCEDURE                                         00006530
00654              3000-SORT-OUTPUT-PROCEDURE THRU 3000-EXIT.           00006540
00655                                                                   00006550
00656      GO TO 9910-EOJ-1.                                            00006560
00657                                                                   00006570
00658      EJECT                                                        00006580
00659  0150-SORT-INPUT-PROCEDURE   SECTION.                             00006590
00660                                                                   00006600
00661      PERFORM 2600-READ-CLAIM-MASTER THRU 2600-EXIT.               00006610
00662      MOVE CL-COMPANY-CD        OF CLAIM-MASTER                    00006620
00663                                TO TBL-COMPANY-CD.                 00006630
00664      MOVE CL-CARRIER           OF CLAIM-MASTER                    00006640
00665                                TO TBL-CARRIER.                    00006650
00666      MOVE CL-CLAIM-NO          OF CLAIM-MASTER                    00006660
00667                                TO TBL-CLAIM-NO.                   00006670
00668      PERFORM 2700-READ-CLAIM-TRAILER THRU 2700-EXIT.              00006680
00669      GO TO 0210-CHECK-EOF-CLAIMS.                                 00006690
00670                                                                   00006700
00671 *****************************************                         00006710
00672 ***      M A I N L I N E   L O O P    ***                         00006720
00673 *****************************************                         00006730
00674                                                                   00006740
00675  0200-INPUT-PROCEDURE-LOOP.                                       00006750
00676                                                                   00006760
00677      PERFORM 2600-READ-CLAIM-MASTER THRU 2600-EXIT.               00006770
00678                                                                   00006780
00679  0210-CHECK-EOF-CLAIMS.                                           00006790
00680                                                                   00006800
00681      IF CL-CONTROL-PRIMARY  OF  CLAIM-MASTER  EQUAL  HIGH-VALUES  00006810
00682          MOVE TBL-CERT-NO (ONE)  TO WS-PRIME-CERT                 00006820
00683              PERFORM 2400-WRITE-CLAIM-MASTERS THRU 2400-EXIT      00006830
00684                  VARYING TBL-SEQ-CNT FROM +1 BY +1 UNTIL          00006840
00685                      TBL-SEQ-CNT IS GREATER THAN TBL-CLAIM-CNT    00006850
00686                          GO TO 1000-EXIT.                         00006860
00687                                                                   00006870
00688      PERFORM 2000-BUILD-CLAIM-MASTER   THRU 2000-EXIT.            00006880
00689      PERFORM 2100-BUILD-DIAGNOSIS-TRLR THRU 2100-EXIT.            00006890
00690                                                                   00006900
00691      MOVE WS-INIT-ADDRESS-SEQU   TO WS-BUILD-ADDRESS-SEQU.        00006910
00692                                                                   00006920
00693      PERFORM 2200-PROCESS-TRAILERS     THRU 2200-EXIT             00006930
00694          UNTIL WS-TRAILER-PRIMARY GREATER THAN CL-CONTROL-PRIMARY 00006940
00695              OF CLAIM-MASTER-RCD.                                 00006950
00696                                                                   00006960
00697      PERFORM 2300-TABLE-CLAIM-MASTERS  THRU 2300-EXIT.            00006970
00698                                                                   00006980
00699      GO TO 0200-INPUT-PROCEDURE-LOOP.                             00006990
00700                                                                   00007000
00701  1000-EXIT.                                                       00007010
00702      EXIT.                                                        00007020
00703                                                                   00007030
00704      EJECT                                                        00007040
00705  2000-BUILD-CLAIM-MASTER.                                         00007050
00706                                                                   00007060
00707      MOVE SPACES                 TO CLAIM-MASTER-RCD.             00007070
00708      MOVE 'CL' TO CL-RECORD-ID   OF CLAIM-MASTER-RCD.             00007080
00709                                                                   00007090
00710      MOVE CL-COMPANY-CD          OF CLAIM-MASTER  TO              00007100
00711           CL-COMPANY-CD          OF CLAIM-MASTER-RCD              00007110
00712           CL-COMPANY-CD-A1       OF CLAIM-MASTER-RCD              00007120
00713           CL-COMPANY-CD-A2       OF CLAIM-MASTER-RCD              00007130
00714           CL-COMPANY-CD-A4       OF CLAIM-MASTER-RCD.             00007140
00715                                                                   00007150
00716      MOVE CL-CARRIER             OF CLAIM-MASTER  TO              00007160
00717           CL-CARRIER             OF CLAIM-MASTER-RCD.             00007170
00718                                                                   00007180
00719      MOVE CL-CLAIM-NO            OF CLAIM-MASTER  TO              00007190
00720           CL-CLAIM-NO            OF CLAIM-MASTER-RCD.             00007200
00721                                                                   00007210
00722      MOVE CL-CERT-NO             OF CLAIM-MASTER  TO              00007220
00723           CL-CERT-NO             OF CLAIM-MASTER-RCD.             00007230
00724                                                                   00007240
00725      MOVE CL-INSURED-LAST-NAME   OF CLAIM-MASTER  TO              00007250
00726           CL-INSURED-LAST-NAME   OF CLAIM-MASTER-RCD.             00007260
00727                                                                   00007270
00728      MOVE CL-SOC-SEC-NO          OF CLAIM-MASTER  TO              00007280
00729           CL-SOC-SEC-NO          OF CLAIM-MASTER-RCD.             00007290
00730                                                                   00007300
00731      MOVE CL-CERT-NO-A4          OF CLAIM-MASTER  TO              00007310
00732           CL-CERT-NO-A4          OF CLAIM-MASTER-RCD.             00007320
00733                                                                   00007330
00734      MOVE CL-INSURED-1ST-NAME    OF CLAIM-MASTER  TO              00007340
00735           CL-INSURED-1ST-NAME    OF CLAIM-MASTER-RCD.             00007350
00736                                                                   00007360
00737      MOVE CL-INSURED-MID-INIT    OF CLAIM-MASTER  TO              00007370
00738           CL-INSURED-MID-INIT    OF CLAIM-MASTER-RCD.             00007380
00739                                                                   00007390
00740      MOVE CL-INSURED-BIRTH-DT    OF CLAIM-MASTER  TO              00007400
00741           CL-INSURED-BIRTH-DT    OF CLAIM-MASTER-RCD.             00007410
00742                                                                   00007420
00743      MOVE CL-INSURED-SEX-CD      OF CLAIM-MASTER  TO              00007430
00744           CL-INSURED-SEX-CD      OF CLAIM-MASTER-RCD.             00007440
00745                                                                   00007450
00746      MOVE CL-INSURED-OCC-CD      OF CLAIM-MASTER  TO              00007460
00747           CL-INSURED-OCC-CD      OF CLAIM-MASTER-RCD.             00007470
00748                                                                   00007480
00749      MOVE CL-PROCESSOR-ID        OF CLAIM-MASTER  TO              00007490
00750           CL-PROCESSOR-ID        OF CLAIM-MASTER-RCD.             00007500
00751                                                                   00007510
00752      MOVE CL-CLAIM-STATUS        OF CLAIM-MASTER  TO              00007520
00753           CL-CLAIM-STATUS        OF CLAIM-MASTER-RCD.             00007530
00754                                                                   00007540
00755      MOVE CL-CLAIM-TYPE          OF CLAIM-MASTER  TO              00007550
00756           CL-CLAIM-TYPE          OF CLAIM-MASTER-RCD.             00007560
00757                                                                   00007570
00758      MOVE CL-CLAIM-PREM-TYPE     OF CLAIM-MASTER  TO              00007580
00759           CL-CLAIM-PREM-TYPE     OF CLAIM-MASTER-RCD.             00007590
00760                                                                   00007600
00761      MOVE CL-INCURRED-DT         OF CLAIM-MASTER  TO              00007610
00762           CL-INCURRED-DT         OF CLAIM-MASTER-RCD.             00007620
00763                                                                   00007630
00764      MOVE CL-REPORTED-DT         OF CLAIM-MASTER  TO              00007640
00765           CL-REPORTED-DT         OF CLAIM-MASTER-RCD.             00007650
00766                                                                   00007660
00767      MOVE CL-FILE-ESTABLISH-DT   OF CLAIM-MASTER  TO              00007670
00768           CL-FILE-ESTABLISH-DT   OF CLAIM-MASTER-RCD.             00007680
00769                                                                   00007690
00770      MOVE CL-EST-END-OF-DISAB-DT OF CLAIM-MASTER  TO              00007700
00771           CL-EST-END-OF-DISAB-DT OF CLAIM-MASTER-RCD.             00007710
00772                                                                   00007720
00773      MOVE CL-LAST-PMT-DT         OF CLAIM-MASTER  TO              00007730
00774           CL-LAST-PMT-DT         OF CLAIM-MASTER-RCD.             00007740
00775                                                                   00007750
00776      IF CL-LAST-PMT-AMT OF CLAIM-MASTER NOT NUMERIC               00007760
00777         MOVE +0 TO CL-LAST-PMT-AMT OF CLAIM-MASTER.               00007770
00778                                                                   00007780
00779      MOVE CL-LAST-PMT-AMT        OF CLAIM-MASTER  TO              00007790
00780           CL-LAST-PMT-AMT        OF CLAIM-MASTER-RCD.             00007800
00781                                                                   00007810
00782      MOVE CL-PAID-THRU-DT        OF CLAIM-MASTER  TO              00007820
00783           CL-PAID-THRU-DT        OF CLAIM-MASTER-RCD.             00007830
00784                                                                   00007840
00785      IF CL-TOTAL-PAID-AMT OF CLAIM-MASTER NOT NUMERIC             00007850
00786         MOVE +0 TO CL-TOTAL-PAID-AMT OF CLAIM-MASTER.             00007860
00787                                                                   00007870
00788      MOVE CL-TOTAL-PAID-AMT      OF CLAIM-MASTER  TO              00007880
00789           CL-TOTAL-PAID-AMT      OF CLAIM-MASTER-RCD.             00007890
00790                                                                   00007900
00791      IF CL-NO-OF-PMTS-MADE OF CLAIM-MASTER NOT NUMERIC            00007910
00792         MOVE +0 TO CL-NO-OF-PMTS-MADE OF CLAIM-MASTER.            00007920
00793                                                                   00007930
00794      MOVE CL-NO-OF-PMTS-MADE     OF CLAIM-MASTER  TO              00007940
00795           CL-NO-OF-PMTS-MADE     OF CLAIM-MASTER-RCD.             00007950
00796                                                                   00007960
00797      MOVE CL-NO-OF-DAYS-PAID     OF CLAIM-MASTER  TO              00007970
00798           CL-NO-OF-DAYS-PAID     OF CLAIM-MASTER-RCD.             00007980
00799                                                                   00007990
00800      MOVE CL-PMT-CALC-METHOD     OF CLAIM-MASTER  TO              00008000
00801           CL-PMT-CALC-METHOD     OF CLAIM-MASTER-RCD.             00008010
00802                                                                   00008020
00803      MOVE CL-CAUSE-CD            OF CLAIM-MASTER  TO              00008030
00804           CL-CAUSE-CD            OF CLAIM-MASTER-RCD.             00008040
00805                                                                   00008050
00806      MOVE CL-LAST-REOPEN-DT      OF CLAIM-MASTER  TO              00008060
00807           CL-LAST-REOPEN-DT      OF CLAIM-MASTER-RCD.             00008070
00808                                                                   00008080
00809      MOVE CL-LAST-CLOSE-DT       OF CLAIM-MASTER  TO              00008090
00810           CL-LAST-CLOSE-DT       OF CLAIM-MASTER-RCD.             00008100
00811                                                                   00008110
00812      MOVE CL-LAST-CLOSE-REASON   OF CLAIM-MASTER  TO              00008120
00813           CL-LAST-CLOSE-REASON   OF CLAIM-MASTER-RCD.             00008130
00814                                                                   00008140
00815 **************************************                            00008150
00816 * GROUP MOVE (NO DATA FIELD CHANGES) *                            00008160
00817 **************************************                            00008170
00818                                                                   00008180
00819      MOVE CL-CERTIFICATE-DATA    OF CLAIM-MASTER  TO              00008190
00820           CL-CERTIFICATE-DATA    OF CLAIM-MASTER-RCD.             00008200
00821                                                                   00008210
00822      MOVE CL-STATUS-CONTROLS     OF CLAIM-MASTER  TO              00008220
00823           CL-STATUS-CONTROLS     OF CLAIM-MASTER-RCD.             00008230
00824                                                                   00008240
00825      MOVE LOW-VALUES             TO CL-PURGED-DT                  00008250
00826                                  OF CLAIM-MASTER-RCD.             00008260
00827                                                                   00008270
00828 ****************************                                      00008280
00829 *    END OF GROUP MOVES    *                                      00008290
00830 ****************************                                      00008300
00831                                                                   00008310
00832      MOVE CL-TRAILER-SEQ-CNT     OF CLAIM-MASTER  TO              00008320
00833           CL-TRAILER-SEQ-CNT     OF CLAIM-MASTER-RCD.             00008330
00834                                                                   00008340
00835      MOVE CL-LAST-INC-DT-CHANGE  OF CLAIM-MASTER  TO              00008350
00836           CL-LAST-INC-DT-CHANGE  OF CLAIM-MASTER-RCD.             00008360
00837                                                                   00008370
00838      MOVE CL-AUTO-PAY-SEQ        OF CLAIM-MASTER  TO              00008380
00839           CL-AUTO-PAY-SEQ        OF CLAIM-MASTER-RCD.             00008390
00840                                                                   00008400
00841      MOVE WS-INIT-ADDRESS-CNT    TO CL-ADDRESS-TRAILER-CNT.       00008410
00842                                                                   00008420
00843      MOVE CL-FILE-LOCATION       OF CLAIM-MASTER  TO              00008430
00844           CL-FILE-LOCATION       OF CLAIM-MASTER-RCD.             00008440
00845                                                                   00008450
00846      MOVE CL-FATAL-ERROR-CNT     OF CLAIM-MASTER  TO              00008460
00847           CL-FATAL-ERROR-CNT     OF CLAIM-MASTER-RCD.             00008470
00848                                                                   00008480
00849      MOVE CL-FORCEABLE-ERROR-CNT OF CLAIM-MASTER  TO              00008490
00850           CL-FORCEABLE-ERROR-CNT OF CLAIM-MASTER-RCD.             00008500
00851                                                                   00008510
00852      MOVE CL-BENEFICIARY         OF CLAIM-MASTER  TO              00008520
00853           CL-BENEFICIARY         OF CLAIM-MASTER-RCD.             00008530
00854                                                                   00008540
00855      MOVE ZEROS                  TO CL-CLAIM-PAYMENT-STATUS OF    00008550
00856           CLAIM-MASTER-RCD.                                       00008560
00857                                                                   00008570
00858  2000-EXIT.                                                       00008580
00859      EXIT.                                                        00008590
00860                                                                   00008600
00861      EJECT                                                        00008610
00862  2100-BUILD-DIAGNOSIS-TRLR.                                       00008620
00863                                                                   00008630
00864      MOVE SPACES                 TO ACTIVITY-TRAILERS-RCD.        00008640
00865                                                                   00008650
00866      MOVE 'AT'                   TO AT-RECORD-ID                  00008660
00867                                  OF ACTIVITY-TRAILERS-RCD.        00008670
00868                                                                   00008680
00869      MOVE CL-CONTROL-PRIMARY     OF CLAIM-MASTER-RCD TO           00008690
00870           AT-CONTROL-PRIMARY     OF ACTIVITY-TRAILERS-RCD.        00008700
00871                                                                   00008710
00872      MOVE SEQUENCE-NINETY        TO AT-SEQUENCE-NO                00008720
00873                                  OF ACTIVITY-TRAILERS-RCD.        00008730
00874                                                                   00008740
00875      MOVE '6'                    TO AT-TRAILER-TYPE               00008750
00876                                  OF ACTIVITY-TRAILERS-RCD.        00008760
00877                                                                   00008770
00878      MOVE CL-LAST-MAINT-DT       OF CLAIM-MASTER-RCD TO           00008780
00879           AT-RECORDED-DT         OF ACTIVITY-TRAILERS-RCD.        00008790
00880                                                                   00008800
00881      MOVE CL-LAST-MAINT-DT       OF CLAIM-MASTER-RCD TO           00008810
00882           AT-GEN-INFO-LAST-MAINT-DT OF ACTIVITY-TRAILERS-RCD.     00008820
00883                                                                   00008830
00884      MOVE CL-LAST-MAINT-USER     OF CLAIM-MASTER-RCD TO           00008840
00885           AT-RECORDED-BY         OF ACTIVITY-TRAILERS-RCD.        00008850
00886                                                                   00008860
00887      MOVE CL-LAST-MAINT-USER     OF CLAIM-MASTER-RCD TO           00008870
00888           AT-GEN-INFO-LAST-UPDATED-BY OF ACTIVITY-TRAILERS-RCD.   00008880
00889                                                                   00008890
00890      IF CL-LAST-MAINT-HHMMSS OF CLAIM-MASTER-RCD NOT NUMERIC      00008900
00891         MOVE +0 TO CL-LAST-MAINT-HHMMSS OF CLAIM-MASTER-RCD.      00008910
00892                                                                   00008920
00893      MOVE CL-LAST-MAINT-HHMMSS   OF CLAIM-MASTER-RCD TO           00008930
00894           AT-LAST-MAINT-HHMMSS   OF ACTIVITY-TRAILERS-RCD.        00008940
00895                                                                   00008950
00896      MOVE CL-DIAGNOSIS-DESCRIP   OF CLAIM-MASTER     TO           00008960
00897           WS-DIAGNOSIS-DESCRIP.                                   00008970
00898                                                                   00008980
00899      MOVE WS-DIAGNOSIS           TO AT-INFO-LINE-1                00008990
00900                                  OF ACTIVITY-TRAILERS-RCD.        00009000
00901                                                                   00009010
00902      ADD +1                      TO DIAGNOSIS-TRLRS-CNT.          00009020
00903                                                                   00009030
00904      PERFORM 2900-RELEASE-SORT-RECORD THRU 2900-EXIT.             00009040
00905                                                                   00009050
00906  2100-EXIT.                                                       00009060
00907      EXIT.                                                        00009070
00908                                                                   00009080
00909      EJECT                                                        00009090
00910  2200-PROCESS-TRAILERS.                                           00009100
00911                                                                   00009110
00912      IF WS-TRAILER-PRIMARY LESS THAN                              00009120
00913          CL-CONTROL-PRIMARY OF CLAIM-MASTER-RCD                   00009130
00914          DISPLAY '**  UNMATCHED TRLR  **'                         00009140
00915          DISPLAY '**  TRAILER CONTROL  = ' WS-TRAILER-PRIMARY     00009150
00916          DISPLAY '**                  **'                         00009160
00917          GO TO 2200-READ-NEXT-TRAILER.                            00009170
00918                                                                   00009180
00919      MOVE SPACES TO ACTIVITY-TRAILERS-RCD.                        00009190
00920                                                                   00009200
00921      IF ADDRESS-TR  OF  ACTIVITY-TRAILERS                         00009210
00922          IF INSURED-ADDRESS       OF ACTIVITY-TRAILERS            00009220
00923              ADD +1               TO CL-INSURED-ADDR-CNT          00009230
00924                                      INSURED-SEQU                 00009240
00925              MOVE INSURED-SEQU    TO AT-SEQUENCE-NO               00009250
00926                                   OF ACTIVITY-TRAILERS            00009260
00927          ELSE                                                     00009270
00928          IF BENEFICIARY-ADDRESS   OF ACTIVITY-TRAILERS            00009280
00929              ADD +1               TO CL-BENIF-ADDR-CNT            00009290
00930                                      BENEFIC-SEQU                 00009300
00931              MOVE BENEFIC-SEQU    TO AT-SEQUENCE-NO               00009310
00932                                   OF ACTIVITY-TRAILERS            00009320
00933          ELSE                                                     00009330
00934          IF ACCOUNT-ADDRESS       OF ACTIVITY-TRAILERS            00009340
00935              ADD +1               TO CL-ACCOUNT-ADDR-CNT          00009350
00936                                      ACCOUNT-SEQU                 00009360
00937              MOVE ACCOUNT-SEQU    TO AT-SEQUENCE-NO               00009370
00938                                   OF ACTIVITY-TRAILERS            00009380
00939          ELSE                                                     00009390
00940          IF PHYSICIAN-ADDRESS     OF ACTIVITY-TRAILERS            00009400
00941              ADD +1               TO CL-DOCTOR-ADDR-CNT           00009410
00942                                      DOCTOR-SEQU                  00009420
00943              MOVE DOCTOR-SEQU     TO AT-SEQUENCE-NO               00009430
00944                                   OF ACTIVITY-TRAILERS            00009440
00945          ELSE                                                     00009450
00946          IF EMPLOYER-ADDRESS      OF ACTIVITY-TRAILERS            00009460
00947              ADD +1               TO CL-EMPLOYER-ADDR-CNT         00009470
00948                                      EMPLOYER-SEQU                00009480
00949              MOVE EMPLOYER-SEQU   TO AT-SEQUENCE-NO               00009490
00950                                   OF ACTIVITY-TRAILERS            00009500
00951          ELSE                                                     00009510
00952          IF OTHER-ADDRESS-1       OF ACTIVITY-TRAILERS            00009520
00953              ADD +1               TO CL-OTHER-1-ADDR-CNT          00009530
00954                                      OTHER-1-SEQU                 00009540
00955              MOVE OTHER-1-SEQU    TO AT-SEQUENCE-NO               00009550
00956                                   OF ACTIVITY-TRAILERS            00009560
00957          ELSE                                                     00009570
00958          IF OTHER-ADDRESS-2       OF ACTIVITY-TRAILERS            00009580
00959              ADD +1               TO CL-OTHER-2-ADDR-CNT          00009590
00960                                      OTHER-2-SEQU                 00009600
00961              MOVE OTHER-2-SEQU    TO AT-SEQUENCE-NO               00009610
00962                                   OF ACTIVITY-TRAILERS.           00009620
00963                                                                   00009630
00964      IF INCURRED-CHG-TR           OF ACTIVITY-TRAILERS            00009640
00965          MOVE AT-OLD-DIAG-DESCRIP OF ACTIVITY-TRAILERS            00009650
00966                                   TO WS-OLD-DIAG-DESCRIP          00009660
00967          MOVE SPACES              TO AT-OLD-DIAG-DESCRIP          00009670
00968          OF ACTIVITY-TRAILERS.                                    00009680
00969                                                                   00009690
00970      MOVE ACTIVITY-TRAILERS       TO ACTIVITY-TRAILERS-RCD.       00009700
00971                                                                   00009710
00972      IF RESERVE-EXPENSE-TR OF ACTIVITY-TRAILERS-RCD               00009720
00973          MOVE AT-RECORDED-DT      OF ACTIVITY-TRAILERS-RCD TO     00009730
00974               AT-RESERVES-LAST-MAINT-DT                           00009740
00975                                   OF ACTIVITY-TRAILERS-RCD        00009750
00976          MOVE CL-PROCESSOR-ID     OF CLAIM-MASTER-RCD      TO     00009760
00977               AT-RESERVES-LAST-UPDATED-BY                         00009770
00978                                   OF ACTIVITY-TRAILERS-RCD.       00009780
00979                                                                   00009790
00980      IF PAYMENT-TR         OF ACTIVITY-TRAILERS-RCD               00009800
00981          MOVE AT-RECORDED-DT      OF ACTIVITY-TRAILERS-RCD TO     00009810
00982               AT-PAYMENT-LAST-MAINT-DT                            00009820
00983                                   OF ACTIVITY-TRAILERS-RCD        00009830
00984          MOVE CL-PROCESSOR-ID     OF CLAIM-MASTER-RCD      TO     00009840
00985               AT-PAYMENT-LAST-UPDATED-BY                          00009850
00986                                   OF ACTIVITY-TRAILERS-RCD        00009860
00987          MOVE 'Y'                 TO AT-CASH-PAYMENT              00009870
00988                                   OF ACTIVITY-TRAILERS-RCD        00009880
00989          MOVE 'N'                 TO AT-GROUPED-PAYMENT           00009890
00990                                   OF ACTIVITY-TRAILERS-RCD        00009900
00991          MOVE +0                  TO AT-PAYMENT-NOTE-SEQ-NO       00009910
00992                                   OF ACTIVITY-TRAILERS-RCD.       00009920
00993                                                                   00009930
00994      IF PAYMENT-TR         OF ACTIVITY-TRAILERS                   00009940
00995          IF INSURED-PAID OF ACTIVITY-TRAILERS                     00009950
00996              MOVE 'I'             TO AT-PAYEE-TYPE                00009960
00997                                   OF ACTIVITY-TRAILERS-RCD        00009970
00998              MOVE '1'             TO AT-PAYEE-SEQ                 00009980
00999                                   OF ACTIVITY-TRAILERS-RCD.       00009990
01000                                                                   00010000
01001      IF PAYMENT-TR         OF ACTIVITY-TRAILERS                   00010010
01002          IF BENEFICIARY-PAID OF ACTIVITY-TRAILERS                 00010020
01003              MOVE 'B'             TO AT-PAYEE-TYPE                00010030
01004                                   OF ACTIVITY-TRAILERS-RCD        00010040
01005              MOVE '1'             TO AT-PAYEE-SEQ                 00010050
01006                                   OF ACTIVITY-TRAILERS-RCD.       00010060
01007                                                                   00010070
01008      IF PAYMENT-TR         OF ACTIVITY-TRAILERS                   00010080
01009          IF ACCOUNT-PAID   OF ACTIVITY-TRAILERS                   00010090
01010              MOVE 'A'             TO AT-PAYEE-TYPE                00010100
01011                                   OF ACTIVITY-TRAILERS-RCD        00010110
01012              MOVE '1'             TO AT-PAYEE-SEQ                 00010120
01013                                   OF ACTIVITY-TRAILERS-RCD.       00010130
01014                                                                   00010140
01015      IF PAYMENT-TR         OF ACTIVITY-TRAILERS                   00010150
01016          IF OTHER-1-PAID   OF ACTIVITY-TRAILERS                   00010160
01017              MOVE 'O'             TO AT-PAYEE-TYPE                00010170
01018                                   OF ACTIVITY-TRAILERS-RCD        00010180
01019              MOVE '1'             TO AT-PAYEE-SEQ                 00010190
01020                                   OF ACTIVITY-TRAILERS-RCD.       00010200
01021                                                                   00010210
01022      IF PAYMENT-TR         OF ACTIVITY-TRAILERS                   00010220
01023          IF OTHER-2-PAID   OF ACTIVITY-TRAILERS                   00010230
01024              MOVE 'Q'             TO AT-PAYEE-TYPE                00010240
01025                                   OF ACTIVITY-TRAILERS-RCD        00010250
01026              MOVE '1'             TO AT-PAYEE-SEQ                 00010260
01027                                   OF ACTIVITY-TRAILERS-RCD.       00010270
01028                                                                   00010280
01029      IF PAYMENT-TR         OF ACTIVITY-TRAILERS                   00010290
01030          IF DOCTOR-PAID    OF ACTIVITY-TRAILERS                   00010300
01031              MOVE 'P'             TO AT-PAYEE-TYPE                00010310
01032                                   OF ACTIVITY-TRAILERS-RCD        00010320
01033              MOVE '1'             TO AT-PAYEE-SEQ                 00010330
01034                                   OF ACTIVITY-TRAILERS-RCD.       00010340
01035                                                                   00010350
01036      IF AUTO-PAY-TR        OF ACTIVITY-TRAILERS-RCD               00010360
01037         MOVE ZEROS        TO AT-AUTO-PAY-DAY                      00010370
01038                                   OF ACTIVITY-TRAILERS-RCD        00010380
01039          MOVE AT-RECORDED-DT      OF ACTIVITY-TRAILERS-RCD TO     00010390
01040               AT-AUTO-PAY-LAST-MAINT-DT                           00010400
01041                                   OF ACTIVITY-TRAILERS-RCD        00010410
01042          MOVE CL-PROCESSOR-ID     OF CLAIM-MASTER-RCD      TO     00010420
01043               AT-AUTO-PAY-LAST-UPDATED-BY                         00010430
01044                                   OF ACTIVITY-TRAILERS-RCD.       00010440
01045                                                                   00010450
01046      IF AUTO-PAY-TR        OF ACTIVITY-TRAILERS                   00010460
01047        IF INSURED-PAID-AUTO     OF ACTIVITY-TRAILERS              00010470
01048          MOVE 'I'                TO AT-AUTO-PAYEE-TYPE            00010480
01049                                  OF ACTIVITY-TRAILERS-RCD         00010490
01050          MOVE '1'                TO AT-AUTO-PAYEE-SEQ             00010500
01051                                  OF ACTIVITY-TRAILERS-RCD         00010510
01052        ELSE                                                       00010520
01053          IF BENEFICIARY-PAID-AUTO OF ACTIVITY-TRAILERS            00010530
01054            MOVE 'B'              TO AT-AUTO-PAYEE-TYPE            00010540
01055                                  OF ACTIVITY-TRAILERS-RCD         00010550
01056            MOVE '1'              TO AT-AUTO-PAYEE-SEQ             00010560
01057                                  OF ACTIVITY-TRAILERS-RCD         00010570
01058          ELSE                                                     00010580
01059            IF ACCOUNT-PAID-AUTO OF ACTIVITY-TRAILERS              00010590
01060              MOVE 'A'            TO AT-AUTO-PAYEE-TYPE            00010600
01061                                  OF ACTIVITY-TRAILERS-RCD         00010610
01062              MOVE '1'            TO AT-AUTO-PAYEE-SEQ             00010620
01063                                  OF ACTIVITY-TRAILERS-RCD         00010630
01064            ELSE                                                   00010640
01065              IF OTHER-1-PAID-AUTO OF ACTIVITY-TRAILERS            00010650
01066                MOVE 'O'          TO AT-AUTO-PAYEE-TYPE            00010660
01067                                  OF ACTIVITY-TRAILERS-RCD         00010670
01068                MOVE '1'          TO AT-AUTO-PAYEE-SEQ             00010680
01069                                  OF ACTIVITY-TRAILERS-RCD         00010690
01070              ELSE                                                 00010700
01071                IF OTHER-2-PAID-AUTO OF ACTIVITY-TRAILERS          00010710
01072                  MOVE 'Q'        TO AT-AUTO-PAYEE-TYPE            00010720
01073                                  OF ACTIVITY-TRAILERS-RCD         00010730
01074                  MOVE '1'        TO AT-AUTO-PAYEE-SEQ             00010740
01075                                  OF ACTIVITY-TRAILERS-RCD.        00010750
01076                                                                   00010760
01077      IF CORRESPONDENCE-TR  OF ACTIVITY-TRAILERS                   00010770
01078        IF INSURED-ADDRESEE OF ACTIVITY-TRAILERS                   00010780
01079          MOVE 'I'                TO  AT-ADDRESEE-TYPE             00010790
01080                                  OF  ACTIVITY-TRAILERS-RCD        00010800
01081          MOVE +1                 TO  AT-ADDRESS-REC-SEQ-NO        00010810
01082                                  OF  ACTIVITY-TRAILERS-RCD        00010820
01083        ELSE                                                       00010830
01084          IF BENEFICIARY-ADDRESEE OF ACTIVITY-TRAILERS             00010840
01085            MOVE 'B'              TO  AT-ADDRESEE-TYPE             00010850
01086                                  OF  ACTIVITY-TRAILERS-RCD        00010860
01087            MOVE +11              TO  AT-ADDRESS-REC-SEQ-NO        00010870
01088                                  OF  ACTIVITY-TRAILERS-RCD        00010880
01089          ELSE                                                     00010890
01090            IF ACCOUNT-ADDRESEE OF ACTIVITY-TRAILERS               00010900
01091              MOVE 'A'            TO  AT-ADDRESEE-TYPE             00010910
01092                                  OF  ACTIVITY-TRAILERS-RCD        00010920
01093              MOVE +21            TO  AT-ADDRESS-REC-SEQ-NO        00010930
01094                                  OF  ACTIVITY-TRAILERS-RCD        00010940
01095            ELSE                                                   00010950
01096              IF PHYSICIAN-ADDRESEE OF ACTIVITY-TRAILERS           00010960
01097                MOVE 'P'          TO  AT-ADDRESEE-TYPE             00010970
01098                                  OF  ACTIVITY-TRAILERS-RCD        00010980
01099                MOVE +31          TO  AT-ADDRESS-REC-SEQ-NO        00010990
01100                                  OF  ACTIVITY-TRAILERS-RCD        00011000
01101              ELSE                                                 00011010
01102                IF EMPLOYER-ADDRESEE OF ACTIVITY-TRAILERS          00011020
01103                  MOVE 'E'        TO  AT-ADDRESEE-TYPE             00011030
01104                                  OF  ACTIVITY-TRAILERS-RCD        00011040
01105                  MOVE +41        TO  AT-ADDRESS-REC-SEQ-NO        00011050
01106                                  OF  ACTIVITY-TRAILERS-RCD        00011060
01107                ELSE                                               00011070
01108                  IF OTHER-ADDRESEE-1 OF ACTIVITY-TRAILERS         00011080
01109                    MOVE 'O'      TO  AT-ADDRESEE-TYPE             00011090
01110                                  OF  ACTIVITY-TRAILERS-RCD        00011100
01111                    MOVE +51      TO  AT-ADDRESS-REC-SEQ-NO        00011110
01112                                  OF  ACTIVITY-TRAILERS-RCD        00011120
01113                  ELSE                                             00011130
01114                    IF OTHER-ADDRESEE-2 OF ACTIVITY-TRAILERS       00011140
01115                      MOVE 'Q'    TO  AT-ADDRESEE-TYPE             00011150
01116                                  OF  ACTIVITY-TRAILERS-RCD        00011160
01117                      MOVE +61    TO  AT-ADDRESS-REC-SEQ-NO        00011170
01118                                  OF  ACTIVITY-TRAILERS-RCD.       00011180
01119                                                                   00011190
01120      IF CORRESPONDENCE-TR  OF ACTIVITY-TRAILERS                   00011200
01121          IF ACCOUNT-ADDRESEE OF ACTIVITY-TRAILERS                 00011210
01122              IF AT-ADDRESS-REC-SEQ-NO OF ACTIVITY-TRAILERS = +0   00011220
01123                  MOVE +0         TO  AT-ADDRESS-REC-SEQ-NO        00011230
01124                                  OF  ACTIVITY-TRAILERS-RCD.       00011240
01125                                                                   00011250
01126      IF CORRESPONDENCE-TR  OF ACTIVITY-TRAILERS-RCD               00011260
01127          MOVE AT-RECORDED-DT      OF ACTIVITY-TRAILERS-RCD TO     00011270
01128               AT-CORR-LAST-MAINT-DT                               00011280
01129                                   OF ACTIVITY-TRAILERS-RCD        00011290
01130          MOVE CL-PROCESSOR-ID     OF CLAIM-MASTER-RCD      TO     00011300
01131               AT-CORR-LAST-UPDATED-BY                             00011310
01132                                   OF ACTIVITY-TRAILERS-RCD        00011320
01133          MOVE LOW-VALUES          TO                              00011330
01134               AT-LETTER-PURGED-DT OF ACTIVITY-TRAILERS-RCD.       00011340
01135                                                                   00011350
01136      IF ADDRESS-TR         OF ACTIVITY-TRAILERS                   00011360
01137        IF INSURED-ADDRESS OF ACTIVITY-TRAILERS                    00011370
01138          MOVE 'I'                    TO  AT-ADDRESS-TYPE          00011380
01139                                      OF  ACTIVITY-TRAILERS-RCD    00011390
01140        ELSE                                                       00011400
01141          IF BENEFICIARY-ADDRESS OF ACTIVITY-TRAILERS              00011410
01142            MOVE 'B'                  TO  AT-ADDRESS-TYPE          00011420
01143                                      OF  ACTIVITY-TRAILERS-RCD    00011430
01144          ELSE                                                     00011440
01145            IF ACCOUNT-ADDRESS OF ACTIVITY-TRAILERS                00011450
01146              MOVE 'A'                TO  AT-ADDRESS-TYPE          00011460
01147                                      OF  ACTIVITY-TRAILERS-RCD    00011470
01148            ELSE                                                   00011480
01149              IF PHYSICIAN-ADDRESS OF ACTIVITY-TRAILERS            00011490
01150                MOVE 'P'              TO  AT-ADDRESS-TYPE          00011500
01151                                      OF  ACTIVITY-TRAILERS-RCD    00011510
01152              ELSE                                                 00011520
01153                IF EMPLOYER-ADDRESS OF ACTIVITY-TRAILERS           00011530
01154                  MOVE 'E'            TO  AT-ADDRESS-TYPE          00011540
01155                                      OF  ACTIVITY-TRAILERS-RCD    00011550
01156                ELSE                                               00011560
01157                  IF OTHER-ADDRESS-1 OF ACTIVITY-TRAILERS          00011570
01158                    MOVE 'O'          TO  AT-ADDRESS-TYPE          00011580
01159                                      OF  ACTIVITY-TRAILERS-RCD    00011590
01160                  ELSE                                             00011600
01161                    IF OTHER-ADDRESS-2 OF ACTIVITY-TRAILERS        00011610
01162                      MOVE 'Q'        TO  AT-ADDRESS-TYPE          00011620
01163                                      OF  ACTIVITY-TRAILERS-RCD.   00011630
01164                                                                   00011640
01165      IF ADDRESS-TR         OF ACTIVITY-TRAILERS-RCD               00011650
01166          MOVE AT-RECORDED-DT      OF ACTIVITY-TRAILERS-RCD TO     00011660
01167               AT-ADDRESS-LAST-MAINT-DT                            00011670
01168                                   OF ACTIVITY-TRAILERS-RCD        00011680
01169          MOVE CL-PROCESSOR-ID     OF CLAIM-MASTER-RCD      TO     00011690
01170               AT-ADDRESS-LAST-UPDATED-BY                          00011700
01171                                   OF ACTIVITY-TRAILERS-RCD.       00011710
01172                                                                   00011720
01173      IF GENERAL-INFO-TR    OF ACTIVITY-TRAILERS-RCD               00011730
01174          MOVE AT-RECORDED-DT      OF ACTIVITY-TRAILERS-RCD TO     00011740
01175               AT-GEN-INFO-LAST-MAINT-DT                           00011750
01176                                   OF ACTIVITY-TRAILERS-RCD        00011760
01177          MOVE CL-PROCESSOR-ID     OF CLAIM-MASTER-RCD      TO     00011770
01178               AT-GEN-INFO-LAST-UPDATED-BY                         00011780
01179                                   OF ACTIVITY-TRAILERS-RCD.       00011790
01180                                                                   00011800
01181      IF AUTO-PROMPT-TR     OF ACTIVITY-TRAILERS-RCD               00011810
01182          MOVE AT-RECORDED-DT      OF ACTIVITY-TRAILERS-RCD TO     00011820
01183               AT-PROMPT-LAST-MAINT-DT                             00011830
01184                                   OF ACTIVITY-TRAILERS-RCD        00011840
01185          MOVE CL-PROCESSOR-ID     OF CLAIM-MASTER-RCD      TO     00011850
01186               AT-PROMPT-LAST-UPDATED-BY                           00011860
01187                                   OF ACTIVITY-TRAILERS-RCD.       00011870
01188                                                                   00011880
01189      IF DENIAL-TR          OF ACTIVITY-TRAILERS-RCD               00011890
01190          MOVE AT-RECORDED-DT      OF ACTIVITY-TRAILERS-RCD TO     00011900
01191               AT-DENIAL-LAST-MAINT-DT                             00011910
01192                                   OF ACTIVITY-TRAILERS-RCD        00011920
01193          MOVE CL-PROCESSOR-ID     OF CLAIM-MASTER-RCD      TO     00011930
01194               AT-DENIAL-LAST-UPDATED-BY                           00011940
01195                                   OF ACTIVITY-TRAILERS-RCD.       00011950
01196                                                                   00011960
01197      IF INCURRED-CHG-TR    OF ACTIVITY-TRAILERS-RCD               00011970
01198          MOVE CL-PROCESSOR-ID     OF CLAIM-MASTER-RCD      TO     00011980
01199               AT-INCURRED-LAST-UPDATED-BY                         00011990
01200                                   OF ACTIVITY-TRAILERS-RCD        00012000
01201          MOVE WS-OLD-DIAGNOSIS    TO AT-OLD-DIAG-DESCRIP   OF     00012010
01202               ACTIVITY-TRAILERS-RCD.                              00012020
01203                                                                   00012030
01204      IF FORM-CONTROL-TR    OF ACTIVITY-TRAILERS                   00012040
01205        IF FORM-TO-INSURED OF ACTIVITY-TRAILERS                    00012050
01206          MOVE 'I'                TO  AT-FORM-ADDRESS              00012060
01207                                  OF  ACTIVITY-TRAILERS-RCD        00012070
01208        ELSE                                                       00012080
01209          IF FORM-TO-ACCOUNT OF ACTIVITY-TRAILERS                  00012090
01210            MOVE 'A'              TO  AT-FORM-ADDRESS              00012100
01211                                  OF  ACTIVITY-TRAILERS-RCD        00012110
01212          ELSE                                                     00012120
01213            IF FORM-TO-OTHER-1    OF  ACTIVITY-TRAILERS            00012130
01214              MOVE 'O'            TO  AT-FORM-ADDRESS              00012140
01215                                  OF  ACTIVITY-TRAILERS-RCD        00012150
01216            ELSE                                                   00012160
01217              IF FORM-TO-OTHER-2  OF  ACTIVITY-TRAILERS            00012170
01218                MOVE 'Q'          TO  AT-FORM-ADDRESS              00012180
01219                                  OF  ACTIVITY-TRAILERS-RCD.       00012190
01220                                                                   00012200
01221      IF FORM-CONTROL-TR    OF ACTIVITY-TRAILERS-RCD               00012210
01222          MOVE LOW-VALUES      TO AT-EMP-FORM-SEND-ON-DT           00012220
01223                                   OF ACTIVITY-TRAILERS-RCD        00012230
01224                                  AT-PHY-FORM-SEND-ON-DT           00012240
01225                                   OF ACTIVITY-TRAILERS-RCD        00012250
01226                                  AT-EMP-FORM-ANSWERED-DT          00012260
01227                                   OF ACTIVITY-TRAILERS-RCD        00012270
01228                                  AT-PHY-FORM-ANSWERED-DT          00012280
01229                                   OF ACTIVITY-TRAILERS-RCD        00012290
01230                                  AT-FORM-REM-PRINT-DT             00012300
01231                                   OF ACTIVITY-TRAILERS-RCD        00012310
01232          MOVE AT-RECORDED-DT      OF ACTIVITY-TRAILERS-RCD TO     00012320
01233               AT-FORM-LAST-MAINT-DT                               00012330
01234                                   OF ACTIVITY-TRAILERS-RCD        00012340
01235          MOVE CL-PROCESSOR-ID     OF CLAIM-MASTER-RCD      TO     00012350
01236               AT-FORM-LAST-UPDATED-BY                             00012360
01237                                   OF ACTIVITY-TRAILERS-RCD.       00012370
01238                                                                   00012380
01239      PERFORM 2900-RELEASE-SORT-RECORD THRU 2900-EXIT.             00012390
01240                                                                   00012400
01241  2200-READ-NEXT-TRAILER.                                          00012410
01242                                                                   00012420
01243      PERFORM 2700-READ-CLAIM-TRAILER THRU 2700-EXIT.              00012430
01244                                                                   00012440
01245  2200-EXIT.                                                       00012450
01246      EXIT.                                                        00012460
01247                                                                   00012470
01248      EJECT                                                        00012480
01249  2300-TABLE-CLAIM-MASTERS.                                        00012490
01250                                                                   00012500
01251      IF TBL-COMPANY-CD EQUAL CL-COMPANY-CD OF CLAIM-MASTER-RCD AND00012510
01252         TBL-CARRIER    EQUAL CL-CARRIER    OF CLAIM-MASTER-RCD AND00012520
01253         TBL-CLAIM-NO   EQUAL CL-CLAIM-NO   OF CLAIM-MASTER-RCD    00012530
01254         COMPUTE TBL-CLAIM-CNT EQUAL TBL-CLAIM-CNT + ONE           00012540
01255             IF TBL-CLAIM-CNT LESS THAN TBL-MAX-CLAIM-CNT          00012550
01256                 MOVE CLAIM-MASTER-RCD                             00012560
01257                              TO TBL-CLAIM-RCDS (TBL-CLAIM-CNT)    00012570
01258                 GO TO 2300-EXIT                                   00012580
01259             ELSE                                                  00012590
01260                 DISPLAY '**** ELMSTR TABLE EXCEEDED ****'         00012600
01261                 DISPLAY '**** ELMSTR TABLE EXCEEDED ****'         00012610
01262                 DISPLAY '**** ELMSTR TABLE EXCEEDED ****'         00012620
01263                 DISPLAY '**** ELMSTR TABLE EXCEEDED ****'         00012630
01264                 DIVIDE WS-ZERO BY WS-ZERO GIVING WS-ZERO.         00012640
01265                                                                   00012650
01266      MOVE CLAIM-MASTER-RCD       TO TBL-SAVE-CLAIM-RCD.           00012660
01267      MOVE TBL-CERT-NO (ONE)      TO WS-PRIME-CERT.                00012670
01268      PERFORM 2400-WRITE-CLAIM-MASTERS THRU 2400-EXIT              00012680
01269          VARYING TBL-SEQ-CNT FROM +1 BY +1                        00012690
01270              UNTIL TBL-SEQ-CNT GREATER THAN TBL-CLAIM-CNT.        00012700
01271                                                                   00012710
01272      MOVE ONE                    TO TBL-CLAIM-CNT.                00012720
01273      MOVE TBL-SAVE-CLAIM-RCD     TO TBL-CLAIM-RCDS (TBL-CLAIM-CNT)00012730
01274      MOVE TBL-SAVE-COMPANY-CD TO TBL-COMPANY-CD.                  00012740
01275      MOVE TBL-SAVE-CARRIER       TO TBL-CARRIER.                  00012750
01276      MOVE TBL-SAVE-CLAIM-NO      TO TBL-CLAIM-NO.                 00012760
01277                                                                   00012770
01278  2300-EXIT.                                                       00012780
01279      EXIT.                                                        00012790
01280                                                                   00012800
01281  2400-WRITE-CLAIM-MASTERS.                                        00012810
01282                                                                   00012820
01283      MOVE TBL-CLAIM-RCDS (TBL-SEQ-CNT)                            00012830
01284                                  TO CLAIM-MASTER-RCD.             00012840
01285      MOVE WS-PRIME-CERT          TO CL-PRIME-CERT-NO.             00012850
01286      MOVE TBL-SEQ-CNT            TO CL-ASSOC-CERT-SEQU.           00012860
01287      MOVE TBL-CLAIM-CNT          TO CL-ASSOC-CERT-TOTAL.          00012870
01288                                                                   00012880
01289      IF TBL-CLAIM-CNT GREATER THAN HIGH-TABLE-CNT                 00012890
01290          MOVE TBL-CLAIM-CNT      TO HIGH-TABLE-CNT.               00012900
01291                                                                   00012910
01292      PERFORM 2800-WRITE-CLAIM-MASTER THRU 2800-EXIT.              00012920
01293                                                                   00012930
01294  2400-EXIT.                                                       00012940
01295      EXIT.                                                        00012950
01296                                                                   00012960
01297      EJECT                                                        00012970
01298 ******************************************************************00012980
01299 ***      R E A D   C L A I M   M A S T E R   R O U T I N E     ***00012990
01300 ******************************************************************00013000
01301                                                                   00013010
01302  2600-READ-CLAIM-MASTER.                                          00013020
01303                                                                   00013030
01304      READ CLAIM-MASTER-IN                                         00013040
01305          AT END                                                   00013050
01306                 MOVE HIGH-VALUES TO CLAIM-MASTER                  00013060
01307                 GO TO 2600-EXIT.                                  00013070
01308                                                                   00013080
01309      ADD +1                      TO CLAIM-IN-CNT.                 00013090
01310                                                                   00013100
01311  2600-EXIT.                                                       00013110
01312      EXIT.                                                        00013120
01313                                                                   00013130
01314 ******************************************************************00013140
01315 ***    R E A D   C L A I M   T R A I L E R    R O U T I N E     **00013150
01316 ******************************************************************00013160
01317                                                                   00013170
01318  2700-READ-CLAIM-TRAILER.                                         00013180
01319                                                                   00013190
01320      READ TRAIL-MASTER-IN                                         00013200
01321          AT END                                                   00013210
01322              MOVE HIGH-VALUES TO ACTIVITY-TRAILERS                00013220
01323                                  WS-TRAILER-PRIMARY               00013230
01324                  GO TO 2700-EXIT.                                 00013240
01325                                                                   00013250
01326      ADD +1                      TO TRLR-IN-CNT.                  00013260
01327                                                                   00013270
01328      MOVE AT-COMPANY-CD OF ACTIVITY-TRAILERS TO WS-COMPANY-CD.    00013280
01329      MOVE AT-CARRIER    OF ACTIVITY-TRAILERS TO WS-CARRIER.       00013290
01330      MOVE AT-CLAIM-NO   OF ACTIVITY-TRAILERS TO WS-CLAIM-NO.      00013300
01331      MOVE AT-CERT-NO    OF ACTIVITY-TRAILERS TO WS-CERT-NO.       00013310
01332                                                                   00013320
01333  2700-EXIT.                                                       00013330
01334      EXIT.                                                        00013340
01335                                                                   00013350
01336      EJECT                                                        00013360
01337 ******************************************************************00013370
01338 ***    W R I T E   C L A I M   M A S T E R   R O U T I N E     ***00013380
01339 ******************************************************************00013390
01340                                                                   00013400
01341  2800-WRITE-CLAIM-MASTER.                                         00013410
01342                                                                   00013420
01343      WRITE CLAIM-MASTER-RCD.                                      00013430
01344                                                                   00013440
01345      ADD +1                      TO CLAIM-OUT-CNT.                00013450
01346                                                                   00013460
01347  2800-EXIT.                                                       00013470
01348      EXIT.                                                        00013480
01349                                                                   00013490
01350 ********************************************************          00013500
01351 ***     R E A L E A S E   S O R T   R E C O R D      ***          00013510
01352 ********************************************************          00013520
01353                                                                   00013530
01354  2900-RELEASE-SORT-RECORD.                                        00013540
01355                                                                   00013550
01356      ADD +1                      TO SORT-RELEASE-CNT.             00013560
01357                                                                   00013570
01358      RELEASE SW-RECORD FROM ACTIVITY-TRAILERS-RCD.                00013580
01359                                                                   00013590
01360  2900-EXIT.                                                       00013600
01361      EXIT.                                                        00013610
01362                                                                   00013620
01363      EJECT                                                        00013630
01364  3000-SORT-OUTPUT-PROCEDURE   SECTION.                            00013640
01365                                                                   00013650
01366      RETURN SORT-WORK INTO ACTIVITY-TRAILERS-RCD                  00013660
01367          AT END  GO TO 3000-EXIT.                                 00013670
01368                                                                   00013680
01369      ADD +1                      TO SORT-RETURN-CNT.              00013690
01370                                                                   00013700
01371  3000-WRITE-CONVERTED-RCD.                                        00013710
01372                                                                   00013720
01373      ADD +1                      TO TRLR-OUT-CNT.                 00013730
01374                                                                   00013740
01375      WRITE ACTIVITY-TRAILERS-RCD.                                 00013750
01376                                                                   00013760
01377      GO TO 3000-SORT-OUTPUT-PROCEDURE.                            00013770
01378                                                                   00013780
01379  3000-EXIT.                                                       00013790
01380      EXIT.                                                        00013800
01381                                                                   00013810
01382      EJECT                                                        00013820
01383 ******************************************************************00013830
01384 ***          E N D   O F   J O B   P R O C E S S I N G         ***00013840
01385 ******************************************************************00013850
01386                                                                   00013860
01387  9910-EOJ-1.                                                      00013870
01388                                                                   00013880
01389                                                                   00013890
01390  9990-FINAL-CLOSE.                                                00013900
01391                                                                   00013910
01392      DISPLAY '****   CLAIM MASTER    READ = ' CLAIM-IN-CNT.       00013920
01393      DISPLAY '****   CLAIM MASTER WRITTEN = ' CLAIM-OUT-CNT.      00013930
01394      DISPLAY '****      RELEASED TO  SORT = ' SORT-RELEASE-CNT.   00013940
01395      DISPLAY '****    RETURNED FROM  SORT = ' SORT-RETURN-CNT     00013950
01396      DISPLAY '****   TRAILER RECORDS READ = ' TRLR-IN-CNT.        00013960
01397      DISPLAY '****  DIAG TRAILERS CREATED = ' DIAGNOSIS-TRLRS-CNT.00013970
01398      DISPLAY '****TRAILER RECORDS WRITTEN = ' TRLR-OUT-CNT.       00013980
01399      DISPLAY '**** LARGEST CLM MSTR GROUP = ' HIGH-TABLE-CNT.     00013990
01400                                                                   00014000
01401      CLOSE CLAIM-MASTER-IN                                        00014010
01402            CLAIM-MASTER-OUT                                       00014020
01403            TRAIL-MASTER-IN                                        00014030
01404            TRAIL-MASTER-OUT.                                      00014040
01405                                                                   00014050
01406      GO TO 9999-END-THE-JOB.                                      00014060
01407                                                                   00014070
01408      EJECT                                                        00014080
01409  ABEND-PGM.                                                       00014090
01410                                  COPY ELCABEND.                   00014100
01411  SKIP3                                                            00014110
01412  9999-END-THE-JOB.                                                00014120
01413                                                                   00014130
01414      GOBACK.                                                      00014140
