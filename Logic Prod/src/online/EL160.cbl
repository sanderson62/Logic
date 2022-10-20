00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL160 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 05/16/95 15:27:49.
00007 *                            VMOD=2.018.
00008 *
00008 *
00009 *AUTHOR.        LOGIC, INC.
00010 *               DALLAS, TEXAS.
00011
00024 *REMARKS. TRANSACTION EX33 - CLAIM AUDIT.
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00025
00026      EJECT
00027  ENVIRONMENT DIVISION.
00028
00029  DATA DIVISION.
00030
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*   EL160  WORKING STORAGE     *'.
00034  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.018 **********'.
00035
00036  01  LCP-TIME-OF-DAY-XX.
00037      05  LCP-TIME-OF-DAY-68        PIC 9(6).
00038      05  FILLER                    PIC 99.
00039  01  LCP-CICS-TIME                 PIC 9(15).
00040
00041 *    COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
00042
00043 *    COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
00044
00045  01  WS-DATE-AREA.
00046      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00047      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00048
00049  01  LITERALS-NUMBERS.
00050      12  SC-ITEM                 PIC S9(4)   VALUE +0001  COMP.
00051      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.
00052      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.
00053      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.
00054      12  XCTL-EL1602             PIC X(8)    VALUE 'EL1602'.
00055      12  THIS-PGM                PIC X(8)    VALUE 'EL160'.
00056      12  DATE-CONV               PIC X(8)    VALUE 'ELDATCV'.
00057      12  REM-TERM-PGM            PIC X(8)    VALUE 'ELRTRM '.
00058      12  THIS-TRAN               PIC X(4)    VALUE 'EX33'.
00059      12  LIT-MAP                 PIC X(4)    VALUE '160A'.
00060      12  LIT-MAP-2               PIC X(4)    VALUE '160B'.
00061      12  MAX-TS-PAGES            PIC 9999    VALUE 250.
00062      12  ALL-NINES               PIC 9(7)V99  VALUE 9999999.99.
00063
00064  01  EDIT-WORK-AREA.
00065      12  WS-SEX-SELECTION        PIC X VALUE SPACE.
00066          88  MALE-SELECTION       VALUE 'M'.
00067          88  FEMALE-SELECTION     VALUE 'F'.
00068      12  CALL-PGM                PIC X(8).
00069      12  TRANS-ID                PIC X(4).
00070      12  CHECK-PFKEYS            PIC 99.
00071      12  TEST-RESP               PIC X.
00072      12  DAYS-PAID               PIC ZZZZ9.
00073      12  PMTS-MADE               PIC ZZZZZ9.
00074      12  EDIT-DOLLARS-8          PIC ZZZZZ.99.
00075      12  EDIT-DOLLARS-9          PIC ZZZZZZ.99.
00076      12  EDIT-APR                PIC ZZ9.9999.
00077      12  COUNT-2                 PIC 99.
00078      12  HOLD-BENEFIT            PIC XX.
00079
00080      12  WS-RESPONSE             PIC S9(8) COMP.
00081          88  WS-RESP-NORMAL                VALUE +00.
00082          88  WS-RESP-NOTFND                VALUE +13.
00083
00084      12  TEST-DATE.
00085          16  FILLER              PIC XX.
00086          16  BIF-DATE            PIC X(6).
00087
00088      12  TEST-AMT.
00089          16  FILLER              PIC X.
00090          16  BIF-AMT             PIC X(9).
00091          16  AMT-BIF REDEFINES BIF-AMT PIC 9(7)V99.
00092
00093      12  W-FILE-ID               PIC X(8) VALUE 'ELMSTR'.
00094      12  W-VALID-FILE-IND        PIC X(8).
00095          88  W-VALID-FILE                  VALUE ' ' 'M' 'R'.
00096          88  W-RETRIEVE                    VALUE 'R'.
00097          88  W-MASTER                      VALUE ' ' 'M'.
00098
00099      12  WORK-DATE-MDY.
00100          16  MONTH-WORK          PIC XX.
00101          16  FILLER              PIC X(4).
00102          16  YEAR-WORK           PIC XX.
00103
00104      12  WORK-DATE-MY.
00105          16  WORK-MONTH          PIC XX.
00106          16  FILLER              PIC X       VALUE '/'.
00107          16  WORK-YEAR           PIC XX.
00108
00109      12  HOLD-TERM-REM.
00110          16  HOLD-ORIG-TERM      PIC ZZ9.
00111          16  FILLER              PIC X       VALUE '/'.
00112          16  HOLD-REM            PIC ZZ9.
00113
00114      12  CURRENT-DATE-BIN        PIC X(2).
00115
00116      12  WS-FORM-SAVE            PIC X(12).
00117
00118      12  WS-AGE                  PIC 9(04).
00119      12  WS-AGE-R REDEFINES WS-AGE.
00120          16  WS-AGE-1-2          PIC 9(02).
00121          16  WS-AGE-3-4          PIC 9(02).
00122
00123      12  WS-PAID-TO-HDG          PIC X(29)   VALUE
00124          'CAUSE CD  EST. END   PAID  TO'.
00125
00126  01  CNTL-WORK-AREA.
00127      12  CARRIER-CNTL            PIC X.
00128      12  INC-DATE-LOW-CNTL       PIC XX.
00129      12  INC-DATE-HIGH-CNTL      PIC XX.
00130      12  GROUP-CNTL              PIC X(06).
00131      12  LST-PMT-LOW-CNTL        PIC XX.
00132      12  LST-PMT-HIGH-CNTL       PIC XX.
00133      12  STATE-CNTL              PIC XX.
00134      12  MO-OPEN-LOW-CNTL        PIC S9(4)       COMP.
00135      12  MO-OPEN-HIGH-CNTL       PIC S9(4)       COMP.
00136      12  ACCOUNT-CNTL            PIC X(10).
00137      12  AMT-PAID-LOW-CNTL       PIC S9(7)V99    COMP-3.
00138      12  AMT-PAID-HIGH-CNTL      PIC S9(7)V99   COMP-3.
00139      12  TYPE-CNTL               PIC X.
00140      12  CAUSE-CD-LOW-CNTL       PIC X(6).
00141      12  CAUSE-CD-HIGH-CNTL      PIC X(6).
00142      12  DEN-CNTL                PIC X.
00143      12  REP-DATE-LOW-CNTL       PIC XX.
00144      12  REP-DATE-HIGH-CNTL      PIC XX.
00145      12  PROC-CNTL               PIC X(4).
00146      12  LST-PAID-LOW-CNTL       PIC S9(7)V99   COMP-3.
00147      12  LST-PAID-HIGH-CNTL      PIC S9(7)V99   COMP-3.
00148      12  PREM-CNTL               PIC X.
00149      12  MNT-DATE-LOW-CNTL       PIC XX.
00150      12  MNT-DATE-HIGH-CNTL      PIC XX.
00151      12  REQ-CNTL                PIC X.
00152      12  EST-DATE-LOW-CNTL       PIC XX.
00153      12  EST-DATE-HIGH-CNTL      PIC XX.
00154      12  SUPR-CNTL               PIC X.
00155      12  FOL-DATE-LOW-CNTL       PIC XX.
00156      12  FOL-DATE-HIGH-CNTL      PIC XX.
00157      12  CERT-CNTL               PIC X.
00158      12  DAYS-LOW-CNTL           PIC S9(4)       COMP.
00159      12  DAYS-HIGH-CNTL          PIC S9(4)       COMP.
00160      12  PRI-CNTL                PIC X.
00161      12  AUTO-CNTL               PIC X.
00162      12  OPCL-CNTL               PIC X.
00163
00164  01  TIME-IN.
00165      12  UN-HOURS                PIC XX.
00166      12  UN-MINUTES              PIC XX.
00167      12  FILLER                  PIC X(4).
00168
00169  01  TIME-OUT.
00170      12  FOR-HOURS               PIC XX.
00171      12  FILLER                  PIC X       VALUE '.'.
00172      12  FOR-MINUTES             PIC XX.
00173
00174  01  ERROR-NUMBERS.
00175      12  ER-0008                 PIC X(4)    VALUE '0008'.
00176      12  ER-0029                 PIC X(4)    VALUE '0029'.
00177      12  ER-0042                 PIC X(4)    VALUE '0042'.
00178      12  ER-0046                 PIC X(4)    VALUE '0046'.
00179      12  ER-0070                 PIC X(4)    VALUE '0070'.
00180      12  ER-0142                 PIC X(4)    VALUE '0142'.
00181      12  ER-0143                 PIC X(4)    VALUE '0143'.
00182      12  ER-0154                 PIC X(4)    VALUE '0154'.
00183      12  ER-0169                 PIC X(4)    VALUE '0169'.
00184      12  ER-0172                 PIC X(4)    VALUE '0172'.
00185      12  ER-0192                 PIC X(4)    VALUE '0192'.
00186      12  ER-0199                 PIC X(4)    VALUE '0199'.
00187      12  ER-0205                 PIC X(4)    VALUE '0205'.
00188      12  ER-0206                 PIC X(4)    VALUE '0206'.
00189      12  ER-0219                 PIC X(4)    VALUE '0219'.
00190      12  ER-0227                 PIC X(4)    VALUE '0227'.
00191      12  ER-0273                 PIC X(4)    VALUE '0273'.
00192      12  ER-0274                 PIC X(4)    VALUE '0274'.
00193      12  ER-0282                 PIC X(4)    VALUE '0282'.
00194      12  ER-0283                 PIC X(4)    VALUE '0283'.
00195      12  ER-0304                 PIC X(4)    VALUE '0304'.
00196      12  ER-0306                 PIC X(4)    VALUE '0306'.
00197      12  ER-0307                 PIC X(4)    VALUE '0307'.
00198      12  ER-0308                 PIC X(4)    VALUE '0308'.
00199      12  ER-0334                 PIC X(4)    VALUE '0334'.
00200      12  ER-0335                 PIC X(4)    VALUE '0335'.
00201      12  ER-0419                 PIC X(4)    VALUE '0419'.
00202      12  ER-0767                 PIC X(4)    VALUE '0767'.
00203      12  ER-0970                 PIC X(4)    VALUE '0970'.
00204      12  ER-2381                 PIC X(4)    VALUE '2381'.
00205      12  ER-2848                 PIC X(4)    VALUE '2848'.
00206      12  ER-9483                 PIC X(4)    VALUE '9483'.
00207      12  ER-9811                 PIC X(4)    VALUE '9811'.
00208
00209  01  HOLD-KEY.
00210      12  HOLD-TERM               PIC X(4).
00211      12  KEY-QUAL                PIC X(4).
00212
00213  01  ERROR-SWITCHES.
00214      12  ERROR-SWITCH            PIC X.
00215          88  SCREEN-ERROR                    VALUE 'X'.
00216
00217      12  BUILD-SWITCH            PIC X.
00218          88  NO-RECORDS                      VALUE 'X'.
00219          88  BUILD-COMPLETE                  VALUE 'Y'.
00220          88  SCREEN-HAS-ERRORS               VALUE 'X'.
00221
00222      12  PROC-SWITCH             PIC X.
00223          88  PROC-SELECTED                   VALUE 'X'.
00224
00225  01  MSTR-KEY.
00226      12  MSTR-COMPANY-CODE       PIC X.
00227      12  MSTR-CARRIER            PIC X.
00228      12  REST-OF-KEY             PIC X(18).
00229
00230  01  MSTR-KEY-4.
00231      12  MSTR-COMPANY-CODE-4     PIC X.
00232      12  MSTR-USER-ID-4          PIC X(4).
00233
00234  01  CERT-KEY.
00235      12  CERT-COMPANY-CODE       PIC X.
00236      12  CERT-CARRIER            PIC X.
00237      12  CERT-GROUP              PIC X(6).
00238      12  CERT-STATE              PIC XX.
00239      12  CERT-ACCOUNT            PIC X(10).
00240      12  CERT-DATE               PIC XX.
00241      12  CERT-CERT               PIC X(11).
00242
00243  01  TRLR-KEY.
00244      12  TRLR-MAIN-KEY           PIC X(20).
00245      12  TRLR-SEQ-NO             PIC 9(4)    COMP.
00246
00247  01  CNTL-KEY.
00248      12  COMPANY-ID              PIC X(3).
00249      12  RECORD-TYPE             PIC X.
00250      12  ACCESS-CD-GENL          PIC X(4).
00251      12  SEQUENCE-NO             PIC 9(4)    COMP.
00252
00253  01  BENEFIT-KEY.
00254      12  BEN-CO-ID               PIC X(3).
00255      12  BEN-REC-TYPE            PIC X.
00256      12  FILLER                  PIC XX.
00257      12  BEN-ACC-CD              PIC XX.
00258      12  BEN-SEQ-NO              PIC S9(4)   COMP.
00259
00260  01  EMPLCY-KEY.
00261      12  EMPLCY-COMPANY-CD       PIC X(01).
00262      12  EMPLCY-CARRIER          PIC X(01).
00263      12  EMPLCY-GROUPING         PIC X(06).
00264      12  EMPLCY-STATE            PIC X(02).
00265      12  EMPLCY-PRODUCER         PIC X(10).
00266      12  EMPLCY-EFF-DT           PIC X(02).
00267      12  EMPLCY-REFERENCE-NO     PIC X(20).
00268
00269  01  EMPLAN-KEY.
00270      12  EMPLAN-COMPANY-CD       PIC X(01).
00271      12  EMPLAN-CARRIER          PIC X(01).
00272      12  EMPLAN-GROUPING         PIC X(06).
00273      12  EMPLAN-STATE            PIC X(02).
00274      12  EMPLAN-PRODUCER         PIC X(10).
00275      12  EMPLAN-PLAN-CODE        PIC X(02).
00276      12  EMPLAN-REV-NO           PIC 9(03).
00277
00278  01  COMP-LENGTHS.
00279      12  COUNT-1                 PIC S9(4)   COMP.
00280      12  EL160A-LENGTH           PIC S9(4)   COMP VALUE +515.
00281      12  EL160B-LENGTH           PIC S9(4)   COMP VALUE +881.
00282      12  DATE-LENGTH             PIC S9(4)   COMP VALUE +8.
00283      12  MO-DAY-LENGTH           PIC S9(4)   COMP VALUE +3.
00284      12  AMT-LENGTH              PIC S9(4)   COMP VALUE +10.
00285
00286      EJECT
00287 *    COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
00288
00289      12  EL160-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00290          16  PI-TS-COUNT         PIC S9(4)   COMP.
00291          16  PI-TS-COUNT-1       PIC S9(4)   COMP.
00292          16  PI-EL160-KEY        PIC X(8).
00293          16  PI-EL1602-KEY       PIC X(8).
00294          16  PI-PRINT-OPTION     PIC X.
00295          16  PI-FORMAT-OPTION    PIC X.
00296          16  PI-PRINT-ID         PIC X(4).
00297          16  PI-ALT-PRINT-ID     PIC X(4).
00298          16  PI-FILE-ID-IND      PIC X.
00299              88  PI-RETRIEVAL-FILE           VALUE 'R'.
00300              88  PI-MASTER-FILE              VALUE 'M'.
00301          16  FILLER              PIC X(609).
00302
00303      EJECT
00304 *    COPY ELCLOGOF SUPPRESS.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
00305
00306 *    COPY ELCCALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCALC.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
00008 *                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
00009 *                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
00010 *                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
00011 *                                                                *
00012 *  PASSED TO ELRTRM                                              *
00013 *  -----------------                                             *
00014 *  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
00015 *  ORIGINAL TERM                                                 *
00016 *  BEGINNING DATE                                                *
00017 *  ENDING DATE                                                   *
00018 *  COMPANY I.D.                                                  *
00019 *  ACCOUNT MASTER USER FIELD                                     *
00020 *  PROCESS SWITCH (CANCEL, CLAIM)                                *
00021 *  FREE LOOK DAYS                                                *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  REMAINING TERM 1 - USED FOR EARNINGS                          *
00026 *  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
00027 *  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
00028 *  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
00029 *----------------------------------------------------------------*
00030 *  PASSED TO ELRAMT                                              *
00031 *  ----------------                                              *
00032 *  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
00033 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00034 *  ORIGINAL AMOUNT                                               *
00035 *  ALTERNATE BENEFIT (BALLON)                                    *
00036 *  A.P.R. - NET PAY ONLY                                         *
00037 *  METHOD
00038 *  PAYMENT FREQUENCY - FOR FARM PLAN                             *
00039 *  COMPANY I.D.                                                  *
00040 *  BENEFIT TYPE                                                  *
00041 *                                                                *
00042 *  RETURNED FROM ELRAMT                                          *
00043 *  --------------------                                          *
00044 *  REMAINING AMOUNT 1 - CURRENT                                  *
00045 *  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
00046 *  REMAINING AMOUNT FACTOR
00047 *----------------------------------------------------------------*
00048 *  PASSED TO ELRESV                                              *
00049 *  -----------------                                             *
00050 *  CERTIFICATE EFFECTIVE DATE                                    *
00051 *  VALUATION DATE                                                *
00052 *  PAID THRU DATE                                                *
00053 *  BENEFIT                                                       *
00054 *  INCURRED DATE                                                 *
00055 *  REPORTED DATE                                                 *
00056 *  ISSUE AGE                                                     *
00057 *  TERM                                                          *
00058 *  CDT PERCENT                                                   *
00059 *  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
00060 * *CLAIM TYPE (LIFE, A/H)                                        *
00061 * *REMAINING BENEFIT (FROM ELRAMT)                               *
00062 * *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
00063 *                                                                *
00064 *  RETURNED FROM ELRESV                                          *
00065 *  --------------------                                          *
00066 *  CDT TABLE USED                                                *
00067 *  CDT FACTOR USED                                               *
00068 *  PAY TO CURRENT RESERVE                                        *
00069 *  I.B.N.R. - A/H ONLY                                           *
00070 *  FUTURE (ACCRUED) AH ONLY                                      *
00071 *----------------------------------------------------------------*
00072 *  PASSED TO ELRATE                                              *
00073 *  ----------------                                              *
00074 *  CERT ISSUE DATE                                               *
00075 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00076 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00077 *  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
00078 *  STATE CODE (CLIENT DEFINED)                                   *
00079 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00080 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00081 *  DEVIATION CODE                                                *
00082 *  ISSUE AGE                                                     *
00083 *  ORIGINAL BENEFIT AMOUNT                                       *
00084 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00085 *  PROCESS TYPE (ISSUE OR CANCEL)                                *
00086 *  BENEFIT KIND (LIFE OR A/H)                                    *
00087 *  A.P.R.                                                        *
00088 *  METHOD
00089 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00090 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00091 *  COMPANY I.D. (3 CHARACTER)                                    *
00092 *  BENEFIT CODE                                                  *
00093 *  BENEFIT OVERRIDE CODE                                         *
00094 *  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
00095 *  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
00096 *  JOINT INDICATOR (CSL ONLY)                                    *
00097 *  FIRST PAYMENT DATE (CSL ONLY)                                 *
00098 *  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
00099 *                                                                *
00100 *  RETURNED FROM ELRATE                                          *
00101 *  --------------------                                          *
00102 *  CALCULATED PREMIUM                                            *
00103 *  PREMIUM RATE                                                  *
00104 *  MORTALITY CODE                                                *
00105 *  MAX ATTAINED AGE                                              *
00106 *  MAX AGE                                                       *
00107 *  MAX TERM                                                      *
00108 *  MAX MONTHLY BENEFIT                                           *
00109 *  MAX TOTAL BENIFIT                                             *
00110 *  COMPOSITE RATE (OPEN-END ONLY)                                *
00111 *----------------------------------------------------------------*
00112 *  PASSED TO ELRFND                                              *
00113 *  ----------------                                              *
00114 *  CERT ISSUE DATE                                               *
00115 *  REFUND DATE                                                   *
00116 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00117 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00118 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00119 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00120 *  STATE CODE (CLIENT DEFINED)                                   *
00121 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00122 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00123 *  DEVIATION CODE                                                *
00124 *  ISSUE AGE                                                     *
00125 *  ORIGINAL BENEFIT AMOUNT                                       *
00126 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00127 *  PROCESS TYPE (CANCEL)                                         *
00128 *  BENEFIT KIND (LIFE OR A/H)                                    *
00129 *  A.P.R.                                                        *
00130 *  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
00131 *  RATING METHOD -  (CODE FROM BENEFIT)                          *
00132 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00133 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00134 *  COMPANY I.D. (3 CHARACTER)                                    *
00135 *  BENEFIT CODE                                                  *
00136 *  BENEFIT OVERRIDE CODE                                         *
00137 *                                                                *
00138 *  RETURNED FROM ELRFND                                          *
00139 *  --------------------                                          *
00140 *  CALCULATED REFUND                                             *
00141 *----------------------------------------------------------------*
00142 *  PASSED TO ELEARN                                              *
00143 *  ----------------                                              *
00144 *  CERT ISSUE DATE                                               *
00145 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00146 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00147 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00148 *  STATE CODE (CLIENT DEFINED)                                   *
00149 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00150 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00151 *  DEVIATION CODE                                                *
00152 *  ISSUE AGE                                                     *
00153 *  ORIGINAL BENEFIT AMOUNT                                       *
00154 *  BENEFIT KIND (LIFE OR A/H)                                    *
00155 *  A.P.R.                                                        *
00156 *  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
00157 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00158 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00159 *  COMPANY I.D. (3 CHARACTER)                                    *
00160 *  BENEFIT CODE                                                  *
00161 *  BENEFIT OVERRIDE CODE                                         *
00162 *                                                                *
00163 *  RETURNED FROM ELEARN                                          *
00164 *  --------------------                                          *
00165 *  INDICATED  EARNINGS                                           *
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 450                                   *
00168 *                                                                *
00169 ******************************************************************
010303******************************************************************
010303*                   C H A N G E   L O G
010303*
010303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010303*-----------------------------------------------------------------
010303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010303* EFFECTIVE    NUMBER
010303*-----------------------------------------------------------------
010303* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
101807* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
010410* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
010410* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
041310* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
041710* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
101110* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
071211* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
040615* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
012820* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
010303******************************************************************
00170
00171  01  CALCULATION-PASS-AREA.
00172      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
00173                                      COMP.
00174
00175      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CP-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
012820                                  '9' 'A' 'B' 'C' 'D' 'E' 'H' 'I'.
00179        88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
00180        88  CP-ERROR-IN-DATES                       VALUE '2'.
00181        88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
00182        88  CP-ERROR-IN-TERMS                       VALUE '4'.
00183        88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
00184        88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
00185        88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
00186        88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
00187        88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
00188        88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
00189        88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
00190        88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
00191        88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
00192        88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
00193        88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
00194        88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
00195        88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
012820       88  CP-ERROR-TERM-BELOW-MINIMUM             VALUE 'I'.
00196
00197      12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
00198        88  NO-CP-ERROR-2                           VALUE ZERO.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
00202          16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
00203          16  CP-CERT-EFF-DT        PIC XX.
00204          16  CP-VALUATION-DT       PIC XX.
00205          16  CP-PAID-THRU-DT       PIC XX.
00206          16  CP-BENEFIT-TYPE       PIC X.
00207            88  CP-AH                               VALUE 'A' 'D'
00208                                                    'I' 'U'.
00209            88  CP-REDUCING-LIFE                    VALUE 'R'.
00210            88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
00211          16  CP-INCURRED-DT        PIC XX.
00212          16  CP-REPORTED-DT        PIC XX.
00213          16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
00214          16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
00215          16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
00216                                      COMP-3.
00217          16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
00218                                      COMP-3.
00219          16  CP-CDT-METHOD         PIC X.
00220            88  CP-CDT-ROUND-NEAR                   VALUE '1'.
00221            88  CP-CDT-ROUND-HIGH                   VALUE '2'.
00222            88  CP-CDT-INTERPOLATED                 VALUE '3'.
00223          16  CP-CLAIM-TYPE         PIC X.
00224            88  CP-AH-CLAIM                         VALUE 'A'.
00225            88  CP-LIFE-CLAIM                       VALUE 'L'.
00226          16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
00227                                      COMP-3.
00228          16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
00229                                      COMP-3.
00230          16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
00231                                      COMP-3.
00232          16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
00233                                      COMP-3.
00234          16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
00235                                      COMP-3.
00236          16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
00237                                      COMP-3.
00238          16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
00239                                      COMP-3.
00240          16  CP-REM-TERM-METHOD    PIC X.
00241            88  CP-EARN-AFTER-15TH                  VALUE '1'.
00242            88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
00243            88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
00244            88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
00245            88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
00246            88  CP-EARN-AFTER-14TH                  VALUE '6'.
00247            88  CP-EARN-AFTER-16TH                  VALUE '7'.
00248          16  CP-EARNING-METHOD     PIC X.
00249            88  CP-EARN-BY-R78                      VALUE '1' 'R'.
00250            88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
00251            88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
00252            88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
00253            88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
00254            88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
00255            88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
00256            88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
00257            88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
00258            88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
033104           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
033104           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
092310           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
                 88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
00259          16  CP-PROCESS-TYPE       PIC X.
00260            88  CP-CLAIM                            VALUE '1'.
00261            88  CP-CANCEL                           VALUE '2'.
00262            88  CP-ISSUE                            VALUE '3'.
00263          16  CP-SPECIAL-CALC-CD    PIC X.
00264            88  CP-OUTSTANDING-BAL              VALUE 'O'.
00265            88  CP-1-MTH-INTEREST               VALUE ' '.
00266            88  CP-0-MTH-INTEREST               VALUE 'A'.
00267            88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
00268            88  CP-CRITICAL-PERIOD              VALUE 'C'.
00269            88  CP-TERM-IS-DAYS                 VALUE 'D'.
00270            88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
00271            88  CP-FARM-PLAN                    VALUE 'F'.
00272            88  CP-RATE-AS-STANDARD             VALUE 'G'.
00273            88  CP-2-MTH-INTEREST               VALUE 'I'.
00274            88  CP-3-MTH-INTEREST               VALUE 'J'.
00275            88  CP-4-MTH-INTEREST               VALUE 'K'.
00276            88  CP-BALLOON-LAST-PMT             VALUE 'L'.
00277            88  CP-MORTGAGE-REC                 VALUE 'M'.
00278            88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
00279            88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
00280            88  CP-NET-PAY-SIMPLE               VALUE 'S'.
00281            88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
00282                                                      'W' 'X'.
00283            88  CP-TRUNCATE-0-MTH               VALUE 'T'.
00284            88  CP-TRUNCATE-1-MTH               VALUE 'U'.
00285            88  CP-TRUNCATE-2-MTH               VALUE 'V'.
00286            88  CP-TRUNCATE-3-MTH               VALUE 'W'.
00287            88  CP-TRUNCATE-4-MTH               VALUE 'X'.
00288            88  CP-SUMMARY-REC                  VALUE 'Z'.
00289            88  CP-PROPERTY-BENEFIT             VALUE '2'.
00290            88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
00291            88  CP-AD-D-BENEFIT                 VALUE '4'.
00292            88  CP-CSL-METH-1                   VALUE '5'.
00293            88  CP-CSL-METH-2                   VALUE '6'.
00294            88  CP-CSL-METH-3                   VALUE '7'.
00295            88  CP-CSL-METH-4                   VALUE '8'.
00296
00297          16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
00298                                      COMP-3.
00299          16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
00300          16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
00301          16  CP-STATE              PIC XX          VALUE SPACE.
00302          16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
00303          16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
00304            88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
00305                '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
00306          16  CP-R78-OPTION         PIC X.
00307            88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
00308            88  CP-TERM-TIMES-TERM                  VALUE '1'.
00309
00310          16  CP-COMPANY-CD         PIC X             VALUE SPACE.
00311          16  CP-IBNR-RESERVE-SW    PIC X.
00312          16  CP-CLAIM-STATUS       PIC X.
00313          16  CP-RATE-FILE          PIC X.
00314          16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
00315                                      COMP-3.
00316
00317          16  CP-LIFE-OVERRIDE-CODE PIC X.
00318          16  CP-AH-OVERRIDE-CODE   PIC X.
00319
00320          16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
00321                                      COMP-3.
               16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
                                         PIC S9(5)V99 COMP-3.
00322          16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
00323                                      COMP-3.
00324          16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
00325                                      COMP-3.
00326          16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
00327                                      COMP-3.
               16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
                                        PIC S9(7)V99 COMP-3.
00328
00329          16  CP-PAID-FROM-DATE     PIC X(02).
00330          16  CP-CLAIM-CALC-METHOD  PIC X(01).
00331          16  CP-EXT-DAYS-CALC      PIC X.
00332            88  CP-EXT-NO-CHG                   VALUE ' '.
00333            88  CP-EXT-CHG-LF                   VALUE '1'.
00334            88  CP-EXT-CHG-AH                   VALUE '2'.
00335            88  CP-EXT-CHG-LF-AH                VALUE '3'.
00336          16  CP-DOMICILE-STATE     PIC XX.
00337          16  CP-CARRIER            PIC X.
00338          16  CP-REIN-FLAG          PIC X.
00339          16  CP-REM-TRM-CALC-OPTION PIC X.
00340            88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
00341                       '2' '3' '4' '5'.
00342            88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
00343            88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
00344            88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
00345            88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
00346            88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
00347            88  CP-EXT-30-DAY-MONTH          VALUE '3'.
00348            88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
                 88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
00349          16  CP-SIG-SWITCH         PIC X.
00350          16  CP-RATING-METHOD      PIC X.
00351            88  CP-RATE-AS-R78                      VALUE '1' 'R'.
00352            88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
00353            88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
00354            88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
00355            88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
00356            88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
00357            88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
00358            88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
00359            88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
00360          16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
00361                                      COMP-3.
090803         16  CP-BEN-CATEGORY       PIC X.
011904         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
                                         PIC S99V9(5) COMP-3.
011904         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
                                         PIC S99V9(5) COMP-3.
080305         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
               16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
041310         16  CP-EXPIRE-DT          PIC XX.
041710         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
               16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-SPEC-CALC      PIC X.
                   88  CP-CALC-GROSS-FEE        VALUE 'G'.
                   88  CP-CALC-CLP              VALUE 'C'.
               16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
               16  CP-CANCEL-REASON      PIC X.
               16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-PMT-MODE           PIC X.
               16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
071211         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
               16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
071211         16  FILLER                PIC X.
00363
00364 ***************    OUTPUT FROM ELRESV   ************************
00365
00366          16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
00367
00368          16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
00369                                      COMP-3.
101807         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  FILLER                PIC X(09).
00377 ***************    OUTPUT FROM ELRTRM   *************************
00378
00379          16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
00380                                      COMP-3.
00381          16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
00382                                      COMP-3.
00383          16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
00384                                      COMP-3.
00385          16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
00386                                      COMP-3.
00387          16  FILLER                PIC X(12).
00388
00389 ***************    OUTPUT FROM ELRAMT   *************************
00390
00391          16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
00392                                      COMP-3.
00393          16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
00394                                      COMP-3.
00395          16  FILLER                PIC X(12).
00396
00397 ***************    OUTPUT FROM ELRATE   *************************
00398
00399          16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
00400                                      COMP-3.
00401          16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
00402                                      COMP-3.
00403          16  CP-MORTALITY-CODE     PIC X(4).
00404          16  CP-RATE-EDIT-FLAG     PIC X.
00405              88  CP-RATES-NEED-APR                  VALUE '1'.
00406          16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
00407                                      COMP-3.
010716         16  CP-CANCEL-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
032905         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
               16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
                                         PIC S9(7)V99 COMP-3.
00409          16  FILLER                PIC X(07).
00410
00411 ***************    OUTPUT FROM ELRFND   *************************
00412
00413          16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
00414                                      COMP-3.
00415          16  CP-REFUND-TYPE-USED   PIC X.
00416            88  CP-R-AS-R78                         VALUE '1'.
00417            88  CP-R-AS-PRORATA                     VALUE '2'.
00418            88  CP-R-AS-CALIF                       VALUE '3'.
00419            88  CP-R-AS-TEXAS                       VALUE '4'.
00420            88  CP-R-AS-FARM-PLAN                   VALUE '4'.
00421            88  CP-R-AS-NET-PAY                     VALUE '5'.
00422            88  CP-R-AS-ANTICIPATION                VALUE '6'.
00423            88  CP-R-AS-MEAN                        VALUE '8'.
00424            88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
033104           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
033104           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
092310           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
092310           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
                 88  CP-R-AS-REPOSSESSION                VALUE 'R'.
00425          16  FILLER                PIC X(12).
00426
00427 ***************    OUTPUT FROM ELEARN   *************************
00428
00429          16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
00430                                      COMP-3.
00431          16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
00432                                      COMP-3.
00433          16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
00434                                      COMP-3.
00435          16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
00436                                      COMP-3.
00437          16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
00438                                      COMP-3.
00439          16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
00440                                      COMP-3.
00441          16  CP-EARNING-TYPE-USED  PIC X.
00442            88  CP-E-AS-SPECIAL                     VALUE 'S'.
00443            88  CP-E-AS-R78                         VALUE '1'.
00444            88  CP-E-AS-PRORATA                     VALUE '2'.
00445            88  CP-E-AS-TEXAS                       VALUE '4'.
00446            88  CP-E-AS-FARM-PLAN                   VALUE '4'.
00447            88  CP-E-AS-NET-PAY                     VALUE '5'.
00448            88  CP-E-AS-ANTICIPATION                VALUE '6'.
00449            88  CP-E-AS-MEAN                        VALUE '8'.
00450            88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
00451          16  FILLER                PIC X(12).
00452
00453 ***************    OUTPUT FROM ELPMNT   *************************
00454
00455          16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
00456                                      COMP-3.
00457          16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
00458                                      COMP-3.
00459          16  FILLER                PIC X(12).
00460
00461 ***************   MISC WORK AREAS    *****************************
00462          16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
00463                                      COMP-3.
00464          16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
00465                                      COMP-3.
00466          16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
00467                                      COMP-3.
00468          16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
00469                                      COMP-3.
00470          16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
00471                                      COMP-3.
00472          16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
00473                                      COMP-3.
00474          16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
00475              88  OPEN-RATE-FILE                   VALUE 'O'.
00476              88  CLOSE-RATE-FILE                  VALUE 'C'.
00477              88  IO-ERROR                         VALUE 'E'.
00478
00479          16  CP-FIRST-PAY-DATE     PIC XX.
00480
00481          16  CP-JOINT-INDICATOR    PIC X.
00482
00483          16  CP-RESERVE-REMAINING-TERM
00484                                    PIC S9(4)V9    VALUE ZERO
00485                                      COMP-3.
00486
00487          16  CP-INSURED-BIRTH-DT   PIC XX.
00488
00489          16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
00490                                      COMP-3.
00491
00492          16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
00493                                      COMP-3.
00494
00495          16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
00496                                      COMP-3.
00497
00498          16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
00499                                      COMP-3.
00500
00501          16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
00502                                      COMP-3.
00503
00504          16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
00505                                      COMP-3.
00506
00507          16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
00508                                      COMP-3.
00509
00510          16  CP-ROA-REFUND         PIC X          VALUE 'N'.
00511              88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
00512
010303         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
010303                                     COMP-3.
041710         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
041710                                     COMP-3.
               16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
040615         16  cp-extra-periods      pic 9 value zeros.
070115         16  cp-net-only-state     pic x value spaces.
041710         16  FILLER                PIC X(13).
00514 ******************************************************************
00307      EJECT
00308 *    COPY EL160S.
       01  EL160AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  CFILEIDL PIC S9(0004) COMP.
           05  CFILEIDF PIC  X(0001).
           05  FILLER REDEFINES CFILEIDF.
               10  CFILEIDA PIC  X(0001).
           05  CFILEIDI PIC  X(0001).
      *    -------------------------------
           05  CARRSL PIC S9(0004) COMP.
           05  CARRSF PIC  X(0001).
           05  FILLER REDEFINES CARRSF.
               10  CARRSA PIC  X(0001).
           05  CARRSI PIC  X(0001).
      *    -------------------------------
           05  INCLSL PIC S9(0004) COMP.
           05  INCLSF PIC  X(0001).
           05  FILLER REDEFINES INCLSF.
               10  INCLSA PIC  X(0001).
           05  INCLSI PIC  X(0008).
      *    -------------------------------
           05  INCHSL PIC S9(0004) COMP.
           05  INCHSF PIC  X(0001).
           05  FILLER REDEFINES INCHSF.
               10  INCHSA PIC  X(0001).
           05  INCHSI PIC  X(0008).
      *    -------------------------------
           05  GRPSL PIC S9(0004) COMP.
           05  GRPSF PIC  X(0001).
           05  FILLER REDEFINES GRPSF.
               10  GRPSA PIC  X(0001).
           05  GRPSI PIC  X(0006).
      *    -------------------------------
           05  PMTDLSL PIC S9(0004) COMP.
           05  PMTDLSF PIC  X(0001).
           05  FILLER REDEFINES PMTDLSF.
               10  PMTDLSA PIC  X(0001).
           05  PMTDLSI PIC  X(0008).
      *    -------------------------------
           05  PMTDHSL PIC S9(0004) COMP.
           05  PMTDHSF PIC  X(0001).
           05  FILLER REDEFINES PMTDHSF.
               10  PMTDHSA PIC  X(0001).
           05  PMTDHSI PIC  X(0008).
      *    -------------------------------
           05  STATESL PIC S9(0004) COMP.
           05  STATESF PIC  X(0001).
           05  FILLER REDEFINES STATESF.
               10  STATESA PIC  X(0001).
           05  STATESI PIC  X(0002).
      *    -------------------------------
           05  OPENLSL PIC S9(0004) COMP.
           05  OPENLSF PIC  X(0001).
           05  FILLER REDEFINES OPENLSF.
               10  OPENLSA PIC  X(0001).
           05  OPENLSI PIC  X(0003).
      *    -------------------------------
           05  OPENHSL PIC S9(0004) COMP.
           05  OPENHSF PIC  X(0001).
           05  FILLER REDEFINES OPENHSF.
               10  OPENHSA PIC  X(0001).
           05  OPENHSI PIC  X(0003).
      *    -------------------------------
           05  ACCTSL PIC S9(0004) COMP.
           05  ACCTSF PIC  X(0001).
           05  FILLER REDEFINES ACCTSF.
               10  ACCTSA PIC  X(0001).
           05  ACCTSI PIC  X(0010).
      *    -------------------------------
           05  AMTLSL PIC S9(0004) COMP.
           05  AMTLSF PIC  X(0001).
           05  FILLER REDEFINES AMTLSF.
               10  AMTLSA PIC  X(0001).
           05  AMTLSI PIC  X(0010).
      *    -------------------------------
           05  AMTHSL PIC S9(0004) COMP.
           05  AMTHSF PIC  X(0001).
           05  FILLER REDEFINES AMTHSF.
               10  AMTHSA PIC  X(0001).
           05  AMTHSI PIC  X(0010).
      *    -------------------------------
           05  TYPESL PIC S9(0004) COMP.
           05  TYPESF PIC  X(0001).
           05  FILLER REDEFINES TYPESF.
               10  TYPESA PIC  X(0001).
           05  TYPESI PIC  X(0001).
      *    -------------------------------
           05  CAUSELSL PIC S9(0004) COMP.
           05  CAUSELSF PIC  X(0001).
           05  FILLER REDEFINES CAUSELSF.
               10  CAUSELSA PIC  X(0001).
           05  CAUSELSI PIC  X(0006).
      *    -------------------------------
           05  CAUSEHSL PIC S9(0004) COMP.
           05  CAUSEHSF PIC  X(0001).
           05  FILLER REDEFINES CAUSEHSF.
               10  CAUSEHSA PIC  X(0001).
           05  CAUSEHSI PIC  X(0006).
      *    -------------------------------
           05  DENSL PIC S9(0004) COMP.
           05  DENSF PIC  X(0001).
           05  FILLER REDEFINES DENSF.
               10  DENSA PIC  X(0001).
           05  DENSI PIC  X(0001).
      *    -------------------------------
           05  REPLSL PIC S9(0004) COMP.
           05  REPLSF PIC  X(0001).
           05  FILLER REDEFINES REPLSF.
               10  REPLSA PIC  X(0001).
           05  REPLSI PIC  X(0008).
      *    -------------------------------
           05  REPHSL PIC S9(0004) COMP.
           05  REPHSF PIC  X(0001).
           05  FILLER REDEFINES REPHSF.
               10  REPHSA PIC  X(0001).
           05  REPHSI PIC  X(0008).
      *    -------------------------------
           05  PROCSL PIC S9(0004) COMP.
           05  PROCSF PIC  X(0001).
           05  FILLER REDEFINES PROCSF.
               10  PROCSA PIC  X(0001).
           05  PROCSI PIC  X(0004).
      *    -------------------------------
           05  PMTLSL PIC S9(0004) COMP.
           05  PMTLSF PIC  X(0001).
           05  FILLER REDEFINES PMTLSF.
               10  PMTLSA PIC  X(0001).
           05  PMTLSI PIC  X(0010).
      *    -------------------------------
           05  PMTHSL PIC S9(0004) COMP.
           05  PMTHSF PIC  X(0001).
           05  FILLER REDEFINES PMTHSF.
               10  PMTHSA PIC  X(0001).
           05  PMTHSI PIC  X(0010).
      *    -------------------------------
           05  PREMSL PIC S9(0004) COMP.
           05  PREMSF PIC  X(0001).
           05  FILLER REDEFINES PREMSF.
               10  PREMSA PIC  X(0001).
           05  PREMSI PIC  X(0001).
      *    -------------------------------
           05  MNTLSL PIC S9(0004) COMP.
           05  MNTLSF PIC  X(0001).
           05  FILLER REDEFINES MNTLSF.
               10  MNTLSA PIC  X(0001).
           05  MNTLSI PIC  X(0008).
      *    -------------------------------
           05  MNTHSL PIC S9(0004) COMP.
           05  MNTHSF PIC  X(0001).
           05  FILLER REDEFINES MNTHSF.
               10  MNTHSA PIC  X(0001).
           05  MNTHSI PIC  X(0008).
      *    -------------------------------
           05  REQSL PIC S9(0004) COMP.
           05  REQSF PIC  X(0001).
           05  FILLER REDEFINES REQSF.
               10  REQSA PIC  X(0001).
           05  REQSI PIC  X(0001).
      *    -------------------------------
           05  ESTLSL PIC S9(0004) COMP.
           05  ESTLSF PIC  X(0001).
           05  FILLER REDEFINES ESTLSF.
               10  ESTLSA PIC  X(0001).
           05  ESTLSI PIC  X(0008).
      *    -------------------------------
           05  ESTHSL PIC S9(0004) COMP.
           05  ESTHSF PIC  X(0001).
           05  FILLER REDEFINES ESTHSF.
               10  ESTHSA PIC  X(0001).
           05  ESTHSI PIC  X(0008).
      *    -------------------------------
           05  SUPRSL PIC S9(0004) COMP.
           05  SUPRSF PIC  X(0001).
           05  FILLER REDEFINES SUPRSF.
               10  SUPRSA PIC  X(0001).
           05  SUPRSI PIC  X(0001).
      *    -------------------------------
           05  FOLLSL PIC S9(0004) COMP.
           05  FOLLSF PIC  X(0001).
           05  FILLER REDEFINES FOLLSF.
               10  FOLLSA PIC  X(0001).
           05  FOLLSI PIC  X(0008).
      *    -------------------------------
           05  FOLHSL PIC S9(0004) COMP.
           05  FOLHSF PIC  X(0001).
           05  FILLER REDEFINES FOLHSF.
               10  FOLHSA PIC  X(0001).
           05  FOLHSI PIC  X(0008).
      *    -------------------------------
           05  CERTSL PIC S9(0004) COMP.
           05  CERTSF PIC  X(0001).
           05  FILLER REDEFINES CERTSF.
               10  CERTSA PIC  X(0001).
           05  CERTSI PIC  X(0001).
      *    -------------------------------
           05  DAYSLSL PIC S9(0004) COMP.
           05  DAYSLSF PIC  X(0001).
           05  FILLER REDEFINES DAYSLSF.
               10  DAYSLSA PIC  X(0001).
           05  DAYSLSI PIC  X(0003).
      *    -------------------------------
           05  DAYSHSL PIC S9(0004) COMP.
           05  DAYSHSF PIC  X(0001).
           05  FILLER REDEFINES DAYSHSF.
               10  DAYSHSA PIC  X(0001).
           05  DAYSHSI PIC  X(0003).
      *    -------------------------------
           05  PRISL PIC S9(0004) COMP.
           05  PRISF PIC  X(0001).
           05  FILLER REDEFINES PRISF.
               10  PRISA PIC  X(0001).
           05  PRISI PIC  X(0001).
      *    -------------------------------
           05  AUTOSL PIC S9(0004) COMP.
           05  AUTOSF PIC  X(0001).
           05  FILLER REDEFINES AUTOSF.
               10  AUTOSA PIC  X(0001).
           05  AUTOSI PIC  X(0001).
      *    -------------------------------
           05  PRTOPTL PIC S9(0004) COMP.
           05  PRTOPTF PIC  X(0001).
           05  FILLER REDEFINES PRTOPTF.
               10  PRTOPTA PIC  X(0001).
           05  PRTOPTI PIC  X(0001).
      *    -------------------------------
           05  OPCLSL PIC S9(0004) COMP.
           05  OPCLSF PIC  X(0001).
           05  FILLER REDEFINES OPCLSF.
               10  OPCLSA PIC  X(0001).
           05  OPCLSI PIC  X(0001).
      *    -------------------------------
           05  FMTOPTL PIC S9(0004) COMP.
           05  FMTOPTF PIC  X(0001).
           05  FILLER REDEFINES FMTOPTF.
               10  FMTOPTA PIC  X(0001).
           05  FMTOPTI PIC  X(0001).
      *    -------------------------------
           05  ASEXL PIC S9(0004) COMP.
           05  ASEXF PIC  X(0001).
           05  FILLER REDEFINES ASEXF.
               10  ASEXA PIC  X(0001).
           05  ASEXI PIC  X(0001).
      *    -------------------------------
           05  ALTPRTL PIC S9(0004) COMP.
           05  ALTPRTF PIC  X(0001).
           05  FILLER REDEFINES ALTPRTF.
               10  ALTPRTA PIC  X(0001).
           05  ALTPRTI PIC  X(0004).
      *    -------------------------------
           05  MSG1L PIC S9(0004) COMP.
           05  MSG1F PIC  X(0001).
           05  FILLER REDEFINES MSG1F.
               10  MSG1A PIC  X(0001).
           05  MSG1I PIC  X(0075).
      *    -------------------------------
           05  MSG2L PIC S9(0004) COMP.
           05  MSG2F PIC  X(0001).
           05  FILLER REDEFINES MSG2F.
               10  MSG2A PIC  X(0001).
           05  MSG2I PIC  X(0075).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  X(0002).
       01  EL160AO REDEFINES EL160AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CFILEIDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCLSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCHSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPSO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTDLSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTDHSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATESO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OPENLSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OPENHSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTSO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMTLSO PIC  Z(7).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMTHSO PIC  Z(7).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPESO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAUSELSO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAUSEHSO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DENSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPLSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPHSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROCSO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTLSO PIC  Z(7).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTHSO PIC  Z(7).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PREMSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTLSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTHSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REQSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ESTLSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ESTHSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUPRSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FOLLSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FOLHSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAYSLSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAYSHSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRISO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUTOSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRTOPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OPCLSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FMTOPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALTPRTO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSG1O PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSG2O PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
       01  EL160BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEBL PIC S9(0004) COMP.
           05  DATEBF PIC  X(0001).
           05  FILLER REDEFINES DATEBF.
               10  DATEBA PIC  X(0001).
           05  DATEBI PIC  X(0008).
      *    -------------------------------
           05  TIMEBL PIC S9(0004) COMP.
           05  TIMEBF PIC  X(0001).
           05  FILLER REDEFINES TIMEBF.
               10  TIMEBA PIC  X(0001).
           05  TIMEBI PIC  X(0005).
      *    -------------------------------
           05  TITLEL PIC S9(0004) COMP.
           05  TITLEF PIC  X(0001).
           05  FILLER REDEFINES TITLEF.
               10  TITLEA PIC  X(0001).
           05  TITLEI PIC  X(0028).
      *    -------------------------------
           05  PIKEYL PIC S9(0004) COMP.
           05  PIKEYF PIC  X(0001).
           05  FILLER REDEFINES PIKEYF.
               10  PIKEYA PIC  X(0001).
           05  PIKEYI PIC  X(0039).
      *    -------------------------------
           05  SCNERRL PIC S9(0004) COMP.
           05  SCNERRF PIC  X(0001).
           05  FILLER REDEFINES SCNERRF.
               10  SCNERRA PIC  X(0001).
           05  SCNERRI PIC  X(0004).
      *    -------------------------------
           05  USERSAVL PIC S9(0004) COMP.
           05  USERSAVF PIC  X(0001).
           05  FILLER REDEFINES USERSAVF.
               10  USERSAVA PIC  X(0001).
           05  USERSAVI PIC  X(0004).
      *    -------------------------------
           05  TIMESAVL PIC S9(0004) COMP.
           05  TIMESAVF PIC  X(0001).
           05  FILLER REDEFINES TIMESAVF.
               10  TIMESAVA PIC  X(0001).
           05  TIMESAVI PIC  9(07).
      *    -------------------------------
           05  NOSCRNL PIC S9(0004) COMP.
           05  NOSCRNF PIC  X(0001).
           05  FILLER REDEFINES NOSCRNF.
               10  NOSCRNA PIC  X(0001).
           05  NOSCRNI PIC  9999.
      *    -------------------------------
           05  TOTSCRNL PIC S9(0004) COMP.
           05  TOTSCRNF PIC  X(0001).
           05  FILLER REDEFINES TOTSCRNF.
               10  TOTSCRNA PIC  X(0001).
           05  TOTSCRNI PIC  X(0004).
      *    -------------------------------
           05  CLAIML PIC S9(0004) COMP.
           05  CLAIMF PIC  X(0001).
           05  FILLER REDEFINES CLAIMF.
               10  CLAIMA PIC  X(0001).
           05  CLAIMI PIC  X(0007).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0001).
      *    -------------------------------
           05  CERTL PIC S9(0004) COMP.
           05  CERTF PIC  X(0001).
           05  FILLER REDEFINES CERTF.
               10  CERTA PIC  X(0001).
           05  CERTI PIC  X(0010).
      *    -------------------------------
           05  CERTSXL PIC S9(0004) COMP.
           05  CERTSXF PIC  X(0001).
           05  FILLER REDEFINES CERTSXF.
               10  CERTSXA PIC  X(0001).
           05  CERTSXI PIC  X(0001).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  STATUSL PIC S9(0004) COMP.
           05  STATUSF PIC  X(0001).
           05  FILLER REDEFINES STATUSF.
               10  STATUSA PIC  X(0001).
           05  STATUSI PIC  X(0001).
      *    -------------------------------
           05  PROCL PIC S9(0004) COMP.
           05  PROCF PIC  X(0001).
           05  FILLER REDEFINES PROCF.
               10  PROCA PIC  X(0001).
           05  PROCI PIC  X(0004).
      *    -------------------------------
           05  FILEL PIC S9(0004) COMP.
           05  FILEF PIC  X(0001).
           05  FILLER REDEFINES FILEF.
               10  FILEA PIC  X(0001).
           05  FILEI PIC  X(0004).
      *    -------------------------------
           05  CREDCDL PIC S9(0004) COMP.
           05  CREDCDF PIC  X(0001).
           05  FILLER REDEFINES CREDCDF.
               10  CREDCDA PIC  X(0001).
           05  CREDCDI PIC  X(0016).
      *    -------------------------------
           05  MLNAMEL PIC S9(0004) COMP.
           05  MLNAMEF PIC  X(0001).
           05  FILLER REDEFINES MLNAMEF.
               10  MLNAMEA PIC  X(0001).
           05  MLNAMEI PIC  X(0015).
      *    -------------------------------
           05  MFNAMEL PIC S9(0004) COMP.
           05  MFNAMEF PIC  X(0001).
           05  FILLER REDEFINES MFNAMEF.
               10  MFNAMEA PIC  X(0001).
           05  MFNAMEI PIC  X(0015).
      *    -------------------------------
           05  MMINITL PIC S9(0004) COMP.
           05  MMINITF PIC  X(0001).
           05  FILLER REDEFINES MMINITF.
               10  MMINITA PIC  X(0001).
           05  MMINITI PIC  X(0001).
      *    -------------------------------
           05  SEXL PIC S9(0004) COMP.
           05  SEXF PIC  X(0001).
           05  FILLER REDEFINES SEXF.
               10  SEXA PIC  X(0001).
           05  SEXI PIC  X(0001).
      *    -------------------------------
           05  BIRTHL PIC S9(0004) COMP.
           05  BIRTHF PIC  X(0001).
           05  FILLER REDEFINES BIRTHF.
               10  BIRTHA PIC  X(0001).
           05  BIRTHI PIC  X(0008).
      *    -------------------------------
           05  SOCIALL PIC S9(0004) COMP.
           05  SOCIALF PIC  X(0001).
           05  FILLER REDEFINES SOCIALF.
               10  SOCIALA PIC  X(0001).
           05  SOCIALI PIC  X(0011).
      *    -------------------------------
           05  OCCL PIC S9(0004) COMP.
           05  OCCF PIC  X(0001).
           05  FILLER REDEFINES OCCF.
               10  OCCA PIC  X(0001).
           05  OCCI PIC  X(0006).
      *    -------------------------------
           05  CBENEL PIC S9(0004) COMP.
           05  CBENEF PIC  X(0001).
           05  FILLER REDEFINES CBENEF.
               10  CBENEA PIC  X(0001).
           05  CBENEI PIC  X(0010).
      *    -------------------------------
           05  BHEADL PIC S9(0004) COMP.
           05  BHEADF PIC  X(0001).
           05  FILLER REDEFINES BHEADF.
               10  BHEADA PIC  X(0001).
           05  BHEADI PIC  X(0029).
      *    -------------------------------
           05  CAUSEL PIC S9(0004) COMP.
           05  CAUSEF PIC  X(0001).
           05  FILLER REDEFINES CAUSEF.
               10  CAUSEA PIC  X(0001).
           05  CAUSEI PIC  X(0026).
      *    -------------------------------
           05  CCAUSCDL PIC S9(0004) COMP.
           05  CCAUSCDF PIC  X(0001).
           05  FILLER REDEFINES CCAUSCDF.
               10  CCAUSCDA PIC  X(0001).
           05  CCAUSCDI PIC  X(0006).
      *    -------------------------------
           05  ENDL PIC S9(0004) COMP.
           05  ENDF PIC  X(0001).
           05  FILLER REDEFINES ENDF.
               10  ENDA PIC  X(0001).
           05  ENDI PIC  X(0008).
      *    -------------------------------
           05  PDTHRUL PIC S9(0004) COMP.
           05  PDTHRUF PIC  X(0001).
           05  FILLER REDEFINES PDTHRUF.
               10  PDTHRUA PIC  X(0001).
           05  PDTHRUI PIC  X(0008).
      *    -------------------------------
           05  PDAMTL PIC S9(0004) COMP.
           05  PDAMTF PIC  X(0001).
           05  FILLER REDEFINES PDAMTF.
               10  PDAMTA PIC  X(0001).
           05  PDAMTI PIC  9(7)V99.
      *    -------------------------------
           05  NODAYSL PIC S9(0004) COMP.
           05  NODAYSF PIC  X(0001).
           05  FILLER REDEFINES NODAYSF.
               10  NODAYSA PIC  X(0001).
           05  NODAYSI PIC  9(5).
      *    -------------------------------
           05  NOPMTSL PIC S9(0004) COMP.
           05  NOPMTSF PIC  X(0001).
           05  FILLER REDEFINES NOPMTSF.
               10  NOPMTSA PIC  X(0001).
           05  NOPMTSI PIC  9(4).
      *    -------------------------------
           05  INCL PIC S9(0004) COMP.
           05  INCF PIC  X(0001).
           05  FILLER REDEFINES INCF.
               10  INCA PIC  X(0001).
           05  INCI PIC  X(0008).
      *    -------------------------------
           05  REPL PIC S9(0004) COMP.
           05  REPF PIC  X(0001).
           05  FILLER REDEFINES REPF.
               10  REPA PIC  X(0001).
           05  REPI PIC  X(0008).
      *    -------------------------------
           05  ESTL PIC S9(0004) COMP.
           05  ESTF PIC  X(0001).
           05  FILLER REDEFINES ESTF.
               10  ESTA PIC  X(0001).
           05  ESTI PIC  X(0008).
      *    -------------------------------
           05  MNTDTL PIC S9(0004) COMP.
           05  MNTDTF PIC  X(0001).
           05  FILLER REDEFINES MNTDTF.
               10  MNTDTA PIC  X(0001).
           05  MNTDTI PIC  X(0008).
      *    -------------------------------
           05  MNTTYPEL PIC S9(0004) COMP.
           05  MNTTYPEF PIC  X(0001).
           05  FILLER REDEFINES MNTTYPEF.
               10  MNTTYPEA PIC  X(0001).
           05  MNTTYPEI PIC  X(0006).
      *    -------------------------------
           05  PRICDL PIC S9(0004) COMP.
           05  PRICDF PIC  X(0001).
           05  FILLER REDEFINES PRICDF.
               10  PRICDA PIC  X(0001).
           05  PRICDI PIC  X(0001).
      *    -------------------------------
           05  SUPVL PIC S9(0004) COMP.
           05  SUPVF PIC  X(0001).
           05  FILLER REDEFINES SUPVF.
               10  SUPVA PIC  X(0001).
           05  SUPVI PIC  X(0001).
      *    -------------------------------
           05  LOANNOL PIC S9(0004) COMP.
           05  LOANNOF PIC  X(0001).
           05  FILLER REDEFINES LOANNOF.
               10  LOANNOA PIC  X(0001).
           05  LOANNOI PIC  X(0008).
      *    -------------------------------
           05  LOANBALL PIC S9(0004) COMP.
           05  LOANBALF PIC  X(0001).
           05  FILLER REDEFINES LOANBALF.
               10  LOANBALA PIC  X(0001).
           05  LOANBALI PIC  9(10)V99.
      *    -------------------------------
           05  CERTEFFL PIC S9(0004) COMP.
           05  CERTEFFF PIC  X(0001).
           05  FILLER REDEFINES CERTEFFF.
               10  CERTEFFA PIC  X(0001).
           05  CERTEFFI PIC  X(0008).
      *    -------------------------------
           05  CERTACTL PIC S9(0004) COMP.
           05  CERTACTF PIC  X(0001).
           05  FILLER REDEFINES CERTACTF.
               10  CERTACTA PIC  X(0001).
           05  CERTACTI PIC  X(0010).
      *    -------------------------------
           05  CERTSTL PIC S9(0004) COMP.
           05  CERTSTF PIC  X(0001).
           05  FILLER REDEFINES CERTSTF.
               10  CERTSTA PIC  X(0001).
           05  CERTSTI PIC  X(0002).
      *    -------------------------------
           05  CERTCARL PIC S9(0004) COMP.
           05  CERTCARF PIC  X(0001).
           05  FILLER REDEFINES CERTCARF.
               10  CERTCARA PIC  X(0001).
           05  CERTCARI PIC  X(0001).
      *    -------------------------------
           05  CERTGRPL PIC S9(0004) COMP.
           05  CERTGRPF PIC  X(0001).
           05  FILLER REDEFINES CERTGRPF.
               10  CERTGRPA PIC  X(0001).
           05  CERTGRPI PIC  X(0006).
      *    -------------------------------
           05  SOCSECL PIC S9(0004) COMP.
           05  SOCSECF PIC  X(0001).
           05  FILLER REDEFINES SOCSECF.
               10  SOCSECA PIC  X(0001).
           05  SOCSECI PIC  X(0011).
      *    -------------------------------
           05  CLNAMEL PIC S9(0004) COMP.
           05  CLNAMEF PIC  X(0001).
           05  FILLER REDEFINES CLNAMEF.
               10  CLNAMEA PIC  X(0001).
           05  CLNAMEI PIC  X(0015).
      *    -------------------------------
           05  CFNAMEL PIC S9(0004) COMP.
           05  CFNAMEF PIC  X(0001).
           05  FILLER REDEFINES CFNAMEF.
               10  CFNAMEA PIC  X(0001).
           05  CFNAMEI PIC  X(0010).
      *    -------------------------------
           05  CINITL PIC S9(0004) COMP.
           05  CINITF PIC  X(0001).
           05  FILLER REDEFINES CINITF.
               10  CINITA PIC  X(0001).
           05  CINITI PIC  X(0001).
      *    -------------------------------
           05  INSAGEL PIC S9(0004) COMP.
           05  INSAGEF PIC  X(0001).
           05  FILLER REDEFINES INSAGEF.
               10  INSAGEA PIC  X(0001).
           05  INSAGEI PIC  X(0002).
      *    -------------------------------
           05  CJLNAMEL PIC S9(0004) COMP.
           05  CJLNAMEF PIC  X(0001).
           05  FILLER REDEFINES CJLNAMEF.
               10  CJLNAMEA PIC  X(0001).
           05  CJLNAMEI PIC  X(0015).
      *    -------------------------------
           05  CJFAMEL PIC S9(0004) COMP.
           05  CJFAMEF PIC  X(0001).
           05  FILLER REDEFINES CJFAMEF.
               10  CJFAMEA PIC  X(0001).
           05  CJFAMEI PIC  X(0010).
      *    -------------------------------
           05  CJINITL PIC S9(0004) COMP.
           05  CJINITF PIC  X(0001).
           05  FILLER REDEFINES CJINITF.
               10  CJINITA PIC  X(0001).
           05  CJINITI PIC  X(0001).
      *    -------------------------------
           05  JAGEL PIC S9(0004) COMP.
           05  JAGEF PIC  X(0001).
           05  FILLER REDEFINES JAGEF.
               10  JAGEA PIC  X(0001).
           05  JAGEI PIC  X(0002).
      *    -------------------------------
           05  CVDESCRL PIC S9(0004) COMP.
           05  CVDESCRF PIC  X(0001).
           05  FILLER REDEFINES CVDESCRF.
               10  CVDESCRA PIC  X(0001).
           05  CVDESCRI PIC  X(0006).
      *    -------------------------------
           05  CVKINDL PIC S9(0004) COMP.
           05  CVKINDF PIC  X(0001).
           05  FILLER REDEFINES CVKINDF.
               10  CVKINDA PIC  X(0001).
           05  CVKINDI PIC  X(0003).
      *    -------------------------------
           05  CVCDL PIC S9(0004) COMP.
           05  CVCDF PIC  X(0001).
           05  FILLER REDEFINES CVCDF.
               10  CVCDA PIC  X(0001).
           05  CVCDI PIC  X(0002).
      *    -------------------------------
           05  CVOTRML PIC S9(0004) COMP.
           05  CVOTRMF PIC  X(0001).
           05  FILLER REDEFINES CVOTRMF.
               10  CVOTRMA PIC  X(0001).
           05  CVOTRMI PIC  X(0003).
      *    -------------------------------
           05  CVRTRML PIC S9(0004) COMP.
           05  CVRTRMF PIC  X(0001).
           05  FILLER REDEFINES CVRTRMF.
               10  CVRTRMA PIC  X(0001).
           05  CVRTRMI PIC  X(0003).
      *    -------------------------------
           05  CVOBENEL PIC S9(0004) COMP.
           05  CVOBENEF PIC  X(0001).
           05  FILLER REDEFINES CVOBENEF.
               10  CVOBENEA PIC  X(0001).
           05  CVOBENEI PIC  9(9)V99.
      *    -------------------------------
           05  CVFORML PIC S9(0004) COMP.
           05  CVFORMF PIC  X(0001).
           05  FILLER REDEFINES CVFORMF.
               10  CVFORMA PIC  X(0001).
           05  CVFORMI PIC  X(0012).
      *    -------------------------------
           05  CVCNCDTL PIC S9(0004) COMP.
           05  CVCNCDTF PIC  X(0001).
           05  FILLER REDEFINES CVCNCDTF.
               10  CVCNCDTA PIC  X(0001).
           05  CVCNCDTI PIC  X(0008).
      *    -------------------------------
           05  CVEXITL PIC S9(0004) COMP.
           05  CVEXITF PIC  X(0001).
           05  FILLER REDEFINES CVEXITF.
               10  CVEXITA PIC  X(0001).
           05  CVEXITI PIC  X(0008).
      *    -------------------------------
           05  CVSTATL PIC S9(0004) COMP.
           05  CVSTATF PIC  X(0001).
           05  FILLER REDEFINES CVSTATF.
               10  CVSTATA PIC  X(0001).
           05  CVSTATI PIC  X(0006).
      *    -------------------------------
           05  CMEMCAPL PIC S9(0004) COMP.
           05  CMEMCAPF PIC  X(0001).
           05  FILLER REDEFINES CMEMCAPF.
               10  CMEMCAPA PIC  X(0001).
           05  CMEMCAPI PIC  X(0010).
      *    -------------------------------
           05  CAPRL PIC S9(0004) COMP.
           05  CAPRF PIC  X(0001).
           05  FILLER REDEFINES CAPRF.
               10  CAPRA PIC  X(0001).
           05  CAPRI PIC  9(4)V9(4).
      *    -------------------------------
           05  CPFREQL PIC S9(0004) COMP.
           05  CPFREQF PIC  X(0001).
           05  FILLER REDEFINES CPFREQF.
               10  CPFREQA PIC  X(0001).
           05  CPFREQI PIC  99.
      *    -------------------------------
           05  CINDGRPL PIC S9(0004) COMP.
           05  CINDGRPF PIC  X(0001).
           05  FILLER REDEFINES CINDGRPF.
               10  CINDGRPA PIC  X(0001).
           05  CINDGRPI PIC  X(0001).
      *    -------------------------------
           05  CPREMTPL PIC S9(0004) COMP.
           05  CPREMTPF PIC  X(0001).
           05  FILLER REDEFINES CPREMTPF.
               10  CPREMTPA PIC  X(0001).
           05  CPREMTPI PIC  X(0002).
      *    -------------------------------
           05  CREINCDL PIC S9(0004) COMP.
           05  CREINCDF PIC  X(0001).
           05  FILLER REDEFINES CREINCDF.
               10  CREINCDA PIC  X(0001).
           05  CREINCDI PIC  X(0003).
      *    -------------------------------
           05  CMEMBERL PIC S9(0004) COMP.
           05  CMEMBERF PIC  X(0001).
           05  FILLER REDEFINES CMEMBERF.
               10  CMEMBERA PIC  X(0001).
           05  CMEMBERI PIC  X(0012).
      *    -------------------------------
           05  MSGBL PIC S9(0004) COMP.
           05  MSGBF PIC  X(0001).
           05  FILLER REDEFINES MSGBF.
               10  MSGBA PIC  X(0001).
           05  MSGBI PIC  X(0075).
      *    -------------------------------
           05  PFKEYBL PIC S9(0004) COMP.
           05  PFKEYBF PIC  X(0001).
           05  FILLER REDEFINES PFKEYBF.
               10  PFKEYBA PIC  X(0001).
           05  PFKEYBI PIC  X(0002).
       01  EL160BO REDEFINES EL160BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEBO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEBO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TITLEO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PIKEYO PIC  X(0039).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SCNERRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERSAVO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMESAVO PIC  9(07).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOSCRNO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTSCRNO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTSXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROCO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FILEO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREDCDO PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MFNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MMINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIRTHO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SOCIALO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OCCO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBENEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BHEADO PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAUSEO PIC  X(0026).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAUSCDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDTHRUO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDAMTO PIC  Z(06).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NODAYSO PIC  ZZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOPMTSO PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ESTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTTYPEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRICDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUPVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANNOO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANBALO PIC  Z,ZZZ,Z99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTEFFO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTACTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTCARO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTGRPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SOCSECO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CFNAMEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INSAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJFAMEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CJINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVDESCRO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVKINDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVCDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVOTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVRTRMO PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVOBENEO PIC  ZZZZZZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVFORMO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVCNCDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVEXITO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CVSTATO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMEMCAPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAPRO PIC  9(3).9(4).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPFREQO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CINDGRPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPREMTPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREINCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMEMBERO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSGBO PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYBO PIC  X(0002).
      *    -------------------------------
00309      EJECT
00310 *    COPY ELCMSTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCMSTR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
00013 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
00014 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00015 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
00016 *       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
00017 *                                                 RKP=75,LEN=21  *
00018 *                                                                *
00019 *   **** NOTE ****                                               *
00020 *             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
00021 *             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
080307* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
031213* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
00025 ******************************************************************
00026  01  CLAIM-MASTER.
00027      12  CL-RECORD-ID                PIC XX.
00028          88  VALID-CL-ID         VALUE 'CL'.
00029
00030      12  CL-CONTROL-PRIMARY.
00031          16  CL-COMPANY-CD           PIC X.
00032          16  CL-CARRIER              PIC X.
00033          16  CL-CLAIM-NO             PIC X(7).
00034          16  CL-CERT-NO.
00035              20  CL-CERT-PRIME       PIC X(10).
00036              20  CL-CERT-SFX         PIC X.
00037
00038      12  CL-CONTROL-BY-NAME.
00039          16  CL-COMPANY-CD-A1        PIC X.
00040          16  CL-INSURED-LAST-NAME    PIC X(15).
00041          16  CL-INSURED-NAME.
00042              20  CL-INSURED-1ST-NAME PIC X(12).
00043              20  CL-INSURED-MID-INIT PIC X.
00044
00045      12  CL-CONTROL-BY-SSN.
00046          16  CL-COMPANY-CD-A2        PIC X.
00047          16  CL-SOC-SEC-NO.
00048              20  CL-SSN-STATE        PIC XX.
00049              20  CL-SSN-ACCOUNT      PIC X(6).
00050              20  CL-SSN-LN3          PIC X(3).
00051
00052      12  CL-CONTROL-BY-CERT-NO.
00053          16  CL-COMPANY-CD-A4        PIC X.
00054          16  CL-CERT-NO-A4.
00055              20  CL-CERT-A4-PRIME    PIC X(10).
00056              20  CL-CERT-A4-SFX      PIC X.
00057
00058      12  CL-CONTROL-BY-CCN.
00059          16  CL-COMPANY-CD-A5        PIC X.
00060          16  CL-CCN-A5.
00061              20  CL-CCN.
00062                  24  CL-CCN-PREFIX-A5 PIC X(4).
00063                  24  CL-CCN-PRIME-A5 PIC X(12).
00064              20  CL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  CL-INSURED-PROFILE-DATA.
00067          16  CL-INSURED-BIRTH-DT     PIC XX.
00068          16  CL-INSURED-SEX-CD       PIC X.
00069              88  INSURED-IS-MALE        VALUE 'M'.
00070              88  INSURED-IS-FEMALE      VALUE 'F'.
00071              88  INSURED-SEX-UNKNOWN    VALUE ' '.
00072          16  CL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  CL-PROCESSING-INFO.
00076          16  CL-PROCESSOR-ID         PIC X(4).
00077          16  CL-CLAIM-STATUS         PIC X.
00078              88  CLAIM-IS-OPEN          VALUE 'O'.
00079              88  CLAIM-IS-CLOSED        VALUE 'C'.
00080          16  CL-CLAIM-TYPE           PIC X.
00081 *            88  AH-CLAIM               VALUE 'A'.
00082 *            88  LIFE-CLAIM             VALUE 'L'.
00083 *            88  PROPERTY-CLAIM         VALUE 'P'.
00084 *            88  IUI-CLAIM              VALUE 'I'.
120503*            88  GAP-CLAIM              VALUE 'G'.
052614*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
100518*            88  OTHER-CLAIM            VALUE 'O'.
022122*            88  hospital-claim         value 'H'.
022122*            88  bereavement-claim      value 'B'.
00085          16  CL-CLAIM-PREM-TYPE      PIC X.
00086              88  SINGLE-PREMIUM         VALUE '1'.
00087              88  O-B-COVERAGE           VALUE '2'.
00088              88  OPEN-END-COVERAGE      VALUE '3'.
00089          16  CL-INCURRED-DT          PIC XX.
00090          16  CL-REPORTED-DT          PIC XX.
00091          16  CL-FILE-ESTABLISH-DT    PIC XX.
00092          16  CL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  CL-LAST-PMT-DT          PIC XX.
00094          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  CL-PAID-THRU-DT         PIC XX.
00096          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  CL-PMT-CALC-METHOD      PIC X.
00100              88  CL-360-DAY-YR          VALUE '1'.
00101              88  CL-365-DAY-YR          VALUE '2'.
00102              88  CL-FULL-MONTHS         VALUE '3'.
00103          16  CL-CAUSE-CD             PIC X(6).
00104
00105          16  CL-PRIME-CERT-NO.
00106              20  CL-PRIME-CERT-PRIME PIC X(10).
00107              20  CL-PRIME-CERT-SFX   PIC X.
00108
00109          16  CL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  CL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  CL-MICROFILM-NO         PIC X(10).
051414         16  FILLER REDEFINES CL-MICROFILM-NO.
051414             20  CL-BENEFIT-PERIOD   PIC 99.
051414             20  FILLER              PIC X(8).
00114          16  CL-PROG-FORM-TYPE       PIC X.
00115          16  CL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  CL-LAST-REOPEN-DT       PIC XX.
00118          16  CL-LAST-CLOSE-DT        PIC XX.
00119          16  CL-LAST-CLOSE-REASON    PIC X(01).
00120              88  FINAL-PAID             VALUE '1'.
00121              88  CLAIM-DENIED           VALUE '2'.
00122              88  AUTO-CLOSE             VALUE '3'.
00123              88  MANUAL-CLOSE           VALUE '4'.
00124              88  BENEFITS-CHANGED       VALUE 'C'.
00125              88  SETUP-ERRORS           VALUE 'E'.
00126          16  CL-ASSOC-CERT-SEQU      PIC S99.
00127          16  CL-ASSOC-CERT-TOTAL     PIC S99.
00128          16  CL-CLAIM-PAYMENT-STATUS PIC 9.
00129              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
080307         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
080307         16  FILLER                  PIC X.
00131
00132      12  CL-CERTIFICATE-DATA.
00133          16  CL-CERT-ORIGIN          PIC X.
00134              88  CERT-WAS-ONLINE        VALUE '1'.
00135              88  CERT-WAS-CREATED       VALUE '2'.
00136              88  COVERAGE-WAS-ADDED     VALUE '3'.
00137          16  CL-CERT-KEY-DATA.
00138              20  CL-CERT-CARRIER     PIC X.
00139              20  CL-CERT-GROUPING    PIC X(6).
00140              20  CL-CERT-STATE       PIC XX.
00141              20  CL-CERT-ACCOUNT.
00142                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
00143                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
00144              20  CL-CERT-EFF-DT      PIC XX.
00145
00146      12  CL-STATUS-CONTROLS.
00147          16  CL-PRIORITY-CD          PIC X.
00148              88  CONFIDENTIAL-DATA      VALUE '8'.
00149              88  HIGHEST-PRIORITY       VALUE '9'.
00150          16  CL-SUPV-ATTN-CD         PIC X.
00151              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
00152              88  SUPV-IS-REQUIRED       VALUE 'Y'.
00153          16  CL-PURGED-DT            PIC XX.
00154          16  CL-RESTORED-DT          PIC XX.
00155          16  CL-NEXT-AUTO-PAY-DT     PIC XX.
00156          16  CL-NEXT-RESEND-DT       PIC XX.
00157          16  CL-NEXT-FOLLOWUP-DT     PIC XX.
031213         16  CL-CRITICAL-PERIOD      PIC 99.
031213*        16  FILLER                  PIC XX.
00159          16  CL-LAST-MAINT-DT        PIC XX.
00160          16  CL-LAST-MAINT-USER      PIC X(4).
00161          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00162          16  CL-LAST-MAINT-TYPE      PIC X.
00163              88  CLAIM-SET-UP           VALUE ' '.
00164              88  PAYMENT-MADE           VALUE '1'.
00165              88  LETTER-SENT            VALUE '2'.
00166              88  MASTER-WAS-ALTERED     VALUE '3'.
00167              88  MASTER-WAS-RESTORED    VALUE '4'.
00168              88  INCURRED-DATE-CHANGED  VALUE '5'.
00169              88  FILE-CONVERTED         VALUE '6'.
00170              88  CHANGE-OF-BENEFITS     VALUE 'C'.
00171              88  ERROR-CORRECTION       VALUE 'E'.
00172          16  CL-RELATED-CLAIM-NO     PIC X(7).
00173          16  CL-HISTORY-ARCHIVE-DT   PIC XX.
00174          16  CL-BENEFICIARY          PIC X(10).
00175          16  CL-FILE-ESTABLISHED-BY  PIC X(4).
120808         16  CL-DENIAL-TYPE          PIC X.
                   88  CL-TYPE-DENIAL          VALUE '1'.
                   88  CL-TYPE-RESCISSION      VALUE '2'.
                   88  CL-TYPE-REFORMATION     VALUE '3'.
                   88  CL-TYPE-REF-TO-RES      VALUE '4'.
                   88  CL-TYPE-RECONSIDERED    VALUE '5'.
081817         16  CL-NO-OF-EXTENSIONS     PIC 99.
081817         16  filler                  pic x(3).
      *        16  CL-CRIT-PER-RECURRENT   PIC X.
      *        16  CL-CRIT-PER-RTW-MOS     PIC 99.
      *        16  CL-RTW-DT               PIC XX.
00177
00178      12  CL-TRAILER-CONTROLS.
00179          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00180              88  CL-1ST-TRL-AVAIL       VALUE +4095.
00181              88  CL-LAST-TRL-AVAIL      VALUE +100.
00182              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
00183          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00184          16  FILLER                  PIC XX.
00185          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00186          16  CL-ADDRESS-TRAILER-CNT.
00187              20  CL-INSURED-ADDR-CNT  PIC S9(1).
00188                  88  NO-INSURED-AVAILABLE    VALUE ZERO.
00189              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
00190                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.
00191              20  CL-BENIF-ADDR-CNT    PIC S9(1).
00192                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
00193              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
00194                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
00195              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
00196                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
00197              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
00198                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
00199              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
00200                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
00201
00202      12  CL-CV-REFERENCE-NO.
00203          16  CL-CV-REFNO-PRIME       PIC X(18).
00204          16  CL-CV-REFNO-SFX         PIC XX.
00205
00206      12  CL-FILE-LOCATION            PIC X(4).
00207
00208      12  CL-PROCESS-ERRORS.
00209          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00210              88  NO-FATAL-ERRORS        VALUE ZERO.
00211          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00212              88  NO-FORCABLE-ERRORS     VALUE ZERO.
00213
00214      12  CL-PRODUCT-CD               PIC X.
00215
00216      12  CL-CURRENT-KEY-DATA.
00217          16  CL-CURRENT-CARRIER      PIC X.
00218          16  CL-CURRENT-GROUPING     PIC X(6).
00219          16  CL-CURRENT-STATE        PIC XX.
00220          16  CL-CURRENT-ACCOUNT      PIC X(10).
00221
00222      12  CL-ASSOCIATES               PIC X.
00223          88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
00224          88  CL-ASSOC-INTERFACE         VALUE 'I'.
00225          88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00226          88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
00227
00228      12  CL-ACTIVITY-CODE            PIC 99.
00229      12  CL-ACTIVITY-MAINT-DT        PIC XX.
00230      12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
00231
00232      12  CL-LAPSE-REPORT-CODE        PIC 9.
00233      12  CL-LAG-REPORT-CODE          PIC 9.
00234      12  CL-LOAN-TYPE                PIC XX.
00235      12  CL-LEGAL-STATE              PIC XX.
00236
CIDMOD     12  CL-YESNOSW                  PIC X.
031213     12  CL-ACCIDENT-CLAIM-SW        PIC X.
031213         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
031213         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
031213         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
051414     12  cl-insured-type             pic x.
051414         88  cl-claim-on-primary         value 'P'.
051414         88  cl-claim-on-co-borrower     value 'C'.
031213     12  cl-benefit-expiration-dt    PIC XX.
00311      EJECT
00312 *    COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
00313      EJECT
00314 *    COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
00315
00316 *    COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00317
00318  01  FILLER REDEFINES DFHAID.
00319      12  FILLER                  PIC X(8).
00320      12  AID-KEYS OCCURS 24 TIMES.
00321          16  FILLER              PIC X.
00322
00323 *    COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00324
00325      EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00327
00328  01  DFHCOMMAREA                 PIC X(1024).
00329
00330  01  CLAIM-MASTER-L              PIC X(350).
00331
00332      EJECT
00333 *    COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
010716* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
090314         16  cm-temp-epiq                  pic xx.
090314             88  EPIQ-CLASS                  value 'EQ'.
090314*        16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
010716         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
011410         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
011410                                           PIC S9(5)V99  COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
062017         16  CM-REF-INTERFACE-SW           PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
062017     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
032612     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
032612     12  FILLER REDEFINES CM-USER-RESERVED.
032612         16  CM-AH-CLASS-CD                PIC XX.
032612         16  F                             PIC X.
00286 ******************************************************************
00334      EJECT
00335 *    COPY ELCTRLR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCTRLR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.014                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY TRAILER FILE                     *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 200    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELTRLR             RKP=2,LEN=22          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
050506* 050506    2006030600001  AJRA  ADD DENIAL PROOF DATE
062806* 062806    2006030600001  AJRA  ADD PAYMENT PROOF DATE
080106* 080106    2006052500001  AJRA  ADD N AND R NOTE TYPES
041807* 041807    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
082807* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
101807* 101807  IR2007100100007  PEMA  EXPAND SIZE OF CLM RESERVE FLDS
070909* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
040110* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
071910* 071910  CR2009122800001  PEMA  ADD EOB SWITCHES
102610* 102610    2009122800001  AJRA  ADD STOP DATE TO LETTER
061511* 061511    2011042000002  AJRA  ADD VFY 2ND BENE TO ADDRESS TRAIL
020413* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM I
021213* 021213    2012092400007  AJRA  CAUSAL STATE SEQUENCE NO
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
102413* 102413  CR2013100800001  AJRA  ADD SPECIAL RELEASE IND
022614* 022614    2013050100003  AJRA  ADD CERT CANCELLED NOTE TYPE - T
040814* 040814    2014030500002  AJRA  ADD ICD CODES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
062217* 062217  CR2017050300002  TANA  ADD AUTH RCVD
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
102418* 102418  CR2018083000001  TANA  ADD ADD NEW CALL TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
00017 ******************************************************************
00018  01  ACTIVITY-TRAILERS.
00019      12  AT-RECORD-ID                    PIC XX.
00020          88  VALID-AT-ID                       VALUE 'AT'.
00021
00022      12  AT-CONTROL-PRIMARY.
00023          16  AT-COMPANY-CD               PIC X.
00024          16  AT-CARRIER                  PIC X.
00025          16  AT-CLAIM-NO                 PIC X(7).
00026          16  AT-CERT-NO.
00027              20  AT-CERT-PRIME           PIC X(10).
00028              20  AT-CERT-SFX             PIC X.
00029          16  AT-SEQUENCE-NO              PIC S9(4)     COMP.
00030              88  AT-1ST-TRL-AVAIL             VALUE +4095.
00031              88  AT-LAST-TRL-AVAIL            VALUE +100.
00032              88  AT-RESV-EXP-HIST-TRL         VALUE +0.
00033              88  AT-INSURED-ADDR-TRL          VALUE +1 THRU +9.
00034              88  AT-BENEFICIARY-ADDR-TRL      VALUE +11 THRU +19.
00035              88  AT-ACCOUNT-ADDR-TRL          VALUE +21 THRU +29.
00036              88  AT-PHYSICIAN-ADDR-TRL        VALUE +31 THRU +39.
00037              88  AT-EMPLOYERS-ADDR-TRL        VALUE +41 THRU +49.
00038              88  AT-OTHER-1-ADDR-TRL          VALUE +51 THRU +59.
00039              88  AT-OTHER-2-ADDR-TRL          VALUE +61 THRU +69.
00040              88  AT-DIAGNOSIS-TRL             VALUE +90.
022106             88  AT-BENEFICIARY-TRL           VALUE +91.
022106             88  AT-SPECIAL-REVIEW-TRL        VALUE +92.
061511             88  AT-VFY-2ND-BENE-NOTE-TRL     VALUE +93.
021213             88  AT-VFY-CAUSAL-STATE          VALUE +94.
                   88  AT-ERROR-MSGS-TRL            VALUE +95.
00041
00042      12  AT-TRAILER-TYPE                 PIC X.
00043          88  RESERVE-EXPENSE-TR               VALUE '1'.
00044          88  PAYMENT-TR                       VALUE '2'.
00045          88  AUTO-PAY-TR                      VALUE '3'.
00046          88  CORRESPONDENCE-TR                VALUE '4'.
00047          88  ADDRESS-TR                       VALUE '5'.
00048          88  GENERAL-INFO-TR                  VALUE '6'.
00049          88  AUTO-PROMPT-TR                   VALUE '7'.
00050          88  DENIAL-TR                        VALUE '8'.
00051          88  INCURRED-CHG-TR                  VALUE '9'.
00052          88  FORM-CONTROL-TR                  VALUE 'A'.
00053
00054      12  AT-RECORDED-DT                  PIC XX.
00055      12  AT-RECORDED-BY                  PIC X(4).
00056      12  AT-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.
00057
00058      12  AT-TRAILER-BODY                 PIC X(165).
00059
00060      12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.
00061          16  AT-RESERVE-CONTROLS.
00062              20  AT-MANUAL-SW            PIC X.
00063                  88  AT-MANUAL-RESERVES-USED VALUE '1'.
00064              20  AT-FUTURE-SW            PIC X.
00065                  88  AT-FUTURE-RESERVES-USED VALUE '1'.
00066              20  AT-PTC-SW               PIC X.
00067                  88  AT-PAY-TO-CURRENT-USED  VALUE '1'.
00068              20  AT-IBNR-SW              PIC X.
00069                  88  AT-IBNR-RESERVES-USED   VALUE '1'.
00070              20  AT-PTC-LF-SW            PIC X.
00071                  88  AT-LF-PTC-USED          VALUE '1'.
00072              20  AT-CDT-ACCESS-METHOD    PIC X.
00073                  88  AT-CDT-ROUND-NEAR       VALUE '1'.
00074                  88  AT-CDT-ROUND-HIGH       VALUE '2'.
00075                  88  AT-CDT-INTERPOLATED     VALUE '3'.
00076              20  AT-PERCENT-OF-CDT       PIC S9(3)V99    COMP-3.
00077          16  AT-LAST-COMPUTED-DT         PIC XX.
101807         16  AT-FUTURE-RESERVE           PIC S9(7)V99    COMP-3.
101807         16  AT-PAY-CURRENT-RESERVE      PIC S9(7)V99    COMP-3.
101807         16  AT-IBNR-RESERVE             PIC S9(7)V99    COMP-3.
101807         16  AT-INITIAL-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-CURRENT-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
101807         16  AT-ITD-ADDITIONAL-RESERVE   PIC S9(7)V99    COMP-3.
00084          16  AT-EXPENSE-CONTROLS.
00085              20  AT-EXPENSE-METHOD       PIC X.
00086                  88  NO-EXPENSE-CALCULATED    VALUE '1'.
00087                  88  FLAT-DOLLAR-PER-PMT      VALUE '2'.
00088                  88  PERCENT-OF-PMT           VALUE '3'.
00089                  88  DOLLAR-PER-OPEN-MONTH    VALUE '4'.
00090              20  AT-EXPENSE-PERCENT      PIC S9(3)V99    COMP-3.
00091              20  AT-EXPENSE-DOLLAR       PIC S9(3)V99    COMP-3.
00092          16  AT-ITD-PAID-EXPENSES        PIC S9(5)V99    COMP-3.
00093          16  AT-ITD-CHARGEABLE-EXPENSE   PIC S9(5)V99    COMP-3.
00094
00095          16  AT-ITD-LIFE-REFUNDS         PIC S9(5)V99    COMP-3.
00096          16  AT-ITD-AH-REFUNDS           PIC S9(5)V99    COMP-3.
00097
101807*        16  FILLER                      PIC X(53).
101807         16  FILLER                      PIC X(47).
00099
00100          16  AT-RESERVES-LAST-MAINT-DT   PIC XX.
00101          16  AT-RESERVES-LAST-UPDATED-BY PIC X(4).
00102
00103          16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.
00104              20  AT-OPEN-CLOSE-DATE      PIC XX.
00105              20  AT-OPEN-CLOSE-TYPE      PIC X.
00106 *                    C = CLOSED
00107 *                    O = OPEN
00108              20  AT-OPEN-CLOSE-REASON    PIC X(5).
00109 *                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE
00110
00111      12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.
00112          16  AT-PAYMENT-TYPE             PIC X.
00113              88  PARTIAL-PAYMENT                VALUE '1'.
00114              88  FINAL-PAYMENT                  VALUE '2'.
00115              88  LUMP-SUM-PAYMENT               VALUE '3'.
00116              88  ADDITIONAL-PAYMENT             VALUE '4'.
00117              88  CHARGEABLE-EXPENSE             VALUE '5'.
00118              88  NON-CHARGEABLE-EXPENSE         VALUE '6'.
00119              88  VOIDED-PAYMENT                 VALUE '9'.
00120              88  TRANSFER                       VALUE 'T'.
022106             88  LIFE-INTEREST                  VALUE 'I'.
00121
00122          16  AT-CLAIM-TYPE               PIC X.
00123              88  PAID-FOR-AH                    VALUE 'A'.
00124              88  PAID-FOR-LIFE                  VALUE 'L'.
00124              88  PAID-FOR-IUI                   VALUE 'I'.
120503             88  PAID-FOR-GAP                   VALUE 'G'.
052614             88  PAID-FOR-FAM                   VALUE 'F'.
022122             88  PAID-FOR-BRV                   VALUE 'B'.
022122             88  PAID-FOR-HOS                   VALUE 'H'.
100518             88  PAID-FOR-OTH                   VALUE 'O'.
00125          16  AT-CLAIM-PREM-TYPE          PIC X.
00126              88  AT-SINGLE-PREMIUM              VALUE '1'.
00127              88  AT-O-B-COVERAGE                VALUE '2'.
00128              88  AT-OPEN-END-COVERAGE           VALUE '3'.
00129          16  AT-AMOUNT-PAID              PIC S9(7)V99  COMP-3.
00130          16  AT-CHECK-NO                 PIC X(7).
00131          16  AT-PAID-FROM-DT             PIC XX.
00132          16  AT-PAID-THRU-DT             PIC XX.
00133          16  AT-DAYS-IN-PERIOD           PIC S9(4)     COMP.
013017         16  AT-ACH-PAYMENT              PIC X.
013017*        16  FILLER                      PIC X.
00135          16  AT-PAYEES-NAME              PIC X(30).
00136          16  AT-PAYMENT-ORIGIN           PIC X.
00137              88  ONLINE-MANUAL-PMT              VALUE '1'.
00138              88  ONLINE-AUTO-PMT                VALUE '2'.
00139              88  OFFLINE-PMT                    VALUE '3'.
00140          16  AT-CHECK-WRITTEN-DT         PIC XX.
00141          16  AT-TO-BE-WRITTEN-DT         PIC XX.
00142          16  AT-VOID-DATA.
00143              20  AT-VOID-DT              PIC XX.
041807*00144       20  AT-VOID-REASON          PIC X(30).
041807             20  AT-VOID-REASON          PIC X(26).
041807         16  AT-PMT-APPROVED-BY          PIC X(04).
00145          16  AT-ADDL-RESERVE             PIC S9(5)V99  COMP-3.
00146          16  AT-EXPENSE-PER-PMT          PIC S9(5)V99  COMP-3.
082807         16  AT-INT-RATE REDEFINES AT-EXPENSE-PER-PMT
082807                                         PIC S99V9(5)  COMP-3.
00147          16  AT-CREDIT-INTERFACE.
00148              20  AT-PMT-SELECT-DT        PIC XX.
00149                  88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.
00150              20  AT-PMT-ACCEPT-DT        PIC XX.
00151                  88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.
00152              20  AT-VOID-SELECT-DT       PIC XX.
00153                  88  VOID-NOT-SELECTED     VALUE LOW-VALUE.
00154              20  AT-VOID-ACCEPT-DT       PIC XX.
00155                  88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.
00156
00157          16  AT-CHECK-QUE-CONTROL        PIC S9(8)     COMP.
00158                  88  PAYMENT-NOT-QUEUED           VALUE ZERO.
00159                  88  CONVERSION-PAYMENT           VALUE +99999999.
00160          16  AT-CHECK-QUE-SEQUENCE       PIC S9(4)     COMP.
00161
00162          16  AT-FORCE-CONTROL            PIC X.
00163              88  PAYMENT-WAS-FORCED           VALUE '1'.
00164          16  AT-PREV-LAST-PMT-DT         PIC XX.
00165          16  AT-PREV-PAID-THRU-DT        PIC XX.
00166          16  AT-PREV-LAST-PMT-AMT        PIC S9(7)V99  COMP-3.
00167          16  AT-ELIMINATION-DAYS         PIC S999      COMP-3.
00168          16  AT-DAILY-RATE               PIC S9(3)V99  COMP-3.
00169          16  AT-BENEFIT-TYPE             PIC X.
00170
00171          16  AT-EXPENSE-TYPE             PIC X.
00172          16  AT-PAYMENT-APPROVAL-SW      PIC X.
00173
00174          16  AT-PAYEE-TYPE-CD.
00175              20  AT-PAYEE-TYPE           PIC X.
00176                  88  INSURED-PAID           VALUE 'I'.
00177                  88  BENEFICIARY-PAID       VALUE 'B'.
00178                  88  ACCOUNT-PAID           VALUE 'A'.
00179                  88  OTHER-1-PAID           VALUE 'O'.
00180                  88  OTHER-2-PAID           VALUE 'Q'.
00181                  88  DOCTOR-PAID            VALUE 'P'.
00182                  88  EMPLOYER-PAID          VALUE 'E'.
00183              20  AT-PAYEE-SEQ            PIC X.
00184
00185          16  AT-CASH-PAYMENT             PIC X.
00186          16  AT-GROUPED-PAYMENT          PIC X.
00187          16  AT-PAYMENT-NOTE-SEQ-NO      PIC S9(4)       COMP.
00188          16  AT-APPROVAL-LEVEL-REQD      PIC X.
00189          16  AT-APPROVED-LEVEL           PIC X.
00190          16  AT-VOID-TYPE                PIC X.
00191              88  AT-PAYMENT-WAS-STOPPED     VALUE 'S'.
00192              88  AT-PAYMENT-WAS-VOIDED      VALUE 'V'.
00193          16  AT-AIG-UNEMP-IND            PIC X.
00194              88  AT-AIG-UNEMPLOYMENT-PMT    VALUE 'U'.
00195          16  AT-ASSOCIATES               PIC X.
00196              88  AT-AIG-INTERFACE           VALUE 'I' 'N'.
00197              88  AT-AIG-NON-INTERFACE       VALUE 'A' 'M'.
00198
00199          16  AT-FORM-CTL-SEQ-NO          PIC S9(4)       COMP.
00200          16  AT-CV-PMT-CODE              PIC X.
00201              88  FULL-DEATH-PAYMENT         VALUE '1'.
00202              88  HALF-DEATH-PAYMENT         VALUE '2'.
00203              88  FULL-ADD-PAYMENT           VALUE '3'.
00204              88  HALF-ADD-PAYMENT           VALUE '4'.
00205              88  FULL-RIDER-PAYMENT         VALUE '5'.
00206              88  HALF-RIDER-PAYMENT         VALUE '6'.
00207              88  NON-CHG-EXP-PAYMENT        VALUE '7'.
00208              88  ADDL-PAYMENT               VALUE '8'.
00209
00210          16  AT-EOB-CODE1                PIC XXX.
00211          16  AT-EOB-CODE2                PIC XXX.
00212          16  AT-EOB-CODE3                PIC XXX.
020413         16  FILLER REDEFINES AT-EOB-CODE3.
020413             20  AT-PRINT-CLM-FORM       PIC X.
020413             20  AT-PRINT-SURVEY         PIC X.
102413             20  AT-SPECIAL-RELEASE      PIC X.
00213          16  AT-EOB-CODE4                PIC XXX.
               16  FILLER REDEFINES AT-EOB-CODE4.
                   20  AT-INT-PMT-SELECT-DT    PIC XX.
                   20  FILLER                  PIC X.
00214          16  AT-EOB-CODE5                PIC XXX.
062806         16  FILLER REDEFINES AT-EOB-CODE5.
062806             20  AT-PMT-PROOF-DT         PIC XX.
062806             20  FILLER                  PIC X.
00215
071910         16  AT-PRINT-EOB-WITH-CHECK     PIC X.
071910             88  AT-PRINT-EOB            VALUE 'Y'.
00217
00218          16  AT-PAYMENT-LAST-MAINT-DT    PIC XX.
00219          16  AT-PAYMENT-LAST-UPDATED-BY  PIC X(4).
00220
00221      12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.
00222          16  AT-SCHEDULE-START-DT        PIC XX.
00223          16  AT-SCHEDULE-END-DT          PIC XX.
00224          16  AT-TERMINATED-DT            PIC XX.
00225          16  AT-LAST-PMT-TYPE            PIC X.
00226              88  LAST-PMT-IS-FINAL              VALUE 'F'.
00227              88  LAST-PMT-IS-PARTIAL            VALUE 'P'.
00228          16  AT-FIRST-PMT-DATA.
00229              20  AT-FIRST-PMT-AMT        PIC S9(7)V99  COMP-3.
00230              20  AT-DAYS-IN-1ST-PMT      PIC S9(4)     COMP.
00231              20  AT-1ST-PAY-THRU-DT      PIC XX.
00232          16  AT-REGULAR-PMT-DATA.
00233              20  AT-REGULAR-PMT-AMT      PIC S9(7)V99  COMP-3.
00234              20  AT-DAYS-IN-REG-PMT      PIC S9(4)     COMP.
00235              20  AT-INTERVAL-MONTHS      PIC S9(4)     COMP.
00236          16  AT-AUTO-PAYEE-CD.
00237              20  AT-AUTO-PAYEE-TYPE      PIC X.
00238                  88  INSURED-PAID-AUTO      VALUE 'I'.
00239                  88  BENEFICIARY-PAID-AUTO  VALUE 'B'.
00240                  88  ACCOUNT-PAID-AUTO      VALUE 'A'.
00241                  88  OTHER-1-PAID-AUTO      VALUE 'O'.
00242                  88  OTHER-2-PAID-AUTO      VALUE 'Q'.
00243              20  AT-AUTO-PAYEE-SEQ       PIC X.
00244          16  AT-AUTO-PAY-DAY             PIC 99.
00245          16  AT-AUTO-CASH                PIC X.
00246              88  AT-CASH                      VALUE 'Y'.
00247              88  AT-NON-CASH                  VALUE 'N'.
070909*        16  FILLER                      PIC X(129).
070909         16  AT-AUTO-END-LETTER          PIC X(4).
070909         16  FILLER                      PIC X(125).
00249
00250          16  AT-AUTO-PAY-LAST-MAINT-DT   PIC XX.
00251          16  AT-AUTO-PAY-LAST-UPDATED-BY PIC X(4).
00252
00253      12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.
00254          16  AT-LETTER-SENT-DT           PIC XX.
00255          16  AT-RECEIPT-FOLLOW-UP        PIC XX.
00256          16  AT-AUTO-RE-SEND-DT          PIC XX.
00257          16  AT-LETTER-ANSWERED-DT       PIC XX.
00258          16  AT-LETTER-ARCHIVE-NO        PIC S9(8)     COMP.
00259          16  AT-LETTER-ORIGIN            PIC X.
00260              88  ONLINE-CREATION              VALUE '1' '3'.
00261              88  OFFLINE-CREATION             VALUE '2' '4'.
                   88  NAPER-ONLINE-CREATION        VALUE '3'.
                   88  NAPER-OFFLINE-CREATION       VALUE '4'.
00262          16  AT-STD-LETTER-FORM          PIC X(4).
00263          16  AT-REASON-TEXT              PIC X(70).
00264          16  AT-ADDRESS-REC-SEQ-NO       PIC S9(4)     COMP.
00265          16  AT-ADDRESEE-TYPE            PIC X.
00266               88  INSURED-ADDRESEE            VALUE 'I'.
00267               88  BENEFICIARY-ADDRESEE        VALUE 'B'.
00268               88  ACCOUNT-ADDRESEE            VALUE 'A'.
00269               88  PHYSICIAN-ADDRESEE          VALUE 'P'.
00270               88  EMPLOYER-ADDRESEE           VALUE 'E'.
00271               88  OTHER-ADDRESEE-1            VALUE 'O'.
00272               88  OTHER-ADDRESEE-2            VALUE 'Q'.
00273          16  AT-ADDRESSEE-NAME           PIC X(30).
00274          16  AT-INITIAL-PRINT-DATE       PIC XX.
00275          16  AT-RESEND-PRINT-DATE        PIC XX.
00276          16  AT-CORR-SOL-UNSOL           PIC X.
00277          16  AT-LETTER-PURGED-DT         PIC XX.
CIDMOD*
CIDMOD*FOLLOWING CID CHGS REENTERED AS DMD CHGS OVERLAID THEM.
CIDMOD*
CIDMOD         16  AT-CSO-REDEFINITION.
040110             20  AT-RESEND-LETTER-FORM   PIC X(4).
040110             20  AT-AUTO-CLOSE-IND       PIC X(1).
040110             20  AT-LETTER-TO-BENE       PIC X(1).
102610             20  AT-STOP-LETTER-DT       PIC X(2).
062217             20  AT-AUTH-RCVD            PIC X(1).
062217             20  FILLER                  PIC X(18).
040110*             20  FILLER                  PIC X(27).
CIDMOD             20  AT-CSO-LETTER-STATUS    PIC X.
CIDMOD                 88  AT-CSO-LETTER-ONLINE    VALUE '1'.
CIDMOD                 88  AT-CSO-LETTER-PURGED    VALUE '2'.
CIDMOD                 88  AT-CSO-LETTER-RELOADED  VALUE '3'.
CIDMOD             20  AT-CSO-LETTER-PURGE-DATE   PIC XX.
CIDMOD             20  AT-CSO-LETTER-RELOAD-DATE  PIC XX.
CIDMOD*
CIDMOD*FOLLOWING DMD CHGS COMMENTED OUT AS THEY OVERLAY CID MODS NEEDED
CIDMOD*
CIDMOD*        16  FILLER                      PIC X(26).
CIDMOD*
CIDMOD*        16  AT-DMD-BSR-CODE             PIC X.
CIDMOD*            88  AT-AUTOMATED-BSR              VALUE 'A'.
CIDMOD*            88  AT-NON-AUTOMATED-BSR          VALUE 'B' ' '.
CIDMOD*
CIDMOD*        16  AT-DMD-LETTER-STATUS        PIC X.
CIDMOD*            88  AT-DMD-LETTER-ONLINE          VALUE '1'.
CIDMOD*            88  AT-DMD-LETTER-PURGED          VALUE '2'.
CIDMOD*            88  AT-DMD-LETTER-RELOADED        VALUE '3'.
CIDMOD*        16  AT-DMD-LETTER-PURGE-DT      PIC XX.
CIDMOD*        16  AT-DMD-LETTER-RELOAD-DT     PIC XX.
00290
00291          16  AT-CORR-LAST-MAINT-DT       PIC XX.
00292          16  AT-CORR-LAST-UPDATED-BY     PIC X(4).
00293
00294      12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.
00295          16  AT-ADDRESS-TYPE             PIC X.
00296              88  INSURED-ADDRESS               VALUE 'I'.
00297              88  BENEFICIARY-ADDRESS           VALUE 'B'.
00298              88  ACCOUNT-ADDRESS               VALUE 'A'.
00299              88  PHYSICIAN-ADDRESS             VALUE 'P'.
00300              88  EMPLOYER-ADDRESS              VALUE 'E'.
00301              88  OTHER-ADDRESS-1               VALUE 'O'.
00302              88  OTHER-ADDRESS-2               VALUE 'Q'.
00303          16  AT-MAIL-TO-NAME             PIC X(30).
00304          16  AT-ADDRESS-LINE-1           PIC X(30).
00305          16  AT-ADDRESS-LINE-2           PIC X(30).
00306          16  AT-CITY-STATE.
                   20  AT-CITY                 PIC X(28).
                   20  AT-STATE                PIC XX.
00307          16  AT-ZIP.
00308              20  AT-ZIP-CODE.
00309                  24  AT-ZIP-1ST          PIC X.
00310                      88  AT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00311                  24  FILLER              PIC X(4).
00312              20  AT-ZIP-PLUS4            PIC X(4).
00313          16  AT-CANADIAN-POSTAL-CODE  REDEFINES  AT-ZIP.
00314              20  AT-CAN-POSTAL-1         PIC XXX.
00315              20  AT-CAN-POSTAL-2         PIC XXX.
00316              20  FILLER                  PIC XXX.
00317          16  AT-PHONE-NO                 PIC 9(11)     COMP-3.
061511*         16  FILLER                      PIC X(23).
061511         16  AT-VFY-2ND-BENE-SSN         PIC X(9).
061511         16  AT-VFY-2ND-BENE-VERIFIED    PIC X.
061511         16  FILLER                      PIC X(13).
00319          16  AT-ADDRESS-LAST-MAINT-DT    PIC XX.
00320          16  AT-ADDRESS-LAST-UPDATED-BY  PIC X(4).
00321
00322      12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00323          16  AT-INFO-LINE-1              PIC X(60).
061013         16  FILLER REDEFINES AT-INFO-LINE-1.
061013             20  AT-NOTE-ERROR-NO OCCURS 15
061013                                         PIC X(4).
00324          16  AT-INFO-LINE-2              PIC X(60).
040814         16  FILLER REDEFINES AT-INFO-LINE-2.
040814             20  AT-ICD-CODE-1           PIC X(8).
040814             20  AT-ICD-CODE-2           PIC X(8).
040814             20  FILLER                  PIC X(44).
00325          16  AT-INFO-TRAILER-TYPE        PIC X.
061013             88  AT-ERRORS-NOTE          VALUE 'E'.
00326              88  AT-PAYMENT-NOTE         VALUE 'P'.
00327              88  AT-CALL-NOTE            VALUE 'C'.
00328              88  AT-MAINT-NOTE           VALUE 'M'.
00329              88  AT-CERT-CHANGE          VALUE 'X'.
080106             88  AT-APPROVAL-NOTE        VALUE 'R'.
080106             88  AT-NOTE-FILE-NOTE       VALUE 'N'.
022614             88  AT-CERT-CANCELLED       VALUE 'T'.
00330          16  AT-CALL-TYPE                PIC X.
00331              88  AT-PHONE-CALL-IN        VALUE 'I'.
102418             88  AT-PHONE-CALL-NEW       VALUE 'N'.
00332              88  AT-PHONE-CALL-OUT       VALUE 'O'.
00333          16  AT-NOTE-CONTINUATION        PIC X.
00334              88  AT-CONTINUED-NOTE       VALUE 'X'.
071910         16  AT-EOB-CODES-EXIST          PIC X.
071910             88  AT-EOB-CODES-PRESENT    VALUE 'Y'.
00335          16  FILLER                      PIC X(35).
00336          16  AT-GEN-INFO-LAST-MAINT-DT   PIC XX.
00337          16  AT-GEN-INFO-LAST-UPDATED-BY PIC X(4).
00338
00339      12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.
00340          16  AT-PROMPT-LINE-1            PIC X(60).
00341          16  AT-PROMPT-LINE-2            PIC X(60).
00342          16  AT-PROMPT-START-DT          PIC XX.
00343          16  AT-PROMPT-END-DT            PIC XX.
00344          16  FILLER                      PIC X(35).
00345          16  AT-PROMPT-LAST-MAINT-DT     PIC XX.
00346          16  AT-PROMPT-LAST-UPDATED-BY   PIC X(4).
00347
00348      12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
00349          16  AT-DENIAL-INFO-1            PIC X(60).
00350          16  AT-DENIAL-INFO-2            PIC X(60).
00351          16  AT-DENIAL-DT                PIC XX.
00352          16  AT-RETRACTION-DT            PIC XX.
00353          16  AT-DENIAL-REASON-CODE       PIC X(4).
050506*         16  FILLER                      PIC X(31).
050506         16  AT-DENIAL-PROOF-DT          PIC XX.
050506         16  FILLER                      PIC X(29).
00355          16  AT-DENIAL-LAST-MAINT-DT     PIC XX.
00356          16  AT-DENIAL-LAST-UPDATED-BY   PIC X(4).
00357
00358      12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.
00359          16  AT-OLD-INCURRED-DT          PIC XX.
00360          16  AT-OLD-REPORTED-DT          PIC XX.
00361          16  AT-OLD-ESTABLISHED-DT       PIC XX.
00362          16  AT-OLD-TOTAL-PAID           PIC S9(7)V99     COMP-3.
00363          16  AT-OLD-DAYS-PAID            PIC S9(4)        COMP.
00364          16  AT-OLD-NO-OF-PMTS           PIC S9(3)        COMP-3.
00365          16  AT-OLD-PAID-THRU-DT         PIC XX.
00366          16  AT-LAST-PMT-MADE-DT         PIC XX.
00367          16  FILLER                      PIC X(26).
00368          16  AT-OLD-DIAG-CODE            PIC X(6).
00369          16  AT-TRAILER-CNT-AT-CHG       PIC S9(4)        COMP.
00370          16  AT-OLD-ITD-PAID-EXPENSE     PIC S9(5)V99     COMP-3.
00371          16  AT-OLD-CHARGABLE-EXPENSE    PIC S9(5)V99     COMP-3.
00372          16  AT-OLD-INIT-MAN-RESV        PIC S9(7)V99     COMP-3.
00373          16  AT-OLD-CURRENT-MAN-RESV     PIC S9(7)V99     COMP-3.
00374          16  AT-OLD-ADDL-MAN-RESV        PIC S9(7)V99     COMP-3.
00375          16  AT-OLD-DIAG-DESCRIP         PIC X(60).
040814         16  AT-OLD-ICD-CODE-1           PIC X(8).
040814         16  AT-OLD-ICD-CODE-2           PIC X(8).
040814         16  FILLER                      PIC X(9).
00377          16  AT-INCURRED-LAST-UPDATED-BY PIC X(4).
00378
00379      12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.
00380          16  AT-FORM-SEND-ON-DT          PIC XX.
00381          16  AT-FORM-FOLLOW-UP-DT        PIC XX.
00382          16  AT-FORM-RE-SEND-DT          PIC XX.
00383          16  AT-FORM-ANSWERED-DT         PIC XX.
00384          16  AT-FORM-PRINTED-DT          PIC XX.
00385          16  AT-FORM-REPRINT-DT          PIC XX.
00386          16  AT-FORM-TYPE                PIC X.
00387              88  INITIAL-FORM                  VALUE '1'.
00388              88  PROGRESS-FORM                 VALUE '2'.
00389          16  AT-INSTRUCT-LN-1            PIC X(28).
00390          16  AT-INSTRUCT-LN-2            PIC X(28).
00391          16  AT-INSTRUCT-LN-3            PIC X(28).
00392          16  AT-FORM-ADDR-SEQ-NO         PIC S9(4)      COMP.
00393          16  AT-FORM-ADDRESS             PIC X.
00394              88  FORM-TO-INSURED              VALUE 'I'.
00395              88  FORM-TO-ACCOUNT              VALUE 'A'.
00396              88  FORM-TO-OTHER-1              VALUE 'O'.
00397              88  FORM-TO-OTHER-2              VALUE 'Q'.
00398          16  AT-RELATED-1.
00399              20 AT-REL-CARR-1            PIC X.
00400              20 AT-REL-CLAIM-1           PIC X(7).
00401              20 AT-REL-CERT-1            PIC X(11).
00402          16  AT-RELATED-2.
00403              20 AT-REL-CARR-2            PIC X.
00404              20 AT-REL-CLAIM-2           PIC X(7).
00405              20 AT-REL-CERT-2            PIC X(11).
00406          16  AT-EMP-FORM-SEND-ON-DT      PIC XX.
00407          16  AT-PHY-FORM-SEND-ON-DT      PIC XX.
00408          16  AT-EMP-FORM-ANSWERED-DT     PIC XX.
00409          16  AT-PHY-FORM-ANSWERED-DT     PIC XX.
00410          16  AT-FORM-REM-PRINT-DT        PIC XX.
102610         16  AT-STOP-FORM-DT             PIC X(2).
00411
102610         16  FILLER                      PIC X(09).
00413          16  AT-FORM-LAST-MAINT-DT       PIC XX.
00414          16  AT-FORM-LAST-UPDATED-BY     PIC X(4).
00415 ******************************************************************
00336      EJECT
00337 *    COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
00338      EJECT
00339 *    COPY MPCPLCY.
00001 ******************************************************************
00002 *                                                                *
00003 *                           MPCPLCY                              *
00004 *                            VMOD=1.024                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = POLICY MASTER                             *
00007 *                                                                *
00008 *   FILE TYPE = VSAM,KSDS                                        *
00009 *   RECORD SIZE = 1200 RECFORM = FIXED                           *
00010 *                                                                *
00011 *   BASE CLUSTER = MPPLCY                         RKP=2,LEN=42   *
00012 *       ALTERNATE PATH2 = ** NOT USED **                         *
00013 *       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
00014 *       ALTERNATE PATH4 = MPPLCY4 (BY REF. NO.)   RKP=60,LEN=25  *
00015 *       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT )   RKP=85,LEN=27  *
00016 *       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT )   RKP=112,LEN=15 *
00017 *       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO.)   RKP=127,LEN=27 *
00018 *                                                                *
00019 *   LOG = YES                                                    *
00020 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00021 ******************************************************************
00022 **WARNING*********************************************************
00023 **ANY CHANGES TO THIS COPY BOOK MAY NEED CORRESPONDING CHANGES****
00024 **TO THE FOLLOWING COPY BOOKS: MPCPOLUP                          *
00025 **                             MPCPHSTD                          *
00026 **                             MPCPHSTC                          *
00027 **                             MPCPHSTT                          *
00028 **                                                               *
00029 ******************************************************************
00030
00031  01  POLICY-MASTER.
00032      12  PM-RECORD-ID                      PIC XX.
00033          88  VALID-PM-ID                      VALUE 'PM'.
00034
00035 ******************************************************************
00036 *   BASE CLUSTER = MPPLCY         (BASE KEY)      RKP=2,LEN=42   *
00037 ******************************************************************
00038
00039      12  PM-CONTROL-PRIMARY.
00040          16  PM-PRODUCER-PRIMARY.
00041              20  PM-PROD-PRIMARY.
00042                  24  PM-COMPANY-CD         PIC X.
00043                  24  PM-CGSP-KEY.
00044                      28  PM-CARRIER        PIC X.
00045                      28  PM-GROUPING.
00046                          32  PM-GROUPING-PREFIX
00047                                            PIC X(3).
00048                          32  PM-GROUPING-PRIME
00049                                            PIC X(3).
00050                      28  PM-STATE          PIC X(2).
00051                      28  PM-PRODUCER.
00052                          32  PM-PRODUCER-PREFIX
00053                                            PIC X(4).
00054                          32  PM-PRODUCER-PRIME
00055                                            PIC X(6).
00056              20  PM-POLICY-EFF-DT              PIC XX.
00057          16  PM-REFERENCE-NUMBER.
00058              20  PM-REFNO-PRIME            PIC X(18).
00059              20  PM-REFNO-SFX              PIC XX.
00060
00061 ******************************************************************
00062 *       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
00063 ******************************************************************
00064
00065      12  PM-CONTROL-BY-SSN.
00066          16  PM-COMPANY-CD-A3              PIC X.
00067          16  PM-SOC-SEC-NO.
00068              20  PM-SSN-STATE              PIC XX.
00069              20  PM-SSN-PRODUCER           PIC X(6).
00070              20  PM-SSN-LN3.
00071                  25  PM-INSURED-INITIALS-A3.
00072                      30 PM-INSURED-INITIAL1-A3 PIC X.
00073                      30 PM-INSURED-INITIAL2-A3 PIC X.
00074                  25 PM-PART-LAST-NAME-A3         PIC X.
00075          16  PM-DATE-A3                     PIC XX.
00076          16  PM-TIME-A3                     PIC S9(04)   COMP.
00077
00078 ******************************************************************
00079 *       ALTERNATE PATH4 = MPPLCY4 (BY REFRENCE)   RKP=60,LEN=25  *
00080 ******************************************************************
00081
00082      12  PM-CONTROL-BY-POLICY-NO.
00083          16  PM-COMPANY-CD-A4              PIC X.
00084          16  PM-POLICY-NO-A4.
00085              20  PM-POLICY-PRIME-A4        PIC X(18).
00086              20  PM-POLICY-SFX-A4          PIC XX.
00087          16  PM-DATE-A4                    PIC XX.
00088          16  PM-TIME-A4                    PIC S9(04)   COMP.
00089
00090 ******************************************************************
00091 *       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT NO) RKP=85,LEN=27  *
00092 ******************************************************************
00093
00094      12  PM-CONTROL-BY-ACCOUNT.
00095          16  PM-COMPANY-CD-A5              PIC X.
00096          16  PM-BANK-ACCOUNT-NUMBER        PIC X(20).
00097          16  PM-DATE-A5                    PIC XX.
00098          16  PM-TIME-A5                    PIC S9(07)   COMP.
00099
00100 ******************************************************************
00101 *       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT NO) RKP=112,LEN=15 *
00102 ******************************************************************
00103
00104      12  PM-CONTROL-BY-TRANSIT.
00105          16  PM-COMPANY-CD-A6              PIC X.
00106          16  PM-BANK-TRANSIT-NUMBER.
00107              20  PM-FEDERAL-NUMBER         PIC X(4).
00108              20  PM-BANK-NUMBER            PIC X(4).
00109          16  PM-DATE-A6                    PIC XX.
00110          16  PM-TIME-A6                    PIC S9(07)   COMP.
00111
00112 ******************************************************************
00113 *       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO)    RKP=127,LEN=27 *
00114 ******************************************************************
00115
00116      12  PM-CONTROL-BY-LOAN-NO.
00117          16  PM-COMPANY-CD-A7              PIC X.
00118          16  PM-LOAN-NUMBER                PIC X(20).
00119          16  PM-DATE-A7                    PIC XX.
00120          16  PM-TIME-A7                    PIC S9(07)   COMP.
00121
00122 ******************************************************************
00123 *                 FILE SYNCHRONIZATION DATA                      *
00124 ******************************************************************
00125
00126      12  FILLER                            PIC X(05).
00127      12  PM-FILE-SYNCH-DATA.
00128          16  PM-LAST-CHANGE-DT             PIC XX.
00129          16  PM-LAST-CHANGE-TIME           PIC S9(7)    COMP.
00130          16  PM-LAST-CHANGE-PROCESSOR      PIC X(4).
00131      12  FILLER                            PIC X(05).
00132
00133 ******************************************************************
00134 *                    INSUREDS PROFILE DATA                       *
00135 ******************************************************************
00136
00137      12  PM-INSURED-PROFILE-DATA.
00138          16  PM-INSURED-NAME.
00139              20  PM-INSURED-LAST-NAME     PIC X(15).
00140              20  PM-INSURED-FIRST-NAME.
00141                  24  PM-INSURED-1ST-INIT PIC X.
00142                  24  FILLER               PIC X(9).
00143              20  PM-INSURED-MIDDLE-INIT PIC X.
00144          16  PM-INSURED-ADDRESS.
00145              20  PM-ADDRESS-LINE-1         PIC X(30).
00146              20  PM-ADDRESS-LINE-2         PIC X(30).
00147              20  PM-CITY                   PIC X(25).
00148              20  PM-RESIDENT-STATE         PIC XX.
00149              20  PM-ZIP-CD.
00150                  24  PM-ZIP-FIRST-FIVE     PIC X(5).
00151                  24  PM-ZIP-PLUS-FOUR      PIC X(4).
00152          16  PM-INSURED-PERSONAL.
00153              20  PM-INSURED-OCC-CLASS      PIC X.
00154                  88  PM-PREFERRED            VALUE '1'.
00155                  88  PM-STANDARD             VALUE '2'.
00156                  88  PM-HAZARDOUS            VALUE '3'.
00157                  88  PM-VERY-HAZARDOUS       VALUE '4'.
00158                  88  PM-EXTREME-HAZARDOUS VALUE '5'.
00159                  88  PM-NOT-OCC              VALUE '6'.
00160                  88  PM-OCC-UNKNOWN          VALUE '9'.
00161              20  PM-INSURED-OCC-CD         PIC X(3).
00162              20  PM-INSURED-OCC-CD-NUM REDEFINES
00163                  PM-INSURED-OCC-CD         PIC 9(3).
00164              20  PM-INSURED-SEX            PIC X.
00165                  88  PM-INSURED-SEX-MALE      VALUE 'M'.
00166                  88  PM-INSURED-SEX-FEMALE VALUE 'F'.
00167              20  PM-INSURED-BIRTH-DT       PIC XX.
00168              20  PM-INSURED-ISSUE-AGE      PIC S9(3)     COMP-3.
00169              20  PM-INSURED-HEIGHT-FT      PIC S9(3)     COMP-3.
00170              20  PM-INSURED-HEIGHT-IN      PIC S9(3)     COMP-3.
00171              20  PM-INSURED-WEIGHT         PIC S9(3)     COMP-3.
00172              20  PM-INSURED-BIRTH-STATE PIC XX.
00173              20  PM-INSURED-PHONE-NO       PIC X(13).
00174              20  PM-INSURED-RATED-AGE      PIC S9(3)     COMP-3.
00175          16  PM-INS-LANGUAGE-IND           PIC X(01).
00176              88  PM-ENGLISH                           VALUE 'E'.
00177              88  PM-FRENCH                            VALUE 'F'.
00178              88  PM-SPANISH                           VALUE 'S'.
00179          16  PM-INSURED-TOT-BENEFIT        PIC S9(7)V99  COMP-3.
00180
00181          16  PM-INSURED-AGE-IND            PIC X(01).
00182              88  PM-INSURED-AGE-75-REACHED            VALUE 'Y'.
00183      12  FILLER                            PIC X(13).
00184
00185 ******************************************************************
00186 *                JOINT INSUREDS PROFILE DATA                     *
00187 ******************************************************************
00188
00189      12  PM-JOINT-PROFILE-DATA.
00190          16  PM-JOINT-NAME.
00191              20  PM-JOINT-LAST-NAME        PIC X(15).
00192              20  PM-JOINT-FIRST-NAME.
00193                  24  PM-JOINT-1ST-INIT     PIC X.
00194                  24  FILLER                PIC X(9).
00195              20  PM-JOINT-MIDDLE-INIT      PIC X.
00196          16  PM-JOINT-SOC-SEC-NO.
00197              20  PM-JT-SSN-STATE           PIC XX.
00198              20  PM-JT-SSN-PRODUCER        PIC X(6).
00199              20  PM-JT-SSN-LN3.
00200                  25  PM-JT-INSURED-INITIALS-A3.
00201                      30 PM-JT-INSURED-INITIAL1-A3 PIC X.
00202                      30 PM-JT-INSURED-INITIAL2-A3 PIC X.
00203                  25 PM-JT-PART-LAST-NAME-A3        PIC X.
00204          16  PM-JOINT-PERSONAL.
00205              20  PM-JOINT-OCC-CLASS        PIC X.
00206                  88 PM-JNT-PREFERRED          VALUE '1'.
00207                  88 PM-JNT-STANDARD           VALUE '2'.
00208                  88 PM-JNT-HAZARDOUS          VALUE '3'.
00209                  88 PM-JNT-VERY-HAZARDOUS     VALUE '4'.
00210                  88 PM-JNT-EXTREME-HAZARDOUS VALUE '5'.
00211                  88 PM-JNT-NOT-OCC            VALUE '6'.
00212                  88 PM-JNT-OCC-UNKNOWN        VALUE '9'.
00213              20  PM-JOINT-OCC-CD           PIC X(3).
00214              20  PM-JOINT-SEX              PIC X.
00215                  88  PM-JOINT-SEX-MALE        VALUE 'M'.
00216                  88  PM-JOINT-SEX-FEMALE      VALUE 'F'.
00217              20  PM-JOINT-BIRTH-DT         PIC XX.
00218              20  PM-JOINT-ISSUE-AGE        PIC S9(3)     COMP-3.
00219              20  PM-JOINT-HEIGHT-FT        PIC S9(3)     COMP-3.
00220              20  PM-JOINT-HEIGHT-IN        PIC S9(3)     COMP-3.
00221              20  PM-JOINT-WEIGHT           PIC S9(3)     COMP-3.
00222              20  PM-JOINT-BIRTH-STATE      PIC XX.
00223              20  PM-JOINT-RATED-AGE        PIC S9(3)     COMP-3.
00224          16  PM-JOINT-TOT-BENEFIT          PIC S9(7)V99  COMP-3.
00225          16  PM-JOINT-AGE-IND              PIC X(01).
00226              88  PM-JOINT-AGE-75-REACHED              VALUE 'Y'.
00227
00228      12  FILLER                            PIC X(12).
00229
00230 ******************************************************************
00231 *                  INSURANCE COVERAGE DATA                       *
00232 ******************************************************************
00233
00234      12  PM-INS-COVERAGE-DATA.
00235          16  PM-FREE-PERIOD                PIC S9(03)    COMP-3.
00236          16  PM-LOAN-TERM                  PIC S9(3)     COMP-3.
00237          16  PM-LOAN-APR                   PIC S9V9999   COMP-3.
00238          16  PM-LOAN-DT                    PIC XX.
00239          16  PM-LOAN-PYMT                  PIC S9(5)V99  COMP-3.
00240          16  PM-LOAN-BALC                  PIC S9(7)V99  COMP-3.
00241          16  PM-INS-BENEFIT-MONTHS         PIC S9(3)     COMP-3.
00242          16  PM-INS-MONTH-BENEFIT          PIC S9(7)V99  COMP-3.
00243          16  PM-INS-TOTAL-BENEFIT          PIC S9(7)V99  COMP-3.
00244          16  PM-INS-PLAN-TYPE              PIC X.
00245              88  PM-AH-MORT-PLAN              VALUE 'A'.
00246              88  PM-AD-D-MORT-PLAN            VALUE 'E'.
00247              88  PM-DISMEM-MORT-PLAN          VALUE 'D'.
00248              88  PM-LIFE-MORT-PLAN            VALUE 'L'.
00249          16  PM-INS-PLAN-CD                PIC XX.
00250          16  PM-INS-PLAN-REVISION          PIC X(3).
00251          16  PM-INS-POLICY-FORM            PIC X(12).
00252          16  PM-INS-MSTR-POLICY.
00253              20  PM-FREE-TYPE              PIC X(04).
00254              20  FILLER                    PIC X(08).
00255          16  PM-INS-MSTR-APP.
00256              20  FILLER                    PIC X(11).
00257              20  PM-INS-B-C-TYPE           PIC X(01).
00258          16  PM-INS-RATE-CD                PIC X(5).
00259          16  PM-INS-SEX-RATING             PIC X.
00260              88  PM-NOT-SEX-RATED              VALUE '1'.
00261              88  PM-SEX-RATED                  VALUE '2'.
00262          16  PM-INS-SUBSTANDARD-PCT        PIC S9V9999   COMP-3.
00263          16  PM-INS-SUBSTANDARD-TYPE       PIC X.
00264          16  PM-INS-TERMINATION-DT         PIC XX.
00265          16  PM-INS-MONTH-PREMIUM      PIC S9(5)V999999  COMP-3.
00266          16  PM-INS-CALC-MO-PREM       PIC S9(5)V999999  COMP-3.
00267          16  PM-REINSURANCE-TABLE          PIC X(3).
00268          16  PM-MORTALITY-CD               PIC X(4).
00269          16  PM-INS-TYPE                   PIC X.
00270              88  PM-INDIVIDUAL                VALUES ARE '1' 'I'.
00271              88  PM-GROUP                     VALUES ARE '2' 'G'.
00272          16  PM-LOAN-OFFICER               PIC X(5).
00273          16  PM-POLICY-FEE                 PIC S9(3)V99 COMP-3.
00274          16  PM-DEPENDENT-COUNT            PIC S99      COMP-3.
00275          16  PM-CWA-AMOUNT                 PIC S9(5)V99  COMP-3.
00276          16  PM-LAST-AUTO-RERATE-DT        PIC XX.
00277          16  PM-PREM-FINANCED-SW           PIC X.
00278              88  PM-PREM-FINANCED              VALUE 'Y'.
00279              88  PM-PREM-NOT-FINANCED          VALUE 'N'.
00280
00281          16  PM-INS-TERM-LETTER-IND        PIC X.
00282              88  PM-TERM-INITIALIZED           VALUE 'Y'.
00283          16  PM-INS-UNDERWRITER-MAX-BEN PIC S9(7)V99     COMP-3.
00284      12  FILLER                            PIC X(11).
00285
00286 ******************************************************************
00287 *                    POLICY BILLING DATA                         *
00288 ******************************************************************
00289
00290      12  PM-BILLING-DATA.
00291          16  PM-BILLING-MODE               PIC X(1).
00292              88  PM-ANNUAL                    VALUE '1'.
00293              88  PM-SEMI-ANNUAL               VALUE '2'.
00294              88  PM-QUARTERLY                 VALUE '3'.
00295              88  PM-MONTHLY                   VALUE '4'.
00296              88  PM-BI-MONTHLY                VALUE '5'.
00297              88  PM-SINGLE-PREM               VALUE '6'.
00298          16  PM-BILLING-SCHEDULE           PIC X(1).
00299          16  PM-BILLING-SW                 PIC X(1).
00300              88  PM-FIRST-BILLING             VALUE 'Y'.
00301              88  PM-PAID-IN-ADVANCE           VALUE 'A'.
00302              88  PM-POLICY-FEE-REFUNDED       VALUE 'F'.
00303          16  PM-BILLING-TYPE               PIC X(1).
00304              88  PM-LIST-BILL                 VALUE '1'.
00305              88  PM-TAPE-BILL                 VALUE '2'.
00306              88  PM-TAPE-LIST-BILL            VALUE '3'.
00307              88  PM-GROUP-BILL          VALUE ARE '1' '2' '3'.
00308              88  PM-DIRECT-BILL               VALUE '4'.
00309              88  PM-PAC-BILL            VALUE ARE '5' 'C' 'S'.
00310              88  PM-CHARGE-CARD-BILL          VALUE '6'.
00311              88  PM-INDIV-BILL
00312                                   VALUE ARE '4' '5' '6' 'C' 'S'.
00313              88  PM-GRP-PLCY-BILL             VALUE '7'.
00314              88  PM-GRP-PLCY-PAC              VALUE '8'.
00315              88  PM-GRP-PLCY-CR-CRD           VALUE '9'.
00316              88  PM-GRP-PLCY            VALUE ARE '7' '8' '9'.
00317              88  PM-GRP-PROD                  VALUE 'A'.
00318              88  PM-EFT-CHECKING              VALUE 'C'.
00319              88  PM-EFT-SAVINGS               VALUE 'S'.
00320          16  PM-PAYMENT-AMT                PIC S9(5)V99  COMP-3.
00321          16  PM-OVER-SHORT-AMT             PIC S9(5)V99  COMP-3.
00322          16  PM-LAST-BILL-DT               PIC XX.
00323          16  PM-LAST-BILL-AMT              PIC S9(5)V99  COMP-3.
00324          16  PM-BILL-TO-DT                 PIC XX.
00325          16  PM-LAST-PYMT-DT               PIC XX.
00326          16  PM-PAID-TO-DT                 PIC XX.
00327          16  PM-PYMT-INVOICE-NUMBER        PIC X(6).
00328          16  PM-MONTHS-PAID                PIC S9(3)     COMP-3.
00329          16  PM-TOTAL-PREM-RECVD           PIC S9(7)V99  COMP-3.
00330          16  PM-BILLING-GROUPING-CODE      PIC X(6).
00331          16  PM-CHARGE-CARD-EXP-DT         PIC X(2).
00332          16  PM-CHARGE-CARD-TYPE           PIC X(2).
00333              88  PM-VISA                      VALUE 'VI'.
00334              88  PM-MSTR-CARD                 VALUE 'MC'.
00335              88  PM-DINERS-CLUB               VALUE 'DN'.
00336              88  PM-DISCOVER                  VALUE 'DS'.
00337              88  PM-CARTE-BLANCHE             VALUE 'CB'.
00338              88  PM-AMERICAN-EXPRESS          VALUE 'AE'.
00339          16  PM-BILL-INVOICE-NUMBER        PIC X(6).
00340          16  PM-BILL-DAY                   PIC S99       COMP-3.
00341          16  PM-RES-PREM-TAX           PIC S9(3)V999999  COMP-3.
00342      12  FILLER                            PIC X(15).
00343
00344 ******************************************************************
00345 *                     CLAIM PAYMENT DATA                         *
00346 ******************************************************************
00347
00348      12  PM-CLAIM-PAYMENT-DATA.
00349          16  PM-CLAIM-BENEFICIARY-NAME     PIC X(25).
00350          16  PM-CLAIM-INTERFACE-SW         PIC X.
00351              88  PM-NO-CLAIM-ATTACHED         VALUE SPACE.
00352              88  PM-POLICY-AND-CLAIM-ONLINE VALUE '1'.
00353              88  PM-POLICY-CREATED-FOR-CLAIM VALUE '2'.
00354              88  PM-CLAIM-CLOSED              VALUE '3'.
00355              88  PM-ACTIVE-CLAIM              VALUE '1' '2'.
00356              88  PM-CLAIM-ATTACHED            VALUE '1' '2' '3'.
00357          16  PM-CLAIM-INCURRED-DT          PIC XX.
00358          16  PM-CLAIM-PAID-TO-DT           PIC XX.
00359          16  PM-CLAIM-PAYMENT-CNT          PIC S9(3)     COMP-3.
00360          16  PM-CLAIM-LAST-PAYMENT-AMT     PIC S9(7)V99  COMP-3.
00361          16  PM-CLAIM-EXPENSES-ITD         PIC S9(7)V99  COMP-3.
00362          16  PM-CLAIM-PAYMENTS-ITD         PIC S9(7)V99  COMP-3.
00363          16  PM-CLAIM-ACCUMULATOR          PIC S9(7)V99  COMP-3.
00364          16  PM-CLAIM-ATTACH-CNT           PIC S9(3)     COMP-3.
00365          16  PM-CLAIM-LIFE-ITD             PIC S9(7)V99  COMP-3.
00366          16  PM-CLAIM-AH-ITD               PIC S9(7)V99  COMP-3.
00367          16  PM-CLAIM-RIDER-ITD            PIC S9(7)V99  COMP-3.
00368
00369      12  FILLER                            PIC X(03).
00370
00371 ******************************************************************
00372 *                POLICY STATUS AND DISPOSITION                   *
00373 ******************************************************************
00374
00375      12  PM-STATUS-DISPOSITION-DATA.
00376          16  PM-ISSUE-EOM-DT               PIC XX.
00377          16  PM-REPLACEMENT-SWITCH         PIC X.
00378          16  PM-APPL-SIGN-DT               PIC XX.
00379          16  PM-UNDERWRITER                PIC X(3).
00380          16  PM-ENTRY-PROCESSOR            PIC X(4).
00381          16  PM-ENTRY-STATUS               PIC X.
00382              88  PM-NORMAL                    VALUE '1'.
00383              88  PM-TAKE-OVER                 VALUE '2'.
00384              88  PM-CONVERSION                VALUE '4'.
00385              88  PM-RE-ISSUE                  VALUE '5'.
00386              88  PM-REINSURANCE-ONLY          VALUE '9'.
00387          16  PM-ENTRY-DT                   PIC XX.
00388          16  PM-ENTRY-TIME                 PIC S9(7) COMP-3.
00389          16  PM-EXIT-DT                    PIC XX.
00390          16  PM-CURRENT-STATUS             PIC X.
00391              88  PM-LAPSE                     VALUE '0'.
00392              88  PM-ACTIVE                    VALUE '1'.
00393              88  PM-PENDING-ISSUE             VALUE '2'.
00394              88  PM-DECLINED                  VALUE '3'.
00395              88  PM-PENDING-CANCEL            VALUE '4'.
00396              88  PM-PENDING-ISSUE-ERROR       VALUE '5'.
00397              88  PM-CLAIM-APPLIED             VALUE '6'.
00398              88  PM-CANCEL                    VALUE '7'.
00399              88  PM-PENDING-UNWTR-REVW        VALUE '8'.
00400              88  PM-PENDING-CANCEL-ERROR      VALUE '9'.
00401              88  PM-CANCEL-TRANSFER           VALUE 'C'.
00402              88  PM-CLAIM-SETTLEMENT          VALUE 'F'.
00403              88  PM-TERMINATE                 VALUE 'T'.
00404 ** NOTE TYPE 1 IS ANYTHING THAT IS OR HAS BEEN ACTIVE.  TYPE 2 IS
00405 ** EVERYTHING ELSE.  IF YOU ADD A STATUS ADD THE VALUE TO ONE OF
00406 ** THESE GROUPS.
00407              88  PM-TYPE-STAT-1
00408                      VALUES ARE '0' '1' '4' '6' '7' '9'
00409                                 'C' 'F' 'T'.
00410              88  PM-TYPE-STAT-2
00411                      VALUES ARE '2' '3' '5' '8'.
00412              88  PM-BILLABLE-STATUS VALUES ARE '0' '1' '6'.
00413              88  PM-PENDING-STATUS
00414                                 VALUES ARE '2' '4' '5' '8' '9'.
00415              88  PM-PENDING-ISSUE-STATUS
00416                                 VALUES ARE '2' '5' '8'.
00417              88  PM-CANCEL-STATUS
00418                                 VALUES ARE '4' '7' '9' 'C'.
00419          16  PM-CANCEL-CAUSE-CD            PIC X(3).
00420          16  PM-CANCEL-DT                  PIC XX.
00421          16  PM-REFUND-AMT                 PIC S9(5)V99  COMP-3.
00422          16  PM-CALC-REFUND-AMT            PIC S9(5)V99  COMP-3.
00423          16  PM-DECLINE-CD                 PIC X(3).
00424          16  PM-DECLINE-DT                 PIC XX.
00425          16  PM-LAST-LAPSE-DT              PIC XX.
00426          16  PM-LAST-REINSTATE-DT          PIC XX.
00427          16  PM-SECURITY-ACCESS-CODE       PIC X.
00428          16  PM-PREV-CONTROL-PRIMARY.
00429              20  PM-PREV-COMPANY-CD             PIC X.
00430              20  PM-PREV-CARRIER                PIC X.
00431              20  PM-PREV-GROUPING.
00432                  24  PM-PREV-GROUPING-PREFIX PIC X(3).
00433                  24  PM-PREV-GROUPING-PRIME     PIC X(3).
00434              20  PM-PREV-STATE                  PIC XX.
00435              20  PM-PREV-PRODUCER.
00436                  24  PM-PREV-PRODUCER-PREFIX PIC X(4).
00437                  24  PM-PREV-PRODUCER-PRIME     PIC X(6).
00438              20  PM-PREV-POLICY-EFF-DT          PIC XX.
00439              20  PM-PREV-REFERENCE-NUMBER.
00440                  24  PM-PREV-REFNO-PRIME        PIC X(18).
00441                  24  PM-PREV-REFNO-SFX          PIC XX.
00442          16  PM-ACTION-DT                  PIC XX.
00443          16  PM-ACTION-CODE                PIC X(3).
00444          16  PM-ACTION-DT-2                PIC XX.
00445          16  PM-ACTION-CODE-2              PIC X(3).
00446          16  PM-ACTION-DT-3                PIC XX.
00447          16  PM-ACTION-CODE-3              PIC X(3).
00448          16  PM-ACTION-DT-4                PIC XX.
00449          16  PM-ACTION-CODE-4              PIC X(3).
00450          16  PM-ACTION-DT-5                PIC XX.
00451          16  PM-ACTION-CODE-5              PIC X(3).
00452
00453          16  PM-KEY-CHANGE                 PIC X.
00454                  88  PM-NO-KEY-CHG      VALUES ARE ' ' 'N'.
00455                  88  PM-KEY-CHG              VALUE 'Y'.
00456          16  PM-KEY-CHANGE-DT              PIC XX.
00457
00458          16  PM-RTI-INDICATOR              PIC X.
00459          16  PM-REASON-CODE                PIC X(3).
00460          16  PM-IN-OUT-PROCESSING-IND      PIC X(1).
00461              88  PM-IN-OUT-PROCESSING      VALUE 'Y'.
00462              88  PM-NOT-IN-OUT-PROCESSING  VALUE SPACES.
00463
00464      12  FILLER                            PIC X(12).
00465
00466 ******************************************************************
00467 *                 AGENT AND COMMISSION DATA                      *
00468 ******************************************************************
00469
00470      12  PM-COMMISSION-DATA.
00471          16  PM-REMIT-TO                   PIC S9(3) COMP-3.
00472          16  PM-COMM-CHANGE-SW             PIC X.
00473                  88  PM-COMMISSION-CHANGE     VALUE 'Y'.
00474          16  PM-AGENT-INFORMATION OCCURS     5 TIMES.
00475              20  PM-AGENT-NUMBER           PIC X(10).
00476              20  PM-AGENT-TYPE             PIC X.
00477                  88  PM-PRODUCER-LEVEL-AGENT
00478                                               VALUES ARE 'C' 'D'.
00479                  88  PM-AGENT-GROSS           VALUE 'C'.
00480                  88  PM-AGENT-REINS           VALUE 'R'.
00481                  88  PM-AGENT-GROSS-REINS     VALUE 'D'.
00482                  88  PM-OVERWRITE-GROSS       VALUE 'O'.
00483                  88  PM-OVERWRITE-GROSS-REINS VALUE 'P'.
00484                  88  PM-OVERWRITE-REINS       VALUE 'T'.
00485                  88  PM-REINS-ONLY            VALUE 'W'.
00486              20  PM-COMMISSION-BILL-PAID PIC X(1).
00487                  88  PM-GENERATE-BILL         VALUE 'B'.
00488                  88  PM-GENERATE-PAID         VALUE 'P'.
00489              20  PM-AGENT-COMP-1ST-YEAR PIC S99V999.
00490              20  PM-COMP-1ST-YEAR-TYPE     PIC X(1).
00491                  88  PM-COMP-1ST-YEAR-PERCENT VALUE '1'.
00492                  88  PM-COMP-1ST-YEAR-DOLLARS VALUE '2'.
00493                  88  PM-COMP-1ST-YEAR-NOT-USED VALUE '3'.
00494              20  PM-RENEWAL-DATA.
00495                  24  PM-AGENT-RENEWAL-DATA OCCURS 6 TIMES.
00496                      28  PM-RENEW-MONTHS     PIC S999    COMP-3.
00497                      28  PM-RENEW-COMMISSION
00498                                              PIC S99V999 COMP-3.
00499                      28  PM-RENEW-TYPE       PIC X(1).
00500                          88  PM-COMP-RENEW-PERCENT      VALUE '1'.
00501                          88  PM-COMP-RENEW-DOLLARS      VALUE '2'.
00502                          88  PM-COMP-RENEW-NOT-USED     VALUE '3'.
00503              20  PM-COMP-RECALC-FLAG       PIC X(1).
00504                  88  PM-BYPASS-RECALC         VALUE 'N'.
00505      12  FILLER                            PIC X(20).
00506 ******************************************************************
00507 *             CUSTOMER DATA                                      *
00508 ******************************************************************
00509      12  PM-CUSTOMER-ID                    PIC X(20).
00510 ******************************************************************
00511      12  FILLER                            PIC X(43).
00512 ******************************************************************
00340      EJECT
00341 *    COPY MPCPLAN.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCPLAN                             *
00004 *                            VMOD=1.012                          *
00005 *                                                                *
00006 *   MORTGAGE SYSTEM PRODUCER PLAN MASTER FILE.                   *
00007 *                                                                *
00008 *   THIS COPYBOOK IS USED FOR THE ONLINE                         *
00009 *   PLAN CODE MASTER FILE.                                       *
00010 *                                                                *
00011 *   FILE DESCRIPTION = PRODUCER PLAN MASTER                      *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 450  RECFORM = FIX                             *
00015 *                                                                *
00016 *   BASE CLUSTER NAME = MPPLAN                    RKP=2,LEN=25   *
00017 *       ALTERNATE PATH1 = MPPLAN2 (ALT GROUPING) RKP=47,LEN=25   *
00018 *                                                                *
00019 *   LOG = NO                                                     *
00020 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00021 *                                                                *
00022 *                                                                *
00023 ******************************************************************
00024
00025  01  PRODUCER-PLANS.
00026      12  PP-RECORD-ID                      PIC  X(02).
00027          88  VALID-PP-ID                      VALUE 'PP'.
00028
00029 ******************************************************************
00030 *   BASE CLUSTER NAME = MPPLAN                    RKP=2,LEN=25   *
00031 ******************************************************************
00032
00033      12  PP-CONTROL-PRIMARY.
00034          16  PP-PROD-PRIMARY.
00035              20  PP-COMPANY-CD             PIC  X(01).
00036              20  PP-CONTROL-A.
00037                  24  PP-CARRIER            PIC  X(01).
00038                  24  PP-GROUPING.
00039                      28  PP-GROUPING-PREFIX
00040                                            PIC  X(03).
00041                      28  PP-GROUPING-PRIME PIC  X(03).
00042                  24  PP-STATE              PIC  X(02).
00043                  24  PP-PRODUCER.
00044                      28  PP-PRODUCER-PREFIX
00045                                            PIC  X(04).
00046                      28  PP-PRODUCER-PRIME PIC  X(06).
00047          16  PP-PRODUCER-PLAN.
00048              20  PP-PLAN-CODE              PIC  X(02).
00049              20  PP-PLAN-REVISION          PIC  9(03).
00050      12  FILLER                            PIC  X(20).
00051
00052 ******************************************************************
00053 *      ALTERNATE PATH1 = MPPLAN2 (ALT GROUPING) RKP=47,LEN=25    *
00054 ******************************************************************
00055
00056      12  PP-CONTROL-BY-VAR-GRP.
00057          16  PP-COMPANY-CD-A1              PIC  X(01).
00058          16  PP-VG-CARRIER                 PIC  X(01).
00059          16  PP-VG-GROUPING                PIC  X(06).
00060          16  PP-VG-STATE                   PIC  X(02).
00061          16  PP-VG-PRODUCER                PIC  X(10).
00062          16  PP-VG-PLAN-CODE               PIC  X(02).
00063          16  PP-VG-PLAN-REVISION           PIC  X(03).
00064      12  FILLER                            PIC  X(20).
00065
00066 ******************************************************************
00067 *                PRODUCER SECURITY DATA                          *
00068 ******************************************************************
00069
00070      12  PP-SECURITY-ACCESS-CODE           PIC  X(01).
00071      12  PP-POLICY-CNT                     PIC S9(07)    COMP-3.
00072
00073 ******************************************************************
00074 *                FILE SYNCHRONIZATION DATA                       *
00075 ******************************************************************
00076
00077      12  PP-MAINT-INFORMATION.
00078          16  PP-LAST-MAINT-DATE            PIC  X(02).
00079          16  PP-LAST-MAINT-HHMMSS          PIC S9(07)    COMP-3.
00080          16  PP-LAST-MAINT-USER            PIC  X(04).
00081      12  FILLER                            PIC  X(10).
00082
00083 ******************************************************************
00084 *                   CRITICAL FILE DATES                          *
00085 ******************************************************************
00086
00087      12  PP-PLAN-DATES.
00088          16  PP-PLAN-EFFECT-DATE           PIC  X(02).
00089          16  PP-PLAN-EXPIRE-DATE           PIC  X(02).
00090
00091      12  FILLER                            PIC  X(10).
00092
00093 ******************************************************************
00094 *                GENERAL INFORMATION                             *
00095 ******************************************************************
00096
00097      12  PP-GENERAL-INFORMATION.
00098          16  PP-ALPHA-SEARCH-SW            PIC  X(01).
00099              88  PP-MIB-ALPHA-ALL              VALUE '1'.
00100              88  PP-MIB-ALPHA-NONE             VALUE '2'.
00101              88  PP-MIB-ALPHA-EXCEEDED         VALUE '3'.
00102              88  PP-CLIENT-ALPHA-ALL           VALUE 'A'.
00103              88  PP-CLIENT-ALPHA-NONE          VALUE 'B'.
00104              88  PP-CLIENT-ALPHA-EXCEEDED      VALUE 'C'.
00105              88  PP-BOTH-ALPHA-ALL             VALUE 'X'.
00106              88  PP-BOTH-ALPHA-NONE            VALUE 'Y'.
00107              88  PP-BOTH-ALPHA-EXCEEDED        VALUE 'Z'.
00108              88  PP-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
00109                                                      'A' 'B' 'C'
00110                                                      'X' 'Y' 'Z'.
00111          16  PP-BENEFIT-TYPE               PIC  X(01).
00112              88  PP-BENEFIT-IS-LEVEL            VALUE '1'.
00113              88  PP-BENEFIT-REDUCES             VALUE '2'.
00114          16  PP-DAYS-TO-1ST-NOTICE         PIC  9(02).
00115          16  PP-DAYS-TO-2ND-NOTICE         PIC  9(02).
00116          16  PP-DAYS-TO-3RD-NOTICE         PIC  9(02).
00117          16  PP-DAYS-TO-4TH-NOTICE         PIC  9(02).
00118          16  PP-EFF-DT-RULE-SW             PIC  X(01).
00119              88  PP-EFF-DT-ENTER               VALUE 'E'.
00120              88  PP-EFF-DT-MONTH               VALUE 'M'.
00121              88  PP-EFF-DT-QTR                 VALUE 'Q'.
00122              88  PP-EFF-DT-SEMI                VALUE 'S'.
00123              88  PP-EFF-DT-ANN                 VALUE 'A'.
00124          16  PP-FREE-EXAM-DAYS             PIC S9(03)   COMP-3.
00125          16  PP-GRACE-PERIOD               PIC S9(03)   COMP-3.
00126          16  PP-HEALTH-QUESTIONS           PIC  9(01).
00127          16  PP-NUMBER-LAPSE-NOTICES       PIC S9(03)   COMP-3.
00128          16  PP-MIB-SEARCH-SW              PIC  X(01).
00129              88  PP-MIB-SEARCH-ALL             VALUE '1'.
00130              88  PP-MIB-SEARCH-NONE            VALUE '2'.
00131              88  PP-MIB-SEARCH-EXCEEDED        VALUE '3'.
00132              88  PP-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
00133          16  PP-PLAN-ABBREV                PIC  X(03).
00134          16  PP-PLAN-AGES.
00135              20  PP-MINIMUM-AGE            PIC S9(03)   COMP-3.
00136              20  PP-MAXIMUM-AGE            PIC S9(03)   COMP-3.
00137              20  PP-MAXIMUM-ATTAIN-AGE     PIC S9(03)   COMP-3.
00138          16  PP-PLAN-BENEFITS.
00139              20  PP-CLAIM-CAP              PIC S9(07)V99 COMP-3.
00140              20  PP-MINIMUM-BENEFIT        PIC S9(07)V99 COMP-3.
00141              20  PP-MAXIMUM-BENEFIT        PIC S9(07)V99 COMP-3.
00142              20  PP-MAXIMUM-MONTHLY-BENEFIT
00143                                            PIC S9(07)V99 COMP-3.
00144          16  PP-PLAN-DESCRIPTION           PIC  X(10).
00145          16  PP-POLICY-FEE                 PIC S9(03)V9(02)
00146                                                         COMP-3.
00147          16  PP-PLAN-IND-GRP               PIC  X(01).
00148          16  PP-PLAN-SNGL-JNT              PIC  X(01).
00149              88  PP-COMBINED-PLAN             VALUE 'C'.
00150              88  PP-JNT-PLAN                  VALUE 'J'.
00151              88  PP-SNGL-PLAN                 VALUE 'S'.
00152          16  PP-PLAN-TERMS.
00153              20  PP-MINIMUM-TERM           PIC S9(03)   COMP-3.
00154              20  PP-MAXIMUM-TERM           PIC S9(03)   COMP-3.
00155          16  PP-PLAN-TYPE                  PIC  X(01).
00156              88  PP-AH-MORT-PLAN              VALUE 'A'.
00157              88  PP-AD-D-MORT-PLAN            VALUE 'E'.
00158              88  PP-DISMEM-MORT-PLAN          VALUE 'D'.
00159              88  PP-LIFE-MORT-PLAN            VALUE 'L'.
00160          16  PP-PREMIUM-TOLERANCES.
00161              20  PP-PREM-TOLERANCE         PIC S9(03)   COMP-3.
00162              20  PP-PREM-TOLERANCE-PCT     PIC SV9(03)  COMP-3.
00163          16  PP-RATE-CODE                  PIC  X(05).
00164          16  PP-REOCCURRING-DISABILITY-PRD PIC S9(03)   COMP-3.
00165          16  PP-REPLACEMENT-LAW-SW         PIC  X(01).
00166              88  PP-NO-REPLACE                VALUE '1'.
00167              88  PP-REPLACE-APPLIES           VALUE '2'.
00168              88  PP-VALID-REPLACEMENT-LAW     VALUE '1' '2'.
00169          16  PP-RETRO-RETENTION            PIC S9V9(04) COMP-3.
00170          16  PP-RERATE-CNTL                PIC  X(01).
00171              88  PP-RERATE-WITH-ISSUE-AGE       VALUE '1'.
00172              88  PP-RERATE-WITH-CURRENT-AGE     VALUE '2'.
00173              88  PP-DO-NOT-RERATE               VALUE '3' ' '.
00174              88  PP-AUTO-RECALC                 VALUE '4'.
00175          16  PP-SEX-RATING                 PIC  X(01).
00176              88  PP-NOT-SEX-RATED             VALUE '1'.
00177              88  PP-SEX-RATED                 VALUE '2'.
00178          16  PP-SUBSTANDARD-DATA.
00179              20  PP-SUBSTANDARD-PERCENT    PIC S9(01)V9(04).
00180              20  PP-SUBSTANDARD-TYPE       PIC  X(01).
00181                  88  PP-PCT-OF-BENEFIT        VALUE '1'.
00182                  88  PP-PCT-OF-PREMIUM        VALUE '2'.
00183                  88  PP-NOT-APPLICABLE        VALUE '3'.
00184          16  PP-YEARS-TO-NEXT-RERATE       PIC  9(02).
00185          16  PP-DEPENDANT-COVERAGE         PIC  X(01).
00186              88  PP-DEP-COVERED               VALUE 'Y'.
00187              88  PP-DEP-NOT-COVERED           VALUE 'N' ' '.
00188          16  PP-REFUND-CALC                PIC  X(01).
00189              88  PP-RFND-MP-REFUND     VALUES ARE ' ' LOW-VALUES.
00190              88  PP-RFND-BY-R78               VALUE '1'.
00191              88  PP-RFND-BY-PRO-RATA          VALUE '2'.
00192              88  PP-RFND-AS-CALIF             VALUE '3'.
00193              88  PP-RFND-AS-TEXAS             VALUE '4'.
00194              88  PP-RFND-IS-NET-PAY           VALUE '5'.
00195              88  PP-RFND-ANTICIPATION         VALUE '6'.
00196              88  PP-RFND-MEAN                 VALUE '8'.
00197              88  PP-VALID-REFUND       VALUES ARE ' ' '1' '2' '3'
00198                                                   '4' '5' '6' '8'
00199                                                   LOW-VALUES.
00200          16  PP-ALT-RATE-CODE              PIC  X(05).
00201
00202      12  FILLER                            PIC  X(39).
00203
00204 ******************************************************************
00205 *                     PLAN FORMS AND LETTERS                     *
00206 ******************************************************************
00207
00208      12  PP-PLAN-MASTER-FORMS.
00209          16  PP-POLICY-FORM                PIC  X(12).
00210          16  PP-MASTER-APPLICATION         PIC  X(12).
00211          16  PP-MASTER-POLICY              PIC  X(12).
00212      12  PP-DELINQUENCY-NOTICE-FORMS.
00213          16  PP-1ST-NOTICE-FORM            PIC  X(04).
00214          16  PP-2ND-NOTICE-FORM            PIC  X(04).
00215          16  PP-3RD-NOTICE-FORM            PIC  X(04).
00216          16  PP-4TH-NOTICE-FORM            PIC  X(04).
00217      12  FILLER                            PIC  X(32).
00218      12  PP-TERMINATION-FORM               PIC  X(04).
00219      12  FILLER                            PIC  X(08).
00220      12  PP-ISSUE-LETTER                   PIC  X(04).
00221
00222      12  FILLER                            PIC  X(80).
00223 ******************************************************************
00342      EJECT
00343 *    COPY ELCRETR.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCRETR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER RETRIEVE FILE                *
00008 *                                                                *
00009 *   **** NOTE -- THIS FILE IS IDENTICAL TO CLAIM MASTER (ELMSTR) *
00010 *   ****      ANY CHANGES TO THIS COPYBOOK OR ELCMSTR MUST BE    *
00011 *   ****      DUPLICATED IN THE OTHER.                           *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00015 *                                                                *
00016 *   BASE CLUSTER = ELRETR                         RKP=2,LEN=20   *
00017 *       ALTERNATE PATH1 = ELRETR2 (BY NAME)       RKP=22,LEN=29  *
00018 *       ALTERNATE PATH2 = ELRETR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00019 *       ALTERNATE PATH3 = ELRETR5 (BY CERT NO)    RKP=63,LEN=12  *
00020 *       ALTERNATE PATH4 = ELRETR6 (BY CREDIT CARD NO)
00021 *                                                 RKP=75,LEN=21  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  RETRIEVE-MASTER.
00027      12  RL-RECORD-ID                PIC XX.
00028          88  VALID-RL-ID         VALUE 'RL'.
00029
00030      12  RL-CONTROL-PRIMARY.
00031          16  RL-COMPANY-CD           PIC X.
00032          16  RL-CARRIER              PIC X.
00033          16  RL-CLAIM-NO             PIC X(7).
00034          16  RL-CERT-NO.
00035              20  RL-CERT-PRIME       PIC X(10).
00036              20  RL-CERT-SFX         PIC X.
00037
00038      12  RL-CONTROL-BY-NAME.
00039          16  RL-COMPANY-CD-A1        PIC X.
00040          16  RL-INSURED-LAST-NAME    PIC X(15).
00041          16  RL-INSURED-NAME.
00042              20  RL-INSURED-1ST-NAME PIC X(12).
00043              20  RL-INSURED-MID-INIT PIC X.
00044
00045      12  RL-CONTROL-BY-SSN.
00046          16  RL-COMPANY-CD-A2        PIC X.
00047          16  RL-SOC-SEC-NO.
00048              20  RL-SSN-STATE        PIC XX.
00049              20  RL-SSN-ACCOUNT      PIC X(6).
00050              20  RL-SSN-LN3          PIC X(3).
00051
00052      12  RL-CONTROL-BY-CERT-NO.
00053          16  RL-COMPANY-CD-A4        PIC X.
00054          16  RL-CERT-NO-A4.
00055              20  RL-CERT-A4-PRIME    PIC X(10).
00056              20  RL-CERT-A4-SFX      PIC X.
00057
00058      12  RL-CONTROL-BY-CCN.
00059          16  RL-COMPANY-CD-A5        PIC X.
00060          16  RL-CCN-A5.
00061              20  RL-CCN-NO.
00062                  24  RL-CCN-PREFIX-A5 PIC X(4).
00063                  24  RL-CCN-PRIME-A5 PIC X(12).
00064              20  RL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  RL-INSURED-PROFILE-DATA.
00067          16  RL-INSURED-BIRTH-DT     PIC XX.
00068          16  RL-INSURED-SEX-CD       PIC X.
00069              88  RL-INSURED-IS-MALE     VALUE 'M'.
00070              88  RL-INSURED-IS-FEMALE   VALUE 'F'.
00071              88  RL-INSURED-SEX-UNKNOWN VALUE ' '.
00072          16  RL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  RL-PROCESSING-INFO.
00076          16  RL-PROCESSOR-ID         PIC X(4).
00077          16  RL-CLAIM-STATUS         PIC X.
00078              88  RL-CLAIM-IS-OPEN       VALUE 'O'.
00079              88  RL-CLAIM-IS-CLOSED     VALUE 'C'.
00080          16  RL-CLAIM-TYPE           PIC X.
00081 *            88  RL-AH-CLAIM            VALUE 'A'.
00082 *            88  RL-LIFE-CLAIM          VALUE 'L'.
00083 *            88  RL-PROPERTY-CLAIM      VALUE 'P'.
00084 *            88  RL-UNEMPLOYMENT-CLAIM  VALUE 'U'.
00085          16  RL-CLAIM-PREM-TYPE      PIC X.
00086              88  RL-SINGLE-PREMIUM         VALUE '1'.
00087              88  RL-O-B-COVERAGE           VALUE '2'.
00088              88  RL-OPEN-END-COVERAGE      VALUE '3'.
00089          16  RL-INCURRED-DT          PIC XX.
00090          16  RL-REPORTED-DT          PIC XX.
00091          16  RL-FILE-ESTABLISH-DT    PIC XX.
00092          16  RL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  RL-LAST-PMT-DT          PIC XX.
00094          16  RL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  RL-PAID-THRU-DT         PIC XX.
00096          16  RL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  RL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  RL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  RL-PMT-CALC-METHOD      PIC X.
00100              88  RL-360-DAY-YR          VALUE '1'.
00101              88  RL-365-DAY-YR          VALUE '2'.
00102              88  RL-FULL-MONTHS         VALUE '3'.
00103          16  RL-CAUSE-CD             PIC X(6).
00104
00105          16  RL-PRIME-CERT-NO.
00106              20  RL-PRIME-CERT-PRIME PIC X(10).
00107              20  RL-PRIME-CERT-SFX   PIC X.
00108
00109          16  RL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  RL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  RL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  RL-MICROFILM-NO         PIC X(10).
00114          16  RL-PROG-FORM-TYPE       PIC X.
00115          16  RL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  RL-LAST-REOPEN-DT       PIC XX.
00118          16  RL-LAST-CLOSE-DT        PIC XX.
00119          16  RL-LAST-CLOSE-REASON    PIC X.
00120              88  RL-FINAL-PAID          VALUE '1'.
00121              88  RL-CLAIM-DENIED        VALUE '2'.
00122              88  RL-AUTO-CLOSE          VALUE '3'.
00123              88  RL-MANUAL-CLOSE        VALUE '4'.
00124          16  RL-ASSOC-CERT-SEQU      PIC S99.
00125          16  RL-ASSOC-CERT-TOTAL     PIC S99.
00126          16  RL-CLAIM-PAYMENT-STATUS PIC 9.
00127              88  RL-PAYMENT-IN-PREP     VALUE 1 THRU 9.
00128          16  FILLER                  PIC X(5).
00129
00130      12  RL-CERTIFICATE-DATA.
00131          16  RL-CERT-ORIGIN          PIC X.
00132              88  RL-CERT-WAS-ONLINE     VALUE '1'.
00133              88  RL-CERT-WAS-CREATED    VALUE '2'.
00134              88  RL-COVERAGE-WAS-ADDED  VALUE '3'.
00135          16  RL-CERT-KEY-DATA.
00136              20  RL-CERT-CARRIER     PIC X.
00137              20  RL-CERT-GROUPING    PIC X(6).
00138              20  RL-CERT-STATE       PIC XX.
00139              20  RL-CERT-ACCOUNT.
00140                  24  RL-CERT-ACCOUNT-PREFIX PIC X(4).
00141                  24  RL-CERT-ACCOUNT-PRIME  PIC X(6).
00142              20  RL-CERT-EFF-DT      PIC XX.
00143
00144      12  RL-STATUS-CONTROLS.
00145          16  RL-PRIORITY-CD          PIC X.
00146              88  RL-HIGHEST-PRIORITY    VALUE '9'.
00147          16  RL-SUPV-ATTN-CD         PIC X.
00148              88  RL-SUPV-NOT-REQUIRED   VALUE ' ' 'N'.
00149              88  RL-SUPV-IS-REQUIRED    VALUE 'Y'.
00150          16  RL-PURGED-DT            PIC XX.
00151          16  RL-RESTORED-DT          PIC XX.
00152          16  RL-NEXT-AUTO-PAY-DT     PIC XX.
00153          16  RL-NEXT-RESEND-DT       PIC XX.
00154          16  RL-NEXT-FOLLOWUP-DT     PIC XX.
00155          16  FILLER                  PIC XX.
00156          16  RL-LAST-MAINT-DT        PIC XX.
00157          16  RL-LAST-MAINT-USER      PIC X(4).
00158          16  RL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00159          16  RL-LAST-MAINT-TYPE      PIC X.
00160              88  RL-CLAIM-SET-UP           VALUE ' '.
00161              88  RL-PAYMENT-MADE           VALUE '1'.
00162              88  RL-LETTER-SENT            VALUE '2'.
00163              88  RL-MASTER-WAS-ALTERED     VALUE '3'.
00164              88  RL-MASTER-WAS-RESTORED    VALUE '4'.
00165              88  RL-INCURRED-DATE-CHANGED  VALUE '5'.
00166              88  RL-FILE-CONVERTED         VALUE '6'.
00167          16  RL-RELATED-CLAIM-NO     PIC X(7).
00168          16  RL-HISTORY-ARCHIVE-DT   PIC XX.
00169          16  RL-BENEFICIARY          PIC X(10).
00170          16  RL-FILE-ESTABLISHED-BY  PIC X(4).
00171          16  FILLER                  PIC X(6).
00172
00173      12  RL-TRAILER-CONTROLS.
00174          16  RL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00175              88  RL-1ST-TRL-AVAIL       VALUE +4095.
00176              88  RL-LAST-TRL-AVAIL      VALUE +100.
00177              88  RL-RESV-EXP-HIST-TRLR  VALUE +0.
00178          16  RL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00179          16  FILLER                  PIC XX.
00180          16  RL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00181          16  RL-ADDRESS-TRAILER-CNT.
00182              20  RL-INSURED-ADDR-CNT  PIC S9.
00183                  88  RL-NO-INSURED-AVAILABLE VALUE ZERO.
00184              20  RL-ACCOUNT-ADDR-CNT  PIC S9.
00185                  88  RL-ACCOUNT-IS-ONLINE    VALUE ZERO.
00186              20  RL-BENIF-ADDR-CNT    PIC S9.
00187                  88  RL-BENEFICIARY-IS-ONLINE VALUE ZERO.
00188              20  RL-EMPLOYER-ADDR-CNT PIC S9.
00189                  88  RL-NO-EMPLOY-AVAILABLE   VALUE ZERO.
00190              20  RL-DOCTOR-ADDR-CNT   PIC S9.
00191                  88  RL-NO-DOCTOR-AVAILABLE   VALUE ZERO.
00192              20  RL-OTHER-1-ADDR-CNT  PIC S9.
00193                  88  RL-NO-OTHER-1-ADDRESSES  VALUE ZERO.
00194              20  RL-OTHER-2-ADDR-CNT  PIC S9.
00195                  88  RL-NO-OTHER-2-ADDRESSES  VALUE ZERO.
00196
00197      12  RL-CV-REFERENCE-NO.
00198          16  RL-CV-REFNO-PRIME       PIC X(18).
00199          16  RL-CV-REFNO-SFX         PIC XX.
00200
00201      12  RL-FILE-LOCATION            PIC X(4).
00202
00203      12  RL-PROCESS-ERRORS.
00204          16  RL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00205              88  RL-NO-FATAL-ERRORS     VALUE ZERO.
00206          16  RL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00207              88  RL-NO-FORCABLE-ERRORS  VALUE ZERO.
00208
00209      12  RL-PRODUCT-CD               PIC X.
00210
00211      12  RL-CURRENT-KEY-DATA.
00212          16  RL-CURRENT-CARRIER      PIC X.
00213          16  RL-CURRENT-GROUPING     PIC X(6).
00214          16  RL-CURRENT-STATE        PIC XX.
00215          16  RL-CURRENT-ACCOUNT      PIC X(10).
00216
00217      12  RL-ASSOCIATES               PIC X.
00218          88  RL-ASSOC-NO-INTERFACE      VALUE 'A'.
00219          88  RL-ASSOC-INTERFACE         VALUE 'I'.
00220          88  RL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00221          88  RL-NON-ASSOC-INTERFACE     VALUE 'M'.
00222
00223      12  RL-ACTIVITY-CODE            PIC 99.
00224      12  RL-ACTIVITY-MAINT-DT        PIC XX.
00225      12  RL-ACTIVITY-MAINT-TYPE      PIC X(4).
00226
00227      12  RL-LAPSE-REPORT-CODE        PIC 9.
00228      12  RL-LAG-REPORT-CODE          PIC 9.
00229      12  RL-LOAN-TYPE                PIC XX.
00230      12  RL-LEGAL-STATE              PIC XX.
00231
00232      12  FILLER                      PIC X(5).
00344      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER-L
                                CERTIFICATE-MASTER ACTIVITY-TRAILERS
                                CONTROL-FILE POLICY-MASTER
                                PRODUCER-PLANS RETRIEVE-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL160' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00346
00347      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00348      MOVE '5'                   TO DC-OPTION-CODE.
00349      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
00350      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00351      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00352
00353      IF EIBCALEN = ZERO
00354          GO TO 8800-UNAUTHORIZED-ACCESS.
00355
00356      
      * EXEC CICS HANDLE CONDITION
00357 *        PGMIDERR (8820-XCTL-ERROR)
00358 *        ERROR    (9990-ABEND)
00359 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00006501' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303036353031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00360
00361      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00362
00363      MOVE 2                      TO EMI-NUMBER-OF-LINES.
00364
00365      MOVE THIS-TRAN              TO TRANS-ID.
00366
00367      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00368          MOVE LOW-VALUES TO EL160AO
00369          MOVE ER-0008    TO EMI-ERROR
00370          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00371          MOVE -1         TO CARRSL
00372          GO TO 8110-SEND-DATA.
00373
00374      IF THIS-PGM NOT = PI-CALLING-PROGRAM
00375          MOVE LOW-VALUES TO EL160AO
00376          PERFORM 0500-BUILD-TS-KEY THRU 0510-EXIT
00377          GO TO 0100-UPDATE-PI.
00378
00379      IF EIBAID = DFHCLEAR
00380          GO TO 8200-RETURN-PRIOR.
00381
00382      IF PI-PROCESSOR-ID = 'LGXX'
00383          NEXT SENTENCE
00384      ELSE
00385          
      * EXEC CICS READQ TS
00386 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00387 *            INTO    (SECURITY-CONTROL)
00388 *            LENGTH  (SC-COMM-LENGTH)
00389 *            ITEM    (SC-ITEM)
00390 *        END-EXEC
      *    MOVE '*$II   L              ''   #00006530' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00391          MOVE SC-CLAIMS-DISPLAY (3)    TO  PI-DISPLAY-CAP
00392          MOVE SC-CLAIMS-UPDATE  (3)    TO  PI-MODIFY-CAP
00393          IF NOT DISPLAY-CAP
00394              MOVE 'READ'               TO  SM-READ
00395              PERFORM 9995-SECURITY-VIOLATION
00396              MOVE ER-0070              TO  EMI-ERROR
00397              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00398              MOVE -1                   TO CARRSL
00399              GO TO 8100-SEND-MAP.
00400
00401      
      * EXEC CICS RECEIVE
00402 *        MAP    ('EL160A')
00403 *        MAPSET ('EL160S')
00404 *    END-EXEC.
           MOVE 'EL160A' TO DFHEIV1
           MOVE 'EL160S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00006546' TO DFHEIV0
           MOVE X'382254202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL160AI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00405
00406      IF PFKEYL > ZERO
00407          PERFORM 0200-TRANS-PF THRU 0210-EXIT.
00408
00409      IF SCREEN-ERROR
00410          MOVE ER-0008 TO EMI-ERROR
00411          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00412          MOVE -1 TO CARRSL
00413          GO TO 8110-SEND-DATA.
00414
00415      IF EIBAID = DFHPF12
00416          GO TO 8300-GET-HELP.
00417
00418      IF EIBAID = DFHPF23
00419          GO TO 8810-PF23-ENTERED.
00420
00421      IF EIBAID = DFHPF24
00422          GO TO 8400-RETURN-MASTER.
00423
00424      IF EIBAID NOT = DFHENTER
00425          MOVE ER-0029 TO EMI-ERROR
00426          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00427          MOVE -1 TO CARRSL
00428          GO TO 8110-SEND-DATA.
00429
00430      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.
00431
00432      IF SCREEN-HAS-ERRORS
00433          GO TO 8110-SEND-DATA.
00434
00435      IF PI-PROCESSOR-ID IS EQUAL TO 'LGXX'
00436          MOVE 2500               TO  MAX-TS-PAGES.
00437
00438      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')
00439          MOVE 2500               TO  MAX-TS-PAGES.
00440
00441      PERFORM 2000-BUILD-TS THRU 2010-EXIT.
00442
00443      MOVE COUNT-1 TO PI-TS-COUNT-1.
00444
00445      IF PI-TS-COUNT-1 = ZEROES
00446          MOVE ER-0142 TO EMI-ERROR
00447          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00448          MOVE -1 TO CARRSL
00449          GO TO 8110-SEND-DATA.
00450
00451      PERFORM 5240-WRITE-TS-160A THRU 5250-EXIT.
00452
00453      MOVE XCTL-EL1602 TO CALL-PGM.
00454
00455      GO TO 9200-XCTL.
00456
00457      EJECT
00458
00459  0100-UPDATE-PI.
00460      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00461          GO TO 0110-UPDATE-UP.
00462
00463      MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6.
00464      MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5.
00465      MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4.
00466      MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3.
00467      MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2.
00468      MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1.
00469      MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM.
00470      MOVE THIS-PGM             TO PI-CALLING-PROGRAM.
00471
00472      PERFORM 0430-DELETE-TS THRU 0450-EXIT.
00473
00474      MOVE -1 TO CARRSL.
00475      GO TO 8100-SEND-MAP.
00476
00477  0110-UPDATE-UP.
00478      MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM.
00479      MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM.
00480      MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1.
00481      MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2.
00482      MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3.
00483      MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4.
00484      MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5.
00485      MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00486
00487      GO TO 0400-FIND-SCREEN.
00488
00489  0200-TRANS-PF.
00490      IF EIBAID NOT = DFHENTER
00491          MOVE 'X' TO ERROR-SWITCH
00492          GO TO 0210-EXIT.
00493
00494      IF PFKEYI NOT NUMERIC
00495          MOVE 'X' TO ERROR-SWITCH
00496          GO TO 0210-EXIT.
00497
00498      MOVE PFKEYI TO CHECK-PFKEYS.
00499
00500      IF CHECK-PFKEYS < 1 OR > 24
00501          MOVE 'X' TO ERROR-SWITCH
00502          GO TO 0210-EXIT.
00503
00504      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
00505
00506  0210-EXIT.
00507      EXIT.
00508
00509  0400-FIND-SCREEN.
00510      
      * EXEC CICS HANDLE CONDITION
00511 *        ITEMERR (0410-TS-NOTFND)
00512 *        QIDERR  (0420-FIND-SCREEN-EXIT)
00513 *    END-EXEC.
      *    MOVE '"$<N                  ! # #00006655' TO DFHEIV0
           MOVE X'22243C4E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303036363535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00514
00515      
      * EXEC CICS READQ TS
00516 *        QUEUE  (PI-EL160-KEY)
00517 *        INTO   (EL160AI)
00518 *        LENGTH (EL160A-LENGTH)
00519 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00006660' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL160-KEY, 
                 EL160AI, 
                 EL160A-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00520
00521      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.
00522
00523      PERFORM 0430-DELETE-TS THRU 0450-EXIT.
00524
00525      GO TO 0420-FIND-SCREEN-EXIT.
00526
00527  0410-TS-NOTFND.
00528      MOVE ER-0192                TO EMI-ERROR.
00529      MOVE -1                     TO CARRSL.
00530      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00531
00532  0420-FIND-SCREEN-EXIT.
00533      MOVE -1 TO CARRSL.
00534      GO TO 8100-SEND-MAP.
00535
00536  0430-DELETE-TS.
00537      
      * EXEC CICS HANDLE CONDITION
00538 *        QIDERR (0440-DELETE-CONT)
00539 *    END-EXEC.
      *    MOVE '"$N                   ! $ #00006682' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303036363832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00540
00541      
      * EXEC CICS DELETEQ TS
00542 *        QUEUE (PI-EL1602-KEY)
00543 *    END-EXEC.
      *    MOVE '*&                    #   #00006686' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL1602-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00544
00545  0440-DELETE-CONT.
00546      
      * EXEC CICS HANDLE CONDITION
00547 *        QIDERR (0450-EXIT)
00548 *    END-EXEC.
      *    MOVE '"$N                   ! % #00006691' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303036363931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00549
00550      
      * EXEC CICS DELETEQ TS
00551 *        QUEUE (PI-EL160-KEY)
00552 *    END-EXEC.
      *    MOVE '*&                    #   #00006695' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL160-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00553
00554  0450-EXIT.
00555      EXIT.
00556
00557  0500-BUILD-TS-KEY.
00558      MOVE ZERO      TO COUNT-1.
00559      MOVE EIBTRMID  TO HOLD-TERM.
00560      MOVE LIT-MAP   TO KEY-QUAL.
00561      MOVE HOLD-KEY  TO PI-EL160-KEY.
00562      MOVE LIT-MAP-2 TO KEY-QUAL.
00563      MOVE HOLD-KEY  TO PI-EL1602-KEY.
00564
00565  0510-EXIT.
00566      EXIT.
00567
00568      EJECT
00569  1000-EDIT-SCREEN.
00570      MOVE SPACES TO CNTL-WORK-AREA.
00571
00572      IF CFILEIDL > ZEROS
00573          MOVE CFILEIDI           TO W-VALID-FILE-IND
00574          IF W-VALID-FILE
00575              IF W-RETRIEVE
00576                  MOVE 'ELRETR'   TO W-FILE-ID
00577                  MOVE 'R'        TO PI-FILE-ID-IND
00578              ELSE
00579                  MOVE 'ELMSTR'   TO W-FILE-ID
00580                  MOVE 'M'        TO PI-FILE-ID-IND
00581          ELSE
00582              MOVE ER-0970        TO EMI-ERROR
00583              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00584              MOVE AL-UABON       TO CFILEIDA
00585              MOVE -1             TO CFILEIDL
00586              GO TO 8110-SEND-DATA
00587      ELSE
00588          MOVE 'M'                TO CFILEIDO
00589                                     PI-FILE-ID-IND
00590          MOVE +1                 TO CFILEIDL
00591          MOVE 'ELMSTR'           TO W-FILE-ID.
00592
00593      IF ASEXI > LOW-VALUES
00594          MOVE ASEXI TO WS-SEX-SELECTION
00595          IF WS-SEX-SELECTION = 'M' OR 'F'
00596              MOVE AL-UANON       TO ASEXA
00597          ELSE
00598             MOVE ER-0219         TO EMI-ERROR
00599             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00600             MOVE AL-UABON        TO ASEXA
00601             MOVE -1              TO ASEXL
00602      ELSE
00603         MOVE AL-UANOF            TO ASEXA.
00604
00605      IF CARRSI > LOW-VALUES
00606          MOVE CARRSI   TO CARRIER-CNTL
00607          MOVE AL-UANON TO CARRSA
00608      ELSE
00609          MOVE AL-UANOF TO CARRSA
00610          MOVE SPACES   TO CARRIER-CNTL.
00611
00612      MOVE SPACE TO ERROR-SWITCH.
00613
00614      IF INCLSI > SPACES
00615          MOVE INCLSI TO TEST-DATE
00616          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
00617          IF NO-CONVERSION-ERROR
00618             MOVE DC-GREG-DATE-1-EDIT TO INCLSI
00619             MOVE DC-BIN-DATE-1       TO INC-DATE-LOW-CNTL
00620             MOVE AL-UANON            TO INCLSA
00621          ELSE
00622             MOVE -1                  TO INCLSL
00623             MOVE AL-UABON            TO INCLSA
00624             MOVE LOW-VALUES          TO INC-DATE-LOW-CNTL
00625      ELSE
00626          MOVE AL-UANOF               TO INCLSA
00627          MOVE LOW-VALUES             TO INC-DATE-LOW-CNTL.
00628
00629      IF INCHSI > SPACES
00630          MOVE INCHSI TO TEST-DATE
00631          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
00632          IF NO-CONVERSION-ERROR
00633             MOVE DC-GREG-DATE-1-EDIT TO INCHSI
00634             MOVE DC-BIN-DATE-1       TO INC-DATE-HIGH-CNTL
00635             MOVE AL-UANON            TO INCHSA
00636          ELSE
00637             MOVE -1                  TO INCHSL
00638             MOVE AL-UABON            TO INCHSA
00639             MOVE HIGH-VALUES         TO INC-DATE-HIGH-CNTL
00640      ELSE
00641          MOVE AL-UANOF               TO INCHSA
00642          MOVE HIGH-VALUES            TO INC-DATE-HIGH-CNTL.
00643
00644      IF INC-DATE-HIGH-CNTL < INC-DATE-LOW-CNTL
00645          MOVE 'X'      TO BUILD-SWITCH
00646          MOVE ER-0308  TO EMI-ERROR
00647          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00648          MOVE -1       TO INCLSL
00649          MOVE AL-UABON TO INCLSA INCHSA.
00650
00651      IF GRPSI > LOW-VALUES
00652          MOVE GRPSI    TO GROUP-CNTL
00653          MOVE AL-UANON TO GRPSA
00654      ELSE
00655          MOVE AL-UANOF TO GRPSA.
00656
00657      MOVE SPACE TO ERROR-SWITCH.
00658
00659      IF PMTDLSI > SPACES
00660          MOVE PMTDLSI TO TEST-DATE
00661          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
00662          IF NO-CONVERSION-ERROR
00663             MOVE DC-GREG-DATE-1-EDIT TO PMTDLSI
00664             MOVE DC-BIN-DATE-1       TO LST-PMT-LOW-CNTL
00665             MOVE AL-UANON            TO PMTDLSA
00666          ELSE
00667             MOVE -1                  TO PMTDLSL
00668             MOVE AL-UABON            TO PMTDLSA
00669             MOVE LOW-VALUES          TO LST-PMT-LOW-CNTL
00670      ELSE
00671          MOVE AL-UANOF               TO PMTDLSA
00672          MOVE LOW-VALUES             TO LST-PMT-LOW-CNTL.
00673
00674      IF PMTDHSI > SPACES
00675          MOVE PMTDHSI TO TEST-DATE
00676          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
00677          IF NO-CONVERSION-ERROR
00678             MOVE DC-GREG-DATE-1-EDIT TO PMTDHSI
00679             MOVE DC-BIN-DATE-1       TO LST-PMT-HIGH-CNTL
00680             MOVE AL-UANON            TO PMTDHSA
00681          ELSE
00682             MOVE -1                  TO PMTDHSL
00683             MOVE AL-UABON            TO PMTDHSA
00684             MOVE HIGH-VALUES         TO LST-PMT-HIGH-CNTL
00685      ELSE
00686          MOVE AL-UANOF               TO PMTDHSA
00687          MOVE HIGH-VALUES            TO LST-PMT-HIGH-CNTL.
00688
00689      IF LST-PMT-HIGH-CNTL < LST-PMT-LOW-CNTL
00690          MOVE 'X'      TO BUILD-SWITCH
00691          MOVE ER-0308  TO EMI-ERROR
00692          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00693          MOVE -1       TO PMTDLSL
00694          MOVE AL-UABON TO PMTDLSA PMTDHSA.
00695
00696      IF STATESI > LOW-VALUES
00697          MOVE STATESI  TO STATE-CNTL
00698          MOVE AL-UANON TO STATESA
00699      ELSE
00700          MOVE AL-UANOF TO STATESA.
00701
00702      MOVE SPACE TO ERROR-SWITCH.
00703
00704      IF OPENLSI = LOW-VALUES
00705          MOVE AL-UANOF     TO OPENLSA
00706          MOVE ZEROS        TO MO-OPEN-LOW-CNTL
00707      ELSE
00708          IF OPENLSI NOT NUMERIC
00709              MOVE 'X'      TO ERROR-SWITCH BUILD-SWITCH
00710              MOVE ZEROS    TO MO-OPEN-LOW-CNTL
00711          ELSE
00712              MOVE OPENLSI  TO MO-OPEN-LOW-CNTL
00713              MOVE AL-UANON TO OPENLSA.
00714
00715      IF SCREEN-ERROR
00716          MOVE -1       TO OPENLSL
00717          MOVE AL-UABON TO OPENLSA
00718          MOVE ER-0306  TO EMI-ERROR
00719          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00720
00721      MOVE SPACE TO ERROR-SWITCH.
00722
00723      IF OPENHSI = LOW-VALUES
00724          MOVE AL-UANOF     TO OPENHSA
00725          MOVE 9999         TO MO-OPEN-HIGH-CNTL
00726      ELSE
00727          IF OPENHSI NOT NUMERIC
00728              MOVE 'X'      TO ERROR-SWITCH BUILD-SWITCH
00729              MOVE 9999 TO MO-OPEN-HIGH-CNTL
00730          ELSE
00731              MOVE OPENHSI  TO MO-OPEN-HIGH-CNTL
00732              MOVE AL-UANON TO OPENHSA.
00733
00734      IF SCREEN-ERROR
00735          MOVE -1       TO OPENHSL
00736          MOVE AL-UABON TO OPENHSA
00737          MOVE ER-0306  TO EMI-ERROR
00738          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00739
00740      IF MO-OPEN-HIGH-CNTL < MO-OPEN-LOW-CNTL
00741          MOVE 'X'      TO BUILD-SWITCH
00742          MOVE ER-0308  TO EMI-ERROR
00743          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00744          MOVE -1       TO OPENLSL
00745          MOVE AL-UABON TO OPENLSA OPENHSA.
00746
00747      IF ACCTSI > LOW-VALUES
00748          MOVE ACCTSI   TO ACCOUNT-CNTL
00749          MOVE AL-UANON TO ACCTSA
00750      ELSE
00751          MOVE AL-UANOF TO ACCTSA.
00752
00753      MOVE SPACE TO ERROR-SWITCH.
00754
00755      IF AMTLSI > LOW-VALUES
00756          MOVE AMTLSI TO TEST-AMT
00757          PERFORM 1170-DEEDIT-AMT THRU 1170-EXIT
00758          MOVE -1       TO AMTLSL
00759      ELSE
00760          MOVE AL-UANOF TO AMTLSA
00761          MOVE ZEROS  TO AMT-PAID-LOW-CNTL.
00762
00763      IF SCREEN-ERROR
00764          MOVE -1           TO AMTLSL
00765          MOVE AL-UABON     TO AMTLSA
00766          MOVE ZEROS        TO AMT-PAID-LOW-CNTL
00767      ELSE
00768          IF AMTLSI > LOW-VALUES
00769              MOVE AMT-BIF  TO AMT-PAID-LOW-CNTL AMTLSO
00770              MOVE AL-UANON TO AMTLSA.
00771
00772      MOVE SPACE TO ERROR-SWITCH.
00773
00774      IF AMTHSI > LOW-VALUES
00775          MOVE AMTHSI       TO TEST-AMT
00776          PERFORM 1170-DEEDIT-AMT THRU 1170-EXIT
00777          MOVE -1           TO AMTHSL
00778      ELSE
00779          MOVE AL-UANOF     TO AMTHSA
00780          MOVE ALL-NINES    TO AMT-PAID-HIGH-CNTL.
00781
00782      IF SCREEN-ERROR
00783          MOVE -1           TO AMTHSL
00784          MOVE AL-UABON     TO AMTHSA
00785          MOVE ALL-NINES    TO AMT-PAID-HIGH-CNTL
00786      ELSE
00787          IF AMTHSI > LOW-VALUES
00788              MOVE AMT-BIF  TO AMT-PAID-HIGH-CNTL AMTHSO
00789              MOVE AL-UANON TO AMTHSA.
00790
00791      IF AMT-PAID-HIGH-CNTL < AMT-PAID-LOW-CNTL
00792          MOVE 'X'          TO BUILD-SWITCH
00793          MOVE ER-0308      TO EMI-ERROR
00794          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00795          MOVE -1           TO AMTLSL
00796          MOVE AL-UABON     TO AMTLSA AMTHSA.
00797
00798      MOVE SPACE TO ERROR-SWITCH.
00799
00800      IF TYPESI > LOW-VALUES
121802        IF TYPESI = PI-LIFE-OVERRIDE-L1 OR
121802                    PI-AH-OVERRIDE-L1   OR
052614*                   'I' OR 'G' OR 'F'
100518*                   'I' OR 'G' OR 'F' OR 'O'
080322                    'I' OR 'G' OR 'F' OR 'O' OR 'B' OR 'H'
00802            MOVE AL-UANON         TO TYPESA
00803            MOVE TYPESI           TO TYPE-CNTL
00804         ELSE
00805            MOVE AL-UABON         TO TYPESA
00806            MOVE -1               TO TYPESL
00807            MOVE 'X'              TO BUILD-SWITCH
00808            MOVE ER-0199          TO EMI-ERROR
00809            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00810      ELSE
00811         MOVE AL-UANOF            TO TYPESA.
00812
00813      IF CAUSELSI > SPACES
00814          MOVE CAUSELSI   TO CAUSE-CD-LOW-CNTL
00815      ELSE
00816          MOVE LOW-VALUES TO CAUSE-CD-LOW-CNTL.
00817
00818      IF CAUSEHSI > SPACES
00819          MOVE CAUSEHSI    TO CAUSE-CD-HIGH-CNTL
00820      ELSE
00821          MOVE HIGH-VALUES TO CAUSE-CD-HIGH-CNTL.
00822
00823      IF CAUSE-CD-HIGH-CNTL < CAUSE-CD-LOW-CNTL
00824          MOVE 'X'       TO BUILD-SWITCH
00825          MOVE ER-0308   TO EMI-ERROR
00826          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00827          MOVE -1        TO CAUSELSL
00828          MOVE AL-UABON  TO CAUSELSA CAUSEHSA.
00829
00830      MOVE SPACE TO ERROR-SWITCH.
00831
00832      IF DENSI > LOW-VALUES
00833          MOVE DENSI        TO TEST-RESP
00834          IF TEST-RESP = 'Y' OR 'N' OR SPACE
00835              MOVE AL-UANON TO DENSA
00836              MOVE DENSI    TO DEN-CNTL
00837          ELSE
00838             MOVE 'X'       TO BUILD-SWITCH
00839             MOVE ER-0046   TO EMI-ERROR
00840             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00841             MOVE AL-UABON  TO DENSA
00842             MOVE -1        TO DENSL
00843      ELSE
00844         MOVE AL-UANOF      TO DENSA.
00845
00846      IF REPLSI > SPACES
00847          MOVE REPLSI TO TEST-DATE
00848          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
00849          IF NO-CONVERSION-ERROR
00850             MOVE DC-GREG-DATE-1-EDIT TO REPLSI
00851             MOVE DC-BIN-DATE-1       TO REP-DATE-LOW-CNTL
00852             MOVE AL-UANON            TO REPLSA
00853            ELSE
00854             MOVE -1                  TO REPLSL
00855             MOVE AL-UABON            TO REPLSA
00856             MOVE LOW-VALUES          TO REP-DATE-LOW-CNTL
00857      ELSE
00858          MOVE AL-UANOF               TO REPLSA
00859          MOVE LOW-VALUES             TO REP-DATE-LOW-CNTL.
00860
00861      IF REPHSI > SPACES
00862          MOVE REPHSI                 TO TEST-DATE
00863          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
00864          IF NO-CONVERSION-ERROR
00865             MOVE DC-GREG-DATE-1-EDIT TO REPHSI
00866             MOVE DC-BIN-DATE-1       TO REP-DATE-HIGH-CNTL
00867             MOVE AL-UANON            TO REPHSA
00868            ELSE
00869             MOVE -1                  TO REPHSL
00870             MOVE AL-UABON            TO REPHSA
00871             MOVE HIGH-VALUES         TO REP-DATE-HIGH-CNTL
00872      ELSE
00873          MOVE AL-UANOF               TO REPHSA
00874          MOVE HIGH-VALUES            TO REP-DATE-HIGH-CNTL.
00875
00876      IF REP-DATE-HIGH-CNTL < REP-DATE-LOW-CNTL
00877          MOVE 'X'      TO BUILD-SWITCH
00878          MOVE ER-0308  TO EMI-ERROR
00879          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00880          MOVE -1       TO REPLSL
00881          MOVE AL-UABON TO REPLSA REPHSA.
00882
00883      MOVE SPACE            TO ERROR-SWITCH.
00884
00885      IF PROCSI > LOW-VALUES
00886          PERFORM 1060-CHECK-PROC THRU 1080-EXIT.
00887
00888      IF SCREEN-ERROR
00889          MOVE AL-UABON     TO PROCSA
00890          MOVE -1           TO PROCSL
00891      ELSE
00892          IF PROCSI = LOW-VALUES
00893              MOVE AL-UANOF TO PROCSA
00894          ELSE
00895              MOVE AL-UANON TO PROCSA
00896              MOVE PROCSI   TO PROC-CNTL.
00897
00898      MOVE SPACE TO ERROR-SWITCH.
00899
00900      IF PMTLSI > LOW-VALUES
00901          MOVE PMTLSI   TO TEST-AMT
00902          PERFORM 1170-DEEDIT-AMT THRU 1170-EXIT
00903          MOVE -1       TO PMTLSL
00904      ELSE
00905          MOVE AL-UANOF TO PMTHSA
00906          MOVE ZEROES   TO LST-PAID-LOW-CNTL.
00907
00908      IF SCREEN-ERROR
00909          MOVE -1       TO PMTLSL
00910          MOVE AL-UABON TO PMTLSA
00911          MOVE ZEROES   TO LST-PAID-LOW-CNTL
00912      ELSE
00913          IF PMTLSI > LOW-VALUES
00914              MOVE AMT-BIF  TO LST-PAID-LOW-CNTL PMTLSO
00915              MOVE AL-UANON TO PMTLSA.
00916
00917      MOVE SPACE TO ERROR-SWITCH.
00918
00919      IF PMTHSI > LOW-VALUES
00920          MOVE PMTHSI    TO TEST-AMT
00921          PERFORM 1170-DEEDIT-AMT THRU 1170-EXIT
00922      ELSE
00923          MOVE AL-UANOF  TO PMTHSA
00924          MOVE ALL-NINES TO LST-PAID-HIGH-CNTL.
00925
00926      IF SCREEN-ERROR
00927          MOVE -1        TO PMTHSL
00928          MOVE AL-UABON  TO PMTHSA
00929          MOVE ALL-NINES TO LST-PAID-HIGH-CNTL
00930      ELSE
00931          IF PMTHSI > LOW-VALUES
00932              MOVE AMT-BIF  TO LST-PAID-HIGH-CNTL PMTHSO
00933              MOVE AL-UANON TO PMTHSA.
00934
00935      IF LST-PAID-HIGH-CNTL < LST-PAID-LOW-CNTL
00936          MOVE 'X'      TO BUILD-SWITCH
00937          MOVE ER-0308  TO EMI-ERROR
00938          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00939          MOVE -1       TO PMTLSL
00940          MOVE AL-UABON TO PMTLSA PMTHSA.
00941
00942      MOVE SPACE TO ERROR-SWITCH.
00943
00944      IF PREMSI > LOW-VALUES
00945         IF PREMSI = SPACE OR
00946           (PREMSI > '0' AND < '4')
00947              MOVE AL-UANON       TO PREMSA
00948              MOVE PREMSI         TO PREM-CNTL
00949         ELSE
00950            MOVE 'X'              TO BUILD-SWITCH
00951            MOVE ER-0227          TO EMI-ERROR
00952            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00953            MOVE AL-UABON         TO PREMSA
00954            MOVE -1               TO PREMSL
00955      ELSE
00956         MOVE AL-UANOF            TO PREMSA.
00957
00958      MOVE SPACE TO ERROR-SWITCH.
00959
00960      IF MNTLSI > SPACES
00961          MOVE MNTLSI TO TEST-DATE
00962          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
00963          IF NO-CONVERSION-ERROR
00964             MOVE DC-GREG-DATE-1-EDIT         TO MNTLSI
00965             MOVE DC-BIN-DATE-1   TO MNT-DATE-LOW-CNTL
00966             MOVE AL-UANON        TO MNTLSA
00967            ELSE
00968             MOVE -1              TO MNTLSL
00969             MOVE AL-UABON        TO MNTLSA
00970             MOVE LOW-VALUES      TO MNT-DATE-LOW-CNTL
00971      ELSE
00972          MOVE AL-UANOF   TO MNTLSA
00973          MOVE LOW-VALUES TO MNT-DATE-LOW-CNTL.
00974
00975      IF MNTHSI > SPACES
00976          MOVE MNTHSI TO TEST-DATE
00977          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
00978          IF NO-CONVERSION-ERROR
00979             MOVE DC-GREG-DATE-1-EDIT TO MNTHSI
00980             MOVE DC-BIN-DATE-1       TO MNT-DATE-HIGH-CNTL
00981             MOVE AL-UANON            TO MNTHSA
00982            ELSE
00983             MOVE -1                  TO MNTHSL
00984             MOVE AL-UABON            TO MNTHSA
00985             MOVE HIGH-VALUES         TO MNT-DATE-HIGH-CNTL
00986      ELSE
00987          MOVE AL-UANOF TO MNTHSA
00988          MOVE HIGH-VALUES TO MNT-DATE-HIGH-CNTL.
00989
00990      IF MNT-DATE-HIGH-CNTL < MNT-DATE-LOW-CNTL
00991          MOVE 'X'       TO BUILD-SWITCH
00992          MOVE ER-0308   TO EMI-ERROR
00993          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00994          MOVE -1        TO MNTLSL
00995          MOVE AL-UABON  TO MNTLSA MNTHSA.
00996
00997      MOVE SPACE TO ERROR-SWITCH.
00998
00999      IF REQSI > LOW-VALUES
01000          MOVE REQSI TO TEST-RESP
01001          IF TEST-RESP = 'Y' OR 'N' OR SPACE
01002              MOVE AL-UANON       TO REQSA
01003              MOVE REQSI          TO REQ-CNTL
01004          ELSE
01005             MOVE 'X'             TO  BUILD-SWITCH
01006             MOVE ER-0046         TO EMI-ERROR
01007             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01008             MOVE AL-UABON        TO REQSA
01009             MOVE -1              TO REQSL
01010      ELSE
01011         MOVE AL-UANOF            TO REQSA.
01012
01013      MOVE SPACE TO ERROR-SWITCH.
01014
01015      IF ESTLSI > SPACES
01016          MOVE ESTLSI TO TEST-DATE
01017          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
01018          IF NO-CONVERSION-ERROR
01019             MOVE DC-GREG-DATE-1-EDIT TO ESTLSI
01020             MOVE DC-BIN-DATE-1       TO EST-DATE-LOW-CNTL
01021             MOVE AL-UANON            TO ESTLSA
01022             ELSE
01023             MOVE -1                  TO ESTLSL
01024             MOVE AL-UABON            TO ESTLSA
01025             MOVE LOW-VALUES          TO EST-DATE-LOW-CNTL
01026      ELSE
01027          MOVE AL-UANOF   TO ESTLSA
01028          MOVE LOW-VALUES TO EST-DATE-LOW-CNTL.
01029
01030      IF ESTHSI > SPACES
01031          MOVE ESTHSI TO TEST-DATE
01032          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
01033          IF NO-CONVERSION-ERROR
01034             MOVE DC-GREG-DATE-1-EDIT TO ESTHSI
01035             MOVE DC-BIN-DATE-1       TO EST-DATE-HIGH-CNTL
01036             MOVE AL-UANON            TO ESTHSA
01037             ELSE
01038             MOVE -1                  TO ESTHSL
01039             MOVE AL-UABON            TO ESTHSA
01040             MOVE HIGH-VALUES         TO EST-DATE-HIGH-CNTL
01041      ELSE
01042          MOVE AL-UANOF               TO ESTHSA
01043          MOVE HIGH-VALUES            TO EST-DATE-HIGH-CNTL.
01044
01045      IF MNT-DATE-HIGH-CNTL < MNT-DATE-LOW-CNTL
01046          MOVE 'X'      TO BUILD-SWITCH
01047          MOVE ER-0308  TO EMI-ERROR
01048          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01049          MOVE -1       TO MNTLSL
01050          MOVE AL-UABON TO MNTLSA MNTHSA.
01051
01052      MOVE SPACE TO ERROR-SWITCH.
01053
01054      IF SUPRSI > LOW-VALUES
01055          MOVE SUPRSI TO TEST-RESP
01056          IF TEST-RESP = 'Y' OR 'N' OR SPACE
01057              MOVE AL-UANON       TO SUPRSA
01058              MOVE SUPRSI         TO SUPR-CNTL
01059          ELSE
01060             MOVE 'X'             TO  BUILD-SWITCH
01061             MOVE ER-0046         TO EMI-ERROR
01062             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01063             MOVE AL-UABON        TO SUPRSA
01064             MOVE -1              TO SUPRSL
01065      ELSE
01066         MOVE AL-UANOF            TO SUPRSA.
01067
01068      MOVE SPACE TO ERROR-SWITCH.
01069
01070      IF FOLLSI > SPACES
01071          MOVE FOLLSI TO TEST-DATE
01072          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
01073          IF NO-CONVERSION-ERROR
01074             MOVE DC-GREG-DATE-1-EDIT TO FOLLSI
01075             MOVE DC-BIN-DATE-1       TO FOL-DATE-LOW-CNTL
01076             MOVE AL-UANON            TO FOLLSA
01077            ELSE
01078             MOVE -1                  TO FOLLSL
01079             MOVE AL-UABON            TO FOLLSA
01080             MOVE LOW-VALUES          TO FOL-DATE-LOW-CNTL
01081      ELSE
01082          MOVE AL-UANOF               TO FOLLSA
01083          MOVE LOW-VALUES             TO FOL-DATE-LOW-CNTL.
01084
01085      MOVE SPACE TO ERROR-SWITCH.
01086
01087      IF FOLHSI > SPACES
01088          MOVE FOLHSI TO TEST-DATE
01089          PERFORM 1130-DEEDIT-DATE THRU 1140-EXIT
01090          IF NO-CONVERSION-ERROR
01091             MOVE DC-GREG-DATE-1-EDIT TO FOLHSI
01092             MOVE DC-BIN-DATE-1       TO FOL-DATE-HIGH-CNTL
01093             MOVE AL-UANON            TO FOLHSA
01094             ELSE
01095             MOVE -1                  TO FOLHSL
01096             MOVE AL-UABON            TO FOLHSA
01097             MOVE HIGH-VALUES         TO FOL-DATE-HIGH-CNTL
01098      ELSE
01099          MOVE AL-UANOF               TO FOLHSA
01100          MOVE HIGH-VALUES            TO FOL-DATE-HIGH-CNTL.
01101
01102      IF FOL-DATE-HIGH-CNTL < FOL-DATE-LOW-CNTL
01103          MOVE 'X'     TO BUILD-SWITCH
01104          MOVE ER-0308 TO EMI-ERROR
01105          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01106          MOVE -1      TO FOLLSL
01107          MOVE AL-UABON TO FOLLSA FOLHSA.
01108
01109      MOVE SPACE TO ERROR-SWITCH.
01110
01111      IF CERTSI > LOW-VALUES
01112          MOVE CERTSI TO TEST-RESP
01113          IF TEST-RESP = 'Y' OR 'N' OR SPACE
01114              MOVE AL-UANON       TO CERTSA
01115              MOVE CERTSI         TO CERT-CNTL
01116          ELSE
01117             MOVE 'X'             TO  BUILD-SWITCH
01118             MOVE ER-0046         TO EMI-ERROR
01119             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01120             MOVE AL-UABON        TO CERTSA
01121             MOVE -1              TO CERTSL
01122      ELSE
01123         MOVE AL-UANOF            TO CERTSA.
01124
01125      MOVE SPACE TO ERROR-SWITCH.
01126
01127      IF DAYSLSI = LOW-VALUES
01128          MOVE AL-UANOF TO DAYSLSA
01129          MOVE ZEROES   TO DAYS-LOW-CNTL
01130      ELSE
01131          IF DAYSLSI NOT NUMERIC
01132              MOVE ZEROS TO DAYS-LOW-CNTL
01133              MOVE 'X' TO ERROR-SWITCH BUILD-SWITCH
01134          ELSE
01135              MOVE DAYSLSI  TO DAYS-LOW-CNTL
01136              MOVE AL-UANON TO DAYSLSA.
01137
01138      IF SCREEN-ERROR
01139          MOVE -1       TO DAYSLSL
01140          MOVE AL-UABON TO DAYSLSA
01141          MOVE ER-0307  TO EMI-ERROR
01142          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01143
01144      MOVE SPACE TO ERROR-SWITCH.
01145
01146      IF DAYSHSI = LOW-VALUES
01147          MOVE AL-UANOF     TO DAYSHSA
01148          MOVE 9999         TO DAYS-HIGH-CNTL
01149      ELSE
01150          IF DAYSHSI NOT NUMERIC
01151              MOVE 9999 TO DAYS-HIGH-CNTL
01152              MOVE 'X'      TO ERROR-SWITCH BUILD-SWITCH
01153          ELSE
01154              MOVE DAYSHSI  TO DAYS-HIGH-CNTL
01155              MOVE AL-UANON TO DAYSHSA.
01156
01157      IF SCREEN-ERROR
01158          MOVE -1       TO DAYSHSL
01159          MOVE AL-UABON TO DAYSHSA
01160          MOVE ER-0307  TO EMI-ERROR
01161          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01162
01163      IF DAYS-HIGH-CNTL < DAYS-LOW-CNTL
01164          MOVE 'X'     TO BUILD-SWITCH
01165          MOVE ER-0308 TO EMI-ERROR
01166          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01167          MOVE -1       TO DAYSLSL
01168          MOVE AL-UABON TO DAYSLSA DAYSHSA.
01169
01170      MOVE SPACE TO ERROR-SWITCH.
01171
01172      IF PRISI > LOW-VALUES
01173          IF PRISI = SPACE OR (PRISI > '0' AND
01174                               NOT   > '9')
01175              MOVE AL-UANON       TO PRISA
01176              MOVE PRISI          TO PRI-CNTL
01177          ELSE
01178             MOVE 'X'             TO  BUILD-SWITCH
01179             MOVE ER-0274         TO EMI-ERROR
01180             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01181             MOVE AL-UABON        TO PRISA
01182             MOVE -1              TO PRISL
01183      ELSE
01184         MOVE AL-UANOF            TO PRISA.
01185
01186
01187      MOVE SPACE TO ERROR-SWITCH.
01188
01189      IF AUTOSI > LOW-VALUES
01190          MOVE AUTOSI TO TEST-RESP
01191          IF TEST-RESP = 'Y' OR 'N' OR SPACE
01192              MOVE AL-UANON        TO AUTOSA
01193              MOVE AUTOSI          TO AUTO-CNTL
01194          ELSE
01195             MOVE 'X'             TO  BUILD-SWITCH
01196             MOVE ER-0046         TO EMI-ERROR
01197             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01198             MOVE AL-UABON        TO AUTOSA
01199             MOVE -1              TO AUTOSL
01200      ELSE
01201         MOVE AL-UANOF            TO AUTOSA.
01202
01203      MOVE SPACE TO ERROR-SWITCH.
01204
01205      IF OPCLSI > LOW-VALUES
01206          MOVE OPCLSI TO TEST-RESP
01207          IF TEST-RESP = 'O' OR 'C' OR 'R' OR SPACE
01208              MOVE AL-UANON       TO OPCLSA
01209              MOVE OPCLSI         TO OPCL-CNTL
01210          ELSE
01211             MOVE 'X'             TO  BUILD-SWITCH
01212             MOVE ER-0767         TO EMI-ERROR
01213             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01214             MOVE AL-UABON        TO OPCLSA
01215             MOVE -1              TO OPCLSL
01216      ELSE
01217         MOVE AL-UANOF            TO OPCLSA.
01218
01219
01220      MOVE SPACE TO ERROR-SWITCH.
01221
01222      IF PRTOPTI > LOW-VALUES
01223          IF PRTOPTI = 'N' OR 'L'
01224              MOVE AL-UANON       TO PRTOPTA
01225              MOVE PRTOPTI        TO PI-PRINT-OPTION
01226          ELSE
01227             MOVE 'X'             TO BUILD-SWITCH
01228             MOVE ER-0334         TO EMI-ERROR
01229             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01230             MOVE AL-UABON        TO PRTOPTA
01231             MOVE -1              TO PRTOPTL
01232      ELSE
01233         MOVE 'L'                 TO PI-PRINT-OPTION
01234         MOVE AL-UANOF            TO PRTOPTA.
01235
01236      MOVE SPACE TO ERROR-SWITCH.
01237
01238      IF FMTOPTI > LOW-VALUES
01239          IF FMTOPTI = 'F' OR 'P'
01240              MOVE AL-UANON       TO FMTOPTA
01241              MOVE FMTOPTI        TO PI-FORMAT-OPTION
01242          ELSE
01243             MOVE 'X'             TO BUILD-SWITCH
01244             MOVE ER-0335         TO EMI-ERROR
01245             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01246             MOVE AL-UABON        TO FMTOPTA
01247             MOVE -1              TO FMTOPTL
01248      ELSE
01249         MOVE AL-UANOF            TO FMTOPTA.
01250
01251      IF ALTPRTI > LOW-VALUES
01252          MOVE AL-UANON                   TO  ALTPRTA
01253          MOVE ALTPRTI                    TO  PI-ALT-PRINT-ID
01254      ELSE
01255          IF PI-PROCESSOR-PRINTER IS NOT EQUAL TO SPACES
01256              MOVE PI-PROCESSOR-PRINTER   TO  PI-ALT-PRINT-ID
01257          ELSE
01258              MOVE SPACES                 TO  PI-ALT-PRINT-ID.
01259
01260      IF SCREEN-HAS-ERRORS
01261          GO TO 1010-EXIT.
01262
01263      MOVE SAVE-BIN-DATE TO CURRENT-DATE-BIN.
01264
01265  1010-EXIT.
01266      EXIT.
01267      EJECT
01268
01269  1060-CHECK-PROC.
01270      IF PROCSI = SPACES
01271          GO TO 1080-EXIT.
01272
01273      MOVE PI-COMPANY-ID TO COMPANY-ID.
01274      MOVE '2'           TO RECORD-TYPE.
01275      MOVE PROCSI        TO ACCESS-CD-GENL.
01276      MOVE ZEROS         TO SEQUENCE-NO.
01277
01278      
      * EXEC CICS HANDLE CONDITION
01279 *        NOTFND  (1070-PROC-NOTFND)
01280 *        NOTOPEN (6030-CNTL-NOT-OPEN)
01281 *    END-EXEC.
      *    MOVE '"$IJ                  ! & #00007427' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303037343237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01282
01283      PERFORM 5120-READ-CNTL THRU 5130-EXIT.
01284
01285      MOVE 'X' TO PROC-SWITCH.
01286      GO TO 1080-EXIT.
01287
01288  1070-PROC-NOTFND.
01289
01290      MOVE 'X' TO ERROR-SWITCH BUILD-SWITCH.
01291      MOVE ER-0273 TO EMI-ERROR.
01292      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01293      MOVE -1 TO CARRSL.
01294
01295  1080-EXIT.
01296      EXIT.
01297
01298  1130-DEEDIT-DATE.
01299      
      * EXEC CICS BIF DEEDIT
01300 *        FIELD  (TEST-DATE)
01301 *        LENGTH (DATE-LENGTH)
01302 *    END-EXEC.
      *    MOVE '@"L                   #   #00007448' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEST-DATE, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01303
01304      MOVE '4'      TO DC-OPTION-CODE.
01305      MOVE BIF-DATE TO DC-GREG-DATE-1-MDY.
01306      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
01307      IF DATE-CONVERSION-ERROR
01308          MOVE 'X'     TO BUILD-SWITCH
01309          MOVE ER-0304 TO EMI-ERROR
01310          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01311
01312  1140-EXIT.
01313      EXIT.
01314
01315  1170-DEEDIT-AMT.
01316      
      * EXEC CICS BIF DEEDIT
01317 *        FIELD  (TEST-AMT)
01318 *        LENGTH (AMT-LENGTH)
01319 *    END-EXEC.
      *    MOVE '@"L                   #   #00007465' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEST-AMT, 
                 AMT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01320
01321      IF TEST-AMT NUMERIC
01322          GO TO 1170-EXIT.
01323
01324      MOVE 'X'     TO ERROR-SWITCH BUILD-SWITCH.
01325      MOVE ER-0419 TO EMI-ERROR.
01326      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01327
01328  1170-EXIT.
01329      EXIT.
01330      EJECT
01331  2000-BUILD-TS.
01332      MOVE ZEROS                  TO COUNT-1.
01333      MOVE PI-COMPANY-CD          TO MSTR-COMPANY-CODE.
01334      MOVE CARRIER-CNTL           TO MSTR-CARRIER
01335      MOVE SPACES                 TO REST-OF-KEY BUILD-SWITCH.
01336
01337      PERFORM 5000-START-BROWSE THRU 5020-EXIT.
01338
01339      IF NO-RECORDS
01340          GO TO 2010-EXIT.
01341
01342      PERFORM 5030-READ-FILE THRU 5050-EXIT.
01343
01344      PERFORM 3000-EDIT-RECORDS THRU 3020-EXIT
01345          UNTIL BUILD-COMPLETE.
01346
01347      PERFORM 5060-END-BROWSE THRU 5070-EXIT.
01348
01349  2010-EXIT.
01350      EXIT.
01351      EJECT
01352  3000-EDIT-RECORDS.
01353      MOVE SPACE TO ERROR-SWITCH.
01354
01355      IF  PI-COMPANY-ID = 'DMD'
01356              AND
01357          SETUP-ERRORS
01358          GO TO 3010-GET-NEXT.
01359
01360      IF WS-SEX-SELECTION NOT = SPACES
01361         IF WS-SEX-SELECTION NOT = CL-INSURED-SEX-CD
01362             GO TO 3010-GET-NEXT.
01363
01364      IF PROC-CNTL NOT = SPACES
01365         IF PROC-CNTL NOT = CL-PROCESSOR-ID
01366             GO TO 3010-GET-NEXT.
01367
01368      IF CARRIER-CNTL NOT = SPACES
01369         IF CARRIER-CNTL NOT = CL-CARRIER
01370             MOVE 'Y'             TO BUILD-SWITCH
01371             GO TO 3020-EXIT.
01372
01373      IF GROUP-CNTL NOT = SPACES
01374         IF GROUP-CNTL NOT = CL-CERT-GROUPING
01375          GO TO 3010-GET-NEXT.
01376
01377      IF STATE-CNTL NOT = SPACES
01378         IF STATE-CNTL NOT = CL-CERT-STATE
01379          GO TO 3010-GET-NEXT.
01380
01381      IF ACCOUNT-CNTL NOT = SPACES
01382          IF ACCOUNT-CNTL NOT = CL-CERT-ACCOUNT
01383          GO TO 3010-GET-NEXT.
01384
01385      IF TYPE-CNTL NOT = SPACES
01386          IF TYPE-CNTL NOT = CL-CLAIM-TYPE
01387          GO TO 3010-GET-NEXT.
01388
01389      IF DEN-CNTL NOT = SPACES
01390          PERFORM 3080-CHECK-DEN THRU 3080-EXIT.
01391
01392      IF SCREEN-ERROR
01393          GO TO 3010-GET-NEXT.
01394
01395      IF PREM-CNTL NOT = SPACES
01396         IF PREM-CNTL NOT = CL-CLAIM-PREM-TYPE
01397          GO TO 3010-GET-NEXT.
01398
01399      IF REQ-CNTL NOT = SPACES
01400          PERFORM 3110-CHECK-REQ THRU 3110-EXIT.
01401
01402      IF SCREEN-ERROR
01403          GO TO 3010-GET-NEXT.
01404
01405      IF SUPR-CNTL NOT = SPACES
01406          PERFORM 3120-CHECK-SUPR THRU 3120-EXIT.
01407
01408      IF SCREEN-ERROR
01409          GO TO 3010-GET-NEXT.
01410
01411      IF CERT-CNTL NOT = SPACES
01412          PERFORM 3130-CHECK-CERT THRU 3130-EXIT.
01413
01414      IF SCREEN-ERROR
01415          GO TO 3010-GET-NEXT.
01416
01417      IF PRI-CNTL NOT = SPACES
01418         IF PRI-CNTL NOT = CL-PRIORITY-CD
01419          GO TO 3010-GET-NEXT.
01420
01421      IF AUTO-CNTL NOT = SPACES
01422          PERFORM 3150-CHECK-AUTO THRU 3150-EXIT.
01423
01424      IF SCREEN-ERROR
01425          GO TO 3010-GET-NEXT.
01426
01427      IF OPCL-CNTL NOT = SPACES
01428          PERFORM 3180-CHECK-OPCL THRU 3180-EXIT.
01429
01430      IF SCREEN-ERROR
01431          GO TO 3010-GET-NEXT.
01432
01433      IF CL-INCURRED-DT < INC-DATE-LOW-CNTL
01434          GO TO 3010-GET-NEXT.
01435
01436      IF CL-INCURRED-DT > INC-DATE-HIGH-CNTL
01437          GO TO 3010-GET-NEXT.
01438
01439      IF CL-LAST-PMT-DT < LST-PMT-LOW-CNTL
01440          GO TO 3010-GET-NEXT.
01441
01442      IF CL-LAST-PMT-DT > LST-PMT-HIGH-CNTL
01443          GO TO 3010-GET-NEXT.
01444
01445      MOVE SPACE TO ERROR-SWITCH.
01446
01447      IF OPENLSI = LOW-VALUES
01448          NEXT SENTENCE
01449      ELSE
01450          PERFORM 3160-CALC-ELAPSED-MONTHS THRU 3160-EXIT
01451          IF DC-ELAPSED-MONTHS <       MO-OPEN-LOW-CNTL OR
01452             DC-ELAPSED-MONTHS > MO-OPEN-HIGH-CNTL
01453                 GO TO 3010-GET-NEXT.
01454
01455      IF SCREEN-ERROR
01456          GO TO 3010-GET-NEXT.
01457
01458      IF AMTLSI > LOW-VALUES
01459          IF CL-TOTAL-PAID-AMT < AMT-PAID-LOW-CNTL
01460              GO TO 3010-GET-NEXT.
01461
01462      IF AMTHSI > LOW-VALUES
01463          IF CL-TOTAL-PAID-AMT > AMT-PAID-HIGH-CNTL
01464              GO TO 3010-GET-NEXT.
01465
01466      IF CAUSELSI > SPACES
01467          IF CL-CAUSE-CD < CAUSE-CD-LOW-CNTL
01468              GO TO 3010-GET-NEXT.
01469
01470      IF CAUSEHSI > SPACES
01471          IF CL-CAUSE-CD > CAUSE-CD-HIGH-CNTL
01472              GO TO 3010-GET-NEXT.
01473
01474      IF REPLSI > SPACES
01475          IF CL-REPORTED-DT < REP-DATE-LOW-CNTL
01476              GO TO 3010-GET-NEXT.
01477
01478      IF REPHSI > SPACES
01479          IF CL-REPORTED-DT > REP-DATE-HIGH-CNTL
01480              GO TO 3010-GET-NEXT.
01481
01482      IF PMTLSI > LOW-VALUES
01483          IF CL-LAST-PMT-AMT < LST-PAID-LOW-CNTL
01484              GO TO 3010-GET-NEXT.
01485
01486      IF PMTHSI > LOW-VALUES
01487          IF CL-LAST-PMT-AMT > LST-PAID-HIGH-CNTL
01488              GO TO 3010-GET-NEXT.
01489
01490      IF MNTLSI > SPACES
01491          IF CL-LAST-MAINT-DT < MNT-DATE-LOW-CNTL
01492              GO TO 3010-GET-NEXT.
01493
01494      IF MNTHSI > SPACES
01495          IF CL-LAST-MAINT-DT > MNT-DATE-HIGH-CNTL
01496              GO TO 3010-GET-NEXT.
01497
01498      IF ESTLSI > SPACES
01499          IF CL-FILE-ESTABLISH-DT < EST-DATE-LOW-CNTL
01500              GO TO 3010-GET-NEXT.
01501
01502      IF ESTHSI > SPACES
01503          IF CL-FILE-ESTABLISH-DT > EST-DATE-HIGH-CNTL
01504              GO TO 3010-GET-NEXT.
01505
01506      IF FOLLSI > SPACES
01507          IF CL-NEXT-FOLLOWUP-DT < FOL-DATE-LOW-CNTL
01508              GO TO 3010-GET-NEXT.
01509
01510      IF FOLHSI > SPACES
01511          IF CL-NEXT-FOLLOWUP-DT > FOL-DATE-HIGH-CNTL
01512              GO TO 3010-GET-NEXT.
01513
01514      PERFORM 3170-CALC-ELAPSED-DAYS THRU 3170-EXIT.
01515
01516      IF SCREEN-ERROR
01517          GO TO 3010-GET-NEXT.
01518
01519      IF DC-ELAPSED-DAYS < DAYS-LOW-CNTL
01520          GO TO 3010-GET-NEXT.
01521
01522      IF DC-ELAPSED-DAYS > DAYS-HIGH-CNTL
01523          GO TO 3010-GET-NEXT.
01524
01525      PERFORM 4000-BUILD-160B THRU 4000-EXIT.
01526
01527  3010-GET-NEXT.
01528      PERFORM 5030-READ-FILE THRU 5050-EXIT.
01529
01530      IF CL-COMPANY-CD NOT = PI-COMPANY-CD
01531          MOVE 'Y' TO BUILD-SWITCH.
01532
01533      IF COUNT-1 = MAX-TS-PAGES
01534         MOVE 'Y'                 TO BUILD-SWITCH.
01535
01536  3020-EXIT.
01537      EXIT.
01538
01539  3080-CHECK-DEN.
01540      IF DEN-CNTL = 'N'    AND
01541         NOT CLAIM-DENIED
01542          GO TO 3080-EXIT.
01543
01544      IF DEN-CNTL = 'Y'    AND
01545         CLAIM-DENIED
01546          GO TO 3080-EXIT.
01547
01548 ******************************************************************
01549 *    IF DENIED CLAIMS ARE SELECTED AND THE LAST CLOSE REASON ON  *
01550 *    THE CLAIM MASTER IS NOT 'DENIED', THE PROGRAM READS THE     *
01551 *    ZERO TRAILER AND CHECKS THE OPEN/CLOSE HISTORY TO SEE IF    *
01552 *    THE CLAIM HAS EVER BEEN DENIED TO DETERMINE IF IT MEETS     *
01553 *    THE SELECTION CRITERIA.                                     *
01554 ******************************************************************
01555
01556      IF DEN-CNTL = 'Y'
01557        IF CL-PURGED-DT EQUAL LOW-VALUES
01558          MOVE CL-CONTROL-PRIMARY     TO  TRLR-MAIN-KEY
01559          MOVE +0                     TO  TRLR-SEQ-NO
01560          PERFORM 5100-READ-TRLR THRU 5110-EXIT
01561          IF ERROR-SWITCH IS EQUAL TO 'X'
01562            GO TO 3080-EXIT
01563          ELSE
01564            IF (AT-OPEN-CLOSE-REASON (1) = 'DENIE' OR 'DENIL') OR
01565               (AT-OPEN-CLOSE-REASON (2) = 'DENIE' OR 'DENIL') OR
01566               (AT-OPEN-CLOSE-REASON (3) = 'DENIE' OR 'DENIL') OR
01567               (AT-OPEN-CLOSE-REASON (4) = 'DENIE' OR 'DENIL') OR
01568               (AT-OPEN-CLOSE-REASON (5) = 'DENIE' OR 'DENIL') OR
01569               (AT-OPEN-CLOSE-REASON (6) = 'DENIE' OR 'DENIL')
01570                GO TO 3080-EXIT.
01571
01572      MOVE 'X' TO ERROR-SWITCH.
01573
01574  3080-EXIT.
01575      EXIT.
01576
01577  3110-CHECK-REQ.
01578      IF REQ-CNTL = 'Y' AND
01579         CL-NEXT-FOLLOWUP-DT > LOW-VALUES
01580          GO TO 3110-EXIT.
01581
01582      IF REQ-CNTL = 'N' AND
01583         CL-NEXT-FOLLOWUP-DT = LOW-VALUES
01584          GO TO 3110-EXIT.
01585
01586      MOVE 'X' TO ERROR-SWITCH.
01587
01588  3110-EXIT.
01589      EXIT.
01590
01591  3120-CHECK-SUPR.
01592      IF SUPR-CNTL = 'Y' AND
01593         CL-SUPV-ATTN-CD = 'Y'
01594          GO TO 3120-EXIT.
01595
01596      IF SUPR-CNTL = 'N' AND
01597         (CL-SUPV-ATTN-CD = 'N' OR SPACE)
01598          GO TO 3120-EXIT.
01599
01600      MOVE 'X' TO ERROR-SWITCH.
01601
01602  3120-EXIT.
01603      EXIT.
01604
01605  3130-CHECK-CERT.
01606      IF CERT-CNTL = 'Y' AND
01607         CERT-WAS-CREATED
01608          GO TO 3130-EXIT.
01609
01610      IF CERT-CNTL = 'N' AND
01611         NOT CERT-WAS-CREATED
01612          GO TO 3130-EXIT.
01613
01614      MOVE 'X' TO ERROR-SWITCH.
01615
01616  3130-EXIT.
01617      EXIT.
01618
01619  3150-CHECK-AUTO.
01620      IF AUTO-CNTL = 'Y' AND
01621         CL-AUTO-PAY-SEQ > ZERO
01622          GO TO 3150-EXIT.
01623
01624      IF AUTO-CNTL = 'N' AND
01625         CL-AUTO-PAY-SEQ = ZERO
01626          GO TO 3150-EXIT.
01627
01628      MOVE 'X' TO ERROR-SWITCH.
01629
01630  3150-EXIT.
01631      EXIT.
01632
01633  3160-CALC-ELAPSED-MONTHS.
01634      MOVE CURRENT-DATE-BIN      TO DC-BIN-DATE-2.
01635
01636      IF CL-LAST-REOPEN-DT = LOW-VALUES
01637          MOVE CL-REPORTED-DT    TO DC-BIN-DATE-1
01638      ELSE
01639          MOVE CL-LAST-REOPEN-DT TO DC-BIN-DATE-1.
01640
01641      MOVE '1'          TO DC-OPTION-CODE.
01642      MOVE SPACE        TO DC-ERROR-CODE.
01643      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
01644      IF DATE-CONVERSION-ERROR
01645          MOVE 'X' TO ERROR-SWITCH.
01646
01647  3160-EXIT.
01648      EXIT.
01649
01650  3170-CALC-ELAPSED-DAYS.
01651      IF CL-LAST-PMT-DT = LOW-VALUES
01652          MOVE ZERO TO DC-ELAPSED-DAYS
01653          GO TO 3170-EXIT.
01654
01655      MOVE CURRENT-DATE-BIN TO DC-BIN-DATE-2.
01656      MOVE CL-LAST-PMT-DT   TO DC-BIN-DATE-1.
01657      MOVE '1'              TO DC-OPTION-CODE.
01658      MOVE SPACE            TO DC-ERROR-CODE.
01659      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
01660      IF DATE-CONVERSION-ERROR
01661          MOVE 'X' TO ERROR-SWITCH.
01662
01663  3170-EXIT.
01664      EXIT.
01665
01666  3180-CHECK-OPCL.
01667      IF OPCL-CNTL = CL-CLAIM-STATUS
01668          GO TO 3180-EXIT.
01669
01670      IF OPCL-CNTL = 'R' AND CLAIM-IS-OPEN AND
01671         CL-LAST-REOPEN-DT  NOT = LOW-VALUES
01672            GO TO 3180-EXIT.
01673
01674      MOVE 'X' TO ERROR-SWITCH.
01675
01676  3180-EXIT.
01677      EXIT.
01678      EJECT
01679  4000-BUILD-160B.
01680
01681      MOVE LOW-VALUES             TO EL160BO.
01682      MOVE SPACES                 TO PI-CONTROL-IN-PROGRESS.
01683      PERFORM 4010-MOVE-MSTR THRU 4010-EXIT.
01684      MOVE CL-CONTROL-PRIMARY     TO TRLR-MAIN-KEY.
01685      MOVE +0                     TO TRLR-SEQ-NO.
01686      MOVE SPACE                  TO ERROR-SWITCH.
01687
01688      IF CL-PURGED-DT NOT EQUAL LOW-VALUES
01689         MOVE 'CLAIM IS PURGED ' TO CAUSEO
01690         MOVE AL-SABOF           TO CAUSEA
01691         GO TO 4000-BYPASS-TRLRS.
01692
01693      PERFORM 5100-READ-TRLR THRU 5110-EXIT.
01694
01695      IF SCREEN-ERROR
01696         MOVE ER-0205            TO SCNERRO
01697      ELSE
01698         MOVE CL-CONTROL-PRIMARY     TO TRLR-MAIN-KEY
01699         MOVE +90                    TO TRLR-SEQ-NO
01700         MOVE SPACE                  TO ERROR-SWITCH
01701         PERFORM 5100-READ-TRLR THRU 5110-EXIT
01702         IF NOT SCREEN-ERROR
01703            IF AT-TRAILER-TYPE EQUAL '6'
01704               MOVE AT-INFO-LINE-1       TO CAUSEO
01705               PERFORM 4020-MOVE-TRLR THRU 4020-EXIT.
01706
01707  4000-BYPASS-TRLRS.
01708
01709      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
01710          GO TO 4000-READ-EMPLCY.
01711
01712      MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.
01713      MOVE CL-CERT-CARRIER        TO CERT-CARRIER PI-CARRIER.
01714      MOVE CL-CERT-GROUPING       TO CERT-GROUP.
01715      MOVE CL-CERT-STATE          TO CERT-STATE.
01716      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
01717      MOVE CL-CERT-NO             TO CERT-CERT PI-CERT-NO.
01718      MOVE CL-CERT-EFF-DT         TO CERT-DATE.
01719      MOVE SPACE                  TO ERROR-SWITCH.
01720
01721      PERFORM 5080-READ-CERT THRU 5090-EXIT.
01722
01723      IF SCREEN-ERROR
01724          MOVE ER-0206            TO SCNERRO
01725      ELSE
01726          PERFORM 4030-MOVE-CERT THRU 4030-EXIT.
01727
01728      MOVE PI-CONTROL-IN-PROGRESS TO PIKEYO.
01729      MOVE PI-UPDATE-BY           TO USERSAVO.
01730      MOVE PI-UPDATE-HHMMSS       TO TIMESAVO.
01731
01732      PERFORM 5140-WRITE-TS THRU 5150-EXIT.
01733
01734      GO TO 4000-EXIT.
01735
01736  4000-READ-EMPLCY.
01737
01738      MOVE CL-COMPANY-CD          TO  EMPLCY-COMPANY-CD.
01739      MOVE CL-CERT-CARRIER        TO  EMPLCY-CARRIER
01740                                      PI-CARRIER.
01741      MOVE CL-CERT-GROUPING       TO  EMPLCY-GROUPING.
01742      MOVE CL-CERT-STATE          TO  EMPLCY-STATE.
01743      MOVE CL-CERT-ACCOUNT        TO  EMPLCY-PRODUCER.
01744      MOVE CL-CERT-EFF-DT         TO  EMPLCY-EFF-DT.
01745      MOVE CL-CV-REFERENCE-NO     TO  EMPLCY-REFERENCE-NO
01746                                      PI-MP-REFERENCE-NO.
01747      MOVE CL-CERT-NO             TO  PI-CERT-NO.
01748      MOVE SPACE                  TO  ERROR-SWITCH.
01749
01750      PERFORM 5095-READ-EMPLCY THRU 5095-EXIT.
01751
01752      IF SCREEN-ERROR
01753          MOVE ER-9483            TO  SCNERRO
01754          GO TO 4000-EXIT.
01755
01756      MOVE PM-COMPANY-CD          TO  EMPLAN-COMPANY-CD.
01757      MOVE PM-CARRIER             TO  EMPLAN-CARRIER.
01758      MOVE PM-GROUPING            TO  EMPLAN-GROUPING.
01759      MOVE PM-STATE               TO  EMPLAN-STATE.
01760      MOVE PM-PRODUCER            TO  EMPLAN-PRODUCER.
01761      MOVE PM-INS-PLAN-CD         TO  EMPLAN-PLAN-CODE.
01762      MOVE PM-INS-PLAN-REVISION   TO  EMPLAN-REV-NO.
01763
01764      PERFORM 5096-READ-EMPLAN THRU 5096-EXIT.
01765
01766      IF SCREEN-ERROR
01767          MOVE ER-9811            TO  SCNERRO
01768      ELSE
01769          PERFORM 4030-MOVE-EMPLCY THRU 4030-EMPLCY-EXIT.
01770
01771      MOVE PI-CONTROL-IN-PROGRESS TO  PIKEYO.
01772      MOVE PI-UPDATE-BY           TO  USERSAVO.
01773      MOVE PI-UPDATE-HHMMSS       TO  TIMESAVO.
01774
01775      PERFORM 5140-WRITE-TS THRU 5150-EXIT.
01776
01777  4000-EXIT.
01778      EXIT.
01779      EJECT
01780  4010-MOVE-MSTR.
01781      MOVE CL-CLAIM-NO            TO CLAIMO PI-CLAIM-NO.
01782      MOVE CL-CLAIM-TYPE          TO TYPEO.
01783      MOVE CL-CERT-PRIME          TO CERTO.
01784      MOVE CL-CERT-SFX            TO CERTSXO.
01785      MOVE CL-CCN                 TO CREDCDO.
01786      MOVE CL-CERT-CARRIER        TO CARRO.
01787      MOVE CL-CLAIM-STATUS        TO STATUSO.
01788      MOVE CL-PROCESSOR-ID        TO PROCO.
01789      MOVE CL-INSURED-LAST-NAME   TO MLNAMEO.
01790      MOVE CL-INSURED-1ST-NAME    TO MFNAMEO.
01791      MOVE CL-INSURED-MID-INIT    TO MMINITO.
01792      MOVE CL-INSURED-SEX-CD      TO SEXO.
01793
01794      IF CL-INSURED-BIRTH-DT > LOW-VALUES
01795          MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1
01796          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01797          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01798          MOVE DC-GREG-DATE-1-EDIT TO BIRTHO
01799      ELSE
01800          MOVE SPACES TO BIRTHO.
01801
01802      IF CL-SSN-STATE   = CL-CERT-STATE AND
01803         CL-SSN-ACCOUNT = CL-CERT-ACCOUNT-PRIME
01804          MOVE SPACES        TO SOCIALO
01805      ELSE
01806          MOVE CL-SOC-SEC-NO TO SOCIALO.
01807
01808      MOVE CL-INSURED-OCC-CD    TO OCCO.
01809      MOVE CL-BENEFICIARY       TO CBENEO.
01810      MOVE CL-CAUSE-CD          TO CCAUSCDO.
01811
01812      IF CL-EST-END-OF-DISAB-DT > LOW-VALUES
01813          MOVE CL-EST-END-OF-DISAB-DT TO DC-BIN-DATE-1
01814          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01815          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01816          MOVE DC-GREG-DATE-1-EDIT TO ENDO
01817      ELSE
01818          MOVE SPACES TO ENDO.
01819
01820      IF CL-PAID-THRU-DT > LOW-VALUES
01821         IF NOT PI-USES-PAID-TO
01822            MOVE CL-PAID-THRU-DT      TO  DC-BIN-DATE-1
01823            MOVE SPACES               TO  DC-OPTION-CODE
01824                                          DC-ERROR-CODE
01825            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01826            MOVE DC-GREG-DATE-1-EDIT  TO  PDTHRUO
01827         ELSE
01828            MOVE WS-PAID-TO-HDG       TO  BHEADO
01829            MOVE CL-PAID-THRU-DT      TO  DC-BIN-DATE-1
01830            MOVE '6'                  TO  DC-OPTION-CODE
01831            MOVE +1                   TO  DC-ELAPSED-DAYS
01832            MOVE +0                   TO  DC-ELAPSED-MONTHS
01833            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01834            MOVE DC-GREG-DATE-1-EDIT  TO  PDTHRUO
01835      ELSE
01836         MOVE SPACES                  TO  PDTHRUO
01837         IF PI-USES-PAID-TO
01838            MOVE WS-PAID-TO-HDG       TO  BHEADO.
01839
01840      MOVE CL-TOTAL-PAID-AMT  TO PDAMTO.
01841      MOVE CL-NO-OF-DAYS-PAID TO NODAYSO.
01842      MOVE CL-NO-OF-PMTS-MADE TO NOPMTSO.
01843
01844      IF CL-INCURRED-DT > LOW-VALUES
01845          MOVE CL-INCURRED-DT TO DC-BIN-DATE-1
01846          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01847          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01848          MOVE DC-GREG-DATE-1-EDIT TO INCO
01849      ELSE
01850          MOVE SPACES TO INCO.
01851
01852      IF CL-REPORTED-DT > LOW-VALUES
01853          MOVE CL-REPORTED-DT TO DC-BIN-DATE-1
01854          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01855          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01856          MOVE DC-GREG-DATE-1-EDIT TO REPO
01857      ELSE
01858          MOVE SPACES TO REPO.
01859
01860      IF CL-FILE-ESTABLISH-DT > LOW-VALUES
01861          MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1
01862          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01863          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01864          MOVE DC-GREG-DATE-1-EDIT TO ESTO
01865      ELSE
01866          MOVE SPACES TO ESTO.
01867
01868 *    IF CL-LAST-PMT-DT > LOW-VALUES
01869 *        MOVE CL-LAST-PMT-DT TO DC-BIN-DATE-1
01870 *        MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01871 *        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01872 *        MOVE DC-GREG-DATE-1-EDIT TO LSTPMTO
01873 *    ELSE
01874 *        MOVE SPACES TO LSTPMTO.
01875
01876 *    MOVE CL-LAST-PMT-AMT TO LSTAMTO.
01877
01878      IF CL-LAST-MAINT-DT > LOW-VALUES
01879          MOVE CL-LAST-MAINT-DT TO DC-BIN-DATE-1
01880          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01881          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01882          MOVE DC-GREG-DATE-1-EDIT TO MNTDTO
01883      ELSE
01884          MOVE SPACES TO MNTDTO.
01885
01886      IF CL-LAST-MAINT-TYPE = SPACE
01887          MOVE 'SET UP' TO MNTTYPEO
01888      ELSE
01889          IF CL-LAST-MAINT-TYPE = '1'
01890              MOVE 'PAYMNT' TO MNTTYPEO
01891          ELSE
01892              IF CL-LAST-MAINT-TYPE = '2'
01893                  MOVE 'LETTER' TO MNTTYPEO
01894              ELSE
01895                  IF CL-LAST-MAINT-TYPE = '3'
01896                      MOVE 'UPDATE' TO MNTTYPEO
01897                  ELSE
01898                      IF CL-LAST-MAINT-TYPE = '4'
01899                          MOVE 'RESTOR' TO MNTTYPEO
01900                      ELSE
01901                          IF CL-LAST-MAINT-TYPE = '5'
01902                              MOVE 'INC DT' TO MNTTYPEO
01903                          ELSE
01904                              IF CL-LAST-MAINT-TYPE = '6'
01905                                  MOVE ' CONV'  TO MNTTYPEO
01906                              ELSE
01907                                  MOVE SPACES TO MNTTYPEO.
01908
01909      MOVE CL-PRIORITY-CD       TO PRICDO.
01910      MOVE CL-SUPV-ATTN-CD      TO SUPVO.
01911      MOVE CL-FILE-LOCATION     TO FILEO.
01912      MOVE CL-LAST-MAINT-USER   TO PI-UPDATE-BY.
01913      MOVE CL-LAST-MAINT-HHMMSS TO PI-UPDATE-HHMMSS.
01914
01915  4010-EXIT.
01916      EXIT.
01917      EJECT
01918  4020-MOVE-TRLR.
01919
01920  4020-EXIT.
01921      EXIT.
01922      EJECT
01923  4030-MOVE-CERT.
01924      IF CM-CERT-EFF-DT > LOW-VALUES
01925          MOVE CM-CERT-EFF-DT TO DC-BIN-DATE-1
01926          MOVE SPACES TO DC-OPTION-CODE DC-ERROR-CODE
01927          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01928          MOVE DC-GREG-DATE-1-EDIT TO CERTEFFO
01929      ELSE
01930          MOVE SPACES              TO CERTEFFO.
01931
01932      MOVE CM-ACCOUNT              TO CERTACTO.
01933      MOVE CM-STATE                TO CERTSTO PI-STATE.
01934      MOVE CM-GROUPING             TO CERTGRPO.
01935      MOVE CM-CARRIER              TO CERTCARO.
01936      MOVE CM-INSURED-LAST-NAME    TO CLNAMEO.
01937      MOVE CM-INSURED-FIRST-NAME   TO CFNAMEO.
01938      MOVE CM-INSURED-INITIAL2     TO CINITO.
01939      MOVE CM-JT-LAST-NAME         TO CJLNAMEO.
01940      MOVE CM-JT-FIRST-NAME        TO CJFAMEO.
01941      MOVE CM-JT-INITIAL           TO CJINITO.
01942
01943      MOVE CM-INSURED-ISSUE-AGE    TO INSAGEO.
01944      MOVE CM-INSURED-JOINT-AGE    TO JAGEO.
01945      IF CM-SSN-STATE   = CM-STATE AND
01946         CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME
01947          MOVE SPACES       TO SOCSECO
01948      ELSE
01949         MOVE CM-SOC-SEC-NO TO SOCSECO.
01950
01951      PERFORM 9300-GET-FREE-LOOK THRU 9300-EXIT.
01952
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
121802         CONTINUE
01955      ELSE
01956          GO TO 4030-MOVE-CERT-AH.
01957
01958      IF CM-LF-BENEFIT-CD = '00'
01959          GO TO 4030-MOVE-REST-OF-CERT.
01960
01961      MOVE CM-LF-BENEFIT-CD       TO CVCDO HOLD-BENEFIT
01962      MOVE PI-LIFE-OVERRIDE-L6    TO CVDESCRO.
01963      MOVE CM-LF-ORIG-TERM        TO CP-ORIGINAL-TERM
01964                                     CVOTRMO.
01965      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.
01966      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
01967      MOVE CL-INCURRED-DT         TO CP-VALUATION-DT.
01968      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
01969      MOVE '4'                    TO CP-REM-TERM-METHOD.
01970      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
01971      PERFORM 9700-LINK-REM-TERM THRU 9700-EXIT.
01972      MOVE CP-REMAINING-TERM-3    TO CVRTRMO.
01973      MOVE CM-LF-BENEFIT-AMT      TO CVOBENEO.
01974      MOVE CM-POLICY-FORM-NO      TO CVFORMO.
01975
01976      IF CM-LF-CURRENT-STATUS = '8'
01977         IF CM-LF-CANCEL-DT NOT = LOW-VALUES
01978             MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1
01979             MOVE ' '             TO DC-OPTION-CODE
01980             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01981             IF NOT DATE-CONVERSION-ERROR
01982                 MOVE DC-GREG-DATE-1-EDIT TO CVCNCDTO.
01983
01984      IF CM-LF-CURRENT-STATUS = '7'
01985         IF CM-LF-DEATH-DT NOT = LOW-VALUES
01986             MOVE CM-LF-DEATH-DT     TO DC-BIN-DATE-1
01987             MOVE SPACE              TO DC-OPTION-CODE
01988             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01989             IF NOT DATE-CONVERSION-ERROR
01990                 MOVE DC-GREG-DATE-1-EDIT TO CVCNCDTO.
01991
01992      IF CM-LF-CURRENT-STATUS = '8'
01993         IF CM-LF-CANCEL-EXIT-DT NOT = LOW-VALUES
01994             MOVE CM-LF-CANCEL-EXIT-DT TO DC-BIN-DATE-1
01995             MOVE SPACE                TO DC-OPTION-CODE
01996             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
01997             IF NOT DATE-CONVERSION-ERROR
01998                 MOVE DC-GREG-DATE-1-EDIT TO CVEXITO.
01999
02000      IF CM-LF-CURRENT-STATUS = '7'
02001         IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES
02002             MOVE CM-LF-DEATH-EXIT-DT  TO DC-BIN-DATE-1
02003             MOVE SPACE                TO DC-OPTION-CODE
02004             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
02005             IF NOT DATE-CONVERSION-ERROR
02006                 MOVE DC-GREG-DATE-1-EDIT TO CVEXITO.
02007
02008      IF CM-LF-CURRENT-STATUS = '1' OR '4'
02009         IF CP-REMAINING-TERM-3 = ZERO
02010            MOVE 'EXPIRED'           TO CVSTATO
02011         ELSE
02012            MOVE 'ACTIVE'            TO CVSTATO.
02013
02014      IF CM-LF-CURRENT-STATUS = '2'
02015         MOVE 'PEND   '           TO CVSTATO.
02016      IF CM-LF-CURRENT-STATUS = '3'
02017         MOVE 'RESTORE'           TO CVSTATO.
02018      IF CM-LF-CURRENT-STATUS = '5'
02019         MOVE 'REISSUE'           TO CVSTATO.
02020      IF CM-LF-CURRENT-STATUS = '6'
02021         MOVE 'LMP DIS'           TO CVSTATO.
02022      IF CM-LF-CURRENT-STATUS = '7'
02023         MOVE 'DEATH  '           TO CVSTATO.
02024      IF CM-LF-CURRENT-STATUS = '8'
02025         MOVE 'CANCEL '           TO CVSTATO.
02026      IF CM-LF-CURRENT-STATUS = '9'
02027         MOVE 'RE-ONLY'           TO CVSTATO.
02028      IF CM-LF-CURRENT-STATUS = 'V'
02029         MOVE 'VOID   '           TO CVSTATO.
02030      IF CM-LF-CURRENT-STATUS = 'D'
02031         MOVE 'DECLINE'           TO CVSTATO.
02032
02033      MOVE SPACES           TO BENEFIT-KEY ERROR-SWITCH.
02034      MOVE PI-COMPANY-ID    TO BEN-CO-ID.
02035      MOVE '4'              TO BEN-REC-TYPE.
02036      MOVE CM-LF-BENEFIT-CD TO BEN-ACC-CD.
02037      MOVE ZEROS            TO BEN-SEQ-NO.
02038
02039      PERFORM 5260-READ-BENEFIT THRU 5280-EXIT.
02040
02041      IF SCREEN-ERROR
02042          MOVE ER-0282 TO SCNERRO
02043          GO TO 4030-MOVE-CERT-AH.
02044
02045      MOVE ZEROS TO COUNT-2.
02046      PERFORM 4040-FIND-BENEFIT THRU 4060-EXIT.
02047      IF SCREEN-ERROR
02048          MOVE ER-0282 TO SCNERRO
02049          GO TO 4030-MOVE-CERT-AH.
02050
02051      MOVE CF-BENEFIT-ALPHA (COUNT-2) TO CVKINDO.
02052
02053  4030-MOVE-CERT-AH.
02054
052614     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
080322                                          OR 'B' OR 'H'
121802         CONTINUE
02057      ELSE
02058          GO TO 4030-MOVE-REST-OF-CERT.
02059
02060      IF CM-AH-BENEFIT-CD = '00'
02061          GO TO 4030-MOVE-REST-OF-CERT.
02062
02063      MOVE PI-AH-OVERRIDE-L6      TO CVDESCRO.
02064
02065      MOVE CM-AH-BENEFIT-CD TO CVCDO HOLD-BENEFIT.
02066      MOVE CM-AH-ORIG-TERM        TO CP-ORIGINAL-TERM
02067                                     CVOTRMO.
02068      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.
02069      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
02070      MOVE CURRENT-DATE-BIN       TO CP-VALUATION-DT.
02071      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
02072      MOVE '4'                    TO CP-REM-TERM-METHOD.
02073      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
02074
02075      PERFORM 9700-LINK-REM-TERM THRU 9700-EXIT.
02076
02077      MOVE CP-REMAINING-TERM-3    TO CVRTRMO.
02078      MOVE CM-AH-BENEFIT-AMT      TO CVOBENEO.
02079      MOVE CM-POLICY-FORM-NO      TO CVFORMO.
02080
02081      IF CM-AH-CURRENT-STATUS = '8'
02082         IF CM-AH-CANCEL-DT NOT = LOW-VALUES
02083             MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1
02084             MOVE ' '             TO DC-OPTION-CODE
02085             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
02086             IF NOT DATE-CONVERSION-ERROR
02087                 MOVE DC-GREG-DATE-1-EDIT TO CVCNCDTO.
02088
02089      IF CM-AH-CURRENT-STATUS = '6'
02090         IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
02091             MOVE CM-AH-SETTLEMENT-DT     TO DC-BIN-DATE-1
02092             MOVE ' '             TO DC-OPTION-CODE
02093             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
02094             IF NOT DATE-CONVERSION-ERROR
02095                 MOVE DC-GREG-DATE-1-EDIT TO CVCNCDTO.
02096
02097      IF CM-AH-CURRENT-STATUS = '8'
02098         IF CM-AH-CANCEL-EXIT-DT NOT = LOW-VALUES
02099             MOVE CM-AH-CANCEL-EXIT-DT TO DC-BIN-DATE-1
02100             MOVE ' '             TO DC-OPTION-CODE
02101             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
02102             IF NOT DATE-CONVERSION-ERROR
02103                 MOVE DC-GREG-DATE-1-EDIT TO CVEXITO.
02104
02105      IF CM-AH-CURRENT-STATUS = '6'
02106         IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES
02107             MOVE CM-AH-SETTLEMENT-EXIT-DT     TO DC-BIN-DATE-1
02108             MOVE ' '             TO DC-OPTION-CODE
02109             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
02110             IF NOT DATE-CONVERSION-ERROR
02111                 MOVE DC-GREG-DATE-1-EDIT TO CVEXITO.
02112
02113      IF CM-AH-CURRENT-STATUS = '1' OR = '4'
02114         IF CP-REMAINING-TERM-3 = ZEROS
02115            MOVE 'EXPIRED'           TO CVSTATO
02116         ELSE
02117            MOVE 'ACTIVE'            TO CVSTATO.
02118
02119      IF CM-AH-CURRENT-STATUS = '2'
02120         MOVE 'PEND   '           TO CVSTATO.
02121      IF CM-AH-CURRENT-STATUS = '3'
02122         MOVE 'RESTORE'           TO CVSTATO.
02123      IF CM-AH-CURRENT-STATUS = '5'
02124         MOVE 'REISSUE'           TO CVSTATO.
02125      IF CM-AH-CURRENT-STATUS = '6'
02126         MOVE 'LMP DIS'           TO CVSTATO.
02127      IF CM-AH-CURRENT-STATUS = '7'
02128         MOVE 'DEATH  '           TO CVSTATO.
02129      IF CM-AH-CURRENT-STATUS = '8'
02130         MOVE 'CANCEL '           TO CVSTATO.
02131      IF CM-AH-CURRENT-STATUS = '9'
02132         MOVE 'RE-ONLY'           TO CVSTATO.
02133      IF CM-AH-CURRENT-STATUS = 'V'
02134         MOVE 'VOID   '           TO CVSTATO.
02135      IF CM-AH-CURRENT-STATUS = 'D'
02136         MOVE 'DECLINE'           TO CVSTATO.
02137
02138      MOVE SPACES           TO BENEFIT-KEY ERROR-SWITCH.
02139      MOVE PI-COMPANY-ID    TO BEN-CO-ID.
02140      MOVE '5'              TO BEN-REC-TYPE.
02141      MOVE CM-AH-BENEFIT-CD TO BEN-ACC-CD.
02142      MOVE ZEROES           TO BEN-SEQ-NO.
02143
02144      PERFORM 5260-READ-BENEFIT THRU 5280-EXIT.
02145
02146      IF SCREEN-ERROR
02147          PERFORM 4070-CHECK-ERROR THRU 4080-EXIT
02148          GO TO 4030-MOVE-REST-OF-CERT.
02149
02150      MOVE ZEROS TO COUNT-2.
02151      PERFORM 4040-FIND-BENEFIT THRU 4060-EXIT.
02152
02153      IF SCREEN-ERROR
02154          PERFORM 4070-CHECK-ERROR THRU 4080-EXIT
02155          GO TO 4030-MOVE-REST-OF-CERT.
02156
02157      MOVE CF-BENEFIT-ALPHA (COUNT-2) TO CVKINDO.
02158
02159  4030-MOVE-REST-OF-CERT.
02160      MOVE CM-LOAN-NUMBER         TO LOANNOO.
02161      MOVE CM-LOAN-BALANCE        TO LOANBALO.
02162      MOVE CM-LOAN-APR            TO CAPRO.
02163      MOVE CM-IND-GRP-TYPE        TO CINDGRPO.
02164
02165      IF CM-SING-PRM
02166          MOVE 'SP'               TO CPREMTPO
02167      ELSE
02168          IF CM-O-B-COVERAGE
02169              MOVE 'OB'           TO CPREMTPO
02170          ELSE
02171              IF CM-OPEN-END
02172                  MOVE 'OE'       TO CPREMTPO
02173              ELSE
02174                  MOVE SPACES     TO CPREMTPO.
02175
02176      MOVE CM-REIN-TABLE          TO CREINCDO.
02177
02178  4030-EXIT.
02179      EXIT.
02180      EJECT
02181  4030-MOVE-EMPLCY.
02182
02183      IF PM-POLICY-EFF-DT > LOW-VALUES
02184          MOVE PM-POLICY-EFF-DT       TO  DC-BIN-DATE-1
02185          MOVE SPACES                 TO  DC-OPTION-CODE
02186                                          DC-ERROR-CODE
02187          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
02188          MOVE DC-GREG-DATE-1-EDIT    TO  CERTEFFO
02189      ELSE
02190          MOVE SPACES                 TO  CERTEFFO.
02191
02192      MOVE PM-PRODUCER             TO  CERTACTO.
02193      MOVE PM-STATE                TO  CERTSTO PI-STATE.
02194      MOVE PM-GROUPING             TO  CERTGRPO.
02195      MOVE PM-CARRIER              TO  CERTCARO.
02196      MOVE PM-INSURED-LAST-NAME    TO  CLNAMEO.
02197      MOVE PM-INSURED-FIRST-NAME   TO  CFNAMEO.
02198      MOVE PM-INSURED-MIDDLE-INIT  TO  CINITO.
02199      MOVE PM-JOINT-LAST-NAME      TO  CJLNAMEO.
02200      MOVE PM-JOINT-FIRST-NAME     TO  CJFAMEO.
02201      MOVE PM-JOINT-MIDDLE-INIT    TO  CJINITO.
02202
02203      MOVE PM-INSURED-ISSUE-AGE    TO  WS-AGE.
02204      MOVE WS-AGE-3-4              TO  INSAGEO.
02205      MOVE PM-JOINT-ISSUE-AGE      TO  WS-AGE.
02206      MOVE WS-AGE-3-4              TO  JAGEO.
02207
02208      IF PM-SSN-STATE   = PM-STATE AND
02209         PM-SSN-PRODUCER = PM-PRODUCER-PRIME
02210          MOVE SPACES              TO  SOCSECO
02211      ELSE
02212          MOVE PM-SOC-SEC-NO       TO  SOCSECO.
02213
02214      PERFORM 9300-GET-FREE-LOOK THRU 9300-EXIT.
02215
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
121802         CONTINUE
02218      ELSE
02219          GO TO 4030-MOVE-EMPLCY-AH.
02220
02221      IF PP-BENEFIT-IS-LEVEL
02222          MOVE 'L'                TO  CP-BENEFIT-TYPE
02223      ELSE
02224          MOVE 'R'                TO  CP-BENEFIT-TYPE.
02225
02226      MOVE PP-REFUND-CALC         TO  CP-EARNING-METHOD
02227                                      CP-RATING-METHOD.
02228
02229      MOVE PI-LIFE-OVERRIDE-L6    TO  CVDESCRO.
02230      MOVE PM-INS-PLAN-CD         TO  CVCDO
02231                                      HOLD-BENEFIT.
02232      MOVE PM-LOAN-TERM           TO  CP-ORIGINAL-TERM
02233                                      CVOTRMO.
02234      MOVE PM-POLICY-EFF-DT       TO  CP-CERT-EFF-DT
02235                                      CP-FIRST-PAY-DATE.
02236      MOVE CL-INCURRED-DT         TO  CP-VALUATION-DT.
02237      MOVE '1'                    TO  CP-REM-TRM-CALC-OPTION.
02238      MOVE '2'                    TO  CP-REM-TERM-METHOD
02239                                      CP-PROCESS-TYPE.
02240      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
02241      MOVE PM-COMPANY-CD          TO  CP-COMPANY-CD.
02242
02243      PERFORM 9700-LINK-REM-TERM THRU 9700-EXIT.
02244
02245      IF (PI-COMPANY-ID IS EQUAL TO 'CIG' OR 'CUK')
02246          COMPUTE CP-REMAINING-TERM-3 = CP-REMAINING-TERM-3 + 1
02247          MOVE CP-REMAINING-TERM-3    TO  CVRTRMO
02248      ELSE
02249          MOVE CP-REMAINING-TERM-3    TO  CVRTRMO.
02250
02251      MOVE PM-INS-TOTAL-BENEFIT   TO  CVOBENEO.
02252
02253      GO TO 4030-MOVE-REST-OF-EMPLCY.
02254
02255  4030-MOVE-EMPLCY-AH.
02256
02257      MOVE PI-AH-OVERRIDE-L6      TO CVDESCRO.
02258
02259      IF PP-BENEFIT-IS-LEVEL
02260          MOVE 'L'                TO  CP-BENEFIT-TYPE
02261      ELSE
02262          MOVE 'R'                TO  CP-BENEFIT-TYPE.
02263
02264      MOVE PP-REFUND-CALC         TO  CP-EARNING-METHOD
02265                                      CP-RATING-METHOD.
02266
02267      MOVE PI-AH-OVERRIDE-L6      TO  CVDESCRO.
02268      MOVE PM-INS-PLAN-CD         TO  CVCDO
02269                                      HOLD-BENEFIT.
02270      MOVE PM-LOAN-TERM           TO  CP-ORIGINAL-TERM
02271                                      CVOTRMO.
02272      MOVE PM-POLICY-EFF-DT       TO  CP-CERT-EFF-DT.
02273      MOVE PM-LOAN-DT             TO  CP-FIRST-PAY-DATE.
02274      MOVE CL-INCURRED-DT         TO  CP-VALUATION-DT.
02275      MOVE '1'                    TO  CP-REM-TRM-CALC-OPTION.
02276      MOVE '2'                    TO  CP-PROCESS-TYPE.
02277      MOVE '3'                    TO  CP-REM-TERM-METHOD.
02278      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
02279      MOVE PM-COMPANY-CD          TO  CP-COMPANY-CD.
02280
02281      PERFORM 9700-LINK-REM-TERM THRU 9700-EXIT.
02282
02283      MOVE CP-REMAINING-TERM-1    TO  CVRTRMO.
02284
02285      MOVE PM-INS-MONTH-BENEFIT   TO  CVOBENEO.
02286
02287  4030-MOVE-REST-OF-EMPLCY.
02288
02289      MOVE PM-INS-POLICY-FORM     TO  CVFORMO.
02290
02291      IF PM-CURRENT-STATUS IS EQUAL TO '7'
02292          IF PM-CANCEL-DT IS NOT EQUAL TO LOW-VALUES
02293              MOVE PM-CANCEL-DT           TO  DC-BIN-DATE-1
02294              MOVE SPACE                  TO  DC-OPTION-CODE
02295              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
02296              IF NOT DATE-CONVERSION-ERROR
02297                  MOVE DC-GREG-DATE-1-EDIT TO  CVCNCDTO.
02298
02299      IF (PM-EXIT-DT IS NOT EQUAL TO LOW-VALUES AND SPACES)
02300          MOVE ' '                        TO  DC-OPTION-CODE
02301          MOVE PM-EXIT-DT                 TO  DC-BIN-DATE-1
02302          MOVE SPACE                      TO  DC-OPTION-CODE
02303          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
02304          IF NOT DATE-CONVERSION-ERROR
02305              MOVE DC-GREG-DATE-1-EDIT    TO  CVEXITO.
02306
02307
02308      IF PM-CURRENT-STATUS IS EQUAL TO '0'
02309          MOVE 'LAPSED'           TO  CVSTATO.
02310      IF PM-CURRENT-STATUS IS EQUAL TO '1'
02311          MOVE 'ACTIVE'           TO  CVSTATO.
02312      IF PM-CURRENT-STATUS IS EQUAL TO '2'
02313         MOVE 'PEND   '           TO  CVSTATO.
02314      IF PM-CURRENT-STATUS IS EQUAL TO '3'
02315         MOVE 'DECLINE'           TO  CVSTATO.
02316      IF (PM-CURRENT-STATUS IS EQUAL TO '4' OR '9')
02317         MOVE 'PNDCNC'            TO  CVSTATO.
02318      IF PM-CURRENT-STATUS IS EQUAL TO '5'
02319         MOVE 'PNDISS'            TO  CVSTATO.
02320      IF PM-CURRENT-STATUS IS EQUAL TO '6'
02321         MOVE 'CLAIM'             TO  CVSTATO.
02322      IF PM-CURRENT-STATUS IS EQUAL TO '7'
02323         MOVE 'CANCEL '           TO  CVSTATO.
02324      IF PM-CURRENT-STATUS IS EQUAL TO '8'
02325         MOVE 'PNDUNW '           TO  CVSTATO.
02326      IF PM-CURRENT-STATUS IS EQUAL TO 'C'
02327         MOVE 'TRNSFR '           TO  CVSTATO.
02328      IF PM-CURRENT-STATUS IS EQUAL TO 'F'
02329         MOVE 'SETTLE '           TO  CVSTATO.
02330      IF PM-CURRENT-STATUS IS EQUAL TO 'T'
02331         MOVE 'TRMNAT '           TO  CVSTATO.
02332
02333      MOVE PP-PLAN-ABBREV         TO  CVKINDO.
02334
02335      MOVE PM-LOAN-NUMBER         TO  LOANNOO.
02336      MOVE PM-LOAN-BALC           TO  LOANBALO.
02337      MOVE PM-LOAN-APR            TO  CAPRO.
02338      MOVE PM-INS-TYPE            TO  CINDGRPO.
02339      MOVE PM-BILLING-MODE        TO  CPREMTPO.
02340
02341  4030-EMPLCY-EXIT.
02342      EXIT.
02343      EJECT
02344  4040-FIND-BENEFIT.
02345      ADD 1 TO COUNT-2.
02346
02347      IF COUNT-2 > 8
02348          GO TO 4050-BENEFIT-NOTFND.
02349
02350      IF CF-BENEFIT-CODE (COUNT-2) = HOLD-BENEFIT
02351          GO TO 4060-EXIT.
02352
02353      IF CF-BENEFIT-CODE (COUNT-2) > HOLD-BENEFIT
02354          GO TO 4050-BENEFIT-NOTFND.
02355
02356      GO TO 4040-FIND-BENEFIT.
02357
02358  4050-BENEFIT-NOTFND.
02359      MOVE 'X' TO ERROR-SWITCH.
02360
02361  4060-EXIT.
02362      EXIT.
02363      EJECT
02364
02365  4070-CHECK-ERROR.
02366      IF SCNERRO > SPACES
02367          GO TO 4080-EXIT.
02368
02369      MOVE ER-0283 TO SCNERRO.
02370
02371  4080-EXIT.
02372      EXIT.
02373
02374      EJECT
02375  5000-START-BROWSE.
02376      
      * EXEC CICS HANDLE CONDITION
02377 *        NOTFND (5010-BAD-KEY)
02378 *        NOTOPEN (6000-MSTR-NOT-OPEN)
02379 *    END-EXEC.
      *    MOVE '"$IJ                  ! '' #00008526' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303038353236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02380
02381      
      * EXEC CICS STARTBR
02382 *        DATASET (W-FILE-ID)
02383 *        RIDFLD (MSTR-KEY)
02384 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008531' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02385
02386      GO TO 5020-EXIT.
02387
02388  5010-BAD-KEY.
02389      MOVE 'X' TO BUILD-SWITCH.
02390
02391  5020-EXIT.
02392      EXIT.
02393
02394  5030-READ-FILE.
02395      
      * EXEC CICS HANDLE CONDITION
02396 *        ENDFILE (5040-END-OF-FILE)
02397 *    END-EXEC.
      *    MOVE '"$''                   ! ( #00008545' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303038353435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02398
02399      
      * EXEC CICS READNEXT
02400 *        INTO    (CLAIM-MASTER)
02401 *        DATASET (W-FILE-ID)
02402 *        RIDFLD  (MSTR-KEY)
02403 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00008549' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV12, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02404
02405      GO TO 5050-EXIT.
02406
02407  5040-END-OF-FILE.
02408      MOVE 'Y' TO BUILD-SWITCH.
02409
02410  5050-EXIT.
02411      EXIT.
02412
02413  5060-END-BROWSE.
02414      
      * EXEC CICS ENDBR
02415 *        DATASET (W-FILE-ID)
02416 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008564' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02417
02418  5070-EXIT.
02419      EXIT.
02420
02421  5080-READ-CERT.
02422      
      * EXEC CICS HANDLE CONDITION
02423 *        NOTFND (5085-CERT-NOTFND)
02424 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00008572' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303038353732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02425
02426      
      * EXEC CICS READ
02427 *        SET (ADDRESS OF CERTIFICATE-MASTER)
02428 *        DATASET ('ELCERT')
02429 *        RIDFLD (CERT-KEY)
02430 *    END-EXEC.
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008576' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02431
02432      GO TO 5090-EXIT.
02433
02434  5085-CERT-NOTFND.
02435      MOVE 'X' TO ERROR-SWITCH.
02436
02437  5090-EXIT.
02438      EXIT.
02439
02440  5095-READ-EMPLCY.
02441      
      * EXEC CICS HANDLE CONDITION
02442 *        NOTFND (5095-EMPLCY-NOTFND)
02443 *    END-EXEC.
      *    MOVE '"$I                   ! * #00008591' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303038353931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02444
02445      
      * EXEC CICS READ
02446 *        SET       (ADDRESS OF POLICY-MASTER)
02447 *        DATASET   ('MPPLCY')
02448 *        RIDFLD    (EMPLCY-KEY)
02449 *    END-EXEC.
           MOVE 'MPPLCY' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008595' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPLCY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02450
02451      GO TO 5095-EXIT.
02452
02453  5095-EMPLCY-NOTFND.
02454      MOVE 'X' TO ERROR-SWITCH.
02455
02456  5095-EXIT.
02457      EXIT.
02458
02459  5096-READ-EMPLAN.
02460      
      * EXEC CICS HANDLE CONDITION
02461 *        NOTFND (5096-EMPLAN-NOTFND)
02462 *    END-EXEC.
      *    MOVE '"$I                   ! + #00008610' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303038363130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02463
02464      
      * EXEC CICS READ
02465 *        SET       (ADDRESS OF PRODUCER-PLANS)
02466 *        DATASET   ('MPPLAN')
02467 *        RIDFLD    (EMPLAN-KEY)
02468 *    END-EXEC.
           MOVE 'MPPLAN' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008614' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-PLANS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02469
02470      GO TO 5096-EXIT.
02471
02472  5096-EMPLAN-NOTFND.
02473      MOVE 'X' TO ERROR-SWITCH.
02474
02475  5096-EXIT.
02476      EXIT.
02477
02478  5100-READ-TRLR.
02479      
      * EXEC CICS HANDLE CONDITION
02480 *        NOTFND (5105-TRLR-NOTFND)
02481 *    END-EXEC.
      *    MOVE '"$I                   ! , #00008629' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303038363239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02482
02483      
      * EXEC CICS READ
02484 *        SET (ADDRESS OF ACTIVITY-TRAILERS)
02485 *        DATASET ('ELTRLR')
02486 *        RIDFLD (TRLR-KEY)
02487 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008633' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02488
02489      GO TO 5110-EXIT.
02490
02491  5105-TRLR-NOTFND.
02492      MOVE 'X' TO ERROR-SWITCH.
02493
02494  5110-EXIT.
02495      EXIT.
02496
02497  5120-READ-CNTL.
02498      
      * EXEC CICS READ
02499 *        SET (ADDRESS OF CONTROL-FILE)
02500 *        DATASET ('ELCNTL')
02501 *        RIDFLD (CNTL-KEY)
02502 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008648' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02503
02504      MOVE CF-FORMS-PRINTER-ID TO PI-PRINT-ID.
02505
02506  5130-EXIT.
02507      EXIT.
02508
02509  5140-WRITE-TS.
02510      ADD 1 TO COUNT-1.
02511      
      * EXEC CICS WRITEQ TS
02512 *        QUEUE (PI-EL1602-KEY)
02513 *        FROM (EL160BO)
02514 *        LENGTH (EL160B-LENGTH)
02515 *        ITEM (COUNT-1)
02516 *    END-EXEC.
      *    MOVE '*" I   L              ''   #00008661' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL1602-KEY, 
                 EL160BO, 
                 EL160B-LENGTH, 
                 COUNT-1, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02517
02518  5150-EXIT.
02519      EXIT.
02520
02521  5240-WRITE-TS-160A.
02522      
      * EXEC CICS WRITEQ TS
02523 *        QUEUE (PI-EL160-KEY)
02524 *        FROM (EL160AO)
02525 *        LENGTH (EL160A-LENGTH)
02526 *    END-EXEC.
      *    MOVE '*"     L              ''   #00008672' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-EL160-KEY, 
                 EL160AO, 
                 EL160A-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02527
02528  5250-EXIT.
02529      EXIT.
02530
02531  5260-READ-BENEFIT.
02532      
      * EXEC CICS HANDLE CONDITION
02533 *        NOTFND (5270-BENEFIT-NOTFND)
02534 *    END-EXEC.
      *    MOVE '"$I                   ! - #00008682' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303038363832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02535
02536      
      * EXEC CICS READ
02537 *        SET (ADDRESS OF CONTROL-FILE)
02538 *        DATASET ('ELCNTL')
02539 *        RIDFLD (BENEFIT-KEY)
02540 *        GTEQ
02541 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00008686' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENEFIT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02542
02543      IF CF-RECORD-TYPE = BEN-REC-TYPE
02544          GO TO 5280-EXIT.
02545
02546  5270-BENEFIT-NOTFND.
02547      MOVE 'X' TO ERROR-SWITCH.
02548
02549  5280-EXIT.
02550      EXIT.
02551      EJECT
02552  6000-MSTR-NOT-OPEN.
02553      MOVE ER-0154 TO EMI-ERROR.
02554      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02555      MOVE -1 TO CARRSL.
02556      GO TO 8110-SEND-DATA.
02557
02558  6030-CNTL-NOT-OPEN.
02559      MOVE ER-0042 TO EMI-ERROR.
02560      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02561      MOVE -1 TO CARRSL.
02562      GO TO 8110-SEND-DATA.
02563
02564      EJECT
02565  8100-SEND-MAP.
02566      IF  PI-CARRIER-SECURITY > SPACES
02567          MOVE PI-CARRIER-SECURITY TO CARRSO
02568          MOVE AL-SANON            TO CARRSA.
02569
02570      IF  PI-ACCOUNT-SECURITY > SPACES
02571          MOVE PI-ACCOUNT-SECURITY TO ACCTSO
02572          MOVE AL-SANON            TO ACCTSA.
02573
02574      IF  PI-ACCOUNT-SECURITY > SPACES        OR
02575          PI-CARRIER-SECURITY > SPACES
02576          MOVE 'N'                TO PRTOPTO
02577          MOVE AL-SANON           TO PRTOPTA
02578          MOVE ER-2381         TO EMI-ERROR
02579          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02580
02581      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.
02582
02583      
      * EXEC CICS SEND
02584 *        MAP ('EL160A')
02585 *        MAPSET ('EL160S')
02586 *        ERASE
02587 *        FREEKB
02588 *        CURSOR
02589 *    END-EXEC.
           MOVE 'EL160A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL160S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00008733' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL160AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02590
02591      GO TO 9000-RETURN-TRANS.
02592
02593  8110-SEND-DATA.
02594      IF  PI-CARRIER-SECURITY > SPACES
02595          MOVE PI-CARRIER-SECURITY TO CARRSO
02596          MOVE AL-SANON            TO CARRSA.
02597
02598      IF  PI-ACCOUNT-SECURITY > SPACES
02599          MOVE PI-ACCOUNT-SECURITY TO ACCTSO
02600          MOVE AL-SANON            TO ACCTSA.
02601
02602      IF  PI-ACCOUNT-SECURITY > SPACES        OR
02603          PI-CARRIER-SECURITY > SPACES
02604          MOVE 'N'                TO PRTOPTO
02605          MOVE AL-SANON           TO PRTOPTA
02606          MOVE ER-2381         TO EMI-ERROR
02607          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02608
02609      PERFORM 8120-FORMAT-TIME-DATE
02610              THRU 8130-EXIT.
02611
02612      
      * EXEC CICS SEND
02613 *        MAP ('EL160A')
02614 *        MAPSET ('EL160S')
02615 *        DATAONLY
02616 *        FREEKB
02617 *        CURSOR
02618 *    END-EXEC.
           MOVE 'EL160A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL160S' TO DFHEIV2
      *    MOVE '8$D    CT    F  H     ,   #00008762' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL160AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02619
02620      GO TO 9000-RETURN-TRANS.
02621
02622  8120-FORMAT-TIME-DATE.
02623      MOVE SAVE-DATE      TO DATEO.
02624      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
02625 *    END-EXEC
      *    MOVE '0"A                   "   #00008774' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02626      
      * EXEC CICS FORMATTIME
02627 *              ABSTIME(LCP-CICS-TIME)
02628 *              TIME(LCP-TIME-OF-DAY-XX)
02629 *    END-EXEC
      *    MOVE 'j$(     (             #   #00008776' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02630      MOVE  LCP-TIME-OF-DAY-68 TO TIME-IN.
02631      MOVE UN-HOURS       TO FOR-HOURS.
02632      MOVE UN-MINUTES     TO FOR-MINUTES.
02633      MOVE TIME-OUT       TO TIMEO.
02634      MOVE LIT-MAP        TO PI-CURRENT-SCREEN-NO.
02635      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
02636      MOVE EMI-MESSAGE-AREA (1) TO MSG1O.
02637
02638  8130-EXIT.
02639      EXIT.
02640
02641  8200-RETURN-PRIOR.
02642      MOVE PI-RETURN-TO-PROGRAM TO CALL-PGM.
02643      GO TO 9200-XCTL.
02644
02645  8300-GET-HELP.
02646      MOVE XCTL-EL010 TO CALL-PGM.
02647      GO TO 9200-XCTL.
02648
02649  8400-RETURN-MASTER.
02650      MOVE XCTL-EL126 TO CALL-PGM.
02651      GO TO 9200-XCTL.
02652
02653  8800-UNAUTHORIZED-ACCESS.
02654      MOVE UNACCESS-MSG TO LOGOFF-MSG.
02655      GO TO 8990-SEND-TEXT.
02656
02657  8810-PF23-ENTERED.
02658      MOVE EIBAID TO PI-ENTRY-CD-1.
02659      MOVE XCTL-EL005 TO CALL-PGM.
02660      GO TO 9200-XCTL.
02661
02662  8820-XCTL-ERROR.
02663      
      * EXEC CICS HANDLE CONDITION
02664 *        PGMIDERR (8990-SEND-TEXT)
02665 *    END-EXEC.
      *    MOVE '"$L                   ! . #00008813' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303038383133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02666
02667      MOVE SPACE        TO PI-ENTRY-CD-1.
02668      MOVE CALL-PGM     TO PI-CALLING-PROGRAM LOGOFF-PGM
02669      MOVE XCTL-EL005 TO CALL-PGM.
02670      MOVE PGMIDERR-MSG TO LOGOFF-FILL.
02671      GO TO 9200-XCTL.
02672
02673  8990-SEND-TEXT.
02674      
      * EXEC CICS SEND TEXT
02675 *        FROM (LOGOFF-TEXT)
02676 *        LENGTH (LOGOFF-LENGTH)
02677 *        ERASE
02678 *        FREEKB
02679 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00008824' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02680
02681      GO TO 9100-RETURN-CICS.
02682      EJECT
02683  9000-RETURN-TRANS.
02684      
      * EXEC CICS RETURN
02685 *        TRANSID (TRANS-ID)
02686 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02687 *        LENGTH (PI-COMM-LENGTH)
02688 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00008834' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02689      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL160' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
02690
02691  9100-RETURN-CICS.
02692      
      * EXEC CICS RETURN
02693 *    END-EXEC.
      *    MOVE '.(                    ''   #00008842' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02694      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL160' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
02695
02696  9200-XCTL.
02697      
      * EXEC CICS XCTL
02698 *        PROGRAM (CALL-PGM)
02699 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02700 *        LENGTH (PI-COMM-LENGTH)
02701 *    END-EXEC.
      *    MOVE '.$C                   %   #00008847' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02702
02703  9300-GET-FREE-LOOK.
02704
02705      MOVE PI-COMPANY-ID TO COMPANY-ID.
02706      MOVE '3'           TO RECORD-TYPE.
02707      MOVE PI-STATE      TO ACCESS-CD-GENL.
02708      MOVE ZEROS         TO SEQUENCE-NO.
02709
02710      
      * EXEC CICS READ
02711 *        SET (ADDRESS OF CONTROL-FILE)
02712 *        DATASET ('ELCNTL')
02713 *        RIDFLD (CNTL-KEY)
02714 *        RESP   (WS-RESPONSE)
02715 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00008860' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038383630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02716
02717      IF WS-RESP-NOTFND
02718         MOVE ER-2848   TO EMI-ERROR
02719         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02720         GO TO 8110-SEND-DATA
02721      ELSE
02722         MOVE CF-ST-FREE-LOOK-PERIOD
02723                         TO CP-FREE-LOOK.
02724
02725  9300-EXIT.
02726      EXIT.
02727
02728  9700-LINK-REM-TERM.
02729      
      * EXEC CICS LINK
02730 *        PROGRAM (REM-TERM-PGM)
02731 *        COMMAREA (CALCULATION-PASS-AREA)
02732 *        LENGTH (CP-COMM-LENGTH)
02733 *    END-EXEC.
      *    MOVE '."C                   (   #00008879' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REM-TERM-PGM, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02734
02735  9700-EXIT.
02736      EXIT.
02737
02738  9800-CONVERT-DATE.
02739      
      * EXEC CICS LINK
02740 *        PROGRAM    (DATE-CONV)
02741 *        COMMAREA   (DATE-CONVERSION-DATA)
02742 *        LENGTH     (DC-COMM-LENGTH)
02743 *    END-EXEC.
      *    MOVE '."C                   (   #00008889' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DATE-CONV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02744
02745  9800-EXIT.
02746      EXIT.
02747
02748  9900-ERROR-FORMAT.
02749      IF EMI-ERRORS-COMPLETE
02750          GO TO 9900-EXIT.
02751
02752      
      * EXEC CICS LINK
02753 *        PROGRAM ('EL001')
02754 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
02755 *        LENGTH (EMI-COMM-LENGTH)
02756 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00008902' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02757
02758  9900-EXIT.
02759      EXIT.
02760
02761  9990-ABEND.
02762      MOVE DFHEIBLK TO EMI-LINE1.
02763
02764      
      * EXEC CICS LINK
02765 *        PROGRAM   ('EL004')
02766 *        COMMAREA  (EMI-LINE1)
02767 *        LENGTH    (72)
02768 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00008914' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02769
02770      GO TO 8110-SEND-DATA.
02771
02772  9995-SECURITY-VIOLATION.
02773 *                            COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00008940' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
02774
02775  9995-EXIT.
02776      EXIT.
02777

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL160' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8820-XCTL-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0410-TS-NOTFND,
                     0420-FIND-SCREEN-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0440-DELETE-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0450-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1070-PROC-NOTFND,
                     6030-CNTL-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 5010-BAD-KEY,
                     6000-MSTR-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 5040-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 5085-CERT-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 5095-EMPLCY-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 5096-EMPLAN-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 5105-TRLR-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 5270-BENEFIT-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 8990-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL160' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
