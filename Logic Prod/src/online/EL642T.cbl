00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL642 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 01/06/95 10:16:47.
00007 *                            VMOD=2.031
00008 *
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023 *
00024 *REMARKS.
00025 *        TRANSACTION - EXH7 - GENERAL AGENT BILLING
00026 *                           (PREPARE AN AGENT STATEMENT).
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
101101******************************************************************
00027
00028  ENVIRONMENT DIVISION.
00029  EJECT
00030  DATA DIVISION.
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032
00033  77  FILLER  PIC X(32)  VALUE '********************************'.
00034  77  FILLER  PIC X(32)  VALUE '*    EL642 WORKING STORAGE     *'.
00035  77  FILLER  PIC X(32)  VALUE '***********VMOD 2.031 **********'.
00036
00037 *    COPY ELCSCTM.
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
00038 *    COPY ELCSCRTY.
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
00039  EJECT
00040  01  STANDARD-AREAS.
00041      12  SC-ITEM             PIC S9(4)   COMP    VALUE +1.
00042
00043      12  W-TRANSFER-CONTROL.
00044          16  WT-CR-CARRIER           PIC X.
00045          16  WT-CR-GROUPING          PIC X(6).
00046          16  WT-CR-STATE             PIC XX.
00047          16  WT-CR-ACCOUNT           PIC X(10).
00048          16  WT-CR-FIN-RESP          PIC X(10).
00049          16  WT-CR-TYPE              PIC X.
00050
00051      12  GETMAIN-SPACE       PIC  X              VALUE SPACE.
00052      12  MAP-NAME            PIC  X(8)           VALUE 'EL642A'.
00053      12  MAPSET-NAME         PIC  X(8)           VALUE 'EL642S'.
00054      12  SCREEN-NUMBER       PIC  X(4)           VALUE '642A'.
00055      12  TRANS-ID            PIC  X(4)           VALUE 'EXH7'.
00056      12  EL633-TRANS-ID      PIC  X(4)           VALUE 'EXB7'.
00057      12  EL633DMD-TRANS-ID   PIC  X(4)           VALUE 'EX1F'.
00058      12  EL635-TRANS-ID      PIC  X(4)           VALUE 'EXJ4'.
00059      12  EL650-TRANS-ID      PIC  X(4)           VALUE 'EXC4'.
00060      12  EL652-TRANS-ID      PIC  X(4)           VALUE 'EXD4'.
00061      12  EL658-TRANS-ID      PIC  X(4)           VALUE 'EXJ3'.
00062      12  THIS-PGM            PIC  X(8)           VALUE 'EL642'.
00063      12  EL642A              PIC  X(8)           VALUE 'EL642A'.
00064      12  EL642B              PIC  X(8)           VALUE 'EL642B'.
00065      12  PGM-NAME            PIC  X(8).
00066      12  TIME-IN             PIC S9(7).
00067      12  TIME-OUT-R  REDEFINES  TIME-IN.
00068          16  FILLER          PIC  X.
00069          16  TIME-OUT        PIC  99V99.
00070          16  FILLER          PIC  XX.
00071      12  XCTL-005            PIC  X(8)           VALUE 'EL005'.
00072      12  XCTL-010            PIC  X(8)           VALUE 'EL010'.
00073      12  XCTL-626            PIC  X(8)           VALUE 'EL626'.
00074      12  XCTL-633            PIC  X(8)           VALUE 'EL633'.
00075      12  XCTL-633DMD         PIC  X(8)           VALUE 'EL633DMD'.
00076      12  XCTL-635            PIC  X(8)           VALUE 'EL635'.
00077      12  XCTL-PYAJ           PIC  X(8)           VALUE 'EL633'.
00078      12  XCTL-650            PIC  X(8)           VALUE 'EL650'.
00079      12  XCTL-652            PIC  X(8)           VALUE 'EL652'.
00080      12  XCTL-6401           PIC  X(8)           VALUE 'EL6401'.
00081      12  XCTL-658            PIC  X(8)           VALUE 'EL658'.
00082      12  LINK-001            PIC  X(8)           VALUE 'EL001'.
00083      12  LINK-004            PIC  X(8)           VALUE 'EL004'.
00084      12  LINK-ELDATCV        PIC  X(8)           VALUE 'ELDATCV'.
00085      12  ERPNDB-ALT-FILE-ID  PIC  X(8)           VALUE 'ERPNDB2'.
00086      12  ERPNDB-FILE-ID      PIC  X(8)           VALUE 'ERPNDB'.
00087      12  ELCNTL-FILE-ID      PIC  X(8)           VALUE 'ELCNTL'.
00088      12  ERCOMP-FILE-ID      PIC  X(8)           VALUE 'ERCOMP'.
00089      12  ERPYAJ-FILE-ID      PIC  X(8)           VALUE 'ERPYAJ'.
00090      12  ERACCT-ALT-FILE-ID  PIC  X(8)           VALUE 'ERACCT2'.
00091      12  ERACCT-FILE-ID      PIC  X(8)           VALUE 'ERACCT'.
00092      12  ERBILL-FILE-ID      PIC  X(8)           VALUE 'ERBILL'.
00093      12  ERGXRF-FILE-ID      PIC  X(8)           VALUE 'ERGXRF'.
00094      12  ERCOMM-FILE-ID      PIC  X(8)           VALUE 'ERCOMM'.
00095      12  ERCTBL-FILE-ID      PIC  X(8)           VALUE 'ERCTBL'.
00096      12  ERRESC-FILE-ID      PIC  X(8)           VALUE 'ERRESC'.
00097      12  RETURNED-FROM       PIC  X(8)           VALUE SPACES.
00098      12  QID.
00099          16  QID-TERM        PIC  X(4).
00100          16  FILLER          PIC  X(4)           VALUE '642A'.
00101
00102  01  WORK-AREA.
00103      12  FIRST-TIME-SW               PIC  X      VALUE 'Y'.
00104          88  FIRST-TIME                          VALUE 'Y'.
00105      12  COMPENSATION-SW             PIC  X      VALUE ' '.
00106          88  GENERAL-AGENT                       VALUE 'G'.
00107      12  LIFE-INDICATOR              PIC  X      VALUE ' '.
00108          88  JOINT-LIFE                          VALUE 'J'.
00109      12  BENEFIT-MASTER-FOUND-SW     PIC  X      VALUE ' '.
00110          88  BENEFIT-MASTER-FOUND                VALUE 'Y'.
00111      12  COMMISSION-TBL-SW           PIC  X      VALUE ' '.
00112          88  COMMISSION-TBL-PRESENT              VALUE 'Y'.
00113      12  INVALID-ACCOUNT-SW          PIC  X      VALUE ' '.
00114          88  INVALID-ACCOUNT                     VALUE 'Y'.
00115      12  NEW-ACCOUNT-SW              PIC  X      VALUE ' '.
00116          88  NEW-ACCOUNT                         VALUE 'Y'.
00117      12  VALID-BILL-TYPE-VALUES      PIC  X      VALUE SPACE.
00118          88  VALID-BILL-TYPE                     VALUE  '1' '2'
00119                                                     '3' '4' '5'.
00120      12  BILL-BATCH-TRAILER-SW       PIC  X      VALUE SPACE.
00121          88  BILL-BATCH-TRAILER                  VALUE 'Y'.
00122      12  ERCOMM-FOUND-SW             PIC  X      VALUE SPACE.
00123          88  ERCOMM-FOUND                        VALUE 'Y'.
00124          88  ERCOMM-NOT-FOUND                    VALUE 'N'.
00125      12  ERCTBL-FOUND-SW             PIC  X      VALUE SPACE.
00126          88  ERCTBL-FOUND                        VALUE 'Y'.
00127          88  ERCTBL-NOT-FOUND                    VALUE 'N'.
00128      12  GXRF-EOF-SW                 PIC  X      VALUE SPACE.
00129          88  GXRF-EOF                            VALUE 'Y'.
00130      12  PNDB-EOF-SW                 PIC  X      VALUE SPACE.
00131          88  PNDB-EOF                            VALUE 'Y'.
00132      12  PYAJ-EOF-SW                 PIC  X      VALUE SPACE.
00133          88  PYAJ-EOF                            VALUE 'Y'.
00134      12  COMP-UPDATE-SW              PIC  X      VALUE SPACE.
00135          88  UPDATE-COMP-TOTALS                  VALUE 'Y'.
00136      12  LIMIT-BILLING-SW            PIC  X      VALUE SPACE.
00137          88  LIMIT-BILLING                       VALUE 'Y'.
00138      12  WS-PROCESS-SW               PIC  X      VALUE SPACE.
00139          88  DO-NOT-BILL-THIS-ACCT               VALUE 'Y'.
00140      12  NO-BILL-REC-SW              PIC  X      VALUE SPACE.
00141          88  NO-BILL-RECS                        VALUE 'Y'.
00142      12  DATA-VOIDED-SW              PIC  X      VALUE SPACE.
00143          88  DATA-VOIDED                         VALUE 'Y'.
00144      12  BATCHES-PROCESSED-SW        PIC  X      VALUE SPACE.
00145          88  MORE-THAN-6-BATCHES                 VALUE 'Y'.
00146      12  ACCOUNT-CARRY-BAL-SW        PIC  X      VALUE SPACE.
00147          88  ACCOUNT-CARRY-BAL                   VALUE 'Y'.
00148      12  BILLING-DETAIL-TYPE         PIC  XX VALUE SPACES.
00149          88  TOTAL-STATEMENT                     VALUE 'TS'.
00150          88  TOTAL-ACCOUNT                       VALUE 'TA'.
00151          88  TOTAL-BOTH                          VALUE 'TB'.
00152          88  GEN-AGT-ADDRESS                     VALUE 'GA'.
00153      12  ELCNTL-UPDATE-SW            PIC  X      VALUE SPACE.
00154          88  ELCNTL-UPDATE                       VALUE 'Y'.
00155      12  WS-BILL-TYPE                PIC  X      VALUE SPACE.
00156          88  WS-UPDATE-FILES                     VALUE '3' '4'
00157                                                        '5'.
00158      12  SUB                     PIC S999 COMP-3 VALUE ZEROS.
00159      12  ACCOM-SUB               PIC  99 COMP VALUE ZEROS.
00160      12  GX-SUB                  PIC S9(4) COMP VALUE ZEROS.
00161      12  COMP-SUB                PIC S9(4) COMP VALUE ZEROS.
00162      12  CE-SUB                  PIC S9(4) COMP VALUE ZEROS.
00163      12  BEN-SUB                 PIC S9(4) COMP VALUE ZEROS.
00164      12  WORK-SEQ-NO             PIC S9(9) COMP-3.
00165      12  WS-ERROR-MSG.
00166          16  WS-ERROR-TEXT       PIC  X(35)      VALUE SPACES.
00167          16  WS-ERROR-BATCH      PIC  X(6)       VALUE SPACES.
00168          16  FILLER              PIC  X(24)      VALUE SPACES.
00169      12  WS-WORK-DATE.
00170          16  WS-MONTH            PIC  99         VALUE ZEROS.
00171          16  WS-DAY              PIC  99         VALUE ZEROS.
00172          16  WS-YEAR             PIC  99         VALUE ZEROS.
00173      12  WS-MONTH-END-DATE.
00174          16  WS-ME-YEAR          PIC  99         VALUE ZEROS.
00175          16  WS-ME-MONTH         PIC  99         VALUE ZEROS.
00176          16  WS-ME-DAY           PIC  99         VALUE ZEROS.
00177      12  WS-CURRENT-DATE         PIC  XX         VALUE SPACES.
00178      12  WS-CURRENT-DATE-MDY     PIC  X(6)       VALUE SPACES.
00179      12  WS-CURRENT-DATE-EDIT    PIC  X(8)       VALUE SPACES.
00180      12  WS-PREV-BILL-DATE       PIC  XX     VALUE LOW-VALUES.
00181      12  MONTHS-DIFF-LF          PIC S9(05).
00182      12  MONTHS-DIFF-AH          PIC S9(05).
00183      12  WS-LF-CANCEL-DATE.
00184          16  WS-LF-CANCEL-YR     PIC 99.
00185          16  WS-LF-CANCEL-MO     PIC 99.
00186          16  WS-LF-CANCEL-DA     PIC 99.
00187      12  WS-AH-CANCEL-DATE.
00188          16  WS-AH-CANCEL-YR     PIC 99.
00189          16  WS-AH-CANCEL-MO     PIC 99.
00190          16  WS-AH-CANCEL-DA     PIC 99.
00191      12  WS-EFFECT-DATE.
00192          16  WS-EFFECT-YR        PIC 99.
00193          16  WS-EFFECT-MO        PIC 99.
00194          16  WS-EFFECT-DA        PIC 99.
00195      12  WS-BEGIN-DATE.
00196          16  FILLER              PIC  X(3)       VALUE SPACES.
00197          16  WS-BEGIN-DAY        PIC  XX         VALUE SPACES.
00198          16  FILLER              PIC  X(3)       VALUE SPACES.
00199      12  PRINT-CONTROL.
00200          16  SINGLE-SPACE        PIC  X          VALUE SPACE.
00201          16  DOUBLE-SPACE        PIC  X          VALUE ZERO.
00202          16  TRIPLE-SPACE        PIC  X          VALUE '-'.
00203          16  SUPPRESS-SPACE      PIC  X          VALUE '+'.
00204          16  TOP-OF-PAGE         PIC  X          VALUE '1'.
00205      12  WS-EXP-DATE.
00206          16  WS-SAV-EXP-DT   OCCURS  10  TIMES
00207                  INDEXED BY  DTNDX   PIC  XX.
00208      12  WS-REMIT-TO.
00209          16  WS-SAV-REMIT-TO     OCCURS  10  TIMES
00210                  INDEXED BY  RTNDX   PIC  X(6).
00211      12  WORK-REMIT-TO           PIC  X(6)       VALUE SPACES.
00212      12  WS-REMITTED             PIC S9(6)V99    VALUE ZEROS.
00213      12  WS-LINECTR              PIC  99         VALUE ZEROS.
00214      12  WS-PGECTR               PIC  9(3)       VALUE 1.
00215      12  WS-LINE-SEQ-NO          PIC S9(4) COMP VALUE +1.
00216      12  WS-CARR-COMP.
00217          16  WS-CARRIER          PIC  X          VALUE SPACE.
00218          16  WS-COMP             PIC  X(3)       VALUE SPACES.
00219      12  WORK-AMT                PIC S9(7)V99    VALUE ZEROS.
00220      12  JOURNAL-LENGTH          PIC S9(4) COMP VALUE ZERO.
00221      12  LF-COMM-TBL-NO          PIC  X(3)       VALUE SPACES.
00222      12  JT-COMM-TBL-NO          PIC  X(3)       VALUE SPACES.
00223      12  AH-COMM-TBL-NO          PIC  X(3)       VALUE SPACES.
00224      12  AGENT-OWES              PIC  X(18)      VALUE
00225              'AGENT OWES       ='.
00226      12  OWED-TO-AGENT           PIC  X(18)      VALUE
00227              'OWED TO AGENT    ='.
00228      12  WS-PY-ENTRY-COMMENT.
00229          16  WS-PY-CURRENT-DATE  PIC  X(6).
00230          16  FILLER              PIC  X(14)      VALUE
00231                  ' CHK TO AGENT'.
00232      12  WS-ACCT-NOT-BILLED      PIC  X(18)      VALUE
00233              'ACCOUNT NOT BILLED'.
00234  EJECT
00235      12  WS-AGENT-ADDR-AREA.
00236          16  WS-AGENT-LINES  OCCURS  6  TIMES
00237                  INDEXED BY  A-INDX.
00238              20  WS-AGENT-ZIP.
00239                  24  WS-AGENT-1ST-ZIP    PIC  X(4).
00240                  24  WS-AGENT-2ND-ZIP    PIC  X(5).
00241              20  FILLER                  PIC  X(12).
00242              20  WS-A-LAST-ZIP.
00243                  24  WS-A-LAST-1ST-ZIP   PIC  X(4).
00244                  24  WS-A-LAST-2ND-ZIP   PIC  X(5).
00245      12  WS-COMPENSATION-WORK-AREA.
00246          16  WS-COMM-CK-AMT      PIC S9(5)V99 COMP-3 VALUE +0.
00247          16  WS-COMM-AGE         PIC  99.
00248          16  WS-COMM-TERM        PIC  9(3).
00249          16  WS-LF-ISS-COMP      PIC S9(5)V99 COMP-3 VALUE +0.
00250          16  WS-AH-ISS-COMP      PIC S9(5)V99 COMP-3 VALUE +0.
00251          16  WS-I-LF-PREMIUM-AMT PIC S9(7)V99 COMP-3 VALUE +0.
00252          16  WS-I-AH-PREMIUM-AMT PIC S9(7)V99 COMP-3 VALUE +0.
00253          16  WS-I-LF-BENEFIT-AMT PIC S9(9)V99 COMP-3 VALUE +0.
00254          16  WS-I-AH-BENEFIT-AMT PIC S9(9)V99 COMP-3 VALUE +0.
00255          16  WS-C-LF-CANCEL-AMT  PIC S9(7)V99 COMP-3 VALUE +0.
00256          16  WS-C-AH-CANCEL-AMT  PIC S9(7)V99 COMP-3 VALUE +0.
00257          16  WS-WK-RATE          PIC SV9(5)    COMP-3 VALUE +.0.
00258  EJECT
00259      12  WS-ACCOUNT-TOTALS       COMP-3.
00260          16  WS-ACCT-BEG-BAL         PIC S9(7)V99 VALUE ZEROS.
00261          16  WS-ACCT-NET-PREM        PIC S9(7)V99 VALUE ZEROS.
00262          16  WS-ACCT-COMP            PIC S9(7)V99 VALUE ZEROS.
00263          16  WS-ACCT-PAY-ADJS        PIC S9(7)V99 VALUE ZEROS.
00264          16  WS-ACCT-UNPAID-NET-PREM PIC S9(7)V99 VALUE ZEROS.
00265          16  WS-ACCT-LF-OVERWRITE    PIC S9(7)V99 VALUE ZEROS.
00266          16  WS-ACCT-AH-OVERWRITE    PIC S9(7)V99 VALUE ZEROS.
00267      12  WS-GENERAL-AGENT-TOTALS COMP-3.
00268          16  WS-GA-BEG-BAL           PIC S9(7)V99 VALUE ZEROS.
00269          16  WS-GA-NET-PREM          PIC S9(7)V99 VALUE ZEROS.
00270          16  WS-GA-COMP              PIC S9(7)V99 VALUE ZEROS.
00271          16  WS-GA-PAY-ADJS          PIC S9(7)V99 VALUE ZEROS.
00272          16  WS-GA-UNPAID-NET-PREM   PIC S9(7)V99 VALUE ZEROS.
00273          16  WS-GA-LF-OVERWRITE      PIC S9(7)V99 VALUE ZEROS.
00274          16  WS-GA-AH-OVERWRITE      PIC S9(7)V99 VALUE ZEROS.
00275          16  WS-GA-END-BAL           PIC S9(7)V99 VALUE ZEROS.
00276          16  WS-GA-AMT-DUE           PIC S9(7)V99 VALUE ZEROS.
00277      12  WS-GENERAL-AGENT-COMMISSIONS.
00278          16  WS-GA-LEVEL         PIC S99     COMP   VALUE +0.
00279          16  WS-GA-LF-COM        PIC SV9(5) COMP-3 VALUE +.0.
00280          16  WS-GA-AH-COM        PIC SV9(5) COMP-3 VALUE +.0.
00281  EJECT
00282  01  ACCESS-KEYS.
00283      12  ERPNDB-PRIME-KEY.
00284          16  ERPNDB-CO-CD        PIC  X          VALUE SPACES.
00285          16  ERPNDB-BATCH        PIC  X(6)       VALUE SPACES.
00286          16  ERPNDB-SEQ-NO       PIC S9(4) COMP.
00287          16  ERPNDB-CHG-SEQ-NO   PIC S9(4) COMP.
00288      12  ERPNDB-LENGTH           PIC S9(4) COMP VALUE +585.
00289      12  ERPNDB-ALT-KEY.
00290          16  ERPNDB-CO-CD-A1     PIC  X          VALUE SPACES.
00291          16  ERPNDB-CARR         PIC  X          VALUE SPACES.
00292          16  ERPNDB-GROUP        PIC  X(6)       VALUE SPACES.
00293          16  ERPNDB-STATE        PIC  XX         VALUE SPACES.
00294          16  ERPNDB-ACCT         PIC  X(10)      VALUE SPACES.
00295          16  ERPNDB-EFF-DT       PIC  XX         VALUE SPACES.
00296          16  ERPNDB-CERT.
00297              20  ERPNDB-CERT-PRM PIC  X(10)      VALUE SPACES.
00298              20  ERPNDB-CERTSFX  PIC  X          VALUE SPACES.
00299          16  ERPNDB-ACHG-SEQ-NO  PIC S9(4) COMP.
00300          16  ERPNDB-REC-TYPE     PIC  X          VALUE SPACES.
00301      12  ELCNTL-KEY.
00302          16  ELCNTL-COMPANY-ID   PIC  X(3)       VALUE SPACES.
00303          16  ELCNTL-REC-TYPE     PIC  X          VALUE SPACES.
00304          16  ELCNTL-FILLER       PIC  X(3)       VALUE SPACES.
00305          16  ELCNTL-CARRIER      PIC  X          VALUE SPACES.
00306          16  ELCNTL-SEQ-NO       PIC S9(4) COMP VALUE ZEROS.
00307      12  ELCNTL-BENEFIT-KEY.
00308          16  CLBENF-COMPANY-ID   PIC  X(3)       VALUE SPACES.
00309          16  CLBENF-REC-TYPE     PIC  X          VALUE SPACES.
00310          16  CLBENF-ACCESS.
00311              20  FILLER          PIC  XX         VALUE SPACES.
00312              20  CLBENF-CD       PIC  XX         VALUE SPACES.
00313          16  CLBENF-SEQ-NO       PIC S9(4) COMP VALUE ZEROS.
00314      12  ELCNTL-LENGTH           PIC S9(4) COMP VALUE +504.
00315      12  ERCOMP-KEY.
00316          16  ERCOMP-COMP-CD      PIC  X          VALUE SPACE.
00317          16  ERCOMP-CARRIER      PIC  X          VALUE SPACES.
00318          16  ERCOMP-GROUPING     PIC  X(6)       VALUE SPACES.
00319          16  ERCOMP-FIN-RESP     PIC  X(10)      VALUE SPACES.
00320          16  ERCOMP-ACCT         PIC  X(10)      VALUE SPACES.
00321          16  ERCOMP-RECORD-TYPE  PIC  X          VALUE SPACES.
00322      12  ERPYAJ-BROWSE-COMP-KEY.
00323          16  ERPYAJ-BR-COMP-CD   PIC  X          VALUE SPACE.
00324          16  ERPYAJ-BR-CARRIER   PIC  X          VALUE SPACES.
00325          16  ERPYAJ-BR-GROUPING  PIC  X(6)       VALUE SPACES.
00326          16  ERPYAJ-BR-FIN-RESP  PIC  X(10)      VALUE SPACES.
00327          16  ERPYAJ-BR-ACCOUNT   PIC  X(10)      VALUE SPACES.
00328      12  ERCOMP-PREV-KEY         PIC  X(29)      VALUE SPACES.
00329      12  ERCOMP-LENGTH           PIC S9(4) COMP VALUE +700.
00330      12  ERPYAJ-KEY.
00331          16  ERPYAJ-COMP-CD      PIC  X          VALUE SPACE.
00332          16  ERPYAJ-CARRIER      PIC  X          VALUE SPACES.
00333          16  ERPYAJ-GROUPING     PIC  X(6)       VALUE SPACES.
00334          16  ERPYAJ-FIN-RESP     PIC  X(10)      VALUE SPACES.
00335          16  ERPYAJ-ACCOUNT      PIC  X(10)      VALUE SPACES.
00336          16  ERPYAJ-FILE-SEQ-NO  PIC S9(8) COMP VALUE +0.
00337          16  ERPYAJ-RECORD-TYPE  PIC  X          VALUE SPACES.
00338      12  ERPYAJ-LENGTH           PIC S9(4) COMP VALUE +200.
00339      12  ERACCT-PRIME-KEY.
00340          16  ERACCT-P-CO-CD      PIC  X          VALUE SPACES.
00341          16  ERACCT-P-CARRIER    PIC  X          VALUE SPACES.
00342          16  ERACCT-P-GROUPING   PIC  X(6)       VALUE SPACES.
00343          16  ERACCT-P-STATE      PIC  XX         VALUE SPACES.
00344          16  ERACCT-P-ACCOUNT    PIC  X(10)      VALUE SPACES.
00345          16  ERACCT-P-EXP-DATE   PIC  XX         VALUE SPACES.
00346          16  FILLER              PIC  X(4)   VALUE LOW-VALUES.
00347      12  ERACCT-LENGTH           PIC S9(4) COMP VALUE +2000.
00348      12  ERACCT-ALT-KEY.
00349          16  ERACCT-A-CO-CD      PIC  X          VALUE SPACES.
00350          16  ERACCT-A-CARRIER    PIC  X          VALUE SPACES.
00351          16  ERACCT-A-GROUPING   PIC  X(6)       VALUE SPACES.
00352          16  ERACCT-A-STATE      PIC  XX         VALUE SPACES.
00353          16  ERACCT-A-ACCOUNT    PIC  X(10)      VALUE SPACES.
00354          16  ERACCT-A-EXP-DATE   PIC  XX         VALUE SPACES.
00355          16  FILLER              PIC  X(4)   VALUE LOW-VALUES.
00356      12  ERBILL-KEY.
00357          16  ERBILL-CO-CD        PIC  X.
00358          16  ERBILL-CARRIER      PIC  X.
00359          16  ERBILL-GROUP        PIC  X(6).
00360          16  ERBILL-ACCT         PIC  X(10).
00361          16  ERBILL-FIN-RESP     PIC  X(10).
00362          16  ERBILL-REC-TYPE     PIC  X.
00363          16  ERBILL-LINE-SEQ-NO  PIC S9(4) COMP.
00364      12  ERBILL-LENGTH           PIC S9(4) COMP VALUE +210.
00365      12  ERGXRF-KEY.
00366          16  ERGXRF-COMPANY-CD   PIC  X.
00367          16  ERGXRF-CARRIER      PIC  X.
00368          16  ERGXRF-GROUPING     PIC  X(6).
00369          16  ERGXRF-AGENT-NO     PIC  X(10).
00370      12  ERGXRF-FIXED-LENGTH     PIC S9(4) COMP VALUE +64.
00371      12  ERGXRF-VAR-LENGTH       PIC S9(4) COMP VALUE +24.
00372      12  ERGXRF-LENGTH           PIC S9(4) COMP VALUE ZEROS.
00373      12  ERCOMM-KEY.
00374          16  ERCOMM-COMPANY-CD   PIC  X.
00375          16  ERCOMM-CARRIER      PIC  X.
00376          16  ERCOMM-GROUPING     PIC  X(6).
00377          16  ERCOMM-STATE        PIC  XX.
00378          16  ERCOMM-ACCOUNT      PIC  X(10).
00379          16  ERCOMM-CERT-EFF-DT  PIC  XX.
00380          16  ERCOMM-CERT-NO.
00381              20  CE-CERT-PRIME   PIC  X(10).
00382              20  CE-CERT-SFX     PIC  X.
00383      12  ERCOMM-LENGTH           PIC S9(4) COMP VALUE +250.
00384      12  ERCTBL-KEY.
00385          16  ERCTBL-COMPANY-CD       PIC  X.
00386          16  ERCTBL-TABLE            PIC  X(3).
00387          16  ERCTBL-CNTRL-2.
00388              20  ERCTBL-BEN-TYPE     PIC  X.
00389              20  ERCTBL-BEN-CODE     PIC  XX.
00390      12  ERCTBL-LENGTH               PIC S9(4) COMP VALUE +200.
00391      12  SAVE-ERACCT-PRIME-KEY.
00392          16  SV-ERACCT-P-CO-CD       PIC  X      VALUE SPACES.
00393          16  SV-ERACCT-P-CARRIER     PIC  X      VALUE SPACES.
00394          16  SV-ERACCT-P-GROUPING    PIC  X(6)   VALUE SPACES.
00395          16  SV-ERACCT-P-STATE       PIC  XX     VALUE SPACES.
00396          16  SV-ERACCT-P-ACCOUNT     PIC  X(10)  VALUE SPACES.
00397          16  SV-ERACCT-P-EXP-DATE    PIC  XX     VALUE SPACES.
00398 *****************************************************************
00399 *      START OF WORKING STORAGE FOR CLIENT-DMD                  *
00400 *****************************************************************
00401 *RESIDENT STATE TAXES MASTER(ERRESC)
00402 *
00403      12  WS-SV-ERRESC-KEY             PIC X(32).
00404
00405      12  WS-ERRESC-KEY.
00406          16 WS-ERRESC-SEARCH-KEY.
00407              20  WS-ERRESC-COMPANY-CD PIC  X.
00408              20  WS-ERRESC-CARRIER    PIC  X.
00409              20  WS-ERRESC-GROUP      PIC X(6).
00410              20  WS-ERRESC-STATE      PIC XX.
00411              20  WS-ERRESC-ACCOUNT    PIC X(10).
00412          16 WS-ERRESC-RESIDUAL-KEY.
00413              20  WS-ERRESC-AGENT      PIC X(10).
00414              20  WS-ERRESC-RES-STATE  PIC XX.
00415              20  WS-ERRESC-EXPIRE-DT  PIC 9(8) COMP-3.
00416 *                                          YYYYMMDD
00417      12  WS-COMMISSION                PIC SV9(5) COMP-3.
00418      12  WS-SUB1                      PIC    S99 COMP-3.
00419      12  WS-SUB                       PIC    S99 COMP-3.
00420      12  WS-CONTRACT-DATE             PIC 9(8).
00421      12  WS-COMM-SW                   PIC XX VALUE SPACES.
00422          88  AH-COMM                             VALUE 'AH'.
00423          88  LF-COMM                             VALUE 'LF'.
00424 *****************************************************************
00425 *        END OF WORKING STORAGE FOR CLIENT-DMD                  *
00426 *****************************************************************
00427
00428  EJECT
00429  01  ERROR-NUMBERS.
00430      12  ER-0004             PIC  X(4)           VALUE '0004'.
00431      12  ER-0008             PIC  X(4)           VALUE '0008'.
00432      12  ER-0022             PIC  X(4)           VALUE '0022'.
00433      12  ER-0029             PIC  X(4)           VALUE '0029'.
00434      12  ER-0070             PIC  X(4)           VALUE '0070'.
00435      12  ER-0194             PIC  X(4)           VALUE '0194'.
00436      12  ER-0195             PIC  X(4)           VALUE '0195'.
00437      12  ER-0196             PIC  X(4)           VALUE '0196'.
00438      12  ER-0197             PIC  X(4)           VALUE '0197'.
00439      12  ER-2208             PIC  X(4)           VALUE '2208'.
00440      12  ER-2210             PIC  X(4)           VALUE '2210'.
00441      12  ER-2215             PIC  X(4)           VALUE '2215'.
00442      12  ER-2230             PIC  X(4)           VALUE '2230'.
00443      12  ER-2233             PIC  X(4)           VALUE '2233'.
00444      12  ER-2370             PIC  X(4)           VALUE '2370'.
00445      12  ER-2371             PIC  X(4)           VALUE '2371'.
00446      12  ER-2249             PIC  X(4)           VALUE '2249'.
00447      12  ER-2250             PIC  X(4)           VALUE '2250'.
00448      12  ER-2383             PIC  X(4)           VALUE '2383'.
00449      12  ER-2385             PIC  X(4)           VALUE '2385'.
00450      12  ER-2399             PIC  X(4)           VALUE '2399'.
00451      12  ER-2400             PIC  X(4)           VALUE '2400'.
00452      12  ER-2401             PIC  X(4)           VALUE '2401'.
00453      12  ER-2403             PIC  X(4)           VALUE '2403'.
00454      12  ER-2404             PIC  X(4)           VALUE '2404'.
00455      12  ER-2405             PIC  X(4)           VALUE '2405'.
00456      12  ER-2406             PIC  X(4)           VALUE '2406'.
00457      12  ER-2407             PIC  X(4)           VALUE '2407'.
00458      12  ER-2408             PIC  X(4)           VALUE '2408'.
00459      12  ER-2409             PIC  X(4)           VALUE '2409'.
00460      12  ER-2411             PIC  X(4)           VALUE '2411'.
00461      12  ER-2421             PIC  X(4)           VALUE '2421'.
00462      12  ER-2434             PIC  X(4)           VALUE '2434'.
00463      12  ER-2435             PIC  X(4)           VALUE '2435'.
00464      12  ER-2436             PIC  X(4)           VALUE '2436'.
00465      12  ER-2438             PIC  X(4)           VALUE '2438'.
00466      12  ER-2439             PIC  X(4)           VALUE '2439'.
00467      12  ER-2443             PIC  X(4)           VALUE '2443'.
00468      12  ER-2472             PIC  X(4)           VALUE '2472'.
00469      12  ER-2564             PIC  X(4)           VALUE '2564'.
00470      12  ER-2570             PIC  X(4)           VALUE '2570'.
00471      12  ER-2571             PIC  X(4)           VALUE '2571'.
00472      12  ER-2590             PIC  X(4)           VALUE '2571'.
00473      12  ER-2597             PIC  X(4)           VALUE '2597'.
00474      12  ER-2910             PIC  X(4)           VALUE '2910'.
00475      12  ER-2911             PIC  X(4)           VALUE '2911'.
00476      12  ER-2912             PIC  X(4)           VALUE '2912'.
00477      12  ER-2913             PIC  X(4)           VALUE '2913'.
00478      12  ER-2914             PIC  X(4)           VALUE '2914'.
00479      12  ER-2915             PIC  X(4)           VALUE '2915'.
00480      12  ER-2916             PIC  X(4)           VALUE '2916'.
00481      12  ER-2917             PIC  X(4)           VALUE '2917'.
00482      12  ER-2918             PIC  X(4)           VALUE '2918'.
00483      12  ER-3145             PIC  X(4)           VALUE '3145'.
00484      12  ER-3165             PIC  X(4)           VALUE '3165'.
00485  EJECT
00486  01  CENTER-DATA-WORK-AREA.
00487      12  X1                  PIC S9(4)   COMP    VALUE +0.
00488      12  X2                  PIC S9(4)   COMP    VALUE +0.
00489      12  X3                  PIC S9(4)   COMP    VALUE +0.
00490      12  X-LEN               PIC S9(4)   COMP    VALUE +0.
00491      12  CENTER-WORK-1.
00492          16  CW1-PIC         PIC  X      OCCURS  44  TIMES.
00493      12  CENTER-WORK-2.
00494          16  CW2-PIC         PIC  X      OCCURS  44  TIMES.
00495  EJECT
00496  01  GA-REPORT-HEADINGS.
00497      12  GA-PREVIEW-HD.
00498          16  FILLER          PIC  X(44)          VALUE
00499                  '                                          **'.
00500          16  FILLER          PIC  X(45)          VALUE
00501                  '** STATEMENT IS FOR REVIEW PURPOSES ONLY ***'.
00502          16  FILLER          PIC  X(44)          VALUE
00503                  '*                                           '.
00504      12  GA-HD1.
00505          16  FILLER          PIC  X(44)          VALUE SPACES.
00506          16  FILLER          PIC  X(44)          VALUE
00507                  '            AGENT    STATEMENT              '.
00508          16  FILLER          PIC  X(37)          VALUE SPACES.
00509          16  FILLER          PIC  X(7)           VALUE ' EL642 '.
00510      12  GA-HD2.
00511          16  FILLER          PIC  X(43)          VALUE SPACES.
00512          16  HD-CO           PIC  X(44)          VALUE SPACES.
00513          16  FILLER          PIC  X(37)          VALUE SPACES.
00514          16  HD-RUN-DT       PIC  X(8)           VALUE SPACES.
00515      12  GA-HD3.
00516          16  FILLER          PIC  X(57)          VALUE SPACES.
00517          16  HD-DT           PIC  X(18)          VALUE SPACES.
00518          16  FILLER          PIC  X(37)          VALUE SPACES.
00519          16  FILLER          PIC  X(5)           VALUE 'PAGE '.
00520          16  HD-PG           PIC ZZ,ZZ9.
00521      12  GA-HD4.
00522          16  FILLER          PIC  X(44)          VALUE
00523                  ' ------------- A C C O U N T -------------  '.
00524          16  FILLER          PIC  X(44)          VALUE
00525                  'BEGINNING      NET       ACCOUNT    PAYMENTS'.
00526          16  FILLER          PIC  X(44)          VALUE
00527                  '     UNPAID                          AMT.   '.
00528      12  GA-HD5.
00529          16  FILLER          PIC  X(44)          VALUE
00530                  '   NUMBER                 NAME              '.
00531          16  FILLER          PIC  X(44)          VALUE
00532                  ' BALANCE     PREMIUM     COMPENS.   ADJUSTS.'.
00533          16  FILLER          PIC  X(44)          VALUE
00534                  '  NET PREMIUM  BENEFIT  OVERWRITE    DUE    '.
00535  EJECT
00536 *                            COPY ELCDATE.
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
00537  EJECT
00538 *                            COPY ELCLOGOF.
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
00539  EJECT
00540 *                            COPY ELCATTR.
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
00541  EJECT
00542 *                            COPY ELCEMIB.
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
00070      12  FILLER                      PIC X(137)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00543  EJECT
00544 *                            COPY ELCINTF.
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
00131      12  FILLER                          PIC X(4).
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
00545      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00546          16  PI-MAP-NAME         PIC  X(8).
00547          16  PI-AGENT-NAME       PIC  X(20).
00548          16  PI-SAV-REMIT-TO     PIC  X(10).
00549          16  PI-SAV-FIN-RESP     PIC  X(10).
00550          16  PI-SAV-ACCT         PIC  X(10).
00551          16  PI-SAV-ACCT-AGT     PIC  X(10).
00552          16  PI-SAV-ACCT-NAME    PIC  X(30).
00553          16  PI-SAV-AGENT        PIC  X(10).
00554          16  PI-SAV-CARR         PIC  X.
00555          16  PI-SAV-GROUP        PIC  X(6).
00556          16  PI-SAV-STATE        PIC  XX.
00557          16  PI-SAV-EXP-DT       PIC  XX.
00558          16  PI-UNPAID-NET-PREM  PIC S9(7)V99    COMP-3.
00559          16  PI-COMP-UNPAID-PREM PIC S9(7)V99    COMP-3.
00560          16  PI-BAL-FRWD         PIC S9(7)V99    COMP-3.
00561          16  PI-PREMIUM          PIC S9(7)V99    COMP-3.
00562          16  PI-REMITTED         PIC S9(7)V99    COMP-3.
00563          16  PI-TOT-ISS-COMP     PIC S9(7)V99    COMP-3.
00564          16  PI-TOT-CAN-COMP     PIC S9(7)V99    COMP-3.
00565          16  PI-ADJUSTMNTS       PIC S9(7)V99    COMP-3.
00566          16  PI-DISBURSED        PIC S9(7)V99    COMP-3.
00567          16  PI-END-BAL          PIC S9(7)V99    COMP-3.
00568          16  PI-LF-ISS-COMP      PIC S9(7)V99    COMP-3.
00569          16  PI-AH-ISS-COMP      PIC S9(7)V99    COMP-3.
00570          16  PI-LF-CAN-COMP      PIC S9(7)V99    COMP-3.
00571          16  PI-AH-CAN-COMP      PIC S9(7)V99    COMP-3.
00572          16  PI-DUE-FOR-ACCT     PIC S9(7)V99    COMP-3.
00573          16  PI-ACCT-BEG-BAL     PIC S9(7)V99    COMP-3.
00574          16  PI-ACCT-NET-PREM    PIC S9(7)V99    COMP-3.
00575          16  PI-ACCT-COMP        PIC S9(7)V99    COMP-3.
00576          16  PI-ACCT-PAY-ADJS    PIC S9(7)V99    COMP-3.
00577          16  PI-BILL-TYPE        PIC  X.
00578              88  PI-PREV-BILL                    VALUE '1'.
00579              88  PI-PREV-REBILL                  VALUE '2'.
00580              88  PI-PREVIEW                      VALUE '1' '2'.
00581              88  PI-BILL                         VALUE '3'.
00582              88  PI-REBILLING                    VALUE '4'.
00583              88  PI-VOID-BILL                    VALUE '5'.
00584              88  PI-TOT-REBILL                   VALUE '2' '4'.
00585              88  PI-UPDATE-FILES                 VALUE '3' '4'
00586                                                        '5'.
00587              88  PI-BILLING-FUNCTION             VALUE '1' '2'
00588                                                        '3' '4'.
00589          16  PI-BILL-ERRS        PIC  X.
00590          16  PI-CHECK-SW         PIC  X.
00591              88  PI-CHECK-PRODUCED               VALUE 'Y'.
00592          16  PI-DATA-BILLED-SW   PIC  X.
00593              88  PI-DATA-BILLED                  VALUE 'Y'.
00594          16  PI-ACCT-BILLED-SW   PIC  X.
00595              88  PI-ACCT-BILLED                  VALUE 'Y'.
00596          16  PI-MONTH-END-DATE.
00597              20  PI-ME-MONTH     PIC  99.
00598              20  FILLER          PIC  X.
00599              20  PI-ME-DAY       PIC  99.
00600              20  FILLER          PIC  X.
00601              20  PI-ME-YEAR      PIC  99.
00602          16  PI-LIMIT-BILLING-ACCOUNTS.
00603              20  PI-BILLING-ACCOUNTS     OCCURS  3  TIMES
00604                                      PIC  X(10).
00605          16  PI-ISSUE-INFO.
00606              20  PI-ISSUES-BILLED    PIC S9(6) COMP-3.
00607              20  PI-ISSUES-INER      PIC S9(6) COMP-3.
00608              20  PI-ISSUES-PREV      PIC S9(6) COMP-3.
00609          16  PI-CANCEL-INFO.
00610              20  PI-CANCELS-BILLED   PIC S9(6) COMP-3.
00611              20  PI-CANCELS-INER     PIC S9(6) COMP-3.
00612              20  PI-CANCELS-PREV     PIC S9(6) COMP-3.
00613          16  PI-UNKNOWN              PIC S9(6) COMP-3.
00614          16  PI-COMP-CONTROL.
00615              20  PI-COMP-CARRIER     PIC  X.
00616              20  PI-COMP-GROUPING    PIC  X(6).
00617              20  PI-COMP-FIN-RESP    PIC  X(10).
00618          16  PI-SCRN-CONTROL.
00619              20  PI-SCR-CARRIER      PIC  X.
00620              20  PI-SCR-GROUPING     PIC  X(6).
00621              20  PI-SCR-STATE        PIC  XX.
00622              20  PI-SCR-ACCOUNT      PIC  X(10).
00623              20  PI-SCR-FIN-RESP     PIC  X(10).
00624              20  PI-SCR-TYPE         PIC  X.
00625          16  PI-TRANSFER-SW          PIC  X.
00626              88  PI-TRANSFER-BEFORE-ACT  VALUE 'Y'.
00627          16  FILLER                  PIC  X(307).
00628
00629  EJECT
00630 *                            COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
00631                              PIC  X(1464).
00632  EJECT
00633 *                            COPY ELCAID.
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
00634
00635  01  FILLER  REDEFINES  DFHAID.
00636      12  FILLER              PIC  X(8).
00637      12  PF-VALUES           PIC  X      OCCURS  2  TIMES.
00638  EJECT
00639 *                            COPY EL642S.
       01  EL642AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  ADATEL PIC S9(0004) COMP.
           05  ADATEF PIC  X(0001).
           05  FILLER REDEFINES ADATEF.
               10  ADATEA PIC  X(0001).
           05  ADATEI PIC  X(0008).
      *    -------------------------------
           05  ATIMEL PIC S9(0004) COMP.
           05  ATIMEF PIC  X(0001).
           05  FILLER REDEFINES ATIMEF.
               10  ATIMEA PIC  X(0001).
           05  ATIMEI PIC  X(0005).
      *    -------------------------------
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  ABEGBALL PIC S9(0004) COMP.
           05  ABEGBALF PIC  X(0001).
           05  FILLER REDEFINES ABEGBALF.
               10  ABEGBALA PIC  X(0001).
           05  ABEGBALI PIC  X(0010).
      *    -------------------------------
           05  ACARHDGL PIC S9(0004) COMP.
           05  ACARHDGF PIC  X(0001).
           05  FILLER REDEFINES ACARHDGF.
               10  ACARHDGA PIC  X(0001).
           05  ACARHDGI PIC  X(0007).
      *    -------------------------------
           05  AGRPHDGL PIC S9(0004) COMP.
           05  AGRPHDGF PIC  X(0001).
           05  FILLER REDEFINES AGRPHDGF.
               10  AGRPHDGA PIC  X(0001).
           05  AGRPHDGI PIC  X(0005).
      *    -------------------------------
           05  AUPDPRML PIC S9(0004) COMP.
           05  AUPDPRMF PIC  X(0001).
           05  FILLER REDEFINES AUPDPRMF.
               10  AUPDPRMA PIC  X(0001).
           05  AUPDPRMI PIC  X(0010).
      *    -------------------------------
           05  AAGENTL PIC S9(0004) COMP.
           05  AAGENTF PIC  X(0001).
           05  FILLER REDEFINES AAGENTF.
               10  AAGENTA PIC  X(0001).
           05  AAGENTI PIC  X(0010).
      *    -------------------------------
           05  ACARIERL PIC S9(0004) COMP.
           05  ACARIERF PIC  X(0001).
           05  FILLER REDEFINES ACARIERF.
               10  ACARIERA PIC  X(0001).
           05  ACARIERI PIC  X(0001).
      *    -------------------------------
           05  AGROUPL PIC S9(0004) COMP.
           05  AGROUPF PIC  X(0001).
           05  FILLER REDEFINES AGROUPF.
               10  AGROUPA PIC  X(0001).
           05  AGROUPI PIC  X(0006).
      *    -------------------------------
           05  AUPDCOML PIC S9(0004) COMP.
           05  AUPDCOMF PIC  X(0001).
           05  FILLER REDEFINES AUPDCOMF.
               10  AUPDCOMA PIC  X(0001).
           05  AUPDCOMI PIC  X(0010).
      *    -------------------------------
           05  APREMUML PIC S9(0004) COMP.
           05  APREMUMF PIC  X(0001).
           05  FILLER REDEFINES APREMUMF.
               10  APREMUMA PIC  X(0001).
           05  APREMUMI PIC  X(0010).
      *    -------------------------------
           05  ACOMPISL PIC S9(0004) COMP.
           05  ACOMPISF PIC  X(0001).
           05  FILLER REDEFINES ACOMPISF.
               10  ACOMPISA PIC  X(0001).
           05  ACOMPISI PIC  X(0010).
      *    -------------------------------
           05  ACOMCANL PIC S9(0004) COMP.
           05  ACOMCANF PIC  X(0001).
           05  FILLER REDEFINES ACOMCANF.
               10  ACOMCANA PIC  X(0001).
           05  ACOMCANI PIC  X(0010).
      *    -------------------------------
           05  ABILL1L PIC S9(0004) COMP.
           05  ABILL1F PIC  X(0001).
           05  FILLER REDEFINES ABILL1F.
               10  ABILL1A PIC  X(0001).
           05  ABILL1I PIC  X(0005).
      *    -------------------------------
           05  AINER1L PIC S9(0004) COMP.
           05  AINER1F PIC  X(0001).
           05  FILLER REDEFINES AINER1F.
               10  AINER1A PIC  X(0001).
           05  AINER1I PIC  X(0005).
      *    -------------------------------
           05  APRVBL1L PIC S9(0004) COMP.
           05  APRVBL1F PIC  X(0001).
           05  FILLER REDEFINES APRVBL1F.
               10  APRVBL1A PIC  X(0001).
           05  APRVBL1I PIC  X(0005).
      *    -------------------------------
           05  AADJUSTL PIC S9(0004) COMP.
           05  AADJUSTF PIC  X(0001).
           05  FILLER REDEFINES AADJUSTF.
               10  AADJUSTA PIC  X(0001).
           05  AADJUSTI PIC  X(0010).
      *    -------------------------------
           05  ABILL2L PIC S9(0004) COMP.
           05  ABILL2F PIC  X(0001).
           05  FILLER REDEFINES ABILL2F.
               10  ABILL2A PIC  X(0001).
           05  ABILL2I PIC  X(0005).
      *    -------------------------------
           05  AINER2L PIC S9(0004) COMP.
           05  AINER2F PIC  X(0001).
           05  FILLER REDEFINES AINER2F.
               10  AINER2A PIC  X(0001).
           05  AINER2I PIC  X(0005).
      *    -------------------------------
           05  APRVBL2L PIC S9(0004) COMP.
           05  APRVBL2F PIC  X(0001).
           05  FILLER REDEFINES APRVBL2F.
               10  APRVBL2A PIC  X(0001).
           05  APRVBL2I PIC  X(0005).
      *    -------------------------------
           05  AREMITL PIC S9(0004) COMP.
           05  AREMITF PIC  X(0001).
           05  FILLER REDEFINES AREMITF.
               10  AREMITA PIC  X(0001).
           05  AREMITI PIC  X(0010).
      *    -------------------------------
           05  ADISBURL PIC S9(0004) COMP.
           05  ADISBURF PIC  X(0001).
           05  FILLER REDEFINES ADISBURF.
               10  ADISBURA PIC  X(0001).
           05  ADISBURI PIC  X(0010).
      *    -------------------------------
           05  AACTHDGL PIC S9(0004) COMP.
           05  AACTHDGF PIC  X(0001).
           05  FILLER REDEFINES AACTHDGF.
               10  AACTHDGA PIC  X(0001).
           05  AACTHDGI PIC  X(0029).
      *    -------------------------------
           05  AACTDUEL PIC S9(0004) COMP.
           05  AACTDUEF PIC  X(0001).
           05  FILLER REDEFINES AACTDUEF.
               10  AACTDUEA PIC  X(0001).
           05  AACTDUEI PIC  X(0010).
      *    -------------------------------
           05  AENDHDGL PIC S9(0004) COMP.
           05  AENDHDGF PIC  X(0001).
           05  FILLER REDEFINES AENDHDGF.
               10  AENDHDGA PIC  X(0001).
           05  AENDHDGI PIC  X(0018).
      *    -------------------------------
           05  ANETDUEL PIC S9(0004) COMP.
           05  ANETDUEF PIC  X(0001).
           05  FILLER REDEFINES ANETDUEF.
               10  ANETDUEA PIC  X(0001).
           05  ANETDUEI PIC  X(0010).
      *    -------------------------------
           05  ABILTYPL PIC S9(0004) COMP.
           05  ABILTYPF PIC  X(0001).
           05  FILLER REDEFINES ABILTYPF.
               10  ABILTYPA PIC  X(0001).
           05  ABILTYPI PIC  X(0001).
      *    -------------------------------
           05  APRODSWL PIC S9(0004) COMP.
           05  APRODSWF PIC  X(0001).
           05  FILLER REDEFINES APRODSWF.
               10  APRODSWA PIC  X(0001).
           05  APRODSWI PIC  X(0001).
      *    -------------------------------
           05  AACCT1L PIC S9(0004) COMP.
           05  AACCT1F PIC  X(0001).
           05  FILLER REDEFINES AACCT1F.
               10  AACCT1A PIC  X(0001).
           05  AACCT1I PIC  X(0010).
      *    -------------------------------
           05  AACCT2L PIC S9(0004) COMP.
           05  AACCT2F PIC  X(0001).
           05  FILLER REDEFINES AACCT2F.
               10  AACCT2A PIC  X(0001).
           05  AACCT2I PIC  X(0010).
      *    -------------------------------
           05  AACCT3L PIC S9(0004) COMP.
           05  AACCT3F PIC  X(0001).
           05  FILLER REDEFINES AACCT3F.
               10  AACCT3A PIC  X(0001).
           05  AACCT3I PIC  X(0010).
      *    -------------------------------
           05  AUNKNERL PIC S9(0004) COMP.
           05  AUNKNERF PIC  X(0001).
           05  FILLER REDEFINES AUNKNERF.
               10  AUNKNERA PIC  X(0001).
           05  AUNKNERI PIC  X(0008).
      *    -------------------------------
           05  AERMSG1L PIC S9(0004) COMP.
           05  AERMSG1F PIC  X(0001).
           05  FILLER REDEFINES AERMSG1F.
               10  AERMSG1A PIC  X(0001).
           05  AERMSG1I PIC  X(0076).
      *    -------------------------------
           05  AERMSG2L PIC S9(0004) COMP.
           05  AERMSG2F PIC  X(0001).
           05  FILLER REDEFINES AERMSG2F.
               10  AERMSG2A PIC  X(0001).
           05  AERMSG2I PIC  X(0076).
      *    -------------------------------
           05  APFNTERL PIC S9(0004) COMP.
           05  APFNTERF PIC  X(0001).
           05  FILLER REDEFINES APFNTERF.
               10  APFNTERA PIC  X(0001).
           05  APFNTERI PIC  9(2).
       01  EL642AO REDEFINES EL642AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABEGBALO PIC  ZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARHDGO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRPHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUPDPRMO PIC  ZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGENTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUPDCOMO PIC  ZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APREMUMO PIC  ZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMPISO PIC  ZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACOMCANO PIC  ZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABILL1O PIC  Z,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AINER1O PIC  Z,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APRVBL1O PIC  Z,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AADJUSTO PIC  ZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABILL2O PIC  Z,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AINER2O PIC  Z,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APRVBL2O PIC  Z,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREMITO PIC  ZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADISBURO PIC  ZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACTHDGO PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACTDUEO PIC  ZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AENDHDGO PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANETDUEO PIC  ZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABILTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APRODSWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUNKNERO PIC  ZZZ,ZZZ-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AERMSG1O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AERMSG2O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFNTERO PIC  99.
      *    -------------------------------
       01  EL642BI REDEFINES EL642AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  BDATEL PIC S9(0004) COMP.
           05  BDATEF PIC  X(0001).
           05  FILLER REDEFINES BDATEF.
               10  BDATEA PIC  X(0001).
           05  BDATEI PIC  X(0008).
      *    -------------------------------
           05  BTIMEL PIC S9(0004) COMP.
           05  BTIMEF PIC  X(0001).
           05  FILLER REDEFINES BTIMEF.
               10  BTIMEA PIC  X(0001).
           05  BTIMEI PIC  X(0005).
      *    -------------------------------
           05  BCMPNYL PIC S9(0004) COMP.
           05  BCMPNYF PIC  X(0001).
           05  FILLER REDEFINES BCMPNYF.
               10  BCMPNYA PIC  X(0001).
           05  BCMPNYI PIC  X(0003).
      *    -------------------------------
           05  BUSERIDL PIC S9(0004) COMP.
           05  BUSERIDF PIC  X(0001).
           05  FILLER REDEFINES BUSERIDF.
               10  BUSERIDA PIC  X(0001).
           05  BUSERIDI PIC  X(0004).
      *    -------------------------------
           05  BCHKNOL PIC S9(0004) COMP.
           05  BCHKNOF PIC  X(0001).
           05  FILLER REDEFINES BCHKNOF.
               10  BCHKNOA PIC  X(0001).
           05  BCHKNOI PIC  9(6).
      *    -------------------------------
           05  BNAMEL PIC S9(0004) COMP.
           05  BNAMEF PIC  X(0001).
           05  FILLER REDEFINES BNAMEF.
               10  BNAMEA PIC  X(0001).
           05  BNAMEI PIC  X(0029).
      *    -------------------------------
           05  BCHKAMTL PIC S9(0004) COMP.
           05  BCHKAMTF PIC  X(0001).
           05  FILLER REDEFINES BCHKAMTF.
               10  BCHKAMTA PIC  X(0001).
           05  BCHKAMTI PIC  9(7)V99.
      *    -------------------------------
           05  BADDR1L PIC S9(0004) COMP.
           05  BADDR1F PIC  X(0001).
           05  FILLER REDEFINES BADDR1F.
               10  BADDR1A PIC  X(0001).
           05  BADDR1I PIC  X(0029).
      *    -------------------------------
           05  BADDR2L PIC S9(0004) COMP.
           05  BADDR2F PIC  X(0001).
           05  FILLER REDEFINES BADDR2F.
               10  BADDR2A PIC  X(0001).
           05  BADDR2I PIC  X(0029).
      *    -------------------------------
           05  BPAYDT1L PIC S9(0004) COMP.
           05  BPAYDT1F PIC  X(0001).
           05  FILLER REDEFINES BPAYDT1F.
               10  BPAYDT1A PIC  X(0001).
           05  BPAYDT1I PIC  X(0008).
      *    -------------------------------
           05  BCITYSTL PIC S9(0004) COMP.
           05  BCITYSTF PIC  X(0001).
           05  FILLER REDEFINES BCITYSTF.
               10  BCITYSTA PIC  X(0001).
           05  BCITYSTI PIC  X(0029).
      *    -------------------------------
           05  BPAYDT2L PIC S9(0004) COMP.
           05  BPAYDT2F PIC  X(0001).
           05  FILLER REDEFINES BPAYDT2F.
               10  BPAYDT2A PIC  X(0001).
           05  BPAYDT2I PIC  X(0008).
      *    -------------------------------
           05  BZIPL PIC S9(0004) COMP.
           05  BZIPF PIC  X(0001).
           05  FILLER REDEFINES BZIPF.
               10  BZIPA PIC  X(0001).
           05  BZIPI PIC  X(0009).
      *    -------------------------------
           05  BALAMTL PIC S9(0004) COMP.
           05  BALAMTF PIC  X(0001).
           05  FILLER REDEFINES BALAMTF.
               10  BALAMTA PIC  X(0001).
           05  BALAMTI PIC  X(0010).
      *    -------------------------------
           05  BERMSGL PIC S9(0004) COMP.
           05  BERMSGF PIC  X(0001).
           05  FILLER REDEFINES BERMSGF.
               10  BERMSGA PIC  X(0001).
           05  BERMSGI PIC  X(0078).
      *    -------------------------------
           05  BPFNTERL PIC S9(0004) COMP.
           05  BPFNTERF PIC  X(0001).
           05  FILLER REDEFINES BPFNTERF.
               10  BPFNTERA PIC  X(0001).
           05  BPFNTERI PIC  9(2).
       01  EL642BO REDEFINES EL642AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCMPNYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BUSERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCHKNOO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAMEO PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCHKAMTO PIC  Z(6).99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BADDR1O PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BADDR2O PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAYDT1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITYSTO PIC  X(0029).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAYDT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BZIPO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALAMTO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSGO PIC  X(0078).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFNTERO PIC  9(2).
      *    -------------------------------
00640  EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
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
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
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
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00642
00643  01  DFHCOMMAREA             PIC  X(1024).
00644  EJECT
00645 *01 PARMLIST             COMP.
00646 *    12  FILLER              PIC S9(8).
00647 *    12  ERPNDB-POINTER      PIC S9(8).
00648 *    12  ELCNTL-POINTER      PIC S9(8).
00649 *    12  ERCOMP-POINTER      PIC S9(8).
00650 *    12  ERPYAJ-POINTER      PIC S9(8).
00651 *    12  ERACCT-POINTER      PIC S9(8).
00652 *    12  ERBILL-POINTER      PIC S9(8).
00653 *    12  ERCOMM-POINTER      PIC S9(8).
00654 *    12  ERCTBL-POINTER      PIC S9(8).
00655 *    12  ERGXRF-POINTER      PIC S9(8).
00656 *    12  ERGXRF2-POINTER     PIC S9(8).
00657 *    12  ERGXRF3-POINTER     PIC S9(8).
00658 *    12  ERGXRF4-POINTER     PIC S9(8).
00659 *    12  ERGXRF5-POINTER     PIC S9(8).
00660 *    12  ERGXRF6-POINTER     PIC S9(8).
00661 *    12  ERGXRF7-POINTER     PIC S9(8).
00662 *    12  ERGXRF8-POINTER     PIC S9(8).
00663  EJECT
00664 *                            COPY ERCPNDB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
00008 *                                                                *
00009 ******************************************************************
00010 *   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
00011 *         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
00012 ******************************************************************
00013 *                                                                *
00014 *                                                                *
00015 *   FILE TYPE = VSAM,KSDS                                        *
00016 *   RECORD SIZE = 585  RECFORM = FIXED                           *
00017 *                                                                *
00018 *   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
00019 *       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
00020 *                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
00021 *                                                 RKP=13,LEN=36  *
00022 *       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
00023 *                                      AND CHG-SEQ.)             *
00024 *                                                RKP=49,LEN=11   *
00025 *       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
00026 *                                      AND CHG-SEQ.)             *
00027 *                                                RKP=60,LEN=15   *
00028 *                                                                *
00029 *   LOG = NO                                                     *
00030 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00031 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
040504* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
020305* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
032306* 032306                   PEMA  ADD BOW LOAN NUMBER
081606* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
073107* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
032109* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
072209* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
122002******************************************************************
00032
00033  01  PENDING-BUSINESS.
00034      12  PB-RECORD-ID                     PIC XX.
00035          88  VALID-PB-ID                        VALUE 'PB'.
00036
00037      12  PB-CONTROL-PRIMARY.
00038          16  PB-COMPANY-CD                PIC X.
00039          16  PB-ENTRY-BATCH               PIC X(6).
00040          16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
00041          16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
00042
00043      12  PB-CONTROL-BY-ACCOUNT.
00044          16  PB-COMPANY-CD-A1             PIC X.
00045          16  PB-CARRIER                   PIC X.
00046          16  PB-GROUPING.
00047              20  PB-GROUPING-PREFIX       PIC XXX.
00048              20  PB-GROUPING-PRIME        PIC XXX.
00049          16  PB-STATE                     PIC XX.
00050          16  PB-ACCOUNT.
00051              20  PB-ACCOUNT-PREFIX        PIC X(4).
00052              20  PB-ACCOUNT-PRIME         PIC X(6).
00053          16  PB-CERT-EFF-DT               PIC XX.
00054          16  PB-CERT-NO.
00055              20  PB-CERT-PRIME            PIC X(10).
00056              20  PB-CERT-SFX              PIC X.
00057          16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
00058
00059          16  PB-RECORD-TYPE               PIC X.
00060              88  PB-MAILING-DATA                VALUE '0'.
00061              88  PB-ISSUE                       VALUE '1'.
00062              88  PB-CANCELLATION                VALUE '2'.
00063              88  PB-BATCH-TRAILER               VALUE '9'.
00064
00065      12  PB-CONTROL-BY-ORIG-BATCH.
00066          16  PB-ORIGINAL-COMPANY-CD       PIC X.
00067          16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
00068          16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
00069          16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
00070
00071      12  PB-CONTROL-BY-CSR.
00072          16  PB-CSR-COMPANY-CD            PIC X.
00073          16  PB-CSR-ID                    PIC X(4).
00074          16  PB-CSR-ENTRY-BATCH           PIC X(6).
00075          16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
00076          16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
00077 ******************************************************************
00078 *    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
00079 ******************************************************************
00080
00081      12  PB-LAST-MAINT-DT                 PIC XX.
00082      12  PB-LAST-MAINT-BY                 PIC X(4).
00083      12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00084
00085      12  PB-RECORD-BODY                   PIC X(375).
00086
00087      12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
00088          16  PB-CERT-ORIGIN               PIC X.
00089              88  CLASIC-CREATED-CERT         VALUE '1'.
00090          16  PB-I-NAME.
00091              20  PB-I-INSURED-LAST-NAME   PIC X(15).
00092              20  PB-I-INSURED-FIRST-NAME.
00093                  24  PB-I-INSURED-1ST-INIT PIC X.
00094                  24  FILLER                PIC X(9).
00095              20  PB-I-INSURED-MIDDLE-INIT PIC X.
00096          16  PB-I-AGE                     PIC S99   COMP-3.
00097          16  PB-I-JOINT-AGE               PIC S99   COMP-3.
00098          16  PB-I-BIRTHDAY                PIC XX.
00099          16  PB-I-INSURED-SEX             PIC X.
00100              88  PB-SEX-MALE     VALUE 'M'.
00101              88  PB-SEX-FEMALE   VALUE 'F'.
00102
00103          16  PB-I-LF-TERM                 PIC S999   COMP-3.
00104          16  PB-I-AH-TERM                 PIC S999   COMP-3.
00105          16  PB-I-LOAN-TERM               PIC S999   COMP-3.
00106          16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
00107          16  PB-I-SKIP-CODE               PIC X.
00108              88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
00109              88  PB-SKIP-JULY              VALUE '1'.
00110              88  PB-SKIP-AUGUST            VALUE '2'.
00111              88  PB-SKIP-SEPTEMBER         VALUE '3'.
00112              88  PB-SKIP-JULY-AUG          VALUE '4'.
00113              88  PB-SKIP-AUG-SEPT          VALUE '5'.
00114              88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
00115              88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
00116              88  PB-SKIP-JUNE              VALUE '8'.
00117              88  PB-SKIP-JUNE-JULY         VALUE '9'.
00118              88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
00119              88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
00120          16  PB-I-TERM-TYPE               PIC X.
00121              88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
00122              88  PB-PAID-WEEKLY            VALUE 'W'.
00123              88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
00124              88  PB-PAID-BI-WEEKLY         VALUE 'B'.
00125              88  PB-PAID-13-YEARLY         VALUE 'T'.
00126          16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
00127          16  PB-I-POLICY-FORM-NO          PIC X(12).
00128          16  PB-I-DATA-ENTRY-SW           PIC X.
00129              88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
00130              88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
00131              88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
00132              88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
00133          16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
073107         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
011410*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
011410         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
011410         16  FILLER                       PIC X.
00136
00137          16  PB-I-LIFE-BENEFIT-CD         PIC XX.
00138              88  PB-VALID-LIFE               VALUE '01' THRU '89'.
00139              88  PB-INVALID-LIFE             VALUE '  ' '00'
00140                                                    '90' THRU '99'.
00141          16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
00142                                           PIC XX.
00143          16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
100703         16  PB-I-AMOUNT-FINANCED REDEFINES
100703                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
00144          16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
100703         16  PB-I-UNPAID-CASH-PRICE REDEFINES
100703                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
00145          16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00146          16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
100703         16  PB-I-CLP-AMOUNT REDEFINES
100703                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
00147          16  PB-I-LF-CALC-FLAG            PIC X.
00148              88 PB-COMP-LF-PREM               VALUE '?'.
00149          16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
00150          16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
00151          16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
00152          16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
00153          16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
00154          16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
00155          16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
00156          16  PB-I-LF-ABBR                 PIC XXX.
00157          16  PB-I-LF-INPUT-CD             PIC XX.
00158
00159          16  PB-I-AH-BENEFIT-CD           PIC XX.
00160              88  PB-VALID-AH                 VALUE '01' THRU '89'.
00161              88  PB-INVALID-AH               VALUE '  ' '00'
00162                                                    '90' THRU '99'.
00163          16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
00164          16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00165          16  PB-I-AH-CALC-FLAG            PIC X.
00166              88 PB-COMP-AH-PREM                  VALUE '?'.
00167          16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
00168          16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
00169          16  PB-I-AH-POLICY-FEE           PIC S9(3)V99   COMP-3.
00170          16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
00171          16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
00172          16  PB-I-AH-ABBR                 PIC XXX.
00173          16  PB-I-AH-INPUT-CD             PIC XXX.
00174
00175          16  PB-I-SPECIAL-REIN-CODE       PIC X.
00176          16  PB-I-REIN-TABLE              PIC XXX.
00177          16  PB-I-BUSINESS-TYPE           PIC 99.
00178          16  PB-I-INDV-GRP-CD             PIC X.
00179          16  PB-I-MORT-CODE.
00180              20  PB-I-TABLE               PIC X.
00181              20  PB-I-INTEREST            PIC XX.
00182              20  PB-I-MORT-TYP            PIC X.
00183          16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
00184          16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
011410         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
00186          16  PB-I-INDV-GRP-OVRD           PIC X.
00187          16  PB-I-RATE-CLASS-OVRD         PIC XX.
00188          16  PB-I-SIG-SW                  PIC X.
00189              88  PB-POLICY-SIGNED             VALUE 'Y'.
00190          16  PB-I-RATE-CLASS              PIC XX.
00191          16  PB-I-RATE-DEVIATION-LF       PIC XXX.
00192          16  PB-I-RATE-DEVIATION-AH       PIC XXX.
00193          16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
00194          16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
00195          16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
00196          16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
00197          16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
00198          16  PB-I-BENEFIT-TYPE            PIC XXX.
00199          16  PB-I-OB-FLAG                 PIC X.
00200              88  PB-I-OB                      VALUE 'B'.
00201              88  PB-I-SUMMARY                 VALUE 'Z'.
00202          16  PB-I-ENTRY-STATUS            PIC X.
00203              88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
122002                                              'M' '5' '9' '2'.
00205              88  PB-I-NORMAL-ENTRY            VALUE '1'.
00206              88  PB-I-POLICY-PENDING          VALUE '2'.
00207              88  PB-I-CONVERSION-ENTRY        VALUE '4'.
00208              88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
122002             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
00209              88  PB-I-REIN-ONLY               VALUE '9'.
00210              88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
00211              88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
00212              88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
00213              88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
00214          16  PB-I-INT-CODE                PIC X.
00215              88  PB-ADD-ON-INTEREST           VALUE 'A'.
00216              88  PB-SIMPLE-INTEREST           VALUE 'S'.
00217          16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
00218          16  PB-I-SOC-SEC-NO              PIC X(11).
00219          16  PB-I-MEMBER-NO               PIC X(12).
00220          16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
110105*        16  PB-I-LOAN-OFFICER            PIC XXX.
110105         16  PB-I-OLD-LOF                 PIC XXX.
00222          16  PB-I-LF-EXPIRE-DT            PIC XX.
00223          16  PB-I-AH-EXPIRE-DT            PIC XX.
00224          16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
00225          16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
00226          16  PB-I-LIFE-INDICATOR          PIC X.
00227              88  PB-I-JOINT-COVERAGE         VALUE 'J'.
00228          16  PB-I-LIVES                   PIC S9(7)       COMP-3.
00229          16  PB-I-MAIL-ADDRS-SW           PIC X.
00230              88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
00231              88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
00232          16  PB-I-1ST-PMT-DT              PIC XX.
00233          16  PB-I-JOINT-INSURED.
00234              20 PB-I-JOINT-LAST-NAME      PIC X(15).
00235              20 PB-I-JOINT-FIRST-NAME.
00236                 24  PB-I-JOINT-FIRST-INIT PIC X.
00237                 24  FILLER                PIC X(9).
00238              20 PB-I-JOINT-MIDDLE-INIT    PIC X.
100703*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
100703         16  PB-I-BENEFICIARY-NAME.
100703             20  PB-I-BANK-NUMBER         PIC X(10).
100703             20  FILLER                   PIC X(15).
00240          16  PB-I-LAST-ADD-ON-DT          PIC XX.
011904         16  PB-I-REFERENCE               PIC X(12).
011904         16  FILLER REDEFINES PB-I-REFERENCE.
011904             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
011904             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
020305             20  PB-I-CLP-STATE           PIC XX.
00242          16  PB-I-UNDERWRITING-STATUS     PIC X.
00243              88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
00244              88  PB-I-POLICY-DECLINED         VALUE 'D'.
00245              88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
00246          16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
00247          16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
00248          16  PB-I-RESIDENT-STATE          PIC XX.
00249          16  PB-I-RATE-CODE               PIC X(4).
00250          16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
PEMMOD         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
100703         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
100703         16  PB-I-BANK-NOCHRGB            PIC 99.
040504         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
081108         16  PB-I-JOINT-BIRTHDAY          PIC XX.
00252
00253      12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
00254          16  PB-C-LF-CANCEL-VOID-SW       PIC X.
00255              88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
00256          16  PB-C-CANCEL-ORIGIN           PIC X.
00257              88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
00258          16  PB-C-LF-CANCEL-DT            PIC XX.
00259          16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00260          16  PB-C-LF-CALC-REQ             PIC X.
00261              88 PB-COMP-LF-CANCEL            VALUE '?'.
00262          16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
00263          16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
00264          16  PB-C-AH-CANCEL-VOID-SW       PIC X.
00265              88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
00266          16  PB-C-AH-CANCEL-DT            PIC XX.
00267          16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00268          16  PB-C-AH-CALC-REQ             PIC X.
00269              88 PB-COMP-AH-CANCEL            VALUE '?'.
00270          16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
00271          16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
00272          16  PB-C-LAST-NAME               PIC X(15).
00273          16  PB-C-REFUND-SW               PIC X.
00274              88  PB-C-REFUND-CREATED          VALUE 'Y'.
00275              88  PB-C-REFUND-REQUESTED        VALUE 'R'.
00276          16  PB-C-LIVES                   PIC S9(3)       COMP-3.
00277          16  PB-C-PAYEE-CODE              PIC X(6).
00278          16  PB-C-LF-REFUND-OVERRIDE      PIC X.
00279          16  PB-C-AH-REFUND-OVERRIDE      PIC X.
00280          16  PB-C-LF-COMM-CHARGEBACK      PIC X.
00281          16  PB-C-AH-COMM-CHARGEBACK      PIC X.
00282          16  PB-C-REFERENCE               PIC X(12).
PEMMOD         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
081606         16  PB-C-POST-CARD-IND           PIC X.
081606         16  PB-C-CANCEL-REASON           PIC X.
072308         16  PB-C-REF-INTERFACE-SW        PIC X.
00283          16  FILLER                       PIC X(09).
PEMMOD*        16  FILLER                       PIC X(18).
00284          16  PB-C-POLICY-FORM-NO          PIC X(12).
072308*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
072308         16  PB-C-NH-INT-ON-REFS          PIC S9(7)V99   COMP-3.
00286          16  PB-CANCELED-CERT-DATA.
00287              20  PB-CI-INSURED-NAME.
00288                  24  PB-CI-LAST-NAME      PIC X(15).
00289                  24  PB-CI-INITIALS       PIC XX.
00290              20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
00291              20  PB-CI-INSURED-SEX        PIC X.
00292              20  PB-CI-LF-TERM            PIC S999        COMP-3.
00293              20  PB-CI-LF-BENEFIT-CD      PIC XX.
00294              20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
00295              20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00296              20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00297              20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
00298              20  PB-CI-AH-TERM            PIC S999        COMP-3.
00299              20  PB-CI-AH-BENEFIT-CD      PIC XX.
00300              20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00301              20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00302              20  PB-CI-RATE-CLASS         PIC XX.
00303              20  PB-CI-RATE-DEV-LF        PIC XXX.
00304              20  PB-CI-RATE-DEV-AH        PIC XXX.
00305              20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
00306              20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
00307              20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
00308              20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
00309              20  PB-CI-LF-ABBR            PIC X(3).
00310              20  PB-CI-AH-ABBR            PIC X(3).
00311              20  PB-CI-OB-FLAG            PIC X.
00312                  88  PB-CI-OB                VALUE 'B'.
00313              20  PB-CI-LF-POLICY-STATUS   PIC X.
00314                  88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00316                  88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
00317                  88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
00318                  88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
00319                  88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
00320                  88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
122002                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00321                  88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
00322                  88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00323                  88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
00324                  88  PB-CI-LF-REIN-ONLY              VALUE '9'.
00325                  88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
00326                  88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
00327              20  PB-CI-AH-POLICY-STATUS   PIC X.
00328                  88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00330                  88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
00331                  88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
00332                  88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
00333                  88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
00334                  88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
122002                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00335                  88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
00336                  88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00337                  88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
00338                  88  PB-CI-AH-REIN-ONLY              VALUE '9'.
00339                  88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
00340                  88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
00341              20  PB-CI-PAY-FREQUENCY      PIC 99.
00342              20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00343              20  PB-CI-SOC-SEC-NO         PIC X(11).
00344              20  PB-CI-MEMBER-NO          PIC X(12).
00345              20  PB-CI-INT-CODE           PIC X.
00346                  88  PB-CI-ADD-ON                  VALUE 'A'.
00347                  88  PB-CI-SIMPLE                  VALUE 'S'.
00348              20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
00349              20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
00350              20  PB-CI-COMP-EXCP-SW       PIC X.
00351                  88  PB-CI-NO-COMP-EXCP            VALUE ' '.
00352                  88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
00353              20  PB-CI-ENTRY-STATUS       PIC X.
00354              20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
00355              20  PB-CI-AH-PAID-THRU-DT    PIC XX.
00356              20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
00357              20  PB-CI-DEATH-DT           PIC XX.
00358              20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
00359              20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
00360              20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00361              20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00362              20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
00363              20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
00364              20  PB-CI-ENTRY-DT              PIC XX.
00365              20  PB-CI-ENTRY-BATCH           PIC X(6).
00366              20  PB-CI-LF-EXPIRE-DT          PIC XX.
00367              20  PB-CI-AH-EXPIRE-DT          PIC XX.
00368              20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
00369              20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
110105             20  PB-CI-OLD-LOF               PIC XXX.
110105*            20  PB-CI-LOAN-OFFICER          PIC XXX.
00371              20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
00372              20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
00373              20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
00374              20  PB-CI-INDV-GRP-CD           PIC X.
100703             20  PB-CI-BENEFICIARY-NAME.
100703                 24  PB-CI-BANK-NUMBER       PIC X(10).
100703                 24  FILLER                  PIC X(15).
00376              20  PB-CI-NOTE-SW               PIC X.
00377              20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
00378              20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
00379              20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
040504             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
110105             20  PB-CI-LOAN-OFFICER          PIC X(5).
032306             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
072209             20  PB-CI-FIRST-NAME            PIC X(10).
00380
072209         16  FILLER                       PIC X(17).
072209*032306  16  FILLER                       PIC X(27).
040504*        16  FILLER                       PIC X(46).
00382
00383      12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
00384          16  FILLER                       PIC X(10).
00385          16  PB-M-INSURED-LAST-NAME       PIC X(15).
00386          16  PB-M-INSURED-FIRST-NAME      PIC X(10).
00387          16  PB-M-INSURED-MID-INIT        PIC X.
00388          16  PB-M-INSURED-AGE             PIC 99.
00389          16  PB-M-INSURED-BIRTHDAY        PIC XX.
00390          16  PB-M-INSURED-SEX             PIC X.
00391          16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
00392          16  PB-M-INSURED-ADDRESS-1       PIC X(30).
00393          16  PB-M-INSURED-ADDRESS-2       PIC X(30).
00394          16  PB-M-INSURED-CITY-STATE.
051810             20  PB-M-INSURED-CITY        PIC X(28).
051810             20  PB-M-INSURED-STATE       PIC XX.
00395          16  PB-M-INSURED-ZIP-CODE.
00396              20  PB-M-INSURED-ZIP-PRIME.
00397                  24  PB-M-INSURED-ZIP-1   PIC X.
00398                      88  PB-M-CANADIAN-POST-CODE
00399                                              VALUE 'A' THRU 'Z'.
00400                  24  FILLER               PIC X(4).
00401              20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
00402          16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
00403                                         PB-M-INSURED-ZIP-CODE.
00404              20  PM-M-INS-CAN-POST1       PIC XXX.
00405              20  PM-M-INS-CAN-POST2       PIC XXX.
00406              20  FILLER                   PIC XXX.
00407          16  PB-M-INSURED-PHONE-NO        PIC 9(10).
081108         16  PB-M-JOINT-BIRTHDAY          PIC XX.
               16  PB-M-CRED-BENE-NAME          PIC X(30).
               16  PB-M-CRED-BENE-ADDR1         PIC X(30).
               16  PB-M-CRED-BENE-ADDR2         PIC X(30).
               16  PB-M-CRED-BENE-CITYST.
                   20  PB-M-CRED-BENE-CITY      PIC X(28).
                   20  PB-M-CRED-BENE-STATE     PIC XX.
081108         16  FILLER                       PIC X(92).
00409
00410      12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
00411          16  FILLER                       PIC X(10).
00412          16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00413          16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00414          16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00415          16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00416          16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00417          16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00418          16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00419          16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00420          16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00421          16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00422          16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00423          16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00424          16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
00425          16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
00426          16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
00427          16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
00428          16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
00429          16  PB-ACCOUNT-NAME              PIC X(30).
00430          16  PB-PREM-REF-RPT-FLAG         PIC X.
00431          16  PB-REFERENCE                 PIC X(12).
00432          16  PB-B-RECEIVED-DT             PIC XX.
00433          16  FILLER                       PIC X(234).
00434
00435      12  PB-RECORD-STATUS.
00436          16  PB-CREDIT-SELECT-DT          PIC XX.
00437          16  PB-CREDIT-ACCEPT-DT          PIC XX.
00438          16  PB-BILLED-DT                 PIC XX.
00439          16  PB-BILLING-STATUS            PIC X.
00440              88  PB-ENTRY-REVERSED            VALUE 'R'.
00441              88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
00442              88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
00443          16  PB-RECORD-BILL               PIC X.
00444              88  PB-RECORD-ON-HOLD            VALUE 'H'.
00445              88  PB-RECORD-RETURNED           VALUE 'R'.
00446              88  PB-RECORD-ENDORSED           VALUE 'E'.
00447              88  PB-OVERRIDE-LIFE             VALUE 'L'.
00448              88  PB-OVERRIDE-AH               VALUE 'A'.
00449              88  PB-OVERRIDE-BOTH             VALUE 'B'.
00450          16  PB-BATCH-ENTRY               PIC X.
00451              88  PB-POLICY-IS-DECLINED        VALUE 'D'.
00452              88  PB-REIN-ONLY-CERT            VALUE 'R'.
00453              88  PB-REISSUED-CERT             VALUE 'E'.
122002             88  PB-MONTHLY-CERT              VALUE 'M'.
00454              88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
00455              88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
00456              88  PB-POLICY-IS-VOIDED          VALUE 'V'.
00457          16  PB-FORCE-CODE                PIC X.
00458              88  PB-FORCE-OFF                 VALUE ' ' '0'.
00459              88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
00460              88  PB-CANCEL-FORCE              VALUE '8'.
00461              88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
00462              88  PB-ALL-CANCEL-FORCED         VALUE '8'.
00463              88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
00464              88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
00465              88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
00466              88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
073107             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
00467          16  PB-FATAL-FLAG                PIC X.
00468              88  PB-FATAL-ERRORS              VALUE 'X'.
00469          16  PB-FORCE-ER-CD               PIC X.
00470              88  PB-FORCE-ERRORS              VALUE 'F'.
00471              88  PB-UNFORCED-ERRORS           VALUE 'X'.
00472          16  PB-WARN-ER-CD                PIC X.
00473              88  PB-WARNING-ERRORS            VALUE 'W'.
00474          16  FILLER                       PIC X.
00475          16  PB-OUT-BAL-CD                PIC X.
00476              88  PB-OUT-OF-BAL                VALUE 'O'.
00477          16  PB-LIFE-OVERRIDE-L1          PIC X.
00478          16  PB-AH-OVERRIDE-L1            PIC X.
00479          16  PB-INPUT-DT                  PIC XX.
00480          16  PB-INPUT-BY                  PIC X(4).
00481          16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
00482          16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
00483          16  PB-TOLERANCE-REJECT-SW       PIC X.
00484          16  PB-LF-EARNING-METHOD         PIC X.
00485          16  PB-AH-EARNING-METHOD         PIC X.
00486          16  PB-LF-TERM-CALC-METHOD       PIC X.
00487          16  PB-AH-TERM-CALC-METHOD       PIC X.
00488          16  PB-REIN-CD                   PIC XXX.
00489          16  PB-LF-REFUND-TYPE            PIC X.
00490          16  PB-AH-REFUND-TYPE            PIC X.
00491          16  PB-ACCT-EFF-DT               PIC XX.
00492          16  PB-ACCT-EXP-DT               PIC XX.
00493          16  PB-COMPANY-ID                PIC X(3).
00494          16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00495          16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00496          16  PB-SV-CARRIER                PIC X.
00497          16  PB-SV-GROUPING               PIC X(6).
00498          16  PB-SV-STATE                  PIC XX.
00499          16  PB-CONFIRMATION-REPT-DT      PIC XX.
00500          16  PB-GA-BILLING-INFO.
00501              20  PB-GA-BILL-DT OCCURS 5 TIMES
00502                                           PIC XX.
00503          16  PB-SV-REMIT-TO  REDEFINES
00504              PB-GA-BILLING-INFO           PIC X(10).
00505          16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
110105         16  PB-I-LOAN-OFFICER            PIC X(5).
081606         16  PB-I-VIN                     PIC X(17).
00506
110105         16  FILLER                       PIC X(04).
110105         16  IMNET-BYPASS-SW              PIC X.
00508
00509 ******************************************************************
00510 *                COMMON EDIT ERRORS                              *
00511 ******************************************************************
00512
00513      12  PB-COMMON-ERRORS.
00514          16  PB-COMMON-ERROR    OCCURS 10 TIMES
00515                                            PIC S9(4)     COMP.
00516
00517 ******************************************************************
00665  EJECT
00666 *                            COPY ELCCNTL.
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
031808
031808         16  FILLER                         PIC X(82).
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
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
00498          16  FILLER                             PIC  X(240).
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
00607              20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
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
011410         16  FILLER                         PIC X(187).
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
082603             20  FILLER                     PIC X(11).
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
092705         16  FILLER                         PIC X(448).
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
00667  EJECT
00668 *                            COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
00669  EJECT
00670 *                            COPY ERCPYAJ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCPYAJ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.015                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING PAYMENT AND ADJUSTMENTS           *
00008 *                                                                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 200  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERPYAJ                         RKP=2,LEN=33   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
042303******************************************************************
042303*                   C H A N G E   L O G
042303*
042303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
042303*-----------------------------------------------------------------
042303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
042303* EFFECTIVE    NUMBER
042303*-----------------------------------------------------------------
042303* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
060205* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
042303******************************************************************
00019
00020  01  PENDING-PAY-ADJ.
00021      12  PY-RECORD-ID                     PIC XX.
00022          88  VALID-PY-ID                        VALUE 'PY'.
00023
00024      12  PY-CONTROL-PRIMARY.
00025          16  PY-COMPANY-CD                PIC X.
00026          16  PY-CARRIER                   PIC X.
00027          16  PY-GROUPING                  PIC X(6).
00028          16  PY-FIN-RESP                  PIC X(10).
00029          16  PY-ACCOUNT                   PIC X(10).
00030          16  PY-PRODUCER REDEFINES PY-ACCOUNT
00031                                           PIC X(10).
00032          16  PY-FILE-SEQ-NO               PIC S9(8)     COMP.
00033          16  PY-RECORD-TYPE               PIC X.
00034              88  PY-REMIT-RECEIVED            VALUE 'R'.
00035              88  PY-DEPOSIT                   VALUE 'D'.
00036              88  PY-CHARGE-TO-AGENT           VALUE 'C'.
00037              88  PY-ADJ-REM-RECEIVED          VALUE 'S'.
00038              88  PY-ADJ-DEPOSIT               VALUE 'T'.
00039              88  PY-ADJ-CHG-TO-AGT            VALUE 'U'.
00040              88  PY-ADD-TO-YTD-COMP           VALUE 'X'.
00041              88  PY-SUBTRACT-YTD-COMP         VALUE 'Y'.
00042              88  PY-ADD-TO-BALANCE            VALUE 'Z'.
00043              88  PY-FICA-ENTRY                VALUE 'F'.
00044              88  PY-REMIT-IND-GROUPING        VALUE 'G'.
00045              88  PY-POLICY-FEE                VALUE 'W'.
042303             88  PY-DUE-PREM-ADJ              VALUE 'P'.
00046
00047      12  PY-PYMT-TYPE                     PIC X.
00048              88  PY-NEW-BUS-PYMT              VALUE 'B'.
00049              88  PY-REINS-PYMT                VALUE 'R'.
00050              88  PY-EXP-PYMT                  VALUE 'E'.
00051
00052      12  PY-BIL-INV                       PIC X(6).
00053      12  PY-REF-NO                        PIC X(12).
00054
00055      12  PY-LAST-MAINT-DT                 PIC XX.
00056      12  PY-LAST-MAINT-BY                 PIC X(4).
00057      12  PY-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00058
00059      12  PY-PYADJ-RECORD.
00060          16  PY-ENTRY-AMT                 PIC S9(7)V99  COMP-3.
00061          16  PY-ENTRY-COMMENT             PIC X(30).
CIDMOD         16  PY-GL-DATA      REDEFINES PY-ENTRY-COMMENT.
CIDMOD             20  PY-GL-ACCOUNT            PIC X(10).
CIDMOD             20  PY-GL-STATE              PIC X(02).
CIDMOD             20  PY-GL-CANC-SW            PIC X(01).
CIDMOD                 88  PY-GL-CANC-SW-ON     VALUE 'Y'.
CIDMOD                 88  PY-GL-CANC-SW-OFF    VALUE 'N'.
CIDMOD             20  PY-GL-COMMENT            PIC X(10).
CIDMOD             20  FILLER      REDEFINES PY-GL-COMMENT.
CIDMOD                 24  PY-GL-CHECK-NO       PIC 9(06).
CIDMOD                 24  FILLER               PIC X(04).
CIDMOD             20  FILLER                   PIC X(07).
00074          16  PY-SAVE-ACCOUNT              PIC X(10).
00075          16  PY-SAVE-TYPE                 PIC X(01).
00076
00077          16  PY-LETTERS.
00078              20  PY-LETTER OCCURS 3 TIMES
00079                            INDEXED BY PY-LET-NDX
00080                                           PIC X(04).
00081
060205         16  PY-ERCOMP-TYPE               PIC X.
060205             88  PY-ACCOUNT-TYPE              VALUE 'A'.
060205             88  PY-GA-TYPE                   VALUE 'G'.
060205             88  PY-BANK-TYPE                 VALUE 'B'.
060205         16  FILLER                       PIC X(05).
00083
00084      12  PY-RECORD-STATUS.
00085          16  PY-CREDIT-SELECT-DT          PIC XX.
00086          16  PY-CREDIT-ACCEPT-DT          PIC XX.
00087          16  PY-BILLED-DATE               PIC XX.
00088          16  PY-REPORTED-DT               PIC XX.
00089          16  PY-PMT-APPLIED               PIC X.
00090              88  PY-ACCOUNT-PMT               VALUE 'A'.
00091              88  PY-GA-PMT                    VALUE 'G'.
00092              88  PY-OVWRITE-PMT               VALUE 'O'.
00093              88  PY-NON-AR-PMT                VALUE 'N'.
00094          16  FILLER                       PIC X(5).
00095          16  PY-INPUT-DT                  PIC XX.
00096          16  PY-CHECK-NUMBER              PIC X(6).
00097          16  PY-VOID-SW                   PIC X.
00098              88  PY-CHECK-VOIDED              VALUE 'V'.
00099          16  PY-CHECK-ORIGIN-SW           PIC X.
00100              88  PY-BILLING-CHECK             VALUE 'B'.
00101              88  PY-REFUND-CHECK              VALUE 'R'.
00102              88  PY-GA-CHECK                  VALUE 'G'.
00103              88  PY-CHECK-WRITTEN             VALUE 'W'.
00104              88  PY-CHECK-REVERSAL            VALUE 'V'.
00105          16  PY-CHECK-WRITTEN-DT          PIC XX.
00106          16  PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.
00107          16  PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.
00108          16  PY-BILL-FLAG                 PIC X.
00109              88  PY-BILLED                    VALUE 'B'.
00110          16  PY-AR-FLAG                   PIC X.
00111              88  PY-AR-CYCLE                  VALUE 'C'.
00112              88  PY-AR-MONTH-END              VALUE 'M'.
00113          16  PY-AR-DATE                   PIC XX.
00114
00115      12  PY-GL-CODES.
00116          16  PY-GL-DB                     PIC X(14).
00117          16  PY-GL-CR                     PIC X(14).
00118          16  PY-GL-FLAG                   PIC X.
00119          16  PY-GL-DATE                   PIC XX.
00120
00121      12  PY-CANCEL-FEE-FLAG               PIC X(2).
00122      12  FILLER                           PIC X(3).
00123 ******************************************************************
00671  EJECT
00672 *                            COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
090803     12  FILLER                            PIC X(5).
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
00673  EJECT
00674 *                            COPY ERCBILL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCBILL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = BILLING STATEMENTS FOR PRINTING           *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 210  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERBILL                        RKP=2,LEN=31    *
00013 *                                                                *
00014 *   LOG = NO                                                     *
00015 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
00017  01  BILLING-STATEMENT.
00018      02 BILLING-STATEMENT-FILE.
00019      12  BI-RECORD-ID                PIC XX.
00020          88  VALID-BI-ID                VALUE 'BI'.
00021
00022      12  BI-CONTROL-PRIMARY.
00023          16  BI-COMPANY-CD           PIC X.
00024          16  BI-CARRIER              PIC X.
00025          16  BI-GROUPING             PIC X(6).
00026          16  BI-ACCOUNT              PIC X(10).
00027          16  BI-FIN-RESP             PIC X(10).
00028          16  BI-RECORD-TYPE          PIC X.
00029              88  BI-HEADER-DATA         VALUE '1'.
00030              88  BI-ADDRESS-DATA        VALUE '2'.
00031              88  BI-TEXT-DATA           VALUE '3'.
00032          16  BI-LINE-SEQ-NO          PIC S9(4)     COMP.
00033
00034      12  BI-TEXT-RECORD.
00035          16  BI-SKIP-CONTROL         PIC X.
00036          16  BI-TEXT-LINE            PIC X(157).
00037          16  BI-TEXT-LINE-1 REDEFINES BI-TEXT-LINE.
00038              20  BI-ADDR-LIT         PIC X(14).
00039              20  BI-CO               PIC X(7).
00040              20  BI-DASH             PIC X.
00041              20  BI-ACCT             PIC X(10).
00042              20  FILLER              PIC XX.
00043              20  BI-ACCT-ADDR        PIC X(30).
00044              20  FILLER              PIC X.
00045              20  BI-REMIT-LIT        PIC X(11).
00046              20  FILLER              PIC XX.
00047              20  BI-REMIT-ADDR       PIC X(30).
00048              20  FILLER              PIC X(49).
00049          16  BI-TEXT-LINE-2 REDEFINES BI-TEXT-LINE.
00050              20  BI-INS-LAST-NAME    PIC X(15).
00051              20  FILLER              PIC X.
00052              20  BI-INS-1ST-NAME     PIC X(10).
00053              20  FILLER REDEFINES BI-INS-1ST-NAME.
00054                  24  BI-INS-INITS    PIC XX.
00055                  24  FILLER          PIC X(8).
00056              20  FILLER              PIC X.
00057              20  BI-UNDRWRTR.
00058                  24  BI-INS-INIT     PIC X.
00059                  24  FILLER          PIC XXX.
00060              20  BI-CERT             PIC X(11).
00061              20  FILLER              PIC XX.
00062              20  BI-EFF-DT           PIC X(8).
00063              20  FILLER              PIC XX.
00064              20  BI-CAN-DT           PIC X(8).
00065              20  FILLER              PIC XX.
00066              20  BI-ED-TERM          PIC ZZZ.
00067              20  FILLER              PIC XX.
00068              20  BI-TYPE             PIC XXXX.
00069              20  FILLER              PIC X.
00070              20  BI-PREM             PIC ZZ,ZZZ,ZZZ.ZZ-.
00071              20  FILLER              PIC X(3).
00072              20  BI-ED-RATE          PIC ZZZ.ZZZ.
00073              20  FILLER              PIC XX.
00074              20  BI-COMM             PIC ZZ,ZZZ,ZZZ.ZZ-.
00075              20  FILLER              PIC XX.
00076              20  BI-FACE-AMT         PIC ZZZ,ZZZ,ZZZ.ZZ-.
00077              20  FILLER              PIC X(26).
00078          16  BI-TEXT-LINE-3 REDEFINES BI-TEXT-LINE.
00079              20  FILLER              PIC X(42).
00080              20  BI-TOT-DESC         PIC X(20).
00081              20  FILLER REDEFINES BI-TOT-DESC.
00082                  24  BI-TOT-LIT      PIC X(6).
00083                  24  BI-OVERRIDE-L6  PIC X(14).
00084              20  FILLER              PIC X(11).
00085              20  BI-TOT-PREM         PIC ZZZ,ZZZ,ZZZ.99-.
00086              20  BI-TOT-DASH REDEFINES
00087                  BI-TOT-PREM         PIC X(15).
00088              20  FILLER              PIC X(11).
00089              20  BI-COM-TOT          PIC ZZZ,ZZZ,ZZZ.99-.
00090              20  FILLER              PIC XX.
00091              20  BI-FACE-TOT         PIC ZZZ,ZZZ,ZZZ.99-.
00092              20  FILLER              PIC X(26).
00093          16  BI-TEXT-LINE-4 REDEFINES BI-TEXT-LINE.
00094              20  BI-ENTRY-DESC       PIC X(30).
00095              20  FILLER              PIC X(11).
00096              20  BI-ENTRY-AMT        PIC ZZZZ,ZZZ,ZZZ.99-.
00097              20  FILLER              PIC X(31).
00098              20  BI-ACCTG-COMMENT    PIC X(30).
00099              20  FILLER              PIC X(39).
00100          16  BI-TEXT-LINE-5 REDEFINES BI-TEXT-LINE.
00101              20  FILLER              PIC X(42).
00102              20  BI-TOT-DESC5        PIC X(20).
00103              20  FILLER              PIC X(11).
00104              20  BI-TOT-PREM5        PIC ZZZ,ZZZ,ZZZ.99-.
00105              20  BI-COM-TOT5         PIC ZZZ,ZZZ,ZZZ.99-.
00106              20  BI-NON-PREM5        PIC ZZZ,ZZZ,ZZZ.99-.
00107              20  BI-NON-COMM5        PIC ZZZ,ZZZ,ZZZ.99-.
00108              20  FILLER              PIC X(24).
00109          16  BI-TEXT-FIRST REDEFINES BI-TEXT-LINE.
00110              20  BI-TEXT-2-81        PIC X(80).
00111              20  FILLER              PIC X(77).
00112          16  BI-TEXT-LAST REDEFINES BI-TEXT-LINE.
00113              20  FILLER              PIC X(53).
00114              20  BI-TEXT-55-133      PIC X(79).
00115              20  FILLER              PIC X(25).
00116          16  BI-TEXT-TYPE            PIC X.
00117              88 DETAIL-LINE              VALUE 'D'.
00118          16  BI-TERM                 PIC S999.
00119          16  BI-BENEFIT-AMT          PIC S9(9)V99 COMP-3.
00120          16  BI-PREMIUM-AMT          PIC S9(7)V99 COMP-3.
00121          16  BI-RATE                 PIC S99V9(5) COMP-3.
00122
00123      12  BI-ADDRESS-RECORD  REDEFINES  BI-TEXT-RECORD.
00124          16  FILLER                  PIC X.
00125          16  BI-ACCT-ADDRESS-LINE    PIC X(30).
00126          16  FILLER                  PIC X(10).
00127          16  BI-REMIT-ADDRESS-LINE   PIC X(30).
00128          16  FILLER                  PIC X(106).
00129
00130      12  BI-HEADER-RECORD  REDEFINES  BI-TEXT-RECORD.
00131          16  BI-PROCESSOR-CD         PIC X(4).
00132          16  BI-STATEMENT-TYPE       PIC X.
00133              88  BI-PREVIEW-ONLY         VALUE 'P'.
00134          16  BI-NO-OF-COPIES         PIC S9.
00135          16  BI-CREATION-DT          PIC XX.
00136          16  BI-INITIAL-PRINT-DATE   PIC XX.
00137          16  BI-ACCOUNT-TOTALS.
00138              20  BI-BAL-FRWD         PIC S9(9)V99     COMP-3.
00139              20  BI-PREMIUM          PIC S9(9)V99     COMP-3.
00140              20  BI-REMITTED         PIC S9(9)V99     COMP-3.
00141              20  BI-TOT-ISS-COMP     PIC S9(9)V99     COMP-3.
00142              20  BI-TOT-CAN-COMP     PIC S9(9)V99     COMP-3.
00143              20  BI-ADJUSTMNTS       PIC S9(9)V99     COMP-3.
00144              20  BI-DISBURSED        PIC S9(9)V99     COMP-3.
00145              20  BI-END-BAL          PIC S9(9)V99     COMP-3.
00146          16  BI-FIN-RESP-ACCT        PIC X(10).
00147          16  BI-FIN-RESP-NAME        PIC X(30).
00148          16  FILLER                  PIC X(79).
00149
00150
00151      02 GA-BILLING-STATEMENT REDEFINES BILLING-STATEMENT-FILE.
00152      12  FILLER                      PIC XX.
00153
00154      12  GA-CONTROL-PRIMARY.
00155          16  FILLER                  PIC X(31).
00156
00157      12  GA-TEXT-RECORD.
00158          16  GA-SKIP-CONTROL         PIC X.
00159          16  GA-TEXT-LINE            PIC X(132).
00160          16  GA-TEXT-LINE-1 REDEFINES GA-TEXT-LINE.
00161              20  FILLER              PIC X.
00162              20  GA-CARRIER          PIC X.
00163              20  GA-GROUPING         PIC X(6).
00164              20  GA-DASH             PIC X.
00165              20  GA-AGENT            PIC X(10).
00166              20  FILLER              PIC X.
00167              20  GA-AGENT-ADDR       PIC X(30).
00168              20  FILLER              PIC X(82).
00169          16  GA-TEXT-LINE-2 REDEFINES GA-TEXT-LINE.
00170              20  GA-ACCT             PIC X(10).
00171              20  FILLER              PIC X.
00172              20  GA-ACCT-NAME        PIC X(30).
00173              20  GA-BEG-BAL          PIC ZZZZ,ZZZ.99-.
00174              20  GA-NET-PREM         PIC ZZZZ,ZZZ.ZZ-.
00175              20  GA-ACCT-COMP        PIC ZZZZ,ZZZ.ZZ-.
00176              20  GA-PMTS-ADJS        PIC ZZZZ,ZZZ.ZZ-.
00177              20  GA-UNPAID-NET-PREM  PIC ZZZZ,ZZZ.ZZ-.
00178              20  GA-BEN-OVERRIDE-L6  PIC X(6).
00179              20  FILLER              PIC X.
00180              20  GA-OVERWRITE        PIC ZZZZ,ZZZ.ZZ-.
00181              20  GA-AMT-DUE          PIC ZZZ,ZZZ.99-.
00182              20  FILLER              PIC X.
00183          16  GA-TEXT-LINE-3 REDEFINES GA-TEXT-LINE.
00184              20  FILLER              PIC X(11).
00185              20  GA-ENTRY-DESC       PIC X(30).
00186              20  FILLER              PIC X(60).
00187              20  GA-ENTRY-COMMENT    PIC X(30).
00188              20  FILLER              PIC X.
00189          16  FILLER.
00190              20  GA-BENEFIT-CD       PIC XX.
00191              20  GA-BEG-BAL-AMT      PIC S9(7)V99 COMP-3.
00192              20  GA-END-BAL-AMT      PIC S9(7)V99 COMP-3.
00193              20  GA-NET-PREM-AMT     PIC S9(7)V99 COMP-3.
00194              20  GA-ACCT-COMP-AMT    PIC S9(7)V99 COMP-3.
00195              20  GA-PMTS-ADJS-AMT    PIC S9(7)V99 COMP-3.
00196              20  GA-UNPAID-NET-AMT   PIC S9(7)V99 COMP-3.
00197              20  GA-OVERWRITE-AMT    PIC S9(7)V99 COMP-3.
00198              20  GA-AMT-DUE-AMT      PIC S9(7)V99 COMP-3.
00199
00200      12  GA-ADDRESS-RECORD  REDEFINES  GA-TEXT-RECORD.
00201          16  FILLER                  PIC X.
00202          16  GA-ACCT-ADDRESS-LINE    PIC X(30).
00203          16  FILLER                  PIC X(144).
00204
00205      12  GA-HEADER-RECORD  REDEFINES  GA-TEXT-RECORD.
00206          16  GA-PROCESSOR-CD         PIC X(4).
00207          16  GA-STATEMENT-TYPE       PIC X.
00208              88  GA-PREVIEW-ONLY         VALUE 'P'.
00209          16  GA-NO-OF-COPIES         PIC S9.
00210          16  GA-CREATION-DT          PIC XX.
00211          16  GA-INITIAL-PRINT-DATE   PIC XX.
00212          16  GA-AGENT-TOTALS.
00213              20  GA-NET-UNPD         PIC S9(9)V99     COMP-3.
00214              20  GA-COMP-UNPD-PREM   PIC S9(9)V99     COMP-3.
00215              20  GA-PREMIUM          PIC S9(9)V99     COMP-3.
00216              20  GA-REMITTED         PIC S9(9)V99     COMP-3.
00217              20  GA-TOT-ISS-COMP     PIC S9(9)V99     COMP-3.
00218              20  GA-TOT-CAN-COMP     PIC S9(9)V99     COMP-3.
00219              20  GA-ADJUSTMNTS       PIC S9(9)V99     COMP-3.
00220              20  GA-DISBURSED        PIC S9(9)V99     COMP-3.
00221              20  GA-END-BALANCE      PIC S9(9)V99     COMP-3.
00222          16  GA-AGENTS-NAME          PIC X(30).
00223          16  FILLER                  PIC X(81).
00224      12  FILLER                      PIC XX.
00225
00226
00675  EJECT
00676 *                            COPY ERCCOMM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMM                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = COMPENSATION RATES WHEN DIFFERENT    *
00008 *                             FROM THOSE IN ACCOUNT MASTER       *
00009 *                                                                *
00010 *        FILE TYPE= VSAM,KSDS                                    *
00011 *        RECORD SIZE = 250    RECFORM = FIXED                    *
00012 *                                                                *
00013 *        BASE CLUSTER = CRCOMM        RKP=2,LEN=33               *
00014 *                                                                *
00015 *        LOG = YES                                               *
00016 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00017 *                                                                *
00018 ******************************************************************
00019
00020  01  COMMISSION-EXCEPTIONS.
00021      12  CE-RECORD-ID                PIC  XX.
00022          88  VALID-CE-ID         VALUE 'CE'.
00023
00024      12  CE-CONTROL-PRIMARY.
00025          16  CE-COMPANY-CD           PIC  X.
00026          16  CE-CARRIER              PIC  X.
00027          16  CE-GROUPING             PIC  X(6).
00028          16  CE-STATE                PIC  XX.
00029          16  CE-ACCOUNT              PIC  X(10).
00030          16  CE-CERT-EFF-DT          PIC  XX.
00031          16  CE-CERT-NO.
00032              20  CE-CERT-PRIME       PIC  X(10).
00033              20  CE-CERT-SFX         PIC  X.
00034
00035      12  CE-CERT-EXPIRATION-DT       PIC  XX.
00036
00037      12  CE-COMP-STRUCTURE.
00038          16  CE-DEFN-1.
00039              20  CE-AGT-COMMS       OCCURS 10 TIMES.
00040                  24  CE-AGENT-NO    PIC  X(10).
00041                  24  CE-COMP-TYPE   PIC  X.
00042                  24  CE-LF-COMP     PIC SV9(5)      COMP-3.
00043                  24  CE-AH-COMP     PIC SV9(5)      COMP-3.
00044          16  CE-DEFN-2              REDEFINES CE-DEFN-1.
00045              20  CE-COMP-TABLES     OCCURS 10 TIMES.
00046                  24  FILLER         PIC  X(11).
00047                  24  CE-LF-COMPT    PIC  X(3).
00048                  24  CE-AH-COMPT    PIC  X(3).
00049
00050      12  FILLER                     PIC  X(43).
00051 ******************************************************************
00677  EJECT
00678 *                            COPY ERCCTBL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCTBL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION TABLE                        *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 200   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCTBL                   RKP=2,LEN=7     *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 *                                                                *
00021 ******************************************************************
00022
00023  01  COMM-TABLE-RECORD.
00024      12  CT-RECORD-ID                      PIC XX.
00025          88  VALID-CT-ID                      VALUE 'CT'.
00026
00027      12  CT-CONTROL-PRIMARY.
00028          16  CT-COMPANY-CD                 PIC X.
00029          16  CT-TABLE                      PIC XXX.
00030          16  CT-CNTRL-2.
00031              20  CT-BEN-TYPE               PIC X.
00032              20  CT-BEN-CODE               PIC XX.
00033
00034      12  CT-MAINT-INFORMATION.
00035          16  CT-LAST-MAINT-DT              PIC XX.
00036          16  CT-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00037          16  CT-LAST-MAINT-USER            PIC X(4).
00038          16  FILLER                        PIC X(31).
00039
00040      12  CT-LIMITS.
00041          16  CT-TBF OCCURS 3 TIMES         PIC S9(7)V99   COMP-3.
00042
00043          16  CT-AGE OCCURS 3 TIMES         PIC S99        COMP-3.
00044
00045          16  CT-TRM OCCURS 3 TIMES         PIC S999       COMP-3.
00046
00047      12  CT-RATES.
00048          16  CT-RTX          OCCURS 27 TIMES.
00049              20  CT-RT                     PIC SV9(5)     COMP-3.
00050              20  CT-RT-R   REDEFINES
00051                  CT-RT                     PIC XXX.
00052
00053      12  FILLER                            PIC  X(42).
00054
00055 ******************************************************************
00679  EJECT
00680 *                            COPY ERCGXRF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCGXRF                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = GENERAL AGENT CROSS REFERENCE             *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 62 - 32,062   RECFORM = VARIABLE
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERGXRF                   RKP=2,LEN=18    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
00021
00022  01  AGENT-CROSS-REFERENCE.
00023      12  GX-RECORD-ID                PIC XX.
00024          88  VALID-GX-ID             VALUE 'GX'.
00025
00026      12  GX-CONTROL-PRIMARY.
00027          16  GX-COMPANY-CD           PIC X.
00028          16  GX-CARRIER              PIC X.
00029          16  GX-GROUPING             PIC X(6).
00030          16  GX-AGENT-NO             PIC X(10).
00031
00032      12  GX-MAINT-INFORMATION.
00033          16  GX-LAST-MAINT-DT        PIC XX.
00034          16  GX-LAST-MAINT-HHMMSS    PIC S9(7)  COMP-3.
00035          16  GX-LAST-MAINT-USER      PIC X(4).
00036          16  FILLER                  PIC X(9).
00037
00038      12  FILLER                      PIC X(37).
00039
00040      12  GX-AGENT-POINTER-CNT        PIC S9(4)  COMP.
00041
00042      12  GX-AGENT-POINTER   OCCURS 1 TO 1006 TIMES
00043                             DEPENDING ON GX-AGENT-POINTER-CNT.
00044          16  GX-AM-CARRIER           PIC X.
00045          16  GX-AM-GROUPING          PIC X(6).
00046          16  GX-AM-STATE             PIC XX.
00047          16  GX-AM-ACCOUNT           PIC X(10).
00048          16  GX-AM-EXPIRATION-DT     PIC XX.
00049          16  GX-AM-LEVEL-NO          PIC S9(4)     COMP.
00050          16  GX-LAST-BILL-DT         PIC XX.
00051          16  GX-AM-EFF-DT            PIC XX.
00052          16  FILLER                  PIC X(4).
00053
00054 ******************************************************************
00681  EJECT
00682 *                            COPY ERCRESC.
00001 ******************************************************************
CIDMOD*                                                                *
00003 *                            ERCRESC                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACCOUNT RESIDENT STATE COMMISSION         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 103 RECFORM = FIXED                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERRESC                    RKP=00,LEN=37  *
00013 *       ALTERNATE PATH1 = NONE                                   *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 *                                                                *
00018 ******************************************************************
00019
00020  01  ACCOUNT-RESIDENT-ST-COMMISSION.
00021
00022      10  ERRESC-RECORD-KEY.
00023          15  RESC-COMPANY-CD           PIC   X.
00024          15  RESC-CARRIER              PIC   X.
00025          15  RESC-GROUP                PIC   X(6).
00026          15  RESC-STATE                PIC   XX.
00027          15  RESC-ACCOUNT              PIC   X(10).
00028          15  RESC-AGENT                PIC   X(10).
00029          15  RESC-RESIDENT-STATE       PIC   XX.
00030          15  RESC-EXPIRE-DATE          PIC  9(8) COMP-3.
00031 *                                           YYYYMMDD
00032      10  RESC-EFFECTIVE-DATE           PIC  9(8) COMP-3.
00033
00034      10  RESC-COMM-RECORD-DATA.
00035          15  RESC-COMMISSIONS OCCURS 12 TIMES.
00036              20 RESC-COVERAGE-CAT      PIC    X.
00037              20 RESC-COMMISSION-PER    PIC SV9(5) COMP-3.
00038              20 RESC-COMMISSION-TAB REDEFINES
00039                 RESC-COMMISSION-PER    PIC  XXX.
00040
00041      10  RESC-MAINT-BY                 PIC X(4).
00042      10  RESC-LST-MAINT-TIME           PIC 9(7)   COMP-3.
00043      10  RESC-LST-MAINT-DATE           PIC 9(8)   COMP-3.
00044 *                                          YYYYMMDD
00683
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PENDING-BUSINESS
                                CONTROL-FILE COMPENSATION-MASTER
                                PENDING-PAY-ADJ ACCOUNT-MASTER
                                BILLING-STATEMENT
                                COMMISSION-EXCEPTIONS
                                COMM-TABLE-RECORD
                                AGENT-CROSS-REFERENCE
                                ACCOUNT-RESIDENT-ST-COMMISSION.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL642' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00685
00686      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00687      MOVE 2                      TO  EMI-NUMBER-OF-LINES.
00688      MOVE EIBTRMID               TO  QID-TERM.
00689      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00690      MOVE '5'                    TO  DC-OPTION-CODE.
00691
00692      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
00693
00694      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE.
00695      MOVE DC-GREG-DATE-1-MDY     TO  WS-CURRENT-DATE-MDY.
00696      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DATE-EDIT.
00697
00698      IF EIBCALEN  =  ZERO
00699          GO TO 8800-UNAUTHORIZED-ACCESS.
00700
00701      IF PI-RETURN-TO-PROGRAM  =  THIS-PGM
00702          MOVE PI-CALLING-PROGRAM  TO  RETURNED-FROM.
00703
00704      IF PI-CALLING-PROGRAM NOT  =  THIS-PGM
00705          IF PI-RETURN-TO-PROGRAM NOT  =  THIS-PGM
00706              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00707              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00708              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00709              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00710              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00711              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00712              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00713              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00714              MOVE PI-CR-CONTROL-IN-PROGRESS
00715                                         TO  W-TRANSFER-CONTROL
00716              MOVE SPACES
00717                                    TO  PI-CR-CONTROL-IN-PROGRESS
00718          ELSE
00719              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00720              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00721              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00722              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00723              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00724              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00725              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00726              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00727
00728      MOVE LOW-VALUES             TO  EL642AI.
00729
00730      IF PI-AR-PROCESSING
00731         MOVE XCTL-635            TO XCTL-PYAJ.
00732
00733      IF PI-COMPANY-ID = 'DMD'
00734         MOVE XCTL-633DMD         TO XCTL-PYAJ.
00735
00736      IF RETURNED-FROM  NOT =  SPACES
00737          PERFORM 0600-RECOVER-TEMP-STORAGE  THRU  0690-EXIT
00738          IF RETURNED-FROM  =  XCTL-PYAJ
00739            AND  NOT  PI-VOID-BILL
00740              MOVE ZEROS          TO  PI-ADJUSTMNTS
00741                                      PI-DISBURSED
00742                                      PI-REMITTED
00743              GO TO 1200-PYAJ-BILLING-COMPLETE
00744          ELSE
00745              MOVE -1             TO  APFNTERL
00746              IF PI-TRANSFER-BEFORE-ACT
00747                  GO TO 8100-SEND-INITIAL-MAP
00748              ELSE
00749                  PERFORM 5000-FORMAT-SCREEN  THRU  5090-EXIT
00750                  GO TO 8100-SEND-INITIAL-MAP.
00751
00752      IF EIBTRNID  NOT =  TRANS-ID
00753          MOVE EL642A             TO  PI-MAP-NAME
00754          MOVE -1                 TO  APFNTERL
00755          GO TO 8100-SEND-INITIAL-MAP.
00756
00757      
      * EXEC CICS HANDLE CONDITION
00758 *        PGMIDERR  (9600-PGMID-ERROR)
00759 *        ERROR     (9990-ABEND)
00760 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00005519' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035353139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00761
00762      IF EIBAID  =  DFHCLEAR
00763          IF PI-MAP-NAME  =  EL642B
00764              PERFORM 5000-FORMAT-SCREEN  THRU  5090-EXIT
00765              MOVE EL642A         TO  PI-MAP-NAME
00766              MOVE -1             TO  APFNTERL
00767              GO TO 8100-SEND-INITIAL-MAP
00768          ELSE
00769              GO TO 9400-CLEAR.
00770
00771      IF PI-PROCESSOR-ID  =  'LGXX'
00772          GO TO 0200-RECEIVE.
00773
00774      
      * EXEC CICS READQ TS
00775 *        QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00776 *        INTO    (SECURITY-CONTROL)
00777 *        LENGTH  (SC-COMM-LENGTH)
00778 *        ITEM    (SC-ITEM)
00779 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00005536' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00780
00781      MOVE SC-CREDIT-DISPLAY (19)  TO  PI-DISPLAY-CAP.
00782      MOVE SC-CREDIT-UPDATE  (19)  TO  PI-MODIFY-CAP.
00783
00784      IF NOT  DISPLAY-CAP
00785          MOVE 'READ'              TO  SM-READ
00786          PERFORM 9995-SECURITY-VIOLATION
00787          MOVE ER-0070             TO  EMI-ERROR
00788          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00789          GO TO 8100-SEND-INITIAL-MAP.
00790
00791  0200-RECEIVE.
00792      IF EIBAID  =  DFHPA1  OR  DFHPA2  OR  DFHPA3
00793          MOVE ER-0008            TO  EMI-ERROR
00794          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00795          IF PI-MAP-NAME  =  EL642A
00796              MOVE -1             TO  APFNTERL
00797              GO TO 8200-SEND-DATAONLY
00798          ELSE
00799              MOVE -1             TO  BPFNTERL
00800              GO TO 8200-SEND-DATAONLY.
00801
00802      
      * EXEC CICS RECEIVE
00803 *        MAP     (PI-MAP-NAME)
00804 *        MAPSET  (MAPSET-NAME)
00805 *        INTO    (EL642AI)
00806 *    END-EXEC.
           MOVE LENGTH OF
            EL642AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005564' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL642AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00807
00808      IF PI-MAP-NAME  =  EL642A
00809          IF APFNTERL  GREATER THAN  ZERO
00810              IF EIBAID  NOT =  DFHENTER
00811                  MOVE ER-0004    TO  EMI-ERROR
00812                  MOVE AL-UNBOF   TO  APFNTERA
00813                  MOVE -1         TO  APFNTERL
00814                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00815                  GO TO 8200-SEND-DATAONLY
00816              ELSE
00817                  IF APFNTERI  NUMERIC
00818                    AND  APFNTERI  GREATER THAN  ZERO
00819                    AND  APFNTERI  LESS THAN  25
00820                      MOVE PF-VALUES (APFNTERI)  TO  EIBAID
00821                  ELSE
00822                      MOVE ER-0029   TO  EMI-ERROR
00823                      MOVE AL-UNBOF  TO  APFNTERA
00824                      MOVE -1        TO  APFNTERL
00825                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00826                      GO TO 8200-SEND-DATAONLY
00827          ELSE
00828              NEXT SENTENCE
00829      ELSE
00830          IF PI-MAP-NAME  =  EL642B
00831              IF BPFNTERL  GREATER THAN  ZERO
00832                  IF EIBAID  NOT =  DFHENTER
00833                      MOVE ER-0004   TO  EMI-ERROR
00834                      MOVE AL-UNBOF  TO  BPFNTERA
00835                      MOVE -1        TO  BPFNTERL
00836                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00837                      GO TO 8200-SEND-DATAONLY
00838                  ELSE
00839                      IF BPFNTERI  NUMERIC
00840                        AND  BPFNTERI  GREATER THAN  ZERO
00841                        AND  BPFNTERI  LESS THAN  25
00842                          MOVE PF-VALUES (BPFNTERI)  TO  EIBAID
00843                      ELSE
00844                          MOVE ER-0029   TO  EMI-ERROR
00845                          MOVE AL-UNBOF  TO  BPFNTERA
00846                          MOVE -1        TO  BPFNTERL
00847                          PERFORM 9900-ERROR-FORMAT
00848                              THRU  9900-EXIT
00849                          GO TO 8200-SEND-DATAONLY.
00850  EJECT
00851  0300-CHECK-PFKEYS.
00852      IF EIBAID  =  DFHPF23
00853          GO TO 8810-PF23.
00854
00855      IF EIBAID  =  DFHPF24
00856          GO TO 9200-RETURN-MAIN-MENU.
00857
00858      IF EIBAID  =  DFHPF12
00859          GO TO 9500-PF12.
00860
00861  0310-CHECK-PFKEYS.
00862      IF EIBAID  =  DFHPF3
00863        AND  PI-MAP-NAME  =  EL642A
00864          PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT
00865          MOVE SPACES             TO  PI-CR-CONTROL-IN-PROGRESS
00866          MOVE XCTL-650           TO  PGM-NAME
00867          GO TO 9300-XCTL.
00868
00869      IF EIBAID  =  DFHPF4
00870        AND  PI-MAP-NAME  =  EL642A
00871         NEXT SENTENCE
00872      ELSE
00873         GO TO 0320-CONT-PFKEYS.
00874
00875      IF PI-CR-FIN-RESP  NOT =  SPACES
00876          MOVE PI-COMP-CARRIER   TO  PI-CR-CARRIER
00877          MOVE PI-COMP-GROUPING  TO  PI-CR-GROUPING
00878          MOVE SPACE             TO  PI-CR-STATE
00879          MOVE LOW-VALUES        TO  PI-CR-ACCOUNT
00880          MOVE PI-SAV-AGENT      TO  PI-CR-FIN-RESP
00881          MOVE 'G'               TO  PI-CR-TYPE
00882          PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT
00883          MOVE XCTL-652       TO  PGM-NAME
00884          GO TO 9300-XCTL.
00885
00886      IF PI-SCR-FIN-RESP EQUAL SPACES OR LOW-VALUES
00887          GO TO 0315-PF4-ERROR.
00888
00889      PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT.
00890
00891      MOVE PI-SCR-CARRIER        TO  PI-CR-CARRIER.
00892      MOVE PI-SCR-GROUPING       TO  PI-CR-GROUPING.
00893      MOVE SPACE                 TO  PI-CR-STATE.
00894      MOVE LOW-VALUES            TO  PI-CR-ACCOUNT.
00895      MOVE PI-SCR-FIN-RESP       TO  PI-CR-FIN-RESP.
00896      MOVE 'G'                   TO  PI-CR-TYPE.
00897      MOVE 'Y'                   TO  PI-TRANSFER-SW.
00898
00899      IF PI-ZERO-CARRIER
00900        OR  PI-ZERO-CAR-GROUP
00901          MOVE ZEROS              TO  PI-CR-CARRIER.
00902
00903      IF PI-ZERO-GROUPING
00904         OR  PI-ZERO-CAR-GROUP
00905          MOVE ZEROS              TO  PI-CR-GROUPING.
00906
00907      MOVE XCTL-652       TO  PGM-NAME
00908
00909      GO TO 9300-XCTL.
00910
00911  0315-PF4-ERROR.
00912
00913      MOVE ER-2407        TO  EMI-ERROR.
00914      MOVE -1             TO  APFNTERL.
00915      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00916      GO TO 8200-SEND-DATAONLY.
00917
00918  0320-CONT-PFKEYS.
00919
00920      IF EIBAID  =  DFHPF6
00921        AND  PI-MAP-NAME  =  EL642A
00922          MOVE LOW-VALUES         TO  EL642AO
00923          MOVE PI-SAV-CARR        TO  PI-CR-CARRIER
00924          MOVE PI-SAV-GROUP       TO  PI-CR-GROUPING
00925          MOVE SPACE              TO  PI-CR-STATE
00926          MOVE LOW-VALUES         TO  PI-CR-ACCOUNT
00927          MOVE PI-SAV-AGENT       TO  PI-CR-FIN-RESP
00928          MOVE 'G'                TO  PI-CR-TYPE
00929          PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT
00930          MOVE XCTL-6401          TO  PGM-NAME
00931          GO TO 9300-XCTL.
00932
00933      IF EIBAID  =  DFHPF7
00934        AND  PI-MAP-NAME  =  EL642A
00935          IF PI-CR-FIN-RESP  NOT =  SPACES
00936              MOVE PI-COMP-CARRIER   TO  PI-CR-CARRIER
00937              MOVE PI-COMP-GROUPING  TO  PI-CR-GROUPING
00938              MOVE SPACE             TO  PI-CR-STATE
00939              MOVE LOW-VALUES        TO  PI-CR-ACCOUNT
00940              MOVE PI-SAV-AGENT      TO  PI-CR-FIN-RESP
00941              MOVE 'G'               TO  PI-CR-TYPE
00942              PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT
00943              MOVE XCTL-PYAJ      TO  PGM-NAME
00944              GO TO 9300-XCTL
00945          ELSE
00946              IF PI-RETURN-TO-PROGRAM = XCTL-633 OR XCTL-635 OR
00947                                        XCTL-633DMD
00948                  GO TO 9400-CLEAR
00949             ELSE
00950                  MOVE ER-2411        TO  EMI-ERROR
00951                  MOVE -1             TO  APFNTERL
00952                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00953                  GO TO 8200-SEND-DATAONLY.
00954
00955      IF EIBAID  =  DFHPF8
00956        AND  PI-MAP-NAME  =  EL642A
00957          IF PI-CR-FIN-RESP  NOT =  SPACES
00958              MOVE PI-SAV-CARR    TO  PI-CR-CARRIER
00959              MOVE PI-SAV-GROUP   TO  PI-CR-GROUPING
00960              MOVE SPACE          TO  PI-CR-STATE
00961              MOVE LOW-VALUES     TO  PI-CR-ACCOUNT
00962              MOVE PI-SAV-AGENT   TO  PI-CR-FIN-RESP
00963              MOVE 'G'            TO  PI-CR-TYPE
00964              PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT
00965              MOVE XCTL-658       TO  PGM-NAME
00966              GO TO 9300-XCTL
00967          ELSE
00968          IF PI-SCR-FIN-RESP  NOT =  SPACES
00969              PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT
00970              MOVE PI-SCR-CARRIER  TO  PI-CR-CARRIER
00971              MOVE PI-SCR-GROUPING TO  PI-CR-GROUPING
00972              MOVE SPACE           TO  PI-CR-STATE
00973              MOVE LOW-VALUES      TO  PI-CR-ACCOUNT
00974              MOVE PI-SCR-FIN-RESP TO  PI-CR-FIN-RESP
00975              MOVE 'G'             TO  PI-CR-TYPE
00976              MOVE XCTL-658       TO  PGM-NAME
00977              GO TO 9300-XCTL
00978          ELSE
00979              MOVE ER-2399        TO  EMI-ERROR
00980              MOVE -1             TO  APFNTERL
00981              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00982              GO TO 8200-SEND-DATAONLY.
00983
00984      IF EIBAID  =  DFHPF1
00985        AND  PI-MAP-NAME  =  EL642B
00986          GO TO 7500-PRODUCE-CHECK.
00987
00988      IF EIBAID  =  DFHPF5
00989        AND  PI-MAP-NAME  =  EL642A
00990          IF PI-CR-FIN-RESP NOT  =  SPACES
00991              MOVE PI-SAV-CARR    TO  PI-CR-CARRIER
00992              MOVE PI-SAV-GROUP   TO  PI-CR-GROUPING
00993              MOVE SPACE          TO  PI-CR-STATE
00994              MOVE LOW-VALUES     TO  PI-CR-ACCOUNT
00995              MOVE PI-SAV-AGENT   TO  PI-CR-FIN-RESP
00996              MOVE 'G'            TO  PI-CR-TYPE
00997              MOVE EL642B         TO  PI-MAP-NAME
00998              GO TO 7000-PROCESS-CHECK
00999          ELSE
01000              MOVE ER-2400        TO  EMI-ERROR
01001              MOVE -1             TO  APFNTERL
01002              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01003              GO TO 8200-SEND-DATAONLY.
01004
01005      IF PI-MAP-NAME  =  EL642A
01006          IF EIBAID  =  DFHENTER
01007            OR  DFHPF3
01008              GO TO 0330-EDIT-DATA.
01009
01010  0320-INPUT-ERROR.
01011      MOVE ER-0029                TO  EMI-ERROR.
01012
01013      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01014
01015      IF PI-MAP-NAME  =  EL642A
01016          MOVE AL-UNBON           TO  APFNTERA
01017          MOVE -1                 TO  APFNTERL
01018      ELSE
01019          MOVE AL-UNBON           TO  BPFNTERA
01020          MOVE -1                 TO  BPFNTERL.
01021
01022      GO TO 8200-SEND-DATAONLY.
01023  EJECT
01024  0330-EDIT-DATA.
01025      MOVE ABILTYPI               TO  VALID-BILL-TYPE-VALUES.
01026
01027      IF VALID-BILL-TYPE
01028          IF NOT MODIFY-CAP
01029              IF ABILTYPI  =  '1' OR '2'
01030                  NEXT SENTENCE
01031              ELSE
01032                  MOVE 'UPDATE'   TO  SM-READ
01033                  PERFORM 9995-SECURITY-VIOLATION
01034                  MOVE ER-0070    TO  EMI-ERROR
01035                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01036                  GO TO 8100-SEND-INITIAL-MAP.
01037
01038      MOVE SPACES                 TO  PI-LIMIT-BILLING-ACCOUNTS.
01039
01040  0350-ERROR-CHECK.
01041      IF EMI-ERROR  =  ZEROS
01042          NEXT SENTENCE
01043      ELSE
01044          GO TO 8200-SEND-DATAONLY.
01045
01046      IF AAGENTL  GREATER THAN  ZEROS
01047          MOVE AL-UANON           TO  AAGENTA
01048          MOVE AAGENTI            TO  PI-SAV-AGENT
01049      ELSE
01050          MOVE -1                 TO  AAGENTL
01051          MOVE AL-UABON           TO  AAGENTA
01052          MOVE ER-2910            TO  EMI-ERROR
01053          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01054
01055      IF ACARIERL  GREATER THAN  ZEROS
01056          MOVE AL-UANON           TO  ACARIERA
01057          MOVE ACARIERI           TO  PI-SAV-CARR
01058                                      PI-CR-CARRIER
01059                                      PI-COMP-CARRIER
01060      ELSE
01061          IF NOT  PI-ZERO-CARRIER
01062            AND  NOT  PI-ZERO-CAR-GROUP
01063              MOVE -1             TO  ACARIERL
01064              MOVE AL-UABON       TO  ACARIERA
01065              MOVE ER-0194        TO  EMI-ERROR
01066              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01067          ELSE
01068              MOVE ZERO           TO  PI-SAV-CARR
01069                                      PI-COMP-CARRIER.
01070
01071      IF AGROUPL  GREATER THAN  ZEROS
01072          MOVE AL-UANON           TO  AGROUPA
01073          MOVE AGROUPI            TO  PI-SAV-GROUP
01074                                      PI-CR-GROUPING
01075                                      PI-COMP-GROUPING
01076      ELSE
01077          IF NOT  PI-ZERO-GROUPING
01078            AND  NOT  PI-ZERO-CAR-GROUP
01079              MOVE -1             TO  AGROUPL
01080              MOVE AL-UABON       TO  AGROUPA
01081              MOVE ER-0195        TO  EMI-ERROR
01082              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01083          ELSE
01084              MOVE ZERO           TO  PI-SAV-GROUP
01085                                      PI-COMP-GROUPING.
01086
01087      MOVE ABILTYPI               TO  VALID-BILL-TYPE-VALUES.
01088
01089      IF VALID-BILL-TYPE
01090          MOVE AL-UANON           TO  ABILTYPA
01091          MOVE ABILTYPI           TO  PI-BILL-TYPE
01092                                      WS-BILL-TYPE
01093      ELSE
01094          MOVE -1                 TO  ABILTYPL
01095          MOVE ER-2249            TO  EMI-ERROR
01096          MOVE AL-UABON           TO  ABILTYPA
01097          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01098
01099      IF PI-AR-PROCESSING
01100         IF PI-PREVIEW
01101            NEXT SENTENCE
01102         ELSE
01103            MOVE -1               TO ABILTYPL
01104            MOVE ER-3145          TO EMI-ERROR
01105            MOVE AL-UABON         TO ABILTYPA
01106            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01107
01108      IF APRODSWL  NOT =  ZEROS
01109          IF PI-PREVIEW
01110              IF APRODSWI  =  'Y'  OR  'N'
01111                  MOVE AL-UANON   TO  APRODSWA
01112              ELSE
01113                  MOVE -1         TO  APRODSWL
01114                  MOVE ER-2436    TO  EMI-ERROR
01115                  MOVE AL-UABON   TO  APRODSWA
01116                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01117          ELSE
01118              MOVE -1             TO  APRODSWL
01119              MOVE ER-2434        TO  EMI-ERROR
01120              MOVE AL-UABON       TO  APRODSWA
01121              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01122      ELSE
01123          IF PI-PREVIEW
01124              MOVE -1             TO  APRODSWL
01125              MOVE ER-2435        TO  EMI-ERROR
01126              MOVE AL-UABON       TO  APRODSWA
01127              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01128
01129      IF AACCT1L  NOT =  ZEROS
01130          MOVE AL-UANON           TO  AACCT1A
01131          MOVE 'Y'                TO  LIMIT-BILLING-SW
01132          MOVE AACCT1I            TO  PI-BILLING-ACCOUNTS (1).
01133
01134      IF AACCT2L  NOT =  ZEROS
01135          MOVE AL-UANON           TO  AACCT2A
01136          MOVE 'Y'                TO  LIMIT-BILLING-SW
01137          MOVE AACCT2I            TO  PI-BILLING-ACCOUNTS (2).
01138
01139      IF AACCT3L  NOT =  ZEROS
01140          MOVE AL-UANON           TO  AACCT3A
01141          MOVE 'Y'                TO  LIMIT-BILLING-SW
01142          MOVE AACCT3I            TO  PI-BILLING-ACCOUNTS (3).
01143
01144      IF EMI-ERROR  =  ZEROS
01145          GO TO 1000-BILLING-PROCESS.
01146
01147      GO TO 8200-SEND-DATAONLY.
01148  EJECT
01149  0500-CREATE-TEMP-STORAGE.
01150      IF PI-BAL-FRWD  NOT  NUMERIC
01151          MOVE ZEROS              TO  PI-BAL-FRWD
01152                                      PI-PREMIUM
01153                                      PI-REMITTED
01154                                      PI-TOT-ISS-COMP
01155                                      PI-TOT-CAN-COMP
01156                                      PI-ADJUSTMNTS
01157                                      PI-DISBURSED
01158                                      PI-END-BAL
01159                                      PI-UNPAID-NET-PREM
01160                                      PI-COMP-UNPAID-PREM
01161                                      PI-LF-ISS-COMP
01162                                      PI-AH-ISS-COMP
01163                                      PI-LF-CAN-COMP
01164                                      PI-AH-CAN-COMP
01165                                      PI-DUE-FOR-ACCT
01166                                      PI-ACCT-BEG-BAL
01167                                      PI-ACCT-NET-PREM
01168                                      PI-ACCT-COMP
01169                                      PI-ACCT-PAY-ADJS.
01170
01171      
      * EXEC CICS WRITEQ TS
01172 *        QUEUE   (QID)
01173 *        FROM    (PROGRAM-INTERFACE-BLOCK)
01174 *        LENGTH  (PI-COMM-LENGTH)
01175 *    END-EXEC.
      *    MOVE '*"                    ''   #00005933' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01176
01177  0590-EXIT.
01178       EXIT.
01179
01180  0600-RECOVER-TEMP-STORAGE.
01181      
      * EXEC CICS READQ TS
01182 *        QUEUE   (QID)
01183 *        INTO    (PROGRAM-INTERFACE-BLOCK)
01184 *        LENGTH  (PI-COMM-LENGTH)
01185 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00005943' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01186
01187      PERFORM 0800-DELETE-TS  THRU  0890-EXIT.
01188
01189      MOVE PI-SCRN-CONTROL TO W-TRANSFER-CONTROL.
01190
01191  0690-EXIT.
01192       EXIT.
01193
01194  0800-DELETE-TS.
01195      
      * EXEC CICS HANDLE CONDITION
01196 *        QIDERR  (0890-EXIT)
01197 *    END-EXEC.
      *    MOVE '"$N                   ! # #00005957' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035393537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01198
01199      
      * EXEC CICS DELETEQ TS
01200 *        QUEUE  (QID)
01201 *    END-EXEC.
      *    MOVE '*&                    #   #00005961' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01202
01203  0890-EXIT.
01204       EXIT.
01205  EJECT
01206 ******************************************************************
01207 *    THIS SECTION PROCESSES THE PENDING BUSINESS FILE BASED ON   *
01208 *    ACCOUNT DATE RANGES FOR EACH ACCOUNT THAT A GENERAL AGENT   *
01209 *    RECEIVES COMMISION FROM.  THE PROFILE OF A GENERAL AGENT    *
01210 *    IS MAINTAINED IN THE AGENT-CROSS-REFERENCE FILE.            *
01211 *                                                                *
01212 *    1.  IF VOID BILLING                                         *
01213 *            GO TO VOID-BILLING.                                 *
01214 *                                                                *
01215 *    2.  READ COMPENSATIONG MASTER FOR GENERAL AGENT TO GET      *
01216 *        THE AGENT'S ENDING BALANCE AND ADDRESS.                 *
01217 *                                                                *
01218 *    3.  READ THE AGENT-CROSS-REFERENCE-FILE.                    *
01219 *                                                                *
01220 *    4.  PROCESS EACH ACCOUNT ASSOCIATED WITH A GENERAL AGENT.   *
01221 *                                                                *
01222 *        A.  PROCESSS EACH DATE RANGE PER ACCOUNT AND ACCUMULATE *
01223 *            STATISTICS FOR THAT ACCOUNT.                        *
01224 *                                                                *
01225 *            1. PROCESS THE ACTIVE RECORDS ON PENDING BUSINESS.  *
01226 *               OMIT ANY RECORDS THAT HAVE ANY FATAL OR UNFORCED *
01227 *               ERRORS.  DO NOT PROCESS ANY REINSURANCE OR BILL- *
01228 *               ING ADJUSTMENT RECORDS.  IF BILLING UPDATE, FLAG
01229 *               EACH PENDING BUSINESS RECORD THAT IS PRCESSED.   *
01230 *                                                                *
01231 *            2.  ACCUMULATE STATISTICS FOR SCREEN DISPLAY.       *
01232 *                                                                *
01233 *            3.  COMPUTE ACCOUNT AND AGENT COMMISSION.           *
01234 *                                                                *
01235 *         B.  ACCUMULATE THE PAYMENT AND ADJUSTMENTS FOR         *
01236 *             EACH ACCOUNT THE GENERAL AGENT IS RESPONSIBLE FOR. *
01237 *                                                                *
01238 *         C.  PRINT ACCUMULATED STATISTICS FOR EACH ACCOUNT.     *
01239 *             OMITT ANY ACCOUNTS THAT DIDN'T HAVE ANY NEW BUS.   *
01240 *                                                                *
01241 *    5.  PRINT AN AGENT TOTAL OF ALL THE ACCOUNTS AND THE        *
01242 *        GENERAL AGENTS PAYMENTS AND ADJUSTMENTS.                *
01243 ******************************************************************
01244  EJECT
01245  1000-BILLING-PROCESS.
01246      MOVE PI-COMPANY-CD          TO  ERPNDB-CO-CD
01247                                      ERPNDB-CO-CD-A1
01248                                      ERCOMP-COMP-CD
01249                                      ERPYAJ-COMP-CD
01250                                      ERACCT-P-CO-CD
01251                                      ERACCT-A-CO-CD
01252                                      ERCOMM-COMPANY-CD
01253                                      ERCTBL-COMPANY-CD.
01254
01255      MOVE ZEROS                  TO  PI-BAL-FRWD
01256                                      PI-UNKNOWN
01257                                      PI-UNPAID-NET-PREM
01258                                      PI-COMP-UNPAID-PREM
01259                                      PI-PREMIUM
01260                                      PI-REMITTED
01261                                      PI-TOT-ISS-COMP
01262                                      PI-TOT-CAN-COMP
01263                                      PI-ADJUSTMNTS
01264                                      PI-DISBURSED
01265                                      PI-END-BAL
01266                                      PI-LF-ISS-COMP
01267                                      PI-AH-ISS-COMP
01268                                      PI-LF-CAN-COMP
01269                                      PI-AH-CAN-COMP
01270                                      PI-DUE-FOR-ACCT
01271                                      PI-ACCT-BEG-BAL
01272                                      PI-ACCT-NET-PREM
01273                                      PI-ACCT-COMP
01274                                      PI-ACCT-PAY-ADJS
01275                                      PI-ISSUES-BILLED
01276                                      PI-ISSUES-INER
01277                                      PI-ISSUES-PREV
01278                                      PI-CANCELS-BILLED
01279                                      PI-CANCELS-INER
01280                                      PI-CANCELS-PREV.
01281
01282      IF PI-VOID-BILL
01283          GO TO 2000-VOID-BILLING.
01284
01285      MOVE SPACES                 TO  ELCNTL-KEY.
01286      MOVE '1'                    TO  ELCNTL-REC-TYPE.
01287
01288      PERFORM 6100-READ-CONTROL-FILE  THRU  6190-EXIT.
01289
01290      IF EMI-ERROR  NOT =  ZEROS
01291          GO TO 8200-SEND-DATAONLY.
01292
01293      IF CF-ACCOUNT-MSTR-MAINT-DT  =  LOW-VALUES
01294        OR  CF-COMPENSATION-MSTR-MAINT-DT  =  LOW-VALUES
01295          MOVE ER-2571            TO  EMI-ERROR
01296          MOVE -1                 TO  APFNTERL
01297          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01298          GO TO 8200-SEND-DATAONLY.
01299
01300      IF CF-FORMS-PRINTER-ID  =  SPACES
01301        AND  (ABILTYPI = '3'  OR  '4')
01302          MOVE ER-2590            TO  EMI-ERROR
01303          MOVE -1                 TO  ABILTYPL
01304          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01305          GO TO 8200-SEND-DATAONLY.
01306
01307      MOVE CF-CURRENT-MONTH-END   TO  DC-BIN-DATE-1.
01308      MOVE SPACE                  TO  DC-OPTION-CODE.
01309
01310      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
01311
01312      MOVE DC-GREG-DATE-1-EDIT    TO  PI-MONTH-END-DATE.
01313      MOVE WS-CURRENT-DATE-EDIT   TO  HD-RUN-DT.
01314      MOVE CF-CL-MAIL-TO-NAME     TO  CENTER-WORK-1.
01315      MOVE +44                    TO  X-LEN.
01316
01317      PERFORM 8600-CENTER-DATA  THRU  8690-C-D-X.
01318
01319      MOVE CENTER-WORK-2          TO  HD-CO.
01320      MOVE SPACE                  TO  PI-DATA-BILLED-SW.
01321      MOVE 'G'                    TO  COMPENSATION-SW
01322                                      PI-CR-TYPE.
01323
01324      PERFORM 6000-READ-COMP-MASTER  THRU  6090-EXIT.
01325
01326      PERFORM 6400-READ-CROSS-REFERENCE  THRU  6490-EXIT.
01327
01328      MOVE +0                     TO  GX-SUB.
01329
01330  1005-GXRF-BILLING-PROCESS-LOOP.
01331      ADD +1                      TO  GX-SUB.
01332
01333      IF GX-SUB  GREATER THAN  GX-AGENT-POINTER-CNT
CIDMOD         IF PI-ACCT-BILLED
CIDMOD             PERFORM 1100-PYAJ-BILLING-PROCESS  THRU  1190-EXIT
CIDMOD             GO TO 1200-PYAJ-BILLING-COMPLETE
CIDMOD         ELSE
CIDMOD             GO TO 1200-PYAJ-BILLING-COMPLETE.
01335
01336      IF LIMIT-BILLING
01337          IF GX-AM-ACCOUNT (GX-SUB)  =  PI-BILLING-ACCOUNTS (1)
01338                                    OR  PI-BILLING-ACCOUNTS (2)
01339                                    OR  PI-BILLING-ACCOUNTS (3)
01340              NEXT SENTENCE
01341          ELSE
01342              GO TO 1005-GXRF-BILLING-PROCESS-LOOP.
01343
01344      IF FIRST-TIME
01345          MOVE 'N'                      TO  FIRST-TIME-SW
01346          MOVE GX-AM-CARRIER (GX-SUB)   TO  PI-CR-CARRIER
01347          MOVE GX-AM-GROUPING (GX-SUB)  TO  PI-CR-GROUPING
01348          MOVE GX-AM-STATE (GX-SUB)     TO  PI-CR-STATE
01349          MOVE GX-AM-ACCOUNT (GX-SUB)   TO  PI-CR-ACCOUNT.
01350
01351      IF GX-AM-CARRIER (GX-SUB)  =  PI-CR-CARRIER
01352        AND  GX-AM-GROUPING (GX-SUB)  =  PI-CR-GROUPING
01353        AND  GX-AM-STATE (GX-SUB)  =  PI-CR-STATE
01354        AND  GX-AM-ACCOUNT (GX-SUB)  =  PI-CR-ACCOUNT
01355          NEXT SENTENCE
01356      ELSE
01357          IF PI-ACCT-BILLED
CIDMOD             PERFORM 1100-PYAJ-BILLING-PROCESS  THRU  1190-EXIT
01358              MOVE 'TA'           TO  BILLING-DETAIL-TYPE
01359              IF APRODSWI  =  'N'
01360                  NEXT SENTENCE
01361              ELSE
01362                  PERFORM 3000-WRITE-BILLING-DETAIL
01363                      THRU  3990-EXIT.
01364
01365      MOVE GX-AM-CARRIER (GX-SUB)   TO  PI-CR-CARRIER.
01366      MOVE GX-AM-GROUPING (GX-SUB)  TO  PI-CR-GROUPING.
01367      MOVE GX-AM-STATE (GX-SUB)     TO  PI-CR-STATE.
01368      MOVE GX-AM-ACCOUNT (GX-SUB)   TO  PI-SAV-ACCT
01369                                        PI-CR-ACCOUNT.
01370      MOVE GX-AM-EXPIRATION-DT (GX-SUB)
01371                                    TO  PI-SAV-EXP-DT.
01372
01373      PERFORM 4300-READ-ACCOUNT-MASTER  THRU  4390-EXIT.
01374
01375      IF INVALID-ACCOUNT
01376          GO TO 1005-GXRF-BILLING-PROCESS-LOOP.
01377
01378      IF PI-UPDATE-FILES
01379          MOVE WS-GA-LEVEL        TO  GX-AM-LEVEL-NO (GX-SUB)
01380          MOVE WS-CURRENT-DATE    TO  GX-LAST-BILL-DT (GX-SUB).
01381
01382      IF NEW-ACCOUNT
01383          IF PI-ACCT-BILLED
CIDMOD             PERFORM 1100-PYAJ-BILLING-PROCESS  THRU  1190-EXIT
01384              MOVE 'TA'           TO  BILLING-DETAIL-TYPE
01385              IF APRODSWI  =  'N'
01386                  NEXT SENTENCE
01387              ELSE
01388                  PERFORM 3000-WRITE-BILLING-DETAIL
01389                      THRU  3990-EXIT.
01390
01391      MOVE 'A'                    TO  COMPENSATION-SW
01392                                       PI-CR-TYPE.
01393
01394      PERFORM 6000-READ-COMP-MASTER     THRU 6090-EXIT.
01395      PERFORM 1100-PYAJ-BILLING-PROCESS THRU 1190-EXIT.
01396      PERFORM 4000-PNDB-START-BROWSE    THRU 4090-EXIT.
01397
01398      IF PNDB-EOF
01399          GO TO 1005-GXRF-BILLING-PROCESS-LOOP.
01400
01401  1010-PNDB-BILLING-PROCESS-LOOP.
01402      PERFORM 4100-PNDB-READ-NEXT  THRU  4150-EXIT.
01403
01404      IF PNDB-EOF
01405          PERFORM 4160-PNDB-END-BROWSE
01406          GO TO 1005-GXRF-BILLING-PROCESS-LOOP.
01407
01408      IF PB-COMPANY-CD-A1  =  PI-COMPANY-CD  AND
01409         PB-SV-CARRIER     =  PI-CR-CARRIER  AND
01410         PB-SV-GROUPING    =  PI-CR-GROUPING AND
01411         PB-SV-STATE       =  PI-CR-STATE    AND
01412         PB-ACCOUNT        =  PI-CR-ACCOUNT
01413          NEXT SENTENCE
01414      ELSE
01415          PERFORM 4160-PNDB-END-BROWSE
01416          GO TO 1005-GXRF-BILLING-PROCESS-LOOP.
01417
01418      IF PB-BATCH-TRAILER
01419          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01420
01421      IF PB-ALT-CHG-SEQ-NO  NOT =  ZEROS
01422          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01423
01424      IF PB-CERT-EFF-DT LESS GX-AM-EXPIRATION-DT (GX-SUB)
01425          NEXT SENTENCE
01426       ELSE
01427          PERFORM 4160-PNDB-END-BROWSE
01428          GO TO 1005-GXRF-BILLING-PROCESS-LOOP.
01429
01430      IF PB-CERT-EFF-DT  LESS THAN  GX-AM-EFF-DT (GX-SUB)
01431          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01432
01433      IF PB-CERT-EFF-DT  GREATER THAN  ERACCT-P-EXP-DATE
01434          PERFORM 4160-PNDB-END-BROWSE
01435          GO TO 1005-GXRF-BILLING-PROCESS-LOOP.
01436
01437      IF (PB-ISSUE)
01438        AND
01439         (PB-I-POLICY-IS-REISSUE OR PB-I-REIN-ONLY OR
122002         PB-I-POLICY-IS-MONTHLY OR
01440          PB-I-POLICY-IS-DECLINED OR PB-I-POLICY-IS-VOIDED OR
01441          PB-I-UNDERWRITE-POLICY)
01442            GO TO 1010-PNDB-BILLING-PROCESS-LOOP
01443      ELSE
01444          IF (PB-CANCELLATION)
01445            AND  (PB-CI-LF-POLICY-IS-REISSUE
01446              OR  PB-CI-AH-POLICY-IS-REISSUE
122002             OR  PB-CI-LF-POLICY-IS-MONTHLY
122002             OR  PB-CI-AH-POLICY-IS-MONTHLY
01447              OR  PB-CI-LF-POLICY-IS-DECLINED
01448              OR  PB-CI-AH-POLICY-IS-VOID
01449              OR  PB-CI-LF-REIN-ONLY
01450              OR  PB-CI-AH-REIN-ONLY)
01451                GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01452
01453      IF PB-CREDIT-ACCEPT-DT  NOT =  LOW-VALUES
01454          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01455
01456      IF PB-RECORD-ON-HOLD
01457          PERFORM 1040-UPDATE-BILLING-STATISTICS  THRU  1049-EXIT
01458          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01459
01460      IF PB-FATAL-ERRORS
01461        OR  PB-UNFORCED-ERRORS
01462          PERFORM 1040-UPDATE-BILLING-STATISTICS  THRU  1049-EXIT
01463          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01464
01465      MOVE 'Y'                    TO  PI-ACCT-BILLED-SW
01466                                      PI-DATA-BILLED-SW.
01467
01468      PERFORM 1040-UPDATE-BILLING-STATISTICS  THRU  1049-EXIT.
01469
01470      PERFORM 1030-COMPUTE-COMMISSION-TOTALS  THRU  1039-EXIT.
01471
01472      IF PI-BILL
01473        OR  PI-REBILLING
01474          PERFORM 4200-PNDB-REWRITE THRU 4290-EXIT.
01475
01476      GO TO 1010-PNDB-BILLING-PROCESS-LOOP.
01477  EJECT
01478  1030-COMPUTE-COMMISSION-TOTALS.
01479      IF PB-CANCELLATION
01480          GO TO 1035-COMPUTE-CANCEL-COMM.
01481
01482      PERFORM 4400-FIND-COMMISSION  THRU  4490-EXIT.
01483
01484      IF PB-OVERRIDE-LIFE
01485        OR  PB-OVERRIDE-BOTH
01486          COMPUTE WS-I-LF-PREMIUM-AMT = PB-I-LF-PREM-CALC +
01487                                        PB-I-LF-ALT-PREM-CALC
01488      ELSE
01489          COMPUTE WS-I-LF-PREMIUM-AMT = PB-I-LF-PREMIUM-AMT +
01490                                        PB-I-LF-ALT-PREMIUM-AMT.
01491
01492      IF PB-OVERRIDE-AH
01493        OR  PB-OVERRIDE-BOTH
01494          MOVE PB-I-AH-PREM-CALC  TO  WS-I-AH-PREMIUM-AMT
01495      ELSE
01496          MOVE PB-I-AH-PREMIUM-AMT
01497                                  TO  WS-I-AH-PREMIUM-AMT.
01498
01499      IF PB-GA-BILL-DT (WS-GA-LEVEL)  =  (LOW-VALUES OR SPACES)
01500        OR  PI-TOT-REBILL
01501          COMPUTE PI-UNPAID-NET-PREM = PI-UNPAID-NET-PREM +
01502              WS-I-LF-PREMIUM-AMT + WS-I-AH-PREMIUM-AMT
01503          COMPUTE WS-ACCT-UNPAID-NET-PREM =
01504              (WS-I-LF-PREMIUM-AMT + WS-I-AH-PREMIUM-AMT) +
01505               WS-ACCT-UNPAID-NET-PREM.
01506
01507      COMPUTE PI-PREMIUM = PI-PREMIUM +
01508          (WS-I-LF-PREMIUM-AMT + WS-I-AH-PREMIUM-AMT).
01509
01510      COMPUTE PI-LF-ISS-COMP ROUNDED =
01511          (WS-I-LF-PREMIUM-AMT * WS-GA-LF-COM).
01512
01513      COMPUTE PI-AH-ISS-COMP ROUNDED =
01514          (WS-I-AH-PREMIUM-AMT  * WS-GA-AH-COM).
01515
01516      IF PB-GA-BILL-DT (WS-GA-LEVEL)  =  (LOW-VALUES OR SPACES)
01517        OR  PI-TOT-REBILL
01518          COMPUTE WS-ACCT-LF-OVERWRITE =
01519              (PI-LF-ISS-COMP + WS-ACCT-LF-OVERWRITE)
01520          COMPUTE WS-ACCT-AH-OVERWRITE =
01521              (PI-AH-ISS-COMP + WS-ACCT-AH-OVERWRITE)
01522          COMPUTE PI-COMP-UNPAID-PREM = PI-COMP-UNPAID-PREM +
01523              PI-LF-ISS-COMP + PI-AH-ISS-COMP.
01524
01525      COMPUTE PI-TOT-ISS-COMP = PI-TOT-ISS-COMP +
01526          (PI-LF-ISS-COMP + PI-AH-ISS-COMP).
01527
01528  1033-COMPUTE-ACCOUNT-COMM.
01529
01530      COMPUTE WS-ACCT-NET-PREM ROUNDED = WS-ACCT-NET-PREM +
01531          (WS-I-LF-PREMIUM-AMT + WS-I-AH-PREMIUM-AMT).
01532
01533      IF PI-SAV-AGENT  NOT =  PI-SAV-REMIT-TO
01534          GO TO 1039-EXIT.
01535
01536      COMPUTE WS-ACCT-COMP ROUNDED =
01537          (WS-I-LF-PREMIUM-AMT * PB-I-LIFE-COMMISSION) +
01538          (WS-I-AH-PREMIUM-AMT * PB-I-AH-COMMISSION)   +
01539           WS-ACCT-COMP.
01540
01541      GO TO 1039-EXIT.
01542
01543  1035-COMPUTE-CANCEL-COMM.
01544      IF PB-CI-CERT-HAS-ERCOMM-ENTRY
01545          PERFORM 4700-PROCESS-COMM-EXCEPTION  THRU  4790-EXIT
01546      ELSE
01547          PERFORM 4400-FIND-COMMISSION  THRU  4490-EXIT.
01548
01549      IF PB-OVERRIDE-LIFE
01550        OR  PB-OVERRIDE-BOTH
01551          MOVE PB-C-LF-REF-CALC   TO  WS-C-LF-CANCEL-AMT
01552      ELSE
01553          MOVE PB-C-LF-CANCEL-AMT
01554                                  TO  WS-C-LF-CANCEL-AMT.
01555
01556      IF PB-OVERRIDE-AH
01557        OR  PB-OVERRIDE-BOTH
01558          MOVE PB-C-AH-REF-CALC   TO  WS-C-AH-CANCEL-AMT
01559      ELSE
01560          MOVE PB-C-AH-CANCEL-AMT
01561                                  TO  WS-C-AH-CANCEL-AMT.
01562
01563      IF PB-GA-BILL-DT (WS-GA-LEVEL)  =  LOW-VALUES  OR  SPACES
01564        OR  PI-TOT-REBILL
01565          COMPUTE PI-UNPAID-NET-PREM = PI-UNPAID-NET-PREM -
01566              (WS-C-LF-CANCEL-AMT + WS-C-AH-CANCEL-AMT)
01567          COMPUTE WS-ACCT-UNPAID-NET-PREM =
01568              WS-ACCT-UNPAID-NET-PREM -
01569              (WS-C-LF-CANCEL-AMT + WS-C-AH-CANCEL-AMT).
01570
01571      COMPUTE PI-PREMIUM = PI-PREMIUM -
01572          (WS-C-LF-CANCEL-AMT + WS-C-AH-CANCEL-AMT).
01573
01574      MOVE  PB-CERT-EFF-DT         TO  DC-BIN-DATE-1.
01575      MOVE  PB-C-LF-CANCEL-DT      TO  DC-BIN-DATE-2.
01576      MOVE  '1'                    TO  DC-OPTION-CODE.
01577      PERFORM  8500-DATE-CONVERT  THRU  8500-EXIT.
01578      MOVE  DC-ELAPSED-MONTHS      TO  MONTHS-DIFF-LF.
01579
01580      IF DC-ODD-DAYS-OVER  GREATER THAN   ZEROS
01581          ADD  +1                  TO  MONTHS-DIFF-LF.
01582
01583      MOVE  PB-CERT-EFF-DT         TO  DC-BIN-DATE-1.
01584      MOVE  PB-C-AH-CANCEL-DT      TO  DC-BIN-DATE-2.
01585      MOVE  '1'                    TO  DC-OPTION-CODE.
01586      PERFORM  8500-DATE-CONVERT  THRU  8500-EXIT.
01587      MOVE  DC-ELAPSED-MONTHS      TO  MONTHS-DIFF-AH.
01588
01589      IF DC-ODD-DAYS-OVER  GREATER THAN   ZEROS
01590          ADD  +1                  TO  MONTHS-DIFF-AH.
01591
01592      MOVE ZEROS                   TO  SUB.
01593      MOVE ZEROS                   TO  PI-LF-CAN-COMP
01594                                       PI-AH-CAN-COMP.
01595
01596  1036-COMPUTE-GA-LOOP.
01597
01598      ADD  +1                      TO  SUB.
01599
01600      IF SUB > +10
01601          MOVE  ZEROS              TO  SUB
01602          GO TO  1037-CONTINUE.
01603
01604      IF AM-AGT (SUB)   NOT  EQUAL  PI-SAV-AGENT
01605          GO TO  1036-COMPUTE-GA-LOOP.
01606
01607      IF AM-COM-TYP (SUB) = 'O' OR 'P' OR 'G' OR 'B'
01608          NEXT SENTENCE
01609      ELSE
01610          GO TO  1036-COMPUTE-GA-LOOP.
01611
01612      IF AM-COMM-CHARGEBACK (SUB) NOT NUMERIC
01613          MOVE  ZEROS               TO  AM-COMM-CHARGEBACK (SUB).
01614
01615      IF AM-COMM-CHARGEBACK (SUB) = '99'
01616          MOVE  ZEROS           TO  PI-LF-CAN-COMP
01617          GO TO 1037-CHECK-AH-COMM.
01618
01619      IF (MONTHS-DIFF-LF  GREATER THAN  AM-COMM-CHARGEBACK (SUB))
01620                               AND
01621         (AM-COMM-CHARGEBACK (SUB)  NOT  EQUAL   ZEROS)
01622          MOVE  ZEROS               TO  PI-LF-CAN-COMP
01623      ELSE
01624          COMPUTE PI-LF-CAN-COMP ROUNDED = PI-LF-CAN-COMP +
01625              (WS-C-LF-CANCEL-AMT * WS-GA-LF-COM).
01626
01627  1037-CHECK-AH-COMM.
01628
01629      IF AM-COMM-CHARGEBACK (SUB)    EQUAL   '99'
01630          MOVE  ZEROS           TO  PI-AH-CAN-COMP
01631          GO TO 1036-COMPUTE-GA-LOOP.
01632
01633      IF (MONTHS-DIFF-AH GREATER THAN  AM-COMM-CHARGEBACK (SUB))
01634                               AND
01635         (AM-COMM-CHARGEBACK (SUB)  NOT  EQUAL   ZEROS)
01636          MOVE  ZEROS               TO  PI-AH-CAN-COMP
01637      ELSE
01638          COMPUTE PI-AH-CAN-COMP ROUNDED =  PI-AH-CAN-COMP +
01639              (WS-C-AH-CANCEL-AMT  * WS-GA-AH-COM).
01640
01641      GO TO 1036-COMPUTE-GA-LOOP.
01642
01643  1037-CONTINUE.
01644
01645      IF PB-GA-BILL-DT (WS-GA-LEVEL)  =  (LOW-VALUES  OR  SPACES)
01646        OR  PI-TOT-REBILL
01647          COMPUTE WS-ACCT-LF-OVERWRITE =
01648              (WS-ACCT-LF-OVERWRITE - PI-LF-CAN-COMP)
01649          COMPUTE WS-ACCT-AH-OVERWRITE =
01650              (WS-ACCT-AH-OVERWRITE - PI-AH-CAN-COMP)
01651          COMPUTE PI-COMP-UNPAID-PREM = PI-COMP-UNPAID-PREM -
01652              PI-LF-CAN-COMP - PI-AH-CAN-COMP.
01653
01654      COMPUTE PI-TOT-CAN-COMP = PI-TOT-CAN-COMP +
01655          (PI-LF-CAN-COMP + PI-AH-CAN-COMP).
01656
01657  1038-COMPUTE-ACCOUNT-COMM.
01658
01659      COMPUTE WS-ACCT-NET-PREM = WS-ACCT-NET-PREM -
01660          (WS-C-LF-CANCEL-AMT + WS-C-AH-CANCEL-AMT).
01661
01662      IF PI-SAV-AGENT  NOT =  PI-SAV-REMIT-TO
01663          GO TO 1039-EXIT.
01664
01665      MOVE ZEROS                   TO  SUB.
01666
01667  103X-ACCOUNT-LOOP.
01668
01669      ADD  +1                      TO  SUB.
01670
01671      IF SUB > +10
01672          MOVE  ZEROS              TO  SUB
01673          GO TO  1039-EXIT.
01674
01675      IF AM-COM-TYP (SUB) = 'C' OR 'D' OR 'F' OR 'S'
01676          NEXT SENTENCE
01677      ELSE
01678          GO TO  103X-ACCOUNT-LOOP.
01679
01680      IF AM-COMM-CHARGEBACK (SUB)    NOT NUMERIC
01681          MOVE  ZEROS               TO  AM-COMM-CHARGEBACK (SUB).
01682
01683      IF AM-COMM-CHARGEBACK (SUB)   EQUAL   '99'
01684          MOVE  ZEROS          TO  WS-C-LF-CANCEL-AMT
01685          GO TO 103X-CHECK-AH-COMM.
01686
01687      IF (MONTHS-DIFF-LF  GREATER THAN  AM-COMM-CHARGEBACK (SUB))
01688                               AND
01689         (AM-COMM-CHARGEBACK (SUB)  NOT  EQUAL   ZEROS)
01690          MOVE  ZEROS              TO  WS-C-LF-CANCEL-AMT.
01691
01692  103X-CHECK-AH-COMM.
01693
01694      IF AM-COMM-CHARGEBACK (SUB)   EQUAL   '99'
01695          MOVE  ZEROS          TO  WS-C-AH-CANCEL-AMT
01696          GO TO 103X-COMPUTE-ACCT-TOT.
01697
01698      IF (MONTHS-DIFF-AH  GREATER THAN  AM-COMM-CHARGEBACK (SUB))
01699                               AND
01700         (AM-COMM-CHARGEBACK (SUB)  NOT  EQUAL   ZEROS)
01701          MOVE  ZEROS              TO  WS-C-AH-CANCEL-AMT.
01702
01703  103X-COMPUTE-ACCT-TOT.
01704
01705      COMPUTE WS-ACCT-COMP   ROUNDED  =  WS-ACCT-COMP  -
01706          ((WS-C-LF-CANCEL-AMT * PB-CI-LIFE-COMMISSION) +
01707           (WS-C-AH-CANCEL-AMT * PB-CI-AH-COMMISSION)).
01708
01709      GO TO 103X-ACCOUNT-LOOP.
01710
01711  1039-EXIT.
01712      EXIT.
01713  EJECT
01714  1040-UPDATE-BILLING-STATISTICS.
01715      IF PB-ISSUE
01716          IF PB-FATAL-ERRORS
01717            OR  PB-UNFORCED-ERRORS
01718            OR  PB-RECORD-ON-HOLD
01719              ADD +1              TO  PI-ISSUES-INER
01720              GO TO 1049-EXIT.
01721
01722      IF PB-CANCELLATION
01723          IF PB-FATAL-ERRORS
01724            OR  PB-UNFORCED-ERRORS
01725            OR  PB-RECORD-ON-HOLD
01726              ADD +1              TO  PI-CANCELS-INER
01727              GO TO 1049-EXIT.
01728
01729      IF PB-ISSUE
01730          IF PB-GA-BILL-DT (WS-GA-LEVEL)  =
01731                 (LOW-VALUES OR SPACES)
01732            OR  PI-TOT-REBILL
01733              ADD +1              TO  PI-ISSUES-BILLED
01734              GO TO 1049-EXIT.
01735
01736      IF PB-CANCELLATION
01737          IF PB-GA-BILL-DT (WS-GA-LEVEL)  =
01738                 (LOW-VALUES OR SPACES)
01739            OR  PI-TOT-REBILL
01740              ADD +1              TO  PI-CANCELS-BILLED
01741              GO TO 1049-EXIT.
01742
01743      IF PB-ISSUE
01744          IF PB-GA-BILL-DT (WS-GA-LEVEL)  NOT =  LOW-VALUES
01745            AND  SPACES
01746              ADD +1              TO  PI-ISSUES-PREV
01747              GO TO 1049-EXIT.
01748
01749      IF PB-CANCELLATION
01750          IF PB-GA-BILL-DT (WS-GA-LEVEL)  NOT =  LOW-VALUES
01751            AND  SPACES
01752              ADD +1              TO  PI-CANCELS-PREV
01753              GO TO 1049-EXIT.
01754
01755      ADD +1                      TO  PI-UNKNOWN.
01756
01757  1049-EXIT.
01758      EXIT.
01759  EJECT
01760  1100-PYAJ-BILLING-PROCESS.
01761 ******************************************************************
01762 *   PROCESS THE ACCOUNTS PAYMENTS AND ADUSTMENTS. IF THE GENERAL *
01763 *   AGENT IS NOT FINANCIALLY RESPONSIBLE FOR THE ACCOUNT DO NOT  *
01764 *   PROCESS THE ACCOUNT'S PAYMENTS AND ADJUSTMENTS.              *
01765 *                                                                *
01766 *   1.  PROCESS THE ACCOUNT'S PAYMENTS AND ADUSTMENTS.           *
01767 *                                                                *
01768 *   2.  IF THE GENERAL AGENT IS NOT FINANCIALLY RESPONSIBLE FOR  *
01769 *       THE ACCOUNT, DO NOT PROCESS THE ACCOUNT'S PAYMENTS AND   *
01770 *       ADJUSTMENTS.                                             *
01771 *                                                                *
01772 *   3.  IF PRINTED BILL IS NOT PRODUCED, DO NOT PROCESS THE      *
01773 *       ACCOUNT'S PAYMENTS AND ADJUSTMENTS.                      *
01774 ******************************************************************
01775
01776      IF PI-SAV-AGENT  =  PI-SAV-REMIT-TO
01777          NEXT SENTENCE
01778      ELSE
01779          GO TO 1190-EXIT.
01780
01781      IF PI-UPDATE-FILES
01782        OR  (PI-PREVIEW  AND  APRODSWI = 'Y')
01783          NEXT SENTENCE
01784      ELSE
01785          GO TO 1190-EXIT.
01786
01787  1105-PYAJ-BILLING-STARTBR-LOOP.
01788      MOVE 'A'                    TO  COMPENSATION-SW.
01789
01790      PERFORM 4500-PYAJ-START-BROWSE  THRU  4590-EXIT.
01791
01792      IF PYAJ-EOF
01793          GO TO 1190-EXIT.
01794
01795  1110-PYAJ-BILLING-READ-LOOP.
01796      PERFORM 4600-PYAJ-READ-NEXT  THRU  4690-EXIT.
01797
01798      IF PYAJ-EOF
01799          PERFORM 4800-PYAJ-END-BROWSE
01800          GO TO 1190-EXIT.
01801
01802      IF PY-COMPANY-CD  =  ERPYAJ-BR-COMP-CD
01803        AND  PY-CARRIER  =  ERPYAJ-BR-CARRIER
01804        AND  PY-GROUPING  =  ERPYAJ-BR-GROUPING
01805        AND  PY-FIN-RESP  =  ERPYAJ-BR-FIN-RESP
01806        AND  PY-ACCOUNT  =  ERPYAJ-BR-ACCOUNT
01807          NEXT SENTENCE
01808      ELSE
01809          PERFORM 4800-PYAJ-END-BROWSE
01810          GO TO 1190-EXIT.
01811
01812      IF PY-CREDIT-ACCEPT-DT  NOT =  LOW-VALUES
01813          GO TO 1110-PYAJ-BILLING-READ-LOOP.
01814
01815      IF PY-RECORD-TYPE  = 'R' OR 'D' OR 'C' OR 'S' OR 'T' OR 'U'
01816                               OR 'Z'
01817          NEXT SENTENCE
01818      ELSE
01819          GO TO 1110-PYAJ-BILLING-READ-LOOP.
01820
01821      IF PY-VOID-SW  NOT =  SPACES
01822          GO TO 1110-PYAJ-BILLING-READ-LOOP.
01823
01824      IF PI-AR-PROCESSING
01825         IF PY-AR-DATE  =  LOW-VALUES
01826            NEXT SENTENCE
01827         ELSE
01828            GO TO 1110-PYAJ-BILLING-READ-LOOP
01829      ELSE
01830         IF (PY-BILLED-DATE  =  LOW-VALUES AND
01831             PY-AR-DATE = LOW-VALUES)
01832            NEXT SENTENCE
01833         ELSE
01834            GO TO 1110-PYAJ-BILLING-READ-LOOP.
01835
01836      IF PY-REMIT-RECEIVED OR PY-ADJ-REM-RECEIVED OR
01837         PY-DEPOSIT OR PY-ADJ-DEPOSIT OR
01838         PY-ADD-TO-BALANCE
01839          ADD PY-ENTRY-AMT        TO  WS-ACCT-PAY-ADJS
01840      ELSE
01841          SUBTRACT PY-ENTRY-AMT   FROM  WS-ACCT-PAY-ADJS.
01842
01843      GO TO 1110-PYAJ-BILLING-READ-LOOP.
01844
01845  1190-EXIT.
01846      EXIT.
01847  EJECT
01848  1200-PYAJ-BILLING-COMPLETE.
01849 ******************************************************************
01850 *      1. IF PRINTED BILL IS PRODUCED PRINT THE GENERAL AGENT'S  *
01851 *         TOTALS.                                                *
01852 *                                                                *
01853 *      2. PROCESS THE GENERAL AGENT'S PAYMENTS AND ADJUSTMENTS.  *
01854 ******************************************************************
01855
01856      IF PI-DATA-BILLED
01857          NEXT SENTENCE
01858      ELSE
01859          MOVE ER-2913            TO  EMI-ERROR
01860          MOVE -1                 TO  AACCT1L
01861          MOVE AL-UABON           TO  AACCT1A
01862          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01863
01864      IF RETURNED-FROM  =  SPACES
01865          IF PI-UPDATE-FILES
01866            OR  (PI-PREVIEW  AND  APRODSWI = 'Y')
01867              IF PI-ACCT-BILLED
01868                  MOVE 'TB'       TO  BILLING-DETAIL-TYPE
01869                  PERFORM 3000-WRITE-BILLING-DETAIL
01870                          THRU  3990-EXIT
01871              ELSE
01872                  MOVE 'TS'       TO  BILLING-DETAIL-TYPE
01873                  PERFORM 3000-WRITE-BILLING-DETAIL
01874                          THRU  3990-EXIT.
01875
01876  1205-PYAJ-BILLING-STARTBR-LOOP.
01877      MOVE PI-SAV-AGENT           TO  PI-SAV-FIN-RESP.
01878      MOVE 'G'                    TO  COMPENSATION-SW.
01879
01880      PERFORM 4500-PYAJ-START-BROWSE  THRU  4590-EXIT.
01881
01882      IF PYAJ-EOF
01883          GO TO 1260-BILLING-COMPLETE.
01884
01885  1210-PYAJ-BILLING-READ-LOOP.
01886      PERFORM 4600-PYAJ-READ-NEXT  THRU  4690-EXIT.
01887
01888      IF PYAJ-EOF
01889          GO TO 1250-END-AGENTS-PAY-ADJS.
01890
01891      IF NOT PI-AR-PROCESSING
01892          IF PY-COMPANY-CD  =  ERPYAJ-BR-COMP-CD
01893            AND  PY-CARRIER  =  ERPYAJ-BR-CARRIER
01894            AND  PY-GROUPING  =  ERPYAJ-BR-GROUPING
01895            AND  PY-FIN-RESP  =  ERPYAJ-BR-FIN-RESP
01896            AND  PY-ACCOUNT  =  ERPYAJ-BR-ACCOUNT
01897              GO TO 1230-CHECK-PYAJ-ENTRY
01898          ELSE
01899              GO TO 1250-END-AGENTS-PAY-ADJS
01900      ELSE
01901          IF PY-COMPANY-CD  =  ERPYAJ-BR-COMP-CD
01902            AND  PY-CARRIER  =  ERPYAJ-BR-CARRIER
01903            AND  PY-GROUPING  =  ERPYAJ-BR-GROUPING
01904            AND  PY-FIN-RESP  =  ERPYAJ-BR-FIN-RESP
01905              NEXT SENTENCE
01906          ELSE
01907              GO TO 1250-END-AGENTS-PAY-ADJS.
01908
01909       IF PY-PMT-APPLIED = 'O' OR
01910          PY-ACCOUNT = LOW-VALUES
01911           NEXT SENTENCE
01912       ELSE
01913           GO TO 1210-PYAJ-BILLING-READ-LOOP.
01914
01915  1230-CHECK-PYAJ-ENTRY.
01916
01917      IF PY-CREDIT-ACCEPT-DT  NOT =  LOW-VALUES
01918          GO TO 1210-PYAJ-BILLING-READ-LOOP.
01919
01920      IF PY-RECORD-TYPE  = 'R' OR 'D' OR 'C' OR 'S' OR 'T' OR 'U'
01921                               OR 'Z'
01922          NEXT SENTENCE
01923      ELSE
01924          GO TO 1210-PYAJ-BILLING-READ-LOOP.
01925
01926      IF PY-VOID-SW  NOT =  SPACES
01927          GO TO 1210-PYAJ-BILLING-READ-LOOP.
01928
01929      IF PI-AR-PROCESSING
01930         IF PY-AR-DATE  =  LOW-VALUES
01931            OR  PI-TOT-REBILL
01932            OR  RETURNED-FROM  =  XCTL-PYAJ
01933                NEXT SENTENCE
01934         ELSE
01935            GO TO 1210-PYAJ-BILLING-READ-LOOP
01936      ELSE
01937         IF (PY-BILLED-DATE  =  LOW-VALUES AND
01938             PY-AR-DATE = LOW-VALUES)
01939            OR  PI-TOT-REBILL
01940            OR  RETURNED-FROM  =  XCTL-PYAJ
01941            NEXT SENTENCE
01942         ELSE
01943            GO TO 1210-PYAJ-BILLING-READ-LOOP.
01944
01945      IF PY-REMIT-RECEIVED OR PY-ADJ-REM-RECEIVED OR
01946         PY-DEPOSIT OR PY-ADJ-DEPOSIT OR
01947         PY-ADD-TO-BALANCE
01948          ADD PY-ENTRY-AMT        TO  PI-REMITTED
01949      ELSE
01950          IF PY-CHARGE-TO-AGENT
01951            OR  PY-ADJ-CHG-TO-AGT
01952              IF PY-GA-CHECK
01953                  ADD PY-ENTRY-AMT  TO  PI-DISBURSED
01954              ELSE
01955                  ADD PY-ENTRY-AMT  TO  PI-ADJUSTMNTS.
01956
01957      IF RETURNED-FROM  =  SPACES
01958          IF PI-UPDATE-FILES
01959            OR  (PI-PREVIEW  AND  APRODSWI = 'Y')
01960              MOVE 'BP'           TO  BILLING-DETAIL-TYPE
01961              PERFORM 3000-WRITE-BILLING-DETAIL  THRU  3990-EXIT.
01962
01963      GO TO 1210-PYAJ-BILLING-READ-LOOP.
01964
01965  1250-END-AGENTS-PAY-ADJS.
01966      PERFORM 4800-PYAJ-END-BROWSE.
01967
01968      IF PI-DATA-BILLED
01969          NEXT SENTENCE
01970      ELSE
01971          MOVE ER-2913            TO  EMI-ERROR
01972          MOVE -1                 TO  AACCT1L
01973          MOVE AL-UABON           TO  AACCT1A
01974          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01975
01976  1260-BILLING-COMPLETE.
01977      IF WS-UPDATE-FILES
01978          PERFORM 6500-REWRITE-CROSS-REFERENCE.
01979
01980      IF EMI-NO-ERRORS
01981          MOVE 0000               TO  EMI-ERROR
01982          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01983
01984      MOVE -1                     TO  AAGENTL.
01985
01986      PERFORM 5000-FORMAT-SCREEN  THRU  5090-EXIT.
01987
01988      GO TO 8100-SEND-INITIAL-MAP.
01989  EJECT
01990  2000-VOID-BILLING.
01991 ******************************************************************
01992 *     THIS SECTION WILL RESET THE GA-BILLING-FLAGS IN THE        *
01993 *     PENDING BUSINESS RECORDS AND RESET THE LAST BILLED         *
01994 *     DATE IN THE GENERAL AGENT'S CROSS-REFERENCE-FILE TO        *
01995 *     LOW-VALUES.                                                *
01996 ******************************************************************
01997
01998      MOVE SPACES                 TO  ELCNTL-KEY.
01999      MOVE '1'                    TO  ELCNTL-REC-TYPE.
02000
02001      PERFORM 6100-READ-CONTROL-FILE  THRU  6190-EXIT.
02002
02003      IF EMI-ERROR  NOT =  ZEROS
02004          GO TO 8200-SEND-DATAONLY.
02005
02006      MOVE CF-CURRENT-MONTH-END   TO  DC-BIN-DATE-1.
02007      MOVE SPACE                  TO  DC-OPTION-CODE.
02008
02009      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
02010
02011      MOVE DC-GREG-DATE-1-EDIT    TO  PI-MONTH-END-DATE.
02012
02013      PERFORM 6400-READ-CROSS-REFERENCE  THRU  6490-EXIT.
02014
02015      MOVE +0                     TO  GX-SUB.
02016
02017  2005-GXRF-BILLING-PROCESS-LOOP.
02018      ADD +1                      TO  GX-SUB.
02019
02020      IF GX-SUB  GREATER THAN  GX-AGENT-POINTER-CNT
02021          GO TO 2100-VOID-PNDB-COMPLETE.
02022
02023      MOVE GX-AM-CARRIER (GX-SUB)   TO  PI-CR-CARRIER.
02024      MOVE GX-AM-GROUPING (GX-SUB)  TO  PI-CR-GROUPING.
02025      MOVE GX-AM-STATE (GX-SUB)     TO  PI-CR-STATE.
02026      MOVE GX-AM-ACCOUNT (GX-SUB)   TO  PI-CR-ACCOUNT.
02027      MOVE GX-AGENT-NO              TO  PI-SAV-AGENT.
02028      MOVE GX-AM-EXPIRATION-DT (GX-SUB)
02029                                    TO  PI-SAV-EXP-DT.
02030      MOVE GX-AM-LEVEL-NO (GX-SUB)  TO  WS-GA-LEVEL.
02031      MOVE LOW-VALUES               TO  GX-LAST-BILL-DT (GX-SUB).
02032
02033      PERFORM 4000-PNDB-START-BROWSE  THRU  4090-EXIT.
02034
02035      IF PNDB-EOF
02036          PERFORM 4160-PNDB-END-BROWSE
02037          GO TO 2005-GXRF-BILLING-PROCESS-LOOP.
02038
02039  2010-VOID-PNDB-LOOP.
02040      PERFORM 4100-PNDB-READ-NEXT  THRU  4150-EXIT.
02041
02042      IF PNDB-EOF
02043          PERFORM 4160-PNDB-END-BROWSE
02044          GO TO 2005-GXRF-BILLING-PROCESS-LOOP.
02045
02046      IF PB-COMPANY-CD-A1  =  PI-COMPANY-CD
02047        AND  PB-SV-CARRIER  =  PI-CR-CARRIER
02048        AND  PB-SV-GROUPING  =  PI-CR-GROUPING
02049        AND  PB-SV-STATE  =  PI-CR-STATE
02050        AND  PB-ACCOUNT  =  PI-CR-ACCOUNT
02051          NEXT SENTENCE
02052      ELSE
02053          PERFORM 4160-PNDB-END-BROWSE
02054          GO TO 2005-GXRF-BILLING-PROCESS-LOOP.
02055
02056      IF PB-CREDIT-ACCEPT-DT  NOT =  LOW-VALUES
02057          GO TO 2010-VOID-PNDB-LOOP.
02058
02059      IF PB-GA-BILL-DT (WS-GA-LEVEL)  =  (LOW-VALUES  OR  SPACES)
02060          GO TO 2010-VOID-PNDB-LOOP.
02061
02062      IF PB-CERT-EFF-DT  =  GX-AM-EXPIRATION-DT (GX-SUB)
02063        OR  PB-CERT-EFF-DT  GREATER THAN
02064                GX-AM-EXPIRATION-DT (GX-SUB)
02065          PERFORM 4160-PNDB-END-BROWSE
02066          GO TO 2005-GXRF-BILLING-PROCESS-LOOP.
02067
02068      IF PB-CERT-EFF-DT  LESS THAN  GX-AM-EFF-DT (GX-SUB)
02069          GO TO 2010-VOID-PNDB-LOOP.
02070
02071      IF PB-BATCH-TRAILER
02072          GO TO 2010-VOID-PNDB-LOOP.
02073
02074      PERFORM 4200-PNDB-REWRITE  THRU  4290-EXIT.
02075
02076      MOVE 'Y'                    TO  DATA-VOIDED-SW.
02077
02078      GO TO 2010-VOID-PNDB-LOOP.
02079
02080  2100-VOID-PNDB-COMPLETE.
02081      IF DATA-VOIDED
02082          NEXT SENTENCE
02083      ELSE
02084          MOVE -1                 TO  APFNTERL
02085          MOVE ER-2401            TO  EMI-ERROR
02086          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02087          GO TO 8100-SEND-INITIAL-MAP.
02088
02089      PERFORM 3940-BILL-GENERIC-DELETE  THRU  3940-EXIT.
02090
02091      PERFORM 6500-REWRITE-CROSS-REFERENCE  THRU  6590-EXIT.
02092
02093  2105-VOID-COMPLETE.
02094      MOVE 0000                   TO  EMI-ERROR
02095
02096      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02097
02098      MOVE -1                     TO  AAGENTL.
02099
02100      GO TO 8100-SEND-INITIAL-MAP.
02101  EJECT
02102  3000-WRITE-BILLING-DETAIL.
02103 ******************************************************************
02104 *     THIS SECTION FORMATS THE BILLING PRINT RECORDS.  THESE     *
02105 *     RECORDS ARE CREATED IN THE SAME FORMAT THAT THEY APPEAR    *
02106 *     WHEN PRINTED.                                              *
02107 *                                                                *
02108 *     THIS SECTION PRINTS THE BILLING STATEMENT AND PRINT LABELS.*
02109 *     BILLING-DETAIL-TYPES                                       *
02110 *            TOTAL-STATEMENT   TS                                *
02111 *            TOTAL-ACCOUNT     TA                                *
02112 *            TOTAL-BOTH        TB                                *
02113 *            GAN-AGT-ADDRESS   GA                                *
02114 *                              BP                                *
02115 ******************************************************************
02116
02117      IF GEN-AGT-ADDRESS
02118          PERFORM 3930-BILL-READ  THRU  3930-EXIT
02119          IF NO-BILL-RECS
02120              GO TO 3200-FORMAT-HDR-ADDR-RECS
02121          ELSE
02122              IF BI-PREVIEW-ONLY
02123                OR  BI-INITIAL-PRINT-DATE  NOT =  LOW-VALUES
02124                OR  PI-TOT-REBILL
02125                  PERFORM 3940-BILL-GENERIC-DELETE
02126                      THRU  3940-EXIT
02127                  GO TO 3200-FORMAT-HDR-ADDR-RECS
02128              ELSE
02129                  MOVE ER-2403    TO  EMI-ERROR
02130                  MOVE -1         TO  ABILTYPL
02131                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02132                  GO TO 8200-SEND-DATAONLY.
02133
02134  3100-FORMAT-DETAIL-LINES.
02135      IF WS-LINECTR  GREATER THAN  23
02136          MOVE ZEROS              TO  WS-LINECTR
02137          MOVE '3'                TO  BI-RECORD-TYPE
02138          MOVE TRIPLE-SPACE       TO  BI-SKIP-CONTROL
02139          MOVE '          CONTINUED ON NEXT PAGE'
02140                                  TO  BI-TEXT-LINE
02141          PERFORM 3920-BILL-WRITE
02142          PERFORM 3300-FORMAT-HEADINGS  THRU  3380-EXIT.
02143
02144      IF WS-LINECTR  =  ZEROS
02145          MOVE DOUBLE-SPACE       TO  BI-SKIP-CONTROL
02146      ELSE
02147          MOVE SINGLE-SPACE       TO  BI-SKIP-CONTROL.
02148
02149      MOVE '3'                    TO  BI-RECORD-TYPE.
02150      MOVE SPACES                 TO  GA-TEXT-LINE.
02151
02152      IF TOTAL-STATEMENT
02153          GO TO 3175-CHECK-AGENT-TOTAL.
02154
02155      IF BILLING-DETAIL-TYPE  =  'BP'
02156         IF PY-CHARGE-TO-AGENT OR PY-ADJ-CHG-TO-AGT
CIDMOD            MOVE PY-ENTRY-COMMENT
CIDMOD                                 TO  GA-ENTRY-DESC
02157             MULTIPLY PY-ENTRY-AMT  BY  -1
02158                 GIVING  GA-PMTS-ADJS
02159             MOVE PY-ENTRY-COMMENT
02160                                  TO  GA-ENTRY-DESC
02161             MOVE '* ACCOUNTING ENTRY *'
02162                                  TO  GA-ENTRY-COMMENT
02163             ADD +1               TO  WS-LINECTR
02164             IF PY-ACCOUNT = LOW-VALUES
02165                 MOVE SPACES      TO  GA-ACCT
02166                 PERFORM 3920-BILL-WRITE
02167                 GO TO 3990-EXIT
02168             ELSE
02169                 MOVE PY-ACCOUNT  TO  GA-ACCT
02170                 PERFORM 3920-BILL-WRITE
02171                 GO TO 3990-EXIT.
02172
02173      IF BILLING-DETAIL-TYPE  =  'BP'
02174          MOVE PY-ENTRY-COMMENT   TO  GA-ENTRY-DESC
02175          MOVE PY-ENTRY-AMT       TO  GA-PMTS-ADJS
02176          MOVE '* ACCOUNTING ENTRY *'
02177                                  TO  GA-ENTRY-COMMENT
02178          MOVE PY-ACCOUNT         TO  GA-ACCT
02179          ADD +1                  TO  WS-LINECTR
02180             IF PY-ACCOUNT = LOW-VALUES
02181                 MOVE SPACES      TO  GA-ACCT
02182                 PERFORM 3920-BILL-WRITE
02183                 GO TO 3990-EXIT
02184             ELSE
02185                 MOVE PY-ACCOUNT  TO  GA-ACCT
02186                 PERFORM 3920-BILL-WRITE
02187                 GO TO 3990-EXIT.
02188
02189  EJECT
02190  3150-FORMAT-ACCOUNT-LINE.
02191      MOVE SPACES                 TO  GA-TEXT-LINE.
02192      MOVE PI-SAV-ACCT-AGT        TO  GA-ACCT.
02193      MOVE PI-SAV-ACCT-NAME       TO  GA-ACCT-NAME.
02194      MOVE WS-ACCT-BEG-BAL        TO  GA-BEG-BAL
02195                                      GA-BEG-BAL-AMT.
02196      MOVE WS-ACCT-NET-PREM       TO  GA-NET-PREM
02197                                      GA-NET-PREM-AMT.
02198      MOVE WS-ACCT-COMP           TO  GA-ACCT-COMP
02199                                      GA-ACCT-COMP-AMT.
02200      MOVE WS-ACCT-PAY-ADJS       TO  GA-PMTS-ADJS
02201                                      GA-PMTS-ADJS-AMT.
02202
02203      IF PI-SAV-AGENT  NOT =  PI-SAV-REMIT-TO
02204          MOVE ZEROS              TO  GA-END-BAL-AMT
02205      ELSE
02206          COMPUTE GA-END-BAL-AMT =
02207              (WS-ACCT-BEG-BAL + WS-ACCT-NET-PREM) -
02208               (WS-ACCT-COMP + WS-ACCT-PAY-ADJS).
02209
02210      MOVE WS-ACCT-UNPAID-NET-PREM
02211                                  TO  GA-UNPAID-NET-PREM
02212                                      GA-UNPAID-NET-AMT.
02213      MOVE ZEROS                  TO  GA-OVERWRITE-AMT.
02214
02215      IF WS-ACCT-LF-OVERWRITE  NOT =  ZEROS
02216          MOVE PI-LIFE-OVERRIDE-L6
02217                                  TO  GA-BEN-OVERRIDE-L6
02218          MOVE PI-LIFE-OVERRIDE-L2
02219                                  TO  GA-BENEFIT-CD
02220          MOVE WS-ACCT-LF-OVERWRITE
02221                                  TO  GA-OVERWRITE
02222                                      GA-OVERWRITE-AMT
02223      ELSE
02224          IF WS-ACCT-AH-OVERWRITE  NOT =  ZEROS
02225              MOVE PI-AH-OVERRIDE-L6
02226                                  TO  GA-BEN-OVERRIDE-L6
02227              MOVE PI-AH-OVERRIDE-L2
02228                                  TO  GA-BENEFIT-CD
02229              MOVE WS-ACCT-AH-OVERWRITE
02230                                  TO  GA-OVERWRITE
02231                                      GA-OVERWRITE-AMT.
02232
02233      COMPUTE GA-AMT-DUE-AMT = GA-END-BAL-AMT -
02234          (WS-ACCT-LF-OVERWRITE + WS-ACCT-AH-OVERWRITE).
02235
02236      ADD  GA-AMT-DUE-AMT         TO  WS-GA-AMT-DUE.
02237
02238      MOVE GA-AMT-DUE-AMT         TO  GA-AMT-DUE.
02239
02240      ADD +1                      TO  WS-LINECTR.
02241
02242      PERFORM 3920-BILL-WRITE.
02243
02244      MOVE SPACES                 TO  GA-TEXT-LINE.
02245      MOVE SINGLE-SPACE           TO  BI-SKIP-CONTROL.
02246
02247      IF WS-ACCT-LF-OVERWRITE  NOT =  ZEROS
02248          IF WS-ACCT-AH-OVERWRITE  NOT =  ZEROS
02249              MOVE PI-AH-OVERRIDE-L6
02250                                  TO  GA-BEN-OVERRIDE-L6
02251              MOVE PI-AH-OVERRIDE-L2
02252                                  TO  GA-BENEFIT-CD
02253              MOVE WS-ACCT-AH-OVERWRITE
02254                                  TO  GA-OVERWRITE
02255                                      GA-OVERWRITE-AMT
02256              MOVE ZEROS          TO  GA-BEG-BAL-AMT
02257                                      GA-END-BAL-AMT
02258                                      GA-NET-PREM-AMT
02259                                      GA-ACCT-COMP-AMT
02260                                      GA-PMTS-ADJS-AMT
02261                                      GA-UNPAID-NET-AMT
02262                                      GA-AMT-DUE-AMT
02263              ADD +1              TO  WS-LINECTR
02264              PERFORM 3920-BILL-WRITE.
02265
02266      ADD WS-ACCT-BEG-BAL         TO  WS-GA-BEG-BAL
02267                                      PI-ACCT-BEG-BAL.
02268      ADD WS-ACCT-NET-PREM        TO  WS-GA-NET-PREM
02269                                      PI-ACCT-NET-PREM.
02270      ADD WS-ACCT-COMP            TO  WS-GA-COMP
02271                                      PI-ACCT-COMP.
02272      ADD WS-ACCT-PAY-ADJS        TO  WS-GA-PAY-ADJS
02273                                      PI-ACCT-PAY-ADJS.
02274      ADD WS-ACCT-UNPAID-NET-PREM
02275                                  TO  WS-GA-UNPAID-NET-PREM.
02276      ADD WS-ACCT-LF-OVERWRITE    TO  WS-GA-LF-OVERWRITE.
02277      ADD WS-ACCT-AH-OVERWRITE    TO  WS-GA-AH-OVERWRITE.
02278      ADD GA-END-BAL-AMT          TO  WS-GA-END-BAL.
02279
02280      MOVE ZEROS                  TO  WS-ACCT-BEG-BAL
02281                                      WS-ACCT-NET-PREM
02282                                      WS-ACCT-COMP
02283                                      WS-ACCT-PAY-ADJS
02284                                      WS-ACCT-UNPAID-NET-PREM
02285                                      WS-ACCT-LF-OVERWRITE
02286                                      WS-ACCT-AH-OVERWRITE.
02287      MOVE SPACE                  TO  PI-ACCT-BILLED-SW.
02288      MOVE SPACE                  TO  ACCOUNT-CARRY-BAL-SW.
02289
02290      IF TOTAL-STATEMENT
02291          GO TO 3400-FORMAT-TOTAL-LINE.
02292
02293  3175-CHECK-AGENT-TOTAL.
02294
02295      IF TOTAL-STATEMENT OR
02296         TOTAL-BOTH
02297          GO TO 3400-FORMAT-TOTAL-LINE.
02298
02299      GO TO 3990-EXIT.
02300  EJECT
02301  3200-FORMAT-HDR-ADDR-RECS.
02302      PERFORM 3910-BILL-GETMAIN.
02303
02304      MOVE ZEROS                  TO  WS-LINE-SEQ-NO.
02305
02306      SET A-INDX                  TO  1.
02307
02308      PERFORM 3210-FORMAT-ADDR-RECS  THRU  3210-EXIT  6  TIMES.
02309
02310      MOVE ZEROS                  TO  WS-LINE-SEQ-NO.
02311
02312      PERFORM 3300-FORMAT-HEADINGS  THRU  3380-EXIT.
02313
02314      PERFORM 3220-WRITE-AGENTS-BAL  THRU  3220-EXIT.
02315
02316      GO TO 3990-EXIT.
02317
02318  3210-FORMAT-ADDR-RECS.
02319      MOVE '2'                    TO  BI-RECORD-TYPE.
02320      MOVE SPACES                 TO  GA-TEXT-LINE.
02321      MOVE WS-AGENT-LINES (A-INDX)
02322                                  TO  GA-ACCT-ADDRESS-LINE.
02323
02324      PERFORM 3920-BILL-WRITE.
02325
02326      SET A-INDX                  UP  BY  1.
02327
02328  3210-EXIT.
02329      EXIT.
02330
02331  3220-WRITE-AGENTS-BAL.
02332      MOVE SPACES                 TO GA-TEXT-LINE.
02333      MOVE DOUBLE-SPACE           TO  BI-SKIP-CONTROL.
02334
02335      PERFORM 3920-BILL-WRITE.
02336
02337      MOVE 'BEGINNING BALANCE'    TO  GA-ACCT-NAME
02338      MOVE PI-BAL-FRWD            TO  GA-BEG-BAL.
02339      MOVE DOUBLE-SPACE           TO  BI-SKIP-CONTROL.
02340
02341      PERFORM 3920-BILL-WRITE.
02342
02343      MOVE SPACES                 TO  GA-TEXT-LINE.
02344      MOVE DOUBLE-SPACE           TO  BI-SKIP-CONTROL.
02345
02346      PERFORM 3920-BILL-WRITE.
02347
02348  3220-EXIT.
02349      EXIT.
02350  EJECT
02351  3300-FORMAT-HEADINGS.
02352      MOVE WS-PGECTR              TO  HD-PG.
02353
02354      ADD +1                      TO  WS-PGECTR.
02355
02356      MOVE '3'                    TO  BI-RECORD-TYPE.
02357      MOVE TOP-OF-PAGE            TO  BI-SKIP-CONTROL.
02358
02359      IF NOT PI-PREVIEW
02360          MOVE GA-HD1             TO  GA-TEXT-LINE
02361      ELSE
02362          MOVE GA-PREVIEW-HD      TO  GA-TEXT-LINE
02363          PERFORM 3920-BILL-WRITE
02364          MOVE '3'                TO  BI-RECORD-TYPE
02365          MOVE DOUBLE-SPACE       TO  BI-SKIP-CONTROL
02366          MOVE GA-HD1             TO  GA-TEXT-LINE.
02367
02368      PERFORM 3920-BILL-WRITE.
02369
02370      MOVE SINGLE-SPACE           TO  BI-SKIP-CONTROL.
02371      MOVE GA-HD2                 TO  GA-TEXT-LINE.
02372
02373      PERFORM 3920-BILL-WRITE.
02374
02375      MOVE SINGLE-SPACE           TO  BI-SKIP-CONTROL.
02376      MOVE GA-HD3                 TO  GA-TEXT-LINE.
02377
02378      PERFORM 3920-BILL-WRITE.
02379
02380      SET A-INDX                  TO  1.
02381
02382      PERFORM 3390-FORMAT-ADDR-TEXT-LINES  THRU  3390-EXIT
02383          6  TIMES.
02384
02385      MOVE DOUBLE-SPACE           TO  BI-SKIP-CONTROL.
02386      MOVE GA-HD4                 TO  GA-TEXT-LINE.
02387
02388      PERFORM 3920-BILL-WRITE.
02389
02390      MOVE SINGLE-SPACE           TO  BI-SKIP-CONTROL.
02391      MOVE GA-HD5                 TO  GA-TEXT-LINE.
02392
02393      PERFORM 3920-BILL-WRITE.
02394
02395  3380-EXIT.
02396      EXIT.
02397
02398  3390-FORMAT-ADDR-TEXT-LINES.
02399      MOVE '3'                    TO  BI-RECORD-TYPE.
02400      MOVE SPACES                 TO  GA-TEXT-LINE.
02401
02402      IF A-INDX  =  1
02403          MOVE DOUBLE-SPACE       TO  BI-SKIP-CONTROL
02404          MOVE PI-SAV-CARR        TO  GA-CARRIER
02405          MOVE PI-SAV-GROUP       TO  GA-GROUPING
02406          MOVE PI-SAV-AGENT       TO  GA-AGENT
02407          MOVE '-'                TO  GA-DASH
02408      ELSE
02409          MOVE SINGLE-SPACE       TO  BI-SKIP-CONTROL.
02410
02411      MOVE WS-AGENT-LINES (A-INDX)  TO  GA-AGENT-ADDR.
02412
02413      PERFORM 3920-BILL-WRITE.
02414
02415      SET A-INDX                  UP  BY  1.
02416
02417  3390-EXIT.
02418      EXIT.
02419  EJECT
02420  3400-FORMAT-TOTAL-LINE.
02421      MOVE SPACES                 TO  GA-TEXT-LINE.
02422      MOVE DOUBLE-SPACE           TO  BI-SKIP-CONTROL.
02423      MOVE '3'                    TO  BI-RECORD-TYPE.
02424      MOVE SPACES                 TO  GA-TEXT-LINE.
02425
02426      PERFORM 3920-BILL-WRITE.
02427
02428      MOVE SPACES                 TO  GA-TEXT-LINE.
02429      MOVE 'AGENT TOTAL'          TO  GA-ACCT-NAME.
02430      MOVE WS-GA-BEG-BAL          TO  GA-BEG-BAL
02431                                      GA-BEG-BAL-AMT.
02432      MOVE WS-GA-NET-PREM         TO  GA-NET-PREM
02433                                      GA-NET-PREM-AMT.
02434      MOVE WS-GA-COMP             TO  GA-ACCT-COMP
02435                                      GA-ACCT-COMP-AMT.
02436      MOVE WS-GA-PAY-ADJS         TO  GA-PMTS-ADJS
02437                                      GA-PMTS-ADJS-AMT.
02438      MOVE WS-GA-END-BAL          TO  GA-END-BAL-AMT.
02439      MOVE WS-GA-UNPAID-NET-PREM  TO  GA-UNPAID-NET-PREM
02440                                      GA-UNPAID-NET-AMT.
02441      MOVE PI-LIFE-OVERRIDE-L6    TO  GA-BEN-OVERRIDE-L6.
02442      MOVE PI-LIFE-OVERRIDE-L2    TO  GA-BENEFIT-CD.
02443      MOVE WS-GA-LF-OVERWRITE     TO  GA-OVERWRITE
02444                                      GA-OVERWRITE-AMT.
02445      MOVE WS-GA-AMT-DUE          TO  GA-AMT-DUE-AMT
02446                                      GA-AMT-DUE.
02447
02448      PERFORM 3920-BILL-WRITE.
02449
02450      MOVE SPACES                 TO  GA-TEXT-LINE.
02451      MOVE SINGLE-SPACE           TO  BI-SKIP-CONTROL.
02452      MOVE PI-AH-OVERRIDE-L6      TO  GA-BEN-OVERRIDE-L6
02453      MOVE PI-AH-OVERRIDE-L2      TO  GA-BENEFIT-CD
02454      MOVE WS-GA-AH-OVERWRITE     TO  GA-OVERWRITE
02455                                      GA-OVERWRITE-AMT.
02456
02457      PERFORM 3920-BILL-WRITE.
02458
02459      MOVE SPACES                 TO  GA-TEXT-LINE.
02460      MOVE SINGLE-SPACE           TO  BI-SKIP-CONTROL.
02461
02462      PERFORM 3920-BILL-WRITE.
02463
02464      MOVE SPACES                 TO  GA-TEXT-LINE.
02465      MOVE '1'                    TO  BI-RECORD-TYPE.
02466      MOVE PI-PROCESSOR-ID        TO  GA-PROCESSOR-CD.
02467
02468      IF PI-PREV-BILL
02469        OR  PI-PREV-REBILL
02470          MOVE 'P'                TO  GA-STATEMENT-TYPE.
02471
02472      MOVE +1                     TO  GA-NO-OF-COPIES.
02473      MOVE WS-CURRENT-DATE        TO  GA-CREATION-DT.
02474      MOVE LOW-VALUES             TO  GA-INITIAL-PRINT-DATE.
02475      MOVE PI-UNPAID-NET-PREM     TO  GA-NET-UNPD.
02476      MOVE PI-COMP-UNPAID-PREM    TO  GA-COMP-UNPD-PREM.
02477      MOVE PI-PREMIUM             TO  GA-PREMIUM.
02478      MOVE PI-REMITTED            TO  GA-REMITTED.
02479      MOVE PI-TOT-ISS-COMP        TO  GA-TOT-ISS-COMP.
02480      MOVE PI-TOT-CAN-COMP        TO  GA-TOT-CAN-COMP.
02481      MOVE PI-ADJUSTMNTS          TO  GA-ADJUSTMNTS.
02482      MOVE PI-DISBURSED           TO  GA-DISBURSED.
02483      MOVE PI-AGENT-NAME          TO  GA-AGENTS-NAME.
02484
02485      PERFORM 3920-BILL-WRITE.
02486
02487      GO TO 3990-EXIT.
02488  EJECT
02489  3910-BILL-GETMAIN.
02490      
      * EXEC CICS GETMAIN
02491 *        SET      (ADDRESS OF BILLING-STATEMENT)
02492 *        LENGTH   (210)
02493 *        INITIMG  (GETMAIN-SPACE)
02494 *    END-EXEC.
           MOVE 210
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007263' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF BILLING-STATEMENT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02495
02496  3920-BILL-WRITE.
02497      MOVE PI-COMPANY-CD          TO  BI-COMPANY-CD.
02498      MOVE PI-SAV-CARR            TO  BI-CARRIER.
02499      MOVE PI-SAV-GROUP           TO  BI-GROUPING.
02500      MOVE LOW-VALUES             TO  BI-ACCOUNT.
02501      MOVE PI-SAV-AGENT           TO  BI-FIN-RESP.
02502      MOVE 'BI'                   TO  BI-RECORD-ID.
02503
02504      ADD +1                      TO  WS-LINE-SEQ-NO.
02505
02506      IF BI-RECORD-TYPE = '1'
02507          MOVE ZEROS              TO  BI-LINE-SEQ-NO
02508      ELSE
02509          MOVE WS-LINE-SEQ-NO     TO  BI-LINE-SEQ-NO.
02510
02511      
      * EXEC CICS WRITE
02512 *        DATASET  (ERBILL-FILE-ID)
02513 *        FROM     (BILLING-STATEMENT)
02514 *        RIDFLD   (BI-CONTROL-PRIMARY)
02515 *    END-EXEC.
           MOVE LENGTH OF
            BILLING-STATEMENT
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007284' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERBILL-FILE-ID, 
                 BILLING-STATEMENT, 
                 DFHEIV11, 
                 BI-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02516
02517  3930-BILL-READ.
02518      MOVE PI-COMPANY-CD          TO  ERBILL-CO-CD.
02519      MOVE PI-SAV-CARR            TO  ERBILL-CARRIER.
02520      MOVE PI-SAV-GROUP           TO  ERBILL-GROUP.
02521      MOVE LOW-VALUES             TO  ERBILL-ACCT.
02522      MOVE PI-SAV-AGENT           TO  ERBILL-FIN-RESP.
02523      MOVE '1'                    TO  ERBILL-REC-TYPE.
02524      MOVE ZEROS                  TO  ERBILL-LINE-SEQ-NO.
02525
02526      
      * EXEC CICS HANDLE CONDITION
02527 *        NOTFND  (3930-BILL-NOTFND)
02528 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00007299' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303037323939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02529
02530      
      * EXEC CICS READ
02531 *        SET      (ADDRESS OF BILLING-STATEMENT)
02532 *        DATASET  (ERBILL-FILE-ID)
02533 *        RIDFLD   (ERBILL-KEY)
02534 *        GTEQ
02535 *    END-EXEC.
      *    MOVE '&"S        G          (   #00007303' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERBILL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERBILL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BILLING-STATEMENT TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02536
02537      IF BI-COMPANY-CD  =  PI-COMPANY-CD
02538        AND  BI-CARRIER  =  ERBILL-CARRIER
02539        AND  BI-GROUPING  =  ERBILL-GROUP
02540        AND  BI-ACCOUNT  =  ERBILL-ACCT
02541        AND  BI-FIN-RESP  =  ERBILL-FIN-RESP
02542          MOVE SPACE              TO  NO-BILL-REC-SW
02543          GO TO 3930-EXIT
02544      ELSE
02545          GO TO 3930-BILL-NOTFND.
02546
02547  3930-BILL-NOTFND.
02548      MOVE 'Y'                    TO  NO-BILL-REC-SW.
02549
02550  3930-EXIT.
02551      EXIT.
02552
02553  3940-BILL-GENERIC-DELETE.
02554      
      * EXEC CICS HANDLE CONDITION
02555 *        NOTFND  (3940-EXIT)
02556 *    END-EXEC.
      *    MOVE '"$I                   ! % #00007327' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303037333237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02557
02558      MOVE PI-COMPANY-CD          TO  ERBILL-CO-CD.
02559      MOVE PI-SAV-CARR            TO  ERBILL-CARRIER.
02560      MOVE PI-SAV-GROUP           TO  ERBILL-GROUP.
02561      MOVE LOW-VALUES             TO  ERBILL-ACCT.
02562      MOVE PI-SAV-AGENT           TO  ERBILL-FIN-RESP.
02563
02564      
      * EXEC CICS DELETE
02565 *        DATASET    (ERBILL-FILE-ID)
02566 *        RIDFLD     (ERBILL-KEY)
02567 *        KEYLENGTH  (28)
02568 *        GENERIC
02569 *    END-EXEC.
           MOVE 28
             TO DFHEIV11
      *    MOVE '&(  RKG               &   #00007337' TO DFHEIV0
           MOVE X'26282020524B472020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERBILL-FILE-ID, 
                 ERBILL-KEY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02570
02571  3940-EXIT.
02572      EXIT.
02573
02574  3990-EXIT.
02575      EXIT.
02576  EJECT
02577  4000-PNDB-START-BROWSE.
02578      MOVE SPACES                 TO  ERPNDB-ALT-KEY.
02579      MOVE PI-COMPANY-CD          TO  ERPNDB-CO-CD-A1.
02580
02581      IF CARR-GROUP-ST-ACCNT-CNTL
02582          MOVE GX-AM-CARRIER  (GX-SUB)  TO  ERPNDB-CARR
02583          MOVE GX-AM-GROUPING (GX-SUB)  TO  ERPNDB-GROUP
02584          MOVE GX-AM-STATE    (GX-SUB)  TO  ERPNDB-STATE
02585          MOVE GX-AM-ACCOUNT  (GX-SUB)  TO  ERPNDB-ACCT
02586      ELSE
02587      IF CARR-ST-ACCNT-CNTL
02588          MOVE GX-AM-CARRIER (GX-SUB)   TO  ERPNDB-CARR
02589          MOVE GX-AM-STATE   (GX-SUB)   TO  ERPNDB-STATE
02590          MOVE GX-AM-ACCOUNT (GX-SUB)   TO  ERPNDB-ACCT
02591      ELSE
02592      IF CARR-ACCNT-CNTL
02593          MOVE GX-AM-CARRIER (GX-SUB)   TO  ERPNDB-CARR
02594          MOVE GX-AM-ACCOUNT (GX-SUB)   TO  ERPNDB-ACCT
02595      ELSE
02596      IF ACCNT-CNTL
02597          MOVE GX-AM-ACCOUNT (GX-SUB)   TO  ERPNDB-ACCT
02598      ELSE
02599          MOVE GX-AM-STATE   (GX-SUB)   TO  ERPNDB-STATE
02600          MOVE GX-AM-ACCOUNT (GX-SUB)   TO  ERPNDB-ACCT.
02601
02602      MOVE GX-AM-EFF-DT (GX-SUB)  TO  ERPNDB-EFF-DT.
02603
02604      
      * EXEC CICS HANDLE CONDITION
02605 *        NOTFND  (4010-REC-NOT-FND)
02606 *    END-EXEC.
      *    MOVE '"$I                   ! & #00007377' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303037333737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02607
02608      
      * EXEC CICS STARTBR
02609 *        DATASET  (ERPNDB-ALT-FILE-ID)
02610 *        RIDFLD   (ERPNDB-ALT-KEY)
02611 *        GTEQ
02612 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007381' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-ALT-FILE-ID, 
                 ERPNDB-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02613
02614      GO TO 4090-EXIT.
02615
02616  4010-REC-NOT-FND.
02617      MOVE 'Y'                    TO  PNDB-EOF-SW.
02618
02619  4090-EXIT.
02620      EXIT.
02621  EJECT
02622  4100-PNDB-READ-NEXT.
02623      
      * EXEC CICS HANDLE CONDITION
02624 *        ENDFILE  (4110-END-OF-FILE)
02625 *    END-EXEC.
      *    MOVE '"$''                   ! '' #00007396' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303037333936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02626
02627      
      * EXEC CICS READNEXT
02628 *        SET      (ADDRESS OF PENDING-BUSINESS)
02629 *        DATASET  (ERPNDB-ALT-FILE-ID)
02630 *        RIDFLD   (ERPNDB-ALT-KEY)
02631 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007400' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-ALT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02632
02633      GO TO 4150-EXIT.
02634
02635  4110-END-OF-FILE.
02636      MOVE 'Y'                    TO  PNDB-EOF-SW.
02637
02638  4150-EXIT.
02639      EXIT.
02640  EJECT
02641  4160-PNDB-END-BROWSE.
02642      
      * EXEC CICS ENDBR
02643 *        DATASET  (ERPNDB-ALT-FILE-ID)
02644 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007415' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-ALT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02645  EJECT
02646  4200-PNDB-REWRITE.
02647      MOVE PB-COMPANY-CD          TO  ERPNDB-CO-CD.
02648      MOVE PB-ENTRY-BATCH         TO  ERPNDB-BATCH.
02649      MOVE PB-BATCH-SEQ-NO        TO  ERPNDB-SEQ-NO.
02650      MOVE PB-BATCH-CHG-SEQ-NO    TO  ERPNDB-CHG-SEQ-NO.
02651
02652      PERFORM 4160-PNDB-END-BROWSE
02653
02654      
      * EXEC CICS READ
02655 *        SET      (ADDRESS OF PENDING-BUSINESS)
02656 *        DATASET  (ERPNDB-FILE-ID)
02657 *        RIDFLD   (ERPNDB-PRIME-KEY)
02658 *        UPDATE
02659 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007427' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-PRIME-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02660
02661      MOVE 'B'                    TO  JP-RECORD-TYPE.
02662      MOVE PENDING-BUSINESS       TO  JP-RECORD-AREA.
02663      MOVE ERPNDB-FILE-ID         TO  JP-FILE-ID.
02664
02665      COMPUTE JOURNAL-LENGTH = ERPNDB-LENGTH + 23.
02666
02667      PERFORM 8400-LOG-JOURNAL-RECORD.
02668
02669      IF PI-VOID-BILL
02670          MOVE LOW-VALUES         TO  PB-GA-BILL-DT (WS-GA-LEVEL)
02671      ELSE
02672          MOVE WS-CURRENT-DATE    TO  PB-GA-BILL-DT (WS-GA-LEVEL).
02673
02674      MOVE PI-PROCESSOR-ID        TO  PB-LAST-MAINT-BY.
02675      MOVE EIBTIME                TO  PB-LAST-MAINT-HHMMSS.
02676      MOVE WS-CURRENT-DATE        TO  PB-LAST-MAINT-DT.
02677      MOVE 'C'                    TO  JP-RECORD-TYPE.
02678      MOVE PENDING-BUSINESS       TO  JP-RECORD-AREA.
02679      MOVE ERPNDB-FILE-ID         TO  JP-FILE-ID.
02680
02681      COMPUTE JOURNAL-LENGTH = ERPNDB-LENGTH + 23.
02682
02683      
      * EXEC CICS REWRITE
02684 *        DATASET  (ERPNDB-FILE-ID)
02685 *        FROM     (PENDING-BUSINESS)
02686 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007456' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02687
02688      PERFORM 8400-LOG-JOURNAL-RECORD.
02689
02690      
      * EXEC CICS HANDLE CONDITION
02691 *        NOTFND  (4290-REC-NOT-FND)
02692 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00007463' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303037343633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02693
02694      
      * EXEC CICS STARTBR
02695 *        DATASET  (ERPNDB-ALT-FILE-ID)
02696 *        RIDFLD   (ERPNDB-ALT-KEY)
02697 *        GTEQ
02698 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007467' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDB-ALT-FILE-ID, 
                 ERPNDB-ALT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02699
02700      PERFORM 4100-PNDB-READ-NEXT  THRU  4150-EXIT.
02701
02702      GO TO 4290-EXIT.
02703
02704  4290-REC-NOT-FND.
02705      MOVE 'Y'                    TO  PYAJ-EOF-SW.
02706
02707  4290-EXIT.
02708      EXIT.
02709  EJECT
02710  4300-READ-ACCOUNT-MASTER.
02711
02712      MOVE SPACE                  TO  INVALID-ACCOUNT-SW.
02713      MOVE PI-COMPANY-CD          TO  ERACCT-P-CO-CD.
02714      MOVE PI-CR-CARRIER          TO  ERACCT-P-CARRIER.
02715      MOVE PI-CR-GROUPING         TO  ERACCT-P-GROUPING.
02716      MOVE PI-CR-STATE            TO  ERACCT-P-STATE.
02717      MOVE PI-CR-ACCOUNT          TO  ERACCT-P-ACCOUNT.
02718      MOVE PI-SAV-EXP-DT          TO  ERACCT-P-EXP-DATE.
02719
02720      
      * EXEC CICS HANDLE CONDITION
02721 *        NOTFND  (4370-ACCOUNT-INVALID)
02722 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00007493' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303037343933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02723
02724      
      * EXEC CICS READ
02725 *        DATASET  (ERACCT-FILE-ID)
02726 *        SET      (ADDRESS OF ACCOUNT-MASTER)
02727 *        RIDFLD   (ERACCT-PRIME-KEY)
02728 *        EQUAL
02729 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007497' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-PRIME-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02730
02731      IF AM-REMIT-TO  NOT GREATER THAN  '00'
02732          GO TO 4370-ACCOUNT-INVALID.
02733
02734      IF AM-AGT (AM-REMIT-TO)  =  SPACES
02735          MOVE PI-CR-ACCOUNT      TO  PI-SAV-REMIT-TO
02736      ELSE
02737          MOVE AM-AGT (AM-REMIT-TO)
02738                                  TO  PI-SAV-REMIT-TO.
02739
02740      MOVE AM-EXPIRATION-DT       TO  ERACCT-P-EXP-DATE.
02741      MOVE AM-CARRIER             TO  PI-CR-CARRIER.
02742      MOVE AM-GROUPING            TO  PI-CR-GROUPING.
02743      MOVE AM-STATE               TO  PI-CR-STATE.
02744      MOVE AM-AGT (AM-REMIT-TO)   TO  PI-SAV-FIN-RESP
02745                                      PI-COMP-FIN-RESP.
02746
02747      IF PI-ZERO-CARRIER
02748        OR  PI-ZERO-CAR-GROUP
02749          MOVE ZEROS              TO  PI-COMP-CARRIER
02750      ELSE
02751          MOVE AM-CARRIER         TO  PI-COMP-CARRIER.
02752
02753      IF PI-ZERO-GROUPING
02754         OR  PI-ZERO-CAR-GROUP
02755          MOVE ZEROS              TO  PI-COMP-GROUPING
02756      ELSE
02757          MOVE AM-GROUPING        TO  PI-COMP-GROUPING.
02758
02759      IF CARR-GROUP-ST-ACCNT-CNTL
02760          MOVE SPACE              TO  NEW-ACCOUNT-SW.
02761
02762      MOVE +0                     TO  ACCOM-SUB.
02763
02764  4320-FIND-ACCT-RESPONSIBILITY.
02765      ADD +1                      TO  ACCOM-SUB.
02766
02767      IF ACCOM-SUB  GREATER THAN  +10
02768          MOVE  AM-ACCOUNT        TO  GA-ACCT
02769          GO TO 4330-FIND-GA-COMMISSION.
02770
02771      IF AM-COM-TYP (ACCOM-SUB) = 'C' OR 'D' OR 'F' OR 'S'
02772          NEXT SENTENCE
02773      ELSE
02774          GO TO 4320-FIND-ACCT-RESPONSIBILITY.
02775
02776      IF AM-AGT (ACCOM-SUB)  NOT =  PI-SAV-ACCT-AGT
02777          MOVE 'Y'                TO  NEW-ACCOUNT-SW.
02778
02779      MOVE AM-AGT (ACCOM-SUB)     TO  PI-SAV-ACCT-AGT.
02780      MOVE AM-NAME                TO  PI-SAV-ACCT-NAME.
02781
02782  4330-FIND-GA-COMMISSION.
02783      MOVE ZEROS                  TO  WS-GA-LEVEL
02784                                      WS-GA-LF-COM
02785                                      WS-GA-AH-COM.
02786      MOVE +0                     TO  ACCOM-SUB.
02787
02788  4340-SEARCH-FOR-GA-COMMISSION.
02789      ADD +1                      TO  ACCOM-SUB.
02790
02791      IF ACCOM-SUB  GREATER THAN  +10
02792          GO TO 4370-ACCOUNT-INVALID.
02793
02794      IF AM-COM-TYP (ACCOM-SUB)  = 'O' OR 'P' OR 'G' OR 'B'
02795          NEXT SENTENCE
02796      ELSE
02797          GO TO 4340-SEARCH-FOR-GA-COMMISSION.
02798
02799      IF AM-AGT (ACCOM-SUB)  NOT =  PI-SAV-AGENT
02800          GO TO 4340-SEARCH-FOR-GA-COMMISSION.
02801
02802      MOVE ACCOM-SUB              TO  WS-GA-LEVEL.
02803
02804      GO TO 4390-EXIT.
02805  EJECT
02806  4370-ACCOUNT-INVALID.
02807      MOVE 'Y'                    TO  INVALID-ACCOUNT-SW.
02808      MOVE ER-2210                TO  EMI-ERROR.
02809
02810      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02811
02812      GO TO 4390-EXIT.
02813
02814  4390-EXIT.
02815      EXIT.
02816  EJECT
02817  4400-FIND-COMMISSION.
02818 ******************************************************************
02819 *    IF THE AGENT'S COMMISSION IS NUMERIC FOR A&H AND LIFE       *
02820 *    IN THE ACCOUNT MASTER THAT IS THE COMMISSION.  IF THE       *
02821 *    COMMISSION IN THE A&H AND LIFE CONTAINS A TABLE CODE        *
02822 *    READ THE COMMISSION-TABLE TO GET THE COMMISSION RATE.       *
02823 *    FOR CLIENT "DMD" THE ABOVE COMMISSION MAY BE OVERWRITTEN    *
02824 *    IF AN ACCOUNT RESIDENT STATE COMMISSION RECORD EXISTS.      *
02825 ******************************************************************
02826
02827      MOVE PI-COMPANY-CD          TO  ERCTBL-COMPANY-CD.
02828
02829  4405-FIND-AH-COMMISSION.
02830 * SET UP COMMISSION TABLE KEY BEFORE DETERMINING COMMISSION
02831      IF PB-ISSUE
02832          MOVE PB-I-AH-BENEFIT-CD  TO  ERCTBL-BEN-CODE
02833      ELSE
02834          MOVE PB-CI-AH-BENEFIT-CD TO  ERCTBL-BEN-CODE.
02835
02836      MOVE 'A'                     TO  ERCTBL-BEN-TYPE.
02837
02838      MOVE 'AH'                   TO WS-COMM-SW.
02839
02840      IF AM-A-COM (WS-GA-LEVEL)  NUMERIC
02841          MOVE AM-A-COM (WS-GA-LEVEL)
02842                                  TO  WS-COMMISSION
02843          PERFORM 6650-GET-RES-STATE-COMM THRU 6700-COMM-EXIT
02844          MOVE WS-COMMISSION      TO  WS-GA-AH-COM
02845          GO TO 4410-FIND-LIFE-COMMISSION.
02846
02847      IF ERCTBL-BEN-CODE  =  ZEROS
02848          MOVE ZEROS              TO  WS-GA-LF-COM
02849          GO TO 4410-FIND-LIFE-COMMISSION.
02850
02851      MOVE ZEROS                  TO WS-SUB1.
02852      PERFORM 6650-GET-RES-STATE-COMM THRU 6700-COMM-EXIT.
02853      IF WS-SUB1 IS GREATER THAN ZEROS
02854          GO TO 4410-FIND-LIFE-COMMISSION.
CIDMOD
CIDMOD     MOVE 'A'                     TO  ERCTBL-BEN-TYPE.
02855
02856      MOVE AM-A-COMA (WS-GA-LEVEL) TO ERCTBL-TABLE.
02857      PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT.
02858
02859      IF ERCTBL-NOT-FOUND
02860          MOVE 'AA'               TO  ERCTBL-BEN-CODE
02861          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT
02862          IF ERCTBL-NOT-FOUND
02863              MOVE -1             TO  AAGENTL
02864              MOVE ER-2916        TO  EMI-ERROR
02865              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02866              PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT
02867              GO TO 8200-SEND-DATAONLY.
02868
02869      PERFORM 6680-AH-GET-COMP-RATE.
02870
02871  4410-FIND-LIFE-COMMISSION.
02872      IF PB-CANCELLATION
02873          MOVE PB-CI-LF-BENEFIT-CD  TO CLBENF-CD
02874      ELSE
02875          MOVE PB-I-LF-BENEFIT-CD   TO CLBENF-CD.
02876
02877      MOVE 'LF'                   TO WS-COMM-SW.
02878
02879      IF CLBENF-CD  =  ZEROS  OR  SPACES
02880          MOVE ZEROS              TO  WS-GA-LF-COM
02881          GO TO 4490-EXIT.
02882
02883      IF PB-ISSUE
02884          MOVE PB-I-LIFE-INDICATOR TO LIFE-INDICATOR
02885          GO TO 4420-CONT-FIND-LIFE-COMMISSION.
02886
02887      PERFORM 6800-READ-BENEFIT-MASTER  THRU  6890-EXIT.
02888
02889      IF BENEFIT-MASTER-FOUND
02890          NEXT SENTENCE
02891      ELSE
02892          MOVE -1                 TO  AAGENTL
02893          MOVE ER-2917            TO  EMI-ERROR
02894          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02895          PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT
02896          GO TO 8200-SEND-DATAONLY.
02897
02898  4420-CONT-FIND-LIFE-COMMISSION.
02899 * SET UP COMMISSION TABLE KEY BEFORE DETERMINING COMMISSION
02900
02901      IF PB-ISSUE
02902          MOVE PB-I-LF-BENEFIT-CD  TO  ERCTBL-BEN-CODE
02903      ELSE
02904          MOVE PB-CI-LF-BENEFIT-CD TO  ERCTBL-BEN-CODE.
02905
02906      MOVE 'L'                     TO  ERCTBL-BEN-TYPE.
02907
02908      IF JOINT-LIFE
02909          NEXT SENTENCE
02910      ELSE
02911          GO TO 4430-SINGLE-LIFE-COMMISSION.
02912
02913      IF AM-J-COM (WS-GA-LEVEL)  NUMERIC
02914          MOVE AM-J-COM (WS-GA-LEVEL)
02915                                  TO  WS-COMMISSION
02916          PERFORM 6650-GET-RES-STATE-COMM THRU 6700-COMM-EXIT
02917          MOVE WS-COMMISSION      TO  WS-GA-LF-COM
02918          GO TO 4490-EXIT.
02919
02920      MOVE ZEROS                  TO WS-SUB1.
02921      PERFORM 6650-GET-RES-STATE-COMM THRU 6700-COMM-EXIT.
02922      IF WS-SUB1 IS GREATER THAN ZEROS
02923          GO TO 4490-EXIT.
02924
02925      MOVE AM-J-COMA (WS-GA-LEVEL) TO ERCTBL-TABLE.
02926
02927      PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT.
02928
02929      IF ERCTBL-NOT-FOUND
02930          MOVE 'AA'               TO  ERCTBL-BEN-CODE
02931          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT
02932          IF ERCTBL-NOT-FOUND
02933              MOVE -1             TO  AAGENTL
02934              MOVE ER-2916        TO  EMI-ERROR
02935              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02936              PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT
02937              GO TO 8200-SEND-DATAONLY.
02938
02939      PERFORM 6680-LF-GET-COMP-RATE.
02940      GO TO 4490-EXIT.
02941
02942  4430-SINGLE-LIFE-COMMISSION.
02943 * SET UP COMMISSION TABLE KEY BEFORE DETERMINING COMMISSION
02944
02945      IF PB-ISSUE
02946          MOVE PB-I-LF-BENEFIT-CD  TO  ERCTBL-BEN-CODE
02947      ELSE
02948          MOVE PB-CI-LF-BENEFIT-CD TO  ERCTBL-BEN-CODE.
02949
02950      MOVE 'L'                     TO  ERCTBL-BEN-TYPE.
02951 *
02952      IF AM-L-COM (WS-GA-LEVEL)  NUMERIC
02953          MOVE AM-L-COM (WS-GA-LEVEL)
02954                                  TO  WS-COMMISSION
02955          PERFORM 6650-GET-RES-STATE-COMM THRU 6700-COMM-EXIT
02956          MOVE WS-COMMISSION      TO  WS-GA-LF-COM
02957          GO TO 4490-EXIT.
02958
02959      MOVE ZEROS                  TO WS-SUB1.
02960      PERFORM 6650-GET-RES-STATE-COMM THRU 6700-COMM-EXIT.
02961      IF WS-SUB1 IS GREATER THAN ZEROS
02962          GO TO 4490-EXIT.
02963
02964      MOVE ZEROS                  TO WS-SUB1.
02965      MOVE AM-L-COMA (WS-GA-LEVEL) TO ERCTBL-TABLE
02966
02967      PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT.
02968
02969      IF ERCTBL-NOT-FOUND
02970          MOVE 'AA'               TO  ERCTBL-BEN-CODE
02971          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT
02972          IF ERCTBL-NOT-FOUND
02973              MOVE -1             TO  AAGENTL
02974              MOVE ER-2916        TO  EMI-ERROR
02975              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02976              PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT
02977              GO TO 8200-SEND-DATAONLY.
02978
02979      PERFORM 6680-LF-GET-COMP-RATE.
02980      GO TO 4490-EXIT.
02981
02982  4490-EXIT.
02983      EXIT.
02984  EJECT
02985  4500-PYAJ-START-BROWSE.
02986      MOVE LOW-VALUES             TO  ERPYAJ-KEY.
02987      MOVE PI-COMPANY-CD          TO  ERPYAJ-COMP-CD.
02988
02989      IF GENERAL-AGENT
02990          MOVE PI-SAV-CARR        TO  ERPYAJ-CARRIER
02991          MOVE PI-SAV-GROUP       TO  ERPYAJ-GROUPING
02992          MOVE PI-SAV-AGENT       TO  ERPYAJ-FIN-RESP
02993          MOVE LOW-VALUES         TO  ERPYAJ-ACCOUNT
02994      ELSE
02995          MOVE PI-COMP-CARRIER    TO  ERPYAJ-CARRIER
02996          MOVE PI-COMP-GROUPING   TO  ERPYAJ-GROUPING
02997          MOVE PI-COMP-FIN-RESP   TO  ERPYAJ-FIN-RESP
02998          MOVE PI-CR-ACCOUNT      TO  ERPYAJ-ACCOUNT.
02999
03000      MOVE ERPYAJ-KEY             TO  ERPYAJ-BROWSE-COMP-KEY.
03001
03002      
      * EXEC CICS HANDLE CONDITION
03003 *        NOTFND  (4510-REC-NOT-FND)
03004 *    END-EXEC.
      *    MOVE '"$I                   ! * #00007777' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303037373737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03005
03006      
      * EXEC CICS STARTBR
03007 *        DATASET  (ERPYAJ-FILE-ID)
03008 *        RIDFLD   (ERPYAJ-KEY)
03009 *        GTEQ
03010 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007781' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03011
03012      GO TO 4590-EXIT.
03013
03014  4510-REC-NOT-FND.
03015      MOVE 'Y'                    TO  PYAJ-EOF-SW.
03016
03017  4590-EXIT.
03018      EXIT.
03019  EJECT
03020  4600-PYAJ-READ-NEXT.
03021      
      * EXEC CICS HANDLE CONDITION
03022 *        ENDFILE  (4610-END-OF-FILE)
03023 *    END-EXEC.
      *    MOVE '"$''                   ! + #00007796' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303037373936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03024
03025      
      * EXEC CICS READNEXT
03026 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
03027 *        DATASET  (ERPYAJ-FILE-ID)
03028 *        RIDFLD   (ERPYAJ-KEY)
03029 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007800' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03030
03031      GO TO 4690-EXIT.
03032
03033  4610-END-OF-FILE.
03034      MOVE 'Y'                    TO  PYAJ-EOF-SW.
03035
03036  4690-EXIT.
03037      EXIT.
03038  EJECT
03039  4700-PROCESS-COMM-EXCEPTION.
03040      PERFORM 6600-READ-COMMISSION-EXCEPTION  THRU  6620-EXIT.
03041
03042      IF ERCOMM-NOT-FOUND
03043          MOVE -1                 TO  AAGENTL
03044          MOVE ER-2915            TO  EMI-ERROR
03045          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03046          PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT
03047          GO TO 8200-SEND-DATAONLY.
03048
03049      MOVE WS-GA-LEVEL            TO  CE-SUB.
03050      MOVE PI-COMPANY-CD          TO  ERCTBL-COMPANY-CD.
03051
03052      IF CE-LF-COMP (CE-SUB)  NUMERIC
03053          IF CE-AH-COMP (CE-SUB)  NUMERIC
03054              MOVE CE-LF-COMP (CE-SUB)  TO  WS-GA-LF-COM
03055              MOVE CE-AH-COMP (CE-SUB)  TO  WS-GA-AH-COM
03056              GO TO 4790-EXIT.
03057
03058  4715-FIND-AH-COMMISSION.
03059      IF CE-AH-COMP (CE-SUB)  NUMERIC
03060          MOVE CE-AH-COMP (CE-SUB)  TO  WS-GA-AH-COM
03061          GO TO 4750-FIND-LIFE-COMMISSION.
03062
03063      MOVE CE-AH-COMPT (CE-SUB)   TO  ERCTBL-TABLE.
03064
03065      IF PB-ISSUE
03066          MOVE PB-I-AH-BENEFIT-CD   TO  ERCTBL-BEN-CODE
03067      ELSE
03068          MOVE PB-CI-AH-BENEFIT-CD  TO  ERCTBL-BEN-CODE.
03069
03070      IF ERCTBL-BEN-CODE  =  ZEROS
03071          MOVE ZEROS              TO  WS-GA-AH-COM
03072          GO TO 4750-FIND-LIFE-COMMISSION.
03073
03074      MOVE 'A'                    TO  ERCTBL-BEN-TYPE.
03075
03076      PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT.
03077
03078      IF ERCTBL-NOT-FOUND
03079          MOVE 'AA'               TO  ERCTBL-BEN-CODE
03080          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT
03081          IF ERCTBL-NOT-FOUND
03082              MOVE -1             TO  AAGENTL
03083              MOVE ER-2916        TO  EMI-ERROR
03084              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03085              PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT
03086              GO TO 8200-SEND-DATAONLY.
03087
03088      IF PB-ISSUE
03089          COMPUTE WS-COMM-CK-AMT =
03090              PB-I-AH-BENEFIT-AMT * PB-I-AH-TERM
03091          MOVE PB-I-AGE           TO  WS-COMM-AGE
03092          MOVE PB-I-AH-TERM       TO  WS-COMM-TERM
03093      ELSE
03094          COMPUTE WS-COMM-CK-AMT =
03095              PB-CI-AH-BENEFIT-AMT * PB-CI-AH-TERM
03096          MOVE PB-CI-INSURED-AGE  TO  WS-COMM-AGE
03097          MOVE PB-CI-AH-TERM      TO  WS-COMM-TERM.
03098
03099      PERFORM 4900-GET-COMP-RATE  THRU  4990-EXIT.
03100
03101      MOVE WS-WK-RATE             TO  WS-GA-AH-COM.
03102
03103  4750-FIND-LIFE-COMMISSION.
03104      IF PB-ISSUE
03105          MOVE PB-I-LF-BENEFIT-CD   TO  CLBENF-CD
03106      ELSE
03107          MOVE PB-CI-LF-BENEFIT-CD  TO  CLBENF-CD.
03108
03109      IF CLBENF-CD  =  ZEROS  OR  SPACES
03110          MOVE ZEROS              TO  WS-GA-LF-COM
03111          GO TO 4790-EXIT.
03112
03113      IF PB-ISSUE
03114          MOVE PB-I-LIFE-INDICATOR  TO  LIFE-INDICATOR
03115          GO TO 4760-CONT-FIND-LIFE-COMMISSION.
03116
03117      PERFORM 6800-READ-BENEFIT-MASTER  THRU  6890-EXIT.
03118
03119      IF BENEFIT-MASTER-FOUND
03120          NEXT SENTENCE
03121      ELSE
03122          MOVE -1                 TO  AAGENTL
03123          MOVE ER-2917            TO  EMI-ERROR
03124          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03125          PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT
03126          GO TO 8200-SEND-DATAONLY.
03127
03128  4760-CONT-FIND-LIFE-COMMISSION.
03129      IF JOINT-LIFE NEXT SENTENCE
03130      ELSE
03131          GO TO 4770-SINGLE-LIFE-COMMISSION.
03132
03133      IF CE-LF-COMP (CE-SUB)  NUMERIC
03134          MOVE CE-LF-COMP (CE-SUB)  TO  WS-GA-LF-COM
03135          GO TO 4790-EXIT.
03136
03137      MOVE CE-LF-COMPT (CE-SUB)   TO  ERCTBL-TABLE.
03138
03139      IF PB-ISSUE
03140          MOVE PB-I-LF-BENEFIT-CD   TO  ERCTBL-BEN-CODE
03141      ELSE
03142          MOVE PB-CI-LF-BENEFIT-CD  TO  ERCTBL-BEN-CODE.
03143
03144      MOVE 'J'                    TO  ERCTBL-BEN-TYPE.
03145
03146      PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT.
03147
03148      IF ERCTBL-NOT-FOUND
03149          MOVE 'AA'               TO  ERCTBL-BEN-CODE
03150          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT
03151          IF ERCTBL-NOT-FOUND
03152              MOVE -1             TO  AAGENTL
03153              MOVE ER-2916        TO  EMI-ERROR
03154              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03155              PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT
03156              GO TO 8200-SEND-DATAONLY.
03157
03158      IF PB-ISSUE
03159          MOVE PB-I-LF-BENEFIT-AMT   TO  WS-COMM-CK-AMT
03160          MOVE PB-I-AGE              TO  WS-COMM-AGE
03161          MOVE PB-I-LF-TERM          TO  WS-COMM-TERM
03162      ELSE
03163          MOVE PB-CI-INSURED-AGE     TO  WS-COMM-AGE
03164          MOVE PB-CI-LF-TERM         TO  WS-COMM-TERM
03165          MOVE PB-CI-LF-BENEFIT-AMT  TO  WS-COMM-CK-AMT.
03166
03167      PERFORM 4900-GET-COMP-RATE  THRU  4990-EXIT.
03168
03169      MOVE WS-WK-RATE             TO  WS-GA-LF-COM.
03170
03171      GO TO 4790-EXIT.
03172
03173  4770-SINGLE-LIFE-COMMISSION.
03174      IF CE-LF-COMP (CE-SUB)  NUMERIC
03175          MOVE CE-LF-COMP (CE-SUB)  TO  WS-GA-LF-COM
03176          GO TO 4790-EXIT.
03177
03178      MOVE CE-LF-COMPT (CE-SUB)   TO  ERCTBL-TABLE.
03179
03180      IF PB-ISSUE
03181          MOVE PB-I-LF-BENEFIT-CD   TO  ERCTBL-BEN-CODE
03182      ELSE
03183          MOVE PB-CI-LF-BENEFIT-CD  TO  ERCTBL-BEN-CODE.
03184
03185      MOVE 'L'                    TO  ERCTBL-BEN-TYPE.
03186
03187      PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT.
03188
03189      IF ERCTBL-NOT-FOUND
03190          MOVE 'AA'               TO  ERCTBL-BEN-CODE
03191          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT
03192          IF ERCTBL-NOT-FOUND
03193              MOVE -1             TO  AAGENTL
03194              MOVE ER-2916        TO  EMI-ERROR
03195              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03196              PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT
03197              GO TO 8200-SEND-DATAONLY.
03198
03199      IF PB-ISSUE
03200          MOVE PB-I-LF-BENEFIT-AMT   TO  WS-COMM-CK-AMT
03201          MOVE PB-I-AGE              TO  WS-COMM-AGE
03202          MOVE PB-I-LF-TERM          TO  WS-COMM-TERM
03203      ELSE
03204          MOVE PB-CI-INSURED-AGE     TO  WS-COMM-AGE
03205          MOVE PB-CI-LF-TERM         TO  WS-COMM-TERM
03206          MOVE PB-CI-LF-BENEFIT-AMT  TO  WS-COMM-CK-AMT.
03207
03208      PERFORM 4900-GET-COMP-RATE  THRU  4990-EXIT.
03209
03210      MOVE WS-WK-RATE             TO  WS-GA-LF-COM.
03211
03212  4790-EXIT.
03213      EXIT.
03214  EJECT
03215  4800-PYAJ-END-BROWSE.
03216      
      * EXEC CICS ENDBR
03217 *        DATASET  (ERPYAJ-FILE-ID)
03218 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007991' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03219  EJECT
03220  4900-GET-COMP-RATE.
03221      MOVE +1                     TO  COMP-SUB.
03222
03223      IF WS-COMM-CK-AMT  GREATER THAN  CT-TBF (1)
03224          ADD +9                  TO  COMP-SUB
03225          IF WS-COMM-CK-AMT  GREATER THAN  CT-TBF (2)
03226              ADD +9              TO  COMP-SUB
03227              IF WS-COMM-CK-AMT  GREATER THAN  CT-TBF (3)
03228                  MOVE -1         TO  AAGENTL
03229                  MOVE ER-2918    TO  EMI-ERROR
03230                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03231                  PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT
03232                  GO TO 8200-SEND-DATAONLY.
03233
03234      IF WS-COMM-AGE  GREATER THAN  CT-AGE (1)
03235          ADD +3                  TO  COMP-SUB
03236          IF WS-COMM-AGE  GREATER THAN  CT-AGE (2)
03237              ADD +1              TO  COMP-SUB
03238              IF WS-COMM-AGE  GREATER THAN  CT-AGE (3)
03239                  MOVE -1         TO  AAGENTL
03240                  MOVE ER-2918    TO  EMI-ERROR
03241                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03242                  PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT
03243                  GO TO 8200-SEND-DATAONLY.
03244
03245      IF WS-COMM-TERM  GREATER  CT-TRM (1)
03246          ADD +1                  TO  COMP-SUB
03247          IF WS-COMM-TERM  GREATER THAN  CT-TRM (2)
03248              ADD +1              TO  COMP-SUB
03249              IF WS-COMM-TERM  GREATER THAN  CT-TRM (3)
03250                  MOVE -1         TO  AAGENTL
03251                  MOVE ER-2918    TO  EMI-ERROR
03252                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03253                  PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT
03254                  GO TO 8200-SEND-DATAONLY.
03255
03256      IF CT-RT (COMP-SUB)  NOT  NUMERIC
03257          MOVE CT-RT-R (COMP-SUB)  TO  ERCTBL-TABLE
03258          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT
03259          GO TO 4900-GET-COMP-RATE.
03260
03261      MOVE CT-RT (COMP-SUB)       TO  WS-WK-RATE.
03262
03263  4990-EXIT.
03264      EXIT.
03265  EJECT
03266  5000-FORMAT-SCREEN.
03267      MOVE LOW-VALUES             TO  EL642AO.
03268
03269      IF PI-SAV-AGENT  GREATER THAN  SPACES
03270          MOVE PI-SAV-AGENT       TO  AAGENTO
03271          MOVE PI-SAV-CARR        TO  ACARIERO
03272          MOVE PI-SAV-GROUP       TO  AGROUPO.
03273
03274      MOVE PI-BILL-TYPE           TO  ABILTYPI.
03275      MOVE AL-UANON               TO  AAGENTA
03276                                      ACARIERA
03277                                      AGROUPA
03278                                      ABILTYPA.
03279
03280      IF PI-BILLING-ACCOUNTS (1)  NOT =  SPACES
03281          MOVE PI-BILLING-ACCOUNTS (1)
03282                                  TO  AACCT1I
03283          MOVE AL-UANON           TO  AACCT1A.
03284
03285      IF PI-BILLING-ACCOUNTS (2)  NOT =  SPACES
03286          MOVE PI-BILLING-ACCOUNTS (2)
03287                                  TO  AACCT2I
03288          MOVE AL-UANON           TO  AACCT2A.
03289
03290      IF PI-BILLING-ACCOUNTS (3)  NOT =  SPACES
03291          MOVE PI-BILLING-ACCOUNTS (3)
03292                                  TO  AACCT3I
03293          MOVE AL-UANON           TO  AACCT3A.
03294
03295  5005-FORMAT-STATS.
03296
03297      IF PI-CR-FIN-RESP = SPACES
03298          GO TO 5090-EXIT.
03299
03300      MOVE PI-BAL-FRWD            TO  ABEGBALO.
03301      MOVE PI-UNPAID-NET-PREM     TO  AUPDPRMO.
03302      MOVE PI-COMP-UNPAID-PREM    TO  AUPDCOMO.
03303      MOVE PI-ISSUES-BILLED       TO  ABILL1O.
03304      MOVE PI-ISSUES-INER         TO  AINER1O.
03305      MOVE PI-ISSUES-PREV         TO  APRVBL1O.
03306      MOVE PI-CANCELS-BILLED      TO  ABILL2O.
03307      MOVE PI-CANCELS-INER        TO  AINER2O.
03308      MOVE PI-CANCELS-PREV        TO  APRVBL2O.
03309
03310
03311      IF PI-UNKNOWN  GREATER THAN  ZEROS
03312          MOVE ER-2912            TO  EMI-ERROR
03313          MOVE PI-UNKNOWN         TO  AUNKNERO
03314          MOVE AL-SABOF           TO  AUNKNERA
03315          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03316
03317      MOVE PI-PREMIUM             TO  APREMUMO.
03318      MOVE PI-REMITTED            TO  AREMITO.
03319      MOVE PI-TOT-ISS-COMP        TO  ACOMPISO.
03320      MOVE PI-TOT-CAN-COMP        TO  ACOMCANO.
03321      MOVE PI-ADJUSTMNTS          TO  AADJUSTO.
03322      MOVE PI-DISBURSED           TO  ADISBURO.
03323      MOVE ZERO                   TO  PI-END-BAL.
03324
03325      IF PI-SAV-AGENT = PI-SAV-REMIT-TO
03326          COMPUTE PI-DUE-FOR-ACCT =
03327              (PI-ACCT-BEG-BAL + PI-ACCT-NET-PREM) -
03328               (PI-ACCT-COMP + PI-ACCT-PAY-ADJS).
03329
03330      COMPUTE PI-END-BAL = PI-BAL-FRWD - PI-TOT-ISS-COMP
03331                         + PI-TOT-CAN-COMP + PI-ADJUSTMNTS
03332                         - PI-REMITTED + PI-DISBURSED.
03333
03334      MOVE PI-END-BAL             TO  ANETDUEO.
03335
03336      IF PI-END-BAL  NEGATIVE
03337          MOVE OWED-TO-AGENT      TO  AENDHDGO
03338      ELSE
03339          IF PI-END-BAL  GREATER THAN  ZERO
03340              MOVE AGENT-OWES     TO  AENDHDGO
03341          ELSE
03342              MOVE 'BALANCE='     TO  AENDHDGO.
03343
03344      IF PI-SAV-AGENT = PI-SAV-REMIT-TO
03345          IF PI-DUE-FOR-ACCT  NOT =  ZERO
03346              MOVE PI-DUE-FOR-ACCT
03347                                  TO  AACTDUEO
03348              IF PI-DUE-FOR-ACCT  NEGATIVE
03349                  MOVE 'DUE GA ON BEHALF OF ACCOUNTS'
03350                                  TO  AACTHDGO
03351              ELSE
03352                  MOVE 'GA OWES ON BEHALF OF ACCOUNTS'
03353                                  TO  AACTHDGO.
03354
03355  5080-CONT.
03356      IF RETURNED-FROM  NOT =  SPACES
03357          IF RETURNED-FROM  NOT =  XCTL-6401
03358              MOVE ER-2421        TO  EMI-ERROR
03359              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03360              MOVE ZEROS          TO  EMI-ERROR.
03361
03362  5090-EXIT.
03363      EXIT.
03364  EJECT
03365  6000-READ-COMP-MASTER.
03366      
      * EXEC CICS HANDLE CONDITION
03367 *        NOTFND  (6070-NO-COMP-MSTR)
03368 *    END-EXEC.
      *    MOVE '"$I                   ! , #00008141' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303038313431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03369
03370      MOVE PI-COMPANY-CD          TO  ERCOMP-COMP-CD.
03371
03372      IF GENERAL-AGENT
03373          MOVE PI-SAV-CARR        TO  ERCOMP-CARRIER
03374          MOVE PI-SAV-GROUP       TO  ERCOMP-GROUPING
03375          MOVE PI-SAV-AGENT       TO  ERCOMP-FIN-RESP
03376      ELSE
03377          MOVE PI-COMP-CARRIER    TO  ERCOMP-CARRIER
03378          MOVE PI-COMP-GROUPING   TO  ERCOMP-GROUPING
03379          MOVE PI-COMP-FIN-RESP   TO  ERCOMP-FIN-RESP.
03380
03381      MOVE ERCOMP-FIN-RESP        TO  PI-CR-FIN-RESP.
03382
03383      IF EIBAID  =  DFHPF5  OR  GENERAL-AGENT
03384          MOVE LOW-VALUES         TO  ERCOMP-ACCT
03385          MOVE 'G'                TO  ERCOMP-RECORD-TYPE
03386      ELSE
03387          MOVE PI-SAV-ACCT-AGT    TO  ERCOMP-ACCT
03388          MOVE 'A'                TO  ERCOMP-RECORD-TYPE.
03389
03390      IF ERCOMP-KEY  =  ERCOMP-PREV-KEY
03391          GO TO 6090-EXIT.
03392
03393      MOVE ERCOMP-KEY             TO  ERCOMP-PREV-KEY.
03394
03395      
      * EXEC CICS READ
03396 *        DATASET  (ERCOMP-FILE-ID)
03397 *        SET      (ADDRESS OF COMPENSATION-MASTER)
03398 *        RIDFLD   (ERCOMP-KEY)
03399 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008170' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03400
03401      IF PI-MAP-NAME  =  EL642B
03402          GO TO 6090-EXIT.
03403
03404      IF GENERAL-AGENT
03405          MOVE CO-CURRENT-END-BAL TO  PI-BAL-FRWD
03406      ELSE
03407          IF PI-SAV-AGENT  =  PI-SAV-REMIT-TO
03408              MOVE CO-CURRENT-END-BAL
03409                                  TO  WS-ACCT-BEG-BAL
03410              GO TO 6090-EXIT
03411          ELSE
03412              MOVE ZEROS          TO  WS-ACCT-BEG-BAL
03413              GO TO 6090-EXIT.
03414
03415      IF PI-UPDATE-FILES
03416        OR  (PI-PREVIEW  AND  APRODSWI  =  'Y')
03417          NEXT SENTENCE
03418      ELSE
03419          GO TO 6090-EXIT.
03420
03421  6002-FORMAT-ADDRESS-LOOP.
03422      MOVE CO-ACCT-NAME           TO  PI-AGENT-NAME.
03423      MOVE CO-MAIL-NAME           TO  WS-AGENT-LINES (1).
03424      MOVE CO-ACCT-NAME           TO  WS-AGENT-LINES (2).
03425      MOVE CO-ADDR-1              TO  WS-AGENT-LINES (3).
03426      MOVE CO-ADDR-2              TO  WS-AGENT-LINES (4).
03427      MOVE CO-ADDR-3              TO  WS-AGENT-LINES (5).
03428      MOVE CO-ZIP                 TO  WS-AGENT-LINES (6).
03429
03430  6060-FORMAT-ADDRESS-LOOP.
03431      IF WS-AGENT-ADDR-AREA  =  SPACES
03432          GO TO 6090-EXIT.
03433
03434      IF WS-AGENT-LINES (1)  =  SPACES
03435          MOVE WS-AGENT-LINES (2)  TO  WS-AGENT-LINES (1)
03436          MOVE WS-AGENT-LINES (3)  TO  WS-AGENT-LINES (2)
03437          MOVE WS-AGENT-LINES (4)  TO  WS-AGENT-LINES (3)
03438          MOVE WS-AGENT-LINES (5)  TO  WS-AGENT-LINES (4)
03439          MOVE WS-AGENT-LINES (6)  TO  WS-AGENT-LINES (5)
03440          MOVE SPACES              TO  WS-AGENT-LINES (6)
03441          GO TO 6060-FORMAT-ADDRESS-LOOP.
03442
03443      IF WS-AGENT-LINES (2)  =  SPACES
03444        AND  WS-AGENT-LINES (3)  =  SPACES
03445        AND  WS-AGENT-LINES (4)  =  SPACES
03446        AND  WS-AGENT-LINES (5)  =  SPACES
03447        AND  WS-AGENT-LINES (6)  =  SPACES
03448          GO TO 6065-MOVE-ZIP.
03449
03450      IF WS-AGENT-LINES (2)  =  SPACES
03451          MOVE WS-AGENT-LINES (3)  TO  WS-AGENT-LINES (2)
03452          MOVE WS-AGENT-LINES (4)  TO  WS-AGENT-LINES (3)
03453          MOVE WS-AGENT-LINES (5)  TO  WS-AGENT-LINES (4)
03454          MOVE WS-AGENT-LINES (6)  TO  WS-AGENT-LINES (5)
03455          MOVE SPACES              TO  WS-AGENT-LINES (6)
03456          GO TO 6060-FORMAT-ADDRESS-LOOP.
03457
03458      IF WS-AGENT-LINES (3)  =  SPACES
03459        AND  WS-AGENT-LINES (4)  =  SPACES
03460        AND  WS-AGENT-LINES (5)  =  SPACES
03461        AND  WS-AGENT-LINES (6)  =  SPACES
03462          GO TO 6065-MOVE-ZIP.
03463
03464      IF WS-AGENT-LINES (3)  =  SPACES
03465          MOVE WS-AGENT-LINES (4)  TO  WS-AGENT-LINES (3)
03466          MOVE WS-AGENT-LINES (5)  TO  WS-AGENT-LINES (4)
03467          MOVE WS-AGENT-LINES (6)  TO  WS-AGENT-LINES (5)
03468          MOVE SPACES              TO  WS-AGENT-LINES (6)
03469          GO TO 6060-FORMAT-ADDRESS-LOOP.
03470
03471      IF WS-AGENT-LINES (4)  =  SPACES
03472        AND  WS-AGENT-LINES (5)  =  SPACES
03473        AND  WS-AGENT-LINES (6)  =  SPACES
03474          GO TO 6065-MOVE-ZIP.
03475
03476      IF WS-AGENT-LINES (4)  =  SPACES
03477          MOVE WS-AGENT-LINES (5)  TO  WS-AGENT-LINES (4)
03478          MOVE WS-AGENT-LINES (6)  TO  WS-AGENT-LINES (5)
03479          MOVE SPACES              TO  WS-AGENT-LINES (6)
03480          GO TO 6060-FORMAT-ADDRESS-LOOP.
03481
03482      IF WS-AGENT-LINES (5)  =  SPACES
03483          MOVE WS-AGENT-LINES (6)  TO  WS-AGENT-LINES (5)
03484          MOVE SPACES              TO  WS-AGENT-LINES (6).
03485
03486  6065-MOVE-ZIP.
03487
03488      IF WS-AGENT-LINES (6) NOT =  SPACES
03489          IF CO-CANADIAN-POST-CODE
03490                  OR
03491             WS-AGENT-1ST-ZIP (6)  NOT =  ZEROS
03492              MOVE WS-AGENT-ZIP (6)
03493                                  TO  WS-A-LAST-ZIP (5)
03494              MOVE SPACES         TO  WS-AGENT-LINES (6)
03495          ELSE
03496              MOVE WS-AGENT-2ND-ZIP (5)
03497                                  TO  WS-A-LAST-2ND-ZIP (4)
03498              MOVE SPACES         TO  WS-AGENT-LINES (5)
03499      ELSE
03500          IF WS-AGENT-LINES (5) NOT =  SPACES
03501              IF CO-CANADIAN-POST-CODE
03502                      OR
03503                 WS-AGENT-1ST-ZIP (5)  NOT =  ZEROS
03504                  MOVE WS-AGENT-ZIP (5)
03505                                  TO  WS-A-LAST-ZIP (5)
03506                  MOVE SPACES     TO  WS-AGENT-ZIP (5)
03507              ELSE
03508                  MOVE WS-AGENT-2ND-ZIP (5)
03509                                  TO  WS-A-LAST-2ND-ZIP (5)
03510                  MOVE SPACES     TO  WS-AGENT-ZIP (5)
03511          ELSE
03512              IF WS-AGENT-LINES (4) NOT =  SPACES
03513                  IF CO-CANADIAN-POST-CODE
03514                          OR
03515                     WS-AGENT-1ST-ZIP (4)  NOT =  ZEROS
03516                      MOVE WS-AGENT-ZIP (4)
03517                                  TO  WS-A-LAST-ZIP (4)
03518                      MOVE SPACES TO  WS-AGENT-ZIP (4)
03519                  ELSE
03520                      MOVE WS-AGENT-2ND-ZIP (4)
03521                                  TO  WS-A-LAST-2ND-ZIP (4)
03522                      MOVE SPACES TO  WS-AGENT-ZIP (4).
03523
03524      MOVE 'GA'                   TO  BILLING-DETAIL-TYPE.
03525
03526      PERFORM 3000-WRITE-BILLING-DETAIL  THRU  3990-EXIT.
03527
03528      GO TO 6090-EXIT.
03529
03530  6070-NO-COMP-MSTR.
03531      IF GENERAL-AGENT
03532          MOVE -1                 TO  AAGENTL
03533          MOVE ER-2230            TO  EMI-ERROR
03534          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
03535          GO TO 8200-SEND-DATAONLY.
03536
03537      MOVE ZEROS                  TO  WS-ACCT-BEG-BAL.
03538
03539      GO TO 6090-EXIT.
03540
03541  6090-EXIT.
03542      EXIT.
03543  EJECT
03544  6100-READ-CONTROL-FILE.
03545      MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID.
03546      MOVE +0                     TO  ELCNTL-SEQ-NO.
03547
03548      
      * EXEC CICS HANDLE CONDITION
03549 *        NOTFND  (6110-NO-RECORD)
03550 *    END-EXEC.
      *    MOVE '"$I                   ! - #00008323' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303038333233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03551
03552      IF NOT  ELCNTL-UPDATE
03553          
      * EXEC CICS READ
03554 *            DATASET  (ELCNTL-FILE-ID)
03555 *            SET      (ADDRESS OF CONTROL-FILE)
03556 *            RIDFLD   (ELCNTL-KEY)
03557 *        END-EXEC
      *    MOVE '&"S        E          (   #00008328' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03558      ELSE
03559          MOVE SPACE              TO  ELCNTL-UPDATE-SW
03560          
      * EXEC CICS READ
03561 *            DATASET  (ELCNTL-FILE-ID)
03562 *            SET      (ADDRESS OF CONTROL-FILE)
03563 *            RIDFLD   (ELCNTL-KEY)
03564 *            UPDATE
03565 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00008335' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03566
03567      GO TO 6190-EXIT.
03568
03569  6110-NO-RECORD.
03570      IF ELCNTL-REC-TYPE  =  1
03571          MOVE ER-0022            TO  EMI-ERROR
03572      ELSE
03573          MOVE ER-2208            TO  EMI-ERROR
03574          MOVE -1                 TO  ACARIERL
03575          MOVE AL-UABON           TO  ACARIERA
03576
03577      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03578
03579  6190-EXIT.
03580      EXIT.
03581  EJECT
03582  6200-REWRITE-CONTROL-FILE.
03583      MOVE 'C'                    TO  JP-RECORD-TYPE.
03584      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
03585      MOVE ELCNTL-FILE-ID         TO  JP-FILE-ID.
03586
03587      COMPUTE JOURNAL-LENGTH = ELCNTL-LENGTH + 23.
03588
03589      
      * EXEC CICS REWRITE
03590 *        DATASET  (ELCNTL-FILE-ID)
03591 *        FROM     (CONTROL-FILE)
03592 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008364' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03593
03594      PERFORM 8400-LOG-JOURNAL-RECORD.
03595
03596  6290-EXIT.
03597      EXIT.
03598  EJECT
03599  6400-READ-CROSS-REFERENCE.
03600      MOVE PI-COMPANY-CD          TO  ERGXRF-COMPANY-CD.
03601      MOVE PI-SAV-AGENT           TO  ERGXRF-AGENT-NO.
03602
03603      IF NOT  PI-ZERO-CARRIER
03604        AND  NOT  PI-ZERO-CAR-GROUP
03605          MOVE PI-CR-CARRIER      TO  ERGXRF-CARRIER
03606      ELSE
03607          MOVE ZEROS              TO  ERGXRF-CARRIER.
03608
03609      IF NOT  PI-ZERO-GROUPING
03610        AND  NOT  PI-ZERO-CAR-GROUP
03611          MOVE PI-CR-GROUPING     TO  ERGXRF-GROUPING
03612      ELSE
03613          MOVE ZEROS              TO  ERGXRF-GROUPING.
03614
03615      
      * EXEC CICS HANDLE CONDITION
03616 *        NOTFND  (6410-NOT-FOUND)
03617 *    END-EXEC.
      *    MOVE '"$I                   ! . #00008390' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303038333930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03618
03619      
      * EXEC CICS READ
03620 *        SET      (ADDRESS OF AGENT-CROSS-REFERENCE)
03621 *        DATASET  (ERGXRF-FILE-ID)
03622 *        RIDFLD   (ERGXRF-KEY)
03623 *        LENGTH   (ERGXRF-LENGTH)
03624 *        UPDATE
03625 *    END-EXEC.
      *    MOVE '&"SL       EU         (   #00008394' TO DFHEIV0
           MOVE X'2622534C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 DFHEIV20, 
                 ERGXRF-LENGTH, 
                 ERGXRF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AGENT-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03626
03627      MOVE GX-AGENT-POINTER-CNT   TO  GX-AGENT-POINTER-CNT.
03628
03629      IF PI-UPDATE-FILES
03630          MOVE 'B'                TO  JP-RECORD-TYPE
03631          MOVE AGENT-CROSS-REFERENCE
03632                                  TO  JP-RECORD-AREA
03633          MOVE ERGXRF-FILE-ID     TO  JP-FILE-ID
03634          COMPUTE JOURNAL-LENGTH = ERGXRF-LENGTH + 23
03635          PERFORM 8400-LOG-JOURNAL-RECORD.
03636
03637      GO TO 6490-EXIT.
03638
03639  6410-NOT-FOUND.
03640      MOVE -1                     TO  AAGENTL.
03641      MOVE ER-2911                TO  EMI-ERROR.
03642
03643      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03644
03645      GO TO 8200-SEND-DATAONLY.
03646
03647  6490-EXIT.
03648      EXIT.
03649  EJECT
03650  6500-REWRITE-CROSS-REFERENCE.
03651      MOVE 'C'                    TO  JP-RECORD-TYPE.
03652      MOVE AGENT-CROSS-REFERENCE  TO  JP-RECORD-AREA.
03653      MOVE ERGXRF-FILE-ID         TO  JP-FILE-ID.
03654
03655      
      * EXEC CICS REWRITE
03656 *        DATASET  (ERGXRF-FILE-ID)
03657 *        FROM     (AGENT-CROSS-REFERENCE)
03658 *        LENGTH   (ERGXRF-LENGTH)
03659 *    END-EXEC.
      *    MOVE '&& L                  %   #00008430' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03660
03661      PERFORM 8400-LOG-JOURNAL-RECORD.
03662
03663  6590-EXIT.
03664      EXIT.
03665  EJECT
03666  6600-READ-COMMISSION-EXCEPTION.
03667      
      * EXEC CICS HANDLE CONDITION
03668 *        NOTFND  (6610-NOT-FOUND)
03669 *    END-EXEC.
      *    MOVE '"$I                   ! / #00008442' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303038343432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03670
03671      MOVE 'Y'                    TO ERCOMM-FOUND-SW.
03672      MOVE PB-CONTROL-BY-ACCOUNT  TO  ERCOMM-KEY.
03673      MOVE PB-SV-CARRIER          TO  ERCOMM-CARRIER.
03674      MOVE PB-SV-GROUPING         TO  ERCOMM-GROUPING.
03675      MOVE PB-SV-STATE            TO  ERCOMM-STATE.
03676
03677      
      * EXEC CICS READ
03678 *        SET      (ADDRESS OF COMMISSION-EXCEPTIONS)
03679 *        DATASET  (ERCOMM-FILE-ID)
03680 *        RIDFLD   (ERCOMM-KEY)
03681 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008452' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMMISSION-EXCEPTIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03682
03683      GO TO 6620-EXIT.
03684
03685  6610-NOT-FOUND.
03686      MOVE 'N'                    TO  ERCOMM-FOUND-SW.
03687
03688  6620-EXIT.
03689      EXIT.
03690
03691 **START***********************************************************
03692 *      CUSTOM CODING FOR CLIENT "DMD"
03693 ******************************************************************
03694 *
03695  6650-GET-RES-STATE-COMM.
03696 *6650-BROWSE-FORWARD.
03697
03698      IF PB-COMPANY-ID NOT = 'DMD'
03699          GO TO 6700-COMM-EXIT.
03700
03701      MOVE PB-COMPANY-CD       TO WS-ERRESC-COMPANY-CD.
03702      MOVE PB-SV-CARRIER       TO WS-ERRESC-CARRIER.
03703      MOVE PB-SV-GROUPING      TO WS-ERRESC-GROUP.
03704      MOVE PB-SV-STATE         TO WS-ERRESC-STATE.
03705      MOVE PB-ACCOUNT          TO WS-ERRESC-ACCOUNT.
03706      MOVE AM-AGT (WS-GA-LEVEL) TO WS-ERRESC-AGENT.
03707      MOVE PB-CERT-NO (1:2)    TO WS-ERRESC-RES-STATE.
03708
03709 *CONVERT PB-CERT-EFF-DT TO YYYYMMDD FORMATT TO READ MASTER
03710 *
03711      MOVE PB-CERT-EFF-DT      TO DC-BIN-DATE-1.
03712      SET BIN-TO-GREG          TO TRUE.
03713      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
03714
03715      MOVE DC-GREG-DATE-1-YMD  TO WS-CONTRACT-DATE (3:6).
03716
03717      IF DC-GREG-DATE-1-YMD (1:2) IS LESS THAN '10'
03718          MOVE '20'            TO WS-CONTRACT-DATE (1:2)
03719      ELSE
03720          MOVE '19'            TO WS-CONTRACT-DATE (1:2).
03721
03722      MOVE WS-CONTRACT-DATE    TO WS-ERRESC-EXPIRE-DT.
03723
03724      
      * EXEC CICS HANDLE CONDITION
03725 *        INVREQ    (6660-START-BROWSE)
03726 *        NOTFND    (6700-COMM-EXIT)
03727 *    END-EXEC.
      *    MOVE '"$8I                  ! 0 #00008499' TO DFHEIV0
           MOVE X'222438492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303038343939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03728
03729      
      * EXEC CICS RESETBR
03730 *        DATASET (ERRESC-FILE-ID)
03731 *        RIDFLD  (WS-ERRESC-KEY)
03732 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         G          &   #00008504' TO DFHEIV0
           MOVE X'263420202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRESC-FILE-ID, 
                 WS-ERRESC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03733
03734  6660-START-BROWSE.
03735
03736      
      * EXEC CICS HANDLE CONDITION
03737 *        NOTFND    (6700-COMM-EXIT)
03738 *        ENDFILE   (6700-COMM-EXIT)
03739 *    END-EXEC.
      *    MOVE '"$I''                  ! 1 #00008511' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303038353131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03740
03741      
      * EXEC CICS STARTBR
03742 *        DATASET (ERRESC-FILE-ID)
03743 *        RIDFLD  (WS-ERRESC-KEY)
03744 *        GTEQ
03745 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008516' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRESC-FILE-ID, 
                 WS-ERRESC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03746
03747      MOVE WS-ERRESC-KEY           TO WS-SV-ERRESC-KEY.
03748
03749  6670-READ-NEXT-ERRESC-MASTER.
03750
03751      
      * EXEC CICS HANDLE CONDITION
03752 *        NOTFND    (6700-COMM-EXIT)
03753 *        ENDFILE   (6700-COMM-EXIT)
03754 *    END-EXEC.
      *    MOVE '"$I''                  ! 2 #00008526' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303038353236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03755
03756      
      * EXEC CICS READNEXT
03757 *        SET      (ADDRESS OF ACCOUNT-RESIDENT-ST-COMMISSION)
03758 *        DATASET (ERRESC-FILE-ID)
03759 *        RIDFLD  (WS-ERRESC-KEY)
03760 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008531' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRESC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERRESC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-RESIDENT-ST-COMMISSION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03761
03762      IF ERRESC-RECORD-KEY (1:32) = WS-SV-ERRESC-KEY
03763         NEXT SENTENCE
03764      ELSE
03765         
      * EXEC CICS ENDBR
03766 *            DATASET (ERRESC-FILE-ID)
03767 *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008540' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRESC-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03768         GO TO 6700-COMM-EXIT.
03769
03770 * CHECK IF CERTIFICATE EFFECTIVE DATE IS WITHIN ACCOUNT RESIDENT
03771 * STATE COMMISSIONS RECORD'S EFFECT TIME PERIOD????
03772      IF WS-CONTRACT-DATE IS LESS THAN RESC-EFFECTIVE-DATE
03773         
      * EXEC CICS ENDBR
03774 *            DATASET (ERRESC-FILE-ID)
03775 *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008548' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRESC-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03776         GO TO 6700-COMM-EXIT.
03777
03778 * SEARCH FOR MATCHING CATEGORY CODE ENTRY
03779 *
03780 *    IF RESC-COMMISSION IS NUMERIC THAT IS THE COMMISSION TO OVER-
03781 *    RIDE AH OR LIFE COMMISSION IN IN THE ACCOUNT MASTER.
03782 *    IF RESC-COMMISSION NOT NUMERIC, USE VALUE TO GET COMMISSION
03783 *    FROM COMMISSION TABLE IF RECORD EXISTS.
03784 *
03785      MOVE ZEROS               TO WS-SUB WS-SUB1.
03786
03787      PERFORM 6680-SEARCH-CATEGORY THRU 6680-EXIT.
03788
03789      IF WS-SUB1 IS GREATER THAN ZERO
03790          IF RESC-COMMISSION-TAB (WS-SUB) NUMERIC
03791              PERFORM 6680-SET-COMMISSION
03792          ELSE
03793              PERFORM 6680-SET-TABLE-CODE.
03794
03795      
      * EXEC CICS ENDBR
03796 *         DATASET (ERRESC-FILE-ID)
03797 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008570' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRESC-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03798
03799      GO TO 6700-COMM-EXIT.
03800
03801  6680-SET-TABLE-CODE.
03802      IF RESC-COMMISSION-TAB (WS-SUB) EQUAL SPACES
03803         MOVE ZEROS             TO WS-SUB1
03804      ELSE
03805
03806 * READ COMMISSION TABLE(CTBL)  KEY VALUES PRESET IN CALLING
03807 * PARAGRAPHS(4405-FIND-AH, 4420-CONT-FIND-LIFE, 4430-SINGLE-LIFE)
03808
03809      MOVE RESC-COMMISSION-TAB (WS-SUB) TO ERCTBL-TABLE
03810      PERFORM 6680-READ-CTBL-TABLE.
03811
03812  6680-READ-CTBL-TABLE.
03813 *
03814 * READ COMMISSION TABLE TO VERIFY IF ACCOUNT RESIDENT STATE
03815 * COMMISSION RECORD CONTAINS A VALID COMPENSATION TABLE CODE.
03816 * IF CODE IS NOT VALID USE CODE IN ACCOUNT MASTER
03817
03818      PERFORM 6700-READ-COMMISSION-TABLE THRU 6790-EXIT.
03819
03820      IF ERCTBL-NOT-FOUND
03821          MOVE 'AA'               TO  ERCTBL-BEN-CODE
03822          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT
03823          IF ERCTBL-NOT-FOUND
03824              MOVE ZEROS          TO  WS-SUB1.
03825
03826      IF ERCTBL-FOUND
03827         IF AH-COMM
03828             PERFORM 6680-AH-GET-COMP-RATE
03829         ELSE
03830             PERFORM 6680-LF-GET-COMP-RATE.
03831
03832  6680-AH-GET-COMP-RATE.
03833      IF PB-ISSUE
03834          COMPUTE WS-COMM-CK-AMT =
03835              PB-I-AH-BENEFIT-AMT * PB-I-AH-TERM
03836          MOVE PB-I-AGE           TO  WS-COMM-AGE
03837          MOVE PB-I-AH-TERM       TO  WS-COMM-TERM
03838      ELSE
03839          COMPUTE WS-COMM-CK-AMT =
03840              PB-CI-AH-BENEFIT-AMT * PB-CI-AH-TERM
03841          MOVE PB-CI-INSURED-AGE  TO  WS-COMM-AGE
03842          MOVE PB-CI-AH-TERM      TO  WS-COMM-TERM.
03843
03844      PERFORM 4900-GET-COMP-RATE  THRU  4990-EXIT.
03845
03846      MOVE WS-WK-RATE             TO  WS-GA-AH-COM.
03847
03848  6680-LF-GET-COMP-RATE.
03849      IF PB-ISSUE
03850          MOVE PB-I-LF-BENEFIT-AMT
03851                                  TO  WS-COMM-CK-AMT
03852          MOVE PB-I-AGE           TO  WS-COMM-AGE
03853          MOVE PB-I-LF-TERM       TO  WS-COMM-TERM
03854      ELSE
03855          MOVE PB-CI-INSURED-AGE  TO  WS-COMM-AGE
03856          MOVE PB-CI-LF-TERM      TO  WS-COMM-TERM
03857          MOVE PB-CI-LF-BENEFIT-AMT
03858                                  TO  WS-COMM-CK-AMT.
03859
03860      PERFORM 4900-GET-COMP-RATE  THRU  4990-EXIT.
03861
03862      MOVE WS-WK-RATE             TO  WS-GA-LF-COM.
03863
03864  6680-SET-COMMISSION.
03865      IF RESC-COMMISSION-PER (WS-SUB) EQUAL ZEROS
03866            MOVE ZEROS          TO WS-SUB1
03867        ELSE
03868            MOVE RESC-COMMISSION-PER (WS-SUB)
03869                               TO WS-COMMISSION
03870            IF AH-COMM
03871               MOVE WS-COMMISSION TO WS-GA-AH-COM
03872            ELSE
03873               MOVE WS-COMMISSION TO WS-GA-LF-COM.
03874
03875  6680-SEARCH-CATEGORY.
03876      ADD +1 TO WS-SUB.
03877      IF WS-SUB IS GREATER THAN +12
03878          GO TO 6680-EXIT.
03879
03880      IF PB-CERT-NO (4:1) = RESC-COVERAGE-CAT (WS-SUB)
03881           MOVE WS-SUB         TO WS-SUB1
03882           GO TO 6680-EXIT.
03883
03884      GO TO 6680-SEARCH-CATEGORY.
03885
03886  6680-EXIT.
03887      EXIT.
03888
03889  6700-COMM-EXIT.
03890      EXIT.
03891 **END*************************************************************
03892 *      CUSTOM CODING FOR CLIENT "DMD"
03893 ******************************************************************
03894 *
03895  EJECT
03896  6700-READ-COMMISSION-TABLE.
03897      
      * EXEC CICS HANDLE CONDITION
03898 *        NOTFND  (6710-NOT-FOUND)
03899 *    END-EXEC.
      *    MOVE '"$I                   ! 3 #00008672' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303038363732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03900
03901      MOVE 'Y'                    TO  ERCTBL-FOUND-SW.
03902
03903      
      * EXEC CICS READ
03904 *        SET      (ADDRESS OF COMM-TABLE-RECORD)
03905 *        DATASET  (ERCTBL-FILE-ID)
03906 *        RIDFLD   (ERCTBL-KEY)
03907 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008678' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCTBL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCTBL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-TABLE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03908
03909      GO TO 6790-EXIT.
03910
03911  6710-NOT-FOUND.
03912      MOVE 'N'                    TO  ERCTBL-FOUND-SW.
03913
03914  6790-EXIT.
03915      EXIT.
03916  EJECT
03917  6800-READ-BENEFIT-MASTER.
03918      
      * EXEC CICS HANDLE CONDITION
03919 *        NOTFND  (6880-NOT-FOUND)
03920 *    END-EXEC.
      *    MOVE '"$I                   ! 4 #00008693' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303038363933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03921
03922      MOVE 'Y'                    TO  BENEFIT-MASTER-FOUND-SW.
03923      MOVE PB-COMPANY-ID          TO  CLBENF-COMPANY-ID.
03924      MOVE '4'                    TO  CLBENF-REC-TYPE.
03925      MOVE +0                     TO  CLBENF-SEQ-NO.
03926
03927      
      * EXEC CICS READ
03928 *        SET      (ADDRESS OF CONTROL-FILE)
03929 *        DATASET  (ELCNTL-FILE-ID)
03930 *        RIDFLD   (ELCNTL-BENEFIT-KEY)
03931 *        GTEQ
03932 *    END-EXEC.
      *    MOVE '&"S        G          (   #00008702' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-BENEFIT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03933
03934      IF CLBENF-COMPANY-ID  NOT =  CF-COMPANY-ID
03935          GO TO 6880-NOT-FOUND.
03936
03937      IF CLBENF-REC-TYPE  NOT =  CF-RECORD-TYPE
03938          GO TO 6880-NOT-FOUND.
03939
03940      MOVE +0                     TO  BEN-SUB.
03941
03942  6810-FIND-BENEFIT.
03943      ADD +1                      TO  BEN-SUB.
03944
03945      IF BEN-SUB  GREATER THAN  +8
03946          GO TO 6880-NOT-FOUND.
03947
03948      IF CF-BENEFIT-CODE (BEN-SUB)  =  CLBENF-CD
03949          MOVE CF-JOINT-INDICATOR (BEN-SUB)
03950                                  TO  LIFE-INDICATOR
03951          GO TO 6890-EXIT.
03952
03953      GO TO 6810-FIND-BENEFIT.
03954
03955  6880-NOT-FOUND.
03956      MOVE 'N'                    TO  BENEFIT-MASTER-FOUND-SW.
03957
03958  6890-EXIT.
03959      EXIT.
03960  EJECT
03961  7000-PROCESS-CHECK.
03962      MOVE LOW-VALUES             TO  EL642BO.
03963
03964      MOVE 'G'                    TO  COMPENSATION-SW
03965                                      PI-CR-TYPE.
03966
03967      PERFORM 6000-READ-COMP-MASTER  THRU  6090-EXIT.
03968
03969      IF EMI-ERROR = ZEROS
03970          MOVE CO-MAIL-NAME       TO  BNAMEO
03971          MOVE CO-ADDR-1          TO  BADDR1O
03972          MOVE CO-ADDR-2          TO  BADDR2O
03973          MOVE CO-ADDR-3          TO  BCITYSTO
03974          MOVE CO-ZIP             TO  BZIPO
03975      ELSE
03976          MOVE EL642A             TO  PI-MAP-NAME
03977          MOVE -1                 TO  BPFNTERL
03978          GO TO 8200-SEND-DATAONLY.
03979
03980      IF PI-COMP-UNPAID-PREM  GREATER THAN  ZERO
03981          MOVE PI-COMP-UNPAID-PREM
03982                                  TO BCHKAMTO
03983      ELSE
03984          MOVE ZEROS              TO  BCHKAMTO.
03985
03986      MOVE AL-UNNON               TO  BCHKAMTA.
03987      MOVE WS-CURRENT-DATE-EDIT   TO  BPAYDT1O.
03988      MOVE -1                     TO  BCHKNOL.
03989
03990      GO TO 8100-SEND-INITIAL-MAP.
03991  EJECT
03992  7500-PRODUCE-CHECK.
03993      MOVE SPACES                 TO  ELCNTL-KEY.
03994      MOVE '1'                    TO  ELCNTL-REC-TYPE.
03995      MOVE 'Y'                    TO  ELCNTL-UPDATE-SW.
03996
03997      PERFORM 6100-READ-CONTROL-FILE  THRU  6190-EXIT.
03998
03999      IF EMI-ERROR  NOT =  ZEROS
04000          GO TO 8200-SEND-DATAONLY.
04001
04002      IF CR-CHECK-NO-AUTO-SEQ
04003          MOVE 'B'                TO  JP-RECORD-TYPE
04004          MOVE CONTROL-FILE       TO  JP-RECORD-AREA
04005          MOVE ELCNTL-FILE-ID     TO  JP-FILE-ID
04006          COMPUTE JOURNAL-LENGTH = ELCNTL-LENGTH + 23
04007          PERFORM 8400-LOG-JOURNAL-RECORD
04008          IF CR-CHECK-CNT-RESET-VALUE
04009              MOVE CF-CR-CHECK-COUNTER
04010                                  TO  BCHKNOI
04011              MOVE +1             TO  CF-CR-CHECK-COUNTER
04012              MOVE 6              TO  BCHKNOL
04013          ELSE
04014              MOVE CF-CR-CHECK-COUNTER
04015                                  TO  BCHKNOI
04016              MOVE 6              TO  BCHKNOL
04017              ADD +1              TO  CF-CR-CHECK-COUNTER.
04018
04019      IF CR-CHECK-NO-MANUAL
04020          IF BCHKNOL = ZEROS
04021              MOVE ER-2438        TO  EMI-ERROR
04022              MOVE -1             TO  BCHKNOL
04023              MOVE AL-UNBON       TO  BCHKNOA
04024              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
04025              GO TO 8200-SEND-DATAONLY
04026          ELSE
04027              IF BCHKNOI NOT NUMERIC
04028                  MOVE ER-2439    TO  EMI-ERROR
04029                  MOVE -1         TO  BCHKNOL
04030                  MOVE AL-UNBON   TO  BCHKNOA
04031                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
04032                  GO TO 8200-SEND-DATAONLY.
04033
04034      
      * EXEC CICS GETMAIN
04035 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
04036 *        LENGTH   (ERPYAJ-LENGTH)
04037 *        INITIMG  (GETMAIN-SPACE)
04038 *    END-EXEC.
      *    MOVE ',"IL                  $   #00008809' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPYAJ-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04039
04040      
      * EXEC CICS BIF DEEDIT
04041 *        FIELD   (BCHKAMTI)
04042 *        LENGTH  (09)
04043 *    END-EXEC.
           MOVE 09
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008815' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BCHKAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04044
04045      IF BCHKAMTI NOT GREATER THAN ZEROS
04046          MOVE ER-3165    TO  EMI-ERROR
04047          MOVE -1         TO  BCHKAMTL
04048          MOVE AL-UNBON   TO  BCHKAMTA
04049          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
04050          GO TO 8200-SEND-DATAONLY.
04051
04052      MOVE 'Y'                    TO  PI-CHECK-SW.
04053      MOVE 'PY'                   TO  PY-RECORD-ID.
04054      MOVE PI-COMPANY-CD          TO  PY-COMPANY-CD.
04055      MOVE PI-SAV-CARR            TO  PY-CARRIER.
04056      MOVE PI-SAV-GROUP           TO  PY-GROUPING.
04057      MOVE PI-SAV-AGENT           TO  PY-FIN-RESP.
04058      MOVE LOW-VALUES             TO  PY-ACCOUNT.
04059      MOVE 'C'                    TO  PY-RECORD-TYPE.
04060      MOVE EIBTIME                TO  PY-FILE-SEQ-NO.
04061      MOVE WS-CURRENT-DATE-MDY    TO  WS-PY-CURRENT-DATE.
04062      MOVE WS-PY-ENTRY-COMMENT    TO  PY-ENTRY-COMMENT.
04063      MOVE BCHKAMTI               TO  PY-ENTRY-AMT.
04064
04065      ADD BCHKAMTI                TO  PI-DISBURSED.
04066
04067      IF NOT  CR-CHECK-NO-AT-PRINT
04068          MOVE BCHKNOI            TO  PY-CHECK-NUMBER.
04069
04070      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.
04071      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.
04072      MOVE WS-CURRENT-DATE        TO  PY-LAST-MAINT-DT
04073                                      PY-INPUT-DT.
04074      MOVE LOW-VALUES             TO  PY-BILLED-DATE
04075                                      PY-AR-DATE.
04076      MOVE ZEROS                  TO  PY-CHECK-QUE-CONTROL
04077                                      PY-CHECK-QUE-SEQUENCE.
04078      MOVE LOW-VALUES             TO  PY-CREDIT-ACCEPT-DT
04079                                      PY-REPORTED-DT
04080                                      PY-CHECK-WRITTEN-DT.
04081      MOVE PI-CR-MONTH-END-DT     TO  PY-CREDIT-SELECT-DT.
04082      MOVE 'G'                    TO  PY-CHECK-ORIGIN-SW.
04083      MOVE 'A'                    TO  JP-RECORD-TYPE.
04084      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
04085      MOVE ERPYAJ-FILE-ID         TO  JP-FILE-ID.
04086
04087      COMPUTE JOURNAL-LENGTH = ERPYAJ-LENGTH + 23.
04088
04089      
      * EXEC CICS WRITE
04090 *        DATASET  (ERPYAJ-FILE-ID)
04091 *        FROM     (PENDING-PAY-ADJ)
04092 *        RIDFLD   (PY-CONTROL-PRIMARY)
04093 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008864' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04094
04095      PERFORM 8400-LOG-JOURNAL-RECORD.
04096
04097      IF CR-CHECK-NO-AUTO-SEQ
04098          PERFORM 6200-REWRITE-CONTROL-FILE  THRU  6290-EXIT
04099      ELSE
04100          
      * EXEC CICS UNLOCK
04101 *            DATASET  (ELCNTL-FILE-ID)
04102 *        END-EXEC.
      *    MOVE '&*                    #   #00008875' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04103
04104      MOVE EL642A                 TO  PI-MAP-NAME.
04105
04106      PERFORM 5000-FORMAT-SCREEN  THRU  5090-EXIT.
04107  EJECT
04108  8100-SEND-INITIAL-MAP.
04109
04110      MOVE SPACES TO PI-TRANSFER-SW.
04111
04112      IF PI-MAP-NAME  =  EL642A
04113          NEXT SENTENCE
04114      ELSE
04115          GO TO 8110-SEND-INITIAL-CHECK-MAP.
04116
04117      MOVE WS-CURRENT-DATE-EDIT   TO  ADATEO.
04118      MOVE EIBTIME                TO  TIME-IN.
04119      MOVE TIME-OUT               TO  ATIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
04120      MOVE -1                     TO  AAGENTL.
04121      MOVE EMI-MESSAGE-AREA (1)   TO  AERMSG1O.
04122      MOVE EMI-MESSAGE-AREA (2)   TO  AERMSG2O.
04123
04124      IF PI-ZERO-CARRIER
04125          MOVE AL-SADOF           TO  ACARHDGA
04126                                      ACARIERA.
04127
04128      IF PI-ZERO-GROUPING
04129          MOVE AL-SADOF           TO  AGRPHDGA
04130                                      AGROUPA.
04131
04132      IF PI-ZERO-CAR-GROUP
04133          MOVE AL-SADOF           TO  ACARHDGA
04134                                      ACARIERA
04135          MOVE AL-SADOF           TO  AGRPHDGA
04136                                      AGROUPA.
04137
04138      IF (EIBTRNID = EL633-TRANS-ID OR EL635-TRANS-ID OR
04139                     EL633DMD-TRANS-ID OR
04140                     EL650-TRANS-ID OR EL652-TRANS-ID OR
04141                     EL658-TRANS-ID)    AND
04142         (WT-CR-FIN-RESP NOT EQUAL SPACES AND LOW-VALUES)
04143          MOVE -1                    TO  ABILTYPL
04144          MOVE WT-CR-STATE           TO  PI-SCR-STATE
04145          MOVE WT-CR-ACCOUNT         TO  PI-SCR-ACCOUNT
04146          MOVE WT-CR-TYPE            TO  PI-SCR-TYPE
04147          IF PI-ZERO-CARRIER
04148              MOVE WT-CR-FIN-RESP    TO  AAGENTI
04149                                         PI-SCR-FIN-RESP
04150              MOVE WT-CR-GROUPING    TO  AGROUPI
04151                                         PI-SCR-GROUPING
04152              MOVE AL-UANON          TO  AAGENTA
04153                                         AGROUPA
04154              MOVE 10                TO  AAGENTL
04155              MOVE 6                 TO  AGROUPL
04156          ELSE
04157             IF PI-ZERO-GROUPING
04158                 MOVE WT-CR-CARRIER    TO  ACARIERI
04159                                           PI-SCR-CARRIER
04160                 MOVE WT-CR-FIN-RESP   TO  AAGENTI
04161                                           PI-SCR-FIN-RESP
04162                 MOVE AL-UANON         TO  ACARIERA
04163                                           AAGENTA
04164                 MOVE 10               TO  AAGENTL
04165                 MOVE 1                TO  ACARIERL
04166             ELSE
04167                IF PI-ZERO-CAR-GROUP
04168                    MOVE WT-CR-FIN-RESP TO  AAGENTI
04169                                            PI-SCR-FIN-RESP
04170                    MOVE AL-UANON       TO  AAGENTA
04171                    MOVE 10             TO  AAGENTL
04172                ELSE
04173                    MOVE WT-CR-CARRIER     TO  ACARIERI
04174                                               PI-SCR-CARRIER
04175                    MOVE WT-CR-FIN-RESP    TO  AAGENTI
04176                                               PI-SCR-FIN-RESP
04177                    MOVE WT-CR-GROUPING    TO  AGROUPI
04178                                               PI-SCR-GROUPING
04179                    MOVE AL-UANON          TO  AAGENTA
04180                                               AGROUPA
04181                                               ACARIERA
04182                    MOVE 10                TO  AAGENTL
04183                    MOVE 6                 TO  AGROUPL
04184                    MOVE 1                 TO  ACARIERL.
04185
04186      
      * EXEC CICS SEND
04187 *        MAP     (PI-MAP-NAME)
04188 *        MAPSET  (MAPSET-NAME)
04189 *        FROM    (EL642AO)
04190 *        ERASE
04191 *        CURSOR
04192 *    END-EXEC.
           MOVE LENGTH OF
            EL642AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00008963' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL642AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04193
04194      GO TO 9100-RETURN-TRAN.
04195  EJECT
04196  8110-SEND-INITIAL-CHECK-MAP.
04197      MOVE WS-CURRENT-DATE-EDIT   TO  BDATEO.
04198      MOVE EIBTIME                TO  TIME-IN.
04199      MOVE TIME-OUT               TO  BTIMEO.
101101     MOVE PI-COMPANY-ID          TO  BCMPNYO.
101101     MOVE PI-PROCESSOR-ID        TO  BUSERIDO.
04200      MOVE -1                     TO  BPFNTERL.
04201      MOVE EMI-MESSAGE-AREA (1)   TO  BERMSGO.
04202
04203      
      * EXEC CICS SEND
04204 *        MAP     (PI-MAP-NAME)
04205 *        MAPSET  (MAPSET-NAME)
04206 *        FROM    (EL642BO)
04207 *        ERASE
04208 *        CURSOR
04209 *    END-EXEC.
           MOVE LENGTH OF
            EL642BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00008982' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL642BO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04210
04211      GO TO 9100-RETURN-TRAN.
04212  EJECT
04213  8200-SEND-DATAONLY.
04214      IF PI-MAP-NAME  =  EL642A
04215          MOVE WS-CURRENT-DATE-EDIT  TO  ADATEO
04216          MOVE EIBTIME               TO  TIME-IN
04217          MOVE TIME-OUT              TO  ATIMEO
101101         MOVE PI-COMPANY-ID         TO  CMPNYIDO
101101         MOVE PI-PROCESSOR-ID       TO  USERIDO
04218          MOVE EMI-MESSAGE-AREA (1)  TO  AERMSG1O
04219          MOVE EMI-MESSAGE-AREA (2)  TO  AERMSG2O
04220          
      * EXEC CICS SEND
04221 *            MAP     (PI-MAP-NAME)
04222 *            MAPSET  (MAPSET-NAME)
04223 *            FROM    (EL642AO)
04224 *            DATAONLY
04225 *            ERASEAUP
04226 *            CURSOR
04227 *        END-EXEC
           MOVE LENGTH OF
            EL642AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00009001' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL642AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04228      ELSE
04229          MOVE WS-CURRENT-DATE-EDIT  TO  BDATEO
04230          MOVE EIBTIME               TO  TIME-IN
04231          MOVE TIME-OUT              TO  BTIMEO
101101         MOVE PI-COMPANY-ID         TO  BCMPNYO
101101         MOVE PI-PROCESSOR-ID       TO  BUSERIDO
04232          MOVE EMI-MESSAGE-AREA (1)  TO  BERMSGO
04233          
      * EXEC CICS SEND
04234 *            MAP     (PI-MAP-NAME)
04235 *            MAPSET  (MAPSET-NAME)
04236 *            FROM    (EL642BO)
04237 *            DATAONLY
04238 *            ERASEAUP
04239 *            CURSOR
04240 *        END-EXEC.
           MOVE LENGTH OF
            EL642BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00009016' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL642BO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04241
04242      GO TO 9100-RETURN-TRAN.
04243  EJECT
04244  8300-SEND-TEXT.
04245      
      * EXEC CICS SEND TEXT
04246 *        FROM    (LOGOFF-TEXT)
04247 *        LENGTH  (LOGOFF-LENGTH)
04248 *        ERASE
04249 *        FREEKB
04250 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009028' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303238' TO DFHEIV0(25:11)
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
           
04251
04252      
      * EXEC CICS RETURN
04253 *    END-EXEC.
      *    MOVE '.(                    &   #00009035' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04254
04255  8400-LOG-JOURNAL-RECORD.
04256      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
04257      MOVE THIS-PGM               TO  JP-PROGRAM-ID.
04258
04259 *    EXEC CICS JOURNAL
04260 *        JFILEID  (PI-JOURNAL-FILE-ID)
04261 *        JTYPEID  ('EL')
04262 *        FROM     (JOURNAL-RECORD)
04263 *        LENGTH   (JOURNAL-LENGTH)
04264 *    END-EXEC.
04265
04266  8500-DATE-CONVERT.
04267      
      * EXEC CICS LINK
04268 *        PROGRAM   (LINK-ELDATCV)
04269 *        COMMAREA  (DATE-CONVERSION-DATA)
04270 *        LENGTH    (DC-COMM-LENGTH) END-EXEC.
      *    MOVE '."C                   ''   #00009050' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04271
04272  8500-EXIT.
04273      EXIT.
04274  EJECT
04275  8600-CENTER-DATA.
04276      MOVE SPACE                  TO  CENTER-WORK-2.
04277
04278      IF CW1-PIC (1)  NOT =  SPACE
04279        AND  CW1-PIC (X-LEN)  NOT =  SPACE
04280          MOVE CENTER-WORK-1      TO  CENTER-WORK-2
04281          GO TO 8690-C-D-X.
04282
04283      MOVE +0                     TO  X1.
04284      MOVE X-LEN                  TO  X2.
04285
04286  8660-C-D-LOOP-1.
04287      IF CW1-PIC (X2)  =  SPACE
04288          ADD +1                  TO  X1
04289          SUBTRACT +1             FROM  X2
04290          GO TO 8660-C-D-LOOP-1.
04291
04292      MOVE +1                     TO  X2.
04293
04294  8670-C-D-LOOP-2.
04295      IF CW1-PIC (X2)  =  SPACE
04296          ADD +1                  TO  X1  X2
04297          GO TO 8670-C-D-LOOP-2.
04298
04299      COMPUTE X3 = (X1 / +2) + +1.
04300      COMPUTE X1 = X-LEN - X1.
04301
04302  8680-C-D-LOOP-3.
04303      IF X1  NOT =  +0
04304          MOVE CW1-PIC (X2)       TO  CW2-PIC (X3)
04305          ADD +1                  TO  X2  X3
04306          SUBTRACT +1             FROM  X1
04307          GO TO 8680-C-D-LOOP-3.
04308
04309  8690-C-D-X.
04310      EXIT.
04311  EJECT
04312  8800-UNAUTHORIZED-ACCESS.
04313      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
04314
04315      GO TO 8300-SEND-TEXT.
04316
04317  8810-PF23.
04318      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
04319      MOVE XCTL-005               TO  PGM-NAME.
04320
04321      GO TO 9300-XCTL.
04322
04323  8900-SYNCPOINT-ROLLBACK.
04324      
      * EXEC CICS SYNCPOINT
04325 *        ROLLBACK
04326 *    END-EXEC.
      *    MOVE '6"R                   !   #00009107' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04327
04328      
      * EXEC CICS DUMP
04329 *        DUMPCODE  ('LGXX')
04330 *        TASK
04331 *    END-EXEC.
           MOVE 'LGXX' TO DFHEIV5
      *    MOVE '<"   T                $   #00009111' TO DFHEIV0
           MOVE X'3C2220202054202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04332
04333  8900-EXIT.
04334      EXIT.
04335
04336  9100-RETURN-TRAN.
04337      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
04338      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
04339
04340      
      * EXEC CICS RETURN
04341 *        TRANSID   (TRANS-ID)
04342 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
04343 *        LENGTH    (PI-COMM-LENGTH)
04344 *    END-EXEC.
      *    MOVE '.(CT                  &   #00009123' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04345
04346  9200-RETURN-MAIN-MENU.
04347      MOVE XCTL-626               TO  PGM-NAME.
04348
04349      GO TO 9300-XCTL.
04350
04351  9300-XCTL.
04352      
      * EXEC CICS XCTL
04353 *        PROGRAM   (PGM-NAME)
04354 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
04355 *        LENGTH    (PI-COMM-LENGTH)
04356 *    END-EXEC.
      *    MOVE '.$C                   $   #00009135' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04357
04358  9400-CLEAR.
04359      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME
04360
04361      GO TO 9300-XCTL.
04362
04363  9500-PF12.
04364      MOVE XCTL-010               TO  PGM-NAME.
04365
04366      GO TO 9300-XCTL.
04367  EJECT
04368  9600-PGMID-ERROR.
04369      
      * EXEC CICS HANDLE CONDITION
04370 *        PGMIDERR  (8300-SEND-TEXT)
04371 *    END-EXEC.
      *    MOVE '"$L                   ! 5 #00009152' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303039313532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04372
04373      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
04374      MOVE ' '                    TO  PI-ENTRY-CD-1.
04375      MOVE XCTL-005               TO  PGM-NAME.
04376      MOVE PGM-NAME               TO  LOGOFF-PGM.
04377      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
04378
04379      GO TO 9300-XCTL.
04380
04381  9900-ERROR-FORMAT.
04382      IF NOT  EMI-ERRORS-COMPLETE
04383          MOVE LINK-001           TO  PGM-NAME
04384          
      * EXEC CICS LINK
04385 *            PROGRAM   (PGM-NAME)
04386 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
04387 *            LENGTH    (EMI-COMM-LENGTH)
04388 *        END-EXEC.
      *    MOVE '."C                   ''   #00009167' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04389
04390  9900-EXIT.
04391      EXIT.
04392
04393  9990-ABEND.
04394      MOVE LINK-004               TO  PGM-NAME.
04395      MOVE DFHEIBLK               TO  EMI-LINE1
04396
04397      
      * EXEC CICS LINK
04398 *        PROGRAM   (PGM-NAME)
04399 *        COMMAREA  (EMI-LINE1)
04400 *        LENGTH    (72)
04401 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00009180' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04402
04403      IF PI-MAP-NAME  =  EL642A
04404          MOVE -1                 TO  APFNTERL
04405      ELSE
04406          MOVE -1                 TO  BPFNTERL.
04407
04408      GO TO 8200-SEND-DATAONLY.
04409
04410  9995-SECURITY-VIOLATION.
04411 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00009211' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL642' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0890-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3930-BILL-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3940-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 4010-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 4110-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4290-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 4370-ACCOUNT-INVALID
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 4510-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4610-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 6070-NO-COMP-MSTR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 6110-NO-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 6410-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 6610-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 6660-START-BROWSE,
                     6700-COMM-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 6700-COMM-EXIT,
                     6700-COMM-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 6700-COMM-EXIT,
                     6700-COMM-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 6710-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 6880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL642' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
