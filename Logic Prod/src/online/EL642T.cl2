00001  IDENTIFICATION DIVISION.                                         02/06/97
00002                                                                   EL642
00003  PROGRAM-ID.                 EL642 .                                 LV031
00004 *              PROGRAM CONVERTED BY                                  CL*26
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*26
00006 *              CONVERSION DATE 01/06/95 10:16:47.                    CL*26
00007 *                            VMOD=2.031                              CL*31
00008 *                                                                 EL642
00008 *                                                                 EL642
00009 *AUTHOR.     LOGIC,INC.                                              CL*26
00010 *            DALLAS, TEXAS.                                          CL*26
00011                                                                   EL642
00012 *DATE-COMPILED.                                                      CL*26
00013                                                                      CL**6
00014 *SECURITY.   *****************************************************   CL*26
00015 *            *                                                   *   CL*26
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*26
00017 *            *                                                   *   CL*26
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*26
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*26
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*26
00021 *            *                                                   *   CL*26
00022 *            *****************************************************   CL*26
00023 *                                                                 EL642
00024 *REMARKS.                                                            CL**5
00025 *        TRANSACTION - EXH7 - GENERAL AGENT BILLING                  CL**5
00026 *                           (PREPARE AN AGENT STATEMENT).            CL**5
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

00027                                                                   EL642
00028  ENVIRONMENT DIVISION.                                               CL**5
00029  EJECT                                                               CL**5
00030  DATA DIVISION.                                                   EL642
00031  WORKING-STORAGE SECTION.                                         EL642
00032                                                                   EL642
00033  77  FILLER  PIC X(32)  VALUE '********************************'. EL642
00034  77  FILLER  PIC X(32)  VALUE '*    EL642 WORKING STORAGE     *'. EL642
00035  77  FILLER  PIC X(32)  VALUE '***********VMOD 2.031 **********'.    CL*31
00036                                                                   EL642
00037      COPY ELCSCTM.                                                   CL*22
00038      COPY ELCSCRTY.                                                  CL*22
00039  EJECT                                                               CL**5
00040  01  STANDARD-AREAS.                                              EL642
00041      12  SC-ITEM             PIC S9(4)   COMP    VALUE +1.           CL**6
00042                                                                      CL*23
00043      12  W-TRANSFER-CONTROL.                                         CL*23
00044          16  WT-CR-CARRIER           PIC X.                          CL*23
00045          16  WT-CR-GROUPING          PIC X(6).                       CL*23
00046          16  WT-CR-STATE             PIC XX.                         CL*23
00047          16  WT-CR-ACCOUNT           PIC X(10).                      CL*23
00048          16  WT-CR-FIN-RESP          PIC X(10).                      CL*23
00049          16  WT-CR-TYPE              PIC X.                          CL*23
00050                                                                      CL*23
00051      12  GETMAIN-SPACE       PIC  X              VALUE SPACE.        CL**6
00052      12  MAP-NAME            PIC  X(8)           VALUE 'EL642A'.     CL**6
00053      12  MAPSET-NAME         PIC  X(8)           VALUE 'EL642S'.     CL**6
00054      12  SCREEN-NUMBER       PIC  X(4)           VALUE '642A'.       CL**6
00055      12  TRANS-ID            PIC  X(4)           VALUE 'EXH7'.       CL**6
00056      12  EL633-TRANS-ID      PIC  X(4)           VALUE 'EXB7'.       CL*23
00057      12  EL633DMD-TRANS-ID   PIC  X(4)           VALUE 'EX1F'.       CL*26
00058      12  EL635-TRANS-ID      PIC  X(4)           VALUE 'EXJ4'.       CL*23
00059      12  EL650-TRANS-ID      PIC  X(4)           VALUE 'EXC4'.       CL*23
00060      12  EL652-TRANS-ID      PIC  X(4)           VALUE 'EXD4'.       CL*23
00061      12  EL658-TRANS-ID      PIC  X(4)           VALUE 'EXJ3'.       CL*23
00062      12  THIS-PGM            PIC  X(8)           VALUE 'EL642'.      CL**6
00063      12  EL642A              PIC  X(8)           VALUE 'EL642A'.     CL**6
00064      12  EL642B              PIC  X(8)           VALUE 'EL642B'.     CL**6
00065      12  PGM-NAME            PIC  X(8).                              CL**6
00066      12  TIME-IN             PIC S9(7).                              CL**6
00067      12  TIME-OUT-R  REDEFINES  TIME-IN.                             CL**5
00068          16  FILLER          PIC  X.                                 CL**6
00069          16  TIME-OUT        PIC  99V99.                             CL**6
00070          16  FILLER          PIC  XX.                                CL**6
00071      12  XCTL-005            PIC  X(8)           VALUE 'EL005'.      CL**6
00072      12  XCTL-010            PIC  X(8)           VALUE 'EL010'.      CL**6
00073      12  XCTL-626            PIC  X(8)           VALUE 'EL626'.      CL**6
00074      12  XCTL-633            PIC  X(8)           VALUE 'EL633'.      CL**6
00075      12  XCTL-633DMD         PIC  X(8)           VALUE 'EL633DMD'.   CL*26
00076      12  XCTL-635            PIC  X(8)           VALUE 'EL635'.      CL**9
00077      12  XCTL-PYAJ           PIC  X(8)           VALUE 'EL633'.      CL**9
00078      12  XCTL-650            PIC  X(8)           VALUE 'EL650'.      CL**6
00079      12  XCTL-652            PIC  X(8)           VALUE 'EL652'.      CL**6
00080      12  XCTL-6401           PIC  X(8)           VALUE 'EL6401'.     CL**6
00081      12  XCTL-658            PIC  X(8)           VALUE 'EL658'.      CL**6
00082      12  LINK-001            PIC  X(8)           VALUE 'EL001'.      CL**6
00083      12  LINK-004            PIC  X(8)           VALUE 'EL004'.      CL**6
00084      12  LINK-ELDATCV        PIC  X(8)           VALUE 'ELDATCV'.    CL**6
00085      12  ERPNDB-ALT-FILE-ID  PIC  X(8)           VALUE 'ERPNDB2'.    CL**6
00086      12  ERPNDB-FILE-ID      PIC  X(8)           VALUE 'ERPNDB'.     CL**6
00087      12  ELCNTL-FILE-ID      PIC  X(8)           VALUE 'ELCNTL'.     CL**6
00088      12  ERCOMP-FILE-ID      PIC  X(8)           VALUE 'ERCOMP'.     CL**6
00089      12  ERPYAJ-FILE-ID      PIC  X(8)           VALUE 'ERPYAJ'.     CL**6
00090      12  ERACCT-ALT-FILE-ID  PIC  X(8)           VALUE 'ERACCT2'.    CL**6
00091      12  ERACCT-FILE-ID      PIC  X(8)           VALUE 'ERACCT'.     CL**6
00092      12  ERBILL-FILE-ID      PIC  X(8)           VALUE 'ERBILL'.     CL**6
00093      12  ERGXRF-FILE-ID      PIC  X(8)           VALUE 'ERGXRF'.     CL**6
00094      12  ERCOMM-FILE-ID      PIC  X(8)           VALUE 'ERCOMM'.     CL**6
00095      12  ERCTBL-FILE-ID      PIC  X(8)           VALUE 'ERCTBL'.     CL**6
00096      12  ERRESC-FILE-ID      PIC  X(8)           VALUE 'ERRESC'.     CL*27
00097      12  RETURNED-FROM       PIC  X(8)           VALUE SPACES.       CL**6
00098      12  QID.                                                     EL642
00099          16  QID-TERM        PIC  X(4).                              CL**6
00100          16  FILLER          PIC  X(4)           VALUE '642A'.       CL**6
00101                                                                   EL642
00102  01  WORK-AREA.                                                   EL642
00103      12  FIRST-TIME-SW               PIC  X      VALUE 'Y'.          CL**6
00104          88  FIRST-TIME                          VALUE 'Y'.          CL**5
00105      12  COMPENSATION-SW             PIC  X      VALUE ' '.          CL**6
00106          88  GENERAL-AGENT                       VALUE 'G'.          CL**5
00107      12  LIFE-INDICATOR              PIC  X      VALUE ' '.          CL**6
00108          88  JOINT-LIFE                          VALUE 'J'.          CL**5
00109      12  BENEFIT-MASTER-FOUND-SW     PIC  X      VALUE ' '.          CL**6
00110          88  BENEFIT-MASTER-FOUND                VALUE 'Y'.          CL**5
00111      12  COMMISSION-TBL-SW           PIC  X      VALUE ' '.          CL**6
00112          88  COMMISSION-TBL-PRESENT              VALUE 'Y'.          CL**5
00113      12  INVALID-ACCOUNT-SW          PIC  X      VALUE ' '.          CL**6
00114          88  INVALID-ACCOUNT                     VALUE 'Y'.          CL**5
00115      12  NEW-ACCOUNT-SW              PIC  X      VALUE ' '.          CL**6
00116          88  NEW-ACCOUNT                         VALUE 'Y'.          CL**5
00117      12  VALID-BILL-TYPE-VALUES      PIC  X      VALUE SPACE.        CL**6
00118          88  VALID-BILL-TYPE                     VALUE  '1' '2'      CL**5
00119                                                     '3' '4' '5'.     CL**5
00120      12  BILL-BATCH-TRAILER-SW       PIC  X      VALUE SPACE.        CL**6
00121          88  BILL-BATCH-TRAILER                  VALUE 'Y'.          CL**5
00122      12  ERCOMM-FOUND-SW             PIC  X      VALUE SPACE.        CL**6
00123          88  ERCOMM-FOUND                        VALUE 'Y'.          CL**5
00124          88  ERCOMM-NOT-FOUND                    VALUE 'N'.          CL**5
00125      12  ERCTBL-FOUND-SW             PIC  X      VALUE SPACE.        CL**6
00126          88  ERCTBL-FOUND                        VALUE 'Y'.          CL**5
00127          88  ERCTBL-NOT-FOUND                    VALUE 'N'.          CL**5
00128      12  GXRF-EOF-SW                 PIC  X      VALUE SPACE.        CL**6
00129          88  GXRF-EOF                            VALUE 'Y'.          CL**5
00130      12  PNDB-EOF-SW                 PIC  X      VALUE SPACE.        CL**6
00131          88  PNDB-EOF                            VALUE 'Y'.          CL**5
00132      12  PYAJ-EOF-SW                 PIC  X      VALUE SPACE.        CL**6
00133          88  PYAJ-EOF                            VALUE 'Y'.          CL**5
00134      12  COMP-UPDATE-SW              PIC  X      VALUE SPACE.        CL**6
00135          88  UPDATE-COMP-TOTALS                  VALUE 'Y'.          CL**5
00136      12  LIMIT-BILLING-SW            PIC  X      VALUE SPACE.        CL**6
00137          88  LIMIT-BILLING                       VALUE 'Y'.          CL**5
00138      12  WS-PROCESS-SW               PIC  X      VALUE SPACE.        CL**6
00139          88  DO-NOT-BILL-THIS-ACCT               VALUE 'Y'.          CL**5
00140      12  NO-BILL-REC-SW              PIC  X      VALUE SPACE.        CL**6
00141          88  NO-BILL-RECS                        VALUE 'Y'.          CL**5
00142      12  DATA-VOIDED-SW              PIC  X      VALUE SPACE.        CL**6
00143          88  DATA-VOIDED                         VALUE 'Y'.          CL**5
00144      12  BATCHES-PROCESSED-SW        PIC  X      VALUE SPACE.        CL**6
00145          88  MORE-THAN-6-BATCHES                 VALUE 'Y'.          CL**5
00146      12  ACCOUNT-CARRY-BAL-SW        PIC  X      VALUE SPACE.        CL**6
00147          88  ACCOUNT-CARRY-BAL                   VALUE 'Y'.          CL**5
00148      12  BILLING-DETAIL-TYPE         PIC  XX VALUE SPACES.           CL**6
00149          88  TOTAL-STATEMENT                     VALUE 'TS'.         CL**5
00150          88  TOTAL-ACCOUNT                       VALUE 'TA'.         CL**5
00151          88  TOTAL-BOTH                          VALUE 'TB'.         CL*25
00152          88  GEN-AGT-ADDRESS                     VALUE 'GA'.         CL**5
00153      12  ELCNTL-UPDATE-SW            PIC  X      VALUE SPACE.        CL**6
00154          88  ELCNTL-UPDATE                       VALUE 'Y'.          CL**5
00155      12  WS-BILL-TYPE                PIC  X      VALUE SPACE.        CL**6
00156          88  WS-UPDATE-FILES                     VALUE '3' '4'       CL**5
00157                                                        '5'.          CL**5
00158      12  SUB                     PIC S999 COMP-3 VALUE ZEROS.        CL*15
00159      12  ACCOM-SUB               PIC  99 COMP VALUE ZEROS.           CL**6
00160      12  GX-SUB                  PIC S9(4) COMP VALUE ZEROS.         CL**6
00161      12  COMP-SUB                PIC S9(4) COMP VALUE ZEROS.         CL**6
00162      12  CE-SUB                  PIC S9(4) COMP VALUE ZEROS.         CL**6
00163      12  BEN-SUB                 PIC S9(4) COMP VALUE ZEROS.         CL**6
00164      12  WORK-SEQ-NO             PIC S9(9) COMP-3.                   CL**6
00165      12  WS-ERROR-MSG.                                            EL642
00166          16  WS-ERROR-TEXT       PIC  X(35)      VALUE SPACES.       CL**5
00167          16  WS-ERROR-BATCH      PIC  X(6)       VALUE SPACES.       CL**6
00168          16  FILLER              PIC  X(24)      VALUE SPACES.       CL**5
00169      12  WS-WORK-DATE.                                            EL642
00170          16  WS-MONTH            PIC  99         VALUE ZEROS.        CL**6
00171          16  WS-DAY              PIC  99         VALUE ZEROS.        CL**6
00172          16  WS-YEAR             PIC  99         VALUE ZEROS.        CL**6
00173      12  WS-MONTH-END-DATE.                                       EL642
00174          16  WS-ME-YEAR          PIC  99         VALUE ZEROS.        CL**6
00175          16  WS-ME-MONTH         PIC  99         VALUE ZEROS.        CL**6
00176          16  WS-ME-DAY           PIC  99         VALUE ZEROS.        CL**6
00177      12  WS-CURRENT-DATE         PIC  XX         VALUE SPACES.       CL**6
00178      12  WS-CURRENT-DATE-MDY     PIC  X(6)       VALUE SPACES.       CL**6
00179      12  WS-CURRENT-DATE-EDIT    PIC  X(8)       VALUE SPACES.       CL**6
00180      12  WS-PREV-BILL-DATE       PIC  XX     VALUE LOW-VALUES.       CL**6
00181      12  MONTHS-DIFF-LF          PIC S9(05).                         CL*15
00182      12  MONTHS-DIFF-AH          PIC S9(05).                         CL*15
00183      12  WS-LF-CANCEL-DATE.                                          CL*15
00184          16  WS-LF-CANCEL-YR     PIC 99.                             CL*31
00185          16  WS-LF-CANCEL-MO     PIC 99.                             CL*31
00186          16  WS-LF-CANCEL-DA     PIC 99.                             CL*31
00187      12  WS-AH-CANCEL-DATE.                                          CL*15
00188          16  WS-AH-CANCEL-YR     PIC 99.                             CL*31
00189          16  WS-AH-CANCEL-MO     PIC 99.                             CL*31
00190          16  WS-AH-CANCEL-DA     PIC 99.                             CL*31
00191      12  WS-EFFECT-DATE.                                             CL*15
00192          16  WS-EFFECT-YR        PIC 99.                             CL*31
00193          16  WS-EFFECT-MO        PIC 99.                             CL*31
00194          16  WS-EFFECT-DA        PIC 99.                             CL*31
00195      12  WS-BEGIN-DATE.                                           EL642
00196          16  FILLER              PIC  X(3)       VALUE SPACES.       CL**6
00197          16  WS-BEGIN-DAY        PIC  XX         VALUE SPACES.       CL**6
00198          16  FILLER              PIC  X(3)       VALUE SPACES.       CL**6
00199      12  PRINT-CONTROL.                                           EL642
00200          16  SINGLE-SPACE        PIC  X          VALUE SPACE.        CL**6
00201          16  DOUBLE-SPACE        PIC  X          VALUE ZERO.         CL**6
00202          16  TRIPLE-SPACE        PIC  X          VALUE '-'.          CL**6
00203          16  SUPPRESS-SPACE      PIC  X          VALUE '+'.          CL**6
00204          16  TOP-OF-PAGE         PIC  X          VALUE '1'.          CL**6
00205      12  WS-EXP-DATE.                                             EL642
00206          16  WS-SAV-EXP-DT   OCCURS  10  TIMES                       CL**5
00207                  INDEXED BY  DTNDX   PIC  XX.                        CL**6
00208      12  WS-REMIT-TO.                                             EL642
00209          16  WS-SAV-REMIT-TO     OCCURS  10  TIMES                   CL**5
00210                  INDEXED BY  RTNDX   PIC  X(6).                      CL**6
00211      12  WORK-REMIT-TO           PIC  X(6)       VALUE SPACES.       CL**6
00212      12  WS-REMITTED             PIC S9(6)V99    VALUE ZEROS.        CL**6
00213      12  WS-LINECTR              PIC  99         VALUE ZEROS.        CL**6
00214      12  WS-PGECTR               PIC  9(3)       VALUE 1.            CL**6
00215      12  WS-LINE-SEQ-NO          PIC S9(4) COMP VALUE +1.            CL**6
00216      12  WS-CARR-COMP.                                            EL642
00217          16  WS-CARRIER          PIC  X          VALUE SPACE.        CL**6
00218          16  WS-COMP             PIC  X(3)       VALUE SPACES.       CL**6
00219      12  WORK-AMT                PIC S9(7)V99    VALUE ZEROS.        CL**6
00220      12  JOURNAL-LENGTH          PIC S9(4) COMP VALUE ZERO.          CL**6
00221      12  LF-COMM-TBL-NO          PIC  X(3)       VALUE SPACES.       CL**6
00222      12  JT-COMM-TBL-NO          PIC  X(3)       VALUE SPACES.       CL**6
00223      12  AH-COMM-TBL-NO          PIC  X(3)       VALUE SPACES.       CL**6
00224      12  AGENT-OWES              PIC  X(18)      VALUE               CL**5
00225              'AGENT OWES       ='.                                   CL**5
00226      12  OWED-TO-AGENT           PIC  X(18)      VALUE               CL**5
00227              'OWED TO AGENT    ='.                                   CL**5
00228      12  WS-PY-ENTRY-COMMENT.                                     EL642
00229          16  WS-PY-CURRENT-DATE  PIC  X(6).                          CL**6
00230          16  FILLER              PIC  X(14)      VALUE               CL**5
00231                  ' CHK TO AGENT'.                                    CL**5
00232      12  WS-ACCT-NOT-BILLED      PIC  X(18)      VALUE               CL**5
00233              'ACCOUNT NOT BILLED'.                                   CL**5
00234  EJECT                                                               CL**5
00235      12  WS-AGENT-ADDR-AREA.                                      EL642
00236          16  WS-AGENT-LINES  OCCURS  6  TIMES                        CL**5
00237                  INDEXED BY  A-INDX.                                 CL**5
00238              20  WS-AGENT-ZIP.                                       CL**5
00239                  24  WS-AGENT-1ST-ZIP    PIC  X(4).                  CL**6
00240                  24  WS-AGENT-2ND-ZIP    PIC  X(5).                  CL**6
00241              20  FILLER                  PIC  X(12).                 CL**5
00242              20  WS-A-LAST-ZIP.                                      CL**5
00243                  24  WS-A-LAST-1ST-ZIP   PIC  X(4).                  CL**6
00244                  24  WS-A-LAST-2ND-ZIP   PIC  X(5).                  CL**6
00245      12  WS-COMPENSATION-WORK-AREA.                                  CL**5
00246          16  WS-COMM-CK-AMT      PIC S9(5)V99 COMP-3 VALUE +0.       CL**6
00247          16  WS-COMM-AGE         PIC  99.                            CL**6
00248          16  WS-COMM-TERM        PIC  9(3).                          CL**6
00249          16  WS-LF-ISS-COMP      PIC S9(5)V99 COMP-3 VALUE +0.       CL**6
00250          16  WS-AH-ISS-COMP      PIC S9(5)V99 COMP-3 VALUE +0.       CL**6
00251          16  WS-I-LF-PREMIUM-AMT PIC S9(7)V99 COMP-3 VALUE +0.       CL**6
00252          16  WS-I-AH-PREMIUM-AMT PIC S9(7)V99 COMP-3 VALUE +0.       CL**6
00253          16  WS-I-LF-BENEFIT-AMT PIC S9(9)V99 COMP-3 VALUE +0.       CL**6
00254          16  WS-I-AH-BENEFIT-AMT PIC S9(9)V99 COMP-3 VALUE +0.       CL**6
00255          16  WS-C-LF-CANCEL-AMT  PIC S9(7)V99 COMP-3 VALUE +0.       CL**6
00256          16  WS-C-AH-CANCEL-AMT  PIC S9(7)V99 COMP-3 VALUE +0.       CL**6
00257          16  WS-WK-RATE          PIC SV9(5)    COMP-3 VALUE +.0.     CL**6
00258  EJECT                                                               CL**5
00259      12  WS-ACCOUNT-TOTALS       COMP-3.                             CL**5
00260          16  WS-ACCT-BEG-BAL         PIC S9(7)V99 VALUE ZEROS.       CL**6
00261          16  WS-ACCT-NET-PREM        PIC S9(7)V99 VALUE ZEROS.       CL**6
00262          16  WS-ACCT-COMP            PIC S9(7)V99 VALUE ZEROS.       CL**6
00263          16  WS-ACCT-PAY-ADJS        PIC S9(7)V99 VALUE ZEROS.       CL**6
00264          16  WS-ACCT-UNPAID-NET-PREM PIC S9(7)V99 VALUE ZEROS.       CL**6
00265          16  WS-ACCT-LF-OVERWRITE    PIC S9(7)V99 VALUE ZEROS.       CL**6
00266          16  WS-ACCT-AH-OVERWRITE    PIC S9(7)V99 VALUE ZEROS.       CL**6
00267      12  WS-GENERAL-AGENT-TOTALS COMP-3.                             CL**5
00268          16  WS-GA-BEG-BAL           PIC S9(7)V99 VALUE ZEROS.       CL**6
00269          16  WS-GA-NET-PREM          PIC S9(7)V99 VALUE ZEROS.       CL**6
00270          16  WS-GA-COMP              PIC S9(7)V99 VALUE ZEROS.       CL**6
00271          16  WS-GA-PAY-ADJS          PIC S9(7)V99 VALUE ZEROS.       CL**6
00272          16  WS-GA-UNPAID-NET-PREM   PIC S9(7)V99 VALUE ZEROS.       CL**6
00273          16  WS-GA-LF-OVERWRITE      PIC S9(7)V99 VALUE ZEROS.       CL**6
00274          16  WS-GA-AH-OVERWRITE      PIC S9(7)V99 VALUE ZEROS.       CL**6
00275          16  WS-GA-END-BAL           PIC S9(7)V99 VALUE ZEROS.       CL**6
00276          16  WS-GA-AMT-DUE           PIC S9(7)V99 VALUE ZEROS.       CL**6
00277      12  WS-GENERAL-AGENT-COMMISSIONS.                            EL642
00278          16  WS-GA-LEVEL         PIC S99     COMP   VALUE +0.        CL**6
00279          16  WS-GA-LF-COM        PIC SV9(5) COMP-3 VALUE +.0.        CL**6
00280          16  WS-GA-AH-COM        PIC SV9(5) COMP-3 VALUE +.0.        CL**6
00281  EJECT                                                               CL**5
00282  01  ACCESS-KEYS.                                                 EL642
00283      12  ERPNDB-PRIME-KEY.                                        EL642
00284          16  ERPNDB-CO-CD        PIC  X          VALUE SPACES.       CL**6
00285          16  ERPNDB-BATCH        PIC  X(6)       VALUE SPACES.       CL**6
00286          16  ERPNDB-SEQ-NO       PIC S9(4) COMP.                     CL**6
00287          16  ERPNDB-CHG-SEQ-NO   PIC S9(4) COMP.                     CL**6
00288      12  ERPNDB-LENGTH           PIC S9(4) COMP VALUE +585.          CL**6
00289      12  ERPNDB-ALT-KEY.                                          EL642
00290          16  ERPNDB-CO-CD-A1     PIC  X          VALUE SPACES.       CL**6
00291          16  ERPNDB-CARR         PIC  X          VALUE SPACES.       CL**6
00292          16  ERPNDB-GROUP        PIC  X(6)       VALUE SPACES.       CL**6
00293          16  ERPNDB-STATE        PIC  XX         VALUE SPACES.       CL**6
00294          16  ERPNDB-ACCT         PIC  X(10)      VALUE SPACES.       CL**5
00295          16  ERPNDB-EFF-DT       PIC  XX         VALUE SPACES.       CL**6
00296          16  ERPNDB-CERT.                                         EL642
00297              20  ERPNDB-CERT-PRM PIC  X(10)      VALUE SPACES.       CL**5
00298              20  ERPNDB-CERTSFX  PIC  X          VALUE SPACES.       CL**6
00299          16  ERPNDB-ACHG-SEQ-NO  PIC S9(4) COMP.                     CL**6
00300          16  ERPNDB-REC-TYPE     PIC  X          VALUE SPACES.       CL**6
00301      12  ELCNTL-KEY.                                              EL642
00302          16  ELCNTL-COMPANY-ID   PIC  X(3)       VALUE SPACES.       CL**6
00303          16  ELCNTL-REC-TYPE     PIC  X          VALUE SPACES.       CL**6
00304          16  ELCNTL-FILLER       PIC  X(3)       VALUE SPACES.       CL**6
00305          16  ELCNTL-CARRIER      PIC  X          VALUE SPACES.       CL**6
00306          16  ELCNTL-SEQ-NO       PIC S9(4) COMP VALUE ZEROS.         CL**6
00307      12  ELCNTL-BENEFIT-KEY.                                      EL642
00308          16  CLBENF-COMPANY-ID   PIC  X(3)       VALUE SPACES.       CL**6
00309          16  CLBENF-REC-TYPE     PIC  X          VALUE SPACES.       CL**6
00310          16  CLBENF-ACCESS.                                       EL642
00311              20  FILLER          PIC  XX         VALUE SPACES.       CL**6
00312              20  CLBENF-CD       PIC  XX         VALUE SPACES.       CL**6
00313          16  CLBENF-SEQ-NO       PIC S9(4) COMP VALUE ZEROS.         CL**6
00314      12  ELCNTL-LENGTH           PIC S9(4) COMP VALUE +504.          CL**6
00315      12  ERCOMP-KEY.                                              EL642
00316          16  ERCOMP-COMP-CD      PIC  X          VALUE SPACE.        CL**6
00317          16  ERCOMP-CARRIER      PIC  X          VALUE SPACES.       CL**6
00318          16  ERCOMP-GROUPING     PIC  X(6)       VALUE SPACES.       CL**6
00319          16  ERCOMP-FIN-RESP     PIC  X(10)      VALUE SPACES.       CL**5
00320          16  ERCOMP-ACCT         PIC  X(10)      VALUE SPACES.       CL**5
00321          16  ERCOMP-RECORD-TYPE  PIC  X          VALUE SPACES.       CL**6
00322      12  ERPYAJ-BROWSE-COMP-KEY.                                  EL642
00323          16  ERPYAJ-BR-COMP-CD   PIC  X          VALUE SPACE.        CL**6
00324          16  ERPYAJ-BR-CARRIER   PIC  X          VALUE SPACES.       CL**6
00325          16  ERPYAJ-BR-GROUPING  PIC  X(6)       VALUE SPACES.       CL**6
00326          16  ERPYAJ-BR-FIN-RESP  PIC  X(10)      VALUE SPACES.       CL**5
00327          16  ERPYAJ-BR-ACCOUNT   PIC  X(10)      VALUE SPACES.       CL**5
00328      12  ERCOMP-PREV-KEY         PIC  X(29)      VALUE SPACES.       CL**5
00329      12  ERCOMP-LENGTH           PIC S9(4) COMP VALUE +700.          CL*22
00330      12  ERPYAJ-KEY.                                              EL642
00331          16  ERPYAJ-COMP-CD      PIC  X          VALUE SPACE.        CL**6
00332          16  ERPYAJ-CARRIER      PIC  X          VALUE SPACES.       CL**6
00333          16  ERPYAJ-GROUPING     PIC  X(6)       VALUE SPACES.       CL**6
00334          16  ERPYAJ-FIN-RESP     PIC  X(10)      VALUE SPACES.       CL**5
00335          16  ERPYAJ-ACCOUNT      PIC  X(10)      VALUE SPACES.       CL**5
00336          16  ERPYAJ-FILE-SEQ-NO  PIC S9(8) COMP VALUE +0.            CL**6
00337          16  ERPYAJ-RECORD-TYPE  PIC  X          VALUE SPACES.       CL**6
00338      12  ERPYAJ-LENGTH           PIC S9(4) COMP VALUE +200.          CL**8
00339      12  ERACCT-PRIME-KEY.                                        EL642
00340          16  ERACCT-P-CO-CD      PIC  X          VALUE SPACES.       CL**6
00341          16  ERACCT-P-CARRIER    PIC  X          VALUE SPACES.       CL**6
00342          16  ERACCT-P-GROUPING   PIC  X(6)       VALUE SPACES.       CL**6
00343          16  ERACCT-P-STATE      PIC  XX         VALUE SPACES.       CL**6
00344          16  ERACCT-P-ACCOUNT    PIC  X(10)      VALUE SPACES.       CL**5
00345          16  ERACCT-P-EXP-DATE   PIC  XX         VALUE SPACES.       CL**6
00346          16  FILLER              PIC  X(4)   VALUE LOW-VALUES.       CL**6
00347      12  ERACCT-LENGTH           PIC S9(4) COMP VALUE +2000.         CL**6
00348      12  ERACCT-ALT-KEY.                                          EL642
00349          16  ERACCT-A-CO-CD      PIC  X          VALUE SPACES.       CL**6
00350          16  ERACCT-A-CARRIER    PIC  X          VALUE SPACES.       CL**6
00351          16  ERACCT-A-GROUPING   PIC  X(6)       VALUE SPACES.       CL**6
00352          16  ERACCT-A-STATE      PIC  XX         VALUE SPACES.       CL**6
00353          16  ERACCT-A-ACCOUNT    PIC  X(10)      VALUE SPACES.       CL**5
00354          16  ERACCT-A-EXP-DATE   PIC  XX         VALUE SPACES.       CL**6
00355          16  FILLER              PIC  X(4)   VALUE LOW-VALUES.       CL**6
00356      12  ERBILL-KEY.                                              EL642
00357          16  ERBILL-CO-CD        PIC  X.                             CL**6
00358          16  ERBILL-CARRIER      PIC  X.                             CL**6
00359          16  ERBILL-GROUP        PIC  X(6).                          CL**6
00360          16  ERBILL-ACCT         PIC  X(10).                         CL**5
00361          16  ERBILL-FIN-RESP     PIC  X(10).                         CL**5
00362          16  ERBILL-REC-TYPE     PIC  X.                             CL**6
00363          16  ERBILL-LINE-SEQ-NO  PIC S9(4) COMP.                     CL**6
00364      12  ERBILL-LENGTH           PIC S9(4) COMP VALUE +210.          CL**6
00365      12  ERGXRF-KEY.                                              EL642
00366          16  ERGXRF-COMPANY-CD   PIC  X.                             CL**6
00367          16  ERGXRF-CARRIER      PIC  X.                             CL**6
00368          16  ERGXRF-GROUPING     PIC  X(6).                          CL**6
00369          16  ERGXRF-AGENT-NO     PIC  X(10).                         CL**5
00370      12  ERGXRF-FIXED-LENGTH     PIC S9(4) COMP VALUE +64.           CL**6
00371      12  ERGXRF-VAR-LENGTH       PIC S9(4) COMP VALUE +24.           CL**6
00372      12  ERGXRF-LENGTH           PIC S9(4) COMP VALUE ZEROS.         CL**6
00373      12  ERCOMM-KEY.                                              EL642
00374          16  ERCOMM-COMPANY-CD   PIC  X.                             CL**6
00375          16  ERCOMM-CARRIER      PIC  X.                             CL**6
00376          16  ERCOMM-GROUPING     PIC  X(6).                          CL**6
00377          16  ERCOMM-STATE        PIC  XX.                            CL**6
00378          16  ERCOMM-ACCOUNT      PIC  X(10).                         CL**5
00379          16  ERCOMM-CERT-EFF-DT  PIC  XX.                            CL**6
00380          16  ERCOMM-CERT-NO.                                      EL642
00381              20  CE-CERT-PRIME   PIC  X(10).                         CL**5
00382              20  CE-CERT-SFX     PIC  X.                             CL**6
00383      12  ERCOMM-LENGTH           PIC S9(4) COMP VALUE +250.          CL**6
00384      12  ERCTBL-KEY.                                              EL642
00385          16  ERCTBL-COMPANY-CD       PIC  X.                         CL**6
00386          16  ERCTBL-TABLE            PIC  X(3).                      CL**6
00387          16  ERCTBL-CNTRL-2.                                      EL642
00388              20  ERCTBL-BEN-TYPE     PIC  X.                         CL**6
00389              20  ERCTBL-BEN-CODE     PIC  XX.                        CL**6
00390      12  ERCTBL-LENGTH               PIC S9(4) COMP VALUE +200.      CL**6
00391      12  SAVE-ERACCT-PRIME-KEY.                                   EL642
00392          16  SV-ERACCT-P-CO-CD       PIC  X      VALUE SPACES.       CL**6
00393          16  SV-ERACCT-P-CARRIER     PIC  X      VALUE SPACES.       CL**6
00394          16  SV-ERACCT-P-GROUPING    PIC  X(6)   VALUE SPACES.       CL**6
00395          16  SV-ERACCT-P-STATE       PIC  XX     VALUE SPACES.       CL**6
00396          16  SV-ERACCT-P-ACCOUNT     PIC  X(10)  VALUE SPACES.       CL**5
00397          16  SV-ERACCT-P-EXP-DATE    PIC  XX     VALUE SPACES.       CL**6
00398 *****************************************************************    CL*27
00399 *      START OF WORKING STORAGE FOR CLIENT-DMD                  *    CL*27
00400 *****************************************************************    CL*27
00401 *RESIDENT STATE TAXES MASTER(ERRESC)                                 CL*27
00402 *                                                                    CL*27
00403      12  WS-SV-ERRESC-KEY             PIC X(32).                     CL*30
00404                                                                      CL*30
00405      12  WS-ERRESC-KEY.                                              CL*27
00406          16 WS-ERRESC-SEARCH-KEY.                                    CL*27
00407              20  WS-ERRESC-COMPANY-CD PIC  X.                        CL*27
00408              20  WS-ERRESC-CARRIER    PIC  X.                        CL*27
00409              20  WS-ERRESC-GROUP      PIC X(6).                      CL*27
00410              20  WS-ERRESC-STATE      PIC XX.                        CL*27
00411              20  WS-ERRESC-ACCOUNT    PIC X(10).                     CL*27
00412          16 WS-ERRESC-RESIDUAL-KEY.                                  CL*27
00413              20  WS-ERRESC-AGENT      PIC X(10).                     CL*27
00414              20  WS-ERRESC-RES-STATE  PIC XX.                        CL*27
00415              20  WS-ERRESC-EXPIRE-DT  PIC 9(8) COMP-3.               CL*27
00416 *                                          YYYYMMDD                  CL*27
00417      12  WS-COMMISSION                PIC SV9(5) COMP-3.             CL*27
00418      12  WS-SUB1                      PIC    S99 COMP-3.             CL*27
00419      12  WS-SUB                       PIC    S99 COMP-3.             CL*27
00420      12  WS-CONTRACT-DATE             PIC 9(8).                      CL*27
00421      12  WS-COMM-SW                   PIC XX VALUE SPACES.           CL*27
00422          88  AH-COMM                             VALUE 'AH'.         CL*27
00423          88  LF-COMM                             VALUE 'LF'.         CL*27
00424 *****************************************************************    CL*27
00425 *        END OF WORKING STORAGE FOR CLIENT-DMD                  *    CL*27
00426 *****************************************************************    CL*27
00427                                                                      CL*27
00428  EJECT                                                               CL**5
00429  01  ERROR-NUMBERS.                                               EL642
00430      12  ER-0004             PIC  X(4)           VALUE '0004'.       CL**6
00431      12  ER-0008             PIC  X(4)           VALUE '0008'.       CL**6
00432      12  ER-0022             PIC  X(4)           VALUE '0022'.       CL**6
00433      12  ER-0029             PIC  X(4)           VALUE '0029'.       CL**6
00434      12  ER-0070             PIC  X(4)           VALUE '0070'.       CL**6
00435      12  ER-0194             PIC  X(4)           VALUE '0194'.       CL**6
00436      12  ER-0195             PIC  X(4)           VALUE '0195'.       CL**6
00437      12  ER-0196             PIC  X(4)           VALUE '0196'.       CL**6
00438      12  ER-0197             PIC  X(4)           VALUE '0197'.       CL**6
00439      12  ER-2208             PIC  X(4)           VALUE '2208'.       CL**6
00440      12  ER-2210             PIC  X(4)           VALUE '2210'.       CL**6
00441      12  ER-2215             PIC  X(4)           VALUE '2215'.       CL**6
00442      12  ER-2230             PIC  X(4)           VALUE '2230'.       CL**6
00443      12  ER-2233             PIC  X(4)           VALUE '2233'.       CL**6
00444      12  ER-2370             PIC  X(4)           VALUE '2370'.       CL**6
00445      12  ER-2371             PIC  X(4)           VALUE '2371'.       CL**6
00446      12  ER-2249             PIC  X(4)           VALUE '2249'.       CL**6
00447      12  ER-2250             PIC  X(4)           VALUE '2250'.       CL**6
00448      12  ER-2383             PIC  X(4)           VALUE '2383'.       CL**6
00449      12  ER-2385             PIC  X(4)           VALUE '2385'.       CL**6
00450      12  ER-2399             PIC  X(4)           VALUE '2399'.       CL**6
00451      12  ER-2400             PIC  X(4)           VALUE '2400'.       CL**6
00452      12  ER-2401             PIC  X(4)           VALUE '2401'.       CL**6
00453      12  ER-2403             PIC  X(4)           VALUE '2403'.       CL**6
00454      12  ER-2404             PIC  X(4)           VALUE '2404'.       CL**6
00455      12  ER-2405             PIC  X(4)           VALUE '2405'.       CL**6
00456      12  ER-2406             PIC  X(4)           VALUE '2406'.       CL**6
00457      12  ER-2407             PIC  X(4)           VALUE '2407'.       CL**6
00458      12  ER-2408             PIC  X(4)           VALUE '2408'.       CL**6
00459      12  ER-2409             PIC  X(4)           VALUE '2409'.       CL**6
00460      12  ER-2411             PIC  X(4)           VALUE '2411'.       CL**6
00461      12  ER-2421             PIC  X(4)           VALUE '2421'.       CL**6
00462      12  ER-2434             PIC  X(4)           VALUE '2434'.       CL**6
00463      12  ER-2435             PIC  X(4)           VALUE '2435'.       CL**6
00464      12  ER-2436             PIC  X(4)           VALUE '2436'.       CL**6
00465      12  ER-2438             PIC  X(4)           VALUE '2438'.       CL**6
00466      12  ER-2439             PIC  X(4)           VALUE '2439'.       CL**6
00467      12  ER-2443             PIC  X(4)           VALUE '2443'.       CL**6
00468      12  ER-2472             PIC  X(4)           VALUE '2472'.       CL**6
00469      12  ER-2564             PIC  X(4)           VALUE '2564'.       CL**6
00470      12  ER-2570             PIC  X(4)           VALUE '2570'.       CL**6
00471      12  ER-2571             PIC  X(4)           VALUE '2571'.       CL**6
00472      12  ER-2590             PIC  X(4)           VALUE '2571'.       CL**6
00473      12  ER-2597             PIC  X(4)           VALUE '2597'.       CL**6
00474      12  ER-2910             PIC  X(4)           VALUE '2910'.       CL**6
00475      12  ER-2911             PIC  X(4)           VALUE '2911'.       CL**6
00476      12  ER-2912             PIC  X(4)           VALUE '2912'.       CL**6
00477      12  ER-2913             PIC  X(4)           VALUE '2913'.       CL**6
00478      12  ER-2914             PIC  X(4)           VALUE '2914'.       CL**6
00479      12  ER-2915             PIC  X(4)           VALUE '2915'.       CL**6
00480      12  ER-2916             PIC  X(4)           VALUE '2916'.       CL**6
00481      12  ER-2917             PIC  X(4)           VALUE '2917'.       CL**6
00482      12  ER-2918             PIC  X(4)           VALUE '2918'.       CL**6
00483      12  ER-3145             PIC  X(4)           VALUE '3145'.       CL**8
00484      12  ER-3165             PIC  X(4)           VALUE '3165'.       CL*23
00485  EJECT                                                               CL**5
00486  01  CENTER-DATA-WORK-AREA.                                       EL642
00487      12  X1                  PIC S9(4)   COMP    VALUE +0.           CL**6
00488      12  X2                  PIC S9(4)   COMP    VALUE +0.           CL**6
00489      12  X3                  PIC S9(4)   COMP    VALUE +0.           CL**6
00490      12  X-LEN               PIC S9(4)   COMP    VALUE +0.           CL**6
00491      12  CENTER-WORK-1.                                           EL642
00492          16  CW1-PIC         PIC  X      OCCURS  44  TIMES.          CL**6
00493      12  CENTER-WORK-2.                                           EL642
00494          16  CW2-PIC         PIC  X      OCCURS  44  TIMES.          CL**6
00495  EJECT                                                               CL**5
00496  01  GA-REPORT-HEADINGS.                                          EL642
00497      12  GA-PREVIEW-HD.                                           EL642
00498          16  FILLER          PIC  X(44)          VALUE               CL**5
00499                  '                                          **'.     CL**5
00500          16  FILLER          PIC  X(45)          VALUE               CL**5
00501                  '** STATEMENT IS FOR REVIEW PURPOSES ONLY ***'.     CL**5
00502          16  FILLER          PIC  X(44)          VALUE               CL**5
00503                  '*                                           '.     CL**5
00504      12  GA-HD1.                                                  EL642
00505          16  FILLER          PIC  X(44)          VALUE SPACES.       CL**5
00506          16  FILLER          PIC  X(44)          VALUE               CL**5
00507                  '            AGENT    STATEMENT              '.     CL**5
00508          16  FILLER          PIC  X(37)          VALUE SPACES.       CL**5
00509          16  FILLER          PIC  X(7)           VALUE ' EL642 '.    CL**6
00510      12  GA-HD2.                                                  EL642
00511          16  FILLER          PIC  X(43)          VALUE SPACES.       CL**5
00512          16  HD-CO           PIC  X(44)          VALUE SPACES.       CL**5
00513          16  FILLER          PIC  X(37)          VALUE SPACES.       CL**5
00514          16  HD-RUN-DT       PIC  X(8)           VALUE SPACES.       CL**6
00515      12  GA-HD3.                                                  EL642
00516          16  FILLER          PIC  X(57)          VALUE SPACES.       CL**5
00517          16  HD-DT           PIC  X(18)          VALUE SPACES.       CL**5
00518          16  FILLER          PIC  X(37)          VALUE SPACES.       CL**5
00519          16  FILLER          PIC  X(5)           VALUE 'PAGE '.      CL**6
00520          16  HD-PG           PIC ZZ,ZZ9.                          EL642
00521      12  GA-HD4.                                                  EL642
00522          16  FILLER          PIC  X(44)          VALUE               CL**5
00523                  ' ------------- A C C O U N T -------------  '.     CL**5
00524          16  FILLER          PIC  X(44)          VALUE               CL**5
00525                  'BEGINNING      NET       ACCOUNT    PAYMENTS'.     CL**5
00526          16  FILLER          PIC  X(44)          VALUE               CL**5
00527                  '     UNPAID                          AMT.   '.     CL**5
00528      12  GA-HD5.                                                  EL642
00529          16  FILLER          PIC  X(44)          VALUE               CL**5
00530                  '   NUMBER                 NAME              '.     CL**5
00531          16  FILLER          PIC  X(44)          VALUE               CL**5
00532                  ' BALANCE     PREMIUM     COMPENS.   ADJUSTS.'.     CL**5
00533          16  FILLER          PIC  X(44)          VALUE               CL**5
00534                  '  NET PREMIUM  BENEFIT  OVERWRITE    DUE    '.     CL**5
00535  EJECT                                                               CL**5
00536                              COPY ELCDATE.                           CL*13
00537  EJECT                                                               CL**5
00538                              COPY ELCLOGOF.                          CL*13
00539  EJECT                                                               CL**5
00540                              COPY ELCATTR.                           CL*13
00541  EJECT                                                               CL**5
00542                              COPY ELCEMIB.                           CL*13
00543  EJECT                                                               CL**5
00544                              COPY ELCINTF.                           CL*13
00545      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                    CL**5
00546          16  PI-MAP-NAME         PIC  X(8).                          CL**6
00547          16  PI-AGENT-NAME       PIC  X(20).                         CL**5
00548          16  PI-SAV-REMIT-TO     PIC  X(10).                         CL**5
00549          16  PI-SAV-FIN-RESP     PIC  X(10).                         CL**5
00550          16  PI-SAV-ACCT         PIC  X(10).                         CL**5
00551          16  PI-SAV-ACCT-AGT     PIC  X(10).                         CL**5
00552          16  PI-SAV-ACCT-NAME    PIC  X(30).                         CL**5
00553          16  PI-SAV-AGENT        PIC  X(10).                         CL**5
00554          16  PI-SAV-CARR         PIC  X.                             CL**6
00555          16  PI-SAV-GROUP        PIC  X(6).                          CL**6
00556          16  PI-SAV-STATE        PIC  XX.                            CL**6
00557          16  PI-SAV-EXP-DT       PIC  XX.                            CL**6
00558          16  PI-UNPAID-NET-PREM  PIC S9(7)V99    COMP-3.             CL**6
00559          16  PI-COMP-UNPAID-PREM PIC S9(7)V99    COMP-3.             CL**6
00560          16  PI-BAL-FRWD         PIC S9(7)V99    COMP-3.             CL**6
00561          16  PI-PREMIUM          PIC S9(7)V99    COMP-3.             CL**6
00562          16  PI-REMITTED         PIC S9(7)V99    COMP-3.             CL**6
00563          16  PI-TOT-ISS-COMP     PIC S9(7)V99    COMP-3.             CL**6
00564          16  PI-TOT-CAN-COMP     PIC S9(7)V99    COMP-3.             CL**6
00565          16  PI-ADJUSTMNTS       PIC S9(7)V99    COMP-3.             CL**6
00566          16  PI-DISBURSED        PIC S9(7)V99    COMP-3.             CL**6
00567          16  PI-END-BAL          PIC S9(7)V99    COMP-3.             CL**6
00568          16  PI-LF-ISS-COMP      PIC S9(7)V99    COMP-3.             CL**6
00569          16  PI-AH-ISS-COMP      PIC S9(7)V99    COMP-3.             CL**6
00570          16  PI-LF-CAN-COMP      PIC S9(7)V99    COMP-3.             CL**6
00571          16  PI-AH-CAN-COMP      PIC S9(7)V99    COMP-3.             CL**6
00572          16  PI-DUE-FOR-ACCT     PIC S9(7)V99    COMP-3.             CL**6
00573          16  PI-ACCT-BEG-BAL     PIC S9(7)V99    COMP-3.             CL**6
00574          16  PI-ACCT-NET-PREM    PIC S9(7)V99    COMP-3.             CL**6
00575          16  PI-ACCT-COMP        PIC S9(7)V99    COMP-3.             CL**6
00576          16  PI-ACCT-PAY-ADJS    PIC S9(7)V99    COMP-3.             CL**6
00577          16  PI-BILL-TYPE        PIC  X.                             CL**6
00578              88  PI-PREV-BILL                    VALUE '1'.          CL**5
00579              88  PI-PREV-REBILL                  VALUE '2'.          CL**5
00580              88  PI-PREVIEW                      VALUE '1' '2'.      CL**5
00581              88  PI-BILL                         VALUE '3'.          CL**5
00582              88  PI-REBILLING                    VALUE '4'.          CL**5
00583              88  PI-VOID-BILL                    VALUE '5'.          CL**5
00584              88  PI-TOT-REBILL                   VALUE '2' '4'.      CL**5
00585              88  PI-UPDATE-FILES                 VALUE '3' '4'       CL**5
00586                                                        '5'.          CL**5
00587              88  PI-BILLING-FUNCTION             VALUE '1' '2'       CL**5
00588                                                        '3' '4'.      CL**5
00589          16  PI-BILL-ERRS        PIC  X.                             CL**6
00590          16  PI-CHECK-SW         PIC  X.                             CL**6
00591              88  PI-CHECK-PRODUCED               VALUE 'Y'.          CL**5
00592          16  PI-DATA-BILLED-SW   PIC  X.                             CL**6
00593              88  PI-DATA-BILLED                  VALUE 'Y'.          CL**5
00594          16  PI-ACCT-BILLED-SW   PIC  X.                             CL**6
00595              88  PI-ACCT-BILLED                  VALUE 'Y'.          CL**5
00596          16  PI-MONTH-END-DATE.                                   EL642
00597              20  PI-ME-MONTH     PIC  99.                            CL**6
00598              20  FILLER          PIC  X.                             CL**6
00599              20  PI-ME-DAY       PIC  99.                            CL**6
00600              20  FILLER          PIC  X.                             CL**6
00601              20  PI-ME-YEAR      PIC  99.                            CL**6
00602          16  PI-LIMIT-BILLING-ACCOUNTS.                           EL642
00603              20  PI-BILLING-ACCOUNTS     OCCURS  3  TIMES            CL**5
00604                                      PIC  X(10).                     CL**5
00605          16  PI-ISSUE-INFO.                                       EL642
00606              20  PI-ISSUES-BILLED    PIC S9(6) COMP-3.               CL**6
00607              20  PI-ISSUES-INER      PIC S9(6) COMP-3.               CL**6
00608              20  PI-ISSUES-PREV      PIC S9(6) COMP-3.               CL**6
00609          16  PI-CANCEL-INFO.                                      EL642
00610              20  PI-CANCELS-BILLED   PIC S9(6) COMP-3.               CL**6
00611              20  PI-CANCELS-INER     PIC S9(6) COMP-3.               CL**6
00612              20  PI-CANCELS-PREV     PIC S9(6) COMP-3.               CL**6
00613          16  PI-UNKNOWN              PIC S9(6) COMP-3.               CL**6
00614          16  PI-COMP-CONTROL.                                     EL642
00615              20  PI-COMP-CARRIER     PIC  X.                         CL**6
00616              20  PI-COMP-GROUPING    PIC  X(6).                      CL**6
00617              20  PI-COMP-FIN-RESP    PIC  X(10).                     CL**5
00618          16  PI-SCRN-CONTROL.                                        CL*23
00619              20  PI-SCR-CARRIER      PIC  X.                         CL*23
00620              20  PI-SCR-GROUPING     PIC  X(6).                      CL*23
00621              20  PI-SCR-STATE        PIC  XX.                        CL*23
00622              20  PI-SCR-ACCOUNT      PIC  X(10).                     CL*23
00623              20  PI-SCR-FIN-RESP     PIC  X(10).                     CL*23
00624              20  PI-SCR-TYPE         PIC  X.                         CL*23
00625          16  PI-TRANSFER-SW          PIC  X.                         CL*23
00626              88  PI-TRANSFER-BEFORE-ACT  VALUE 'Y'.                  CL*23
00627          16  FILLER                  PIC  X(307).                    CL*27
00628                                                                      CL*23
00629  EJECT                                                               CL**5
00630                              COPY ELCJPFX.                           CL*13
00631                              PIC  X(1464).                           CL**5
00632  EJECT                                                               CL**5
00633                              COPY ELCAID.                            CL*13
00634                                                                      CL**5
00635  01  FILLER  REDEFINES  DFHAID.                                      CL**5
00636      12  FILLER              PIC  X(8).                              CL**6
00637      12  PF-VALUES           PIC  X      OCCURS  2  TIMES.           CL**6
00638  EJECT                                                               CL**5
00639                              COPY EL642S.                            CL*13
00640  EJECT                                                               CL**5
00641  LINKAGE SECTION.                                                 EL642
00642                                                                   EL642
00643  01  DFHCOMMAREA             PIC  X(1024).                           CL**5
00644  EJECT                                                               CL**5
00645 *01 PARMLIST             COMP.                                       CL*26
00646 *    12  FILLER              PIC S9(8).                              CL*26
00647 *    12  ERPNDB-POINTER      PIC S9(8).                              CL*26
00648 *    12  ELCNTL-POINTER      PIC S9(8).                              CL*26
00649 *    12  ERCOMP-POINTER      PIC S9(8).                              CL*26
00650 *    12  ERPYAJ-POINTER      PIC S9(8).                              CL*26
00651 *    12  ERACCT-POINTER      PIC S9(8).                              CL*26
00652 *    12  ERBILL-POINTER      PIC S9(8).                              CL*26
00653 *    12  ERCOMM-POINTER      PIC S9(8).                              CL*26
00654 *    12  ERCTBL-POINTER      PIC S9(8).                              CL*26
00655 *    12  ERGXRF-POINTER      PIC S9(8).                              CL*26
00656 *    12  ERGXRF2-POINTER     PIC S9(8).                              CL*26
00657 *    12  ERGXRF3-POINTER     PIC S9(8).                              CL*26
00658 *    12  ERGXRF4-POINTER     PIC S9(8).                              CL*26
00659 *    12  ERGXRF5-POINTER     PIC S9(8).                              CL*26
00660 *    12  ERGXRF6-POINTER     PIC S9(8).                              CL*26
00661 *    12  ERGXRF7-POINTER     PIC S9(8).                              CL*26
00662 *    12  ERGXRF8-POINTER     PIC S9(8).                              CL*26
00663  EJECT                                                               CL**5
00664                              COPY ERCPNDB.                           CL*13
00665  EJECT                                                               CL**5
00666                              COPY ELCCNTL.                           CL*13
00667  EJECT                                                               CL**5
00668                              COPY ERCCOMP.                           CL*13
00669  EJECT                                                               CL**5
00670                              COPY ERCPYAJ.                           CL*13
00671  EJECT                                                               CL**5
00672                              COPY ERCACCT.                           CL*13
00673  EJECT                                                               CL**5
00674                              COPY ERCBILL.                           CL*13
00675  EJECT                                                               CL**5
00676                              COPY ERCCOMM.                           CL*13
00677  EJECT                                                               CL**5
00678                              COPY ERCCTBL.                           CL*13
00679  EJECT                                                               CL**5
00680                              COPY ERCGXRF.                           CL*13
00681  EJECT                                                               CL**5
00682                              COPY ERCRESC.                           CL*27
00683                                                                      CL*27
00684  PROCEDURE DIVISION.                                              EL642
00685                                                                   EL642
00686      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.        CL**5
00687      MOVE 2                      TO  EMI-NUMBER-OF-LINES.            CL**5
00688      MOVE EIBTRMID               TO  QID-TERM.                       CL**5
00689      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.                CL**6
00690      MOVE '5'                    TO  DC-OPTION-CODE.                 CL**6
00691                                                                      CL**5
00692      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                     CL**6
00693                                                                      CL**5
00694      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE.                CL**5
00695      MOVE DC-GREG-DATE-1-MDY     TO  WS-CURRENT-DATE-MDY.            CL**5
00696      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DATE-EDIT.           CL**5
00697                                                                      CL**5
00698      IF EIBCALEN  =  ZERO                                            CL**5
00699          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL642
00700                                                                   EL642
00701      IF PI-RETURN-TO-PROGRAM  =  THIS-PGM                            CL**5
00702          MOVE PI-CALLING-PROGRAM  TO  RETURNED-FROM.                 CL**5
00703                                                                   EL642
00704      IF PI-CALLING-PROGRAM NOT  =  THIS-PGM                          CL**5
00705          IF PI-RETURN-TO-PROGRAM NOT  =  THIS-PGM                    CL**5
00706              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6       CL**5
00707              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5       CL**5
00708              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4       CL**5
00709              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3       CL**5
00710              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2       CL**5
00711              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1       CL**5
00712              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM     CL**5
00713              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM       CL**5
00714              MOVE PI-CR-CONTROL-IN-PROGRESS                          CL*23
00715                                         TO  W-TRANSFER-CONTROL       CL*23
00716              MOVE SPACES                                             CL**5
00717                                    TO  PI-CR-CONTROL-IN-PROGRESS     CL*23
00718          ELSE                                                     EL642
00719              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM       CL**5
00720              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM     CL**5
00721              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1       CL**5
00722              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2       CL**5
00723              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3       CL**5
00724              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4       CL**5
00725              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5       CL**5
00726              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.      CL**5
00727                                                                   EL642
00728      MOVE LOW-VALUES             TO  EL642AI.                        CL**5
00729                                                                   EL642
00730      IF PI-AR-PROCESSING                                             CL**9
00731         MOVE XCTL-635            TO XCTL-PYAJ.                       CL**9
00732                                                                      CL*26
00733      IF PI-COMPANY-ID = 'DMD'                                        CL*26
00734         MOVE XCTL-633DMD         TO XCTL-PYAJ.                       CL*26
00735                                                                      CL**9
00736      IF RETURNED-FROM  NOT =  SPACES                                 CL**5
00737          PERFORM 0600-RECOVER-TEMP-STORAGE  THRU  0690-EXIT          CL**5
00738          IF RETURNED-FROM  =  XCTL-PYAJ                              CL**9
00739            AND  NOT  PI-VOID-BILL                                    CL**5
00740              MOVE ZEROS          TO  PI-ADJUSTMNTS                   CL**5
00741                                      PI-DISBURSED                    CL**5
00742                                      PI-REMITTED                     CL**5
00743              GO TO 1200-PYAJ-BILLING-COMPLETE                     EL642
00744          ELSE                                                     EL642
00745              MOVE -1             TO  APFNTERL                        CL**5
00746              IF PI-TRANSFER-BEFORE-ACT                               CL*23
00747                  GO TO 8100-SEND-INITIAL-MAP                         CL*23
00748              ELSE                                                    CL*23
00749                  PERFORM 5000-FORMAT-SCREEN  THRU  5090-EXIT         CL*23
00750                  GO TO 8100-SEND-INITIAL-MAP.                        CL*23
00751                                                                   EL642
00752      IF EIBTRNID  NOT =  TRANS-ID                                    CL**5
00753          MOVE EL642A             TO  PI-MAP-NAME                     CL**5
00754          MOVE -1                 TO  APFNTERL                        CL**5
00755          GO TO 8100-SEND-INITIAL-MAP.                             EL642
00756                                                                   EL642
00757      EXEC CICS HANDLE CONDITION                                   EL642
00758          PGMIDERR  (9600-PGMID-ERROR)                             EL642
00759          ERROR     (9990-ABEND)                                   EL642
00760      END-EXEC.                                                       CL*27
00761                                                                   EL642
00762      IF EIBAID  =  DFHCLEAR                                          CL**5
00763          IF PI-MAP-NAME  =  EL642B                                   CL**5
00764              PERFORM 5000-FORMAT-SCREEN  THRU  5090-EXIT             CL**5
00765              MOVE EL642A         TO  PI-MAP-NAME                     CL**5
00766              MOVE -1             TO  APFNTERL                        CL**5
00767              GO TO 8100-SEND-INITIAL-MAP                          EL642
00768          ELSE                                                     EL642
00769              GO TO 9400-CLEAR.                                    EL642
00770                                                                   EL642
00771      IF PI-PROCESSOR-ID  =  'LGXX'                                   CL**5
00772          GO TO 0200-RECEIVE.                                      EL642
00773                                                                   EL642
00774      EXEC CICS READQ TS                                           EL642
00775          QUEUE   (PI-SECURITY-TEMP-STORE-ID)                         CL**5
00776          INTO    (SECURITY-CONTROL)                                  CL**5
00777          LENGTH  (SC-COMM-LENGTH)                                    CL**5
00778          ITEM    (SC-ITEM)                                           CL**5
00779      END-EXEC.                                                       CL*27
00780                                                                   EL642
00781      MOVE SC-CREDIT-DISPLAY (19)  TO  PI-DISPLAY-CAP.                CL**5
00782      MOVE SC-CREDIT-UPDATE  (19)  TO  PI-MODIFY-CAP.                 CL**5
00783                                                                   EL642
00784      IF NOT  DISPLAY-CAP                                             CL**5
00785          MOVE 'READ'              TO  SM-READ                        CL**5
00786          PERFORM 9995-SECURITY-VIOLATION                          EL642
00787          MOVE ER-0070             TO  EMI-ERROR                      CL**5
00788          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**5
00789          GO TO 8100-SEND-INITIAL-MAP.                             EL642
00790                                                                   EL642
00791  0200-RECEIVE.                                                    EL642
00792      IF EIBAID  =  DFHPA1  OR  DFHPA2  OR  DFHPA3                    CL**5
00793          MOVE ER-0008            TO  EMI-ERROR                       CL**5
00794          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**5
00795          IF PI-MAP-NAME  =  EL642A                                   CL**5
00796              MOVE -1             TO  APFNTERL                        CL**5
00797              GO TO 8200-SEND-DATAONLY                             EL642
00798          ELSE                                                     EL642
00799              MOVE -1             TO  BPFNTERL                        CL**5
00800              GO TO 8200-SEND-DATAONLY.                            EL642
00801                                                                   EL642
00802      EXEC CICS RECEIVE                                            EL642
00803          MAP     (PI-MAP-NAME)                                       CL**5
00804          MAPSET  (MAPSET-NAME)                                       CL**5
00805          INTO    (EL642AI)                                           CL**5
00806      END-EXEC.                                                       CL*27
00807                                                                   EL642
00808      IF PI-MAP-NAME  =  EL642A                                       CL**5
00809          IF APFNTERL  GREATER THAN  ZERO                             CL**5
00810              IF EIBAID  NOT =  DFHENTER                              CL**5
00811                  MOVE ER-0004    TO  EMI-ERROR                       CL**5
00812                  MOVE AL-UNBOF   TO  APFNTERA                        CL**5
00813                  MOVE -1         TO  APFNTERL                        CL**5
00814                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL**5
00815                  GO TO 8200-SEND-DATAONLY                         EL642
00816              ELSE                                                 EL642
00817                  IF APFNTERI  NUMERIC                                CL**5
00818                    AND  APFNTERI  GREATER THAN  ZERO                 CL**5
00819                    AND  APFNTERI  LESS THAN  25                      CL**5
00820                      MOVE PF-VALUES (APFNTERI)  TO  EIBAID           CL**5
00821                  ELSE                                             EL642
00822                      MOVE ER-0029   TO  EMI-ERROR                    CL**5
00823                      MOVE AL-UNBOF  TO  APFNTERA                     CL**5
00824                      MOVE -1        TO  APFNTERL                     CL**5
00825                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT      CL**5
00826                      GO TO 8200-SEND-DATAONLY                     EL642
00827          ELSE                                                     EL642
00828              NEXT SENTENCE                                        EL642
00829      ELSE                                                         EL642
00830          IF PI-MAP-NAME  =  EL642B                                   CL**5
00831              IF BPFNTERL  GREATER THAN  ZERO                         CL**5
00832                  IF EIBAID  NOT =  DFHENTER                          CL**5
00833                      MOVE ER-0004   TO  EMI-ERROR                    CL**5
00834                      MOVE AL-UNBOF  TO  BPFNTERA                     CL**5
00835                      MOVE -1        TO  BPFNTERL                     CL**5
00836                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT      CL**5
00837                      GO TO 8200-SEND-DATAONLY                        CL**5
00838                  ELSE                                             EL642
00839                      IF BPFNTERI  NUMERIC                            CL**5
00840                        AND  BPFNTERI  GREATER THAN  ZERO             CL**5
00841                        AND  BPFNTERI  LESS THAN  25                  CL**5
00842                          MOVE PF-VALUES (BPFNTERI)  TO  EIBAID       CL**5
00843                      ELSE                                            CL**5
00844                          MOVE ER-0029   TO  EMI-ERROR                CL**5
00845                          MOVE AL-UNBOF  TO  BPFNTERA                 CL**5
00846                          MOVE -1        TO  BPFNTERL                 CL**5
00847                          PERFORM 9900-ERROR-FORMAT                   CL**5
00848                              THRU  9900-EXIT                         CL**5
00849                          GO TO 8200-SEND-DATAONLY.                   CL**5
00850  EJECT                                                               CL**5
00851  0300-CHECK-PFKEYS.                                               EL642
00852      IF EIBAID  =  DFHPF23                                           CL**5
00853          GO TO 8810-PF23.                                         EL642
00854                                                                   EL642
00855      IF EIBAID  =  DFHPF24                                           CL**5
00856          GO TO 9200-RETURN-MAIN-MENU.                             EL642
00857                                                                   EL642
00858      IF EIBAID  =  DFHPF12                                           CL**5
00859          GO TO 9500-PF12.                                         EL642
00860                                                                   EL642
00861  0310-CHECK-PFKEYS.                                                  CL**5
00862      IF EIBAID  =  DFHPF3                                            CL**5
00863        AND  PI-MAP-NAME  =  EL642A                                   CL**5
00864          PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT           CL**5
00865          MOVE SPACES             TO  PI-CR-CONTROL-IN-PROGRESS       CL**5
00866          MOVE XCTL-650           TO  PGM-NAME                        CL**5
00867          GO TO 9300-XCTL.                                         EL642
00868                                                                   EL642
00869      IF EIBAID  =  DFHPF4                                            CL*23
00870        AND  PI-MAP-NAME  =  EL642A                                   CL*23
00871         NEXT SENTENCE                                                CL*23
00872      ELSE                                                            CL*23
00873         GO TO 0320-CONT-PFKEYS.                                      CL*23
00874                                                                      CL*23
00875      IF PI-CR-FIN-RESP  NOT =  SPACES                                CL*23
00876          MOVE PI-COMP-CARRIER   TO  PI-CR-CARRIER                    CL*23
00877          MOVE PI-COMP-GROUPING  TO  PI-CR-GROUPING                   CL*23
00878          MOVE SPACE             TO  PI-CR-STATE                      CL*23
00879          MOVE LOW-VALUES        TO  PI-CR-ACCOUNT                    CL*23
00880          MOVE PI-SAV-AGENT      TO  PI-CR-FIN-RESP                   CL*23
00881          MOVE 'G'               TO  PI-CR-TYPE                       CL*23
00882          PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT           CL*23
00883          MOVE XCTL-652       TO  PGM-NAME                            CL*23
00884          GO TO 9300-XCTL.                                            CL*23
00885                                                                      CL*23
00886      IF PI-SCR-FIN-RESP EQUAL SPACES OR LOW-VALUES                   CL*23
00887          GO TO 0315-PF4-ERROR.                                       CL*23
00888                                                                      CL*23
00889      PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT.              CL*23
00890                                                                      CL*23
00891      MOVE PI-SCR-CARRIER        TO  PI-CR-CARRIER.                   CL*23
00892      MOVE PI-SCR-GROUPING       TO  PI-CR-GROUPING.                  CL*23
00893      MOVE SPACE                 TO  PI-CR-STATE.                     CL*23
00894      MOVE LOW-VALUES            TO  PI-CR-ACCOUNT.                   CL*23
00895      MOVE PI-SCR-FIN-RESP       TO  PI-CR-FIN-RESP.                  CL*23
00896      MOVE 'G'                   TO  PI-CR-TYPE.                      CL*23
00897      MOVE 'Y'                   TO  PI-TRANSFER-SW.                  CL*23
00898                                                                      CL*23
00899      IF PI-ZERO-CARRIER                                              CL*23
00900        OR  PI-ZERO-CAR-GROUP                                         CL*23
00901          MOVE ZEROS              TO  PI-CR-CARRIER.                  CL*23
00902                                                                      CL*23
00903      IF PI-ZERO-GROUPING                                             CL*23
00904         OR  PI-ZERO-CAR-GROUP                                        CL*23
00905          MOVE ZEROS              TO  PI-CR-GROUPING.                 CL*23
00906                                                                      CL*23
00907      MOVE XCTL-652       TO  PGM-NAME                                CL*23
00908                                                                      CL*23
00909      GO TO 9300-XCTL.                                                CL*23
00910                                                                      CL*23
00911  0315-PF4-ERROR.                                                     CL*23
00912                                                                      CL*23
00913      MOVE ER-2407        TO  EMI-ERROR.                              CL*23
00914      MOVE -1             TO  APFNTERL.                               CL*23
00915      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL*23
00916      GO TO 8200-SEND-DATAONLY.                                       CL*23
00917                                                                      CL*23
00918  0320-CONT-PFKEYS.                                                   CL*23
00919                                                                      CL*23
00920      IF EIBAID  =  DFHPF6                                            CL**5
00921        AND  PI-MAP-NAME  =  EL642A                                   CL**5
00922          MOVE LOW-VALUES         TO  EL642AO                         CL**5
00923          MOVE PI-SAV-CARR        TO  PI-CR-CARRIER                EL642
00924          MOVE PI-SAV-GROUP       TO  PI-CR-GROUPING               EL642
00925          MOVE SPACE              TO  PI-CR-STATE                  EL642
00926          MOVE LOW-VALUES         TO  PI-CR-ACCOUNT                EL642
00927          MOVE PI-SAV-AGENT       TO  PI-CR-FIN-RESP               EL642
00928          MOVE 'G'                TO  PI-CR-TYPE                   EL642
00929          PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT           CL**5
00930          MOVE XCTL-6401          TO  PGM-NAME                        CL**5
00931          GO TO 9300-XCTL.                                         EL642
00932                                                                   EL642
00933      IF EIBAID  =  DFHPF7                                            CL**5
00934        AND  PI-MAP-NAME  =  EL642A                                   CL**5
00935          IF PI-CR-FIN-RESP  NOT =  SPACES                            CL**5
00936              MOVE PI-COMP-CARRIER   TO  PI-CR-CARRIER                CL**5
00937              MOVE PI-COMP-GROUPING  TO  PI-CR-GROUPING               CL**5
00938              MOVE SPACE             TO  PI-CR-STATE                  CL**5
00939              MOVE LOW-VALUES        TO  PI-CR-ACCOUNT                CL**5
00940              MOVE PI-SAV-AGENT      TO  PI-CR-FIN-RESP               CL**5
00941              MOVE 'G'               TO  PI-CR-TYPE                   CL**5
00942              PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT       CL**5
00943              MOVE XCTL-PYAJ      TO  PGM-NAME                        CL**9
00944              GO TO 9300-XCTL                                      EL642
00945          ELSE                                                     EL642
00946              IF PI-RETURN-TO-PROGRAM = XCTL-633 OR XCTL-635 OR       CL*26
00947                                        XCTL-633DMD                   CL*26
00948                  GO TO 9400-CLEAR                                    CL*23
00949             ELSE                                                     CL*23
00950                  MOVE ER-2411        TO  EMI-ERROR                   CL*23
00951                  MOVE -1             TO  APFNTERL                    CL*23
00952                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL*23
00953                  GO TO 8200-SEND-DATAONLY.                           CL*23
00954                                                                   EL642
00955      IF EIBAID  =  DFHPF8                                            CL**5
00956        AND  PI-MAP-NAME  =  EL642A                                   CL**5
00957          IF PI-CR-FIN-RESP  NOT =  SPACES                            CL**5
00958              MOVE PI-SAV-CARR    TO  PI-CR-CARRIER                   CL**5
00959              MOVE PI-SAV-GROUP   TO  PI-CR-GROUPING                  CL**5
00960              MOVE SPACE          TO  PI-CR-STATE                     CL**5
00961              MOVE LOW-VALUES     TO  PI-CR-ACCOUNT                   CL**5
00962              MOVE PI-SAV-AGENT   TO  PI-CR-FIN-RESP                  CL**5
00963              MOVE 'G'            TO  PI-CR-TYPE                      CL**5
00964              PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT       CL**5
00965              MOVE XCTL-658       TO  PGM-NAME                        CL*23
00966              GO TO 9300-XCTL                                         CL*23
00967          ELSE                                                        CL*23
00968          IF PI-SCR-FIN-RESP  NOT =  SPACES                           CL*23
00969              PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT       CL*23
00970              MOVE PI-SCR-CARRIER  TO  PI-CR-CARRIER                  CL*23
00971              MOVE PI-SCR-GROUPING TO  PI-CR-GROUPING                 CL*23
00972              MOVE SPACE           TO  PI-CR-STATE                    CL*23
00973              MOVE LOW-VALUES      TO  PI-CR-ACCOUNT                  CL*23
00974              MOVE PI-SCR-FIN-RESP TO  PI-CR-FIN-RESP                 CL*23
00975              MOVE 'G'             TO  PI-CR-TYPE                     CL*23
00976              MOVE XCTL-658       TO  PGM-NAME                        CL**5
00977              GO TO 9300-XCTL                                      EL642
00978          ELSE                                                     EL642
00979              MOVE ER-2399        TO  EMI-ERROR                       CL**5
00980              MOVE -1             TO  APFNTERL                        CL**5
00981              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
00982              GO TO 8200-SEND-DATAONLY.                            EL642
00983                                                                   EL642
00984      IF EIBAID  =  DFHPF1                                            CL**5
00985        AND  PI-MAP-NAME  =  EL642B                                   CL**5
00986          GO TO 7500-PRODUCE-CHECK.                                EL642
00987                                                                   EL642
00988      IF EIBAID  =  DFHPF5                                            CL**5
00989        AND  PI-MAP-NAME  =  EL642A                                   CL**5
00990          IF PI-CR-FIN-RESP NOT  =  SPACES                            CL**5
00991              MOVE PI-SAV-CARR    TO  PI-CR-CARRIER                   CL**5
00992              MOVE PI-SAV-GROUP   TO  PI-CR-GROUPING                  CL**5
00993              MOVE SPACE          TO  PI-CR-STATE                     CL**5
00994              MOVE LOW-VALUES     TO  PI-CR-ACCOUNT                   CL**5
00995              MOVE PI-SAV-AGENT   TO  PI-CR-FIN-RESP                  CL**5
00996              MOVE 'G'            TO  PI-CR-TYPE                      CL**5
00997              MOVE EL642B         TO  PI-MAP-NAME                     CL**5
00998              GO TO 7000-PROCESS-CHECK                             EL642
00999          ELSE                                                     EL642
01000              MOVE ER-2400        TO  EMI-ERROR                       CL**5
01001              MOVE -1             TO  APFNTERL                        CL**5
01002              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
01003              GO TO 8200-SEND-DATAONLY.                            EL642
01004                                                                   EL642
01005      IF PI-MAP-NAME  =  EL642A                                       CL**5
01006          IF EIBAID  =  DFHENTER                                      CL**5
01007            OR  DFHPF3                                                CL**5
01008              GO TO 0330-EDIT-DATA.                                EL642
01009                                                                   EL642
01010  0320-INPUT-ERROR.                                                EL642
01011      MOVE ER-0029                TO  EMI-ERROR.                      CL**5
01012                                                                      CL**5
01013      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**5
01014                                                                      CL**5
01015      IF PI-MAP-NAME  =  EL642A                                       CL**5
01016          MOVE AL-UNBON           TO  APFNTERA                        CL**5
01017          MOVE -1                 TO  APFNTERL                        CL**5
01018      ELSE                                                         EL642
01019          MOVE AL-UNBON           TO  BPFNTERA                        CL**5
01020          MOVE -1                 TO  BPFNTERL.                       CL**5
01021                                                                      CL**5
01022      GO TO 8200-SEND-DATAONLY.                                    EL642
01023  EJECT                                                               CL**5
01024  0330-EDIT-DATA.                                                  EL642
01025      MOVE ABILTYPI               TO  VALID-BILL-TYPE-VALUES.         CL**5
01026                                                                   EL642
01027      IF VALID-BILL-TYPE                                           EL642
01028          IF NOT MODIFY-CAP                                           CL*31
01029              IF ABILTYPI  =  '1' OR '2'                              CL**5
01030                  NEXT SENTENCE                                       CL**5
01031              ELSE                                                    CL**5
01032                  MOVE 'UPDATE'   TO  SM-READ                         CL**5
01033                  PERFORM 9995-SECURITY-VIOLATION                     CL**5
01034                  MOVE ER-0070    TO  EMI-ERROR                       CL**5
01035                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL**5
01036                  GO TO 8100-SEND-INITIAL-MAP.                        CL**5
01037                                                                   EL642
01038      MOVE SPACES                 TO  PI-LIMIT-BILLING-ACCOUNTS.      CL**5
01039                                                                   EL642
01040  0350-ERROR-CHECK.                                                   CL**5
01041      IF EMI-ERROR  =  ZEROS                                          CL**5
01042          NEXT SENTENCE                                               CL**5
01043      ELSE                                                         EL642
01044          GO TO 8200-SEND-DATAONLY.                                   CL**5
01045                                                                   EL642
01046      IF AAGENTL  GREATER THAN  ZEROS                                 CL**5
01047          MOVE AL-UANON           TO  AAGENTA                         CL**5
01048          MOVE AAGENTI            TO  PI-SAV-AGENT                    CL**5
01049      ELSE                                                         EL642
01050          MOVE -1                 TO  AAGENTL                         CL**5
01051          MOVE AL-UABON           TO  AAGENTA                         CL**5
01052          MOVE ER-2910            TO  EMI-ERROR                       CL**5
01053          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                 CL**5
01054                                                                   EL642
01055      IF ACARIERL  GREATER THAN  ZEROS                                CL**5
01056          MOVE AL-UANON           TO  ACARIERA                        CL**5
01057          MOVE ACARIERI           TO  PI-SAV-CARR                     CL**5
01058                                      PI-CR-CARRIER                   CL**5
01059                                      PI-COMP-CARRIER                 CL*23
01060      ELSE                                                         EL642
01061          IF NOT  PI-ZERO-CARRIER                                     CL**5
01062            AND  NOT  PI-ZERO-CAR-GROUP                               CL**5
01063              MOVE -1             TO  ACARIERL                        CL**5
01064              MOVE AL-UABON       TO  ACARIERA                        CL**5
01065              MOVE ER-0194        TO  EMI-ERROR                       CL**5
01066              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
01067          ELSE                                                        CL**5
01068              MOVE ZERO           TO  PI-SAV-CARR                     CL*23
01069                                      PI-COMP-CARRIER.                CL*23
01070                                                                      CL**5
01071      IF AGROUPL  GREATER THAN  ZEROS                                 CL**5
01072          MOVE AL-UANON           TO  AGROUPA                         CL**5
01073          MOVE AGROUPI            TO  PI-SAV-GROUP                    CL**5
01074                                      PI-CR-GROUPING                  CL**5
01075                                      PI-COMP-GROUPING                CL*23
01076      ELSE                                                            CL**5
01077          IF NOT  PI-ZERO-GROUPING                                    CL**5
01078            AND  NOT  PI-ZERO-CAR-GROUP                               CL**5
01079              MOVE -1             TO  AGROUPL                         CL**5
01080              MOVE AL-UABON       TO  AGROUPA                         CL**5
01081              MOVE ER-0195        TO  EMI-ERROR                       CL**5
01082              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
01083          ELSE                                                     EL642
01084              MOVE ZERO           TO  PI-SAV-GROUP                    CL*23
01085                                      PI-COMP-GROUPING.               CL*23
01086                                                                   EL642
01087      MOVE ABILTYPI               TO  VALID-BILL-TYPE-VALUES.         CL**5
01088                                                                   EL642
01089      IF VALID-BILL-TYPE                                           EL642
01090          MOVE AL-UANON           TO  ABILTYPA                        CL**5
01091          MOVE ABILTYPI           TO  PI-BILL-TYPE                    CL**5
01092                                      WS-BILL-TYPE                    CL**5
01093      ELSE                                                         EL642
01094          MOVE -1                 TO  ABILTYPL                        CL**5
01095          MOVE ER-2249            TO  EMI-ERROR                       CL**5
01096          MOVE AL-UABON           TO  ABILTYPA                        CL**5
01097          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                 CL**5
01098                                                                      CL**8
01099      IF PI-AR-PROCESSING                                             CL**8
01100         IF PI-PREVIEW                                                CL**8
01101            NEXT SENTENCE                                             CL**8
01102         ELSE                                                         CL**8
01103            MOVE -1               TO ABILTYPL                         CL**8
01104            MOVE ER-3145          TO EMI-ERROR                        CL**8
01105            MOVE AL-UABON         TO ABILTYPA                         CL**8
01106            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**8
01107                                                                   EL642
01108      IF APRODSWL  NOT =  ZEROS                                       CL**5
01109          IF PI-PREVIEW                                            EL642
01110              IF APRODSWI  =  'Y'  OR  'N'                            CL**5
01111                  MOVE AL-UANON   TO  APRODSWA                        CL**5
01112              ELSE                                                 EL642
01113                  MOVE -1         TO  APRODSWL                        CL**5
01114                  MOVE ER-2436    TO  EMI-ERROR                       CL**5
01115                  MOVE AL-UABON   TO  APRODSWA                        CL**5
01116                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL**5
01117          ELSE                                                     EL642
01118              MOVE -1             TO  APRODSWL                        CL**5
01119              MOVE ER-2434        TO  EMI-ERROR                       CL**5
01120              MOVE AL-UABON       TO  APRODSWA                        CL**5
01121              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
01122      ELSE                                                         EL642
01123          IF PI-PREVIEW                                            EL642
01124              MOVE -1             TO  APRODSWL                        CL**5
01125              MOVE ER-2435        TO  EMI-ERROR                       CL**5
01126              MOVE AL-UABON       TO  APRODSWA                        CL**5
01127              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.             CL**5
01128                                                                   EL642
01129      IF AACCT1L  NOT =  ZEROS                                        CL**5
01130          MOVE AL-UANON           TO  AACCT1A                         CL**5
01131          MOVE 'Y'                TO  LIMIT-BILLING-SW                CL**5
01132          MOVE AACCT1I            TO  PI-BILLING-ACCOUNTS (1).        CL**5
01133                                                                   EL642
01134      IF AACCT2L  NOT =  ZEROS                                        CL**5
01135          MOVE AL-UANON           TO  AACCT2A                         CL**5
01136          MOVE 'Y'                TO  LIMIT-BILLING-SW                CL**5
01137          MOVE AACCT2I            TO  PI-BILLING-ACCOUNTS (2).        CL**5
01138                                                                   EL642
01139      IF AACCT3L  NOT =  ZEROS                                        CL**5
01140          MOVE AL-UANON           TO  AACCT3A                         CL**5
01141          MOVE 'Y'                TO  LIMIT-BILLING-SW                CL**5
01142          MOVE AACCT3I            TO  PI-BILLING-ACCOUNTS (3).        CL**5
01143                                                                   EL642
01144      IF EMI-ERROR  =  ZEROS                                          CL**5
01145          GO TO 1000-BILLING-PROCESS.                              EL642
01146                                                                   EL642
01147      GO TO 8200-SEND-DATAONLY.                                    EL642
01148  EJECT                                                               CL**5
01149  0500-CREATE-TEMP-STORAGE.                                        EL642
01150      IF PI-BAL-FRWD  NOT  NUMERIC                                    CL**5
01151          MOVE ZEROS              TO  PI-BAL-FRWD                     CL**5
01152                                      PI-PREMIUM                      CL**5
01153                                      PI-REMITTED                     CL**5
01154                                      PI-TOT-ISS-COMP                 CL**5
01155                                      PI-TOT-CAN-COMP                 CL**5
01156                                      PI-ADJUSTMNTS                   CL**5
01157                                      PI-DISBURSED                    CL**5
01158                                      PI-END-BAL                      CL**5
01159                                      PI-UNPAID-NET-PREM              CL**5
01160                                      PI-COMP-UNPAID-PREM             CL**5
01161                                      PI-LF-ISS-COMP                  CL**5
01162                                      PI-AH-ISS-COMP                  CL**5
01163                                      PI-LF-CAN-COMP                  CL**5
01164                                      PI-AH-CAN-COMP                  CL**5
01165                                      PI-DUE-FOR-ACCT                 CL**5
01166                                      PI-ACCT-BEG-BAL                 CL**5
01167                                      PI-ACCT-NET-PREM                CL**5
01168                                      PI-ACCT-COMP                    CL**5
01169                                      PI-ACCT-PAY-ADJS.               CL**5
01170                                                                   EL642
01171      EXEC CICS WRITEQ TS                                          EL642
01172          QUEUE   (QID)                                               CL**5
01173          FROM    (PROGRAM-INTERFACE-BLOCK)                           CL**5
01174          LENGTH  (PI-COMM-LENGTH)                                    CL**5
01175      END-EXEC.                                                       CL*27
01176                                                                   EL642
01177  0590-EXIT.                                                       EL642
01178       EXIT.                                                       EL642
01179                                                                   EL642
01180  0600-RECOVER-TEMP-STORAGE.                                       EL642
01181      EXEC CICS READQ TS                                           EL642
01182          QUEUE   (QID)                                               CL**5
01183          INTO    (PROGRAM-INTERFACE-BLOCK)                           CL**5
01184          LENGTH  (PI-COMM-LENGTH)                                    CL**5
01185      END-EXEC.                                                       CL*27
01186                                                                   EL642
01187      PERFORM 0800-DELETE-TS  THRU  0890-EXIT.                        CL**5
01188                                                                      CL*23
01189      MOVE PI-SCRN-CONTROL TO W-TRANSFER-CONTROL.                     CL*23
01190                                                                   EL642
01191  0690-EXIT.                                                       EL642
01192       EXIT.                                                       EL642
01193                                                                   EL642
01194  0800-DELETE-TS.                                                  EL642
01195      EXEC CICS HANDLE CONDITION                                   EL642
01196          QIDERR  (0890-EXIT)                                         CL**5
01197      END-EXEC.                                                       CL*27
01198                                                                   EL642
01199      EXEC CICS DELETEQ TS                                         EL642
01200          QUEUE  (QID)                                                CL**5
01201      END-EXEC.                                                       CL*27
01202                                                                   EL642
01203  0890-EXIT.                                                       EL642
01204       EXIT.                                                       EL642
01205  EJECT                                                               CL**5
01206 ******************************************************************EL642
01207 *    THIS SECTION PROCESSES THE PENDING BUSINESS FILE BASED ON   *EL642
01208 *    ACCOUNT DATE RANGES FOR EACH ACCOUNT THAT A GENERAL AGENT   *EL642
01209 *    RECEIVES COMMISION FROM.  THE PROFILE OF A GENERAL AGENT    *EL642
01210 *    IS MAINTAINED IN THE AGENT-CROSS-REFERENCE FILE.            *EL642
01211 *                                                                *EL642
01212 *    1.  IF VOID BILLING                                         *EL642
01213 *            GO TO VOID-BILLING.                                 *EL642
01214 *                                                                *EL642
01215 *    2.  READ COMPENSATIONG MASTER FOR GENERAL AGENT TO GET      *EL642
01216 *        THE AGENT'S ENDING BALANCE AND ADDRESS.                 *EL642
01217 *                                                                *EL642
01218 *    3.  READ THE AGENT-CROSS-REFERENCE-FILE.                    *EL642
01219 *                                                                *EL642
01220 *    4.  PROCESS EACH ACCOUNT ASSOCIATED WITH A GENERAL AGENT.   *EL642
01221 *                                                                *EL642
01222 *        A.  PROCESSS EACH DATE RANGE PER ACCOUNT AND ACCUMULATE *EL642
01223 *            STATISTICS FOR THAT ACCOUNT.                        *EL642
01224 *                                                                *EL642
01225 *            1. PROCESS THE ACTIVE RECORDS ON PENDING BUSINESS.  *EL642
01226 *               OMIT ANY RECORDS THAT HAVE ANY FATAL OR UNFORCED *EL642
01227 *               ERRORS.  DO NOT PROCESS ANY REINSURANCE OR BILL- *EL642
01228 *               ING ADJUSTMENT RECORDS.  IF BILLING UPDATE, FLAG  EL642
01229 *               EACH PENDING BUSINESS RECORD THAT IS PRCESSED.   *EL642
01230 *                                                                *EL642
01231 *            2.  ACCUMULATE STATISTICS FOR SCREEN DISPLAY.       *EL642
01232 *                                                                *EL642
01233 *            3.  COMPUTE ACCOUNT AND AGENT COMMISSION.           *EL642
01234 *                                                                *EL642
01235 *         B.  ACCUMULATE THE PAYMENT AND ADJUSTMENTS FOR         *EL642
01236 *             EACH ACCOUNT THE GENERAL AGENT IS RESPONSIBLE FOR. *EL642
01237 *                                                                *EL642
01238 *         C.  PRINT ACCUMULATED STATISTICS FOR EACH ACCOUNT.     *EL642
01239 *             OMITT ANY ACCOUNTS THAT DIDN'T HAVE ANY NEW BUS.   *EL642
01240 *                                                                *EL642
01241 *    5.  PRINT AN AGENT TOTAL OF ALL THE ACCOUNTS AND THE        *EL642
01242 *        GENERAL AGENTS PAYMENTS AND ADJUSTMENTS.                *EL642
01243 ******************************************************************EL642
01244  EJECT                                                               CL**5
01245  1000-BILLING-PROCESS.                                            EL642
01246      MOVE PI-COMPANY-CD          TO  ERPNDB-CO-CD                    CL**5
01247                                      ERPNDB-CO-CD-A1                 CL**5
01248                                      ERCOMP-COMP-CD                  CL**5
01249                                      ERPYAJ-COMP-CD                  CL**5
01250                                      ERACCT-P-CO-CD                  CL**5
01251                                      ERACCT-A-CO-CD                  CL**5
01252                                      ERCOMM-COMPANY-CD               CL**5
01253                                      ERCTBL-COMPANY-CD.              CL**5
01254                                                                   EL642
01255      MOVE ZEROS                  TO  PI-BAL-FRWD                     CL**5
01256                                      PI-UNKNOWN                      CL**5
01257                                      PI-UNPAID-NET-PREM              CL**5
01258                                      PI-COMP-UNPAID-PREM             CL**5
01259                                      PI-PREMIUM                      CL**5
01260                                      PI-REMITTED                     CL**5
01261                                      PI-TOT-ISS-COMP                 CL**5
01262                                      PI-TOT-CAN-COMP                 CL**5
01263                                      PI-ADJUSTMNTS                   CL**5
01264                                      PI-DISBURSED                    CL**5
01265                                      PI-END-BAL                      CL**5
01266                                      PI-LF-ISS-COMP                  CL**5
01267                                      PI-AH-ISS-COMP                  CL**5
01268                                      PI-LF-CAN-COMP                  CL**5
01269                                      PI-AH-CAN-COMP                  CL**5
01270                                      PI-DUE-FOR-ACCT                 CL**5
01271                                      PI-ACCT-BEG-BAL                 CL**5
01272                                      PI-ACCT-NET-PREM                CL**5
01273                                      PI-ACCT-COMP                    CL**5
01274                                      PI-ACCT-PAY-ADJS                CL**5
01275                                      PI-ISSUES-BILLED                CL**5
01276                                      PI-ISSUES-INER                  CL**5
01277                                      PI-ISSUES-PREV                  CL**5
01278                                      PI-CANCELS-BILLED               CL**5
01279                                      PI-CANCELS-INER                 CL**5
01280                                      PI-CANCELS-PREV.                CL**5
01281                                                                   EL642
01282      IF PI-VOID-BILL                                              EL642
01283          GO TO 2000-VOID-BILLING.                                 EL642
01284                                                                   EL642
01285      MOVE SPACES                 TO  ELCNTL-KEY.                     CL**5
01286      MOVE '1'                    TO  ELCNTL-REC-TYPE.                CL**5
01287                                                                   EL642
01288      PERFORM 6100-READ-CONTROL-FILE  THRU  6190-EXIT.                CL**5
01289                                                                   EL642
01290      IF EMI-ERROR  NOT =  ZEROS                                      CL**5
01291          GO TO 8200-SEND-DATAONLY.                                EL642
01292                                                                   EL642
01293      IF CF-ACCOUNT-MSTR-MAINT-DT  =  LOW-VALUES                      CL**5
01294        OR  CF-COMPENSATION-MSTR-MAINT-DT  =  LOW-VALUES              CL**5
01295          MOVE ER-2571            TO  EMI-ERROR                       CL**5
01296          MOVE -1                 TO  APFNTERL                        CL**5
01297          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**5
01298          GO TO 8200-SEND-DATAONLY.                                EL642
01299                                                                   EL642
01300      IF CF-FORMS-PRINTER-ID  =  SPACES                               CL**5
01301        AND  (ABILTYPI = '3'  OR  '4')                                CL**5
01302          MOVE ER-2590            TO  EMI-ERROR                       CL**5
01303          MOVE -1                 TO  ABILTYPL                        CL**5
01304          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**5
01305          GO TO 8200-SEND-DATAONLY.                                   CL**5
01306                                                                   EL642
01307      MOVE CF-CURRENT-MONTH-END   TO  DC-BIN-DATE-1.                  CL**5
01308      MOVE SPACE                  TO  DC-OPTION-CODE.                 CL**5
01309                                                                   EL642
01310      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                     CL**5
01311                                                                   EL642
01312      MOVE DC-GREG-DATE-1-EDIT    TO  PI-MONTH-END-DATE.              CL**5
01313      MOVE WS-CURRENT-DATE-EDIT   TO  HD-RUN-DT.                      CL**5
01314      MOVE CF-CL-MAIL-TO-NAME     TO  CENTER-WORK-1.                  CL**5
01315      MOVE +44                    TO  X-LEN.                          CL**5
01316                                                                      CL**5
01317      PERFORM 8600-CENTER-DATA  THRU  8690-C-D-X.                     CL**5
01318                                                                      CL**5
01319      MOVE CENTER-WORK-2          TO  HD-CO.                          CL**5
01320      MOVE SPACE                  TO  PI-DATA-BILLED-SW.              CL**5
01321      MOVE 'G'                    TO  COMPENSATION-SW                 CL**5
01322                                      PI-CR-TYPE.                  EL642
01323                                                                   EL642
01324      PERFORM 6000-READ-COMP-MASTER  THRU  6090-EXIT.                 CL**5
01325                                                                   EL642
01326      PERFORM 6400-READ-CROSS-REFERENCE  THRU  6490-EXIT.             CL**5
01327                                                                      CL**5
01328      MOVE +0                     TO  GX-SUB.                         CL**5
01329                                                                   EL642
01330  1005-GXRF-BILLING-PROCESS-LOOP.                                  EL642
01331      ADD +1                      TO  GX-SUB.                         CL**5
01332                                                                   EL642
01333      IF GX-SUB  GREATER THAN  GX-AGENT-POINTER-CNT                   CL**5
CIDMOD         IF PI-ACCT-BILLED                                             000
CIDMOD             PERFORM 1100-PYAJ-BILLING-PROCESS  THRU  1190-EXIT   IT   000
CIDMOD             GO TO 1200-PYAJ-BILLING-COMPLETE                          000
CIDMOD         ELSE                                                          000
CIDMOD             GO TO 1200-PYAJ-BILLING-COMPLETE.                       CL*25
01335                                                                   EL642
01336      IF LIMIT-BILLING                                                CL**5
01337          IF GX-AM-ACCOUNT (GX-SUB)  =  PI-BILLING-ACCOUNTS (1)       CL**5
01338                                    OR  PI-BILLING-ACCOUNTS (2)       CL**5
01339                                    OR  PI-BILLING-ACCOUNTS (3)       CL**5
01340              NEXT SENTENCE                                           CL**5
01341          ELSE                                                        CL**5
01342              GO TO 1005-GXRF-BILLING-PROCESS-LOOP.                   CL**5
01343                                                                   EL642
01344      IF FIRST-TIME                                                   CL**5
01345          MOVE 'N'                      TO  FIRST-TIME-SW             CL**5
01346          MOVE GX-AM-CARRIER (GX-SUB)   TO  PI-CR-CARRIER             CL**5
01347          MOVE GX-AM-GROUPING (GX-SUB)  TO  PI-CR-GROUPING            CL**5
01348          MOVE GX-AM-STATE (GX-SUB)     TO  PI-CR-STATE               CL**5
01349          MOVE GX-AM-ACCOUNT (GX-SUB)   TO  PI-CR-ACCOUNT.            CL**5
01350                                                                   EL642
01351      IF GX-AM-CARRIER (GX-SUB)  =  PI-CR-CARRIER                     CL**5
01352        AND  GX-AM-GROUPING (GX-SUB)  =  PI-CR-GROUPING               CL**5
01353        AND  GX-AM-STATE (GX-SUB)  =  PI-CR-STATE                     CL**5
01354        AND  GX-AM-ACCOUNT (GX-SUB)  =  PI-CR-ACCOUNT                 CL**5
01355          NEXT SENTENCE                                               CL**5
01356      ELSE                                                         EL642
01357          IF PI-ACCT-BILLED                                           CL**5
CIDMOD             PERFORM 1100-PYAJ-BILLING-PROCESS  THRU  1190-EXIT   IT   000
01358              MOVE 'TA'           TO  BILLING-DETAIL-TYPE             CL**5
01359              IF APRODSWI  =  'N'                                     CL**5
01360                  NEXT SENTENCE                                       CL**5
01361              ELSE                                                 EL642
01362                  PERFORM 3000-WRITE-BILLING-DETAIL                   CL**5
01363                      THRU  3990-EXIT.                                CL**5
01364                                                                   EL642
01365      MOVE GX-AM-CARRIER (GX-SUB)   TO  PI-CR-CARRIER.                CL**5
01366      MOVE GX-AM-GROUPING (GX-SUB)  TO  PI-CR-GROUPING.               CL**5
01367      MOVE GX-AM-STATE (GX-SUB)     TO  PI-CR-STATE.                  CL**5
01368      MOVE GX-AM-ACCOUNT (GX-SUB)   TO  PI-SAV-ACCT                   CL**5
01369                                        PI-CR-ACCOUNT.                CL**5
01370      MOVE GX-AM-EXPIRATION-DT (GX-SUB)                               CL**5
01371                                    TO  PI-SAV-EXP-DT.                CL**5
01372                                                                   EL642
01373      PERFORM 4300-READ-ACCOUNT-MASTER  THRU  4390-EXIT.              CL**5
01374                                                                   EL642
01375      IF INVALID-ACCOUNT                                           EL642
01376          GO TO 1005-GXRF-BILLING-PROCESS-LOOP.                       CL**5
01377                                                                   EL642
01378      IF PI-UPDATE-FILES                                              CL**5
01379          MOVE WS-GA-LEVEL        TO  GX-AM-LEVEL-NO (GX-SUB)         CL**5
01380          MOVE WS-CURRENT-DATE    TO  GX-LAST-BILL-DT (GX-SUB).       CL**5
01381                                                                   EL642
01382      IF NEW-ACCOUNT                                               EL642
01383          IF PI-ACCT-BILLED                                           CL**5
CIDMOD             PERFORM 1100-PYAJ-BILLING-PROCESS  THRU  1190-EXIT   IT   000
01384              MOVE 'TA'           TO  BILLING-DETAIL-TYPE             CL**5
01385              IF APRODSWI  =  'N'                                     CL**5
01386                  NEXT SENTENCE                                       CL**5
01387              ELSE                                                 EL642
01388                  PERFORM 3000-WRITE-BILLING-DETAIL                   CL**5
01389                      THRU  3990-EXIT.                                CL**5
01390                                                                   EL642
01391      MOVE 'A'                    TO  COMPENSATION-SW              EL642
01392                                       PI-CR-TYPE.                    CL**5
01393                                                                   EL642
01394      PERFORM 6000-READ-COMP-MASTER     THRU 6090-EXIT.               CL*31
01395      PERFORM 1100-PYAJ-BILLING-PROCESS THRU 1190-EXIT.               CL*31
01396      PERFORM 4000-PNDB-START-BROWSE    THRU 4090-EXIT.               CL*31
01397                                                                   EL642
01398      IF PNDB-EOF                                                  EL642
01399          GO TO 1005-GXRF-BILLING-PROCESS-LOOP.                    EL642
01400                                                                   EL642
01401  1010-PNDB-BILLING-PROCESS-LOOP.                                  EL642
01402      PERFORM 4100-PNDB-READ-NEXT  THRU  4150-EXIT.                   CL**5
01403                                                                   EL642
01404      IF PNDB-EOF                                                  EL642
01405          PERFORM 4160-PNDB-END-BROWSE                             EL642
01406          GO TO 1005-GXRF-BILLING-PROCESS-LOOP.                    EL642
01407                                                                   EL642
01408      IF PB-COMPANY-CD-A1  =  PI-COMPANY-CD  AND                      CL*31
01409         PB-SV-CARRIER     =  PI-CR-CARRIER  AND                      CL*31
01410         PB-SV-GROUPING    =  PI-CR-GROUPING AND                      CL*31
01411         PB-SV-STATE       =  PI-CR-STATE    AND                      CL*31
01412         PB-ACCOUNT        =  PI-CR-ACCOUNT                           CL*31
01413          NEXT SENTENCE                                            EL642
01414      ELSE                                                         EL642
01415          PERFORM 4160-PNDB-END-BROWSE                             EL642
01416          GO TO 1005-GXRF-BILLING-PROCESS-LOOP.                    EL642
01417                                                                   EL642
01418      IF PB-BATCH-TRAILER                                          EL642
01419          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                    EL642
01420                                                                   EL642
01421      IF PB-ALT-CHG-SEQ-NO  NOT =  ZEROS                              CL**5
01422          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                       CL**5
01423                                                                      CL**5
01424      IF PB-CERT-EFF-DT LESS GX-AM-EXPIRATION-DT (GX-SUB)             CL*31
01425          NEXT SENTENCE                                               CL*31
01426       ELSE                                                           CL*31
01427          PERFORM 4160-PNDB-END-BROWSE                             EL642
01428          GO TO 1005-GXRF-BILLING-PROCESS-LOOP.                    EL642
01429                                                                   EL642
01430      IF PB-CERT-EFF-DT  LESS THAN  GX-AM-EFF-DT (GX-SUB)          EL642
01431          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                       CL**5
01432                                                                   EL642
01433      IF PB-CERT-EFF-DT  GREATER THAN  ERACCT-P-EXP-DATE              CL**5
01434          PERFORM 4160-PNDB-END-BROWSE                             EL642
01435          GO TO 1005-GXRF-BILLING-PROCESS-LOOP.                    EL642
01436                                                                   EL642
01437      IF (PB-ISSUE)                                                   CL**5
01438        AND                                                           CL*14
01439         (PB-I-POLICY-IS-REISSUE OR PB-I-REIN-ONLY OR                 CL*14
122002         PB-I-POLICY-IS-MONTHLY OR
01440          PB-I-POLICY-IS-DECLINED OR PB-I-POLICY-IS-VOIDED OR         CL*14
01441          PB-I-UNDERWRITE-POLICY)                                     CL*14
01442            GO TO 1010-PNDB-BILLING-PROCESS-LOOP                      CL*31
01443      ELSE                                                         EL642
01444          IF (PB-CANCELLATION)                                        CL**5
01445            AND  (PB-CI-LF-POLICY-IS-REISSUE                          CL**5
01446              OR  PB-CI-AH-POLICY-IS-REISSUE                          CL**5
122002             OR  PB-CI-LF-POLICY-IS-MONTHLY
122002             OR  PB-CI-AH-POLICY-IS-MONTHLY
01447              OR  PB-CI-LF-POLICY-IS-DECLINED                         CL*14
01448              OR  PB-CI-AH-POLICY-IS-VOID                             CL*14
01449              OR  PB-CI-LF-REIN-ONLY                                  CL**5
01450              OR  PB-CI-AH-REIN-ONLY)                                 CL**5
01451                GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                 CL**5
01452                                                                   EL642
01453      IF PB-CREDIT-ACCEPT-DT  NOT =  LOW-VALUES                       CL**5
01454          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                       CL**5
01455                                                                   EL642
01456      IF PB-RECORD-ON-HOLD                                         EL642
01457          PERFORM 1040-UPDATE-BILLING-STATISTICS  THRU  1049-EXIT     CL**5
01458          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                    EL642
01459                                                                   EL642
01460      IF PB-FATAL-ERRORS                                           EL642
01461        OR  PB-UNFORCED-ERRORS                                        CL**5
01462          PERFORM 1040-UPDATE-BILLING-STATISTICS  THRU  1049-EXIT     CL**5
01463          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                       CL**5
01464                                                                   EL642
01465      MOVE 'Y'                    TO  PI-ACCT-BILLED-SW               CL**5
01466                                      PI-DATA-BILLED-SW.              CL**5
01467                                                                   EL642
01468      PERFORM 1040-UPDATE-BILLING-STATISTICS  THRU  1049-EXIT.        CL**5
01469                                                                   EL642
01470      PERFORM 1030-COMPUTE-COMMISSION-TOTALS  THRU  1039-EXIT.        CL**5
01471                                                                   EL642
01472      IF PI-BILL                                                      CL**5
01473        OR  PI-REBILLING                                              CL**5
01474          PERFORM 4200-PNDB-REWRITE THRU 4290-EXIT.                EL642
01475                                                                   EL642
01476      GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                        EL642
01477  EJECT                                                               CL**5
01478  1030-COMPUTE-COMMISSION-TOTALS.                                  EL642
01479      IF PB-CANCELLATION                                           EL642
01480          GO TO 1035-COMPUTE-CANCEL-COMM.                          EL642
01481                                                                   EL642
01482      PERFORM 4400-FIND-COMMISSION  THRU  4490-EXIT.                  CL**5
01483                                                                   EL642
01484      IF PB-OVERRIDE-LIFE                                             CL**5
01485        OR  PB-OVERRIDE-BOTH                                          CL**5
01486          COMPUTE WS-I-LF-PREMIUM-AMT = PB-I-LF-PREM-CALC +           CL**5
01487                                        PB-I-LF-ALT-PREM-CALC         CL**5
01488      ELSE                                                            CL**5
01489          COMPUTE WS-I-LF-PREMIUM-AMT = PB-I-LF-PREMIUM-AMT +         CL**5
01490                                        PB-I-LF-ALT-PREMIUM-AMT.      CL**5
01491                                                                      CL**5
01492      IF PB-OVERRIDE-AH                                               CL**5
01493        OR  PB-OVERRIDE-BOTH                                          CL**5
01494          MOVE PB-I-AH-PREM-CALC  TO  WS-I-AH-PREMIUM-AMT             CL**5
01495      ELSE                                                            CL**5
01496          MOVE PB-I-AH-PREMIUM-AMT                                    CL**5
01497                                  TO  WS-I-AH-PREMIUM-AMT.            CL**5
01498                                                                      CL**5
01499      IF PB-GA-BILL-DT (WS-GA-LEVEL)  =  (LOW-VALUES OR SPACES)       CL**5
01500        OR  PI-TOT-REBILL                                             CL**5
01501          COMPUTE PI-UNPAID-NET-PREM = PI-UNPAID-NET-PREM +           CL**5
01502              WS-I-LF-PREMIUM-AMT + WS-I-AH-PREMIUM-AMT               CL**5
01503          COMPUTE WS-ACCT-UNPAID-NET-PREM =                        EL642
01504              (WS-I-LF-PREMIUM-AMT + WS-I-AH-PREMIUM-AMT) +           CL**5
01505               WS-ACCT-UNPAID-NET-PREM.                            EL642
01506                                                                   EL642
01507      COMPUTE PI-PREMIUM = PI-PREMIUM +                            EL642
01508          (WS-I-LF-PREMIUM-AMT + WS-I-AH-PREMIUM-AMT).                CL**5
01509                                                                   EL642
01510      COMPUTE PI-LF-ISS-COMP ROUNDED =                             EL642
01511          (WS-I-LF-PREMIUM-AMT * WS-GA-LF-COM).                       CL**5
01512                                                                   EL642
01513      COMPUTE PI-AH-ISS-COMP ROUNDED =                             EL642
01514          (WS-I-AH-PREMIUM-AMT  * WS-GA-AH-COM).                      CL**5
01515                                                                   EL642
01516      IF PB-GA-BILL-DT (WS-GA-LEVEL)  =  (LOW-VALUES OR SPACES)       CL**5
01517        OR  PI-TOT-REBILL                                             CL**5
01518          COMPUTE WS-ACCT-LF-OVERWRITE =                           EL642
01519              (PI-LF-ISS-COMP + WS-ACCT-LF-OVERWRITE)              EL642
01520          COMPUTE WS-ACCT-AH-OVERWRITE =                           EL642
01521              (PI-AH-ISS-COMP + WS-ACCT-AH-OVERWRITE)                 CL**5
01522          COMPUTE PI-COMP-UNPAID-PREM = PI-COMP-UNPAID-PREM +         CL**5
01523              PI-LF-ISS-COMP + PI-AH-ISS-COMP.                        CL**5
01524                                                                   EL642
01525      COMPUTE PI-TOT-ISS-COMP = PI-TOT-ISS-COMP +                  EL642
01526          (PI-LF-ISS-COMP + PI-AH-ISS-COMP).                       EL642
01527                                                                   EL642
01528  1033-COMPUTE-ACCOUNT-COMM.                                       EL642
01529                                                                   EL642
01530      COMPUTE WS-ACCT-NET-PREM ROUNDED = WS-ACCT-NET-PREM +        EL642
01531          (WS-I-LF-PREMIUM-AMT + WS-I-AH-PREMIUM-AMT).                CL**5
01532                                                                   EL642
01533      IF PI-SAV-AGENT  NOT =  PI-SAV-REMIT-TO                         CL**5
01534          GO TO 1039-EXIT.                                         EL642
01535                                                                   EL642
01536      COMPUTE WS-ACCT-COMP ROUNDED =                               EL642
01537          (WS-I-LF-PREMIUM-AMT * PB-I-LIFE-COMMISSION) +              CL**5
01538          (WS-I-AH-PREMIUM-AMT * PB-I-AH-COMMISSION)   +              CL**5
01539           WS-ACCT-COMP.                                           EL642
01540                                                                   EL642
01541      GO TO 1039-EXIT.                                             EL642
01542                                                                   EL642
01543  1035-COMPUTE-CANCEL-COMM.                                        EL642
01544      IF PB-CI-CERT-HAS-ERCOMM-ENTRY                                  CL**5
01545          PERFORM 4700-PROCESS-COMM-EXCEPTION  THRU  4790-EXIT        CL**5
01546      ELSE                                                         EL642
01547          PERFORM 4400-FIND-COMMISSION  THRU  4490-EXIT.              CL**5
01548                                                                   EL642
01549      IF PB-OVERRIDE-LIFE                                             CL**5
01550        OR  PB-OVERRIDE-BOTH                                          CL**5
01551          MOVE PB-C-LF-REF-CALC   TO  WS-C-LF-CANCEL-AMT              CL**5
01552      ELSE                                                            CL**5
01553          MOVE PB-C-LF-CANCEL-AMT                                     CL**5
01554                                  TO  WS-C-LF-CANCEL-AMT.             CL**5
01555                                                                      CL**5
01556      IF PB-OVERRIDE-AH                                               CL**5
01557        OR  PB-OVERRIDE-BOTH                                          CL**5
01558          MOVE PB-C-AH-REF-CALC   TO  WS-C-AH-CANCEL-AMT              CL**5
01559      ELSE                                                            CL**5
01560          MOVE PB-C-AH-CANCEL-AMT                                     CL**5
01561                                  TO  WS-C-AH-CANCEL-AMT.             CL**5
01562                                                                      CL**5
01563      IF PB-GA-BILL-DT (WS-GA-LEVEL)  =  LOW-VALUES  OR  SPACES       CL**5
01564        OR  PI-TOT-REBILL                                             CL**5
01565          COMPUTE PI-UNPAID-NET-PREM = PI-UNPAID-NET-PREM -           CL**5
01566              (WS-C-LF-CANCEL-AMT + WS-C-AH-CANCEL-AMT)               CL**5
01567          COMPUTE WS-ACCT-UNPAID-NET-PREM =                        EL642
01568              WS-ACCT-UNPAID-NET-PREM -                               CL**5
01569              (WS-C-LF-CANCEL-AMT + WS-C-AH-CANCEL-AMT).              CL**5
01570                                                                   EL642
01571      COMPUTE PI-PREMIUM = PI-PREMIUM -                            EL642
01572          (WS-C-LF-CANCEL-AMT + WS-C-AH-CANCEL-AMT).                  CL**5
01573                                                                   EL642
01574      MOVE  PB-CERT-EFF-DT         TO  DC-BIN-DATE-1.                 CL*15
01575      MOVE  PB-C-LF-CANCEL-DT      TO  DC-BIN-DATE-2.                 CL*19
01576      MOVE  '1'                    TO  DC-OPTION-CODE.                CL*19
01577      PERFORM  8500-DATE-CONVERT  THRU  8500-EXIT.                    CL*15
01578      MOVE  DC-ELAPSED-MONTHS      TO  MONTHS-DIFF-LF.                CL*19
01579                                                                   EL642
01580      IF DC-ODD-DAYS-OVER  GREATER THAN   ZEROS                       CL*31
01581          ADD  +1                  TO  MONTHS-DIFF-LF.                CL*19
01582                                                                      CL*15
01583      MOVE  PB-CERT-EFF-DT         TO  DC-BIN-DATE-1.                 CL*19
01584      MOVE  PB-C-AH-CANCEL-DT      TO  DC-BIN-DATE-2.                 CL*19
01585      MOVE  '1'                    TO  DC-OPTION-CODE.                CL*19
01586      PERFORM  8500-DATE-CONVERT  THRU  8500-EXIT.                    CL*19
01587      MOVE  DC-ELAPSED-MONTHS      TO  MONTHS-DIFF-AH.                CL*19
01588                                                                      CL*18
01589      IF DC-ODD-DAYS-OVER  GREATER THAN   ZEROS                       CL*31
01590          ADD  +1                  TO  MONTHS-DIFF-AH.                CL*19
01591                                                                      CL*15
01592      MOVE ZEROS                   TO  SUB.                           CL*15
01593      MOVE ZEROS                   TO  PI-LF-CAN-COMP                 CL*20
01594                                       PI-AH-CAN-COMP.                CL*20
01595                                                                      CL*15
01596  1036-COMPUTE-GA-LOOP.                                               CL*15
01597                                                                      CL*15
01598      ADD  +1                      TO  SUB.                           CL*15
01599                                                                      CL*15
01600      IF SUB > +10                                                    CL*31
01601          MOVE  ZEROS              TO  SUB                            CL*15
01602          GO TO  1037-CONTINUE.                                       CL*15
01603                                                                      CL*21
01604      IF AM-AGT (SUB)   NOT  EQUAL  PI-SAV-AGENT                      CL*31
01605          GO TO  1036-COMPUTE-GA-LOOP.                                CL*21
01606                                                                      CL*15
01607      IF AM-COM-TYP (SUB) = 'O' OR 'P' OR 'G' OR 'B'                  CL*31
01608          NEXT SENTENCE                                               CL*26
01609      ELSE                                                            CL*26
01610          GO TO  1036-COMPUTE-GA-LOOP.                                CL*15
01611                                                                      CL*16
01612      IF AM-COMM-CHARGEBACK (SUB) NOT NUMERIC                         CL*31
01613          MOVE  ZEROS               TO  AM-COMM-CHARGEBACK (SUB).     CL*16
01614                                                                      CL*15
01615      IF AM-COMM-CHARGEBACK (SUB) = '99'                              CL*31
01616          MOVE  ZEROS           TO  PI-LF-CAN-COMP                    CL*17
01617          GO TO 1037-CHECK-AH-COMM.                                   CL*19
01618                                                                      CL*17
01619      IF (MONTHS-DIFF-LF  GREATER THAN  AM-COMM-CHARGEBACK (SUB))     CL*31
01620                               AND                                    CL*15
01621         (AM-COMM-CHARGEBACK (SUB)  NOT  EQUAL   ZEROS)               CL*31
01622          MOVE  ZEROS               TO  PI-LF-CAN-COMP                CL*15
01623      ELSE                                                            CL*15
01624          COMPUTE PI-LF-CAN-COMP ROUNDED = PI-LF-CAN-COMP +           CL*19
01625              (WS-C-LF-CANCEL-AMT * WS-GA-LF-COM).                    CL*15
01626                                                                      CL*19
01627  1037-CHECK-AH-COMM.                                                 CL*19
01628                                                                      CL*17
01629      IF AM-COMM-CHARGEBACK (SUB)    EQUAL   '99'                     CL*31
01630          MOVE  ZEROS           TO  PI-AH-CAN-COMP                    CL*17
01631          GO TO 1036-COMPUTE-GA-LOOP.                                 CL*19
01632                                                                      CL*15
01633      IF (MONTHS-DIFF-AH GREATER THAN  AM-COMM-CHARGEBACK (SUB))      CL*31
01634                               AND                                    CL*15
01635         (AM-COMM-CHARGEBACK (SUB)  NOT  EQUAL   ZEROS)               CL*31
01636          MOVE  ZEROS               TO  PI-AH-CAN-COMP                CL*15
01637      ELSE                                                            CL*15
01638          COMPUTE PI-AH-CAN-COMP ROUNDED =  PI-AH-CAN-COMP +          CL*19
01639              (WS-C-AH-CANCEL-AMT  * WS-GA-AH-COM).                   CL*15
01640                                                                      CL*19
01641      GO TO 1036-COMPUTE-GA-LOOP.                                     CL*19
01642                                                                      CL*15
01643  1037-CONTINUE.                                                      CL*15
01644                                                                   EL642
01645      IF PB-GA-BILL-DT (WS-GA-LEVEL)  =  (LOW-VALUES  OR  SPACES)     CL**5
01646        OR  PI-TOT-REBILL                                             CL**5
01647          COMPUTE WS-ACCT-LF-OVERWRITE =                           EL642
01648              (WS-ACCT-LF-OVERWRITE - PI-LF-CAN-COMP)              EL642
01649          COMPUTE WS-ACCT-AH-OVERWRITE =                           EL642
01650              (WS-ACCT-AH-OVERWRITE - PI-AH-CAN-COMP)                 CL**5
01651          COMPUTE PI-COMP-UNPAID-PREM = PI-COMP-UNPAID-PREM -         CL**5
01652              PI-LF-CAN-COMP - PI-AH-CAN-COMP.                        CL**5
01653                                                                   EL642
01654      COMPUTE PI-TOT-CAN-COMP = PI-TOT-CAN-COMP +                  EL642
01655          (PI-LF-CAN-COMP + PI-AH-CAN-COMP).                       EL642
01656                                                                   EL642
01657  1038-COMPUTE-ACCOUNT-COMM.                                       EL642
01658                                                                   EL642
01659      COMPUTE WS-ACCT-NET-PREM = WS-ACCT-NET-PREM -                EL642
01660          (WS-C-LF-CANCEL-AMT + WS-C-AH-CANCEL-AMT).                  CL**5
01661                                                                   EL642
01662      IF PI-SAV-AGENT  NOT =  PI-SAV-REMIT-TO                         CL**5
01663          GO TO 1039-EXIT.                                         EL642
01664                                                                   EL642
01665      MOVE ZEROS                   TO  SUB.                           CL*17
01666                                                                      CL*17
01667  103X-ACCOUNT-LOOP.                                                  CL*15
01668                                                                      CL*15
01669      ADD  +1                      TO  SUB.                           CL*15
01670                                                                      CL*15
01671      IF SUB > +10                                                    CL*31
01672          MOVE  ZEROS              TO  SUB                            CL*15
01673          GO TO  1039-EXIT.                                           CL*15
01674                                                                      CL*15
01675      IF AM-COM-TYP (SUB) = 'C' OR 'D' OR 'F' OR 'S'                  CL*31
01676          NEXT SENTENCE                                               CL*26
01677      ELSE                                                            CL*26
01678          GO TO  103X-ACCOUNT-LOOP.                                   CL*15
01679                                                                      CL*18
01680      IF AM-COMM-CHARGEBACK (SUB)    NOT NUMERIC                      CL*31
01681          MOVE  ZEROS               TO  AM-COMM-CHARGEBACK (SUB).     CL*18
01682                                                                      CL*15
01683      IF AM-COMM-CHARGEBACK (SUB)   EQUAL   '99'                      CL*31
01684          MOVE  ZEROS          TO  WS-C-LF-CANCEL-AMT                 CL*19
01685          GO TO 103X-CHECK-AH-COMM.                                   CL*19
01686                                                                      CL*17
01687      IF (MONTHS-DIFF-LF  GREATER THAN  AM-COMM-CHARGEBACK (SUB))     CL*31
01688                               AND                                    CL*15
01689         (AM-COMM-CHARGEBACK (SUB)  NOT  EQUAL   ZEROS)               CL*31
01690          MOVE  ZEROS              TO  WS-C-LF-CANCEL-AMT.            CL*17
01691                                                                      CL*17
01692  103X-CHECK-AH-COMM.                                                 CL*19
01693                                                                      CL*19
01694      IF AM-COMM-CHARGEBACK (SUB)   EQUAL   '99'                      CL*31
01695          MOVE  ZEROS          TO  WS-C-AH-CANCEL-AMT                 CL*19
01696          GO TO 103X-COMPUTE-ACCT-TOT.                                CL*19
01697                                                                      CL*17
01698      IF (MONTHS-DIFF-AH  GREATER THAN  AM-COMM-CHARGEBACK (SUB))     CL*31
01699                               AND                                    CL*15
01700         (AM-COMM-CHARGEBACK (SUB)  NOT  EQUAL   ZEROS)               CL*31
01701          MOVE  ZEROS              TO  WS-C-AH-CANCEL-AMT.            CL*17
01702                                                                      CL*17
01703  103X-COMPUTE-ACCT-TOT.                                              CL*19
01704                                                                      CL*19
01705      COMPUTE WS-ACCT-COMP   ROUNDED  =  WS-ACCT-COMP  -              CL*17
01706          ((WS-C-LF-CANCEL-AMT * PB-CI-LIFE-COMMISSION) +             CL*17
01707           (WS-C-AH-CANCEL-AMT * PB-CI-AH-COMMISSION)).               CL*17
01708                                                                      CL*19
01709      GO TO 103X-ACCOUNT-LOOP.                                        CL*19
01710                                                                   EL642
01711  1039-EXIT.                                                       EL642
01712      EXIT.                                                        EL642
01713  EJECT                                                               CL**5
01714  1040-UPDATE-BILLING-STATISTICS.                                  EL642
01715      IF PB-ISSUE                                                     CL**5
01716          IF PB-FATAL-ERRORS                                          CL**5
01717            OR  PB-UNFORCED-ERRORS                                    CL**5
01718            OR  PB-RECORD-ON-HOLD                                     CL**5
01719              ADD +1              TO  PI-ISSUES-INER                  CL**5
01720              GO TO 1049-EXIT.                                     EL642
01721                                                                   EL642
01722      IF PB-CANCELLATION                                              CL**5
01723          IF PB-FATAL-ERRORS                                          CL**5
01724            OR  PB-UNFORCED-ERRORS                                    CL**5
01725            OR  PB-RECORD-ON-HOLD                                     CL**5
01726              ADD +1              TO  PI-CANCELS-INER                 CL**5
01727              GO TO 1049-EXIT.                                     EL642
01728                                                                   EL642
01729      IF PB-ISSUE                                                     CL**5
01730          IF PB-GA-BILL-DT (WS-GA-LEVEL)  =                           CL**5
01731                 (LOW-VALUES OR SPACES)                               CL**5
01732            OR  PI-TOT-REBILL                                         CL**5
01733              ADD +1              TO  PI-ISSUES-BILLED                CL**5
01734              GO TO 1049-EXIT.                                     EL642
01735                                                                   EL642
01736      IF PB-CANCELLATION                                              CL**5
01737          IF PB-GA-BILL-DT (WS-GA-LEVEL)  =                           CL**5
01738                 (LOW-VALUES OR SPACES)                               CL**5
01739            OR  PI-TOT-REBILL                                         CL**5
01740              ADD +1              TO  PI-CANCELS-BILLED               CL**5
01741              GO TO 1049-EXIT.                                     EL642
01742                                                                   EL642
01743      IF PB-ISSUE                                                     CL**5
01744          IF PB-GA-BILL-DT (WS-GA-LEVEL)  NOT =  LOW-VALUES           CL**5
01745            AND  SPACES                                               CL**5
01746              ADD +1              TO  PI-ISSUES-PREV                  CL**5
01747              GO TO 1049-EXIT.                                        CL**5
01748                                                                      CL**5
01749      IF PB-CANCELLATION                                              CL**5
01750          IF PB-GA-BILL-DT (WS-GA-LEVEL)  NOT =  LOW-VALUES           CL**5
01751            AND  SPACES                                               CL**5
01752              ADD +1              TO  PI-CANCELS-PREV                 CL**5
01753              GO TO 1049-EXIT.                                        CL**5
01754                                                                      CL**5
01755      ADD +1                      TO  PI-UNKNOWN.                     CL**5
01756                                                                   EL642
01757  1049-EXIT.                                                       EL642
01758      EXIT.                                                        EL642
01759  EJECT                                                               CL**5
01760  1100-PYAJ-BILLING-PROCESS.                                       EL642
01761 ******************************************************************EL642
01762 *   PROCESS THE ACCOUNTS PAYMENTS AND ADUSTMENTS. IF THE GENERAL *EL642
01763 *   AGENT IS NOT FINANCIALLY RESPONSIBLE FOR THE ACCOUNT DO NOT  *EL642
01764 *   PROCESS THE ACCOUNT'S PAYMENTS AND ADJUSTMENTS.              *EL642
01765 *                                                                *EL642
01766 *   1.  PROCESS THE ACCOUNT'S PAYMENTS AND ADUSTMENTS.           *EL642
01767 *                                                                *EL642
01768 *   2.  IF THE GENERAL AGENT IS NOT FINANCIALLY RESPONSIBLE FOR  *EL642
01769 *       THE ACCOUNT, DO NOT PROCESS THE ACCOUNT'S PAYMENTS AND   *EL642
01770 *       ADJUSTMENTS.                                             *EL642
01771 *                                                                *EL642
01772 *   3.  IF PRINTED BILL IS NOT PRODUCED, DO NOT PROCESS THE      *EL642
01773 *       ACCOUNT'S PAYMENTS AND ADJUSTMENTS.                      *EL642
01774 ******************************************************************EL642
01775                                                                   EL642
01776      IF PI-SAV-AGENT  =  PI-SAV-REMIT-TO                             CL**5
01777          NEXT SENTENCE                                            EL642
01778      ELSE                                                            CL**5
01779          GO TO 1190-EXIT.                                         EL642
01780                                                                   EL642
01781      IF PI-UPDATE-FILES                                           EL642
01782        OR  (PI-PREVIEW  AND  APRODSWI = 'Y')                         CL**5
01783          NEXT SENTENCE                                               CL**5
01784      ELSE                                                         EL642
01785          GO TO 1190-EXIT.                                            CL**5
01786                                                                   EL642
01787  1105-PYAJ-BILLING-STARTBR-LOOP.                                  EL642
01788      MOVE 'A'                    TO  COMPENSATION-SW.             EL642
01789                                                                   EL642
01790      PERFORM 4500-PYAJ-START-BROWSE  THRU  4590-EXIT.                CL**5
01791                                                                   EL642
01792      IF PYAJ-EOF                                                  EL642
01793          GO TO 1190-EXIT.                                         EL642
01794                                                                   EL642
01795  1110-PYAJ-BILLING-READ-LOOP.                                     EL642
01796      PERFORM 4600-PYAJ-READ-NEXT  THRU  4690-EXIT.                   CL**5
01797                                                                   EL642
01798      IF PYAJ-EOF                                                  EL642
01799          PERFORM 4800-PYAJ-END-BROWSE                             EL642
01800          GO TO 1190-EXIT.                                         EL642
01801                                                                   EL642
01802      IF PY-COMPANY-CD  =  ERPYAJ-BR-COMP-CD                          CL**5
01803        AND  PY-CARRIER  =  ERPYAJ-BR-CARRIER                         CL**5
01804        AND  PY-GROUPING  =  ERPYAJ-BR-GROUPING                       CL**5
01805        AND  PY-FIN-RESP  =  ERPYAJ-BR-FIN-RESP                       CL**5
01806        AND  PY-ACCOUNT  =  ERPYAJ-BR-ACCOUNT                         CL**5
01807          NEXT SENTENCE                                            EL642
01808      ELSE                                                         EL642
01809          PERFORM 4800-PYAJ-END-BROWSE                             EL642
01810          GO TO 1190-EXIT.                                         EL642
01811                                                                   EL642
01812      IF PY-CREDIT-ACCEPT-DT  NOT =  LOW-VALUES                       CL**5
01813          GO TO 1110-PYAJ-BILLING-READ-LOOP.                       EL642
01814                                                                   EL642
01815      IF PY-RECORD-TYPE  = 'R' OR 'D' OR 'C' OR 'S' OR 'T' OR 'U'     CL*15
01816                               OR 'Z'                                 CL*15
01817          NEXT SENTENCE                                            EL642
01818      ELSE                                                         EL642
01819          GO TO 1110-PYAJ-BILLING-READ-LOOP.                       EL642
01820                                                                   EL642
01821      IF PY-VOID-SW  NOT =  SPACES                                    CL**5
01822          GO TO 1110-PYAJ-BILLING-READ-LOOP.                       EL642
01823                                                                   EL642
01824      IF PI-AR-PROCESSING                                             CL**8
01825         IF PY-AR-DATE  =  LOW-VALUES                                 CL**8
01826            NEXT SENTENCE                                             CL**8
01827         ELSE                                                         CL**8
01828            GO TO 1110-PYAJ-BILLING-READ-LOOP                         CL**8
01829      ELSE                                                         EL642
01830         IF (PY-BILLED-DATE  =  LOW-VALUES AND                        CL**9
01831             PY-AR-DATE = LOW-VALUES)                                 CL**9
01832            NEXT SENTENCE                                             CL**8
01833         ELSE                                                         CL**8
01834            GO TO 1110-PYAJ-BILLING-READ-LOOP.                        CL**8
01835                                                                   EL642
01836      IF PY-REMIT-RECEIVED OR PY-ADJ-REM-RECEIVED OR                  CL*15
01837         PY-DEPOSIT OR PY-ADJ-DEPOSIT OR                              CL*15
01838         PY-ADD-TO-BALANCE                                            CL*15
01839          ADD PY-ENTRY-AMT        TO  WS-ACCT-PAY-ADJS                CL**5
01840      ELSE                                                         EL642
01841          SUBTRACT PY-ENTRY-AMT   FROM  WS-ACCT-PAY-ADJS.             CL**5
01842                                                                   EL642
01843      GO TO 1110-PYAJ-BILLING-READ-LOOP.                           EL642
01844                                                                   EL642
01845  1190-EXIT.                                                       EL642
01846      EXIT.                                                        EL642
01847  EJECT                                                               CL**5
01848  1200-PYAJ-BILLING-COMPLETE.                                      EL642
01849 ******************************************************************EL642
01850 *      1. IF PRINTED BILL IS PRODUCED PRINT THE GENERAL AGENT'S  *EL642
01851 *         TOTALS.                                                *EL642
01852 *                                                                *EL642
01853 *      2. PROCESS THE GENERAL AGENT'S PAYMENTS AND ADJUSTMENTS.  *EL642
01854 ******************************************************************EL642
01855                                                                   EL642
01856      IF PI-DATA-BILLED                                               CL**5
01857          NEXT SENTENCE                                               CL**5
01858      ELSE                                                            CL**5
01859          MOVE ER-2913            TO  EMI-ERROR                    EL642
01860          MOVE -1                 TO  AACCT1L                      EL642
01861          MOVE AL-UABON           TO  AACCT1A                      EL642
01862          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                 CL**5
01863                                                                   EL642
01864      IF RETURNED-FROM  =  SPACES                                     CL*25
01865          IF PI-UPDATE-FILES                                          CL*25
01866            OR  (PI-PREVIEW  AND  APRODSWI = 'Y')                     CL*25
01867              IF PI-ACCT-BILLED                                       CL*25
01868                  MOVE 'TB'       TO  BILLING-DETAIL-TYPE             CL*25
01869                  PERFORM 3000-WRITE-BILLING-DETAIL                   CL*25
01870                          THRU  3990-EXIT                             CL*25
01871              ELSE                                                    CL*25
01872                  MOVE 'TS'       TO  BILLING-DETAIL-TYPE             CL**5
01873                  PERFORM 3000-WRITE-BILLING-DETAIL                   CL**5
01874                          THRU  3990-EXIT.                            CL*25
01875                                                                   EL642
01876  1205-PYAJ-BILLING-STARTBR-LOOP.                                  EL642
01877      MOVE PI-SAV-AGENT           TO  PI-SAV-FIN-RESP.             EL642
01878      MOVE 'G'                    TO  COMPENSATION-SW.             EL642
01879                                                                   EL642
01880      PERFORM 4500-PYAJ-START-BROWSE  THRU  4590-EXIT.                CL**5
01881                                                                      CL**5
01882      IF PYAJ-EOF                                                  EL642
01883          GO TO 1260-BILLING-COMPLETE.                             EL642
01884                                                                   EL642
01885  1210-PYAJ-BILLING-READ-LOOP.                                     EL642
01886      PERFORM 4600-PYAJ-READ-NEXT  THRU  4690-EXIT.                   CL**5
01887                                                                      CL**5
01888      IF PYAJ-EOF                                                  EL642
01889          GO TO 1250-END-AGENTS-PAY-ADJS.                          EL642
01890                                                                   EL642
01891      IF NOT PI-AR-PROCESSING                                         CL*25
01892          IF PY-COMPANY-CD  =  ERPYAJ-BR-COMP-CD                      CL*25
01893            AND  PY-CARRIER  =  ERPYAJ-BR-CARRIER                     CL*25
01894            AND  PY-GROUPING  =  ERPYAJ-BR-GROUPING                   CL*25
01895            AND  PY-FIN-RESP  =  ERPYAJ-BR-FIN-RESP                   CL*25
01896            AND  PY-ACCOUNT  =  ERPYAJ-BR-ACCOUNT                     CL*25
01897              GO TO 1230-CHECK-PYAJ-ENTRY                             CL*25
01898          ELSE                                                        CL*25
01899              GO TO 1250-END-AGENTS-PAY-ADJS                          CL*25
01900      ELSE                                                         EL642
01901          IF PY-COMPANY-CD  =  ERPYAJ-BR-COMP-CD                      CL*25
01902            AND  PY-CARRIER  =  ERPYAJ-BR-CARRIER                     CL*25
01903            AND  PY-GROUPING  =  ERPYAJ-BR-GROUPING                   CL*25
01904            AND  PY-FIN-RESP  =  ERPYAJ-BR-FIN-RESP                   CL*25
01905              NEXT SENTENCE                                           CL*25
01906          ELSE                                                        CL*25
01907              GO TO 1250-END-AGENTS-PAY-ADJS.                         CL*25
01908                                                                      CL*25
01909       IF PY-PMT-APPLIED = 'O' OR                                     CL*25
01910          PY-ACCOUNT = LOW-VALUES                                     CL*25
01911           NEXT SENTENCE                                              CL*25
01912       ELSE                                                           CL*25
01913           GO TO 1210-PYAJ-BILLING-READ-LOOP.                         CL*25
01914                                                                      CL*25
01915  1230-CHECK-PYAJ-ENTRY.                                              CL*25
01916                                                                   EL642
01917      IF PY-CREDIT-ACCEPT-DT  NOT =  LOW-VALUES                       CL**5
01918          GO TO 1210-PYAJ-BILLING-READ-LOOP.                       EL642
01919                                                                   EL642
01920      IF PY-RECORD-TYPE  = 'R' OR 'D' OR 'C' OR 'S' OR 'T' OR 'U'     CL*15
01921                               OR 'Z'                                 CL*15
01922          NEXT SENTENCE                                            EL642
01923      ELSE                                                         EL642
01924          GO TO 1210-PYAJ-BILLING-READ-LOOP.                       EL642
01925                                                                   EL642
01926      IF PY-VOID-SW  NOT =  SPACES                                    CL**5
01927          GO TO 1210-PYAJ-BILLING-READ-LOOP.                       EL642
01928                                                                   EL642
01929      IF PI-AR-PROCESSING                                             CL**8
01930         IF PY-AR-DATE  =  LOW-VALUES                                 CL**8
01931            OR  PI-TOT-REBILL                                         CL**8
01932            OR  RETURNED-FROM  =  XCTL-PYAJ                           CL**9
01933                NEXT SENTENCE                                         CL**8
01934         ELSE                                                         CL**8
01935            GO TO 1210-PYAJ-BILLING-READ-LOOP                         CL**8
01936      ELSE                                                         EL642
01937         IF (PY-BILLED-DATE  =  LOW-VALUES AND                        CL**9
01938             PY-AR-DATE = LOW-VALUES)                                 CL*10
01939            OR  PI-TOT-REBILL                                         CL**8
01940            OR  RETURNED-FROM  =  XCTL-PYAJ                           CL**9
01941            NEXT SENTENCE                                             CL**8
01942         ELSE                                                         CL**8
01943            GO TO 1210-PYAJ-BILLING-READ-LOOP.                        CL**8
01944                                                                   EL642
01945      IF PY-REMIT-RECEIVED OR PY-ADJ-REM-RECEIVED OR                  CL*15
01946         PY-DEPOSIT OR PY-ADJ-DEPOSIT OR                              CL*15
01947         PY-ADD-TO-BALANCE                                            CL*15
01948          ADD PY-ENTRY-AMT        TO  PI-REMITTED                  EL642
01949      ELSE                                                         EL642
01950          IF PY-CHARGE-TO-AGENT                                       CL**5
01951            OR  PY-ADJ-CHG-TO-AGT                                     CL*15
01952              IF PY-GA-CHECK                                       EL642
01953                  ADD PY-ENTRY-AMT  TO  PI-DISBURSED                  CL**5
01954              ELSE                                                 EL642
01955                  ADD PY-ENTRY-AMT  TO  PI-ADJUSTMNTS.                CL**5
01956                                                                   EL642
01957      IF RETURNED-FROM  =  SPACES                                     CL**5
01958          IF PI-UPDATE-FILES                                          CL**5
01959            OR  (PI-PREVIEW  AND  APRODSWI = 'Y')                     CL**5
01960              MOVE 'BP'           TO  BILLING-DETAIL-TYPE             CL**5
01961              PERFORM 3000-WRITE-BILLING-DETAIL  THRU  3990-EXIT.     CL**5
01962                                                                   EL642
01963      GO TO 1210-PYAJ-BILLING-READ-LOOP.                           EL642
01964                                                                   EL642
01965  1250-END-AGENTS-PAY-ADJS.                                        EL642
01966      PERFORM 4800-PYAJ-END-BROWSE.                                EL642
01967                                                                      CL*25
01968      IF PI-DATA-BILLED                                               CL*25
01969          NEXT SENTENCE                                               CL*25
01970      ELSE                                                            CL*25
01971          MOVE ER-2913            TO  EMI-ERROR                       CL*25
01972          MOVE -1                 TO  AACCT1L                         CL*25
01973          MOVE AL-UABON           TO  AACCT1A                         CL*25
01974          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                 CL*25
01975                                                                   EL642
01976  1260-BILLING-COMPLETE.                                           EL642
01977      IF WS-UPDATE-FILES                                           EL642
01978          PERFORM 6500-REWRITE-CROSS-REFERENCE.                       CL**5
01979                                                                   EL642
01980      IF EMI-NO-ERRORS                                             EL642
01981          MOVE 0000               TO  EMI-ERROR                       CL**5
01982          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                 CL**5
01983                                                                   EL642
01984      MOVE -1                     TO  AAGENTL.                        CL**5
01985                                                                   EL642
01986      PERFORM 5000-FORMAT-SCREEN  THRU  5090-EXIT.                    CL**5
01987                                                                   EL642
01988      GO TO 8100-SEND-INITIAL-MAP.                                 EL642
01989  EJECT                                                               CL**5
01990  2000-VOID-BILLING.                                               EL642
01991 ******************************************************************EL642
01992 *     THIS SECTION WILL RESET THE GA-BILLING-FLAGS IN THE        *EL642
01993 *     PENDING BUSINESS RECORDS AND RESET THE LAST BILLED         *EL642
01994 *     DATE IN THE GENERAL AGENT'S CROSS-REFERENCE-FILE TO        *EL642
01995 *     LOW-VALUES.                                                *EL642
01996 ******************************************************************EL642
01997                                                                   EL642
01998      MOVE SPACES                 TO  ELCNTL-KEY.                     CL**5
01999      MOVE '1'                    TO  ELCNTL-REC-TYPE.                CL**5
02000                                                                   EL642
02001      PERFORM 6100-READ-CONTROL-FILE  THRU  6190-EXIT.                CL**5
02002                                                                   EL642
02003      IF EMI-ERROR  NOT =  ZEROS                                      CL**5
02004          GO TO 8200-SEND-DATAONLY.                                   CL**5
02005                                                                      CL**5
02006      MOVE CF-CURRENT-MONTH-END   TO  DC-BIN-DATE-1.                  CL**5
02007      MOVE SPACE                  TO  DC-OPTION-CODE.                 CL**5
02008                                                                      CL**5
02009      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                     CL**5
02010                                                                      CL**5
02011      MOVE DC-GREG-DATE-1-EDIT    TO  PI-MONTH-END-DATE.              CL**5
02012                                                                      CL**5
02013      PERFORM 6400-READ-CROSS-REFERENCE  THRU  6490-EXIT.             CL**5
02014                                                                      CL**5
02015      MOVE +0                     TO  GX-SUB.                         CL**5
02016                                                                   EL642
02017  2005-GXRF-BILLING-PROCESS-LOOP.                                  EL642
02018      ADD +1                      TO  GX-SUB.                         CL**5
02019                                                                   EL642
02020      IF GX-SUB  GREATER THAN  GX-AGENT-POINTER-CNT                   CL**5
02021          GO TO 2100-VOID-PNDB-COMPLETE.                              CL**5
02022                                                                   EL642
02023      MOVE GX-AM-CARRIER (GX-SUB)   TO  PI-CR-CARRIER.                CL**5
02024      MOVE GX-AM-GROUPING (GX-SUB)  TO  PI-CR-GROUPING.               CL**5
02025      MOVE GX-AM-STATE (GX-SUB)     TO  PI-CR-STATE.                  CL**5
02026      MOVE GX-AM-ACCOUNT (GX-SUB)   TO  PI-CR-ACCOUNT.                CL**5
02027      MOVE GX-AGENT-NO              TO  PI-SAV-AGENT.                 CL**5
02028      MOVE GX-AM-EXPIRATION-DT (GX-SUB)                               CL**5
02029                                    TO  PI-SAV-EXP-DT.                CL**5
02030      MOVE GX-AM-LEVEL-NO (GX-SUB)  TO  WS-GA-LEVEL.                  CL**5
02031      MOVE LOW-VALUES               TO  GX-LAST-BILL-DT (GX-SUB).     CL**5
02032                                                                   EL642
02033      PERFORM 4000-PNDB-START-BROWSE  THRU  4090-EXIT.                CL**5
02034                                                                      CL**5
02035      IF PNDB-EOF                                                  EL642
02036          PERFORM 4160-PNDB-END-BROWSE                             EL642
02037          GO TO 2005-GXRF-BILLING-PROCESS-LOOP.                    EL642
02038                                                                   EL642
02039  2010-VOID-PNDB-LOOP.                                             EL642
02040      PERFORM 4100-PNDB-READ-NEXT  THRU  4150-EXIT.                   CL**5
02041                                                                   EL642
02042      IF PNDB-EOF                                                  EL642
02043          PERFORM 4160-PNDB-END-BROWSE                             EL642
02044          GO TO 2005-GXRF-BILLING-PROCESS-LOOP.                    EL642
02045                                                                   EL642
02046      IF PB-COMPANY-CD-A1  =  PI-COMPANY-CD                           CL**5
02047        AND  PB-SV-CARRIER  =  PI-CR-CARRIER                          CL**5
02048        AND  PB-SV-GROUPING  =  PI-CR-GROUPING                        CL**5
02049        AND  PB-SV-STATE  =  PI-CR-STATE                              CL**5
02050        AND  PB-ACCOUNT  =  PI-CR-ACCOUNT                             CL**5
02051          NEXT SENTENCE                                            EL642
02052      ELSE                                                         EL642
02053          PERFORM 4160-PNDB-END-BROWSE                             EL642
02054          GO TO 2005-GXRF-BILLING-PROCESS-LOOP.                    EL642
02055                                                                   EL642
02056      IF PB-CREDIT-ACCEPT-DT  NOT =  LOW-VALUES                       CL**5
02057          GO TO 2010-VOID-PNDB-LOOP.                               EL642
02058                                                                   EL642
02059      IF PB-GA-BILL-DT (WS-GA-LEVEL)  =  (LOW-VALUES  OR  SPACES)     CL**5
02060          GO TO 2010-VOID-PNDB-LOOP.                               EL642
02061                                                                   EL642
02062      IF PB-CERT-EFF-DT  =  GX-AM-EXPIRATION-DT (GX-SUB)              CL**5
02063        OR  PB-CERT-EFF-DT  GREATER THAN                              CL**5
02064                GX-AM-EXPIRATION-DT (GX-SUB)                          CL**5
02065          PERFORM 4160-PNDB-END-BROWSE                             EL642
02066          GO TO 2005-GXRF-BILLING-PROCESS-LOOP.                    EL642
02067                                                                   EL642
02068      IF PB-CERT-EFF-DT  LESS THAN  GX-AM-EFF-DT (GX-SUB)          EL642
02069          GO TO 2010-VOID-PNDB-LOOP.                                  CL**5
02070                                                                   EL642
02071      IF PB-BATCH-TRAILER                                          EL642
02072          GO TO 2010-VOID-PNDB-LOOP.                               EL642
02073                                                                   EL642
02074      PERFORM 4200-PNDB-REWRITE  THRU  4290-EXIT.                     CL**5
02075                                                                      CL**5
02076      MOVE 'Y'                    TO  DATA-VOIDED-SW.                 CL**5
02077                                                                      CL**5
02078      GO TO 2010-VOID-PNDB-LOOP.                                   EL642
02079                                                                   EL642
02080  2100-VOID-PNDB-COMPLETE.                                         EL642
02081      IF DATA-VOIDED                                               EL642
02082          NEXT SENTENCE                                            EL642
02083      ELSE                                                         EL642
02084          MOVE -1                 TO  APFNTERL                        CL**5
02085          MOVE ER-2401            TO  EMI-ERROR                       CL**5
02086          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**5
02087          GO TO 8100-SEND-INITIAL-MAP.                             EL642
02088                                                                   EL642
02089      PERFORM 3940-BILL-GENERIC-DELETE  THRU  3940-EXIT.           EL642
02090                                                                   EL642
02091      PERFORM 6500-REWRITE-CROSS-REFERENCE  THRU  6590-EXIT.          CL**5
02092                                                                   EL642
02093  2105-VOID-COMPLETE.                                              EL642
02094      MOVE 0000                   TO  EMI-ERROR                       CL**5
02095                                                                   EL642
02096      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**5
02097                                                                      CL**5
02098      MOVE -1                     TO  AAGENTL.                        CL**5
02099                                                                      CL**5
02100      GO TO 8100-SEND-INITIAL-MAP.                                 EL642
02101  EJECT                                                               CL**5
02102  3000-WRITE-BILLING-DETAIL.                                       EL642
02103 ******************************************************************EL642
02104 *     THIS SECTION FORMATS THE BILLING PRINT RECORDS.  THESE     *EL642
02105 *     RECORDS ARE CREATED IN THE SAME FORMAT THAT THEY APPEAR    *EL642
02106 *     WHEN PRINTED.                                              *EL642
02107 *                                                                *EL642
02108 *     THIS SECTION PRINTS THE BILLING STATEMENT AND PRINT LABELS.*EL642
02109 *     BILLING-DETAIL-TYPES                                       *   CL*25
02110 *            TOTAL-STATEMENT   TS                                *   CL*25
02111 *            TOTAL-ACCOUNT     TA                                *   CL*25
02112 *            TOTAL-BOTH        TB                                *   CL*25
02113 *            GAN-AGT-ADDRESS   GA                                *   CL*25
02114 *                              BP                                *   CL*25
02115 ******************************************************************EL642
02116                                                                   EL642
02117      IF GEN-AGT-ADDRESS                                           EL642
02118          PERFORM 3930-BILL-READ  THRU  3930-EXIT                     CL**5
02119          IF NO-BILL-RECS                                          EL642
02120              GO TO 3200-FORMAT-HDR-ADDR-RECS                      EL642
02121          ELSE                                                     EL642
02122              IF BI-PREVIEW-ONLY                                   EL642
02123                OR  BI-INITIAL-PRINT-DATE  NOT =  LOW-VALUES          CL**5
02124                OR  PI-TOT-REBILL                                     CL**5
02125                  PERFORM 3940-BILL-GENERIC-DELETE                    CL**5
02126                      THRU  3940-EXIT                                 CL**5
02127                  GO TO 3200-FORMAT-HDR-ADDR-RECS                  EL642
02128              ELSE                                                 EL642
02129                  MOVE ER-2403    TO  EMI-ERROR                       CL**5
02130                  MOVE -1         TO  ABILTYPL                        CL**5
02131                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL**5
02132                  GO TO 8200-SEND-DATAONLY.                        EL642
02133                                                                   EL642
02134  3100-FORMAT-DETAIL-LINES.                                        EL642
02135      IF WS-LINECTR  GREATER THAN  23                                 CL**5
02136          MOVE ZEROS              TO  WS-LINECTR                      CL**5
02137          MOVE '3'                TO  BI-RECORD-TYPE                  CL**5
02138          MOVE TRIPLE-SPACE       TO  BI-SKIP-CONTROL                 CL**5
02139          MOVE '          CONTINUED ON NEXT PAGE'                  EL642
02140                                  TO  BI-TEXT-LINE                    CL**5
02141          PERFORM 3920-BILL-WRITE                                  EL642
02142          PERFORM 3300-FORMAT-HEADINGS  THRU  3380-EXIT.              CL**5
02143                                                                   EL642
02144      IF WS-LINECTR  =  ZEROS                                         CL**5
02145          MOVE DOUBLE-SPACE       TO  BI-SKIP-CONTROL                 CL**5
02146      ELSE                                                            CL**5
02147          MOVE SINGLE-SPACE       TO  BI-SKIP-CONTROL.                CL**5
02148                                                                      CL**5
02149      MOVE '3'                    TO  BI-RECORD-TYPE.                 CL**5
02150      MOVE SPACES                 TO  GA-TEXT-LINE.                EL642
02151                                                                   EL642
02152      IF TOTAL-STATEMENT                                              CL*25
02153          GO TO 3175-CHECK-AGENT-TOTAL.                               CL*25
02154                                                                      CL*25
02155      IF BILLING-DETAIL-TYPE  =  'BP'                                 CL**5
02156         IF PY-CHARGE-TO-AGENT OR PY-ADJ-CHG-TO-AGT                   CL*15
CIDMOD            MOVE PY-ENTRY-COMMENT                                      000
CIDMOD                                 TO  GA-ENTRY-DESC                     000
02157             MULTIPLY PY-ENTRY-AMT  BY  -1                            CL**5
02158                 GIVING  GA-PMTS-ADJS                                 CL**5
02159             MOVE PY-ENTRY-COMMENT                                    CL*25
02160                                  TO  GA-ENTRY-DESC                   CL*25
02161             MOVE '* ACCOUNTING ENTRY *'                              CL**5
02162                                  TO  GA-ENTRY-COMMENT                CL**5
02163             ADD +1               TO  WS-LINECTR                      CL**5
02164             IF PY-ACCOUNT = LOW-VALUES                               CL*25
02165                 MOVE SPACES      TO  GA-ACCT                         CL*25
02166                 PERFORM 3920-BILL-WRITE                              CL*25
02167                 GO TO 3990-EXIT                                      CL*25
02168             ELSE                                                     CL*25
02169                 MOVE PY-ACCOUNT  TO  GA-ACCT                         CL*25
02170                 PERFORM 3920-BILL-WRITE                              CL*25
02171                 GO TO 3990-EXIT.                                     CL*25
02172                                                                   EL642
02173      IF BILLING-DETAIL-TYPE  =  'BP'                                 CL**5
02174          MOVE PY-ENTRY-COMMENT   TO  GA-ENTRY-DESC                   CL**5
02175          MOVE PY-ENTRY-AMT       TO  GA-PMTS-ADJS                    CL**5
02176          MOVE '* ACCOUNTING ENTRY *'                                 CL**5
02177                                  TO  GA-ENTRY-COMMENT                CL**5
02178          MOVE PY-ACCOUNT         TO  GA-ACCT                         CL*25
02179          ADD +1                  TO  WS-LINECTR                      CL**5
02180             IF PY-ACCOUNT = LOW-VALUES                               CL*25
02181                 MOVE SPACES      TO  GA-ACCT                         CL*25
02182                 PERFORM 3920-BILL-WRITE                              CL*25
02183                 GO TO 3990-EXIT                                      CL*25
02184             ELSE                                                     CL*25
02185                 MOVE PY-ACCOUNT  TO  GA-ACCT                         CL*25
02186                 PERFORM 3920-BILL-WRITE                              CL*25
02187                 GO TO 3990-EXIT.                                     CL*25
02188                                                                      CL*25
02189  EJECT                                                               CL**5
02190  3150-FORMAT-ACCOUNT-LINE.                                        EL642
02191      MOVE SPACES                 TO  GA-TEXT-LINE.                EL642
02192      MOVE PI-SAV-ACCT-AGT        TO  GA-ACCT.                     EL642
02193      MOVE PI-SAV-ACCT-NAME       TO  GA-ACCT-NAME.                EL642
02194      MOVE WS-ACCT-BEG-BAL        TO  GA-BEG-BAL                   EL642
02195                                      GA-BEG-BAL-AMT.              EL642
02196      MOVE WS-ACCT-NET-PREM       TO  GA-NET-PREM                  EL642
02197                                      GA-NET-PREM-AMT.             EL642
02198      MOVE WS-ACCT-COMP           TO  GA-ACCT-COMP                 EL642
02199                                      GA-ACCT-COMP-AMT.            EL642
02200      MOVE WS-ACCT-PAY-ADJS       TO  GA-PMTS-ADJS                 EL642
02201                                      GA-PMTS-ADJS-AMT.            EL642
02202                                                                   EL642
02203      IF PI-SAV-AGENT  NOT =  PI-SAV-REMIT-TO                         CL**5
02204          MOVE ZEROS              TO  GA-END-BAL-AMT                  CL**5
02205      ELSE                                                            CL**5
02206          COMPUTE GA-END-BAL-AMT =                                 EL642
02207              (WS-ACCT-BEG-BAL + WS-ACCT-NET-PREM) -                  CL**5
02208               (WS-ACCT-COMP + WS-ACCT-PAY-ADJS).                     CL**5
02209                                                                   EL642
02210      MOVE WS-ACCT-UNPAID-NET-PREM                                    CL**5
02211                                  TO  GA-UNPAID-NET-PREM              CL**5
02212                                      GA-UNPAID-NET-AMT.           EL642
02213      MOVE ZEROS                  TO  GA-OVERWRITE-AMT.               CL**5
02214                                                                   EL642
02215      IF WS-ACCT-LF-OVERWRITE  NOT =  ZEROS                           CL**5
02216          MOVE PI-LIFE-OVERRIDE-L6                                    CL**5
02217                                  TO  GA-BEN-OVERRIDE-L6              CL**5
02218          MOVE PI-LIFE-OVERRIDE-L2                                    CL**5
02219                                  TO  GA-BENEFIT-CD                   CL**5
02220          MOVE WS-ACCT-LF-OVERWRITE                                   CL**5
02221                                  TO  GA-OVERWRITE                    CL**5
02222                                      GA-OVERWRITE-AMT                CL**5
02223      ELSE                                                         EL642
02224          IF WS-ACCT-AH-OVERWRITE  NOT =  ZEROS                       CL**5
02225              MOVE PI-AH-OVERRIDE-L6                                  CL**5
02226                                  TO  GA-BEN-OVERRIDE-L6              CL**5
02227              MOVE PI-AH-OVERRIDE-L2                                  CL**5
02228                                  TO  GA-BENEFIT-CD                   CL**5
02229              MOVE WS-ACCT-AH-OVERWRITE                               CL**5
02230                                  TO  GA-OVERWRITE                    CL**5
02231                                      GA-OVERWRITE-AMT.               CL**5
02232                                                                   EL642
02233      COMPUTE GA-AMT-DUE-AMT = GA-END-BAL-AMT -                    EL642
02234          (WS-ACCT-LF-OVERWRITE + WS-ACCT-AH-OVERWRITE).              CL**5
02235                                                                   EL642
02236      ADD  GA-AMT-DUE-AMT         TO  WS-GA-AMT-DUE.               EL642
02237                                                                      CL**5
02238      MOVE GA-AMT-DUE-AMT         TO  GA-AMT-DUE.                  EL642
02239                                                                      CL**5
02240      ADD +1                      TO  WS-LINECTR.                  EL642
02241                                                                      CL**5
02242      PERFORM 3920-BILL-WRITE.                                     EL642
02243                                                                      CL**5
02244      MOVE SPACES                 TO  GA-TEXT-LINE.                EL642
02245      MOVE SINGLE-SPACE           TO  BI-SKIP-CONTROL.             EL642
02246                                                                   EL642
02247      IF WS-ACCT-LF-OVERWRITE  NOT =  ZEROS                           CL**5
02248          IF WS-ACCT-AH-OVERWRITE  NOT =  ZEROS                       CL**5
02249              MOVE PI-AH-OVERRIDE-L6                                  CL**5
02250                                  TO  GA-BEN-OVERRIDE-L6              CL**5
02251              MOVE PI-AH-OVERRIDE-L2                                  CL**5
02252                                  TO  GA-BENEFIT-CD                   CL**5
02253              MOVE WS-ACCT-AH-OVERWRITE                               CL**5
02254                                  TO  GA-OVERWRITE                    CL**5
02255                                      GA-OVERWRITE-AMT                CL**5
02256              MOVE ZEROS          TO  GA-BEG-BAL-AMT                  CL**5
02257                                      GA-END-BAL-AMT                  CL**5
02258                                      GA-NET-PREM-AMT                 CL**5
02259                                      GA-ACCT-COMP-AMT                CL**5
02260                                      GA-PMTS-ADJS-AMT                CL**5
02261                                      GA-UNPAID-NET-AMT               CL**5
02262                                      GA-AMT-DUE-AMT                  CL**5
02263              ADD +1              TO  WS-LINECTR                      CL**5
02264              PERFORM 3920-BILL-WRITE.                                CL**5
02265                                                                   EL642
02266      ADD WS-ACCT-BEG-BAL         TO  WS-GA-BEG-BAL                   CL**5
02267                                      PI-ACCT-BEG-BAL.                CL**5
02268      ADD WS-ACCT-NET-PREM        TO  WS-GA-NET-PREM                  CL**5
02269                                      PI-ACCT-NET-PREM.               CL**5
02270      ADD WS-ACCT-COMP            TO  WS-GA-COMP                      CL**5
02271                                      PI-ACCT-COMP.                   CL**5
02272      ADD WS-ACCT-PAY-ADJS        TO  WS-GA-PAY-ADJS                  CL**5
02273                                      PI-ACCT-PAY-ADJS.               CL**5
02274      ADD WS-ACCT-UNPAID-NET-PREM                                     CL**5
02275                                  TO  WS-GA-UNPAID-NET-PREM.          CL**5
02276      ADD WS-ACCT-LF-OVERWRITE    TO  WS-GA-LF-OVERWRITE.          EL642
02277      ADD WS-ACCT-AH-OVERWRITE    TO  WS-GA-AH-OVERWRITE.          EL642
02278      ADD GA-END-BAL-AMT          TO  WS-GA-END-BAL.               EL642
02279                                                                   EL642
02280      MOVE ZEROS                  TO  WS-ACCT-BEG-BAL              EL642
02281                                      WS-ACCT-NET-PREM             EL642
02282                                      WS-ACCT-COMP                 EL642
02283                                      WS-ACCT-PAY-ADJS             EL642
02284                                      WS-ACCT-UNPAID-NET-PREM      EL642
02285                                      WS-ACCT-LF-OVERWRITE         EL642
02286                                      WS-ACCT-AH-OVERWRITE.        EL642
02287      MOVE SPACE                  TO  PI-ACCT-BILLED-SW.           EL642
02288      MOVE SPACE                  TO  ACCOUNT-CARRY-BAL-SW.        EL642
02289                                                                   EL642
02290      IF TOTAL-STATEMENT                                              CL**5
02291          GO TO 3400-FORMAT-TOTAL-LINE.                               CL*25
02292                                                                      CL*25
02293  3175-CHECK-AGENT-TOTAL.                                             CL*25
02294                                                                      CL*25
02295      IF TOTAL-STATEMENT OR                                           CL*25
02296         TOTAL-BOTH                                                   CL*25
02297          GO TO 3400-FORMAT-TOTAL-LINE.                            EL642
02298                                                                   EL642
02299      GO TO 3990-EXIT.                                             EL642
02300  EJECT                                                               CL**5
02301  3200-FORMAT-HDR-ADDR-RECS.                                       EL642
02302      PERFORM 3910-BILL-GETMAIN.                                   EL642
02303                                                                   EL642
02304      MOVE ZEROS                  TO  WS-LINE-SEQ-NO.                 CL**5
02305                                                                      CL**5
02306      SET A-INDX                  TO  1.                              CL**5
02307                                                                      CL**5
02308      PERFORM 3210-FORMAT-ADDR-RECS  THRU  3210-EXIT  6  TIMES.       CL**5
02309                                                                      CL**5
02310      MOVE ZEROS                  TO  WS-LINE-SEQ-NO.                 CL**5
02311                                                                      CL**5
02312      PERFORM 3300-FORMAT-HEADINGS  THRU  3380-EXIT.                  CL**5
02313                                                                      CL**5
02314      PERFORM 3220-WRITE-AGENTS-BAL  THRU  3220-EXIT.                 CL**5
02315                                                                      CL**5
02316      GO TO 3990-EXIT.                                             EL642
02317                                                                   EL642
02318  3210-FORMAT-ADDR-RECS.                                           EL642
02319      MOVE '2'                    TO  BI-RECORD-TYPE.                 CL**5
02320      MOVE SPACES                 TO  GA-TEXT-LINE.                   CL**5
02321      MOVE WS-AGENT-LINES (A-INDX)                                    CL**5
02322                                  TO  GA-ACCT-ADDRESS-LINE.           CL**5
02323                                                                      CL**5
02324      PERFORM 3920-BILL-WRITE.                                     EL642
02325                                                                      CL**5
02326      SET A-INDX                  UP  BY  1.                          CL**5
02327                                                                      CL**5
02328  3210-EXIT.                                                       EL642
02329      EXIT.                                                        EL642
02330                                                                   EL642
02331  3220-WRITE-AGENTS-BAL.                                           EL642
02332      MOVE SPACES                 TO GA-TEXT-LINE.                 EL642
02333      MOVE DOUBLE-SPACE           TO  BI-SKIP-CONTROL.                CL**5
02334                                                                      CL**5
02335      PERFORM 3920-BILL-WRITE.                                     EL642
02336                                                                      CL**5
02337      MOVE 'BEGINNING BALANCE'    TO  GA-ACCT-NAME                 EL642
02338      MOVE PI-BAL-FRWD            TO  GA-BEG-BAL.                     CL**5
02339      MOVE DOUBLE-SPACE           TO  BI-SKIP-CONTROL.                CL**5
02340                                                                      CL**5
02341      PERFORM 3920-BILL-WRITE.                                     EL642
02342                                                                      CL**5
02343      MOVE SPACES                 TO  GA-TEXT-LINE.                   CL**5
02344      MOVE DOUBLE-SPACE           TO  BI-SKIP-CONTROL.                CL**5
02345                                                                      CL**5
02346      PERFORM 3920-BILL-WRITE.                                     EL642
02347                                                                   EL642
02348  3220-EXIT.                                                       EL642
02349      EXIT.                                                        EL642
02350  EJECT                                                               CL**5
02351  3300-FORMAT-HEADINGS.                                            EL642
02352      MOVE WS-PGECTR              TO  HD-PG.                          CL**5
02353                                                                      CL**5
02354      ADD +1                      TO  WS-PGECTR.                      CL**5
02355                                                                      CL**5
02356      MOVE '3'                    TO  BI-RECORD-TYPE.                 CL**5
02357      MOVE TOP-OF-PAGE            TO  BI-SKIP-CONTROL.                CL**5
02358                                                                      CL**5
02359      IF NOT PI-PREVIEW                                            EL642
02360          MOVE GA-HD1             TO  GA-TEXT-LINE                    CL**5
02361      ELSE                                                         EL642
02362          MOVE GA-PREVIEW-HD      TO  GA-TEXT-LINE                    CL**5
02363          PERFORM 3920-BILL-WRITE                                  EL642
02364          MOVE '3'                TO  BI-RECORD-TYPE                  CL**5
02365          MOVE DOUBLE-SPACE       TO  BI-SKIP-CONTROL                 CL**5
02366          MOVE GA-HD1             TO  GA-TEXT-LINE.                   CL**5
02367                                                                   EL642
02368      PERFORM 3920-BILL-WRITE.                                     EL642
02369                                                                      CL**5
02370      MOVE SINGLE-SPACE           TO  BI-SKIP-CONTROL.                CL**5
02371      MOVE GA-HD2                 TO  GA-TEXT-LINE.                   CL**5
02372                                                                      CL**5
02373      PERFORM 3920-BILL-WRITE.                                     EL642
02374                                                                      CL**5
02375      MOVE SINGLE-SPACE           TO  BI-SKIP-CONTROL.                CL**5
02376      MOVE GA-HD3                 TO  GA-TEXT-LINE.                   CL**5
02377                                                                      CL**5
02378      PERFORM 3920-BILL-WRITE.                                     EL642
02379                                                                      CL**5
02380      SET A-INDX                  TO  1.                              CL**5
02381                                                                      CL**5
02382      PERFORM 3390-FORMAT-ADDR-TEXT-LINES  THRU  3390-EXIT            CL**5
02383          6  TIMES.                                                   CL**5
02384                                                                      CL**5
02385      MOVE DOUBLE-SPACE           TO  BI-SKIP-CONTROL.                CL**5
02386      MOVE GA-HD4                 TO  GA-TEXT-LINE.                   CL**5
02387                                                                      CL**5
02388      PERFORM 3920-BILL-WRITE.                                     EL642
02389                                                                      CL**5
02390      MOVE SINGLE-SPACE           TO  BI-SKIP-CONTROL.                CL**5
02391      MOVE GA-HD5                 TO  GA-TEXT-LINE.                   CL**5
02392                                                                      CL**5
02393      PERFORM 3920-BILL-WRITE.                                     EL642
02394                                                                   EL642
02395  3380-EXIT.                                                       EL642
02396      EXIT.                                                        EL642
02397                                                                   EL642
02398  3390-FORMAT-ADDR-TEXT-LINES.                                     EL642
02399      MOVE '3'                    TO  BI-RECORD-TYPE.                 CL**5
02400      MOVE SPACES                 TO  GA-TEXT-LINE.                   CL**5
02401                                                                   EL642
02402      IF A-INDX  =  1                                                 CL**5
02403          MOVE DOUBLE-SPACE       TO  BI-SKIP-CONTROL                 CL**5
02404          MOVE PI-SAV-CARR        TO  GA-CARRIER                      CL**5
02405          MOVE PI-SAV-GROUP       TO  GA-GROUPING                     CL**5
02406          MOVE PI-SAV-AGENT       TO  GA-AGENT                        CL**5
02407          MOVE '-'                TO  GA-DASH                         CL**5
02408      ELSE                                                            CL**5
02409          MOVE SINGLE-SPACE       TO  BI-SKIP-CONTROL.                CL**5
02410                                                                      CL**5
02411      MOVE WS-AGENT-LINES (A-INDX)  TO  GA-AGENT-ADDR.                CL**5
02412                                                                      CL**5
02413      PERFORM 3920-BILL-WRITE.                                     EL642
02414                                                                      CL**5
02415      SET A-INDX                  UP  BY  1.                          CL**5
02416                                                                   EL642
02417  3390-EXIT.                                                       EL642
02418      EXIT.                                                        EL642
02419  EJECT                                                               CL**5
02420  3400-FORMAT-TOTAL-LINE.                                          EL642
02421      MOVE SPACES                 TO  GA-TEXT-LINE.                EL642
02422      MOVE DOUBLE-SPACE           TO  BI-SKIP-CONTROL.             EL642
02423      MOVE '3'                    TO  BI-RECORD-TYPE.              EL642
02424      MOVE SPACES                 TO  GA-TEXT-LINE.                EL642
02425                                                                      CL**5
02426      PERFORM 3920-BILL-WRITE.                                     EL642
02427                                                                      CL**5
02428      MOVE SPACES                 TO  GA-TEXT-LINE.                EL642
02429      MOVE 'AGENT TOTAL'          TO  GA-ACCT-NAME.                EL642
02430      MOVE WS-GA-BEG-BAL          TO  GA-BEG-BAL                   EL642
02431                                      GA-BEG-BAL-AMT.              EL642
02432      MOVE WS-GA-NET-PREM         TO  GA-NET-PREM                  EL642
02433                                      GA-NET-PREM-AMT.             EL642
02434      MOVE WS-GA-COMP             TO  GA-ACCT-COMP                 EL642
02435                                      GA-ACCT-COMP-AMT.            EL642
02436      MOVE WS-GA-PAY-ADJS         TO  GA-PMTS-ADJS                 EL642
02437                                      GA-PMTS-ADJS-AMT.            EL642
02438      MOVE WS-GA-END-BAL          TO  GA-END-BAL-AMT.              EL642
02439      MOVE WS-GA-UNPAID-NET-PREM  TO  GA-UNPAID-NET-PREM           EL642
02440                                      GA-UNPAID-NET-AMT.              CL**5
02441      MOVE PI-LIFE-OVERRIDE-L6    TO  GA-BEN-OVERRIDE-L6.          EL642
02442      MOVE PI-LIFE-OVERRIDE-L2    TO  GA-BENEFIT-CD.               EL642
02443      MOVE WS-GA-LF-OVERWRITE     TO  GA-OVERWRITE                 EL642
02444                                      GA-OVERWRITE-AMT.            EL642
02445      MOVE WS-GA-AMT-DUE          TO  GA-AMT-DUE-AMT               EL642
02446                                      GA-AMT-DUE.                  EL642
02447                                                                   EL642
02448      PERFORM 3920-BILL-WRITE.                                     EL642
02449                                                                      CL**5
02450      MOVE SPACES                 TO  GA-TEXT-LINE.                EL642
02451      MOVE SINGLE-SPACE           TO  BI-SKIP-CONTROL.             EL642
02452      MOVE PI-AH-OVERRIDE-L6      TO  GA-BEN-OVERRIDE-L6           EL642
02453      MOVE PI-AH-OVERRIDE-L2      TO  GA-BENEFIT-CD                EL642
02454      MOVE WS-GA-AH-OVERWRITE     TO  GA-OVERWRITE                 EL642
02455                                      GA-OVERWRITE-AMT.            EL642
02456                                                                   EL642
02457      PERFORM 3920-BILL-WRITE.                                     EL642
02458                                                                      CL**5
02459      MOVE SPACES                 TO  GA-TEXT-LINE.                EL642
02460      MOVE SINGLE-SPACE           TO  BI-SKIP-CONTROL.             EL642
02461                                                                      CL**5
02462      PERFORM 3920-BILL-WRITE.                                     EL642
02463                                                                   EL642
02464      MOVE SPACES                 TO  GA-TEXT-LINE.                EL642
02465      MOVE '1'                    TO  BI-RECORD-TYPE.              EL642
02466      MOVE PI-PROCESSOR-ID        TO  GA-PROCESSOR-CD.             EL642
02467                                                                      CL**5
02468      IF PI-PREV-BILL                                                 CL**5
02469        OR  PI-PREV-REBILL                                            CL**5
02470          MOVE 'P'                TO  GA-STATEMENT-TYPE.           EL642
02471                                                                   EL642
02472      MOVE +1                     TO  GA-NO-OF-COPIES.             EL642
02473      MOVE WS-CURRENT-DATE        TO  GA-CREATION-DT.              EL642
02474      MOVE LOW-VALUES             TO  GA-INITIAL-PRINT-DATE.       EL642
02475      MOVE PI-UNPAID-NET-PREM     TO  GA-NET-UNPD.                    CL**5
02476      MOVE PI-COMP-UNPAID-PREM    TO  GA-COMP-UNPD-PREM.              CL**5
02477      MOVE PI-PREMIUM             TO  GA-PREMIUM.                  EL642
02478      MOVE PI-REMITTED            TO  GA-REMITTED.                 EL642
02479      MOVE PI-TOT-ISS-COMP        TO  GA-TOT-ISS-COMP.             EL642
02480      MOVE PI-TOT-CAN-COMP        TO  GA-TOT-CAN-COMP.             EL642
02481      MOVE PI-ADJUSTMNTS          TO  GA-ADJUSTMNTS.               EL642
02482      MOVE PI-DISBURSED           TO  GA-DISBURSED.                EL642
02483      MOVE PI-AGENT-NAME          TO  GA-AGENTS-NAME.              EL642
02484                                                                   EL642
02485      PERFORM 3920-BILL-WRITE.                                     EL642
02486                                                                      CL**5
02487      GO TO 3990-EXIT.                                             EL642
02488  EJECT                                                               CL**5
02489  3910-BILL-GETMAIN.                                               EL642
02490      EXEC CICS GETMAIN                                            EL642
02491          SET      (ADDRESS OF BILLING-STATEMENT)                     CL*26
02492          LENGTH   (210)                                              CL**5
02493          INITIMG  (GETMAIN-SPACE)                                    CL**5
02494      END-EXEC.                                                       CL*27
02495                                                                   EL642
02496  3920-BILL-WRITE.                                                 EL642
02497      MOVE PI-COMPANY-CD          TO  BI-COMPANY-CD.                  CL**5
02498      MOVE PI-SAV-CARR            TO  BI-CARRIER.                     CL**5
02499      MOVE PI-SAV-GROUP           TO  BI-GROUPING.                    CL**5
02500      MOVE LOW-VALUES             TO  BI-ACCOUNT.                     CL**5
02501      MOVE PI-SAV-AGENT           TO  BI-FIN-RESP.                    CL**5
02502      MOVE 'BI'                   TO  BI-RECORD-ID.                   CL**5
02503                                                                      CL**5
02504      ADD +1                      TO  WS-LINE-SEQ-NO.                 CL**5
02505                                                                      CL**5
02506      IF BI-RECORD-TYPE = '1'                                      EL642
02507          MOVE ZEROS              TO  BI-LINE-SEQ-NO                  CL**5
02508      ELSE                                                         EL642
02509          MOVE WS-LINE-SEQ-NO     TO  BI-LINE-SEQ-NO.                 CL**5
02510                                                                   EL642
02511      EXEC CICS WRITE                                              EL642
02512          DATASET  (ERBILL-FILE-ID)                                   CL**5
02513          FROM     (BILLING-STATEMENT)                                CL**5
02514          RIDFLD   (BI-CONTROL-PRIMARY)                               CL**5
02515      END-EXEC.                                                       CL*27
02516                                                                   EL642
02517  3930-BILL-READ.                                                  EL642
02518      MOVE PI-COMPANY-CD          TO  ERBILL-CO-CD.                   CL**5
02519      MOVE PI-SAV-CARR            TO  ERBILL-CARRIER.                 CL**5
02520      MOVE PI-SAV-GROUP           TO  ERBILL-GROUP.                   CL**5
02521      MOVE LOW-VALUES             TO  ERBILL-ACCT.                    CL**5
02522      MOVE PI-SAV-AGENT           TO  ERBILL-FIN-RESP.                CL**5
02523      MOVE '1'                    TO  ERBILL-REC-TYPE.                CL**5
02524      MOVE ZEROS                  TO  ERBILL-LINE-SEQ-NO.             CL**5
02525                                                                   EL642
02526      EXEC CICS HANDLE CONDITION                                   EL642
02527          NOTFND  (3930-BILL-NOTFND)                                  CL**5
02528      END-EXEC.                                                       CL*27
02529                                                                   EL642
02530      EXEC CICS READ                                               EL642
02531          SET      (ADDRESS OF BILLING-STATEMENT)                     CL*26
02532          DATASET  (ERBILL-FILE-ID)                                   CL**5
02533          RIDFLD   (ERBILL-KEY)                                       CL**5
02534          GTEQ                                                     EL642
02535      END-EXEC.                                                       CL*27
02536                                                                   EL642
02537      IF BI-COMPANY-CD  =  PI-COMPANY-CD                              CL**5
02538        AND  BI-CARRIER  =  ERBILL-CARRIER                            CL**5
02539        AND  BI-GROUPING  =  ERBILL-GROUP                             CL**5
02540        AND  BI-ACCOUNT  =  ERBILL-ACCT                               CL**5
02541        AND  BI-FIN-RESP  =  ERBILL-FIN-RESP                          CL**5
02542          MOVE SPACE              TO  NO-BILL-REC-SW                  CL**5
02543          GO TO 3930-EXIT                                          EL642
02544      ELSE                                                         EL642
02545          GO TO 3930-BILL-NOTFND.                                  EL642
02546                                                                   EL642
02547  3930-BILL-NOTFND.                                                EL642
02548      MOVE 'Y'                    TO  NO-BILL-REC-SW.                 CL**5
02549                                                                   EL642
02550  3930-EXIT.                                                       EL642
02551      EXIT.                                                        EL642
02552                                                                   EL642
02553  3940-BILL-GENERIC-DELETE.                                        EL642
02554      EXEC CICS HANDLE CONDITION                                   EL642
02555          NOTFND  (3940-EXIT)                                         CL**5
02556      END-EXEC.                                                       CL*27
02557                                                                   EL642
02558      MOVE PI-COMPANY-CD          TO  ERBILL-CO-CD.                   CL**5
02559      MOVE PI-SAV-CARR            TO  ERBILL-CARRIER.                 CL**5
02560      MOVE PI-SAV-GROUP           TO  ERBILL-GROUP.                   CL**5
02561      MOVE LOW-VALUES             TO  ERBILL-ACCT.                    CL**5
02562      MOVE PI-SAV-AGENT           TO  ERBILL-FIN-RESP.                CL**5
02563                                                                   EL642
02564      EXEC CICS DELETE                                             EL642
02565          DATASET    (ERBILL-FILE-ID)                                 CL**5
02566          RIDFLD     (ERBILL-KEY)                                     CL**5
02567          KEYLENGTH  (28)                                             CL**5
02568          GENERIC                                                  EL642
02569      END-EXEC.                                                       CL*27
02570                                                                   EL642
02571  3940-EXIT.                                                       EL642
02572      EXIT.                                                        EL642
02573                                                                   EL642
02574  3990-EXIT.                                                       EL642
02575      EXIT.                                                        EL642
02576  EJECT                                                               CL**5
02577  4000-PNDB-START-BROWSE.                                          EL642
02578      MOVE SPACES                 TO  ERPNDB-ALT-KEY.                 CL**5
02579      MOVE PI-COMPANY-CD          TO  ERPNDB-CO-CD-A1.                CL**5
02580                                                                   EL642
02581      IF CARR-GROUP-ST-ACCNT-CNTL                                     CL**5
02582          MOVE GX-AM-CARRIER  (GX-SUB)  TO  ERPNDB-CARR               CL*31
02583          MOVE GX-AM-GROUPING (GX-SUB)  TO  ERPNDB-GROUP              CL**5
02584          MOVE GX-AM-STATE    (GX-SUB)  TO  ERPNDB-STATE              CL*31
02585          MOVE GX-AM-ACCOUNT  (GX-SUB)  TO  ERPNDB-ACCT               CL*31
02586      ELSE                                                            CL*31
02587      IF CARR-ST-ACCNT-CNTL                                           CL*31
02588          MOVE GX-AM-CARRIER (GX-SUB)   TO  ERPNDB-CARR               CL*31
02589          MOVE GX-AM-STATE   (GX-SUB)   TO  ERPNDB-STATE              CL*31
02590          MOVE GX-AM-ACCOUNT (GX-SUB)   TO  ERPNDB-ACCT               CL**5
02591      ELSE                                                         EL642
02592      IF CARR-ACCNT-CNTL                                              CL*31
02593          MOVE GX-AM-CARRIER (GX-SUB)   TO  ERPNDB-CARR               CL*31
02594          MOVE GX-AM-ACCOUNT (GX-SUB)   TO  ERPNDB-ACCT               CL*31
02595      ELSE                                                            CL*31
02596      IF ACCNT-CNTL                                                   CL*31
02597          MOVE GX-AM-ACCOUNT (GX-SUB)   TO  ERPNDB-ACCT               CL*31
02598      ELSE                                                            CL*31
02599          MOVE GX-AM-STATE   (GX-SUB)   TO  ERPNDB-STATE              CL*31
02600          MOVE GX-AM-ACCOUNT (GX-SUB)   TO  ERPNDB-ACCT.              CL*31
02601                                                                   EL642
02602      MOVE GX-AM-EFF-DT (GX-SUB)  TO  ERPNDB-EFF-DT.                  CL**5
02603                                                                   EL642
02604      EXEC CICS HANDLE CONDITION                                   EL642
02605          NOTFND  (4010-REC-NOT-FND)                                  CL**5
02606      END-EXEC.                                                       CL*27
02607                                                                   EL642
02608      EXEC CICS STARTBR                                            EL642
02609          DATASET  (ERPNDB-ALT-FILE-ID)                               CL**5
02610          RIDFLD   (ERPNDB-ALT-KEY)                                   CL**5
02611          GTEQ                                                     EL642
02612      END-EXEC.                                                       CL*27
02613                                                                   EL642
02614      GO TO 4090-EXIT.                                             EL642
02615                                                                   EL642
02616  4010-REC-NOT-FND.                                                EL642
02617      MOVE 'Y'                    TO  PNDB-EOF-SW.                    CL**5
02618                                                                   EL642
02619  4090-EXIT.                                                       EL642
02620      EXIT.                                                        EL642
02621  EJECT                                                               CL**5
02622  4100-PNDB-READ-NEXT.                                             EL642
02623      EXEC CICS HANDLE CONDITION                                   EL642
02624          ENDFILE  (4110-END-OF-FILE)                                 CL**5
02625      END-EXEC.                                                       CL*27
02626                                                                   EL642
02627      EXEC CICS READNEXT                                           EL642
02628          SET      (ADDRESS OF PENDING-BUSINESS)                      CL*26
02629          DATASET  (ERPNDB-ALT-FILE-ID)                               CL**5
02630          RIDFLD   (ERPNDB-ALT-KEY)                                   CL**5
02631      END-EXEC.                                                       CL*27
02632                                                                   EL642
02633      GO TO 4150-EXIT.                                             EL642
02634                                                                   EL642
02635  4110-END-OF-FILE.                                                EL642
02636      MOVE 'Y'                    TO  PNDB-EOF-SW.                    CL**5
02637                                                                   EL642
02638  4150-EXIT.                                                       EL642
02639      EXIT.                                                        EL642
02640  EJECT                                                               CL**5
02641  4160-PNDB-END-BROWSE.                                            EL642
02642      EXEC CICS ENDBR                                              EL642
02643          DATASET  (ERPNDB-ALT-FILE-ID)                               CL**5
02644      END-EXEC.                                                       CL*27
02645  EJECT                                                               CL**5
02646  4200-PNDB-REWRITE.                                               EL642
02647      MOVE PB-COMPANY-CD          TO  ERPNDB-CO-CD.                   CL**5
02648      MOVE PB-ENTRY-BATCH         TO  ERPNDB-BATCH.                   CL**5
02649      MOVE PB-BATCH-SEQ-NO        TO  ERPNDB-SEQ-NO.                  CL**5
02650      MOVE PB-BATCH-CHG-SEQ-NO    TO  ERPNDB-CHG-SEQ-NO.              CL**5
02651                                                                   EL642
02652      PERFORM 4160-PNDB-END-BROWSE                                 EL642
02653                                                                   EL642
02654      EXEC CICS READ                                               EL642
02655          SET      (ADDRESS OF PENDING-BUSINESS)                      CL*26
02656          DATASET  (ERPNDB-FILE-ID)                                   CL**5
02657          RIDFLD   (ERPNDB-PRIME-KEY)                                 CL**5
02658          UPDATE                                                   EL642
02659      END-EXEC.                                                       CL*27
02660                                                                   EL642
02661      MOVE 'B'                    TO  JP-RECORD-TYPE.                 CL**5
02662      MOVE PENDING-BUSINESS       TO  JP-RECORD-AREA.                 CL**5
02663      MOVE ERPNDB-FILE-ID         TO  JP-FILE-ID.                     CL**5
02664                                                                      CL**5
02665      COMPUTE JOURNAL-LENGTH = ERPNDB-LENGTH + 23.                 EL642
02666                                                                      CL**5
02667      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL642
02668                                                                   EL642
02669      IF PI-VOID-BILL                                              EL642
02670          MOVE LOW-VALUES         TO  PB-GA-BILL-DT (WS-GA-LEVEL)     CL**5
02671      ELSE                                                         EL642
02672          MOVE WS-CURRENT-DATE    TO  PB-GA-BILL-DT (WS-GA-LEVEL).    CL**5
02673                                                                   EL642
02674      MOVE PI-PROCESSOR-ID        TO  PB-LAST-MAINT-BY.               CL**5
02675      MOVE EIBTIME                TO  PB-LAST-MAINT-HHMMSS.           CL**5
02676      MOVE WS-CURRENT-DATE        TO  PB-LAST-MAINT-DT.               CL**5
02677      MOVE 'C'                    TO  JP-RECORD-TYPE.                 CL**5
02678      MOVE PENDING-BUSINESS       TO  JP-RECORD-AREA.                 CL**5
02679      MOVE ERPNDB-FILE-ID         TO  JP-FILE-ID.                     CL**5
02680                                                                      CL**5
02681      COMPUTE JOURNAL-LENGTH = ERPNDB-LENGTH + 23.                 EL642
02682                                                                      CL**5
02683      EXEC CICS REWRITE                                            EL642
02684          DATASET  (ERPNDB-FILE-ID)                                   CL**5
02685          FROM     (PENDING-BUSINESS)                                 CL**5
02686      END-EXEC.                                                       CL*27
02687                                                                      CL**5
02688      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL642
02689                                                                   EL642
02690      EXEC CICS HANDLE CONDITION                                      CL*26
02691          NOTFND  (4290-REC-NOT-FND)                                  CL*26
02692      END-EXEC.                                                       CL*27
02693                                                                      CL*26
02694      EXEC CICS STARTBR                                            EL642
02695          DATASET  (ERPNDB-ALT-FILE-ID)                               CL**5
02696          RIDFLD   (ERPNDB-ALT-KEY)                                   CL**5
02697          GTEQ                                                        CL**5
02698      END-EXEC.                                                       CL*27
02699                                                                      CL**5
02700      PERFORM 4100-PNDB-READ-NEXT  THRU  4150-EXIT.                   CL**5
02701                                                                      CL*26
02702      GO TO 4290-EXIT.                                                CL*26
02703                                                                      CL*26
02704  4290-REC-NOT-FND.                                                   CL*26
02705      MOVE 'Y'                    TO  PYAJ-EOF-SW.                    CL*26
02706                                                                   EL642
02707  4290-EXIT.                                                       EL642
02708      EXIT.                                                        EL642
02709  EJECT                                                               CL**5
02710  4300-READ-ACCOUNT-MASTER.                                        EL642
02711                                                                      CL*11
02712      MOVE SPACE                  TO  INVALID-ACCOUNT-SW.             CL**5
02713      MOVE PI-COMPANY-CD          TO  ERACCT-P-CO-CD.                 CL**5
02714      MOVE PI-CR-CARRIER          TO  ERACCT-P-CARRIER.               CL**5
02715      MOVE PI-CR-GROUPING         TO  ERACCT-P-GROUPING.              CL**5
02716      MOVE PI-CR-STATE            TO  ERACCT-P-STATE.                 CL**5
02717      MOVE PI-CR-ACCOUNT          TO  ERACCT-P-ACCOUNT.               CL**5
02718      MOVE PI-SAV-EXP-DT          TO  ERACCT-P-EXP-DATE.              CL**5
02719                                                                   EL642
02720      EXEC CICS HANDLE CONDITION                                   EL642
02721          NOTFND  (4370-ACCOUNT-INVALID)                              CL**5
02722      END-EXEC.                                                       CL*27
02723                                                                   EL642
02724      EXEC CICS READ                                               EL642
02725          DATASET  (ERACCT-FILE-ID)                                   CL**5
02726          SET      (ADDRESS OF ACCOUNT-MASTER)                        CL*26
02727          RIDFLD   (ERACCT-PRIME-KEY)                                 CL**5
02728          EQUAL                                                    EL642
02729      END-EXEC.                                                       CL*27
02730                                                                   EL642
02731      IF AM-REMIT-TO  NOT GREATER THAN  '00'                          CL**5
02732          GO TO 4370-ACCOUNT-INVALID.                              EL642
02733                                                                   EL642
02734      IF AM-AGT (AM-REMIT-TO)  =  SPACES                              CL**5
02735          MOVE PI-CR-ACCOUNT      TO  PI-SAV-REMIT-TO              EL642
02736      ELSE                                                         EL642
02737          MOVE AM-AGT (AM-REMIT-TO)                                   CL**5
02738                                  TO  PI-SAV-REMIT-TO.                CL**5
02739                                                                   EL642
02740      MOVE AM-EXPIRATION-DT       TO  ERACCT-P-EXP-DATE.              CL**5
02741      MOVE AM-CARRIER             TO  PI-CR-CARRIER.                  CL**5
02742      MOVE AM-GROUPING            TO  PI-CR-GROUPING.                 CL**5
02743      MOVE AM-STATE               TO  PI-CR-STATE.                    CL**5
02744      MOVE AM-AGT (AM-REMIT-TO)   TO  PI-SAV-FIN-RESP                 CL**5
02745                                      PI-COMP-FIN-RESP.               CL**5
02746                                                                   EL642
02747      IF PI-ZERO-CARRIER                                           EL642
02748        OR  PI-ZERO-CAR-GROUP                                         CL**5
02749          MOVE ZEROS              TO  PI-COMP-CARRIER                 CL**5
02750      ELSE                                                         EL642
02751          MOVE AM-CARRIER         TO  PI-COMP-CARRIER.                CL**5
02752                                                                   EL642
02753      IF PI-ZERO-GROUPING                                          EL642
02754         OR  PI-ZERO-CAR-GROUP                                        CL**5
02755          MOVE ZEROS              TO  PI-COMP-GROUPING                CL**5
02756      ELSE                                                         EL642
02757          MOVE AM-GROUPING        TO  PI-COMP-GROUPING.               CL**5
02758                                                                   EL642
02759      IF CARR-GROUP-ST-ACCNT-CNTL                                     CL**5
02760          MOVE SPACE              TO  NEW-ACCOUNT-SW.                 CL**5
02761                                                                   EL642
02762      MOVE +0                     TO  ACCOM-SUB.                      CL**5
02763                                                                   EL642
02764  4320-FIND-ACCT-RESPONSIBILITY.                                   EL642
02765      ADD +1                      TO  ACCOM-SUB.                      CL**5
02766                                                                      CL**5
02767      IF ACCOM-SUB  GREATER THAN  +10                                 CL**5
02768          MOVE  AM-ACCOUNT        TO  GA-ACCT                         CL**5
02769          GO TO 4330-FIND-GA-COMMISSION.                           EL642
02770                                                                   EL642
02771      IF AM-COM-TYP (ACCOM-SUB) = 'C' OR 'D' OR 'F' OR 'S'            CL*26
02772          NEXT SENTENCE                                            EL642
02773      ELSE                                                         EL642
02774          GO TO 4320-FIND-ACCT-RESPONSIBILITY.                     EL642
02775                                                                   EL642
02776      IF AM-AGT (ACCOM-SUB)  NOT =  PI-SAV-ACCT-AGT                   CL**5
02777          MOVE 'Y'                TO  NEW-ACCOUNT-SW.                 CL**5
02778                                                                   EL642
02779      MOVE AM-AGT (ACCOM-SUB)     TO  PI-SAV-ACCT-AGT.                CL**5
02780      MOVE AM-NAME                TO  PI-SAV-ACCT-NAME.               CL**5
02781                                                                   EL642
02782  4330-FIND-GA-COMMISSION.                                         EL642
02783      MOVE ZEROS                  TO  WS-GA-LEVEL                     CL**5
02784                                      WS-GA-LF-COM                    CL**5
02785                                      WS-GA-AH-COM.                   CL**5
02786      MOVE +0                     TO  ACCOM-SUB.                      CL**5
02787                                                                   EL642
02788  4340-SEARCH-FOR-GA-COMMISSION.                                   EL642
02789      ADD +1                      TO  ACCOM-SUB.                      CL**5
02790                                                                      CL**5
02791      IF ACCOM-SUB  GREATER THAN  +10                                 CL*24
02792          GO TO 4370-ACCOUNT-INVALID.                              EL642
02793                                                                   EL642
02794      IF AM-COM-TYP (ACCOM-SUB)  = 'O' OR 'P' OR 'G' OR 'B'           CL*26
02795          NEXT SENTENCE                                            EL642
02796      ELSE                                                         EL642
02797          GO TO 4340-SEARCH-FOR-GA-COMMISSION.                     EL642
02798                                                                   EL642
02799      IF AM-AGT (ACCOM-SUB)  NOT =  PI-SAV-AGENT                      CL**5
02800          GO TO 4340-SEARCH-FOR-GA-COMMISSION.                     EL642
02801                                                                   EL642
02802      MOVE ACCOM-SUB              TO  WS-GA-LEVEL.                    CL**5
02803                                                                   EL642
02804      GO TO 4390-EXIT.                                             EL642
02805  EJECT                                                               CL**5
02806  4370-ACCOUNT-INVALID.                                            EL642
02807      MOVE 'Y'                    TO  INVALID-ACCOUNT-SW.             CL**5
02808      MOVE ER-2210                TO  EMI-ERROR.                      CL**5
02809                                                                      CL**5
02810      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL642
02811                                                                      CL**5
02812      GO TO 4390-EXIT.                                             EL642
02813                                                                   EL642
02814  4390-EXIT.                                                       EL642
02815      EXIT.                                                        EL642
02816  EJECT                                                               CL**5
02817  4400-FIND-COMMISSION.                                            EL642
02818 ******************************************************************EL642
02819 *    IF THE AGENT'S COMMISSION IS NUMERIC FOR A&H AND LIFE       *EL642
02820 *    IN THE ACCOUNT MASTER THAT IS THE COMMISSION.  IF THE       *EL642
02821 *    COMMISSION IN THE A&H AND LIFE CONTAINS A TABLE CODE        *EL642
02822 *    READ THE COMMISSION-TABLE TO GET THE COMMISSION RATE.       *EL642
02823 *    FOR CLIENT "DMD" THE ABOVE COMMISSION MAY BE OVERWRITTEN    *   CL*27
02824 *    IF AN ACCOUNT RESIDENT STATE COMMISSION RECORD EXISTS.      *   CL*27
02825 ******************************************************************EL642
02826                                                                   EL642
02827      MOVE PI-COMPANY-CD          TO  ERCTBL-COMPANY-CD.              CL**5
02828                                                                   EL642
02829  4405-FIND-AH-COMMISSION.                                         EL642
02830 * SET UP COMMISSION TABLE KEY BEFORE DETERMINING COMMISSION          CL*27
02831      IF PB-ISSUE                                                     CL**5
02832          MOVE PB-I-AH-BENEFIT-CD  TO  ERCTBL-BEN-CODE                CL*31
02833      ELSE                                                            CL**5
02834          MOVE PB-CI-AH-BENEFIT-CD TO  ERCTBL-BEN-CODE.               CL*31
02835                                                                      CL*31
02836      MOVE 'A'                     TO  ERCTBL-BEN-TYPE.               CL*27
02837                                                                      CL*27
02838      MOVE 'AH'                   TO WS-COMM-SW.                      CL*27
02839                                                                      CL*27
02840      IF AM-A-COM (WS-GA-LEVEL)  NUMERIC                              CL*27
02841          MOVE AM-A-COM (WS-GA-LEVEL)                                 CL*27
02842                                  TO  WS-COMMISSION                   CL*27
02843          PERFORM 6650-GET-RES-STATE-COMM THRU 6700-COMM-EXIT         CL*27
02844          MOVE WS-COMMISSION      TO  WS-GA-AH-COM                    CL*27
02845          GO TO 4410-FIND-LIFE-COMMISSION.                            CL*27
02846                                                                   EL642
02847      IF ERCTBL-BEN-CODE  =  ZEROS                                    CL**5
02848          MOVE ZEROS              TO  WS-GA-LF-COM                    CL**5
02849          GO TO 4410-FIND-LIFE-COMMISSION.                         EL642
02850                                                                   EL642
02851      MOVE ZEROS                  TO WS-SUB1.                         CL*27
02852      PERFORM 6650-GET-RES-STATE-COMM THRU 6700-COMM-EXIT.            CL*27
02853      IF WS-SUB1 IS GREATER THAN ZEROS                                CL*27
02854          GO TO 4410-FIND-LIFE-COMMISSION.                            CL*27
CIDMOD
CIDMOD     MOVE 'A'                     TO  ERCTBL-BEN-TYPE.                 000
02855                                                                   EL642
02856      MOVE AM-A-COMA (WS-GA-LEVEL) TO ERCTBL-TABLE.                   CL*27
02857      PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT.            CL**5
02858                                                                      CL**5
02859      IF ERCTBL-NOT-FOUND                                             CL**5
02860          MOVE 'AA'               TO  ERCTBL-BEN-CODE                 CL*31
02861          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT         CL**5
02862          IF ERCTBL-NOT-FOUND                                         CL**5
02863              MOVE -1             TO  AAGENTL                         CL**5
02864              MOVE ER-2916        TO  EMI-ERROR                       CL**5
02865              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
02866              PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT        CL**5
02867              GO TO 8200-SEND-DATAONLY.                               CL**5
02868                                                                      CL**5
02869      PERFORM 6680-AH-GET-COMP-RATE.                                  CL*27
02870                                                                   EL642
02871  4410-FIND-LIFE-COMMISSION.                                       EL642
02872      IF PB-CANCELLATION                                              CL**5
02873          MOVE PB-CI-LF-BENEFIT-CD  TO CLBENF-CD                      CL*31
02874      ELSE                                                            CL**5
02875          MOVE PB-I-LF-BENEFIT-CD   TO CLBENF-CD.                     CL*31
02876                                                                   EL642
02877      MOVE 'LF'                   TO WS-COMM-SW.                      CL*27
02878                                                                      CL*31
02879      IF CLBENF-CD  =  ZEROS  OR  SPACES                              CL**7
02880          MOVE ZEROS              TO  WS-GA-LF-COM                    CL**5
02881          GO TO 4490-EXIT.                                         EL642
02882                                                                   EL642
02883      IF PB-ISSUE                                                     CL**5
02884          MOVE PB-I-LIFE-INDICATOR TO LIFE-INDICATOR                  CL*31
02885          GO TO 4420-CONT-FIND-LIFE-COMMISSION.                    EL642
02886                                                                   EL642
02887      PERFORM 6800-READ-BENEFIT-MASTER  THRU  6890-EXIT.              CL**5
02888                                                                      CL**5
02889      IF BENEFIT-MASTER-FOUND                                         CL**5
02890          NEXT SENTENCE                                               CL**5
02891      ELSE                                                            CL**5
02892          MOVE -1                 TO  AAGENTL                         CL**5
02893          MOVE ER-2917            TO  EMI-ERROR                       CL**5
02894          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**5
02895          PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT            CL**5
02896          GO TO 8200-SEND-DATAONLY.                                EL642
02897                                                                   EL642
02898  4420-CONT-FIND-LIFE-COMMISSION.                                  EL642
02899 * SET UP COMMISSION TABLE KEY BEFORE DETERMINING COMMISSION          CL*27
02900                                                                      CL*27
02901      IF PB-ISSUE                                                     CL*27
02902          MOVE PB-I-LF-BENEFIT-CD  TO  ERCTBL-BEN-CODE                CL*31
02903      ELSE                                                            CL*27
02904          MOVE PB-CI-LF-BENEFIT-CD TO  ERCTBL-BEN-CODE.               CL*31
02905                                                                      CL*31
02906      MOVE 'L'                     TO  ERCTBL-BEN-TYPE.               CL*31
02907                                                                      CL*31
02908      IF JOINT-LIFE                                                   CL**5
02909          NEXT SENTENCE                                               CL**5
02910      ELSE                                                            CL**5
02911          GO TO 4430-SINGLE-LIFE-COMMISSION.                       EL642
02912                                                                   EL642
02913      IF AM-J-COM (WS-GA-LEVEL)  NUMERIC                              CL**5
02914          MOVE AM-J-COM (WS-GA-LEVEL)                                 CL**5
02915                                  TO  WS-COMMISSION                   CL*27
02916          PERFORM 6650-GET-RES-STATE-COMM THRU 6700-COMM-EXIT         CL*27
02917          MOVE WS-COMMISSION      TO  WS-GA-LF-COM                    CL*27
02918          GO TO 4490-EXIT.                                         EL642
02919                                                                   EL642
02920      MOVE ZEROS                  TO WS-SUB1.                         CL*27
02921      PERFORM 6650-GET-RES-STATE-COMM THRU 6700-COMM-EXIT.            CL*27
02922      IF WS-SUB1 IS GREATER THAN ZEROS                                CL*27
02923          GO TO 4490-EXIT.                                            CL*27
02924                                                                   EL642
02925      MOVE AM-J-COMA (WS-GA-LEVEL) TO ERCTBL-TABLE.                   CL*27
02926                                                                      CL**5
02927      PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT.            CL**5
02928                                                                      CL**5
02929      IF ERCTBL-NOT-FOUND                                             CL**5
02930          MOVE 'AA'               TO  ERCTBL-BEN-CODE                 CL*31
02931          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT         CL**5
02932          IF ERCTBL-NOT-FOUND                                      EL642
02933              MOVE -1             TO  AAGENTL                         CL**5
02934              MOVE ER-2916        TO  EMI-ERROR                       CL**5
02935              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
02936              PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT        CL**5
02937              GO TO 8200-SEND-DATAONLY.                               CL**5
02938                                                                   EL642
02939      PERFORM 6680-LF-GET-COMP-RATE.                                  CL*27
02940      GO TO 4490-EXIT.                                             EL642
02941                                                                   EL642
02942  4430-SINGLE-LIFE-COMMISSION.                                     EL642
02943 * SET UP COMMISSION TABLE KEY BEFORE DETERMINING COMMISSION          CL*27
02944                                                                      CL*27
02945      IF PB-ISSUE                                                     CL*27
02946          MOVE PB-I-LF-BENEFIT-CD  TO  ERCTBL-BEN-CODE                CL*31
02947      ELSE                                                            CL*27
02948          MOVE PB-CI-LF-BENEFIT-CD TO  ERCTBL-BEN-CODE.               CL*31
02949                                                                      CL*27
02950      MOVE 'L'                     TO  ERCTBL-BEN-TYPE.               CL*31
02951 *                                                                    CL*27
02952      IF AM-L-COM (WS-GA-LEVEL)  NUMERIC                              CL**5
02953          MOVE AM-L-COM (WS-GA-LEVEL)                                 CL**5
02954                                  TO  WS-COMMISSION                   CL*27
02955          PERFORM 6650-GET-RES-STATE-COMM THRU 6700-COMM-EXIT         CL*27
02956          MOVE WS-COMMISSION      TO  WS-GA-LF-COM                    CL*27
02957          GO TO 4490-EXIT.                                         EL642
02958                                                                   EL642
02959      MOVE ZEROS                  TO WS-SUB1.                         CL*27
02960      PERFORM 6650-GET-RES-STATE-COMM THRU 6700-COMM-EXIT.            CL*27
02961      IF WS-SUB1 IS GREATER THAN ZEROS                                CL*27
02962          GO TO 4490-EXIT.                                            CL*27
02963                                                                      CL**5
02964      MOVE ZEROS                  TO WS-SUB1.                         CL*27
02965      MOVE AM-L-COMA (WS-GA-LEVEL) TO ERCTBL-TABLE                    CL*27
02966                                                                      CL**5
02967      PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT.            CL**5
02968                                                                      CL**5
02969      IF ERCTBL-NOT-FOUND                                             CL**5
02970          MOVE 'AA'               TO  ERCTBL-BEN-CODE                 CL*31
02971          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT         CL**5
02972          IF ERCTBL-NOT-FOUND                                      EL642
02973              MOVE -1             TO  AAGENTL                         CL**5
02974              MOVE ER-2916        TO  EMI-ERROR                       CL**5
02975              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
02976              PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT        CL**5
02977              GO TO 8200-SEND-DATAONLY.                               CL**5
02978                                                                   EL642
02979      PERFORM 6680-LF-GET-COMP-RATE.                                  CL*27
02980      GO TO 4490-EXIT.                                                CL*27
02981                                                                   EL642
02982  4490-EXIT.                                                       EL642
02983      EXIT.                                                        EL642
02984  EJECT                                                               CL**5
02985  4500-PYAJ-START-BROWSE.                                          EL642
02986      MOVE LOW-VALUES             TO  ERPYAJ-KEY.                     CL**7
02987      MOVE PI-COMPANY-CD          TO  ERPYAJ-COMP-CD.                 CL**5
02988                                                                   EL642
02989      IF GENERAL-AGENT                                                CL**5
02990          MOVE PI-SAV-CARR        TO  ERPYAJ-CARRIER                  CL**5
02991          MOVE PI-SAV-GROUP       TO  ERPYAJ-GROUPING                 CL**5
02992          MOVE PI-SAV-AGENT       TO  ERPYAJ-FIN-RESP                 CL**5
02993          MOVE LOW-VALUES         TO  ERPYAJ-ACCOUNT                  CL**5
02994      ELSE                                                            CL**5
02995          MOVE PI-COMP-CARRIER    TO  ERPYAJ-CARRIER                  CL**5
02996          MOVE PI-COMP-GROUPING   TO  ERPYAJ-GROUPING                 CL**5
02997          MOVE PI-COMP-FIN-RESP   TO  ERPYAJ-FIN-RESP                 CL**5
02998          MOVE PI-CR-ACCOUNT      TO  ERPYAJ-ACCOUNT.                 CL**5
02999                                                                   EL642
03000      MOVE ERPYAJ-KEY             TO  ERPYAJ-BROWSE-COMP-KEY.         CL**5
03001                                                                   EL642
03002      EXEC CICS HANDLE CONDITION                                   EL642
03003          NOTFND  (4510-REC-NOT-FND)                                  CL**5
03004      END-EXEC.                                                       CL*27
03005                                                                   EL642
03006      EXEC CICS STARTBR                                            EL642
03007          DATASET  (ERPYAJ-FILE-ID)                                   CL**5
03008          RIDFLD   (ERPYAJ-KEY)                                       CL**5
03009          GTEQ                                                     EL642
03010      END-EXEC.                                                       CL*27
03011                                                                   EL642
03012      GO TO 4590-EXIT.                                             EL642
03013                                                                   EL642
03014  4510-REC-NOT-FND.                                                EL642
03015      MOVE 'Y'                    TO  PYAJ-EOF-SW.                    CL**5
03016                                                                   EL642
03017  4590-EXIT.                                                       EL642
03018      EXIT.                                                        EL642
03019  EJECT                                                               CL**5
03020  4600-PYAJ-READ-NEXT.                                             EL642
03021      EXEC CICS HANDLE CONDITION                                   EL642
03022          ENDFILE  (4610-END-OF-FILE)                                 CL**5
03023      END-EXEC.                                                       CL*27
03024                                                                   EL642
03025      EXEC CICS READNEXT                                           EL642
03026          SET      (ADDRESS OF PENDING-PAY-ADJ)                       CL*26
03027          DATASET  (ERPYAJ-FILE-ID)                                   CL**5
03028          RIDFLD   (ERPYAJ-KEY)                                       CL**5
03029      END-EXEC.                                                       CL*27
03030                                                                      CL**5
03031      GO TO 4690-EXIT.                                             EL642
03032                                                                   EL642
03033  4610-END-OF-FILE.                                                EL642
03034      MOVE 'Y'                    TO  PYAJ-EOF-SW.                    CL**5
03035                                                                   EL642
03036  4690-EXIT.                                                       EL642
03037      EXIT.                                                        EL642
03038  EJECT                                                               CL**5
03039  4700-PROCESS-COMM-EXCEPTION.                                     EL642
03040      PERFORM 6600-READ-COMMISSION-EXCEPTION  THRU  6620-EXIT.        CL*27
03041                                                                   EL642
03042      IF ERCOMM-NOT-FOUND                                             CL**5
03043          MOVE -1                 TO  AAGENTL                         CL**5
03044          MOVE ER-2915            TO  EMI-ERROR                       CL**5
03045          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**5
03046          PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT            CL**5
03047          GO TO 8200-SEND-DATAONLY.                                EL642
03048                                                                   EL642
03049      MOVE WS-GA-LEVEL            TO  CE-SUB.                         CL**5
03050      MOVE PI-COMPANY-CD          TO  ERCTBL-COMPANY-CD.              CL**5
03051                                                                   EL642
03052      IF CE-LF-COMP (CE-SUB)  NUMERIC                                 CL**5
03053          IF CE-AH-COMP (CE-SUB)  NUMERIC                             CL**5
03054              MOVE CE-LF-COMP (CE-SUB)  TO  WS-GA-LF-COM              CL**5
03055              MOVE CE-AH-COMP (CE-SUB)  TO  WS-GA-AH-COM              CL**5
03056              GO TO 4790-EXIT.                                        CL**5
03057                                                                   EL642
03058  4715-FIND-AH-COMMISSION.                                         EL642
03059      IF CE-AH-COMP (CE-SUB)  NUMERIC                                 CL**5
03060          MOVE CE-AH-COMP (CE-SUB)  TO  WS-GA-AH-COM                  CL**5
03061          GO TO 4750-FIND-LIFE-COMMISSION.                         EL642
03062                                                                   EL642
03063      MOVE CE-AH-COMPT (CE-SUB)   TO  ERCTBL-TABLE.                   CL**5
03064                                                                   EL642
03065      IF PB-ISSUE                                                     CL**5
03066          MOVE PB-I-AH-BENEFIT-CD   TO  ERCTBL-BEN-CODE               CL**5
03067      ELSE                                                            CL**5
03068          MOVE PB-CI-AH-BENEFIT-CD  TO  ERCTBL-BEN-CODE.              CL**5
03069                                                                   EL642
03070      IF ERCTBL-BEN-CODE  =  ZEROS                                    CL**5
03071          MOVE ZEROS              TO  WS-GA-AH-COM                    CL**5
03072          GO TO 4750-FIND-LIFE-COMMISSION.                         EL642
03073                                                                   EL642
03074      MOVE 'A'                    TO  ERCTBL-BEN-TYPE.                CL**5
03075                                                                      CL**5
03076      PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT.            CL**5
03077                                                                      CL**5
03078      IF ERCTBL-NOT-FOUND                                             CL**5
03079          MOVE 'AA'               TO  ERCTBL-BEN-CODE                 CL*31
03080          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT         CL**5
03081          IF ERCTBL-NOT-FOUND                                      EL642
03082              MOVE -1             TO  AAGENTL                         CL**5
03083              MOVE ER-2916        TO  EMI-ERROR                       CL**5
03084              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
03085              PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT        CL**5
03086              GO TO 8200-SEND-DATAONLY.                            EL642
03087                                                                   EL642
03088      IF PB-ISSUE                                                     CL**5
03089          COMPUTE WS-COMM-CK-AMT =                                 EL642
03090              PB-I-AH-BENEFIT-AMT * PB-I-AH-TERM                      CL**5
03091          MOVE PB-I-AGE           TO  WS-COMM-AGE                     CL**5
03092          MOVE PB-I-AH-TERM       TO  WS-COMM-TERM                    CL**5
03093      ELSE                                                         EL642
03094          COMPUTE WS-COMM-CK-AMT =                                 EL642
03095              PB-CI-AH-BENEFIT-AMT * PB-CI-AH-TERM                    CL**5
03096          MOVE PB-CI-INSURED-AGE  TO  WS-COMM-AGE                     CL**5
03097          MOVE PB-CI-AH-TERM      TO  WS-COMM-TERM.                   CL**5
03098                                                                   EL642
03099      PERFORM 4900-GET-COMP-RATE  THRU  4990-EXIT.                    CL**5
03100                                                                      CL**5
03101      MOVE WS-WK-RATE             TO  WS-GA-AH-COM.                   CL**5
03102                                                                   EL642
03103  4750-FIND-LIFE-COMMISSION.                                       EL642
03104      IF PB-ISSUE                                                     CL**5
03105          MOVE PB-I-LF-BENEFIT-CD   TO  CLBENF-CD                     CL**5
03106      ELSE                                                            CL**5
03107          MOVE PB-CI-LF-BENEFIT-CD  TO  CLBENF-CD.                    CL**5
03108                                                                   EL642
03109      IF CLBENF-CD  =  ZEROS  OR  SPACES                              CL**7
03110          MOVE ZEROS              TO  WS-GA-LF-COM                    CL**5
03111          GO TO 4790-EXIT.                                         EL642
03112                                                                   EL642
03113      IF PB-ISSUE                                                     CL**5
03114          MOVE PB-I-LIFE-INDICATOR  TO  LIFE-INDICATOR                CL**5
03115          GO TO 4760-CONT-FIND-LIFE-COMMISSION.                    EL642
03116                                                                   EL642
03117      PERFORM 6800-READ-BENEFIT-MASTER  THRU  6890-EXIT.              CL**5
03118                                                                      CL**5
03119      IF BENEFIT-MASTER-FOUND                                         CL**5
03120          NEXT SENTENCE                                               CL**5
03121      ELSE                                                            CL**5
03122          MOVE -1                 TO  AAGENTL                         CL**5
03123          MOVE ER-2917            TO  EMI-ERROR                       CL**5
03124          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**5
03125          PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT            CL**5
03126          GO TO 8200-SEND-DATAONLY.                                EL642
03127                                                                   EL642
03128  4760-CONT-FIND-LIFE-COMMISSION.                                  EL642
03129      IF JOINT-LIFE NEXT SENTENCE                                     CL**5
03130      ELSE                                                            CL**5
03131          GO TO 4770-SINGLE-LIFE-COMMISSION.                       EL642
03132                                                                   EL642
03133      IF CE-LF-COMP (CE-SUB)  NUMERIC                                 CL**5
03134          MOVE CE-LF-COMP (CE-SUB)  TO  WS-GA-LF-COM                  CL**5
03135          GO TO 4790-EXIT.                                         EL642
03136                                                                   EL642
03137      MOVE CE-LF-COMPT (CE-SUB)   TO  ERCTBL-TABLE.                   CL**5
03138                                                                   EL642
03139      IF PB-ISSUE                                                     CL**5
03140          MOVE PB-I-LF-BENEFIT-CD   TO  ERCTBL-BEN-CODE               CL**5
03141      ELSE                                                            CL**5
03142          MOVE PB-CI-LF-BENEFIT-CD  TO  ERCTBL-BEN-CODE.              CL**5
03143                                                                      CL**5
03144      MOVE 'J'                    TO  ERCTBL-BEN-TYPE.                CL**5
03145                                                                      CL**5
03146      PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT.            CL**5
03147                                                                      CL**5
03148      IF ERCTBL-NOT-FOUND                                             CL**5
03149          MOVE 'AA'               TO  ERCTBL-BEN-CODE                 CL*31
03150          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT         CL**5
03151          IF ERCTBL-NOT-FOUND                                      EL642
03152              MOVE -1             TO  AAGENTL                         CL**5
03153              MOVE ER-2916        TO  EMI-ERROR                       CL**5
03154              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
03155              PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT        CL**5
03156              GO TO 8200-SEND-DATAONLY.                            EL642
03157                                                                   EL642
03158      IF PB-ISSUE                                                     CL**5
03159          MOVE PB-I-LF-BENEFIT-AMT   TO  WS-COMM-CK-AMT               CL**5
03160          MOVE PB-I-AGE              TO  WS-COMM-AGE                  CL**5
03161          MOVE PB-I-LF-TERM          TO  WS-COMM-TERM                 CL**5
03162      ELSE                                                         EL642
03163          MOVE PB-CI-INSURED-AGE     TO  WS-COMM-AGE                  CL**5
03164          MOVE PB-CI-LF-TERM         TO  WS-COMM-TERM                 CL**5
03165          MOVE PB-CI-LF-BENEFIT-AMT  TO  WS-COMM-CK-AMT.              CL**5
03166                                                                   EL642
03167      PERFORM 4900-GET-COMP-RATE  THRU  4990-EXIT.                    CL**5
03168                                                                      CL**5
03169      MOVE WS-WK-RATE             TO  WS-GA-LF-COM.                   CL**5
03170                                                                   EL642
03171      GO TO 4790-EXIT.                                             EL642
03172                                                                   EL642
03173  4770-SINGLE-LIFE-COMMISSION.                                     EL642
03174      IF CE-LF-COMP (CE-SUB)  NUMERIC                                 CL**5
03175          MOVE CE-LF-COMP (CE-SUB)  TO  WS-GA-LF-COM                  CL**5
03176          GO TO 4790-EXIT.                                         EL642
03177                                                                   EL642
03178      MOVE CE-LF-COMPT (CE-SUB)   TO  ERCTBL-TABLE.                   CL**5
03179                                                                   EL642
03180      IF PB-ISSUE                                                     CL**5
03181          MOVE PB-I-LF-BENEFIT-CD   TO  ERCTBL-BEN-CODE               CL**5
03182      ELSE                                                            CL**5
03183          MOVE PB-CI-LF-BENEFIT-CD  TO  ERCTBL-BEN-CODE.              CL**5
03184                                                                      CL**5
03185      MOVE 'L'                    TO  ERCTBL-BEN-TYPE.                CL**5
03186                                                                      CL**5
03187      PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT.            CL**5
03188                                                                      CL**5
03189      IF ERCTBL-NOT-FOUND                                             CL**5
03190          MOVE 'AA'               TO  ERCTBL-BEN-CODE                 CL*31
03191          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT         CL**5
03192          IF ERCTBL-NOT-FOUND                                      EL642
03193              MOVE -1             TO  AAGENTL                         CL**5
03194              MOVE ER-2916        TO  EMI-ERROR                       CL**5
03195              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
03196              PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT        CL**5
03197              GO TO 8200-SEND-DATAONLY.                            EL642
03198                                                                   EL642
03199      IF PB-ISSUE                                                     CL**5
03200          MOVE PB-I-LF-BENEFIT-AMT   TO  WS-COMM-CK-AMT               CL**5
03201          MOVE PB-I-AGE              TO  WS-COMM-AGE                  CL**5
03202          MOVE PB-I-LF-TERM          TO  WS-COMM-TERM                 CL**5
03203      ELSE                                                         EL642
03204          MOVE PB-CI-INSURED-AGE     TO  WS-COMM-AGE                  CL**5
03205          MOVE PB-CI-LF-TERM         TO  WS-COMM-TERM                 CL**5
03206          MOVE PB-CI-LF-BENEFIT-AMT  TO  WS-COMM-CK-AMT.              CL**5
03207                                                                   EL642
03208      PERFORM 4900-GET-COMP-RATE  THRU  4990-EXIT.                    CL**5
03209                                                                   EL642
03210      MOVE WS-WK-RATE             TO  WS-GA-LF-COM.                   CL**5
03211                                                                   EL642
03212  4790-EXIT.                                                       EL642
03213      EXIT.                                                        EL642
03214  EJECT                                                               CL**5
03215  4800-PYAJ-END-BROWSE.                                            EL642
03216      EXEC CICS ENDBR                                              EL642
03217          DATASET  (ERPYAJ-FILE-ID)                                   CL**5
03218      END-EXEC.                                                       CL*27
03219  EJECT                                                               CL**5
03220  4900-GET-COMP-RATE.                                              EL642
03221      MOVE +1                     TO  COMP-SUB.                       CL**5
03222                                                                   EL642
03223      IF WS-COMM-CK-AMT  GREATER THAN  CT-TBF (1)                     CL**5
03224          ADD +9                  TO  COMP-SUB                        CL**5
03225          IF WS-COMM-CK-AMT  GREATER THAN  CT-TBF (2)                 CL**5
03226              ADD +9              TO  COMP-SUB                        CL**5
03227              IF WS-COMM-CK-AMT  GREATER THAN  CT-TBF (3)             CL**5
03228                  MOVE -1         TO  AAGENTL                         CL**5
03229                  MOVE ER-2918    TO  EMI-ERROR                       CL**5
03230                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL**5
03231                  PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT    CL**5
03232                  GO TO 8200-SEND-DATAONLY.                           CL**5
03233                                                                   EL642
03234      IF WS-COMM-AGE  GREATER THAN  CT-AGE (1)                        CL**5
03235          ADD +3                  TO  COMP-SUB                        CL**5
03236          IF WS-COMM-AGE  GREATER THAN  CT-AGE (2)                    CL**5
03237              ADD +1              TO  COMP-SUB                        CL**5
03238              IF WS-COMM-AGE  GREATER THAN  CT-AGE (3)                CL**5
03239                  MOVE -1         TO  AAGENTL                         CL**5
03240                  MOVE ER-2918    TO  EMI-ERROR                       CL**5
03241                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL**5
03242                  PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT    CL**5
03243                  GO TO 8200-SEND-DATAONLY.                           CL**5
03244                                                                   EL642
03245      IF WS-COMM-TERM  GREATER  CT-TRM (1)                            CL**5
03246          ADD +1                  TO  COMP-SUB                        CL**5
03247          IF WS-COMM-TERM  GREATER THAN  CT-TRM (2)                   CL**5
03248              ADD +1              TO  COMP-SUB                        CL**5
03249              IF WS-COMM-TERM  GREATER THAN  CT-TRM (3)               CL**5
03250                  MOVE -1         TO  AAGENTL                         CL**5
03251                  MOVE ER-2918    TO  EMI-ERROR                       CL**5
03252                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL**5
03253                  PERFORM 8900-SYNCPOINT-ROLLBACK  THRU  8900-EXIT    CL**5
03254                  GO TO 8200-SEND-DATAONLY.                           CL**5
03255                                                                      CL**5
03256      IF CT-RT (COMP-SUB)  NOT  NUMERIC                               CL**5
03257          MOVE CT-RT-R (COMP-SUB)  TO  ERCTBL-TABLE                   CL**5
03258          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT         CL**5
03259          GO TO 4900-GET-COMP-RATE.                                EL642
03260                                                                   EL642
03261      MOVE CT-RT (COMP-SUB)       TO  WS-WK-RATE.                     CL**5
03262                                                                   EL642
03263  4990-EXIT.                                                       EL642
03264      EXIT.                                                        EL642
03265  EJECT                                                               CL**5
03266  5000-FORMAT-SCREEN.                                              EL642
03267      MOVE LOW-VALUES             TO  EL642AO.                        CL**5
03268                                                                   EL642
03269      IF PI-SAV-AGENT  GREATER THAN  SPACES                           CL**5
03270          MOVE PI-SAV-AGENT       TO  AAGENTO                         CL**5
03271          MOVE PI-SAV-CARR        TO  ACARIERO                        CL**5
03272          MOVE PI-SAV-GROUP       TO  AGROUPO.                        CL**5
03273                                                                   EL642
03274      MOVE PI-BILL-TYPE           TO  ABILTYPI.                       CL**5
03275      MOVE AL-UANON               TO  AAGENTA                         CL**5
03276                                      ACARIERA                        CL**5
03277                                      AGROUPA                         CL**5
03278                                      ABILTYPA.                       CL**5
03279                                                                      CL**5
03280      IF PI-BILLING-ACCOUNTS (1)  NOT =  SPACES                       CL**5
03281          MOVE PI-BILLING-ACCOUNTS (1)                                CL**5
03282                                  TO  AACCT1I                         CL**5
03283          MOVE AL-UANON           TO  AACCT1A.                        CL**5
03284                                                                      CL**5
03285      IF PI-BILLING-ACCOUNTS (2)  NOT =  SPACES                       CL**5
03286          MOVE PI-BILLING-ACCOUNTS (2)                                CL**5
03287                                  TO  AACCT2I                         CL**5
03288          MOVE AL-UANON           TO  AACCT2A.                        CL**5
03289                                                                      CL**5
03290      IF PI-BILLING-ACCOUNTS (3)  NOT =  SPACES                       CL**5
03291          MOVE PI-BILLING-ACCOUNTS (3)                                CL**5
03292                                  TO  AACCT3I                         CL**5
03293          MOVE AL-UANON           TO  AACCT3A.                        CL**5
03294                                                                   EL642
03295  5005-FORMAT-STATS.                                               EL642
03296                                                                   EL642
03297      IF PI-CR-FIN-RESP = SPACES                                      CL*31
03298          GO TO 5090-EXIT.                                            CL*31
03299                                                                      CL*23
03300      MOVE PI-BAL-FRWD            TO  ABEGBALO.                       CL*25
03301      MOVE PI-UNPAID-NET-PREM     TO  AUPDPRMO.                       CL**5
03302      MOVE PI-COMP-UNPAID-PREM    TO  AUPDCOMO.                       CL**5
03303      MOVE PI-ISSUES-BILLED       TO  ABILL1O.                        CL**5
03304      MOVE PI-ISSUES-INER         TO  AINER1O.                        CL**5
03305      MOVE PI-ISSUES-PREV         TO  APRVBL1O.                       CL**5
03306      MOVE PI-CANCELS-BILLED      TO  ABILL2O.                        CL**5
03307      MOVE PI-CANCELS-INER        TO  AINER2O.                        CL**5
03308      MOVE PI-CANCELS-PREV        TO  APRVBL2O.                       CL**5
03309                                                                   EL642
03310                                                                   EL642
03311      IF PI-UNKNOWN  GREATER THAN  ZEROS                              CL**5
03312          MOVE ER-2912            TO  EMI-ERROR                       CL**5
03313          MOVE PI-UNKNOWN         TO  AUNKNERO                        CL**5
03314          MOVE AL-SABOF           TO  AUNKNERA                        CL**5
03315          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                 CL**5
03316                                                                   EL642
03317      MOVE PI-PREMIUM             TO  APREMUMO.                       CL**5
03318      MOVE PI-REMITTED            TO  AREMITO.                        CL**5
03319      MOVE PI-TOT-ISS-COMP        TO  ACOMPISO.                       CL**5
03320      MOVE PI-TOT-CAN-COMP        TO  ACOMCANO.                       CL**5
03321      MOVE PI-ADJUSTMNTS          TO  AADJUSTO.                       CL**5
03322      MOVE PI-DISBURSED           TO  ADISBURO.                       CL**5
03323      MOVE ZERO                   TO  PI-END-BAL.                     CL*31
03324                                                                   EL642
03325      IF PI-SAV-AGENT = PI-SAV-REMIT-TO                               CL**5
03326          COMPUTE PI-DUE-FOR-ACCT =                                   CL**5
03327              (PI-ACCT-BEG-BAL + PI-ACCT-NET-PREM) -                  CL**5
03328               (PI-ACCT-COMP + PI-ACCT-PAY-ADJS).                     CL**5
03329                                                                   EL642
03330      COMPUTE PI-END-BAL = PI-BAL-FRWD - PI-TOT-ISS-COMP              CL*25
03331                         + PI-TOT-CAN-COMP + PI-ADJUSTMNTS            CL**5
03332                         - PI-REMITTED + PI-DISBURSED.                CL**5
03333                                                                   EL642
03334      MOVE PI-END-BAL             TO  ANETDUEO.                       CL**5
03335                                                                      CL**5
03336      IF PI-END-BAL  NEGATIVE                                         CL**5
03337          MOVE OWED-TO-AGENT      TO  AENDHDGO                        CL**5
03338      ELSE                                                         EL642
03339          IF PI-END-BAL  GREATER THAN  ZERO                           CL**5
03340              MOVE AGENT-OWES     TO  AENDHDGO                        CL**5
03341          ELSE                                                        CL**5
03342              MOVE 'BALANCE='     TO  AENDHDGO.                       CL**5
03343                                                                      CL**5
03344      IF PI-SAV-AGENT = PI-SAV-REMIT-TO                               CL**5
03345          IF PI-DUE-FOR-ACCT  NOT =  ZERO                             CL**5
03346              MOVE PI-DUE-FOR-ACCT                                    CL**5
03347                                  TO  AACTDUEO                        CL**5
03348              IF PI-DUE-FOR-ACCT  NEGATIVE                            CL**5
03349                  MOVE 'DUE GA ON BEHALF OF ACCOUNTS'                 CL**5
03350                                  TO  AACTHDGO                        CL**5
03351              ELSE                                                    CL**5
03352                  MOVE 'GA OWES ON BEHALF OF ACCOUNTS'                CL**5
03353                                  TO  AACTHDGO.                       CL**5
03354                                                                   EL642
03355  5080-CONT.                                                       EL642
03356      IF RETURNED-FROM  NOT =  SPACES                                 CL**5
03357          IF RETURNED-FROM  NOT =  XCTL-6401                          CL**5
03358              MOVE ER-2421        TO  EMI-ERROR                       CL**5
03359              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
03360              MOVE ZEROS          TO  EMI-ERROR.                      CL**5
03361                                                                   EL642
03362  5090-EXIT.                                                       EL642
03363      EXIT.                                                        EL642
03364  EJECT                                                               CL**5
03365  6000-READ-COMP-MASTER.                                           EL642
03366      EXEC CICS HANDLE CONDITION                                   EL642
03367          NOTFND  (6070-NO-COMP-MSTR)                                 CL**5
03368      END-EXEC.                                                       CL*27
03369                                                                   EL642
03370      MOVE PI-COMPANY-CD          TO  ERCOMP-COMP-CD.                 CL**5
03371                                                                   EL642
03372      IF GENERAL-AGENT                                                CL**5
03373          MOVE PI-SAV-CARR        TO  ERCOMP-CARRIER                  CL**5
03374          MOVE PI-SAV-GROUP       TO  ERCOMP-GROUPING                 CL**5
03375          MOVE PI-SAV-AGENT       TO  ERCOMP-FIN-RESP                 CL**5
03376      ELSE                                                         EL642
03377          MOVE PI-COMP-CARRIER    TO  ERCOMP-CARRIER                  CL**5
03378          MOVE PI-COMP-GROUPING   TO  ERCOMP-GROUPING                 CL**5
03379          MOVE PI-COMP-FIN-RESP   TO  ERCOMP-FIN-RESP.                CL**5
03380                                                                      CL**5
03381      MOVE ERCOMP-FIN-RESP        TO  PI-CR-FIN-RESP.                 CL**5
03382                                                                      CL**5
03383      IF EIBAID  =  DFHPF5  OR  GENERAL-AGENT                         CL**5
03384          MOVE LOW-VALUES         TO  ERCOMP-ACCT                     CL**5
03385          MOVE 'G'                TO  ERCOMP-RECORD-TYPE              CL**5
03386      ELSE                                                            CL**5
03387          MOVE PI-SAV-ACCT-AGT    TO  ERCOMP-ACCT                     CL**5
03388          MOVE 'A'                TO  ERCOMP-RECORD-TYPE.             CL**5
03389                                                                   EL642
03390      IF ERCOMP-KEY  =  ERCOMP-PREV-KEY                               CL**5
03391          GO TO 6090-EXIT.                                            CL**5
03392                                                                   EL642
03393      MOVE ERCOMP-KEY             TO  ERCOMP-PREV-KEY.             EL642
03394                                                                   EL642
03395      EXEC CICS READ                                               EL642
03396          DATASET  (ERCOMP-FILE-ID)                                   CL**5
03397          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL*26
03398          RIDFLD   (ERCOMP-KEY)                                       CL**5
03399      END-EXEC.                                                       CL*27
03400                                                                   EL642
03401      IF PI-MAP-NAME  =  EL642B                                       CL**5
03402          GO TO 6090-EXIT.                                         EL642
03403                                                                   EL642
03404      IF GENERAL-AGENT                                             EL642
03405          MOVE CO-CURRENT-END-BAL TO  PI-BAL-FRWD                     CL*25
03406      ELSE                                                         EL642
03407          IF PI-SAV-AGENT  =  PI-SAV-REMIT-TO                         CL**5
03408              MOVE CO-CURRENT-END-BAL                                 CL*25
03409                                  TO  WS-ACCT-BEG-BAL                 CL*25
03410              GO TO 6090-EXIT                                         CL**5
03411          ELSE                                                        CL**5
03412              MOVE ZEROS          TO  WS-ACCT-BEG-BAL                 CL**5
03413              GO TO 6090-EXIT.                                        CL**5
03414                                                                   EL642
03415      IF PI-UPDATE-FILES                                              CL**5
03416        OR  (PI-PREVIEW  AND  APRODSWI  =  'Y')                       CL**5
03417          NEXT SENTENCE                                            EL642
03418      ELSE                                                         EL642
03419          GO TO 6090-EXIT.                                            CL**5
03420                                                                   EL642
03421  6002-FORMAT-ADDRESS-LOOP.                                        EL642
03422      MOVE CO-ACCT-NAME           TO  PI-AGENT-NAME.                  CL**5
03423      MOVE CO-MAIL-NAME           TO  WS-AGENT-LINES (1).             CL**5
03424      MOVE CO-ACCT-NAME           TO  WS-AGENT-LINES (2).             CL**5
03425      MOVE CO-ADDR-1              TO  WS-AGENT-LINES (3).             CL**5
03426      MOVE CO-ADDR-2              TO  WS-AGENT-LINES (4).             CL**5
03427      MOVE CO-ADDR-3              TO  WS-AGENT-LINES (5).             CL**5
03428      MOVE CO-ZIP                 TO  WS-AGENT-LINES (6).             CL**5
03429                                                                   EL642
03430  6060-FORMAT-ADDRESS-LOOP.                                        EL642
03431      IF WS-AGENT-ADDR-AREA  =  SPACES                                CL**5
03432          GO TO 6090-EXIT.                                            CL**5
03433                                                                   EL642
03434      IF WS-AGENT-LINES (1)  =  SPACES                                CL**5
03435          MOVE WS-AGENT-LINES (2)  TO  WS-AGENT-LINES (1)             CL**5
03436          MOVE WS-AGENT-LINES (3)  TO  WS-AGENT-LINES (2)             CL**5
03437          MOVE WS-AGENT-LINES (4)  TO  WS-AGENT-LINES (3)             CL**5
03438          MOVE WS-AGENT-LINES (5)  TO  WS-AGENT-LINES (4)             CL**5
03439          MOVE WS-AGENT-LINES (6)  TO  WS-AGENT-LINES (5)             CL**5
03440          MOVE SPACES              TO  WS-AGENT-LINES (6)             CL**5
03441          GO TO 6060-FORMAT-ADDRESS-LOOP.                             CL**5
03442                                                                   EL642
03443      IF WS-AGENT-LINES (2)  =  SPACES                                CL**5
03444        AND  WS-AGENT-LINES (3)  =  SPACES                            CL**5
03445        AND  WS-AGENT-LINES (4)  =  SPACES                            CL**5
03446        AND  WS-AGENT-LINES (5)  =  SPACES                            CL**5
03447        AND  WS-AGENT-LINES (6)  =  SPACES                            CL**5
03448          GO TO 6065-MOVE-ZIP.                                        CL**5
03449                                                                   EL642
03450      IF WS-AGENT-LINES (2)  =  SPACES                                CL**5
03451          MOVE WS-AGENT-LINES (3)  TO  WS-AGENT-LINES (2)             CL**5
03452          MOVE WS-AGENT-LINES (4)  TO  WS-AGENT-LINES (3)             CL**5
03453          MOVE WS-AGENT-LINES (5)  TO  WS-AGENT-LINES (4)             CL**5
03454          MOVE WS-AGENT-LINES (6)  TO  WS-AGENT-LINES (5)             CL**5
03455          MOVE SPACES              TO  WS-AGENT-LINES (6)             CL**5
03456          GO TO 6060-FORMAT-ADDRESS-LOOP.                             CL**5
03457                                                                   EL642
03458      IF WS-AGENT-LINES (3)  =  SPACES                                CL**5
03459        AND  WS-AGENT-LINES (4)  =  SPACES                            CL**5
03460        AND  WS-AGENT-LINES (5)  =  SPACES                            CL**5
03461        AND  WS-AGENT-LINES (6)  =  SPACES                            CL**5
03462          GO TO 6065-MOVE-ZIP.                                        CL**5
03463                                                                   EL642
03464      IF WS-AGENT-LINES (3)  =  SPACES                                CL**5
03465          MOVE WS-AGENT-LINES (4)  TO  WS-AGENT-LINES (3)             CL**5
03466          MOVE WS-AGENT-LINES (5)  TO  WS-AGENT-LINES (4)             CL**5
03467          MOVE WS-AGENT-LINES (6)  TO  WS-AGENT-LINES (5)             CL**5
03468          MOVE SPACES              TO  WS-AGENT-LINES (6)             CL**5
03469          GO TO 6060-FORMAT-ADDRESS-LOOP.                             CL**5
03470                                                                   EL642
03471      IF WS-AGENT-LINES (4)  =  SPACES                                CL**5
03472        AND  WS-AGENT-LINES (5)  =  SPACES                            CL**5
03473        AND  WS-AGENT-LINES (6)  =  SPACES                            CL**5
03474          GO TO 6065-MOVE-ZIP.                                        CL**5
03475                                                                   EL642
03476      IF WS-AGENT-LINES (4)  =  SPACES                                CL**5
03477          MOVE WS-AGENT-LINES (5)  TO  WS-AGENT-LINES (4)             CL**5
03478          MOVE WS-AGENT-LINES (6)  TO  WS-AGENT-LINES (5)             CL**5
03479          MOVE SPACES              TO  WS-AGENT-LINES (6)             CL**5
03480          GO TO 6060-FORMAT-ADDRESS-LOOP.                             CL**5
03481                                                                   EL642
03482      IF WS-AGENT-LINES (5)  =  SPACES                                CL**5
03483          MOVE WS-AGENT-LINES (6)  TO  WS-AGENT-LINES (5)             CL**5
03484          MOVE SPACES              TO  WS-AGENT-LINES (6).            CL**5
03485                                                                   EL642
03486  6065-MOVE-ZIP.                                                   EL642
03487                                                                      CL*12
03488      IF WS-AGENT-LINES (6) NOT =  SPACES                             CL*31
03489          IF CO-CANADIAN-POST-CODE                                    CL*31
03490                  OR                                                  CL*12
03491             WS-AGENT-1ST-ZIP (6)  NOT =  ZEROS                       CL*31
03492              MOVE WS-AGENT-ZIP (6)                                   CL**5
03493                                  TO  WS-A-LAST-ZIP (5)               CL**5
03494              MOVE SPACES         TO  WS-AGENT-LINES (6)              CL**5
03495          ELSE                                                        CL**5
03496              MOVE WS-AGENT-2ND-ZIP (5)                               CL**5
03497                                  TO  WS-A-LAST-2ND-ZIP (4)           CL**5
03498              MOVE SPACES         TO  WS-AGENT-LINES (5)              CL**5
03499      ELSE                                                            CL**5
03500          IF WS-AGENT-LINES (5) NOT =  SPACES                         CL*31
03501              IF CO-CANADIAN-POST-CODE                                CL*31
03502                      OR                                              CL*12
03503                 WS-AGENT-1ST-ZIP (5)  NOT =  ZEROS                   CL*31
03504                  MOVE WS-AGENT-ZIP (5)                               CL**5
03505                                  TO  WS-A-LAST-ZIP (5)               CL**5
03506                  MOVE SPACES     TO  WS-AGENT-ZIP (5)                CL**5
03507              ELSE                                                    CL**5
03508                  MOVE WS-AGENT-2ND-ZIP (5)                           CL**5
03509                                  TO  WS-A-LAST-2ND-ZIP (5)           CL**5
03510                  MOVE SPACES     TO  WS-AGENT-ZIP (5)                CL**5
03511          ELSE                                                        CL**5
03512              IF WS-AGENT-LINES (4) NOT =  SPACES                     CL*31
03513                  IF CO-CANADIAN-POST-CODE                            CL*31
03514                          OR                                          CL*12
03515                     WS-AGENT-1ST-ZIP (4)  NOT =  ZEROS               CL*31
03516                      MOVE WS-AGENT-ZIP (4)                           CL**5
03517                                  TO  WS-A-LAST-ZIP (4)               CL**5
03518                      MOVE SPACES TO  WS-AGENT-ZIP (4)                CL*12
03519                  ELSE                                             EL642
03520                      MOVE WS-AGENT-2ND-ZIP (4)                       CL**5
03521                                  TO  WS-A-LAST-2ND-ZIP (4)           CL*12
03522                      MOVE SPACES TO  WS-AGENT-ZIP (4).               CL*12
03523                                                                   EL642
03524      MOVE 'GA'                   TO  BILLING-DETAIL-TYPE.         EL642
03525                                                                      CL**5
03526      PERFORM 3000-WRITE-BILLING-DETAIL  THRU  3990-EXIT.             CL**5
03527                                                                   EL642
03528      GO TO 6090-EXIT.                                             EL642
03529                                                                   EL642
03530  6070-NO-COMP-MSTR.                                               EL642
03531      IF GENERAL-AGENT                                                CL**5
03532          MOVE -1                 TO  AAGENTL                         CL**5
03533          MOVE ER-2230            TO  EMI-ERROR                       CL**5
03534          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**5
03535          GO TO 8200-SEND-DATAONLY.                                EL642
03536                                                                   EL642
03537      MOVE ZEROS                  TO  WS-ACCT-BEG-BAL.                CL**5
03538                                                                      CL**5
03539      GO TO 6090-EXIT.                                             EL642
03540                                                                   EL642
03541  6090-EXIT.                                                       EL642
03542      EXIT.                                                        EL642
03543  EJECT                                                               CL**5
03544  6100-READ-CONTROL-FILE.                                          EL642
03545      MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID.              CL**5
03546      MOVE +0                     TO  ELCNTL-SEQ-NO.                  CL**5
03547                                                                   EL642
03548      EXEC CICS HANDLE CONDITION                                   EL642
03549          NOTFND  (6110-NO-RECORD)                                    CL**5
03550      END-EXEC.                                                       CL*27
03551                                                                   EL642
03552      IF NOT  ELCNTL-UPDATE                                           CL**5
03553          EXEC CICS READ                                           EL642
03554              DATASET  (ELCNTL-FILE-ID)                               CL**5
03555              SET      (ADDRESS OF CONTROL-FILE)                      CL*26
03556              RIDFLD   (ELCNTL-KEY)                                   CL**5
03557          END-EXEC                                                    CL*27
03558      ELSE                                                         EL642
03559          MOVE SPACE              TO  ELCNTL-UPDATE-SW                CL**5
03560          EXEC CICS READ                                           EL642
03561              DATASET  (ELCNTL-FILE-ID)                               CL**5
03562              SET      (ADDRESS OF CONTROL-FILE)                      CL*26
03563              RIDFLD   (ELCNTL-KEY)                                   CL**5
03564              UPDATE                                               EL642
03565          END-EXEC.                                                   CL*27
03566                                                                      CL**5
03567      GO TO 6190-EXIT.                                             EL642
03568                                                                   EL642
03569  6110-NO-RECORD.                                                  EL642
03570      IF ELCNTL-REC-TYPE  =  1                                        CL**5
03571          MOVE ER-0022            TO  EMI-ERROR                       CL**5
03572      ELSE                                                         EL642
03573          MOVE ER-2208            TO  EMI-ERROR                       CL**5
03574          MOVE -1                 TO  ACARIERL                        CL**5
03575          MOVE AL-UABON           TO  ACARIERA                        CL**5
03576                                                                      CL**5
03577      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**5
03578                                                                   EL642
03579  6190-EXIT.                                                       EL642
03580      EXIT.                                                        EL642
03581  EJECT                                                               CL**5
03582  6200-REWRITE-CONTROL-FILE.                                       EL642
03583      MOVE 'C'                    TO  JP-RECORD-TYPE.                 CL**5
03584      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.                 CL**5
03585      MOVE ELCNTL-FILE-ID         TO  JP-FILE-ID.                     CL**5
03586                                                                      CL**5
03587      COMPUTE JOURNAL-LENGTH = ELCNTL-LENGTH + 23.                 EL642
03588                                                                      CL**5
03589      EXEC CICS REWRITE                                            EL642
03590          DATASET  (ELCNTL-FILE-ID)                                   CL**5
03591          FROM     (CONTROL-FILE)                                     CL**5
03592      END-EXEC.                                                       CL*27
03593                                                                      CL**5
03594      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL642
03595                                                                   EL642
03596  6290-EXIT.                                                       EL642
03597      EXIT.                                                        EL642
03598  EJECT                                                               CL**5
03599  6400-READ-CROSS-REFERENCE.                                       EL642
03600      MOVE PI-COMPANY-CD          TO  ERGXRF-COMPANY-CD.              CL**5
03601      MOVE PI-SAV-AGENT           TO  ERGXRF-AGENT-NO.                CL**5
03602                                                                   EL642
03603      IF NOT  PI-ZERO-CARRIER                                         CL**5
03604        AND  NOT  PI-ZERO-CAR-GROUP                                   CL**5
03605          MOVE PI-CR-CARRIER      TO  ERGXRF-CARRIER                  CL**5
03606      ELSE                                                         EL642
03607          MOVE ZEROS              TO  ERGXRF-CARRIER.                 CL**5
03608                                                                      CL**5
03609      IF NOT  PI-ZERO-GROUPING                                        CL**5
03610        AND  NOT  PI-ZERO-CAR-GROUP                                   CL**5
03611          MOVE PI-CR-GROUPING     TO  ERGXRF-GROUPING                 CL**5
03612      ELSE                                                         EL642
03613          MOVE ZEROS              TO  ERGXRF-GROUPING.                CL**5
03614                                                                   EL642
03615      EXEC CICS HANDLE CONDITION                                   EL642
03616          NOTFND  (6410-NOT-FOUND)                                 EL642
03617      END-EXEC.                                                       CL*27
03618                                                                   EL642
03619      EXEC CICS READ                                               EL642
03620          SET      (ADDRESS OF AGENT-CROSS-REFERENCE)                 CL*26
03621          DATASET  (ERGXRF-FILE-ID)                                   CL**5
03622          RIDFLD   (ERGXRF-KEY)                                       CL**5
03623          LENGTH   (ERGXRF-LENGTH)                                    CL**5
03624          UPDATE                                                   EL642
03625      END-EXEC.                                                       CL*27
03626                                                                   EL642
03627      MOVE GX-AGENT-POINTER-CNT   TO  GX-AGENT-POINTER-CNT.           CL**5
03628                                                                   EL642
03629      IF PI-UPDATE-FILES                                           EL642
03630          MOVE 'B'                TO  JP-RECORD-TYPE                  CL**5
03631          MOVE AGENT-CROSS-REFERENCE                                  CL**5
03632                                  TO  JP-RECORD-AREA                  CL**5
03633          MOVE ERGXRF-FILE-ID     TO  JP-FILE-ID                      CL**5
03634          COMPUTE JOURNAL-LENGTH = ERGXRF-LENGTH + 23                 CL**5
03635          PERFORM 8400-LOG-JOURNAL-RECORD.                            CL**5
03636                                                                   EL642
03637      GO TO 6490-EXIT.                                             EL642
03638                                                                   EL642
03639  6410-NOT-FOUND.                                                  EL642
03640      MOVE -1                     TO  AAGENTL.                        CL**5
03641      MOVE ER-2911                TO  EMI-ERROR.                      CL**5
03642                                                                      CL**5
03643      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**5
03644                                                                      CL**5
03645      GO TO 8200-SEND-DATAONLY.                                    EL642
03646                                                                   EL642
03647  6490-EXIT.                                                       EL642
03648      EXIT.                                                        EL642
03649  EJECT                                                               CL**5
03650  6500-REWRITE-CROSS-REFERENCE.                                    EL642
03651      MOVE 'C'                    TO  JP-RECORD-TYPE.              EL642
03652      MOVE AGENT-CROSS-REFERENCE  TO  JP-RECORD-AREA.              EL642
03653      MOVE ERGXRF-FILE-ID         TO  JP-FILE-ID.                  EL642
03654                                                                   EL642
03655      EXEC CICS REWRITE                                            EL642
03656          DATASET  (ERGXRF-FILE-ID)                                   CL**5
03657          FROM     (AGENT-CROSS-REFERENCE)                            CL**5
03658          LENGTH   (ERGXRF-LENGTH)                                    CL**5
03659      END-EXEC.                                                       CL*27
03660                                                                   EL642
03661      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL642
03662                                                                   EL642
03663  6590-EXIT.                                                       EL642
03664      EXIT.                                                        EL642
03665  EJECT                                                               CL**5
03666  6600-READ-COMMISSION-EXCEPTION.                                  EL642
03667      EXEC CICS HANDLE CONDITION                                   EL642
03668          NOTFND  (6610-NOT-FOUND)                                 EL642
03669      END-EXEC.                                                       CL*27
03670                                                                   EL642
03671      MOVE 'Y'                    TO ERCOMM-FOUND-SW.              EL642
03672      MOVE PB-CONTROL-BY-ACCOUNT  TO  ERCOMM-KEY.                  EL642
03673      MOVE PB-SV-CARRIER          TO  ERCOMM-CARRIER.              EL642
03674      MOVE PB-SV-GROUPING         TO  ERCOMM-GROUPING.             EL642
03675      MOVE PB-SV-STATE            TO  ERCOMM-STATE.                EL642
03676                                                                   EL642
03677      EXEC CICS READ                                               EL642
03678          SET      (ADDRESS OF COMMISSION-EXCEPTIONS)                 CL*26
03679          DATASET  (ERCOMM-FILE-ID)                                   CL**5
03680          RIDFLD   (ERCOMM-KEY)                                       CL**5
03681      END-EXEC.                                                       CL*27
03682                                                                   EL642
03683      GO TO 6620-EXIT.                                                CL*27
03684                                                                   EL642
03685  6610-NOT-FOUND.                                                  EL642
03686      MOVE 'N'                    TO  ERCOMM-FOUND-SW.                CL**5
03687                                                                   EL642
03688  6620-EXIT.                                                          CL*27
03689      EXIT.                                                        EL642
03690                                                                      CL*27
03691 **START***********************************************************   CL*27
03692 *      CUSTOM CODING FOR CLIENT "DMD"                                CL*27
03693 ******************************************************************   CL*27
03694 *                                                                    CL*27
03695  6650-GET-RES-STATE-COMM.                                            CL*27
03696 *6650-BROWSE-FORWARD.                                                CL*27
03697                                                                      CL*27
03698      IF PB-COMPANY-ID NOT = 'DMD'                                    CL*27
03699          GO TO 6700-COMM-EXIT.                                       CL*27
03700                                                                      CL*27
03701      MOVE PB-COMPANY-CD       TO WS-ERRESC-COMPANY-CD.               CL*27
03702      MOVE PB-SV-CARRIER       TO WS-ERRESC-CARRIER.                  CL*27
03703      MOVE PB-SV-GROUPING      TO WS-ERRESC-GROUP.                    CL*27
03704      MOVE PB-SV-STATE         TO WS-ERRESC-STATE.                    CL*27
03705      MOVE PB-ACCOUNT          TO WS-ERRESC-ACCOUNT.                  CL*27
03706      MOVE AM-AGT (WS-GA-LEVEL) TO WS-ERRESC-AGENT.                   CL*27
03707      MOVE PB-CERT-NO (1:2)    TO WS-ERRESC-RES-STATE.                CL*27
03708                                                                      CL*27
03709 *CONVERT PB-CERT-EFF-DT TO YYYYMMDD FORMATT TO READ MASTER           CL*27
03710 *                                                                    CL*27
03711      MOVE PB-CERT-EFF-DT      TO DC-BIN-DATE-1.                      CL*27
03712      SET BIN-TO-GREG          TO TRUE.                               CL*27
03713      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                       CL*27
03714                                                                      CL*27
03715      MOVE DC-GREG-DATE-1-YMD  TO WS-CONTRACT-DATE (3:6).             CL*27
03716                                                                      CL*27
03717      IF DC-GREG-DATE-1-YMD (1:2) IS LESS THAN '10'                   CL*27
03718          MOVE '20'            TO WS-CONTRACT-DATE (1:2)              CL*27
03719      ELSE                                                            CL*27
03720          MOVE '19'            TO WS-CONTRACT-DATE (1:2).             CL*27
03721                                                                      CL*27
03722      MOVE WS-CONTRACT-DATE    TO WS-ERRESC-EXPIRE-DT.                CL*27
03723                                                                      CL*27
03724      EXEC CICS HANDLE CONDITION                                      CL*27
03725          INVREQ    (6660-START-BROWSE)                               CL*28
03726          NOTFND    (6700-COMM-EXIT)                                  CL*27
03727      END-EXEC.                                                       CL*27
03728                                                                      CL*27
03729      EXEC CICS RESETBR                                               CL*27
03730          DATASET (ERRESC-FILE-ID)                                    CL*27
03731          RIDFLD  (WS-ERRESC-KEY)                                     CL*27
03732      END-EXEC.                                                       CL*27
03733                                                                      CL*27
03734  6660-START-BROWSE.                                                  CL*27
03735                                                                      CL*27
03736      EXEC CICS HANDLE CONDITION                                      CL*27
03737          NOTFND    (6700-COMM-EXIT)                                  CL*27
03738          ENDFILE   (6700-COMM-EXIT)                                  CL*27
03739      END-EXEC.                                                       CL*27
03740                                                                      CL*27
03741      EXEC CICS STARTBR                                               CL*27
03742          DATASET (ERRESC-FILE-ID)                                    CL*27
03743          RIDFLD  (WS-ERRESC-KEY)                                     CL*27
03744          GTEQ                                                        CL*27
03745      END-EXEC.                                                       CL*27
03746                                                                      CL*27
03747      MOVE WS-ERRESC-KEY           TO WS-SV-ERRESC-KEY.               CL*30
03748                                                                      CL*29
03749  6670-READ-NEXT-ERRESC-MASTER.                                       CL*27
03750                                                                      CL*27
03751      EXEC CICS HANDLE CONDITION                                      CL*27
03752          NOTFND    (6700-COMM-EXIT)                                  CL*27
03753          ENDFILE   (6700-COMM-EXIT)                                  CL*27
03754      END-EXEC.                                                       CL*27
03755                                                                      CL*27
03756      EXEC CICS READNEXT                                              CL*27
03757          SET      (ADDRESS OF ACCOUNT-RESIDENT-ST-COMMISSION)        CL*27
03758          DATASET (ERRESC-FILE-ID)                                    CL*27
03759          RIDFLD  (WS-ERRESC-KEY)                                     CL*30
03760      END-EXEC.                                                       CL*27
03761                                                                      CL*27
03762      IF ERRESC-RECORD-KEY (1:32) = WS-SV-ERRESC-KEY                  CL*30
03763         NEXT SENTENCE                                                CL*27
03764      ELSE                                                            CL*27
03765         EXEC CICS ENDBR                                              CL*27
03766              DATASET (ERRESC-FILE-ID)                                CL*27
03767         END-EXEC                                                     CL*27
03768         GO TO 6700-COMM-EXIT.                                        CL*27
03769                                                                      CL*27
03770 * CHECK IF CERTIFICATE EFFECTIVE DATE IS WITHIN ACCOUNT RESIDENT     CL*27
03771 * STATE COMMISSIONS RECORD'S EFFECT TIME PERIOD????                  CL*27
03772      IF WS-CONTRACT-DATE IS LESS THAN RESC-EFFECTIVE-DATE            CL*27
03773         EXEC CICS ENDBR                                              CL*27
03774              DATASET (ERRESC-FILE-ID)                                CL*27
03775         END-EXEC                                                     CL*27
03776         GO TO 6700-COMM-EXIT.                                        CL*27
03777                                                                      CL*27
03778 * SEARCH FOR MATCHING CATEGORY CODE ENTRY                            CL*27
03779 *                                                                    CL*27
03780 *    IF RESC-COMMISSION IS NUMERIC THAT IS THE COMMISSION TO OVER-   CL*27
03781 *    RIDE AH OR LIFE COMMISSION IN IN THE ACCOUNT MASTER.            CL*27
03782 *    IF RESC-COMMISSION NOT NUMERIC, USE VALUE TO GET COMMISSION     CL*31
03783 *    FROM COMMISSION TABLE IF RECORD EXISTS.                         CL*27
03784 *                                                                    CL*27
03785      MOVE ZEROS               TO WS-SUB WS-SUB1.                     CL*27
03786                                                                      CL*27
03787      PERFORM 6680-SEARCH-CATEGORY THRU 6680-EXIT.                    CL*27
03788                                                                      CL*27
03789      IF WS-SUB1 IS GREATER THAN ZERO                                 CL*27
03790          IF RESC-COMMISSION-TAB (WS-SUB) NUMERIC                     CL*31
03791              PERFORM 6680-SET-COMMISSION                             CL*31
03792          ELSE                                                        CL*27
03793              PERFORM 6680-SET-TABLE-CODE.                            CL*31
03794                                                                      CL*27
03795      EXEC CICS ENDBR                                                 CL*27
03796           DATASET (ERRESC-FILE-ID)                                   CL*27
03797      END-EXEC.                                                       CL*27
03798                                                                      CL*27
03799      GO TO 6700-COMM-EXIT.                                           CL*27
03800                                                                      CL*27
03801  6680-SET-TABLE-CODE.                                                CL*27
03802      IF RESC-COMMISSION-TAB (WS-SUB) EQUAL SPACES                    CL*27
03803         MOVE ZEROS             TO WS-SUB1                            CL*27
03804      ELSE                                                            CL*27
03805                                                                      CL*27
03806 * READ COMMISSION TABLE(CTBL)  KEY VALUES PRESET IN CALLING          CL*27
03807 * PARAGRAPHS(4405-FIND-AH, 4420-CONT-FIND-LIFE, 4430-SINGLE-LIFE)    CL*27
03808                                                                      CL*27
03809      MOVE RESC-COMMISSION-TAB (WS-SUB) TO ERCTBL-TABLE               CL*27
03810      PERFORM 6680-READ-CTBL-TABLE.                                   CL*27
03811                                                                      CL*27
03812  6680-READ-CTBL-TABLE.                                               CL*27
03813 *                                                                    CL*27
03814 * READ COMMISSION TABLE TO VERIFY IF ACCOUNT RESIDENT STATE          CL*27
03815 * COMMISSION RECORD CONTAINS A VALID COMPENSATION TABLE CODE.        CL*27
03816 * IF CODE IS NOT VALID USE CODE IN ACCOUNT MASTER                    CL*27
03817                                                                      CL*27
03818      PERFORM 6700-READ-COMMISSION-TABLE THRU 6790-EXIT.              CL*27
03819                                                                      CL*27
03820      IF ERCTBL-NOT-FOUND                                             CL*27
03821          MOVE 'AA'               TO  ERCTBL-BEN-CODE                 CL*31
03822          PERFORM 6700-READ-COMMISSION-TABLE  THRU  6790-EXIT         CL*27
03823          IF ERCTBL-NOT-FOUND                                         CL*27
03824              MOVE ZEROS          TO  WS-SUB1.                        CL*27
03825                                                                      CL*27
03826      IF ERCTBL-FOUND                                                 CL*27
03827         IF AH-COMM                                                   CL*27
03828             PERFORM 6680-AH-GET-COMP-RATE                            CL*27
03829         ELSE                                                         CL*27
03830             PERFORM 6680-LF-GET-COMP-RATE.                           CL*27
03831                                                                      CL*27
03832  6680-AH-GET-COMP-RATE.                                              CL*27
03833      IF PB-ISSUE                                                     CL*27
03834          COMPUTE WS-COMM-CK-AMT =                                    CL*27
03835              PB-I-AH-BENEFIT-AMT * PB-I-AH-TERM                      CL*27
03836          MOVE PB-I-AGE           TO  WS-COMM-AGE                     CL*27
03837          MOVE PB-I-AH-TERM       TO  WS-COMM-TERM                    CL*27
03838      ELSE                                                            CL*27
03839          COMPUTE WS-COMM-CK-AMT =                                    CL*27
03840              PB-CI-AH-BENEFIT-AMT * PB-CI-AH-TERM                    CL*27
03841          MOVE PB-CI-INSURED-AGE  TO  WS-COMM-AGE                     CL*27
03842          MOVE PB-CI-AH-TERM      TO  WS-COMM-TERM.                   CL*27
03843                                                                      CL*27
03844      PERFORM 4900-GET-COMP-RATE  THRU  4990-EXIT.                    CL*27
03845                                                                      CL*27
03846      MOVE WS-WK-RATE             TO  WS-GA-AH-COM.                   CL*27
03847                                                                      CL*27
03848  6680-LF-GET-COMP-RATE.                                              CL*27
03849      IF PB-ISSUE                                                     CL*27
03850          MOVE PB-I-LF-BENEFIT-AMT                                    CL*27
03851                                  TO  WS-COMM-CK-AMT                  CL*27
03852          MOVE PB-I-AGE           TO  WS-COMM-AGE                     CL*27
03853          MOVE PB-I-LF-TERM       TO  WS-COMM-TERM                    CL*27
03854      ELSE                                                            CL*27
03855          MOVE PB-CI-INSURED-AGE  TO  WS-COMM-AGE                     CL*27
03856          MOVE PB-CI-LF-TERM      TO  WS-COMM-TERM                    CL*27
03857          MOVE PB-CI-LF-BENEFIT-AMT                                   CL*27
03858                                  TO  WS-COMM-CK-AMT.                 CL*27
03859                                                                      CL*27
03860      PERFORM 4900-GET-COMP-RATE  THRU  4990-EXIT.                    CL*27
03861                                                                      CL*27
03862      MOVE WS-WK-RATE             TO  WS-GA-LF-COM.                   CL*27
03863                                                                      CL*27
03864  6680-SET-COMMISSION.                                                CL*27
03865      IF RESC-COMMISSION-PER (WS-SUB) EQUAL ZEROS                     CL*27
03866            MOVE ZEROS          TO WS-SUB1                            CL*27
03867        ELSE                                                          CL*27
03868            MOVE RESC-COMMISSION-PER (WS-SUB)                         CL*27
03869                               TO WS-COMMISSION                       CL*27
03870            IF AH-COMM                                                CL*27
03871               MOVE WS-COMMISSION TO WS-GA-AH-COM                     CL*27
03872            ELSE                                                      CL*27
03873               MOVE WS-COMMISSION TO WS-GA-LF-COM.                    CL*27
03874                                                                      CL*27
03875  6680-SEARCH-CATEGORY.                                               CL*27
03876      ADD +1 TO WS-SUB.                                               CL*27
03877      IF WS-SUB IS GREATER THAN +12                                   CL*27
03878          GO TO 6680-EXIT.                                            CL*27
03879                                                                      CL*27
03880      IF PB-CERT-NO (4:1) = RESC-COVERAGE-CAT (WS-SUB)                CL*27
03881           MOVE WS-SUB         TO WS-SUB1                             CL*27
03882           GO TO 6680-EXIT.                                           CL*27
03883                                                                      CL*27
03884      GO TO 6680-SEARCH-CATEGORY.                                     CL*27
03885                                                                      CL*27
03886  6680-EXIT.                                                          CL*27
03887      EXIT.                                                           CL*27
03888                                                                      CL*27
03889  6700-COMM-EXIT.                                                     CL*27
03890      EXIT.                                                           CL*27
03891 **END*************************************************************   CL*27
03892 *      CUSTOM CODING FOR CLIENT "DMD"                                CL*27
03893 ******************************************************************   CL*27
03894 *                                                                    CL*27
03895  EJECT                                                               CL**5
03896  6700-READ-COMMISSION-TABLE.                                      EL642
03897      EXEC CICS HANDLE CONDITION                                   EL642
03898          NOTFND  (6710-NOT-FOUND)                                 EL642
03899      END-EXEC.                                                       CL*27
03900                                                                   EL642
03901      MOVE 'Y'                    TO  ERCTBL-FOUND-SW.             EL642
03902                                                                   EL642
03903      EXEC CICS READ                                               EL642
03904          SET      (ADDRESS OF COMM-TABLE-RECORD)                     CL*26
03905          DATASET  (ERCTBL-FILE-ID)                                   CL**5
03906          RIDFLD   (ERCTBL-KEY)                                       CL**5
03907      END-EXEC.                                                       CL*27
03908                                                                      CL**5
03909      GO TO 6790-EXIT.                                             EL642
03910                                                                   EL642
03911  6710-NOT-FOUND.                                                  EL642
03912      MOVE 'N'                    TO  ERCTBL-FOUND-SW.                CL**5
03913                                                                   EL642
03914  6790-EXIT.                                                       EL642
03915      EXIT.                                                        EL642
03916  EJECT                                                               CL**5
03917  6800-READ-BENEFIT-MASTER.                                        EL642
03918      EXEC CICS HANDLE CONDITION                                   EL642
03919          NOTFND  (6880-NOT-FOUND)                                 EL642
03920      END-EXEC.                                                       CL*27
03921                                                                   EL642
03922      MOVE 'Y'                    TO  BENEFIT-MASTER-FOUND-SW.        CL**5
03923      MOVE PB-COMPANY-ID          TO  CLBENF-COMPANY-ID.              CL**5
03924      MOVE '4'                    TO  CLBENF-REC-TYPE.                CL**5
03925      MOVE +0                     TO  CLBENF-SEQ-NO.                  CL**5
03926                                                                   EL642
03927      EXEC CICS READ                                               EL642
03928          SET      (ADDRESS OF CONTROL-FILE)                          CL*26
03929          DATASET  (ELCNTL-FILE-ID)                                   CL**5
03930          RIDFLD   (ELCNTL-BENEFIT-KEY)                               CL**5
03931          GTEQ                                                     EL642
03932      END-EXEC.                                                       CL*27
03933                                                                   EL642
03934      IF CLBENF-COMPANY-ID  NOT =  CF-COMPANY-ID                      CL**5
03935          GO TO 6880-NOT-FOUND.                                    EL642
03936                                                                   EL642
03937      IF CLBENF-REC-TYPE  NOT =  CF-RECORD-TYPE                       CL**5
03938          GO TO 6880-NOT-FOUND.                                    EL642
03939                                                                   EL642
03940      MOVE +0                     TO  BEN-SUB.                        CL**5
03941                                                                   EL642
03942  6810-FIND-BENEFIT.                                               EL642
03943      ADD +1                      TO  BEN-SUB.                        CL**5
03944                                                                      CL**5
03945      IF BEN-SUB  GREATER THAN  +8                                    CL**5
03946          GO TO 6880-NOT-FOUND.                                    EL642
03947                                                                   EL642
03948      IF CF-BENEFIT-CODE (BEN-SUB)  =  CLBENF-CD                      CL**7
03949          MOVE CF-JOINT-INDICATOR (BEN-SUB)                           CL**5
03950                                  TO  LIFE-INDICATOR                  CL**5
03951          GO TO 6890-EXIT.                                         EL642
03952                                                                   EL642
03953      GO TO 6810-FIND-BENEFIT.                                     EL642
03954                                                                   EL642
03955  6880-NOT-FOUND.                                                  EL642
03956      MOVE 'N'                    TO  BENEFIT-MASTER-FOUND-SW.     EL642
03957                                                                   EL642
03958  6890-EXIT.                                                       EL642
03959      EXIT.                                                        EL642
03960  EJECT                                                               CL**5
03961  7000-PROCESS-CHECK.                                              EL642
03962      MOVE LOW-VALUES             TO  EL642BO.                        CL**5
03963                                                                   EL642
03964      MOVE 'G'                    TO  COMPENSATION-SW                 CL**5
03965                                      PI-CR-TYPE.                     CL**5
03966                                                                   EL642
03967      PERFORM 6000-READ-COMP-MASTER  THRU  6090-EXIT.                 CL**5
03968                                                                   EL642
03969      IF EMI-ERROR = ZEROS                                         EL642
03970          MOVE CO-MAIL-NAME       TO  BNAMEO                          CL**5
03971          MOVE CO-ADDR-1          TO  BADDR1O                         CL**5
03972          MOVE CO-ADDR-2          TO  BADDR2O                         CL**5
03973          MOVE CO-ADDR-3          TO  BCITYSTO                        CL**5
03974          MOVE CO-ZIP             TO  BZIPO                           CL**5
03975      ELSE                                                         EL642
03976          MOVE EL642A             TO  PI-MAP-NAME                     CL*23
03977          MOVE -1                 TO  BPFNTERL                        CL**5
03978          GO TO 8200-SEND-DATAONLY.                                   CL**5
03979                                                                   EL642
03980      IF PI-COMP-UNPAID-PREM  GREATER THAN  ZERO                      CL**5
03981          MOVE PI-COMP-UNPAID-PREM                                    CL**5
03982                                  TO BCHKAMTO                         CL**5
03983      ELSE                                                            CL**5
03984          MOVE ZEROS              TO  BCHKAMTO.                       CL**5
03985                                                                   EL642
03986      MOVE AL-UNNON               TO  BCHKAMTA.                       CL**5
03987      MOVE WS-CURRENT-DATE-EDIT   TO  BPAYDT1O.                       CL**5
03988      MOVE -1                     TO  BCHKNOL.                        CL**5
03989                                                                   EL642
03990      GO TO 8100-SEND-INITIAL-MAP.                                 EL642
03991  EJECT                                                               CL**5
03992  7500-PRODUCE-CHECK.                                              EL642
03993      MOVE SPACES                 TO  ELCNTL-KEY.                     CL**5
03994      MOVE '1'                    TO  ELCNTL-REC-TYPE.                CL**5
03995      MOVE 'Y'                    TO  ELCNTL-UPDATE-SW.               CL**5
03996                                                                   EL642
03997      PERFORM 6100-READ-CONTROL-FILE  THRU  6190-EXIT.                CL**5
03998                                                                      CL**5
03999      IF EMI-ERROR  NOT =  ZEROS                                      CL**5
04000          GO TO 8200-SEND-DATAONLY.                                EL642
04001                                                                   EL642
04002      IF CR-CHECK-NO-AUTO-SEQ                                      EL642
04003          MOVE 'B'                TO  JP-RECORD-TYPE                  CL**5
04004          MOVE CONTROL-FILE       TO  JP-RECORD-AREA                  CL**5
04005          MOVE ELCNTL-FILE-ID     TO  JP-FILE-ID                      CL**5
04006          COMPUTE JOURNAL-LENGTH = ELCNTL-LENGTH + 23              EL642
04007          PERFORM 8400-LOG-JOURNAL-RECORD                          EL642
04008          IF CR-CHECK-CNT-RESET-VALUE                              EL642
04009              MOVE CF-CR-CHECK-COUNTER                                CL**5
04010                                  TO  BCHKNOI                         CL**5
04011              MOVE +1             TO  CF-CR-CHECK-COUNTER             CL**5
04012              MOVE 6              TO  BCHKNOL                         CL**5
04013          ELSE                                                     EL642
04014              MOVE CF-CR-CHECK-COUNTER                                CL**5
04015                                  TO  BCHKNOI                         CL**5
04016              MOVE 6              TO  BCHKNOL                         CL**5
04017              ADD +1              TO  CF-CR-CHECK-COUNTER.            CL**5
04018                                                                   EL642
04019      IF CR-CHECK-NO-MANUAL                                        EL642
04020          IF BCHKNOL = ZEROS                                       EL642
04021              MOVE ER-2438        TO  EMI-ERROR                       CL**5
04022              MOVE -1             TO  BCHKNOL                         CL**5
04023              MOVE AL-UNBON       TO  BCHKNOA                         CL**5
04024              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**5
04025              GO TO 8200-SEND-DATAONLY                             EL642
04026          ELSE                                                     EL642
04027              IF BCHKNOI NOT NUMERIC                               EL642
04028                  MOVE ER-2439    TO  EMI-ERROR                       CL**5
04029                  MOVE -1         TO  BCHKNOL                         CL**5
04030                  MOVE AL-UNBON   TO  BCHKNOA                         CL**5
04031                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL**5
04032                  GO TO 8200-SEND-DATAONLY.                        EL642
04033                                                                   EL642
04034      EXEC CICS GETMAIN                                            EL642
04035          SET      (ADDRESS OF PENDING-PAY-ADJ)                       CL*26
04036          LENGTH   (ERPYAJ-LENGTH)                                    CL**8
04037          INITIMG  (GETMAIN-SPACE)                                    CL**5
04038      END-EXEC.                                                       CL*27
04039                                                                   EL642
04040      EXEC CICS BIF DEEDIT                                         EL642
04041          FIELD   (BCHKAMTI)                                          CL**5
04042          LENGTH  (09)                                                CL**5
04043      END-EXEC.                                                       CL*27
04044                                                                   EL642
04045      IF BCHKAMTI NOT GREATER THAN ZEROS                              CL*23
04046          MOVE ER-3165    TO  EMI-ERROR                               CL*23
04047          MOVE -1         TO  BCHKAMTL                                CL*23
04048          MOVE AL-UNBON   TO  BCHKAMTA                                CL*23
04049          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL*23
04050          GO TO 8200-SEND-DATAONLY.                                   CL*23
04051                                                                      CL*23
04052      MOVE 'Y'                    TO  PI-CHECK-SW.                    CL**5
04053      MOVE 'PY'                   TO  PY-RECORD-ID.                   CL**5
04054      MOVE PI-COMPANY-CD          TO  PY-COMPANY-CD.                  CL**5
04055      MOVE PI-SAV-CARR            TO  PY-CARRIER.                     CL**5
04056      MOVE PI-SAV-GROUP           TO  PY-GROUPING.                    CL**5
04057      MOVE PI-SAV-AGENT           TO  PY-FIN-RESP.                    CL**5
04058      MOVE LOW-VALUES             TO  PY-ACCOUNT.                     CL**5
04059      MOVE 'C'                    TO  PY-RECORD-TYPE.                 CL**5
04060      MOVE EIBTIME                TO  PY-FILE-SEQ-NO.                 CL**5
04061      MOVE WS-CURRENT-DATE-MDY    TO  WS-PY-CURRENT-DATE.             CL**5
04062      MOVE WS-PY-ENTRY-COMMENT    TO  PY-ENTRY-COMMENT.               CL**5
04063      MOVE BCHKAMTI               TO  PY-ENTRY-AMT.                   CL**5
04064                                                                   EL642
04065      ADD BCHKAMTI                TO  PI-DISBURSED.                   CL**5
04066                                                                   EL642
04067      IF NOT  CR-CHECK-NO-AT-PRINT                                    CL**5
04068          MOVE BCHKNOI            TO  PY-CHECK-NUMBER.                CL**5
04069                                                                   EL642
04070      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.               CL**5
04071      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.           CL**5
04072      MOVE WS-CURRENT-DATE        TO  PY-LAST-MAINT-DT                CL**5
04073                                      PY-INPUT-DT.                    CL**5
04074      MOVE LOW-VALUES             TO  PY-BILLED-DATE                  CL**9
04075                                      PY-AR-DATE.                     CL**9
04076      MOVE ZEROS                  TO  PY-CHECK-QUE-CONTROL            CL**5
04077                                      PY-CHECK-QUE-SEQUENCE.          CL**5
04078      MOVE LOW-VALUES             TO  PY-CREDIT-ACCEPT-DT             CL**5
04079                                      PY-REPORTED-DT                  CL**5
04080                                      PY-CHECK-WRITTEN-DT.            CL**5
04081      MOVE PI-CR-MONTH-END-DT     TO  PY-CREDIT-SELECT-DT.            CL**5
04082      MOVE 'G'                    TO  PY-CHECK-ORIGIN-SW.             CL**5
04083      MOVE 'A'                    TO  JP-RECORD-TYPE.                 CL**5
04084      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.                 CL**5
04085      MOVE ERPYAJ-FILE-ID         TO  JP-FILE-ID.                     CL**5
04086                                                                      CL**5
04087      COMPUTE JOURNAL-LENGTH = ERPYAJ-LENGTH + 23.                 EL642
04088                                                                      CL**5
04089      EXEC CICS WRITE                                              EL642
04090          DATASET  (ERPYAJ-FILE-ID)                                   CL**5
04091          FROM     (PENDING-PAY-ADJ)                                  CL**5
04092          RIDFLD   (PY-CONTROL-PRIMARY)                               CL**5
04093      END-EXEC.                                                       CL*27
04094                                                                      CL**5
04095      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL642
04096                                                                   EL642
04097      IF CR-CHECK-NO-AUTO-SEQ                                      EL642
04098          PERFORM 6200-REWRITE-CONTROL-FILE  THRU  6290-EXIT          CL**5
04099      ELSE                                                         EL642
04100          EXEC CICS UNLOCK                                         EL642
04101              DATASET  (ELCNTL-FILE-ID)                               CL**5
04102          END-EXEC.                                                   CL*27
04103                                                                   EL642
04104      MOVE EL642A                 TO  PI-MAP-NAME.                    CL**5
04105                                                                      CL**5
04106      PERFORM 5000-FORMAT-SCREEN  THRU  5090-EXIT.                    CL**5
04107  EJECT                                                               CL**5
04108  8100-SEND-INITIAL-MAP.                                           EL642
04109                                                                      CL*23
04110      MOVE SPACES TO PI-TRANSFER-SW.                                  CL*23
04111                                                                      CL*23
04112      IF PI-MAP-NAME  =  EL642A                                       CL**5
04113          NEXT SENTENCE                                            EL642
04114      ELSE                                                         EL642
04115          GO TO 8110-SEND-INITIAL-CHECK-MAP.                       EL642
04116                                                                   EL642
04117      MOVE WS-CURRENT-DATE-EDIT   TO  ADATEO.                         CL**5
04118      MOVE EIBTIME                TO  TIME-IN.                        CL**5
04119      MOVE TIME-OUT               TO  ATIMEO.                         CL**5
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
04120      MOVE -1                     TO  AAGENTL.                        CL**5
04121      MOVE EMI-MESSAGE-AREA (1)   TO  AERMSG1O.                       CL**5
04122      MOVE EMI-MESSAGE-AREA (2)   TO  AERMSG2O.                       CL**5
04123                                                                   EL642
04124      IF PI-ZERO-CARRIER                                           EL642
04125          MOVE AL-SADOF           TO  ACARHDGA                        CL**5
04126                                      ACARIERA.                       CL**5
04127                                                                   EL642
04128      IF PI-ZERO-GROUPING                                          EL642
04129          MOVE AL-SADOF           TO  AGRPHDGA                        CL**5
04130                                      AGROUPA.                        CL**5
04131                                                                   EL642
04132      IF PI-ZERO-CAR-GROUP                                         EL642
04133          MOVE AL-SADOF           TO  ACARHDGA                        CL**5
04134                                      ACARIERA                        CL**5
04135          MOVE AL-SADOF           TO  AGRPHDGA                        CL**5
04136                                      AGROUPA.                        CL**5
04137                                                                      CL*23
04138      IF (EIBTRNID = EL633-TRANS-ID OR EL635-TRANS-ID OR              CL*23
04139                     EL633DMD-TRANS-ID OR                             CL*26
04140                     EL650-TRANS-ID OR EL652-TRANS-ID OR              CL*23
04141                     EL658-TRANS-ID)    AND                           CL*23
04142         (WT-CR-FIN-RESP NOT EQUAL SPACES AND LOW-VALUES)             CL*31
04143          MOVE -1                    TO  ABILTYPL                     CL*23
04144          MOVE WT-CR-STATE           TO  PI-SCR-STATE                 CL*23
04145          MOVE WT-CR-ACCOUNT         TO  PI-SCR-ACCOUNT               CL*23
04146          MOVE WT-CR-TYPE            TO  PI-SCR-TYPE                  CL*23
04147          IF PI-ZERO-CARRIER                                          CL*23
04148              MOVE WT-CR-FIN-RESP    TO  AAGENTI                      CL*23
04149                                         PI-SCR-FIN-RESP              CL*23
04150              MOVE WT-CR-GROUPING    TO  AGROUPI                      CL*23
04151                                         PI-SCR-GROUPING              CL*23
04152              MOVE AL-UANON          TO  AAGENTA                      CL*23
04153                                         AGROUPA                      CL*23
04154              MOVE 10                TO  AAGENTL                      CL*23
04155              MOVE 6                 TO  AGROUPL                      CL*23
04156          ELSE                                                        CL*23
04157             IF PI-ZERO-GROUPING                                      CL*23
04158                 MOVE WT-CR-CARRIER    TO  ACARIERI                   CL*23
04159                                           PI-SCR-CARRIER             CL*23
04160                 MOVE WT-CR-FIN-RESP   TO  AAGENTI                    CL*23
04161                                           PI-SCR-FIN-RESP            CL*23
04162                 MOVE AL-UANON         TO  ACARIERA                   CL*23
04163                                           AAGENTA                    CL*23
04164                 MOVE 10               TO  AAGENTL                    CL*23
04165                 MOVE 1                TO  ACARIERL                   CL*23
04166             ELSE                                                     CL*23
04167                IF PI-ZERO-CAR-GROUP                                  CL*23
04168                    MOVE WT-CR-FIN-RESP TO  AAGENTI                   CL*23
04169                                            PI-SCR-FIN-RESP           CL*23
04170                    MOVE AL-UANON       TO  AAGENTA                   CL*23
04171                    MOVE 10             TO  AAGENTL                   CL*23
04172                ELSE                                                  CL*23
04173                    MOVE WT-CR-CARRIER     TO  ACARIERI               CL*23
04174                                               PI-SCR-CARRIER         CL*23
04175                    MOVE WT-CR-FIN-RESP    TO  AAGENTI                CL*23
04176                                               PI-SCR-FIN-RESP        CL*23
04177                    MOVE WT-CR-GROUPING    TO  AGROUPI                CL*23
04178                                               PI-SCR-GROUPING        CL*23
04179                    MOVE AL-UANON          TO  AAGENTA                CL*23
04180                                               AGROUPA                CL*23
04181                                               ACARIERA               CL*23
04182                    MOVE 10                TO  AAGENTL                CL*23
04183                    MOVE 6                 TO  AGROUPL                CL*23
04184                    MOVE 1                 TO  ACARIERL.              CL*23
04185                                                                   EL642
04186      EXEC CICS SEND                                               EL642
04187          MAP     (PI-MAP-NAME)                                       CL**5
04188          MAPSET  (MAPSET-NAME)                                       CL**5
04189          FROM    (EL642AO)                                           CL**5
04190          ERASE                                                    EL642
04191          CURSOR                                                   EL642
04192      END-EXEC.                                                       CL*27
04193                                                                   EL642
04194      GO TO 9100-RETURN-TRAN.                                      EL642
04195  EJECT                                                               CL**5
04196  8110-SEND-INITIAL-CHECK-MAP.                                     EL642
04197      MOVE WS-CURRENT-DATE-EDIT   TO  BDATEO.                         CL**5
04198      MOVE EIBTIME                TO  TIME-IN.                        CL**5
04199      MOVE TIME-OUT               TO  BTIMEO.                         CL**5
101101     MOVE PI-COMPANY-ID          TO  BCMPNYO.
101101     MOVE PI-PROCESSOR-ID        TO  BUSERIDO.
04200      MOVE -1                     TO  BPFNTERL.                       CL**5
04201      MOVE EMI-MESSAGE-AREA (1)   TO  BERMSGO.                        CL**5
04202                                                                   EL642
04203      EXEC CICS SEND                                               EL642
04204          MAP     (PI-MAP-NAME)                                       CL**5
04205          MAPSET  (MAPSET-NAME)                                       CL**5
04206          FROM    (EL642BO)                                           CL**5
04207          ERASE                                                    EL642
04208          CURSOR                                                   EL642
04209      END-EXEC.                                                       CL*27
04210                                                                   EL642
04211      GO TO 9100-RETURN-TRAN.                                      EL642
04212  EJECT                                                               CL**5
04213  8200-SEND-DATAONLY.                                              EL642
04214      IF PI-MAP-NAME  =  EL642A                                       CL**5
04215          MOVE WS-CURRENT-DATE-EDIT  TO  ADATEO                       CL**5
04216          MOVE EIBTIME               TO  TIME-IN                      CL**5
04217          MOVE TIME-OUT              TO  ATIMEO                       CL**5
101101         MOVE PI-COMPANY-ID         TO  CMPNYIDO
101101         MOVE PI-PROCESSOR-ID       TO  USERIDO
04218          MOVE EMI-MESSAGE-AREA (1)  TO  AERMSG1O                     CL**5
04219          MOVE EMI-MESSAGE-AREA (2)  TO  AERMSG2O                     CL**5
04220          EXEC CICS SEND                                           EL642
04221              MAP     (PI-MAP-NAME)                                   CL**5
04222              MAPSET  (MAPSET-NAME)                                   CL**5
04223              FROM    (EL642AO)                                       CL**5
04224              DATAONLY                                             EL642
04225              ERASEAUP                                             EL642
04226              CURSOR                                               EL642
04227          END-EXEC                                                    CL*27
04228      ELSE                                                         EL642
04229          MOVE WS-CURRENT-DATE-EDIT  TO  BDATEO                       CL**5
04230          MOVE EIBTIME               TO  TIME-IN                      CL**5
04231          MOVE TIME-OUT              TO  BTIMEO                       CL**5
101101         MOVE PI-COMPANY-ID         TO  BCMPNYO
101101         MOVE PI-PROCESSOR-ID       TO  BUSERIDO
04232          MOVE EMI-MESSAGE-AREA (1)  TO  BERMSGO                      CL**5
04233          EXEC CICS SEND                                           EL642
04234              MAP     (PI-MAP-NAME)                                   CL**5
04235              MAPSET  (MAPSET-NAME)                                   CL**5
04236              FROM    (EL642BO)                                       CL**5
04237              DATAONLY                                             EL642
04238              ERASEAUP                                             EL642
04239              CURSOR                                               EL642
04240          END-EXEC.                                                   CL*27
04241                                                                      CL**5
04242      GO TO 9100-RETURN-TRAN.                                      EL642
04243  EJECT                                                               CL**5
04244  8300-SEND-TEXT.                                                  EL642
04245      EXEC CICS SEND TEXT                                          EL642
04246          FROM    (LOGOFF-TEXT)                                       CL**5
04247          LENGTH  (LOGOFF-LENGTH)                                     CL**5
04248          ERASE                                                    EL642
04249          FREEKB                                                   EL642
04250      END-EXEC.                                                       CL*27
04251                                                                      CL**5
04252      EXEC CICS RETURN                                             EL642
04253      END-EXEC.                                                       CL*27
04254                                                                   EL642
04255  8400-LOG-JOURNAL-RECORD.                                         EL642
04256      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                     CL**5
04257      MOVE THIS-PGM               TO  JP-PROGRAM-ID.                  CL**5
04258                                                                   EL642
04259 *    EXEC CICS JOURNAL                                            EL642
04260 *        JFILEID  (PI-JOURNAL-FILE-ID)                               CL**5
04261 *        JTYPEID  ('EL')                                             CL**5
04262 *        FROM     (JOURNAL-RECORD)                                   CL**5
04263 *        LENGTH   (JOURNAL-LENGTH)                                   CL**5
04264 *    END-EXEC.                                                       CL*27
04265                                                                   EL642
04266  8500-DATE-CONVERT.                                               EL642
04267      EXEC CICS LINK                                               EL642
04268          PROGRAM   (LINK-ELDATCV)                                    CL**5
04269          COMMAREA  (DATE-CONVERSION-DATA)                            CL**5
04270          LENGTH    (DC-COMM-LENGTH) END-EXEC.                        CL**5
04271                                                                      CL**5
04272  8500-EXIT.                                                       EL642
04273      EXIT.                                                        EL642
04274  EJECT                                                               CL**5
04275  8600-CENTER-DATA.                                                EL642
04276      MOVE SPACE                  TO  CENTER-WORK-2.                  CL**5
04277                                                                   EL642
04278      IF CW1-PIC (1)  NOT =  SPACE                                    CL**5
04279        AND  CW1-PIC (X-LEN)  NOT =  SPACE                            CL**5
04280          MOVE CENTER-WORK-1      TO  CENTER-WORK-2                   CL**5
04281          GO TO 8690-C-D-X.                                           CL**5
04282                                                                   EL642
04283      MOVE +0                     TO  X1.                             CL**5
04284      MOVE X-LEN                  TO  X2.                             CL**5
04285                                                                   EL642
04286  8660-C-D-LOOP-1.                                                 EL642
04287      IF CW1-PIC (X2)  =  SPACE                                       CL**5
04288          ADD +1                  TO  X1                              CL**5
04289          SUBTRACT +1             FROM  X2                            CL**5
04290          GO TO 8660-C-D-LOOP-1.                                      CL**5
04291                                                                   EL642
04292      MOVE +1                     TO  X2.                             CL**5
04293                                                                   EL642
04294  8670-C-D-LOOP-2.                                                 EL642
04295      IF CW1-PIC (X2)  =  SPACE                                       CL**5
04296          ADD +1                  TO  X1  X2                          CL**5
04297          GO TO 8670-C-D-LOOP-2.                                      CL**5
04298                                                                   EL642
04299      COMPUTE X3 = (X1 / +2) + +1.                                 EL642
04300      COMPUTE X1 = X-LEN - X1.                                     EL642
04301                                                                   EL642
04302  8680-C-D-LOOP-3.                                                 EL642
04303      IF X1  NOT =  +0                                                CL**5
04304          MOVE CW1-PIC (X2)       TO  CW2-PIC (X3)                    CL**5
04305          ADD +1                  TO  X2  X3                          CL**5
04306          SUBTRACT +1             FROM  X1                            CL**5
04307          GO TO 8680-C-D-LOOP-3.                                      CL**5
04308                                                                   EL642
04309  8690-C-D-X.                                                      EL642
04310      EXIT.                                                        EL642
04311  EJECT                                                               CL**5
04312  8800-UNAUTHORIZED-ACCESS.                                        EL642
04313      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                     CL**5
04314                                                                      CL**5
04315      GO TO 8300-SEND-TEXT.                                        EL642
04316                                                                   EL642
04317  8810-PF23.                                                       EL642
04318      MOVE EIBAID                 TO  PI-ENTRY-CD-1.                  CL**5
04319      MOVE XCTL-005               TO  PGM-NAME.                       CL**5
04320                                                                      CL**5
04321      GO TO 9300-XCTL.                                             EL642
04322                                                                   EL642
04323  8900-SYNCPOINT-ROLLBACK.                                         EL642
04324      EXEC CICS SYNCPOINT                                          EL642
04325          ROLLBACK                                                    CL**5
04326      END-EXEC.                                                       CL*27
04327                                                                      CL**5
04328      EXEC CICS DUMP                                                  CL**5
04329          DUMPCODE  ('LGXX')                                          CL**5
04330          TASK                                                        CL**5
04331      END-EXEC.                                                       CL*27
04332                                                                   EL642
04333  8900-EXIT.                                                       EL642
04334      EXIT.                                                        EL642
04335                                                                   EL642
04336  9100-RETURN-TRAN.                                                EL642
04337      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.               CL**5
04338      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.           CL**5
04339                                                                      CL**5
04340      EXEC CICS RETURN                                             EL642
04341          TRANSID   (TRANS-ID)                                        CL**5
04342          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                         CL**5
04343          LENGTH    (PI-COMM-LENGTH)                                  CL**5
04344      END-EXEC.                                                       CL*27
04345                                                                   EL642
04346  9200-RETURN-MAIN-MENU.                                           EL642
04347      MOVE XCTL-626               TO  PGM-NAME.                       CL**5
04348                                                                      CL**5
04349      GO TO 9300-XCTL.                                             EL642
04350                                                                   EL642
04351  9300-XCTL.                                                       EL642
04352      EXEC CICS XCTL                                               EL642
04353          PROGRAM   (PGM-NAME)                                        CL**5
04354          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                         CL**5
04355          LENGTH    (PI-COMM-LENGTH)                                  CL**5
04356      END-EXEC.                                                       CL*27
04357                                                                   EL642
04358  9400-CLEAR.                                                      EL642
04359      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME                        CL**5
04360                                                                      CL**5
04361      GO TO 9300-XCTL.                                             EL642
04362                                                                   EL642
04363  9500-PF12.                                                       EL642
04364      MOVE XCTL-010               TO  PGM-NAME.                       CL**5
04365                                                                      CL**5
04366      GO TO 9300-XCTL.                                             EL642
04367  EJECT                                                               CL**5
04368  9600-PGMID-ERROR.                                                EL642
04369      EXEC CICS HANDLE CONDITION                                   EL642
04370          PGMIDERR  (8300-SEND-TEXT)                                  CL**5
04371      END-EXEC.                                                       CL*27
04372                                                                      CL**5
04373      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.             CL**5
04374      MOVE ' '                    TO  PI-ENTRY-CD-1.                  CL**5
04375      MOVE XCTL-005               TO  PGM-NAME.                       CL**5
04376      MOVE PGM-NAME               TO  LOGOFF-PGM.                     CL**5
04377      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                    CL**5
04378                                                                      CL**5
04379      GO TO 9300-XCTL.                                             EL642
04380                                                                   EL642
04381  9900-ERROR-FORMAT.                                               EL642
04382      IF NOT  EMI-ERRORS-COMPLETE                                     CL**5
04383          MOVE LINK-001           TO  PGM-NAME                        CL**5
04384          EXEC CICS LINK                                           EL642
04385              PROGRAM   (PGM-NAME)                                    CL**5
04386              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)               CL**5
04387              LENGTH    (EMI-COMM-LENGTH)                             CL**5
04388          END-EXEC.                                                   CL*27
04389                                                                      CL**5
04390  9900-EXIT.                                                       EL642
04391      EXIT.                                                        EL642
04392                                                                   EL642
04393  9990-ABEND.                                                      EL642
04394      MOVE LINK-004               TO  PGM-NAME.                       CL**5
04395      MOVE DFHEIBLK               TO  EMI-LINE1                       CL**5
04396                                                                      CL**5
04397      EXEC CICS LINK                                               EL642
04398          PROGRAM   (PGM-NAME)                                     EL642
04399          COMMAREA  (EMI-LINE1)                                    EL642
04400          LENGTH    (72)                                           EL642
04401      END-EXEC.                                                       CL*27
04402                                                                   EL642
04403      IF PI-MAP-NAME  =  EL642A                                       CL**5
04404          MOVE -1                 TO  APFNTERL                        CL**5
04405      ELSE                                                         EL642
04406          MOVE -1                 TO  BPFNTERL.                       CL**5
04407                                                                   EL642
04408      GO TO 8200-SEND-DATAONLY.                                    EL642
04409                                                                      CL**5
04410  9995-SECURITY-VIOLATION.                                         EL642
04411                              COPY ELCSCTP.                        EL642
