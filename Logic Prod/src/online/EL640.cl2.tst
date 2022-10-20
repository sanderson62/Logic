00001  ID DIVISION.                                                     04/29/97
00002                                                                   EL640
00003  PROGRAM-ID.                 EL640.                                  LV040
00004 *              PROGRAM CONVERTED BY                                  CL*33
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*33
00006 *              CONVERSION DATE 04/27/94 15:38:12.                    CL*33
00007 *                            VMOD=2.040                              CL*40
00008 *                                                                 EL640
00009 *AUTHOR.     LOGIC,INC.                                              CL*33
00010 *            DALLAS, TEXAS.                                          CL*33
00011                                                                   EL640
00012 *DATE-COMPILED.                                                      CL*33
00013 *SECURITY.   *****************************************************   CL*33
00014 *            *                                                   *   CL*33
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*33
00016 *            *                                                   *   CL*33
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*33
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*33
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*33
00020 *            *                                                   *   CL*33
00021 *            *****************************************************   CL*33
00022 *                                                                 EL640
00023 *REMARKS.    TRANSACTION - EXC1 - ACCOUNT BILLING.                   CL**8
00024 *                                                                    CL**8
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL640AO FILLER
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
083104* 083104    2004083000002  PEMA PREVENT ONLINE BILLING  
101101******************************************************************

00025  ENVIRONMENT DIVISION.                                            EL640
00026                                                                   EL640
00027      EJECT                                                        EL640
00028  DATA DIVISION.                                                   EL640
00029  WORKING-STORAGE SECTION.                                         EL640
00030  77  FILLER  PIC X(32)  VALUE '********************************'. EL640
00031  77  FILLER  PIC X(32)  VALUE '*    EL640 WORKING STORAGE     *'. EL640
00032  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.040 ************'.    CL*40
070805 77  CNC-FACT                    PIC S999V9(7) COMP-3 VALUE +0.
070805 77  S1                          PIC S999 COMP-3 VALUE +0.
00033                                                                      CL*28
00034      COPY ELCSCTM.                                                   CL*28
00035      COPY ELCSCRTY.                                                  CL*28
00036                                                                   EL640
00037     EJECT                                                         EL640
070805 01  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
           88  RESP-NORMAL               VALUE +0.
           88  RESP-NOTFND               VALUE +13.
           88  RESP-NOTOPEN              VALUE +19.
           88  RESP-ENDFILE              VALUE +20.
00038                                                                   EL640
00039  01  STANDARD-AREAS.                                              EL640
070805     12  WS-AH-CATEGORY              PIC X       VALUE ' '.
070805     12  CLAS-LOOK                   PIC XX      VALUE '  '.
00040      12  SC-ITEM             PIC S9(4) COMP VALUE +1.             EL640
00041                                                                      CL*30
00042      12  W-TRANSFER-CONTROL.                                         CL*30
00043          16  WT-CR-CARRIER           PIC X     VALUE SPACES.         CL*37
00044          16  WT-CR-GROUPING          PIC X(6)  VALUE SPACES.         CL*37
00045          16  WT-CR-STATE             PIC XX    VALUE SPACES.         CL*37
00046          16  WT-CR-ACCOUNT           PIC X(10) VALUE SPACES.         CL*37
00047          16  WT-CR-FIN-RESP          PIC X(10) VALUE SPACES.         CL*37
00048          16  WT-CR-TYPE              PIC X     VALUE SPACES.         CL*37
00049                                                                      CL*30
00050      12  GETMAIN-SPACE       PIC X       VALUE SPACE.             EL640
00051      12  MAP-NAME            PIC X(8)    VALUE 'EL640A'.          EL640
00052      12  MAPSET-NAME         PIC X(8)    VALUE 'EL640S'.          EL640
00053      12  SCREEN-NUMBER       PIC X(4)    VALUE '640A'.            EL640
00054      12  TRANS-ID            PIC X(4)    VALUE 'EXC1'.            EL640
00055      12  EL6311-TRANS-ID     PIC X(4)    VALUE 'EXB1'.               CL*31
00056      12  EL633-TRANS-ID      PIC X(4)    VALUE 'EXB7'.               CL*30
00057      12  EL633DMD-TRANS-ID   PIC X(4)    VALUE 'EX1F'.               CL*35
00058      12  EL635-TRANS-ID      PIC X(4)    VALUE 'EXJ4'.               CL*30
00059      12  EL650-TRANS-ID      PIC X(4)    VALUE 'EXC4'.               CL*30
00060      12  EL652-TRANS-ID      PIC X(4)    VALUE 'EXD4'.               CL*30
00061      12  EL658-TRANS-ID      PIC X(4)    VALUE 'EXJ3'.               CL*30
00062      12  THIS-PGM            PIC X(8)    VALUE 'EL640'.           EL640
00063      12  EL640A              PIC X(8)    VALUE 'EL640A'.          EL640
00064      12  EL640B              PIC X(8)    VALUE 'EL640B'.          EL640
00065      12  PGM-NAME            PIC X(8).                            EL640
00066      12  TIME-IN             PIC S9(7).                           EL640
00067      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL640
00068          16  FILLER          PIC X.                               EL640
00069          16  TIME-OUT        PIC 99V99.                           EL640
00070          16  FILLER          PIC XX.                                 CL*40
00071                                                                      CL*40
00072      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL640
00073      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL640
00074      12  XCTL-626            PIC X(8)    VALUE 'EL626'.           EL640
00075      12  XCTL-633            PIC X(8)    VALUE 'EL633'.           EL640
00076      12  XCTL-633DMD         PIC X(8)    VALUE 'EL633DMD'.           CL*35
00077      12  XCTL-635            PIC X(8)    VALUE 'EL635'.              CL*15
00078      12  XCTL-PYAJ           PIC X(8)    VALUE 'EL633'.              CL*15
00079      12  XCTL-650            PIC X(8)    VALUE 'EL650'.           EL640
00080      12  XCTL-652            PIC X(8)    VALUE 'EL652'.           EL640
00081      12  XCTL-658            PIC X(8)    VALUE 'EL658'.              CL*11
00082      12  XCTL-6401           PIC X(8)    VALUE 'EL6401'.             CL*11
00083      12  LINK-001            PIC X(8)    VALUE 'EL001'.              CL*11
00084      12  LINK-004            PIC X(8)    VALUE 'EL004'.              CL*11
00085      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.            CL*11
00086      12  ERPNDB-ALT-FILE-ID  PIC X(8)    VALUE 'ERPNDB2'.            CL*11
00087      12  ERPNDB-FILE-ID      PIC X(8)    VALUE 'ERPNDB'.             CL*11
00088      12  ELCNTL-FILE-ID      PIC X(8)    VALUE 'ELCNTL'.             CL*11
00089      12  ELLETR-FILE-ID      PIC X(8)    VALUE 'ELLETR'.             CL*29
00090      12  ERCOMP-FILE-ID      PIC X(8)    VALUE 'ERCOMP'.             CL*11
00091      12  ERPYAJ-FILE-ID      PIC X(8)    VALUE 'ERPYAJ'.             CL*11
00092      12  ERACCT-ALT-FILE-ID  PIC X(8)    VALUE 'ERACCT2'.            CL*11
00093      12  ERACCT-FILE-ID      PIC X(8)    VALUE 'ERACCT'.             CL*11
00094      12  ERBILL-FILE-ID      PIC X(8)    VALUE 'ERBILL'.             CL*11
00095      12  RETURNED-FROM       PIC X(8)    VALUE SPACES.            EL640
00096      12  QID.                                                     EL640
00097          16  QID-TERM        PIC X(4).                            EL640
00098          16  FILLER          PIC X(4)    VALUE '640A'.            EL640
CIDMOD     12  CLIENT-ITY          PIC XXX     VALUE 'ITY'.                  000
00099                                                                   EL640
00100  01  WORK-AREA.                                                   EL640
00101      12  W-LETTER-IND            PIC X     VALUE SPACE.              CL*29
00102          88  W-LETTER-NOT-FOUND    VALUE 'Y'.                        CL*29
00103      12  FIRST-TIME-SW           PIC X     VALUE 'Y'.             EL640
00104          88  FIRST-TIME            VALUE 'Y'.                     EL640
00105      12  C-RECORD-FOUND-SW       PIC X     VALUE 'Y'.                CL**2
00106          88  C-RECORD-FOUND        VALUE 'Y'.                        CL**2
00107      12  ACCOUNT-FIN-RESP-SW     PIC X     VALUE ' '.             EL640
00108          88  ACCOUNT-FIN-RESP      VALUE 'Y'.                     EL640
00109          88  ACCOUNT-NOT-FIN-RESP  VALUE 'N'.                     EL640
00110      12  VALID-BILL-TYPE-VALUES  PIC X     VALUE SPACE.           EL640
083104         88  VALID-BILL-TYPE       VALUE  '1' '2'.
083104*        88  VALID-BILL-TYPE       VALUE  '1' '2' '3' '4' '5'.    EL640
00112      12  BILL-BATCH-TRAILER-SW   PIC X     VALUE SPACE.           EL640
00113          88  BILL-BATCH-TRAILER    VALUE 'Y'.                     EL640
00114      12  PNDB-EOF-SW             PIC X     VALUE SPACE.           EL640
00115          88  PNDB-EOF              VALUE 'Y'.                     EL640
00116      12  PYAJ-EOF-SW             PIC X     VALUE SPACE.           EL640
00117          88  PYAJ-EOF              VALUE 'Y'.                     EL640
00118      12  COMP-UPDATE-SW          PIC X     VALUE SPACE.           EL640
00119          88  UPDATE-COMP-TOTALS    VALUE 'Y'.                     EL640
00120      12  LIMIT-BILLING-SW        PIC X     VALUE SPACE.           EL640
00121          88  LIMIT-BILLING         VALUE 'Y'.                     EL640
00122      12  WS-PROCESS-SW           PIC X     VALUE SPACE.           EL640
00123          88  DO-NOT-BILL-THIS-ACCT VALUE 'Y'.                     EL640
00124      12  WS-CHARGEBACK-LF-SW     PIC X     VALUE SPACE.              CL*24
00125          88  CHARGEBACK-CAN-LF     VALUE 'Y'.                        CL*24
00126          88  NO-CHARGEBACK-LF      VALUE 'N'.                        CL*24
00127      12  WS-CHARGEBACK-AH-SW     PIC X     VALUE SPACE.              CL*24
00128          88  CHARGEBACK-CAN-AH     VALUE 'Y'.                        CL*24
00129          88  NO-CHARGEBACK-AH      VALUE 'N'.                        CL*24
00130      12  NO-BILL-REC-SW          PIC X     VALUE SPACE.           EL640
00131          88  NO-BILL-RECS          VALUE 'Y'.                     EL640
00132      12  DATA-VOIDED-SW          PIC X     VALUE SPACE.           EL640
00133          88  DATA-VOIDED           VALUE 'Y'.                     EL640
00134      12  BATCHES-PROCESSED-SW    PIC X     VALUE SPACE.           EL640
00135          88  MORE-THAN-6-BATCHES   VALUE 'Y'.                     EL640
00136      12  BILLING-DETAIL-TYPE     PIC XX    VALUE SPACES.          EL640
00137          88  TOTAL-STATEMENT       VALUE 'TS'.                    EL640
00138          88  BILLING-DETAIL        VALUE 'BD'.                    EL640
00139          88  CARRIER-ADDRESS       VALUE 'CA'.                    EL640
00140          88  GEN-AGT-ADDRESS       VALUE 'GA'.                    EL640
00141      12  ELCNTL-UPDATE-SW        PIC X     VALUE SPACE.           EL640
00142          88  ELCNTL-UPDATE         VALUE 'Y'.                     EL640
00143      12  WS-ENTERED-FROM-MAP     PIC X     VALUE 'A'.                CL*39
00144          88  WS-ENTERED-FROM-A     VALUE 'A'.                        CL*39
00145                                                                      CL*40
00146      12  ACCOM-SUB               PIC 99    VALUE ZEROS.           EL640
00147      12  SUB                     PIC S999  COMP-3  VALUE ZEROS.      CL*22
00148                                                                      CL*40
00149      12  WORK-ZIP                PIC 9(9)  VALUE ZEROS.              CL*19
00150      12  WORK-ZIPR1 REDEFINES WORK-ZIP.                              CL*19
00151          16  WORK-ZIPR1-4        PIC 9(4).                           CL*19
00152          16  WORK-ZIPR1-5        PIC 9(5).                           CL*19
00153      12  WORK-ZIPR2 REDEFINES WORK-ZIP.                              CL*19
00154          16  WORK-ZIPR2-5        PIC 9(5).                           CL*19
00155          16  WORK-ZIPR2-4        PIC 9(4).                           CL*19
00156      12  WS-AM-ZIP.                                                  CL*18
00157          16  WS-AM-ZIP-PRIME     PIC X(5).                           CL*18
00158          16  WS-AM-ZIP-DASH      PIC X.                              CL*18
00159          16  WS-AM-ZIP-PLUS4     PIC X(4).                           CL*18
00160      12  WS-AM-CANADIAN-ZIP  REDEFINES  WS-AM-ZIP.                   CL*18
00161          16  WS-AM-CAN-POST-1    PIC XXX.                            CL*18
00162          16  FILLER              PIC X.                              CL*18
00163          16  WS-AM-CAN-POST-2    PIC XXX.                            CL*18
00164          16  FILLER              PIC XXX.                            CL*18
00165      12  WS-CF-ZIP.                                                  CL*18
00166          16  WS-CF-ZIP-PRIME     PIC X(5) VALUE SPACES.              CL*37
00167          16  WS-CF-ZIP-DASH      PIC X    VALUE SPACES.              CL*37
00168          16  WS-CF-ZIP-PLUS4     PIC X(4) VALUE SPACES.              CL*37
00169      12  WS-CF-CANADIAN-ZIP  REDEFINES  WS-CF-ZIP.                   CL*18
00170          16  WS-CF-CAN-POST-1    PIC XXX.                            CL*18
00171          16  FILLER              PIC X.                              CL*18
00172          16  WS-CF-CAN-POST-2    PIC XXX.                            CL*18
00173          16  FILLER              PIC XXX.                            CL*18
00174      12  WS-CO-ZIP.                                                  CL*18
00175          16  WS-CO-ZIP-PRIME     PIC X(5) VALUE SPACES.              CL*37
00176          16  WS-CO-ZIP-DASH      PIC X    VALUE SPACES.              CL*37
00177          16  WS-CO-ZIP-PLUS4     PIC X(4) VALUE SPACES.              CL*37
00178      12  WS-CO-CANADIAN-ZIP  REDEFINES  WS-CO-ZIP.                   CL*18
00179          16  WS-CO-CAN-POST-1    PIC XXX.                            CL*18
00180          16  FILLER              PIC X.                              CL*18
00181          16  WS-CO-CAN-POST-2    PIC XXX.                            CL*18
00182          16  FILLER              PIC XXX.                            CL*18
00183                                                                      CL*40
00184      12  WORK-SEQ-NO             PIC S9(9)   COMP-3 VALUE +0.        CL*37
00185      12  WS-ERROR-MSG.                                            EL640
00186          16  WS-ERROR-TEXT       PIC X(35) VALUE SPACES.          EL640
00187          16  WS-ERROR-BATCH      PIC X(6)  VALUE SPACES.          EL640
00188          16  FILLER              PIC X(24) VALUE SPACES.          EL640
00189      12  WS-WORK-DATE.                                            EL640
00190          16  WS-MONTH            PIC 99    VALUE ZEROS.           EL640
00191          16  WS-DAY              PIC 99    VALUE ZEROS.           EL640
00192          16  WS-YEAR             PIC 99    VALUE ZEROS.           EL640
00193      12  WS-MONTH-END-DATE.                                       EL640
00194          16  WS-ME-YEAR          PIC 99    VALUE ZEROS.           EL640
00195          16  WS-ME-MONTH         PIC 99    VALUE ZEROS.           EL640
00196          16  WS-ME-DAY           PIC 99    VALUE ZEROS.           EL640
00197      12  WS-CURRENT-DATE         PIC XX    VALUE SPACES.          EL640
00198      12  WS-CURRENT-DATE-MDY     PIC X(6)  VALUE SPACES.          EL640
00199      12  WS-CURRENT-DATE-EDIT    PIC X(8)  VALUE SPACES.          EL640
00200      12  WS-PREV-BILL-DATE       PIC XX    VALUE LOW-VALUES.      EL640
00201      12  WS-BEGIN-DATE.                                           EL640
00202          16  FILLER              PIC X(3)  VALUE SPACES.          EL640
00203          16  WS-BEGIN-DAY        PIC XX    VALUE SPACES.          EL640
00204          16  FILLER              PIC X(3)  VALUE SPACES.          EL640
CIDMOD     12  PRINT-CONTROL.                                                000
CIDMOD         16  SINGLE-SPACE        PIC X     VALUE SPACE.                000
CIDMOD         16  DOUBLE-SPACE        PIC X     VALUE ZERO.                 000
CIDMOD         16  TRIPLE-SPACE        PIC X     VALUE '-'.                  000
CIDMOD         16  SUPPRESS-SPACE      PIC X     VALUE '+'.                  000
CIDMOD         16  TOP-OF-PAGE         PIC X     VALUE '1'.                  000
00205      12  WS-EXP-DATE.                                             EL640
00206          16  WS-SAV-EXP-DT     OCCURS 10 TIMES                    EL640
00207                                INDEXED BY DTNDX  PIC XX.          EL640
00208      12  WS-REMIT-TO.                                             EL640
00209          16  WS-SAV-REMIT-TO   OCCURS 10 TIMES                    EL640
00210                                INDEXED BY RTNDX  PIC X(10).       EL640
00211      12  WORK-REMIT-TO           PIC X(10) VALUE SPACES.          EL640
CIDMOD     12  WS-REMITTED             PIC S9(6)V99 VALUE ZEROS.             000
00212      12  WS-LINECTR              PIC 99    VALUE ZEROS.           EL640
00213      12  WS-PGECTR               PIC 999   VALUE 1.               EL640
00214      12  WS-LINE-SEQ-NO          PIC S9(4) COMP VALUE +1.         EL640
00215      12  WS-CARR-COMP.                                            EL640
00216          16  WS-CARRIER          PIC X     VALUE SPACE.           EL640
00217          16  WS-COMP             PIC X(6)  VALUE SPACES.          EL640
00218                                                                      CL*40
00219      12  REPORT-TOTALS.                                           EL640
00220          16  TOT-REPT-LF-PREM    PIC S9(7)V99   VALUE ZEROS.      EL640
CIDMOD         16  TOT-REPT-LF-REF     PIC S9(7)V99   VALUE ZEROS.      EL640
00221          16  TOT-REPT-LF-COMP    PIC S9(7)V99   VALUE ZEROS.      EL640
CIDMOD         16  TOT-REPT-LF-COMP-R  PIC S9(7)V99   VALUE ZEROS.      EL640
00222          16  TOT-REPT-AH-PREM    PIC S9(7)V99   VALUE ZEROS.      EL640
CIDMOD         16  TOT-REPT-AH-REF     PIC S9(7)V99   VALUE ZEROS.      EL640
00223          16  TOT-REPT-AH-COMP    PIC S9(7)V99   VALUE ZEROS.      EL640
CIDMOD         16  TOT-REPT-AH-COMP-R  PIC S9(7)V99   VALUE ZEROS.      EL640
00224          16  TOT-LF-REPT-FACE-AMT   PIC S9(7)V99   VALUE ZEROS.   EL640
00225          16  TOT-AH-REPT-FACE-AMT   PIC S9(7)V99   VALUE ZEROS.   EL640
00226          16  TOT-REPT-PREM       PIC S9(7)V99   VALUE ZEROS.      EL640
00227          16  TOT-REPT-CANCEL-FEE PIC S9(7)V99   VALUE ZEROS.         CL*33
00228      12  WORK-AMT                PIC S9(7)V99   VALUE ZEROS.      EL640
00229      12  WORK-CHECK-AMT          PIC S9(7)V99   VALUE ZEROS.         CL*39
00230      12  WORK-CANCEL-FEE         PIC S9(7)V99   VALUE ZEROS.         CL*33
00231      12  WS-I-LF-PREMIUM-AMT     PIC S9(7)V99   COMP-3 VALUE +0.     CL*40
00232      12  WS-I-LF-BENEFIT-AMT     PIC S9(7)V99   COMP-3 VALUE +0.  EL640
00233      12  WS-I-AH-PREMIUM-AMT     PIC S9(7)V99   COMP-3 VALUE +0.     CL*40
00234      12  WS-I-AH-BENEFIT-AMT     PIC S9(7)V99   COMP-3 VALUE +0.  EL640
00235      12  WS-C-LF-CANCEL-AMT      PIC S9(5)V99   COMP-3 VALUE +0.  EL640
00236      12  WS-C-AH-CANCEL-AMT      PIC S9(5)V99   COMP-3 VALUE +0.  EL640
00237      12  WS-C-CANCEL-FEE         PIC S9(5)V99   COMP-3 VALUE +0.     CL*33
00238      12  WS-LF-CAN-COMP          PIC S9(6)V99   COMP-3 VALUE +0.  EL640
00239      12  WS-AH-CAN-COMP          PIC S9(6)V99   COMP-3 VALUE +0.  EL640
00240      12  WS-PAY-ADJ              PIC S9(6)V99   COMP-3 VALUE +0.     CL*33
00241      12  WS-CI-LIFE-BENEFIT      PIC S9(7)V99   COMP-3 VALUE +0.  EL640
00242      12  LF-COMM-TBL-NO          PIC X(3)       VALUE SPACES.     EL640
00243      12  JT-COMM-TBL-NO          PIC X(3)       VALUE SPACES.     EL640
00244      12  AH-COMM-TBL-NO          PIC X(3)       VALUE SPACES.     EL640
00245      12  AGENT-OWES              PIC X(18)                        EL640
00246                          VALUE 'AGENT OWES       ='.              EL640
00247      12  OWED-TO-AGENT           PIC X(18)                        EL640
00248                          VALUE 'OWED TO AGENT    ='.              EL640
00249      12  WS-PY-ENTRY-COMMENT.                                     EL640
00250          16  WS-PY-CURRENT-DATE      PIC X(6) VALUE SPACES.          CL*37
00251          16  FILLER                  PIC X(14) VALUE              EL640
00252              ' CHK TO AGENT'.                                     EL640
00253                                                                   EL640
00254      12  WS-ITY-COMMENT.                                             CL*14
00255          16  WS-PY-BEGIN-DT          PIC X(8) VALUE SPACES.          CL*37
00256          16  FILLER                  PIC X(06) VALUE                 CL*14
00257              ' THRU '.                                               CL*14
00258          16  WS-PY-END-DT            PIC X(8) VALUE SPACES.          CL*37
00259                                                                      CL*14
00260      EJECT                                                        EL640
00261      12  WS-ACCT-ADDR-AREA.                                       EL640
00262          16  WS-ACCT-LINES OCCURS 6 TIMES INDEXED BY A-INDX.         CL**4
00263            18  WS-ACCT-ZIP           PIC X(10) VALUE SPACES.         CL*37
00264            18  FILLER                PIC X(10) VALUE SPACES.         CL*37
00265            18  WS-A-LAST-ZIP         PIC X(10) VALUE SPACES.         CL*37
00266                                                                   EL640
00267      12  WS-REMIT-ADDR-AREA.                                      EL640
00268          16  WS-REMIT-ACCT           PIC X(10) VALUE SPACES.         CL*37
00269          16  WS-REMIT-LINES OCCURS 6 TIMES INDEXED BY R-INDX.     EL640
00270            18  WS-REMIT-ZIP          PIC X(10) VALUE SPACES.         CL*37
00271            18  FILLER                PIC X(10) VALUE SPACES.         CL*37
00272            18  WS-R-LAST-ZIP         PIC X(10) VALUE SPACES.         CL*37
00273                                                                      CL*22
00274      12  WS-EFFECT-DATE.                                             CL*22
00275          16  WS-EFFECT-YR            PIC 99 VALUE ZEROS.             CL*40
00276          16  WS-EFFECT-MO            PIC 99 VALUE ZEROS.             CL*40
00277          16  WS-EFFECT-DA            PIC 99 VALUE ZEROS.             CL*40
00278                                                                      CL*22
00279      12  WS-LIFE-CANCEL-DT.                                          CL*22
00280          16  WS-LF-CANCEL-YR         PIC 99 VALUE ZEROS.             CL*40
00281          16  WS-LF-CANCEL-MO         PIC 99 VALUE ZEROS.             CL*40
00282          16  WS-LF-CANCEL-DA         PIC 99 VALUE ZEROS.             CL*40
00283                                                                      CL*22
00284      12  WS-AH-CANCEL-DT.                                            CL*22
00285          16  WS-AH-CANCEL-YR         PIC 99 VALUE ZEROS.             CL*40
00286          16  WS-AH-CANCEL-MO         PIC 99 VALUE ZEROS.             CL*40
00287          16  WS-AH-CANCEL-DA         PIC 99 VALUE ZEROS.             CL*40
00288                                                                      CL*22
00289      12  MONTHS-DIFF-LF            PIC S9(05)  VALUE ZEROS.          CL*37
00290      12  MONTHS-DIFF-AH            PIC S9(05)  VALUE ZEROS.          CL*37
00291                                                                      CL*36
00292      12  WS-BENEFIT.                                                 CL*36
00293          16  FILLER                PIC X   VALUE SPACES.             CL*37
00294          16  WS-BENE-CD            PIC XX  VALUE SPACES.             CL*37
00295                                                                      CL*36
00296      12  WS-DESC.                                                    CL*36
00297          16 WS-BATCH-DESC        PIC X(10) VALUE SPACES.             CL*37
00298          16 WS-BATCH-NBR         PIC X(6)  VALUE SPACES.             CL*37
00299          16 FILLER               PIC X(4)  VALUE SPACES.             CL*37
00300      12  WS-NON-PREM             PIC S9(7)V99   COMP-3.              CL*36
00301      12  WS-NON-COMM             PIC S9(7)V99   COMP-3.              CL*36
00302      EJECT                                                        EL640
00303  01  ACCESS-KEYS.                                                 EL640
00304      12  ERPNDB-PRIME-KEY.                                        EL640
00305          16  ERPNDB-CO-CD        PIC X     VALUE SPACES.          EL640
00306          16  ERPNDB-BATCH        PIC X(6)  VALUE SPACES.          EL640
00307          16  ERPNDB-SEQ-NO       PIC S9(4) COMP VALUE +0.            CL*37
00308          16  ERPNDB-CHG-SEQ-NO   PIC S9(4) COMP VALUE +0.            CL*37
00309                                                                   EL640
00310      12  ERPNDB-LENGTH           PIC S9(4) COMP VALUE +585.       EL640
00311                                                                   EL640
00312      12  ERPNDB-ALT-KEY.                                          EL640
00313          16  ERPNDB-CO-CD-A1     PIC X     VALUE SPACES.          EL640
00314          16  ERPNDB-CARR         PIC X     VALUE SPACES.          EL640
00315          16  ERPNDB-GROUP        PIC X(6)  VALUE SPACES.          EL640
00316          16  ERPNDB-STATE        PIC XX    VALUE SPACES.          EL640
00317          16  ERPNDB-ACCT         PIC X(10) VALUE SPACES.          EL640
00318          16  ERPNDB-EFF-DT       PIC XX    VALUE SPACES.          EL640
00319          16  ERPNDB-CERT.                                         EL640
00320              20  ERPNDB-CERT-PRM PIC X(10) VALUE SPACES.          EL640
00321              20  ERPNDB-CERTSFX  PIC X     VALUE SPACES.          EL640
00322          16  ERPNDB-ACHG-SEQ-NO  PIC S9(4) COMP.                  EL640
00323          16  ERPNDB-REC-TYPE     PIC X     VALUE SPACES.          EL640
00324                                                                   EL640
00325      12  ELCNTL-KEY.                                              EL640
00326          16  ELCNTL-COMPANY-ID   PIC XXX   VALUE SPACES.          EL640
00327          16  ELCNTL-REC-TYPE     PIC X     VALUE SPACES.          EL640
070805         16  ELCNTL-ACCESS.
00328              20  ELCNTL-FILLER   PIC X(3)  VALUE SPACES.
00329              20  ELCNTL-CARRIER  PIC X     VALUE SPACES.
00330          16  ELCNTL-SEQ-NO       PIC S9(4) COMP VALUE ZEROS.      EL640
00331                                                                   EL640
00332      12  ELCNTL-LENGTH           PIC S9(4) COMP VALUE +504.       EL640
00333                                                                   EL640
00334      12  ERCOMP-KEY.                                              EL640
00335          16  ERCOMP-COMP-CD      PIC X     VALUE SPACE.           EL640
00336          16  ERCOMP-CARRIER      PIC X     VALUE SPACES.          EL640
00337          16  ERCOMP-GROUPING     PIC X(6)  VALUE SPACES.          EL640
00338          16  ERCOMP-FIN-RESP     PIC X(10) VALUE SPACES.          EL640
00339          16  ERCOMP-ACCT         PIC X(10) VALUE SPACES.          EL640
00340          16  ERCOMP-RECORD-TYPE  PIC X     VALUE SPACES.          EL640
00341                                                                   EL640
00342      12  ERPYAJ-BROWSE-COMP-KEY.                                  EL640
00343          16  ERPYAJ-BR-COMP-CD   PIC X     VALUE SPACE.           EL640
00344          16  ERPYAJ-BR-CARRIER   PIC X     VALUE SPACES.          EL640
00345          16  ERPYAJ-BR-GROUPING  PIC X(6)  VALUE SPACES.          EL640
00346          16  ERPYAJ-BR-FIN-RESP  PIC X(10) VALUE SPACES.          EL640
00347          16  ERPYAJ-BR-ACCOUNT   PIC X(10) VALUE SPACES.          EL640
00348                                                                   EL640
00349      12  ERCOMP-LENGTH           PIC S9(4) COMP VALUE +700.          CL*28
00350                                                                   EL640
00351      12  ERPYAJ-KEY.                                              EL640
00352          16  ERPYAJ-COMP-CD      PIC X     VALUE SPACE.           EL640
00353          16  ERPYAJ-CARRIER      PIC X     VALUE SPACES.          EL640
00354          16  ERPYAJ-GROUPING     PIC X(6)  VALUE SPACES.          EL640
00355          16  ERPYAJ-FIN-RESP     PIC X(10) VALUE SPACES.          EL640
00356          16  ERPYAJ-ACCOUNT      PIC X(10) VALUE SPACES.          EL640
00357          16  ERPYAJ-FILE-SEQ-NO  PIC S9(8) VALUE +0 COMP.         EL640
00358          16  ERPYAJ-RECORD-TYPE  PIC X     VALUE SPACES.          EL640
00359                                                                   EL640
00360      12  ERPYAJ-LENGTH           PIC S9(4) COMP VALUE +200.          CL*14
00361                                                                   EL640
00362      12  ERACCT-PRIME-KEY.                                        EL640
00363          16  ERACCT-P-CO-CD      PIC X     VALUE SPACES.          EL640
00364          16  ERACCT-P-CARRIER    PIC X     VALUE SPACES.          EL640
00365          16  ERACCT-P-GROUPING   PIC X(6)  VALUE SPACES.          EL640
00366          16  ERACCT-P-STATE      PIC XX    VALUE SPACES.          EL640
00367          16  ERACCT-P-ACCOUNT    PIC X(10) VALUE SPACES.          EL640
00368          16  ERACCT-P-EXP-DATE   PIC XX    VALUE SPACES.          EL640
00369          16  FILLER              PIC X(4)  VALUE SPACES.          EL640
00370                                                                   EL640
00371      12  ERACCT-LENGTH           PIC S9(4) COMP VALUE +2000.      EL640
00372                                                                   EL640
00373      12  ERACCT-ALT-KEY.                                          EL640
00374          16  ERACCT-A-CO-CD      PIC X     VALUE SPACES.          EL640
00375          16  ERACCT-A-CARRIER    PIC X     VALUE SPACES.          EL640
00376          16  ERACCT-A-GROUPING   PIC X(6)  VALUE SPACES.          EL640
00377          16  ERACCT-A-STATE      PIC XX    VALUE SPACES.          EL640
00378          16  ERACCT-A-ACCOUNT    PIC X(10) VALUE SPACES.          EL640
00379          16  ERACCT-A-EXP-DATE   PIC XX    VALUE SPACES.          EL640
00380          16  FILLER              PIC X(4)  VALUE SPACES.          EL640
00381                                                                   EL640
00382      12  ERBILL-KEY.                                              EL640
00383          16  ERBILL-CO-CD        PIC X.                           EL640
00384          16  ERBILL-CARRIER      PIC X.                           EL640
00385          16  ERBILL-GROUP        PIC X(6).                        EL640
00386          16  ERBILL-ACCT         PIC X(10).                       EL640
00387          16  ERBILL-FIN-RESP     PIC X(10).                       EL640
00388          16  ERBILL-REC-TYPE     PIC X.                           EL640
00389          16  ERBILL-LINE-SEQ-NO  PIC S9(4) COMP.                  EL640
00390                                                                   EL640
00391      12  ERBILL-LENGTH           PIC S9(4) COMP VALUE +210.       EL640
00392                                                                   EL640
00393      12  ELLETR-KEY.                                                 CL*29
00394          16  ELLETR-COMPANY-CD   PIC  X      VALUE SPACE.            CL*40
00395          16  ELLETR-FORM-NO      PIC  X(12)  VALUE SPACES.           CL*29
00396          16  ELLETR-LINE-SEQ     PIC S9(04)  VALUE +0 COMP.          CL*29
00397                                                                      CL*29
00398      EJECT                                                        EL640
00399                                                                   EL640
00400  01  ERROR-NUMBERS.                                               EL640
00401      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL640
00402      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL640
00403      12  ER-0013                 PIC X(4)  VALUE '0013'.             CL*29
00404      12  ER-0022                 PIC X(4)  VALUE '0022'.          EL640
00405      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL640
00406      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL640
00407      12  ER-0194                 PIC X(4)  VALUE '0194'.          EL640
00408      12  ER-0195                 PIC X(4)  VALUE '0195'.          EL640
00409      12  ER-0196                 PIC X(4)  VALUE '0196'.          EL640
00410      12  ER-0197                 PIC X(4)  VALUE '0197'.          EL640
00411      12  ER-2208                 PIC X(4)  VALUE '2208'.          EL640
00412      12  ER-2210                 PIC X(4)  VALUE '2210'.          EL640
00413      12  ER-2212                 PIC X(4)  VALUE '2212'.             CL*33
00414      12  ER-2215                 PIC X(4)  VALUE '2215'.          EL640
00415      12  ER-2230                 PIC X(4)  VALUE '2230'.          EL640
00416      12  ER-2233                 PIC X(4)  VALUE '2233'.          EL640
00417      12  ER-2249                 PIC X(4)  VALUE '2249'.          EL640
00418      12  ER-2250                 PIC X(4)  VALUE '2250'.          EL640
00419      12  ER-2370                 PIC X(4)  VALUE '2370'.          EL640
00420      12  ER-2371                 PIC X(4)  VALUE '2371'.          EL640
00421      12  ER-2383                 PIC X(4)  VALUE '2383'.          EL640
00422      12  ER-2385                 PIC X(4)  VALUE '2385'.          EL640
00423      12  ER-2401                 PIC X(4)  VALUE '2401'.          EL640
00424      12  ER-2403                 PIC X(4)  VALUE '2403'.          EL640
00425      12  ER-2404                 PIC X(4)  VALUE '2404'.          EL640
00426      12  ER-2405                 PIC X(4)  VALUE '2405'.          EL640
00427      12  ER-2406                 PIC X(4)  VALUE '2406'.          EL640
00428      12  ER-2407                 PIC X(4)  VALUE '2407'.          EL640
00429      12  ER-2408                 PIC X(4)  VALUE '2408'.          EL640
00430      12  ER-2409                 PIC X(4)  VALUE '2409'.          EL640
00431      12  ER-2411                 PIC X(4)  VALUE '2411'.          EL640
00432      12  ER-2421                 PIC X(4)  VALUE '2421'.          EL640
00433      12  ER-2434                 PIC X(4)  VALUE '2434'.          EL640
00434      12  ER-2435                 PIC X(4)  VALUE '2435'.          EL640
00435      12  ER-2436                 PIC X(4)  VALUE '2436'.          EL640
00436      12  ER-2438                 PIC X(4)  VALUE '2438'.          EL640
00437      12  ER-2439                 PIC X(4)  VALUE '2439'.          EL640
00438      12  ER-2443                 PIC X(4)  VALUE '2443'.          EL640
00439      12  ER-2472                 PIC X(4)  VALUE '2472'.          EL640
00440      12  ER-2539                 PIC X(4)  VALUE '2539'.          EL640
00441      12  ER-2564                 PIC X(4)  VALUE '2564'.          EL640
00442      12  ER-2570                 PIC X(4)  VALUE '2570'.          EL640
00443      12  ER-2571                 PIC X(4)  VALUE '2571'.          EL640
00444      12  ER-2597                 PIC X(4)  VALUE '2597'.          EL640
00445      12  ER-3145                 PIC X(4)  VALUE '3145'.             CL*14
00446      12  ER-3165                 PIC X(4)  VALUE '3165'.             CL*30
00447      12  ER-3188                 PIC X(4)  VALUE '3188'.             CL*22
00448      12  ER-7389                 PIC X(4)  VALUE '7389'.             CL*29
00449      EJECT                                                        EL640
00450  01  BI-REPORT-HEADINGS.                                          EL640
00451      12  BI-PREVIEW-HD.                                           EL640
00452          16  FILLER          PIC X(42)           VALUE SPACES.    EL640
00453          16  FILLER          PIC X(47)           VALUE            EL640
00454                 '**** STATEMENT IS FOR REVIEW PURPOSES ONLY ****'.EL640
00455          16  FILLER          PIC X(43)           VALUE SPACES.    EL640
00456                                                                   EL640
00457      12  BI-HD1.                                                  EL640
00458          16  FILLER          PIC X(44)           VALUE SPACES.    EL640
00459          16  FILLER          PIC X(44)           VALUE            EL640
00460                  '  CREDIT LIFE, ACCIDENT & HEALTH STATEMENT  '.  EL640
00461          16  FILLER          PIC X(36)           VALUE SPACES.    EL640
00462          16  FILLER          PIC X(8)            VALUE ' EL640 '. EL640
00463                                                                   EL640
00464      12  BI-HD2.                                                  EL640
00465          16  FILLER          PIC X(51)           VALUE SPACES.    EL640
00466          16  BI-HD-CO        PIC X(30).                           EL640
00467          16  FILLER          PIC X(43)           VALUE SPACES.    EL640
00468          16  BI-HD-RUN-DT    PIC X(8)            VALUE SPACES.    EL640
00469                                                                   EL640
00470      12  BI-HD3.                                                  EL640
00471          16  FILLER          PIC X(57)           VALUE SPACES.    EL640
00472          16  BI-HD-BILL-DT   PIC X(18).                           EL640
00473          16  FILLER          PIC X(37)           VALUE SPACES.    EL640
00474          16  FILLER          PIC X(5)            VALUE 'PAGE'.    EL640
00475          16  BI-HD-PG        PIC ZZ,ZZ9.                          EL640
00476                                                                   EL640
00477      12  BI-HD4.                                                  EL640
00478          16  FILLER          PIC  X(44)      VALUE                EL640
00479              '                               CERTIFICATE  '.      EL640
00480          16  FILLER          PIC  X(44)      VALUE                EL640
00481              'EFFECTIVE   CANCEL       BEN.      WRITTEN  '.      EL640
00482          12  FILLER          PIC  X(44)      VALUE                EL640
00483              '    COMP.                    FACE AMOUNT /  '.      EL640
00484                                                                   EL640
00485      12  BI-HD5.                                                  EL640
00486          16  FILLER          PIC  X(44)      VALUE                EL640
00487              '       NAME OF INSURED            NUMBER    '.      EL640
00488          16  FILLER          PIC  X(44)      VALUE                EL640
00489              '  DATE       DATE   TERM TYPE      PREMIUM  '.      EL640
00490          16  FILLER          PIC  X(44)      VALUE                EL640
00491              '     PCT.    COMPENSATION        BENEFIT    '.      EL640
00492                                                                   EL640
00493      12  BI-HD6.                                                     CL*36
00494          16  FILLER          PIC  X(44)      VALUE SPACES.           CL*36
00495          16  FILLER          PIC  X(37)      VALUE SPACES.           CL*36
00496          16  FILLER          PIC  X(45)      VALUE                   CL*36
00497              '     PROCESSED                  NON-PROCESSED'.        CL*36
00498          16  FILLER          PIC  X(07)      VALUE SPACES.           CL*36
00499      12  BI-HD7.                                                     CL*36
00500          16  FILLER          PIC  X(44)      VALUE SPACES.           CL*36
00501          16  FILLER          PIC  X(35)      VALUE SPACES.           CL*36
00502          16  FILLER          PIC  X(45)      VALUE                   CL*36
00503              ' PREMIUM     COMMISSION        PREMIUM     CO'.        CL*36
00504          16  FILLER          PIC  X(09)      VALUE                   CL*36
00505              'MMISSION'.                                             CL*36
00506                                                                      CL*36
00507      EJECT                                                        EL640
00508                                                                   EL640
00509                              COPY ELCDATE.                           CL*20
00510      EJECT                                                        EL640
00511                              COPY ELCLOGOF.                          CL*20
00512      EJECT                                                        EL640
00513                              COPY ELCATTR.                           CL*20
00514      EJECT                                                        EL640
00515                              COPY ELCEMIB.                           CL*20
00516      EJECT                                                        EL640
00517                              COPY ELCINTF.                           CL*20
00518      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                EL640
00519          16  PI-MAP-NAME         PIC X(8).                        EL640
00520          16  PI-ACCT-NAME        PIC X(30).                       EL640
00521          16  PI-SAV-REMIT-TO     PIC X(10).                       EL640
00522          16  PI-SAV-ACCT         PIC X(10).                       EL640
00523          16  PI-SAV-CARR         PIC X.                           EL640
00524          16  PI-SAV-GROUP        PIC X(6).                        EL640
00525          16  PI-SAV-STATE        PIC XX.                          EL640
00526          16  PI-SAV-EXP-DT       PIC XX.                          EL640
00527          16  PI-BAL-FRWD-GA      PIC S9(7)V99   COMP-3.              CL*33
00528          16  PI-BAL-FRWD         PIC S9(7)V99   COMP-3.              CL*33
00529          16  PI-PREMIUM          PIC S9(7)V99   COMP-3.              CL*33
00530          16  PI-REMITTED         PIC S9(7)V99   COMP-3.              CL*33
00531          16  PI-TOT-ISS-COMP     PIC S9(7)V99   COMP-3.              CL*33
00532          16  PI-TOT-CAN-COMP     PIC S9(7)V99   COMP-3.              CL*33
00533          16  PI-ADJUSTMENTS      PIC S9(7)V99   COMP-3.              CL*33
00534          16  PI-DISBURSED        PIC S9(7)V99   COMP-3.              CL*33
00535          16  PI-END-BAL          PIC S9(7)V99   COMP-3.              CL*33
00536          16  PI-LF-ISS-COMP      PIC S9(7)V99   COMP-3.              CL*33
00537          16  PI-AH-ISS-COMP      PIC S9(7)V99   COMP-3.              CL*33
00538          16  PI-LF-CAN-COMP      PIC S9(7)V99   COMP-3.              CL*33
00539          16  PI-AH-CAN-COMP      PIC S9(7)V99   COMP-3.              CL*33
00540          16  PI-LIMIT-BILLING-BTCHS.                              EL640
00541              20  PI-BILLING-BATCHES   OCCURS 3 TIMES              EL640
00542                                  PIC X(6).                        EL640
00543          16  PI-BILL-TYPE        PIC X.                           EL640
00544              88  PI-PREV-BILL        VALUE '1'.                   EL640
00545              88  PI-PREV-REBILL      VALUE '2'.                   EL640
00546              88  PI-PREVIEW          VALUE '1' '2'.               EL640
00547              88  PI-BILL             VALUE '3'.                   EL640
00548              88  PI-REBILLING        VALUE '4'.                   EL640
00549              88  PI-VOID-BILL        VALUE '5'.                   EL640
00550              88  PI-TOT-REBILL       VALUE '2' '4'.               EL640
00551              88  PI-UPDATE-FILES     VALUE '3' '4' '5'.           EL640
00552              88  PI-BILLING-FUNCTION VALUE '1' '2' '3' '4'.       EL640
00553          16  PI-BILL-ERRS        PIC X.                           EL640
00554          16  PI-CHECK-SW         PIC X.                           EL640
00555              88  PI-CHECK-PRODUCED   VALUE 'Y'.                   EL640
00556          16  PI-DATA-BILLED-SW   PIC X.                           EL640
00557              88  PI-DATA-BILLED      VALUE 'Y'.                   EL640
00558          16  PI-MONTH-END-DATE.                                   EL640
00559              20  PI-ME-MONTH     PIC 99.                          EL640
00560              20  FILLER          PIC X.                           EL640
00561              20  PI-ME-DAY       PIC 99.                          EL640
00562              20  FILLER          PIC X.                           EL640
00563              20  PI-ME-YEAR      PIC 99.                          EL640
00564          16  PI-BILLING-COUNTS.                                   EL640
00565              20  FILLER OCCURS 6 TIMES INDEXED BY PINDX.          EL640
00566                  24  PI-BATCH    PIC X(6).                        EL640
00567                  24  PI-BILLED   PIC 9(6)  COMP-3.                EL640
00568                  24  PI-PREV     PIC 9(6)  COMP-3.                EL640
00569                  24  PI-NOBILL   PIC 9(6)  COMP-3.                EL640
00570                  24  PI-PREM     PIC S9(7)V99   COMP-3.              CL*36
00571                  24  PI-COMM     PIC S9(7)V99   COMP-3.              CL*36
00572                  24  PI-NON-PREM PIC S9(7)V99   COMP-3.              CL*36
00573                  24  PI-NON-COMM PIC S9(7)V99   COMP-3.              CL*36
00574          16  PI-COMP-CONTROL.                                     EL640
00575              20  PI-COMP-CARRIER     PIC X.                       EL640
00576              20  PI-COMP-GROUPING    PIC X(6).                    EL640
00577              20  PI-COMP-FIN-RESP    PIC X(10).                   EL640
00578          16  PI-SCRN-CONTROL.                                        CL*30
00579              20  PI-SCR-CARRIER      PIC X.                          CL*30
00580              20  PI-SCR-GROUPING     PIC X(6).                       CL*30
00581              20  PI-SCR-STATE        PIC X(02).                      CL*30
00582              20  PI-SCR-ACCOUNT      PIC X(10).                      CL*30
00583              20  PI-SCR-FIN-RESP     PIC X(10).                      CL*30
00584              20  PI-SCR-TYPE         PIC X.                          CL*30
00585          16  PI-TRANSFER-SW          PIC X.                          CL*30
00586              88  PI-TRANSFER-BEFORE-ACT   VALUE 'Y'.                 CL*30
00587          16  FILLER                  PIC X(200).                     CL*36
00588                                                                   EL640
00589      EJECT                                                        EL640
00590                              COPY ELCJPFX.                           CL*20
00591                              PIC X(1464).                         EL640
00592      EJECT                                                        EL640
00593                              COPY ELCAID.                            CL*20
00594  01  FILLER    REDEFINES DFHAID.                                  EL640
00595      12  FILLER              PIC X(8).                            EL640
00596      12  PF-VALUES           PIC X       OCCURS 2.                EL640
00597      EJECT                                                        EL640
00598                                           COPY EL640S.               CL*20
00599  01  MAP-A REDEFINES EL640AO.                                     EL640
101101     12  FILLER                  PIC X(162).                      EL640
00601      12  FILLER OCCURS 6 TIMES INDEXED BY ANDX.                   EL640
00602          16  FILLER              PIC X(35).                       EL640
00603          16  BATCH-LEN           PIC S9(4)   COMP.                EL640
00604          16  BATCH-ATTRB         PIC X.                           EL640
00605          16  BATCH               PIC X(6).                        EL640
00606          16  BILL-LEN            PIC S9(4)   COMP.                EL640
00607          16  BILL-ATTRB          PIC X.                           EL640
00608          16  BILL                PIC Z(6).                        EL640
00609          16  PREV-LEN            PIC S9(4)   COMP.                EL640
00610          16  PREV-ATTRB          PIC X.                           EL640
00611          16  PREV                PIC Z(6).                        EL640
00612          16  NOBILL-LEN          PIC S9(4)   COMP.                EL640
00613          16  NOBILL-ATTRB        PIC X.                           EL640
00614          16  NOBILL              PIC Z(6).                        EL640
00615      EJECT                                                        EL640
00616  LINKAGE SECTION.                                                 EL640
00617  01  DFHCOMMAREA             PIC X(1024).                         EL640
00618                                                                   EL640
00619      EJECT                                                        EL640
00620      EJECT                                                        EL640
00621                              COPY ERCPNDB.                           CL*20
00622      EJECT                                                        EL640
00623                              COPY ELCCNTL.                           CL*20
00624      EJECT                                                        EL640
00625                              COPY ERCCOMP.                           CL*20
00626      EJECT                                                        EL640
00627                              COPY ERCPYAJ.                           CL*20
00628      EJECT                                                        EL640
00629                              COPY ERCACCT.                           CL*20
00630      EJECT                                                        EL640
00631                              COPY ERCBILL.                           CL*20
00632      EJECT                                                           CL*29
00633                              COPY ELCTEXT.                           CL*29
00634      EJECT                                                        EL640
00635                                                                   EL640
00636  PROCEDURE DIVISION.                                              EL640
00637                                                                   EL640
00638      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL640
00639      MOVE 2                      TO EMI-NUMBER-OF-LINES.          EL640
00640      MOVE EIBTRMID               TO QID-TERM.                     EL640
00641      MOVE EIBDATE                TO DC-JULIAN-YYDDD.                 CL*11
00642      MOVE '5'                    TO DC-OPTION-CODE.                  CL*11
00643      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                       CL*11
00644      MOVE DC-BIN-DATE-1          TO WS-CURRENT-DATE.              EL640
00645      MOVE DC-GREG-DATE-1-MDY     TO WS-CURRENT-DATE-MDY.          EL640
00646      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DATE-EDIT.         EL640
00647                                                                   EL640
00648      IF EIBCALEN = 0                                              EL640
00649          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL640
00650                                                                   EL640
00651      IF PI-RETURN-TO-PROGRAM = THIS-PGM                           EL640
00652          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM.                EL640
00653                                                                   EL640
00654      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL640
00655          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL640
00656              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL640
00657              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL640
00658              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL640
00659              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL640
00660              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL640
00661              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL640
00662              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL640
00663              MOVE THIS-PGM             TO PI-CALLING-PROGRAM         CL*11
00664              MOVE PI-CR-CONTROL-IN-PROGRESS TO W-TRANSFER-CONTROL    CL*38
00665              MOVE SPACES          TO PI-CR-CONTROL-IN-PROGRESS       CL*11
00666          ELSE                                                     EL640
00667              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL640
00668              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL640
00669              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL640
00670              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL640
00671              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL640
00672              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL640
00673              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL640
00674              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL640
00675                                                                   EL640
00676      MOVE LOW-VALUES             TO EL640AI.                      EL640
00677                                                                   EL640
00678      IF PI-AR-PROCESSING                                             CL*15
00679         MOVE XCTL-635            TO XCTL-PYAJ.                       CL*15
00680                                                                      CL*15
00681      IF PI-COMPANY-ID = 'DMD'                                        CL*35
00682         MOVE XCTL-633DMD         TO XCTL-PYAJ.                       CL*35
00683                                                                      CL*35
00684      IF RETURNED-FROM NOT = SPACES                                EL640
00685          PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0690-EXIT         EL640
00686          IF RETURNED-FROM = XCTL-PYAJ AND NOT PI-VOID-BILL           CL*15
00687              MOVE ZEROS          TO PI-ADJUSTMENTS                   CL*33
00688                                     PI-DISBURSED                  EL640
00689                                     PI-REMITTED                   EL640
00690              GO TO 1100-PYAJ-BILLING-PROCESS                      EL640
00691          ELSE                                                     EL640
00692              MOVE -1             TO APFNTERL                      EL640
00693              PERFORM 5000-FORMAT-SCREEN THRU 5090-EXIT            EL640
00694              GO TO 8100-SEND-INITIAL-MAP.                         EL640
00695                                                                   EL640
00696      IF EIBTRNID NOT = TRANS-ID                                   EL640
00697          PERFORM 0400-CLEAR-PI  THRU  0499-EXIT                      CL*37
00698          MOVE EL640A             TO PI-MAP-NAME                   EL640
00699          MOVE -1                 TO APFNTERL                      EL640
00700          GO TO 8100-SEND-INITIAL-MAP.                             EL640
00701                                                                   EL640
00702      EXEC CICS HANDLE CONDITION                                   EL640
00703          PGMIDERR  (9600-PGMID-ERROR)                             EL640
00704          ERROR     (9990-ABEND)                                   EL640
00705      END-EXEC.                                                       CL*11
00706                                                                   EL640
00707      IF EIBAID = DFHCLEAR                                         EL640
00708          IF PI-MAP-NAME = EL640B                                  EL640
00709              PERFORM 5000-FORMAT-SCREEN THRU 5090-EXIT            EL640
00710              MOVE EL640A         TO PI-MAP-NAME                      CL*40
00711              MOVE -1             TO APFNTERL                      EL640
00712              GO TO 8100-SEND-INITIAL-MAP                          EL640
00713          ELSE                                                     EL640
00714              GO TO 9400-CLEAR.                                    EL640
00715                                                                   EL640
00716      IF PI-PROCESSOR-ID = 'LGXX'                                  EL640
00717          GO TO 0200-RECEIVE.                                      EL640
00718                                                                   EL640
00719      EXEC CICS READQ TS                                           EL640
00720          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       EL640
00721          INTO   (SECURITY-CONTROL)                                EL640
00722          LENGTH (SC-COMM-LENGTH)                                  EL640
00723          ITEM   (SC-ITEM)                                         EL640
00724      END-EXEC.                                                    EL640
00725                                                                   EL640
00726      MOVE SC-CREDIT-DISPLAY (18)  TO PI-DISPLAY-CAP.              EL640
00727      MOVE SC-CREDIT-UPDATE  (18)  TO PI-MODIFY-CAP.               EL640
00728                                                                   EL640
00729      IF NOT DISPLAY-CAP                                           EL640
00730          MOVE 'READ'          TO SM-READ                          EL640
00731          PERFORM 9995-SECURITY-VIOLATION                          EL640
00732          MOVE ER-0070         TO  EMI-ERROR                       EL640
00733          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL640
00734          GO TO 8100-SEND-INITIAL-MAP.                             EL640
00735                                                                   EL640
00736  0200-RECEIVE.                                                    EL640
00737      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                          CL*11
00738          MOVE ER-0008            TO EMI-ERROR                     EL640
00739          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL640
00740          IF PI-MAP-NAME = EL640A                                  EL640
00741              MOVE -1             TO APFNTERL                      EL640
00742              GO TO 8200-SEND-DATAONLY                             EL640
00743          ELSE                                                     EL640
00744              MOVE -1             TO BPFNTERL                      EL640
00745              GO TO 8200-SEND-DATAONLY.                            EL640
00746                                                                   EL640
00747      EXEC CICS RECEIVE                                            EL640
00748          MAP      (PI-MAP-NAME)                                   EL640
00749          MAPSET   (MAPSET-NAME)                                   EL640
00750          INTO     (EL640AI)                                       EL640
00751      END-EXEC.                                                       CL*11
00752                                                                   EL640
CIDMOD     IF PI-END-BAL NOT NUMERIC                                         000
CIDMOD         MOVE ZEROS              TO PI-END-BAL.                        000
CIDMOD                                                                       000
00753      IF PI-MAP-NAME = EL640A                                      EL640
00754          IF APFNTERL > ZERO                                          CL*40
00755              IF EIBAID NOT = DFHENTER                             EL640
00756                  MOVE ER-0004    TO EMI-ERROR                     EL640
00757                  MOVE AL-UNBOF   TO APFNTERA                      EL640
00758                  MOVE -1         TO APFNTERL                      EL640
00759                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL640
00760                  GO TO 8200-SEND-DATAONLY                         EL640
00761              ELSE                                                 EL640
00762                  IF APFNTERI NUMERIC AND                             CL*40
00763                    (APFNTERI > 0 AND < 25)                           CL*40
00764                      MOVE PF-VALUES (APFNTERI) TO EIBAID          EL640
00765                  ELSE                                             EL640
00766                      MOVE ER-0029  TO EMI-ERROR                   EL640
00767                      MOVE AL-UNBOF TO APFNTERA                    EL640
00768                      MOVE -1       TO APFNTERL                    EL640
00769                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL640
00770                      GO TO 8200-SEND-DATAONLY                     EL640
00771          ELSE                                                     EL640
00772              NEXT SENTENCE                                        EL640
00773      ELSE                                                         EL640
00774      IF PI-MAP-NAME = EL640B                                      EL640
00775          IF BPFNTERL > ZERO                                          CL*40
00776              IF EIBAID NOT = DFHENTER                             EL640
00777                  MOVE ER-0004    TO EMI-ERROR                     EL640
00778                  MOVE AL-UNBOF   TO BPFNTERA                      EL640
00779                  MOVE -1         TO BPFNTERL                      EL640
00780                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL640
00781                  GO TO 8200-SEND-DATAONLY                         EL640
00782              ELSE                                                 EL640
00783                  IF BPFNTERI NUMERIC AND                             CL*40
00784                    (BPFNTERI > 0 AND < 25)                           CL*40
00785                      MOVE PF-VALUES (BPFNTERI) TO EIBAID          EL640
00786                  ELSE                                             EL640
00787                      MOVE ER-0029  TO EMI-ERROR                   EL640
00788                      MOVE AL-UNBOF TO BPFNTERA                    EL640
00789                      MOVE -1       TO BPFNTERL                    EL640
00790                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL640
00791                      GO TO 8200-SEND-DATAONLY.                    EL640
00792      EJECT                                                        EL640
00793  0300-CHECK-PFKEYS.                                               EL640
00794      IF EIBAID = DFHPF23                                          EL640
00795          GO TO 8810-PF23.                                         EL640
00796                                                                   EL640
00797      IF EIBAID = DFHPF24                                          EL640
00798          GO TO 9200-RETURN-MAIN-MENU.                             EL640
00799                                                                   EL640
00800      IF EIBAID = DFHPF12                                          EL640
00801          GO TO 9500-PF12.                                         EL640
00802                                                                   EL640
00803 ******************************************************************EL640
00804 *       IF CARRIER OR ACCOUNT SECURITY ALLOW ONLY                *EL640
00805 *            BILLING TYPE OPTION ONE OR TWO.                     *EL640
00806 *                        04/27/84                                *EL640
00807 ******************************************************************EL640
00808                                                                   EL640
00809      IF PI-NO-CARRIER-SECURITY OR                                    CL*40
00810         PI-NO-ACCOUNT-SECURITY                                       CL*40
00811           GO TO 0305-CHECK-PFKEYS.                                   CL*40
00812                                                                   EL640
00813      IF EIBAID = DFHENTER OR DFHPF6                                  CL*40
00814         NEXT SENTENCE                                                CL*40
00815        ELSE                                                       EL640
00816         GO TO 0320-INPUT-ERROR.                                   EL640
00817                                                                   EL640
00818  0305-CHECK-PFKEYS.                                                  CL*30
00819      IF EIBAID = DFHPF3                                           EL640
00820        AND PI-MAP-NAME = EL640A                                   EL640
00821          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT          EL640
00822          MOVE PI-CARRIER         TO PI-CR-CARRIER                 EL640
00823          MOVE PI-GROUPING        TO PI-CR-GROUPING                EL640
00824          MOVE PI-STATE           TO PI-CR-STATE                   EL640
00825          MOVE PI-ACCOUNT         TO PI-CR-ACCOUNT                 EL640
00826          MOVE XCTL-650           TO PGM-NAME                      EL640
00827          GO TO 9300-XCTL.                                         EL640
00828                                                                   EL640
00829      IF EIBAID = DFHPF4                                              CL*30
00830        AND PI-MAP-NAME = EL640A                                      CL*30
00831         NEXT SENTENCE                                                CL*30
00832      ELSE                                                            CL*30
00833         GO TO 0320-CONT-PFKEYS.                                      CL*30
00834                                                                      CL*30
00835      IF PI-CR-FIN-RESP NOT = SPACES                                  CL*30
00836          PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT           CL*30
00837          MOVE PI-COMP-CARRIER    TO PI-CR-CARRIER                    CL*30
00838          MOVE PI-COMP-GROUPING   TO PI-CR-GROUPING                   CL*30
00839          MOVE PI-COMP-FIN-RESP   TO PI-CR-FIN-RESP                   CL*30
00840          MOVE XCTL-652           TO PGM-NAME                         CL*30
00841          GO TO 9300-XCTL.                                            CL*30
00842                                                                      CL*30
00843      IF PI-SCR-FIN-RESP = SPACES OR LOW-VALUES                       CL*30
00844          GO TO 0315-PF4-ERROR.                                       CL*30
00845                                                                      CL*30
00846      PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT.              CL*30
00847                                                                      CL*30
00848      MOVE PI-SCR-CARRIER    TO PI-CR-CARRIER.                        CL*30
00849      MOVE PI-SCR-GROUPING   TO PI-CR-GROUPING.                       CL*30
00850      MOVE PI-SCR-FIN-RESP   TO PI-CR-FIN-RESP.                       CL*30
00851      MOVE PI-SCR-ACCOUNT    TO PI-CR-ACCOUNT.                        CL*30
00852      MOVE PI-SCR-TYPE       TO PI-CR-TYPE.                           CL*30
00853      MOVE 'Y'               TO PI-TRANSFER-SW.                       CL*30
00854                                                                      CL*30
00855      IF PI-ZERO-CARRIER  OR                                          CL*30
00856         PI-ZERO-CAR-GROUP                                            CL*30
00857          MOVE ZEROS              TO PI-CR-CARRIER.                   CL*30
00858                                                                      CL*30
00859      IF PI-ZERO-GROUPING  OR                                         CL*30
00860         PI-ZERO-CAR-GROUP                                            CL*30
00861          MOVE ZEROS              TO PI-CR-GROUPING.                  CL*30
00862                                                                      CL*30
00863      MOVE XCTL-652               TO PGM-NAME.                        CL*33
00864                                                                      CL*30
00865      GO TO 9300-XCTL.                                                CL*30
00866                                                                      CL*30
00867  0315-PF4-ERROR.                                                     CL*30
00868                                                                      CL*30
00869      MOVE ER-2407                TO EMI-ERROR.                       CL*33
00870      MOVE -1                     TO APFNTERL.                        CL*33
00871      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*30
00872      GO TO 8200-SEND-DATAONLY.                                       CL*30
00873                                                                      CL*30
00874  0320-CONT-PFKEYS.                                                   CL*30
00875      IF EIBAID = DFHPF6                                           EL640
00876        AND PI-MAP-NAME = EL640A                                   EL640
00877          MOVE LOW-VALUES         TO EL640AO                       EL640
00878          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT          EL640
00879          MOVE XCTL-6401          TO PGM-NAME                      EL640
00880          GO TO 9300-XCTL.                                         EL640
00881                                                                   EL640
00882      IF EIBAID = DFHPF7                                           EL640
00883        AND PI-MAP-NAME = EL640A                                   EL640
00884          IF PI-CR-FIN-RESP NOT = SPACES                           EL640
00885              PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT      EL640
00886              MOVE PI-COMP-CARRIER    TO PI-CR-CARRIER             EL640
00887              MOVE PI-COMP-GROUPING   TO PI-CR-GROUPING            EL640
00888              MOVE PI-COMP-FIN-RESP   TO PI-CR-FIN-RESP            EL640
00889              MOVE XCTL-PYAJ          TO PGM-NAME                     CL*40
00890              GO TO 9300-XCTL                                      EL640
00891          ELSE                                                     EL640
00892              IF PI-RETURN-TO-PROGRAM = XCTL-633 OR XCTL-635 OR       CL*35
00893                                        XCTL-633DMD                   CL*35
00894                  GO TO 9400-CLEAR                                    CL*30
00895              ELSE                                                    CL*30
00896                  MOVE ER-2411        TO EMI-ERROR                    CL*30
00897                  MOVE -1             TO APFNTERL                     CL*30
00898                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*30
00899                  GO TO 8200-SEND-DATAONLY.                           CL*30
00900                                                                   EL640
00901      IF EIBAID = DFHPF8                                           EL640
00902        AND PI-MAP-NAME = EL640A                                   EL640
00903            PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT        EL640
00904            MOVE SPACES         TO PI-CR-CONTROL-IN-PROGRESS       EL640
00905            MOVE XCTL-658       TO PGM-NAME                        EL640
00906            GO TO 9300-XCTL.                                       EL640
00907                                                                   EL640
00908      IF EIBAID = DFHPF1                                           EL640
00909        AND PI-MAP-NAME = EL640B                                   EL640
00910          GO TO 7500-PRODUCE-CHECK.                                EL640
00911                                                                      CL*22
00912      IF EIBAID = DFHPF5                                              CL*22
00913        AND PI-AR-PROCESSING                                          CL*22
00914          MOVE ER-3188            TO EMI-ERROR                        CL*22
00915          MOVE -1                 TO APFNTERL                         CL*22
00916          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*22
00917          GO TO 8200-SEND-DATAONLY.                                   CL*22
00918                                                                   EL640
00919      IF PI-COMPANY-ID = 'DMD'                                        CL*40
00920        IF EIBAID = DFHPF5                                            CL*40
00921          GO TO 0320-INPUT-ERROR.                                     CL*37
00922                                                                      CL*37
00923      IF EIBAID = DFHPF5                                              CL*37
00924        AND PI-MAP-NAME = EL640A                                   EL640
00925             MOVE EL640B     TO PI-MAP-NAME                        EL640
00926             GO TO 7000-PROCESS-CHECK.                                CL*14
00927                                                                   EL640
00928      IF PI-MAP-NAME = EL640A                                      EL640
00929          IF EIBAID = DFHENTER OR DFHPF3                              CL*11
00930              GO TO 0330-EDIT-DATA                                    CL*39
00931          ELSE                                                        CL*39
00932              GO TO 0320-INPUT-ERROR                                  CL*39
00933      ELSE                                                            CL*39
00934          IF PI-MAP-NAME = EL640B                                     CL*39
00935              IF EIBAID = DFHENTER                                    CL*39
00936                  EXEC CICS BIF DEEDIT                                CL*39
00937                      FIELD  (BCHKAMTI)                               CL*39
00938                      LENGTH (11)                                     CL*39
00939                  END-EXEC                                            CL*39
00940                  MOVE BCHKAMTI   TO  WORK-CHECK-AMT                  CL*39
00941                  MOVE 'B'        TO  WS-ENTERED-FROM-MAP             CL*39
00942                  GO TO 7000-PROCESS-CHECK.                           CL*39
00943                                                                   EL640
00944  0320-INPUT-ERROR.                                                EL640
00945      MOVE ER-0029                TO EMI-ERROR.                    EL640
00946      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL640
00947                                                                      CL*33
00948      IF PI-MAP-NAME = EL640A                                      EL640
00949          MOVE AL-UNBON           TO APFNTERA                      EL640
00950          MOVE -1                 TO APFNTERL                      EL640
00951      ELSE                                                         EL640
00952          MOVE AL-UNBON           TO BPFNTERA                      EL640
00953          MOVE -1                 TO BPFNTERL.                     EL640
00954                                                                      CL*33
00955      GO TO 8200-SEND-DATAONLY.                                    EL640
00956                                                                   EL640
00957      EJECT                                                        EL640
00958  0330-EDIT-DATA.                                                  EL640
00959      MOVE ABILTYPI               TO VALID-BILL-TYPE-VALUES.       EL640
00960                                                                      CL*11
00961      IF VALID-BILL-TYPE                                           EL640
00962         IF NOT MODIFY-CAP                                         EL640
00963            IF ABILTYPI = '1' OR '2'                               EL640
00964               NEXT SENTENCE                                       EL640
00965            ELSE                                                   EL640
00966               MOVE 'UPDATE'         TO SM-READ                    EL640
00967               PERFORM 9995-SECURITY-VIOLATION                     EL640
00968               MOVE ER-0070          TO EMI-ERROR                  EL640
00969               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            EL640
00970               GO TO 8100-SEND-INITIAL-MAP.                        EL640
00971                                                                   EL640
00972      MOVE SPACES                 TO PI-BILLING-COUNTS             EL640
00973                                     PI-LIMIT-BILLING-BTCHS.       EL640
00974                                                                   EL640
00975 ******************************************************************EL640
00976 *         SECURITY CHECK FOR CARRIER AND ACCOUNT.                *EL640
00977 *                     04/12/84                                   *EL640
00978 ******************************************************************EL640
00979                                                                   EL640
00980      IF PI-CARRIER-SECURITY > SPACES                                 CL*40
00981         IF ACARIERL > ZEROS                                          CL*40
00982             IF PI-CARRIER-SECURITY = ACARIERI                     EL640
00983                 MOVE AL-UANON    TO ACARIERA                         CL*33
00984             ELSE                                                     CL*33
00985                 MOVE -1          TO ACARIERL                         CL*33
00986                 MOVE AL-UABON    TO ACARIERA                         CL*33
00987                 MOVE ER-2370     TO EMI-ERROR                        CL*33
00988                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            CL*33
00989                                                                   EL640
00990  350-ERROR-CHECK.                                                 EL640
00991      IF EMI-ERROR = ZEROS                                            CL*11
00992          NEXT SENTENCE                                               CL*11
00993      ELSE                                                            CL*33
00994          GO TO 8200-SEND-DATAONLY.                                   CL*11
00995                                                                   EL640
00996      IF AACCTL > ZEROS                                               CL*40
00997          MOVE AL-UANON           TO AACCTA                        EL640
00998          MOVE AACCTI             TO PI-SAV-ACCT                   EL640
00999                                     PI-CR-ACCOUNT                 EL640
01000      ELSE                                                         EL640
01001          MOVE -1                 TO AACCTL                        EL640
01002          MOVE AL-UABON           TO AACCTA                        EL640
01003          MOVE ER-0197            TO EMI-ERROR                     EL640
01004          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL640
01005                                                                   EL640
01006      IF ACARIERL > ZEROS                                             CL*40
01007          MOVE AL-UANON           TO ACARIERA                      EL640
01008          MOVE ACARIERI           TO PI-SAV-CARR                   EL640
01009                                     WS-CARRIER                    EL640
01010      ELSE                                                         EL640
01011          IF NOT ST-ACCNT-CNTL AND NOT ACCNT-CNTL                  EL640
01012              MOVE -1             TO ACARIERL                      EL640
01013              MOVE AL-UABON       TO ACARIERA                      EL640
01014              MOVE ER-0194        TO EMI-ERROR                     EL640
01015              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL640
01016                                                                   EL640
01017      IF AGROUPL > ZEROS                                              CL*40
01018          MOVE AL-UANON           TO AGROUPA                       EL640
01019          MOVE AGROUPI            TO PI-SAV-GROUP                  EL640
01020                                     WS-COMP                       EL640
01021      ELSE                                                         EL640
01022          IF CARR-GROUP-ST-ACCNT-CNTL                              EL640
01023              MOVE -1             TO AGROUPL                       EL640
01024              MOVE AL-UABON       TO AGROUPA                       EL640
01025              MOVE ER-0195        TO EMI-ERROR                     EL640
01026              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL640
01027                                                                   EL640
01028      IF ASTATEL > ZEROS                                              CL*40
01029          MOVE AL-UANON           TO ASTATEA                       EL640
01030          MOVE ASTATEI            TO PI-SAV-STATE                  EL640
01031      ELSE                                                         EL640
01032          IF NOT ACCNT-CNTL AND NOT CARR-ACCNT-CNTL                EL640
01033              MOVE -1             TO ASTATEL                       EL640
01034              MOVE AL-UABON       TO ASTATEA                       EL640
01035              MOVE ER-0196        TO EMI-ERROR                     EL640
01036              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL640
01037                                                                   EL640
01038      MOVE ABILTYPI               TO VALID-BILL-TYPE-VALUES.       EL640
01039                                                                      CL*11
01040      IF VALID-BILL-TYPE                                           EL640
01041          MOVE AL-UANON           TO ABILTYPA                      EL640
01042          MOVE ABILTYPI           TO PI-BILL-TYPE                  EL640
01043      ELSE                                                         EL640
01044          MOVE -1                 TO ABILTYPL                      EL640
01045          MOVE ER-2249            TO EMI-ERROR                     EL640
01046          MOVE AL-UABON           TO ABILTYPA                      EL640
01047          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL640
01048                                                                      CL*14
01049      IF PI-AR-PROCESSING                                             CL*14
01050         IF PI-PREVIEW                                                CL*14
01051            NEXT SENTENCE                                             CL*14
01052         ELSE                                                         CL*14
01053            MOVE -1               TO ABILTYPL                         CL*14
01054            MOVE ER-3145          TO EMI-ERROR                        CL*14
01055            MOVE AL-UABON         TO ABILTYPA                         CL*14
01056            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL*14
01057                                                                   EL640
01058      IF APRODSWL NOT = ZEROS                                      EL640
01059          IF PI-PREVIEW                                            EL640
01060              IF APRODSWI = 'Y' OR 'N'                             EL640
01061                  MOVE AL-UANON   TO APRODSWA                      EL640
01062              ELSE                                                 EL640
01063                  MOVE -1         TO APRODSWL                      EL640
01064                  MOVE ER-2436    TO EMI-ERROR                     EL640
01065                  MOVE AL-UABON   TO APRODSWA                      EL640
01066                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL640
01067          ELSE                                                     EL640
01068              MOVE -1             TO APRODSWL                      EL640
01069              MOVE ER-2434        TO EMI-ERROR                     EL640
01070              MOVE AL-UABON       TO APRODSWA                      EL640
01071              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL640
01072      ELSE                                                         EL640
01073          IF PI-PREVIEW                                            EL640
01074              MOVE -1             TO APRODSWL                      EL640
01075              MOVE ER-2435        TO EMI-ERROR                     EL640
01076              MOVE AL-UABON       TO APRODSWA                      EL640
01077              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL640
01078                                                                   EL640
01079      IF ABILERRI = 'Y' OR 'N'                                     EL640
01080          MOVE ABILERRI           TO PI-BILL-ERRS                  EL640
01081          MOVE AL-UANON           TO ABILERRA                      EL640
01082      ELSE                                                         EL640
01083          IF NOT PI-VOID-BILL                                      EL640
01084              MOVE -1             TO ABILERRL                      EL640
01085              MOVE ER-2250        TO EMI-ERROR                     EL640
01086              MOVE AL-UABON       TO ABILERRA                      EL640
01087              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL640
01088                                                                   EL640
01089      IF ABTCH1L NOT = ZEROS                                       EL640
01090          MOVE AL-UANON           TO ABTCH1A                       EL640
01091          MOVE 'Y'                TO LIMIT-BILLING-SW              EL640
01092          MOVE ABTCH1I            TO PI-BILLING-BATCHES (1).       EL640
01093                                                                   EL640
01094      IF ABTCH2L NOT = ZEROS                                       EL640
01095          MOVE AL-UANON           TO ABTCH2A                       EL640
01096          MOVE 'Y'                TO LIMIT-BILLING-SW              EL640
01097          MOVE ABTCH2I            TO PI-BILLING-BATCHES (2).       EL640
01098                                                                   EL640
01099      IF ABTCH3L NOT = ZEROS                                       EL640
01100          MOVE AL-UANON           TO ABTCH3A                       EL640
01101          MOVE 'Y'                TO LIMIT-BILLING-SW              EL640
01102          MOVE ABTCH3I            TO PI-BILLING-BATCHES (3).       EL640
01103                                                                   EL640
01104      IF EMI-ERROR = ZEROS                                         EL640
01105          GO TO 1000-BILLING-PROCESS.                              EL640
01106                                                                   EL640
01107      GO TO 8200-SEND-DATAONLY.                                    EL640
01108      EJECT                                                        EL640
01109  0400-CLEAR-PI.                                                      CL*34
01110      MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.            CL*40
01111                                                                      CL*40
01112      MOVE ZEROS                  TO PI-BAL-FRWD-GA  PI-BAL-FRWD      CL*34
01113                                     PI-PREMIUM      PI-REMITTED      CL*34
01114                                     PI-TOT-ISS-COMP                  CL*34
01115                                     PI-TOT-CAN-COMP                  CL*34
01116                                     PI-ADJUSTMENTS  PI-DISBURSED     CL*34
01117                                     PI-END-BAL      PI-LF-ISS-COMP   CL*34
01118                                     PI-AH-ISS-COMP  PI-LF-CAN-COMP   CL*34
01119                                     PI-AH-CAN-COMP.                  CL*34
01120                                                                      CL*34
01121      MOVE SPACES                 TO PI-BATCH (1)                     CL*34
01122                                     PI-BATCH (2)                     CL*34
01123                                     PI-BATCH (3)                     CL*34
01124                                     PI-BATCH (4)                     CL*34
01125                                     PI-BATCH (5)                     CL*34
01126                                     PI-BATCH (6).                    CL*34
01127                                                                      CL*34
01128      MOVE SPACES                 TO PI-BILLING-BATCHES (1)           CL*34
01129                                     PI-BILLING-BATCHES (2)           CL*34
01130                                     PI-BILLING-BATCHES (3).          CL*34
01131                                                                      CL*34
01132      MOVE ZEROS                  TO PI-BILLED (1) PI-PREV (1)        CL*34
01133                                     PI-NOBILL (1)                    CL*34
01134                                     PI-PREM (1)   PI-COMM (1)        CL*36
01135                                     PI-NON-PREM (1)                  CL*36
01136                                     PI-NON-COMM (1)                  CL*36
01137                                     PI-BILLED (2) PI-PREV (2)        CL*34
01138                                     PI-NOBILL (2)                    CL*34
01139                                     PI-PREM (2)   PI-COMM (2)        CL*36
01140                                     PI-NON-PREM (2)                  CL*36
01141                                     PI-NON-COMM (2)                  CL*36
01142                                     PI-BILLED (3) PI-PREV (3)        CL*34
01143                                     PI-NOBILL (3)                    CL*34
01144                                     PI-PREM (3)   PI-COMM (3)        CL*36
01145                                     PI-NON-PREM (3)                  CL*36
01146                                     PI-NON-COMM (3)                  CL*36
01147                                     PI-BILLED (4) PI-PREV (4)        CL*34
01148                                     PI-NOBILL (4)                    CL*34
01149                                     PI-PREM (4)   PI-COMM (4)        CL*36
01150                                     PI-NON-PREM (4)                  CL*36
01151                                     PI-NON-COMM (4)                  CL*36
01152                                     PI-BILLED (5) PI-PREV (5)        CL*34
01153                                     PI-NOBILL (5)                    CL*34
01154                                     PI-PREM (5)   PI-COMM (5)        CL*36
01155                                     PI-NON-PREM (5)                  CL*36
01156                                     PI-NON-COMM (5)                  CL*36
01157                                     PI-BILLED (6) PI-PREV (6)        CL*34
01158                                     PI-NOBILL (6)                    CL*36
01159                                     PI-PREM (6)   PI-COMM (6)        CL*36
01160                                     PI-NON-PREM (6)                  CL*36
01161                                     PI-NON-COMM (6).                 CL*36
01162                                                                      CL*34
01163  0499-EXIT.                                                          CL*34
01164       EXIT.                                                          CL*34
01165                                                                      CL*34
01166  0500-CREATE-TEMP-STORAGE.                                        EL640
01167                                                                      CL*30
01168      IF PI-BAL-FRWD NOT NUMERIC                                   EL640
01169          MOVE ZEROS              TO PI-BAL-FRWD                   EL640
01170                                     PI-PREMIUM                    EL640
01171                                     PI-REMITTED                   EL640
01172                                     PI-TOT-ISS-COMP               EL640
01173                                     PI-TOT-CAN-COMP               EL640
01174                                     PI-ADJUSTMENTS                   CL*33
01175                                     PI-DISBURSED                  EL640
01176                                     PI-END-BAL.                   EL640
01177                                                                      CL*33
01178      EXEC CICS WRITEQ TS                                          EL640
01179          QUEUE  (QID)                                                CL*11
01180          FROM   (PROGRAM-INTERFACE-BLOCK)                            CL*11
01181          LENGTH (PI-COMM-LENGTH)                                     CL*11
01182      END-EXEC.                                                       CL*11
01183                                                                   EL640
01184  0590-EXIT.                                                       EL640
01185       EXIT.                                                       EL640
01186                                                                   EL640
01187  0600-RECOVER-TEMP-STORAGE.                                       EL640
01188      EXEC CICS READQ TS                                           EL640
01189          QUEUE  (QID)                                                CL*11
01190          INTO   (PROGRAM-INTERFACE-BLOCK)                            CL*11
01191          LENGTH (PI-COMM-LENGTH)                                     CL*11
01192      END-EXEC.                                                       CL*11
01193                                                                   EL640
01194      PERFORM 0800-DELETE-TS THRU 0890-EXIT.                       EL640
01195                                                                      CL*37
01196      MOVE PI-CR-CARRIER    TO PI-SCR-CARRIER.                        CL*37
01197      MOVE PI-CR-GROUPING   TO PI-SCR-GROUPING.                       CL*37
01198      MOVE PI-CR-FIN-RESP   TO PI-SCR-FIN-RESP.                       CL*37
01199      MOVE PI-CR-ACCOUNT    TO PI-SCR-ACCOUNT.                        CL*37
01200      MOVE PI-CR-TYPE       TO PI-SCR-TYPE.                           CL*37
01201                                                                      CL*30
01202      MOVE PI-SCRN-CONTROL TO W-TRANSFER-CONTROL.                     CL*30
01203                                                                   EL640
01204  0690-EXIT.                                                       EL640
01205       EXIT.                                                       EL640
01206                                                                   EL640
01207  0800-DELETE-TS.                                                  EL640
01208      EXEC CICS HANDLE CONDITION                                   EL640
01209          QIDERR (0890-EXIT)                                          CL*11
01210      END-EXEC.                                                       CL*11
01211                                                                      CL*11
01212      EXEC CICS DELETEQ TS                                         EL640
01213          QUEUE  (QID)                                                CL*11
01214      END-EXEC.                                                       CL*11
01215                                                                   EL640
01216  0890-EXIT.                                                       EL640
01217       EXIT.                                                       EL640
01218      EJECT                                                        EL640
01219  1000-BILLING-PROCESS.                                            EL640
01220 *                *******************************************      EL640
01221 *                *  THIS SECTION PROCESSES THE PENDING     *      EL640
01222 *                *  BUSINESS AND PAYMNTS/ADJMNTS FILES.    *      EL640
01223 *                *  THE PREMIUM AMT WRITTEN IS EXTRACTED   *      EL640
01224 *                *  FROM THE PENDING BUSINESS FILE.        *      EL640
01225 *                *******************************************      EL640
01226                                                                   EL640
01227      MOVE PI-COMPANY-CD          TO ERPNDB-CO-CD                  EL640
01228                                     ERPNDB-CO-CD-A1               EL640
01229                                     ERCOMP-COMP-CD                EL640
01230                                     ERPYAJ-COMP-CD                EL640
01231                                     ERACCT-P-CO-CD                EL640
01232                                     ERACCT-A-CO-CD.               EL640
01233                                                                      CL*11
01234      MOVE WS-CURRENT-DATE-EDIT   TO BI-HD-RUN-DT.                 EL640
01235                                                                      CL*11
01236      MOVE ZEROS                  TO PI-BAL-FRWD                   EL640
01237                                     PI-PREMIUM                    EL640
01238                                     PI-REMITTED                   EL640
01239                                     PI-TOT-ISS-COMP               EL640
01240                                     PI-TOT-CAN-COMP               EL640
01241                                     PI-ADJUSTMENTS                   CL*33
01242                                     PI-DISBURSED                  EL640
01243                                     PI-END-BAL.                   EL640
01244                                                                   EL640
01245      IF PI-VOID-BILL                                              EL640
01246          GO TO 2000-VOID-BILLING.                                 EL640
01247                                                                   EL640
01248      MOVE SPACES                 TO ELCNTL-KEY.                   EL640
01249      MOVE '1'                    TO ELCNTL-REC-TYPE.              EL640
01250      PERFORM 6100-READ-CONTROL-FILE THRU 6120-EXIT.               EL640
01251                                                                   EL640
01252      IF EMI-ERROR NOT = ZEROS                                     EL640
01253          GO TO 8200-SEND-DATAONLY.                                EL640
01254                                                                   EL640
01255      IF CF-ACCOUNT-MSTR-MAINT-DT      = LOW-VALUES  OR               CL*40
01256         CF-COMPENSATION-MSTR-MAINT-DT = LOW-VALUES                   CL*40
01257          MOVE ER-2571            TO EMI-ERROR                     EL640
01258          MOVE -1                 TO APFNTERL                      EL640
01259          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL640
01260          GO TO 8200-SEND-DATAONLY.                                EL640
01261                                                                   EL640
01262      IF CF-FORMS-PRINTER-ID = SPACES                              EL640
01263         IF ABILTYPI = '3' OR '4'                                     CL*40
01264           MOVE 2590               TO EMI-ERROR                       CL*40
01265           MOVE -1                 TO ABILTYPL                        CL*40
01266           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                   CL*40
01267           GO TO 8200-SEND-DATAONLY.                                  CL*40
01268                                                                   EL640
01269      MOVE CF-CL-MAIL-TO-NAME     TO BI-HD-CO.                     EL640
01270      MOVE CF-CR-MONTH-END-DT     TO DC-BIN-DATE-1.                EL640
01271      MOVE SPACE                  TO DC-OPTION-CODE.               EL640
01272      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL640
01273      MOVE DC-GREG-DATE-1-EDIT    TO PI-MONTH-END-DATE.            EL640
01274      MOVE DC-GREG-DATE-1-ALPHA   TO BI-HD-BILL-DT.                EL640
01275      MOVE SPACE                  TO PI-DATA-BILLED-SW.            EL640
01276                                                                      CL*11
01277      PERFORM 4000-PNDB-START-BROWSE THRU 4090-EXIT.               EL640
01278                                                                      CL*11
01279      IF PNDB-EOF                                                  EL640
01280          GO TO 1100-PYAJ-BILLING-PROCESS.                         EL640
01281                                                                   EL640
01282  1010-PNDB-BILLING-PROCESS-LOOP.                                  EL640
01283      PERFORM 4100-PNDB-READ-NEXT THRU 4190-EXIT.                  EL640
01284                                                                      CL*11
01285      IF PNDB-EOF                                                  EL640
01286          PERFORM 3500-TOTAL-OUT-DETAIL THRU 3599-EXIT             EL640
01287          GO TO 1100-PYAJ-BILLING-PROCESS.                         EL640
01288                                                                      CL*11
01289      IF PB-COMPANY-CD-A1 = PI-COMPANY-CD  AND                        CL*11
01290         PB-CARRIER       = PI-SAV-CARR    AND                        CL*11
01291         PB-GROUPING      = PI-SAV-GROUP   AND                        CL*11
01292         PB-STATE         = PI-SAV-STATE   AND                        CL*11
01293         PB-ACCOUNT       = PI-SAV-ACCT                               CL*11
01294          NEXT SENTENCE                                            EL640
01295      ELSE                                                         EL640
01296          PERFORM 3500-TOTAL-OUT-DETAIL THRU 3599-EXIT             EL640
01297          GO TO 1100-PYAJ-BILLING-PROCESS.                         EL640
01298                                                                   EL640
01299      IF (PB-ISSUE) AND                                               CL*17
01300         (PB-I-POLICY-IS-REISSUE OR PB-I-REIN-ONLY OR                 CL*21
122002         PB-I-POLICY-IS-MONTHLY OR
01301          PB-I-POLICY-IS-DECLINED OR PB-I-POLICY-IS-VOIDED OR         CL*21
01302          PB-I-UNDERWRITE-POLICY)                                     CL*21
01303           GO TO 1010-PNDB-BILLING-PROCESS-LOOP                    EL640
01304      ELSE                                                         EL640
01305         IF (PB-CANCELLATION)                                         CL*21
01306            AND  (PB-CI-LF-POLICY-IS-REISSUE                          CL*21
01307              OR  PB-CI-AH-POLICY-IS-REISSUE                          CL*21
122002             OR  PB-CI-LF-POLICY-IS-MONTHLY
122002             OR  PB-CI-AH-POLICY-IS-MONTHLY
01308              OR  PB-CI-LF-POLICY-IS-DECLINED                         CL*21
01309              OR  PB-CI-AH-POLICY-IS-VOID                             CL*21
01310              OR  PB-CI-LF-REIN-ONLY                                  CL*21
01311              OR  PB-CI-AH-REIN-ONLY)                                 CL*21
01312              GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                EL640
01313                                                                   EL640
01314      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL640
01315          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                    EL640
01316                                                                   EL640
01317      IF PB-RECORD-ON-HOLD OR                                         CL*11
01318         PB-RECORD-RETURNED                                           CL*11
01319          PERFORM 1040-UPDATE-BILLING-STATISTICS THRU 1049-EXIT       CL*40
01320          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                    EL640
01321                                                                   EL640
01322      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS                             EL640
01323          IF PB-ENTRY-REVERSED                                     EL640
01324              GO TO 1010-PNDB-BILLING-PROCESS-LOOP                 EL640
01325          ELSE                                                     EL640
01326              IF PI-REBILLING                                      EL640
01327                  PERFORM 4200-PNDB-REWRITE THRU 4290-EXIT         EL640
01328                  GO TO 1010-PNDB-BILLING-PROCESS-LOOP.            EL640
01329                                                                   EL640
01330      IF PB-BILLED-DT = LOW-VALUES                                 EL640
01331        OR PI-TOT-REBILL                                           EL640
01332        OR PB-ALT-CHG-SEQ-NO NOT = ZEROS                           EL640
01333          NEXT SENTENCE                                            EL640
01334      ELSE                                                         EL640
01335          IF NOT PB-BATCH-TRAILER                                  EL640
01336              PERFORM 1040-UPDATE-BILLING-STATISTICS THRU 1049-EXIT   CL*40
01337              GO TO 1010-PNDB-BILLING-PROCESS-LOOP                 EL640
01338          ELSE                                                     EL640
01339              GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                EL640
01340                                                                   EL640
01341      IF LIMIT-BILLING                                             EL640
01342          IF PB-ENTRY-BATCH = ABTCH1I OR                              CL*40
01343                              ABTCH2I OR                              CL*40
01344                              ABTCH3I                                 CL*40
01345              NEXT SENTENCE                                        EL640
01346          ELSE                                                     EL640
01347              GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                EL640
01348                                                                   EL640
01349      IF PB-FATAL-ERRORS  OR                                          CL*40
01350         PB-UNFORCED-ERRORS                                           CL*40
01351          IF ABILERRI = 'N'                                        EL640
01352              PERFORM 8900-SYNCPOINT-ROLLBACK THRU 8900-EXIT       EL640
01353              PERFORM 3940-BILL-GENERIC-DELETE THRU 3940-EXIT      EL640
01354              MOVE ER-2570        TO EMI-ERROR                     EL640
01355              MOVE -1             TO ABILERRL                      EL640
01356              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL640
01357              SET EMI-INDX TO 1                                       CL*40
01358              IF EMI-ERROR-NUMBER (EMI-INDX) = ER-2570             EL640
01359                  MOVE EMI-ERROR-TEXT (EMI-INDX) TO WS-ERROR-TEXT  EL640
01360                  MOVE PB-ENTRY-BATCH            TO WS-ERROR-BATCH    CL*40
01361                  MOVE WS-ERROR-MSG TO EMI-ERROR-TEXT (EMI-INDX)   EL640
01362                  GO TO 8200-SEND-DATAONLY                         EL640
01363              ELSE                                                 EL640
01364                  SET EMI-INDX    TO 2                             EL640
01365                  MOVE EMI-ERROR-TEXT (EMI-INDX) TO WS-ERROR-TEXT  EL640
01366                  MOVE PB-ENTRY-BATCH            TO WS-ERROR-BATCH    CL*40
01367                  MOVE WS-ERROR-MSG TO EMI-ERROR-TEXT (EMI-INDX)   EL640
01368                  GO TO 8200-SEND-DATAONLY                         EL640
01369          ELSE                                                     EL640
01370              PERFORM 1040-UPDATE-BILLING-STATISTICS THRU 1049-EXIT   CL*40
01371              GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                EL640
01372                                                                   EL640
01373      IF PB-BATCH-TRAILER                                          EL640
01374          IF PI-UPDATE-FILES                                       EL640
01375              PERFORM 1050-BILL-BATCH-TRAILER-CHK THRU 1059-EXIT   EL640
01376              IF BILL-BATCH-TRAILER                                EL640
01377                  MOVE SPACE           TO BILL-BATCH-TRAILER-SW       CL*40
01378                  MOVE WS-CURRENT-DATE TO PB-BILLED-DT             EL640
01379                  PERFORM 4200-PNDB-REWRITE THRU 4290-EXIT         EL640
01380                  GO TO 1010-PNDB-BILLING-PROCESS-LOOP             EL640
01381              ELSE                                                 EL640
01382                  GO TO 1010-PNDB-BILLING-PROCESS-LOOP             EL640
01383          ELSE                                                     EL640
01384              GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                EL640
01385                                                                   EL640
01386      MOVE 'Y'                    TO PI-DATA-BILLED-SW.            EL640
01387                                                                   EL640
01388      PERFORM 1300-CHECK-OVERRIDE-PREM THRU 1399-EXIT.                CL**8
01389                                                                   EL640
01390      IF PB-CERT-EFF-DT > ERACCT-A-EXP-DATE                           CL*40
01391          MOVE SPACE              TO WS-PROCESS-SW                    CL*11
01392          MOVE PB-CERT-EFF-DT     TO PI-SAV-EXP-DT                 EL640
01393          PERFORM 4300-READ-ACCOUNT-MASTER  THRU 4390-EXIT            CL*11
01394          PERFORM 1020-CHK-IF-AGENT-IS-SAME THRU 1029-EXIT         EL640
01395          IF EMI-ERROR NOT = ZEROS                                 EL640
01396              IF PI-UPDATE-FILES                                   EL640
01397                OR (PI-PREVIEW AND APRODSWI = 'Y')                 EL640
01398                  PERFORM 8900-SYNCPOINT-ROLLBACK THRU 8900-EXIT   EL640
01399                  IF EMI-ERROR NOT = '2403'                        EL640
01400                      PERFORM 3940-BILL-GENERIC-DELETE             EL640
01401                              THRU 3940-EXIT                       EL640
01402                      GO TO 8200-SEND-DATAONLY                     EL640
01403                  ELSE                                             EL640
01404                      GO TO 8200-SEND-DATAONLY                     EL640
01405              ELSE                                                 EL640
01406                  GO TO 8200-SEND-DATAONLY.                        EL640
01407                                                                   EL640
01408      PERFORM 1040-UPDATE-BILLING-STATISTICS THRU 1049-EXIT.       EL640
01409                                                                   EL640
01410      IF DO-NOT-BILL-THIS-ACCT                                     EL640
01411          GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                    EL640
01412                                                                   EL640
01413      PERFORM 1030-COMPUTE-COMMISSION-TOTALS THRU 1039-EXIT.       EL640
01414                                                                      CL*33
01415      IF EMI-ERROR NOT = ZEROS                                     EL640
01416          IF PI-UPDATE-FILES                                       EL640
01417            OR (PI-PREVIEW AND APRODSWI = 'Y')                     EL640
01418              PERFORM 8900-SYNCPOINT-ROLLBACK THRU 8900-EXIT       EL640
01419              PERFORM 3940-BILL-GENERIC-DELETE THRU 3940-EXIT      EL640
01420              GO TO 8200-SEND-DATAONLY                             EL640
01421          ELSE                                                     EL640
01422              GO TO 8200-SEND-DATAONLY.                            EL640
01423                                                                   EL640
01424      IF PI-UPDATE-FILES                                           EL640
01425        OR (PI-PREVIEW AND APRODSWI = 'Y')                         EL640
01426          MOVE 'BD'               TO BILLING-DETAIL-TYPE           EL640
01427          PERFORM 3000-WRITE-BILLING-DETAIL THRU 3990-EXIT.        EL640
01428                                                                      CL*11
01429      IF PI-BILL OR PI-REBILLING                                   EL640
01430          PERFORM 4200-PNDB-REWRITE THRU 4290-EXIT.                EL640
01431                                                                   EL640
01432      GO TO 1010-PNDB-BILLING-PROCESS-LOOP.                        EL640
01433      EJECT                                                        EL640
01434  1020-CHK-IF-AGENT-IS-SAME.                                       EL640
01435      MOVE +1 TO ACCOM-SUB.                                        EL640
01436                                                                      CL*11
01437  1025-FIND-AGENT-LOOP.                                            EL640
01438      IF AM-COM-TYP (ACCOM-SUB) = 'C' OR 'D' OR 'F' OR 'S'            CL*33
01439          NEXT SENTENCE                                            EL640
01440      ELSE                                                         EL640
01441          ADD +1 TO ACCOM-SUB                                         CL*40
01442          IF ACCOM-SUB > 10                                           CL*40
01443              MOVE -1             TO AACCTL                        EL640
01444              MOVE ER-2404        TO EMI-ERROR                     EL640
01445              MOVE AL-UABON       TO AACCTA                        EL640
01446              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL640
01447          ELSE                                                     EL640
01448              GO TO 1025-FIND-AGENT-LOOP.                          EL640
01449                                                                   EL640
01450      IF AM-AGT (ACCOM-SUB) NOT = PI-SAV-ACCT                      EL640
01451          IF FIRST-TIME                                            EL640
01452              MOVE -1             TO AACCTL                        EL640
01453              MOVE ER-2405        TO EMI-ERROR                     EL640
01454              MOVE AL-UABON       TO AACCTA                        EL640
01455              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL640
01456              GO TO 1029-EXIT                                      EL640
01457          ELSE                                                     EL640
01458              MOVE 'Y'            TO WS-PROCESS-SW                 EL640
01459              GO TO 1029-EXIT.                                     EL640
01460                                                                   EL640
01461      IF FIRST-TIME                                                EL640
01462          MOVE AM-AGT (AM-REMIT-TO) TO PI-SAV-REMIT-TO             EL640
01463                                       PI-CR-FIN-RESP                 CL**4
01464          MOVE 'A'                  TO PI-CR-TYPE                     CL**4
01465          PERFORM 6000-READ-COMP-MASTER THRU 6090-EXIT             EL640
01466          MOVE SPACE                TO FIRST-TIME-SW                  CL**4
01467          IF EMI-ERROR NOT = ZEROS                                 EL640
01468              GO TO 1029-EXIT.                                     EL640
01469                                                                   EL640
01470      IF AM-AGT (AM-REMIT-TO) = PI-SAV-REMIT-TO                    EL640
01471          NEXT SENTENCE                                            EL640
01472      ELSE                                                         EL640
01473          MOVE 'Y'                TO WS-PROCESS-SW                 EL640
01474          MOVE ER-2408            TO EMI-ERROR                     EL640
01475          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL640
01476          PERFORM 8900-SYNCPOINT-ROLLBACK THRU 8900-EXIT              CL*33
01477          PERFORM 3940-BILL-GENERIC-DELETE  THRU  3940-EXIT           CL*33
01478          MOVE -1                 TO AACCTL                           CL*33
01479          GO TO 8200-SEND-DATAONLY.                                   CL*33
01480                                                                      CL*33
01481      MOVE AM-NAME                TO PI-ACCT-NAME.                 EL640
01482                                                                   EL640
01483      IF PI-BILL OR PI-REBILLING                                   EL640
01484          PERFORM 4400-ERACCT-REWRITE THRU 4490-EXIT.              EL640
01485                                                                      CL*11
01486  1029-EXIT.                                                       EL640
01487      EXIT.                                                        EL640
01488      EJECT                                                        EL640
01489  1030-COMPUTE-COMMISSION-TOTALS.                                  EL640
01490      IF PB-CANCELLATION                                           EL640
01491          GO TO 1035-COMPUTE-CANCEL-COMM.                          EL640
01492                                                                   EL640
01493      COMPUTE PI-PREMIUM = (WS-I-LF-PREMIUM-AMT +                  EL640
01494          WS-I-AH-PREMIUM-AMT) + PI-PREMIUM.                       EL640
01495      COMPUTE PI-LF-ISS-COMP ROUNDED =                             EL640
01496          (WS-I-LF-PREMIUM-AMT * PB-I-LIFE-COMMISSION).            EL640

           MOVE SPACES                 TO WS-AH-CATEGORY
           IF (PI-COMPANY-ID = 'DCC')
              AND (PB-VALID-AH)
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '5'                 TO ELCNTL-REC-TYPE
              MOVE PB-I-AH-BENEFIT-CD  TO ELCNTL-ACCESS (3:2)
                                          CLAS-LOOK
              MOVE +0                  TO ELCNTL-SEQ-NO
              PERFORM 6130-FIND-BENEFIT-CD
                                       THRU 6140-EXIT
           END-IF

070805     IF WS-AH-CATEGORY = 'G'
070805        COMPUTE PI-AH-ISS-COMP ROUNDED = 
070805           WS-I-AH-PREMIUM-AMT - PB-I-LF-ALT-PREMIUM-AMT
070805              - PB-I-ADDL-CLP
070805     ELSE
01497         COMPUTE PI-AH-ISS-COMP ROUNDED =
01498             WS-I-AH-PREMIUM-AMT * PB-I-AH-COMMISSION
070805     END-IF
           
01499      COMPUTE PI-TOT-ISS-COMP = PI-TOT-ISS-COMP +                  EL640
01500                                PI-LF-ISS-COMP +                   EL640
01501                                PI-AH-ISS-COMP.                    EL640
01502      GO TO 1039-EXIT.                                             EL640
01503                                                                   EL640
01504  1035-COMPUTE-CANCEL-COMM.                                        EL640
01505                                                                      CL*22
           MOVE SPACES                 TO WS-AH-CATEGORY

           IF (PI-COMPANY-ID = 'DCC')
              AND (PB-CI-AH-BENEFIT-CD NOT = SPACES AND ZEROS)
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '5'                 TO ELCNTL-REC-TYPE
              MOVE PB-CI-AH-BENEFIT-CD TO ELCNTL-ACCESS (3:2)
                                          CLAS-LOOK
              MOVE +0                  TO ELCNTL-SEQ-NO
              PERFORM 6130-FIND-BENEFIT-CD
                                       THRU 6140-EXIT
           END-IF

           IF WS-AH-CATEGORY = 'G'
              COMPUTE PI-PREMIUM = PI-PREMIUM -
                WS-C-LF-CANCEL-AMT
           ELSE
01506         COMPUTE PI-PREMIUM = PI-PREMIUM -
01507          (WS-C-LF-CANCEL-AMT + WS-C-AH-CANCEL-AMT)
           END-IF
01508                                                                      CL*22
01509      COMPUTE PI-LF-CAN-COMP   ROUNDED =                              CL*24
01510          (WS-C-LF-CANCEL-AMT * PB-CI-LIFE-COMMISSION) * -1.       EL640
01511                                                                      CL*22




070805     IF WS-AH-CATEGORY = 'G'
070805        COMPUTE CNC-FACT = PB-C-AH-CANCEL-AMT / 
070805           PB-CI-AH-PREMIUM-AMT
070805           COMPUTE PI-AH-CAN-COMP ROUNDED =
070805           (CNC-FACT * (PB-CI-AH-PREMIUM-AMT - 
070805           PB-CI-LF-ALT-PREMIUM-AMT
070805           - PB-CI-ADDL-CLP))
070805*          - PB-CI-ADDL-CLP)) * -1
070805     ELSE
01512         COMPUTE PI-AH-CAN-COMP   ROUNDED =
01513          (WS-C-AH-CANCEL-AMT * PB-CI-AH-COMMISSION) * -1
01514      END-IF

01515      MOVE PB-CERT-EFF-DT           TO  DC-BIN-DATE-1.                CL*40
01516      MOVE PB-C-LF-CANCEL-DT        TO  DC-BIN-DATE-2.                CL*40
01517      MOVE '1'                      TO  DC-OPTION-CODE.               CL*40
01518      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                       CL*40
01519      MOVE DC-ELAPSED-MONTHS        TO  MONTHS-DIFF-LF.               CL*40
01520                                                                      CL*22
01521      IF DC-ODD-DAYS-OVER > ZEROS                                     CL*40
01522          ADD +1 TO MONTHS-DIFF-LF.                                   CL*40
01523                                                                      CL*22
01524      MOVE PB-CERT-EFF-DT           TO  DC-BIN-DATE-1.                CL*40
01525      MOVE PB-C-AH-CANCEL-DT        TO  DC-BIN-DATE-2.                CL*40
01526      MOVE '1'                      TO  DC-OPTION-CODE.               CL*40
01527      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                       CL*40
01528      MOVE DC-ELAPSED-MONTHS        TO  MONTHS-DIFF-AH.               CL*40
01529                                                                      CL*22
01530      IF DC-ODD-DAYS-OVER > ZEROS                                     CL*40
01531          ADD +1 TO MONTHS-DIFF-AH.                                   CL*40
01532                                                                      CL*22
01533      PERFORM 4300-READ-ACCOUNT-MASTER THRU 4390-EXIT.                CL*40
01534                                                                      CL*22
01535      MOVE  +0                       TO  SUB.                         CL*22
01536      MOVE  'Y'                      TO  WS-CHARGEBACK-LF-SW          CL*25
01537                                         WS-CHARGEBACK-AH-SW.         CL*24
01538                                                                      CL*22
01539      MOVE ZEROS                     TO  WS-LF-CAN-COMP               CL*40
01540                                         WS-AH-CAN-COMP.              CL*40
01541  1035-ACCT-COMM-LOOP.                                                CL*22
01542                                                                      CL*22
01543      ADD +1 TO SUB.                                                  CL*40
01544                                                                      CL*22
01545      IF SUB > +10                                                    CL*40
01546          GO TO 1035-COMPUTE-TOT-CAN.                                 CL*40
01547                                                                      CL*22
01548      IF AM-COM-TYP (SUB) NOT = 'C' AND 'D' AND 'F' AND 'S'           CL*40
01549          GO TO 1035-ACCT-COMM-LOOP.                                  CL*40
01550                                                                      CL*23
01551      IF AM-COMM-CHARGEBACK (SUB) NOT NUMERIC                         CL*40
01552          MOVE  ZEROS            TO  AM-COMM-CHARGEBACK (SUB).        CL*23
01553                                                                      CL*22
01554      IF AM-COMM-CHARGEBACK (SUB) = '99'                              CL*40
01555          MOVE  'N'               TO  WS-CHARGEBACK-LF-SW             CL*25
01556          GO TO 1035-CHECK-AH-CHARGEBACK.                             CL*40
01557                                                                      CL*24
01558      IF (MONTHS-DIFF-LF > AM-COMM-CHARGEBACK (SUB))                  CL*40
01559                             AND                                      CL*22
01560         (AM-COMM-CHARGEBACK (SUB) NOT = ZEROS)                       CL*40
01561          MOVE  'N'               TO  WS-CHARGEBACK-LF-SW             CL*25
01562      ELSE                                                            CL*22
01563          COMPUTE WS-LF-CAN-COMP   ROUNDED =  WS-LF-CAN-COMP +        CL*25
01564             (WS-C-LF-CANCEL-AMT * PB-CI-LIFE-COMMISSION).            CL*24
01565                                                                      CL*24
01566  1035-CHECK-AH-CHARGEBACK.                                           CL*24
01567                                                                      CL*24
01568      IF AM-COMM-CHARGEBACK (SUB) = '99'                              CL*40
01569          MOVE  'N'                TO  WS-CHARGEBACK-AH-SW            CL*25
01570          GO TO  1035-ACCT-COMM-LOOP.                                 CL*26
01571                                                                      CL*22
01572      IF (MONTHS-DIFF-AH > AM-COMM-CHARGEBACK (SUB))                  CL*40
01573                             AND                                      CL*22
01574         (AM-COMM-CHARGEBACK (SUB) NOT = ZEROS)                       CL*40
01575          MOVE  'N'                TO  WS-CHARGEBACK-AH-SW            CL*26
01576      ELSE                                                            CL*22
              IF WS-AH-CATEGORY = 'G'
                 MOVE PI-AH-CAN-COMP   TO WS-AH-CAN-COMP
              ELSE
01577          COMPUTE WS-AH-CAN-COMP ROUNDED = WS-AH-CAN-COMP +           CL*40
01578             (WS-C-AH-CANCEL-AMT * PB-CI-AH-COMMISSION)
              END-IF
           END-IF
01579                                                                      CL*25
01580      GO TO 1035-ACCT-COMM-LOOP.                                      CL*25
01581                                                                      CL*22
01582  1035-COMPUTE-TOT-CAN.                                               CL*22
01583                                                                      CL*22
01584      COMPUTE PI-TOT-CAN-COMP = PI-TOT-CAN-COMP +                  EL640
01585                                WS-LF-CAN-COMP +                   EL640
01586                                WS-AH-CAN-COMP.                    EL640
01587                                                                      CL*25
01588  1039-EXIT.                                                       EL640
01589      EXIT.                                                        EL640
01590                                                                      CL*40
01591      EJECT                                                        EL640
01592  1040-UPDATE-BILLING-STATISTICS.                                  EL640
01593      SET PINDX                   TO 1.                            EL640
01594                                                                      CL*11
01595  1041-SEARCH-LOOP.                                                EL640
01596      IF PI-NON-PREM (PINDX) NOT NUMERIC                              CL*36
01597         MOVE ZEROS               TO PI-NON-PREM (PINDX).             CL*36
01598      IF WS-NON-PREM  NOT NUMERIC                                     CL*36
01599         MOVE ZEROS               TO WS-NON-PREM.                     CL*36
01600      IF PI-NON-COMM (PINDX) NOT NUMERIC                              CL*36
01601         MOVE ZEROS               TO PI-NON-COMM (PINDX).             CL*36
01602      IF WS-NON-COMM  NOT NUMERIC                                     CL*36
01603         MOVE ZEROS               TO WS-NON-COMM.                     CL*36
01604                                                                      CL*36
01605      IF PI-BATCH (PINDX) NOT = SPACES                             EL640
01606          GO TO 1042-CHECK-FOR-MATCH.                              EL640
01607                                                                      CL*11
01608      MOVE PB-ENTRY-BATCH         TO PI-BATCH (PINDX).             EL640
01609                                                                      CL*36
01610      IF PI-COMPANY-ID = 'DMD'                                        CL*40
01611          PERFORM 1060-DMD-ERROR-ROUTINE THRU 1060-EXIT.              CL*40
01612                                                                      CL*11
01613      IF DO-NOT-BILL-THIS-ACCT OR                                     CL*40
01614         PB-FATAL-ERRORS       OR                                     CL*40
01615         PB-UNFORCED-ERRORS    OR                                     CL*40
01616         PB-RECORD-ON-HOLD     OR                                     CL*40
01617         PB-RECORD-RETURNED    OR                                     CL*40
01618         PI-VOID-BILL                                                 CL*40
01619          MOVE +1                 TO PI-NOBILL (PINDX)             EL640
01620          MOVE ZEROS              TO PI-BILLED (PINDX)             EL640
01621                                     PI-PREV   (PINDX)             EL640
01622      ELSE                                                         EL640
01623          IF PB-BILLED-DT = LOW-VALUES                             EL640
01624            OR PI-TOT-REBILL                                       EL640
01625              MOVE +1             TO PI-BILLED (PINDX)             EL640
01626              MOVE ZEROS          TO PI-NOBILL (PINDX)             EL640
01627                                     PI-PREV   (PINDX)             EL640
01628          ELSE                                                     EL640
01629              MOVE +1             TO PI-PREV   (PINDX)             EL640
01630              MOVE ZEROS          TO PI-BILLED (PINDX)             EL640
01631                                     PI-NOBILL (PINDX).            EL640
01632                                                                      CL*40
01633      GO TO 1049-EXIT.                                             EL640
01634                                                                      CL*11
01635  1042-CHECK-FOR-MATCH.                                            EL640
01636      IF PI-BATCH (PINDX) = PB-ENTRY-BATCH                         EL640
01637          NEXT SENTENCE                                            EL640
01638      ELSE                                                         EL640
01639          GO TO 1043-SET-INDEX-UP.                                 EL640
01640                                                                      CL*36
01641      IF PI-COMPANY-ID = 'DMD'                                        CL*40
01642          PERFORM 1060-DMD-ERROR-ROUTINE THRU 1060-EXIT.              CL*40
01643                                                                      CL*11
01644      IF DO-NOT-BILL-THIS-ACCT OR                                     CL*40
01645         PB-FATAL-ERRORS       OR                                     CL*40
01646         PB-UNFORCED-ERRORS    OR                                     CL*40
01647         PB-RECORD-ON-HOLD     OR                                     CL*40
01648         PB-RECORD-RETURNED    OR                                     CL*40
01649         PI-VOID-BILL                                                 CL*40
01650          ADD +1 TO PI-NOBILL (PINDX)                                 CL*40
01651      ELSE                                                         EL640
01652          IF PB-BILLED-DT = LOW-VALUES                             EL640
01653            OR PI-TOT-REBILL                                       EL640
01654              ADD +1          TO PI-BILLED (PINDX)                 EL640
01655          ELSE                                                     EL640
01656              ADD +1          TO PI-PREV   (PINDX).                EL640
01657                                                                      CL*11
01658      GO TO 1049-EXIT.                                             EL640
01659                                                                      CL*11
01660  1043-SET-INDEX-UP.                                               EL640
01661      SET PINDX UP BY 1.                                              CL*11
01662                                                                      CL*11
01663      IF PINDX > +6                                                   CL*40
01664          MOVE 'Y'                TO BATCHES-PROCESSED-SW          EL640
01665          GO TO 1049-EXIT                                          EL640
01666      ELSE                                                         EL640
01667          GO TO 1041-SEARCH-LOOP.                                  EL640
01668                                                                      CL*11
01669  1049-EXIT.                                                       EL640
01670      EXIT.                                                        EL640
01671                                                                      CL*40
01672      EJECT                                                        EL640
01673  1050-BILL-BATCH-TRAILER-CHK.                                     EL640
01674      SET PINDX                   TO 1.                            EL640
01675                                                                      CL*11
01676  1052-LOOP.                                                       EL640
01677      IF PI-BATCH (PINDX) = SPACES                                 EL640
01678          GO TO 1059-EXIT.                                         EL640
01679                                                                      CL*11
01680      IF PB-ENTRY-BATCH = PI-BATCH (PINDX)                         EL640
01681        AND PI-BILLED (PINDX) NOT = ZEROS                          EL640
01682          MOVE 'Y'                TO BILL-BATCH-TRAILER-SW         EL640
01683          GO TO 1059-EXIT.                                         EL640
01684                                                                      CL*11
01685      SET PINDX UP BY 1.                                           EL640
01686      IF PINDX NOT > 6                                                CL*40
01687          GO TO 1052-LOOP.                                         EL640
01688                                                                      CL*11
01689  1059-EXIT.                                                       EL640
01690      EXIT.                                                        EL640
01691                                                                      CL*40
01692      EJECT                                                           CL*40
01693  1060-DMD-ERROR-ROUTINE.                                             CL*40
01694      IF PB-FATAL-ERRORS OR PB-UNFORCED-ERRORS                        CL*40
01695         IF ABILERRI = 'Y'                                            CL*40
01696            IF PB-ISSUE                                               CL*40
070805              IF PB-VALID-LIFE
01697                  COMPUTE WS-NON-PREM = PB-I-LF-PREMIUM-AMT
01698                                   + PB-I-LF-ALT-PREMIUM-AMT
01699                                   + PB-I-AH-PREMIUM-AMT
070805              ELSE
070805                 COMPUTE WS-NON-PREM = PB-I-AH-PREMIUM-AMT
070805              END-IF 
01700               ADD WS-NON-PREM TO PI-NON-PREM (PINDX)                 CL*40
01701               COMPUTE WS-NON-COMM ROUNDED =                          CL*40
01702               ((PB-I-LF-PREMIUM-AMT + PB-I-LF-ALT-PREMIUM-AMT)       CL*40
01703                                     * PB-I-LIFE-COMMISSION)          CL*40
01704                                     +                                CL*40
01705                (PB-I-AH-PREMIUM-AMT * PB-I-AH-COMMISSION)            CL*40
01706               ADD WS-NON-COMM TO PI-NON-COMM (PINDX)                 CL*40
01707            ELSE                                                      CL*40
01708            IF PB-CANCELLATION                                        CL*40
01709               COMPUTE WS-NON-PREM =                                  CL*40
01710               (PB-C-LF-CANCEL-AMT +  PB-C-AH-CANCEL-AMT) * -1        CL*40
01711               ADD WS-NON-PREM TO PI-NON-PREM (PINDX)                 CL*40
01712               COMPUTE WS-NON-COMM ROUNDED =                          CL*40
01713               ((PB-C-LF-CANCEL-AMT * PB-CI-LIFE-COMMISSION)          CL*40
01714                                    +                                 CL*40
01715                (PB-C-AH-CANCEL-AMT * PB-CI-AH-COMMISSION))           CL*40
01716                                    * -1                              CL*40
01717               ADD WS-NON-COMM TO PI-NON-COMM (PINDX).                CL*40
01718                                                                      CL*40
01719  1060-EXIT.                                                          CL*40
01720      EXIT.                                                           CL*40
01721                                                                      CL*40
01722      EJECT                                                        EL640
01723  1100-PYAJ-BILLING-PROCESS.                                       EL640
01724 *                *******************************************      EL640
01725 *                *  PROCESS THE PAYMENTS/ADJUSTMENTS FILE    *       CL*40
01726 *                *******************************************      EL640
01727                                                                      CL*40
01728      IF LIMIT-BILLING AND NOT PI-DATA-BILLED                      EL640
01729          MOVE ER-2443            TO EMI-ERROR                     EL640
01730          MOVE -1                 TO ABTCH1L                       EL640
01731          MOVE AL-UABON           TO ABTCH1A                       EL640
01732          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL640
01733          GO TO 8200-SEND-DATAONLY.                                EL640
01734                                                                   EL640
01735      IF NOT PI-DATA-BILLED                                        EL640
01736          PERFORM 4300-READ-ACCOUNT-MASTER THRU 4390-EXIT          EL640
01737          IF EMI-ERROR = ZEROS                                     EL640
01738              MOVE AM-AGT (AM-REMIT-TO) TO PI-SAV-REMIT-TO         EL640
01739                                           PI-CR-FIN-RESP             CL*11
01740              MOVE AM-REMIT-TO            TO ACCOM-SUB             EL640
01741              MOVE 'A'                    TO PI-CR-TYPE            EL640
01742              PERFORM 6000-READ-COMP-MASTER THRU 6090-EXIT         EL640
01743              IF EMI-ERROR NOT = ZEROS                             EL640
01744                  GO TO 8200-SEND-DATAONLY                         EL640
01745              ELSE                                                 EL640
01746                  NEXT SENTENCE                                    EL640
01747          ELSE                                                     EL640
01748              GO TO 8200-SEND-DATAONLY.                            EL640
01749                                                                   EL640
01750  1105-PYAJ-BILLING-STARTBR-LOOP.                                  EL640
01751      PERFORM 4500-PYAJ-START-BROWSE THRU 4590-EXIT.               EL640
01752                                                                      CL*11
01753      IF PYAJ-EOF                                                  EL640
01754          GO TO 1200-PYAJ-BILLING-COMPLETE.                        EL640
01755                                                                      CL*11
01756  1110-PYAJ-BILLING-READ-LOOP.                                     EL640
01757      PERFORM 4600-PYAJ-READ-NEXT THRU 4690-EXIT.                  EL640
01758                                                                      CL*11
01759      IF PYAJ-EOF                                                  EL640
01760          GO TO 1200-PYAJ-BILLING-COMPLETE.                        EL640
01761                                                                      CL*11
01762      IF PY-COMPANY-CD = PI-COMPANY-CD      AND                       CL*11
01763         PY-CARRIER    = ERPYAJ-BR-CARRIER  AND                       CL*11
01764         PY-GROUPING   = ERPYAJ-BR-GROUPING AND                       CL*11
01765         PY-FIN-RESP   = PI-SAV-REMIT-TO    AND                       CL*11
01766         PY-ACCOUNT    = PI-SAV-ACCT                                  CL*11
01767          NEXT SENTENCE                                            EL640
01768      ELSE                                                         EL640
01769          GO TO 1200-PYAJ-BILLING-COMPLETE.                        EL640
01770                                                                   EL640
01771      IF PY-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL640
01772          GO TO 1110-PYAJ-BILLING-READ-LOOP.                       EL640
01773                                                                   EL640
01774      IF PY-RECORD-TYPE = 'R' OR 'D' OR 'C' OR 'S' OR 'T' OR 'U'      CL*22
01775                          OR 'Z'                                      CL*22
01776          NEXT SENTENCE                                            EL640
01777      ELSE                                                         EL640
01778          GO TO 1110-PYAJ-BILLING-READ-LOOP.                       EL640
01779                                                                   EL640
CIDMOD     IF PY-VOID-SW NOT = SPACES                                        000
CIDMOD         GO TO 1110-PYAJ-BILLING-READ-LOOP.                            000
CIDMOD                                                                       000
01780      IF PI-AR-PROCESSING                                             CL*14
01781         IF PY-AR-DATE = LOW-VALUES                                   CL*14
01782            OR PI-TOT-REBILL                                          CL*14
01783            OR RETURNED-FROM = XCTL-PYAJ                              CL*15
01784            NEXT SENTENCE                                             CL*14
01785         ELSE                                                         CL*14
01786            GO TO 1110-PYAJ-BILLING-READ-LOOP                         CL*14
01787      ELSE                                                         EL640
01788         IF (PY-BILLED-DATE = LOW-VALUES AND                          CL*15
01789             PY-AR-DATE = LOW-VALUES)                                 CL*15
01790            OR PI-TOT-REBILL                                          CL*14
01791            OR RETURNED-FROM = XCTL-PYAJ                              CL*15
01792            NEXT SENTENCE                                             CL*14
01793         ELSE                                                         CL*14
01794            GO TO 1110-PYAJ-BILLING-READ-LOOP.                        CL*14
01795                                                                      CL*11
01796      MOVE 'Y'                    TO PI-DATA-BILLED-SW.            EL640
01797                                                                      CL*11
01798      IF PY-REMIT-RECEIVED OR PY-ADJ-REM-RECEIVED OR                  CL**6
01799          PY-DEPOSIT OR PY-ADJ-DEPOSIT OR                             CL*22
01800          PY-ADD-TO-BALANCE                                           CL**6
01801          ADD PY-ENTRY-AMT  TO  PI-REMITTED                           CL*22
01802      ELSE                                                         EL640
01803          IF (PY-CHARGE-TO-AGENT OR PY-ADJ-CHG-TO-AGT)                CL*22
01804             AND PY-BILLING-CHECK                                  EL640
01805              ADD PY-ENTRY-AMT TO PI-DISBURSED                        CL*22
01806          ELSE                                                     EL640
01807              ADD PY-ENTRY-AMT TO PI-ADJUSTMENTS.                     CL*33
01808                                                                      CL*11
01809      IF RETURNED-FROM = SPACES                                    EL640
01810          IF PI-UPDATE-FILES                                       EL640
01811            OR (PI-PREVIEW AND APRODSWI = 'Y')                     EL640
01812              MOVE 'BP'           TO BILLING-DETAIL-TYPE           EL640
01813              PERFORM 3000-WRITE-BILLING-DETAIL THRU 3990-EXIT.    EL640
01814                                                                      CL*11
01815      IF RETURNED-FROM = SPACES                                    EL640
01816        AND (PI-BILL OR PI-REBILLING)                              EL640
01817          PERFORM 4800-PYAJ-END-BROWSE                             EL640
01818          PERFORM 4700-PYAJ-REWRITE THRU 4790-EXIT                 EL640
01819          ADD +1 TO ERPYAJ-FILE-SEQ-NO                             EL640
01820          GO TO 1105-PYAJ-BILLING-STARTBR-LOOP.                    EL640
01821                                                                      CL*11
01822      GO TO 1110-PYAJ-BILLING-READ-LOOP.                           EL640
01823                                                                      CL*40
01824      EJECT                                                        EL640
01825  1200-PYAJ-BILLING-COMPLETE.                                      EL640
01826      IF PI-DATA-BILLED                                            EL640
01827        IF RETURNED-FROM = SPACES                                     CL*11
01828          IF PI-UPDATE-FILES                                          CL*11
01829             OR (PI-PREVIEW AND APRODSWI = 'Y')                       CL*11
01830                MOVE 'TS'           TO BILLING-DETAIL-TYPE            CL*11
01831                PERFORM 3000-WRITE-BILLING-DETAIL THRU 3990-EXIT.     CL*11
01832                                                                      CL*40
01833      PERFORM 5000-FORMAT-SCREEN THRU 5090-EXIT.                   EL640
01834                                                                      CL*11
01835      GO TO 8100-SEND-INITIAL-MAP.                                 EL640
01836                                                                      CL*40
01837      EJECT                                                        EL640
01838  1300-CHECK-OVERRIDE-PREM.                                           CL**8
01839      IF PB-CI-CANCEL-FEE NOT NUMERIC                                 CL*40
01840          MOVE ZEROS  TO  PB-CI-CANCEL-FEE.                           CL*33
01841                                                                      CL*33
01842      IF PB-OVERRIDE-LIFE  OR                                         CL*11
01843         PB-OVERRIDE-BOTH                                             CL*11
01844          GO TO 1310-LIFE-OVERRIDE.                                   CL**8
01845                                                                   EL640
01846      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS                                CL**8
01847          IF PB-ISSUE                                                 CL**8
070805           IF PB-VALID-LIFE
01848             COMPUTE  WS-I-LF-PREMIUM-AMT =                           CL*11
01849                     (PB-I-LF-PREMIUM-AMT +                           CL*11
01850                      PB-I-LF-ALT-PREMIUM-AMT) * -1                   CL*11
01851             COMPUTE  WS-I-LF-BENEFIT-AMT =                           CL**8
01852                     (PB-I-LF-BENEFIT-AMT +                           CL*11
01853                      PB-I-LF-ALT-BENEFIT-AMT) * -1
070805           END-IF
01854          ELSE                                                        CL**8
01855              MULTIPLY PB-C-LF-CANCEL-AMT BY -1                       CL**8
01856                             GIVING WS-C-LF-CANCEL-AMT                CL**8
01857              MULTIPLY PB-CI-LF-BENEFIT-AMT BY -1                     CL**8
01858                             GIVING WS-CI-LIFE-BENEFIT                CL**8
01859      ELSE                                                            CL**8
01860          IF PB-ISSUE                                                 CL**8
070805           IF PB-VALID-LIFE
01861             ADD PB-I-LF-PREMIUM-AMT   PB-I-LF-ALT-PREMIUM-AMT        CL**8
01862                 GIVING WS-I-LF-PREMIUM-AMT                           CL**8
01863             COMPUTE  WS-I-LF-BENEFIT-AMT =                           CL**8
01864                      PB-I-LF-BENEFIT-AMT +                           CL*11
01865                      PB-I-LF-ALT-BENEFIT-AMT
070805           END-IF
01866          ELSE                                                        CL**8
01867             MOVE PB-C-LF-CANCEL-AMT   TO WS-C-LF-CANCEL-AMT          CL*11
01868             MOVE PB-CI-LF-BENEFIT-AMT TO WS-CI-LIFE-BENEFIT.         CL**8
01869                                                                      CL**8
01870      GO TO 1320-CHECK-AH.                                            CL**8
01871                                                                      CL**8
01872  1310-LIFE-OVERRIDE.                                                 CL**8
01873      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS                                CL**8
01874          IF PB-ISSUE                                                 CL**8
070805           IF PB-VALID-LIFE
01875             COMPUTE WS-I-LF-PREMIUM-AMT =                            CL**8
01876                    (PB-I-LF-PREM-CALC +                              CL**8
01877                       PB-I-LF-ALT-PREM-CALC) * -1                    CL**8
01878             COMPUTE  WS-I-LF-BENEFIT-AMT =                           CL**8
01879                      (PB-I-LF-BENEFIT-AMT +                          CL**8
01880                       PB-I-LF-ALT-BENEFIT-AMT) * -1
070805           END-IF
01881          ELSE                                                        CL**8
01882              MULTIPLY PB-C-LF-REF-CALC BY -1                         CL**8
01883                             GIVING WS-C-LF-CANCEL-AMT                CL**8
01884              MULTIPLY PB-CI-LF-BENEFIT-AMT BY -1                     CL**8
01885                             GIVING WS-CI-LIFE-BENEFIT                CL**8
01886      ELSE                                                            CL**8
01887          IF PB-ISSUE                                                 CL**8
070805           IF PB-VALID-LIFE
01888             ADD PB-I-LF-PREM-CALC   PB-I-LF-ALT-PREM-CALC            CL**8
01889                 GIVING WS-I-LF-PREMIUM-AMT
01890             COMPUTE  WS-I-LF-BENEFIT-AMT =                           CL**8
01891                      PB-I-LF-BENEFIT-AMT +                           CL*11
01892                      PB-I-LF-ALT-BENEFIT-AMT                         CL*11
070805           END-IF
01893          ELSE                                                        CL**8
01894             MOVE PB-C-LF-REF-CALC     TO WS-C-LF-CANCEL-AMT          CL*11
01895             MOVE PB-CI-LF-BENEFIT-AMT TO WS-CI-LIFE-BENEFIT.         CL**8
01896                                                                      CL**8
01897  1320-CHECK-AH.                                                      CL**8
01898      IF PB-OVERRIDE-AH  OR                                           CL*11
01899         PB-OVERRIDE-BOTH                                             CL*11
01900          GO TO 1330-AH-OVERRIDE.                                     CL**8
01901                                                                      CL**8
01902      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS                                CL**8
01903          IF PB-ISSUE                                                 CL**8
01904             MULTIPLY PB-I-AH-BENEFIT-AMT BY -1                       CL**8
01905                             GIVING WS-I-AH-BENEFIT-AMT               CL**8
01906             MULTIPLY PB-I-AH-PREMIUM-AMT BY -1                       CL**8
01907                             GIVING WS-I-AH-PREMIUM-AMT               CL**8
01908          ELSE                                                        CL**8
01909              MULTIPLY PB-C-AH-CANCEL-AMT BY -1                       CL**8
01910                             GIVING WS-C-AH-CANCEL-AMT                CL**8
01911      ELSE                                                            CL**8
01912          IF PB-ISSUE                                                 CL**8
01913             MOVE PB-I-AH-BENEFIT-AMT TO WS-I-AH-BENEFIT-AMT          CL**8
01914             MOVE PB-I-AH-PREMIUM-AMT TO WS-I-AH-PREMIUM-AMT          CL**8
01915          ELSE                                                        CL**8
01916             MOVE PB-C-AH-CANCEL-AMT TO WS-C-AH-CANCEL-AMT            CL*33
01917             IF  PB-CI-CANCEL-FEE > ZEROS                             CL*40
01918                 MOVE PB-CI-CANCEL-FEE     TO WS-C-CANCEL-FEE         CL*33
01919                 MULTIPLY WS-C-CANCEL-FEE BY -1                       CL*33
01920                                           GIVING WORK-CANCEL-FEE     CL*33
01921                 ADD WORK-CANCEL-FEE       TO TOT-REPT-CANCEL-FEE.    CL*33
01922                                                                      CL**8
01923      GO TO 1399-EXIT.                                                CL**8
01924                                                                      CL**8
01925  1330-AH-OVERRIDE.                                                   CL**8
01926      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS                                CL**8
01927          IF PB-ISSUE                                                 CL**8
01928             MULTIPLY PB-I-AH-BENEFIT-AMT BY -1                       CL**8
01929                             GIVING WS-I-AH-BENEFIT-AMT               CL**8
01930             MULTIPLY PB-I-AH-PREM-CALC BY -1                         CL**8
01931                             GIVING WS-I-AH-PREMIUM-AMT               CL**8
01932          ELSE                                                        CL**8
01933              MULTIPLY PB-C-AH-REF-CALC BY -1                         CL**8
01934                             GIVING WS-C-AH-CANCEL-AMT                CL**8
01935      ELSE                                                            CL**8
01936          IF PB-ISSUE                                                 CL**8
01937             MOVE PB-I-AH-BENEFIT-AMT TO WS-I-AH-BENEFIT-AMT          CL**8
01938             MOVE PB-I-AH-PREM-CALC   TO WS-I-AH-PREMIUM-AMT          CL*11
01939          ELSE                                                        CL**8
01940             MOVE PB-C-AH-REF-CALC    TO WS-C-AH-CANCEL-AMT           CL*33
01941             IF PB-CI-CANCEL-FEE > ZEROS                              CL*40
01942                 MOVE PB-CI-CANCEL-FEE     TO WS-C-CANCEL-FEE         CL*33
01943                 MULTIPLY WS-C-CANCEL-FEE BY -1                       CL*33
01944                                           GIVING WORK-CANCEL-FEE     CL*33
01945                 ADD WORK-CANCEL-FEE       TO TOT-REPT-CANCEL-FEE.    CL*33
01946                                                                      CL**8
01947  1399-EXIT.                                                          CL**8
01948      EXIT.                                                           CL**8
01949                                                                      CL*40
01950      EJECT                                                           CL**8
01951  2000-VOID-BILLING.                                               EL640
01952                                                                   EL640
01953 *                *******************************************      EL640
01954 *                *  PROCESS VOID BILLING.  THIS SECTION    *      EL640
01955 *                *  WILL RESET THE FILES TO THE STATUS     *      EL640
01956 *                *  THEY WERE IN PRIOR TO THE LAST BILLING *      EL640
01957 *                *******************************************      EL640
01958                                                                   EL640
01959      MOVE SPACES                 TO WS-EXP-DATE                   EL640
01960                                     WS-REMIT-TO.                  EL640
01961      SET DTNDX  RTNDX            TO 1.                            EL640
01962      MOVE SPACES                 TO ELCNTL-KEY.                   EL640
01963      MOVE '1'                    TO ELCNTL-REC-TYPE.              EL640
01964                                                                      CL*11
01965      PERFORM 6100-READ-CONTROL-FILE THRU 6120-EXIT.               EL640
01966                                                                      CL*11
01967      IF EMI-ERROR NOT = ZEROS                                     EL640
01968          GO TO 8200-SEND-DATAONLY.                                EL640
01969                                                                      CL*11
01970      MOVE CF-CURRENT-MONTH-END   TO DC-BIN-DATE-1.                EL640
01971      MOVE SPACE                  TO DC-OPTION-CODE.               EL640
01972      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL640
01973      MOVE DC-GREG-DATE-1-EDIT    TO PI-MONTH-END-DATE.            EL640
01974                                                                      CL*11
01975      PERFORM 4000-PNDB-START-BROWSE THRU 4090-EXIT.               EL640
01976                                                                      CL*11
01977      IF PNDB-EOF                                                  EL640
01978          GO TO 2100-VOID-PNDB-COMPLETE.                           EL640
01979                                                                      CL*11
01980  2010-VOID-PNDB-LOOP.                                             EL640
01981      PERFORM 4100-PNDB-READ-NEXT THRU 4190-EXIT.                  EL640
01982                                                                      CL*11
01983      IF PNDB-EOF                                                  EL640
01984          GO TO 2100-VOID-PNDB-COMPLETE.                           EL640
01985                                                                      CL*11
01986      IF PB-COMPANY-CD-A1 = PI-COMPANY-CD  AND                        CL*11
01987         PB-CARRIER       = PI-SAV-CARR    AND                        CL*11
01988         PB-GROUPING      = PI-SAV-GROUP   AND                        CL*11
01989         PB-STATE         = PI-SAV-STATE   AND                        CL*11
01990         PB-ACCOUNT       = PI-SAV-ACCT                               CL*11
01991          NEXT SENTENCE                                            EL640
01992      ELSE                                                         EL640
01993          GO TO 2100-VOID-PNDB-COMPLETE.                           EL640
01994                                                                   EL640
01995      IF PB-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      EL640
01996          GO TO 2010-VOID-PNDB-LOOP.                               EL640
01997                                                                   EL640
01998      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS                             EL640
01999          MOVE PB-CONTROL-PRIMARY TO ERPNDB-PRIME-KEY              EL640
02000          EXEC CICS ENDBR                                             CL**7
02001               DATASET (ERPNDB-ALT-FILE-ID)                           CL**7
02002          END-EXEC                                                    CL*40
02003          EXEC CICS DELETE                                         EL640
02004              DATASET (ERPNDB-FILE-ID)                             EL640
02005              RIDFLD  (ERPNDB-PRIME-KEY)                           EL640
02006          END-EXEC                                                    CL*40
02007          PERFORM 4000-PNDB-START-BROWSE THRU 4090-EXIT               CL**7
02008          IF PNDB-EOF                                                 CL**7
02009             GO TO 2100-VOID-PNDB-COMPLETE                            CL**7
02010          ELSE                                                        CL**7
02011             GO TO 2010-VOID-PNDB-LOOP.                               CL**7
02012                                                                   EL640
02013      IF PB-BILLED-DT = LOW-VALUES  AND                               CL*11
02014         PB-CHG-COUNT = ZEROS                                         CL*11
02015          GO TO 2010-VOID-PNDB-LOOP.                               EL640
02016                                                                   EL640
02017      IF PB-BATCH-TRAILER                                          EL640
02018          GO TO 2115-CONT.                                         EL640
02019                                                                   EL640
02020      IF PB-CERT-EFF-DT > ERACCT-A-EXP-DATE                           CL*40
02021          MOVE PB-CERT-EFF-DT     TO PI-SAV-EXP-DT                 EL640
02022          PERFORM 4300-READ-ACCOUNT-MASTER THRU 4390-EXIT          EL640
02023          IF AM-AGT (AM-REMIT-TO) NOT = WORK-REMIT-TO              EL640
02024              MOVE AM-AGT (AM-REMIT-TO) TO WORK-REMIT-TO           EL640
02025                                           WS-SAV-REMIT-TO (RTNDX) EL640
02026              IF RTNDX < 10                                           CL*40
02027                  SET RTNDX UP BY 1.                               EL640
02028                                                                      CL*11
02029  2115-CONT.                                                       EL640
02030      IF NOT PB-BATCH-TRAILER                                      EL640
02031          PERFORM 1040-UPDATE-BILLING-STATISTICS THRU 1049-EXIT       CL*40
02032          PERFORM 1300-CHECK-OVERRIDE-PREM       THRU 1399-EXIT       CL*40
02033          PERFORM 1030-COMPUTE-COMMISSION-TOTALS THRU 1039-EXIT.      CL*40
02034                                                                      CL*11
02035      PERFORM 4200-PNDB-REWRITE THRU 4290-EXIT.                    EL640
02036                                                                      CL*33
02037      MOVE 'Y'                    TO DATA-VOIDED-SW.               EL640
02038      GO TO 2010-VOID-PNDB-LOOP.                                   EL640
02039                                                                   EL640
02040  2100-VOID-PNDB-COMPLETE.                                         EL640
02041      SET RTNDX                   TO 1.                            EL640
02042                                                                      CL*11
02043      IF NOT DATA-VOIDED                                           EL640
02044          SET DTNDX TO RTNDX                                          CL*11
02045          PERFORM 4300-READ-ACCOUNT-MASTER THRU 4390-EXIT          EL640
02046          MOVE AM-AGT (AM-REMIT-TO) TO WS-SAV-REMIT-TO (RTNDX).    EL640
02047                                                                      CL*11
02048  2105-PYAJ-LOOP.                                                  EL640
02049      IF RTNDX > 10                                                   CL*40
02050        OR WS-SAV-REMIT-TO (RTNDX) = SPACES                        EL640
02051          GO TO 2105-VOID-CONTINUE.                                EL640
02052                                                                      CL*11
02053      MOVE WS-SAV-REMIT-TO (RTNDX) TO PI-SAV-REMIT-TO.                CL*40
02054      PERFORM 2200-VOID-PYAJ THRU 2290-EXIT.                       EL640
02055                                                                      CL*40
02056      SET RTNDX UP BY 1.                                           EL640
02057      GO TO 2105-PYAJ-LOOP.                                        EL640
02058                                                                   EL640
02059  2105-VOID-CONTINUE.                                              EL640
02060      IF DATA-VOIDED                                               EL640
02061          NEXT SENTENCE                                            EL640
02062      ELSE                                                         EL640
02063          MOVE -1                 TO APFNTERL                      EL640
02064          MOVE ER-2401            TO EMI-ERROR                     EL640
02065          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL640
02066          GO TO 8100-SEND-INITIAL-MAP.                             EL640
02067                                                                      CL*11
02068      SET DTNDX                   TO 1.                            EL640
02069                                                                      CL*11
02070  2105-VOID-MASTERS-LOOP.                                          EL640
02071      IF WS-SAV-EXP-DT (DTNDX) = SPACES                            EL640
02072        OR DTNDX > 10                                                 CL*40
02073          GO TO 2105-VOID-COMPLETE.                                EL640
02074                                                                      CL*11
02075      MOVE WS-SAV-EXP-DT (DTNDX) TO PI-SAV-EXP-DT.                 EL640
02076                                                                      CL*33
02077      PERFORM 4400-ERACCT-REWRITE THRU 4490-EXIT.                  EL640
02078                                                                      CL*11
02079      IF PI-SAV-REMIT-TO NOT = ERCOMP-FIN-RESP                     EL640
02080          PERFORM 6000-READ-COMP-MASTER THRU 6090-EXIT             EL640
02081          PERFORM 3930-BILL-READ        THRU 3930-EXIT                CL*40
02082          IF NOT NO-BILL-RECS                                      EL640
02083              PERFORM 3940-BILL-GENERIC-DELETE THRU 3940-EXIT.     EL640
02084                                                                      CL*11
02085      SET DTNDX UP BY 1.                                           EL640
02086                                                                      CL*11
02087      GO TO 2105-VOID-MASTERS-LOOP.                                EL640
02088                                                                      CL*11
02089  2105-VOID-COMPLETE.                                              EL640
02090      MOVE 0000                   TO EMI-ERROR.                       CL*40
02091      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL640
02092      SET PINDX ANDX TO 1.                                            CL*11
02093                                                                   EL640
02094  2110-FORMAT-LOOP.                                                EL640
02095      IF PI-BATCH (PINDX) NOT = SPACES                             EL640
02096          MOVE PI-BATCH  (PINDX)  TO BATCH  (ANDX)                    CL*11
02097          MOVE PI-NOBILL (PINDX)  TO NOBILL (ANDX)                 EL640
02098          MOVE PI-BILLED (PINDX)  TO BILL   (ANDX)                    CL*11
02099          SET ANDX PINDX UP BY 1                                      CL*11
02100          IF PINDX > 6                                                CL*40
02101              NEXT SENTENCE                                        EL640
02102          ELSE                                                     EL640
02103              GO TO 2110-FORMAT-LOOP.                              EL640
02104                                                                      CL*11
02105      MOVE -1                     TO AACCTL.                       EL640
02106                                                                      CL*11
02107      GO TO 8100-SEND-INITIAL-MAP.                                 EL640
02108      EJECT                                                        EL640
02109  2200-VOID-PYAJ.                                                  EL640
02110      PERFORM 4500-PYAJ-START-BROWSE THRU 4590-EXIT.               EL640
02111                                                                      CL*11
02112      IF PYAJ-EOF                                                  EL640
02113          GO TO 2290-EXIT.                                         EL640
02114                                                                      CL*11
02115  2210-VOID-PYAJ-LOOP.                                             EL640
02116      PERFORM 4600-PYAJ-READ-NEXT THRU 4690-EXIT.                  EL640
02117                                                                      CL*11
02118      IF PYAJ-EOF                                                  EL640
02119          PERFORM 4800-PYAJ-END-BROWSE                             EL640
02120          GO TO 2290-EXIT.                                         EL640
02121                                                                      CL*11
02122      IF PY-COMPANY-CD = PI-COMPANY-CD      AND                       CL*11
02123         PY-CARRIER    = ERPYAJ-BR-CARRIER  AND                       CL*11
02124         PY-GROUPING   = ERPYAJ-BR-GROUPING AND                       CL*11
02125         PY-FIN-RESP   = PI-SAV-REMIT-TO    AND                       CL*11
02126         PY-ACCOUNT    = PI-SAV-ACCT                                  CL*11
02127          NEXT SENTENCE                                            EL640
02128      ELSE                                                         EL640
02129          PERFORM 4800-PYAJ-END-BROWSE                             EL640
02130          GO TO 2290-EXIT.                                         EL640
02131                                                                   EL640
02132      IF (PY-BILLED-DATE = LOW-VALUES AND                             CL*15
02133             PY-AR-DATE = LOW-VALUES)                                 CL*15
02134        OR PY-CREDIT-ACCEPT-DT NOT = LOW-VALUES                    EL640
02135          GO TO 2210-VOID-PYAJ-LOOP.                               EL640
02136                                                                      CL*11
02137      PERFORM 4800-PYAJ-END-BROWSE.                                EL640
02138                                                                      CL*33
02139      PERFORM 4700-PYAJ-REWRITE THRU 4790-EXIT.                    EL640
02140                                                                      CL*33
02141      MOVE 'Y'                    TO DATA-VOIDED-SW.               EL640
02142      ADD +1 TO ERPYAJ-FILE-SEQ-NO.                                EL640
02143                                                                      CL*40
02144      GO TO 2200-VOID-PYAJ.                                        EL640
02145                                                                   EL640
02146  2290-EXIT.                                                       EL640
02147      EXIT.                                                        EL640
02148                                                                   EL640
02149      EJECT                                                        EL640
02150                                                                   EL640
02151  3000-WRITE-BILLING-DETAIL.                                       EL640
02152                                                                   EL640
02153 *                *******************************************      EL640
02154 *                *  FORMATS THE BILLING DETAIL RECORDS.    *      EL640
02155 *                *  THESE RECORDS ARE CREATED IN THE       *      EL640
02156 *                *  SAME FORMAT THAT THEY WILL APPEAR      *      EL640
02157 *                *  WHEN PRINTED.                          *      EL640
02158 *                *******************************************      EL640
02159                                                                   EL640
02160      IF CARRIER-ADDRESS  OR                                          CL*11
02161         GEN-AGT-ADDRESS                                              CL*11
02162           PERFORM 3930-BILL-READ THRU 3930-EXIT                      CL*11
02163          IF NO-BILL-RECS                                             CL*33
02164              GO TO 3200-FORMAT-HDR-ADDR-RECS                      EL640
02165          ELSE                                                     EL640
02166              IF BI-PREVIEW-ONLY                                   EL640
02167                OR BI-INITIAL-PRINT-DATE NOT = LOW-VALUES          EL640
02168                OR PI-TOT-REBILL                                   EL640
02169                  PERFORM 3940-BILL-GENERIC-DELETE THRU 3940-EXIT  EL640
02170                  GO TO 3200-FORMAT-HDR-ADDR-RECS                  EL640
02171              ELSE                                                 EL640
02172                  MOVE ER-2403    TO EMI-ERROR                     EL640
02173                  MOVE -1         TO ABILTYPL                      EL640
02174                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL640
02175                  GO TO 3990-EXIT.                                 EL640
02176                                                                   EL640
02177      IF TOTAL-STATEMENT                                           EL640
02178          GO TO 3400-FORMAT-TOTAL-LINES.                           EL640
02179                                                                   EL640
02180  3100-FORMAT-DETAIL-LINES.                                        EL640
02181      IF PI-PREM (PINDX) NOT NUMERIC                                  CL*36
02182         MOVE ZEROS               TO PI-PREM (PINDX).                 CL*36
02183      IF PI-COMM (PINDX) NOT NUMERIC                                  CL*36
02184         MOVE ZEROS               TO PI-COMM (PINDX).                 CL*36
02185                                                                      CL*36
02186      IF WS-LINECTR > 23                                              CL*40
02187          MOVE ZEROS              TO WS-LINECTR                    EL640
02188          MOVE '3'                TO BI-RECORD-TYPE                EL640
CIDMOD         MOVE TRIPLE-SPACE       TO BI-SKIP-CONTROL                    000
02190          MOVE '          CONTINUED ON NEXT PAGE'                  EL640
02191                                  TO BI-TEXT-LINE                  EL640
02192          PERFORM 3920-BILL-WRITE                                  EL640
02193          PERFORM 3300-FORMAT-HEADINGS THRU 3380-EXIT.             EL640
02194                                                                      CL*11
02195      IF WS-LINECTR = ZEROS                                        EL640
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL                    000
02197      ELSE                                                         EL640
CIDMOD         MOVE SINGLE-SPACE       TO BI-SKIP-CONTROL                    000
CIDMOD     END-IF.                                                           000
02199                                                                      CL*11
02200      MOVE '3'                    TO BI-RECORD-TYPE.                  CL*33
02201                                                                   EL640
02202      IF BILLING-DETAIL-TYPE = 'BP'                                EL640
02203         IF PY-CHARGE-TO-AGENT OR PY-ADJ-CHG-TO-AGT                   CL*40
02204              MOVE PY-ENTRY-COMMENT   TO BI-TEXT-LINE              EL640
02205              MULTIPLY PY-ENTRY-AMT   BY -1                        EL640
02206                               GIVING BI-TOT-PREM                  EL640
02207              MOVE '* ACCOUNTING ENTRY *'  TO BI-TOT-DESC             CL**3
02208              ADD +1 TO WS-LINECTR                                    CL*40
02209              PERFORM 3920-BILL-WRITE                              EL640
02210              GO TO 3990-EXIT.                                     EL640
02211                                                                   EL640
02212      IF BILLING-DETAIL-TYPE = 'BP'                                EL640
02213          MOVE PY-ENTRY-COMMENT   TO BI-TEXT-LINE                  EL640
02214          MOVE PY-ENTRY-AMT       TO BI-TOT-PREM                   EL640
02215          MOVE '* ACCOUNTING ENTRY *'  TO BI-TOT-DESC                 CL**3
02216          ADD +1 TO WS-LINECTR                                        CL*40
02217          PERFORM 3920-BILL-WRITE                                  EL640
02218          GO TO 3990-EXIT.                                         EL640
02219                                                                   EL640
02220      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
02221                                                                   EL640
02222      IF PB-ISSUE                                                  EL640
02223         NEXT SENTENCE                                             EL640
02224      ELSE                                                         EL640
02225         GO TO 3110-FORMAT-DETAIL-CANCEL.                          EL640
02226                                                                   EL640
02227      MOVE PB-I-INSURED-FIRST-NAME    TO BI-INS-1ST-NAME.          EL640
02228      MOVE PB-I-INSURED-MIDDLE-INIT   TO BI-INS-INIT.              EL640
02229                                                                      CL*36
02230      IF PI-COMPANY-ID = 'DMD' AND                                    CL*36
02231         PB-ISSUE                                                     CL*36
02232         MOVE PB-SV-STATE             TO BI-UNDRWRTR.                 CL*36
02233                                                                      CL*36
02234      MOVE PB-I-INSURED-LAST-NAME     TO BI-INS-LAST-NAME.         EL640
02235      MOVE PB-CERT-NO                 TO BI-CERT.                  EL640
02236      MOVE PB-I-LF-TERM               TO BI-ED-TERM  BI-TERM.         CL*11
02237      MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.            EL640
02238      MOVE SPACE                      TO DC-OPTION-CODE.           EL640
02239      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL640
02240      MOVE DC-GREG-DATE-1-EDIT        TO BI-EFF-DT.                EL640
02241                                                                      CL*36
02242      IF PI-COMPANY-ID = 'DMD' AND                                    CL*36
02243         PB-ISSUE  AND                                                CL*36
02244         NOT PB-INVALID-LIFE                                          CL*36
02245           MOVE PB-I-LF-BENEFIT-CD    TO WS-BENE-CD                   CL*40
02246           MOVE WS-BENEFIT            TO PB-I-LF-ABBR.                CL*40
02247                                                                   EL640
02248      IF NOT PB-INVALID-LIFE                                          CL*13
02249         MOVE PB-I-LF-ABBR            TO BI-TYPE                   EL640
02250         MOVE WS-I-LF-PREMIUM-AMT     TO BI-PREM                   EL640
02251                                         BI-PREMIUM-AMT            EL640
02252         ADD WS-I-LF-PREMIUM-AMT      TO TOT-REPT-LF-PREM          EL640
02253         MULTIPLY PB-I-LIFE-COMMISSION BY 100 GIVING BI-ED-RATE    EL640
02254         MOVE PB-I-LIFE-COMMISSION    TO BI-RATE                   EL640
02255         MOVE PI-LF-ISS-COMP          TO BI-COMM                   EL640
02256         ADD  PI-LF-ISS-COMP          TO TOT-REPT-LF-COMP          EL640
02257         MOVE WS-I-LF-BENEFIT-AMT     TO BI-FACE-AMT               EL640
02258                                         BI-BENEFIT-AMT            EL640
02259         ADD WS-I-LF-BENEFIT-AMT      TO TOT-LF-REPT-FACE-AMT      EL640
02260         ADD WS-I-LF-PREMIUM-AMT      TO  PI-PREM (PINDX)             CL*36
02261         ADD PI-LF-ISS-COMP           TO  PI-COMM (PINDX)             CL*36
02262         PERFORM 3920-BILL-WRITE                                   EL640
02263         ADD +1 TO WS-LINECTR                                         CL*40
CIDMOD        MOVE SINGLE-SPACE            TO BI-SKIP-CONTROL              CL*40
02265         MOVE SPACES                  TO BI-TEXT-LINE.             EL640
02266                                                                      CL*36
02267      IF PI-COMPANY-ID = 'DMD' AND                                    CL*36
02268         PB-ISSUE  AND                                                CL*36
02269         NOT PB-INVALID-AH                                            CL*36
02270           MOVE PB-I-AH-BENEFIT-CD    TO WS-BENE-CD                   CL*40
02271           MOVE WS-BENEFIT            TO PB-I-AH-ABBR.                CL*40
02272                                                                   EL640
02273      IF NOT PB-INVALID-AH                                            CL*13
02274         MOVE PB-I-AH-ABBR            TO BI-TYPE                   EL640
02275         MOVE WS-I-AH-PREMIUM-AMT     TO BI-PREM                   EL640
02276                                         BI-PREMIUM-AMT            EL640
02277         ADD WS-I-AH-PREMIUM-AMT      TO TOT-REPT-AH-PREM          EL640
02278         MULTIPLY PB-I-AH-COMMISSION BY 100 GIVING BI-ED-RATE      EL640
02279         MOVE PB-I-AH-COMMISSION      TO BI-RATE                   EL640
02280         MOVE PI-AH-ISS-COMP          TO BI-COMM                   EL640
02281         ADD PI-AH-ISS-COMP           TO TOT-REPT-AH-COMP          EL640
02282         MOVE WS-I-AH-BENEFIT-AMT     TO BI-FACE-AMT               EL640
02283         ADD WS-I-AH-BENEFIT-AMT      TO TOT-AH-REPT-FACE-AMT      EL640
02284         ADD WS-I-AH-PREMIUM-AMT      TO  PI-PREM (PINDX)             CL*36
02285         ADD PI-AH-ISS-COMP           TO  PI-COMM (PINDX)             CL*36
02286         PERFORM 3920-BILL-WRITE                                   EL640
CIDMOD        MOVE SINGLE-SPACE            TO BI-SKIP-CONTROL              CL*40
02288         ADD +1                       TO WS-LINECTR.               EL640
02289                                                                   EL640
02290      GO TO 3990-EXIT.                                             EL640
02291                                                                   EL640
02292  3110-FORMAT-DETAIL-CANCEL.                                       EL640
02293      MOVE PB-CI-LAST-NAME            TO BI-INS-LAST-NAME.         EL640
02294      MOVE PB-CI-INITIALS             TO BI-INS-INITS.             EL640
02295                                                                      CL*36
02296      IF PI-COMPANY-ID = 'DMD' AND                                    CL*36
02297         PB-CANCELLATION                                              CL*36
02298         MOVE PB-SV-STATE             TO BI-UNDRWRTR.                 CL*36
02299                                                                      CL*36
02300      MOVE PB-CERT-NO                 TO BI-CERT.                  EL640
02301      MOVE PB-CI-LF-TERM              TO BI-ED-TERM                EL640
02302                                         BI-TERM.                  EL640
02303      MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.            EL640
02304      MOVE SPACE                      TO DC-OPTION-CODE.           EL640
02305      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL640
02306      MOVE DC-GREG-DATE-1-EDIT        TO BI-EFF-DT.                   CL*11
02307                                                                      CL*36
02308      IF PI-COMPANY-ID = 'DMD' AND                                    CL*36
02309         PB-CANCELLATION AND                                          CL*36
02310         PB-CI-LF-ABBR > SPACES                                       CL*40
02311           MOVE PB-CI-LF-BENEFIT-CD   TO WS-BENE-CD                   CL*40
02312           MOVE WS-BENEFIT            TO PB-CI-LF-ABBR.               CL*40
02313                                                                   EL640
02314      IF PB-CI-LF-ABBR > SPACES                                       CL*40
02315          MOVE PB-C-LF-CANCEL-DT      TO DC-BIN-DATE-1                CL*13
02316          MOVE SPACE                  TO DC-OPTION-CODE               CL*13
02317          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                    CL*13
02318          MOVE DC-GREG-DATE-1-EDIT    TO BI-CAN-DT                    CL*13
02319          MOVE PB-CI-LF-ABBR          TO BI-TYPE                   EL640
02320          MOVE WS-C-LF-CANCEL-AMT     TO BI-PREMIUM-AMT            EL640
02321          MULTIPLY WS-C-LF-CANCEL-AMT BY -1 GIVING WORK-AMT        EL640
02322          MOVE WORK-AMT               TO BI-PREM                   EL640
CIDMOD         ADD WS-C-LF-CANCEL-AMT      TO TOT-REPT-LF-REF
02323          ADD WORK-AMT                TO TOT-REPT-LF-PREM          EL640
02324                                         PI-PREM (PINDX)              CL*40
02325          MULTIPLY PB-CI-LIFE-COMMISSION                           EL640
02326                                      BY 100 GIVING BI-ED-RATE     EL640
02327          MOVE PB-CI-LIFE-COMMISSION  TO BI-RATE                   EL640
02328          IF WS-CHARGEBACK-LF-SW = 'N'                                CL*40
02329              MOVE  ZEROS                 TO PI-LF-CAN-COMP           CL*24
02330              MOVE PI-LF-CAN-COMP         TO BI-COMM                  CL*24
02331              ADD PI-LF-CAN-COMP          TO TOT-REPT-LF-COMP         CL*24
02332                                             PI-COMM (PINDX)          CL*40
CIDMOD             COMPUTE TOT-REPT-LF-COMP-R =
CIDMOD               TOT-REPT-LF-COMP-R + (PI-LF-CAN-COMP * -1)
02333              MOVE WS-CI-LIFE-BENEFIT     TO BI-BENEFIT-AMT           CL*24
02334              ADD +1                      TO WS-LINECTR               CL*24
02335              PERFORM 3920-BILL-WRITE                                 CL*24
02336              MOVE SPACES                 TO BI-TEXT-LINE             CL*24
02337          ELSE                                                        CL*24
02338              MOVE PI-LF-CAN-COMP         TO BI-COMM                  CL*24
02339              ADD PI-LF-CAN-COMP          TO TOT-REPT-LF-COMP         CL*24
02340                                             PI-COMM (PINDX)          CL*40
CIDMOD             COMPUTE TOT-REPT-LF-COMP-R =
CIDMOD               TOT-REPT-LF-COMP-R + (PI-LF-CAN-COMP * -1)
02341              MOVE  ZEROS                 TO PI-LF-CAN-COMP           CL*27
02342              MOVE WS-CI-LIFE-BENEFIT     TO BI-BENEFIT-AMT           CL*24
02343              ADD +1                      TO WS-LINECTR               CL*24
02344              PERFORM 3920-BILL-WRITE                                 CL*24
02345              MOVE SPACES                 TO BI-TEXT-LINE.            CL*24
02346                                                                      CL*36
02347      IF PI-COMPANY-ID = 'DMD' AND                                    CL*36
02348         PB-CANCELLATION AND                                          CL*36
02349         PB-CI-AH-ABBR > SPACES                                       CL*40
02350           MOVE PB-CI-AH-BENEFIT-CD   TO WS-BENE-CD                   CL*40
02351           MOVE WS-BENEFIT            TO PB-CI-AH-ABBR.               CL*40
02352                                                                   EL640
02353      IF PB-CI-AH-ABBR > SPACES                                       CL*40
02354          MOVE PB-C-AH-CANCEL-DT      TO DC-BIN-DATE-1                CL*13
02355          MOVE SPACE                  TO DC-OPTION-CODE               CL*13
02356          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                    CL*13
02357          MOVE DC-GREG-DATE-1-EDIT    TO BI-CAN-DT                    CL*13
02358          MOVE PB-CI-AH-ABBR          TO BI-TYPE                   EL640
02359          MOVE WS-C-AH-CANCEL-AMT     TO BI-PREMIUM-AMT            EL640
02360          MULTIPLY WS-C-AH-CANCEL-AMT BY -1 GIVING WORK-AMT        EL640
02361          MOVE WORK-AMT               TO BI-PREM                   EL640
CIDMOD         ADD WS-C-AH-CANCEL-AMT      TO TOT-REPT-AH-REF
02362          ADD WORK-AMT                TO TOT-REPT-AH-PREM          EL640
02363                                         PI-PREM (PINDX)              CL*40
02364          MULTIPLY PB-CI-AH-COMMISSION BY 100 GIVING BI-ED-RATE    EL640
02365          MOVE PB-CI-AH-COMMISSION    TO BI-RATE                   EL640
02366          IF WS-CHARGEBACK-AH-SW = 'N'                                CL*40
02367              MOVE  ZEROS                 TO PI-AH-CAN-COMP           CL*24
02368              MOVE PI-AH-CAN-COMP         TO BI-COMM                  CL*24
02369              ADD PI-AH-CAN-COMP          TO TOT-REPT-AH-COMP         CL*24
02370                                             PI-COMM (PINDX)          CL*40
CIDMOD             COMPUTE TOT-REPT-AH-COMP-R =
CIDMOD               TOT-REPT-AH-COMP-R + (PI-AH-CAN-COMP * -1)
02371              ADD +1                      TO WS-LINECTR               CL*24
02372              PERFORM 3920-BILL-WRITE                                 CL*24
02373          ELSE                                                        CL*24
02374              MOVE PI-AH-CAN-COMP         TO BI-COMM                  CL*24
02375              ADD PI-AH-CAN-COMP          TO TOT-REPT-AH-COMP         CL*24
02376                                             PI-COMM (PINDX)          CL*40
CIDMOD             COMPUTE TOT-REPT-AH-COMP-R =
CIDMOD               TOT-REPT-AH-COMP-R + (PI-AH-CAN-COMP * -1)
02377              MOVE  ZEROS                 TO PI-AH-CAN-COMP           CL*27
02378              ADD +1                      TO WS-LINECTR               CL*24
02379              PERFORM 3920-BILL-WRITE.                                CL*24
02380                                                                   EL640
02381      IF PB-CI-CANCEL-FEE NOT NUMERIC                                 CL*40
02382          MOVE ZEROS                      TO PB-CI-CANCEL-FEE.        CL*40
02383                                                                      CL*33
02384      IF PB-CI-CANCEL-FEE > ZEROS                                     CL*40
02385          MOVE SPACES                 TO BI-INS-LAST-NAME             CL*33
02386                                         BI-INS-INITS                 CL*33
02387                                         BI-EFF-DT                    CL*33
02388                                         BI-CAN-DT                    CL*33
02389                                         BI-TYPE                      CL*33
02390          MOVE 'CANCEL FEE'           TO BI-CERT                      CL*33
02391          MOVE WORK-CANCEL-FEE        TO BI-PREM                      CL*33
02392                                         BI-COMM                      CL*33
02393          MOVE ZEROS                  TO BI-PREMIUM-AMT               CL*33
02394                                         BI-RATE                      CL*40
02395                                         BI-ED-RATE                   CL*40
02396                                         BI-ED-TERM                   CL*33
02397                                         BI-TERM                      CL*33
02398          ADD +1                      TO WS-LINECTR                   CL*33
02399          PERFORM 3920-BILL-WRITE.                                    CL*33
02400                                                                      CL*33
02401      GO TO 3990-EXIT.                                             EL640
02402                                                                   EL640
02403      EJECT                                                        EL640
02404                                                                   EL640
02405  3200-FORMAT-HDR-ADDR-RECS.                                       EL640
02406      PERFORM 3910-BILL-GETMAIN.                                   EL640
02407                                                                   EL640
02408      MOVE ZEROS                  TO WS-LINE-SEQ-NO.               EL640
02409      SET A-INDX  R-INDX          TO 1.                               CL*11
02410      PERFORM 3210-FORMAT-ADDR-RECS  THRU 3210-EXIT 6 TIMES.          CL**4
02411      MOVE ZEROS                  TO WS-LINE-SEQ-NO.               EL640
02412      PERFORM 3300-FORMAT-HEADINGS THRU 3380-EXIT.                 EL640
02413      GO TO 3990-EXIT.                                             EL640
02414                                                                   EL640
02415  3210-FORMAT-ADDR-RECS.                                           EL640
02416      MOVE '2'                     TO BI-RECORD-TYPE.                 CL*11
02417      MOVE WS-ACCT-LINES (A-INDX)  TO BI-ACCT-ADDRESS-LINE.           CL*11
02418      MOVE WS-REMIT-LINES (R-INDX) TO BI-REMIT-ADDRESS-LINE.       EL640
02419      PERFORM 3920-BILL-WRITE.                                     EL640
02420      SET A-INDX  R-INDX         UP BY 1.                             CL*11
02421                                                                   EL640
02422  3210-EXIT.                                                       EL640
02423      EXIT.                                                        EL640
02424                                                                      CL*40
02425      EJECT                                                        EL640
02426  3300-FORMAT-HEADINGS.                                            EL640
02427      MOVE WS-PGECTR              TO BI-HD-PG.                     EL640
02428      ADD +1                      TO WS-PGECTR.                    EL640
02429      MOVE '3'                    TO BI-RECORD-TYPE.               EL640
CIDMOD     MOVE TOP-OF-PAGE            TO BI-SKIP-CONTROL.                 CL*40
02431                                                                      CL*11
02432      IF NOT PI-PREVIEW                                            EL640
02433          MOVE BI-HD1             TO BI-TEXT-LINE                  EL640
02434      ELSE                                                         EL640
02435          MOVE BI-PREVIEW-HD      TO BI-TEXT-LINE                  EL640
02436          PERFORM 3920-BILL-WRITE                                  EL640
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL                  CL*40
02438          MOVE BI-HD1             TO BI-TEXT-LINE.                 EL640
02439                                                                      CL*11
02440      PERFORM 3920-BILL-WRITE.                                     EL640
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
02442      MOVE BI-HD2                 TO BI-TEXT-LINE.                 EL640
02443      PERFORM 3920-BILL-WRITE.                                     EL640
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
02445      MOVE BI-HD3                 TO BI-TEXT-LINE.                 EL640
02446      PERFORM 3920-BILL-WRITE.                                     EL640
02447      SET A-INDX  R-INDX          TO 1.                               CL*11
02448      PERFORM 3390-FORMAT-ADDR-TEXT-LINES THRU 3390-EXIT 5 TIMES.  EL640
02449      MOVE '3'                    TO BI-RECORD-TYPE.               EL640
CIDMOD     MOVE DOUBLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
02451      MOVE BI-HD4                 TO BI-TEXT-LINE.                 EL640
02452      PERFORM 3920-BILL-WRITE.                                     EL640
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
02454      MOVE BI-HD5                 TO BI-TEXT-LINE.                 EL640
02455      PERFORM 3920-BILL-WRITE.                                     EL640
02456                                                                      CL*11
02457  3380-EXIT.                                                       EL640
02458      EXIT.                                                        EL640
02459                                                                   EL640
02460  3390-FORMAT-ADDR-TEXT-LINES.                                     EL640
02461      MOVE '3'                    TO BI-RECORD-TYPE.               EL640
02462      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
02463                                                                   EL640
02464      IF A-INDX = 1                                                EL640
02465          MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL                  CL*40
02466          MOVE 'ACCOUNT NO. - '   TO BI-ADDR-LIT                   EL640
02467          MOVE WS-CARR-COMP       TO BI-CO                         EL640
02468          MOVE '-'                TO BI-DASH                       EL640
02469          MOVE PI-SAV-ACCT        TO BI-ACCT                       EL640
02470          MOVE 'REMIT TO - '      TO BI-REMIT-LIT                  EL640
02471      ELSE                                                         EL640
02472          MOVE SINGLE-SPACE       TO BI-SKIP-CONTROL.                 CL*40
02473                                                                      CL*11
02474      MOVE WS-ACCT-LINES (A-INDX)  TO BI-ACCT-ADDR.                   CL*40
02475      MOVE WS-REMIT-LINES (R-INDX) TO BI-REMIT-ADDR.               EL640
02476      PERFORM 3920-BILL-WRITE.                                     EL640
02477      SET A-INDX  R-INDX  UP BY 1.                                    CL*40
02478                                                                      CL*11
02479  3390-EXIT.                                                       EL640
02480      EXIT.                                                        EL640
02481                                                                      CL*40
02482      EJECT                                                        EL640
02483  3400-FORMAT-TOTAL-LINES.                                         EL640
02484      COMPUTE PI-END-BAL = PI-BAL-FRWD +                           EL640
02485                           PI-PREMIUM +                               CL*33
02486                           TOT-REPT-CANCEL-FEE -                      CL*33
02487                           PI-REMITTED -                           EL640
02488                           PI-TOT-ISS-COMP +                       EL640
02489                           PI-TOT-CAN-COMP +                       EL640
02490                           PI-ADJUSTMENTS +                           CL*33
02491                           PI-DISBURSED.                           EL640
02492                                                                   EL640
02493      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
02494                                                                   EL640
02495      MOVE '1'                    TO BI-RECORD-TYPE.               EL640
02496      MOVE PI-PROCESSOR-ID        TO BI-PROCESSOR-CD.              EL640
02497                                                                      CL*11
02498      IF PI-PREV-BILL OR PI-PREV-REBILL                            EL640
02499          MOVE 'P'                TO BI-STATEMENT-TYPE.            EL640
02500                                                                      CL*11
02501      MOVE +1                     TO BI-NO-OF-COPIES.              EL640
02502      MOVE WS-CURRENT-DATE        TO BI-CREATION-DT.               EL640
02503      MOVE LOW-VALUES             TO BI-INITIAL-PRINT-DATE.        EL640
02504      MOVE PI-BAL-FRWD            TO BI-BAL-FRWD.                  EL640
02505      MOVE PI-PREMIUM             TO BI-PREMIUM.                   EL640
02506      MOVE PI-REMITTED            TO BI-REMITTED.                  EL640
02507      MOVE PI-TOT-ISS-COMP        TO BI-TOT-ISS-COMP.              EL640
02508      MOVE PI-TOT-CAN-COMP        TO BI-TOT-CAN-COMP.              EL640
02509      MOVE PI-ADJUSTMENTS         TO BI-ADJUSTMNTS.                   CL*33
02510      MOVE PI-DISBURSED           TO BI-DISBURSED.                 EL640
02511      MOVE PI-END-BAL             TO BI-END-BAL.                   EL640
02512                                                                      CL*11
02513      IF PI-SAV-ACCT = PI-SAV-REMIT-TO                             EL640
02514          MOVE WS-ACCT-LINES (1)  TO BI-FIN-RESP-NAME              EL640
02515      ELSE                                                         EL640
02516          MOVE WS-REMIT-LINES (1) TO BI-FIN-RESP-NAME.             EL640
02517                                                                      CL*11
02518      PERFORM 3920-BILL-WRITE.                                     EL640
02519                                                                      CL*10
02520      IF WS-LINECTR > 16                                              CL*40
02521          MOVE ZEROS              TO WS-LINECTR                       CL*10
02522          MOVE '3'                TO BI-RECORD-TYPE                   CL*10
CIDMOD         MOVE TRIPLE-SPACE       TO BI-SKIP-CONTROL                  CL*40
02524          MOVE '          CONTINUED ON NEXT PAGE'                     CL*10
02525                                  TO BI-TEXT-LINE                     CL*10
02526          PERFORM 3920-BILL-WRITE                                     CL*10
02527          PERFORM 3300-FORMAT-HEADINGS THRU 3380-EXIT.                CL*10
02528                                                                      CL*10
02529      IF PI-BAL-FRWD NOT = ZERO                                    EL640
02530          MOVE SPACES             TO BI-TEXT-LINE                  EL640
02531          MOVE '3'                TO BI-RECORD-TYPE                EL640
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL                  CL*40
02533          MOVE 'BALANCE FORWARD'  TO BI-TOT-DESC                   EL640
02534          MOVE PI-BAL-FRWD        TO BI-TOT-PREM                   EL640
02535          PERFORM 3920-BILL-WRITE.                                 EL640
02536                                                                   EL640
02537      IF PI-PREMIUM NOT = ZERO                                     EL640
02538          MOVE SPACES             TO BI-TEXT-LINE                  EL640
02539          MOVE '3'                TO BI-RECORD-TYPE                EL640
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL                  CL*40
02541          MOVE 'THIS MONTHS PREMIUM'  TO BI-TOT-DESC               EL640
02542          COMPUTE WORK-AMT  =  TOT-REPT-CANCEL-FEE + PI-PREMIUM       CL*33
02543          MOVE WORK-AMT           TO BI-TOT-PREM                      CL*33
02544          PERFORM 3920-BILL-WRITE.                                 EL640
02545                                                                   EL640
02546      IF PI-TOT-ISS-COMP NOT = ZEROS  OR                              CL*40
02547         PI-TOT-CAN-COMP NOT = ZEROS                                  CL*40
02548          MOVE SPACES             TO BI-TEXT-LINE                  EL640
02549          MOVE '3'                TO BI-RECORD-TYPE                EL640
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL                  CL*40
02551          MOVE 'LESS COMPENSATION' TO BI-TOT-DESC                     CL*40
02552          SUBTRACT PI-TOT-CAN-COMP FROM PI-TOT-ISS-COMP            EL640
02553                           GIVING BI-TOT-PREM                      EL640
02554          PERFORM 3920-BILL-WRITE.                                 EL640
02555                                                                   EL640
02556      IF PI-REMITTED  NOT = ZEROS  OR                                 CL*40
02557         PI-DISBURSED NOT = ZEROS                                     CL*40
02558          MOVE SPACES             TO BI-TEXT-LINE                  EL640
02559          MOVE '3'                TO BI-RECORD-TYPE                EL640
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL                  CL*40
02561          MOVE 'LESS PAYMENTS'    TO BI-TOT-DESC                   EL640
02562          IF PI-COMPANY-ID = 'UCL'                                    CL*30
02563              SUBTRACT PI-DISBURSED FROM PI-REMITTED                  CL*30
02564                       GIVING BI-TOT-PREM                             CL*30
02565              PERFORM 3920-BILL-WRITE                                 CL*30
02566            ELSE                                                      CL*30
02567              MOVE PI-REMITTED        TO BI-TOT-PREM                  CL*30
02568              PERFORM 3920-BILL-WRITE.                                CL*30
02569                                                                   EL640
02570      IF PI-ADJUSTMENTS NOT = ZERO                                    CL*33
02571          MOVE SPACES             TO BI-TEXT-LINE                  EL640
02572          MOVE '3'                TO BI-RECORD-TYPE                EL640
CIDMOD         MOVE DOUBLE-SPACE       TO BI-SKIP-CONTROL                  CL*40
02574          MOVE 'ADJUSTMENT'       TO BI-TOT-DESC                   EL640
02575          ADD PI-ADJUSTMENTS, PI-DISBURSED GIVING BI-TOT-PREM         CL*33
02576          PERFORM 3920-BILL-WRITE.                                 EL640
02577                                                                   EL640
02578      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
02579      MOVE '3'                    TO BI-RECORD-TYPE.               EL640
CIDMOD     MOVE DOUBLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
02581      MOVE ALL '-'                TO BI-TOT-DASH.                  EL640
02582      PERFORM 3920-BILL-WRITE.                                     EL640
02583                                                                   EL640
02584      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
02585      MOVE '3'                    TO BI-RECORD-TYPE.               EL640
CIDMOD     MOVE DOUBLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
02587                                                                      CL*11
02588      IF PI-END-BAL < +1.00 AND > -1.00                               CL*40
02589          NEXT SENTENCE                                            EL640
02590      ELSE                                                         EL640
02591          IF PI-END-BAL < +0                                          CL*40
02592              MOVE 'WE WILL REFUND' TO BI-TOT-DESC                 EL640
02593          ELSE                                                     EL640
02594              MOVE 'PLEASE REMIT' TO BI-TOT-DESC.                  EL640
02595                                                                      CL*11
02596      MOVE PI-END-BAL             TO BI-TOT-PREM.                     CL*11
02597                                                                      CL*11
02598      PERFORM 3920-BILL-WRITE.                                     EL640
02599                                                                      CL*11
02600      IF CO-CARRY-BALANCE                                          EL640
02601          IF PI-END-BAL > +1.00                                       CL*40
02602              MOVE SPACES         TO BI-TEXT-LINE                  EL640
02603              MOVE '3'            TO BI-RECORD-TYPE                EL640
CIDMOD             MOVE DOUBLE-SPACE   TO BI-SKIP-CONTROL                  CL*40
02605              MOVE 'PLEASE RETURN ONE COPY WITH REMITTANCE'        EL640
02606                                  TO BI-TEXT-LINE                  EL640
02607              PERFORM 3920-BILL-WRITE.                             EL640
02608                                                                      CL*11
02609      IF PI-COMPANY-ID = 'DMD'                                        CL*36
02610         MOVE SPACES                 TO BI-TEXT-LINE                  CL*36
02611         MOVE '3'                    TO BI-RECORD-TYPE                CL*36
02612         MOVE '0'                    TO BI-SKIP-CONTROL               CL*40
02613         MOVE BI-HD6                 TO BI-TEXT-LINE                  CL*36
02614         PERFORM 3920-BILL-WRITE                                      CL*36
02615         MOVE SPACES                 TO BI-TEXT-LINE                  CL*36
02616         MOVE ' '                    TO BI-SKIP-CONTROL               CL*40
02617         MOVE BI-HD7                 TO BI-TEXT-LINE                  CL*36
02618         PERFORM 3920-BILL-WRITE                                      CL*36
02619                                                                      CL*36
02620         PERFORM 3600-FIND-BATCH-NUMBER THRU 3699-EXIT                CL*36
02621           VARYING PINDX FROM 1 BY 1                                  CL*36
02622             UNTIL PINDX > +6.                                        CL*40
02623                                                                      CL*36
02624      MOVE ZEROS                  TO PI-PREM     (PINDX)              CL*40
02625                                     PI-COMM     (PINDX)              CL*40
02626                                     PI-NON-PREM (PINDX)              CL*40
02627                                     PI-NON-COMM (PINDX).             CL*40
02628                                                                      CL*36
02629      GO TO 3990-EXIT.                                             EL640
02630      EJECT                                                        EL640
02631                                                                   EL640
02632  3500-TOTAL-OUT-DETAIL.                                           EL640
02633      IF PI-DATA-BILLED AND                                        EL640
02634         RETURNED-FROM = SPACES                                       CL*11
02635            NEXT SENTENCE                                             CL*11
02636        ELSE                                                          CL*11
02637            GO TO 3599-EXIT.                                          CL*11
02638                                                                   EL640
02639      IF PI-UPDATE-FILES  OR                                          CL*11
02640         (PI-PREVIEW AND APRODSWI = 'Y')                              CL*11
02641            NEXT SENTENCE                                             CL*11
02642         ELSE                                                         CL*11
02643            GO TO 3599-EXIT.                                          CL*11
02644                                                                   EL640
02645      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
02646 *    MOVE '3'                    TO BI-RECORD-TYPE.               EL640
CIDMOD*    MOVE DOUBLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
02648 *    MOVE 'TOTAL'                TO BI-TOT-LIT.                   EL640
02649 *    MOVE PI-LIFE-OVERRIDE-L6    TO BI-OVERRIDE-L6.               EL640
02650 *    MOVE TOT-REPT-LF-PREM       TO BI-TOT-PREM.                  EL640
02651 *    MOVE TOT-REPT-LF-COMP       TO BI-COM-TOT.                   EL640
02652 *    MOVE TOT-LF-REPT-FACE-AMT   TO BI-FACE-TOT.                  EL640
02653 *    PERFORM 3920-BILL-WRITE.                                     EL640
02654 *                                                                 EL640
02655 *    MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
CIDMOD*    MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
02657 *    MOVE 'TOTAL'                TO BI-TOT-LIT.                   EL640
02658 *    MOVE PI-AH-OVERRIDE-L6      TO BI-OVERRIDE-L6.               EL640
02659 *    MOVE TOT-REPT-AH-PREM       TO BI-TOT-PREM                   EL640
02660 *    MOVE TOT-REPT-AH-COMP       TO BI-COM-TOT.                   EL640
02661 *    MOVE TOT-AH-REPT-FACE-AMT   TO BI-FACE-TOT.                  EL640
02662 *    PERFORM 3920-BILL-WRITE.                                     EL640
02663                                                                   EL640
02645      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
02646      MOVE '3'                    TO BI-RECORD-TYPE.               EL640
CIDMOD     MOVE DOUBLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
CIDMOD     MOVE 'TOTAL PREM '          TO BI-TOT-DESC
CIDMOD     MOVE PI-LIFE-OVERRIDE-L6    TO BI-TOT-DESC (12:6)
CIDMOD     ADD TOT-REPT-LF-REF TOT-REPT-LF-PREM
CIDMOD          GIVING BI-TOT-PREM
CIDMOD     ADD TOT-REPT-LF-COMP TOT-REPT-LF-COMP-R
CIDMOD          GIVING BI-COM-TOT
CIDMOD*    MOVE TOT-REPT-LF-COMP       TO BI-COM-TOT.                   EL640
CIDMOD*    MOVE TOT-LF-REPT-FACE-AMT   TO BI-FACE-TOT.                  EL640
02653      PERFORM 3920-BILL-WRITE.                                     EL640
02654                                                                   EL640
02645      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
02646      MOVE '3'                    TO BI-RECORD-TYPE.               EL640
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
CIDMOD     MOVE 'TOTAL REF  '          TO BI-TOT-DESC
CIDMOD     MOVE PI-LIFE-OVERRIDE-L6    TO BI-TOT-DESC (12:6)
CIDMOD     MOVE TOT-REPT-LF-REF        TO BI-TOT-PREM.                  EL640
CIDMOD     MOVE TOT-REPT-LF-COMP-R     TO BI-COM-TOT
02653      PERFORM 3920-BILL-WRITE.                                     EL640
02654                                                                   EL640
02645      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
02646      MOVE '3'                    TO BI-RECORD-TYPE.               EL640
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
CIDMOD     MOVE 'TOTAL NET  '          TO BI-TOT-DESC
CIDMOD     MOVE PI-LIFE-OVERRIDE-L6    TO BI-TOT-DESC (12:6)
CIDMOD     MOVE TOT-REPT-LF-PREM       TO BI-TOT-PREM
CIDMOD     MOVE TOT-REPT-LF-COMP       TO BI-COM-TOT
02652      MOVE TOT-LF-REPT-FACE-AMT   TO BI-FACE-TOT.                  EL640
02653      PERFORM 3920-BILL-WRITE.                                     EL640
02654                                                                   EL640
02655      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
CIDMOD     MOVE 'TOTAL PREM '          TO BI-TOT-DESC
CIDMOD     MOVE PI-AH-OVERRIDE-L6      TO BI-TOT-DESC (12:6)
CIDMOD     ADD TOT-REPT-AH-REF TOT-REPT-AH-PREM
CIDMOD          GIVING BI-TOT-PREM
CIDMOD     ADD TOT-REPT-AH-COMP TOT-REPT-AH-COMP-R
CIDMOD          GIVING BI-COM-TOT
CIDMOD*    MOVE TOT-AH-REPT-FACE-AMT   TO BI-FACE-TOT.                  EL640
02662      PERFORM 3920-BILL-WRITE.                                     EL640
02663                                                                   EL640
02655      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
CIDMOD     MOVE 'TOTAL REF  '          TO BI-TOT-DESC
CIDMOD     MOVE PI-AH-OVERRIDE-L6      TO BI-TOT-DESC (12:6)
CIDMOD     MOVE TOT-REPT-AH-REF        TO BI-TOT-PREM                   EL640
CIDMOD     MOVE TOT-REPT-AH-COMP-R     TO BI-COM-TOT.                   EL640
02662      PERFORM 3920-BILL-WRITE.                                     EL640
02663                                                                   EL640
02655      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
CIDMOD     MOVE 'TOTAL NET  '          TO BI-TOT-DESC
CIDMOD     MOVE PI-AH-OVERRIDE-L6      TO BI-TOT-DESC (12:6)
CIDMOD     MOVE TOT-REPT-AH-PREM       TO BI-TOT-PREM
CIDMOD     MOVE TOT-REPT-AH-COMP       TO BI-COM-TOT.                   EL640
02661      MOVE TOT-AH-REPT-FACE-AMT   TO BI-FACE-TOT.                  EL640
02662      PERFORM 3920-BILL-WRITE.                                     EL640
02663                                                                   EL640
02664      IF PB-CI-CANCEL-FEE NOT NUMERIC                                 CL*40
02665          MOVE ZEROS  TO  PB-CI-CANCEL-FEE.                           CL*33
02666                                                                      CL*33
02667      IF PB-CI-CANCEL-FEE > ZEROS                                     CL*40
02668          MOVE SPACES                 TO BI-TEXT-LINE                 CL*33
CIDMOD         MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL              CL*40
02670          MOVE 'TOTAL'                TO BI-TOT-LIT                   CL*33
02671          MOVE 'CAN-FE'               TO BI-OVERRIDE-L6               CL*33
02672          MOVE TOT-REPT-CANCEL-FEE    TO BI-TOT-PREM                  CL*33
02673                                         BI-COM-TOT                   CL*33
02674          MOVE ZEROS                  TO BI-FACE-TOT                  CL*33
02675          PERFORM 3920-BILL-WRITE.                                    CL*33
02676                                                                      CL*33
02677      MOVE SPACES                 TO BI-TEXT-LINE.                 EL640
CIDMOD     MOVE SINGLE-SPACE           TO BI-SKIP-CONTROL.                 CL*40
CIDMOD     PERFORM 3920-BILL-WRITE.                                     EL640
02680                                                                   EL640
02681  3599-EXIT.                                                       EL640
02682      EXIT.                                                           CL*36
02683      EJECT                                                           CL*36
02684                                                                      CL*36
02685  3600-FIND-BATCH-NUMBER.                                             CL*36
02686      IF PI-PREM (PINDX) NOT NUMERIC                                  CL*36
02687         MOVE ZEROS               TO PI-PREM (PINDX).                 CL*36
02688      IF PI-COMM (PINDX) NOT NUMERIC                                  CL*36
02689         MOVE ZEROS               TO PI-COMM (PINDX).                 CL*36
02690      IF PI-NON-PREM (PINDX) NOT NUMERIC                              CL*36
02691         MOVE ZEROS               TO PI-NON-PREM (PINDX).             CL*36
02692      IF PI-NON-COMM (PINDX) NOT NUMERIC                              CL*36
02693         MOVE ZEROS               TO PI-NON-COMM (PINDX).             CL*36
02694                                                                      CL*36
02695      IF PI-PREM (PINDX)     = ZEROS   AND                            CL*40
02696         PI-COMM (PINDX)     = ZEROS   AND                            CL*40
02697         PI-NON-PREM (PINDX) = ZEROS   AND                            CL*36
02698         PI-NON-COMM (PINDX) = ZEROS                                  CL*36
02699           GO TO 3699-EXIT.                                           CL*40
02700                                                                      CL*36
02701      MOVE SPACES                 TO BI-TEXT-LINE.                    CL*36
02702      MOVE '3'                    TO BI-RECORD-TYPE.                  CL*36
02703      MOVE ' '                    TO BI-SKIP-CONTROL.                 CL*40
02704      MOVE 'BATCH NO. '           TO WS-BATCH-DESC.                   CL*36
02705      MOVE PI-BATCH (PINDX)       TO WS-BATCH-NBR.                    CL*36
02706      MOVE WS-DESC                TO BI-TOT-DESC5.                    CL*36
02707      MOVE PI-PREM (PINDX)        TO BI-TOT-PREM5.                    CL*36
02708      MOVE PI-COMM (PINDX)        TO BI-COM-TOT5.                     CL*36
02709      MOVE PI-NON-PREM (PINDX)    TO BI-NON-PREM5.                    CL*36
02710      MOVE PI-NON-COMM (PINDX)    TO BI-NON-COMM5.                    CL*36
02711      PERFORM 3920-BILL-WRITE.                                        CL*36
02712                                                                      CL*40
02713  3699-EXIT.                                                          CL*36
02714      EXIT.                                                           CL*33
02715      EJECT                                                        EL640
02716                                                                   EL640
02717  3910-BILL-GETMAIN.                                               EL640
02718      EXEC CICS GETMAIN                                            EL640
02719          SET     (ADDRESS OF BILLING-STATEMENT)                      CL*33
02720          LENGTH  (210)                                               CL*11
02721          INITIMG (GETMAIN-SPACE)                                  EL640
02722      END-EXEC.                                                       CL*11
02723                                                                   EL640
02724  3920-BILL-WRITE.                                                 EL640
02725      MOVE PI-COMPANY-CD          TO BI-COMPANY-CD.                EL640
02726      MOVE PI-CR-CARRIER          TO BI-CARRIER.                   EL640
02727      MOVE PI-CR-GROUPING         TO BI-GROUPING                   EL640
02728      MOVE PI-SAV-ACCT            TO BI-ACCOUNT.                   EL640
02729      MOVE PI-SAV-REMIT-TO        TO BI-FIN-RESP.                  EL640
02730                                                                   EL640
02731      MOVE 'BI'                   TO BI-RECORD-ID.                 EL640
02732      ADD +1                      TO WS-LINE-SEQ-NO.               EL640
02733                                                                      CL*11
02734      IF BI-RECORD-TYPE = '1'                                      EL640
02735          MOVE ZEROS              TO BI-LINE-SEQ-NO                EL640
02736      ELSE                                                         EL640
02737          MOVE WS-LINE-SEQ-NO     TO BI-LINE-SEQ-NO.               EL640
02738                                                                      CL*11
02739      EXEC CICS WRITE                                              EL640
02740          DATASET (ERBILL-FILE-ID)                                 EL640
02741          FROM    (BILLING-STATEMENT)                                 CL*11
02742          RIDFLD  (BI-CONTROL-PRIMARY)                                CL*11
02743      END-EXEC.                                                       CL*11
02744                                                                   EL640
02745  3930-BILL-READ.                                                  EL640
02746      MOVE PI-COMPANY-CD          TO ERBILL-CO-CD.                 EL640
02747      MOVE PI-CR-CARRIER          TO ERBILL-CARRIER.               EL640
02748      MOVE PI-CR-GROUPING         TO ERBILL-GROUP.                 EL640
02749      MOVE PI-SAV-ACCT            TO ERBILL-ACCT.                  EL640
02750      MOVE PI-SAV-REMIT-TO        TO ERBILL-FIN-RESP.              EL640
02751      MOVE '1'                    TO ERBILL-REC-TYPE.              EL640
02752      MOVE ZEROS                  TO ERBILL-LINE-SEQ-NO.           EL640
02753                                                                      CL*11
02754      EXEC CICS HANDLE CONDITION                                   EL640
02755          NOTFND  (3930-BILL-NOTFND)                                  CL*11
02756          NOTOPEN (3930-BILL-NOTOPEN)                              EL640
02757      END-EXEC.                                                       CL*11
02758                                                                      CL*11
02759      EXEC CICS READ                                               EL640
02760          SET     (ADDRESS OF BILLING-STATEMENT)                      CL*33
02761          DATASET (ERBILL-FILE-ID)                                 EL640
02762          RIDFLD  (ERBILL-KEY)                                        CL*11
02763          GTEQ                                                     EL640
02764      END-EXEC.                                                       CL*11
02765                                                                      CL*11
02766      IF BI-COMPANY-CD = PI-COMPANY-CD  AND                           CL*11
02767         BI-CARRIER    = ERBILL-CARRIER AND                           CL*11
02768         BI-GROUPING   = ERBILL-GROUP   AND                           CL*11
02769         BI-ACCOUNT    = ERBILL-ACCT    AND                           CL*11
02770         BI-FIN-RESP   = ERBILL-FIN-RESP                              CL*11
02771           MOVE SPACE              TO NO-BILL-REC-SW                  CL*11
02772           GO TO 3930-EXIT                                            CL*11
02773       ELSE                                                           CL*11
02774           GO TO 3930-BILL-NOTFND.                                    CL*11
02775                                                                      CL*11
02776  3930-BILL-NOTOPEN.                                               EL640
02777      MOVE ER-2564                TO EMI-ERROR.                    EL640
02778      MOVE -1 TO APFNTERL.                                         EL640
02779      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL640
02780      GO TO 8200-SEND-DATAONLY.                                    EL640
02781                                                                      CL*11
02782  3930-BILL-NOTFND.                                                EL640
02783      MOVE 'Y'                    TO NO-BILL-REC-SW.               EL640
02784                                                                      CL*11
02785  3930-EXIT.                                                       EL640
02786      EXIT.                                                        EL640
02787                                                                   EL640
02788  3940-BILL-GENERIC-DELETE.                                        EL640
02789      EXEC CICS HANDLE CONDITION                                   EL640
02790          NOTFND (3940-EXIT)                                       EL640
02791      END-EXEC.                                                       CL*11
02792                                                                   EL640
02793      MOVE PI-COMPANY-CD          TO ERBILL-CO-CD.                 EL640
02794      MOVE PI-CR-CARRIER          TO ERBILL-CARRIER.               EL640
02795      MOVE PI-CR-GROUPING         TO ERBILL-GROUP.                 EL640
02796      MOVE PI-SAV-ACCT            TO ERBILL-ACCT.                  EL640
02797                                                                   EL640
02798      IF ST-ACCNT-CNTL OR ACCNT-CNTL                               EL640
02799         MOVE ZEROS               TO ERBILL-CARRIER                EL640
02800                                     ERBILL-GROUP.                    CL*11
02801                                                                   EL640
02802      IF CARR-ST-ACCNT-CNTL OR CARR-ACCNT-CNTL                     EL640
02803         MOVE ZEROS               TO ERBILL-GROUP.                 EL640
02804                                                                   EL640
02805      EXEC CICS DELETE                                             EL640
02806          DATASET   (ERBILL-FILE-ID)                                  CL*11
02807          RIDFLD    (ERBILL-KEY)                                      CL*11
02808          KEYLENGTH (18)                                              CL**9
02809          GENERIC                                                  EL640
02810      END-EXEC.                                                       CL*11
02811                                                                      CL*11
02812  3940-EXIT.                                                       EL640
02813      EXIT.                                                        EL640
02814                                                                   EL640
02815  3990-EXIT.                                                       EL640
02816      EXIT.                                                        EL640
02817      EJECT                                                        EL640
02818 ******************************************************************EL640
02819 *                                                                *EL640
02820 *   ALL I/O FOR THE VARIOUS FILES OTHER THAN THE COMPENSATION    *EL640
02821 *   MASTER IS DONE IN THE FOLLOWING SECTIONS BEGINNING WITH 4XXX *EL640
02822 *   WERE THE XXX INDICATES NUMERIC VALUES.                       *EL640
02823 *                                                                *EL640
02824 ******************************************************************EL640
02825  4000-PNDB-START-BROWSE.                                          EL640
02826      MOVE PI-COMPANY-CD          TO ERPNDB-CO-CD-A1.              EL640
02827      MOVE PI-SAV-CARR            TO ERPNDB-CARR                   EL640
02828      MOVE PI-SAV-GROUP           TO ERPNDB-GROUP.                 EL640
02829      MOVE PI-SAV-STATE           TO ERPNDB-STATE.                 EL640
02830      MOVE PI-SAV-ACCT            TO ERPNDB-ACCT.                  EL640
02831      MOVE LOW-VALUES             TO ERPNDB-CERT.                  EL640
02832                                                                      CL*11
02833      EXEC CICS HANDLE CONDITION                                   EL640
02834          NOTFND (4010-REC-NOT-FND)                                EL640
02835      END-EXEC.                                                       CL*11
02836                                                                      CL*11
02837      EXEC CICS STARTBR                                            EL640
02838          DATASET(ERPNDB-ALT-FILE-ID)                              EL640
02839          RIDFLD (ERPNDB-ALT-KEY)                                     CL*11
02840          GTEQ                                                     EL640
02841      END-EXEC.                                                       CL*11
02842                                                                      CL*11
02843      GO TO 4090-EXIT.                                             EL640
02844                                                                   EL640
02845  4010-REC-NOT-FND.                                                EL640
02846      MOVE 'Y'                    TO PNDB-EOF-SW.                  EL640
02847                                                                   EL640
02848  4090-EXIT.                                                       EL640
02849      EXIT.                                                        EL640
02850      EJECT                                                        EL640
02851  4100-PNDB-READ-NEXT.                                             EL640
02852      EXEC CICS HANDLE CONDITION                                   EL640
02853          ENDFILE (4110-END-OF-FILE)                               EL640
02854      END-EXEC.                                                       CL*11
02855                                                                      CL*11
02856      EXEC CICS READNEXT                                           EL640
02857          SET     (ADDRESS OF PENDING-BUSINESS)                       CL*33
02858          DATASET (ERPNDB-ALT-FILE-ID)                             EL640
02859          RIDFLD  (ERPNDB-ALT-KEY)                                    CL*11
02860      END-EXEC.                                                       CL*11
02861                                                                      CL*11
02862      GO TO 4190-EXIT.                                             EL640
02863                                                                   EL640
02864  4110-END-OF-FILE.                                                EL640
02865      MOVE 'Y'                    TO PNDB-EOF-SW.                  EL640
02866                                                                   EL640
02867  4190-EXIT.                                                       EL640
02868      EXIT.                                                        EL640
02869                                                                      CL*40
02870      EJECT                                                        EL640
02871  4200-PNDB-REWRITE.                                               EL640
02872      MOVE PB-COMPANY-CD          TO ERPNDB-CO-CD.                 EL640
02873      MOVE PB-ENTRY-BATCH         TO ERPNDB-BATCH.                 EL640
02874      MOVE PB-BATCH-SEQ-NO        TO ERPNDB-SEQ-NO.                EL640
02875      MOVE PB-BATCH-CHG-SEQ-NO    TO ERPNDB-CHG-SEQ-NO.            EL640
02876                                                                   EL640
02877      EXEC CICS ENDBR                                              EL640
02878          DATASET (ERPNDB-ALT-FILE-ID)                                CL*11
02879      END-EXEC.                                                       CL*11
02880                                                                   EL640
02881      EXEC CICS HANDLE CONDITION                                      CL*33
02882          NOTFND   (4270-PNDB-NOTFIND)                                CL*33
02883          NOTOPEN  (4280-PNDB-FILE-NOTOPEN)                           CL*33
02884      END-EXEC.                                                       CL*33
02885                                                                      CL*33
02886      EXEC CICS READ                                               EL640
02887          SET     (ADDRESS OF PENDING-BUSINESS)                       CL*33
02888          DATASET (ERPNDB-FILE-ID)                                 EL640
02889          RIDFLD  (ERPNDB-PRIME-KEY)                                  CL*11
02890          UPDATE                                                   EL640
02891      END-EXEC.                                                       CL*11
02892                                                                      CL*11
02893      IF PB-ALT-CHG-SEQ-NO NOT = ZEROS                             EL640
02894          MOVE 'R'                TO PB-BILLING-STATUS.            EL640
02895                                                                      CL*11
02896      IF PI-VOID-BILL                                              EL640
02897          MOVE LOW-VALUES         TO PB-BILLED-DT                  EL640
02898          MOVE ZEROS              TO PB-CHG-COUNT                  EL640
02899      ELSE                                                         EL640
02900          MOVE WS-CURRENT-DATE    TO PB-BILLED-DT.                 EL640
02901                                                                   EL640
02902      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.             EL640
02903      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.         EL640
02904      MOVE WS-CURRENT-DATE        TO PB-LAST-MAINT-DT.             EL640
02905                                                                      CL*11
02906      EXEC CICS REWRITE                                            EL640
02907          DATASET (ERPNDB-FILE-ID)                                 EL640
02908          FROM    (PENDING-BUSINESS)                                  CL*11
02909      END-EXEC.                                                       CL*11
02910                                                                   EL640
02911      EXEC CICS STARTBR                                            EL640
02912          DATASET (ERPNDB-ALT-FILE-ID)                                CL*11
02913          RIDFLD  (ERPNDB-ALT-KEY)                                    CL*11
02914          GTEQ                                                     EL640
02915      END-EXEC.                                                       CL*11
02916                                                                   EL640
02917      PERFORM 4100-PNDB-READ-NEXT THRU 4190-EXIT.                     CL**3
02918                                                                      CL*33
02919      GO TO 4290-EXIT.                                                CL*33
02920                                                                      CL*33
02921  4270-PNDB-NOTFIND.                                                  CL*33
02922      MOVE ER-2212                TO EMI-ERROR.                       CL*33
02923      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*33
02924      GO TO 4290-EXIT.                                                CL*33
02925                                                                      CL*33
02926  4280-PNDB-FILE-NOTOPEN.                                             CL*33
02927      MOVE -1                     TO ACARIERL.                        CL*33
02928      MOVE ER-2210                TO EMI-ERROR.                       CL*33
02929      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*33
02930                                                                   EL640
02931  4290-EXIT.                                                       EL640
02932      EXIT.                                                        EL640
02933      EJECT                                                        EL640
02934  4300-READ-ACCOUNT-MASTER.                                        EL640
02935      MOVE PI-COMPANY-CD          TO ERACCT-A-CO-CD.               EL640
02936      MOVE PI-SAV-CARR            TO ERACCT-A-CARRIER.             EL640
02937      MOVE PI-SAV-GROUP           TO ERACCT-A-GROUPING.            EL640
02938      MOVE PI-SAV-STATE           TO ERACCT-A-STATE.               EL640
02939      MOVE PI-SAV-ACCT            TO ERACCT-A-ACCOUNT.             EL640
02940      MOVE PI-SAV-EXP-DT          TO ERACCT-A-EXP-DATE.            EL640
02941                                                                      CL*11
02942      MOVE SPACES                 TO WS-AM-ZIP.                       CL*18
02943                                                                      CL*18
02944      EXEC CICS HANDLE CONDITION                                   EL640
02945          NOTFND   (4370-ACCOUNT-INVALID)                          EL640
02946          NOTOPEN  (4380-ACCT-FILE-NOTOPEN)                        EL640
02947      END-EXEC.                                                       CL*11
02948                                                                      CL*11
02949      EXEC CICS READ                                               EL640
02950          DATASET   (ERACCT-ALT-FILE-ID)                           EL640
02951          SET       (ADDRESS OF ACCOUNT-MASTER)                       CL*33
02952          RIDFLD    (ERACCT-ALT-KEY)                               EL640
02953          GTEQ                                                     EL640
02954      END-EXEC.                                                       CL*11
02955                                                                      CL*18
02956      IF AM-CANADIAN-POST-CODE                                        CL*18
02957          MOVE AM-CAN-POSTAL-1    TO WS-AM-CAN-POST-1                 CL*18
02958          MOVE AM-CAN-POSTAL-2    TO WS-AM-CAN-POST-2                 CL*18
02959      ELSE                                                            CL*18
02960          MOVE AM-ZIP-PRIME       TO WS-AM-ZIP-PRIME                  CL*18
02961          IF AM-ZIP-PLUS4 NOT = ZEROS  AND  SPACES                    CL*18
02962              MOVE '-'            TO WS-AM-ZIP-DASH                   CL*18
02963              MOVE AM-ZIP-PLUS4   TO WS-AM-ZIP-PLUS4.                 CL*18
02964                                                                   EL640
02965      IF AM-REMIT-TO NOT NUMERIC                                   EL640
02966          MOVE +00 TO AM-REMIT-TO.                                    CL*11
02967                                                                   EL640
02968      IF AM-REMIT-TO = ZEROS                                          CL*11
02969          MOVE ER-2385            TO EMI-ERROR                     EL640
02970          MOVE -1                 TO AACCTL                        EL640
02971          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*11
02972                                                                   EL640
02973      MOVE AM-CARRIER             TO PI-CARRIER.                   EL640
02974      MOVE AM-GROUPING            TO PI-GROUPING.                  EL640
02975      MOVE AM-STATE               TO PI-STATE.                     EL640
02976      MOVE AM-ACCOUNT             TO PI-ACCOUNT.                   EL640
02977                                                                   EL640
02978      MOVE AM-AGT (AM-REMIT-TO)   TO PI-COMP-FIN-RESP.                CL*33
02979                                                                   EL640
02980      IF PI-ZERO-CARRIER  OR                                          CL*11
02981         PI-ZERO-CAR-GROUP                                            CL*11
02982          MOVE ZEROS              TO PI-COMP-CARRIER               EL640
02983      ELSE                                                         EL640
02984          MOVE AM-CARRIER         TO PI-COMP-CARRIER.              EL640
02985                                                                   EL640
02986      IF PI-ZERO-GROUPING  OR                                         CL*11
02987         PI-ZERO-CAR-GROUP                                            CL*11
02988          MOVE ZEROS              TO PI-COMP-GROUPING              EL640
02989      ELSE                                                         EL640
02990          MOVE AM-GROUPING        TO PI-COMP-GROUPING.             EL640
02991                                                                   EL640
02992      IF CARR-GROUP-ST-ACCNT-CNTL                                     CL*40
02993          MOVE AM-CARRIER         TO PI-CR-CARRIER                 EL640
02994          MOVE AM-GROUPING        TO PI-CR-GROUPING                EL640
02995          MOVE AM-STATE           TO PI-CR-STATE.                  EL640
02996                                                                   EL640
02997      IF ST-ACCNT-CNTL                                                CL*40
02998          MOVE ZEROS              TO PI-CR-CARRIER                 EL640
02999                                     PI-CR-GROUPING                   CL*11
03000          MOVE AM-STATE           TO PI-CR-STATE.                  EL640
03001                                                                   EL640
03002      IF CARR-ST-ACCNT-CNTL                                           CL*40
03003          MOVE AM-CARRIER         TO PI-CR-CARRIER                 EL640
03004          MOVE ZEROS              TO PI-CR-GROUPING                EL640
03005          MOVE AM-STATE           TO PI-CR-STATE.                  EL640
03006                                                                   EL640
03007      IF ACCNT-CNTL                                                   CL*40
03008          MOVE ZEROS              TO PI-CR-CARRIER                 EL640
03009                                     PI-CR-GROUPING                   CL*11
03010                                     PI-CR-STATE.                     CL*11
03011                                                                   EL640
03012      IF CARR-ACCNT-CNTL                                              CL*40
03013          MOVE AM-CARRIER         TO PI-CR-CARRIER                 EL640
03014          MOVE ZEROS              TO PI-CR-GROUPING                EL640
03015                                     PI-CR-STATE.                     CL*11
03016                                                                   EL640
03017      MOVE AM-EXPIRATION-DT       TO ERACCT-A-EXP-DATE.            EL640
03018                                                                   EL640
03019      IF AM-AGT (AM-REMIT-TO) = PI-SAV-ACCT                           CL*40
03020          MOVE 'Y'                TO ACCOUNT-FIN-RESP-SW              CL*33
03021      ELSE                                                            CL*33
03022          MOVE 'N'                TO ACCOUNT-FIN-RESP-SW.             CL*33
03023                                                                      CL*33
03024      IF PI-VOID-BILL                                              EL640
03025          MOVE AM-EXPIRATION-DT   TO WS-SAV-EXP-DT (DTNDX)         EL640
03026          IF DTNDX < 10                                               CL*40
03027              SET DTNDX UP BY 1                                    EL640
03028              GO TO 4390-EXIT                                      EL640
03029          ELSE                                                     EL640
03030              GO TO 4390-EXIT.                                     EL640
03031                                                                   EL640
03032      IF PI-COMPANY-CD = AM-COMPANY-CD-A1 AND                         CL*11
03033         PI-SAV-CARR   = AM-VG-CARRIER    AND                         CL*11
03034         PI-SAV-GROUP  = AM-VG-GROUPING   AND                         CL*11
03035         PI-SAV-STATE  = AM-VG-STATE      AND                         CL*11
03036         PI-SAV-ACCT   = AM-VG-ACCOUNT                                CL*11
03037           NEXT SENTENCE                                              CL*11
03038      ELSE                                                            CL*11
03039           GO TO 4370-ACCOUNT-INVALID.                                CL*11
03040                                                                      CL*11
03041      IF PI-GA-BILLING                                                CL*11
03042          NEXT SENTENCE                                               CL*11
03043      ELSE                                                            CL*33
03044          GO TO 4390-EXIT.                                            CL**4
03045                                                                   EL640
03046      IF AM-AGT (AM-REMIT-TO) = PI-SAV-ACCT                           CL*40
03047          MOVE 'Y'                TO ACCOUNT-FIN-RESP-SW              CL*33
03048      ELSE                                                            CL*33
03049          MOVE 'N'                TO ACCOUNT-FIN-RESP-SW.          EL640
03050                                                                   EL640
03051      GO TO 4390-EXIT.                                             EL640
03052                                                                      CL**4
03053  4370-ACCOUNT-INVALID.                                            EL640
03054      MOVE -1                     TO AACCTL.                       EL640
03055                                                                      CL**4
03056      MOVE SPACES                 TO PI-CARRIER                       CL*30
03057                                     PI-GROUPING                      CL*30
03058                                     PI-STATE                         CL*30
03059                                     PI-ACCOUNT.                      CL*30
03060                                                                      CL*30
03061      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL640
03062          MOVE AL-UABON           TO ACARIERA                         CL*33
03063                                     AGROUPA                          CL*33
03064                                     ASTATEA                          CL*33
03065                                     AACCTA                           CL*33
03066      ELSE                                                         EL640
03067          IF ST-ACCNT-CNTL                                         EL640
03068              MOVE AL-UABON       TO ASTATEA                          CL*33
03069                                     AACCTA                           CL*33
03070          ELSE                                                     EL640
03071              IF CARR-ST-ACCNT-CNTL                                EL640
03072                  MOVE AL-UABON   TO ACARIERA                         CL*33
03073                                     ASTATEA                          CL*33
03074                                     AACCTA                           CL*33
03075              ELSE                                                 EL640
03076                  IF ACCNT-CNTL                                    EL640
03077                      MOVE AL-UABON                                   CL*33
03078                                  TO AACCTA                           CL*33
03079                  ELSE                                             EL640
03080                      MOVE AL-UABON                                   CL*33
03081                                  TO ACARIERA                         CL*33
03082                                     AACCTA.                          CL*33
03083                                                                      CL*33
03084      MOVE ER-2210                TO EMI-ERROR.                    EL640
03085      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL640
03086      GO TO 4390-EXIT.                                             EL640
03087                                                                      CL*11
03088  4380-ACCT-FILE-NOTOPEN.                                          EL640
03089      MOVE -1                     TO ACARIERL.                     EL640
03090      MOVE ER-2215                TO EMI-ERROR.                    EL640
03091      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL640
03092                                                                      CL*11
03093  4390-EXIT.                                                       EL640
03094      EXIT.                                                        EL640
03095                                                                      CL*40
03096      EJECT                                                        EL640
03097  4400-ERACCT-REWRITE.                                             EL640
03098      IF NOT PI-VOID-BILL                                          EL640
03099          MOVE LOW-VALUES         TO ERACCT-PRIME-KEY              EL640
03100          MOVE AM-COMPANY-CD      TO ERACCT-P-CO-CD                EL640
03101          MOVE AM-CARRIER         TO ERACCT-P-CARRIER              EL640
03102          MOVE AM-GROUPING        TO ERACCT-P-GROUPING             EL640
03103          MOVE AM-STATE           TO ERACCT-P-STATE                EL640
03104          MOVE AM-ACCOUNT         TO ERACCT-P-ACCOUNT              EL640
03105          MOVE AM-EXPIRATION-DT   TO ERACCT-P-EXP-DATE             EL640
03106      ELSE                                                         EL640
03107          MOVE LOW-VALUES         TO ERACCT-PRIME-KEY              EL640
03108          MOVE PI-COMPANY-CD      TO ERACCT-P-CO-CD                EL640
03109          MOVE PI-CARRIER         TO ERACCT-P-CARRIER              EL640
03110          MOVE PI-GROUPING        TO ERACCT-P-GROUPING             EL640
03111          MOVE PI-STATE           TO ERACCT-P-STATE                EL640
03112          MOVE PI-SAV-ACCT        TO ERACCT-P-ACCOUNT              EL640
03113          MOVE PI-SAV-EXP-DT      TO ERACCT-P-EXP-DATE.            EL640
03114                                                                   EL640
03115      EXEC CICS HANDLE CONDITION                                      CL*33
03116          NOTFND   (4470-ACCOUNT-INVALID)                             CL*33
03117          NOTOPEN  (4480-ACCT-FILE-NOTOPEN)                           CL*33
03118      END-EXEC.                                                       CL*33
03119                                                                      CL*33
03120      EXEC CICS READ                                               EL640
03121          DATASET   (ERACCT-FILE-ID)                               EL640
03122          SET       (ADDRESS OF ACCOUNT-MASTER)                       CL*33
03123          RIDFLD    (ERACCT-PRIME-KEY)                             EL640
03124          UPDATE                                                   EL640
03125      END-EXEC.                                                       CL*11
03126                                                                      CL*11
03127      IF PI-VOID-BILL                                              EL640
03128          MOVE AM-AGT (AM-REMIT-TO) TO PI-SAV-REMIT-TO.            EL640
03129                                                                      CL*11
03130      IF PI-VOID-BILL                                              EL640
03131          MOVE SPACE              TO AM-BILLING-STATUS             EL640
03132      ELSE                                                         EL640
03133          MOVE 'B'                TO AM-BILLING-STATUS.            EL640
03134                                                                      CL*11
03135      MOVE PI-PROCESSOR-ID        TO AM-LAST-MAINT-USER.           EL640
03136      MOVE EIBTIME                TO AM-LAST-MAINT-HHMMSS.         EL640
03137      MOVE WS-CURRENT-DATE        TO AM-LAST-MAINT-DT.             EL640
03138                                                                      CL*11
03139      EXEC CICS REWRITE                                            EL640
03140          DATASET (ERACCT-FILE-ID)                                 EL640
03141          FROM    (ACCOUNT-MASTER)                                    CL*11
03142      END-EXEC.                                                       CL*11
03143                                                                      CL*11
03144      GO TO 4490-EXIT.                                                CL*33
03145                                                                      CL*33
03146  4470-ACCOUNT-INVALID.                                               CL*33
03147      MOVE -1                     TO AACCTL.                          CL*33
03148                                                                      CL*33
03149      MOVE SPACES                 TO PI-CARRIER                       CL*33
03150                                     PI-GROUPING                      CL*33
03151                                     PI-STATE                         CL*33
03152                                     PI-ACCOUNT.                      CL*33
03153                                                                      CL*33
03154      IF CARR-GROUP-ST-ACCNT-CNTL                                     CL*33
03155          MOVE AL-UABON           TO ACARIERA                         CL*33
03156                                     AGROUPA                          CL*33
03157                                     ASTATEA                          CL*33
03158                                     AACCTA                           CL*33
03159      ELSE                                                            CL*33
03160          IF ST-ACCNT-CNTL                                            CL*33
03161              MOVE AL-UABON       TO ASTATEA                          CL*33
03162                                     AACCTA                           CL*33
03163          ELSE                                                        CL*33
03164              IF CARR-ST-ACCNT-CNTL                                   CL*33
03165                  MOVE AL-UABON   TO ACARIERA                         CL*33
03166                                     ASTATEA                          CL*33
03167                                     AACCTA                           CL*33
03168              ELSE                                                    CL*33
03169                  IF ACCNT-CNTL                                       CL*33
03170                      MOVE AL-UABON                                   CL*33
03171                                  TO AACCTA                           CL*33
03172                  ELSE                                                CL*33
03173                      MOVE AL-UABON                                   CL*33
03174                                  TO ACARIERA                         CL*33
03175                                     AACCTA.                          CL*33
03176                                                                      CL*33
03177      MOVE ER-2210                TO EMI-ERROR.                       CL*33
03178      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*33
03179      GO TO 4490-EXIT.                                                CL*33
03180                                                                      CL*33
03181  4480-ACCT-FILE-NOTOPEN.                                             CL*33
03182      MOVE -1                     TO ACARIERL.                        CL*33
03183      MOVE ER-2215                TO EMI-ERROR.                       CL*33
03184      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*33
03185                                                                   EL640
03186  4490-EXIT.                                                       EL640
03187      EXIT.                                                        EL640
03188      EJECT                                                        EL640
03189                                                                      CL*33
03190  4500-PYAJ-START-BROWSE.                                          EL640
03191      MOVE PI-COMPANY-CD          TO ERPYAJ-COMP-CD.               EL640
03192                                                                      CL*11
03193      IF NOT PI-ZERO-CARRIER  AND                                     CL*11
03194         NOT PI-ZERO-CAR-GROUP                                        CL*11
03195          MOVE PI-COMP-CARRIER    TO ERPYAJ-CARRIER                EL640
03196      ELSE                                                         EL640
03197          MOVE ZEROS              TO ERPYAJ-CARRIER.               EL640
03198                                                                      CL*11
03199      IF NOT PI-ZERO-GROUPING  AND                                    CL*11
03200         NOT PI-ZERO-CAR-GROUP                                        CL*11
03201          MOVE PI-COMP-GROUPING   TO ERPYAJ-GROUPING               EL640
03202      ELSE                                                         EL640
03203          MOVE ZEROS              TO ERPYAJ-GROUPING.              EL640
03204                                                                      CL*11
03205      MOVE PI-SAV-REMIT-TO        TO ERPYAJ-FIN-RESP.              EL640
03206      MOVE PI-SAV-ACCT            TO ERPYAJ-ACCOUNT.               EL640
03207                                                                      CL*11
03208      IF PI-SAV-REMIT-TO = SPACES                                  EL640
03209          MOVE PI-SAV-ACCT        TO PI-SAV-REMIT-TO               EL640
03210                                     ERPYAJ-FIN-RESP.              EL640
03211                                                                      CL*11
03212      MOVE LOW-VALUES             TO ERPYAJ-RECORD-TYPE.           EL640
03213      MOVE ERPYAJ-KEY             TO ERPYAJ-BROWSE-COMP-KEY.       EL640
03214                                                                      CL*11
03215      EXEC CICS HANDLE CONDITION                                   EL640
03216          NOTFND (4510-REC-NOT-FND)                                EL640
03217      END-EXEC.                                                       CL*11
03218                                                                      CL*11
03219      EXEC CICS STARTBR                                            EL640
03220          DATASET(ERPYAJ-FILE-ID)                                  EL640
03221          RIDFLD (ERPYAJ-KEY)                                         CL*11
03222          GTEQ                                                     EL640
03223      END-EXEC.                                                       CL*11
03224                                                                      CL*11
03225      GO TO 4590-EXIT.                                             EL640
03226                                                                   EL640
03227  4510-REC-NOT-FND.                                                EL640
03228      MOVE 'Y'                    TO PYAJ-EOF-SW.                  EL640
03229                                                                   EL640
03230  4590-EXIT.                                                       EL640
03231      EXIT.                                                        EL640
03232                                                                      CL*40
03233      EJECT                                                        EL640
03234  4600-PYAJ-READ-NEXT.                                             EL640
03235      EXEC CICS HANDLE CONDITION                                   EL640
03236          ENDFILE (4610-END-OF-FILE)                               EL640
03237      END-EXEC.                                                       CL*11
03238                                                                      CL*11
03239      EXEC CICS READNEXT                                           EL640
03240          SET     (ADDRESS OF PENDING-PAY-ADJ)                        CL*33
03241          DATASET (ERPYAJ-FILE-ID)                                 EL640
03242          RIDFLD  (ERPYAJ-KEY)                                        CL*11
03243      END-EXEC.                                                       CL*11
03244                                                                      CL*11
03245      GO TO 4690-EXIT.                                             EL640
03246                                                                   EL640
03247  4610-END-OF-FILE.                                                EL640
03248      MOVE 'Y'                    TO PYAJ-EOF-SW.                  EL640
03249                                                                   EL640
03250  4690-EXIT.                                                       EL640
03251      EXIT.                                                        EL640
03252      EJECT                                                        EL640
03253  4700-PYAJ-REWRITE.                                               EL640
03254      EXEC CICS READ                                               EL640
03255          SET     (ADDRESS OF PENDING-PAY-ADJ)                        CL*33
03256          DATASET (ERPYAJ-FILE-ID)                                 EL640
03257          RIDFLD  (ERPYAJ-KEY)                                        CL*11
03258          UPDATE                                                   EL640
03259      END-EXEC.                                                       CL*11
03260                                                                      CL*11
03261      IF PI-VOID-BILL                                              EL640
03262          MOVE LOW-VALUES         TO PY-BILLED-DATE                EL640
03263                                     PY-AR-DATE                       CL*15
03264          IF ACCOUNT-NOT-FIN-RESP                                     CL*33
03265              IF PY-REMIT-RECEIVED OR PY-ADJ-REM-RECEIVED OR          CL*33
03266                  PY-DEPOSIT OR PY-ADJ-DEPOSIT OR                     CL*33
03267                  PY-ADD-TO-BALANCE                                   CL*33
03268                  ADD PY-ENTRY-AMT  TO  PI-REMITTED                   CL*33
03269              ELSE                                                    CL*33
03270                  IF (PY-CHARGE-TO-AGENT OR PY-ADJ-CHG-TO-AGT)        CL*33
03271                     AND PY-BILLING-CHECK                             CL*33
03272                     ADD PY-ENTRY-AMT TO PI-DISBURSED                 CL*33
03273                  ELSE                                                CL*33
03274                      ADD PY-ENTRY-AMT TO PI-ADJUSTMENTS              CL*33
03275          ELSE                                                        CL*33
03276              NEXT SENTENCE                                           CL*33
03277      ELSE                                                         EL640
03278          MOVE WS-CURRENT-DATE    TO PY-BILLED-DATE.               EL640
03279                                                                      CL*11
03280      MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY.             EL640
03281      MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS.         EL640
03282      MOVE WS-CURRENT-DATE        TO PY-LAST-MAINT-DT.             EL640
03283                                                                      CL*11
03284      EXEC CICS REWRITE                                            EL640
03285          DATASET (ERPYAJ-FILE-ID)                                 EL640
03286          FROM    (PENDING-PAY-ADJ)                                   CL*11
03287      END-EXEC.                                                       CL*11
03288                                                                      CL*11
03289  4790-EXIT.                                                       EL640
03290      EXIT.                                                        EL640
03291                                                                   EL640
03292  4800-PYAJ-END-BROWSE.                                            EL640
03293      EXEC CICS ENDBR                                              EL640
03294          DATASET (ERPYAJ-FILE-ID)                                 EL640
03295      END-EXEC.                                                       CL*11
03296      EJECT                                                        EL640
03297  5000-FORMAT-SCREEN.                                              EL640
03298 *                **********************************************   EL640
03299 *                *  FORMAT THE BILLING TOTALS SCREEN (EL640A) *   EL640
03300 *                **********************************************   EL640
03301      MOVE LOW-VALUES             TO EL640AO.                      EL640
03302                                                                   EL640
03303      IF PI-SAV-ACCT > SPACES                                         CL*40
03304         MOVE PI-SAV-ACCT         TO AACCTO                        EL640
03305         MOVE PI-SAV-CARR         TO ACARIERO                      EL640
03306         MOVE PI-SAV-GROUP        TO AGROUPO                       EL640
03307         MOVE PI-SAV-STATE        TO ASTATEO.                      EL640
03308                                                                   EL640
03309      MOVE PI-BILL-TYPE           TO ABILTYPI.                     EL640
03310      MOVE PI-BILL-ERRS           TO ABILERRI.                     EL640
03311      MOVE AL-UANON               TO AACCTA                        EL640
03312                                     ACARIERA                      EL640
03313                                     AGROUPA                       EL640
03314                                     ASTATEA                       EL640
03315                                     ABILTYPA                      EL640
03316                                     ABILERRA.                     EL640
03317                                                                   EL640
03318      IF ACCOUNT-NOT-FIN-RESP                                         CL*40
03319          MOVE ER-2539            TO EMI-ERROR                     EL640
03320          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL640
03321                                                                   EL640
03322      IF PI-BILLING-BATCHES (1) NOT = SPACES                       EL640
03323          MOVE PI-BILLING-BATCHES (1) TO ABTCH1I                   EL640
03324          MOVE AL-UANON               TO ABTCH1A.                     CL*11
03325                                                                      CL*11
03326      IF PI-BILLING-BATCHES (2) NOT = SPACES                       EL640
03327          MOVE PI-BILLING-BATCHES (2) TO ABTCH2I                   EL640
03328          MOVE AL-UANON               TO ABTCH2A.                     CL*11
03329                                                                      CL*11
03330      IF PI-BILLING-BATCHES (3) NOT = SPACES                       EL640
03331          MOVE PI-BILLING-BATCHES (3) TO ABTCH3I                   EL640
03332          MOVE AL-UANON               TO ABTCH3A.                     CL*11
03333                                                                   EL640
03334      SET ANDX  PINDX            TO 1.                                CL*11
03335                                                                      CL*11
03336  5005-FORMAT-LOOP.                                                EL640
03337      IF PI-BATCH (PINDX) NOT = SPACES                             EL640
03338          MOVE PI-BATCH  (PINDX)  TO BATCH  (ANDX)                    CL*11
03339          MOVE PI-NOBILL (PINDX)  TO NOBILL (ANDX)                 EL640
03340          MOVE PI-BILLED (PINDX)  TO BILL   (ANDX)                    CL*11
03341          MOVE PI-PREV   (PINDX)  TO PREV   (ANDX)                    CL*11
03342          SET ANDX PINDX UP BY 1                                      CL*40
03343          IF ANDX > 6                                                 CL*40
03344              NEXT SENTENCE                                        EL640
03345          ELSE                                                     EL640
03346              GO TO 5005-FORMAT-LOOP.                              EL640
03347                                                                      CL*11
03348      IF MORE-THAN-6-BATCHES                                       EL640
03349          MOVE ER-2472            TO EMI-ERROR                     EL640
03350          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL640
03351          MOVE ZEROS              TO EMI-ERROR.                    EL640
03352                                                                   EL640
03353      IF NOT PI-DATA-BILLED                                        EL640
03354          IF RETURNED-FROM NOT = SPACES                            EL640
03355              GO TO 5080-CONT                                      EL640
03356          ELSE                                                     EL640
03357              MOVE ER-2409        TO EMI-ERROR                     EL640
03358              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL640
03359              GO TO 5090-EXIT.                                     EL640
03360                                                                   EL640
03361      MOVE PI-BAL-FRWD            TO ABALFWDO.                     EL640
03362      MOVE PI-PREMIUM             TO APREMUMO.                     EL640
03363      MOVE PI-REMITTED            TO AREMITO.                      EL640
03364      MOVE PI-TOT-ISS-COMP        TO ACOMPISO.                     EL640
03365      MOVE PI-TOT-CAN-COMP        TO ACOMCANO.                     EL640
03366      COMPUTE PI-ADJUSTMENTS  =   PI-ADJUSTMENTS  +                   CL*33
03367                                  TOT-REPT-CANCEL-FEE.                CL*33
03368      MOVE PI-ADJUSTMENTS         TO AADJUSTO.                        CL*33
03369      MOVE PI-DISBURSED           TO ADISBURO.                     EL640
03370                                                                   EL640
03371      COMPUTE PI-END-BAL = PI-BAL-FRWD +                           EL640
03372                           PI-PREMIUM -                            EL640
03373                           PI-REMITTED -                           EL640
03374                           PI-TOT-ISS-COMP +                       EL640
03375                           PI-TOT-CAN-COMP +                       EL640
03376                           PI-ADJUSTMENTS +                           CL*33
03377                           PI-DISBURSED.                           EL640
03378                                                                      CL*11
03379      MOVE PI-END-BAL             TO ANETDUEO.                     EL640
03380                                                                      CL*11
03381      IF PI-END-BAL NEGATIVE                                       EL640
03382          MOVE OWED-TO-AGENT      TO AENDHDGO                      EL640
03383      ELSE                                                         EL640
03384          IF PI-END-BAL > ZERO                                        CL*40
03385              MOVE AGENT-OWES     TO AENDHDGO                      EL640
03386          ELSE                                                     EL640
03387              MOVE 'BALANCE          =' TO AENDHDGO.               EL640
03388                                                                   EL640
03389  5080-CONT.                                                       EL640
03390      IF RETURNED-FROM NOT = SPACES                                EL640
03391          IF (RETURNED-FROM NOT = XCTL-6401)  AND                     CL*30
03392              (PI-CR-FIN-RESP NOT = SPACES)                           CL*40
03393                MOVE ER-2421        TO EMI-ERROR                      CL*30
03394                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              CL*30
03395                MOVE ZEROS          TO EMI-ERROR                      CL*30
03396          ELSE                                                     EL640
03397              NEXT SENTENCE                                        EL640
03398      ELSE                                                         EL640
03399          IF PI-BILL OR PI-REBILLING                               EL640
03400              MOVE 'Y'            TO COMP-UPDATE-SW                EL640
03401              PERFORM 6000-READ-COMP-MASTER THRU 6090-EXIT         EL640
03402              MOVE 'Y'            TO ELCNTL-UPDATE-SW              EL640
03403              MOVE SPACES         TO ELCNTL-KEY                    EL640
03404              MOVE '1'            TO ELCNTL-REC-TYPE               EL640
03405              PERFORM 6100-READ-CONTROL-FILE THRU 6120-EXIT        EL640
03406              MOVE WS-CURRENT-DATE TO CF-ACCOUNT-MSTR-MAINT-DT     EL640
03407                                      CF-COMPENSATION-MSTR-MAINT-DTEL640
03408              PERFORM 6200-REWRITE-CONTROL-FILE THRU 6290-EXIT.    EL640
03409                                                                   EL640
03410  5090-EXIT.                                                       EL640
03411      EXIT.                                                        EL640
03412                                                                      CL*40
03413      EJECT                                                        EL640
03414  6000-READ-COMP-MASTER.                                           EL640
03415      EXEC CICS HANDLE CONDITION                                   EL640
03416          NOTFND   (6070-NO-COMP-MSTR)                             EL640
03417          NOTOPEN  (6080-COMP-FILE-NOTOPEN)                        EL640
03418      END-EXEC.                                                       CL*11
03419                                                                   EL640
03420      MOVE PI-COMPANY-CD          TO ERCOMP-COMP-CD.               EL640
03421      MOVE PI-COMP-CARRIER        TO ERCOMP-CARRIER                EL640
03422      MOVE PI-COMP-GROUPING       TO ERCOMP-GROUPING               EL640
03423      MOVE PI-COMP-FIN-RESP       TO ERCOMP-FIN-RESP.              EL640
03424                                                                   EL640
03425      MOVE SPACES                 TO WS-CO-ZIP.                       CL*18
03426                                                                      CL*18
03427      IF EIBAID = DFHPF5                                           EL640
03428          MOVE LOW-VALUES         TO ERCOMP-ACCT                   EL640
03429          MOVE 'G'                TO ERCOMP-RECORD-TYPE            EL640
03430      ELSE                                                         EL640
03431          MOVE PI-SAV-ACCT        TO ERCOMP-ACCT                   EL640
03432          MOVE 'A'                TO ERCOMP-RECORD-TYPE.           EL640
03433                                                                      CL*11
03434  6001-READ.                                                       EL640
03435      IF UPDATE-COMP-TOTALS OR PI-VOID-BILL                        EL640
03436          EXEC CICS READ                                           EL640
03437              DATASET   (ERCOMP-FILE-ID)                           EL640
03438              SET       (ADDRESS OF COMPENSATION-MASTER)              CL*33
03439              RIDFLD    (ERCOMP-KEY)                               EL640
03440              UPDATE                                               EL640
03441          END-EXEC                                                    CL*11
03442      ELSE                                                         EL640
03443          EXEC CICS READ                                           EL640
03444              DATASET   (ERCOMP-FILE-ID)                           EL640
03445              SET       (ADDRESS OF COMPENSATION-MASTER)              CL*33
03446              RIDFLD    (ERCOMP-KEY)                               EL640
03447          END-EXEC.                                                   CL*11
03448                                                                      CL**4
03449      IF CO-CANADIAN-POST-CODE                                        CL*18
03450          MOVE CO-CAN-POSTAL-1    TO WS-CO-CAN-POST-1                 CL*18
03451          MOVE CO-CAN-POSTAL-2    TO WS-CO-CAN-POST-2                 CL*18
03452      ELSE                                                            CL*18
03453          MOVE CO-ZIP-PRIME       TO WS-CO-ZIP-PRIME                  CL*18
03454          IF CO-ZIP-PLUS4 NOT = ZEROS  AND  SPACES                    CL*18
03455              MOVE '-'            TO WS-CO-ZIP-DASH                   CL*18
03456              MOVE CO-ZIP-PLUS4   TO WS-CO-ZIP-PLUS4.                 CL*18
03457                                                                      CL*18
03458  6002-CONTINUE.                                                   EL640
03459      IF EIBAID = DFHPF5                                           EL640
03460          GO TO 6090-EXIT.                                         EL640
03461                                                                      CL**4
03462      IF FIRST-TIME                                                   CL**4
03463         MOVE CO-MAIL-NAME        TO WS-ACCT-LINES (1)                CL*33
03464         MOVE CO-ACCT-NAME        TO WS-ACCT-LINES (2)                CL*33
03465         MOVE CO-ADDR-1           TO WS-ACCT-LINES (3)                CL*33
03466         MOVE CO-ADDR-2           TO WS-ACCT-LINES (4)                CL*33
03467         MOVE CO-ADDR-3           TO WS-ACCT-LINES (5)                CL*33
03468         MOVE WS-CO-ZIP           TO WS-ACCT-LINES (6)                CL*33
03469         PERFORM 6400-FORMAT-ACCOUNT-ADDRS THRU 6490-EXIT.            CL**4
03470                                                                      CL**4
03471      IF PI-VOID-BILL                                              EL640
03472          MOVE  PI-ME-MONTH       TO WS-ME-MONTH                      CL*33
03473          MOVE  PI-ME-DAY         TO WS-ME-DAY                        CL*33
03474          MOVE  PI-ME-YEAR        TO WS-ME-YEAR                       CL*33
03475          IF ACCOUNT-FIN-RESP                                         CL*33
03476              MOVE CO-MONTHLY-TOTALS TO CO-CURRENT-MONTHLY-TOTALS     CL*40
03477              MOVE CO-AGING-TOTALS   TO CO-CURRENT-AGING-TOTALS       CL*40
03478              MOVE CO-YTD-TOTALS     TO CO-CURRENT-YTD-TOTALS         CL*40
03479              MOVE CO-LAST-STMT-DT   TO CO-CURRENT-LAST-STMT-DT       CL*40
03480              PERFORM 6060-ERCOMP-REWRITE                          EL640
03481              GO TO 6090-EXIT                                      EL640
03482          ELSE                                                        CL*33
03483              MOVE CO-YTD-TOTALS     TO CO-CURRENT-YTD-TOTALS         CL*40
03484              MOVE CO-LAST-STMT-DT   TO CO-CURRENT-LAST-STMT-DT       CL*40
03485              PERFORM 6060-ERCOMP-REWRITE                          EL640
03486              MOVE LOW-VALUES        TO ERCOMP-ACCT                   CL*40
03487              MOVE 'G'               TO ERCOMP-RECORD-TYPE            CL*40
03488              PERFORM 6001-READ                                       CL*33
03489              COMPUTE CO-CURRENT-CUR-CHG =                            CL*33
03490                      CO-CURRENT-CUR-CHG - PI-PREMIUM                 CL*33
03491              COMPUTE CO-CURRENT-CUR-COM =                            CL*33
03492                      CO-CURRENT-CUR-COM - PI-TOT-ISS-COMP -          CL*33
03493                      PI-TOT-CAN-COMP                                 CL*33
03494              COMPUTE CO-CURRENT-CUR-PMT =                            CL*33
03495                      CO-CURRENT-CUR-PMT - PI-REMITTED +              CL*33
03496                      PI-DISBURSED + PI-ADJUSTMENTS                   CL*33
03497              COMPUTE CO-CURRENT-END-BAL =                            CL*33
03498                      CO-CURRENT-BAL-FWD + CO-CURRENT-CUR-CHG -       CL*33
03499                      CO-CURRENT-CUR-COM - CO-CURRENT-CUR-PMT         CL*33
03500              MOVE CO-AGING-TOTALS   TO  CO-CURRENT-AGING-TOTALS      CL*40
03501              PERFORM 6050-AGING  THRU  6059-AGING-EXIT               CL*33
03502              IF CO-CURRENT-CUR-CHG = ZEROS AND                       CL*33
03503                 CO-CURRENT-CUR-COM = ZEROS AND                       CL*33
03504                 CO-CURRENT-CUR-PMT = ZEROS                           CL*33
03505                  MOVE CO-LAST-STMT-DT TO CO-CURRENT-LAST-STMT-DT     CL*40
03506                  PERFORM 6060-ERCOMP-REWRITE                         CL*33
03507                  GO TO 6090-EXIT                                     CL*33
03508              ELSE                                                    CL*33
03509                  PERFORM 6060-ERCOMP-REWRITE                         CL*33
03510                  GO TO 6090-EXIT.                                    CL*33
03511                                                                      CL*33
03512      IF UPDATE-COMP-TOTALS                                           CL*33
03513          MOVE  PI-ME-MONTH       TO WS-ME-MONTH                      CL*33
03514          MOVE  PI-ME-DAY         TO WS-ME-DAY                        CL*33
03515          MOVE  PI-ME-YEAR        TO WS-ME-YEAR                       CL*33
03516          IF ACCOUNT-FIN-RESP                                         CL*33
03517              IF NOT PI-CHECK-PRODUCED                                CL*33
03518                  IF CO-CURRENT-LAST-STMT-DT = WS-MONTH-END-DATE      CL*33
03519                      MOVE CO-CURRENT-END-BAL                         CL*33
03520                                  TO CO-CURRENT-BAL-FWD               CL*33
03521                      COMPUTE CO-CURRENT-CUR-COM =                    CL*33
03522                              CO-CURRENT-CUR-COM +                    CL*33
03523                              PI-TOT-ISS-COMP - PI-TOT-CAN-COMP       CL*33
03524                      COMPUTE CO-CURRENT-YTD-COM =                    CL*33
03525                              CO-CURRENT-YTD-COM +                    CL*33
03526                              PI-TOT-ISS-COMP - PI-TOT-CAN-COMP       CL*33
03527                      COMPUTE CO-CURRENT-CUR-CHG =                    CL*33
03528                              CO-CURRENT-CUR-CHG + PI-PREMIUM         CL*33
03529                      COMPUTE CO-CURRENT-CUR-PMT =                    CL*33
03530                              CO-CURRENT-CUR-PMT +                    CL*33
03531                              PI-REMITTED - PI-DISBURSED -            CL*33
03532                              PI-ADJUSTMENTS                          CL*33
03533                      COMPUTE CO-CURRENT-END-BAL =                    CL*33
03534                              CO-CURRENT-BAL-FWD + PI-PREMIUM -       CL*33
03535                              PI-TOT-ISS-COMP + PI-TOT-CAN-COMP -     CL*33
03536                              PI-REMITTED + PI-DISBURSED +            CL*33
03537                              PI-ADJUSTMENTS                          CL*33
03538                      PERFORM 6050-AGING THRU 6059-AGING-EXIT         CL*33
03539                      PERFORM 6060-ERCOMP-REWRITE                     CL*33
03540                      GO TO 6090-EXIT                                 CL*33
03541                 ELSE                                                 CL*33
03542                     MOVE CO-CURRENT-END-BAL TO CO-CURRENT-BAL-FWD    CL*40
03543                     MOVE PI-END-BAL         TO CO-CURRENT-END-BAL    CL*40
03544                     COMPUTE CO-CURRENT-CUR-COM =                     CL*33
03545                             PI-TOT-ISS-COMP - PI-TOT-CAN-COMP        CL*33
03546                     ADD CO-CURRENT-CUR-COM TO CO-CURRENT-YTD-COM     CL*40
03547                     MOVE PI-PREMIUM        TO CO-CURRENT-CUR-CHG     CL*40
03548                     COMPUTE CO-CURRENT-CUR-PMT =                     CL*33
03549                              PI-REMITTED - PI-DISBURSED -            CL*33
03550                              PI-ADJUSTMENTS                          CL*33
03551                     PERFORM 6050-AGING THRU 6059-AGING-EXIT          CL*33
03552                     MOVE WS-MONTH-END-DATE                           CL*33
03553                                  TO CO-CURRENT-LAST-STMT-DT          CL*33
03554                     PERFORM 6060-ERCOMP-REWRITE                      CL*33
03555                     GO TO 6090-EXIT                                  CL*33
03556              ELSE                                                    CL*33
03557                  MOVE SPACE      TO PI-CHECK-SW                      CL*33
03558                  MOVE PI-END-BAL TO CO-CURRENT-END-BAL               CL*33
03559                  COMPUTE CO-CURRENT-CUR-CHG = PI-PREMIUM +           CL*33
03560                          (PI-ADJUSTMENTS - PI-DISBURSED)             CL*33
03561                  PERFORM 6060-ERCOMP-REWRITE                         CL*33
03562                  GO TO 6090-EXIT                                     CL*33
03563          ELSE                                                     EL640
03564              COMPUTE CO-CURRENT-YTD-COM =                            CL*33
03565                      CO-CURRENT-YTD-COM + PI-TOT-ISS-COMP            CL*33
03566                                         - PI-TOT-CAN-COMP            CL*33
03567              PERFORM 6060-ERCOMP-REWRITE                          EL640
03568              MOVE LOW-VALUES    TO ERCOMP-ACCT                       CL*33
03569              MOVE 'G'           TO ERCOMP-RECORD-TYPE                CL*33
03570              PERFORM 6001-READ                                       CL*33
03571              IF CO-CURRENT-LAST-STMT-DT = WS-MONTH-END-DATE          CL*33
03572                  MOVE CO-CURRENT-END-BAL TO CO-CURRENT-BAL-FWD       CL*40
03573                  COMPUTE CO-CURRENT-CUR-COM =                        CL*33
03574                          CO-CURRENT-CUR-COM +                        CL*33
03575                          PI-TOT-ISS-COMP - PI-TOT-CAN-COMP           CL*33
03576                  COMPUTE CO-CURRENT-CUR-CHG =                        CL*33
03577                          CO-CURRENT-CUR-CHG + PI-PREMIUM             CL*33
03578                  COMPUTE CO-CURRENT-CUR-PMT =                        CL*33
03579                          CO-CURRENT-CUR-PMT +                        CL*33
03580                          PI-REMITTED - PI-DISBURSED -                CL*33
03581                          PI-ADJUSTMENTS                              CL*33
03582                  COMPUTE CO-CURRENT-END-BAL =                        CL*33
03583                          CO-CURRENT-BAL-FWD + PI-PREMIUM -           CL*33
03584                          PI-TOT-ISS-COMP + PI-TOT-CAN-COMP -         CL*33
03585                          PI-REMITTED + PI-DISBURSED +                CL*33
03586                          PI-ADJUSTMENTS                              CL*33
03587                  PERFORM 6050-AGING THRU 6059-AGING-EXIT             CL*33
03588                  PERFORM 6060-ERCOMP-REWRITE                         CL*33
03589                  GO TO 6090-EXIT                                     CL*33
03590              ELSE                                                    CL*33
03591                  MOVE CO-CURRENT-END-BAL TO CO-CURRENT-BAL-FWD       CL*40
03592                  COMPUTE CO-CURRENT-CUR-COM =                        CL*33
03593                          PI-TOT-ISS-COMP - PI-TOT-CAN-COMP           CL*33
03594                  MOVE PI-PREMIUM         TO CO-CURRENT-CUR-CHG       CL*40
03595                  COMPUTE CO-CURRENT-CUR-PMT =                        CL*33
03596                          PI-REMITTED - PI-DISBURSED -                CL*33
03597                          PI-ADJUSTMENTS                              CL*33
03598                  COMPUTE CO-CURRENT-END-BAL =                        CL*33
03599                          CO-CURRENT-BAL-FWD + PI-PREMIUM -           CL*33
03600                          PI-TOT-ISS-COMP + PI-TOT-CAN-COMP -         CL*33
03601                          PI-REMITTED + PI-DISBURSED +                CL*33
03602                          PI-ADJUSTMENTS                              CL*33
03603                  PERFORM 6050-AGING THRU 6059-AGING-EXIT             CL*33
03604                  MOVE WS-MONTH-END-DATE                              CL*33
03605                                       TO CO-CURRENT-LAST-STMT-DT     CL*40
03606                  PERFORM 6060-ERCOMP-REWRITE                         CL*33
03607                  GO TO 6090-EXIT.                                    CL*33
03608                                                                   EL640
03609      IF PI-TOT-REBILL                                             EL640
03610          MOVE CO-END-BAL           TO PI-BAL-FRWD                    CL*11
03611      ELSE                                                         EL640
03612          MOVE CO-CURRENT-END-BAL   TO PI-BAL-FRWD.                EL640
03613                                                                   EL640
03614      IF PI-UPDATE-FILES                                           EL640
03615        OR (PI-PREVIEW AND APRODSWI = 'Y')                         EL640
03616          NEXT SENTENCE                                            EL640
03617      ELSE                                                         EL640
03618          GO TO 6090-EXIT.                                         EL640
03619                                                                   EL640
03620      IF AM-AGT (ACCOM-SUB) = PI-SAV-REMIT-TO                      EL640
03621          PERFORM 6040-READ-COMP-MASTER THRU 6049-EXIT                CL**2
03622          IF C-RECORD-FOUND                                           CL**2
03623              MOVE CO-ACCT-NAME   TO WS-REMIT-LINES (1)               CL*33
03624              MOVE CO-MAIL-NAME   TO WS-REMIT-LINES (2)               CL*33
03625              MOVE CO-ADDR-1      TO WS-REMIT-LINES (3)               CL*33
03626              MOVE CO-ADDR-2      TO WS-REMIT-LINES (4)               CL*33
03627              MOVE CO-ADDR-3      TO WS-REMIT-LINES (5)               CL*33
03628              MOVE WS-CO-ZIP      TO WS-REMIT-LINES (6)               CL*33
03629              PERFORM 6300-ELIMINATE-LINE-SPACES THRU 6390-EXIT       CL*33
03630              MOVE 'CA'           TO BILLING-DETAIL-TYPE              CL*33
03631              PERFORM 3000-WRITE-BILLING-DETAIL THRU 3990-EXIT        CL*33
03632          ELSE                                                        CL**2
03633              MOVE SPACES         TO ELCNTL-KEY                       CL*33
03634              MOVE '6'            TO ELCNTL-REC-TYPE                  CL*33
03635              MOVE PI-CARRIER     TO ELCNTL-CARRIER                   CL*33
03636              PERFORM 6100-READ-CONTROL-FILE THRU 6120-EXIT           CL*33
03637              MOVE CF-MAIL-TO-NAME   TO WS-REMIT-LINES (1)            CL*40
03638              MOVE CF-IN-CARE-OF     TO WS-REMIT-LINES (2)            CL*40
03639              MOVE CF-ADDRESS-LINE-1 TO WS-REMIT-LINES (3)            CL*40
03640              MOVE CF-ADDRESS-LINE-2 TO WS-REMIT-LINES (4)            CL*40
03641              MOVE CF-CITY-STATE     TO WS-REMIT-LINES (5)            CL*40
03642              MOVE WS-CF-ZIP         TO WS-REMIT-LINES (6)            CL*40
03643              PERFORM 6300-ELIMINATE-LINE-SPACES THRU 6390-EXIT       CL*33
03644              MOVE 'CA'              TO BILLING-DETAIL-TYPE           CL*40
03645              PERFORM 3000-WRITE-BILLING-DETAIL THRU 3990-EXIT        CL*33
03646      ELSE                                                         EL640
03647          MOVE LOW-VALUES         TO ERCOMP-ACCT                   EL640
03648          MOVE 'G'                TO ERCOMP-RECORD-TYPE            EL640
03649          PERFORM 6001-READ                                        EL640
03650          MOVE CO-ACCT-NAME       TO WS-REMIT-LINES (1)            EL640
03651          MOVE CO-MAIL-NAME       TO WS-REMIT-LINES (2)            EL640
03652          MOVE CO-ADDR-1          TO WS-REMIT-LINES (3)            EL640
03653          MOVE CO-ADDR-2          TO WS-REMIT-LINES (4)            EL640
03654          MOVE CO-ADDR-3          TO WS-REMIT-LINES (5)            EL640
03655          MOVE WS-CO-ZIP          TO WS-REMIT-LINES (6)               CL*18
03656          MOVE CO-CURRENT-END-BAL TO PI-BAL-FRWD-GA                   CL*33
03657          PERFORM 6300-ELIMINATE-LINE-SPACES THRU 6390-EXIT        EL640
03658          MOVE 'GA'               TO BILLING-DETAIL-TYPE           EL640
03659          PERFORM 3000-WRITE-BILLING-DETAIL THRU 3990-EXIT.        EL640
03660                                                                      CL*11
03661      GO TO 6090-EXIT.                                             EL640
03662                                                                   EL640
03663  6070-NO-COMP-MSTR.                                               EL640
03664      MOVE ER-2230                TO EMI-ERROR.                    EL640
03665      MOVE -1                     TO AACCTL.                       EL640
03666      MOVE AL-UABON               TO ACARIERA                      EL640
03667                                     AGROUPA                       EL640
03668                                     AACCTA.                       EL640
03669      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL640
03670      GO TO 6090-EXIT.                                             EL640
03671                                                                   EL640
03672  6080-COMP-FILE-NOTOPEN.                                          EL640
03673      MOVE -1                     TO APFNTERL                      EL640
03674      MOVE ER-2233                TO EMI-ERROR.                    EL640
03675      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL640
03676                                                                   EL640
03677  6090-EXIT.                                                       EL640
03678      EXIT.                                                        EL640
03679      EJECT                                                        EL640
03680                                                                      CL*33
03681  6040-READ-COMP-MASTER.                                              CL*33
03682      EXEC CICS HANDLE CONDITION                                      CL*33
03683          NOTFND   (6045-NO-COMP-MSTR)                                CL*33
03684          NOTOPEN  (6045-COMP-FILE-NOTOPEN)                           CL*33
03685      END-EXEC.                                                       CL*33
03686                                                                      CL*33
03687      MOVE 'Y'                    TO C-RECORD-FOUND-SW.               CL*33
03688      MOVE PI-COMPANY-CD          TO ERCOMP-COMP-CD.                  CL*33
03689      MOVE PI-COMP-CARRIER        TO ERCOMP-CARRIER                   CL*33
03690      MOVE PI-COMP-GROUPING       TO ERCOMP-GROUPING                  CL*33
03691      MOVE LOW-VALUES             TO ERCOMP-FIN-RESP                  CL*33
03692                                     ERCOMP-ACCT.                     CL*33
03693      MOVE 'C'                    TO ERCOMP-RECORD-TYPE.              CL*33
03694                                                                      CL*33
03695      MOVE SPACES                 TO WS-CO-ZIP.                       CL*33
03696                                                                      CL*33
03697      EXEC CICS READ                                                  CL*33
03698          DATASET   (ERCOMP-FILE-ID)                                  CL*33
03699          SET       (ADDRESS OF COMPENSATION-MASTER)                  CL*33
03700          RIDFLD    (ERCOMP-KEY)                                      CL*33
03701      END-EXEC.                                                       CL*33
03702                                                                      CL*33
03703      IF CO-CANADIAN-POST-CODE                                        CL*33
03704          MOVE CO-CAN-POSTAL-1    TO WS-CO-CAN-POST-1                 CL*33
03705          MOVE CO-CAN-POSTAL-2    TO WS-CO-CAN-POST-2                 CL*33
03706      ELSE                                                            CL*33
03707          MOVE CO-ZIP-PRIME       TO WS-CO-ZIP-PRIME                  CL*33
03708          IF CO-ZIP-PLUS4 NOT = ZEROS  AND  SPACES                    CL*33
03709              MOVE '-'            TO WS-CO-ZIP-DASH                   CL*33
03710              MOVE CO-ZIP-PLUS4   TO WS-CO-ZIP-PLUS4.                 CL*33
03711                                                                      CL*33
03712      GO TO 6049-EXIT.                                                CL*33
03713                                                                      CL*33
03714  6045-COMP-FILE-NOTOPEN.                                             CL*33
03715      MOVE -1                     TO APFNTERL.                        CL*40
03716      MOVE ER-2233                TO EMI-ERROR.                       CL*33
03717      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*33
03718                                                                      CL*33
03719      GO TO 6049-EXIT.                                                CL*33
03720                                                                      CL*33
03721  6045-NO-COMP-MSTR.                                                  CL*33
03722      MOVE 'N'                    TO C-RECORD-FOUND-SW.               CL*33
03723                                                                      CL*33
03724  6049-EXIT.                                                          CL*33
03725      EXIT.                                                           CL*33
03726      EJECT                                                           CL*33
03727                                                                      CL*33
03728  6050-AGING.                                                         CL*33
03729                                                                      CL*33
03730      IF NOT PI-VOID-BILL                                             CL*33
03731          IF CO-CURRENT-LAST-STMT-DT NOT = WS-MONTH-END-DATE          CL*33
03732              ADD  CO-CURRENT-OV60  TO CO-CURRENT-OV90                CL*33
03733              MOVE CO-CURRENT-OV30                                    CL*33
03734                                  TO CO-CURRENT-OV60                  CL*33
03735              MOVE CO-CURRENT-CUR TO CO-CURRENT-OV30.                 CL*33
03736                                                                      CL*33
03737      COMPUTE CO-CURRENT-CUR =                                        CL*33
03738              CO-CURRENT-CUR-CHG - CO-CURRENT-CUR-COM.                CL*33
03739                                                                      CL*33
03740      COMPUTE WS-PAY-ADJ =                                            CL*33
03741              PI-REMITTED - PI-DISBURSED - PI-ADJUSTMENTS.            CL*33
03742                                                                      CL*33
03743      IF WS-PAY-ADJ NEGATIVE                                          CL*33
03744          SUBTRACT WS-PAY-ADJ  FROM  CO-CURRENT-CUR                   CL*33
03745      ELSE                                                            CL*33
03746          SUBTRACT WS-PAY-ADJ  FROM  CO-CURRENT-OV90.                 CL*33
03747                                                                      CL*33
03748      IF CO-CURRENT-CUR NEGATIVE                                      CL*33
03749          ADD CO-CURRENT-CUR  TO  CO-CURRENT-OV90                     CL*33
03750          MOVE ZEROS              TO  CO-CURRENT-CUR.                 CL*33
03751                                                                      CL*33
03752      IF CO-CURRENT-OV90 NEGATIVE                                     CL*33
03753          ADD CO-CURRENT-OV90  TO  CO-CURRENT-OV60                    CL*33
03754          MOVE ZEROS              TO  CO-CURRENT-OV90.                CL*33
03755                                                                      CL*33
03756      IF CO-CURRENT-OV60 NEGATIVE                                     CL*33
03757          ADD CO-CURRENT-OV60  TO  CO-CURRENT-OV30                    CL*33
03758          MOVE ZEROS              TO  CO-CURRENT-OV60.                CL*33
03759                                                                      CL*33
03760      IF CO-CURRENT-OV30 NEGATIVE                                     CL*33
03761          ADD CO-CURRENT-OV30  TO  CO-CURRENT-CUR                     CL*33
03762          MOVE ZEROS              TO  CO-CURRENT-OV30.                CL*33
03763                                                                      CL*33
03764  6059-AGING-EXIT.                                                    CL*33
03765      EXIT.                                                           CL*33
03766      EJECT                                                           CL*33
03767                                                                      CL*33
03768  6060-ERCOMP-REWRITE.                                                CL*33
03769                                                                      CL*33
03770      EXEC CICS REWRITE                                               CL*33
03771          DATASET (ERCOMP-FILE-ID)                                    CL*33
03772          FROM    (COMPENSATION-MASTER)                               CL*33
03773      END-EXEC.                                                       CL*40
03774                                                                      CL*33
03775  6100-READ-CONTROL-FILE.                                          EL640
03776      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            EL640
03777      MOVE +0                     TO ELCNTL-SEQ-NO.                EL640
03778                                                                   EL640
03779      MOVE SPACES                 TO WS-CF-ZIP.                       CL*18
03780                                                                      CL*18
03781      EXEC CICS HANDLE CONDITION                                   EL640
03782          NOTFND   (6110-NO-RECORD)                                EL640
03783      END-EXEC.                                                       CL*11
03784                                                                   EL640
03785      IF NOT ELCNTL-UPDATE                                         EL640
03786          EXEC CICS READ                                           EL640
03787              DATASET (ELCNTL-FILE-ID)                             EL640
03788              SET     (ADDRESS OF CONTROL-FILE)                       CL*33
03789              RIDFLD  (ELCNTL-KEY)                                    CL*11
03790          END-EXEC                                                    CL*11
03791      ELSE                                                         EL640
03792          MOVE SPACE              TO ELCNTL-UPDATE-SW              EL640
03793          EXEC CICS READ                                           EL640
03794              DATASET (ELCNTL-FILE-ID)                             EL640
03795              SET     (ADDRESS OF CONTROL-FILE)                       CL*33
03796              RIDFLD  (ELCNTL-KEY)                                    CL*11
03797              UPDATE                                               EL640
03798          END-EXEC.                                                   CL*33
03799                                                                      CL*18
03800      IF CF-ZIP-CODE-NUM NOT NUMERIC                                  CL*18
03801          MOVE ZEROS              TO CF-ZIP-CODE-NUM.                 CL*18
03802                                                                      CL*19
03803      IF CF-ZIP-CODE-NUM NOT = ZEROS                                  CL*18
03804          MOVE CF-ZIP-CODE-NUM    TO WORK-ZIP                         CL*18
03805          IF WORK-ZIPR1-4 = ZEROS                                     CL*19
03806              MOVE WORK-ZIPR1-5   TO WORK-ZIPR2-5                     CL*19
03807              MOVE ZEROS          TO WORK-ZIPR2-4                     CL*19
03808              MOVE WORK-ZIP       TO CF-ZIP-CODE                      CL*19
03809          ELSE                                                        CL*33
03810              MOVE WORK-ZIP       TO CF-ZIP-CODE.                     CL*19
03811                                                                      CL*18
03812      IF CF-CANADIAN-POST-CODE                                        CL*18
03813          MOVE CF-CAN-POSTAL-1    TO WS-CF-CAN-POST-1                 CL*18
03814          MOVE CF-CAN-POSTAL-2    TO WS-CF-CAN-POST-2                 CL*18
03815      ELSE                                                            CL*18
03816          MOVE CF-ZIP-PRIME       TO WS-CF-ZIP-PRIME                  CL*18
03817          IF CF-ZIP-PLUS4 NOT = ZEROS  AND  SPACES                    CL*18
03818              MOVE '-'            TO WS-CF-ZIP-DASH                   CL*18
03819              MOVE CF-ZIP-PLUS4   TO WS-CF-ZIP-PLUS4.                 CL*18
03820                                                                      CL*11
03821      GO TO 6120-EXIT.                                             EL640
03822                                                                   EL640
03823  6110-NO-RECORD.                                                  EL640
03824      IF ELCNTL-REC-TYPE = 1                                       EL640
03825          MOVE ER-0022            TO EMI-ERROR                     EL640
03826      ELSE                                                         EL640
03827          MOVE ER-2208            TO EMI-ERROR                     EL640
03828          MOVE -1                 TO ACARIERL                      EL640
03829          MOVE AL-UABON           TO ACARIERA                      EL640
03830          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*11
03831                                                                   EL640
03832  6120-EXIT.                                                       EL640
03833      EXIT.                                                        EL640

       6130-FIND-BENEFIT-CD.

           EXEC CICS READ
              DATASET (ELCNTL-FILE-ID)
              SET     (ADDRESS OF CONTROL-FILE)
              RIDFLD  (ELCNTL-KEY)
              GTEQ
              RESP    (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              IF (CF-COMPANY-ID = PI-COMPANY-ID)
                 AND (CF-RECORD-TYPE = '5')
                 PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                    (S1 > +8)
                    OR (CF-BENEFIT-CODE (S1) = CLAS-LOOK)
                 END-PERFORM
                 IF S1 NOT > +8
                    MOVE CF-BENEFIT-CATEGORY (S1)
                                       TO WS-AH-CATEGORY
                    GO TO 6140-EXIT
                 END-IF
              END-IF
           END-IF

           .
       6140-NO-RECORD.

           MOVE ER-2208                TO EMI-ERROR
           MOVE -1                     TO ACARIERL
           MOVE AL-UABON               TO ACARIERA
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT

           .
       6140-EXIT.
           EXIT.

03835  6200-REWRITE-CONTROL-FILE.                                       EL640
03836                                                                      CL*11
03837      EXEC CICS REWRITE                                            EL640
03838          DATASET (ELCNTL-FILE-ID)                                 EL640
03839          FROM    (CONTROL-FILE)                                      CL*11
03840      END-EXEC.                                                       CL*11
03841                                                                      CL*11
03842  6290-EXIT.                                                       EL640
03843      EXIT.                                                        EL640
03844                                                                      CL*40
03845      EJECT                                                        EL640
03846  6300-ELIMINATE-LINE-SPACES.                                      EL640
03847      IF WS-REMIT-LINES (1) = SPACES                               EL640
03848          MOVE WS-REMIT-LINES (2)  TO WS-REMIT-LINES (1)              CL*11
03849          MOVE WS-REMIT-LINES (3)  TO WS-REMIT-LINES (2)              CL*11
03850          MOVE WS-REMIT-LINES (4)  TO WS-REMIT-LINES (3)              CL*11
03851          MOVE WS-REMIT-LINES (5)  TO WS-REMIT-LINES (4)              CL*11
03852          MOVE WS-REMIT-LINES (6)  TO WS-REMIT-LINES (5)              CL*11
03853          MOVE SPACES              TO WS-REMIT-LINES (6)              CL*11
03854          GO TO 6300-ELIMINATE-LINE-SPACES.                           CL*11
03855                                                                   EL640
03856      IF WS-REMIT-LINES (2) = SPACES AND                           EL640
03857         WS-REMIT-LINES (3) = SPACES AND                           EL640
03858         WS-REMIT-LINES (4) = SPACES AND                           EL640
03859         WS-REMIT-LINES (5) = SPACES AND                           EL640
03860         WS-REMIT-LINES (6) = SPACES                               EL640
03861           GO TO 6350-MOVE-ZIP.                                       CL*11
03862                                                                   EL640
03863      IF WS-REMIT-LINES (2) = SPACES                               EL640
03864          MOVE WS-REMIT-LINES (3)  TO WS-REMIT-LINES (2)              CL*11
03865          MOVE WS-REMIT-LINES (4)  TO WS-REMIT-LINES (3)              CL*11
03866          MOVE WS-REMIT-LINES (5)  TO WS-REMIT-LINES (4)              CL*11
03867          MOVE WS-REMIT-LINES (6)  TO WS-REMIT-LINES (5)              CL*11
03868          MOVE SPACES              TO WS-REMIT-LINES (6)              CL*11
03869          GO TO 6300-ELIMINATE-LINE-SPACES.                           CL*11
03870                                                                   EL640
03871      IF WS-REMIT-LINES (3) = SPACES AND                           EL640
03872         WS-REMIT-LINES (4) = SPACES AND                           EL640
03873         WS-REMIT-LINES (5) = SPACES AND                           EL640
03874         WS-REMIT-LINES (6) = SPACES                               EL640
03875           GO TO 6350-MOVE-ZIP.                                       CL*11
03876                                                                   EL640
03877      IF WS-REMIT-LINES (3) = SPACES                               EL640
03878          MOVE WS-REMIT-LINES (4)  TO WS-REMIT-LINES (3)              CL*11
03879          MOVE WS-REMIT-LINES (5)  TO WS-REMIT-LINES (4)              CL*11
03880          MOVE WS-REMIT-LINES (6)  TO WS-REMIT-LINES (5)              CL*11
03881          MOVE SPACES              TO WS-REMIT-LINES (6)              CL*11
03882          GO TO 6300-ELIMINATE-LINE-SPACES.                           CL*11
03883                                                                   EL640
03884      IF WS-REMIT-LINES (4) = SPACES AND                           EL640
03885         WS-REMIT-LINES (5) = SPACES AND                           EL640
03886         WS-REMIT-LINES (6) = SPACES                               EL640
03887           GO TO 6350-MOVE-ZIP.                                       CL*11
03888                                                                   EL640
03889      IF WS-REMIT-LINES (4) = SPACES                               EL640
03890          MOVE WS-REMIT-LINES (5)  TO WS-REMIT-LINES (4)              CL*11
03891          MOVE WS-REMIT-LINES (6)  TO WS-REMIT-LINES (5)              CL*11
03892          MOVE SPACES              TO WS-REMIT-LINES (6)              CL*18
03893          GO TO 6300-ELIMINATE-LINE-SPACES.                           CL*18
03894                                                                   EL640
03895      IF WS-REMIT-LINES (5) = SPACES                               EL640
03896          MOVE WS-REMIT-LINES (6)  TO WS-REMIT-LINES (5)              CL*11
03897          MOVE SPACES              TO WS-REMIT-LINES (6).             CL*11
03898                                                                   EL640
03899  6350-MOVE-ZIP.                                                   EL640
03900      IF WS-REMIT-LINES (6) NOT = SPACES                           EL640
03901          MOVE WS-REMIT-ZIP (6)        TO WS-R-LAST-ZIP (5)           CL*18
03902          MOVE SPACES                  TO WS-REMIT-LINES (6)          CL*18
03903      ELSE                                                            CL*18
03904          IF WS-REMIT-LINES (5) NOT = SPACES                          CL*18
03905              MOVE WS-REMIT-ZIP (5)     TO WS-R-LAST-ZIP (5)          CL*18
03906              MOVE SPACES               TO WS-REMIT-ZIP (5)           CL*18
03907          ELSE                                                        CL*18
03908              IF WS-REMIT-LINES (4) NOT = SPACES                      CL*18
03909                  MOVE WS-REMIT-ZIP (4) TO WS-R-LAST-ZIP (4)          CL*18
03910                  MOVE SPACES           TO WS-REMIT-ZIP (4).          CL*18
03911                                                                      CL*18
03912  6390-EXIT.                                                       EL640
03913      EXIT.                                                           CL**4
03914                                                                      CL*40
03915      EJECT                                                           CL**4
03916  6400-FORMAT-ACCOUNT-ADDRS.                                          CL**4
03917      IF WS-ACCT-ADDR-AREA = SPACES                                   CL**4
03918          GO TO 6490-EXIT.                                            CL*11
03919                                                                      CL**4
03920      IF WS-ACCT-LINES (1) = SPACES                                   CL**4
03921          MOVE WS-ACCT-LINES (2)  TO WS-ACCT-LINES (1)                CL*11
03922          MOVE WS-ACCT-LINES (3)  TO WS-ACCT-LINES (2)                CL*11
03923          MOVE WS-ACCT-LINES (4)  TO WS-ACCT-LINES (3)                CL*11
03924          MOVE WS-ACCT-LINES (5)  TO WS-ACCT-LINES (4)                CL*11
03925          MOVE WS-ACCT-LINES (6)  TO WS-ACCT-LINES (5)                CL*11
03926          MOVE SPACES              TO WS-ACCT-LINES (6)               CL*11
03927          GO TO 6400-FORMAT-ACCOUNT-ADDRS.                            CL*11
03928                                                                      CL**4
03929      IF WS-ACCT-LINES (2) = SPACES AND                               CL**4
03930         WS-ACCT-LINES (3) = SPACES AND                               CL**4
03931         WS-ACCT-LINES (4) = SPACES AND                               CL**4
03932         WS-ACCT-LINES (5) = SPACES AND                               CL**4
03933         WS-ACCT-LINES (6) = SPACES                                   CL**4
03934           GO TO 6450-MOVE-ZIP.                                       CL*11
03935                                                                      CL**4
03936      IF WS-ACCT-LINES (2) = SPACES                                   CL**4
03937          MOVE WS-ACCT-LINES (3)  TO WS-ACCT-LINES (2)                CL*11
03938          MOVE WS-ACCT-LINES (4)  TO WS-ACCT-LINES (3)                CL*11
03939          MOVE WS-ACCT-LINES (5)  TO WS-ACCT-LINES (4)                CL*11
03940          MOVE WS-ACCT-LINES (6)  TO WS-ACCT-LINES (5)                CL*11
03941          MOVE SPACES             TO WS-ACCT-LINES (6)                CL*11
03942          GO TO 6400-FORMAT-ACCOUNT-ADDRS.                            CL*11
03943                                                                      CL**4
03944      IF WS-ACCT-LINES (3) = SPACES AND                               CL**4
03945         WS-ACCT-LINES (4) = SPACES AND                               CL**4
03946         WS-ACCT-LINES (5) = SPACES AND                               CL**4
03947         WS-ACCT-LINES (6) = SPACES                                   CL**4
03948           GO TO 6450-MOVE-ZIP.                                       CL*11
03949                                                                      CL**4
03950      IF WS-ACCT-LINES (3) = SPACES                                   CL**4
03951          MOVE WS-ACCT-LINES (4)  TO WS-ACCT-LINES (3)                CL*11
03952          MOVE WS-ACCT-LINES (5)  TO WS-ACCT-LINES (4)                CL*11
03953          MOVE WS-ACCT-LINES (6)  TO WS-ACCT-LINES (5)                CL*11
03954          MOVE SPACES             TO WS-ACCT-LINES (6)                CL*11
03955          GO TO 6400-FORMAT-ACCOUNT-ADDRS.                            CL*11
03956                                                                      CL**4
03957      IF WS-ACCT-LINES (4) = SPACES AND                               CL**4
03958         WS-ACCT-LINES (5) = SPACES AND                               CL**4
03959         WS-ACCT-LINES (6) = SPACES                                   CL**4
03960           GO TO 6450-MOVE-ZIP.                                       CL*11
03961                                                                      CL**4
03962      IF WS-ACCT-LINES (4) = SPACES                                   CL**4
03963          MOVE WS-ACCT-LINES (5)  TO WS-ACCT-LINES (4)                CL*11
03964          MOVE WS-ACCT-LINES (6)  TO WS-ACCT-LINES (5)                CL*11
03965          MOVE SPACES             TO WS-ACCT-LINES (6)                CL*11
03966          GO TO 6400-FORMAT-ACCOUNT-ADDRS.                            CL*11
03967                                                                      CL**4
03968      IF WS-ACCT-LINES (5) = SPACES                                   CL**4
03969          MOVE WS-ACCT-LINES (6)  TO WS-ACCT-LINES (5)                CL*11
03970          MOVE SPACES             TO WS-ACCT-LINES (6).               CL*11
03971                                                                      CL**4
03972  6450-MOVE-ZIP.                                                      CL**4
03973      IF WS-ACCT-LINES (6) NOT = SPACES                               CL**4
03974          MOVE WS-ACCT-ZIP (6)         TO WS-A-LAST-ZIP (5)           CL*19
03975          MOVE SPACES                  TO WS-ACCT-LINES (6)           CL*18
03976      ELSE                                                            CL*18
03977          IF WS-ACCT-LINES (5) NOT = SPACES                           CL*18
03978              MOVE WS-ACCT-ZIP (5)      TO WS-A-LAST-ZIP (5)          CL*19
03979              MOVE SPACES               TO WS-ACCT-ZIP (5)            CL*18
03980          ELSE                                                        CL*18
03981              IF WS-ACCT-LINES (4) NOT = SPACES                       CL*18
03982                  MOVE WS-ACCT-ZIP (4)  TO WS-A-LAST-ZIP (4)          CL*19
03983                  MOVE SPACES           TO WS-ACCT-ZIP (4).           CL*18
03984                                                                      CL**4
03985  6490-EXIT.                                                          CL**4
03986      EXIT.                                                        EL640
03987      EJECT                                                        EL640
03988  7000-PROCESS-CHECK.                                              EL640
03989 *                *******************************************      EL640
03990 *                *    FORMAT THE CHECK SCREEN (EL640D)     *      EL640
03991 *                *******************************************      EL640
03992      MOVE LOW-VALUES             TO EL640BO.                      EL640
03993                                                                      CL*11
03994      IF PI-SAV-ACCT = PI-SAV-REMIT-TO                             EL640
03995          PERFORM 4300-READ-ACCOUNT-MASTER THRU 4390-EXIT          EL640
03996          IF EMI-ERROR = ZEROS                                     EL640
03997              MOVE AM-NAME        TO BNAMEO                        EL640
03998              MOVE AM-PERSON      TO BADDR1O                       EL640
03999              MOVE AM-ADDRS       TO BADDR2O                       EL640
04000              MOVE AM-CITY        TO BCITYSTO                      EL640
04001              MOVE WS-AM-ZIP      TO BZIPO                            CL*18
04002          ELSE                                                     EL640
04003              MOVE EL640A         TO PI-MAP-NAME                      CL*30
04004              MOVE -1             TO BPFNTERL                      EL640
04005              GO TO 8200-SEND-DATAONLY                             EL640
04006      ELSE                                                         EL640
04007          PERFORM 6000-READ-COMP-MASTER THRU 6090-EXIT             EL640
04008          IF EMI-ERROR = ZEROS                                     EL640
04009              MOVE CO-ACCT-NAME   TO BNAMEO                        EL640
04010              MOVE CO-ADDR-1      TO BADDR1O                       EL640
04011              MOVE CO-ADDR-2      TO BADDR2O                       EL640
04012              MOVE CO-ADDR-3      TO BCITYSTO                      EL640
04013              MOVE WS-CO-ZIP      TO BZIPO                            CL*18
04014          ELSE                                                     EL640
04015              MOVE EL640A         TO PI-MAP-NAME                      CL*30
04016              MOVE -1             TO BPFNTERL                      EL640
04017              GO TO 8200-SEND-DATAONLY.                            EL640
04018                                                                      CL*11
04019      MOVE AL-UNNON               TO BCHKAMTA.                     EL640
04020      MOVE PI-ACCT-NAME           TO BACCNAMO.                     EL640
04021      MOVE PI-SAV-ACCT            TO BACCTO.                       EL640
04022      MOVE PI-COMP-CARRIER        TO BCARIERO.                        CL*14
04023      MOVE PI-COMP-GROUPING       TO BGROUPO.                         CL*14
04024      MOVE PI-SAV-STATE           TO BSTATEO.                      EL640
04025      MOVE PI-MONTH-END-DATE      TO BPAYDT2O                      EL640
04026                                     WS-BEGIN-DATE.                EL640
04027      MOVE '01'                   TO WS-BEGIN-DAY.                 EL640
04028      MOVE WS-BEGIN-DATE          TO BPAYDT1O.                     EL640
04029      MOVE -1                     TO BCHKNOL                       EL640
04030                                                                      CL*39
04031      IF WS-ENTERED-FROM-A                                            CL*39
04032          COMPUTE WORK-CHECK-AMT = PI-END-BAL * -1                    CL*39
04033          MOVE WORK-CHECK-AMT     TO BCHKAMTO                         CL*39
04034      ELSE                                                            CL*39
04035          MOVE WORK-CHECK-AMT     TO BCHKAMTO.                        CL*39
04036                                                                      CL*39
04037      IF WORK-CHECK-AMT NOT > ZEROS                                   CL*40
04038          MOVE ER-3165    TO  EMI-ERROR                               CL*39
04039          MOVE -1         TO  BCHKAMTL                                CL*39
04040          MOVE AL-UNBON   TO  BCHKAMTA                                CL*39
04041          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*39
04042                                                                      CL*39
04043      GO TO 8100-SEND-INITIAL-MAP.                                 EL640
04044      EJECT                                                        EL640
04045                                                                      CL*33
04046  7500-PRODUCE-CHECK.                                              EL640
04047 *                *******************************************      EL640
04048 *                *   PRODUCES A CHECK FOR LATER PRINTING   *      EL640
04049 *                *******************************************      EL640
04050      MOVE SPACES                 TO ELCNTL-KEY.                   EL640
04051      MOVE '1'                    TO ELCNTL-REC-TYPE.              EL640
04052      MOVE 'Y'                    TO ELCNTL-UPDATE-SW.             EL640
04053      PERFORM 6100-READ-CONTROL-FILE THRU 6120-EXIT.               EL640
04054                                                                      CL*11
04055      IF EMI-ERROR NOT = ZEROS                                     EL640
04056          GO TO 8200-SEND-DATAONLY.                                EL640
04057                                                                      CL*11
04058      IF CR-CHECK-NO-AUTO-SEQ                                      EL640
04059          IF CR-CHECK-CNT-RESET-VALUE                              EL640
04060              MOVE CF-CR-CHECK-COUNTER TO BCHKNOI                  EL640
04061              MOVE +1             TO CF-CR-CHECK-COUNTER           EL640
04062              MOVE 6              TO BCHKNOL                       EL640
04063          ELSE                                                     EL640
04064              MOVE CF-CR-CHECK-COUNTER TO BCHKNOI                  EL640
04065              MOVE 6              TO BCHKNOL                       EL640
04066              ADD +1              TO CF-CR-CHECK-COUNTER.          EL640
04067                                                                   EL640
04068      IF CR-CHECK-NO-MANUAL                                        EL640
04069          IF BCHKNOL = ZEROS                                       EL640
04070              MOVE ER-2438        TO EMI-ERROR                     EL640
04071              MOVE -1             TO BCHKNOL                       EL640
04072              MOVE AL-UNBON       TO BCHKNOA                       EL640
04073              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL640
04074              GO TO 8200-SEND-DATAONLY                             EL640
04075          ELSE                                                     EL640
04076              IF BCHKNOI NOT NUMERIC                               EL640
04077                  MOVE ER-2439    TO EMI-ERROR                     EL640
04078                  MOVE -1         TO BCHKNOL                       EL640
04079                  MOVE AL-UNBON   TO BCHKNOA                       EL640
04080                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL640
04081                  GO TO 8200-SEND-DATAONLY.                        EL640
04082                                                                   EL640
04083      PERFORM 7600-CHECK-FORMS THRU 7600-EXIT.                        CL*29
04084                                                                      CL*29
04085      EXEC CICS GETMAIN                                            EL640
04086          SET     (ADDRESS OF PENDING-PAY-ADJ)                        CL*33
04087          LENGTH  (ERPYAJ-LENGTH)                                     CL*14
04088          INITIMG (GETMAIN-SPACE)                                  EL640
04089      END-EXEC.                                                       CL*11
04090                                                                      CL*11
04091      EXEC CICS BIF DEEDIT                                         EL640
04092          FIELD  (BCHKAMTI)                                           CL*11
04093          LENGTH (11)                                                 CL*39
04094      END-EXEC.                                                       CL*11
04095                                                                      CL*30
04096      IF BCHKAMTI NOT > ZEROS                                         CL*40
04097          MOVE ER-3165    TO  EMI-ERROR                               CL*30
04098          MOVE -1         TO  BCHKAMTL                                CL*30
04099          MOVE AL-UNBON   TO  BCHKAMTA                                CL*30
04100          MOVE BCHKAMTI   TO  BCHKAMTO                                CL*39
04101          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*30
04102          GO TO 8200-SEND-DATAONLY.                                   CL*30
04103                                                                      CL*11
04104      MOVE 'Y'                    TO PI-CHECK-SW.                  EL640
04105      MOVE 'PY'                   TO PY-RECORD-ID.                 EL640
04106      MOVE PI-COMPANY-CD          TO PY-COMPANY-CD.                EL640
04107      MOVE PI-COMP-CARRIER        TO PY-CARRIER.                      CL*14
04108      MOVE PI-COMP-GROUPING       TO PY-GROUPING.                     CL*14
04109      MOVE PI-SAV-REMIT-TO        TO PY-FIN-RESP.                  EL640
04110      MOVE PI-SAV-ACCT            TO PY-ACCOUNT.                   EL640
04111      MOVE 'C'                    TO PY-RECORD-TYPE.               EL640
04112      MOVE EIBTIME                TO PY-FILE-SEQ-NO.               EL640
04113                                                                   EL640
CIDMOD     IF PI-COMPANY-ID = CLIENT-ITY                                     000
04115         MOVE PI-MONTH-END-DATE   TO WS-PY-END-DT                     CL*14
04116                                     WS-BEGIN-DATE                    CL*14
04117         MOVE '01'                TO WS-BEGIN-DAY                     CL*14
04118         MOVE WS-BEGIN-DATE       TO WS-PY-BEGIN-DT                   CL*14
04119         MOVE WS-ITY-COMMENT      TO PY-ENTRY-COMMENT                 CL*14
04120      ELSE                                                            CL*14
04121         MOVE WS-CURRENT-DATE-MDY TO WS-PY-CURRENT-DATE               CL*14
04122         MOVE WS-PY-ENTRY-COMMENT TO PY-ENTRY-COMMENT.                CL*14
04123                                                                      CL*14
04124      MOVE BCHKAMTI               TO PY-ENTRY-AMT.                 EL640
04125      ADD BCHKAMTI TO PI-DISBURSED.                                   CL*40
04126      MOVE BFORM1I                TO PY-LETTER (1).                   CL*29
04127      MOVE BFORM2I                TO PY-LETTER (2).                   CL*29
04128      MOVE BFORM3I                TO PY-LETTER (3).                   CL*29
04129                                                                      CL*11
04130      IF NOT CR-CHECK-NO-AT-PRINT                                  EL640
04131          MOVE BCHKNOI            TO PY-CHECK-NUMBER.              EL640
04132                                                                      CL*11
04133      MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY.             EL640
04134      MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS.         EL640
04135      MOVE WS-CURRENT-DATE        TO PY-LAST-MAINT-DT              EL640
04136                                     PY-INPUT-DT.                  EL640
04137      IF PI-BILL OR PI-REBILLING                                   EL640
04138          MOVE DC-BIN-DATE-1      TO PY-BILLED-DATE                EL640
04139      ELSE                                                         EL640
04140          MOVE LOW-VALUES         TO PY-BILLED-DATE                   CL*15
04141                                     PY-AR-DATE.                      CL*15
04142                                                                      CL*11
04143      MOVE ZEROS                  TO PY-CHECK-QUE-CONTROL          EL640
04144                                     PY-CHECK-QUE-SEQUENCE.        EL640
04145      MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT           EL640
04146                                     PY-REPORTED-DT                EL640
04147                                     PY-CHECK-WRITTEN-DT.          EL640
04148      MOVE PI-CR-MONTH-END-DT     TO PY-CREDIT-SELECT-DT.          EL640
04149      MOVE 'B'                    TO PY-CHECK-ORIGIN-SW.           EL640
04150                                                                   EL640
04151      EXEC CICS WRITE                                              EL640
04152          DATASET (ERPYAJ-FILE-ID)                                 EL640
04153          FROM    (PENDING-PAY-ADJ)                                   CL*11
04154          RIDFLD  (PY-CONTROL-PRIMARY)                                CL*11
04155      END-EXEC.                                                       CL*11
04156                                                                      CL*11
04157      IF CR-CHECK-NO-AUTO-SEQ                                      EL640
04158          PERFORM 6200-REWRITE-CONTROL-FILE THRU 6290-EXIT         EL640
04159      ELSE                                                         EL640
04160          EXEC CICS UNLOCK                                         EL640
04161              DATASET (ELCNTL-FILE-ID)                             EL640
04162          END-EXEC.                                                   CL*11
04163                                                                      CL*11
04164      MOVE EL640A                 TO PI-MAP-NAME.                  EL640
04165      PERFORM 5000-FORMAT-SCREEN THRU 5090-EXIT.                   EL640
04166      GO TO 8100-SEND-INITIAL-MAP.                                    CL*39
04167                                                                      CL*33
04168  7600-CHECK-FORMS.                                                   CL*29
04169                                                                      CL*29
04170      IF BFORM1L NOT = ZEROS                                          CL*40
04171              AND                                                     CL*29
04172         BFORM1I NOT = SPACES                                         CL*40
04173          MOVE BFORM1I            TO ELLETR-FORM-NO                   CL*29
04174          PERFORM 7620-CHECK-ON-LETTER THRU 7620-EXIT                 CL*29
04175          IF W-LETTER-NOT-FOUND                                       CL*40
04176              MOVE ER-7389        TO EMI-ERROR                        CL*29
04177              MOVE -1             TO BFORM1L                          CL*29
04178              MOVE AL-UABON       TO BFORM1A                          CL*29
04179              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*29
04180              GO TO 8200-SEND-DATAONLY.                               CL*29
04181                                                                      CL*29
04182      IF BFORM2L NOT = ZEROS                                          CL*40
04183              AND                                                     CL*29
04184         BFORM2I NOT = SPACES                                         CL*40
04185          MOVE BFORM2I            TO ELLETR-FORM-NO                   CL*29
04186          PERFORM 7620-CHECK-ON-LETTER THRU 7620-EXIT                 CL*29
04187                                                                      CL*29
04188          IF W-LETTER-NOT-FOUND                                       CL*40
04189              MOVE ER-7389        TO EMI-ERROR                        CL*29
04190              MOVE -1             TO BFORM2L                          CL*29
04191              MOVE AL-UABON       TO BFORM2A                          CL*29
04192              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*29
04193              GO TO 8200-SEND-DATAONLY.                               CL*29
04194                                                                      CL*29
04195      IF BFORM3L NOT = ZEROS                                          CL*40
04196              AND                                                     CL*29
04197         BFORM3I NOT = SPACES                                         CL*40
04198          MOVE BFORM3I            TO ELLETR-FORM-NO                   CL*29
04199          IF  W-LETTER-NOT-FOUND                                      CL*29
04200              MOVE ER-7389        TO EMI-ERROR                        CL*29
04201              MOVE -1             TO BFORM3L                          CL*29
04202              MOVE AL-UABON       TO BFORM3A                          CL*29
04203              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*29
04204              GO TO 8200-SEND-DATAONLY.                               CL*29
04205                                                                      CL*29
04206      IF BFORM1L = ZEROS                                              CL*40
04207          IF BFORM2L NOT = ZEROS                                      CL*40
04208              MOVE BFORM2L        TO BFORM1L                          CL*29
04209              MOVE BFORM3L        TO BFORM2L                          CL*29
04210              MOVE BFORM2A        TO BFORM1A                          CL*33
04211              MOVE BFORM3A        TO BFORM2A                          CL*29
04212              MOVE BFORM2I        TO BFORM1I                          CL*29
04213              MOVE BFORM3I        TO BFORM2I                          CL*29
04214              MOVE LOW-VALUES     TO BFORM3I                          CL*29
04215                                     BFORM3A                          CL*29
04216              MOVE ZEROS          TO BFORM3L                          CL*29
04217          ELSE                                                        CL*29
04218              IF BFORM3L NOT = ZEROS                                  CL*40
04219                  MOVE BFORM3L    TO BFORM1L                          CL*29
04220                  MOVE BFORM3I    TO BFORM1I                          CL*29
04221                  MOVE BFORM3A    TO BFORM1A                          CL*29
04222                  MOVE LOW-VALUES TO BFORM3I                          CL*29
04223                                     BFORM3A                          CL*29
04224                  MOVE ZEROS      TO BFORM3L                          CL*29
04225              ELSE                                                    CL*29
04226                  NEXT SENTENCE                                       CL*29
04227      ELSE                                                            CL*29
04228          IF BFORM2L = ZEROS                                          CL*40
04229              IF BFORM3L NOT = ZEROS                                  CL*40
04230                  MOVE BFORM3L    TO BFORM2L                          CL*29
04231                  MOVE BFORM3I    TO BFORM2I                          CL*29
04232                  MOVE BFORM3A    TO BFORM2A                          CL*29
04233                  MOVE LOW-VALUES TO BFORM3I                          CL*29
04234                                     BFORM3A                          CL*29
04235                  MOVE ZEROS      TO BFORM3L.                         CL*29
04236                                                                      CL*29
04237  7600-EXIT.                                                          CL*29
04238       EXIT.                                                          CL*33
04239  EJECT                                                               CL*29
04240  7620-CHECK-ON-LETTER.                                               CL*29
04241                                                                      CL*29
04242      MOVE SPACES                 TO W-LETTER-IND.                    CL*29
04243                                                                      CL*29
04244      MOVE PI-COMPANY-CD          TO ELLETR-COMPANY-CD.               CL*29
04245      MOVE +1                     TO ELLETR-LINE-SEQ.                 CL*29
04246                                                                      CL*29
04247      EXEC CICS HANDLE CONDITION                                      CL*29
04248          NOTFND   (7620-LETR-NOT-FOUND)                              CL*29
04249          ENDFILE  (7620-LETR-NOT-FOUND)                              CL*29
04250          NOTOPEN  (7620-LETR-NOT-OPEN)                               CL*29
04251      END-EXEC.                                                       CL*29
04252                                                                      CL*29
04253      EXEC CICS READ                                                  CL*29
04254          SET     (ADDRESS OF TEXT-FILES)                             CL*33
04255          DATASET (ELLETR-FILE-ID)                                    CL*29
04256          RIDFLD  (ELLETR-KEY)                                        CL*29
04257      END-EXEC.                                                       CL*29
04258                                                                      CL*29
04259      GO TO 7620-EXIT.                                                CL*29
04260                                                                      CL*29
04261  7620-LETR-NOT-FOUND.                                                CL*29
04262                                                                      CL*29
04263      MOVE 'Y'                    TO W-LETTER-IND.                    CL*29
04264      GO TO 7620-EXIT.                                                CL*29
04265                                                                      CL*29
04266  7620-LETR-NOT-OPEN.                                                 CL*29
04267                                                                      CL*29
04268      MOVE ER-0013                TO EMI-ERROR.                       CL*29
04269      MOVE -1                     TO BFORM1L.                         CL*29
04270      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*29
04271      GO TO 8200-SEND-DATAONLY.                                       CL*29
04272                                                                      CL*29
04273  7620-EXIT.                                                          CL*29
04274       EXIT.                                                          CL*29
04275      EJECT                                                        EL640
04276  8100-SEND-INITIAL-MAP.                                           EL640
04277                                                                      CL*30
04278      MOVE SPACES TO PI-TRANSFER-SW.                                  CL*30
04279                                                                      CL*30
04280      IF PI-MAP-NAME = EL640A                                      EL640
04281          NEXT SENTENCE                                            EL640
04282      ELSE                                                         EL640
04283          GO TO 8110-SEND-INITIAL-CHECK-MAP.                       EL640
04284                                                                      CL*37
04285      IF PI-COMPANY-ID = 'DMD'                                        CL*40
04286         MOVE AL-SADOF            TO APF5HDGA.                        CL*37
04287                                                                   EL640
04288      IF NOT PI-GA-BILLING                                         EL640
04289         MOVE AL-SADOF            TO AGAHDGA.                      EL640
04290                                                                   EL640
04291      MOVE WS-CURRENT-DATE-EDIT   TO ADATEO.                       EL640
04292      MOVE EIBTIME                TO TIME-IN.                      EL640
04293      MOVE TIME-OUT               TO ATIMEO.                       EL640
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO USERIDO.
04294                                                                      CL*30
04295      MOVE -1                     TO AACCTL.                       EL640
04296      MOVE EMI-MESSAGE-AREA (1)   TO AERMSG1O.                     EL640
04297      MOVE EMI-MESSAGE-AREA (2)   TO AERMSG2O.                     EL640
04298                                                                      CL*11
04299      IF CARR-GROUP-ST-ACCNT-CNTL                                  EL640
04300          NEXT SENTENCE                                            EL640
04301      ELSE                                                         EL640
04302          IF ST-ACCNT-CNTL                                         EL640
04303              MOVE AL-SADOF       TO ACARHDGA                      EL640
04304                                     AGRPHDGA                      EL640
04305              MOVE AL-SANOF       TO ACARIERA                      EL640
04306                                     AGROUPA                       EL640
04307          ELSE                                                     EL640
04308              IF CARR-ST-ACCNT-CNTL                                EL640
04309                  MOVE AL-SADOF   TO AGRPHDGA                      EL640
04310                  MOVE AL-SANOF   TO AGROUPA                       EL640
04311              ELSE                                                 EL640
04312                  IF ACCNT-CNTL                                    EL640
04313                      MOVE AL-SADOF TO ACARHDGA                    EL640
04314                                       AGRPHDGA                    EL640
04315                                       ASTHDGA                     EL640
04316                      MOVE AL-SANOF TO ACARIERA                    EL640
04317                                       AGROUPA                     EL640
04318                                       ASTATEA                     EL640
04319                  ELSE                                             EL640
04320                      IF CARR-ACCNT-CNTL                           EL640
04321                          MOVE AL-SADOF TO AGRPHDGA                EL640
04322                                           ASTHDGA                 EL640
04323                          MOVE AL-SANOF TO AGROUPA                 EL640
04324                                           ASTATEA.                EL640
04325                                                                      CL*30
04326      IF (EIBTRNID = EL633-TRANS-ID  OR  EL635-TRANS-ID  OR           CL*30
04327                     EL633DMD-TRANS-ID  OR                            CL*35
04328                     EL650-TRANS-ID  OR  EL652-TRANS-ID  OR           CL*30
04329                     EL658-TRANS-ID)  AND                             CL*31
04330           (WT-CR-ACCOUNT NOT = SPACES AND LOW-VALUES)                CL*40
04331         MOVE WT-CR-CARRIER       TO PI-SCR-CARRIER                   CL*30
04332         MOVE WT-CR-GROUPING      TO PI-SCR-GROUPING                  CL*30
04333         MOVE WT-CR-STATE         TO PI-SCR-STATE                     CL*30
04334         MOVE WT-CR-FIN-RESP      TO PI-SCR-FIN-RESP                  CL*30
04335         MOVE WT-CR-TYPE          TO PI-SCR-TYPE                      CL*30
04336         IF CARR-GROUP-ST-ACCNT-CNTL                                  CL*30
04337             MOVE AL-UANON        TO ACARIERA                         CL*30
04338                                     AGROUPA                          CL*30
04339                                     ASTATEA                          CL*30
04340                                     AACCTA                           CL*30
04341             MOVE WT-CR-CARRIER   TO ACARIERI                         CL*30
04342                                     PI-SCR-CARRIER                   CL*30
04343             MOVE WT-CR-GROUPING  TO AGROUPI                          CL*30
04344                                     PI-SCR-GROUPING                  CL*30
04345             MOVE WT-CR-ACCOUNT   TO AACCTI                           CL*30
04346                                     PI-SCR-ACCOUNT                   CL*30
04347             MOVE 1               TO ACARIERL                         CL*30
04348             MOVE 6               TO AGROUPL                          CL*30
04349             MOVE 10              TO AACCTL                           CL*30
04350             MOVE -1              TO ASTATEL                          CL*30
04351         ELSE                                                         CL*30
04352          IF ST-ACCNT-CNTL                                            CL*30
04353             MOVE AL-UANON        TO ASTATEA                          CL*30
04354                                     AACCTA                           CL*30
04355             MOVE WT-CR-ACCOUNT   TO AACCTI                           CL*30
04356                                     PI-SCR-ACCOUNT                   CL*30
04357             MOVE 10              TO AACCTL                           CL*30
04358             MOVE -1              TO ASTATEL                          CL*30
04359          ELSE                                                        CL*30
04360             IF CARR-ST-ACCNT-CNTL                                    CL*30
04361                 MOVE AL-UANON       TO ACARIERA                      CL*30
04362                                        ASTATEA                       CL*30
04363                                        AACCTA                        CL*30
04364                 MOVE WT-CR-CARRIER TO ACARIERI                       CL*30
04365                                       PI-SCR-CARRIER                 CL*30
04366                 MOVE WT-CR-ACCOUNT TO AACCTI                         CL*30
04367                                       PI-SCR-ACCOUNT                 CL*30
04368                 MOVE 1              TO ACARIERL                      CL*30
04369                 MOVE 10             TO AACCTL                        CL*30
04370                 MOVE -1             TO ASTATEL                       CL*30
04371             ELSE                                                     CL*30
04372                 IF ACCNT-CNTL                                        CL*30
04373                     MOVE AL-UANON       TO AACCTA                    CL*30
04374                     MOVE WT-CR-ACCOUNT  TO AACCTI                    CL*30
04375                                            PI-SCR-ACCOUNT            CL*30
04376                     MOVE 10             TO AACCTL                    CL*30
04377                     MOVE -1             TO ABILTYPL                  CL*30
04378                 ELSE                                                 CL*30
04379                     IF CARR-ACCNT-CNTL                               CL*30
04380                         MOVE AL-UANON        TO ACARIERA             CL*30
04381                                                 AACCTA               CL*30
04382                         MOVE WT-CR-CARRIER   TO ACARIERI             CL*30
04383                                                 PI-SCR-CARRIER       CL*30
04384                         MOVE WT-CR-ACCOUNT   TO AACCTI               CL*30
04385                                                 PI-SCR-ACCOUNT       CL*30
04386                         MOVE 1               TO ACARIERL             CL*31
04387                         MOVE 10              TO AACCTL               CL*31
04388                         MOVE -1              TO ABILTYPL.            CL*31
04389                                                                      CL*31
04390      IF (EIBTRNID = EL6311-TRANS-ID) AND                             CL*31
04391           (WT-CR-ACCOUNT NOT = SPACES AND LOW-VALUES)                CL*40
04392         MOVE WT-CR-CARRIER       TO PI-SCR-CARRIER                   CL*31
04393         MOVE WT-CR-GROUPING      TO PI-SCR-GROUPING                  CL*31
04394         MOVE WT-CR-STATE         TO PI-SCR-STATE                     CL*31
04395         MOVE WT-CR-FIN-RESP      TO PI-SCR-FIN-RESP                  CL*31
04396         MOVE WT-CR-TYPE          TO PI-SCR-TYPE                      CL*31
04397         MOVE SPACES              TO PI-SAV-CARR                      CL*31
04398                                     PI-SAV-GROUP                     CL*31
04399                                     PI-SAV-STATE                     CL*31
04400                                     PI-SAV-ACCT                      CL*31
04401         MOVE LOW-VALUES          TO PI-SAV-EXP-DT                    CL*32
04402         IF CARR-GROUP-ST-ACCNT-CNTL                                  CL*31
04403             MOVE AL-UANON        TO ACARIERA                         CL*31
04404                                     AGROUPA                          CL*31
04405                                     ASTATEA                          CL*31
04406                                     AACCTA                           CL*31
04407             MOVE WT-CR-CARRIER   TO ACARIERI                         CL*31
04408                                     PI-SCR-CARRIER                   CL*31
04409                                     PI-SAV-CARR                      CL*31
04410             MOVE WT-CR-GROUPING  TO AGROUPI                          CL*31
04411                                     PI-SCR-GROUPING                  CL*31
04412                                     PI-SAV-GROUP                     CL*31
04413             MOVE WT-CR-STATE     TO ASTATEI                          CL*31
04414                                     PI-SCR-STATE                     CL*31
04415             MOVE WT-CR-ACCOUNT   TO AACCTI                           CL*31
04416                                     PI-SCR-ACCOUNT                   CL*31
04417             MOVE 1               TO ACARIERL                         CL*31
04418             MOVE 6               TO AGROUPL                          CL*31
04419             MOVE 10              TO AACCTL                           CL*31
04420             MOVE 2               TO ASTATEL                          CL*31
04421             MOVE -1              TO ABILTYPL                         CL*31
04422         ELSE                                                         CL*31
04423          IF ST-ACCNT-CNTL                                            CL*31
04424             MOVE AL-UANON        TO ASTATEA                          CL*31
04425                                     AACCTA                           CL*31
04426             MOVE WT-CR-STATE     TO ASTATEI                          CL*31
04427                                     PI-SCR-STATE                     CL*31
04428                                     PI-SAV-STATE                     CL*31
04429             MOVE WT-CR-ACCOUNT   TO AACCTI                           CL*31
04430                                     PI-SCR-ACCOUNT                   CL*31
04431                                     PI-SAV-ACCT                      CL*31
04432             MOVE 10              TO AACCTL                           CL*31
04433             MOVE -1              TO ASTATEL                          CL*31
04434          ELSE                                                        CL*31
04435             IF CARR-ST-ACCNT-CNTL                                    CL*31
04436                 MOVE AL-UANON      TO ACARIERA                       CL*31
04437                                       ASTATEA                        CL*31
04438                                       AACCTA                         CL*31
04439                 MOVE WT-CR-CARRIER TO ACARIERI                       CL*31
04440                                       PI-SCR-CARRIER                 CL*31
04441                                       PI-SAV-CARR                    CL*31
04442                 MOVE WT-CR-STATE   TO ASTATEI                        CL*31
04443                                       PI-SCR-STATE                   CL*31
04444                                       PI-SAV-STATE                   CL*31
04445                 MOVE WT-CR-ACCOUNT TO AACCTI                         CL*31
04446                                       PI-SCR-ACCOUNT                 CL*31
04447                                       PI-SAV-ACCT                    CL*31
04448                 MOVE 1             TO ACARIERL                       CL*31
04449                 MOVE 10            TO AACCTL                         CL*31
04450                 MOVE 2             TO ASTATEL                        CL*31
04451                 MOVE -1            TO ABILTYPL                       CL*31
04452             ELSE                                                     CL*31
04453                 IF ACCNT-CNTL                                        CL*31
04454                     MOVE AL-UANON       TO AACCTA                    CL*31
04455                     MOVE WT-CR-ACCOUNT  TO AACCTI                    CL*31
04456                                            PI-SCR-ACCOUNT            CL*31
04457                                            PI-SAV-ACCT               CL*31
04458                     MOVE 10             TO AACCTL                    CL*31
04459                     MOVE -1             TO ABILTYPL                  CL*31
04460                 ELSE                                                 CL*31
04461                     IF CARR-ACCNT-CNTL                               CL*31
04462                         MOVE AL-UANON        TO ACARIERA             CL*31
04463                                                 AACCTA               CL*31
04464                         MOVE WT-CR-CARRIER   TO ACARIERI             CL*31
04465                                                 PI-SCR-CARRIER       CL*31
04466                                                 PI-SAV-CARR          CL*31
04467                         MOVE WT-CR-ACCOUNT   TO AACCTI               CL*31
04468                                                 PI-SCR-ACCOUNT       CL*31
04469                                                 PI-SAV-ACCT          CL*31
04470                         MOVE 1               TO ACARIERL             CL*30
04471                         MOVE 10              TO AACCTL               CL*30
04472                         MOVE -1              TO ABILTYPL.            CL*30
04473                                                                      CL*30
04474      EXEC CICS SEND                                               EL640
04475          MAP      (PI-MAP-NAME)                                   EL640
04476          MAPSET   (MAPSET-NAME)                                   EL640
04477          FROM     (EL640AO)                                       EL640
04478          ERASE                                                    EL640
04479          CURSOR                                                   EL640
04480      END-EXEC.                                                       CL*11
04481                                                                   EL640
04482      GO TO 9100-RETURN-TRAN.                                      EL640
04483      EJECT                                                        EL640
04484  8110-SEND-INITIAL-CHECK-MAP.                                     EL640
04485      MOVE WS-CURRENT-DATE-EDIT   TO BDATEO.                       EL640
04486      MOVE EIBTIME                TO TIME-IN.                      EL640
04487      MOVE TIME-OUT               TO BTIMEO.                       EL640
101101     MOVE PI-COMPANY-ID          TO BCMPNYO.
101101     MOVE PI-PROCESSOR-ID        TO BUSERIDO.
04488      MOVE -1                     TO BPFNTERL.                     EL640
04489      MOVE EMI-MESSAGE-AREA (1)   TO BERMSGO.                      EL640
04490                                                                   EL640
04491      EXEC CICS SEND                                               EL640
04492          MAP      (PI-MAP-NAME)                                   EL640
04493          MAPSET   (MAPSET-NAME)                                   EL640
04494          FROM     (EL640BO)                                       EL640
04495          ERASE                                                    EL640
04496          CURSOR                                                   EL640
04497      END-EXEC.                                                       CL*11
04498                                                                   EL640
04499      GO TO 9100-RETURN-TRAN.                                      EL640
04500      EJECT                                                        EL640
04501  8200-SEND-DATAONLY.                                              EL640
04502      IF PI-MAP-NAME = EL640A                                      EL640
04503          MOVE WS-CURRENT-DATE-EDIT   TO ADATEO                    EL640
04504          MOVE EIBTIME                TO TIME-IN                   EL640
04505          MOVE TIME-OUT               TO ATIMEO                    EL640
101101         MOVE PI-COMPANY-ID          TO CMPNYIDO
101101         MOVE PI-PROCESSOR-ID        TO USERIDO
04506          MOVE EMI-MESSAGE-AREA (1)   TO AERMSG1O                  EL640
04507          MOVE EMI-MESSAGE-AREA (2)   TO AERMSG2O                  EL640
04508          EXEC CICS SEND                                           EL640
04509              MAP      (PI-MAP-NAME)                               EL640
04510              MAPSET   (MAPSET-NAME)                               EL640
04511              FROM     (EL640AO)                                   EL640
04512              DATAONLY                                             EL640
04513              ERASEAUP                                             EL640
04514              CURSOR                                               EL640
04515          END-EXEC                                                    CL*11
04516      ELSE                                                         EL640
04517          MOVE WS-CURRENT-DATE-EDIT   TO BDATEO                    EL640
04518          MOVE EIBTIME                TO TIME-IN                   EL640
04519          MOVE TIME-OUT               TO BTIMEO                    EL640
101101         MOVE PI-COMPANY-ID          TO BCMPNYO
101101         MOVE PI-PROCESSOR-ID        TO BUSERIDO
04520          MOVE EMI-MESSAGE-AREA (1)   TO BERMSGO                   EL640
04521          EXEC CICS SEND                                           EL640
04522              MAP      (PI-MAP-NAME)                               EL640
04523              MAPSET   (MAPSET-NAME)                               EL640
04524              FROM     (EL640BO)                                   EL640
04525              DATAONLY                                             EL640
04526              ERASEAUP                                             EL640
04527              CURSOR                                               EL640
04528          END-EXEC.                                                   CL*11
04529                                                                      CL*11
04530      GO TO 9100-RETURN-TRAN.                                      EL640
04531      EJECT                                                        EL640
04532  8300-SEND-TEXT.                                                  EL640
04533      EXEC CICS SEND TEXT                                          EL640
04534          FROM     (LOGOFF-TEXT)                                   EL640
04535          LENGTH   (LOGOFF-LENGTH)                                 EL640
04536          ERASE                                                    EL640
04537          FREEKB                                                   EL640
04538      END-EXEC.                                                       CL*11
04539                                                                      CL*11
04540      EXEC CICS RETURN                                             EL640
04541      END-EXEC.                                                       CL*11
04542                                                                   EL640
04543  8500-DATE-CONVERT.                                               EL640
04544      EXEC CICS LINK                                               EL640
04545          PROGRAM  (LINK-ELDATCV)                                  EL640
04546          COMMAREA (DATE-CONVERSION-DATA)                          EL640
04547          LENGTH   (DC-COMM-LENGTH)                                   CL*11
04548      END-EXEC.                                                       CL*11
04549                                                                      CL*11
04550  8500-EXIT.                                                       EL640
04551      EXIT.                                                        EL640
04552      EJECT                                                        EL640
04553  8800-UNAUTHORIZED-ACCESS.                                        EL640
04554      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL640
04555      GO TO 8300-SEND-TEXT.                                        EL640
04556                                                                   EL640
04557  8810-PF23.                                                       EL640
04558      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL640
04559      MOVE XCTL-005               TO PGM-NAME.                     EL640
04560      GO TO 9300-XCTL.                                             EL640
04561                                                                   EL640
04562  8900-SYNCPOINT-ROLLBACK.                                         EL640
04563      EXEC CICS SYNCPOINT                                          EL640
04564           ROLLBACK                                                   CL*11
04565      END-EXEC.                                                       CL*11
04566                                                                      CL*11
04567  8900-EXIT.                                                       EL640
04568      EXIT.                                                        EL640
04569                                                                   EL640
04570  9100-RETURN-TRAN.                                                EL640
04571      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL640
04572      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.         EL640
04573                                                                      CL*11
04574      EXEC CICS RETURN                                             EL640
04575          TRANSID    (TRANS-ID)                                    EL640
04576          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL640
04577          LENGTH     (PI-COMM-LENGTH)                              EL640
04578      END-EXEC.                                                       CL*11
04579                                                                   EL640
04580  9200-RETURN-MAIN-MENU.                                           EL640
04581      MOVE XCTL-626               TO PGM-NAME.                     EL640
04582      GO TO 9300-XCTL.                                             EL640
04583                                                                   EL640
04584  9300-XCTL.                                                       EL640
04585      EXEC CICS XCTL                                               EL640
04586          PROGRAM    (PGM-NAME)                                    EL640
04587          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL640
04588          LENGTH     (PI-COMM-LENGTH)                              EL640
04589      END-EXEC.                                                       CL*11
04590                                                                   EL640
04591  9400-CLEAR.                                                      EL640
04592      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      EL640
04593      GO TO 9300-XCTL.                                             EL640
04594                                                                   EL640
04595  9500-PF12.                                                       EL640
04596      MOVE XCTL-010               TO PGM-NAME.                     EL640
04597      GO TO 9300-XCTL.                                             EL640
04598      EJECT                                                        EL640
04599  9600-PGMID-ERROR.                                                EL640
04600      EXEC CICS HANDLE CONDITION                                   EL640
04601          PGMIDERR    (8300-SEND-TEXT)                             EL640
04602      END-EXEC.                                                       CL*11
04603                                                                      CL*11
04604      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL640
04605      MOVE ' '                    TO PI-ENTRY-CD-1.                EL640
04606      MOVE XCTL-005               TO PGM-NAME.                     EL640
04607      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL640
04608      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL640
04609      GO TO 9300-XCTL.                                             EL640
04610                                                                   EL640
04611  9900-ERROR-FORMAT.                                               EL640
04612      IF NOT EMI-ERRORS-COMPLETE                                   EL640
04613          MOVE LINK-001           TO PGM-NAME                      EL640
04614          EXEC CICS LINK                                           EL640
04615              PROGRAM    (PGM-NAME)                                EL640
04616              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL640
04617              LENGTH     (EMI-COMM-LENGTH)                         EL640
04618          END-EXEC.                                                   CL*11
04619                                                                      CL*11
04620  9900-EXIT.                                                       EL640
04621      EXIT.                                                        EL640
04622                                                                   EL640
04623  9990-ABEND.                                                      EL640
04624      MOVE LINK-004               TO PGM-NAME.                     EL640
04625      MOVE DFHEIBLK               TO EMI-LINE1.                       CL*11
04626                                                                      CL*11
04627      EXEC CICS LINK                                               EL640
04628          PROGRAM   (PGM-NAME)                                     EL640
04629          COMMAREA  (EMI-LINE1)                                    EL640
04630          LENGTH    (72)                                           EL640
04631      END-EXEC.                                                       CL*11
04632                                                                   EL640
04633      IF PI-MAP-NAME = EL640A                                      EL640
04634          MOVE -1 TO APFNTERL                                      EL640
04635      ELSE                                                         EL640
04636          MOVE -1 TO BPFNTERL.                                     EL640
04637                                                                   EL640
04638      GO TO 8200-SEND-DATAONLY.                                    EL640
04639                                                                   EL640
04640  9995-SECURITY-VIOLATION.                                         EL640
04641                              COPY ELCSCTP.                        EL640
04642                                                                   EL640
