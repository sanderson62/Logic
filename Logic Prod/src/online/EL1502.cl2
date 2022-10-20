00001  IDENTIFICATION DIVISION.                                         04/29/97
00002                                                                   EL1502
00003  PROGRAM-ID.                 EL1502.                                 LV024
00004 *              PROGRAM CONVERTED BY                                  CL*14
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*14
00006 *              CONVERSION DATE 05/04/95 09:35:38.                    CL*14
00007 *                            VMOD=2.024.                             CL*24
00008 *                                                                 EL1502
00008 *                                                                 EL1502
00009 *AUTHOR.     LOGIC,INC.                                              CL*14
00010 *            DALLAS, TEXAS.                                          CL*14
00011                                                                   EL1502
00023                                                                   EL1502
00024 *REMARKS.    TRANSACTION - E022 - CHRONOLOGICAL CLAIM HISTORY.    EL1502
062602******************************************************************
062602*                   C H A N G E   L O G
062602*
062602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
062602*-----------------------------------------------------------------
062602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
062602* EFFECTIVE    NUMBER
062602*-----------------------------------------------------------------
062602* 062602    2002030700006  PEMA  Add note type of 'S'
062602*                                  (special review)
121802* 121802    2001061800003  SMVA  REMOVE OBSOLETE CODE              
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST CALC
071210* 071210    2009122800001  AJRA  ADD 'LETTER TO BENE' ON LETTER
071510* 071510    2010070600001  PEMA  INT PMT VOID SHOULD NOT OPEN CLM
113010* 113010    2009122800001  AJRA  DISPLAY STOP DATE
041612* 041612  IR2012041300001  AJRA  REMOVE VOID AND STOP PAY FROM SCRN
041613* 041613  CR2013031200002  AJRA  ADD MAIL RECEIVED ACTION TYPE
052113* 052113    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
062217* 062217  CR2017050300002  TANA  ADD AUTH RCVD INDICATOR
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
121802******************************************************************
00025                                                                   EL1502
00026  ENVIRONMENT DIVISION.                                            EL1502
00027                                                                   EL1502
00028      EJECT                                                        EL1502
00029  DATA DIVISION.                                                   EL1502
00030  WORKING-STORAGE SECTION.                                         EL1502
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL1502
00032  77  FILLER  PIC X(32)  VALUE '*   EL1502 WORKING STORAGE     *'. EL1502
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.024 *********'.    CL*24
00034                                                                   EL1502
00035                                      COPY ELCSCTM.                   CL**7
00036                                                                   EL1502
00037                                      COPY ELCSCRTY.                  CL**7
00038                                                                   EL1502
00039  EJECT                                                            EL1502
00040  01  WS-DATE-AREA.                                                EL1502
00041      12  SAVE-DATE                   PIC X(8)    VALUE SPACES.       CL*21
00042      12  SAVE-DATE-CCYYMMDD.                                         CL*14
00043          16  SAVE-DATE-CC            PIC XX      VALUE SPACES.       CL*21
00044          16  SAVE-DATE-YMD.                                          CL*14
00045              20  SAVE-DATE-YY        PIC XX      VALUE SPACES.       CL*21
00046              20  FILLER              PIC X(4)    VALUE SPACES.       CL*21
00047      12  SAVE-BIN-DATE               PIC XX      VALUE SPACES.       CL*21
00048                                                                   EL1502
00049  01  STANDARD-AREAS.                                              EL1502
00050      12  GETMAIN-SPACE               PIC X       VALUE SPACE.        CL*21
00051      12  MAP-NAME                    PIC X(8)    VALUE 'EL150C'.     CL*21
00052      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL1502S'.    CL*21
00053      12  TRANS-ID                    PIC X(4)    VALUE 'E022'.       CL*21
00054      12  THIS-PGM                    PIC X(8)    VALUE 'EL1502'.     CL*21
00055      12  PGM-NAME                    PIC X(8).                       CL*21
00056      12  TIME-IN                     PIC S9(7).                      CL*21
00057      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL1502
00058          16  FILLER                  PIC X.                          CL*21
00059          16  TIME-OUT                PIC 99V99.                   EL1502
00060          16  FILLER                  PIC XX.                         CL*21
00061      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.      CL*21
00062      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.      CL*21
00063      12  XCTL-126                    PIC X(8)    VALUE 'EL126'.      CL*21
00064      12  XCTL-142                    PIC X(8)    VALUE 'EL142'.      CL*21
00065      12  LINK-001                    PIC X(8)    VALUE 'EL001'.      CL*21
00066      12  LINK-004                    PIC X(8)    VALUE 'EL004'.      CL*21
00067      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.    CL*21
00068      12  FILE-ID                     PIC X(8).                       CL*21
00069      12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.   CL*21
00070      12  WS-ITEM-COUNT               PIC S9(4)   VALUE +1    COMP.   CL*21
00071                                                                   EL1502
00072  01  MISC-WORK-AREAS.                                             EL1502
00073      12  WS-CK-Q-CONTROL             PIC S9(8) COMP.                 CL*24
00074      12  W-CALLING-PGM               PIC X(8).                       CL*21
00075      12  QID.                                                     EL1502
00076          16  QID-TERM                PIC X(4).                       CL*21
00077          16  FILLER                  PIC X(4)    VALUE '150B'.       CL*21
00078      12  WS-TABLE-QID.                                            EL1502
00079          16  WS-QID-TERM             PIC X(4).                       CL*21
00080          16  FILLER                  PIC X(4)    VALUE 'TBLE'.       CL*21
00081                                                                   EL1502
00082      12  MAP-LENGTH                  PIC S9(4)   VALUE +1920 COMP.   CL*21
00083      12  WS-TABLE-LENGTH             PIC S9(4)   VALUE +3200 COMP.   CL*22
00084      12  PASS-SWITCH                 PIC X       VALUE 'A'.          CL*21
00085      12  DISPLAY-CNT                 PIC S9(4)   VALUE +1    COMP.   CL*21
00086      12  FILE-SWITCH                 PIC X(4)    VALUE SPACES.       CL*21
00087      12  WS-SUB                      PIC 9       VALUE 0.            CL*21
00088      12  SUB                         PIC 9       VALUE 1.            CL*21
00089      12  SUB-1                       PIC 9       VALUE 1.            CL*21
00090      12  RETURNED-FROM               PIC X(8)    VALUE SPACES.       CL*21
00091      12  DIRECTION-SWITCH            PIC X       VALUE 'N'.          CL*21
00092      12  WS-RECORDS-READ-SW          PIC X       VALUE 'N'.          CL*21
00093          88  RECORDS-READ                        VALUE 'Y'.       EL1502
00094      12  WS-ENDBR-SW                 PIC X       VALUE 'N'.          CL*21
00095      12  SAVE-CONTROL                PIC X(39).                   EL1502
00096      12  WS-RECEIVED-DATE            PIC XX      VALUE LOW-VALUES.   CL*21
00097      12  WS-CHECK-WRITTEN-DT         PIC XX      VALUE LOW-VALUES.   CL*21
00098      12  WS-PMT-APPROVAL-SW          PIC X       VALUE SPACE.        CL*21
00099      12  WS-CF-PAYMENT-APPROVAL-SW   PIC X       VALUE ' '.          CL*21
00100          88  WS-CF-PMT-APPROVAL-USED             VALUE 'Y' 'G'.      CL*11
00101          88  WS-CF-NO-APPROVAL                   VALUE ' ' 'N'.      CL*11
00102          88  WS-CF-ALL-APPROVED                  VALUE 'Y'.          CL*11
00103          88  WS-CF-GRADUATED-APPROVAL            VALUE 'G'.          CL*11
00104                                                                      CL*11
00105      12  WS-CV-PMT-CODE              PIC X       VALUE SPACE.        CL*21
00106      12  WS-PAY-TYPE                 PIC X       VALUE SPACE.        CL*21
00107      12  WS-AMOUNT-PAID              PIC S9(7)V99 VALUE ZEROS.    EL1502
00108      12  WS-PAYMENT-ORIGIN           PIC X       VALUE SPACE.        CL*21
00109      12  WS-RECON-SW                 PIC X       VALUE ' '.          CL*21
00110                                                                   EL1502
00111      12  WS-DEEDIT-LENGTH            PIC S9(4)   VALUE +16   COMP.   CL*21
00112      12  WS-DEEDIT-FIELD             PIC X(16)   VALUE ZEROS.        CL*13
00113      12  WS-DEEDIT-FIELD-V0 REDEFINES WS-DEEDIT-FIELD             EL1502
00114                                      PIC S9(16).                     CL*13
00115                                                                      CL*13
00116      12  WS-LF-COVERAGE-TYPE         PIC X       VALUE SPACE.        CL*21
00117      12  WS-BEN-SEARCH-SW            PIC X       VALUE 'N'.          CL*21
00118          88  BENEFIT-FOUND                       VALUE 'Y'.       EL1502
00119          88  NO-BENEFIT-FOUND                    VALUE 'N'.       EL1502
00120      12  WS-ACCESS.                                               EL1502
00121          16  FILLER                  PIC XX      VALUE SPACES.       CL*21
00122          16  WS-BEN-CD               PIC XX      VALUE SPACES.       CL*21
00123      12  WS-FORMAT-CERT-NO.                                       EL1502
00124          16  WS-FORMAT-CERT-PRIME    PIC X(10)   VALUE SPACES.    EL1502
00125          16  FILLER                  PIC X       VALUE SPACE.        CL*21
00126          16  WS-FORMAT-CERT-SFX      PIC X       VALUE SPACE.        CL*21
00127      12  WS-PRINTED-SW               PIC X       VALUE 'N'.          CL*21
00128          88  PAYMENT-HAS-BEEN-PRINTED            VALUE 'Y'.       EL1502
00129          88  PAYMENT-NOT-PRINTED                 VALUE 'N'.       EL1502
00130      12  WS-RELEASED-SW              PIC X       VALUE 'N'.          CL*21
00131          88  PAYMENT-NOT-RELEASED                VALUE 'N'.       EL1502
00132      12  WS-UPDATE-SW                PIC X       VALUE 'N'.          CL*21
00133      12  WS-VOID-CODE                PIC X       VALUE ' '.          CL*21
00134                                                                      CL*12
00135      12  WS-WORK-DATE.                                               CL*12
00136          16  WS-WORK-MM              PIC 99      VALUE ZEROS.        CL*21
00137          16  WS-WORK-DD              PIC 99      VALUE ZEROS.        CL*21
00138          16  WS-WORK-YY              PIC 99      VALUE ZEROS.        CL*21
00139                                                                      CL*12
00140      12  WS-RCON-DATE.                                               CL*12
00141          16  WS-RCON-YEAR.                                           CL*12
00142              20  WS-RCON-YY-1        PIC 99.                         CL*21
00143              20  WS-RCON-YY-2        PIC 99.                         CL*21
00144              20  WS-RCON-MM          PIC 99.                         CL*21
00145              20  WS-RCON-DD          PIC 99.                         CL*21
00146      12  W-NAME-LAST                 PIC  X(15).                     CL*21
00147      12  W-NAME-FIRST                PIC  X(15).                     CL*21
00148      12  W-NAME-MIDDLE.                                              CL*14
00149          16  FILLER                  PIC  X.                         CL*21
00150          16  W-NAME-MIDDLE-2         PIC  X.                         CL*21
00151          16  FILLER                  PIC  X(13).                     CL*21
00152                                                                   EL1502
00153      12  HOLD-RECORDED-DT            PIC XX      VALUE LOW-VALUES.   CL*21
00154      12  HOLD-SUB                    PIC 9(4)    VALUE ZEROS.        CL*21
00155      12  SUB-2                       PIC 9(4)    VALUE ZEROS.        CL*21
00156      12  SUB-3                       PIC 9(4)    VALUE ZEROS.        CL*21
00157      12  WS-MAX-SUB                  PIC 9(4)    VALUE ZEROS.        CL*21
00158                                                                   EL1502
00159  01  WS-UNSORTED-TABLE.                                           EL1502
00160      12  WS-UNSRTD-TABLE   OCCURS 200 TIMES.                      EL1502
00161          16  WS-RECORDED-DT          PIC XX.                         CL*21
00162          16  WS-CERT-NO              PIC X(11).                   EL1502
00163          16  WS-TRLR-SEQ             PIC S9(4)   COMP.               CL*21
00164          16  WS-SORTED-SW            PIC X.                          CL*21
00165                                                                   EL1502
00166  01  WS-SORTED-TABLE.                                             EL1502
00167      12  WS-SRTD-TABLE OCCURS 200 TIMES.                          EL1502
00168          16  WS-SRTD-RECORDED-DT     PIC XX.                         CL*21
00169          16  WS-SRTD-CERT            PIC X(11).                   EL1502
00170          16  WS-SRTD-TRLR-SEQ        PIC S9(4)   COMP.               CL*21
00171          16  WS-SRTD-SW              PIC X.                          CL*22
00172                                                                   EL1502
00173      12  WS-DMO-LENGTH               PIC S9(4)   VALUE +108 COMP.    CL*15
00174      12  WS-DCT-LENGTH               PIC S9(4)   VALUE +53  COMP.    CL*15
00175  EJECT                                                            EL1502
00176  01  ACCESS-KEYS.                                                 EL1502
00177      12  ELMSTR-KEY.                                              EL1502
00178          16  MSTR-COMP-CD            PIC X.                          CL*21
00179          16  MSTR-CARRIER            PIC X.                          CL*21
00180          16  MSTR-CLAIM-NO           PIC X(7).                       CL*21
00181          16  MSTR-CERT-NO.                                        EL1502
00182              20  MSTR-CERT-NO-PRIME  PIC X(10).                   EL1502
00183              20  MSTR-CERT-NO-SUFX   PIC X.                          CL*21
00184      12  ELCNTL-KEY.                                              EL1502
00185          16  CNTL-COMP-ID            PIC X(3).                       CL*21
00186          16  CNTL-REC-TYPE           PIC X.                          CL*21
00187          16  CNTL-ACCESS             PIC X(4).                       CL*21
00188          16  CNTL-SEQ-NO             PIC S9(4)     COMP.             CL*21
00189      12  ELCERT-KEY.                                              EL1502
00190          16  CERT-COMP-CD            PIC X.                          CL*21
00191          16  CERT-CARRIER            PIC X.                          CL*21
00192          16  CERT-GROUPING           PIC X(6).                       CL*21
00193          16  CERT-STATE              PIC XX.                         CL*21
00194          16  CERT-ACCOUNT            PIC X(10).                   EL1502
00195          16  CERT-EFF-DT             PIC XX.                         CL*21
00196          16  CERT-CERT-NO.                                        EL1502
00197              20  CERT-CERT-NO-PRIME  PIC X(10).                   EL1502
00198              20  CERT-CERT-NO-SUFX   PIC X.                          CL*21
00199      12  ELTRLR-KEY.                                              EL1502
00200          16  TRLR-COMP-CD            PIC X.                          CL*21
00201          16  TRLR-CARRIER            PIC X.                          CL*21
00202          16  TRLR-CLAIM-NO           PIC X(7).                       CL*21
00203          16  TRLR-CERT-NO.                                        EL1502
00204              20  TRLR-CERT-NO-PRIME  PIC X(10).                   EL1502
00205              20  TRLR-CERT-NO-SUFX   PIC X.                          CL*21
00206          16  TRLR-SEQ-NO             PIC S9(4)   COMP.               CL*21
00207      12  ELACTQ-KEY.                                              EL1502
00208          16  ACTQ-COMP-CD            PIC X.                          CL*21
00209          16  ACTQ-CARRIER            PIC X.                          CL*21
00210          16  ACTQ-CLAIM-NO           PIC X(7).                       CL*21
00211          16  ACTQ-CERT-NO.                                        EL1502
00212              20  ACTQ-CERT-NO-PRIME  PIC X(10).                   EL1502
00213              20  ACTQ-CERT-NO-SUFX   PIC X.                          CL*21
00214      12  ELCHKQ-KEY.                                              EL1502
00215          16  CHKQ-COMP-CD            PIC X.                          CL*21
00216          16  CHKQ-CONTROL            PIC S9(8)   COMP.               CL*21
00217          16  CHKQ-SEQ-NO             PIC S9(4)   COMP.               CL*21
00218      12  ELRCON-KEY.                                                 CL*12
00219          16  RCON-COMPANY-CD         PIC X.                          CL*21
00220          16  RCON-CHECK-NO           PIC X(7).                       CL*21
00221          16  RCON-CHECK-ORIGIN       PIC X.                          CL*21
00222          16  RCON-GL-ACCOUNT-NO      PIC X(10).                      CL*12
00223      12  EMPLCY-KEY.                                                 CL*13
00224          16  PLCY-COMPANY-CD         PIC X.                          CL*21
00225          16  PLCY-CARRIER            PIC X.                          CL*21
00226          16  PLCY-GROUPING           PIC X(6).                       CL*21
00227          16  PLCY-STATE              PIC XX.                         CL*21
00228          16  PLCY-PRODUCER           PIC X(10).                      CL*13
00229          16  PLCY-EFF-DT             PIC XX.                         CL*21
00230          16  PLCY-REFERENCE-NO       PIC X(20).                      CL*13
00231      12  W-NOTE-KEY.                                                 CL*14
00232          16  W-NOTE-COMP-CD          PIC X.                          CL*21
00233          16  W-NOTE-CERT-KEY.                                        CL*14
00234              20  W-NOTE-CARRIER      PIC X.                          CL*21
00235              20  W-NOTE-GROUPING     PIC X(6).                       CL*21
00236              20  W-NOTE-STATE        PIC XX.                         CL*21
00237              20  W-NOTE-ACCOUNT      PIC X(10).                      CL*21
00238              20  W-NOTE-EFF-DT       PIC XX.                         CL*21
00239              20  W-NOTE-CERT-NO      PIC X(11).                      CL*21
00240                                                                   EL1502
00241      EJECT                                                        EL1502
00242  01  ERROR-MESSAGES.                                              EL1502
00243      12  ER-0000                     PIC X(4)    VALUE '0000'.       CL*21
00244      12  ER-0004                     PIC X(4)    VALUE '0004'.       CL*21
00245      12  ER-0008                     PIC X(4)    VALUE '0008'.       CL*21
00246      12  ER-0029                     PIC X(4)    VALUE '0029'.       CL*21
00247      12  ER-0033                     PIC X(4)    VALUE '0033'.       CL*21
00248      12  ER-0042                     PIC X(4)    VALUE '0042'.       CL*21
00249      12  ER-0068                     PIC X(4)    VALUE '0068'.       CL*21
00250      12  ER-0070                     PIC X(4)    VALUE '0070'.       CL*21
00251      12  ER-0130                     PIC X(4)    VALUE '0130'.       CL*21
00252      12  ER-0154                     PIC X(4)    VALUE '0154'.       CL*21
00253      12  ER-0169                     PIC X(4)    VALUE '0169'.       CL*21
00254      12  ER-0172                     PIC X(4)    VALUE '0172'.       CL*21
00255      12  ER-0190                     PIC X(4)    VALUE '0190'.       CL*21
00256      12  ER-0204                     PIC X(4)    VALUE '0204'.       CL*21
00257      12  ER-0206                     PIC X(4)    VALUE '0206'.       CL*21
00258      12  ER-0282                     PIC X(4)    VALUE '0282'.       CL*21
00259      12  ER-0303                     PIC X(4)    VALUE '0303'.       CL*21
00260      12  ER-0334                     PIC X(4)    VALUE '0334'.       CL*21
00261      12  ER-0335                     PIC X(4)    VALUE '0335'.       CL*21
00262      12  ER-0336                     PIC X(4)    VALUE '0336'.       CL*21
00263      12  ER-0337                     PIC X(4)    VALUE '0337'.       CL*21
00264      12  ER-0338                     PIC X(4)    VALUE '0338'.       CL*21
00265      12  ER-0376                     PIC X(4)    VALUE '0376'.       CL*21
00266      12  ER-0412                     PIC X(4)    VALUE '0412'.       CL*21
00267      12  ER-0413                     PIC X(4)    VALUE '0413'.       CL*21
00268      12  ER-0660                     PIC X(4)    VALUE '0660'.       CL*21
00269      12  ER-0661                     PIC X(4)    VALUE '0661'.       CL*21
00270      12  ER-0662                     PIC X(4)    VALUE '0662'.       CL*21
00271      12  ER-0663                     PIC X(4)    VALUE '0663'.       CL*21
00272      12  ER-0664                     PIC X(4)    VALUE '0664'.       CL*21
00273      12  ER-0665                     PIC X(4)    VALUE '0665'.       CL*21
00274      12  ER-0666                     PIC X(4)    VALUE '0666'.       CL*21
00275      12  ER-0667                     PIC X(4)    VALUE '0667'.       CL*21
00276      12  ER-0672                     PIC X(4)    VALUE '0672'.       CL*21
00277      12  ER-0776                     PIC X(4)    VALUE '0776'.       CL*21
00278      12  ER-0800                     PIC X(4)    VALUE '0800'.       CL*21
00279      12  ER-0801                     PIC X(4)    VALUE '0801'.       CL*21
00280      12  ER-0816                     PIC X(4)    VALUE '0816'.       CL*21
00281      12  ER-0823                     PIC X(4)    VALUE '0823'.       CL*21
00282      12  ER-0833                     PIC X(4)    VALUE '0833'.       CL*21
00283      12  ER-0835                     PIC X(4)    VALUE '0835'.       CL*21
00284      12  ER-0849                     PIC X(4)    VALUE '0849'.       CL*21
00285      12  ER-0919                     PIC X(4)    VALUE '0919'.       CL*21
00286      12  ER-0920                     PIC X(4)    VALUE '0920'.       CL*21
00287      12  ER-0921                     PIC X(4)    VALUE '0921'.       CL*21
00288      12  ER-0922                     PIC X(4)    VALUE '0922'.       CL*21
00289      12  ER-0923                     PIC X(4)    VALUE '0923'.       CL*21
00290      12  ER-0925                     PIC X(4)    VALUE '0925'.       CL*21
00291      12  ER-0939                     PIC X(4)    VALUE '0939'.       CL*21
00292      12  ER-0940                     PIC X(4)    VALUE '0940'.       CL*21
00293      12  ER-0941                     PIC X(4)    VALUE '0941'.       CL*21
00294      12  ER-0942                     PIC X(4)    VALUE '0942'.       CL*21
00295      12  ER-0946                     PIC X(4)    VALUE '0946'.       CL*21
00296      12  ER-0947                     PIC X(4)    VALUE '0947'.       CL*21
00297      12  ER-0948                     PIC X(4)    VALUE '0948'.       CL*21
00298      12  ER-0949                     PIC X(4)    VALUE '0949'.       CL*21
00299      12  ER-0950                     PIC X(4)    VALUE '0950'.       CL*21
00300      12  ER-0951                     PIC X(4)    VALUE '0951'.       CL*21
00301      12  ER-0952                     PIC X(4)    VALUE '0952'.       CL*21
00302      12  ER-0954                     PIC X(4)    VALUE '0954'.       CL*21
00303      12  ER-0974                     PIC X(4)    VALUE '0974'.       CL*21
00304      12  ER-0975                     PIC X(4)    VALUE '0975'.       CL*21
00305      12  ER-2378                     PIC X(4)    VALUE '2378'.       CL*21
00306      12  ER-2379                     PIC X(4)    VALUE '2379'.       CL*21
00307      12  ER-7999                     PIC X(4)    VALUE '7999'.       CL*21
062602     12  ER-8003                     PIC X(4)    VALUE '8003'.       CL*22
00308      12  ER-8051                     PIC X(4)    VALUE '8051'.       CL*22
00309      12  ER-8052                     PIC X(4)    VALUE '8052'.       CL*22
00310      12  ER-8053                     PIC X(4)    VALUE '8053'.       CL*22
00311      12  ER-8054                     PIC X(4)    VALUE '8054'.       CL*22
00312      12  ER-8055                     PIC X(4)    VALUE '8055'.       CL*22
00313      12  ER-8056                     PIC X(4)    VALUE '8056'.       CL*22
00314      12  ER-8057                     PIC X(4)    VALUE '8057'.       CL*22
00315      12  ER-8058                     PIC X(4)    VALUE '8058'.       CL*22
00316      12  ER-8059                     PIC X(4)    VALUE '8059'.       CL*22
00317      12  ER-8060                     PIC X(4)    VALUE '8060'.       CL*22
00318      12  ER-8061                     PIC X(4)    VALUE '8061'.       CL*22
00319      12  ER-8062                     PIC X(4)    VALUE '8062'.       CL*22
00320      12  ER-8063                     PIC X(4)    VALUE '8063'.       CL*22
00321      12  ER-8064                     PIC X(4)    VALUE '8064'.       CL*22
00322      12  ER-8065                     PIC X(4)    VALUE '8065'.       CL*22
00323      12  ER-8066                     PIC X(4)    VALUE '8066'.       CL*22
00324      12  ER-8152                     PIC X(4)    VALUE '8152'.       CL*22
00325      12  ER-8153                     PIC X(4)    VALUE '8153'.       CL*22
00326      12  ER-8154                     PIC X(4)    VALUE '8154'.       CL*22
00327      12  ER-8155                     PIC X(4)    VALUE '8155'.       CL*22
00328      12  ER-9211                     PIC X(4)    VALUE '9211'.       CL*22
00329      12  ER-9883                     PIC X(4)    VALUE '9883'.       CL*22
00330                                                                   EL1502
00331  EJECT                                                            EL1502
00332  01  TEXT-WORK-AREAS.                                             EL1502
00333      12  PAYMENT-LINE-1.                                          EL1502
00334          16  PMT-HDG1.                                            EL1502
00335              20  PMT-LINE-NO         PIC X.                          CL*21
00336              20  FILLER              PIC X.                          CL*21
00337              20  PMT-HDG1-LIT        PIC X(8).                       CL*21
00338          16  PMT-TEXT-1.                                          EL1502
00339              20  PMT-CERT-NO         PIC X(12).                   EL1502
00340              20  PMT-TYPE-LIT        PIC X(7).                       CL*21
00341              20  PMT-TYPE            PIC X(7).                       CL*21
00342              20  PMT-CHECK-NO-LIT    PIC X(11).                   EL1502
00343              20  PMT-CHECK-NO        PIC X(7).                       CL*21
00344              20  PMT-AMT-PD-LIT      PIC X(5).                       CL*21
00345              20  PMT-AMT-PAID        PIC Z(6).99-.                   CL*13
00346              20  PMT-REC-BY-LIT      PIC X(5).                       CL*21
00347              20  PMT-REC-BY          PIC X(4).                       CL*21
00348      12  PAYMENT-LINE-2.                                          EL1502
00349          16  PMT-HDG2.                                            EL1502
00350              20  FILLER              PIC XX.                         CL*21
00351              20  PMT-HDG2-LIT        PIC X(8).                       CL*21
00352          16  PMT-TEXT-2.                                          EL1502
00353              20  PMT-REC-DT          PIC X(8).                       CL*21
00354              20  FILLER              PIC X(5).                       CL*21
00355              20  PMT-FROM-LIT        PIC X(6).                       CL*21
00356              20  PMT-PAID-FROM       PIC X(8).                       CL*21
00357              20  PMT-THRU-LIT        PIC X(10).                   EL1502
00358              20  PMT-PAID-THRU       PIC X(8).                       CL*21
00359              20  PMT-VOID-LIT        PIC X(9).                       CL*21
00360              20  PMT-VOID-DT         PIC X(8).                       CL*21
00361              20  FILLER              PIC XX.                         CL*21
00362              20  PMT-PAYEE           PIC X(4).                       CL*21
00363      12  AUTO-PMT-LINE-1.                                         EL1502
00364          16  AUTO-PMT-HDG1.                                       EL1502
00365              20  AUTO-LINE-NO        PIC X.                          CL*21
00366              20  FILLER              PIC X.                          CL*21
00367              20  AUTO-HDG1-LIT       PIC X(8).                       CL*21
00368          16  AUTO-PMT-TEXT1.                                      EL1502
00369              20  AUTO-CERT-NO        PIC X(12).                   EL1502
00370              20  AUTO-EFF-DT-LIT     PIC X(7).                       CL*21
00371              20  AUTO-EFF-DT         PIC X(8).                       CL*21
00372              20  AUTO-1ST-PMT-LIT    PIC X(10).                   EL1502
00373              20  AUTO-1ST-PMT-DT     PIC X(8).                       CL*21
00374              20  AUTO-1ST-AMT-LIT    PIC X(6).                       CL*21
00375              20  AUTO-1ST-AMT        PIC Z(5).99.                    CL*21
00376              20  AUTO-REC-BY-LIT     PIC X(5).                       CL*21
00377              20  AUTO-REC-BY         PIC X(4).                       CL*21
00378      12  AUTO-PMT-LINE-2.                                         EL1502
00379          16  AUTO-PMT-HDG2.                                       EL1502
00380              20  FILLER              PIC XX.                         CL*21
00381              20  AUTO-HDG2-LIT       PIC X(8).                       CL*21
00382          16  AUTO-PMT-TEXT2.                                      EL1502
00383              20  AUTO-REC-DT         PIC X(8).                       CL*21
00384              20  FILLER              PIC X(5).                       CL*21
00385              20  AUTO-LST-PMT-LIT    PIC X(6).                       CL*21
00386              20  AUTO-LST-PMT-DT     PIC X(8).                       CL*21
00387              20  AUTO-STATUS-LIT     PIC X(10).                   EL1502
00388              20  AUTO-STATUS         PIC X(6).                       CL*21
00389              20  AUTO-REG-AMT-LIT    PIC X(8).                       CL*21
00390              20  AUTO-REG-AMT        PIC Z(5).99.                    CL*21
00391              20  AUTO-PAYEE-LIT      PIC X(4).                       CL*21
00392              20  AUTO-PAYEE          PIC X(5).                       CL*21
00393      12  CORRESPONDENCE-LINE-1.                                   EL1502
00394          16  CORR-HDG1.                                           EL1502
00395              20  CORR-LINE-NO        PIC X.                          CL*21
00396              20  FILLER              PIC X.                          CL*21
00397              20  CORR-HDG1-LIT       PIC X(8).                       CL*21
00398          16  CORR-TEXT-1.                                         EL1502
00399              20  CORR-CERT-NO        PIC X(12).                   EL1502
00400              20  CORR-FORM-LIT       PIC X(7).                       CL*21
071210             20  CORR-FORM-TYPE      PIC X(6).                       CL*21
00402              20  CORR-DT-SENT-LIT    PIC X(11).                   EL1502
00403              20  CORR-DT-SENT        PIC X(8).                       CL*21
071210             20  CORR-LET-TO-BEN     PIC X(15).
071210             20  FILLER REDEFINES CORR-LET-TO-BEN.
071210                 25  CORR-PURGE-LIT      PIC X(6).
071210                 25  CORR-PURGE-DT       PIC X(9).
00406              20  CORR-REC-BY-LIT     PIC X(5).                       CL*21
00407              20  CORR-REC-BY         PIC X(4).                       CL*21
00408      12  CORRESPONDENCE-LINE-2.                                   EL1502
00409          16  CORR-HDG2.                                           EL1502
00410              20  FILLER              PIC XX.                         CL*21
00411              20  CORR-HDG2-LIT       PIC X(8).                       CL*21
00412          16  CORR-TEXT-2.                                         EL1502
00413              20  CORR-REC-DT         PIC X(8).                       CL*21
00414              20  FILLER              PIC X(5).                       CL*21
00415              20  CORR-RESEND-LIT     PIC X(6).                       CL*21
00416              20  CORR-RESEND-DT      PIC X(8).                       CL*21
00417              20  CORR-RECVD-LIT      PIC X(10).                   EL1502
00418              20  CORR-RECVD-DT       PIC X(8).                       CL*21
00419              20  CORR-FOLLOW-UP-LIT  PIC X(6).                       CL*21
00420              20  CORR-FOLLOW-UP-DT   PIC X(8).                       CL*21
00421              20  CORR-ADDRESEE-LIT   PIC X(5).                       CL*21
00422              20  CORR-ADDRESEE       PIC X(4).                       CL*21
00423      12  FORM-LINE-1.                                             EL1502
00424          16  FORM-HDG1.                                           EL1502
00425              20  FORM-LINE-NO        PIC X.                          CL*21
00426              20  FILLER              PIC X.                          CL*21
00427              20  FORM-HDG1-LIT       PIC X(8).                       CL*21
00428          16  FORM-TEXT-1.                                         EL1502
00429              20  FORM-CERT-NO        PIC X(12).                   EL1502
00430              20  FORM-TYPE-LIT       PIC X(7).                       CL*21
00431              20  FORM-TYPE           PIC X(8).                       CL*21
00432              20  FORM-SEND-ON-LIT    PIC X(10).                   EL1502
00433              20  FORM-SEND-ON-DT     PIC X(8).                       CL*21
00434              20  FORM-RESEND-LIT     PIC X(6).                       CL*21
00435              20  FORM-RESEND-DT      PIC X(8).                       CL*21
00436              20  FORM-REC-BY-LIT     PIC X(5).                       CL*21
00437              20  FORM-REC-BY         PIC X(4).                       CL*21
00438      12  FORM-LINE-2.                                             EL1502
00439          16  FORM-HDG2.                                           EL1502
00440              20  FILLER              PIC XX.                         CL*21
00441              20  FORM-HDG2-LIT       PIC X(8).                       CL*21
00442          16  FORM-TEXT-2.                                         EL1502
00443              20  FORM-REC-DT         PIC X(8).                       CL*21
00444              20  FILLER              PIC X(5).                       CL*21
00445              20  FORM-REC-INS-LIT    PIC X(6).                       CL*21
00446              20  FORM-REC-INS-DT     PIC X(8).                       CL*21
00447              20  FORM-REC-PHY-LIT    PIC X(10).                   EL1502
00448              20  FORM-REC-PHY-DT     PIC X(8).                       CL*21
00449              20  FORM-REC-EMP-LIT    PIC X(6).                       CL*21
00450              20  FORM-REC-EMP-DT     PIC X(8).                       CL*21
00451              20  FILLER              PIC X(9).                       CL*21
00452      12  INCUR-CHG-LINE-1.                                        EL1502
00453          16  INCUR-CHG-HDG1.                                      EL1502
00454              20  INCUR-LINE-NO       PIC X.                          CL*21
00455              20  FILLER              PIC X.                          CL*21
00456              20  INCUR-HDG1-LIT      PIC X(8).                       CL*21
00457          16  INCUR-TEXT-1.                                        EL1502
00458              20  INCUR-CERT-NO       PIC X(12).                   EL1502
00459              20  INCUR-INCUR-LIT     PIC X(7).                       CL*21
00460              20  INCUR-INCUR-DT      PIC X(8).                       CL*21
00461              20  INCUR-REPORT-LIT    PIC X(10).                   EL1502
00462              20  INCUR-REPORT-DT     PIC X(8).                       CL*21
00463              20  INCUR-ESTAB-LIT     PIC X(6).                       CL*21
00464              20  INCUR-ESTAB-DT      PIC X(8).                       CL*21
00465              20  INCUR-REC-BY-LIT    PIC X(5).                       CL*21
00466              20  INCUR-REC-BY        PIC X(4).                       CL*21
00467      12  INCUR-CHG-LINE-2.                                        EL1502
00468          16  INCUR-CHG-HDG2.                                      EL1502
00469              20  FILLER              PIC XX.                         CL*21
00470              20  INCUR-HDG2-LIT      PIC X(8).                       CL*21
00471          16  INCUR-TEXT-2.                                        EL1502
00472              20  INCUR-REC-DT        PIC X(8).                       CL*21
00473              20  FILLER              PIC X(5).                       CL*21
00474              20  INCUR-PD-THRU-LIT   PIC X(6).                       CL*21
00475              20  INCUR-PD-THRU-DT    PIC X(8).                       CL*21
00476              20  INCUR-TOT-PD-LIT    PIC X(10).                   EL1502
00477              20  INCUR-TOT-PD        PIC Z(5).ZZ.                    CL*21
00478              20  INCUR-TOT-DAYS-LIT  PIC X(6).                       CL*21
00479              20  INCUR-TOT-DAYS-PD   PIC ZZZ.                     EL1502
00480              20  INCUR-NO-PMTS-LIT   PIC X(10).                   EL1502
00481              20  INCUR-NO-PMTS       PIC ZZZ.                     EL1502
00482              20  FILLER              PIC X.                          CL*21
00483      12  DENIAL-LINE-1.                                           EL1502
00484          16  DENIAL-HDG1.                                         EL1502
00485              20  DENIAL-LINE-NO      PIC X.                          CL*21
00486              20  FILLER              PIC X.                          CL*21
00487              20  DENIAL-HDG1-LIT     PIC X(8).                       CL*21
00488          16  DENIAL-TEXT-1.                                       EL1502
00489              20  DENIAL-CERT-NO      PIC X(12).                   EL1502
00490              20  DENIAL-LN1-LIT      PIC X(7).                       CL*21
00491              20  DENIAL-LN1          PIC X(40).                   EL1502
00492              20  DENIAL-REC-BY-LIT   PIC X(5).                       CL*21
00493              20  DENIAL-REC-BY       PIC X(4).                       CL*21
00494      12  DENIAL-LINE-2.                                           EL1502
00495          16  DENIAL-HDG2.                                         EL1502
00496              20  FILLER              PIC XX.                         CL*21
00497              20  DENIAL-HDG2-LIT     PIC X(8).                       CL*21
00498          16  DENIAL-TEXT-2.                                       EL1502
00499              20  DENIAL-REC-DT       PIC X(8).                       CL*21
00500              20  FILLER              PIC X(5).                       CL*21
00501              20  DENIAL-LN2-LIT      PIC X(6).                       CL*21
00502              20  DENIAL-LN2          PIC X(40).                   EL1502
00503              20  FILLER              PIC X(9).                       CL*21
00504      12  GEN-INFO-LINE-1.                                         EL1502
00505          16  GEN-INFO-HDG1.                                       EL1502
00506              20  GI-LINE-NO          PIC X.                          CL*21
00507              20  FILLER              PIC X.                          CL*21
00508              20  GI-HDG1-LIT         PIC X(8).                       CL*21
00509          16  GEN-INFO-TEXT-1.                                     EL1502
00510              20  GEN-INFO-CERT-NO    PIC X(12).                   EL1502
00511              20  GEN-INFO-MSG-1-LIT  PIC X(7).                       CL*21
00512              20  GEN-INFO-MSG-1      PIC X(40).                   EL1502
00513              20  GEN-INFO-REC-BY-LIT PIC X(5).                       CL*21
00514              20  GEN-INFO-REC-BY     PIC X(4).                       CL*21
00515      12  GEN-INFO-LINE-2.                                         EL1502
00516          16  GEN-INFO-HDG2.                                       EL1502
00517              20  FILLER              PIC XX.                         CL*21
00518              20  GI-HDG2-LIT         PIC X(8).                       CL*21
00519          16  GEN-INFO-TEXT-2.                                     EL1502
00520              20  GEN-INFO-REC-DT     PIC X(8).                       CL*21
00521              20  FILLER              PIC X(5).                       CL*21
00522              20  GEN-INFO-MSG-2-LIT  PIC X(6).                       CL*21
00523              20  GEN-INFO-MSG-2      PIC X(40).                   EL1502
00524              20  FILLER              PIC X(9).                       CL*21
00525      12  REMINDER-LINE-1.                                         EL1502
00526          16  REMINDER-HDG1.                                       EL1502
00527              20  REM-LINE-NO         PIC X.                          CL*21
00528              20  FILLER              PIC X.                          CL*21
00529              20  REM-HDG1-LIT        PIC X(8).                       CL*21
00530          16  REMINDER-TEXT-1.                                     EL1502
00531              20  REM-CERT-NO         PIC X(12).                   EL1502
00532              20  REM-LINE-1-LIT      PIC X(7).                       CL*21
00533              20  REM-LINE-1          PIC X(40).                   EL1502
00534              20  REM-REC-BY-LIT      PIC X(5).                       CL*21
00535              20  REM-REC-BY          PIC X(4).                       CL*21
00536      12  REMINDER-LINE-2.                                         EL1502
00537          16  REMINDER-HDG2.                                       EL1502
00538              20  FILLER              PIC XX.                         CL*21
00539              20  REM-HDG2-LIT        PIC X(8).                       CL*21
00540          16  REMINDER-TEXT-2.                                     EL1502
00541              20  REM-REC-DT          PIC X(8).                       CL*21
00542              20  FILLER              PIC X(5).                       CL*21
00543              20  REM-LINE-2-LIT      PIC X(6).                       CL*21
00544              20  REM-LINE-2          PIC X(40).                   EL1502
00545              20  FILLER              PIC X(9).                       CL*21
00546                                                                   EL1502
00547  01  PAYMENT-DESCRIPTION-TABLE.                                   EL1502
00548      12  FILLER                      PIC X(7)    VALUE 'PARTIAL'.    CL*21
00549      12  FILLER                      PIC X(7)    VALUE 'FINAL  '.    CL*21
00550      12  FILLER                      PIC X(7)    VALUE 'LMP SUM'.    CL*21
00551      12  FILLER                      PIC X(7)    VALUE 'ADDL   '.    CL*21
00552      12  FILLER                      PIC X(7)    VALUE 'CHG EXP'.    CL*21
00553      12  FILLER                      PIC X(7)    VALUE 'NON-CHG'.    CL*21
00554      12  FILLER                      PIC X(7)    VALUE 'LF PRM '.    CL*21
00555      12  FILLER                      PIC X(7)    VALUE 'A/H PRM'.    CL*21
00556      12  FILLER                      PIC X(7)    VALUE 'ENT COR'.    CL*21
00557  01  PAYMENT-DESC-R   REDEFINES PAYMENT-DESCRIPTION-TABLE.        EL1502
00558      12  PAY-DESC                    PIC X(7)    OCCURS 2.           CL*21
00559                                                                   EL1502
00560  01  CV-PAYMENT-DESCRIPTION-TABLE.                                   CL*13
00561      12  FILLER                      PIC X(7)    VALUE 'FUL DTH'.    CL*21
00562      12  FILLER                      PIC X(7)    VALUE 'HLF DTH'.    CL*21
00563      12  FILLER                      PIC X(7)    VALUE 'FUL ADD'.    CL*21
00564      12  FILLER                      PIC X(7)    VALUE 'HLF ADD'.    CL*21
00565      12  FILLER                      PIC X(7)    VALUE 'FUL RID'.    CL*21
00566      12  FILLER                      PIC X(7)    VALUE 'HLF RID'.    CL*21
00567      12  FILLER                      PIC X(7)    VALUE 'NON-CHG'.    CL*21
00568      12  FILLER                      PIC X(7)    VALUE 'ADDL   '.    CL*21
00569                                                                      CL*21
00570  01  CV-PAYMENT-DESC-R REDEFINES CV-PAYMENT-DESCRIPTION-TABLE.       CL*13
00571      12  CV-PAY-DESC                 PIC X(7)    OCCURS 2.           CL*21
00572                                                                      CL*14
00573      EJECT                                                           CL*14
00574                                      COPY ELCDATE.                   CL**7
00575      EJECT                                                        EL1502
00576                                      COPY ELCLOGOF.                  CL**7
00577      EJECT                                                        EL1502
00578                                      COPY ELCATTR.                   CL**7
00579      EJECT                                                        EL1502
00580                                      COPY ELCEMIB.                   CL**7
00581      EJECT                                                           CL*14
121802*                                    COPY ELCDCTB.                   CL*14
121802*    EJECT                                                           CL*14
121802*                                    COPY ELCDMO.                    CL*14
121802*    EJECT                                                           CL*14
121802*                                    COPY ELCNWA.                    CL*14
121802*    EJECT                                                           CL*13
121802*                                    COPY MPCPOLUP.                  CL*13
121802*    EJECT                                                        EL1502
00590                                      COPY ELCINTF.                   CL**7
00591                                                                      CL*21
00592      12  PI-REDEF    REDEFINES PI-PROGRAM-WORK-AREA.              EL1502
062602*        16  FILLER                  PIC 9(4).                       CL*21
062602         16  FILLER                  PIC x(2).                       CL*28
062602         16  pi-el142-priority       pic x.
062602         16  filler                  pic x.
00594          16  PI-MAP-NAME             PIC X(8).                       CL*21
00595          16  PI-QUALIFICATION-SWITCHES  COMP-3.                   EL1502
00596              20  PI-REMINDERS-SW     PIC S9.                         CL*21
00597              20  PI-LETTERS-SW       PIC S9.                         CL*21
00598              20  PI-PAYMENTS-SW      PIC S9.                         CL*21
00599              20  PI-AUTO-PAY-SW      PIC S9.                         CL*21
00600              20  PI-NOTES-SW         PIC S9.                         CL*21
00601              20  PI-RES-EXP-SW       PIC S9.                         CL*21
00602              20  PI-DENIALS-SW       PIC S9.                         CL*21
00603              20  PI-INCURRED-DATE-SW PIC S9.                         CL*21
00604              20  PI-FORMS-SW         PIC S9.                         CL*21
00605          16  FILLER                  PIC X(8).                       CL*21
00606          16  PI-ACTIVITY-TRAILERS-KEY.                            EL1502
00607              20  PI-ATK-COMPANY-CODE PIC X.                          CL*21
00608              20  PI-ATK-CARRIER      PIC X.                          CL*21
00609              20  PI-ATK-CLAIM-NO     PIC X(7).                       CL*21
00610              20  PI-ATK-CERT-NO      PIC X(11).                   EL1502
00611              20  PI-ATK-SEQ-NO       PIC S9(4)     COMP.             CL*21
00612          16  PI-PREV-ACTIVITY-TRAILERS-KEY.                       EL1502
00613              20  PI-PREV-ATK-COMPANY-CODE PIC X.                     CL*21
00614              20  PI-PREV-ATK-CARRIER      PIC X.                     CL*21
00615              20  PI-PREV-ATK-CLAIM-NO     PIC X(7).                  CL*21
00616              20  PI-PREV-ATK-CERT-NO      PIC X(11).              EL1502
00617              20  PI-PREV-ATK-SEQ-NO       PIC S9(4)     COMP.        CL*21
00618          16  PI-SAVE-KEY.                                         EL1502
00619              20  PI-SAVE-ATK-COMPANY-CODE PIC X.                     CL*21
00620              20  PI-SAVE-ATK-CARRIER      PIC X.                     CL*21
00621              20  PI-SAVE-ATK-CLAIM-NO     PIC X(7).                  CL*21
00622              20  PI-SAVE-ATK-CERT-NO      PIC X(11).              EL1502
00623              20  PI-SAVE-ATK-SEQ-NO       PIC S9(4)     COMP.        CL*21
00624          16  PI-PREV-AID                  PIC X.                     CL*21
00625          16  PI-RECORD-COUNT              PIC S9        COMP-3.      CL*21
00626          16  PI-END-OF-FILE               PIC S9        COMP-3.      CL*21
00627          16  PI-SAVE-CURSOR               PIC S9(4)     COMP.        CL*21
00628          16  PI-PURGED-SW                 PIC X.                  EL1502
00629              88  CLAIM-IS-PURGED                 VALUE 'Y'.       EL1502
00630          16  PI-LINE-NO                   PIC 9.                     CL*21
00631          16  PI-PREV-SEQ-NO               PIC S9(4)   COMP.          CL*21
00632          16  PI-FIRST-TIME-SW             PIC X.                     CL*21
00633              88  FIRST-TIME                      VALUE 'Y'.       EL1502
00634                                                                   EL1502
00635          16  PI-SEQ-NUMBERS.                                      EL1502
00636              20  PI-SEQ-NO-TABLE OCCURS 8 TIMES.                  EL1502
00637                  24  PI-TRLR-LN-NO        PIC 9.                     CL*21
00638                  24  PI-TRLR-SEQ-NO       PIC S9(4)   COMP.          CL*21
00639                  24  PI-TRLR-TYPE         PIC X.                     CL*21
00640                  24  PI-TRLR-CERT         PIC X(11).              EL1502
00641                  24  PI-TRLR-SUB          PIC 9(4).                  CL*21
00642                                                                   EL1502
00643          16  PI-PREV-CLAIM-NO             PIC X(7).                  CL*21
00644          16  PI-PREV-CARRIER              PIC X.                     CL*21
00645          16  PI-PRIME-CERT-NO.                                    EL1502
00646              20  PI-PRIME-CERT            PIC X(10).              EL1502
00647              20  PI-PRIME-SUFX            PIC X.                     CL*21
00648          16  PI-PRIME-HDG                 PIC X(12).              EL1502
00649          16  PI-MAX-SUB                   PIC 9(4).                  CL*21
00650          16  PI-PAY-TYPE                  PIC X.                     CL*21
00651          16  PI-FULL-SCREEN-IND           PIC X.                     CL*21
00652              88  PI-FULL-SCREEN-SHOWN         VALUE 'Y'.             CL*14
00653          16  FILLER                  PIC X(346).                     CL*14
00654                                                                   EL1502
00655      EJECT                                                        EL1502
00656                                      COPY ELCAID.                    CL**7
00657  01  FILLER    REDEFINES DFHAID.                                  EL1502
00658      12  FILLER                      PIC X(8).                    EL1502
00659      12  PF-VALUES                   PIC X       OCCURS 24 TIMES. EL1502
00660      EJECT                                                        EL1502
00661                                      COPY EL1502S.                   CL**7
00662  01  EL150CI-R REDEFINES EL150CI.                                 EL1502
00663      12  FILLER                      PIC X(77).                   EL1502
00664      12  EL150CI-OCCURS OCCURS 16.                                EL1502
00665          16  MAP-HDG-LENGTH          PIC S9(4)   COMP.               CL*21
00666          16  MAP-HDG-ATTRB           PIC X.                       EL1502
00667          16  MAP-HDG.                                             EL1502
00668              20  MAP-LINE-NO         PIC X.                          CL*21
00669              20  FILLER              PIC X.                          CL*21
00670              20  MAP-HDG-LIT         PIC X(8).                       CL*21
00671          16  MAP-TEXT-LENGTH         PIC S9(4)   COMP.               CL*21
00672          16  MAP-TEXT-ATTRB          PIC X.                          CL*21
00673          16  MAP-TEXT                PIC X(68).                   EL1502
00674      12  FILLER                      PIC X(144).                     CL*14
00675                                                                   EL1502
00676      EJECT                                                        EL1502
00677  LINKAGE SECTION.                                                 EL1502
00678  01  DFHCOMMAREA                     PIC X(1024).                 EL1502
00679                                                                   EL1502
00680                                      COPY ELCMSTR.                   CL**7
00681      EJECT                                                        EL1502
00682                                      COPY ELCCNTL.                   CL**7
00683      EJECT                                                        EL1502
00684                                      COPY ELCCERT.                   CL**7
00685      EJECT                                                        EL1502
00686                                      COPY ELCTRLR.                   CL**7
00687      EJECT                                                        EL1502
00688                                      COPY ELCACTQ.                   CL**7
00689      EJECT                                                        EL1502
00690                                      COPY ELCCHKQ.                   CL**7
00691      EJECT                                                           CL*12
00692                                      COPY ELCRCON.                   CL*12
00693      EJECT                                                           CL*13
121802*                                    COPY ERCDMDNT.                  CL*14
121802*    EJECT                                                           CL*14
121802*                                    COPY MPCPLCY.                   CL*13
121802*    EJECT                                                        EL1502
00698  PROCEDURE DIVISION.                                              EL1502
00699                                                                   EL1502
00700      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL1502
00701      MOVE '5'                    TO  DC-OPTION-CODE.              EL1502
00702      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1502
00703      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL1502
00704      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL1502
00705      MOVE DC-GREG-DATE-1-YMD     TO  SAVE-DATE-YMD.                  CL*14
00706                                                                      CL*14
00707      IF SAVE-DATE-YY > 70                                            CL*23
00708          MOVE 19                 TO  SAVE-DATE-CC                    CL*14
00709      ELSE                                                            CL*14
00710          MOVE 20                 TO  SAVE-DATE-CC.                   CL*14
00711                                                                   EL1502
00712      IF EIBCALEN = 0                                              EL1502
00713          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL1502
00714                                                                   EL1502
00715      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL1502
00716                                                                   EL1502
00717      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.         EL1502
00718      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.           EL1502
00719                                                                   EL1502
00720      MOVE EIBTRMID               TO QID-TERM                      EL1502
00721                                     WS-QID-TERM.                  EL1502
00722                                                                   EL1502
00723      EXEC CICS HANDLE CONDITION                                   EL1502
00724          QIDERR   (1000-SHOW-CLAIM-HISTORY)                       EL1502
00725          MAPFAIL  (0100-FIRST-TIME-IN)                            EL1502
00726          NOTOPEN  (8500-FILE-NOTOPEN)                             EL1502
00727          PGMIDERR (9600-PGMID-ERROR)                              EL1502
00728          ERROR    (9990-ABEND)                                    EL1502
00729      END-EXEC.                                                    EL1502
00730                                                                   EL1502
00731      IF PI-RETURN-TO-PROGRAM = THIS-PGM                              CL*17
00732          MOVE PI-CALLING-PROGRAM       TO RETURNED-FROM.          EL1502
00733                                                                   EL1502
00734      IF PI-CALLING-PROGRAM NOT = THIS-PGM                            CL*17
00735          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                      CL*17
00736              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL1502
00737              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL1502
00738              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL1502
00739              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL1502
00740              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL1502
00741              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL1502
00742              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL1502
00743              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL1502
00744          ELSE                                                     EL1502
00745              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL1502
00746              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL1502
00747              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL1502
00748              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL1502
00749              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL1502
00750              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL1502
00751              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL1502
00752              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL1502
00753                                                                   EL1502
00754      IF RETURNED-FROM NOT = SPACES                                   CL*17
00755          GO TO 0600-RECOVER-TEMP-STORAGE.                         EL1502
00756                                                                   EL1502
00757      IF EIBAID = DFHCLEAR                                            CL*17
00758          GO TO 9400-CLEAR.                                        EL1502
00759                                                                   EL1502
00760      IF PI-PROCESSOR-ID = 'LGXX'                                     CL*17
00761          NEXT SENTENCE                                            EL1502
00762      ELSE                                                         EL1502
00763          EXEC CICS READQ TS                                       EL1502
00764              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL1502
00765              INTO    (SECURITY-CONTROL)                           EL1502
00766              LENGTH  (SC-COMM-LENGTH)                             EL1502
00767              ITEM    (SC-ITEM)                                    EL1502
00768          END-EXEC                                                 EL1502
00769              MOVE SC-CLAIMS-DISPLAY (16)  TO  PI-DISPLAY-CAP      EL1502
00770              MOVE SC-CLAIMS-UPDATE  (16)  TO  PI-MODIFY-CAP       EL1502
00771              IF NOT DISPLAY-CAP                                   EL1502
00772                  MOVE 'READ'              TO  SM-READ             EL1502
00773                  PERFORM 9995-SECURITY-VIOLATION                  EL1502
00774                  MOVE ER-0070             TO  EMI-ERROR           EL1502
00775                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL1502
00776                  GO TO 8100-SEND-INITIAL-MAP.                     EL1502
00777                                                                   EL1502
00778      IF EIBTRNID = TRANS-ID                                          CL*17
00779          GO TO 0200-RECEIVE.                                      EL1502
00780                                                                   EL1502
00781  EJECT                                                            EL1502
00782  0100-FIRST-TIME-IN.                                              EL1502
00783      MOVE LOW-VALUES             TO  EL150CO                      EL1502
00784                                      PI-PROGRAM-WORK-AREA         EL1502
00785                                      HOLD-RECORDED-DT.            EL1502
00786                                                                   EL1502
00787      MOVE 'Y'                    TO  PI-FIRST-TIME-SW             EL1502
00788                                      WS-RECORDS-READ-SW.          EL1502
00789      MOVE 'F'                    TO  DIRECTION-SWITCH.            EL1502
00790      MOVE 1                      TO  PI-LINE-NO                   EL1502
00791                                      SUB-2                        EL1502
00792                                      SUB-3.                       EL1502
00793      MOVE ZERO                   TO  WS-MAX-SUB                      CL*21
00794                                      PI-REMINDERS-SW                 CL*21
00795                                      PI-LETTERS-SW                EL1502
00796                                      PI-PAYMENTS-SW               EL1502
00797                                      PI-AUTO-PAY-SW               EL1502
00798                                      PI-NOTES-SW                  EL1502
00799                                      PI-RES-EXP-SW                EL1502
00800                                      PI-DENIALS-SW                EL1502
00801                                      PI-INCURRED-DATE-SW          EL1502
00802                                      PI-FORMS-SW                  EL1502
00803                                      SUB.                         EL1502
00804                                                                   EL1502
00805      PERFORM 7400-DEL-TEMP-STOR-TABLE THRU 7400-EXIT.             EL1502
00806                                                                   EL1502
00807      EXEC CICS HANDLE CONDITION                                   EL1502
00808          QIDERR   (0900-BUILD-RELATED-TABLE)                      EL1502
00809      END-EXEC.                                                    EL1502
00810                                                                   EL1502
00811      EXEC CICS DELETEQ TS                                         EL1502
00812          QUEUE(QID)                                               EL1502
00813      END-EXEC.                                                    EL1502
00814                                                                   EL1502
00815      PERFORM 0900-BUILD-RELATED-TABLE THRU 0999-RELATED-EXIT.     EL1502
00816                                                                      CL*21
00817      GO TO 1000-SHOW-CLAIM-HISTORY.                               EL1502
00818                                                                   EL1502
00819      EJECT                                                        EL1502
00820  0200-RECEIVE.                                                    EL1502
00821      MOVE 'B'                    TO  PASS-SWITCH.                 EL1502
00822      MOVE LOW-VALUES             TO  EL150CI.                     EL1502
00823                                                                   EL1502
00824      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                          CL*17
00825          MOVE ER-0008            TO  EMI-ERROR                    EL1502
00826          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1502
00827          MOVE -1                 TO  ENTERPFL                     EL1502
00828          GO TO 8200-SEND-DATAONLY.                                EL1502
00829                                                                   EL1502
00830      EXEC CICS RECEIVE                                            EL1502
00831          MAP      (MAP-NAME)                                      EL1502
00832          MAPSET   (MAPSET-NAME)                                   EL1502
00833          INTO     (EL150CI)                                       EL1502
00834      END-EXEC.                                                    EL1502
00835                                                                   EL1502
00836      IF ENTERPFL = 0                                                 CL*17
00837          GO TO 0300-CHECK-PFKEYS.                                 EL1502
00838                                                                   EL1502
00839      IF EIBAID NOT = DFHENTER                                        CL*23
00840          MOVE ER-0004            TO  EMI-ERROR                    EL1502
00841          GO TO 0320-INPUT-ERROR.                                  EL1502
00842                                                                   EL1502
00843      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)               CL*23
00844          MOVE PF-VALUES (ENTERPFI)   TO  EIBAID                   EL1502
00845      ELSE                                                         EL1502
00846          MOVE ER-0029                TO  EMI-ERROR                EL1502
00847          GO TO 0320-INPUT-ERROR.                                  EL1502
00848                                                                   EL1502
00849  0300-CHECK-PFKEYS.                                               EL1502
00850      IF EIBAID = DFHPF23                                             CL*17
00851          GO TO 8810-PF23.                                         EL1502
00852                                                                   EL1502
00853      IF EIBAID = DFHPF24                                             CL*17
00854          GO TO 9200-RETURN-MAIN-MENU.                             EL1502
00855                                                                   EL1502
00856      IF EIBAID = DFHPF12                                             CL*17
00857          GO TO 9500-PF12.                                         EL1502
00858                                                                   EL1502
00859      IF EIBAID = DFHPF3                                              CL*17
00860          IF LINENOL > +0                                             CL*23
00861              GO TO 0500-CREATE-TEMP-STORAGE                       EL1502
00862          ELSE                                                     EL1502
00863              MOVE ER-0672        TO  EMI-ERROR                    EL1502
00864              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL1502
00865              MOVE -1             TO  LINENOL                      EL1502
00866              GO TO 8200-SEND-DATAONLY.                            EL1502
00867                                                                   EL1502
041612*    IF EIBAID = DFHPF4                                              CL*24
041612*       IF LINENOL > +0                                              CL*29
041612*          IF (PI-EL142-PRIORITY = '8')
041612*             AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
041612*                  AND 'AMWA')
041612*             MOVE ER-8003       TO EMI-ERROR                     EL1501
041612*             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               CL*18
041612*             MOVE -1            TO LINENOL                          CL*18
041612*             GO TO 8200-SEND-DATAONLY                               CL*18
041612*          ELSE
041612*             MOVE 'V'           TO WS-VOID-CODE                     CL*18
041612*             GO TO 5000-VOID-PAYMENT                             EL1501
041612*          END-IF
041612*       ELSE                                                      EL1501
041612*          MOVE ER-0663          TO EMI-ERROR                     EL1501
041612*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  CL*18
041612*          MOVE -1               TO LINENOL                          CL*18
041612*          GO TO 8200-SEND-DATAONLY                                  CL*18
041612*       END-IF
041612*    END-IF
041612*                                                                    CL*18
041612*    IF EIBAID = DFHPF5                                              CL*24
041612*       IF LINENOL > +0                                              CL*29
041612*          IF (PI-EL142-PRIORITY = '8')
041612*             AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
041612*                  AND 'AMWA')
041612*             MOVE ER-8003       TO EMI-ERROR                     EL1501
041612*             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               CL*18
041612*             MOVE -1            TO LINENOL                        CL*18
041612*             GO TO 8200-SEND-DATAONLY                               CL*18
041612*          ELSE
041612*             MOVE 'S'           TO WS-VOID-CODE                     CL*18
041612*             GO TO 5000-VOID-PAYMENT                                CL*18
041612*          END-IF
041612*       ELSE                                                         CL*18
041612*          MOVE ER-0835          TO EMI-ERROR                        CL*18
041612*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1501
041612*          MOVE -1               TO LINENOL                       EL1501
041612*          GO TO 8200-SEND-DATAONLY                               EL1501
041612*       END-IF
041612*    END-IF
00798                                                                      CL*21
062602*    IF EIBAID = DFHPF4                                              CL*17
00869 *        IF LINENOL > +0                                             CL*23
00870 *            MOVE 'V'            TO  WS-VOID-CODE                    CL*12
00871 *            GO TO 5000-VOID-PAYMENT                              EL1502
00872 *        ELSE                                                     EL1502
00873 *            MOVE ER-0663        TO  EMI-ERROR                    EL1502
00874 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*12
00875 *            MOVE -1             TO  LINENOL                         CL*12
00876 *            GO TO 8200-SEND-DATAONLY.                               CL*12
00877 *                                                                    CL*12
00878 *    IF EIBAID = DFHPF5                                              CL*17
00879 *        IF LINENOL > +0                                             CL*23
00880 *            MOVE 'S'            TO  WS-VOID-CODE                    CL*12
00881 *            GO TO 5000-VOID-PAYMENT                                 CL*12
00882 *        ELSE                                                        CL*12
00883 *            MOVE ER-0835        TO  EMI-ERROR                       CL*12
00884 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL1502
00885 *            MOVE -1             TO  LINENOL                      EL1502
00886 *            GO TO 8200-SEND-DATAONLY.                            EL1502
00887                                                                      CL*14
121802*    IF EIBAID = DFHPF6                                              CL*21
121802*        IF PI-COMPANY-ID = 'DMD'                                    CL*21
121802*            IF LINENOL NOT = ZEROS                                  CL*23
121802*                MOVE LINENOI        TO SUB-1                        CL*21
121802*                IF PI-TRLR-TYPE (SUB-1) = '2'                       CL*23
121802*                    PERFORM 0500-CREATE-TEMP-STORAGE                CL*14
121802*                    MOVE 'EL402DMD' TO PGM-NAME                     CL*21
121802*                    GO TO 9300-XCTL                                 CL*14
121802*                ELSE                                                CL*14
121802*                    MOVE ER-0940    TO EMI-ERROR                    CL*21
121802*                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        CL*14
121802*                    MOVE -1         TO LINENOL                      CL*21
121802*                    GO TO 8200-SEND-DATAONLY                        CL*14
121802*            ELSE                                                    CL*14
121802*                MOVE ER-0939    TO EMI-ERROR                        CL*14
121802*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*14
121802*                MOVE -1         TO LINENOL                          CL*14
121802*                GO TO 8200-SEND-DATAONLY.                           CL*14
00906                                                                   EL1502
00907      MOVE SPACES                 TO  ERRMSG1O.                    EL1502
00908                                                                   EL1502
00909      IF EIBAID = DFHPF1                                              CL*17
00910          MOVE 'F'                TO  DIRECTION-SWITCH             EL1502
00911          PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT         EL1502
00912          GO TO 1000-SHOW-CLAIM-HISTORY.                           EL1502
00913                                                                   EL1502
00914      IF EIBAID = DFHPF2                                              CL*17
00915          MOVE 'B'                TO  DIRECTION-SWITCH             EL1502
00916          PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT         EL1502
00917          GO TO 1000-SHOW-CLAIM-HISTORY.                           EL1502
00918                                                                   EL1502
00919      IF EIBAID = DFHENTER                                            CL*17
00920          GO TO 0330-EDIT-DATA.                                    EL1502
00921                                                                   EL1502
00922      MOVE ER-0029                TO EMI-ERROR.                    EL1502
00923                                                                   EL1502
00924  0320-INPUT-ERROR.                                                EL1502
00925      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1502
00926                                                                   EL1502
00927      IF ENTERPFL = 0                                                 CL*17
00928          MOVE -1                 TO ENTERPFL                      EL1502
00929      ELSE                                                         EL1502
00930          MOVE AL-UNBON           TO ENTERPFA                      EL1502
00931          MOVE -1                 TO ENTERPFL.                     EL1502
00932                                                                   EL1502
00933      GO TO 8200-SEND-DATAONLY.                                    EL1502
00934                                                                   EL1502
00935  0330-EDIT-DATA.                                                  EL1502
00936      IF NOT MODIFY-CAP                                            EL1502
00937          MOVE 'UPDATE'           TO  SM-READ                      EL1502
00938          PERFORM 9995-SECURITY-VIOLATION                          EL1502
00939          MOVE ER-0070            TO  EMI-ERROR                    EL1502
00940          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1502
00941          GO TO 8100-SEND-INITIAL-MAP.                             EL1502
00942                                                                   EL1502
00943      IF LINENOL  > 0 AND                                             CL*23
00944         RECVDTL  > 0 AND                                             CL*23
00945         RECVTYPL > 0                                                 CL*23
00946             GO TO 4000-RECEIVE-FORMS.                                CL*21
00947                                                                   EL1502
00948      IF LINENOL > 0 AND                                              CL*23
00949         RECVDTL > 0                                                  CL*23
00950             GO TO 3000-RECEIVE-LETTERS.                              CL*21
00951                                                                   EL1502
00952      IF LINENOL > 0                                                  CL*23
00953          MOVE LINENOI            TO  SUB-1                        EL1502
00954          IF PI-TRLR-TYPE (SUB-1) = 'A'                               CL*17
00955              MOVE ER-0665        TO  EMI-ERROR                    EL1502
00956              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL1502
00957              MOVE -1             TO  LINENOL                      EL1502
00958              GO TO 8200-SEND-DATAONLY                             EL1502
00959          ELSE                                                     EL1502
00960          IF PI-TRLR-TYPE (SUB-1) = '4'                               CL*23
00961              MOVE ER-0666        TO  EMI-ERROR                       CL*23
00962              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*23
00963              MOVE -1             TO  LINENOL                         CL*23
00964              GO TO 8200-SEND-DATAONLY                                CL*23
00965          ELSE                                                        CL*23
00966          IF PI-TRLR-TYPE (SUB-1) = '2'                               CL*23
00967              MOVE ER-0667    TO  EMI-ERROR                           CL*23
00968              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*23
00969              MOVE -1         TO  LINENOL                             CL*23
00970              GO TO 8200-SEND-DATAONLY.                               CL*23
00971                                                                   EL1502
00972      MOVE 'F'                    TO  DIRECTION-SWITCH.            EL1502
00973      PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT.            EL1502
00974      GO TO 1000-SHOW-CLAIM-HISTORY.                               EL1502
00975                                                                   EL1502
00976      EJECT                                                        EL1502
00977  0500-CREATE-TEMP-STORAGE.                                        EL1502
00978      MOVE EIBCPOSN               TO  PI-SAVE-CURSOR.              EL1502
00979      MOVE SPACES                 TO PI-FULL-SCREEN-IND.              CL*14
00980                                                                   EL1502
00981      EXEC CICS WRITEQ TS                                          EL1502
00982          QUEUE    (QID)                                           EL1502
00983          FROM     (PROGRAM-INTERFACE-BLOCK)                       EL1502
00984          LENGTH   (PI-COMM-LENGTH)                                EL1502
00985      END-EXEC.                                                    EL1502
00986                                                                   EL1502
00987      IF LINENOL > +0                                                 CL*23
00988          MOVE LINENOI            TO  SUB-1                        EL1502
00989          IF PI-TRLR-TYPE (SUB-1) = '2'                               CL*23
00990             MOVE +1                      TO  PI-PAYMENTS-SW       EL1502
00991             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO        EL1502
00992                                              PI-SAVE-ATK-SEQ-NO   EL1502
00993          ELSE                                                     EL1502
00994          IF PI-TRLR-TYPE (SUB-1) = '3'                               CL*23
00995             MOVE +1                      TO  PI-AUTO-PAY-SW          CL*21
00996             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*21
00997                                              PI-SAVE-ATK-SEQ-NO   EL1502
00998          ELSE                                                        CL*21
00999          IF PI-TRLR-TYPE (SUB-1) = '4'                               CL*23
01000             MOVE +1                      TO  PI-LETTERS-SW           CL*21
01001             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*21
01002                                              PI-SAVE-ATK-SEQ-NO   EL1502
01003          ELSE                                                        CL*21
01004          IF PI-TRLR-TYPE (SUB-1) = '6'                               CL*23
01005             MOVE +1                      TO  PI-NOTES-SW             CL*21
01006             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*21
01007                                              PI-SAVE-ATK-SEQ-NO   EL1502
01008          ELSE                                                        CL*21
01009          IF PI-TRLR-TYPE (SUB-1) = '7'                               CL*23
01010             MOVE +1                      TO  PI-REMINDERS-SW         CL*21
01011             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*21
01012                                              PI-SAVE-ATK-SEQ-NO   EL1502
01013          ELSE                                                        CL*21
01014          IF PI-TRLR-TYPE (SUB-1) = '8'                               CL*23
01015             MOVE +1                      TO  PI-DENIALS-SW           CL*21
01016             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*21
01017                                              PI-SAVE-ATK-SEQ-NO   EL1502
01018          ELSE                                                        CL*21
01019          IF PI-TRLR-TYPE (SUB-1) = '9'                               CL*23
01020             MOVE +1                      TO  PI-INCURRED-DATE-SW     CL*21
01021             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*21
01022                                              PI-SAVE-ATK-SEQ-NO   EL1502
01023          ELSE                                                        CL*21
01024             MOVE +1                      TO  PI-FORMS-SW             CL*21
01025             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*21
01026                                              PI-SAVE-ATK-SEQ-NO   EL1502
01027      ELSE                                                         EL1502
01028          MOVE +1                         TO  PI-REMINDERS-SW      EL1502
01029                                              PI-LETTERS-SW        EL1502
01030                                              PI-PAYMENTS-SW       EL1502
01031                                              PI-AUTO-PAY-SW       EL1502
01032                                              PI-NOTES-SW          EL1502
01033                                              PI-RES-EXP-SW        EL1502
01034                                              PI-DENIALS-SW        EL1502
01035                                              PI-INCURRED-DATE-SW  EL1502
01036                                              PI-FORMS-SW          EL1502
01037          MOVE +0                         TO  PI-ATK-SEQ-NO.       EL1502
01038                                                                      CL*14
01039      IF EIBAID = DFHPF6                                              CL*17
01040          MOVE PI-COMPANY-CD      TO  PI-SAVE-ATK-COMPANY-CODE        CL*14
01041                                      PI-ATK-COMPANY-CODE             CL*14
01042          MOVE PI-CARRIER         TO  PI-SAVE-ATK-CARRIER             CL*14
01043                                      PI-ATK-CARRIER                  CL*14
01044          MOVE PI-CLAIM-NO        TO  PI-SAVE-ATK-CLAIM-NO            CL*14
01045                                      PI-ATK-CLAIM-NO                 CL*14
01046          MOVE PI-TRLR-CERT (SUB-1) TO PI-SAVE-ATK-CERT-NO            CL*21
01047                                       PI-ATK-CERT-NO                 CL*21
01048                                       PI-CERT-NO                     CL*21
01049          MOVE 'Y'                TO  PI-FIRST-TIME-SW.               CL*14
01050                                                                   EL1502
01051      IF EIBAID = DFHPF3                                              CL*17
01052          MOVE XCTL-142           TO  PGM-NAME                     EL1502
01053          MOVE 'EL142A'           TO  PI-MAP-NAME                  EL1502
01054          MOVE PI-COMPANY-CD      TO  PI-SAVE-ATK-COMPANY-CODE     EL1502
01055                                      PI-ATK-COMPANY-CODE          EL1502
01056          MOVE PI-CARRIER         TO  PI-SAVE-ATK-CARRIER          EL1502
01057                                      PI-ATK-CARRIER               EL1502
01058          MOVE PI-CLAIM-NO        TO  PI-SAVE-ATK-CLAIM-NO         EL1502
01059                                      PI-ATK-CLAIM-NO              EL1502
01060          MOVE PI-TRLR-CERT (SUB-1)   TO  PI-SAVE-ATK-CERT-NO      EL1502
01061                                          PI-ATK-CERT-NO           EL1502
01062                                          PI-CERT-NO               EL1502
01063          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL1502
01064          GO TO 9300-XCTL.                                         EL1502
01065                                                                   EL1502
01066      EJECT                                                        EL1502
01067  0600-RECOVER-TEMP-STORAGE.                                       EL1502
01068      MOVE PI-CONTROL-IN-PROGRESS TO SAVE-CONTROL.                 EL1502
01069                                                                   EL1502
01070      EXEC CICS HANDLE CONDITION                                   EL1502
01071          QIDERR   (0690-QIDERR)                                   EL1502
01072      END-EXEC.                                                    EL1502
01073                                                                   EL1502
01074      EXEC CICS READQ TS                                           EL1502
01075          QUEUE    (QID)                                           EL1502
01076          INTO     (PROGRAM-INTERFACE-BLOCK)                       EL1502
01077          LENGTH   (PI-COMM-LENGTH)                                EL1502
01078      END-EXEC.                                                    EL1502
01079                                                                   EL1502
01080      EXEC CICS DELETEQ TS                                         EL1502
01081          QUEUE   (QID)                                            EL1502
01082      END-EXEC.                                                    EL1502
01083                                                                   EL1502
01084      MOVE SAVE-CONTROL           TO  PI-CONTROL-IN-PROGRESS.      EL1502
01085      MOVE 'F'                    TO  DIRECTION-SWITCH.            EL1502
01086                                                                   EL1502
01087      PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT.            EL1502
01088      GO TO 1000-SHOW-CLAIM-HISTORY.                               EL1502
01089                                                                   EL1502
01090  0690-QIDERR.                                                     EL1502
01091      MOVE ER-0033                TO  EMI-ERROR.                   EL1502
01092      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1502
01093      MOVE LOW-VALUES             TO  EL150CO.                     EL1502
01094      MOVE -1                     TO  ENTERPFL.                    EL1502
01095      GO TO 8100-SEND-INITIAL-MAP.                                 EL1502
01096                                                                   EL1502
01097      EJECT                                                        EL1502
01098  0700-CREATE-TEMP-STORAGE.                                        EL1502
01099                                                                   EL1502
01100      EXEC CICS WRITEQ TS                                          EL1502
01101          QUEUE    (WS-TABLE-QID)                                  EL1502
01102          FROM     (WS-SORTED-TABLE)                               EL1502
01103          LENGTH   (WS-TABLE-LENGTH)                               EL1502
01104          ITEM     (WS-ITEM-COUNT)                                 EL1502
01105      END-EXEC.                                                    EL1502
01106                                                                   EL1502
01107  0700-EXIT.                                                       EL1502
01108      EXIT.                                                        EL1502
01109                                                                   EL1502
01110      EJECT                                                        EL1502
01111  0800-RECOVER-TEMP-STORAGE.                                       EL1502
01112                                                                   EL1502
01113      EXEC CICS HANDLE CONDITION                                   EL1502
01114          QIDERR   (0890-QIDERR)                                   EL1502
01115      END-EXEC.                                                    EL1502
01116                                                                   EL1502
01117      EXEC CICS READQ TS                                           EL1502
01118          QUEUE    (WS-TABLE-QID)                                  EL1502
01119          INTO     (WS-SORTED-TABLE)                               EL1502
01120          LENGTH   (WS-TABLE-LENGTH)                               EL1502
01121          ITEM     (WS-ITEM-COUNT)                                 EL1502
01122      END-EXEC.                                                    EL1502
01123                                                                   EL1502
01124      GO TO 0899-EXIT.                                             EL1502
01125                                                                   EL1502
01126  0890-QIDERR.                                                     EL1502
01127      MOVE ER-0033                TO  EMI-ERROR.                   EL1502
01128      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1502
01129      MOVE LOW-VALUES             TO  EL150CO.                     EL1502
01130      MOVE -1                     TO  ENTERPFL.                    EL1502
01131      GO TO 8100-SEND-INITIAL-MAP.                                 EL1502
01132                                                                   EL1502
01133  0899-EXIT.                                                       EL1502
01134      EXIT.                                                        EL1502
01135                                                                   EL1502
01136      EJECT                                                        EL1502
01137  0900-BUILD-RELATED-TABLE.                                        EL1502
01138                                                                   EL1502
01139      EXEC CICS HANDLE CONDITION                                   EL1502
01140          ENDFILE   (0950-SORT-ROUTINE)                            EL1502
01141      END-EXEC.                                                    EL1502
01142                                                                   EL1502
01143      MOVE LOW-VALUES             TO  ELMSTR-KEY.                  EL1502
01144      MOVE PI-COMPANY-CD          TO  MSTR-COMP-CD.                EL1502
01145      MOVE PI-CARRIER             TO  MSTR-CARRIER.                EL1502
01146      MOVE PI-CLAIM-NO            TO  MSTR-CLAIM-NO.               EL1502
01147                                                                   EL1502
01148      EXEC CICS STARTBR                                            EL1502
01149          DATASET   ('ELMSTR')                                     EL1502
01150          RIDFLD    (ELMSTR-KEY)                                   EL1502
01151      END-EXEC.                                                    EL1502
01152                                                                   EL1502
01153  0910-READNEXT-CLAIM-MASTER.                                      EL1502
01154                                                                   EL1502
01155      EXEC CICS HANDLE CONDITION                                      CL*12
01156          ENDFILE   (0915-ELMSTR-ENDFILE)                             CL*12
01157      END-EXEC.                                                       CL*12
01158                                                                      CL*12
01159      EXEC CICS READNEXT                                           EL1502
01160          DATASET   ('ELMSTR')                                     EL1502
01161          RIDFLD    (ELMSTR-KEY)                                   EL1502
01162          SET       (ADDRESS OF CLAIM-MASTER)                         CL*14
01163      END-EXEC.                                                    EL1502
01164                                                                   EL1502
01165      IF PI-FIRST-TIME-SW = 'Y'                                       CL*17
01166          MOVE 'N'                        TO  PI-FIRST-TIME-SW     EL1502
01167          MOVE CL-CLAIM-NO                TO  PI-PREV-CLAIM-NO     EL1502
01168          MOVE CL-CARRIER                 TO  PI-PREV-CARRIER      EL1502
01169          IF CL-ASSOC-CERT-TOTAL > +1                                 CL*23
01170              MOVE CL-PRIME-CERT-PRIME    TO  PI-PRIME-CERT        EL1502
01171              MOVE CL-PRIME-CERT-SFX      TO  PI-PRIME-SUFX        EL1502
01172              MOVE 'PRIME CERT :'         TO  PI-PRIME-HDG         EL1502
01173              MOVE AL-SABON               TO  PRIMEHDA             EL1502
01174          ELSE                                                     EL1502
01175              MOVE PI-CERT-NO             TO  PI-PRIME-CERT        EL1502
01176              MOVE PI-CERT-SFX            TO  PI-PRIME-SUFX        EL1502
01177              MOVE 'CERT NO/SFX:'         TO  PI-PRIME-HDG         EL1502
01178              MOVE AL-SABON               TO  PRIMEHDA.            EL1502
01179                                                                   EL1502
01180      IF CL-CLAIM-NO = PI-PREV-CLAIM-NO AND                           CL*17
01181         CL-CARRIER  = PI-PREV-CARRIER                                CL*17
01182          NEXT SENTENCE                                            EL1502
01183      ELSE                                                         EL1502
01184          MOVE HIGH-VALUES        TO  WS-RECORDED-DT (SUB-2)       EL1502
01185          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL1502
01186          MOVE SUB-2              TO  WS-MAX-SUB                   EL1502
01187                                      PI-MAX-SUB                   EL1502
01188          MOVE 1                  TO  SUB-2 SUB-3                  EL1502
01189          GO TO 0950-SORT-ROUTINE.                                 EL1502
01190                                                                   EL1502
01191      IF SUB-2 > 200                                                  CL*23
01192          MOVE 200                TO  WS-MAX-SUB                   EL1502
01193                                      PI-MAX-SUB                   EL1502
01194          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL1502
01195          MOVE 1                  TO  SUB-2 SUB-3                  EL1502
01196          GO TO 0950-SORT-ROUTINE.                                 EL1502
01197                                                                   EL1502
01198      MOVE LOW-VALUES             TO  ELTRLR-KEY.                  EL1502
01199      MOVE CL-COMPANY-CD          TO  TRLR-COMP-CD.                EL1502
01200      MOVE CL-CARRIER             TO  TRLR-CARRIER.                EL1502
01201      MOVE CL-CLAIM-NO            TO  TRLR-CLAIM-NO.               EL1502
01202      MOVE CL-CERT-NO             TO  TRLR-CERT-NO.                EL1502
01203      MOVE ZEROS                  TO  TRLR-SEQ-NO.                 EL1502
01204                                                                   EL1502
01205      PERFORM 0925-BUILD-TRAILER-TABLE THRU 0949-BUILD-TABLE-EXIT. EL1502
01206      GO TO 0910-READNEXT-CLAIM-MASTER.                            EL1502
01207                                                                   EL1502
01208  0915-ELMSTR-ENDFILE.                                                CL*12
01209                                                                      CL*12
01210      EXEC CICS ENDBR                                                 CL*12
01211          DATASET   ('ELMSTR')                                        CL*12
01212      END-EXEC.                                                       CL*12
01213                                                                      CL*12
01214      MOVE HIGH-VALUES            TO  WS-RECORDED-DT (SUB-2).         CL*12
01215      MOVE 'Y'                    TO  PI-FIRST-TIME-SW.               CL*12
01216      MOVE SUB-2                  TO  WS-MAX-SUB                      CL*12
01217                                      PI-MAX-SUB.                     CL*12
01218      MOVE 1                      TO  SUB-2   SUB-3.                  CL*12
01219      GO TO 0950-SORT-ROUTINE.                                        CL*12
01220                                                                      CL*12
01221      EJECT                                                           CL*12
01222  0925-BUILD-TRAILER-TABLE.                                        EL1502
01223                                                                   EL1502
01224      EXEC CICS HANDLE CONDITION                                   EL1502
01225          ENDFILE   (0945-ENDBR-ELTRLR)                            EL1502
01226      END-EXEC.                                                    EL1502
01227                                                                   EL1502
01228      EXEC CICS STARTBR                                            EL1502
01229          DATASET   ('ELTRLR')                                     EL1502
01230          RIDFLD    (ELTRLR-KEY)                                   EL1502
01231      END-EXEC.                                                    EL1502
01232                                                                   EL1502
01233  0935-READNEXT-TRAILER.                                           EL1502
01234                                                                   EL1502
01235      EXEC CICS READNEXT                                           EL1502
01236          DATASET   ('ELTRLR')                                     EL1502
01237          RIDFLD    (ELTRLR-KEY)                                   EL1502
01238          SET       (ADDRESS OF ACTIVITY-TRAILERS)                    CL*14
01239      END-EXEC.                                                    EL1502
01240                                                                   EL1502
01241      IF AT-COMPANY-CD NOT = CL-COMPANY-CD OR                         CL*17
01242         AT-CARRIER    NOT = CL-CARRIER    OR                         CL*17
01243         AT-CLAIM-NO   NOT = CL-CLAIM-NO   OR                         CL*17
01244         AT-CERT-NO    NOT = CL-CERT-NO                               CL*17
01245          GO TO 0945-ENDBR-ELTRLR.                                 EL1502
01246                                                                   EL1502
01247      MOVE 'Y'                    TO  WS-ENDBR-SW.                 EL1502
01248                                                                   EL1502
01249      IF TRLR-SEQ-NO = 90                                             CL*17
01250          GO TO 0935-READNEXT-TRAILER.                             EL1502
01251                                                                   EL1502
01252      IF RESERVE-EXPENSE-TR OR ADDRESS-TR                          EL1502
01253          GO TO 0935-READNEXT-TRAILER.                             EL1502
01254                                                                   EL1502
01255      IF GENERAL-INFO-TR                                           EL1502
052113         IF AT-PAYMENT-NOTE or at-errors-note
01257              GO TO 0935-READNEXT-TRAILER.                         EL1502
01258                                                                   EL1502
01259      IF AUTO-PROMPT-TR                                            EL1502
01260          IF SAVE-BIN-DATE > AT-PROMPT-END-DT                         CL*23
01261              GO TO 0935-READNEXT-TRAILER.                         EL1502
01262                                                                   EL1502
01263      MOVE AT-CERT-NO             TO  WS-CERT-NO     (SUB-2).      EL1502
01264      MOVE AT-SEQUENCE-NO         TO  WS-TRLR-SEQ    (SUB-2).      EL1502
01265      MOVE AT-RECORDED-DT         TO  WS-RECORDED-DT (SUB-2).      EL1502
01266                                                                   EL1502
01267      ADD +1 TO SUB-2.                                                CL*23
01268                                                                      CL*23
01269      IF SUB-2 > 200                                                  CL*23
01270          GO TO 0945-ENDBR-ELTRLR.                                 EL1502
01271                                                                   EL1502
01272      GO TO 0935-READNEXT-TRAILER.                                 EL1502
01273                                                                   EL1502
01274  0945-ENDBR-ELTRLR.                                               EL1502
01275                                                                   EL1502
01276      IF WS-ENDBR-SW = 'N'                                            CL*17
01277          GO TO 0949-BUILD-TABLE-EXIT.                             EL1502
01278                                                                   EL1502
01279      EXEC CICS ENDBR                                              EL1502
01280          DATASET   ('ELTRLR')                                     EL1502
01281      END-EXEC.                                                    EL1502
01282                                                                   EL1502
01283  0949-BUILD-TABLE-EXIT.                                           EL1502
01284      EXIT.                                                        EL1502
01285                                                                   EL1502
01286      EJECT                                                        EL1502
01287  0950-SORT-ROUTINE.                                               EL1502
01288      IF PI-FIRST-TIME-SW = 'Y'                                       CL*17
01289          MOVE 'N'                        TO  PI-FIRST-TIME-SW     EL1502
01290          IF WS-SORTED-SW (SUB-2) = 'Y'                               CL*17
01291              ADD 1 TO SUB-2                                          CL*23
01292              IF (SUB-3 > 200 OR                                      CL*23
01293                  SUB-3 > WS-MAX-SUB)                                 CL*23
01294                  MOVE 'Y'                    TO  PI-FIRST-TIME-SW EL1502
01295                  PERFORM 0700-CREATE-TEMP-STORAGE THRU 0700-EXIT  EL1502
01296                  GO TO 0990-SORT-EXIT                             EL1502
01297              ELSE                                                 EL1502
01298                  MOVE 'Y'                    TO  PI-FIRST-TIME-SW EL1502
01299                  GO TO 0950-SORT-ROUTINE                          EL1502
01300          ELSE                                                     EL1502
01301              MOVE WS-RECORDED-DT (SUB-2) TO  HOLD-RECORDED-DT     EL1502
01302              MOVE SUB-2                  TO  HOLD-SUB             EL1502
01303              GO TO 0950-SORT-ROUTINE                              EL1502
01304      ELSE                                                         EL1502
01305          ADD 1 TO SUB-2                                              CL*23
01306          IF SUB-3 > 200                                              CL*23
01307              MOVE 'Y'                    TO  PI-FIRST-TIME-SW     EL1502
01308              PERFORM 0700-CREATE-TEMP-STORAGE THRU 0700-EXIT      EL1502
01309              GO TO 0990-SORT-EXIT.                                EL1502
01310                                                                   EL1502
01311      IF WS-SORTED-SW (SUB-2) = 'Y'                                   CL*17
01312          GO TO 0950-SORT-ROUTINE.                                 EL1502
01313                                                                   EL1502
01314      IF SUB-3 > WS-MAX-SUB                                           CL*23
01315          MOVE 'Y'            TO  PI-FIRST-TIME-SW                 EL1502
01316          PERFORM 0700-CREATE-TEMP-STORAGE THRU 0700-EXIT          EL1502
01317          GO TO 0990-SORT-EXIT.                                    EL1502
01318                                                                   EL1502
01319      IF WS-RECORDED-DT (SUB-2) = HIGH-VALUES OR                      CL*17
01320         SUB-2 > WS-MAX-SUB                                           CL*23
01321          MOVE WS-RECORDED-DT (HOLD-SUB) TO                        EL1502
01322                                   WS-SRTD-RECORDED-DT (SUB-3)     EL1502
01323          MOVE WS-CERT-NO     (HOLD-SUB) TO                        EL1502
01324                                   WS-SRTD-CERT        (SUB-3)     EL1502
01325          MOVE WS-TRLR-SEQ    (HOLD-SUB) TO                        EL1502
01326                                   WS-SRTD-TRLR-SEQ    (SUB-3)     EL1502
01327          MOVE 'Y'            TO WS-SORTED-SW       (HOLD-SUB)        CL*21
01328                                 PI-FIRST-TIME-SW                     CL*21
01329          ADD 1               TO SUB-3                                CL*21
01330          MOVE 1              TO SUB-2                                CL*21
01331          GO TO 0950-SORT-ROUTINE.                                 EL1502
01332                                                                   EL1502
01333      IF WS-RECORDED-DT (SUB-2) > HOLD-RECORDED-DT                    CL*23
01334          MOVE WS-RECORDED-DT (SUB-2) TO  HOLD-RECORDED-DT         EL1502
01335          MOVE SUB-2                  TO  HOLD-SUB                 EL1502
01336          GO TO 0950-SORT-ROUTINE.                                 EL1502
01337                                                                   EL1502
01338      GO TO 0950-SORT-ROUTINE.                                     EL1502
01339                                                                   EL1502
01340  0990-SORT-EXIT.                                                  EL1502
01341      EXIT.                                                        EL1502
01342                                                                      CL*23
01343  0999-RELATED-EXIT.                                               EL1502
01344      EXIT.                                                        EL1502
01345                                                                   EL1502
01346      EJECT                                                        EL1502
01347  1000-SHOW-CLAIM-HISTORY.                                         EL1502
01348                                                                   EL1502
01349      IF DIRECTION-SWITCH = 'F'                                       CL*17
01350          IF FIRST-TIME                                            EL1502
01351              PERFORM 2070-INIT-SCREEN-AREA THRU 2070-EXIT         EL1502
01352              MOVE 'N'                TO  PI-FIRST-TIME-SW         EL1502
01353              MOVE +1                 TO  DISPLAY-CNT              EL1502
01354                                          SUB-1                    EL1502
01355                                          SUB-2                    EL1502
01356          ELSE                                                     EL1502
01357              MOVE PI-TRLR-SUB (8)    TO  SUB-2                    EL1502
01358              ADD +1                  TO  SUB-2                    EL1502
01359              IF SUB-2 > PI-MAX-SUB                                   CL*23
01360                  GO TO 8200-SEND-DATAONLY                         EL1502
01361              ELSE                                                 EL1502
01362                  PERFORM 2070-INIT-SCREEN-AREA THRU 2070-EXIT     EL1502
01363                  MOVE +1                 TO  DISPLAY-CNT          EL1502
01364                                              SUB-1                EL1502
01365                                              PI-LINE-NO              CL*12
01366                  IF SUB-2 = +9                                       CL*17
01367                     IF WS-SRTD-CERT (SUB-2) = LOW-VALUES             CL*17
01368                      MOVE +1             TO  SUB-2.                  CL*12
01369                                                                   EL1502
01370      IF DIRECTION-SWITCH = 'B'                                       CL*17
01371          IF FIRST-TIME                                            EL1502
01372              GO TO 8200-SEND-DATAONLY                             EL1502
01373          ELSE                                                     EL1502
01374              PERFORM 2070-INIT-SCREEN-AREA THRU 2070-EXIT         EL1502
01375              MOVE PI-TRLR-SUB (1)    TO  SUB-2                    EL1502
01376              IF SUB-2 > +8                                           CL*23
01377                  SUBTRACT +1        FROM SUB-2                       CL*23
01378                  MOVE +15            TO  DISPLAY-CNT              EL1502
01379                  MOVE +8             TO  SUB-1                    EL1502
01380                                          PI-LINE-NO               EL1502
01381              ELSE                                                 EL1502
01382                  MOVE 'F'            TO  DIRECTION-SWITCH         EL1502
01383                  MOVE +1             TO  DISPLAY-CNT              EL1502
01384                                          SUB-1                    EL1502
01385                                          SUB-2                    EL1502
01386                                          PI-LINE-NO.              EL1502
01387                                                                   EL1502
01388  1010-BUILD-HISTORY-SCREEN.                                       EL1502
01389                                                                   EL1502
01390      PERFORM 2000-BUILD-TRAILER-DISPLAY THRU 2999-EXIT.           EL1502
01391                                                                   EL1502
01392      MOVE PI-CARRIER             TO  CARRO.                       EL1502
01393      MOVE PI-CLAIM-NO            TO  CLMNOO.                      EL1502
01394                                                                   EL1502
01395      MOVE -1                     TO  LINENOL.                     EL1502
01396                                                                   EL1502
01397      IF RECORDS-READ                                              EL1502
01398          MOVE 'N'                TO  WS-RECORDS-READ-SW           EL1502
01399          GO TO 8100-SEND-INITIAL-MAP                              EL1502
01400      ELSE                                                         EL1502
01401          GO TO 8200-SEND-DATAONLY.                                EL1502
01402                                                                   EL1502
01403      EJECT                                                        EL1502
01404  2000-BUILD-TRAILER-DISPLAY.                                      EL1502
01405                                                                   EL1502
01406      IF WS-SRTD-CERT (SUB-2) = LOW-VALUES OR                         CL*17
01407         SUB-2 > PI-MAX-SUB                                           CL*23
01408          GO TO 2999-EXIT.                                         EL1502
01409                                                                   EL1502
01410      MOVE PI-COMPANY-CD              TO  TRLR-COMP-CD.            EL1502
01411      MOVE PI-CARRIER                 TO  TRLR-CARRIER.            EL1502
01412      MOVE PI-CLAIM-NO                TO  TRLR-CLAIM-NO.           EL1502
01413      MOVE WS-SRTD-CERT (SUB-2)       TO  TRLR-CERT-NO.            EL1502
01414      MOVE WS-SRTD-TRLR-SEQ (SUB-2)   TO  TRLR-SEQ-NO.             EL1502
01415      MOVE 'TRLR'                     TO  FILE-SWITCH.             EL1502
01416                                                                   EL1502
01417  2020-BROWSE-FORWARD.                                             EL1502
01418                                                                   EL1502
01419      EXEC CICS HANDLE CONDITION                                   EL1502
01420          ENDFILE   (2950-NO-MORE-TRAILERS)                        EL1502
01421          NOTFND    (2950-NO-MORE-TRAILERS)                        EL1502
01422      END-EXEC.                                                    EL1502
01423                                                                   EL1502
01424      EXEC CICS READ                                               EL1502
01425          DATASET   ('ELTRLR')                                     EL1502
01426          RIDFLD    (ELTRLR-KEY)                                   EL1502
01427          SET       (ADDRESS OF ACTIVITY-TRAILERS)                    CL*14
01428      END-EXEC.                                                    EL1502
01429                                                                   EL1502
01430      MOVE 'Y'                    TO  WS-RECORDS-READ-SW.          EL1502
01431      GO TO 2090-DISPLAY-TRAILER.                                  EL1502
01432                                                                   EL1502
01433  EJECT                                                            EL1502
01434  2070-INIT-SCREEN-AREA.                                           EL1502
01435                                                                   EL1502
01436      MOVE SPACES                 TO  TEXT-WORK-AREAS              EL1502
01437                                      MAP-HDG (1)   MAP-TEXT (1)   EL1502
01438                                      MAP-HDG (2)   MAP-TEXT (2)   EL1502
01439                                      MAP-HDG (3)   MAP-TEXT (3)   EL1502
01440                                      MAP-HDG (4)   MAP-TEXT (4)   EL1502
01441                                      MAP-HDG (5)   MAP-TEXT (5)   EL1502
01442                                      MAP-HDG (6)   MAP-TEXT (6)   EL1502
01443                                      MAP-HDG (7)   MAP-TEXT (7)   EL1502
01444                                      MAP-HDG (8)   MAP-TEXT (8)   EL1502
01445                                      MAP-HDG (9)   MAP-TEXT (9)   EL1502
01446                                      MAP-HDG (10)  MAP-TEXT (10)  EL1502
01447                                      MAP-HDG (11)  MAP-TEXT (11)  EL1502
01448                                      MAP-HDG (12)  MAP-TEXT (12)  EL1502
01449                                      MAP-HDG (13)  MAP-TEXT (13)  EL1502
01450                                      MAP-HDG (14)  MAP-TEXT (14)  EL1502
01451                                      MAP-HDG (15)  MAP-TEXT (15)  EL1502
01452                                      MAP-HDG (16)  MAP-TEXT (16). EL1502
01453                                                                   EL1502
01454      MOVE ZEROS                  TO  PMT-AMT-PAID                 EL1502
01455                                      AUTO-1ST-AMT                 EL1502
01456                                      AUTO-REG-AMT                 EL1502
01457                                      INCUR-TOT-PD.                EL1502
01458                                                                   EL1502
01459  2070-EXIT.                                                       EL1502
01460      EXIT.                                                        EL1502
01461                                                                   EL1502
01462  EJECT                                                            EL1502
01463  2090-DISPLAY-TRAILER.                                            EL1502
01464                                                                   EL1502
01465      IF PAYMENT-TR                                                EL1502
01466          GO TO 2100-PAYMENT-TRAILER.                              EL1502
01467                                                                   EL1502
01468      IF AUTO-PAY-TR                                               EL1502
01469         GO TO 2200-AUTO-PAYMENT-TRAILER.                          EL1502
01470                                                                   EL1502
01471      IF CORRESPONDENCE-TR                                         EL1502
01472          GO TO 2300-CORRESPONDENCE-TRAILER.                       EL1502
01473                                                                   EL1502
01474      IF GENERAL-INFO-TR                                           EL1502
01475          GO TO 2400-GENERAL-INFO-TRAILER.                         EL1502
01476                                                                   EL1502
01477      IF AUTO-PROMPT-TR                                            EL1502
01478          GO TO 2500-AUTO-PROMPT-TRAILER.                          EL1502
01479                                                                   EL1502
01480      IF DENIAL-TR                                                 EL1502
01481          GO TO 2600-DENIAL-TRAILER.                               EL1502
01482                                                                   EL1502
01483      IF INCURRED-CHG-TR                                           EL1502
01484          GO TO 2700-INCURRED-CHANGE-TRAILER.                      EL1502
01485                                                                   EL1502
01486      IF FORM-CONTROL-TR                                           EL1502
01487          GO TO 2710-FORM-CONTROL-TRAILER.                         EL1502
01488                                                                   EL1502
01489      GO TO 2020-BROWSE-FORWARD.                                   EL1502
01490                                                                   EL1502
01491      EJECT                                                        EL1502
01492  2100-PAYMENT-TRAILER.                                            EL1502
01493      MOVE SPACES                 TO  PMT-HDG1.                    EL1502
01494      MOVE ZEROS                  TO  PMT-AMT-PAID.                EL1502
01495      MOVE PI-LINE-NO             TO  PMT-LINE-NO.                 EL1502
01496      MOVE 'PAYMENT '             TO  PMT-HDG1-LIT.                EL1502
01497                                                                   EL1502
01498      MOVE SPACES                 TO  PMT-TEXT-1.                  EL1502
01499      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.        EL1502
01500      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.          EL1502
01501      MOVE WS-FORMAT-CERT-NO      TO  PMT-CERT-NO.                 EL1502
01502                                                                   EL1502
01503      MOVE ' TYPE: '              TO  PMT-TYPE-LIT.                EL1502
01504                                                                      CL*13
022106     IF TRANSFER
022106        MOVE 'TRANSFR'           TO PMT-TYPE
022106        GO TO 2100-CONT
022106     ELSE
022106        IF AT-PAYMENT-TYPE = 'I'
022106           MOVE 'INTEREST'       TO PMT-TYPE
022106           GO TO 2100-CONT
022106        END-IF
022106     END-IF
01508                                                                      CL*14
01509      IF AT-CV-PMT-CODE = ' '                                         CL*17
01510          MOVE AT-PAYMENT-TYPE            TO  WS-SUB                  CL*13
01511          IF WS-SUB < 1 OR > 9                                        CL*23
01512              MOVE 2                      TO  WS-SUB                  CL*13
01513              MOVE PAY-DESC (WS-SUB)      TO  PMT-TYPE                CL*13
01514          ELSE                                                        CL*13
01515              MOVE PAY-DESC (WS-SUB)      TO  PMT-TYPE                CL*13
01516      ELSE                                                            CL*13
01517          MOVE AT-CV-PMT-CODE             TO  WS-SUB                  CL*13
01518          IF WS-SUB < 1 OR > 8                                        CL*23
01519              MOVE 1                      TO  WS-SUB                  CL*13
01520              MOVE CV-PAY-DESC (WS-SUB)   TO  PMT-TYPE                CL*13
01521          ELSE                                                        CL*13
01522              MOVE CV-PAY-DESC (WS-SUB)   TO  PMT-TYPE.               CL*13
01523                                                                      CL*14
01524  2100-CONT.                                                          CL*14
01525                                                                   EL1502
013017     if at-ach-payment = 'Y'
013017        move ' ACH PAYMT '       to pmt-check-no-lit
              move 'ACH PMNT'          to PMT-HDG1-LIT
013017     else
013017        MOVE ' CHECK NO: '       TO  PMT-CHECK-NO-LIT
013017     end-if

01526 *    MOVE ' CHECK NO: '          TO  PMT-CHECK-NO-LIT.            EL1502
01527      MOVE AT-CHECK-NO            TO  PMT-CHECK-NO.                EL1502
01528                                                                   EL1502
01529      MOVE ' AMT:'                TO  PMT-AMT-PD-LIT.                 CL*13
01530      MOVE AT-AMOUNT-PAID         TO  PMT-AMT-PAID.                EL1502
01531      MOVE ' BY: '                TO  PMT-REC-BY-LIT.              EL1502
01532      MOVE AT-RECORDED-BY         TO  PMT-REC-BY.                  EL1502
01533                                                                   EL1502
01534      MOVE SPACES                 TO  PMT-HDG2                     EL1502
01535                                      PMT-TEXT-2.                  EL1502
01536      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               EL1502
01537      MOVE ' '                    TO  DC-OPTION-CODE.              EL1502
01538      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1502
01539      IF NO-CONVERSION-ERROR                                       EL1502
01540          MOVE DC-GREG-DATE-1-EDIT    TO  PMT-REC-DT               EL1502
01541      ELSE                                                         EL1502
01542          MOVE SPACES                 TO  PMT-REC-DT.              EL1502
01543                                                                   EL1502
01544      IF AT-PAID-FROM-DT = LOW-VALUES OR SPACES                       CL*17
01545          MOVE SPACES                     TO  PMT-PAID-FROM        EL1502
01546                                              PMT-FROM-LIT         EL1502
01547      ELSE                                                         EL1502
01548          MOVE 'FROM: '                   TO  PMT-FROM-LIT         EL1502
01549          MOVE AT-PAID-FROM-DT            TO  DC-BIN-DATE-1        EL1502
01550          MOVE ' '                        TO  DC-OPTION-CODE       EL1502
01551          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
01552          IF NO-CONVERSION-ERROR                                   EL1502
01553              MOVE DC-GREG-DATE-1-EDIT    TO PMT-PAID-FROM         EL1502
01554          ELSE                                                     EL1502
01555              MOVE SPACES                 TO  PMT-PAID-FROM.       EL1502
01556                                                                   EL1502
01557      IF AT-PAID-THRU-DT = LOW-VALUES OR SPACES                       CL*17
01558          MOVE SPACES                     TO  PMT-PAID-THRU        EL1502
01559                                              PMT-THRU-LIT         EL1502
01560      ELSE                                                         EL1502
01561          IF PI-USES-PAID-TO                                       EL1502
01562              MOVE ' PAID TO: '           TO  PMT-THRU-LIT         EL1502
01563              MOVE AT-PAID-THRU-DT        TO  DC-BIN-DATE-1        EL1502
01564              MOVE '6'                    TO  DC-OPTION-CODE       EL1502
01565              MOVE +1                     TO  DC-ELAPSED-DAYS      EL1502
01566              MOVE +0                     TO  DC-ELAPSED-MONTHS    EL1502
01567              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1502
01568              IF NO-CONVERSION-ERROR                               EL1502
01569                  MOVE DC-GREG-DATE-1-EDIT TO PMT-PAID-THRU        EL1502
01570              ELSE                                                 EL1502
01571                  MOVE SPACES             TO  PMT-PAID-THRU        EL1502
01572          ELSE                                                     EL1502
01573              MOVE ' PD THRU: '           TO  PMT-THRU-LIT         EL1502
01574              MOVE AT-PAID-THRU-DT        TO  DC-BIN-DATE-1        EL1502
01575              MOVE ' '                    TO  DC-OPTION-CODE       EL1502
01576              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1502
01577              IF NO-CONVERSION-ERROR                               EL1502
01578                  MOVE DC-GREG-DATE-1-EDIT TO PMT-PAID-THRU        EL1502
01579              ELSE                                                 EL1502
01580                  MOVE SPACES             TO  PMT-PAID-THRU.       EL1502
01581                                                                   EL1502
01582      IF INSURED-PAID                                              EL1502
01583          MOVE 'INS '             TO  PMT-PAYEE.                   EL1502
01584      IF BENEFICIARY-PAID                                          EL1502
01585          MOVE 'BENE'             TO  PMT-PAYEE.                   EL1502
01586      IF ACCOUNT-PAID                                              EL1502
01587          MOVE 'ACCT'             TO  PMT-PAYEE.                   EL1502
01588      IF OTHER-1-PAID                                              EL1502
01589          MOVE 'OTH1'             TO  PMT-PAYEE.                   EL1502
01590      IF OTHER-2-PAID                                              EL1502
01591          MOVE 'OTH2'             TO  PMT-PAYEE.                   EL1502
01592      IF DOCTOR-PAID                                               EL1502
01593          MOVE 'DOC '             TO  PMT-PAYEE.                   EL1502
01594      IF EMPLOYER-PAID                                                CL*12
01595          MOVE 'EMP '             TO  PMT-PAYEE.                      CL*12
01596                                                                      CL*12
01597      IF AT-VOID-DT = LOW-VALUES OR SPACES                            CL*21
01598          NEXT SENTENCE                                               CL*12
01599      ELSE                                                            CL*12
01600          IF AT-VOID-TYPE = 'S'                                       CL*17
01601              MOVE ' ST PAY: '            TO  PMT-VOID-LIT            CL*12
01602          ELSE                                                        CL*12
01603              MOVE ' VOIDED: '            TO  PMT-VOID-LIT.           CL*12
01604                                                                   EL1502
01605      IF AT-VOID-DT = LOW-VALUES OR SPACES                            CL*17
01606          MOVE SPACES                     TO  PMT-VOID-DT          EL1502
01607                                              PMT-VOID-LIT         EL1502
01608      ELSE                                                         EL1502
01609          MOVE AT-VOID-DT                 TO  DC-BIN-DATE-1        EL1502
01610          MOVE ' '                        TO  DC-OPTION-CODE       EL1502
01611          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
01612          IF NO-CONVERSION-ERROR                                   EL1502
01613              MOVE DC-GREG-DATE-1-EDIT    TO  PMT-VOID-DT          EL1502
01614          ELSE                                                     EL1502
01615              MOVE SPACES                 TO  PMT-VOID-DT.         EL1502
01616                                                                   EL1502
01617      MOVE PMT-HDG1               TO  MAP-HDG        (DISPLAY-CNT).EL1502
01618      MOVE PMT-TEXT-1             TO  MAP-TEXT       (DISPLAY-CNT).EL1502
01619      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1502
01620      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
01621      ADD +1 TO DISPLAY-CNT.                                          CL*23
01622      MOVE PMT-HDG2               TO  MAP-HDG        (DISPLAY-CNT).EL1502
01623      MOVE PMT-TEXT-2             TO  MAP-TEXT       (DISPLAY-CNT).EL1502
01624      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1502
01625                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
01626      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1502
01627      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1502
01628      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1502
01629      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).      EL1502
01630      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).      EL1502
01631                                                                   EL1502
01632      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1502
01633                                                                   EL1502
01634      EJECT                                                        EL1502
01635  2200-AUTO-PAYMENT-TRAILER.                                       EL1502
01636      MOVE SPACES                 TO  AUTO-PMT-HDG1.               EL1502
01637      MOVE PI-LINE-NO             TO  AUTO-LINE-NO.                EL1502
01638      MOVE 'AUTO PMT'             TO  AUTO-HDG1-LIT.               EL1502
01639                                                                   EL1502
01640      MOVE SPACES                 TO  AUTO-PMT-TEXT1.              EL1502
01641      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.        EL1502
01642      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.          EL1502
01643      MOVE WS-FORMAT-CERT-NO      TO  AUTO-CERT-NO.                EL1502
01644                                                                   EL1502
01645      IF AT-SCHEDULE-START-DT = LOW-VALUES OR SPACES                  CL*17
01646          MOVE SPACES                     TO  AUTO-EFF-DT          EL1502
01647                                              AUTO-EFF-DT-LIT      EL1502
01648      ELSE                                                         EL1502
01649          MOVE ' EFF : '                  TO  AUTO-EFF-DT-LIT      EL1502
01650          MOVE AT-SCHEDULE-START-DT       TO  DC-BIN-DATE-1        EL1502
01651          MOVE ' '                        TO  DC-OPTION-CODE       EL1502
01652          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
01653          IF NO-CONVERSION-ERROR                                   EL1502
01654              MOVE DC-GREG-DATE-1-EDIT    TO  AUTO-EFF-DT          EL1502
01655          ELSE                                                     EL1502
01656              MOVE SPACES                 TO  AUTO-EFF-DT.         EL1502
01657                                                                   EL1502
01658      IF AT-1ST-PAY-THRU-DT = LOW-VALUES OR SPACES                    CL*17
01659          MOVE SPACES                     TO  AUTO-1ST-PMT-DT      EL1502
01660                                              AUTO-1ST-PMT-LIT     EL1502
01661      ELSE                                                         EL1502
01662          MOVE ' 1ST PMT: '               TO  AUTO-1ST-PMT-LIT     EL1502
01663          IF PI-USES-PAID-TO                                          CL**4
01664              MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1           CL**4
01665              MOVE '6'                    TO  DC-OPTION-CODE          CL**4
01666              MOVE +1                     TO  DC-ELAPSED-DAYS         CL**4
01667              MOVE +0                     TO  DC-ELAPSED-MONTHS       CL**4
01668              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL**4
01669              IF NO-CONVERSION-ERROR                                  CL**4
01670                  MOVE DC-GREG-DATE-1-EDIT TO AUTO-1ST-PMT-DT         CL**4
01671              ELSE                                                    CL**4
01672                  MOVE SPACES             TO  AUTO-1ST-PMT-DT         CL**4
01673          ELSE                                                     EL1502
01674              MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1           CL**4
01675              MOVE ' '                    TO  DC-OPTION-CODE          CL**4
01676              MOVE +0                     TO  DC-ELAPSED-DAYS         CL**4
01677                                              DC-ELAPSED-MONTHS       CL**4
01678              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL**4
01679              IF NO-CONVERSION-ERROR                                  CL**4
01680                  MOVE DC-GREG-DATE-1-EDIT TO AUTO-1ST-PMT-DT         CL**4
01681              ELSE                                                    CL**4
01682                  MOVE SPACES             TO  AUTO-1ST-PMT-DT.        CL**4
01683                                                                   EL1502
01684      MOVE ' AMT: '               TO  AUTO-1ST-AMT-LIT.            EL1502
01685      MOVE AT-FIRST-PMT-AMT       TO  AUTO-1ST-AMT.                EL1502
01686                                                                   EL1502
01687      MOVE ' BY: '                TO  AUTO-REC-BY-LIT.             EL1502
01688      MOVE AT-RECORDED-BY         TO  AUTO-REC-BY.                 EL1502
01689                                                                   EL1502
01690      MOVE SPACES                 TO  AUTO-PMT-HDG2                EL1502
01691                                      AUTO-PMT-TEXT2.              EL1502
01692      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               EL1502
01693      MOVE ' '                    TO  DC-OPTION-CODE.              EL1502
01694      MOVE +0                     TO  DC-ELAPSED-DAYS                 CL**4
01695                                      DC-ELAPSED-MONTHS.              CL**4
01696      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1502
01697      IF NO-CONVERSION-ERROR                                       EL1502
01698          MOVE DC-GREG-DATE-1-EDIT    TO  AUTO-REC-DT              EL1502
01699      ELSE                                                         EL1502
01700          MOVE SPACES                 TO  AUTO-REC-DT.             EL1502
01701                                                                   EL1502
01702      IF AT-TERMINATED-DT = LOW-VALUES OR SPACES                      CL*17
01703          IF AT-SCHEDULE-END-DT = LOW-VALUES                          CL*17
01704              MOVE SPACES                     TO  AUTO-LST-PMT-DT  EL1502
01705          ELSE                                                     EL1502
01706              MOVE 'END : '                   TO  AUTO-LST-PMT-LIT EL1502
01707              IF PI-USES-PAID-TO                                      CL**4
01708                  MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1       CL**4
01709                  MOVE '6'                    TO  DC-OPTION-CODE      CL**4
01710                  MOVE +1                     TO  DC-ELAPSED-DAYS     CL**4
01711                  MOVE +0                     TO  DC-ELAPSED-MONTHS   CL**4
01712                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT       CL**4
01713                  IF NO-CONVERSION-ERROR                              CL**4
01714                      MOVE DC-GREG-DATE-1-EDIT TO AUTO-LST-PMT-DT     CL**4
01715                  ELSE                                                CL**4
01716                      MOVE SPACES             TO  AUTO-LST-PMT-DT     CL**4
01717              ELSE                                                 EL1502
01718                  MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1       CL**4
01719                  MOVE ' '                    TO  DC-OPTION-CODE      CL**4
01720                  MOVE +0                     TO  DC-ELAPSED-DAYS     CL**4
01721                                                  DC-ELAPSED-MONTHS   CL**4
01722                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT       CL**4
01723                  IF NO-CONVERSION-ERROR                              CL**4
01724                      MOVE DC-GREG-DATE-1-EDIT TO AUTO-LST-PMT-DT     CL**4
01725                  ELSE                                                CL**4
01726                      MOVE SPACES             TO  AUTO-LST-PMT-DT     CL**4
01727      ELSE                                                         EL1502
01728          MOVE ' TERM: '                      TO  AUTO-LST-PMT-LIT EL1502
01729          MOVE AT-TERMINATED-DT               TO  DC-BIN-DATE-1    EL1502
01730          MOVE ' '                            TO  DC-OPTION-CODE   EL1502
01731          MOVE +0                             TO  DC-ELAPSED-DAYS     CL**4
01732                                                  DC-ELAPSED-MONTHS   CL**4
01733          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
01734          IF NO-CONVERSION-ERROR                                   EL1502
01735              MOVE DC-GREG-DATE-1-EDIT        TO  AUTO-LST-PMT-DT  EL1502
01736          ELSE                                                     EL1502
01737              MOVE SPACES                     TO  AUTO-LST-PMT-DT. EL1502
01738                                                                   EL1502
01739      MOVE ' STATUS : '           TO  AUTO-STATUS-LIT.             EL1502
01740                                                                      CL*23
01741      IF AT-TERMINATED-DT NOT = LOW-VALUES                            CL*17
01742          MOVE 'TERM'             TO  AUTO-STATUS                  EL1502
01743      ELSE                                                         EL1502
01744          MOVE 'ACTIVE'           TO  AUTO-STATUS.                 EL1502
01745                                                                   EL1502
01746      MOVE '   REG: '             TO  AUTO-REG-AMT-LIT.            EL1502
01747      MOVE AT-REGULAR-PMT-AMT     TO  AUTO-REG-AMT.                EL1502
01748                                                                   EL1502
01749      MOVE ' TO: '                TO  AUTO-PAYEE-LIT.              EL1502
01750                                                                      CL*23
01751      IF INSURED-PAID-AUTO                                         EL1502
01752          MOVE ' INS '            TO  AUTO-PAYEE.                     CL**4
01753      IF BENEFICIARY-PAID-AUTO                                     EL1502
01754          MOVE ' BENE'            TO  AUTO-PAYEE.                     CL**4
01755      IF ACCOUNT-PAID-AUTO                                         EL1502
01756          MOVE ' ACCT'            TO  AUTO-PAYEE.                     CL**4
01757      IF OTHER-1-PAID-AUTO                                         EL1502
01758          MOVE ' OTH1'            TO  AUTO-PAYEE.                     CL**4
01759      IF OTHER-2-PAID                                              EL1502
01760          MOVE ' OTH2'            TO  AUTO-PAYEE.                     CL**4
01761      IF DOCTOR-PAID                                               EL1502
01762          MOVE ' DOC '            TO  AUTO-PAYEE.                     CL**4
01763                                                                   EL1502
01764      MOVE AUTO-PMT-HDG1          TO  MAP-HDG        (DISPLAY-CNT).EL1502
01765      MOVE AUTO-PMT-TEXT1         TO  MAP-TEXT       (DISPLAY-CNT).EL1502
01766      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1502
01767      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
01768      ADD +1 TO DISPLAY-CNT.                                          CL*23
01769      MOVE AUTO-PMT-HDG2          TO  MAP-HDG        (DISPLAY-CNT).EL1502
01770      MOVE AUTO-PMT-TEXT2         TO  MAP-TEXT       (DISPLAY-CNT).EL1502
01771      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1502
01772                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
01773      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1502
01774      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1502
01775      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1502
01776      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).      EL1502
01777      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).      EL1502
01778                                                                   EL1502
01779      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1502
01780                                                                   EL1502
01781      EJECT                                                        EL1502
01782  2300-CORRESPONDENCE-TRAILER.                                     EL1502
01783      MOVE SPACES                 TO  CORR-HDG1.                   EL1502
01784      MOVE PI-LINE-NO             TO  CORR-LINE-NO.                EL1502
01785      MOVE 'LETTER'               TO  CORR-HDG1-LIT.               EL1502
01786                                                                   EL1502
01787      MOVE SPACES                 TO  CORR-TEXT-1.                 EL1502
01788      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.        EL1502
01789      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.          EL1502
01790      MOVE WS-FORMAT-CERT-NO      TO  CORR-CERT-NO.                EL1502
01791                                                                   EL1502
01792      MOVE ' FORM: '              TO  CORR-FORM-LIT.               EL1502
01793      MOVE AT-STD-LETTER-FORM     TO  CORR-FORM-TYPE.              EL1502
01794                                                                   EL1502
01795      IF AT-LETTER-SENT-DT = LOW-VALUES OR SPACES                     CL*17
01796          MOVE SPACES                     TO  CORR-DT-SENT         EL1502
01797                                              CORR-DT-SENT-LIT     EL1502
01798      ELSE                                                         EL1502
01799          MOVE '  DT SENT: '              TO  CORR-DT-SENT-LIT     EL1502
01800          MOVE AT-LETTER-SENT-DT          TO  DC-BIN-DATE-1        EL1502
01801          MOVE ' '                        TO  DC-OPTION-CODE       EL1502
01802          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
01803          IF NO-CONVERSION-ERROR                                   EL1502
01804              MOVE DC-GREG-DATE-1-EDIT    TO  CORR-DT-SENT         EL1502
01805          ELSE                                                     EL1502
01806              MOVE SPACES                 TO  CORR-DT-SENT.        EL1502
01807                                                                   EL1502
01808      IF AT-LETTER-PURGED-DT = LOW-VALUES OR SPACES                   CL*17
071210       IF AT-LETTER-TO-BENE EQUAL 'Y'
071210          MOVE ' LETTER TO BENE' TO CORR-LET-TO-BEN
071210       ELSE 
062217          IF AT-AUTH-RCVD >  SPACES
062217             MOVE ' AUTH RCVD: ' TO CORR-LET-TO-BEN
062217             MOVE AT-AUTH-RCVD TO CORR-LET-TO-BEN(13:1)
062217          ELSE
01809              MOVE SPACES             TO  CORR-PURGE-DT            EL1502
01810                                          CORR-PURGE-LIT           EL1502
062217          END-IF
071210       END-IF
01811      ELSE                                                         EL1502
01812          MOVE ' PUR: '                   TO  CORR-PURGE-LIT       EL1502
01813          MOVE AT-LETTER-PURGED-DT        TO  DC-BIN-DATE-1        EL1502
01814          MOVE ' '                        TO  DC-OPTION-CODE       EL1502
01815          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
01816          IF NO-CONVERSION-ERROR                                   EL1502
01817              MOVE DC-GREG-DATE-1-EDIT    TO  CORR-PURGE-DT        EL1502
01818          ELSE                                                     EL1502
01819              MOVE SPACES                 TO  CORR-PURGE-DT.       EL1502
01820                                                                   EL1502
01821      MOVE ' BY: '                TO  CORR-REC-BY-LIT.             EL1502
01822      MOVE AT-RECORDED-BY         TO  CORR-REC-BY.                 EL1502
01823                                                                   EL1502
01824      MOVE SPACES                 TO  CORR-HDG2                    EL1502
01825                                      CORR-TEXT-2.                 EL1502
01826      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               EL1502
01827      MOVE ' '                    TO  DC-OPTION-CODE.              EL1502
01828      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1502
01829      IF NO-CONVERSION-ERROR                                       EL1502
01830          MOVE DC-GREG-DATE-1-EDIT TO CORR-REC-DT                  EL1502
01831      ELSE                                                         EL1502
01832          MOVE SPACES             TO  CORR-REC-DT.                 EL1502
01833                                                                   EL1502
01834      IF AT-RESEND-PRINT-DATE = LOW-VALUES OR SPACES                  CL*17
01835          IF AT-AUTO-RE-SEND-DT = LOW-VALUES OR SPACES                CL*17
01836              MOVE SPACES                    TO  CORR-RESEND-DT    EL1502
01837          ELSE                                                     EL1502
01838              MOVE 'RSND: '                  TO  CORR-RESEND-LIT   EL1502
01839              MOVE AT-AUTO-RE-SEND-DT        TO  DC-BIN-DATE-1     EL1502
01840              MOVE ' '                       TO  DC-OPTION-CODE    EL1502
01841              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1502
01842              IF NO-CONVERSION-ERROR                               EL1502
01843                  MOVE DC-GREG-DATE-1-EDIT   TO  CORR-RESEND-DT    EL1502
01844              ELSE                                                 EL1502
01845                  MOVE SPACES                TO  CORR-RESEND-DT    EL1502
01846      ELSE                                                         EL1502
01847          MOVE 'RSNT :'                      TO  CORR-RESEND-LIT   EL1502
01848          MOVE AT-RESEND-PRINT-DATE          TO  DC-BIN-DATE-1     EL1502
01849          MOVE ' '                           TO  DC-OPTION-CODE    EL1502
01850          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
01851          IF NO-CONVERSION-ERROR                                   EL1502
01852              MOVE DC-GREG-DATE-1-EDIT       TO  CORR-RESEND-DT    EL1502
01853          ELSE                                                     EL1502
01854              MOVE SPACES                    TO  CORR-RESEND-DT.   EL1502
01855                                                                   EL1502
113010     IF AT-STOP-LETTER-DT NOT = LOW-VALUES AND SPACES
113010        AND (AT-LETTER-ANSWERED-DT = LOW-VALUES  OR
113010             AT-LETTER-ANSWERED-DT > AT-STOP-LETTER-DT)
113010           MOVE '    STOP:'             TO  CORR-RECVD-LIT
113010           MOVE AT-STOP-LETTER-DT       TO  DC-BIN-DATE-1
113010           MOVE ' '                     TO  DC-OPTION-CODE
113010           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
113010           IF NO-CONVERSION-ERROR
113010              MOVE DC-GREG-DATE-1-EDIT  TO  CORR-RECVD-DT
113010           ELSE
113010              MOVE SPACES               TO  CORR-RECVD-DT
113010                                            CORR-RECVD-LIT
113010           END-IF
113010     ELSE
113010        IF AT-LETTER-ANSWERED-DT = LOW-VALUES
113010            MOVE SPACES                 TO  CORR-RECVD-DT
113010                                            CORR-RECVD-LIT
113010        ELSE
041613            IF AT-LETTER-SENT-DT = LOW-VALUES OR SPACES
041613               MOVE 'MAIL RCVD'    TO CORR-HDG1-LIT
041613            END-IF
113010            MOVE ' RECVD  : '           TO  CORR-RECVD-LIT
113010            MOVE AT-LETTER-ANSWERED-DT  TO  DC-BIN-DATE-1
113010            MOVE ' '                    TO  DC-OPTION-CODE
113010            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
113010            IF NO-CONVERSION-ERROR
113010                MOVE DC-GREG-DATE-1-EDIT TO CORR-RECVD-DT
113010            ELSE
113010                MOVE SPACES             TO  CORR-RECVD-DT
113010                                            CORR-RECVD-LIT
113010            END-IF
113010        END-IF
113010     END-IF.
01868                                                                   EL1502
01869      IF AT-RECEIPT-FOLLOW-UP = LOW-VALUES OR SPACES                  CL*17
01870          MOVE SPACES                    TO  CORR-FOLLOW-UP-DT     EL1502
01871                                             CORR-FOLLOW-UP-LIT    EL1502
01872      ELSE                                                         EL1502
01873          MOVE ' FOL: '                  TO  CORR-FOLLOW-UP-LIT    EL1502
01874          MOVE AT-RECEIPT-FOLLOW-UP      TO  DC-BIN-DATE-1         EL1502
01875          MOVE ' '                       TO  DC-OPTION-CODE        EL1502
01876          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
01877          IF NO-CONVERSION-ERROR                                   EL1502
01878              MOVE DC-GREG-DATE-1-EDIT   TO  CORR-FOLLOW-UP-DT     EL1502
01879          ELSE                                                     EL1502
01880              MOVE SPACES                TO  CORR-FOLLOW-UP-DT.    EL1502
01881                                                                   EL1502
01882      MOVE ' TO: '                       TO  CORR-ADDRESEE-LIT.    EL1502
01883                                                                      CL*21
01884      IF AT-ADDRESEE-TYPE = 'I'                                       CL*17
01885          MOVE 'INS '                    TO  CORR-ADDRESEE            CL*21
01886      ELSE                                                         EL1502
01887      IF AT-ADDRESEE-TYPE = 'B'                                       CL*21
01888          MOVE 'BENE'                    TO  CORR-ADDRESEE            CL*21
01889      ELSE                                                            CL*21
01890      IF AT-ADDRESEE-TYPE = 'A'                                       CL*21
01891          MOVE 'ACCT'                    TO  CORR-ADDRESEE            CL*21
01892      ELSE                                                            CL*21
01893      IF AT-ADDRESEE-TYPE = 'P'                                       CL*21
01894          MOVE 'DOC '                    TO  CORR-ADDRESEE            CL*21
01895      ELSE                                                            CL*21
01896      IF AT-ADDRESEE-TYPE = 'E'                                       CL*21
01897          MOVE 'EMP '                    TO  CORR-ADDRESEE            CL*21
01898      ELSE                                                            CL*21
01899      IF AT-ADDRESEE-TYPE = 'O'                                       CL*21
01900          MOVE 'OTH1'                    TO  CORR-ADDRESEE            CL*21
01901      ELSE                                                            CL*21
01902          MOVE 'OTH2'                    TO  CORR-ADDRESEE.           CL*21
01903                                                                   EL1502
01904      MOVE CORR-HDG1              TO  MAP-HDG        (DISPLAY-CNT).EL1502
01905      MOVE CORR-TEXT-1            TO  MAP-TEXT       (DISPLAY-CNT).EL1502
01906      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1502
01907      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
01908      ADD +1 TO DISPLAY-CNT.                                          CL*23
01909      MOVE CORR-HDG2              TO  MAP-HDG        (DISPLAY-CNT).EL1502
01910      MOVE CORR-TEXT-2            TO  MAP-TEXT       (DISPLAY-CNT).EL1502
01911      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1502
01912                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
01913      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1502
01914      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1502
01915      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1502
01916      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).      EL1502
01917      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).      EL1502
01918                                                                   EL1502
01919      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1502
01920                                                                   EL1502
01921      EJECT                                                        EL1502
01922  2400-GENERAL-INFO-TRAILER.                                       EL1502
01923                                                                   EL1502
01924      MOVE SPACES                 TO  GEN-INFO-LINE-1                 CL*14
01925                                      GEN-INFO-LINE-2.                CL*14
01926      MOVE PI-LINE-NO             TO  GI-LINE-NO.                  EL1502
01927                                                                   EL1502
01928      IF AT-MAINT-NOTE                                                CL*21
01929          MOVE 'MAINT'            TO  GI-HDG1-LIT                     CL*14
01930      ELSE                                                            CL*14
01931      IF AT-CALL-NOTE                                                 CL*23
01932          MOVE 'CALL'             TO  GI-HDG1-LIT                     CL*23
01933          IF AT-PHONE-CALL-IN                                         CL*23
01934              MOVE '     IN'      TO  GI-HDG2-LIT                     CL*23
01935            ELSE                                                      CL*23
01936              MOVE '     OUT'     TO  GI-HDG2-LIT                     CL*23
01937      ELSE                                                            CL*23
01938      IF AT-CERT-CHANGE                                               CL*23
01939          MOVE 'CERT CHG'         TO  GI-HDG1-LIT                     CL*23
01940      ELSE                                                            CL*23
01941          MOVE 'NOTE'             TO  GI-HDG1-LIT.                    CL*23
01942                                                                      CL*14
01943      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.        EL1502
01944      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.          EL1502
01945      MOVE WS-FORMAT-CERT-NO      TO  GEN-INFO-CERT-NO.               CL*12
01946                                                                   EL1502
01947      MOVE ' MSG : '              TO  GEN-INFO-MSG-1-LIT.          EL1502
01948      MOVE AT-INFO-LINE-1         TO  GEN-INFO-MSG-1.              EL1502
01949      MOVE ' BY: '                TO  GEN-INFO-REC-BY-LIT.         EL1502
01950      MOVE AT-RECORDED-BY         TO  GEN-INFO-REC-BY.             EL1502
01951                                                                   EL1502
01952      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               EL1502
01953      MOVE ' '                    TO  DC-OPTION-CODE.              EL1502
01954      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1502
01955      IF NO-CONVERSION-ERROR                                       EL1502
01956          MOVE DC-GREG-DATE-1-EDIT TO GEN-INFO-REC-DT              EL1502
01957      ELSE                                                         EL1502
01958          MOVE SPACES             TO  GEN-INFO-REC-DT.             EL1502
01959                                                                   EL1502
01960      MOVE 'MSG : '               TO  GEN-INFO-MSG-2-LIT.          EL1502
01961      MOVE AT-INFO-LINE-2         TO  GEN-INFO-MSG-2.              EL1502
01962                                                                   EL1502
01963      MOVE GEN-INFO-HDG1          TO  MAP-HDG        (DISPLAY-CNT).EL1502
01964      MOVE GEN-INFO-TEXT-1        TO  MAP-TEXT       (DISPLAY-CNT).EL1502
01965      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1502
01966      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
01967      ADD +1 TO DISPLAY-CNT.                                          CL*23
01968      MOVE GEN-INFO-HDG2          TO  MAP-HDG        (DISPLAY-CNT).EL1502
01969      MOVE GEN-INFO-TEXT-2        TO  MAP-TEXT       (DISPLAY-CNT).EL1502
01970      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1502
01971                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
01972      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1502
01973      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1502
01974      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1502
01975      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).      EL1502
01976      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).      EL1502
01977                                                                   EL1502
01978      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1502
01979                                                                   EL1502
01980      EJECT                                                        EL1502
01981  2500-AUTO-PROMPT-TRAILER.                                        EL1502
01982                                                                   EL1502
01983      MOVE SPACES                 TO  REMINDER-HDG1.               EL1502
01984      MOVE PI-LINE-NO             TO  REM-LINE-NO.                 EL1502
01985      MOVE 'REMINDER'             TO  REM-HDG1-LIT.                EL1502
01986                                                                   EL1502
01987      MOVE SPACES                 TO  REMINDER-TEXT-1.             EL1502
01988      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.        EL1502
01989      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.          EL1502
01990      MOVE WS-FORMAT-CERT-NO      TO  REM-CERT-NO.                 EL1502
01991                                                                   EL1502
01992      MOVE ' LN 1: '              TO  REM-LINE-1-LIT.              EL1502
01993      MOVE AT-PROMPT-LINE-1       TO  REM-LINE-1.                  EL1502
01994      MOVE ' BY: '                TO  REM-REC-BY-LIT.              EL1502
01995      MOVE AT-RECORDED-BY         TO  REM-REC-BY.                  EL1502
01996                                                                   EL1502
01997      MOVE SPACES                 TO  REMINDER-HDG2                EL1502
01998                                      REMINDER-TEXT-2.             EL1502
01999      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               EL1502
02000      MOVE ' '                    TO  DC-OPTION-CODE.              EL1502
02001      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1502
02002      IF NO-CONVERSION-ERROR                                       EL1502
02003          MOVE DC-GREG-DATE-1-EDIT TO REM-REC-DT                   EL1502
02004      ELSE                                                         EL1502
02005          MOVE SPACES             TO  REM-REC-DT.                  EL1502
02006                                                                   EL1502
02007      MOVE 'LN 2: '               TO  REM-LINE-2-LIT.              EL1502
02008      MOVE AT-PROMPT-LINE-2       TO  REM-LINE-2.                  EL1502
02009                                                                   EL1502
02010      MOVE REMINDER-HDG1          TO  MAP-HDG        (DISPLAY-CNT).EL1502
02011      MOVE REMINDER-TEXT-1        TO  MAP-TEXT       (DISPLAY-CNT).EL1502
02012      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1502
02013      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
02014      ADD +1 TO DISPLAY-CNT.                                          CL*23
02015      MOVE REMINDER-HDG2          TO  MAP-HDG        (DISPLAY-CNT).EL1502
02016      MOVE REMINDER-TEXT-2        TO  MAP-TEXT       (DISPLAY-CNT).EL1502
02017      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1502
02018                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
02019      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1502
02020      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1502
02021      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1502
02022      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).      EL1502
02023      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).      EL1502
02024                                                                   EL1502
02025      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1502
02026                                                                   EL1502
02027      EJECT                                                        EL1502
02028  2600-DENIAL-TRAILER.                                             EL1502
02029      MOVE SPACES                 TO  DENIAL-HDG1.                 EL1502
02030      MOVE PI-LINE-NO             TO  DENIAL-LINE-NO.              EL1502
02031      MOVE 'DENIAL'               TO  DENIAL-HDG1-LIT.             EL1502
02032                                                                   EL1502
02033      MOVE SPACES                 TO  DENIAL-TEXT-1.               EL1502
02034      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.        EL1502
02035      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.          EL1502
02036      MOVE WS-FORMAT-CERT-NO      TO  DENIAL-CERT-NO.              EL1502
02037                                                                   EL1502
02038      MOVE ' LN 1: '              TO  DENIAL-LN1-LIT.              EL1502
02039      MOVE AT-DENIAL-INFO-1       TO  DENIAL-LN1.                  EL1502
02040      MOVE ' BY: '                TO  DENIAL-REC-BY-LIT.           EL1502
02041      MOVE AT-RECORDED-BY         TO  DENIAL-REC-BY.               EL1502
02042                                                                   EL1502
02043      MOVE SPACES                 TO  DENIAL-HDG2                  EL1502
02044                                      DENIAL-TEXT-2.               EL1502
02045      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.               EL1502
02046      MOVE ' '                    TO  DC-OPTION-CODE.              EL1502
02047      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1502
02048      IF NO-CONVERSION-ERROR                                       EL1502
02049          MOVE DC-GREG-DATE-1-EDIT TO DENIAL-REC-DT                EL1502
02050      ELSE                                                         EL1502
02051          MOVE SPACES             TO  DENIAL-REC-DT.               EL1502
02052                                                                   EL1502
02053      MOVE 'LN 2: '               TO  DENIAL-LN2-LIT.              EL1502
02054      MOVE AT-DENIAL-INFO-2       TO  DENIAL-LN2.                  EL1502
02055                                                                   EL1502
02056      MOVE DENIAL-HDG1            TO  MAP-HDG        (DISPLAY-CNT).EL1502
02057      MOVE DENIAL-TEXT-1          TO  MAP-TEXT       (DISPLAY-CNT).EL1502
02058      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1502
02059      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
02060      ADD +1 TO DISPLAY-CNT.                                          CL*23
02061      MOVE DENIAL-HDG2            TO  MAP-HDG        (DISPLAY-CNT).EL1502
02062      MOVE DENIAL-TEXT-2          TO  MAP-TEXT       (DISPLAY-CNT).EL1502
02063      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1502
02064                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
02065      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1502
02066      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1502
02067      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1502
02068      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).      EL1502
02069      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).      EL1502
02070                                                                   EL1502
02071      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1502
02072                                                                   EL1502
02073      EJECT                                                        EL1502
02074  2700-INCURRED-CHANGE-TRAILER.                                    EL1502
02075      MOVE SPACES                 TO  INCUR-CHG-HDG1.              EL1502
02076      MOVE PI-LINE-NO             TO  INCUR-LINE-NO.               EL1502
02077      MOVE 'INCUR CG'             TO  INCUR-HDG1-LIT.              EL1502
02078                                                                   EL1502
02079      MOVE SPACES                 TO  INCUR-TEXT-1.                EL1502
02080      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.        EL1502
02081      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.          EL1502
02082      MOVE WS-FORMAT-CERT-NO      TO  INCUR-CERT-NO.               EL1502
02083                                                                   EL1502
02084      IF AT-OLD-INCURRED-DT = LOW-VALUES OR SPACES                    CL*17
02085          MOVE SPACES                     TO  INCUR-INCUR-DT       EL1502
02086                                              INCUR-INCUR-LIT      EL1502
02087      ELSE                                                         EL1502
02088          MOVE ' INCR: '                  TO  INCUR-INCUR-LIT      EL1502
02089          MOVE AT-OLD-INCURRED-DT         TO  DC-BIN-DATE-1        EL1502
02090          MOVE ' '                        TO  DC-OPTION-CODE       EL1502
02091          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
02092          IF NO-CONVERSION-ERROR                                   EL1502
02093              MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-INCUR-DT       EL1502
02094          ELSE                                                     EL1502
02095              MOVE SPACES                 TO  INCUR-INCUR-DT.      EL1502
02096                                                                   EL1502
02097      IF AT-OLD-REPORTED-DT = LOW-VALUES OR SPACES                    CL*17
02098          MOVE SPACES                     TO  INCUR-REPORT-DT      EL1502
02099                                              INCUR-REPORT-LIT     EL1502
02100      ELSE                                                         EL1502
02101          MOVE ' REPORT : '               TO  INCUR-REPORT-LIT     EL1502
02102          MOVE AT-OLD-REPORTED-DT         TO  DC-BIN-DATE-1        EL1502
02103          MOVE ' '                        TO  DC-OPTION-CODE       EL1502
02104          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
02105          IF NO-CONVERSION-ERROR                                   EL1502
02106              MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-REPORT-DT      EL1502
02107          ELSE                                                     EL1502
02108              MOVE SPACES                 TO  INCUR-REPORT-DT.     EL1502
02109                                                                   EL1502
02110      IF AT-OLD-ESTABLISHED-DT = LOW-VALUES OR SPACES                 CL*17
02111          MOVE SPACES                     TO  INCUR-ESTAB-DT       EL1502
02112                                              INCUR-ESTAB-LIT      EL1502
02113      ELSE                                                         EL1502
02114          MOVE ' EST: '                   TO  INCUR-ESTAB-LIT      EL1502
02115          MOVE AT-OLD-ESTABLISHED-DT      TO  DC-BIN-DATE-1        EL1502
02116          MOVE ' '                        TO  DC-OPTION-CODE       EL1502
02117          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
02118          IF NO-CONVERSION-ERROR                                   EL1502
02119              MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-ESTAB-DT       EL1502
02120          ELSE                                                     EL1502
02121              MOVE SPACES                 TO  INCUR-ESTAB-DT.      EL1502
02122                                                                   EL1502
02123      MOVE ' BY: '                        TO  INCUR-REC-BY-LIT.    EL1502
02124      MOVE AT-RECORDED-BY                 TO  INCUR-REC-BY.        EL1502
02125                                                                   EL1502
02126      MOVE SPACES                         TO  INCUR-CHG-HDG2       EL1502
02127                                              INCUR-TEXT-2.        EL1502
02128      MOVE AT-RECORDED-DT                 TO  DC-BIN-DATE-1.       EL1502
02129      MOVE ' '                            TO  DC-OPTION-CODE.      EL1502
02130      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1502
02131      IF NO-CONVERSION-ERROR                                       EL1502
02132          MOVE DC-GREG-DATE-1-EDIT        TO  INCUR-REC-DT         EL1502
02133      ELSE                                                         EL1502
02134          MOVE SPACES                     TO  INCUR-REC-DT.        EL1502
02135                                                                   EL1502
02136      IF PI-USES-PAID-TO                                           EL1502
02137          MOVE '  TO: '                   TO  INCUR-PD-THRU-LIT    EL1502
02138      ELSE                                                         EL1502
02139          MOVE 'THRU: '                   TO  INCUR-PD-THRU-LIT.   EL1502
02140                                                                   EL1502
02141      IF AT-OLD-PAID-THRU-DT = LOW-VALUES OR SPACES                   CL*17
02142          MOVE SPACES                     TO  INCUR-PD-THRU-DT     EL1502
02143      ELSE                                                         EL1502
02144          MOVE AT-OLD-PAID-THRU-DT        TO  DC-BIN-DATE-1        EL1502
02145          MOVE ' '                        TO  DC-OPTION-CODE       EL1502
02146          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
02147          IF NO-CONVERSION-ERROR                                   EL1502
02148              MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-PD-THRU-DT     EL1502
02149          ELSE                                                     EL1502
02150              MOVE SPACES                 TO  INCUR-PD-THRU-DT.    EL1502
02151                                                                   EL1502
02152      IF AT-OLD-TOTAL-PAID = ZEROS                                    CL*17
02153          MOVE ZEROS              TO  INCUR-TOT-PD                 EL1502
02154          MOVE SPACES             TO  INCUR-TOT-PD-LIT             EL1502
02155      ELSE                                                         EL1502
02156          MOVE AT-OLD-TOTAL-PAID  TO  INCUR-TOT-PD                 EL1502
02157          MOVE ' TOT PD : '       TO  INCUR-TOT-PD-LIT.            EL1502
02158                                                                   EL1502
02159      IF AT-OLD-DAYS-PAID = ZEROS                                     CL*17
02160          MOVE ZEROS              TO  INCUR-TOT-DAYS-PD            EL1502
02161          MOVE SPACES             TO  INCUR-TOT-DAYS-LIT           EL1502
02162      ELSE                                                         EL1502
02163          MOVE AT-OLD-DAYS-PAID   TO  INCUR-TOT-DAYS-PD            EL1502
02164          MOVE ' DAY: '           TO  INCUR-TOT-DAYS-LIT.          EL1502
02165                                                                   EL1502
02166      IF AT-OLD-NO-OF-PMTS = ZEROS                                    CL*17
02167          MOVE ZEROS              TO  INCUR-NO-PMTS                EL1502
02168          MOVE SPACES             TO  INCUR-NO-PMTS-LIT            EL1502
02169      ELSE                                                         EL1502
02170          MOVE ' NO PMTS : '      TO  INCUR-NO-PMTS-LIT            EL1502
02171          MOVE AT-OLD-NO-OF-PMTS  TO  INCUR-NO-PMTS.               EL1502
02172                                                                   EL1502
02173      MOVE INCUR-CHG-HDG1         TO  MAP-HDG        (DISPLAY-CNT).EL1502
02174      MOVE INCUR-TEXT-1           TO  MAP-TEXT       (DISPLAY-CNT).EL1502
02175      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1502
02176      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
02177      ADD +1 TO DISPLAY-CNT.                                          CL*23
02178      MOVE INCUR-CHG-HDG2         TO  MAP-HDG        (DISPLAY-CNT).EL1502
02179      MOVE INCUR-TEXT-2           TO  MAP-TEXT       (DISPLAY-CNT).EL1502
02180      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1502
02181                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
02182      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1502
02183      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1502
02184      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1502
02185      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).      EL1502
02186      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).      EL1502
02187                                                                   EL1502
02188      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1502
02189                                                                   EL1502
02190      EJECT                                                        EL1502
02191  2710-FORM-CONTROL-TRAILER.                                       EL1502
02192      MOVE SPACES                 TO  FORM-HDG1.                   EL1502
02193      MOVE PI-LINE-NO             TO  FORM-LINE-NO.                EL1502
02194      MOVE 'FORM    '             TO  FORM-HDG1-LIT.               EL1502
02195                                                                   EL1502
02196      MOVE SPACES                 TO  FORM-TEXT-1.                 EL1502
02197      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.        EL1502
02198      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.          EL1502
02199      MOVE WS-FORMAT-CERT-NO      TO  FORM-CERT-NO.                EL1502
02200                                                                   EL1502
02201      MOVE ' TYPE: '              TO  FORM-TYPE-LIT.               EL1502
02202                                                                      CL*23
02203      IF INITIAL-FORM                                              EL1502
02204         MOVE 'INITIAL'           TO  FORM-TYPE                    EL1502
02205      ELSE                                                         EL1502
02206         MOVE 'PROGRESS'          TO  FORM-TYPE.                   EL1502
02207                                                                   EL1502
02208      IF AT-FORM-PRINTED-DT = LOW-VALUES OR SPACES                    CL*17
02209          MOVE ' SEND ON: '                   TO  FORM-SEND-ON-LIT EL1502
02210          IF AT-FORM-SEND-ON-DT = LOW-VALUES OR SPACES                CL*17
02211              MOVE SPACES                     TO  FORM-SEND-ON-DT  EL1502
02212          ELSE                                                     EL1502
02213              MOVE AT-FORM-SEND-ON-DT         TO  DC-BIN-DATE-1    EL1502
02214              MOVE ' '                        TO  DC-OPTION-CODE   EL1502
02215              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1502
02216              IF NO-CONVERSION-ERROR                               EL1502
02217                  MOVE DC-GREG-DATE-1-EDIT    TO  FORM-SEND-ON-DT  EL1502
02218              ELSE                                                 EL1502
02219                  MOVE SPACES                 TO  FORM-SEND-ON-DT  EL1502
02220      ELSE                                                         EL1502
02221          MOVE ' SENT ON: '                   TO  FORM-SEND-ON-LIT EL1502
02222          MOVE AT-FORM-PRINTED-DT             TO  DC-BIN-DATE-1    EL1502
02223          MOVE ' '                            TO  DC-OPTION-CODE   EL1502
02224          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
02225          IF NO-CONVERSION-ERROR                                   EL1502
02226              MOVE DC-GREG-DATE-1-EDIT        TO  FORM-SEND-ON-DT  EL1502
02227          ELSE                                                     EL1502
02228              MOVE SPACES                     TO  FORM-SEND-ON-DT. EL1502
02229                                                                   EL1502
02230      IF AT-FORM-REPRINT-DT = LOW-VALUES OR SPACES                    CL*17
02231          IF AT-FORM-RE-SEND-DT = LOW-VALUES OR SPACES                CL*17
02232              MOVE SPACES                     TO  FORM-RESEND-DT   EL1502
02233                                                  FORM-RESEND-LIT  EL1502
02234          ELSE                                                     EL1502
02235              MOVE ' RES: '                   TO  FORM-RESEND-LIT  EL1502
02236              MOVE AT-FORM-RE-SEND-DT         TO  DC-BIN-DATE-1    EL1502
02237              MOVE ' '                        TO  DC-OPTION-CODE   EL1502
02238              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1502
02239              IF NO-CONVERSION-ERROR                               EL1502
02240                  MOVE DC-GREG-DATE-1-EDIT    TO  FORM-RESEND-DT   EL1502
02241              ELSE                                                 EL1502
02242                  MOVE SPACES                 TO  FORM-RESEND-DT   EL1502
02243      ELSE                                                         EL1502
02244          MOVE ' RES: '                       TO  FORM-RESEND-LIT  EL1502
02245          MOVE AT-FORM-REPRINT-DT             TO  DC-BIN-DATE-1    EL1502
02246          MOVE ' '                            TO  DC-OPTION-CODE   EL1502
02247          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
02248          IF NO-CONVERSION-ERROR                                   EL1502
02249              MOVE DC-GREG-DATE-1-EDIT        TO  FORM-RESEND-DT   EL1502
02250          ELSE                                                     EL1502
02251              MOVE SPACES                     TO  FORM-RESEND-DT.  EL1502
02252                                                                   EL1502
02253      MOVE ' BY: '                        TO  FORM-REC-BY-LIT.     EL1502
02254      MOVE AT-RECORDED-BY                 TO  FORM-REC-BY.         EL1502
02255                                                                   EL1502
02256      MOVE SPACES                      TO  FORM-HDG2               EL1502
02257                                           FORM-TEXT-2.            EL1502
02258      MOVE AT-RECORDED-DT              TO  DC-BIN-DATE-1.          EL1502
02259      MOVE ' '                         TO  DC-OPTION-CODE.         EL1502
02260      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1502
02261      IF NO-CONVERSION-ERROR                                       EL1502
02262          MOVE DC-GREG-DATE-1-EDIT     TO  FORM-REC-DT             EL1502
02263      ELSE                                                         EL1502
02264          MOVE SPACES                  TO  FORM-REC-DT.            EL1502
02265                                                                   EL1502
02266      IF AT-FORM-ANSWERED-DT = LOW-VALUES OR SPACES                   CL*23
02267          MOVE SPACES                  TO  FORM-REC-INS-DT         EL1502
02268                                           FORM-REC-INS-LIT        EL1502
02269      ELSE                                                         EL1502
02270          MOVE 'INS : '                TO  FORM-REC-INS-LIT        EL1502
02271          MOVE AT-FORM-ANSWERED-DT     TO  DC-BIN-DATE-1           EL1502
02272          MOVE ' '                     TO  DC-OPTION-CODE          EL1502
02273          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
02274          IF NO-CONVERSION-ERROR                                   EL1502
02275              MOVE DC-GREG-DATE-1-EDIT TO  FORM-REC-INS-DT         EL1502
02276          ELSE                                                     EL1502
02277              MOVE SPACES              TO  FORM-REC-INS-DT.        EL1502
02278                                                                   EL1502
02279      IF AT-PHY-FORM-ANSWERED-DT = LOW-VALUES OR SPACES               CL*17
02280          MOVE SPACES                   TO  FORM-REC-PHY-DT        EL1502
02281                                            FORM-REC-PHY-LIT       EL1502
02282      ELSE                                                         EL1502
02283          MOVE ' PHYS   : '             TO  FORM-REC-PHY-LIT       EL1502
02284          MOVE AT-PHY-FORM-ANSWERED-DT  TO  DC-BIN-DATE-1          EL1502
02285          MOVE ' '                      TO  DC-OPTION-CODE         EL1502
02286          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
02287          IF NO-CONVERSION-ERROR                                   EL1502
02288              MOVE DC-GREG-DATE-1-EDIT  TO  FORM-REC-PHY-DT        EL1502
02289          ELSE                                                     EL1502
02290              MOVE SPACES               TO  FORM-REC-PHY-DT.       EL1502
02291                                                                   EL1502
02292      IF AT-EMP-FORM-ANSWERED-DT = LOW-VALUES OR SPACES               CL*17
02293          MOVE SPACES                   TO  FORM-REC-EMP-DT        EL1502
02294                                            FORM-REC-EMP-LIT       EL1502
02295      ELSE                                                         EL1502
02296          MOVE ' EMP: '                 TO  FORM-REC-EMP-LIT       EL1502
02297          MOVE AT-EMP-FORM-ANSWERED-DT  TO  DC-BIN-DATE-1          EL1502
02298          MOVE ' '                      TO  DC-OPTION-CODE         EL1502
02299          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1502
02300          IF NO-CONVERSION-ERROR                                   EL1502
02301              MOVE DC-GREG-DATE-1-EDIT  TO  FORM-REC-EMP-DT        EL1502
02302          ELSE                                                     EL1502
02303              MOVE SPACES               TO  FORM-REC-EMP-DT.       EL1502
02304                                                                   EL1502
02305      MOVE FORM-HDG1              TO  MAP-HDG        (DISPLAY-CNT).EL1502
02306      MOVE FORM-TEXT-1            TO  MAP-TEXT       (DISPLAY-CNT).EL1502
02307      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1502
02308      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
02309      ADD +1 TO DISPLAY-CNT.                                          CL*23
02310      MOVE FORM-HDG2              TO  MAP-HDG        (DISPLAY-CNT).EL1502
02311      MOVE FORM-TEXT-2            TO  MAP-TEXT       (DISPLAY-CNT).EL1502
02312      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1502
02313                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1502
02314      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1502
02315      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1502
02316      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1502
02317      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).      EL1502
02318      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).      EL1502
02319                                                                   EL1502
02320      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1502
02321                                                                   EL1502
02322  2800-INCR-DISPLAY-CNT.                                           EL1502
02323                                                                   EL1502
02324      IF DIRECTION-SWITCH = 'F'                                       CL*17
02325          ADD +1 TO DISPLAY-CNT                                       CL*23
02326                    SUB-2                                             CL*23
02327                    PI-LINE-NO                                        CL*23
02328                    SUB-1                                             CL*23
02329          IF DISPLAY-CNT > +16                                        CL*23
02330              GO TO 2999-EXIT                                      EL1502
02331          ELSE                                                     EL1502
02332              NEXT SENTENCE                                        EL1502
02333      ELSE                                                         EL1502
02334          SUBTRACT +3 FROM DISPLAY-CNT                             EL1502
02335          SUBTRACT +1 FROM PI-LINE-NO                              EL1502
02336                           SUB-1                                   EL1502
02337                           SUB-2                                   EL1502
02338          IF DISPLAY-CNT < +1                                         CL*23
02339              GO TO 2999-EXIT.                                     EL1502
02340                                                                   EL1502
02341      GO TO 2000-BUILD-TRAILER-DISPLAY.                            EL1502
02342                                                                   EL1502
02343  2950-NO-MORE-TRAILERS.                                           EL1502
02344      MOVE ER-0303                TO  EMI-ERROR.                   EL1502
02345      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1502
02346                                                                   EL1502
02347  2999-EXIT.                                                       EL1502
02348      EXIT.                                                        EL1502
02349                                                                   EL1502
02350      EJECT                                                        EL1502
02351  3000-RECEIVE-LETTERS.                                            EL1502
02352                                                                   EL1502
02353      MOVE LINENOI                TO  SUB-1.                       EL1502
02354                                                                   EL1502
02355      IF PI-TRLR-TYPE (SUB-1) = '4'                                   CL*17
02356          NEXT SENTENCE                                            EL1502
02357      ELSE                                                         EL1502
02358      IF PI-TRLR-TYPE (SUB-1) = '2'                                   CL*23
02359          MOVE ER-0667            TO  EMI-ERROR                       CL*23
02360          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*23
02361          MOVE -1                 TO  LINENOL                         CL*23
02362          GO TO 8200-SEND-DATAONLY                                    CL*23
02363      ELSE                                                            CL*23
02364      IF PI-TRLR-TYPE (SUB-1) = 'A'                                   CL*23
02365          MOVE ER-0665            TO  EMI-ERROR                       CL*23
02366          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*23
02367          MOVE -1                 TO  LINENOL                         CL*23
02368          GO TO 8200-SEND-DATAONLY                                    CL*23
02369      ELSE                                                            CL*23
02370          MOVE ER-0660            TO  EMI-ERROR                       CL*23
02371          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*23
02372          MOVE -1                 TO  LINENOL                         CL*23
02373          GO TO 8200-SEND-DATAONLY.                                   CL*23
02374                                                                   EL1502
02375      IF RECVDTI = SPACES                                             CL*17
02376          MOVE LOW-VALUES                   TO  WS-RECEIVED-DATE   EL1502
02377      ELSE                                                         EL1502
02378          MOVE RECVDTI                      TO  WS-DEEDIT-FIELD    EL1502
02379          PERFORM 9800-DEEDIT THRU 9800-EXIT                       EL1502
02380          IF WS-DEEDIT-FIELD-V0 NUMERIC                               CL*23
02381              MOVE '4'                      TO  DC-OPTION-CODE     EL1502
02382              MOVE WS-DEEDIT-FIELD-V0       TO  DC-GREG-DATE-1-MDY EL1502
02383              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1502
02384              IF NO-CONVERSION-ERROR                               EL1502
02385                  MOVE DC-BIN-DATE-1        TO  WS-RECEIVED-DATE   EL1502
02386                  MOVE DC-GREG-DATE-1-EDIT  TO  RECVDTO            EL1502
02387                  MOVE AL-UANON             TO  RECVDTA            EL1502
02388              ELSE                                                 EL1502
02389                  MOVE LOW-VALUES           TO  WS-RECEIVED-DATE.  EL1502
02390                                                                   EL1502
02391      MOVE PI-TRLR-SEQ-NO (SUB-1) TO  TRLR-SEQ-NO.                 EL1502
02392      MOVE PI-TRLR-CERT   (SUB-1) TO  TRLR-CERT-NO.                EL1502
02393      PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.                EL1502
02394                                                                   EL1502
02395      MOVE WS-RECEIVED-DATE       TO  AT-LETTER-ANSWERED-DT.       EL1502
02396      MOVE PI-PROCESSOR-ID        TO  AT-CORR-LAST-UPDATED-BY.     EL1502
02397      MOVE SAVE-BIN-DATE          TO  AT-CORR-LAST-MAINT-DT.       EL1502
02398                                                                   EL1502
02399      PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.                    EL1502
02400                                                                   EL1502
02401      MOVE PI-TRLR-CERT   (SUB-1) TO  MSTR-CERT-NO.                EL1502
02402      PERFORM 7500-READ-CLAIM-MSTR-UPDATE THRU 7500-EXIT.          EL1502
02403                                                                      CL*14
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*21
121802*        MOVE 11                 TO CL-ACTIVITY-CODE                 CL*14
121802*        MOVE SAVE-BIN-DATE      TO CL-ACTIVITY-MAINT-DT             CL*14
121802*        MOVE 'CORR'             TO CL-ACTIVITY-MAINT-TYPE           CL*14
121802*        MOVE PI-PROCESSOR-ID    TO CL-PROCESSOR-ID.                 CL*14
02409                                                                      CL*14
02410      PERFORM 7600-REWRITE-CLAIM-MSTR THRU 7600-EXIT.              EL1502
02411                                                                   EL1502
02412      MOVE ER-0000                TO  EMI-ERROR.                   EL1502
02413      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1502
02414      MOVE -1                     TO  LINENOL.                     EL1502
02415      MOVE AL-UANOF               TO  LINENOA  RECVDTA             EL1502
02416                                      RECVTYPA.                    EL1502
02417      MOVE LOW-VALUES             TO  EL150CO.                     EL1502
02418      MOVE 'F'                    TO  DIRECTION-SWITCH.            EL1502
02419      MOVE +1                     TO  DISPLAY-CNT                  EL1502
02420                                      SUB-1                        EL1502
02421                                      PI-LINE-NO.                  EL1502
02422      MOVE PI-TRLR-SUB (SUB-1)    TO  SUB-2.                       EL1502
02423      PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT.            EL1502
02424      GO TO 1010-BUILD-HISTORY-SCREEN.                             EL1502
02425                                                                   EL1502
02426      EJECT                                                        EL1502
02427  4000-RECEIVE-FORMS.                                              EL1502
02428                                                                   EL1502
02429      MOVE LINENOI                TO  SUB-1.                       EL1502
02430                                                                   EL1502
02431      IF PI-TRLR-TYPE (SUB-1) = 'A'                                   CL*17
02432          NEXT SENTENCE                                            EL1502
02433      ELSE                                                         EL1502
02434      IF PI-TRLR-TYPE (SUB-1) = '4'                                   CL*23
02435          MOVE ER-0666            TO  EMI-ERROR                       CL*23
02436          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*23
02437          MOVE -1                 TO  LINENOL                         CL*23
02438          GO TO 8200-SEND-DATAONLY                                    CL*23
02439      ELSE                                                            CL*23
02440      IF PI-TRLR-TYPE (SUB-1) = '2'                                   CL*23
02441          MOVE ER-0667            TO  EMI-ERROR                       CL*23
02442          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*23
02443          MOVE -1                 TO  LINENOL                         CL*23
02444          GO TO 8200-SEND-DATAONLY                                    CL*23
02445      ELSE                                                            CL*23
02446          MOVE ER-0661            TO  EMI-ERROR                       CL*23
02447          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*23
02448          MOVE -1                 TO  LINENOL                         CL*23
02449          GO TO 8200-SEND-DATAONLY.                                   CL*23
02450                                                                   EL1502
02451      IF RECVTYPI = 'I' OR 'P' OR 'E'                                 CL*17
02452          NEXT SENTENCE                                            EL1502
02453      ELSE                                                         EL1502
02454          MOVE ER-0662            TO  EMI-ERROR                    EL1502
02455          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1502
02456          MOVE -1                 TO  RECVTYPL                     EL1502
02457          GO TO 8200-SEND-DATAONLY.                                EL1502
02458                                                                   EL1502
02459      IF RECVDTI = SPACES                                             CL*17
02460          MOVE LOW-VALUES                   TO  WS-RECEIVED-DATE   EL1502
02461      ELSE                                                         EL1502
02462          MOVE RECVDTI                      TO  WS-DEEDIT-FIELD    EL1502
02463          PERFORM 9800-DEEDIT THRU 9800-EXIT                       EL1502
02464          IF WS-DEEDIT-FIELD-V0 IS NUMERIC                         EL1502
02465              MOVE '4'                      TO  DC-OPTION-CODE     EL1502
02466              MOVE WS-DEEDIT-FIELD-V0       TO  DC-GREG-DATE-1-MDY EL1502
02467              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1502
02468              IF NO-CONVERSION-ERROR                               EL1502
02469                  MOVE DC-BIN-DATE-1        TO  WS-RECEIVED-DATE   EL1502
02470                  MOVE DC-GREG-DATE-1-EDIT  TO  RECVDTO            EL1502
02471                  MOVE AL-UANON             TO  RECVDTA            EL1502
02472              ELSE                                                 EL1502
02473                  MOVE LOW-VALUES           TO  WS-RECEIVED-DATE.  EL1502
02474                                                                   EL1502
02475      MOVE PI-TRLR-CERT   (SUB-1)     TO  TRLR-CERT-NO.            EL1502
02476      MOVE PI-TRLR-SEQ-NO (SUB-1)     TO  TRLR-SEQ-NO.             EL1502
02477      PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.                EL1502
02478                                                                   EL1502
02479      IF RECVTYPI = 'I'                                               CL*17
02480          MOVE WS-RECEIVED-DATE       TO  AT-FORM-ANSWERED-DT      EL1502
02481      ELSE                                                         EL1502
02482      IF RECVTYPI = 'P'                                               CL*23
02483          MOVE WS-RECEIVED-DATE       TO  AT-PHY-FORM-ANSWERED-DT     CL*23
02484      ELSE                                                            CL*23
02485          MOVE WS-RECEIVED-DATE       TO  AT-EMP-FORM-ANSWERED-DT.    CL*23
02486                                                                   EL1502
02487      MOVE PI-PROCESSOR-ID            TO  AT-FORM-LAST-UPDATED-BY. EL1502
02488      MOVE SAVE-BIN-DATE              TO  AT-FORM-LAST-MAINT-DT.   EL1502
02489                                                                   EL1502
02490      PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.                    EL1502
02491                                                                   EL1502
02492      MOVE ER-0000                TO  EMI-ERROR.                   EL1502
02493      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1502
02494      MOVE -1                     TO  LINENOL.                     EL1502
02495      MOVE AL-UANOF               TO  LINENOA  RECVDTA             EL1502
02496                                      RECVTYPA.                    EL1502
02497      MOVE LOW-VALUES             TO  EL150CO.                     EL1502
02498      MOVE 'F'                    TO  DIRECTION-SWITCH             EL1502
02499      MOVE +1                     TO  DISPLAY-CNT                  EL1502
02500                                      SUB-1                        EL1502
02501                                      PI-LINE-NO.                  EL1502
02502      MOVE PI-TRLR-SUB (SUB-1)    TO  SUB-2.                       EL1502
02503      PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT.            EL1502
02504                                                                      CL*23
02505      GO TO 1010-BUILD-HISTORY-SCREEN.                             EL1502
02506                                                                   EL1502
02507      EJECT                                                        EL1502
02508                                                                   EL1502
02509  5000-VOID-PAYMENT.                                               EL1502
02510                                                                   EL1502
02511      MOVE LINENOI                TO  SUB-1.                       EL1502
02512                                                                   EL1502
02513      IF PI-TRLR-TYPE (SUB-1) = '2'                                   CL*17
02514          NEXT SENTENCE                                            EL1502
02515      ELSE                                                         EL1502
02516      IF PI-TRLR-TYPE (SUB-1) = '4'                                   CL*23
02517          MOVE ER-0666            TO  EMI-ERROR                       CL*23
02518          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*23
02519          MOVE -1                 TO  LINENOL                         CL*23
02520          GO TO 8200-SEND-DATAONLY                                    CL*23
02521      ELSE                                                            CL*23
02522      IF PI-TRLR-TYPE (SUB-1) = 'A'                                   CL*23
02523          MOVE ER-0665            TO  EMI-ERROR                       CL*23
02524          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*23
02525          MOVE -1                 TO  LINENOL                         CL*23
02526          GO TO 8200-SEND-DATAONLY                                    CL*23
02527      ELSE                                                            CL*23
02528          MOVE ER-0664            TO  EMI-ERROR                       CL*23
02529          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*23
02530          MOVE -1                 TO  LINENOL                         CL*23
02531          GO TO 8200-SEND-DATAONLY.                                   CL*23
02532                                                                   EL1502
02533      MOVE PI-TRLR-CERT (SUB-1)   TO  MSTR-CERT-NO.                EL1502
02534      PERFORM 7500-READ-CLAIM-MSTR-UPDATE THRU 7500-EXIT.          EL1502
02535                                                                   EL1502
02536      MOVE PI-TRLR-CERT   (SUB-1) TO  TRLR-CERT-NO.                EL1502
02537      MOVE PI-TRLR-SEQ-NO (SUB-1) TO  TRLR-SEQ-NO.                 EL1502
02538      PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.                EL1502
02539                                                                   EL1502
02540      IF AT-VOID-DT = LOW-VALUES OR SPACES                            CL*21
02541          NEXT SENTENCE                                               CL*12
02542      ELSE                                                            CL*12
02543          MOVE ER-0800            TO  EMI-ERROR                       CL*12
02544          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*12
02545          MOVE -1                 TO  LINENOL                         CL*12
02546          PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT               CL*12
02547          PERFORM 7010-UNLOCK-TRLR THRU 7010-EXIT                     CL*12
02548          GO TO 8200-SEND-DATAONLY.                                   CL*12
02549                                                                      CL*12
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*21
121802*        IF AT-CASH-PAYMENT = 'N'                                    CL*17
121802*            IF AT-CHECK-WRITTEN-DT = SPACES OR LOW-VALUES           CL*23
121802*                MOVE ER-0833    TO  EMI-ERROR                       CL*12
121802*                MOVE -1         TO  LINENOL                         CL*12
121802*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*12
121802*                PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT       CL*12
121802*                PERFORM 7010-UNLOCK-TRLR THRU 7010-EXIT             CL*12
121802*                GO TO 8200-SEND-DATAONLY.                           CL*12
121802*                                                                    CL*12
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*21
121802*      IF AT-CASH-PAYMENT = 'N'                                      CL*17
121802*        IF AT-RECORDED-DT = SAVE-BIN-DATE                           CL*17
121802*          NEXT SENTENCE                                             CL*12
121802*        ELSE                                                        CL*12
121802*          IF PI-PROCESSOR-USER-ALMIGHTY = 'Y'                       CL*17
121802*            NEXT SENTENCE                                           CL*12
121802*          ELSE                                                      CL*12
121802*            MOVE ER-0816        TO  EMI-ERROR                       CL*12
121802*            MOVE -1             TO  LINENOL                         CL*12
121802*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*12
121802*            PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT           CL*12
121802*            PERFORM 7010-UNLOCK-TRLR THRU 7010-EXIT                 CL*12
121802*            GO TO 8200-SEND-DATAONLY.                               CL*12
121802*                                                                    CL*12
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*21
121802*        IF CLAIM-IS-CLOSED                                          CL*21
121802*            IF SETUP-ERRORS                                         CL*21
121802*                MOVE ER-0941    TO EMI-ERROR                        CL*14
121802*                MOVE -1         TO LINENOL                          CL*14
121802*                PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT       CL*23
121802*                PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT       CL*23
121802*                PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT       CL*23
121802*                GO TO 8200-SEND-DATAONLY                            CL*14
121802*            ELSE                                                    CL*14
121802*            IF BENEFITS-CHANGED                                     CL*23
121802*                IF SYSTEM-MODIFY-CAP                                CL*23
121802*                    NEXT SENTENCE                                   CL*23
121802*                  ELSE                                              CL*23
121802*                    MOVE ER-0942  TO EMI-ERROR                      CL*23
121802*                    MOVE -1       TO LINENOL                        CL*23
121802*                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT   
121802*                    PERFORM 7510-UNLOCK-CLAIM-MSTR                  CL*23
121802*                       THRU 7510-EXIT                               CL*23
121802*                    PERFORM 7010-UNLOCK-TRLR THRU 7010-EXIT         CL*23
121802*                    GO TO 8200-SEND-DATAONLY.                       CL*23
121802*                                                                    CL*14
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*21
121802*        IF AT-CASH-PAYMENT = 'N'                                    CL*21
121802*            IF SYSTEM-MODIFY-CAP                                    CL*21
121802*                NEXT SENTENCE                                       CL*14
121802*            ELSE                                                    CL*14
121802*                IF AT-RECORDED-DT = SAVE-BIN-DATE                   CL*23
121802*                    NEXT SENTENCE                                   CL*14
121802*                ELSE                                                CL*14
121802*                    MOVE ER-0920  TO EMI-ERROR                      CL*21
121802*                    MOVE -1       TO LINENOL                        CL*21
121802*                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        CL*14
121802*                    PERFORM 7510-UNLOCK-CLAIM-MSTR                  CL*14
121802*                       THRU 7510-EXIT                               CL*23
121802*                    PERFORM 7010-UNLOCK-TRLR THRU 7010-EXIT         CL*14
121802*                    GO TO 8200-SEND-DATAONLY.                       CL*14
121802*                                                                    CL*14
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*17
121802*       IF AT-PAYMENT-TYPE NOT = '4' AND '5' AND '6'                 CL*23
121802*          MOVE 'O'              TO  CL-CLAIM-STATUS                 CL*23
121802*       END-IF                                                       CL*23
121802*       PERFORM 5300-CREATE-DMO THRU 5300-EXIT.                      CL*23
121802*                                                                    CL*14
02619      MOVE AT-CHECK-WRITTEN-DT    TO  WS-CHECK-WRITTEN-DT.         EL1502
02620      MOVE AT-PAYMENT-APPROVAL-SW TO  WS-PMT-APPROVAL-SW.          EL1502
02621      MOVE AT-AMOUNT-PAID         TO  WS-AMOUNT-PAID.              EL1502
02622      MOVE AT-PAYMENT-ORIGIN      TO  WS-PAYMENT-ORIGIN.           EL1502
02623      MOVE AT-CV-PMT-CODE         TO  WS-CV-PMT-CODE.                 CL*13
02624                                                                   EL1502
022106     IF AT-PAYMENT-TYPE NOT = '5' AND '6' AND 'I'
02626          SUBTRACT AT-AMOUNT-PAID    FROM CL-TOTAL-PAID-AMT           CL*21
02627          SUBTRACT AT-DAYS-IN-PERIOD FROM CL-NO-OF-DAYS-PAID       EL1502
02628          IF AT-PAYMENT-TYPE NOT = '4'                                CL*21
02629              SUBTRACT +1 FROM CL-NO-OF-PMTS-MADE                  EL1502
02630              IF AT-PAID-THRU-DT NOT = CL-PAID-THRU-DT OR             CL*17
02631                 AT-RECORDED-BY = 'ZZZZ'                              CL*17
02632                  NEXT SENTENCE                                    EL1502
02633              ELSE                                                 EL1502
02634                  MOVE AT-PREV-LAST-PMT-DT    TO  CL-LAST-PMT-DT   EL1502
02635                  MOVE AT-PREV-PAID-THRU-DT   TO  CL-PAID-THRU-DT  EL1502
02636                  MOVE AT-PREV-LAST-PMT-AMT   TO  CL-LAST-PMT-AMT. EL1502
02637                                                                   EL1502
02638      IF CL-NO-OF-DAYS-PAID < ZERO                                    CL*23
02639          MOVE +0                 TO  CL-NO-OF-DAYS-PAID.          EL1502
02640                                                                   EL1502
02641      IF CL-NO-OF-PMTS-MADE < ZERO                                    CL*23
02642          MOVE +0                 TO  CL-NO-OF-PMTS-MADE.          EL1502
02643                                                                   EL1502
02644      MOVE SAVE-BIN-DATE          TO  AT-VOID-DT                   EL1502
02645                                      CL-LAST-REOPEN-DT.           EL1502
02646                                                                   EL1502
02647      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL1502
02648      MOVE '1'                    TO  CNTL-REC-TYPE.               EL1502
02649      MOVE SPACES                 TO  CNTL-ACCESS.                 EL1502
02650      MOVE +0                     TO  CNTL-SEQ-NO.                 EL1502
02651      MOVE 'CNTL'                 TO  FILE-SWITCH.                 EL1502
02652                                                                   EL1502
02653      PERFORM 7900-READ-CONTROL-FILE THRU 7900-EXIT.               EL1502
02654                                                                   EL1502
02655      MOVE CF-PAYMENT-APPROVAL-SW TO  WS-CF-PAYMENT-APPROVAL-SW.      CL*12
02656                                                                      CL*11
02657      IF CF-PMT-APPROVAL-USED                                         CL**7
02658          MOVE 'V'                TO  AT-PAYMENT-APPROVAL-SW.      EL1502
02659                                                                   EL1502
02660 ******************************************************************   CL*12
02661 **  1.  BYPASS READING THE RECON RECORD FOR THE FOLLOWING       **   CL*12
02662 **      REASONS:                                                **   CL*12
02663 **      A.  NON-CASH PAYMENT                                    **   CL*12
02664 **      B.  CHECK HAS NOT BEEN PRINTED                          **   CL*12
02665 **      C.  USER IS NOT A RECON USER                            **   CL*12
02666 **      D.  PAYMENT IS A MANUAL (OFFLINE) PAYMENT               **   CL*13
02667 ******************************************************************   CL*12
02668                                                                      CL*12
02669      IF AT-CASH-PAYMENT = 'N'                                        CL*17
02670          GO TO 5005-CONT-VOID.                                       CL*12
02671                                                                      CL*12
02672      IF AT-CHECK-NO = SPACES OR LOW-VALUES                           CL*21
02673          GO TO 5005-CONT-VOID.                                       CL*12
02674                                                                      CL*12
02675      IF CF-CLAIMS-CHECK-RECON-USER NOT = 'Y'                         CL*17
02676          GO TO 5005-CONT-VOID.                                       CL*13
02677                                                                      CL*13
02678      IF OFFLINE-PMT                                                  CL*13
02679          GO TO 5005-CONT-VOID.                                       CL*12
02680                                                                      CL*12
02681 ******************************************************************   CL*12
02682 **  1.  RECON SW VALUES = :                                     **   CL*12
02683 **      A.  R = CHECK HAS BEEN REDEEMED - CANNOT BE VOIDED      **   CL*12
02684 **      B.  X = RECON RECORD NOT FOUND                          **   CL*12
02685 ******************************************************************   CL*12
02686                                                                      CL*12
02687      MOVE 'RCON'                     TO  FILE-SWITCH.                CL*12
02688      PERFORM 5700-UPDATE-RECON THRU 5700-EXIT.                       CL*12
02689      IF WS-RECON-SW = 'R'                                            CL*17
02690          PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT               CL*12
02691          PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT               CL*23
02692          GO TO 8200-SEND-DATAONLY.                                   CL*12
02693                                                                      CL*12
121802*    IF WS-RECON-SW = 'X'                                            CL*17
121802*        IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CIG' OR 'CUK'         CL*21
121802*            PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT           CL*12
121802*            PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT           CL*23
121802*            GO TO 8200-SEND-DATAONLY.                               CL*12
02699                                                                      CL*12
02700  5005-CONT-VOID.                                                     CL*12
02701      IF AT-RECORDED-BY = 'ZZZZ'                                      CL*17
02702          GO TO 5010-BYPASS.                                       EL1502
02703                                                                   EL1502
02704      MOVE '7'                    TO  PI-PAY-TYPE.                    CL**9
02705      MOVE AT-PAYMENT-TYPE        TO  WS-PAY-TYPE.                 EL1502
02706                                                                   EL1502
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*24
121802*        MOVE 88888888           TO WS-CK-Q-CONTROL                  CL*24
121802*     ELSE                                                           CL*24
02710          MOVE 99999999           TO WS-CK-Q-CONTROL.                 CL*24
02711                                                                      CL*24
02712      IF AT-CHECK-QUE-CONTROL > ZEROS AND < WS-CK-Q-CONTROL           CL*24
02713          PERFORM 5200-UPDATE-CHECK-QUE THRU 5299-EXIT                CL*23
02714      ELSE                                                         EL1502
02715          IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES AND SPACES          CL*23
02716              MOVE 'Y'                TO  WS-PRINTED-SW               CL**3
02717                                          WS-RELEASED-SW              CL**3
02718          ELSE                                                        CL**3
02719              MOVE 'N'                TO  WS-PRINTED-SW            EL1502
02720                                          WS-RELEASED-SW.          EL1502
02721                                                                   EL1502
02722  5010-BYPASS.                                                     EL1502
02723                                                                   EL1502
02724      IF PAYMENT-HAS-BEEN-PRINTED OR                                  CL**2
02725         OFFLINE-PMT                                                  CL**2
02726          MOVE CF-CURRENT-MONTH-END   TO  AT-VOID-SELECT-DT        EL1502
02727      ELSE                                                         EL1502
02728          MOVE LOW-VALUES             TO  AT-PMT-SELECT-DT         EL1502
02729                                          AT-VOID-SELECT-DT.       EL1502
02730                                                                   EL1502
02731      MOVE WS-VOID-CODE               TO  AT-VOID-TYPE.               CL*12
02732                                                                      CL*12
02733      MOVE PI-PROCESSOR-ID        TO  AT-PAYMENT-LAST-UPDATED-BY.     CL*13
02734      MOVE SAVE-BIN-DATE          TO  AT-PAYMENT-LAST-MAINT-DT.       CL*13
02735                                                                      CL*13
02736      PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.                       CL**8

071510     IF WS-PAY-TYPE NOT = 'I'
071510        PERFORM 5400-UPDATE-ZERO-TRAILER
071510                                 THRU 5400-EXIT
071510     END-IF

121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'                                  CL*17
121802*        PERFORM 5500-UPDATE-POLICY-MASTER THRU 5500-EXIT            CL*13
121802*    ELSE                                                            CL*13
02742          PERFORM 5600-UPDATE-CERT THRU 5600-EXIT.                    CL*13
02743                                                                   EL1502
02744      MOVE CL-CONTROL-PRIMARY     TO  ELACTQ-KEY.                  EL1502
02745                                                                      CL**9
071510     IF WS-PAY-TYPE = '4' OR '5' OR '6' OR 'I'
02747          GO TO 5020-CONTINUE-VOID.                                   CL*13
02748                                                                      CL*13
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'                                  CL*17
121802*        IF CL-NO-OF-PMTS-MADE > +0                                  CL*23
121802*            GO TO 5020-CONTINUE-VOID.                               CL*13
02752                                                                      CL*13
02753      MOVE 'O'                    TO  CL-CLAIM-STATUS.                CL*13
02754                                                                      CL*13
02755  5020-CONTINUE-VOID.                                                 CL*13
02756                                                                   EL1502
02757      PERFORM 7600-REWRITE-CLAIM-MSTR THRU 7600-EXIT.              EL1502
02758                                                                   EL1502
02759      IF WS-PAYMENT-ORIGIN = '3'                                      CL*17
02760          GO TO 5100-CONTINUE.                                     EL1502
02761                                                                   EL1502
02762      IF PAYMENT-HAS-BEEN-PRINTED AND                              EL1502
02763         NOT WS-CF-PMT-APPROVAL-USED                                  CL*11
02764          GO TO 5100-CONTINUE.                                     EL1502
02765                                                                   EL1502
02766      MOVE 'ACTQ'                 TO  FILE-SWITCH.                    CL*12
02767      PERFORM 7700-READ-ELACTQ THRU 7799-EXIT.                     EL1502
02768                                                                   EL1502
02769  5100-CONTINUE.                                                   EL1502
02770                                                                   EL1502
02771      MOVE ER-0000                TO  EMI-ERROR.                   EL1502
02772      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1502
02773      MOVE -1                     TO  LINENOL.                     EL1502
02774      MOVE AL-UANOF               TO  LINENOA  RECVDTA             EL1502
02775                                      RECVTYPA.                    EL1502
02776      MOVE LOW-VALUES             TO  EL150CO.                     EL1502
02777      MOVE 'F'                    TO  DIRECTION-SWITCH             EL1502
02778      MOVE +1                     TO  DISPLAY-CNT                  EL1502
02779                                      SUB-1                        EL1502
02780                                      PI-LINE-NO.                  EL1502
02781      MOVE PI-TRLR-SUB (SUB-1)    TO  SUB-2.                       EL1502
02782      PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT.            EL1502
02783                                                                      CL*23
02784      GO TO 1010-BUILD-HISTORY-SCREEN.                             EL1502
02785                                                                   EL1502
02786  EJECT                                                            EL1502
02787  5200-UPDATE-CHECK-QUE.                                           EL1502
02788                                                                   EL1502
02789      MOVE PI-COMPANY-CD          TO  CHKQ-COMP-CD.                EL1502
02790      MOVE AT-CHECK-QUE-CONTROL   TO  CHKQ-CONTROL.                EL1502
02791      MOVE AT-CHECK-QUE-SEQUENCE  TO  CHKQ-SEQ-NO.                 EL1502
02792                                                                   EL1502
02793      EXEC CICS HANDLE CONDITION                                   EL1502
02794          NOTFND   (5290-NOTFND)                                   EL1502
02795      END-EXEC.                                                    EL1502
02796                                                                   EL1502
02797      EXEC CICS READ                                               EL1502
02798          DATASET   ('ELCHKQ')                                     EL1502
02799          RIDFLD    (ELCHKQ-KEY)                                   EL1502
02800          SET       (ADDRESS OF CHECK-QUE)                            CL*14
02801          UPDATE                                                   EL1502
02802      END-EXEC.                                                    EL1502
02803                                                                   EL1502
02804      MOVE 'Y'                    TO  WS-RELEASED-SW.              EL1502
02805                                                                   EL1502
02806      IF CQ-TIMES-PRINTED = +0                                        CL*17
02807          MOVE 'N'                TO  WS-PRINTED-SW                EL1502
02808          GO TO 5210-DELETE-CHECK-QUE                              EL1502
02809      ELSE                                                         EL1502
02810          MOVE 'Y'                TO  WS-PRINTED-SW.               EL1502
02811                                                                   EL1502
02812      MOVE WS-VOID-CODE           TO  CQ-VOID-INDICATOR.              CL*12
02813                                                                   EL1502
02814      EXEC CICS REWRITE                                            EL1502
02815          DATASET   ('ELCHKQ')                                     EL1502
02816          FROM      (CHECK-QUE)                                    EL1502
02817      END-EXEC.                                                    EL1502
02818                                                                   EL1502
02819      GO TO 5299-EXIT.                                             EL1502
02820                                                                   EL1502
02821  5210-DELETE-CHECK-QUE.                                           EL1502
02822                                                                   EL1502
02823      EXEC CICS DELETE                                             EL1502
02824          DATASET   ('ELCHKQ')                                     EL1502
02825      END-EXEC.                                                    EL1502
02826                                                                   EL1502
02827      MOVE +0                     TO  AT-CHECK-QUE-CONTROL         EL1502
02828                                      AT-CHECK-QUE-SEQUENCE.       EL1502
02829      GO TO 5299-EXIT.                                             EL1502
02830                                                                   EL1502
02831  5290-NOTFND.                                                     EL1502
02832      MOVE 'N'                    TO  WS-PRINTED-SW                EL1502
02833                                      WS-RELEASED-SW.              EL1502
02834  5299-EXIT.                                                       EL1502
02835      EXIT.                                                        EL1502
02836                                  EJECT                               CL*14
121802*5300-CREATE-DMO. Remove as obsolete or dead code                    CL*14
121802*5300-CONT. Remove as obsolete or dead code                          CL*15
121802*5300-NOTE-NOT-FOUND. Remove as obsolete or dead code                CL*14
121802*5300-EXIT. Remove as obsolete or dead code                          CL*14
121802*5350-FORMAT-LAST-NAME-1ST. Remove as obsolete or dead code          CL*14
121802*5350-EXIT. Remove as dead code                                      CL*14
121802*5360-MOVE-NAME. Remove as dead code                                 CL*14
121802*5360-MOVE-NAME-CYCLE. Remove as dead code                           CL*14
121802*5360-EXIT. Remove as dead code                                      CL*14
03261      EJECT                                                        EL1502
03262  5400-UPDATE-ZERO-TRAILER.                                        EL1502
03263                                                                   EL1502
03264      MOVE ELMSTR-KEY             TO  ELTRLR-KEY.                  EL1502
03265                                                                   EL1502
03266      MOVE ZEROS                  TO  TRLR-SEQ-NO.                 EL1502
03267      PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.                EL1502
03268                                                                   EL1502
03269      IF WS-PAY-TYPE = '5'                                            CL*17
03270          SUBTRACT WS-AMOUNT-PAID FROM AT-ITD-CHARGEABLE-EXPENSE.  EL1502
03271                                                                   EL1502
03272      IF WS-PAY-TYPE = '6'                                            CL*17
03273          SUBTRACT WS-AMOUNT-PAID FROM AT-ITD-PAID-EXPENSES.       EL1502
03274                                                                   EL1502
03275      IF AT-INITIAL-MANUAL-RESERVE NOT = ZEROS                        CL*17
03276          ADD WS-AMOUNT-PAID      TO  AT-CURRENT-MANUAL-RESERVE.   EL1502
03277                                                                   EL1502
03278  5410-CHECK-OPEN-CLOSE.                                           EL1502
03279                                                                   EL1502
03280      IF PI-PAY-TYPE = '5' OR '6'                                     CL*21
03281          PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT                 EL1502
03282          GO TO 5400-EXIT.                                         EL1502
03283                                                                   EL1502
03284      IF PI-PAY-TYPE = '1' OR '4' OR '7'                              CL*23
03285         IF CLAIM-IS-OPEN                                             CL*23
03286            PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT                  CL*21
03287            GO TO 5400-EXIT.                                          CL*21
03288                                                                      CL**9
03289      IF PI-PAY-TYPE = '2' OR '3'                                     CL*23
03290         IF CLAIM-IS-CLOSED                                           CL*23
03291            PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT                  CL*21
03292            GO TO 5400-EXIT.                                          CL*21
03293                                                                   EL1502
03294      MOVE 1                      TO  SUB.                         EL1502
03295                                                                   EL1502
03296  5420-LOOP.                                                       EL1502
03297                                                                   EL1502
03298      IF AT-OPEN-CLOSE-TYPE (SUB) = SPACES                            CL*17
03299          MOVE SAVE-BIN-DATE      TO  AT-OPEN-CLOSE-DATE (SUB)     EL1502
03300          MOVE 'O'                TO  AT-OPEN-CLOSE-TYPE (SUB)     EL1502
03301          MOVE 'FORCE'            TO  AT-OPEN-CLOSE-REASON (SUB)   EL1502
03302          PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT                 EL1502
03303          GO TO 5400-EXIT.                                         EL1502
03304                                                                   EL1502
03305      IF SUB = 6                                                      CL*17
03306       MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1) EL1502
03307       MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2) EL1502
03308       MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3) EL1502
03309       MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4) EL1502
03310       MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5) EL1502
03311       MOVE SPACES                    TO AT-OPEN-CLOSE-HISTORY (6) EL1502
03312       GO TO 5420-LOOP.                                            EL1502
03313                                                                   EL1502
03314      ADD 1 TO SUB.                                                   CL*23
03315      GO TO 5420-LOOP.                                             EL1502
03316                                                                   EL1502
03317  5400-EXIT.                                                       EL1502
03318      EXIT.                                                        EL1502
03319                                                                   EL1502
03320      EJECT                                                        EL1502
121802*5500-UPDATE-POLICY-MASTER. Remove as dead code                      CL*13
121802*5500-UPDATE-LF-POLICY-DATA. Remove as dead code                     CL*13
121802*5500-UPDATE-AH-POLICY-DATA. Remove as dead code                     CL*13
121802*5500-UPDATE-CLAIM-HISTORY. Remove as dead code                      CL*13
121802*5500-FINISH-POLICY-UPDATE. Remove as dead code                      CL*13
121802*5500-EXIT. Remove as dead code                                      CL*13

03468  5600-UPDATE-CERT.                                                EL1502
03469                                                                   EL1502
03470      MOVE PI-COMPANY-CD          TO  CERT-COMP-CD.                EL1502
03471      MOVE CL-CERT-CARRIER        TO  CERT-CARRIER.                EL1502
03472      MOVE CL-CERT-GROUPING       TO  CERT-GROUPING.               EL1502
03473      MOVE CL-CERT-STATE          TO  CERT-STATE.                  EL1502
03474      MOVE CL-CERT-ACCOUNT        TO  CERT-ACCOUNT.                EL1502
03475      MOVE CL-CERT-EFF-DT         TO  CERT-EFF-DT.                 EL1502
03476      MOVE CL-CERT-NO             TO  CERT-CERT-NO.                EL1502
03477      MOVE 'CERT'                 TO  FILE-SWITCH.                 EL1502
03478                                                                   EL1502
03479      PERFORM 7800-READ-CERT-UPDATE.                               EL1502
03480                                                                   EL1502
100518     IF CL-CLAIM-TYPE NOT = PI-LIFE-OVERRIDE-L1 AND 'O'              CL*17
03482          GO TO 5610-AH-VOID.                                      EL1502
03483                                                                   EL1502
03484      MOVE CM-LF-BENEFIT-CD       TO  WS-BEN-CD.                   EL1502
03485      MOVE WS-ACCESS              TO  CNTL-ACCESS.                 EL1502
03486      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL1502
03487      MOVE '4'                    TO  CNTL-REC-TYPE.               EL1502
03488      MOVE ZEROS                  TO  CNTL-SEQ-NO.                 EL1502
03489      MOVE 'BENE'                 TO  FILE-SWITCH.                 EL1502
03490      MOVE +0                     TO  SUB.                         EL1502
03491      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.                    EL1502
03492      IF NO-BENEFIT-FOUND                                          EL1502
03493          GO TO 8400-NOT-FOUND.                                    EL1502
03494                                                                      CL*21
03495      MOVE CF-LF-COVERAGE-TYPE (SUB)  TO  WS-LF-COVERAGE-TYPE.     EL1502
03496                                                                   EL1502
03497      IF PI-LIFE-OVERRIDE-L1 = 'P' OR                                 CL*17
03498         WS-LF-COVERAGE-TYPE = 'P'                                    CL*17
03499          IF WS-PAY-TYPE = '4'                                        CL*17
03500              SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT     EL1502
03501              IF CM-LF-CURRENT-STATUS = '1' OR '2'                    CL*17
03502                  PERFORM 7810-REWRITE-CERT THRU 7810-EXIT            CL*13
03503                  GO TO 5600-EXIT                                  EL1502
03504              ELSE                                                 EL1502
03505                  MOVE CM-LF-STATUS-AT-DEATH  TO                   EL1502
03506                                              CM-LF-CURRENT-STATUS EL1502
03507                  MOVE SPACES             TO  CM-LF-STATUS-AT-DEATH   CL*21
03508                  MOVE LOW-VALUES         TO  CM-LF-DEATH-EXIT-DT     CL*21
03509                                              CM-LF-DEATH-DT       EL1502
03510                  PERFORM 7810-REWRITE-CERT THRU 7810-EXIT            CL*13
03511                  GO TO 5600-EXIT.                                 EL1502
03512                                                                   EL1502
03513      IF WS-PAY-TYPE = '4'                                            CL*17
03514          SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT         EL1502
03515          PERFORM 7810-REWRITE-CERT THRU 7810-EXIT                    CL*13
03516          GO TO 5600-EXIT.                                         EL1502
03517                                                                   EL1502
03518      IF WS-PAY-TYPE = '2'                                            CL*17
03519          IF CM-LF-CURRENT-STATUS = '1' OR '2'                        CL*17
03520              SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT     EL1502
03521              PERFORM 7810-REWRITE-CERT THRU 7810-EXIT                CL*13
03522              GO TO 5600-EXIT                                      EL1502
03523          ELSE                                                     EL1502
03524              MOVE CM-LF-STATUS-AT-DEATH TO  CM-LF-CURRENT-STATUS  EL1502
03525              MOVE SPACES                TO  CM-LF-STATUS-AT-DEATH EL1502
03526              SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT     EL1502
03527              MOVE LOW-VALUES            TO  CM-LF-DEATH-EXIT-DT   EL1502
03528                                             CM-LF-DEATH-DT        EL1502
03529              PERFORM 7810-REWRITE-CERT THRU 7810-EXIT                CL*13
03530              GO TO 5600-EXIT                                      EL1502
03531      ELSE                                                         EL1502
03532          GO TO 5620-UNLOCK-CERT.                                  EL1502
03533                                                                   EL1502
03534  5610-AH-VOID.                                                    EL1502
03535                                                                   EL1502
03536      IF WS-PAY-TYPE = '4'                                            CL*17
03537          SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT          EL1502
03538          PERFORM 7810-REWRITE-CERT THRU 7810-EXIT                    CL*13
03539          GO TO 5600-EXIT.                                         EL1502
03540                                                                   EL1502
03541      IF WS-PAY-TYPE = '3'                                            CL*17
03542          MOVE CM-AH-STATUS-AT-SETTLEMENT                          EL1502
03543                                  TO  CM-AH-CURRENT-STATUS         EL1502
03544          MOVE SPACES             TO  CM-AH-STATUS-AT-SETTLEMENT   EL1502
03545          SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT          EL1502
03546          MOVE LOW-VALUES         TO  CM-AH-SETTLEMENT-EXIT-DT     EL1502
03547                                      CM-AH-SETTLEMENT-DT          EL1502
03548          PERFORM 7810-REWRITE-CERT THRU 7810-EXIT                    CL*13
03549          GO TO 5600-EXIT                                          EL1502
03550      ELSE                                                         EL1502
03551          GO TO 5620-UNLOCK-CERT.                                  EL1502
03552                                                                   EL1502
03553  5620-UNLOCK-CERT.                                                EL1502
03554                                                                   EL1502
03555      EXEC CICS UNLOCK                                             EL1502
03556          DATASET   ('ELCERT')                                     EL1502
03557      END-EXEC.                                                    EL1502
03558                                                                   EL1502
03559  5600-EXIT.                                                       EL1502
03560      EXIT.                                                        EL1502
03561                                                                   EL1502
03562      EJECT                                                        EL1502
03563  5700-UPDATE-RECON.                                                  CL*12
03564                                                                      CL*12
03565      EXEC CICS HANDLE CONDITION                                      CL*12
03566          NOTFND    (5700-NOT-FOUND)                                  CL*12
03567          NOTOPEN   (8500-FILE-NOTOPEN)                               CL*12
03568      END-EXEC.                                                       CL*12
03569                                                                      CL*12
03570      MOVE PI-COMPANY-CD               TO  RCON-COMPANY-CD.           CL*12
03571      MOVE AT-CHECK-NO                 TO  RCON-CHECK-NO.             CL*12
03572      MOVE 'C'                         TO  RCON-CHECK-ORIGIN.         CL*12
03573      MOVE SPACES                      TO  RCON-GL-ACCOUNT-NO.        CL*12
03574                                                                      CL*12
03575      EXEC CICS READ                                                  CL*12
03576          DATASET   ('ELRCON')                                        CL*12
03577          RIDFLD    (ELRCON-KEY)                                      CL*12
03578          SET       (ADDRESS OF CHECK-RECONCILIATION)                 CL*14
03579          UPDATE                                                      CL*12
03580      END-EXEC.                                                       CL*12
03581                                                                      CL*12
03582 ******************************************************************   CL*12
03583 *         IF THE CHECK HAS BEEN REDEEMED - DO NOT VOID               CL*12
03584 ******************************************************************   CL*12
03585                                                                      CL*12
03586      IF RC-STATUS = 'R'                                              CL*17
03587          MOVE 'X'                TO  WS-RECON-SW                     CL*12
03588          MOVE ER-0823            TO  EMI-ERROR                       CL*12
03589          MOVE -1                 TO  LINENOL                         CL*12
03590          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*12
03591          EXEC CICS UNLOCK                                            CL*12
03592              DATASET   ('ELRCON')                                    CL*12
03593          END-EXEC                                                    CL*12
03594          GO TO 5700-EXIT                                             CL*12
03595      ELSE                                                            CL*12
03596          MOVE ' '                TO  WS-RECON-SW.                    CL*12
03597                                                                      CL*12
03598      MOVE WS-VOID-CODE           TO  RC-STATUS.                      CL*12
03599                                                                      CL*12
03600      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.                CL*12
03601      MOVE '5'                    TO  DC-OPTION-CODE.                 CL*12
03602      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*12
03603      MOVE DC-GREG-DATE-1-MDY     TO  WS-WORK-DATE.                   CL*12
03604      IF WS-WORK-YY > 50                                              CL*23
03605          MOVE '19'               TO  WS-RCON-YY-1                    CL*12
03606          MOVE WS-WORK-YY         TO  WS-RCON-YY-2                    CL*12
03607          MOVE WS-WORK-MM         TO  WS-RCON-MM                      CL*12
03608          MOVE WS-WORK-DD         TO  WS-RCON-DD                      CL*12
03609      ELSE                                                            CL*12
03610          MOVE '20'               TO  WS-RCON-YY-1                    CL*12
03611          MOVE WS-WORK-YY         TO  WS-RCON-YY-2                    CL*12
03612          MOVE WS-WORK-MM         TO  WS-RCON-MM                      CL*12
03613          MOVE WS-WORK-DD         TO  WS-RCON-DD.                     CL*12
03614                                                                      CL*12
03615      MOVE WS-RCON-DATE           TO  RC-STATUS-DATE.                 CL*12
03616                                                                      CL*12
03617      MOVE PI-PROCESSOR-ID        TO  RC-LAST-MAINT-BY.               CL*12
03618      MOVE SAVE-BIN-DATE          TO  RC-LAST-MAINT-DT.               CL*12
03619      MOVE EIBTIME                TO  RC-LAST-MAINT-HHMMSS.           CL*12
03620                                                                      CL*12
03621      EXEC CICS REWRITE                                               CL*12
03622          DATASET   ('ELRCON')                                        CL*12
03623          FROM      (CHECK-RECONCILIATION)                            CL*12
03624      END-EXEC.                                                       CL*12
03625                                                                      CL*12
03626      GO TO 5700-EXIT.                                                CL*12
03627                                                                      CL*12
03628  5700-NOT-FOUND.                                                     CL*12
03629                                                                      CL*12
03630      MOVE 'X'                    TO  WS-RECON-SW.                    CL*12
03631      MOVE -1                     TO  LINENOL.                        CL*12
03632      MOVE ER-0801                TO  EMI-ERROR.                      CL*12
03633      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*12
03634                                                                      CL*12
03635  5700-EXIT.                                                          CL*12
03636      EXIT.                                                           CL*12
03637      EJECT                                                           CL*12
03638  7000-READ-TRLR-UPDATE.                                           EL1502
03639                                                                   EL1502
03640      MOVE PI-COMPANY-CD          TO  TRLR-COMP-CD.                EL1502
03641      MOVE PI-CARRIER             TO  TRLR-CARRIER.                EL1502
03642      MOVE PI-CLAIM-NO            TO  TRLR-CLAIM-NO.               EL1502
03643                                                                   EL1502
03644      EXEC CICS READ                                               EL1502
03645          DATASET   ('ELTRLR')                                     EL1502
03646          RIDFLD    (ELTRLR-KEY)                                   EL1502
03647          SET       (ADDRESS OF ACTIVITY-TRAILERS)                    CL*14
03648          UPDATE                                                   EL1502
03649      END-EXEC.                                                    EL1502
03650                                                                   EL1502
03651  7000-EXIT.                                                       EL1502
03652      EXIT.                                                           CL*12
03653                                                                      CL*12
03654  7010-UNLOCK-TRLR.                                                   CL*12
03655                                                                      CL*12
03656      EXEC CICS UNLOCK                                                CL*12
03657          DATASET   ('ELTRLR')                                        CL*12
03658      END-EXEC.                                                       CL*12
03659                                                                      CL*12
03660  7010-EXIT.                                                          CL*12
03661      EXIT.                                                        EL1502
03662                                                                   EL1502
03663  7100-REWRITE-TRLR.                                               EL1502
03664                                                                   EL1502
03665      MOVE PI-PROCESSOR-ID        TO  PI-UPDATE-BY.                EL1502
03666      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS         EL1502
03667                                      PI-UPDATE-HHMMSS.            EL1502
03668                                                                   EL1502
03669      EXEC CICS REWRITE                                            EL1502
03670          DATASET   ('ELTRLR')                                     EL1502
03671          FROM      (ACTIVITY-TRAILERS)                            EL1502
03672      END-EXEC.                                                    EL1502
03673                                                                   EL1502
03674  7100-EXIT.                                                       EL1502
03675      EXIT.                                                        EL1502
03676                                                                   EL1502
03677      EJECT                                                        EL1502
03678  7200-FIND-BENEFIT.                                               EL1502
03679                                                                   EL1502
03680      MOVE 'N'                    TO  WS-BEN-SEARCH-SW.            EL1502
03681                                                                   EL1502
03682      EXEC CICS HANDLE CONDITION                                   EL1502
03683          ENDFILE   (7200-EXIT)                                    EL1502
03684          NOTFND    (7200-EXIT)                                    EL1502
03685      END-EXEC.                                                    EL1502
03686                                                                   EL1502
03687      EXEC CICS READ                                               EL1502
03688          DATASET   ('ELCNTL')                                     EL1502
03689          RIDFLD    (ELCNTL-KEY)                                   EL1502
03690          SET       (ADDRESS OF CONTROL-FILE)                         CL*14
03691          GTEQ                                                     EL1502
03692      END-EXEC.                                                    EL1502
03693                                                                   EL1502
03694      IF CNTL-COMP-ID NOT  = CF-COMPANY-ID OR                         CL*21
03695         CNTL-REC-TYPE NOT = CF-RECORD-TYPE                           CL*17
03696          GO TO 7200-EXIT.                                         EL1502
03697                                                                   EL1502
03698      PERFORM 7200-BENEFIT-DUMMY THRU 7200-DUMMY-EXIT              EL1502
03699          VARYING SUB FROM 1 BY 1 UNTIL                            EL1502
03700          ((SUB > 8) OR                                               CL*23
03701          (CF-BENEFIT-CODE (SUB) = WS-BEN-CD)).                       CL*17
03702                                                                   EL1502
03703      IF SUB NOT = 9                                                  CL*17
03704          MOVE 'Y'             TO  WS-BEN-SEARCH-SW.               EL1502
03705                                                                   EL1502
03706      GO TO 7200-EXIT.                                             EL1502
03707                                                                   EL1502
03708  7200-BENEFIT-DUMMY.                                              EL1502
03709                                                                   EL1502
03710  7200-DUMMY-EXIT.                                                 EL1502
03711      EXIT.                                                        EL1502
03712                                                                   EL1502
03713  7200-EXIT.                                                       EL1502
03714      EXIT.                                                        EL1502
03715  7400-DEL-TEMP-STOR-TABLE.                                        EL1502
03716                                                                   EL1502
03717      EXEC CICS HANDLE CONDITION                                   EL1502
03718          QIDERR   (7400-EXIT)                                     EL1502
03719      END-EXEC.                                                    EL1502
03720                                                                   EL1502
03721      EXEC CICS DELETEQ TS                                         EL1502
03722          QUEUE    (WS-TABLE-QID)                                  EL1502
03723      END-EXEC.                                                    EL1502
03724                                                                   EL1502
03725  7400-EXIT.                                                       EL1502
03726      EXIT.                                                        EL1502
03727                                                                   EL1502
03728      EJECT                                                        EL1502
03729  7500-READ-CLAIM-MSTR-UPDATE.                                     EL1502
03730                                                                   EL1502
03731      MOVE PI-COMPANY-CD          TO  MSTR-COMP-CD.                EL1502
03732      MOVE PI-CARRIER             TO  MSTR-CARRIER.                EL1502
03733      MOVE PI-CLAIM-NO            TO  MSTR-CLAIM-NO.               EL1502
03734                                                                   EL1502
03735      EXEC CICS READ                                               EL1502
03736          DATASET   ('ELMSTR')                                     EL1502
03737          RIDFLD    (ELMSTR-KEY)                                   EL1502
03738          SET       (ADDRESS OF CLAIM-MASTER)                         CL*14
03739          UPDATE                                                   EL1502
03740      END-EXEC.                                                    EL1502
03741                                                                   EL1502
03742  7500-EXIT.                                                       EL1502
03743      EXIT.                                                           CL*12
03744                                                                      CL*12
03745  7510-UNLOCK-CLAIM-MSTR.                                             CL*12
03746                                                                      CL*12
03747      EXEC CICS UNLOCK                                                CL*12
03748          DATASET   ('ELMSTR')                                        CL*12
03749      END-EXEC.                                                       CL*12
03750                                                                      CL*12
03751  7510-EXIT.                                                          CL*12
03752      EXIT.                                                        EL1502
03753                                                                   EL1502
03754  7600-REWRITE-CLAIM-MSTR.                                         EL1502
03755                                                                   EL1502
03756      MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT.            EL1502
03757      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.        EL1502
03758      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.          EL1502
03759      MOVE '3'                    TO  CL-LAST-MAINT-TYPE.          EL1502
03760                                                                   EL1502
03761      EXEC CICS REWRITE                                            EL1502
03762          DATASET   ('ELMSTR')                                     EL1502
03763          FROM      (CLAIM-MASTER)                                 EL1502
03764      END-EXEC.                                                    EL1502
03765                                                                   EL1502
03766  7600-EXIT.                                                       EL1502
03767      EXIT.                                                        EL1502
03768                                                                   EL1502
03769      EJECT                                                        EL1502
03770  7700-READ-ELACTQ.                                                EL1502
03771                                                                   EL1502
03772      EXEC CICS HANDLE CONDITION                                   EL1502
03773          NOTFND   (7799-EXIT)                                     EL1502
03774      END-EXEC.                                                    EL1502
03775                                                                   EL1502
03776      EXEC CICS READ                                               EL1502
03777          DATASET   ('ELACTQ')                                     EL1502
03778          RIDFLD    (ELACTQ-KEY)                                   EL1502
03779          SET       (ADDRESS OF ACTIVITY-QUE)                         CL*14
03780          UPDATE                                                   EL1502
03781      END-EXEC.                                                    EL1502
03782                                                                   EL1502
03783      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC                          CL*23
03784          MOVE ZEROS              TO  AQ-PMT-UNAPPROVED-COUNT.     EL1502
03785                                                                   EL1502
03786      IF AQ-PAYMENT-COUNTER NOT NUMERIC                               CL*23
03787          MOVE +0                 TO  AQ-PAYMENT-COUNTER.          EL1502
03788                                                                   EL1502
03789      IF WS-CF-PMT-APPROVAL-USED                                      CL*11
03790          IF AQ-PMT-UNAPPROVED-COUNT > +0 AND                         CL*23
03791             WS-CHECK-WRITTEN-DT = LOW-VALUES AND                     CL*17
03792             WS-PMT-APPROVAL-SW = 'U'                                 CL*17
03793              SUBTRACT +1 FROM AQ-PMT-UNAPPROVED-COUNT             EL1502
03794              MOVE 'Y'           TO  WS-UPDATE-SW.                    CL*23
03795                                                                   EL1502
03796      IF PAYMENT-NOT-PRINTED OR                                    EL1502
03797         PAYMENT-NOT-RELEASED                                      EL1502
03798          IF AQ-PAYMENT-COUNTER > +0                                  CL*23
03799              SUBTRACT +1 FROM AQ-PAYMENT-COUNTER                  EL1502
03800              MOVE 'Y'         TO  WS-UPDATE-SW.                   EL1502
03801                                                                   EL1502
03802      IF AQ-PAYMENT-COUNTER = +0                                      CL*17
03803          MOVE ' '             TO  AQ-PENDING-PAYMENT-FLAG.        EL1502
03804                                                                   EL1502
03805      IF WS-UPDATE-SW = 'Y'                                           CL*17
03806          IF AQ-PENDING-ACTIVITY-FLAGS = SPACES                       CL*17
03807              MOVE 'N'            TO  WS-UPDATE-SW                 EL1502
03808              GO TO 7720-DELETE-ACTIVITY-QUE                       EL1502
03809          ELSE                                                     EL1502
03810              MOVE 'N'            TO  WS-UPDATE-SW                 EL1502
03811      ELSE                                                         EL1502
03812          GO TO 7750-UNLOCK-ACTIVITY-QUE.                          EL1502
03813                                                                   EL1502
03814  7710-REWRITE-ACTIVITY-QUE.                                       EL1502
03815                                                                   EL1502
03816      EXEC CICS REWRITE                                            EL1502
03817          DATASET   ('ELACTQ')                                     EL1502
03818          FROM      (ACTIVITY-QUE)                                 EL1502
03819      END-EXEC.                                                    EL1502
03820                                                                   EL1502
03821      GO TO 7799-EXIT.                                             EL1502
03822                                                                   EL1502
03823  7720-DELETE-ACTIVITY-QUE.                                        EL1502
03824                                                                   EL1502
03825      EXEC CICS DELETE                                             EL1502
03826          DATASET   ('ELACTQ')                                     EL1502
03827      END-EXEC.                                                    EL1502
03828                                                                   EL1502
03829      GO TO 7799-EXIT.                                             EL1502
03830                                                                   EL1502
03831  7750-UNLOCK-ACTIVITY-QUE.                                        EL1502
03832                                                                   EL1502
03833      EXEC CICS UNLOCK                                             EL1502
03834          DATASET   ('ELACTQ')                                     EL1502
03835      END-EXEC.                                                    EL1502
03836                                                                   EL1502
03837      MOVE 'N'                    TO  WS-UPDATE-SW.                EL1502
03838                                                                   EL1502
03839  7799-EXIT.                                                       EL1502
03840      EXIT.                                                        EL1502
03841                                                                   EL1502
03842      EJECT                                                        EL1502
03843  7800-READ-CERT-UPDATE.                                           EL1502
03844                                                                   EL1502
03845      EXEC CICS READ                                               EL1502
03846          DATASET   ('ELCERT')                                     EL1502
03847          RIDFLD    (ELCERT-KEY)                                   EL1502
03848          SET       (ADDRESS OF CERTIFICATE-MASTER)                   CL*14
03849          UPDATE                                                   EL1502
03850      END-EXEC.                                                    EL1502
03851                                                                   EL1502
03852  7800-EXIT.                                                       EL1502
03853      EXIT.                                                        EL1502
03854                                                                   EL1502
03855  7810-REWRITE-CERT.                                                  CL*13
03856                                                                   EL1502
03857      EXEC CICS REWRITE                                            EL1502
03858          DATASET   ('ELCERT')                                     EL1502
03859          FROM      (CERTIFICATE-MASTER)                           EL1502
03860      END-EXEC.                                                    EL1502
03861                                                                   EL1502
03862  7810-EXIT.                                                          CL*13
03863      EXIT.                                                        EL1502
03864                                                                   EL1502
03865      EJECT                                                        EL1502
03866  7900-READ-CONTROL-FILE.                                          EL1502
03867                                                                   EL1502
03868      EXEC CICS READ                                               EL1502
03869          DATASET   ('ELCNTL')                                     EL1502
03870          RIDFLD    (ELCNTL-KEY)                                   EL1502
03871          SET       (ADDRESS OF CONTROL-FILE)                         CL*14
03872      END-EXEC.                                                    EL1502
03873                                                                   EL1502
03874  7900-EXIT.                                                       EL1502
03875      EXIT.                                                        EL1502
03876                                                                   EL1502
03877      EJECT                                                        EL1502
03878  8000-LOAD-ERROR-MESSAGES.                                        EL1502
03879      IF EMI-NO-ERRORS                                             EL1502
03880          GO TO 8000-EXIT.                                         EL1502
03881                                                                   EL1502
03882      IF EMI-NUMBER-OF-LINES = 1                                   EL1502
03883          MOVE EMI-LINE1          TO  ERRMSG1O                     EL1502
03884          GO TO 8000-EXIT.                                         EL1502
03885                                                                   EL1502
03886      MOVE EMI-LINE1              TO  ERRMSG1O.                    EL1502
03887                                                                   EL1502
03888  8000-EXIT.                                                       EL1502
03889      EXIT.                                                        EL1502
03890                                                                   EL1502
03891  8100-SEND-INITIAL-MAP.                                           EL1502
03892                                                                      CL*14
03893      IF PI-FULL-SCREEN-SHOWN                                         CL*23
03894          GO TO 8200-SEND-DATAONLY.                                   CL*14
03895                                                                      CL*14
03896      MOVE 'Y'                    TO PI-FULL-SCREEN-IND.              CL*14
03897      MOVE SAVE-DATE              TO  RUNDTEO.                     EL1502
03898      MOVE EIBTIME                TO  TIME-IN.                     EL1502
03899      MOVE TIME-OUT               TO  RUNTIMEO.                    EL1502
03900      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.             EL1502
03901      MOVE PI-PRIME-CERT          TO  PCERTNOO.                    EL1502
03902      MOVE PI-PRIME-SUFX          TO  SUFXO.                       EL1502
03903      MOVE PI-PRIME-HDG           TO  PRIMEHDO.                    EL1502
03904      MOVE AL-SABON               TO  PCERTNOA                     EL1502
03905                                      SUFXA                        EL1502
03906                                      PRIMEHDA.                    EL1502
03907                                                                      CL*14
121802*    IF PI-COMPANY-ID NOT = 'DMD'                                    CL*21
03909          MOVE SPACES             TO PF6O.                            CL*14
03910          MOVE AL-SADOF           TO PF6A.                            CL*14
03911                                                                   EL1502
03912      EXEC CICS SEND                                               EL1502
03913          MAP      (MAP-NAME)                                      EL1502
03914          MAPSET   (MAPSET-NAME)                                   EL1502
03915          FROM     (EL150CO)                                       EL1502
03916          ERASE                                                    EL1502
03917          CURSOR                                                   EL1502
03918      END-EXEC.                                                       CL*21
03919                                                                   EL1502
03920      GO TO 9100-RETURN-TRAN.                                      EL1502
03921                                                                   EL1502
03922  8200-SEND-DATAONLY.                                              EL1502
03923                                                                      CL*14
03924      IF NOT PI-FULL-SCREEN-SHOWN                                     CL*21
03925          GO TO 8100-SEND-INITIAL-MAP.                                CL*14
03926                                                                      CL*14
03927      MOVE SAVE-DATE              TO  RUNDTEO.                     EL1502
03928      MOVE EIBTIME                TO  TIME-IN.                     EL1502
03929      MOVE TIME-OUT               TO  RUNTIMEO.                    EL1502
03930      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.             EL1502
03931                                                                   EL1502
03932      MOVE PI-CARRIER             TO  CARRO.                       EL1502
03933      MOVE PI-CLAIM-NO            TO  CLMNOO.                      EL1502
03934      MOVE PI-PRIME-CERT          TO  PCERTNOO.                    EL1502
03935      MOVE PI-PRIME-SUFX          TO  SUFXO.                       EL1502
03936      MOVE PI-PRIME-HDG           TO  PRIMEHDO.                    EL1502
03937      MOVE AL-SABON               TO  PCERTNOA                     EL1502
03938                                      SUFXA                        EL1502
03939                                      PRIMEHDA.                    EL1502
03940                                                                      CL*14
121802*    IF PI-COMPANY-ID NOT = 'DMD'                                    CL*21
03942          MOVE SPACES             TO PF6O.                            CL*14
03943          MOVE AL-SADOF           TO PF6A.                            CL*14
03944                                                                   EL1502
03945      EXEC CICS SEND                                               EL1502
03946          MAP      (MAP-NAME)                                      EL1502
03947          MAPSET   (MAPSET-NAME)                                   EL1502
03948          FROM     (EL150CO)                                       EL1502
03949          DATAONLY                                                 EL1502
03950          CURSOR                                                   EL1502
03951      END-EXEC.                                                       CL*21
03952                                                                   EL1502
03953      GO TO 9100-RETURN-TRAN.                                      EL1502
03954                                                                   EL1502
03955  8300-SEND-TEXT.                                                  EL1502
03956      EXEC CICS SEND TEXT                                          EL1502
03957          FROM     (LOGOFF-TEXT)                                   EL1502
03958          LENGTH   (LOGOFF-LENGTH)                                 EL1502
03959          ERASE                                                    EL1502
03960          FREEKB                                                   EL1502
03961      END-EXEC.                                                       CL*21
03962                                                                   EL1502
03963      EXEC CICS RETURN                                             EL1502
03964      END-EXEC.                                                       CL*21
03965                                                                   EL1502
03966  8400-NOT-FOUND.                                                  EL1502
03967      IF FILE-SWITCH = 'BENE'                                         CL*17
03968          MOVE ER-0282            TO  EMI-ERROR.                   EL1502
03969                                                                   EL1502
03970      MOVE -1                     TO  LINENOL.                     EL1502
03971      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1502
03972                                                                   EL1502
03973      IF PASS-SWITCH = 'A'                                            CL*17
03974          GO TO 8100-SEND-INITIAL-MAP                              EL1502
03975      ELSE                                                         EL1502
03976          GO TO 8200-SEND-DATAONLY.                                EL1502
03977                                                                   EL1502
03978  8500-FILE-NOTOPEN.                                               EL1502
03979                                                                   EL1502
03980      IF FILE-SWITCH = 'TRLR'                                         CL*17
03981          MOVE ER-0172            TO  EMI-ERROR.                   EL1502
03982                                                                   EL1502
03983      IF FILE-SWITCH = 'CERT'                                         CL*17
03984          MOVE ER-0169            TO  EMI-ERROR.                   EL1502
03985                                                                   EL1502
03986      IF FILE-SWITCH = 'CNTL'                                         CL*17
03987          MOVE ER-0042            TO  EMI-ERROR.                   EL1502
03988                                                                   EL1502
03989      IF FILE-SWITCH = 'ACTQ'                                         CL*17
03990          MOVE ER-0338            TO  EMI-ERROR.                   EL1502
03991                                                                   EL1502
03992      IF FILE-SWITCH = 'MSTR'                                         CL*17
03993          MOVE ER-0154            TO  EMI-ERROR.                   EL1502
03994                                                                      CL*12
03995      IF FILE-SWITCH = 'RCON'                                         CL*17
03996          MOVE ER-0776            TO  EMI-ERROR.                      CL*12
03997                                                                      CL*13
03998      IF FILE-SWITCH = 'PLCY'                                         CL*17
03999          MOVE ER-9883            TO  EMI-ERROR.                      CL*13
04000                                                                   EL1502
04001      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1502
04002                                                                   EL1502
04003      MOVE -1                     TO  LINENOL.                     EL1502
04004                                                                   EL1502
04005      IF PASS-SWITCH = 'A'                                         EL1502
04006          GO TO 8100-SEND-INITIAL-MAP                              EL1502
04007      ELSE                                                         EL1502
04008          GO TO 8200-SEND-DATAONLY.                                EL1502
04009                                                                   EL1502
04010  8800-UNAUTHORIZED-ACCESS.                                        EL1502
04011      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL1502
04012      GO TO 8300-SEND-TEXT.                                        EL1502
04013                                                                   EL1502
04014  8810-PF23.                                                       EL1502
04015      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL1502
04016      MOVE XCTL-005               TO  PGM-NAME.                    EL1502
04017      GO TO 9300-XCTL.                                             EL1502
04018                                                                   EL1502
04019  9100-RETURN-TRAN.                                                EL1502
04020      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL1502
04021      MOVE '150C'                 TO  PI-CURRENT-SCREEN-NO.        EL1502
04022                                                                   EL1502
04023      EXEC CICS RETURN                                             EL1502
04024          TRANSID    (TRANS-ID)                                    EL1502
04025          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL1502
04026          LENGTH     (PI-COMM-LENGTH)                              EL1502
04027      END-EXEC.                                                       CL*21
04028                                                                   EL1502
04029  9200-RETURN-MAIN-MENU.                                           EL1502
04030      MOVE XCTL-126               TO PGM-NAME.                     EL1502
04031      GO TO 9300-XCTL.                                             EL1502
04032                                                                   EL1502
04033  9300-XCTL.                                                       EL1502
04034      EXEC CICS HANDLE CONDITION                                      CL*14
04035          PGMIDERR   (9350-NOT-FOUND)                                 CL*14
04036      END-EXEC.                                                       CL*21
04037                                                                      CL*14
04038      EXEC CICS XCTL                                               EL1502
04039          PROGRAM    (PGM-NAME)                                    EL1502
04040          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL1502
04041          LENGTH     (PI-COMM-LENGTH)                              EL1502
04042      END-EXEC.                                                       CL*21
04043                                                                      CL*14
04044  9350-NOT-FOUND.                                                     CL*14
04045      MOVE ER-0923                TO EMI-ERROR.                       CL*14
04046      MOVE -1                     TO LINENOL.                         CL*14
04047      PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT.                  CL*21
04048      PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT.                  CL*21
04049      PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT.                  CL*21
04050      GO TO 8200-SEND-DATAONLY.                                       CL*14
04051                                                                   EL1502
04052  9400-CLEAR.                                                      EL1502
04053      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL1502
04054      GO TO 9300-XCTL.                                             EL1502
04055                                                                   EL1502
04056  9500-PF12.                                                       EL1502
04057      MOVE XCTL-010               TO  PGM-NAME.                    EL1502
04058      GO TO 9300-XCTL.                                             EL1502
04059                                                                   EL1502
04060  9600-PGMID-ERROR.                                                EL1502
04061      EXEC CICS HANDLE CONDITION                                   EL1502
04062          PGMIDERR   (8300-SEND-TEXT)                              EL1502
04063      END-EXEC.                                                       CL*21
04064                                                                   EL1502
04065      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL1502
04066      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL1502
04067      MOVE XCTL-005               TO  PGM-NAME.                    EL1502
04068      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL1502
04069      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL1502
04070      GO TO 9300-XCTL.                                             EL1502
04071                                                                   EL1502
04072  9700-LINK-DATE-CONVERT.                                          EL1502
04073      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL1502
04074                                                                   EL1502
04075      EXEC CICS LINK                                               EL1502
04076          PROGRAM    (PGM-NAME)                                    EL1502
04077          COMMAREA   (DATE-CONVERSION-DATA)                        EL1502
04078          LENGTH     (DC-COMM-LENGTH)                              EL1502
04079      END-EXEC.                                                       CL*21
04080                                                                      CL*21
04081  9700-EXIT.                                                       EL1502
04082      EXIT.                                                        EL1502
04083                                                                   EL1502
04084  9800-DEEDIT.                                                        CL*14
04085                                                                   EL1502
04086      EXEC CICS BIF DEEDIT                                         EL1502
04087          FIELD   (WS-DEEDIT-FIELD)                                EL1502
04088          LENGTH  (WS-DEEDIT-LENGTH)                               EL1502
04089      END-EXEC.                                                    EL1502
04090                                                                   EL1502
04091  9800-EXIT.                                                       EL1502
04092      EXIT.                                                        EL1502
04093                                                                   EL1502
04094  9900-ERROR-FORMAT.                                               EL1502
04095      IF NOT EMI-ERRORS-COMPLETE                                   EL1502
04096          MOVE LINK-001           TO PGM-NAME                      EL1502
04097          EXEC CICS LINK                                           EL1502
04098              PROGRAM    (PGM-NAME)                                EL1502
04099              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL1502
04100              LENGTH     (EMI-COMM-LENGTH)                         EL1502
04101          END-EXEC.                                                   CL*21
04102                                                                   EL1502
04103  9900-EXIT.                                                       EL1502
04104      EXIT.                                                        EL1502
04105                                                                   EL1502
04106  9990-ABEND.                                                      EL1502
04107      MOVE -1                     TO  ENTERPFL.                    EL1502
04108      MOVE LINK-004               TO  PGM-NAME.                    EL1502
04109                                                                   EL1502
04110      MOVE DFHEIBLK               TO  EMI-LINE1                    EL1502
04111      EXEC CICS LINK                                               EL1502
04112          PROGRAM   (PGM-NAME)                                     EL1502
04113          COMMAREA  (EMI-LINE1)                                    EL1502
04114          LENGTH    (72)                                           EL1502
04115      END-EXEC.                                                    EL1502
04116                                                                   EL1502
04117      MOVE EMI-LINE1              TO  ERRMSG1O.                    EL1502
04118      GO TO 8200-SEND-DATAONLY.                                    EL1502
04119                                                                   EL1502
04120  EJECT                                                            EL1502
04121  9995-SECURITY-VIOLATION.                                         EL1502
04122                              COPY ELCSCTP.                        EL1502
04123                                                                   EL1502
