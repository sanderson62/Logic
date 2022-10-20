00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1502.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 05/04/95 09:35:38.
00007 *                            VMOD=2.024.
00008 *
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00023
00024 *REMARKS.    TRANSACTION - E022 - CHRONOLOGICAL CLAIM HISTORY.
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
041612* 041612  IR2012041300001  AJRA  REMOVE VOID AND STOP PAY FROM SCR
041613* 041613  CR2013031200002  AJRA  ADD MAIL RECEIVED ACTION TYPE
052113* 052113    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
062217* 062217  CR2017050300002  TANA  ADD AUTH RCVD INDICATOR
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
121802******************************************************************
00025
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*   EL1502 WORKING STORAGE     *'.
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.024 *********'.
00034
00035 *                                    COPY ELCSCTM.
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
00036
00037 *                                    COPY ELCSCRTY.
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
00038
00039  EJECT
00040  01  WS-DATE-AREA.
00041      12  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00042      12  SAVE-DATE-CCYYMMDD.
00043          16  SAVE-DATE-CC            PIC XX      VALUE SPACES.
00044          16  SAVE-DATE-YMD.
00045              20  SAVE-DATE-YY        PIC XX      VALUE SPACES.
00046              20  FILLER              PIC X(4)    VALUE SPACES.
00047      12  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
00048
00049  01  STANDARD-AREAS.
00050      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00051      12  MAP-NAME                    PIC X(8)    VALUE 'EL150C'.
00052      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL1502S'.
00053      12  TRANS-ID                    PIC X(4)    VALUE 'E022'.
00054      12  THIS-PGM                    PIC X(8)    VALUE 'EL1502'.
00055      12  PGM-NAME                    PIC X(8).
00056      12  TIME-IN                     PIC S9(7).
00057      12  TIME-OUT-R  REDEFINES TIME-IN.
00058          16  FILLER                  PIC X.
00059          16  TIME-OUT                PIC 99V99.
00060          16  FILLER                  PIC XX.
00061      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00062      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00063      12  XCTL-126                    PIC X(8)    VALUE 'EL126'.
00064      12  XCTL-142                    PIC X(8)    VALUE 'EL142'.
00065      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00066      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00067      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00068      12  FILE-ID                     PIC X(8).
00069      12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.
00070      12  WS-ITEM-COUNT               PIC S9(4)   VALUE +1    COMP.
00071
00072  01  MISC-WORK-AREAS.
00073      12  WS-CK-Q-CONTROL             PIC S9(8) COMP.
00074      12  W-CALLING-PGM               PIC X(8).
00075      12  QID.
00076          16  QID-TERM                PIC X(4).
00077          16  FILLER                  PIC X(4)    VALUE '150B'.
00078      12  WS-TABLE-QID.
00079          16  WS-QID-TERM             PIC X(4).
00080          16  FILLER                  PIC X(4)    VALUE 'TBLE'.
00081
00082      12  MAP-LENGTH                  PIC S9(4)   VALUE +1920 COMP.
00083      12  WS-TABLE-LENGTH             PIC S9(4)   VALUE +3200 COMP.
00084      12  PASS-SWITCH                 PIC X       VALUE 'A'.
00085      12  DISPLAY-CNT                 PIC S9(4)   VALUE +1    COMP.
00086      12  FILE-SWITCH                 PIC X(4)    VALUE SPACES.
00087      12  WS-SUB                      PIC 9       VALUE 0.
00088      12  SUB                         PIC 9       VALUE 1.
00089      12  SUB-1                       PIC 9       VALUE 1.
00090      12  RETURNED-FROM               PIC X(8)    VALUE SPACES.
00091      12  DIRECTION-SWITCH            PIC X       VALUE 'N'.
00092      12  WS-RECORDS-READ-SW          PIC X       VALUE 'N'.
00093          88  RECORDS-READ                        VALUE 'Y'.
00094      12  WS-ENDBR-SW                 PIC X       VALUE 'N'.
00095      12  SAVE-CONTROL                PIC X(39).
00096      12  WS-RECEIVED-DATE            PIC XX      VALUE LOW-VALUES.
00097      12  WS-CHECK-WRITTEN-DT         PIC XX      VALUE LOW-VALUES.
00098      12  WS-PMT-APPROVAL-SW          PIC X       VALUE SPACE.
00099      12  WS-CF-PAYMENT-APPROVAL-SW   PIC X       VALUE ' '.
00100          88  WS-CF-PMT-APPROVAL-USED             VALUE 'Y' 'G'.
00101          88  WS-CF-NO-APPROVAL                   VALUE ' ' 'N'.
00102          88  WS-CF-ALL-APPROVED                  VALUE 'Y'.
00103          88  WS-CF-GRADUATED-APPROVAL            VALUE 'G'.
00104
00105      12  WS-CV-PMT-CODE              PIC X       VALUE SPACE.
00106      12  WS-PAY-TYPE                 PIC X       VALUE SPACE.
00107      12  WS-AMOUNT-PAID              PIC S9(7)V99 VALUE ZEROS.
00108      12  WS-PAYMENT-ORIGIN           PIC X       VALUE SPACE.
00109      12  WS-RECON-SW                 PIC X       VALUE ' '.
00110
00111      12  WS-DEEDIT-LENGTH            PIC S9(4)   VALUE +16   COMP.
00112      12  WS-DEEDIT-FIELD             PIC X(16)   VALUE ZEROS.
00113      12  WS-DEEDIT-FIELD-V0 REDEFINES WS-DEEDIT-FIELD
00114                                      PIC S9(16).
00115
00116      12  WS-LF-COVERAGE-TYPE         PIC X       VALUE SPACE.
00117      12  WS-BEN-SEARCH-SW            PIC X       VALUE 'N'.
00118          88  BENEFIT-FOUND                       VALUE 'Y'.
00119          88  NO-BENEFIT-FOUND                    VALUE 'N'.
00120      12  WS-ACCESS.
00121          16  FILLER                  PIC XX      VALUE SPACES.
00122          16  WS-BEN-CD               PIC XX      VALUE SPACES.
00123      12  WS-FORMAT-CERT-NO.
00124          16  WS-FORMAT-CERT-PRIME    PIC X(10)   VALUE SPACES.
00125          16  FILLER                  PIC X       VALUE SPACE.
00126          16  WS-FORMAT-CERT-SFX      PIC X       VALUE SPACE.
00127      12  WS-PRINTED-SW               PIC X       VALUE 'N'.
00128          88  PAYMENT-HAS-BEEN-PRINTED            VALUE 'Y'.
00129          88  PAYMENT-NOT-PRINTED                 VALUE 'N'.
00130      12  WS-RELEASED-SW              PIC X       VALUE 'N'.
00131          88  PAYMENT-NOT-RELEASED                VALUE 'N'.
00132      12  WS-UPDATE-SW                PIC X       VALUE 'N'.
00133      12  WS-VOID-CODE                PIC X       VALUE ' '.
00134
00135      12  WS-WORK-DATE.
00136          16  WS-WORK-MM              PIC 99      VALUE ZEROS.
00137          16  WS-WORK-DD              PIC 99      VALUE ZEROS.
00138          16  WS-WORK-YY              PIC 99      VALUE ZEROS.
00139
00140      12  WS-RCON-DATE.
00141          16  WS-RCON-YEAR.
00142              20  WS-RCON-YY-1        PIC 99.
00143              20  WS-RCON-YY-2        PIC 99.
00144              20  WS-RCON-MM          PIC 99.
00145              20  WS-RCON-DD          PIC 99.
00146      12  W-NAME-LAST                 PIC  X(15).
00147      12  W-NAME-FIRST                PIC  X(15).
00148      12  W-NAME-MIDDLE.
00149          16  FILLER                  PIC  X.
00150          16  W-NAME-MIDDLE-2         PIC  X.
00151          16  FILLER                  PIC  X(13).
00152
00153      12  HOLD-RECORDED-DT            PIC XX      VALUE LOW-VALUES.
00154      12  HOLD-SUB                    PIC 9(4)    VALUE ZEROS.
00155      12  SUB-2                       PIC 9(4)    VALUE ZEROS.
00156      12  SUB-3                       PIC 9(4)    VALUE ZEROS.
00157      12  WS-MAX-SUB                  PIC 9(4)    VALUE ZEROS.
00158
00159  01  WS-UNSORTED-TABLE.
00160      12  WS-UNSRTD-TABLE   OCCURS 200 TIMES.
00161          16  WS-RECORDED-DT          PIC XX.
00162          16  WS-CERT-NO              PIC X(11).
00163          16  WS-TRLR-SEQ             PIC S9(4)   COMP.
00164          16  WS-SORTED-SW            PIC X.
00165
00166  01  WS-SORTED-TABLE.
00167      12  WS-SRTD-TABLE OCCURS 200 TIMES.
00168          16  WS-SRTD-RECORDED-DT     PIC XX.
00169          16  WS-SRTD-CERT            PIC X(11).
00170          16  WS-SRTD-TRLR-SEQ        PIC S9(4)   COMP.
00171          16  WS-SRTD-SW              PIC X.
00172
00173      12  WS-DMO-LENGTH               PIC S9(4)   VALUE +108 COMP.
00174      12  WS-DCT-LENGTH               PIC S9(4)   VALUE +53  COMP.
00175  EJECT
00176  01  ACCESS-KEYS.
00177      12  ELMSTR-KEY.
00178          16  MSTR-COMP-CD            PIC X.
00179          16  MSTR-CARRIER            PIC X.
00180          16  MSTR-CLAIM-NO           PIC X(7).
00181          16  MSTR-CERT-NO.
00182              20  MSTR-CERT-NO-PRIME  PIC X(10).
00183              20  MSTR-CERT-NO-SUFX   PIC X.
00184      12  ELCNTL-KEY.
00185          16  CNTL-COMP-ID            PIC X(3).
00186          16  CNTL-REC-TYPE           PIC X.
00187          16  CNTL-ACCESS             PIC X(4).
00188          16  CNTL-SEQ-NO             PIC S9(4)     COMP.
00189      12  ELCERT-KEY.
00190          16  CERT-COMP-CD            PIC X.
00191          16  CERT-CARRIER            PIC X.
00192          16  CERT-GROUPING           PIC X(6).
00193          16  CERT-STATE              PIC XX.
00194          16  CERT-ACCOUNT            PIC X(10).
00195          16  CERT-EFF-DT             PIC XX.
00196          16  CERT-CERT-NO.
00197              20  CERT-CERT-NO-PRIME  PIC X(10).
00198              20  CERT-CERT-NO-SUFX   PIC X.
00199      12  ELTRLR-KEY.
00200          16  TRLR-COMP-CD            PIC X.
00201          16  TRLR-CARRIER            PIC X.
00202          16  TRLR-CLAIM-NO           PIC X(7).
00203          16  TRLR-CERT-NO.
00204              20  TRLR-CERT-NO-PRIME  PIC X(10).
00205              20  TRLR-CERT-NO-SUFX   PIC X.
00206          16  TRLR-SEQ-NO             PIC S9(4)   COMP.
00207      12  ELACTQ-KEY.
00208          16  ACTQ-COMP-CD            PIC X.
00209          16  ACTQ-CARRIER            PIC X.
00210          16  ACTQ-CLAIM-NO           PIC X(7).
00211          16  ACTQ-CERT-NO.
00212              20  ACTQ-CERT-NO-PRIME  PIC X(10).
00213              20  ACTQ-CERT-NO-SUFX   PIC X.
00214      12  ELCHKQ-KEY.
00215          16  CHKQ-COMP-CD            PIC X.
00216          16  CHKQ-CONTROL            PIC S9(8)   COMP.
00217          16  CHKQ-SEQ-NO             PIC S9(4)   COMP.
00218      12  ELRCON-KEY.
00219          16  RCON-COMPANY-CD         PIC X.
00220          16  RCON-CHECK-NO           PIC X(7).
00221          16  RCON-CHECK-ORIGIN       PIC X.
00222          16  RCON-GL-ACCOUNT-NO      PIC X(10).
00223      12  EMPLCY-KEY.
00224          16  PLCY-COMPANY-CD         PIC X.
00225          16  PLCY-CARRIER            PIC X.
00226          16  PLCY-GROUPING           PIC X(6).
00227          16  PLCY-STATE              PIC XX.
00228          16  PLCY-PRODUCER           PIC X(10).
00229          16  PLCY-EFF-DT             PIC XX.
00230          16  PLCY-REFERENCE-NO       PIC X(20).
00231      12  W-NOTE-KEY.
00232          16  W-NOTE-COMP-CD          PIC X.
00233          16  W-NOTE-CERT-KEY.
00234              20  W-NOTE-CARRIER      PIC X.
00235              20  W-NOTE-GROUPING     PIC X(6).
00236              20  W-NOTE-STATE        PIC XX.
00237              20  W-NOTE-ACCOUNT      PIC X(10).
00238              20  W-NOTE-EFF-DT       PIC XX.
00239              20  W-NOTE-CERT-NO      PIC X(11).
00240
00241      EJECT
00242  01  ERROR-MESSAGES.
00243      12  ER-0000                     PIC X(4)    VALUE '0000'.
00244      12  ER-0004                     PIC X(4)    VALUE '0004'.
00245      12  ER-0008                     PIC X(4)    VALUE '0008'.
00246      12  ER-0029                     PIC X(4)    VALUE '0029'.
00247      12  ER-0033                     PIC X(4)    VALUE '0033'.
00248      12  ER-0042                     PIC X(4)    VALUE '0042'.
00249      12  ER-0068                     PIC X(4)    VALUE '0068'.
00250      12  ER-0070                     PIC X(4)    VALUE '0070'.
00251      12  ER-0130                     PIC X(4)    VALUE '0130'.
00252      12  ER-0154                     PIC X(4)    VALUE '0154'.
00253      12  ER-0169                     PIC X(4)    VALUE '0169'.
00254      12  ER-0172                     PIC X(4)    VALUE '0172'.
00255      12  ER-0190                     PIC X(4)    VALUE '0190'.
00256      12  ER-0204                     PIC X(4)    VALUE '0204'.
00257      12  ER-0206                     PIC X(4)    VALUE '0206'.
00258      12  ER-0282                     PIC X(4)    VALUE '0282'.
00259      12  ER-0303                     PIC X(4)    VALUE '0303'.
00260      12  ER-0334                     PIC X(4)    VALUE '0334'.
00261      12  ER-0335                     PIC X(4)    VALUE '0335'.
00262      12  ER-0336                     PIC X(4)    VALUE '0336'.
00263      12  ER-0337                     PIC X(4)    VALUE '0337'.
00264      12  ER-0338                     PIC X(4)    VALUE '0338'.
00265      12  ER-0376                     PIC X(4)    VALUE '0376'.
00266      12  ER-0412                     PIC X(4)    VALUE '0412'.
00267      12  ER-0413                     PIC X(4)    VALUE '0413'.
00268      12  ER-0660                     PIC X(4)    VALUE '0660'.
00269      12  ER-0661                     PIC X(4)    VALUE '0661'.
00270      12  ER-0662                     PIC X(4)    VALUE '0662'.
00271      12  ER-0663                     PIC X(4)    VALUE '0663'.
00272      12  ER-0664                     PIC X(4)    VALUE '0664'.
00273      12  ER-0665                     PIC X(4)    VALUE '0665'.
00274      12  ER-0666                     PIC X(4)    VALUE '0666'.
00275      12  ER-0667                     PIC X(4)    VALUE '0667'.
00276      12  ER-0672                     PIC X(4)    VALUE '0672'.
00277      12  ER-0776                     PIC X(4)    VALUE '0776'.
00278      12  ER-0800                     PIC X(4)    VALUE '0800'.
00279      12  ER-0801                     PIC X(4)    VALUE '0801'.
00280      12  ER-0816                     PIC X(4)    VALUE '0816'.
00281      12  ER-0823                     PIC X(4)    VALUE '0823'.
00282      12  ER-0833                     PIC X(4)    VALUE '0833'.
00283      12  ER-0835                     PIC X(4)    VALUE '0835'.
00284      12  ER-0849                     PIC X(4)    VALUE '0849'.
00285      12  ER-0919                     PIC X(4)    VALUE '0919'.
00286      12  ER-0920                     PIC X(4)    VALUE '0920'.
00287      12  ER-0921                     PIC X(4)    VALUE '0921'.
00288      12  ER-0922                     PIC X(4)    VALUE '0922'.
00289      12  ER-0923                     PIC X(4)    VALUE '0923'.
00290      12  ER-0925                     PIC X(4)    VALUE '0925'.
00291      12  ER-0939                     PIC X(4)    VALUE '0939'.
00292      12  ER-0940                     PIC X(4)    VALUE '0940'.
00293      12  ER-0941                     PIC X(4)    VALUE '0941'.
00294      12  ER-0942                     PIC X(4)    VALUE '0942'.
00295      12  ER-0946                     PIC X(4)    VALUE '0946'.
00296      12  ER-0947                     PIC X(4)    VALUE '0947'.
00297      12  ER-0948                     PIC X(4)    VALUE '0948'.
00298      12  ER-0949                     PIC X(4)    VALUE '0949'.
00299      12  ER-0950                     PIC X(4)    VALUE '0950'.
00300      12  ER-0951                     PIC X(4)    VALUE '0951'.
00301      12  ER-0952                     PIC X(4)    VALUE '0952'.
00302      12  ER-0954                     PIC X(4)    VALUE '0954'.
00303      12  ER-0974                     PIC X(4)    VALUE '0974'.
00304      12  ER-0975                     PIC X(4)    VALUE '0975'.
00305      12  ER-2378                     PIC X(4)    VALUE '2378'.
00306      12  ER-2379                     PIC X(4)    VALUE '2379'.
00307      12  ER-7999                     PIC X(4)    VALUE '7999'.
062602     12  ER-8003                     PIC X(4)    VALUE '8003'.
00308      12  ER-8051                     PIC X(4)    VALUE '8051'.
00309      12  ER-8052                     PIC X(4)    VALUE '8052'.
00310      12  ER-8053                     PIC X(4)    VALUE '8053'.
00311      12  ER-8054                     PIC X(4)    VALUE '8054'.
00312      12  ER-8055                     PIC X(4)    VALUE '8055'.
00313      12  ER-8056                     PIC X(4)    VALUE '8056'.
00314      12  ER-8057                     PIC X(4)    VALUE '8057'.
00315      12  ER-8058                     PIC X(4)    VALUE '8058'.
00316      12  ER-8059                     PIC X(4)    VALUE '8059'.
00317      12  ER-8060                     PIC X(4)    VALUE '8060'.
00318      12  ER-8061                     PIC X(4)    VALUE '8061'.
00319      12  ER-8062                     PIC X(4)    VALUE '8062'.
00320      12  ER-8063                     PIC X(4)    VALUE '8063'.
00321      12  ER-8064                     PIC X(4)    VALUE '8064'.
00322      12  ER-8065                     PIC X(4)    VALUE '8065'.
00323      12  ER-8066                     PIC X(4)    VALUE '8066'.
00324      12  ER-8152                     PIC X(4)    VALUE '8152'.
00325      12  ER-8153                     PIC X(4)    VALUE '8153'.
00326      12  ER-8154                     PIC X(4)    VALUE '8154'.
00327      12  ER-8155                     PIC X(4)    VALUE '8155'.
00328      12  ER-9211                     PIC X(4)    VALUE '9211'.
00329      12  ER-9883                     PIC X(4)    VALUE '9883'.
00330
00331  EJECT
00332  01  TEXT-WORK-AREAS.
00333      12  PAYMENT-LINE-1.
00334          16  PMT-HDG1.
00335              20  PMT-LINE-NO         PIC X.
00336              20  FILLER              PIC X.
00337              20  PMT-HDG1-LIT        PIC X(8).
00338          16  PMT-TEXT-1.
00339              20  PMT-CERT-NO         PIC X(12).
00340              20  PMT-TYPE-LIT        PIC X(7).
00341              20  PMT-TYPE            PIC X(7).
00342              20  PMT-CHECK-NO-LIT    PIC X(11).
00343              20  PMT-CHECK-NO        PIC X(7).
00344              20  PMT-AMT-PD-LIT      PIC X(5).
00345              20  PMT-AMT-PAID        PIC Z(6).99-.
00346              20  PMT-REC-BY-LIT      PIC X(5).
00347              20  PMT-REC-BY          PIC X(4).
00348      12  PAYMENT-LINE-2.
00349          16  PMT-HDG2.
00350              20  FILLER              PIC XX.
00351              20  PMT-HDG2-LIT        PIC X(8).
00352          16  PMT-TEXT-2.
00353              20  PMT-REC-DT          PIC X(8).
00354              20  FILLER              PIC X(5).
00355              20  PMT-FROM-LIT        PIC X(6).
00356              20  PMT-PAID-FROM       PIC X(8).
00357              20  PMT-THRU-LIT        PIC X(10).
00358              20  PMT-PAID-THRU       PIC X(8).
00359              20  PMT-VOID-LIT        PIC X(9).
00360              20  PMT-VOID-DT         PIC X(8).
00361              20  FILLER              PIC XX.
00362              20  PMT-PAYEE           PIC X(4).
00363      12  AUTO-PMT-LINE-1.
00364          16  AUTO-PMT-HDG1.
00365              20  AUTO-LINE-NO        PIC X.
00366              20  FILLER              PIC X.
00367              20  AUTO-HDG1-LIT       PIC X(8).
00368          16  AUTO-PMT-TEXT1.
00369              20  AUTO-CERT-NO        PIC X(12).
00370              20  AUTO-EFF-DT-LIT     PIC X(7).
00371              20  AUTO-EFF-DT         PIC X(8).
00372              20  AUTO-1ST-PMT-LIT    PIC X(10).
00373              20  AUTO-1ST-PMT-DT     PIC X(8).
00374              20  AUTO-1ST-AMT-LIT    PIC X(6).
00375              20  AUTO-1ST-AMT        PIC Z(5).99.
00376              20  AUTO-REC-BY-LIT     PIC X(5).
00377              20  AUTO-REC-BY         PIC X(4).
00378      12  AUTO-PMT-LINE-2.
00379          16  AUTO-PMT-HDG2.
00380              20  FILLER              PIC XX.
00381              20  AUTO-HDG2-LIT       PIC X(8).
00382          16  AUTO-PMT-TEXT2.
00383              20  AUTO-REC-DT         PIC X(8).
00384              20  FILLER              PIC X(5).
00385              20  AUTO-LST-PMT-LIT    PIC X(6).
00386              20  AUTO-LST-PMT-DT     PIC X(8).
00387              20  AUTO-STATUS-LIT     PIC X(10).
00388              20  AUTO-STATUS         PIC X(6).
00389              20  AUTO-REG-AMT-LIT    PIC X(8).
00390              20  AUTO-REG-AMT        PIC Z(5).99.
00391              20  AUTO-PAYEE-LIT      PIC X(4).
00392              20  AUTO-PAYEE          PIC X(5).
00393      12  CORRESPONDENCE-LINE-1.
00394          16  CORR-HDG1.
00395              20  CORR-LINE-NO        PIC X.
00396              20  FILLER              PIC X.
00397              20  CORR-HDG1-LIT       PIC X(8).
00398          16  CORR-TEXT-1.
00399              20  CORR-CERT-NO        PIC X(12).
00400              20  CORR-FORM-LIT       PIC X(7).
071210             20  CORR-FORM-TYPE      PIC X(6).
00402              20  CORR-DT-SENT-LIT    PIC X(11).
00403              20  CORR-DT-SENT        PIC X(8).
071210             20  CORR-LET-TO-BEN     PIC X(15).
071210             20  FILLER REDEFINES CORR-LET-TO-BEN.
071210                 25  CORR-PURGE-LIT      PIC X(6).
071210                 25  CORR-PURGE-DT       PIC X(9).
00406              20  CORR-REC-BY-LIT     PIC X(5).
00407              20  CORR-REC-BY         PIC X(4).
00408      12  CORRESPONDENCE-LINE-2.
00409          16  CORR-HDG2.
00410              20  FILLER              PIC XX.
00411              20  CORR-HDG2-LIT       PIC X(8).
00412          16  CORR-TEXT-2.
00413              20  CORR-REC-DT         PIC X(8).
00414              20  FILLER              PIC X(5).
00415              20  CORR-RESEND-LIT     PIC X(6).
00416              20  CORR-RESEND-DT      PIC X(8).
00417              20  CORR-RECVD-LIT      PIC X(10).
00418              20  CORR-RECVD-DT       PIC X(8).
00419              20  CORR-FOLLOW-UP-LIT  PIC X(6).
00420              20  CORR-FOLLOW-UP-DT   PIC X(8).
00421              20  CORR-ADDRESEE-LIT   PIC X(5).
00422              20  CORR-ADDRESEE       PIC X(4).
00423      12  FORM-LINE-1.
00424          16  FORM-HDG1.
00425              20  FORM-LINE-NO        PIC X.
00426              20  FILLER              PIC X.
00427              20  FORM-HDG1-LIT       PIC X(8).
00428          16  FORM-TEXT-1.
00429              20  FORM-CERT-NO        PIC X(12).
00430              20  FORM-TYPE-LIT       PIC X(7).
00431              20  FORM-TYPE           PIC X(8).
00432              20  FORM-SEND-ON-LIT    PIC X(10).
00433              20  FORM-SEND-ON-DT     PIC X(8).
00434              20  FORM-RESEND-LIT     PIC X(6).
00435              20  FORM-RESEND-DT      PIC X(8).
00436              20  FORM-REC-BY-LIT     PIC X(5).
00437              20  FORM-REC-BY         PIC X(4).
00438      12  FORM-LINE-2.
00439          16  FORM-HDG2.
00440              20  FILLER              PIC XX.
00441              20  FORM-HDG2-LIT       PIC X(8).
00442          16  FORM-TEXT-2.
00443              20  FORM-REC-DT         PIC X(8).
00444              20  FILLER              PIC X(5).
00445              20  FORM-REC-INS-LIT    PIC X(6).
00446              20  FORM-REC-INS-DT     PIC X(8).
00447              20  FORM-REC-PHY-LIT    PIC X(10).
00448              20  FORM-REC-PHY-DT     PIC X(8).
00449              20  FORM-REC-EMP-LIT    PIC X(6).
00450              20  FORM-REC-EMP-DT     PIC X(8).
00451              20  FILLER              PIC X(9).
00452      12  INCUR-CHG-LINE-1.
00453          16  INCUR-CHG-HDG1.
00454              20  INCUR-LINE-NO       PIC X.
00455              20  FILLER              PIC X.
00456              20  INCUR-HDG1-LIT      PIC X(8).
00457          16  INCUR-TEXT-1.
00458              20  INCUR-CERT-NO       PIC X(12).
00459              20  INCUR-INCUR-LIT     PIC X(7).
00460              20  INCUR-INCUR-DT      PIC X(8).
00461              20  INCUR-REPORT-LIT    PIC X(10).
00462              20  INCUR-REPORT-DT     PIC X(8).
00463              20  INCUR-ESTAB-LIT     PIC X(6).
00464              20  INCUR-ESTAB-DT      PIC X(8).
00465              20  INCUR-REC-BY-LIT    PIC X(5).
00466              20  INCUR-REC-BY        PIC X(4).
00467      12  INCUR-CHG-LINE-2.
00468          16  INCUR-CHG-HDG2.
00469              20  FILLER              PIC XX.
00470              20  INCUR-HDG2-LIT      PIC X(8).
00471          16  INCUR-TEXT-2.
00472              20  INCUR-REC-DT        PIC X(8).
00473              20  FILLER              PIC X(5).
00474              20  INCUR-PD-THRU-LIT   PIC X(6).
00475              20  INCUR-PD-THRU-DT    PIC X(8).
00476              20  INCUR-TOT-PD-LIT    PIC X(10).
00477              20  INCUR-TOT-PD        PIC Z(5).ZZ.
00478              20  INCUR-TOT-DAYS-LIT  PIC X(6).
00479              20  INCUR-TOT-DAYS-PD   PIC ZZZ.
00480              20  INCUR-NO-PMTS-LIT   PIC X(10).
00481              20  INCUR-NO-PMTS       PIC ZZZ.
00482              20  FILLER              PIC X.
00483      12  DENIAL-LINE-1.
00484          16  DENIAL-HDG1.
00485              20  DENIAL-LINE-NO      PIC X.
00486              20  FILLER              PIC X.
00487              20  DENIAL-HDG1-LIT     PIC X(8).
00488          16  DENIAL-TEXT-1.
00489              20  DENIAL-CERT-NO      PIC X(12).
00490              20  DENIAL-LN1-LIT      PIC X(7).
00491              20  DENIAL-LN1          PIC X(40).
00492              20  DENIAL-REC-BY-LIT   PIC X(5).
00493              20  DENIAL-REC-BY       PIC X(4).
00494      12  DENIAL-LINE-2.
00495          16  DENIAL-HDG2.
00496              20  FILLER              PIC XX.
00497              20  DENIAL-HDG2-LIT     PIC X(8).
00498          16  DENIAL-TEXT-2.
00499              20  DENIAL-REC-DT       PIC X(8).
00500              20  FILLER              PIC X(5).
00501              20  DENIAL-LN2-LIT      PIC X(6).
00502              20  DENIAL-LN2          PIC X(40).
00503              20  FILLER              PIC X(9).
00504      12  GEN-INFO-LINE-1.
00505          16  GEN-INFO-HDG1.
00506              20  GI-LINE-NO          PIC X.
00507              20  FILLER              PIC X.
00508              20  GI-HDG1-LIT         PIC X(8).
00509          16  GEN-INFO-TEXT-1.
00510              20  GEN-INFO-CERT-NO    PIC X(12).
00511              20  GEN-INFO-MSG-1-LIT  PIC X(7).
00512              20  GEN-INFO-MSG-1      PIC X(40).
00513              20  GEN-INFO-REC-BY-LIT PIC X(5).
00514              20  GEN-INFO-REC-BY     PIC X(4).
00515      12  GEN-INFO-LINE-2.
00516          16  GEN-INFO-HDG2.
00517              20  FILLER              PIC XX.
00518              20  GI-HDG2-LIT         PIC X(8).
00519          16  GEN-INFO-TEXT-2.
00520              20  GEN-INFO-REC-DT     PIC X(8).
00521              20  FILLER              PIC X(5).
00522              20  GEN-INFO-MSG-2-LIT  PIC X(6).
00523              20  GEN-INFO-MSG-2      PIC X(40).
00524              20  FILLER              PIC X(9).
00525      12  REMINDER-LINE-1.
00526          16  REMINDER-HDG1.
00527              20  REM-LINE-NO         PIC X.
00528              20  FILLER              PIC X.
00529              20  REM-HDG1-LIT        PIC X(8).
00530          16  REMINDER-TEXT-1.
00531              20  REM-CERT-NO         PIC X(12).
00532              20  REM-LINE-1-LIT      PIC X(7).
00533              20  REM-LINE-1          PIC X(40).
00534              20  REM-REC-BY-LIT      PIC X(5).
00535              20  REM-REC-BY          PIC X(4).
00536      12  REMINDER-LINE-2.
00537          16  REMINDER-HDG2.
00538              20  FILLER              PIC XX.
00539              20  REM-HDG2-LIT        PIC X(8).
00540          16  REMINDER-TEXT-2.
00541              20  REM-REC-DT          PIC X(8).
00542              20  FILLER              PIC X(5).
00543              20  REM-LINE-2-LIT      PIC X(6).
00544              20  REM-LINE-2          PIC X(40).
00545              20  FILLER              PIC X(9).
00546
00547  01  PAYMENT-DESCRIPTION-TABLE.
00548      12  FILLER                      PIC X(7)    VALUE 'PARTIAL'.
00549      12  FILLER                      PIC X(7)    VALUE 'FINAL  '.
00550      12  FILLER                      PIC X(7)    VALUE 'LMP SUM'.
00551      12  FILLER                      PIC X(7)    VALUE 'ADDL   '.
00552      12  FILLER                      PIC X(7)    VALUE 'CHG EXP'.
00553      12  FILLER                      PIC X(7)    VALUE 'NON-CHG'.
00554      12  FILLER                      PIC X(7)    VALUE 'LF PRM '.
00555      12  FILLER                      PIC X(7)    VALUE 'A/H PRM'.
00556      12  FILLER                      PIC X(7)    VALUE 'ENT COR'.
00557  01  PAYMENT-DESC-R   REDEFINES PAYMENT-DESCRIPTION-TABLE.
00558      12  PAY-DESC                    PIC X(7)    OCCURS 2.
00559
00560  01  CV-PAYMENT-DESCRIPTION-TABLE.
00561      12  FILLER                      PIC X(7)    VALUE 'FUL DTH'.
00562      12  FILLER                      PIC X(7)    VALUE 'HLF DTH'.
00563      12  FILLER                      PIC X(7)    VALUE 'FUL ADD'.
00564      12  FILLER                      PIC X(7)    VALUE 'HLF ADD'.
00565      12  FILLER                      PIC X(7)    VALUE 'FUL RID'.
00566      12  FILLER                      PIC X(7)    VALUE 'HLF RID'.
00567      12  FILLER                      PIC X(7)    VALUE 'NON-CHG'.
00568      12  FILLER                      PIC X(7)    VALUE 'ADDL   '.
00569
00570  01  CV-PAYMENT-DESC-R REDEFINES CV-PAYMENT-DESCRIPTION-TABLE.
00571      12  CV-PAY-DESC                 PIC X(7)    OCCURS 2.
00572
00573      EJECT
00574 *                                    COPY ELCDATE.
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
00575      EJECT
00576 *                                    COPY ELCLOGOF.
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
00577      EJECT
00578 *                                    COPY ELCATTR.
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
00579      EJECT
00580 *                                    COPY ELCEMIB.
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
00581      EJECT
121802*                                    COPY ELCDCTB.
121802*    EJECT
121802*                                    COPY ELCDMO.
121802*    EJECT
121802*                                    COPY ELCNWA.
121802*    EJECT
121802*                                    COPY MPCPOLUP.
121802*    EJECT
00590 *                                    COPY ELCINTF.
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
00591
00592      12  PI-REDEF    REDEFINES PI-PROGRAM-WORK-AREA.
062602*        16  FILLER                  PIC 9(4).
062602         16  FILLER                  PIC x(2).
062602         16  pi-el142-priority       pic x.
062602         16  filler                  pic x.
00594          16  PI-MAP-NAME             PIC X(8).
00595          16  PI-QUALIFICATION-SWITCHES  COMP-3.
00596              20  PI-REMINDERS-SW     PIC S9.
00597              20  PI-LETTERS-SW       PIC S9.
00598              20  PI-PAYMENTS-SW      PIC S9.
00599              20  PI-AUTO-PAY-SW      PIC S9.
00600              20  PI-NOTES-SW         PIC S9.
00601              20  PI-RES-EXP-SW       PIC S9.
00602              20  PI-DENIALS-SW       PIC S9.
00603              20  PI-INCURRED-DATE-SW PIC S9.
00604              20  PI-FORMS-SW         PIC S9.
00605          16  FILLER                  PIC X(8).
00606          16  PI-ACTIVITY-TRAILERS-KEY.
00607              20  PI-ATK-COMPANY-CODE PIC X.
00608              20  PI-ATK-CARRIER      PIC X.
00609              20  PI-ATK-CLAIM-NO     PIC X(7).
00610              20  PI-ATK-CERT-NO      PIC X(11).
00611              20  PI-ATK-SEQ-NO       PIC S9(4)     COMP.
00612          16  PI-PREV-ACTIVITY-TRAILERS-KEY.
00613              20  PI-PREV-ATK-COMPANY-CODE PIC X.
00614              20  PI-PREV-ATK-CARRIER      PIC X.
00615              20  PI-PREV-ATK-CLAIM-NO     PIC X(7).
00616              20  PI-PREV-ATK-CERT-NO      PIC X(11).
00617              20  PI-PREV-ATK-SEQ-NO       PIC S9(4)     COMP.
00618          16  PI-SAVE-KEY.
00619              20  PI-SAVE-ATK-COMPANY-CODE PIC X.
00620              20  PI-SAVE-ATK-CARRIER      PIC X.
00621              20  PI-SAVE-ATK-CLAIM-NO     PIC X(7).
00622              20  PI-SAVE-ATK-CERT-NO      PIC X(11).
00623              20  PI-SAVE-ATK-SEQ-NO       PIC S9(4)     COMP.
00624          16  PI-PREV-AID                  PIC X.
00625          16  PI-RECORD-COUNT              PIC S9        COMP-3.
00626          16  PI-END-OF-FILE               PIC S9        COMP-3.
00627          16  PI-SAVE-CURSOR               PIC S9(4)     COMP.
00628          16  PI-PURGED-SW                 PIC X.
00629              88  CLAIM-IS-PURGED                 VALUE 'Y'.
00630          16  PI-LINE-NO                   PIC 9.
00631          16  PI-PREV-SEQ-NO               PIC S9(4)   COMP.
00632          16  PI-FIRST-TIME-SW             PIC X.
00633              88  FIRST-TIME                      VALUE 'Y'.
00634
00635          16  PI-SEQ-NUMBERS.
00636              20  PI-SEQ-NO-TABLE OCCURS 8 TIMES.
00637                  24  PI-TRLR-LN-NO        PIC 9.
00638                  24  PI-TRLR-SEQ-NO       PIC S9(4)   COMP.
00639                  24  PI-TRLR-TYPE         PIC X.
00640                  24  PI-TRLR-CERT         PIC X(11).
00641                  24  PI-TRLR-SUB          PIC 9(4).
00642
00643          16  PI-PREV-CLAIM-NO             PIC X(7).
00644          16  PI-PREV-CARRIER              PIC X.
00645          16  PI-PRIME-CERT-NO.
00646              20  PI-PRIME-CERT            PIC X(10).
00647              20  PI-PRIME-SUFX            PIC X.
00648          16  PI-PRIME-HDG                 PIC X(12).
00649          16  PI-MAX-SUB                   PIC 9(4).
00650          16  PI-PAY-TYPE                  PIC X.
00651          16  PI-FULL-SCREEN-IND           PIC X.
00652              88  PI-FULL-SCREEN-SHOWN         VALUE 'Y'.
00653          16  FILLER                  PIC X(346).
00654
00655      EJECT
00656 *                                    COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  '�'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00657  01  FILLER    REDEFINES DFHAID.
00658      12  FILLER                      PIC X(8).
00659      12  PF-VALUES                   PIC X       OCCURS 24 TIMES.
00660      EJECT
00661 *                                    COPY EL1502S.
       01  EL150CI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEL PIC S9(0004) COMP.
           05  RUNDTEF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEF.
               10  RUNDTEA PIC  X(0001).
           05  RUNDTEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  CLMNOL PIC S9(0004) COMP.
           05  CLMNOF PIC  X(0001).
           05  FILLER REDEFINES CLMNOF.
               10  CLMNOA PIC  X(0001).
           05  CLMNOI PIC  X(0007).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  PRIMEHDL PIC S9(0004) COMP.
           05  PRIMEHDF PIC  X(0001).
           05  FILLER REDEFINES PRIMEHDF.
               10  PRIMEHDA PIC  X(0001).
           05  PRIMEHDI PIC  X(0012).
      *    -------------------------------
           05  PCERTNOL PIC S9(0004) COMP.
           05  PCERTNOF PIC  X(0001).
           05  FILLER REDEFINES PCERTNOF.
               10  PCERTNOA PIC  X(0001).
           05  PCERTNOI PIC  X(0010).
      *    -------------------------------
           05  SUFXL PIC S9(0004) COMP.
           05  SUFXF PIC  X(0001).
           05  FILLER REDEFINES SUFXF.
               10  SUFXA PIC  X(0001).
           05  SUFXI PIC  X(0001).
      *    -------------------------------
           05  LN1HDGL PIC S9(0004) COMP.
           05  LN1HDGF PIC  X(0001).
           05  FILLER REDEFINES LN1HDGF.
               10  LN1HDGA PIC  X(0001).
           05  LN1HDGI PIC  X(0010).
      *    -------------------------------
           05  LN1TXTL PIC S9(0004) COMP.
           05  LN1TXTF PIC  X(0001).
           05  FILLER REDEFINES LN1TXTF.
               10  LN1TXTA PIC  X(0001).
           05  LN1TXTI PIC  X(0068).
      *    -------------------------------
           05  LN2HDGL PIC S9(0004) COMP.
           05  LN2HDGF PIC  X(0001).
           05  FILLER REDEFINES LN2HDGF.
               10  LN2HDGA PIC  X(0001).
           05  LN2HDGI PIC  X(0010).
      *    -------------------------------
           05  LN2TXTL PIC S9(0004) COMP.
           05  LN2TXTF PIC  X(0001).
           05  FILLER REDEFINES LN2TXTF.
               10  LN2TXTA PIC  X(0001).
           05  LN2TXTI PIC  X(0068).
      *    -------------------------------
           05  LN3HDGL PIC S9(0004) COMP.
           05  LN3HDGF PIC  X(0001).
           05  FILLER REDEFINES LN3HDGF.
               10  LN3HDGA PIC  X(0001).
           05  LN3HDGI PIC  X(0010).
      *    -------------------------------
           05  LN3TXTL PIC S9(0004) COMP.
           05  LN3TXTF PIC  X(0001).
           05  FILLER REDEFINES LN3TXTF.
               10  LN3TXTA PIC  X(0001).
           05  LN3TXTI PIC  X(0068).
      *    -------------------------------
           05  LN4HDGL PIC S9(0004) COMP.
           05  LN4HDGF PIC  X(0001).
           05  FILLER REDEFINES LN4HDGF.
               10  LN4HDGA PIC  X(0001).
           05  LN4HDGI PIC  X(0010).
      *    -------------------------------
           05  LN4TXTL PIC S9(0004) COMP.
           05  LN4TXTF PIC  X(0001).
           05  FILLER REDEFINES LN4TXTF.
               10  LN4TXTA PIC  X(0001).
           05  LN4TXTI PIC  X(0068).
      *    -------------------------------
           05  LN5HDGL PIC S9(0004) COMP.
           05  LN5HDGF PIC  X(0001).
           05  FILLER REDEFINES LN5HDGF.
               10  LN5HDGA PIC  X(0001).
           05  LN5HDGI PIC  X(0010).
      *    -------------------------------
           05  LN5TXTL PIC S9(0004) COMP.
           05  LN5TXTF PIC  X(0001).
           05  FILLER REDEFINES LN5TXTF.
               10  LN5TXTA PIC  X(0001).
           05  LN5TXTI PIC  X(0068).
      *    -------------------------------
           05  LN6HDGL PIC S9(0004) COMP.
           05  LN6HDGF PIC  X(0001).
           05  FILLER REDEFINES LN6HDGF.
               10  LN6HDGA PIC  X(0001).
           05  LN6HDGI PIC  X(0010).
      *    -------------------------------
           05  LN6TXTL PIC S9(0004) COMP.
           05  LN6TXTF PIC  X(0001).
           05  FILLER REDEFINES LN6TXTF.
               10  LN6TXTA PIC  X(0001).
           05  LN6TXTI PIC  X(0068).
      *    -------------------------------
           05  LN7HDGL PIC S9(0004) COMP.
           05  LN7HDGF PIC  X(0001).
           05  FILLER REDEFINES LN7HDGF.
               10  LN7HDGA PIC  X(0001).
           05  LN7HDGI PIC  X(0010).
      *    -------------------------------
           05  LN7TXTL PIC S9(0004) COMP.
           05  LN7TXTF PIC  X(0001).
           05  FILLER REDEFINES LN7TXTF.
               10  LN7TXTA PIC  X(0001).
           05  LN7TXTI PIC  X(0068).
      *    -------------------------------
           05  LN8HDGL PIC S9(0004) COMP.
           05  LN8HDGF PIC  X(0001).
           05  FILLER REDEFINES LN8HDGF.
               10  LN8HDGA PIC  X(0001).
           05  LN8HDGI PIC  X(0010).
      *    -------------------------------
           05  LN8TXTL PIC S9(0004) COMP.
           05  LN8TXTF PIC  X(0001).
           05  FILLER REDEFINES LN8TXTF.
               10  LN8TXTA PIC  X(0001).
           05  LN8TXTI PIC  X(0068).
      *    -------------------------------
           05  LN9HDGL PIC S9(0004) COMP.
           05  LN9HDGF PIC  X(0001).
           05  FILLER REDEFINES LN9HDGF.
               10  LN9HDGA PIC  X(0001).
           05  LN9HDGI PIC  X(0010).
      *    -------------------------------
           05  LN9TXTL PIC S9(0004) COMP.
           05  LN9TXTF PIC  X(0001).
           05  FILLER REDEFINES LN9TXTF.
               10  LN9TXTA PIC  X(0001).
           05  LN9TXTI PIC  X(0068).
      *    -------------------------------
           05  LN10HDGL PIC S9(0004) COMP.
           05  LN10HDGF PIC  X(0001).
           05  FILLER REDEFINES LN10HDGF.
               10  LN10HDGA PIC  X(0001).
           05  LN10HDGI PIC  X(0010).
      *    -------------------------------
           05  LN10TXTL PIC S9(0004) COMP.
           05  LN10TXTF PIC  X(0001).
           05  FILLER REDEFINES LN10TXTF.
               10  LN10TXTA PIC  X(0001).
           05  LN10TXTI PIC  X(0068).
      *    -------------------------------
           05  LN11HDGL PIC S9(0004) COMP.
           05  LN11HDGF PIC  X(0001).
           05  FILLER REDEFINES LN11HDGF.
               10  LN11HDGA PIC  X(0001).
           05  LN11HDGI PIC  X(0010).
      *    -------------------------------
           05  LN11TXTL PIC S9(0004) COMP.
           05  LN11TXTF PIC  X(0001).
           05  FILLER REDEFINES LN11TXTF.
               10  LN11TXTA PIC  X(0001).
           05  LN11TXTI PIC  X(0068).
      *    -------------------------------
           05  LN12HDGL PIC S9(0004) COMP.
           05  LN12HDGF PIC  X(0001).
           05  FILLER REDEFINES LN12HDGF.
               10  LN12HDGA PIC  X(0001).
           05  LN12HDGI PIC  X(0010).
      *    -------------------------------
           05  LN12TXTL PIC S9(0004) COMP.
           05  LN12TXTF PIC  X(0001).
           05  FILLER REDEFINES LN12TXTF.
               10  LN12TXTA PIC  X(0001).
           05  LN12TXTI PIC  X(0068).
      *    -------------------------------
           05  LN13HDGL PIC S9(0004) COMP.
           05  LN13HDGF PIC  X(0001).
           05  FILLER REDEFINES LN13HDGF.
               10  LN13HDGA PIC  X(0001).
           05  LN13HDGI PIC  X(0010).
      *    -------------------------------
           05  LN13TXTL PIC S9(0004) COMP.
           05  LN13TXTF PIC  X(0001).
           05  FILLER REDEFINES LN13TXTF.
               10  LN13TXTA PIC  X(0001).
           05  LN13TXTI PIC  X(0068).
      *    -------------------------------
           05  LN14HDGL PIC S9(0004) COMP.
           05  LN14HDGF PIC  X(0001).
           05  FILLER REDEFINES LN14HDGF.
               10  LN14HDGA PIC  X(0001).
           05  LN14HDGI PIC  X(0010).
      *    -------------------------------
           05  LN14TXTL PIC S9(0004) COMP.
           05  LN14TXTF PIC  X(0001).
           05  FILLER REDEFINES LN14TXTF.
               10  LN14TXTA PIC  X(0001).
           05  LN14TXTI PIC  X(0068).
      *    -------------------------------
           05  LN15HDGL PIC S9(0004) COMP.
           05  LN15HDGF PIC  X(0001).
           05  FILLER REDEFINES LN15HDGF.
               10  LN15HDGA PIC  X(0001).
           05  LN15HDGI PIC  X(0010).
      *    -------------------------------
           05  LN15TXTL PIC S9(0004) COMP.
           05  LN15TXTF PIC  X(0001).
           05  FILLER REDEFINES LN15TXTF.
               10  LN15TXTA PIC  X(0001).
           05  LN15TXTI PIC  X(0068).
      *    -------------------------------
           05  LN16HDGL PIC S9(0004) COMP.
           05  LN16HDGF PIC  X(0001).
           05  FILLER REDEFINES LN16HDGF.
               10  LN16HDGA PIC  X(0001).
           05  LN16HDGI PIC  X(0010).
      *    -------------------------------
           05  LN16TXTL PIC S9(0004) COMP.
           05  LN16TXTF PIC  X(0001).
           05  FILLER REDEFINES LN16TXTF.
               10  LN16TXTA PIC  X(0001).
           05  LN16TXTI PIC  X(0068).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  LINENOL PIC S9(0004) COMP.
           05  LINENOF PIC  X(0001).
           05  FILLER REDEFINES LINENOF.
               10  LINENOA PIC  X(0001).
           05  LINENOI PIC  X(0001).
      *    -------------------------------
           05  RECVDTL PIC S9(0004) COMP.
           05  RECVDTF PIC  X(0001).
           05  FILLER REDEFINES RECVDTF.
               10  RECVDTA PIC  X(0001).
           05  RECVDTI PIC  X(0008).
      *    -------------------------------
           05  RECVTYPL PIC S9(0004) COMP.
           05  RECVTYPF PIC  X(0001).
           05  FILLER REDEFINES RECVTYPF.
               10  RECVTYPA PIC  X(0001).
           05  RECVTYPI PIC  X(0001).
      *    -------------------------------
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  99.
      *    -------------------------------
           05  PF6L PIC S9(0004) COMP.
           05  PF6F PIC  X(0001).
           05  FILLER REDEFINES PF6F.
               10  PF6A PIC  X(0001).
           05  PF6I PIC  X(0016).
       01  EL150CO REDEFINES EL150CI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRIMEHDO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN1HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN1TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN2HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN2TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN3HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN3TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN4HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN4TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN5HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN5TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN6HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN6TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN7HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN7TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN8HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN8TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN9HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN9TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN10HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN10TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN11HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN11TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN12HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN12TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN13HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN13TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN14HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN14TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN15HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN15TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN16HDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LN16TXTO PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINENOO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECVDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECVTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF6O PIC  X(0016).
      *    -------------------------------
00662  01  EL150CI-R REDEFINES EL150CI.
00663      12  FILLER                      PIC X(77).
00664      12  EL150CI-OCCURS OCCURS 16.
00665          16  MAP-HDG-LENGTH          PIC S9(4)   COMP.
00666          16  MAP-HDG-ATTRB           PIC X.
00667          16  MAP-HDG.
00668              20  MAP-LINE-NO         PIC X.
00669              20  FILLER              PIC X.
00670              20  MAP-HDG-LIT         PIC X(8).
00671          16  MAP-TEXT-LENGTH         PIC S9(4)   COMP.
00672          16  MAP-TEXT-ATTRB          PIC X.
00673          16  MAP-TEXT                PIC X(68).
00674      12  FILLER                      PIC X(144).
00675
00676      EJECT
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
00678  01  DFHCOMMAREA                     PIC X(1024).
00679
00680 *                                    COPY ELCMSTR.
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
00681      EJECT
00682 *                                    COPY ELCCNTL.
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
00683      EJECT
00684 *                                    COPY ELCCERT.
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
00685      EJECT
00686 *                                    COPY ELCTRLR.
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
00687      EJECT
00688 *                                    COPY ELCACTQ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCACTQ.                            *
00004 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY QUE FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 60     RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELACTQ             RKP=2,LEN=20          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCACTQ                          *
00017 ******************************************************************
00018
00019  01  ACTIVITY-QUE.
00020      12  AQ-RECORD-ID                PIC XX.
00021          88  VALID-AQ-ID                VALUE 'AQ'.
00022
00023      12  AQ-CONTROL-PRIMARY.
00024          16  AQ-COMPANY-CD           PIC X.
00025          16  AQ-CARRIER              PIC X.
00026          16  AQ-CLAIM-NO             PIC X(7).
00027          16  AQ-CERT-NO.
00028              20  AQ-CERT-PRIME       PIC X(10).
00029              20  AQ-CERT-SFX         PIC X.
00030
00031      12  AQ-PENDING-ACTIVITY-FLAGS.
00032          88  NO-PENDING-ACTIVITY        VALUE SPACES.
00033          16  AQ-PENDING-PAYMENT-FLAG PIC X.
00034              88  PENDING-PAYMENTS       VALUE '1'.
00035          16  AQ-PENDING-STATUS-FLAG  PIC X.
00036              88  PENDING-FULL-PRINT     VALUE '1'.
00037              88  PENDING-PART-PRINT     VALUE '2'.
00038          16  AQ-PENDING-LETTER-FLAG  PIC X.
00039              88  PENDING-LETTERS        VALUE '1'.
00040          16  AQ-PENDING-CLAIM-RESTORE PIC X.
00041              88  PENDING-RESTORE        VALUE 'C'.
00042              88  PENDING-RESTORE-LETTER VALUE 'L'.
00043
00044      12  FILLER                      PIC X(20).
00045
00046      12  AQ-RESEND-DATE              PIC XX.
00047      12  AQ-FOLLOWUP-DATE            PIC XX.
00048      12  AQ-PAYMENT-COUNTER          PIC S9        COMP-3.
00049      12  AQ-PMT-UNAPPROVED-COUNT     PIC S9        COMP-3.
00050      12  AQ-AUTO-LETTER              PIC X(4).
00051      12  FILLER                      PIC XX.
00052      12  AQ-LAST-UPDATED-BY          PIC S9(4)     COMP.
00053 *****************************************************************
00689      EJECT
00690 *                                    COPY ELCCHKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCHKQ.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE                            *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 100  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCHKQ                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH1 = ELCHKQ2 (BY PAYEE)      RKP=9,LEN=26   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-QUE.
00019      12  CQ-RECORD-ID                PIC XX.
00020          88  VALID-CQ-ID         VALUE 'CQ'.
00021
00022      12  CQ-CONTROL-PRIMARY.
00023          16  CQ-COMPANY-CD           PIC X.
00024          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00025          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00026
00027      12  CQ-CONTROL-BY-PAYEE.
DJNA           16  CQ-CONTROL-BY-NUMBER.
DJNA               20  CQ-COMPANY-CD-A1     PIC X.
DJNA               20  CQ-CONTROL-NUMBER-A1 PIC S9(8)      COMP.
00030          16  CQ-PAYEE-CARRIER        PIC X.
00031          16  CQ-PAYEE-GROUPING       PIC X(6).
00032          16  CQ-PAYEE-STATE          PIC XX.
00033          16  CQ-PAYEE-BENE-ACCT      PIC X(10).
00034          16  CQ-SEQUENCE-NUMBER-A1   PIC S9(4)       COMP.
00035
00036      12  CQ-DMD-CONTROL  REDEFINES  CQ-CONTROL-BY-PAYEE.
00037          16  CQ-DMD-COMPANY-CD-A2    PIC X.
00038          16  CQ-DMD-PAYEE-TYPE-A2    PIC X.
00039          16  CQ-DMD-BENE-CODE-A2     PIC X(10).
00040          16  CQ-DMD-CLAIM-NO-A2      PIC X(7).
00041          16  CQ-DMD-TIME-SEQ-A2      PIC S9(7)       COMP.
00042          16  FILLER                  PIC X(3).
00043
00044      12  CQ-ENTRY-TYPE               PIC X.
00045              88  CHECK-ON-QUE           VALUE 'Q'.
00046              88  ALIGNMENT-CHECK        VALUE 'A'.
00047              88  SPOILED-CHECK          VALUE 'S'.
00048              88  PAYMENT-ABORTED        VALUE 'X'.
00049
00050      12  CQ-CLAIM-MAST-CNTL.
00051          16  CQ-CARRIER              PIC X.
00052          16  CQ-CLAIM-NO             PIC X(7).
00053          16  CQ-CERT-NO.
00054              20  CQ-CERT-PRIME       PIC X(10).
00055              20  CQ-CERT-SFX         PIC X.
00056          16  CQ-CLAIM-TYPE           PIC X.
00057              88  CQ-LIFE-CLAIM          VALUE 'L'.
00058              88  CQ-AH-CLAIM            VALUE 'A'.
00059          16  CQ-CLAIM-SUB-TYPE       PIC X.
00060              88  CQ-FIXED-COVERAGE      VALUE '1'.
00061              88  CQ-O-B-COVERAGE        VALUE '2'.
00062              88  CQ-OPEN-END-COVERAGE   VALUE '3'.
00063
00064      12  CQ-PMT-TRLR-SEQUENCE        PIC S9(4)       COMP.
00065      12  CQ-CHECK-NUMBER             PIC X(7).
00066      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00067      12  CQ-PAYMENT-TYPE             PIC X.
00068              88  CQ-PARTIAL-PAYMENT        VALUE '1'.
00069              88  CQ-FINAL-PAYMENT          VALUE '2'.
00070              88  CQ-LUMP-SUM-PAYMENT       VALUE '3'.
00071              88  CQ-ADDITIONAL-PAYMENT     VALUE '4'.
00072              88  CQ-CHARGEABLE-EXPENSE     VALUE '5'.
00073              88  CQ-NON-CHARGEABLE-EXPENSE VALUE '6'.
00074              88  CQ-LIFE-PREMIUM-REFUND    VALUE '7'.
00075              88  CQ-AH-PREMIUM-REFUND      VALUE '8'.
00076      12  CQ-VOID-INDICATOR           PIC X.
00077              88  CHECK-IS-STOPPED          VALUE 'S'.
00078              88  CHECK-IS-VOID             VALUE 'V'.
00079      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
00080      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00081      12  CQ-CHECK-BY-USER            PIC X(4).
00082      12  CQ-PRE-NUMBERING-SW         PIC X.
00083        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00084        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00085
00086      12  CQ-CHECK-WRITTEN-DT         PIC XX.
00087      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
00088      12  CQ-LEDGER-FLAG              PIC X(01).
00089      12  CQ-VOID-AFTER-LEDGER        PIC X(01).
00090      12  CQ-LAST-UPDATED-DT          PIC XX.
00091      12  CQ-LAST-UPDATED-HHMMSS      PIC S9(6)       COMP-3.
00092      12  CQ-APPLIED-TO-RCON-DT       PIC XX.
00093
00094      12  FILLER                      PIC X(04).
00095
00096 ******************************************************************
00691      EJECT
00692 *                                    COPY ELCRCON.
00001 ******************************************************************
00002 *                                                                *
      *
121703*   THIS COPYBOOK IS NOT BEING USED IN LOGIC
      *
      *
00002 *                                                                *
00003 *                            ELCRCON.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK RECONCILIATION FILE                 *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 194  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELRCON                         RKP=2,LEN=19   *
00013 *                                                                *
00014 *   LOG = YES                                                    *
00015 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
00017
00018  01  CHECK-RECONCILIATION.
00019      12  RC-RECORD-ID                          PIC XX.
00020          88  VALID-RC-ID                    VALUE 'RC'.
00021      12  RC-CONTROL-PRIMARY.
00022          16  RC-COMPANY-CD                     PIC X.
00023          16  RC-CHECK-NO                       PIC X(7).
00024          16  RC-CHECK-ORIGIN                   PIC X.
00025          16  RC-GL-ACCOUNT-NO                  PIC X(10).
00026
00027      12  RC-CHECK-DATA.
00028          16  RC-ISSUE-DATE.
00029              20  RC-ISSUE-YYYY.
00030                22  RC-ISSUE-CC                 PIC XX.
00031                22  RC-ISSUE-YY                 PIC XX.
00032              20  RC-ISSUE-MM                   PIC XX.
00033              20  RC-ISSUE-DD                   PIC XX.
00034          16  RC-CHECK-AMOUNT                   PIC 9(7)V99.
00035          16  RC-CARRIER                        PIC X.
00036          16  RC-CLAIM-NO                       PIC X(7).
00037          16  RC-REFERENCE-NO                   PIC X(20).
00038          16  RC-MORTGAGE-REF   REDEFINES  RC-REFERENCE-NO.
00039              20  RC-MORT-REF-1-18              PIC X(18).
00040              20  RC-MORT-REF-SUF-19-20         PIC XX.
00041          16  FILLER  REDEFINES  RC-MORTGAGE-REF.
00042              20  FILLER                        PIC X(13).
00043              20  RC-CLAIM-REF.
00044                22  RC-CLAIM-PREFIX             PIC X.
00045                22  RC-CLAIM-NO-REF             PIC X(6).
00046          16  RC-COVERAGE-TYPE                  PIC X.
00047          16  RC-BENEFIT-CODE                   PIC XX.
00048          16  RC-BENEFICIARY                    PIC X(10).
00049          16  RC-PAYMENT-TYPE                   PIC X.
00050          16  RC-STATUS                         PIC X.
020403           88  RC-STATUS-ABANDONED                VALUE 'A'.
00052            88  RC-STATUS-DESTROYED                VALUE 'D'.
00053            88  RC-STATUS-OUTSTANDING              VALUE 'O'.
00054            88  RC-STATUS-REDEEMED                 VALUE 'R'.
00055            88  RC-STATUS-STOP-PAY                 VALUE 'S'.
00056            88  RC-STATUS-UNREDEEMED               VALUE 'U'.
00057            88  RC-STATUS-VOIDED                   VALUE 'V'.
00058          16  RC-STATUS-DATE.
00059              20  RC-STATUS-YYYY.
00060                22  RC-STATUS-CC                PIC XX.
00061                22  RC-STATUS-YY                PIC XX.
00062              20  RC-STATUS-MM                  PIC XX.
00063              20  RC-STATUS-DD                  PIC XX.
00064
00065      12  RC-MAINT-AREA.
00066          16  RC-LAST-MAINT-BY                  PIC X(4).
00067          16  RC-LAST-MAINT-DT                  PIC XX.
00068          16  RC-LAST-MAINT-HHMMSS    COMP-3    PIC S9(7).
00069
00070      12  RC-CHECK-MAINT-AREA.
00071          16  RC-LAST-CHECK-BY                  PIC X(4).
00072          16  RC-LAST-CHECK-DT                  PIC XX.
00073          16  RC-LAST-CHECK-HHMMSS    COMP-3    PIC S9(7).
020403     12  RC-CASHED-AMOUNT                      PIC 9(7)V99.
00074
020403     12  RC-CHECK-NOTE                         PIC X(67).
020403     12  FILLER                                PIC X(11).
00076
00693      EJECT
121802*                                    COPY ERCDMDNT.
121802*    EJECT
121802*                                    COPY MPCPLCY.
121802*    EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER
                                CONTROL-FILE CERTIFICATE-MASTER
                                ACTIVITY-TRAILERS ACTIVITY-QUE
                                CHECK-QUE CHECK-RECONCILIATION.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1502' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00699
00700      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00701      MOVE '5'                    TO  DC-OPTION-CODE.
00702      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00703      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00704      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00705      MOVE DC-GREG-DATE-1-YMD     TO  SAVE-DATE-YMD.
00706
00707      IF SAVE-DATE-YY > 70
00708          MOVE 19                 TO  SAVE-DATE-CC
00709      ELSE
00710          MOVE 20                 TO  SAVE-DATE-CC.
00711
00712      IF EIBCALEN = 0
00713          GO TO 8800-UNAUTHORIZED-ACCESS.
00714
00715      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00716
00717      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.
00718      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.
00719
00720      MOVE EIBTRMID               TO QID-TERM
00721                                     WS-QID-TERM.
00722
00723      
      * EXEC CICS HANDLE CONDITION
00724 *        QIDERR   (1000-SHOW-CLAIM-HISTORY)
00725 *        MAPFAIL  (0100-FIRST-TIME-IN)
00726 *        NOTOPEN  (8500-FILE-NOTOPEN)
00727 *        PGMIDERR (9600-PGMID-ERROR)
00728 *        ERROR    (9990-ABEND)
00729 *    END-EXEC.
      *    MOVE '"$N?JL.               ! " #00004861' TO DFHEIV0
           MOVE X'22244E3F4A4C2E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034383631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00730
00731      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00732          MOVE PI-CALLING-PROGRAM       TO RETURNED-FROM.
00733
00734      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00735          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00736              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00737              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00738              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00739              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00740              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00741              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00742              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00743              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00744          ELSE
00745              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00746              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00747              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00748              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00749              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00750              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00751              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00752              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00753
00754      IF RETURNED-FROM NOT = SPACES
00755          GO TO 0600-RECOVER-TEMP-STORAGE.
00756
00757      IF EIBAID = DFHCLEAR
00758          GO TO 9400-CLEAR.
00759
00760      IF PI-PROCESSOR-ID = 'LGXX'
00761          NEXT SENTENCE
00762      ELSE
00763          
      * EXEC CICS READQ TS
00764 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00765 *            INTO    (SECURITY-CONTROL)
00766 *            LENGTH  (SC-COMM-LENGTH)
00767 *            ITEM    (SC-ITEM)
00768 *        END-EXEC
      *    MOVE '*$II   L              ''   #00004901' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00769              MOVE SC-CLAIMS-DISPLAY (16)  TO  PI-DISPLAY-CAP
00770              MOVE SC-CLAIMS-UPDATE  (16)  TO  PI-MODIFY-CAP
00771              IF NOT DISPLAY-CAP
00772                  MOVE 'READ'              TO  SM-READ
00773                  PERFORM 9995-SECURITY-VIOLATION
00774                  MOVE ER-0070             TO  EMI-ERROR
00775                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00776                  GO TO 8100-SEND-INITIAL-MAP.
00777
00778      IF EIBTRNID = TRANS-ID
00779          GO TO 0200-RECEIVE.
00780
00781  EJECT
00782  0100-FIRST-TIME-IN.
00783      MOVE LOW-VALUES             TO  EL150CO
00784                                      PI-PROGRAM-WORK-AREA
00785                                      HOLD-RECORDED-DT.
00786
00787      MOVE 'Y'                    TO  PI-FIRST-TIME-SW
00788                                      WS-RECORDS-READ-SW.
00789      MOVE 'F'                    TO  DIRECTION-SWITCH.
00790      MOVE 1                      TO  PI-LINE-NO
00791                                      SUB-2
00792                                      SUB-3.
00793      MOVE ZERO                   TO  WS-MAX-SUB
00794                                      PI-REMINDERS-SW
00795                                      PI-LETTERS-SW
00796                                      PI-PAYMENTS-SW
00797                                      PI-AUTO-PAY-SW
00798                                      PI-NOTES-SW
00799                                      PI-RES-EXP-SW
00800                                      PI-DENIALS-SW
00801                                      PI-INCURRED-DATE-SW
00802                                      PI-FORMS-SW
00803                                      SUB.
00804
00805      PERFORM 7400-DEL-TEMP-STOR-TABLE THRU 7400-EXIT.
00806
00807      
      * EXEC CICS HANDLE CONDITION
00808 *        QIDERR   (0900-BUILD-RELATED-TABLE)
00809 *    END-EXEC.
      *    MOVE '"$N                   ! # #00004945' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034393435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00810
00811      
      * EXEC CICS DELETEQ TS
00812 *        QUEUE(QID)
00813 *    END-EXEC.
      *    MOVE '*&                    #   #00004949' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00814
00815      PERFORM 0900-BUILD-RELATED-TABLE THRU 0999-RELATED-EXIT.
00816
00817      GO TO 1000-SHOW-CLAIM-HISTORY.
00818
00819      EJECT
00820  0200-RECEIVE.
00821      MOVE 'B'                    TO  PASS-SWITCH.
00822      MOVE LOW-VALUES             TO  EL150CI.
00823
00824      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00825          MOVE ER-0008            TO  EMI-ERROR
00826          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00827          MOVE -1                 TO  ENTERPFL
00828          GO TO 8200-SEND-DATAONLY.
00829
00830      
      * EXEC CICS RECEIVE
00831 *        MAP      (MAP-NAME)
00832 *        MAPSET   (MAPSET-NAME)
00833 *        INTO     (EL150CI)
00834 *    END-EXEC.
           MOVE LENGTH OF
            EL150CI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004968' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150CI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00835
00836      IF ENTERPFL = 0
00837          GO TO 0300-CHECK-PFKEYS.
00838
00839      IF EIBAID NOT = DFHENTER
00840          MOVE ER-0004            TO  EMI-ERROR
00841          GO TO 0320-INPUT-ERROR.
00842
00843      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)
00844          MOVE PF-VALUES (ENTERPFI)   TO  EIBAID
00845      ELSE
00846          MOVE ER-0029                TO  EMI-ERROR
00847          GO TO 0320-INPUT-ERROR.
00848
00849  0300-CHECK-PFKEYS.
00850      IF EIBAID = DFHPF23
00851          GO TO 8810-PF23.
00852
00853      IF EIBAID = DFHPF24
00854          GO TO 9200-RETURN-MAIN-MENU.
00855
00856      IF EIBAID = DFHPF12
00857          GO TO 9500-PF12.
00858
00859      IF EIBAID = DFHPF3
00860          IF LINENOL > +0
00861              GO TO 0500-CREATE-TEMP-STORAGE
00862          ELSE
00863              MOVE ER-0672        TO  EMI-ERROR
00864              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00865              MOVE -1             TO  LINENOL
00866              GO TO 8200-SEND-DATAONLY.
00867
041612*    IF EIBAID = DFHPF4
041612*       IF LINENOL > +0
041612*          IF (PI-EL142-PRIORITY = '8')
041612*             AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
041612*                  AND 'AMWA')
041612*             MOVE ER-8003       TO EMI-ERROR
041612*             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041612*             MOVE -1            TO LINENOL
041612*             GO TO 8200-SEND-DATAONLY
041612*          ELSE
041612*             MOVE 'V'           TO WS-VOID-CODE
041612*             GO TO 5000-VOID-PAYMENT
041612*          END-IF
041612*       ELSE
041612*          MOVE ER-0663          TO EMI-ERROR
041612*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041612*          MOVE -1               TO LINENOL
041612*          GO TO 8200-SEND-DATAONLY
041612*       END-IF
041612*    END-IF
041612*
041612*    IF EIBAID = DFHPF5
041612*       IF LINENOL > +0
041612*          IF (PI-EL142-PRIORITY = '8')
041612*             AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
041612*                  AND 'AMWA')
041612*             MOVE ER-8003       TO EMI-ERROR
041612*             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041612*             MOVE -1            TO LINENOL
041612*             GO TO 8200-SEND-DATAONLY
041612*          ELSE
041612*             MOVE 'S'           TO WS-VOID-CODE
041612*             GO TO 5000-VOID-PAYMENT
041612*          END-IF
041612*       ELSE
041612*          MOVE ER-0835          TO EMI-ERROR
041612*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
041612*          MOVE -1               TO LINENOL
041612*          GO TO 8200-SEND-DATAONLY
041612*       END-IF
041612*    END-IF
00798
062602*    IF EIBAID = DFHPF4
00869 *        IF LINENOL > +0
00870 *            MOVE 'V'            TO  WS-VOID-CODE
00871 *            GO TO 5000-VOID-PAYMENT
00872 *        ELSE
00873 *            MOVE ER-0663        TO  EMI-ERROR
00874 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00875 *            MOVE -1             TO  LINENOL
00876 *            GO TO 8200-SEND-DATAONLY.
00877 *
00878 *    IF EIBAID = DFHPF5
00879 *        IF LINENOL > +0
00880 *            MOVE 'S'            TO  WS-VOID-CODE
00881 *            GO TO 5000-VOID-PAYMENT
00882 *        ELSE
00883 *            MOVE ER-0835        TO  EMI-ERROR
00884 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00885 *            MOVE -1             TO  LINENOL
00886 *            GO TO 8200-SEND-DATAONLY.
00887
121802*    IF EIBAID = DFHPF6
121802*        IF PI-COMPANY-ID = 'DMD'
121802*            IF LINENOL NOT = ZEROS
121802*                MOVE LINENOI        TO SUB-1
121802*                IF PI-TRLR-TYPE (SUB-1) = '2'
121802*                    PERFORM 0500-CREATE-TEMP-STORAGE
121802*                    MOVE 'EL402DMD' TO PGM-NAME
121802*                    GO TO 9300-XCTL
121802*                ELSE
121802*                    MOVE ER-0940    TO EMI-ERROR
121802*                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*                    MOVE -1         TO LINENOL
121802*                    GO TO 8200-SEND-DATAONLY
121802*            ELSE
121802*                MOVE ER-0939    TO EMI-ERROR
121802*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*                MOVE -1         TO LINENOL
121802*                GO TO 8200-SEND-DATAONLY.
00906
00907      MOVE SPACES                 TO  ERRMSG1O.
00908
00909      IF EIBAID = DFHPF1
00910          MOVE 'F'                TO  DIRECTION-SWITCH
00911          PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT
00912          GO TO 1000-SHOW-CLAIM-HISTORY.
00913
00914      IF EIBAID = DFHPF2
00915          MOVE 'B'                TO  DIRECTION-SWITCH
00916          PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT
00917          GO TO 1000-SHOW-CLAIM-HISTORY.
00918
00919      IF EIBAID = DFHENTER
00920          GO TO 0330-EDIT-DATA.
00921
00922      MOVE ER-0029                TO EMI-ERROR.
00923
00924  0320-INPUT-ERROR.
00925      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00926
00927      IF ENTERPFL = 0
00928          MOVE -1                 TO ENTERPFL
00929      ELSE
00930          MOVE AL-UNBON           TO ENTERPFA
00931          MOVE -1                 TO ENTERPFL.
00932
00933      GO TO 8200-SEND-DATAONLY.
00934
00935  0330-EDIT-DATA.
00936      IF NOT MODIFY-CAP
00937          MOVE 'UPDATE'           TO  SM-READ
00938          PERFORM 9995-SECURITY-VIOLATION
00939          MOVE ER-0070            TO  EMI-ERROR
00940          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00941          GO TO 8100-SEND-INITIAL-MAP.
00942
00943      IF LINENOL  > 0 AND
00944         RECVDTL  > 0 AND
00945         RECVTYPL > 0
00946             GO TO 4000-RECEIVE-FORMS.
00947
00948      IF LINENOL > 0 AND
00949         RECVDTL > 0
00950             GO TO 3000-RECEIVE-LETTERS.
00951
00952      IF LINENOL > 0
00953          MOVE LINENOI            TO  SUB-1
00954          IF PI-TRLR-TYPE (SUB-1) = 'A'
00955              MOVE ER-0665        TO  EMI-ERROR
00956              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00957              MOVE -1             TO  LINENOL
00958              GO TO 8200-SEND-DATAONLY
00959          ELSE
00960          IF PI-TRLR-TYPE (SUB-1) = '4'
00961              MOVE ER-0666        TO  EMI-ERROR
00962              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00963              MOVE -1             TO  LINENOL
00964              GO TO 8200-SEND-DATAONLY
00965          ELSE
00966          IF PI-TRLR-TYPE (SUB-1) = '2'
00967              MOVE ER-0667    TO  EMI-ERROR
00968              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00969              MOVE -1         TO  LINENOL
00970              GO TO 8200-SEND-DATAONLY.
00971
00972      MOVE 'F'                    TO  DIRECTION-SWITCH.
00973      PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT.
00974      GO TO 1000-SHOW-CLAIM-HISTORY.
00975
00976      EJECT
00977  0500-CREATE-TEMP-STORAGE.
00978      MOVE EIBCPOSN               TO  PI-SAVE-CURSOR.
00979      MOVE SPACES                 TO PI-FULL-SCREEN-IND.
00980
00981      
      * EXEC CICS WRITEQ TS
00982 *        QUEUE    (QID)
00983 *        FROM     (PROGRAM-INTERFACE-BLOCK)
00984 *        LENGTH   (PI-COMM-LENGTH)
00985 *    END-EXEC.
      *    MOVE '*"     L              ''   #00005161' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00986
00987      IF LINENOL > +0
00988          MOVE LINENOI            TO  SUB-1
00989          IF PI-TRLR-TYPE (SUB-1) = '2'
00990             MOVE +1                      TO  PI-PAYMENTS-SW
00991             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
00992                                              PI-SAVE-ATK-SEQ-NO
00993          ELSE
00994          IF PI-TRLR-TYPE (SUB-1) = '3'
00995             MOVE +1                      TO  PI-AUTO-PAY-SW
00996             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
00997                                              PI-SAVE-ATK-SEQ-NO
00998          ELSE
00999          IF PI-TRLR-TYPE (SUB-1) = '4'
01000             MOVE +1                      TO  PI-LETTERS-SW
01001             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
01002                                              PI-SAVE-ATK-SEQ-NO
01003          ELSE
01004          IF PI-TRLR-TYPE (SUB-1) = '6'
01005             MOVE +1                      TO  PI-NOTES-SW
01006             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
01007                                              PI-SAVE-ATK-SEQ-NO
01008          ELSE
01009          IF PI-TRLR-TYPE (SUB-1) = '7'
01010             MOVE +1                      TO  PI-REMINDERS-SW
01011             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
01012                                              PI-SAVE-ATK-SEQ-NO
01013          ELSE
01014          IF PI-TRLR-TYPE (SUB-1) = '8'
01015             MOVE +1                      TO  PI-DENIALS-SW
01016             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
01017                                              PI-SAVE-ATK-SEQ-NO
01018          ELSE
01019          IF PI-TRLR-TYPE (SUB-1) = '9'
01020             MOVE +1                      TO  PI-INCURRED-DATE-SW
01021             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
01022                                              PI-SAVE-ATK-SEQ-NO
01023          ELSE
01024             MOVE +1                      TO  PI-FORMS-SW
01025             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
01026                                              PI-SAVE-ATK-SEQ-NO
01027      ELSE
01028          MOVE +1                         TO  PI-REMINDERS-SW
01029                                              PI-LETTERS-SW
01030                                              PI-PAYMENTS-SW
01031                                              PI-AUTO-PAY-SW
01032                                              PI-NOTES-SW
01033                                              PI-RES-EXP-SW
01034                                              PI-DENIALS-SW
01035                                              PI-INCURRED-DATE-SW
01036                                              PI-FORMS-SW
01037          MOVE +0                         TO  PI-ATK-SEQ-NO.
01038
01039      IF EIBAID = DFHPF6
01040          MOVE PI-COMPANY-CD      TO  PI-SAVE-ATK-COMPANY-CODE
01041                                      PI-ATK-COMPANY-CODE
01042          MOVE PI-CARRIER         TO  PI-SAVE-ATK-CARRIER
01043                                      PI-ATK-CARRIER
01044          MOVE PI-CLAIM-NO        TO  PI-SAVE-ATK-CLAIM-NO
01045                                      PI-ATK-CLAIM-NO
01046          MOVE PI-TRLR-CERT (SUB-1) TO PI-SAVE-ATK-CERT-NO
01047                                       PI-ATK-CERT-NO
01048                                       PI-CERT-NO
01049          MOVE 'Y'                TO  PI-FIRST-TIME-SW.
01050
01051      IF EIBAID = DFHPF3
01052          MOVE XCTL-142           TO  PGM-NAME
01053          MOVE 'EL142A'           TO  PI-MAP-NAME
01054          MOVE PI-COMPANY-CD      TO  PI-SAVE-ATK-COMPANY-CODE
01055                                      PI-ATK-COMPANY-CODE
01056          MOVE PI-CARRIER         TO  PI-SAVE-ATK-CARRIER
01057                                      PI-ATK-CARRIER
01058          MOVE PI-CLAIM-NO        TO  PI-SAVE-ATK-CLAIM-NO
01059                                      PI-ATK-CLAIM-NO
01060          MOVE PI-TRLR-CERT (SUB-1)   TO  PI-SAVE-ATK-CERT-NO
01061                                          PI-ATK-CERT-NO
01062                                          PI-CERT-NO
01063          MOVE 'Y'                TO  PI-FIRST-TIME-SW
01064          GO TO 9300-XCTL.
01065
01066      EJECT
01067  0600-RECOVER-TEMP-STORAGE.
01068      MOVE PI-CONTROL-IN-PROGRESS TO SAVE-CONTROL.
01069
01070      
      * EXEC CICS HANDLE CONDITION
01071 *        QIDERR   (0690-QIDERR)
01072 *    END-EXEC.
      *    MOVE '"$N                   ! $ #00005250' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035323530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01073
01074      
      * EXEC CICS READQ TS
01075 *        QUEUE    (QID)
01076 *        INTO     (PROGRAM-INTERFACE-BLOCK)
01077 *        LENGTH   (PI-COMM-LENGTH)
01078 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00005254' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01079
01080      
      * EXEC CICS DELETEQ TS
01081 *        QUEUE   (QID)
01082 *    END-EXEC.
      *    MOVE '*&                    #   #00005260' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01083
01084      MOVE SAVE-CONTROL           TO  PI-CONTROL-IN-PROGRESS.
01085      MOVE 'F'                    TO  DIRECTION-SWITCH.
01086
01087      PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT.
01088      GO TO 1000-SHOW-CLAIM-HISTORY.
01089
01090  0690-QIDERR.
01091      MOVE ER-0033                TO  EMI-ERROR.
01092      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01093      MOVE LOW-VALUES             TO  EL150CO.
01094      MOVE -1                     TO  ENTERPFL.
01095      GO TO 8100-SEND-INITIAL-MAP.
01096
01097      EJECT
01098  0700-CREATE-TEMP-STORAGE.
01099
01100      
      * EXEC CICS WRITEQ TS
01101 *        QUEUE    (WS-TABLE-QID)
01102 *        FROM     (WS-SORTED-TABLE)
01103 *        LENGTH   (WS-TABLE-LENGTH)
01104 *        ITEM     (WS-ITEM-COUNT)
01105 *    END-EXEC.
      *    MOVE '*" I   L              ''   #00005280' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TABLE-QID, 
                 WS-SORTED-TABLE, 
                 WS-TABLE-LENGTH, 
                 WS-ITEM-COUNT, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01106
01107  0700-EXIT.
01108      EXIT.
01109
01110      EJECT
01111  0800-RECOVER-TEMP-STORAGE.
01112
01113      
      * EXEC CICS HANDLE CONDITION
01114 *        QIDERR   (0890-QIDERR)
01115 *    END-EXEC.
      *    MOVE '"$N                   ! % #00005293' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035323933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01116
01117      
      * EXEC CICS READQ TS
01118 *        QUEUE    (WS-TABLE-QID)
01119 *        INTO     (WS-SORTED-TABLE)
01120 *        LENGTH   (WS-TABLE-LENGTH)
01121 *        ITEM     (WS-ITEM-COUNT)
01122 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00005297' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TABLE-QID, 
                 WS-SORTED-TABLE, 
                 WS-TABLE-LENGTH, 
                 WS-ITEM-COUNT, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01123
01124      GO TO 0899-EXIT.
01125
01126  0890-QIDERR.
01127      MOVE ER-0033                TO  EMI-ERROR.
01128      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01129      MOVE LOW-VALUES             TO  EL150CO.
01130      MOVE -1                     TO  ENTERPFL.
01131      GO TO 8100-SEND-INITIAL-MAP.
01132
01133  0899-EXIT.
01134      EXIT.
01135
01136      EJECT
01137  0900-BUILD-RELATED-TABLE.
01138
01139      
      * EXEC CICS HANDLE CONDITION
01140 *        ENDFILE   (0950-SORT-ROUTINE)
01141 *    END-EXEC.
      *    MOVE '"$''                   ! & #00005319' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035333139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01142
01143      MOVE LOW-VALUES             TO  ELMSTR-KEY.
01144      MOVE PI-COMPANY-CD          TO  MSTR-COMP-CD.
01145      MOVE PI-CARRIER             TO  MSTR-CARRIER.
01146      MOVE PI-CLAIM-NO            TO  MSTR-CLAIM-NO.
01147
01148      
      * EXEC CICS STARTBR
01149 *        DATASET   ('ELMSTR')
01150 *        RIDFLD    (ELMSTR-KEY)
01151 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005328' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01152
01153  0910-READNEXT-CLAIM-MASTER.
01154
01155      
      * EXEC CICS HANDLE CONDITION
01156 *        ENDFILE   (0915-ELMSTR-ENDFILE)
01157 *    END-EXEC.
      *    MOVE '"$''                   ! '' #00005335' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035333335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01158
01159      
      * EXEC CICS READNEXT
01160 *        DATASET   ('ELMSTR')
01161 *        RIDFLD    (ELMSTR-KEY)
01162 *        SET       (ADDRESS OF CLAIM-MASTER)
01163 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005339' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01164
01165      IF PI-FIRST-TIME-SW = 'Y'
01166          MOVE 'N'                        TO  PI-FIRST-TIME-SW
01167          MOVE CL-CLAIM-NO                TO  PI-PREV-CLAIM-NO
01168          MOVE CL-CARRIER                 TO  PI-PREV-CARRIER
01169          IF CL-ASSOC-CERT-TOTAL > +1
01170              MOVE CL-PRIME-CERT-PRIME    TO  PI-PRIME-CERT
01171              MOVE CL-PRIME-CERT-SFX      TO  PI-PRIME-SUFX
01172              MOVE 'PRIME CERT :'         TO  PI-PRIME-HDG
01173              MOVE AL-SABON               TO  PRIMEHDA
01174          ELSE
01175              MOVE PI-CERT-NO             TO  PI-PRIME-CERT
01176              MOVE PI-CERT-SFX            TO  PI-PRIME-SUFX
01177              MOVE 'CERT NO/SFX:'         TO  PI-PRIME-HDG
01178              MOVE AL-SABON               TO  PRIMEHDA.
01179
01180      IF CL-CLAIM-NO = PI-PREV-CLAIM-NO AND
01181         CL-CARRIER  = PI-PREV-CARRIER
01182          NEXT SENTENCE
01183      ELSE
01184          MOVE HIGH-VALUES        TO  WS-RECORDED-DT (SUB-2)
01185          MOVE 'Y'                TO  PI-FIRST-TIME-SW
01186          MOVE SUB-2              TO  WS-MAX-SUB
01187                                      PI-MAX-SUB
01188          MOVE 1                  TO  SUB-2 SUB-3
01189          GO TO 0950-SORT-ROUTINE.
01190
01191      IF SUB-2 > 200
01192          MOVE 200                TO  WS-MAX-SUB
01193                                      PI-MAX-SUB
01194          MOVE 'Y'                TO  PI-FIRST-TIME-SW
01195          MOVE 1                  TO  SUB-2 SUB-3
01196          GO TO 0950-SORT-ROUTINE.
01197
01198      MOVE LOW-VALUES             TO  ELTRLR-KEY.
01199      MOVE CL-COMPANY-CD          TO  TRLR-COMP-CD.
01200      MOVE CL-CARRIER             TO  TRLR-CARRIER.
01201      MOVE CL-CLAIM-NO            TO  TRLR-CLAIM-NO.
01202      MOVE CL-CERT-NO             TO  TRLR-CERT-NO.
01203      MOVE ZEROS                  TO  TRLR-SEQ-NO.
01204
01205      PERFORM 0925-BUILD-TRAILER-TABLE THRU 0949-BUILD-TABLE-EXIT.
01206      GO TO 0910-READNEXT-CLAIM-MASTER.
01207
01208  0915-ELMSTR-ENDFILE.
01209
01210      
      * EXEC CICS ENDBR
01211 *        DATASET   ('ELMSTR')
01212 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005390' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01213
01214      MOVE HIGH-VALUES            TO  WS-RECORDED-DT (SUB-2).
01215      MOVE 'Y'                    TO  PI-FIRST-TIME-SW.
01216      MOVE SUB-2                  TO  WS-MAX-SUB
01217                                      PI-MAX-SUB.
01218      MOVE 1                      TO  SUB-2   SUB-3.
01219      GO TO 0950-SORT-ROUTINE.
01220
01221      EJECT
01222  0925-BUILD-TRAILER-TABLE.
01223
01224      
      * EXEC CICS HANDLE CONDITION
01225 *        ENDFILE   (0945-ENDBR-ELTRLR)
01226 *    END-EXEC.
      *    MOVE '"$''                   ! ( #00005404' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303035343034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01227
01228      
      * EXEC CICS STARTBR
01229 *        DATASET   ('ELTRLR')
01230 *        RIDFLD    (ELTRLR-KEY)
01231 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005408' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01232
01233  0935-READNEXT-TRAILER.
01234
01235      
      * EXEC CICS READNEXT
01236 *        DATASET   ('ELTRLR')
01237 *        RIDFLD    (ELTRLR-KEY)
01238 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
01239 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005415' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01240
01241      IF AT-COMPANY-CD NOT = CL-COMPANY-CD OR
01242         AT-CARRIER    NOT = CL-CARRIER    OR
01243         AT-CLAIM-NO   NOT = CL-CLAIM-NO   OR
01244         AT-CERT-NO    NOT = CL-CERT-NO
01245          GO TO 0945-ENDBR-ELTRLR.
01246
01247      MOVE 'Y'                    TO  WS-ENDBR-SW.
01248
01249      IF TRLR-SEQ-NO = 90
01250          GO TO 0935-READNEXT-TRAILER.
01251
01252      IF RESERVE-EXPENSE-TR OR ADDRESS-TR
01253          GO TO 0935-READNEXT-TRAILER.
01254
01255      IF GENERAL-INFO-TR
052113         IF AT-PAYMENT-NOTE or at-errors-note
01257              GO TO 0935-READNEXT-TRAILER.
01258
01259      IF AUTO-PROMPT-TR
01260          IF SAVE-BIN-DATE > AT-PROMPT-END-DT
01261              GO TO 0935-READNEXT-TRAILER.
01262
01263      MOVE AT-CERT-NO             TO  WS-CERT-NO     (SUB-2).
01264      MOVE AT-SEQUENCE-NO         TO  WS-TRLR-SEQ    (SUB-2).
01265      MOVE AT-RECORDED-DT         TO  WS-RECORDED-DT (SUB-2).
01266
01267      ADD +1 TO SUB-2.
01268
01269      IF SUB-2 > 200
01270          GO TO 0945-ENDBR-ELTRLR.
01271
01272      GO TO 0935-READNEXT-TRAILER.
01273
01274  0945-ENDBR-ELTRLR.
01275
01276      IF WS-ENDBR-SW = 'N'
01277          GO TO 0949-BUILD-TABLE-EXIT.
01278
01279      
      * EXEC CICS ENDBR
01280 *        DATASET   ('ELTRLR')
01281 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005459' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01282
01283  0949-BUILD-TABLE-EXIT.
01284      EXIT.
01285
01286      EJECT
01287  0950-SORT-ROUTINE.
01288      IF PI-FIRST-TIME-SW = 'Y'
01289          MOVE 'N'                        TO  PI-FIRST-TIME-SW
01290          IF WS-SORTED-SW (SUB-2) = 'Y'
01291              ADD 1 TO SUB-2
01292              IF (SUB-3 > 200 OR
01293                  SUB-3 > WS-MAX-SUB)
01294                  MOVE 'Y'                    TO  PI-FIRST-TIME-SW
01295                  PERFORM 0700-CREATE-TEMP-STORAGE THRU 0700-EXIT
01296                  GO TO 0990-SORT-EXIT
01297              ELSE
01298                  MOVE 'Y'                    TO  PI-FIRST-TIME-SW
01299                  GO TO 0950-SORT-ROUTINE
01300          ELSE
01301              MOVE WS-RECORDED-DT (SUB-2) TO  HOLD-RECORDED-DT
01302              MOVE SUB-2                  TO  HOLD-SUB
01303              GO TO 0950-SORT-ROUTINE
01304      ELSE
01305          ADD 1 TO SUB-2
01306          IF SUB-3 > 200
01307              MOVE 'Y'                    TO  PI-FIRST-TIME-SW
01308              PERFORM 0700-CREATE-TEMP-STORAGE THRU 0700-EXIT
01309              GO TO 0990-SORT-EXIT.
01310
01311      IF WS-SORTED-SW (SUB-2) = 'Y'
01312          GO TO 0950-SORT-ROUTINE.
01313
01314      IF SUB-3 > WS-MAX-SUB
01315          MOVE 'Y'            TO  PI-FIRST-TIME-SW
01316          PERFORM 0700-CREATE-TEMP-STORAGE THRU 0700-EXIT
01317          GO TO 0990-SORT-EXIT.
01318
01319      IF WS-RECORDED-DT (SUB-2) = HIGH-VALUES OR
01320         SUB-2 > WS-MAX-SUB
01321          MOVE WS-RECORDED-DT (HOLD-SUB) TO
01322                                   WS-SRTD-RECORDED-DT (SUB-3)
01323          MOVE WS-CERT-NO     (HOLD-SUB) TO
01324                                   WS-SRTD-CERT        (SUB-3)
01325          MOVE WS-TRLR-SEQ    (HOLD-SUB) TO
01326                                   WS-SRTD-TRLR-SEQ    (SUB-3)
01327          MOVE 'Y'            TO WS-SORTED-SW       (HOLD-SUB)
01328                                 PI-FIRST-TIME-SW
01329          ADD 1               TO SUB-3
01330          MOVE 1              TO SUB-2
01331          GO TO 0950-SORT-ROUTINE.
01332
01333      IF WS-RECORDED-DT (SUB-2) > HOLD-RECORDED-DT
01334          MOVE WS-RECORDED-DT (SUB-2) TO  HOLD-RECORDED-DT
01335          MOVE SUB-2                  TO  HOLD-SUB
01336          GO TO 0950-SORT-ROUTINE.
01337
01338      GO TO 0950-SORT-ROUTINE.
01339
01340  0990-SORT-EXIT.
01341      EXIT.
01342
01343  0999-RELATED-EXIT.
01344      EXIT.
01345
01346      EJECT
01347  1000-SHOW-CLAIM-HISTORY.
01348
01349      IF DIRECTION-SWITCH = 'F'
01350          IF FIRST-TIME
01351              PERFORM 2070-INIT-SCREEN-AREA THRU 2070-EXIT
01352              MOVE 'N'                TO  PI-FIRST-TIME-SW
01353              MOVE +1                 TO  DISPLAY-CNT
01354                                          SUB-1
01355                                          SUB-2
01356          ELSE
01357              MOVE PI-TRLR-SUB (8)    TO  SUB-2
01358              ADD +1                  TO  SUB-2
01359              IF SUB-2 > PI-MAX-SUB
01360                  GO TO 8200-SEND-DATAONLY
01361              ELSE
01362                  PERFORM 2070-INIT-SCREEN-AREA THRU 2070-EXIT
01363                  MOVE +1                 TO  DISPLAY-CNT
01364                                              SUB-1
01365                                              PI-LINE-NO
01366                  IF SUB-2 = +9
01367                     IF WS-SRTD-CERT (SUB-2) = LOW-VALUES
01368                      MOVE +1             TO  SUB-2.
01369
01370      IF DIRECTION-SWITCH = 'B'
01371          IF FIRST-TIME
01372              GO TO 8200-SEND-DATAONLY
01373          ELSE
01374              PERFORM 2070-INIT-SCREEN-AREA THRU 2070-EXIT
01375              MOVE PI-TRLR-SUB (1)    TO  SUB-2
01376              IF SUB-2 > +8
01377                  SUBTRACT +1        FROM SUB-2
01378                  MOVE +15            TO  DISPLAY-CNT
01379                  MOVE +8             TO  SUB-1
01380                                          PI-LINE-NO
01381              ELSE
01382                  MOVE 'F'            TO  DIRECTION-SWITCH
01383                  MOVE +1             TO  DISPLAY-CNT
01384                                          SUB-1
01385                                          SUB-2
01386                                          PI-LINE-NO.
01387
01388  1010-BUILD-HISTORY-SCREEN.
01389
01390      PERFORM 2000-BUILD-TRAILER-DISPLAY THRU 2999-EXIT.
01391
01392      MOVE PI-CARRIER             TO  CARRO.
01393      MOVE PI-CLAIM-NO            TO  CLMNOO.
01394
01395      MOVE -1                     TO  LINENOL.
01396
01397      IF RECORDS-READ
01398          MOVE 'N'                TO  WS-RECORDS-READ-SW
01399          GO TO 8100-SEND-INITIAL-MAP
01400      ELSE
01401          GO TO 8200-SEND-DATAONLY.
01402
01403      EJECT
01404  2000-BUILD-TRAILER-DISPLAY.
01405
01406      IF WS-SRTD-CERT (SUB-2) = LOW-VALUES OR
01407         SUB-2 > PI-MAX-SUB
01408          GO TO 2999-EXIT.
01409
01410      MOVE PI-COMPANY-CD              TO  TRLR-COMP-CD.
01411      MOVE PI-CARRIER                 TO  TRLR-CARRIER.
01412      MOVE PI-CLAIM-NO                TO  TRLR-CLAIM-NO.
01413      MOVE WS-SRTD-CERT (SUB-2)       TO  TRLR-CERT-NO.
01414      MOVE WS-SRTD-TRLR-SEQ (SUB-2)   TO  TRLR-SEQ-NO.
01415      MOVE 'TRLR'                     TO  FILE-SWITCH.
01416
01417  2020-BROWSE-FORWARD.
01418
01419      
      * EXEC CICS HANDLE CONDITION
01420 *        ENDFILE   (2950-NO-MORE-TRAILERS)
01421 *        NOTFND    (2950-NO-MORE-TRAILERS)
01422 *    END-EXEC.
      *    MOVE '"$''I                  ! ) #00005599' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035353939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01423
01424      
      * EXEC CICS READ
01425 *        DATASET   ('ELTRLR')
01426 *        RIDFLD    (ELTRLR-KEY)
01427 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
01428 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (   #00005604' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01429
01430      MOVE 'Y'                    TO  WS-RECORDS-READ-SW.
01431      GO TO 2090-DISPLAY-TRAILER.
01432
01433  EJECT
01434  2070-INIT-SCREEN-AREA.
01435
01436      MOVE SPACES                 TO  TEXT-WORK-AREAS
01437                                      MAP-HDG (1)   MAP-TEXT (1)
01438                                      MAP-HDG (2)   MAP-TEXT (2)
01439                                      MAP-HDG (3)   MAP-TEXT (3)
01440                                      MAP-HDG (4)   MAP-TEXT (4)
01441                                      MAP-HDG (5)   MAP-TEXT (5)
01442                                      MAP-HDG (6)   MAP-TEXT (6)
01443                                      MAP-HDG (7)   MAP-TEXT (7)
01444                                      MAP-HDG (8)   MAP-TEXT (8)
01445                                      MAP-HDG (9)   MAP-TEXT (9)
01446                                      MAP-HDG (10)  MAP-TEXT (10)
01447                                      MAP-HDG (11)  MAP-TEXT (11)
01448                                      MAP-HDG (12)  MAP-TEXT (12)
01449                                      MAP-HDG (13)  MAP-TEXT (13)
01450                                      MAP-HDG (14)  MAP-TEXT (14)
01451                                      MAP-HDG (15)  MAP-TEXT (15)
01452                                      MAP-HDG (16)  MAP-TEXT (16).
01453
01454      MOVE ZEROS                  TO  PMT-AMT-PAID
01455                                      AUTO-1ST-AMT
01456                                      AUTO-REG-AMT
01457                                      INCUR-TOT-PD.
01458
01459  2070-EXIT.
01460      EXIT.
01461
01462  EJECT
01463  2090-DISPLAY-TRAILER.
01464
01465      IF PAYMENT-TR
01466          GO TO 2100-PAYMENT-TRAILER.
01467
01468      IF AUTO-PAY-TR
01469         GO TO 2200-AUTO-PAYMENT-TRAILER.
01470
01471      IF CORRESPONDENCE-TR
01472          GO TO 2300-CORRESPONDENCE-TRAILER.
01473
01474      IF GENERAL-INFO-TR
01475          GO TO 2400-GENERAL-INFO-TRAILER.
01476
01477      IF AUTO-PROMPT-TR
01478          GO TO 2500-AUTO-PROMPT-TRAILER.
01479
01480      IF DENIAL-TR
01481          GO TO 2600-DENIAL-TRAILER.
01482
01483      IF INCURRED-CHG-TR
01484          GO TO 2700-INCURRED-CHANGE-TRAILER.
01485
01486      IF FORM-CONTROL-TR
01487          GO TO 2710-FORM-CONTROL-TRAILER.
01488
01489      GO TO 2020-BROWSE-FORWARD.
01490
01491      EJECT
01492  2100-PAYMENT-TRAILER.
01493      MOVE SPACES                 TO  PMT-HDG1.
01494      MOVE ZEROS                  TO  PMT-AMT-PAID.
01495      MOVE PI-LINE-NO             TO  PMT-LINE-NO.
01496      MOVE 'PAYMENT '             TO  PMT-HDG1-LIT.
01497
01498      MOVE SPACES                 TO  PMT-TEXT-1.
01499      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.
01500      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.
01501      MOVE WS-FORMAT-CERT-NO      TO  PMT-CERT-NO.
01502
01503      MOVE ' TYPE: '              TO  PMT-TYPE-LIT.
01504
022106     IF TRANSFER
022106        MOVE 'TRANSFR'           TO PMT-TYPE
022106        GO TO 2100-CONT
022106     ELSE
022106        IF AT-PAYMENT-TYPE = 'I'
022106           MOVE 'INTEREST'       TO PMT-TYPE
022106           GO TO 2100-CONT
022106        END-IF
022106     END-IF
01508
01509      IF AT-CV-PMT-CODE = ' '
01510          MOVE AT-PAYMENT-TYPE            TO  WS-SUB
01511          IF WS-SUB < 1 OR > 9
01512              MOVE 2                      TO  WS-SUB
01513              MOVE PAY-DESC (WS-SUB)      TO  PMT-TYPE
01514          ELSE
01515              MOVE PAY-DESC (WS-SUB)      TO  PMT-TYPE
01516      ELSE
01517          MOVE AT-CV-PMT-CODE             TO  WS-SUB
01518          IF WS-SUB < 1 OR > 8
01519              MOVE 1                      TO  WS-SUB
01520              MOVE CV-PAY-DESC (WS-SUB)   TO  PMT-TYPE
01521          ELSE
01522              MOVE CV-PAY-DESC (WS-SUB)   TO  PMT-TYPE.
01523
01524  2100-CONT.
01525
013017     if at-ach-payment = 'Y'
013017        move ' ACH PAYMT '       to pmt-check-no-lit
              move 'ACH PMNT'          to PMT-HDG1-LIT
013017     else
013017        MOVE ' CHECK NO: '       TO  PMT-CHECK-NO-LIT
013017     end-if
01526 *    MOVE ' CHECK NO: '          TO  PMT-CHECK-NO-LIT.
01527      MOVE AT-CHECK-NO            TO  PMT-CHECK-NO.
01528
01529      MOVE ' AMT:'                TO  PMT-AMT-PD-LIT.
01530      MOVE AT-AMOUNT-PAID         TO  PMT-AMT-PAID.
01531      MOVE ' BY: '                TO  PMT-REC-BY-LIT.
01532      MOVE AT-RECORDED-BY         TO  PMT-REC-BY.
01533
01534      MOVE SPACES                 TO  PMT-HDG2
01535                                      PMT-TEXT-2.
01536      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
01537      MOVE ' '                    TO  DC-OPTION-CODE.
01538      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
01539      IF NO-CONVERSION-ERROR
01540          MOVE DC-GREG-DATE-1-EDIT    TO  PMT-REC-DT
01541      ELSE
01542          MOVE SPACES                 TO  PMT-REC-DT.
01543
01544      IF AT-PAID-FROM-DT = LOW-VALUES OR SPACES
01545          MOVE SPACES                     TO  PMT-PAID-FROM
01546                                              PMT-FROM-LIT
01547      ELSE
01548          MOVE 'FROM: '                   TO  PMT-FROM-LIT
01549          MOVE AT-PAID-FROM-DT            TO  DC-BIN-DATE-1
01550          MOVE ' '                        TO  DC-OPTION-CODE
01551          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01552          IF NO-CONVERSION-ERROR
01553              MOVE DC-GREG-DATE-1-EDIT    TO PMT-PAID-FROM
01554          ELSE
01555              MOVE SPACES                 TO  PMT-PAID-FROM.
01556
01557      IF AT-PAID-THRU-DT = LOW-VALUES OR SPACES
01558          MOVE SPACES                     TO  PMT-PAID-THRU
01559                                              PMT-THRU-LIT
01560      ELSE
01561          IF PI-USES-PAID-TO
01562              MOVE ' PAID TO: '           TO  PMT-THRU-LIT
01563              MOVE AT-PAID-THRU-DT        TO  DC-BIN-DATE-1
01564              MOVE '6'                    TO  DC-OPTION-CODE
01565              MOVE +1                     TO  DC-ELAPSED-DAYS
01566              MOVE +0                     TO  DC-ELAPSED-MONTHS
01567              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01568              IF NO-CONVERSION-ERROR
01569                  MOVE DC-GREG-DATE-1-EDIT TO PMT-PAID-THRU
01570              ELSE
01571                  MOVE SPACES             TO  PMT-PAID-THRU
01572          ELSE
01573              MOVE ' PD THRU: '           TO  PMT-THRU-LIT
01574              MOVE AT-PAID-THRU-DT        TO  DC-BIN-DATE-1
01575              MOVE ' '                    TO  DC-OPTION-CODE
01576              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01577              IF NO-CONVERSION-ERROR
01578                  MOVE DC-GREG-DATE-1-EDIT TO PMT-PAID-THRU
01579              ELSE
01580                  MOVE SPACES             TO  PMT-PAID-THRU.
01581
01582      IF INSURED-PAID
01583          MOVE 'INS '             TO  PMT-PAYEE.
01584      IF BENEFICIARY-PAID
01585          MOVE 'BENE'             TO  PMT-PAYEE.
01586      IF ACCOUNT-PAID
01587          MOVE 'ACCT'             TO  PMT-PAYEE.
01588      IF OTHER-1-PAID
01589          MOVE 'OTH1'             TO  PMT-PAYEE.
01590      IF OTHER-2-PAID
01591          MOVE 'OTH2'             TO  PMT-PAYEE.
01592      IF DOCTOR-PAID
01593          MOVE 'DOC '             TO  PMT-PAYEE.
01594      IF EMPLOYER-PAID
01595          MOVE 'EMP '             TO  PMT-PAYEE.
01596
01597      IF AT-VOID-DT = LOW-VALUES OR SPACES
01598          NEXT SENTENCE
01599      ELSE
01600          IF AT-VOID-TYPE = 'S'
01601              MOVE ' ST PAY: '            TO  PMT-VOID-LIT
01602          ELSE
01603              MOVE ' VOIDED: '            TO  PMT-VOID-LIT.
01604
01605      IF AT-VOID-DT = LOW-VALUES OR SPACES
01606          MOVE SPACES                     TO  PMT-VOID-DT
01607                                              PMT-VOID-LIT
01608      ELSE
01609          MOVE AT-VOID-DT                 TO  DC-BIN-DATE-1
01610          MOVE ' '                        TO  DC-OPTION-CODE
01611          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01612          IF NO-CONVERSION-ERROR
01613              MOVE DC-GREG-DATE-1-EDIT    TO  PMT-VOID-DT
01614          ELSE
01615              MOVE SPACES                 TO  PMT-VOID-DT.
01616
01617      MOVE PMT-HDG1               TO  MAP-HDG        (DISPLAY-CNT).
01618      MOVE PMT-TEXT-1             TO  MAP-TEXT       (DISPLAY-CNT).
01619      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
01620      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
01621      ADD +1 TO DISPLAY-CNT.
01622      MOVE PMT-HDG2               TO  MAP-HDG        (DISPLAY-CNT).
01623      MOVE PMT-TEXT-2             TO  MAP-TEXT       (DISPLAY-CNT).
01624      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
01625                                      MAP-TEXT-ATTRB (DISPLAY-CNT).
01626      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
01627      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
01628      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
01629      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).
01630      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).
01631
01632      GO TO 2800-INCR-DISPLAY-CNT.
01633
01634      EJECT
01635  2200-AUTO-PAYMENT-TRAILER.
01636      MOVE SPACES                 TO  AUTO-PMT-HDG1.
01637      MOVE PI-LINE-NO             TO  AUTO-LINE-NO.
01638      MOVE 'AUTO PMT'             TO  AUTO-HDG1-LIT.
01639
01640      MOVE SPACES                 TO  AUTO-PMT-TEXT1.
01641      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.
01642      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.
01643      MOVE WS-FORMAT-CERT-NO      TO  AUTO-CERT-NO.
01644
01645      IF AT-SCHEDULE-START-DT = LOW-VALUES OR SPACES
01646          MOVE SPACES                     TO  AUTO-EFF-DT
01647                                              AUTO-EFF-DT-LIT
01648      ELSE
01649          MOVE ' EFF : '                  TO  AUTO-EFF-DT-LIT
01650          MOVE AT-SCHEDULE-START-DT       TO  DC-BIN-DATE-1
01651          MOVE ' '                        TO  DC-OPTION-CODE
01652          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01653          IF NO-CONVERSION-ERROR
01654              MOVE DC-GREG-DATE-1-EDIT    TO  AUTO-EFF-DT
01655          ELSE
01656              MOVE SPACES                 TO  AUTO-EFF-DT.
01657
01658      IF AT-1ST-PAY-THRU-DT = LOW-VALUES OR SPACES
01659          MOVE SPACES                     TO  AUTO-1ST-PMT-DT
01660                                              AUTO-1ST-PMT-LIT
01661      ELSE
01662          MOVE ' 1ST PMT: '               TO  AUTO-1ST-PMT-LIT
01663          IF PI-USES-PAID-TO
01664              MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1
01665              MOVE '6'                    TO  DC-OPTION-CODE
01666              MOVE +1                     TO  DC-ELAPSED-DAYS
01667              MOVE +0                     TO  DC-ELAPSED-MONTHS
01668              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01669              IF NO-CONVERSION-ERROR
01670                  MOVE DC-GREG-DATE-1-EDIT TO AUTO-1ST-PMT-DT
01671              ELSE
01672                  MOVE SPACES             TO  AUTO-1ST-PMT-DT
01673          ELSE
01674              MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1
01675              MOVE ' '                    TO  DC-OPTION-CODE
01676              MOVE +0                     TO  DC-ELAPSED-DAYS
01677                                              DC-ELAPSED-MONTHS
01678              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01679              IF NO-CONVERSION-ERROR
01680                  MOVE DC-GREG-DATE-1-EDIT TO AUTO-1ST-PMT-DT
01681              ELSE
01682                  MOVE SPACES             TO  AUTO-1ST-PMT-DT.
01683
01684      MOVE ' AMT: '               TO  AUTO-1ST-AMT-LIT.
01685      MOVE AT-FIRST-PMT-AMT       TO  AUTO-1ST-AMT.
01686
01687      MOVE ' BY: '                TO  AUTO-REC-BY-LIT.
01688      MOVE AT-RECORDED-BY         TO  AUTO-REC-BY.
01689
01690      MOVE SPACES                 TO  AUTO-PMT-HDG2
01691                                      AUTO-PMT-TEXT2.
01692      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
01693      MOVE ' '                    TO  DC-OPTION-CODE.
01694      MOVE +0                     TO  DC-ELAPSED-DAYS
01695                                      DC-ELAPSED-MONTHS.
01696      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
01697      IF NO-CONVERSION-ERROR
01698          MOVE DC-GREG-DATE-1-EDIT    TO  AUTO-REC-DT
01699      ELSE
01700          MOVE SPACES                 TO  AUTO-REC-DT.
01701
01702      IF AT-TERMINATED-DT = LOW-VALUES OR SPACES
01703          IF AT-SCHEDULE-END-DT = LOW-VALUES
01704              MOVE SPACES                     TO  AUTO-LST-PMT-DT
01705          ELSE
01706              MOVE 'END : '                   TO  AUTO-LST-PMT-LIT
01707              IF PI-USES-PAID-TO
01708                  MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1
01709                  MOVE '6'                    TO  DC-OPTION-CODE
01710                  MOVE +1                     TO  DC-ELAPSED-DAYS
01711                  MOVE +0                     TO  DC-ELAPSED-MONTHS
01712                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01713                  IF NO-CONVERSION-ERROR
01714                      MOVE DC-GREG-DATE-1-EDIT TO AUTO-LST-PMT-DT
01715                  ELSE
01716                      MOVE SPACES             TO  AUTO-LST-PMT-DT
01717              ELSE
01718                  MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1
01719                  MOVE ' '                    TO  DC-OPTION-CODE
01720                  MOVE +0                     TO  DC-ELAPSED-DAYS
01721                                                  DC-ELAPSED-MONTHS
01722                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01723                  IF NO-CONVERSION-ERROR
01724                      MOVE DC-GREG-DATE-1-EDIT TO AUTO-LST-PMT-DT
01725                  ELSE
01726                      MOVE SPACES             TO  AUTO-LST-PMT-DT
01727      ELSE
01728          MOVE ' TERM: '                      TO  AUTO-LST-PMT-LIT
01729          MOVE AT-TERMINATED-DT               TO  DC-BIN-DATE-1
01730          MOVE ' '                            TO  DC-OPTION-CODE
01731          MOVE +0                             TO  DC-ELAPSED-DAYS
01732                                                  DC-ELAPSED-MONTHS
01733          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01734          IF NO-CONVERSION-ERROR
01735              MOVE DC-GREG-DATE-1-EDIT        TO  AUTO-LST-PMT-DT
01736          ELSE
01737              MOVE SPACES                     TO  AUTO-LST-PMT-DT.
01738
01739      MOVE ' STATUS : '           TO  AUTO-STATUS-LIT.
01740
01741      IF AT-TERMINATED-DT NOT = LOW-VALUES
01742          MOVE 'TERM'             TO  AUTO-STATUS
01743      ELSE
01744          MOVE 'ACTIVE'           TO  AUTO-STATUS.
01745
01746      MOVE '   REG: '             TO  AUTO-REG-AMT-LIT.
01747      MOVE AT-REGULAR-PMT-AMT     TO  AUTO-REG-AMT.
01748
01749      MOVE ' TO: '                TO  AUTO-PAYEE-LIT.
01750
01751      IF INSURED-PAID-AUTO
01752          MOVE ' INS '            TO  AUTO-PAYEE.
01753      IF BENEFICIARY-PAID-AUTO
01754          MOVE ' BENE'            TO  AUTO-PAYEE.
01755      IF ACCOUNT-PAID-AUTO
01756          MOVE ' ACCT'            TO  AUTO-PAYEE.
01757      IF OTHER-1-PAID-AUTO
01758          MOVE ' OTH1'            TO  AUTO-PAYEE.
01759      IF OTHER-2-PAID
01760          MOVE ' OTH2'            TO  AUTO-PAYEE.
01761      IF DOCTOR-PAID
01762          MOVE ' DOC '            TO  AUTO-PAYEE.
01763
01764      MOVE AUTO-PMT-HDG1          TO  MAP-HDG        (DISPLAY-CNT).
01765      MOVE AUTO-PMT-TEXT1         TO  MAP-TEXT       (DISPLAY-CNT).
01766      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
01767      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
01768      ADD +1 TO DISPLAY-CNT.
01769      MOVE AUTO-PMT-HDG2          TO  MAP-HDG        (DISPLAY-CNT).
01770      MOVE AUTO-PMT-TEXT2         TO  MAP-TEXT       (DISPLAY-CNT).
01771      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
01772                                      MAP-TEXT-ATTRB (DISPLAY-CNT).
01773      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
01774      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
01775      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
01776      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).
01777      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).
01778
01779      GO TO 2800-INCR-DISPLAY-CNT.
01780
01781      EJECT
01782  2300-CORRESPONDENCE-TRAILER.
01783      MOVE SPACES                 TO  CORR-HDG1.
01784      MOVE PI-LINE-NO             TO  CORR-LINE-NO.
01785      MOVE 'LETTER'               TO  CORR-HDG1-LIT.
01786
01787      MOVE SPACES                 TO  CORR-TEXT-1.
01788      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.
01789      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.
01790      MOVE WS-FORMAT-CERT-NO      TO  CORR-CERT-NO.
01791
01792      MOVE ' FORM: '              TO  CORR-FORM-LIT.
01793      MOVE AT-STD-LETTER-FORM     TO  CORR-FORM-TYPE.
01794
01795      IF AT-LETTER-SENT-DT = LOW-VALUES OR SPACES
01796          MOVE SPACES                     TO  CORR-DT-SENT
01797                                              CORR-DT-SENT-LIT
01798      ELSE
01799          MOVE '  DT SENT: '              TO  CORR-DT-SENT-LIT
01800          MOVE AT-LETTER-SENT-DT          TO  DC-BIN-DATE-1
01801          MOVE ' '                        TO  DC-OPTION-CODE
01802          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01803          IF NO-CONVERSION-ERROR
01804              MOVE DC-GREG-DATE-1-EDIT    TO  CORR-DT-SENT
01805          ELSE
01806              MOVE SPACES                 TO  CORR-DT-SENT.
01807
01808      IF AT-LETTER-PURGED-DT = LOW-VALUES OR SPACES
071210       IF AT-LETTER-TO-BENE EQUAL 'Y'
071210          MOVE ' LETTER TO BENE' TO CORR-LET-TO-BEN
071210       ELSE
062217          IF AT-AUTH-RCVD >  SPACES
062217             MOVE ' AUTH RCVD: ' TO CORR-LET-TO-BEN
062217             MOVE AT-AUTH-RCVD TO CORR-LET-TO-BEN(13:1)
062217          ELSE
01809              MOVE SPACES             TO  CORR-PURGE-DT
01810                                          CORR-PURGE-LIT
062217          END-IF
071210       END-IF
01811      ELSE
01812          MOVE ' PUR: '                   TO  CORR-PURGE-LIT
01813          MOVE AT-LETTER-PURGED-DT        TO  DC-BIN-DATE-1
01814          MOVE ' '                        TO  DC-OPTION-CODE
01815          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01816          IF NO-CONVERSION-ERROR
01817              MOVE DC-GREG-DATE-1-EDIT    TO  CORR-PURGE-DT
01818          ELSE
01819              MOVE SPACES                 TO  CORR-PURGE-DT.
01820
01821      MOVE ' BY: '                TO  CORR-REC-BY-LIT.
01822      MOVE AT-RECORDED-BY         TO  CORR-REC-BY.
01823
01824      MOVE SPACES                 TO  CORR-HDG2
01825                                      CORR-TEXT-2.
01826      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
01827      MOVE ' '                    TO  DC-OPTION-CODE.
01828      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
01829      IF NO-CONVERSION-ERROR
01830          MOVE DC-GREG-DATE-1-EDIT TO CORR-REC-DT
01831      ELSE
01832          MOVE SPACES             TO  CORR-REC-DT.
01833
01834      IF AT-RESEND-PRINT-DATE = LOW-VALUES OR SPACES
01835          IF AT-AUTO-RE-SEND-DT = LOW-VALUES OR SPACES
01836              MOVE SPACES                    TO  CORR-RESEND-DT
01837          ELSE
01838              MOVE 'RSND: '                  TO  CORR-RESEND-LIT
01839              MOVE AT-AUTO-RE-SEND-DT        TO  DC-BIN-DATE-1
01840              MOVE ' '                       TO  DC-OPTION-CODE
01841              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01842              IF NO-CONVERSION-ERROR
01843                  MOVE DC-GREG-DATE-1-EDIT   TO  CORR-RESEND-DT
01844              ELSE
01845                  MOVE SPACES                TO  CORR-RESEND-DT
01846      ELSE
01847          MOVE 'RSNT :'                      TO  CORR-RESEND-LIT
01848          MOVE AT-RESEND-PRINT-DATE          TO  DC-BIN-DATE-1
01849          MOVE ' '                           TO  DC-OPTION-CODE
01850          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01851          IF NO-CONVERSION-ERROR
01852              MOVE DC-GREG-DATE-1-EDIT       TO  CORR-RESEND-DT
01853          ELSE
01854              MOVE SPACES                    TO  CORR-RESEND-DT.
01855
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
01868
01869      IF AT-RECEIPT-FOLLOW-UP = LOW-VALUES OR SPACES
01870          MOVE SPACES                    TO  CORR-FOLLOW-UP-DT
01871                                             CORR-FOLLOW-UP-LIT
01872      ELSE
01873          MOVE ' FOL: '                  TO  CORR-FOLLOW-UP-LIT
01874          MOVE AT-RECEIPT-FOLLOW-UP      TO  DC-BIN-DATE-1
01875          MOVE ' '                       TO  DC-OPTION-CODE
01876          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01877          IF NO-CONVERSION-ERROR
01878              MOVE DC-GREG-DATE-1-EDIT   TO  CORR-FOLLOW-UP-DT
01879          ELSE
01880              MOVE SPACES                TO  CORR-FOLLOW-UP-DT.
01881
01882      MOVE ' TO: '                       TO  CORR-ADDRESEE-LIT.
01883
01884      IF AT-ADDRESEE-TYPE = 'I'
01885          MOVE 'INS '                    TO  CORR-ADDRESEE
01886      ELSE
01887      IF AT-ADDRESEE-TYPE = 'B'
01888          MOVE 'BENE'                    TO  CORR-ADDRESEE
01889      ELSE
01890      IF AT-ADDRESEE-TYPE = 'A'
01891          MOVE 'ACCT'                    TO  CORR-ADDRESEE
01892      ELSE
01893      IF AT-ADDRESEE-TYPE = 'P'
01894          MOVE 'DOC '                    TO  CORR-ADDRESEE
01895      ELSE
01896      IF AT-ADDRESEE-TYPE = 'E'
01897          MOVE 'EMP '                    TO  CORR-ADDRESEE
01898      ELSE
01899      IF AT-ADDRESEE-TYPE = 'O'
01900          MOVE 'OTH1'                    TO  CORR-ADDRESEE
01901      ELSE
01902          MOVE 'OTH2'                    TO  CORR-ADDRESEE.
01903
01904      MOVE CORR-HDG1              TO  MAP-HDG        (DISPLAY-CNT).
01905      MOVE CORR-TEXT-1            TO  MAP-TEXT       (DISPLAY-CNT).
01906      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
01907      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
01908      ADD +1 TO DISPLAY-CNT.
01909      MOVE CORR-HDG2              TO  MAP-HDG        (DISPLAY-CNT).
01910      MOVE CORR-TEXT-2            TO  MAP-TEXT       (DISPLAY-CNT).
01911      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
01912                                      MAP-TEXT-ATTRB (DISPLAY-CNT).
01913      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
01914      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
01915      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
01916      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).
01917      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).
01918
01919      GO TO 2800-INCR-DISPLAY-CNT.
01920
01921      EJECT
01922  2400-GENERAL-INFO-TRAILER.
01923
01924      MOVE SPACES                 TO  GEN-INFO-LINE-1
01925                                      GEN-INFO-LINE-2.
01926      MOVE PI-LINE-NO             TO  GI-LINE-NO.
01927
01928      IF AT-MAINT-NOTE
01929          MOVE 'MAINT'            TO  GI-HDG1-LIT
01930      ELSE
01931      IF AT-CALL-NOTE
01932          MOVE 'CALL'             TO  GI-HDG1-LIT
01933          IF AT-PHONE-CALL-IN
01934              MOVE '     IN'      TO  GI-HDG2-LIT
01935            ELSE
01936              MOVE '     OUT'     TO  GI-HDG2-LIT
01937      ELSE
01938      IF AT-CERT-CHANGE
01939          MOVE 'CERT CHG'         TO  GI-HDG1-LIT
01940      ELSE
01941          MOVE 'NOTE'             TO  GI-HDG1-LIT.
01942
01943      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.
01944      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.
01945      MOVE WS-FORMAT-CERT-NO      TO  GEN-INFO-CERT-NO.
01946
01947      MOVE ' MSG : '              TO  GEN-INFO-MSG-1-LIT.
01948      MOVE AT-INFO-LINE-1         TO  GEN-INFO-MSG-1.
01949      MOVE ' BY: '                TO  GEN-INFO-REC-BY-LIT.
01950      MOVE AT-RECORDED-BY         TO  GEN-INFO-REC-BY.
01951
01952      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
01953      MOVE ' '                    TO  DC-OPTION-CODE.
01954      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
01955      IF NO-CONVERSION-ERROR
01956          MOVE DC-GREG-DATE-1-EDIT TO GEN-INFO-REC-DT
01957      ELSE
01958          MOVE SPACES             TO  GEN-INFO-REC-DT.
01959
01960      MOVE 'MSG : '               TO  GEN-INFO-MSG-2-LIT.
01961      MOVE AT-INFO-LINE-2         TO  GEN-INFO-MSG-2.
01962
01963      MOVE GEN-INFO-HDG1          TO  MAP-HDG        (DISPLAY-CNT).
01964      MOVE GEN-INFO-TEXT-1        TO  MAP-TEXT       (DISPLAY-CNT).
01965      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
01966      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
01967      ADD +1 TO DISPLAY-CNT.
01968      MOVE GEN-INFO-HDG2          TO  MAP-HDG        (DISPLAY-CNT).
01969      MOVE GEN-INFO-TEXT-2        TO  MAP-TEXT       (DISPLAY-CNT).
01970      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
01971                                      MAP-TEXT-ATTRB (DISPLAY-CNT).
01972      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
01973      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
01974      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
01975      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).
01976      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).
01977
01978      GO TO 2800-INCR-DISPLAY-CNT.
01979
01980      EJECT
01981  2500-AUTO-PROMPT-TRAILER.
01982
01983      MOVE SPACES                 TO  REMINDER-HDG1.
01984      MOVE PI-LINE-NO             TO  REM-LINE-NO.
01985      MOVE 'REMINDER'             TO  REM-HDG1-LIT.
01986
01987      MOVE SPACES                 TO  REMINDER-TEXT-1.
01988      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.
01989      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.
01990      MOVE WS-FORMAT-CERT-NO      TO  REM-CERT-NO.
01991
01992      MOVE ' LN 1: '              TO  REM-LINE-1-LIT.
01993      MOVE AT-PROMPT-LINE-1       TO  REM-LINE-1.
01994      MOVE ' BY: '                TO  REM-REC-BY-LIT.
01995      MOVE AT-RECORDED-BY         TO  REM-REC-BY.
01996
01997      MOVE SPACES                 TO  REMINDER-HDG2
01998                                      REMINDER-TEXT-2.
01999      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
02000      MOVE ' '                    TO  DC-OPTION-CODE.
02001      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02002      IF NO-CONVERSION-ERROR
02003          MOVE DC-GREG-DATE-1-EDIT TO REM-REC-DT
02004      ELSE
02005          MOVE SPACES             TO  REM-REC-DT.
02006
02007      MOVE 'LN 2: '               TO  REM-LINE-2-LIT.
02008      MOVE AT-PROMPT-LINE-2       TO  REM-LINE-2.
02009
02010      MOVE REMINDER-HDG1          TO  MAP-HDG        (DISPLAY-CNT).
02011      MOVE REMINDER-TEXT-1        TO  MAP-TEXT       (DISPLAY-CNT).
02012      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
02013      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
02014      ADD +1 TO DISPLAY-CNT.
02015      MOVE REMINDER-HDG2          TO  MAP-HDG        (DISPLAY-CNT).
02016      MOVE REMINDER-TEXT-2        TO  MAP-TEXT       (DISPLAY-CNT).
02017      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
02018                                      MAP-TEXT-ATTRB (DISPLAY-CNT).
02019      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
02020      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
02021      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
02022      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).
02023      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).
02024
02025      GO TO 2800-INCR-DISPLAY-CNT.
02026
02027      EJECT
02028  2600-DENIAL-TRAILER.
02029      MOVE SPACES                 TO  DENIAL-HDG1.
02030      MOVE PI-LINE-NO             TO  DENIAL-LINE-NO.
02031      MOVE 'DENIAL'               TO  DENIAL-HDG1-LIT.
02032
02033      MOVE SPACES                 TO  DENIAL-TEXT-1.
02034      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.
02035      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.
02036      MOVE WS-FORMAT-CERT-NO      TO  DENIAL-CERT-NO.
02037
02038      MOVE ' LN 1: '              TO  DENIAL-LN1-LIT.
02039      MOVE AT-DENIAL-INFO-1       TO  DENIAL-LN1.
02040      MOVE ' BY: '                TO  DENIAL-REC-BY-LIT.
02041      MOVE AT-RECORDED-BY         TO  DENIAL-REC-BY.
02042
02043      MOVE SPACES                 TO  DENIAL-HDG2
02044                                      DENIAL-TEXT-2.
02045      MOVE AT-RECORDED-DT         TO  DC-BIN-DATE-1.
02046      MOVE ' '                    TO  DC-OPTION-CODE.
02047      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02048      IF NO-CONVERSION-ERROR
02049          MOVE DC-GREG-DATE-1-EDIT TO DENIAL-REC-DT
02050      ELSE
02051          MOVE SPACES             TO  DENIAL-REC-DT.
02052
02053      MOVE 'LN 2: '               TO  DENIAL-LN2-LIT.
02054      MOVE AT-DENIAL-INFO-2       TO  DENIAL-LN2.
02055
02056      MOVE DENIAL-HDG1            TO  MAP-HDG        (DISPLAY-CNT).
02057      MOVE DENIAL-TEXT-1          TO  MAP-TEXT       (DISPLAY-CNT).
02058      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
02059      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
02060      ADD +1 TO DISPLAY-CNT.
02061      MOVE DENIAL-HDG2            TO  MAP-HDG        (DISPLAY-CNT).
02062      MOVE DENIAL-TEXT-2          TO  MAP-TEXT       (DISPLAY-CNT).
02063      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
02064                                      MAP-TEXT-ATTRB (DISPLAY-CNT).
02065      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
02066      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
02067      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
02068      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).
02069      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).
02070
02071      GO TO 2800-INCR-DISPLAY-CNT.
02072
02073      EJECT
02074  2700-INCURRED-CHANGE-TRAILER.
02075      MOVE SPACES                 TO  INCUR-CHG-HDG1.
02076      MOVE PI-LINE-NO             TO  INCUR-LINE-NO.
02077      MOVE 'INCUR CG'             TO  INCUR-HDG1-LIT.
02078
02079      MOVE SPACES                 TO  INCUR-TEXT-1.
02080      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.
02081      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.
02082      MOVE WS-FORMAT-CERT-NO      TO  INCUR-CERT-NO.
02083
02084      IF AT-OLD-INCURRED-DT = LOW-VALUES OR SPACES
02085          MOVE SPACES                     TO  INCUR-INCUR-DT
02086                                              INCUR-INCUR-LIT
02087      ELSE
02088          MOVE ' INCR: '                  TO  INCUR-INCUR-LIT
02089          MOVE AT-OLD-INCURRED-DT         TO  DC-BIN-DATE-1
02090          MOVE ' '                        TO  DC-OPTION-CODE
02091          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02092          IF NO-CONVERSION-ERROR
02093              MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-INCUR-DT
02094          ELSE
02095              MOVE SPACES                 TO  INCUR-INCUR-DT.
02096
02097      IF AT-OLD-REPORTED-DT = LOW-VALUES OR SPACES
02098          MOVE SPACES                     TO  INCUR-REPORT-DT
02099                                              INCUR-REPORT-LIT
02100      ELSE
02101          MOVE ' REPORT : '               TO  INCUR-REPORT-LIT
02102          MOVE AT-OLD-REPORTED-DT         TO  DC-BIN-DATE-1
02103          MOVE ' '                        TO  DC-OPTION-CODE
02104          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02105          IF NO-CONVERSION-ERROR
02106              MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-REPORT-DT
02107          ELSE
02108              MOVE SPACES                 TO  INCUR-REPORT-DT.
02109
02110      IF AT-OLD-ESTABLISHED-DT = LOW-VALUES OR SPACES
02111          MOVE SPACES                     TO  INCUR-ESTAB-DT
02112                                              INCUR-ESTAB-LIT
02113      ELSE
02114          MOVE ' EST: '                   TO  INCUR-ESTAB-LIT
02115          MOVE AT-OLD-ESTABLISHED-DT      TO  DC-BIN-DATE-1
02116          MOVE ' '                        TO  DC-OPTION-CODE
02117          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02118          IF NO-CONVERSION-ERROR
02119              MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-ESTAB-DT
02120          ELSE
02121              MOVE SPACES                 TO  INCUR-ESTAB-DT.
02122
02123      MOVE ' BY: '                        TO  INCUR-REC-BY-LIT.
02124      MOVE AT-RECORDED-BY                 TO  INCUR-REC-BY.
02125
02126      MOVE SPACES                         TO  INCUR-CHG-HDG2
02127                                              INCUR-TEXT-2.
02128      MOVE AT-RECORDED-DT                 TO  DC-BIN-DATE-1.
02129      MOVE ' '                            TO  DC-OPTION-CODE.
02130      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02131      IF NO-CONVERSION-ERROR
02132          MOVE DC-GREG-DATE-1-EDIT        TO  INCUR-REC-DT
02133      ELSE
02134          MOVE SPACES                     TO  INCUR-REC-DT.
02135
02136      IF PI-USES-PAID-TO
02137          MOVE '  TO: '                   TO  INCUR-PD-THRU-LIT
02138      ELSE
02139          MOVE 'THRU: '                   TO  INCUR-PD-THRU-LIT.
02140
02141      IF AT-OLD-PAID-THRU-DT = LOW-VALUES OR SPACES
02142          MOVE SPACES                     TO  INCUR-PD-THRU-DT
02143      ELSE
02144          MOVE AT-OLD-PAID-THRU-DT        TO  DC-BIN-DATE-1
02145          MOVE ' '                        TO  DC-OPTION-CODE
02146          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02147          IF NO-CONVERSION-ERROR
02148              MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-PD-THRU-DT
02149          ELSE
02150              MOVE SPACES                 TO  INCUR-PD-THRU-DT.
02151
02152      IF AT-OLD-TOTAL-PAID = ZEROS
02153          MOVE ZEROS              TO  INCUR-TOT-PD
02154          MOVE SPACES             TO  INCUR-TOT-PD-LIT
02155      ELSE
02156          MOVE AT-OLD-TOTAL-PAID  TO  INCUR-TOT-PD
02157          MOVE ' TOT PD : '       TO  INCUR-TOT-PD-LIT.
02158
02159      IF AT-OLD-DAYS-PAID = ZEROS
02160          MOVE ZEROS              TO  INCUR-TOT-DAYS-PD
02161          MOVE SPACES             TO  INCUR-TOT-DAYS-LIT
02162      ELSE
02163          MOVE AT-OLD-DAYS-PAID   TO  INCUR-TOT-DAYS-PD
02164          MOVE ' DAY: '           TO  INCUR-TOT-DAYS-LIT.
02165
02166      IF AT-OLD-NO-OF-PMTS = ZEROS
02167          MOVE ZEROS              TO  INCUR-NO-PMTS
02168          MOVE SPACES             TO  INCUR-NO-PMTS-LIT
02169      ELSE
02170          MOVE ' NO PMTS : '      TO  INCUR-NO-PMTS-LIT
02171          MOVE AT-OLD-NO-OF-PMTS  TO  INCUR-NO-PMTS.
02172
02173      MOVE INCUR-CHG-HDG1         TO  MAP-HDG        (DISPLAY-CNT).
02174      MOVE INCUR-TEXT-1           TO  MAP-TEXT       (DISPLAY-CNT).
02175      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
02176      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
02177      ADD +1 TO DISPLAY-CNT.
02178      MOVE INCUR-CHG-HDG2         TO  MAP-HDG        (DISPLAY-CNT).
02179      MOVE INCUR-TEXT-2           TO  MAP-TEXT       (DISPLAY-CNT).
02180      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
02181                                      MAP-TEXT-ATTRB (DISPLAY-CNT).
02182      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
02183      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
02184      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
02185      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).
02186      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).
02187
02188      GO TO 2800-INCR-DISPLAY-CNT.
02189
02190      EJECT
02191  2710-FORM-CONTROL-TRAILER.
02192      MOVE SPACES                 TO  FORM-HDG1.
02193      MOVE PI-LINE-NO             TO  FORM-LINE-NO.
02194      MOVE 'FORM    '             TO  FORM-HDG1-LIT.
02195
02196      MOVE SPACES                 TO  FORM-TEXT-1.
02197      MOVE AT-CERT-PRIME          TO  WS-FORMAT-CERT-PRIME.
02198      MOVE AT-CERT-SFX            TO  WS-FORMAT-CERT-SFX.
02199      MOVE WS-FORMAT-CERT-NO      TO  FORM-CERT-NO.
02200
02201      MOVE ' TYPE: '              TO  FORM-TYPE-LIT.
02202
02203      IF INITIAL-FORM
02204         MOVE 'INITIAL'           TO  FORM-TYPE
02205      ELSE
02206         MOVE 'PROGRESS'          TO  FORM-TYPE.
02207
02208      IF AT-FORM-PRINTED-DT = LOW-VALUES OR SPACES
02209          MOVE ' SEND ON: '                   TO  FORM-SEND-ON-LIT
02210          IF AT-FORM-SEND-ON-DT = LOW-VALUES OR SPACES
02211              MOVE SPACES                     TO  FORM-SEND-ON-DT
02212          ELSE
02213              MOVE AT-FORM-SEND-ON-DT         TO  DC-BIN-DATE-1
02214              MOVE ' '                        TO  DC-OPTION-CODE
02215              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02216              IF NO-CONVERSION-ERROR
02217                  MOVE DC-GREG-DATE-1-EDIT    TO  FORM-SEND-ON-DT
02218              ELSE
02219                  MOVE SPACES                 TO  FORM-SEND-ON-DT
02220      ELSE
02221          MOVE ' SENT ON: '                   TO  FORM-SEND-ON-LIT
02222          MOVE AT-FORM-PRINTED-DT             TO  DC-BIN-DATE-1
02223          MOVE ' '                            TO  DC-OPTION-CODE
02224          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02225          IF NO-CONVERSION-ERROR
02226              MOVE DC-GREG-DATE-1-EDIT        TO  FORM-SEND-ON-DT
02227          ELSE
02228              MOVE SPACES                     TO  FORM-SEND-ON-DT.
02229
02230      IF AT-FORM-REPRINT-DT = LOW-VALUES OR SPACES
02231          IF AT-FORM-RE-SEND-DT = LOW-VALUES OR SPACES
02232              MOVE SPACES                     TO  FORM-RESEND-DT
02233                                                  FORM-RESEND-LIT
02234          ELSE
02235              MOVE ' RES: '                   TO  FORM-RESEND-LIT
02236              MOVE AT-FORM-RE-SEND-DT         TO  DC-BIN-DATE-1
02237              MOVE ' '                        TO  DC-OPTION-CODE
02238              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02239              IF NO-CONVERSION-ERROR
02240                  MOVE DC-GREG-DATE-1-EDIT    TO  FORM-RESEND-DT
02241              ELSE
02242                  MOVE SPACES                 TO  FORM-RESEND-DT
02243      ELSE
02244          MOVE ' RES: '                       TO  FORM-RESEND-LIT
02245          MOVE AT-FORM-REPRINT-DT             TO  DC-BIN-DATE-1
02246          MOVE ' '                            TO  DC-OPTION-CODE
02247          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02248          IF NO-CONVERSION-ERROR
02249              MOVE DC-GREG-DATE-1-EDIT        TO  FORM-RESEND-DT
02250          ELSE
02251              MOVE SPACES                     TO  FORM-RESEND-DT.
02252
02253      MOVE ' BY: '                        TO  FORM-REC-BY-LIT.
02254      MOVE AT-RECORDED-BY                 TO  FORM-REC-BY.
02255
02256      MOVE SPACES                      TO  FORM-HDG2
02257                                           FORM-TEXT-2.
02258      MOVE AT-RECORDED-DT              TO  DC-BIN-DATE-1.
02259      MOVE ' '                         TO  DC-OPTION-CODE.
02260      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
02261      IF NO-CONVERSION-ERROR
02262          MOVE DC-GREG-DATE-1-EDIT     TO  FORM-REC-DT
02263      ELSE
02264          MOVE SPACES                  TO  FORM-REC-DT.
02265
02266      IF AT-FORM-ANSWERED-DT = LOW-VALUES OR SPACES
02267          MOVE SPACES                  TO  FORM-REC-INS-DT
02268                                           FORM-REC-INS-LIT
02269      ELSE
02270          MOVE 'INS : '                TO  FORM-REC-INS-LIT
02271          MOVE AT-FORM-ANSWERED-DT     TO  DC-BIN-DATE-1
02272          MOVE ' '                     TO  DC-OPTION-CODE
02273          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02274          IF NO-CONVERSION-ERROR
02275              MOVE DC-GREG-DATE-1-EDIT TO  FORM-REC-INS-DT
02276          ELSE
02277              MOVE SPACES              TO  FORM-REC-INS-DT.
02278
02279      IF AT-PHY-FORM-ANSWERED-DT = LOW-VALUES OR SPACES
02280          MOVE SPACES                   TO  FORM-REC-PHY-DT
02281                                            FORM-REC-PHY-LIT
02282      ELSE
02283          MOVE ' PHYS   : '             TO  FORM-REC-PHY-LIT
02284          MOVE AT-PHY-FORM-ANSWERED-DT  TO  DC-BIN-DATE-1
02285          MOVE ' '                      TO  DC-OPTION-CODE
02286          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02287          IF NO-CONVERSION-ERROR
02288              MOVE DC-GREG-DATE-1-EDIT  TO  FORM-REC-PHY-DT
02289          ELSE
02290              MOVE SPACES               TO  FORM-REC-PHY-DT.
02291
02292      IF AT-EMP-FORM-ANSWERED-DT = LOW-VALUES OR SPACES
02293          MOVE SPACES                   TO  FORM-REC-EMP-DT
02294                                            FORM-REC-EMP-LIT
02295      ELSE
02296          MOVE ' EMP: '                 TO  FORM-REC-EMP-LIT
02297          MOVE AT-EMP-FORM-ANSWERED-DT  TO  DC-BIN-DATE-1
02298          MOVE ' '                      TO  DC-OPTION-CODE
02299          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02300          IF NO-CONVERSION-ERROR
02301              MOVE DC-GREG-DATE-1-EDIT  TO  FORM-REC-EMP-DT
02302          ELSE
02303              MOVE SPACES               TO  FORM-REC-EMP-DT.
02304
02305      MOVE FORM-HDG1              TO  MAP-HDG        (DISPLAY-CNT).
02306      MOVE FORM-TEXT-1            TO  MAP-TEXT       (DISPLAY-CNT).
02307      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
02308      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
02309      ADD +1 TO DISPLAY-CNT.
02310      MOVE FORM-HDG2              TO  MAP-HDG        (DISPLAY-CNT).
02311      MOVE FORM-TEXT-2            TO  MAP-TEXT       (DISPLAY-CNT).
02312      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
02313                                      MAP-TEXT-ATTRB (DISPLAY-CNT).
02314      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
02315      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
02316      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
02317      MOVE WS-SRTD-CERT (SUB-2)   TO  PI-TRLR-CERT   (SUB-1).
02318      MOVE SUB-2                  TO  PI-TRLR-SUB    (SUB-1).
02319
02320      GO TO 2800-INCR-DISPLAY-CNT.
02321
02322  2800-INCR-DISPLAY-CNT.
02323
02324      IF DIRECTION-SWITCH = 'F'
02325          ADD +1 TO DISPLAY-CNT
02326                    SUB-2
02327                    PI-LINE-NO
02328                    SUB-1
02329          IF DISPLAY-CNT > +16
02330              GO TO 2999-EXIT
02331          ELSE
02332              NEXT SENTENCE
02333      ELSE
02334          SUBTRACT +3 FROM DISPLAY-CNT
02335          SUBTRACT +1 FROM PI-LINE-NO
02336                           SUB-1
02337                           SUB-2
02338          IF DISPLAY-CNT < +1
02339              GO TO 2999-EXIT.
02340
02341      GO TO 2000-BUILD-TRAILER-DISPLAY.
02342
02343  2950-NO-MORE-TRAILERS.
02344      MOVE ER-0303                TO  EMI-ERROR.
02345      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02346
02347  2999-EXIT.
02348      EXIT.
02349
02350      EJECT
02351  3000-RECEIVE-LETTERS.
02352
02353      MOVE LINENOI                TO  SUB-1.
02354
02355      IF PI-TRLR-TYPE (SUB-1) = '4'
02356          NEXT SENTENCE
02357      ELSE
02358      IF PI-TRLR-TYPE (SUB-1) = '2'
02359          MOVE ER-0667            TO  EMI-ERROR
02360          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02361          MOVE -1                 TO  LINENOL
02362          GO TO 8200-SEND-DATAONLY
02363      ELSE
02364      IF PI-TRLR-TYPE (SUB-1) = 'A'
02365          MOVE ER-0665            TO  EMI-ERROR
02366          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02367          MOVE -1                 TO  LINENOL
02368          GO TO 8200-SEND-DATAONLY
02369      ELSE
02370          MOVE ER-0660            TO  EMI-ERROR
02371          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02372          MOVE -1                 TO  LINENOL
02373          GO TO 8200-SEND-DATAONLY.
02374
02375      IF RECVDTI = SPACES
02376          MOVE LOW-VALUES                   TO  WS-RECEIVED-DATE
02377      ELSE
02378          MOVE RECVDTI                      TO  WS-DEEDIT-FIELD
02379          PERFORM 9800-DEEDIT THRU 9800-EXIT
02380          IF WS-DEEDIT-FIELD-V0 NUMERIC
02381              MOVE '4'                      TO  DC-OPTION-CODE
02382              MOVE WS-DEEDIT-FIELD-V0       TO  DC-GREG-DATE-1-MDY
02383              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02384              IF NO-CONVERSION-ERROR
02385                  MOVE DC-BIN-DATE-1        TO  WS-RECEIVED-DATE
02386                  MOVE DC-GREG-DATE-1-EDIT  TO  RECVDTO
02387                  MOVE AL-UANON             TO  RECVDTA
02388              ELSE
02389                  MOVE LOW-VALUES           TO  WS-RECEIVED-DATE.
02390
02391      MOVE PI-TRLR-SEQ-NO (SUB-1) TO  TRLR-SEQ-NO.
02392      MOVE PI-TRLR-CERT   (SUB-1) TO  TRLR-CERT-NO.
02393      PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.
02394
02395      MOVE WS-RECEIVED-DATE       TO  AT-LETTER-ANSWERED-DT.
02396      MOVE PI-PROCESSOR-ID        TO  AT-CORR-LAST-UPDATED-BY.
02397      MOVE SAVE-BIN-DATE          TO  AT-CORR-LAST-MAINT-DT.
02398
02399      PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.
02400
02401      MOVE PI-TRLR-CERT   (SUB-1) TO  MSTR-CERT-NO.
02402      PERFORM 7500-READ-CLAIM-MSTR-UPDATE THRU 7500-EXIT.
02403
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        MOVE 11                 TO CL-ACTIVITY-CODE
121802*        MOVE SAVE-BIN-DATE      TO CL-ACTIVITY-MAINT-DT
121802*        MOVE 'CORR'             TO CL-ACTIVITY-MAINT-TYPE
121802*        MOVE PI-PROCESSOR-ID    TO CL-PROCESSOR-ID.
02409
02410      PERFORM 7600-REWRITE-CLAIM-MSTR THRU 7600-EXIT.
02411
02412      MOVE ER-0000                TO  EMI-ERROR.
02413      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02414      MOVE -1                     TO  LINENOL.
02415      MOVE AL-UANOF               TO  LINENOA  RECVDTA
02416                                      RECVTYPA.
02417      MOVE LOW-VALUES             TO  EL150CO.
02418      MOVE 'F'                    TO  DIRECTION-SWITCH.
02419      MOVE +1                     TO  DISPLAY-CNT
02420                                      SUB-1
02421                                      PI-LINE-NO.
02422      MOVE PI-TRLR-SUB (SUB-1)    TO  SUB-2.
02423      PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT.
02424      GO TO 1010-BUILD-HISTORY-SCREEN.
02425
02426      EJECT
02427  4000-RECEIVE-FORMS.
02428
02429      MOVE LINENOI                TO  SUB-1.
02430
02431      IF PI-TRLR-TYPE (SUB-1) = 'A'
02432          NEXT SENTENCE
02433      ELSE
02434      IF PI-TRLR-TYPE (SUB-1) = '4'
02435          MOVE ER-0666            TO  EMI-ERROR
02436          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02437          MOVE -1                 TO  LINENOL
02438          GO TO 8200-SEND-DATAONLY
02439      ELSE
02440      IF PI-TRLR-TYPE (SUB-1) = '2'
02441          MOVE ER-0667            TO  EMI-ERROR
02442          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02443          MOVE -1                 TO  LINENOL
02444          GO TO 8200-SEND-DATAONLY
02445      ELSE
02446          MOVE ER-0661            TO  EMI-ERROR
02447          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02448          MOVE -1                 TO  LINENOL
02449          GO TO 8200-SEND-DATAONLY.
02450
02451      IF RECVTYPI = 'I' OR 'P' OR 'E'
02452          NEXT SENTENCE
02453      ELSE
02454          MOVE ER-0662            TO  EMI-ERROR
02455          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02456          MOVE -1                 TO  RECVTYPL
02457          GO TO 8200-SEND-DATAONLY.
02458
02459      IF RECVDTI = SPACES
02460          MOVE LOW-VALUES                   TO  WS-RECEIVED-DATE
02461      ELSE
02462          MOVE RECVDTI                      TO  WS-DEEDIT-FIELD
02463          PERFORM 9800-DEEDIT THRU 9800-EXIT
02464          IF WS-DEEDIT-FIELD-V0 IS NUMERIC
02465              MOVE '4'                      TO  DC-OPTION-CODE
02466              MOVE WS-DEEDIT-FIELD-V0       TO  DC-GREG-DATE-1-MDY
02467              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
02468              IF NO-CONVERSION-ERROR
02469                  MOVE DC-BIN-DATE-1        TO  WS-RECEIVED-DATE
02470                  MOVE DC-GREG-DATE-1-EDIT  TO  RECVDTO
02471                  MOVE AL-UANON             TO  RECVDTA
02472              ELSE
02473                  MOVE LOW-VALUES           TO  WS-RECEIVED-DATE.
02474
02475      MOVE PI-TRLR-CERT   (SUB-1)     TO  TRLR-CERT-NO.
02476      MOVE PI-TRLR-SEQ-NO (SUB-1)     TO  TRLR-SEQ-NO.
02477      PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.
02478
02479      IF RECVTYPI = 'I'
02480          MOVE WS-RECEIVED-DATE       TO  AT-FORM-ANSWERED-DT
02481      ELSE
02482      IF RECVTYPI = 'P'
02483          MOVE WS-RECEIVED-DATE       TO  AT-PHY-FORM-ANSWERED-DT
02484      ELSE
02485          MOVE WS-RECEIVED-DATE       TO  AT-EMP-FORM-ANSWERED-DT.
02486
02487      MOVE PI-PROCESSOR-ID            TO  AT-FORM-LAST-UPDATED-BY.
02488      MOVE SAVE-BIN-DATE              TO  AT-FORM-LAST-MAINT-DT.
02489
02490      PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.
02491
02492      MOVE ER-0000                TO  EMI-ERROR.
02493      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02494      MOVE -1                     TO  LINENOL.
02495      MOVE AL-UANOF               TO  LINENOA  RECVDTA
02496                                      RECVTYPA.
02497      MOVE LOW-VALUES             TO  EL150CO.
02498      MOVE 'F'                    TO  DIRECTION-SWITCH
02499      MOVE +1                     TO  DISPLAY-CNT
02500                                      SUB-1
02501                                      PI-LINE-NO.
02502      MOVE PI-TRLR-SUB (SUB-1)    TO  SUB-2.
02503      PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT.
02504
02505      GO TO 1010-BUILD-HISTORY-SCREEN.
02506
02507      EJECT
02508
02509  5000-VOID-PAYMENT.
02510
02511      MOVE LINENOI                TO  SUB-1.
02512
02513      IF PI-TRLR-TYPE (SUB-1) = '2'
02514          NEXT SENTENCE
02515      ELSE
02516      IF PI-TRLR-TYPE (SUB-1) = '4'
02517          MOVE ER-0666            TO  EMI-ERROR
02518          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02519          MOVE -1                 TO  LINENOL
02520          GO TO 8200-SEND-DATAONLY
02521      ELSE
02522      IF PI-TRLR-TYPE (SUB-1) = 'A'
02523          MOVE ER-0665            TO  EMI-ERROR
02524          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02525          MOVE -1                 TO  LINENOL
02526          GO TO 8200-SEND-DATAONLY
02527      ELSE
02528          MOVE ER-0664            TO  EMI-ERROR
02529          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02530          MOVE -1                 TO  LINENOL
02531          GO TO 8200-SEND-DATAONLY.
02532
02533      MOVE PI-TRLR-CERT (SUB-1)   TO  MSTR-CERT-NO.
02534      PERFORM 7500-READ-CLAIM-MSTR-UPDATE THRU 7500-EXIT.
02535
02536      MOVE PI-TRLR-CERT   (SUB-1) TO  TRLR-CERT-NO.
02537      MOVE PI-TRLR-SEQ-NO (SUB-1) TO  TRLR-SEQ-NO.
02538      PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.
02539
02540      IF AT-VOID-DT = LOW-VALUES OR SPACES
02541          NEXT SENTENCE
02542      ELSE
02543          MOVE ER-0800            TO  EMI-ERROR
02544          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02545          MOVE -1                 TO  LINENOL
02546          PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
02547          PERFORM 7010-UNLOCK-TRLR THRU 7010-EXIT
02548          GO TO 8200-SEND-DATAONLY.
02549
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*        IF AT-CASH-PAYMENT = 'N'
121802*            IF AT-CHECK-WRITTEN-DT = SPACES OR LOW-VALUES
121802*                MOVE ER-0833    TO  EMI-ERROR
121802*                MOVE -1         TO  LINENOL
121802*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*                PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
121802*                PERFORM 7010-UNLOCK-TRLR THRU 7010-EXIT
121802*                GO TO 8200-SEND-DATAONLY.
121802*
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*      IF AT-CASH-PAYMENT = 'N'
121802*        IF AT-RECORDED-DT = SAVE-BIN-DATE
121802*          NEXT SENTENCE
121802*        ELSE
121802*          IF PI-PROCESSOR-USER-ALMIGHTY = 'Y'
121802*            NEXT SENTENCE
121802*          ELSE
121802*            MOVE ER-0816        TO  EMI-ERROR
121802*            MOVE -1             TO  LINENOL
121802*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*            PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
121802*            PERFORM 7010-UNLOCK-TRLR THRU 7010-EXIT
121802*            GO TO 8200-SEND-DATAONLY.
121802*
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        IF CLAIM-IS-CLOSED
121802*            IF SETUP-ERRORS
121802*                MOVE ER-0941    TO EMI-ERROR
121802*                MOVE -1         TO LINENOL
121802*                PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT
121802*                PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
121802*                PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT
121802*                GO TO 8200-SEND-DATAONLY
121802*            ELSE
121802*            IF BENEFITS-CHANGED
121802*                IF SYSTEM-MODIFY-CAP
121802*                    NEXT SENTENCE
121802*                  ELSE
121802*                    MOVE ER-0942  TO EMI-ERROR
121802*                    MOVE -1       TO LINENOL
121802*                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*                    PERFORM 7510-UNLOCK-CLAIM-MSTR
121802*                       THRU 7510-EXIT
121802*                    PERFORM 7010-UNLOCK-TRLR THRU 7010-EXIT
121802*                    GO TO 8200-SEND-DATAONLY.
121802*
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        IF AT-CASH-PAYMENT = 'N'
121802*            IF SYSTEM-MODIFY-CAP
121802*                NEXT SENTENCE
121802*            ELSE
121802*                IF AT-RECORDED-DT = SAVE-BIN-DATE
121802*                    NEXT SENTENCE
121802*                ELSE
121802*                    MOVE ER-0920  TO EMI-ERROR
121802*                    MOVE -1       TO LINENOL
121802*                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*                    PERFORM 7510-UNLOCK-CLAIM-MSTR
121802*                       THRU 7510-EXIT
121802*                    PERFORM 7010-UNLOCK-TRLR THRU 7010-EXIT
121802*                    GO TO 8200-SEND-DATAONLY.
121802*
121802*    IF PI-COMPANY-ID = 'DMD'
121802*       IF AT-PAYMENT-TYPE NOT = '4' AND '5' AND '6'
121802*          MOVE 'O'              TO  CL-CLAIM-STATUS
121802*       END-IF
121802*       PERFORM 5300-CREATE-DMO THRU 5300-EXIT.
121802*
02619      MOVE AT-CHECK-WRITTEN-DT    TO  WS-CHECK-WRITTEN-DT.
02620      MOVE AT-PAYMENT-APPROVAL-SW TO  WS-PMT-APPROVAL-SW.
02621      MOVE AT-AMOUNT-PAID         TO  WS-AMOUNT-PAID.
02622      MOVE AT-PAYMENT-ORIGIN      TO  WS-PAYMENT-ORIGIN.
02623      MOVE AT-CV-PMT-CODE         TO  WS-CV-PMT-CODE.
02624
022106     IF AT-PAYMENT-TYPE NOT = '5' AND '6' AND 'I'
02626          SUBTRACT AT-AMOUNT-PAID    FROM CL-TOTAL-PAID-AMT
02627          SUBTRACT AT-DAYS-IN-PERIOD FROM CL-NO-OF-DAYS-PAID
02628          IF AT-PAYMENT-TYPE NOT = '4'
02629              SUBTRACT +1 FROM CL-NO-OF-PMTS-MADE
02630              IF AT-PAID-THRU-DT NOT = CL-PAID-THRU-DT OR
02631                 AT-RECORDED-BY = 'ZZZZ'
02632                  NEXT SENTENCE
02633              ELSE
02634                  MOVE AT-PREV-LAST-PMT-DT    TO  CL-LAST-PMT-DT
02635                  MOVE AT-PREV-PAID-THRU-DT   TO  CL-PAID-THRU-DT
02636                  MOVE AT-PREV-LAST-PMT-AMT   TO  CL-LAST-PMT-AMT.
02637
02638      IF CL-NO-OF-DAYS-PAID < ZERO
02639          MOVE +0                 TO  CL-NO-OF-DAYS-PAID.
02640
02641      IF CL-NO-OF-PMTS-MADE < ZERO
02642          MOVE +0                 TO  CL-NO-OF-PMTS-MADE.
02643
02644      MOVE SAVE-BIN-DATE          TO  AT-VOID-DT
02645                                      CL-LAST-REOPEN-DT.
02646
02647      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
02648      MOVE '1'                    TO  CNTL-REC-TYPE.
02649      MOVE SPACES                 TO  CNTL-ACCESS.
02650      MOVE +0                     TO  CNTL-SEQ-NO.
02651      MOVE 'CNTL'                 TO  FILE-SWITCH.
02652
02653      PERFORM 7900-READ-CONTROL-FILE THRU 7900-EXIT.
02654
02655      MOVE CF-PAYMENT-APPROVAL-SW TO  WS-CF-PAYMENT-APPROVAL-SW.
02656
02657      IF CF-PMT-APPROVAL-USED
02658          MOVE 'V'                TO  AT-PAYMENT-APPROVAL-SW.
02659
02660 ******************************************************************
02661 **  1.  BYPASS READING THE RECON RECORD FOR THE FOLLOWING       **
02662 **      REASONS:                                                **
02663 **      A.  NON-CASH PAYMENT                                    **
02664 **      B.  CHECK HAS NOT BEEN PRINTED                          **
02665 **      C.  USER IS NOT A RECON USER                            **
02666 **      D.  PAYMENT IS A MANUAL (OFFLINE) PAYMENT               **
02667 ******************************************************************
02668
02669      IF AT-CASH-PAYMENT = 'N'
02670          GO TO 5005-CONT-VOID.
02671
02672      IF AT-CHECK-NO = SPACES OR LOW-VALUES
02673          GO TO 5005-CONT-VOID.
02674
02675      IF CF-CLAIMS-CHECK-RECON-USER NOT = 'Y'
02676          GO TO 5005-CONT-VOID.
02677
02678      IF OFFLINE-PMT
02679          GO TO 5005-CONT-VOID.
02680
02681 ******************************************************************
02682 **  1.  RECON SW VALUES = :                                     **
02683 **      A.  R = CHECK HAS BEEN REDEEMED - CANNOT BE VOIDED      **
02684 **      B.  X = RECON RECORD NOT FOUND                          **
02685 ******************************************************************
02686
02687      MOVE 'RCON'                     TO  FILE-SWITCH.
02688      PERFORM 5700-UPDATE-RECON THRU 5700-EXIT.
02689      IF WS-RECON-SW = 'R'
02690          PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
02691          PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT
02692          GO TO 8200-SEND-DATAONLY.
02693
121802*    IF WS-RECON-SW = 'X'
121802*        IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CIG' OR 'CUK'
121802*            PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
121802*            PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT
121802*            GO TO 8200-SEND-DATAONLY.
02699
02700  5005-CONT-VOID.
02701      IF AT-RECORDED-BY = 'ZZZZ'
02702          GO TO 5010-BYPASS.
02703
02704      MOVE '7'                    TO  PI-PAY-TYPE.
02705      MOVE AT-PAYMENT-TYPE        TO  WS-PAY-TYPE.
02706
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        MOVE 88888888           TO WS-CK-Q-CONTROL
121802*     ELSE
02710          MOVE 99999999           TO WS-CK-Q-CONTROL.
02711
02712      IF AT-CHECK-QUE-CONTROL > ZEROS AND < WS-CK-Q-CONTROL
02713          PERFORM 5200-UPDATE-CHECK-QUE THRU 5299-EXIT
02714      ELSE
02715          IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES AND SPACES
02716              MOVE 'Y'                TO  WS-PRINTED-SW
02717                                          WS-RELEASED-SW
02718          ELSE
02719              MOVE 'N'                TO  WS-PRINTED-SW
02720                                          WS-RELEASED-SW.
02721
02722  5010-BYPASS.
02723
02724      IF PAYMENT-HAS-BEEN-PRINTED OR
02725         OFFLINE-PMT
02726          MOVE CF-CURRENT-MONTH-END   TO  AT-VOID-SELECT-DT
02727      ELSE
02728          MOVE LOW-VALUES             TO  AT-PMT-SELECT-DT
02729                                          AT-VOID-SELECT-DT.
02730
02731      MOVE WS-VOID-CODE               TO  AT-VOID-TYPE.
02732
02733      MOVE PI-PROCESSOR-ID        TO  AT-PAYMENT-LAST-UPDATED-BY.
02734      MOVE SAVE-BIN-DATE          TO  AT-PAYMENT-LAST-MAINT-DT.
02735
02736      PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.
071510     IF WS-PAY-TYPE NOT = 'I'
071510        PERFORM 5400-UPDATE-ZERO-TRAILER
071510                                 THRU 5400-EXIT
071510     END-IF
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'
121802*        PERFORM 5500-UPDATE-POLICY-MASTER THRU 5500-EXIT
121802*    ELSE
02742          PERFORM 5600-UPDATE-CERT THRU 5600-EXIT.
02743
02744      MOVE CL-CONTROL-PRIMARY     TO  ELACTQ-KEY.
02745
071510     IF WS-PAY-TYPE = '4' OR '5' OR '6' OR 'I'
02747          GO TO 5020-CONTINUE-VOID.
02748
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'
121802*        IF CL-NO-OF-PMTS-MADE > +0
121802*            GO TO 5020-CONTINUE-VOID.
02752
02753      MOVE 'O'                    TO  CL-CLAIM-STATUS.
02754
02755  5020-CONTINUE-VOID.
02756
02757      PERFORM 7600-REWRITE-CLAIM-MSTR THRU 7600-EXIT.
02758
02759      IF WS-PAYMENT-ORIGIN = '3'
02760          GO TO 5100-CONTINUE.
02761
02762      IF PAYMENT-HAS-BEEN-PRINTED AND
02763         NOT WS-CF-PMT-APPROVAL-USED
02764          GO TO 5100-CONTINUE.
02765
02766      MOVE 'ACTQ'                 TO  FILE-SWITCH.
02767      PERFORM 7700-READ-ELACTQ THRU 7799-EXIT.
02768
02769  5100-CONTINUE.
02770
02771      MOVE ER-0000                TO  EMI-ERROR.
02772      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02773      MOVE -1                     TO  LINENOL.
02774      MOVE AL-UANOF               TO  LINENOA  RECVDTA
02775                                      RECVTYPA.
02776      MOVE LOW-VALUES             TO  EL150CO.
02777      MOVE 'F'                    TO  DIRECTION-SWITCH
02778      MOVE +1                     TO  DISPLAY-CNT
02779                                      SUB-1
02780                                      PI-LINE-NO.
02781      MOVE PI-TRLR-SUB (SUB-1)    TO  SUB-2.
02782      PERFORM 0800-RECOVER-TEMP-STORAGE THRU 0899-EXIT.
02783
02784      GO TO 1010-BUILD-HISTORY-SCREEN.
02785
02786  EJECT
02787  5200-UPDATE-CHECK-QUE.
02788
02789      MOVE PI-COMPANY-CD          TO  CHKQ-COMP-CD.
02790      MOVE AT-CHECK-QUE-CONTROL   TO  CHKQ-CONTROL.
02791      MOVE AT-CHECK-QUE-SEQUENCE  TO  CHKQ-SEQ-NO.
02792
02793      
      * EXEC CICS HANDLE CONDITION
02794 *        NOTFND   (5290-NOTFND)
02795 *    END-EXEC.
      *    MOVE '"$I                   ! * #00007017' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303037303137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02796
02797      
      * EXEC CICS READ
02798 *        DATASET   ('ELCHKQ')
02799 *        RIDFLD    (ELCHKQ-KEY)
02800 *        SET       (ADDRESS OF CHECK-QUE)
02801 *        UPDATE
02802 *    END-EXEC.
           MOVE 'ELCHKQ' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00007021' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCHKQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02803
02804      MOVE 'Y'                    TO  WS-RELEASED-SW.
02805
02806      IF CQ-TIMES-PRINTED = +0
02807          MOVE 'N'                TO  WS-PRINTED-SW
02808          GO TO 5210-DELETE-CHECK-QUE
02809      ELSE
02810          MOVE 'Y'                TO  WS-PRINTED-SW.
02811
02812      MOVE WS-VOID-CODE           TO  CQ-VOID-INDICATOR.
02813
02814      
      * EXEC CICS REWRITE
02815 *        DATASET   ('ELCHKQ')
02816 *        FROM      (CHECK-QUE)
02817 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
           MOVE 'ELCHKQ' TO DFHEIV1
      *    MOVE '&& L                  %   #00007038' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02818
02819      GO TO 5299-EXIT.
02820
02821  5210-DELETE-CHECK-QUE.
02822
02823      
      * EXEC CICS DELETE
02824 *        DATASET   ('ELCHKQ')
02825 *    END-EXEC.
           MOVE 'ELCHKQ' TO DFHEIV1
      *    MOVE '&(                    &   #00007047' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02826
02827      MOVE +0                     TO  AT-CHECK-QUE-CONTROL
02828                                      AT-CHECK-QUE-SEQUENCE.
02829      GO TO 5299-EXIT.
02830
02831  5290-NOTFND.
02832      MOVE 'N'                    TO  WS-PRINTED-SW
02833                                      WS-RELEASED-SW.
02834  5299-EXIT.
02835      EXIT.
02836                                  EJECT
121802*5300-CREATE-DMO. Remove as obsolete or dead code
121802*5300-CONT. Remove as obsolete or dead code
121802*5300-NOTE-NOT-FOUND. Remove as obsolete or dead code
121802*5300-EXIT. Remove as obsolete or dead code
121802*5350-FORMAT-LAST-NAME-1ST. Remove as obsolete or dead code
121802*5350-EXIT. Remove as dead code
121802*5360-MOVE-NAME. Remove as dead code
121802*5360-MOVE-NAME-CYCLE. Remove as dead code
121802*5360-EXIT. Remove as dead code
03261      EJECT
03262  5400-UPDATE-ZERO-TRAILER.
03263
03264      MOVE ELMSTR-KEY             TO  ELTRLR-KEY.
03265
03266      MOVE ZEROS                  TO  TRLR-SEQ-NO.
03267      PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.
03268
03269      IF WS-PAY-TYPE = '5'
03270          SUBTRACT WS-AMOUNT-PAID FROM AT-ITD-CHARGEABLE-EXPENSE.
03271
03272      IF WS-PAY-TYPE = '6'
03273          SUBTRACT WS-AMOUNT-PAID FROM AT-ITD-PAID-EXPENSES.
03274
03275      IF AT-INITIAL-MANUAL-RESERVE NOT = ZEROS
03276          ADD WS-AMOUNT-PAID      TO  AT-CURRENT-MANUAL-RESERVE.
03277
03278  5410-CHECK-OPEN-CLOSE.
03279
03280      IF PI-PAY-TYPE = '5' OR '6'
03281          PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT
03282          GO TO 5400-EXIT.
03283
03284      IF PI-PAY-TYPE = '1' OR '4' OR '7'
03285         IF CLAIM-IS-OPEN
03286            PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT
03287            GO TO 5400-EXIT.
03288
03289      IF PI-PAY-TYPE = '2' OR '3'
03290         IF CLAIM-IS-CLOSED
03291            PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT
03292            GO TO 5400-EXIT.
03293
03294      MOVE 1                      TO  SUB.
03295
03296  5420-LOOP.
03297
03298      IF AT-OPEN-CLOSE-TYPE (SUB) = SPACES
03299          MOVE SAVE-BIN-DATE      TO  AT-OPEN-CLOSE-DATE (SUB)
03300          MOVE 'O'                TO  AT-OPEN-CLOSE-TYPE (SUB)
03301          MOVE 'FORCE'            TO  AT-OPEN-CLOSE-REASON (SUB)
03302          PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT
03303          GO TO 5400-EXIT.
03304
03305      IF SUB = 6
03306       MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1)
03307       MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2)
03308       MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3)
03309       MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4)
03310       MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5)
03311       MOVE SPACES                    TO AT-OPEN-CLOSE-HISTORY (6)
03312       GO TO 5420-LOOP.
03313
03314      ADD 1 TO SUB.
03315      GO TO 5420-LOOP.
03316
03317  5400-EXIT.
03318      EXIT.
03319
03320      EJECT
121802*5500-UPDATE-POLICY-MASTER. Remove as dead code
121802*5500-UPDATE-LF-POLICY-DATA. Remove as dead code
121802*5500-UPDATE-AH-POLICY-DATA. Remove as dead code
121802*5500-UPDATE-CLAIM-HISTORY. Remove as dead code
121802*5500-FINISH-POLICY-UPDATE. Remove as dead code
121802*5500-EXIT. Remove as dead code
03468  5600-UPDATE-CERT.
03469
03470      MOVE PI-COMPANY-CD          TO  CERT-COMP-CD.
03471      MOVE CL-CERT-CARRIER        TO  CERT-CARRIER.
03472      MOVE CL-CERT-GROUPING       TO  CERT-GROUPING.
03473      MOVE CL-CERT-STATE          TO  CERT-STATE.
03474      MOVE CL-CERT-ACCOUNT        TO  CERT-ACCOUNT.
03475      MOVE CL-CERT-EFF-DT         TO  CERT-EFF-DT.
03476      MOVE CL-CERT-NO             TO  CERT-CERT-NO.
03477      MOVE 'CERT'                 TO  FILE-SWITCH.
03478
03479      PERFORM 7800-READ-CERT-UPDATE.
03480
100518     IF CL-CLAIM-TYPE NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
03482          GO TO 5610-AH-VOID.
03483
03484      MOVE CM-LF-BENEFIT-CD       TO  WS-BEN-CD.
03485      MOVE WS-ACCESS              TO  CNTL-ACCESS.
03486      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
03487      MOVE '4'                    TO  CNTL-REC-TYPE.
03488      MOVE ZEROS                  TO  CNTL-SEQ-NO.
03489      MOVE 'BENE'                 TO  FILE-SWITCH.
03490      MOVE +0                     TO  SUB.
03491      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.
03492      IF NO-BENEFIT-FOUND
03493          GO TO 8400-NOT-FOUND.
03494
03495      MOVE CF-LF-COVERAGE-TYPE (SUB)  TO  WS-LF-COVERAGE-TYPE.
03496
03497      IF PI-LIFE-OVERRIDE-L1 = 'P' OR
03498         WS-LF-COVERAGE-TYPE = 'P'
03499          IF WS-PAY-TYPE = '4'
03500              SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
03501              IF CM-LF-CURRENT-STATUS = '1' OR '2'
03502                  PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
03503                  GO TO 5600-EXIT
03504              ELSE
03505                  MOVE CM-LF-STATUS-AT-DEATH  TO
03506                                              CM-LF-CURRENT-STATUS
03507                  MOVE SPACES             TO  CM-LF-STATUS-AT-DEATH
03508                  MOVE LOW-VALUES         TO  CM-LF-DEATH-EXIT-DT
03509                                              CM-LF-DEATH-DT
03510                  PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
03511                  GO TO 5600-EXIT.
03512
03513      IF WS-PAY-TYPE = '4'
03514          SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
03515          PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
03516          GO TO 5600-EXIT.
03517
03518      IF WS-PAY-TYPE = '2'
03519          IF CM-LF-CURRENT-STATUS = '1' OR '2'
03520              SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
03521              PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
03522              GO TO 5600-EXIT
03523          ELSE
03524              MOVE CM-LF-STATUS-AT-DEATH TO  CM-LF-CURRENT-STATUS
03525              MOVE SPACES                TO  CM-LF-STATUS-AT-DEATH
03526              SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
03527              MOVE LOW-VALUES            TO  CM-LF-DEATH-EXIT-DT
03528                                             CM-LF-DEATH-DT
03529              PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
03530              GO TO 5600-EXIT
03531      ELSE
03532          GO TO 5620-UNLOCK-CERT.
03533
03534  5610-AH-VOID.
03535
03536      IF WS-PAY-TYPE = '4'
03537          SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT
03538          PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
03539          GO TO 5600-EXIT.
03540
03541      IF WS-PAY-TYPE = '3'
03542          MOVE CM-AH-STATUS-AT-SETTLEMENT
03543                                  TO  CM-AH-CURRENT-STATUS
03544          MOVE SPACES             TO  CM-AH-STATUS-AT-SETTLEMENT
03545          SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT
03546          MOVE LOW-VALUES         TO  CM-AH-SETTLEMENT-EXIT-DT
03547                                      CM-AH-SETTLEMENT-DT
03548          PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
03549          GO TO 5600-EXIT
03550      ELSE
03551          GO TO 5620-UNLOCK-CERT.
03552
03553  5620-UNLOCK-CERT.
03554
03555      
      * EXEC CICS UNLOCK
03556 *        DATASET   ('ELCERT')
03557 *    END-EXEC.
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&*                    #   #00007223' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03558
03559  5600-EXIT.
03560      EXIT.
03561
03562      EJECT
03563  5700-UPDATE-RECON.
03564
03565      
      * EXEC CICS HANDLE CONDITION
03566 *        NOTFND    (5700-NOT-FOUND)
03567 *        NOTOPEN   (8500-FILE-NOTOPEN)
03568 *    END-EXEC.
      *    MOVE '"$IJ                  ! + #00007233' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303037323333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03569
03570      MOVE PI-COMPANY-CD               TO  RCON-COMPANY-CD.
03571      MOVE AT-CHECK-NO                 TO  RCON-CHECK-NO.
03572      MOVE 'C'                         TO  RCON-CHECK-ORIGIN.
03573      MOVE SPACES                      TO  RCON-GL-ACCOUNT-NO.
03574
03575      
      * EXEC CICS READ
03576 *        DATASET   ('ELRCON')
03577 *        RIDFLD    (ELRCON-KEY)
03578 *        SET       (ADDRESS OF CHECK-RECONCILIATION)
03579 *        UPDATE
03580 *    END-EXEC.
           MOVE 'ELRCON' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00007243' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELRCON-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECONCILIATION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03581
03582 ******************************************************************
03583 *         IF THE CHECK HAS BEEN REDEEMED - DO NOT VOID
03584 ******************************************************************
03585
03586      IF RC-STATUS = 'R'
03587          MOVE 'X'                TO  WS-RECON-SW
03588          MOVE ER-0823            TO  EMI-ERROR
03589          MOVE -1                 TO  LINENOL
03590          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03591          
      * EXEC CICS UNLOCK
03592 *            DATASET   ('ELRCON')
03593 *        END-EXEC
           MOVE 'ELRCON' TO DFHEIV1
      *    MOVE '&*                    #   #00007259' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03594          GO TO 5700-EXIT
03595      ELSE
03596          MOVE ' '                TO  WS-RECON-SW.
03597
03598      MOVE WS-VOID-CODE           TO  RC-STATUS.
03599
03600      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
03601      MOVE '5'                    TO  DC-OPTION-CODE.
03602      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
03603      MOVE DC-GREG-DATE-1-MDY     TO  WS-WORK-DATE.
03604      IF WS-WORK-YY > 50
03605          MOVE '19'               TO  WS-RCON-YY-1
03606          MOVE WS-WORK-YY         TO  WS-RCON-YY-2
03607          MOVE WS-WORK-MM         TO  WS-RCON-MM
03608          MOVE WS-WORK-DD         TO  WS-RCON-DD
03609      ELSE
03610          MOVE '20'               TO  WS-RCON-YY-1
03611          MOVE WS-WORK-YY         TO  WS-RCON-YY-2
03612          MOVE WS-WORK-MM         TO  WS-RCON-MM
03613          MOVE WS-WORK-DD         TO  WS-RCON-DD.
03614
03615      MOVE WS-RCON-DATE           TO  RC-STATUS-DATE.
03616
03617      MOVE PI-PROCESSOR-ID        TO  RC-LAST-MAINT-BY.
03618      MOVE SAVE-BIN-DATE          TO  RC-LAST-MAINT-DT.
03619      MOVE EIBTIME                TO  RC-LAST-MAINT-HHMMSS.
03620
03621      
      * EXEC CICS REWRITE
03622 *        DATASET   ('ELRCON')
03623 *        FROM      (CHECK-RECONCILIATION)
03624 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-RECONCILIATION
             TO DFHEIV11
           MOVE 'ELRCON' TO DFHEIV1
      *    MOVE '&& L                  %   #00007289' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CHECK-RECONCILIATION, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03625
03626      GO TO 5700-EXIT.
03627
03628  5700-NOT-FOUND.
03629
03630      MOVE 'X'                    TO  WS-RECON-SW.
03631      MOVE -1                     TO  LINENOL.
03632      MOVE ER-0801                TO  EMI-ERROR.
03633      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03634
03635  5700-EXIT.
03636      EXIT.
03637      EJECT
03638  7000-READ-TRLR-UPDATE.
03639
03640      MOVE PI-COMPANY-CD          TO  TRLR-COMP-CD.
03641      MOVE PI-CARRIER             TO  TRLR-CARRIER.
03642      MOVE PI-CLAIM-NO            TO  TRLR-CLAIM-NO.
03643
03644      
      * EXEC CICS READ
03645 *        DATASET   ('ELTRLR')
03646 *        RIDFLD    (ELTRLR-KEY)
03647 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
03648 *        UPDATE
03649 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00007312' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03650
03651  7000-EXIT.
03652      EXIT.
03653
03654  7010-UNLOCK-TRLR.
03655
03656      
      * EXEC CICS UNLOCK
03657 *        DATASET   ('ELTRLR')
03658 *    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&*                    #   #00007324' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03659
03660  7010-EXIT.
03661      EXIT.
03662
03663  7100-REWRITE-TRLR.
03664
03665      MOVE PI-PROCESSOR-ID        TO  PI-UPDATE-BY.
03666      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS
03667                                      PI-UPDATE-HHMMSS.
03668
03669      
      * EXEC CICS REWRITE
03670 *        DATASET   ('ELTRLR')
03671 *        FROM      (ACTIVITY-TRAILERS)
03672 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00007337' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03673
03674  7100-EXIT.
03675      EXIT.
03676
03677      EJECT
03678  7200-FIND-BENEFIT.
03679
03680      MOVE 'N'                    TO  WS-BEN-SEARCH-SW.
03681
03682      
      * EXEC CICS HANDLE CONDITION
03683 *        ENDFILE   (7200-EXIT)
03684 *        NOTFND    (7200-EXIT)
03685 *    END-EXEC.
      *    MOVE '"$''I                  ! , #00007350' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303037333530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03686
03687      
      * EXEC CICS READ
03688 *        DATASET   ('ELCNTL')
03689 *        RIDFLD    (ELCNTL-KEY)
03690 *        SET       (ADDRESS OF CONTROL-FILE)
03691 *        GTEQ
03692 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00007355' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03693
03694      IF CNTL-COMP-ID NOT  = CF-COMPANY-ID OR
03695         CNTL-REC-TYPE NOT = CF-RECORD-TYPE
03696          GO TO 7200-EXIT.
03697
03698      PERFORM 7200-BENEFIT-DUMMY THRU 7200-DUMMY-EXIT
03699          VARYING SUB FROM 1 BY 1 UNTIL
03700          ((SUB > 8) OR
03701          (CF-BENEFIT-CODE (SUB) = WS-BEN-CD)).
03702
03703      IF SUB NOT = 9
03704          MOVE 'Y'             TO  WS-BEN-SEARCH-SW.
03705
03706      GO TO 7200-EXIT.
03707
03708  7200-BENEFIT-DUMMY.
03709
03710  7200-DUMMY-EXIT.
03711      EXIT.
03712
03713  7200-EXIT.
03714      EXIT.
03715  7400-DEL-TEMP-STOR-TABLE.
03716
03717      
      * EXEC CICS HANDLE CONDITION
03718 *        QIDERR   (7400-EXIT)
03719 *    END-EXEC.
      *    MOVE '"$N                   ! - #00007385' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303037333835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03720
03721      
      * EXEC CICS DELETEQ TS
03722 *        QUEUE    (WS-TABLE-QID)
03723 *    END-EXEC.
      *    MOVE '*&                    #   #00007389' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TABLE-QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03724
03725  7400-EXIT.
03726      EXIT.
03727
03728      EJECT
03729  7500-READ-CLAIM-MSTR-UPDATE.
03730
03731      MOVE PI-COMPANY-CD          TO  MSTR-COMP-CD.
03732      MOVE PI-CARRIER             TO  MSTR-CARRIER.
03733      MOVE PI-CLAIM-NO            TO  MSTR-CLAIM-NO.
03734
03735      
      * EXEC CICS READ
03736 *        DATASET   ('ELMSTR')
03737 *        RIDFLD    (ELMSTR-KEY)
03738 *        SET       (ADDRESS OF CLAIM-MASTER)
03739 *        UPDATE
03740 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00007403' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03741
03742  7500-EXIT.
03743      EXIT.
03744
03745  7510-UNLOCK-CLAIM-MSTR.
03746
03747      
      * EXEC CICS UNLOCK
03748 *        DATASET   ('ELMSTR')
03749 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&*                    #   #00007415' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03750
03751  7510-EXIT.
03752      EXIT.
03753
03754  7600-REWRITE-CLAIM-MSTR.
03755
03756      MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT.
03757      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.
03758      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.
03759      MOVE '3'                    TO  CL-LAST-MAINT-TYPE.
03760
03761      
      * EXEC CICS REWRITE
03762 *        DATASET   ('ELMSTR')
03763 *        FROM      (CLAIM-MASTER)
03764 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&& L                  %   #00007429' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03765
03766  7600-EXIT.
03767      EXIT.
03768
03769      EJECT
03770  7700-READ-ELACTQ.
03771
03772      
      * EXEC CICS HANDLE CONDITION
03773 *        NOTFND   (7799-EXIT)
03774 *    END-EXEC.
      *    MOVE '"$I                   ! . #00007440' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303037343430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03775
03776      
      * EXEC CICS READ
03777 *        DATASET   ('ELACTQ')
03778 *        RIDFLD    (ELACTQ-KEY)
03779 *        SET       (ADDRESS OF ACTIVITY-QUE)
03780 *        UPDATE
03781 *    END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00007444' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03782
03783      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC
03784          MOVE ZEROS              TO  AQ-PMT-UNAPPROVED-COUNT.
03785
03786      IF AQ-PAYMENT-COUNTER NOT NUMERIC
03787          MOVE +0                 TO  AQ-PAYMENT-COUNTER.
03788
03789      IF WS-CF-PMT-APPROVAL-USED
03790          IF AQ-PMT-UNAPPROVED-COUNT > +0 AND
03791             WS-CHECK-WRITTEN-DT = LOW-VALUES AND
03792             WS-PMT-APPROVAL-SW = 'U'
03793              SUBTRACT +1 FROM AQ-PMT-UNAPPROVED-COUNT
03794              MOVE 'Y'           TO  WS-UPDATE-SW.
03795
03796      IF PAYMENT-NOT-PRINTED OR
03797         PAYMENT-NOT-RELEASED
03798          IF AQ-PAYMENT-COUNTER > +0
03799              SUBTRACT +1 FROM AQ-PAYMENT-COUNTER
03800              MOVE 'Y'         TO  WS-UPDATE-SW.
03801
03802      IF AQ-PAYMENT-COUNTER = +0
03803          MOVE ' '             TO  AQ-PENDING-PAYMENT-FLAG.
03804
03805      IF WS-UPDATE-SW = 'Y'
03806          IF AQ-PENDING-ACTIVITY-FLAGS = SPACES
03807              MOVE 'N'            TO  WS-UPDATE-SW
03808              GO TO 7720-DELETE-ACTIVITY-QUE
03809          ELSE
03810              MOVE 'N'            TO  WS-UPDATE-SW
03811      ELSE
03812          GO TO 7750-UNLOCK-ACTIVITY-QUE.
03813
03814  7710-REWRITE-ACTIVITY-QUE.
03815
03816      
      * EXEC CICS REWRITE
03817 *        DATASET   ('ELACTQ')
03818 *        FROM      (ACTIVITY-QUE)
03819 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&& L                  %   #00007484' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03820
03821      GO TO 7799-EXIT.
03822
03823  7720-DELETE-ACTIVITY-QUE.
03824
03825      
      * EXEC CICS DELETE
03826 *        DATASET   ('ELACTQ')
03827 *    END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&(                    &   #00007493' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03828
03829      GO TO 7799-EXIT.
03830
03831  7750-UNLOCK-ACTIVITY-QUE.
03832
03833      
      * EXEC CICS UNLOCK
03834 *        DATASET   ('ELACTQ')
03835 *    END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&*                    #   #00007501' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03836
03837      MOVE 'N'                    TO  WS-UPDATE-SW.
03838
03839  7799-EXIT.
03840      EXIT.
03841
03842      EJECT
03843  7800-READ-CERT-UPDATE.
03844
03845      
      * EXEC CICS READ
03846 *        DATASET   ('ELCERT')
03847 *        RIDFLD    (ELCERT-KEY)
03848 *        SET       (ADDRESS OF CERTIFICATE-MASTER)
03849 *        UPDATE
03850 *    END-EXEC.
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00007513' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03851
03852  7800-EXIT.
03853      EXIT.
03854
03855  7810-REWRITE-CERT.
03856
03857      
      * EXEC CICS REWRITE
03858 *        DATASET   ('ELCERT')
03859 *        FROM      (CERTIFICATE-MASTER)
03860 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&& L                  %   #00007525' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03861
03862  7810-EXIT.
03863      EXIT.
03864
03865      EJECT
03866  7900-READ-CONTROL-FILE.
03867
03868      
      * EXEC CICS READ
03869 *        DATASET   ('ELCNTL')
03870 *        RIDFLD    (ELCNTL-KEY)
03871 *        SET       (ADDRESS OF CONTROL-FILE)
03872 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00007536' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03873
03874  7900-EXIT.
03875      EXIT.
03876
03877      EJECT
03878  8000-LOAD-ERROR-MESSAGES.
03879      IF EMI-NO-ERRORS
03880          GO TO 8000-EXIT.
03881
03882      IF EMI-NUMBER-OF-LINES = 1
03883          MOVE EMI-LINE1          TO  ERRMSG1O
03884          GO TO 8000-EXIT.
03885
03886      MOVE EMI-LINE1              TO  ERRMSG1O.
03887
03888  8000-EXIT.
03889      EXIT.
03890
03891  8100-SEND-INITIAL-MAP.
03892
03893      IF PI-FULL-SCREEN-SHOWN
03894          GO TO 8200-SEND-DATAONLY.
03895
03896      MOVE 'Y'                    TO PI-FULL-SCREEN-IND.
03897      MOVE SAVE-DATE              TO  RUNDTEO.
03898      MOVE EIBTIME                TO  TIME-IN.
03899      MOVE TIME-OUT               TO  RUNTIMEO.
03900      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
03901      MOVE PI-PRIME-CERT          TO  PCERTNOO.
03902      MOVE PI-PRIME-SUFX          TO  SUFXO.
03903      MOVE PI-PRIME-HDG           TO  PRIMEHDO.
03904      MOVE AL-SABON               TO  PCERTNOA
03905                                      SUFXA
03906                                      PRIMEHDA.
03907
121802*    IF PI-COMPANY-ID NOT = 'DMD'
03909          MOVE SPACES             TO PF6O.
03910          MOVE AL-SADOF           TO PF6A.
03911
03912      
      * EXEC CICS SEND
03913 *        MAP      (MAP-NAME)
03914 *        MAPSET   (MAPSET-NAME)
03915 *        FROM     (EL150CO)
03916 *        ERASE
03917 *        CURSOR
03918 *    END-EXEC.
           MOVE LENGTH OF
            EL150CO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00007580' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150CO, 
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
           
03919
03920      GO TO 9100-RETURN-TRAN.
03921
03922  8200-SEND-DATAONLY.
03923
03924      IF NOT PI-FULL-SCREEN-SHOWN
03925          GO TO 8100-SEND-INITIAL-MAP.
03926
03927      MOVE SAVE-DATE              TO  RUNDTEO.
03928      MOVE EIBTIME                TO  TIME-IN.
03929      MOVE TIME-OUT               TO  RUNTIMEO.
03930      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
03931
03932      MOVE PI-CARRIER             TO  CARRO.
03933      MOVE PI-CLAIM-NO            TO  CLMNOO.
03934      MOVE PI-PRIME-CERT          TO  PCERTNOO.
03935      MOVE PI-PRIME-SUFX          TO  SUFXO.
03936      MOVE PI-PRIME-HDG           TO  PRIMEHDO.
03937      MOVE AL-SABON               TO  PCERTNOA
03938                                      SUFXA
03939                                      PRIMEHDA.
03940
121802*    IF PI-COMPANY-ID NOT = 'DMD'
03942          MOVE SPACES             TO PF6O.
03943          MOVE AL-SADOF           TO PF6A.
03944
03945      
      * EXEC CICS SEND
03946 *        MAP      (MAP-NAME)
03947 *        MAPSET   (MAPSET-NAME)
03948 *        FROM     (EL150CO)
03949 *        DATAONLY
03950 *        CURSOR
03951 *    END-EXEC.
           MOVE LENGTH OF
            EL150CO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00007613' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150CO, 
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
           
03952
03953      GO TO 9100-RETURN-TRAN.
03954
03955  8300-SEND-TEXT.
03956      
      * EXEC CICS SEND TEXT
03957 *        FROM     (LOGOFF-TEXT)
03958 *        LENGTH   (LOGOFF-LENGTH)
03959 *        ERASE
03960 *        FREEKB
03961 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00007624' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363234' TO DFHEIV0(25:11)
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
           
03962
03963      
      * EXEC CICS RETURN
03964 *    END-EXEC.
      *    MOVE '.(                    ''   #00007631' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03965
03966  8400-NOT-FOUND.
03967      IF FILE-SWITCH = 'BENE'
03968          MOVE ER-0282            TO  EMI-ERROR.
03969
03970      MOVE -1                     TO  LINENOL.
03971      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03972
03973      IF PASS-SWITCH = 'A'
03974          GO TO 8100-SEND-INITIAL-MAP
03975      ELSE
03976          GO TO 8200-SEND-DATAONLY.
03977
03978  8500-FILE-NOTOPEN.
03979
03980      IF FILE-SWITCH = 'TRLR'
03981          MOVE ER-0172            TO  EMI-ERROR.
03982
03983      IF FILE-SWITCH = 'CERT'
03984          MOVE ER-0169            TO  EMI-ERROR.
03985
03986      IF FILE-SWITCH = 'CNTL'
03987          MOVE ER-0042            TO  EMI-ERROR.
03988
03989      IF FILE-SWITCH = 'ACTQ'
03990          MOVE ER-0338            TO  EMI-ERROR.
03991
03992      IF FILE-SWITCH = 'MSTR'
03993          MOVE ER-0154            TO  EMI-ERROR.
03994
03995      IF FILE-SWITCH = 'RCON'
03996          MOVE ER-0776            TO  EMI-ERROR.
03997
03998      IF FILE-SWITCH = 'PLCY'
03999          MOVE ER-9883            TO  EMI-ERROR.
04000
04001      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04002
04003      MOVE -1                     TO  LINENOL.
04004
04005      IF PASS-SWITCH = 'A'
04006          GO TO 8100-SEND-INITIAL-MAP
04007      ELSE
04008          GO TO 8200-SEND-DATAONLY.
04009
04010  8800-UNAUTHORIZED-ACCESS.
04011      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
04012      GO TO 8300-SEND-TEXT.
04013
04014  8810-PF23.
04015      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
04016      MOVE XCTL-005               TO  PGM-NAME.
04017      GO TO 9300-XCTL.
04018
04019  9100-RETURN-TRAN.
04020      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
04021      MOVE '150C'                 TO  PI-CURRENT-SCREEN-NO.
04022
04023      
      * EXEC CICS RETURN
04024 *        TRANSID    (TRANS-ID)
04025 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
04026 *        LENGTH     (PI-COMM-LENGTH)
04027 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00007691' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04028
04029  9200-RETURN-MAIN-MENU.
04030      MOVE XCTL-126               TO PGM-NAME.
04031      GO TO 9300-XCTL.
04032
04033  9300-XCTL.
04034      
      * EXEC CICS HANDLE CONDITION
04035 *        PGMIDERR   (9350-NOT-FOUND)
04036 *    END-EXEC.
      *    MOVE '"$L                   ! / #00007702' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303037373032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04037
04038      
      * EXEC CICS XCTL
04039 *        PROGRAM    (PGM-NAME)
04040 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
04041 *        LENGTH     (PI-COMM-LENGTH)
04042 *    END-EXEC.
      *    MOVE '.$C                   %   #00007706' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04043
04044  9350-NOT-FOUND.
04045      MOVE ER-0923                TO EMI-ERROR.
04046      MOVE -1                     TO LINENOL.
04047      PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT.
04048      PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT.
04049      PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT.
04050      GO TO 8200-SEND-DATAONLY.
04051
04052  9400-CLEAR.
04053      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
04054      GO TO 9300-XCTL.
04055
04056  9500-PF12.
04057      MOVE XCTL-010               TO  PGM-NAME.
04058      GO TO 9300-XCTL.
04059
04060  9600-PGMID-ERROR.
04061      
      * EXEC CICS HANDLE CONDITION
04062 *        PGMIDERR   (8300-SEND-TEXT)
04063 *    END-EXEC.
      *    MOVE '"$L                   ! 0 #00007729' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303037373239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04064
04065      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
04066      MOVE ' '                    TO  PI-ENTRY-CD-1.
04067      MOVE XCTL-005               TO  PGM-NAME.
04068      MOVE PGM-NAME               TO  LOGOFF-PGM.
04069      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
04070      GO TO 9300-XCTL.
04071
04072  9700-LINK-DATE-CONVERT.
04073      MOVE LINK-ELDATCV           TO PGM-NAME.
04074
04075      
      * EXEC CICS LINK
04076 *        PROGRAM    (PGM-NAME)
04077 *        COMMAREA   (DATE-CONVERSION-DATA)
04078 *        LENGTH     (DC-COMM-LENGTH)
04079 *    END-EXEC.
      *    MOVE '."C                   (   #00007743' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04080
04081  9700-EXIT.
04082      EXIT.
04083
04084  9800-DEEDIT.
04085
04086      
      * EXEC CICS BIF DEEDIT
04087 *        FIELD   (WS-DEEDIT-FIELD)
04088 *        LENGTH  (WS-DEEDIT-LENGTH)
04089 *    END-EXEC.
      *    MOVE '@"L                   #   #00007754' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 WS-DEEDIT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04090
04091  9800-EXIT.
04092      EXIT.
04093
04094  9900-ERROR-FORMAT.
04095      IF NOT EMI-ERRORS-COMPLETE
04096          MOVE LINK-001           TO PGM-NAME
04097          
      * EXEC CICS LINK
04098 *            PROGRAM    (PGM-NAME)
04099 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
04100 *            LENGTH     (EMI-COMM-LENGTH)
04101 *        END-EXEC.
      *    MOVE '."C                   (   #00007765' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04102
04103  9900-EXIT.
04104      EXIT.
04105
04106  9990-ABEND.
04107      MOVE -1                     TO  ENTERPFL.
04108      MOVE LINK-004               TO  PGM-NAME.
04109
04110      MOVE DFHEIBLK               TO  EMI-LINE1
04111      
      * EXEC CICS LINK
04112 *        PROGRAM   (PGM-NAME)
04113 *        COMMAREA  (EMI-LINE1)
04114 *        LENGTH    (72)
04115 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00007779' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04116
04117      MOVE EMI-LINE1              TO  ERRMSG1O.
04118      GO TO 8200-SEND-DATAONLY.
04119
04120  EJECT
04121  9995-SECURITY-VIOLATION.
04122 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00007807' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383037' TO DFHEIV0(25:11)
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
04123

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1502' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 1000-SHOW-CLAIM-HISTORY,
                     0100-FIRST-TIME-IN,
                     8500-FILE-NOTOPEN,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0900-BUILD-RELATED-TABLE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0690-QIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0890-QIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0950-SORT-ROUTINE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 0915-ELMSTR-ENDFILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 0945-ENDBR-ELTRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 2950-NO-MORE-TRAILERS,
                     2950-NO-MORE-TRAILERS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 5290-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 5700-NOT-FOUND,
                     8500-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 7200-EXIT,
                     7200-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 7400-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 7799-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 9350-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1502' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
