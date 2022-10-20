00001  IDENTIFICATION DIVISION.                                         11/20/97
00002                                                                   EL156
00003  PROGRAM-ID.                 EL156 .                                 LV098
00004 *              PROGRAM CONVERTED BY                                  CL*81
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*81
00006 *              CONVERSION DATE 05/17/94 13:55:56.                    CL*81
00007 *                            VMOD=2.098                              CL*98
00008 *                                                                 EL156
00008 *                                                                 EL156
00009 *AUTHOR.     LOGIC,INC.                                              CL*81
00010 *            DALLAS, TEXAS.                                          CL*81
00011                                                                   EL156
00024 *REMARKS.    TRANSACTION - EX29 - PAYMENT PROCESSING                 CL**8

121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST CALC
052506* 052506    2006030600001  AJRA  ADD PROOF DATE
071806* 071806    2004040700004  PEMA  ADD TONYS PROOF DATE TO INT CALC
082807* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
110807* 110807  IR2007110700001  AJRA  DO NOT CALC INT ON MANUAL PMTS
022208* 022208    2008021500001  AJRA  DO NOT DEFAULT PROOF DATE,FORCE ENTRY
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
060608* 060608    2008040300002  PEMA  ADD EFF DT TO ELCLMI PASS AREA
091808* 091808    2008022800002  AJRA  GET CHECK NUMBER FROM STATE CNTL FOR AK
012810* 012810    2007111300001  AJRA  EXPAND EDIT PATTERN 
041710* 041710    2007111300001  AJRA  ADD MSG AND CALC PMT FOR SC NET PAY + 6
092310* 092310  IR2010090900001  PEMA  CHANGE CRIT PERIOD EDIT
112210* 112210    2010091000003  AJRA  MICHIGAN FULL MONTH ON 1ST PAYMENT
042011* 042011  CR2011040600001  PEMA  ADD EOB PROCESSING FOR DCC
061511* 061511    2011042000002  AJRA  VERIFY 2ND BENEFICIARY SSN
030512* 030512    2011120900003  AJRA  ADD AHL COMPANY CODE
041812* 041812  IR2012041800001  AJRA  FIX TERM/REM ON SCREEN
013013* 013013    2012092400003  AJRA  SPECIAL EOB FOR ACCELERATED DEATH
020413* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM IND
020513* 020513    2012092400005  AJRA  LIFE PAYEE NAME SHOULD NOT = INS NAME
032813* 032813    2011013100001  AJRA  SELECT CLAIM PAYMENTS FOR REAUDIT
061013* 061013    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
110513* 110513    2013102900002  AJRA  FIX AK PAYEE STATE CODE ROUTINE
102413* 102413    2013100800001  AJRA  ADD SPECIAL RELEASE IND
111113* 111113    2013110600002  AJRA  CHG LEVEL 5 RESTRICTIONS TO LEVEL 4 & 5
030514* 030514    2014021300001  AJRA  FORCE LF PAYMENT > BENEFIT AMT
032414* 032414    2013092400002  AJRA  LIFE ADDIT PMTS TO ESTATE SHOULD USE I
032514* 032514    2013111100001  AJRA  VERIFY SSN FOR DISAB PMTS
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052814* 052814  CR2014041800001  AJRA  SET APPROVAL FLAG FOR 3550 MESSAGE
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
070714* 070714    2014052800001  PEMA  correct read on erpdef for DCC
100314* 100314  CR2014061900001  PEMA  Add pct of benefit funcionality
111714* 111714  CR2014073000001  PEMA  DRAFTS TO CHECKS
032015* 032015  CR2014121000003  TANA  Default prnt survey to 'N'for DCC
110515* 110515  IR2015103000001  TANA  Check app level for 3550 message
120115* 120115  CR2015082700002  PEMA  ADD JNT BORROWER PROCESSING
012016* 012016  IR2016011800003  PEMA  FIXED LENGTH OF PAYEE NAME
050616* 050616  CR2015030200004  TANA  Additional pmt Pd thru exp date
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
102617* 102617    2017101800003  TANA  Fix family leave auth level
022718* 022718    2017100200004  TANA  Set hold & pays > than 30 days
031218* 031218    2018030800002  TANA  Handle offline pmts w/ hold & pay
082218* 082218  CR2018051400001  TANA  Hold and Pay
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
112018* 112018  CR2018102900001  TANA  REQUIRE EOB NOTE STARTING WITH F
040819* 040819  IR2019030400004  TANA  ADD EDITS FOR HOLD AND PAY
043019* 043019  IR2019041900001  TANA  Correct total claim pymts msg
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
090821* 090821  CR2021081200003  PEMA  Add error if inc > act cnc dt
022122* 022122  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00025                                                                   EL156
00026      EJECT                                                        EL156
00027  ENVIRONMENT DIVISION.                                            EL156
00028                                                                   EL156
00029  DATA DIVISION.                                                   EL156
00030                                                                   EL156
00031  WORKING-STORAGE SECTION.                                         EL156
00032                                                                   EL156
00033  77  FILLER  PIC X(32)  VALUE '********************************'. EL156
00034  77  FILLER  PIC X(32)  VALUE '*    EL156 WORKING STORAGE     *'. EL156
00035  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.098 **********'.    CL*98
       77  WS-SSN  PIC X(9) VALUE ZEROS.
       77  WS-SET-NOTE2-MDT            PIC X  VALUE SPACES.
       77  C0                          PIC S999 COMP-3 VALUE +0.
       77  C1                          PIC S999 COMP-3 VALUE +0.
       77  C2                          PIC S999 COMP-3 VALUE +0.
       77  C3                          PIC S999 COMP-3 VALUE +0.
       77  s1                          PIC S999 COMP-3 VALUE +0.
       77  s2                          pic s999 comp-3 value +0.
       77  P1                          PIC S999 COMP-3 VALUE +0.
       77  WS-DDF-COMM-AND-MFEE        PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-ADMIN-FEES           PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-CSO-ADMIN-FEE        PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-1ST-YR-TOT-EXP       PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-STATE-EXT-DAYS-CHG       PIC X  VALUE ' '.
       77  TEX-FACT-8                  PIC S9V9(6)     COMP-3.
       77  WS-COMM-PCT                 PIC S9(5)V9(5)  COMP-3 VALUE +0.
       77  WS-TERM                     PIC S999 COMP-3 VALUE +0.
       77  ws-monthly-benefit          pic s9(11)v99 comp-3 value +0.
       77  ws-max-bens                 pic s999 comp-3 value +0.
       77  ws-prev-days-paid           pic s9(5) comp-3 value +0.
       77  ws-prev-amt-paid            pic s9(9)v99 comp-3 value +0.
       77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
       77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
       77  ws-pd-bens                  pic s9(5) comp-3 value +0.
100314 77  ws-work-ben-pct             pic s9v999 comp-3 value +0.
022122 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
022122 77  WS-EDIT-AGE                 PIC S999       COMP-3 VALUE ZERO.
022122 77  ws-MAX-TOT-BEN              pic s9(7)v99 comp-3 value +0.


00036                                                                   EL156
00037      COPY ELCSCTM.                                                   CL*43
00038                                                                   EL156
00039      COPY ELCSCRTY.                                                  CL*43
00040                                                                   EL156
012407 01  WS-BIN-PROOF-DT             PIC XX  VALUE LOW-VALUES.
00041  01  WS-DATE-AREA.                                                EL156
00042      12  SAVE-DATE           PIC X(8)    VALUE SPACES.               CL*82
00043      12  SAVE-BIN-DATE       PIC XX      VALUE SPACES.               CL*82
00044      12  SAVE-DATE-CCYYMMDD.                                         CL*82
00045          16  SAVE-DATE-CC            PIC XX      VALUE SPACES.       CL*88
00046          16  SAVE-DATE-YMD.                                          CL*82
00047              20  SAVE-DATE-YY        PIC XX      VALUE SPACES.       CL*88
00048              20  FILLER              PIC X(04)   VALUE SPACES.       CL*82
00049                                                                   EL156
       01  WS-SAVED-EOB-CODES          PIC X(60) VALUE SPACES.
00050  01  STANDARD-AREAS.                                              EL156
CIDMOD     12  WS-RESPONSE         PIC S9(8)   COMP.                       CL*55
CIDMOD         88  WS-RESP-NORMAL              VALUE +00.                  CL*55
CIDMOD         88  WS-RESP-ERROR               VALUE +01.                  CL*55
CIDMOD         88  WS-RESP-NOTFND              VALUE +13.
022106         88  WS-RESP-DUPREC              VALUE +14.
               88  WS-RESP-ENDFILE             VALUE +20.
00051      12  PMT-HEAD-A.                                                 CL*16
00052          16  FILLER          PIC X(09)   VALUE 'PAY-FROM '.          CL*16
00053          16  HDGA-VRBLE      PIC X(10)   VALUE 'PAY THRU  '.         CL*16
00054          16  FILLER          PIC X(47)   VALUE 'DAYS   PAYMENT AMT   CL*16
00055 -        '     RESERVES  TYPE    AMOUNT'.                            CL*16
00056      12  PMT-HEAD-B.                                                 CL*16
00057          16  FILLER          PIC X(12)   VALUE 'PAYS FROM   '.       CL*16
00058          16  HDGB-VRBLE      PIC X(12)   VALUE 'PAYS THRU   '.       CL*16
00059          16  FILLER          PIC X(10)   VALUE 'DAYS   BY '.         CL*16
00060      12  SC-ITEM             PIC S9(4)   VALUE +0001  COMP.       EL156
00061      12  GETMAIN-SPACE       PIC X       VALUE SPACE.             EL156
00062      12  MAP-NAME            PIC X(8)    VALUE 'EL156A'.          EL156
00063      12  MAP-NAMEA           PIC X(8)    VALUE 'EL156A'.          EL156
00064      12  MAP-NAMEB           PIC X(8)    VALUE 'EL156B'.          EL156
00065      12  MAPSET-NAME         PIC X(8)    VALUE 'EL156S'.          EL156
00066      12  TRANS-ID            PIC X(4)    VALUE 'EX29'.            EL156
00067      12  THIS-PGM            PIC X(8)    VALUE 'EL156'.           EL156
00068      12  PGM-NAME            PIC X(8).                            EL156
00069      12  TIME-IN             PIC S9(7).                           EL156
00070      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL156
00071          16  FILLER          PIC X.                               EL156
00072          16  TIME-OUT        PIC 99V99.                           EL156
00073          16  FILLER          PIC XX.                              EL156
00074      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL156
00075      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL156
00076      12  XCTL-126            PIC X(8)    VALUE 'EL126'.           EL156
00077      12  XCTL-141            PIC X(8)    VALUE 'EL141'.           EL156
00078      12  XCTL-157            PIC X(8)    VALUE 'EL157'.              CL*17
00079      12  XCTL-155            PIC X(8)    VALUE 'EL155'.           EL156
00080      12  XCTL-130            PIC X(8)    VALUE 'EL130'.           EL156
00081      12  XCTL-132            PIC X(8)    VALUE 'EL132'.           EL156
00082      12  LINK-001            PIC X(8)    VALUE 'EL001'.           EL156
00083      12  LINK-004            PIC X(8)    VALUE 'EL004'.           EL156
00084      12  LINK-1523           PIC X(8)    VALUE 'EL1523'.             CL*65
00085      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.            CL*20
00086      12  LINK-REMTERM        PIC X(8)    VALUE 'ELRTRM'.          EL156
00087      12  LINK-REMAMT         PIC X(8)    VALUE 'ELRAMT'.          EL156
00088      12  ELARCH-LENGTH       PIC S9(4)   VALUE +90     COMP.         CL*20
CIDMOD     12  WS-BLANK            PIC X       VALUE ' '.                    000
00089                                                                   EL156
00090  01  MISC-WORK-AREAS.                                             EL156
00091      12  W-INTEREST              PIC S9(7)V99  COMP-3 VALUE +0.      CL*93
00092      12  W-PAYMENT-TOTAL         PIC S9(7)V99  COMP-3 VALUE +0.      CL*93
00093      12  W-PERCENT               PIC S9V9(4)   COMP-3 VALUE +0.      CL*93
00094      12  W-TOTAL-INTEREST        PIC S9(7)V99  COMP-3 VALUE +0.      CL*93
00095      12  W-PAID-DAYS             PIC S999      COMP-3 VALUE +0.      CL*88
00096      12  W-PAID-MONTHS           PIC S999      COMP-3 VALUE +0.      CL*88
00097      12  W-REM                   PIC S999      COMP-3 VALUE +0.      CL*98
00098      12  W-REM-DAYS              PIC S999      COMP-3 VALUE +0.      CL*98
00099      12  W-TERM-IN-DAYS          PIC S9(4)     COMP-3.               CL*98
00100      12  W-REM-TERM-IN-DAYS      PIC S9(4)     COMP-3.               CL*98
00101      12  W-CALLED-NAME           PIC X(08).                          CL*93
00102      12  W-EOB-CODES.                                                CL*82
00103          16  W-EOB-CODE-GRP OCCURS 5 TIMES.                          CL*93
00104              20  W-EOB-CODE      PIC XXX.                            CL*93
00105              20  W-EOB-FILLER    PIC X.                              CL*95
00106      12  W-EOB-NDX               PIC S9        VALUE ZERO.           CL*93
00107      12  W-NAME-LAST             PIC X(15).                          CL*93
00108      12  W-NAME-FIRST            PIC X(15).                          CL*93
00109      12  W-NAME-MIDDLE.                                              CL*82
00110          16  FILLER              PIC X.                              CL*93
00111          16  W-NAME-MIDDLE-2     PIC X.                              CL*93
00112          16  FILLER              PIC X(13).                          CL*93
00113      12  W-DMD-CHECK-NO.                                             CL*82
00114          16  W-DMD-CHECK-NO-1    PIC X(4).                           CL*88
00115          16  W-DMD-CHECK-NO-2    PIC X.                              CL*88
00116          16  W-DMD-CHECK-NO-3    PIC XX.                             CL*88
00117                                                                      CL*88
00118      12  WS-COMPUTE-FROM-DT  PIC XX.                                 CL*79
00119      12  WS-PMT-MOS          PIC 999V99        VALUE ZEROS.          CL*88
00120      12  WS-HER-PART-DAYS    PIC 999V99        VALUE ZEROS.          CL*88
00121      12  WS-HER-MO-PMT-AMT   PIC 9(07)V99      VALUE ZEROS.          CL*76
00122      12  WS-FORM-CTL-SEQ-NO  PIC S9(4)   COMP  VALUE +0.             CL*68
00123      12  WS-HOLD-ELMSTR-KEY  PIC X(20) VALUE LOW-VALUES.             CL*81
00124      12  WS-PRINTED-SW       PIC X VALUE SPACES.                     CL*88
00125          88  PAYMENT-HAS-BEEN-PRINTED  VALUE 'Y'.                    CL*30
00126          88  PAYMENT-NOT-PRINTED       VALUE 'N'.                    CL*30
00127      12  WS-RELEASED-SW      PIC X VALUE SPACES.                     CL*88
00128          88  PAYMENT-NOT-RELEASED      VALUE 'N'.                    CL*30
00129      12  WS-UPDATE-SW        PIC X VALUE ' '.                        CL*88
00130      12  WS-GROUPING.                                                CL*30
00131          16  WS-GROUP-1      PIC X VALUE SPACES.                     CL*88
00132          16  FILLER          PIC X(05) VALUE SPACES.                 CL*30
00133      12  WS-BROWSE-TRLR-SW   PIC X VALUE ' '.                        CL*88
00134      12  WS-BROWSE-START-SW  PIC X VALUE ' '.                        CL*88
00135      12  WS-WORK-STATE-ACCT.                                         CL*16
00136          16  WS-WORK-ACCT    PIC X(10)   VALUE SPACES.               CL*81
00137          16  FILLER          PIC X       VALUE '/'.                  CL*88
00138          16  WS-WORK-STATE   PIC XX      VALUE SPACES.               CL*88
00139      12  WS-CHECK-WRITTEN-DT PIC XX      VALUE LOW-VALUES.        EL156
00140      12  WS-PMT-APPROVAL-SW  PIC X       VALUE SPACES.            EL156
00141      12  WS-SPECIAL-CALC-CD  PIC X       VALUE SPACES.            EL156
00142      12  QID.                                                     EL156
00143          16  QID-TERM        PIC X(4)    VALUE SPACES.               CL*81
00144          16  FILLER          PIC X(4)    VALUE '156A'.            EL156
00145      12  MAP-LENGTH          PIC S9(4)   VALUE +1098 COMP.           CL*82
00146      12  SUB-1               PIC S9(4)   VALUE +0     COMP.       EL156
00147      12  FIRST-ENTRY         PIC X       VALUE 'N'.               EL156
00148      12  FILE-SWITCH         PIC X(4)    VALUE SPACES.            EL156
00149      12  BENE-FOUND-SW       PIC X       VALUE SPACE.             EL156
00150      12  FOUND-TOL-SW        PIC X       VALUE 'N'.               EL156
00151      12  RETURNED-FROM-B     PIC X       VALUE 'N'.               EL156
00152      12  RETURNED-FROM       PIC X(8)    VALUE SPACES.            EL156
00153      12  WS-STATUS           PIC X       VALUE SPACES.               CL*81
00154      12  WS-TODAY-DATE       PIC XX      VALUE LOW-VALUES.        EL156
00155      12  WS-RETRO-ELIM-DATE  PIC XX      VALUE LOW-VALUES.        EL156
00156      12  WS-EXP-DT           PIC XX      VALUE LOW-VALUES.        EL156
           12  ws-extended-exp-dt  pic xx      value low-values.
00157      12  WS-SV-EPYFROM       PIC XX      VALUE LOW-VALUES.        EL156
00158      12  WS-SV-AIGFROM       PIC XX      VALUE LOW-VALUES.           CL*65
00159      12  WS-SV-EPYTHRU       PIC XX      VALUE LOW-VALUES.        EL156
00160      12  WS-SV-PREV-PD-THRU  PIC XX      VALUE LOW-VALUES.           CL*88
00161      12  WS-STATE-ABBREV     PIC XX      VALUE SPACES.               CL*88
00162      12  WS-BEN-DAYS         PIC 99      VALUE ZEROS.                CL*88
00163      12  WS-PMT-UNAPPROVED-CNT PIC S9(3)   VALUE +0      COMP-3.     CL*65
00164      12  WS-DAYS-TOL         PIC S9(3)     VALUE +0      COMP-3.  EL156
00165      12  WS-PMT-TOL          PIC S9(3)V99  VALUE +0      COMP-3.  EL156
00166      12  WS-MAX-DAYS-TOL     PIC S9(3)     VALUE +0      COMP-3.     CL*16
00167      12  WS-MAX-PMT-TOL      PIC S9(7)V99  VALUE +0      COMP-3.  EL156
00168      12  WS-MAX-LF-PMT-TOL   PIC S9(7)V99  VALUE +0      COMP-3.     CL*10
00169      12  WS-AH-BEN-AMT       PIC S9(7)V999 VALUE +0      COMP-3.  EL156
030514     12  WS-LF-BEN-AMT       PIC S9(9)V99  VALUE +0      COMP-3.
00170      12  WS-BEN-AMT-LEFT     PIC S9(7)V99  VALUE +0.                 CL*25
00171      12  WS-REMAINING-AMT    PIC S9(9)V99  VALUE +0.                 CL*25
00172      12  WS-TOL-SEVERITY     PIC X         VALUE ' '.             EL156
00173      12  WS-LF-COVERAGE-TYPE PIC X         VALUE ' '.                CL*21
00174      12  WS-EARNING-METHOD   PIC X         VALUE ' '.             EL156
00175      12  WS-CALC-METHOD      PIC X         VALUE ' '.             EL156
00176      12  WS-PAY-TYPE         PIC X         VALUE ' '.             EL156
00177      12  EXPENSE-MONTH-SAVE      PIC S9(4)     COMP-3 VALUE +0.   EL156
00178      12  WS-RESERVE-WORK         PIC S9(6)V99  COMP-3 VALUE +0.   EL156
012810     12  WS-EDIT-PATTERN         PIC Z(7).99  VALUE ZEROS.           CL*81
012810     12  WS-EDIT REDEFINES WS-EDIT-PATTERN PIC X(10).             EL156
00181      12  WORK-DATE-FROM.                                          EL156
00182          16  WDF-MONTH       PIC 99        VALUE ZERO.            EL156
00183          16  WDF-DAY         PIC 99        VALUE ZERO.            EL156
00184          16  WDF-YEAR        PIC 99        VALUE ZERO.            EL156
00185      12  WORK-DATE-THRU.                                          EL156
00186          16  WDT-MONTH       PIC 99        VALUE ZERO.            EL156
00187          16  WDT-DAY         PIC 99        VALUE ZERO.            EL156
00188          16  WDT-YEAR        PIC 99        VALUE ZERO.            EL156
00189      12  FROM-DAYS-IN-MONTH  PIC 99        VALUE ZERO.            EL156
00190      12  THRU-DAYS-IN-MONTH  PIC 99        VALUE ZERO.            EL156
00191      12  FROM-PMT-DAYS       PIC 99        VALUE ZERO.            EL156
00192      12  THRU-PMT-DAYS       PIC 99        VALUE ZERO.            EL156
00193      12  SAVE-ELAPSED-MONTHS PIC 999       VALUE ZERO.            EL156
00194      12  SAVE-ELAPSED-DAYS   PIC 9(04)     VALUE ZERO.               CL**9
00195      12  WS-SAVE-ODD-DAYS    PIC S9(04)    VALUE ZERO.               CL**9
00196      12  SAVE-ODD-DAYS-OVER  PIC 999       VALUE ZERO.               CL*88
00197      12  WS-DAILY-RATE       PIC S999V99999  VALUE +0.               CL*88
00198      12  WS-PY-WORK          PIC S9(8)V99 VALUE +0.                  CL*81
00199      12  WS-PY-WK REDEFINES WS-PY-WORK.                           EL156
00200          16  WS-PY-NUM       PIC S9(7)V99.                        EL156
00201          16  WS-PY-SIGN      PIC X.                               EL156
00202      12  WS-DY-WORK          PIC S9(5).                           EL156
00203      12  WS-DY-WK REDEFINES WS-DY-WORK.                           EL156
00204          16  WS-DY-NUM       PIC S9(4).                           EL156
00205          16  WS-DY-SIGN      PIC X.                               EL156
00206      12  WS-COMP-MSTR-SW     PIC X       VALUE 'N'.                  CL*88
00207          88  COMP-REC-FOUND              VALUE 'Y'.                  CL*25
00208      12  WS-BEN-SEARCH-SW    PIC X       VALUE 'N'.               EL156
00209          88  BENEFIT-FOUND       VALUE 'Y'.                       EL156
00210          88  NO-BENEFIT-FOUND    VALUE 'N'.                       EL156
00211      12  WS-TERMS.                                                EL156
041812         16  WST-ORIG        PIC ZZ9     VALUE ZEROS.                CL*82
00213          16  FILLER          PIC X       VALUE '/'.               EL156
041812         16  WST-REM         PIC ZZ9     VALUE ZEROS.                CL*82
00215          16  WST-REM-DAYS-GRP.                                       CL*82
00216              20  WST-SLASH   PIC X       VALUE '/'.                  CL*98
00217              20  WST-REM-DAYS                                        CL*82
00218                              PIC --9.                                CL*82
00219      12  WS-ACCESS.                                               EL156
00220          16  FILLER          PIC XX      VALUE SPACES.            EL156
00221          16  WS-BEN-CD       PIC XX      VALUE SPACES.               CL*81
00222      12  WS-STATE-ACCESS.                                         EL156
00223          16  WS-ST-ACCESS    PIC XX      VALUE SPACES.               CL*81
00224          16  FILLER          PIC XX      VALUE SPACES.            EL156
00225      12  WS-CARR-ACCESS.                                          EL156
00226          16  FILLER          PIC X(3)    VALUE SPACES.            EL156
00227          16  WS-CARR         PIC X       VALUE SPACES.               CL*81
00228      12  DATE-WORK.                                               EL156
00229          16  FILLER          PIC XX      VALUE SPACES.               CL*93
00230          16  NUM-WORK        PIC X(6)    VALUE SPACES.               CL*81
00231      12  WS-SAVE-INPUT.                                           EL156
00232          16  WS-PMTTYPE      PIC X       VALUE SPACE.             EL156
00233          16  WS-PAYEE        PIC XX      VALUE SPACE.                CL*88
00234          16  WS-PMTNOTE1     PIC X(60)   VALUE SPACES.               CL*16
00235          16  WS-PMTNOTE2     PIC X(60)   VALUE SPACES.               CL*16
00236          16  WS-OFFLINE      PIC X       VALUE SPACE.             EL156
00237          16  WS-CHECKNO      PIC X(7)    VALUE SPACES.            EL156
00238          16  WS-HOLDTIL      PIC 9(6)    VALUE 0.                 EL156
00239          16  WS-EPYFROM      PIC 9(6)    VALUE 0.                 EL156
00240          16  WS-EPYTHRU      PIC 9(6)    VALUE 0.                 EL156
00241          16  WS-EDAYS        PIC S9(5)    VALUE +0.               EL156
00242          16  WS-EPYAMT       PIC S9(7)V99 VALUE +0.               EL156
00243          16  WS-ERESV        PIC S9(7)V99 VALUE +0.                  CL*25
00244          16  WS-EEXPENS      PIC 9(6)V99  VALUE 0.                EL156
00245          16  WS-ETYPE        PIC X       VALUE SPACE.             EL156
00246          16  WS-CASH         PIC X       VALUE SPACE.                CL*16
00247          16  WS-GROUPED      PIC X       VALUE SPACE.                CL*16
00248          16  WS-INT-RATE     PIC S99V9(5) VALUE +0.                  CL*57
00249          16  WS-AIGFROM      PIC 9(6)    VALUE 0.                    CL*65
00250          16  WS-LOAN-NO      PIC X(20)   VALUE SPACES.               CL*78
052506         16  WS-PROOF-DATE   PIC 9(6)    VALUE 0.
               16  WS-PRINT-EOB-YN PIC X       VALUE ' '.
020413         16  WS-PRINT-CLM-FRM-YN PIC X   VALUE SPACE.
020413         16  WS-PRINT-SURVEY-YN PIC X    VALUE SPACE.
102413         16  WS-SPECIAL-RELEASE-YN PIC X VALUE SPACE.
120115         16  ws-int-to-rem-borr pic x value spaces.
00251      12  LAST-ERROR-LINE.                                         EL156
00252          16  WS-FATAL-CTR        PIC ZZ9  VALUE ZEROS.               CL*81
00253          16  FILLER              PIC X(30)                        EL156
00254              VALUE ' FATAL ERRORS ENCOUNTERED'.                   EL156
00255          16  WS-FORCE-CTR        PIC ZZ9   VALUE ZEROS.              CL*81
00256          16  FILLER              PIC X(28)                        EL156
00257              VALUE ' FORCIBLE ERRORS ENCOUNTERED'.                   CL*49
00258      12  WS-CHECK-AREA.                                           EL156
00259          16  WS-CHECK-CARR       PIC X   VALUE SPACES.               CL*81
00260          16  WS-CHECK-NUM        PIC 9(6) VALUE ZEROS.               CL*81
00261      12  WS-CHECK-NUMERIC REDEFINES WS-CHECK-AREA PIC 9(7).       EL156
00262      12  WS-CHECK-AREA-RDF REDEFINES WS-CHECK-AREA.                  CL*65
00263          16  FILLER              PIC XX.                             CL*93
00264          16  WS-A-CHECK-NUM-5    PIC 9(5).                           CL*65
00265      12  WS-CHECK-AREA-RDF1 REDEFINES WS-CHECK-AREA.                 CL*65
00266          16  FILLER              PIC X.                              CL*93
00267          16  WS-A-CHECK-NUM-6    PIC 9(6).                           CL*65
00268      12  OPTION-1-SAVE-AREA.                                      EL156
00269          16  SAVE1-FROM-DAYS-IN-MONTH    PIC 999  VALUE ZEROS.    EL156
00270          16  SAVE1-THRU-DAYS-IN-MONTH    PIC 999  VALUE ZEROS.    EL156
00271      12  WS-WORK-ZIP.                                             EL156
00272          16  WS-ZIP              PIC X(5)  VALUE ZEROS.           EL156
00273          16  WS-ZIP-EXT          PIC X(4)  VALUE ZEROS.           EL156
00274      12  WS-NUMERIC-ZIP REDEFINES WS-WORK-ZIP                     EL156
00275                                  PIC 9(9).                        EL156
00276      12  WS-FLI-PMTNOTE.                                          EL156
00277          16  WS-PMTOPTION        PIC XX.                             CL*93
00278          16  WS-DOCNAME          PIC X(28).                       EL156
00279          16  WS-AMOUNT-LINE REDEFINES WS-DOCNAME.                 EL156
00280              20  WS-AMOUNT       PIC X(8).                        EL156
00281              20  FILLER          PIC X(20).                       EL156
00282          16  WS-NUMERIC-LINE REDEFINES WS-DOCNAME.                EL156
00283              20  WS-NUM-AMT      PIC 9(6)V99.                     EL156
00284              20  FILLER          PIC X(20).                       EL156
00285                                                                   EL156
00286      12  WS-HAN-PMTNOTE.                                             CL*69
00287          16  WS-HAN-PMT-CODE     PIC X.                              CL*69
00288          16  WS-HAN-PMT-NOTE     PIC X(39).                          CL*69
00289                                                                      CL*69
00290      12  WS-NCL-PMT-OPTION       PIC XX.                             CL*93
00291          88  WS-VALID-NCL-OPTION           VALUE '00' THRU '19'.     CL*47
00292                                                                      CL*45
00293      12  WS-CLAIM-SEQUENCE.                                          CL*16
00294          16  FILLER              PIC X VALUE '('.                    CL*88
00295          16  WS-CUR-SEQU         PIC Z9    VALUE ZEROS.              CL*16
00296          16  FILLER              PIC X(04) VALUE ' OF '.             CL*16
00297          16  WS-OF-SEQU          PIC Z9    VALUE ZEROS.              CL*16
00298          16  FILLER              PIC X VALUE ')'.                    CL*88
00299                                                                      CL*47
00300      12  WS-REFUND-AMT           PIC Z(7).99.                        CL*47
00301      12  WS-CALC-INT             PIC S9(7)V99.                       CL*57
00302      12  ws-benefits-paid     PIC S9(4)V999 VALUE +0  COMP-3.     CL*65
00303                                                                      CL*65
00304      12  SUB                     PIC 99 VALUE ZEROS.                 CL*88
00305                                                                      CL*16
00306      12  WS-BOANOTE1.                                                CL*58
00307          16  WS-BOANOTE1-DESC    PIC X(11).                          CL*65
00308          16  WS-BOANOTE1-EPYAMT  PIC Z,ZZZ,ZZZ.99.                   CL*65
00309          16  FILLER              PIC X(17).                          CL*65
00310      12  WS-BOANOTE2.                                                CL*58
00311          16  WS-BOANOTE2-DESC    PIC X(11).                          CL*65
00312          16  WS-BOANOTE2-INT     PIC Z,ZZZ,ZZZ.99.                   CL*65
00313          16  FILLER              PIC X(17).                          CL*79
00314                                                                      CL*79
00315      12  WS-NCLNOTE1.                                                CL*79
00316          16  WS-NCLNOTE1-DESC    PIC X(11).                          CL*79
00317          16  WS-NCLNOTE1-CPYAMT  PIC Z,ZZZ,ZZZ.99.                   CL*79
00318          16  FILLER              PIC X(17).                          CL*79
00319      12  WS-NCLNOTE2.                                                CL*79
00320          16  WS-NCLNOTE2-DESC    PIC X(11).                          CL*79
00321          16  WS-NCLNOTE2-INT     PIC Z,ZZZ,ZZZ.99.                   CL*79
00322          16  FILLER              PIC X(17).                          CL*65
00323                                                                      CL*58
052506     12  WS-MAX-LETTER-ANSWER-DT PIC XX         VALUE LOW-VALUES.
031808     12  WS-SAVE-APPROVAL-LEVEL-REQD  PIC X.
031808     12  WS-SAVE-APPROVED-LEVEL  PIC X.
00324      12  WS-MAX-EXP-PMT          PIC S9(07)V99  VALUE +0.            CL*65
00325      12  WS-ACTIVITY-CODE        PIC 99         VALUE ZEROS.         CL*88
00326      12  WS-LETTER-SW            PIC X          VALUE 'N'.           CL*88
00327      12  WS-ACT-REC-FOUND-SW     PIC X          VALUE 'N'.           CL*88
00328      12  WS-BROWSE-SW            PIC X          VALUE 'N'.           CL*88
00329      12  WS-APPROVAL-LEVEL       PIC X          VALUE ' '.           CL*88
00330      12  WS-OPEN-CLOSE-SW        PIC X          VALUE ' '.           CL*88
00331                                                                      CL*65
00332      12  WS-AIG-WORK-AREAS.                                          CL*65
00333          16  WS-RECALC-PAYFROM-SW  PIC X VALUE 'N'.                  CL*65
00334              88  WS-AIG-RECALC-PAYFROM   VALUE 'Y'.                  CL*65
00335          16  WS-AIG-PAYFROM-SW PIC X     VALUE 'N'.                  CL*65
00336              88  WS-USE-AIG-PAYFROM      VALUE 'Y'.                  CL*65
00337          16  WS-FROM-DATE-MDY.                                       CL*65
00338              20  WS-FROM-MO      PIC 99.                             CL*88
00339              20  WS-FROM-DA      PIC 99.                             CL*88
00340              20  WS-FROM-YR      PIC 99.                             CL*88
00341          16  WS-TEMP-DATE-MDY.                                       CL*65
00342              20  WS-TEMP-MO      PIC 99.                             CL*88
00343              20  WS-TEMP-DA      PIC 99.                             CL*88
00344              20  WS-TEMP-YR      PIC 99.                             CL*88
00345          16  WS-BIN-FROM-DT      PIC XX.                             CL*65
00346          16  WS-TEMP-INC-MDY.                                        CL*65
00347              20  WS-TEMP-INC-MO  PIC 99.                             CL*88
00348              20  WS-TEMP-INC-DA  PIC 99.                             CL*88
00349              20  WS-TEMP-INC-YR  PIC 99.                             CL*88
00350          16  WS-PMT-DUE-MDY.                                         CL*65
00351              20  WS-PMT-DUE-MO   PIC 99.                             CL*88
00352              20  WS-PMT-DUE-DA   PIC 99.                             CL*88
00353              20  WS-PMT-DUE-YR   PIC 99.                             CL*88
00354          16  WS-PMT-DUE-DT       PIC XX.                             CL*65
00355          16  WS-NEXT-PMT-DUE-DT  PIC XX.                             CL*65
00356          16  WS-RETRO-ELIM-DT    PIC XX.                             CL*65
00357          16  WS-DUE-DAY-SW               PIC X       VALUE 'N'.      CL*65
00358              88 WS-RESTORE-DUE-DAY                   VALUE 'Y'.      CL*65
00359          16  WS-SAVE-DUE-DA      PIC 99.                             CL*65
00360          16  WS-CDAYS            PIC S9(05).                         CL*65
00361          16  WS-DAYS-OFF         PIC S9(05).                         CL*65
00362          16  WS-ELIMINATION-SW   PIC X     VALUE 'N'.                CL*65
00363              88  ELIM-SATISFIED            VALUE 'Y'.                CL*65
00364          16  WS-AIG-CHECKNO.                                         CL*65
00365              20  WS-A-CHECKNO-1-2        PIC XX      VALUE SPACES.   CL*93
00366              20  WS-A-CHECKNO-3-7.                                   CL*65
00367                  24  FILLER              PIC X(3)    VALUE SPACES.   CL*65
00368                  24  WS-A-CHECKNO-6-7    PIC XX      VALUE SPACES.   CL*93
00369                      88  WS-AIG-CREDIT-ENTERED       VALUE 'CR'.     CL*65
00370          16  WS-AIG-GROUPING.                                        CL*65
00371              20  WS-A-BENE               PIC XXX.                    CL*88
00372              20  FILLER                  PIC XXX.                    CL*88
00373          16  WS-PAYMENT-TRAILER-SW       PIC X       VALUE ' '.      CL*65
00374              88  WS-1ST-SPECIAL-PMT                  VALUE '1'.      CL*65
00375              88  WS-2ND-SPECIAL-PMT                  VALUE '2'.      CL*65
00376          16  WS-AIG-UNEMPLY-DESC         PIC X(60)                   CL*65
00377                  VALUE 'UNEMPLOYMENT PAYMENT'.                       CL*65
00378          16  WS-CHK-THRU-DT              PIC XX.                     CL*65
00379          16  WS-HOLD-BIN-DATE-1          PIC XX.                     CL*65
00380                                                                      CL*64
031808 01  MISC-CSO-AREA.
           12  ELCRTT-LENGTH       PIC S9(04)    COMP     VALUE +552.
041309     12  ELCRTT-DSID         PIC X(8)    VALUE 'ELCRTT'.
031808     12  WS-NEED-APPROVAL                PIC X(1)   VALUE 'N'.
031808         88  APPROVAL-NEEDED                        VALUE 'Y'.
031808     12  WS-APPROV-NOTE                  PIC X(43)
031808         VALUE 'REFERRED FOR CLAIM PAYMENT APPROVAL REVIEW.'.
052814     12  WS-3550-APPROV-NOTE             PIC X(43)
052814         VALUE 'APPROVL NEEDED-DOES CLAIMANT HAVE COVERAGE?'.
043019     12  WS-DUPE-APPROV-NOTE             PIC X(43)
043019         VALUE 'APPROVAL NEEDED - DUPLICATE PAYMENT EXISTS '.
031808     12  WS-REAUDIT-NOTE                 PIC X(43)
031808         VALUE 'REFERRED FOR RANDOM AUDIT APPROVAL REVIEW. '.
031808     12  WS-CALC-PMT-MONTHS      PIC S9(2)V9(8)  VALUE +0.
031808     12  WS-CALC-MO-AH-AMT       PIC S9(07)V99   VALUE +0.
091808     12  WS-SUB                  PIC S9(3) COMP-3 VALUE +0.
091808     12  WS-BEG-SUB              PIC S9(3) COMP-3 VALUE +0.
091808     12  WS-END-SUB              PIC S9(3) COMP-3 VALUE +0.
091808     12  WS-STATE-LENGTH         PIC S9(3) COMP-3 VALUE +0.
091808     12  WS-PAYEE-STATE-FOUND    PIC X(01)        VALUE 'N'.
091808         88 PAYEE-STATE-FOUND                     VALUE 'Y'.
091808     12  WS-PAYEE-CITY-STATE     PIC X(30).
091808     12  FILLER  REDEFINES WS-PAYEE-CITY-STATE.
091808         16  WS-PAYEE-CITY-ST OCCURS 30 TIMES PIC X(1).
112210     12  WS-PD-DAYS              PIC S9(5) VALUE +0.
020513     12  WS-SUB2                 PIC S9(3) COMP-3 VALUE +0.
020513     12  WS-WORK-NAME            PIC X(30).
020513     12  FILLER  REDEFINES WS-WORK-NAME.
020513         16  WS-WORK-NAME-X OCCURS 30 TIMES PIC X(1).
020513     12  WS-INSURED-NAME         PIC X(30).
020513     12  FILLER  REDEFINES WS-INSURED-NAME.
020513         16  WS-INSURED-NAME-X OCCURS 30 TIMES PIC X(1).
020513     12  WS-PAYEE-NAME           PIC X(30).
020513     12  FILLER  REDEFINES WS-PAYEE-NAME.
020513         16  WS-PAYEE-NAME-X OCCURS 30 TIMES PIC X(1).
022718     12  WS-HOLD-UNTIL-SW        PIC X VALUE SPACES.
022718         88  HOLD-UNTIL-CHK            VALUE 'Y'.
031218         88  HOLD-UNTIL-PMT            VALUE 'P'.
022718     12  WS-HOLD-UNTIL-DT        PIC X(02) VALUE ZERO.
040819     12  WS-UNPAID-HOLD-UNTIL-SW PIC X VALUE SPACES.
040819         88  UNPAID-HOLD-UNTIL-EXISTS  VALUE 'Y'.
040819     12  WS-DUPE-VOID-PMT-SW     PIC X VALUE SPACES.
040819         88  DUPE-VOID-PMT-EXISTS      VALUE 'Y'.
040819     12  WS-CHECK-ALL-PMTS-SW    PIC X VALUE SPACES.
040819         88  CHECK-ALL-PMTS            VALUE 'Y'.
040819     12  WS-TOT-AMOUNT-PAID      PIC S9(7)V99  COMP-3 VALUE ZERO.
040819     12  WS-UNPD-HOLD-UNTIL-DT   PIC X(02) VALUE ZERO.

031808
00381  01  ERROR-MESSAGES.                                              EL156
00382      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL156
00383      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL156
00384      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL156
00385      12  ER-0019                 PIC X(4)  VALUE '0019'.          EL156
00386      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL156
00387      12  ER-0042                 PIC X(4)  VALUE '0042'.          EL156
00388      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL156
00389      12  ER-0130                 PIC X(4)  VALUE '0130'.             CL*16
00390      12  ER-0149                 PIC X(4)  VALUE '0149'.          EL156
00391      12  ER-0154                 PIC X(4)  VALUE '0154'.          EL156
00392      12  ER-0168                 PIC X(4)  VALUE '0168'.          EL156
00393      12  ER-0169                 PIC X(4)  VALUE '0169'.          EL156
00394      12  ER-0172                 PIC X(4)  VALUE '0174'.          EL156
00395      12  ER-0198                 PIC X(4)  VALUE '0198'.          EL156
00396      12  ER-0204                 PIC X(4)  VALUE '0204'.          EL156
00397      12  ER-0206                 PIC X(4)  VALUE '0206'.          EL156
00398      12  ER-0252                 PIC X(4)  VALUE '0252'.          EL156
00399      12  ER-0254                 PIC X(4)  VALUE '0254'.          EL156
00400      12  ER-0282                 PIC X(4)  VALUE '0282'.          EL156
00401      12  ER-0283                 PIC X(4)  VALUE '0283'.          EL156
00402      12  ER-0294                 PIC X(4)  VALUE '0294'.          EL156
00403      12  ER-0338                 PIC X(4)  VALUE '0338'.
           12  ER-0419                 PIC X(4)  VALUE '0419'.
00404      12  ER-0420                 PIC X(4)  VALUE '0420'.          EL156
00405      12  ER-0421                 PIC X(4)  VALUE '0421'.          EL156
00406      12  ER-0422                 PIC X(4)  VALUE '0422'.          EL156
00407      12  ER-0433                 PIC X(4)  VALUE '0433'.          EL156
00408      12  ER-0436                 PIC X(4)  VALUE '0436'.          EL156
00409      12  ER-0437                 PIC X(4)  VALUE '0437'.          EL156
00410      12  ER-0438                 PIC X(4)  VALUE '0438'.          EL156
00411      12  ER-0439                 PIC X(4)  VALUE '0439'.          EL156
00412      12  ER-0440                 PIC X(4)  VALUE '0440'.          EL156
00413      12  ER-0441                 PIC X(4)  VALUE '0441'.          EL156
00414      12  ER-0442                 PIC X(4)  VALUE '0442'.          EL156
00415      12  ER-0443                 PIC X(4)  VALUE '0443'.          EL156
00416      12  ER-0444                 PIC X(4)  VALUE '0444'.          EL156
00417      12  ER-0445                 PIC X(4)  VALUE '0445'.          EL156
00418      12  ER-0446                 PIC X(4)  VALUE '0446'.          EL156
00419      12  ER-0447                 PIC X(4)  VALUE '0447'.          EL156
00420      12  ER-0448                 PIC X(4)  VALUE '0448'.          EL156
00421      12  ER-0449                 PIC X(4)  VALUE '0449'.          EL156
00422      12  ER-0452                 PIC X(4)  VALUE '0452'.          EL156
00423      12  ER-0458                 PIC X(4)  VALUE '0458'.          EL156
00424      12  ER-0459                 PIC X(4)  VALUE '0459'.          EL156
00425      12  ER-0460                 PIC X(4)  VALUE '0460'.          EL156
00426      12  ER-0461                 PIC X(4)  VALUE '0461'.          EL156
00427      12  ER-0462                 PIC X(4)  VALUE '0462'.          EL156
00428      12  ER-0463                 PIC X(4)  VALUE '0463'.          EL156
00429      12  ER-0464                 PIC X(4)  VALUE '0464'.          EL156
00430      12  ER-0465                 PIC X(4)  VALUE '0465'.          EL156
00431      12  ER-0466                 PIC X(4)  VALUE '0466'.          EL156
00432      12  ER-0467                 PIC X(4)  VALUE '0467'.          EL156
00433      12  ER-0468                 PIC X(4)  VALUE '0468'.          EL156
00434      12  ER-0469                 PIC X(4)  VALUE '0469'.          EL156
00435      12  ER-0473                 PIC X(4)  VALUE '0473'.          EL156
00436      12  ER-0474                 PIC X(4)  VALUE '0474'.          EL156
00437      12  ER-0475                 PIC X(4)  VALUE '0475'.          EL156
00438      12  ER-0476                 PIC X(4)  VALUE '0476'.          EL156
00439      12  ER-0477                 PIC X(4)  VALUE '0477'.          EL156
00440      12  ER-0478                 PIC X(4)  VALUE '0478'.          EL156
00441      12  ER-0479                 PIC X(4)  VALUE '0479'.          EL156
00442      12  ER-0481                 PIC X(4)  VALUE '0481'.          EL156
00443      12  ER-0491                 PIC X(4)  VALUE '0491'.          EL156
00444      12  ER-0492                 PIC X(4)  VALUE '0492'.          EL156
00445      12  ER-0493                 PIC X(4)  VALUE '0493'.          EL156
00446      12  ER-0494                 PIC X(4)  VALUE '0494'.          EL156
00447      12  ER-0495                 PIC X(4)  VALUE '0495'.          EL156
00448      12  ER-0499                 PIC X(4)  VALUE '0499'.          EL156
00449      12  ER-0500                 PIC X(4)  VALUE '0500'.          EL156
00450      12  ER-0501                 PIC X(4)  VALUE '0501'.          EL156
00451      12  ER-0502                 PIC X(4)  VALUE '0502'.          EL156
00452      12  ER-0508                 PIC X(4)  VALUE '0508'.          EL156
00453      12  ER-0513                 PIC X(4)  VALUE '0513'.          EL156
00454      12  ER-0518                 PIC X(4)  VALUE '0518'.          EL156
00455      12  ER-0524                 PIC X(4)  VALUE '0524'.          EL156
00456      12  ER-0525                 PIC X(4)  VALUE '0525'.          EL156
00457      12  ER-0526                 PIC X(4)  VALUE '0526'.          EL156
00458      12  ER-0530                 PIC X(4)  VALUE '0530'.          EL156
00459      12  ER-0540                 PIC X(4)  VALUE '0540'.          EL156
00460      12  ER-0541                 PIC X(4)  VALUE '0541'.          EL156
00461      12  ER-0542                 PIC X(4)  VALUE '0542'.          EL156
00462      12  ER-0543                 PIC X(4)  VALUE '0543'.          EL156
00463      12  ER-0545                 PIC X(4)  VALUE '0545'.          EL156
00464      12  ER-0552                 PIC X(4)  VALUE '0552'.          EL156
00465      12  ER-0555                 PIC X(4)  VALUE '0555'.          EL156
00466      12  ER-0556                 PIC X(4)  VALUE '0556'.          EL156
00467      12  ER-0557                 PIC X(4)  VALUE '0557'.          EL156
00468      12  ER-0572                 PIC X(4)  VALUE '0572'.          EL156
00469      12  ER-0573                 PIC X(4)  VALUE '0573'.          EL156
00470      12  ER-0594                 PIC X(4)  VALUE '0594'.          EL156
00471      12  ER-0616                 PIC X(4)  VALUE '0616'.          EL156
00472      12  ER-0617                 PIC X(4)  VALUE '0617'.          EL156
00473      12  ER-0618                 PIC X(4)  VALUE '0618'.          EL156
00474      12  ER-0657                 PIC X(4)  VALUE '0657'.          EL156
00475      12  ER-0658                 PIC X(4)  VALUE '0658'.             CL**4
00476      12  ER-0699                 PIC X(4)  VALUE '0699'.             CL*45
00477      12  ER-0730                 PIC X(4)  VALUE '0730'.             CL*47
00478      12  ER-0737                 PIC X(4)  VALUE '0737'.             CL*48
00479      12  ER-0760                 PIC X(4)  VALUE '0760'.             CL*50
00480      12  ER-0768                 PIC X(4)  VALUE '0768'.             CL*64
00481      12  ER-0769                 PIC X(4)  VALUE '0769'.             CL*80
00482      12  ER-0770                 PIC X(4)  VALUE '0770'.             CL*65
00483      12  ER-0802                 PIC X(4)  VALUE '0802'.             CL*65
00484      12  ER-0803                 PIC X(4)  VALUE '0803'.             CL*65
00485      12  ER-0819                 PIC X(4)  VALUE '0819'.             CL*65
00486      12  ER-0834                 PIC X(4)  VALUE '0834'.             CL*65
00487      12  ER-0836                 PIC X(4)  VALUE '0836'.             CL*69
00488      12  ER-0844                 PIC X(4)  VALUE '0844'.             CL*80
00489      12  ER-0845                 PIC X(4)  VALUE '0845'.             CL*80
00490      12  ER-0846                 PIC X(4)  VALUE '0846'.             CL*80
00491      12  ER-0849                 PIC X(4)  VALUE '0849'.             CL*83
00492      12  ER-0851                 PIC X(4)  VALUE '0851'.             CL*85
052506     12  ER-0872                 PIC X(4)  VALUE '0872'.
052506     12  ER-0873                 PIC X(4)  VALUE '0873'.
022106     12  ER-0874                 PIC X(4)  VALUE '0874'.
041710     12  ER-0879                 PIC X(4)  VALUE '0879'.
00493      12  ER-0919                 PIC X(4)  VALUE '0919'.             CL*82
00494      12  ER-0921                 PIC X(4)  VALUE '0921'.             CL*82
00495      12  ER-0923                 PIC X(4)  VALUE '0923'.             CL*82
00496      12  ER-0925                 PIC X(4)  VALUE '0925'.             CL*82
00497      12  ER-0944                 PIC X(4)  VALUE '0944'.             CL*82
00498      12  ER-0945                 PIC X(4)  VALUE '0945'.             CL*82
00499      12  ER-0946                 PIC X(4)  VALUE '0946'.             CL*82
00500      12  ER-0947                 PIC X(4)  VALUE '0947'.             CL*82
00501      12  ER-0948                 PIC X(4)  VALUE '0948'.             CL*82
00502      12  ER-0949                 PIC X(4)  VALUE '0949'.             CL*82
00503      12  ER-0950                 PIC X(4)  VALUE '0950'.             CL*82
00504      12  ER-0951                 PIC X(4)  VALUE '0951'.             CL*82
00505      12  ER-0954                 PIC X(4)  VALUE '0954'.             CL*82
00506      12  ER-0956                 PIC X(4)  VALUE '0956'.             CL*82
00507      12  ER-0967                 PIC X(4)  VALUE '0967'.             CL*82
00508      12  ER-0968                 PIC X(4)  VALUE '0968'.             CL*82
00509      12  ER-0974                 PIC X(4)  VALUE '0974'.             CL*86
00510      12  ER-0975                 PIC X(4)  VALUE '0975'.             CL*86
           12  ER-1561                 PIC X(4)  VALUE '1561'.
020413     12  ER-1566                 PIC X(4)  VALUE '1566'.
020413     12  ER-1567                 PIC X(4)  VALUE '1567'.
102413     12  ER-1569                 PIC X(4)  VALUE '1569'.
120115     12  er-1582                 pic x(4)  value '1582'.
           12  er-1657                 pic x(4)  value '1657'.
           12  er-1658                 pic x(4)  value '1658'.
           12  er-1667                 pic x(4)  value '1667'.
           12  er-1670                 pic x(4)  value '1670'.
052113     12  er-1671                 pic x(4)  value '1671'.
052113     12  er-1672                 pic x(4)  value '1672'.
052113     12  er-1673                 pic x(4)  value '1673'.
052113     12  er-1674                 pic x(4)  value '1674'.
           12  er-1677                 pic x(4)  value '1677'.
090821     12  er-1681                 pic x(4)  value '1681'.
00511      12  ER-1775                 PIC X(4)  VALUE '1775'.
00512      12  ER-1880                 PIC X(4)  VALUE '1880'.             CL*16
100518     12  ER-1930                 PIC X(4)  VALUE '1930'.
022106     12  ER-2044                 PIC X(4)  VALUE '2044'.
00513      12  ER-3140                 PIC X(4)  VALUE '3140'.             CL*16
00514      12  ER-3141                 PIC X(4)  VALUE '3141'.             CL*16
00515      12  ER-3255                 PIC X(4)  VALUE '3255'.             CL*51
082218     12  ER-3272                 PIC X(4)  VALUE '3272'.
040819     12  ER-3276                 PIC X(4)  VALUE '3276'.
040819     12  ER-3277                 PIC X(4)  VALUE '3277'.
040819     12  ER-3278                 PIC X(4)  VALUE '3278'.
040819     12  ER-3279                 PIC X(4)  VALUE '3279'.
043019     12  ER-3280                 PIC X(4)  VALUE '3280'.
00516      12  ER-3516                 PIC X(4)  VALUE '3516'.             CL*65
00517      12  ER-3527                 PIC X(4)  VALUE '3527'.             CL*65
00518      12  ER-3528                 PIC X(4)  VALUE '3528'.             CL*65
00519      12  ER-3529                 PIC X(4)  VALUE '3529'.             CL*64
00520      12  ER-3530                 PIC X(4)  VALUE '3530'.             CL*65
00521      12  ER-3531                 PIC X(4)  VALUE '3531'.             CL*58
00522      12  ER-3532                 PIC X(4)  VALUE '3532'.             CL*65
00523      12  ER-3533                 PIC X(4)  VALUE '3533'.             CL*65
00524      12  ER-3534                 PIC X(4)  VALUE '3534'.             CL*65
00525      12  ER-3535                 PIC X(4)  VALUE '3535'.             CL*65
00526      12  ER-3536                 PIC X(4)  VALUE '3536'.             CL*65
00527      12  ER-3537                 PIC X(4)  VALUE '3537'.             CL*65
00528      12  ER-3538                 PIC X(4)  VALUE '3538'.             CL*65
00529      12  ER-3539                 PIC X(4)  VALUE '3539'.             CL*65
00530      12  ER-3540                 PIC X(4)  VALUE '3540'.             CL*65
00531      12  ER-3541                 PIC X(4)  VALUE '3541'.             CL*65
00532      12  ER-3542                 PIC X(4)  VALUE '3542'.             CL*65
00533      12  ER-3543                 PIC X(4)  VALUE '3543'.             CL*65
00534      12  ER-3546                 PIC X(4)  VALUE '3546'.
112210     12  ER-3549                 PIC X(4)  VALUE '3549'.
112018     12  ER-3551                 PIC X(4)  VALUE '3551'.
           12  ER-3818                 PIC X(4)  VALUE '3818'.
00535      12  ER-7540                 PIC X(4)  VALUE '7540'.             CL*80
061511     12  ER-7576                 PIC X(4)  VALUE '7576'.
020513     12  ER-7579                 PIC X(4)  VALUE '7579'.
032414     12  ER-7580                 PIC X(4)  VALUE '7580'.
032514     12  ER-7583                 PIC X(4)  VALUE '7583'.
100518     12  ER-7585                 PIC X(4)  VALUE '7585'.
00536      12  ER-7830                 PIC X(4)  VALUE '7830'.             CL*88
00537      12  ER-7831                 PIC X(4)  VALUE '7831'.             CL*88
00538      12  ER-7832                 PIC X(4)  VALUE '7832'.             CL*88
00539      12  ER-7833                 PIC X(4)  VALUE '7833'.             CL*88
00540      12  ER-7834                 PIC X(4)  VALUE '7834'.             CL*88
00541      12  ER-7835                 PIC X(4)  VALUE '7835'.             CL*88
00542      12  ER-7836                 PIC X(4)  VALUE '7836'.             CL*88
00543      12  ER-7837                 PIC X(4)  VALUE '7837'.             CL*88
00544      12  ER-7838                 PIC X(4)  VALUE '7838'.             CL*88
00545      12  ER-7841                 PIC X(4)  VALUE '7841'.             CL*94
00546      12  ER-7844                 PIC X(4)  VALUE '7844'.             CL*95
00547      12  ER-7845                 PIC X(4)  VALUE '7845'.             CL*95
00548      12  ER-8000                 PIC X(4)  VALUE '8000'.             CL*97
00549      12  ER-8051                 PIC X(4)  VALUE '8051'.             CL*83
00550      12  ER-8052                 PIC X(4)  VALUE '8052'.             CL*83
00551      12  ER-8053                 PIC X(4)  VALUE '8053'.             CL*83
00552      12  ER-8054                 PIC X(4)  VALUE '8054'.             CL*83
00553      12  ER-8055                 PIC X(4)  VALUE '8055'.             CL*83
00554      12  ER-8056                 PIC X(4)  VALUE '8056'.             CL*83
00555      12  ER-8057                 PIC X(4)  VALUE '8057'.             CL*83
00556      12  ER-8058                 PIC X(4)  VALUE '8058'.             CL*83
00557      12  ER-8059                 PIC X(4)  VALUE '8059'.             CL*83
00558      12  ER-8060                 PIC X(4)  VALUE '8060'.             CL*83
00559      12  ER-8061                 PIC X(4)  VALUE '8061'.             CL*83
00560      12  ER-8062                 PIC X(4)  VALUE '8062'.             CL*83
00561      12  ER-8063                 PIC X(4)  VALUE '8063'.             CL*83
00562      12  ER-8064                 PIC X(4)  VALUE '8064'.             CL*83
00563      12  ER-8065                 PIC X(4)  VALUE '8065'.             CL*83
00564      12  ER-8066                 PIC X(4)  VALUE '8066'.             CL*83
00565      12  ER-8146                 PIC X(4)  VALUE '8146'.             CL*84
00566      12  ER-8147                 PIC X(4)  VALUE '8147'.             CL*84
00567      12  ER-8152                 PIC X(4)  VALUE '8152'.             CL*85
00568      12  ER-8153                 PIC X(4)  VALUE '8153'.             CL*85
00569      12  ER-8154                 PIC X(4)  VALUE '8154'.             CL*87
00570      12  ER-8155                 PIC X(4)  VALUE '8155'.             CL*87
00571      12  ER-8310                 PIC X(4)  VALUE '8310'.             CL*83
00572      12  ER-8311                 PIC X(4)  VALUE '8311'.             CL*83
00573      12  ER-8312                 PIC X(4)  VALUE '8312'.             CL*83
00574      12  ER-8313                 PIC X(4)  VALUE '8313'.             CL*83
00575      12  ER-8314                 PIC X(4)  VALUE '8314'.             CL*83
00576      12  ER-8315                 PIC X(4)  VALUE '8315'.             CL*83
00577      12  ER-8316                 PIC X(4)  VALUE '8316'.             CL*83
00578      12  ER-8317                 PIC X(4)  VALUE '8317'.             CL*83
CIDMOD                                                                       000
CIDMOD 01  CSO-WORK-FIELDS.                                                  000
CIDMOD     05  ERROR-ON-OUTPUT-SW  PIC X     VALUE 'N'.                      000
CIDMOD       88  ERROR-ON-OUTPUT             VALUE 'Y'.                      000
041710
041710     05  WRK-6MO                 PIC S9(7)V99 VALUE +0.
041710     05  WRK-CERT-NOTE-ADD       PIC X VALUE SPACES.
041710         88  CERT-NOTE-ADDED           VALUE 'Y'.
041710     05  WRK-ORIG-NOTE-ADD       PIC X VALUE SPACES.
041710         88  ORIG-NOTE-ADDED           VALUE 'Y'.
041710     05  WRK-NOTE-SEQ            PIC S9(4) COMP  VALUE +0.
032813
032813     05  WRK-NEXT-AUDIT-CHK-NO   PIC S9(8) COMP  VALUE +0.
041710
041710 01  SC-NP6-CERT-NOTE.
041710     12  FILLER                  PIC X(12)
041710               VALUE 'REM BENEFIT '.
041710     12  SC-NP6-REM-BEN          PIC ZZZ,ZZZ.99.
041710     12  SC-NP6-COMM             PIC X(8)
041710               VALUE ' + 6 MO '.
041710     12  SC-NP6-6-MO             PIC ZZZ,ZZZ.99.
041710     12  FILLER                  PIC X(12)
041710               VALUE ' = AMT PAID '.
041710     12  SC-NP6-AMT-PAID         PIC ZZZ,ZZZ.99.
041710
041710 01  SC-NP6-ORIG-NOTE.
041710     12  FILLER                  PIC X(63)  VALUE
041710         'BENEFITS PAID CANNOT EXCEED ORIGINAL LIFE BENEFIT'.

       01  WS-PDEF-RECORD-SW           PIC X  VALUE ' '.
           88  PDEF-FOUND                   VALUE 'Y'.

090821 01  filler.
090821     05  ws-eracct-startbr-ind   pic x  value spaces.
090821         88  eracct-browse-started  value 'Y'.
090821     05  ws-lo-acct-dt           pic xx value low-values.
090821     05  ws-hi-acct-dt           pic xx value low-values.
090821     05  ws-acct-status          pic x value spaces.
090821         88  acct-cancelled          value '3'.
090821     05  WS-I-SAY-STOP-IND       PIC X  VALUE ' '.
090821         88  i-say-STOP            value 'S'.

       01  WS-ACCT-RECORD-SW           PIC X  VALUE ' '.
           88  ACCT-FOUND                   VALUE 'Y'.

       01  CTBL-KEY-SAVE               PIC X(5).
       01  CTBL-KEY.
           05  CTBL-COMPANY-CD         PIC X.
           05  CTBL-TABLE              PIC XXX.
           05  CTBL-BEN-TYPE           PIC X.
           05  CTBL-BEN-CODE           PIC XX.

       01  ERPDEF-KEY-SAVE             PIC X(18).
       01  ERPDEF-KEY.
           12  ERPDEF-COMPANY-CD       PIC X.
           12  ERPDEF-STATE            PIC XX.
           12  ERPDEF-PROD-CD          PIC XXX.
           12  F                       PIC X(7).
           12  ERPDEF-BEN-TYPE         PIC X.
           12  ERPDEF-BEN-CODE         PIC XX.
           12  ERPDEF-EXP-DT           PIC XX.

       01  ELCRTT-KEY.                                              
           05  CTRLR-COMP-CD       PIC X.                               
           05  CTRLR-CARRIER       PIC X.                               
           05  CTRLR-GROUPING      PIC X(6).                            
           05  CTRLR-STATE         PIC X(2).                            
           05  CTRLR-ACCOUNT       PIC X(10).
           05  CTRLR-EFF-DT        PIC XX.                              
           05  CTRLR-CERT-NO       PIC X(11).  
           05  CTRLR-REC-TYPE      PIC X.

00580  01  ACCESS-KEYS.                                                 EL156
00581      12  ELARCH-KEY.                                                 CL*20
00582          16  ARCH-CO             PIC X.                              CL*88
00583          16  ARCH-NUMBER         PIC S9(08) COMP.                    CL*36
00584          16  ARCH-REC-TYPE       PIC X.                              CL*88
00585          16  ARCH-SEQ            PIC S9(04) COMP.                    CL*36
00586      12  ELMSTR-KEY.                                              EL156
00587          16  MSTR-COMP-CD        PIC X.                              CL*36
00588          16  MSTR-CARRIER        PIC X.                              CL*36
00589          16  MSTR-CLAIM-NO       PIC X(7).                           CL*36
00590          16  MSTR-CERT-NO        PIC X(11).                          CL*36
00591      12  ELCNTL-KEY.                                              EL156
00592          16  CNTL-COMP-ID        PIC X(3).                           CL*36
00593          16  CNTL-REC-TYPE       PIC X.                              CL*36
00594          16  CNTL-ACCESS         PIC X(4).                           CL*36
00595          16  CNTL-SEQ-NO         PIC S9(4)    COMP.                  CL*36
00596      12  ELCERT-KEY.                                              EL156
00597          16  CERT-COMP-CD        PIC X.                              CL*36
00598          16  CERT-CARRIER        PIC X.                              CL*36
00599          16  CERT-GROUPING       PIC X(6).                           CL*36
00600          16  CERT-STATE          PIC XX.                             CL*93
00601          16  CERT-ACCOUNT        PIC X(10).                          CL*36
00602          16  CERT-EFF-DT         PIC XX.                             CL*36
00603          16  CERT-CERT-NO        PIC X(11).                          CL*36
00604      12  ELTRLR-KEY.                                              EL156
00605          16  TRLR-COMP-CD        PIC X.                              CL*36
00606          16  TRLR-CARRIER        PIC X.                              CL*36
00607          16  TRLR-CLAIM-NO       PIC X(7).                           CL*36
00608          16  TRLR-CERT-NO        PIC X(11).                          CL*36
00609          16  TRLR-SEQ-NO         PIC S9(4)   COMP.                   CL*36
00610      12  ELACTQ-KEY.                                              EL156
00611          16  ACTQ-COMP-CD        PIC X.                              CL*36
00612          16  ACTQ-CARRIER        PIC X.                              CL*36
00613          16  ACTQ-CLAIM-NO       PIC X(7).                           CL*36
00614          16  ACTQ-CERT-NO        PIC X(11).                          CL*36
00615      12  W-NOTE-KEY.                                                 CL*82
00616          16  W-NOTE-COMP-CD      PIC X.                              CL*82
00617          16  W-NOTE-CERT-KEY.                                        CL*82
00618              20  W-NOTE-CARRIER  PIC X.                              CL*82
00619              20  W-NOTE-GROUPING PIC X(6).                           CL*82
00620              20  W-NOTE-STATE    PIC XX.                             CL*93
00621              20  W-NOTE-ACCOUNT  PIC X(10).                          CL*82
00622              20  W-NOTE-EFF-DT   PIC XX.                             CL*82
00623              20  W-NOTE-CERT-NO  PIC X(11).                          CL*82
00624      12  WS-ERACCT-SAVE-KEY      PIC X(20).                          CL*36
00625      12  WS-ERACCT-HOLD-RECORD   PIC X(2000).                        CL*36
00626      12  ERACCT-KEY.                                              EL156
00627          16  ERACCT-PARTIAL-KEY.                                     CL*36
00628              20  ACCT-COMP-CD    PIC X.                              CL*36
00629              20  ACCT-CARRIER    PIC X.                              CL*36
00630              20  ACCT-GROUPING   PIC X(6).                           CL*36
00631              20  ACCT-STATE      PIC XX.                             CL*93
00632              20  ACCT-ACCOUNT    PIC X(10).                          CL*36
00633          16  ACCT-EXP-DT         PIC XX.                             CL*36
00634          16  FILLER              PIC X(4) VALUE SPACES.              CL*36
00635      12  ELCHKQ-KEY.                                              EL156
00636          16  CHKQ-COMP-CD        PIC X.                              CL*36
00637          16  CHKQ-CONTROL        PIC S9(8) COMP.                     CL*36
00638          16  CHKQ-SEQ-NO         PIC S9(4) COMP.                     CL*36
00639      12  ELBENE-KEY.                                              EL156
00640          16  BENE-COMP-CD         PIC X.                             CL*88
00641          16  BENE-RECORD-TYPE     PIC X.                             CL*88
00642          16  BENE-NUMBER          PIC X(10).                         CL*36
00643      12  ERCOMP-KEY.                                                 CL*25
00644          16  COMP-COMP-CD        PIC X.                              CL*88
00645          16  COMP-CARRIER        PIC X.                              CL*88
00646          16  COMP-GROUP          PIC X(06).                          CL*36
00647          16  COMP-FIN-RESP       PIC X(10).                          CL*36
00648          16  COMP-ACCOUNT        PIC X(10).                          CL*36
00649          16  COMP-TYPE           PIC X.                              CL*88
00650                                                                      CL*88
00651      12  DLO035-KEY.                                                 CL*88
00652          16  DL35-PROCESS-TYPE   PIC X.                              CL*88
00653          16  DL35-CREDIT-CARD    PIC X(20).                          CL*88
00654          16  DL35-BEN-TYPE       PIC XX.                             CL*88
00655          16  DL35-PAYMENT-AMT    PIC 9(7)V99.                        CL*88
00656          16  DL35-RETURN-CODE    PIC XX.                             CL*88
00657                                                                      CL*88
00658      EJECT                                                        EL156
00659      COPY ELCDATE.                                                   CL*43
00660      EJECT                                                        EL156
00661      COPY ELCLOGOF.                                                  CL*43
00662      EJECT                                                        EL156
00663      COPY ELCCALC.                                                   CL*43
021406     COPY ELCICALC.
00664      EJECT                                                        EL156
00665      COPY ELCATTR.                                                   CL*43
00666      EJECT                                                        EL156
00667      COPY ELCEMIB.                                                   CL*43
00668      EJECT                                                        EL156
00669      COPY ELCLNKLT.                                                  CL*65
00670      EJECT                                                           CL*65
00671      COPY ELCNWA.                                                    CL*82
00672      EJECT                                                        EL156
00673      COPY ELCDMO.                                                    CL*82
00674      EJECT                                                           CL*82
00675      COPY ELCDMDCD.                                                  CL*82
00676      EJECT                                                           CL*82
00677      COPY ELCDCTB.                                                   CL*82
00678      EJECT                                                           CL*82
00679  01  CD-RCRD-LENGTH                  PIC S9(4)  COMP VALUE +132.     CL*93
00680  01  DCT-RCRD-LENGTH                 PIC S9(4)  COMP VALUE +53.      CL*93
00681  01  DM-DMO-LENGTH                   PIC S9(4)  COMP VALUE +108.     CL*93
00682  01  DL35-LENGTH                     PIC S9(4)  COMP VALUE +34.      CL*88
00683                                  EJECT                               CL*82
00684      COPY ELCINTF.                                                   CL*82
00685      12  PI-REDEF     REDEFINES PI-PROGRAM-WORK-AREA.             EL156
00686          16  PI-SAVE-FORM        PIC X(12).                          CL*82
00687          16  PI-ACCT-KEY         PIC X(26).                       EL156
00688          16  PI-PAYEE-NAME       PIC X(30).                       EL156
00689          16  PI-DAILY-RATE       PIC S9(3)V99.                    EL156
00690          16  PI-MONTH-END-SAVE   PIC XX.                          EL156
00691          16  PI-BENEFIT-SAVE.                                     EL156
00692              20  PI-BEN-DAYS     PIC 99.                          EL156
00693              20  PI-BEN-TYPE     PIC X.                           EL156
00694          16  PI-MANUAL-SW        PIC 9.                           EL156
00695          16  PI-SAVE-CURSOR      PIC S9(4) COMP.                     CL*81
00696          16  PI-PASS-SW          PIC X.                           EL156
00697          16  PI-PFKEY-USED       PIC X.                           EL156
00698          16  PI-CK-ASSIGN        PIC X.                           EL156
00699          16  PI-FATAL-COUNT      PIC 99.                          EL156
00700          16  PI-FORCE-COUNT      PIC 99.                          EL156
00701          16  PI-SAVE-INPUT.                                       EL156
00702              20  PI-PMTTYPE      PIC X.                           EL156
00703              20  PI-PAYEE.                                           CL*16
00704                  24  PI-PAYEE-TYPE PIC X.                            CL*88
00705                  24  PI-PAYEE-SEQ  PIC X.                            CL*88
00706                  24  PI-PAYEE-SEQ-NUM REDEFINES                      CL*16
00707                    PI-PAYEE-SEQ    PIC S9.                           CL*88
00708              20  PI-PMTNOTE1     PIC X(60).                          CL*16
00709              20  PI-PMTNOTE2     PIC X(60).                          CL*16
00710              20  PI-OFFLINE      PIC X.                           EL156
00711              20  PI-ECHECKNO     PIC X(7).                        EL156
00712              20  PI-ECHECKNO-RDF REDEFINES PI-ECHECKNO.              CL*65
00713                  28  PI-ECHECKNO-N     PIC X.                        CL*65
00714                  28  PI-ECHECKNO-6     PIC X(6).                     CL*65
00715              20  PI-ECHECKNO-RDF1 REDEFINES PI-ECHECKNO.             CL*65
00716                  24  PI-ECHECKNO-CR    PIC XX.                       CL*93
00717                  24  PI-ECHECKNO-5     PIC X(5).                     CL*65
00718              20  PI-HOLDTIL      PIC 9(6).                        EL156
00719              20  PI-EPYFROM      PIC 9(6).                        EL156
00720              20  PI-EPYTHRU      PIC 9(6).                        EL156
00721              20  PI-EDAYS        PIC S9(5).                       EL156
00722              20  PI-EPYAMT       PIC S9(7)V99.                    EL156
00723              20  PI-ERESV        PIC S9(7)V99.                       CL**2
00724              20  PI-EEXPENS      PIC 9(6)V99.                     EL156
00725              20  PI-ETYPE        PIC X.                           EL156
00726              20  PI-CASH         PIC X.                              CL*16
00727              20  PI-GROUPED      PIC X.                              CL*16
00728              20  PI-INT-RATE     PIC S99V9(5).
00729              20  PI-AIGFROM      PIC 9(6).                           CL*65
00730              20  PI-LOAN-NO      PIC X(20).                          CL*78
052506             20  PI-PROOF-DATE   PIC 9(6).
                   20  PI-PRINT-EOB-YN PIC X.
020413             20  PI-PRINT-CLM-FRM-YN PIC X.
020413             20  PI-PRINT-SURVEY-YN PIC X.
102413             20  PI-SPECIAL-RELEASE-YN PIC X.
120115             20  pi-int-to-rem-borr pic x.
00731          16  PI-SAVE-COMPUTED.                                    EL156
00732              20  PI-CCHECKNO     PIC X(7).                        EL156
00733              20  PI-CCHECKNO-RDF REDEFINES PI-CCHECKNO.              CL*65
00734                  28  PI-CCHECKNO-N     PIC X.                        CL*65
00735                  28  PI-CCHECKNO-6     PIC X(6).                     CL*65
00736              20  PI-CCHECKNO-RDF1 REDEFINES PI-CCHECKNO.             CL*65
00737                  24  PI-CCHECKNO-CR    PIC XX.                       CL*93
00738                  24  PI-CCHECKNO-5     PIC X(5).                     CL*65
00739              20  PI-CPYFROM      PIC XX.                          EL156
00740              20  PI-CPYTHRU      PIC XX.                          EL156
00741              20  PI-CDAYS        PIC S9(5).                       EL156
00742              20  PI-CPYAMT       PIC S9(7)V99.                    EL156
00743              20  PI-CRESV        PIC 9(6)V99.                     EL156
00744              20  PI-CEXPENS      PIC 9(6)V99.                     EL156
00745              20  PI-PMT-APPR-SW  PIC X.                           EL156
00746                  88 PI-PMT-APPROVED    VALUE 'G' 'Y'.                CL*43
00747                  88 PI-PMT-GRADUATED   VALUE 'G'.                    CL*43
00748                  88 PI-PMT-NO-APPROVAL VALUE ' ' 'N'.                CL*43
00749          16  PI-PREV-CLMNO       PIC X(7).                           CL*24
00750          16  PI-LF-COVERAGE-TYPE PIC X.                              CL*88
00751          16  PI-MAX-PMT-TOL      PIC S9(7)V99  COMP-3.               CL*47
00752          16  PI-MAX-LF-PMT-TOL   PIC S9(7)V99  COMP-3.               CL*47
00753          16  PI-PROVISIONAL-IND  PIC X.                              CL*65
00754              88 PI-PROV-PMT      VALUE 'P'.                          CL*65
00755          16  PI-PREV-TRLR-KEY    PIC X(22).                          CL*65
00756          16  PI-SPLIT-PMT-SW     PIC X.                              CL*88
00757          16  PI-MAX-EXP-PMT      PIC S9(7)V99 COMP-3.                CL*65
00758          16  PI-ACTIVITY-CODE    PIC 99.                             CL*88
00759          16  PI-RESET-SW         PIC X.                              CL*88
00760          16  PI-SPECIAL-BEN-SW   PIC X.                              CL*88
00761              88 PI-AIG-SPECIAL-BENEFIT VALUE 'Y'.                    CL*65
00762          16  PI-REDUND-PMTAMT    PIC S9(7)V99.                       CL*65
00763          16  PI-EXP-DT           PIC XX.                             CL*65
00764          16  PI-LOAN-DUE-DAY     PIC 99.                             CL*65
00765          16  PI-3RD-PARTY-SW     PIC X.                              CL*88
00766          16  PI-LOAN-UPDATE-SW   PIC X.                              CL*88
00767                                                                      CL*93
00768          16  PI-DMD-DATA.                                            CL*82
00769              20  PI-SV-CERT-KEY       PIC X(21).                     CL*93
00770              20  PI-SV-CERT-NO        PIC X(11).                     CL*93
00771              20  PI-SV-BEN            PIC X(10).                     CL*93
00772              20  PI-SV-CCN            PIC X(16).                     CL*93
00773              20  PI-INTEREST-PAID-IND PIC X.                         CL*93
00774                  88  PI-INTEREST-PAID  VALUE 'Y'.                    CL*91
00775              20  PI-MAX-BENEFIT-AMT   PIC S9(5)V99 COMP-3.           CL*93
00776              20  PI-MAX-BENEFIT-PYMT  PIC S9(5)V99 COMP-3.           CL*93
00777              20  PI-MIN-PAYMENT-AMT   PIC S9(5)V99 COMP-3.           CL*93
00778              20  PI-TIME-OF-LOSS-BAL  PIC S9(5)V99 COMP-3.           CL*93
00779          16  PI-AT-EOB-CODES.                                        CL*93
00780              20 PI-AT-EOB-CODE        PIC XXX   OCCURS 5.            CL*93
021406         16  PI-INT-AMT               PIC S9(5)V99 COMP-3.
012507         16  PI-INT-DAYS              PIC S9(5) COMP-3.
082807         16  PI-INT-RATE-USED         PIC S99V9(5) COMP-3.
031808         16  PI-SAVE-ELAPSED-MONTHS   PIC 9(03).
031808         16  PI-SAVE-ODD-DAYS-OVER    PIC S9(04).
091808         16  PI-CHECK-STATE-FOR-AK    PIC X(02).
041710         16  PI-ORIG-BEN-AMT          PIC S9(7)V99 COMP-3.
041710         16  PI-REM-BEN-AMT           PIC S9(7)V99 COMP-3.
041710         16  PI-MO-BEN-AMT            PIC S9(7)V99 COMP-3.
041710         16  PI-LF-BENEFIT-CD         PIC X(2).
               16  PI-EOB-CODES-EXIST PIC X.
               16  PI-SET-NOTE2-MDT   PIC X.
061511         16  PI-VFY-2ND-BENE          PIC X.
013013         16  PI-CLM-TYPE              PIC X.
020413         16  PI-APPROVAL-LEVEL        PIC X.
032813         16  PI-REAUDIT-INTERVAL      PIC S9(5) COMP-3.
032813         16  PI-REAUDIT-NEEDED        PIC X.
               16  PI-DCC-PRODUCT-CODE      PIC XXX.
               16  PI-AH-BENEFIT-CD         PIC XX.
               16  pi-ah-term               pic s999 comp-3.
               16  pi-ah-benefit-amt        pic s9(7)v99 comp-3.
               16  pi-max-ext               pic 999.
               16  pi-max-ext-used          pic x.
052814         16  PI-APPROVAL-3550-NEEDED  PIC X.
052814         16  PI-JOINT-COV-IND         PIC X.
052814             88  PI-JOINT-COVERAGE       VALUE 'J'.
052814             88  PI-SINGLE-COVERAGE      VALUE ' '.
052814         16  PI-JOINT-INSURED-IND     PIC X.
052814             88  PI-JOINT-INSURED        VALUE 'Y'.
052814             88  PI-PRIMARY-INSURED      VALUE 'N'.
012016         16  pi-int-payees-name       pic x(30).
120115         16  PI-RB-JOINT-COV-IND      PIC X.
120115             88  PI-RB-JOINT-COVERAGE    VALUE 'J'.
120115             88  PI-RB-SINGLE-COVERAGE   VALUE ' '.
013017         16  pi-ach-payment           pic x.
043019         16  PI-DUPE-APPROVAL-NEEDED  PIC X.
052506*00781          16  FILLER                   PIC X(176).
031808*052506         16  FILLER                   PIC X(161). 
091808*031808         16  FILLER                   PIC X(154).
041710*091808         16  FILLER                   PIC X(152).
061511*041710         16  FILLER                   PIC X(94).
043019         16  FILLER                   PIC X(33).
00782                                                                   EL156
00783      EJECT                                                        EL156
00784      COPY ELCJPFX.                                                   CL*43
00785                              PIC X(750).                             CL*42
00786      EJECT                                                        EL156
00787      COPY ELCAID.                                                    CL*43
00788  01  FILLER    REDEFINES DFHAID.                                  EL156
00789      12  FILLER              PIC X(8).                            EL156
00790      12  PF-VALUES           PIC X       OCCURS 2.                EL156
00791      EJECT                                                        EL156
00792      COPY EL156S.                                                    CL*43

00794  LINKAGE SECTION.                                                 EL156
00795  01  DFHCOMMAREA             PIC X(1024).                         EL156
00796                                                                   EL156
00797      COPY ELCMSTR.                                                   CL*43
00798      EJECT                                                           CL*43
00799      COPY ELCCNTL.                                                   CL*43
00800      EJECT                                                        EL156
00801      COPY ELCCERT.                                                   CL*43
00802      EJECT                                                        EL156
00803      COPY ELCTRLR.                                                   CL*43
00804      EJECT                                                        EL156
00805      COPY ELCACTQ.                                                   CL*43
00806      EJECT                                                        EL156
00807      COPY ERCACCT.                                                   CL*43
00808      EJECT                                                        EL156
00809      COPY ELCCHKQ.                                                   CL*43
00810      EJECT                                                        EL156
00811      COPY ELCBENE.                                                   CL*43
00812      EJECT                                                        EL156
00813      COPY ELCARCH.                                                   CL*43
00814      EJECT                                                           CL*20
CIDMOD     COPY ELCDAR.                                                    CL*43
CIDMOD     EJECT                                                           CL*20
00815      COPY ERCCOMP.                                                   CL*43
00816      EJECT                                                        EL156
00817      COPY ERCDMDNT.                                                  CL*82
00818      EJECT                                                           CL*82
041710     COPY ERCCNOT.
                                   COPY ERCPDEF.
                                   COPY ELCCRTT.

00819  PROCEDURE DIVISION.                                              EL156
00820                                                                   EL156
00821      IF EIBCALEN = 0                                              EL156
00822          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL156
00823                                                                   EL156
00824      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.         CL*43
00825      MOVE EIBTRMID               TO QID-TERM.                        CL*43
00826      MOVE 2                      TO EMI-NUMBER-OF-LINES.             CL*43
00827      MOVE '2'                    TO EMI-SWITCH2.                  EL156
00828      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.         EL156
00829      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.           EL156
00830                                                                   EL156
00831      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL156
00832      MOVE '5'                    TO DC-OPTION-CODE.               EL156
00833      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
00834      MOVE DC-BIN-DATE-1          TO WS-TODAY-DATE                    CL*43
00835                                     SAVE-BIN-DATE.                   CL*43
00836      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE.                    EL156
00837      MOVE DC-GREG-DATE-1-YMD     TO SAVE-DATE-YMD.                   CL*82
00838                                                                      CL*82
00839      IF SAVE-DATE-YY > 70                                            CL*94
00840          MOVE 19                 TO SAVE-DATE-CC                     CL*82
00841      ELSE                                                            CL*82
00842          MOVE 20                 TO SAVE-DATE-CC.                    CL*82
00843                                                                   EL156
052506     IF PI-PROOF-DATE NOT = 0 AND LOW-VALUES
052506         MOVE PI-PROOF-DATE TO WS-PROOF-DATE
052506     END-IF.
052506
013013     IF PI-PRINT-EOB-YN = 'Y' OR 'S'
013013        MOVE PI-PRINT-EOB-YN     TO WS-PRINT-EOB-YN
           ELSE
              MOVE 'N'                 TO WS-PRINT-EOB-YN
           END-IF
020413
020413     IF PI-PRINT-CLM-FRM-YN = 'N'
020413         MOVE PI-PRINT-CLM-FRM-YN TO WS-PRINT-CLM-FRM-YN
020413     ELSE
020413         MOVE 'Y'                TO WS-PRINT-CLM-FRM-YN
020413     END-IF
020413
020413     IF PI-PRINT-SURVEY-YN = 'N'
020413         MOVE PI-PRINT-SURVEY-YN TO WS-PRINT-SURVEY-YN
020413     ELSE
020413         MOVE 'Y'                TO WS-PRINT-SURVEY-YN
020413     END-IF
102413
102413     IF PI-SPECIAL-RELEASE-YN = 'Y'
102413         MOVE PI-SPECIAL-RELEASE-YN TO WS-SPECIAL-RELEASE-YN
102413     ELSE
102413         MOVE 'N'                TO WS-SPECIAL-RELEASE-YN
102413     END-IF

00844      EXEC CICS HANDLE CONDITION                                   EL156
00845          NOTOPEN   (9990-ABEND)                                      CL*57
00846          PGMIDERR  (8600-PGRM-NOT-FOUND)                             CL*82
00847          MAPFAIL   (0400-FIRST-TIME)                              EL156
00848          DUPREC    (6180-DUPREC)                                  EL156
00849          ERROR     (9990-ABEND)                                   EL156
00850      END-EXEC.                                                       CL*88
00851                                                                   EL156
00852      IF PI-RETURN-TO-PROGRAM = THIS-PGM                           EL156
00853          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM.                EL156
00854                                                                   EL156
00855      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL156
00856          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL156
00857              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL156
00858              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL156
00859              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL156
00860              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL156
00861              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL156
00862              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL156
00863              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL156
00864              MOVE THIS-PGM             TO PI-CALLING-PROGRAM         CL*43
00865              MOVE LOW-VALUES           TO EL156AI                    CL*81
00866              MOVE 'Y'                  TO FIRST-ENTRY                CL*82
00867          ELSE                                                     EL156
00868              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL156
00869              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL156
00870              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL156
00871              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL156
00872              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL156
00873              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL156
00874              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL156
00875              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL156
00876                                                                   EL156
00877      IF RETURNED-FROM NOT = SPACES
              IF (RETURNED-FROM = 'EL614')
                 MOVE PI-PROGRAM-WORK-AREA (1:60) TO WS-SAVED-EOB-CODES
                 IF WS-SAVED-EOB-CODES NOT = SPACES
                    MOVE 'Y'                TO WS-SET-NOTE2-MDT
                 ELSE
                    MOVE 'N'                TO WS-SET-NOTE2-MDT
                 END-IF
              END-IF
00878          PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0699-EXIT         EL156
00879             MOVE 'Y'             TO FIRST-ENTRY
                  IF RETURNED-FROM = 'EL614'
                     AND (PI-PASS-SW NOT = 'C')
                     IF WS-SAVED-EOB-CODES NOT = SPACES
                        MOVE WS-SAVED-EOB-CODES
                                       TO PI-PMTNOTE2
                                          WS-PMTNOTE2
                        MOVE 'Y'       TO PI-EOB-CODES-EXIST
                     ELSE
                        MOVE SPACES    TO PI-PMTNOTE2
                                          WS-PMTNOTE2
                                          PI-EOB-CODES-EXIST
                     END-IF
                  END-IF
00880             PERFORM 3050-REBUILD-ENTERED THRU 3059-EXIT              CL*16

00881             PERFORM 7010-BUILD-MAP THRU 7023-BYPASS-REMAINING-TERM

                  IF RETURNED-FROM = 'EL614'
                     GO TO 1000-EDIT-DATA
                  END-IF
00882             IF (PMTTYPEI = LOW-VALUES OR SPACES) AND                 CL*93
00883                (PAYEEI   = LOW-VALUES OR SPACES)                     CL*93
00884                GO TO 8100-SEND-INITIAL-MAP                           CL*39
00885             ELSE                                                     CL*39
00886                GO TO 1000-EDIT-DATA.                                 CL*39
00887                                                                   EL156
00888      IF EIBTRNID NOT = TRANS-ID                                   EL156
00889          GO TO 0400-FIRST-TIME.                                   EL156
00890                                                                   EL156
00891      IF EIBAID = DFHCLEAR                                         EL156
00892         IF PI-PASS-SW = 'C'                                          CL*16
00893            PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0699-EXIT          CL*16
00894            PERFORM 3050-REBUILD-ENTERED      THRU 3059-EXIT          CL*93
00895            PERFORM 7010-BUILD-MAP THRU 7020-BYPASS-BENEFIT           CL*28
00896            MOVE 'Y'            TO RETURNED-FROM-B                    CL*93
00897            GO TO 1000-EDIT-DATA                                      CL*16
00898         ELSE                                                         CL*16
00899            GO TO 9400-CLEAR.                                         CL*16
00900                                                                   EL156
00901      IF PI-PASS-SW = 'C'                                             CL*43
00902          MOVE MAP-NAMEB          TO MAP-NAME                         CL*90
00903          IF EIBAID = DFHPF1 OR DFHPF13                            EL156
00904              GO TO 5000-UPDATE                                    EL156
00905            ELSE                                                   EL156
00906              IF EIBAID NOT = DFHENTER                             EL156
00907                  MOVE LOW-VALUES TO EL156BI                       EL156
00908                  MOVE MAP-NAMEB  TO MAP-NAME                      EL156
00909                  MOVE ER-0029    TO EMI-ERROR                     EL156
00910                  MOVE -1         TO  ENTPFBL                      EL156
00911                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL156
00912                  GO TO 8200-SEND-DATAONLY.                        EL156
00913                                                                   EL156
00914      EJECT                                                        EL156
00915  0200-RECEIVE.                                                    EL156
00916      MOVE LOW-VALUES             TO EL156AI.                      EL156
00917      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL156
00918          MOVE ER-0008            TO EMI-ERROR                     EL156
00919          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL156
00920          IF PI-PASS-SW = 'C'                                         CL*43
00921              MOVE -1             TO ENTPFBL                       EL156
00922              GO TO 8200-SEND-DATAONLY                             EL156
00923          ELSE                                                     EL156
00924              MOVE -1             TO PMTTYPEL                      EL156
00925              GO TO 8200-SEND-DATAONLY.                            EL156
00926                                                                      CL*82
00927      IF PI-COMPANY-ID = 'DMD'                                        CL*93
00928          PERFORM 7700-GET-DCT THRU 7700-EXIT.                        CL*84
00929                                                                   EL156
00930      IF PI-PASS-SW = 'C'                                             CL*43
00931          MOVE MAP-NAMEB          TO MAP-NAME.                     EL156
00932                                                                   EL156
00933      EXEC CICS RECEIVE                                            EL156
00934          MAP   (MAP-NAME)                                         EL156
00935          MAPSET(MAPSET-NAME)                                      EL156
               ASIS
00936          INTO  (EL156AI)                                          EL156
00937      END-EXEC.                                                       CL*88
00938                                                                   EL156
00939      IF PI-PASS-SW NOT = 'C'                                         CL*43
00940          GO TO 0250-MAPA.                                         EL156
00941                                                                   EL156
00942      IF ENTPFBL NOT > ZERO                                           CL*94
00943          MOVE MAP-NAMEB          TO MAP-NAME                      EL156
00944          MOVE ER-0029            TO EMI-ERROR                     EL156
00945          MOVE -1                 TO  ENTPFBL                      EL156
00946          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL156
00947          GO TO 8200-SEND-DATAONLY.                                EL156
00948                                                                   EL156
00949      IF (ENTPFBI NUMERIC) AND (ENTPFBI > 0 AND < 25)                 CL*94
00950          MOVE PF-VALUES (ENTPFBI) TO EIBAID                       EL156
00951        ELSE                                                       EL156
00952          MOVE MAP-NAMEB           TO MAP-NAME                        CL*93
00953          MOVE ER-0029             TO EMI-ERROR                       CL*93
00954          MOVE -1                  TO ENTPFBL                         CL*93
00955          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL156
00956          GO TO 8200-SEND-DATAONLY.                                EL156
00957                                                                   EL156
00958      IF EIBAID = DFHPF1 OR DFHPF13                                EL156
00959          IF MODIFY-CAP                                            EL156
00960             GO TO 5000-UPDATE                                        CL*93
00961           ELSE                                                       CL*93
00962             MOVE 'UPDATE'           TO SM-READ                       CL*93
00963             PERFORM 9995-SECURITY-VIOLATION                          CL*93
00964             MOVE MAP-NAMEB          TO MAP-NAME                      CL*93
00965             MOVE ER-0070            TO EMI-ERROR                     CL*93
00966             MOVE -1                 TO  ENTPFBL                      CL*93
00967             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 CL*93
00968             GO TO 8200-SEND-DATAONLY.                                CL*93
00969                                                                   EL156
00970  0250-MAPA.                                                       EL156
00971      IF ENTERPFL = 0                                              EL156
00972          GO TO 0300-CHECK-PFKEYS.                                 EL156
00973                                                                   EL156
00974      IF EIBAID NOT = DFHENTER                                     EL156
00975          MOVE ER-0004            TO EMI-ERROR                     EL156
00976          GO TO 0320-INPUT-ERROR.                                  EL156
00977                                                                   EL156
00978      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)               CL*94
00979          MOVE PF-VALUES (ENTERPFI) TO EIBAID                      EL156
00980      ELSE                                                         EL156
00981          MOVE ER-0029            TO EMI-ERROR                     EL156
00982          GO TO 0320-INPUT-ERROR.                                  EL156
00983                                                                   EL156
00984  0300-CHECK-PFKEYS.                                               EL156
00985      IF EIBAID = DFHPF7 OR DFHPF8                                 EL156
00986          GO TO 8820-PF7-8.                                        EL156
00987                                                                   EL156
00988      IF EIBAID = DFHPF23                                          EL156
00989          GO TO 8810-PF23.                                         EL156
00990                                                                   EL156
00991      IF EIBAID = DFHPF24                                          EL156
00992          GO TO 9200-RETURN-MAIN-MENU.                             EL156
00993                                                                   EL156
00994      IF EIBAID = DFHPF12                                          EL156
00995          GO TO 9500-PF12.                                         EL156
00996                                                                   EL156
00997      IF EIBAID = DFHPF1                                              CL*93
00998         IF PI-PASS-SW = 'A' OR 'B'                                   CL*93
00999           GO TO 0330-BROWSE-FORWARD.                                 CL*93
01000                                                                      CL*16
01001      IF EIBAID = DFHPF2                                              CL*93
01002         IF PI-PASS-SW = 'A' OR 'B'                                   CL*93
01003           GO TO 0350-BROWSE-BACKWARD.                                CL*93
01004                                                                      CL*93
01005      IF EIBAID = DFHPF6
              IF PI-EOB-CODES-EXIST = 'Y'
                 MOVE PI-PMTNOTE2         TO WS-PMTNOTE2
              ELSE
                 MOVE SPACES              TO WS-PMTNOTE2
              END-IF
               PERFORM 3000-MOVE-INPUT-TO-SAVE-AREA THRU 3000-EXIT
               IF WS-SAVE-INPUT NOT = PI-SAVE-INPUT
                  MOVE WS-SAVE-INPUT   TO PI-SAVE-INPUT
               END-IF
01006          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT             CL*82
120115         IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121               OR 'FNL'
01008              MOVE 'EL614'        TO PGM-NAME
                   MOVE WS-PMTNOTE2    TO PI-PROGRAM-WORK-AREA
01009              GO TO 9300-XCTL                                         CL*82
01010          ELSE                                                        CL*82
01011              MOVE XCTL-157       TO PGM-NAME                         CL*82
01012              GO TO 9300-XCTL.                                        CL*82
01013                                                                   EL156
01014      IF EIBAID = DFHPF9                                           EL156
01015         PERFORM 0500-CREATE-TEMP-STORAGE  THRU 0599-EXIT          EL156
01016         MOVE XCTL-141            TO PGM-NAME                      EL156
01017         GO TO 9300-XCTL.                                          EL156
01018                                                                   EL156
01019      IF EIBAID = DFHPF10                                             CL*17
01020         PERFORM 0500-CREATE-TEMP-STORAGE  THRU 0599-EXIT             CL*17
01021         MOVE XCTL-155            TO PGM-NAME                         CL*22
01022         GO TO 9300-XCTL.                                             CL*17
01023                                                                      CL*17
01024      MOVE SPACES                 TO ERRMSG0O                         CL*43
01025                                     ERRMSG1O                         CL*43
01026                                     ERRMSG2O.                        CL*43
01027                                                                   EL156
01028      IF EIBAID = DFHPF3 OR DFHPF4 OR DFHPF5                       EL156
01029          IF PI-PASS-SW NOT = 'B'                                  EL156
01030              MOVE ER-0422        TO EMI-ERROR                     EL156
01031              GO TO 0320-INPUT-ERROR                                  CL*86
01032            ELSE                                                      CL*86
01033              GO TO 0700-TEST-FOR-CHANGES.                            CL*86
01034                                                                   EL156
01035      IF EIBAID = DFHPF11                                             CL*86
01036         IF PI-PASS-SW = 'C'                                          CL*86
01037            MOVE ER-0029          TO EMI-ERROR                        CL*86
01038            GO TO 0320-INPUT-ERROR.                                   CL*86
01039                                                                      CL*39
01040      IF EIBAID = DFHENTER OR DFHPF11                                 CL*40
01041         NEXT SENTENCE                                             EL156
01042      ELSE                                                         EL156
01043         MOVE ER-0029             TO EMI-ERROR                     EL156
01044         GO TO 0320-INPUT-ERROR.                                   EL156
01045                                                                   EL156

01046      PERFORM 3000-MOVE-INPUT-TO-SAVE-AREA THRU 3000-EXIT.         EL156
01047                                                                      CL*30
100314     if pi-epyfrom not = zeros
100314        if pi-epyfrom not = ws-epyfrom
100314           move zeros        to ws-edays
100314        end-if
100314     end-if
100314
100314     if pi-epythru not = zeros
100314        if pi-epythru not = ws-epythru
100314           move zeros        to ws-edays
100314        end-if
100314     end-if
100314
01048      IF WS-SAVE-INPUT NOT = PI-SAVE-INPUT                            CL*88
01049          MOVE WS-SAVE-INPUT      TO PI-SAVE-INPUT                    CL*30
01050          GO TO 1000-EDIT-DATA                                        CL*30
01051      ELSE                                                            CL*30
01052          IF PI-FATAL-COUNT NOT = 0 OR                                CL*88
01053             PI-FORCE-COUNT NOT = 0                                   CL*88
01054              GO TO 1000-EDIT-DATA                                    CL*31
01055          ELSE                                                        CL*31
01056              GO TO 2000-EDIT-DONE.                                   CL*31
01057                                                                   EL156
01058  0320-INPUT-ERROR.                                                EL156
01059      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
01060                                                                   EL156
01061      IF ENTERPFL = 0                                              EL156
01062          MOVE -1                 TO PMTTYPEL                      EL156
01063      ELSE                                                         EL156
01064          MOVE AL-UNBON           TO ENTERPFA                      EL156
01065          MOVE -1                 TO ENTERPFL.                     EL156
01066                                                                   EL156
01067      GO TO 8200-SEND-DATAONLY.                                    EL156
01068                                                                   EL156
01069      EJECT                                                        EL156
01070  0330-BROWSE-FORWARD.                                                CL*16
01071                                                                      CL*16
01072      MOVE CLMNOI            TO PI-CLAIM-NO.                          CL*43
01073      MOVE CARRI             TO PI-CARRIER.                           CL*43
01074      MOVE CERTNOI           TO PI-CERT-PRIME.                        CL*43
01075      MOVE SUFXI             TO PI-CERT-SFX.                          CL*43
01076                                                                      CL*16
01077      MOVE PI-CARRIER        TO MSTR-CARRIER.                         CL*43
01078      MOVE PI-COMPANY-CD     TO MSTR-COMP-CD.                         CL*16
01079      MOVE PI-CLAIM-NO       TO MSTR-CLAIM-NO.                        CL*16
01080      MOVE PI-CERT-NO        TO MSTR-CERT-NO.                         CL*16
01081                                                                      CL*16
01082      MOVE ELMSTR-KEY        TO WS-HOLD-ELMSTR-KEY.                   CL*16
01083                                                                      CL*16
01084      EXEC CICS HANDLE CONDITION                                      CL*16
01085           NOTFND   (0380-CLAIM-END-FILE)                             CL*16
01086           ENDFILE  (0380-CLAIM-END-FILE)                             CL*16
01087      END-EXEC.                                                       CL*16
01088                                                                      CL*16
01089      EXEC CICS STARTBR                                               CL*16
01090           DATASET    ('ELMSTR')                                      CL*16
01091           RIDFLD     (ELMSTR-KEY)                                    CL*16
01092           GTEQ                                                       CL*16
01093      END-EXEC.                                                       CL*16
01094                                                                      CL*16
01095      MOVE 'Y' TO WS-BROWSE-START-SW.                                 CL*16
01096                                                                      CL*16
01097  0335-READNEXT.                                                      CL*16
01098                                                                      CL*16
01099      EXEC CICS READNEXT                                              CL*16
01100           DATASET    ('ELMSTR')                                      CL*16
01101           RIDFLD     (ELMSTR-KEY)                                    CL*16
01102           SET        (ADDRESS OF CLAIM-MASTER)                       CL*81
01103      END-EXEC.                                                       CL*16
01104                                                                      CL*24
01105      IF PI-COMPANY-CD NOT = MSTR-COMP-CD                             CL*93
01106         GO TO 0380-CLAIM-END-FILE.                                   CL*16
01107                                                                      CL*16
01108      IF ELMSTR-KEY = WS-HOLD-ELMSTR-KEY                              CL*93
01109         MOVE CL-CLAIM-NO         TO  PI-PREV-CLMNO                   CL*77
01110         GO TO 0335-READNEXT.                                         CL*16
01111                                                                      CL*24
01112      IF PI-COMPANY-ID = 'CRI'                                        CL*35
01113          IF CL-CLAIM-NO  NOT =  PI-PREV-CLMNO                        CL*24
01114             GO TO 0380-CLAIM-END-FILE.                               CL*24
01115                                                                      CL*77
01116      IF CL-SYSTEM-IDENTIFIER = 'CV'                                  CL*88
01117          GO TO 0335-READNEXT.                                        CL*77
01118                                                                      CL*16
01119      IF PI-CARRIER-SECURITY > SPACES                                 CL*94
01120          IF CL-CARRIER = PI-CARRIER-SECURITY                         CL*88
01121              NEXT SENTENCE                                           CL*49
01122          ELSE                                                        CL*49
01123              GO TO 0335-READNEXT.                                    CL*49
01124                                                                      CL*49
01125      IF PI-ACCOUNT-SECURITY > SPACES                                 CL*94
01126          IF CL-CERT-ACCOUNT = PI-ACCOUNT-SECURITY                    CL*88
01127              NEXT SENTENCE                                           CL*49
01128          ELSE                                                        CL*49
01129              GO TO 0335-READNEXT.                                    CL*49
01130                                                                      CL*49
01131      MOVE MSTR-CARRIER      TO PI-CARRIER.                           CL*16
01132      MOVE MSTR-CLAIM-NO     TO PI-CLAIM-NO.                          CL*16
01133      MOVE MSTR-CERT-NO      TO PI-CERT-NO.                           CL*16
01134                                                                      CL*16
01135      GO TO 0400-FIRST-TIME.                                          CL*16
01136                                                                      CL*16
01137  0350-BROWSE-BACKWARD.                                               CL*16
01138                                                                      CL*16
01139      MOVE CLMNOI            TO PI-CLAIM-NO                           CL*43
01140                                PI-PREV-CLMNO.                        CL*43
01141      MOVE CARRI             TO PI-CARRIER.                           CL*43
01142      MOVE CERTNOI           TO PI-CERT-PRIME.                        CL*43
01143      MOVE SUFXI             TO PI-CERT-SFX.                          CL*43
01144                                                                      CL*16
01145      MOVE PI-CARRIER        TO MSTR-CARRIER.                         CL*43
01146                                                                      CL*16
01147      MOVE PI-COMPANY-CD     TO MSTR-COMP-CD.                         CL*16
01148      MOVE PI-CLAIM-NO       TO MSTR-CLAIM-NO.                        CL*16
01149      MOVE PI-CERT-NO        TO MSTR-CERT-NO.                         CL*16
01150                                                                      CL*16
01151      EXEC CICS HANDLE CONDITION                                      CL*16
01152           NOTFND   (0380-CLAIM-END-FILE)                             CL*16
01153           ENDFILE  (0380-CLAIM-END-FILE)                             CL*16
01154      END-EXEC.                                                       CL*16
01155                                                                      CL*16
01156      EXEC CICS STARTBR                                               CL*16
01157           DATASET    ('ELMSTR')                                      CL*16
01158           RIDFLD     (ELMSTR-KEY)                                    CL*16
01159           GTEQ                                                       CL*16
01160      END-EXEC.                                                       CL*16
01161                                                                      CL*16
01162      MOVE 'Y' TO WS-BROWSE-START-SW.                                 CL*16
01163                                                                      CL*16
01164      EXEC CICS READNEXT                                              CL*16
01165           DATASET    ('ELMSTR')                                      CL*16
01166           RIDFLD     (ELMSTR-KEY)                                    CL*16
01167           SET        (ADDRESS OF CLAIM-MASTER)                       CL*81
01168      END-EXEC.                                                       CL*16
01169                                                                      CL*16
01170      EXEC CICS READPREV                                              CL*16
01171           DATASET    ('ELMSTR')                                      CL*16
01172           RIDFLD     (ELMSTR-KEY)                                    CL*16
01173           SET        (ADDRESS OF CLAIM-MASTER)                       CL*81
01174      END-EXEC.                                                       CL*16
01175                                                                      CL*16
01176  0360-BROWSE-BACKWARD.                                               CL*49
01177                                                                      CL*49
01178      EXEC CICS READPREV                                              CL*16
01179           DATASET    ('ELMSTR')                                      CL*16
01180           RIDFLD     (ELMSTR-KEY)                                    CL*16
01181           SET        (ADDRESS OF CLAIM-MASTER)                       CL*81
01182      END-EXEC.                                                       CL*16
01183                                                                      CL*16
01184      IF PI-COMPANY-CD NOT = MSTR-COMP-CD                             CL*93
01185         GO TO 0380-CLAIM-END-FILE.                                   CL*16
01186                                                                      CL*24
01187      IF PI-COMPANY-ID = 'CRI'                                        CL*35
01188          IF CL-CLAIM-NO  NOT =  PI-PREV-CLMNO                        CL*24
01189             GO TO 0380-CLAIM-END-FILE.                               CL*24
01190                                                                      CL*77
01191      IF CL-SYSTEM-IDENTIFIER = 'CV'                                  CL*88
01192          GO TO 0360-BROWSE-BACKWARD.                                 CL*77
01193                                                                      CL*49
01194      IF PI-CARRIER-SECURITY > SPACES                                 CL*94
01195          IF CL-CARRIER = PI-CARRIER-SECURITY                         CL*88
01196              NEXT SENTENCE                                           CL*49
01197          ELSE                                                        CL*49
01198              GO TO 0360-BROWSE-BACKWARD.                             CL*49
01199                                                                      CL*49
01200      IF PI-ACCOUNT-SECURITY > SPACES                                 CL*94
01201          IF CL-CERT-ACCOUNT = PI-ACCOUNT-SECURITY                    CL*88
01202              NEXT SENTENCE                                           CL*49
01203          ELSE                                                        CL*49
01204              GO TO 0360-BROWSE-BACKWARD.                             CL*49
01205                                                                      CL*16
01206      MOVE MSTR-CARRIER      TO PI-CARRIER.                           CL*16
01207      MOVE MSTR-CLAIM-NO     TO PI-CLAIM-NO.                          CL*16
01208      MOVE MSTR-CERT-NO      TO PI-CERT-NO.                           CL*16
01209                                                                      CL*16
01210      GO TO 0400-FIRST-TIME.                                          CL*16
01211                                                                      CL*16
01212  0380-CLAIM-END-FILE.                                                CL*16
01213                                                                      CL*16
01214      MOVE ER-0130     TO EMI-ERROR.                                  CL*43
01215      MOVE -1          TO CLMNOL.                                     CL*43
01216      MOVE AL-UABON    TO CARRA                                       CL*16
01217                          CLMNOA                                      CL*16
01218                          CERTNOA                                     CL*16
01219                          SUFXA.                                      CL*16
01220                                                                      CL*93
01221      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*94
01222                                                                      CL*16
01223      GO TO 8200-SEND-DATAONLY.                                       CL*16
01224                                                                      CL*16
01225  0400-FIRST-TIME.                                                 EL156
01226                                                                      CL*16
01227      IF WS-BROWSE-START-SW = 'Y'                                     CL*93
01228         MOVE 'N'         TO WS-BROWSE-START-SW                       CL*93
01229         EXEC CICS ENDBR                                              CL*16
01230              DATASET    ('ELMSTR')                                   CL*16
01231         END-EXEC.                                                    CL*16
01232                                                                      CL*16
01233      IF PI-PROCESSOR-ID = 'LGXX'                                  EL156
01234          NEXT SENTENCE                                            EL156
01235      ELSE                                                         EL156
01236          EXEC CICS READQ TS                                       EL156
01237              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL156
01238              INTO    (SECURITY-CONTROL)                           EL156
01239              LENGTH  (SC-COMM-LENGTH)                             EL156
01240              ITEM    (SC-ITEM)                                    EL156
01241          END-EXEC                                                 EL156
01242          MOVE SC-CLAIMS-DISPLAY (10)  TO  PI-DISPLAY-CAP          EL156
01243          MOVE SC-CLAIMS-UPDATE  (10)  TO  PI-MODIFY-CAP.          EL156
01244                                                                   EL156
01245      MOVE LOW-VALUES             TO EL156AI.                      EL156
01246      PERFORM 0800-DELETE-TS THRU 0800-EXIT.                          CL*93
01247      MOVE 'Y'                    TO FIRST-ENTRY.                  EL156
01248      GO TO 7000-SHOW-CLAIM.                                       EL156
01249                                                                   EL156
01250  0500-CREATE-TEMP-STORAGE.                                        EL156
01251      MOVE EIBCPOSN               TO PI-SAVE-CURSOR.               EL156
01252                                                                   EL156
01253      EXEC CICS WRITEQ TS                                          EL156
01254          QUEUE (QID)                                              EL156
01255          FROM  (PROGRAM-INTERFACE-BLOCK)                          EL156
01256          LENGTH(PI-COMM-LENGTH)                                   EL156
01257      END-EXEC.                                                       CL*88
01258                                                                   EL156
01259      EXEC CICS WRITEQ TS                                          EL156
01260          QUEUE (QID)                                              EL156
01261          FROM  (EL156AI)                                          EL156
01262          LENGTH(MAP-LENGTH)                                       EL156
01263      END-EXEC.                                                       CL*88
01264                                                                   EL156
01265  0599-EXIT.                                                       EL156
01266       EXIT.                                                       EL156
01267                                                                      CL*98
01268      EJECT                                                        EL156
01269  0600-RECOVER-TEMP-STORAGE.                                       EL156
01270      EXEC CICS READQ TS                                           EL156
01271          QUEUE (QID)                                              EL156
01272          INTO  (PROGRAM-INTERFACE-BLOCK)                          EL156
01273          LENGTH(PI-COMM-LENGTH)                                   EL156
01274      END-EXEC.                                                       CL*88
01275                                                                   EL156
01276      EXEC CICS READQ TS                                           EL156
01277          QUEUE (QID)                                              EL156
01278          INTO  (EL156AI)                                          EL156
01279          LENGTH(MAP-LENGTH)                                       EL156
01280      END-EXEC.                                                       CL*88
01281                                                                   EL156
01282      PERFORM 0800-DELETE-TS THRU 0800-EXIT.                          CL*93
01283                                                                   EL156
01284      IF PMTTYPEI NOT = LOW-VALUES                                 EL156
01285          MOVE AL-UANON           TO PMTTYPEA.                     EL156
01286                                                                   EL156
01287      IF PAYEEI NOT = LOW-VALUES                                   EL156
01288          MOVE AL-UANON           TO PAYEEA.                       EL156
01289                                                                   EL156
01290      IF NOTE1I NOT = LOW-VALUES                                      CL*93
01291          MOVE AL-UANON           TO NOTE1A.                          CL*16
01292                                                                      CL*16

           IF (RETURNED-FROM = 'EL614')
              MOVE WS-SET-NOTE2-MDT    TO PI-SET-NOTE2-MDT
           END-IF

01293 *    IF (NOTE2I NOT = LOW-VALUES AND SPACES)
      *       AND (PI-EOB-CODES-EXIST = 'Y')
      *       AND (PI-SET-NOTE2-MDT = 'Y')
      *       MOVE ' '                 TO PI-SET-NOTE2-MDT
           IF PI-SET-NOTE2-MDT = 'Y'
01294          MOVE AL-SANON           TO NOTE2A
           END-IF
01295                                                                   EL156
01296      IF OFFLINEI NOT = LOW-VALUES                                 EL156
01297          MOVE AL-UANON           TO OFFLINEA.                     EL156
01298                                                                      CL*16
01299 *    IF GROUPEDI NOT = LOW-VALUES                                    CL*16
01300 *        MOVE AL-UANON           TO GROUPEDA.                        CL*16
01301                                                                      CL*16
01302 *    IF CASHI NOT = LOW-VALUES                                       CL*93
01303 *        MOVE AL-UANON           TO CASHA.                           CL*16
01304                                                                   EL156
01305      IF CHECKNOI NOT = LOW-VALUES                                 EL156
01306          MOVE AL-UANON           TO CHECKNOA.                     EL156
01307                                                                   EL156
01308      IF HOLDTILI NOT = LOW-VALUES                                 EL156
01309          MOVE AL-UANON           TO HOLDTILA.                     EL156
01310                                                                   EL156
01311      IF EPYFROMI NOT = LOW-VALUES                                 EL156
01312          MOVE AL-UANON           TO EPYFROMA.                     EL156
01313                                                                   EL156
01314      IF AIGFROMI NOT = LOW-VALUES                                    CL*65
01315          MOVE AL-UANON           TO AIGFROMA.                        CL*65
01316                                                                      CL*65
01317      IF EPYTHRUI NOT = LOW-VALUES                                    CL*88
01318          MOVE AL-UANON           TO EPYTHRUA.                     EL156
01319                                                                   EL156
01320      IF EDAYSI NOT = LOW-VALUES                                   EL156
01321          MOVE AL-UNNON           TO EDAYSA.                       EL156
01322                                                                   EL156
01323      IF EPYAMTL NOT = +0                                             CL*88
01324          MOVE AL-UNNON           TO EPYAMTA.                      EL156
01325                                                                   EL156
01326      IF ERESVL NOT = +0                                              CL*88
01327          MOVE AL-UNNON           TO ERESVA.                       EL156
01328                                                                   EL156
01329      IF EEXPENSL NOT = +0                                            CL*88
01330          MOVE AL-UNNON           TO EEXPENSA.                     EL156
01331                                                                   EL156
01332      IF ETYPEI NOT = LOW-VALUES                                   EL156
01333          MOVE AL-UNNON           TO ETYPEA.                       EL156
01334                                                                   EL156
052506     IF PROOFDTI NOT = 0
052506         MOVE AL-UNNON           TO PROOFDTA.
052506
013013     IF EOBYNI = 'Y' OR 'N' OR 'S'
              MOVE AL-UANON            TO EOBYNA
           END-IF

020413     IF CLMFMYNI = 'Y' OR 'N'
020413        MOVE AL-UANON            TO CLMFMYNA
020413     END-IF
020413
020413     IF SURVYYNI = 'Y' OR 'N'
111113        IF PI-APPROVAL-LEVEL = '4' OR '5'
020413           MOVE AL-UANON         TO SURVYYNA
020413        END-IF
020413     END-IF
020413
102413     IF SPRELYNI = 'Y' OR 'N'
102413        MOVE AL-UANON            TO SPRELYNA
102413     END-IF
102413
           .
01335  0699-EXIT.                                                       EL156
01336       EXIT.                                                       EL156
01337                                                                      CL*98
01338      EJECT                                                        EL156
01339  0700-TEST-FOR-CHANGES.                                           EL156
01340      IF (EIBAID = DFHPF4) AND (NOT FORCE-CAP)                     EL156
01341          MOVE ER-0433            TO EMI-ERROR                     EL156
01342          GO TO 0750-FUNCTION-ERROR.                               EL156
01343                                                                   EL156
01344      PERFORM 3000-MOVE-INPUT-TO-SAVE-AREA THRU 3000-EXIT.         EL156
01345                                                                   EL156
01346      IF WS-SAVE-INPUT NOT = PI-SAVE-INPUT                         EL156
01347          MOVE WS-SAVE-INPUT      TO PI-SAVE-INPUT                 EL156
01348          MOVE 'A'                TO PI-PASS-SW                    EL156
01349          GO TO 1000-EDIT-DATA.                                    EL156
01350                                                                   EL156
01351      IF PI-FATAL-COUNT NOT = 0                                    EL156
01352          MOVE ER-0499            TO EMI-ERROR                     EL156
01353          GO TO 0750-FUNCTION-ERROR.                               EL156
01354                                                                   EL156
01355      IF (PI-FORCE-COUNT NOT = 0) AND (EIBAID NOT = DFHPF4)        EL156
01356          MOVE ER-0500            TO EMI-ERROR                     EL156
01357          GO TO 0750-FUNCTION-ERROR.                               EL156
01358                                                                   EL156
01359      IF (EIBAID  = DFHPF5 AND PI-CPYAMT = ZEROS) OR               EL156
01360         ((EIBAID = DFHPF3 OR DFHPF4) AND PI-EPYAMT = ZEROS)       EL156
01361            MOVE ER-0542          TO EMI-ERROR                     EL156
01362            MOVE -1               TO EPYAMTL                       EL156
01363            GO TO 0750-FUNCTION-ERROR.                             EL156
01364                                                                   EL156
01365      IF PI-OFFLINE = 'Y'  OR                                      EL156
01366         (PI-PMTTYPE = '4' AND PI-EPYAMT NEGATIVE)                 EL156
01367         MOVE EIBAID              TO PI-PFKEY-USED                    CL*67
01368         GO TO 5000-UPDATE.                                        EL156
01369                                                                   EL156
01370      GO TO 6000-BUILD-SCREEN-B.                                   EL156
01371                                                                   EL156
01372  0750-FUNCTION-ERROR.                                             EL156
01373      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
01374                                                                   EL156
01375      IF ENTERPFL > 0                                                 CL*94
01376          MOVE -1                 TO ENTERPFL                      EL156
01377          MOVE AL-UNBON           TO ENTERPFA                      EL156
01378          GO TO 8200-SEND-DATAONLY                                 EL156
01379      ELSE                                                         EL156
01380          MOVE -1                 TO ENTERPFL                      EL156
01381          MOVE AL-UNNOF           TO ENTERPFA                      EL156
01382          GO TO 8200-SEND-DATAONLY.                                EL156
01383                                                                   EL156
01384      EJECT                                                        EL156
01385  0800-DELETE-TS.                                                     CL*93
01386      EXEC CICS HANDLE CONDITION                                   EL156
01387          QIDERR(0800-EXIT)                                           CL*93
01388      END-EXEC.                                                       CL*88
01389                                                                   EL156
01390      EXEC CICS DELETEQ TS                                         EL156
01391          QUEUE(QID)                                               EL156
01392      END-EXEC.                                                       CL*88
01393                                                                   EL156
01394  0800-EXIT.                                                          CL*93
01395       EXIT.                                                       EL156

061013 0900-GET-DDF-LIMITS.
061013
           if cm-clp-state = zeros or spaces or low-values
              move cm-state            to cm-clp-state
           end-if
061013     MOVE ' '                    TO WS-PDEF-RECORD-SW
061013     MOVE PI-COMPANY-CD          TO ERPDEF-KEY
061013     MOVE cm-clp-state           TO ERPDEF-STATE
061013     MOVE PI-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD

070714     evaluate true
070714        when (cl-claim-type = 'L' or 'P')
070714           and (pi-lf-benefit-cd not = '00' and '  ' and 'DD'
070714             and 'CU')
070714           move 'L'              to erpdef-ben-type
070714           move pi-lf-benefit-cd to erpdef-ben-code
070714        when (cl-claim-type not = 'L' and 'P')
070714           and (pi-ah-benefit-cd not = '00' and '  ')
070714           move 'A'              to erpdef-ben-type
070714           move pi-ah-benefit-cd to erpdef-ben-code
070714        when (cl-claim-type not = 'L' and 'P')
070714           and (pi-ah-benefit-cd = '00' or '  ')
070714           move 'L'              to erpdef-ben-type
070714           move pi-lf-benefit-cd to erpdef-ben-code
070714        when (cl-claim-type = 'L' or 'P')
070714           and (pi-lf-benefit-cd = '00' or '  ' or 'DD' or 'CU')
070714           move 'A'              to erpdef-ben-type
070714           move pi-ah-benefit-cd to erpdef-ben-code
070714        when other
070714           move 'A'              to erpdef-ben-type
070714           move pi-ah-benefit-cd to erpdef-ben-code
070714     end-evaluate

      *    if (cl-claim-type = 'L' or 'P')
      *       and (pi-lf-benefit-cd not = '00' and '  ' and 'DD'
      *          and 'CU')
      *       move 'L'                 to erpdef-ben-type
      *       move pi-lf-benefit-cd    to erpdef-ben-code
      *    else
061013*       MOVE 'A'                 TO ERPDEF-BEN-TYPE
061013*       MOVE PI-AH-BENEFIT-CD    TO ERPDEF-BEN-CODE
      *    end-if

061013     MOVE CL-CERT-EFF-DT         TO ERPDEF-EXP-DT
061013
061013     MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
061013
061013     EXEC CICS STARTBR
061013         DATASET  ('ERPDEF')
061013         RIDFLD   (ERPDEF-KEY)
061013         GTEQ
061013         RESP     (WS-RESPONSE)
061013     END-EXEC
061013
061013     IF NOT WS-RESP-NORMAL
061013        GO TO 0900-EXIT
061013     END-IF
061013
061013     .
061013 0900-READNEXT.
061013
061013     EXEC CICS READNEXT
061013        DATASET  ('ERPDEF')
061013        SET      (ADDRESS OF PRODUCT-MASTER)
061013        RIDFLD   (ERPDEF-KEY)
061013        RESP     (WS-RESPONSE)
061013     END-EXEC
061013
061013     IF NOT WS-RESP-NORMAL
061013        GO TO 0900-ENDBR
061013     END-IF
061013
061013     IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
061013        IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
061013           MOVE 'Y'              TO WS-PDEF-RECORD-SW
061013        ELSE
061013           GO TO 0900-READNEXT
061013        END-IF
061013     ELSE
061013        GO TO 0900-ENDBR
061013     END-IF
061013
061013     IF CM-LOAN-TERM = ZEROS
061013        MOVE CP-ORIGINAL-TERM    TO CP-LOAN-TERM
061013     END-IF
061013
061013     .
061013 0900-ENDBR.
061013
061013     EXEC CICS ENDBR
061013        DATASET  ('ERPDEF')
061013     END-EXEC
061013
061013     .
061013 0900-EXIT.
061013     EXIT.

01398  1000-EDIT-DATA.                                                  EL156
01399                                                                      CL*48
01400      MOVE 0 TO PI-FATAL-COUNT  PI-FORCE-COUNT  PI-BEN-DAYS        EL156
01401          PI-CDAYS  PI-CPYAMT  PI-CRESV  PI-CEXPENS PI-MANUAL-SW
021406         PI-INT-AMT PI-INT-DAYS PI-INT-RATE-USED
01402                                                                   EL156
01403      MOVE LOW-VALUES TO PI-CPYFROM  PI-CPYTHRU.                   EL156
01404                                                                   EL156
01405      IF NOT MODIFY-CAP                                            EL156
01406          MOVE ER-0070            TO EMI-ERROR                     EL156
01407          MOVE -1                 TO PMTTYPEL                      EL156
01408          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL156
01409          GO TO 2000-EDIT-DONE.                                    EL156
01410                                                                   EL156
01411      IF PI-PMTTYPE NOT = '1' AND '2' AND '3' AND '4' AND          EL156
01412                          '5' AND '6'                                 CL*65
01413          MOVE ER-0420            TO EMI-ERROR                     EL156
01414          MOVE -1                 TO PMTTYPEL                      EL156
01415          MOVE AL-UABON           TO PMTTYPEA                      EL156
01416          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL156
01417      ELSE                                                         EL156
01418          MOVE AL-UANON           TO PMTTYPEA.                     EL156
01419                                                                   EL156
020413*    IF PI-OFFLINE = 'Y'                                             CL*93
020413*       IF PI-GROUPED = 'Y'                                          CL*93
020413*          MOVE ER-1880         TO EMI-ERROR                         CL*16
020413*          MOVE -1              TO GROUPEDL                          CL*16
020413*          MOVE AL-UABON        TO GROUPEDA                          CL*16
020413*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL*16
020413*                                                                    CL*16
020413*    IF GROUPEDL > +0                                                CL*94
020413*       IF PI-GROUPED = 'Y' OR 'N'                                   CL*93
020413*          MOVE AL-UANON TO GROUPEDA                                 CL*18
020413*       ELSE                                                         CL*18
020413*          MOVE ER-3140            TO EMI-ERROR                      CL*18
020413*          MOVE -1                 TO GROUPEDL                       CL*18
020413*          MOVE AL-UABON           TO GROUPEDA                       CL*18
020413*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL*18
020413*                                                                    CL*16
020413*    IF CASHL > +0                                                   CL*94
020413*       IF PI-CASH = 'Y' OR 'N'                                      CL*93
020413*          MOVE AL-UANON           TO CASHA                          CL*18
020413*       ELSE                                                         CL*18
020413*          MOVE ER-3141            TO EMI-ERROR                      CL*18
020413*          MOVE -1                 TO CASHL                          CL*18
020413*          MOVE AL-UABON           TO CASHA                          CL*18
020413*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL*18
01444                                                                      CL*78
01445      IF LOANNOL > +0                                                 CL*94
01446          MOVE 'Y'                TO  PI-LOAN-UPDATE-SW.              CL*78
01447                                                                      CL*16
01448  1000-EDIT-PAYEE.                                                    CL*71
01449                                                                      CL*71
01450      IF PI-PAYEE-SEQ NOT NUMERIC                                     CL*69
01451          MOVE ER-0294            TO  EMI-ERROR                       CL*71
01452          MOVE -1                 TO  PAYEEL                          CL*71
01453          MOVE AL-UABON           TO  PAYEEA                          CL*71
01454          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*71
01455          GO TO 1000-CONT-EDIT.                                       CL*71
01456                                                                      CL*69
01457      IF PI-PAYEE-TYPE =  'I' OR 'B' OR 'A' OR 'P' OR                 CL*93
01458                          'E' OR 'O' OR 'Q'                           CL*93
01459          NEXT SENTENCE                                               CL*71
01460      ELSE                                                            CL*71
01461          MOVE ER-0294            TO  EMI-ERROR                       CL*71
01462          MOVE -1                 TO  PAYEEL                          CL*71
01463          MOVE AL-UABON           TO  PAYEEA                          CL*71
01464          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*71
01465          GO TO 1000-CONT-EDIT.                                       CL*71
01466                                                                      CL*71
01467      IF ((PI-PAYEE-TYPE = 'I' OR 'E' OR 'O' OR 'P' OR 'Q')  AND      CL*71
01468         (PI-PAYEE-SEQ NOT < '1' AND                                  CL*94
01469          PI-PAYEE-SEQ NOT > '9'))                                    CL*94
01470       OR                                                             CL*71
01471         ((PI-PAYEE-TYPE = 'A' OR 'B')  AND                           CL*71
01472          (PI-PAYEE-SEQ NOT > '9'))                                   CL*94
01473          MOVE AL-UANON            TO PAYEEA                          CL*71
01474      ELSE                                                            CL*16
01475          MOVE ER-0294            TO EMI-ERROR                     EL156
01476          MOVE -1                 TO PAYEEL                        EL156
01477          MOVE AL-UABON           TO PAYEEA                        EL156
01478          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*16
01479                                                                      CL*94
01480      IF PI-COMPANY-ID = 'DMD'                                        CL*94
01481          IF PI-PAYEE-TYPE = 'B' AND                                  CL*94
01482             PI-PAYEE-SEQ NOT = '0'                                   CL*94
01483                MOVE ER-7841            TO EMI-ERROR                  CL*94
01484                MOVE -1                 TO PAYEEL                     CL*94
01485                MOVE AL-UABON           TO PAYEEA                     CL*94
01486                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             CL*94
01487                                                                      CL*69
01488      IF PI-COMPANY-ID = 'JHL'                                        CL*95
01489          IF PI-PAYEE = 'B0'                                          CL*89
01490              NEXT SENTENCE                                           CL*89
01491            ELSE                                                      CL*89
01492          EXEC CICS HANDLE CONDITION                                  CL*89
01493              ENDFILE(1950-NOT-FOUND)                                 CL*89
01494              NOTFND (1950-NOT-FOUND)                                 CL*89
01495          END-EXEC                                                    CL*89
01496          MOVE 'N'                    TO FOUND-TOL-SW                 CL*89
01497          MOVE PI-COMPANY-ID          TO CNTL-COMP-ID                 CL*89
01498          MOVE '2'                    TO CNTL-REC-TYPE                CL*89
01499          MOVE PI-PROCESSOR-ID        TO CNTL-ACCESS                  CL*89
01500          MOVE +0                     TO CNTL-SEQ-NO                  CL*89
01501          MOVE 'PROC'                 TO FILE-SWITCH                  CL*89
01502          PERFORM 7930-READ-CONTROL THRU 7930-EXIT                    CL*89
01503          IF PI-PAYEE = 'I1' AND                                      CL*89
01504             APPROVAL-LEVEL-2                                         CL*89
01505              NEXT SENTENCE                                           CL*89
01506            ELSE                                                      CL*89
01507          IF NOT PI-USER-ALMIGHTY-YES                                 CL*69
01508              MOVE ER-3546        TO EMI-ERROR                        CL*69
01509              MOVE -1             TO PAYEEL                           CL*69
01510              MOVE AL-UABON       TO PAYEEA                           CL*69
01511              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*69
01512                                                                      CL*65
01513      IF PI-PAYEE-TYPE = 'E'                                          CL*88
01514         IF PI-PMTTYPE = '5' OR '6'                                   CL*88
01515             NEXT SENTENCE                                            CL*65
01516         ELSE                                                         CL*65
01517             MOVE ER-3539            TO EMI-ERROR                     CL*65
01518             MOVE -1                 TO PAYEEL                        CL*65
01519             MOVE AL-UABON           TO PAYEEA                        CL*65
01520             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                CL*65
01521                                                                      CL*71
01522  1000-CONT-EDIT.                                                     CL*71
01523                                                                   EL156
01524      IF PI-OFFLINE NOT = 'Y' AND 'N'                                 CL*93
01525         IF PI-EPYAMT NOT NEGATIVE                                 EL156
01526             MOVE ER-0421         TO EMI-ERROR                     EL156
01527             MOVE -1              TO OFFLINEL                      EL156
01528             MOVE AL-UABON        TO OFFLINEA                      EL156
01529             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL156
01530         ELSE                                                      EL156
01531             MOVE AL-UANON        TO OFFLINEA.                     EL156
01532                                                                   EL156
120115     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121           OR 'FNL'
              IF PI-PRINT-EOB-YN = 'Y' OR 'N'
                 CONTINUE
013013        ELSE
013013           IF PI-PRINT-EOB-YN = 'S' AND
100518              PI-CLM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
013013                CONTINUE
                 ELSE
                    MOVE ER-1561       TO EMI-ERROR
                    MOVE -1            TO EOBYNL
                    MOVE AL-UABON      TO EOBYNA
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
013013        END-IF
020413
020413        IF PI-PRINT-CLM-FRM-YN NOT = 'Y' AND 'N'
020413            MOVE ER-1566        TO EMI-ERROR
020413            MOVE -1             TO CLMFMYNL
020413            MOVE AL-UABON       TO CLMFMYNA
020413            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
020413        END-IF
020413
020413        IF PI-PRINT-SURVEY-YN NOT = 'Y' AND 'N'
020413            MOVE ER-1567        TO EMI-ERROR
020413            MOVE -1             TO SURVYYNL
020413            MOVE AL-UABON       TO SURVYYNA
020413            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
020413        END-IF
020413 
102413        IF PI-SPECIAL-RELEASE-YN NOT = 'Y' AND 'N'
102413            MOVE ER-1569        TO EMI-ERROR
102413            MOVE -1             TO SPRELYNL
102413            MOVE AL-UABON       TO SPRELYNA
102413            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
102413        END-IF
102413 
           END-IF

01533      IF PI-EPYAMT IS NEGATIVE                                        CL*69
01534          IF PI-OFFLINE NOT = 'Y'                                     CL*88
01535              MOVE ER-0836        TO  EMI-ERROR                       CL*69
01536              MOVE -1             TO  OFFLINEL                        CL*69
01537              MOVE AL-UABON       TO  OFFLINEA                        CL*69
01538              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*69
052506
052506     IF PI-PROOF-DATE = ZEROS OR LOW-VALUES OR SPACES
052506        MOVE ER-0872             TO EMI-ERROR
052506        MOVE -1                  TO PROOFDTL
052506        MOVE AL-UABON            TO PROOFDTA
052506        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
052506     ELSE
052506        MOVE '4'                TO DC-OPTION-CODE  
052506        MOVE PI-PROOF-DATE      TO DC-GREG-DATE-1-MDY
052506        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                                                       
052506        IF DATE-CONVERSION-ERROR                           
052506            MOVE ER-0873        TO EMI-ERROR           
052506            MOVE -1                  TO PROOFDTL
052506            MOVE AL-UABON            TO PROOFDTA
052506            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
052506        ELSE
012407            MOVE DC-BIN-DATE-1   TO WS-BIN-PROOF-DT
052506            IF DC-BIN-DATE-1 > WS-TODAY-DATE
052506                MOVE ER-0873    TO EMI-ERROR
052506                MOVE -1                  TO PROOFDTL
052506                MOVE AL-UABON            TO PROOFDTA
052506                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
052506            END-IF
052506        END-IF
052506     END-IF.
01539                                                                      CL*95
01540      IF PI-COMPANY-ID = 'DMD'                                        CL*95
01541         IF NOTE1L NOT = ZEROS                                        CL*95
01542             MOVE ER-7844        TO  EMI-ERROR                        CL*95
01543             MOVE -1             TO  NOTE1L                           CL*95
01544             MOVE AL-UABON       TO  NOTE1A                           CL*95
01545             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                CL*95
01546                                                                      CL*82
01547      IF PI-COMPANY-ID = 'DMD'                                        CL*88
01548         IF NOTE2L NOT = ZEROS                                        CL*93
01549           GO TO 1004-DMD-EOB-CHECKS.                                 CL*96
01550                                                                   EL156
01551      IF NOTE1L NOT > +0                                              CL*94
01552          GO TO 1005-CHECK-ERRORS.                                    CL*69
01553                                                                      CL*69
01554      IF PI-COMPANY-ID NOT = 'FLI' AND 'FLU' AND 'HAN' AND 'JHL'      CL*69
01555          GO TO 1005-CHECK-ERRORS.                                    CL*69
01556                                                                      CL*69
01557      IF PI-COMPANY-ID = 'HAN' OR 'JHL'                               CL*69
01558          GO TO 1003-HAN-PMT-NOTES.                                   CL*69
01559                                                                      CL*69
01560      MOVE NOTE1I                 TO WS-FLI-PMTNOTE.                  CL*69
01561                                                                   EL156
01562      IF WS-PMTOPTION NUMERIC                                      EL156
01563         IF WS-PMTOPTION > '00' AND < '17'                            CL*94
01564            NEXT SENTENCE                                          EL156
01565         ELSE                                                      EL156
01566            MOVE ER-0618          TO EMI-ERROR                     EL156
01567            MOVE -1               TO NOTE1L                           CL*16
01568            MOVE AL-UABON         TO NOTE1A                           CL*16
01569            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL156
01570      ELSE                                                         EL156
01571         GO TO 1005-CHECK-ERRORS.                                  EL156
01572                                                                   EL156
01573      IF WS-PMTOPTION = '05' AND                                   EL156
01574         WS-DOCNAME = SPACES                                       EL156
01575           MOVE ER-0616             TO EMI-ERROR                      CL*93
01576           MOVE -1                  TO NOTE1L                         CL*93
01577           MOVE AL-UABON            TO NOTE1A                         CL*93
01578           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                  CL*93
01579                                                                   EL156
01580      IF WS-PMTOPTION = '15' OR '16'                                  CL*93
01581          EXEC CICS BIF DEEDIT                                     EL156
01582              FIELD (WS-AMOUNT)                                       CL*93
01583              LENGTH(08)                                           EL156
01584          END-EXEC                                                    CL*88
01585         IF (WS-NUM-AMT = ZEROS) OR                                EL156
01586            (WS-AMOUNT NOT NUMERIC)                                EL156
01587            MOVE ER-0617          TO EMI-ERROR                     EL156
01588            MOVE -1               TO NOTE1L                           CL*16
01589            MOVE AL-UABON         TO NOTE1A                           CL*16
01590            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL156
01591                                                                   EL156
01592      MOVE WS-FLI-PMTNOTE TO NOTE1I WS-PMTNOTE1 PI-PMTNOTE1.          CL*16
01593                                                                      CL*69
01594      GO TO 1005-CHECK-ERRORS.                                        CL*69
01595                                                                      CL*69
01596  1003-HAN-PMT-NOTES.                                                 CL*69
01597                                                                      CL*69
01598      MOVE NOTE1I                 TO WS-HAN-PMTNOTE.                  CL*69
01599                                                                      CL*69
01600      IF WS-HAN-PMT-CODE = SPACE                                      CL*69
01601          MOVE SPACES             TO WS-HAN-PMT-NOTE                  CL*69
01602      ELSE                                                            CL*69
01603      IF WS-HAN-PMT-CODE = 'A'                                        CL*69
01604          MOVE ':FINAL PAYMENT / MAXIMUM BENEFIT-PAID'                CL*69
01605                                  TO WS-HAN-PMT-NOTE                  CL*69
01606      ELSE                                                            CL*69
01607      IF WS-HAN-PMT-CODE = 'B'                                        CL*69
01608          MOVE ':FINAL PAYMENT / DECEASED'                            CL*69
01609                                  TO WS-HAN-PMT-NOTE                  CL*69
01610      ELSE                                                            CL*69
01611      IF WS-HAN-PMT-CODE = 'C'                                        CL*69
01612          MOVE ':FINAL PAYMENT / NO LONGER DISABLED'                  CL*69
01613                                  TO WS-HAN-PMT-NOTE                  CL*69
01614      ELSE                                                            CL*69
01615      IF WS-HAN-PMT-CODE = 'D'                                        CL*69
01616          MOVE ':FINAL PAYMENT / ADDL INFO NOT PROVIDED'              CL*69
01617                                  TO WS-HAN-PMT-NOTE                  CL*69
01618      ELSE                                                            CL*69
01619      IF WS-HAN-PMT-CODE = 'E'                                        CL*69
01620          MOVE ':FINAL PAYMENT / RETURNED TO WORK'                    CL*69
01621                                  TO WS-HAN-PMT-NOTE                  CL*69
01622      ELSE                                                            CL*69
01623      IF WS-HAN-PMT-CODE = 'F'                                        CL*69
01624          MOVE ':FINAL PAYMENT / PAID TO MATURITY DATE'               CL*69
01625                                  TO WS-HAN-PMT-NOTE                  CL*69
01626      ELSE                                                            CL*69
01627      IF WS-HAN-PMT-CODE = 'P'                                        CL*69
01628          MOVE ':PARTIAL PAYMENT' TO WS-HAN-PMT-NOTE.                 CL*69
01629                                                                      CL*69
01630      MOVE WS-HAN-PMTNOTE TO NOTE1I WS-PMTNOTE1 PI-PMTNOTE1.          CL*69
01631      GO TO 1005-CHECK-ERRORS.                                        CL*82
01632                                                                      CL*82
01633  1004-DMD-EOB-CHECKS.                                                CL*96
01634                                                                      CL*82
01635      INITIALIZE PI-AT-EOB-CODES.                                     CL*93
01636                                                                      CL*93
01637      IF NOTE2I (20:21) > SPACES                                      CL*95
01638          MOVE ER-7845          TO EMI-ERROR                          CL*95
01639          MOVE -1               TO NOTE2L                             CL*95
01640          MOVE AL-UABON         TO NOTE2A                             CL*95
01641          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*95
01642          GO TO 1005-CHECK-ERRORS                                     CL*98
01643       ELSE                                                           CL*95
01644          MOVE NOTE2I             TO W-EOB-CODES.                     CL*95
01645                                                                      CL*82
01646      MOVE +1                     TO W-EOB-NDX.                       CL*93
01647                                                                      CL*82
01648  1004-CHECK-EACH-EOB.                                                CL*82
01649                                                                      CL*82
01650      IF W-EOB-NDX > +5                                               CL*94
01651              OR                                                      CL*82
01652         W-EOB-CODE (W-EOB-NDX) = SPACES                              CL*93
01653          GO TO 1004-CHECK-DUP-EOB.                                   CL*97
01654                                                                      CL*82
01655      IF W-EOB-FILLER (W-EOB-NDX) NOT = SPACES                        CL*96
01656          MOVE ER-0956            TO EMI-ERROR                        CL*82
01657          MOVE -1                 TO NOTE2L                           CL*82
01658          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*82
01659          GO TO 1005-CHECK-ERRORS.                                    CL*98
01660                                                                      CL*82
01661      MOVE 'CL'                   TO CD-SYSTEM-ID.                    CL*82
01662      MOVE 'EO'                   TO CD-RECORD-TYPE.                  CL*82
01663      MOVE W-EOB-CODE (W-EOB-NDX) TO CD-RECORD-KEY.                   CL*82
01664      MOVE 'DLO023'               TO PGM-NAME.                        CL*82
01665                                                                      CL*82
01666      EXEC CICS LINK                                                  CL*83
01667          PROGRAM    (PGM-NAME)                                       CL*83
01668          COMMAREA   (CD-COMMUNICATION-AREA)                          CL*83
01669          LENGTH     (CD-RCRD-LENGTH)                                 CL*83
01670      END-EXEC.                                                       CL*88
01671                                                                      CL*83
01672      IF CD-RETURN-CODE = 'OK'                                        CL*93
01673          GO TO 1004-CONT.                                            CL*83
01674                                                                      CL*83
01675      MOVE -1                     TO NOTE2L.                          CL*83
01676                                                                      CL*83
01677      IF CD-RETURN-CODE = '01'                                        CL*88
01678          MOVE ER-8310            TO EMI-ERROR                        CL*83
01679          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*82
01680          GO TO 1005-CHECK-ERRORS.                                    CL*98
01681                                                                      CL*83
01682      IF CD-RETURN-CODE = '02'                                        CL*88
01683          MOVE ER-8311            TO EMI-ERROR                        CL*83
01684          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*83
01685          GO TO 1005-CHECK-ERRORS.                                    CL*98
01686                                                                      CL*83
01687      IF CD-RETURN-CODE = '03'                                        CL*88
01688          MOVE ER-8312            TO EMI-ERROR                        CL*83
01689          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*83
01690          GO TO 1005-CHECK-ERRORS.                                    CL*98
01691                                                                      CL*83
01692      IF CD-RETURN-CODE = '04'                                        CL*88
01693          MOVE ER-8313            TO EMI-ERROR                        CL*83
01694          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*83
01695          GO TO 1005-CHECK-ERRORS.                                    CL*98
01696                                                                      CL*83
01697      IF CD-RETURN-CODE = '05'                                        CL*88
01698          MOVE ER-8314            TO EMI-ERROR                        CL*83
01699          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*83
01700          GO TO 1005-CHECK-ERRORS.                                    CL*98
01701                                                                      CL*83
01702      IF CD-RETURN-CODE = '06'                                        CL*88
01703          MOVE ER-8315            TO EMI-ERROR                        CL*83
01704          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*83
01705          GO TO 1005-CHECK-ERRORS.                                    CL*98
01706                                                                      CL*83
01707      IF CD-RETURN-CODE = 'E1'                                        CL*88
01708          MOVE ER-8316            TO EMI-ERROR                        CL*83
01709          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*83
01710          GO TO 1005-CHECK-ERRORS.                                    CL*98
01711                                                                      CL*83
01712      IF CD-RETURN-CODE = 'N1'                                        CL*88
01713          MOVE ER-8317            TO EMI-ERROR                        CL*83
01714          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*83
01715          GO TO 1005-CHECK-ERRORS.                                    CL*98
01716                                                                      CL*83
01717  1004-CONT.                                                          CL*83
01718      MOVE W-EOB-CODE (W-EOB-NDX) TO PI-AT-EOB-CODE (W-EOB-NDX).      CL*93
01719                                                                      CL*82
01720      ADD +1 TO W-EOB-NDX.                                            CL*93
01721      GO TO 1004-CHECK-EACH-EOB.                                      CL*82
01722                                                                      CL*97
01723  1004-CHECK-DUP-EOB.                                                 CL*97
01724      IF ((PI-AT-EOB-CODE (1) NOT = SPACE) AND                        CL*97
01725          (PI-AT-EOB-CODE (1) = PI-AT-EOB-CODE (2) OR                 CL*97
01726                                PI-AT-EOB-CODE (3) OR                 CL*97
01727                                PI-AT-EOB-CODE (4) OR                 CL*97
01728                                PI-AT-EOB-CODE (5)))                  CL*97
01729                   OR                                                 CL*97
01730         ((PI-AT-EOB-CODE (2) NOT = SPACE) AND                        CL*97
01731          (PI-AT-EOB-CODE (2) = PI-AT-EOB-CODE (3) OR                 CL*97
01732                                PI-AT-EOB-CODE (4) OR                 CL*97
01733                                PI-AT-EOB-CODE (5)))                  CL*97
01734                   OR                                                 CL*97
01735         ((PI-AT-EOB-CODE (3) NOT = SPACE) AND                        CL*97
01736          (PI-AT-EOB-CODE (3) = PI-AT-EOB-CODE (4) OR                 CL*97
01737                                PI-AT-EOB-CODE (5)))                  CL*97
01738                   OR                                                 CL*97
01739         ((PI-AT-EOB-CODE (4) NOT = SPACE) AND                        CL*97
01740          (PI-AT-EOB-CODE (4) = PI-AT-EOB-CODE (5)))                  CL*97
01741             MOVE ER-8000            TO EMI-ERROR                     CL*97
01742             MOVE AL-UABON           TO NOTE2A                        CL*98
01743             MOVE -1                 TO NOTE2L                        CL*98
01744             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 CL*97
01745             GO TO 1005-CHECK-ERRORS.                                 CL*98
01746                                                                   EL156
01747  1005-CHECK-ERRORS.                                               EL156
121802*    IF PI-COMPANY-ID NOT = 'DMD'                                    CL*98
01749         GO TO 1006-CHECK-ERRORS.                                     CL*98
01750                                                                      CL*98
01751 *********  DMD CODE START ******************                         CL*98
121802*    IF CL-CLAIM-TYPE NOT = PI-AH-OVERRIDE-L1 AND 'I' AND 'G'
121802*       GO TO 1006-CHECK-ERRORS.                                     CL*98
121802*                                                                    CL*98
121802*    PERFORM 7700-GET-DCT THRU 7700-EXIT.                            CL*98
121802*                                                                    CL*98
121802*    COMPUTE W-PAYMENT-TOTAL                                         CL*98
121802*         = CL-TOTAL-PAID-AMT + PI-EPYAMT                            CL*98
121802*    IF (W-PAYMENT-TOTAL > PI-MAX-BENEFIT-PYMT)                      CL*98
121802*               AND                                                  CL*98
121802*       (PI-MAX-BENEFIT-PYMT > ZEROS)                                CL*98
121802*          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                     CL*98
121802*          MOVE ER-0967    TO EMI-ERROR                              CL*98
121802*          MOVE -1         TO PMTTYPEL                               CL*98
121802*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL*98
121802*                                                                    CL*98
121802*    GO TO 1006-CHECK-ERRORS.                                        CL*98
01768 *********  DMD CODE END ******************                           CL*98
01769                                                                      CL*98
01770  1006-CHECK-ERRORS.                                                  CL*98

120115     if emi-forcable-ctr = zeros
120115        and emi-fatal-ctr = zeros
120115        continue
120115     else
120115        IF NOT EMI-NO-ERRORS
120115           GO TO 2000-EDIT-DONE
120115        end-if
120115     end-if

01774      EXEC CICS HANDLE CONDITION                                   EL156
01775          NOTFND(1950-NOT-FOUND)                                   EL156
01776      END-EXEC.                                                       CL*88
01777                                                                   EL156
01778      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.                 EL156
01779      MOVE PI-CARRIER             TO MSTR-CARRIER.                    CL*43
01780      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.                EL156
01781      MOVE PI-CERT-NO             TO MSTR-CERT-NO.                 EL156
01782      MOVE 'MSTR'                 TO FILE-SWITCH.                  EL156
01783                                                                   EL156
01784      PERFORM 7900-READ-CLAIM THRU 7900-EXIT.                      EL156
01785                                                                   EL156
01786      IF PI-COMPANY-ID = 'DMD'                                        CL*88
01787          PERFORM 8010-DMD-ERROR-CHECKS THRU 8090-EXIT.               CL*88
01788                                                                      CL*88
012407     IF WS-BIN-PROOF-DT NOT = LOW-VALUES
012407        IF WS-BIN-PROOF-DT < CL-INCURRED-DT
012407           MOVE ER-0873    TO EMI-ERROR
012407           MOVE -1                  TO PROOFDTL
012407           MOVE AL-UABON            TO PROOFDTA
012407           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
012407        END-IF
012407     END-IF

           if cl-benefit-period = zeros
              move er-1667             to emi-error
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           end-if
              
061013     MOVE ELMSTR-KEY             TO ELTRLR-KEY     
061013     MOVE +95                    TO TRLR-SEQ-NO    
061013     EXEC CICS READ                              
061013        DATASET  ('ELTRLR')                     
061013        SET      (ADDRESS OF ACTIVITY-TRAILERS) 
061013        RIDFLD   (ELTRLR-KEY)                   
061013        RESP     (WS-RESPONSE)
061013     END-EXEC
061013
061013     if ws-resp-normal
061013        perform varying s1 from +1 by +1 until
061013           at-note-error-no (s1) = spaces
061013           if at-note-error-no (s1) = '1662'
061013              continue
061013           else
061013              move at-note-error-no (s1)
061013                                 to emi-error
061013              if at-note-error-no (s1) = '1653'
061013                 evaluate true
061013                    when cl-claim-type = 'L'
061013                       move '  LF  '
061013                                 to emi-claim-type
061013                    when cl-claim-type = 'I'
061013                       move '  IU  '
061013                                 to emi-claim-type
022122                    when cl-claim-type = 'F'
022122                       move ' FMLA '
022122                                 to emi-claim-type
022122                    when cl-claim-type = 'B'
022122                       move ' BRV  '
022122                                 to emi-claim-type
022122                    when cl-claim-type = 'H'
022122                       move ' HOSP '
061013                                 to emi-claim-type
100518                    when cl-claim-type = 'O'
100518                       move '  OT  '
100518                                 to emi-claim-type
022122                    when other
022122                       move '  AH  '
022122                                 to emi-claim-type
061013                 end-evaluate
061013              end-if
061013              PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013           end-if
061013        end-perform
061013     end-if

01789      IF CL-AUTO-PAY-SEQ NOT = ZEROS                               EL156
01790          IF PI-PMTTYPE = '5' OR '6'                                  CL*88
01791              NEXT SENTENCE                                           CL*35
01792          ELSE                                                        CL*35
01793              MOVE ER-0555             TO EMI-ERROR                   CL*35
01794              MOVE -1                  TO PMTTYPEL                    CL*35
01795              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*50
01796                                                                      CL*50
01797      IF CL-LAST-CLOSE-REASON = '2'                                   CL*88
01798          IF CL-CLAIM-STATUS = 'C'                                    CL*88
01799              MOVE ER-0760        TO  EMI-ERROR                       CL*50
01800              MOVE -1             TO  PMTTYPEL                        CL*50
01801              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*35
01802                                                                   EL156
01803      IF PI-ACTIVITY-CODE = 9                                         CL*93
01804          MOVE ER-3538            TO  EMI-ERROR                       CL*65
01805          MOVE -1                 TO  PMTTYPEL                        CL*65
01806          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*65
01807                                                                      CL*65
01808      IF PI-COMPANY-ID NOT = 'AIG' AND 'AUK'                          CL*93
01809          GO TO 1008-READ-CERT.                                       CL*65
01810 **********************AIG LOGIC STARTS *********************         CL*82
01811      IF CL-CERT-PRIME = 'INCOMPLETE'                                 CL*88
01812          MOVE ER-3540            TO  EMI-ERROR                       CL*65
01813          MOVE -1                 TO  PMTTYPEL                        CL*65
01814          MOVE AL-UABON           TO  PMTTYPEA                        CL*65
01815          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*65
01816          GO TO 2000-EDIT-DONE.                                       CL*65
01817                                                                      CL*65
01818      IF PI-PMTTYPE = '4'                                             CL*88
01819          IF PI-EPYAMT IS NEGATIVE                                    CL*65
01820              IF PI-ECHECKNO-CR = 'CR'                                CL*88
01821                  NEXT SENTENCE                                       CL*65
01822              ELSE                                                    CL*65
01823                  MOVE 'CR'       TO  PI-ECHECKNO-CR.                 CL*65
01824                                                                      CL*65
01825      IF PI-PMTTYPE = '4'                                             CL*88
01826          IF PI-ECHECKNO-CR = 'CR'                                    CL*88
01827              IF PI-EPYAMT IS NEGATIVE                                CL*65
01828                  NEXT SENTENCE                                       CL*65
01829              ELSE                                                    CL*65
01830                  MOVE ER-0834    TO  EMI-ERROR                       CL*65
01831                  MOVE -1         TO  EPYAMTL                         CL*65
01832                  MOVE AL-UNBON   TO  EPYAMTA                         CL*65
01833                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           CL*65
01834                                                                      CL*65
01835      IF PI-PMTTYPE = '4'                                             CL*88
01836          IF PI-EPYAMT IS NEGATIVE                                    CL*65
01837              IF PI-OFFLINE = 'Y'                                     CL*88
01838                  NEXT SENTENCE                                       CL*65
01839              ELSE                                                    CL*65
01840                  MOVE 'Y'        TO  PI-OFFLINE                      CL*65
01841                                      WS-OFFLINE                      CL*65
01842                                      OFFLINEO                        CL*65
01843                  MOVE AL-UANON   TO  OFFLINEA.                       CL*65
01844                                                                      CL*65
020413*    IF PI-PMTTYPE = '4'                                             CL*88
020413*        IF PI-EPYAMT IS NEGATIVE                                    CL*65
020413*            IF PI-CASH = 'Y'                                        CL*88
020413*                NEXT SENTENCE                                       CL*65
020413*            ELSE                                                    CL*65
020413*                MOVE 'Y'        TO  CASHI PI-CASH WS-CASH           CL*93
020413*                MOVE AL-UANON   TO  CASHA                           CL*65
020413*                GO TO 1007-DEFAULT-AIG-CHECKNO.                     CL*65
020413*                                                                    CL*65
020413*    IF PI-CASH NOT = 'Y' AND 'N'                                    CL*93
020413*         IF PI-OFFLINE = 'Y'  AND                                   CL*93
020413*            PI-ECHECKNO-CR = 'CR'                                   CL*93
020413*              MOVE 'Y'          TO  CASHI PI-CASH WS-CASH           CL*93
020413*              MOVE AL-UANON     TO  CASHA                           CL*65
020413*              GO TO 1007-DEFAULT-AIG-CHECKNO.                       CL*65
020413*                                                                    CL*65
020413*    IF PI-CASH NOT = 'Y' AND 'N'                                    CL*93
020413*        IF PI-PMTTYPE = '5' OR '6'                                  CL*93
020413*            MOVE 'Y'            TO  CASHI PI-CASH WS-CASH           CL*93
020413*            MOVE AL-UANON       TO  CASHA                           CL*65
020413*            GO TO 1007-DEFAULT-AIG-CHECKNO.                         CL*65
020413*                                                                    CL*65
020413*    IF PI-CASH NOT = 'Y' AND 'N'                                    CL*93
020413*       IF CL-ASSOCIATES = 'N' OR 'A'                                CL*93
020413*           MOVE 'Y'             TO  CASHI PI-CASH WS-CASH           CL*93
020413*           MOVE AL-UANON        TO  CASHA                           CL*65
020413*       ELSE                                                         CL*65
020413*           MOVE 'N'             TO  CASHI PI-CASH WS-CASH           CL*93
020413*           MOVE AL-UANON        TO  CASHA.                          CL*65
020413*                                                                    CL*65
020413*    IF PI-PMTTYPE = '5' OR '6'                                      CL*88
020413*        IF PI-CASH = 'N'                                            CL*88
020413*            MOVE ER-0819        TO  EMI-ERROR                       CL*65
020413*            MOVE AL-UANON       TO  CASHA                           CL*65
020413*            MOVE -1             TO  CASHL                           CL*65
020413*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*65
020413*                                                                    CL*65
020413*    IF CL-ASSOCIATES = 'N' OR 'A'                                   CL*93
020413*       IF CASHI = 'N'                                               CL*93
020413*        MOVE ER-3537            TO  EMI-ERROR                       CL*65
020413*        MOVE AL-UANON           TO  CASHA                           CL*65
020413*        MOVE -1                 TO  CASHL                           CL*65
020413*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*65
01888                                                                      CL*65
01889  1007-DEFAULT-AIG-CHECKNO.                                           CL*65
01890                                                                      CL*65
01891      IF CL-CAUSE-CD = SPACES OR LOW-VALUES                           CL*93
01892          MOVE ER-3532            TO  EMI-ERROR                       CL*65
01893          MOVE -1                 TO  PMTTYPEL                        CL*65
01894          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*65
01895 **********************AIG LOGIC ENDS ***********************         CL*82
01896                                                                      CL*65
01897  1008-READ-CERT.                                                     CL*65
01898                                                                      CL*65
01899      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.                 EL156
01900      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.                 EL156
01901      MOVE CL-CERT-GROUPING       TO CERT-GROUPING.                EL156
01902      MOVE CL-CERT-STATE          TO CERT-STATE.                   EL156
01903      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.                 EL156
01904      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.                  EL156
01905      MOVE CL-CERT-NO             TO CERT-CERT-NO.                 EL156
01906      MOVE 'CERT'                 TO FILE-SWITCH.                  EL156
01907                                                                   EL156
01908      PERFORM 7970-READ-CERT THRU 7970-EXIT.                       EL156
01909                                                                   EL156
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                EL156
01911         MOVE CM-LF-LOAN-EXPIRE-DT
                                       TO WS-EXP-DT PI-EXP-DT
                                          ws-extended-exp-dt
01912      ELSE                                                         EL156
01913         MOVE CM-AH-LOAN-EXPIRE-DT
                                       TO WS-EXP-DT PI-EXP-DT
                                          ws-extended-exp-dt
           end-if

022122     move cl-insured-birth-dt    to dc-bin-date-1
022122     move cl-incurred-dt         to dc-bin-date-2
022122     move '1'                    to dc-option-code
022122     PERFORM 9700-LINK-DATE-CONVERT
022122                                 THRU 9700-EXIT
022122     compute ws-att-age =
022122        dc-elapsed-months / 12
022122
022122     MOVE '6'                    TO DC-OPTION-CODE
022122     move zeros                  to dc-elapsed-months
022122                                    dc-elapsed-days
022122     move low-values to dc-bin-date-1 dc-bin-date-2

061013     if pi-dcc-product-code not = spaces
061013        move cm-ah-benefit-cd    to pi-ah-benefit-cd
              move cm-lf-benefit-cd    to pi-lf-benefit-cd
061013        PERFORM 0900-GET-DDF-limits
061013                                 THRU 0900-EXIT
061013        IF PDEF-FOUND
100518         AND CL-CLAIM-TYPE NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
061013           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122              (P1 > +11)
022122              OR ((PD-PROD-CODE (P1) = CL-CLAIM-TYPE)
022122                   AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
061013           END-PERFORM
022122           IF P1 < +12
                    MOVE '6'           TO DC-OPTION-CODE
                    MOVE WS-EXP-DT     TO DC-BIN-DATE-1
081817              IF CL-NO-OF-EXTENSIONS NUMERIC
081817                 MOVE CL-NO-OF-EXTENSIONS TO DC-ELAPSED-MONTHS
081817                                             PI-MAX-EXT
081817              ELSE
061013                 MOVE PD-MAX-EXTENSION (P1)
061013                                 TO DC-ELAPSED-MONTHS
081817              END-IF
                    PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
                    if no-conversion-error
                       move dc-bin-date-2 to ws-extended-exp-dt
                    end-if
022122              if pd-wait-days(p1) not numeric
022122                 move zeros      to pd-wait-days(p1)
022122              end-if
070714           else
070714              move er-1674       to emi-error
070714              PERFORM 9900-ERROR-FORMAT
070714                                 THRU 9900-EXIT
                 end-if
081817        ELSE
081817           IF CL-NO-OF-EXTENSIONS NUMERIC
081817              MOVE CL-NO-OF-EXTENSIONS TO DC-ELAPSED-MONTHS
081817                                          PI-MAX-EXT
081817           ELSE
081817              MOVE ZERO  TO DC-ELAPSED-MONTHS
081817           END-IF
081817           MOVE WS-EXP-DT     TO DC-BIN-DATE-1
081817           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
081817           if no-conversion-error
081817              move dc-bin-date-2 to ws-extended-exp-dt
081817           end-if
              end-if
022122        if pdef-found
022122           AND CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 or 'O'
022122           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122              (P1 > +11)
022122              OR ((PD-PROD-CODE (P1) = CL-CLAIM-TYPE)
022122                AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
022122           END-PERFORM
022122           IF P1 < +12
022122              if pd-ben-pct(p1) not numeric
022122                 move zeros to pd-ben-pct(p1)
022122              end-if
022122           end-if
022122        end-if
081817     ELSE
081817        IF CL-NO-OF-EXTENSIONS NUMERIC
081817           MOVE CL-NO-OF-EXTENSIONS TO DC-ELAPSED-MONTHS
081817                                       PI-MAX-EXT
081817        ELSE
081817           MOVE ZERO  TO DC-ELAPSED-MONTHS
081817        END-IF
081817        MOVE WS-EXP-DT     TO DC-BIN-DATE-1
081817        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
081817        if no-conversion-error
081817           move dc-bin-date-2 to ws-extended-exp-dt
081817        end-if
           end-if

121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
121802*        PERFORM 3400-COMPUTE-EXPIRY THRU 3499-EXIT.                 CL*65
01917                                                                   EL156
01918      IF CL-INCURRED-DT < CM-CERT-EFF-DT                              CL*94
090821         move er-1681            to emi-error
01919 *        MOVE ER-0458            TO EMI-ERROR                     EL156
01920          PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.               EL156
01921                                                                   EL156
           IF (CL-INCURRED-DT > WS-EXP-DT)
              AND (CL-INCURRED-DT < WS-EXTENDED-EXP-DT)
              MOVE ER-1677             TO EMI-ERROR
              PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT
           END-IF

           move ws-extended-exp-dt     to ws-exp-dt pi-exp-dt
01922      IF CL-INCURRED-DT NOT < WS-EXTENDED-EXP-DT
01923          MOVE ER-0459            TO EMI-ERROR                     EL156
01924          PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.               EL156


090821     if cl-claim-type = PI-LIFE-OVERRIDE-L1 OR 'O'
090821        move cm-lf-benefit-cd    to ws-ben-cd
090821        move '4'                 to cntl-rec-type
090821     else
090821        move cm-ah-benefit-cd    to ws-ben-cd
090821        move '5'                 to cntl-rec-type
090821     end-if
090821     move ws-access              to cntl-access
090821     move pi-company-id          to cntl-comp-id
090821     move zeros                  to cntl-seq-no
090821     move 'BENE'                 to file-switch
090821     perform 7200-find-benefit   thru 7200-exit
090821     if not no-benefit-found
090821        move cf-special-calc-cd(sub-1)
090821                                 to ws-special-calc-cd
090821     end-if
090821
090821     perform 7990-get-lo-hi-acct-dates
090821                                 thru 7990-exit
090821     if (cl-incurred-dt >= ws-hi-acct-dt)
090821        and (acct-cancelled)
090821        and ((ws-special-calc-cd = 'O')
090821                 or
090821            (pi-company-id = 'DCC' AND cl-carrier = '7'))
090821        MOVE er-1681             TO EMI-ERROR
090821        PERFORM 1010-GENERAL-ERROR
090821                                 THRU 1010-EXIT
090821     end-if
090821
090821*    if (cl-incurred-dt < ws-lo-acct-dt)
090821*       MOVE er-1681             TO EMI-ERROR
090821*       PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT
090821*    end-if

01926      IF CL-REPORTED-DT = LOW-VALUES                               EL156
01927         MOVE ER-0530             TO EMI-ERROR                     EL156
01928         PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.                EL156
01929                                                                      CL*65
01930      IF PI-COMPANY-ID = 'NCL'                                        CL*65
01931         IF CM-CREDIT-INTERFACE-SW-1 = '2'                            CL*93
01932             MOVE ER-0770            TO EMI-ERROR                     CL*65
01933             PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.               CL*65
01934                                                                      CL*48
01935      IF PI-COMPANY-ID = 'NCL'                                        CL*49
01936         IF (CM-SING-PRM AND CERT-WAS-CREATED-FOR-CLAIM)              CL*48
01937             MOVE ER-0737            TO EMI-ERROR                     CL*48
01938             PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.               CL*48
01939                                                                   EL156
01940      IF CM-ENTRY-STATUS = 'D' OR 'V'                                 CL*88
01941          MOVE ER-0768            TO  EMI-ERROR                       CL*64
01942          PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.                  CL*64
01943                                                                      CL*64
01944      IF CM-CREDIT-INTERFACE-SW-1 = '4'                               CL*88
01945          MOVE ER-0769            TO  EMI-ERROR                       CL*80
01946          PERFORM 1010-GENERAL-ERROR THRU 1010-EXIT.                  CL*80
01947                                                                      CL*80
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                EL156
01949          GO TO 1020-DO-LIFE-EDITS                                 EL156
01950      ELSE                                                         EL156
01951          GO TO 1030-DO-AH-EDITS.                                  EL156
01952                                                                   EL156
01953  1010-GENERAL-ERROR.                                              EL156
01954      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
01955                                                                   EL156
01956      IF EMI-MESSAGE-FORMATTED                                     EL156
01957          MOVE -1                 TO PMTTYPEL.                     EL156
01958                                                                   EL156
01959  1010-EXIT.                                                       EL156
01960      EXIT.                                                        EL156
01961                                                                      CL*98
01962      EJECT                                                        EL156
01963  1020-DO-LIFE-EDITS.                                              EL156
01964      IF CM-LF-BENEFIT-CD = '00'                                      CL*16
01965          IF PI-PMTTYPE = '5' OR '6'                                  CL*88
01966              MOVE ER-0658        TO EMI-ERROR                        CL**4
01967              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT               CL**4
01968          ELSE                                                        CL**4
120115            if (pi-company-id = 'DCC' or 'VPP')
070714               and (pi-dcc-product-code not = spaces)
070714               continue
070714            else
01969                MOVE ER-1775      TO EMI-ERROR
01970                PERFORM 1040-PMTTYPE-ERROR
070714                                 THRU 1040-EXIT
070714            end-if
070714         end-if
070714     end-if 
01971                                                                   EL156
           move cm-ah-benefit-cd to pi-ah-benefit-cd
01972      IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES                      EL156
01973          MOVE ER-0460            TO EMI-ERROR                     EL156
01974          PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.               EL156
01975                                                                   EL156
041710     MOVE CM-LF-BENEFIT-CD      TO PI-LF-BENEFIT-CD.
01976      MOVE CM-LF-BENEFIT-CD       TO  WS-BEN-CD.                      CL*25
01977      MOVE WS-ACCESS              TO  CNTL-ACCESS.                    CL*25
01978      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                   CL*25
01979      MOVE '4'                    TO  CNTL-REC-TYPE.                  CL*25
01980      MOVE ZEROS                  TO  CNTL-SEQ-NO.                    CL*25
01981      MOVE 'BENE'                 TO  FILE-SWITCH.                    CL*25
01982      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.                       CL*25
01983      IF NO-BENEFIT-FOUND                                             CL*25
01984          GO TO 1950-NOT-FOUND.                                       CL*25
01985                                                                      CL*90
01986      MOVE CF-LF-COVERAGE-TYPE (SUB-1)   TO  PI-LF-COVERAGE-TYPE.     CL*25
01987                                                                      CL*25
01988      IF CM-LF-DEATH-EXIT-DT = LOW-VALUES                          EL156
100518       AND NOT CM-LF-LUMP-SUM-DISAB
01989          IF PI-PMTTYPE = '4'                                      EL156
01990              IF (PI-LIFE-OVERRIDE-L1 = 'P' OR                        CL*93
01991                 PI-LF-COVERAGE-TYPE = 'P')
100518               AND NOT CM-LF-LUMP-SUM-DISAB
01992                  NEXT SENTENCE                                       CL*25
01993              ELSE                                                    CL*25
01994                  MOVE ER-0461        TO EMI-ERROR                    CL*25
01995                  PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT           CL*25
01996          ELSE                                                        CL*20
01997             NEXT SENTENCE                                            CL*20
01998      ELSE                                                         EL156
01999          IF PI-PMTTYPE = '2'                                      EL156
02000             IF NOT CM-O-B-COVERAGE                                EL156
02001               AND CM-LF-CURRENT-STATUS = '7'                      EL156
02002                 MOVE ER-0462     TO EMI-ERROR                     EL156
02003                 PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.        EL156
02004                                                                      CL*47
02005      IF PI-PMTTYPE = '2'                                             CL*88
02006          IF (CM-LF-CANCEL-DT      NOT = LOW-VALUES AND SPACES) OR    CL*93
02007             (CM-LF-CANCEL-EXIT-DT NOT = LOW-VALUES AND SPACES)       CL*93
02008              MOVE CM-LF-ITD-CANCEL-AMT  TO  WS-REFUND-AMT            CL*93
02009              MOVE ER-0730            TO  EMI-ERROR                   CL*47
02010              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT               CL*47
02011              MOVE WS-REFUND-AMT      TO  EMI-TEXT-VARIABLE (1).      CL*47
02012                                                                      CL*25
02013      IF PI-PMTTYPE = '2'                                             CL*88
02014          IF PI-LIFE-OVERRIDE-L1 = 'P' OR                             CL*93
02015             PI-LF-COVERAGE-TYPE = 'P'                                CL*93
02016              IF CL-PAID-THRU-DT = CL-INCURRED-DT                     CL*88
02017                  MOVE ER-0462    TO  EMI-ERROR                       CL*25
02018                  PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.          CL*25
02019                                                                   EL156
02020      IF CM-LF-CANCEL-DT NOT = LOW-VALUES                          EL156
02021        AND CL-INCURRED-DT > CM-LF-CANCEL-DT                          CL*94
02022          MOVE ER-0466            TO EMI-ERROR                     EL156
02023          PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.               EL156
02024                                                                   EL156
02025      IF PI-COMPANY-ID = 'FLI' OR 'FLU'                            EL156
02026          IF PI-PMTTYPE = '3'                                      EL156
02027              MOVE 0436           TO EMI-ERROR                     EL156
02028              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT            EL156
02029              GO TO 1045-EDIT-PAYEE-ADDR                           EL156
02030          ELSE                                                     EL156
02031              GO TO 1045-EDIT-PAYEE-ADDR.                          EL156
02032                                                                   EL156
100518     IF CL-CLAIM-TYPE = 'O'
100518         IF PI-PMTTYPE = '1' OR '2'                                  CL*93
100518             MOVE ER-1930          TO EMI-ERROR                      CL*25
100518             PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT               CL*25
100518         END-IF
100518     ELSE
02033      IF PI-LIFE-OVERRIDE-L1 = 'P' OR                                 CL*93
02034         PI-LF-COVERAGE-TYPE = 'P'                                    CL*93
02035          IF PI-PMTTYPE = '3'                                         CL*93
02036              MOVE ER-0436          TO  EMI-ERROR                     CL*25
02037              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT               CL*25
02038          ELSE                                                        CL*25
02039              NEXT SENTENCE                                           CL*25
02040      ELSE                                                            CL*20
02041          IF PI-PMTTYPE = '1' OR '3'                                  CL*93
02042              MOVE ER-0436            TO EMI-ERROR                    CL*25
02043              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.              CL*25
041710*
041710     IF PI-COMPANY-ID = 'CID' AND
041710        CL-CERT-STATE = 'SC'  AND
041710        (CM-LF-BENEFIT-CD = '2I' OR '2J' OR '2K' OR '2L')
041710           MOVE ER-0879    TO  EMI-ERROR
041710           PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT
                 move 'NET PAY + 6'    TO EMI-ERROR-TEXT (1) (18:11)
041710     END-IF.

           IF PI-COMPANY-ID = 'AHL'
              evaluate true
                 when CM-LF-BENEFIT-CD = '5I' OR '6J'
                    MOVE ER-0879       TO  EMI-ERROR
                    PERFORM 1040-PMTTYPE-ERROR
                                       THRU 1040-EXIT
                    move 'NET PAY + 6' TO EMI-ERROR-TEXT (1) (18:11)
                 when CM-LF-BENEFIT-CD = '5G' OR '5J' OR '5S' OR
                       '6D' OR '6H' OR '6R'
                    MOVE ER-0879       TO  EMI-ERROR
                    PERFORM 1040-PMTTYPE-ERROR
                                       THRU 1040-EXIT
                    move 'NET PAY + 2' TO EMI-ERROR-TEXT (1) (18:11)
                 when CM-LF-BENEFIT-CD = '5M' OR '6M'
                    MOVE ER-0879       TO  EMI-ERROR
                    PERFORM 1040-PMTTYPE-ERROR
                                       THRU 1040-EXIT
                    move 'NET PAY + 1' TO EMI-ERROR-TEXT (1) (18:11)
              end-evaluate
           END-IF

02045      GO TO 1045-EDIT-PAYEE-ADDR.                                  EL156
02046                                                                   EL156
02047      EJECT                                                        EL156
02048  1030-DO-AH-EDITS.                                                EL156
           MOVE CM-AH-BENEFIT-CD       TO PI-AH-BENEFIT-CD
02049      IF CM-AH-BENEFIT-CD = '00'                                      CL*16
02050          IF PI-PMTTYPE = '5' OR '6'                                  CL*88
02051              MOVE ER-0658        TO EMI-ERROR                        CL**4
02052              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT               CL**4
02053          ELSE                                                        CL**4
02054              MOVE ER-1775        TO EMI-ERROR                        CL**4
02055              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.              CL**4
02056                                                                   EL156
02057      IF CL-TOTAL-PAID-AMT = ZEROS                                 EL156
02058          IF PI-PMTTYPE = '4'                                      EL156
02059              MOVE ER-0438        TO EMI-ERROR                     EL156
02060              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.           EL156
02061                                                                   EL156
02062      IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES                      EL156
02063          IF PI-PMTTYPE = '3'                                      EL156
02064              MOVE ER-0463        TO EMI-ERROR                     EL156
02065              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT            EL156
02066          ELSE                                                     EL156
02067              IF (CL-INCURRED-DT NOT < CM-LF-DEATH-EXIT-DT)           CL*94
02068               AND CM-LF-CURRENT-STATUS = '7'                      EL156
02069                  MOVE ER-0465    TO EMI-ERROR                     EL156
02070                  PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.       EL156
02071                                                                   EL156
02072      IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES                      EL156
02073          IF PI-PMTTYPE = '3'                                      EL156
02074              MOVE ER-0464        TO EMI-ERROR                     EL156
02075              PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT            EL156
02076          ELSE                                                     EL156
02077              IF CL-INCURRED-DT > CM-AH-SETTLEMENT-DT                 CL*94
02078                  MOVE ER-0469    TO EMI-ERROR                     EL156
02079                  PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.       EL156
02080                                                                   EL156
02081      IF CM-AH-CANCEL-DT NOT = LOW-VALUES                          EL156
02082          IF PI-PMTTYPE = '3'                                      EL156
02083              IF CL-INCURRED-DT NOT = CM-AH-CANCEL-DT              EL156
02084                  MOVE ER-0467    TO EMI-ERROR                     EL156
02085                  PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT        EL156
02086              ELSE                                                 EL156
02087                  NEXT SENTENCE                                    EL156
02088          ELSE                                                     EL156
02089              IF CL-INCURRED-DT NOT < CM-AH-CANCEL-DT                 CL*94
02090                  MOVE ER-0468    TO EMI-ERROR                     EL156
02091                  PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT.       EL156
02092                                                                   EL156
112018     IF PI-PMTTYPE = '2'                                          EL156
112018        PERFORM WITH TEST AFTER
112018         VARYING SUB FROM +1 BY +5
112018          UNTIL SUB > +60
112018            OR PI-PMTNOTE2(SUB:1) = 'F'
112018            OR PI-PMTNOTE2(SUB:1) NOT > SPACES
112018        END-PERFORM
112018        IF PI-PMTNOTE2(SUB:1) NOT = 'F'
112018           MOVE ER-3551    TO EMI-ERROR                           EL156
112018           PERFORM 1040-PMTTYPE-ERROR THRU 1040-EXIT              EL156
112018        END-IF
112018     END-IF.

02093      GO TO 1045-EDIT-PAYEE-ADDR.                                  EL156
02094                                                                   EL156
02095  1040-PMTTYPE-ERROR.                                              EL156
02096      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
02097                                                                   EL156
02098      IF EMI-MESSAGE-FORMATTED                                     EL156
02099          MOVE -1                 TO PMTTYPEL                      EL156
02100          MOVE AL-UABON           TO PMTTYPEA.                     EL156
02101                                                                   EL156
02102  1040-EXIT.                                                       EL156
02103      EXIT.                                                        EL156
02104                                                                      CL*98
02105      EJECT                                                        EL156
02106  1045-EDIT-PAYEE-ADDR.                                            EL156
02107                                                                      CL*16
02108      IF PI-OFFLINE = 'N' AND                                      EL156
02109         ((PI-PAYEE-TYPE = 'A' AND CL-ACCOUNT-ADDR-CNT                CL*93
02110            < PI-PAYEE-SEQ-NUM)                                       CL*94
02111         OR                                                           CL*18
02112          (PI-COMPANY-ID NOT = 'AIG' AND 'AUK') AND                   CL*93
02113          (PI-PAYEE-TYPE = 'B')                 AND                   CL*93
02114          (CL-BENIF-ADDR-CNT < PI-PAYEE-SEQ-NUM)                      CL*94
02115         OR                                                           CL*18
02116          (PI-PAYEE-TYPE = 'I' AND CL-INSURED-ADDR-CNT                CL*93
02117            < PI-PAYEE-SEQ-NUM)                                       CL*94
02118         OR                                                           CL*16
02119          (PI-PAYEE-TYPE = 'O' AND CL-OTHER-1-ADDR-CNT                CL*93
02120            < PI-PAYEE-SEQ-NUM)                                       CL*94
02121         OR                                                           CL*16
02122          (PI-PAYEE-TYPE = 'Q' AND CL-OTHER-2-ADDR-CNT                CL*93
02123            < PI-PAYEE-SEQ-NUM)                                       CL*94
02124         OR                                                           CL*16
02125          (PI-PAYEE-TYPE = 'P' AND CL-DOCTOR-ADDR-CNT                 CL*93
02126            < PI-PAYEE-SEQ-NUM)                                       CL*94
02127         OR                                                           CL*65
02128          (PI-PAYEE-TYPE = 'E' AND CL-EMPLOYER-ADDR-CNT               CL*93
02129            < PI-PAYEE-SEQ-NUM))                                      CL*94
02130              MOVE ER-0437        TO EMI-ERROR                     EL156
02131              MOVE -1             TO PAYEEL                        EL156
02132              MOVE AL-UABON       TO PAYEEA                        EL156
02133              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL156
02134                                                                   EL156
02135      IF PI-OFFLINE    = 'N' AND                                      CL*93
02136         PI-PAYEE-TYPE = 'B' AND                                      CL*93
02137         PI-PAYEE-SEQ  = '0'                                          CL*93
02138          PERFORM 7920-READ-BENE THRU 7920-EXIT                       CL*65
02139          IF BENE-FOUND-SW = 'N'                                      CL*65
02140              MOVE ER-0437        TO EMI-ERROR                        CL*65
02141              MOVE -1             TO PAYEEL                           CL*65
02142              MOVE AL-UABON       TO PAYEEA                           CL*65
02143              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*65
02144                                                                      CL*65
02145      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
02146         IF PI-OFFLINE    = 'N'  AND                                  CL*93
02147            PI-PAYEE-TYPE = 'B'  AND                                  CL*93
02148            PI-PAYEE-SEQ  = '9'                                       CL*93
02149              PERFORM 7920-READ-BENE THRU 7920-EXIT                   CL*93
02150              IF BENE-FOUND-SW = 'N'                                  CL*93
02151                 MOVE ER-0437        TO EMI-ERROR                     CL*93
02152                 MOVE -1             TO PAYEEL                        CL*93
02153                 MOVE AL-UABON       TO PAYEEA                        CL*93
02154                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            CL*93
02155                                                                      CL*18
121802*    IF PI-COMPANY-ID = 'CRI'                                        CL*88
121802*        IF CL-CLAIM-TYPE = 'L'                                      CL*88
121802*            MOVE 'N'                TO  WS-GROUPED                  CL*30
121802*                                        PI-GROUPED                  CL*30
121802*                                        GROUPEDI                    CL*30
121802*            MOVE AL-UANON           TO  GROUPEDA                    CL*30
121802*            GO TO 1050-EDIT-CHECK-NO                                CL*30
121802*        ELSE                                                        CL*30
121802*            MOVE CL-CERT-GROUPING   TO  WS-GROUPING                 CL*30
121802*            IF WS-GROUP-1 = 'C'                                     CL*88
121802*                NEXT SENTENCE                                       CL*30
121802*            ELSE                                                    CL*30
121802*                MOVE 'N'            TO  WS-GROUPED                  CL*30
121802*                                        PI-GROUPED                  CL*30
121802*                                        GROUPEDI                    CL*30
121802*                MOVE AL-UANON       TO  GROUPEDA                    CL*30
121802*                GO TO 1050-EDIT-CHECK-NO.                           CL*30
02173                                                                      CL*30
020413*     IF PI-GROUPED = 'Y'                                             CL*93
020413*        IF PI-PAYEE-TYPE = 'B' OR 'A'                                CL*93
020413*           NEXT SENTENCE                                             CL*18
020413*        ELSE                                                         CL*18
020413*           MOVE ER-0437        TO EMI-ERROR                          CL*18
020413*           MOVE -1             TO PAYEEL                             CL*18
020413**02180            MOVE AL-UABON       TO PAYEEA
020413*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL*18
020413*                                                                  EL156
020413*     IF (PI-PAYEE-TYPE = 'B' OR 'A') AND                             CL*93
020413*        (PI-GROUPED    = ' ')                                        CL*93
020413*        NEXT SENTENCE                                                CL*16
020413*     ELSE                                                            CL*16
020413*
02187      GO TO 1050-EDIT-CHECK-NO.

020413*     IF PI-COMPANY-ID = 'CRI'                                        CL*93
020413*        MOVE 'N'           TO GROUPEDI                               CL*18
020413*                              PI-GROUPED                             CL*18
020413*                              WS-GROUPED                             CL*20
020413*        MOVE AL-UANON      TO GROUPEDA.                              CL*20
020413*                                                                     CL*16
020413*     IF PI-COMPANY-ID = 'CRI'                                        CL*93
020413*        IF PI-PAYEE-TYPE = 'B'                                       CL*93
020413*           IF BENE-FOUND-SW = 'Y'                                    CL*93
020413*              IF BE-GROUP-CHECKS-Y-N = 'Y'                           CL*93
020413*                 MOVE 'Y'           TO GROUPEDI                      CL*43
020413*                                       PI-GROUPED                    CL*43
020413*                                       WS-GROUPED                    CL*43
020413*                 MOVE AL-UANON      TO GROUPEDA                      CL*20
020413*              ELSE                                                   CL*18
020413*                 NEXT SENTENCE                                       CL*18
020413*           ELSE                                                      CL*18
020413*              PERFORM 7920-READ-BENE THRU 7920-EXIT                  CL*18
020413*              IF BENE-FOUND-SW = 'Y'                                 CL*18
020413*                 IF BE-GROUP-CHECKS-Y-N = 'Y'                        CL*93
020413*                    MOVE 'Y'      TO GROUPEDI                        CL*43
020413*                                     PI-GROUPED                      CL*43
020413*                                     WS-GROUPED                      CL*43
020413*                    MOVE AL-UANON TO GROUPEDA.                       CL*20
020413*                                                                     CL*16
02213                                                                      CL*16
02214      IF PI-COMPANY-ID NOT = 'CRI'                                    CL*93
02215         GO TO 1050-EDIT-CHECK-NO.                                    CL*36
02216                                                                      CL*36
02217 *************** CRI CODE START ******************                    CL*94
02218      IF PI-PAYEE-TYPE NOT = 'A'                                      CL*93
02219         GO TO 1050-EDIT-CHECK-NO.                                    CL*36
02220                                                                      CL*36
02221      MOVE LOW-VALUES             TO ERACCT-KEY.                      CL*43
02222      MOVE PI-COMPANY-CD          TO ACCT-COMP-CD.                    CL*43
02223      MOVE PI-CARRIER             TO ACCT-CARRIER.                    CL*43
02224      MOVE PI-GROUPING            TO ACCT-GROUPING.                   CL*43
02225      MOVE PI-STATE               TO ACCT-STATE.                      CL*43
02226      MOVE PI-ACCOUNT             TO ACCT-ACCOUNT.                    CL*43
02227      MOVE PI-CERT-EFF-DT         TO ACCT-EXP-DT.                     CL*43
02228      MOVE 'ACCT'                 TO FILE-SWITCH.                     CL*43
02229                                                                      CL*36
02230      MOVE ERACCT-PARTIAL-KEY     TO WS-ERACCT-SAVE-KEY.              CL*43
02231      MOVE SPACES                 TO WS-ERACCT-HOLD-RECORD.           CL*43
02232                                                                      CL*36
02233      EXEC CICS HANDLE CONDITION                                      CL*36
02234           NOTFND   (1950-NOT-FOUND)                                  CL*36
02235           ENDFILE  (1950-NOT-FOUND)                                  CL*36
02236      END-EXEC.                                                       CL*36
02237                                                                      CL*36
02238      EXEC CICS STARTBR                                               CL*36
02239           DATASET   ('ERACCT')                                       CL*36
02240           RIDFLD    (ERACCT-KEY)                                     CL*36
02241           GTEQ                                                       CL*36
02242      END-EXEC.                                                       CL*36
02243                                                                      CL*36
02244  1046-READNEXT-ERACCT.

090821     set eracct-browse-started to true
02245                                                                      CL*36
02246      EXEC CICS READNEXT                                              CL*36
02247           DATASET   ('ERACCT')                                       CL*36
02248           RIDFLD    (ERACCT-KEY)                                     CL*36
02249           SET       (ADDRESS OF ACCOUNT-MASTER)                      CL*81
02250      END-EXEC.                                                       CL*43
02251                                                                      CL*36
02252      IF WS-ERACCT-SAVE-KEY NOT = ERACCT-PARTIAL-KEY                  CL*93
02253         IF WS-ERACCT-HOLD-RECORD = SPACES                            CL*93
02254            GO TO 1950-NOT-FOUND                                      CL*36
02255         ELSE                                                         CL*36
02256            MOVE WS-ERACCT-HOLD-RECORD TO ACCOUNT-MASTER              CL*36
02257            GO TO 1048-CONTINUE.                                      CL*36
02258                                                                      CL*36
02259      IF ACCT-EXP-DT = HIGH-VALUES                                    CL*93
02260         NEXT SENTENCE                                                CL*36
02261      ELSE                                                            CL*36
02262         MOVE ACCOUNT-MASTER TO WS-ERACCT-HOLD-RECORD                 CL*36
02263         GO TO 1046-READNEXT-ERACCT.                                  CL*36
02264                                                                      CL*36
02265  1048-CONTINUE.                                                      CL*36
02266                                                                      CL*36
020413*    IF AM-GROUPED-CHECKS-Y-N = 'Y'                                  CL*93
020413*       MOVE 'Y'                 TO GROUPEDI                         CL*36
020413*                                   PI-GROUPED                       CL*36
020413*                                   WS-GROUPED                       CL*36
020413*       MOVE AL-UANON            TO GROUPEDA.                        CL*36
02272 *************** CRI CODE END ******************                      CL*94
02273                                                                      CL*16
02274      EJECT                                                        EL156
02275  1050-EDIT-CHECK-NO.                                              EL156

090821     if eracct-browse-started
090821        exec cics endbr
090821           dataset('ERACCT')
090821        end-exec
090821        move spaces              to ws-eracct-startbr-ind
090821     end-if

02277      IF PI-OFFLINE = 'Y'                                             CL*93
02278          IF PI-ECHECKNO = SPACES                                     CL*65
02279               MOVE ER-0439        TO EMI-ERROR                       CL*65
02280               GO TO 1090-CHECK-NO-ERROR                              CL*65
02281            ELSE                                                      CL*93
020413*        IF PI-COMPANY-ID = 'AIG' OR 'AUK'                           CL*93
020413*           IF PI-ECHECKNO-N = 'N' AND                               CL*93
020413*              PI-CASH       = 'Y'                                   CL*93
020413*                MOVE ER-3535        TO EMI-ERROR                    CL*65
020413*                GO TO 1090-CHECK-NO-ERROR                           CL*65
020413*             ELSE                                                   CL*93
020413*                MOVE AL-UANON    TO CHECKNOA                        CL*65
020413*                GO TO 1100-EDIT-HOLDTIL                             CL*93
020413*         ELSE                                                       CL*93
02291             MOVE AL-UANON    TO CHECKNOA                             CL*93
02292             GO TO 1100-EDIT-HOLDTIL.                                 CL*93
02293                                                                   EL156
02294      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
02295      MOVE '6'                    TO CNTL-REC-TYPE.                EL156
02296                                                                   EL156
02297      IF CONTROL-IS-ACTUAL-CARRIER                                 EL156
02298          MOVE PI-CARRIER         TO WS-CARR                       EL156
02299      ELSE                                                         EL156
02300          MOVE PI-CARRIER-CONTROL-LEVEL TO WS-CARR.                EL156
02301                                                                      CL*30
02302      IF PI-COMPANY-ID = 'FIA'                                        CL*88
02303          MOVE PI-CARRIER         TO WS-CARR.                         CL*30
02304                                                                   EL156
02305      MOVE WS-CARR-ACCESS         TO CNTL-ACCESS.                  EL156
02306      MOVE 0                      TO CNTL-SEQ-NO.                  EL156
02307      MOVE 'CARR'                 TO FILE-SWITCH.                  EL156
02308                                                                   EL156
02309      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.                    EL156
02310                                                                   EL156
02311      EJECT                                                        EL156
02312      IF PI-COMPANY-ID NOT = 'AIG' AND 'AUK'                          CL*93
02313          GO TO 1051-EDIT-CHECKNO-CONT.                               CL*65
02314                                                                      CL*94
02315 **********************AIG LOGIC STARTS**********************         CL*82
02316      IF PI-ECHECKNO = SPACES                                         CL*65
02317          IF CHECK-NO-MANUAL                                          CL*65
02318              MOVE ER-0441        TO EMI-ERROR                        CL*65
02319              GO TO 1090-CHECK-NO-ERROR                               CL*65
02320          ELSE                                                     EL156
02321              MOVE AL-UANOF       TO CHECKNOA                         CL*65
02322              GO TO 1100-EDIT-HOLDTIL.                                CL*65
02323                                                                      CL*65
02324      IF PI-ECHECKNO NOT = SPACES                                     CL*65
02325          IF CHECK-NO-MANUAL                                          CL*65
02326              MOVE AL-UANON       TO CHECKNOA                      EL156
02327              GO TO 1100-EDIT-HOLDTIL.                                CL*65
02328                                                                   EL156
02329      IF PI-ECHECKNO-CR = 'CR'                                        CL*93
02330         IF PI-OFFLINE = 'Y'                                          CL*93
02331             MOVE AL-UANON       TO CHECKNOA                          CL*65
02332             GO TO 1100-EDIT-HOLDTIL                                  CL*65
02333         ELSE                                                         CL*65
02334             MOVE ER-3534        TO EMI-ERROR                         CL*65
02335             GO TO 1090-CHECK-NO-ERROR.                               CL*65
02336                                                                      CL*65
02337      IF PI-ECHECKNO-N = 'N'                                          CL*93
02338         IF PI-CASH = 'N'                                             CL*93
02339             MOVE AL-UANON       TO CHECKNOA                          CL*65
02340             GO TO 1100-EDIT-HOLDTIL                                  CL*65
02341         ELSE                                                         CL*65
02342             MOVE ER-3535          TO EMI-ERROR                       CL*65
02343             GO TO 1090-CHECK-NO-ERROR.                               CL*65
02344                                                                      CL*65
02345      MOVE ER-0442        TO EMI-ERROR                                CL*65
02346      GO TO 1090-CHECK-NO-ERROR.                                      CL*65
02347 **********************AIG LOGIC ENDS************************         CL*82
02348                                                                      CL*65
02349  1051-EDIT-CHECKNO-CONT.                                             CL*65
02350                                                                      CL*93
02351      IF PI-COMPANY-ID = 'DMD'                                        CL*93
02352         IF PI-ECHECKNO NOT = SPACES                                  CL*93
02353            IF PI-ECHECKNO (6:2) NOT = 'TP'                           CL*93
02354               MOVE ER-0442        TO EMI-ERROR                       CL*93
02355               GO TO 1090-CHECK-NO-ERROR                              CL*93
02356             ELSE                                                     CL*93
02357               GO TO 1100-EDIT-HOLDTIL                                CL*93
02358           ELSE                                                       CL*93
02359            GO TO 1100-EDIT-HOLDTIL.                                  CL*93
02360                                                                      CL*93
02361      IF PI-ECHECKNO = SPACES                                      EL156
02362          IF CHECK-NO-MANUAL                                       EL156
02363              MOVE ER-0441        TO EMI-ERROR                     EL156
02364              GO TO 1090-CHECK-NO-ERROR                            EL156
02365          ELSE                                                     EL156
02366              MOVE AL-UANOF       TO CHECKNOA                      EL156
02367              GO TO 1100-EDIT-HOLDTIL                              EL156
02368      ELSE                                                         EL156
02369          IF CHECK-NO-MANUAL                                          CL*93
02370              MOVE AL-UANON       TO CHECKNOA                      EL156
02371              GO TO 1100-EDIT-HOLDTIL                              EL156
02372          ELSE                                                     EL156
02373              MOVE ER-0442        TO EMI-ERROR                     EL156
02374              GO TO 1090-CHECK-NO-ERROR.                           EL156
02375                                                                   EL156
02376  1090-CHECK-NO-ERROR.                                             EL156
02377      MOVE -1                     TO CHECKNOL.                     EL156
02378      MOVE AL-UABON               TO CHECKNOA.                     EL156
02379      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
02380                                                                   EL156
02381      GO TO 2000-EDIT-DONE.                                        EL156
02382                                                                   EL156
02383      EJECT                                                        EL156
02384  1100-EDIT-HOLDTIL.                                               EL156
040819     MOVE ZERO TO WS-TOT-AMOUNT-PAID
040819                  WS-UNPD-HOLD-UNTIL-DT
022718     SET HOLD-UNTIL-CHK TO TRUE
022718     MOVE CL-PAID-THRU-DT TO WS-HOLD-UNTIL-DT
022718     PERFORM 1166-CHECK-ALL-TRLRS THRU 1166-TRLR-EXIT.
040819     IF CL-TOTAL-PAID-AMT NOT = WS-TOT-AMOUNT-PAID
040819         MOVE ER-3277            TO EMI-ERROR
040819         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040819     END-IF
040819
022718     MOVE SPACE TO WS-HOLD-UNTIL-SW
040819
02385      MOVE AL-UANON               TO HOLDTILA.                     EL156
02386                                                                   EL156
02387      IF PI-HOLDTIL = ZEROS                                        EL156
02388         IF PI-OFFLINE = 'Y'                                       EL156
02389            MOVE ER-0572          TO EMI-ERROR                     EL156
02390            GO TO 1105-HOLDTIL-ERROR                               EL156
02391         ELSE                                                      EL156
02392            GO TO 1110-EDIT-PAID-FROM.                             EL156
02393                                                                   EL156
02394      MOVE '4'                    TO DC-OPTION-CODE.               EL156
02395      MOVE PI-HOLDTIL             TO DC-GREG-DATE-1-MDY.           EL156
02396      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
02397                                                                   EL156
02398      IF DATE-CONVERSION-ERROR                                     EL156
02399          MOVE ER-0446            TO EMI-ERROR                     EL156
02400          GO TO 1105-HOLDTIL-ERROR                                 EL156
           END-IF

PEMTST     IF PI-OFFLINE = 'Y'
              IF DC-BIN-DATE-1 < SAVE-BIN-DATE
02399            MOVE ER-0446            TO EMI-ERROR
02400            GO TO 1105-HOLDTIL-ERROR
              END-IF
           END-IF
040819                                                                  EL156
040819     IF DC-BIN-DATE-1 < WS-UNPD-HOLD-UNTIL-DT
040819        MOVE ER-3278          TO EMI-ERROR
040819        GO TO 1105-HOLDTIL-ERROR
040819     END-IF
02401
02402      IF PI-OFFLINE = 'Y'                                          EL156
02403         GO TO 1110-EDIT-PAID-FROM.                                EL156
02404                                                                   EL156
02405      MOVE DC-BIN-DATE-1          TO DC-BIN-DATE-2.                EL156
02406      MOVE WS-TODAY-DATE          TO DC-BIN-DATE-1.                EL156
02407      MOVE '1'                    TO DC-OPTION-CODE.               EL156
02408      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
02409                                                                   EL156
022718*    IF DC-ELAPSED-DAYS > 30 OR DATE-CONVERSION-ERROR                CL*94
022718     IF DC-ELAPSED-DAYS > 60 OR DATE-CONVERSION-ERROR
02411          MOVE ER-0447            TO EMI-ERROR                     EL156
02412          GO TO 1105-HOLDTIL-ERROR.                                EL156
02413                                                                   EL156
02414      GO TO 1110-EDIT-PAID-FROM.                                   EL156
02415                                                                   EL156
02416  1105-HOLDTIL-ERROR.                                              EL156
02417      MOVE -1                     TO HOLDTILL.                     EL156
02418      MOVE AL-UNBON               TO HOLDTILA.                     EL156
02419      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
02420                                                                   EL156
02421      EJECT                                                        EL156
02422  1110-EDIT-PAID-FROM.                                             EL156
02423      MOVE AL-UANON               TO EPYFROMA.                     EL156
02424                                                                   EL156
02425      IF PI-EPYFROM = ZEROS                                        EL156
02426          GO TO 1120-CALCULATE-PAID-FROM.                          EL156
02427                                                                   EL156
02428      IF PI-PMTTYPE = '5' OR '6'                                      CL*65
02429          MOVE ER-0448            TO EMI-ERROR                     EL156
02430          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL156
02431          MOVE AL-UNBON           TO EPYFROMA                      EL156
02432          MOVE -1                 TO EPYFROML                      EL156
02433          GO TO 1150-EDIT-PAID-THRU.                               EL156
02434                                                                   EL156
02435      MOVE '4'                    TO DC-OPTION-CODE.               EL156
02436      MOVE PI-EPYFROM             TO DC-GREG-DATE-1-MDY.           EL156
02437      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
02438                                                                   EL156
02439      IF DATE-CONVERSION-ERROR                                     EL156
02440          MOVE ER-0449            TO EMI-ERROR                     EL156
02441          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL156
02442          MOVE AL-UNBON           TO EPYFROMA                      EL156
02443          MOVE -1                 TO EPYFROML                      EL156
02444      ELSE                                                         EL156
02445          MOVE DC-BIN-DATE-1      TO WS-SV-EPYFROM.                EL156
02446                                                                   EL156
02447      EJECT                                                        EL156
02448  1120-CALCULATE-PAID-FROM.                                        EL156
02449      IF PI-PMTTYPE = '5' OR '6'                                      CL*65
02450          GO TO 1150-EDIT-PAID-THRU.                               EL156
02451                                                                      CL*65
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
121802*       IF PI-PMTTYPE = '4'                                          CL*93
121802*         GO TO 1150-EDIT-PAID-THRU.                                 CL*65
02455                                                                   EL156
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                EL156
02457          MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1                 EL156
02458          MOVE ' '                TO DC-OPTION-CODE                EL156
02459          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL156
02460          MOVE DC-BIN-DATE-1      TO PI-CPYFROM                    EL156
02461          GO TO 1145-TEST-PAID-FROM.                               EL156
02462                                                                   EL156
02463      IF PI-PMTTYPE = '4'                                          EL156
02464         GO TO 1150-EDIT-PAID-THRU.                                EL156
02465                                                                   EL156
02466      MOVE CM-AH-BENEFIT-CD       TO WS-BEN-CD.                    EL156
02467      MOVE WS-ACCESS              TO CNTL-ACCESS.                  EL156
02468      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
02469      MOVE '5'                    TO CNTL-REC-TYPE.                EL156
02470      MOVE ZEROS                  TO CNTL-SEQ-NO.                  EL156
02471      MOVE 'BENE'                 TO FILE-SWITCH.                  EL156
02472                                                                   EL156
02473      IF WS-BEN-CD = ZEROS                                            CL*16
02474          GO TO 1950-NOT-FOUND.                                    EL156
02475                                                                   EL156
02476      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.                    EL156
02477                                                                   EL156
02478      IF NO-BENEFIT-FOUND                                          EL156
02479          GO TO 1950-NOT-FOUND.                                    EL156
02480                                                                   EL156
02481      MOVE CF-BENEFIT-ALPHA   (SUB-1) TO PI-BENEFIT-SAVE.             CL*43
02482      MOVE CF-SPECIAL-CALC-CD (SUB-1) TO WS-SPECIAL-CALC-CD.       EL156

022122     if pdef-found
022122        if pd-ret-elim(p1) = 'R' or 'E'
022122           move pd-ret-elim(p1)  to pi-ben-type
022122           move pd-wait-days(p1) to pi-ben-days
022122        end-if
022122     end-if

02484      IF PI-BEN-DAYS NOT NUMERIC                                      CL*21
02485         MOVE ZEROS               TO PI-BEN-DAYS.                     CL*27
02486                                                                      CL*21
02487      MOVE PI-BEN-DAYS            TO WS-BEN-DAYS.                     CL*80
02480                                                                   EL156
022718     IF WS-HOLD-UNTIL-DT > CL-PAID-THRU-DT
022718         MOVE WS-HOLD-UNTIL-DT     TO DC-BIN-DATE-1                  CL*65
022718         MOVE '6'                 TO DC-OPTION-CODE                  CL*65
022718         MOVE +0                  TO DC-ELAPSED-MONTHS               CL*65
022718         MOVE +1                  TO DC-ELAPSED-DAYS                 CL*65
022718         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT               CL*65
022718         MOVE DC-BIN-DATE-2       TO PI-CPYFROM                      CL*65
022718         GO TO 1145-TEST-PAID-FROM.                                  CL*65
02488                                                                      CL*80
02489      IF (CL-PAID-THRU-DT NOT = LOW-VALUES AND SPACES)  AND           CL*65
02490         (CL-TOTAL-PAID-AMT > ZEROS)                                  CL*94
02491          MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1                   CL*65
02492          MOVE '6'                 TO DC-OPTION-CODE                  CL*65
02493          MOVE +0                  TO DC-ELAPSED-MONTHS               CL*65
02494          MOVE +1                  TO DC-ELAPSED-DAYS                 CL*65
02495          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT               CL*65
02496          MOVE DC-BIN-DATE-2       TO PI-CPYFROM                      CL*65
02497          GO TO 1145-TEST-PAID-FROM.                                  CL*65
02498                                                                   EL156
02499 ******************************************************************   CL*20
02500 ****** ON FIRST PAYMENTS, ELIM/RETRO EDITS TO BE MADE AGAINST THE    CL*65
02501 ****** SPECIAL ENTERED DATE.                                         CL*65
02502 ******************************************************************   CL*20
02503                                                                      CL*20
02504      IF PI-AIGFROM = ZEROS                                           CL*93
02505          GO TO 1130-CONTINUE-PAID-FROM.                              CL*65
02506                                                                      CL*30
02507      MOVE AL-UANON TO AIGFROMA.                                      CL*65
02508                                                                      CL*65
02509      MOVE '4'                    TO DC-OPTION-CODE.                  CL*65
02510      MOVE PI-AIGFROM             TO DC-GREG-DATE-1-MDY.              CL*65
02511      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*65
02512                                                                      CL*65
02513      IF DATE-CONVERSION-ERROR                                        CL*65
02514          MOVE ER-0449            TO EMI-ERROR                        CL*65
02515          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*65
02516          MOVE AL-UNBON           TO AIGFROMA                         CL*65
02517          MOVE -1                 TO AIGFROML                         CL*65
02518          MOVE CL-INCURRED-DT TO DC-BIN-DATE-1                        CL*65
02519      ELSE                                                            CL*65
02520          MOVE 'Y'                TO WS-AIG-PAYFROM-SW                CL*65
02521          MOVE DC-BIN-DATE-1      TO WS-SV-AIGFROM.                   CL*65
02522                                                                      CL*65
02523      GO TO 1140-CONTINUE-PAID-FROM.                                  CL*65
02524                                                                      CL*65
02525  1130-CONTINUE-PAID-FROM.                                            CL*65
02526                                                                      CL*65
02527      IF (CL-PAID-THRU-DT   = LOW-VALUES OR SPACES) OR                CL*93
02528         (CL-TOTAL-PAID-AMT = ZEROS)                                  CL*93
02529            MOVE CL-INCURRED-DT TO DC-BIN-DATE-1.                     CL*21
02530                                                                      CL*65
02531  1140-CONTINUE-PAID-FROM.                                            CL*65
02532                                                                      CL*20
02533      IF PI-BEN-TYPE = 'R'                                            CL*28
02534          IF PI-BEN-DAYS = ZEROS                                      CL*88
02535              NEXT SENTENCE                                           CL*28
02536          ELSE                                                        CL*28
02537          IF PI-COMPANY-ID NOT = 'DMD'                                CL*94
02538              SUBTRACT 1 FROM PI-BEN-DAYS.                            CL*28
02539                                                                      CL*43
02540      MOVE DC-BIN-DATE-1          TO WS-HOLD-BIN-DATE-1.              CL*65
02541                                                                      CL*94
02542      IF PI-COMPANY-ID = 'DMD'                                        CL*94
02543          MOVE -1                 TO DC-ELAPSED-DAYS                  CL*94
02544          MOVE +0                 TO DC-ELAPSED-MONTHS                CL*94
02545          MOVE '6'                TO DC-OPTION-CODE                   CL*94
02546          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT               CL*94
02547          MOVE DC-BIN-DATE-2      TO DC-BIN-DATE-1                    CL*94
02548          DIVIDE PI-BEN-DAYS BY 30 GIVING DC-ELAPSED-MONTHS           CL*94
02549                                REMAINDER DC-ELAPSED-DAYS             CL*94
02550       ELSE                                                           CL*94
02551          MOVE PI-BEN-DAYS        TO DC-ELAPSED-DAYS                  CL*94
02552          MOVE +0                 TO DC-ELAPSED-MONTHS.               CL*94
02553                                                                      CL*94
02554      MOVE '6'                    TO DC-OPTION-CODE.               EL156
02555      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
02556      MOVE DC-BIN-DATE-2          TO WS-RETRO-ELIM-DATE.           EL156
02557                                                                      CL*95
02558      MOVE WS-HOLD-BIN-DATE-1     TO DC-BIN-DATE-1.                   CL*95
02559                                                                   EL156
02560      IF WS-RETRO-ELIM-DATE > WS-TODAY-DATE                           CL*94
02561          MOVE ER-0541            TO EMI-ERROR                        CL*27
02562          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*27
02563          MOVE -1                 TO EPYFROML                         CL*27
02564          MOVE AL-UNBON           TO EPYFROMA.                        CL*27
02565                                                                      CL*27
02566      IF (PI-COMPANY-ID = 'AIG' OR 'AUK')  AND                        CL*93
02567         (PI-BENEFIT-SAVE = '60E')                                    CL*94
02568         IF (CL-PAID-THRU-DT   = LOW-VALUES OR SPACES) OR             CL*93
02569            (CL-TOTAL-PAID-AMT = ZEROS)                               CL*93
02570              MOVE WS-HOLD-BIN-DATE-1  TO DC-BIN-DATE-1               CL*65
02571              MOVE 30                  TO DC-ELAPSED-DAYS             CL*93
02572              MOVE +0                  TO DC-ELAPSED-MONTHS           CL*65
02573              MOVE '6'                 TO DC-OPTION-CODE              CL*81
02574              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.          CL*65
02575                                                                      CL*65
02576      IF PI-BEN-TYPE = 'R'                                         EL156
02577          MOVE DC-BIN-DATE-1      TO PI-CPYFROM                       CL*27
02578      ELSE                                                         EL156
02579          MOVE DC-BIN-DATE-2      TO PI-CPYFROM                       CL*27
02580          IF WS-SV-EPYFROM = LOW-VALUES                               CL*51
02581             MOVE DC-BIN-DATE-2   TO WS-SV-EPYFROM.                   CL*51
02582                                                                      CL*65
02583      IF CL-PAID-THRU-DT = LOW-VALUES OR SPACES                       CL*93
02584          IF WS-USE-AIG-PAYFROM   OR                                  CL*65
02585             WS-AIG-RECALC-PAYFROM                                    CL*93
02586               MOVE PI-CPYFROM     TO  WS-SV-EPYFROM                  CL*65
02587                                       DC-BIN-DATE-1                  CL*65
02588               MOVE ' '            TO  DC-OPTION-CODE                 CL*65
02589               MOVE +0             TO  DC-ELAPSED-MONTHS              CL*65
02590                                       DC-ELAPSED-DAYS                CL*65
02591               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT          CL*65
02592               IF NO-CONVERSION-ERROR                                 CL*65
02593                   MOVE DC-GREG-DATE-1-MDY TO PI-EPYFROM.             CL*65
02594                                                                      CL*65
02595      IF PI-CPYFROM < CL-INCURRED-DT                                  CL*94
02596         MOVE -1                  TO EPYFROML                         CL*65
02597         MOVE ER-3542             TO EMI-ERROR                        CL*65
02598         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    CL*65
02599                                                                      CL*51
02600      IF CM-ENTRY-DT < CL-INCURRED-DT                                 CL*94
02601         MOVE CM-ENTRY-DT            TO DC-BIN-DATE-1                 CL*51
02602         MOVE CL-INCURRED-DT         TO DC-BIN-DATE-2                 CL*51
02603         MOVE +0                     TO DC-ELAPSED-DAYS               CL*51
02604         MOVE +0                     TO DC-ELAPSED-MONTHS             CL*51
02605         MOVE '1'                    TO DC-OPTION-CODE                CL*51
02606         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                CL*51
02607         ELSE                                                         CL*51
02608         MOVE ZEROS TO DC-ELAPSED-MONTHS.                             CL*51
02609                                                                      CL*51
02610      IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'DMD'                      CL*89
02611          NEXT SENTENCE                                               CL*65
02612      ELSE                                                            CL*65
02613          IF DC-ELAPSED-MONTHS < 2                                    CL*94
02614              MOVE ER-3255        TO EMI-ERROR                        CL*65
02615              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*65
02616                                                                   EL156
02617  1145-TEST-PAID-FROM.                                             EL156
02618      IF LOW-VALUES = WS-SV-EPYFROM OR PI-CPYFROM                  EL156
02619          GO TO 1150-EDIT-PAID-THRU.                               EL156
02620                                                                   EL156
02621      IF WS-SV-EPYFROM NOT = PI-CPYFROM                            EL156
02622          MOVE PI-CPYFROM         TO DC-BIN-DATE-1                    CL*35
02623          MOVE ' '                TO DC-OPTION-CODE                   CL*35
02624          MOVE +0                 TO DC-ELAPSED-MONTHS                CL*35
02625                                     DC-ELAPSED-DAYS                  CL*35
02626          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT               CL*35
02627          IF NO-CONVERSION-ERROR                                      CL*35
02628              MOVE DC-GREG-DATE-1-MDY TO EMI-DATE-FIELD               CL*35
02629          ELSE                                                        CL*35
02630              MOVE SPACES             TO EMI-DATE-FIELD.              CL*35
02631                                                                      CL*35
022718     IF PI-HOLDTIL NOT = ZEROS
022718        CONTINUE
022718     ELSE
02632      IF WS-SV-EPYFROM NOT = PI-CPYFROM                               CL*88
02633          IF CL-TOTAL-PAID-AMT = ZEROS                                CL*93
02634              MOVE ER-3543            TO EMI-ERROR                    CL*65
02635              MOVE WS-SV-EPYFROM      TO PI-CPYFROM                   CL*65
02636              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*65
02637              IF EMI-MESSAGE-FORMATTED                                CL*65
02638                 MOVE -1             TO EPYFROML                      CL*65
02639                 MOVE AL-UNBON       TO EPYFROMA                      CL*65
02640              ELSE                                                    CL*65
02641                 NEXT SENTENCE                                        CL*65
02642          ELSE                                                        CL*65
02643              MOVE ER-0452            TO EMI-ERROR                    CL*65
02644              MOVE WS-SV-EPYFROM      TO PI-CPYFROM                   CL*65
02645              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*65
02646              IF EMI-MESSAGE-FORMATTED                                CL*65
02647                 MOVE -1             TO EPYFROML                      CL*65
02648                 MOVE AL-UNBON       TO EPYFROMA.                     CL*65
02649                                                                   EL156
02650      EJECT                                                        EL156
02651  1150-EDIT-PAID-THRU.                                             EL156
02652                                                                      CL*16
02653      MOVE AL-UANON               TO EPYTHRUA.                     EL156
02654                                                                   EL156
02655      IF PI-EPYTHRU = ZEROS                                        EL156
050616*        IF PI-PMTTYPE = '4' OR '5' OR '6'                           CL*93
050616         IF PI-PMTTYPE = '5' OR '6'                                  CL*93
100518           OR (CL-CLAIM-TYPE NOT = 'L' AND 'O'
100518             AND PI-PMTTYPE = '4')
02657              GO TO 1200-EDIT-FIND-TOLERANCES                      EL156
02658          ELSE                                                     EL156
050616            IF (PI-PMTTYPE = '3') OR (PI-PMTTYPE = '4') OR        EL156
02660                (PI-PMTTYPE = '2' AND                              EL156
100518                 CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O')      EL156
02662                GO TO 1152-CHECK-TYPE                              EL156
02663             ELSE                                                  EL156
02664                MOVE ER-0473      TO EMI-ERROR                     EL156
02665                GO TO 1170-PAID-THRU-FATAL-ERROR                   EL156
02666      ELSE                                                         EL156
02667          IF PI-PMTTYPE = '5' OR '6'                                  CL*93
02668              MOVE ER-0474        TO EMI-ERROR                     EL156
02669              GO TO 1170-PAID-THRU-FATAL-ERROR.                    EL156
02670                                                                   EL156
02671      MOVE '4'                    TO DC-OPTION-CODE.               EL156
02672      MOVE PI-EPYTHRU             TO DC-GREG-DATE-1-MDY.           EL156
02673      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
02674                                                                   EL156
02675      IF DATE-CONVERSION-ERROR                                     EL156
02676          MOVE ER-0475            TO EMI-ERROR                     EL156
02677          GO TO 1170-PAID-THRU-FATAL-ERROR.                        EL156
02678                                                                   EL156
02679      MOVE DC-BIN-DATE-1       TO WS-SV-EPYTHRU.                      CL*16
02680                                                                   EL156
02681      IF PI-COMPANY-ID = 'DMD'                                        CL*88
02682          IF PI-PMTTYPE NOT = '2'                                     CL*93
02683                  AND                                                 CL*82
02684             WS-EXP-DT > WS-SV-EPYTHRU                                CL*94
02685              MOVE WS-SV-EPYTHRU    TO DC-BIN-DATE-1                  CL*93
02686              MOVE WS-EXP-DT        TO DC-BIN-DATE-2                  CL*93
02687              MOVE '1'              TO DC-OPTION-CODE                 CL*93
02688              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL*82
02689              IF DATE-CONVERSION-ERROR                                CL*88
02690                  MOVE ER-0944      TO EMI-ERROR                      CL*93
02691                  GO TO 1170-PAID-THRU-FATAL-ERROR                    CL*82
02692              ELSE                                                    CL*82
02693                  IF DC-ELAPSED-MONTHS = ZEROS AND                    CL*93
02694                     DC-ELAPSED-DAYS   = 1                            CL*93
02695                      MOVE ER-0945  TO EMI-ERROR                      CL*93
02696                      MOVE -1       TO EPYTHRUL                       CL*93
02697                      MOVE AL-UABON TO EPYTHRUA                       CL*93
02698                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.       CL*82
02699                                                                      CL*82
02700      IF PI-PMTTYPE = '4'                                             CL*88
02701          NEXT SENTENCE                                               CL*67
02702      ELSE                                                            CL*67
02703          IF WS-SV-EPYTHRU = WS-EXP-DT                                CL*88
02704              IF PI-PMTTYPE NOT = '2' AND '3'                         CL*93
02705                  MOVE ER-3541    TO EMI-ERROR                        CL*67
02706                  MOVE -1         TO PMTTYPEL                         CL*67
02707                  MOVE AL-UABON   TO PMTTYPEA                         CL*67
02708                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           CL*67
02709                                                                      CL*65
02710      IF WS-SV-EPYTHRU < WS-SV-EPYFROM                                CL*94
02711          MOVE ER-0501            TO EMI-ERROR                     EL156
02712          GO TO 1170-PAID-THRU-FATAL-ERROR.                        EL156
02713                                                                   EL156
02714      IF WS-SV-EPYTHRU > WS-TODAY-DATE                                CL*94
121802        IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122                                             OR 'B' or 'H'
121802*          IF PI-COMPANY-ID = 'AIG' OR 'AUK'                         CL*88
121802*              MOVE ER-0540         TO EMI-ERROR                     CL*65
121802*              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              CL*65
121802*              MOVE -1              TO EPYTHRUL                      CL*65
121802*              MOVE AL-UNBON        TO EPYTHRUA                      CL*65
121802*          ELSE                                                      CL*65
121802*          IF PI-COMPANY-ID = 'DMD'                                  CL*93
121802*              MOVE ER-7540         TO EMI-ERROR                     CL*93
121802*              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              CL*93
121802*              MOVE -1              TO EPYTHRUL                      CL*93
121802*              MOVE AL-UNBON        TO EPYTHRUA                      CL*93
121802*          ELSE                                                      CL*93
02728                IF PI-PMTTYPE NOT = '2' AND '3'                       CL*93
02729                   IF PI-COMPANY-ID = '???'                           CL*80
02730                       MOVE ER-7540  TO EMI-ERROR                     CL*80
02731                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT       CL*80
02732                       MOVE -1       TO EPYTHRUL                      CL*80
02733                       MOVE AL-UNBON TO EPYTHRUA                      CL*80
02734                   ELSE                                               CL*80
02735                       MOVE ER-0540  TO EMI-ERROR                     CL*80
02736                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT       CL*80
02737                       MOVE -1       TO EPYTHRUL                      CL*80
02738                       MOVE AL-UNBON TO EPYTHRUA.                     CL*80
02739                                                                   EL156
043019     MOVE 'N' TO PI-DUPE-APPROVAL-NEEDED
02740      IF ((WS-SV-EPYTHRU < CL-PAID-THRU-DT)                           CL*94
02741             OR                                                       CL*44
02742         (WS-SV-EPYTHRU NOT = LOW-VALUES AND                          CL*93
02743          WS-SV-EPYTHRU < CL-PAID-THRU-DT)                            CL*94
02744             OR                                                       CL*44
02745         (PI-CPYFROM NOT = LOW-VALUES AND                             CL*93
02746          PI-CPYFROM < CL-PAID-THRU-DT))                              CL*94
043019       AND NOT PI-PMTTYPE = '4'
02747         PERFORM 1166-CHECK-ALL-TRLRS THRU 1166-TRLR-EXIT
040819     ELSE
040819     IF WS-SV-EPYTHRU NOT = LOW-VALUES
040819       AND WS-SV-EPYFROM NOT = LOW-VALUES
040819        SET CHECK-ALL-PMTS TO TRUE
040819        PERFORM 1166-CHECK-ALL-TRLRS THRU 1166-TRLR-EXIT.
040819
040819     IF PI-HOLDTIL = ZEROS
040819       AND UNPAID-HOLD-UNTIL-EXISTS
040819       AND NOT DUPE-VOID-PMT-EXISTS
040819        MOVE ER-3276    TO EMI-ERROR
040819        MOVE -1         TO HOLDTILL
040819        MOVE AL-UABON   TO HOLDTILA
040819        GO TO 1170-PAID-THRU-FATAL-ERROR
040819     END-IF.
02748                                                                   EL156
02749  1152-CHECK-TYPE.                                                 EL156
121802     IF (CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122                                           OR 'B' or 'H')
02751         OR (PI-PMTTYPE = '5' OR '6')
02752          GO TO 1160-EDIT-PAID-THRU-AH.                            EL156
02753                                                                   EL156
02754  1155-EDIT-PAID-THRU-LIFE.                                        EL156
02755      MOVE CM-LF-BENEFIT-CD       TO WS-BEN-CD.                    EL156
02756      MOVE WS-ACCESS              TO CNTL-ACCESS.                  EL156
02757      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
02758      MOVE '4'                    TO CNTL-REC-TYPE.                EL156
02759      MOVE ZEROS                  TO CNTL-SEQ-NO.                  EL156
02760      MOVE 'BENE'                 TO FILE-SWITCH.                  EL156
02761                                                                   EL156
02762      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.                    EL156
02763                                                                   EL156
02764      IF NO-BENEFIT-FOUND                                          EL156
02765          GO TO 1950-NOT-FOUND.                                    EL156
02766                                                                   EL156
02767      MOVE CF-SPECIAL-CALC-CD (SUB-1) TO WS-SPECIAL-CALC-CD.       EL156
02768                                                                   EL156
02769      MOVE WS-EXP-DT              TO PI-CPYTHRU.                   EL156
02770                                                                      CL*25
02771      IF PI-LIFE-OVERRIDE-L1 = 'P' OR                                 CL*88
02772         PI-LF-COVERAGE-TYPE = 'P'                                    CL*88
02773          MOVE CL-INCURRED-DT     TO  PI-CPYTHRU                      CL*25
02774          GO TO 1200-EDIT-FIND-TOLERANCES.                            CL*25
02775                                                                   EL156
02776      IF PI-EPYTHRU = ZEROS                                        EL156
02777         MOVE PI-CPYTHRU          TO WS-SV-EPYTHRU.                EL156
02778                                                                   EL156
02779      IF WS-SV-EPYTHRU NOT = WS-EXP-DT                             EL156
02780          MOVE ER-0476            TO EMI-ERROR                     EL156
02781          PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.             EL156
02782                                                                   EL156
02783      MOVE CM-CERT-EFF-DT        TO DC-BIN-DATE-1.                    CL*43
02784 *    MOVE CM-LF-CRITICAL-PERIOD TO DC-ELAPSED-MONTHS.             EL156
           move cl-critical-period    to dc-elapsed-months
02785      MOVE +0                    TO DC-ELAPSED-DAYS.                  CL*43
02786      MOVE '6'                   TO DC-OPTION-CODE.                   CL*43
02787      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
02788                                                                   EL156
02789      IF (NO-CONVERSION-ERROR AND                                  EL156
02790          WS-SPECIAL-CALC-CD = 'C')                                EL156
02791         IF CL-INCURRED-DT > DC-BIN-DATE-2                            CL*94
02792             MOVE ER-0657 TO EMI-ERROR                             EL156
02793             PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.          EL156
02794                                                                   EL156
02795      IF PI-COMPANY-ID = 'NCL'                                        CL*88
02796          IF WS-SPECIAL-CALC-CD = 'C' AND                             CL*93
                  cl-critical-period = zeros
02797 *           CM-LF-CRITICAL-PERIOD = +0                               CL*93
02798              MOVE ER-0844       TO EMI-ERROR                         CL*80
02799              PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.            CL*80
02800                                                                      CL*80
02801      GO TO 1200-EDIT-FIND-TOLERANCES.                             EL156
02802                                                                   EL156
02803  1160-EDIT-PAID-THRU-AH.                                          EL156
02804      IF PI-EPYTHRU = ZEROS                                        EL156
02805         MOVE WS-EXP-DT           TO PI-CPYTHRU                    EL156
02806                                     WS-SV-EPYTHRU                 EL156
02807      ELSE                                                         EL156
02808         MOVE WS-SV-EPYTHRU       TO PI-CPYTHRU.                   EL156
02809                                                                   EL156
02810      IF WS-SV-EPYTHRU < PI-CPYFROM                                   CL*94
02811          MOVE ER-0508            TO EMI-ERROR                     EL156
02812          GO TO 1170-PAID-THRU-FATAL-ERROR.                        EL156
02813                                                                   EL156
           perform 7975-read-acct      thru 7975-exit

           move zeros                  to pi-max-ext
           if PI-DCC-PRODUCT-CODE NOT = SPACES
              PERFORM 0900-GET-DDF-LIMITS
                                       THRU 0900-EXIT
              IF PDEF-FOUND
                 PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122              (P1 > +11)
022122              OR ((PD-PROD-CODE (P1) = CL-CLAIM-TYPE)
022122              AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
                 END-PERFORM
022122           IF P1 < +12
                    MOVE PD-MAX-extension (P1)
                                       TO pi-max-ext
                 else
                    move er-1674       to emi-error
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
              else
                 move er-1672          to emi-error
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           end-if

           move spaces                 to pi-max-ext-used
02814      IF PI-PMTTYPE = '3'                                          EL156
02815         MOVE WS-EXP-DT           TO PI-CPYTHRU                    EL156
02816         IF WS-SV-EPYTHRU NOT = WS-EXP-DT
02817            MOVE ER-0477          TO EMI-ERROR
02818            PERFORM 1165-PAID-THRU-ERROR
                                       THRU 1165-EXIT
02819         ELSE
02820            continue
              end-if
02821      ELSE
02822         IF WS-SV-EPYTHRU > WS-EXP-DT
081817         IF PI-DCC-PRODUCT-CODE NOT > SPACES
                 move ws-exp-dt        to dc-bin-date-1
                 move ws-sv-epythru    to dc-bin-date-2
                 MOVE '1'              TO DC-OPTION-CODE
                 MOVE +0               TO DC-ELAPSED-DAYS
                                          DC-ELAPSED-MONTHS
                 PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
                 if no-conversion-error
                    if dc-elapsed-days > zeros
                       add +1 to dc-elapsed-months
                    end-if
                    if dc-elapsed-months > pi-max-ext
                       MOVE ER-0557    TO EMI-ERROR
                       PERFORM 1165-PAID-THRU-ERROR
                                       THRU 1165-EXIT
                    else
                       move 'Y'        to pi-max-ext-used
                       move er-1670    to emi-error
                       perform 9900-error-format
                                       thru 9900-exit
                    end-if
                 end-if
081817         ELSE
081817            MOVE ER-0557    TO EMI-ERROR
081817            PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT
081817         END-IF
              end-if
           end-if
02825                                                                   EL156
02826      IF (WS-RETRO-ELIM-DATE > WS-SV-EPYTHRU) AND                     CL*94
02827          PI-PMTTYPE NOT = '3'                                        CL*20
02828          MOVE ER-0541        TO EMI-ERROR                            CL*20
02829          PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.                CL*20
02830                                                                   EL156
02831      IF (CM-AH-CANCEL-DT NOT = LOW-VALUES) AND                    EL156
02832         (PI-PMTTYPE NOT = '3')                                    EL156
02833          IF WS-SV-EPYTHRU > CM-AH-CANCEL-DT                          CL*94
02834              MOVE ER-0478        TO EMI-ERROR                     EL156
02835              PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.         EL156
02836                                                                   EL156
02837      IF (CM-AH-SETTLEMENT-DT NOT = LOW-VALUES) AND                EL156
02838         (PI-PMTTYPE NOT = '4')                                    EL156
02839          IF WS-SV-EPYTHRU > CM-AH-SETTLEMENT-DT                      CL*94
02840              MOVE ER-0479        TO EMI-ERROR                     EL156
02841              PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.         EL156
02842                                                                   EL156
02843      IF (CL-EST-END-OF-DISAB-DT NOT = LOW-VALUES) AND             EL156
02844          WS-SV-EPYTHRU > CL-EST-END-OF-DISAB-DT                      CL*94
02845              MOVE ER-0545        TO EMI-ERROR                     EL156
02846              PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.         EL156
02847                                                                   EL156
02848      IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES                      EL156
02849          IF WS-SV-EPYTHRU > CM-LF-DEATH-EXIT-DT                      CL*94
02850              MOVE ER-0481        TO EMI-ERROR                     EL156
02851              PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.            CL*80
02852                                                                      CL*80
121802*    IF PI-COMPANY-ID = 'NCL'                                        CL*88
121802*        IF WS-SPECIAL-CALC-CD = 'C' AND                             CL*93
121802*           CM-AH-CRITICAL-PERIOD = +0                               CL*93
121802*            MOVE ER-0844        TO  EMI-ERROR                       CL*80
121802*            PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.         EL156
02858                                                                   EL156
02859 *    IF (CM-AH-CRITICAL-PERIOD = ZEROS) OR                           CL*93
           if (cl-critical-period = zeros)
02860         or (WS-SPECIAL-CALC-CD NOT = 'C')
02861           GO TO 1200-EDIT-FIND-TOLERANCES.                           CL*65
02862                                                                      CL*65
02863      MOVE PI-CPYFROM TO DC-BIN-DATE-1.                            EL156
02864      MOVE PI-CPYTHRU TO DC-BIN-DATE-2.                            EL156
02865      MOVE '1'        TO DC-OPTION-CODE.                              CL*43
02866      MOVE +0         TO DC-ELAPSED-DAYS                              CL*65
02867                         DC-ELAPSED-MONTHS.                           CL*65
02868      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
02869                                                                   EL156
092310*    IF NO-CONVERSION-ERROR                                          CL*65
092310*       COMPUTE ws-benefits-paid =                                CL*65
092310*           (DC-ELAPSED-DAYS + CL-NO-OF-DAYS-PAID) / 30.5            CL*65
092310*       IF ws-benefits-paid > CM-AH-CRITICAL-PERIOD               CL*94
092310*           MOVE ER-0657 TO EMI-ERROR                             EL156
092310*           PERFORM 1165-PAID-THRU-ERROR THRU 1165-EXIT.          EL156
02876
02877      GO TO 1200-EDIT-FIND-TOLERANCES.                             EL156
02878                                                                   EL156
02879  1165-PAID-THRU-ERROR.                                            EL156
02880      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
02881                                                                   EL156
02882      IF EMI-MESSAGE-FORMATTED                                     EL156
02883          MOVE -1                 TO EPYTHRUL                      EL156
02884          MOVE AL-UNBON           TO EPYTHRUA.                     EL156
02885                                                                   EL156
02886  1165-EXIT.                                                       EL156
02887      EXIT.                                                        EL156
02888                                                                   EL156
02889  1166-CHECK-ALL-TRLRS.                                               CL*40
02890                                                                      CL*39
02891      IF PI-PMTTYPE = '4'                                             CL*93
043019       AND NOT HOLD-UNTIL-CHK
043019       AND NOT CHECK-ALL-PMTS
02892         GO TO 1166-TRLR-EXIT.                                        CL*52
02893                                                                      CL*52
02894      MOVE ' ' TO WS-BROWSE-TRLR-SW.                                  CL*39
02895                                                                      CL*39
022718     IF HOLD-UNTIL-CHK
022718        CONTINUE
022718     ELSE
02896      IF WS-SV-EPYTHRU = LOW-VALUES  AND                              CL*93
02897         WS-SV-EPYFROM = LOW-VALUES                                   CL*93
02898           GO TO 1166-END-CHECK.                                      CL*93
02899                                                                      CL*39
02900      EXEC CICS HANDLE CONDITION                                      CL*39
02901          ENDFILE   (1166-END-CHECK)                                  CL*40
02902          NOTFND    (1166-END-CHECK)                                  CL*40
02903      END-EXEC.                                                       CL*88
02904                                                                      CL*39
02905      MOVE ELMSTR-KEY             TO ELTRLR-KEY.                      CL*39
02906      MOVE +100                   TO TRLR-SEQ-NO.                     CL*39
02907                                                                      CL*39
02908      EXEC CICS STARTBR                                               CL*39
02909           DATASET    ('ELTRLR')                                      CL*39
02910           RIDFLD     (ELTRLR-KEY)                                    CL*39
02911           GTEQ                                                       CL*39
02912      END-EXEC.                                                       CL*39
02913                                                                      CL*39
02914      MOVE 'Y' TO WS-BROWSE-TRLR-SW.                                  CL*39
02915                                                                      CL*39
02916  1166-READ-TRLR-NEXT.                                                CL*40
02917                                                                      CL*39
02918      EXEC CICS READNEXT                                              CL*39
02919           DATASET    ('ELTRLR')                                      CL*39
02920           RIDFLD     (ELTRLR-KEY)                                    CL*39
02921           SET        (ADDRESS OF ACTIVITY-TRAILERS)                  CL*81
02922      END-EXEC.                                                       CL*39
02923                                                                      CL*39
02924      IF TRLR-COMP-CD  = MSTR-COMP-CD  AND                            CL*93
02925         TRLR-CARRIER  = MSTR-CARRIER  AND                            CL*93
02926         TRLR-CLAIM-NO = MSTR-CLAIM-NO AND                            CL*93
02927         TRLR-CERT-NO  = MSTR-CERT-NO                                 CL*93
02928           NEXT SENTENCE                                              CL*93
02929       ELSE                                                           CL*93
02930           GO TO 1166-END-CHECK.                                      CL*93
02931                                                                      CL*39
02932 * **  BYPASS ALL NON-PAYMENT TRAILERS                                CL*39
02933                                                                      CL*39
02934      IF AT-TRAILER-TYPE = '2'                                        CL*93
02935         NEXT SENTENCE                                                CL*39
02936      ELSE                                                            CL*39
02937         GO TO 1166-READ-TRLR-NEXT.                                   CL*40
02938                                                                      CL*39
02939 * **  BYPASS ALL NON-PARTIAL PAYMENTS                                CL*39
02940                                                                      CL*39
043019     IF (HOLD-UNTIL-CHK
043019       OR CHECK-ALL-PMTS)
043019       AND AT-PAYMENT-TYPE <= '4'
043019        NEXT SENTENCE                                                CL*40
043019     ELSE
02941      IF AT-PAYMENT-TYPE = '1' OR '2'                                 CL*93
02942         NEXT SENTENCE                                                CL*39
02943      ELSE                                                            CL*39
02944         GO TO 1166-READ-TRLR-NEXT.                                   CL*40
02945                                                                      CL*40
02951      IF ((AT-PAID-FROM-DT = LOW-VALUES OR SPACES) OR                 CL*93
02952         (AT-PAID-THRU-DT = LOW-VALUES OR SPACES))                    CL*93
043019        AND NOT HOLD-UNTIL-CHK
02953           GO TO 1166-READ-TRLR-NEXT.                                 CL*93

02946 * **  BYPASS ALL VOID PAYMENTS                                       CL*40
02947                                                                      CL*40
02948      IF AT-VOID-DT NOT = LOW-VALUES                                  CL*93
040819        IF WS-SV-EPYTHRU = AT-PAID-THRU-DT
040819          AND WS-SV-EPYFROM = AT-PAID-FROM-DT
040819            SET DUPE-VOID-PMT-EXISTS TO TRUE
040819        END-IF
02949         GO TO 1166-READ-TRLR-NEXT.                                   CL*40
02950                                                                      CL*39
022718     IF HOLD-UNTIL-CHK
022718        IF AT-PAID-THRU-DT > WS-HOLD-UNTIL-DT
022718          AND AT-TO-BE-WRITTEN-DT > ZERO
022718           MOVE AT-PAID-THRU-DT TO WS-HOLD-UNTIL-DT
022718        END-IF
040819        IF AT-TO-BE-WRITTEN-DT > ZERO
040819          AND AT-CHECK-WRITTEN-DT = LOW-VALUES
040819           SET UNPAID-HOLD-UNTIL-EXISTS TO TRUE
040819           IF AT-TO-BE-WRITTEN-DT > WS-UNPD-HOLD-UNTIL-DT
040819              MOVE AT-TO-BE-WRITTEN-DT TO WS-UNPD-HOLD-UNTIL-DT
040819           END-IF
040819        END-IF
040819        IF AT-AMOUNT-PAID NUMERIC
040819          AND (AT-CHECK-WRITTEN-DT > LOW-VALUES
040819             OR AT-TO-BE-WRITTEN-DT < ZERO)
040819          AND AT-PAYMENT-TYPE  <= '4'
040819           ADD AT-AMOUNT-PAID TO WS-TOT-AMOUNT-PAID
022718        END-IF
022718     END-IF
02954                                                                      CL*39
022718     IF HOLD-UNTIL-CHK
022718        CONTINUE
022718     ELSE
02955      IF ((WS-SV-EPYTHRU < AT-PAID-FROM-DT) AND                       CL*94
02956          (WS-SV-EPYFROM < AT-PAID-FROM-DT))                          CL*94
02957             OR                                                       CL*39
02958         ((WS-SV-EPYTHRU > AT-PAID-THRU-DT) AND                       CL*94
02959          (WS-SV-EPYFROM > AT-PAID-THRU-DT))                          CL*94
02960         NEXT SENTENCE                                                CL*39
02961      ELSE                                                            CL*39
040819         IF WS-SV-EPYTHRU = AT-PAID-THRU-DT
040819          AND WS-SV-EPYFROM = AT-PAID-FROM-DT
043019          AND (PI-OFFLINE NOT EQUAL 'Y')
043019          AND (CL-CLAIM-TYPE NOT = 'L' AND 'O')
043019            IF PI-PMTTYPE = '4'
043019               MOVE 'Y' TO PI-DUPE-APPROVAL-NEEDED
043019               MOVE ER-3280            TO EMI-ERROR
043019               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
043019               MOVE -1                 TO EPYTHRUL
043019               MOVE AL-UNBON           TO EPYTHRUA
043019            ELSE
040819               MOVE ER-3279            TO EMI-ERROR                  CL*65
040819               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              CL*65
040819               MOVE -1                 TO EPYTHRUL                   CL*65
040819               MOVE AL-UNBON           TO EPYTHRUA                   CL*65
040819         ELSE
02962          IF CL-TOTAL-PAID-AMT > ZEROS
040819           AND NOT CHECK-ALL-PMTS
02963              MOVE ER-0502            TO EMI-ERROR                    CL*65
02964              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*65
02965              MOVE -1                 TO EPYTHRUL                     CL*65
02966              MOVE AL-UNBON           TO EPYTHRUA.                    CL*65
02967                                                                      CL*39
02968      GO TO 1166-READ-TRLR-NEXT.                                      CL*40
02969                                                                      CL*39
02970  1166-END-CHECK.                                                     CL*40
02971                                                                      CL*39
02972      IF WS-BROWSE-TRLR-SW = 'Y'                                      CL*93
02973         EXEC CICS ENDBR                                              CL*39
02974              DATASET   ('ELTRLR')                                    CL*39
02975         END-EXEC.                                                    CL*39
02976                                                                      CL*39
02977      MOVE 'Y' TO WS-BROWSE-TRLR-SW.                                  CL*39
02978                                                                      CL*39
02979  1166-TRLR-EXIT.                                                     CL*40
02980      EXIT.                                                           CL*39
02981                                                                      CL*39
02982      EJECT                                                           CL*39
02983  1170-PAID-THRU-FATAL-ERROR.                                      EL156
02984      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
02985                                                                   EL156
02986      MOVE -1                     TO EPYTHRUL.                     EL156
02987      MOVE AL-UNBON               TO EPYTHRUA.                     EL156
02988                                                                   EL156
02989  1200-EDIT-FIND-TOLERANCES.                                       EL156
02990      EXEC CICS HANDLE CONDITION                                   EL156
02991          ENDFILE(1950-NOT-FOUND)                                  EL156
02992          NOTFND (1950-NOT-FOUND)                                     CL*43
02993      END-EXEC.                                                       CL*88
02994                                                                   EL156
02995      IF PI-PROCESSOR-ID = 'LGXX'                                     CL*71
02996         GO TO 1240-FIND-ACCT-TOLERANCES.                          EL156
02997                                                                   EL156
02998      MOVE 'N'                    TO FOUND-TOL-SW.                 EL156
02999      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
03000      MOVE '2'                    TO CNTL-REC-TYPE.                EL156
03001      MOVE PI-PROCESSOR-ID        TO CNTL-ACCESS.                  EL156
03002      MOVE +0                     TO CNTL-SEQ-NO.                  EL156
03003      MOVE 'PROC'                 TO FILE-SWITCH.                  EL156
03004                                                                   EL156
03005      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.                    EL156
03006                                                                   EL156
03007      MOVE CF-PROC-CALC-AMT-TOL   TO WS-PMT-TOL.                   EL156
03008      MOVE CF-PROC-MAX-REG-PMT    TO WS-MAX-PMT-TOL                   CL*47
03009                                     PI-MAX-PMT-TOL.                  CL*47
03010      MOVE CF-PROC-MAX-REG-DAYS   TO WS-MAX-DAYS-TOL.              EL156
03011                                                                      CL*71
03012      IF PI-COMPANY-ID = 'HER' OR 'KSM'                               CL*88
03013          MOVE CF-PROC-MAX-REG-PMT   TO  WS-HER-MO-PMT-AMT.           CL*76
03014                                                                      CL*75
03015      MOVE CF-PROC-CALC-DAYS-TOL  TO WS-DAYS-TOL.                  EL156
03016      MOVE CF-PROC-MAX-LF-PMT     TO WS-MAX-LF-PMT-TOL                CL*47
03017                                     PI-MAX-LF-PMT-TOL.               CL*47
03018                                                                   EL156
03019      IF CF-PROC-MAX-EXP-PMT NOT NUMERIC                              CL*64
03020          MOVE ZEROS              TO CF-PROC-MAX-EXP-PMT.             CL*64
03021                                                                      CL*64
03022      MOVE CF-PROC-MAX-EXP-PMT    TO WS-MAX-EXP-PMT                   CL*64
03023                                     PI-MAX-EXP-PMT.                  CL*64
03024                                                                      CL*64
03025  1240-FIND-ACCT-TOLERANCES.                                       EL156
03026                                                                      CL*36
03027      IF WS-PMT-TOL NOT = +0                                       EL156
03028          MOVE 'Y'                TO FOUND-TOL-SW.                 EL156
03029                                                                   EL156
03030      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*88
03031          MOVE PI-COMPANY-CD          TO ACCT-COMP-CD                 CL*65
03032          MOVE CL-CURRENT-CARRIER     TO ACCT-CARRIER                 CL*65
03033          MOVE CL-CURRENT-GROUPING    TO ACCT-GROUPING                CL*65
03034          MOVE CL-CURRENT-STATE       TO ACCT-STATE                   CL*65
03035          MOVE CL-CURRENT-ACCOUNT     TO ACCT-ACCOUNT                 CL*65
03036          MOVE CL-CERT-EFF-DT         TO ACCT-EXP-DT                  CL*65
03037      ELSE                                                            CL*65
03038          MOVE PI-COMPANY-CD          TO ACCT-COMP-CD                 CL*65
03039          MOVE PI-CARRIER             TO ACCT-CARRIER                 CL*65
03040          MOVE PI-GROUPING            TO ACCT-GROUPING                CL*65
03041          MOVE PI-STATE               TO ACCT-STATE                   CL*65
03042          MOVE PI-ACCOUNT             TO ACCT-ACCOUNT                 CL*65
03043          MOVE PI-CERT-EFF-DT         TO ACCT-EXP-DT.                 CL*65
03044                                                                      CL*65
03045      MOVE 'ACCT'                 TO FILE-SWITCH.                     CL*43
03046                                                                      CL*36
03047      MOVE ERACCT-PARTIAL-KEY     TO WS-ERACCT-SAVE-KEY.              CL*43
03048      MOVE SPACES                 TO WS-ERACCT-HOLD-RECORD.           CL*43
03049                                                                      CL*36
03050      EXEC CICS HANDLE CONDITION                                      CL*36
03051           NOTFND   (1950-NOT-FOUND)                                  CL*36
03052           ENDFILE  (1950-NOT-FOUND)                                  CL*36
03053      END-EXEC.                                                       CL*36
03054                                                                   EL156
090821     move spaces to ws-eracct-startbr-ind
03055      EXEC CICS STARTBR                                            EL156
03056          DATASET('ERACCT')                                        EL156
03057          RIDFLD (ERACCT-KEY)                                         CL*93
03058          GTEQ                                                     EL156
03059      END-EXEC.                                                       CL*88

090821     set eracct-browse-started to true

           .
03061  1245-FIND-ACCT-TOLERANCES-LOOP.                                  EL156
03062                                                                      CL*36
03063      EXEC CICS READNEXT                                           EL156
03064          DATASET('ERACCT')                                        EL156
03065          SET    (ADDRESS OF ACCOUNT-MASTER)                          CL*81
03066          RIDFLD (ERACCT-KEY)                                      EL156
03067      END-EXEC.                                                       CL*88
03068                                                                   EL156
03069      IF WS-ERACCT-SAVE-KEY NOT = ERACCT-PARTIAL-KEY                  CL*93
03070         IF WS-ERACCT-HOLD-RECORD = SPACES                            CL*93
03071            GO TO 1950-NOT-FOUND                                      CL*36
03072         ELSE                                                         CL*36
03073            MOVE WS-ERACCT-HOLD-RECORD TO ACCOUNT-MASTER              CL*36
03074            GO TO 1250-CONTINUE.                                      CL*36
03075                                                                   EL156
03076      IF ACCT-EXP-DT = HIGH-VALUES                                    CL*93
03077         NEXT SENTENCE                                                CL*36
03078      ELSE                                                            CL*36
03079         MOVE ACCOUNT-MASTER TO WS-ERACCT-HOLD-RECORD                 CL*36
03080         GO TO 1245-FIND-ACCT-TOLERANCES-LOOP.                        CL*36
03081                                                                   EL156
03082  1250-CONTINUE.                                                      CL*36

090821     if eracct-browse-started
090821        exec cics endbr
090821           dataset('ERACCT')
090821        end-exec
090821        move spaces to ws-eracct-startbr-ind
090821     end-if

03084      MOVE AM-CONTROL-PRIMARY     TO PI-ACCT-KEY.                     CL*36
03085                                                                   EL156
03086      IF FOUND-TOL-SW = 'N'                                        EL156
03087          MOVE AM-TOL-CLM         TO WS-PMT-TOL.                   EL156
03088                                                                   EL156
03089 ********* 1260-FIND-STATE-TOLERANCES.                             EL156
03090                                                                   EL156
03091      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
03092      MOVE PI-STATE               TO WS-ST-ACCESS.                 EL156
03093      MOVE WS-STATE-ACCESS        TO CNTL-ACCESS.                  EL156
03094      MOVE '3'                    TO CNTL-REC-TYPE.                EL156
03095      MOVE +0                     TO CNTL-SEQ-NO.                  EL156
03096      MOVE 'STAT'                 TO FILE-SWITCH.                  EL156
03097                                                                   EL156
03098      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.                    EL156
03099                                                                   EL156
03100      IF WS-PMT-TOL = +0                                           EL156
03101          MOVE CF-ST-TOL-CLAIM    TO WS-PMT-TOL.                   EL156
03102                                                                   EL156
03103      MOVE CF-ST-CLAIM-REJECT-SW  TO WS-TOL-SEVERITY.              EL156
03104      MOVE CF-ST-SPLIT-PAYMENT    TO PI-SPLIT-PMT-SW.                 CL*65
03105      MOVE CF-STATE-ABBREVIATION  TO WS-STATE-ABBREV.                 CL*80
03106      MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.                    CL*96
061511     MOVE CF-ST-VFY-2ND-BENE     TO PI-VFY-2ND-BENE.
061511
032514****FOR ANY PAYMENT NOT MADE TO BENEFICIARY, CHECK FOR VERIFY IND
032514****AND FOR LIFE PAYMENTS CHECK PAYEE NAME NOT EQUAL TO DECEASED
032514     IF PI-PAYEE-TYPE <> 'B'
061511           PERFORM 3600-CHECK-VERIFICATION THRU 3600-EXIT
061511     END-IF.
03107                                                                      CL*65
03108      IF PI-PMTTYPE      = '2'  AND                                   CL*93
03109         PI-SPLIT-PMT-SW = 'Y'  AND                                   CL*93
100518        (CL-CLAIM-TYPE   = PI-LIFE-OVERRIDE-L1 OR 'O')               CL*93
03111           MOVE ER-3530                TO EMI-ERROR                   CL*65
03112           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                  CL*65
03113                                                                   EL156
03114 ********* 1280-FIND-BENEFIT-TOLERANCES.                           EL156
03115                                                                   EL156
121802     IF (CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122         OR 'B' OR 'H')
03118         OR (PI-PMTTYPE = '5' OR '6')
03119          GO TO 1300-FIND-CARR-TOLERANCES.                         EL156
03120                                                                   EL156
03121      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
03122      MOVE '4'                    TO CNTL-REC-TYPE.                EL156
03123      MOVE +0                     TO CNTL-SEQ-NO.                  EL156
03124      MOVE CM-LF-BENEFIT-CD       TO WS-BEN-CD.                    EL156
03125      MOVE WS-ACCESS              TO CNTL-ACCESS.                  EL156
03126      MOVE 'BENE'                 TO FILE-SWITCH.                  EL156
03127                                                                   EL156
03128      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.                    EL156
03129                                                                   EL156
03130      IF NO-BENEFIT-FOUND                                          EL156
03131          GO TO 1950-NOT-FOUND.                                    EL156
03132                                                                   EL156
03133      MOVE CF-CO-EARNINGS-CALC (SUB-1)    TO  WS-EARNING-METHOD.      CL*80
03134      MOVE CF-SPECIAL-CALC-CD  (SUB-1)    TO  WS-SPECIAL-CALC-CD.     CL*80
03135      MOVE CF-LF-COVERAGE-TYPE (SUB-1)    TO  WS-LF-COVERAGE-TYPE.    CL*80
03136                                                                   EL156
03137      EXEC CICS HANDLE CONDITION                                   EL156
03138          NOTFND(1950-NOT-FOUND)                                   EL156
03139      END-EXEC.                                                       CL*88
03140                                                                   EL156
03141  1300-FIND-CARR-TOLERANCES.                                       EL156
03142      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
03143      MOVE '6'                    TO CNTL-REC-TYPE.                EL156
03144                                                                   EL156
03145      IF CONTROL-IS-ACTUAL-CARRIER                                 EL156
03146          MOVE PI-CARRIER               TO WS-CARR                    CL*93
03147      ELSE                                                         EL156
03148          MOVE PI-CARRIER-CONTROL-LEVEL TO WS-CARR.                EL156
03149                                                                   EL156
03150      MOVE WS-CARR-ACCESS         TO CNTL-ACCESS.                  EL156
03151      MOVE +0                     TO CNTL-SEQ-NO.                  EL156
03152      MOVE 'CARR'                 TO FILE-SWITCH.                  EL156
03153                                                                   EL156
03154      PERFORM 7930-READ-CONTROL   THRU 7930-EXIT.                  EL156
03155                                                                   EL156
03156      IF WS-DAYS-TOL = +0                                          EL156
03157          MOVE CF-CALC-DAYS-TOL   TO WS-DAYS-TOL.                  EL156
03158                                                                   EL156
03159      IF WS-PMT-TOL = +0                                           EL156
03160          MOVE CF-CALC-AMT-TOL    TO WS-PMT-TOL.                   EL156
03161                                                                   EL156
03162      IF WS-MAX-DAYS-TOL = +0                                      EL156
03163          MOVE CF-MAX-REG-DAYS    TO WS-MAX-DAYS-TOL.              EL156
03164                                                                   EL156
03165      IF WS-MAX-PMT-TOL = +0                                          CL*93
03166          MOVE CF-MAX-REG-PMT     TO WS-MAX-PMT-TOL                   CL*47
03167                                     PI-MAX-PMT-TOL.                  CL*47
03168                                                                      CL*10
03169      IF WS-MAX-LF-PMT-TOL = +0                                       CL*93
03170         MOVE CF-MAX-REG-PMT      TO WS-MAX-LF-PMT-TOL                CL*47
03171                                     PI-MAX-LF-PMT-TOL.               CL*47
03172                                                                   EL156
03173      MOVE CF-CLAIM-CALC-METHOD   TO WS-CALC-METHOD.               EL156
03174                                                                   EL156
03175 ********* 1320-FIND-COMP-TOLERANCES.                              EL156
03176                                                                   EL156
03177      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
03178      MOVE '1'                    TO CNTL-REC-TYPE.                EL156
03179      MOVE SPACES                 TO CNTL-ACCESS.                  EL156
03180      MOVE +0                     TO CNTL-SEQ-NO.                  EL156
03181      MOVE 'COMP'                 TO FILE-SWITCH.                  EL156
03182                                                                   EL156
03183      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.                    EL156
03184                                                                   EL156
03185      MOVE CF-CURRENT-MONTH-END   TO PI-MONTH-END-SAVE.            EL156
03186      MOVE CF-PAYMENT-APPROVAL-SW TO PI-PMT-APPR-SW.               EL156
03187                                                                   EL156
03188      IF WS-PMT-TOL NOT = +0                                          CL*93
03189         IF WS-TOL-SEVERITY NOT = ' '                                 CL*93
03190          GO TO 1360-EDIT-DAYS.                                    EL156
03191                                                                   EL156
03192      IF WS-PMT-TOL = +0                                           EL156
03193          MOVE CF-CO-TOL-CLAIM    TO WS-PMT-TOL.                   EL156
03194                                                                   EL156
03195      IF WS-TOL-SEVERITY = ' '                                     EL156
03196          MOVE CF-CO-CLAIM-REJECT-SW TO WS-TOL-SEVERITY.           EL156
03197                                                                   EL156
03198  1360-EDIT-DAYS.                                                  EL156
03199      IF PI-PMTTYPE NOT = '4'                                         CL*93
03200         IF PI-EDAYS NEGATIVE OR                                      CL*93
03201            PI-EPYAMT NEGATIVE                                        CL*94
03202           MOVE ER-0513             TO EMI-ERROR                      CL*93
03203           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                   CL*93
03204           MOVE -1                  TO PMTTYPEL.                      CL*93
03205                                                                   EL156
03206      IF PI-PMTTYPE = '5' OR '6'                                      CL*43
03207         GO TO 1380-EDIT-EXPENSE-PMT-AMT.                             CL*64
03208                                                                      CL*65
03209      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
03210         IF PI-PMTTYPE = '4'                                          CL*93
03211           MOVE PI-EDAYS          TO PI-CDAYS.                        CL*65
03212                                                                   EL156
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                EL156
03214          GO TO 1440-COMPUTE-LIFE-REMAINING.                       EL156
03215                                                                   EL156
03216      IF PI-PMTTYPE = '4'                                             CL*43
03217         MOVE PI-EDAYS            TO PI-CDAYS                      EL156
03218         GO TO 1400-EDIT-PAYMENT.                                  EL156
03219                                                                   EL156
03220      IF LOW-VALUES = PI-CPYFROM OR PI-CPYTHRU                     EL156
03221         GO TO 1500-EDIT-EXPENSES.                                 EL156
03222                                                                      CL*65
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
121802*       IF CL-CLAIM-TYPE    = PI-AH-OVERRIDE-L1 AND                  CL*93
121802*          CM-AH-BENEFIT-CD = '23'              AND                  CL*93
121802*          CL-CERT-STATE    = '04'              AND                  CL*93
121802*         (PI-PMTTYPE = '1' OR '2' OR '3')                           CL*93
121802*            PERFORM 3200-CALC-AIG-DAYS THRU 3299-EXIT               CL*93
121802*            GO TO 1362-CHECK-MAX-DAYS.                              CL*93
03230                                                                      CL*20
03231      IF PI-EDAYS = ZEROS                                             CL*93
121802*        IF PI-COMPANY-ID = 'DMD'                                    CL*88
121802*            MOVE WS-SV-EPYFROM  TO DC-BIN-DATE-1                    CL*82
121802*            MOVE -1             TO DC-ELAPSED-DAYS                  CL*82
121802*            MOVE ZEROS          TO DC-ELAPSED-MONTHS                CL*82
121802*            MOVE '6'            TO DC-OPTION-CODE                   CL*82
121802*            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL*82
121802*            MOVE DC-BIN-DATE-2  TO DC-BIN-DATE-1                    CL*82
121802*            MOVE WS-SV-EPYTHRU  TO DC-BIN-DATE-2                    CL*82
121802*            MOVE '1'            TO DC-OPTION-CODE                   CL*82
121802*            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL*82
121802*            IF NO-CONVERSION-ERROR                                  CL*88
121802*                COMPUTE PI-EDAYS = DC-ELAPSED-MONTHS * 30           CL*93
121802*                                 + DC-ODD-DAYS-OVER                 CL*93
121802*               MOVE 6           TO EDAYSL                           CL*86
121802*               MOVE AL-UNNON    TO EDAYSA                           CL*86
121802*            ELSE                                                    CL*82
121802*                NEXT SENTENCE                                       CL*82
121802*       ELSE                                                         CL*82
03250             MOVE WS-SV-EPYFROM   TO DC-BIN-DATE-1                    CL*93
03251             MOVE WS-SV-EPYTHRU   TO DC-BIN-DATE-2                    CL*93
03252             MOVE '1'             TO DC-OPTION-CODE                   CL*93
03253             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            CL*82
03254             IF NO-CONVERSION-ERROR                                   CL*88
03255                 MOVE DC-ELAPSED-DAYS  TO PI-EDAYS                    CL*93
03256                 ADD 1                 TO PI-EDAYS                    CL*93
03257                 MOVE AL-UNNON         TO EDAYSA.                     CL*93
03258                                                                      CL*82
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*88
121802*        MOVE PI-CPYFROM         TO DC-BIN-DATE-1                    CL*82
121802*        MOVE -1                 TO DC-ELAPSED-DAYS                  CL*82
121802*        MOVE ZEROS              TO DC-ELAPSED-MONTHS                CL*82
121802*        MOVE '6'                TO DC-OPTION-CODE                   CL*82
121802*        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT               CL*82
121802*        MOVE DC-BIN-DATE-2      TO DC-BIN-DATE-1                    CL*82
121802*        MOVE PI-CPYTHRU         TO DC-BIN-DATE-2                    CL*82
121802*        MOVE '1'                TO DC-OPTION-CODE                   CL*82
121802*        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT               CL*82
121802*    ELSE                                                            CL*82
03270          MOVE PI-CPYFROM         TO DC-BIN-DATE-1.                   CL*93
03271          MOVE PI-CPYTHRU         TO DC-BIN-DATE-2.                   CL*93
03272          MOVE '1'                TO DC-OPTION-CODE.
03273          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.              CL*82
03274                                                                      CL*82
03275      IF NO-CONVERSION-ERROR                                          CL*82
03276         IF PI-COMPANY-ID = 'DMD'                                     CL*93
03277             COMPUTE PI-CDAYS = DC-ELAPSED-MONTHS * 30                CL*93
03278                              + DC-ODD-DAYS-OVER                      CL*93
03279         ELSE                                                         CL*82
03280             MOVE DC-ELAPSED-DAYS TO PI-CDAYS                         CL*82
03281             ADD 1                TO PI-CDAYS.                        CL*82
03282                                                                   EL156
03283      IF DC-ODD-DAYS-OVER NOT = ZEROS                              EL156
03284         MOVE DC-ELAPSED-MONTHS   TO EXPENSE-MONTH-SAVE            EL156
03285         ADD 1                    TO EXPENSE-MONTH-SAVE.           EL156
03286                                                                   EL156
03287      IF PI-EDAYS NOT NUMERIC                                      EL156
03288          MOVE ER-0491            TO EMI-ERROR                     EL156
03289          GO TO 1370-DAYS-ERROR.                                   EL156
03290                                                                      CL*65
112210     IF CL-NO-OF-PMTS-MADE = 0
030512        AND (PI-COMPANY-ID = 'CID' OR 'AHL')
112210        AND CL-CERT-STATE = 'MI'
112210        AND (CM-AH-BENEFIT-CD = '20' OR '21' OR '22' OR '23')
112210            PERFORM 1390-CHECK-PAYMENT-DAYS THRU 1390-EXIT
112210     END-IF.
112210
03291  1362-CHECK-MAX-DAYS.                                                CL*65
03292                                                                   EL156
03293      IF WS-MAX-DAYS-TOL = +999                                    EL156
03294          GO TO 1365-SKIP-MAX-DAYS-EDIT.                           EL156
03295                                                                      CL*71
03296      IF PI-COMPANY-ID = 'HER' OR 'KSM'                               CL*88
03297          IF PI-EDAYS = ZEROS                                         CL*88
03298              COMPUTE WS-PMT-MOS ROUNDED = (PI-CDAYS / +30) + .005    CL*76
03299          ELSE                                                        CL*71
03300              COMPUTE WS-PMT-MOS ROUNDED = (PI-EDAYS / +30) + .005.   CL*76
03301                                                                   EL156
03302      IF WS-MAX-DAYS-TOL NOT = ZERO                                EL156
03303         IF PI-EDAYS > WS-MAX-DAYS-TOL OR                             CL*94
03304            PI-CDAYS > WS-MAX-DAYS-TOL                                CL*94
03305              MOVE ER-0492          TO EMI-ERROR                      CL*93
03306              GO TO 1370-DAYS-ERROR.                                  CL*93
03307                                                                   EL156
03308  1365-SKIP-MAX-DAYS-EDIT.                                         EL156
03309      IF PI-EDAYS = ZEROS                                          EL156
03310         GO TO 1400-EDIT-PAYMENT.                                  EL156
03311                                                                   EL156
03312      IF WS-DAYS-TOL NOT = ZERO                                    EL156
03313         IF ((PI-CDAYS - WS-DAYS-TOL) > PI-EDAYS) OR                  CL*94
03314            ((PI-CDAYS + WS-DAYS-TOL) < PI-EDAYS)                     CL*94
03315                 MOVE ER-0493     TO EMI-ERROR                     EL156
03316                 GO TO 1370-DAYS-ERROR.                            EL156
03317                                                                   EL156
03318      GO TO 1400-EDIT-PAYMENT.                                     EL156
03319                                                                   EL156
03320  1370-DAYS-ERROR.                                                 EL156
03321      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
03322                                                                   EL156
03323      IF EMI-MESSAGE-FORMATTED                                     EL156
03324          MOVE -1                 TO EDAYSL                        EL156
03325          MOVE AL-UABON           TO EDAYSA.                       EL156
03326                                                                      CL*64
03327      GO TO 1400-EDIT-PAYMENT.                                        CL*64
03328                                                                      CL*64
03329  1380-EDIT-EXPENSE-PMT-AMT.                                          CL*64
03330      IF PI-PMTTYPE = '5' OR '6'                                      CL*64
03331          NEXT SENTENCE                                               CL*64
03332      ELSE                                                            CL*64
03333          GO TO 1400-EDIT-PAYMENT.                                    CL*64
03334                                                                      CL*64
03335      IF WS-MAX-EXP-PMT NOT = ZEROS                                   CL*93
03336         IF PI-EPYAMT > WS-MAX-EXP-PMT                                CL*94
03337             MOVE ER-3529            TO EMI-ERROR                     CL*65
03338             PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT.               CL*65
03339                                                                      CL*64
03340      GO TO 1500-EDIT-EXPENSES.                                       CL*64
03341                                                                   EL156
03342      EJECT                                                        EL156
112210
112210 1390-CHECK-PAYMENT-DAYS.
112210
112210     IF PI-EDAYS = 0
112210         MOVE PI-CDAYS TO WS-PD-DAYS
112210     ELSE
112210         MOVE PI-EDAYS TO WS-PD-DAYS
112210     END-IF.
112210
112210     IF CM-AH-BENEFIT-CD = '20' OR '21'
112210         IF WS-PD-DAYS = 14
112210            GO TO 1390-EXIT
112210         END-IF
112210     END-IF.
030512
030512     IF PI-COMPANY-ID = 'AHL'  AND
030512        CM-AH-BENEFIT-CD = '1K'
030512         IF WS-PD-DAYS = 14
030512            GO TO 1390-EXIT
030512         END-IF
030512     END-IF.
112210
112210     IF WS-PD-DAYS < 30
112210         MOVE ER-3549              TO EMI-ERROR
112210         PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
112210     END-IF.
112210
112210 1390-EXIT.
112210      EXIT.
112210
03343  1400-EDIT-PAYMENT.                                               EL156
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*88
121802*       IF CL-CERT-NO (5:2) = '69' OR '70'                           CL*88
121802*           PERFORM 6500-READ-DLO035 THRU 6500-EXIT.                 CL*88
03347                                                                      CL*88
120115     IF PI-COMPANY-ID = 'DCC' or 'VPP'
061013        MOVE PI-COMPANY-CD       TO ACCT-COMP-CD      
061013        MOVE PI-CARRIER          TO ACCT-CARRIER      
061013        MOVE PI-GROUPING         TO ACCT-GROUPING     
061013        MOVE PI-STATE            TO ACCT-STATE        
061013        MOVE PI-ACCOUNT          TO ACCT-ACCOUNT      
061013        MOVE PI-CERT-EFF-DT      TO ACCT-EXP-DT
061013        MOVE ERACCT-PARTIAL-KEY  TO WS-ERACCT-SAVE-KEY
061013        MOVE SPACES              TO WS-ERACCT-HOLD-RECORD
061013
061013        EXEC CICS READ
061013           DATASET ('ERACCT')
061013           RIDFLD  (ERACCT-KEY)
061013           SET     (ADDRESS OF ACCOUNT-MASTER)
061013           GTEQ
061013           resp    (WS-RESPONSE)
061013        END-EXEC
061013        IF WS-RESP-NORMAL
061013           AND WS-ERACCT-SAVE-KEY = AM-CONTROL-PRIMARY (1:20)
061013           and pi-cert-eff-dt < am-expiration-dt
061013           and pi-cert-eff-dt >= am-effective-dt
061013           move am-dcc-product-code to pi-dcc-product-code
061013        else
061013           move spaces           to pi-dcc-product-code
061013        end-if
061013        if PI-DCC-PRODUCT-CODE NOT = SPACES
061013           PERFORM 0900-GET-DDF-LIMITS
061013                                 THRU 0900-EXIT
061013           IF PDEF-FOUND
061013              PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122                 (P1 > 11)
022122                 OR ((PD-PROD-CODE (P1) = CL-CLAIM-TYPE)
022122                   AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
061013              END-PERFORM
022122              IF P1 < +12
100314                 if pd-ben-pct (p1) not numeric
100314                    move zeros   to pd-ben-pct (p1)
100314                 end-if
100314                 if pd-ben-pct (p1) = zeros
100314                    move +1      to ws-work-ben-pct
100314                 else
100314                    move pd-ben-pct (p1)
100314                                 to ws-work-ben-pct
100314                 end-if
100314                 compute cm-ah-benefit-amt =
100314                    cm-ah-benefit-amt * ws-work-ben-pct
100314                 if cm-ah-benefit-amt > pd-max-amt (p1)
100314                    move pd-max-amt (p1) to cm-ah-benefit-amt
100314                 end-if
                    else
                       move er-1674    to emi-error
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
061013              END-IF
                 else
                    move er-1672       to emi-error
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
061013           END-IF
061013        end-if
061013     END-IF

03348      IF PI-PMTTYPE NOT = '4'                                      EL156
03349         GO TO 1405-CALC-PAYMENT.                                  EL156

03351      IF PI-EDAYS NOT NUMERIC                                      EL156
03352          MOVE PI-EPYAMT           TO PI-CPYAMT                       CL*65
03353          MOVE SPACES              TO PI-SPECIAL-BEN-SW               CL*93
03354          GO TO 1430-CHECK-AH-TOLERANCE.                              CL*65
03355                                                                   EL156
121802*    IF PI-COMPANY-ID = 'FIA'                                        CL*93
121802*       IF CERT-ACCOUNT = '0000011043'                               CL*93
121802*        COMPUTE PI-DAILY-RATE ROUNDED =                             CL*65
121802*                      (CM-AH-BENEFIT-AMT * 13) / 365             EL156
121802*        COMPUTE PI-CPYAMT ROUNDED =                              EL156
121802*                          PI-DAILY-RATE * PI-EDAYS               EL156
121802*        MOVE SPACES TO PI-SPECIAL-BEN-SW                            CL*65
121802*        GO TO 1430-CHECK-AH-TOLERANCE.                              CL*65
03364                                                                   EL156
03365      IF WS-CALC-METHOD = '1' OR '4' OR '6'                        EL156
03366          COMPUTE PI-DAILY-RATE ROUNDED = CM-AH-BENEFIT-AMT / 30      CL*93
03367          COMPUTE PI-CPYAMT     ROUNDED = PI-DAILY-RATE * PI-EDAYS    CL*93
03368          GO TO 1420-CHECK-AIG-BENEFIT.                               CL*93
03369                                                                   EL156
03370      IF WS-CALC-METHOD = '2' OR  '5'                              EL156
03371          COMPUTE PI-DAILY-RATE ROUNDED =                          EL156
03372                         (CM-AH-BENEFIT-AMT * 12) / 365            EL156
03373          COMPUTE PI-CPYAMT ROUNDED =                              EL156
03374                            PI-DAILY-RATE * PI-EDAYS               EL156
03375         GO TO 1420-CHECK-AIG-BENEFIT.                                CL*65
03376                                                                   EL156
03377      IF WS-CALC-METHOD = '3'                                      EL156
03378          COMPUTE PI-DAILY-RATE =                                  EL156
03379                          (CM-AH-BENEFIT-AMT * 12) / 365           EL156
03380          COMPUTE PI-CPYAMT ROUNDED =                              EL156
03381                            PI-DAILY-RATE * PI-EDAYS               EL156
03382         GO TO 1420-CHECK-AIG-BENEFIT.                                CL*65
03383      EJECT                                                        EL156
03384 *    SAMPLE DATE RANGES AND DAYS PAID FOR OPTION 1 AND 6          EL156
03385                                                                   EL156
03386 ****************************************************************  EL156
03387 ****************************************************************  EL156
03388 *                  CALCULATED FROM      PAYMENT      PAYMENT      EL156
03389 *   DATES           DATE ROUTINE        OPT  1       OPT  6       EL156
03390 * FROM    THRU    MONTHS ODD ELAPSED   DAYS-PAID    DAYS-PAID     EL156
03391 *1-01-83  1-15-83    0    15     15        15           15        EL156
03392 *1-01-83  1-30-83    0    30     30        30           30        EL156
03393 *1-01-83  1-31-83    1     0     31        30           30        EL156
03394 *1-01-83  2-01-83    1     1     32        31           31        EL156
03395 *1-01-83  2-15-83    1    15     46        45           45        EL156
03396 *1-01-83  2-28-83    2     0     59        60           60        EL156
03397 *1-01-83  3-01-83    2     1     60        61           61        EL156
03398 *1-01-83  3-31-83    3     0     90        90           90        EL156
03399 *1-15-83  1-31-83    0    17     17        17           16        EL156
03400 *1-15-83  2-01-83    0    18     18        18           17        EL156
03401 *1-15-83  2-28-83    1    14     45        44           46        EL156
03402 *1-21-83  3-09-83    1    20     48        50           49        EL156
03403 *2-05-83  3-06-83    1     2     30        32           32        EL156
03404 *2-05-83  3-23-83    1    19     47        49           49        EL156
03405 *2-15-83  2-28-83    0    14     14        14           16        EL156
03406 *2-15-83  3-01-83    0    15     15        15           17        EL156
03407 *2-21-83  3-19-83    0    27     27        27           29        EL156
03408 *2-21-83  3-20-83    1     0     28        30           30        EL156
03409 *2-21-83  4-10-83    1    18     49        30           50        EL156
03410 *2-21-83  5-19-83    2    27     88        87           89        EL156
03411 *2-21-83  5-22-83    3     2     91        92           92        EL156
03412 *3-21-83  5-19-83    1    30     60        60           59        EL156
03413 ****************************************************************  EL156
03414      EJECT                                                        EL156
03415  1405-CALC-PAYMENT.                                               EL156
03416      MOVE PI-CPYFROM             TO DC-BIN-DATE-1.                EL156
03417      MOVE ' '                    TO DC-OPTION-CODE.               EL156
03418      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
03419      MOVE DC-DAYS-IN-MONTH       TO FROM-DAYS-IN-MONTH.           EL156
03420      MOVE DC-GREG-DATE-1-MDY     TO WORK-DATE-FROM.               EL156
03421                                                                   EL156
03422      MOVE PI-CPYTHRU             TO DC-BIN-DATE-1.                EL156
03423      MOVE ' '                    TO DC-OPTION-CODE.               EL156
03424      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
03425      MOVE DC-GREG-DATE-1-MDY     TO WORK-DATE-THRU.                  CL*43
03426      MOVE DC-DAYS-IN-MONTH       TO THRU-DAYS-IN-MONTH.           EL156
03427                                                                   EL156
03428      MOVE PI-CPYFROM             TO DC-BIN-DATE-1.                EL156
03429      MOVE -1                     TO DC-ELAPSED-DAYS.              EL156
03430      MOVE ZEROS                  TO DC-ELAPSED-MONTHS.            EL156
03431      MOVE '6'                    TO DC-OPTION-CODE.               EL156
03432      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
03433      MOVE DC-DAYS-IN-MONTH       TO SAVE1-FROM-DAYS-IN-MONTH.     EL156
03434                                                                   EL156
03435      MOVE DC-BIN-DATE-2          TO DC-BIN-DATE-1.                EL156
03436      MOVE PI-CPYTHRU             TO DC-BIN-DATE-2.                EL156
03437      MOVE '1'                    TO DC-OPTION-CODE.               EL156
03438      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
03439                                                                   EL156
03440      MOVE DC-ELAPSED-MONTHS      TO SAVE-ELAPSED-MONTHS.          EL156
03441      MOVE DC-ODD-DAYS-OVER       TO SAVE-ODD-DAYS-OVER.           EL156
03442      MOVE DC-ELAPSED-DAYS        TO SAVE-ELAPSED-DAYS.            EL156
03443                                                                   EL156
03444 ******************************************************************EL156
03445 *   THE FOLLOWING STATEMENT ADJUSTS THE ODD DAYS OVER BY THE     *EL156
03446 *     DIFFERENCE BETWEEN DAYS IN MONTH (FROM MONTH) AND          *EL156
03447 *     DAYS IN MONTH (ONE MONTH PRIOR TO THRU MONTH)              *EL156
03448 ******************************************************************EL156
03449                                                                   EL156
03450      IF WS-CALC-METHOD = '1'           AND                        EL156
03451         DC-ELAPSED-MONTHS > ZERO AND                                 CL*94
03452         DC-ODD-DAYS-OVER NOT = ZEROS   AND                        EL156
03453         WDF-DAY > WDT-DAY                                            CL*94
03454           MOVE PI-CPYTHRU          TO DC-BIN-DATE-1                  CL*93
03455           MOVE ZEROS               TO DC-ELAPSED-DAYS                CL*93
03456           MOVE -1                  TO DC-ELAPSED-MONTHS              CL*93
03457           MOVE '6'                 TO DC-OPTION-CODE                 CL*93
03458           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT              CL*93
03459           MOVE DC-DAYS-IN-MONTH    TO SAVE1-THRU-DAYS-IN-MONTH       CL*93
03460           COMPUTE WS-SAVE-ODD-DAYS =                                 CL*93
03461           (SAVE1-THRU-DAYS-IN-MONTH - SAVE1-FROM-DAYS-IN-MONTH)      CL*93
03462           ADD WS-SAVE-ODD-DAYS     TO SAVE-ODD-DAYS-OVER.            CL*93
03463                                                                   EL156
03464      MOVE SAVE-ELAPSED-MONTHS    TO DC-ELAPSED-MONTHS.            EL156
03465      MOVE SAVE-ODD-DAYS-OVER     TO DC-ODD-DAYS-OVER              EL156
03466      MOVE SAVE-ELAPSED-DAYS      TO DC-ELAPSED-DAYS.              EL156
031808
031808     MOVE SAVE-ELAPSED-MONTHS TO PI-SAVE-ELAPSED-MONTHS.
031808     MOVE SAVE-ODD-DAYS-OVER  TO PI-SAVE-ODD-DAYS-OVER.
03467                                                                   EL156
121802*    IF PI-COMPANY-ID = 'FIA'                                        CL*93
121802*       IF CERT-ACCOUNT = '0000011043'                               CL*93
121802*        COMPUTE PI-DAILY-RATE ROUNDED =                             CL*93
121802*                          (CM-AH-BENEFIT-AMT * 13) / 365         EL156
121802*        COMPUTE PI-CPYAMT ROUNDED =                              EL156
121802*                          PI-DAILY-RATE * DC-ELAPSED-DAYS        EL156
121802*        GO TO 1420-CHECK-AIG-BENEFIT.                               CL*65
03475                                                                      CL*65
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
121802*       IF CL-CLAIM-TYPE    = PI-AH-OVERRIDE-L1  AND                 CL*93
121802*          CM-AH-BENEFIT-CD = '23'               AND                 CL*93
121802*          CL-CERT-STATE    = '04'               AND                 CL*93
121802*         (PI-PMTTYPE = '1' OR '2' OR '3')                           CL*93
121802*            COMPUTE WS-DAILY-RATE ROUNDED =                         CL*93
121802*                               CM-AH-BENEFIT-AMT / 30               CL*93
121802*            COMPUTE PI-CPYAMT ROUNDED =                             CL*93
121802*                               (WS-DAILY-RATE * PI-CDAYS)           CL*93
121802*            MOVE WS-DAILY-RATE TO PI-DAILY-RATE                     CL*93
121802*            GO TO 1420-CHECK-AIG-BENEFIT.                           CL*93
03487                                                                   EL156
03488      IF WS-CALC-METHOD = '1'                                      EL156
03489          COMPUTE WS-DAILY-RATE ROUNDED = CM-AH-BENEFIT-AMT / 30   EL156
03490          COMPUTE PI-CPYAMT ROUNDED =                              EL156
03491              (CM-AH-BENEFIT-AMT * DC-ELAPSED-MONTHS) +            EL156
03492              (WS-DAILY-RATE     * DC-ODD-DAYS-OVER)               EL156
03493          MOVE WS-DAILY-RATE    TO PI-DAILY-RATE                      CL*93
03494          GO TO 1420-CHECK-AIG-BENEFIT.                               CL*65
03495                                                                   EL156
03496      IF WS-CALC-METHOD = '2'                                      EL156
03497          COMPUTE PI-DAILY-RATE ROUNDED =                          EL156
03498                           (CM-AH-BENEFIT-AMT * 12) / 365          EL156
03499          COMPUTE PI-CPYAMT ROUNDED =                              EL156
03500              (CM-AH-BENEFIT-AMT * DC-ELAPSED-MONTHS) +            EL156
03501              (PI-DAILY-RATE     * DC-ODD-DAYS-OVER)               EL156
03502          GO TO 1420-CHECK-AIG-BENEFIT.                               CL*65
03503                                                                   EL156
03504      IF WS-CALC-METHOD = '4'                                      EL156
03505          MOVE CM-AH-BENEFIT-AMT   TO  WS-AH-BEN-AMT               EL156
03506          COMPUTE WS-DAILY-RATE ROUNDED = (WS-AH-BEN-AMT / 30)        CL**3
03507          MOVE WS-DAILY-RATE TO PI-DAILY-RATE                         CL**3
03508          COMPUTE PI-CPYAMT ROUNDED =                              EL156
03509                            WS-DAILY-RATE * DC-ELAPSED-DAYS           CL**3
03510          GO TO 1420-CHECK-AIG-BENEFIT.                               CL*65
03511                                                                   EL156
03512      IF WS-CALC-METHOD = '5'                                      EL156
03513          COMPUTE PI-DAILY-RATE ROUNDED =                          EL156
03514                             (CM-AH-BENEFIT-AMT * 12) / 365        EL156
03515          COMPUTE PI-CPYAMT ROUNDED =                              EL156
03516                            PI-DAILY-RATE * DC-ELAPSED-DAYS        EL156
03517          GO TO 1420-CHECK-AIG-BENEFIT.                               CL*65
03518                                                                   EL156
03519      IF WS-CALC-METHOD NOT = '3'                                  EL156
03520         GO TO 1410-CHECK-PAYMENT.                                 EL156
03521                                                                   EL156
03522 ******   CALCULATION METHOD  3  ROUTINE                           EL156
03523      COMPUTE PI-DAILY-RATE =                                      EL156
03524                  CM-AH-BENEFIT-AMT / FROM-DAYS-IN-MONTH.          EL156
03525                                                                   EL156
03526 ******CHECK TO SEE IF WITHIN SAME MONTH                           EL156
03527      IF (WDF-MONTH = WDT-MONTH) AND (WDF-YEAR = WDT-YEAR)         EL156
03528         COMPUTE PI-CPYAMT ROUNDED = SAVE-ELAPSED-DAYS    *        EL156
03529                      (CM-AH-BENEFIT-AMT / FROM-DAYS-IN-MONTH)     EL156
03530          GO TO 1420-CHECK-AIG-BENEFIT.                               CL*65
03531                                                                   EL156
03532 ******CHECK TO SEE IF EVEN MONTHS AND NO ODD DAYS                 EL156
03533      IF SAVE-ELAPSED-MONTHS NOT = ZEROS   AND                     EL156
03534              SAVE-ODD-DAYS-OVER = ZEROS                           EL156
03535       COMPUTE PI-CPYAMT  ROUNDED =                                EL156
03536                   CM-AH-BENEFIT-AMT * SAVE-ELAPSED-MONTHS         EL156
03537       GO TO 1420-CHECK-AIG-BENEFIT.                                  CL*65
03538                                                                   EL156
03539 ******BUMP THE FROM DATE WITH THE NUMBER OF MONTHS                EL156
03540 ******AND FIND THE NUMBER OF DAYS LEFT IN THIS MONTH.             EL156
03541      IF SAVE-ELAPSED-MONTHS NOT = ZERO                            EL156
03542         ADD SAVE-ELAPSED-MONTHS  TO WDF-MONTH                     EL156
03543         IF WDF-MONTH > 12                                            CL*94
03544            ADD 1                 TO WDF-YEAR                      EL156
03545            SUBTRACT 12         FROM WDF-MONTH.                       CL*93
03546                                                                      CL*84
03547 ******CONVERT THE NEW FROM MONTH TO BINARY                        EL156
03548      IF SAVE-ELAPSED-MONTHS NOT = ZERO                            EL156
03549         MOVE WORK-DATE-FROM   TO DC-GREG-DATE-1-MDY                  CL*93
03550         MOVE '4'              TO DC-OPTION-CODE                      CL*93
03551         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             EL156
03552         MOVE DC-DAYS-IN-MONTH TO FROM-DAYS-IN-MONTH.                 CL*93
03553                                                                   EL156
03554      IF WDF-MONTH = WDT-MONTH AND                                    CL*93
03555         WDF-YEAR  = WDT-YEAR                                         CL*93
03556           COMPUTE PI-CPYAMT ROUNDED = ((WDT-DAY  -  WDF-DAY)  *      CL*93
03557                     (CM-AH-BENEFIT-AMT / FROM-DAYS-IN-MONTH)) +   EL156
03558                     (CM-AH-BENEFIT-AMT * SAVE-ELAPSED-MONTHS)     EL156
03559         ELSE                                                      EL156
03560           COMPUTE FROM-PMT-DAYS = FROM-DAYS-IN-MONTH - WDF-DAY       CL*93
03561           MOVE WDT-DAY              TO THRU-PMT-DAYS                 CL*93
03562           COMPUTE PI-CPYAMT ROUNDED =                                CL*93
03563        (FROM-PMT-DAYS * (CM-AH-BENEFIT-AMT / FROM-DAYS-IN-MONTH)) EL156
03564        +                                                          EL156
03565        (THRU-PMT-DAYS * (CM-AH-BENEFIT-AMT / THRU-DAYS-IN-MONTH)) EL156
03566        +                                                          EL156
03567        (CM-AH-BENEFIT-AMT * SAVE-ELAPSED-MONTHS).                 EL156
03568                                                                      CL*43
03569      GO TO 1420-CHECK-AIG-BENEFIT.                                   CL*65
03570      EJECT                                                        EL156
03571  1410-CHECK-PAYMENT.                                              EL156
03572 ******PAYMENT OPTION 6                                            EL156
03573 ******CHECK TO SEE IF LESS THAN 1 MONTH AND WITHIN SAME MONTH     EL156
03574      IF SAVE-ELAPSED-MONTHS > ZERO                                   CL*94
03575         IF SAVE-ODD-DAYS-OVER = ZEROS                             EL156
03576            GO TO 1412-CHECK-TO-DAY                                EL156
03577         ELSE                                                      EL156
03578            IF WDF-DAY > WDT-DAY                                      CL*94
03579               IF WDF-MONTH = 02                                   EL156
03580                   PERFORM 1415-CHECK-LEAP-YEAR THRU 1415-EXIT     EL156
03581                   ADD 2  TO  SAVE-ODD-DAYS-OVER                      CL*93
03582                   GO TO 1412-CHECK-TO-DAY                         EL156
03583               ELSE                                                EL156
03584                   IF FROM-DAYS-IN-MONTH = 31                         CL*80
03585                      SUBTRACT 1 FROM SAVE-ODD-DAYS-OVER           EL156
03586                      GO TO 1412-CHECK-TO-DAY                      EL156
03587                   ELSE                                            EL156
03588                      GO TO 1412-CHECK-TO-DAY                      EL156
03589            ELSE                                                   EL156
03590                GO TO 1412-CHECK-TO-DAY.                           EL156
03591                                                                   EL156
03592      IF WDF-MONTH NOT = WDT-MONTH                                 EL156
03593         IF WDF-MONTH = 02                                         EL156
03594            PERFORM 1415-CHECK-LEAP-YEAR THRU 1415-EXIT            EL156
03595            IF WDF-DAY = 29                                        EL156
03596               ADD 1              TO SAVE-ODD-DAYS-OVER            EL156
03597            ELSE                                                   EL156
03598               ADD 2              TO SAVE-ODD-DAYS-OVER            EL156
03599         ELSE                                                      EL156
03600            IF FROM-DAYS-IN-MONTH = 31                                CL*80
03601               SUBTRACT 1         FROM SAVE-ODD-DAYS-OVER.         EL156
03602                                                                   EL156
03603  1412-CHECK-TO-DAY.                                               EL156
03604         IF WDT-MONTH = 02 AND SAVE-ODD-DAYS-OVER NOT = 0          EL156
03605            IF WDT-DAY = 28                                        EL156
03606               ADD 2              TO SAVE-ODD-DAYS-OVER            EL156
03607            ELSE                                                   EL156
03608               IF WDT-DAY = 29                                     EL156
03609                  ADD 1           TO SAVE-ODD-DAYS-OVER            EL156
03610               ELSE                                                EL156
03611                  NEXT SENTENCE                                    EL156
03612         ELSE                                                      EL156
03613            IF WDT-DAY = 31 AND SAVE-ODD-DAYS-OVER NOT = 0         EL156
03614               SUBTRACT 1         FROM SAVE-ODD-DAYS-OVER.         EL156
03615                                                                   EL156
03616      COMPUTE WS-DAILY-RATE ROUNDED = CM-AH-BENEFIT-AMT / 30.         CL*43
03617      MOVE WS-DAILY-RATE          TO PI-DAILY-RATE.                   CL*43
03618      COMPUTE PI-CPYAMT ROUNDED =                                  EL156
03619          (CM-AH-BENEFIT-AMT * SAVE-ELAPSED-MONTHS) +              EL156
03620          (WS-DAILY-RATE     * SAVE-ODD-DAYS-OVER).                EL156
03621                                                                   EL156
03622      GO TO 1420-CHECK-AIG-BENEFIT.                                   CL*65
03623                                                                   EL156
03624  1415-CHECK-LEAP-YEAR.                                            EL156
03625      IF FROM-DAYS-IN-MONTH = 29                                   EL156
03626         SUBTRACT 1 FROM SAVE-ODD-DAYS-OVER.                       EL156
03627                                                                   EL156
03628  1415-EXIT.                                                       EL156
03629      EXIT.                                                        EL156
03630                                                                   EL156
03631      EJECT                                                        EL156
03632  1420-CHECK-AIG-BENEFIT.                                             CL*65
03633                                                                      CL*65
03634      IF WS-SPECIAL-CALC-CD = 'Q'                                     CL*88
03635          GO TO 1425-CHECK-WHOLE-MONTH.                               CL*80
03636                                                                      CL*80
121802*    IF (PI-COMPANY-ID = 'AIG' OR 'AUK')   AND                       CL*93
121802*       (CM-AH-BENEFIT-CD = '63' OR '66')  AND                       CL*93
121802*       (CL-NO-OF-PMTS-MADE < +6)          AND                       CL*94
121802*       (PI-PMTTYPE = '1' OR '2')                                    CL*93
121802*        NEXT SENTENCE                                               CL*65
121802*    ELSE                                                            CL*65
03643          MOVE SPACES TO PI-SPECIAL-BEN-SW.
03644          GO TO 1430-CHECK-AH-TOLERANCE.                              CL*65
03645                                                                      CL*65
03646 ******************AIG LOGIC STARTS**********************             CL*82
121802*    MOVE 'Y'          TO PI-SPECIAL-BEN-SW.                         CL*93
121802*    COMPUTE PI-CPYAMT = PI-CPYAMT * 1.5.                            CL*65
121802*    MOVE ER-3533      TO EMI-ERROR.                                 CL*93
121802*    PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT.                      CL*65
121802*    GO TO 1430-CHECK-AH-TOLERANCE.                                  CL*80
03652 ******************AIG LOGIC ENDS************************             CL*82
03653                                                                      CL*80
03654  1425-CHECK-WHOLE-MONTH.                                             CL*80
03655 ******************************************************************   CL*80
03656 ****    THIS PARAGRAPH COMPUTES THE 1ST PAYMENT FOR WHOLE     ****   CL*80
03657 ****    MONTH DISABILITY CLAIMS.  THE PROCESS IS AS FOLLOWS:  ****   CL*80
03658 ****      1.  IF THE PERIOD BEING PAID IS THE RETRO PERIOD    ****   CL*80
03659 ****          A WHOLE MONTHS BENEFIT IS PAID.                 ****   CL*80
03660 ****      2.  IF MORE THAN THE RETRO PERIOD IS BEING PAID THE ****   CL*80
03661 ****          ADDITIONAL DAYS MUST BE IN WHOLE CALENDAR       ****   CL*80
03662 ****          MONTHS.  PAYMENT = 1 MONTH (RETRO               ****   CL*88
03663 ****          PERIOD) + NUMBER OF ADDITIONAL MONTHS BEING     ****   CL*80
03664 ****          PAID * MONTHLY BENEFIT.                         ****   CL*80
03665 ******************************************************************   CL*80
03666                                                                      CL*80
03667      MOVE SPACES                     TO  PI-SPECIAL-BEN-SW.          CL*80
03668                                                                      CL*80
03669      IF CL-NO-OF-PMTS-MADE > +0                                      CL*94
03670          GO TO 1425-CONT-WHOLE-MONTH-PMTS.                           CL*80
03671                                                                      CL*80
03672      IF PI-CDAYS < WS-BEN-DAYS                                       CL*94
03673          GO TO 1430-CHECK-AH-TOLERANCE.                              CL*80
03674                                                                      CL*80
03675      IF PI-CDAYS = WS-BEN-DAYS                                       CL*88
03676          MOVE CM-AH-BENEFIT-AMT      TO  PI-CPYAMT                   CL*80
03677          GO TO 1430-CHECK-AH-TOLERANCE.                              CL*80
03678                                                                      CL*80
121802*    IF PI-COMPANY-ID = 'ACM'                                        CL*93
121802*       NEXT SENTENCE                                                CL*81
121802*    ELSE                                                            CL*81
03682         GO TO 1425-NOT-ACM-W15C.                                     CL*81
03683                                                                      CL*81
121802*    MOVE PI-CPYFROM             TO  DC-BIN-DATE-1.                  CL*94
121802*    MOVE -1                     TO  DC-ELAPSED-DAYS.                CL*94
121802*    MOVE +0                     TO  DC-ELAPSED-MONTHS.              CL*81
121802*    MOVE '6'                    TO  DC-OPTION-CODE.                 CL*81
121802*    PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*81
121802*    IF NO-CONVERSION-ERROR                                          CL*81
121802*       MOVE DC-BIN-DATE-2       TO  DC-BIN-DATE-1                   CL*81
121802*       MOVE PI-CPYTHRU          TO  DC-BIN-DATE-2                   CL*81
121802*       MOVE '1'                 TO  DC-OPTION-CODE                  CL*81
121802*       MOVE +0                  TO  DC-ELAPSED-DAYS                 CL*81
121802*       MOVE +0                  TO  DC-ELAPSED-MONTHS               CL*81
121802*       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                CL*81
121802*       IF NO-CONVERSION-ERROR                                       CL*81
121802*          IF (DC-ELAPSED-MONTHS > +0) OR                            CL*94
121802*             (DC-ELAPSED-DAYS NOT < WS-BEN-DAYS)                    CL*94
121802*             NEXT SENTENCE                                          CL*81
121802*          ELSE                                                      CL*81
121802*             MOVE +0            TO  PI-CPYAMT                       CL*81
121802*             MOVE ER-0846       TO  EMI-ERROR                       CL*81
121802*             PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT              CL*81
121802*             GO TO 1430-CHECK-AH-TOLERANCE                          CL*81
121802*       ELSE                                                         CL*81
121802*           MOVE ER-0846         TO  EMI-ERROR                       CL*81
121802*           PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT                CL*81
121802*           GO TO 1430-CHECK-AH-TOLERANCE.                           CL*81
121802*                                                                    CL*81
121802*    IF DC-ODD-DAYS-OVER NOT < WS-BEN-DAYS                           CL*94
121802*       ADD +1 TO DC-ELAPSED-MONTHS.                                 CL*81
121802*                                                                    CL*81
121802*    COMPUTE PI-CPYAMT ROUNDED =                                     CL*81
121802*                CM-AH-BENEFIT-AMT * DC-ELAPSED-MONTHS.              CL*93
121802*                                                                    CL*81
121802*    GO TO 1430-CHECK-AH-TOLERANCE.                                  CL*81
03717                                                                      CL*81
03718  1425-NOT-ACM-W15C.                                                  CL*81
03719                                                                      CL*81
03720      MOVE PI-CPYFROM                 TO  DC-BIN-DATE-1.              CL*80
03721      MOVE '6'                        TO  DC-OPTION-CODE.             CL*80
03722      MOVE +0                         TO  DC-ELAPSED-MONTHS.          CL*80
03723      COMPUTE DC-ELAPSED-DAYS = WS-BEN-DAYS - 1.                      CL*80
03724                                                                      CL*80
03725      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*80
03726      IF NO-CONVERSION-ERROR                                          CL*80
03727          MOVE DC-BIN-DATE-2          TO  DC-BIN-DATE-1               CL*80
03728      ELSE                                                            CL*80
03729          MOVE ER-0846                TO  EMI-ERROR                   CL*80
03730          PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT                   CL*80
03731          GO TO 1430-CHECK-AH-TOLERANCE.                              CL*80
03732                                                                      CL*80
03733      MOVE PI-CPYTHRU                 TO  DC-BIN-DATE-2.              CL*80
03734      MOVE '1'                        TO  DC-OPTION-CODE.             CL*80
03735      MOVE +0                         TO  DC-ELAPSED-MONTHS.          CL*80
03736      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*80
03737      IF NO-CONVERSION-ERROR                                          CL*80
03738          IF DC-ODD-DAYS-OVER = +0                                    CL*88
03739              NEXT SENTENCE                                           CL*80
03740          ELSE                                                        CL*80
03741              MOVE ER-0846            TO  EMI-ERROR                   CL*80
03742              PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT               CL*80
03743              GO TO 1430-CHECK-AH-TOLERANCE                           CL*80
03744      ELSE                                                            CL*80
03745          MOVE ER-0846                TO  EMI-ERROR                   CL*80
03746          PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT                   CL*80
03747          GO TO 1430-CHECK-AH-TOLERANCE.                              CL*80
03748                                                                      CL*80
03749      COMPUTE PI-CPYAMT ROUNDED = CM-AH-BENEFIT-AMT +                 CL*80
03750          (CM-AH-BENEFIT-AMT * DC-ELAPSED-MONTHS).                    CL*80
03751                                                                      CL*80
03752      GO TO 1430-CHECK-AH-TOLERANCE.                                  CL*80
03753                                                                      CL*80
03754  1425-CONT-WHOLE-MONTH-PMTS.                                         CL*80
03755 ******************************************************************   CL*80
03756 ****    WHOLE MONTH DISABILITY EDIT.  IN ORDER TO QUALIFY     ****   CL*80
03757 ****    FOR A PAYMENT TO BE MADE THE PAYMENT PERIOD MUST      ****   CL*80
03758 ****    BE IN CALENDAR MONTH INCREMENTS.                      ****   CL*80
03759 ******************************************************************   CL*80
03760                                                                      CL*80
03761      IF PI-COMPANY-ID = 'ACM'                                        CL*93
03762         NEXT SENTENCE                                                CL*81
03763      ELSE                                                            CL*80
03764         IF DC-ODD-DAYS-OVER = +0                                     CL*88
03765             NEXT SENTENCE                                            CL*81
03766         ELSE                                                         CL*81
03767             MOVE ER-0845            TO  EMI-ERROR                    CL*81
03768             PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT                CL*81
03769             GO TO 1430-CHECK-AH-TOLERANCE.                           CL*81
03770                                                                      CL*80
03771      COMPUTE PI-CPYAMT ROUNDED =                                     CL*80
03772                 CM-AH-BENEFIT-AMT * DC-ELAPSED-MONTHS.               CL*93
03773                                                                      CL*65
03774  1430-CHECK-AH-TOLERANCE.                                         EL156
03775      IF PI-PMT-APPR-SW = 'G'                                         CL*88
03776          GO TO 1430-CONT-CHECK-AH-TOLERANCE.                         CL*71
03777                                                                      CL*71
121802*    IF PI-COMPANY-ID = 'HER' OR 'KSM'                               CL*88
121802*        COMPUTE WS-MAX-PMT-TOL = WS-MAX-PMT-TOL * WS-PMT-MOS        CL*73
121802*        MOVE WS-MAX-PMT-TOL         TO  PI-MAX-PMT-TOL.             CL*73
121802*                                                                    CL*76
121802*    IF PI-COMPANY-ID = 'HER' OR 'KSM'                               CL*88
121802*      IF WDF-MONTH = 02                                             CL*88
121802*        IF SAVE-ELAPSED-MONTHS = 0                                  CL*88
121802*          NEXT SENTENCE                                             CL*76
121802*        ELSE                                                        CL*76
121802*          IF SAVE-ODD-DAYS-OVER > 0                                 CL*94
121802*            MOVE +0                     TO  WS-MAX-PMT-TOL          CL*76
121802*            COMPUTE WS-PMT-MOS ROUNDED =                            CL*76
121802*                (SAVE-ODD-DAYS-OVER / +30) + .005                   CL*76
121802*            COMPUTE WS-MAX-PMT-TOL = WS-HER-MO-PMT-AMT *            CL*76
121802*                                     WS-PMT-MOS                     CL*76
121802*            COMPUTE WS-MAX-PMT-TOL = WS-MAX-PMT-TOL +               CL*76
121802*                (WS-HER-MO-PMT-AMT * SAVE-ELAPSED-MONTHS)           CL*76
121802*          ELSE                                                      CL*76
121802*            MOVE +0                     TO  WS-MAX-PMT-TOL          CL*76
121802*            MOVE SAVE-ELAPSED-MONTHS    TO  WS-PMT-MOS              CL*76
121802*            COMPUTE WS-MAX-PMT-TOL = WS-HER-MO-PMT-AMT *            CL*76
121802*                                     WS-PMT-MOS.                    CL*76
03800                                                                      CL*71
03801      IF WS-MAX-PMT-TOL NOT = ZERO                                    CL*71
03802         IF (PI-EPYAMT > WS-MAX-PMT-TOL) OR                           CL*94
03803            (PI-CPYAMT > WS-MAX-PMT-TOL)                              CL*94
03804             MOVE ER-0494         TO  EMI-ERROR                       CL*71
03805             PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT                CL*71
03806             GO TO 1500-EDIT-EXPENSES.                                CL*71
03807                                                                      CL*71
03808  1430-CONT-CHECK-AH-TOLERANCE.                                       CL*71
03809                                                                   EL156
03810      IF PI-EPYAMT = ZEROS OR   PI-EPYAMT NEGATIVE                 EL156
03811         GO TO 1500-EDIT-EXPENSES.                                 EL156
03812                                                                   EL156
03813      IF CM-O-B-COVERAGE                                           EL156
03814         GO TO 1500-EDIT-EXPENSES.                                 EL156
03815                                                                   EL156
03816      IF PI-COMPANY-ID = 'NCL'                                        CL*49
03817        IF FOUND-TOL-SW = 'Y'                                         CL*93
03818          IF WS-PMT-TOL NOT = ZERO                                    CL*93
03819            IF ((PI-CPYAMT - WS-PMT-TOL) > PI-EPYAMT) OR              CL*94
03820               ((PI-CPYAMT + WS-PMT-TOL) < PI-EPYAMT)                 CL*94
03821                   MOVE PI-PMTNOTE1 TO WS-NCL-PMT-OPTION              CL*93
03822                   IF WS-NCL-PMT-OPTION = SPACES                      CL*93
03823                       MOVE ER-0699 TO EMI-ERROR                      CL*93
03824                       PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT      CL*93
03825                   ELSE                                               CL*93
03826                   IF WS-NCL-PMT-OPTION ALPHABETIC-UPPER              CL*93
03827                       NEXT SENTENCE                                  CL*93
03828                   ELSE                                               CL*93
03829                   IF NOT WS-VALID-NCL-OPTION                         CL*93
03830                       MOVE ER-0699 TO EMI-ERROR                      CL*93
03831                       PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT.     CL*93
03832                                                                      CL*45
03833      IF WS-PMT-TOL NOT = ZERO
03834         IF ((PI-CPYAMT - WS-PMT-TOL) > PI-EPYAMT)
03835            or ((PI-CPYAMT + WS-PMT-TOL) < PI-EPYAMT)
03836            MOVE ER-0495          TO EMI-ERROR
03837            PERFORM 1490-PAYMENT-ERROR
                                       THRU 1490-EXIT
061013        end-if
061013     end-if
03838                                                                   EL156
03839      GO TO 1500-EDIT-EXPENSES.                                    EL156
03840                                                                   EL156
03841      EJECT                                                        EL156
03842  1440-COMPUTE-LIFE-REMAINING.                                     EL156
03843                                                                      CL*21
03844      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
03845         IF PI-PMTTYPE = '4'                                          CL*93
03846           MOVE PI-EPYAMT TO PI-CPYAMT                                CL*65
03847           GO TO 1460-TEST-TYPE.                                      CL*65
03848                                                                      CL*65
03849      IF WS-LF-COVERAGE-TYPE = 'L'                                    CL*88
03850         COMPUTE PI-CPYAMT = CM-LF-BENEFIT-AMT                        CL*93
03851                           - CL-TOTAL-PAID-AMT                        CL*93
03852         GO TO 1450-CHECK-MAX.                                     EL156
03853                                                                   EL156
03854      IF PI-LIFE-OVERRIDE-L1 = 'P' OR                                 CL*93
03855         PI-LF-COVERAGE-TYPE = 'P'                                    CL*93
03856           COMPUTE PI-CPYAMT = CM-LF-BENEFIT-AMT -                    CL*93
03857                               CM-LF-ITD-DEATH-AMT                    CL*93
03858           GO TO 1450-CHECK-MAX.                                      CL*93
03859                                                                      CL*25
03860      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.               EL156
03861      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.               CL*14
03862      MOVE CL-INCURRED-DT         TO CP-VALUATION-DT.              EL156
03863      MOVE CM-LF-ORIG-TERM        TO CP-ORIGINAL-TERM.             EL156
03864      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.                EL156
03865      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.          CL*38
03866      MOVE '4'                    TO CP-REM-TERM-METHOD.           EL156
03867      MOVE WS-EARNING-METHOD      TO CP-EARNING-METHOD.               CL*80
03868      MOVE WS-SPECIAL-CALC-CD     TO CP-SPECIAL-CALC-CD.              CL*80
03869                                                                      CL*21
03870      PERFORM 9800-LINK-REM-TERM.                                  EL156
03871                                                                   EL156
03872      MOVE WS-LF-COVERAGE-TYPE    TO CP-BENEFIT-TYPE.                 CL*21
03873      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.                EL156
03874      MOVE CM-LF-ORIG-TERM        TO CP-ORIGINAL-TERM.             EL156
03875      MOVE CM-LF-BENEFIT-AMT      TO CP-ORIGINAL-BENEFIT.          EL156
03876      MOVE CM-LF-ALT-BENEFIT-AMT  TO CP-ALTERNATE-BENEFIT.            CL*19
03877      MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM.            EL156
03878      MOVE CM-LOAN-APR            TO CP-LOAN-APR.                  EL156
03879      MOVE CM-LOAN-TERM           TO CP-LOAN-TERM.                    CL*94
03880      MOVE CM-PAY-FREQUENCY       TO CP-PAY-FREQUENCY.             EL156
03881      MOVE WS-STATE-ABBREV        TO CP-STATE-STD-ABBRV.              CL*80
03882      MOVE WS-EARNING-METHOD      TO CP-EARNING-METHOD.               CL*80
03883      MOVE WS-SPECIAL-CALC-CD     TO CP-SPECIAL-CALC-CD.              CL*80
CIDMOD     MOVE CM-RATE-CLASS          TO CP-CLASS-CODE
041710     MOVE CM-LF-BENEFIT-CD       TO CP-BENEFIT-CD.
041710     MOVE CM-PMT-EXTENSION-DAYS  TO CP-TERM-OR-EXT-DAYS.
041710     MOVE 'Y'                    TO CP-LF-CLAIM-CALC-SW.

022122     move zeros                  to ws-max-tot-ben
022122                                    ws-work-ben-pct
120115     IF PI-COMPANY-ID = 'DCC' or 'VPP'
061013        MOVE PI-COMPANY-CD       TO ACCT-COMP-CD      
061013        MOVE PI-CARRIER          TO ACCT-CARRIER      
061013        MOVE PI-GROUPING         TO ACCT-GROUPING     
061013        MOVE PI-STATE            TO ACCT-STATE        
061013        MOVE PI-ACCOUNT          TO ACCT-ACCOUNT      
061013        MOVE PI-CERT-EFF-DT      TO ACCT-EXP-DT
061013        MOVE ERACCT-PARTIAL-KEY  TO WS-ERACCT-SAVE-KEY
061013        MOVE SPACES              TO WS-ERACCT-HOLD-RECORD
061013
061013        EXEC CICS READ
061013           DATASET ('ERACCT')
061013           RIDFLD  (ERACCT-KEY)
061013           SET     (ADDRESS OF ACCOUNT-MASTER)
061013           GTEQ
061013           resp    (WS-RESPONSE)
061013        END-EXEC
061013        IF WS-RESP-NORMAL
061013           AND (WS-ERACCT-SAVE-KEY = AM-CONTROL-PRIMARY (1:20))
061013           and (pi-cert-eff-dt < am-expiration-dt)
061013           and (pi-cert-eff-dt >= am-effective-dt)
061013           move am-dcc-product-code to pi-dcc-product-code
061013        else
061013           move spaces           to pi-dcc-product-code
061013        end-if
061013        if pi-dcc-product-code not = spaces
061013           move cm-ah-benefit-cd to pi-ah-benefit-cd
                 move cm-lf-benefit-cd to pi-lf-benefit-cd
061013           PERFORM 0900-GET-DDF-limits
061013                                 THRU 0900-EXIT
022122           move cl-insured-birth-dt to dc-bin-date-1
022122           move cl-incurred-dt to dc-bin-date-2
022122           move '1' to dc-option-code
022122           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
022122           compute ws-att-age =
022122              dc-elapsed-months / 12
022122
022122           move zeros to dc-elapsed-months dc-elapsed-days

061013           IF PDEF-FOUND
061013              PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122                 (P1 > +11)
022122                 OR (PD-PROD-CODE (P1) = cl-claim-type
022122                   AND PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
061013              END-PERFORM
022122              IF P1 < +12
061013                 MOVE PD-MAX-AMT (P1)
022122                                 TO ws-MAX-TOT-BEN
022122                 if pd-ben-pct (p1) not numeric
022122                    move zeros   to pd-ben-pct (p1)
022122                 end-if
022122                 if pd-ben-pct (p1) = zeros
022122                    move +1      to ws-work-ben-pct
022122                 else
022122                    move pd-ben-pct (p1)
022122                                 to ws-work-ben-pct
022122                 end-if
                    else
                       move er-1674    to emi-error
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
061013              END-IF
                 else
                    move er-1672       to emi-error
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
061013           END-IF
061013        end-if
061013     END-IF

03885      EXEC CICS LINK                                               EL156
03886         PROGRAM  (LINK-REMAMT)                                    EL156
03887         COMMAREA (CALCULATION-PASS-AREA)                          EL156
03888         LENGTH   (CP-COMM-LENGTH)                                 EL156
03889      END-EXEC.                                                       CL*88

022122     if ws-work-ben-pct <> 0
022122        compute cp-remaining-amt =
022122           cp-remaining-amt * ws-work-ben-pct
022122     end-if
022122     if ws-max-tot-ben <> 0
022122        if cp-remaining-amt > ws-max-tot-ben
022122           move ws-max-tot-ben to cp-remaining-amt
022122        end-if
022122     end-if
03890                                                                   EL156
03891      COMPUTE PI-CPYAMT = CP-REMAINING-AMT - CL-TOTAL-PAID-AMT.    EL156
03892                                                                   EL156
041710     MOVE ' '                    TO CP-LF-CLAIM-CALC-SW.
041710     MOVE CP-SCNP-6MO-AMT        TO PI-MO-BEN-AMT.
041710 
03893      IF PI-COMPANY-ID = 'BOA' AND                                    CL*93
03894         PI-PMTTYPE    = '2'   AND                                    CL*93
03895         PI-OFFLINE    = 'N'                                          CL*93
03896            GO TO 1441-COMPUTE-BOA-INTEREST.                          CL*79
03897                                                                      CL*58
03898      IF PI-COMPANY-ID = 'NCL' AND                                    CL*79
03899         PI-PMTTYPE    = '2'   AND                                    CL*79
03900         PI-OFFLINE    = 'N'                                          CL*79
03901           GO TO 1442-COMPUTE-NCL-INTEREST                            CL*79
03902        ELSE                                                          CL*79
03903           GO TO 1445-CONTINUE.                                       CL*79
03904                                                                      CL*79
03905  1441-COMPUTE-BOA-INTEREST.                                          CL*79
03906      MOVE +0             TO WS-CALC-INT.                             CL*93
03907                                                                      CL*57
03908      MOVE CL-INCURRED-DT TO DC-BIN-DATE-1.                           CL*57
03909      MOVE WS-TODAY-DATE  TO DC-BIN-DATE-2.                           CL*93
03910      MOVE +0             TO DC-ELAPSED-MONTHS                        CL*57
03911                             DC-ELAPSED-DAYS.                         CL*57
03912      MOVE '1'            TO DC-OPTION-CODE.                          CL*93
03913      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*57
03914      IF NO-CONVERSION-ERROR                                          CL*57
03915          NEXT SENTENCE                                               CL*57
03916      ELSE                                                            CL*57
03917          GO TO 1445-CONTINUE.                                        CL*57
03918                                                                      CL*57
03919      IF (PI-INT-RATE NOT > ZEROS)  AND                               CL*94
03920         (DC-ELAPSED-DAYS > +30)                                      CL*94
03921          MOVE ER-3531            TO EMI-ERROR                        CL*58
03922          MOVE -1                 TO PINTL                            CL*58
03923          MOVE AL-UNBON           TO PINTA                            CL*58
03924          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*58
03925                                                                      CL*58
03926      IF PI-EPYAMT NOT > ZEROS                                        CL*94
03927          GO TO 1445-CONTINUE.                                        CL*57
03928                                                                      CL*57
03929      IF (PI-INT-RATE NOT > ZEROS)  OR                                CL*94
03930         (DC-ELAPSED-DAYS NOT > +30)                                  CL*94
03931          MOVE PI-EPYAMT TO PI-CPYAMT                                 CL*58
03932          GO TO 1441-FORMAT-NOTE.                                     CL*79
03933                                                                      CL*57
03934      COMPUTE WS-CALC-INT ROUNDED =  PI-EPYAMT                        CL*58
03935                                  *  PI-INT-RATE                      CL*58
03936                                  *  DC-ELAPSED-DAYS                  CL*58
03937                                  /  365                              CL*58
03938                                  /  100.                             CL*58
03939                                                                      CL*58
03940      COMPUTE PI-CPYAMT = PI-EPYAMT + WS-CALC-INT.                    CL*58
03941                                                                      CL*58
03942  1441-FORMAT-NOTE.                                                   CL*79
03943                                                                      CL*58
03944      MOVE SPACES        TO WS-BOANOTE1.                              CL*93
03945      MOVE 'BENEFIT -  ' TO WS-BOANOTE1-DESC.                         CL*58
03946      MOVE PI-EPYAMT     TO WS-BOANOTE1-EPYAMT.                       CL*93
03947      MOVE WS-BOANOTE1   TO WS-PMTNOTE1 PI-PMTNOTE1 NOTE1I.           CL*93
03948      MOVE AL-UANON      TO NOTE1A.                                   CL*93
03949                                                                      CL*58
03950      IF WS-CALC-INT > ZEROS                                          CL*94
03951          MOVE SPACES        TO WS-BOANOTE2                           CL*93
03952          MOVE 'INTEREST - ' TO WS-BOANOTE2-DESC                      CL*58
03953          MOVE WS-CALC-INT   TO WS-BOANOTE2-INT                       CL*93
03954          MOVE WS-BOANOTE2   TO WS-PMTNOTE2 PI-PMTNOTE2 NOTE2I        CL*93
03955          MOVE AL-UANON      TO NOTE2A.                               CL*93
03956                                                                      CL*57
03957      GO TO 1445-CONTINUE.                                            CL*79
03958                                                                      CL*79
03959  1442-COMPUTE-NCL-INTEREST.                                          CL*79
03960      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                    CL*79
03961      MOVE PI-STATE               TO WS-ST-ACCESS.                    CL*79
03962      MOVE WS-STATE-ACCESS        TO CNTL-ACCESS.                     CL*79
03963      MOVE '3'                    TO CNTL-REC-TYPE.                   CL*79
03964      MOVE +0                     TO CNTL-SEQ-NO.                     CL*79
03965      MOVE 'STAT'                 TO FILE-SWITCH.                     CL*79
03966                                                                      CL*79
03967      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.                       CL*79
03968                                                                      CL*79
03969      MOVE +0                 TO WS-CALC-INT.                         CL*79
03970                                                                      CL*79
03971      IF ST-STAT-FROM-INCURRED                                        CL*79
03972          MOVE CL-INCURRED-DT TO WS-COMPUTE-FROM-DT                   CL*79
03973        ELSE                                                          CL*79
03974      IF ST-STAT-FROM-REPORTED                                        CL*79
03975          MOVE CL-REPORTED-DT TO WS-COMPUTE-FROM-DT                   CL*79
03976        ELSE                                                          CL*79
03977          GO TO 1445-CONTINUE.                                        CL*79
03978                                                                      CL*79
03979      MOVE WS-COMPUTE-FROM-DT TO DC-BIN-DATE-1.                       CL*79
03980      MOVE WS-TODAY-DATE      TO DC-BIN-DATE-2.                       CL*79
03981      MOVE +0                 TO DC-ELAPSED-MONTHS                    CL*79
03982                                 DC-ELAPSED-DAYS.                     CL*79
03983      MOVE '1'                TO DC-OPTION-CODE.                      CL*79
03984      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*79
03985      IF NO-CONVERSION-ERROR                                          CL*79
03986          NEXT SENTENCE                                               CL*79
03987      ELSE                                                            CL*79
03988          GO TO 1445-CONTINUE.                                        CL*79
03989                                                                      CL*79
03990      IF (CF-ST-STAT-INTEREST NOT > ZEROS) OR                         CL*94
03991         (DC-ELAPSED-DAYS NOT > CF-ST-NO-DAYS-ELAPSED)                CL*94
03992          GO TO 1445-CONTINUE.                                        CL*79
03993                                                                      CL*79
03994      COMPUTE WS-CALC-INT ROUNDED =  PI-CPYAMT                        CL*79
03995                                  *  CF-ST-STAT-INTEREST.             CL*79
03996                                                                      CL*79
03997      MOVE SPACES              TO WS-NCLNOTE1.                        CL*79
03998      MOVE 'BENEFIT -  '       TO WS-NCLNOTE1-DESC.                   CL*79
03999      MOVE PI-CPYAMT           TO WS-NCLNOTE1-CPYAMT.                 CL*79
04000      MOVE WS-NCLNOTE1         TO WS-PMTNOTE1                         CL*79
04001                                  PI-PMTNOTE1                         CL*79
04002                                  NOTE1I.                             CL*94
04003      MOVE AL-UANON            TO NOTE1A.                             CL*79
04004                                                                      CL*79
04005      IF WS-CALC-INT > ZEROS                                          CL*94
04006          MOVE SPACES          TO WS-NCLNOTE2                         CL*79
04007          MOVE 'INTEREST - '   TO WS-NCLNOTE2-DESC                    CL*79
04008          MOVE WS-CALC-INT     TO WS-NCLNOTE2-INT                     CL*79
04009          MOVE WS-NCLNOTE2     TO WS-PMTNOTE2                         CL*79
04010                                  PI-PMTNOTE2                         CL*79
04011                                  NOTE2I                              CL*79
04012          MOVE AL-UANON        TO NOTE2A.                             CL*79
04013                                                                      CL*79
04014      COMPUTE PI-CPYAMT = PI-CPYAMT + WS-CALC-INT.                    CL*79
04015                                                                      CL*79
04016  1445-CONTINUE.                                                      CL*57
04017                                                                      CL*57
04018      IF PI-EPYAMT = ZEROS OR PI-PMTTYPE = '4'                     EL156
04019         GO TO 1460-TEST-TYPE.                                        CL*45
04020                                                                   EL156
04021      IF CM-O-B-COVERAGE                                           EL156
04022         GO TO 1460-TEST-TYPE.                                        CL*45
04023                                                                      CL*58
04024      IF PI-COMPANY-ID = 'BOA' OR 'NCL'                               CL*93
04025          GO TO 1450-CHECK-MAX.                                       CL*58
04026                                                                   EL156
120115     if (pi-company-id = 'DCC' or 'VPP')
061013        and (pdef-found)
100314        if pi-epyamt > pd-max-amt (p1)
100314           move er-1657          to emi-error
100314           perform 1490-payment-error
100314                                 thru 1490-exit
100314           go to 1460-test-type
100314        end-if
061013     else
04027         IF (PI-CPYAMT - (CP-REMAINING-AMT-PRV - CP-REMAINING-AMT)
04028            > PI-EPYAMT)
04029                          OR
04030            (PI-CPYAMT + (CP-REMAINING-AMT-PRV - CP-REMAINING-AMT)
04031            < PI-EPYAMT)
04032            MOVE ER-0594          TO EMI-ERROR
04033            PERFORM 1490-PAYMENT-ERROR
                                       THRU 1490-EXIT
04034            GO TO 1460-TEST-TYPE
              end-if
           end-if

           .
04036  1450-CHECK-MAX.                                                  EL156
04037                                                                      CL*25
04038      IF PI-LIFE-OVERRIDE-L1 = 'P' OR                                 CL*88
04039         PI-LF-COVERAGE-TYPE = 'P'                                    CL*88
04040          COMPUTE WS-BEN-AMT-LEFT = CM-LF-BENEFIT-AMT -               CL*25
04041                                    CM-LF-ITD-DEATH-AMT               CL*25
04042      ELSE                                                            CL*25
04043          COMPUTE WS-BEN-AMT-LEFT = CM-LF-BENEFIT-AMT -               CL*25
04044                                    CL-TOTAL-PAID-AMT.                CL*25
04045                                                                      CL*25
04046      IF PI-EPYAMT > WS-BEN-AMT-LEFT                                  CL*94
04047           MOVE ER-0543           TO EMI-ERROR                     EL156
04048           PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT.              EL156
04049                                                                   EL156
04050  1460-TEST-TYPE.                                                     CL*45
04051                                                                      CL*10
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
100518        IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
030514           COMPUTE WS-LF-BEN-AMT = CM-LF-BENEFIT-AMT +
030514                                   CM-LF-ALT-BENEFIT-AMT -
030514                                   CL-TOTAL-PAID-AMT
030514           IF PI-EPYAMT > WS-LF-BEN-AMT
030514               MOVE ER-0543      TO EMI-ERROR
030514               PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT
030514           END-IF
030514        END-IF
030514     END-IF
030514
04052      IF PI-PMT-APPR-SW = 'G'                                         CL*88
04053        NEXT SENTENCE                                                 CL*47
04054      ELSE                                                            CL*47
04055        IF WS-MAX-LF-PMT-TOL NOT = ZERO                               CL*93
04056          IF PI-EPYAMT = ZERO                                         CL*88
04057              IF PI-CPYAMT > WS-MAX-LF-PMT-TOL                        CL*94
04058                  MOVE ER-0494    TO  EMI-ERROR                       CL*40
04059                  PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT           CL*40
04060                  GO TO 1500-EDIT-EXPENSES                            CL*40
04061              ELSE                                                    CL*40
04062                  NEXT SENTENCE                                       CL*40
04063          ELSE                                                        CL*40
04064              IF PI-EPYAMT > WS-MAX-LF-PMT-TOL                        CL*94
04065                  MOVE ER-0494    TO  EMI-ERROR                       CL*40
04066                  PERFORM 1490-PAYMENT-ERROR THRU 1490-EXIT           CL*40
04067                  GO TO 1500-EDIT-EXPENSES.                           CL*40
04068                                                                      CL*10
04069      IF PI-PMTTYPE NOT = '2' OR                                      CL*25
04070         (WS-LF-COVERAGE-TYPE = 'L' OR 'P')                           CL*80
04071          GO TO 1500-EDIT-EXPENSES.                                   CL*25
04072                                                                   EL156
04073      MOVE CP-REMAINING-AMT-PRV   TO WS-EDIT-PATTERN.              EL156
04074      MOVE ER-0526                TO EMI-ERROR.                    EL156
04075      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
04076                                                                   EL156
04077      IF EMI-ERROR-NUMBER (1) = '0526'                             EL156
04078         MOVE WS-EDIT             TO EMI-TEXT-VARIABLE (1).        EL156
04079                                                                   EL156
04080      IF EMI-ERROR-NUMBER (2) = '0526'                             EL156
04081         MOVE WS-EDIT             TO EMI-TEXT-VARIABLE (2).        EL156
04082                                                                   EL156
04083      GO TO 1500-EDIT-EXPENSES.                                    EL156
04084                                                                      CL*45
04085  1490-PAYMENT-ERROR.                                              EL156
04086      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
04087                                                                   EL156
04088      IF EMI-MESSAGE-FORMATTED                                     EL156
04089          MOVE -1                 TO EPYAMTL                       EL156
04090          MOVE AL-UNBON           TO EPYAMTA.                      EL156
04091                                                                   EL156
04092  1490-EXIT.                                                       EL156
04093       EXIT.                                                       EL156
04094      EJECT                                                        EL156
04095  1500-EDIT-EXPENSES.                                              EL156
04096      IF PI-COMPANY-ID = 'MET' OR 'AIG' OR 'AUK' OR 'DMD'             CL*98
04097          GO TO 1500-SKIP-ERROR-0556.                              EL156
04098                                                                      CL*82
04099  1500-CONTINUE-ON.                                                   CL*82
04100                                                                      CL*82
04101      IF CM-O-B-COVERAGE                                           EL156
04102         MOVE ER-0556             TO EMI-ERROR                     EL156
04103         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL156
04104                                                                   EL156
04105  1500-SKIP-ERROR-0556.                                            EL156
04106      IF PI-PMTTYPE = '4' OR '5' OR '6'                               CL*65
04107         IF PI-EEXPENS NOT = ZEROS                                 EL156
04108            MOVE ER-0524          TO EMI-ERROR                     EL156
04109            GO TO 1590-EXPENSE-ERROR                               EL156
04110         ELSE                                                      EL156
04111            GO TO 1600-EDIT-RESERVES.                                 CL*47
04112                                                                   EL156
04113      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
04114      MOVE '6'                    TO CNTL-REC-TYPE.                EL156
04115                                                                   EL156
04116      IF CONTROL-IS-ACTUAL-CARRIER                                 EL156
04117          MOVE PI-CARRIER         TO WS-CARR                       EL156
04118      ELSE                                                         EL156
04119          MOVE PI-CARRIER-CONTROL-LEVEL TO WS-CARR.                EL156
04120                                                                   EL156
04121      MOVE WS-CARR-ACCESS         TO CNTL-ACCESS.                  EL156
04122      MOVE +0                     TO CNTL-SEQ-NO.                  EL156
04123      MOVE 'CARR'                 TO FILE-SWITCH.                  EL156
04124                                                                   EL156
04125      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.                    EL156
04126                                                                   EL156
04127      IF EXPENSE-CALC-MANUAL                                       EL156
04128         GO TO 1600-EDIT-RESERVES.                                 EL156
04129                                                                   EL156
04130      IF DOLLARS-PER-PMT                                           EL156
04131         MOVE CF-EXPENSE-DOLLAR   TO PI-CEXPENS                    EL156
04132         GO TO 1600-EDIT-RESERVES.                                 EL156
04133                                                                   EL156
04134      IF DOLLARS-PER-MONTH AND
121802        (CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122                                           OR 'B' or 'H')
04136        COMPUTE PI-CEXPENS = CF-EXPENSE-DOLLAR * EXPENSE-MONTH-SAVEEL156
04137        GO TO 1600-EDIT-RESERVES.                                  EL156
04138                                                                   EL156
04139      IF PERCENT-OF-PAYMENT                                        EL156
04140         COMPUTE PI-CEXPENS = PI-CPYAMT * CF-EXPENSE-PERCENT.      EL156
04141                                                                   EL156
04142      GO TO 1600-EDIT-RESERVES.                                    EL156
04143                                                                   EL156
04144  1590-EXPENSE-ERROR.                                              EL156
04145      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
04146                                                                   EL156
04147      IF EMI-MESSAGE-FORMATTED                                     EL156
04148         MOVE -1                  TO EEXPENSL                      EL156
04149         MOVE AL-UNBON            TO EEXPENSA.                     EL156
04150                                                                   EL156
04151      EJECT                                                        EL156
04152  1600-EDIT-RESERVES.                                              EL156
04153      IF PI-PMTTYPE = '4' OR '5' OR '6'                               CL*65
04154         IF PI-ERESV = ZEROS                                       EL156
04155            GO TO 1700-EDIT-EXPENSE-TYPE                           EL156
04156         ELSE                                                      EL156
04157            MOVE ER-0525          TO EMI-ERROR                     EL156
04158            PERFORM 1690-RESERVE-ERROR THRU 1690-EXIT              EL156
04159            GO TO 1700-EDIT-EXPENSE-TYPE.                          EL156
04160                                                                   EL156
04161      MOVE ELMSTR-KEY             TO ELTRLR-KEY.                   EL156
04162      MOVE ZEROS                  TO TRLR-SEQ-NO.                  EL156
04163      PERFORM 7950-READ-TRAILER THRU 7950-EXIT.                    EL156
04164                                                                   EL156
04165      IF NOT AT-MANUAL-RESERVES-USED                               EL156
04166         IF PI-ERESV NOT = ZEROS                                   EL156
04167            MOVE ER-0518          TO EMI-ERROR                     EL156
04168            PERFORM 1690-RESERVE-ERROR THRU 1690-EXIT.             EL156
04169                                                                   EL156
04170      IF AT-MANUAL-RESERVES-USED                                   EL156
04171         MOVE AT-CURRENT-MANUAL-RESERVE  TO PI-CRESV               EL156
04172         GO TO 1700-EDIT-EXPENSE-TYPE.                             EL156
04173                                                                   EL156
04174      IF AT-FUTURE-RESERVES-USED                                   EL156
04175         ADD AT-CURRENT-MANUAL-RESERVE   TO WS-RESERVE-WORK.       EL156
04176                                                                   EL156
04177      IF AT-FUTURE-RESERVES-USED                                   EL156
04178         ADD AT-FUTURE-RESERVE    TO WS-RESERVE-WORK.              EL156
04179                                                                   EL156
04180      IF AT-PAY-TO-CURRENT-USED OR AT-LF-PTC-USED                  EL156
04181         ADD AT-PAY-CURRENT-RESERVE TO WS-RESERVE-WORK.            EL156
04182                                                                   EL156
04183      IF AT-IBNR-RESERVES-USED                                     EL156
04184         ADD AT-IBNR-RESERVE      TO WS-RESERVE-WORK.              EL156
04185                                                                   EL156
04186      MOVE WS-RESERVE-WORK        TO PI-CRESV.                     EL156
04187      GO TO 1700-EDIT-EXPENSE-TYPE.                                EL156
04188                                                                   EL156
04189  1690-RESERVE-ERROR.                                              EL156
04190      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
04191                                                                   EL156
04192      IF EMI-MESSAGE-FORMATTED                                     EL156
04193         MOVE AL-UNBON            TO ERESVA                        EL156
04194         MOVE -1                  TO ERESVL.                       EL156
04195                                                                   EL156
04196  1690-EXIT.                                                       EL156
04197       EXIT.                                                       EL156
04198                                                                   EL156
04199     EJECT                                                         EL156
04200  1700-EDIT-EXPENSE-TYPE.                                          EL156
04201      IF PI-PMTTYPE NOT = '5' AND '6'                              EL156
04202         GO TO 2000-EDIT-DONE.                                     EL156
04203                                                                   EL156
04204      IF PI-ETYPE = ' ' OR '1' OR '2' OR '3' OR '4' OR '5' OR      EL156
04205                    '6' OR '7' OR '8' OR '9'                       EL156
04206         GO TO 2000-EDIT-DONE.                                     EL156
04207                                                                   EL156
04208  1705-EXPENSE-ERROR.                                              EL156
04209      MOVE ER-0573                TO EMI-ERROR.                    EL156
04210      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*94
04211                                                                   EL156
04212      IF EMI-MESSAGE-FORMATTED                                     EL156
04213         MOVE AL-UABON            TO ETYPEA                        EL156
04214         MOVE -1                  TO ETYPEL.                       EL156
04215                                                                   EL156
04216      GO TO 2000-EDIT-DONE.                                        EL156
04217      EJECT                                                        EL156
04218  1950-NOT-FOUND.                                                  EL156
04219      IF FILE-SWITCH = 'MSTR'                                      EL156
04220         MOVE ER-0204             TO EMI-ERROR.                    EL156
04221                                                                   EL156
04222      IF FILE-SWITCH = 'CARR'                                      EL156
04223         MOVE ER-0252             TO EMI-ERROR.                    EL156
04224                                                                   EL156
04225      IF FILE-SWITCH = 'COMP'                                      EL156
04226         MOVE ER-0254             TO EMI-ERROR.                    EL156
04227                                                                   EL156
04228      IF FILE-SWITCH = 'STAT'                                      EL156
04229         MOVE ER-0149             TO EMI-ERROR.                    EL156
04230                                                                   EL156
04231      IF FILE-SWITCH = 'PROC'                                      EL156
04232         MOVE ER-0019             TO EMI-ERROR.                    EL156
04233                                                                   EL156
04234      IF FILE-SWITCH = 'ACCT'                                      EL156
04235         MOVE ER-0198             TO EMI-ERROR.                    EL156

090821     if eracct-browse-started
090821        exec cics endbr
090821           dataset('ERACCT')
090821        end-exec
090821        move spaces              to ws-eracct-startbr-ind
090821     end-if

04237      IF FILE-SWITCH = 'BENE'                                      EL156
100518        IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'             EL156
04239            MOVE ER-0282          TO EMI-ERROR                     EL156
04240         ELSE                                                      EL156
04241            MOVE ER-0283          TO EMI-ERROR.                    EL156
04242                                                                   EL156
04243      MOVE -1                     TO PMTTYPEL.                     EL156
04244      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
04245                                                                   EL156
04246      MOVE EMI-FORCABLE-CTR       TO  PI-FORCE-COUNT                  CL*35
04247                                      WS-FORCE-CTR.                   CL*35
04248                                                                      CL*35
04249      MOVE EMI-FATAL-CTR          TO  PI-FATAL-COUNT                  CL*35
04250                                      WS-FATAL-CTR.                   CL*35
04251                                                                      CL*35
04252      GO TO 8200-SEND-DATAONLY.                                    EL156
04253                                                                   EL156
04254      EJECT                                                        EL156
04255  2000-EDIT-DONE.                                                  EL156

061013     perform 2210-dcc-max-benes thru 2210-exit

120115     if pi-company-id not = 'DCC' and 'VPP'
              IF (WS-SPECIAL-CALC-CD = 'C')
                 and (cl-critical-period > 0)
092310           IF CM-AH-BENEFIT-AMT NOT = ZERO
092310              IF PI-EPYAMT > ZEROS
092310                 COMPUTE ws-benefits-paid = (CL-TOTAL-PAID-AMT +
092310                    PI-EPYAMT) / CM-AH-BENEFIT-AMT
092310              ELSE
092310                 COMPUTE ws-benefits-paid = (CL-TOTAL-PAID-AMT +
092310                    PI-CPYAMT) / CM-AH-BENEFIT-AMT
092310              END-IF
                    if ws-benefits-paid > cl-critical-period
092310                 MOVE ER-0657          TO EMI-ERROR
092310                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
092310              END-IF
092310           END-IF
              END-IF
           end-if

04256      IF PI-HOLDTIL NOT = ZEROS                                    EL156
04257         MOVE PI-HOLDTIL          TO HOLDTILO                      EL156
04258         INSPECT HOLDTILO REPLACING ALL SPACES BY '/'                 CL*16
082218        MOVE ER-3272          TO EMI-ERROR
082218        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
082218     END-IF
04259                                                                   EL156
04260      IF PI-EPYFROM NOT = ZEROS                                    EL156
04261         MOVE PI-EPYFROM          TO EPYFROMO                      EL156
04262         INSPECT EPYFROMI REPLACING ALL SPACES BY '/'.                CL*16
04263                                                                   EL156
04264      IF PI-EPYTHRU NOT = ZEROS                                    EL156
04265         MOVE PI-EPYTHRU            TO EPYTHRUO                       CL*93
04266         INSPECT EPYTHRUI REPLACING ALL SPACES BY '/'                 CL*16
04267         IF PI-USES-PAID-TO                                           CL*16
04268            MOVE PI-EPYTHRU         TO DC-GREG-DATE-1-MDY             CL*16
04269            MOVE '4'                TO DC-OPTION-CODE                 CL*16
04270            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             CL*16
04271            IF NO-CONVERSION-ERROR                                    CL*16
04272               MOVE +1              TO DC-ELAPSED-DAYS                CL*93
04273               MOVE +0              TO DC-ELAPSED-MONTHS              CL*93
04274               MOVE '6'             TO DC-OPTION-CODE                 CL*93
04275               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT          CL*16
04276               IF NO-CONVERSION-ERROR                                 CL*16
04277                  MOVE DC-GREG-DATE-1-EDIT TO EPYTHRUI.               CL*16
04278                                                                   EL156
04279      MOVE PI-EDAYS               TO EDAYSO.                       EL156
04280                                                                      CL*93
04281      IF PI-AIGFROM NOT = ZEROS                                       CL*93
04282         MOVE PI-AIGFROM          TO AIGFROMO                         CL*65
04283         INSPECT AIGFROMO REPLACING ALL SPACES BY '/'.                CL*65
04284                                                                      CL*82
04285      MOVE PI-EPYAMT              TO EPYAMTO.                      EL156
04286      MOVE PI-ERESV               TO ERESVO.                       EL156
04287      MOVE PI-EEXPENS             TO EEXPENSO.                     EL156
04288                                                                   EL156
04289      MOVE PI-CPYFROM             TO DC-BIN-DATE-1.                EL156
04290      MOVE SPACES                 TO DC-OPTION-CODE.               EL156
04291      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
04292                                                                   EL156
04293      IF DATE-CONVERSION-ERROR                                     EL156
04294         MOVE SPACES              TO CPYFROMI                      EL156
04295      ELSE                                                         EL156
04296         MOVE DC-GREG-DATE-1-EDIT TO CPYFROMI                      EL156
04297         IF PI-EPYFROM = ZEROS                                     EL156
04298            MOVE AL-UANON             TO EPYFROMA                     CL*93
04299            MOVE DC-GREG-DATE-1-MDY   TO PI-EPYFROM                EL156
04300            MOVE DC-GREG-DATE-1-EDIT  TO EPYFROMI.                 EL156
04301                                                                   EL156
04302      IF PI-USES-PAID-TO                                              CL*16
04303         MOVE PI-CPYTHRU                    TO DC-BIN-DATE-1          CL*50
04304         MOVE +1                            TO DC-ELAPSED-DAYS        CL*50
04305         MOVE +0                            TO DC-ELAPSED-MONTHS      CL*50
04306         MOVE '6'                           TO DC-OPTION-CODE         CL*50
04307         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                CL*16
04308         IF NO-CONVERSION-ERROR                                       CL*16
04309            MOVE DC-GREG-DATE-1-EDIT        TO CPYTHRUI               CL*50
04310            IF PI-EPYTHRU = ZEROS                                     CL*16
04311               MOVE AL-UANON                TO EPYTHRUA               CL*50
04312               MOVE DC-GREG-DATE-1-EDIT     TO EPYTHRUI               CL*50
04313               MOVE ' '                     TO DC-OPTION-CODE         CL*50
04314               MOVE +0                      TO DC-ELAPSED-DAYS        CL*50
04315                                               DC-ELAPSED-MONTHS      CL*50
04316               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT          CL*50
04317               IF NO-CONVERSION-ERROR                                 CL*50
04318                   MOVE DC-GREG-DATE-1-MDY  TO  PI-EPYTHRU            CL*50
04319                   GO TO 2001-CONTINUE                                CL*50
04320               ELSE                                                   CL*50
04321                   GO TO 2001-CONTINUE                                CL*50
04322            ELSE                                                      CL*16
04323               GO TO 2001-CONTINUE.                                   CL*16
04324                                                                      CL*16
04325      MOVE PI-CPYTHRU             TO DC-BIN-DATE-1.                EL156
04326      MOVE SPACES                 TO DC-OPTION-CODE.               EL156
04327      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
04328                                                                   EL156
04329      IF DATE-CONVERSION-ERROR                                     EL156
04330         MOVE SPACES              TO CPYTHRUI                      EL156
04331      ELSE                                                            CL*16
04332         MOVE DC-GREG-DATE-1-EDIT TO CPYTHRUI                      EL156
04333         IF PI-EPYTHRU = ZEROS                                     EL156
04334            MOVE AL-UANON TO EPYTHRUA                              EL156
04335            MOVE DC-GREG-DATE-1-MDY   TO PI-EPYTHRU                EL156
04336            MOVE DC-GREG-DATE-1-EDIT  TO EPYTHRUI.                 EL156
04337                                                                      CL*16
04338  2001-CONTINUE.                                                      CL*16
04339                                                                   EL156
04340      MOVE PI-CDAYS               TO CDAYSO.                       EL156
04341                                                                   EL156
04342      IF PI-EDAYS = ZEROS                                          EL156
04343         MOVE AL-UNNON TO EDAYSA                                   EL156
04344         MOVE PI-CDAYS            TO EDAYSO PI-EDAYS.              EL156
04345                                                                   EL156
04346      MOVE PI-CPYAMT              TO CPYAMTO.                      EL156
04347                                                                   EL156
04348      IF PI-EPYAMT = ZEROS                                         EL156
04349         MOVE AL-UNNON            TO EPYAMTA                          CL*94
04350         MOVE PI-CPYAMT           TO EPYAMTO  PI-EPYAMT.           EL156
04351                                                                   EL156
04352      MOVE PI-CRESV               TO CRESVO.                       EL156
04353      MOVE PI-CEXPENS             TO CEXPENSO.                     EL156
04354                                                                   EL156
04355      IF PI-EEXPENS = ZEROS                                        EL156
04356         MOVE AL-UNNON TO EEXPENSA                                 EL156
04357         MOVE PI-CEXPENS          TO EEXPENSO  PI-EEXPENS.         EL156
04358                                                                   EL156
062121     IF (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
022106        AND (CL-CLAIM-TYPE = 'L')
110807        AND (PI-OFFLINE NOT EQUAL 'Y')
022106        IF PI-PMTTYPE = '2' OR '4'
022106           PERFORM 2200-CALC-INTEREST
022106                                 THRU 2200-EXIT
022106        END-IF
022106     END-IF

04359      MOVE EMI-FORCABLE-CTR       TO PI-FORCE-COUNT WS-FORCE-CTR.  EL156
04360      MOVE EMI-FATAL-CTR          TO PI-FATAL-COUNT WS-FATAL-CTR.  EL156
04361      MOVE LAST-ERROR-LINE        TO ERRMSG0O.                     EL156
04362                                                                   EL156
04363      IF EMI-FATAL-CTR = ZEROS                                     EL156
04364         MOVE 'B'                 TO PI-PASS-SW.                   EL156
04365                                                                   EL156
04366      IF NOT EMI-MESSAGE-FORMATTED                                 EL156
04367          MOVE -1                 TO EPYFROML                      EL156
04368      ELSE                                                         EL156
04369          IF EMI-ERROR-NUMBER (1) = '0526' AND                        CL*93
04370             EMI-ERROR-NUMBER (2) = SPACES                            CL*93
04371              MOVE -1             TO EPYFROML.                     EL156
04372                                                                   EL156

04373      GO TO 8200-SEND-DATAONLY.                                    EL156
04374                                                                   EL156
04375      EJECT                                                        EL156
       2200-CALC-INTEREST.

071806     IF PI-PROOF-DATE NOT = ZEROS AND LOW-VALUES
071806        MOVE PI-PROOF-DATE       TO DC-GREG-DATE-1-MDY
071806        MOVE '4'                 TO DC-OPTION-CODE
071806        PERFORM 9700-LINK-DATE-CONVERT
071806        IF DATE-CONVERSION-ERROR
071806            MOVE LOW-VALUES      TO CP-PRF-DT
071806        ELSE
071806           MOVE DC-BIN-DATE-1    TO CP-PRF-DT
071806        END-IF
           END-IF

120115     if (pi-int-to-rem-borr <> 'Y' and 'N')
120115        if pi-rb-joint-coverage
120115           move 'Y'              to pi-int-to-rem-borr
120115        end-if
120115     end-if
           MOVE PI-COMPANY-CD          TO CP-COMPANY-CD
              IN CLAIM-INT-PASS-AREA
           MOVE CL-CERT-STATE          TO CP-STATE
              IN CLAIM-INT-PASS-AREA
           MOVE CM-LF-BENEFIT-CD       TO CP-PRODUCT
           MOVE 'LF'                   TO CP-COVERAGE
           MOVE CL-INCURRED-DT         TO CP-INC-DT
           MOVE CL-FILE-ESTABLISH-DT   TO CP-EST-DT
           MOVE LOW-VALUES             TO CP-LSTPD-DT
           MOVE CL-REPORTED-DT         TO CP-RPT-DT
           MOVE CL-CERT-EFF-DT         TO CP-EFF-DT
           MOVE PI-EPYAMT              TO CP-CLAIM-AMT
           
           EXEC CICS LINK
              PROGRAM   ('ELCLMI')
              COMMAREA  (CLAIM-INT-PASS-AREA)
              LENGTH    (CP-CLAIM-LENGTH)
           END-EXEC

           MOVE AL-PANOF               TO PINTHA
                                          PINTA
120115     if cp-clm-int-amt = zeros
120115        move ' ' to pi-int-to-rem-borr
120115     end-if

           IF NO-CI-ERROR
              MOVE CP-CLM-INT-AMT      TO PINTO
                                          PI-INT-AMT
              MOVE CP-CLM-INT-NODAYS   TO PI-INT-DAYS
              MOVE CP-CLM-INT-RATE     TO PI-INT-RATE-USED
              MOVE AL-PANOF            TO PINTHA
                                          PINTA
120115        if pi-rb-joint-coverage
120115           and pi-int-amt > zeros
120115           move '  INT TO'       to itrb1o
120115           move ' REM BORR?'     to itrb2o
120115           move pi-int-to-rem-borr
120115                                 to itrbyno
120115           move al-uanon         to itrbyna
120115           move +1               to itrbynl
120115        else
120115           move ' '              to pi-int-to-rem-borr
120115        end-if
           ELSE
              IF CI-RETURN-CODE = '8'
                 MOVE ER-3818          TO EMI-ERROR
                 MOVE -1               TO ZINTL
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 GO TO 2200-EXIT
              ELSE
                 MOVE ZEROS            TO PINTO
                                          PI-INT-AMT
                                          PI-INT-DAYS
                                          PI-INT-RATE-USED
              END-IF
           END-IF

           IF PI-INT-AMT NOT = ZEROS
              IF (CL-SOC-SEC-NO (1:9) NUMERIC)
                 AND (CL-SOC-SEC-NO NOT = ZEROS)
                 CONTINUE
              ELSE
                 MOVE CL-SOC-SEC-NO (1:3)
                                       TO WS-SSN (1:3)
                 MOVE CL-SOC-SEC-NO (5:2)
                                       TO WS-SSN (4:2)
                 MOVE CL-SOC-SEC-NO (8:4)
                                       TO WS-SSN (6:4)
                 IF (WS-SSN NUMERIC)
                    AND (WS-SSN NOT = ZEROS)
                    CONTINUE
                 ELSE
                    MOVE ER-2044       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
              END-IF
              MOVE ELMSTR-KEY          TO ELTRLR-KEY
120115        if pi-int-to-rem-borr = 'Y'
120115           move +61              to trlr-seq-no
120115        else
120115           MOVE +51              TO TRLR-SEQ-NO
120115        end-if
              EXEC CICS READ
                 DATASET  ('ELTRLR')
                 SET      (ADDRESS OF ACTIVITY-TRAILERS)
                 RIDFLD   (ELTRLR-KEY)
                 RESP     (WS-RESPONSE)
              END-EXEC
              IF (NOT WS-RESP-NORMAL)
                          OR
                 ((WS-RESP-NORMAL)
                 AND ((AT-ADDRESS-LINE-1 = SPACES)
                 OR (AT-CITY-STATE = SPACES)))
                 MOVE ER-0874          TO EMI-ERROR
120115           if pi-rb-joint-coverage
120115              move -1            to itrbynl
120115           end-if
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
120115        else
120115           move at-mail-to-name  to pi-int-payees-name
              END-IF
           END-IF
           
           .
       2200-EXIT.
           EXIT.

061013 2210-dcc-max-benes.
061013
           move pi-ah-term             to ws-max-bens
           if cl-critical-period not = zeros and spaces
              move cl-critical-period  to ws-max-bens
           end-if
061013     move cm-ah-benefit-amt      to ws-monthly-benefit
061013     if pdef-found
              and (pd-max-amt (p1) not = zeros)
100314        if pd-ben-pct (p1) not numeric
100314           move zeros            to pd-ben-pct (p1)
100314        end-if
100314        if pd-ben-pct (p1) = zeros
100314           move +1               to ws-work-ben-pct
100314        else
100314           move pd-ben-pct (p1)  to ws-work-ben-pct
100314        end-if
100314*       compute ws-monthly-benefit =
100314*          ws-monthly-benefit * ws-work-ben-pct
100314*       if ws-monthly-benefit > pd-max-amt (p1)
100314*          move pd-max-amt (p1) to ws-monthly-benefit
100314*       end-if
061013     end-if
061013
061013     move cl-cert-key-data       to elcrtt-key (2:21)
061013     move cl-company-cd          to ctrlr-comp-cd
061013     move cl-cert-no             to ctrlr-cert-no
061013     move 'B'                    to ctrlr-rec-type
061013
061013     EXEC CICS READ
061013        DATASET  ('ELCRTT')
061013        set      (address of CERTIFICATE-TRAILERS)
061013        RIDFLD   (ELCRTT-KEY)
061013        RESP     (WS-RESPONSE)
061013     END-EXEC
061013     IF not WS-RESP-NORMAL
061013        go to 2210-exit
061013     end-if
061013
061013*    if instypei = 'P'
061013*       move +1 to s1
061013*    else
061013*       MOVE +2 to s1
061013*    end-if
061013
061013*    move +1 to s1
061013*    evaluate cl-claim-type
061013*       when 'A'
061013*          move +1 to s2
061013*       when 'I'
061013*          move +2 to s2
061013*       when 'G'
061013*          move +3 to s2
061013*       when 'L'
061013*          move +4 to s2
061013*       when 'P'
061013*          move +5 to s2
061013*    end-evaluate
061013

           move zeros                  to ws-prev-days-paid
                                          ws-benefits-paid
                                          ws-prev-amt-paid
           perform varying s1 from +1 by +1 until
              (s1 > +24)
              or (cs-claim-type (s1) = spaces)
120115        if pi-company-id not = 'DCC' and 'VPP'
                 if cs-claim-type (s1) = cl-claim-type
                    compute ws-prev-days-paid =
                       ws-prev-days-paid + cs-days-paid (s1)
                    compute ws-prev-amt-paid =
                       ws-prev-amt-paid + cs-total-paid (s1)
                 end-if
              else
                 if (cs-claim-type (s1) = cl-claim-type)
                    and (cs-benefit-period (s1) = cl-benefit-period)
                    and (cs-insured-type (s1) = cl-insured-type)
                    compute ws-prev-days-paid =
                       ws-prev-days-paid + cs-days-paid (s1)
                    compute ws-prev-amt-paid =
                       ws-prev-amt-paid + cs-total-paid (s1)
                 end-if
              end-if
           end-perform

           if pi-epyamt > zeros
              compute ws-benefits-paid =
                 (ws-prev-amt-paid + pi-epyamt)
                 / ws-monthly-benefit
           else
              compute ws-benefits-paid =
                 (ws-prev-amt-paid + pi-cpyamt)
                 / ws-monthly-benefit
           end-if

030515*    if pi-company-id = 'DCC'
100518     if cl-claim-type not = 'L' and 'O' and 'P'
              if ws-benefits-paid > ws-max-bens
061013           move er-1658          to emi-error
061013           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061013        end-if
           end-if
030515*    end-if

      *     if s1 < +25
061013*        if pi-epyamt > zeros
061013*           compute ws-benefits-paid =
061013*              (cs-total-paid (s1) + PI-EPYAMT)
061013*              / ws-monthly-benefit
061013*        else
061013*           compute ws-benefits-paid =
061013*              (cs-total-paid (s1) + PI-CPYAMT)
061013*              / ws-monthly-benefit
061013*        end-if
061013
061013*       if ws-benefits-paid > pd-crit-period (p1)
      *        if ws-benefits-paid > ws-max-bens
061013*           move er-1658          to emi-error
061013*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061013*        end-if
      *     end-if

061013     .
061013 2210-exit.
061013     exit.
061013
061013 2220-update-elcrtt.
061013
061013     EXEC CICS GETMAIN
061013        SET      (ADDRESS OF CERTIFICATE-TRAILERS)
061013        LENGTH   (ELCRTT-LENGTH)
061013        INITIMG  (GETMAIN-SPACE)
061013     END-EXEC
061013
061013     move cl-cert-key-data       to elcrtt-key (2:21)
061013     move cl-company-cd          to ctrlr-comp-cd
061013     move cl-cert-no             to ctrlr-cert-no
061013     move 'B'                    to ctrlr-rec-type
061013
061013     EXEC CICS READ
061013        UPDATE
061013        DATASET  (ELCRTT-DSID)
061013        into     (CERTIFICATE-TRAILERS)
061013        RIDFLD   (ELCRTT-KEY)
061013        RESP     (WS-RESPONSE)
061013     END-EXEC
061013
061013     IF WS-RESP-NORMAL
061013        perform 2225-upd-crt-trlr thru 2225-exit
061013        EXEC CICS REWRITE
061013           DATASET  (ELCRTT-DSID)
061013           from     (CERTIFICATE-TRAILERS)
061013           RESP     (WS-RESPONSE)
061013        END-EXEC
061013     else
061013        if ws-resp-notfnd
061013           move 'CS'             to certificate-trailers
061013           move cl-company-cd    to cs-company-cd
061013           move cl-cert-key-data to cs-control-primary (2:21)
061013           move cl-cert-no       to cs-cert-no
061013           move 'B'              to cs-trailer-type
061013           perform varying s1 from +1 by +1 until s1 > +24
061013              move zeros         to cs-benefit-period (s1)
061013                                    cs-days-paid (s1)
061013                                    cs-total-paid (s1)
                                          cs-remaining-bens (s1)
061013           end-perform
061013           perform 2225-upd-crt-trlr
061013                                 thru 2225-exit
061013           EXEC CICS WRITE
061013              DATASET  (ELCRTT-DSID)
061013              from     (CERTIFICATE-TRAILERS)
061013              RIDFLD   (cs-control-primary)
061013              RESP     (WS-RESPONSE)
061013           END-EXEC
061013        end-if
061013     END-IF
061013
061013     .
061013 2220-exit.
061013     exit.
061013
061013 2225-upd-crt-trlr.
061013
061013*    if instypei = 'P'
061013*       move +1 to s1
061013*    else
061013*       MOVE +2 to s1
061013*    end-if
061013
061013*    move +1 to s1
061013*    evaluate cl-claim-type
061013*       when 'A'
061013*          move +1 to s2
061013*       when 'I'
061013*          move +2 to s2
061013*       when 'G'
061013*          move +3 to s2
061013*       when 'L'
061013*          move +4 to s2
061013*       when 'P'
061013*          move +5 to s2
061013*    end-evaluate

           perform varying s1 from +1 by +1 until
              (s1 > +24)
              or (cl-claim-no = cs-claim-no (s1))
           end-perform

           if s1 < +25
061013        compute cs-total-paid (s1) =
                 cs-total-paid (s1) + at-amount-paid
061013        compute cs-days-paid (s1) = 
                 cs-days-paid (s1) +  at-days-in-period
      *       compute cs-remaining-bens (s1) =
      *          cs-remaining-bens (s1) - (cs-days-paid (s1) / 30)
              perform 2230-accum-bens-paid thru 2230-exit
           end-if
061013
061013     .
061013 2225-exit.
061013     exit.

       2230-accum-bens-paid.

100518     if cl-claim-type not = 'L' and 'O' and 'P'
              move pi-ah-term          to ws-max-bens
              if cl-critical-period not = zeros and spaces
                 move cl-critical-period
                                       to ws-max-bens
              end-if
           else
              move 01                  to ws-max-bens
           end-if

           move zeros to ws-tot-days-paid ws-tot-amt-paid
           perform varying s2 from +1 by +1 until
              (s2 > +24)
              or (cs-claim-no (s2) = spaces)
120115        if pi-company-id not = 'DCC' and 'VPP'
                 if cs-claim-type (s2) = cl-claim-type
                    compute ws-tot-days-paid =
                       ws-tot-days-paid + cs-days-paid (s2)
                    compute ws-tot-amt-paid =
                       ws-tot-amt-paid + cs-total-paid (s2)
                 end-if
              else
                 if (cs-benefit-period (s2) = cl-benefit-period)
                    and (cs-claim-type (s2) = cl-claim-type)
                    and (cs-insured-type (s2) = cl-insured-type)
                    compute ws-tot-days-paid =
                       ws-tot-days-paid + cs-days-paid (s2)
                    compute ws-tot-amt-paid =
                       ws-tot-amt-paid + cs-total-paid (s2)
                 end-if
              end-if
           end-perform
100518     if cl-claim-type = 'L' or 'P' or 'O'
              if ws-tot-amt-paid > zeros
                 move zeros            to cs-remaining-bens (s1)
              else
                 move 01               to cs-remaining-bens (s1)
              end-if
           else
              compute ws-pd-bens rounded =
                 ws-tot-amt-paid / pi-ah-benefit-amt
              compute cs-remaining-bens (s1) =
                 ws-max-bens - ws-pd-bens
              if cs-remaining-bens (s1) < zeros
                 move zeros            to cs-remaining-bens (s1)
              end-if
           end-if

           .
       2230-exit.
           exit.

04376  3000-MOVE-INPUT-TO-SAVE-AREA.                                    EL156
04377      IF PMTTYPEL > 0                                                 CL*94
04378          MOVE PMTTYPEI           TO WS-PMTTYPE.                   EL156
04379                                                                   EL156
120115     if ws-pmttype = '4'
120115        if pi-rb-joint-coverage
120115           if payeel = +0
120115              move +2            to payeel
120115              move al-uanon      to payeea
120115              move 'Q1'          to payeeo
120115           else
120115              if payeei not = 'Q1' and 'q1'
120115                 move er-1582    to emi-error
120115                 PERFORM 9900-ERROR-FORMAT thru 9900-exit
120115              end-if
120115           end-if
120115        end-if
120115     end-if

04380      MOVE SPACES                 TO PI-PROVISIONAL-IND.              CL*65
04381                                                                      CL*65
04382      IF WS-PMTTYPE = '9'                                             CL*88
04383          MOVE 'P'                TO PI-PROVISIONAL-IND               CL*65
04384          MOVE '1'                TO WS-PMTTYPE.                      CL*65
04385                                                                      CL*65
04386      IF PAYEEL > 0                                                   CL*94
               MOVE FUNCTION UPPER-CASE(PAYEEI) TO PAYEEI
04387          MOVE PAYEEI             TO WS-PAYEE.                     EL156
04388                                                                   EL156
04389      IF NOTE1L > +0                                                  CL*94
04390          MOVE NOTE1I             TO WS-PMTNOTE1                      CL*16
04391                                     WS-FLI-PMTNOTE                   CL*69
04392                                     WS-HAN-PMTNOTE.                  CL*69
04393                                                                      CL*16
04394      IF NOTE2L > +0                                                  CL*94
04395          MOVE NOTE2I             TO WS-PMTNOTE2.                     CL*16
04396                                                                   EL156
04397      IF OFFLINEL > 0                                                 CL*94
04398          MOVE OFFLINEI           TO WS-OFFLINE.                   EL156
052506
052506     IF PROOFDTL > 0
052506         MOVE PROOFDTI            TO DATE-WORK
052506         PERFORM 3100-DEEDIT-DATE THRU 3100-EXIT
052506         MOVE NUM-WORK            TO DC-GREG-DATE-1-MDY 
052506         MOVE '4'                 TO DC-OPTION-CODE
052506         PERFORM 9700-LINK-DATE-CONVERT THRU  9700-EXIT
052506         IF NO-CONVERSION-ERROR                     
052506            MOVE NUM-WORK         TO PROOFDTO         
052506            INSPECT PROOFDTI REPLACING ALL ' ' BY '/'
052506            MOVE NUM-WORK         TO WS-PROOF-DATE
052506         ELSE
052506            MOVE ZEROS            TO WS-PROOF-DATE
052506         END-IF
052506     END-IF.                  
04399                                                                      CL*16
           IF EOBYNL > +0
013013         MOVE FUNCTION UPPER-CASE(EOBYNI) TO EOBYNI
013013         MOVE EOBYNI              TO WS-PRINT-EOB-YN
           END-IF
020413
020413     IF CLMFMYNL > +0
020413         MOVE FUNCTION UPPER-CASE(CLMFMYNI) TO CLMFMYNI
020413         MOVE CLMFMYNI            TO WS-PRINT-CLM-FRM-YN
020413     END-IF
020413
020413     IF SURVYYNL > +0
020413         MOVE FUNCTION UPPER-CASE(SURVYYNI) TO SURVYYNI
020413         MOVE SURVYYNI            TO WS-PRINT-SURVEY-YN
020413     END-IF
102413
102413     IF SPRELYNL > +0
102413         MOVE FUNCTION UPPER-CASE(SPRELYNI) TO SPRELYNI
102413         MOVE SPRELYNI            TO WS-SPECIAL-RELEASE-YN
102413     END-IF

120115     if itrbynl <> +0
120115        move function upper-case(itrbyni) to itrbyni
120115        move itrbyni             to ws-int-to-rem-borr
120115     end-if

020413*    IF GROUPEDL > +0                                                CL*94
020413*        MOVE GROUPEDI           TO WS-GROUPED.                      CL*16
020413*                                                                    CL*16
020413*    IF CASHL > +0                                                   CL*94
020413*       MOVE CASHI               TO WS-CASH.                         CL*82
04406      IF LOANNOL > +0
04407          MOVE LOANNOI            TO WS-LOAN-NO.                      CL*78
04408                                                                      CL*78
04409      IF CHECKNOL > 0                                                 CL*94
04410          MOVE CHECKNOI           TO WS-CHECKNO, WS-AIG-CHECKNO.      CL*65
04411                                                                      CL*65
04412      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
04413          IF WS-AIG-CREDIT-ENTERED                                    CL*65
04414              MOVE WS-A-CHECKNO-6-7 TO WS-A-CHECKNO-1-2               CL*93
04415              MOVE ZEROS            TO WS-A-CHECKNO-3-7               CL*93
04416              MOVE WS-AIG-CHECKNO   TO WS-CHECKNO CHECKNOO            CL*93
04417              MOVE AL-UANON         TO CHECKNOA.                      CL*93
04418                                                                   EL156
04419      IF HOLDTILL > 0                                                 CL*94
04420          MOVE HOLDTILI           TO DATE-WORK                     EL156
04421          PERFORM 3100-DEEDIT-DATE THRU 3100-EXIT                  EL156
04422          MOVE NUM-WORK           TO WS-HOLDTIL.                   EL156
04423                                                                   EL156
           IF ZINTL > +0
              EXEC CICS BIF DEEDIT
                  FIELD   (ZINTI)
                  LENGTH  (07)
              END-EXEC
              IF ZINTI NOT NUMERIC
                 MOVE ER-0419          TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT
                 MOVE AL-UABON         TO ZINTA
                 MOVE -1               TO ZINTL
              ELSE
                 MOVE ZINTI            TO CP-INT-RATE
                                          WS-INT-RATE
                 MOVE CP-INT-RATE      TO ZINTO
                 MOVE AL-UNNON         TO ZINTA
              END-IF
           ELSE
              MOVE ZEROS               TO CP-INT-RATE
           END-IF

04424      IF EPYFROML > 0                                                 CL*94
04425          MOVE EPYFROMI           TO DATE-WORK                     EL156
04426          PERFORM 3100-DEEDIT-DATE THRU 3100-EXIT                  EL156
04427          MOVE NUM-WORK           TO WS-EPYFROM.                   EL156
04428                                                                      CL*65
04429      IF AIGFROML > 0                                                 CL*94
04430          MOVE AIGFROMI           TO DATE-WORK                        CL*65
04431          PERFORM 3100-DEEDIT-DATE THRU 3100-EXIT                     CL*65
04432          MOVE NUM-WORK           TO WS-AIGFROM                       CL*65
04433      ELSE                                                            CL*65
04434          IF (AIGFROMI = LOW-VALUES) AND                              CL*93
04435             (PI-AIGFROM NOT = ZEROS)                                 CL*93
04436               MOVE 'Y' TO WS-RECALC-PAYFROM-SW.                      CL*65
04437                                                                   EL156
04438      IF EPYTHRUL > +0                                                CL*94
04439         MOVE EPYTHRUI           TO DATE-WORK                         CL*16
04440         PERFORM 3100-DEEDIT-DATE THRU 3100-EXIT                      CL*16
04441         MOVE NUM-WORK           TO WS-EPYTHRU                        CL*16
04442         MOVE WS-EPYTHRU         TO DC-GREG-DATE-1-MDY                CL*16
04443         MOVE '4'                TO DC-OPTION-CODE                    CL*16
04444         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                CL*16
04445         IF NO-CONVERSION-ERROR                                       CL*16
04446            IF PI-USES-PAID-TO                                        CL*16
04447               MOVE '6'                TO DC-OPTION-CODE              CL*93
04448               MOVE -1                 TO DC-ELAPSED-DAYS             CL*93
04449               MOVE +0                 TO DC-ELAPSED-MONTHS           CL*93
04450               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT          CL*16
04451               IF NO-CONVERSION-ERROR                                 CL*16
04452                  MOVE DC-BIN-DATE-2   TO DC-BIN-DATE-1               CL*93
04453                  MOVE ' '             TO DC-OPTION-CODE              CL*93
04454                  MOVE +0              TO DC-ELAPSED-DAYS             CL*93
04455                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT       CL*16
04456               IF NO-CONVERSION-ERROR                                 CL*16
04457                  MOVE DC-GREG-DATE-1-MDY TO WS-EPYTHRU.              CL*16
04458                                                                   EL156
04459 *****DEEDIT DAYS AND ADD ZERO IN ORDER TO SET A 'C' SIGN          EL156
04460 *****THAT WILL BE NEEDED LATER FOR A GROUP TEST.                  EL156
04461      IF EDAYSL > 0                                                   CL*94
04462          EXEC CICS BIF DEEDIT                                     EL156
04463              FIELD(EDAYSI)                                        EL156
04464              LENGTH(6)                                            EL156
04465          END-EXEC                                                    CL*88
04466          MOVE EDAYSI             TO WS-DY-WORK                    EL156
04467          IF WS-DY-SIGN = SPACES                                   EL156
04468             MOVE WS-DY-NUM       TO WS-EDAYS                      EL156
04469             ADD 0                TO WS-EDAYS                      EL156
04470          ELSE                                                        CL*20
04471             MOVE EDAYSI          TO WS-EDAYS                      EL156
04472             ADD 0                TO WS-EDAYS.                     EL156
04473                                                                   EL156
04474 *****DEEDIT PAYMENT AND ADD ZERO IN ORDER TO SET A 'C' SIGN       EL156
04475 *****THAT WILL BE NEEDED LATER FOR A GROUP TEST.                  EL156
04476      IF EPYAMTL > 0                                                  CL*94
04477          EXEC CICS BIF DEEDIT                                     EL156
04478              FIELD(EPYAMTI)                                       EL156
04479              LENGTH(10)                                           EL156
04480          END-EXEC                                                    CL*88
04481          MOVE EPYAMTI            TO WS-PY-WORK                    EL156
04482          IF WS-PY-SIGN = SPACES                                   EL156
04483             MOVE WS-PY-NUM       TO WS-EPYAMT                     EL156
04484             ADD 0                TO WS-EPYAMT                     EL156
04485          ELSE                                                     EL156
04486             MOVE EPYAMTI         TO WS-EPYAMT                     EL156
04487             ADD 0                TO WS-EPYAMT.                    EL156
04488                                                                   EL156
04489      IF ERESVL > 0                                                   CL*94
04490          EXEC CICS BIF DEEDIT                                     EL156
04491              FIELD(ERESVI)                                        EL156
04492              LENGTH(9)                                               CL**2
04493          END-EXEC                                                    CL*88
04494          MOVE ERESVI TO WS-ERESV.                                 EL156
04495                                                                   EL156
04496      IF EEXPENSL > 0                                                 CL*94
04497          EXEC CICS BIF DEEDIT                                     EL156
04498              FIELD(EEXPENSI)                                      EL156
04499              LENGTH(8)                                            EL156
04500          END-EXEC                                                    CL*88
04501          MOVE EEXPENSI           TO WS-EEXPENS.                   EL156
04502                                                                   EL156
04503      IF ETYPEL > ZEROS                                               CL*94
04504          MOVE ETYPEI             TO WS-ETYPE.                        CL*94
04505                                                                   EL156
04506      IF PI-COMPANY-ID = 'BOA'                                        CL*93
04507         IF PINTL > ZEROS                                             CL*94
04508            EXEC CICS BIF DEEDIT                                      CL*57
04509              FIELD (PINTI)                                           CL*93
04510              LENGTH(05)                                              CL*57
04511            END-EXEC                                                  CL*57
04512            IF PINTI NUMERIC                                          CL*57
04513                MOVE AL-UNNON     TO PINTA                            CL*93
04514                MOVE PINTI        TO WS-INT-RATE                      CL*82
04515                MOVE WS-INT-RATE  TO PINTO.                           CL*82
04516                                                                      CL*57
04517  3000-EXIT.                                                       EL156
04518      EXIT.                                                           CL*16
04519                                                                      CL*16
04520      EJECT                                                           CL*16
04521  3050-REBUILD-ENTERED.                                               CL*16
04522                                                                      CL*16
04523      MOVE PI-SAVE-INPUT TO WS-SAVE-INPUT.                            CL*16
04524                                                                      CL*65
04525      IF PI-PROV-PMT                                                  CL*65
04526          MOVE '9'           TO PMTTYPEO                              CL*65
04527      ELSE                                                            CL*65
04528          MOVE PI-PMTTYPE    TO PMTTYPEO.                             CL*65
04529                                                                      CL*65
04530      MOVE PI-PAYEE          TO PAYEEO.                               CL*65
04531      MOVE PI-PMTNOTE1       TO NOTE1O.                               CL*65
04532      MOVE PI-PMTNOTE2       TO NOTE2O.                               CL*65
04533      MOVE PI-OFFLINE        TO OFFLINEO.                             CL*65
052506     IF PI-PROOF-DATE NOT = ZEROS
052506         MOVE PI-PROOF-DATE TO PROOFDTO
052506         INSPECT PROOFDTI REPLACING ALL SPACES BY '/'
052506     END-IF.
04534 *    MOVE PI-GROUPED        TO GROUPEDO.                             CL*65
04535                                                                      CL*82
04536 *    MOVE PI-CASH           TO CASHO.                                CL*65
04537                                                                      CL*57
04538      IF PI-INT-RATE > ZEROS                                          CL*94
04539         MOVE PI-INT-RATE         TO PINTO
           end-if

062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
              IF PI-INT-AMT > ZEROS
                 MOVE PI-INT-AMT       TO PINTO
                 MOVE AL-PANOF         TO PINTA
                                          PINTHA
120115           if pi-rb-joint-coverage
120115              move '  INT TO'    to itrb1o
120115              move ' REM BORR?'  to itrb2o
120115              move 'Y'           to itrbyno
120115              move al-uanon      to itrbyna
120115              move +1            to itrbynl
120115           end-if
              END-IF
           END-IF
           
           MOVE PI-PRINT-EOB-YN        TO EOBYNI
020413     MOVE PI-PRINT-CLM-FRM-YN    TO CLMFMYNI
020413     MOVE PI-PRINT-SURVEY-YN     TO SURVYYNI
102413     MOVE PI-SPECIAL-RELEASE-YN  TO SPRELYNI

04541      IF PI-HOLDTIL NOT = ZEROS                                       CL*93
04542         MOVE PI-HOLDTIL          TO HOLDTILO                         CL*16
04543         INSPECT HOLDTILO REPLACING ALL SPACES BY '/'.                CL*16
04544                                                                      CL*84
04545      IF PI-EPYFROM NOT = ZEROS                                       CL*93
04546         MOVE PI-EPYFROM          TO EPYFROMO                         CL*16
04547         INSPECT EPYFROMO REPLACING ALL SPACES BY '/'.                CL*16
04548                                                                      CL*84
04549      IF PI-AIGFROM NOT = ZEROS                                       CL*93
04550         MOVE PI-AIGFROM          TO AIGFROMO                         CL*65
04551         INSPECT AIGFROMO REPLACING ALL SPACES BY '/'.                CL*65
04552                                                                      CL*84
04553      IF PI-EPYTHRU NOT = ZEROS                                       CL*93
04554         MOVE PI-EPYTHRU          TO EPYTHRUO                         CL*16
04555         INSPECT EPYTHRUO REPLACING ALL SPACES BY '/'                 CL*16
04556         IF PI-USES-PAID-TO                                           CL*16
04557            MOVE PI-EPYTHRU         TO DC-GREG-DATE-1-MDY             CL*16
04558            MOVE '4'                TO DC-OPTION-CODE                 CL*16
04559            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             CL*16
04560            IF NO-CONVERSION-ERROR                                    CL*16
04561               MOVE +1 TO DC-ELAPSED-DAYS                             CL*16
04562               MOVE +0 TO DC-ELAPSED-MONTHS                           CL*16
04563               MOVE '6' TO DC-OPTION-CODE                             CL*16
04564               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT          CL*16
04565               IF NO-CONVERSION-ERROR                                 CL*16
04566                  MOVE DC-GREG-DATE-1-EDIT TO EPYTHRUI.               CL*16
04567                                                                      CL*16
04568      IF PI-EDAYS NOT = ZEROS                                         CL*93
04569         MOVE PI-EDAYS TO EDAYSO.                                     CL*16
04570                                                                      CL*16
04571      IF PI-EPYAMT NOT = ZEROS                                        CL*93
04572         MOVE PI-EPYAMT TO EPYAMTO.                                   CL*16
04573                                                                      CL*16
04574      IF PI-ERESV NOT = ZEROS                                         CL*93
04575         MOVE PI-ERESV TO ERESVO.                                     CL*16
04576                                                                      CL*16
04577      IF PI-EEXPENS NOT = ZEROS                                       CL*93
04578         MOVE PI-EEXPENS TO EEXPENSO.                                 CL*16
04579                                                                      CL*16
04580      MOVE PI-ETYPE TO ETYPEO.                                        CL*16
04581                                                                      CL*16
04582      IF PI-ECHECKNO NOT = SPACES                                     CL*16
04583         MOVE PI-ECHECKNO         TO CHECKNOO                         CL*16
04584      ELSE                                                            CL*16
04585         MOVE PI-CCHECKNO         TO CHECKNOO.                        CL*16
04586                                                                      CL*16
04587      MOVE PI-CPYFROM             TO DC-BIN-DATE-1.                   CL*16
04588      MOVE SPACES                 TO DC-OPTION-CODE.                  CL*16
04589      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*16
04590                                                                      CL*16
04591      IF DATE-CONVERSION-ERROR                                        CL*16
04592         MOVE SPACES              TO CPYFROMI                         CL*16
04593      ELSE                                                            CL*16
04594         MOVE DC-GREG-DATE-1-EDIT TO CPYFROMI.                        CL*16
04595                                                                      CL*16
04596      IF PI-USES-PAID-TO                                              CL*16
04597         MOVE PI-CPYTHRU          TO DC-BIN-DATE-1                    CL*16
04598         MOVE +1                  TO DC-ELAPSED-DAYS                  CL*16
04599         MOVE +0                  TO DC-ELAPSED-MONTHS                CL*16
04600         MOVE '6'                 TO DC-OPTION-CODE                   CL*16
04601         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                CL*16
04602         IF NO-CONVERSION-ERROR                                       CL*16
04603            MOVE DC-GREG-DATE-1-EDIT TO CPYTHRUI                      CL*16
04604            GO TO 3051-CONTINUE.                                      CL*16
04605                                                                      CL*16
04606      MOVE PI-CPYTHRU             TO DC-BIN-DATE-1.                   CL*16
04607      MOVE SPACES                 TO DC-OPTION-CODE.                  CL*16
04608      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*16
04609                                                                      CL*16
04610      IF DATE-CONVERSION-ERROR                                        CL*16
04611         MOVE SPACES              TO CPYTHRUI                         CL*16
04612      ELSE                                                            CL*16
04613         MOVE DC-GREG-DATE-1-EDIT TO CPYTHRUI.                        CL*16
04614                                                                      CL*16
04615  3051-CONTINUE.                                                      CL*16
04616                                                                      CL*16
04617      IF PI-CDAYS NOT = ZEROS                                         CL*93
04618         MOVE PI-CDAYS TO CDAYSO.                                     CL*16
04619                                                                      CL*16
04620      IF PI-CPYAMT NOT = ZEROS                                        CL*93
04621         MOVE PI-CPYAMT TO CPYAMTO.                                   CL*16
04622                                                                      CL*16
04623      IF PI-CRESV NOT = ZEROS                                         CL*93
04624         MOVE PI-CRESV TO CRESVO.                                     CL*16
04625                                                                      CL*16
04626      IF PI-CEXPENS NOT = ZEROS                                       CL*93
04627         MOVE PI-CEXPENS TO CEXPENSO.                                 CL*16
04628                                                                      CL*16
04629  3059-EXIT.                                                          CL*16
04630      EXIT.                                                        EL156
04631                                                                   EL156
04632  3100-DEEDIT-DATE.                                                EL156
04633      EXEC CICS BIF DEEDIT                                         EL156
04634          FIELD(DATE-WORK)                                         EL156
04635          LENGTH(8)                                                EL156
04636      END-EXEC.                                                       CL*88
04637                                                                   EL156
04638  3100-EXIT.                                                       EL156
04639      EXIT.                                                        EL156
04640                                                                   EL156
04641  3200-CALC-AIG-DAYS.                                                 CL*65
04642                                                                   EL156
04643      MOVE 'N'                    TO WS-DUE-DAY-SW.                   CL*93
04644                                                                   EL156
04645      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.                   CL*65
04646      MOVE ' '                    TO DC-OPTION-CODE.                  CL*65
04647      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*65
04648      IF NO-CONVERSION-ERROR                                          CL*65
04649          MOVE DC-GREG-DATE-1-MDY     TO WS-TEMP-INC-MDY              CL*65
04650      ELSE                                                            CL*65
04651          GO TO 3290-ERROR.                                           CL*65
04652                                                                   EL156
04653      IF WS-SV-AIGFROM NOT = LOW-VALUES                               CL*93
04654          MOVE WS-SV-AIGFROM  TO DC-BIN-DATE-1                        CL*65
04655                                 WS-BIN-FROM-DT                       CL*65
04656      ELSE                                                            CL*65
04657          IF CL-TOTAL-PAID-AMT > ZEROS                                CL*94
04658              IF WS-SV-EPYFROM = LOW-VALUES                           CL*93
04659                  MOVE PI-CPYFROM     TO DC-BIN-DATE-1                CL*65
04660                                         WS-BIN-FROM-DT               CL*65
04661              ELSE                                                    CL*65
04662                  MOVE WS-SV-EPYFROM  TO DC-BIN-DATE-1                CL*65
04663                                         WS-BIN-FROM-DT               CL*65
04664          ELSE                                                        CL*65
04665              MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1                CL*65
04666                                         WS-BIN-FROM-DT.              CL*65
04667                                                                   EL156
04668  3210-CONTINUE.                                                      CL*65
04669                                                                   EL156
04670      MOVE ' '                    TO DC-OPTION-CODE.                  CL*65
04671      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*65
04672      IF DATE-CONVERSION-ERROR                                        CL*65
04673          GO TO 3290-ERROR.                                           CL*65
04674                                                                   EL156
04675      MOVE DC-GREG-DATE-1-MDY TO WS-PMT-DUE-MDY.                      CL*65
04676                                                                   EL156
04677      IF PI-LOAN-DUE-DAY = 31                                         CL*93
04678          MOVE 30              TO WS-PMT-DUE-DA                       CL*65
04679                                  WS-SAVE-DUE-DA                      CL*65
04680      ELSE                                                            CL*65
04681          MOVE PI-LOAN-DUE-DAY TO WS-PMT-DUE-DA                       CL*65
04682                                  WS-SAVE-DUE-DA.                     CL*65
04683                                                                      CL*65
04684      IF (PI-LOAN-DUE-DAY > 28) AND                                   CL*94
04685         (WS-PMT-DUE-MO = 2)                                          CL*93
04686          MOVE 'Y'                TO WS-DUE-DAY-SW                    CL*93
04687          MOVE 28                 TO WS-PMT-DUE-DA.                   CL*93
04688                                                                      CL*65
04689      MOVE WS-PMT-DUE-MDY         TO DC-GREG-DATE-1-MDY.              CL*65
04690      MOVE '4'                    TO DC-OPTION-CODE.                  CL*65
04691      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*65
04692      IF NO-CONVERSION-ERROR                                          CL*65
04693          MOVE DC-BIN-DATE-1      TO WS-PMT-DUE-DT                    CL*65
04694      ELSE                                                            CL*65
04695          GO TO 3290-ERROR.                                           CL*65
04696                                                                      CL*65
04697 *--------------------------------------------------------------*     CL*65
04698 *    IF THE 1ST LOAN PMT DATE LESS THAN INCURRED DATE          *     CL*65
04699 *    AND THE EFFECTIVE DAY LESS THAN INCURRED DAY              *     CL*65
04700 *    ADD ONE MONTH TO THE CURRENT PAYMENT DUE DATE             *     CL*65
04701 *                                                              *     CL*65
04702 *    EXAMPLE: CERT LOAN PMT DUE  - 05/03/87                    *     CL*65
04703 *             INCURRED DATE      - 04/15/91                    *     CL*65
04704 *             PMT DUE DATE       - 04/03/91                    *     CL*65
04705 *             PMT DUE DATE       - 05/03/91                    *     CL*65
04706 *--------------------------------------------------------------*     CL*65
04707                                                                      CL*65
04708      IF (CL-TOTAL-PAID-AMT NOT > ZEROS)  AND                         CL*94
04709         (WS-SV-AIGFROM = LOW-VALUES)                                 CL*93
04710          IF (WS-PMT-DUE-MO = WS-TEMP-INC-MO) AND                     CL*94
04711             (WS-PMT-DUE-DA < WS-TEMP-INC-DA)                         CL*94
04712              MOVE WS-PMT-DUE-DT      TO DC-BIN-DATE-1                CL*65
04713              PERFORM 3300-BUMP-DATE THRU 3399-EXIT                   CL*65
04714              IF NO-CONVERSION-ERROR                                  CL*65
04715                  MOVE DC-BIN-DATE-2      TO WS-PMT-DUE-DT            CL*93
04716                  MOVE DC-GREG-DATE-1-MDY TO WS-PMT-DUE-MDY           CL*65
04717              ELSE                                                    CL*65
04718                  GO TO 3290-ERROR.                                   CL*65
04719                                                                      CL*65
04720      IF (CL-TOTAL-PAID-AMT NOT > ZEROS)  AND                         CL*94
04721         (WS-SV-AIGFROM = LOW-VALUES)                                 CL*93
04722          GO TO 3205-CONT-CALC-PMT.                                   CL*65
04723                                                                      CL*65
04724      MOVE WS-BIN-FROM-DT         TO DC-BIN-DATE-1.                   CL*65
04725      MOVE ' '                    TO DC-OPTION-CODE.                  CL*65
04726      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*65
04727      IF NO-CONVERSION-ERROR                                          CL*65
04728          MOVE DC-GREG-DATE-1-MDY     TO WS-FROM-DATE-MDY             CL*65
04729      ELSE                                                            CL*65
04730          GO TO 3290-ERROR.                                           CL*65
04731                                                                      CL*65
04732      IF (WS-PMT-DUE-MO = WS-FROM-MO) AND                             CL*93
04733         (WS-PMT-DUE-DA < WS-FROM-DA)                                 CL*94
04734           MOVE WS-PMT-DUE-DT      TO DC-BIN-DATE-1                   CL*65
04735           PERFORM 3300-BUMP-DATE THRU 3399-EXIT                      CL*65
04736           IF NO-CONVERSION-ERROR                                     CL*65
04737               MOVE DC-BIN-DATE-2      TO WS-PMT-DUE-DT               CL*93
04738               MOVE DC-GREG-DATE-1-MDY TO WS-PMT-DUE-MDY              CL*65
04739           ELSE                                                       CL*65
04740               GO TO 3290-ERROR.                                      CL*65
04741                                                                      CL*65
04742  3205-CONT-CALC-PMT.                                                 CL*65
04743                                                                      CL*65
04744 *--------------------------------------------------------------*     CL*65
04745 *    VERIFY THAT THE SYSTEM CALCULATED LOAN PAYMENT DATE FALLS *     CL*65
04746 *    BETWEEN THE PAY FROM / THRU DATES.                        *     CL*65
04747 *--------------------------------------------------------------*     CL*65
04748                                                                      CL*65
04749      IF CL-TOTAL-PAID-AMT = ZEROS AND                                CL*93
04750         PI-PMTTYPE = '2'                                             CL*93
04751           NEXT SENTENCE                                              CL*65
04752      ELSE                                                            CL*65
04753         IF (WS-PMT-DUE-DT < WS-BIN-FROM-DT) OR                       CL*94
04754            (WS-PMT-DUE-DT > WS-SV-EPYTHRU)                           CL*94
04755              MOVE WS-PMT-DUE-MDY     TO EMI-DATE-FIELD               CL*65
04756              MOVE ER-3527            TO EMI-ERROR                    CL*65
04757              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*65
04758              MOVE -1                 TO EPYTHRUL                     CL*65
04759              MOVE AL-UNBON           TO EPYTHRUA                     CL*65
04760                                         EPYFROMA                     CL*65
04761              GO TO 3299-EXIT.                                        CL*65
04762                                                                      CL*65
04763 *--------------------------------------------------------------*     CL*65
04764 *    CALCULATE RETRO ELIMINATION DATE                          *     CL*65
04765 *--------------------------------------------------------------*     CL*65
04766                                                                      CL*65
04767      IF WS-SV-AIGFROM NOT = LOW-VALUES                               CL*93
04768          MOVE WS-SV-AIGFROM  TO DC-BIN-DATE-1                        CL*65
04769      ELSE                                                            CL*65
04770          MOVE CL-INCURRED-DT TO DC-BIN-DATE-1.                       CL*65
04771                                                                      CL*65
04772      MOVE PI-BEN-DAYS    TO DC-ELAPSED-DAYS.                         CL*65
04773      MOVE +0             TO DC-ELAPSED-MONTHS.                       CL*65
04774      MOVE '6'            TO DC-OPTION-CODE.                          CL*65
04775      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*65
04776      IF NO-CONVERSION-ERROR                                          CL*65
04777          MOVE DC-BIN-DATE-2 TO WS-RETRO-ELIM-DT                      CL*65
04778      ELSE                                                            CL*65
04779          GO TO 3290-ERROR.                                           CL*65
04780                                                                      CL*65
04781  3210-CHECK-PMT-DATE.                                                CL*65
04782                                                                      CL*65
04783 *--------------------------------------------------------------*     CL*65
04784 *    VERIFY THAT THE CALCULATED LOAN DUE DATE FALLS AFTER      *     CL*65
04785 *    THE ENTERED PAY FROM DATE                                 *     CL*65
04786 *--------------------------------------------------------------*     CL*65
04787                                                                      CL*65
04788      IF WS-PMT-DUE-DT < WS-SV-EPYFROM                                CL*94
04789          NEXT SENTENCE                                               CL*65
04790      ELSE                                                            CL*65
04791          GO TO 3220-COMPUTE-DAYS.                                    CL*65
04792                                                                      CL*65
04793      MOVE WS-PMT-DUE-DT      TO DC-BIN-DATE-1.                       CL*65
04794      PERFORM 3300-BUMP-DATE THRU 3399-EXIT.                          CL*65
04795      IF NO-CONVERSION-ERROR                                          CL*65
04796          MOVE DC-BIN-DATE-2      TO WS-PMT-DUE-DT                    CL*93
04797          MOVE DC-GREG-DATE-1-MDY TO WS-PMT-DUE-MDY                   CL*65
04798          GO TO 3210-CHECK-PMT-DATE                                   CL*65
04799      ELSE                                                            CL*65
04800          GO TO 3290-ERROR.                                           CL*65
04801                                                                      CL*65
04802  3220-COMPUTE-DAYS.                                                  CL*65
04803                                                                      CL*65
04804      MOVE WS-PMT-DUE-DT TO WS-NEXT-PMT-DUE-DT.                       CL*65
04805      MOVE ZEROS         TO WS-CDAYS.                                 CL*93
04806      MOVE 'N'           TO WS-ELIMINATION-SW.                        CL*93
04807                                                                      CL*65
04808 *--------------------------------------------------------------*     CL*65
04809 *    COMPUTE CALIFORNIA DUE DAYS                               *     CL*65
04810 *--------------------------------------------------------------*     CL*65
04811                                                                      CL*65
04812      IF CL-TOTAL-PAID-AMT > ZEROS                                    CL*94
04813          MOVE 'Y' TO WS-ELIMINATION-SW                               CL*65
04814          GO TO 3240-FIRST-PMT-DUE                                    CL*65
04815      ELSE                                                            CL*65
04816         IF WS-RETRO-ELIM-DT > WS-PMT-DUE-DT                          CL*94
04817             NEXT SENTENCE                                            CL*65
04818         ELSE                                                         CL*65
04819             GO TO 3240-FIRST-PMT-DUE.                                CL*65
04820                                                                      CL*65
04821  3230-NEXT-PMT-DUE.                                                  CL*65
04822                                                                      CL*65
04823      MOVE WS-NEXT-PMT-DUE-DT TO DC-BIN-DATE-1.                       CL*65
04824      PERFORM 3300-BUMP-DATE THRU 3399-EXIT.                          CL*65
04825      IF NO-CONVERSION-ERROR                                          CL*65
04826          MOVE DC-BIN-DATE-2 TO WS-NEXT-PMT-DUE-DT                    CL*65
04827          MOVE DC-GREG-DATE-1-MDY TO WS-PMT-DUE-MDY                   CL*65
04828      ELSE                                                            CL*65
04829          GO TO 3290-ERROR.                                           CL*65
04830                                                                      CL*65
04831  3240-FIRST-PMT-DUE.                                                 CL*65
04832                                                                      CL*65
04833      IF CL-TOTAL-PAID-AMT > ZEROS                                    CL*94
04834          NEXT SENTENCE                                               CL*65
04835      ELSE                                                            CL*65
04836         IF WS-RETRO-ELIM-DT > WS-NEXT-PMT-DUE-DT                     CL*94
04837             GO TO 3230-NEXT-PMT-DUE.                                 CL*65
04838                                                                      CL*65
04839      IF WS-NEXT-PMT-DUE-DT > WS-SV-EPYTHRU                           CL*94
04840          GO TO 3250-VERIFY-ELIMINATION.                              CL*65
04841                                                                      CL*65
04842      ADD +30 TO WS-CDAYS.                                            CL*65
04843      MOVE 'Y' TO WS-ELIMINATION-SW.                                  CL*65
04844      GO TO 3230-NEXT-PMT-DUE.                                        CL*65
04845                                                                      CL*65
04846  3250-VERIFY-ELIMINATION.                                            CL*65
04847                                                                      CL*65
04848      IF ELIM-SATISFIED                                               CL*65
04849          GO TO 3260-END-CALC.                                        CL*65
04850                                                                      CL*65
04851      IF PI-PMTTYPE = '1'                                             CL*93
04852          MOVE ZEROS TO WS-CDAYS                                      CL*65
04853          GO TO 3260-END-CALC.                                        CL*65
04854                                                                      CL*65
04855      IF WS-SV-AIGFROM NOT = LOW-VALUES                               CL*93
04856          MOVE WS-SV-AIGFROM  TO DC-BIN-DATE-1                        CL*65
04857      ELSE                                                            CL*65
04858          IF CL-TOTAL-PAID-AMT > ZEROS                                CL*94
04859              IF WS-SV-EPYFROM = LOW-VALUES                           CL*93
04860                  MOVE PI-CPYFROM     TO DC-BIN-DATE-1                CL*65
04861              ELSE                                                    CL*65
04862                  MOVE WS-SV-EPYFROM  TO DC-BIN-DATE-1                CL*65
04863          ELSE                                                        CL*65
04864              MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1.               CL*65
04865                                                                      CL*65
04866      MOVE WS-SV-EPYTHRU          TO DC-BIN-DATE-2.                   CL*65
04867      MOVE '1'                    TO DC-OPTION-CODE.                  CL*65
04868      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*65
04869      IF NO-CONVERSION-ERROR                                          CL*65
04870          COMPUTE WS-DAYS-OFF =  DC-ELAPSED-DAYS + 1                  CL*65
04871      ELSE                                                            CL*65
04872          GO TO 3290-ERROR.                                           CL*65
04873                                                                      CL*65
04874      IF CL-TOTAL-PAID-AMT > ZEROS                                    CL*94
04875          MOVE WS-DAYS-OFF TO WS-CDAYS                                CL*65
04876      ELSE                                                            CL*65
04877          IF PI-BEN-DAYS > WS-DAYS-OFF                                CL*94
04878              MOVE ZEROS TO WS-CDAYS                                  CL*65
04879          ELSE                                                        CL*65
04880              COMPUTE WS-CDAYS = WS-DAYS-OFF - PI-BEN-DAYS.           CL*65
04881                                                                      CL*65
04882  3260-END-CALC.                                                      CL*65
04883                                                                      CL*65
04884      MOVE WS-CDAYS TO PI-CDAYS.                                      CL*65
04885                                                                      CL*65
04886      IF PI-EDAYS = ZEROS                                             CL*93
04887          MOVE PI-CDAYS TO PI-EDAYS                                   CL*65
04888          MOVE AL-UNNON TO EDAYSA.                                    CL*65
04889                                                                      CL*65
04890      GO TO 3299-EXIT.                                                CL*65
04891                                                                      CL*65
04892  3290-ERROR.                                                         CL*65
04893                                                                      CL*65
04894      MOVE ER-3528            TO EMI-ERROR.                           CL*65
04895      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*65
04896      MOVE -1                 TO CDAYSL.                              CL*65
04897      MOVE AL-UNBON           TO CDAYSA.                              CL*65
04898                                                                      CL*65
04899  3299-EXIT.                                                          CL*65
04900      EXIT.                                                           CL*65
04901                                                                      CL*65
04902  3300-BUMP-DATE.                                                     CL*65
04903                                                                      CL*65
04904      IF WS-RESTORE-DUE-DAY                                           CL*65
04905          MOVE 'N'              TO WS-DUE-DAY-SW                      CL*93
04906          MOVE WS-SAVE-DUE-DA   TO WS-PMT-DUE-DA                      CL*93
04907          ADD 1                 TO WS-PMT-DUE-MO                      CL*93
04908          MOVE WS-PMT-DUE-MDY   TO DC-GREG-DATE-1-MDY                 CL*65
04909          MOVE '4'              TO DC-OPTION-CODE                     CL*97
04910          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT               CL*65
04911          IF NO-CONVERSION-ERROR                                      CL*65
04912              MOVE DC-GREG-DATE-1-MDY TO WS-PMT-DUE-MDY               CL*65
04913              MOVE DC-BIN-DATE-1      TO DC-BIN-DATE-2                CL*65
04914              GO TO 3399-EXIT                                         CL*65
04915          ELSE                                                        CL*65
04916              MOVE '9'          TO DC-ERROR-CODE                      CL*93
04917              GO TO 3399-EXIT.                                        CL*65
04918                                                                      CL*65
04919      IF (WS-PMT-DUE-MO = 1) AND                                      CL*93
04920         (WS-SAVE-DUE-DA > 28)                                        CL*94
04921            MOVE 'Y' TO WS-DUE-DAY-SW.                                CL*65
04922                                                                      CL*65
04923      MOVE +0                 TO DC-ELAPSED-DAYS.                     CL*65
04924      MOVE +1                 TO DC-ELAPSED-MONTHS.                   CL*65
04925      MOVE '6'                TO DC-OPTION-CODE.                      CL*65
04926      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*65
04927                                                                      CL*65
04928  3399-EXIT.                                                          CL*65
04929      EXIT.                                                           CL*65
04930                                                                      CL*65
04931      EJECT                                                           CL*65
04932  3400-COMPUTE-EXPIRY.                                                CL*65
04933                                                                      CL*65
04934      MOVE CM-LOAN-1ST-PMT-DT    TO DC-BIN-DATE-1.                    CL*65
04935      MOVE +0                    TO DC-ELAPSED-DAYS                   CL*65
04936                                    DC-ODD-DAYS-OVER.                 CL*65
04937      MOVE '1'                   TO DC-END-OF-MONTH.                  CL*65
04938      MOVE '6'                   TO DC-OPTION-CODE.                   CL*65
04939                                                                      CL*65
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                   CL*65
04941          COMPUTE DC-ELAPSED-MONTHS = CM-LF-ORIG-TERM - +1            CL*65
04942      ELSE                                                            CL*65
04943          COMPUTE DC-ELAPSED-MONTHS = CM-AH-ORIG-TERM - +1.           CL*65
04944                                                                      CL*65
04945      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*65
04946      IF NO-CONVERSION-ERROR                                          CL*65
04947          MOVE DC-BIN-DATE-2  TO WS-EXP-DT PI-EXP-DT                  CL*93
04948      ELSE                                                            CL*65
04949          GO TO 3390-CONTINUE.                                        CL*65
04950                                                                      CL*65
04951      IF CL-CLAIM-TYPE NOT = (PI-LIFE-OVERRIDE-L1 OR 'O')             CL*65
04952          GO TO 3390-CONTINUE.                                        CL*65
04953                                                                   EL156
04954      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
04955      MOVE '4'                    TO CNTL-REC-TYPE.                   CL*65
04956      MOVE +0                     TO CNTL-SEQ-NO.                  EL156
04957      MOVE CM-LF-BENEFIT-CD       TO WS-BEN-CD.                       CL*65
04958      MOVE WS-ACCESS              TO CNTL-ACCESS.                     CL*65
04959      MOVE 'BENE'                 TO FILE-SWITCH.                     CL*65
04960                                                                   EL156
04961      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.                       CL*65
04962                                                                   EL156
04963      IF NO-BENEFIT-FOUND                                             CL*65
04964          GO TO 3390-CONTINUE.                                        CL*65
04965                                                                   EL156
120115     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121           OR 'FNL'
PEMMOD        IF (PI-OFFLINE = 'Y') OR
PEMMOD           (PI-PMTTYPE = '7')
CIDMOD           PERFORM 9870-OUTPUT-ACTIVITY-RECORD                         000
PEMMOD                                     THRU 9870-EXIT                    000
CIDMOD           IF ERROR-ON-OUTPUT                                          000
CIDMOD             MOVE -1                 TO ENTERPFL                       000
CIDMOD             MOVE AL-UANON           TO ENTERPFA                       000
CIDMOD             MOVE MAP-NAMEA          TO MAP-NAME                       000
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  000
CIDMOD             GO TO 8200-SEND-DATAONLY.                                 000
CIDMOD                                                                       000
100518     IF (CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O')  AND            CL*65
04967         (CF-CO-EARNINGS-CALC (SUB-1) = 'B' OR 'K' OR 'L') AND        CL*93
04968         (CF-SPECIAL-CALC-CD  (SUB-1) = 'L')                          CL*93
04969           MOVE WS-EXP-DT TO DC-BIN-DATE-1                            CL*65
04970           MOVE +0        TO DC-ELAPSED-DAYS                          CL*93
04971           MOVE +1        TO DC-ELAPSED-MONTHS                        CL*93
04972           MOVE 6         TO DC-OPTION-CODE                           CL*93
04973           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.             CL*65
04974           IF NO-CONVERSION-ERROR                                     CL*65
04975               MOVE DC-BIN-DATE-2  TO WS-EXP-DT PI-EXP-DT.            CL*93
04976                                                                   EL156
04977  3390-CONTINUE.                                                      CL*65
04978                                                                   EL156
121802      IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122                                           OR 'B' or 'H'
04980          MOVE CM-AH-LOAN-EXPIRE-DT
                                       TO DC-BIN-DATE-1
04981       ELSE                                                           CL*65
04982          MOVE CM-LF-LOAN-EXPIRE-DT
                                       TO DC-BIN-DATE-1
            END-IF
04983                                                                   EL156
04984       MOVE ' '                    TO DC-OPTION-CODE.                 CL*93
04985       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                 CL*65
04986       MOVE DC-GREG-DATE-1-MDY     TO WS-TEMP-DATE-MDY.               CL*65
04987       MOVE WS-TEMP-DA             TO PI-LOAN-DUE-DAY.                CL*65
04988                                                                      CL*30
04989  3499-EXIT.                                                          CL*65
04990      EXIT.                                                           CL*65
04991                                                                   EL156
04992      EJECT                                                        EL156
061511
061511 3600-CHECK-VERIFICATION.
061511
061511     MOVE ELMSTR-KEY             TO ELTRLR-KEY.
061511
061511     MOVE PI-PAYEE-SEQ-NUM       TO TRLR-SEQ-NO.
061511
061511     IF PI-PAYEE-TYPE = 'I'
061511        GO TO 3600-READ-TRAILER.
061511
061511     IF PI-PAYEE-TYPE = 'B'
061511        ADD +10                  TO TRLR-SEQ-NO
061511        GO TO 3600-READ-TRAILER.
061511
061511     IF PI-PAYEE-TYPE = 'A'
061511        ADD +20                  TO TRLR-SEQ-NO
061511        GO TO 3600-READ-TRAILER.
061511
061511     IF PI-PAYEE-TYPE = 'O'
061511        ADD +50                  TO TRLR-SEQ-NO
061511        GO TO 3600-READ-TRAILER.
061511
061511     IF PI-PAYEE-TYPE = 'Q'
061511        ADD +60                  TO TRLR-SEQ-NO
061511        GO TO 3600-READ-TRAILER.
061511
061511     IF PI-PAYEE-TYPE = 'P'
061511        ADD +30                  TO TRLR-SEQ-NO
061511        GO TO 3600-READ-TRAILER.
061511
061511     IF PI-PAYEE-TYPE = 'E'
061511        ADD +40                  TO TRLR-SEQ-NO.
061511
061511 3600-READ-TRAILER.
061511
061511     EXEC CICS HANDLE CONDITION
061511          NOTFND(3600-CONTINUE)
061511     END-EXEC.
061511
061511     PERFORM 7950-READ-TRAILER THRU 7950-EXIT.
061511
061511 3600-CONTINUE.
032514
100518     IF CL-CLAIM-TYPE NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
032514         GO TO 3600-AH-VERIFY
032514     END-IF.
061511
032514     IF PI-VFY-2ND-BENE = 'L' OR 'B'
061511       IF AT-MAIL-TO-NAME (1:9) <> 'ESTATE OF'
061511         IF AT-VFY-2ND-BENE-VERIFIED <> 'Y'
061511           MOVE ER-7576            TO EMI-ERROR
061511           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061511           MOVE -1                 TO PAYEEL
061511           MOVE AL-UABON           TO PAYEEA
020513           GO TO 3600-EXIT
061511         END-IF
020513       END-IF
061511     END-IF.
061511
020513     MOVE AT-MAIL-TO-NAME TO WS-WORK-NAME.
020513     MOVE 1 TO WS-SUB2
020513     MOVE SPACES TO WS-PAYEE-NAME
020513     PERFORM VARYING WS-SUB FROM 1 BY 1 
020513             UNTIL WS-SUB > 30
020513         IF WS-WORK-NAME-X (WS-SUB) > SPACES
020513            MOVE WS-WORK-NAME-X (WS-SUB) TO
020513                 WS-PAYEE-NAME-X (WS-SUB2)
020513            ADD +1 TO WS-SUB2
020513         END-IF
020513     END-PERFORM.
020513
020513     STRING CL-INSURED-1ST-NAME
020513            CL-INSURED-MID-INIT
020513            CL-INSURED-LAST-NAME INTO WS-WORK-NAME.
020513     MOVE 1 TO WS-SUB2
020513     MOVE SPACES TO WS-INSURED-NAME
020513     PERFORM VARYING WS-SUB FROM 1 BY 1 
020513             UNTIL WS-SUB > 30
020513         IF WS-WORK-NAME-X (WS-SUB) > SPACES
020513            MOVE WS-WORK-NAME-X (WS-SUB) TO
020513                 WS-INSURED-NAME-X (WS-SUB2)
020513            ADD +1 TO WS-SUB2
020513         END-IF
020513     END-PERFORM.
020513
100518     IF CL-CLAIM-TYPE = 'O'
100518        IF WS-PAYEE-NAME (1:6) = 'ESTATE'
100518           MOVE ER-7585            TO EMI-ERROR
100518           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
100518           MOVE -1                 TO PAYEEL
100518           MOVE AL-UABON           TO PAYEEA
100518        ELSE
100518           GO TO 3600-EXIT
100518        END-IF
100518     END-IF.

020513     IF WS-PAYEE-NAME = WS-INSURED-NAME
020513        MOVE ER-7579            TO EMI-ERROR
020513        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
020513        MOVE -1                 TO PAYEEL
020513        MOVE AL-UABON           TO PAYEEA
020513     END-IF.
020513         
032414     IF PI-PMTTYPE = '4'  AND
032414        WS-PAYEE-NAME (1:8) = 'ESTATEOF'  AND
032414        PI-PAYEE-TYPE <> 'I'
032414          MOVE ER-7580          TO EMI-ERROR
032414          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
032414          MOVE -1               TO PAYEEL
032414          MOVE AL-UABON         TO PAYEEA
032414     END-IF.
032414
032514     GO TO 3600-EXIT.
032514
032514 3600-AH-VERIFY.
032514
032514     IF PI-VFY-2ND-BENE = 'A' OR 'B'
032514        IF AT-VFY-2ND-BENE-VERIFIED <> 'Y'
032514           MOVE ER-7583            TO EMI-ERROR
032514           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
032514           MOVE -1                 TO PAYEEL
032514           MOVE AL-UABON           TO PAYEEA
032514        END-IF
032514     END-IF.
032514
032514
061511 3600-EXIT.
061511     EXIT.
061511
04993  5000-UPDATE.                                                     EL156
04994                                                                      CL*65
04995      PERFORM 7300-CHECK-AUTO-ACTIVITY THRU 7399-EXIT.                CL*65
04996                                                                      CL*65
04997      PERFORM 6200-UPDATE-CLAIM-MSTR THRU 6299-EXIT.               EL156
04998                                                                   EL156
04999      IF PI-PMTTYPE = '4' AND PI-EPYAMT NEGATIVE  OR               EL156
05000         PI-OFFLINE = 'Y'                                          EL156
05001         NEXT SENTENCE                                             EL156
05002      ELSE                                                         EL156
05003         PERFORM 6700-UPDATE-ACTQ THRU 6799-EXIT.                  EL156
05004                                                                      CL*20
121802*    IF PI-COMPANY-ID = 'CRI'                                        CL*93
121802*       IF PI-PMTTYPE = '1' OR  '2'                                  CL*93
121802*          IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
121802*             PERFORM 6800-BUILD-FORM-TRAILER   THRU 6899-EXIT       CL*93
121802*             PERFORM 6900-BUILD-ARCHIVE-HEADER THRU 6999-EXIT.      CL*51
05010                                                                      CL*51
121802*    IF PI-COMPANY-ID = 'RMC' OR 'LAP'                               CL*93
121802*       IF PI-PMTTYPE = '1'                                          CL*93
121802*          IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
121802*             PERFORM 6800-BUILD-FORM-TRAILER THRU 6899-EXIT.        CL*81
05015                                                                   EL156
05016      IF PI-PMTTYPE      = '2' AND                                    CL*93
05017         PI-SPLIT-PMT-SW = 'Y' AND                                    CL*93
100518        (CL-CLAIM-TYPE   = PI-LIFE-OVERRIDE-L1 OR 'O')               CL*93
05019           MOVE ER-3530                TO EMI-ERROR                   CL*65
05020           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                  CL*65
05021                                                                      CL*65
062121     IF (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
100518        AND (CL-CLAIM-TYPE = 'L' OR 'O')
022106        IF PI-PMTTYPE = '2' OR '4'
022106           IF PI-INT-AMT NOT = ZEROS
022106              PERFORM 7110-BUILD-PMT-TRLR
022106                                 THRU 7130-EXIT
022106           ELSE
022106              IF PI-PMTTYPE = '2'
022106                 PERFORM 7140-BUILD-NOTE-TRLR
022106                                 THRU 7170-EXIT
022106              END-IF
022106           END-IF
022106        END-IF
022106     END-IF

121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*88
121802*        IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1                      CL*88
121802*            MOVE 'N'                 TO  WS-LETTER-SW.              CL*65
05025
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*93
121802*        IF CL-NO-OF-PMTS-MADE = +1                                  CL*93
121802*                OR                                                  CL*82
121802*           PI-PMTTYPE = '2'                                         CL*93
121802*            PERFORM 5300-CREATE-DMO THRU 5300-EXIT.                 CL*82
05031                                                                      CL*82
05032      PERFORM 0800-DELETE-TS THRU 0800-EXIT.                          CL*93
05033                                                                      CL*87
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*88
121802*       IF CL-CERT-NO (5:2) = '69' OR '70'                           CL*88
121802*           PERFORM 6600-UPDATE-DLO035 THRU 6600-EXIT.               CL*88
05037                                                                      CL*88
05038      EXEC CICS REWRITE                                            EL156
05039           DATASET('ELMSTR')                                       EL156
05040           FROM(CLAIM-MASTER)                                      EL156
05041       END-EXEC.                                                      CL*88
05042                                                                   EL156
05043      IF WS-LETTER-SW = 'N'                                           CL*88
05044          GO TO 5000-END-UPDATE.                                      CL*65
05045                                                                      CL*65
05046  5000-GENERATE-AUTO-LETTER.                                          CL*71
05047                                                                      CL*71
05048      EXEC CICS LINK                                                  CL*65
05049          PROGRAM    (LINK-1523)                                      CL*65
05050          COMMAREA   (W-1523-LINKDATA)                                CL*65
05051          LENGTH     (W-1523-COMM-LENGTH)                             CL*65
05052      END-EXEC.                                                       CL*65
05053                                                                      CL*65
05054      IF W-1523-FATAL-ERROR                                           CL*65
05055          MOVE W-1523-ERROR-CODE      TO  EMI-ERROR                   CL*65
05056          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*65
05057          MOVE ER-0802                TO  EMI-ERROR                   CL*65
05058          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*71
05059                                                                      CL*65
05060      IF W-1523-ERROR-CODE = 0191                                     CL*93
05061          MOVE ER-0803                TO  EMI-ERROR                   CL*65
05062          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*65
05063                                                                      CL*71
05064      IF PI-COMPANY-ID      = 'HAN' AND                               CL*93
05065         PI-PMTTYPE         = '1'   AND                               CL*93
05066         PI-3RD-PARTY-SW    = 'Y'   AND                               CL*93
05067         W-1523-FORM-NUMBER = 'EOB1'                                  CL*93
05068            MOVE LOW-VALUES              TO W-1523-LINKDATA           CL*93
05069            MOVE PROGRAM-INTERFACE-BLOCK TO W-1523-COMMON-PI-DATA     CL*93
05070            MOVE 'EOB3'                  TO W-1523-FORM-NUMBER        CL*93
05071            GO TO 5000-GENERATE-AUTO-LETTER.                          CL*93
05072                                                                      CL*65
05073  5000-END-UPDATE.                                                    CL*65
05074                                                                      CL*65
05075      MOVE ER-0000                TO EMI-ERROR.                       CL*93
05076      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
05077      GO TO 7000-SHOW-CLAIM.                                       EL156
05078                                                                   EL156
05079      EJECT                                                        EL156
05080  5000-MOVE-NAME.                                                  EL156
05081      MOVE CL-INSURED-LAST-NAME   TO LSTNMEO.                      EL156
05082      MOVE CL-INSURED-1ST-NAME    TO FSTNMEO.                      EL156
05083      MOVE CL-INSURED-MID-INIT    TO MINITO.                       EL156
05084                                                                   EL156
05085  5000-EXIT.                                                       EL156
05086      EXIT.                                                        EL156
05087                                  EJECT                               CL*82
05088  5300-CREATE-DMO.                                                    CL*82
05089                                                                   EL156
05090      MOVE CL-CERT-KEY-DATA       TO W-NOTE-CERT-KEY.                 CL*82
05091      MOVE PI-COMPANY-CD          TO W-NOTE-COMP-CD.                  CL*82
05092      MOVE CL-CERT-NO             TO W-NOTE-CERT-NO.                  CL*82
05093                                                                      CL*82
05094      EXEC CICS HANDLE CONDITION                                      CL*82
05095           NOTFND   (5300-NOTE-NOT-FOUND)                             CL*82
05096      END-EXEC.                                                       CL*88
05097                                                                      CL*82
05098      EXEC CICS READ                                                  CL*82
05099           DATASET('ERNOTE')                                          CL*82
05100           SET    (ADDRESS OF CERTIFICATE-NOTE)                       CL*82
05101           RIDFLD (W-NOTE-KEY)                                        CL*82
05102      END-EXEC.                                                       CL*88
05103                                                                      CL*82
05104      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.          CL*82
05105      MOVE CL-BENEFICIARY         TO DCT-LOGIC-BENEFICIARY-ID.        CL*82
05106      MOVE CL-CCN                 TO DCT-CREDIT-CARD-NUMBER.          CL*82
05107                                                                      CL*82
05108      IF CL-CERT-GROUPING (5:2) = ZEROS OR SPACES                     CL*85
05109          MOVE 'CC'                   TO DCT-PRODUCT-CODE             CL*97
05110      ELSE                                                            CL*82
05111          MOVE CL-CERT-GROUPING (5:2) TO DCT-PRODUCT-CODE.            CL*85
05112                                                                      CL*82
05113      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.             CL*83
05114      MOVE '02'                   TO DCT-COLUMN-ID-REQUESTED.         CL*82
05115      MOVE 'DLO006'               TO PGM-NAME.                        CL*82
05116                                                                      CL*83
05117      EXEC CICS LINK                                                  CL*83
05118          PROGRAM    (PGM-NAME)                                       CL*83
05119          COMMAREA   (DCT-COMMUNICATION-AREA)                         CL*83
05120          LENGTH     (DCT-RCRD-LENGTH)                                CL*83
05121      END-EXEC.                                                       CL*88
05122                                                                      CL*83
05123      IF DCT-RETURN-CODE = 'OK'                                       CL*88
05124          GO TO 5300-CONT.                                            CL*83
05125                                                                      CL*82
05126      IF DCT-RETURN-CODE = '01' OR '02'                               CL*88
05127          GO TO 5300-EXIT.                                            CL*82
05128                                                                      CL*82
05129      IF DCT-RETURN-CODE = '03'                                       CL*88
05130          MOVE ER-0951            TO EMI-ERROR                        CL*82
05131          GO TO 5300-BAD-EXIT.                                        CL*87
05132                                                                      CL*82
05133      IF DCT-RETURN-CODE = '04'                                       CL*88
05134          MOVE ER-0946            TO EMI-ERROR                        CL*82
05135          GO TO 5300-BAD-EXIT.                                        CL*87
05136                                                                      CL*82
05137      IF DCT-RETURN-CODE = '05'                                       CL*88
05138          MOVE ER-0947            TO EMI-ERROR                        CL*82
05139          GO TO 5300-BAD-EXIT.                                        CL*94
05140                                                                      CL*94
05141      IF DCT-RETURN-CODE = '06'                                       CL*94
05142          MOVE ER-0921            TO EMI-ERROR                        CL*94
05143          GO TO 5300-BAD-EXIT.                                        CL*94
05144                                                                      CL*94
05145      IF DCT-RETURN-CODE = '07'                                       CL*94
05146          MOVE ER-0849            TO EMI-ERROR                        CL*94
05147          GO TO 5300-BAD-EXIT.                                        CL*87
05148                                                                      CL*82
05149      IF DCT-RETURN-CODE = '08'                                       CL*88
05150          MOVE ER-0948            TO EMI-ERROR                        CL*82
05151          GO TO 5300-BAD-EXIT.                                        CL*87
05152                                                                      CL*82
05153      IF DCT-RETURN-CODE = 'N1'                                       CL*88
05154          MOVE ER-0950            TO EMI-ERROR                        CL*82
05155          GO TO 5300-BAD-EXIT.                                        CL*87
05156                                                                      CL*82
05157      IF DCT-RETURN-CODE = 'E1'                                       CL*86
05158          MOVE ER-0974            TO EMI-ERROR                        CL*86
05159          GO TO 5300-BAD-EXIT.                                        CL*87
05160                                                                      CL*86
05161      IF DCT-RETURN-CODE = 'E2'                                       CL*86
05162          MOVE ER-0975            TO EMI-ERROR                        CL*86
05163          GO TO 5300-BAD-EXIT.                                        CL*87
05164                                                                      CL*86
05165      IF DCT-RETURN-CODE NOT = 'OK'                                   CL*86
05166          MOVE ER-0949            TO EMI-ERROR                        CL*82
05167          GO TO 5300-BAD-EXIT.                                        CL*87
05168                                                                      CL*82
05169  5300-CONT.                                                          CL*83
05170                                                                      CL*83
05171      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.          CL*82
05172      MOVE 'CS'                   TO DM-RECORD-TYPE.                  CL*82
05173      MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.                    CL*82
05174      MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.                    CL*82
05175      MOVE CL-CLAIM-NO            TO DM-CLAIM-NO.                     CL*82
05176      MOVE CL-CERT-NO (4:1)       TO DM-CLAIM-TYPE.                   CL*87
05177      MOVE CL-CCN                 TO DM-CREDIT-CARD-NUMBER.           CL*83
05178      MOVE SAVE-DATE-CCYYMMDD     TO DM-STATUS-DATE.                  CL*82
05179                                                                      CL*82
05180      MOVE CL-INSURED-LAST-NAME   TO W-NAME-LAST.                     CL*82
05181      MOVE CL-INSURED-1ST-NAME    TO W-NAME-FIRST.                    CL*82
05182      MOVE CL-INSURED-MID-INIT    TO W-NAME-MIDDLE.                   CL*82
05183      PERFORM 5350-FORMAT-LAST-NAME-1ST THRU 5350-EXIT.               CL*82
05184      MOVE WS-NAME-WORK           TO DM-INSURED-NAME.                 CL*82
05185                                                                      CL*82
05186      MOVE CL-CARRIER             TO DM-STAT-CARRIER.                 CL*87
05187                                                                      CL*87
05188      IF PI-PMTTYPE = '2'                                             CL*88
05189          MOVE 'F'                TO DM-STAT-CHANGE-TYPE              CL*82
05190          MOVE '4'                TO DM-CLAIM-STATUS                  CL*82
05191      ELSE                                                            CL*82
05192          MOVE 'I'                TO DM-STAT-CHANGE-TYPE              CL*82
05193          MOVE '2'                TO DM-CLAIM-STATUS.                 CL*83
05194                                                                      CL*82
05195      MOVE 'DLO025'               TO PGM-NAME.                        CL*82
05196                                                                      CL*82
05197      EXEC CICS LINK                                                  CL*83
05198          PROGRAM    (PGM-NAME)                                       CL*83
05199          COMMAREA   (DMO-COMMUNICATION-AREA)                         CL*83
05200          LENGTH     (DM-DMO-LENGTH)                                  CL*83
05201      END-EXEC.                                                       CL*88
05202                                                                      CL*82
05203      IF DM-RETURN-CODE = 'OK'                                        CL*88
05204          GO TO 5300-EXIT.                                            CL*83
05205                                                                      CL*83
05206      IF DM-RETURN-CODE = '01'                                        CL*88
05207          MOVE ER-8051            TO EMI-ERROR                        CL*83
05208          GO TO 5300-BAD-EXIT.                                        CL*87
05209                                                                      CL*83
05210      IF DM-RETURN-CODE = '02'                                        CL*88
05211          MOVE ER-8052            TO EMI-ERROR                        CL*83
05212          GO TO 5300-BAD-EXIT.                                        CL*87
05213                                                                      CL*83
05214      IF DM-RETURN-CODE = '03'                                        CL*88
05215          MOVE ER-8053            TO EMI-ERROR                        CL*83
05216          GO TO 5300-BAD-EXIT.                                        CL*87
05217                                                                      CL*83
05218      IF DM-RETURN-CODE = '04'                                        CL*88
05219          MOVE ER-8054            TO EMI-ERROR                        CL*83
05220          GO TO 5300-BAD-EXIT.                                        CL*87
05221                                                                      CL*83
05222      IF DM-RETURN-CODE = '05'                                        CL*88
05223          MOVE ER-8055            TO EMI-ERROR                        CL*83
05224          GO TO 5300-BAD-EXIT.                                        CL*87
05225                                                                      CL*83
05226      IF DM-RETURN-CODE = '06'                                        CL*88
05227          MOVE ER-8056            TO EMI-ERROR                        CL*83
05228          GO TO 5300-BAD-EXIT.                                        CL*87
05229                                                                      CL*83
05230      IF DM-RETURN-CODE = '07'                                        CL*88
05231          MOVE ER-8057            TO EMI-ERROR                        CL*83
05232          GO TO 5300-BAD-EXIT.                                        CL*87
05233                                                                      CL*83
05234      IF DM-RETURN-CODE = '08'                                        CL*88
05235          MOVE ER-8058            TO EMI-ERROR                        CL*83
05236          GO TO 5300-BAD-EXIT.                                        CL*87
05237                                                                      CL*83
05238      IF DM-RETURN-CODE = '09'                                        CL*88
05239          MOVE ER-8059            TO EMI-ERROR                        CL*83
05240          GO TO 5300-BAD-EXIT.                                        CL*87
05241                                                                      CL*83
05242      IF DM-RETURN-CODE = '10'                                        CL*88
05243          MOVE ER-8060            TO EMI-ERROR                        CL*83
05244          GO TO 5300-BAD-EXIT.                                        CL*87
05245                                                                      CL*83
05246      IF DM-RETURN-CODE = '11'                                        CL*88
05247          MOVE ER-8061            TO EMI-ERROR                        CL*83
05248          GO TO 5300-BAD-EXIT.                                        CL*87
05249                                                                      CL*83
05250      IF DM-RETURN-CODE = '12'                                        CL*88
05251          MOVE ER-8062            TO EMI-ERROR                        CL*83
05252          GO TO 5300-BAD-EXIT.                                        CL*87
05253                                                                      CL*83
05254      IF DM-RETURN-CODE = '13'                                        CL*88
05255          MOVE ER-8063            TO EMI-ERROR                        CL*83
05256          GO TO 5300-BAD-EXIT.                                        CL*87
05257                                                                      CL*83
05258      IF DM-RETURN-CODE = '14'                                        CL*88
05259          MOVE ER-8064            TO EMI-ERROR                        CL*83
05260          GO TO 5300-BAD-EXIT.                                        CL*87
05261                                                                      CL*83
05262      IF DM-RETURN-CODE = '15'                                        CL*88
05263          MOVE ER-8065            TO EMI-ERROR                        CL*83
05264          GO TO 5300-BAD-EXIT.                                        CL*87
05265                                                                      CL*87
05266      IF DM-RETURN-CODE = '16'                                        CL*87
05267          MOVE ER-8154            TO EMI-ERROR                        CL*87
05268          GO TO 5300-BAD-EXIT.                                        CL*87
05269                                                                      CL*87
05270      IF DM-RETURN-CODE = '17'                                        CL*87
05271          MOVE ER-8155            TO EMI-ERROR                        CL*87
05272          GO TO 5300-BAD-EXIT.                                        CL*87
05273                                                                      CL*85
05274      IF DM-RETURN-CODE = 'N1'                                        CL*85
05275          MOVE ER-8152            TO EMI-ERROR                        CL*85
05276          GO TO 5300-BAD-EXIT.                                        CL*87
05277                                                                      CL*85
05278      IF DM-RETURN-CODE = 'E1'                                        CL*85
05279          MOVE ER-8153            TO EMI-ERROR                        CL*85
05280          GO TO 5300-BAD-EXIT.                                        CL*87
05281                                                                      CL*83
05282      MOVE ER-8066            TO EMI-ERROR.                           CL*87
05283      GO TO 5300-BAD-EXIT.                                            CL*87
05284                                                                      CL*82
05285  5300-NOTE-NOT-FOUND.                                                CL*82
05286                                                                      CL*82
05287      MOVE ER-0954                TO EMI-ERROR.                       CL*87
05288      GO TO 5300-BAD-EXIT.                                            CL*87
05289                                                                      CL*87
05290  5300-BAD-EXIT.                                                      CL*87
05291      EXEC CICS SYNCPOINT ROLLBACK END-EXEC.                          CL*87
05292      IF MAP-NAME = 'EL156A'                                          CL*88
05293          MOVE -1             TO ENTERPFL                             CL*88
05294        ELSE                                                          CL*88
05295          MOVE LOW-VALUES     TO EL156BI                              CL*88
05296          MOVE -1             TO ENTPFBL.                             CL*88
05297                                                                      CL*93
05298      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*87
05299      GO TO 8200-SEND-DATAONLY.                                       CL*82
05300                                                                      CL*82
05301  5300-EXIT.                                                          CL*82
05302      EXIT.                                                           CL*82
05303                                  EJECT                               CL*82
05304  5350-FORMAT-LAST-NAME-1ST.                                          CL*82
05305 *****************************************************************    CL*82
05306 *             M O V E   N A M E   R O U T I N E                 *    CL*82
05307 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *    CL*82
05308 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *    CL*82
05309 *     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *    CL*82
05310 *                  FIELD                   VALUE                *    CL*82
05311 *           W-NAME-LAST    (CL15)      SMITH                    *    CL*82
05312 *           W-NAME-FIRST   (CL15)      JOHN                     *    CL*82
05313 *           W-NAME-MIDDLE  (CL15)      ALLEN                    *    CL*83
05314 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *    CL*82
05315 *     CONTAIN                                                   *    CL*82
05316 *                SMITH, JOHN ALLEN                              *    CL*82
05317 *     OR                                                        *    CL*82
05318 *                SMITH, JOHN A.                                 *    CL*82
05319 *     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *    CL*82
05320 *     COPYBOOK, ELCNWA.                                         *    CL*82
05321 *****************************************************************.   CL*82
05322                                                                      CL*82
05323      MOVE SPACES                 TO  WS-NAME-WORK-AREA.              CL*82
05324      MOVE ZERO                   TO  WS-NAME-SW.                     CL*82
05325      SET NWA-INDEX               TO +1.                              CL*82
05326                                                                      CL*82
05327      IF W-NAME-LAST   = SPACES AND                                   CL*93
05328         W-NAME-MIDDLE = SPACES                                       CL*93
05329          MOVE +1                 TO WS-NAME-SW.                      CL*82
05330                                                                      CL*82
05331      MOVE W-NAME-LAST            TO WS-NAME-WORK2.                   CL*82
05332      PERFORM 5360-MOVE-NAME THRU 5360-EXIT.                          CL*82
05333                                                                      CL*82
05334      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.                   CL*82
05335      PERFORM 5360-MOVE-NAME THRU 5360-EXIT.                          CL*82
05336                                                                      CL*82
05337      SET NWA-INDEX UP BY +1.                                         CL*82
05338                                                                      CL*82
05339      IF W-NAME-MIDDLE NOT = SPACES                                   CL*93
05340          IF W-NAME-MIDDLE-2 = SPACES                                 CL*93
05341              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)                CL*82
05342              SET NWA-INDEX UP BY +1                                  CL*82
05343              MOVE '.'            TO WS-NW (NWA-INDEX)                CL*82
05344              SET NWA-INDEX UP BY +2                                  CL*82
05345          ELSE                                                        CL*82
05346              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2                    CL*82
05347              PERFORM 5360-MOVE-NAME THRU 5360-EXIT.                  CL*82
05348                                                                      CL*82
05349  5350-EXIT.                                                          CL*82
05350      EXIT.                                                           CL*82
05351                                  EJECT                               CL*82
05352  5360-MOVE-NAME.                                                     CL*82
05353                                                                      CL*82
05354      IF WS-NAME-SW > +1                                              CL*94
05355          GO TO 5360-EXIT.                                            CL*82
05356                                                                      CL*82
05357      IF WS-NAME-WORK2 = SPACES                                       CL*88
05358          GO TO 5360-EXIT.                                            CL*82
05359                                                                      CL*82
05360      SET NWA-INDEX2            TO +1.                                CL*82
05361      SET NWA-INDEX3            TO +2.                                CL*82
05362                                                                      CL*82
05363  5360-MOVE-NAME-CYCLE.                                               CL*82
05364                                                                      CL*82
05365      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).                CL*82
05366                                                                      CL*82
05367      IF NWA-INDEX < +30                                              CL*94
05368          SET NWA-INDEX UP BY +1                                      CL*82
05369      ELSE                                                            CL*82
05370          ADD +2                TO  WS-NAME-SW                        CL*82
05371          GO TO 5360-EXIT.                                            CL*82
05372                                                                      CL*82
05373      IF NWA-INDEX2 < +20                                             CL*94
05374          SET NWA-INDEX3 UP BY +1                                     CL*82
05375          SET NWA-INDEX2 UP BY +1.                                    CL*82
05376                                                                      CL*82
05377      IF WS-NW2 (NWA-INDEX2) = SPACES AND                             CL*93
05378         WS-NW2 (NWA-INDEX3) = SPACES                                 CL*93
05379          IF WS-NAME-SW = ZERO                                        CL*93
05380              MOVE ','            TO  WS-NW (NWA-INDEX)               CL*82
05381              SET NWA-INDEX UP BY +2                                  CL*82
05382              MOVE +1             TO  WS-NAME-SW                      CL*82
05383              GO TO 5360-EXIT                                         CL*82
05384          ELSE                                                        CL*82
05385              GO TO 5360-EXIT.                                        CL*82
05386                                                                      CL*82
05387      GO TO 5360-MOVE-NAME-CYCLE.                                     CL*82
05388                                                                      CL*82
05389  5360-EXIT.                                                          CL*82
05390      EXIT.                                                           CL*82
05391                                  EJECT                               CL*82
05392  6000-BUILD-SCREEN-B.                                             EL156
05393      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.                 EL156
05394      MOVE PI-CARRIER             TO MSTR-CARRIER.                    CL*43
05395      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.                EL156
05396      MOVE PI-CERT-NO             TO MSTR-CERT-NO.                 EL156
05397                                                                   EL156
05398      PERFORM 7900-READ-CLAIM THRU 7900-EXIT.                      EL156
05399                                                                   EL156
05400  6050-BUILD-SCREEN.                                               EL156
05401      PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT.             EL156
05402      MOVE LOW-VALUES             TO EL156BI.                      EL156
05403      MOVE ELMSTR-KEY             TO ELTRLR-KEY.                   EL156
05404                                                                   EL156
05405      MOVE PI-PAYEE-SEQ-NUM       TO TRLR-SEQ-NO.                     CL*16
05406                                                                      CL*16
05407      IF PI-PAYEE-TYPE = 'I'                                          CL*93
05408         GO TO 6060-READ-TRAILER.                                  EL156
05409                                                                   EL156
05410      IF PI-PAYEE-TYPE = 'B'                                          CL*93
05411         ADD +10                  TO TRLR-SEQ-NO                      CL*16
05412         GO TO 6060-READ-TRAILER.                                  EL156
05413                                                                   EL156
05414      IF PI-PAYEE-TYPE = 'A'                                          CL*93
05415         ADD +20                  TO TRLR-SEQ-NO                      CL*16
05416         GO TO 6060-READ-TRAILER.                                  EL156
05417                                                                   EL156
05418      IF PI-PAYEE-TYPE = 'O'                                          CL*93
05419         ADD +50                  TO TRLR-SEQ-NO                      CL*16
05420         GO TO 6060-READ-TRAILER.                                  EL156
05421                                                                   EL156
05422      IF PI-PAYEE-TYPE = 'Q'                                          CL*93
05423         ADD +60                  TO TRLR-SEQ-NO                      CL*16
05424         GO TO 6060-READ-TRAILER.                                  EL156
05425                                                                   EL156
05426      IF PI-PAYEE-TYPE = 'P'                                          CL*93
05427         ADD +30                  TO TRLR-SEQ-NO                      CL*65
05428         GO TO 6060-READ-TRAILER.                                     CL*65
05429                                                                      CL*65
05430      IF PI-PAYEE-TYPE = 'E'                                          CL*93
05431         ADD +40                  TO TRLR-SEQ-NO.                     CL*65
05432                                                                   EL156
05433  6060-READ-TRAILER.                                               EL156
05434                                                                      CL*16
05435      IF PI-PAYEE-TYPE    = 'A' AND                                   CL*93
05436         PI-PAYEE-SEQ-NUM = 0                                         CL*93
05437           GO TO 6061-READ-ACCOUNT.                                   CL*93
05438                                                                   EL156
05439      IF (PI-COMPANY-ID    = 'AIG' OR 'AUK')  AND                     CL*93
05440         (PI-PAYEE-TYPE    = 'B')             AND                     CL*93
05441         (PI-PAYEE-SEQ-NUM = 9)                                       CL*93
05442           PERFORM 7920-READ-BENE THRU 7920-EXIT                      CL*93
05443           IF BENE-FOUND-SW = 'N'                                     CL*93
05444             GO TO 6065-NOT-FOUND                                     CL*65
05445           ELSE                                                       CL*93
05446             MOVE BE-MAIL-TO-NAME   TO PI-PAYEE-NAME  NAMEBO          CL*65
05447             MOVE BE-ADDRESS-LINE-1 TO ADDR1BO                        CL*65
05448             MOVE BE-ADDRESS-LINE-2 TO ADDR2BO                        CL*65
05449 *           MOVE BE-CITY-STATE     TO CITYSTBO                       CL*65
                  STRING BE-CITY ' ' BE-STATE
                     DELIMITED BY '  ' INTO CITYSTBO
                  END-STRING
05450             MOVE BE-ZIP-CODE       TO ZIPBO                          CL*65
05451             GO TO 6061-READ-ACCOUNT.                                 CL*65
05452                                                                      CL*65
05453      IF PI-PAYEE-TYPE    = 'B' AND                                   CL*93
05454         PI-PAYEE-SEQ-NUM = 0                                         CL*93
05455           PERFORM 7920-READ-BENE THRU 7920-EXIT                      CL*93
05456           IF BENE-FOUND-SW = 'N'                                     CL*93
05457             GO TO 6065-NOT-FOUND                                  EL156
05458           ELSE                                                       CL*93
05459             MOVE BE-MAIL-TO-NAME   TO PI-PAYEE-NAME  NAMEBO       EL156
05460             MOVE BE-ADDRESS-LINE-1 TO ADDR1BO                     EL156
05461             MOVE BE-ADDRESS-LINE-2 TO ADDR2BO                     EL156
05462 *           MOVE BE-CITY-STATE     TO CITYSTBO                    EL156
                  STRING BE-CITY ' ' BE-STATE
                     DELIMITED BY '  ' INTO CITYSTBO
                  END-STRING
05463             MOVE BE-ZIP-CODE       TO ZIPBO
013017            if be-ach-yes-or-no = 'Y'
013017               move 'Y'          to pi-ach-payment
013017            else
013017               move 'N'          to pi-ach-payment
013017            end-if
05464             GO TO 6061-READ-ACCOUNT.                              EL156
05465                                                                   EL156
05466      EXEC CICS HANDLE CONDITION                                   EL156
05467           NOTFND(6065-NOT-FOUND)                                  EL156
05468       END-EXEC.                                                      CL*88
05469                                                                   EL156
05470      PERFORM 7950-READ-TRAILER THRU 7950-EXIT.                    EL156
05471                                                                   EL156
05472      MOVE AT-MAIL-TO-NAME        TO PI-PAYEE-NAME.                   CL*43
05473      MOVE AT-MAIL-TO-NAME        TO NAMEBO.                       EL156
05474      MOVE AT-ADDRESS-LINE-1      TO ADDR1BO.                      EL156
05475      MOVE AT-ADDRESS-LINE-2      TO ADDR2BO.                      EL156
05476 *    MOVE AT-CITY-STATE          TO CITYSTBO.                     EL156
           STRING AT-CITY ' ' AT-STATE
              DELIMITED BY '  ' INTO CITYSTBO
           END-STRING
05477      MOVE AT-ZIP-CODE            TO ZIPBO.                        EL156
05478                                                                   EL156
05479  6061-READ-ACCOUNT.                                               EL156
05480      EXEC CICS HANDLE CONDITION                                   EL156
05481           NOTFND(6068-ACCT-NOT-FND)                               EL156
05482       END-EXEC.                                                      CL*88
05483                                                                   EL156
05484      EXEC CICS READ                                               EL156
05485           DATASET('ERACCT')                                       EL156
05486           SET    (ADDRESS OF ACCOUNT-MASTER)                         CL*81
05487           RIDFLD (PI-ACCT-KEY)                                       CL*43
05488       END-EXEC.                                                      CL*88
05489                                                                   EL156
05490      MOVE AM-CARRIER             TO ACARRBO.                      EL156
05491      MOVE AM-GROUPING            TO GROUPBO.                      EL156
05492      MOVE AM-STATE               TO STATEBO.                      EL156
05493      MOVE AM-ACCOUNT             TO ACCTBO.                       EL156
05494      MOVE AM-NAME                TO ACCTNMBO.                     EL156
05495                                                                   EL156
05496      IF PI-PAYEE-TYPE = 'A' AND                                      CL*93
05497         PI-PAYEE-SEQ-NUM = 0                                         CL*93
05498           MOVE AM-NAME             TO NAMEBO                         CL*93
05499                                       PI-PAYEE-NAME                  CL*93
05500           MOVE AM-ADDRS            TO ADDR1BO                        CL*93
05501           MOVE SPACES              TO ADDR2BO                        CL*93
05502 *         MOVE AM-CITY             TO CITYSTBO                       CL*93
                STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
                   DELIMITED BY '  ' INTO CITYSTBO
                END-STRING
05503           MOVE AM-ZIP-PRIME        TO WS-ZIP                         CL*93
05504           MOVE AM-ZIP-PLUS4        TO WS-ZIP-EXT                     CL*93
05505           INSPECT WS-WORK-ZIP REPLACING ALL SPACES BY '0'            CL*93
05506           MOVE WS-NUMERIC-ZIP      TO ZIPBO.                         CL*93
091808

111714***  We don't really need the code below because we don't do the
111714***  AK thing anymore.  However, we may have a state special
111714***  of some sort in the future so why not just leave it??

091808     IF CITYSTBO EQUAL SPACES
091808         MOVE 'XX' TO PI-CHECK-STATE-FOR-AK
091808         GO TO 6070-FORMAT-DATA
091808     END-IF.
091808
091808     MOVE CITYSTBO TO WS-PAYEE-CITY-STATE.
091808     MOVE 'N' TO WS-PAYEE-STATE-FOUND.
091808     MOVE 0 TO WS-BEG-SUB WS-END-SUB.
091808     PERFORM VARYING WS-SUB FROM 30 BY -1
091808          UNTIL WS-SUB = 0 OR PAYEE-STATE-FOUND
091808        IF WS-END-SUB = 0  AND
110513           (WS-PAYEE-CITY-ST (WS-SUB) EQUAL SPACES
110513              OR LOW-VALUES  OR '.')
091808            CONTINUE
091808        ELSE
091808            IF WS-END-SUB = 0
091808                MOVE WS-SUB TO WS-END-SUB
091808            ELSE
091808                IF WS-PAYEE-CITY-ST (WS-SUB) = SPACES
091808                    COMPUTE WS-BEG-SUB = WS-SUB + 1 
091808                    MOVE 'Y' TO WS-PAYEE-STATE-FOUND
091808                END-IF
091808            END-IF
091808        END-IF
091808        IF WS-PAYEE-CITY-ST (WS-SUB) EQUAL ','
091808            COMPUTE WS-BEG-SUB = WS-SUB + 1 
091808            MOVE 'Y' TO WS-PAYEE-STATE-FOUND
091808        END-IF
091808     END-PERFORM.
091808
091808     IF WS-BEG-SUB > 0
091808         COMPUTE WS-STATE-LENGTH = WS-END-SUB - WS-BEG-SUB + 1
091808         IF WS-STATE-LENGTH = 2
091808             MOVE WS-PAYEE-CITY-STATE (WS-BEG-SUB:WS-STATE-LENGTH) 
091808                  TO PI-CHECK-STATE-FOR-AK
091808         ELSE
091808             IF WS-PAYEE-CITY-STATE (WS-BEG-SUB:WS-STATE-LENGTH) =
091808                'ALASKA'
091808                 MOVE 'AK' TO PI-CHECK-STATE-FOR-AK
091808             ELSE
091808                 MOVE 'XX' TO PI-CHECK-STATE-FOR-AK
091808             END-IF
091808         END-IF
091808     ELSE
091808         MOVE 'XX' TO PI-CHECK-STATE-FOR-AK
091808     END-IF.
05507                                                                   EL156
05508      GO TO 6070-FORMAT-DATA.                                      EL156
05509                                                                   EL156
05510  6065-NOT-FOUND.                                                  EL156
05511      MOVE 'NO ADDRESS RECORD FOUND'  TO ADDR1BO.                  EL156
05512      GO TO 6061-READ-ACCOUNT.                                     EL156
05513                                                                   EL156
05514  6068-ACCT-NOT-FND.                                               EL156
05515      MOVE 'NO ACCOUNT RECORD FOUND'  TO ACCTNMBO.                 EL156
05516                                                                   EL156
05517  6070-FORMAT-DATA.                                                EL156
05518      IF PI-HOLDTIL = ZEROS                                        EL156
05519         MOVE SAVE-DATE           TO PAYDTBI                       EL156
05520        ELSE                                                       EL156
05521         MOVE PI-HOLDTIL          TO PAYDTBO                       EL156
05522         INSPECT PAYDTBI REPLACING ALL SPACES BY '/'.                 CL*16
05523                                                                   EL156
052506     MOVE PI-PROOF-DATE          TO PROOFDBO.
052506     INSPECT PROOFDBI REPLACING ALL SPACES BY '/'.
052506
05524      IF EIBAID = DFHPF3 OR DFHPF4                                 EL156
05525         NEXT SENTENCE                                                CL*16
05526      ELSE                                                            CL*16
05527        GO TO 6071-CONTINUE.                                          CL*16
05528                                                                      CL*16
05529      MOVE PI-EPYFROM          TO PYFROMBO.                           CL*43
05530      INSPECT PYFROMBI REPLACING ALL SPACES BY '/'.                   CL*43
05531                                                                      CL*16
05532      IF NOT PI-USES-PAID-TO                                          CL*16
05533         MOVE PI-EPYTHRU          TO PYTHRUBO                      EL156
05534         INSPECT PYTHRUBI REPLACING ALL SPACES BY '/'                 CL*16
05535      ELSE                                                            CL*16
05536         MOVE PI-EPYTHRU       TO DC-GREG-DATE-1-MDY                  CL*16
05537         MOVE '4'              TO DC-OPTION-CODE                      CL*16
05538         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                CL*16
05539         IF NO-CONVERSION-ERROR                                       CL*16
05540            MOVE +1            TO DC-ELAPSED-DAYS                     CL*16
05541            MOVE +0            TO DC-ELAPSED-MONTHS                   CL*16
05542            MOVE '6'           TO DC-OPTION-CODE                      CL*16
05543            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             CL*16
05544            IF NO-CONVERSION-ERROR                                    CL*16
05545               MOVE DC-GREG-DATE-1-EDIT TO PYTHRUBI.                  CL*16
05546                                                                      CL*16
05547      MOVE PI-EDAYS            TO DAYSBO.                             CL*43
05548      MOVE PI-EPYAMT           TO PAYAMTBO.                           CL*43
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
              IF PI-INT-AMT > ZEROS
                 MOVE PI-INT-AMT       TO INTAMTBO
              END-IF
           END-IF

05549      GO TO 6072-SET-TYPE.                                            CL*16
05550                                                                      CL*16
05551  6071-CONTINUE.                                                      CL*16
05552                                                                   EL156
05553      IF PI-CPYFROM NOT = LOW-VALUES                               EL156
05554         MOVE PI-CPYFROM          TO DC-BIN-DATE-1                 EL156
05555         MOVE SPACES              TO DC-OPTION-CODE                EL156
05556         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             EL156
05557         MOVE DC-GREG-DATE-1-EDIT TO PYFROMBI.                     EL156
05558                                                                   EL156
05559      IF PI-CPYTHRU NOT = LOW-VALUES                               EL156
05560         IF NOT PI-USES-PAID-TO                                       CL*16
05561            MOVE PI-CPYTHRU         TO DC-BIN-DATE-1                  CL*16
05562            MOVE SPACES             TO DC-OPTION-CODE                 CL*16
05563            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             CL*16
05564            MOVE DC-GREG-DATE-1-EDIT TO PYTHRUBI                      CL*16
05565         ELSE                                                         CL*16
05566            MOVE PI-CPYTHRU         TO DC-BIN-DATE-1                  CL*16
05567            MOVE '6'                TO DC-OPTION-CODE                 CL*16
05568            MOVE +1                 TO DC-ELAPSED-DAYS                CL*16
05569            MOVE +0                 TO DC-ELAPSED-MONTHS              CL*16
05570            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             CL*16
05571            MOVE DC-GREG-DATE-1-EDIT TO PYTHRUBI.                     CL*16
05572                                                                   EL156
05573      MOVE PI-CDAYS               TO DAYSBO.                       EL156
05574      MOVE PI-CPYAMT              TO PAYAMTBO.                     EL156
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
              IF PI-INT-AMT > ZEROS
                 MOVE PI-INT-AMT       TO INTAMTBO
              END-IF
           END-IF

           .
05576  6072-SET-TYPE.                                                   EL156
05577      IF PI-PMTTYPE = '1'                                          EL156
05578         MOVE 'PARTIAL'           TO PAYTYPBO.                     EL156
05579                                                                   EL156
05580      IF PI-PMTTYPE = '2'                                          EL156
05581         MOVE 'FINAL'             TO PAYTYPBO.                     EL156
05582                                                                   EL156
05583      IF PI-PMTTYPE = '3'                                          EL156
05584         MOVE 'LUMP SUM'          TO PAYTYPBO.                     EL156
05585                                                                   EL156
05586      IF PI-PMTTYPE = '4'                                          EL156
05587         MOVE 'ADDITIONAL'        TO PAYTYPBO.                     EL156
05588                                                                   EL156
05589      IF PI-PMTTYPE = '5'                                          EL156
05590         MOVE 'CHARGEABLE'        TO PAYTYPBO.                     EL156
05591                                                                   EL156
05592      IF PI-PMTTYPE = '6'                                          EL156
05593         MOVE 'NON-CHARGEABLE'    TO PAYTYPBO.                     EL156
05594                                                                   EL156
05595      IF PI-PROV-PMT                                                  CL*65
05596         MOVE 'PROVISIONAL   '    TO PAYTYPBO.                        CL*65
05597                                                                   EL156
05598      IF PI-ECHECKNO NOT = SPACES                                  EL156
05599         MOVE PI-ECHECKNO         TO CKNUMBO                       EL156
05600      ELSE                                                         EL156
05601         MOVE PI-CCHECKNO         TO CKNUMBO.                      EL156
05602                                                                   EL156
120115     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121           OR 'FNL'
CIDMOD        MOVE ELMSTR-KEY          TO  ELTRLR-KEY                      CL*25
CIDMOD        MOVE +91                 TO  TRLR-SEQ-NO                     CL*25
CIDMOD        EXEC CICS READ                                               EL156
CIDMOD            DATASET  ('ELTRLR')                                      EL156
CIDMOD            SET      (ADDRESS OF ACTIVITY-TRAILERS)                     CL
CIDMOD            RIDFLD   (ELTRLR-KEY)                                    EL156
CIDMOD            RESP     (WS-RESPONSE)
CIDMOD        END-EXEC                                                        CL
CIDMOD                                                                  EL156
CIDMOD        IF WS-RESP-NORMAL
CIDMOD           MOVE AT-INFO-LINE-1   TO  LOANNBO                         CL*25
CIDMOD        ELSE
CIDMOD           MOVE SPACES           TO  LOANNBO
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
CIDMOD*       MOVE PI-LOAN-NO          TO LOANNBO
CIDMOD
05603      IF CKNUMBO = SPACES                                          EL156
05604          MOVE SPACES             TO CKNUMHDO.                     EL156
05605                                                                   EL156
05606      MOVE CL-CLAIM-NO            TO CLMNOBO.                      EL156
05607      MOVE CL-CARRIER             TO CARRBO.                       EL156
05608      MOVE CL-CERT-PRIME          TO CERTNOBO.                     EL156
05609      MOVE CL-CERT-SFX            TO SUFXBO.                       EL156
05610                                                                   EL156
121802     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122                                          OR 'B' or 'H'

              EVALUATE CL-CLAIM-TYPE
                 WHEN 'I'
121802              MOVE '  IU  '      TO CLMTYPBO
                 WHEN 'G'
                    MOVE ' GAP  '      TO CLMTYPBO
                 WHEN 'F'
                    MOVE ' FMLA '      TO CLMTYPBO
022122           WHEN 'B'
022122              MOVE ' BRV  '      TO CLMTYPBO
022122           WHEN 'H'
022122              MOVE ' HOSP '      TO CLMTYPBO
                 WHEN OTHER
121802              MOVE PI-AH-OVERRIDE-L6
                                       TO CLMTYPBO
121802        END-EVALUATE

05612         MOVE ZEROS               TO UNDISBO                          CL*91
121802*       MOVE PI-AH-OVERRIDE-L6   TO CLMTYPBO                      EL156
05614         IF PI-PMTTYPE = '5' OR '6'                                   CL*93
05615            GO TO 6073-GET-NOTIFY-DATA                                CL*25
05616         ELSE                                                      EL156
05617            MOVE PI-DAILY-RATE    TO RATEBO                        EL156
05618            GO TO 6073-GET-NOTIFY-DATA.                               CL*25
05619                                                                   EL156
05620      MOVE PI-LIFE-OVERRIDE-L6    TO CLMTYPBO.                     EL156
05621                                                                   EL156
05622      IF (PI-PMTTYPE = '5' OR '6') OR EIBAID = DFHPF5              EL156
05623         MOVE ZEROS               TO UNDISBO                       EL156
05624      ELSE                                                         EL156
05625         COMPUTE UNDISBO = PI-CPYAMT - PI-EPYAMT.                  EL156
05626                                                                   EL156
05627  6073-GET-NOTIFY-DATA.                                               CL*25
05628                                                                      CL*25
05629      MOVE 'N'                    TO  PI-3RD-PARTY-SW.                CL*71
05630                                                                      CL*71
05631      IF AM-NOTIF-OF-PAYMENTS NOT = 'Y'                               CL*88
05632          MOVE SPACES             TO  NAMEB2O                         CL*25
05633                                      ADDR1B2O                        CL*25
05634                                      ADDR2B2O                        CL*25
05635                                      CITYST2O                        CL*25
05637                                      AGTHDGO                         CL*26
CIDMOD         MOVE ZEROS              TO  ZIPB2O
05638          GO TO 6080-SET-INCURRED.                                    CL*25
05639                                                                      CL*25
05640      MOVE ELMSTR-KEY             TO  ELTRLR-KEY.                     CL*25
05641      MOVE +29                    TO  TRLR-SEQ-NO.                    CL*25
05642                                                                      CL*25
05643      EXEC CICS HANDLE CONDITION                                      CL*25
05644          NOTFND   (6074-GET-COMP-DATA)                               CL*25
05645      END-EXEC.                                                       CL*25
05646                                                                      CL*25
05647      PERFORM 7950-READ-TRAILER THRU 7950-EXIT.                       CL*25
05648                                                                      CL*25
05649      MOVE AT-MAIL-TO-NAME        TO  NAMEB2O.                        CL*25
05650      MOVE AT-ADDRESS-LINE-1      TO  ADDR1B2O.                       CL*25
05651      MOVE AT-ADDRESS-LINE-2      TO  ADDR2B2O.                       CL*25
05652 *    MOVE AT-CITY-STATE          TO  CITYST2O.                       CL*25
           STRING AT-CITY ' ' AT-STATE
              DELIMITED BY '  ' INTO CITYST2O
           END-STRING
05653      MOVE AT-ZIP-CODE            TO  ZIPB2O.                         CL*25
05654      GO TO 6080-SET-INCURRED.                                        CL*25
05655                                                                      CL*25
05656  6074-GET-COMP-DATA.                                                 CL*25
05657                                                                      CL*25
05658      IF AM-3RD-PARTY-NOTIF-LEVEL IS NOT NUMERIC                      CL*25
05659          MOVE ZEROS              TO  AM-3RD-PARTY-NOTIF-LEVEL.       CL*30
05660                                                                      CL*25
05661      IF AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) = ZEROS OR SPACES          CL*93
05662          MOVE 'NO ADDRESS AVAILABLE' TO  NAMEB2O                     CL*25
05663                                          ADDR1B2O                    CL*25
05664                                          ADDR2B2O                    CL*25
05665                                          CITYST2O                    CL*25
05666          MOVE ZEROS                  TO  ZIPB2O                      CL*25
05667          GO TO 6080-SET-INCURRED.                                    CL*25
05668                                                                      CL*25
05669      MOVE 'Y'                    TO  PI-3RD-PARTY-SW.                CL*71
05670      MOVE AM-COMPANY-CD          TO  COMP-COMP-CD.                   CL*25
05671      MOVE AM-CARRIER             TO  COMP-CARRIER.                   CL*25
05672      MOVE AM-GROUPING            TO  COMP-GROUP.                     CL*25
05673      MOVE 'A'                    TO  COMP-TYPE.                      CL*25
05674      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)                          CL*25
05675                                  TO  COMP-FIN-RESP.                  CL*25
05676                                                                      CL*43
05677      IF AM-3RD-PARTY-NOTIF-LEVEL = AM-REMIT-TO                       CL*88
05678          IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' OR                 CL*93
052814                                       'G' OR 'B' or 'S'             CL*93
05680              MOVE 'G'            TO  COMP-TYPE                       CL*37
05681              MOVE LOW-VALUES     TO  COMP-ACCOUNT                    CL*37
05682          ELSE                                                        CL*37
05683              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)                  CL*47
05684                                  TO  COMP-ACCOUNT                    CL*47
05685      ELSE                                                            CL*28
05686          MOVE 'G'                TO  COMP-TYPE                       CL*28
05687          MOVE LOW-VALUES         TO  COMP-ACCOUNT.                   CL*28
05688                                                                      CL*25
05689      PERFORM 7800-READ-COMP-MSTR THRU 7800-COMP-EXIT.                CL*25
05690                                                                      CL*25
05691      IF COMP-REC-FOUND                                               CL*25
05692          MOVE CO-ACCT-NAME       TO  NAMEB2O                         CL*25
05693          MOVE CO-ADDR-1          TO  ADDR1B2O                        CL*25
05694          MOVE CO-ADDR-2          TO  ADDR2B2O                        CL*25
05695 *        MOVE CO-ADDR-3          TO  CITYST2O                        CL*25
               STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
                  DELIMITED BY '  ' INTO CITYST2O
               END-STRING
05696          MOVE CO-ZIP             TO  ZIPB2O                          CL*25
05697      ELSE                                                            CL*25
05698          MOVE 'NO COMP ADDRESS AVAILABLE'                            CL*25
05699                                  TO  NAMEB2O                         CL*25
05700                                      ADDR1B2O                        CL*25
05701                                      ADDR2B2O                        CL*25
05702                                      CITYST2O                        CL*25
05703          MOVE ZEROS               TO ZIPB2O.                         CL*28
05704                                                                   EL156
05705  6080-SET-INCURRED.                                               EL156
05706      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.                EL156
05707      MOVE SPACES                 TO DC-OPTION-CODE.               EL156
05708      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
05709      MOVE DC-GREG-DATE-1-EDIT    TO INCURBO.                      EL156
05710      MOVE PI-PROCESSOR-ID        TO BYBO.                         EL156
05711      MOVE MAP-NAMEB              TO MAP-NAME.                     EL156
05712      MOVE 'C'                    TO PI-PASS-SW.                   EL156
05713      MOVE EIBAID                 TO PI-PFKEY-USED.                EL156
05714      MOVE -1                     TO ENTPFBL.                         CL*43
05715      GO TO 8100-SEND-INITIAL-MAP.                                 EL156
05716                                                                   EL156
05717      EJECT                                                        EL156
05718  6100-BUILD-PAYMENT-TRAILER.                                      EL156
091808
05720      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
05721      MOVE '6'                    TO CNTL-REC-TYPE.                EL156
05722                                                                   EL156
05723      IF CONTROL-IS-ACTUAL-CARRIER                                 EL156
05724          MOVE PI-CARRIER         TO WS-CARR                       EL156
05725      ELSE                                                         EL156
05726          MOVE PI-CARRIER-CONTROL-LEVEL TO WS-CARR.                EL156
05727                                                                   EL156
05728      IF PI-COMPANY-ID = 'FIA'                                        CL*88
05729          MOVE PI-CARRIER         TO  WS-CARR.                        CL*30
05730                                                                      CL*30
05731      MOVE WS-CARR-ACCESS         TO CNTL-ACCESS.                  EL156
05732      MOVE 0                      TO CNTL-SEQ-NO.                  EL156
05733      MOVE 'CARR'                 TO FILE-SWITCH.                  EL156
05734                                                                   EL156
05735      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.                    EL156
05736                                                                      CL*65
05737      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
05738          IF PI-OFFLINE     = 'Y' AND                                 CL*93
05739             PI-ECHECKNO-CR = 'CR'                                    CL*93
05740               GO TO 6110-READ-CONTROL                                CL*65
05741          ELSE                                                        CL*65
05742             IF PI-CASH = 'N'                                         CL*93
05743                 GO TO 6110-READ-CONTROL.                             CL*65
05744                                                                   EL156
05745      IF CHECK-NO-MANUAL    OR                                     EL156
05746         CHECK-NO-AT-PRINT  OR                                     EL156
05747         PI-OFFLINE = 'Y'                                          EL156
05748           GO TO 6130-BUILD-TRAILER.                                  CL*93

05750      IF PI-COMPANY-ID = 'FIA'                                        CL*88
05751          NEXT SENTENCE                                               CL*30
05752      ELSE                                                            CL*30
05753          IF NOT CONTROL-IS-ACTUAL-CARRIER                            CL*30
05754              GO TO 6110-READ-CONTROL.                                CL*30
05755                                                                   EL156
05756      PERFORM 7940-READ-CONTROL-UPDATE THRU 7940-EXIT.             EL156
05757                                                                   EL156
05758      IF CHECK-CNT-RESET-VALUE                                     EL156
05759         MOVE +1                  TO CF-CHECK-COUNTER              EL156
05760      ELSE                                                         EL156
05761         ADD +1                   TO CF-CHECK-COUNTER.             EL156
05762                                                                   EL156
05763      MOVE CF-CHECK-COUNTER       TO WS-CHECK-NUMERIC.             EL156
05764                                                                   EL156
05765      IF CHECK-NO-CARR-SEQ                                         EL156
05766         MOVE PI-CARRIER          TO WS-CHECK-CARR.                EL156
05767                                                                   EL156
05768      MOVE WS-CHECK-AREA          TO PI-CCHECKNO.                  EL156
032813
032813     MOVE 'N' TO PI-REAUDIT-NEEDED.
032813     IF CF-CHECK-COUNTER >= CF-CARRIER-NEXT-AUDIT-CHK-NO  AND
032813        CF-CARRIER-NEXT-AUDIT-CHK-NO > ZERO
032813         MOVE 'Y' TO PI-REAUDIT-NEEDED
032813         COMPUTE WRK-NEXT-AUDIT-CHK-NO = 
032813            CF-CHECK-COUNTER + PI-REAUDIT-INTERVAL
032813         MOVE WRK-NEXT-AUDIT-CHK-NO TO 
032813                      CF-CARRIER-NEXT-AUDIT-CHK-NO
032813     END-IF.
032813
05769      GO TO 6120-REWRITE.                                          EL156
091808
091808 6105-STATE-CHECK-NUMBER.
091808     IF CHECK-NO-MANUAL    OR        
091808        CHECK-NO-AT-PRINT  OR        
091808        PI-OFFLINE = 'Y'            
091808           GO TO 6130-BUILD-TRAILER
091808     END-IF. 
091808
091808     MOVE PI-COMPANY-ID      TO CNTL-COMP-ID.
091808     MOVE PI-CHECK-STATE-FOR-AK TO WS-ST-ACCESS.
091808     MOVE WS-STATE-ACCESS    TO CNTL-ACCESS.
091808     MOVE '3'                TO CNTL-REC-TYPE.
091808     MOVE +0                 TO CNTL-SEQ-NO.
091808     MOVE 'STAT'             TO FILE-SWITCH.
091808     PERFORM 7940-READ-CONTROL-UPDATE THRU 7940-EXIT.
091808
091808     IF CF-ST-CHECK-CNT-RESET
091808         MOVE +1             TO CF-ST-CHECK-COUNTER
091808     ELSE 
091808         ADD +1              TO CF-ST-CHECK-COUNTER
091808     END-IF.
091808
091808     MOVE CF-ST-CHECK-COUNTER TO WS-CHECK-NUMERIC.
091808     MOVE WS-CHECK-AREA       TO PI-CCHECKNO.
091808     GO TO 6120-REWRITE.
05770                                                                   EL156
05771  6110-READ-CONTROL.                                               EL156
05772      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
05773      MOVE '1'                    TO CNTL-REC-TYPE.                EL156
05774      MOVE SPACES                 TO CNTL-ACCESS.                  EL156
05775      MOVE 0                      TO CNTL-SEQ-NO.                  EL156
05776                                                                   EL156
05777      PERFORM 7940-READ-CONTROL-UPDATE THRU 7940-EXIT.             EL156
05778                                                                   EL156
05779      IF CO-CHECK-COUNT-RESET                                      EL156
05780         MOVE +1                  TO CF-CO-CHECK-COUNTER           EL156
05781      ELSE                                                            CL*21
05782         ADD +1                   TO CF-CO-CHECK-COUNTER.          EL156
05783                                                                   EL156
05784      MOVE CF-CO-CHECK-COUNTER    TO WS-CHECK-NUMERIC.             EL156
05785                                                                      CL*65
05786      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
05787          IF PI-OFFLINE     = 'Y'  AND                                CL*93
05788             PI-ECHECKNO-CR = 'CR'                                    CL*93
05789               MOVE WS-A-CHECK-NUM-5   TO PI-ECHECKNO-5               CL*65
05790               MOVE PI-ECHECKNO        TO PI-CCHECKNO CHECKNOO        CL*93
05791               MOVE AL-UANON           TO CHECKNOA                    CL*65
05792               GO TO 6120-REWRITE                                     CL*65
05793          ELSE                                                        CL*65
05794          IF PI-CASH = 'N'                                            CL*93
05795              MOVE 'N'                TO PI-ECHECKNO-N                CL*65
05796              MOVE WS-A-CHECK-NUM-6   TO PI-ECHECKNO-6                CL*65
05797              MOVE PI-ECHECKNO        TO PI-CCHECKNO CHECKNOO         CL*93
05798              MOVE AL-UANON           TO CHECKNOA                     CL*65
05799              GO TO 6120-REWRITE.                                     CL*65
05800                                                                      CL*65
05801      MOVE WS-CHECK-NUMERIC       TO PI-CCHECKNO.                  EL156
05802                                                                   EL156
05803  6120-REWRITE.                                                    EL156
05804      EXEC CICS REWRITE                                            EL156
05805           DATASET('ELCNTL')                                       EL156
05806           FROM(CONTROL-FILE)                                      EL156
05807       END-EXEC.                                                      CL*88
05808                                                                   EL156
05809      MOVE 'Y'                    TO PI-CK-ASSIGN.                 EL156
05810                                                                   EL156
05811      EJECT                                                           CL*65
05812  6130-BUILD-TRAILER.                                              EL156
05813                                                                      CL**2
05814      EXEC CICS GETMAIN                                            EL156
05815           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL*81
05816           LENGTH    (200)                                            CL**2
05817           INITIMG   (GETMAIN-SPACE)                                  CL**2
05818       END-EXEC.                                                      CL*88
05819                                                                   EL156
05820      MOVE LOW-VALUES             TO AT-PREV-LAST-PMT-DT              CL*43
05821                                     AT-PREV-PAID-THRU-DT.            CL*43
052506     MOVE LOW-VALUES             TO AT-PMT-PROOF-DT.
05822      MOVE ZEROS                  TO AT-PREV-LAST-PMT-AMT.         EL156
05823      MOVE ELMSTR-KEY             TO AT-CONTROL-PRIMARY.           EL156
05824      MOVE 'AT'                   TO AT-RECORD-ID.                 EL156
05825      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.               EL156
05826                                                                      CL*16
05827      IF (PI-PMTNOTE1 = SPACES OR LOW-VALUES) AND                     CL*93
05828         (PI-PMTNOTE2 = SPACES OR LOW-VALUES)                         CL*93
05829            MOVE +0               TO AT-PAYMENT-NOTE-SEQ-NO           CL*93
05830      ELSE                                                            CL*16
05831            MOVE AT-SEQUENCE-NO   TO AT-PAYMENT-NOTE-SEQ-NO           CL*93
05832            SUBTRACT +1         FROM AT-PAYMENT-NOTE-SEQ-NO.          CL*93
05833                                                                      CL*68
05834      IF PI-COMPANY-ID = 'LAP' OR 'RMC'                               CL*68
05835          IF AT-PAYMENT-NOTE-SEQ-NO = ZERO                            CL*68
05836              MOVE AT-SEQUENCE-NO TO AT-FORM-CTL-SEQ-NO               CL*68
05837              SUBTRACT +1       FROM AT-FORM-CTL-SEQ-NO               CL*68
05838            ELSE                                                      CL*68
05839              MOVE AT-PAYMENT-NOTE-SEQ-NO TO AT-FORM-CTL-SEQ-NO       CL*68
05840              SUBTRACT +1               FROM AT-FORM-CTL-SEQ-NO.      CL*68
05841                                                                      CL*16
05842      MOVE '2'                    TO AT-TRAILER-TYPE.              EL156
05843      MOVE WS-TODAY-DATE          TO AT-RECORDED-DT                   CL*16
05844                                     AT-PAYMENT-LAST-MAINT-DT.        CL*43
05845      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                   CL*16
05846                                     AT-PAYMENT-LAST-UPDATED-BY.      CL*43
05847      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.         EL156
05848      MOVE PI-PMTTYPE             TO AT-PAYMENT-TYPE.              EL156
05849      MOVE CL-CLAIM-TYPE          TO AT-CLAIM-TYPE.                EL156
05850      MOVE CL-CLAIM-PREM-TYPE     TO AT-CLAIM-PREM-TYPE.           EL156
05851                                                                      CL*16
05852      IF PI-PMT-APPROVED                                              CL*43
05853         IF PI-OFFLINE  = 'N'                                         CL*43
05854            MOVE 'U'              TO AT-PAYMENT-APPROVAL-SW.          CL*16
05855                                                                   EL156
05856      IF PI-PFKEY-USED = DFHPF5                                    EL156
05857         GO TO 6140-PF5-ENTERED.                                   EL156
05858                                                                   EL156
05859      MOVE PI-EEXPENS             TO AT-EXPENSE-PER-PMT.           EL156
05860      MOVE PI-EPYAMT              TO AT-AMOUNT-PAID.               EL156
05861                                                                      CL*65
05862      IF PI-AIG-SPECIAL-BENEFIT                                       CL*65
05863          IF WS-1ST-SPECIAL-PMT                                       CL*65
05864              COMPUTE AT-AMOUNT-PAID ROUNDED = PI-EPYAMT / 1.5        CL*65
05865              COMPUTE PI-REDUND-PMTAMT = PI-EPYAMT -                  CL*65
05866                                         AT-AMOUNT-PAID               CL*65
05867          ELSE                                                        CL*65
05868              MOVE PI-REDUND-PMTAMT TO AT-AMOUNT-PAID.                CL*65
05869                                                                      CL*65
05870      MOVE PI-ETYPE               TO AT-EXPENSE-TYPE.              EL156
05871                                                                   EL156
05872      MOVE PI-EPYFROM             TO DC-GREG-DATE-1-MDY.           EL156
05873      MOVE '4'                    TO DC-OPTION-CODE.               EL156
05874      PERFORM 9700-LINK-DATE-CONVERT.                              EL156
05875                                                                   EL156
05876      IF DATE-CONVERSION-ERROR                                     EL156
05877         MOVE LOW-VALUES          TO AT-PAID-FROM-DT               EL156
05878      ELSE                                                            CL*16
05879         MOVE DC-BIN-DATE-1       TO AT-PAID-FROM-DT.              EL156
05880                                                                   EL156
05881      MOVE PI-EPYTHRU             TO DC-GREG-DATE-1-MDY.           EL156
05882      MOVE '4'                    TO DC-OPTION-CODE.               EL156
05883      PERFORM 9700-LINK-DATE-CONVERT.                              EL156
05884                                                                   EL156
05885      IF DATE-CONVERSION-ERROR                                     EL156
05886         MOVE LOW-VALUES          TO AT-PAID-THRU-DT               EL156
05887      ELSE                                                            CL*16
05888         MOVE DC-BIN-DATE-1       TO AT-PAID-THRU-DT.              EL156
05889                                                                   EL156
05890      MOVE PI-EDAYS               TO AT-DAYS-IN-PERIOD
013017     move pi-ach-payment         to at-ach-payment
05891      GO TO 6150-FINISH-MOVE.                                      EL156
05892                                                                   EL156
05893  6140-PF5-ENTERED.                                                EL156
05894      MOVE PI-CEXPENS             TO AT-EXPENSE-PER-PMT.           EL156
05895      MOVE PI-CPYAMT              TO AT-AMOUNT-PAID.               EL156
05896                                                                      CL*65
05897      IF PI-AIG-SPECIAL-BENEFIT                                       CL*65
05898          IF WS-1ST-SPECIAL-PMT                                       CL*65
05899              COMPUTE AT-AMOUNT-PAID ROUNDED = PI-CPYAMT / 1.5        CL*65
05900              COMPUTE PI-REDUND-PMTAMT = PI-CPYAMT -                  CL*65
05901                                         AT-AMOUNT-PAID               CL*65
05902          ELSE                                                        CL*65
05903              MOVE PI-REDUND-PMTAMT TO AT-AMOUNT-PAID.                CL*65
05904                                                                      CL*65
05905      MOVE PI-CPYFROM             TO AT-PAID-FROM-DT.              EL156
05906      MOVE PI-CPYTHRU             TO AT-PAID-THRU-DT.              EL156
05907      MOVE PI-CDAYS               TO AT-DAYS-IN-PERIOD.            EL156
05908                                                                   EL156
05909  6150-FINISH-MOVE.                                                EL156
05910                                                                      CL*16
05911      IF PI-ECHECKNO = SPACES                                      EL156
05912          MOVE PI-CCHECKNO    TO AT-CHECK-NO                          CL*65
05913      ELSE                                                         EL156
05914          MOVE PI-ECHECKNO    TO AT-CHECK-NO.                         CL*65
05915                                                                   EL156
05916      MOVE PI-PAYEE               TO AT-PAYEE-TYPE-CD.             EL156
05917      MOVE PI-PAYEE-NAME          TO AT-PAYEES-NAME.               EL156
05918                                                                      CL*16
020413*    MOVE PI-GROUPED             TO AT-GROUPED-PAYMENT.              CL*16
020413*    MOVE PI-CASH                TO AT-CASH-PAYMENT.                 CL*16
05921                                                                      CL*65
05922      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*88
05923          MOVE CL-ASSOCIATES      TO AT-ASSOCIATES.                   CL*65
05924                                                                   EL156
05925      IF PI-OFFLINE = 'N'                                          EL156
05926         MOVE '1'                 TO AT-PAYMENT-ORIGIN             EL156
05927         MOVE LOW-VALUES          TO AT-PMT-SELECT-DT                 CL**5
05928      ELSE                                                         EL156
05929         MOVE PI-MONTH-END-SAVE   TO AT-PMT-SELECT-DT                 CL**5
05930         MOVE '3'                 TO AT-PAYMENT-ORIGIN.            EL156
05931                                                                      CL*65
013017     move pi-ach-payment         to at-ach-payment
05932      MOVE SPACES                 TO AT-AIG-UNEMP-IND.                CL*65
05933                                                                      CL*65
05934      IF WS-2ND-SPECIAL-PMT                                           CL*65
05935          MOVE 'Y'                TO AT-CASH-PAYMENT                  CL*65
05936          MOVE SPACES             TO AT-CHECK-NO                      CL*65
05937          MOVE '3'                TO AT-PAYMENT-ORIGIN                CL*65
05938          MOVE PI-MONTH-END-SAVE  TO AT-PMT-SELECT-DT                 CL*65
05939          MOVE 'U'                TO AT-AIG-UNEMP-IND                 CL*65
05940          MOVE 'I'                TO AT-PAYEE-TYPE-CD.                CL*65
05941                                                                   EL156
05942      MOVE LOW-VALUES             TO AT-CHECK-WRITTEN-DT           EL156
05943                                     AT-TO-BE-WRITTEN-DT.          EL156
05944                                                                   EL156
05945      IF PI-HOLDTIL NOT = ZEROS                                    EL156
05946         MOVE PI-HOLDTIL          TO DC-GREG-DATE-1-MDY            EL156
05947         MOVE '4'                 TO DC-OPTION-CODE                EL156
05948         PERFORM 9700-LINK-DATE-CONVERT                            EL156
05949         IF PI-OFFLINE = 'N'                                       EL156
05950            MOVE DC-BIN-DATE-1    TO AT-TO-BE-WRITTEN-DT              CL*93
031218           MOVE 'P'              TO WS-HOLD-UNTIL-SW
05951         ELSE                                                      EL156
05952            MOVE DC-BIN-DATE-1    TO AT-CHECK-WRITTEN-DT.             CL*93
05953                                                                   EL156
05954      MOVE PI-ERESV               TO AT-ADDL-RESERVE.              EL156
05955      MOVE ZEROS                  TO AT-CHECK-QUE-CONTROL             CL*43
05956                                     AT-CHECK-QUE-SEQUENCE.           CL*43
05957                                                                   EL156
05958      IF PI-PFKEY-USED = DFHPF4                                    EL156
05959         MOVE '1'                 TO AT-FORCE-CONTROL.             EL156
05960                                                                   EL156
05961      IF PI-PMTTYPE NOT = '4' AND '5' AND '6'                      EL156
05962         MOVE CL-LAST-PMT-DT      TO AT-PREV-LAST-PMT-DT           EL156
05963         MOVE CL-PAID-THRU-DT     TO AT-PREV-PAID-THRU-DT          EL156
05964         MOVE CL-LAST-PMT-AMT     TO AT-PREV-LAST-PMT-AMT.         EL156
05965                                                                   EL156
05966      MOVE LOW-VALUES             TO AT-PMT-ACCEPT-DT              EL156
05967                                     AT-VOID-SELECT-DT             EL156
05968                                     AT-VOID-ACCEPT-DT             EL156
05969                                     AT-VOID-DT.                   EL156

121802     IF (CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122         OR 'B' OR 'H')
05972         AND (PI-PMTTYPE NOT = '4' AND '5' AND '6') 
05973         MOVE PI-DAILY-RATE       TO AT-DAILY-RATE
05974         MOVE PI-BEN-DAYS         TO AT-ELIMINATION-DAYS
05975         MOVE PI-BEN-TYPE         TO AT-BENEFIT-TYPE
05976      ELSE
05977         MOVE ZEROS               TO AT-DAILY-RATE
05978                                     AT-ELIMINATION-DAYS
05979         MOVE SPACES              TO AT-BENEFIT-TYPE
           END-IF

052506     MOVE PI-PROOF-DATE          TO DC-GREG-DATE-1-MDY.           EL156
052506     MOVE '4'                    TO DC-OPTION-CODE.               EL156
052506     PERFORM 9700-LINK-DATE-CONVERT.                              EL156
052506     IF DATE-CONVERSION-ERROR                                     EL156
052506         MOVE LOW-VALUES          TO AT-PMT-PROOF-DT              EL156
052506     ELSE                                                            CL*16
052506         MOVE DC-BIN-DATE-1       TO AT-PMT-PROOF-DT              EL156
052506     END-IF.

           MOVE PI-PRINT-EOB-YN        TO AT-PRINT-EOB-WITH-CHECK
020413     MOVE PI-PRINT-CLM-FRM-YN    TO AT-PRINT-CLM-FORM
020413     MOVE PI-PRINT-SURVEY-YN     TO AT-PRINT-SURVEY
102413     MOVE PI-SPECIAL-RELEASE-YN  TO AT-SPECIAL-RELEASE

032813     IF (PI-REAUDIT-NEEDED = 'Y')
              or (pi-max-ext-used = 'Y')
052814        OR (PI-APPROVAL-3550-NEEDED = 'Y')
043019        OR PI-DUPE-APPROVAL-NEEDED = 'Y'
110515         PERFORM 6155-READ-CONTROL
110515         PERFORM 6157-SET-APP-LEVEL
110515         IF AT-APPROVAL-LEVEL-REQD < '4'
111113            MOVE '4' TO AT-APPROVAL-LEVEL-REQD
110515         END-IF
032813         MOVE 'Y' TO WS-NEED-APPROVAL
032813         GO TO 6170-WRITE
032813     END-IF.
05981 *******************************************************              CL*43
05982      IF NOT PI-PMT-GRADUATED                                         CL*43
05983          GO TO 6170-WRITE.                                           CL*43

110515     PERFORM 6155-READ-CONTROL.
05992                                                                      CL*47
05993      IF PAID-FOR-LIFE                                                CL*47
05994          IF AT-AMOUNT-PAID < PI-MAX-LF-PMT-TOL                       CL*94
05995              MOVE 'A'            TO  AT-PAYMENT-APPROVAL-SW          CL*47
05996                                      WS-PMT-APPROVAL-SW              CL*47
05997              GO TO 6170-WRITE.                                       CL*47
05998                                                                      CL*47
031808*05999      IF PAID-FOR-AH                                                  CL*47
031808     IF (PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP) 
06000          IF AT-AMOUNT-PAID < PI-MAX-PMT-TOL                          CL*94
06001              MOVE 'A'            TO  AT-PAYMENT-APPROVAL-SW          CL*47
06002                                      WS-PMT-APPROVAL-SW              CL*47
06003              GO TO 6170-WRITE.                                       CL*47
06004                                                                      CL*43
06005      MOVE '1'                    TO AT-APPROVED-LEVEL.               CL*43
06006                                                                      CL*43
110515     PERFORM 6157-SET-APP-LEVEL.
031808
031808     MOVE AT-APPROVAL-LEVEL-REQD TO WS-SAVE-APPROVAL-LEVEL-REQD.
06022                                                                      CL*65
06023      IF NOT PI-PMT-GRADUATED                                         CL*65
06024          GO TO 6170-WRITE.                                           CL*65
06025                                                                      CL*65
06026      EXEC CICS HANDLE CONDITION                                      CL*65
06027          NOTFND   (6170-WRITE)                                       CL*65
06028      END-EXEC.                                                       CL*65
06029                                                                      CL*65
06030      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                    CL*65
06031      MOVE '2'                    TO CNTL-REC-TYPE.                   CL*65
06032      MOVE +0                     TO CNTL-SEQ-NO.                     CL*65
06033      MOVE PI-PROCESSOR-ID        TO CNTL-ACCESS.                     CL*65
06034                                                                      CL*65
06035      EXEC CICS READ                                                  CL*65
06036           DATASET  ('ELCNTL')                                        CL*65
06037           SET      (ADDRESS OF CONTROL-FILE)                         CL*81
06038           RIDFLD   (ELCNTL-KEY)                                      CL*65
06039      END-EXEC.                                                       CL*65
06040                                                                      CL*65
06041      MOVE CF-APPROVAL-LEVEL TO WS-APPROVAL-LEVEL.                    CL*65
06042                                                                      CL*65
06043      IF WS-APPROVAL-LEVEL = '1' OR '2' OR '3'                        CL*93
091813                         OR '4' OR '5'
06044          NEXT SENTENCE                                               CL*65
06045      ELSE                                                            CL*65
031808         MOVE 'Y' TO WS-NEED-APPROVAL
06046          GO TO 6170-WRITE.                                           CL*65
06047                                                                      CL*65
06048      IF WS-APPROVAL-LEVEL < AT-APPROVAL-LEVEL-REQD                   CL*94
031808         MOVE 'Y' TO WS-NEED-APPROVAL
06049          IF WS-APPROVAL-LEVEL = '1'                                  CL*93
06050              MOVE '2'        TO  AT-APPROVED-LEVEL                   CL*65
031808                            WS-SAVE-APPROVED-LEVEL
06051              GO TO 6170-WRITE                                        CL*65
06052          ELSE                                                        CL*65
06053              IF WS-APPROVAL-LEVEL = '2'                              CL*93
06054                  MOVE '3'        TO  AT-APPROVED-LEVEL               CL*65
031808                                WS-SAVE-APPROVED-LEVEL
06055                  GO TO 6170-WRITE                                    CL*65
031808             ELSE IF WS-APPROVAL-LEVEL = '3'
031808                 MOVE '4'        TO  AT-APPROVED-LEVEL
031808                                WS-SAVE-APPROVED-LEVEL
091813                 GO TO 6170-WRITE
091813             ELSE IF WS-APPROVAL-LEVEL = '4'
091813                 MOVE '5'        TO  AT-APPROVED-LEVEL
091813                                WS-SAVE-APPROVED-LEVEL
06057                  GO TO 6170-WRITE.                                   CL*65
06058                                                                      CL*65
06059      MOVE 'A'            TO  AT-PAYMENT-APPROVAL-SW                  CL*65
06060                              WS-PMT-APPROVAL-SW.                     CL*65
06061      MOVE SPACES         TO  AT-APPROVED-LEVEL                       CL*65
06062                              AT-APPROVAL-LEVEL-REQD.                 CL*65
06063                                                                      CL*65
06064 *******************************************************              CL*43
110515 6155-READ-CONTROL.
05985      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
05986      MOVE '1'                    TO CNTL-REC-TYPE.
05987      MOVE SPACES                 TO CNTL-ACCESS.
05988      MOVE +0                     TO CNTL-SEQ-NO.
05989      MOVE 'COMP'                 TO FILE-SWITCH.
05990
05991      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.

110515 6157-SET-APP-LEVEL.

031808******A&H Limits are set as a monthly payment.  Calculate the monthly
031808******equivalent of the amount paid today
031808     IF PI-SAVE-ELAPSED-MONTHS > 0 OR
031808        PI-SAVE-ODD-DAYS-OVER > 0
031808           COMPUTE WS-CALC-PMT-MONTHS = (PI-SAVE-ODD-DAYS-OVER
031808                               / 30) + PI-SAVE-ELAPSED-MONTHS
031808           COMPUTE WS-CALC-MO-AH-AMT ROUNDED = AT-AMOUNT-PAID /
031808                                  WS-CALC-PMT-MONTHS
031808     ELSE
031808           MOVE AT-AMOUNT-PAID TO WS-CALC-MO-AH-AMT
031808     END-IF.
031808*06007      IF ((PAID-FOR-AH) AND
031808     IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
031808*06008         (CF-AH-PAY-APP-LEVEL-1 >= AT-AMOUNT-PAID))
031808        (CF-AH-PAY-APP-LEVEL-1 >= WS-CALC-MO-AH-AMT))
06009               OR
100518        ((PAID-FOR-LIFE OR PAID-FOR-OTH) AND
06011         (CF-LIFE-PAY-APP-LEVEL-1 >= AT-AMOUNT-PAID))
06012             MOVE '1'         TO AT-APPROVAL-LEVEL-REQD
06013        ELSE
031808*06014      IF ((PAID-FOR-AH)  AND
031808         IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
031808*06015         (CF-AH-PAY-APP-LEVEL-2 >= AT-AMOUNT-PAID))
031808           (CF-AH-PAY-APP-LEVEL-2 >= WS-CALC-MO-AH-AMT))
06016               OR
100518        ((PAID-FOR-LIFE OR PAID-FOR-OTH) AND
06018         (CF-LIFE-PAY-APP-LEVEL-2 >= AT-AMOUNT-PAID))
06019             MOVE '2'         TO AT-APPROVAL-LEVEL-REQD
06020          ELSE
031808*06021             MOVE '3'         TO AT-APPROVAL-LEVEL-REQD.
031808           IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617                OR PAID-FOR-FAM
022122                OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
031808               (CF-AH-PAY-APP-LEVEL-3 >= WS-CALC-MO-AH-AMT))
031808              OR
100518          ((PAID-FOR-LIFE OR PAID-FOR-OTH) AND
031808          (CF-LIFE-PAY-APP-LEVEL-3 >= AT-AMOUNT-PAID))
031808            MOVE '3'         TO AT-APPROVAL-LEVEL-REQD
031808       ELSE
091813           IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617                OR PAID-FOR-FAM
022122                OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
091813               (CF-AH-PAY-APP-LEVEL-4 >= WS-CALC-MO-AH-AMT))
091813              OR
100518          ((PAID-FOR-LIFE OR PAID-FOR-OTH) AND
091813          (CF-LIFE-PAY-APP-LEVEL-4 >= AT-AMOUNT-PAID))
091813            MOVE '4'         TO AT-APPROVAL-LEVEL-REQD
091813       ELSE
091813            MOVE '5'         TO AT-APPROVAL-LEVEL-REQD.
031808
031808     IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
031808        (CF-AH-APP-DAY-LEVEL-1 >= AT-DAYS-IN-PERIOD))
031808        IF ('1' >= AT-APPROVAL-LEVEL-REQD)
031808            MOVE '1'         TO AT-APPROVAL-LEVEL-REQD
031808        END-IF
031808     ELSE
031808     IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
031808        (CF-AH-APP-DAY-LEVEL-2 >= AT-DAYS-IN-PERIOD))
031808        IF ('2' >= AT-APPROVAL-LEVEL-REQD)
031808            MOVE '2'         TO AT-APPROVAL-LEVEL-REQD
031808        END-IF
031808     ELSE
031808     IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
031808        (CF-AH-APP-DAY-LEVEL-3 >= AT-DAYS-IN-PERIOD))
031808        IF ('3' >= AT-APPROVAL-LEVEL-REQD)
031808            MOVE '3'         TO AT-APPROVAL-LEVEL-REQD
031808        END-IF
031808     ELSE
091813     IF ((PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS) AND
091813        (CF-AH-APP-DAY-LEVEL-4 >= AT-DAYS-IN-PERIOD))
091813        IF ('4' >= AT-APPROVAL-LEVEL-REQD)
091813            MOVE '4'         TO AT-APPROVAL-LEVEL-REQD
091813        END-IF
091813     ELSE
031808     IF (PAID-FOR-AH OR PAID-FOR-IUI OR PAID-FOR-GAP
102617           OR PAID-FOR-FAM
022122           OR PAID-FOR-BRV OR PAID-FOR-HOS)
091813            MOVE '5'         TO AT-APPROVAL-LEVEL-REQD.

06065                                                                   EL156
06066  6170-WRITE.                                                      EL156
06067                                                                      CL*94
120115     if pi-company-id = 'DCC' or 'VPP'
061013        PERFORM 0900-GET-DDF-limits
061013                                 THRU 0900-EXIT
061013     end-if

061013     perform 2220-update-elcrtt  thru 2220-exit

06068      IF PI-COMPANY-ID = 'DMD'                                        CL*94
06069         IF PI-AT-EOB-CODES NOT = SPACES                              CL*94
06070            MOVE PI-AT-EOB-CODE (1) TO AT-EOB-CODE1                   CL*94
06071            MOVE PI-AT-EOB-CODE (2) TO AT-EOB-CODE2                   CL*94
06072            MOVE PI-AT-EOB-CODE (3) TO AT-EOB-CODE3                   CL*94
06073            MOVE PI-AT-EOB-CODE (4) TO AT-EOB-CODE4                   CL*94
06074            MOVE PI-AT-EOB-CODE (5) TO AT-EOB-CODE5.                  CL*94
06075                                                                   EL156
06076      EXEC CICS HANDLE CONDITION                                      CL*49
06077          DUPREC    (6180-DUPREC)                                     CL*49
06078      END-EXEC.                                                       CL*49
06079                                                                      CL*49
06080      EXEC CICS WRITE                                              EL156
06081           DATASET       ('ELTRLR')                                   CL*16
06082           FROM          (ACTIVITY-TRAILERS)                          CL*16
06083           RIDFLD        (AT-CONTROL-PRIMARY)                         CL*16
06084       END-EXEC.                                                      CL*88
06085                                                                      CL*16
06086      IF (PI-PMTNOTE1 = SPACES OR LOW-VALUES) AND                     CL*93
06087         (PI-PMTNOTE2 = SPACES OR LOW-VALUES)                         CL*93
061010*06088           GO TO 6199-EXIT.                                           CL*93
061010           GO TO 6176-APPROVAL-NOTE.
06089                                                                      CL*16
06090      EXEC CICS GETMAIN                                               CL*16
06091           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL*81
06092           LENGTH    (200)                                            CL*16
06093           INITIMG   (GETMAIN-SPACE)                                  CL*16
06094       END-EXEC.                                                      CL*88
06095                                                                      CL*16
06096      SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT.                            CL*16
06097                                                                      CL*16
06098      MOVE ELMSTR-KEY             TO AT-CONTROL-PRIMARY.              CL*16
06099      MOVE 'AT'                   TO AT-RECORD-ID.                    CL*16
06100      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.                  CL*16
06101      MOVE '6'                    TO AT-TRAILER-TYPE.                 CL*16
06102      MOVE WS-TODAY-DATE          TO AT-RECORDED-DT                   CL*16
06103                                     AT-GEN-INFO-LAST-MAINT-DT        CL*16
06104      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                   CL*16
06105                                     AT-GEN-INFO-LAST-UPDATED-BY      CL*16
06106      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.            CL*16
06107                                                                      CL*65
06108      MOVE PI-PMTNOTE1            TO AT-INFO-LINE-1.                  CL*16
06109      MOVE PI-PMTNOTE2            TO AT-INFO-LINE-2
           MOVE PI-EOB-CODES-EXIST     TO AT-EOB-CODES-EXIST
06110                                                                      CL*65
06111      MOVE 'P'                    TO AT-INFO-TRAILER-TYPE.            CL*16
06112                                                                      CL*16
06113  6175-WRITE.                                                         CL*16
06114                                                                      CL*16
06115      EXEC CICS HANDLE CONDITION                                      CL*49
06116          DUPREC    (6190-DUPREC)                                     CL*49
06117      END-EXEC.                                                       CL*49
06118                                                                      CL*49
06119      EXEC CICS WRITE                                                 CL*16
06120           DATASET     ('ELTRLR')                                     CL*16
06121           FROM        (ACTIVITY-TRAILERS)                            CL*16
06122           RIDFLD      (AT-CONTROL-PRIMARY)                           CL*16
06123       END-EXEC.                                                      CL*88
06124                                                                   EL156
061010 6176-APPROVAL-NOTE.
061010
061010     IF NOT APPROVAL-NEEDED
061010         GO TO 6199-EXIT.
061010
061010     EXEC CICS GETMAIN                                  
061010         SET       (ADDRESS OF ACTIVITY-TRAILERS)        
061010         LENGTH    (200)                                 
061010         INITIMG   (GETMAIN-SPACE)                       
061010     END-EXEC.
061010                                        
061010     SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT
061010                                                      
061010     MOVE ELMSTR-KEY         TO AT-CONTROL-PRIMARY
061010     MOVE 'AT'               TO AT-RECORD-ID
061010     MOVE CL-TRAILER-SEQ-CNT TO AT-SEQUENCE-NO
061010     MOVE '6'                TO AT-TRAILER-TYPE
061010     MOVE WS-TODAY-DATE      TO AT-RECORDED-DT 
061010                                AT-GEN-INFO-LAST-MAINT-DT 
061010     MOVE PI-PROCESSOR-ID    TO AT-RECORDED-BY            
061010                                AT-GEN-INFO-LAST-UPDATED-BY
061010     MOVE EIBTIME            TO AT-LAST-MAINT-HHMMSS
061010                                                        
032813     IF PI-REAUDIT-NEEDED = 'Y'
032813         MOVE WS-REAUDIT-NOTE TO AT-INFO-LINE-1
032813     ELSE
032813         MOVE WS-APPROV-NOTE  TO AT-INFO-LINE-1
032813     END-IF
052814     IF PI-APPROVAL-3550-NEEDED = 'Y'
052814         MOVE WS-3550-APPROV-NOTE TO AT-INFO-LINE-1
052814     END-IF
043019     IF PI-DUPE-APPROVAL-NEEDED = 'Y'
043019         MOVE WS-DUPE-APPROV-NOTE TO AT-INFO-LINE-1
043019     END-IF
043019
061010     MOVE SPACES             TO AT-INFO-LINE-2
061010                                              
061010     MOVE 'R'                TO AT-INFO-TRAILER-TYPE.
061010 6177-WRITE.
061010                                                                     CL*16
061010     EXEC CICS HANDLE CONDITION                                      CL*49
061010         DUPREC    (6185-DUPREC)                                     CL*49
061010     END-EXEC.                                                       CL*49
061010                                                                     CL*49
061010     EXEC CICS WRITE                                                 CL*16
061010          DATASET     ('ELTRLR')                                     CL*16
061010          FROM        (ACTIVITY-TRAILERS)                            CL*16
061010          RIDFLD      (AT-CONTROL-PRIMARY)                           CL*16
061010      END-EXEC.                                                      CL*88
031808                                                                  EL156
031808
06125      GO TO 6199-EXIT.                                             EL156
06126                                                                   EL156
06127  6180-DUPREC.                                                     EL156
06128                                                                      CL*49
06129      SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT.                         EL156
06130      MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO.              EL156
06131      GO TO 6170-WRITE.                                            EL156
031808
031808 6185-DUPREC.                                        
031808     SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT.            
031808     MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO. 
031808     GO TO 6177-WRITE.           
031808
06133  6190-DUPREC.                                                        CL*16
06134      SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT.                            CL*16
06135      MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO.                 CL*16
06136      GO TO 6175-WRITE.                                               CL*16
06137                                                                   EL156
06138  6199-EXIT.                                                       EL156
06139       EXIT.                                                       EL156
06140                                                                   EL156
06141      EJECT                                                        EL156
06142  6200-UPDATE-CLAIM-MSTR.                                          EL156
06143      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.                 EL156
06144      MOVE PI-CARRIER             TO MSTR-CARRIER.                    CL*43
06145      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.                EL156
06146      MOVE PI-CERT-NO             TO MSTR-CERT-NO.                 EL156
06147                                                                   EL156
06148      PERFORM 7910-READ-CLAIM-UPDATE THRU 7910-EXIT.               EL156
06149                                                                      CL*82
06150      MOVE ZEROS                  TO W-INTEREST.                      CL*82
06151                                                                   EL156
120115     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121           OR 'FNL'
PEMMOD        IF (PI-OFFLINE = 'Y') OR
PEMMOD           (PI-PMTTYPE = '7')
CIDMOD           PERFORM 9870-OUTPUT-ACTIVITY-RECORD                         000
PEMMOD                                 THRU 9870-EXIT                        000
PEMMOD        END-IF
PEMMOD     END-IF
CIDMOD     IF NOT ERROR-ON-OUTPUT                                            000
CIDMOD        CONTINUE                                                       000
CIDMOD     ELSE                                                              000
CIDMOD        EXEC CICS REWRITE                                              000
CIDMOD           DATASET('ELMSTR')                                           000
CIDMOD           FROM(CLAIM-MASTER)                                          000
CIDMOD        END-EXEC                                                       000
CIDMOD
CIDMOD        MOVE -1                  TO ENTPFBL                            000
CIDMOD        MOVE AL-UANON            TO ENTPFBA                            000
CIDMOD        MOVE MAP-NAMEB           TO MAP-NAME                           000
CIDMOD        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                       000
CIDMOD        GO TO 8200-SEND-DATAONLY                                       000
CIDMOD     END-IF                                                            000
CIDMOD
06152      SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.                             CL*97
06153                                                                   EL156
06154      MOVE SPACES TO WS-PAYMENT-TRAILER-SW.                           CL*65
06155                                                                      CL*97
06156      IF PI-AIG-SPECIAL-BENEFIT                                       CL*65
06157          MOVE '1' TO WS-PAYMENT-TRAILER-SW.                          CL*65
06158                                                                      CL*65
06159      PERFORM 6100-BUILD-PAYMENT-TRAILER THRU 6199-EXIT.           EL156
06160                                                                   EL156
06161      IF PI-SPECIAL-BEN-SW = 'Y'                                      CL*93
06162          MOVE '2' TO WS-PAYMENT-TRAILER-SW                           CL*65
06163          PERFORM 6130-BUILD-TRAILER THRU 6199-EXIT.                  CL*65
06164                                                                      CL*65
06165      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*88
06166          PERFORM 6400-UPDATE-ZERO-TRAILER THRU 6499-EXIT             CL*65
06167      ELSE                                                            CL*65
06168          IF PI-PMTTYPE NOT = '4'                                     CL*65
06169             PERFORM 6400-UPDATE-ZERO-TRAILER THRU 6499-EXIT.         CL*65
06170                                                                   EL156
06171      IF PI-PMTTYPE = '5' OR '6'                                   EL156
06172         GO TO  6250-SET-MAINT.                                    EL156
06173                                                                   EL156
06174      MOVE ZEROS                      TO  CL-LAPSE-REPORT-CODE        CL*70
06175                                          CL-LAG-REPORT-CODE.         CL*70
06176                                                                      CL*70
06177      IF CL-ACTIVITY-CODE = 09                                        CL*88
06178          GO TO 6200-CONT-UPDATE.                                     CL*65
06179                                                                      CL*65
06180      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
06181          IF CL-ACTIVITY-CODE = 11                                    CL*88
06182              GO TO 6200-CONT-UPDATE.                                 CL*65
06183                                                                      CL*65
06184      IF WS-ACT-REC-FOUND-SW = 'N'                                    CL*88
06185          GO TO 6200-CONT-UPDATE.                                     CL*65
06186                                                                      CL*65
06187      IF PI-RESET-SW = 'Y' OR PI-PROV-PMT                             CL*88
06188          MOVE WS-TODAY-DATE          TO  CL-ACTIVITY-MAINT-DT        CL*65
06189          MOVE 'PMT'                  TO  CL-ACTIVITY-MAINT-TYPE      CL*65
06190          MOVE WS-ACTIVITY-CODE       TO  CL-ACTIVITY-CODE.           CL*65
06191                                                                      CL*65
06192      IF WS-UPDATE-SW = 'Y'                                           CL*88
06193          MOVE LOW-VALUES             TO  CL-NEXT-RESEND-DT           CL*65
06194                                          CL-NEXT-FOLLOWUP-DT.        CL*65
06195                                                                      CL*65
06196  6200-CONT-UPDATE.                                                   CL*65
06197                                                                      CL*65
06198      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
06199        IF CLAIM-IS-CLOSED                                            CL*65
06200          IF PI-PMTTYPE = '2' OR '3' OR '4'                           CL*93
06201            IF WS-OPEN-CLOSE-SW = '2'                                 CL*88
06202              MOVE WS-TODAY-DATE     TO  CL-LAST-REOPEN-DT            CL*65
06203                                         CL-LAST-CLOSE-DT.            CL*65
06204                                                                      CL*65
06205      IF CLAIM-IS-CLOSED AND PI-PMTTYPE = '1'                         CL*65
06206         MOVE WS-TODAY-DATE       TO CL-LAST-REOPEN-DT             EL156
06207         MOVE 'O'                 TO CL-CLAIM-STATUS.              EL156
06208                                                                   EL156
06209      IF PI-PMTTYPE = ('2' OR '3') AND CLAIM-IS-OPEN
06210         MOVE WS-TODAY-DATE       TO CL-LAST-CLOSE-DT
06211         MOVE '1'                 TO CL-LAST-CLOSE-REASON
06212         MOVE 'C'                 TO CL-CLAIM-STATUS
06213         IF PI-PMTTYPE = '2'
121802           AND (CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
022122              or 'F' OR 'B' OR 'H')
121802           CONTINUE
06216         ELSE
06217            PERFORM 6300-UPDATE-CERT THRU 6399-EXIT
              END-IF
06218      ELSE
06219         IF PI-PMTTYPE = '2' OR '3'
06220            IF PI-PMTTYPE = '2' AND
121802              (CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
022122                 or 'F' OR 'B' OR 'H')
121802              CONTINUE
06223            ELSE
06224               PERFORM 6300-UPDATE-CERT THRU 6399-EXIT
                 END-IF
              END-IF 
           END-IF
06225                                                                   EL156
06226      IF PI-PMTTYPE = '4'                                          EL156
06227         PERFORM 6300-UPDATE-CERT THRU 6399-EXIT.                  EL156
06228                                                                   EL156
06229      IF PI-COMPANY-ID = 'CVL'                                        CL*88
06230          IF PI-LOAN-UPDATE-SW = 'Y'                                  CL*88
06231              MOVE PI-COMPANY-CD      TO  CERT-COMP-CD                CL*78
06232              MOVE CL-CERT-CARRIER    TO  CERT-CARRIER                CL*78
06233              MOVE CL-CERT-GROUPING   TO  CERT-GROUPING               CL*78
06234              MOVE CL-CERT-STATE      TO  CERT-STATE                  CL*78
06235              MOVE CL-CERT-ACCOUNT    TO  CERT-ACCOUNT                CL*78
06236              MOVE CL-CERT-EFF-DT     TO  CERT-EFF-DT                 CL*78
06237              MOVE CL-CERT-NO         TO  CERT-CERT-NO                CL*78
06238              MOVE 'CERT'             TO  FILE-SWITCH                 CL*78
06239              PERFORM 7980-READ-CERT-UPDATE THRU 7980-EXIT            CL*78
06240              MOVE PI-LOAN-NO         TO  CM-BENEFICIARY              CL*78
06241              PERFORM 6350-REWRITE-CERT THRU 6399-EXIT.               CL*78
06242                                                                      CL*78
06243      IF PI-PFKEY-USED = DFHPF5 AND PI-PROCESSOR-ID NOT = 'FIX '   EL156
06244         GO TO 6220-USE-COMPUTED.                                  EL156
06245                                                                   EL156
06246      IF (PI-PMTTYPE = '4')  AND                                      CL*65
06247         (PI-EPYAMT < ZEROS)                                          CL*94
06248           COMPUTE CL-NO-OF-DAYS-PAID =                               CL*65
06249                        CL-NO-OF-DAYS-PAID - PI-EDAYS                 CL*65
06250           IF CL-NO-OF-DAYS-PAID < +0                                 CL*94
06251               MOVE ZEROS TO CL-NO-OF-DAYS-PAID                       CL*65
06252           ELSE                                                       CL*65
06253               NEXT SENTENCE                                          CL*65
06254      ELSE                                                            CL*65
06255           ADD PI-EDAYS           TO CL-NO-OF-DAYS-PAID.              CL*65
06256                                                                   EL156
031218     IF HOLD-UNTIL-PMT
022718        CONTINUE
022718     ELSE
06257      IF PI-PROCESSOR-ID = 'FIX '                                  EL156
06258         IF CL-TOTAL-PAID-AMT NOT = ZEROS                          EL156
06259            NEXT SENTENCE                                          EL156
06260         ELSE                                                      EL156
06261            ADD PI-EPYAMT         TO CL-TOTAL-PAID-AMT             EL156
06262      ELSE                                                         EL156
06263         ADD PI-EPYAMT            TO CL-TOTAL-PAID-AMT.            EL156
06264                                                                   EL156
06265      IF PI-PMTTYPE = '4'                                          EL156
06266         GO TO 6250-SET-MAINT.                                     EL156
06267                                                                   EL156
06270      MOVE PI-EPYTHRU             TO DC-GREG-DATE-1-MDY.           EL156
06271      MOVE '4'                    TO DC-OPTION-CODE.               EL156
06272      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL156
031218     IF HOLD-UNTIL-PMT
022718        CONTINUE
022718     ELSE
06273      IF DATE-CONVERSION-ERROR                                     EL156
06274         MOVE LOW-VALUES          TO CL-PAID-THRU-DT               EL156
06275      ELSE                                                         EL156
022718        ADD 1                 TO CL-NO-OF-PMTS-MADE               EL156
06276         IF DC-BIN-DATE-1 > CL-PAID-THRU-DT                           CL*94
06277            MOVE DC-BIN-DATE-1    TO CL-PAID-THRU-DT               EL156
06278            IF PI-AIG-SPECIAL-BENEFIT                                 CL*65
06279                COMPUTE CL-LAST-PMT-AMT = PI-EPYAMT / 1.5             CL*65
06280            ELSE                                                      CL*65
06281                MOVE PI-EPYAMT        TO CL-LAST-PMT-AMT.             CL*65
06282                                                                   EL156
06283      GO TO 6250-SET-MAINT.                                        EL156
06284                                                                   EL156
06285  6220-USE-COMPUTED.                                               EL156
06286                                                                      CL*65
06287      IF (PI-PMTTYPE = '4')  AND                                      CL*65
06288         (PI-CPYAMT < ZEROS)                                          CL*94
06289           COMPUTE CL-NO-OF-DAYS-PAID =                               CL*65
06290                        CL-NO-OF-DAYS-PAID - PI-CDAYS                 CL*65
06291           IF CL-NO-OF-DAYS-PAID < +0                                 CL*94
06292               MOVE ZEROS TO CL-NO-OF-DAYS-PAID                       CL*65
06293           ELSE                                                       CL*65
06294               NEXT SENTENCE                                          CL*65
06295      ELSE                                                            CL*65
06296           ADD PI-CDAYS           TO CL-NO-OF-DAYS-PAID.              CL*65
06297                                                                      CL*65
031218     IF NOT HOLD-UNTIL-PMT
022718        ADD 1                    TO CL-NO-OF-PMTS-MADE
06298         ADD PI-CPYAMT            TO CL-TOTAL-PAID-AMT.            EL156
06299                                                                   EL156
06300      IF PI-PMTTYPE = '4'                                          EL156
06301         GO TO 6250-SET-MAINT.                                     EL156
06302                                                                   EL156
06303      IF PI-AIG-SPECIAL-BENEFIT                                       CL*65
06304          COMPUTE CL-LAST-PMT-AMT = PI-CPYAMT / 1.5                   CL*65
06305      ELSE                                                            CL*65
06306          MOVE PI-CPYAMT        TO CL-LAST-PMT-AMT.                   CL*65
06307                                                                      CL*65
06310      IF PI-CPYTHRU > CL-PAID-THRU-DT                                 CL*94
031218       AND NOT HOLD-UNTIL-PMT
06311         MOVE PI-CPYTHRU          TO CL-PAID-THRU-DT.              EL156
06312                                                                   EL156
06313  6250-SET-MAINT.                                                  EL156
06314      MOVE WS-TODAY-DATE          TO CL-LAST-MAINT-DT.             EL156
06315                                                                   EL156
06316      IF PI-PROCESSOR-ID = 'FIX '                                  EL156
06317         IF DC-BIN-DATE-1 = CL-PAID-THRU-DT                        EL156
06318            MOVE WS-TODAY-DATE    TO CL-LAST-PMT-DT                EL156
06319         ELSE                                                      EL156
06320            NEXT SENTENCE                                          EL156
06321      ELSE                                                         EL156
06322         MOVE WS-TODAY-DATE       TO CL-LAST-PMT-DT.               EL156
06323                                                                   EL156
06324      MOVE PI-PROCESSOR-ID        TO CL-LAST-MAINT-USER.           EL156
06325      MOVE EIBTIME                TO CL-LAST-MAINT-HHMMSS.         EL156
06326      MOVE '1'                    TO CL-LAST-MAINT-TYPE.           EL156
06327                                                                   EL156
06328  6299-EXIT.                                                       EL156
06329       EXIT.                                                       EL156
06330      EJECT                                                        EL156
06331  6300-UPDATE-CERT.                                                EL156
041710
041710*  ADD CERTNOTE FOR SC NP+6
041710     IF PI-COMPANY-ID = 'CID' AND
041710       (PI-PMTTYPE = '2' OR '3') AND
041710        CL-CERT-STATE = 'SC'  AND
041710        (PI-LF-BENEFIT-CD = '2I' OR '2J' OR '2K' OR '2L')
041710           PERFORM 7180-ADD-CERT-NOTE THRU 7189-EXIT
041710     END-IF.
041710
030512     IF PI-COMPANY-ID = 'AHL' AND
030512       (PI-PMTTYPE = '2' OR '3') AND
030512        (PI-LF-BENEFIT-CD = '5I' OR '6J' OR '5M' OR '6M' OR
                 '5G' OR '5J' OR '5S' OR '6D' OR '6H' OR '6R')
030512           PERFORM 7180-ADD-CERT-NOTE THRU 7189-EXIT
030512     END-IF.
030512
06332      MOVE PI-COMPANY-CD    TO CERT-COMP-CD.                       EL156
06333      MOVE CL-CERT-CARRIER  TO CERT-CARRIER.                       EL156
06334      MOVE CL-CERT-GROUPING TO CERT-GROUPING.                      EL156
06335      MOVE CL-CERT-STATE    TO CERT-STATE.                         EL156
06336      MOVE CL-CERT-ACCOUNT  TO CERT-ACCOUNT.                       EL156
06337      MOVE CL-CERT-EFF-DT   TO CERT-EFF-DT.                        EL156
06338      MOVE CL-CERT-NO       TO CERT-CERT-NO.                       EL156
06339      MOVE 'CERT'           TO FILE-SWITCH.                        EL156
06340                                                                   EL156
06341      PERFORM 7980-READ-CERT-UPDATE THRU 7980-EXIT.                EL156
041710
041710     IF CERT-NOTE-ADDED
041710        IF CM-NOTE-SW = ' '
041710           MOVE '4'             TO CM-NOTE-SW
041710        ELSE
041710           IF CM-NOTE-SW = '1'
041710               MOVE '5'           TO CM-NOTE-SW
041710           ELSE
041710              IF CM-NOTE-SW = '2'
041710                 MOVE '6'         TO CM-NOTE-SW
041710              ELSE
041710                 IF CM-NOTE-SW = '3'
041710                    MOVE '7'      TO CM-NOTE-SW
041710                 END-IF
041710              END-IF
041710           END-IF
041710        END-IF
041710     END-IF.
06342                                                                      CL*78
06343      IF PI-COMPANY-ID = 'CVL'                                        CL*88
06344          MOVE PI-LOAN-NO   TO  CM-BENEFICIARY                        CL*78
06345          MOVE 'N'          TO  PI-LOAN-UPDATE-SW.                    CL*78
06346                                                                   EL156
06347      IF PI-PMTTYPE NOT = '4'                                      EL156
06348         GO TO 6340-UPDATE-CERT.                                   EL156
06349                                                                   EL156
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                EL156
06351         IF PI-PFKEY-USED = DFHPF5                                 EL156
06352            ADD PI-CPYAMT         TO CM-LF-ITD-DEATH-AMT           EL156
06353         ELSE                                                      EL156
06354            ADD PI-EPYAMT         TO CM-LF-ITD-DEATH-AMT.             CL*28
06355                                                                      CL*28
121802*    IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1                          CL*88
121802*        IF PI-COMPANY-ID = 'ACC' OR 'FDL'                           CL*88
121802*            IF PI-LIFE-OVERRIDE-L1 = 'P' OR                         CL*88
121802*               PI-LF-COVERAGE-TYPE = 'P'                            CL*88
121802*                GO TO 6350-REWRITE-CERT.                            CL*39
06361                                                                      CL*35
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                   CL*88
06363        IF PI-LIFE-OVERRIDE-L1 = 'P' OR                               CL*88
06364           PI-LF-COVERAGE-TYPE = 'P'                                  CL*88
06365            IF CM-LF-ITD-DEATH-AMT >= CM-LF-BENEFIT-AMT               CL*94
06366                 MOVE CL-INCURRED-DT       TO CM-LF-DEATH-DT          CL*39
06367                 MOVE PI-MONTH-END-SAVE    TO CM-LF-DEATH-EXIT-DT     CL*39
06368                 MOVE CM-LF-CURRENT-STATUS TO CM-LF-STATUS-AT-DEATH   CL*39
06369                 MOVE '7'                  TO CM-LF-CURRENT-STATUS    CL*43
06370                 GO TO 6350-REWRITE-CERT                              CL*39
06371             ELSE                                                     CL*39
06372                 GO TO 6350-REWRITE-CERT.                             CL*39
06373                                                                      CL*28
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                   CL*88
06375          GO TO 6350-REWRITE-CERT.                                    CL*28
06376                                                                   EL156
06377      IF NOT CM-AH-LUMP-SUM-DISAB                                  EL156
06378         GO TO 6380-UNLOCK-CERT.                                      CL*43
06379                                                                   EL156
06380      IF PI-PFKEY-USED = DFHPF5                                    EL156
06381         ADD PI-CPYAMT            TO CM-AH-ITD-LUMP-PMT            EL156
06382                                     CM-AH-ITD-AH-PMT                 CL*39
06383         MOVE PI-EPYTHRU          TO CM-AH-PAID-THRU-DT               CL*39
06384         GO TO 6350-REWRITE-CERT                                   EL156
06385      ELSE                                                         EL156
06386         ADD PI-EPYAMT            TO CM-AH-ITD-LUMP-PMT            EL156
06387                                     CM-AH-ITD-AH-PMT                 CL*39
06388         MOVE PI-EPYTHRU          TO CM-AH-PAID-THRU-DT               CL*39
06389         GO TO 6350-REWRITE-CERT.                                  EL156
06390                                                                   EL156
06391  6340-UPDATE-CERT.                                                EL156
06392                                                                      CL*25
100518     IF CL-CLAIM-TYPE = 'L' OR 'O'                                   CL*88
06394          IF PI-PFKEY-USED = DFHPF5                                   CL*25
06395              ADD PI-CPYAMT          TO CM-LF-ITD-DEATH-AMT           CL*25
06396          ELSE                                                        CL*25
06397              ADD PI-EPYAMT          TO CM-LF-ITD-DEATH-AMT.          CL*25
06398                                                                      CL*35
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                   CL*88
06400          IF PI-COMPANY-ID = 'ACC' OR 'FDL'                           CL*88
06401              IF PI-LIFE-OVERRIDE-L1 = 'P' OR                         CL*88
06402                 PI-LF-COVERAGE-TYPE = 'P'                            CL*88
06403                  GO TO 6350-REWRITE-CERT.                            CL*39
06404                                                                      CL*25
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                   CL*88
06406        IF PI-LIFE-OVERRIDE-L1 = 'P' OR                               CL*88
06407           PI-LF-COVERAGE-TYPE = 'P'                                  CL*88
06408            IF CM-LF-ITD-DEATH-AMT >= CM-LF-BENEFIT-AMT               CL*94
06409                 MOVE CL-INCURRED-DT       TO CM-LF-DEATH-DT          CL*39
06410                 MOVE PI-MONTH-END-SAVE    TO CM-LF-DEATH-EXIT-DT     CL*39
06411                 MOVE CM-LF-CURRENT-STATUS TO CM-LF-STATUS-AT-DEATH   CL*39
06412                 MOVE '7'                  TO CM-LF-CURRENT-STATUS    CL*43
06413                 GO TO 6350-REWRITE-CERT.                             CL*39
06414                                                                      CL*25
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1                          CL*88
06416          IF PI-LIFE-OVERRIDE-L1 = ('L' OR 'O') AND                   CL*88
06417             PI-LF-COVERAGE-TYPE NOT = 'P'                            CL*88
06418              MOVE CL-INCURRED-DT         TO CM-LF-DEATH-DT           CL*39
06419              MOVE PI-MONTH-END-SAVE      TO CM-LF-DEATH-EXIT-DT      CL*39
06420              MOVE CM-LF-CURRENT-STATUS   TO CM-LF-STATUS-AT-DEATH    CL*39
06421              MOVE '7'                    TO CM-LF-CURRENT-STATUS     CL*43
06422              GO TO 6350-REWRITE-CERT.                                CL*39
06423                                                                      CL*25
100518     IF CL-CLAIM-TYPE = 'O'                                          CL*88
100518         IF PI-LIFE-OVERRIDE-L1 = ('L' OR 'O') AND                   CL*88
100518            PI-LF-COVERAGE-TYPE NOT = 'P'                            CL*88
100518             MOVE CL-INCURRED-DT         TO CM-LF-CANCEL-DT          CL*39
100518             MOVE PI-MONTH-END-SAVE      TO CM-LF-CANCEL-EXIT-DT     CL*39
100518             MOVE CM-LF-CURRENT-STATUS   TO CM-LF-STATUS-AT-CANCEL   CL*39
100518             MOVE '6'            TO CM-LF-CURRENT-STATUS             CL*43
100518             GO TO 6350-REWRITE-CERT.                                CL*39
100518                                                                     CL*25
121802     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122                                          OR 'B' or 'H'
06425          MOVE CL-INCURRED-DT         TO CM-AH-SETTLEMENT-DT          CL*25
06426          MOVE PI-MONTH-END-SAVE      TO CM-AH-SETTLEMENT-EXIT-DT     CL*25
06427          MOVE CM-AH-CURRENT-STATUS   TO CM-AH-STATUS-AT-SETTLEMENT   CL*39
06428          MOVE '6'                    TO CM-AH-CURRENT-STATUS         CL*25
06429          MOVE PI-EPYTHRU             TO CM-AH-PAID-THRU-DT           CL*39
06430          IF PI-PFKEY-USED = DFHPF5                                   CL*25
06431              ADD PI-CPYAMT           TO CM-AH-ITD-LUMP-PMT           CL*25
06432                                         CM-AH-ITD-AH-PMT             CL*39
06433              GO TO 6350-REWRITE-CERT                                 CL*25
06434          ELSE                                                        CL*25
06435              ADD PI-EPYAMT           TO CM-AH-ITD-LUMP-PMT           CL*25
06436                                         CM-AH-ITD-AH-PMT             CL*39
06437              GO TO 6350-REWRITE-CERT.                                CL*25
06438                                                                   EL156
06439  6350-REWRITE-CERT.                                               EL156
06440                                                                   EL156
06441      EXEC CICS REWRITE                                            EL156
06442           DATASET  ('ELCERT')                                     EL156
06443           FROM(CERTIFICATE-MASTER)                                EL156
06444       END-EXEC.                                                      CL*88
06445                                                                   EL156
06446      GO TO 6399-EXIT.                                             EL156
06447                                                                   EL156
06448  6380-UNLOCK-CERT.                                                EL156
06449      EXEC CICS UNLOCK                                             EL156
06450           DATASET  ('ELCERT')                                     EL156
06451       END-EXEC.                                                      CL*88
06452                                                                   EL156
06453  6399-EXIT.                                                       EL156
06454       EXIT.                                                       EL156
06455      EJECT                                                        EL156
06456  6400-UPDATE-ZERO-TRAILER.                                        EL156
06457      MOVE ELMSTR-KEY             TO ELTRLR-KEY.                   EL156
06458      MOVE ZEROS                  TO TRLR-SEQ-NO.                  EL156
06459                                                                   EL156
06460      PERFORM 7960-READ-TRAILER-UPDATE THRU 7960-EXIT.             EL156
06461                                                                   EL156
06462      IF PI-EPYAMT NOT = ZEROS                                        CL**2
06463          SUBTRACT PI-EPYAMT    FROM AT-CURRENT-MANUAL-RESERVE        CL**2
06464      ELSE                                                            CL**2
06465          SUBTRACT PI-CPYAMT    FROM AT-CURRENT-MANUAL-RESERVE.       CL**2
06466                                                                      CL**2
06467      IF PI-ERESV NOT = ZEROS                                         CL**2
06468         ADD PI-ERESV             TO AT-CURRENT-MANUAL-RESERVE.       CL**2
06469                                                                   EL156
06470      IF AT-CURRENT-MANUAL-RESERVE NEGATIVE                        EL156
06471         MOVE ZEROS               TO AT-CURRENT-MANUAL-RESERVE.    EL156
06472                                                                   EL156
06473      IF PI-PFKEY-USED = DFHPF3 OR DFHPF4                          EL156
06474         ADD PI-EEXPENS           TO AT-ITD-CHARGEABLE-EXPENSE     EL156
06475      ELSE                                                         EL156
06476         ADD PI-CEXPENS           TO AT-ITD-CHARGEABLE-EXPENSE.    EL156
06477                                                                   EL156
06478      IF PI-PMTTYPE = '5'                                          EL156
06479         IF PI-EPYAMT = ZEROS                                      EL156
06480            ADD PI-CPYAMT         TO AT-ITD-CHARGEABLE-EXPENSE     EL156
06481         ELSE                                                      EL156
06482            ADD PI-EPYAMT         TO AT-ITD-CHARGEABLE-EXPENSE.    EL156
06483                                                                   EL156
06484      IF PI-PMTTYPE = '6'                                          EL156
06485         IF PI-EPYAMT = ZEROS                                      EL156
06486            ADD PI-CPYAMT         TO AT-ITD-PAID-EXPENSES          EL156
06487         ELSE                                                      EL156
06488            ADD PI-EPYAMT         TO AT-ITD-PAID-EXPENSES.         EL156
06489                                                                   EL156
06490  6420-CHECK-OPEN-CLOSE.                                           EL156
06491      IF PI-PMTTYPE = '5' OR '6'                                   EL156
06492         GO TO 6450-REWRITE.                                       EL156
06493                                                                   EL156
06494      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
06495          IF PI-PMTTYPE = '2' OR '3' OR '4'                           CL*93
06496              IF CLAIM-IS-CLOSED                                      CL*65
06497                  MOVE '1'        TO  WS-OPEN-CLOSE-SW                CL*65
06498                  MOVE 1          TO  SUB-1                           CL*65
06499                  GO TO 6440-AIG-LOOP.                                CL*65
06500                                                                      CL*65
06501      IF (PI-PMTTYPE = '1' OR '4') AND CLAIM-IS-OPEN                  CL*65
06502         GO TO 6450-REWRITE.                                       EL156
06503                                                                   EL156
06504      IF (PI-PMTTYPE = '2' OR '3') AND CLAIM-IS-CLOSED             EL156
06505         GO TO 6450-REWRITE.                                       EL156
06506                                                                   EL156
06507      MOVE 1                      TO SUB-1.                        EL156
06508                                                                   EL156
06509  6430-LOOP.                                                       EL156
06510      IF AT-OPEN-CLOSE-TYPE (SUB-1) = SPACES                       EL156
06511         MOVE WS-TODAY-DATE       TO AT-OPEN-CLOSE-DATE (SUB-1)    EL156
06512         IF PI-PMTTYPE = '1' OR '4'                                   CL*65
06513            MOVE 'O'              TO AT-OPEN-CLOSE-TYPE (SUB-1)    EL156
06514            MOVE 'FORCE'          TO AT-OPEN-CLOSE-REASON (SUB-1)  EL156
06515            GO TO 6450-REWRITE                                     EL156
06516         ELSE                                                      EL156
06517            MOVE 'C'              TO AT-OPEN-CLOSE-TYPE (SUB-1)    EL156
06518            MOVE 'FINAL'          TO AT-OPEN-CLOSE-REASON (SUB-1)  EL156
06519            GO TO 6450-REWRITE.                                    EL156
06520                                                                   EL156
06521      IF SUB-1 = 6                                                 EL156
06522       MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1) EL156
06523       MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2) EL156
06524       MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3) EL156
06525       MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4) EL156
06526       MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5) EL156
06527       MOVE SPACES                    TO AT-OPEN-CLOSE-HISTORY (6) EL156
06528       GO TO 6430-LOOP.                                            EL156
06529                                                                   EL156
06530      ADD 1                       TO SUB-1.                        EL156
06531      GO TO 6430-LOOP.                                             EL156
06532                                                                      CL*65
06533                                                                      CL*65
06534  6440-AIG-LOOP.                                                      CL*65
06535                                                                      CL*65
06536      IF AT-OPEN-CLOSE-TYPE (SUB-1) = SPACES                          CL*65
06537        MOVE WS-TODAY-DATE        TO AT-OPEN-CLOSE-DATE (SUB-1)       CL*65
06538        IF WS-OPEN-CLOSE-SW = '1'                                     CL*88
06539          MOVE 'O'                TO  AT-OPEN-CLOSE-TYPE (SUB-1)      CL*65
06540          IF PI-PMTTYPE = '2'                                         CL*88
06541            MOVE 'FINAL'          TO  AT-OPEN-CLOSE-REASON (SUB-1)    CL*65
06542            MOVE '2'              TO  WS-OPEN-CLOSE-SW                CL*65
06543          ELSE                                                        CL*65
06544            IF PI-PMTTYPE = '3'                                       CL*88
06545              MOVE 'SETTL'        TO  AT-OPEN-CLOSE-REASON (SUB-1)    CL*65
06546              MOVE '2'            TO  WS-OPEN-CLOSE-SW                CL*65
06547            ELSE                                                      CL*65
06548              IF PI-PMTTYPE = '4'                                     CL*88
06549                MOVE 'ADDL'       TO  AT-OPEN-CLOSE-REASON (SUB-1)    CL*65
06550                MOVE '2'          TO  WS-OPEN-CLOSE-SW                CL*65
06551              ELSE                                                    CL*65
06552                MOVE 'UNK '       TO  AT-OPEN-CLOSE-REASON (SUB-1)    CL*65
06553                MOVE '2'          TO  WS-OPEN-CLOSE-SW                CL*65
06554        ELSE                                                          CL*65
06555          MOVE 'C'                TO  AT-OPEN-CLOSE-TYPE (SUB-1)      CL*65
06556          MOVE 'FORCE'            TO  AT-OPEN-CLOSE-REASON (SUB-1)    CL*65
06557          GO TO 6450-REWRITE.                                         CL*65
06558                                                                      CL*65
06559      IF SUB-1 = 6                                                    CL*65
06560       MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1)    CL*65
06561       MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2)    CL*65
06562       MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3)    CL*65
06563       MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4)    CL*65
06564       MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5)    CL*65
06565       MOVE SPACES                    TO AT-OPEN-CLOSE-HISTORY (6)    CL*65
06566       GO TO 6440-AIG-LOOP.                                           CL*65
06567                                                                      CL*65
06568      ADD 1                       TO SUB-1.                           CL*65
06569      GO TO 6440-AIG-LOOP.                                            CL*65
06570                                                                   EL156
06571  6450-REWRITE.                                                    EL156
06572      MOVE PI-PROCESSOR-ID     TO AT-RESERVES-LAST-UPDATED-BY.        CL*43
06573      MOVE EIBTIME             TO AT-LAST-MAINT-HHMMSS.               CL*43
06574      MOVE WS-TODAY-DATE       TO AT-RESERVES-LAST-MAINT-DT.          CL*43
06575                                                                      CL*93
06576      IF PI-COMPANY-ID = 'DMD'                                        CL*93
06577          MOVE PI-AT-EOB-CODE (1) TO AT-EOB-CODE1                     CL*93
06578          MOVE PI-AT-EOB-CODE (2) TO AT-EOB-CODE2                     CL*93
06579          MOVE PI-AT-EOB-CODE (3) TO AT-EOB-CODE3                     CL*93
06580          MOVE PI-AT-EOB-CODE (4) TO AT-EOB-CODE4                     CL*93
06581          MOVE PI-AT-EOB-CODE (5) TO AT-EOB-CODE5.                    CL*93
06582                                                                      CL*16
06583      EXEC CICS REWRITE                                            EL156
06584           DATASET  ('ELTRLR')                                     EL156
06585           FROM     (ACTIVITY-TRAILERS)                            EL156
06586       END-EXEC.                                                      CL*88
06587                                                                   EL156
06588  6499-EXIT.                                                       EL156
06589       EXIT.                                                          CL*88
06590                                                                      CL*88
06591  EJECT                                                               CL*88
06592  6500-READ-DLO035.                                                   CL*88
06593      MOVE 'E'                    TO DL35-PROCESS-TYPE.               CL*88
06594      MOVE CL-CCN-A5              TO DL35-CREDIT-CARD.                CL*88
06595      MOVE CL-CERT-NO (5:2)       TO DL35-BEN-TYPE.                   CL*88
06596      MOVE PI-EPYAMT              TO DL35-PAYMENT-AMT.                CL*88
06597                                                                      CL*88
06598      EXEC CICS LINK                                                  CL*88
06599          PROGRAM    ('DLO035')                                       CL*88
06600          COMMAREA   (DLO035-KEY)                                     CL*88
06601          LENGTH     (DL35-LENGTH)                                    CL*88
06602      END-EXEC.                                                       CL*88
06603                                                                      CL*88
06604      IF DL35-RETURN-CODE = 'OK'                                      CL*88
06605          GO TO 6500-EXIT.                                            CL*88
06606                                                                      CL*88
06607      IF DL35-RETURN-CODE = '01'                                      CL*88
06608          MOVE ER-7830            TO EMI-ERROR                        CL*88
06609          GO TO 6500-BAD-EXIT.                                        CL*88
06610                                                                      CL*88
06611      IF DL35-RETURN-CODE = '02'                                      CL*88
06612          MOVE ER-7831            TO EMI-ERROR                        CL*88
06613          GO TO 6500-BAD-EXIT.                                        CL*88
06614                                                                      CL*88
06615      IF DL35-RETURN-CODE = '03'                                      CL*88
06616          MOVE ER-7832            TO EMI-ERROR                        CL*88
06617          GO TO 6500-BAD-EXIT.                                        CL*88
06618                                                                      CL*88
06619      IF DL35-RETURN-CODE = '04'                                      CL*88
06620          MOVE ER-7833            TO EMI-ERROR                        CL*88
06621          GO TO 6500-BAD-EXIT.                                        CL*88
06622                                                                      CL*88
06623      IF DL35-RETURN-CODE = '05'                                      CL*88
06624          MOVE ER-7834            TO EMI-ERROR                        CL*88
06625          GO TO 6500-BAD-EXIT.                                        CL*88
06626                                                                      CL*88
06627      IF DL35-RETURN-CODE = '06'                                      CL*88
06628          MOVE ER-7835            TO EMI-ERROR                        CL*88
06629          GO TO 6500-BAD-EXIT.                                        CL*88
06630                                                                      CL*88
06631      IF DL35-RETURN-CODE = '07'                                      CL*88
06632          MOVE ER-7836            TO EMI-ERROR                        CL*88
06633          GO TO 6500-BAD-EXIT.                                        CL*88
06634                                                                      CL*88
06635      IF DL35-RETURN-CODE = 'E1'                                      CL*88
06636          MOVE ER-7837            TO EMI-ERROR                        CL*88
06637          GO TO 6500-BAD-EXIT.                                        CL*88
06638                                                                      CL*88
06639      IF DL35-RETURN-CODE NOT = 'OK'                                  CL*88
06640          MOVE ER-7838            TO EMI-ERROR                        CL*88
06641          GO TO 6500-BAD-EXIT.                                        CL*88
06642                                                                      CL*88
06643  6500-EXIT.                                                          CL*88
06644       EXIT.                                                          CL*88
06645                                                                      CL*88
06646  6500-BAD-EXIT.                                                      CL*88
06647      EXEC CICS SYNCPOINT ROLLBACK END-EXEC.                          CL*88
06648      MOVE -1                 TO PMTTYPEL.                            CL*88
06649      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*88
06650      GO TO 8200-SEND-DATAONLY.                                       CL*88
06651                                                                      CL*88
06652  EJECT                                                               CL*88
06653  6600-UPDATE-DLO035.                                                 CL*88
06654      MOVE 'U'                    TO DL35-PROCESS-TYPE.               CL*88
06655      MOVE CL-CCN-A5              TO DL35-CREDIT-CARD.                CL*88
06656      MOVE CL-CERT-NO (5:2)       TO DL35-BEN-TYPE.                   CL*88
06657      MOVE PI-EPYAMT              TO DL35-PAYMENT-AMT.                CL*88
06658                                                                      CL*88
06659      EXEC CICS LINK                                                  CL*88
06660          PROGRAM    ('DLO035')                                       CL*88
06661          COMMAREA   (DLO035-KEY)                                     CL*88
06662          LENGTH     (DL35-LENGTH)                                    CL*88
06663      END-EXEC.                                                       CL*88
06664                                                                      CL*88
06665      IF DL35-RETURN-CODE = 'OK'                                      CL*88
06666          GO TO 6600-EXIT.                                            CL*88
06667                                                                      CL*88
06668      IF DL35-RETURN-CODE = '01'                                      CL*88
06669          MOVE ER-7830            TO EMI-ERROR                        CL*88
06670          GO TO 6500-BAD-EXIT.                                        CL*88
06671                                                                      CL*88
06672      IF DL35-RETURN-CODE = '02'                                      CL*88
06673          MOVE ER-7831            TO EMI-ERROR                        CL*88
06674          GO TO 6500-BAD-EXIT.                                        CL*88
06675                                                                      CL*88
06676      IF DL35-RETURN-CODE = '03'                                      CL*88
06677          MOVE ER-7832            TO EMI-ERROR                        CL*88
06678          GO TO 6500-BAD-EXIT.                                        CL*88
06679                                                                      CL*88
06680      IF DL35-RETURN-CODE = '04'                                      CL*88
06681          MOVE ER-7833            TO EMI-ERROR                        CL*88
06682          GO TO 6500-BAD-EXIT.                                        CL*88
06683                                                                      CL*88
06684      IF DL35-RETURN-CODE = '05'                                      CL*88
06685          MOVE ER-7834            TO EMI-ERROR                        CL*88
06686          GO TO 6500-BAD-EXIT.                                        CL*88
06687                                                                      CL*88
06688      IF DL35-RETURN-CODE = '06'                                      CL*88
06689          MOVE ER-7835            TO EMI-ERROR                        CL*88
06690          GO TO 6500-BAD-EXIT.                                        CL*88
06691                                                                      CL*88
06692      IF DL35-RETURN-CODE = '07'                                      CL*88
06693          MOVE ER-7836            TO EMI-ERROR                        CL*88
06694          GO TO 6500-BAD-EXIT.                                        CL*88
06695                                                                      CL*88
06696      IF DL35-RETURN-CODE = 'E1'                                      CL*88
06697          MOVE ER-7837            TO EMI-ERROR                        CL*88
06698          GO TO 6500-BAD-EXIT.                                        CL*88
06699                                                                      CL*88
06700      IF DL35-RETURN-CODE NOT = 'OK'                                  CL*88
06701          MOVE ER-7838            TO EMI-ERROR                        CL*88
06702          GO TO 6500-BAD-EXIT.                                        CL*88
06703                                                                      CL*88
06704  6600-EXIT.                                                          CL*88
06705       EXIT.                                                       EL156
06706                                                                   EL156
06707      EJECT                                                        EL156
06708  6700-UPDATE-ACTQ.                                                EL156
06709 ***************************************************************   EL156
06710 *    THE ACTIVITY QUE FILE IS READ TO GET THE RECORD FOR      *   EL156
06711 *    THIS CLAIM.  A NEW RECORD WILL BE BUILT IF THERE ISNT    *   EL156
06712 *    ONE ALREADY THERE.                                       *   EL156
06713 ***************************************************************   EL156
06714                                                                   EL156
06715      EXEC CICS HANDLE CONDITION                                   EL156
06716           NOTFND(6720-BUILD-NEW-RECORD)                           EL156
06717       END-EXEC.                                                      CL*88
06718                                                                   EL156
06719      EXEC CICS READ                                               EL156
06720           DATASET('ELACTQ')                                       EL156
06721           RIDFLD (ELMSTR-KEY)                                        CL*93
06722           SET    (ADDRESS OF ACTIVITY-QUE)                           CL*93
06723           UPDATE                                                  EL156
06724       END-EXEC.                                                      CL*88
06725                                                                   EL156
06726      MOVE '1'                    TO AQ-PENDING-PAYMENT-FLAG.      EL156
06727                                                                      CL*93
06728      IF PI-AIG-SPECIAL-BENEFIT AND                                   CL*93
06729         PI-CASH = 'Y'                                                CL*93
06730          ADD 2                   TO AQ-PAYMENT-COUNTER               CL*65
06731      ELSE                                                            CL*65
06732          ADD 1                   TO AQ-PAYMENT-COUNTER.              CL*65
06733                                                                   EL156
06734      IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC                       CL*47
06735          MOVE +0                 TO  AQ-PMT-UNAPPROVED-COUNT.        CL*47
06736                                                                      CL*65
06737      MOVE +0                     TO  WS-PMT-UNAPPROVED-CNT.          CL*65
06738                                                                      CL*47
06739      IF PI-PMT-GRADUATED                                             CL*47
06740          IF WS-PMT-APPROVAL-SW = 'A'                                 CL*88
06741              NEXT SENTENCE                                           CL*47
06742          ELSE                                                        CL*47
06743             IF PI-AIG-SPECIAL-BENEFIT AND                            CL*93
06744                PI-CASH = 'Y'                                         CL*93
06745                 ADD +2           TO  AQ-PMT-UNAPPROVED-COUNT         CL*65
06746                                      WS-PMT-UNAPPROVED-CNT           CL*65
06747             ELSE                                                     CL*65
06748                 ADD +1           TO  AQ-PMT-UNAPPROVED-COUNT         CL*65
06749                                      WS-PMT-UNAPPROVED-CNT           CL*65
06750      ELSE                                                         EL156
06751          IF PI-PMT-APPR-SW = 'Y'                                     CL*88
06752             IF PI-AIG-SPECIAL-BENEFIT AND                            CL*93
06753                PI-CASH = 'Y'                                         CL*93
06754                 ADD +2           TO  AQ-PMT-UNAPPROVED-COUNT         CL*65
06755                                      WS-PMT-UNAPPROVED-CNT           CL*65
06756             ELSE                                                     CL*65
06757                 ADD +1           TO  AQ-PMT-UNAPPROVED-COUNT         CL*65
06758                                      WS-PMT-UNAPPROVED-CNT.          CL*65
06759                                                                      CL*65
06760      IF WS-PMT-UNAPPROVED-CNT > ZERO                                 CL*94
06761          MOVE ER-3536 TO EMI-ERROR                                   CL*65
06762          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*65
06763                                                                      CL*65
06764      MOVE +156                   TO  AQ-LAST-UPDATED-BY.             CL*65
06765                                                                   EL156
06766      EXEC CICS REWRITE                                            EL156
06767           DATASET('ELACTQ')                                       EL156
06768           FROM   (ACTIVITY-QUE)                                      CL*93
06769       END-EXEC.                                                      CL*88
06770                                                                   EL156
06771      GO TO 6799-EXIT.                                             EL156
06772                                                                   EL156
06773  6720-BUILD-NEW-RECORD.                                           EL156
06774      EXEC CICS GETMAIN                                            EL156
06775           SET    (ADDRESS OF ACTIVITY-QUE)                           CL*93
06776           LENGTH (60)                                                CL*93
06777           INITIMG(GETMAIN-SPACE)                                  EL156
06778       END-EXEC.                                                      CL*88
06779                                                                   EL156
06780      MOVE ZEROS                  TO AQ-PMT-UNAPPROVED-COUNT.         CL*65
06781                                                                      CL*58
06782      IF PI-AIG-SPECIAL-BENEFIT AND                                   CL*93
06783         PI-CASH = 'Y'                                                CL*93
06784          MOVE 2                  TO AQ-PAYMENT-COUNTER               CL*65
06785      ELSE                                                            CL*65
06786          MOVE 1                  TO AQ-PAYMENT-COUNTER.              CL*65
06787                                                                      CL*65
06788      MOVE ELMSTR-KEY             TO AQ-CONTROL-PRIMARY.           EL156
06789      MOVE 'AQ'                   TO AQ-RECORD-ID.                 EL156
06790      MOVE SPACES                 TO AQ-PENDING-ACTIVITY-FLAGS.    EL156
06791      MOVE LOW-VALUES             TO AQ-RESEND-DATE                   CL*65
06792                                     AQ-FOLLOWUP-DATE.                CL*65
06793      MOVE +156                   TO AQ-LAST-UPDATED-BY.              CL*65
06794      MOVE '1'                    TO AQ-PENDING-PAYMENT-FLAG.      EL156
06795                                                                      CL*47
06796      IF PI-PMT-GRADUATED                                             CL*47
06797          IF WS-PMT-APPROVAL-SW = 'A'                                 CL*88
06798              NEXT SENTENCE                                           CL*47
06799          ELSE                                                        CL*47
06800             IF PI-AIG-SPECIAL-BENEFIT AND                            CL*93
06801                PI-CASH = 'Y'                                         CL*93
06802                 ADD +2           TO  AQ-PMT-UNAPPROVED-COUNT         CL*65
06803             ELSE                                                     CL*65
06804                 ADD +1           TO  AQ-PMT-UNAPPROVED-COUNT         CL*65
06805      ELSE                                                            CL*47
06806          IF PI-PMT-APPR-SW = 'Y'                                     CL*88
06807             IF PI-AIG-SPECIAL-BENEFIT AND                            CL*93
06808                PI-CASH = 'Y'                                         CL*93
06809                 ADD +2           TO  AQ-PMT-UNAPPROVED-COUNT         CL*65
06810             ELSE                                                     CL*65
06811                 ADD +1           TO  AQ-PMT-UNAPPROVED-COUNT.        CL*65
06812                                                                      CL*65
06813      IF AQ-PMT-UNAPPROVED-COUNT > ZERO                               CL*94
06814          MOVE ER-3536 TO EMI-ERROR                                   CL*65
06815          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*65
06816                                                                   EL156
06817      EXEC CICS WRITE                                              EL156
06818           DATASET('ELACTQ')                                       EL156
06819           RIDFLD (ELMSTR-KEY)                                        CL*43
06820           FROM   (ACTIVITY-QUE)                                      CL*43
06821       END-EXEC.                                                      CL*88
06822                                                                   EL156
06823  6799-EXIT.                                                       EL156
06824       EXIT.                                                       EL156
06825                                                                   EL156
06826      EJECT                                                        EL156
06827  6800-BUILD-FORM-TRAILER.                                            CL*20
06828                                                                      CL*20
06829      EXEC CICS GETMAIN                                               CL*20
06830           SET      (ADDRESS OF ACTIVITY-TRAILERS)                    CL*81
06831           LENGTH   (200)                                             CL*20
06832           INITIMG  (GETMAIN-SPACE)                                   CL*20
06833      END-EXEC.                                                       CL*20
06834                                                                      CL*20
06835      SUBTRACT +1               FROM CL-TRAILER-SEQ-CNT.              CL*20
06836                                                                      CL*20
06837      MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY.              CL*43
06838      MOVE 'AT'                   TO AT-RECORD-ID.                    CL*43
06839      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.                  CL*43
06840                                                                      CL*68
06841      MOVE 'A'                    TO AT-TRAILER-TYPE.                 CL*43
06842      MOVE WS-TODAY-DATE          TO AT-RECORDED-DT                   CL*20
06843                                     AT-FORM-LAST-MAINT-DT            CL*20
06844                                     AT-FORM-SEND-ON-DT.              CL*43
06845      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                   CL*20
06846                                     AT-FORM-LAST-UPDATED-BY.         CL*43
06847      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.            CL*43
06848      MOVE WS-TODAY-DATE          TO DC-BIN-DATE-1.                   CL*43
06849      MOVE '6'                    TO DC-OPTION-CODE.                  CL*43
06850      MOVE +29                    TO DC-ELAPSED-DAYS.                 CL*43
06851      MOVE +0                     TO DC-ELAPSED-MONTHS.               CL*43
06852      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*43
06853      MOVE DC-BIN-DATE-2          TO AT-FORM-FOLLOW-UP-DT.            CL*43
06854                                                                      CL*20
06855      IF PI-COMPANY-ID = 'RMC' OR 'LAP'                               CL*93
06856          MOVE  LOW-VALUES        TO AT-FORM-FOLLOW-UP-DT.            CL*53
06857                                                                      CL*53
06858      MOVE LOW-VALUES             TO AT-FORM-ANSWERED-DT              CL*20
06859                                     AT-FORM-RE-SEND-DT               CL*20
06860                                     AT-EMP-FORM-ANSWERED-DT          CL*20
06861                                     AT-PHY-FORM-ANSWERED-DT          CL*20
06862                                     AT-EMP-FORM-SEND-ON-DT           CL*20
06863                                     AT-PHY-FORM-SEND-ON-DT           CL*20
06864                                     AT-FORM-PRINTED-DT               CL*20
06865                                     AT-FORM-REPRINT-DT.              CL*20
06866      MOVE '2'                    TO AT-FORM-TYPE.                    CL*43
06867                                                                      CL*20
06868      MOVE CL-INSURED-ADDR-CNT    TO AT-FORM-ADDR-SEQ-NO.             CL*43
06869      MOVE 'I'                    TO AT-FORM-ADDRESS.                 CL*20
06870                                                                      CL*20
06871      IF PI-COMPANY-ID = 'RMC' OR 'LAP'                               CL*93
06872          NEXT SENTENCE                                               CL*53
06873      ELSE                                                            CL*53
06874          IF CL-PROG-FORM-TYPE NOT = 'S'                              CL*93
06875             MOVE WS-TODAY-DATE   TO AT-PHY-FORM-SEND-ON-DT.          CL*53
06876                                                                      CL*20
06877      EXEC CICS WRITE                                                 CL*20
06878           DATASET  ('ELTRLR')                                        CL*20
06879           FROM     (ACTIVITY-TRAILERS)                               CL*20
06880           RIDFLD   (AT-CONTROL-PRIMARY)                              CL*20
06881       END-EXEC.                                                      CL*88
06882                                                                      CL*20
06883  6899-EXIT.                                                          CL*20
06884       EXIT.                                                          CL*20
06885                                                                      CL*20
06886      EJECT                                                           CL*20
06887  6900-BUILD-ARCHIVE-HEADER.                                          CL*20
06888                                                                      CL*20
06889      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                    CL*43
06890      MOVE '1'                    TO CNTL-REC-TYPE.                   CL*43
06891      MOVE +0                     TO CNTL-SEQ-NO.                     CL*43
06892      MOVE SPACES                 TO CNTL-ACCESS.                     CL*43
06893                                                                      CL*20
06894      EXEC CICS READ                                                  CL*20
06895           DATASET  ('ELCNTL')                                        CL*20
06896           SET      (ADDRESS OF CONTROL-FILE)                         CL*81
06897           RIDFLD   (ELCNTL-KEY)                                      CL*20
06898           UPDATE                                                     CL*20
06899      END-EXEC.                                                       CL*20
06900                                                                      CL*20
06901      ADD +1                      TO CF-CO-ARCHIVE-COUNTER.           CL*20
06902      MOVE CF-CO-ARCHIVE-COUNTER  TO ARCH-NUMBER.                     CL*20
06903                                                                      CL*20
06904      EXEC CICS REWRITE                                               CL*20
06905           FROM     (CONTROL-FILE)                                    CL*20
06906           DATASET  ('ELCNTL')                                        CL*20
06907      END-EXEC.                                                       CL*20
06908                                                                      CL*20
06909      EXEC CICS HANDLE CONDITION                                      CL*20
06910           DUPKEY   (6900-BUILD-ARCHIVE-HEADER)                       CL*20
06911           DUPREC   (6900-BUILD-ARCHIVE-HEADER)                       CL*20
06912      END-EXEC.                                                       CL*20
06913                                                                      CL*20
06914      EXEC CICS GETMAIN                                               CL*20
06915           SET     (ADDRESS OF LETTER-ARCHIVE)                        CL*81
06916           LENGTH  (ELARCH-LENGTH)                                    CL*20
06917           INITIMG (GETMAIN-SPACE)                                    CL*20
06918      END-EXEC.                                                       CL*20
06919                                                                      CL*20
06920      MOVE 'LA'                   TO LA-RECORD-ID.                    CL*43
06921      MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO                    CL*20
06922                                     LA-ARCHIVE-NO-A1.                CL*43
06923      MOVE '4'                    TO LA-RECORD-TYPE                   CL*20
06924                                     LA-RECORD-TYPE-A1.               CL*43
06925      MOVE +0                     TO LA-LINE-SEQ-NO                   CL*20
06926                                     LA-LINE-SEQ-NO-A1.               CL*43
06927      MOVE PI-COMPANY-CD          TO LA-COMPANY-CD                    CL*20
06928                                     LA-COMPANY-CD-A1.                CL*43
06929      MOVE CL-CARRIER             TO LA4-CARRIER.                     CL*43
06930      MOVE CL-CLAIM-NO            TO LA4-CLAIM-NO.                    CL*43
06931      MOVE CL-CERT-NO             TO LA4-CERT-NO.                     CL*43
06932      MOVE CL-CERT-STATE          TO LA4-STATE.                       CL*29
06933      MOVE +0                     TO LA4-NO-OF-COPIES.                CL*20
06934      MOVE PI-PROCESSOR-ID        TO LA4-PROCESSOR-CD.                CL*43
06935      MOVE WS-TODAY-DATE          TO LA4-CREATION-DT.                 CL*43
06936      MOVE LOW-VALUES             TO LA4-RESEND-DATE                  CL*20
06937                                     LA4-INITIAL-PRINT-DATE           CL*43
06938                                     LA4-RESEND-PRINT-DATE            CL*43
06939                                     LA4-FORM-REM-PRINT-DT.           CL*43
06940      MOVE CL-TRAILER-SEQ-CNT     TO LA4-FORM-TRLR-SEQ.               CL*43
06941      MOVE '2'                    TO LA4-FORM-TYPE.                   CL*43
06942                                                                      CL*20
06943      EXEC CICS WRITE                                                 CL*20
06944           DATASET  ('ELARCH')                                        CL*20
06945           FROM     (LETTER-ARCHIVE)                                  CL*20
06946           RIDFLD   (LA-CONTROL-PRIMARY)                              CL*20
06947      END-EXEC.                                                       CL*20
06948                                                                      CL*20
06949  6999-EXIT.                                                          CL*20
06950       EXIT.                                                          CL*20
06951                                                                      CL*20
06952      EJECT                                                           CL*20
06953  7000-SHOW-CLAIM.                                                 EL156
06954      MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.         EL156
06955      MOVE 'A'                    TO PI-PASS-SW.                   EL156
06956      MOVE LOW-VALUES             TO PI-CPYFROM                    EL156
06957                                     PI-MONTH-END-SAVE                CL*16
06958                                     PI-CPYTHRU.                   EL156
06959                                                                   EL156
06960      MOVE ZEROS                  TO PI-FATAL-COUNT                EL156
06961                                     PI-DAILY-RATE                    CL*12
06962                                     PI-BEN-DAYS                      CL*12
06963                                     PI-MANUAL-SW                     CL*12
06964                                     PI-SAVE-CURSOR                   CL*16
06965                                     PI-FORCE-COUNT                   CL*16
06966                                     PI-HOLDTIL                    EL156
06967                                     PI-EPYFROM                    EL156
06968                                     PI-EPYTHRU                    EL156
06969                                     PI-EDAYS                         CL*12
06970                                     PI-EPYAMT                     EL156
06971                                     PI-ERESV                      EL156
06972                                     PI-EEXPENS                    EL156
06973                                     PI-CDAYS                      EL156
06974                                     PI-CPYAMT                     EL156
06975                                     PI-CRESV                      EL156
06976                                     PI-CEXPENS                       CL*57
06977                                     PI-INT-RATE                      CL*65
06978                                     PI-AIGFROM.                      CL*65
052506     MOVE ZEROS                  TO PI-PROOF-DATE.
041710     MOVE ZEROS                  TO PI-ORIG-BEN-AMT
041710                                    PI-REM-BEN-AMT
041710                                    PI-MO-BEN-AMT
                                          pi-ah-term.
06979                                                                   EL156
06980      MOVE LOW-VALUES             TO EL156AI.                      EL156
06981      MOVE MAP-NAMEA              TO MAP-NAME.                     EL156
020413     EXEC CICS HANDLE CONDITION
020413         NOTFND   (7010-BUILD-MAP)
020413     END-EXEC.
020413
020413     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
020413     MOVE '2'                    TO CNTL-REC-TYPE.
020413     MOVE +0                     TO CNTL-SEQ-NO.
020413     MOVE PI-PROCESSOR-ID        TO CNTL-ACCESS.
020413
020413     EXEC CICS READ
020413          DATASET  ('ELCNTL')
020413          SET      (ADDRESS OF CONTROL-FILE)
020413          RIDFLD   (ELCNTL-KEY)
020413     END-EXEC.
020413
020413     MOVE CF-APPROVAL-LEVEL TO PI-APPROVAL-LEVEL.
010413
111113     IF PI-APPROVAL-LEVEL = '4' OR '5'
020413         MOVE AL-UANON      TO SURVYYNA
020413     END-IF.
020413
020413
032813     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                    CL*43
032813     MOVE '1'                    TO CNTL-REC-TYPE.
032813     MOVE SPACES                 TO CNTL-ACCESS.
032813     MOVE +0                     TO CNTL-SEQ-NO.
032813
032813     EXEC CICS READ
032813          DATASET  ('ELCNTL')
032813          SET      (ADDRESS OF CONTROL-FILE)
032813          RIDFLD   (ELCNTL-KEY)
032813     END-EXEC.
032813
032813     MOVE CF-CO-REAUDIT-INTERVAL TO PI-REAUDIT-INTERVAL.
032813
06983  7010-BUILD-MAP.                                                     CL*16
06984                                                                   EL156
06985      EXEC CICS HANDLE CONDITION                                   EL156
06986          NOTFND(7100-SHOW-RECORD-NOT-FOUND)                       EL156
06987      END-EXEC.                                                       CL*88
06988                                                                   EL156
06989      MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.                 EL156
06990      MOVE PI-CARRIER             TO MSTR-CARRIER.                    CL*43
06991      MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.                EL156
06992      MOVE PI-CERT-NO             TO MSTR-CERT-NO.                 EL156
06993      MOVE 'MSTR'                 TO FILE-SWITCH.                  EL156
06994                                                                   EL156
06995      PERFORM 7900-READ-CLAIM THRU 7900-EXIT.                      EL156
06996                                                                   EL156
06997      MOVE CL-CERT-KEY-DATA       TO PI-SV-CERT-KEY.                  CL*84
06998      MOVE CL-CERT-NO             TO PI-SV-CERT-NO.                   CL*84
06999      MOVE CL-BENEFICIARY         TO PI-SV-BEN.                       CL*84
07000      MOVE CL-CCN                 TO PI-SV-CCN.                       CL*84
07001                                                                      CL*84
07002      MOVE CL-LAST-MAINT-USER     TO PI-UPDATE-BY.                 EL156
07003      MOVE CL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL156
07004      MOVE CL-CLAIM-NO            TO CLMNOO.                       EL156
07005      MOVE CL-CARRIER             TO CARRO.                        EL156
07006      MOVE CL-CERT-PRIME          TO CERTNOO.                      EL156
07007      MOVE CL-CERT-SFX            TO SUFXO.                        EL156
07008                                                                   EL156
062121     IF (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
100518        AND (CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O')
              CONTINUE
           ELSE
              MOVE AL-SADOF            TO ZINTHA
                                          ZINTA
                                          PINTHA
                                          PINTA
           END-IF

013013     MOVE CL-CLAIM-TYPE          TO PI-CLM-TYPE
121802     EVALUATE TRUE
121802     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
07010          MOVE PI-AH-OVERRIDE-L6   TO CLMTYPO    

121802     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
07012          MOVE PI-LIFE-OVERRIDE-L6 TO CLMTYPO
07013                                                                   EL156
121802     WHEN CL-CLAIM-TYPE = 'I'
121802         MOVE '  IU  '           TO CLMTYPO

121802     WHEN CL-CLAIM-TYPE = 'G'
121802         MOVE ' GAP  '           TO CLMTYPO
121802     WHEN CL-CLAIM-TYPE = 'F'
121802         MOVE ' FMLA '           TO CLMTYPO
022122     WHEN CL-CLAIM-TYPE = 'B'
022122        MOVE ' BRV  '            TO CLMTYPO
022122     WHEN CL-CLAIM-TYPE = 'H'
022122        MOVE ' HOSP '            TO CLMTYPO
100518     WHEN CL-CLAIM-TYPE = 'O'
100518         MOVE ' OTH '           TO CLMTYPO

121802     END-EVALUATE.

100518     IF CL-CLAIM-TYPE = 'O'
100518        MOVE 'S' TO EOBYNO
100518                    PI-PRINT-EOB-YN
100518                    WS-PRINT-EOB-YN
100518     END-IF

07014      IF CLAIM-IS-OPEN                                             EL156
07015          MOVE ' OPEN'            TO CLMSTATO                      EL156
07016      ELSE                                                         EL156
07017          MOVE 'CLOSED'           TO CLMSTATO.                     EL156
07018                                                                   EL156
07019      PERFORM 5000-MOVE-NAME  THRU 5000-EXIT.                      EL156
07020                                                                   EL156
07021      IF CL-ASSOC-CERT-TOTAL = +0 OR +1                               CL*93
07022          MOVE SPACES             TO  PCERTNOO                        CL*16
07023                                      PSUFXO                          CL*16
07024                                      PRIMHDGO                        CL*16
07025                                      SEQUO                           CL*16
07026          MOVE AL-SADOF           TO  PCERTNOA                        CL*16
07027                                      PSUFXA                          CL*16
07028                                      PRIMHDGA                        CL*16
07029                                      SEQUA                           CL*16
07030      ELSE                                                            CL*16
07031          MOVE CL-ASSOC-CERT-SEQU     TO  WS-CUR-SEQU                 CL*16
07032          MOVE CL-ASSOC-CERT-TOTAL    TO  WS-OF-SEQU                  CL*16
07033          MOVE WS-CLAIM-SEQUENCE      TO  SEQUO                       CL*16
07034          MOVE CL-PRIME-CERT-PRIME    TO  PCERTNOO                    CL*16
07035          MOVE CL-PRIME-CERT-SFX      TO  PSUFXO                      CL*16
07036          MOVE 'PRIME CERT :'         TO  PRIMHDGO                    CL*16
07037          MOVE AL-SANON               TO  PCERTNOA                    CL*16
07038                                          PSUFXA                      CL*16
07039                                          PRIMHDGA                    CL*16
07040          MOVE AL-SABON               TO  SEQUA.                      CL*16
07041                                                                   EL156
07042      MOVE CL-ACTIVITY-CODE           TO  PI-ACTIVITY-CODE.           CL*65
07043                                                                      CL*65
07044      IF CL-NO-OF-PMTS-MADE = 0 AND                                   CL*93
07045         CL-TOTAL-PAID-AMT = 0                                        CL*93
07046          NEXT SENTENCE                                               CL*82
07047      ELSE                                                            CL*82
07048          IF CL-PAID-THRU-DT NOT = LOW-VALUES                         CL*93
07049              IF NOT PI-USES-PAID-TO                                  CL*93
07050                  MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1           CL*93
07051                  MOVE ' '                 TO DC-OPTION-CODE          CL*93
07052                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT       CL*82
07053                  MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO                 CL*93
07054              ELSE                                                    CL*82
07055                  MOVE CL-PAID-THRU-DT     TO DC-BIN-DATE-1           CL*93
07056                  MOVE '6'                 TO DC-OPTION-CODE          CL*93
07057                  MOVE +1                  TO DC-ELAPSED-DAYS         CL*93
07058                  MOVE +0                  TO DC-ELAPSED-MONTHS       CL*93
07059                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT       CL*82
07060                  MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO.                CL*82
07061                                                                      CL*65
07062      IF MAP-NAME = 'EL156A'                                          CL*93
07063         IF CL-PAID-THRU-DT NOT = LOW-VALUES AND SPACES
07064            IF CL-TOTAL-PAID-AMT NOT = ZEROS
07065              MOVE AL-SADOF TO    AIGFRMHA AIGFROMA
                 end-if
              end-if
           end-if

07067      IF CL-LAST-PMT-DT NOT = LOW-VALUES                           EL156
07068          MOVE CL-LAST-PMT-DT     TO DC-BIN-DATE-1                 EL156
07069          MOVE ' '                TO DC-OPTION-CODE                EL156
07070          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL156
07071          MOVE DC-GREG-DATE-1-EDIT TO LSTDTEO.                     EL156
07072                                                                   EL156
07073      IF CL-INCURRED-DT NOT = LOW-VALUES                           EL156
07074          MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1                 EL156
07075          MOVE ' '                TO DC-OPTION-CODE                EL156
07076          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL156
07077          MOVE DC-GREG-DATE-1-EDIT    TO INCURO.                   EL156
07078                                                                   EL156
07079      MOVE CL-TOTAL-PAID-AMT      TO TOTPAIDO.                     EL156
07080                                                                   EL156
07081      IF CL-CERT-EFF-DT NOT = LOW-VALUES                           EL156
07082          MOVE CL-CERT-EFF-DT     TO DC-BIN-DATE-1                 EL156
07083          MOVE ' '                TO DC-OPTION-CODE                EL156
07084          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL156
07085          MOVE DC-GREG-DATE-1-EDIT    TO EFFECTO.                  EL156
07086                                                                   EL156
07087      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
07088          MOVE CL-CURRENT-ACCOUNT TO WS-WORK-ACCT                     CL*65
07089          MOVE CL-CURRENT-STATE   TO WS-WORK-STATE                    CL*65
07090      ELSE                                                            CL*65
07091          MOVE CL-CERT-ACCOUNT    TO WS-WORK-ACCT                     CL*65
07092          MOVE CL-CERT-STATE      TO WS-WORK-STATE.                   CL*65
07093                                                                      CL*82
07094      MOVE WS-WORK-STATE-ACCT     TO STACCTO.                         CL*16
07095      MOVE PI-COMPANY-CD          TO CERT-COMP-CD.                 EL156
07096      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.                 EL156
07097      MOVE CL-CERT-GROUPING       TO CERT-GROUPING                    CL*49
07098                                     PI-GROUPING.                     CL*49
07099      MOVE CL-CERT-STATE          TO CERT-STATE                       CL*49
07100                                     PI-STATE.                        CL*49
07101      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT                     CL*49
07102                                     PI-ACCOUNT.                      CL*49
07103      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.                  EL156
07104      MOVE CL-CERT-NO             TO CERT-CERT-NO.                 EL156
07105      MOVE 'CERT'                 TO FILE-SWITCH.                  EL156
07106                                                                   EL156
07107      PERFORM 7970-READ-CERT THRU 7970-EXIT.                       EL156

052814     IF CM-INSURED-LAST-NAME EQUAL CL-INSURED-LAST-NAME  AND
052814        CM-INSURED-FIRST-NAME EQUAL CL-INSURED-1ST-NAME  AND
052814        CM-INSURED-INITIAL2 EQUAL CL-INSURED-MID-INIT
052814            MOVE 'N' TO PI-JOINT-INSURED-IND
052814     ELSE
052814            MOVE 'Y' TO PI-JOINT-INSURED-IND
052814     END-IF.
052814
           perform 7975-read-acct      thru 7975-exit

07109      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                    CL*80
07110      MOVE PI-STATE               TO WS-ST-ACCESS.                    CL*80
07111      MOVE WS-STATE-ACCESS        TO CNTL-ACCESS.                     CL*80
07112      MOVE '3'                    TO CNTL-REC-TYPE.                   CL*80
07113      MOVE +0                     TO CNTL-SEQ-NO.                     CL*80
07114      MOVE 'STAT'                 TO FILE-SWITCH.                     CL*80
07115                                                                      CL*80
07116      PERFORM 7930-READ-CONTROL THRU 7930-EXIT.                       CL*80
07117                                                                      CL*80
07118      MOVE CF-STATE-ABBREVIATION  TO WS-STATE-ABBREV.                 CL*80
07119      MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.                    CL*96
07120                                                                      CL*80
121802     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
07122         GO TO 7020-BYPASS-BENEFIT.                                   CL*21
07123                                                                      CL*21
07124      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                    CL*21
07125      MOVE '4'                    TO CNTL-REC-TYPE.                   CL*21
07126      MOVE +0                     TO CNTL-SEQ-NO.                     CL*21
07127      MOVE CM-LF-BENEFIT-CD       TO WS-BEN-CD.                       CL*21
07128      MOVE WS-ACCESS              TO CNTL-ACCESS.                     CL*21
07129      MOVE 'BENE'                 TO FILE-SWITCH.                     CL*21
07130                                                                      CL*21
07131      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.                       CL*21
07132                                                                      CL*21
07133      IF NO-BENEFIT-FOUND                                             CL*21
07134         GO TO 7020-BYPASS-BENEFIT.                                   CL*21
07135                                                                      CL*21
07136      MOVE CF-CO-EARNINGS-CALC (SUB-1)    TO  WS-EARNING-METHOD.      CL*80
07137      MOVE CF-SPECIAL-CALC-CD  (SUB-1)    TO  WS-SPECIAL-CALC-CD.     CL*80
07138      MOVE CF-LF-COVERAGE-TYPE (SUB-1)    TO  WS-LF-COVERAGE-TYPE.    CL*80
07139                                                                      CL*21
07140  7020-BYPASS-BENEFIT.                                                CL*21
07141                                                                      CL*21
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
121802*        PERFORM 3400-COMPUTE-EXPIRY THRU 3499-EXIT                  CL*65
121802*        MOVE PI-LOAN-DUE-DAY TO DUEDAYO.                            CL*65
07145                                                                      CL*65
061013     move zeros                  to PI-MAX-BENEFIT-AMT
061013     move cm-ah-benefit-cd       to pi-ah-benefit-cd
           move cm-ah-orig-term        to pi-ah-term
           move cm-ah-benefit-amt      to pi-ah-benefit-amt

121802     IF CL-CLAIM-TYPE NOT = PI-AH-OVERRIDE-L1 AND 'I' AND
100314          'G' and 'F'
022122          AND 'B' AND 'H'
07147          GO TO 7021-GO-TO-LIFE.

022122     move cl-insured-birth-dt    to dc-bin-date-1
022122     move cl-incurred-dt         to dc-bin-date-2
022122     move '1'                    to dc-option-code
022122     PERFORM 9700-LINK-DATE-CONVERT
022122                                 THRU 9700-EXIT
022122     compute ws-att-age =
022122        dc-elapsed-months / 12
022122     move zeros to dc-elapsed-months dc-elapsed-days
061013
120115     IF (PI-COMPANY-ID = 'DCC' or 'VPP')
061013        and (acct-found)
061013        AND (PI-DCC-PRODUCT-CODE not = '   ')
061013        PERFORM 0900-GET-DDF-limits
061013                                 THRU 0900-EXIT
061013        IF PDEF-FOUND
061013           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122              (P1 > +11)
022122              OR ((PD-PROD-CODE (P1) = cl-claim-type)
022122              AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
061013           END-PERFORM
022122           IF P1 < +12
100314              if pd-ben-pct (p1) not numeric
100314                 move zeros      to pd-ben-pct (p1)
100314              end-if
100314              if pd-ben-pct (p1) = zeros
100314                 move +1         to ws-work-ben-pct
100314              else
100314                 move pd-ben-pct (p1)
100314                                 to ws-work-ben-pct
100314              end-if
                    compute cm-ah-benefit-amt =
                       cm-ah-benefit-amt * ws-work-ben-pct
061013              move pd-max-amt (p1) to pi-max-benefit-amt
022122              if pd-wait-days(p1) not numeric
022122                 move zeros      to pd-wait-days(p1)
022122              end-if
061013           END-IF
061013        END-IF
061013     end-if

07149      MOVE 'A'                    TO CP-BENEFIT-TYPE.                 CL*82
07150      MOVE 'MO. BENEFIT :'        TO BENECAPO.                        CL*82
061013     if (pi-max-benefit-amt not = zeros)
061013        and (cm-ah-benefit-amt > pi-max-benefit-amt)
061013        move pi-max-benefit-amt  to beneo
061013     else
061013        MOVE CM-AH-BENEFIT-AMT   TO BENEO
061013     end-if
07152      MOVE CM-POLICY-FORM-NO      TO PI-SAVE-FORM.                    CL*82
07153      MOVE CM-AH-ORIG-TERM        TO WST-ORIG                         CL*82
07154                                     CP-ORIGINAL-TERM.                CL*82
07155      MOVE CM-AH-CURRENT-STATUS   TO WS-STATUS.                       CL*82
07156                                                                      CL*82
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
121802*        MOVE WS-EXP-DT            TO DC-BIN-DATE-1                  CL*93
121802*    ELSE                                                            CL*82
07160          MOVE CM-AH-LOAN-EXPIRE-DT TO DC-BIN-DATE-1.                 CL*93
07161                                                                      CL*82
07162      MOVE ' '                    TO DC-OPTION-CODE.                  CL*82
07163      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*82
07164      MOVE DC-GREG-DATE-1-EDIT    TO EXPDTEO.                         CL*82
07165                                                                      CL*82
07166      IF PI-COMPANY-ID = 'DMD'                                        CL*88
07167          PERFORM 9830-DMD-REMAINING-TERM THRU 9830-EXIT              CL*82
07168          GO TO 7023-BYPASS-REMAINING-TERM.                           CL*82
07169                                                                      CL*82
07170      MOVE SPACES                 TO WST-REM-DAYS-GRP.                CL*94
07171      MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT.                 CL*94
07172      GO TO 7022-AROUND-LIFE.                                         CL*94
07173                                                                      CL*82
07174  7021-GO-TO-LIFE.                                                    CL*82
07175                                                                      CL*82
07176      MOVE WS-LF-COVERAGE-TYPE    TO CP-BENEFIT-TYPE.                 CL*94
07177      MOVE CM-POLICY-FORM-NO      TO PI-SAVE-FORM.                    CL*94
07178      MOVE CM-LF-ORIG-TERM        TO WST-ORIG                         CL*82
07179                                     CP-ORIGINAL-TERM.                CL*94
07180      MOVE WS-EARNING-METHOD      TO CP-EARNING-METHOD.               CL*94
07181      MOVE WS-SPECIAL-CALC-CD     TO CP-SPECIAL-CALC-CD.              CL*94
07182      MOVE CL-INCURRED-DT         TO CP-VALUATION-DT.                 CL*94
07183      MOVE CM-LF-CURRENT-STATUS   TO WS-STATUS.                       CL*94
07184                                                                      CL*87
07185      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
07186          MOVE WS-EXP-DT            TO DC-BIN-DATE-1                  CL*93
07187      ELSE                                                         EL156
07188          MOVE CM-LF-LOAN-EXPIRE-DT TO DC-BIN-DATE-1.                 CL*82
07189                                                                   EL156
07190      MOVE ' '                    TO DC-OPTION-CODE.                  CL*82
07191      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*82
07192      MOVE DC-GREG-DATE-1-EDIT    TO EXPDTEO.                         CL*82
07193                                                                      CL*82
07194  7022-AROUND-LIFE.                                                   CL*82
07195                                                                   EL156
07196      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.                   CL*96
07197      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.               EL156
07198      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.               CL*16
07199      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.          CL*38
07200      MOVE '4'                    TO CP-REM-TERM-METHOD.           EL156
07201      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.                   EL156
07202      MOVE CP-REMAINING-TERM-3    TO WST-REM.                      EL156
07203                                                                      CL*82
07204      MOVE SPACES                 TO WST-REM-DAYS-GRP.                CL*94
07205                                                                      CL*21
121802     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122        OR 'B' OR 'H'
07207         IF CP-REMAINING-TERM-3 > CM-AH-ORIG-TERM                     CL*94
07208            MOVE CM-AH-ORIG-TERM  TO WST-REM.                         CL*21
07209                                                                      CL*21
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                   CL*21
07211         IF CP-REMAINING-TERM-3 > CM-LF-ORIG-TERM                     CL*94
07212            MOVE CM-LF-ORIG-TERM  TO WST-REM.                         CL*21
07213                                                                      CL*82
07214  7023-BYPASS-REMAINING-TERM.                                         CL*82

022122     move zeros                  to ws-max-tot-ben
022122                                    ws-work-ben-pct

100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                   CL*88
07217        IF CM-LF-CURRENT-STATUS = '6' OR '7' OR '8'                   CL*93
07218           MOVE ZEROS                  TO BENEO                       CL*93
07219        ELSE                                                          CL*93
07220           MOVE WS-LF-COVERAGE-TYPE    TO CP-BENEFIT-TYPE             CL*93
07221           MOVE PI-COMPANY-ID          TO CP-COMPANY-ID               CL*93
07222           MOVE CM-LF-ORIG-TERM        TO CP-ORIGINAL-TERM            CL*93
07223           MOVE CM-LF-BENEFIT-AMT      TO CP-ORIGINAL-BENEFIT         CL*93
07224           MOVE CM-LF-ALT-BENEFIT-AMT  TO CP-ALTERNATE-BENEFIT        CL*93
07225           MOVE CP-REMAINING-TERM-3    TO CP-REMAINING-TERM           CL*93
07226           MOVE CM-LOAN-APR            TO CP-LOAN-APR                 CL*93
07227           MOVE CM-LOAN-TERM           TO CP-LOAN-TERM                CL*93
07228           MOVE CM-PAY-FREQUENCY       TO CP-PAY-FREQUENCY            CL*93
07229           MOVE WS-EARNING-METHOD      TO CP-EARNING-METHOD           CL*93
07230           MOVE WS-SPECIAL-CALC-CD     TO CP-SPECIAL-CALC-CD          CL*93
07231           MOVE WS-STATE-ABBREV        TO CP-STATE-STD-ABBRV          CL*93

022122          move cl-insured-birth-dt
022122                                 to dc-bin-date-1
022122          move cl-incurred-dt    to dc-bin-date-2
022122          move '1'               to dc-option-code
022122          PERFORM 9700-LINK-DATE-CONVERT
022122                                 THRU 9700-EXIT
022122          compute ws-att-age =
022122              dc-elapsed-months / 12
022122           move zeros to dc-elapsed-months dc-elapsed-days

120115          IF (PI-COMPANY-ID = 'DCC' or 'VPP')
061013             AND (PI-DCC-PRODUCT-CODE not = '   ')
061013             PERFORM 0900-GET-DDF-limits
061013                                 THRU 0900-EXIT
061013             IF PDEF-FOUND
061013                PERFORM VARYING P1 FROM +1 BY +1 UNTIL
022122                   (P1 > +11)
022122                   OR (PD-PROD-CODE (P1) = cl-claim-type
022122                        and PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
061013                END-PERFORM
022122                IF P1 < +12
022122                   MOVE PD-MAX-AMT (P1)
022122                                 TO ws-MAX-TOT-BEN
022122                   if pd-ben-pct (p1) not numeric
022122                      move zeros to pd-ben-pct (p1)
022122                   end-if
022122                   if pd-ben-pct (p1) = zeros
022122                      move +1    to ws-work-ben-pct
022122                   else
022122                      move pd-ben-pct (p1)
022122                                 to ws-work-ben-pct
022122                   end-if
061013                END-IF
061013             END-IF
061013          END-IF

07232           EXEC CICS LINK                                             CL*93
07233              PROGRAM  (LINK-REMAMT)                                  CL*93
07234              COMMAREA (CALCULATION-PASS-AREA)                        CL*93
07235              LENGTH   (CP-COMM-LENGTH)                               CL*93
07236           END-EXEC                                                   CL*93
07237           MOVE CP-REMAINING-AMT       TO BENEO
             end-if
           end-if

061013     MOVE ELMSTR-KEY             TO ELTRLR-KEY
061013     MOVE +95                    TO TRLR-SEQ-NO
061013     EXEC CICS READ
061013        DATASET  ('ELTRLR')
061013        SET      (ADDRESS OF ACTIVITY-TRAILERS) 
061013        RIDFLD   (ELTRLR-KEY)                   
061013        RESP     (WS-RESPONSE)
061013     END-EXEC
061013
061013     if ws-resp-normal
061013        perform varying s1 from +1 by +1 until
061013           at-note-error-no (s1) = spaces
061013           move at-note-error-no (s1)
061013                                 to emi-error
061013           if at-note-error-no (s1) = '1653'
061013              evaluate true
061013                 when cl-claim-type = 'L'
061013                    move '  LF  '
061013                                 to emi-claim-type
061013                 when cl-claim-type = 'I'
061013                    move '  IU  '
061013                                 to emi-claim-type
061013                 when cl-claim-type = 'F'
061013                    move ' FMLA '
061013                                 to emi-claim-type
022122                 when cl-claim-type = 'B'
022122                    move ' BRV  '
022122                                 to emi-claim-type
022122                 when cl-claim-type = 'H'
022122                    move ' HOSP '
022122                                 to emi-claim-type
100518                 when cl-claim-type = 'O'
100518                    move ' OTH '
100518                                 to emi-claim-type
061013                 when other
061013                    move '  AH  '
061013                                 to emi-claim-type
061013              end-evaluate
061013           end-if
061013           PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013        end-perform
061013     end-if

100518    IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'                    CL*93
07240          IF PI-LIFE-OVERRIDE-L1 = 'P' OR                             CL*93
07241             WS-LF-COVERAGE-TYPE = 'P'                                CL*93
07242              COMPUTE WS-REMAINING-AMT = CM-LF-BENEFIT-AMT -          CL*25
07243                                         CM-LF-ITD-DEATH-AMT          CL*25
07244              MOVE WS-REMAINING-AMT  TO  BENEO.                       CL*25
07245                                                                      CL*20
041710     MOVE CM-LF-BENEFIT-AMT      TO PI-ORIG-BEN-AMT.
041710     MOVE BENEO                  TO PI-REM-BEN-AMT.
07246      MOVE WS-TERMS               TO TERMSO.                       EL156
07247                                                                   EL156
07248      IF WS-STATUS = '6' OR '7' OR '8'                             EL156
07249          MOVE AL-SABOF           TO CRTSTATA.                     EL156
07250                                                                   EL156
07251      IF WS-STATUS = '1' OR '4'                                    EL156
07252         IF CP-REMAINING-TERM-3 = ZEROS                            EL156
07253            MOVE 'EXPIRED'        TO CRTSTATO                      EL156
07254         ELSE                                                         CL*20
07255            MOVE 'ACTIVE  '       TO CRTSTATO.                     EL156
07256                                                                   EL156
07257      IF WS-STATUS = '2'                                           EL156
07258          MOVE 'PEND    '         TO CRTSTATO.                     EL156
07259                                                                   EL156
07260      IF WS-STATUS = '3'                                           EL156
07261          MOVE 'RESTORE '         TO CRTSTATO.                     EL156
07262                                                                   EL156
07263      IF WS-STATUS = '5'                                           EL156
07264          MOVE 'REISSUE '         TO CRTSTATO.                     EL156
07265                                                                   EL156
07266      IF WS-STATUS = '6'                                           EL156
07267          MOVE 'LMP DIS'          TO CRTSTATO.                     EL156
07268                                                                   EL156
07269      IF WS-STATUS = '7'                                           EL156
07270          MOVE 'DEATH'            TO CRTSTATO.                     EL156
07271                                                                   EL156
07272      IF WS-STATUS = '8'                                           EL156
07273          MOVE 'CANCEL'           TO CRTSTATO.                     EL156
07274                                                                   EL156
07275      IF WS-STATUS = '9'                                           EL156
07276          MOVE 'RE-ONLY '         TO CRTSTATO.                     EL156
07277                                                                      CL*46
07278      IF WS-STATUS = 'V'                                              CL*46
07279          MOVE 'VOID  '           TO CRTSTATO.                        CL*46
07280                                                                      CL*46
07281      IF WS-STATUS = 'D'                                              CL*46
07282          MOVE 'DECLINE '         TO CRTSTATO.                        CL*46
07283                                                                   EL156
121802     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' or 'F'
022122         OR 'B' OR 'H'
07285          MOVE CM-AH-BENEFIT-CD   TO WS-BEN-CD                     EL156
07286          MOVE '5'                TO CNTL-REC-TYPE                 EL156
07287      ELSE                                                         EL156
07288          MOVE CM-LF-BENEFIT-CD   TO WS-BEN-CD                     EL156
07289          MOVE '4'                TO CNTL-REC-TYPE.                EL156
07290                                                                   EL156
07291      MOVE '** NONE **'           TO COVERO.                       EL156
07292      MOVE AL-SABON               TO COVERA.                       EL156
07293                                                                      CL*20
07294      IF PI-COMPANY-ID = 'CVL'                                        CL*88
07295          IF CM-BENEFICIARY NOT = SPACES AND LOW-VALUES               CL*93
07296              MOVE CM-BENEFICIARY TO LOANNOO                          CL*78
07297              MOVE AL-UANON       TO LOANNOA.                         CL*78
07298                                                                      CL*78
120115     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121           OR 'FNL'
CIDMOD        MOVE ELMSTR-KEY          TO  ELTRLR-KEY                      CL*25
CIDMOD        MOVE +91                 TO  TRLR-SEQ-NO                     CL*25
CIDMOD        EXEC CICS READ                                               EL156
CIDMOD            DATASET  ('ELTRLR')                                      EL156
CIDMOD            SET      (ADDRESS OF ACTIVITY-TRAILERS)                     CL
CIDMOD            RIDFLD   (ELTRLR-KEY)                                    EL156
CIDMOD            RESP     (WS-RESPONSE)
CIDMOD        END-EXEC                                                        CL
CIDMOD                                                                  EL156
CIDMOD        IF WS-RESP-NORMAL
CIDMOD           MOVE AT-INFO-LINE-1   TO  LOANNOO                         CL*25
CIDMOD        ELSE
CIDMOD           MOVE SPACES           TO  LOANNOO
CIDMOD        END-IF
022208*052506   
022208*052506        PERFORM 7250-FIND-LETTER-TRLR THRU 7250-EXIT
022208*052506   
CIDMOD     END-IF
CIDMOD
07299      IF CL-PROG-FORM-TYPE = 'S' OR 'L'                               CL*93
07300         MOVE CL-PROG-FORM-TYPE   TO FORMTYPO.                        CL*20
07301                                                                   EL156
07302      IF WS-BEN-CD = ZEROS                                            CL*16
07303          GO TO 7050-SHOW-CONTINUE.                                EL156
07304                                                                   EL156
07305      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                 EL156
07306      MOVE WS-ACCESS              TO CNTL-ACCESS.                  EL156
07307      MOVE +0                     TO CNTL-SEQ-NO.                  EL156
07308      MOVE 'BENE'                 TO FILE-SWITCH.                  EL156
07309                                                                   EL156
07310      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.                    EL156

07312      IF BENEFIT-FOUND                                             EL156
052814        MOVE CF-JOINT-INDICATOR (SUB-1)
                                       TO PI-JOINT-COV-IND
                                          PI-RB-JOINT-COV-IND
07313         MOVE CF-BENEFIT-DESCRIP (SUB-1)
                                       TO COVERO
07314         MOVE AL-SANON            TO COVERA
           END-IF

120115     if (cntl-rec-type = '4')   *> Life claim
120115        and (cm-ah-benefit-cd not = zeros) *> AH coverage
120115        MOVE PI-COMPANY-ID    TO CNTL-COMP-ID
120115        MOVE CM-AH-BENEFIT-CD TO WS-BEN-CD
120115        MOVE '5'              TO CNTL-REC-TYPE
120115        MOVE WS-ACCESS        TO CNTL-ACCESS
120115        MOVE +0               TO CNTL-SEQ-NO
120115        MOVE 'BENE'           TO FILE-SWITCH
120115        PERFORM 7200-FIND-BENEFIT
120115                              THRU 7200-EXIT
120115        IF (BENEFIT-FOUND)
120115           and (pi-rb-joint-cov-ind) not = 'J'
120115           MOVE CF-JOINT-INDICATOR (SUB-1)
120115                              TO PI-rb-JOINT-COV-IND
120115        end-if
120115     end-if

           .
07317  7050-SHOW-CONTINUE.                                              EL156
052814
052814     IF PI-JOINT-INSURED AND NOT PI-JOINT-COVERAGE
052814        AND CL-TOTAL-PAID-AMT NOT GREATER THAN ZERO
052814            MOVE 'Y' TO PI-APPROVAL-3550-NEEDED
052814     END-IF
07318                                                                      CL*82
07319      IF PI-COMPANY-ID = 'DMD'                                        CL*88
07320          PERFORM 8010-DMD-ERROR-CHECKS THRU 8090-EXIT.               CL*88
07321                                                                      CL*82
07322      MOVE -1                     TO PMTTYPEL.                     EL156
07323      GO TO 8100-SEND-INITIAL-MAP.                                 EL156
07324                                                                   EL156
07325  7100-SHOW-RECORD-NOT-FOUND.                                      EL156
07326      IF FILE-SWITCH = 'MSTR'                                      EL156
07327          MOVE ER-0204            TO EMI-ERROR.                       CL*80
07328                                                                      CL*80
07329      IF FILE-SWITCH = 'CERT'                                         CL*80
07330          MOVE ER-0206            TO EMI-ERROR.                    EL156
07331                                                                      CL*80
07332      IF FILE-SWITCH = 'STAT'                                         CL*80
07333          MOVE ER-0149            TO EMI-ERROR.                       CL*80
07334                                                                   EL156
07335      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
07336                                                                   EL156
07337      MOVE -1                     TO PMTTYPEL.                     EL156
07338      GO TO 8100-SEND-INITIAL-MAP
07339                                                                   EL156
           .
022106 7110-BUILD-PMT-TRLR.

           MOVE ELMSTR-KEY             TO ELTRLR-KEY
           MOVE +51                    TO TRLR-SEQ-NO

           EXEC CICS READ
               DATASET  ('ELTRLR')
               SET      (ADDRESS OF ACTIVITY-TRAILERS)
               RIDFLD   (ELTRLR-KEY)
               RESP     (WS-RESPONSE)
           END-EXEC

           IF WS-RESP-NORMAL
              MOVE AT-MAIL-TO-NAME     TO PI-PAYEE-NAME
           END-IF

022106     SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT

022106     EXEC CICS GETMAIN
022106          SET       (ADDRESS OF ACTIVITY-TRAILERS)
022106          LENGTH    (200)
022106          INITIMG   (GETMAIN-SPACE)
022106      END-EXEC

022106     MOVE LOW-VALUES             TO AT-PREV-LAST-PMT-DT
022106                                    AT-PREV-PAID-THRU-DT
022106     MOVE ZEROS                  TO AT-PREV-LAST-PMT-AMT
022106     MOVE ELMSTR-KEY             TO AT-CONTROL-PRIMARY
022106     MOVE 'AT'                   TO AT-RECORD-ID
022106     MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO

022106     MOVE +0                     TO AT-PAYMENT-NOTE-SEQ-NO

022106     MOVE '2'                    TO AT-TRAILER-TYPE
022106     MOVE WS-TODAY-DATE          TO AT-RECORDED-DT
022106                                    AT-PAYMENT-LAST-MAINT-DT
022106     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
022106                                    AT-PAYMENT-LAST-UPDATED-BY
022106     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
022106     MOVE 'I'                    TO AT-PAYMENT-TYPE
022106     MOVE CL-CLAIM-TYPE          TO AT-CLAIM-TYPE
022106     MOVE CL-CLAIM-PREM-TYPE     TO AT-CLAIM-PREM-TYPE

022106     MOVE +0                     TO AT-EXPENSE-PER-PMT
022106     MOVE PI-INT-AMT             TO AT-AMOUNT-PAID

022106     MOVE LOW-VALUES             TO AT-PAID-FROM-DT
022106     MOVE LOW-VALUES             TO AT-PAID-THRU-DT

012506     MOVE PI-INT-DAYS            TO AT-DAYS-IN-PERIOD
082807     MOVE PI-INT-RATE-USED       TO AT-INT-RATE

120115     if pi-int-to-rem-borr = 'Y'
120115        move 'Q1'                to at-payee-type-cd
120115     else
120115        MOVE 'O1'                TO AT-PAYEE-TYPE-CD
120115     end-if
120115     MOVE PI-int-payees-name     TO AT-PAYEES-NAME

022106     MOVE PI-MONTH-END-SAVE      TO AT-PMT-SELECT-DT
022106     MOVE '3'                    TO AT-PAYMENT-ORIGIN

022106     MOVE SPACES                 TO AT-AIG-UNEMP-IND

022106     MOVE LOW-VALUES             TO AT-CHECK-WRITTEN-DT
022106                                    AT-TO-BE-WRITTEN-DT

022106     MOVE ZEROS                  TO AT-CHECK-QUE-CONTROL
022106                                    AT-CHECK-QUE-SEQUENCE
022106                                    AT-ADDL-RESERVE

022106     MOVE LOW-VALUES             TO AT-PMT-ACCEPT-DT
022106                                    AT-VOID-SELECT-DT
022106                                    AT-VOID-ACCEPT-DT
022106                                    AT-VOID-DT
052506                                    AT-PMT-PROOF-DT
                                          AT-INT-PMT-SELECT-DT

022106     MOVE ZEROS                  TO AT-DAILY-RATE
022106                                    AT-ELIMINATION-DAYS
022106     MOVE SPACES                 TO AT-BENEFIT-TYPE

022106*    EXEC CICS HANDLE CONDITION
022106*        DUPREC    (7120-DUPREC)
022106*    END-EXEC

           .
022106 7115-WRITE-INT-TRLR.

022106     EXEC CICS WRITE
022106          DATASET       ('ELTRLR')
022106          FROM          (ACTIVITY-TRAILERS)
022106          RIDFLD        (AT-CONTROL-PRIMARY)
                RESP          (WS-RESPONSE)
022106      END-EXEC

            IF WS-RESP-DUPREC
               GO TO 7120-DUPREC
            END-IF

022106     GO TO 7130-EXIT

           .
022106 7120-DUPREC.

022106     SUBTRACT +1                 FROM CL-TRAILER-SEQ-CNT
022106     MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO
022106     GO TO 7115-WRITE-INT-TRLR

           .
022106 7130-EXIT.
022106      EXIT.

022106 7140-BUILD-NOTE-TRLR.
022106 
022106     EXEC CICS GETMAIN
022106          SET       (ADDRESS OF ACTIVITY-TRAILERS)
022106          LENGTH    (200)
022106          INITIMG   (GETMAIN-SPACE)
022106      END-EXEC

022106     SUBTRACT +1                 FROM CL-TRAILER-SEQ-CNT

022106     MOVE ELMSTR-KEY             TO AT-CONTROL-PRIMARY
022106     MOVE 'AT'                   TO AT-RECORD-ID
022106     MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO
022106     MOVE '6'                    TO AT-TRAILER-TYPE
022106     MOVE WS-TODAY-DATE          TO AT-RECORDED-DT
022106                                    AT-GEN-INFO-LAST-MAINT-DT
022106     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
022106                                    AT-GEN-INFO-LAST-UPDATED-BY
022106     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS

022106     MOVE 'NO INTEREST DUE'      TO AT-INFO-LINE-1

022106     .
022106 7150-WRITE.

022106     EXEC CICS HANDLE CONDITION
022106         DUPREC    (7160-DUPREC)
022106     END-EXEC

022106     EXEC CICS WRITE
022106          DATASET     ('ELTRLR')
022106          FROM        (ACTIVITY-TRAILERS)
022106          RIDFLD      (AT-CONTROL-PRIMARY)
                RESP        (WS-RESPONSE)
022106      END-EXEC

            IF WS-RESP-DUPREC
               GO TO 7160-DUPREC
            END-IF

022106     GO TO 7170-EXIT

022106     .
022106 7160-DUPREC.

022106     SUBTRACT +1                 FROM CL-TRAILER-SEQ-CNT
022106     MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO
022106     GO TO 7150-WRITE

022106     .
022106 7170-EXIT.                                                       EL156
022106      EXIT.                                                       EL156
041710
041710 7180-ADD-CERT-NOTE.
041710*ADD CERT NOTE FOR SC NP+6
041710 
041710     EXEC CICS GETMAIN
041710          SET       (ADDRESS OF CERT-NOTE-FILE)
041710          LENGTH    (150)
041710          INITIMG   (GETMAIN-SPACE)
041710      END-EXEC.
041710
041710     MOVE SPACES                 TO WRK-CERT-NOTE-ADD.
041710     MOVE SPACES                 TO WRK-ORIG-NOTE-ADD.
041710     MOVE +1                     TO WRK-NOTE-SEQ.
041710     MOVE LOW-VALUES             TO CERT-NOTE-FILE.
041710     MOVE PI-COMPANY-CD          TO CZ-COMPANY-CD.
041710     MOVE CL-CERT-CARRIER        TO CZ-CARRIER.
041710     MOVE CL-CERT-GROUPING       TO CZ-GROUPING.
041710     MOVE CL-CERT-STATE          TO CZ-STATE.
041710     MOVE CL-CERT-ACCOUNT        TO CZ-ACCOUNT.
041710     MOVE CL-CERT-EFF-DT         TO CZ-CERT-EFF-DT.
041710     MOVE CL-CERT-NO             TO CZ-CERT-NO.
041710     MOVE '2'                    TO CZ-RECORD-TYPE.
041710     MOVE WRK-NOTE-SEQ           TO CZ-NOTE-SEQUENCE.
041710     MOVE 'CZ'                   TO CZ-RECORD-ID.
041710     MOVE WS-TODAY-DATE          TO CZ-LAST-MAINT-DT.
041710     MOVE EIBTIME                TO CZ-LAST-MAINT-HHMMSS.
041710     MOVE PI-PROCESSOR-ID        TO CZ-LAST-MAINT-USER.
041710     MOVE PI-REM-BEN-AMT         TO SC-NP6-REM-BEN.
041710     MOVE PI-MO-BEN-AMT          TO SC-NP6-6-MO.

           MOVE ' + 6 MO '             TO SC-NP6-COMM
           IF PI-COMPANY-ID = 'AHL'
              IF PI-LF-BENEFIT-CD = '5I' OR '6J'
                 CONTINUE
              ELSE
                 IF PI-LF-BENEFIT-CD = '5M' OR '6M'
                    MOVE ' + 1 MO '    TO SC-NP6-COMM
                 ELSE
                    MOVE ' + 2 MO '    TO SC-NP6-COMM
                 END-IF
              END-IF
           END-IF

041710     MOVE PI-CPYAMT              TO SC-NP6-AMT-PAID.
041710     MOVE SC-NP6-CERT-NOTE       TO CZ-NOTE.
041710
041710 7181-WRITE.
041710
041710     EXEC CICS WRITE
041710          DATASET     ('ERCNOT')
041710          FROM        (CERT-NOTE-FILE)
041710          RIDFLD      (CZ-CONTROL-PRIMARY)
041710          RESP        (WS-RESPONSE)
041710     END-EXEC.
041710
041710     IF WS-RESP-DUPREC
041710         GO TO 7182-DUPREC
041710     END-IF.
041710
041710     SET CERT-NOTE-ADDED TO TRUE.
041710
041710     IF PI-CPYAMT EQUAL PI-ORIG-BEN-AMT 
041710         PERFORM 7190-ADD-ORIG-NOTE THRU 7199-EXIT
041710     END-IF.
041710
041710     GO TO 7189-EXIT.
041710
041710 7182-DUPREC.
041710
041710     ADD +1                      TO  WRK-NOTE-SEQ.
041710     MOVE WRK-NOTE-SEQ           TO  CZ-NOTE-SEQUENCE.
041710     GO TO 7181-WRITE.
041710
041710 7189-EXIT.
041710      EXIT.
041710
041710 7190-ADD-ORIG-NOTE.
041710 
041710     EXEC CICS GETMAIN
041710          SET       (ADDRESS OF CERT-NOTE-FILE)
041710          LENGTH    (150)
041710          INITIMG   (GETMAIN-SPACE)
041710      END-EXEC.
041710
041710     ADD +1                      TO WRK-NOTE-SEQ.
041710     MOVE LOW-VALUES             TO CERT-NOTE-FILE.
041710     MOVE PI-COMPANY-CD          TO CZ-COMPANY-CD.
041710     MOVE CL-CERT-CARRIER        TO CZ-CARRIER.
041710     MOVE CL-CERT-GROUPING       TO CZ-GROUPING.
041710     MOVE CL-CERT-STATE          TO CZ-STATE.
041710     MOVE CL-CERT-ACCOUNT        TO CZ-ACCOUNT.
041710     MOVE CL-CERT-EFF-DT         TO CZ-CERT-EFF-DT.
041710     MOVE CL-CERT-NO             TO CZ-CERT-NO.
041710     MOVE '2'                    TO CZ-RECORD-TYPE.
041710     MOVE WRK-NOTE-SEQ           TO CZ-NOTE-SEQUENCE.
041710     MOVE 'CZ'                   TO CZ-RECORD-ID.
041710     MOVE WS-TODAY-DATE          TO CZ-LAST-MAINT-DT.
041710     MOVE EIBTIME                TO CZ-LAST-MAINT-HHMMSS.
041710     MOVE PI-PROCESSOR-ID        TO CZ-LAST-MAINT-USER.
041710     MOVE SC-NP6-ORIG-NOTE       TO CZ-NOTE.
041710
041710 7191-WRITE.
041710
041710     EXEC CICS WRITE
041710          DATASET     ('ERCNOT')
041710          FROM        (CERT-NOTE-FILE)
041710          RIDFLD      (CZ-CONTROL-PRIMARY)
041710          RESP        (WS-RESPONSE)
041710     END-EXEC.
041710
041710     IF WS-RESP-DUPREC
041710         GO TO 7192-DUPREC
041710     END-IF.
041710
041710     GO TO 7199-EXIT.
041710
041710 7192-DUPREC.
041710
041710     ADD +1                      TO  WRK-NOTE-SEQ.
041710     MOVE WRK-NOTE-SEQ           TO  CZ-NOTE-SEQUENCE.
041710     GO TO 7191-WRITE.
041710
041710 7199-EXIT.
041710      EXIT.

07341  7200-FIND-BENEFIT.                                               EL156
07342      MOVE 'N' TO WS-BEN-SEARCH-SW.                                EL156
07343                                                                   EL156
07344      EXEC CICS HANDLE CONDITION                                   EL156
07345          ENDFILE(7200-EXIT)                                       EL156
07346          NOTFND(7200-EXIT)                                        EL156
07347      END-EXEC.                                                       CL*88
07348                                                                   EL156
07349      EXEC CICS READ                                               EL156
07350          DATASET('ELCNTL')                                        EL156
07351          SET(ADDRESS OF CONTROL-FILE)                                CL*81
07352          RIDFLD(ELCNTL-KEY)                                       EL156
07353          GTEQ                                                     EL156
07354      END-EXEC.                                                       CL*88
07355                                                                   EL156
07356      IF CNTL-COMP-ID  NOT = CF-COMPANY-ID  OR                     EL156
07357         CNTL-REC-TYPE NOT = CF-RECORD-TYPE                        EL156
07358            GO TO 7200-EXIT.                                       EL156
07359                                                                   EL156
07360      PERFORM 7200-BENEFIT-DUMMY THRU 7200-DUMMY-EXIT              EL156
07361          VARYING SUB-1 FROM 1 BY 1 UNTIL                          EL156
07362             ((SUB-1 > 8) OR                                          CL*94
07363             (CF-BENEFIT-CODE (SUB-1) = WS-BEN-CD)).                  CL*16
07364                                                                   EL156
07365      IF SUB-1 NOT = 9                                             EL156
07366          MOVE 'Y' TO WS-BEN-SEARCH-SW.                            EL156
07367                                                                   EL156
07368      GO TO 7200-EXIT.                                             EL156
07369                                                                   EL156
07370  7200-BENEFIT-DUMMY.                                              EL156
07371                                                                   EL156
07372  7200-DUMMY-EXIT.                                                 EL156
07373      EXIT.                                                        EL156
07374                                                                   EL156
07375  7200-EXIT.                                                       EL156
07376      EXIT.                                                           CL*25
07377      EJECT                                                           CL*25
022208*052506
022208*052506 7250-FIND-LETTER-TRLR.
022208*052506     EXEC CICS HANDLE CONDITION
022208*052506         ENDFILE  (7250-END)
022208*052506         NOTFND   (7250-END)   
022208*052506     END-EXEC.       
022208*052506                     
022208*052506     EXEC CICS STARTBR 
022208*052506         DATASET   ('ELTRLR')    
022208*052506         RIDFLD    (ELTRLR-KEY)  
022208*052506         GTEQ                    
022208*052506     END-EXEC.                   
022208*052506                                 
022208*052506     MOVE LOW-VALUES TO WS-MAX-LETTER-ANSWER-DT.
022208*052506
022208*052506 7250-FIND-LOOP.
022208*052506     EXEC CICS READNEXT                         
022208*052506         DATASET   ('ELTRLR')                   
022208*052506         RIDFLD    (ELTRLR-KEY)                 
022208*052506         SET       (ADDRESS OF ACTIVITY-TRAILERS)
022208*052506     END-EXEC.                                   
022208*052506
022208*052506     IF PI-COMPANY-CD NOT = TRLR-COMP-CD  OR          
022208*052506        PI-CARRIER    NOT = TRLR-CARRIER  OR          
022208*052506        PI-CLAIM-NO   NOT = TRLR-CLAIM-NO OR          
022208*052506        PI-CERT-NO    NOT = TRLR-CERT-NO              
022208*052506        GO TO 7250-END
022208*052506     END-IF.                              
022208*052506                                                      
022208*052506     IF AT-TRAILER-TYPE NOT = '4'                            
022208*052506         GO TO 7250-FIND-LOOP
022208*052506     END-IF.                               
022208*052506
022208*052506     IF AT-LETTER-ANSWERED-DT GREATER THAN WS-MAX-LETTER-ANSWER-DT
022208*052506         MOVE AT-LETTER-ANSWERED-DT TO WS-MAX-LETTER-ANSWER-DT
022208*052506     END-IF.
022208*052506
022208*052506     GO TO 7250-FIND-LOOP.
022208*052506
022208*052506 7250-END.
022208*052506
022208*052506     IF WS-MAX-LETTER-ANSWER-DT NOT = LOW-VALUES AND SPACES
022208*051506         MOVE SPACES             TO  DC-OPTION-CODE               
022208*052506         MOVE WS-MAX-LETTER-ANSWER-DT TO  DC-BIN-DATE-1             
022208*052506         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT                            
022208*052506         MOVE DC-GREG-DATE-1-MDY  TO  PI-PROOF-DATE                     
022208*052506         MOVE PI-PROOF-DATE  TO  PROOFDTO
022208*052506         INSPECT PROOFDTI REPLACING ALL SPACES BY '/'
022208*052506     END-IF.                              
022208*052506
022208*052506     EXEC CICS ENDBR           
022208*052506          DATASET    ('ELTRLR')
022208*052506     END-EXEC.        
022208*052506
022208*052506 7250-EXIT.            
022208*052506     EXIT.             
022208*052506     EJECT             
022208*052506
07378  7300-CHECK-AUTO-ACTIVITY.                                           CL*65
07379                                                                      CL*65
07380      IF PI-PMTTYPE = '1' OR '2' OR '3' OR '4'                        CL*93
07381          NEXT SENTENCE                                               CL*65
07382      ELSE                                                            CL*65
07383          GO TO 7399-EXIT.                                            CL*65
07384                                                                      CL*65
07385      PERFORM 7400-CHECK-AUTO-ACTIVITY THRU 7400-EXIT.                CL*65
07386                                                                      CL*65
07387      IF WS-ACT-REC-FOUND-SW = 'N'                                    CL*88
07388          GO TO 7399-EXIT.                                            CL*65
07389                                                                      CL*65
07390      IF PI-ACTIVITY-CODE = 09                                        CL*88
07391          GO TO 7300-CONT-AUTO-ACTIVITY.                              CL*65
07392                                                                      CL*65
07393      IF (PI-COMPANY-ID = 'AIG' OR 'AUK') AND                         CL*88
07394         (PI-ACTIVITY-CODE = 11)                                      CL*88
07395          GO TO 7300-CONT-AUTO-ACTIVITY.                              CL*65
07396                                                                      CL*67
07397      IF PI-PROVISIONAL-IND = 'P'                                     CL*88
07398          IF PI-PMTTYPE = '1'                                         CL*88
07399              IF CF-SYS-ACTIVE-SW (9) = ' ' OR 'N'                    CL*93
07400                  MOVE 'N'    TO  PI-RESET-SW                         CL*67
07401                  GO TO 7399-EXIT.                                    CL*67
07402                                                                      CL*65
07403      IF PI-PMTTYPE = '1'                                             CL*88
07404          IF CF-SYS-ACTIVE-SW (2) = ' ' OR 'N'                        CL*93
07405              MOVE 'N'        TO  PI-RESET-SW                         CL*65
07406              GO TO 7399-EXIT.                                        CL*65
07407                                                                      CL*65
07408      IF PI-PMTTYPE = '2' OR '3'                                      CL*93
07409          IF CF-SYS-ACTIVE-SW (3) = ' ' OR 'N'                        CL*93
07410              MOVE 'N'        TO  PI-RESET-SW                         CL*65
07411              GO TO 7399-EXIT.                                        CL*65
07412                                                                      CL*65
07413      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*88
07414          IF PI-PROVISIONAL-IND = 'P'                                 CL*88
07415              NEXT SENTENCE                                           CL*70
07416          ELSE                                                        CL*70
07417              PERFORM 7500-RESET-AUTO-ACTIVITY THRU 7599-EXIT.        CL*70
07418                                                                      CL*65
07419  7300-CONT-AUTO-ACTIVITY.                                            CL*65
07420                                                                      CL*65
07421      IF PI-PFKEY-USED = DFHPF3 OR DFHPF4                             CL*93
07422          MOVE PI-EPYTHRU     TO DC-GREG-DATE-1-MDY                   CL*65
07423          MOVE '4'            TO DC-OPTION-CODE                       CL*65
07424          MOVE +0             TO DC-ELAPSED-DAYS                      CL*65
07425                                 DC-ELAPSED-MONTHS                    CL*65
07426          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT               CL*65
07427          MOVE DC-BIN-DATE-1  TO WS-CHK-THRU-DT                       CL*93
07428          IF PI-USES-PAID-TO                                          CL*65
07429              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL*65
07430              IF NO-CONVERSION-ERROR                                  CL*65
07431                  MOVE +1     TO DC-ELAPSED-DAYS                      CL*65
07432                  MOVE +0     TO DC-ELAPSED-MONTHS                    CL*65
07433                  MOVE '6'    TO DC-OPTION-CODE                       CL*65
07434                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT       CL*65
07435                  IF NO-CONVERSION-ERROR                              CL*65
07436                      MOVE DC-BIN-DATE-2   TO WS-CHK-THRU-DT.         CL*65
07437                                                                      CL*65
07438      IF PI-PFKEY-USED = DFHPF5                                       CL*65
07439          MOVE PI-CPYTHRU TO WS-CHK-THRU-DT                           CL*65
07440          IF PI-USES-PAID-TO                                          CL*65
07441              MOVE PI-CPYTHRU          TO DC-BIN-DATE-1               CL*65
07442              MOVE +1                  TO DC-ELAPSED-DAYS             CL*65
07443              MOVE +0                  TO DC-ELAPSED-MONTHS           CL*65
07444              MOVE '6'                 TO DC-OPTION-CODE              CL*65
07445              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL*65
07446              IF NO-CONVERSION-ERROR                                  CL*65
07447                  MOVE DC-BIN-DATE-2   TO WS-CHK-THRU-DT.             CL*65
07448                                                                      CL*65
07449      IF WS-CHK-THRU-DT < PI-EXP-DT                                   CL*94
07450          NEXT SENTENCE                                               CL*65
07451      ELSE                                                            CL*65
07452          MOVE 03                     TO  WS-ACTIVITY-CODE            CL*65
07453          GO TO 7390-PROCESS-ACTIVITY.                                CL*65
07454                                                                      CL*65
07455      IF PI-PROV-PMT                                                  CL*65
07456          MOVE 09                     TO  WS-ACTIVITY-CODE            CL*65
07457      ELSE                                                            CL*65
07458         IF PI-PMTTYPE = '1'                                          CL*93
07459             MOVE 02                  TO  WS-ACTIVITY-CODE            CL*65
07460         ELSE                                                         CL*65
07461         IF PI-PMTTYPE = '2' OR '3'                                   CL*94
07462             MOVE 07                  TO  WS-ACTIVITY-CODE            CL*94
07463         ELSE                                                         CL*94
07464         IF PI-PMTTYPE = '4'                                          CL*94
07465             MOVE 00                  TO  WS-ACTIVITY-CODE            CL*94
07466         ELSE                                                         CL*94
07467             MOVE 'N'                 TO  PI-RESET-SW                 CL*94
07468             GO TO 7399-EXIT.                                         CL*94
07469                                                                      CL*65
07470  7390-PROCESS-ACTIVITY.                                              CL*65
07471                                                                      CL*65
07472      PERFORM 7450-FORMAT-AUTO-LETTER THRU 7450-EXIT.                 CL*65
07473                                                                      CL*65
07474  7399-EXIT.                                                          CL*65
07475      EXIT.                                                           CL*65
07476                                                                      CL*65
07477      EJECT                                                           CL*65
07478  7400-CHECK-AUTO-ACTIVITY.                                           CL*65
07479                                                                      CL*65
07480      EXEC CICS HANDLE CONDITION                                      CL*65
07481          NOTFND   (7400-NOT-FOUND)                                   CL*65
07482      END-EXEC.                                                       CL*65
07483                                                                      CL*65
07484      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                   CL*65
07485      MOVE 'T'                    TO  CNTL-REC-TYPE.                  CL*65
07486      MOVE SPACES                 TO  CNTL-ACCESS.                    CL*65
07487      MOVE +0                     TO  CNTL-SEQ-NO.                    CL*65
07488                                                                      CL*65
07489      EXEC CICS READ                                                  CL*65
07490          DATASET   ('ELCNTL')                                        CL*65
07491          RIDFLD    (ELCNTL-KEY)                                      CL*65
07492          SET       (ADDRESS OF CONTROL-FILE)                         CL*81
07493      END-EXEC.                                                       CL*65
07494                                                                      CL*65
07495      MOVE 'Y'                        TO  WS-ACT-REC-FOUND-SW.        CL*65
07496      GO TO 7400-EXIT.                                                CL*65
07497                                                                      CL*65
07498  7400-NOT-FOUND.                                                     CL*65
07499      MOVE 'N'                        TO  PI-RESET-SW                 CL*65
07500                                          WS-LETTER-SW                CL*65
07501                                          WS-ACT-REC-FOUND-SW.        CL*65
07502                                                                      CL*93
07503      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*65
07504                                                                      CL*65
07505  7400-EXIT.                                                          CL*65
07506      EXIT.                                                           CL*65
07507                                                                      CL*65
07508      EJECT                                                           CL*65
07509  7450-FORMAT-AUTO-LETTER.                                            CL*65
07510                                                                      CL*65
07511      IF PI-ACTIVITY-CODE = ZEROS OR LOW-VALUES OR SPACES             CL*93
07512          MOVE 'Y'                    TO PI-RESET-SW                  CL*93
07513          GO TO 7450-CONTINUE-PROCESS.                                CL*65
07514                                                                      CL*65
07515      MOVE PI-ACTIVITY-CODE           TO  SUB.                        CL*65
07516                                                                      CL*65
07517      IF SUB > 9                                                      CL*94
07518          SUBTRACT 9 FROM SUB                                         CL*65
07519          MOVE CF-USER-RESET-SW (SUB)  TO PI-RESET-SW                 CL*65
07520      ELSE                                                            CL*65
07521          MOVE CF-SYS-RESET-SW (SUB)   TO PI-RESET-SW.                CL*65
07522                                                                      CL*65
07523  7450-CONTINUE-PROCESS.                                              CL*65
07524                                                                      CL*65
07525      IF WS-ACTIVITY-CODE = ZEROS                                     CL*88
07526          MOVE 'N'                    TO  WS-LETTER-SW                CL*65
07527          GO TO 7450-EXIT.                                            CL*65
07528                                                                      CL*65
07529      MOVE WS-ACTIVITY-CODE           TO  SUB.                        CL*65
07530                                                                      CL*65
07531      IF CF-SYS-ACTIVE-SW (SUB) = ' ' OR 'N'                          CL*88
07532          MOVE 'N'                    TO  PI-RESET-SW                 CL*65
07533                                          WS-LETTER-SW                CL*65
07534          GO TO 7450-EXIT.                                            CL*65
07535                                                                      CL*65
07536      IF CF-SYS-LETTER-ID (SUB) = SPACES OR LOW-VALUES                CL*88
07537          MOVE 'N'                    TO  WS-LETTER-SW                CL*65
07538          GO TO 7450-EXIT.                                            CL*65
07539                                                                      CL*65
07540      MOVE 'Y'                        TO  WS-LETTER-SW.               CL*65
07541      MOVE LOW-VALUES                 TO  W-1523-LINKDATA.            CL*65
07542      MOVE PROGRAM-INTERFACE-BLOCK    TO  W-1523-COMMON-PI-DATA.      CL*65
07543                                                                      CL*65
07544      MOVE CF-SYS-LETTER-ID  (SUB)    TO W-1523-FORM-NUMBER.          CL*65
07545                                                                      CL*65
07546      IF CF-SYS-RESEND-DAYS (SUB) NOT = ZEROS                         CL*88
07547          MOVE SAVE-BIN-DATE          TO  DC-BIN-DATE-1               CL*65
07548          MOVE '6'                    TO  DC-OPTION-CODE              CL*65
07549          MOVE CF-SYS-RESEND-DAYS (SUB)   TO  DC-ELAPSED-DAYS         CL*65
07550          MOVE +0                     TO  DC-ELAPSED-MONTHS           CL*65
07551          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT               CL*65
07552          IF NO-CONVERSION-ERROR                                      CL*65
07553              MOVE DC-BIN-DATE-2      TO  W-1523-RESEND-DATE          CL*65
07554          ELSE                                                        CL*65
07555               MOVE LOW-VALUES        TO  W-1523-RESEND-DATE.         CL*65
07556                                                                      CL*65
07557      IF CF-SYS-FOLLOW-UP-DAYS (SUB) NOT = ZEROS                      CL*88
07558          MOVE SAVE-BIN-DATE          TO  DC-BIN-DATE-1               CL*65
07559          MOVE '6'                    TO  DC-OPTION-CODE              CL*65
07560          MOVE CF-SYS-FOLLOW-UP-DAYS (SUB) TO DC-ELAPSED-DAYS         CL*65
07561          MOVE +0                     TO  DC-ELAPSED-MONTHS           CL*65
07562          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT               CL*65
07563          IF NO-CONVERSION-ERROR                                      CL*65
07564              MOVE DC-BIN-DATE-2      TO  W-1523-FOLLOW-UP-DATE       CL*65
07565          ELSE                                                        CL*65
07566              MOVE LOW-VALUES         TO  W-1523-FOLLOW-UP-DATE.      CL*65
07567                                                                      CL*65
07568  7450-EXIT.                                                          CL*65
07569      EXIT.                                                           CL*65
07570                                                                      CL*65
07571      EJECT                                                           CL*65
07572  7500-RESET-AUTO-ACTIVITY.                                           CL*65
07573                                                                      CL*65
07574      MOVE PI-COMPANY-CD          TO  TRLR-COMP-CD.                   CL*65
07575      MOVE PI-CARRIER             TO  TRLR-CARRIER.                   CL*65
07576      MOVE PI-CLAIM-NO            TO  TRLR-CLAIM-NO.                  CL*65
07577      MOVE PI-CERT-NO             TO  TRLR-CERT-NO.                   CL*65
07578      MOVE +100                   TO  TRLR-SEQ-NO.                    CL*65
07579                                                                      CL*65
07580  7500-STARTBR-TRLR.                                                  CL*65
07581                                                                      CL*65
07582      EXEC CICS HANDLE CONDITION                                      CL*65
07583          ENDFILE  (7590-END)                                         CL*65
07584          NOTFND   (7590-END)                                         CL*65
07585      END-EXEC.                                                       CL*65
07586                                                                      CL*65
07587      EXEC CICS STARTBR                                               CL*65
07588          DATASET   ('ELTRLR')                                        CL*65
07589          RIDFLD    (ELTRLR-KEY)                                      CL*65
07590          GTEQ                                                        CL*65
07591      END-EXEC.                                                       CL*65
07592                                                                      CL*65
07593      MOVE 'Y'                        TO  WS-BROWSE-SW.               CL*65
07594                                                                      CL*65
07595  7500-READ-NEXT.                                                     CL*65
07596                                                                      CL*65
07597      EXEC CICS READNEXT                                              CL*65
07598          DATASET   ('ELTRLR')                                        CL*65
07599          RIDFLD    (ELTRLR-KEY)                                      CL*65
07600          SET       (ADDRESS OF ACTIVITY-TRAILERS)                    CL*81
07601      END-EXEC.                                                       CL*65
07602                                                                      CL*65
07603      IF PI-COMPANY-CD NOT = TRLR-COMP-CD  OR                         CL*93
07604         PI-CARRIER    NOT = TRLR-CARRIER  OR                         CL*93
07605         PI-CLAIM-NO   NOT = TRLR-CLAIM-NO OR                         CL*93
07606         PI-CERT-NO    NOT = TRLR-CERT-NO                             CL*93
07607          GO TO 7590-END.                                             CL*65
07608                                                                      CL*65
07609      IF ELTRLR-KEY = PI-PREV-TRLR-KEY                                CL*88
07610          GO TO 7500-READ-NEXT.                                       CL*65
07611                                                                      CL*65
07612      IF AT-TRAILER-TYPE NOT = '4'                                    CL*88
07613          GO TO 7500-READ-NEXT.                                       CL*65
07614                                                                      CL*65
07615      IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES AND SPACES            CL*93
07616          GO TO 7500-READ-NEXT.                                       CL*65
07617                                                                      CL*65
07618      IF (AT-AUTO-RE-SEND-DT   = LOW-VALUES OR SPACES) AND            CL*93
07619         (AT-RECEIPT-FOLLOW-UP = LOW-VALUES OR SPACES)                CL*88
07620          GO TO 7500-READ-NEXT.                                       CL*65
07621                                                                      CL*65
07622      IF AT-RECEIPT-FOLLOW-UP < SAVE-BIN-DATE AND                     CL*94
07623         AT-AUTO-RE-SEND-DT   < SAVE-BIN-DATE AND                     CL*94
07624         AT-RESEND-PRINT-DATE < SAVE-BIN-DATE                         CL*94
07625          GO TO 7500-READ-NEXT.                                       CL*65
07626                                                                      CL*65
07627      IF (AT-AUTO-RE-SEND-DT NOT = LOW-VALUES AND SPACES)             CL*93
07628                          AND                                         CL*93
07629         (AT-RESEND-PRINT-DATE NOT = LOW-VALUES AND SPACES)           CL*93
07630                          AND                                         CL*93
07631         (AT-RECEIPT-FOLLOW-UP = LOW-VALUES)                          CL*93
07632             GO TO 7500-READ-NEXT.                                    CL*93
07633                                                                      CL*65
07634      MOVE ELTRLR-KEY                 TO  PI-PREV-TRLR-KEY.           CL*65
07635                                                                      CL*65
07636      EXEC CICS ENDBR                                                 CL*65
07637          DATASET   ('ELTRLR')                                        CL*65
07638      END-EXEC.                                                       CL*65
07639                                                                      CL*65
07640      MOVE 'N'                        TO  WS-BROWSE-SW.               CL*65
07641                                                                      CL*65
07642      EXEC CICS READ                                                  CL*65
07643          DATASET   ('ELTRLR')                                        CL*65
07644          RIDFLD    (ELTRLR-KEY)                                      CL*65
07645          SET       (ADDRESS OF ACTIVITY-TRAILERS)                    CL*81
07646          UPDATE                                                      CL*65
07647      END-EXEC.                                                       CL*65
07648                                                                      CL*65
07649      MOVE 'Y'                        TO  WS-UPDATE-SW.               CL*65
07650                                                                      CL*65
07651      IF AT-AUTO-RE-SEND-DT NOT < SAVE-BIN-DATE                       CL*94
07652          MOVE LOW-VALUES             TO  AT-AUTO-RE-SEND-DT          CL*65
07653      ELSE                                                            CL*65
07654          IF AT-RECEIPT-FOLLOW-UP NOT < SAVE-BIN-DATE                 CL*94
07655              NEXT SENTENCE                                           CL*65
07656          ELSE                                                        CL*65
07657              GO TO 7500-REWRITE-TRLR.                                CL*65
07658                                                                      CL*65
07659      MOVE LOW-VALUES                 TO  AT-RECEIPT-FOLLOW-UP.       CL*65
07660                                                                      CL*65
07661      MOVE PI-COMPANY-CD              TO  ARCH-CO.                    CL*65
07662      MOVE AT-LETTER-ARCHIVE-NO       TO  ARCH-NUMBER.                CL*65
07663      MOVE '1'                        TO  ARCH-REC-TYPE.              CL*65
07664      MOVE +0                         TO  ARCH-SEQ.                   CL*65
07665                                                                      CL*65
07666      PERFORM 7600-READ-ARCH-UPDATE THRU 7699-EXIT.                   CL*65
07667                                                                      CL*65
07668  7500-REWRITE-TRLR.                                                  CL*65
07669                                                                      CL*65
07670      EXEC CICS REWRITE                                               CL*65
07671          DATASET   ('ELTRLR')                                        CL*65
07672          FROM      (ACTIVITY-TRAILERS)                               CL*65
07673      END-EXEC.                                                       CL*65
07674                                                                      CL*65
07675      GO TO 7500-STARTBR-TRLR.                                        CL*65
07676                                                                      CL*65
07677  7590-END.                                                           CL*65
07678                                                                      CL*65
07679      IF WS-BROWSE-SW = 'Y'                                           CL*88
07680          MOVE 'N'                    TO  WS-BROWSE-SW                CL*65
07681          EXEC CICS ENDBR                                             CL*65
07682              DATASET   ('ELTRLR')                                    CL*65
07683          END-EXEC.                                                   CL*65
07684                                                                      CL*65
07685  7599-EXIT.                                                          CL*65
07686      EXIT.                                                           CL*65
07687                                                                      CL*65
07688      EJECT                                                           CL*65
07689  7600-READ-ARCH-UPDATE.                                              CL*65
07690                                                                      CL*65
07691      EXEC CICS HANDLE CONDITION                                      CL*65
07692          NOTFND   (7699-EXIT)                                        CL*65
07693      END-EXEC.                                                       CL*65
07694                                                                      CL*65
07695      EXEC CICS READ                                                  CL*65
07696          DATASET   ('ELARCH')                                        CL*65
07697          RIDFLD    (ELARCH-KEY)                                      CL*65
07698          SET       (ADDRESS OF LETTER-ARCHIVE)                       CL*81
07699          UPDATE                                                      CL*65
07700      END-EXEC.                                                       CL*65
07701                                                                      CL*65
07702      MOVE LOW-VALUES                 TO  LA-RESEND-DATE.             CL*65
07703                                                                      CL*65
07704      EXEC CICS REWRITE                                               CL*65
07705          DATASET   ('ELARCH')                                        CL*65
07706          FROM      (LETTER-ARCHIVE)                                  CL*65
07707      END-EXEC.                                                       CL*65
07708                                                                      CL*65
07709  7699-EXIT.                                                          CL*65
07710      EXIT.                                                           CL*65
07711                                  EJECT                               CL*82
07712  7700-GET-DCT.                                                       CL*82
07713                                                                      CL*65
07714      MOVE PI-SV-CERT-KEY         TO W-NOTE-CERT-KEY.                 CL*84
07715      MOVE PI-COMPANY-CD          TO W-NOTE-COMP-CD.                  CL*82
07716      MOVE PI-SV-CERT-NO          TO W-NOTE-CERT-NO.                  CL*84
07717      MOVE ZEROS                  TO PI-INTEREST-PAID-IND             CL*82
07718                                     PI-MAX-BENEFIT-AMT               CL*82
07719                                     PI-MAX-BENEFIT-PYMT              CL*82
07720                                     PI-MIN-PAYMENT-AMT               CL*82
07721                                     PI-TIME-OF-LOSS-BAL.             CL*82
07722                                                                      CL*82
07723      EXEC CICS HANDLE CONDITION                                      CL*82
07724           NOTFND (5300-NOTE-NOT-FOUND)                               CL*82
07725       END-EXEC.                                                      CL*88
07726                                                                      CL*82
07727      EXEC CICS READ                                                  CL*82
07728           DATASET('ERNOTE')                                          CL*82
07729           SET    (ADDRESS OF CERTIFICATE-NOTE)                       CL*82
07730           RIDFLD (W-NOTE-KEY)                                        CL*82
07731       END-EXEC.                                                      CL*88
07732                                                                      CL*82
07733      MOVE CN-CSI-CC-INTEREST-PAID TO PI-INTEREST-PAID-IND.           CL*93
07734      MOVE CN-CSI-CC-MAX-BEN-LIMIT TO PI-MAX-BENEFIT-AMT.             CL*93
07735      MOVE CN-CSI-CC-MAX-BENEFITS  TO PI-MAX-BENEFIT-PYMT.            CL*93
07736      MOVE CN-CSI-CC-MIN-PAY-AMT   TO PI-MIN-PAYMENT-AMT.             CL*93
07737      MOVE CN-CSI-CC-TOL-BALANCE   TO PI-TIME-OF-LOSS-BAL.            CL*93
07738                                                                      CL*82
07739      IF PI-PAYEE-TYPE NOT = 'B'                                      CL*94
07740          GO TO 7700-EXIT.                                            CL*94
07741                                                                      CL*94
07742      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.          CL*82
07743      MOVE PI-SV-BEN              TO DCT-LOGIC-BENEFICIARY-ID.        CL*84
07744      MOVE PI-SV-CCN              TO DCT-CREDIT-CARD-NUMBER.          CL*84
07745                                                                      CL*82
07746      IF PI-SV-CERT-KEY (6:2) = ZERO OR SPACES                        CL*86
07747          MOVE 'CC'                 TO DCT-PRODUCT-CODE               CL*93
07748      ELSE                                                            CL*82
07749          MOVE PI-SV-CERT-KEY (6:2) TO DCT-PRODUCT-CODE.              CL*85
07750                                                                      CL*82
07751      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.             CL*83
07752      MOVE '01'                   TO DCT-COLUMN-ID-REQUESTED.         CL*82
07753      MOVE 'DLO006'               TO PGM-NAME.                        CL*82
07754                                                                      CL*83
07755      EXEC CICS LINK                                                  CL*83
07756          PROGRAM    (PGM-NAME)                                       CL*83
07757          COMMAREA   (DCT-COMMUNICATION-AREA)                         CL*83
07758          LENGTH     (DCT-RCRD-LENGTH)                                CL*83
07759      END-EXEC.                                                       CL*88
07760                                                                      CL*83
07761      IF DCT-RETURN-CODE = 'OK'                                       CL*88
07762          GO TO 7700-CONT.                                            CL*83
07763                                                                      CL*82
07764      IF DCT-RETURN-CODE = '01' OR '02'                               CL*88
07765          GO TO 7700-SET-TO-YES.                                      CL*82
07766                                                                      CL*82
07767      IF DCT-RETURN-CODE = '03'                                       CL*88
07768          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*82
07769          MOVE ER-0951            TO EMI-ERROR                        CL*82
07770          MOVE -1                 TO PMTTYPEL                         CL*82
07771          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*82
07772          GO TO 8200-SEND-DATAONLY.                                   CL*82
07773                                                                      CL*82
07774      IF DCT-RETURN-CODE = '04'                                       CL*88
07775          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*82
07776          MOVE ER-0946            TO EMI-ERROR                        CL*82
07777          MOVE -1                 TO PMTTYPEL                         CL*82
07778          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*82
07779          GO TO 8200-SEND-DATAONLY.                                   CL*82
07780                                                                      CL*82
07781      IF DCT-RETURN-CODE = '05'                                       CL*88
07782          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*82
07783          MOVE ER-0947            TO EMI-ERROR                        CL*82
07784          MOVE -1                 TO PMTTYPEL                         CL*94
07785          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*94
07786          GO TO 8200-SEND-DATAONLY.                                   CL*94
07787                                                                      CL*94
07788      IF DCT-RETURN-CODE = '06'                                       CL*94
07789          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*94
07790          MOVE ER-0921            TO EMI-ERROR                        CL*94
07791          MOVE -1                 TO PMTTYPEL                         CL*94
07792          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*94
07793          GO TO 8200-SEND-DATAONLY.                                   CL*94
07794                                                                      CL*94
07795      IF DCT-RETURN-CODE = '07'                                       CL*94
07796          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*94
07797          MOVE ER-0919            TO EMI-ERROR                        CL*94
07798          MOVE -1                 TO PMTTYPEL                         CL*82
07799          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*82
07800          GO TO 8200-SEND-DATAONLY.                                   CL*82
07801                                                                      CL*82
07802      IF DCT-RETURN-CODE = '08'                                       CL*88
07803          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*82
07804          MOVE ER-0948            TO EMI-ERROR                        CL*82
07805          MOVE -1                 TO PMTTYPEL                         CL*82
07806          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*82
07807          GO TO 8200-SEND-DATAONLY.                                   CL*82
07808                                                                      CL*82
07809      IF DCT-RETURN-CODE = 'N1'                                       CL*88
07810          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*82
07811          MOVE ER-0950            TO EMI-ERROR                        CL*82
07812          MOVE -1                 TO PMTTYPEL                         CL*82
07813          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*82
07814          GO TO 8200-SEND-DATAONLY.                                   CL*82
07815                                                                      CL*82
07816      IF DCT-RETURN-CODE = 'E1'                                       CL*86
07817          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*86
07818          MOVE ER-0974            TO EMI-ERROR                        CL*86
07819          MOVE -1                 TO PMTTYPEL                         CL*86
07820          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*86
07821          GO TO 8200-SEND-DATAONLY.                                   CL*86
07822                                                                      CL*86
07823      IF DCT-RETURN-CODE = 'E2'                                       CL*86
07824          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*86
07825          MOVE ER-0975            TO EMI-ERROR                        CL*86
07826          MOVE -1                 TO PMTTYPEL                         CL*86
07827          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*86
07828          GO TO 8200-SEND-DATAONLY.                                   CL*86
07829                                                                      CL*86
07830      IF DCT-RETURN-CODE NOT = 'OK'                                   CL*93
07831          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*82
07832          MOVE ER-0949            TO EMI-ERROR                        CL*82
07833          MOVE -1                 TO PMTTYPEL                         CL*82
07834          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*82
07835          GO TO 8200-SEND-DATAONLY.                                   CL*82
07836                                                                      CL*83
07837  7700-CONT.                                                          CL*83
07838                                                                      CL*82
020413*    MOVE 'N'                    TO CASHO.                           CL*82
020413*    MOVE +1                     TO CASHL.                           CL*82
020413*    MOVE AL-UANON               TO CASHA.                           CL*82
07842                                                                      CL*82
07843      IF PI-OFFLINE = 'Y'                                             CL*93
07844          GO TO 7700-EXIT.                                            CL*93
07845                                                                      CL*93
07846      MOVE DCT-DISTRIBUTION-CODE  TO W-DMD-CHECK-NO-1.                CL*82
07847      MOVE CL-CERT-NO (4:1)       TO W-DMD-CHECK-NO-2.                CL*85
07848      MOVE 'TP'                   TO W-DMD-CHECK-NO-3.                CL*85
07849      MOVE W-DMD-CHECK-NO         TO CHECKNOO.                        CL*82
07850      MOVE +7                     TO CHECKNOL.                        CL*82
07851      MOVE AL-UANON               TO CHECKNOA.                        CL*93
07852      GO TO 7700-EXIT.                                                CL*82
07853                                                                      CL*82
07854  7700-SET-TO-YES.                                                    CL*82
07855                                                                      CL*82
020413*    MOVE 'Y'                    TO CASHO.                           CL*82
020413*    MOVE +1                     TO CASHL.                           CL*82
020413*    MOVE AL-UANON               TO CASHA.                           CL*82
07859                                                                      CL*82
07860  7700-EXIT.                                                          CL*82
07861      EXIT.                                                           CL*82
07862                                  EJECT                               CL*82
07863  7800-READ-COMP-MSTR.                                                CL*25
07864                                                                      CL*25
07865      EXEC CICS HANDLE CONDITION                                      CL*25
07866          NOTFND   (7800-COMP-NOTFND)                                 CL*25
07867      END-EXEC.                                                       CL*25
07868                                                                      CL*25
07869      EXEC CICS READ                                                  CL*25
07870          DATASET   ('ERCOMP')                                        CL*25
07871          RIDFLD    (ERCOMP-KEY)                                      CL*25
07872          SET       (ADDRESS OF COMPENSATION-MASTER)                  CL*81
07873      END-EXEC.                                                       CL*25
07874                                                                      CL*25
07875      MOVE 'Y'                    TO  WS-COMP-MSTR-SW.                CL*25
07876      GO TO 7800-COMP-EXIT.                                           CL*25
07877                                                                      CL*25
07878  7800-COMP-NOTFND.                                                   CL*25
07879      MOVE 'N'                    TO  WS-COMP-MSTR-SW.                CL*25
07880                                                                      CL*25
07881  7800-COMP-EXIT.                                                     CL*25
07882      EXIT.                                                        EL156
07883      EJECT                                                        EL156
07884  7900-READ-CLAIM.                                                 EL156
07885      EXEC CICS READ                                               EL156
07886          DATASET  ('ELMSTR')                                      EL156
07887          SET      (ADDRESS OF CLAIM-MASTER)                          CL*81
07888          RIDFLD   (ELMSTR-KEY)                                    EL156
07889      END-EXEC.                                                       CL*88
07890                                                                   EL156
07891  7900-EXIT.                                                       EL156
07892       EXIT.                                                       EL156
07893                                                                   EL156
07894  7910-READ-CLAIM-UPDATE.                                          EL156
07895      EXEC CICS READ                                               EL156
07896          DATASET  ('ELMSTR')                                      EL156
07897          SET      (ADDRESS OF CLAIM-MASTER)                          CL*81
07898          RIDFLD   (ELMSTR-KEY)                                    EL156
07899          UPDATE                                                   EL156
07900      END-EXEC.                                                       CL*88
07901                                                                   EL156
07902  7910-EXIT.                                                       EL156
07903       EXIT.                                                       EL156
07904      EJECT                                                        EL156
07905  7920-READ-BENE.                                                  EL156
07906      MOVE PI-COMPANY-CD      TO BENE-COMP-CD.                     EL156
07907      MOVE 'B'                TO BENE-RECORD-TYPE.                    CL*16
07908      MOVE CL-BENEFICIARY     TO BENE-NUMBER.                      EL156
07909                                                                      CL*93
07910      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*93
07911         IF PI-PAYEE-TYPE    = 'B' AND                                CL*93
07912            PI-PAYEE-SEQ-NUM = 9                                      CL*93
07913              MOVE CL-CURRENT-GROUPING TO WS-AIG-GROUPING             CL*93
07914              MOVE WS-A-BENE           TO BENE-NUMBER.                CL*93
07915                                                                   EL156
07916      EXEC CICS HANDLE CONDITION                                   EL156
07917          NOTFND   (7920-NOT-FOUND)                                EL156
07918      END-EXEC.                                                       CL*88
07919                                                                   EL156
07920      EXEC CICS READ                                               EL156
07921          DATASET  ('ELBENE')                                      EL156
07922          SET      (ADDRESS OF BENEFICIARY-MASTER)                    CL*81
07923          RIDFLD   (ELBENE-KEY)                                    EL156
07924      END-EXEC.                                                       CL*88
07925                                                                   EL156
07926      MOVE 'Y' TO BENE-FOUND-SW.                                      CL*16
07927      GO TO 7920-EXIT.                                             EL156
07928                                                                   EL156
07929  7920-NOT-FOUND.                                                  EL156
07930      MOVE 'N'                    TO BENE-FOUND-SW.                EL156
07931                                                                   EL156
07932  7920-EXIT.                                                       EL156
07933       EXIT.                                                       EL156
07934      EJECT                                                        EL156
07935  7930-READ-CONTROL.                                               EL156
07936      EXEC CICS READ                                               EL156
07937          DATASET  ('ELCNTL')                                      EL156
07938          SET      (ADDRESS OF CONTROL-FILE)                          CL*81
07939          RIDFLD   (ELCNTL-KEY)                                    EL156
07940      END-EXEC.                                                       CL*88
07941                                                                   EL156
07942  7930-EXIT.                                                       EL156
07943       EXIT.                                                       EL156
07944                                                                   EL156
07945  7940-READ-CONTROL-UPDATE.                                        EL156
07946      EXEC CICS READ                                               EL156
07947          DATASET  ('ELCNTL')                                      EL156
07948          SET      (ADDRESS OF CONTROL-FILE)                          CL*81
07949          RIDFLD   (ELCNTL-KEY)                                    EL156
07950          UPDATE                                                   EL156
07951      END-EXEC.                                                       CL*88
07952                                                                   EL156
07953  7940-EXIT.                                                       EL156
07954       EXIT.                                                       EL156
07955      EJECT                                                        EL156
07956  7950-READ-TRAILER.                                               EL156
07957      EXEC CICS READ                                               EL156
07958          DATASET  ('ELTRLR')                                      EL156
07959          SET      (ADDRESS OF ACTIVITY-TRAILERS)                     CL*81
07960          RIDFLD   (ELTRLR-KEY)                                    EL156
07961      END-EXEC.                                                       CL*88
07962                                                                   EL156
07963  7950-EXIT.                                                       EL156
07964       EXIT.                                                       EL156
07965                                                                   EL156
07966  7960-READ-TRAILER-UPDATE.                                        EL156
07967      EXEC CICS READ                                               EL156
07968          DATASET  ('ELTRLR')                                      EL156
07969          SET      (ADDRESS OF ACTIVITY-TRAILERS)                     CL*81
07970          RIDFLD   (ELTRLR-KEY)                                    EL156
07971          UPDATE                                                   EL156
07972      END-EXEC.                                                       CL*88
07973                                                                   EL156
07974  7960-EXIT.                                                       EL156
07975       EXIT.                                                       EL156
07976      EJECT                                                        EL156
07977  7970-READ-CERT.                                                  EL156
07978      EXEC CICS READ                                               EL156
07979          DATASET  ('ELCERT')                                      EL156
07980          SET      (ADDRESS OF CERTIFICATE-MASTER)                    CL*81
07981          RIDFLD   (ELCERT-KEY)                                    EL156
07982      END-EXEC.                                                       CL*88
07983                                                                   EL156
07984  7970-EXIT.                                                       EL156
07985       EXIT.                                                       EL156

061013 7975-read-acct.
061013
061013     move spaces              to WS-ACCT-RECORD-SW
061013                                 pi-dcc-product-code
061013
061013     MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
061013     MOVE PI-CARRIER          TO ACCT-CARRIER
061013     MOVE PI-GROUPING         TO ACCT-GROUPING
061013     MOVE PI-STATE            TO ACCT-STATE
061013     MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
061013     MOVE PI-CERT-EFF-DT      TO ACCT-EXP-DT
061013     MOVE ERACCT-PARTIAL-KEY  TO WS-ERACCT-SAVE-KEY
061013     MOVE SPACES              TO WS-ERACCT-HOLD-RECORD
061013
061013     EXEC CICS READ
061013        DATASET ('ERACCT')
061013        RIDFLD  (ERACCT-KEY)
061013        SET     (ADDRESS OF ACCOUNT-MASTER)
061013        GTEQ
061013        resp    (WS-RESPONSE)
061013     END-EXEC
061013     IF WS-RESP-NORMAL
061013        AND WS-ERACCT-SAVE-KEY = AM-CONTROL-PRIMARY (1:20)
061013        and pi-cert-eff-dt < am-expiration-dt
061013        and pi-cert-eff-dt >= am-effective-dt
061013        move am-dcc-product-code to pi-dcc-product-code
061013        set acct-found to true
061013     end-if
061013
061013     .
061013 7975-exit.
061013     exit.

07987  7980-READ-CERT-UPDATE.                                           EL156
07988      EXEC CICS READ                                               EL156
07989          DATASET  ('ELCERT')                                      EL156
07990          SET      (ADDRESS OF CERTIFICATE-MASTER)                    CL*81
07991          RIDFLD   (ELCERT-KEY)                                    EL156
07992          UPDATE                                                   EL156
07993      END-EXEC.                                                       CL*88
07994                                                                   EL156
07995  7980-EXIT.                                                       EL156
07996       EXIT.                                                       EL156

090821 7990-get-lo-hi-acct-dates.
090821
090821     MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
090821     MOVE PI-CARRIER          TO ACCT-CARRIER
090821     MOVE PI-GROUPING         TO ACCT-GROUPING
090821     MOVE PI-STATE            TO ACCT-STATE
090821     MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
090821     MOVE low-values          TO ACCT-EXP-DT
090821     MOVE ERACCT-PARTIAL-KEY  TO WS-ERACCT-SAVE-KEY
090821     move spaces              to ws-i-say-stop-ind
090821                                 ws-eracct-startbr-ind
090821                                 ws-acct-status
090821
090821     EXEC CICS STARTBR
090821          DATASET    ('ERACCT')
090821          RIDFLD     (ERACCT-KEY)
090821          GTEQ
090821          resp       (ws-response)
090821     END-EXEC
090821     if ws-resp-normal
090821        set eracct-browse-started to true
090821     end-if
090821
090821     perform until i-say-stop
090821        EXEC CICS READNEXT
090821           DATASET ('ERACCT')
090821           RIDFLD  (ERACCT-KEY)
090821           SET     (ADDRESS OF ACCOUNT-MASTER)
090821           resp    (WS-RESPONSE)
090821        END-EXEC
090821
090821        IF WS-RESP-NORMAL
090821           AND WS-ERACCT-SAVE-KEY = AM-CONTROL-PRIMARY (1:20)
090821           if ws-lo-acct-dt = low-values
090821              move am-effective-dt
090821                                 to ws-lo-acct-dt
090821           end-if
090821           if am-expiration-dt > ws-hi-acct-dt
090821              move am-expiration-dt
090821                                 to ws-hi-acct-dt
090821           end-if
090821           move am-status        to ws-acct-status
090821        else
090821           set i-say-stop to true
090821        end-if
090821     end-perform
090821
090821     if eracct-browse-started
090821        exec cics endbr
090821           dataset('ERACCT')
090821        end-exec
090821        move spaces to ws-eracct-startbr-ind
090821     end-if
090821
090821     .
090821 7990-exit.
090821     exit.

07998  8000-LOAD-ERROR-MESSAGES.                                        EL156
07999      IF PI-PASS-SW = 'C'                                          EL156
08000         MOVE EMI-LINE1           TO ERMSG1BO                      EL156
08001      ELSE                                                         EL156
08002         MOVE EMI-LINE1           TO ERRMSG1O                      EL156
08003         MOVE EMI-LINE2           TO ERRMSG2O.                     EL156
08004                                                                   EL156
08005  8000-EXIT.                                                       EL156
08006      EXIT.                                                        EL156
08007                                                                   EL156
08008      EJECT                                                           CL*88
08009                                                                      CL*88
08010  8010-DMD-ERROR-CHECKS.                                              CL*88
08011      PERFORM 7700-GET-DCT THRU 7700-EXIT.                            CL*88
08012                                                                      CL*88
08013      IF CN-CSI-CC-ISSUE-DT = SPACES OR LOW-VALUES OR ZEROS           CL*88
08014          GO TO 8051-ISSUE-ERROR.                                     CL*88
08015                                                                      CL*88
08016      MOVE CN-CSI-CC-ISSUE-DT     TO SAVE-DATE-CCYYMMDD.              CL*88
08017      MOVE SAVE-DATE-YMD          TO DC-GREG-DATE-1-YMD.              CL*88
08018      MOVE '3'                    TO DC-OPTION-CODE.                  CL*88
08019      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*88
08020      IF DC-BIN-DATE-1 > CL-INCURRED-DT                               CL*94
08021          GO TO 8051-ISSUE-ERROR.                                     CL*88
08022                                                                      CL*88
08023      IF CL-CERT-NO (4:1) = 'U' OR 'F'                                CL*88
08024        IF (CL-SOC-SEC-NO = SPACES OR LOW-VALUES OR ZEROS)            CL*88
08025                            OR                                        CL*88
08026           (CL-SSN-STATE   = CL-CERT-STATE  AND                       CL*88
08027            CL-SSN-ACCOUNT = CL-CERT-ACCOUNT-PRIME)                   CL*88
08028              MOVE ER-0851         TO EMI-ERROR                       CL*88
08029              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*88
08030        END-IF                                                        CL*93
08031        IF CN-CSI-CC-PREMIUM-AMT = ZERO                               CL*88
08032              MOVE ER-8147         TO EMI-ERROR                       CL*88
08033              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*93
08034                                                                      CL*88
08035      GO TO 8090-EXIT.                                                CL*88
08036                                                                      CL*88
08037  8051-ISSUE-ERROR.                                                   CL*88
08038      MOVE ER-8146            TO EMI-ERROR.                           CL*88
08039      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*88
08040      GO TO 8090-EXIT.                                                CL*88
08041                                                                      CL*88
08042  8090-EXIT.                                                          CL*88
08043       EXIT.                                                          CL*88
08044                                                                      CL*88
08045  EJECT                                                               CL*88
08046  8100-SEND-INITIAL-MAP.                                           EL156
08047                                                                      CL*16
08048      MOVE SAVE-DATE              TO RUNDTEAO.                     EL156
08049      MOVE EIBTIME                TO TIME-IN.                      EL156
08050      MOVE TIME-OUT               TO RUNTIMAO.                     EL156
08051      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.             EL156
08052                                                                   EL156
           IF MAP-NAME = 'EL156A'
120115        IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121              OR 'FNL'
                 MOVE 'PF6=EOB CODES'  TO PF6TITO
              END-IF
120115        IF PI-COMPANY-ID = 'DCC' or 'VPP'
032015           MOVE 'N'     TO SURVYYNO
032015        END-IF
           END-IF

020413*    IF MAP-NAME = 'EL156A'                                          CL*93
020413*       IF PI-COMPANY-ID = 'CRI'                                     CL*93
020413*          NEXT SENTENCE                                             CL*18
020413*       ELSE                                                         CL*18
020413*          MOVE AL-SADOF TO  GROUPHA  GROUPEDA.                      CL*93
08064                                                                      CL*18
08065      IF MAP-NAME = 'EL156A'                                          CL*93
062121        IF PI-COMPANY-ID = 'BOA' OR 'CID' OR 'AHL' OR 'FNL'
08067            NEXT SENTENCE                                             CL*57
08068         ELSE                                                         CL*57
08069            MOVE AL-SADOF TO  PINTA    PINTHA.                        CL*93
08070                                                                      CL*57
08071      IF MAP-NAME = 'EL156A'                                          CL*93
08072         IF PI-COMPANY-ID = 'AIG' OR 'AUK'                            CL*93
08073             NEXT SENTENCE                                            CL*65
08074         ELSE                                                         CL*65
08075            MOVE AL-SADOF TO    DUEDAYHA DUEDAYA.                     CL*65
08076                                                                      CL*65
020413*    IF MAP-NAME = 'EL156A'                                          CL*93
020413*       IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'DMD'                   CL*93
020413*          NEXT SENTENCE                                             CL*18
020413*       ELSE                                                         CL*18
020413*          MOVE AL-SADOF TO CASHHA CASHA.                            CL*82
08082                                                                      CL*18
08083      IF MAP-NAME = 'EL156A'                                          CL*93
030512        IF PI-COMPANY-ID = 'CVL' OR 'CID' OR 'DCC' OR 'AHL'
062121                        or 'VPP' OR 'FNL'
08085            NEXT SENTENCE                                             CL*78
08086         ELSE                                                         CL*78
08087            MOVE AL-SADOF        TO  LOANHA                           CL*78
08088                                     LOANNOA.                         CL*78
08089                                                                      CL*78
08090      IF MAP-NAME = 'EL156A'                                          CL*93
08091         MOVE -1                 TO  ENTERPFL                         CL*16
08092         IF PI-USES-PAID-TO                                           CL*16
08093            MOVE 'PAID TO DATE :' TO PTHRHDGO                         CL*16
08094            MOVE '  PAY TO  '    TO  HDGA-VRBLE                       CL*16
08095            MOVE PMT-HEAD-A      TO  PMTHDGAO                         CL*16
08096         ELSE                                                         CL*16
08097            NEXT SENTENCE                                             CL*16
08098      ELSE                                                            CL*16
08099         IF MAP-NAME = 'EL156B'                                       CL*93
08100            IF PI-USES-PAID-TO                                        CL*16
08101               MOVE ' PAYS TO    ' TO HDGB-VRBLE                      CL*16
08102               MOVE PMT-HEAD-B     TO PMTHDGBO.                       CL*16
08103                                                                   EL156
08104      EXEC CICS SEND                                               EL156
08105          MAP   (MAP-NAME)                                         EL156
08106          MAPSET(MAPSET-NAME)                                      EL156
08107          FROM  (EL156AI)                                          EL156
08108          ERASE                                                    EL156
08109          CURSOR                                                   EL156
08110      END-EXEC.                                                       CL*88
08111                                                                   EL156
08112      GO TO 9100-RETURN-TRAN.                                      EL156
08113                                                                   EL156
08114  8200-SEND-DATAONLY.                                              EL156
08115                                                                      CL*82
08116      IF FIRST-ENTRY = 'Y'                                            CL*93
08117          GO TO 8100-SEND-INITIAL-MAP.                                CL*82
08118                                                                      CL*82
08119      IF RETURNED-FROM-B = 'Y'                                     EL156
08120          GO TO 8100-SEND-INITIAL-MAP.                             EL156
08121                                                                   EL156
08122      MOVE SAVE-DATE              TO RUNDTEAO.                     EL156
08123      MOVE EIBTIME                TO TIME-IN.                      EL156
08124      MOVE TIME-OUT               TO RUNTIMAO.                     EL156
08125                                                                   EL156
08126      IF EIBAID = DFHPF11                                          EL156
08127         MOVE 'Y'                 TO EMI-ROLL-SWITCH               EL156
08128         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL*39
08129         MOVE 'EL001   '          TO RETURNED-FROM                    CL*86
08130         PERFORM 3050-REBUILD-ENTERED THRU 3059-EXIT                  CL*86
08131         PERFORM 7010-BUILD-MAP THRU 7020-BYPASS-BENEFIT              CL*86
08132         GO TO 8100-SEND-INITIAL-MAP.                                 CL*86
08133                                                                   EL156
08134      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.             EL156
08135                                                                   EL156
08136      IF MAP-NAME =  'EL156A'                                      EL156
08137         MOVE -1                 TO  ENTERPFL                         CL*16
08138         IF PI-USES-PAID-TO                                           CL*16
08139            MOVE 'PAID TO DATE :' TO PTHRHDGO                         CL*16
08140            MOVE '  PAY TO  '    TO  HDGA-VRBLE                       CL*16
08141            MOVE PMT-HEAD-A      TO  PMTHDGAO                         CL*16
08142         ELSE                                                         CL*16
08143            NEXT SENTENCE                                             CL*16
08144      ELSE                                                            CL*16
08145         IF MAP-NAME = 'EL156B'                                       CL*93
08146            IF PI-USES-PAID-TO                                        CL*16
08147               MOVE ' PAYS TO    ' TO HDGB-VRBLE                      CL*16
08148               MOVE PMT-HEAD-B     TO PMTHDGBO.                       CL*16
08149                                                                   EL156
08150      IF MAP-NAME =  'EL156A'                                         CL*57
08151         IF PI-COMPANY-ID = 'BOA'                                     CL*93
08152             IF PI-INT-RATE NUMERIC                                   CL*57
08153                 MOVE PI-INT-RATE TO PINTO.                           CL*57

           IF MAP-NAME = 'EL156A'
120115        IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121              OR 'FNL'
                 MOVE 'PF6=EOB CODES'  TO PF6TITO
      *          MOVE 'Y'              TO EOBYNO
              END-IF
           END-IF

08160                                                                      CL*82
08161      EXEC CICS SEND                                               EL156
08162          MAP   (MAP-NAME)                                         EL156
08163          MAPSET(MAPSET-NAME)                                      EL156
08164          FROM  (EL156AI)                                          EL156
08165          DATAONLY                                                 EL156
08166          CURSOR                                                   EL156
08167      END-EXEC.                                                       CL*88
08168                                                                   EL156
08169      GO TO 9100-RETURN-TRAN.                                      EL156
08170                                                                   EL156
08171  8300-SEND-TEXT.                                                  EL156
08172      EXEC CICS SEND TEXT                                          EL156
08173          FROM(LOGOFF-TEXT)                                        EL156
08174          LENGTH(LOGOFF-LENGTH)                                    EL156
08175          ERASE                                                    EL156
08176          FREEKB                                                   EL156
08177      END-EXEC.                                                       CL*88
08178                                                                   EL156
08179      EXEC CICS RETURN                                             EL156
08180      END-EXEC.                                                       CL*88
08181                                                                   EL156
08182  8500-FILE-NOTOPEN.                                               EL156
08183      IF FILE-SWITCH = 'MSTR'                                      EL156
08184          MOVE ER-0154            TO EMI-ERROR.                    EL156
08185      IF FILE-SWITCH = 'TRLR'                                      EL156
08186          MOVE ER-0172            TO EMI-ERROR.                    EL156
08187      IF FILE-SWITCH = 'CERT'                                      EL156
08188          MOVE ER-0169            TO EMI-ERROR.                    EL156
08189      IF FILE-SWITCH = 'CNTL' OR 'CARR' OR 'BENE' OR                  CL*88
08190                       'PROC' OR 'STAT'                               CL*88
08191          MOVE ER-0042            TO EMI-ERROR.                    EL156
08192      IF FILE-SWITCH = 'ACTQ'                                      EL156
08193          MOVE ER-0338            TO EMI-ERROR.                    EL156
08194      IF FILE-SWITCH = 'ACCT'                                      EL156
08195          MOVE ER-0168            TO EMI-ERROR.                    EL156
08196                                                                   EL156
08197      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL156
08198      MOVE -1                     TO PMTTYPEL.                     EL156
08199                                                                   EL156
08200      IF FIRST-ENTRY = 'Y'                                         EL156
08201          GO TO 8100-SEND-INITIAL-MAP.                             EL156
08202                                                                   EL156
08203      GO TO 8200-SEND-DATAONLY.                                    EL156
08204                                                                      CL*82
08205  8600-PGRM-NOT-FOUND.                                                CL*82
08206                                                                      CL*82
08207      MOVE ER-0923                TO EMI-ERROR.                       CL*82
08208      MOVE -1                     TO ENTPFBL.                         CL*82
08209      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*82
08210                                                                      CL*82
08211      IF FIRST-ENTRY = 'Y'                                            CL*93
08212          GO TO 8100-SEND-INITIAL-MAP                                 CL*82
08213      ELSE                                                            CL*82
08214          GO TO 8200-SEND-DATAONLY.                                   CL*82
08215                                                                   EL156
08216  8800-UNAUTHORIZED-ACCESS.                                        EL156
08217      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL156
08218      GO TO 8300-SEND-TEXT.                                        EL156
08219                                                                   EL156
08220  8810-PF23.                                                       EL156
08221      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL156
08222      MOVE XCTL-005               TO PGM-NAME.                     EL156
08223      GO TO 9300-XCTL.                                             EL156
08224                                                                   EL156
08225  8820-PF7-8.                                                      EL156
08226      MOVE XCTL-126               TO PI-CALLING-PROGRAM.           EL156
08227      MOVE SPACES                 TO PI-SAVED-PROGRAM-1            EL156
08228                                     PI-SAVED-PROGRAM-2            EL156
08229                                     PI-SAVED-PROGRAM-3            EL156
08230                                     PI-SAVED-PROGRAM-4            EL156
08231                                     PI-SAVED-PROGRAM-5            EL156
08232                                     PI-SAVED-PROGRAM-6            EL156
08233                                     PI-RETURN-TO-PROGRAM.         EL156
08234      IF EIBAID = DFHPF8                                           EL156
08235         MOVE XCTL-132            TO PGM-NAME                      EL156
08236      ELSE                                                         EL156
08237         MOVE XCTL-130            TO PGM-NAME.                     EL156
08238                                                                   EL156
08239      GO TO 9300-XCTL.                                             EL156
08240                                                                   EL156
08241  9100-RETURN-TRAN.                                                EL156
08242      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL156
08243      MOVE '156A'                 TO PI-CURRENT-SCREEN-NO.         EL156
08244                                                                   EL156
08245      EXEC CICS RETURN                                             EL156
08246          TRANSID(TRANS-ID)                                        EL156
08247          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL156
08248          LENGTH(PI-COMM-LENGTH)                                   EL156
08249      END-EXEC.                                                       CL*88
08250                                                                   EL156
08251  9200-RETURN-MAIN-MENU.                                           EL156
08252      MOVE XCTL-126               TO PGM-NAME.                     EL156
08253      GO TO 9300-XCTL.                                             EL156
08254                                                                   EL156
08255  9300-XCTL.                                                       EL156
08256      EXEC CICS XCTL                                               EL156
08257          PROGRAM(PGM-NAME)                                        EL156
08258          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL156
08259          LENGTH(PI-COMM-LENGTH)                                   EL156
08260      END-EXEC.                                                       CL*88
08261                                                                   EL156
08262  9400-CLEAR.                                                      EL156
08263      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL156
08264      GO TO 9300-XCTL.                                             EL156
08265                                                                   EL156
08266  9500-PF12.                                                       EL156
08267      MOVE XCTL-010               TO PGM-NAME.                     EL156
08268      GO TO 9300-XCTL.                                             EL156
08269                                                                   EL156
08270  9700-LINK-DATE-CONVERT.                                          EL156
08271      MOVE LINK-ELDATCV           TO PGM-NAME.                        CL*20
08272      EXEC CICS LINK                                               EL156
08273          PROGRAM   (PGM-NAME)                                     EL156
08274          COMMAREA  (DATE-CONVERSION-DATA)                         EL156
08275          LENGTH    (DC-COMM-LENGTH)                               EL156
08276      END-EXEC.                                                       CL*88
08277                                                                   EL156
08278  9700-EXIT.                                                       EL156
08279      EXIT.                                                        EL156
08280                                                                   EL156
08281  9800-LINK-REM-TERM.                                              EL156
08282      MOVE LINK-REMTERM TO PGM-NAME.                               EL156
08283                                                                   EL156
08284      EXEC CICS LINK                                               EL156
08285          PROGRAM   (PGM-NAME)                                     EL156
08286          COMMAREA  (CALCULATION-PASS-AREA)                        EL156
08287          LENGTH    (CP-COMM-LENGTH)                               EL156
08288      END-EXEC.                                                       CL*88
08289                                                                   EL156
08290  9800-EXIT.                                                       EL156
08291      EXIT.                                                        EL156
08292                                  EJECT                               CL*82
08293  9830-DMD-REMAINING-TERM.                                            CL*82
08294                                                                      CL*82
08295      DIVIDE CL-NO-OF-DAYS-PAID BY +30                                CL*82
08296          GIVING W-PAID-MONTHS REMAINDER W-PAID-DAYS.                 CL*82
08297                                                                      CL*82
08298      COMPUTE W-TERM-IN-DAYS = CM-AH-ORIG-TERM * 30 +                 CL*98
08299                               CM-PMT-EXTENSION-DAYS.                 CL*98
08300                                                                      CL*82
08301      COMPUTE W-REM-TERM-IN-DAYS = W-TERM-IN-DAYS -                   CL*98
08302                                   CL-NO-OF-DAYS-PAID.                CL*98
08303                                                                      CL*98
08304      DIVIDE W-REM-TERM-IN-DAYS BY +30                                CL*98
08305          GIVING W-REM REMAINDER W-REM-DAYS.                          CL*98
08306                                                                      CL*98
08307      MOVE W-REM                  TO WST-REM.                         CL*98
08308                                                                      CL*98
08309      IF W-REM-DAYS > ZEROS                                           CL*98
08310          MOVE '/'                TO WST-SLASH                        CL*98
08311          MOVE W-REM-DAYS         TO WST-REM-DAYS                     CL*98
08312      ELSE                                                            CL*82
08313          MOVE SPACES             TO WST-REM-DAYS-GRP.                CL*98
08314                                                                      CL*82
08315  9830-EXIT.                                                          CL*82
08316      EXIT.                                                           CL*82
08317                                                                   EL156
CIDMOD 9870-OUTPUT-ACTIVITY-RECORD.                                     00007117
CIDMOD                                                                  00007118
CIDMOD     EXEC CICS GETMAIN                                            00007119
CIDMOD          SET(ADDRESS OF DAILY-ACTIVITY-RECORD)                   00007120
CIDMOD          LENGTH(25)                                              00007121
CIDMOD          INITIMG(WS-BLANK)                                       00007122
CIDMOD     END-EXEC.                                                    00007123
CIDMOD                                                                  00007123
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.        00007125
CIDMOD     MOVE ELMSTR-KEY             TO DA-KEY.                       00007126
CIDMOD     MOVE CL-TRAILER-SEQ-CNT     TO DA-TRAILER-SEQ-NO.            00007127
CIDMOD
CIDMOD     IF PI-PMTTYPE EQUAL '7'                                      00007128
CIDMOD         MOVE 'V'                TO DA-RECORD-TYPE                00007129
CIDMOD     ELSE                                                         00007130
CIDMOD         MOVE 'P'                TO DA-RECORD-TYPE.               00007131
CIDMOD
CIDMOD     EXEC CICS HANDLE CONDITION                                   00007136
CIDMOD          NOTOPEN(9870-NOTOPEN)                                   00007137
CIDMOD          DUPREC(9870-EXIT)                                       00007138
CIDMOD     END-EXEC.                                                    00007139
CIDMOD
CIDMOD     EXEC CICS WRITE                                              00007140
CIDMOD          DATASET('DLYACTV')                                      00007141
CIDMOD          RIDFLD(DA-KEY)                                          00007142
CIDMOD          FROM(DAILY-ACTIVITY-RECORD)                             00007143
CIDMOD          LENGTH(25)                                              00007144
CIDMOD     END-EXEC.                                                    00007145
CIDMOD     MOVE 'N'                    TO ERROR-ON-OUTPUT-SW.           00007146
CIDMOD     GO TO 9870-EXIT.                                             00007148
CIDMOD                                                                  00007149
CIDMOD 9870-NOTOPEN.                                                    00007150
CIDMOD                                                                  00007151
CIDMOD     MOVE '2955'                 TO EMI-ERROR.                    00007153
CIDMOD     MOVE 'Y'                    TO ERROR-ON-OUTPUT-SW.           00007154
CIDMOD                                                                  00007155
CIDMOD 9870-EXIT.                                                       00007156
CIDMOD     EXIT.                                                        00007157
CIDMOD                                                                  00007158
08318  9900-ERROR-FORMAT.                                               EL156
08319                                                                      CL*82
08320      IF EMI-ERROR = ER-0923                                          CL*93
08321          MOVE PGM-NAME           TO W-CALLED-NAME.                   CL*82
08322                                                                      CL*82
08323      MOVE LINK-001      TO PGM-NAME.                                 CL*93
08324      MOVE PI-COMPANY-ID TO EMI-CLIENT-ID.                            CL*65
08325                                                                   EL156
08326      EXEC CICS LINK                                               EL156
08327          PROGRAM (PGM-NAME)                                       EL156
08328          COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)                  EL156
08329          LENGTH  (EMI-COMM-LENGTH)                                EL156
08330      END-EXEC.                                                       CL*88
08331                                                                      CL*82
08332      IF EMI-ERROR = ER-0923                                          CL*93
08333          MOVE W-CALLED-NAME      TO EMI-TEXT-VARIABLE (1).           CL*82
08334                                                                   EL156
08335  9900-EXIT.                                                       EL156
08336      EXIT.                                                        EL156
08337                                                                   EL156
08338  9990-ABEND.                                                      EL156
08339      MOVE LINK-004               TO PGM-NAME.                        CL*93
08340      MOVE DFHEIBLK               TO EMI-LINE1.                    EL156
08341      EXEC CICS LINK                                               EL156
08342          PROGRAM   (PGM-NAME)                                     EL156
08343          COMMAREA  (EMI-LINE1)                                    EL156
08344          LENGTH    (64)                                           EL156
08345      END-EXEC.                                                       CL*88
08346                                                                   EL156
08347      MOVE 'EL156A'               TO  MAP-NAME.                    EL156
08348      MOVE LOW-VALUES             TO  EL156AO.                     EL156
08349      GO TO 8100-SEND-INITIAL-MAP.                                 EL156
08350                                                                   EL156
08351  9995-SECURITY-VIOLATION.                                         EL156
08352      COPY ELCSCTP.                                                   CL*43
08353                                                                   EL156
