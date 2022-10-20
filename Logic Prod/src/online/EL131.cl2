00001  IDENTIFICATION DIVISION.                                         11/20/97
00002                                                                   EL131
00003  PROGRAM-ID.                 EL131 .                                 LV046
00004 *              PROGRAM CONVERTED BY                                  CL*32
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*32
00006 *              CONVERSION DATE 04/03/95 18:43:40.                    CL*32
00007 *                            VMOD=2.046.                             CL*46
00008 *                                                                 EL131
00009 *AUTHOR.        LOGIC, INC.                                          CL*32
00010 *               DALLAS, TEXAS.                                       CL*32
00024 *REMARKS. TRANSACTION EX24 - CLAIM MAINTENANCE.                      CL**7
00023 *                                                                 EL131
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID, POPULATE COMPO
062602* 062602    2002030700006  PEMA  Add note type of 'S'
062602*                                  (special review)
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
071508* 071508    2008071000003  AJRA  ADD EDIT FOR SOC SEC NUMBER
012009* 012009    2007042600001  PEMA  RESTRICT CLAIM TYPE FOR CID
032612* 032612    2011110200001  PEMA  AHL CHANGES
040412* 040412    2012040400003  AJRA  DO NOT DELETE CERT ON AHL
032613* 032613    2013032500002  AJRA  PROTECT CERT#,CARR,GRP,ST,ACCT,EFFDT
052113* 052113    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
060413* 060413    2013060300001  AJRA  CHECK FOR AUTO PAY WHEN BENE REMOVED
071613* 071613    2013071200001  PEMA  REMOVE INC & RPT DATE EDITS.
080613* 080613    2013071600001  PEMA  ALLOW INC DTE CHANGE
082013* 082013    2013040900001  AJRA  ADD PF15 BENEFICIARY MAINT
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
111113* 111113    2013110600002  AJRA  CHG LEVEL 5 RESTRICTIONS TO LEVEL 4 & 5
040814* 040814    2014030500002  AJRA  ADD ICD CODES
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
070714* 070714    2014052800001  PEMA  correct read on erpdef for DCC
031715* 031715  CR2015022700002  PEMA  CHECK CHG IN DIAGNOSIS
031815* 031815    2015021700001  TANA  BENE EDIT IF A PAYMENT PENDING
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
040616* 040617  CR2017022200003  TANA  INCREASE CHG INC DT TO 180 DAYS
062217* 062217  CR2017050300002  TANA  ADD AUTH RCVD FIELD
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
101917* 101917  CR2017083000003  TANA  ADD CONTRACT CONTESTABLE MSG
021418* 021418  CR2017110200001  TANA  EDIT NAME CHANGE AGST INSRD TYPE
052918* 052918  CR2018031500002  TANA  Add Message for filing time limit
061418* 061418  IR2018053000003  TANA  Fix Causal state / filing limit
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
011020* 011020  IR2019121100004  PEMA  ADD last name to compare.
071720* 071720  CR2019112600001  TANA  Remove filing time limit error
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
090821* 090821  CR2021081200003  PEMA  Add error if inc > act cnc dt
080322* 080322  CR2021100800003  TANA  Add B and H claim types
101501******************************************************************

00025                                                                   EL131
00026      EJECT                                                        EL131
00027  ENVIRONMENT DIVISION.                                            EL131
00028                                                                   EL131
00029  DATA DIVISION.                                                   EL131
00030                                                                   EL131
00031  WORKING-STORAGE SECTION.                                         EL131
00036                                                                   EL131
00037  77  FILLER  PIC X(32)  VALUE '********************************'. EL131
00038  77  FILLER  PIC X(32)  VALUE '*   EL131  WORKING STORAGE     *'. EL131
00039  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.046 *********'.    CL*46
052113 77  s1                          pic s999 comp-3 value+0.
052113 77  s2                          pic s999 comp-3 value+0.
052113 77  a1                          pic s999 comp-3 value+0.
052113 77  WS-CRITICAL-PERIOD          PIC 99    VALUE ZEROS.
052113 77  WS-CRIT-PER-RTW-MOS         PIC 99    VALUE ZEROS.
052113 77  WS-EXCL-PERIOD              PIC S999 COMP-3 VALUE +0.
       77  ws-pre-exsist               pic s999 comp-3 value +0.
052113 77  WS-COV-ENDS                 PIC S999 COMP-3 VALUE +0.
052113 77  WS-ACC-PERIOD               PIC S999 COMP-3 VALUE +0.
       77  ws-max-extension            pic s999 comp-3 value +0.
       77  ws-max-moben                pic s9(7)v99 comp-3 value +0.
052113 77  WS-MONTHS-BETWEEN           PIC S999 COMP-3 VALUE +0.
052113 77  WS-ERPDEF-SW                PIC X     VALUE ' '.
052113     88  ERPDEF-FOUND                 VALUE 'Y'.
031815 77  WS-TRLR-FILE-EOF            PIC X     VALUE ' '.
031815     88  TRLR-FILE-EOF                VALUE 'Y'.
031815 77  WS-PAYMENT-PENDING          PIC X     VALUE ' '.
031815     88  PAYMENT-PENDING              VALUE 'Y'.
062217 77  WS-AUTH-RCVD                PIC X     VALUE ' '.
062217     88  AUTH-RCVD                    VALUE 'Y'.
062217     88  NO-AUTH-RCVD                 VALUE 'N'.

       77  ws-eracct-sw                pic x  value ' '.
           88  acct-found                value 'Y'.
       77  ws-benper-sw                pic x   value ' '.
           88  good-benper               value 'Y'.
       77  ws-claim-type               pic x value ' '.
       77  ws-ben-per                  pic 99 value 00.
       77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
       77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
       77  ws-pd-bens                  pic s9(5) comp-3 value +0.
       77  ws-max-bens                 pic s9(5) comp-3 value +0.
090821 77  sub                         pic s999 comp-3 value +0.
080322 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
00040                                                                   EL131
00032  01  LCP-TIME-OF-DAY-XX.                                             CL*32
00033      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL*32
00034      05  FILLER                    PIC 99.                           CL*32
00035  01  LCP-CICS-TIME                 PIC 9(15).                        CL*32
00041                              COPY ELCSCTM.                           CL*22
00042                                                                   EL131
00043                              COPY ELCSCRTY.                          CL*22
00044                                                                   EL131
00045                              COPY ELCNWA.                            CL*34
00046                                                                      CL*34
052113 01  ws-dcc-error-line.
052113     05  filler occurs 15.
052113         10  ws-error-no        pic x(4).
00047  01  WS-DATE-AREA.                                                EL131
00048      12  SAVE-DATE              PIC X(8)    VALUE SPACES.            CL*34
00049      12  SAVE-BIN-DATE          PIC XX      VALUE SPACES.            CL*34
00050      12  SAVE-DATE-CCYYMMDD.                                         CL*34
00051          16  SAVE-DATE-CC       PIC XX      VALUE SPACES.            CL*34
00052          16  SAVE-DATE-YMD.                                          CL*34
00053              20  SAVE-DATE-YY   PIC XX      VALUE SPACES.            CL*34
00054              20  FILLER         PIC X(4)    VALUE SPACES.            CL*34
CIDMOD
CIDMOD 01  WS-BLANK                   PIC X       VALUE ' '.                 000
CIDMOD
CIDMOD 01  CSO-WORK-FIELDS.                                                  000
CIDMOD     05  WS-HOLD-CLAIM-STATUS    PIC X.                                000
           05  WS-CRIT-PER-RECURRENT   PIC 99    VALUE 00.
           05  WS-CRIT-PER-ALPHA REDEFINES WS-CRIT-PER-RECURRENT.
               10  WS-RECURRENT-YN     PIC X.
               10  FILLER              PIC X.
00055                                                                   EL131
00056  01  LITERALS-NUMBERS.                                            EL131
00057      12  SC-ITEM                 PIC S9(4)   VALUE +1    COMP.       CL**8
00058      12  NINETY                  PIC S9(4)   VALUE +90   COMP.       CL**8
00059      12  ELMSTR-GENERIC-LENGTH   PIC S9(4)   VALUE +9    COMP.       CL**8
00060      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         EL131
00061      12  THIS-PGM                PIC X(8)    VALUE 'EL131'.       EL131
00062      12  TRAN-ID                 PIC X(4)    VALUE 'EX24'.        EL131
00063      12  MAP-ID                  PIC X(4)    VALUE '131A'.        EL131
082013     12  MAP-LENGTH              PIC S9(4)   VALUE +1060  COMP.
00064                                                                      CL*26
00065      12  XCTL-EL001              PIC X(8)    VALUE 'EL001'.          CL**8
00066      12  XCTL-EL004              PIC X(8)    VALUE 'EL004'.          CL**8
00067      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.       EL131
00068      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.       EL131
082013     12  XCTL-EL114              PIC X(8)    VALUE 'EL114'.
00069      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.       EL131
00070      12  XCTL-EL1273             PIC X(8)    VALUE 'EL1273'.         CL**8
00071      12  XCTL-EL141              PIC X(8)    VALUE 'EL141'.       EL131
00072      12  XCTL-EL142              PIC X(8)    VALUE 'EL142'.       EL131
00073      12  XCTL-EL400DMD           PIC X(8)    VALUE 'EL400DMD'.       CL*34
00074                                                                      CL*26
00075      12  LINK-1523               PIC X(8)    VALUE 'EL1523'.         CL*26
00076                                                                      CL*26
00077      12  DATE-CONV               PIC X(8)    VALUE 'ELDATCV'.     EL131
00078                                                                      CL*26
00079      12  RTRM-FILE-ID            PIC X(8)    VALUE 'ELRTRM'.      EL131
00080      12  CLMS-FILE-ID            PIC X(8)    VALUE 'ELMSTR'.      EL131
00081      12  NOTE-FILE-ID            PIC X(8)    VALUE 'ERNOTE'.         CL*34
00082      12  CERT-FILE-ID            PIC X(8)    VALUE 'ELCERT'.      EL131
00083      12  TRLR-FILE-ID            PIC X(8)    VALUE 'ELTRLR'.      EL131
00084      12  CNTL-FILE-ID            PIC X(8)    VALUE 'ELCNTL'.      EL131
00085      12  ACTQ-FILE-ID            PIC X(8)    VALUE 'ELACTQ'.      EL131
00086      12  CHKQ-FILE-ID            PIC X(8)    VALUE 'ELCHKQ'.      EL131
00087      12  ACCT-FILE-ID            PIC X(8)    VALUE 'ERACCT'.      EL131
00088      12  ARCH-FILE-ID            PIC X(8)    VALUE 'ELARCH'.         CL*10
00089      12  ALPH-FILE-ID            PIC X(8)    VALUE 'ELALPH'.         CL*26
101917     12  WS-CONTEST-NOTE      PIC X(44)
101917         VALUE 'CONTRACT NOW CONTESTABLE - INC/RPT DT CHGD'.
052918     12  WS-FILE-LIM-NOTE     PIC X(44)
052918         VALUE 'FILING TIME LIMIT OK - INCRD DT CHGD'.
061418
061418     12  WS-FILING-NOTE     PIC X(30)
061418         VALUE 'FILING TIME LIMIT EXCEEDED'.

00090                                                                      CL*26
00091      12  MISC-SUB                PIC S9(3)   COMP-3  VALUE +0.    EL131
00092      12  ONE-OR-MIN1             PIC S9      COMP-3  VALUE +1.       CL*34
00093      12  WS-READNEXT-SWITCH      PIC S99     VALUE +1.               CL**8
00094      12  WS-ASSOC-CERT-TOTAL     PIC S99             VALUE ZEROS.    CL**8
00095          88  NO-ASSOCIATED-CERTS                     VALUE ZEROS.    CL**8
00096      12  WS-ASSOC-CERT-SEQU      PIC S99             VALUE ZEROS.    CL**8
00097      12  WS-ASSOCIATED-CERTS     PIC S9      COMP-3  VALUE +0.       CL**8
00098      12  WS-DIAGNOSIS            PIC X(60)   VALUE SPACES.           CL*39
040814     12  WS-ICD-CODE-1           PIC X(8).
040814     12  WS-ICD-CODE-2           PIC X(8).
00099      12  WS-CLAIM-SEQU.                                              CL**8
00100          16  FILLER              PIC X VALUE '('.                    CL*34
00101          16  WS-CURRENT-SEQU     PIC Z9.                             CL**8
00102          16  FILLER              PIC X(04) VALUE ' OF '.             CL**8
00103          16  WS-OF-SEQU          PIC Z9.                             CL**8
00104          16  FILLER              PIC X VALUE ')'.                    CL*34
00105      12  WS-EDIT-BEN-CODE        PIC XX.                             CL**8
00106          88  INVALID-BENEFIT-CODE   VALUE '  ' '00'                  CL**8
00107                                           '90' THRU '99'.            CL**8
00108                                                                      CL*26
00109      12  WS-EDIT-AMT             PIC ZZZ,ZZZ.99.                     CL*34
00110      12  WS-EDIT-NUMBER          PIC ZZZ9.                           CL*34
00111                                                                      CL*45
00152      12  WS-RESPONSE             PIC S9(8)   COMP.                    
00153          88  WS-RESP-NORMAL              VALUE +00.               
052113         88  WS-RESP-ERROR               VALUE +01.               
052113         88  WS-RESP-NOTFND              VALUE +13.               
052113         88  WS-RESP-DUPKEY              VALUE +15.
052113         88  WS-RESP-NOTOPEN             VALUE +19.
052113         88  WS-RESP-ENDFILE             VALUE +20.
00115                                                                      CL*34
00116      12  WS-MOS-PAID             PIC 999.                            CL*34
00117      12  WS-ODD-DAYS             PIC 99.                             CL*34
00118      12  WS-REM-MOS              PIC 999.                            CL*34
00119      12  WS-REM-DAYS             PIC 99.                             CL*34
00120      12  WS-TERM-IN-DAYS         PIC 9(4).                           CL*46
00121      12  WS-REM-TERM-IN-DAYS     PIC 9(4).                           CL*46
00122                                                                      CL*34
00123      12  WS-BROWSE-SW            PIC X       VALUE 'N'.              CL*34
00124      12  WS-UPDATE-SW            PIC X       VALUE 'N'.              CL*34
00125      12  WS-OPEN-CLOSE-SW        PIC X       VALUE ' '.              CL*34
00126      12  WS-RESET-SW             PIC X       VALUE 'N'.              CL*34
00127      12  WS-REC-FOUND-SW         PIC X       VALUE 'N'.              CL*34
00128      12  WS-DMO-LENGTH           PIC S9(4)   VALUE +108 COMP.        CL*36
00129      12  WS-DCT-LENGTH           PIC S9(4)   VALUE +53 COMP.         CL*36

090821 01  filler.
090821     05  ws-prev-inc-dt          pic xx value low-values.
090821     05  ws-mob-cert-ind         pic x value ' '.
090821         88  mob-cert        value 'M'.
090821     05  ws-eracct-startbr-ind   pic x  value spaces.
090821         88  eracct-browse-started  value 'Y'.
090821     05  ws-lo-acct-dt           pic xx value low-values.
090821     05  ws-hi-acct-dt           pic xx value low-values.
090821     05  ws-acct-status          pic x value spaces.
090821         88  acct-cancelled          value '3'.
090821     05  WS-I-SAY-STOP-IND       PIC X  VALUE ' '.
090821         88  i-say-STOP            value 'S'.
090821     05  er-1679-text        pic x(60) value
090821       '1679-N CONTRACT IS NOT CONTESTABLE'.
090821     05  er-1682-text        pic x(60) value
090821       '1682-N INC DATE > MOB ACCOUNT CANCEL DATE'.
090821     05  er-1683-text        pic x(60) value
090821       '1683-N INC DATE < CERTIFICATE EFF DATE'.
090821     05  ws-rec-type             pic x.
090821     05  ws-ben-hold             pic xx.
090821     05  ws-special-calc-cd      pic x.

00131  01  DMD-DATE-YYYYMMDD.                                              CL*34
00132      12  DMD-DECADE          PIC XX      VALUE SPACES.               CL*34
00133      12  DMD-YYMMDD.                                                 CL*34
00134          16  DMD-YY          PIC XX      VALUE SPACES.               CL*34
00135          16  DMD-MM          PIC XX      VALUE SPACES.               CL*34
00136          16  DMD-DD          PIC XX      VALUE SPACES.               CL*34
00137                                                                      CL*34
00138  01  DMD-DATE-MMDDYYYY.                                              CL*34
00139      12  DMD-MDY-MM          PIC XX      VALUE SPACES.               CL*34
00140      12  DMD-MDY-SLASH1      PIC X       VALUE '/'.                  CL*34
00141      12  DMD-MDY-DD          PIC XX      VALUE SPACES.               CL*34
00142      12  DMD-MDY-SLASH2      PIC X       VALUE '/'.                  CL*34
00143      12  DMD-MDY-DECADE      PIC XX      VALUE SPACES.               CL*34
00144      12  DMD-MDY-YY          PIC XX      VALUE SPACES.               CL*34
00145                                                                      CL*34
00146  01  WS-MAIL-CODE.                                                   CL*34
00147      12  FILLER              PIC X.                                  CL*34
00148      12  DMD-MAIL-CODE.                                              CL*34
00149          16  WS-MAIL-4       PIC X(4).                               CL*34
00150          16  WS-MAIL-5       PIC X.                                  CL*34
00151      12  FILLER              PIC X(4).                               CL*34
00152                                                                      CL*34
00153      12  W-NAME-LAST             PIC  X(15).                         CL*34
00154      12  W-NAME-FIRST            PIC  X(15).                         CL*34
00155      12  W-NAME-MIDDLE.                                              CL*34
00156          16  FILLER              PIC  X.                             CL*34
00157          16  W-NAME-MIDDLE-2     PIC  X.                             CL*34
00158          16  FILLER              PIC  X(13).                         CL*34
00159                                                                      CL*34
00160      COPY ELCDCTB.                                                   CL*34
00161      EJECT                                                        EL131
00162  01  ERROR-SWITCHES.                                              EL131
00163      12  ERROR-SWITCH            PIC X.                           EL131
00164          88  SCREEN-ERROR                    VALUE 'X'.           EL131
00165      12  MSTR-KEY-SWITCH         PIC X.                           EL131
00166          88  MSTR-KEY-CHANGED                VALUE 'X'.           EL131
00167      12  CERT-KEY-SWITCH         PIC X.                           EL131
00168 *********SET TO X IF ALTERNATE INDEXES WILL CHANGE                EL131
00169          88  CERT-ALT-KEY-CHANGED            VALUE 'X'.              CL**8
00170 *********SET TO Y IF PRIMARY KEY WILL CHANGE                      EL131
00171          88  CERT-KEY-CHANGED                VALUE 'Y'.              CL**8
00172      12  MSTR-SWITCH             PIC X.                           EL131
00173          88  MSTR-UPDATES                    VALUE 'X'.           EL131
           12  crtt-switch             pic x value spaces.
               88  crtt-update             value 'X'.
00174      12  CERT-SWITCH             PIC X.                           EL131
00175          88  CERT-UPDATES                    VALUE 'X'.           EL131
00176      12  UPDATE-SWITCH           PIC X.                           EL131
00177          88  UPDATES-PRESENT                 VALUE 'X'.           EL131
00178      12  TRLR-SWITCH             PIC X.                           EL131
00179          88  TRLR-UPDATE-REQUIRED            VALUE 'X'.           EL131
00180          88  UPDATE-MADE                     VALUE 'Y'.           EL131
00181      12  NINETY-TRLR-SWITCH      PIC X.                              CL**8
00182          88  NINETY-TRLR-UPDATED             VALUE 'X'.              CL**8
052113     12  NINETY5-TRLR-SWITCH      PIC X  VALUE ' '.
052113         88  NINETY5-TRLR-UPDATED             VALUE 'X'.
00183      12  EARNINGS-CALC-SWITCH    PIC X.                           EL131
00184          88  TEX-REG                         VALUE '4'.           EL131
00185          88  NET-PAY                         VALUE '5'.           EL131
CIDMOD     12  DLYACTV-SW              PIC X       VALUE 'N'.                000
CIDMOD         88  DLYACTV-RECORD-NEEDED           VALUE 'Y'.                000
00186      EJECT                                                        EL131
00187  01  EDIT-WORK-AREA.                                              EL131
00188      12  COUNT-2                 PIC 9.                           EL131
00189      12  CALL-PGM                PIC X(8).                        EL131
00190      12  TRANS-ID                PIC X(4).                        EL131
00191      12  CHECK-PFKEYS            PIC 99.                          EL131
00192      12  CHECK-MAINT             PIC X.                           EL131
00193          88  ADD-ALPHA-RECORD                VALUE 'A'.              CL*26
00194          88  CHANGE-CLAIM                    VALUE 'C'.              CL*26
00195          88  DELETE-CLAIM                    VALUE 'D'.           EL131
00196          88  CHANGE-NAME                     VALUE 'N'.              CL*26
00197          88  VALID-OPTIONS                   VALUE 'A' 'C'           CL*26
00198                                                    'D' 'N'.          CL*26
00199                                                                      CL*26
00200      12  HOLD-BENEFIT            PIC XX.                             CL**8
00201      12  HOLD-FREQ               PIC XX.                          EL131
052113     12  HOLD-REPORTED           PIC XX  VALUE LOW-VALUES.
052113     12  HOLD-INCUR              PIC XX  VALUE LOW-VALUES.
00204      12  HOLD-PDTHRU             PIC XX.                          EL131
00205      12  HOLD-ADDON              PIC XX.                             CL*14
00206      12  HOLD-NODAYS             PIC 9(4).                        EL131
00207      12  HOLD-NOPMTS             PIC 9(3).                        EL131
00208      12  HOLD-PDAMT              PIC 9(7)V99    COMP-3.           EL131
00209      12  HOLD-LOANBAL            PIC 9(7)V99    COMP-3  VALUE 0.     CL*14
00210      12  HOLD-APR                PIC 9(3)V9(4)  COMP-3  VALUE 0.     CL*14
00211      12  HOLD-BIRTH              PIC XX.                          EL131
00212      12  HOLD-END                PIC XX.                          EL131
00213      12  HOLD-EFF                PIC XX.                          EL131
00214      12  HOLD-ENTRY              PIC XX.                          EL131
00215      12  HOLD-LF-CV-BEN          PIC S9(9)V99   COMP-3  VALUE +0.    CL*14
00216      12  HOLD-LF-RATE            PIC S9(4)V99   COMP-3  VALUE +0.    CL*14
00217      12  HOLD-AH-CV-BEN          PIC S9(9)V99   COMP-3  VALUE +0.    CL*14
00218      12  HOLD-AH-RATE            PIC S9(4)V99   COMP-3  VALUE +0.    CL*14
00219      12  HOLD-LF-CV-CAN          PIC XX.                             CL**8
00220      12  HOLD-AH-CV-CAN          PIC XX.                             CL**8
00221      12  DIVIDE-QUOT             PIC 9(3).                        EL131
00222      12  DIVIDE-REM              PIC 9(3).                        EL131
00223                                                                      CL*26
00224      12  WS-ALPHA-DATE.                                              CL*26
00225          16  WS-ALPHA-YEAR.                                          CL*26
00226              20  WS-ALPHA-YY-1   PIC 99.                             CL*34
00227              20  WS-ALPHA-YY-2   PIC 99.                             CL*34
00228          16  WS-ALPHA-MM         PIC 99.                             CL*34
00229          16  WS-ALPHA-DD         PIC 99.                             CL*34
00230                                                                      CL*26
00231      12  BUILD-VALID-DATE.                                        EL131
00232          16  BUILD-MONTH         PIC 99.                          EL131
00233          16  BUILD-DAY           PIC 99.                          EL131
00234          16  BUILD-YEAR          PIC 99.                          EL131
00235      12  DEEDIT-MMYY-INPUT.                                       EL131
00236          16  FILLER              PIC X.                           EL131
00237          16  DEEDIT-MONTH        PIC XX.                          EL131
00238          16  DEEDIT-YEAR         PIC XX.                          EL131
00239      12  DEEDIT-BEN              PIC 9(9)V99.                     EL131
00240      12  DEEDIT-DATE-INPUT.                                       EL131
00241          16  FILLER              PIC XX.                          EL131
00242          16  DEEDIT-DATE         PIC X(6).                        EL131
00243      12  WORK-DATE-MDY.                                           EL131
00244          16  MONTH-WORK          PIC XX.                          EL131
00245          16  FILLER              PIC X(4).                        EL131
00246          16  YEAR-WORK           PIC XX.                          EL131
00247      12  WORK-DATE-MY.                                            EL131
00248          16  WORK-MONTH          PIC XX.                          EL131
00249          16  WORK-SLASH          PIC X       VALUE '/'.              CL*32
00250          16  WORK-YEAR           PIC XX.                          EL131
00251      12  HOLD-DATE.                                               EL131
00252          16  HOLD-MONTH          PIC 99.                          EL131
00253          16  FILLER              PIC X(4).                        EL131
00254          16  HOLD-YEAR           PIC 99.                          EL131
00255      12  SPLIT-INFO-LINE-1.                                          CL*34
00256          16  SPLIT-INFO-DESC     PIC X(15).                          CL*34
00257          16  FILLER              PIC X(5)    VALUE ' OLD='.          CL*34
00258          16  SPLIT-INFO-OLD      PIC X(40).                          CL*34
00259      12  EFF-TERM-MO             PIC 9(4).                        EL131
00260      12  CURR-MO                 PIC 9(4).                        EL131
00261      12  WS-BIN-CURRENT-DT       PIC XX.                          EL131
00262      12  WS-CALC-METHOD          PIC X.                              CL*14
00263      12  WS-INITIALS.                                             EL131
00264          16  WS-INIT-1           PIC X.                           EL131
00265          16  WS-INIT-2           PIC X.                           EL131
00266      12  WS-REIN-TABLE.                                              CL*14
00267          16  WS-REIN-1           PIC X.                              CL*14
00268          16  WS-REIN-2           PIC X.                              CL*14
00269          16  WS-REIN-3           PIC X.                              CL*14
00270      12  PI-KEY.                                                  EL131
00271          16  PI-TERM             PIC X(4).                        EL131
00272          16  PI-QUAL             PIC X(4).                        EL131
00273      12  CHECK-KEY               PIC X(20).                       EL131
00274      12  SAVE-ACCT-KEY           PIC X(20).                       EL131
00275      12  SKIP-ATTRIBUTE          PIC X      VALUE SPACES.         EL131
071508     12  WS-SOC-SEC-NUMBER.
071508         16  WS-SOC-SEC-NO       PIC 9(9).
071508         16  WS-SOC-SEC-BLANK    PIC X(2).
071508     12  WS-SOC-SEC-REDEF REDEFINES WS-SOC-SEC-NUMBER.
071508         16  WS-SSN-1-3          PIC 9(3).
071508         16  WS-SSN-DASH1        PIC X(1).
071508         16  WS-SSN-4-5          PIC 9(2).
071508         16  WS-SSN-DASH2        PIC X(1).
071508         16  WS-SSN-6-9          PIC 9(4).
060413     12  AUTO-PAY-TO-BENE        PIC X     VALUE 'N'.
082013     12  WS-RETURNED-FROM        PIC X(8)  VALUE SPACES.

082013
082013 01  WS-SAVE-FIELDS.
082013     12  WS-SAVE-BENEFICIARY     PIC X(10) VALUE LOW-VALUES.
082013     12  WS-SAVE-MAINTI          PIC X(1) VALUE LOW-VALUES.
082013     12  WS-SAVE-MAINTL          PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-STATUSI         PIC X(6) VALUE LOW-VALUES.
082013     12  WS-SAVE-STATUSL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-REPI            PIC X(8) VALUE LOW-VALUES.
082013     12  WS-SAVE-REPL            PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-CAUSEI          PIC X(6) VALUE LOW-VALUES.
082013     12  WS-SAVE-CAUSEL          PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-ENDI            PIC X(8) VALUE LOW-VALUES.
082013     12  WS-SAVE-ENDL            PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-DIAGI           PIC X(60) VALUE LOW-VALUES.
082013     12  WS-SAVE-DIAGL           PIC S9(4) COMP VALUE +0.  
040814     12  WS-SAVE-ICD1I           PIC X(8)  VALUE LOW-VALUES.
040814     12  WS-SAVE-ICD1L           PIC S9(4) COMP VALUE +0.
040814     12  WS-SAVE-ICD2I           PIC X(8)  VALUE LOW-VALUES.
040814     12  WS-SAVE-ICD2L           PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-BENEI           PIC X(10) VALUE LOW-VALUES.
082013     12  WS-SAVE-BENEL           PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-BIRTHI          PIC X(8) VALUE LOW-VALUES.
082013     12  WS-SAVE-BIRTHL          PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-SOCIALI         PIC X(11) VALUE LOW-VALUES.
082013     12  WS-SAVE-SOCIALL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-SEXI            PIC X(1) VALUE LOW-VALUES.
082013     12  WS-SAVE-SEXL            PIC S9(4) COMP VALUE +0.   
082013     12  WS-SAVE-MLNAMEI         PIC X(15) VALUE LOW-VALUES.
082013     12  WS-SAVE-MLNAMEL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-MFNAMEI         PIC X(12) VALUE LOW-VALUES.
082013     12  WS-SAVE-MFNAMEL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-MMINITI         PIC X(1) VALUE LOW-VALUES.
082013     12  WS-SAVE-MMINITL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-LOANNOI         PIC X(8) VALUE LOW-VALUES.
082013     12  WS-SAVE-LOANNOL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-LOANBALI        PIC 9(7)V99 VALUE ZEROS.
082013     12  WS-SAVE-LOANBALL        PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-PROCI           PIC X(4) VALUE LOW-VALUES.
082013     12  WS-SAVE-PROCL           PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-SUPVI           PIC X(1) VALUE LOW-VALUES.
082013     12  WS-SAVE-SUPVL           PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-PRICDI          PIC X(1) VALUE LOW-VALUES.
082013     12  WS-SAVE-PRICDL          PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-FILETOI         PIC X(4) VALUE LOW-VALUES.
082013     12  WS-SAVE-FILETOL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-PDTHRUI         PIC X(8) VALUE LOW-VALUES.
082013     12  WS-SAVE-PDTHRUL         PIC S9(4) COMP VALUE +0. 
082013     12  WS-SAVE-PDAMTI          PIC 9(7)V99 VALUE ZEROS.
082013     12  WS-SAVE-PDAMTL          PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-NODAYSI         PIC 9(5) VALUE ZEROS.
082013     12  WS-SAVE-NODAYSL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-NOPMTSI         PIC 9(4) VALUE ZEROS.
082013     12  WS-SAVE-NOPMTSL         PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-FORMTYPI        PIC X(1) VALUE LOW-VALUES.
082013     12  WS-SAVE-FORMTYPL        PIC S9(4) COMP VALUE +0.
082013     12  WS-SAVE-OCCI            PIC X(6) VALUE LOW-VALUES.
082013     12  WS-SAVE-OCCL            PIC S9(4) COMP VALUE +0.
00276                                                                      CL*14
00277  01  TERM-CALCULATION-WORK-AREA     COMP-3.                          CL*14
00278      12  M                   PIC S9(7)V99           VALUE ZEROS.     CL*14
00279      12  L                   PIC S9(7)V99           VALUE ZEROS.     CL*14
00280      12  N                   PIC S9(3)              VALUE ZEROS.     CL*14
00281      12  N-STORE             PIC S9(3)              VALUE ZEROS.     CL*14
00282      12  NV-STORE            PIC S9(3)              VALUE ZEROS.     CL*14
00283      12  I                   PIC S99V9(5)           VALUE ZEROS.     CL*14
00284      12  A-N                 PIC S9(7)V9(8)         VALUE ZEROS.     CL*14
00285      12  IA-N                PIC S9(7)V9(8)         VALUE ZEROS.     CL*14
00286      12  V                   PIC S9(3)V9(14)        VALUE ZEROS.     CL*14
00287      12  R                   PIC S9(3)              VALUE ZEROS.     CL*14
00288      12  M1                  PIC S9(7)V99           VALUE ZEROS.     CL*14
00289      12  V-EX-N              PIC S9(3)V9(14)        VALUE ZEROS.     CL*14
00290      12  TERM1               PIC S9(8)V9(9)         VALUE ZEROS.     CL*14
00291      12  TERM2               PIC S9(8)V9(9)         VALUE ZEROS.     CL*14
00292      12  TERM3               PIC S9(8)V9(9)         VALUE ZEROS.     CL*14
00293      12  TERM4               PIC S9(3)V9(14)        VALUE ZEROS.     CL*14
00294      12  LEFT-TOT-1          PIC S9(9)V9(8)         VALUE ZEROS.     CL*14
00295      12  RIGHT-TOT-1         PIC S9(9)V9(8)         VALUE ZEROS.     CL*14
00296      12  RIGHT-TOT-2         PIC S9(9)V9(8)         VALUE ZEROS.     CL*14
00297                                                                      CL*14
00298  01  TERM-CALC-WORK-AREA.                                            CL*14
00299      12  WS-AH-RATE          PIC S999V9(5)          VALUE ZEROS.     CL*14
00300      12  WS-LF-RATE          PIC S999V9(5)          VALUE ZEROS.     CL*14
00301      12  WS-TERM             PIC S9(3)              VALUE ZEROS.     CL*14
00302      12  WS-TERM-REM         PIC S9(3)V99           VALUE ZEROS.     CL*14
00303      12  WS-REMAIN           PIC S99                VALUE ZEROS.     CL*14
00304      12  V-EXPONENTS.                                                CL*14
00305         14  V-EXPONENT       PIC S9(3)V9(14) COMP-3 OCCURS 250.      CL*14
00306      12  V-EX-ONETIME        PIC 9                  VALUE 1.         CL*14
00307                                                                   EL131
00308  01  TIME-IN.                                                     EL131
00309      12  UN-HOURS                PIC XX.                          EL131
00310      12  UN-MINUTES              PIC XX.                          EL131
00311      12  FILLER                  PIC XX.                          EL131
00312                                                                   EL131
00313  01  TIME-OUT.                                                    EL131
00314      12  FOR-HOURS               PIC XX.                          EL131
00315      12  FILLER                  PIC X       VALUE '.'.           EL131
00316      12  FOR-MINUTES             PIC XX.                          EL131
00317                                                                   EL131
00318  01  ERROR-NUMBERS.                                               EL131
00319      12  ER-0000                 PIC X(4)    VALUE '0000'.        EL131
00320      12  ER-0004                 PIC X(4)    VALUE '0004'.        EL131
00321      12  ER-0008                 PIC X(4)    VALUE '0008'.        EL131
00322      12  ER-0023                 PIC X(4)    VALUE '0023'.        EL131
00323      12  ER-0029                 PIC X(4)    VALUE '0029'.        EL131
00324      12  ER-0068                 PIC X(4)    VALUE '0068'.        EL131
00325      12  ER-0070                 PIC X(4)    VALUE '0070'.        EL131
00326      12  ER-0149                 PIC X(4)    VALUE '0149'.        EL131
00327      12  ER-0154                 PIC X(4)    VALUE '0154'.        EL131
00328      12  ER-0169                 PIC X(4)    VALUE '0169'.        EL131
00329      12  ER-0192                 PIC X(4)    VALUE '0192'.        EL131
00330      12  ER-0199                 PIC X(4)    VALUE '0199'.        EL131
00331      12  ER-0203                 PIC X(4)    VALUE '0203'.        EL131
00332      12  ER-0204                 PIC X(4)    VALUE '0204'.           CL**4
00333      12  ER-0205                 PIC X(4)    VALUE '0205'.        EL131
00334      12  ER-0206                 PIC X(4)    VALUE '0206'.        EL131
00335      12  ER-0208                 PIC X(4)    VALUE '0208'.        EL131
00336      12  ER-0210                 PIC X(4)    VALUE '0210'.        EL131
00337      12  ER-0215                 PIC X(4)    VALUE '0215'.        EL131
00338      12  ER-0219                 PIC X(4)    VALUE '0219'.        EL131
00339      12  ER-0220                 PIC X(4)    VALUE '0220'.        EL131
00340      12  ER-0222                 PIC X(4)    VALUE '0222'.        EL131
00341      12  ER-0223                 PIC X(4)    VALUE '0223'.        EL131
00342      12  ER-0224                 PIC X(4)    VALUE '0224'.        EL131
00343      12  ER-0227                 PIC X(4)    VALUE '0227'.        EL131
00344      12  ER-0230                 PIC X(4)    VALUE '0230'.        EL131
00345      12  ER-0232                 PIC X(4)    VALUE '0232'.        EL131
00346      12  ER-0233                 PIC X(4)    VALUE '0233'.        EL131
00347      12  ER-0236                 PIC X(4)    VALUE '0236'.        EL131
00348      12  ER-0237                 PIC X(4)    VALUE '0237'.        EL131
00349      12  ER-0238                 PIC X(4)    VALUE '0238'.        EL131
00350      12  ER-0240                 PIC X(4)    VALUE '0240'.        EL131
00351      12  ER-0241                 PIC X(4)    VALUE '0241'.        EL131
00352      12  ER-0243                 PIC X(4)    VALUE '0243'.        EL131
00353      12  ER-0244                 PIC X(4)    VALUE '0244'.        EL131
00354      12  ER-0246                 PIC X(4)    VALUE '0246'.        EL131
00355      12  ER-0247                 PIC X(4)    VALUE '0247'.        EL131
00356      12  ER-0248                 PIC X(4)    VALUE '0248'.        EL131
00357      12  ER-0250                 PIC X(4)    VALUE '0250'.        EL131
00358      12  ER-0251                 PIC X(4)    VALUE '0251'.        EL131
00359      12  ER-0253                 PIC X(4)    VALUE '0253'.        EL131
00360      12  ER-0256                 PIC X(4)    VALUE '0256'.        EL131
00361      12  ER-0257                 PIC X(4)    VALUE '0257'.        EL131
00362      12  ER-0258                 PIC X(4)    VALUE '0258'.        EL131
00363      12  ER-0259                 PIC X(4)    VALUE '0259'.        EL131
00364      12  ER-0260                 PIC X(4)    VALUE '0260'.        EL131
00365      12  ER-0261                 PIC X(4)    VALUE '0261'.        EL131
00366      12  ER-0262                 PIC X(4)    VALUE '0262'.        EL131
00367      12  ER-0263                 PIC X(4)    VALUE '0263'.        EL131
00368      12  ER-0273                 PIC X(4)    VALUE '0273'.        EL131
00369      12  ER-0274                 PIC X(4)    VALUE '0274'.        EL131
00370      12  ER-0276                 PIC X(4)    VALUE '0276'.        EL131
00371      12  ER-0282                 PIC X(4)    VALUE '0282'.        EL131
00372      12  ER-0283                 PIC X(4)    VALUE '0283'.        EL131
00373      12  ER-0309                 PIC X(4)    VALUE '0309'.        EL131
00374      12  ER-0310                 PIC X(4)    VALUE '0310'.        EL131
00375      12  ER-0311                 PIC X(4)    VALUE '0311'.        EL131
00376      12  ER-0333                 PIC X(4)    VALUE '0333'.        EL131
00377      12  ER-0360                 PIC X(4)    VALUE '0360'.        EL131
00378      12  ER-0377                 PIC X(4)    VALUE '0377'.        EL131
00379      12  ER-0402                 PIC X(4)    VALUE '0402'.        EL131
00380      12  ER-0416                 PIC X(4)    VALUE '0416'.        EL131
00381      12  ER-0426                 PIC X(4)    VALUE '0426'.        EL131
00382      12  ER-0427                 PIC X(4)    VALUE '0427'.        EL131
00383      12  ER-0428                 PIC X(4)    VALUE '0428'.        EL131
00384      12  ER-0429                 PIC X(4)    VALUE '0429'.        EL131
00385      12  ER-0430                 PIC X(4)    VALUE '0430'.        EL131
052113     12  ER-0458                 PIC X(4)    VALUE '0458'.        EL131
00386      12  ER-0475                 PIC X(4)    VALUE '0475'.        EL131
00387      12  ER-0491                 PIC X(4)    VALUE '0491'.        EL131
00388      12  ER-0509                 PIC X(4)    VALUE '0509'.        EL131
00389      12  ER-0510                 PIC X(4)    VALUE '0510'.        EL131
052113     12  ER-0511                 PIC X(4)    VALUE '0511'.
052113     12  ER-0512                 PIC X(4)    VALUE '0512'.
00390      12  ER-0519                 PIC X(4)    VALUE '0519'.        EL131
00391      12  ER-0520                 PIC X(4)    VALUE '0520'.        EL131
00392      12  ER-0521                 PIC X(4)    VALUE '0521'.        EL131
00393      12  ER-0547                 PIC X(4)    VALUE '0547'.        EL131
00394      12  ER-0548                 PIC X(4)    VALUE '0548'.        EL131
00395      12  ER-0549                 PIC X(4)    VALUE '0549'.        EL131
00396      12  ER-0565                 PIC X(4)    VALUE '0565'.        EL131
00397      12  ER-0598                 PIC X(4)    VALUE '0598'.        EL131
00398      12  ER-0639                 PIC X(4)    VALUE '0639'.        EL131
00399      12  ER-0797                 PIC X(4)    VALUE '0797'.           CL*26
00400      12  ER-0802                 PIC X(4)    VALUE '0802'.           CL*26
00401      12  ER-0803                 PIC X(4)    VALUE '0803'.           CL*26
00402      12  ER-0849                 PIC X(4)    VALUE '0849'.           CL*36
00403      12  ER-0885                 PIC X(4)    VALUE '0885'.           CL*37
071508     12  ER-0887                 PIC X(4)    VALUE '0887'.
00404      12  ER-0919                 PIC X(4)    VALUE '0919'.           CL*34
00405      12  ER-0921                 PIC X(4)    VALUE '0921'.           CL*34
00406      12  ER-0938                 PIC X(4)    VALUE '0938'.           CL*34
00407      12  ER-0946                 PIC X(4)    VALUE '0946'.           CL*34
00408      12  ER-0947                 PIC X(4)    VALUE '0947'.           CL*34
00409      12  ER-0948                 PIC X(4)    VALUE '0948'.           CL*34
00410      12  ER-0949                 PIC X(4)    VALUE '0949'.           CL*34
00411      12  ER-0950                 PIC X(4)    VALUE '0950'.           CL*34
00412      12  ER-0951                 PIC X(4)    VALUE '0951'.           CL*34
00413      12  ER-0954                 PIC X(4)    VALUE '0954'.           CL*34
00414      12  ER-0974                 PIC X(4)    VALUE '0974'.           CL*43
00415      12  ER-0975                 PIC X(4)    VALUE '0975'.           CL*43
040814     12  ER-0992                 PIC X(4)    VALUE '0992'.
031715     12  er-1581                 pic x(4)    value '1581'.
052113     12  ER-1651                 PIC X(4)    VALUE '1651'.
052113     12  ER-1652                 PIC X(4)    VALUE '1652'.
052113     12  ER-1653                 PIC X(4)    VALUE '1653'.
052113     12  ER-1654                 PIC X(4)    VALUE '1654'.
052113     12  ER-1655                 PIC X(4)    VALUE '1655'.
052113     12  er-1656                 pic x(4)    value '1656'.
052113     12  ER-1661                 PIC X(4)    VALUE '1661'.
052113     12  ER-1662                 PIC X(4)    VALUE '1662'.
080613     12  ER-1663                 PIC X(4)    VALUE '1663'.
           12  er-1664                 pic x(4)    value '1664'.
           12  er-1665                 pic x(4)    value '1665'.
           12  er-1666                 pic x(4)    value '1666'.
           12  er-1668                 pic x(4)    value '1668'.
           12  er-1669                 pic x(4)    value '1669'.
021418     12  er-1675                 pic x(4)    value '1675'.
           12  er-1676                 pic x(4)    value '1676'.
           12  er-1677                 pic x(4)    value '1677'.
081817     12  ER-1678                 PIC X(4)    VALUE '1678'.
101917     12  ER-1679                 PIC X(4)    VALUE '1679'.
090821     12  er-1682                 pic x(4)    value '1682'.
090821     12  er-1683                 pic x(4)    value '1683'.
031815     12  ER-1772                 PIC X(4)    VALUE '1772'.
060413     12  ER-1773                 PIC X(4)    VALUE '1773'.
081817     12  ER-1778                 PIC X(4)    VALUE '1778'.
00416      12  ER-2280                 PIC X(4)    VALUE '2280'.           CL*14
00417      12  ER-2878                 PIC X(4)    VALUE '2878'.           CL*36
052918     12  ER-7572                 PIC X(4)    VALUE '7572'.
00418      12  ER-7641                 PIC X(4)    VALUE '7641'.           CL*14
00419      12  ER-7642                 PIC X(4)    VALUE '7642'.           CL*14
00420      12  ER-7650                 PIC X(4)    VALUE '7650'.           CL*14
00421      12  ER-7651                 PIC X(4)    VALUE '7651'.           CL*14
00422      12  ER-7687                 PIC X(4)    VALUE '7687'.           CL**8
00423      12  ER-7689                 PIC X(4)    VALUE '7689'.           CL**8
00424      12  ER-7690                 PIC X(4)    VALUE '7690'.           CL**8
00425      12  ER-7691                 PIC X(4)    VALUE '7691'.           CL**8
00426      12  ER-8003                 PIC X(4)    VALUE '8003'.           CL*36
00426      12  ER-8051                 PIC X(4)    VALUE '8051'.           CL*36
00427      12  ER-8052                 PIC X(4)    VALUE '8052'.           CL*36
00428      12  ER-8053                 PIC X(4)    VALUE '8053'.           CL*36
00429      12  ER-8054                 PIC X(4)    VALUE '8054'.           CL*36
00430      12  ER-8055                 PIC X(4)    VALUE '8055'.           CL*36
00431      12  ER-8056                 PIC X(4)    VALUE '8056'.           CL*36
00432      12  ER-8057                 PIC X(4)    VALUE '8057'.           CL*36
00433      12  ER-8058                 PIC X(4)    VALUE '8058'.           CL*36
00434      12  ER-8059                 PIC X(4)    VALUE '8059'.           CL*36
00435      12  ER-8060                 PIC X(4)    VALUE '8060'.           CL*36
00436      12  ER-8061                 PIC X(4)    VALUE '8061'.           CL*36
00437      12  ER-8062                 PIC X(4)    VALUE '8062'.           CL*36
00438      12  ER-8063                 PIC X(4)    VALUE '8063'.           CL*36
00439      12  ER-8064                 PIC X(4)    VALUE '8064'.           CL*36
00440      12  ER-8065                 PIC X(4)    VALUE '8065'.           CL*36
00441      12  ER-8066                 PIC X(4)    VALUE '8066'.           CL*36
00442      12  ER-8152                 PIC X(4)    VALUE '8152'.           CL*40
00443      12  ER-8153                 PIC X(4)    VALUE '8153'.           CL*40
00444      12  ER-8154                 PIC X(4)    VALUE '8154'.           CL*42
00445      12  ER-8155                 PIC X(4)    VALUE '8155'.           CL*42

052113 01  ERPDEF-KEY-SAVE             PIC X(18).
052113 01  ERPDEF-KEY.
052113     12  ERPDEF-COMPANY-CD       PIC X.
052113     12  ERPDEF-STATE            PIC XX.
052113     12  ERPDEF-PROD-CD          PIC XXX.
052113     12  F                       PIC X(7).
052113     12  ERPDEF-BEN-TYPE         PIC X.
052113     12  ERPDEF-BEN-CODE         PIC XX.
052113     12  ERPDEF-EXP-DT           PIC XX.

00447  01  MSTR-KEY.                                                    EL131
00448      12  COMPANY-CODE            PIC X.                           EL131
00449      12  CARRIER-CODE            PIC X.                           EL131
00450      12  CLAIM-NO                PIC X(7).                        EL131
00451      12  CERT-NO.                                                 EL131
00452          16  CERT-NO-PRIME       PIC X(10).                       EL131
00453          16  CERT-NO-SUFX        PIC X.                           EL131
00454                                                                      CL**8
00455  01  WS-SAVE-CLAIM-KEY.                                              CL**8
00456      12  WS-SAVE-COMPANY-CD      PIC X.                              CL**8
00457      12  WS-SAVE-CARRIER         PIC X.                              CL**8
00458      12  WS-SAVE-CLAIM-NO        PIC X(7).                           CL**8
00459      12  WS-SAVE-CERT-NO.                                            CL**8
00460          16  WS-CERT-NO-PRIME    PIC X(10).                          CL**8
00461          16  WS-CERT-NO-SUFX     PIC X.                              CL**8
00462                                                                   EL131
00463  01  CERT-KEY.                                                    EL131
00464      12  CERT-COMPANY-CODE       PIC X.                           EL131
00465      12  CERT-CARRIER            PIC X.                           EL131
00466      12  CERT-GROUP              PIC X(6).                        EL131
00467      12  CERT-STATE              PIC XX.                          EL131
00468      12  CERT-ACCOUNT            PIC X(10).                       EL131
00469      12  CERT-DATE               PIC XX.                          EL131
00470      12  CERT-CERT.                                               EL131
00471          16  CERT-CERT-PRIME     PIC X(10).                       EL131
00472          16  CERT-CERT-SUFX      PIC X.                           EL131
00473                                                                   EL131
052113 01  ELCRTT-KEY.                                              
052113     05  CTRLR-COMP-CD       PIC X.                               
052113     05  CTRLR-CARRIER       PIC X.                               
052113     05  CTRLR-GROUPING      PIC X(6).                            
052113     05  CTRLR-STATE         PIC X(2).                            
052113     05  CTRLR-ACCOUNT       PIC X(10).
052113     05  CTRLR-EFF-DT        PIC XX.                              
052113     05  CTRLR-CERT-NO       PIC X(11).  
052113     05  CTRLR-REC-TYPE      PIC X.

00474  01  NOTE-KEY.                                                       CL*34
00475      12  NOTE-COMP-CD            PIC X.                              CL*34
00476      12  NOTE-CERT-KEY.                                              CL*34
00477          16  NOTE-CARRIER        PIC X.                              CL*34
00478          16  NOTE-GROUP          PIC X(6).                           CL*34
00479          16  NOTE-STATE          PIC XX.                             CL*34
00480          16  NOTE-ACCOUNT        PIC X(10).                          CL*34
00481          16  NOTE-DATE           PIC XX.                             CL*34
00482          16  NOTE-CERT-NO        PIC X(11).                          CL*34
00483                                                                      CL*34
00484  01  TRLR-KEY.                                                    EL131
00485      12  TRLR-MAIN-KEY.                                              CL*26
00486          16  TRLR-COMPANY-CD     PIC X.                              CL*34
00487          16  TRLR-CARRIER        PIC X.                              CL*34
00488          16  TRLR-CLAIM-NO       PIC X(07).                          CL*26
00489          16  TRLR-CERT-NO        PIC X(11).                          CL*26
00490      12  TRLR-SEQ-NO             PIC S9(4)   COMP.                EL131
00491                                                                   EL131
00492  01  BENEFIT-KEY.                                                 EL131
00493      12  BEN-CO-ID               PIC X(3).                        EL131
00494      12  BEN-REC-TYPE            PIC X.                           EL131
00495      12  FILLER                  PIC XX.                          EL131
00496      12  BEN-ACC-CD              PIC XX.                          EL131
00497      12  BEN-SEQ-NO              PIC S9(4)   COMP.                EL131
00498                                                                   EL131
00499  01  CNTL-KEY.                                                    EL131
00500      12  CNTL-CO-ID              PIC X(3).                        EL131
00501      12  CNTL-REC-TYPE           PIC X.                           EL131
00502      12  CNTL-PROC-ID            PIC X(4).                        EL131
00503      12  CNTL-STATE-ACCESS REDEFINES CNTL-PROC-ID.                EL131
00504          16  CNTL-STATE-NUMBER   PIC XX.                          EL131
00505          16  FILLER              PIC XX.                          EL131
00506      12  CNTL-CARRIER-ACCESS REDEFINES CNTL-PROC-ID.              EL131
00507          16  FILLER              PIC X(3).                        EL131
00508          16  CNTL-CARRIER        PIC X.                           EL131
090821     12  CNTL-bene-ACCESS REDEFINES CNTL-PROC-ID.
090821         16  filler              PIC XX.                          EL131
090821         16  cntl-benefit        PIC XX.                          EL131
00509      12  CNTL-SEQ-NO             PIC S9(4)   COMP.                EL131
00510                                                                   EL131
00511  01  ACTQ-KEY                    PIC X(20).                       EL131
00512                                                                   EL131
00513  01  WS-LAST-TRLR-KEY            PIC X(22)  VALUE SPACES.         EL131
00514                                                                   EL131
00515  01  CHKQ-KEY.                                                    EL131
00516      12  CHKQ-COMPANY-CODE       PIC X.                           EL131
00517      12  CHKQ-CONTROL-NO         PIC S9(8)  COMP.                 EL131
00518      12  CHKQ-SEQ-NO             PIC S9(4)  COMP.                 EL131
00519                                                                   EL131
00520  01  ARCH-KEY.                                                       CL*10
00521      12  ARCH-COMPANY-CODE       PIC X.                              CL*10
00522      12  ARCH-ARCHIVE-NO         PIC S9(8)  COMP.                    CL*10
00523      12  ARCH-RECORD-TYPE        PIC X.                              CL*10
00524      12  ARCH-SEQ-NO             PIC S9(4)  COMP.                    CL*10
00525                                                                      CL*10
00526  01  ACCT-KEY.                                                    EL131
00527      05  ACCT-PARTIAL-KEY.                                        EL131
00528          12  ACCT-COMPANY-CODE   PIC X.                           EL131
00529          12  ACCT-CARRIER        PIC X.                           EL131
00530          12  ACCT-GROUP          PIC X(6).                        EL131
00531          12  ACCT-STATE          PIC XX.                          EL131
00532          12  ACCT-ACCOUNT        PIC X(10).                       EL131
00533      05  ACCT-DATE               PIC XX.                          EL131
00534                                                                   EL131
00535  01  WS-ELBENE-KEY.                                                  CL*34
00536      12  WS-ELBENE-COMPANY-CD    PIC X.                              CL*34
00537      12  WS-ELBENE-RECORD-TYPE   PIC X.                              CL*34
00538      12  WS-ELBENE-ID            PIC X(10).                          CL*34
00539                                                                   EL131
00540  01  ELALPH-KEY.                                                     CL*26
00541      12  ELALPH-COMPANY-CD       PIC X.                              CL*34
00542      12  ELALPH-SOURCE           PIC X.                              CL*34
00543      12  ELALPH-NAME.                                                CL*26
00544          16  ELALPH-LAST-NAME    PIC X(15).                          CL*26
00545          16  ELALPH-FIRST-NAME.                                      CL*26
00546              20  ELALPH-FIRST-INIT   PIC X.                          CL*34
00547              20  FILLER              PIC X(11).                      CL*26
00548          16  ELALPH-MIDDLE-INIT  PIC X.                              CL*34
00549      12  ELALPH-DATE             PIC X(08).                          CL*26
00550      12  ELALPH-TIME             PIC S9(04)    COMP.                 CL*26
00551                                                                      CL*26
00552  01  COMP-LENGTHS.                                                EL131
00553      12  CNTL-GENERIC-LENGTH     PIC S9(4)   COMP VALUE +8.       EL131
00554      12  JOURNAL-LENGTH          PIC S9(4)   COMP VALUE +0.       EL131
00555      12  DATE-LENGTH             PIC S9(4)   COMP VALUE +8.       EL131
00556      12  MO-YR-LENGTH            PIC S9(4)   COMP VALUE +5.       EL131
00557      12  TERM-LENGTH             PIC S9(4)   COMP VALUE +3.       EL131
00558      12  BEN-LENGTH              PIC S9(4)   COMP VALUE +11.      EL131
00559      12  FREQ-LENGTH             PIC S9(4)   COMP VALUE +2.       EL131
00560      12  MSTR-LENGTH             PIC S9(4)   COMP VALUE +350.        CL*26
00561      12  CERT-LENGTH             PIC S9(4)   COMP VALUE +450.     EL131
00562      12  TRLR-LENGTH             PIC S9(4)   COMP VALUE +200.     EL131
00563      12  ACTQ-LENGTH             PIC S9(4)   COMP VALUE +60.      EL131
00564      12  CHKQ-LENGTH             PIC S9(4)   COMP VALUE +100.        CL**8
00565      12  ARCH-LENGTH             PIC S9(4)   COMP VALUE +90.         CL*10
00566      12  ALPH-LENGTH             PIC S9(4)   COMP VALUE +128.        CL*26
00567      EJECT                                                           CL*26
00568                                  COPY ELCLNKLT.                      CL*26
00569      EJECT                                                        EL131
00570                                  COPY ELCINTF.                       CL*22
00571      12  EL131-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.          EL131
00572          16  PI-NO-PMTS                    PIC 9(4).                 CL**8
00573          16  PI-CERT-SWITCH                PIC X.                    CL**8
00574              88  CREATED-CERT              VALUE '2'.                CL**8
00575          16  PI-PREM-TYPE                  PIC X.                    CL**8
00576          16  PI-SAVE-CERT-NO.                                     EL131
00577              20  PI-SAVE-CERT-NO-PRIME     PIC X(10).             EL131
00578              20  PI-SAVE-CERT-NO-SUFX      PIC X.                 EL131
00579          16  FILLER                        PIC X(46).                CL**8
00580          16  PI-CLAIM-DELETED-SWITCH       PIC X(1).                 CL**8
00581          16  PI-PAYMENT-AMT                PIC S9(7)V99   COMP-3.    CL*14
00582          16  PI-REC-FOUND-SW               PIC X.                    CL*34
00583          16  PI-LETTER-SW                  PIC X.                    CL*34
00584          16  PI-PREV-TRLR-KEY              PIC X(22).
052113         16  pi-claim-type                 pic x.
060413         16  PI-AUTO-PAY-SEQ               PIC S9(4)      COMP.
080613         16  pi-approval-level             pic x.
080613         16  FILLER                        PIC X(543).               CL*32
00586                                                                      CL*34
00587  01  SAVE-RECORD                 PIC X(450).                         CL*34
00588                                                                   EL131
00589      EJECT                                                        EL131
00590                                  COPY ELCLOGOF SUPPRESS.             CL*22
00591                                                                   EL131
00592                                  COPY ELCATTR SUPPRESS.              CL*22
00593                                                                   EL131
00594                                  COPY ELCAID SUPPRESS.               CL*22
00595  01  FILLER REDEFINES DFHAID.                                     EL131
00596      12  FILLER                  PIC X(8).                        EL131
00597      12  AID-KEYS OCCURS 24 TIMES.                                EL131
00598          16  FILLER              PIC X.                           EL131
00599                                                                   EL131
00600                                  COPY EL131S.                        CL*27
00601                                                                   EL131
00602                                  COPY ELCEMIB SUPPRESS.              CL*26
00603                                                                   EL131
00604                                  COPY ELCJPFX SUPPRESS.              CL*22
00605                                  PIC X(750).                         CL*27
00606                                                                   EL131
00607                                  COPY ELCCALC.
00608                                                                   EL131
00609                                  COPY ELCDATE.
00610                                                                   EL131
00611                                  COPY ELCDMO.                        CL*34
052113                                 COPY ERCPDEF.
052113                                 COPY ELCCRTT.
00612                                                                   EL131
00613  LINKAGE SECTION.                                                 EL131
00614  01  DFHCOMMAREA                 PIC X(1024).                     EL131
00615                                                                   EL131
00616                                  COPY ELCMSTR.                       CL*39
00617                                                                   EL131
00618                                  COPY ELCCERT.                       CL*39
00619                                                                   EL131
00620                                  COPY ELCTRLR.                       CL*39
00621                                                                   EL131
00622                                  COPY ELCCNTL.                       CL*39
00623                                                                   EL131
00624                                  COPY ELCACTQ.                       CL*22
00625                                                                   EL131
00626                                  COPY ERCACCT.                       CL*39
00627                                                                   EL131
00628                                  COPY ELCCHKQ.                       CL*22
00629                                                                      CL*10
00630                                  COPY ELCARCH.                       CL*39
00631                                                                   EL131
00632                                  COPY ELCBENE.                       CL*39
00633                                                                      CL*26
CIDMOD                                 COPY ELCDAR.                          000
00633                                                                      CL*26
00634                                  COPY ELCALPH.                       CL*39
00635                                                                   EL131
00636                                  COPY ERCDMDNT.                      CL*34
00637                                                                      CL*34
00638      EJECT                                                        EL131
00639  PROCEDURE DIVISION.                                              EL131
00640                                                                   EL131
00641      IF EIBCALEN = ZERO                                           EL131
00642          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL131
00643                                                                   EL131
00644      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL131
00645      MOVE '5'                    TO DC-OPTION-CODE.               EL131
00646      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.                    EL131
00647      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                      CL**8
00648      MOVE DC-GREG-DATE-1-YMD     TO  SAVE-DATE-YMD.                  CL*34
00649      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.                  CL**8
00650                                                                      CL*34
00651      IF SAVE-DATE-YY GREATER 70                                      CL*34
00652          MOVE 19                 TO SAVE-DATE-CC                     CL*34
00653       ELSE                                                           CL*34
00654          MOVE 20                 TO SAVE-DATE-CC.                    CL*34
00655                                                                   EL131
00656      EXEC CICS HANDLE CONDITION                                   EL131
00657          PGMIDERR (8820-XCTL-ERROR)                               EL131
00658          NOTFND   (8150-ENTERED-CLAIM-NOTFOUND)                      CL**4
00659          ERROR    (9990-ABEND)                                    EL131
00660      END-EXEC.                                                       CL*34
00661                                                                   EL131
00662      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.         CL**8
00663      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.         EL131
00664      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.           EL131
00665                                                                   EL131
00666      INITIALIZE    EDIT-WORK-AREA.                                   CL*32
00667      MOVE '/'                    TO WORK-SLASH.                      CL*32
00668      MOVE SPACES                 TO ERROR-SWITCHES.                  CL**8
00669                                                                   EL131
00670      MOVE TRAN-ID                TO TRANS-ID.                        CL**8
00671      MOVE EIBTRMID               TO PI-TERM.                         CL**8
00672      MOVE MAP-ID                 TO PI-QUAL.                         CL**8
00673                                                                   EL131
00674      MOVE 2                      TO EMI-NUMBER-OF-LINES.             CL**8
00675                                                                   EL131
00676      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL131
00677          MOVE LOW-VALUES         TO EL131AO                          CL**8
00678          MOVE ER-0008            TO EMI-ERROR                     EL131
00679          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL131
00680          MOVE -1                 TO MAINTL                           CL**8
00681          GO TO 8110-SEND-DATA.                                    EL131
00682                                                                   EL131
00683      IF THIS-PGM NOT = PI-CALLING-PROGRAM                         EL131
00684          MOVE LOW-VALUES         TO EL131AO                          CL**8
082013         MOVE PI-CALLING-PROGRAM TO WS-RETURNED-FROM
00685          PERFORM 0100-UPDATE-PI THRU 0120-UPDATE-PI-EXIT          EL131
00686          MOVE 'X'                TO PI-RETURN-CD-1                   CL**8
00687          PERFORM 5000-BUILD-MAP THRU 5000-EXIT                       CL*34
00688          MOVE -1                 TO MAINTL                           CL**8
082013         IF WS-RETURNED-FROM = XCTL-EL114
082013             PERFORM 4200-LOAD-CHANGES THRU 4200-EXIT
082013         END-IF
00689          GO TO 8100-SEND-MAP.                                     EL131
00690                                                                   EL131
00691      IF EIBAID = DFHCLEAR                                         EL131
00692          GO TO 8200-RETURN-PRIOR.                                 EL131
00693                                                                   EL131
00694      IF PI-PROCESSOR-ID = 'LGXX'                                  EL131
00695          NEXT SENTENCE                                            EL131
00696      ELSE                                                         EL131
00697          EXEC CICS READQ TS                                       EL131
00698              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL131
00699              INTO    (SECURITY-CONTROL)                           EL131
00700              LENGTH  (SC-COMM-LENGTH)                             EL131
00701              ITEM    (SC-ITEM)                                    EL131
00702          END-EXEC                                                 EL131
00703          MOVE SC-CLAIMS-DISPLAY (5)    TO  PI-DISPLAY-CAP         EL131
00704          MOVE SC-CLAIMS-UPDATE  (5)    TO  PI-MODIFY-CAP          EL131
00705          IF NOT DISPLAY-CAP                                       EL131
00706              MOVE 'READ'               TO  SM-READ                EL131
00707              PERFORM 9995-SECURITY-VIOLATION                      EL131
00708              MOVE ER-0070              TO  EMI-ERROR              EL131
00709              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL131
00710              GO TO 8100-SEND-MAP.                                 EL131
00711                                                                   EL131
00712      EXEC CICS RECEIVE                                            EL131
00713          MAP    ('EL131A')                                        EL131
00714          MAPSET ('EL131S')                                        EL131
00715      END-EXEC.                                                       CL**8
00716                                                                   EL131
00717      IF PFKEYL GREATER THAN ZERO                                  EL131
00718          PERFORM 0200-TRANS-PF THRU 0210-EXIT.                       CL*34
00719                                                                   EL131
00720      IF SCREEN-ERROR                                              EL131
00721          MOVE ER-0004 TO EMI-ERROR                                EL131
00722          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL131
00723          MOVE -1 TO MAINTL                                        EL131
00724          GO TO 8110-SEND-DATA.                                    EL131
00725                                                                   EL131
00726      IF EIBAID = DFHPF3                                           EL131
00727          GO TO 8500-TRLR-MNT.                                     EL131
00728                                                                   EL131
00729      IF EIBAID = DFHPF4                                           EL131
00730          GO TO 8600-ADDR-MNT.                                     EL131
00731                                                                      CL**8
00732      IF EIBAID = DFHPF5                                              CL**8
00733          GO TO 8700-CERT-MNT.                                        CL**8
00734                                                                   EL131
00735      IF PI-COMPANY-ID = 'DMD'                                        CL*37
00736        IF EIBAID = DFHPF6                                            CL*37
00737          GO TO 8750-DMD-CLM-FIX.                                     CL*39
00738                                                                      CL*34
00739      IF EIBAID = DFHPF7                                           EL131
00740          GO TO 4000-FORCE-ERRORS.                                 EL131
00741                                                                   EL131
00742      IF EIBAID = DFHPF12                                          EL131
00743          GO TO 8300-GET-HELP.                                     EL131
082013
082013     IF EIBAID = DFHPF15
082013         GO TO 8725-BENEFICIARY-MNT
082013     END-IF.
00744                                                                   EL131
00745      IF EIBAID = DFHPF23                                          EL131
00746          GO TO 8810-PF23-ENTERED.                                 EL131
00747                                                                   EL131
00748      IF EIBAID = DFHPF24                                          EL131
00749          GO TO 8400-RETURN-MASTER.                                EL131
00750                                                                   EL131
00751      IF EIBAID NOT = DFHENTER                                     EL131
00752          MOVE ER-0029            TO EMI-ERROR                     EL131
00753          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
00754          MOVE -1                 TO MAINTL                        EL131
00755          GO TO 8110-SEND-DATA.                                    EL131
00756                                                                   EL131
00757      IF PI-RETURN-CD-1 =  'X'                                     EL131
00758          MOVE ER-0311            TO EMI-ERROR                     EL131
00759          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
00760          MOVE -1                 TO MAINTL                        EL131
00761          GO TO 8110-SEND-DATA.                                    EL131
00762                                                                   EL131
00763      IF MAINTI = 'A'                                                 CL*34
00764          GO TO 0500-ADD-ALPHA-ONLY.                                  CL*26
00765                                                                      CL*26
00766      IF MAINTI = 'N'                                                 CL*34
00767          GO TO 0600-CHANGE-NAME.                                     CL*26
00768                                                                      CL*26
00769      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.                     EL131

021418     PERFORM 1030-EDIT-NAME THRU 1030-EXIT.
00770                                                                   EL131
00771      IF SCREEN-ERROR                                              EL131
00772          GO TO 8110-SEND-DATA.                                    EL131
00773                                                                   EL131
00774      IF NOT UPDATES-PRESENT AND NOT DELETE-CLAIM                  EL131
00775          MOVE ER-0276            TO EMI-ERROR                     EL131
00776          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
00777          MOVE -1                 TO MAINTL                        EL131
00778          GO TO 8110-SEND-DATA.                                    EL131
00779                                                                   EL131
00780      PERFORM 2000-UPDATE-CLAIM THRU 2000-EXIT.                    EL131
00781                                                                   EL131
00782      IF SCREEN-ERROR                                              EL131
00783          GO TO 8110-SEND-DATA.                                    EL131
00784                                                                   EL131
00785      MOVE 'W'                    TO EMI-ACTION-SWITCH.            EL131
00786                                                                   EL131
00787      PERFORM 5000-BUILD-MAP THRU 5000-EXIT.                          CL*34
00788                                                                   EL131
00789      IF EMI-FORCABLE-CTR GREATER THAN ZERO                        EL131
00790          GO TO 8110-SEND-DATA.                                    EL131
00791                                                                   EL131
00792      IF EMI-FATAL-CTR GREATER THAN ZERO                           EL131
00793          GO TO 8110-SEND-DATA.                                    EL131
00794                                                                   EL131
00795      MOVE SPACE                  TO EMI-ACTION-SWITCH.            EL131
00796                                                                      CL*26
00797      IF WS-OPEN-CLOSE-SW = 'Y'                                       CL*34
00798          IF PI-LETTER-SW = 'Y'                                       CL*34
00799              MOVE PI-COMPANY-ID  TO  CNTL-CO-ID                      CL*26
00800              MOVE 'T'            TO  CNTL-REC-TYPE                   CL*26
00801              MOVE SPACES         TO  CNTL-PROC-ID                    CL*26
00802              MOVE +0             TO  CNTL-SEQ-NO                     CL*26
00803              EXEC CICS READ                                          CL*26
00804                  DATASET   (CNTL-FILE-ID)                            CL*26
00805                  RIDFLD    (CNTL-KEY)                                CL*26
00806                  SET       (ADDRESS OF CONTROL-FILE)                 CL*32
00807              END-EXEC                                                CL*26
00808              PERFORM 7850-AUTO-LETTER-WRITER THRU 7850-EXIT.         CL*26
00809                                                                   EL131
00810      MOVE ER-0000                TO EMI-ERROR.                    EL131
00811      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
00812                                                                   EL131
00813      MOVE SPACE                  TO MAINTO.                       EL131
00814      MOVE -1                     TO MAINTL.                       EL131
00815      GO TO 8100-SEND-MAP.                                         EL131
00816                                                                   EL131
00817      EJECT                                                        EL131
00818  0100-UPDATE-PI.                                                  EL131
082013
082013     MOVE LOW-VALUES             TO WS-SAVE-BENEFICIARY.
082013     IF WS-RETURNED-FROM = XCTL-EL114
082013***sometimes there is crap in positions 7-10 when no code was selected
082013        IF PI-PROGRAM-WORK-AREA (1:5) > SPACES
082013          MOVE PI-PROGRAM-WORK-AREA (1:10) TO WS-SAVE-BENEFICIARY
082013        END-IF
082013     END-IF.
082013
00819      IF PI-RETURN-TO-PROGRAM = THIS-PGM                           EL131
00820          GO TO 0110-UPDATE-UP.                                    EL131
00821                                                                   EL131
00822      MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6.           EL131
00823      MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5.           EL131
00824      MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4.           EL131
00825      MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3.           EL131
00826      MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2.           EL131
00827      MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1.           EL131
00828      MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM.         EL131
00829      MOVE THIS-PGM               TO PI-CALLING-PROGRAM.           EL131
00830      GO TO 0120-UPDATE-PI-EXIT.                                   EL131
00831                                                                   EL131
00832  0110-UPDATE-UP.                                                  EL131
00833      MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM.             EL131
00834      MOVE PI-SAVED-PROGRAM-1 TO PI-RETURN-TO-PROGRAM.             EL131
00835      MOVE PI-SAVED-PROGRAM-2 TO PI-SAVED-PROGRAM-1.               EL131
00836      MOVE PI-SAVED-PROGRAM-3 TO PI-SAVED-PROGRAM-2.               EL131
00837      MOVE PI-SAVED-PROGRAM-4 TO PI-SAVED-PROGRAM-3.               EL131
00838      MOVE PI-SAVED-PROGRAM-5 TO PI-SAVED-PROGRAM-4.               EL131
00839      MOVE PI-SAVED-PROGRAM-6 TO PI-SAVED-PROGRAM-5.               EL131
00840      MOVE SPACES             TO PI-SAVED-PROGRAM-6.                  CL*34
00841                                                                   EL131
00842      EXEC CICS HANDLE CONDITION                                   EL131
00843           QIDERR    (0130-TS-ERROR)                               EL131
00844           ITEMERR   (0130-TS-ERROR)                               EL131
00845      END-EXEC.                                                    EL131
00846                                                                   EL131
082013     IF WS-RETURNED-FROM = XCTL-EL114
082013        EXEC CICS READQ TS
082013             QUEUE    (PI-KEY)
082013             INTO     (EL131AI)
082013             LENGTH   (MAP-LENGTH)
082013        END-EXEC
082013
082013        IF WS-SAVE-BENEFICIARY > SPACES
082013            MOVE WS-SAVE-BENEFICIARY TO BENEI
082013            MOVE +10            TO BENEL
082013            MOVE LOW-VALUES TO WS-SAVE-BENEFICIARY
082013        END-IF
082013        PERFORM 4100-SAVE-CHANGES THRU 4100-EXIT
082013     END-IF.
082013
00847      EXEC CICS READQ TS                                           EL131
00848           QUEUE    (PI-KEY)                                       EL131
00849           INTO     (PROGRAM-INTERFACE-BLOCK)                      EL131
00850           LENGTH   (PI-COMM-LENGTH)                               EL131
00851      END-EXEC.                                                    EL131
00852                                                                   EL131
00853      EXEC CICS DELETEQ TS                                         EL131
00854           QUEUE    (PI-KEY)                                       EL131
00855      END-EXEC.                                                    EL131
00856                                                                   EL131
00857  0120-UPDATE-PI-EXIT.                                             EL131
00858      EXIT.                                                        EL131
00859                                                                   EL131
00860  0130-TS-ERROR.                                                   EL131
00861      MOVE LOW-VALUES             TO EL131AO.                      EL131
00862      MOVE ER-0192                TO EMI-ERROR.                    EL131
00863                                                                   EL131
00864      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*34
00865                                                                   EL131
00866      GO TO 8100-SEND-MAP.                                         EL131
00867                                                                   EL131
00868  0200-TRANS-PF.                                                   EL131
00869      IF EIBAID NOT = DFHENTER                                     EL131
00870          MOVE 'X'                TO ERROR-SWITCH                  EL131
00871          GO TO 0210-EXIT.                                            CL*34
00872                                                                   EL131
00873      IF PFKEYI NOT NUMERIC                                        EL131
00874          MOVE 'X'                TO ERROR-SWITCH                  EL131
00875          GO TO 0210-EXIT.                                            CL*34
00876                                                                   EL131
00877      MOVE PFKEYI                 TO CHECK-PFKEYS.                 EL131
00878                                                                   EL131
00879      IF CHECK-PFKEYS LESS 1 OR GREATER 24                         EL131
00880          MOVE 'X' TO ERROR-SWITCH                                 EL131
00881          GO TO 0210-EXIT.                                            CL*34
00882                                                                   EL131
00883      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.                      EL131
00884                                                                   EL131
00885  0210-EXIT.                                                          CL*34
00886      EXIT.                                                        EL131
00887                                                                      CL**8
00888      EJECT                                                        EL131
00889  0500-ADD-ALPHA-ONLY.                                                CL*26
00890 ******************************************************************   CL*26
00891 *    ADD NAME TO ALPHA FILE.  NO CHANGES ARE MADE TO THE CLAIM   *   CL*26
00892 *    RECORD.                                                     *   CL*26
00893 ******************************************************************   CL*26
00894                                                                      CL*26
00895      IF MLNAMEL = +0 AND                                             CL*34
00896         MFNAMEL = +0 AND                                             CL*34
00897         MMINITL = +0                                                 CL*34
00898          MOVE ER-0797                TO  EMI-ERROR                   CL*26
00899          MOVE -1                     TO  MAINTL                      CL*26
00900          MOVE AL-UABON               TO  MAINTA                      CL*26
00901          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*26
00902          GO TO 8110-SEND-DATA.                                       CL*26
00903                                                                      CL*26
00904      MOVE PI-COMPANY-CD              TO  COMPANY-CODE.               CL*26
00905      MOVE PI-CARRIER                 TO  CARRIER-CODE.               CL*26
00906      MOVE PI-CLAIM-NO                TO  CLAIM-NO.                   CL*26
00907      MOVE PI-CERT-NO                 TO  CERT-NO.                    CL*26
00908                                                                      CL*26
00909      EXEC CICS READ                                                  CL*26
00910          DATASET   (CLMS-FILE-ID)                                    CL*26
00911          RIDFLD    (MSTR-KEY)                                        CL*26
00912          SET       (ADDRESS OF CLAIM-MASTER)                         CL*32
00913      END-EXEC.                                                       CL*26
00914                                                                      CL*26
00915      EXEC CICS GETMAIN                                               CL*26
00916          SET       (ADDRESS OF ALPHA-INDEX)                          CL*32
00917          LENGTH    (ALPH-LENGTH)                                     CL*26
00918          INITIMG   (GETMAIN-SPACE)                                   CL*26
00919      END-EXEC.                                                       CL*26
00920                                                                      CL*26
00921      MOVE 'AI'                       TO  AI-RECORD-ID.               CL*26
00922      MOVE CL-COMPANY-CD              TO  AI-COMPANY-CD               CL*26
00923                                          AI-CL-COMPANY-CD.           CL*26
00924      MOVE 'C'                        TO  AI-SOURCE                   CL*26
00925                                          AI-CL-SOURCE.               CL*26
00926                                                                      CL*26
00927      IF MLNAMEL IS GREATER THAN +0                                   CL*26
00928          MOVE MLNAMEI                TO  AI-LAST-NAME                CL*26
00929      ELSE                                                            CL*26
00930          MOVE CL-INSURED-LAST-NAME   TO  AI-LAST-NAME.               CL*26
00931                                                                      CL*26
00932      IF MFNAMEL IS GREATER THAN +0                                   CL*26
00933          MOVE MFNAMEI                TO  AI-FIRST-NAME               CL*26
00934      ELSE                                                            CL*26
00935          MOVE CL-INSURED-1ST-NAME    TO  AI-FIRST-NAME.              CL*26
00936                                                                      CL*26
00937      IF MMINITL IS GREATER THAN +0                                   CL*26
00938          MOVE MMINITI                TO  AI-MIDDLE-INIT              CL*26
00939      ELSE                                                            CL*26
00940          MOVE CL-INSURED-MID-INIT    TO  AI-MIDDLE-INIT.             CL*26
00941                                                                      CL*26
00942      MOVE EIBDATE                    TO  DC-JULIAN-YYDDD.            CL*26
00943      MOVE '5'                        TO  DC-OPTION-CODE.             CL*26
00944      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.                       CL*26
00945      MOVE DC-GREG-DATE-1-MDY         TO  BUILD-VALID-DATE.           CL*26
00946      IF BUILD-YEAR IS GREATER THAN 50                                CL*26
00947          MOVE '19'                   TO  WS-ALPHA-YY-1               CL*26
00948          MOVE BUILD-YEAR             TO  WS-ALPHA-YY-2               CL*26
00949          MOVE BUILD-MONTH            TO  WS-ALPHA-MM                 CL*26
00950          MOVE BUILD-DAY              TO  WS-ALPHA-DD                 CL*26
00951      ELSE                                                            CL*26
00952          MOVE '20'                   TO  WS-ALPHA-YY-1               CL*26
00953          MOVE BUILD-YEAR             TO  WS-ALPHA-YY-2               CL*26
00954          MOVE BUILD-MONTH            TO  WS-ALPHA-MM                 CL*26
00955          MOVE BUILD-DAY              TO  WS-ALPHA-DD.                CL*26
00956                                                                      CL*26
00957      MOVE WS-ALPHA-DATE              TO  AI-DATE                     CL*26
00958                                          AI-CL-DATE.                 CL*26
00959                                                                      CL*26
00960      MOVE EIBTIME                    TO  AI-TIME                     CL*26
00961                                          AI-CL-TIME.                 CL*26
00962                                                                      CL*26
00963      MOVE CL-COMPANY-CD              TO  AI-CL-COMPANY-CD.           CL*26
00964      MOVE 'C'                        TO  AI-CL-SOURCE.               CL*26
00965      MOVE CL-CARRIER                 TO  AI-CL-CARRIER.              CL*26
00966      MOVE CL-CLAIM-NO                TO  AI-CL-CLAIM-NUMBER.         CL*26
00967      MOVE CL-CERT-NO                 TO  AI-CL-CERTIFICATE-NUMBER.   CL*26
00968      MOVE CL-INCURRED-DT             TO  AI-CL-INCURRED-DATE.        CL*26
00969      MOVE LOW-VALUES                 TO  AI-CL-CLOSE-DATE.           CL*26
00970                                                                      CL*26
00971      MOVE PI-PROCESSOR-ID            TO  AI-LAST-MAINT-BY.           CL*26
00972      MOVE SAVE-BIN-DATE              TO  AI-LAST-MAINT-DT.           CL*26
00973      MOVE EIBTIME                    TO  AI-LAST-MAINT-HHMMSS.       CL*26
00974                                                                      CL*26
00975      EXEC CICS WRITE                                                 CL*26
00976          DATASET   (ALPH-FILE-ID)                                    CL*26
00977          RIDFLD    (AI-CONTROL-PRIMARY)                              CL*26
00978          FROM      (ALPHA-INDEX)                                     CL*26
00979      END-EXEC.                                                       CL*26
00980                                                                      CL*26
00981      MOVE LOW-VALUES                 TO  EL131AO.                    CL*26
00982      PERFORM 5000-BUILD-MAP THRU 5000-EXIT.                          CL*34
00983      MOVE ER-0000                    TO  EMI-ERROR.                  CL*26
00984      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*26
00985      MOVE -1                         TO  MAINTL.                     CL*26
00986      GO TO 8100-SEND-MAP.                                            CL*26
00987                                                                      CL*26
00988      EJECT                                                           CL*26
00989  0600-CHANGE-NAME.                                                   CL*26
00990 ******************************************************************   CL*26
00991 *    MOVE NAME IN THE CLAIM RECORD TO THE ALPHA FILE AND REPLACE *   CL*26
00992 *    IT WITH THE NAME THAT IS KEYED ON THE SCREEN.               *   CL*26
00993 ******************************************************************   CL*26
00994                                                                      CL*26
00995      IF MLNAMEL = +0 AND                                             CL*34
00996         MFNAMEL = +0 AND                                             CL*34
00997         MMINITL = +0                                                 CL*34
00998          MOVE ER-0276                TO  EMI-ERROR                   CL*26
00999          MOVE -1                     TO  MAINTL                      CL*26
01000          MOVE AL-UABON               TO  MAINTA                      CL*26
01001          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*26
01002          GO TO 8110-SEND-DATA.                                       CL*26
01003                                                                      CL*26
01004      MOVE PI-COMPANY-CD              TO  COMPANY-CODE.               CL*26
01005      MOVE PI-CARRIER                 TO  CARRIER-CODE.               CL*26
01006      MOVE PI-CLAIM-NO                TO  CLAIM-NO.                   CL*26
01007      MOVE PI-CERT-NO                 TO  CERT-NO.                    CL*26
01008                                                                      CL*26
01009      EXEC CICS READ                                                  CL*26
01010          DATASET   (CLMS-FILE-ID)                                    CL*26
01011          RIDFLD    (MSTR-KEY)                                        CL*26
01012          SET       (ADDRESS OF CLAIM-MASTER)                         CL*32
01013      END-EXEC.                                                       CL*26
01014                                                                      CL*26
01015      EXEC CICS GETMAIN                                               CL*26
01016          SET       (ADDRESS OF ALPHA-INDEX)                          CL*32
01017          LENGTH    (ALPH-LENGTH)                                     CL*26
01018          INITIMG   (GETMAIN-SPACE)                                   CL*26
01019      END-EXEC.                                                       CL*26
01020                                                                      CL*26
021418     MOVE CL-COMPANY-CD          TO CERT-COMPANY-CODE
021418     MOVE CL-CERT-CARRIER        TO CERT-CARRIER
021418     MOVE CL-CERT-GROUPING       TO CERT-GROUP
021418     MOVE CL-CERT-STATE          TO CERT-STATE
021418     MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT
021418     MOVE CL-CERT-EFF-DT         TO CERT-DATE
021418     MOVE CL-CERT-NO             TO CERT-CERT
021418
021418     EXEC CICS READ
021418        DATASET   (CERT-FILE-ID)
021418        RIDFLD    (CERT-KEY)
021418        SET       (ADDRESS OF CERTIFICATE-MASTER)
021418        RESP      (WS-RESPONSE)
021418     END-EXEC
021418
021418     IF INSTYPEL > ZEROS
021418        MOVE INSTYPEI TO CL-INSURED-TYPE
021418     END-IF
021418
021418     IF MFNAMEL > +0                                                 CL*34
021418        IF CL-INSURED-TYPE = 'C'
021418          AND (CM-INSURED-FIRST-NAME = MFNAMEI(1:10))
021418           MOVE ER-1675             TO EMI-ERROR
021418           MOVE -1                  TO MFNAMEL
021418           MOVE AL-UABON            TO MFNAMEA
021418           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
021418           GO TO 8110-SEND-DATA
021418        ELSE
021418          IF CL-INSURED-TYPE = 'P'
021418            AND (CM-INSURED-FIRST-NAME <> MFNAMEI(1:10))
021418             MOVE ER-1676             TO EMI-ERROR
021418             MOVE -1                  TO MFNAMEL
021418             MOVE AL-UABON            TO MFNAMEA
021418             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
021418             GO TO 8110-SEND-DATA
021418          END-IF
021418        END-IF
021418     END-IF

01021      MOVE 'AI'                       TO  AI-RECORD-ID.               CL*26
01022      MOVE CL-COMPANY-CD              TO  AI-COMPANY-CD               CL*26
01023                                          AI-CL-COMPANY-CD.           CL*26
01024      MOVE 'C'                        TO  AI-SOURCE                   CL*26
01025                                          AI-CL-SOURCE.               CL*26
01026                                                                      CL*26
01027      MOVE CL-INSURED-LAST-NAME       TO  AI-LAST-NAME.               CL*26
01028      MOVE CL-INSURED-1ST-NAME        TO  AI-FIRST-NAME.              CL*26
01029      MOVE CL-INSURED-MID-INIT        TO  AI-MIDDLE-INIT.             CL*26
01030                                                                      CL*26
01031      MOVE EIBDATE                    TO  DC-JULIAN-YYDDD.            CL*26
01032      MOVE '5'                        TO  DC-OPTION-CODE.             CL*26
01033      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.                       CL*26
01034      MOVE DC-GREG-DATE-1-MDY         TO  BUILD-VALID-DATE.           CL*26
01035      IF BUILD-YEAR IS GREATER THAN 50                                CL*26
01036          MOVE '19'                   TO  WS-ALPHA-YY-1               CL*26
01037          MOVE BUILD-YEAR             TO  WS-ALPHA-YY-2               CL*26
01038          MOVE BUILD-MONTH            TO  WS-ALPHA-MM                 CL*26
01039          MOVE BUILD-DAY              TO  WS-ALPHA-DD                 CL*26
01040      ELSE                                                            CL*26
01041          MOVE '20'                   TO  WS-ALPHA-YY-1               CL*26
01042          MOVE BUILD-YEAR             TO  WS-ALPHA-YY-2               CL*26
01043          MOVE BUILD-MONTH            TO  WS-ALPHA-MM                 CL*26
01044          MOVE BUILD-DAY              TO  WS-ALPHA-DD.                CL*26
01045                                                                      CL*26
01046      MOVE WS-ALPHA-DATE              TO  AI-DATE                     CL*26
01047                                          AI-CL-DATE.                 CL*26
01048                                                                      CL*26
01049      MOVE EIBTIME                    TO  AI-TIME                     CL*26
01050                                          AI-CL-TIME.                 CL*26
01051                                                                      CL*26
01052      MOVE CL-COMPANY-CD              TO  AI-CL-COMPANY-CD.           CL*26
01053      MOVE 'C'                        TO  AI-CL-SOURCE.               CL*26
01054      MOVE CL-CARRIER                 TO  AI-CL-CARRIER.              CL*26
01055      MOVE CL-CLAIM-NO                TO  AI-CL-CLAIM-NUMBER.         CL*26
01056      MOVE CL-CERT-NO                 TO  AI-CL-CERTIFICATE-NUMBER.   CL*26
01057      MOVE CL-INCURRED-DT             TO  AI-CL-INCURRED-DATE.        CL*26
01058      MOVE LOW-VALUES                 TO  AI-CL-CLOSE-DATE.           CL*26
01059                                                                      CL*26
01060      MOVE PI-PROCESSOR-ID            TO  AI-LAST-MAINT-BY.           CL*26
01061      MOVE SAVE-BIN-DATE              TO  AI-LAST-MAINT-DT.           CL*26
01062      MOVE EIBTIME                    TO  AI-LAST-MAINT-HHMMSS.       CL*26
01063                                                                      CL*26
062121     IF PI-COMPANY-ID = 'CID' or 'AHL' or 'FNL'
CIDMOD         MOVE 'Y'                TO CL-YESNOSW
CIDMOD     END-IF.
02826                                                                   EL131
01064      EXEC CICS WRITE                                                 CL*26
01065          DATASET   (ALPH-FILE-ID)                                    CL*26
01066          RIDFLD    (AI-CONTROL-PRIMARY)                              CL*26
01067          FROM      (ALPHA-INDEX)                                     CL*26
01068      END-EXEC.                                                       CL*26
01069                                                                      CL*26
01070      EXEC CICS READ                                                  CL*26
01071          DATASET   (CLMS-FILE-ID)                                    CL*26
01072          RIDFLD    (MSTR-KEY)                                        CL*26
01073          SET       (ADDRESS OF CLAIM-MASTER)                         CL*32
01074          UPDATE                                                      CL*26
01075      END-EXEC.                                                       CL*26
01076                                                                      CL*26
062602     if (cl-priority-cd = '8')
062602        and (pi-processor-id not = 'PEMA'and 'JMS '
052113             AND 'AMWA' AND 'KMSB')
062602        MOVE ER-8003             TO EMI-ERROR
062602        PERFORM 9900-ERROR-FORMAT
062602                                 THRU 9900-EXIT
062602        MOVE -1                  TO MAINTL
062602        GO TO 8110-SEND-DATA
062602     end-if
062602
01077      IF MLNAMEL IS GREATER THAN +0                                   CL*26
01078          MOVE MLNAMEI                TO  CL-INSURED-LAST-NAME.       CL*26
01079                                                                      CL*26
01080      IF MFNAMEL IS GREATER THAN +0                                   CL*26
01081          MOVE MFNAMEI                TO  CL-INSURED-1ST-NAME.        CL*26
01082                                                                      CL*26
01083      IF MMINITL IS GREATER THAN +0                                   CL*26
01084          MOVE MMINITI                TO  CL-INSURED-MID-INIT.        CL*26
01085                                                                      CL*26
01086      MOVE PI-PROCESSOR-ID            TO  CL-LAST-MAINT-USER.         CL*26
01087      MOVE SAVE-BIN-DATE              TO  CL-LAST-MAINT-DT.           CL*26
01088      MOVE EIBTIME                    TO  CL-LAST-MAINT-HHMMSS.       CL*26
01089      MOVE '3'                        TO  CL-LAST-MAINT-TYPE.         CL*26
01090                                                                      CL*26
062121     IF PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL'
CIDMOD         MOVE 'Y'                TO CL-YESNOSW
CIDMOD     END-IF.
02826                                                                   EL131
01091      EXEC CICS HANDLE CONDITION                                      CL*26
01092          DUPKEY   (0600-CONTINUE-NAME-UPDATE)                        CL*26
01093      END-EXEC.                                                       CL*26
01094                                                                      CL*26
01095      EXEC CICS REWRITE                                               CL*26
01096          DATASET   (CLMS-FILE-ID)                                    CL*26
01097          FROM      (CLAIM-MASTER)                                    CL*26
01098      END-EXEC.                                                       CL*26
01099                                                                      CL*26
01100  0600-CONTINUE-NAME-UPDATE.                                          CL*26
01101                                                                      CL*26
01102      MOVE LOW-VALUES                 TO  EL131AO.                    CL*26
01103      PERFORM 5000-BUILD-MAP THRU 5000-EXIT.                          CL*34
01104      MOVE ER-0000                    TO  EMI-ERROR.                  CL*26
01105      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*26
01106      MOVE -1                         TO  MAINTL.                     CL*26
01107      GO TO 8100-SEND-MAP.                                            CL*26
01108                                                                      CL*26
01109      EJECT                                                           CL*26
01110  1000-EDIT-SCREEN.                                                EL131
01111                                                                      CL**8
01112      MOVE MAINTI                 TO CHECK-MAINT.                     CL**8
01113                                                                      CL*34
01114      IF NOT VALID-OPTIONS                                         EL131
01115          MOVE 'X'                TO ERROR-SWITCH                     CL*34
01116          MOVE -1                 TO MAINTL                           CL*34
01117          MOVE AL-UABON           TO MAINTA                           CL*34
01118          MOVE ER-0023            TO EMI-ERROR                        CL*34
01119          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
01120          GO TO 1010-EXIT.                                         EL131
01121                                                                   EL131
01122      IF NOT MODIFY-CAP                                            EL131
01123          MOVE 'UPDATE'           TO  SM-READ                         CL**8
01124          PERFORM 9995-SECURITY-VIOLATION                          EL131
01125          MOVE ER-0070               TO  EMI-ERROR                 EL131
01126          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL131
01127          GO TO 8100-SEND-MAP.                                     EL131
01128                                                                   EL131
01129      IF DELETE-CLAIM                                              EL131
01130          MOVE AL-UANON TO MAINTA                                  EL131
01131          GO TO 1010-EXIT.                                         EL131
01132                                                                   EL131
01133      MOVE AL-UANON               TO MAINTA.                       EL131
01134                                                                      CL*38
01135      IF CCNOI GREATER THAN LOW-VALUES                                CL*38
01136          MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH.                      CL*38
01137                                                                   EL131
01138      IF PI-COMPANY-ID = 'CSL'
01139         CONTINUE
01140      ELSE
01141         IF TYPEL > ZERO
062121           IF ((PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL')
012009              AND (TYPEI = PI-AH-OVERRIDE-L1 OR
100518                    PI-LIFE-OVERRIDE-L1 OR 'O'))
012009                       OR
020816              ((PI-COMPANY-ID = 'DCC' OR 'VPP')
012009              AND (TYPEI = PI-AH-OVERRIDE-L1 OR
100518                PI-LIFE-OVERRIDE-L1 OR 'I' OR 'G' OR 'F' OR 'O'
080322                                    OR 'B' OR 'H' ))
01143               MOVE AL-UANON      TO TYPEA
01144               MOVE 'X'           TO UPDATE-SWITCH MSTR-SWITCH
01145            ELSE
01146               MOVE ER-0199       TO EMI-ERROR
01147               PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
01148               MOVE AL-UABON      TO TYPEA
01149               MOVE -1            TO TYPEL
01150               MOVE 'X'           TO ERROR-SWITCH
                 END-IF
01151         ELSE
01152            MOVE AL-UANOF         TO TYPEA
              END-IF
           END-IF

01154      IF STATUSI GREATER THAN LOW-VALUES                           EL131
01155          IF STATUSI = 'OPEN' OR 'CLOSED' OR 'O' OR 'C'               CL*34
01156              MOVE 'Y'            TO WS-OPEN-CLOSE-SW                 CL*26
01157              MOVE AL-UANON       TO STATUSA                       EL131
01158              MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH TRLR-SWITCH    EL131
CIDMOD             MOVE 'Y'            TO DLYACTV-SW                         000
01159          ELSE                                                     EL131
01160              MOVE ER-0333        TO EMI-ERROR                     EL131
01161              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL131
01162              MOVE AL-UABON       TO STATUSA                       EL131
01163              MOVE -1             TO STATUSL                       EL131
01164              MOVE 'X'            TO ERROR-SWITCH                  EL131
CIDMOD         END-IF
01165      ELSE                                                         EL131
01166          MOVE SPACE              TO TRLR-SWITCH                   EL131
CIDMOD         MOVE 'N'                TO DLYACTV-SW                         000
01167          MOVE AL-UANOF           TO STATUSA.                      EL131
01168                                                                   EL131
01169      IF PROCI GREATER THAN LOW-VALUES                             EL131
01170          PERFORM 1120-EDIT-PROC THRU 1120-EXIT                       CL*34
01171      ELSE                                                         EL131
01172          MOVE AL-UANOF TO PROCA.                                  EL131
01173                                                                   EL131
01174      IF SEXI GREATER THAN LOW-VALUES                              EL131
01175          IF SEXI = 'F' OR 'M'                                     EL131
01176              MOVE AL-UANON       TO SEXA                          EL131
01177              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH     EL131
01178            ELSE                                                   EL131
01179              MOVE ER-0219        TO EMI-ERROR                     EL131
01180              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL131
01181              MOVE AL-UABON       TO SEXA                          EL131
01182              MOVE -1             TO SEXL                          EL131
01183              MOVE 'X'            TO ERROR-SWITCH                  EL131
01184        ELSE                                                       EL131
01185          MOVE AL-UANOF           TO SEXA.                         EL131
01186                                                                   EL131
01187      IF BIRTHL = +0                                                  CL*34
01188          MOVE LOW-VALUES         TO  HOLD-BIRTH                      CL*26
01189          MOVE AL-UANOF           TO  BIRTHA                          CL*26
01190          GO TO 1000-CONTINUE-EDITS.                                  CL*26
01191                                                                      CL*26
01192      IF BIRTHI = SPACES                                              CL*34
01193          MOVE LOW-VALUES         TO  HOLD-BIRTH                      CL*26
01194          MOVE 'X'                TO  UPDATE-SWITCH                   CL*26
01195                                      MSTR-SWITCH                     CL*26
01196          GO TO 1000-CONTINUE-EDITS.                                  CL*26
01197                                                                      CL*26
01198      MOVE BIRTHI                 TO  DEEDIT-DATE-INPUT.              CL*26
01199      PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT.                        CL*26
01200      MOVE DEEDIT-DATE            TO  DC-GREG-DATE-1-MDY.             CL*26
01201      MOVE '4'                    TO  DC-OPTION-CODE.                 CL*26
01202      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.                       CL*26
01203      IF DATE-CONVERSION-ERROR                                        CL*26
01204          MOVE ER-0220            TO  EMI-ERROR                       CL*26
01205          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*26
01206          MOVE AL-UABON           TO  BIRTHA                          CL*26
01207          MOVE -1                 TO  BIRTHL                          CL*26
01208          MOVE 'X'                TO  ERROR-SWITCH                    CL*26
01209          GO TO 1000-CONTINUE-EDITS.                                  CL*26
01210                                                                      CL*26
01211 ******************************************************************   CL*26
01212 **   IF CALCULATED BIRTH DATE IS GREATER THAN TODAYS DATE       **   CL*26
01213 **   USE THE CENTURY ADJUSTMENT SWITCH IN THE DATE ROUTINE      **   CL*26
01214 **   TO SUBTRACT 100 YEARS TO OBTAIN THE CORRECT BIRTH DATE.    **   CL*26
01215 ******************************************************************   CL*26
01216      IF DC-BIN-DATE-1 IS GREATER THAN SAVE-BIN-DATE                  CL*26
01217          MOVE BIRTHI             TO  DEEDIT-DATE-INPUT               CL*26
01218          PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT                     CL*26
01219          MOVE DEEDIT-DATE        TO  DC-GREG-DATE-1-MDY              CL*26
01220          MOVE '4'                TO  DC-OPTION-CODE                  CL*26
01221          MOVE '1'                TO  DC-CENTURY-ADJUSTMENT           CL*26
01222          MOVE +0                 TO  DC-ELAPSED-MONTHS               CL*26
01223                                      DC-ELAPSED-DAYS                 CL*26
01224          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*26
01225          IF DATE-CONVERSION-ERROR                                    CL*26
01226              MOVE ER-0220        TO  EMI-ERROR                       CL*26
01227              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*26
01228              MOVE AL-UABON       TO  BIRTHA                          CL*26
01229              MOVE -1             TO  BIRTHL                          CL*26
01230              MOVE 'X'            TO  ERROR-SWITCH                    CL*26
01231              GO TO 1000-CONTINUE-EDITS.                              CL*26
01232                                                                      CL*26
01233      MOVE AL-UANON               TO  BIRTHA.                         CL*26
01234      MOVE 'X'                    TO  UPDATE-SWITCH                   CL*26
01235                                      MSTR-SWITCH.                    CL*26
01236      MOVE DC-GREG-DATE-1-EDIT    TO  BIRTHO.                         CL*26
01237      MOVE DC-BIN-DATE-1          TO  HOLD-BIRTH.                     CL*26
01238      MOVE ' '                    TO  DC-CENTURY-ADJUSTMENT.          CL*26
01239                                                                      CL*26
01240  1000-CONTINUE-EDITS.                                                CL*26
01241                                                                   EL131
071508     IF SOCIALI GREATER THAN LOW-VALUES
071508         MOVE SOCIALI TO WS-SOC-SEC-NUMBER
071508         IF WS-SOC-SEC-NO NUMERIC AND 
071508           (WS-SOC-SEC-BLANK = SPACES OR LOW-VALUES)
071508             NEXT SENTENCE
071508         ELSE
071508            IF WS-SSN-1-3 NUMERIC AND WS-SSN-4-5 NUMERIC AND
071508               WS-SSN-6-9 NUMERIC AND WS-SSN-DASH1 = '-' AND
071508               WS-SSN-DASH2 = '-'
071508                 NEXT SENTENCE
071508            ELSE
071508                 MOVE ER-0887        TO  EMI-ERROR
071508                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
071508                 MOVE AL-UABON       TO  SOCIALA
071508                 MOVE -1             TO  SOCIALL
071508                 MOVE 'X'            TO  ERROR-SWITCH
071508            END-IF
071508        END-IF
071508     END-IF.

031715     if critpl > zeros
031715        and critpi numeric
031715        move al-uanon            to critpa
031715        move 'X'                 to update-switch
031715     end-if

01242      IF SOCIALI GREATER THAN LOW-VALUES                           EL131
01243          MOVE AL-UANON           TO SOCIALA                       EL131
01244          MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH MSTR-KEY-SWITCH    EL131
01245      ELSE                                                         EL131
01246          MOVE AL-UANOF           TO SOCIALA.                      EL131
01247                                                                   EL131
01248      IF OCCI GREATER THAN LOW-VALUES                              EL131
01249          MOVE AL-UANON           TO OCCA                          EL131
01250          MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH     EL131
01251      ELSE                                                         EL131
01252          MOVE AL-UANOF           TO OCCA.                         EL131
01253                                                                   EL131
01254      IF BENEL NOT GREATER THAN ZERO                               EL131
01255             GO TO 1005-CONTINUE-EDITS.                            EL131
01256                                                                   EL131
01257      IF BENEI = SPACES                                            EL131
060413        IF PI-AUTO-PAY-SEQ NOT = ZEROS
060413           MOVE 'N'             TO AUTO-PAY-TO-BENE
060413           PERFORM 2800-CHECK-AUTO-PAY THRU 2800-EXIT
060413           IF AUTO-PAY-TO-BENE = 'Y'
060413               MOVE ER-1773      TO  EMI-ERROR
060413               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
060413               MOVE 'X'          TO ERROR-SWITCH
060413               MOVE AL-UABON     TO BENEA
060413               MOVE -1           TO BENEL
060413           ELSE
060413               MOVE 'X'          TO UPDATE-SWITCH MSTR-SWITCH
060413           END-IF
060413        ELSE
031815           PERFORM 1050-EDIT-BENE THRU 1050-EXIT
031815           IF PAYMENT-PENDING
031815               MOVE ER-1772      TO  EMI-ERROR
031815               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
031815               MOVE 'X'          TO ERROR-SWITCH
031815               MOVE AL-UABON     TO BENEA
031815               MOVE -1           TO BENEL
031815           END-IF
060413           MOVE 'X'              TO UPDATE-SWITCH MSTR-SWITCH
060413        END-IF
01259         GO TO 1005-CONTINUE-EDITS.                                EL131
01260                                                                   EL131
01261      EXEC CICS HANDLE CONDITION                                   EL131
01262          NOTFND (1004-BENE-NOT-FOUND)                             EL131
01263      END-EXEC.                                                    EL131
01264                                                                   EL131
01265      MOVE PI-COMPANY-CD          TO  WS-ELBENE-COMPANY-CD.           CL*34
01266      MOVE 'B'                    TO  WS-ELBENE-RECORD-TYPE.          CL*34
01267      MOVE BENEI                  TO  WS-ELBENE-ID.                   CL*34
01268                                                                   EL131
01269      EXEC CICS READ                                               EL131
01270          DATASET ('ELBENE')                                       EL131
01271          RIDFLD  (WS-ELBENE-KEY)                                     CL*34
01272          SET     (ADDRESS OF BENEFICIARY-MASTER)                     CL*32
01273      END-EXEC.                                                    EL131
01274                                                                   EL131
01275      MOVE AL-UANON               TO  BENEA.                       EL131
01276      MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH.                       EL131
01277                                                                   EL131
01278      GO TO 1005-CONTINUE-EDITS.                                   EL131
01279                                                                   EL131
01280  1004-BENE-NOT-FOUND.                                             EL131
01281      MOVE ER-0565                TO  EMI-ERROR.                   EL131
01282                                                                   EL131
01283      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
01284      MOVE 'X'                    TO ERROR-SWITCH.                 EL131
01285      MOVE AL-UABON               TO BENEA.                        EL131
01286      MOVE -1                     TO BENEL.                        EL131
01287                                                                   EL131
01288  1005-CONTINUE-EDITS.                                             EL131
01289                                                                      CL**8
01290      IF DIAGI GREATER THAN LOW-VALUES                             EL131
01291          MOVE AL-UANON           TO DIAGA                         EL131
01292          MOVE 'X'                TO UPDATE-SWITCH                    CL**8
01293                                     NINETY-TRLR-SWITCH               CL**8
01294      ELSE                                                         EL131
01295          MOVE AL-UANOF           TO DIAGA.                        EL131
040814
040814     IF ICD1I GREATER THAN LOW-VALUES
040814         IF (ICD1I GREATER THAN SPACES) AND 
040814            (ICD1I(4:1) NOT = '.')
040814             IF ICD1I(4:1) = ' '
040814                 MOVE '.' TO ICD1I(4:1)
040814             ELSE
040814                 IF ICD1I(8:1) = ' '
040814                     MOVE ICD1I(7:1) TO ICD1I(8:1)
040814                     MOVE ICD1I(6:1) TO ICD1I(7:1)
040814                     MOVE ICD1I(5:1) TO ICD1I(6:1)
040814                     MOVE ICD1I(4:1) TO ICD1I(5:1)
040814                     MOVE '.'        TO ICD1I(4:1)
040814                 END-IF
040814             END-IF
040814         END-IF
040814         IF (ICD1I GREATER THAN SPACES) AND
040814            (ICD1I(1:1) NOT > ' ' OR
040814             ICD1I(2:1) NOT > ' ' OR 
040814             ICD1I(3:1) NOT > ' ' OR 
040814             ICD1I(4:1) NOT = '.')
040814              MOVE ER-0992        TO  EMI-ERROR
040814              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814              MOVE 'X'            TO ERROR-SWITCH
040814              MOVE AL-UABON       TO ICD1A
040814              MOVE -1             TO ICD1L
040814         ELSE
040814             MOVE AL-UANON       TO ICD1A
040814             MOVE 'X'            TO UPDATE-SWITCH
040814                                    NINETY-TRLR-SWITCH
040814         END-IF
040814     ELSE
040814         MOVE AL-UANOF           TO ICD1A
040814     END-IF.
040814
040814     IF ICD2I GREATER THAN LOW-VALUES
040814         IF (ICD2I GREATER THAN SPACES) AND 
040814            (ICD2I(4:1) NOT = '.')
040814             IF ICD2I(4:1) = ' '
040814                 MOVE '.' TO ICD2I(4:1)
040814             ELSE
040814                 IF ICD2I(8:1) = ' '
040814                     MOVE ICD2I(7:1) TO ICD2I(8:1)
040814                     MOVE ICD2I(6:1) TO ICD2I(7:1)
040814                     MOVE ICD2I(5:1) TO ICD2I(6:1)
040814                     MOVE ICD2I(4:1) TO ICD2I(5:1)
040814                     MOVE '.'        TO ICD2I(4:1)
040814                 END-IF
040814             END-IF
040814         END-IF
040814         IF (ICD2I GREATER THAN SPACES) AND 
040814            (ICD2I(1:1) NOT > ' ' OR
040814             ICD2I(2:1) NOT > ' ' OR 
040814             ICD2I(3:1) NOT > ' ' OR 
040814             ICD2I(4:1) NOT = '.')
040814              MOVE ER-0992        TO EMI-ERROR
040814              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040814              MOVE 'X'            TO ERROR-SWITCH
040814              MOVE AL-UABON       TO ICD2A
040814              MOVE -1             TO ICD2L
040814         ELSE
040814             MOVE AL-UANON       TO ICD2A
040814             MOVE 'X'            TO UPDATE-SWITCH
040814                                    NINETY-TRLR-SWITCH
040814         END-IF
040814     ELSE
040814         MOVE AL-UANOF           TO ICD2A
040814     END-IF.
01296                                                                   EL131
040814*    IF CAUSEI GREATER THAN LOW-VALUES                            EL131
040814*        MOVE AL-UANON           TO CAUSEA                        EL131
040814*        MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH     EL131
040814*    ELSE                                                         EL131
040814*        MOVE AL-UANOF           TO CAUSEA.                       EL131
01302                                                                   EL131
040814*    IF ENDL GREATER THAN ZERO                                    EL131
040814*        MOVE LOW-VALUES         TO HOLD-END                      EL131
040814*        IF ENDI = SPACES                                         EL131
040814*            MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH     EL131
040814*        ELSE                                                     EL131
040814*            MOVE ENDI           TO DEEDIT-DATE-INPUT             EL131
040814*            EXEC CICS BIF DEEDIT                                 EL131
040814*                FIELD    (DEEDIT-DATE-INPUT)                     EL131
040814*                LENGTH   (DATE-LENGTH)                           EL131
040814*            END-EXEC                                             EL131
040814*            MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY            EL131
040814*            MOVE '4'            TO DC-OPTION-CODE                EL131
040814*            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT             EL131
040814*            IF NOT DATE-CONVERSION-ERROR                         EL131
040814*                MOVE DC-BIN-DATE-1      TO HOLD-END              EL131
040814*                MOVE AL-UANON   TO ENDA                          EL131
040814*                MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH     EL131
040814*                MOVE DC-GREG-DATE-1-EDIT TO ENDO                 EL131
040814*            ELSE                                                 EL131
040814*                MOVE ER-0224    TO EMI-ERROR                     EL131
040814*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*34
040814*                MOVE AL-UABON   TO ENDA                          EL131
040814*                MOVE -1         TO ENDL                          EL131
040814*                MOVE 'X'        TO ERROR-SWITCH                  EL131
040814*      ELSE                                                       EL131
040814*          MOVE AL-UANOF         TO ENDA.                         EL131
01329                                                                   EL131
01330      IF INCL GREATER THAN ZERO
01331          MOVE LOW-VALUES         TO HOLD-INCUR                    EL131
01332          IF INCI = SPACES                                         EL131
01333              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH     EL131
01334          ELSE                                                     EL131
01335              MOVE INCI           TO DEEDIT-DATE-INPUT             EL131
01336              EXEC CICS BIF DEEDIT                                 EL131
01337                  FIELD    (DEEDIT-DATE-INPUT)                     EL131
01338                  LENGTH   (DATE-LENGTH)                           EL131
01339              END-EXEC                                             EL131
01340              MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY            EL131
01341              MOVE '4'            TO DC-OPTION-CODE                EL131
01342              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT             EL131
01343              IF NOT DATE-CONVERSION-ERROR                         EL131
01344                  MOVE DC-BIN-DATE-1      TO HOLD-INCUR            EL131
01345                  MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH     EL131
01346                  MOVE AL-UANON           TO INCA                  EL131
01347                  MOVE DC-GREG-DATE-1-EDIT TO INCO                 EL131
01348              ELSE                                                 EL131
01349                  MOVE ER-0222    TO EMI-ERROR                     EL131
01350                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*34
01351                  MOVE AL-UABON   TO INCA                          EL131
01352                  MOVE -1         TO INCL                          EL131
01353                  MOVE 'X'        TO ERROR-SWITCH
090821             end-if
090821         end-if
01354      ELSE
01355         IF PI-NO-PMTS = ZEROS
01356            MOVE AL-UANOF     TO INCA
090821        end-if
090821     end-if
01357                                                                   EL131
01358      IF REPL GREATER THAN ZERO                                    EL131
01359          MOVE LOW-VALUES         TO HOLD-REPORTED                 EL131
01360          IF REPI = SPACES                                         EL131
01361              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH     EL131
01362          ELSE                                                     EL131
01363              MOVE REPI           TO DEEDIT-DATE-INPUT             EL131
01364              EXEC CICS BIF DEEDIT                                 EL131
01365                  FIELD    (DEEDIT-DATE-INPUT)                     EL131
01366                  LENGTH   (DATE-LENGTH)                           EL131
01367              END-EXEC                                             EL131
01368              MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY            EL131
01369              MOVE '4'            TO DC-OPTION-CODE                EL131
01370              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT             EL131
01371              IF NOT DATE-CONVERSION-ERROR                         EL131
01372                  MOVE DC-BIN-DATE-1      TO HOLD-REPORTED         EL131
01373                  MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH     EL131
01374                  MOVE AL-UANON   TO REPA                          EL131
01375                  MOVE DC-GREG-DATE-1-EDIT TO REPO                 EL131
01376              ELSE                                                 EL131
01377                  MOVE ER-0223    TO EMI-ERROR                     EL131
01378                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*34
01379                  MOVE AL-UABON   TO REPA                          EL131
01380                  MOVE -1         TO REPL                          EL131
01381                  MOVE 'X'        TO ERROR-SWITCH                  EL131
01382        ELSE                                                       EL131
01383          MOVE AL-UANOF TO REPA.                                   EL131
01384                                                                   EL131
01385      IF PDTHRUI GREATER THAN LOW-VALUES                           EL131
01386         MOVE LOW-VALUES         TO HOLD-PDTHRU                       CL**8
01387         IF PDTHRUI = SPACES                                          CL**8
01388            MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH          CL**8
01389         ELSE                                                         CL**8
01390            MOVE PDTHRUI        TO DEEDIT-DATE-INPUT                  CL**8
01391            EXEC CICS BIF DEEDIT                                      CL**8
01392                 FIELD    (DEEDIT-DATE-INPUT)                         CL**8
01393                 LENGTH   (DATE-LENGTH)                               CL**8
01394            END-EXEC                                                  CL**8
01395            MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY                 CL**8
01396            MOVE '4'            TO DC-OPTION-CODE                     CL**8
01397            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                  CL**8
01398            IF NOT DATE-CONVERSION-ERROR                              CL**8
01399               MOVE DC-BIN-DATE-1      TO HOLD-PDTHRU                 CL**8
01400               MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH           CL**8
01401               MOVE AL-UANON   TO PDTHRUA                             CL**8
01402               MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO                    CL**8
01403               IF PI-USES-PAID-TO                                     CL**8
01404                  MOVE '6'            TO DC-OPTION-CODE               CL**8
01405                  MOVE -1             TO DC-ELAPSED-DAYS              CL**8
01406                  MOVE +0             TO DC-ELAPSED-MONTHS            CL**8
01407                  PERFORM 9800-CONVERT-DATE THRU 9800-EXIT            CL**8
01408                  IF NO-CONVERSION-ERROR                              CL**8
01409                     MOVE DC-BIN-DATE-2 TO HOLD-PDTHRU                CL**8
01410                  ELSE                                                CL**8
01411                     NEXT SENTENCE                                    CL**8
01412               ELSE                                                   CL**8
01413                  NEXT SENTENCE                                       CL**8
01414            ELSE                                                      CL**8
01415               MOVE ER-0475    TO EMI-ERROR                           CL**8
01416               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               CL**8
01417               MOVE AL-UABON   TO PDTHRUA                             CL**8
01418               MOVE -1         TO PDTHRUL                             CL**8
01419               MOVE 'X'        TO ERROR-SWITCH                        CL**8
01420      ELSE                                                            CL**8
01421         IF MODIFY-CAP                                                CL**8
01422            MOVE AL-UANOF        TO PDTHRUA.                          CL**8
01423                                                                   EL131
01424      IF PDAMTL GREATER THAN +0                                       CL*32
01425          MOVE ZEROS              TO HOLD-PDAMT                    EL131
01426          EXEC CICS BIF DEEDIT                                     EL131
01427              FIELD    (PDAMTI)                                    EL131
01428              LENGTH   (9)                                         EL131
01429          END-EXEC                                                 EL131
01430          IF PDAMTI NUMERIC                                        EL131
01431              MOVE PDAMTI         TO HOLD-PDAMT PDAMTO             EL131
01432              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH     EL131
01433              MOVE AL-UNNON       TO PDAMTA                        EL131
01434          ELSE                                                     EL131
01435              MOVE ER-0547        TO EMI-ERROR                     EL131
01436              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL131
01437              MOVE AL-UNBON       TO PDAMTA                        EL131
01438              MOVE -1             TO PDAMTL                        EL131
01439              MOVE 'X'            TO ERROR-SWITCH                  EL131
01440      ELSE                                                         EL131
01441          IF MODIFY-CAP                                            EL131
01442             MOVE AL-UNNOF        TO PDAMTA.                       EL131
01443                                                                   EL131
01444      IF NODAYSI GREATER THAN LOW-VALUES                           EL131
01445          MOVE ZEROS              TO HOLD-NODAYS                   EL131
01446          EXEC CICS BIF DEEDIT                                     EL131
01447              FIELD (NODAYSI)                                      EL131
01448              LENGTH (5)                                           EL131
01449          END-EXEC                                                 EL131
01450          IF NODAYSI NUMERIC                                       EL131
01451              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH     EL131
01452              MOVE AL-UNNON       TO NODAYSA                       EL131
01453              MOVE NODAYSI        TO  HOLD-NODAYS  NODAYSO         EL131
01454          ELSE                                                     EL131
01455              MOVE ER-0491        TO EMI-ERROR                     EL131
01456              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL131
01457              MOVE AL-UNBON       TO NODAYSA                       EL131
01458              MOVE -1             TO NODAYSL                       EL131
01459              MOVE 'X'            TO ERROR-SWITCH                  EL131
01460      ELSE                                                         EL131
01461          IF MODIFY-CAP                                            EL131
01462             MOVE AL-UNNOF        TO NODAYSA.                      EL131
01463                                                                   EL131
01464      IF NOPMTSI GREATER THAN LOW-VALUES                           EL131
01465          MOVE ZEROS              TO HOLD-NOPMTS                   EL131
01466          EXEC CICS BIF DEEDIT                                     EL131
01467              FIELD (NOPMTSI)                                      EL131
01468              LENGTH (4)                                           EL131
01469          END-EXEC                                                 EL131
01470          IF NOPMTSI NUMERIC                                       EL131
01471              MOVE NOPMTSI        TO HOLD-NOPMTS  NOPMTSO          EL131
01472              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH     EL131
01473              MOVE AL-UNNON       TO NOPMTSA                       EL131
01474            ELSE                                                   EL131
01475              MOVE ER-0548        TO EMI-ERROR                     EL131
01476              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL131
01477              MOVE AL-UNBON       TO NOPMTSA                       EL131
01478              MOVE -1             TO NOPMTSL                       EL131
01479              MOVE 'X'            TO ERROR-SWITCH                  EL131
01480        ELSE                                                       EL131
01481          IF MODIFY-CAP                                            EL131
01482             MOVE AL-UNNOF        TO NOPMTSA.                      EL131
01483                                                                   EL131
01484      IF FORMTYPI GREATER THAN LOW-VALUES                             CL*14
01485          IF FORMTYPI = ' ' OR 'L' OR 'S'                             CL*14
01486              MOVE AL-UANON       TO FORMTYPA                         CL*14
01487              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH        CL*14
01488            ELSE                                                      CL*14
01489              MOVE ER-7650        TO EMI-ERROR                        CL*14
01490              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*14
01491              MOVE AL-UABON       TO FORMTYPA                         CL*14
01492              MOVE -1             TO FORMTYPL                         CL*14
01493              MOVE 'X'            TO ERROR-SWITCH                     CL*14
01494        ELSE                                                          CL*14
01495          MOVE AL-UANOF           TO FORMTYPA.                        CL*14
01496                                                                      CL*14
01497      IF PRICDL GREATER THAN ZERO                                  EL131
01498          IF (PRICDI = ' ') OR                                        CL*34
01499             (PRICDI GREATER THAN ZERO   AND                          CL*22
01500              PRICDI NOT GREATER THAN '9')                            CL*22
01501              MOVE AL-UANON       TO PRICDA                        EL131
01502              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH     EL131
01503          ELSE                                                     EL131
01504              MOVE ER-0274        TO EMI-ERROR                     EL131
01505              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL131
01506              MOVE AL-UABON       TO PRICDA                        EL131
01507              MOVE -1             TO PRICDL                        EL131
01508              MOVE 'X'            TO ERROR-SWITCH                  EL131
01509      ELSE                                                         EL131
01510          MOVE AL-UANOF TO PRICDA.                                 EL131
01511                                                                      CL*34
01512      IF PI-COMPANY-ID = 'DMD'                                        CL*35
01513      IF PRICDL GREATER THAN ZERO                                     CL*34
01514        IF SYSTEM-MODIFY-CAP  OR                                      CL*34
01515           PI-PROCESSOR-ID = 'LGXX'                                   CL*34
01516              NEXT SENTENCE                                           CL*34
01517            ELSE                                                      CL*34
01518             IF CL-PRIORITY-CD = '9'  OR                              CL*34
01519                PRICDI         = '9'                                  CL*34
01520                   MOVE ER-0938        TO EMI-ERROR                   CL*34
01521                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT           CL*34
01522                   MOVE AL-UABON       TO PRICDA                      CL*34
01523                   MOVE -1             TO PRICDL                      CL*34
01524                   MOVE 'X'            TO ERROR-SWITCH.               CL*34
01525                                                                      CL**8
01526      IF FILETOL GREATER THAN ZERO                                    CL**8
01527          MOVE AL-UANON           TO FILETOA                          CL**8
01528          MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH        CL**8
01529      ELSE                                                            CL**8
01530          MOVE AL-UANOF TO FILETOA.                                   CL**8
01531                                                                   EL131
01532      IF SUPVL GREATER THAN ZERO                                   EL131
01533          IF SUPVI = 'Y' OR 'N' OR SPACE                           EL131
01534              MOVE AL-UANON       TO SUPVA                         EL131
01535              MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH     EL131
01536          ELSE                                                     EL131
01537              MOVE ER-0230        TO EMI-ERROR                     EL131
01538              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL131
01539              MOVE AL-UABON       TO SUPVA                         EL131
01540              MOVE -1             TO SUPVL                         EL131
01541              MOVE 'X'            TO ERROR-SWITCH                  EL131
01542      ELSE                                                         EL131
01543          MOVE AL-UANOF           TO SUPVA.                        EL131
01544
01545      IF MLNAMEL GREATER THAN ZERO                                 EL131
01546          IF MLNAMEI GREATER THAN SPACES                           EL131
01547             MOVE AL-UANON        TO MLNAMEA                       EL131
01548             MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH                 EL131
01549                                      MSTR-KEY-SWITCH              EL131
01550          ELSE                                                     EL131
01551             MOVE ER-0236         TO EMI-ERROR                     EL131
01552             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL131
01553             MOVE AL-UABON        TO MLNAMEA                       EL131
01554             MOVE -1              TO MLNAMEL                       EL131
01555             MOVE 'X'             TO ERROR-SWITCH                  EL131
01556      ELSE                                                         EL131
01557          MOVE AL-UANOF           TO MLNAMEA.                      EL131
01558                                                                   EL131
01559      IF MFNAMEI GREATER THAN LOW-VALUES                           EL131
01560          MOVE AL-UANON           TO MFNAMEA                       EL131
01561          MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH     EL131
01562      ELSE                                                         EL131
01563          MOVE AL-UANOF           TO MFNAMEA.                      EL131
01564                                                                   EL131
01565      IF MMINITI GREATER THAN LOW-VALUES                           EL131
01566          MOVE AL-UANON           TO MMINITA                       EL131
01567          MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH     EL131
01568      ELSE                                                         EL131
01569          MOVE AL-UANOF           TO MMINITA.                      EL131
01570                                                                   EL131
01571      IF CRTLNMEI GREATER THAN LOW-VALUES                          EL131
01572          IF CRTLNMEI NOT = SPACES                                 EL131
01573             MOVE AL-UANON        TO CRTLNMEA                      EL131
01574             MOVE 'X'             TO  UPDATE-SWITCH CERT-SWITCH    EL131
01575                                      CERT-KEY-SWITCH              EL131
01576         ELSE                                                      EL131
01577             MOVE ER-0236         TO EMI-ERROR                     EL131
01578             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL131
01579             MOVE AL-UABON        TO CRTLNMEA                      EL131
01580             MOVE -1              TO CRTLNMEL                      EL131
01581             MOVE 'X'             TO ERROR-SWITCH                  EL131
01582      ELSE                                                         EL131
01583          MOVE AL-UANOF           TO CRTLNMEA.                     EL131
01584                                                                   EL131
01585      IF CRTFNMEI GREATER THAN LOW-VALUES                          EL131
01586          MOVE AL-UANON           TO CRTFNMEA                      EL131
01587          MOVE 'X'                TO UPDATE-SWITCH CERT-SWITCH     EL131
01588      ELSE                                                         EL131
01589          MOVE AL-UANOF           TO CRTFNMEA.                     EL131
01590                                                                   EL131
01591      IF CRTINITI GREATER THAN LOW-VALUES                          EL131
01592          MOVE AL-UANON           TO CRTINITA                      EL131
01593          MOVE 'X' TO UPDATE-SWITCH CERT-SWITCH CERT-KEY-SWITCH    EL131
01594      ELSE                                                         EL131
01595          MOVE AL-UANOF           TO CRTINITA.                     EL131
01596                                                                   EL131
01597      IF ISSAGEL GREATER THAN ZERO                                 EL131
01598          IF ISSAGEI NUMERIC                                       EL131
01599             MOVE AL-UANON        TO ISSAGEA                       EL131
01600             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH     EL131
01601          ELSE                                                     EL131
01602             MOVE ER-0237         TO EMI-ERROR                     EL131
01603             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL131
01604             MOVE AL-UABON        TO ISSAGEA                       EL131
01605             MOVE -1              TO ISSAGEL                       EL131
01606             MOVE 'X'             TO ERROR-SWITCH                  EL131
01607      ELSE                                                         EL131
01608          MOVE AL-UANOF           TO ISSAGEA.                      EL131
01609                                                                   EL131
01610      IF JNTFNMEI GREATER THAN LOW-VALUES                          EL131
01611          MOVE AL-UANON           TO JNTFNMEA                      EL131
01612          MOVE 'X'                TO UPDATE-SWITCH CERT-SWITCH     EL131
01613      ELSE                                                         EL131
01614          MOVE AL-UANOF           TO JNTFNMEA.                     EL131
01615                                                                   EL131
01616      IF JNTLNMEI GREATER THAN LOW-VALUES                          EL131
01617          MOVE AL-UANON           TO JNTLNMEA                      EL131
01618          MOVE 'X'                TO UPDATE-SWITCH CERT-SWITCH     EL131
01619      ELSE                                                         EL131
01620          MOVE AL-UANOF           TO JNTLNMEA.                     EL131
01621                                                                   EL131
01622      IF JNTINITI GREATER THAN LOW-VALUES                          EL131
01623          MOVE AL-UANON           TO JNTINITA                      EL131
01624          MOVE 'X'                TO UPDATE-SWITCH CERT-SWITCH     EL131
01625      ELSE                                                         EL131
01626          MOVE AL-UANOF           TO JNTINITA.                     EL131
01627                                                                   EL131
01628      IF JNTAGEL GREATER THAN ZERO                                 EL131
01629          IF JNTAGEI NUMERIC                                       EL131
01630             MOVE AL-UANON        TO JNTAGEA                       EL131
01631             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH     EL131
01632          ELSE                                                     EL131
01633             MOVE ER-0238         TO EMI-ERROR                     EL131
01634             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL131
01635             MOVE AL-UABON        TO JNTAGEA                       EL131
01636             MOVE -1              TO JNTAGEL                       EL131
01637             MOVE 'X'             TO ERROR-SWITCH                  EL131
01638      ELSE                                                         EL131
01639          MOVE AL-UANOF           TO JNTAGEA.                      EL131
01640                                                                   EL131
01641      IF ADDONDTI GREATER THAN LOW-VALUES                             CL*14
01642          MOVE LOW-VALUES         TO HOLD-ADDON                       CL*14
01643          IF ADDONDTI = SPACES                                        CL*14
01644              MOVE 'X'  TO UPDATE-SWITCH MSTR-SWITCH CERT-SWITCH      CL*14
01645          ELSE                                                        CL*14
01646              MOVE ADDONDTI       TO DEEDIT-DATE-INPUT                CL*14
01647              EXEC CICS BIF DEEDIT                                    CL*14
01648                  FIELD    (DEEDIT-DATE-INPUT)                        CL*14
01649                  LENGTH   (DATE-LENGTH)                              CL*14
01650              END-EXEC                                                CL*14
01651              MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY               CL*14
01652              MOVE '4'            TO DC-OPTION-CODE                   CL*14
01653              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                CL*14
01654              IF NOT DATE-CONVERSION-ERROR                            CL*14
01655                  MOVE DC-BIN-DATE-1      TO HOLD-ADDON               CL*14
01656                  MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH        CL*14
01657                                                   CERT-SWITCH        CL*14
01658                  MOVE AL-UANON   TO ADDONDTA                         CL*14
01659                  MOVE DC-GREG-DATE-1-EDIT TO ADDONDTO                CL*14
01660              ELSE                                                    CL*14
01661                  MOVE ER-7651    TO EMI-ERROR                        CL*14
01662                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*34
01663                  MOVE AL-UABON   TO ADDONDTA                         CL*14
01664                  MOVE -1         TO ADDONDTL                         CL*14
01665                  MOVE 'X'        TO ERROR-SWITCH                     CL*14
01666        ELSE                                                          CL*14
01667          MOVE AL-UANOF TO ADDONDTA.                                  CL*14
01668                                                                      CL*14
01669      IF LCVCDI GREATER THAN LOW-VALUES                               CL**8
01670          MOVE LCVCDI             TO WS-EDIT-BEN-CODE                 CL**8
01671          PERFORM 1240-EDIT-LIFE-CODE THRU 1240-EXIT                  CL**8
01672      ELSE                                                            CL**8
01673          MOVE AL-UANOF           TO LCVCDA.                          CL**8
01674                                                                      CL**8
01675      IF ACVCDI GREATER THAN LOW-VALUES                               CL**8
01676          MOVE ACVCDI             TO WS-EDIT-BEN-CODE                 CL**8
01677          PERFORM 1310-EDIT-AH-CODE THRU 1310-EXIT                    CL*34
01678      ELSE                                                            CL**8
01679          MOVE AL-UANOF           TO ACVCDA.                          CL**8
01680                                                                      CL**8
01681      IF LCVOTRMI GREATER THAN LOW-VALUES                             CL**8
01682          EXEC CICS BIF DEEDIT                                     EL131
01683              FIELD (LCVOTRMI)                                        CL**8
01684              LENGTH(3)                                            EL131
01685          END-EXEC                                                 EL131
01686          IF LCVOTRMI IS NUMERIC                                      CL**8
01687              MOVE LCVOTRMI       TO LCVOTRMO                         CL*14
01688              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH     EL131
01689              MOVE AL-UANON       TO LCVOTRMA                         CL**8
01690          ELSE                                                     EL131
01691              MOVE ER-0241        TO EMI-ERROR                     EL131
01692              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL131
01693              MOVE AL-UNBON       TO LCVOTRMA                         CL**8
01694              MOVE 'X'            TO ERROR-SWITCH                  EL131
01695              MOVE -1             TO LCVOTRML                         CL**8
01696      ELSE                                                         EL131
01697          MOVE AL-UANOF           TO LCVOTRMA.                        CL**8
01698                                                                   EL131
01699      IF ACVOTRMI GREATER THAN LOW-VALUES                             CL**8
01700          EXEC CICS BIF DEEDIT                                     EL131
01701              FIELD (ACVOTRMI)                                        CL**8
01702              LENGTH(3)                                               CL**8
01703          END-EXEC                                                 EL131
01704          IF ACVOTRMI IS NUMERIC                                      CL**8
01705              MOVE ACVOTRMI       TO ACVOTRMO                         CL*14
01706              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH        CL**8
01707              MOVE AL-UANON       TO ACVOTRMA                         CL**8
01708          ELSE                                                        CL**8
01709              MOVE ER-0241        TO EMI-ERROR                        CL**8
01710              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**8
01711              MOVE AL-UNBON       TO ACVOTRMA                         CL**8
01712              MOVE 'X'            TO ERROR-SWITCH                     CL**8
01713              MOVE -1             TO ACVOTRML                         CL**8
01714      ELSE                                                            CL**8
01715          MOVE AL-UANOF           TO ACVOTRMA.                        CL**8
01716                                                                      CL*14
01717      IF LCVRATEL GREATER THAN +0                                     CL*32
01718          EXEC CICS BIF DEEDIT                                        CL*14
01719              FIELD (LCVRATEI)                                        CL*14
01720              LENGTH(6)                                               CL*14
01721          END-EXEC                                                    CL*14
01722          IF LCVRATEI IS NUMERIC                                      CL*14
01723              MOVE LCVRATEI       TO HOLD-LF-RATE  LCVRATEO           CL*14
01724              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH        CL*14
01725              MOVE AL-UNNON       TO LCVRATEA                         CL*14
01726          ELSE                                                        CL*14
01727              MOVE ER-2280        TO EMI-ERROR                        CL*14
01728              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*14
01729              MOVE AL-UNBON       TO LCVRATEA                         CL*14
01730              MOVE 'X'            TO ERROR-SWITCH                     CL*14
01731              MOVE -1             TO LCVRATEL                         CL*14
01732      ELSE                                                            CL*14
01733          MOVE AL-UNNOF           TO LCVRATEA.                        CL*14
01734                                                                      CL*14
01735      IF ACVRATEL GREATER THAN +0                                     CL*32
01736          EXEC CICS BIF DEEDIT                                        CL*14
01737              FIELD (ACVRATEI)                                        CL*14
01738              LENGTH(6)                                               CL*14
01739          END-EXEC                                                    CL*14
01740          IF ACVRATEI IS NUMERIC                                      CL*14
01741              MOVE ACVRATEI       TO HOLD-AH-RATE  ACVRATEO           CL*14
01742              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH        CL*14
01743              MOVE AL-UNNON       TO ACVRATEA                         CL*14
01744          ELSE                                                        CL*14
01745              MOVE ER-2280        TO EMI-ERROR                        CL*14
01746              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*14
01747              MOVE AL-UNBON       TO ACVRATEA                         CL*14
01748              MOVE 'X'            TO ERROR-SWITCH                     CL*14
01749              MOVE -1             TO ACVRATEL                         CL*14
01750      ELSE                                                            CL*14
01751          MOVE AL-UNNOF           TO ACVRATEA.                        CL*14
01752                                                                      CL**8
01753      IF LCVBENEL GREATER THAN ZERO                                   CL**8
01754          EXEC CICS BIF DEEDIT                                        CL**8
01755              FIELD (LCVBENEI)                                        CL**8
01756              LENGTH (11)                                             CL**8
01757          END-EXEC                                                    CL**8
01758          IF LCVBENEI IS NUMERIC                                      CL**8
01759             MOVE LCVBENEI        TO HOLD-LF-CV-BEN  LCVBENEO         CL**8
01760             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH     EL131
01761             MOVE AL-UNNON        TO LCVBENEA                         CL**8
01762          ELSE                                                     EL131
01763             MOVE ER-0243         TO EMI-ERROR                     EL131
01764             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL131
01765             MOVE AL-UNBON        TO LCVBENEA                         CL**8
01766             MOVE -1              TO LCVBENEL                         CL**8
01767             MOVE 'X'             TO ERROR-SWITCH                  EL131
01768      ELSE                                                         EL131
01769          MOVE AL-UANOF           TO LCVBENEA.                        CL**8
01770                                                                   EL131
01771      IF ACVBENEL GREATER THAN ZERO                                   CL**8
01772          EXEC CICS BIF DEEDIT                                        CL**8
01773              FIELD (ACVBENEI)                                        CL**8
01774              LENGTH (11)                                             CL**8
01775          END-EXEC                                                    CL**8
01776          IF ACVBENEI IS NUMERIC                                      CL**8
01777             MOVE ACVBENEI        TO HOLD-AH-CV-BEN  ACVBENEO         CL**8
01778             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH        CL**8
01779             MOVE AL-UNNON        TO ACVBENEA                         CL**8
01780          ELSE                                                        CL**8
01781             MOVE ER-0243         TO EMI-ERROR                        CL**8
01782             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 CL**8
01783             MOVE AL-UNBON        TO ACVBENEA                         CL**8
01784             MOVE -1              TO ACVBENEL                         CL**8
01785             MOVE 'X'             TO ERROR-SWITCH                     CL**8
01786      ELSE                                                         EL131
01787          MOVE AL-UANOF           TO ACVBENEA.                        CL**8
01788                                                                   EL131
01789      IF LCVFORMI GREATER THAN LOW-VALUES                             CL**8
01790         MOVE 'X'                 TO UPDATE-SWITCH CERT-SWITCH        CL**8
01791         MOVE AL-UANON            TO LCVFORMA                         CL**8
01792      ELSE                                                            CL**8
01793         MOVE AL-UANOF            TO LCVFORMA.                        CL**8
01794                                                                      CL**8
01795      IF ACVFORMI GREATER THAN LOW-VALUES                             CL**8
01796         MOVE 'X'                 TO UPDATE-SWITCH CERT-SWITCH        CL**8
01797         MOVE AL-UANON            TO ACVFORMA                         CL**8
01798      ELSE                                                            CL**8
01799         MOVE AL-UANOF            TO ACVFORMA.                        CL**8
01800                                                                      CL**8
01801      IF LCVCNDTL GREATER THAN ZERO                                   CL**8
01802          MOVE LOW-VALUES         TO HOLD-LF-CV-CAN                   CL**8
01803          IF LCVCNDTI = SPACES                                        CL**8
01804              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH     EL131
01805          ELSE                                                     EL131
01806              MOVE LCVCNDTI       TO DEEDIT-DATE-INPUT                CL**8
01807              PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT                 CL*34
01808              MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY            EL131
01809              MOVE '4'            TO DC-OPTION-CODE                EL131
01810              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT             EL131
01811              IF NOT DATE-CONVERSION-ERROR                         EL131
01812                  MOVE DC-BIN-DATE-1      TO HOLD-LF-CV-CAN           CL**8
01813                  MOVE AL-UANON   TO LCVCNDTA                         CL**8
01814                  MOVE 'X'        TO UPDATE-SWITCH CERT-SWITCH     EL131
01815                  MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO                CL**8
01816              ELSE                                                    CL**8
01817                  MOVE ER-0246    TO EMI-ERROR                        CL**8
01818                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*34
01819                  MOVE AL-UABON   TO LCVCNDTA                         CL**8
01820                  MOVE -1         TO LCVCNDTL                         CL**8
01821                  MOVE 'X'        TO ERROR-SWITCH                     CL**8
01822      ELSE                                                            CL**8
01823          MOVE LOW-VALUES         TO HOLD-LF-CV-CAN                   CL**8
01824          MOVE AL-UANOF           TO LCVCNDTA.                        CL**8
01825                                                                      CL**8
01826      IF ACVCNDTL GREATER THAN ZERO                                   CL**8
01827          MOVE LOW-VALUES         TO HOLD-AH-CV-CAN                   CL**8
01828          IF ACVCNDTI = SPACES                                        CL**8
01829              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH        CL**8
01830          ELSE                                                        CL**8
01831              MOVE ACVCNDTI       TO DEEDIT-DATE-INPUT                CL**8
01832              PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT                 CL*34
01833              MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY               CL**8
01834              MOVE '4'            TO DC-OPTION-CODE                   CL**8
01835              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                CL**8
01836              IF NOT DATE-CONVERSION-ERROR                            CL**8
01837                  MOVE DC-BIN-DATE-1      TO HOLD-AH-CV-CAN           CL**8
01838                  MOVE AL-UANON   TO ACVCNDTA                         CL**8
01839                  MOVE 'X'        TO UPDATE-SWITCH CERT-SWITCH        CL**8
01840                  MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO                CL**8
01841              ELSE                                                 EL131
01842                  MOVE ER-0246    TO EMI-ERROR                     EL131
01843                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*34
01844                  MOVE AL-UABON   TO ACVCNDTA                         CL**8
01845                  MOVE -1         TO ACVCNDTL                         CL**8
01846                  MOVE 'X'        TO ERROR-SWITCH                  EL131
01847      ELSE                                                         EL131
01848          MOVE LOW-VALUES         TO HOLD-AH-CV-CAN                   CL**8
01849          MOVE AL-UANOF           TO ACVCNDTA.                        CL**8
01850                                                                   EL131
01851      IF APRL GREATER THAN +0                                         CL*32
01852          EXEC CICS BIF DEEDIT                                     EL131
01853              FIELD    (APRI)                                      EL131
01854              LENGTH   (8)                                         EL131
01855          END-EXEC                                                 EL131
01856          IF APRI NUMERIC                                          EL131
01857             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH     EL131
01858             MOVE APRI            TO HOLD-APR APRO                    CL*14
01859             MOVE AL-UANON        TO APRA                          EL131
01860           ELSE                                                    EL131
01861             MOVE ER-0259         TO EMI-ERROR                     EL131
01862             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL131
01863             MOVE AL-UABON        TO APRA                          EL131
01864             MOVE -1              TO APRL                          EL131
01865             MOVE 'X'             TO ERROR-SWITCH                  EL131
01866      ELSE                                                         EL131
01867          MOVE AL-UANOF           TO APRA.                         EL131
01868                                                                   EL131
01869      IF PMTFREQL GREATER THAN ZERO                                EL131
01870          EXEC CICS BIF DEEDIT                                     EL131
01871              FIELD    (PMTFREQI)                                  EL131
01872              LENGTH   (2)                                         EL131
01873          END-EXEC                                                 EL131
01874          IF PMTFREQI NUMERIC                                      EL131
01875             MOVE PMTFREQI        TO  HOLD-FREQ PMTFREQO           EL131
01876             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH     EL131
01877             MOVE AL-UANON        TO PMTFREQA                      EL131
01878           ELSE                                                    EL131
01879             MOVE ER-0427         TO EMI-ERROR                     EL131
01880             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL131
01881             MOVE AL-UABON        TO PMTFREQA                      EL131
01882             MOVE -1              TO PMTFREQL                      EL131
01883             MOVE 'X'             TO ERROR-SWITCH                  EL131
01884      ELSE                                                         EL131
01885          MOVE AL-UANOF           TO PMTFREQA.                     EL131
01886                                                                   EL131
01887      IF INDGRPL GREATER THAN ZERO                                 EL131
01888          IF INDGRPI = 'I' OR 'G' OR SPACE                         EL131
01889              MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH     EL131
01890              MOVE AL-UANON       TO INDGRPA                       EL131
01891           ELSE                                                    EL131
01892              MOVE ER-0260        TO EMI-ERROR                     EL131
01893              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL131
01894              MOVE AL-UABON       TO INDGRPA                       EL131
01895              MOVE -1             TO INDGRPL                       EL131
01896              MOVE 'X'            TO ERROR-SWITCH                  EL131
01897      ELSE                                                         EL131
01898          MOVE AL-UANOF           TO INDGRPA.                      EL131
01899                                                                   EL131
01900      IF PREMTYPL GREATER THAN ZERO                                EL131
01901          IF PREMTYPI = '1' OR '2' OR '3'                             CL*34
01902             MOVE PREMTYPI        TO PI-PREM-TYPE                     CL*14
01903             MOVE AL-UNNON        TO PREMTYPA                      EL131
01904             MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH     EL131
01905           ELSE                                                    EL131
01906             MOVE ER-0227         TO EMI-ERROR                     EL131
01907             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL131
01908             MOVE AL-UNBON        TO PREMTYPA                      EL131
01909             MOVE -1              TO PREMTYPL                      EL131
01910             MOVE 'X'             TO ERROR-SWITCH                  EL131
01911      ELSE                                                         EL131
01912          MOVE AL-UANOF           TO PREMTYPA.                     EL131
01913                                                                   EL131
01914      IF LOANBALL GREATER THAN ZERO                                EL131
01915          EXEC CICS BIF DEEDIT                                     EL131
01916               FIELD    (LOANBALI)                                 EL131
01917               LENGTH   (9)                                           CL**8
01918          END-EXEC                                                 EL131
01919          IF LOANBALI NUMERIC                                      EL131
01920              MOVE LOANBALI       TO  HOLD-LOANBAL LOANBALO        EL131
01921              MOVE 'X' TO UPDATE-SWITCH CERT-SWITCH MSTR-SWITCH    EL131
01922              MOVE AL-UNNON TO LOANBALA                            EL131
01923          ELSE                                                     EL131
01924              MOVE ER-0639        TO EMI-ERROR                     EL131
01925              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL131
01926              MOVE AL-UNBON       TO LOANBALA                      EL131
01927              MOVE -1             TO LOANBALL                      EL131
01928              MOVE 'X'            TO ERROR-SWITCH                  EL131
01929      ELSE                                                         EL131
01930          MOVE AL-UNNOF           TO LOANBALA.                     EL131
01931                                                                   EL131
062217*    IF LOANNOL GREATER THAN ZERO                                 EL131
062217*          MOVE 'X'                 TO UPDATE-SWITCH CERT-SWITCH  EL131
062217*          MOVE AL-UANON            TO LOANNOA                    EL131
062217*       ELSE                                                      EL131
062217*          MOVE AL-UANOF            TO LOANNOA.                   EL131
01937                                                                   EL131
01938      IF REINCDI GREATER THAN LOW-VALUES                           EL131
01939         MOVE 'X'                 TO UPDATE-SWITCH CERT-SWITCH     EL131
01940         MOVE AL-UANON            TO REINCDA                       EL131
01941      ELSE                                                         EL131
01942         MOVE AL-UANOF            TO REINCDA.                      EL131
01943                                                                   EL131
01944      IF CERTI GREATER THAN LOW-VALUES                             EL131
01945          PERFORM 1460-EDIT-CERT-NO THRU 1460-EXIT                    CL*34
01946      ELSE                                                         EL131
01947          MOVE AL-UANOF           TO CERTA.                        EL131
01948                                                                   EL131
01949      IF CERTEFFI GREATER THAN LOW-VALUES                          EL131
01950          PERFORM 1470-EDIT-EFF THRU 1470-EXIT                        CL*34
01951      ELSE                                                         EL131
01952          MOVE AL-UANOF           TO CERTEFFA.                     EL131
01953                                                                   EL131
01954      IF CERTACTI GREATER THAN LOW-VALUES                          EL131
01955          PERFORM 1480-EDIT-ACCT THRU 1480-EXIT                       CL*34
01956      ELSE                                                         EL131
01957          MOVE AL-UANOF           TO CERTACTA.                     EL131
01958                                                                   EL131
01959      IF CERTSTI GREATER THAN LOW-VALUES                           EL131
01960          PERFORM 1490-EDIT-STATE THRU 1490-EXIT                      CL*34
01961      ELSE                                                         EL131
01962          MOVE AL-UANOF           TO CERTSTA.                      EL131
01963                                                                   EL131
01964      IF CERTCARI GREATER THAN LOW-VALUES                          EL131
01965          PERFORM 1500-EDIT-CARR THRU 1500-EXIT                       CL*34
01966      ELSE                                                         EL131
01967          MOVE AL-UANOF           TO CERTCARA.                     EL131
01901                                                                        000
CIDMOD     IF DLYACTV-RECORD-NEEDED                                          000
CIDMOD         PERFORM 1150-OUTPUT-ACTIVITY-RECORD THRU                      000
CIDMOD                 1150-EXIT                                             000
CIDMOD     END-IF.
CIDMOD
01969      IF CERTEFFI GREATER LOW-VALUES OR                            EL131
01970         CERTACTI GREATER LOW-VALUES OR                            EL131
01971         CERTSTI  GREATER LOW-VALUES OR                            EL131
01972         CERTCARI GREATER LOW-VALUES OR                            EL131
01973         CERTGRPI GREATER LOW-VALUES                               EL131
01974         PERFORM 1620-VERIFY-ACCT THRU 1620-EXIT.                  EL131
01975                                                                      CL*14
121802*    IF PI-COMPANY-ID = 'CRI' OR 'PEM' OR 'NCL'
121802*      IF PI-PREM-TYPE NOT = '1'  AND         
121802*         PI-NO-PMTS = ZEROS                 
121802*          IF LOANBALL GREATER +0 OR        
121802*             ACVBENEL GREATER +0          
121802*               PERFORM 1620-VERIFY-ACCT THRU 1620-EXIT. 

           if not acct-found
              perform 1620-VERIFY-ACCT THRU 1620-EXIT
           end-if


           MOVE PI-COMPANY-CD          TO COMPANY-CODE
           MOVE PI-CARRIER             TO CARRIER-CODE
           MOVE PI-CLAIM-NO            TO CLAIM-NO
           MOVE PI-CERT-NO             TO CERT-NO

           EXEC CICS READ                                  
               DATASET   (CLMS-FILE-ID)                    
               RIDFLD    (MSTR-KEY)                        
               SET       (ADDRESS OF CLAIM-MASTER)         
               RESP      (WS-RESPONSE)
           END-EXEC

           MOVE CL-COMPANY-CD          TO CERT-COMPANY-CODE
           MOVE CL-CERT-CARRIER        TO CERT-CARRIER
           MOVE CL-CERT-GROUPING       TO CERT-GROUP
           MOVE CL-CERT-STATE          TO CERT-STATE
           MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT
           MOVE CL-CERT-EFF-DT         TO CERT-DATE
           MOVE CL-CERT-NO             TO CERT-CERT

           EXEC CICS READ                                  
              DATASET   (CERT-FILE-ID)                    
              RIDFLD    (CERT-KEY)                        
              SET       (ADDRESS OF CERTIFICATE-MASTER)   
              RESP      (WS-RESPONSE)
           END-EXEC
                                                           
           if mfnamei > low-values
              move mfnamei to w-name-first
           else
              move cl-insured-1st-name to w-name-first
           end-if

           if mlnamei > low-values
              move mlnamei to w-name-last
           else
              move cl-insured-last-name to w-name-last
           end-if

           if instypel > zeros
              evaluate true
                 when cl-no-of-days-paid > zeros
                    move er-1668       to emi-error
                    perform 9900-error-format
                                       thru 9900-exit
                    move al-uabon      to instypea
                    move -1            to instypel
                    move 'X'           to error-switch
                 when instypei = 'C'
                    if (w-name-first (1:10) = cm-insured-first-name)
011020                 and (w-name-last = cm-insured-last-name)
                       move er-1666    to emi-error
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                       move al-uabon   to instypea
                       move -1         to instypel
                       move 'X'        to error-switch
                    end-if
                 when instypei = 'P'
                    if (w-name-first (1:10) <> cm-insured-first-name)
                       or (w-name-last <> cm-insured-last-name)
                       move er-1676    to emi-error
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                       move al-uabon   to instypea
                       move -1         to instypel
                       move 'X'        to error-switch
                    end-if
              end-evaluate
           end-if

      *    if (instypel > zeros)
      *       and (cl-no-of-days-paid > zero)
      *       move er-1668             to emi-error
      *       perform 9900-error-format thru 9900-exit
      *       move al-uabon            to instypea
      *       move -1                  to instypel
      *       move 'X'                 to error-switch
      *    end-if
      *
      *    if instypel > zero
      *       if instypei = 'C'
      *          if w-name-first (1:10) = cm-insured-first-name
      *             move er-1666          to emi-error
      *             PERFORM 9900-ERROR-FORMAT
      *                                   THRU 9900-EXIT
      *             move al-uabon         to instypea
      *             move -1               to instypel
      *             move 'X'              to error-switch
      *          end-if
      *       end-if
      *    end-if

020816     if (pi-company-id = 'DCC' OR 'VPP')
              and (acct-found)
              display ' about to perform 2120 '
              perform 2120-check-pdef  thru 2120-exit
              display ' back from 2120- ' ws-pre-exsist
081817        IF EXTENSL > ZERO
081817           IF EXTENSI NUMERIC
081817              IF EXTENSI >  WS-MAX-EXTENSION
081817                AND ERPDEF-FOUND
081817                 MOVE ER-1678          TO EMI-ERROR
081817                 PERFORM 9900-ERROR-FORMAT
081817                                       THRU 9900-EXIT
081817                 MOVE AL-UABON         TO EXTENSA
081817                 MOVE -1               TO EXTENSL
081817                 MOVE 'X'              TO ERROR-SWITCH
081817              ELSE
081817                 MOVE 'X'              TO UPDATE-SWITCH
081817              END-IF
081817           ELSE
081817              MOVE ER-1778          TO EMI-ERROR
081817              PERFORM 9900-ERROR-FORMAT
081817                                    THRU 9900-EXIT
081817              MOVE AL-UABON         TO EXTENSA
081817              MOVE -1               TO EXTENSL
081817              MOVE 'X'              TO ERROR-SWITCH
081817           END-IF
081817        END-IF
           end-if

           if benperl > zeros
              move al-uanon            to benpera
           end-if
           if benperl > zeros
              if benperi numeric
                 perform 1015-edit-benefit-period
                                       thru 1015-exit
              else
                 move er-1669          to emi-error
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 move al-uabon         to benpera
                 move -1               to benperl
                 move 'X'              to error-switch
              end-if
          end-if

           if instypel > zeros
              if instypei = 'P' OR 'C'
                 move 'X'              to update-switch
                                          mstr-switch
                 move al-uanon         to instypea
              else
                  move er-1654          to emi-error
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 move al-uabon         to instypea
                 move -1               to instypel
                 move 'X'              to error-switch
              end-if
          end-if

052113     if accswl > zeros
052113        if accswi = 'Y' OR 'N'
052113           MOVE 'X'              TO UPDATE-SWITCH
052113                                    MSTR-SWITCH
052113           MOVE AL-UANON         TO accswa
052113*       else
052113*          move er-1656          to emi-error
052113*          PERFORM 9900-ERROR-FORMAT
052113*                                THRU 9900-EXIT
052113*          move al-uabon         to accswa
052113*          move -1               to accswl
052113*          move 'X'              to error-switch
052113*          move al-uanof         to accswa
052113        end-if
052113     end-if

01983      IF CERTL = ZEROS AND                                            CL*34
01984         SUFXL = ZEROS                                                CL*34
01985           GO TO 1010-EXIT.                                           CL*34
01986                                                                      CL**8
01987      IF CERTL GREATER THAN ZEROS                                     CL**8
01988          IF CERTI NOT = PI-SAVE-CERT-NO-PRIME                        CL*34
01989              MOVE CERTI          TO PI-SAVE-CERT-NO-PRIME            CL**8
01990              MOVE 'Y'            TO SKIP-ATTRIBUTE                   CL**8
01991              MOVE 'X'            TO ERROR-SWITCH                     CL**8
01992              PERFORM 7000-RESET-ATTRIBUTE THRU 7000-EXIT.            CL**8
01993                                                                      CL**8
01994      IF SUFXL GREATER THAN ZEROS                                     CL**8
01995          IF SUFXI NOT = PI-SAVE-CERT-NO-SUFX                         CL*34
01996              MOVE SUFXI          TO PI-SAVE-CERT-NO-SUFX             CL**8
01997              MOVE 'Y'            TO SKIP-ATTRIBUTE                   CL**8
01998              MOVE 'X'            TO ERROR-SWITCH                     CL**8
01999              PERFORM 7000-RESET-ATTRIBUTE THRU 7000-EXIT.            CL**8
02000                                                                   EL131
02001  1010-EXIT.                                                       EL131
02002      EXIT.                                                        EL131
02003                                                                   EL131
       1015-edit-benefit-period.

           move ' '                    to ws-benper-sw
           evaluate true
              when not erpdef-found
                 set good-benper       to true
              when pd-recurring-yn (a1) = 'Y'
                 set good-benper       to true
              when pd-recurring-yn (a1) = 'N'
                 if benperi < 02
                    set good-benper    to true
                 end-if
              when pd-rec-crit-period (a1) numeric
                 if benperi <= pd-rec-crit-period (a1)
                    set good-benper    to true
                 end-if
           end-evaluate

           MOVE CERT-KEY               TO ELCRTT-KEY
           MOVE 'B'                    TO CTRLR-REC-TYPE
           EXEC CICS READ
              DATASET   ('ELCRTT')
              RIDFLD    (ELCRTT-KEY)
              INTO      (CERTIFICATE-TRAILERS)
              RESP      (WS-RESPONSE)
           END-EXEC            
           IF WS-RESP-NORMAL
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or ((cs-claim-type (s1) = cl-claim-type)
                 and (cs-insured-type (s1) = cl-insured-type)
                 and (cs-benefit-period (s1) > benperi))
              end-perform
              if s1 < +25
                 move er-1665          to emi-error
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 move al-uabon         to benpera
                 move -1               to benperl
                 move 'X'              to error-switch
                 go to 1015-exit
      *       else
      *          set good-benper       to true
              end-if
           end-if

           if good-benper
              move 'X'                 to update-switch
                                          mstr-switch
              move al-uanon            to benpera
           else
              move er-1669             to emi-error
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              move al-uabon            to benpera
              move -1                  to benperl
              move 'X'                 to error-switch
           end-if

           .
       1015-exit.
           exit.
021418 1030-EDIT-NAME.
021418     IF INSTYPEL > ZEROS
021418        MOVE INSTYPEI TO CL-INSURED-TYPE
021418     END-IF
021418     IF MFNAMEL > +0                                                 CL*34
021418        IF CL-INSURED-TYPE = 'C'
021418          AND (CM-INSURED-FIRST-NAME = MFNAMEI(1:10))
021418           MOVE ER-1675             TO EMI-ERROR
021418           MOVE -1                  TO MFNAMEL
021418           MOVE AL-UABON            TO MFNAMEA
021418           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
021418           MOVE 'X'                 TO ERROR-SWITCH
021418        ELSE
021418          IF CL-INSURED-TYPE = 'P'
021418            AND (CM-INSURED-FIRST-NAME <> MFNAMEI(1:10))
021418             MOVE ER-1676             TO EMI-ERROR
021418             MOVE -1                  TO MFNAMEL
021418             MOVE AL-UABON            TO MFNAMEA
021418             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
021418             MOVE 'X'                 TO ERROR-SWITCH
021418          END-IF
021418        END-IF
021418     END-IF.
021418
021418 1030-EXIT.
021418     EXIT.

031815 1050-EDIT-BENE.
031815     MOVE PI-COMPANY-CD      TO TRLR-COMPANY-CD.
031815     MOVE PI-CARRIER         TO TRLR-CARRIER.
031815     MOVE PI-CLAIM-NO        TO TRLR-CLAIM-NO.
031815     MOVE PI-CERT-NO         TO TRLR-CERT-NO.
031815     MOVE 1000               TO TRLR-SEQ-NO.
031815
031815     EXEC CICS HANDLE CONDITION
031815          NOTFND    (1052-DONE)
031815          ENDFILE   (1052-DONE)
031815     END-EXEC.
031815
031815     EXEC CICS STARTBR
031815         DATASET (TRLR-FILE-ID)
031815         RIDFLD  (TRLR-KEY)
031815     END-EXEC
031815
031815
031815     MOVE SPACES TO WS-TRLR-FILE-EOF
031815                    WS-PAYMENT-PENDING
031815     PERFORM 1052-READ-TRLR THRU 1052-EXIT
031815       UNTIL  TRLR-FILE-EOF
031815         OR PAYMENT-PENDING
031815
031815     EXEC CICS ENDBR
031815         DATASET (TRLR-FILE-ID)
031815     END-EXEC.
031815 1050-EXIT.
031815     EXIT.
031815
031815 1052-READ-TRLR.
031815     EXEC CICS READNEXT
031815          SET      (ADDRESS OF ACTIVITY-TRAILERS)
031815          DATASET  (TRLR-FILE-ID)
031815          RIDFLD   (TRLR-KEY)
031815     END-EXEC.
031815
031815     IF PI-COMPANY-CD   = TRLR-COMPANY-CD
031815       AND PI-CARRIER  = TRLR-CARRIER
031815       AND PI-CLAIM-NO = TRLR-CLAIM-NO
031815       AND PI-CERT-NO  = TRLR-CERT-NO
031815        CONTINUE
031815     ELSE
031815        GO TO 1052-DONE
031815     END-IF.
031815
031815     IF PAYMENT-TR
031815       AND BENEFICIARY-PAID
031815       AND AT-VOID-DT = LOW-VALUES
031815       AND AT-CHECK-WRITTEN-DT NOT > SPACES
031815         SET PAYMENT-PENDING TO TRUE
031815     END-IF.
062217
062217     IF CORRESPONDENCE-TR
062217        IF AT-AUTH-RCVD = 'Y'
062217           SET AUTH-RCVD TO TRUE
062217        ELSE
062217        IF AT-AUTH-RCVD = 'N'
062217           SET NO-AUTH-RCVD TO TRUE
062217        END-IF
062217        END-IF
062217     END-IF.
031815
031815     GO TO 1052-EXIT.
031815
031815 1052-DONE.
031815     SET TRLR-FILE-EOF TO TRUE.
031815 1052-EXIT.
031815     EXIT.
031815     EJECT


02005  1120-EDIT-PROC.                                                  EL131
02006      MOVE PI-COMPANY-ID          TO CNTL-CO-ID.                   EL131
02007      MOVE '2'                    TO CNTL-REC-TYPE.                EL131
02008      MOVE PROCI                  TO CNTL-PROC-ID.                 EL131
02009      MOVE ZERO                   TO CNTL-SEQ-NO.                  EL131
02010                                                                   EL131
02011      EXEC CICS HANDLE CONDITION                                   EL131
02012           NOTFND    (1120-CNTL-NOTFND)                            EL131
02013      END-EXEC.                                                    EL131
02014                                                                   EL131
02015      EXEC CICS READ                                               EL131
02016           SET      (ADDRESS OF CONTROL-FILE)                         CL*32
02017           DATASET  (CNTL-FILE-ID)                                 EL131
02018           RIDFLD   (CNTL-KEY)                                     EL131
02019      END-EXEC.                                                    EL131
02020                                                                   EL131
02021      MOVE AL-UANON               TO PROCA.                        EL131
02022      MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH MSTR-KEY-SWITCH.       EL131
02023      GO TO 1120-EXIT.                                                CL*34
02024                                                                   EL131
02025  1120-CNTL-NOTFND.                                                EL131
02026      MOVE ER-0273                TO EMI-ERROR.                    EL131
02027      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
02028      MOVE AL-UABON               TO PROCA.                        EL131
02029      MOVE -1                     TO PROCL.                        EL131
02030      MOVE 'X'                    TO ERROR-SWITCH.                 EL131
02031                                                                   EL131
02032  1120-EXIT.                                                          CL*34
02033      EXIT.                                                        EL131
CIDMOD                                                                       000
CIDMOD*************************************************************     ***  000
CIDMOD*****    BEGIN PROCESSING FOR OUTPUTTING DLYACTV RECORD    **     ***  000
CIDMOD*************************************************************     ***  000
CIDMOD                                                                       000
CIDMOD 1150-OUTPUT-ACTIVITY-RECORD.                                          000
CIDMOD                                                                       000
CIDMOD     EXEC CICS GETMAIN                                                 000
CIDMOD         SET (ADDRESS OF DAILY-ACTIVITY-RECORD)                        000
CIDMOD         LENGTH (25)                                                   000
CIDMOD         INITIMG (WS-BLANK)                                            000
CIDMOD     END-EXEC.                                                         000
CIDMOD                                                                       000
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.             000
CIDMOD     MOVE PI-COMPANY-CD          TO DA-COMP-CD.                        000
CIDMOD     MOVE PI-CARRIER             TO DA-CARRIER.                        000
CIDMOD     MOVE PI-CLAIM-NO            TO DA-CLAIM-NO.                       000
CIDMOD     MOVE PI-CERT-NO             TO DA-CERT-NO.                        000
CIDMOD     MOVE ZEROS                  TO DA-TRAILER-SEQ-NO.                 000
CIDMOD     MOVE WS-HOLD-CLAIM-STATUS   TO DA-RECORD-TYPE.                    000
CIDMOD
CIDMOD     EXEC CICS HANDLE CONDITION                                        000
CIDMOD         NOTOPEN (1150-NOTOPEN-ERROR)                                  000
CIDMOD         DUPREC (1150-EXIT)                                            000
CIDMOD     END-EXEC.                                                         000
CIDMOD     MOVE DAILY-ACTIVITY-RECORD  TO JP-RECORD-AREA.                    000
CIDMOD     MOVE 'DLYACTV'              TO JP-FILE-ID.                        000
CIDMOD     MOVE 'A'                    TO JP-RECORD-TYPE.                    000
CIDMOD     MOVE +48                    TO JOURNAL-LENGTH.                    000
CIDMOD
CIDMOD     EXEC CICS WRITE                                                   000
CIDMOD         DATASET ('DLYACTV')                                           000
CIDMOD         RIDFLD (DA-KEY)                                               000
CIDMOD         FROM (DAILY-ACTIVITY-RECORD)                                  000
CIDMOD         LENGTH (25)                                                   000
CIDMOD     END-EXEC.                                                         000
CIDMOD
CIDMOD     GO TO 1150-EXIT.                                                  000
CIDMOD                                                                       000
CIDMOD 1150-NOTOPEN-ERROR.                                                   000
CIDMOD                                                                       000
CIDMOD     MOVE '2955'                 TO EMI-ERROR.                         000
CIDMOD     MOVE -1                     TO MAINTL.                            000
CIDMOD     MOVE AL-UANON               TO MAINTA.                            000
CIDMOD     MOVE 'X'                    TO ERROR-SWITCH.                      000
CIDMOD     PERFORM 9900-ERROR-FORMAT THRU                                    000
CIDMOD             9900-EXIT.                                                000
CIDMOD                                                                       000
CIDMOD 1150-EXIT.                                                            000
CIDMOD     EXIT.                                                             000
CIDMOD                                                                       000
CIDMOD*************************************************************     **** 000
CIDMOD*****    END OF PROCESSING FOR OUTPUTTING DLYACTV RECORD    *     **** 000
CIDMOD*************************************************************     **** 000
CIDMOD                                                                  EL131
02035  1240-EDIT-LIFE-CODE.                                             EL131
02036                                                                      CL**8
02037      IF INVALID-BENEFIT-CODE                                         CL**8
02038          GO TO 1240-SET-ERROR.                                    EL131
02039                                                                   EL131
02040      MOVE SPACES                 TO BENEFIT-KEY.                  EL131
02041      MOVE PI-COMPANY-ID          TO BEN-CO-ID.                    EL131
02042      MOVE '4'                    TO BEN-REC-TYPE.                 EL131
02043      MOVE LCVCDI                 TO BEN-ACC-CD HOLD-BENEFIT.         CL**8
02044      MOVE ZERO                   TO BEN-SEQ-NO.                   EL131
02045                                                                   EL131
02046      EXEC CICS READ GTEQ                                          EL131
02047           DATASET  (CNTL-FILE-ID)                                 EL131
02048           RIDFLD   (BENEFIT-KEY)                                  EL131
02049           SET      (ADDRESS OF CONTROL-FILE)                         CL*32
02050      END-EXEC.                                                    EL131
02051                                                                   EL131
02052      IF CF-COMPANY-ID  NOT = PI-COMPANY-ID OR                     EL131
02053         CF-RECORD-TYPE NOT = '4'                                  EL131
02054          GO TO 1240-SET-ERROR.                                    EL131
02055                                                                   EL131
02056      MOVE ZERO                   TO COUNT-2.                      EL131
02057      PERFORM 5040-FIND-BENEFIT THRU 5060-EXIT.                    EL131
02058                                                                      CL**8
02059      IF SCREEN-ERROR                                              EL131
02060          GO TO 1240-SET-ERROR.                                    EL131
02061                                                                   EL131
02062  1240-SET-CODE-SWITCHES.                                          EL131
02063      MOVE 'X'                    TO UPDATE-SWITCH CERT-SWITCH.    EL131
02064      MOVE AL-UANON               TO LCVCDA.                          CL**8
02065      GO TO 1240-EXIT.                                             EL131
02066                                                                   EL131
02067  1240-SET-ERROR.                                                  EL131
02068      MOVE ER-0240                TO EMI-ERROR.                    EL131
02069      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
02070      MOVE 'X'                    TO ERROR-SWITCH.                 EL131
02071      MOVE AL-UABON               TO LCVCDA.                          CL**8
02072      MOVE -1                     TO LCVCDL.                          CL**8
02073                                                                   EL131
02074  1240-EXIT.                                                       EL131
02075      EXIT.                                                        EL131
02076                                                                   EL131
02077  1310-EDIT-AH-CODE.                                               EL131
02078      IF INVALID-BENEFIT-CODE                                         CL**8
02079          GO TO 1310-SET-ERROR.                                    EL131
02080                                                                   EL131
02081      MOVE SPACES                 TO BENEFIT-KEY.                  EL131
02082      MOVE PI-COMPANY-ID          TO BEN-CO-ID.                    EL131
02083      MOVE '5'                    TO BEN-REC-TYPE.                 EL131
02084      MOVE ACVCDI                 TO BEN-ACC-CD HOLD-BENEFIT.         CL**8
02085      MOVE ZERO                   TO BEN-SEQ-NO.                   EL131
02086                                                                   EL131
02087      EXEC CICS READ GTEQ                                          EL131
02088           DATASET  (CNTL-FILE-ID)                                 EL131
02089           RIDFLD   (BENEFIT-KEY)                                  EL131
02090           SET      (ADDRESS OF CONTROL-FILE)                         CL*32
02091      END-EXEC.                                                    EL131
02092                                                                   EL131
02093      IF CF-COMPANY-ID  NOT = PI-COMPANY-ID OR                     EL131
02094         CF-RECORD-TYPE NOT = '5'                                  EL131
02095          GO TO 1310-SET-ERROR.                                    EL131
02096                                                                   EL131
02097      MOVE ZERO                   TO COUNT-2.                      EL131
02098      PERFORM 5040-FIND-BENEFIT THRU 5060-EXIT.                    EL131
02099      IF SCREEN-ERROR                                              EL131
02100          GO TO 1310-SET-ERROR.                                    EL131
02101                                                                   EL131
02102  1310-SET-CODE-SWITCHES.                                          EL131
02103      MOVE 'X'                    TO UPDATE-SWITCH CERT-SWITCH.    EL131
02104      MOVE AL-UANON               TO ACVCDA.                          CL**8
02105      GO TO 1310-EXIT.                                                CL*34
02106                                                                   EL131
02107  1310-SET-ERROR.                                                  EL131
02108      MOVE ER-0250                TO EMI-ERROR.                    EL131
02109      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*34
02110      MOVE AL-UABON               TO ACVCDA.                          CL**8
02111      MOVE 'X'                    TO ERROR-SWITCH.                 EL131
02112      MOVE -1                     TO ACVCDL.                          CL**8
02113                                                                   EL131
02114  1310-EXIT.                                                          CL*34
02115      EXIT.                                                        EL131
02116                                                                   EL131
02117      EJECT                                                        EL131
02118  1460-EDIT-CERT-NO.                                               EL131
02119      MOVE CERTI                  TO WS-CERT-NO-PRIME.             EL131
02120      MOVE SUFXI                  TO WS-CERT-NO-SUFX.              EL131
02121                                                                   EL131
02122      IF WS-SAVE-CERT-NO = SPACES                                     CL*34
02123          MOVE ER-0203            TO EMI-ERROR                     EL131
02124          GO TO 1460-CERT-NO-ERROR.                                EL131
02125                                                                   EL131
02126      IF WS-CERT-NO-PRIME = SPACES AND                             EL131
02127         WS-CERT-NO-SUFX GREATER THAN SPACE                        EL131
02128             MOVE ER-0210         TO EMI-ERROR                     EL131
02129             GO TO 1460-CERT-NO-ERROR.                             EL131
02130                                                                   EL131
02131      MOVE 'X'                    TO UPDATE-SWITCH.                EL131
02132      MOVE 'Y'                    TO CERT-KEY-SWITCH.              EL131
02133      MOVE AL-UANON               TO CERTA.                        EL131
02134                                                                      CL**5
02135      IF SUFXL IS GREATER THAN 0                                      CL**5
02136          MOVE AL-UANON           TO SUFXA.                           CL**5
02137                                                                      CL**5
02138      GO TO 1460-EXIT.                                             EL131
02139                                                                   EL131
02140  1460-CERT-NO-ERROR.                                              EL131
02141      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
02142      MOVE AL-UABON               TO SUFXA.                        EL131
02143      MOVE AL-UABON               TO CERTA.                        EL131
02144      MOVE -1                     TO CERTL.                        EL131
02145      MOVE 'X'                    TO ERROR-SWITCH.                 EL131
02146                                                                   EL131
02147  1460-EXIT.                                                       EL131
02148      EXIT.                                                        EL131
02149                                                                   EL131
02150  1470-EDIT-EFF.                                                   EL131
02151      IF CERTEFFI = SPACES                                         EL131
02152          GO TO 1470-DATE-ERROR.                                   EL131
02153                                                                   EL131
02154      MOVE CERTEFFI               TO DEEDIT-DATE-INPUT.            EL131
02155      PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT.                     EL131
02156      IF DEEDIT-DATE = ZERO                                        EL131
02157          GO TO 1470-DATE-ERROR.                                   EL131
02158                                                                   EL131
02159      MOVE DEEDIT-DATE            TO DC-GREG-DATE-1-MDY.           EL131
02160      MOVE '4'                    TO DC-OPTION-CODE.               EL131
02161      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.                    EL131
02162      IF NOT DATE-CONVERSION-ERROR                                 EL131
02163          MOVE DC-BIN-DATE-1      TO HOLD-EFF                      EL131
02164          MOVE AL-UANON           TO CERTEFFA                      EL131
02165          MOVE 'X'                TO UPDATE-SWITCH                 EL131
02166          MOVE 'Y'                TO CERT-KEY-SWITCH               EL131
02167          MOVE DC-GREG-DATE-1-EDIT TO CERTEFFO                     EL131
02168          GO TO 1470-EXIT.                                         EL131
02169                                                                   EL131
02170  1470-DATE-ERROR.                                                 EL131
02171      MOVE ER-0215                TO EMI-ERROR.                    EL131
02172      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
02173      MOVE AL-UABON               TO CERTEFFA.                     EL131
02174      MOVE -1                     TO CERTEFFL.                     EL131
02175      MOVE 'X'                    TO ERROR-SWITCH.                 EL131
02176                                                                   EL131
02177  1470-EXIT.                                                       EL131
02178      EXIT.                                                        EL131
02179                                                                   EL131
02180      EJECT                                                        EL131
02181  1480-EDIT-ACCT.                                                  EL131
02182      IF CERTACTI = SPACES                                         EL131
02183          MOVE ER-0232            TO EMI-ERROR                     EL131
02184          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL131
02185          MOVE AL-UABON           TO CERTACTA                      EL131
02186          MOVE -1                 TO CERTACTL                      EL131
02187          MOVE 'X'                TO ERROR-SWITCH                  EL131
02188      ELSE                                                         EL131
02189          MOVE 'X'                TO UPDATE-SWITCH                 EL131
02190          MOVE 'Y'                TO CERT-KEY-SWITCH.              EL131
02191                                                                   EL131
02192  1480-EXIT.                                                       EL131
02193      EXIT.                                                        EL131
02194                                                                   EL131
02195  1490-EDIT-STATE.                                                 EL131
02196      IF CERTSTI = SPACES                                          EL131
02197          MOVE ER-0233            TO EMI-ERROR                     EL131
02198          GO TO 1490-STATE-ERROR.                                  EL131
02199                                                                      CL*34
02200      MOVE PI-COMPANY-ID          TO CNTL-CO-ID.                   EL131
02201      MOVE '3'                    TO CNTL-REC-TYPE.                EL131
02202      MOVE SPACES                 TO CNTL-STATE-ACCESS.            EL131
02203      MOVE CERTSTI                TO CNTL-STATE-NUMBER.            EL131
02204      MOVE ZERO                   TO CNTL-SEQ-NO.                  EL131
02205                                                                      CL*34
02206      EXEC CICS HANDLE CONDITION                                   EL131
02207           NOTFND (1490-STATE-NOTFND)                              EL131
02208      END-EXEC.                                                       CL*34
02209                                                                   EL131
02210      EXEC CICS READ                                               EL131
02211           SET        (ADDRESS OF CONTROL-FILE)                       CL*32
02212           DATASET    ('ELCNTL')                                   EL131
02213           RIDFLD     (CNTL-KEY)                                   EL131
02214           KEYLENGTH  (CNTL-GENERIC-LENGTH)                        EL131
02215           GENERIC                                                    CL*34
02216      END-EXEC.                                                       CL*34
02217                                                                   EL131
02218      MOVE AL-UANON               TO CERTSTA.                      EL131
02219      MOVE 'X'                    TO UPDATE-SWITCH.                EL131
02220      MOVE 'Y'                    TO CERT-KEY-SWITCH.              EL131
02221      GO TO 1490-EXIT.                                             EL131
02222                                                                   EL131
02223  1490-STATE-NOTFND.                                               EL131
02224      MOVE ER-0149                TO EMI-ERROR.                    EL131
02225                                                                   EL131
02226  1490-STATE-ERROR.                                                EL131
02227      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
02228      MOVE AL-UABON               TO CERTSTA.                      EL131
02229      MOVE -1                     TO CERTSTL.                      EL131
02230      MOVE 'X'                    TO ERROR-SWITCH.                 EL131
02231                                                                   EL131
02232  1490-EXIT.                                                       EL131
02233      EXIT.                                                        EL131
02234      EJECT                                                        EL131
02235  1500-EDIT-CARR.                                                  EL131
02236      IF CERTCARI = SPACES                                         EL131
02237          GO TO 1500-CARR-NOTFND.                                  EL131
02238                                                                   EL131
02239      MOVE PI-COMPANY-ID          TO CNTL-CO-ID.                   EL131
02240      MOVE '6'                    TO CNTL-REC-TYPE.                EL131
02241      MOVE SPACES                 TO CNTL-CARRIER-ACCESS.          EL131
02242      MOVE CERTCARI               TO CNTL-CARRIER.                 EL131
02243      MOVE ZERO                   TO CNTL-SEQ-NO.                  EL131
02244                                                                      CL*34
02245      EXEC CICS HANDLE CONDITION                                   EL131
02246           NOTFND   (1500-CARR-NOTFND)                             EL131
02247      END-EXEC.                                                       CL*34
02248                                                                   EL131
02249      EXEC CICS READ                                               EL131
02250           SET        (ADDRESS OF CONTROL-FILE)                       CL*32
02251           DATASET    ('ELCNTL')                                   EL131
02252           RIDFLD     (CNTL-KEY)                                   EL131
02253           KEYLENGTH  (CNTL-GENERIC-LENGTH)                        EL131
02254           GENERIC                                                    CL*34
02255      END-EXEC.                                                       CL*34
02256                                                                   EL131
02257      MOVE AL-UANON               TO CERTCARA.                     EL131
02258      MOVE 'X'                    TO UPDATE-SWITCH.                EL131
02259      MOVE 'Y'                    TO CERT-KEY-SWITCH.              EL131
02260      GO TO 1500-EXIT.                                             EL131
02261                                                                   EL131
02262  1500-CARR-NOTFND.                                                EL131
02263      MOVE ER-0360                TO EMI-ERROR.                    EL131
02264      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
02265      MOVE AL-UABON               TO CERTCARA.                     EL131
02266      MOVE -1                     TO CERTCARL.                     EL131
02267      MOVE 'X'                    TO ERROR-SWITCH.                 EL131
02268                                                                   EL131
02269  1500-EXIT.                                                       EL131
02270      EXIT.                                                        EL131
02271      EJECT                                                        EL131
02272  1510-DEEDIT-DATE.                                                EL131
02273      EXEC CICS BIF DEEDIT                                         EL131
02274           FIELD    (DEEDIT-DATE-INPUT)                            EL131
02275           LENGTH   (DATE-LENGTH)                                  EL131
02276      END-EXEC.                                                    EL131
02277                                                                   EL131
02278  1510-EXIT.                                                       EL131
02279      EXIT.                                                        EL131
02280                                                                   EL131
02281      EJECT                                                        EL131
02282  1620-VERIFY-ACCT.                                                EL131
02283                                                                      CL**8
           move ' '                    to ws-eracct-sw
02284      MOVE PI-COMPANY-CD          TO ACCT-COMPANY-CODE.            EL131
02285      MOVE PI-CARRIER             TO ACCT-CARRIER.                 EL131
02286      MOVE PI-GROUPING            TO ACCT-GROUP.                   EL131
02287      MOVE PI-STATE               TO ACCT-STATE.                   EL131
02288      MOVE PI-ACCOUNT             TO ACCT-ACCOUNT.                 EL131
02289                                                                   EL131
02290      IF CERTEFFI GREATER THAN LOW-VALUES                          EL131
02291          MOVE HOLD-EFF           TO ACCT-DATE                     EL131
02292      ELSE                                                         EL131
02293          MOVE PI-CERT-EFF-DT     TO HOLD-EFF ACCT-DATE.           EL131
02294                                                                   EL131
02295      IF CERTACTI GREATER THAN LOW-VALUES                          EL131
02296          MOVE CERTACTI           TO  ACCT-ACCOUNT.                EL131
02297      IF CERTSTI GREATER THAN LOW-VALUES                           EL131
02298          MOVE CERTSTI            TO  ACCT-STATE.                  EL131
02299      IF CERTCARI GREATER THAN LOW-VALUES                          EL131
02300          MOVE CERTCARI           TO  ACCT-CARRIER.                EL131
02301      IF CERTGRPI GREATER THAN LOW-VALUES                          EL131
02302          MOVE CERTGRPI           TO ACCT-GROUP.                   EL131
02303                                                                   EL131
02304      MOVE ACCT-PARTIAL-KEY       TO SAVE-ACCT-KEY.                EL131
02305                                                                   EL131
02306      EXEC CICS HANDLE CONDITION                                   EL131
02307           NOTFND  (1620-ACCT-NOTFND)                              EL131
02308      END-EXEC.                                                    EL131
02309                                                                   EL131
02310      EXEC CICS STARTBR                                            EL131
02311           DATASET   (ACCT-FILE-ID)                                EL131
02312           RIDFLD    (ACCT-KEY)                                    EL131
02313      END-EXEC.                                                    EL131
02314                                                                   EL131
02315  1620-ACCT-LOOP.                                                  EL131
02316                                                                      CL**8
02317      EXEC CICS READNEXT                                           EL131
02318           DATASET  (ACCT-FILE-ID)                                 EL131
02319           RIDFLD   (ACCT-KEY)                                     EL131
02320           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL*32
02321      END-EXEC.                                                    EL131
02322                                                                   EL131
02323      IF SAVE-ACCT-KEY NOT = ACCT-PARTIAL-KEY                      EL131
02324          GO TO 1620-ACCT-NOTFND.                                  EL131
02325                                                                   EL131
02326      IF HOLD-EFF NOT LESS AM-EFFECTIVE-DT  AND                    EL131
02327         HOLD-EFF     LESS AM-EXPIRATION-DT                        EL131
02328          NEXT SENTENCE                                            EL131
02329      ELSE                                                         EL131
02330          GO TO 1620-ACCT-LOOP.                                    EL131
02331                                                                   EL131
           set acct-found to true

02332      IF PI-COMPANY-ID NOT = 'CRI' AND 'PEM' AND 'NCL'                CL*25
02333          GO TO 1620-ACCT-END-BROWSE.                                 CL*14
02334                                                                      CL*14
02335      IF LOANBALL GREATER THAN +0                                     CL*32
02336          IF AM-MAX-TOT-BEN NUMERIC  AND                              CL*14
02337             AM-MAX-TOT-BEN GREATER THAN ZERO                         CL*14
02338              IF HOLD-LOANBAL GREATER THAN AM-MAX-TOT-BEN             CL*14
02339                  MOVE AL-UNBON     TO LOANBALA                       CL*14
02340                  MOVE -1           TO LOANBALL                       CL*14
02341                  MOVE 'X'          TO ERROR-SWITCH                   CL*14
02342                  MOVE ER-7641      TO EMI-ERROR                      CL*14
02343                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           CL*14
02344                                                                      CL*14
02345      IF ACVBENEL GREATER THAN +0                                     CL*32
02346          IF AM-MAX-MON-BEN NUMERIC  AND                              CL*14
02347             AM-MAX-MON-BEN GREATER THAN ZERO                         CL*14
02348              IF HOLD-AH-CV-BEN GREATER THAN AM-MAX-MON-BEN           CL*14
02349                  MOVE AL-UNBON     TO ACVBENEA                       CL*14
02350                  MOVE -1           TO ACVBENEL                       CL*14
02351                  MOVE 'X'          TO ERROR-SWITCH                   CL*14
02352                  MOVE ER-7642      TO EMI-ERROR                      CL*14
02353                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           CL*14
02354                                                                      CL*14
02355  1620-ACCT-END-BROWSE.                                               CL*14
02356                                                                      CL*14
02357      EXEC CICS ENDBR                                              EL131
02358           DATASET (ACCT-FILE-ID)                                  EL131
02359      END-EXEC.                                                    EL131
02360                                                                   EL131
02361      GO TO 1620-EXIT.                                             EL131
02362                                                                   EL131
02363  1620-ACCT-NOTFND.                                                EL131
02364                                                                      CL**8
02365      MOVE -1                     TO MAINTL.                       EL131
02366      MOVE 'X'                    TO ERROR-SWITCH.                 EL131
02367      MOVE ER-0426                TO EMI-ERROR.                       CL**8
02368      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
02369                                                                   EL131
02370  1620-EXIT.                                                       EL131
02371      EXIT.                                                        EL131
02372                                                                      CL**8
02373      EJECT                                                        EL131
02374  2000-UPDATE-CLAIM.                                               EL131
02375                                                                      CL*26
02376      MOVE SPACES                 TO ERROR-SWITCH.                 EL131
02377      MOVE PI-COMPANY-CD          TO COMPANY-CODE.                 EL131
02378      MOVE PI-CARRIER             TO CARRIER-CODE.                    CL*22
02379      MOVE PI-CLAIM-NO            TO CLAIM-NO.                     EL131
02380      MOVE PI-CERT-NO             TO CERT-NO.                      EL131
02381                                                                   EL131
02382      IF DELETE-CLAIM                                              EL131
02383          GO TO 3000-DELETE-CLAIM.                                 EL131
02384                                                                   EL131
02385      IF  NOT MODIFY-CAP                                           EL131
02386          MOVE ER-0070            TO EMI-ERROR                     EL131
02387          MOVE 'X'                TO ERROR-SWITCH                  EL131
02388          MOVE -1                 TO MAINTL                        EL131
02389          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL131
02390          GO TO 2000-EXIT.                                         EL131
02391                                                                   EL131
02392      MOVE MSTR-KEY               TO TRLR-MAIN-KEY ACTQ-KEY.       EL131
02393      MOVE ZERO                   TO TRLR-SEQ-NO.                  EL131
02394                                                                   EL131
02395      EXEC CICS READ UPDATE                                        EL131
02396           SET       (ADDRESS OF CLAIM-MASTER)                        CL*32
02397           DATASET   (CLMS-FILE-ID)                                EL131
02398           RIDFLD    (MSTR-KEY)                                    EL131
02399      END-EXEC.                                                    EL131
02400                                                                   EL131
080613     if (incl > zeros)
080613        and (cl-no-of-pmts-made > 0)
080613        and (hold-incur > low-values)
080613        and (hold-incur not = cl-incurred-dt)
080613        if hold-incur > cl-incurred-dt
080613           move cl-incurred-dt   to dc-bin-date-1
080613           move hold-incur       to dc-bin-date-2
080613        else
080613           move hold-incur       to dc-bin-date-1
080613           move cl-incurred-dt   to dc-bin-date-2
080613        end-if
080613        move '1'                 to dc-option-code
080613        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
040616        if dc-elapsed-days > +180
080613           MOVE ER-1663             TO EMI-ERROR
080613           PERFORM 9900-ERROR-FORMAT
080613                                 THRU 9900-EXIT
080613           MOVE -1                  TO MAINTL
080613           GO TO 8110-SEND-DATA
080613        end-if
080613     end-if

062602     IF (CL-PRIORITY-CD = '8')
062602        AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
052113             AND 'AMWA' AND 'KMSB')
062602        MOVE ER-8003             TO EMI-ERROR
062602        PERFORM 9900-ERROR-FORMAT
062602                                 THRU 9900-EXIT
062602        MOVE -1                  TO MAINTL
062602        GO TO 8110-SEND-DATA
052113     END-IF
062602
02401      IF PI-UPDATE-BY NOT = CL-LAST-MAINT-USER                     EL131
02402          MOVE ER-0068            TO EMI-ERROR                        CL**8
02403          PERFORM 2010-RELEASE-REC THRU 2010-EXIT                  EL131
02404          GO TO 2000-EXIT.                                         EL131
02405                                                                   EL131
02406      IF PI-UPDATE-HHMMSS NOT = CL-LAST-MAINT-HHMMSS               EL131
02407          MOVE ER-0068            TO EMI-ERROR                        CL**8
02408          PERFORM 2010-RELEASE-REC THRU 2010-EXIT                  EL131
02409          GO TO 2000-EXIT.                                         EL131
02410                                                                   EL131
02411      IF CL-CLAIM-PAYMENT-STATUS NOT NUMERIC                          CL**8
02412          MOVE ZEROS              TO CL-CLAIM-PAYMENT-STATUS.         CL**8
02413                                                                      CL**8
02414      IF CL-PURGED-DT NOT = LOW-VALUES                                CL*34
02415          MOVE ER-7691            TO EMI-ERROR                        CL**8
02416          PERFORM 2010-RELEASE-REC THRU 2010-EXIT                     CL**8
02417          GO TO 2000-EXIT.                                            CL**8
02418                                                                      CL**8
02419      IF CERT-KEY-CHANGED                                             CL**8
02420          PERFORM 2060-CHANGE-CERT THRU 2060-EXIT.                    CL*34
02421                                                                   EL131
02422      IF SCREEN-ERROR                                              EL131
02423          MOVE ER-0068            TO EMI-ERROR                        CL**8
02424          PERFORM 2010-RELEASE-REC THRU 2010-EXIT                  EL131
02425          GO TO 2000-EXIT.                                         EL131
02426                                                                   EL131
02427      IF NINETY-TRLR-UPDATED                                          CL*39
02428          PERFORM 2425-UPDATE-NINETY-TRLR THRU 2425-EXIT.             CL*39
02429                                                                      CL*39
02430      IF MSTR-KEY-CHANGED                                          EL131
02431          PERFORM 2030-KEY-UPDATE THRU 2030-EXIT                      CL*34
02432         ELSE                                                         CL*34
02433          PERFORM 2020-NORMAL-UPDATE THRU 2020-EXIT.                  CL*34
02434                                                                   EL131
02435      IF TRLR-UPDATE-REQUIRED                                      EL131
02436          PERFORM 2300-UPDATE-TRLR THRU 2300-EXIT.                    CL*34
02437                                                                      CL**8
02438  2000-CHECK-CERT.                                                 EL131
02439      IF NOT CERT-UPDATES                                          EL131
02440          GO TO 2000-EXIT.                                         EL131
02441                                                                   EL131
02442      MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.            EL131
02443      MOVE PI-CARRIER             TO CERT-CARRIER.                 EL131
02444      MOVE PI-GROUPING            TO CERT-GROUP.                   EL131
02445      MOVE PI-STATE               TO CERT-STATE.                   EL131
02446      MOVE PI-ACCOUNT             TO CERT-ACCOUNT.                 EL131
02447      MOVE PI-CERT-EFF-DT         TO CERT-DATE.                    EL131
02448      MOVE PI-CERT-NO             TO CERT-CERT.                    EL131
02449                                                                   EL131
02450      IF CERT-ALT-KEY-CHANGED                                         CL**8
02451          PERFORM 2050-KEY-UPDATE THRU 2050-EXIT                      CL*34
02452      ELSE                                                         EL131
02453          PERFORM 2040-NORMAL-UPDATE THRU 2040-EXIT.                  CL*34
02454                                                                   EL131
02455  2000-EXIT.                                                       EL131
02456      EXIT.                                                        EL131
02457      EJECT                                                        EL131
02458  2010-RELEASE-REC.                                                EL131
02459      EXEC CICS UNLOCK                                             EL131
02460           DATASET  (CLMS-FILE-ID)                                 EL131
02461      END-EXEC                                                     EL131
02462                                                                   EL131
02463      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
02464      MOVE 'X'                    TO ERROR-SWITCH.                 EL131
02465                                                                   EL131
02466  2010-EXIT.                                                       EL131
02467      EXIT.                                                        EL131
02468                                                                   EL131
02469  2020-NORMAL-UPDATE.                                              EL131

02470      PERFORM 2600-CREATE-MAINT-NOTE THRU 2600-EXIT.                  CL*34
02471                                                                   EL131
02472      PERFORM 2100-MOVE-MSTR THRU 2100-EXIT.                       EL131
02473                                                                   EL131
02474      IF WS-OPEN-CLOSE-SW = 'Y'                                       CL*34
02475        PERFORM  7800-CHECK-AUTO-ACTIVITY THRU 7800-EXIT              CL*26
02476        IF PI-REC-FOUND-SW = 'Y'                                      CL*34
02477          IF CL-ACTIVITY-CODE = 09                                    CL*34
02478            NEXT SENTENCE                                             CL*26
02479          ELSE                                                        CL*26
02480            IF WS-RESET-SW = 'Y'                                      CL*34
02481              IF CL-CLAIM-STATUS = 'C'                                CL*34
02482                MOVE 07               TO  CL-ACTIVITY-CODE            CL*26
02483                MOVE SAVE-BIN-DATE    TO  CL-ACTIVITY-MAINT-DT        CL*26
02484                MOVE 'CLOS'           TO  CL-ACTIVITY-MAINT-TYPE      CL*26
02485              ELSE                                                    CL*26
02486                MOVE 08               TO  CL-ACTIVITY-CODE            CL*26
02487                MOVE SAVE-BIN-DATE    TO  CL-ACTIVITY-MAINT-DT        CL*26
02488                MOVE 'OPEN'           TO  CL-ACTIVITY-MAINT-TYPE.     CL*26
02489                                                                      CL*26
062121     IF PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL'
CIDMOD         MOVE 'Y'                TO CL-YESNOSW
CIDMOD     END-IF.
02826                                                                   EL131
020816     if pi-company-id = 'DCC' OR 'VPP'
              perform 1620-VERIFY-ACCT THRU 1620-EXIT
052113        perform 2120-check-pdef  thru 2120-exit
052113     end-if

           if ((benperl <> zeros)
              and (benperi numeric))
                        or
                  (instypel <> zeros)
              perform 2140-update-elcrtt thru 2140-exit
           end-if

02490      EXEC CICS HANDLE CONDITION                                   EL131
02491           DUPKEY   (2020-EXIT)                                       CL*27
02492      END-EXEC.                                                    EL131
02493                                                                   EL131
02494      EXEC CICS REWRITE                                            EL131
02495           DATASET   (CLMS-FILE-ID)                                EL131
02496           FROM      (CLAIM-MASTER)                                EL131
02497      END-EXEC.                                                    EL131
02498                                                                   EL131
02499  2020-EXIT.                                                       EL131
02500      EXIT.                                                        EL131
02501      EJECT                                                        EL131
02502  2030-KEY-UPDATE.                                                 EL131
02503      MOVE CLAIM-MASTER           TO JP-RECORD-AREA.               EL131
02504                                                                   EL131
02505      EXEC CICS DELETE                                             EL131
02506          DATASET  (CLMS-FILE-ID)                                  EL131
02507      END-EXEC.                                                    EL131
02508                                                                   EL131
02509      EXEC CICS GETMAIN                                            EL131
02510           SET      (ADDRESS OF CLAIM-MASTER)                         CL*32
02511           LENGTH   (MSTR-LENGTH)                                  EL131
02512           INITIMG  (GETMAIN-SPACE)                                EL131
02513      END-EXEC.                                                    EL131
02514                                                                   EL131
02515      MOVE JP-RECORD-AREA         TO CLAIM-MASTER.                 EL131
02516                                                                   EL131
02517      PERFORM 2600-CREATE-MAINT-NOTE THRU 2600-EXIT.                  CL*34
02518                                                                      CL*34
02519      PERFORM 2100-MOVE-MSTR THRU 2100-EXIT.                       EL131
02520                                                                      CL*26
02521      IF WS-OPEN-CLOSE-SW = 'Y'                                       CL*34
02522        PERFORM 7800-CHECK-AUTO-ACTIVITY THRU 7800-EXIT               CL*26
02523          IF PI-REC-FOUND-SW = 'Y'                                    CL*34
02524            IF CL-ACTIVITY-CODE = 09                                  CL*34
02525              NEXT SENTENCE                                           CL*26
02526            ELSE                                                      CL*26
02527              IF WS-RESET-SW = 'Y'                                    CL*34
02528                IF CL-CLAIM-STATUS = 'C'                              CL*34
02529                  MOVE 07             TO  CL-ACTIVITY-CODE            CL*26
02530                  MOVE SAVE-BIN-DATE  TO  CL-ACTIVITY-MAINT-DT        CL*26
02531                  MOVE 'CLOS'         TO  CL-ACTIVITY-MAINT-TYPE      CL*26
02532                ELSE                                                  CL*26
02533                  MOVE 08             TO  CL-ACTIVITY-CODE            CL*26
02534                  MOVE SAVE-BIN-DATE  TO  CL-ACTIVITY-MAINT-DT        CL*26
02535                  MOVE 'OPEN'         TO  CL-ACTIVITY-MAINT-TYPE.     CL*26
02536                                                                   EL131
02537      EXEC CICS HANDLE CONDITION                                   EL131
02538           DUPKEY    (2030-EXIT)                                      CL*27
02539      END-EXEC.                                                       CL*27
02540                                                                   EL131
02541 ** POPULATE THE CREDIT-CARD NO WITH THE CERT NO.                     CL*33
02542 *    MOVE CL-CERT-NO             TO CL-CCN-A5.                       CL*39
02826                                                                   EL131
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
CIDMOD         MOVE 'Y'                TO CL-YESNOSW
CIDMOD     END-IF.
02543                                                                      CL*33
02544      EXEC CICS WRITE                                              EL131
02545           DATASET   (CLMS-FILE-ID)                                EL131
02546           RIDFLD    (MSTR-KEY)                                    EL131
02547           FROM      (CLAIM-MASTER)                                EL131
02548      END-EXEC.                                                    EL131
02549                                                                   EL131
02550  2030-EXIT.                                                       EL131
02551      EXIT.                                                        EL131
02552                                                                   EL131
02553      EJECT                                                        EL131
02554  2040-NORMAL-UPDATE.                                              EL131
02555      EXEC CICS READ UPDATE                                        EL131
02556           DATASET  (CERT-FILE-ID)                                 EL131
02557           RIDFLD   (CERT-KEY)                                     EL131
02558           SET      (ADDRESS OF CERTIFICATE-MASTER)                   CL*32
02559      END-EXEC.                                                    EL131
02560                                                                   EL131
02561      PERFORM 2200-MOVE-CERT THRU 2200-EXIT.                       EL131
02562                                                                   EL131
02563      EXEC CICS HANDLE CONDITION                                   EL131
02564           DUPKEY   (2040-EXIT)                                       CL*27
02565      END-EXEC.                                                    EL131
02566                                                                   EL131
02567      EXEC CICS REWRITE                                            EL131
02568           DATASET  (CERT-FILE-ID)                                 EL131
02569           FROM     (CERTIFICATE-MASTER)                           EL131
02570      END-EXEC.                                                    EL131
02571                                                                   EL131
02572  2040-EXIT.                                                       EL131
02573      EXIT.                                                        EL131
02574                                                                   EL131
02575  2050-KEY-UPDATE.                                                 EL131
02576      EXEC CICS READ UPDATE                                        EL131
02577           DATASET  (CERT-FILE-ID)                                 EL131
02578           RIDFLD   (CERT-KEY)                                     EL131
02579           SET      (ADDRESS OF CERTIFICATE-MASTER)                   CL*32
02580      END-EXEC.                                                       CL*34
02581                                                                   EL131
02582      MOVE CERTIFICATE-MASTER     TO JP-RECORD-AREA.               EL131
02583                                                                   EL131
02584      EXEC CICS DELETE                                             EL131
02585           DATASET   (CERT-FILE-ID)                                EL131
02586      END-EXEC.                                                       CL*34
02587                                                                   EL131
02588      EXEC CICS GETMAIN                                            EL131
02589           SET       (ADDRESS OF CERTIFICATE-MASTER)                  CL*32
02590           LENGTH    (CERT-LENGTH)                                 EL131
02591           INITIMG   (GETMAIN-SPACE)                               EL131
02592      END-EXEC.                                                    EL131
02593                                                                   EL131
02594      MOVE JP-RECORD-AREA         TO CERTIFICATE-MASTER.           EL131
02595                                                                   EL131
02596      PERFORM 2200-MOVE-CERT THRU 2200-EXIT.                       EL131
02597                                                                   EL131
02598      EXEC CICS HANDLE CONDITION                                   EL131
02599           DUPKEY (2050-EXIT)                                         CL*27
02600      END-EXEC.                                                    EL131
02601                                                                   EL131
02602      EXEC CICS WRITE                                              EL131
02603           DATASET  (CERT-FILE-ID)                                 EL131
02604           RIDFLD   (CERT-KEY)                                     EL131
02605           FROM     (CERTIFICATE-MASTER)                           EL131
02606      END-EXEC.                                                    EL131
02607                                                                   EL131
02608  2050-EXIT.                                                       EL131
02609      EXIT.                                                        EL131
02610      EJECT                                                        EL131
02611  2060-CHANGE-CERT.                                                EL131
02612      MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.            EL131
02613      MOVE PI-CARRIER             TO CERT-CARRIER.                 EL131
02614      MOVE PI-GROUPING            TO CERT-GROUP.                   EL131
02615      MOVE PI-STATE               TO CERT-STATE.                   EL131
02616      MOVE PI-ACCOUNT             TO CERT-ACCOUNT.                 EL131
02617      MOVE PI-CERT-EFF-DT         TO CERT-DATE.                    EL131
02618      MOVE PI-CERT-NO             TO CERT-CERT.                    EL131
02619      IF CERTEFFI GREATER THAN LOW-VALUES                          EL131
02620          MOVE HOLD-EFF           TO CERT-DATE.                    EL131
02621      IF CERTACTI GREATER THAN LOW-VALUES                          EL131
02622          MOVE CERTACTI           TO CERT-ACCOUNT.                 EL131
02623      IF CERTSTI GREATER THAN LOW-VALUES                           EL131
02624          MOVE CERTSTI            TO CERT-STATE.                   EL131
02625      IF CERTCARI GREATER THAN LOW-VALUES                          EL131
02626          MOVE CERTCARI           TO CERT-CARRIER.                 EL131
02627      IF CERTGRPI GREATER THAN LOW-VALUES                          EL131
02628          MOVE CERTGRPI           TO CERT-GROUP.                   EL131
02629      IF CERTI GREATER THAN LOW-VALUES                             EL131
02630          MOVE CERTI              TO CERT-CERT-PRIME.              EL131
02631      IF SUFXI GREATER THAN LOW-VALUES                             EL131
02632          MOVE SUFXI              TO CERT-CERT-SUFX.               EL131
02633                                                                   EL131
02634      EXEC CICS HANDLE CONDITION                                   EL131
02635           NOTFND   (2060-CERT-NOTFND)                             EL131
02636      END-EXEC.                                                    EL131
02637                                                                   EL131
02638      EXEC CICS READ UPDATE                                        EL131
02639           DATASET  (CERT-FILE-ID)                                 EL131
02640           RIDFLD   (CERT-KEY)                                     EL131
02641           SET      (ADDRESS OF CERTIFICATE-MASTER)                   CL*32
02642      END-EXEC.                                                    EL131
02643                                                                   EL131
02644      ADD 1                       TO CM-CLAIM-ATTACHED-COUNT.      EL131
02645                                                                   EL131
02646      IF CM-CLAIM-INTERFACE-SW = SPACES                            EL131
02647         MOVE '1'   TO CM-CLAIM-INTERFACE-SW PI-CERT-SWITCH.       EL131
02648                                                                   EL131
02649      EXEC CICS HANDLE CONDITION                                   EL131
02650           DUPKEY   (2060-DUPKEY)                                  EL131
02651      END-EXEC.                                                    EL131
02652                                                                   EL131
02653      EXEC CICS REWRITE                                            EL131
02654           DATASET  (CERT-FILE-ID)                                 EL131
02655           FROM     (CERTIFICATE-MASTER)                           EL131
02656      END-EXEC.                                                    EL131
02657                                                                   EL131
02658  2060-DUPKEY.                                                     EL131
02659                                                                   EL131
02660      PERFORM 2070-CHECK-OLD THRU 2070-EXIT.                          CL*34
02661                                                                   EL131
02662  2060-CHANGE-TRLR-ACTQ.                                           EL131
02663      IF SCREEN-ERROR                                              EL131
02664          GO TO 2060-EXIT.                                         EL131
02665                                                                   EL131
02666      IF CERTI    GREATER LOW-VALUES OR                            EL131
02667         SUFXI    GREATER LOW-VALUES OR                            EL131
02668         CERTCARI GREATER LOW-VALUES                               EL131
02669          PERFORM 2400-UPDATE-TRLR THRU 2400-EXIT                     CL*34
02670          PERFORM 2430-UPDATE-ACTQ THRU 2430-EXIT.                    CL*34
02671                                                                   EL131
02672      MOVE SPACES                 TO CERT-KEY-SWITCH                  CL**8
02673                                     CERT-SWITCH                      CL**8
02674                                     TRLR-SWITCH                      CL**8
02675                                     NINETY-TRLR-SWITCH.              CL**8
02676      MOVE 'X' TO MSTR-KEY-SWITCH MSTR-SWITCH.                     EL131
02677      GO TO 2060-EXIT.                                             EL131
02678                                                                   EL131
02679  2060-CERT-NOTFND.                                                EL131
02680      PERFORM 2070-CHECK-OLD THRU 2070-EXIT.                          CL*34
02681                                                                   EL131
02682      IF WS-REC-FOUND-SW = 'N'                                        CL*34
02683          MOVE ER-0206            TO  EMI-ERROR                       CL*27
02684          MOVE -1                 TO  MAINTL                          CL*27
02685          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*27
02686          EXEC CICS UNLOCK                                            CL*27
02687              DATASET   (CLMS-FILE-ID)                                CL*27
02688          END-EXEC                                                    CL*27
02689          GO TO 8110-SEND-DATA.                                       CL*27
02690                                                                      CL*27
02691      PERFORM 2080-CREATE-CERT THRU 2080-EXIT.                        CL*34
02692                                                                   EL131
02693      GO TO 2060-CHANGE-TRLR-ACTQ.                                 EL131
02694                                                                   EL131
02695  2060-EXIT.                                                       EL131
02696      EXIT.                                                        EL131
02697      EJECT                                                        EL131
02698  2070-CHECK-OLD.                                                  EL131
02699                                                                      CL*27
02700      EXEC CICS HANDLE CONDITION                                      CL*27
02701          NOTFND   (2070-NOT-FOUND)                                   CL*27
02702      END-EXEC.                                                       CL*27
02703                                                                      CL*27
02704      MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.            EL131
02705      MOVE PI-CARRIER             TO CERT-CARRIER.                 EL131
02706      MOVE PI-GROUPING            TO CERT-GROUP.                   EL131
02707      MOVE PI-STATE               TO CERT-STATE.                   EL131
02708      MOVE PI-ACCOUNT             TO CERT-ACCOUNT.                 EL131
02709      MOVE PI-CERT-EFF-DT         TO CERT-DATE.                    EL131
02710      MOVE PI-CERT-NO             TO CERT-CERT.                    EL131
02711                                                                   EL131
02712      EXEC CICS READ UPDATE                                        EL131
02713           DATASET  (CERT-FILE-ID)                                 EL131
02714           RIDFLD   (CERT-KEY)                                     EL131
02715           SET      (ADDRESS OF CERTIFICATE-MASTER)                   CL*32
02716      END-EXEC.                                                    EL131
02717                                                                   EL131
02718      MOVE 'Y'                    TO WS-REC-FOUND-SW.                 CL*27
02719      MOVE CERTIFICATE-MASTER     TO SAVE-RECORD.                  EL131
02720      SUBTRACT 1                  FROM CM-CLAIM-ATTACHED-COUNT.    EL131
02721                                                                   EL131
02722      IF CM-CLAIM-ATTACHED-COUNT LESS THAN ZERO                    EL131
02723          MOVE ZERO               TO CM-CLAIM-ATTACHED-COUNT.      EL131
02724                                                                   EL131
02725      IF CM-CLAIM-ATTACHED-COUNT = ZERO                            EL131
02726        AND                                                        EL131
02727         CERT-WAS-CREATED-FOR-CLAIM                                EL131
02728          MOVE CERTIFICATE-MASTER TO JP-RECORD-AREA                EL131
02729          EXEC CICS DELETE                                         EL131
02730               DATASET   (CERT-FILE-ID)                            EL131
02731          END-EXEC                                                 EL131
02732          GO TO 2070-EXIT.                                         EL131
02733                                                                   EL131
02734      IF CM-CLAIM-ATTACHED-COUNT = ZERO                            EL131
02735          MOVE SPACE TO CM-CLAIM-INTERFACE-SW PI-CERT-SWITCH.      EL131
02736                                                                   EL131
02737      EXEC CICS HANDLE CONDITION                                   EL131
02738           DUPKEY   (2070-EXIT)                                       CL*27
02739      END-EXEC.                                                    EL131
02740                                                                   EL131
02741      EXEC CICS REWRITE                                            EL131
02742           DATASET  (CERT-FILE-ID)                                 EL131
02743           FROM     (CERTIFICATE-MASTER)                           EL131
02744      END-EXEC.                                                    EL131
02745                                                                   EL131
02746      GO TO 2070-EXIT.                                                CL*27
02747                                                                      CL*27
02748  2070-NOT-FOUND.                                                     CL*27
02749      MOVE 'N'                        TO  WS-REC-FOUND-SW.            CL*27
02750                                                                      CL*27
02751  2070-EXIT.                                                       EL131
02752      EXIT.                                                        EL131
02753      EJECT                                                        EL131
02754  2080-CREATE-CERT.                                                EL131
02755      EXEC CICS GETMAIN                                            EL131
02756           SET       (ADDRESS OF CERTIFICATE-MASTER)                  CL*32
02757           LENGTH    (CERT-LENGTH)                                 EL131
02758           INITIMG   (GETMAIN-SPACE)                               EL131
02759      END-EXEC.                                                    EL131
02760                                                                   EL131
02761      MOVE SAVE-RECORD            TO CERTIFICATE-MASTER.           EL131
02762                                                                   EL131
02763      IF CERTEFFI GREATER THAN LOW-VALUES                          EL131
02764          MOVE HOLD-EFF           TO CERT-DATE CM-CERT-EFF-DT         CL**7
02765                                     DC-BIN-DATE-1                    CL**7
02766          MOVE '6'                TO DC-OPTION-CODE                   CL**7
02767          MOVE +1                 TO DC-ELAPSED-MONTHS                CL**7
02768          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL**7
02769          IF NO-CONVERSION-ERROR                                      CL**7
02770              MOVE DC-BIN-DATE-2  TO CM-LOAN-1ST-PMT-DT.              CL**7
02771                                                                      CL**7
02772      IF CERTACTI GREATER THAN LOW-VALUES                          EL131
02773          MOVE CERTACTI           TO CERT-ACCOUNT CM-ACCOUNT.      EL131
02774      IF CERTSTI GREATER THAN LOW-VALUES                           EL131
02775          MOVE CERTSTI            TO CERT-STATE CM-STATE.          EL131
02776      IF CERTCARI GREATER THAN LOW-VALUES                          EL131
02777          MOVE CERTCARI           TO CERT-CARRIER CM-CARRIER.      EL131
02778      IF CERTGRPI GREATER THAN LOW-VALUES                          EL131
02779          MOVE CERTGRPI           TO CERT-GROUP CM-GROUPING.       EL131
02780      IF CERTI GREATER THAN LOW-VALUES                             EL131
02781          MOVE CERTI              TO CERT-CERT-PRIME               EL131
02782                                     CM-CERT-PRIME                 EL131
02783          MOVE CM-CERT-NO         TO CM-CERT-NO-A4.                EL131
02784                                                                   EL131
02785      IF SUFXI GREATER THAN LOW-VALUES                             EL131
02786          MOVE SUFXI              TO CERT-CERT-SUFX                EL131
02787                                     CM-CERT-SFX                   EL131
02788          MOVE CM-CERT-NO         TO CM-CERT-NO-A4.                EL131
02789                                                                   EL131
02790      PERFORM 2200-MOVE-CERT THRU 2200-EXIT.                       EL131
02791                                                                   EL131
02792      MOVE LOW-VALUES             TO CM-LAST-MONTH-END.               CL*28
02793                                                                      CL*28
02794      MOVE SPACES                 TO CM-ENTRY-BATCH                   CL*28
02795                                     CM-LF-EXIT-BATCH                 CL*28
02796                                     CM-AH-EXIT-BATCH                 CL*28
02797                                     CM-CREDIT-INTERFACE-SW-1         CL*28
02798                                     CM-CREDIT-INTERFACE-SW-2.        CL*28
02799                                                                      CL*28
02800      IF CM-LF-CURRENT-STATUS = '2' OR '3' OR '4' OR '5' OR           CL*34
02801                                '9' OR 'D' OR 'V'                     CL*34
02802          MOVE '1'                TO CM-LF-CURRENT-STATUS.            CL*28
02803                                                                      CL*28
02804      IF CM-AH-CURRENT-STATUS = '2' OR '3' OR '4' OR '5' OR           CL*34
02805                                '9' OR 'D' OR 'V'                     CL*34
02806          MOVE '1'                TO CM-AH-CURRENT-STATUS.            CL*28
02807                                                                      CL*28
02808      MOVE '2'                    TO CM-CLAIM-INTERFACE-SW         EL131
02809                                     CL-CERT-ORIGIN PI-CERT-SWITCH.EL131
02810      MOVE 1                      TO CM-CLAIM-ATTACHED-COUNT.      EL131
02811                                                                   EL131
02812      EXEC CICS HANDLE CONDITION                                   EL131
02813           DUPKEY (2080-EXIT)                                         CL*27
02814      END-EXEC.                                                    EL131
02815                                                                   EL131
02816      EXEC CICS WRITE                                              EL131
02817           DATASET  (CERT-FILE-ID)                                 EL131
02818           RIDFLD   (CERT-KEY)                                     EL131
02819           FROM     (CERTIFICATE-MASTER)                           EL131
02820      END-EXEC.                                                    EL131
02821                                                                   EL131
02822  2080-EXIT.                                                       EL131
02823      EXIT.                                                        EL131
02824      EJECT                                                        EL131
02825  2100-MOVE-MSTR.                                                  EL131
02827      IF TYPEI GREATER THAN LOW-VALUES                                CL*21
02828          MOVE TYPEI              TO CL-CLAIM-TYPE.                   CL*21
02829                                                                      CL**8
02830      IF STATUSI = LOW-VALUES                                         CL*34
02831          GO TO 2100-NO-STATUS-CHANGE.                                CL**8
02832                                                                      CL**8
02833      IF STATUSI = 'OPEN' OR 'O'                                      CL*34
02834          IF CL-CLAIM-STATUS = 'O'                                    CL*34
02835              MOVE SPACES         TO TRLR-SWITCH                      CL**8
02836                  GO TO 2100-NO-STATUS-CHANGE.                        CL**8
02837                                                                      CL**8
02838      IF STATUSI = 'CLOSED' OR 'C'                                    CL*34
02839          IF CL-CLAIM-STATUS = 'C'                                    CL*34
02840              MOVE SPACES         TO TRLR-SWITCH                      CL**8
02841                  GO TO 2100-NO-STATUS-CHANGE.                        CL**8
02842                                                                      CL*34
02843      IF PI-COMPANY-ID = 'DMD'                                        CL*34
02844         IF STATUSI = 'OPEN' OR 'O'                                   CL*34
02845            IF CL-LAST-CLOSE-REASON = 'C' OR 'E'                      CL*34
02846               MOVE ZERO       TO STATUSL                             CL*34
02847               MOVE LOW-VALUES TO STATUSI                             CL*34
02848               GO TO 2100-NO-STATUS-CHANGE.                           CL*34
02849                                                                      CL*34
02850      IF PI-COMPANY-ID = 'DMD'                                        CL*34
02851          PERFORM 8000-CREATE-DMO-REC THRU 8000-EXIT.                 CL*34
02852                                                                      CL**8
02853      MOVE STATUSI                TO CL-CLAIM-STATUS.                 CL**8
02854                                                                      CL**8
02855  2100-NO-STATUS-CHANGE.                                              CL**8
02856                                                                      CL*38
02857      IF CCNOI GREATER THAN LOW-VALUES                                CL*38
02858          MOVE CCNOI              TO CL-CCN.                          CL*38
02859                                                                   EL131
02860      IF PROCI GREATER THAN LOW-VALUES                             EL131
02861          MOVE PROCI              TO CL-PROCESSOR-ID.              EL131
02862                                                                   EL131
052113     if (accswi > low-values)
052113        and (accswi = 'Y' OR 'N')
052113        move accswi              to cl-accident-claim-sw
052113     end-if

           if benperi > low-values
              and benperi numeric
              move benperi             to cl-benefit-period
           end-if

           if instypei > low-values
              move instypei            to cl-insured-type
           end-if

02863      IF SEXI GREATER THAN LOW-VALUES                              EL131
02864          MOVE SEXI               TO CL-INSURED-SEX-CD.            EL131
02865                                                                   EL131
02866      IF BIRTHI GREATER THAN LOW-VALUES                            EL131
02867          MOVE HOLD-BIRTH         TO CL-INSURED-BIRTH-DT.          EL131
02868                                                                   EL131
02869      IF SOCIALI GREATER THAN LOW-VALUES                           EL131
02870        AND                                                        EL131
02871         SOCIALI = SPACES                                          EL131
02872          MOVE CL-CERT-STATE      TO CL-SSN-STATE                  EL131
02873          MOVE CL-CERT-ACCOUNT-PRIME   TO CL-SSN-ACCOUNT           EL131
02874          MOVE CL-INSURED-LAST-NAME   TO CL-SSN-LN3                EL131
02875      ELSE                                                         EL131
02876          IF SOCIALI GREATER THAN LOW-VALUES                       EL131
02877              MOVE SOCIALI        TO CL-SOC-SEC-NO.                EL131
02878                                                                   EL131
02879      IF OCCI GREATER THAN LOW-VALUES                              EL131
02880          MOVE OCCI               TO CL-INSURED-OCC-CD.            EL131
02881                                                                   EL131
02882      IF BENEL GREATER THAN ZERO                                   EL131
02883          MOVE BENEI              TO CL-BENEFICIARY.                  CL**8
02884                                                                   EL131
02885      IF DIAGI GREATER THAN LOW-VALUES                             EL131
02886          MOVE DIAGI              TO WS-DIAGNOSIS.                    CL*39
040814
040814     IF ICD1I GREATER THAN LOW-VALUES
040814         MOVE ICD1I              TO WS-ICD-CODE-1
040814     END-IF.
040814
040814     IF ICD2I GREATER THAN LOW-VALUES
040814         MOVE ICD2I              TO WS-ICD-CODE-2
040814     END-IF.
02887                                                                   EL131
02888      IF PREMTYPI GREATER THAN LOW-VALUES                          EL131
02889          MOVE PREMTYPI           TO CL-CLAIM-PREM-TYPE.           EL131
02890                                                                   EL131
040814*    IF CAUSEI GREATER THAN LOW-VALUES                            EL131
040814*        MOVE CAUSEI             TO CL-CAUSE-CD.                  EL131
02893                                                                   EL131
040814*    IF ENDI GREATER THAN LOW-VALUES                              EL131
040814*        MOVE HOLD-END           TO CL-EST-END-OF-DISAB-DT.       EL131
02896                                                                   EL131
02897      IF PDTHRUI GREATER THAN LOW-VALUES                           EL131
02898         MOVE HOLD-PDTHRU         TO CL-PAID-THRU-DT.              EL131
02899                                                                   EL131
02900      IF PDAMTL GREATER THAN +0                                       CL*32
02901         MOVE HOLD-PDAMT          TO CL-TOTAL-PAID-AMT.            EL131
02902                                                                   EL131
02903      IF NODAYSI GREATER THAN LOW-VALUES                           EL131
02904         MOVE HOLD-NODAYS         TO CL-NO-OF-DAYS-PAID.           EL131
02905                                                                   EL131
02906      IF NOPMTSI GREATER THAN LOW-VALUES                           EL131
02907         MOVE HOLD-NOPMTS         TO CL-NO-OF-PMTS-MADE.           EL131
02908                                                                   EL131
02909      IF FORMTYPI GREATER THAN LOW-VALUES                             CL*14
02910         MOVE FORMTYPI            TO CL-PROG-FORM-TYPE.               CL*14
02911                                                                      CL*14
02912      IF INCI GREATER THAN LOW-VALUES                              EL131
02913         MOVE HOLD-INCUR          TO CL-INCURRED-DT.               EL131
02914                                                                   EL131
02915      IF REPI GREATER THAN LOW-VALUES                              EL131
02916         MOVE HOLD-REPORTED       TO CL-REPORTED-DT.               EL131
02917                                                                      CL*14
02918      IF ADDONDTI GREATER THAN LOW-VALUES                             CL*14
02919         MOVE HOLD-ADDON          TO CL-LAST-ADD-ON-DT.               CL*14
02920                                                                   EL131
02921      IF PRICDI GREATER THAN LOW-VALUES                            EL131
02922          MOVE PRICDI             TO CL-PRIORITY-CD.               EL131
02923                                                                      CL**8
052113     IF CRITPL  > ZEROS
052113        MOVE CRITPI              TO CL-CRITICAL-PERIOD
052113     END-IF
052113
081817     IF EXTENSL  > ZEROS
081817        MOVE EXTENSI             TO CL-NO-OF-EXTENSIONS
081817     END-IF

052113*    IF CRITPTL > ZEROS
052113*       MOVE CRITPTI             TO CL-CRIT-PER-RECURRENT
052113*    END-IF
052113
052113*    IF RTWMOSL > ZEROS
052113*       MOVE RTWMOSI             TO CL-CRIT-PER-RTW-MOS
052113*    END-IF

02924      IF FILETOI GREATER THAN LOW-VALUES                              CL**8
02925          MOVE FILETOI            TO CL-FILE-LOCATION.                CL**8
02926                                                                   EL131
02927      IF SUPVI GREATER THAN LOW-VALUES                             EL131
02928          MOVE SUPVI              TO CL-SUPV-ATTN-CD.              EL131
02929                                                                   EL131
02930      IF MLNAMEI GREATER THAN LOW-VALUES                           EL131
02931          MOVE MLNAMEI            TO CL-INSURED-LAST-NAME.         EL131
02932                                                                   EL131
02933      IF MFNAMEI GREATER THAN LOW-VALUES                           EL131
02934          MOVE MFNAMEI            TO CL-INSURED-1ST-NAME.          EL131
02935                                                                   EL131
02936      IF MMINITI GREATER THAN LOW-VALUES                           EL131
02937          MOVE MMINITI            TO CL-INSURED-MID-INIT.          EL131
02938                                                                   EL131
02939      IF CERTEFFI GREATER THAN LOW-VALUES                          EL131
02940          MOVE HOLD-EFF           TO CL-CERT-EFF-DT.               EL131
02941                                                                   EL131
02942      IF CERTACTI GREATER THAN LOW-VALUES                          EL131
02943          MOVE CERTACTI           TO CL-CERT-ACCOUNT.              EL131
02944                                                                   EL131
02945      IF CERTSTI GREATER THAN LOW-VALUES                           EL131
02946          MOVE CERTSTI            TO CL-CERT-STATE.                EL131
02947                                                                   EL131
02948      IF CERTCARI GREATER THAN LOW-VALUES                          EL131
02949          MOVE CERTCARI           TO CL-CERT-CARRIER CARRIER-CODE  EL131
02950                                     CL-CARRIER.                   EL131
02951                                                                   EL131
02952      IF CERTGRPI GREATER THAN LOW-VALUES                          EL131
02953          MOVE CERTGRPI           TO CL-CERT-GROUPING.             EL131
02954                                                                   EL131
02955      IF CERTI GREATER THAN LOW-VALUES                             EL131
02956          MOVE CERTI              TO CERT-NO-PRIME.                EL131
02957                                                                   EL131
02958      IF SUFXI GREATER THAN LOW-VALUES                             EL131
02959          MOVE SUFXI              TO CERT-NO-SUFX.                 EL131
02960                                                                   EL131
02961      MOVE CERT-NO                TO CL-CERT-NO                    EL131
02962                                     CL-CERT-NO-A4.                EL131
02963                                                                   EL131
02964      IF PI-COMPANY-ID = 'FLA'                                        CL*34
02965           NEXT SENTENCE                                              CL*23
02966      ELSE                                                            CL*23
02967          MOVE SAVE-BIN-DATE      TO CL-LAST-MAINT-DT                 CL*34
02968          MOVE PI-PROCESSOR-ID    TO CL-LAST-MAINT-USER               CL*23
02969          MOVE EIBTIME            TO CL-LAST-MAINT-HHMMSS             CL*23
02970          MOVE '3'                TO CL-LAST-MAINT-TYPE.              CL*23
02971                                                                   EL131
02972      IF TRLR-UPDATE-REQUIRED                                      EL131
02973         MOVE CL-CONTROL-PRIMARY TO TRLR-KEY                       EL131
02974         MOVE ZEROS              TO TRLR-SEQ-NO                    EL131
02975         EXEC CICS READ                                            EL131
02976              DATASET   (TRLR-FILE-ID)                             EL131
02977              RIDFLD    (TRLR-KEY)                                 EL131
02978              SET       (ADDRESS OF ACTIVITY-TRAILERS)                CL*32
02979         END-EXEC                                                  EL131
02980         IF AT-TRAILER-TYPE = '1'                                  EL131
02981            MOVE +1 TO MISC-SUB                                    EL131
02982            PERFORM 2100-BUMP-OPEN-CLOSE-HIST UNTIL                EL131
02983            MISC-SUB GREATER THAN +6 OR                            EL131
02984            AT-OPEN-CLOSE-TYPE (MISC-SUB) = SPACES                 EL131
02985            IF MISC-SUB GREATER THAN +1                            EL131
02986               SUBTRACT +1 FROM MISC-SUB                           EL131
02987               IF AT-OPEN-CLOSE-TYPE (MISC-SUB) = STATUSI          EL131
02988                  GO TO 2100-BYPASS-LAST-UPDATE.                   EL131
02989                                                                   EL131
02990      IF TRLR-UPDATE-REQUIRED AND                                  EL131
02991         CLAIM-IS-OPEN                                             EL131
02992          MOVE SAVE-BIN-DATE      TO CL-LAST-REOPEN-DT                CL*34
02993      ELSE                                                         EL131
02994          IF TRLR-UPDATE-REQUIRED AND                              EL131
02995             CLAIM-IS-CLOSED                                       EL131
02996              MOVE SAVE-BIN-DATE  TO CL-LAST-CLOSE-DT                 CL*34
02997              MOVE '4'            TO CL-LAST-CLOSE-REASON.         EL131
02998                                                                   EL131
02999  2100-BYPASS-LAST-UPDATE.                                         EL131
03000      MOVE EMI-FORCABLE-CTR       TO CL-FORCEABLE-ERROR-CNT.       EL131
03001      MOVE EMI-FATAL-CTR          TO CL-FATAL-ERROR-CNT.           EL131
03002                                                                   EL131
03003      GO TO 2100-EXIT.                                             EL131
03004                                                                   EL131
03005  2100-BUMP-OPEN-CLOSE-HIST.                                       EL131
03006      ADD +1                      TO MISC-SUB.                     EL131
03007                                                                   EL131
03008  2100-EXIT.                                                       EL131
03009      EXIT.                                                        EL131
03010      EJECT                                                        EL131

052113 2120-check-pdef.

052113     move +0                     to s1
052113     MOVE ZEROS                  TO WS-MONTHS-BETWEEN
052113     move spaces                 to ws-dcc-error-line
052113     PERFORM 3997-GET-ERPDEF  THRU 3997-EXIT
052113     IF ERPDEF-FOUND
052113        perform 2130-check-trlr  thru 2130-exit
052113        move +1                  to s1
052113*       if cl-accident-claim-sw = ' '
052113*          move er-1655             to ws-error-no (s1)
052113*          add +1 to s1
052113*       end-if

052113        MOVE CL-CERT-EFF-DT      TO DC-BIN-DATE-1

052113        MOVE cl-incurred-dt      TO DC-BIN-DATE-2
              if (hold-incur > low-values)
                 and (incl > zeros)
                 and (hold-incur not = cl-incurred-dt)
                 move hold-incur       to dc-bin-date-2
              end-if
052113        MOVE '1'                 TO DC-OPTION-CODE
052113        MOVE +0                  TO DC-ELAPSED-MONTHS
052113                                    DC-ELAPSED-DAYS
052113        PERFORM 9800-CONVERT-DATE
052113                                 THRU 9800-EXIT
052113        IF NO-CONVERSION-ERROR
052113           MOVE DC-ELAPSED-MONTHS
052113                                 TO WS-MONTHS-BETWEEN
052113           IF DC-ELAPSED-DAYS > 1
052113              ADD 1 TO WS-MONTHS-BETWEEN
052113           END-IF
052113        ELSE
                 display ' dte conv error ' dc-error-code
052113           MOVE ZEROS            TO WS-MONTHS-BETWEEN
052113        END-IF
052113        IF (WS-EXCL-PERIOD = ZEROS)
052113           OR (WS-MONTHS-BETWEEN = ZEROS)
052113           CONTINUE
052113        ELSE
052113           IF WS-MONTHS-BETWEEN  <= WS-EXCL-PERIOD
052113              MOVE ER-1651       TO ws-error-no (s1)
052113              add +1             to s1
052113           END-IF
052113        END-IF

              display ' mos diff ' ws-months-between ' '
                 ws-pre-exsist
052113        IF (WS-pre-exsist = ZEROS)
052113           OR (WS-MONTHS-BETWEEN = ZEROS)
052113           CONTINUE
052113        ELSE
052113           IF WS-months-between <= WS-pre-exsist  
052113              MOVE ER-1677       TO ws-error-no (s1)
052113              add +1             to s1
052113           END-IF
052113        END-IF



052113        IF (WS-COV-ENDS = ZEROS)
052113           OR (WS-MONTHS-BETWEEN = ZEROS)
052113           CONTINUE
052113        ELSE
052113           IF WS-MONTHS-BETWEEN > WS-COV-ENDS
052113              MOVE -1            TO MAINTL
052113              MOVE ER-1653       TO ws-error-no (s1)
052113              add +1 to s1
052113           END-IF
052113        END-IF
052113        IF (WS-ACC-PERIOD = ZEROS)
052113           OR (WS-MONTHS-BETWEEN = ZEROS)
052113           CONTINUE
052113        ELSE
052113           IF (WS-MONTHS-BETWEEN <= WS-ACC-PERIOD)
052113              and (accswi = spaces or low-values)
052113*             and (cl-accident-claim-sw = ' ')
052113              move er-1655       to ws-error-no (s1)
052113              add +1             to s1
052113           end-if
052113           IF WS-MONTHS-BETWEEN <= WS-ACC-PERIOD
052113*             if (cl-accident-claim-sw not = 'Y')
052113              if (accswi not = 'Y')
052113                 MOVE ER-1652    TO ws-error-no (s1)
052113              else
052113                 move er-1662    to ws-error-no (s1)
052113              end-if
052113              MOVE -1            TO MAINTL
052113              add +1             to s1
052113           END-IF
052113        END-IF
              if cl-claim-type  = 'I'
                 move er-1661          to ws-error-no (s1)
                 add +1                to s1
              end-if
052113        perform 2130-check-trlr  thru 2130-exit
052113     END-IF
052113
052113     .
052113 2120-exit.
052113     exit.
052113
052113 2130-check-trlr.
052113
052113     evaluate true
052113        when s1 = +0
052113           move cl-control-primary
052113                                 to trlr-key
052113           MOVE +95              TO TRLR-SEQ-NO    
052113           EXEC CICS READ
052113              update
052113              DATASET  ('ELTRLR')
052113              SET      (ADDRESS OF ACTIVITY-TRAILERS)
052113              RIDFLD   (TRLR-KEY)
052113              RESP     (WS-RESPONSE)
052113           END-EXEC
052113           if ws-resp-normal
052113              move spaces        to at-info-line-1
052113           end-if
052113        when s1 = +1
052113           if ws-resp-normal
052113              exec cics rewrite
052113                 dataset   ('ELTRLR')
052113                 from      (activity-trailers)
052113              end-exec
052113           end-if
052113        when s1 > +1
052113           if ws-resp-normal
052113              move ws-dcc-error-line
052113                                 to at-info-line-1
052113              exec cics rewrite
052113                 dataset   ('ELTRLR')
052113                 from      (activity-trailers)
052113              end-exec
052113           else
052113              EXEC CICS GETMAIN
052113                 SET      (ADDRESS OF ACTIVITY-TRAILERS)
052113                 LENGTH   (TRLR-LENGTH)
052113                 INITIMG  (GETMAIN-SPACE)
052113              END-EXEC
052113              move 'AT'          to at-record-id
052113              move cl-control-primary
052113                                 to at-control-primary
052113              move +95           to at-sequence-no
052113              move '6'           to at-trailer-type
052113              move ws-dcc-error-line
052113                                 to at-info-line-1
052113              move 'E'           to at-info-trailer-type
052113              move save-bin-date to at-recorded-dt
052113                                    at-gen-info-last-maint-dt
052113              move pi-processor-id
052113                                 to at-recorded-by
052113                                    at-gen-info-last-updated-by
052113              move eibtime       to at-last-maint-hhmmss
052113              exec cics write
052113                 dataset   ('ELTRLR')
052113                 from      (activity-trailers)
052113                 ridfld    (trlr-key)
052113              end-exec
052113              perform varying s1 from +1 by +1 until
052113                 ws-error-no (s1) = spaces
052113                 move ws-error-no (s1)
052113                                 to emi-error
052113                 PERFORM 9900-ERROR-FORMAT
052113                                 THRU 9900-EXIT
052113              end-perform
052113           end-if
052113     end-evaluate
052113
052113     .
052113 2130-exit.
052113     exit.

       2140-update-elcrtt.

           move ' '                    to crtt-switch
052113     MOVE CERT-KEY               TO ELCRTT-KEY
052113     MOVE 'B'                    TO CTRLR-REC-TYPE
052113     EXEC CICS READ
052113        UPDATE
052113        DATASET   ('ELCRTT')
052113        RIDFLD    (ELCRTT-KEY)
052113        INTO      (CERTIFICATE-TRAILERS)
052113        RESP      (WS-RESPONSE)
052113     END-EXEC            
052113     IF WS-RESP-NORMAL
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or (cs-claim-no (s1) = cl-claim-no)
              end-perform
              if (s1 < +25)
                 if (benperl <> zeros)
                    and (cs-benefit-period (s1) not = benperi)
                    move benperi       to cs-benefit-period (s1)
                    set crtt-update    to true
                end-if
                if (instypel <> zeros)
                   and (instypei not = cs-insured-type (s1))
                   move instypei      to cs-insured-type (s1)
                   set crtt-update    to true
                end-if
                move cs-claim-type (s1) to ws-claim-type
                move cs-benefit-period (s1) to ws-ben-per
              end-if
              if crtt-update
                 perform 2150-accum    thru 2150-exit
052113           EXEC CICS REWRITE
052113              DATASET   ('ELCRTT')
052113              FROM      (CERTIFICATE-TRAILERS)
052113           END-EXEC
              else
                 exec cics unlock
                    dataset   ('ELCRTT')
                 end-exec
              end-if
           end-if

           .
       2140-exit.
           exit.
       2150-accum.

           if not erpdef-found
              perform 3997-get-erpdef  thru 3997-exit
           end-if

           if (erpdef-found)
              and (ws-max-moben not = zeros)
              and (ws-max-moben < cm-ah-benefit-amt)
              continue
           else
              move cm-ah-benefit-amt   to ws-max-moben
           end-if

           move cm-ah-orig-term        to ws-max-bens
           if cl-critical-period not = zeros and spaces
              move cl-critical-period  to ws-max-bens
           end-if

           move zeros to ws-tot-days-paid ws-tot-amt-paid
           perform varying s2 from +1 by +1 until
              (s2 > +24)
              or (cs-claim-no (s2) = spaces)
              if (cs-benefit-period (s2) = ws-ben-per)
                 and (cs-claim-type (s2) = ws-claim-type)
                 and (cs-insured-type (s2) = cl-insured-type)
      *          compute ws-tot-days-paid =
      *             ws-tot-days-paid + cs-days-paid (s2)
                 compute ws-tot-amt-paid =
                    ws-tot-amt-paid + cs-total-paid (s2)
              end-if
           end-perform
           if s2 < +25
              compute ws-pd-bens rounded =
                 ws-tot-amt-paid / ws-max-moben
              compute cs-remaining-bens (s1) =
                 ws-max-bens - ws-pd-bens
              if cs-remaining-bens (s1) < zeros
                 move zeros            to cs-remaining-bens (s1)
              end-if
           end-if

           .
       2150-exit.
           exit.

03011  2200-MOVE-CERT.                                                  EL131
03012      IF CRTLNMEI GREATER THAN LOW-VALUES                          EL131
03013          MOVE CRTLNMEI           TO CM-INSURED-LAST-NAME.         EL131
03014                                                                   EL131
03015      IF CRTFNMEI GREATER THAN LOW-VALUES                          EL131
03016          MOVE CRTFNMEI           TO CM-INSURED-FIRST-NAME         EL131
03017          MOVE CM-INSURED-1ST-INIT TO CM-INSURED-INITIAL1.         EL131
03018                                                                   EL131
03019      IF CRTINITI GREATER THAN LOW-VALUES                          EL131
03020          MOVE CRTINITI           TO CM-INSURED-INITIAL2.          EL131
03021                                                                   EL131
03022      IF CM-INSURED-INITIALS = SPACES                              EL131
03023          MOVE '**'               TO CM-INSURED-INITIALS.          EL131
03024                                                                   EL131
03025      IF ISSAGEI GREATER THAN LOW-VALUES                           EL131
03026          MOVE ISSAGEI            TO CM-INSURED-ISSUE-AGE.         EL131
03027                                                                   EL131
03028      IF JNTFNMEI GREATER THAN LOW-VALUES                          EL131
03029          MOVE JNTFNMEI           TO CM-JT-FIRST-NAME.             EL131
03030                                                                   EL131
03031      IF JNTLNMEI GREATER THAN LOW-VALUES                          EL131
03032          MOVE JNTLNMEI           TO CM-JT-LAST-NAME.              EL131
03033                                                                   EL131
03034      IF JNTINITI GREATER THAN LOW-VALUES                          EL131
03035          MOVE JNTINITI           TO CM-JT-INITIAL.                EL131
03036                                                                   EL131
03037      IF JNTAGEI GREATER THAN LOW-VALUES                           EL131
03038          MOVE JNTAGEI            TO CM-INSURED-JOINT-AGE.         EL131
03039                                                                   EL131
03040      IF ADDONDTI GREATER THAN LOW-VALUES                             CL*14
03041         MOVE HOLD-ADDON          TO CM-LAST-ADD-ON-DT.               CL*14
03042                                                                      CL*14
03043      IF LCVCDI GREATER THAN LOW-VALUES                               CL**8
03044         IF LCVCDI = SPACES OR ZEROS                                  CL**8
03045            MOVE ZEROS            TO CM-LF-BENEFIT-CD              EL131
03046         ELSE                                                      EL131
03047            MOVE LCVCDI           TO CM-LF-BENEFIT-CD.                CL**8
03048                                                                   EL131
03049      IF LCVOTRMI GREATER THAN LOW-VALUES                             CL**8
03050          MOVE LCVOTRMI           TO CM-LF-ORIG-TERM.                 CL**8
03051                                                                      CL*14
03052      IF LCVRATEL GREATER THAN +0                                     CL*32
03053          MOVE HOLD-LF-RATE       TO CM-LF-PREMIUM-RATE.              CL*14
03054                                                                   EL131
03055      IF LCVBENEL GREATER THAN +0                                     CL*32
03056          MOVE HOLD-LF-CV-BEN     TO CM-LF-BENEFIT-AMT.               CL**8
03057                                                                   EL131
03058      IF LCVFORMI GREATER THAN LOW-VALUES                             CL**8
03059          MOVE LCVFORMI           TO CM-POLICY-FORM-NO.               CL**8
03060                                                                   EL131
03061      IF LCVCNDTI = LOW-VALUES                                        CL**8
03062         GO TO 2210-SET-STATUS.                                    EL131
03063                                                                   EL131
03064      IF HOLD-LF-CV-CAN NOT = LOW-VALUES                              CL**8
03065         MOVE '8'                 TO CM-LF-CURRENT-STATUS          EL131
03066         MOVE HOLD-LF-CV-CAN      TO CM-LF-CANCEL-DT                  CL**8
03067         GO TO 2210-SET-STATUS.                                    EL131
03068                                                                   EL131
03069      IF CM-LF-CURRENT-STATUS = '7'                                EL131
03070         MOVE CM-LF-STATUS-AT-DEATH   TO CM-LF-CURRENT-STATUS      EL131
03071         MOVE SPACES              TO CM-LF-STATUS-AT-DEATH         EL131
03072         MOVE LOW-VALUES          TO CM-LF-DEATH-EXIT-DT           EL131
03073                                     CM-LF-DEATH-DT                EL131
03074         GO TO 2210-SET-STATUS.                                    EL131
03075                                                                   EL131
03076      IF CM-LF-CURRENT-STATUS = '8'                                EL131
03077         MOVE '1'                 TO CM-LF-CURRENT-STATUS          EL131
03078         MOVE LOW-VALUES          TO CM-LF-DEATH-EXIT-DT           EL131
03079                                     CM-LF-CANCEL-DT.                 CL*34
03080                                                                   EL131
03081  2210-SET-STATUS.                                                 EL131
03082                                                                      CL**8
03083      IF CM-LF-CURRENT-STATUS = SPACES                             EL131
03084          MOVE '1'                TO CM-LF-CURRENT-STATUS.         EL131
03085                                                                   EL131
03086      IF CM-LF-ORIG-TERM GREATER THAN ZERO                            CL**3
03087          MOVE '6'                TO DC-OPTION-CODE                   CL**3
03088          MOVE CM-LF-ORIG-TERM    TO DC-ELAPSED-MONTHS                CL**3
03089          MOVE CM-CERT-EFF-DT     TO DC-BIN-DATE-1                    CL**3
03090          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL**3
03091          IF NO-CONVERSION-ERROR                                      CL**3
03092              MOVE DC-BIN-DATE-2  TO CM-LF-LOAN-EXPIRE-DT             CL**3
03093          ELSE                                                        CL**3
03094              MOVE LOW-VALUES     TO CM-LF-LOAN-EXPIRE-DT             CL**3
03095      ELSE                                                            CL**3
03096          MOVE LOW-VALUES         TO CM-LF-LOAN-EXPIRE-DT.            CL**3
03097                                                                   EL131
03098  2215-PROCESS-AH-SIDE.                                            EL131
03099                                                                      CL**8
03100      IF ACVCDI GREATER THAN LOW-VALUES                               CL**8
03101         IF ACVCDI = SPACES OR ZEROS                                  CL**8
03102            MOVE ZEROS            TO CM-AH-BENEFIT-CD              EL131
03103         ELSE                                                      EL131
03104            MOVE ACVCDI           TO CM-AH-BENEFIT-CD.                CL**8
03105                                                                   EL131
03106      IF ACVOTRMI GREATER THAN LOW-VALUES                             CL**8
03107          MOVE ACVOTRMI           TO CM-AH-ORIG-TERM.                 CL**8
03108                                                                   EL131
03109      IF ACVRATEL GREATER THAN +0                                     CL*32
03110          MOVE HOLD-AH-RATE       TO CM-AH-PREMIUM-RATE.              CL*14
03111                                                                      CL*14
03112      IF ACVBENEL GREATER THAN +0                                     CL*32
03113          MOVE HOLD-AH-CV-BEN     TO CM-AH-BENEFIT-AMT                CL*14
03114                                     PI-PAYMENT-AMT.                  CL*14
03115                                                                   EL131
03116      IF ACVFORMI GREATER THAN LOW-VALUES                             CL**8
03117          MOVE ACVFORMI           TO CM-POLICY-FORM-NO.               CL**8
03118                                                                   EL131
03119      IF ACVCNDTI = LOW-VALUES                                        CL**8
03120         GO TO 2220-SET-STATUS.                                    EL131
03121                                                                   EL131
03122      IF HOLD-AH-CV-CAN NOT = LOW-VALUES                              CL**8
03123         MOVE '8'                 TO CM-AH-CURRENT-STATUS          EL131
03124         MOVE HOLD-AH-CV-CAN      TO CM-AH-CANCEL-DT                  CL**8
03125         GO TO 2220-SET-STATUS.                                    EL131
03126                                                                   EL131
03127      IF CM-AH-CURRENT-STATUS = '6'                                EL131
03128         MOVE CM-AH-STATUS-AT-SETTLEMENT                           EL131
03129                                  TO CM-AH-CURRENT-STATUS          EL131
03130         MOVE SPACES              TO CM-AH-STATUS-AT-SETTLEMENT    EL131
03131         MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-EXIT-DT      EL131
03132                                     CM-AH-SETTLEMENT-DT              CL*34
03133         GO TO 2220-SET-STATUS.                                    EL131
03134                                                                   EL131
03135      IF CM-AH-CURRENT-STATUS = '8'                                EL131
03136         MOVE '1'                 TO CM-AH-CURRENT-STATUS          EL131
03137         MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-EXIT-DT      EL131
03138                                     CM-AH-CANCEL-DT.                 CL*34
03139                                                                   EL131
03140  2220-SET-STATUS.                                                 EL131
03141      IF CM-AH-CURRENT-STATUS = SPACES                             EL131
03142          MOVE '1'                TO CM-AH-CURRENT-STATUS.         EL131
03143                                                                      CL**3
03144      IF CM-AH-ORIG-TERM GREATER THAN ZERO                            CL**3
03145          MOVE '6'                TO DC-OPTION-CODE                   CL**3
03146          MOVE CM-AH-ORIG-TERM    TO DC-ELAPSED-MONTHS                CL**3
03147          MOVE CM-CERT-EFF-DT     TO DC-BIN-DATE-1                    CL**3
03148          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL**3
03149          IF NO-CONVERSION-ERROR                                      CL**3
03150              MOVE DC-BIN-DATE-2  TO CM-AH-LOAN-EXPIRE-DT             CL**3
03151          ELSE                                                        CL**3
03152              MOVE LOW-VALUES     TO CM-AH-LOAN-EXPIRE-DT             CL**3
03153      ELSE                                                            CL**3
03154          MOVE LOW-VALUES         TO CM-AH-LOAN-EXPIRE-DT.            CL**3
03155                                                                   EL131
03156  2225-FINISH-CERT.                                                EL131
03157      IF APRL GREATER THAN +0                                         CL*32
03158          MOVE HOLD-APR           TO CM-LOAN-APR.                     CL*14
03159                                                                   EL131
03160      IF PMTFREQI GREATER THAN LOW-VALUES                          EL131
03161          MOVE PMTFREQI           TO CM-PAY-FREQUENCY.             EL131
03162                                                                   EL131
03163      IF INDGRPI GREATER THAN LOW-VALUES                           EL131
03164          MOVE INDGRPI            TO CM-IND-GRP-TYPE.              EL131
03165                                                                   EL131
062217*    IF LOANNOI GREATER THAN LOW-VALUES                           EL131
062217*       MOVE LOANNOI             TO CM-LOAN-NUMBER.               EL131
03168                                                                   EL131
03169      IF LOANBALL GREATER THAN +0                                     CL*32
03170         MOVE HOLD-LOANBAL        TO CM-LOAN-BALANCE.              EL131
03171                                                                   EL131
03172      IF PREMTYPI GREATER THAN LOW-VALUES                          EL131
03173          MOVE PREMTYPI           TO CM-PREMIUM-TYPE.              EL131
03174                                                                   EL131
03175      IF REINCDI GREATER THAN LOW-VALUES                           EL131
03176          MOVE REINCDI            TO CM-SPECIAL-REIN-CODE             CL*14
03177                                     WS-REIN-1                        CL*14
03178                                     WS-REIN-2                        CL*14
03179                                     WS-REIN-3                        CL*14
03180          MOVE WS-REIN-TABLE      TO CM-REIN-TABLE.                   CL*14
03181                                                                   EL131
03182      MOVE LOW-VALUES             TO CM-AH-PAID-THRU-DT.           EL131
03183                                                                   EL131
03184      MOVE ZEROS                  TO CM-LOAN-TERM                  EL131
03185                                     CM-LIFE-COMM-PCT              EL131
03186                                     CM-AH-COMM-PCT.               EL131
03187                                                                      CL*14
121802*    IF PI-COMPANY-ID = 'CRI' OR 'PEM' OR 'NCL'  
121802*      IF CM-PREMIUM-TYPE NOT = '1'  AND        
121802*         PI-NO-PMTS = ZEROS                   
121802*          IF LOANBALL GREATER THAN +0          OR         
121802*             ACVBENEL GREATER THAN +0          OR        
121802*             APRL     GREATER THAN +0          OR       
121802*             LCVRATEL GREATER THAN +0          OR      
121802*             ACVRATEL GREATER THAN +0                 
121802*               PERFORM 6000-CALCULATE-CERT-TERM THRU 6000-EXIT.  
03197                                                                      CL*14
03198  2200-EXIT.                                                       EL131
03199      EXIT.                                                        EL131
03200      EJECT                                                        EL131
03201  2300-UPDATE-TRLR.                                                EL131

052113     MOVE ZERO                   TO TRLR-SEQ-NO.                  EL131
03203      EXEC CICS READ UPDATE                                        EL131
03204           DATASET  (TRLR-FILE-ID)                                 EL131
03205           RIDFLD   (TRLR-KEY)                                     EL131
03206           SET      (ADDRESS OF ACTIVITY-TRAILERS)                    CL*32
03207      END-EXEC.                                                       CL*34
03208                                                                   EL131
03209      MOVE ZERO                   TO COUNT-2.                      EL131
03210                                                                   EL131
03211      PERFORM 2310-INS-STATUS THRU 2310-EXIT                       EL131
03212          UNTIL UPDATE-MADE OR COUNT-2 GREATER THAN 6.             EL131
03213                                                                   EL131
03214      IF NOT UPDATE-MADE                                           EL131
03215          PERFORM 2320-SHIFT-STATUS THRU 2320-EXIT.                   CL*34
03216                                                                   EL131
03217      EXEC CICS REWRITE                                            EL131
03218           DATASET  (TRLR-FILE-ID)                                 EL131
03219           FROM     (ACTIVITY-TRAILERS)                            EL131
03220      END-EXEC.                                                    EL131
03221                                                                   EL131
03222  2300-EXIT.                                                       EL131
03223      EXIT.                                                        EL131
03224                                                                   EL131
03225  2310-INS-STATUS.                                                 EL131
03226      ADD 1                       TO COUNT-2.                      EL131
03227      IF AT-OPEN-CLOSE-TYPE (COUNT-2) = SPACE                      EL131
03228          PERFORM 2330-ADD-UPDATE THRU 2330-EXIT                      CL*34
03229          MOVE 'Y'                TO TRLR-SWITCH.                  EL131
03230                                                                   EL131
03231  2310-EXIT.                                                       EL131
03232      EXIT.                                                        EL131
03233                                                                   EL131
03234  2320-SHIFT-STATUS.                                               EL131
03235      IF AT-OPEN-CLOSE-TYPE (6) = STATUSI                          EL131
03236         MOVE 6                   TO COUNT-2                       EL131
03237         GO TO 2320-EXIT.                                          EL131
03238                                                                   EL131
03239      MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1). EL131
03240      MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2). EL131
03241      MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3). EL131
03242      MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4). EL131
03243      MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5). EL131
03244      MOVE 6       TO COUNT-2.                                     EL131
03245      PERFORM 2330-ADD-UPDATE THRU 2330-EXIT.                      EL131
03246                                                                   EL131
03247  2320-EXIT.                                                       EL131
03248      EXIT.                                                        EL131
03249                                                                   EL131
03250  2330-ADD-UPDATE.                                                 EL131
03251                                                                      CL**8
03252      IF COUNT-2 GREATER THAN 1                                    EL131
03253         COMPUTE MISC-SUB = (COUNT-2 - 1)                          EL131
03254         IF AT-OPEN-CLOSE-TYPE (MISC-SUB) = STATUSI                EL131
03255            GO TO 2330-EXIT.                                       EL131
03256                                                                   EL131
03257      MOVE SAVE-BIN-DATE         TO AT-OPEN-CLOSE-DATE (COUNT-2)      CL*34
03258                                    AT-RESERVES-LAST-MAINT-DT         CL**8
03259                                                                      CL**8
03260      MOVE PI-PROCESSOR-ID       TO AT-RESERVES-LAST-UPDATED-BY       CL**8
03261      MOVE EIBTIME               TO AT-LAST-MAINT-HHMMSS              CL**8
03262      MOVE STATUSI               TO AT-OPEN-CLOSE-TYPE (COUNT-2)      CL**8
03263      MOVE 'ALTER'               TO AT-OPEN-CLOSE-REASON (COUNT-2).EL131
03264                                                                   EL131
03265  2330-EXIT.                                                       EL131
03266      EXIT.                                                        EL131
03267                                                                   EL131
03268      EJECT                                                        EL131
03269  2400-UPDATE-TRLR.                                                EL131
03270      EXEC CICS HANDLE CONDITION                                   EL131
03271           NOTFND    (2400-TRLR-NOTFND)                            EL131
03272           ENDFILE   (2410-EXIT)                                   EL131
03273      END-EXEC.                                                    EL131
03274                                                                   EL131
03275      EXEC CICS STARTBR                                            EL131
03276          DATASET (TRLR-FILE-ID)                                   EL131
03277          RIDFLD  (TRLR-KEY)                                       EL131
03278      END-EXEC.                                                    EL131
03279                                                                   EL131
03280      EXEC CICS GETMAIN                                            EL131
03281           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL*32
03282           LENGTH    (TRLR-LENGTH)                                 EL131
03283           INITIMG   (GETMAIN-SPACE)                               EL131
03284      END-EXEC.                                                    EL131
03285                                                                   EL131
03286      PERFORM 2410-CHANGE-TRLR THRU 2410-EXIT.                        CL*34
03287                                                                   EL131
03288      EXEC CICS ENDBR                                              EL131
03289          DATASET (TRLR-FILE-ID)                                   EL131
03290      END-EXEC.                                                    EL131
03291                                                                   EL131
03292      GO TO 2400-EXIT.                                                CL*34
03293                                                                   EL131
03294  2400-TRLR-NOTFND.                                                EL131
03295      MOVE ER-0205                TO EMI-ERROR.                    EL131
03296      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*34
03297      MOVE 'X'                    TO ERROR-SWITCH.                 EL131
03298                                                                   EL131
03299  2400-EXIT.                                                          CL*34
03300      EXIT.                                                        EL131
03301                                                                   EL131
03302      EJECT                                                        EL131
03303  2410-CHANGE-TRLR.                                                EL131
03304      EXEC CICS READNEXT                                           EL131
03305          DATASET (TRLR-FILE-ID)                                   EL131
03306          RIDFLD  (TRLR-KEY)                                       EL131
03307          INTO    (ACTIVITY-TRAILERS)                              EL131
03308      END-EXEC.                                                    EL131
03309                                                                   EL131
03310      IF AT-CONTROL-PRIMARY = WS-LAST-TRLR-KEY                     EL131
03311         GO TO 2410-CHANGE-TRLR.                                   EL131
03312                                                                   EL131
03313      MOVE AT-CONTROL-PRIMARY     TO WS-LAST-TRLR-KEY                 CL*34
03314                                     CHECK-KEY.                       CL*34
03315                                                                   EL131
03316      IF CHECK-KEY NOT = MSTR-KEY                                  EL131
03317          GO TO 2410-EXIT.                                         EL131
03318                                                                   EL131
03319      EXEC CICS ENDBR                                              EL131
03320          DATASET (TRLR-FILE-ID)                                   EL131
03321      END-EXEC.                                                    EL131
03322                                                                   EL131
03323      EXEC CICS READ UPDATE                                        EL131
03324          DATASET (TRLR-FILE-ID)                                   EL131
03325          RIDFLD  (TRLR-KEY)                                       EL131
03326          INTO    (ACTIVITY-TRAILERS)                              EL131
03327      END-EXEC.                                                    EL131
03328                                                                   EL131
03329      EXEC CICS DELETE                                             EL131
03330          DATASET (TRLR-FILE-ID)                                      CL*34
03331      END-EXEC.                                                       CL*34
03332                                                                   EL131
03333      IF AT-SEQUENCE-NO = ZERO AND                                 EL131
03334         TRLR-UPDATE-REQUIRED                                      EL131
03335          PERFORM 2420-UPDATE-TRLR THRU 2420-EXIT.                    CL*34
03336                                                                   EL131
03337      IF CERTI GREATER THAN LOW-VALUES                             EL131
03338         MOVE CERTI               TO AT-CERT-PRIME.                EL131
03339                                                                   EL131
03340      IF SUFXI GREATER THAN LOW-VALUES                             EL131
03341         MOVE SUFXI               TO AT-CERT-SFX.                  EL131
03342                                                                   EL131
03343      IF CERTCARI GREATER THAN LOW-VALUES                          EL131
03344         MOVE CERTCARI            TO AT-CARRIER.                   EL131
03345                                                                   EL131
03346      IF PAYMENT-TR                                                EL131
03347         IF AT-CHECK-QUE-CONTROL NOT = ZEROS                       EL131
03348            AND AT-CHECK-QUE-CONTROL NOT = 99999999                EL131
03349               PERFORM 2460-UPDATE-CHECKQ THRU 2460-EXIT.             CL*34
03350                                                                   EL131
03351      IF CORRESPONDENCE-TR                                            CL*10
03352         AND AT-LETTER-ARCHIVE-NO NOT = ZEROS                         CL*34
03353            PERFORM 2475-UPDATE-LETTER THRU 2475-EXIT.                CL*34
03354                                                                   EL131
03355      EXEC CICS WRITE                                              EL131
03356           DATASET  (TRLR-FILE-ID)                                 EL131
03357           RIDFLD   (AT-CONTROL-PRIMARY)                           EL131
03358           FROM     (ACTIVITY-TRAILERS)                            EL131
03359      END-EXEC.                                                    EL131
03360                                                                   EL131
03361      EXEC CICS STARTBR                                            EL131
03362           DATASET  (TRLR-FILE-ID)                                 EL131
03363           RIDFLD   (TRLR-KEY)                                     EL131
03364      END-EXEC.                                                    EL131
03365                                                                   EL131
03366      GO TO 2410-CHANGE-TRLR.                                      EL131
03367                                                                   EL131
03368  2410-EXIT.                                                       EL131
03369      EXIT.                                                        EL131
03370                                                                   EL131
03371  2420-UPDATE-TRLR.                                                EL131
03372      MOVE ZERO TO COUNT-2.                                        EL131
03373      PERFORM 2310-INS-STATUS THRU 2310-EXIT                       EL131
03374          UNTIL UPDATE-MADE OR COUNT-2 GREATER THAN 6.             EL131
03375                                                                   EL131
03376      IF NOT UPDATE-MADE                                           EL131
03377          PERFORM 2320-SHIFT-STATUS THRU 2320-EXIT.                   CL*34
03378                                                                   EL131
03379  2420-EXIT.                                                       EL131
03380      EXIT.                                                        EL131
03381                                                                      CL**8
03382      EJECT                                                           CL**8
03383  2425-UPDATE-NINETY-TRLR.                                            CL**8
03384                                                                      CL**8
03385      MOVE +90                    TO TRLR-SEQ-NO.                     CL**8
03386                                                                      CL**8
03387      EXEC CICS HANDLE CONDITION                                      CL**8
03388           NOTFND    (2425-NINETY-NOTFND)                             CL**8
03389           ENDFILE   (2425-NINETY-NOTFND)                             CL**8
03390      END-EXEC.                                                       CL**8
03391                                                                      CL**8
03392      EXEC CICS READ UPDATE                                           CL**8
03393          DATASET (TRLR-FILE-ID)                                      CL**8
03394          SET     (ADDRESS OF ACTIVITY-TRAILERS)                      CL*32
03395          RIDFLD  (TRLR-KEY)                                          CL**8
03396      END-EXEC.                                                       CL**8

031715     if (diagi > low-values)
031715        and (diagi not = at-info-line-1)
031715        and (cl-total-paid-amt > zeros)
031715        move cl-cert-eff-dt      to dc-bin-date-1
031715        move cl-paid-thru-dt     to dc-bin-date-2
031715        move '1'                 to dc-option-code
031715        perform 9800-convert-date
031715                                 thru 9800-exit
031715        if no-conversion-error
031715           and dc-elapsed-months < +24
031715           and (not emi-bypass-forcables)
031715           move er-1581          to emi-error
031715           perform 9900-error-format
031715                                thru 9900-exit
031715           move -1              to diagl
031715           go to 8110-send-data
031715        end-if
031715     end-if

03398      MOVE AT-INFO-LINE-1        TO WS-DIAGNOSIS.                     CL*39
040814     MOVE AT-ICD-CODE-1         TO WS-ICD-CODE-1.
040814     MOVE AT-ICD-CODE-2         TO WS-ICD-CODE-2.
03399                                                                      CL*39
040814     IF DIAGI GREATER THAN LOW-VALUES
040814         MOVE DIAGI             TO AT-INFO-LINE-1
040814     END-IF.
040814
040814     IF ICD1I GREATER THAN LOW-VALUES
040814         MOVE ICD1I             TO AT-ICD-CODE-1
040814     END-IF.
040814
040814     IF ICD2I GREATER THAN LOW-VALUES
040814         MOVE ICD2I             TO AT-ICD-CODE-2
040814     END-IF.
03401                                                                      CL**8
03402      EXEC CICS REWRITE                                               CL**8
03403          DATASET (TRLR-FILE-ID)                                      CL**8
03404          FROM    (ACTIVITY-TRAILERS)                                 CL**8
03405      END-EXEC.                                                       CL**8
03406                                                                      CL*39
040814     IF DIAGI GREATER THAN LOW-VALUES
040814       AND DIAGI NOT = WS-DIAGNOSIS
03408          MOVE 'DIAGNOSIS'          TO SPLIT-INFO-DESC                CL*39
03409          MOVE WS-DIAGNOSIS         TO SPLIT-INFO-OLD                 CL*39
03410          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*39
040814
040814     IF ICD1I GREATER THAN LOW-VALUES
040814       AND ICD1I NOT = WS-ICD-CODE-1
040814         MOVE 'ICD CODE 1'         TO SPLIT-INFO-DESC
040814         MOVE WS-ICD-CODE-1        TO SPLIT-INFO-OLD
040814         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT
040814     END-IF.
040814
040814     IF ICD2I GREATER THAN LOW-VALUES
040814       AND ICD2I NOT = WS-ICD-CODE-2
040814         MOVE 'ICD CODE 2'         TO SPLIT-INFO-DESC
040814         MOVE WS-ICD-CODE-2        TO SPLIT-INFO-OLD
040814         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT
040814     END-IF.
03411                                                                      CL**8
03412      GO TO 2425-EXIT.                                                CL**8
03413                                                                      CL**8
03414  2425-NINETY-NOTFND.                                                 CL**8
03415                                                                      CL**8
03416      EXEC CICS GETMAIN                                               CL*13
03417           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL*32
03418           LENGTH    (TRLR-LENGTH)                                    CL*13
03419           INITIMG   (GETMAIN-SPACE)                                  CL*13
03420      END-EXEC.                                                       CL*13
03421                                                                      CL*13
03422      MOVE +90                  TO TRLR-SEQ-NO.                       CL*13
03423      MOVE TRLR-KEY             TO AT-CONTROL-PRIMARY.                CL*13
03424                                                                      CL*13
03425      MOVE 'AT'                 TO AT-RECORD-ID                       CL*13
03426      MOVE '6'                  TO AT-TRAILER-TYPE                    CL*13
03427      MOVE SAVE-BIN-DATE        TO AT-RECORDED-DT                     CL*13
03428                                   AT-GEN-INFO-LAST-MAINT-DT          CL*13
03429      MOVE PI-PROCESSOR-ID      TO AT-RECORDED-BY                     CL*13
03430                                   AT-GEN-INFO-LAST-UPDATED-BY        CL*13
03431      MOVE EIBTIME              TO AT-LAST-MAINT-HHMMSS               CL*13
03432      MOVE DIAGI                TO AT-INFO-LINE-1.                    CL*13
040814     MOVE ICD1I                TO AT-ICD-CODE-1.
040814     MOVE ICD2I                TO AT-ICD-CODE-2.
03433                                                                      CL*13
03434      EXEC CICS WRITE                                                 CL*13
03435          DATASET (TRLR-FILE-ID)                                      CL*13
03436          FROM    (ACTIVITY-TRAILERS)                                 CL*13
03437          RIDFLD  (TRLR-KEY)                                          CL*13
03438      END-EXEC.                                                       CL*13
03439                                                                      CL*13
03440  2425-EXIT.                                                          CL**8
03441      EXIT.                                                           CL**8
03442                                                                      CL**8
03443      EJECT                                                        EL131
03444  2430-UPDATE-ACTQ.                                                EL131
03445      EXEC CICS HANDLE CONDITION                                   EL131
03446           NOTFND    (2430-EXIT)                                      CL*34
03447           ENDFILE   (2440-EXIT)                                   EL131
03448      END-EXEC.                                                    EL131
03449                                                                   EL131
03450      EXEC CICS STARTBR                                            EL131
03451           DATASET  (ACTQ-FILE-ID)                                 EL131
03452           RIDFLD   (ACTQ-KEY)                                     EL131
03453      END-EXEC.                                                    EL131
03454                                                                   EL131
03455      EXEC CICS GETMAIN                                            EL131
03456           SET      (ADDRESS OF ACTIVITY-QUE)                         CL*32
03457           LENGTH   (ACTQ-LENGTH)                                  EL131
03458           INITIMG  (GETMAIN-SPACE)                                EL131
03459      END-EXEC.                                                    EL131
03460                                                                   EL131
03461      PERFORM 2440-CHANGE-ACTQ THRU 2440-EXIT.                        CL*34
03462                                                                   EL131
03463  2430-EXIT.                                                          CL*34
03464      EXIT.                                                        EL131
03465                                                                   EL131
03466  2440-CHANGE-ACTQ.                                                EL131
03467                                                                   EL131
03468      EXEC CICS READNEXT                                           EL131
03469           DATASET  (ACTQ-FILE-ID)                                 EL131
03470           RIDFLD   (ACTQ-KEY)                                     EL131
03471           INTO     (ACTIVITY-QUE)                                 EL131
03472      END-EXEC.                                                    EL131
03473                                                                   EL131
03474      EXEC CICS ENDBR                                                 CL**5
03475           DATASET (ACTQ-FILE-ID)                                     CL**5
03476      END-EXEC.                                                       CL**5
03477                                                                      CL**5
03478      IF ACTQ-KEY NOT = MSTR-KEY                                   EL131
03479          GO TO 2440-EXIT.                                         EL131
03480                                                                   EL131
03481      EXEC CICS READ UPDATE                                        EL131
03482           DATASET  (ACTQ-FILE-ID)                                 EL131
03483           RIDFLD   (AQ-CONTROL-PRIMARY)                           EL131
03484           SET      (ADDRESS OF ACTIVITY-QUE)                         CL*32
03485      END-EXEC.                                                    EL131
03486                                                                   EL131
03487      MOVE ACTIVITY-QUE           TO JP-RECORD-AREA                EL131
03488                                                                   EL131
03489      EXEC CICS DELETE                                             EL131
03490           DATASET (ACTQ-FILE-ID)                                  EL131
03491      END-EXEC.                                                    EL131
03492                                                                   EL131
03493      MOVE JP-RECORD-AREA         TO ACTIVITY-QUE.                 EL131
03494      IF CERTI GREATER THAN LOW-VALUES                             EL131
03495         MOVE CERTI               TO AQ-CERT-PRIME.                EL131
03496      IF SUFXI GREATER THAN LOW-VALUES                             EL131
03497         MOVE SUFXI               TO AQ-CERT-SFX.                  EL131
03498      IF CERTCARI GREATER THAN LOW-VALUES                          EL131
03499         MOVE CERTCARI            TO AQ-CARRIER.                   EL131
03500                                                                   EL131
03501      EXEC CICS HANDLE CONDITION                                      CL*27
03502           DUPREC   (2440-EXIT)                                       CL*27
03503      END-EXEC.                                                       CL*27
03504                                                                   EL131
03505      EXEC CICS WRITE                                              EL131
03506           DATASET  (ACTQ-FILE-ID)                                 EL131
03507           RIDFLD   (AQ-CONTROL-PRIMARY)                           EL131
03508           FROM     (ACTIVITY-QUE)                                 EL131
03509      END-EXEC.                                                    EL131
03510                                                                   EL131
03511  2440-EXIT.                                                       EL131
03512      EXIT.                                                        EL131
03513                                                                   EL131
03514      EJECT                                                        EL131
03515  2460-UPDATE-CHECKQ.                                              EL131
03516      EXEC CICS HANDLE CONDITION                                   EL131
03517           NOTFND   (2460-EXIT)                                    EL131
03518      END-EXEC.                                                    EL131
03519                                                                   EL131
03520      MOVE AT-CHECK-QUE-CONTROL   TO CHKQ-CONTROL-NO.              EL131
03521      MOVE AT-CHECK-QUE-SEQUENCE  TO CHKQ-SEQ-NO.                  EL131
03522      MOVE PI-COMPANY-CD          TO CHKQ-COMPANY-CODE.            EL131
03523                                                                   EL131
03524      EXEC CICS READ UPDATE                                        EL131
03525           DATASET   (CHKQ-FILE-ID)                                EL131
03526           RIDFLD    (CHKQ-KEY)                                    EL131
03527           SET       (ADDRESS OF CHECK-QUE)                           CL*32
03528      END-EXEC.                                                    EL131
03529                                                                   EL131
03530      IF CERTI GREATER THAN LOW-VALUES                             EL131
03531         MOVE CERTI               TO CQ-CERT-PRIME.                EL131
03532      IF SUFXI GREATER THAN LOW-VALUES                             EL131
03533         MOVE SUFXI               TO CQ-CERT-SFX.                  EL131
03534      IF CERTCARI GREATER THAN LOW-VALUES                          EL131
03535         MOVE CERTCARI            TO CQ-CARRIER.                   EL131
03536                                                                   EL131
03537      EXEC CICS REWRITE                                            EL131
03538           DATASET  (CHKQ-FILE-ID)                                 EL131
03539           FROM     (CHECK-QUE)                                    EL131
03540      END-EXEC.                                                    EL131
03541                                                                   EL131
03542  2460-EXIT.                                                       EL131
03543      EXIT.                                                        EL131
03544                                                                   EL131
03545      EJECT                                                           CL*10
03546  2475-UPDATE-LETTER.                                                 CL*10
03547      EXEC CICS HANDLE CONDITION                                      CL*10
03548           NOTFND   (2475-EXIT)                                       CL*10
03549      END-EXEC.                                                       CL*10
03550                                                                      CL*10
03551      MOVE AT-LETTER-ARCHIVE-NO   TO ARCH-ARCHIVE-NO.                 CL*10
03552      MOVE '1'                    TO ARCH-RECORD-TYPE.                CL*10
03553      MOVE ZEROS                  TO ARCH-SEQ-NO.                     CL*10
03554      MOVE PI-COMPANY-CD          TO ARCH-COMPANY-CODE.               CL*10
03555                                                                      CL*10
03556      EXEC CICS READ UPDATE                                           CL*10
03557           DATASET   (ARCH-FILE-ID)                                   CL*10
03558           RIDFLD    (ARCH-KEY)                                       CL*10
03559           SET       (ADDRESS OF LETTER-ARCHIVE)                      CL*32
03560      END-EXEC.                                                       CL*10
03561                                                                      CL*10
03562 * COMPARE LETTER HEADER INFORMATION TO ASSURE THAT IT BELONGS        CL*10
03563 * TO THIS MASTER-KEY INFORMATION.                                    CL*10
03564                                                                      CL*10
03565      IF LA-CARRIER  = CARRIER-CODE AND                               CL*34
03566         LA-CLAIM-NO = CLAIM-NO     AND                               CL*34
03567         LA-CERT-NO  = CERT-NO                                        CL*34
03568         NEXT SENTENCE                                                CL*10
03569      ELSE                                                            CL*10
03570         GO TO 2475-UNLOCK.                                           CL*10
03571                                                                      CL*10
03572      IF CERTI GREATER THAN LOW-VALUES                                CL*10
03573         MOVE CERTI               TO LA-CERT-PRIME.                   CL*10
03574      IF SUFXI GREATER THAN LOW-VALUES                                CL*10
03575         MOVE SUFXI               TO LA-CERT-SFX.                     CL*10
03576      IF CERTCARI GREATER THAN LOW-VALUES                             CL*10
03577         MOVE CERTCARI            TO LA-CARRIER.                      CL*10
03578                                                                      CL*10
03579      EXEC CICS REWRITE                                               CL*10
03580           DATASET  (ARCH-FILE-ID)                                    CL*10
03581           FROM     (LETTER-ARCHIVE)                                  CL*10
03582      END-EXEC.                                                       CL*10
03583                                                                      CL*34
03584      GO TO 2475-EXIT.                                                CL*10
03585                                                                      CL*10
03586  2475-UNLOCK.                                                        CL*10
03587      EXEC CICS UNLOCK                                                CL*10
03588           DATASET  (ARCH-FILE-ID)                                    CL*10
03589      END-EXEC.                                                       CL*14
03590                                                                      CL*10
03591  2475-EXIT.                                                          CL*10
03592      EXIT.                                                        EL131
03593      EJECT                                                        EL131
03594  2600-CREATE-MAINT-NOTE.                                             CL*34
03595      MOVE PI-COMPANY-ID          TO CNTL-CO-ID.                      CL*34
03596      MOVE '1'                    TO CNTL-REC-TYPE.                   CL*34
03597      MOVE SPACES                 TO CNTL-PROC-ID.                    CL*34
03598      MOVE ZERO                   TO CNTL-SEQ-NO.                     CL*34
03599                                                                   EL131
03600      EXEC CICS READ                                                  CL*34
03601           SET        (ADDRESS OF CONTROL-FILE)                       CL*34
03602           DATASET    ('ELCNTL')                                      CL*34
03603           RIDFLD     (CNTL-KEY)                                      CL*34
03604      END-EXEC.                                                       CL*34
03605                                                                      CL*34
03606      IF CO-NO-USE-AUDIT-CHANGES                                      CL*34
03607          GO TO 2600-EXIT.                                            CL*34
03608                                                                      CL*34
03609      IF PROCL GREATER ZERO                                           CL*37
03610      IF PROCI NOT = LOW-VALUES                                       CL*37
03611      IF PROCI NOT = CL-PROCESSOR-ID                                  CL*37
03612          MOVE 'PROCESSOR'          TO SPLIT-INFO-DESC                CL*34
03613          MOVE CL-PROCESSOR-ID      TO SPLIT-INFO-OLD                 CL*34
03614          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03615                                                                      CL*34
03616      IF SEXL GREATER ZERO                                            CL*37
03617      IF SEXI NOT = LOW-VALUES                                        CL*37
03618      IF SEXI NOT = CL-INSURED-SEX-CD                                 CL*37
03619          MOVE 'SEX'                TO SPLIT-INFO-DESC                CL*34
03620          MOVE CL-INSURED-SEX-CD    TO SPLIT-INFO-OLD                 CL*34
03621          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03622                                                                      CL*34
03623      IF BIRTHL GREATER ZERO                                          CL*37
03624      IF HOLD-BIRTH NOT = LOW-VALUES                                  CL*37
03625      IF HOLD-BIRTH NOT = CL-INSURED-BIRTH-DT                         CL*37
03626          MOVE 'BIRTH'              TO SPLIT-INFO-DESC                CL*34
03627          MOVE CL-INSURED-BIRTH-DT  TO DC-BIN-DATE-1                  CL*34
03628          MOVE SPACES               TO DC-OPTION-CODE                 CL*34
03629          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
03630          MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD                 CL*34
03631          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03632                                                                      CL*34
03633      IF SOCIALL GREATER ZERO                                         CL*37
03634      IF SOCIALI NOT = LOW-VALUES                                     CL*37
03635      IF SOCIALI NOT = CL-SOC-SEC-NO                                  CL*37
03636          MOVE 'SOC SEC NO'         TO SPLIT-INFO-DESC                CL*34
03637          MOVE CL-SOC-SEC-NO        TO SPLIT-INFO-OLD                 CL*34
03638          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03639                                                                      CL*34
03640      IF OCCL GREATER ZERO                                            CL*37
03641      IF OCCI NOT = LOW-VALUES                                        CL*37
03642      IF OCCI NOT = CL-INSURED-OCC-CD                                 CL*37
03643          MOVE 'OCCUPATION'         TO SPLIT-INFO-DESC                CL*34
03644          MOVE CL-INSURED-OCC-CD    TO SPLIT-INFO-OLD                 CL*34
03645          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03646                                                                      CL*34
03647      IF BENEL GREATER ZERO                                           CL*37
03648      IF BENEI NOT = LOW-VALUES                                       CL*37
03649      IF BENEI NOT = CL-BENEFICIARY                                   CL*37
03650          MOVE 'BENEFICIARY'        TO SPLIT-INFO-DESC                CL*34
03651          MOVE CL-BENEFICIARY      TO SPLIT-INFO-OLD                  CL*34
03652          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03653                                                                      CL*34
03654      IF PREMTYPL GREATER ZERO                                        CL*37
03655      IF PREMTYPI NOT = LOW-VALUES                                    CL*37
03656      IF PREMTYPI NOT = CL-CLAIM-PREM-TYPE                            CL*37
03657          MOVE 'PREM TYPE'          TO SPLIT-INFO-DESC                CL*34
03658          MOVE CL-CLAIM-PREM-TYPE   TO SPLIT-INFO-OLD                 CL*34
03659          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03660                                                                      CL*34
040814*    IF CAUSEL GREATER ZERO                                          CL*37
040814*    IF CAUSEI NOT = LOW-VALUES                                      CL*37
040814*    IF CAUSEI NOT = CL-CAUSE-CD                                     CL*37
040814*        MOVE 'CAUSE CD'           TO SPLIT-INFO-DESC                CL*34
040814*        MOVE CL-CAUSE-CD          TO SPLIT-INFO-OLD                 CL*34
040814*        PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03667                                                                      CL*34
040814*    IF ENDL GREATER ZERO                                            CL*37
040814*    IF HOLD-END NOT = LOW-VALUES                                    CL*37
040814*    IF HOLD-END NOT = CL-EST-END-OF-DISAB-DT                        CL*37
040814*        MOVE 'END DT'             TO SPLIT-INFO-DESC                CL*34
040814*        MOVE CL-EST-END-OF-DISAB-DT                                 CL*34
040814*                                  TO DC-BIN-DATE-1                  CL*34
040814*        MOVE SPACES               TO DC-OPTION-CODE                 CL*34
040814*        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
040814*        MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD                 CL*34
040814*        PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03678                                                                      CL*34
03679      IF PDTHRUL GREATER ZERO                                         CL*37
03680      IF HOLD-PDTHRU NOT = LOW-VALUES                                 CL*37
03681      IF HOLD-PDTHRU NOT = CL-PAID-THRU-DT                            CL*37
03682          MOVE 'PAID THRU'          TO SPLIT-INFO-DESC                CL*34
03683          MOVE CL-PAID-THRU-DT      TO DC-BIN-DATE-1                  CL*34
03684          MOVE SPACES               TO DC-OPTION-CODE                 CL*34
03685          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
03686          MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD                 CL*34
03687          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03688                                                                      CL*34
03689      IF PDAMTL GREATER ZERO                                          CL*37
03690      IF HOLD-PDAMT NOT = ZEROS                                       CL*37
03691      IF HOLD-PDAMT NOT = CL-TOTAL-PAID-AMT                           CL*37
03692          MOVE 'PAID AMOUNT'        TO SPLIT-INFO-DESC                CL*34
03693          MOVE CL-TOTAL-PAID-AMT    TO WS-EDIT-AMT                    CL*34
03694          MOVE WS-EDIT-AMT          TO SPLIT-INFO-OLD                 CL*34
03695          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03696                                                                      CL*34
03697      IF NODAYSL GREATER ZERO                                         CL*37
03698      IF HOLD-NODAYS NOT = ZEROS                                      CL*37
03699      IF HOLD-NODAYS NOT = CL-NO-OF-DAYS-PAID                         CL*37
03700          MOVE 'DAYS PAID'          TO SPLIT-INFO-DESC                CL*34
03701          MOVE CL-NO-OF-DAYS-PAID   TO WS-EDIT-NUMBER                 CL*34
03702          MOVE WS-EDIT-NUMBER       TO SPLIT-INFO-OLD                 CL*34
03703          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03704                                                                      CL*34
03705      IF NOPMTSL GREATER ZERO                                         CL*37
03706      IF HOLD-NOPMTS NOT = ZEROS                                      CL*37
03707      IF HOLD-NOPMTS NOT = CL-NO-OF-PMTS-MADE                         CL*37
03708          MOVE 'PMTS MADE'          TO SPLIT-INFO-DESC                CL*34
03709          MOVE CL-NO-OF-PMTS-MADE   TO WS-EDIT-NUMBER                 CL*34
03710          MOVE WS-EDIT-NUMBER       TO SPLIT-INFO-OLD                 CL*34
03711          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03712                                                                      CL*34
03713      IF FORMTYPL GREATER ZERO                                        CL*37
03714      IF FORMTYPI NOT = LOW-VALUES                                    CL*37
03715      IF FORMTYPI NOT = CL-PROG-FORM-TYPE                             CL*37
03716          MOVE 'FORM TYPE'          TO SPLIT-INFO-DESC                CL*34
03717          MOVE CL-PROG-FORM-TYPE    TO SPLIT-INFO-OLD                 CL*34
03718          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03719                                                                      CL*34
03720      IF REPL GREATER ZERO                                            CL*37
03721      IF HOLD-REPORTED NOT = LOW-VALUES                               CL*37
03722      IF HOLD-REPORTED NOT = CL-REPORTED-DT                           CL*37
03723          MOVE 'REPORTED DT'        TO SPLIT-INFO-DESC                CL*34
03724          MOVE CL-REPORTED-DT       TO DC-BIN-DATE-1                  CL*34
03725          MOVE SPACES               TO DC-OPTION-CODE                 CL*34
03726          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
03727          MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD                 CL*34
03728          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03729                                                                      CL*34
03730      IF ADDONDTL GREATER ZERO                                        CL*37
03731      IF HOLD-ADDON NOT = LOW-VALUES                                  CL*37
03732      IF HOLD-ADDON NOT = CL-LAST-ADD-ON-DT                           CL*37
03733          MOVE 'ADD ON DT'          TO SPLIT-INFO-DESC                CL*34
03734          MOVE CL-LAST-ADD-ON-DT    TO DC-BIN-DATE-1                  CL*34
03735          MOVE SPACES               TO DC-OPTION-CODE                 CL*34
03736          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
03737          MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD                 CL*34
03738          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03739                                                                      CL*34
03740      IF PRICDL GREATER ZERO                                          CL*37
03741      IF PRICDI NOT = LOW-VALUES                                      CL*37
03742      IF PRICDI NOT = CL-PRIORITY-CD                                  CL*37
03743          MOVE 'PRIORITY CODE'      TO SPLIT-INFO-DESC                CL*34
03744          MOVE CL-PRIORITY-CD       TO SPLIT-INFO-OLD                 CL*34
03745          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03746                                                                      CL*34
03747      IF FILETOL GREATER ZERO                                         CL*37
03748      IF FILETOI NOT = LOW-VALUES                                     CL*37
03749      IF FILETOI NOT = CL-FILE-LOCATION                               CL*37
03750          MOVE 'FILE TO'            TO SPLIT-INFO-DESC                CL*34
03751          MOVE CL-FILE-LOCATION     TO SPLIT-INFO-OLD                 CL*34
03752          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03753                                                                      CL*34
03754      IF SUPVL GREATER ZERO                                           CL*37
03755      IF SUPVI NOT = LOW-VALUES                                       CL*37
03756      IF SUPVI NOT = CL-SUPV-ATTN-CD                                  CL*37
03757          MOVE 'SUPV (Y/N)'         TO SPLIT-INFO-DESC                CL*34
03758          MOVE CL-SUPV-ATTN-CD      TO SPLIT-INFO-OLD                 CL*34
03759          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34

052113*    IF (CRITPTL > ZERO)
052113*       AND (CRITPTI NOT = LOW-VALUES)
052113*       AND (CRITPTI NOT = CL-CRIT-PER-RECURRENT)
052113*       MOVE 'CRIT PER RECURR'   TO SPLIT-INFO-DESC
052113*       MOVE CL-CRIT-PER-RECURRENT
052113*                                TO SPLIT-INFO-OLD
052113*       PERFORM 2650-WRITE-MAINT-NOTE
052113*                                THRU 2650-EXIT
052113*    END-IF
052113
052113*    IF (RTWMOSL > ZERO)
052113*       AND (RTWMOSI NOT = LOW-VALUES)
052113*       AND (RTWMOSI NOT = CL-CRIT-PER-RTW-MOS)
052113*       MOVE 'CRIT PER RTW MO'   TO SPLIT-INFO-DESC
052113*       MOVE CL-CRIT-PER-RTW-MOS TO SPLIT-INFO-OLD
052113*       PERFORM 2650-WRITE-MAINT-NOTE
052113*                                THRU 2650-EXIT
052113*    END-IF

03761      IF MLNAMEL GREATER ZERO                                         CL*37
03762      IF MLNAMEI NOT = LOW-VALUES                                     CL*37
03763      IF MLNAMEI NOT = CL-INSURED-LAST-NAME                           CL*37
03764          MOVE 'INS LAST NAME'      TO SPLIT-INFO-DESC                CL*34
03765          MOVE CL-INSURED-LAST-NAME TO SPLIT-INFO-OLD                 CL*34
03766          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03767                                                                      CL*34
03768      IF MFNAMEL GREATER ZERO                                         CL*37
03769      IF MFNAMEI NOT = LOW-VALUES                                     CL*37
03770      IF MFNAMEI NOT = CL-INSURED-1ST-NAME                            CL*37
03771          MOVE 'INS 1ST NAME'       TO SPLIT-INFO-DESC                CL*34
03772          MOVE CL-INSURED-1ST-NAME  TO SPLIT-INFO-OLD                 CL*34
03773          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03774                                                                      CL*34
03775      IF MMINITL GREATER ZERO                                         CL*37
03776      IF MMINITI NOT = LOW-VALUES                                     CL*37
03777      IF MMINITI NOT = CL-INSURED-MID-INIT                            CL*37
03778          MOVE 'INS MID INITL'      TO SPLIT-INFO-DESC                CL*34
03779          MOVE CL-INSURED-MID-INIT  TO SPLIT-INFO-OLD                 CL*34
03780          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03781                                                                      CL*34
03782      IF CERTEFFL GREATER ZERO                                        CL*37
03783      IF HOLD-EFF NOT = LOW-VALUES                                    CL*37
03784      IF HOLD-EFF NOT = CL-CERT-EFF-DT                                CL*37
03785          MOVE 'CERT EFF DT'        TO SPLIT-INFO-DESC                CL*34
03786          MOVE CL-CERT-EFF-DT       TO DC-BIN-DATE-1                  CL*34
03787          MOVE SPACES               TO DC-OPTION-CODE                 CL*34
03788          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
03789          MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD                 CL*34
03790          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03791                                                                      CL*34
03792      IF CERTACTL GREATER ZERO                                        CL*37
03793      IF CERTACTI NOT = LOW-VALUES                                    CL*37
03794      IF CERTACTI NOT = CL-CERT-ACCOUNT                               CL*37
03795          MOVE 'CERT ACCOUNT'       TO SPLIT-INFO-DESC                CL*34
03796          MOVE CL-CERT-ACCOUNT      TO SPLIT-INFO-OLD                 CL*34
03797          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03798                                                                      CL*34
03799      IF CERTSTL GREATER ZERO                                         CL*37
03800      IF CERTSTI NOT = LOW-VALUES                                     CL*37
03801      IF CERTSTI NOT = CL-CERT-STATE                                  CL*37
03802          MOVE 'CERT STATE'         TO SPLIT-INFO-DESC                CL*34
03803          MOVE CL-CERT-STATE        TO SPLIT-INFO-OLD                 CL*34
03804          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03805                                                                      CL*34
03806      IF CERTCARL GREATER ZERO                                        CL*37
03807      IF CERTCARI NOT = LOW-VALUES                                    CL*37
03808      IF CERTCARI NOT = CL-CERT-CARRIER                               CL*37
03809          MOVE 'CERT CARRIER'       TO SPLIT-INFO-DESC                CL*34
03810          MOVE CL-CERT-CARRIER      TO SPLIT-INFO-OLD                 CL*34
03811          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03812                                                                      CL*34
03813      IF CERTGRPL GREATER ZERO                                        CL*37
03814      IF CERTGRPI NOT = LOW-VALUES                                    CL*37
03815      IF CERTGRPI NOT = CL-CERT-GROUPING                              CL*37
03816          MOVE 'CERT GROUPING'      TO SPLIT-INFO-DESC                CL*34
03817          MOVE CL-CERT-GROUPING     TO SPLIT-INFO-OLD                 CL*34
03818          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34
03819                                                                      CL*34
03820      IF CERTL GREATER ZERO OR                                        CL*37
03821         SUFXL GREATER ZERO                                           CL*37
03822      IF (CERTI NOT = LOW-VALUES AND                                  CL*37
03823                      CL-CERT-PRIME)                                  CL*37
03824                 OR                                                   CL*37
03825         (SUFXI NOT = LOW-VALUES AND                                  CL*37
03826                      CL-CERT-SFX)                                    CL*37
03827          MOVE 'CERT NO'            TO SPLIT-INFO-DESC                CL*34
03828          MOVE CL-CERT-NO           TO SPLIT-INFO-OLD                 CL*34
03829          PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.               CL*34

080613     IF incl > zeros
080613        IF (HOLD-INCUR NOT = LOW-VALUES)
080613           AND (HOLD-INCUR NOT = CL-INCURRED-DT)
080613           MOVE 'INC DTE'        TO SPLIT-INFO-DESC
080613           MOVE CL-INCURRED-DT   TO DC-BIN-DATE-1
080613           MOVE SPACES           TO DC-OPTION-CODE
080613           PERFORM 9800-CONVERT-DATE
080613                                 THRU 9800-EXIT
080613           MOVE DC-GREG-DATE-1-EDIT
080613                                 TO SPLIT-INFO-OLD
080613           PERFORM 2650-WRITE-MAINT-NOTE
080613                                 THRU 2650-EXIT
080613        END-IF
080613     END-IF

101917     IF PI-STATE = 'VA' OR 'PA' OR 'GA'
101917       IF (REPL GREATER ZERO
101917         AND HOLD-REPORTED NOT = LOW-VALUES
101917         AND HOLD-REPORTED NOT = CL-REPORTED-DT)
101917        OR (incl > zeros
101917          AND (HOLD-INCUR NOT = LOW-VALUES)
101917             AND (HOLD-INCUR NOT = CL-INCURRED-DT))
101917          PERFORM 2610-CHECK-2-YEAR-CONTESTABLE THRU 2610-EXIT
101917       END-IF
101917     END-IF

090821     if incl > zeros
090821        move cl-incurred-dt      to ws-prev-inc-dt
090821        perform 7990-get-lo-hi-acct-dates
090821                                 thru 7990-exit
090821        if (hold-incur >= ws-hi-acct-dt)
090821           and (acct-cancelled)
090821           and (mob-cert)
090821           MOVE er-1682          TO EMI-ERROR
090821           MOVE -1               TO INCL
090821           MOVE AL-UABON         TO INCA
090821           PERFORM 9900-ERROR-FORMAT
090821                                 THRU 9900-EXIT
090821           PERFORM 2620-CHECK-TRLR
090821                                 THRU 2620-EXIT
090821        end-if
090821        if hold-incur < cm-cert-eff-dt
090821           MOVE er-1683          TO EMI-ERROR
090821           MOVE -1               TO INCL
090821           MOVE AL-UABON         TO INCA
090821           PERFORM 9900-ERROR-FORMAT
090821                                 THRU 9900-EXIT
090821           PERFORM 2620-CHECK-TRLR
090821                                 THRU 2620-EXIT
090821        end-if
090821     end-if

071720*052918IF incl > zeros
071720*052918  AND PI-COMPANY-ID = 'CID'
071720*052918   IF (HOLD-INCUR NOT = LOW-VALUES)
071720*052918      AND (HOLD-INCUR NOT = CL-INCURRED-DT)
071720*052918      IF TYPEI = PI-AH-OVERRIDE-L1
071720*052918         SET ELAPSED-BETWEEN-BIN TO TRUE
071720*052918         MOVE ZERO               TO DC-ELAPSED-MONTHS
071720*052918                                    DC-ELAPSED-DAYS
071720*052918
071720*052918         MOVE HOLD-INCUR  TO DC-BIN-DATE-1
071720*052918         MOVE SAVE-BIN-DATE TO DC-BIN-DATE-2
071720*052918         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
071720*052918         IF DC-ODD-DAYS-OVER > ZERO
071720*052918            ADD 1 TO DC-ELAPSED-MONTHS
071720*052918         END-IF
071720*052918
071720*052918         IF PI-STATE = 'HI'
071720*052918           AND DC-ELAPSED-MONTHS <= 18
071720*052918            CONTINUE
071720*052918         ELSE
071720*052918            IF DC-ELAPSED-MONTHS > 15
071720*052918               MOVE ER-7572            TO EMI-ERROR
071720*052918               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
071720*052918            END-IF
071720*052918         END-IF
071720*052918         PERFORM 2620-CHECK-TRLR THRU 2620-EXIT
071720*052918      END-IF
071720*052918   END-IF
071720*052918END-IF

           .
03831  2600-EXIT.                                                          CL*34
03832       EXIT.                                                          CL*34
101917 2610-CHECK-2-YEAR-CONTESTABLE.
101917     SET ELAPSED-BETWEEN-BIN TO TRUE
101917     MOVE ZERO               TO DC-ELAPSED-MONTHS
101917                                DC-ELAPSED-DAYS
101917
101917     MOVE HOLD-EFF TO DC-BIN-DATE-1.
101917     IF HOLD-INCUR > SPACES
101917        MOVE HOLD-INCUR TO DC-BIN-DATE-2
101917     ELSE
101917        MOVE CL-INCURRED-DT TO DC-BIN-DATE-2
101917     END-IF
101917     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
101917     IF DC-ODD-DAYS-OVER > ZERO
101917        ADD 1 TO DC-ELAPSED-MONTHS
101917     END-IF
101917
101917     IF DC-ELAPSED-MONTHS <= 24
101917        IF HOLD-REPORTED > SPACES
101917           MOVE HOLD-REPORTED TO DC-BIN-DATE-2
101917        ELSE
101917           MOVE CL-REPORTED-DT TO DC-BIN-DATE-2
101917        END-IF
101917        MOVE ZERO           TO DC-ELAPSED-MONTHS
101917                               DC-ELAPSED-DAYS
101917        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
101917        IF DC-ODD-DAYS-OVER > ZERO
101917           ADD 1 TO DC-ELAPSED-MONTHS
101917        END-IF
101917        IF DC-ELAPSED-MONTHS > 24
101917           MOVE ER-1679            TO EMI-ERROR
101917           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
061418        ELSE
061418           MOVE 1                  TO EMI-ERROR
101917        END-IF
101917     END-IF.
101917
101917     PERFORM 2615-CHECK-TRLR THRU 2615-EXIT.
101917
101917 2610-EXIT.
101917     EXIT.
101917 2615-CHECK-TRLR.
101917
101917     MOVE CL-CONTROL-PRIMARY   TO TRLR-KEY
101917     MOVE +96              TO TRLR-SEQ-NO
101917     EXEC CICS READ
101917        UPDATE
101917        DATASET  ('ELTRLR')
101917        SET      (ADDRESS OF ACTIVITY-TRAILERS)
101917        RIDFLD   (TRLR-KEY)
101917        RESP     (WS-RESPONSE)
101917     END-EXEC
101917     IF WS-RESP-NORMAL
101917        IF EMI-ERROR =  ER-1679
101917           IF EMI-ERROR-NUMBER(1) = ER-1679
101917              MOVE EMI-LINE1 TO AT-INFO-LINE-1
101917           ELSE
101917           IF EMI-ERROR-NUMBER(2) = ER-1679
101917              MOVE EMI-LINE2 TO AT-INFO-LINE-1
101917           ELSE
101917           IF EMI-ERROR-NUMBER(3) = ER-1679
101917              MOVE EMI-LINE3 TO AT-INFO-LINE-1
101917           END-IF
101917           END-IF
101917           END-IF
101917           EXEC CICS REWRITE
101917              DATASET   ('ELTRLR')
101917              FROM      (ACTIVITY-TRAILERS)
101917           END-EXEC
101917        ELSE
061418           IF EMI-ERROR = 1
101917              MOVE WS-CONTEST-NOTE TO AT-INFO-LINE-1
061418              MOVE ZERO TO EMI-ERROR
101917              EXEC CICS REWRITE
101917                 DATASET   ('ELTRLR')
101917                 FROM      (ACTIVITY-TRAILERS)
101917              END-EXEC
061418           ELSE
061418              EXEC CICS DELETE
061418                 DATASET   ('ELTRLR')
061418              END-EXEC
061418           END-IF
101917        END-IF
101917     ELSE
061418     IF EMI-ERROR =  ER-1679
101917        EXEC CICS GETMAIN
101917           SET      (ADDRESS OF ACTIVITY-TRAILERS)
101917           LENGTH   (TRLR-LENGTH)
101917           INITIMG  (GETMAIN-SPACE)
101917        END-EXEC
101917        MOVE 'AT'          TO AT-RECORD-ID
101917        MOVE CL-CONTROL-PRIMARY
101917                           TO AT-CONTROL-PRIMARY
101917        MOVE +96           TO AT-SEQUENCE-NO
101917        MOVE '6'           TO AT-TRAILER-TYPE
101917        MOVE SPACES             TO AT-GENERAL-INFO-TR
101917        INITIALIZE AT-GENERAL-INFO-TR
101917        IF EMI-ERROR-NUMBER(1) = ER-1679
101917           MOVE EMI-LINE1 TO AT-INFO-LINE-1
101917        ELSE
101917        IF EMI-ERROR-NUMBER(2) = ER-1679
101917           MOVE EMI-LINE2 TO AT-INFO-LINE-1
101917        ELSE
101917        IF EMI-ERROR-NUMBER(3) = ER-1679
101917           MOVE EMI-LINE3 TO AT-INFO-LINE-1
101917        END-IF
101917        END-IF
101917        END-IF
101917        MOVE SPACE           TO AT-INFO-TRAILER-TYPE
101917        MOVE SAVE-BIN-DATE TO AT-RECORDED-DT
101917                              AT-GEN-INFO-LAST-MAINT-DT
101917        MOVE PI-PROCESSOR-ID
101917                           TO AT-RECORDED-BY
101917                              AT-GEN-INFO-LAST-UPDATED-BY
101917        MOVE EIBTIME       TO AT-LAST-MAINT-HHMMSS
101917        EXEC CICS WRITE
101917           DATASET   ('ELTRLR')
101917           FROM      (ACTIVITY-TRAILERS)
101917           RIDFLD    (TRLR-KEY)
101917        END-EXEC
061418     END-IF
101917     END-IF
101917     .
101917 2615-EXIT.
101917     EXIT.
101917
090821 2620-CHECK-TRLR.
090821
090821     MOVE SPACES                 TO AT-GENERAL-INFO-TR
090821     INITIALIZE AT-GENERAL-INFO-TR
090821     MOVE MSTR-KEY               TO AT-CONTROL-PRIMARY
090821     MOVE 'AT'                   TO AT-RECORD-ID
090821     evaluate true
090821        when emi-error = er-1679
090821           move er-1679-text     to at-info-line-1
090821        when emi-error = er-1682
090821           move er-1682-text     to at-info-line-1
090821        when emi-error = er-1683
090821           move er-1683-text     to at-info-line-1
090821     end-evaluate
090821     move +97                    to at-sequence-no
090821     MOVE '6'                    TO AT-TRAILER-TYPE
090821     MOVE save-bin-date          TO AT-RECORDED-DT
090821                                    AT-GEN-INFO-LAST-MAINT-DT
090821     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
090821                                    AT-GEN-INFO-LAST-UPDATED-BY
090821     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
090821
090821     MOVE SPACES                 TO AT-INFO-LINE-2
090821
090821     .
090821 2620-WRITE.
090821
090821     EXEC CICS HANDLE CONDITION
090821         DUPREC    (2620-DUPREC)
090821     END-EXEC.
090821
090821     EXEC CICS WRITE
090821          DATASET     ('ELTRLR')
090821          FROM        (ACTIVITY-TRAILERS)
090821          RIDFLD      (AT-CONTROL-PRIMARY)
090821      END-EXEC
090821
090821     GO TO 2620-EXIT
090821
090821     .
090821 2620-DUPREC.
090821
090821     SUBTRACT +1 FROM AT-SEQUENCE-NO
090821     GO TO 2620-WRITE
090821
090821     .
090821 2620-EXIT.
090821     EXIT.


071720*0529182620-CHECK-TRLR.
071720*052918
071720*052918MOVE CL-CONTROL-PRIMARY   TO TRLR-KEY
071720*052918MOVE +97                  TO TRLR-SEQ-NO
071720*052918EXEC CICS READ
071720*052918   UPDATE
071720*052918   DATASET  ('ELTRLR')
071720*052918   SET      (ADDRESS OF ACTIVITY-TRAILERS)
071720*052918   RIDFLD   (TRLR-KEY)
071720*052918   RESP     (WS-RESPONSE)
071720*052918END-EXEC
071720*052918IF WS-RESP-NORMAL
071720*052918   IF EMI-ERROR =  ER-7572
071720*052918      MOVE WS-FILING-NOTE TO AT-INFO-LINE-1
071720*052918      EXEC CICS REWRITE
071720*052918         DATASET   ('ELTRLR')
071720*052918         FROM      (ACTIVITY-TRAILERS)
071720*052918      END-EXEC
071720*052918   ELSE
071720*052918      MOVE WS-FILE-LIM-NOTE TO AT-INFO-LINE-1
071720*052918         EXEC CICS REWRITE
071720*052918            DATASET   ('ELTRLR')
071720*052918            FROM      (ACTIVITY-TRAILERS)
071720*052918         END-EXEC
071720*052918   END-IF
071720*052918ELSE
071720*052918   IF EMI-ERROR =  ER-7572
071720*052918      EXEC CICS GETMAIN
071720*052918         SET      (ADDRESS OF ACTIVITY-TRAILERS)
071720*052918         LENGTH   (TRLR-LENGTH)
071720*052918         INITIMG  (GETMAIN-SPACE)
071720*052918      END-EXEC
071720*052918      MOVE 'AT'          TO AT-RECORD-ID
071720*052918      MOVE CL-CONTROL-PRIMARY
071720*052918                         TO AT-CONTROL-PRIMARY
071720*052918      MOVE +97           TO AT-SEQUENCE-NO
071720*052918      MOVE '6'           TO AT-TRAILER-TYPE
071720*052918      MOVE SPACES             TO AT-GENERAL-INFO-TR
071720*052918      INITIALIZE AT-GENERAL-INFO-TR
071720*052918      MOVE WS-FILING-NOTE TO AT-INFO-LINE-1
071720*052918      MOVE SPACE           TO AT-INFO-TRAILER-TYPE
071720*052918      MOVE SAVE-BIN-DATE TO AT-RECORDED-DT
071720*052918                            AT-GEN-INFO-LAST-MAINT-DT
071720*052918      MOVE PI-PROCESSOR-ID
071720*052918                         TO AT-RECORDED-BY
071720*052918                            AT-GEN-INFO-LAST-UPDATED-BY
071720*052918      MOVE EIBTIME       TO AT-LAST-MAINT-HHMMSS
071720*052918      EXEC CICS WRITE
071720*052918         DATASET   ('ELTRLR')
071720*052918         FROM      (ACTIVITY-TRAILERS)
071720*052918         RIDFLD    (TRLR-KEY)
071720*052918      END-EXEC
071720*052918   END-IF
071720*052918END-IF
071720*052918.
071720*0529182620-EXIT.
071720*052918EXIT.

03833                                                                      CL*34
03834  2650-WRITE-MAINT-NOTE.                                              CL*34
03835      EXEC CICS GETMAIN                                               CL*34
03836           SET      (ADDRESS OF ACTIVITY-TRAILERS)                    CL*34
03837           LENGTH   (TRLR-LENGTH)                                     CL*34
03838           INITIMG  (GETMAIN-SPACE)                                   CL*34
03839      END-EXEC.                                                       CL*34
03840                                                                      CL*34
03841      EXEC CICS HANDLE CONDITION                                      CL*34
03842           DUPREC   (2650-EXIT)                                       CL*34
03843      END-EXEC.                                                       CL*34
03844                                                                      CL*34
03845      MOVE SPACES             TO ACTIVITY-TRAILERS.                   CL*34
03846                                                                      CL*39
03847      IF CERTL GREATER ZERO                                           CL*39
03848          MOVE CERTI          TO CL-CERT-PRIME.                       CL*39
03849                                                                      CL*39
03850      IF SUFXL GREATER ZERO                                           CL*39
03851          MOVE SUFXI          TO CL-CERT-SFX.                         CL*39
03852                                                                      CL*34
03853      MOVE CL-CONTROL-PRIMARY TO AT-CONTROL-PRIMARY.                  CL*34
03854                                                                      CL*34
03855      SUBTRACT 1 FROM  CL-TRAILER-SEQ-CNT.                            CL*34
03856      MOVE CL-TRAILER-SEQ-CNT TO AT-SEQUENCE-NO.                      CL*34
03857                                                                      CL*34
03858      MOVE 'AT'               TO AT-RECORD-ID.                        CL*34
03859                                                                      CL*34
03860      MOVE '6'                TO AT-TRAILER-TYPE.                     CL*34
03861      MOVE SAVE-BIN-DATE      TO AT-RECORDED-DT                       CL*34
03862                                 AT-GEN-INFO-LAST-MAINT-DT.           CL*34
03863      MOVE PI-PROCESSOR-ID    TO AT-RECORDED-BY                       CL*34
03864                                 AT-GEN-INFO-LAST-UPDATED-BY.         CL*34
03865      MOVE EIBTIME            TO AT-LAST-MAINT-HHMMSS.                CL*34
03866      MOVE SPLIT-INFO-LINE-1  TO AT-INFO-LINE-1.                      CL*34
03867      MOVE 'M'                TO AT-INFO-TRAILER-TYPE.                CL*34
03868                                                                      CL*34
03869      EXEC CICS WRITE                                                 CL*34
03870           DATASET  (TRLR-FILE-ID)                                    CL*34
03871           RIDFLD   (AT-CONTROL-PRIMARY)                              CL*34
03872           FROM     (ACTIVITY-TRAILERS)                               CL*34
03873      END-EXEC.                                                       CL*34
03874                                                                      CL*34
03875  2650-EXIT.                                                          CL*34
03876       EXIT.                                                          CL*34
03877                                                                      CL*34
060413 2800-CHECK-AUTO-PAY.
060413
060413     MOVE PI-COMPANY-CD      TO TRLR-COMPANY-CD.
060413     MOVE PI-CARRIER         TO TRLR-CARRIER.
060413     MOVE PI-CLAIM-NO        TO TRLR-CLAIM-NO.
060413     MOVE PI-CERT-NO         TO TRLR-CERT-NO.
060413     MOVE PI-AUTO-PAY-SEQ    TO TRLR-SEQ-NO.
060413
060413     EXEC CICS HANDLE CONDITION
060413          NOTFND    (2800-EXIT)
060413          ENDFILE   (2800-EXIT)
060413     END-EXEC.
060413
060413     EXEC CICS READ
060413         DATASET (TRLR-FILE-ID)
060413         SET     (ADDRESS OF ACTIVITY-TRAILERS)
060413         RIDFLD  (TRLR-KEY)
060413     END-EXEC.
060413
060413     IF BENEFICIARY-PAID-AUTO
060413         MOVE 'Y'              TO AUTO-PAY-TO-BENE
060413     END-IF.
060413
060413 2800-EXIT.
060413      EXIT.
060413
03878      EJECT                                                        EL131
03879  3000-DELETE-CLAIM.                                               EL131
03880      IF  NOT MODIFY-CAP                                           EL131
03881          MOVE ER-0070            TO EMI-ERROR                     EL131
03882          MOVE -1                 TO MAINTL                        EL131
03883          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL131
03884          GO TO 8110-SEND-DATA.                                    EL131
03885                                                                   EL131
03886      EXEC CICS READ                                               EL131
03887           SET      (ADDRESS OF CLAIM-MASTER)                         CL*32
03888           DATASET  (CLMS-FILE-ID)                                 EL131
03889           RIDFLD   (MSTR-KEY)                                     EL131
03890      END-EXEC.                                                    EL131
03891                                                                   EL131
03892      IF CL-1ST-TRL-AVAIL                                          EL131
03893          PERFORM 3010-DEL-MSTR-TRLR THRU 3010-EXIT                   CL*34
03894      ELSE                                                         EL131
03895         MOVE MSTR-KEY            TO TRLR-MAIN-KEY                 EL131
03896         MOVE 0                   TO TRLR-SEQ-NO                   EL131
03897          EXEC CICS STARTBR                                        EL131
03898              DATASET (TRLR-FILE-ID)                               EL131
03899              RIDFLD  (TRLR-KEY)                                   EL131
03900          END-EXEC                                                 EL131
03901          PERFORM 3060-READ-TRLR THRU 3060-EXIT                       CL*34
03902          EXEC CICS ENDBR                                          EL131
03903              DATASET (TRLR-FILE-ID)                               EL131
03904          END-EXEC                                                 EL131
03905         IF SCREEN-ERROR                                           EL131
03906             MOVE ER-0208         TO EMI-ERROR                     EL131
03907             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 CL*34
03908             MOVE -1              TO MAINTL                        EL131
03909             GO TO 8110-SEND-DATA                                  EL131
03910         ELSE                                                      EL131
03911             PERFORM 3010-DEL-MSTR-TRLR THRU 3010-EXIT.               CL*34
03912                                                                   EL131
03913      IF SCREEN-ERROR                                              EL131
03914          MOVE -1                 TO MAINTL                        EL131
03915          GO TO 8110-SEND-DATA.                                    EL131
03916                                                                      CL**8
03917      MOVE -1                     TO ONE-OR-MIN1.                     CL*34
03918      PERFORM 7700-CHECK-SEQUENCE THRU 7799-EXIT.                     CL**8
03919                                                                      CL**8
03920      IF WS-ASSOC-CERT-TOTAL NOT = ZERO                               CL*34
03921          MOVE CL-CONTROL-PRIMARY TO MSTR-KEY                         CL**8
03922                                     WS-SAVE-CLAIM-KEY                CL**8
03923          MOVE +1                 TO ONE-OR-MIN1                      CL*34
03924          PERFORM 7710-RESEQUENCE-CLAIMS THRU 7799-EXIT.              CL*34
03925                                                                      CL**8
03926      MOVE 'D'                    TO PI-CLAIM-DELETED-SWITCH.         CL**8
03927                                                                   EL131
03928      MOVE LOW-VALUES             TO EL131AO.                      EL131
03929      MOVE ER-0000                TO EMI-ERROR.                    EL131
03930      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
03931      MOVE 'X'                    TO PI-RETURN-CD-1.               EL131
03932      MOVE -1                     TO MAINTL.                       EL131
03933      GO TO 8100-SEND-MAP.                                         EL131
03934                                                                   EL131
03935  3010-DEL-MSTR-TRLR.                                              EL131
03936      EXEC CICS READ UPDATE                                        EL131
03937           SET       (ADDRESS OF CLAIM-MASTER)                        CL*32
03938           DATASET   (CLMS-FILE-ID)                                EL131
03939           RIDFLD    (MSTR-KEY)                                    EL131
03940      END-EXEC.                                                    EL131
03941                                                                   EL131
062602     if (cl-priority-cd = '8')
062602        and (pi-processor-id not = 'PEMA' and 'JMS '
062602             AND 'AMWA')
062602        MOVE ER-8003             TO EMI-ERROR
062602        PERFORM 9900-ERROR-FORMAT
062602                                 THRU 9900-EXIT
062602        MOVE -1                  TO MAINTL
062602        GO TO 8110-SEND-DATA
062602     end-if
062602
03942      MOVE MSTR-KEY               TO TRLR-MAIN-KEY ACTQ-KEY.       EL131
03943      MOVE ZERO                   TO TRLR-SEQ-NO.                  EL131
03944                                                                   EL131
03945      EXEC CICS HANDLE CONDITION                                   EL131
03946           NOTFND    (3010-DEL-ACTQ)                                  CL*22
03947           ENDFILE   (3040-EXIT)                                   EL131
03948      END-EXEC.                                                    EL131
03949                                                                   EL131
03950      PERFORM 3040-DELETE-TRLR THRU 3040-EXIT.                        CL*34
03951                                                                      CL*22
03952  3010-DEL-ACTQ.                                                      CL*22
03953                                                                   EL131
03954      EXEC CICS HANDLE CONDITION                                   EL131
03955           NOTFND    (3010-NO-ACTIVITY)                            EL131
03956           ENDFILE   (3050-EXIT)                                      CL*34
03957      END-EXEC.                                                    EL131
03958                                                                   EL131
03959      EXEC CICS STARTBR                                            EL131
03960           DATASET  (ACTQ-FILE-ID)                                 EL131
03961           RIDFLD   (ACTQ-KEY)                                     EL131
03962      END-EXEC.                                                    EL131
03963                                                                   EL131
03964      EXEC CICS GETMAIN                                            EL131
03965           SET      (ADDRESS OF ACTIVITY-QUE)                         CL*32
03966           LENGTH   (ACTQ-LENGTH)                                  EL131
03967           INITIMG  (GETMAIN-SPACE)                                EL131
03968      END-EXEC.                                                    EL131
03969                                                                   EL131
03970      PERFORM 3050-DELETE-ACTQ THRU 3050-EXIT.                        CL*34
03971                                                                   EL131
03972      EXEC CICS ENDBR                                              EL131
03973           DATASET (ACTQ-FILE-ID)                                  EL131
03974      END-EXEC.                                                    EL131
03975                                                                   EL131
03976  3010-NO-ACTIVITY.                                                EL131
03977                                                                      CL*28
03978      EXEC CICS HANDLE CONDITION                                      CL*28
03979          NOTFND   (3010-CERT-NOT-FOUND)                              CL*28
03980      END-EXEC.                                                       CL*28
03981                                                                      CL*28
03982      MOVE CL-COMPANY-CD          TO CERT-COMPANY-CODE.            EL131
03983      MOVE CL-CERT-CARRIER        TO CERT-CARRIER.                 EL131
03984      MOVE CL-CERT-GROUPING       TO CERT-GROUP.                   EL131
03985      MOVE CL-CERT-STATE          TO CERT-STATE.                   EL131
03986      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.                 EL131
03987      MOVE CL-CERT-EFF-DT         TO CERT-DATE.                    EL131
03988      MOVE CL-CERT-NO             TO CERT-CERT.                    EL131
03989                                                                   EL131
03990      EXEC CICS READ                                                  CL*28
03991          DATASET   (CERT-FILE-ID)                                    CL*28
03992          RIDFLD    (CERT-KEY)                                        CL*28
03993          SET       (ADDRESS OF CERTIFICATE-MASTER)                   CL*32
03994      END-EXEC.                                                       CL*28
03995                                                                      CL*28
03996      IF CERT-WAS-CREATED AND CM-CLAIM-ATTACHED-COUNT = 1          EL131
062121       AND PI-COMPANY-ID NOT EQUAL 'AHL' AND 'FNL'
03997          PERFORM 3030-DELETE-CERT THRU 3030-EXIT                     CL*34
03998      ELSE                                                         EL131
03999          PERFORM 3020-UPDATE-CERT THRU 3020-EXIT.                    CL*34
04000                                                                      CL*28
052113 3010-UPDATE-CERT-TRLR.
052113
052113     MOVE CERT-KEY               TO ELCRTT-KEY
052113     MOVE 'B'                    TO CTRLR-REC-TYPE
052113     EXEC CICS READ
052113        UPDATE
052113        DATASET   ('ELCRTT')
052113        RIDFLD    (ELCRTT-KEY)
052113        INTO      (CERTIFICATE-TRAILERS)
052113        RESP      (WS-RESPONSE)
052113     END-EXEC            
052113     IF WS-RESP-NORMAL
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or (cs-claim-no (s1) = cl-claim-no)
              end-perform
              if s1 < +25
                 move spaces           to cs-claim-no (s1)
                                          cs-claim-type (s1)
                                          cs-insured-type (s1)
                 move zeros            to cs-days-paid (s1)
                                          cs-total-paid (s1)
                                          cs-benefit-period (s1)
                 if s1 not = +24
                    compute s2 = s1 + +1
                    perform 3015-bump  thru 3015-exit
                 end-if
052113           EXEC CICS REWRITE
052113              DATASET   ('ELCRTT')
052113              FROM      (CERTIFICATE-TRAILERS)
052113           END-EXEC
              end-if
           end-if

           .
04001  3010-CERT-NOT-FOUND.                                                CL*28
04002                                                                   EL131
04003      EXEC CICS DELETE                                             EL131
04004          DATASET  (CLMS-FILE-ID)                                  EL131
04005      END-EXEC.                                                    EL131
04006                                                                   EL131
04007  3010-EXIT.                                                          CL*34
04008       EXIT.                                                          CL*46
04009                                                                   EL131
       3015-bump.

           perform varying s2 from s2 by +1 until s2 > +24
              move cs-mb-claim-data (s2)
                                       to cs-mb-claim-data (s1)
              move spaces              to cs-mb-claim-data (s2)
              add +1 to s1
           end-perform

           .
       3015-exit.
           exit.

04010  3020-UPDATE-CERT.                                                EL131
04011      EXEC CICS READ UPDATE                                        EL131
04012           DATASET  (CERT-FILE-ID)                                 EL131
04013           RIDFLD   (CERT-KEY)                                     EL131
04014           SET      (ADDRESS OF CERTIFICATE-MASTER)                   CL*32
04015      END-EXEC.                                                    EL131
04016                                                                   EL131
04017      SUBTRACT 1       FROM CM-CLAIM-ATTACHED-COUNT.               EL131
04018                                                                   EL131
04019      EXEC CICS HANDLE CONDITION                                   EL131
04020           DUPKEY   (3020-EXIT)                                    EL131
04021      END-EXEC.                                                    EL131
04022                                                                   EL131
04023      EXEC CICS REWRITE                                            EL131
04024           DATASET  (CERT-FILE-ID)                                 EL131
04025           FROM     (CERTIFICATE-MASTER)                           EL131
04026      END-EXEC.                                                    EL131
04027                                                                   EL131
04028  3020-EXIT.                                                       EL131
04029      EXIT.                                                        EL131
04030                                                                   EL131
04031  3030-DELETE-CERT.                                                EL131
04032      EXEC CICS READ UPDATE                                        EL131
04033           DATASET  (CERT-FILE-ID)                                 EL131
04034           RIDFLD   (CERT-KEY)                                     EL131
04035           SET      (ADDRESS OF CERTIFICATE-MASTER)                   CL*32
04036      END-EXEC.                                                    EL131
04037                                                                   EL131
04038      EXEC CICS DELETE                                             EL131
04039           DATASET   (CERT-FILE-ID)                                EL131
04040      END-EXEC.                                                    EL131
04041                                                                      CL*27
04042  3030-EXIT.                                                       EL131
04043      EXIT.                                                        EL131
04044                                                                      CL*34
04045  3040-DELETE-TRLR.                                                EL131
04046      EXEC CICS READ                                               EL131
04047           SET      (ADDRESS OF ACTIVITY-TRAILERS)                    CL*32
04048           DATASET  (TRLR-FILE-ID)                                 EL131
04049           RIDFLD   (TRLR-KEY)                                     EL131
04050           GTEQ                                                    EL131
04051      END-EXEC.                                                    EL131
04052                                                                   EL131
04053      IF PI-COMPANY-CD = AT-COMPANY-CD                             EL131
04054         MOVE AT-CONTROL-PRIMARY  TO TRLR-KEY                      EL131
04055         IF TRLR-MAIN-KEY GREATER THAN CL-CONTROL-PRIMARY          EL131
04056            GO TO 3040-EXIT                                        EL131
04057         ELSE                                                      EL131
04058            NEXT SENTENCE                                          EL131
04059      ELSE                                                         EL131
04060         GO TO 3040-EXIT.                                          EL131
04061                                                                   EL131
04062      EXEC CICS DELETE                                             EL131
04063           DATASET  (TRLR-FILE-ID)                                 EL131
04064           RIDFLD   (TRLR-KEY)                                     EL131
04065      END-EXEC.                                                    EL131
04066                                                                   EL131
04067      GO TO 3040-DELETE-TRLR.                                      EL131
04068                                                                      CL*34
04069  3040-EXIT.                                                       EL131
04070      EXIT.                                                        EL131
04071                                                                      CL*34
04072  3050-DELETE-ACTQ.                                                EL131
04073      EXEC CICS READNEXT                                           EL131
04074           DATASET  (ACTQ-FILE-ID)                                 EL131
04075           RIDFLD   (ACTQ-KEY)                                     EL131
04076           INTO     (ACTIVITY-QUE)                                 EL131
04077      END-EXEC.                                                    EL131
04078                                                                   EL131
04079      IF ACTQ-KEY GREATER THAN CL-CONTROL-PRIMARY                  EL131
04080          GO TO 3050-EXIT.                                            CL*34
04081                                                                   EL131
04082      EXEC CICS ENDBR                                                 CL**2
04083           DATASET  (ACTQ-FILE-ID)                                    CL**2
04084      END-EXEC.                                                       CL**2
04085                                                                      CL**2
04086      EXEC CICS DELETE                                             EL131
04087           DATASET   (ACTQ-FILE-ID)                                EL131
04088           RIDFLD    (ACTQ-KEY)                                    EL131
04089      END-EXEC.                                                    EL131
04090                                                                      CL**2
04091      EXEC CICS STARTBR                                               CL**2
04092           DATASET   (ACTQ-FILE-ID)                                   CL**2
04093           RIDFLD    (ACTQ-KEY)                                       CL**2
04094      END-EXEC.                                                       CL**2
04095                                                                   EL131
04096      GO TO 3050-DELETE-ACTQ.                                      EL131
04097                                                                      CL*34
04098  3050-EXIT.                                                          CL*34
04099      EXIT.                                                        EL131
04100      EJECT                                                        EL131
04101  3060-READ-TRLR.                                                  EL131
04102      EXEC CICS HANDLE CONDITION                                   EL131
04103           NOTFND    (3060-EXIT)                                   EL131
04104           ENDFILE   (3060-EXIT)                                   EL131
04105      END-EXEC.                                                    EL131
04106                                                                   EL131
04107  3060-LOOP.                                                       EL131
04108      EXEC CICS READNEXT                                           EL131
04109           SET      (ADDRESS OF ACTIVITY-TRAILERS)                    CL*32
04110           DATASET  (TRLR-FILE-ID)                                 EL131
04111           RIDFLD   (TRLR-KEY)                                     EL131
04112      END-EXEC.                                                    EL131
04113                                                                   EL131
04114      IF TRLR-MAIN-KEY GREATER THAN CL-CONTROL-PRIMARY             EL131
04115          GO TO 3060-EXIT.                                         EL131
04116                                                                   EL131
04117      IF FORM-CONTROL-TR                                              CL*17
04118          IF AT-RECORDED-DT = SAVE-BIN-DATE                           CL*34
04119              NEXT SENTENCE                                           CL*17
04120          ELSE                                                        CL*17
04121              MOVE 'X'            TO ERROR-SWITCH                     CL*17
04122              GO TO 3060-EXIT                                         CL*17
04123      ELSE                                                            CL*17
04124          IF PAYMENT-TR OR CORRESPONDENCE-TR                          CL*17
04125             MOVE 'X'                 TO ERROR-SWITCH                 CL*17
04126             GO TO 3060-EXIT.                                         CL*17
04127                                                                   EL131
04128      GO TO 3060-LOOP.                                             EL131
04129                                                                   EL131
04130  3060-EXIT.                                                       EL131
04131      EXIT.                                                        EL131
04132      EJECT                                                        EL131

052113 3997-GET-ERPDEF.
052113
052113     move cl-company-cd          to cert-company-code
052113     move cl-cert-key-data       to cert-key (2:21)
052113     move cl-cert-no             to cert-cert
052113
052113     EXEC CICS READ
052113         DATASET   (CERT-FILE-ID)
052113         RIDFLD    (CERT-KEY)
052113         SET       (ADDRESS OF CERTIFICATE-MASTER)
052113         resp      (ws-response)
052113     END-EXEC
052113
052113     MOVE SPACES                 TO WS-CRIT-PER-ALPHA
052113                                    WS-ERPDEF-SW
052113     MOVE ZEROS                  TO WS-CRITICAL-PERIOD
052113                                    WS-CRIT-PER-RTW-MOS
052113
           if cm-clp-state = spaces or low-values or zeros
              move cm-state            to cm-clp-state
           end-if
052113     MOVE PI-COMPANY-CD          TO ERPDEF-KEY
052113     MOVE CM-clp-state           TO ERPDEF-STATE
052113     MOVE am-dcc-product-code    TO ERPDEF-PROD-CD

070714     evaluate true
100518        when (cl-claim-type = 'L' or 'P' OR 'O')
070714           and (cm-lf-benefit-cd not = '00' and '  ' and 'DD'
070714             and 'CU')
070714           move 'L'              to erpdef-ben-type
070714           move cm-lf-benefit-cd to erpdef-ben-code
100518        when (cl-claim-type not = 'L' and 'P' AND 'O')
070714           and (cm-ah-benefit-cd not = '00' and '  ')
070714           move 'A'              to erpdef-ben-type
070714           move cm-ah-benefit-cd to erpdef-ben-code
100518        when (cl-claim-type not = 'L' and 'P' AND 'O')
070714           and (cm-ah-benefit-cd = '00' or '  ')
070714           move 'L'              to erpdef-ben-type
070714           move cm-lf-benefit-cd to erpdef-ben-code
100518        when (cl-claim-type = 'L' or 'P' OR 'O')
070714           and (cm-lf-benefit-cd = '00' or '  ' or 'DD' or 'CU')
070714           move 'A'              to erpdef-ben-type
070714           move cm-ah-benefit-cd to erpdef-ben-code
070714        when other
070714           move 'A'              to erpdef-ben-type
070714           move cm-ah-benefit-cd to erpdef-ben-code
070714     end-evaluate
070714
070714*    if (cl-claim-type = 'L' or 'P')
070714*       and (cm-lf-benefit-cd not = '00' and '  ' and 'DD'
070714*          and 'CU')
070714*       move 'L'                 to erpdef-ben-type
070714*       move cm-lf-benefit-cd    to erpdef-ben-code
070714*    else
070714*       MOVE 'A'                 TO ERPDEF-BEN-TYPE
070714*       MOVE CM-AH-BENEFIT-CD    TO ERPDEF-BEN-CODE
070714*    end-if

PEMTST     move cl-insured-birth-dt    to dc-bin-date-1
           move cl-incurred-dt         to dc-bin-date-2
           move '1'                    to dc-option-code
           PERFORM 9800-CONVERT-DATE   THRU 9800-EXIT
           compute ws-att-age =
              dc-elapsed-months / 12

081817     MOVE '6'                    TO DC-OPTION-CODE
           move zeros                  to dc-elapsed-months
                                          dc-elapsed-days
           move low-values to dc-bin-date-1 dc-bin-date-2

052113     MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
052113     MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
052113
052113     EXEC CICS STARTBR
052113         DATASET  ('ERPDEF')
052113         RIDFLD   (ERPDEF-KEY)
052113         GTEQ
052113         RESP     (WS-RESPONSE)
052113     END-EXEC
052113
052113     IF WS-RESP-NORMAL
052113        EXEC CICS READNEXT
052113           DATASET  ('ERPDEF')
052113           INTO     (PRODUCT-MASTER)
052113           RIDFLD   (ERPDEF-KEY)
052113           RESP     (WS-RESPONSE)
052113        END-EXEC

052113        IF WS-RESP-NORMAL
052113           IF (ERPDEF-KEY-SAVE (1:16) =
052113              PD-CONTROL-PRIMARY (1:16))
052113              AND (CM-CERT-EFF-DT < PD-PROD-EXP-DT)

052113              PERFORM VARYING A1 FROM +1 BY +1 UNTIL
052113                 (A1 > +11)
052113                 OR ((PD-PROD-CODE (A1) = cl-claim-type)
                        AND (PD-MAX-ATT-AGE (a1) >= WS-ATT-AGE))
052113              END-PERFORM
052113              IF A1 < +12

052113                 SET ERPDEF-FOUND TO TRUE
052113                 MOVE PD-CRIT-PERIOD (A1)
052113                              TO WS-CRITICAL-PERIOD
052113                 MOVE PD-REC-CRIT-PERIOD (A1)
052113                              TO WS-CRIT-PER-RECURRENT
052113                 IF PD-RTW-MOS (A1) NUMERIC
052113                    MOVE PD-RTW-MOS (A1)
052113                              TO WS-CRIT-PER-RTW-MOS
052113                 ELSE
052113                    MOVE 0    TO WS-CRIT-PER-RTW-MOS
052113                 END-IF
052113                 IF PD-EXCLUSION-PERIOD-DAYS (A1) NUMERIC
052113                    MOVE PD-EXCLUSION-PERIOD-DAYS (A1)
052113                                 TO WS-EXCL-PERIOD
052113                 END-IF
052113                 IF PD-COVERAGE-ENDS-MOS (A1) NUMERIC
052113                    MOVE PD-COVERAGE-ENDS-MOS (A1)
052113                                 TO WS-COV-ENDS
052113                 END-IF
052113                 IF PD-ACCIDENT-ONLY-MOS (A1) NUMERIC
052113                    MOVE PD-ACCIDENT-ONLY-MOS (A1)
052113                                 TO WS-ACC-PERIOD
052113                 END-IF
                       if pd-max-extension (a1) numeric
                          move pd-max-extension (a1)
                                       to ws-max-extension
                       end-if
                       if pd-max-amt (a1) numeric
                          move pd-max-amt (a1)
                                       to ws-max-moben
                       end-if
                       if pd-pre-exist-excl-type(a1) numeric
                          move pd-pre-exist-excl-type(a1)
                                       to ws-pre-exsist
                       end-if
052113              END-IF
052113           END-IF
052113        END-IF
052113     END-IF
052113
052113     .
052113 3997-EXIT.
052113     EXIT.

04133  4000-FORCE-ERRORS.                                               EL131

04134      IF PI-RETURN-CD-1 = 'X'                                      EL131
04135          MOVE ER-0311            TO EMI-ERROR                     EL131
04136          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
04137          MOVE -1                 TO MAINTL                        EL131
04138          GO TO 8110-SEND-DATA.                                    EL131
04139                                                                   EL131
04140      IF NOT FORCE-CAP                                             EL131
04141          MOVE ER-0416            TO EMI-ERROR                     EL131
04142          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
04143          MOVE -1                 TO MAINTL                        EL131
04144          GO TO 8110-SEND-DATA.                                    EL131
04145                                                                   EL131
04146      MOVE 'F'                    TO EMI-ACTION-SWITCH.            EL131
04147      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.                     EL131
04148      IF EMI-FATAL-CTR GREATER THAN ZERO                              CL*34
04149          GO TO 4000-FATAL-ERRORS.                                 EL131
04150                                                                   EL131
04151      PERFORM 2000-UPDATE-CLAIM THRU 2000-EXIT.                    EL131
04152                                                                   EL131
04153      IF EMI-FATAL-CTR GREATER THAN ZERO                           EL131
04154          GO TO 4000-FATAL-ERRORS.                                 EL131
04155                                                                   EL131
04156      PERFORM 5000-BUILD-MAP THRU 5000-EXIT.                          CL*34
04157                                                                   EL131
04158      IF EMI-FATAL-CTR GREATER THAN ZERO                           EL131
04159          GO TO 4000-FATAL-ERRORS.                                 EL131
04160                                                                   EL131
04161      MOVE SPACE                  TO EMI-ACTION-SWITCH MAINTO.     EL131
04162      MOVE AL-UANOF               TO CERTA.                        EL131
04163      MOVE ER-0000 TO EMI-ERROR.                                   EL131
04164      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
04165                                                                   EL131
04166  4000-FATAL-ERRORS.                                               EL131
04167      MOVE -1                     TO MAINTL.                       EL131
04168      GO TO 8110-SEND-DATA.                                        EL131
04169      EJECT                                                        EL131
082013
082013 4100-SAVE-CHANGES.
082013     IF MAINTL > 0
082013         MOVE MAINTI  TO WS-SAVE-MAINTI
082013         MOVE MAINTL  TO WS-SAVE-MAINTL
082013     END-IF
082013     IF STATUSL > 0
082013         MOVE STATUSI  TO WS-SAVE-STATUSI
082013         MOVE STATUSL  TO WS-SAVE-STATUSL
082013     END-IF
082013     IF REPL > 0
082013         MOVE REPI  TO WS-SAVE-REPI
082013         MOVE REPL  TO WS-SAVE-REPL
082013     END-IF
040814*    IF CAUSEL > 0
040814*        MOVE CAUSEI  TO WS-SAVE-CAUSEI
040814*        MOVE CAUSEL  TO WS-SAVE-CAUSEL
040814*    END-IF
040814*    IF ENDL > 0
040814*        MOVE ENDI  TO WS-SAVE-ENDI
040814*        MOVE ENDL  TO WS-SAVE-ENDL
040814*    END-IF
082013     IF DIAGL > 0
082013         MOVE DIAGI  TO WS-SAVE-DIAGI
082013         MOVE DIAGL  TO WS-SAVE-DIAGL
082013     END-IF
040814     IF ICD1L > 0
040814         MOVE ICD1I  TO WS-SAVE-ICD1I
040814         MOVE ICD1L  TO WS-SAVE-ICD1L
040814     END-IF
040814     IF ICD2L > 0
040814         MOVE ICD2I  TO WS-SAVE-ICD2I
040814         MOVE ICD2L  TO WS-SAVE-ICD2L
040814     END-IF
082013     IF BENEL > 0
082013         MOVE BENEI  TO WS-SAVE-BENEI
082013         MOVE BENEL  TO WS-SAVE-BENEL
082013     END-IF
082013     IF BIRTHL > 0
082013         MOVE BIRTHI  TO WS-SAVE-BIRTHI
082013         MOVE BIRTHL  TO WS-SAVE-BIRTHL
082013     END-IF
082013     IF SOCIALL > 0
082013         MOVE SOCIALI  TO WS-SAVE-SOCIALI
082013         MOVE SOCIALL  TO WS-SAVE-SOCIALL
082013     END-IF
082013     IF SEXL > 0
082013         MOVE SEXI  TO WS-SAVE-SEXI
082013         MOVE SEXL  TO WS-SAVE-SEXL
082013     END-IF
082013     IF MLNAMEL > 0
082013         MOVE MLNAMEI  TO WS-SAVE-MLNAMEI
082013         MOVE MLNAMEL  TO WS-SAVE-MLNAMEL
082013     END-IF
082013     IF MFNAMEL > 0
082013         MOVE MFNAMEI  TO WS-SAVE-MFNAMEI
082013         MOVE MFNAMEL  TO WS-SAVE-MFNAMEL
082013     END-IF
082013     IF MMINITL > 0
082013         MOVE MMINITI  TO WS-SAVE-MMINITI
082013         MOVE MMINITL  TO WS-SAVE-MMINITL
082013     END-IF
062217*    IF LOANNOL > 0
062217*        MOVE LOANNOI  TO WS-SAVE-LOANNOI
062217*        MOVE LOANNOL  TO WS-SAVE-LOANNOL
062217*    END-IF
082013     IF LOANBALL > 0
082013         MOVE LOANBALI  TO WS-SAVE-LOANBALI
082013         MOVE LOANBALL  TO WS-SAVE-LOANBALL
082013     END-IF
082013     IF PROCL > 0
082013         MOVE PROCI  TO WS-SAVE-PROCI
082013         MOVE PROCL  TO WS-SAVE-PROCL
082013     END-IF
082013     IF SUPVL > 0
082013         MOVE SUPVI  TO WS-SAVE-SUPVI
082013         MOVE SUPVL  TO WS-SAVE-SUPVL
082013     END-IF
082013     IF PRICDL > 0
082013         MOVE PRICDI  TO WS-SAVE-PRICDI
082013         MOVE PRICDL  TO WS-SAVE-PRICDL
082013     END-IF
082013     IF FILETOL > 0
082013         MOVE FILETOI  TO WS-SAVE-FILETOI
082013         MOVE FILETOL  TO WS-SAVE-FILETOL
082013     END-IF
082013     IF PDTHRUL > 0
082013         MOVE PDTHRUI  TO WS-SAVE-PDTHRUI
082013         MOVE PDTHRUL  TO WS-SAVE-PDTHRUL
082013     END-IF
082013     IF PDAMTL > 0
082013         MOVE PDAMTI  TO WS-SAVE-PDAMTI
082013         MOVE PDAMTL  TO WS-SAVE-PDAMTL
082013     END-IF
082013     IF NODAYSL > 0
082013         MOVE NODAYSI  TO WS-SAVE-NODAYSI
082013         MOVE NODAYSL  TO WS-SAVE-NODAYSL
082013     END-IF
082013     IF NOPMTSL > 0
082013         MOVE NOPMTSI  TO WS-SAVE-NOPMTSI
082013         MOVE NOPMTSL  TO WS-SAVE-NOPMTSL
082013     END-IF
082013     IF FORMTYPL > 0
082013         MOVE FORMTYPI  TO WS-SAVE-FORMTYPI
082013         MOVE FORMTYPL  TO WS-SAVE-FORMTYPL
082013     END-IF
082013     IF OCCL > 0
082013         MOVE OCCI  TO WS-SAVE-OCCI
082013         MOVE OCCL  TO WS-SAVE-OCCL
082013     END-IF.
082013 4100-EXIT.
082013      EXIT.
082013
082013 4200-LOAD-CHANGES.
082013     IF WS-SAVE-MAINTL > 0
082013         MOVE WS-SAVE-MAINTI  TO MAINTI
082013         MOVE WS-SAVE-MAINTL  TO MAINTL
082013         MOVE AL-UANON        TO MAINTA
082013     END-IF
082013     IF WS-SAVE-STATUSL > 0
082013         MOVE WS-SAVE-STATUSI  TO STATUSI
082013         MOVE WS-SAVE-STATUSL  TO STATUSL
082013         MOVE AL-UANON         TO STATUSA
082013     END-IF
082013     IF WS-SAVE-REPL > 0
082013         MOVE WS-SAVE-REPI  TO REPI
082013         MOVE WS-SAVE-REPL  TO REPL
082013         MOVE AL-UANON      TO REPA
082013     END-IF
040814*    IF WS-SAVE-CAUSEL > 0
040814*        MOVE WS-SAVE-CAUSEI  TO CAUSEI
040814*        MOVE WS-SAVE-CAUSEL  TO CAUSEL
040814*        MOVE AL-UANON        TO CAUSEA
040814*    END-IF
040814*    IF WS-SAVE-ENDL > 0
040814*        MOVE WS-SAVE-ENDI  TO ENDI
040814*        MOVE WS-SAVE-ENDL  TO ENDL
040814*        MOVE AL-UANON      TO ENDA
040814*    END-IF
082013     IF WS-SAVE-DIAGL > 0
082013         MOVE WS-SAVE-DIAGI  TO DIAGI
082013         MOVE WS-SAVE-DIAGL  TO DIAGL
082013         MOVE AL-UANON       TO DIAGA
082013     END-IF
040814     IF WS-SAVE-ICD1L > 0
040814         MOVE WS-SAVE-ICD1I  TO ICD1I
040814         MOVE WS-SAVE-ICD1L  TO ICD1L
040814         MOVE AL-UANON       TO ICD1A
040814     END-IF
040814     IF WS-SAVE-ICD2L > 0
040814         MOVE WS-SAVE-ICD2I  TO ICD2I
040814         MOVE WS-SAVE-ICD2L  TO ICD2L
040814         MOVE AL-UANON       TO ICD2A
040814     END-IF
082013     IF WS-SAVE-BENEL > 0
082013         MOVE WS-SAVE-BENEI  TO BENEI
082013         MOVE WS-SAVE-BENEL  TO BENEL
082013         MOVE AL-UANON       TO BENEA
082013     END-IF
082013     IF WS-SAVE-BIRTHL > 0
082013         MOVE WS-SAVE-BIRTHI  TO BIRTHI
082013         MOVE WS-SAVE-BIRTHL  TO BIRTHL
082013         MOVE AL-UANON        TO BIRTHA
082013     END-IF
082013     IF WS-SAVE-SOCIALL > 0
082013         MOVE WS-SAVE-SOCIALI  TO SOCIALI
082013         MOVE WS-SAVE-SOCIALL  TO SOCIALL
082013         MOVE AL-UANON         TO SOCIALA
082013     END-IF
082013     IF WS-SAVE-SEXL > 0
082013         MOVE WS-SAVE-SEXI  TO SEXI
082013         MOVE WS-SAVE-SEXL  TO SEXL
082013         MOVE AL-UANON      TO SEXA
082013     END-IF
082013     IF WS-SAVE-MLNAMEL > 0
082013         MOVE WS-SAVE-MLNAMEI  TO MLNAMEI
082013         MOVE WS-SAVE-MLNAMEL  TO MLNAMEL
082013         MOVE AL-UANON         TO MLNAMEA
082013     END-IF
082013     IF WS-SAVE-MFNAMEL > 0
082013         MOVE WS-SAVE-MFNAMEI  TO MFNAMEI
082013         MOVE WS-SAVE-MFNAMEL  TO MFNAMEL
082013         MOVE AL-UANON         TO MFNAMEA
082013     END-IF
082013     IF WS-SAVE-MMINITL > 0
082013         MOVE WS-SAVE-MMINITI  TO MMINITI
082013         MOVE WS-SAVE-MMINITL  TO MMINITL
082013         MOVE AL-UANON         TO MMINITA
082013     END-IF
062217*    IF WS-SAVE-LOANNOL > 0
062217*        MOVE WS-SAVE-LOANNOI  TO LOANNOI
062217*        MOVE WS-SAVE-LOANNOL  TO LOANNOL
062217*        MOVE AL-UANON         TO LOANNOA
062217*    END-IF
082013     IF WS-SAVE-LOANBALL > 0
082013         MOVE WS-SAVE-LOANBALI  TO LOANBALI
082013         MOVE WS-SAVE-LOANBALL  TO LOANBALL
082013         MOVE AL-UANON          TO LOANBALA
082013     END-IF
082013     IF WS-SAVE-PROCL > 0
082013         MOVE WS-SAVE-PROCI  TO PROCI
082013         MOVE WS-SAVE-PROCL  TO PROCL
082013         MOVE AL-UANON       TO PROCA
082013     END-IF
082013     IF WS-SAVE-SUPVL > 0
082013         MOVE WS-SAVE-SUPVI  TO SUPVI
082013         MOVE WS-SAVE-SUPVL  TO SUPVL
082013         MOVE AL-UANON       TO SUPVA
082013     END-IF
082013     IF WS-SAVE-PRICDL > 0
082013         MOVE WS-SAVE-PRICDI  TO PRICDI
082013         MOVE WS-SAVE-PRICDL  TO PRICDL
082013         MOVE AL-UANON        TO PRICDA
082013     END-IF
082013     IF WS-SAVE-FILETOL > 0
082013         MOVE WS-SAVE-FILETOI  TO FILETOI
082013         MOVE WS-SAVE-FILETOL  TO FILETOL
082013         MOVE AL-UANON         TO FILETOA
082013     END-IF
082013     IF WS-SAVE-PDTHRUL > 0
082013         MOVE WS-SAVE-PDTHRUI  TO PDTHRUI
082013         MOVE WS-SAVE-PDTHRUL  TO PDTHRUL
082013         MOVE AL-UANON         TO PDTHRUA
082013     END-IF
082013     IF WS-SAVE-PDAMTL > 0
082013         MOVE WS-SAVE-PDAMTI  TO PDAMTI
082013         MOVE WS-SAVE-PDAMTL  TO PDAMTL
082013         MOVE AL-UANON        TO PDAMTA
082013     END-IF
082013     IF WS-SAVE-NODAYSL > 0
082013         MOVE WS-SAVE-NODAYSI  TO NODAYSI
082013         MOVE WS-SAVE-NODAYSL  TO NODAYSL
082013         MOVE AL-UANON         TO NODAYSA
082013     END-IF
082013     IF WS-SAVE-NOPMTSL > 0
082013         MOVE WS-SAVE-NOPMTSI  TO NOPMTSI
082013         MOVE WS-SAVE-NOPMTSL  TO NOPMTSL
082013         MOVE AL-UANON         TO NOPMTSA
082013     END-IF
082013     IF WS-SAVE-FORMTYPL > 0
082013         MOVE WS-SAVE-FORMTYPI  TO FORMTYPI
082013         MOVE WS-SAVE-FORMTYPL  TO FORMTYPL
082013         MOVE AL-UANON          TO FORMTYPA
082013     END-IF
082013     IF WS-SAVE-OCCL > 0
082013         MOVE WS-SAVE-OCCI  TO OCCI
082013         MOVE WS-SAVE-OCCL  TO OCCL
082013         MOVE AL-UANON      TO OCCA
082013     END-IF.
082013 4200-EXIT.
082013      EXIT.
082013
04170  5000-BUILD-MAP.                                                  EL131
04171      IF PI-RETURN-CD-1 = SPACE                                    EL131
04172          PERFORM 5010-RESET-PI-AREA THRU 5010-EXIT.                  CL*34
04173                                                                   EL131
04174      MOVE SPACE                  TO PI-RETURN-CD-1.               EL131
04175                                                                   EL131
04176      MOVE LOW-VALUES             TO EL131AI.                      EL131
04177                                                                   EL131
04178      MOVE PI-COMPANY-CD          TO COMPANY-CODE.                 EL131
04179      MOVE PI-CARRIER             TO CARRIER-CODE.                    CL*22
04180      MOVE PI-CLAIM-NO            TO CLAIM-NO.                     EL131
04181      MOVE PI-CERT-NO             TO CERT-NO.                      EL131
04182                                                                      CL**8
04183  5000-READ-CLAIM-MASTER.                                             CL**8
04184                                                                   EL131
04185      EXEC CICS READ                                               EL131
04186           SET      (ADDRESS OF CLAIM-MASTER)                         CL*32
04187           DATASET  (CLMS-FILE-ID)                                 EL131
04188           RIDFLD   (MSTR-KEY)                                     EL131
04189      END-EXEC.                                                    EL131
04190                                                                   EL131
04191      PERFORM 5020-MOVE-MSTR THRU 5020-EXIT.                       EL131
04192                                                                      CL**8
04193  5000-READ-DIAGNOSIS-TRAILER.                                        CL**8
04194                                                                      CL**8
04195      EXEC CICS HANDLE CONDITION                                      CL**8
04196           NOTFND (5000-TRLR-NOT-FOUND)                               CL**8
04197      END-EXEC.                                                       CL**8
04198                                                                      CL**8
04199      MOVE MSTR-KEY               TO TRLR-KEY.                        CL**8
04200      MOVE NINETY                 TO TRLR-SEQ-NO.                     CL**8
04201                                                                      CL**8
04202      EXEC CICS READ                                                  CL**8
04203           SET      (ADDRESS OF ACTIVITY-TRAILERS)                    CL*32
04204           DATASET  (TRLR-FILE-ID)                                    CL**8
04205           RIDFLD   (TRLR-KEY)                                        CL**8
04206      END-EXEC.                                                       CL**8
04207                                                                      CL**8
04208      MOVE AT-INFO-LINE-1         TO DIAGO.                           CL**8
040814     MOVE AT-ICD-CODE-1          TO ICD1O.
040814     MOVE AT-ICD-CODE-2          TO ICD2O.
04209                                                                      CL**8
04210  5000-READ-CERTIFICATE-MASTER.                                       CL**8
04211                                                                   EL131
04212      MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.            EL131
04213      MOVE CL-CERT-CARRIER        TO CERT-CARRIER                  EL131
04214      MOVE CL-CERT-GROUPING       TO CERT-GROUP PI-GROUPING.       EL131
04215      MOVE CL-CERT-STATE          TO CERT-STATE PI-STATE.          EL131
04216      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT PI-ACCOUNT.      EL131
04217      MOVE CL-CERT-NO             TO CERT-CERT PI-CERT-NO.         EL131
04218      MOVE CL-CERT-EFF-DT         TO CERT-DATE PI-CERT-EFF-DT.     EL131
04219                                                                   EL131
04220      EXEC CICS HANDLE CONDITION                                   EL131
04221           NOTFND (5000-CERT-NOT-FOUND)                            EL131
04222      END-EXEC.                                                    EL131
04223                                                                   EL131
04224      EXEC CICS READ                                               EL131
04225           SET      (ADDRESS OF CERTIFICATE-MASTER)                   CL*32
04226           DATASET  (CERT-FILE-ID)                                 EL131
04227           RIDFLD   (CERT-KEY)                                     EL131
04228      END-EXEC.                                                    EL131
04229                                                                   EL131
04230      PERFORM 5030-MOVE-CERT THRU 5030-EXIT.                       EL131

062217     PERFORM 5100-LOAD-CORR-TRLR THRU 5100-EXIT.

04231      PERFORM 7600-BROWSE-CLAIM THRU 7699-EXIT.                       CL**8
04232                                                                   EL131
04233      GO TO 5000-EXIT.                                                CL*34
04234                                                                      CL**8
04235  5000-TRLR-NOT-FOUND.                                                CL**8
04236      MOVE ER-7687                TO EMI-ERROR.                       CL**8
04237      MOVE -1                     TO DIAGL.                           CL**8
04238      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**8
04239      GO TO 5000-EXIT.                                                CL*34
04240                                                                   EL131
04241  5000-CERT-NOT-FOUND.                                             EL131
04242      MOVE ER-0206                TO EMI-ERROR.                    EL131
04243      MOVE -1                     TO CERTL.                        EL131
04244      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL131
04245                                                                   EL131
04246  5000-EXIT.                                                          CL*34
04247      EXIT.                                                        EL131
04248      EJECT                                                        EL131
04249  5010-RESET-PI-AREA.                                              EL131
04250      IF CERTI GREATER THAN LOW-VALUES                             EL131
04251          MOVE CERTI              TO PI-CERT-PRIME.                EL131
04252                                                                   EL131
04253      IF SUFXI GREATER THAN LOW-VALUES                             EL131
04254          MOVE SUFXI              TO PI-CERT-SFX.                  EL131
04255                                                                   EL131
04256      IF CERTEFFI GREATER THAN LOW-VALUES                          EL131
04257          MOVE HOLD-EFF           TO PI-CERT-EFF-DT.               EL131
04258                                                                   EL131
04259      IF CERTACTI GREATER THAN LOW-VALUES                          EL131
04260          MOVE CERTACTI           TO PI-ACCOUNT.                   EL131
04261                                                                   EL131
04262      IF CERTSTI GREATER THAN LOW-VALUES                           EL131
04263          MOVE CERTSTI            TO PI-STATE.                     EL131
04264                                                                   EL131
04265      IF CERTCARI GREATER THAN LOW-VALUES                          EL131
04266          MOVE CERTCARI           TO PI-CARRIER.                   EL131
04267                                                                   EL131
04268      IF CERTGRPI GREATER THAN LOW-VALUES                          EL131
04269          MOVE CERTGRPI           TO PI-GROUPING.                  EL131
04270                                                                   EL131
04271  5010-EXIT.                                                       EL131
04272      EXIT.                                                        EL131
04273                                                                      CL**8
04274      EJECT                                                        EL131
04275  5020-MOVE-MSTR.                                                  EL131
04276                                                                      CL**8
04277      MOVE CL-ASSOC-CERT-SEQU     TO WS-CURRENT-SEQU.                 CL**8
04278      MOVE CL-ASSOC-CERT-TOTAL    TO WS-OF-SEQU.                      CL**8
04279      MOVE WS-CLAIM-SEQU          TO SEQUO.                           CL**8
04280      MOVE AL-SABON               TO SEQUA.                           CL**8
04281      MOVE CL-CLAIM-NO            TO CLAIMO PI-CLAIM-NO.           EL131
04282      MOVE CL-CLAIM-TYPE          TO TYPEO
052113                                    pi-claim-type
04283                                                                      CL*21
04284      IF CL-TOTAL-PAID-AMT  = +0 AND                                  CL*34
04285         CL-LAST-PMT-AMT    = +0 AND                                  CL*34
04286         CL-NO-OF-PMTS-MADE = +0                                      CL*34
04287           MOVE AL-UANON          TO TYPEA.                           CL*34
04288                                                                      CL*21
04289      MOVE CL-PRIME-CERT-PRIME    TO PCERTNOO.                        CL**8
04290      MOVE CL-PRIME-CERT-SFX      TO PSUFXO.                          CL**8
04291      MOVE CL-CERT-PRIME          TO CERTO.                        EL131
04292      MOVE CL-CERT-SFX            TO SUFXO.                        EL131
04293      MOVE CL-CERT-CARRIER        TO CARRO.                        EL131
04294                                                                   EL131
04295      IF CL-CLAIM-STATUS = 'C'                                     EL131
04296          MOVE 'CLOSED'           TO STATUSO                          CL**8
04297      ELSE                                                         EL131
04298          MOVE 'OPEN  '           TO STATUSO.                         CL**8
04299                                                                   EL131
052113     move cl-accident-claim-sw   to accswo
           if cl-benefit-period not numeric
              move zeros               to cl-benefit-period
           end-if

           move cl-benefit-period      to benpero
           move cl-insured-type        to instypeo

04300      MOVE CL-PROCESSOR-ID        TO PROCO.                        EL131
04301                                                                      CL*34
04302      MOVE CL-FILE-ESTABLISHED-BY TO AUIDO.                           CL*34
04303      MOVE CL-CCN                 TO CCNOO.                           CL*34
04304                                                                      CL*34
04305      MOVE CL-INSURED-LAST-NAME   TO MLNAMEO.                      EL131
04306      MOVE CL-INSURED-1ST-NAME    TO MFNAMEO.                      EL131
04307      MOVE CL-INSURED-MID-INIT    TO MMINITO.                      EL131
04308      MOVE CL-INSURED-SEX-CD      TO SEXO.                         EL131
04309                                                                   EL131
04310      IF CL-INSURED-BIRTH-DT GREATER THAN LOW-VALUES               EL131
04311          MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1                EL131
04312          MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE  EL131
04313          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
04314          MOVE DC-GREG-DATE-1-EDIT TO BIRTHO                       EL131
04315      ELSE                                                         EL131
04316          MOVE SPACES             TO BIRTHO.                       EL131
04317                                                                   EL131
04318      IF CL-SSN-STATE   = CL-CERT-STATE  AND                       EL131
04319         CL-SSN-ACCOUNT = CL-CERT-ACCOUNT-PRIME                    EL131
04320          NEXT SENTENCE                                            EL131
04321        ELSE                                                       EL131
04322          MOVE CL-SOC-SEC-NO      TO SOCIALO.                      EL131
04323                                                                      CL*37
04324      IF PI-COMPANY-ID = 'DMD'                                        CL*37
04325        IF CL-CERT-PRIME (4:1) = 'U' OR 'F'                           CL*39
04326           IF SOCIALO = SPACES OR LOW-VALUES                          CL*37
04327             MOVE ER-0885         TO EMI-ERROR                        CL*37
04328             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                CL*37
04329                                                                      CL*37
04330      MOVE CL-INSURED-OCC-CD      TO OCCO.                         EL131
040814*    MOVE CL-CAUSE-CD            TO CAUSEO.                       EL131
04332                                                                   EL131
040814*    IF CL-EST-END-OF-DISAB-DT GREATER THAN LOW-VALUES            EL131
040814*        MOVE CL-EST-END-OF-DISAB-DT TO DC-BIN-DATE-1             EL131
040814*        MOVE SPACES                 TO DC-OPTION-CODE            EL131
040814*        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
040814*        MOVE DC-GREG-DATE-1-EDIT TO ENDO                         EL131
040814*    ELSE                                                         EL131
040814*        MOVE SPACES             TO ENDO.                         EL131
04340                                                                   EL131
04341      IF CL-PAID-THRU-DT GREATER THAN LOW-VALUES                   EL131
04342         MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1                     CL**8
04343         MOVE SPACES             TO DC-OPTION-CODE                    CL**8
04344         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                     CL**8
04345         MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO                          CL**8
04346         IF PI-USES-PAID-TO                                           CL**8
04347            MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1                  CL**8
04348            MOVE '6'                TO DC-OPTION-CODE                 CL**8
04349            MOVE +1                 TO DC-ELAPSED-DAYS                CL**8
04350            MOVE +0                 TO DC-ELAPSED-MONTHS              CL**8
04351            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                  CL**8
04352            MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO                       CL**8
04353         ELSE                                                         CL**8
04354            NEXT SENTENCE                                             CL**8
04355      ELSE                                                         EL131
04356         MOVE SPACES             TO PDTHRUO.                          CL**8
04357                                                                   EL131
04358      MOVE CL-TOTAL-PAID-AMT      TO PDAMTO.                       EL131
04359      MOVE CL-NO-OF-DAYS-PAID     TO NODAYSO.                      EL131
04360      MOVE CL-NO-OF-PMTS-MADE     TO NOPMTSO.                      EL131
04361      MOVE CL-PROG-FORM-TYPE      TO FORMTYPO.                        CL*14
04362                                                                   EL131
04363      IF MODIFY-CAP                                                EL131
04364         MOVE AL-UNNOF            TO PDAMTA                        EL131
04365                                     NODAYSA                       EL131
04366                                     NOPMTSA                       EL131
04367         MOVE AL-UANOF            TO PDTHRUA.                      EL131

080613     MOVE PI-COMPANY-ID          TO CNTL-CO-ID
080613     MOVE '2'                    TO CNTL-REC-TYPE
080613     MOVE +0                     TO CNTL-SEQ-NO
080613     MOVE PI-PROCESSOR-ID        TO CNTL-PROC-ID
080613
080613     EXEC CICS READ
080613          DATASET  (CNTL-FILE-ID)
080613          SET      (ADDRESS OF CONTROL-FILE)
080613          RIDFLD   (CNTL-KEY)
080613          RESP     (WS-RESPONSE)
080613     END-EXEC
080613
080613     IF WS-RESP-NORMAL
080613        MOVE CF-APPROVAL-LEVEL   TO PI-APPROVAL-LEVEL
080613     end-if

04369      IF CL-INCURRED-DT GREATER THAN LOW-VALUES                    EL131
04370          MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1                 EL131
04371          MOVE SPACES             TO DC-OPTION-CODE                EL131
04372          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
04373          MOVE DC-GREG-DATE-1-EDIT TO INCO                         EL131
04374      ELSE                                                         EL131
04375          MOVE SPACES             TO INCO.                         EL131
04376                                                                   EL131
04377      IF (CL-NO-OF-PMTS-MADE = ZEROS)
111113        OR (PI-APPROVAL-LEVEL = '4' OR '5')
04378         MOVE AL-UANOF            TO INCA
080613     end-if

04380      MOVE CL-NO-OF-PMTS-MADE     TO PI-NO-PMTS.                   EL131
04381                                                                   EL131
04382      IF CL-REPORTED-DT GREATER THAN LOW-VALUES                    EL131
04383          MOVE CL-REPORTED-DT     TO DC-BIN-DATE-1                 EL131
04384          MOVE SPACES             TO DC-OPTION-CODE                EL131
04385          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
04386          MOVE DC-GREG-DATE-1-EDIT TO REPO                         EL131
04387      ELSE                                                         EL131
04388          MOVE SPACES             TO REPO.                         EL131
04389                                                                   EL131
04390      IF CL-FILE-ESTABLISH-DT GREATER THAN LOW-VALUES              EL131
04391          MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1               EL131
04392          MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE  EL131
04393          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
04394          MOVE DC-GREG-DATE-1-EDIT TO ESTO                         EL131
04395      ELSE                                                         EL131
04396          MOVE SPACES             TO ESTO.                         EL131
04397                                                                   EL131
04398      IF CL-LAST-MAINT-DT GREATER THAN LOW-VALUES                  EL131
04399          MOVE CL-LAST-MAINT-DT   TO DC-BIN-DATE-1                 EL131
04400          MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE  EL131
04401          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
04402          MOVE DC-GREG-DATE-1-EDIT TO MNTDTO                       EL131
04403      ELSE                                                         EL131
04404          MOVE SPACES             TO MNTDTO.                       EL131
04405                                                                   EL131
04406                                                                      CL*34
04407      IF CL-LAST-MAINT-TYPE = SPACE                                EL131
04408          MOVE 'SET-UP'           TO MNTTYPEO                         CL*34
04409      ELSE                                                         EL131
04410      IF CL-LAST-MAINT-TYPE = '1'                                     CL*34
04411          MOVE 'PAYMNT'           TO MNTTYPEO                         CL*34
04412      ELSE                                                            CL*34
04413      IF CL-LAST-MAINT-TYPE = '2'                                     CL*34
04414          MOVE 'LETTER'           TO MNTTYPEO                         CL*34
04415      ELSE                                                            CL*34
04416      IF CL-LAST-MAINT-TYPE = '3'                                     CL*34
04417          MOVE 'UPDATE'           TO MNTTYPEO                         CL*34
04418      ELSE                                                            CL*34
04419      IF CL-LAST-MAINT-TYPE = '4'                                     CL*34
04420          MOVE 'RESTOR'           TO MNTTYPEO                         CL*34
04421      ELSE                                                            CL*34
04422      IF CL-LAST-MAINT-TYPE = '5'                                     CL*34
04423          MOVE 'INC DT'           TO MNTTYPEO                         CL*34
04424      ELSE                                                            CL*34
04425      IF CL-LAST-MAINT-TYPE = '6'                                     CL*34
04426          MOVE ' CONV'            TO MNTTYPEO                         CL*34
04427      ELSE                                                            CL*34
04428      IF CL-LAST-MAINT-TYPE = 'C'                                     CL*34
04429          MOVE 'CHGBEN'           TO MNTTYPEO                         CL*34
04430      ELSE                                                            CL*34
04431      IF CL-LAST-MAINT-TYPE = 'E'                                     CL*34
04432          MOVE 'ERRCOR'           TO MNTTYPEO                         CL*34
04433      ELSE                                                            CL*34
04434          MOVE SPACES             TO MNTTYPEO.                        CL*34
04435                                                                   EL131
04436      MOVE CL-BENEFICIARY         TO BENEO.                           CL**8
04437      MOVE CL-FILE-LOCATION       TO FILETOO.                         CL**8
052113     IF CL-CRITICAL-PERIOD NOT NUMERIC
052113        MOVE ZEROS               TO CL-CRITICAL-PERIOD
052113     END-IF
052113     MOVE CL-CRITICAL-PERIOD     TO CRITPO
081817     IF CL-NO-OF-EXTENSIONS NOT NUMERIC
081817        MOVE ZEROS               TO CL-NO-OF-EXTENSIONS
081817     END-IF
081817     MOVE CL-NO-OF-EXTENSIONS    TO EXTENSO
052113*    MOVE CL-CRIT-PER-RECURRENT
052113*                                TO CRITPTO
052113*    IF CL-CRIT-PER-RTW-MOS NOT NUMERIC
052113*       MOVE ZEROS               TO CL-CRIT-PER-RTW-MOS
052113*    END-IF
052113*    MOVE CL-CRIT-PER-RTW-MOS    TO RTWMOSO
04438      MOVE CL-PRIORITY-CD         TO PRICDO.                       EL131
04439      MOVE CL-SUPV-ATTN-CD        TO SUPVO.                        EL131
04440      MOVE CL-LAST-MAINT-USER     TO PI-UPDATE-BY.                 EL131
04441      MOVE CL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.             EL131
060413     MOVE CL-AUTO-PAY-SEQ        TO PI-AUTO-PAY-SEQ.
04442                                                                   EL131
04443  5020-EXIT.                                                       EL131
04444      EXIT.                                                        EL131
04445      EJECT                                                        EL131
04446  5030-MOVE-CERT.                                                  EL131
04447                                                                      CL**8
04448      IF CM-CERT-EFF-DT GREATER THAN LOW-VALUES                    EL131
04449          MOVE CM-CERT-EFF-DT     TO DC-BIN-DATE-1                 EL131
04450          MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE  EL131
04451          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
04452          MOVE DC-GREG-DATE-1-EDIT    TO CERTEFFO HOLD-DATE        EL131
04453        ELSE                                                       EL131
04454          MOVE SPACES             TO CERTEFFO                      EL131
04455          MOVE ZERO               TO HOLD-DATE.                    EL131
04456                                                                   EL131
04457      MOVE CM-ACCOUNT             TO CERTACTO.                     EL131
04458      MOVE CM-STATE               TO CERTSTO PI-STATE.             EL131
04459      MOVE CM-GROUPING            TO CERTGRPO.                     EL131
04460      MOVE CM-CARRIER             TO CERTCARO.                     EL131
04461      MOVE CM-INSURED-LAST-NAME   TO CRTLNMEO.                     EL131
04462      MOVE CM-INSURED-INITIAL2    TO CRTINITO.                     EL131
04463      MOVE CM-INSURED-FIRST-NAME  TO CRTFNMEO.                     EL131
04464      MOVE CM-INSURED-ISSUE-AGE   TO ISSAGEO.                      EL131
04465      MOVE CM-JT-LAST-NAME        TO JNTLNMEO.                     EL131
04466      MOVE CM-JT-FIRST-NAME       TO JNTFNMEO.                     EL131
04467      MOVE CM-JT-INITIAL          TO JNTINITO.                     EL131
04468      MOVE CM-INSURED-JOINT-AGE   TO JNTAGEO.                      EL131
04469                                                                      CL*45
04470 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***                CL*45
04471      MOVE PI-COMPANY-ID          TO CNTL-CO-ID.                      CL*45
04472      MOVE '3'                    TO CNTL-REC-TYPE.                   CL*45
04473      MOVE SPACES                 TO CNTL-STATE-ACCESS.               CL*45
04474      MOVE CM-STATE               TO CNTL-STATE-NUMBER.               CL*45
04475      MOVE ZERO                   TO CNTL-SEQ-NO.                     CL*45
04476                                                                      CL*45
04477      EXEC CICS READ                                                  CL*45
04478           SET        (ADDRESS OF CONTROL-FILE)                       CL*45
04479           DATASET    ('ELCNTL')                                      CL*45
04480           RIDFLD     (CNTL-KEY)                                      CL*45
04481           RESP       (WS-RESPONSE)                                   CL*45
04482      END-EXEC.                                                       CL*45
04483                                                                      CL*45
04484      IF WS-RESP-NOTFND                                               CL*45
04485         MOVE ZERO                TO CP-FREE-LOOK                     CL*45
04486      ELSE                                                            CL*45
04487         MOVE CF-ST-FREE-LOOK-PERIOD                                  CL*45
04488                                  TO CP-FREE-LOOK.                    CL*45
04489                                                                   EL131
04490      IF CM-LF-BENEFIT-CD = '00'                                      CL*34
04491         GO TO 5030-MOVE-CERT-AH.                                     CL*12
04492                                                                      CL*12
04493      EJECT                                                        EL131
04494      MOVE EIBDATE                TO DC-JULIAN-YYDDD               EL131
04495      MOVE '5'                    TO DC-OPTION-CODE                EL131
04496      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                     EL131
04497      MOVE DC-BIN-DATE-1          TO WS-BIN-CURRENT-DT.            EL131
04498      MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.                        CL**8
04499      MOVE CM-LF-BENEFIT-CD       TO LCVCDO HOLD-BENEFIT.             CL**8
04500      MOVE CM-LF-ORIG-TERM        TO LCVOTRMO  CP-ORIGINAL-TERM.      CL**8
04501      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT                EL131
04502      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.               CL**8
04503      MOVE CL-INCURRED-DT         TO CP-VALUATION-DT                  CL*15
04504      MOVE '4'                    TO CP-REM-TERM-METHOD            EL131
04505      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID                 EL131
04506      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.          CL*22
04507      PERFORM 9700-LINK-RTRM-FILE-ID THRU 9700-EXIT.               EL131
04508                                                                   EL131
04509      MOVE CP-REMAINING-TERM-3    TO LCVRTRMO.                        CL**8
04510      IF CM-LF-PREMIUM-RATE NUMERIC                                   CL*14
04511          MOVE CM-LF-PREMIUM-RATE TO LCVRATEO                         CL*14
04512      ELSE                                                            CL*14
04513          MOVE ZEROS              TO LCVRATEO.                        CL*14
04514                                                                      CL*31
04515      IF CM-LF-ALT-BENEFIT-AMT NOT NUMERIC                            CL*31
04516          MOVE ZEROS              TO CM-LF-ALT-BENEFIT-AMT.           CL*31
04517                                                                      CL*34
04518      COMPUTE LCVBENEO = CM-LF-BENEFIT-AMT + CM-LF-ALT-BENEFIT-AMT.   CL*31
04519                                                                      CL*31
04520      MOVE CM-POLICY-FORM-NO      TO LCVFORMO.                        CL**8
04521                                                                   EL131
04522      IF CM-LF-CURRENT-STATUS = '8'                                EL131
04523         IF CM-LF-CANCEL-DT NOT = LOW-VALUES                       EL131
04524             MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1                 EL131
04525             MOVE ' '             TO DC-OPTION-CODE                EL131
04526             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT              EL131
04527             IF NOT DATE-CONVERSION-ERROR                          EL131
04528                 MOVE DC-GREG-DATE-1-EDIT     TO LCVCNDTO.            CL**8
04529                                                                   EL131
04530      IF CM-LF-CURRENT-STATUS = '7'                                EL131
04531         IF CM-LF-DEATH-DT NOT = LOW-VALUES                        EL131
04532             MOVE CM-LF-DEATH-DT  TO DC-BIN-DATE-1                 EL131
04533             MOVE ' '             TO DC-OPTION-CODE                EL131
04534             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT              EL131
04535             IF NOT DATE-CONVERSION-ERROR                          EL131
04536                 MOVE DC-GREG-DATE-1-EDIT     TO LCVCNDTO.            CL**8
04537                                                                   EL131
04538      IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES                      EL131
04539          MOVE ' '                TO DC-OPTION-CODE                EL131
04540          MOVE CM-LF-DEATH-EXIT-DT TO DC-BIN-DATE-1                EL131
04541          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
04542          IF NOT DATE-CONVERSION-ERROR                             EL131
04543              MOVE DC-GREG-DATE-1-EDIT TO HOLD-DATE                EL131
04544              MOVE HOLD-MONTH     TO WORK-MONTH                    EL131
04545              MOVE HOLD-YEAR      TO WORK-YEAR                     EL131
04546              MOVE WORK-DATE-MY   TO LCVEXITO.                        CL**8
04547                                                                   EL131
04548      IF CM-LF-CURRENT-STATUS = '1' OR = '4'                       EL131
04549         IF CP-REMAINING-TERM-3 = ZEROS                            EL131
04550            MOVE 'EXPIRED'        TO LCVSTATO                         CL**8
04551         ELSE                                                      EL131
04552            MOVE 'ACTIVE'         TO LCVSTATO.                        CL**8
04553                                                                   EL131
04554      IF CM-LF-CURRENT-STATUS = '2'                                EL131
04555         MOVE 'PEND   '           TO LCVSTATO.                        CL**8
04556      IF CM-LF-CURRENT-STATUS = '3'                                EL131
04557         MOVE 'RESTORE'           TO LCVSTATO.                        CL**8
04558      IF CM-LF-CURRENT-STATUS = '5'                                EL131
04559         MOVE 'REISSUE'           TO LCVSTATO.                        CL**8
04560      IF CM-LF-CURRENT-STATUS = '6'                                EL131
100518        MOVE 'LMPBEN'            TO LCVSTATO.                        CL**8
04562      IF CM-LF-CURRENT-STATUS = '7'                                EL131
04563         MOVE 'DEATH  '           TO LCVSTATO.                        CL**8
04564      IF CM-LF-CURRENT-STATUS = '8'                                EL131
04565         MOVE 'CANCEL '           TO LCVSTATO.                        CL**8
04566      IF CM-LF-CURRENT-STATUS = '9'                                EL131
04567         MOVE 'RE-ONLY'           TO LCVSTATO.                        CL**8
04568      IF CM-LF-CURRENT-STATUS = 'V'                                   CL*24
04569         MOVE 'VOID   '           TO LCVSTATO.                        CL*24
04570      IF CM-LF-CURRENT-STATUS = 'D'                                   CL*24
04571         MOVE 'DECLINE'           TO LCVSTATO.                        CL*24
04572                                                                   EL131
04573      MOVE SPACES                 TO BENEFIT-KEY ERROR-SWITCH.     EL131
04574      MOVE PI-COMPANY-ID          TO BEN-CO-ID.                    EL131
04575      MOVE '4'                    TO BEN-REC-TYPE.                 EL131
04576      MOVE CM-LF-BENEFIT-CD       TO BEN-ACC-CD.                   EL131
04577      MOVE ZERO                   TO BEN-SEQ-NO.                   EL131
04578                                                                   EL131
04579      EXEC CICS READ GTEQ                                          EL131
04580           DATASET  (CNTL-FILE-ID)                                 EL131
04581           RIDFLD   (BENEFIT-KEY)                                  EL131
04582           SET      (ADDRESS OF CONTROL-FILE)                         CL*32
04583      END-EXEC.                                                    EL131
04584                                                                   EL131
04585      IF CF-COMPANY-ID NOT = PI-COMPANY-ID OR                      EL131
04586         CF-RECORD-TYPE NOT = '4'                                  EL131
04587          MOVE ER-0240            TO EMI-ERROR                     EL131
04588          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL131
04589          GO TO 5030-MOVE-CERT-FINISH.                             EL131
04590                                                                   EL131
04591      MOVE ZERO                   TO COUNT-2.                      EL131
04592      PERFORM 5040-FIND-BENEFIT THRU 5060-EXIT.                    EL131
04593                                                                   EL131
04594      IF SCREEN-ERROR                                              EL131
04595          MOVE ER-0282            TO EMI-ERROR                     EL131
04596          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL131
04597         ELSE                                                      EL131
04598          MOVE CF-BENEFIT-ALPHA (COUNT-2) TO LCVKINDO.                CL**8
04599                                                                   EL131
04600  5030-MOVE-CERT-AH.                                               EL131
04601                                                                      CL*18
04602      IF CM-LF-BENEFIT-CD = '00'                                      CL*34
04603          IF CM-AH-BENEFIT-CD = '00'                                  CL*34
04604              GO TO 5030-MOVE-CERT-FINISH.                            CL*18
04605                                                                      CL**8
04606      IF CM-AH-BENEFIT-CD = '00'                                      CL*34
04607         MOVE SPACES             TO ACVKINDO                          CL*12
04608         COMPUTE PI-PAYMENT-AMT =                                     CL*14
04609                     CM-LF-BENEFIT-AMT / CM-LF-ORIG-TERM              CL*14
04610         GO TO 5030-MOVE-CERT-FINISH.                                 CL*12
04611                                                                      CL*12
04612      MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.                        CL**8
04613      MOVE CM-AH-BENEFIT-CD       TO ACVCDO HOLD-BENEFIT.             CL**8
04614      MOVE DC-GREG-DATE-1-EDIT    TO HOLD-DATE.                    EL131
04615      MOVE CM-AH-ORIG-TERM        TO ACVOTRMO CP-ORIGINAL-TERM.       CL**8
04616      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT                EL131
04617      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.               CL**8
04618      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.                   CL*16
04619      MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT.                 CL*16
04620                                                                      CL*34
04621 **DMD **********************************************                 CL*34
04622      IF PI-COMPANY-ID = 'DMD'                                        CL*34
04623          COMPUTE WS-TERM-IN-DAYS = CM-AH-ORIG-TERM * 30 +            CL*46
04624                                    CM-PMT-EXTENSION-DAYS             CL*46
04625          COMPUTE WS-REM-TERM-IN-DAYS = WS-TERM-IN-DAYS -             CL*46
04626                                       CL-NO-OF-DAYS-PAID             CL*46
04627          DIVIDE WS-REM-TERM-IN-DAYS BY +30                           CL*46
04628              GIVING WS-REM-MOS REMAINDER WS-REM-DAYS                 CL*46
04629          MOVE WS-REM-MOS         TO ACVRTRMO                         CL*34
04630                                     CP-REMAINING-TERM-3              CL*34
052113*        MOVE WS-REM-DAYS        TO DAYSPO                           CL*34
04632          GO TO 5030-SKIP-RTRM.                                       CL*34
04633 **DMD **********************************************                 CL*34
04634                                                                      CL*34
04635      MOVE '4'                    TO CP-REM-TERM-METHOD.              CL*16
04636      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.          CL*22
04637      PERFORM 9700-LINK-RTRM-FILE-ID THRU 9700-EXIT.               EL131
04638      MOVE CP-REMAINING-TERM-3    TO ACVRTRMO.                        CL**8
04639                                                                      CL*34
04640  5030-SKIP-RTRM.                                                     CL*34
04641      IF CM-AH-PREMIUM-RATE NUMERIC                                   CL*14
04642          MOVE CM-AH-PREMIUM-RATE TO ACVRATEO                         CL*14
04643      ELSE                                                            CL*14
04644          MOVE ZEROS              TO ACVRATEO.                        CL*14
04645                                                                      CL*34
04646      MOVE CM-AH-BENEFIT-AMT      TO ACVBENEO  PI-PAYMENT-AMT.        CL*14
04647      MOVE CM-POLICY-FORM-NO      TO ACVFORMO.                        CL**8
04648                                                                   EL131
04649      IF CM-AH-CURRENT-STATUS = '8'                                EL131
04650         IF CM-AH-CANCEL-DT NOT = LOW-VALUES                       EL131
04651             MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1                 EL131
04652             MOVE ' '             TO DC-OPTION-CODE                EL131
04653             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT              EL131
04654             IF NOT DATE-CONVERSION-ERROR                          EL131
04655                 MOVE DC-GREG-DATE-1-EDIT     TO ACVCNDTO.            CL**8
04656                                                                   EL131
04657      IF CM-AH-CURRENT-STATUS = '6' OR '7'                         EL131
04658         IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES                   EL131
04659             MOVE CM-AH-SETTLEMENT-DT         TO DC-BIN-DATE-1     EL131
04660             MOVE ' '             TO DC-OPTION-CODE                EL131
04661             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT              EL131
04662             IF NOT DATE-CONVERSION-ERROR                          EL131
04663                 MOVE DC-GREG-DATE-1-EDIT     TO ACVCNDTO.            CL**8
04664                                                                   EL131
04665      IF CM-AH-CURRENT-STATUS = '1' OR = '4'                       EL131
04666         IF CP-REMAINING-TERM-3 = ZEROS                            EL131
04667            MOVE 'EXPIRED'        TO ACVSTATO                         CL**8
04668         ELSE                                                      EL131
04669            MOVE 'ACTIVE'         TO ACVSTATO.                        CL**8
04670                                                                   EL131
04671      IF CM-AH-CURRENT-STATUS = '2'                                EL131
04672         MOVE 'PEND   '           TO ACVSTATO.                        CL**8
04673      IF CM-AH-CURRENT-STATUS = '3'                                EL131
04674         MOVE 'RESTORE'           TO ACVSTATO.                        CL**8
04675      IF CM-AH-CURRENT-STATUS = '5'                                EL131
04676         MOVE 'REISSUE'           TO ACVSTATO.                        CL**8
04677      IF CM-AH-CURRENT-STATUS = '6'                                EL131
04678         MOVE 'LMP DIS'           TO ACVSTATO.                        CL**8
04679      IF CM-AH-CURRENT-STATUS = '7'                                EL131
04680         MOVE 'DEATH  '           TO ACVSTATO.                        CL**8
04681      IF CM-AH-CURRENT-STATUS = '8'                                EL131
04682         MOVE 'CANCEL '           TO ACVSTATO.                        CL**8
04683      IF CM-AH-CURRENT-STATUS = '9'                                EL131
04684         MOVE 'RE-ONLY'           TO ACVSTATO.                        CL**8
04685      IF CM-AH-CURRENT-STATUS = 'V'                                   CL*24
04686         MOVE 'VOID   '           TO ACVSTATO.                        CL*24
04687      IF CM-AH-CURRENT-STATUS = '9'                                   CL*24
04688         MOVE 'DECLINE'           TO ACVSTATO.                        CL*24
04689                                                                   EL131
04690      IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES                 EL131
04691          MOVE ' '                TO DC-OPTION-CODE                EL131
04692          MOVE CM-AH-SETTLEMENT-EXIT-DT TO DC-BIN-DATE-1           EL131
04693          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
04694          IF NOT DATE-CONVERSION-ERROR                             EL131
04695              MOVE DC-GREG-DATE-1-EDIT     TO HOLD-DATE            EL131
04696              MOVE HOLD-MONTH     TO WORK-MONTH                    EL131
04697              MOVE HOLD-YEAR      TO WORK-YEAR                     EL131
04698              MOVE WORK-DATE-MY   TO ACVEXITO.                        CL**8
04699                                                                   EL131
04700      MOVE SPACES                 TO BENEFIT-KEY ERROR-SWITCH.     EL131
04701                                                                   EL131
04702      MOVE PI-COMPANY-ID          TO BEN-CO-ID.                    EL131
04703      MOVE '5'                    TO BEN-REC-TYPE.                 EL131
04704      MOVE CM-AH-BENEFIT-CD       TO BEN-ACC-CD.                   EL131
04705      MOVE ZERO                   TO BEN-SEQ-NO.                   EL131
04706                                                                   EL131
04707      EXEC CICS READ GTEQ                                          EL131
04708           DATASET  (CNTL-FILE-ID)                                 EL131
04709           RIDFLD   (BENEFIT-KEY)                                  EL131
04710           SET      (ADDRESS OF CONTROL-FILE)                         CL*32
04711      END-EXEC.                                                    EL131
04712                                                                   EL131
04713      IF CF-COMPANY-ID NOT = PI-COMPANY-ID OR                      EL131
04714         CF-RECORD-TYPE NOT = '5'                                  EL131
04715          MOVE ER-0250            TO EMI-ERROR                     EL131
04716          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
04717          GO TO 5030-MOVE-CERT-FINISH.                             EL131
04718                                                                   EL131
04719      MOVE ZERO                   TO COUNT-2.                      EL131
04720      PERFORM 5040-FIND-BENEFIT THRU 5060-EXIT.                    EL131
04721                                                                      CL*34
04722      IF SCREEN-ERROR                                              EL131
04723          MOVE ER-0283            TO EMI-ERROR                     EL131
04724          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL131
04725        ELSE                                                       EL131
04726          MOVE CF-BENEFIT-ALPHA (COUNT-2) TO ACVKINDO.                CL**8
04727                                                                   EL131
04728  5030-MOVE-CERT-FINISH.                                           EL131
04729                                                                      CL*12
04730      MOVE CM-LOAN-APR            TO APRO.                         EL131
04731      MOVE CM-PAY-FREQUENCY       TO PMTFREQO.                     EL131
04732      MOVE CM-IND-GRP-TYPE        TO INDGRPO.                      EL131
04733      MOVE CM-PREMIUM-TYPE        TO PREMTYPO  PI-PREM-TYPE.       EL131
062217*    MOVE CM-LOAN-NUMBER         TO LOANNOO                       EL131
04735      MOVE CM-LOAN-BALANCE        TO LOANBALO.                        CL*14
04736      MOVE CM-SPECIAL-REIN-CODE   TO REINCDO.                         CL*14
04737                                                                      CL*34
04738      IF CM-LAST-ADD-ON-DT NOT = SPACES  AND  ZEROS                   CL*14
04739          MOVE CM-LAST-ADD-ON-DT  TO DC-BIN-DATE-1                    CL*14
04740          MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE     CL*14
04741          PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                    CL*34
04742          MOVE DC-GREG-DATE-1-EDIT  TO ADDONDTO                       CL*14
04743      ELSE                                                            CL*14
04744          MOVE SPACES               TO ADDONDTO.                      CL*14
04745                                                                      CL*34
04746      MOVE CM-CLAIM-INTERFACE-SW  TO PI-CERT-SWITCH.               EL131
04747                                                                   EL131
04748  5030-EXIT.                                                       EL131
04749      EXIT.                                                        EL131
04750      EJECT                                                        EL131
04751  5040-FIND-BENEFIT.                                               EL131
04752      ADD 1                       TO COUNT-2.                      EL131
04753      IF COUNT-2 GREATER THAN 8                                    EL131
04754          GO TO 5050-BENEFIT-NOTFND.                               EL131
04755                                                                      CL*34
04756      IF CF-BENEFIT-CODE (COUNT-2) = HOLD-BENEFIT                     CL**8
04757          GO TO 5060-EXIT.                                         EL131
04758                                                                      CL*34
04759      IF CF-BENEFIT-CODE (COUNT-2) GREATER THAN HOLD-BENEFIT          CL**8
04760          GO TO 5050-BENEFIT-NOTFND.                               EL131
04761                                                                   EL131
04762      GO TO 5040-FIND-BENEFIT.                                     EL131
04763                                                                   EL131
04764  5050-BENEFIT-NOTFND.                                             EL131
04765      MOVE 'X'                    TO ERROR-SWITCH.                 EL131
04766                                                                   EL131
04767  5060-EXIT.                                                       EL131
04768      EXIT.                                                        EL131
062217 5100-LOAD-CORR-TRLR.
062217     MOVE PI-COMPANY-CD      TO TRLR-COMPANY-CD.
062217     MOVE PI-CARRIER         TO TRLR-CARRIER.
062217     MOVE PI-CLAIM-NO        TO TRLR-CLAIM-NO.
062217     MOVE PI-CERT-NO         TO TRLR-CERT-NO.
062217     MOVE 1000               TO TRLR-SEQ-NO.
062217
062217     EXEC CICS HANDLE CONDITION
062217          NOTFND    (1052-DONE)
062217          ENDFILE   (1052-DONE)
062217     END-EXEC.
062217
062217     EXEC CICS STARTBR
062217         DATASET (TRLR-FILE-ID)
062217         RIDFLD  (TRLR-KEY)
062217     END-EXEC
062217
062217
062217     MOVE SPACES TO WS-TRLR-FILE-EOF
062217                    WS-AUTH-RCVD
062217     PERFORM 1052-READ-TRLR THRU 1052-EXIT
062217       UNTIL  TRLR-FILE-EOF
062217         OR WS-AUTH-RCVD > SPACES.
062217
062217     EXEC CICS ENDBR
062217         DATASET (TRLR-FILE-ID)
062217     END-EXEC.
062217
062217     IF AUTH-RCVD
062217        MOVE 'Y' TO AUTHRCVO
062217     ELSE
062217        MOVE 'N' TO AUTHRCVO
062217     END-IF.
062217
062217
062217 5100-EXIT.
062217     EXIT.

04769                                                                      CL*14
04770      EJECT                                                        EL131
04771  6000-CALCULATE-CERT-TERM.                                           CL*14
04772                                                                      CL*14
04773      IF CM-LF-PREMIUM-RATE NOT NUMERIC                               CL*14
04774          MOVE ZEROS              TO CM-LF-PREMIUM-RATE.              CL*14
04775      IF CM-AH-PREMIUM-RATE NOT NUMERIC                               CL*14
04776          MOVE ZEROS              TO CM-AH-PREMIUM-RATE.              CL*14
04777      IF CM-LOAN-APR NOT NUMERIC                                      CL*14
04778          MOVE ZEROS              TO CM-LOAN-APR.                     CL*14
04779                                                                      CL*14
04780 *    IF WS-CALC-METHOD  = 'G'                                        CL*14
04781          PERFORM 6100-CALC-GROSS-TERM THRU 6200-EXIT                 CL*14
04782 *     ELSE                                                           CL*14
04783 *        PERFORM 6500-CALC-NET-TERM   THRU 6700-EXIT.                CL*14
04784                                                                      CL*14
04785      IF N GREATER 120                                                CL*14
04786          MOVE 120                TO N.                               CL*14
04787                                                                      CL*14
04788      MOVE '6'                    TO DC-OPTION-CODE.                  CL*14
04789      MOVE N                      TO DC-ELAPSED-MONTHS.               CL*14
04790      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.                   CL*14
04791      PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.                       CL*14
04792                                                                      CL*14
04793      IF CM-LF-BENEFIT-CD NOT = ZERO                                  CL*14
04794          MOVE N                  TO CM-LF-ORIG-TERM                  CL*14
04795          COMPUTE CM-LF-BENEFIT-AMT = PI-PAYMENT-AMT * N              CL*14
04796          IF NO-CONVERSION-ERROR                                      CL*14
04797              MOVE DC-BIN-DATE-2  TO CM-LF-LOAN-EXPIRE-DT.            CL*14
04798                                                                      CL*14
04799      IF CM-AH-BENEFIT-CD NOT = ZERO                                  CL*14
04800          MOVE N                  TO CM-AH-ORIG-TERM                  CL*14
04801          IF NO-CONVERSION-ERROR                                      CL*14
04802              MOVE DC-BIN-DATE-2  TO CM-AH-LOAN-EXPIRE-DT.            CL*14
04803                                                                      CL*14
04804  6000-EXIT.                                                          CL*34
04805       EXIT.                                                          CL*14
04806                                                                      CL*14
04807  6100-CALC-GROSS-TERM.                                               CL*14
04808      MOVE CM-LOAN-BALANCE         TO L.                              CL*14
04809      MOVE PI-PAYMENT-AMT          TO M.                              CL*14
04810                                                                      CL*14
04811      COMPUTE N = L / M.                                              CL*14
04812      IF N LESS 1                                                     CL*14
04813          MOVE 1  TO N.                                               CL*14
04814                                                                      CL*14
04815      IF CM-LF-BENEFIT-CD NOT = ZERO                                  CL*14
04816          COMPUTE WS-LF-RATE = CM-LF-PREMIUM-RATE / +1000             CL*14
04817      ELSE                                                            CL*14
04818          MOVE ZEROS               TO WS-LF-RATE.                     CL*14
04819                                                                      CL*14
04820      IF CM-AH-BENEFIT-CD NOT = ZERO                                  CL*14
04821          COMPUTE WS-AH-RATE = CM-AH-PREMIUM-RATE / +1000             CL*14
04822      ELSE                                                            CL*14
04823          MOVE ZEROS               TO WS-AH-RATE.                     CL*14
04824                                                                      CL*14
04825      COMPUTE I = CM-LOAN-APR / +1200.                                CL*14
04826                                                                      CL*14
04827  6105-LOOP.                                                          CL*14
04828      IF N GREATER 240                                                CL*14
04829          GO TO 6200-EXIT.                                            CL*14
04830      PERFORM 6210-CALC-LEFT-RIGHTONE THRU 6220-EXIT.                 CL*14
04831      IF LEFT-TOT-1 GREATER RIGHT-TOT-1                               CL*14
04832          ADD 1 TO N                                                  CL*14
04833          GO TO 6105-LOOP.                                            CL*14
04834                                                                      CL*14
04835  6200-EXIT.                                                          CL*14
04836       EXIT.                                                          CL*14
04837                                                                      CL*14
04838  6210-CALC-LEFT-RIGHTONE.                                            CL*14
04839       MOVE L         TO LEFT-TOT-1.                                  CL*14
04840       SUBTRACT 1 FROM N.                                             CL*14
04841       PERFORM 6300-CALC-A-N THRU 6300-EXIT.                          CL*14
04842       PERFORM 6350-CALC-IA-N THRU 6400-EXIT.                         CL*14
04843       ADD 1 TO N.                                                    CL*14
04844  6211-CALC-TERM1.                                                    CL*14
04845       MOVE N         TO TERM1.                                       CL*14
04846       MULTIPLY M BY TERM1.                                           CL*14
04847  6212-LOOP.                                                          CL*14
04848       COMPUTE TERM1 = (WS-AH-RATE + WS-LF-RATE) * TERM1.             CL*14
04849       ADD 1 TO A-N.                                                  CL*14
04850       MULTIPLY A-N BY TERM1.                                         CL*14
04851       SUBTRACT 1 FROM A-N.                                           CL*14
04852       ADD TERM1 TO LEFT-TOT-1.                                       CL*14
04853  6213-CALC-TERM2.                                                    CL*14
04854       MOVE M         TO TERM2.                                       CL*14
04855       COMPUTE TERM2 = (WS-AH-RATE + WS-LF-RATE) * TERM2.             CL*14
04856       MULTIPLY IA-N BY TERM2.                                        CL*14
04857       SUBTRACT TERM2 FROM LEFT-TOT-1.                                CL*14
04858  6215-CALC-RIGHTONE.                                                 CL*14
04859       MOVE M         TO RIGHT-TOT-1.                                 CL*14
04860       PERFORM 6300-CALC-A-N THRU 6300-EXIT.                          CL*14
04861       MULTIPLY A-N BY RIGHT-TOT-1.                                   CL*14
04862  6220-EXIT.                                                          CL*14
04863       EXIT.                                                          CL*14
04864                                                                      CL*14
04865  6300-CALC-A-N.                                                      CL*14
04866      IF N LESS 1                                                     CL*14
04867          MOVE 0     TO A-N                                           CL*14
04868          GO TO 6300-EXIT.                                            CL*14
04869      IF I = 0                                                        CL*14
04870          MOVE .00001 TO I.                                           CL*14
04871      ADD 1 TO I.                                                     CL*14
04872      DIVIDE I INTO 1 GIVING V.                                       CL*14
04873      SUBTRACT 1 FROM I.                                              CL*14
04874      PERFORM 6450-CALC-V-EX-N THRU 6490-EXIT.                        CL*14
04875      SUBTRACT V-EX-N FROM 1 GIVING TERM3.                            CL*14
04876      DIVIDE I INTO TERM3 GIVING A-N.                                 CL*14
04877  6300-EXIT.                                                          CL*14
04878       EXIT.                                                          CL*14
04879                                                                      CL*14
04880  6350-CALC-IA-N.                                                     CL*14
04881      IF N LESS 1                                                     CL*14
04882          MOVE 0      TO IA-N                                         CL*14
04883          GO TO 6400-EXIT.                                            CL*14
04884      ADD 1 TO N.                                                     CL*14
04885      PERFORM 6450-CALC-V-EX-N THRU 6490-EXIT.                        CL*14
04886      SUBTRACT 1 FROM N.                                              CL*14
04887      MULTIPLY N BY V-EX-N GIVING TERM3.                              CL*14
04888      SUBTRACT TERM3 FROM A-N GIVING TERM3.                           CL*14
04889      SUBTRACT V FROM 1 GIVING TERM4.                                 CL*14
04890      DIVIDE TERM4 INTO TERM3 GIVING IA-N.                            CL*14
04891  6400-EXIT.                                                          CL*14
04892       EXIT.                                                          CL*14
04893                                                                      CL*14
04894  6450-CALC-V-EX-N.                                                   CL*14
04895      IF N LESS 1                                                     CL*14
04896          MOVE 1    TO V-EX-N                                         CL*14
04897          GO TO 6490-EXIT.                                            CL*14
04898      MOVE N        TO NV-STORE.                                      CL*14
04899      IF V-EX-ONETIME = 1  OR                                         CL*14
04900         V NOT = V-EXPONENT (1)                                       CL*14
04901           PERFORM 6470-BUILD-V-EX-TABLE THRU 6480-EXIT.              CL*14
04902  6460-LOOP.                                                          CL*14
04903      IF N GREATER 248                                                CL*14
04904          MOVE 248 TO N.                                              CL*14
04905      IF N = 1                                                        CL*14
04906          MOVE V               TO V-EX-N                              CL*14
04907       ELSE                                                           CL*14
04908          MOVE V-EXPONENT (N)  TO V-EX-N.                             CL*14
04909      GO TO 6490-EXIT.                                                CL*14
04910                                                                      CL*14
04911  6470-BUILD-V-EX-TABLE.                                              CL*14
04912      MOVE 2      TO N.                                               CL*14
04913      MOVE V      TO V-EXPONENT (1)                                   CL*14
04914                     V-EX-N.                                          CL*14
04915  6471-LOOP.                                                          CL*14
04916      MULTIPLY V BY V-EX-N.                                           CL*14
04917      MOVE V-EX-N   TO V-EXPONENT (N).                                CL*14
04918      ADD 1 TO N.                                                     CL*14
04919      IF N LESS 248                                                   CL*14
04920          GO TO 6471-LOOP.                                            CL*14
04921      MOVE NV-STORE     TO N.                                         CL*14
04922      MOVE 0            TO V-EX-ONETIME.                              CL*14
04923  6480-EXIT.                                                          CL*14
04924       EXIT.                                                          CL*14
04925                                                                      CL*14
04926  6490-EXIT.                                                          CL*14
04927       EXIT.                                                          CL*14
04928                                                                      CL*14
04929  6500-CALC-NET-TERM.                                                 CL*14
04930      MOVE CM-LOAN-BALANCE         TO L.                              CL*14
04931      MOVE PI-PAYMENT-AMT          TO M.                              CL*14
04932                                                                      CL*14
04933      DIVIDE L BY M GIVING N REMAINDER WS-REMAIN.                     CL*14
04934      IF WS-REMAIN GREATER ZERO                                       CL*14
04935          ADD 1 TO N.                                                 CL*14
04936      IF N = 0                                                        CL*14
04937          MOVE 1 TO N.                                                CL*14
04938                                                                      CL*14
04939  6700-EXIT.                                                          CL*14
04940       EXIT.                                                          CL*14
04941                                                                      CL*14
04942  6999-CALC-TERM-EXIT.                                                CL*14
04943      EXIT.                                                           CL*14
04944                                                                      CL*14
04945      EJECT                                                           CL*14
04946  7000-RESET-ATTRIBUTE.                                               CL*14
04947                                                                      CL*14
04948      MOVE AL-UANON               TO CERTEFFA  CERTACTA               CL*14
04949                                     CERTSTA   CERTCARA               CL*14
04950                                     CERTGRPA.                        CL*14
04951      MOVE -1                     TO CERTCARL.                        CL*14
04952                                                                      CL*14
04953  7000-EXIT.                                                          CL*14
04954      EXIT.                                                        EL131
04955                                                                   EL131
04957  7600-BROWSE-CLAIM.                                                  CL**8
04958                                                                      CL**8
04959      EXEC CICS READ                                                  CL**8
04960          DATASET(CLMS-FILE-ID)                                       CL**8
04961          SET    (ADDRESS OF CLAIM-MASTER)                            CL*32
04962          RIDFLD (MSTR-KEY)                                           CL**8
04963          GENERIC                                                     CL**8
04964          EQUAL                                                       CL**8
04965          KEYLENGTH(ELMSTR-GENERIC-LENGTH)                            CL**8
04966      END-EXEC.                                                       CL**8
04967                                                                      CL**8
04968      MOVE CL-CONTROL-PRIMARY     TO MSTR-KEY.                        CL**8
04969                                                                      CL**8
032612     if cl-assoc-cert-total = 0
032612        move 1 to cl-assoc-cert-total
032612     end-if
032612     IF CL-ASSOC-CERT-SEQU = 0
032612        MOVE 1 TO CL-ASSOC-CERT-SEQU
032612     END-IF
032612
04970      IF CL-ASSOC-CERT-TOTAL = 1                                      CL*34
04971          MOVE AL-SADOF           TO BCERT1A                          CL**8
04972                                     BSUFX1A                          CL**8
04973                                     BCERT2A                          CL**8
04974                                     BSUFX2A                          CL**8
04975                                     BCERT3A                          CL**8
04976                                     BSUFX3A                          CL**8
04977                                     BCERT4A                          CL**8
04978                                     BSUFX4A                          CL**8
04979                                     BCERT5A                          CL**8
04980                                     BSUFX5A                          CL**8
04981                                     PCERTNOA                         CL**8
04982                                     PSUFXA                           CL**8
04983                                     LABEL1A                          CL**8
04984                                     LABEL2A                          CL**8
04985          GO TO 7699-EXIT.                                            CL**8
04986                                                                      CL**8
04987  7610-BROWSE-CLAIM-LOOP.                                             CL**8
04988                                                                      CL**8
04989      MOVE LOW-VALUES             TO BCERT1O                          CL**8
04990                                     BSUFX1O                          CL**8
04991                                     BCERT2O                          CL**8
04992                                     BSUFX2O                          CL**8
04993                                     BCERT3O                          CL**8
04994                                     BSUFX3O                          CL**8
04995                                     BCERT4O                          CL**8
04996                                     BSUFX4O                          CL**8
04997                                     BCERT5O                          CL**8
04998                                     BSUFX5O.                         CL**8
04999                                                                      CL**8
05000      EXEC CICS HANDLE CONDITION                                      CL**8
05001          ENDFILE  (7630-END-BROWSE)                                  CL**8
05002      END-EXEC.                                                       CL**8
05003                                                                      CL**8
05004      EXEC CICS STARTBR                                               CL**8
05005          DATASET    (CLMS-FILE-ID)                                   CL**8
05006          RIDFLD     (MSTR-KEY)                                       CL**8
05007      END-EXEC.                                                       CL**8
05008                                                                      CL**8
05009      MOVE +1                     TO WS-ASSOCIATED-CERTS.             CL**8
05010                                                                      CL**8
05011  7620-READ-CLAIM-LOOP.                                               CL**8
05012                                                                      CL**8
05013      EXEC CICS READNEXT                                              CL**8
05014          DATASET   (CLMS-FILE-ID)                                    CL**8
05015          SET       (ADDRESS OF CLAIM-MASTER)                         CL*32
05016          RIDFLD    (MSTR-KEY)                                        CL**8
05017      END-EXEC.                                                       CL**8
05018                                                                      CL**8
05019      IF CL-COMPANY-CD  NOT = PI-COMPANY-CD   OR                      CL*34
05020         CL-CARRIER     NOT = CARRI           OR                      CL*34
05021         CL-CLAIM-NO    NOT = CLAIMI                                  CL*34
05022         GO TO 7630-END-BROWSE.                                       CL**8
05023                                                                      CL**8
05024      IF WS-ASSOCIATED-CERTS = 1                                      CL*34
05025          MOVE CL-CERT-PRIME      TO BCERT1O                          CL**8
05026          MOVE CL-CERT-SFX        TO BSUFX1O.                         CL**8
05027                                                                      CL**8
05028      IF WS-ASSOCIATED-CERTS = 2                                      CL*34
05029          MOVE CL-CERT-PRIME      TO BCERT2O                          CL**8
05030          MOVE CL-CERT-SFX        TO BSUFX2O.                         CL**8
05031                                                                      CL**8
05032      IF WS-ASSOCIATED-CERTS = 3                                      CL*34
05033          MOVE CL-CERT-PRIME      TO BCERT3O                          CL**8
05034          MOVE CL-CERT-SFX        TO BSUFX3O.                         CL**8
05035                                                                      CL**8
05036      IF WS-ASSOCIATED-CERTS = 4                                      CL*34
05037          MOVE CL-CERT-PRIME      TO BCERT4O                          CL**8
05038          MOVE CL-CERT-SFX        TO BSUFX4O.                         CL**8
05039                                                                      CL**8
05040      IF WS-ASSOCIATED-CERTS = 5                                      CL*34
05041          MOVE CL-CERT-PRIME      TO BCERT5O                          CL**8
05042          MOVE CL-CERT-SFX        TO BSUFX5O                          CL**8
05043      ELSE                                                            CL**8
05044          ADD +1                  TO WS-ASSOCIATED-CERTS              CL**8
05045          GO TO 7620-READ-CLAIM-LOOP.                                 CL**8
05046                                                                      CL**8
05047  7630-END-BROWSE.                                                    CL**8
05048                                                                      CL**8
05049      EXEC CICS ENDBR                                                 CL**8
05050          DATASET   (CLMS-FILE-ID)                                    CL**8
05051      END-EXEC.                                                       CL**8
05052                                                                      CL**8
05053  7640-HIGHLIGHT-CERT-DISPLAYED.                                      CL**8
05054                                                                      CL**8
05055      MOVE AL-SANON               TO BCERT1A                          CL**8
05056                                     BSUFX1A                          CL**8
05057                                     BCERT2A                          CL**8
05058                                     BSUFX2A                          CL**8
05059                                     BCERT3A                          CL**8
05060                                     BSUFX3A                          CL**8
05061                                     BCERT4A                          CL**8
05062                                     BSUFX4A                          CL**8
05063                                     BCERT5A                          CL**8
05064                                     BSUFX5A.                         CL**8
05065                                                                      CL**8
05066      IF BCERT1O = CERTI AND                                          CL*34
05067         BSUFX1O = SUFXI                                              CL*34
05068             MOVE AL-SABON        TO BCERT1A                          CL**8
05069                                     BSUFX1A.                         CL**8
05070                                                                      CL**8
05071      IF BCERT2O = CERTI AND                                          CL*34
05072         BSUFX2O = SUFXI                                              CL*34
05073             MOVE AL-SABON        TO BCERT2A                          CL**8
05074                                     BSUFX2A.                         CL**8
05075                                                                      CL**8
05076      IF BCERT3O = CERTI AND                                          CL*34
05077         BSUFX3O = SUFXI                                              CL*34
05078             MOVE AL-SABON        TO BCERT3A                          CL**8
05079                                     BSUFX3A.                         CL**8
05080                                                                      CL**8
05081      IF BCERT4O = CERTI AND                                          CL*34
05082         BSUFX4O = SUFXI                                              CL*34
05083             MOVE AL-SABON        TO BCERT4A                          CL**8
05084                                     BSUFX4A.                         CL**8
05085                                                                      CL**8
05086      IF BCERT5O = CERTI AND                                          CL*34
05087         BSUFX5O = SUFXI                                              CL*34
05088             MOVE AL-SABON        TO BCERT5A                          CL**8
05089                                     BSUFX5A.                         CL**8
05090                                                                      CL**8
05091  7699-EXIT.                                                          CL**8
05092      EXIT.                                                           CL**8
05093                                                                      CL**8
05094      EJECT                                                           CL**8
05095  7700-CHECK-SEQUENCE.                                                CL**8
05096                                                                      CL**8
05097      EXEC CICS HANDLE CONDITION                                      CL**8
05098          ENDFILE  (7799-EXIT)                                        CL**8
05099          NOTFND   (7799-EXIT)                                        CL**8
05100      END-EXEC.                                                       CL**8
05101                                                                      CL**8
05102      EXEC CICS READ                                                  CL**8
05103          DATASET(CLMS-FILE-ID)                                       CL**8
05104          SET    (ADDRESS OF CLAIM-MASTER)                            CL*32
05105          RIDFLD (MSTR-KEY)                                           CL**8
05106          GENERIC                                                     CL**8
05107          KEYLENGTH(ELMSTR-GENERIC-LENGTH)                            CL**8
05108      END-EXEC.                                                       CL**8
05109                                                                      CL**8
05110      COMPUTE WS-ASSOC-CERT-TOTAL =                                   CL*34
05111              CL-ASSOC-CERT-TOTAL + ONE-OR-MIN1.                      CL*34
05112                                                                      CL*34
05113      GO TO 7799-EXIT.                                                CL**8
05114                                                                      CL**8
05115  7710-RESEQUENCE-CLAIMS.                                             CL**8
05116                                                                      CL**8
05117      EXEC CICS HANDLE CONDITION                                      CL**8
05118          ENDFILE  (7799-EXIT)                                        CL*32
05119      END-EXEC.                                                       CL**8
05120                                                                      CL**8
05121      EXEC CICS STARTBR                                               CL**8
05122          DATASET(CLMS-FILE-ID)                                       CL**8
05123          RIDFLD (MSTR-KEY)                                           CL**8
05124      END-EXEC.                                                       CL**8
05125                                                                      CL**8
05126      COMPUTE WS-ASSOC-CERT-SEQU =                                    CL*34
05127              WS-ASSOC-CERT-SEQU + 1.                                 CL*34
05128                                                                      CL*34
05129      COMPUTE WS-READNEXT-SWITCH =                                    CL*34
05130              WS-READNEXT-SWITCH + 1.                                 CL*34
05131                                                                      CL**8
05132  7720-READ-CLAIM-LOOP.                                               CL**8
05133                                                                      CL**8
05134      EXEC CICS READNEXT                                              CL**8
05135          DATASET(CLMS-FILE-ID)                                       CL**8
05136          SET    (ADDRESS OF CLAIM-MASTER)                            CL*32
05137          RIDFLD (MSTR-KEY)                                           CL**8
05138      END-EXEC.                                                       CL**8
05139                                                                      CL**8
05140      IF WS-READNEXT-SWITCH = 1                                       CL*34
05141          ADD 1                   TO WS-READNEXT-SWITCH               CL*34
05142          GO TO 7720-READ-CLAIM-LOOP.                                 CL**8
05143                                                                      CL**8
05144  7730-END-BROWSE.                                                    CL**8
05145                                                                      CL**8
05146      EXEC CICS ENDBR                                                 CL**8
05147          DATASET(CLMS-FILE-ID)                                       CL**8
05148      END-EXEC.                                                       CL**8
05149                                                                      CL**8
05150  7740-READ-CLAIM-UPDATE.                                             CL**8
05151                                                                      CL**8
05152      IF CL-COMPANY-CD  NOT = WS-SAVE-COMPANY-CD  OR                  CL*34
05153         CL-CARRIER     NOT = WS-SAVE-CARRIER     OR                  CL*34
05154         CL-CLAIM-NO    NOT = WS-SAVE-CLAIM-NO                        CL*34
05155         GO TO 7799-EXIT                                              CL**8
05156      ELSE                                                            CL**8
05157         MOVE ZERO                TO WS-READNEXT-SWITCH.              CL**8
05158                                                                      CL**8
05159      EXEC CICS READ                                                  CL**8
05160          DATASET(CLMS-FILE-ID)                                       CL**8
05161          SET    (ADDRESS OF CLAIM-MASTER)                            CL*32
05162          RIDFLD (MSTR-KEY)                                           CL**8
05163          UPDATE                                                      CL**8
05164      END-EXEC.                                                       CL**8
05165                                                                      CL**8
062602     if (cl-priority-cd = '8')
062602        and (pi-processor-id not = 'PEMA' and 'JMS '
062602             AND 'AMWA')
062602        MOVE ER-8003             TO EMI-ERROR
062602        PERFORM 9900-ERROR-FORMAT
062602                                 THRU 9900-EXIT
062602        MOVE -1                  TO MAINTL
062602        GO TO 8110-SEND-DATA
062602     end-if
062602
062121     IF PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL'
CIDMOD         MOVE 'Y'                TO CL-YESNOSW
CIDMOD     END-IF.
02826                                                                   EL131
05166      MOVE WS-ASSOC-CERT-TOTAL    TO CL-ASSOC-CERT-TOTAL.             CL**8
05167      MOVE WS-ASSOC-CERT-SEQU     TO CL-ASSOC-CERT-SEQU.              CL**8
05168                                                                      CL**8
05169      EXEC CICS REWRITE                                               CL**8
05170           DATASET(CLMS-FILE-ID)                                      CL**8
05171           FROM   (CLAIM-MASTER)                                      CL**8
05172      END-EXEC.                                                       CL**8
05173                                                                      CL**8
05174      GO TO 7710-RESEQUENCE-CLAIMS.                                   CL**8
05175                                                                      CL**8
05176  7799-EXIT.                                                          CL**8
05177      EXIT.                                                           CL**8
05178                                                                      CL**8
05179      EJECT                                                           CL**8
05180  7800-CHECK-AUTO-ACTIVITY.                                           CL*26
05181                                                                      CL*26
05182      EXEC CICS HANDLE CONDITION                                      CL*26
05183          NOTFND   (7800-NOT-FOUND)                                   CL*26
05184      END-EXEC.                                                       CL*26
05185                                                                      CL*26
05186      MOVE PI-COMPANY-ID              TO  CNTL-CO-ID.                 CL*26
05187      MOVE 'T'                        TO  CNTL-REC-TYPE.              CL*26
05188      MOVE SPACES                     TO  CNTL-PROC-ID.               CL*26
05189      MOVE +0                         TO  CNTL-SEQ-NO.                CL*26
05190                                                                      CL*26
05191      EXEC CICS READ                                                  CL*26
05192          DATASET   (CNTL-FILE-ID)                                    CL*26
05193          RIDFLD    (CNTL-KEY)                                        CL*26
05194          SET       (ADDRESS OF CONTROL-FILE)                         CL*32
05195      END-EXEC.                                                       CL*26
05196                                                                      CL*26
05197      IF CL-ACTIVITY-CODE IS NOT NUMERIC                              CL*26
05198          MOVE ZEROS                  TO  CL-ACTIVITY-CODE.           CL*26
05199                                                                      CL*26
05200      IF CL-ACTIVITY-CODE NOT = ZEROS                                 CL*34
05201          MOVE CL-ACTIVITY-CODE                 TO  MISC-SUB          CL*26
05202          IF MISC-SUB IS GREATER THAN +9                              CL*26
05203              SUBTRACT +9 FROM MISC-SUB                               CL*26
05204              MOVE CF-USER-RESET-SW (MISC-SUB)  TO  WS-RESET-SW       CL*26
05205          ELSE                                                        CL*26
05206              MOVE CF-SYS-RESET-SW  (MISC-SUB)  TO  WS-RESET-SW       CL*26
05207      ELSE                                                            CL*26
05208          MOVE 'Y'                              TO  WS-RESET-SW.      CL*26
05209                                                                      CL*26
05210      IF STATUSI = 'C' OR 'CLOSED'                                    CL*34
05211          IF CF-SYS-ACTIVE-SW (7) = ' ' OR 'N'                        CL*34
05212              MOVE 'N'                TO  PI-REC-FOUND-SW             CL*26
05213                                          PI-LETTER-SW                CL*26
05214              GO TO 7800-EXIT.                                        CL*26
05215                                                                      CL*26
05216      IF STATUSI = 'C' OR 'CLOSED'                                    CL*34
05217          IF CF-SYS-LETTER-ID (7) = SPACES OR LOW-VALUES              CL*34
05218              MOVE 'N'                TO  PI-LETTER-SW                CL*26
05219          ELSE                                                        CL*26
05220              MOVE 'Y'                TO  PI-LETTER-SW.               CL*26
05221                                                                      CL*26
05222      IF STATUSI = 'O' OR 'OPEN'                                      CL*34
05223          IF CF-SYS-ACTIVE-SW (8) = ' ' OR 'N'                        CL*34
05224              MOVE 'N'                TO  PI-REC-FOUND-SW             CL*26
05225                                          PI-LETTER-SW                CL*29
05226              GO TO 7800-EXIT.                                        CL*29
05227                                                                      CL*26
05228      IF STATUSI = 'O' OR 'OPEN'                                      CL*34
05229          IF CF-SYS-LETTER-ID (8) = SPACES OR LOW-VALUES              CL*34
05230              MOVE 'N'                TO  PI-LETTER-SW                CL*26
05231          ELSE                                                        CL*26
05232              MOVE 'Y'                TO  PI-LETTER-SW.               CL*26
05233                                                                      CL*26
05234      MOVE 'Y'                        TO  PI-REC-FOUND-SW.            CL*26
05235      GO TO 7800-EXIT.                                                CL*26
05236                                                                      CL*26
05237  7800-NOT-FOUND.                                                     CL*26
05238                                                                      CL*26
05239      MOVE 'N'                        TO  PI-REC-FOUND-SW             CL*26
05240                                          PI-LETTER-SW.               CL*26
05241                                                                      CL*26
05242  7800-EXIT.                                                          CL*26
05243      EXIT.                                                           CL*26
05244                                                                      CL*26
05245  7850-AUTO-LETTER-WRITER.                                            CL*26
05246                                                                      CL*26
05247      MOVE LOW-VALUES                 TO  W-1523-LINKDATA.            CL*26
05248      MOVE PROGRAM-INTERFACE-BLOCK    TO  W-1523-COMMON-PI-DATA.      CL*26
05249                                                                      CL*26
05250      IF STATUSI = 'C' OR 'CLOSED'                                    CL*34
05251          MOVE CF-SYS-LETTER-ID (7)   TO  W-1523-FORM-NUMBER          CL*26
05252      ELSE                                                            CL*26
05253          MOVE CF-SYS-LETTER-ID (8)   TO  W-1523-FORM-NUMBER.         CL*26
05254                                                                      CL*26
05255      IF STATUSI = 'C' OR 'CLOSED'                                    CL*34
05256          IF CF-SYS-RESEND-DAYS (7) NOT = ZEROS                       CL*34
05257              MOVE SAVE-BIN-DATE      TO  DC-BIN-DATE-1               CL*26
05258              MOVE '6'                TO  DC-OPTION-CODE              CL*26
05259              MOVE CF-SYS-RESEND-DAYS (7) TO  DC-ELAPSED-DAYS         CL*26
05260              MOVE +0                 TO  DC-ELAPSED-MONTHS           CL*26
05261              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                CL*26
05262              IF NO-CONVERSION-ERROR                                  CL*26
05263                  MOVE DC-BIN-DATE-2  TO  W-1523-RESEND-DATE          CL*26
05264              ELSE                                                    CL*26
05265                  MOVE LOW-VALUES     TO  W-1523-RESEND-DATE.         CL*26
05266                                                                      CL*26
05267      IF STATUSI = 'C' OR 'CLOSED'                                    CL*34
05268          IF CF-SYS-FOLLOW-UP-DAYS (7) NOT = ZEROS                    CL*34
05269              MOVE SAVE-BIN-DATE      TO  DC-BIN-DATE-1               CL*26
05270              MOVE '6'                TO  DC-OPTION-CODE              CL*26
05271              MOVE CF-SYS-FOLLOW-UP-DAYS (7) TO  DC-ELAPSED-DAYS      CL*26
05272              MOVE +0                 TO  DC-ELAPSED-MONTHS           CL*26
05273              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                CL*26
05274              IF NO-CONVERSION-ERROR                                  CL*26
05275                  MOVE DC-BIN-DATE-2  TO  W-1523-FOLLOW-UP-DATE       CL*26
05276              ELSE                                                    CL*26
05277                  MOVE LOW-VALUES     TO  W-1523-FOLLOW-UP-DATE.      CL*26
05278                                                                      CL*26
05279      IF STATUSI = 'O' OR 'OPEN'                                      CL*34
05280          IF CF-SYS-RESEND-DAYS (8) NOT = ZEROS                       CL*34
05281              MOVE SAVE-BIN-DATE      TO  DC-BIN-DATE-1               CL*26
05282              MOVE '6'                TO  DC-OPTION-CODE              CL*26
05283              MOVE CF-SYS-RESEND-DAYS (8) TO  DC-ELAPSED-DAYS         CL*26
05284              MOVE +0                 TO  DC-ELAPSED-MONTHS           CL*26
05285              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                CL*26
05286              IF NO-CONVERSION-ERROR                                  CL*26
05287                  MOVE DC-BIN-DATE-2  TO  W-1523-RESEND-DATE          CL*26
05288              ELSE                                                    CL*26
05289                  MOVE LOW-VALUES     TO  W-1523-RESEND-DATE.         CL*26
05290                                                                      CL*26
05291      IF STATUSI = 'O' OR 'OPEN'                                      CL*34
05292          IF CF-SYS-FOLLOW-UP-DAYS (8) NOT = ZEROS                    CL*34
05293              MOVE SAVE-BIN-DATE      TO  DC-BIN-DATE-1               CL*26
05294              MOVE '6'                TO  DC-OPTION-CODE              CL*26
05295              MOVE CF-SYS-FOLLOW-UP-DAYS (8) TO  DC-ELAPSED-DAYS      CL*26
05296              MOVE +0                 TO  DC-ELAPSED-MONTHS           CL*26
05297              PERFORM 9800-CONVERT-DATE THRU 9800-EXIT                CL*26
05298              IF NO-CONVERSION-ERROR                                  CL*26
05299                  MOVE DC-BIN-DATE-2  TO  W-1523-FOLLOW-UP-DATE       CL*26
05300              ELSE                                                    CL*26
05301                  MOVE LOW-VALUES     TO  W-1523-FOLLOW-UP-DATE.      CL*26
05302                                                                      CL*26
05303      EXEC CICS LINK                                                  CL*26
05304          PROGRAM    (LINK-1523)                                      CL*26
05305          COMMAREA   (W-1523-LINKDATA)                                CL*26
05306          LENGTH     (W-1523-COMM-LENGTH)                             CL*26
05307      END-EXEC.                                                       CL*26
05308                                                                      CL*26
05309      IF W-1523-ERROR-CODE = ZEROS                                    CL*34
05310          GO TO 7850-EXIT.                                            CL*26
05311                                                                      CL*26
05312      IF W-1523-FATAL-ERROR                                           CL*26
05313          MOVE ER-0802                TO  EMI-ERROR                   CL*26
05314          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*26
05315                                                                      CL*26
05316      IF W-1523-ERROR-CODE = 0191                                     CL*34
05317          MOVE ER-0803                TO  EMI-ERROR                   CL*26
05318          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*26
05319                                                                      CL*26
05320  7850-EXIT.                                                          CL*26
05321      EXIT.                                                           CL*26
05322                                                                      CL*26

090821 7900-READ-BENEFIT.
090821
090821     move spaces                 to cntl-key
090821     move pi-company-id          to cntl-co-id
090821     if cl-claim-type = 'L' or 'O' or 'P'
090821        move cm-lf-benefit-cd    to cntl-benefit
090821                                    ws-ben-hold
090821        move '4'                 to cntl-rec-type
090821                                    ws-rec-type
090821     else
090821        move cm-ah-benefit-cd    to cntl-benefit
090821                                    ws-ben-hold
090821        move '5'                 to cntl-rec-type
090821                                    ws-rec-type
090821     end-if
090821     MOVE ZEROS                  TO CNTL-SEQ-NO
090821
090821     .
090821 7900-READ-FILE.                                                  
090821
090821     PERFORM 7975-READ-CNTL-GTEQ THRU 7975-EXIT
090821
090821     if not WS-RESP-NORMAL
090821        go to 7900-not-found
090821     end-if
090821
090821     IF (PI-COMPANY-ID NOT = CF-COMPANY-ID)  OR
090821        (WS-REC-TYPE   NOT = CF-RECORD-TYPE)
090821        GO TO 7900-NOT-FOUND
090821     END-IF
090821                                                                  
090821     MOVE 1                      TO SUB
090821
090821     .
090821 7900-LOOP.                                                       
090821     IF SUB = 9                                                   
090821         GO TO 7900-NOT-FOUND
090821     END-IF
090821                                                                  
090821     IF WS-BEN-HOLD <> CF-BENEFIT-CODE (SUB)                   
090821        ADD 1                    TO SUB                           
090821        GO TO 7900-LOOP
090821     END-IF
090821                                                                  
090821     MOVE CF-SPECIAL-CALC-CD (SUB)  TO WS-SPECIAL-CALC-CD.        
090821     if (ws-special-calc-cd = 'O')
090821                OR
090821        (pi-company-id = 'DCC' and cl-carrier = '7')
090821        set mob-cert to true
090821     end-if
090821     GO TO 7900-EXIT
090821
090821     .                                                                        
090821 7900-NOT-FOUND.                                                  
090821     MOVE SPACES                 TO WS-special-calc-cd
090821
090821     .                                                                        
090821 7900-EXIT.                                                       
090821      EXIT.                                                       
090821
090821 7975-READ-CNTL-GTEQ.                                             
090821     
090821     EXEC CICS READ
090821          DATASET   ('ELCNTL')
090821          SET       (ADDRESS OF CONTROL-FILE)
090821          RIDFLD    (CNTL-KEY)
090821          resp      (ws-response)
090821          GTEQ
090821      END-EXEC.
090821                                                                  
090821 7975-EXIT.                                                       
090821      EXIT.                                                       
                                                                        
090821 7990-get-lo-hi-acct-dates.
090821
090821     MOVE PI-COMPANY-CD       TO ACCT-company-code
090821     MOVE PI-CARRIER          TO ACCT-CARRIER
090821     MOVE PI-GROUPING         TO ACCT-GROUP
090821     MOVE PI-STATE            TO ACCT-STATE
090821     MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
090821     MOVE low-values          TO ACCT-date
090821     MOVE ACCT-KEY            TO SAVE-ACCT-KEY
090821
090821     move spaces              to ws-i-say-stop-ind
090821                                 ws-eracct-startbr-ind
090821                                 ws-acct-status
090821
090821     EXEC CICS STARTBR
090821          DATASET    ('ERACCT')
090821          RIDFLD     (ACCT-KEY)
090821          GTEQ
090821          resp       (ws-response)
090821     END-EXEC
090821
090821     if ws-resp-normal
090821        set eracct-browse-started to true
090821     end-if
090821
090821     perform until i-say-stop
090821        EXEC CICS READNEXT
090821           DATASET ('ERACCT')
090821           RIDFLD  (ACCT-KEY)
090821           SET     (ADDRESS OF ACCOUNT-MASTER)
090821           resp    (WS-RESPONSE)
090821        END-EXEC
090821
090821        IF WS-RESP-NORMAL
090821           AND save-acct-key(1:20) =
090821                       AM-CONTROL-PRIMARY (1:20)
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
090821     end-if
090821
090821     perform 7900-READ-BENEFIT   thru 7900-exit
090821
090821     .
090821 7990-exit.
090821     exit.


05324  8000-CREATE-DMO-REC.                                                CL*34
05325      MOVE PI-COMPANY-CD          TO NOTE-COMP-CD.                    CL*34
05326      MOVE CL-CERT-KEY-DATA       TO NOTE-CERT-KEY.                   CL*34
05327      MOVE CL-CERT-NO             TO NOTE-CERT-NO.                    CL*34
05328                                                                      CL*34
05329      EXEC CICS HANDLE CONDITION                                      CL*34
05330           NOTFND   (8000-NOTE-NOT-FOUND)                             CL*34
05331      END-EXEC.                                                       CL*34
05332                                                                      CL*34
05333      EXEC CICS READ                                                  CL*34
05334           DATASET(NOTE-FILE-ID)                                      CL*34
05335           SET    (ADDRESS OF CERTIFICATE-NOTE)                       CL*34
05336           RIDFLD (NOTE-KEY)                                          CL*34
05337      END-EXEC.                                                       CL*34
05338                                                                      CL*34
05339      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.          CL*34
05340      MOVE CL-BENEFICIARY         TO DCT-LOGIC-BENEFICIARY-ID.        CL*34
05341      MOVE CL-CCN                 TO DCT-CREDIT-CARD-NUMBER.          CL*34
05342                                                                      CL*34
05343      IF CL-CERT-GROUPING (5:2) = SPACES OR ZEROS                     CL*39
05344          MOVE 'CC'               TO DCT-PRODUCT-CODE                 CL*34
05345      ELSE                                                            CL*34
05346          MOVE CL-CERT-GROUPING (5:2) TO DCT-PRODUCT-CODE.            CL*39
05347                                                                      CL*34
05348      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.             CL*36
05349      MOVE '02'                   TO DCT-COLUMN-ID-REQUESTED.         CL*34
05350                                                                      CL*34
05351      EXEC CICS LINK                                                  CL*36
05352          PROGRAM    ('DLO006')                                       CL*36
05353          COMMAREA   (DCT-COMMUNICATION-AREA)                         CL*36
05354          LENGTH     (WS-DCT-LENGTH)                                  CL*36
05355      END-EXEC.                                                       CL*36
05356                                                                      CL*34
05357      IF  DCT-RETURN-CODE = 'OK'                                      CL*36
05358          GO TO 8000-CONT.                                            CL*36
05359                                                                      CL*34
05360      IF DCT-RETURN-CODE = '01' OR '02'                               CL*34
05361          GO TO 8000-EXIT.                                            CL*34
05362                                                                      CL*34
05363      IF DCT-RETURN-CODE = '03'                                       CL*34
05364          MOVE ER-0951            TO EMI-ERROR                        CL*34
05365          MOVE -1                 TO MAINTL                           CL*34
05366          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
05367          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*34
05368          GO TO 8110-SEND-DATA.                                       CL*34
05369                                                                      CL*34
05370      IF DCT-RETURN-CODE = '04'                                       CL*34
05371          MOVE ER-0946            TO EMI-ERROR                        CL*34
05372          MOVE -1                 TO MAINTL                           CL*34
05373          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
05374          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*34
05375          GO TO 8110-SEND-DATA.                                       CL*34
05376                                                                      CL*34
05377      IF DCT-RETURN-CODE = '05'                                       CL*34
05378          MOVE ER-0947            TO EMI-ERROR                        CL*34
05379          MOVE -1                 TO MAINTL                           CL*34
05380          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
05381          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*34
05382          GO TO 8110-SEND-DATA.                                       CL*34
05383                                                                      CL*34
05384      IF DCT-RETURN-CODE = '06'                                       CL*36
05385          MOVE ER-0921            TO EMI-ERROR                        CL*36
05386          MOVE -1                 TO MAINTL                           CL*36
05387          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05388          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05389          GO TO 8110-SEND-DATA.                                       CL*36
05390                                                                      CL*36
05391      IF DCT-RETURN-CODE = '07'                                       CL*36
05392          MOVE ER-0919            TO EMI-ERROR                        CL*36
05393          MOVE -1                 TO MAINTL                           CL*36
05394          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05395          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05396          GO TO 8110-SEND-DATA.                                       CL*36
05397                                                                      CL*36
05398      IF DCT-RETURN-CODE = '08'                                       CL*34
05399          MOVE ER-0948            TO EMI-ERROR                        CL*34
05400          MOVE -1                 TO MAINTL                           CL*34
05401          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
05402          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*34
05403          GO TO 8110-SEND-DATA.                                       CL*34
05404                                                                      CL*34
05405      IF DCT-RETURN-CODE = 'N1'                                       CL*34
05406          MOVE ER-0950            TO EMI-ERROR                        CL*34
05407          MOVE -1                 TO MAINTL                           CL*34
05408          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
05409          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*34
05410          GO TO 8110-SEND-DATA.                                       CL*34
05411                                                                      CL*34
05412      IF DCT-RETURN-CODE = 'E1'                                       CL*43
05413          MOVE ER-0974            TO EMI-ERROR                        CL*43
05414          MOVE -1                 TO MAINTL                           CL*43
05415          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*43
05416          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*43
05417          GO TO 8110-SEND-DATA.                                       CL*43
05418                                                                      CL*43
05419      IF DCT-RETURN-CODE = 'E2'                                       CL*43
05420          MOVE ER-0975            TO EMI-ERROR                        CL*43
05421          MOVE -1                 TO MAINTL                           CL*43
05422          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*43
05423          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*43
05424          GO TO 8110-SEND-DATA.                                       CL*43
05425                                                                      CL*43
05426      IF DCT-RETURN-CODE NOT = 'OK'                                   CL*43
05427          MOVE ER-0949            TO EMI-ERROR                        CL*36
05428          MOVE -1                 TO MAINTL                           CL*36
05429          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05430          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05431          GO TO 8110-SEND-DATA.                                       CL*36
05432                                                                      CL*36
05433  8000-CONT.                                                          CL*36
05434                                                                      CL*34
05435      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.          CL*34
05436      MOVE 'CS'                   TO DM-RECORD-TYPE.                  CL*34
05437      MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.                    CL*34
05438      MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.                    CL*34
05439      MOVE CL-CLAIM-NO            TO DM-CLAIM-NO.                     CL*34
05440      MOVE CL-CERT-NO (4:1)       TO DM-CLAIM-TYPE.                   CL*42
05441      MOVE CL-CCN                 TO DM-CREDIT-CARD-NUMBER.           CL*36
05442      MOVE SAVE-DATE-CCYYMMDD     TO DM-STATUS-DATE.                  CL*34
05443                                                                      CL*34
05444      MOVE CL-INSURED-LAST-NAME   TO W-NAME-LAST.                     CL*34
05445      MOVE CL-INSURED-1ST-NAME    TO W-NAME-FIRST.                    CL*34
05446      MOVE CL-INSURED-MID-INIT    TO W-NAME-MIDDLE.                   CL*34
05447      PERFORM 8050-FORMAT-LAST-NAME-1ST THRU 8050-EXIT.               CL*34
05448      MOVE WS-NAME-WORK           TO DM-INSURED-NAME.                 CL*34
05449                                                                      CL*34
05450      IF STATUSI = 'OPEN' OR 'O'                                      CL*44
05451          MOVE 'R'                TO DM-STAT-CHANGE-TYPE              CL*44
05452       ELSE                                                           CL*44
05453          MOVE 'C'                TO DM-STAT-CHANGE-TYPE.             CL*44
05454                                                                      CL*44
05455      IF STATUSI = 'OPEN' OR 'O'                                      CL*44
05456         IF CL-NO-OF-PMTS-MADE = 0                                    CL*44
05457          MOVE '1'                TO DM-CLAIM-STATUS                  CL*44
05458       ELSE                                                           CL*44
05459          MOVE '2'                TO DM-CLAIM-STATUS.                 CL*44
05460                                                                      CL*44
05461      IF STATUSI = 'CLOSED' OR 'C'                                    CL*44
05462         IF NOT FINAL-PAID                                            CL*44
05463          MOVE '3'                TO DM-CLAIM-STATUS                  CL*44
05464       ELSE                                                           CL*44
05465          MOVE '4'                TO DM-CLAIM-STATUS.                 CL*44
05466                                                                      CL*44
05467      MOVE CL-CARRIER             TO DM-STAT-CARRIER.                 CL*42
05468                                                                      CL*34
05469      EXEC CICS LINK                                                  CL*36
05470          PROGRAM    ('DLO025')                                       CL*36
05471          COMMAREA   (DMO-COMMUNICATION-AREA)                         CL*36
05472          LENGTH     (WS-DMO-LENGTH)                                  CL*36
05473      END-EXEC.                                                       CL*36
05474                                                                      CL*36
05475      IF  DM-RETURN-CODE = 'OK'                                       CL*36
05476          GO TO 8000-EXIT.                                            CL*36
05477                                                                      CL*34
05478      IF DM-RETURN-CODE = '01'                                        CL*36
05479          MOVE ER-8051            TO EMI-ERROR                        CL*36
05480          MOVE -1                 TO MAINTL                           CL*36
05481          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05482          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05483          GO TO 8110-SEND-DATA.                                       CL*36
05484                                                                      CL*34
05485      IF DM-RETURN-CODE = '02'                                        CL*36
05486          MOVE ER-8052            TO EMI-ERROR                        CL*36
05487          MOVE -1                 TO MAINTL                           CL*34
05488          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
05489          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*34
05490          GO TO 8110-SEND-DATA.                                       CL*34
05491                                                                      CL*34
05492      IF DM-RETURN-CODE = '03'                                        CL*36
05493          MOVE ER-8053            TO EMI-ERROR                        CL*36
05494          MOVE -1                 TO MAINTL                           CL*36
05495          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05496          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05497          GO TO 8110-SEND-DATA.                                       CL*36
05498                                                                      CL*36
05499      IF DM-RETURN-CODE = '04'                                        CL*36
05500          MOVE ER-8054            TO EMI-ERROR                        CL*36
05501          MOVE -1                 TO MAINTL                           CL*36
05502          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05503          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05504          GO TO 8110-SEND-DATA.                                       CL*36
05505                                                                      CL*36
05506      IF DM-RETURN-CODE = '05'                                        CL*36
05507          MOVE ER-8055            TO EMI-ERROR                        CL*36
05508          MOVE -1                 TO MAINTL                           CL*36
05509          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05510          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05511          GO TO 8110-SEND-DATA.                                       CL*36
05512                                                                      CL*36
05513      IF DM-RETURN-CODE = '06'                                        CL*36
05514          MOVE ER-8056            TO EMI-ERROR                        CL*36
05515          MOVE -1                 TO MAINTL                           CL*36
05516          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05517          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05518          GO TO 8110-SEND-DATA.                                       CL*36
05519                                                                      CL*36
05520      IF DM-RETURN-CODE = '07'                                        CL*36
05521          MOVE ER-8057            TO EMI-ERROR                        CL*36
05522          MOVE -1                 TO MAINTL                           CL*36
05523          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05524          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05525          GO TO 8110-SEND-DATA.                                       CL*36
05526                                                                      CL*36
05527      IF DM-RETURN-CODE = '08'                                        CL*36
05528          MOVE ER-8058            TO EMI-ERROR                        CL*36
05529          MOVE -1                 TO MAINTL                           CL*36
05530          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05531          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05532          GO TO 8110-SEND-DATA.                                       CL*36
05533                                                                      CL*36
05534      IF DM-RETURN-CODE = '09'                                        CL*36
05535          MOVE ER-8059            TO EMI-ERROR                        CL*36
05536          MOVE -1                 TO MAINTL                           CL*36
05537          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05538          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05539          GO TO 8110-SEND-DATA.                                       CL*36
05540                                                                      CL*36
05541      IF DM-RETURN-CODE = '10'                                        CL*36
05542          MOVE ER-8060            TO EMI-ERROR                        CL*36
05543          MOVE -1                 TO MAINTL                           CL*36
05544          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05545          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05546          GO TO 8110-SEND-DATA.                                       CL*36
05547                                                                      CL*36
05548      IF DM-RETURN-CODE = '11'                                        CL*36
05549          MOVE ER-8061            TO EMI-ERROR                        CL*36
05550          MOVE -1                 TO MAINTL                           CL*36
05551          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05552          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05553          GO TO 8110-SEND-DATA.                                       CL*36
05554                                                                      CL*36
05555      IF DM-RETURN-CODE = '12'                                        CL*36
05556          MOVE ER-8062            TO EMI-ERROR                        CL*36
05557          MOVE -1                 TO MAINTL                           CL*36
05558          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05559          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05560          GO TO 8110-SEND-DATA.                                       CL*36
05561                                                                      CL*36
05562      IF DM-RETURN-CODE = '13'                                        CL*36
05563          MOVE ER-8063            TO EMI-ERROR                        CL*36
05564          MOVE -1                 TO MAINTL                           CL*36
05565          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05566          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05567          GO TO 8110-SEND-DATA.                                       CL*36
05568                                                                      CL*36
05569      IF DM-RETURN-CODE = '14'                                        CL*36
05570          MOVE ER-8064            TO EMI-ERROR                        CL*36
05571          MOVE -1                 TO MAINTL                           CL*36
05572          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05573          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05574          GO TO 8110-SEND-DATA.                                       CL*36
05575                                                                      CL*36
05576      IF DM-RETURN-CODE = '15'                                        CL*36
05577          MOVE ER-8065            TO EMI-ERROR                        CL*36
05578          MOVE -1                 TO MAINTL                           CL*42
05579          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*42
05580          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*42
05581          GO TO 8110-SEND-DATA.                                       CL*42
05582                                                                      CL*42
05583      IF DM-RETURN-CODE = '16'                                        CL*42
05584          MOVE ER-8154            TO EMI-ERROR                        CL*42
05585          MOVE -1                 TO MAINTL                           CL*42
05586          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*42
05587          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*42
05588          GO TO 8110-SEND-DATA.                                       CL*42
05589                                                                      CL*42
05590      IF DM-RETURN-CODE = '17'                                        CL*42
05591          MOVE ER-8155            TO EMI-ERROR                        CL*42
05592          MOVE -1                 TO MAINTL                           CL*40
05593          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*40
05594          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*40
05595          GO TO 8110-SEND-DATA.                                       CL*40
05596                                                                      CL*40
05597      IF DM-RETURN-CODE = 'N1'                                        CL*40
05598          MOVE ER-8152            TO EMI-ERROR                        CL*40
05599          MOVE -1                 TO MAINTL                           CL*40
05600          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*40
05601          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*40
05602          GO TO 8110-SEND-DATA.                                       CL*40
05603                                                                      CL*40
05604      IF DM-RETURN-CODE = 'E1'                                        CL*40
05605          MOVE ER-8153            TO EMI-ERROR                        CL*40
05606          MOVE -1                 TO MAINTL                           CL*36
05607          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*36
05608          PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT               CL*36
05609          GO TO 8110-SEND-DATA.                                       CL*36
05610                                                                      CL*36
05611      MOVE ER-8066                TO EMI-ERROR.                       CL*36
05612      MOVE -1                     TO MAINTL.                          CL*36
05613      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*36
05614      PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT.                  CL*36
05615      GO TO 8110-SEND-DATA.                                           CL*36
05616                                                                      CL*34
05617  8000-NOTE-NOT-FOUND.                                                CL*34
05618      EXEC CICS SYNCPOINT ROLLBACK END-EXEC.                          CL*34
05619      MOVE ER-0954                TO EMI-ERROR.                       CL*34
05620      MOVE -1                     TO MAINTL.                          CL*34
05621      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*34
05622      GO TO 8110-SEND-DATA.                                           CL*34
05623                                                                      CL*34
05624  8000-EXIT.                                                          CL*34
05625      EXIT.                                                           CL*34
05626  EJECT                                                               CL*34
05627  8050-FORMAT-LAST-NAME-1ST.                                          CL*34
05628 *****************************************************************    CL*34
05629 *             M O V E   N A M E   R O U T I N E                 *    CL*34
05630 *                                                               *    CL*34
05631 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *    CL*34
05632 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *    CL*34
05633 *     FIELDS IN THE FOLLOWING WORKING-STORAGE FIELDS.           *    CL*34
05634 *                                                               *    CL*34
05635 *                  FIELD               VALUE                    *    CL*34
05636 *                  -----               -----                    *    CL*34
05637 *           W-NAME-LAST    (CL15)      SMITH                    *    CL*34
05638 *           W-NAME-FIRST   (CL15)      JOHN                     *    CL*34
05639 *           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *    CL*34
05640 *                                                               *    CL*34
05641 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK WILL CONTAIN       *    CL*34
05642 *                SMITH, JOHN ALLEN                              *    CL*34
05643 *                     OR                                        *    CL*34
05644 *                SMITH, JOHN A.                                 *    CL*34
05645 *                                                               *    CL*34
05646 *     TO USE THIS ROUTINE YOU NEED THE WORKING-STORAGE          *    CL*34
05647 *     COPYBOOK, ELCNWA.                                         *    CL*34
05648 *****************************************************************.   CL*34
05649                                                                      CL*34
05650      MOVE SPACES                 TO WS-NAME-WORK-AREA.               CL*34
05651      MOVE ZERO                   TO WS-NAME-SW.                      CL*34
05652      SET NWA-INDEX               TO +1.                              CL*34
05653                                                                      CL*34
05654      IF W-NAME-LAST   = SPACES  AND                                  CL*34
05655         W-NAME-MIDDLE = SPACES                                       CL*34
05656           MOVE +1                TO WS-NAME-SW.                      CL*34
05657                                                                      CL*34
05658      MOVE W-NAME-LAST            TO WS-NAME-WORK2.                   CL*34
05659      PERFORM 8060-MOVE-NAME THRU 8060-EXIT.                          CL*34
05660                                                                      CL*34
05661      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.                   CL*34
05662      PERFORM 8060-MOVE-NAME THRU 8060-EXIT.                          CL*34
05663                                                                      CL*34
05664      SET NWA-INDEX UP BY +1.                                         CL*34
05665                                                                      CL*34
05666      IF W-NAME-MIDDLE NOT = SPACES                                   CL*34
05667          IF W-NAME-MIDDLE-2 = SPACES                                 CL*34
05668              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)                CL*34
05669              SET NWA-INDEX UP BY +1                                  CL*34
05670              MOVE '.'            TO WS-NW (NWA-INDEX)                CL*34
05671              SET NWA-INDEX UP BY +2                                  CL*34
05672          ELSE                                                        CL*34
05673              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2                    CL*34
05674              PERFORM 8060-MOVE-NAME THRU 8060-EXIT.                  CL*34
05675                                                                      CL*34
05676  8050-EXIT.                                                          CL*34
05677      EXIT.                                                           CL*34
05678                                                                      CL*34
05679  EJECT                                                               CL*34
05680  8060-MOVE-NAME.                                                     CL*34
05681      IF WS-NAME-SW GREATER THAN +1                                   CL*34
05682          GO TO 8060-EXIT.                                            CL*34
05683                                                                      CL*34
05684      IF WS-NAME-WORK2 = SPACES                                       CL*34
05685          GO TO 8060-EXIT.                                            CL*34
05686                                                                      CL*34
05687      SET NWA-INDEX2            TO +1.                                CL*34
05688      SET NWA-INDEX3            TO +2.                                CL*34
05689                                                                      CL*34
05690  8060-MOVE-NAME-CYCLE.                                               CL*34
05691      MOVE WS-NW2 (NWA-INDEX2)  TO WS-NW (NWA-INDEX).                 CL*34
05692                                                                      CL*34
05693      IF NWA-INDEX LESS THAN +30                                      CL*34
05694          SET NWA-INDEX UP BY +1                                      CL*34
05695      ELSE                                                            CL*34
05696          ADD +2 TO  WS-NAME-SW                                       CL*34
05697          GO TO 8060-EXIT.                                            CL*34
05698                                                                      CL*34
05699      IF NWA-INDEX2 LESS THAN +20                                     CL*34
05700          SET NWA-INDEX3 UP BY +1                                     CL*34
05701          SET NWA-INDEX2 UP BY +1.                                    CL*34
05702                                                                      CL*34
05703      IF WS-NW2 (NWA-INDEX2) = SPACES  AND                            CL*34
05704         WS-NW2 (NWA-INDEX3) = SPACES                                 CL*34
05705          IF WS-NAME-SW = ZERO                                        CL*34
05706              MOVE ','            TO WS-NW (NWA-INDEX)                CL*34
05707              SET NWA-INDEX UP BY +2                                  CL*34
05708              MOVE +1             TO WS-NAME-SW                       CL*34
05709              GO TO 8060-EXIT                                         CL*34
05710          ELSE                                                        CL*34
05711              GO TO 8060-EXIT.                                        CL*34
05712                                                                      CL*34
05713      GO TO 8060-MOVE-NAME-CYCLE.                                     CL*34
05714                                                                      CL*34
05715  8060-EXIT.                                                          CL*34
05716      EXIT.                                                           CL*34
05717                                                                      CL*34
05718      EJECT                                                           CL*34
05719                                                                      CL*34
05720  8070-UNLOCK-CLAIM-MSTR.                                             CL*34
05721      EXEC CICS UNLOCK                                                CL*34
05722          DATASET   (CLMS-FILE-ID)                                    CL*34
05723      END-EXEC.                                                       CL*34
05724                                                                      CL*34
05725  8070-EXIT.                                                          CL*34
05726       EXIT.                                                          CL*34
05727                                                                      CL*34
05728      EJECT                                                           CL*34
05729  8100-SEND-MAP.                                                   EL131
05730      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.                   CL*34
05731                                                                   EL131
           move al-uanon               to accswa
05732      IF NOT PI-NO-CARRIER-SECURITY                                EL131
05733         MOVE AL-SANOF            TO CERTCARA.                     EL131
05734      IF NOT PI-NO-ACCOUNT-SECURITY                                EL131
05735         MOVE AL-SANOF            TO CERTACTA.                     EL131
05736                                                                      CL**8
05737      IF PI-PROCESSOR-ID = 'LGXX'                                     CL*38
05738         MOVE AL-UANON            TO CCNOA.                           CL*38
05739                                                                      CL*38
05740      IF PI-COMPANY-ID = 'DMD'                                        CL*37
05741         NEXT SENTENCE                                                CL*37
05742      ELSE                                                            CL*37
05743         MOVE AL-SADOF            TO PFKEY6A.                         CL*37

081817     IF NOT (PI-COMPANY-ID = 'DCC' OR 'VPP')
081817        MOVE AL-SANOF TO EXTENSA
081817     END-IF

05744                                                                      CL*37
05745      IF PI-USES-PAID-TO                                              CL**8
05746         MOVE 'PAID TO  :' TO PTHRHDGO.                               CL**8
05747                                                                   EL131
05748      EXEC CICS SEND                                               EL131
05749           MAP     ('EL131A')                                      EL131
05750           MAPSET  ('EL131S')                                      EL131
05751           ERASE                                                   EL131
05752           FREEKB                                                  EL131
05753           CURSOR                                                  EL131
05754      END-EXEC.                                                    EL131
05755                                                                   EL131
05756      GO TO 9000-RETURN-TRANS.                                     EL131
05757                                                                   EL131
05758  8110-SEND-DATA.                                                  EL131
05759      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.                   CL*34
05760                                                                   EL131
05761      IF PI-USES-PAID-TO                                              CL**8
05762         MOVE 'PAID TO  :' TO PTHRHDGO.                               CL**8
05763                                                                      CL*38
05764      IF PI-PROCESSOR-ID = 'LGXX'                                     CL*38
05765         MOVE AL-UANON            TO CCNOA.                           CL*38
05766                                                                      CL*37
05767      IF PI-COMPANY-ID = 'DMD'                                        CL*37
05768         NEXT SENTENCE                                                CL*37
05769      ELSE                                                            CL*37
05770         MOVE AL-SADOF            TO PFKEY6A.                         CL*37
05771                                                                      CL**8
05772      EXEC CICS SEND                                               EL131
05773           MAP      ('EL131A')                                     EL131
05774           MAPSET   ('EL131S')                                     EL131
05775           DATAONLY                                                EL131
05776           FREEKB                                                  EL131
05777           CURSOR                                                  EL131
05778      END-EXEC.                                                    EL131
05779                                                                   EL131
05780      GO TO 9000-RETURN-TRANS.                                     EL131
05781                                                                   EL131
05782  8120-FORMAT-TIME-DATE.                                           EL131

052113     perform 8140-check-for-prev-errors
052113                                 thru 8140-exit

05784      IF SKIP-ATTRIBUTE = 'Y'                                      EL131
05785         MOVE SPACES              TO SKIP-ATTRIBUTE                EL131
05786         MOVE ER-0598             TO EMI-ERROR                     EL131
05787         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL131
05788         GO TO 8125-SKIP-ATTRIBUTE.                                EL131
05789                                                                   EL131
032613*     IF PI-COMPANY-ID = 'DMD'                                        CL*41
032613      IF PI-PROCESSOR-ID <> 'EMER'
05791          MOVE AL-PANOF           TO CERTEFFA  CERTACTA  CERTSTA      CL*41
032613                                    CERTCARA  CERTGRPA CERTA SUFXA.
05793                                                                      CL*41
05794      MOVE AL-PANOF               TO CRTLNMEA  CRTFNMEA  CRTINITA     CL*41
05795                                     ISSAGEA   JNTLNMEA  JNTFNMEA  EL131
05796                                     JNTINITA  JNTAGEA   APRA         CL**8
05797                                     LCVCDA    LCVOTRMA  LCVRTRMA     CL**8
05798                                     LCVBENEA  LCVFORMA  LCVCNDTA     CL**8
05799                                     LCVEXITA  LCVSTATA  LCVKINDA     CL**8
05800                                     ACVCDA    ACVOTRMA  ACVRTRMA     CL**8
05801                                     ACVBENEA  ACVFORMA  ACVCNDTA     CL**8
05802                                     ACVEXITA  ACVSTATA  ACVKINDA     CL**8
05803                                     PMTFREQA  INDGRPA   PREMTYPA  EL131
05804                                     REINCDA   LCVRATEA  ACVRATEA     CL*41
05805                                     ADDONDTA.                        CL*41
05806                                                                      CL*36
121802*    IF PI-PROCESSOR-ID NOT = 'LGXX'                        
121802*       IF PI-COMPANY-ID = 'DMD'                             
121802*          MOVE AL-SANOF         TO TYPEA     CERTA     CERTSTA 
121802*                                   SUFXA                      
121802*                                   PDAMTA    NODAYSA   NOPMTSA  
121802*                                   INCA      OCCA              
121802*          IF NOT SYSTEM-MODIFY-CAP                            
121802*              MOVE AL-SANOF         TO PDTHRUA.              
05815                                                                      CL*14
05816      IF PI-COMPANY-ID NOT = 'CRI' AND 'PEM' AND 'NCL'                CL*25
05817          MOVE AL-SANOF           TO LCVRATEA  ACVRATEA.              CL*14
05818                                                                   EL131
05819  8125-SKIP-ATTRIBUTE.                                             EL131
05820                                                                      CL**8
05821      MOVE SAVE-DATE              TO DATEO.                        EL131
05822      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL*32
05823      END-EXEC                                                        CL*32
05824      EXEC CICS FORMATTIME                                            CL*32
05825                ABSTIME(LCP-CICS-TIME)                                CL*32
05826                TIME(LCP-TIME-OF-DAY-XX)                              CL*32
05827      END-EXEC                                                        CL*32
05828      MOVE  LCP-TIME-OF-DAY-68 TO TIME-IN.                            CL*32
05829      MOVE UN-HOURS               TO FOR-HOURS.                    EL131
05830      MOVE UN-MINUTES             TO FOR-MINUTES.                  EL131
05831      MOVE TIME-OUT               TO TIMEO.                        EL131
101501     MOVE PI-COMPANY-ID          TO COMPO.
101501     MOVE PI-PROCESSOR-ID        TO USERIDO.
05832      MOVE MAP-ID                 TO PI-CURRENT-SCREEN-NO.         EL131
05833      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL131
05834      MOVE EMI-MESSAGE-AREA (1)   TO MSG1O.                        EL131
052113     MOVE EMI-MESSAGE-AREA (2)   TO MSG2O.                        EL131
05835                                                                   EL131
05836  8130-EXIT.                                                       EL131
05837      EXIT.                                                        EL131
05838                                                                      CL**4
052113 8140-check-for-prev-errors.
052113
052113     MOVE PI-COMPANY-CD          TO trlr-company-cd
052113     MOVE PI-CARRIER             TO trlr-carrier
052113     MOVE PI-CLAIM-NO            TO trlr-claim-no
052113     MOVE PI-CERT-NO             TO trlr-cert-no
052113     MOVE +95                    TO TRLR-SEQ-NO    
052113
052113     EXEC CICS READ                              
052113        DATASET  ('ELTRLR')                     
052113        SET      (ADDRESS OF ACTIVITY-TRAILERS) 
052113        RIDFLD   (TRLR-KEY)                   
052113        RESP     (WS-RESPONSE)
052113     END-EXEC
052113
052113     if ws-resp-normal
052113        perform varying s1 from +1 by +1 until
052113           at-note-error-no (s1) = spaces
052113           if at-note-error-no (s1) = '1652'
052113              continue
052113           else
052113              move at-note-error-no (s1)
052113                                 to emi-error
052113              move -1 to maintl
052113              if at-note-error-no (s1) = '1653'
052113                 evaluate true
052113                    when pi-claim-type = 'L'
052113                       move '  LF  '
052113                        to emi-claim-type
052113                    when pi-claim-type = 'I'
052113                       move '  IU  '
052113                        to emi-claim-type
052614                    WHEN PI-CLAIM-TYPE = 'F'
052614                       MOVE '  FL  '
052614                        TO EMI-CLAIM-TYPE
080322                    WHEN PI-CLAIM-TYPE = 'B'
080322                       MOVE '  BR  '
080322                        TO EMI-CLAIM-TYPE
080322                    WHEN PI-CLAIM-TYPE = 'H'
080322                       MOVE '  HS  '
080322                        TO EMI-CLAIM-TYPE
100518                    WHEN PI-CLAIM-TYPE = 'O'
100518                       MOVE '  OT  '
100518                        TO EMI-CLAIM-TYPE
052113                    when other
052113                       move '  AH  '
052113                        to emi-claim-type
052113                 end-evaluate
052113              end-if
052113              PERFORM 9900-ERROR-FORMAT
052113                                 THRU 9900-EXIT
052113           end-if
052113*          if at-note-error-no (s1) = '1653'
052113*             perform varying emi-sub from 1 by 1 until
052113*                emi-sub > 3
052113*                if emi-error-number (emi-sub) = '1653'
052113*                   evaluate true
052113*                      when pi-claim-type = 'L'
052113*                         move '  LF  '
052113*                          to emi-claim-type
052113*                      when pi-claim-type = 'I'
052113*                         move '  IU  '
052113*                          to emi-claim-type
052113*                      when other
052113*                         move '  AH  '
052113*                          to emi-claim-type
052113*                   end-evaluate
052113*                end-if
052113*             end-perform
052113*          end-if
052113        end-perform
052113     else
052113        display ' resp not normal ' ws-response ' '
052113        trlr-key (2:19)
052113     end-if
052113
052113     .
052113 8140-exit.
052113     exit.

05839  8150-ENTERED-CLAIM-NOTFOUND.                                        CL**4
05840                                                                      CL**8
05841      MOVE ER-0204                TO EMI-ERROR.                       CL**4
05842      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
05843      MOVE -1                     TO MAINTL.                          CL**4
05844      GO TO 8100-SEND-MAP.                                            CL**4
05845                                                                   EL131
05846  8200-RETURN-PRIOR.                                               EL131
05847      MOVE SPACE                  TO PI-RETURN-CD-1.               EL131
05848      MOVE PI-RETURN-TO-PROGRAM   TO CALL-PGM.                     EL131
05849      GO TO 9200-XCTL.                                             EL131
05850                                                                   EL131
05851  8300-GET-HELP.                                                   EL131
05852      MOVE XCTL-EL010             TO CALL-PGM.                     EL131
05853      GO TO 9200-XCTL.                                             EL131
05854                                                                   EL131
05855  8400-RETURN-MASTER.                                              EL131
05856      MOVE SPACE                  TO PI-RETURN-CD-1.               EL131
05857      MOVE XCTL-EL126             TO CALL-PGM.                     EL131
05858      GO TO 9200-XCTL.                                             EL131
05859                                                                   EL131
05860  8500-TRLR-MNT.                                                   EL131
05861      IF PI-RETURN-CD-1 = 'X'                                      EL131
05862          MOVE ER-0311            TO EMI-ERROR                     EL131
05863          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
05864          MOVE -1                 TO MAINTL                        EL131
05865          GO TO 8110-SEND-DATA.                                    EL131
05866                                                                   EL131
05867      EXEC CICS WRITEQ TS                                          EL131
05868           QUEUE     (PI-KEY)                                      EL131
05869           FROM      (PROGRAM-INTERFACE-BLOCK)                     EL131
05870           LENGTH    (PI-COMM-LENGTH)                              EL131
05871      END-EXEC.                                                    EL131
05872                                                                   EL131
05873      MOVE XCTL-EL142             TO CALL-PGM.                     EL131
05874      GO TO 9200-XCTL.                                             EL131
05875                                                                   EL131
05876  8600-ADDR-MNT.                                                   EL131
05877      IF PI-RETURN-CD-1 = 'X'                                      EL131
05878          MOVE ER-0311            TO EMI-ERROR                     EL131
05879          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
05880          MOVE -1                 TO MAINTL                        EL131
05881          GO TO 8110-SEND-DATA.                                    EL131
05882                                                                   EL131
05883      EXEC CICS WRITEQ TS                                          EL131
05884           QUEUE     (PI-KEY)                                      EL131
05885           FROM      (PROGRAM-INTERFACE-BLOCK)                     EL131
05886           LENGTH    (PI-COMM-LENGTH)                              EL131
05887      END-EXEC.                                                    EL131
05888                                                                   EL131
05889      MOVE XCTL-EL141             TO CALL-PGM.                     EL131
05890      GO TO 9200-XCTL.                                                CL**8
05891                                                                      CL**8
05892  8700-CERT-MNT.                                                      CL**8
05893      IF PI-RETURN-CD-1 = 'X'                                         CL**8
05894          MOVE ER-7690            TO EMI-ERROR                        CL**8
05895          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*34
05896          MOVE -1                 TO MAINTL                           CL**8
05897          GO TO 8110-SEND-DATA.                                       CL**8
05898                                                                      CL**8
05899      EXEC CICS WRITEQ TS                                             CL**8
05900           QUEUE     (PI-KEY)                                         CL**8
05901           FROM      (PROGRAM-INTERFACE-BLOCK)                        CL**8
05902           LENGTH    (PI-COMM-LENGTH)                                 CL**8
05903      END-EXEC.                                                       CL**8
05904                                                                      CL**8
05905      MOVE XCTL-EL1273            TO CALL-PGM.                        CL**8
05906      GO TO 9200-XCTL.                                                CL*34
05907                                                                      CL*34
082013 8725-BENEFICIARY-MNT.
082013     IF PI-RETURN-CD-1 = 'X'
082013         MOVE ER-0311            TO EMI-ERROR
082013         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
082013         MOVE -1                 TO MAINTL
082013         GO TO 8110-SEND-DATA.
082013
082013     EXEC CICS WRITEQ TS
082013          QUEUE     (PI-KEY)
082013          FROM      (EL131AI)
082013          LENGTH    (MAP-LENGTH)
082013     END-EXEC.
082013
082013     EXEC CICS WRITEQ TS
082013          QUEUE     (PI-KEY)
082013          FROM      (PROGRAM-INTERFACE-BLOCK)
082013          LENGTH    (PI-COMM-LENGTH)
082013     END-EXEC.
082013
082013     MOVE XCTL-EL114             TO CALL-PGM.
082013     GO TO 9200-XCTL.
082013
05908  8750-DMD-CLM-FIX.                                                   CL*39
05909      EXEC CICS WRITEQ TS                                             CL*34
05910           QUEUE     (PI-KEY)                                         CL*34
05911           FROM      (PROGRAM-INTERFACE-BLOCK)                        CL*34
05912           LENGTH    (PI-COMM-LENGTH)                                 CL*34
05913      END-EXEC.                                                       CL*34
05914                                                                      CL*34
05915      MOVE XCTL-EL400DMD          TO CALL-PGM.                        CL*34
05916      GO TO 9200-XCTL.                                             EL131
05917                                                                   EL131
05918  8800-UNAUTHORIZED-ACCESS.                                        EL131
05919      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL131
05920      GO TO 8990-SEND-TEXT.                                        EL131
05921                                                                   EL131
05922  8810-PF23-ENTERED.                                               EL131
05923      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL131
05924      MOVE XCTL-EL005             TO CALL-PGM.                     EL131
05925      GO TO 9200-XCTL.                                             EL131
05926                                                                   EL131
05927  8820-XCTL-ERROR.                                                 EL131
05928      EXEC CICS HANDLE CONDITION                                   EL131
05929          PGMIDERR (8990-SEND-TEXT)                                EL131
05930      END-EXEC.                                                    EL131
05931                                                                   EL131
05932      MOVE SPACE                 TO PI-ENTRY-CD-1.                 EL131
05933      MOVE CALL-PGM              TO PI-CALLING-PROGRAM  LOGOFF-PGM EL131
05934      MOVE PGMIDERR-MSG          TO LOGOFF-FILL.                   EL131
05935      MOVE XCTL-EL005            TO CALL-PGM.                      EL131
05936      GO TO 9200-XCTL.                                             EL131
05937                                                                   EL131
05938  8990-SEND-TEXT.                                                  EL131
05939      EXEC CICS SEND TEXT                                          EL131
05940           FROM      (LOGOFF-TEXT)                                 EL131
05941           LENGTH    (LOGOFF-LENGTH)                               EL131
05942           ERASE                                                   EL131
05943           FREEKB                                                  EL131
05944      END-EXEC.                                                    EL131
05945                                                                   EL131
05946      GO TO 9100-RETURN-CICS.                                      EL131
05947                                                                   EL131
05948      EJECT                                                        EL131
05949  9000-RETURN-TRANS.                                               EL131
05950      EXEC CICS RETURN                                             EL131
05951           TRANSID   (TRANS-ID)                                    EL131
05952           COMMAREA  (PROGRAM-INTERFACE-BLOCK)                     EL131
05953           LENGTH    (PI-COMM-LENGTH)                              EL131
05954      END-EXEC.                                                    EL131
05955                                                                   EL131
05956      GOBACK.                                                      EL131
05957                                                                   EL131
05958  9100-RETURN-CICS.                                                EL131
05959      EXEC CICS RETURN                                             EL131
05960      END-EXEC.                                                    EL131
05961                                                                   EL131
05962      GOBACK.                                                      EL131
05963                                                                   EL131
05964  9200-XCTL.                                                       EL131
05965      EXEC CICS XCTL                                               EL131
05966           PROGRAM    (CALL-PGM)                                   EL131
05967           COMMAREA   (PROGRAM-INTERFACE-BLOCK)                    EL131
05968           LENGTH     (PI-COMM-LENGTH)                             EL131
05969      END-EXEC.                                                    EL131
05970                                                                   EL131
05971  9700-LINK-RTRM-FILE-ID.                                          EL131
05972      EXEC CICS LINK                                               EL131
05973          PROGRAM  (RTRM-FILE-ID)                                  EL131
05974          COMMAREA (CALCULATION-PASS-AREA)                         EL131
05975          LENGTH   (CP-COMM-LENGTH)                                EL131
05976      END-EXEC.                                                    EL131
05977                                                                   EL131
05978  9700-EXIT.                                                       EL131
05979       EXIT.                                                       EL131
05980                                                                   EL131
05981  9800-CONVERT-DATE.                                               EL131
05982      MOVE SPACES TO DC-ERROR-CODE.                                EL131
05983      EXEC CICS LINK                                               EL131
05984           PROGRAM    (DATE-CONV)                                  EL131
05985           COMMAREA   (DATE-CONVERSION-DATA)                       EL131
05986           LENGTH     (DC-COMM-LENGTH)                             EL131
05987      END-EXEC.                                                    EL131
05988                                                                   EL131
05989  9800-EXIT.                                                       EL131
05990      EXIT.                                                        EL131
05991                                                                   EL131
05992  9900-ERROR-FORMAT.                                               EL131
05993      EXEC CICS LINK                                               EL131
05994           PROGRAM    (XCTL-EL001)                                 EL131
05995           COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)              EL131
05996           LENGTH     (EMI-COMM-LENGTH)                            EL131
05997      END-EXEC.                                                    EL131
05998                                                                   EL131
05999  9900-EXIT.                                                       EL131
06000      EXIT.                                                        EL131
06001                                                                   EL131
06002  9990-ABEND.                                                      EL131
06003      MOVE DFHEIBLK               TO EMI-LINE1.                    EL131
06004      EXEC CICS LINK                                               EL131
06005           PROGRAM   (XCTL-EL004)                                  EL131
06006           COMMAREA  (EMI-LINE1)                                   EL131
06007           LENGTH    (72)                                          EL131
06008       END-EXEC.                                                   EL131
06009                                                                   EL131
06010      MOVE -1                     TO  MAINTL                       EL131
06011                                                                   EL131
06012      GO TO 8110-SEND-DATA.                                        EL131
06013                                                                   EL131
06014  9995-SECURITY-VIOLATION.                                         EL131
06015             COPY ELCSCTP.                                         EL131
06016                                                                   EL131
