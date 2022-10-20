00001  ID DIVISION.                                                     11/04/97
00002                                                                   EL154
00003  PROGRAM-ID.                 EL154.                                  LV022
00004 *              PROGRAM CONVERTED BY                                  CL*20
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*20
00006 *              CONVERSION DATE 11/21/94 15:39:26.                    CL*20
00007 *                            VMOD=2.022                              CL*22
00008 *                                                                 EL154
00008 *                                                                 EL154
00009 *AUTHOR.     LOGIC,INC.                                              CL*20
00010 *            DALLAS, TEXAS.                                          CL*20
00011                                                                   EL154
00012 *DATE-COMPILED.                                                      CL*20
00013 *SECURITY.   *****************************************************   CL*20
00014 *            *                                                   *   CL*20
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*20
00016 *            *                                                   *   CL*20
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*20
00018 *                                                                *   CL*20
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*20
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*20
00021 *            *                                                   *   CL*20
00022 *            *****************************************************   CL*20
00023                                                                   EL154
00024 *REMARKS.    TRANSACTION - EX28 - SET-UP AUTOMATIC PAYMENTS.         CL**5
00025 *        USED TO CREATE NEW AUTOMATIC PAYMENT SCHEDULES AND/OR       CL**5
00026 *        SIGN-OFF CURRENT SCHEDULES.  ONCE A SCHEDULE HAS BEEN       CL**5
00027 *        CREATED IT MAY ONLY BY REPLACED - NOT CHANGED               CL**5
00028 *        OR DELETED.                                                 CL**5
090108******************************************************************
090108*                   C H A N G E   L O G
090108*
090108* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
090108*-----------------------------------------------------------------
090108*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
090108* EFFECTIVE    NUMBER
090108*-----------------------------------------------------------------
090108* 090108    2007041300006  AJRA  ENTER LETTER FORM NUMBER ON SET UP
031809* 031809    2009031600001  AJRA  ALLOW IU PAYMENT PAST EXP DATE
070909* 070909    2009060400001  AJRA  ADD AUTO PAY STOP LETTER
061013* 061013    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
100713* 100713  IR2013092400001  PEMA  USE EXP DT IF NO CRIT PER - DCC
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100314* 100314  CR2014061900001  PEMA  Add pct of benefit funcionality
030515* 030515  IR2015030300002  PEMA  FIX CRIT PERIOD EDITS
010816* 010816  IR2015092900001  PEMA  USE CLP STATE WHERE NEEDED
031417* 031417  IR2017030200001  TANA  FIX DCC CRIT PERIOD EDITS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
101718* 101718  IR2018100400002  TANA  FIX HOLD & PAY W/ AUTO PAY
100219* 100219  IR2019092600002  PEMA  FIX PROB WITH CRIT PER EDIT
080322* 080322  CR2021100800003  TANA  Add B and H claim types
090108******************************************************************
00029                                                                   EL154
00030  ENVIRONMENT DIVISION.                                            EL154
00031                                                                   EL154
00032      EJECT                                                        EL154
00033  DATA DIVISION.                                                   EL154
00034  WORKING-STORAGE SECTION.                                         EL154
00035  77  FILLER  PIC X(32)  VALUE '********************************'. EL154
00036  77  FILLER  PIC X(32)  VALUE '*    EL154 WORKING STORAGE     *'. EL154
00037  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.022 *********'.    CL*22
061013 77  P1                          PIC S999 COMP-3 VALUE +0.
061013 77  P2                          PIC S999 COMP-3 VALUE +0.
061013 77  s1                          PIC S999 COMP-3 VALUE +0.
061013 77  s2                          PIC S999 COMP-3 VALUE +0.
061013 77  ws-benefit-expiration-dt    pic xx value low-values.
100314 77  ws-work-ben-pct             pic s9v999 comp-3 value +0.
030515 77  ws-proposed-benefits        pic s999 comp-3 value +0.
00038                                                                   EL154
00039                              COPY ELCSCTM.                           CL*15
00040                                                                   EL154
00041                              COPY ELCSCRTY.                          CL*15
00042                                                                   EL154
00043  01  WS-DATE-AREA.                                                EL154
00044      05  CURRENT-GREG-DATE   PIC X(8)    VALUE SPACES.               CL*13
00045      05  CURRENT-BIN-DATE    PIC X(2)    VALUE SPACES.               CL*13
00046      05  RETRO-ELIM-DATE     PIC XX      VALUE LOW-VALUES.           CL*13
00047                                                                   EL154
061013 01  ws-dcc-product-code         pic xxx value spaces.
010816 01  ws-clp-state                pic xx  value spaces.
061013 01  ws-ah-benefit-cd            pic xx  value '00'.
       01  ws-edit-1st-pmt-amt         pic s9(9)v99 comp-3 value +0.
061013 01  ws-monthly-benefit          pic s9(9)v99 comp-3 value +0.
061013 01  ws-max-benefit-amt          pic s9(9)v99 comp-3 value +0.
061013 01  ws-max-benefit-pmts         pic s999 comp-3 value +0.
061013 01  ws-benefits-prev-paid       pic s999 comp-3 value +0.
030515 01  ws-total-paid               pic s9(9)v99 comp-3 value +0.
061013 01  WS-PDEF-RECORD-SW           PIC X  VALUE ' '.
061013     88  PDEF-FOUND                   VALUE 'Y'.
061013 01  WS-ACCT-RECORD-SW           PIC X  VALUE ' '.
061013     88  ACCT-FOUND                   VALUE 'Y'.
061013 01  ws-cert-trlr-sw             PIC X  VALUE ' '.
061013     88  cert-trlr-found              VALUE 'Y'.
00048  01  STANDARD-AREAS.                                              EL154
061013     12  WS-RESPONSE             PIC S9(8)   COMP.
061013         88  WS-RESP-NORMAL        VALUE +00.
061013         88  WS-RESP-ERROR         VALUE +01.
061013         88  WS-RESP-NOTFND        VALUE +13.
061013         88  WS-RESP-DUPREC        VALUE +14.
061013         88  WS-RESP-ENDFILE       VALUE +20.

00049      12  SC-ITEM             PIC S9(4)   VALUE +1  COMP.          EL154
00050      12  SUB-1               PIC S9(4)   VALUE +0  COMP.             CL*13
00051      12  GETMAIN-SPACE       PIC X       VALUE SPACE.             EL154
00052      12  MAP-NAME.                                                EL154
00053          16  MAP-PRE         PIC XX      VALUE 'EL'.              EL154
00054          16  MAP-NUMBER      PIC X(4)    VALUE '154A'.            EL154
00055          16  FILLER          PIC XX      VALUE SPACES.            EL154
00056      12  MAPSET-NAME         PIC X(8)    VALUE 'EL154S'.          EL154
00057      12  TRANS-ID            PIC X(4)    VALUE 'EX28'.            EL154
00058      12  LGXX-ID             PIC X(4)    VALUE 'LGXX'.            EL154
00059      12  PGM-NAME            PIC X(8).                            EL154
00060      12  TIME-IN             PIC S9(7).                           EL154
00061      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL154
00062          16  FILLER          PIC X.                               EL154
00063          16  TIME-OUT        PIC 99V99.                           EL154
00064          16  FILLER          PIC X(2).                            EL154
00065      12  XCTL-005            PIC X(8)    VALUE 'EL005'.           EL154
00066      12  XCTL-010            PIC X(8)    VALUE 'EL010'.           EL154
00067      12  XCTL-126            PIC X(8)    VALUE 'EL126'.           EL154
00068                                                                      CL*17
00069      12  LINK-001            PIC X(8)    VALUE 'EL001'.           EL154
00070      12  LINK-004            PIC X(8)    VALUE 'EL004'.           EL154
00071      12  LINK-1523           PIC X(8)    VALUE 'EL1523'.             CL*17
00072      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.         EL154
00073      12  THIS-PGM            PIC X(8)    VALUE 'EL154'.           EL154
00074                                                                   EL154
00075      12  DEEDIT-FIELD            PIC X(15).                       EL154
00076      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD   PIC S9(15).    EL154
00077                                                                   EL154
00078      12  TRLR-LENGTH             PIC S9(4)   COMP VALUE +200.        CL**5
00079      12  NEFFDTE-SAVE            PIC XX       VALUE LOW-VALUES.   EL154
00080      12  NFSTPMT-SAVE            PIC XX       VALUE LOW-VALUES.   EL154
00081      12  NLSTPMT-SAVE            PIC XX       VALUE LOW-VALUES.   EL154
00082      12  NFSTAMT-SAVE            PIC S9(7)V99 VALUE +0   COMP-3.     CL*21
00083      12  NREGPMT-SAVE            PIC S9(7)V99 VALUE +0   COMP-3.     CL*21
00084      12  CURRENT-SAVE            PIC XX       VALUE LOW-VALUES.   EL154
090108     12  LETTDTE-SAVE            PIC X(10)    VALUE SPACES.
00085      12  WS-WORK                 PIC S9(5)    VALUE ZERO COMP-3.  EL154
00086      12  WS-REMAINDER            PIC S9(5)    VALUE ZERO COMP-3.  EL154
00087      12  WS-DAYS-MINUS-TOL       PIC S9(5)    VALUE ZERO COMP-3.  EL154
00088      12  WS-DAYS-PLUS-TOL        PIC S9(5)    VALUE ZERO COMP-3.  EL154
00089      12  WS-DAILY-BEN-AMT        PIC S9(5)V99 VALUE ZERO COMP-3.  EL154
00090      12  WS-AMT-MINUS-TOL        PIC S9(7)V99 VALUE ZERO COMP-3.  EL154
00091      12  WS-AMT-PLUS-TOL         PIC S9(7)V99 VALUE ZERO COMP-3.  EL154
00092      12  WS-AMT-TOL              PIC S9(5)V99 VALUE ZERO COMP-3.     CL**5
00093      12  WS-DAY-TOL              PIC S9(05)   VALUE ZERO COMP-3.  EL154
00094      12  WS-MAX-AUTO-PMT         PIC S9(7)V99 VALUE ZERO COMP-3.  EL154
00095      12  WS-MAX-AUTO-MOS         PIC S999     VALUE ZERO COMP-3.  EL154
00096      12  WS-ELAPSED-DAYS         PIC S9(04)   VALUE +0   COMP.       CL**5
00097      12  WS-REG-PMT              PIC S9(7)V99 VALUE ZERO COMP-3.  EL154
00098      12  WS-EXP-DT               PIC XX       VALUE LOW-VALUES.   EL154
031809     12  WS-EXP-DT-PLUS7         PIC XX       VALUE LOW-VALUES.   EL154
00099      12  WS-CALC-METHOD          PIC X.                           EL154
00100      12  WS-DAILY-RATE           PIC S9(3)V99.                    EL154
00101      12  WS-CONFIRM-NEEDED       PIC X.                              CL**7
00102      12  WS-ELIMINATION-SW       PIC X        VALUE SPACES.          CL*14
00103          88  NO-EFFDTE-ENTERED   VALUE 'N'.                          CL*14
00104      12  WS-BENEFIT-FOUND-SW     PIC X        VALUE SPACES.          CL*13
00105          88  BENEFIT-FOUND       VALUE 'Y'.                          CL*13
00106      12  WS-ACT-REC-FOUND-SW     PIC X(01)    VALUE 'N'.             CL*17
00107      12  WS-LETTER-SW            PIC X(01)    VALUE 'N'.             CL*17
090108     12  WS-LETTER-FORM-NUM      PIC X(04)    VALUE SPACES.
070909     12  WS-END-LETTER-FORM-NUM  PIC X(04)    VALUE SPACES.
00108      12  WS-RESET-SW             PIC X(01)    VALUE 'N'.             CL*17
00109      12  WS-PAYEE-CODE.                                              CL**5
00110          16  WS-PAYEE-CD         PIC X(01).                          CL**5
00111          16  WS-PAYEE-SEQ        PIC X(01).                          CL**5
00112          16  WS-PAYEE-SEQ-NUM REDEFINES                              CL**5
00113              WS-PAYEE-SEQ        PIC 9(01).                          CL**5
00114      12  WS-ACCESS.                                                  CL*13
00115          16  FILLER              PIC XX       VALUE SPACES.          CL*13
00116          16  WS-BEN-CD           PIC XX.                             CL*13
101718     12  WS-BROWSE-TRLR-SW   PIC X VALUE ' '.
101718     12  WS-HOLD-UNTIL-DT        PIC X(02) VALUE LOW-VALUES.
00117                                                                      CL*10
00118      12  MISC-HEADING-AREAS.                                         CL*10
00119          16  FIRST-PMT-HD1       PIC X(23)    VALUE                  CL*10
00120              'FIRST PAY THRU DATE   :'.                              CL*10
00121          16  FIRST-PMT-HD2       PIC X(23)    VALUE                  CL*10
00122              'FIRST PAY-TO DATE     :'.                              CL*20
00123          16  FIRST-PMT-HD3       PIC X(26)    VALUE                  CL*10
00124              '   FIRST PAY-THRU DATE   :'.                           CL*20
00125          16  FIRST-PMT-HD4       PIC X(26)    VALUE                  CL*10
00126              '   FIRST PAY-TO DATE     :'.                           CL*20
00127          16  LAST-PMT-HD1        PIC X(24)    VALUE                  CL*10
00128              'LAST PAY-THRU DATE     :'.                             CL*20
00129          16  LAST-PMT-HD2        PIC X(24)    VALUE                  CL*10
00130              'LAST PAY-TO DATE       :'.                             CL*20
00131          16  LAST-PMT-HD3        PIC X(23)    VALUE                  CL*10
00132              'LAST PAY-THRU DATE    :'.                              CL*20
00133          16  LAST-PMT-HD4        PIC X(23)    VALUE                  CL*10
00134              'LAST PAY-TO DATE      :'.                              CL*20
00135      EJECT                                                        EL154
00136  01  ERROR-MESSAGES.                                              EL154
00137      12  ER-0000                 PIC X(4)     VALUE '0000'.       EL154
00138      12  ER-0004                 PIC X(4)     VALUE '0004'.       EL154
00139      12  ER-0008                 PIC X(4)     VALUE '0008'.       EL154
00140      12  ER-0029                 PIC X(4)     VALUE '0029'.       EL154
00141      12  ER-0042                 PIC X(4)     VALUE '0042'.       EL154
00142      12  ER-0050                 PIC X(4)     VALUE '0050'.       EL154
00143      12  ER-0070                 PIC X(4)     VALUE '0070'.       EL154
00144      12  ER-0130                 PIC X(4)     VALUE '0130'.          CL**5
00145      12  ER-0154                 PIC X(4)     VALUE '0154'.       EL154
00146      12  ER-0169                 PIC X(4)     VALUE '0169'.       EL154
00147      12  ER-0172                 PIC X(4)     VALUE '0172'.       EL154
00148      12  ER-0186                 PIC X(4)     VALUE '0186'.       EL154
00149      12  ER-0206                 PIC X(4)     VALUE '0206'.       EL154
00150      12  ER-0283                 PIC X(4)     VALUE '0283'.          CL*13
00151      12  ER-0346                 PIC X(4)     VALUE '0346'.       EL154
00152      12  ER-0347                 PIC X(4)     VALUE '0347'.       EL154
00153      12  ER-0348                 PIC X(4)     VALUE '0348'.       EL154
00154      12  ER-0349                 PIC X(4)     VALUE '0349'.       EL154
00155      12  ER-0350                 PIC X(4)     VALUE '0350'.       EL154
00156      12  ER-0351                 PIC X(4)     VALUE '0351'.       EL154
00157      12  ER-0352                 PIC X(4)     VALUE '0352'.       EL154
00158      12  ER-0353                 PIC X(4)     VALUE '0353'.       EL154
00159      12  ER-0354                 PIC X(4)     VALUE '0354'.       EL154
00160      12  ER-0357                 PIC X(4)     VALUE '0357'.       EL154
00161      12  ER-0358                 PIC X(4)     VALUE '0358'.       EL154
00162      12  ER-0372                 PIC X(4)     VALUE '0372'.       EL154
00163      12  ER-0396                 PIC X(4)     VALUE '0396'.       EL154
00164      12  ER-0397                 PIC X(4)     VALUE '0397'.       EL154
00165      12  ER-0398                 PIC X(4)     VALUE '0398'.       EL154
00166      12  ER-0399                 PIC X(4)     VALUE '0399'.       EL154
00167      12  ER-0401                 PIC X(4)     VALUE '0401'.       EL154
00168      12  ER-0402                 PIC X(4)     VALUE '0402'.       EL154
00169      12  ER-0403                 PIC X(4)     VALUE '0403'.       EL154
00170      12  ER-0404                 PIC X(4)     VALUE '0404'.       EL154
00171      12  ER-0450                 PIC X(4)     VALUE '0450'.       EL154
00172      12  ER-0541                 PIC X(4)     VALUE '0541'.          CL*13
00173      12  ER-0559                 PIC X(4)     VALUE '0559'.       EL154
00174      12  ER-0563                 PIC X(4)     VALUE '0563'.       EL154
00175      12  ER-0597                 PIC X(4)     VALUE '0597'.       EL154
00176      12  ER-0632                 PIC X(4)     VALUE '0632'.          CL**5
00177      12  ER-0670                 PIC X(4)     VALUE '0670'.          CL**7
00178      12  ER-0802                 PIC X(4)     VALUE '0802'.          CL*17
00179      12  ER-0803                 PIC X(4)     VALUE '0803'.          CL*17
00180      12  ER-0815                 PIC X(4)     VALUE '0815'.          CL*17
00181      12  ER-0826                 PIC X(4)     VALUE '0826'.          CL*17
00182      12  ER-0839                 PIC X(4)     VALUE '0839'.          CL*18
00183      12  ER-1599                 PIC X(4)     VALUE '1599'.       EL154
061013     12  er-1657                 pic x(4)     value '1657'.
061013     12  er-1658                 pic x(4)     value '1658'.
           12  er-1672                 pic x(4)     value '1672'.
           12  er-1674                 pic x(4)     value '1674'.
101718     12  ER-1931                 PIC X(4)     VALUE '1931'.
031809     12  ER-3396                 PIC X(4)     VALUE '3396'.
070909     12  ER-3397                 PIC X(4)     VALUE '3397'.
00184      12  ER-3537                 PIC X(4)     VALUE '3537'.          CL*17
090108     12  ER-3801                 PIC X(4)     VALUE '3801'.
00185      12  ER-8200                 PIC X(4)     VALUE '8200'.          CL*21
00186      12  ER-9483                 PIC X(4)     VALUE '9483'.          CL*18
00187      12  ER-9883                 PIC X(4)     VALUE '9883'.          CL*18
00188                                                                   EL154
00189      EJECT                                                        EL154
00190  01  ACCESS-KEYS.                                                 EL154
00191      12  CNTL-ID                  PIC X(8)    VALUE 'ELCNTL'.     EL154
00192      12  CLAM-ID                  PIC X(8)    VALUE 'ELMSTR'.     EL154
00193      12  ACTV-ID                  PIC X(8)    VALUE 'ELTRLR'.     EL154
00194      12  CERT-ID                  PIC X(8)    VALUE 'ELCERT'.     EL154
00195      12  BENE-ID                  PIC X(8)    VALUE 'ELBENE'.     EL154
00196      12  EMPLCY-ID                PIC X(8)    VALUE 'MPPLCY'.        CL*18
00197                                                                   EL154
00198      12  CNTL-KEY.                                                EL154
00199          16  CNTL-CO              PIC X(3).                       EL154
00200          16  CNTL-RECORD-TYPE     PIC X       VALUE '1'.          EL154
00201          16  CNTL-GENL.                                           EL154
00202            18 CNTL-GEN1           PIC X(2)    VALUE SPACES.       EL154
00203            18 CNTL-GEN2.                                          EL154
00204              20 CNTL-GEN3         PIC X       VALUE SPACES.       EL154
00205              20 CNTL-GEN4         PIC X       VALUE SPACES.       EL154
00206          16  CNTL-SEQ             PIC S9(4)   VALUE +0    COMP.   EL154
00207                                                                   EL154
061013     12  ELTRLR-KEY.
061013         16  TRLR-COMP-CD        PIC X.
061013         16  TRLR-CARRIER        PIC X.
061013         16  TRLR-CLAIM-NO       PIC X(7).
061013         16  TRLR-CERT-NO        PIC X(11).
061013         16  TRLR-SEQ-NO         PIC S9(4)   COMP.

00208      12  CLAM-KEY.                                                EL154
00209          16  CLAM-CO              PIC X.                          EL154
00210          16  CLAM-CARRIER         PIC X.                          EL154
00211          16  CLAM-CLAIM           PIC X(7).                       EL154
00212          16  CLAM-CERT-NUM        PIC X(11).                      EL154
00213                                                                   EL154
00214      12  CLAIM-SAVE-KEY           PIC X(20).                         CL**5
00215                                                                      CL**5
00216      12  ACTV-KEY.                                                EL154
00217          16  ACTV-PARTIAL-KEY.                                    EL154
00218              20  ACTV-CO          PIC X.                          EL154
00219              20  ACTV-CARRIER     PIC X.                          EL154
00220              20  ACTV-CLAIM       PIC X(7).                       EL154
00221              20  ACTV-CERT-NUM    PIC X(11).                      EL154
00222          16  ACTV-SEQ             PIC S9(4)  COMP.                EL154
00223                                                                   EL154
00224      12  CERT-KEY.                                                EL154
00225          16  CERT-CO             PIC X.                           EL154
00226          16  CERT-CARRIER        PIC X.                           EL154
00227          16  CERT-GROUPING       PIC X(6).                        EL154
00228          16  CERT-STATE          PIC XX.                          EL154
00229          16  CERT-ACCOUNT        PIC X(10).                       EL154
00230          16  CERT-EFF-DT         PIC XX.                          EL154
00231          16  CERT-CERT-NUM       PIC X(11).                       EL154
00232                                                                      CL*18
061013     12  WS-ERACCT-SAVE-KEY      PIC X(20).
061013     12  ERACCT-KEY.
061013         16  ERACCT-PARTIAL-KEY.
061013             20  ACCT-COMP-CD    PIC X.
061013             20  ACCT-CARRIER    PIC X.
061013             20  ACCT-GROUPING   PIC X(6).
061013             20  ACCT-STATE      PIC XX.
061013             20  ACCT-ACCOUNT    PIC X(10).
061013         16  ACCT-EXP-DT         PIC XX.
061013         16  FILLER              PIC X(4) VALUE SPACES.
00233      12  EMPLCY-KEY.                                                 CL*18
00234          16  EMPLCY-COMPANY-CD   PIC X(01).                          CL*18
00235          16  EMPLCY-CARRIER      PIC X(01).                          CL*18
00236          16  EMPLCY-GROUPING     PIC X(06).                          CL*18
00237          16  EMPLCY-STATE        PIC X(02).                          CL*18
00238          16  EMPLCY-PRODUCER     PIC X(10).                          CL*18
00239          16  EMPLCY-EFF-DT       PIC X(02).                          CL*18
00240          16  EMPLCY-REFERENCE-NO PIC X(20).                          CL*18
00241                                                                   EL154
00242      12  BENE-KEY.                                                EL154
00243          16  BENE-CO             PIC X(01).                          CL**5
00244          16  BENE-REC-TYPE       PIC X(01).                          CL**5
00245          16  BENE-CODE           PIC X(10).                          CL**5

061013 01  ERPDEF-KEY-SAVE             PIC X(18).
061013 01  ERPDEF-KEY.
061013     12  ERPDEF-COMPANY-CD       PIC X.
061013     12  ERPDEF-STATE            PIC XX.
061013     12  ERPDEF-PROD-CD          PIC XXX.
061013     12  F                       PIC X(7).
061013     12  ERPDEF-BEN-TYPE         PIC X.
061013     12  ERPDEF-BEN-CODE         PIC XX.
061013     12  ERPDEF-EXP-DT           PIC XX.
061013
061013 01  ELCRTT-KEY.                                              
061013     05  CTRLR-COMP-CD       PIC X.                               
061013     05  CTRLR-CARRIER       PIC X.                               
061013     05  CTRLR-GROUPING      PIC X(6).                            
061013     05  CTRLR-STATE         PIC X(2).                            
061013     05  CTRLR-ACCOUNT       PIC X(10).
061013     05  CTRLR-EFF-DT        PIC XX.                              
061013     05  CTRLR-CERT-NO       PIC X(11).  
061013     05  CTRLR-REC-TYPE      PIC X.

00247                              COPY ELCCALC.                           CL*15
00248                                                                      CL**5
00249                                                                      CL**5
00250      EJECT                                                        EL154
00251                              COPY ELCDATE.                           CL*15
00252                                                                   EL154
00253      EJECT                                                        EL154
00254                              COPY ELCLOGOF.                          CL*15
00255                                                                   EL154
00256      EJECT                                                        EL154
00257                              COPY ELCATTR.                           CL*15
00258                                                                   EL154
00259      EJECT                                                        EL154
00260                              COPY ELCEMIB.                           CL*15
00261      EJECT                                                           CL*17
00262                              COPY ELCLNKLT.                          CL*17
090108
090108     12  W-1523-LETTER-DATE       PIC X(10).
00263      EJECT                                                        EL154
00264                              COPY ELCINTF.                           CL*15
00265      12  PI-REDEF       REDEFINES PI-PROGRAM-WORK-AREA.              CL*13
00266          16  PI-BENEFIT-SAVE.                                        CL*13
00267              18  PI-BEN-DAYS      PIC 99.                            CL*13
00268              18  PI-BEN-TYPE      PIC X.                             CL*13
070909*00269   16  FILLER               PIC X(637).                        CL*20
070909         16  PI-AUTO-END-LETTER   PIC X(04).
070909         16  FILLER               PIC X(633).
00270                                                                   EL154
00271      EJECT                                                        EL154
00272                              COPY ELCAID.                            CL*15
00273  01  FILLER    REDEFINES DFHAID.                                  EL154
00274      12  FILLER              PIC X(8).                            EL154
00275      12  PF-VALUES           PIC X       OCCURS 2.                EL154
00276                                                                   EL154
00277      EJECT                                                        EL154
00278                              COPY EL154S.                            CL*15
00279      EJECT                                                        EL154
00280  LINKAGE SECTION.                                                 EL154
00281  01  DFHCOMMAREA             PIC X(1024).                         EL154
00282                                                                   EL154
00283      EJECT                                                        EL154
00284                              COPY ELCCNTL.                           CL*15
00285      EJECT                                                        EL154
00286                              COPY ELCMSTR.                           CL*15
00287      EJECT                                                        EL154
00288                              COPY ELCTRLR.                           CL*15
00289      EJECT                                                        EL154
00290                              COPY ELCCERT.                           CL*15
00291      EJECT                                                        EL154
00292                              COPY ELCBENE.
061013                             COPY ERCACCT.
061013                             COPY ERCPDEF.
061013                             copy ELCCRTT.
00293      EJECT                                                           CL*18
00294                              COPY MPCPLCY.                           CL*18
00295      EJECT                                                        EL154
00296  PROCEDURE DIVISION.                                              EL154
00297      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL154
00298      MOVE '5'                   TO DC-OPTION-CODE.                EL154
00299      PERFORM 9700-DATE-LINK.                                      EL154
00300      MOVE DC-GREG-DATE-1-EDIT   TO  CURRENT-GREG-DATE.               CL*13
00301      MOVE DC-BIN-DATE-1         TO  CURRENT-BIN-DATE.                CL*13
00302                                                                   EL154
00303                                                                   EL154
00304      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.                 EL154
00305      MOVE 2 TO EMI-NUMBER-OF-LINES.                               EL154
00306      IF EIBCALEN = 0                                              EL154
00307          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL154
00308                                                                   EL154
00309      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL154
00310          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL154
00311              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL154
00312              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL154
00313              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL154
00314              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL154
00315              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL154
00316              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL154
00317              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL154
00318              MOVE THIS-PGM  TO PI-CALLING-PROGRAM                 EL154
00319              MOVE LOW-VALUES TO EL154AO                           EL154
00320              GO TO 7000-BUILD-SCREEN.                             EL154
00321                                                                   EL154
00322      EXEC CICS HANDLE CONDITION                                   EL154
00323          NOTOPEN(8860-CLAM-NOT-OPEN)                              EL154
00324          PGMIDERR(9600-PGMID-ERROR)                               EL154
00325          ERROR(9990-ABEND)                                        EL154
00326          END-EXEC.                                                EL154
00327                                                                   EL154
00328      IF EIBAID = DFHCLEAR                                         EL154
00329          GO TO 9400-CLEAR.                                        EL154
00330                                                                   EL154
00331      IF PI-PROCESSOR-ID = 'LGXX'                                  EL154
00332          GO TO 0200-RECEIVE.                                      EL154
00333                                                                   EL154
00334      EXEC CICS READQ TS                                           EL154
00335          QUEUE   (PI-SECURITY-TEMP-STORE-ID)                      EL154
00336          INTO    (SECURITY-CONTROL)                               EL154
00337          LENGTH  (SC-COMM-LENGTH)                                 EL154
00338          ITEM    (SC-ITEM)                                        EL154
00339      END-EXEC.                                                    EL154
00340                                                                   EL154
00341      MOVE SC-CLAIMS-DISPLAY (9)   TO  PI-DISPLAY-CAP.             EL154
00342      MOVE SC-CLAIMS-UPDATE  (9)   TO  PI-MODIFY-CAP.              EL154
00343                                                                   EL154
00344      IF NOT MODIFY-CAP                                            EL154
00345          MOVE 'UPDATE'            TO  SM-READ                     EL154
00346          PERFORM 9995-SECURITY-VIOLATION                          EL154
00347          MOVE ER-0070             TO  EMI-ERROR                   EL154
00348          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL154
00349          GO TO 8100-SEND-INITIAL-MAP.                             EL154
00350                                                                   EL154
00351      EJECT                                                        EL154
00352  0200-RECEIVE.                                                    EL154
00353      MOVE LOW-VALUES TO EL154AI.                                  EL154
00354      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL154
00355          MOVE ER-0008 TO EMI-ERROR                                EL154
00356          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL154
00357          GO TO 8200-SEND-DATAONLY.                                EL154
00358                                                                   EL154
00359      EXEC CICS RECEIVE                                            EL154
00360          MAP(MAP-NAME)                                            EL154
00361          MAPSET(MAPSET-NAME)                                      EL154
00362          INTO(EL154AI)                                            EL154
00363          END-EXEC.                                                EL154
00364                                                                   EL154
00365      IF ENTERPFL = 0                                              EL154
00366          GO TO 0300-CHECK-PFKEYS.                                 EL154
00367      IF EIBAID NOT = DFHENTER                                     EL154
00368          MOVE ER-0004 TO EMI-ERROR                                EL154
00369          GO TO 0320-INPUT-ERROR.                                  EL154
00370                                                                   EL154
00371      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)      CL**8
00372          MOVE PF-VALUES (ENTERPFI) TO EIBAID                      EL154
00373      ELSE                                                         EL154
00374          MOVE ER-0029 TO EMI-ERROR                                EL154
00375          GO TO 0320-INPUT-ERROR.                                  EL154
00376                                                                   EL154
00377  0300-CHECK-PFKEYS.                                               EL154
00378                                                                      CL**5
00379      IF EIBAID = DFHPF23                                          EL154
00380          GO TO 8810-PF23.                                         EL154
00381      IF EIBAID = DFHPF24                                          EL154
00382          GO TO 9200-RETURN-MAIN-MENU.                             EL154
00383      IF EIBAID = DFHPF12                                          EL154
00384          GO TO 9500-PF12.                                         EL154
00385      IF EIBAID EQUAL DFHPF1                                          CL**5
00386         GO TO 0400-PAGE-FORWARD.                                     CL**5
00387      IF EIBAID EQUAL DFHPF2                                          CL**5
00388         GO TO 0700-PAGE-BACKWARD.                                    CL**5
00389      IF EIBAID EQUAL DFHPF3                                          CL**7
00390         MOVE CURRENT-BIN-DATE TO CURRENT-SAVE                        CL*13
00391         IF TERMI = 'A'                                               CL**7
00392             GO TO 2000-ADD-ONLY                                      CL**7
00393         ELSE                                                         CL**7
00394         IF TERMI = 'N'                                               CL**7
00395             GO TO 3000-TERMINATE-AND-GENERATE.                       CL**7
00396      IF (TERML NOT = 0) AND (EIBAID NOT = DFHENTER)               EL154
00397          MOVE ER-0050 TO EMI-ERROR                                EL154
00398          GO TO 0320-INPUT-ERROR.                                  EL154
00399      IF EIBAID = DFHENTER                                         EL154
00400          GO TO 0330-EDIT-DATA.                                    EL154
00401                                                                   EL154
00402      MOVE ER-0029 TO EMI-ERROR.                                   EL154
00403  0320-INPUT-ERROR.                                                EL154
00404      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL154
00405      MOVE AL-UNBON TO ENTERPFA.                                   EL154
00406      IF ENTERPFL = 0                                              EL154
00407          MOVE -1 TO TERML                                         EL154
00408      ELSE                                                         EL154
00409          MOVE -1 TO ENTERPFL.                                     EL154
00410                                                                   EL154
00411      GO TO 8200-SEND-DATAONLY.                                    EL154
00412                                                                   EL154
00413      EJECT                                                        EL154
00414  0330-EDIT-DATA.                                                  EL154
00415      IF NOT MODIFY-CAP                                            EL154
00416            MOVE ER-0070          TO EMI-ERROR                     EL154
00417            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL154
00418            MOVE LOW-VALUES       TO EL154AO                       EL154
00419            GO TO 8100-SEND-INITIAL-MAP.                           EL154
00420                                                                   EL154
00421      MOVE CURRENT-BIN-DATE       TO CURRENT-SAVE                     CL*13
00422                                                                   EL154
00423      IF TERMI = 'Y'                                               EL154
00424          GO TO 1000-DELETE-CURRENT.                               EL154
00425      IF TERMI = 'A'                                               EL154
00426          GO TO 2000-ADD-ONLY.                                     EL154
00427      IF TERMI = 'N'                                               EL154
00428          GO TO 3000-TERMINATE-AND-GENERATE.                       EL154
00429      MOVE ER-0347                TO EMI-ERROR.                    EL154
00430      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL154
00431      MOVE -1                     TO TERML.                        EL154
00432      MOVE AL-UABON               TO TERMA.                        EL154
00433      GO TO 8200-SEND-DATAONLY.                                    EL154
00434                                                                   EL154
00435      EJECT                                                        EL154
00436  0400-PAGE-FORWARD.                                                  CL**5
00437      EXEC CICS HANDLE CONDITION                                      CL**5
00438           NOTFND    (8850-CLAM-END-FILE)                             CL**5
00439           ENDFILE   (8850-CLAM-END-FILE)                             CL**5
00440      END-EXEC.                                                       CL**5
00441                                                                      CL**5
00442      MOVE PI-CARRIER      TO CLAM-CARRIER                            CL**5
00443      MOVE PI-COMPANY-CD   TO CLAM-CO                                 CL**5
00444      MOVE PI-CLAIM-NO     TO CLAM-CLAIM                              CL**5
00445      MOVE PI-CERT-NO      TO CLAM-CERT-NUM                           CL**5
00446      MOVE CLAM-KEY        TO CLAIM-SAVE-KEY.                         CL**5
00447                                                                      CL**5
00448      EXEC CICS STARTBR                                               CL**5
00449           DATASET   ('ELMSTR')                                       CL**5
00450           RIDFLD    (CLAM-KEY)                                       CL**5
00451      END-EXEC.                                                       CL**5
00452                                                                      CL**5
00453  0450-READ-NEXT-CLAIM.                                               CL**5
00454                                                                      CL**5
00455      EXEC CICS READNEXT                                              CL**5
00456           DATASET   ('ELMSTR')                                       CL**5
00457           RIDFLD    (CLAM-KEY)                                       CL**5
00458           SET       (ADDRESS OF CLAIM-MASTER)                        CL*20
00459      END-EXEC.                                                       CL**5
00460                                                                      CL**5
00461      IF PI-COMPANY-CD NOT EQUAL CLAM-CO                              CL**5
00462         GO TO 8850-CLAM-END-FILE.                                    CL**5
00463                                                                      CL**5
00464      IF CLAM-KEY EQUAL CLAIM-SAVE-KEY                                CL**5
00465         GO TO 0450-READ-NEXT-CLAIM.                                  CL**5
00466                                                                      CL**5
00467      IF PI-CARRIER-SECURITY GREATER SPACES                           CL*20
00468          IF CL-CERT-CARRIER NOT = PI-CARRIER-SECURITY                CL*20
00469              GO TO 0450-READ-NEXT-CLAIM.                             CL*20
00470                                                                      CL*20
00471      IF PI-ACCOUNT-SECURITY GREATER SPACES                           CL*20
00472          IF CL-CERT-ACCOUNT NOT = PI-ACCOUNT-SECURITY                CL*20
00473              GO TO 0450-READ-NEXT-CLAIM.                             CL*20
00474                                                                      CL**5
00475      IF CL-CLAIM-TYPE IS EQUAL TO PI-LIFE-OVERRIDE-L1                CL*16
00476          GO TO 0450-READ-NEXT-CLAIM.                                 CL*16
00477                                                                      CL*16
00478      MOVE CL-CLAIM-NO         TO PI-CLAIM-NO.                        CL*16
00479      MOVE CL-CERT-CARRIER     TO PI-CARRIER                          CL**5
00480      MOVE CL-CERT-GROUPING    TO PI-GROUPING                         CL**5
00481      MOVE CL-CERT-STATE       TO PI-STATE                            CL**5
00482      MOVE CL-CERT-ACCOUNT     TO PI-ACCOUNT                          CL**5
00483      MOVE CL-CERT-EFF-DT      TO PI-CERT-EFF-DT                      CL**5
00484      MOVE CL-CERT-NO          TO PI-CERT-NO                          CL**5
00485                                                                      CL**5
00486      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL*18
00487          MOVE CL-CV-REFERENCE-NO TO  PI-MP-REFERENCE-NO.             CL*18
00488                                                                      CL*18
00489      EXEC CICS ENDBR                                                 CL**5
00490           DATASET    ('ELMSTR')                                      CL**5
00491      END-EXEC.                                                       CL**5
00492                                                                      CL**5
00493      MOVE LOW-VALUES TO EL154AO.                                     CL**5
00494      GO TO 7000-BUILD-SCREEN.                                        CL**5
00495                                                                      CL**5
00496  0700-PAGE-BACKWARD.                                                 CL**5
00497                                                                      CL**5
00498      EXEC CICS HANDLE CONDITION                                      CL**5
00499           NOTFND    (8850-CLAM-END-FILE)                             CL**5
00500           ENDFILE   (8850-CLAM-END-FILE)                             CL**5
00501      END-EXEC.                                                       CL**5
00502                                                                      CL**5
00503      MOVE PI-CARRIER      TO CLAM-CARRIER                            CL**5
00504      MOVE PI-COMPANY-CD   TO CLAM-CO                                 CL**5
00505      MOVE PI-CLAIM-NO     TO CLAM-CLAIM                              CL**5
00506      MOVE PI-CERT-NO      TO CLAM-CERT-NUM                           CL**5
00507      MOVE CLAM-KEY        TO CLAIM-SAVE-KEY.                         CL**5
00508                                                                      CL**5
00509      EXEC CICS STARTBR                                               CL**5
00510           DATASET   ('ELMSTR')                                       CL**5
00511           RIDFLD    (CLAM-KEY)                                       CL**5
00512      END-EXEC.                                                       CL**5
00513                                                                      CL**5
00514  0750-READ-PREV-CLAIM.                                               CL**5
00515                                                                      CL**5
00516      EXEC CICS READPREV                                              CL**5
00517           DATASET   ('ELMSTR')                                       CL**5
00518           RIDFLD    (CLAM-KEY)                                       CL**5
00519           SET       (ADDRESS OF CLAIM-MASTER)                        CL*20
00520      END-EXEC.                                                       CL**5
00521                                                                      CL**5
00522      IF PI-COMPANY-CD NOT EQUAL CLAM-CO                              CL**5
00523         GO TO 8850-CLAM-END-FILE.                                    CL**5
00524                                                                      CL**5
00525      IF CLAM-KEY EQUAL CLAIM-SAVE-KEY                                CL**5
00526         GO TO 0750-READ-PREV-CLAIM.                                  CL**5
00527                                                                      CL**5
00528      IF PI-CARRIER-SECURITY GREATER SPACES                           CL*20
00529          IF CL-CERT-CARRIER NOT = PI-CARRIER-SECURITY                CL*20
00530              GO TO 0750-READ-PREV-CLAIM.                             CL*20
00531                                                                      CL*20
00532      IF PI-ACCOUNT-SECURITY GREATER SPACES                           CL*20
00533          IF CL-CERT-ACCOUNT NOT = PI-ACCOUNT-SECURITY                CL*20
00534              GO TO 0750-READ-PREV-CLAIM.                             CL*20
00535                                                                      CL**5
00536      IF CL-CLAIM-TYPE IS EQUAL TO PI-LIFE-OVERRIDE-L1                CL*16
00537          GO TO 0750-READ-PREV-CLAIM.                                 CL*16
00538                                                                      CL*16
00539      MOVE CL-CLAIM-NO         TO PI-CLAIM-NO.                        CL*16
00540      MOVE CL-CERT-CARRIER     TO PI-CARRIER                          CL**5
00541      MOVE CL-CERT-GROUPING    TO PI-GROUPING                         CL**5
00542      MOVE CL-CERT-STATE       TO PI-STATE                            CL**5
00543      MOVE CL-CERT-ACCOUNT     TO PI-ACCOUNT                          CL**5
00544      MOVE CL-CERT-EFF-DT      TO PI-CERT-EFF-DT                      CL**5
00545      MOVE CL-CERT-NO          TO PI-CERT-NO                          CL**5
00546                                                                      CL*18
00547      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL*18
00548          MOVE CL-CV-REFERENCE-NO TO  PI-MP-REFERENCE-NO.             CL*18
00549                                                                      CL**5
00550      EXEC CICS ENDBR                                                 CL**5
00551           DATASET    ('ELMSTR')                                      CL**5
00552      END-EXEC.                                                       CL**5
00553                                                                      CL**5
00554      MOVE LOW-VALUES TO EL154AO.                                     CL**5
00555      GO TO 7000-BUILD-SCREEN.                                        CL**5
00556                                                                      CL**5
00557      EJECT                                                           CL*17
00558  1000-DELETE-CURRENT.                                             EL154
00559      PERFORM 6900-SET-CODES THRU 6999-EXIT.                       EL154
00560      PERFORM 5000-READ-CLAIM THRU 5099-EXIT.                      EL154
00561                                                                   EL154
00562      MOVE CL-AUTO-PAY-SEQ        TO ACTV-SEQ.                     EL154
00563      PERFORM 4000-DELETE-AUTO-PAY THRU 4099-EXIT.                 EL154
00564      MOVE ZEROS                  TO CL-AUTO-PAY-SEQ.              EL154
00565      MOVE LOW-VALUES             TO CL-NEXT-AUTO-PAY-DT.          EL154
00566                                                                   EL154
00567      PERFORM 7100-CHECK-AUTO-ACTIVITY THRU 7100-EXIT.                CL*17
00568                                                                      CL*17
00569      PERFORM 5200-REWRITE THRU 5299-EXIT.                            CL*15
00570                                                                      CL*15
070909     MOVE SPACES TO WS-END-LETTER-FORM-NUM.
070909*    IF WS-ACT-REC-FOUND-SW IS EQUAL TO 'Y'                          CL*17
070909     IF PRINTL IS GREATER THAN +0         
070909         MOVE PRINTI   TO PI-AUTO-END-LETTER
070909     END-IF.
070909     IF PI-AUTO-END-LETTER IS GREATER THAN SPACES
070909         IF PI-AUTO-END-LETTER IS EQUAL TO 'N   '
070909             GO TO 1000-FINISH-DELETE
070909         ELSE
070909             MOVE PI-AUTO-END-LETTER TO WS-END-LETTER-FORM-NUM
070909             MOVE 'Y'    TO WS-ACT-REC-FOUND-SW
070909                            WS-LETTER-SW
070909         END-IF
070909     END-IF.
00575                                                                      CL*17
00576      IF WS-ACT-REC-FOUND-SW IS EQUAL TO 'Y'                          CL*17
00577          IF WS-LETTER-SW IS EQUAL TO 'Y'                             CL*17
00578              PERFORM 7150-CREATE-AUTO-LETTER THRU 7150-EXIT.         CL*17
00579                                                                      CL*17
00580  1000-FINISH-DELETE.                                                 CL*17
00581                                                                      CL*17
00582      IF EMI-NO-ERRORS                                                CL*17
00583          GO TO 8200-SEND-DATAONLY.                                   CL*17
00584                                                                      CL*17
00585      MOVE LOW-VALUES             TO EL154AO.                      EL154
00586      MOVE -1                     TO TERML.                           CL*17
00587      GO TO 8100-SEND-INITIAL-MAP.                                    CL*17
00588      EJECT                                                        EL154
00589  2000-ADD-ONLY.                                                   EL154
00590      PERFORM 6900-SET-CODES THRU 6999-EXIT.                       EL154
00591      PERFORM 5900-VERIFY-ALL-FIELDS THRU 5999-EXIT.               EL154
00592      IF NOT EMI-NO-ERRORS                                         EL154
00593         GO TO 8200-SEND-DATAONLY.                                 EL154
00594                                                                   EL154
00595      PERFORM 5000-READ-CLAIM THRU 5099-EXIT.                      EL154
00596      PERFORM 6000-EDIT-INPUT-DATA THRU 6300-EXIT.                 EL154
00597      IF NOT EMI-NO-ERRORS                                         EL154
00598          GO TO 8200-SEND-DATAONLY.                                EL154
00599                                                                   EL154
00600      IF CL-AUTO-PAY-SEQ  NOT = ZEROS                              EL154
00601         MOVE ER-0372             TO EMI-ERROR                     EL154
00602         MOVE -1                  TO NEFFDTEL                      EL154
00603         MOVE AL-UABON            TO NEFFDTEA                      EL154
00604         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL154
00605         GO TO 8200-SEND-DATAONLY.                                 EL154
00606                                                                   EL154
00607      PERFORM 6400-PAYMENT-CALCULATION  THRU 6499-EXIT.            EL154
00608                                                                      CL*16
00609      IF NFSTAMT-SAVE IS EQUAL TO +0                                  CL*16
00610          MOVE ER-0350            TO  EMI-ERROR                       CL*16
00611          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*16
00612          MOVE -1                 TO  NFSTAMTL                        CL*16
00613          MOVE AL-UNBON           TO  NFSTAMTA                        CL*16
00614          GO TO 8200-SEND-DATAONLY.                                   CL*16
00615                                                                      CL**7
00616      IF EIBAID NOT = DFHPF3                                          CL**7
00617          GO TO 3500-CONFIRM-DATA.                                    CL**7
00618                                                                      CL**7
00619      EXEC CICS HANDLE CONDITION                                   EL154
00620          NOTOPEN(8870-ACTV-NOT-OPEN)                              EL154
00621          END-EXEC.                                                EL154
00622                                                                   EL154
00623      PERFORM 4100-BUILD-AUTO-PAY THRU 4199-EXIT.                  EL154
00624                                                                   EL154
00625      PERFORM 7100-CHECK-AUTO-ACTIVITY THRU 7100-EXIT.                CL*17
00626                                                                      CL*17
00627      PERFORM 5200-REWRITE THRU 5299-EXIT.                            CL*15
00628                                                                      CL*17
090108     MOVE SPACES TO WS-LETTER-FORM-NUM.
090108*     IF WS-ACT-REC-FOUND-SW IS EQUAL TO 'Y'                          CL*17
00630          IF NPRINTL IS GREATER THAN +0                               CL*17
090108            IF NPRINTI IS EQUAL TO 'N   '   
090108                GO TO 2000-FINISH-ADD                            
090108            ELSE
090108                MOVE NPRINTI TO WS-LETTER-FORM-NUM
090108                MOVE 'Y' TO WS-ACT-REC-FOUND-SW
090108                            WS-LETTER-SW.
00633                                                                      CL*17
00634      IF WS-ACT-REC-FOUND-SW IS EQUAL TO 'Y'                          CL*17
00635          IF WS-LETTER-SW IS EQUAL TO 'Y'                             CL*17
00636              PERFORM 7150-CREATE-AUTO-LETTER THRU 7150-EXIT.         CL*17
00637                                                                      CL*17
00638  2000-FINISH-ADD.                                                    CL*17
00639                                                                      CL*17
00640      IF EMI-NO-ERRORS                                                CL*17
00641          GO TO 8200-SEND-DATAONLY.                                   CL*17
00642                                                                      CL*15
00643      MOVE LOW-VALUES             TO EL154AO.                      EL154
00644      GO TO 7000-BUILD-SCREEN.                                     EL154
00645                                                                   EL154
00646      EJECT                                                        EL154
00647  3000-TERMINATE-AND-GENERATE.                                     EL154
00648      PERFORM 6900-SET-CODES THRU 6999-EXIT.                       EL154
00649      PERFORM 5900-VERIFY-ALL-FIELDS THRU 5999-EXIT.               EL154
00650      IF NOT EMI-NO-ERRORS                                         EL154
00651          GO TO 8200-SEND-DATAONLY.                                EL154
00652                                                                   EL154
00653      PERFORM 5000-READ-CLAIM THRU 5099-EXIT.                      EL154
00654      PERFORM 6000-EDIT-INPUT-DATA THRU 6300-EXIT.                 EL154
00655      IF NOT EMI-NO-ERRORS                                         EL154
00656          GO TO 8200-SEND-DATAONLY.                                EL154
00657                                                                   EL154
00658      PERFORM 6400-PAYMENT-CALCULATION THRU 6499-EXIT.             EL154
00659                                                                      CL**7
00660      IF EIBAID NOT = DFHPF3                                          CL**7
00661          GO TO 3500-CONFIRM-DATA.                                    CL**7
00662                                                                      CL**7
00663      MOVE CL-AUTO-PAY-SEQ        TO ACTV-SEQ.                     EL154
00664      PERFORM 4000-DELETE-AUTO-PAY THRU 4099-EXIT.                 EL154
00665                                                                      CL**7
00666      PERFORM 4100-BUILD-AUTO-PAY  THRU 4199-EXIT.                 EL154
00667                                                                   EL154
00668      PERFORM 5200-REWRITE THRU 5299-EXIT.                            CL*15
00669                                                                      CL*17
00670      IF EMI-NO-ERRORS                                                CL*17
00671          GO TO 8200-SEND-DATAONLY.                                   CL*17
00672                                                                      CL*15
00673      MOVE LOW-VALUES TO EL154AO.                                  EL154
00674      GO TO 7000-BUILD-SCREEN.                                     EL154
00675      EJECT                                                        EL154
00676  3500-CONFIRM-DATA.                                                  CL**7
00677                                                                      CL**7
00678      MOVE -1                     TO  ENTERPFL.                       CL*10
00679      MOVE ER-0670                TO  EMI-ERROR.                      CL*10
00680      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**7
00681      GO TO 8200-SEND-DATAONLY.                                       CL**7
00682                                                                      CL**7
00683  4000-DELETE-AUTO-PAY.                                            EL154
00684      EXEC CICS HANDLE CONDITION                                   EL154
00685          NOTOPEN(8870-ACTV-NOT-OPEN)                              EL154
00686          NOTFND(4099-EXIT)                                        EL154
00687          END-EXEC.                                                EL154
00688                                                                   EL154
00689      EXEC CICS READ                                               EL154
00690          UPDATE                                                   EL154
00691          DATASET(ACTV-ID)                                         EL154
00692          SET(ADDRESS OF ACTIVITY-TRAILERS)                           CL*20
00693          RIDFLD(ACTV-KEY)                                         EL154
00694          END-EXEC.                                                EL154
070909
070909     IF PRINTL > ZEROS
070909         MOVE PRINTI             TO AT-AUTO-END-LETTER
070909     END-IF.
00695                                                                   EL154
00696      MOVE CURRENT-SAVE           TO AT-TERMINATED-DT                 CL**5
00697                                     AT-AUTO-PAY-LAST-MAINT-DT        CL**5
00698      MOVE PI-PROCESSOR-ID        TO AT-AUTO-PAY-LAST-UPDATED-BY      CL**5
00699      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS             CL**5
00700      EXEC CICS REWRITE                                            EL154
00701          DATASET(ACTV-ID)                                         EL154
00702          FROM(ACTIVITY-TRAILERS)                                  EL154
00703          END-EXEC.                                                EL154
00704                                                                   EL154
00705  4099-EXIT.                                                       EL154
00706       EXIT.                                                       EL154
00707      EJECT                                                        EL154
00708  4100-BUILD-AUTO-PAY.                                             EL154
00709 ******************************************************************   CL*10
00710 *               CREATE AUTO PAY TRAILER                              CL*10
00711 ******************************************************************   CL*10
00712                                                                      CL*10
00713      SUBTRACT 1                  FROM CL-TRAILER-SEQ-CNT.         EL154
00714      MOVE CL-TRAILER-SEQ-CNT     TO ACTV-SEQ                      EL154
00715                                     CL-AUTO-PAY-SEQ.              EL154
00716      EXEC CICS GETMAIN                                            EL154
00717           SET       (ADDRESS OF ACTIVITY-TRAILERS)                   CL*20
00718           LENGTH    (TRLR-LENGTH)                                    CL**5
00719           INITIMG   (GETMAIN-SPACE)                                  CL**5
00720      END-EXEC.                                                       CL**5
00721                                                                   EL154
00722      MOVE ZEROS                  TO AT-DAYS-IN-REG-PMT.           EL154
00723      MOVE NEFFDTE-SAVE           TO AT-SCHEDULE-START-DT.         EL154
00724      MOVE NFSTPMT-SAVE           TO AT-1ST-PAY-THRU-DT            EL154
00725                                     CL-NEXT-AUTO-PAY-DT.          EL154
00726      MOVE DAYPAYI                TO AT-AUTO-PAY-DAY.                 CL**5
00727      MOVE NLSTPMT-SAVE           TO AT-SCHEDULE-END-DT.           EL154
00728      MOVE NFSTAMT-SAVE           TO AT-FIRST-PMT-AMT.             EL154
00729      MOVE NDAYSFI                TO AT-DAYS-IN-1ST-PMT.           EL154
00730                                                                      CL*10
00731 *    IF PI-USES-PAID-TO                                              CL*10
00732 *        MOVE NFSTPMT-SAVE       TO  DC-BIN-DATE-1                   CL*10
00733 *        MOVE '6'                TO  DC-OPTION-CODE                  CL*10
00734 *        MOVE -1                 TO  DC-ELAPSED-DAYS                 CL*10
00735 *        MOVE +0                 TO  DC-ELAPSED-MONTHS               CL*10
00736 *        PERFORM 9700-DATE-LINK                                      CL*10
00737 *        IF NO-CONVERSION-ERROR                                      CL*10
00738 *            MOVE DC-BIN-DATE-2  TO  AT-1ST-PAY-THRU-DT              CL*10
00739 *                                    CL-NEXT-AUTO-PAY-DT             CL*10
00740 *        ELSE                                                        CL*10
00741 *            MOVE LOW-VALUES     TO  AT-1ST-PAY-THRU-DT              CL*10
00742 *                                    CL-NEXT-AUTO-PAY-DT             CL*10
00743 *    ELSE                                                            CL*10
00744 *        MOVE NFSTPMT-SAVE       TO  AT-1ST-PAY-THRU-DT              CL*10
00745 *                                    CL-NEXT-AUTO-PAY-DT.            CL*10
00746                                                                      CL*10
00747 *    IF PI-USES-PAID-TO                                              CL*10
00748 *        MOVE NLSTPMT-SAVE       TO  DC-BIN-DATE-1                   CL*10
00749 *        MOVE '6'                TO  DC-OPTION-CODE                  CL*10
00750 *        MOVE -1                 TO  DC-ELAPSED-DAYS                 CL*10
00751 *        MOVE +0                 TO  DC-ELAPSED-MONTHS               CL*10
00752 *        PERFORM 9700-DATE-LINK                                      CL*10
00753 *        IF NO-CONVERSION-ERROR                                      CL*10
00754 *            MOVE DC-BIN-DATE-2  TO  AT-SCHEDULE-END-DT              CL*10
00755 *        ELSE                                                        CL*10
00756 *            MOVE LOW-VALUES     TO  AT-SCHEDULE-END-DT              CL*10
00757 *    ELSE                                                            CL*10
00758 *        MOVE NLSTPMT-SAVE       TO  AT-SCHEDULE-END-DT.             CL*10
00759                                                                   EL154
00760      IF NLASTI = 'Y'                                              EL154
00761         MOVE 'F'                 TO AT-LAST-PMT-TYPE              EL154
00762      ELSE                                                            CL**5
00763         MOVE 'P'                 TO AT-LAST-PMT-TYPE.             EL154
00764                                                                   EL154
00765      MOVE NMONTHSI               TO AT-INTERVAL-MONTHS.           EL154
00766      MOVE NREGPMT-SAVE           TO AT-REGULAR-PMT-AMT.           EL154
00767                                                                   EL154
00768      MOVE WS-PAYEE-CODE          TO AT-AUTO-PAYEE-CD.                CL**6
00769                                                                      CL*17
00770      IF NINDCTRI    IS EQUAL TO  'N'                                 CL*17
00771          MOVE 'N'                TO AT-AUTO-CASH                     CL*17
00772      ELSE                                                            CL*17
00773          MOVE 'Y'                TO AT-AUTO-CASH.                    CL*17
070909
070909     MOVE ENDLETTI               TO AT-AUTO-END-LETTER.
00774                                                                      CL*19
00775      IF PI-COMPANY-ID = 'CSL'                                        CL*20
00776         IF PI-CARRIER = 'A' OR 'B' OR 'C' OR '1' OR '2' OR           CL*20
00777                         '3' OR '4'                                   CL*20
00778          INSPECT AT-AUTO-PAYEE-CD CONVERTING 'IBA' TO 'IOA'.         CL*20
00779                                                                   EL154
00780      MOVE LOW-VALUE              TO AT-TERMINATED-DT.             EL154
00781      MOVE 'AT'                   TO AT-RECORD-ID.                 EL154
00782      MOVE  3                     TO AT-TRAILER-TYPE.              EL154
00783      MOVE CURRENT-SAVE           TO AT-RECORDED-DT                   CL**5
00784                                     AT-AUTO-PAY-LAST-MAINT-DT        CL**5
00785      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                   CL**5
00786                                     AT-AUTO-PAY-LAST-UPDATED-BY      CL**5
00787      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.         EL154
00788      MOVE ACTV-KEY               TO AT-CONTROL-PRIMARY.           EL154
00789                                                                   EL154
00790      EXEC CICS WRITE                                              EL154
00791           DATASET(ACTV-ID)                                        EL154
00792           FROM   (ACTIVITY-TRAILERS)                              EL154
00793           RIDFLD (AT-CONTROL-PRIMARY)                             EL154
00794           END-EXEC.                                               EL154
00795                                                                   EL154
00796  4199-EXIT.                                                       EL154
00797       EXIT.                                                       EL154
00798      EJECT                                                        EL154
00799  5000-READ-CLAIM.                                                 EL154
00800      EXEC CICS READ                                               EL154
00801          UPDATE                                                   EL154
00802          DATASET(CLAM-ID)                                         EL154
00803          SET    (ADDRESS OF CLAIM-MASTER)                            CL*20
00804          RIDFLD (CLAM-KEY)                                        EL154
00805          END-EXEC.                                                EL154
00806                                                                   EL154
00807  5099-EXIT.                                                       EL154
00808       EXIT.                                                       EL154
00809                                                                   EL154
00810  5200-REWRITE.                                                       CL*15
00811      EXEC CICS HANDLE CONDITION                                   EL154
00812          DUPKEY                                                   EL154
00813          END-EXEC.                                                EL154
00814                                                                   EL154
00815      MOVE PI-PROCESSOR-ID        TO CL-LAST-MAINT-USER.           EL154
00816      MOVE EIBTIME                TO CL-LAST-MAINT-HHMMSS.         EL154
00817      MOVE CURRENT-SAVE           TO CL-LAST-MAINT-DT.             EL154
00818      EXEC CICS REWRITE                                            EL154
00819          DATASET(CLAM-ID)                                         EL154
00820          FROM   (CLAIM-MASTER)                                    EL154
00821          END-EXEC.                                                EL154
00822                                                                   EL154
00823 *    MOVE 1                      TO EMI-NUMBER-OF-LINES.             CL*17
00824      MOVE ER-0000                TO EMI-ERROR.                    EL154
00825      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL154
00826                                                                   EL154
00827  5299-EXIT.                                                       EL154
00828       EXIT.                                                       EL154
00829      EJECT                                                        EL154
00830  5900-VERIFY-ALL-FIELDS.                                          EL154
00831 ******************************************************************   CL*10
00832 *       PRELIMINARY EDITS TO VERIFY THAT THE REQUIRED FIELDS     *   CL*10
00833 *       HAVE DATA ENTERED IN THEM                                *   CL*10
00834 ******************************************************************   CL*10
00835                                                                      CL**5
00836      IF DAYPAYL EQUAL +0                                             CL**5
00837         MOVE ZEROS TO DAYPAYI.                                       CL**5
00838                                                                      CL**5
00839      IF NEFFDTEL EQUAL +0                                            CL**5
00840         MOVE LOW-VALUES TO NEFFDTEI.                                 CL**7
00841                                                                   EL154
00842      IF NFSTAMTL EQUAL +0                                            CL*21
00843         MOVE ZEROS TO NFSTAMTI                                       CL*21
00844                       NFSTAMTO.                                      CL*21
00845                                                                      CL*21
00846      IF NDAYSFL EQUAL +0                                             CL*21
00847         MOVE ZEROS TO NDAYSFI.                                       CL*21
00848                                                                      CL*21
00849      IF NFSTAMTL EQUAL +0                                            CL*21
00850         IF NDAYSFL NOT EQUAL +0                                      CL*21
00851            MOVE +0        TO NDAYSFL                                 CL*21
00852            MOVE ZEROS     TO NDAYSFI.                                CL*21
00853                                                                      CL*21
00854      IF NMONTHSL = ZEROS                                             CL**8
00855         MOVE -1                  TO NMONTHSL                         CL**8
00856         GO TO 5910-REQUIRED-ERROR.                                   CL**8
00857                                                                      CL**8
00858      IF NFSTPMTL = ZEROS                                          EL154
00859         MOVE -1                  TO NFSTPMTL                      EL154
00860         GO TO 5910-REQUIRED-ERROR.                                EL154
00861                                                                   EL154
00862      IF NLSTPMTL = ZEROS                                          EL154
00863         MOVE -1                  TO NLSTPMTL                      EL154
00864         GO TO 5910-REQUIRED-ERROR.                                EL154
00865                                                                   EL154
00866      IF NLASTL = ZEROS                                            EL154
00867         MOVE -1                  TO NLASTL                        EL154
00868         GO TO 5910-REQUIRED-ERROR.                                EL154
00869                                                                   EL154
00870      IF NPAYEEL = ZEROS                                           EL154
00871         MOVE -1                  TO NPAYEEL                       EL154
00872         GO TO 5910-REQUIRED-ERROR.                                EL154
090108
090108     IF LETTDTEL EQUAL +0           
090108        MOVE LOW-VALUES TO LETTDTEI.
00873                                                                      CL*17
00874      IF (PI-COMPANY-ID  =  'AIG' OR  'AUK') AND                      CL*17
00875         (NINDCTRL =  ZEROS)                                          CL*17
00876            MOVE -1               TO NINDCTRL                         CL*17
00877            GO TO 5910-REQUIRED-ERROR.                                CL*17
00878                                                                   EL154
00879      GO TO 5999-EXIT.                                             EL154
00880                                                                   EL154
00881  5910-REQUIRED-ERROR.                                             EL154
00882      MOVE ER-0358                TO EMI-ERROR.                    EL154
00883      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL154
00884                                                                   EL154
00885  5999-EXIT.                                                       EL154
00886       EXIT.                                                       EL154
00887                                                                   EL154
00888      EJECT                                                        EL154
00889  6000-EDIT-INPUT-DATA.                                            EL154
00890 ******************************************************               CL**5
00891 *    IF THE EFFECTIVE OR "PAID FROM" WAS NOT         *               CL**5
00892 *    ENTERED, THEN USE THE INCURRED OR LAST PAID     *               CL**5
00893 *    THRU DATE                                       *               CL**5
00894 ******************************************************               CL**5
00895                                                                      CL**5
101718     PERFORM 6500-CHECK-ALL-TRLRS THRU 6500-TRLR-EXIT.

00896      MOVE SPACES TO WS-ELIMINATION-SW.                               CL*14
00897                                                                      CL*14
00898      IF NEFFDTEL NOT = ZEROS                                      EL154
00899         MOVE NEFFDTEI            TO DEEDIT-FIELD                  EL154
00900         PERFORM 8600-DEEDIT                                       EL154
00901         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            EL154
00902         MOVE '4'                 TO DC-OPTION-CODE                EL154
00903         PERFORM 9700-DATE-LINK                                    EL154
00904         IF DATE-CONVERSION-ERROR                                  EL154
00905            MOVE ER-0348          TO EMI-ERROR                     EL154
00906            MOVE -1               TO NEFFDTEL                      EL154
00907            MOVE AL-UABON         TO NEFFDTEA                      EL154
00908            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL154
00909         ELSE                                                         CL**5
101718           IF DC-BIN-DATE-1 NOT GREATER WS-HOLD-UNTIL-DT
101718              MOVE ER-1931          TO EMI-ERROR                  EL154
101718              MOVE -1               TO NEFFDTEL                   EL154
101718              MOVE AL-UABON         TO NEFFDTEA                   EL154
101718              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            EL154
101718           ELSE
00910               MOVE DEEDIT-FIELD-V0  TO NEFFDTEO                   EL154
00911               INSPECT NEFFDTEI CONVERTING SPACES TO '/'              CL*20
00912               MOVE AL-UANON         TO NEFFDTEA                   EL154
00913               MOVE DC-BIN-DATE-1    TO NEFFDTE-SAVE                  CL**5
101718           END-IF
00914      ELSE                                                            CL**5
101718        IF WS-HOLD-UNTIL-DT > CL-PAID-THRU-DT
101718           MOVE WS-HOLD-UNTIL-DT    TO DC-BIN-DATE-1
101718           MOVE '6'                 TO DC-OPTION-CODE
101718           MOVE +0                  TO DC-ELAPSED-MONTHS
101718           MOVE +1                  TO DC-ELAPSED-DAYS
101718           PERFORM 9700-DATE-LINK
101718           IF NO-CONVERSION-ERROR
101718              MOVE DC-BIN-DATE-2 TO NEFFDTE-SAVE
101718              MOVE DC-GREG-DATE-1-MDY  TO NEFFDTEO
101718              INSPECT NEFFDTEI CONVERTING SPACES TO '/'
101718           ELSE
101718              MOVE LOW-VALUES TO NEFFDTE-SAVE
101718        ELSE
00915         IF CL-PAID-THRU-DT EQUAL LOW-VALUES                          CL**5
00916            MOVE 'N' TO WS-ELIMINATION-SW                             CL*14
00917            MOVE CL-INCURRED-DT TO NEFFDTE-SAVE                       CL**5
00918                                   DC-BIN-DATE-1                      CL**5
00919            MOVE ' ' TO DC-OPTION-CODE                                CL**5
00920            PERFORM 9700-DATE-LINK                                    CL**5
00921            IF NO-CONVERSION-ERROR                                    CL**5
00922               MOVE DC-GREG-DATE-1-MDY  TO NEFFDTEO                   CL**5
00923               INSPECT NEFFDTEI CONVERTING SPACES TO '/'              CL*20
00924            ELSE                                                      CL**5
00925               MOVE LOW-VALUES TO NEFFDTE-SAVE                        CL**5
00926         ELSE                                                         CL**5
00927            MOVE CL-PAID-THRU-DT TO DC-BIN-DATE-1                     CL**5
00928            MOVE +1 TO DC-ELAPSED-DAYS                                CL**5
00929            MOVE +0 TO DC-ELAPSED-MONTHS                              CL**5
00930            MOVE '6' TO DC-OPTION-CODE                                CL**5
00931            PERFORM 9700-DATE-LINK                                    CL**5
00932            IF NO-CONVERSION-ERROR                                    CL**5
00933               MOVE DC-BIN-DATE-2 TO NEFFDTE-SAVE                     CL**5
00934               MOVE DC-GREG-DATE-1-MDY  TO NEFFDTEO                   CL**5
00935               INSPECT NEFFDTEI CONVERTING SPACES TO '/'              CL*20
00936            ELSE                                                      CL**5
00937               MOVE LOW-VALUES TO NEFFDTE-SAVE.                       CL**5
00938                                                                   EL154
00939 ******************************************************************   CL*10
00940 *       EDIT FIRST PAY THRU/TO DATE.  IF THE "PAY TO" OPTION ON      CL*10
00941 *       THE COMPANY CONTROL IS SET, THE FIRST PAYMENT SAVE DATE      CL*10
00942 *       IS SET BACK MINUS 1 DAY FOR PROCESSING.                      CL*10
00943 ******************************************************************   CL*10
00944                                                                      CL*10
00945      IF NFSTPMTL NOT = ZEROS                                      EL154
00946         MOVE NFSTPMTI            TO DEEDIT-FIELD                  EL154
00947         PERFORM 8600-DEEDIT                                       EL154
00948         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            EL154
00949         MOVE '4'                 TO DC-OPTION-CODE                EL154
00950         PERFORM 9700-DATE-LINK                                    EL154
00951         IF DATE-CONVERSION-ERROR                                  EL154
00952            MOVE ER-0349          TO EMI-ERROR                     EL154
00953            MOVE -1               TO NFSTPMTL                      EL154
00954            MOVE AL-UABON         TO NFSTPMTA                      EL154
00955            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL154
00956           ELSE                                                    EL154
00957            MOVE DEEDIT-FIELD-V0  TO NFSTPMTO                      EL154
00958            INSPECT NFSTPMTI CONVERTING SPACES TO '/'                 CL*20
00959            MOVE AL-UANON         TO NFSTPMTA.                        CL*10
00960 *          MOVE DC-BIN-DATE-1    TO NFSTPMT-SAVE.                    CL*10
00961                                                                      CL*10
00962      IF PI-USES-PAID-TO                                              CL*10
00963          MOVE '6'                TO  DC-OPTION-CODE                  CL*10
00964          MOVE -1                 TO  DC-ELAPSED-DAYS                 CL*10
00965          MOVE +0                 TO  DC-ELAPSED-MONTHS               CL*10
00966          PERFORM 9700-DATE-LINK                                      CL*10
00967          IF NO-CONVERSION-ERROR                                      CL*10
00968              MOVE DC-BIN-DATE-2  TO  NFSTPMT-SAVE                    CL*10
00969          ELSE                                                        CL*10
00970              MOVE ER-0349        TO  EMI-ERROR                       CL*10
00971              MOVE -1             TO  NFSTPMTL                        CL*10
00972              MOVE AL-UABON       TO  NFSTPMTA                        CL*10
00973              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*10
00974      ELSE                                                            CL*10
00975          MOVE DC-BIN-DATE-1      TO  NFSTPMT-SAVE.                   CL*10
00976                                                                      CL*10
00977 ******************************************************************   CL*10
00978 *       EDIT LAST PAY THRU/TO DATE.  IF THE "PAY TO" OPTION ON       CL*10
00979 *       THE COMPANY CONTROL IS SET, THE FIRST PAYMENT SAVE DATE      CL*10
00980 *       IS SET BACK MINUS 1 DAY FOR PROCESSING.                      CL*10
00981 ******************************************************************   CL*10
00982                                                                   EL154
00983      IF NLSTPMTL NOT = ZEROS                                      EL154
00984         MOVE NLSTPMTI            TO DEEDIT-FIELD                  EL154
00985         PERFORM 8600-DEEDIT                                       EL154
00986         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY            EL154
00987         MOVE '4'                 TO DC-OPTION-CODE                EL154
00988         PERFORM 9700-DATE-LINK                                    EL154
00989         IF DATE-CONVERSION-ERROR                                  EL154
00990            MOVE ER-0354          TO EMI-ERROR                     EL154
00991            MOVE -1               TO NLSTPMTL                      EL154
00992            MOVE AL-UABON         TO NLSTPMTA                      EL154
00993            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL154
00994           ELSE                                                    EL154
00995            MOVE DEEDIT-FIELD-V0  TO NLSTPMTO                      EL154
00996            INSPECT NLSTPMTI CONVERTING SPACES TO '/'                 CL*20
00997            MOVE AL-UANON         TO NLSTPMTA.                        CL*10
00998                                                                      CL*10
00999      IF PI-USES-PAID-TO                                              CL*10
01000          MOVE '6'                TO  DC-OPTION-CODE                  CL*10
01001          MOVE -1                 TO  DC-ELAPSED-DAYS                 CL*10
01002          MOVE +0                 TO  DC-ELAPSED-MONTHS               CL*10
01003          PERFORM 9700-DATE-LINK                                      CL*10
01004          IF NO-CONVERSION-ERROR                                      CL*10
01005              MOVE DC-BIN-DATE-2  TO  NLSTPMT-SAVE                    CL*10
01006          ELSE                                                        CL*10
01007              MOVE ER-0354        TO  EMI-ERROR                       CL*10
01008              MOVE -1             TO  NLSTPMTL                        CL*10
01009              MOVE AL-UABON       TO  NLSTPMTA                        CL*10
01010              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*10
01011      ELSE                                                            CL*10
01012          MOVE DC-BIN-DATE-1      TO  NLSTPMT-SAVE.                   CL*10
01013                                                                      CL*10
01014 ******************************************************************   CL*10
01015 *       EDIT FIRST PAYMENT AMOUNT                                    CL*10
01016 ******************************************************************   CL*10
01017                                                                   EL154
01018      IF NFSTAMTL NOT = ZEROS                                      EL154
01019         EXEC CICS BIF DEEDIT                                        CL*20
01020            FIELD (NFSTAMTI)                                        CL*20
01021            LENGTH(10)                                              CL*20
01022         END-EXEC                                                    CL*20
01023         IF NFSTAMTI NOT NUMERIC                                      CL*20
01024            MOVE ER-0350          TO EMI-ERROR                     EL154
01025            MOVE -1               TO NFSTAMTL                      EL154
01026            MOVE AL-UNBON         TO NFSTAMTA                      EL154
01027            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL154
01028         ELSE
01029            MOVE NFSTAMTI         TO NFSTAMT-SAVE                     CL*20
01030                                     NFSTAMTO                         CL*20
01031            MOVE AL-UNNON         TO NFSTAMTA
              end-if
           end-if

01033 ******************************************************************   CL*10
01034 *       EDIT DAY TO PAY                                              CL*10
01035 ******************************************************************   CL*10
01036                                                                      CL*10
01037      IF DAYPAYL NOT EQUAL +0                                         CL**5
01038         IF DAYPAYI NUMERIC AND                                       CL**5
01039            DAYPAYI LESS THAN 32                                      CL**5
01040            MOVE AL-UNNON         TO DAYPAYA                          CL**5
01041         ELSE                                                         CL**5
01042            MOVE ER-0632          TO EMI-ERROR                        CL**5
01043            MOVE -1               TO DAYPAYL                          CL**5
01044            MOVE AL-UNBON         TO DAYPAYA                          CL**5
01045            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**5
01046                                                                      CL**5
01047 ******************************************************************   CL*10
01048 *       EDIT NUMBER OF DAYS IN FIRST PAYMENT PERIOD                  CL*10
01049 ******************************************************************   CL*10
01050                                                                      CL*10
01051      IF NDAYSFL NOT = ZEROS                                       EL154
01052         IF NDAYSFI NOT NUMERIC                                    EL154
01053            MOVE ER-0351          TO EMI-ERROR                     EL154
01054            MOVE -1               TO NDAYSFL                       EL154
01055            MOVE AL-UNBON         TO NDAYSFA                       EL154
01056            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL154
01057         ELSE                                                         CL**5
01058            MOVE AL-UNNON         TO NDAYSFA.                      EL154
01059                                                                      CL*10
01060 ******************************************************************   CL*10
01061 *       EDIT NUMBER OF MONTHS BETWEEN PAYMENTS                       CL*10
01062 ******************************************************************   CL*10
01063                                                                      CL**7
01064      IF NMONTHSL EQUAL +0                                            CL**7
01065         MOVE +1                  TO NMONTHSL                         CL**7
01066         MOVE 1                   TO NMONTHSI.                        CL**8
01067                                                                      CL**8
01068      IF NMONTHSL NOT = ZEROS                                         CL**8
01069         IF NMONTHSI NOT NUMERIC                                   EL154
01070            MOVE ER-0353          TO EMI-ERROR                     EL154
01071            MOVE -1               TO NMONTHSL                      EL154
01072            MOVE AL-UNBON         TO NMONTHSA                      EL154
01073            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL154
01074         ELSE                                                         CL**5
01075            IF NMONTHSI NOT GREATER THAN ZERO                         CL*15
01076               MOVE ER-0353          TO EMI-ERROR                     CL*15
01077               MOVE -1               TO NMONTHSL                      CL*15
01078               MOVE AL-UNBON         TO NMONTHSA                      CL*15
01079               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               CL*15
01080               ELSE                                                   CL*15
01081               MOVE AL-UNNON         TO NMONTHSA.                     CL*15
01082                                                                      CL*10
01083 ******************************************************************   CL*10
01084 *       EDIT PAYEE CODE                                              CL*10
01085 ******************************************************************   CL*10
01086                                                                   EL154
01087      MOVE NPAYEEI TO WS-PAYEE-CODE.                                  CL**5
01088                                                                      CL**5
01089      IF WS-PAYEE-SEQ NOT NUMERIC                                     CL**5
01090         MOVE ER-0357          TO EMI-ERROR                           CL**6
01091         MOVE -1               TO NPAYEEL                             CL**6
01092         MOVE AL-UNBON         TO NPAYEEA                             CL**6
01093         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL**6
01094         GO TO 6300-EXIT.                                             CL**6
01095                                                                      CL**5
01096      IF WS-PAYEE-CD EQUAL 'I' AND                                    CL**6
01097         WS-PAYEE-SEQ-NUM EQUAL 0                                     CL**6
01098         MOVE ER-0357          TO EMI-ERROR                           CL**6
01099         MOVE -1               TO NPAYEEL                             CL**6
01100         MOVE AL-UNBON         TO NPAYEEA                             CL**6
01101         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    CL**6
01102                                                                      CL**6
01103      IF WS-PAYEE-SEQ-NUM GREATER THAN 9                              CL**6
01104         MOVE ER-0357          TO EMI-ERROR                           CL**5
01105         MOVE -1               TO NPAYEEL                             CL**5
01106         MOVE AL-UNBON         TO NPAYEEA                             CL**5
01107         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    CL**5
01108                                                                      CL**5
01109      IF NPAYEEL NOT EQUAL +0                                         CL**5
01110         IF WS-PAYEE-CD = ('I' OR 'B' OR 'A')                         CL**5
01111            MOVE AL-UNNON         TO NPAYEEA                       EL154
01112         ELSE                                                         CL**5
01113            MOVE ER-0357          TO EMI-ERROR                     EL154
01114            MOVE -1               TO NPAYEEL                       EL154
01115            MOVE AL-UNBON         TO NPAYEEA                       EL154
01116            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL154
01117                                                                   EL154
01118 ******************************************************************   CL*10
01119 *       EDIT LAST PAYMENT TYPE                                       CL*10
01120 ******************************************************************   CL*10
01121                                                                      CL*10
01122      IF NLASTL NOT = ZEROS                                        EL154
01123         IF NLASTI = 'Y' OR 'N'                                       CL*20
01124            MOVE AL-UANON         TO NLASTA                        EL154
01125         ELSE                                                         CL**5
01126            MOVE ER-0352          TO EMI-ERROR                     EL154
01127            MOVE -1               TO NLASTL                        EL154
01128            MOVE AL-UABON         TO NLASTA                        EL154
01129            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL154
01130                                                                   EL154
01131 ******************************************************************   CL*17
01132 *       EDIT CASH / NON CASH INDICATOR                               CL*17
01133 ******************************************************************   CL*17
01134                                                                      CL*17
01135      IF NINDCTRL NOT =  ZEROS                                        CL*17
01136         IF NINDCTRI  = 'Y' OR 'N'                                    CL*17
01137            MOVE AL-UANON         TO NINDCTRA                         CL*17
01138         ELSE                                                         CL*17
01139            MOVE ER-0815          TO EMI-ERROR                        CL*17
01140            MOVE -1               TO NINDCTRL                         CL*17
01141            MOVE AL-UABON         TO NINDCTRA                         CL*17
01142            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL*17
01143                                                                      CL*17
01144      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')                   CL*17
01145          IF (CL-ASSOCIATES IS EQUAL TO 'A' OR 'N')                   CL*17
01146              IF NINDCTRI IS EQUAL TO 'N'                             CL*17
01147                  MOVE ER-3537    TO  EMI-ERROR                       CL*17
01148                  MOVE -1         TO  NINDCTRL                        CL*17
01149                  MOVE AL-UABON   TO  NINDCTRA                        CL*17
01150                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           CL*17
01151                                                                      CL*17
01152 ******************************************************************   CL*17
01153 *       EDIT LETTER PRINT INDICATOR FOR AUTO ACTIVITY                CL*17
01154 ******************************************************************   CL*17
01155                                                                      CL*17
01156      IF NPRINTL IS NOT EQUAL TO +0                                   CL*17
090108*01157         IF NPRINTI IS EQUAL TO 'N'                           
01158            MOVE AL-UANON         TO  NPRINTA                         CL*17
090108*01159         ELSE                                                     
090108*01160            MOVE ER-0826          TO  EMI-ERROR                  
090108*01161            MOVE -1               TO  NPRINTL                    
090108*01162            MOVE AL-UABON         TO  NPRINTA                    
090108*01163            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            
090108     END-IF.
01164                                                                      CL*17
090108******************************************************
090108*    IF THE AUTO LETTER DATE IS ENTERED IT IS USED   *
090108*    TO RESOLVE VAIABLE @@AUTOPYDT IN THE LETTER     *
090108*    FORM THAT IS ENTERED IN THE PRINT LETTER FIELD. *
090108******************************************************
090108                                                      
090108     IF LETTDTEL NOT = ZEROS     
090108        MOVE LETTDTEI            TO DEEDIT-FIELD 
090108        PERFORM 8600-DEEDIT                      
090108        MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
090108        MOVE '4'                 TO DC-OPTION-CODE
090108        PERFORM 9700-DATE-LINK                     
090108        IF DATE-CONVERSION-ERROR                  
090108           MOVE ER-3801          TO EMI-ERROR     
090108           MOVE -1               TO LETTDTEL      
090108           MOVE AL-UABON         TO LETTDTEA      
090108           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
090108        ELSE                                       
090108           MOVE DEEDIT-FIELD-V0  TO LETTDTEO       
090108           INSPECT LETTDTEI CONVERTING SPACES TO '/'
090108           MOVE AL-UANON         TO LETTDTEA        
090108           MOVE DC-GREG-DATE-A-EDIT TO LETTDTE-SAVE
090108        END-IF
090108     ELSE
090108         MOVE SPACES TO LETTDTE-SAVE
090108     END-IF.    
090108
070909******************************************************************
070909*       EDIT END LETTER PRINT INDICATOR FOR AUTO ACTIVITY  
070909******************************************************************
070909     IF ENDLETTL NOT = ZEROS
070909             MOVE AL-UANON          TO ENDLETTA
070909     ELSE
070909         IF NPRINTL NOT = ZEROS
070909             IF NPRINTI NOT = 'N   '
070909                 MOVE ER-3397    TO EMI-ERROR
070909                 MOVE -1         TO ENDLETTL
070909                 MOVE AL-UABON   TO ENDLETTA
070909                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
070909             END-IF
070909         END-IF
070909     END-IF.
070909
01165      IF NOT EMI-NO-ERRORS                                         EL154
01166         GO TO 6300-EXIT.                                          EL154
01167                                                                   EL154
01168      IF PI-PROCESSOR-ID  =  LGXX-ID                               EL154
01169         GO TO 6150-READ-CARRIER.                                  EL154
01170                                                                      CL*10
01171 ******************************************************************   CL*10
01172 *       READ PROCESSOR RECORD ON THE CONTROL FILE TO GET THE         CL*10
01173 *       PROCESSOR LIMITS                                             CL*10
01174 ******************************************************************   CL*10
01175                                                                   EL154
01176      EXEC CICS HANDLE CONDITION                                   EL154
01177           NOTFND (6150-READ-CARRIER)                              EL154
01178           NOTOPEN(8890-CNTL-NOT-OPEN)                             EL154
01179           END-EXEC.                                               EL154
01180                                                                   EL154
01181      EXEC CICS READ                                               EL154
01182           DATASET(CNTL-ID)                                        EL154
01183           RIDFLD (CNTL-KEY)                                       EL154
01184           SET    (ADDRESS OF CONTROL-FILE)                           CL*20
01185           END-EXEC.                                               EL154
01186                                                                   EL154
01187      MOVE CF-PROC-CALC-AMT-TOL       TO WS-AMT-TOL.               EL154
01188      MOVE CF-PROC-CALC-DAYS-TOL      TO WS-DAY-TOL.               EL154
01189      MOVE CF-PROC-MAX-AUTO-PMT       TO WS-MAX-AUTO-PMT.          EL154
01190      MOVE CF-PROC-MAX-AUTO-MOS       TO WS-MAX-AUTO-MOS.          EL154
01191      MOVE CF-PROC-MAX-REG-PMT        TO WS-REG-PMT.               EL154
01192                                                                   EL154
01193   6150-READ-CARRIER.                                              EL154
01194 ******************************************************************   CL*10
01195 *       READ THE CARRIER RECORD TO GET PAYMENT TOLERANCES            CL*10
01196 ******************************************************************   CL*10
01197                                                                      CL*10
01198      MOVE '6'                    TO CNTL-RECORD-TYPE.             EL154
01199      MOVE SPACES                 TO CNTL-GENL.                    EL154
01200                                                                      CL**3
01201      IF CONTROL-IS-ACTUAL-CARRIER                                    CL**3
01202          MOVE PI-CARRIER                 TO CNTL-GEN4                CL**3
01203      ELSE                                                            CL**3
01204          MOVE PI-CARRIER-CONTROL-LEVEL   TO CNTL-GEN4.               CL**3
01205                                                                   EL154
01206      EXEC CICS HANDLE CONDITION                                   EL154
01207           NOTFND(6180-READ-CERT)                                  EL154
01208           END-EXEC.                                               EL154
01209                                                                   EL154
01210      EXEC CICS READ                                               EL154
01211           DATASET(CNTL-ID)                                        EL154
01212           RIDFLD (CNTL-KEY)                                       EL154
01213           SET    (ADDRESS OF CONTROL-FILE)                           CL*20
01214           END-EXEC.                                               EL154
01215                                                                   EL154
01216      MOVE CF-CLAIM-CALC-METHOD   TO WS-CALC-METHOD.               EL154
01217                                                                   EL154
01218      IF WS-AMT-TOL = ZERO                                         EL154
01219         MOVE CF-CALC-AMT-TOL     TO WS-AMT-TOL.                   EL154
01220                                                                   EL154
01221      IF WS-DAY-TOL = ZEROS                                        EL154
01222         MOVE CF-CALC-DAYS-TOL    TO WS-DAY-TOL.                   EL154
01223                                                                   EL154
01224      IF WS-MAX-AUTO-PMT = ZEROS                                   EL154
01225         MOVE CF-MAX-AUTO-PMT     TO WS-MAX-AUTO-PMT.              EL154
01226                                                                   EL154
01227      IF WS-MAX-AUTO-MOS = ZEROS                                   EL154
01228         MOVE CF-MAX-AUTO-MOS     TO WS-MAX-AUTO-MOS.              EL154
01229                                                                   EL154
01230      IF WS-REG-PMT = ZEROS                                        EL154
01231         MOVE CF-MAX-REG-PMT      TO WS-REG-PMT.                   EL154
01232                                                                   EL154
01233  6180-READ-CERT.                                                  EL154
01234                                                                      CL*18
01235      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL*18
01236          GO TO 6190-READ-EMPLCY.                                     CL*18
01237                                                                      CL*18
01238      EXEC CICS HANDLE CONDITION                                   EL154
01239           NOTOPEN   (8880-CERT-NOT-OPEN)                             CL*18
01240           NOTFND    (6185-CERT-NOT-FOUND)                            CL*18
01241           END-EXEC.                                               EL154
01242                                                                   EL154
01243      EXEC CICS READ                                               EL154
01244           DATASET(CERT-ID)                                        EL154
01245           RIDFLD (CERT-KEY)                                       EL154
01246           SET    (ADDRESS OF CERTIFICATE-MASTER)                     CL*20
01247           END-EXEC.                                               EL154
01248                                                                   EL154
01249      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK')                   CL*17
01250          MOVE CM-LOAN-1ST-PMT-DT     TO  DC-BIN-DATE-1               CL*17
01251          MOVE '6'                    TO  DC-OPTION-CODE              CL*17
01252          MOVE '1'                    TO  DC-END-OF-MONTH             CL*17
01253          MOVE +0                     TO  DC-ELAPSED-DAYS             CL*17
01254                                          DC-ODD-DAYS-OVER            CL*17
01255          COMPUTE DC-ELAPSED-MONTHS = CM-AH-ORIG-TERM - +1            CL*17
01256          PERFORM 9700-DATE-LINK                                      CL*17
01257          IF NO-CONVERSION-ERROR                                      CL*17
01258              MOVE DC-BIN-DATE-2      TO  WS-EXP-DT                   CL*17
01259          ELSE                                                        CL*17
01260              MOVE LOW-VALUES         TO  WS-EXP-DT                   CL*17
01261      ELSE                                                            CL*17
01262          MOVE CM-AH-LOAN-EXPIRE-DT   TO  WS-EXP-DT.                  CL*17
01263                                                                   EL154
061013     move cm-ah-benefit-amt      to ws-monthly-benefit
061013     move cm-ah-benefit-cd       to ws-ah-benefit-cd
01264      GO TO 6200-START-FINAL-EDITS.                                EL154
01265                                                                   EL154
01266  6185-CERT-NOT-FOUND.                                                CL*18
01267      MOVE ER-0206                TO EMI-ERROR.                    EL154
01268      MOVE -1                     TO NEFFDTEL.                     EL154
01269      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL154
01270      GO TO 6300-EXIT.                                             EL154
01271                                                                      CL*18
01272  6190-READ-EMPLCY.                                                   CL*18
01273                                                                      CL*18
01274      EXEC CICS HANDLE CONDITION                                      CL*18
01275          NOTOPEN   (8885-EMPLCY-NOT-OPEN)                            CL*18
01276          NOTFND    (6195-EMPLCY-NOT-FOUND)                           CL*18
01277      END-EXEC.                                                       CL*18
01278                                                                      CL*18
01279      MOVE CL-CV-REFERENCE-NO     TO  EMPLCY-REFERENCE-NO.            CL*18
01280                                                                      CL*18
01281      EXEC CICS READ                                                  CL*18
01282          DATASET   (EMPLCY-ID)                                       CL*18
01283          RIDFLD    (EMPLCY-KEY)                                      CL*18
01284          SET       (ADDRESS OF POLICY-MASTER)                        CL*20
01285      END-EXEC.                                                       CL*18
01286                                                                      CL*18
01287      MOVE PM-INS-TERMINATION-DT  TO  WS-EXP-DT.                      CL*18
01288      GO TO 6200-START-FINAL-EDITS.                                   CL*18
01289                                                                      CL*18
01290  6195-EMPLCY-NOT-FOUND.                                              CL*18
01291                                                                      CL*18
01292      MOVE ER-9483                TO  EMI-ERROR.                      CL*18
01293      MOVE -1                     TO  NEFFDTEL.                       CL*18
01294      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*18
01295      GO TO 6300-EXIT.                                                CL*18
01296                                                                      CL*18
01297      EJECT                                                        EL154
01298  6200-START-FINAL-EDITS.                                          EL154
01299                                                                      CL*14
01300 ******************************************************************   CL*14
01301 ****** VERIFY ELIMINATION PERIOD                                     CL*14
01302 ******************************************************************   CL*14
01303                                                                      CL*14
01304      IF CL-PAID-THRU-DT NOT EQUAL LOW-VALUES                         CL*14
01305           GO TO 6225-CONTINUE-EDIT.                                  CL*14
01306                                                                      CL*18
01307      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL*18
01308          GO TO 6225-CONTINUE-EDIT.                                   CL*18
01309                                                                      CL*14
01310      MOVE CM-AH-BENEFIT-CD       TO WS-BEN-CD.                       CL*14
01311      MOVE WS-ACCESS              TO CNTL-GENL.                       CL*14
01312      MOVE PI-COMPANY-ID          TO CNTL-CO.                         CL*14
01313      MOVE '5'                    TO CNTL-RECORD-TYPE.                CL*14
01314      MOVE ZEROS                  TO CNTL-SEQ.                        CL*14
01315                                                                      CL*14
01316      IF WS-BEN-CD = ZEROS                                            CL*14
01317          MOVE ER-0283            TO EMI-ERROR                        CL*14
01318          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*14
01319          MOVE -1                 TO NFSTPMTL                         CL*14
01320          MOVE AL-UNBON           TO NFSTPMTA                         CL*14
01321          GO TO 6300-EXIT.                                            CL*14
01322                                                                      CL*14
01323      PERFORM 7200-LOCATE-BENEFIT THRU 7200-EXIT.                     CL*14
01324                                                                      CL*14
01325      IF NOT BENEFIT-FOUND                                            CL*14
01326          MOVE ER-0283            TO EMI-ERROR                        CL*14
01327          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*14
01328          MOVE -1                 TO NFSTPMTL                         CL*14
01329          MOVE AL-UNBON           TO NFSTPMTA                         CL*14
01330          GO TO 6300-EXIT.                                            CL*14
01331                                                                      CL*14
01332      MOVE CF-BENEFIT-ALPHA (SUB-1) TO PI-BENEFIT-SAVE.               CL*14
01333                                                                      CL*14
01334      IF PI-BEN-DAYS NOT NUMERIC                                      CL*14
01335         MOVE ZEROS               TO PI-BEN-DAYS.                     CL*14
01336                                                                      CL*14
01337      IF PI-BEN-TYPE = 'R'                                            CL*14
01338          IF PI-BEN-DAYS IS EQUAL TO ZEROS                            CL*14
01339              NEXT SENTENCE                                           CL*14
01340          ELSE                                                        CL*14
01341              SUBTRACT 1 FROM PI-BEN-DAYS.                            CL*14
01342                                                                      CL*14
01343      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.                   CL*14
01344      MOVE PI-BEN-DAYS            TO DC-ELAPSED-DAYS.                 CL*14
01345      MOVE +0                     TO DC-ELAPSED-MONTHS.               CL*14
01346      MOVE '6'                    TO DC-OPTION-CODE.                  CL*14
01347      PERFORM 9700-DATE-LINK.                                         CL*14
01348      MOVE DC-BIN-DATE-2          TO RETRO-ELIM-DATE.                 CL*14
01349      IF NO-EFFDTE-ENTERED                                            CL*21
01350         IF PI-BEN-TYPE EQUAL 'E'                                     CL*21
01351           MOVE  SPACES              TO  WS-ELIMINATION-SW            CL*14
01352           MOVE  DC-BIN-DATE-2       TO  NEFFDTE-SAVE                 CL*14
01353           MOVE  DC-GREG-DATE-1-MDY  TO  NEFFDTEO                     CL*14
01354           INSPECT NEFFDTEI CONVERTING SPACES TO '/'.                 CL*20
01355                                                                      CL*14
01356      IF PI-BEN-TYPE = 'E'                                            CL*21
01357          IF RETRO-ELIM-DATE GREATER THAN NEFFDTE-SAVE                CL*21
01358              MOVE ER-0541            TO EMI-ERROR                    CL*21
01359              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*21
01360              MOVE -1                 TO NEFFDTEL                     CL*21
01361              MOVE AL-UNBON           TO NEFFDTEA.                    CL*21
01362                                                                      CL*21
01363      IF PI-BEN-TYPE = 'R'                                            CL*21
01364          IF RETRO-ELIM-DATE GREATER THAN NFSTPMT-SAVE                CL*21
01365              MOVE ER-0541            TO EMI-ERROR                    CL*21
01366              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*21
01367              MOVE -1                 TO NEFFDTEL                     CL*21
01368              MOVE AL-UNBON           TO NEFFDTEA.                    CL*21
01369                                                                      CL*21
01370                                                                      CL*14
01371  6225-CONTINUE-EDIT.                                                 CL*14
01372 ******************************************************            EL154
01373 *VERIFY THAT THE LAST PAYMENT DATE IS NOT BEYOND     *            EL154
01374 *THE EXPIRATION DATE.                                *            EL154
01375 ******************************************************            EL154
031809* FOR BIU ALLOW PAYMENT PAST EXP + 7 MONTHS
031809     MOVE WS-EXP-DT TO DC-BIN-DATE-1.
031809     MOVE 6         TO DC-OPTION-CODE.
031809     MOVE 0         TO DC-ELAPSED-DAYS.
031809     MOVE 7         TO DC-ELAPSED-MONTHS.
031809     PERFORM 9700-DATE-LINK.
031809     IF NO-CONVERSION-ERROR
031809         MOVE DC-BIN-DATE-2 TO WS-EXP-DT-PLUS7
031809     ELSE
031809         MOVE WS-EXP-DT     TO WS-EXP-DT-PLUS7
031809     END-IF.
031809
01376      IF NLSTPMT-SAVE GREATER THAN WS-EXP-DT                       EL154
031809       IF CL-CERT-ACCOUNT (9:2) = 'BI' AND 
031809                      NLSTPMT-SAVE <= WS-EXP-DT-PLUS7
031809         IF PI-LAST-ERROR-NO EQUAL ER-3396 OR ER-0670
031809             CONTINUE
031809         ELSE
031809             MOVE ER-3396             TO EMI-ERROR
031809             MOVE AL-UABON            TO NLSTPMTA
031809             MOVE -1                  TO NLSTPMTL
031809             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT 
031809         END-IF
031809       ELSE
01377         MOVE ER-0396             TO EMI-ERROR                     EL154
01378         MOVE AL-UABON            TO NLSTPMTA                      EL154
01379         MOVE -1                  TO NLSTPMTL                      EL154
01380         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL154
01381         GO TO 6300-EXIT.                                          EL154
01382                                                                   EL154
01383 ******************************************************            EL154
01384 *VERIFY THAT THE LAST PAYMENT DATE IS GREATER THAN   *            EL154
01385 *THE FIRST PAYMENT DATE.                             *            EL154
01386 ******************************************************            EL154
01387                                                                   EL154
01388      IF NLSTPMT-SAVE NOT GREATER THAN NFSTPMT-SAVE                EL154
01389         MOVE ER-0559             TO EMI-ERROR                     EL154
01390         MOVE AL-UABON            TO NLSTPMTA                      EL154
01391         MOVE -1                  TO NLSTPMTL                      EL154
01392         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL154
01393         GO TO 6300-EXIT.                                          EL154
01394                                                                   EL154
01395 ******************************************************            EL154
01396 *    MAKE SURE THE DIFFERENCE BETWEEN THE FIRST      *            EL154
01397 *    PAYMENT DATE AND LAST PAYMENT DATE IS           *            EL154
01398 *    DIVISABLE BY THE INTERVAL OF MONTHS,            *            EL154
01399 *    AND THE DURATION IS IN FULL MONTH PMTS.         *            EL154
01400 ******************************************************            EL154
01401      MOVE NFSTPMT-SAVE           TO DC-BIN-DATE-1.                EL154
01402      MOVE NLSTPMT-SAVE           TO DC-BIN-DATE-2.                EL154
01403      MOVE '1'                    TO DC-OPTION-CODE.               EL154
01404      PERFORM 9700-DATE-LINK.                                      EL154
030515     move dc-elapsed-months      to ws-proposed-benefits
100219     if dc-odd-days-over > 0
030515        add 1 to ws-proposed-benefits
030515     end-if
01405      DIVIDE DC-ELAPSED-MONTHS BY NMONTHSI GIVING WS-WORK          EL154
01406             REMAINDER WS-REMAINDER.                               EL154
01407      IF WS-REMAINDER NOT = 0                                      EL154
01408         MOVE ER-0397             TO EMI-ERROR                     EL154
01409         MOVE -1                  TO NMONTHSL                      EL154
01410         MOVE AL-UNBON            TO NMONTHSA                      EL154
01411         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT                 EL154
01412         GO TO 6300-EXIT.                                          EL154
01413                                                                   EL154
01414      IF DC-ODD-DAYS-OVER GREATER THAN +0                          EL154
01415         MOVE ER-0597             TO EMI-ERROR                     EL154
01416         MOVE -1                  TO NLSTPMTL                      EL154
01417         MOVE AL-UNBON            TO NLSTPMTA                      EL154
01418                                     NFSTPMTA                      EL154
01419         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT                 EL154
01420         GO TO 6300-EXIT.                                          EL154
01421                                                                   EL154
01422 ******************************************************            EL154
01423 *    VERIFY THAT FIRST PAYMENT DATE IS NOT LESS      *            EL154
01424 *    THAN THE CURRENT DATE.                          *            EL154
01425 ******************************************************            EL154
01426      IF NFSTPMT-SAVE LESS THAN CURRENT-SAVE                       EL154
01427         MOVE ER-0563             TO EMI-ERROR                     EL154
01428         MOVE -1                  TO NFSTPMTL                      EL154
01429         MOVE AL-UNBON            TO NFSTPMTA                      EL154
01430         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL154
01431         GO TO 6300-EXIT.                                          EL154
01432                                                                   EL154
01433 ******************************************************            EL154
01434 *                                                    *               CL**5
01435 *    EDIT THE NUMBER OF DAYS IN FIRST PAYMENT        *               CL**5
01436 *                                                    *               CL**5
01437 ******************************************************            EL154
01438                                                                      CL**5
01439      MOVE NEFFDTE-SAVE       TO DC-BIN-DATE-1                        CL**5
01440      MOVE -1                 TO DC-ELAPSED-DAYS                      CL**5
01441      MOVE +0                 TO DC-ELAPSED-MONTHS                    CL**5
01442      MOVE '6'                TO DC-OPTION-CODE                       CL**5
01443      PERFORM 9700-DATE-LINK                                          CL**5
01444      IF NO-CONVERSION-ERROR                                          CL**5
01445         MOVE DC-BIN-DATE-2   TO DC-BIN-DATE-1.                       CL**5
01446                                                                      CL**5
01447      MOVE NFSTPMT-SAVE       TO DC-BIN-DATE-2                        CL**5
01448      MOVE +0                 TO DC-ELAPSED-MONTHS                    CL**5
01449                                 DC-ELAPSED-DAYS                      CL**5
01450      MOVE '1'                TO DC-OPTION-CODE                       CL**5
01451      PERFORM 9700-DATE-LINK                                          CL**5
01452      IF NO-CONVERSION-ERROR                                          CL**5
01453         MOVE DC-ELAPSED-DAYS  TO WS-ELAPSED-DAYS.                    CL**5
01454                                                                      CL**5
01455      IF NDAYSFL EQUAL +0                                             CL**5
01456        OR                                                            CL**5
01457         NDAYSFI NOT NUMERIC                                          CL**5
01458        OR                                                            CL**5
01459         NDAYSFI EQUAL ZEROS                                          CL**5
01460         MOVE DC-ELAPSED-DAYS  TO NDAYSFI.                            CL**5
01461                                                                      CL**5
01462      IF DATE1-GREATER-DATE2                                       EL154
01463         MOVE ER-0450 TO EMI-ERROR                                 EL154
01464         MOVE -1      TO NEFFDTEL                                  EL154
01465         MOVE AL-UABON TO NEFFDTEA                                 EL154
01466         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL154
01467         GO TO 6300-EXIT.                                          EL154
01468                                                                   EL154
01469      ADD WS-DAY-TOL    NDAYSFI GIVING WS-DAYS-PLUS-TOL               CL**5
01470      SUBTRACT WS-DAY-TOL FROM NDAYSFI GIVING WS-DAYS-MINUS-TOL       CL**5
01471      IF WS-ELAPSED-DAYS  LESS THAN WS-DAYS-MINUS-TOL  OR             CL**5
01472         WS-ELAPSED-DAYS GREATER THAN WS-DAYS-PLUS-TOL OR             CL**5
01473         NDAYSFI = ZEROS                                           EL154
01474          MOVE ER-0398             TO EMI-ERROR                    EL154
01475          MOVE -1                  TO NDAYSFL                      EL154
01476          MOVE AL-UNBON            TO NDAYSFA                      EL154
01477          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL154
01478          GO TO 6300-EXIT.                                         EL154
01479                                                                   EL154
01480 ******************************************************            EL154
01481 *                                                    *               CL**5
01482 *    VERIFY OR CALCULATE THE FIRST PAYMENT AMOUNT    *               CL**5
01483 *                                                    *               CL**5
01484 ******************************************************               CL**5
01485                                                                      CL**5
01486      MOVE PI-COMPANY-ID         TO CP-COMPANY-ID.                    CL**5
01487      MOVE NEFFDTE-SAVE          TO CP-PAID-FROM-DATE.                CL**5
01488      MOVE NFSTPMT-SAVE          TO CP-PAID-THRU-DT.                  CL**5
01489      MOVE WS-CALC-METHOD        TO CP-CLAIM-CALC-METHOD.             CL**5
01490                                                                      CL*18
01491      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL*18
01492          MOVE PM-PRODUCER           TO CP-ACCOUNT-NUMBER             CL*18
01493          MOVE PM-INS-MONTH-BENEFIT  TO  CP-ORIGINAL-BENEFIT          CL*18
01494      ELSE                                                            CL*18
01495          MOVE CM-AH-BENEFIT-AMT     TO CP-ORIGINAL-BENEFIT           CL*18
01496          MOVE CM-ACCOUNT            TO CP-ACCOUNT-NUMBER.            CL*18
01497                                                                      CL*18
01498      EXEC CICS LINK                                                  CL**5
01499           PROGRAM   ('ELPMNT')                                       CL**5
01500           COMMAREA  (CALCULATION-PASS-AREA)                          CL**5
01501           LENGTH    (CP-COMM-LENGTH)                                 CL*22
01502      END-EXEC.                                                       CL**5
01503                                                                      CL**5
01504      IF CP-CLAIM-PAYMENT NOT NUMERIC                                 CL**5
01505         MOVE +0 TO CP-CLAIM-PAYMENT.                                 CL**5
01506                                                                      CL**5
01507      IF CP-ACTUAL-DAYS NOT NUMERIC                                   CL**5
01508         MOVE +0 TO CP-ACTUAL-DAYS.                                   CL**5
01509                                                                      CL**5
01510      IF NFSTAMT-SAVE EQUAL +0                                        CL**5
01511         MOVE CP-CLAIM-PAYMENT TO NFSTAMT-SAVE                        CL**5
01512                                  NFSTAMTO.                           CL**5
01513                                                                      CL**5
01514      IF NDAYSFI EQUAL ZEROS                                          CL**5
01515         MOVE CP-ACTUAL-DAYS   TO NDAYSFI.                            CL**5
01516                                                                      CL**5
01517                                                                      CL**5
01518      EJECT                                                           CL**5
01519 ******************************************************               CL**5
01520 *    VERIFY THAT DAYS IN FIRST PAY PERIOD IS REALLY  *               CL**5
01521 *    THE DIFFERENCE BETWEEN THE IST PAYMENT DATE     *               CL**5
01522 *    AND THE EFFECTIVE DATE                          *               CL**5
01523 ******************************************************               CL**5
01524 *    MOVE NEFFDTE-SAVE           TO DC-BIN-DATE-1.                   CL**5
01525 *    MOVE NFSTPMT-SAVE           TO DC-BIN-DATE-2.                   CL**5
01526 *    MOVE '1'                    TO DC-OPTION-CODE.                  CL**5
01527 *    PERFORM 9700-DATE-LINK.                                         CL**5
01528 *    IF DATE1-GREATER-DATE2                                          CL**5
01529 *       MOVE ER-0450 TO EMI-ERROR                                    CL**5
01530 *       MOVE -1      TO NEFFDTEL                                     CL**5
01531 *       MOVE AL-UABON TO NEFFDTEA                                    CL**5
01532 *       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL**5
01533 *       GO TO 6300-EXIT.                                             CL**5
01534 *                                                                    CL**5
01535 *    ADD WS-DAY-TOL    NDAYSFI GIVING WS-DAYS-PLUS-TOL.              CL**5
01536 *    SUBTRACT WS-DAY-TOL FROM NDAYSFI GIVING WS-DAYS-MINUS-TOL.      CL**5
01537 *    IF DC-ELAPSED-DAYS  LESS THAN WS-DAYS-MINUS-TOL  OR             CL**5
01538 *       DC-ELAPSED-DAYS GREATER THAN WS-DAYS-PLUS-TOL OR             CL**5
01539 *       NDAYSFI = ZEROS                                              CL**5
01540 *        MOVE ER-0398             TO EMI-ERROR                       CL**5
01541 *        MOVE -1                  TO NDAYSFL                         CL**5
01542 *        MOVE AL-UNBON            TO NDAYSFA                         CL**5
01543 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**5
01544 *        GO TO 6300-EXIT.                                            CL**5
01545 *                                                                    CL**5
01546 ******************************************************               CL**5
01547 *   VERIFY THAT THE FIRST PAYMENT AMOUNT IS WITHIN   *            EL154
01548 *   THE TOLERANCE OF THE DAILY BENEFITS              *            EL154
01549 ******************************************************            EL154
01550 *    IF CL-PMT-CALC-METHOD = '2' OR '5'                              CL**5
01551 *       COMPUTE WS-DAILY-BEN-AMT = CM-AH-BENEFIT-AMT * 12  / 365     CL**5
01552 *    ELSE                                                            CL**5
01553 *       COMPUTE WS-DAILY-BEN-AMT = CM-AH-BENEFIT-AMT * 12  / 360.    CL**5
01554                                                                   EL154
01555      IF WS-AMT-TOL NOT = ZEROS                                    EL154
01556         COMPUTE WS-AMT-PLUS-TOL = (NFSTAMT-SAVE + WS-AMT-TOL)        CL**5
01557         COMPUTE WS-AMT-MINUS-TOL = (NFSTAMT-SAVE - WS-AMT-TOL)       CL**5
01558         IF CP-CLAIM-PAYMENT LESS THAN WS-AMT-MINUS-TOL OR            CL**5
01559            CP-CLAIM-PAYMENT GREATER THAN WS-AMT-PLUS-TOL OR          CL**5
01560            NFSTAMT-SAVE = +0                                      EL154
01561            MOVE ER-0399          TO EMI-ERROR                     EL154
01562            MOVE -1               TO NFSTAMTL                      EL154
01563            MOVE AL-UNBON         TO NFSTAMTA                      EL154
01564            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL154
01565            GO TO 6300-EXIT.                                       EL154
01566                                                                   EL154
01567 ******************************************************            EL154
01568 *   VERIFY THERE IS AN ADDR TRAILER AVAILABLE        *            EL154
01569 *   FOR THE PAYEE CODE SPECIFIED                     *            EL154
01570 ******************************************************            EL154
01571      IF (WS-PAYEE-CD EQUAL 'I' AND                                   CL**5
01572         WS-PAYEE-SEQ-NUM GREATER THAN CL-INSURED-ADDR-CNT)           CL**5
01573        OR                                                            CL**5
01574         (WS-PAYEE-CD EQUAL 'B' AND                                   CL**5
01575         WS-PAYEE-SEQ-NUM GREATER THAN CL-BENIF-ADDR-CNT)             CL**6
01576        OR                                                            CL**6
01577         (WS-PAYEE-CD EQUAL 'A' AND                                   CL**6
01578         WS-PAYEE-SEQ-NUM GREATER THAN CL-ACCOUNT-ADDR-CNT)           CL**6
01579         GO TO 6250-ADDR-ERROR.                                    EL154
01580                                                                   EL154
01581      IF (WS-PAYEE-CD EQUAL 'I')                                      CL**6
01582        OR                                                            CL**6
01583         ((WS-PAYEE-CD EQUAL 'B' OR 'A') AND                          CL**6
01584         (WS-PAYEE-SEQ-NUM GREATER THAN 0))                           CL**6
01585         GO TO 6260-CONT.                                             CL**6
01586                                                                      CL**6
01587      IF WS-PAYEE-CD EQUAL 'A'                                        CL**6
01588         GO TO 6260-CONT.                                             CL**6
01589                                                                      CL**5
01590      IF (WS-PAYEE-CD EQUAL 'B' AND                                   CL**5
01591         CL-BENEFICIARY EQUAL SPACES)                                 CL**5
01592         GO TO 6250-ADDR-ERROR.                                       CL**6
01593                                                                      CL**5
01594      MOVE PI-COMPANY-CD          TO BENE-CO.                      EL154
01595      MOVE 'B'                    TO BENE-REC-TYPE.                   CL**5
01596      MOVE CL-BENEFICIARY         TO BENE-CODE.                    EL154
01597                                                                   EL154
01598      EXEC CICS HANDLE CONDITION                                   EL154
01599           NOTFND   (6250-ADDR-ERROR)                              EL154
01600           END-EXEC.                                               EL154
01601                                                                   EL154
01602      EXEC CICS READ                                               EL154
01603           DATASET     (BENE-ID)                                   EL154
01604           SET         (ADDRESS OF BENEFICIARY-MASTER)                CL*20
01605           RIDFLD      (BENE-KEY)                                  EL154
01606           END-EXEC.                                               EL154
01607                                                                   EL154
01608      GO TO 6260-CONT.                                             EL154
01609                                                                   EL154
01610  6250-ADDR-ERROR.                                                 EL154
01611      IF (PI-COMPANY-ID EQUAL 'AIG' OR 'AUK' OR 'CIG' OR 'CUK')       CL*18
01612          IF (WS-PAYEE-CD EQUAL 'B')   AND                            CL*18
01613             (WS-PAYEE-SEQ-NUM EQUAL 9)                               CL*18
01614              GO TO 6260-CONT.                                        CL*18
01615                                                                      CL*17
01616      MOVE ER-0401         TO EMI-ERROR.                           EL154
01617      MOVE -1              TO NPAYEEL.                             EL154
01618      MOVE AL-UNBON        TO NPAYEEA.                             EL154
01619      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL154
01620                                                                   EL154
01621  6260-CONT.                                                       EL154
01622 ******************************************************            EL154
01623 *   VERIFY THAT THE EFFECTIVE DATE IS NOT            *            EL154
01624 *   BEFORE THE INCURRED DATE                         *            EL154
01625 ******************************************************            EL154
01626      IF NEFFDTE-SAVE LESS THAN CL-INCURRED-DT                     EL154
01627         MOVE ER-0402             TO EMI-ERROR                     EL154
01628         MOVE -1                  TO NEFFDTEL                      EL154
01629         MOVE AL-UABON            TO NEFFDTEA                      EL154
01630         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL154
01631                                                                   EL154
01632 ******************************************************            EL154
01633 *   VERIFY THAT THE CLAIM HAS NOT BEEN CLOSED.       *            EL154
01634 ******************************************************            EL154
01635      IF CLAIM-IS-CLOSED                                           EL154
01636         MOVE ER-1599            TO EMI-ERROR                      EL154
01637         MOVE -1                  TO TERML                         EL154
01638         MOVE AL-UABON            TO TERMA                         EL154
01639         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL154
01640                                                                   EL154
01641 ******************************************************            EL154
01642 *  VERIFY THAT THE EFFECTIVE DATE IS                 *            EL154
01643 *  SUBSEQUENT TO THE PAID-THRU DATE                  *            EL154
01644 ******************************************************            EL154
01645      IF NEFFDTE-SAVE NOT GREATER THAN CL-PAID-THRU-DT             EL154
01646         MOVE ER-0403             TO EMI-ERROR                     EL154
01647         MOVE -1                  TO NEFFDTEL                      EL154
01648         MOVE AL-UABON            TO NEFFDTEA                      EL154
01649         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL154
01650                                                                   EL154
01651 ******************************************************            EL154
01652 *  VERIFY THAT THE NUMBER OF MONTHS BETWEEN          *            EL154
01653 *  THE EFFECTIVE DATE AND THE LAST PAYMENT DATE      *            EL154
01654 *  IS NOT GREATER THAN THE MAXIMUM ALLOWED           *            EL154
01655 ******************************************************            EL154
01656      IF WS-MAX-AUTO-MOS NOT = ZEROS                               EL154
01657         MOVE NEFFDTE-SAVE        TO DC-BIN-DATE-1                 EL154
01658         MOVE NLSTPMT-SAVE        TO DC-BIN-DATE-2                 EL154
01659         MOVE '1'                 TO DC-OPTION-CODE                EL154
01660         PERFORM 9700-DATE-LINK                                    EL154
01661         IF DC-ELAPSED-MONTHS GREATER THAN WS-MAX-AUTO-MOS         EL154
01662            MOVE ER-0404             TO EMI-ERROR                  EL154
01663            MOVE -1                  TO NEFFDTEL                   EL154
01664            MOVE AL-UABON            TO NEFFDTEA NLSTPMTA          EL154
01665            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL154
01666                                                                      CL*13
01667 ******************************************************               CL*18
01668 *  VERIFY THAT THE CERTIFICATE DOES NOT HAVE A       *               CL*18
01669 *  CURRENT STATUS OF '8' (CANCEL APPLIED)            *               CL*18
01670 ******************************************************               CL*18
01671                                                                      CL*18
01672      IF PI-COMPANY-ID IS EQUAL TO 'HAN' OR 'LGX'                     CL*18
01673          IF CM-AH-CANCEL-APPLIED                                     CL*18
01674              MOVE ER-0839            TO  EMI-ERROR                   CL*18
01675              MOVE -1                 TO  ENTERPFL                    CL*18
01676              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*18
061013****************************************************************** 
061013*   check to see if the pd thru dates are past the benefit exp dt
061013****************************************************************** 
061013
010816     if pi-company-id not = 'DCC' and 'VPP'
030515        GO TO 6300-bypass-dcc
061013     END-IF
061013
           move zeros                  to ws-max-benefit-amt
                                          ws-max-benefit-pmts

061013     perform 6320-read-acct      thru 6320-exit
           if ws-dcc-product-code not = spaces
061013        perform 6340-GET-DDF-LIMITS
061013                                 thru 6340-exit
              if pdef-found
                 PERFORM VARYING P1 FROM +1 BY +1 UNTIL
080322              (P1 > +11)
                    OR (PD-PROD-CODE (P1) = cl-claim-type)
                 END-PERFORM
080322           IF P1 < +12
100314              if pd-ben-pct (p1) not numeric
100314                 move zeros      to pd-ben-pct (p1)
100314              end-if
100314              if pd-ben-pct (p1) = zeros
100314                 move +1         to ws-work-ben-pct
100314              else
100314                 move pd-ben-pct (p1)
100314                                 to ws-work-ben-pct
100314              end-if
100314              MOVE PD-MAX-AMT (P1)
100314                                 TO ws-max-benefit-amt
100314              compute ws-monthly-benefit =
100314                 ws-monthly-benefit * ws-work-ben-pct
100314              if ws-monthly-benefit > pd-max-amt (p1)
100314                 move pd-max-amt (p1) to ws-monthly-benefit
100314              end-if
                    move pd-crit-period (p1)
                                       to ws-max-benefit-pmts
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

100314*    if (ws-max-benefit-amt not = zeros)
100314*       and (ws-max-benefit-amt < ws-monthly-benefit)
100314*       move ws-max-benefit-amt  to ws-monthly-benefit
100314*    end-if
061013     perform 6350-get-cert-trlr  thru 6350-exit
061013
061013     if (nfstamt-save = zeros)
061013        or (nfstamt-save = cp-claim-payment)
061013        move ws-monthly-benefit  to nfstamt-save
061013                                    nfstamto
061013     end-if
061013
031417*    compute ws-edit-1st-pmt-amt rounded =
031417*       ws-monthly-benefit * (ndaysfi / 30)

031417*061013if nfstamt-save > ws-edit-1st-pmt-amt
031417*061013   move er-1657             to emi-error
031417*061013   move -1                  to nfstamtl
031417*061013   move al-unbon            to nfstamta
031417*061013   perform 9900-error-format
031417*061013                            thru 9900-exit
031417*061013end-if
061013
061013     if nfstamt-save = zeros
061013        move ws-monthly-benefit to nfstamt-save
061013     end-if
061013
031417     MOVE DC-ELAPSED-MONTHS      TO WS-PROPOSED-BENEFITS
031417     COMPUTE WS-BENEFITS-PREV-PAID = WS-BENEFITS-PREV-PAID +
031417        (NFSTAMT-SAVE / WS-MONTHLY-BENEFIT)
031417     IF CL-CRITICAL-PERIOD > ZEROS
031417        IF (WS-BENEFITS-PREV-PAID + WS-PROPOSED-BENEFITS) >
031417           CL-CRITICAL-PERIOD
031417              MOVE ER-1657             TO EMI-ERROR
031417              MOVE -1                  TO NFSTAMTL
031417              MOVE AL-UNBON            TO NFSTAMTA
031417              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
031417        END-IF
031417     END-IF

100713     move cm-ah-loan-expire-dt   to ws-benefit-expiration-dt
061013     IF CL-CRITICAL-PERIOD NOT = ZEROS and spaces
061013        move cl-critical-period  to dc-elapsed-months
061013        MOVE CL-INCURRED-DT      TO DC-BIN-DATE-1
061013        MOVE ZEROS               TO DC-ELAPSED-DAYS
061013        MOVE '6'                 TO DC-OPTION-CODE
061013        PERFORM 9700-DATE-LINK
061013        MOVE DC-BIN-DATE-2       TO ws-BENEFIT-EXPIRATION-DT
061013     END-IF
061013
061013     if nlstpmt-save > ws-benefit-expiration-dt
061013        move er-1658             to emi-error
061013        move -1                  to nlstpmtl
061013        move al-uabon            to nlstpmta
061013        perform 9900-error-format thru 9900-exit
061013     end-if
061013
061013     MOVE cl-control-primary     TO ELTRLR-KEY
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
061013           if at-note-error-no (s1) not = '1661' and '1662'
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
052614                 when cl-claim-type = 'F'
052614                    move '  FL  '
052614                                 to emi-claim-type
080322                 when cl-claim-type = 'B'
080322                    move '  BR  '
080322                                 to emi-claim-type
080322                 when cl-claim-type = 'H'
080322                    move '  HS '
080322                                 to emi-claim-type
100518                 when cl-claim-type = 'O'
100518                    move '  OT  '
100518                                 to emi-claim-type
061013                 when other
061013                    move '  AH  '
061013                                 to emi-claim-type
061013              end-evaluate
061013           end-if
061013           move -1               to nlstpmtl
061013           PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013           end-if
061013        end-perform
061013     end-if
030515     go to 6300-exit

           .
030515 6300-bypass-dcc.
030515
030515
030515     perform 6350-get-cert-trlr  thru 6350-exit
030515
030515     compute ws-benefits-prev-paid = ws-benefits-prev-paid +
030515        (nfstamt-save / ws-monthly-benefit)
030515     if cl-critical-period > zeros
030515        if (ws-benefits-prev-paid + ws-proposed-benefits) >
030515           cl-critical-period
030515           move er-1658          to emi-error
030515           move -1               to nlstpmtl
030515           move al-uabon         to nlstpmta
030515           perform 9900-error-format
030515                                 thru 9900-exit
030515        end-if
030515     end-if

           .
061013 6300-EXIT.                                                       EL154
061013      EXIT.                                                       EL154
061013 6320-read-acct.
061013
061013     move spaces              to WS-ACCT-RECORD-SW
061013                                 ws-dcc-product-code
010816                                 ws-clp-state
061013
061013     MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
061013     MOVE PI-CARRIER          TO ACCT-CARRIER
061013     MOVE PI-GROUPING         TO ACCT-GROUPING
061013     MOVE PI-STATE            TO ACCT-STATE
061013     MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
061013     MOVE PI-CERT-EFF-DT      TO ACCT-EXP-DT
061013     MOVE ERACCT-PARTIAL-KEY  TO WS-ERACCT-SAVE-KEY
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
061013        move am-dcc-product-code to ws-dcc-product-code
010816        move am-dcc-clp-state    to ws-clp-state
061013        set acct-found           to true
061013     end-if
061013
061013     .
061013 6320-exit.
061013     exit.
061013
061013 6340-GET-DDF-LIMITS.
061013
061013     MOVE ' '                    TO WS-PDEF-RECORD-SW
061013     MOVE PI-COMPANY-CD          TO ERPDEF-KEY
061013     MOVE pi-state               TO ERPDEF-STATE
010816     if ws-clp-state not = pi-state and spaces and zeros
010816        move ws-clp-state        to erpdef-state
010816     end-if
061013     MOVE WS-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
061013     MOVE 'A'                    TO ERPDEF-BEN-TYPE
061013     MOVE ws-AH-BENEFIT-CD       TO ERPDEF-BEN-CODE
061013     MOVE pi-CERT-EFF-DT         TO ERPDEF-EXP-DT
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
061013        GO TO 6340-EXIT
061013     END-IF
061013
061013     .
061013 6340-READNEXT.
061013
061013     EXEC CICS READNEXT
061013        DATASET  ('ERPDEF')
061013        SET      (ADDRESS OF PRODUCT-MASTER)
061013        RIDFLD   (ERPDEF-KEY)
061013        RESP     (WS-RESPONSE)
061013     END-EXEC
061013
061013     IF NOT WS-RESP-NORMAL
061013        GO TO 6340-ENDBR
061013     END-IF
061013
061013     IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
061013        IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
061013           MOVE 'Y'              TO WS-PDEF-RECORD-SW
061013        ELSE
061013           GO TO 6340-READNEXT
061013        END-IF
061013     ELSE
061013        GO TO 6340-ENDBR
061013     END-IF

061013     .
061013 6340-ENDBR.
061013
061013     EXEC CICS ENDBR
061013        DATASET  ('ERPDEF')
061013     END-EXEC
061013
061013     .
061013 6340-EXIT.
061013     EXIT.
061013
061013 6350-get-cert-trlr.
061013
061013     move spaces                 to ws-cert-trlr-sw
061013     move zeros                  to ws-benefits-prev-paid
030515                                    ws-total-paid
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
061013        go to 6350-exit
061013     end-if
061013
061013     set cert-trlr-found         to true

           perform varying s1 from +1 by +1 until
              (s1 > +24)
              or (cs-claim-no (s1) = spaces)
030515        if (cs-benefit-period (s1) = cl-benefit-period)
030515           and (cs-claim-type (s1) = cl-claim-type)
030515           and (cs-insured-type (s1) = cl-insured-type)
030515           compute ws-total-paid =
030515              ws-total-paid + cs-total-paid (s1)
              end-if
           end-perform

           compute ws-benefits-prev-paid =
030515        (ws-total-paid / ws-monthly-benefit)

061013     .
061013 6350-exit.
061013     exit.

01681  6400-PAYMENT-CALCULATION.                                        EL154
01682                                                                      CL**4
01683      IF WS-CALC-METHOD = '1' OR '2' OR '3' OR '4' OR '5' OR '6'      CL**4
01684          IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                    CL*18
01685              COMPUTE NREGPMT-SAVE ROUNDED =                          CL*18
01686                  (PM-INS-MONTH-BENEFIT * NMONTHSI)                   CL*18
01687              MOVE NREGPMT-SAVE   TO  NREGAMTO                        CL*18
01688              GO TO 6499-EXIT                                         CL*18
01689          ELSE                                                        CL*18
01690              COMPUTE NREGPMT-SAVE ROUNDED =                          CL*18
061013                 (ws-monthly-benefit * NMONTHSI)                     CL*18
01692              MOVE NREGPMT-SAVE   TO  NREGAMTO                        CL*18
01693              GO TO 6499-EXIT.                                        CL*18
01694                                                                   EL154
01695      MOVE NFSTPMT-SAVE           TO DC-BIN-DATE-1.                EL154
01696      MOVE +0                     TO DC-ELAPSED-DAYS.                 CL**5
01697      MOVE NMONTHSI               TO DC-ELAPSED-MONTHS.            EL154
01698      MOVE '6'                    TO DC-OPTION-CODE.               EL154
01699      PERFORM 9700-DATE-LINK.                                      EL154
01700                                                                   EL154
01701      MOVE NFSTPMT-SAVE           TO DC-BIN-DATE-1.                EL154
01702      MOVE '1'                    TO DC-OPTION-CODE.               EL154
01703      PERFORM 9700-DATE-LINK.                                      EL154
01704                                                                   EL154
01705      IF (PI-COMPANY-ID  = 'FIA' AND                                  CL*20
01706         CL-CERT-ACCOUNT = '0000011043')                              CL*20
01707         COMPUTE WS-DAILY-RATE ROUNDED =                           EL154
01708                              (CM-AH-BENEFIT-AMT * 13) / 365       EL154
01709         COMPUTE NREGPMT-SAVE ROUNDED =                            EL154
01710                              WS-DAILY-RATE * DC-ELAPSED-DAYS      EL154
01711         MOVE NREGPMT-SAVE  TO  NREGAMTO                              CL**7
01712         GO TO 6499-EXIT.                                          EL154
01713                                                                   EL154
01714 *    IF WS-CALC-METHOD = '4'                                         CL**4
01715 *       COMPUTE WS-DAILY-RATE ROUNDED =                              CL**4
01716 *                            (CM-AH-BENEFIT-AMT / 30)                CL**4
01717 *       COMPUTE NREGPMT-SAVE ROUNDED =                               CL**4
01718 *                            WS-DAILY-RATE * DC-ELAPSED-DAYS.        CL**4
01719 *                                                                    CL**4
01720 *    IF WS-CALC-METHOD = '5'                                         CL**4
01721 *       COMPUTE WS-DAILY-RATE ROUNDED =                              CL**4
01722 *                            (CM-AH-BENEFIT-AMT * 12) / 365          CL**4
01723 *       COMPUTE NREGPMT-SAVE ROUNDED =                               CL**4
01724 *                            WS-DAILY-RATE * DC-ELAPSED-DAYS.        CL**4
01725                                                                   EL154
01726  6499-EXIT.                                                       EL154
01727       EXIT.                                                       EL154
101718 6500-CHECK-ALL-TRLRS.                                               CL*40
101718                                                                     CL*39
101718     MOVE ' ' TO WS-BROWSE-TRLR-SW.                                  CL*39
101718                                                                     CL*39
101718     EXEC CICS HANDLE CONDITION                                      CL*39
101718         ENDFILE   (6500-END-CHECK)                                  CL*40
101718         NOTFND    (6500-END-CHECK)                                  CL*40
101718     END-EXEC.                                                       CL*88
101718                                                                     CL*39
101718     MOVE PI-COMPANY-CD          TO TRLR-COMP-CD
101718     MOVE PI-CARRIER             TO TRLR-CARRIER
101718     MOVE PI-CLAIM-NO            TO TRLR-CLAIM-NO
101718     MOVE PI-CERT-NO             TO TRLR-CERT-NO                     CL**5
101718
101718     MOVE +100                   TO TRLR-SEQ-NO.                     CL*39
101718                                                                     CL*39
101718     EXEC CICS STARTBR                                               CL*39
101718          DATASET    ('ELTRLR')                                      CL*39
101718          RIDFLD     (ELTRLR-KEY)                                    CL*39
101718          GTEQ                                                       CL*39
101718     END-EXEC.                                                       CL*39
101718                                                                     CL*39
101718     MOVE 'Y' TO WS-BROWSE-TRLR-SW.                                  CL*39
101718                                                                     CL*39
101718 6500-READ-TRLR-NEXT.                                                CL*40
101718                                                                     CL*39
101718     EXEC CICS READNEXT                                              CL*39
101718          DATASET    ('ELTRLR')                                      CL*39
101718          RIDFLD     (ELTRLR-KEY)                                    CL*39
101718          SET        (ADDRESS OF ACTIVITY-TRAILERS)                  CL*81
101718     END-EXEC.                                                       CL*39
101718                                                                     CL*39
101718     IF TRLR-COMP-CD  = PI-COMPANY-CD  AND                           CL*93
101718        TRLR-CARRIER  = PI-CARRIER  AND                              CL*93
101718        TRLR-CLAIM-NO = PI-CLAIM-NO AND                              CL*93
101718        TRLR-CERT-NO  = PI-CERT-NO                                   CL*93
101718          NEXT SENTENCE                                              CL*93
101718      ELSE                                                           CL*93
101718          GO TO 6500-END-CHECK.                                      CL*93
101718                                                                     CL*39
101718* **  BYPASS ALL NON-PAYMENT TRAILERS                                CL*39
101718                                                                     CL*39
101718     IF AT-TRAILER-TYPE = '2'                                        CL*93
101718        NEXT SENTENCE                                                CL*39
101718     ELSE                                                            CL*39
101718        GO TO 6500-READ-TRLR-NEXT.                                   CL*40
101718                                                                     CL*39
101718* **  BYPASS ALL NON-PARTIAL PAYMENTS                                CL*39
101718                                                                     CL*39
101718     IF AT-PAYMENT-TYPE = '1' OR '2'                                 CL*93
101718        NEXT SENTENCE                                                CL*39
101718     ELSE                                                            CL*39
101718        GO TO 6500-READ-TRLR-NEXT.                                   CL*40
101718                                                                     CL*40
101718* **  BYPASS ALL VOID PAYMENTS                                       CL*40
101718                                                                     CL*40
101718     IF AT-VOID-DT NOT = LOW-VALUES                                  CL*93
101718        GO TO 6500-READ-TRLR-NEXT.                                   CL*40
101718                                                                     CL*39
101718     IF (AT-PAID-FROM-DT = LOW-VALUES OR SPACES) OR                  CL*93
101718        (AT-PAID-THRU-DT = LOW-VALUES OR SPACES)                     CL*93
101718          GO TO 6500-READ-TRLR-NEXT.                                 CL*93
101718
101718     IF AT-PAID-THRU-DT > WS-HOLD-UNTIL-DT
101718       AND AT-TO-BE-WRITTEN-DT > ZERO
101718        MOVE AT-PAID-THRU-DT TO WS-HOLD-UNTIL-DT
101718     END-IF
101718                                                                     CL*39
101718     GO TO 6500-READ-TRLR-NEXT.                                      CL*40
101718                                                                     CL*39
101718 6500-END-CHECK.                                                     CL*40
101718                                                                     CL*39
101718     IF WS-BROWSE-TRLR-SW = 'Y'                                      CL*93
101718        EXEC CICS ENDBR                                              CL*39
101718             DATASET   ('ELTRLR')                                    CL*39
101718        END-EXEC.                                                    CL*39
101718                                                                     CL*39
101718     MOVE 'Y' TO WS-BROWSE-TRLR-SW.                                  CL*39
101718                                                                     CL*39
101718 6500-TRLR-EXIT.                                                     CL*40
101718     EXIT.                                                           CL*39

01728      EJECT                                                        EL154
01729  6900-SET-CODES.                                                  EL154
01730      MOVE PI-CLAIM-NO            TO CLAM-CLAIM                    EL154
01731                                     ACTV-CLAIM.                   EL154
01732                                                                   EL154
01733      MOVE PI-COMPANY-ID          TO CNTL-CO.                      EL154
01734                                                                   EL154
01735      MOVE PI-COMPANY-CD          TO CLAM-CO                       EL154
01736                                     ACTV-CO                       EL154
01737                                     CERT-CO                          CL*18
01738                                     EMPLCY-COMPANY-CD.               CL*18
01739                                                                   EL154
01740      MOVE PI-CARRIER             TO CLAM-CARRIER                  EL154
01741                                     ACTV-CARRIER                  EL154
01742                                     CERT-CARRIER                     CL*18
01743                                     EMPLCY-CARRIER.                  CL*18
01744                                                                   EL154
01745      MOVE PI-CERT-NO             TO CLAM-CERT-NUM                 EL154
01746                                     ACTV-CERT-NUM                 EL154
01747                                     CERT-CERT-NUM.                EL154
01748                                                                   EL154
01749      MOVE PI-GROUPING            TO CERT-GROUPING                    CL*18
01750                                     EMPLCY-GROUPING.                 CL*18
01751      MOVE PI-STATE               TO CERT-STATE                       CL*18
01752                                     EMPLCY-STATE.                    CL*18
01753      MOVE PI-ACCOUNT             TO CERT-ACCOUNT                     CL*18
01754                                     EMPLCY-PRODUCER.                 CL*18
01755      MOVE PI-CERT-EFF-DT         TO CERT-EFF-DT                      CL*18
01756                                     EMPLCY-EFF-DT.                   CL*18
01757      MOVE '2'                    TO CNTL-RECORD-TYPE.             EL154
01758      MOVE PI-PROCESSOR-ID        TO CNTL-GENL.                    EL154
01759      MOVE ZEROS                  TO CNTL-SEQ.                     EL154
01760                                                                   EL154
01761  6999-EXIT.                                                       EL154
01762       EXIT.                                                       EL154
01763      EJECT                                                        EL154
01764  7000-BUILD-SCREEN.                                               EL154
01765      PERFORM 6900-SET-CODES.                                      EL154
01766                                                                   EL154
01767      EXEC CICS HANDLE CONDITION                                   EL154
01768           NOTOPEN(8860-CLAM-NOT-OPEN)                             EL154
01769           NOTFND (7090-CLAIM-NOT-FOUND)                           EL154
01770           END-EXEC.                                               EL154
01771                                                                   EL154
01772      EXEC CICS READ                                               EL154
01773           DATASET(CLAM-ID)                                        EL154
01774           SET    (ADDRESS OF CLAIM-MASTER)                           CL*20
01775           RIDFLD (CLAM-KEY)                                       EL154
01776           END-EXEC.                                               EL154
01777                                                                   EL154
01778      MOVE CL-CLAIM-NO            TO CLAIMO                           CL**5
01779      MOVE CL-CERT-PRIME          TO CERTO                            CL**5
01780      MOVE CL-CERT-SFX            TO SUFFIXO                          CL**5
01781      MOVE CL-CARRIER             TO CARRO                            CL**5
01782      MOVE CL-AUTO-PAY-SEQ        TO ACTV-SEQ.                     EL154
01783                                                                   EL154
01784      IF ACTV-SEQ = ZEROS                                          EL154
01785         GO TO 7080-NO-TRAILER.                                    EL154
01786                                                                   EL154
01787      EXEC CICS HANDLE CONDITION                                   EL154
01788           NOTOPEN(8870-ACTV-NOT-OPEN)                             EL154
01789           NOTFND (7080-NO-TRAILER)                                EL154
01790           END-EXEC.                                               EL154
01791                                                                   EL154
01792      EXEC CICS READ                                               EL154
01793           DATASET(ACTV-ID)                                        EL154
01794           SET    (ADDRESS OF ACTIVITY-TRAILERS)                      CL*20
01795           RIDFLD (ACTV-KEY)                                       EL154
01796      END-EXEC.                                                       CL**5
01797                                                                   EL154
01798      MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.                EL154
01799      MOVE SPACE                  TO DC-OPTION-CODE.               EL154
01800      PERFORM 9700-DATE-LINK.                                      EL154
01801      MOVE DC-GREG-DATE-1-EDIT    TO CESTDTEI.                     EL154
01802                                                                   EL154
01803                                                                   EL154
01804      MOVE AT-SCHEDULE-START-DT   TO DC-BIN-DATE-1.                EL154
01805      MOVE SPACE                  TO DC-OPTION-CODE.               EL154
01806      PERFORM 9700-DATE-LINK.                                      EL154
01807      MOVE DC-GREG-DATE-1-EDIT    TO CEFFDTEI.                     EL154
01808                                                                      CL**9
01809      IF AT-AUTO-PAY-DAY NUMERIC                                      CL**9
01810          MOVE AT-AUTO-PAY-DAY    TO DAY2PAYI                         CL**9
01811      ELSE                                                            CL**9
01812          MOVE ZEROS              TO DAY2PAYI.                        CL**9
01813                                                                   EL154
01814      IF PI-USES-PAID-TO                                              CL*10
01815          MOVE AT-1ST-PAY-THRU-DT      TO  DC-BIN-DATE-1              CL*10
01816          MOVE '6'                     TO  DC-OPTION-CODE             CL*10
01817          MOVE +1                      TO  DC-ELAPSED-DAYS            CL*10
01818          MOVE +0                      TO  DC-ELAPSED-MONTHS          CL*10
01819          PERFORM 9700-DATE-LINK                                      CL*10
01820          IF NO-CONVERSION-ERROR                                      CL*10
01821              MOVE DC-GREG-DATE-1-EDIT TO  CFSTPMTI                   CL*10
01822          ELSE                                                        CL*10
01823              MOVE LOW-VALUES          TO  CFSTPMTI                   CL*10
01824      ELSE                                                            CL*10
01825          MOVE AT-1ST-PAY-THRU-DT      TO  DC-BIN-DATE-1              CL*10
01826          MOVE SPACE                   TO  DC-OPTION-CODE             CL*10
01827          PERFORM 9700-DATE-LINK                                      CL*10
01828          IF NO-CONVERSION-ERROR                                      CL*10
01829              MOVE DC-GREG-DATE-1-EDIT TO  CFSTPMTI                   CL*10
01830          ELSE                                                        CL*10
01831              MOVE LOW-VALUES          TO  CFSTPMTI.                  CL*10
01832                                                                   EL154
01833      IF PI-USES-PAID-TO                                              CL*10
01834          MOVE AT-SCHEDULE-END-DT      TO  DC-BIN-DATE-1              CL*10
01835          MOVE '6'                     TO  DC-OPTION-CODE             CL*10
01836          MOVE +1                      TO  DC-ELAPSED-DAYS            CL*10
01837          MOVE +0                      TO  DC-ELAPSED-MONTHS          CL*10
01838          PERFORM 9700-DATE-LINK                                      CL*10
01839          IF NO-CONVERSION-ERROR                                      CL*10
01840              MOVE DC-GREG-DATE-1-EDIT TO  CLSTPMTI                   CL*10
01841          ELSE                                                        CL*10
01842              MOVE LOW-VALUES          TO  CLSTPMTI                   CL*10
01843      ELSE                                                            CL*10
01844          MOVE AT-SCHEDULE-END-DT      TO  DC-BIN-DATE-1              CL*10
01845          MOVE SPACE                   TO  DC-OPTION-CODE             CL*10
01846          MOVE +0                      TO  DC-ELAPSED-DAYS            CL*10
01847                                           DC-ELAPSED-MONTHS          CL*10
01848          PERFORM 9700-DATE-LINK                                      CL*10
01849          IF NO-CONVERSION-ERROR                                      CL*10
01850              MOVE DC-GREG-DATE-1-EDIT TO  CLSTPMTI                   CL*10
01851          ELSE                                                        CL*10
01852              MOVE LOW-VALUES          TO  CLSTPMTI.                  CL*10
01853                                                                   EL154
01854      MOVE AT-FIRST-PMT-AMT       TO CFSTAMTO.                     EL154
01855      MOVE AT-DAYS-IN-1ST-PMT     TO CDAYSFO.                      EL154
01856                                                                   EL154
01857      IF AT-LAST-PMT-TYPE = 'F'                                    EL154
01858         MOVE 'Y'                 TO CLASTO                        EL154
01859      ELSE                                                            CL**5
01860         MOVE 'N'                 TO CLASTO.                       EL154
01861                                                                   EL154
01862      MOVE AT-RECORDED-BY         TO CESTBYO.                      EL154
01863      MOVE AT-REGULAR-PMT-AMT     TO CREGPMTO.                     EL154
01864      MOVE AT-AUTO-CASH           TO CINDCTRO.                        CL*17
070909
070909     MOVE AT-AUTO-END-LETTER     TO PRINTI
070909                                    PI-AUTO-END-LETTER.
01865                                                                   EL154
01866      IF AT-AUTO-PAYEE-TYPE EQUAL 'I'                                 CL**5
01867         MOVE 'INSURED'           TO CPAYEEO                       EL154
01868      ELSE                                                            CL**5
01869      IF AT-AUTO-PAYEE-TYPE EQUAL 'B'                                 CL**5
01870         MOVE 'BENEFIC'           TO CPAYEEO                          CL*11
01871      ELSE                                                            CL**5
01872      IF AT-AUTO-PAYEE-TYPE EQUAL 'A'                                 CL**5
01873         MOVE 'ACCOUNT'           TO CPAYEEO.                      EL154
01874                                                                   EL154
01875      MOVE AT-INTERVAL-MONTHS     TO CMONTHSO.                     EL154
01876      MOVE -1                     TO TERML.                        EL154
01877      GO TO 8100-SEND-INITIAL-MAP.                                 EL154
01878                                                                   EL154
01879  7080-NO-TRAILER.                                                 EL154
01880      MOVE 'A'                    TO TERMI.                        EL154
01881      MOVE AL-PADON               TO TERMA                            CL*17
01882                                     PRINTA.                          CL*17
01883      MOVE -1                     TO NFSTPMTL.                        CL**5
01884      MOVE ER-0346                TO EMI-ERROR.                    EL154
01885      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL154
01886      GO TO 8100-SEND-INITIAL-MAP.                                 EL154
01887                                                                   EL154
01888  7090-CLAIM-NOT-FOUND.                                            EL154
01889      MOVE ER-0186                TO EMI-ERROR.                    EL154
01890      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL154
01891      GO TO 8100-SEND-INITIAL-MAP.                                 EL154
01892                                                                   EL154
01893      EJECT                                                           CL*17
01894  7100-CHECK-AUTO-ACTIVITY.                                           CL*17
01895                                                                      CL*17
01896      EXEC CICS HANDLE CONDITION                                      CL*17
01897          NOTFND   (7100-NOT-FOUND)                                   CL*17
01898      END-EXEC.                                                       CL*17
01899                                                                      CL*17
01900      MOVE PI-COMPANY-ID          TO  CNTL-CO.                        CL*17
01901      MOVE 'T'                    TO  CNTL-RECORD-TYPE.               CL*17
01902      MOVE SPACES                 TO  CNTL-GENL.                      CL*17
01903      MOVE +0                     TO  CNTL-SEQ.                       CL*17
01904                                                                      CL*17
01905      EXEC CICS READ                                                  CL*17
01906          DATASET   (CNTL-ID)                                         CL*17
01907          RIDFLD    (CNTL-KEY)                                        CL*17
01908          SET       (ADDRESS OF CONTROL-FILE)                         CL*20
01909      END-EXEC.                                                       CL*17
01910                                                                      CL*17
01911      MOVE 'Y'                        TO  WS-ACT-REC-FOUND-SW.        CL*17
01912                                                                      CL*17
01913  7100-CHECK-SYSTEM-ACTIVITY.                                         CL*17
01914                                                                      CL*17
01915      IF CL-ACTIVITY-CODE IS NOT EQUAL TO ZEROS                       CL*17
01916          MOVE CL-ACTIVITY-CODE             TO  SUB-1                 CL*17
01917          IF SUB-1 IS GREATER THAN +9                                 CL*17
01918              SUBTRACT +9 FROM SUB-1                                  CL*17
01919              MOVE CF-USER-RESET-SW (SUB-1) TO  WS-RESET-SW           CL*17
01920          ELSE                                                        CL*17
01921              MOVE CF-SYS-RESET-SW  (SUB-1) TO  WS-RESET-SW           CL*17
01922      ELSE                                                            CL*17
01923          MOVE 'Y'                          TO  WS-RESET-SW.          CL*17
01924                                                                      CL*17
01925      IF TERMI IS EQUAL TO 'A'                                        CL*17
01926          IF (CF-SYS-ACTIVE-SW (5) IS EQUAL TO 'N' OR ' ')            CL*17
01927              MOVE 'N'                TO  WS-LETTER-SW                CL*17
01928                                          WS-RESET-SW                 CL*17
01929              GO TO 7100-EXIT.                                        CL*17
01930                                                                      CL*17
01931      IF TERMI IS EQUAL TO 'A'                                        CL*17
01932          IF (CF-SYS-LETTER-ID (5) EQUAL SPACES OR LOW-VALUES)        CL*17
01933              MOVE 'N'                TO  WS-LETTER-SW                CL*17
01934          ELSE                                                        CL*17
01935              MOVE 'Y'                TO  WS-LETTER-SW.               CL*17
01936                                                                      CL*17
01937      IF TERMI IS EQUAL TO 'Y'                                        CL*17
01938          IF (CF-SYS-ACTIVE-SW (6) IS EQUAL TO 'N' OR ' ')            CL*17
01939              MOVE 'N'                TO  WS-LETTER-SW                CL*17
01940                                          WS-RESET-SW                 CL*17
01941              GO TO 7100-EXIT.                                        CL*17
01942                                                                      CL*17
01943      IF TERMI IS EQUAL TO 'Y'                                        CL*17
01944          IF (CF-SYS-LETTER-ID (6) EQUAL SPACES OR LOW-VALUES)        CL*17
01945              MOVE 'N'                TO  WS-LETTER-SW                CL*17
01946          ELSE                                                        CL*17
01947              MOVE 'Y'                TO  WS-LETTER-SW.               CL*17
01948                                                                      CL*17
01949      IF CL-ACTIVITY-CODE IS EQUAL TO 09                              CL*17
01950          GO TO 7100-EXIT.                                            CL*17
01951                                                                      CL*17
01952      IF (PI-COMPANY-ID IS EQUAL TO 'AIG' OR 'AUK') AND               CL*17
01953         (CL-ACTIVITY-CODE IS EQUAL TO 11)                            CL*17
01954          GO TO 7100-EXIT.                                            CL*17
01955                                                                      CL*17
01956      IF WS-RESET-SW IS EQUAL TO 'N'                                  CL*17
01957          GO TO 7100-EXIT.                                            CL*17
01958                                                                      CL*17
01959      MOVE CURRENT-BIN-DATE           TO  CL-ACTIVITY-MAINT-DT.       CL*17
01960                                                                      CL*17
01961      IF TERMI IS EQUAL TO 'A'                                        CL*17
01962          MOVE 05                     TO  CL-ACTIVITY-CODE            CL*17
01963          MOVE 'AUTO'                 TO  CL-ACTIVITY-MAINT-TYPE      CL*17
01964      ELSE                                                            CL*17
01965          MOVE 06                     TO  CL-ACTIVITY-CODE            CL*17
01966          MOVE 'TERM'                 TO  CL-ACTIVITY-MAINT-TYPE.     CL*17
01967                                                                      CL*17
01968      GO TO 7100-EXIT.                                                CL*17
01969                                                                      CL*17
01970  7100-NOT-FOUND.                                                     CL*17
01971                                                                      CL*17
01972      MOVE 'N'                        TO  WS-ACT-REC-FOUND-SW         CL*17
01973                                          WS-LETTER-SW.               CL*17
01974                                                                      CL*17
01975  7100-EXIT.                                                          CL*17
01976      EXIT.                                                           CL*17
01977                                                                      CL*17
01978  7150-CREATE-AUTO-LETTER.                                            CL*17
01979                                                                      CL*17
01980      MOVE LOW-VALUES                 TO  W-1523-LINKDATA.            CL*17
01981      MOVE PROGRAM-INTERFACE-BLOCK    TO  W-1523-COMMON-PI-DATA.      CL*17
01982                                                                      CL*17
01983      IF TERMI IS EQUAL TO 'A'                                        CL*17
090108         IF WS-LETTER-FORM-NUM > SPACES
090108            MOVE WS-LETTER-FORM-NUM     TO  W-1523-FORM-NUMBER         
090108         ELSE
01984          MOVE CF-SYS-LETTER-ID (5)   TO  W-1523-FORM-NUMBER  
090108         END-IF
01985      ELSE                                                            CL*17
070909         IF WS-END-LETTER-FORM-NUM > SPACES
070909             MOVE WS-END-LETTER-FORM-NUM TO W-1523-FORM-NUMBER
070909         ELSE
01986          MOVE CF-SYS-LETTER-ID (6)   TO  W-1523-FORM-NUMBER.         CL*17
01987                                                                      CL*17
01988      IF TERMI IS EQUAL TO 'A'                                        CL*17
01989        IF CF-SYS-RESEND-DAYS (5) IS NOT EQUAL TO ZEROS               CL*17
01990          MOVE CURRENT-BIN-DATE       TO  DC-BIN-DATE-1               CL*17
01991          MOVE '6'                    TO  DC-OPTION-CODE              CL*17
01992          MOVE CF-SYS-RESEND-DAYS (5) TO  DC-ELAPSED-DAYS             CL*17
01993          MOVE +0                     TO  DC-ELAPSED-MONTHS           CL*17
01994          PERFORM 9700-DATE-LINK                                      CL*17
01995          IF NO-CONVERSION-ERROR                                      CL*17
01996              MOVE DC-BIN-DATE-2      TO  W-1523-RESEND-DATE          CL*17
01997          ELSE                                                        CL*17
01998              MOVE LOW-VALUES         TO  W-1523-RESEND-DATE.         CL*17
01999                                                                      CL*17
02000                                                                      CL*17
02001      IF TERMI IS EQUAL TO 'A'                                        CL*17
02002        IF CF-SYS-FOLLOW-UP-DAYS (5) IS NOT EQUAL TO ZEROS            CL*17
02003          MOVE CURRENT-BIN-DATE       TO  DC-BIN-DATE-1               CL*17
02004          MOVE '6'                    TO  DC-OPTION-CODE              CL*17
02005          MOVE CF-SYS-FOLLOW-UP-DAYS (5)  TO  DC-ELAPSED-DAYS         CL*17
02006          MOVE +0                     TO  DC-ELAPSED-MONTHS           CL*17
02007          PERFORM 9700-DATE-LINK                                      CL*17
02008          IF NO-CONVERSION-ERROR                                      CL*17
02009              MOVE DC-BIN-DATE-2      TO  W-1523-FOLLOW-UP-DATE       CL*17
02010          ELSE                                                        CL*17
02011              MOVE LOW-VALUES         TO  W-1523-FOLLOW-UP-DATE.      CL*17
02012                                                                      CL*17
02013      IF TERMI IS EQUAL TO 'Y'                                        CL*17
02014        IF CF-SYS-RESEND-DAYS (6) IS NOT EQUAL TO ZEROS               CL*17
02015          MOVE CURRENT-BIN-DATE       TO  DC-BIN-DATE-1               CL*17
02016          MOVE '6'                    TO  DC-OPTION-CODE              CL*17
02017          MOVE CF-SYS-RESEND-DAYS (6) TO  DC-ELAPSED-DAYS             CL*17
02018          MOVE +0                     TO  DC-ELAPSED-MONTHS           CL*17
02019          PERFORM 9700-DATE-LINK                                      CL*17
02020          IF NO-CONVERSION-ERROR                                      CL*17
02021              MOVE DC-BIN-DATE-2      TO  W-1523-RESEND-DATE          CL*17
02022          ELSE                                                        CL*17
02023              MOVE LOW-VALUES         TO  W-1523-RESEND-DATE.         CL*17
02024                                                                      CL*17
02025                                                                      CL*17
02026      IF TERMI IS EQUAL TO 'Y'                                        CL*17
02027        IF CF-SYS-FOLLOW-UP-DAYS (6) IS NOT EQUAL TO ZEROS            CL*17
02028          MOVE CURRENT-BIN-DATE       TO  DC-BIN-DATE-1               CL*17
02029          MOVE '6'                    TO  DC-OPTION-CODE              CL*17
02030          MOVE CF-SYS-FOLLOW-UP-DAYS (6)  TO  DC-ELAPSED-DAYS         CL*17
02031          MOVE +0                     TO  DC-ELAPSED-MONTHS           CL*17
02032          PERFORM 9700-DATE-LINK                                      CL*17
02033          IF NO-CONVERSION-ERROR                                      CL*17
02034              MOVE DC-BIN-DATE-2      TO  W-1523-FOLLOW-UP-DATE       CL*17
02035          ELSE                                                        CL*17
02036              MOVE LOW-VALUES         TO  W-1523-FOLLOW-UP-DATE.      CL*17
090108
090108     MOVE LETTDTE-SAVE               TO W-1523-LETTER-DATE.
02037                                                                      CL*17
02038      EXEC CICS LINK                                                  CL*17
02039          PROGRAM    (LINK-1523)                                      CL*17
02040          COMMAREA   (W-1523-LINKDATA)                                CL*17
02041          LENGTH     (W-1523-COMM-LENGTH)                             CL*17
02042      END-EXEC.                                                       CL*17
02043                                                                      CL*17
02044      IF W-1523-ERROR-CODE IS EQUAL TO ZEROS                          CL*17
02045          GO TO 7150-EXIT.                                            CL*17
02046                                                                      CL*17
02047      IF W-1523-FATAL-ERROR                                           CL*17
02048          MOVE ER-0802                TO  EMI-ERROR                   CL*17
02049          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*17
02050          GO TO 7150-EXIT.                                            CL*17
02051                                                                      CL*17
02052      IF W-1523-ERROR-CODE IS EQUAL TO 0191                           CL*17
02053          MOVE ER-0803                TO  EMI-ERROR                   CL*17
02054          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*17
02055                                                                      CL*17
02056  7150-EXIT.                                                          CL*17
02057      EXIT.                                                           CL*17
02058      EJECT                                                           CL*17
02059  7200-LOCATE-BENEFIT.                                                CL*13
02060      MOVE SPACES TO WS-BENEFIT-FOUND-SW.                             CL*13
02061                                                                      CL*13
02062      EXEC CICS HANDLE CONDITION                                      CL*13
02063          ENDFILE(7200-EXIT)                                          CL*13
02064          NOTFND(7200-EXIT)                                           CL*13
02065          END-EXEC.                                                   CL*13
02066                                                                      CL*13
02067      EXEC CICS READ                                                  CL*13
02068          DATASET(CNTL-ID)                                            CL*13
02069          SET(ADDRESS OF CONTROL-FILE)                                CL*20
02070          RIDFLD(CNTL-KEY)                                            CL*13
02071          GTEQ                                                        CL*13
02072          END-EXEC.                                                   CL*13
02073                                                                      CL*13
02074      IF (CNTL-CO NOT = CF-COMPANY-ID)   OR                           CL*13
02075         (CNTL-RECORD-TYPE NOT = CF-RECORD-TYPE)                      CL*13
02076            GO TO 7200-EXIT.                                          CL*13
02077                                                                      CL*13
02078      PERFORM 7200-BENEFIT-DUMMY THRU 7200-DUMMY-EXIT                 CL*13
02079          VARYING SUB-1 FROM 1 BY 1 UNTIL                             CL*13
02080             ((SUB-1 GREATER 8) OR                                    CL*13
02081             (CF-BENEFIT-CODE (SUB-1) = WS-BEN-CD)).                  CL*13
02082                                                                      CL*13
02083      IF SUB-1 NOT = 9                                                CL*13
02084          MOVE 'Y' TO WS-BENEFIT-FOUND-SW.                            CL*13
02085                                                                      CL*13
02086      GO TO 7200-EXIT.                                                CL*13
02087                                                                      CL*13
02088  7200-BENEFIT-DUMMY.                                                 CL*13
02089                                                                      CL*13
02090  7200-DUMMY-EXIT.                                                    CL*13
02091      EXIT.                                                           CL*13
02092                                                                      CL*13
02093  7200-EXIT.                                                          CL*13
02094      EXIT.                                                           CL*13
02095      EJECT                                                           CL*13
02096  8100-SEND-INITIAL-MAP.                                           EL154
02097      MOVE CURRENT-GREG-DATE      TO RUNDTEO.                         CL*13
02098      MOVE EIBTIME                TO TIME-IN.                      EL154
02099      MOVE TIME-OUT               TO RUNTIMEO.                     EL154
02100      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O                      EL154
02101      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O                      EL154
02102                                                                      CL*10
02103      IF PI-USES-PAID-TO                                              CL*10
02104          MOVE FIRST-PMT-HD2      TO  FSTHD1O                         CL*10
02105                                      FSTHD2O                         CL*10
02106          MOVE LAST-PMT-HD2       TO  LSTHD1O                         CL*10
02107          MOVE LAST-PMT-HD4       TO  LSTHD2O                         CL*10
02108      ELSE                                                            CL*10
02109          MOVE FIRST-PMT-HD1      TO  FSTHD1O                         CL*10
02110                                      FSTHD2O                         CL*10
02111          MOVE LAST-PMT-HD1       TO  LSTHD1O                         CL*10
02112          MOVE LAST-PMT-HD3       TO  LSTHD2O.                        CL*10
02113                                                                      CL*10
02114      EXEC CICS SEND                                               EL154
02115          MAP(MAP-NAME)                                            EL154
02116          MAPSET(MAPSET-NAME)                                      EL154
02117          FROM(EL154AO)                                            EL154
02118          ERASE                                                    EL154
02119          CURSOR                                                   EL154
02120          END-EXEC.                                                EL154
02121                                                                   EL154
02122      MOVE 'EL154   ' TO PI-CALLING-PROGRAM.                       EL154
02123      GO TO 9100-RETURN-TRAN.                                      EL154
02124                                                                   EL154
02125  8200-SEND-DATAONLY.                                              EL154
02126      MOVE CURRENT-GREG-DATE TO RUNDTEO.                              CL*13
02127      MOVE EIBTIME           TO TIME-IN.                              CL*13
02128      MOVE TIME-OUT          TO RUNTIMEO.                             CL*13
02129      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O                      EL154
02130      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O                      EL154
02131                                                                      CL*10
02132      IF PI-USES-PAID-TO                                              CL*10
02133          MOVE FIRST-PMT-HD2      TO  FSTHD1O                         CL*10
02134                                      FSTHD2O                         CL*10
02135          MOVE LAST-PMT-HD2       TO  LSTHD1O                         CL*10
02136          MOVE LAST-PMT-HD4       TO  LSTHD2O                         CL*10
02137      ELSE                                                            CL*10
02138          MOVE FIRST-PMT-HD1      TO  FSTHD1O                         CL*10
02139                                      FSTHD2O                         CL*10
02140          MOVE LAST-PMT-HD1       TO  LSTHD1O                         CL*10
02141          MOVE LAST-PMT-HD3       TO  LSTHD2O.                        CL*10
02142                                                                      CL*10
02143      EXEC CICS SEND                                               EL154
02144          MAP(MAP-NAME)                                            EL154
02145          MAPSET(MAPSET-NAME)                                      EL154
02146          FROM(EL154AO)                                            EL154
02147          DATAONLY                                                 EL154
02148          ERASEAUP                                                 EL154
02149          CURSOR                                                   EL154
02150          END-EXEC.                                                EL154
02151                                                                   EL154
02152      GO TO 9100-RETURN-TRAN.                                      EL154
02153                                                                   EL154
02154  8300-SEND-TEXT.                                                  EL154
02155      EXEC CICS SEND TEXT                                          EL154
02156          FROM(LOGOFF-TEXT)                                        EL154
02157          LENGTH(LOGOFF-LENGTH)                                    EL154
02158          ERASE                                                    EL154
02159          FREEKB                                                   EL154
02160          END-EXEC.                                                EL154
02161                                                                   EL154
02162      EXEC CICS RETURN                                             EL154
02163          END-EXEC.                                                EL154
02164                                                                   EL154
02165      EJECT                                                        EL154
02166  8600-DEEDIT.                                                     EL154
02167      EXEC CICS BIF DEEDIT                                         EL154
02168           FIELD(DEEDIT-FIELD)                                     EL154
02169           LENGTH(15)                                              EL154
02170           END-EXEC.                                               EL154
02171                                                                   EL154
02172  8800-UNAUTHORIZED-ACCESS.                                        EL154
02173      MOVE UNACCESS-MSG TO LOGOFF-MSG.                             EL154
02174      GO TO 8300-SEND-TEXT.                                        EL154
02175                                                                   EL154
02176  8810-PF23.                                                       EL154
02177      MOVE EIBAID TO PI-ENTRY-CD-1.                                EL154
02178      MOVE XCTL-005 TO PGM-NAME.                                   EL154
02179      GO TO 9300-XCTL.                                             EL154
02180                                                                   EL154
02181                                                                      CL**5
02182  8850-CLAM-END-FILE.                                                 CL**5
02183      MOVE -1                     TO  ENTERPFL.                       CL*16
02184      MOVE ER-0130 TO EMI-ERROR.                                      CL**5
02185      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**5
02186      GO TO 8200-SEND-DATAONLY.                                       CL**5
02187                                                                   EL154
02188  8860-CLAM-NOT-OPEN.                                              EL154
02189      MOVE -1                     TO  ENTERPFL.                       CL*16
02190      MOVE ER-0154 TO EMI-ERROR.                                   EL154
02191      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL154
02192      GO TO 8200-SEND-DATAONLY.                                    EL154
02193                                                                   EL154
02194  8870-ACTV-NOT-OPEN.                                              EL154
02195      MOVE -1                     TO  ENTERPFL.                       CL*16
02196      MOVE ER-0172 TO EMI-ERROR.                                   EL154
02197      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL154
02198      GO TO 8200-SEND-DATAONLY.                                    EL154
02199                                                                   EL154
02200  8880-CERT-NOT-OPEN.                                              EL154
02201      MOVE -1                     TO  ENTERPFL.                       CL*16
02202      MOVE ER-0169                TO EMI-ERROR.                    EL154
02203      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*18
02204      GO TO 8200-SEND-DATAONLY.                                       CL*18
02205                                                                      CL*18
02206  8885-EMPLCY-NOT-OPEN.                                               CL*18
02207      MOVE -1                     TO  ENTERPFL.                       CL*18
02208      MOVE ER-9883                TO  EMI-ERROR.                      CL*18
02209      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL154
02210      GO TO 8200-SEND-DATAONLY.                                    EL154
02211                                                                   EL154
02212  8890-CNTL-NOT-OPEN.                                              EL154
02213      MOVE -1                     TO  ENTERPFL.                       CL*16
02214      MOVE ER-0042                TO EMI-ERROR.                    EL154
02215      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL154
02216      GO TO 8200-SEND-DATAONLY.                                    EL154
02217                                                                   EL154
02218  9000-RETURN-CICS.                                                EL154
02219      EXEC CICS RETURN                                             EL154
02220          END-EXEC.                                                EL154
02221                                                                   EL154
02222  9100-RETURN-TRAN.                                                EL154
02223      MOVE    EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.            EL154
02224      MOVE '154A' TO PI-CURRENT-SCREEN-NO.                         EL154
02225      EXEC CICS RETURN                                             EL154
02226          TRANSID(TRANS-ID)                                        EL154
02227          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL154
02228          LENGTH(PI-COMM-LENGTH)                                   EL154
02229          END-EXEC.                                                EL154
02230                                                                   EL154
02231  9200-RETURN-MAIN-MENU.                                           EL154
02232      MOVE XCTL-126 TO PGM-NAME.                                   EL154
02233      GO TO 9300-XCTL.                                             EL154
02234                                                                   EL154
02235  9300-XCTL.                                                       EL154
02236      EXEC CICS XCTL                                               EL154
02237          PROGRAM(PGM-NAME)                                        EL154
02238          COMMAREA(PROGRAM-INTERFACE-BLOCK)                        EL154
02239          LENGTH(PI-COMM-LENGTH)                                   EL154
02240          END-EXEC.                                                EL154
02241                                                                   EL154
02242  9400-CLEAR.                                                      EL154
02243      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.                       EL154
02244      GO TO 9300-XCTL.                                             EL154
02245                                                                   EL154
02246  9500-PF12.                                                       EL154
02247      MOVE XCTL-010 TO PGM-NAME.                                   EL154
02248      GO TO 9300-XCTL.                                             EL154
02249                                                                   EL154
02250  9600-PGMID-ERROR.                                                EL154
02251      EXEC CICS HANDLE CONDITION                                   EL154
02252          PGMIDERR(8300-SEND-TEXT)                                 EL154
02253          END-EXEC.                                                EL154
02254                                                                   EL154
02255      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.                     EL154
02256      MOVE ' '          TO PI-ENTRY-CD-1.                          EL154
02257      MOVE XCTL-005     TO PGM-NAME.                               EL154
02258      MOVE PGM-NAME     TO LOGOFF-PGM.                             EL154
02259      MOVE PGMIDERR-MSG TO LOGOFF-FILL.                            EL154
02260      GO TO 9300-XCTL.                                             EL154
02261                                                                   EL154
02262  9700-DATE-LINK.                                                  EL154
02263      MOVE LINK-ELDATCV TO PGM-NAME                                EL154
02264      EXEC CICS LINK                                               EL154
02265          PROGRAM    (PGM-NAME)                                    EL154
02266          COMMAREA   (DATE-CONVERSION-DATA)                        EL154
02267          LENGTH     (DC-COMM-LENGTH)                              EL154
02268          END-EXEC.                                                EL154
02269                                                                   EL154
02270  9900-ERROR-FORMAT.                                               EL154
02271      IF NOT EMI-ERRORS-COMPLETE                                   EL154
02272          MOVE LINK-001 TO PGM-NAME                                EL154
02273          EXEC CICS LINK                                           EL154
02274              PROGRAM(PGM-NAME)                                    EL154
02275              COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)              EL154
02276              LENGTH(EMI-COMM-LENGTH)                              EL154
02277              END-EXEC.                                            EL154
02278                                                                   EL154
02279  9900-EXIT.                                                       EL154
02280      EXIT.                                                        EL154
02281                                                                   EL154
02282  9990-ABEND.                                                      EL154
02283      MOVE LINK-004 TO PGM-NAME.                                   EL154
02284      MOVE DFHEIBLK TO EMI-LINE1.                                  EL154
02285      EXEC CICS LINK                                               EL154
02286          PROGRAM(PGM-NAME)                                        EL154
02287          COMMAREA(EMI-LINE1)                                      EL154
02288          LENGTH(72)                                               EL154
02289          END-EXEC.                                                EL154
02290                                                                   EL154
02291      GO TO 8200-SEND-DATAONLY.                                    EL154
02292      GOBACK.                                                      EL154
02293                                                                   EL154
02294  9995-SECURITY-VIOLATION.                                         EL154
02295                              COPY ELCSCTP.                        EL154
02296                                                                   EL154
02297  9995-EXIT.                                                       EL154
02298      EXIT.                                                        EL154
02299                                                                   EL154
