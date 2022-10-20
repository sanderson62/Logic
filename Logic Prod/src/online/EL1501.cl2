00001  IDENTIFICATION DIVISION.                                         04/29/97
00002                                                                   EL1501
00003  PROGRAM-ID.                 EL1501.                                 LV030
00004 *              PROGRAM CONVERTED BY                                  CL*21
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*21
00006 *              CONVERSION DATE 05/01/95 15:33:51.                    CL*21
00007 *                            VMOD=2.030.                             CL*30
00008 *                                                                 EL1501
00009 *AUTHOR.     LOGIC,INC.                                              CL*21
00010 *            DALLAS, TEXAS.                                          CL*21
00011                                                                   EL1501
00024 *REMARKS.    TRANSACTION - EXH9 - CLAIM HISTORY                   EL1501
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
041613* 041613  CR2013031200002  AJRA  ADD MAIL RECEIVED ACTION TYPE
061013* 061013    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
031214* 031214    2014031200001  AJRA  UPDATE TOT INT ON VOID
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
062217* 062217  CR2017050300002  TANA  ADD AUTH RCVD INDICATOR
022718* 022718  CR2017100200004  TANA  DONT UPDT ON HOLD UNTIL PMT
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
010719* 010719  IR2019010400001  PEMA  FIX VOID FOR ACH PAYMENT
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802*****************************************************************
00025                                                                   EL1501
00026  ENVIRONMENT DIVISION.                                            EL1501
00027                                                                   EL1501
00028      EJECT                                                        EL1501
00029  DATA DIVISION.                                                   EL1501
00030  WORKING-STORAGE SECTION.                                         EL1501
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL1501
00032  77  FILLER  PIC X(32)  VALUE '*   EL1501 WORKING STORAGE     *'. EL1501
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.030 *********'.    CL*30
061013 77  S1                          PIC S999 COMP-3 VALUE +0.
061013 77  S2                          PIC S999 COMP-3 VALUE +0.
       77  ws-max-bens                 pic s999 comp-3 value +0.
       77  ws-prev-days-paid           pic s9(5) comp-3 value +0.
       77  ws-prev-amt-paid            pic s9(9)v99 comp-3 value +0.
       77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
       77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
       77  ws-pd-bens                  pic s9(5) comp-3 value +0.
00034                                                                   EL1501
00035                                      COPY ELCSCTM.                   CL*13
00036                                                                   EL1501
00037                                      COPY ELCSCRTY.                  CL*13
00038                                                                   EL1501
00039  EJECT                                                            EL1501
00040  01  WS-DATE-AREA.                                                EL1501
00041      12  SAVE-DATE                   PIC X(8)    VALUE SPACES.       CL*28
00042      12  SAVE-DATE-CCYYMMDD.                                         CL*21
00043          16  SAVE-DATE-CC            PIC XX      VALUE SPACES.       CL*28
00044          16  SAVE-DATE-YMD.                                          CL*21
00045              20  SAVE-DATE-YY        PIC XX      VALUE SPACES.       CL*28
00046              20  FILLER              PIC X(4)    VALUE SPACES.       CL*28
00047      12  SAVE-BIN-DATE               PIC XX      VALUE SPACES.       CL*28
00048                                                                   EL1501
061013 01  ws-save-error-interface-block pic x(400) value low-values.

00049  01  STANDARD-AREAS.                                              EL1501
061013     12  WS-RESPONSE         PIC S9(8)   COMP.
061013         88  RESP-NORMAL              VALUE +00.
061013         88  RESP-ERROR               VALUE +01.
061013         88  RESP-NOTFND              VALUE +13.
061013         88  RESP-DUPREC              VALUE +14.
061013         88  RESP-ENDFILE             VALUE +20.

00050      12  GETMAIN-SPACE               PIC X       VALUE SPACE.        CL*28
00051      12  MAP-NAME                    PIC X(8)    VALUE 'EL150B'.     CL*28
00052      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL1501S'.    CL*28
00053      12  TRANS-ID                    PIC X(4)    VALUE 'EXH9'.       CL*28
00054      12  THIS-PGM                    PIC X(8)    VALUE 'EL1501'.     CL*28
00055      12  PGM-NAME                    PIC X(8).                       CL*28
00056      12  TIME-IN                     PIC S9(7).                      CL*28
00057      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL1501
00058          16  FILLER                  PIC X.                          CL*28
00059          16  TIME-OUT                PIC 99V99.                   EL1501
00060          16  FILLER                  PIC XX.                         CL*28
00061      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.      CL*28
00062      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.      CL*28
00063      12  XCTL-126                    PIC X(8)    VALUE 'EL126'.      CL*28
00064      12  XCTL-142                    PIC X(8)    VALUE 'EL142'.      CL*28
00065      12  LINK-001                    PIC X(8)    VALUE 'EL001'.      CL*28
00066      12  LINK-004                    PIC X(8)    VALUE 'EL004'.      CL*28
00067      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.    CL*28
00068      12  FILE-ID                     PIC X(8).                       CL*28
00069      12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.   CL*28
CIDMOD     12  WS-BLANK                    PIC X       VALUE ' '.            000
00070                                                                   EL1501
00071  01  MISC-WORK-AREAS.                                             EL1501
           12  ws-email-return-cd      pic s9(8) comp-5 value +0.
           12  ws-email-string.
               16  f                   pic x(28) value
                'smtp -f slunikix -t pema -s '.
               16  f                   pic x(29) value
                '''ACH Void Alert for Company '.
               16  ws-email-client-id  pic xxx value spaces.
               16  f                   pic x(7) value ''' -ml '.
               16  f                   pic x(53) value
                '''A void on an ACH payment has just been performed.  '.
               16  f                   pic x(14) value
                ' Claim Number '.
               16  ws-email-claim-no   pic x(10).
               16  f                   pic x(14) value
                ' Check Number '.
               16  ws-email-check-no   pic x(10).
               16  f                   pic x(7) value
                ' Payee '.
               16  ws-email-payee      pic x(30).
               16  f                   pic x(14) value
                ' Check Amount '.
               16  ws-email-check-amt  pic zzz,zz9.99.
               16  f                   pic xxx  value ''''.
               16  f                   pic x value low-values.

           12  ws-curl-return-cd       pic s9(8) comp-5 value +0.
           12  ws-curl-string.
               16  f                   pic x(19) value
                'curl --data "USZip='.
               16  ws-curl-zip         pic x(5) value spaces.
               16  f                   pic xx value '" '.
               16  f                   pic x(46) value
                'http://webservicex.net/uszip.asmx/GetInfoByZIP'.

00072      12  WS-CK-Q-CONTROL             PIC S9(8) COMP.                 CL*30
00073      12  W-CALLED-NAME               PIC X(8).                       CL*28
00074      12  QID.                                                     EL1501
00075          16  QID-TERM                PIC X(4).                       CL*28
00076          16  FILLER                  PIC X(4)    VALUE '150B'.       CL*28
00077      12  MAP-LENGTH                  PIC S9(4)   VALUE +1920 COMP.   CL*28
00078      12  PASS-SWITCH                 PIC X       VALUE 'A'.          CL*28
00079      12  DISPLAY-CNT                 PIC S9(4)   VALUE +1    COMP.   CL*28
00080      12  FILE-SWITCH                 PIC X(4)    VALUE SPACES.       CL*28
00081      12  WS-SUB                      PIC 9       VALUE 0.            CL*28
00082      12  SUB                         PIC 9       VALUE 1.            CL*28
00083      12  SUB-1                       PIC 9       VALUE 1.            CL*28
00084      12  RETURNED-FROM               PIC X(8)    VALUE SPACES.       CL*28
00085      12  DIRECTION-SWITCH            PIC X       VALUE 'N'.          CL*28
00086      12  WS-RECORDS-READ-SW          PIC X       VALUE 'N'.          CL*28
00087          88  RECORDS-READ                        VALUE 'Y'.       EL1501
00088      12  SAVE-CONTROL                PIC X(39).                   EL1501
00089      12  WS-RECEIVED-DATE            PIC XX      VALUE LOW-VALUES.   CL*28
00090      12  WS-CHECK-WRITTEN-DT         PIC XX      VALUE LOW-VALUES.   CL*28
00091      12  WS-PMT-APPROVAL-SW          PIC X       VALUE SPACE.        CL*28
00092      12  WS-CF-PMT-APPROVAL-SW       PIC X       VALUE SPACE.        CL*28
00093          88  WS-CF-PMT-APPROVAL-USED             VALUE 'Y' 'G'.      CL*17
00094          88  WS-CF-NO-APPROVAL                   VALUE ' ' 'N'.      CL*17
00095          88  WS-CF-ALL-APPROVED                  VALUE 'Y'.          CL*17
00096          88  WS-CF-GRADUATED-APPROVAL            VALUE 'G'.          CL*17
00097                                                                      CL*17
00098      12  WS-CV-PMT-CODE              PIC X       VALUE SPACE.        CL*28
00099      12  WS-PAY-TYPE                 PIC X       VALUE SPACE.        CL*28
00100      12  WS-AMOUNT-PAID              PIC S9(7)V99 VALUE ZEROS.    EL1501
00101      12  WS-PAYMENT-ORIGIN           PIC X       VALUE SPACE.        CL*28
00102      12  WS-RECON-SW                 PIC X       VALUE ' '.          CL*28
00103          88  RECON-RCD-REDEEMED                  VALUE 'R'.          CL*18
00104          88  RECON-RCD-NOT-FOUND                 VALUE 'X'.          CL*18
00105                                                                   EL1501
00106      12  WS-DEEDIT-LENGTH            PIC S9(4)   VALUE +16   COMP.   CL*28
00107      12  WS-DEEDIT-FIELD             PIC X(16)   VALUE ZEROS.        CL*19
00108      12  WS-DEEDIT-FIELD-V0 REDEFINES WS-DEEDIT-FIELD             EL1501
00109                                      PIC S9(16).                     CL*19
00110                                                                      CL*19
00111      12  WS-LF-COVERAGE-TYPE         PIC X       VALUE SPACE.        CL*28
00112      12  WS-BEN-SEARCH-SW            PIC X       VALUE 'N'.          CL*28
00113          88  BENEFIT-FOUND                       VALUE 'Y'.          CL**6
00114          88  NO-BENEFIT-FOUND                    VALUE 'N'.          CL**6
00115      12  WS-ACCESS.                                                  CL**6
00116          16  FILLER                  PIC XX      VALUE SPACES.       CL*28
00117          16  WS-BEN-CD               PIC XX      VALUE SPACES.       CL*28
00118                                                                      CL**8
00119      12  WS-PRINTED-SW               PIC X       VALUE 'N'.          CL*28
00120          88  PAYMENT-HAS-BEEN-PRINTED            VALUE 'Y'.          CL**8
00121          88  PAYMENT-NOT-PRINTED                 VALUE 'N'.          CL**8
00122                                                                      CL**8
00123      12  WS-RELEASED-SW              PIC X       VALUE 'N'.          CL*28
00124          88  PAYMENT-NOT-RELEASED                VALUE 'N'.          CL**8
00125                                                                      CL**8
00126      12  WS-UPDATE-SW                PIC X       VALUE 'N'.          CL*28
00127      12  WS-VOID-CODE                PIC X       VALUE ' '.          CL*28
00128                                                                      CL*18
00129      12  WS-WORK-DATE.                                               CL*18
00130          16  WS-WORK-MM              PIC 99      VALUE ZEROS.        CL*28
00131          16  WS-WORK-DD              PIC 99      VALUE ZEROS.        CL*28
00132          16  WS-WORK-YY              PIC 99      VALUE ZEROS.        CL*28
00133                                                                      CL*18
00134      12  WS-RCON-DATE.                                               CL*18
00135          16  WS-RCON-YEAR.                                           CL*18
00136              20  WS-RCON-YY-1        PIC 99.                         CL*28
00137              20  WS-RCON-YY-2        PIC 99.                         CL*28
00138          16  WS-RCON-MM              PIC 99.                         CL*28
00139          16  WS-RCON-DD              PIC 99.                         CL*28
00140      12  W-NAME-LAST             PIC  X(15).                         CL*21
00141      12  W-NAME-FIRST            PIC  X(15).                         CL*21
00142      12  W-NAME-MIDDLE.                                              CL*21
00143          16  FILLER              PIC  X.                             CL*28
00144          16  W-NAME-MIDDLE-2     PIC  X.                             CL*28
00145          16  FILLER              PIC  X(13).                         CL*21
00146                                                                   EL1501
00147      12  WS-DMO-LENGTH           PIC S9(4)   VALUE +108 COMP.        CL*22
00148      12  WS-DCT-LENGTH           PIC S9(4)   VALUE +53 COMP.         CL*22
CIDMOD 01  CSO-WORK-FIELDS.                                                  000
CIDMOD     05  ERROR-ON-OUTPUT-SW          PIC X       VALUE 'N'.            000
CIDMOD       88  ERROR-ON-OUTPUT                       VALUE 'Y'.            000
CIDMOD EJECT                                                            EL1501
061013 01  ELCRTT-KEY.                                              
061013     05  CTRLR-COMP-CD       PIC X.                               
061013     05  CTRLR-CARRIER       PIC X.                               
061013     05  CTRLR-GROUPING      PIC X(6).                            
061013     05  CTRLR-STATE         PIC X(2).                            
061013     05  CTRLR-ACCOUNT       PIC X(10).
061013     05  CTRLR-EFF-DT        PIC XX.                              
061013     05  CTRLR-CERT-NO       PIC X(11).  
061013     05  CTRLR-REC-TYPE      PIC X.

00150  01  ACCESS-KEYS.                                                 EL1501
00151      12  ELMSTR-KEY.                                              EL1501
00152          16  MSTR-COMP-CD            PIC X.                          CL*28
00153          16  MSTR-CARRIER            PIC X.                          CL*28
00154          16  MSTR-CLAIM-NO           PIC X(7).                       CL*28
00155          16  MSTR-CERT-NO.                                        EL1501
00156              20  MSTR-CERT-NO-PRIME  PIC X(10).                   EL1501
00157              20  MSTR-CERT-NO-SUFX   PIC X.                          CL*28
00158      12  ELCNTL-KEY.                                              EL1501
00159          16  CNTL-COMP-ID            PIC X(3).                       CL*28
00160          16  CNTL-REC-TYPE           PIC X.                          CL*28
00161          16  CNTL-ACCESS             PIC X(4).                       CL*28
00162          16  CNTL-SEQ-NO             PIC S9(4)     COMP.             CL*28
00163      12  ELCERT-KEY.                                              EL1501
00164          16  CERT-COMP-CD            PIC X.                          CL*28
00165          16  CERT-CARRIER            PIC X.                          CL*28
00166          16  CERT-GROUPING           PIC X(6).                       CL*28
00167          16  CERT-STATE              PIC XX.                         CL*28
00168          16  CERT-ACCOUNT            PIC X(10).                   EL1501
00169          16  CERT-EFF-DT             PIC XX.                         CL*28
00170          16  CERT-CERT-NO.                                        EL1501
00171              20  CERT-CERT-NO-PRIME  PIC X(10).                   EL1501
00172              20  CERT-CERT-NO-SUFX   PIC X.                          CL*28
00173      12  ELTRLR-KEY.                                              EL1501
00174          16  TRLR-COMP-CD            PIC X.                          CL*28
00175          16  TRLR-CARRIER            PIC X.                          CL*28
00176          16  TRLR-CLAIM-NO           PIC X(7).                       CL*28
00177          16  TRLR-CERT-NO.                                        EL1501
00178              20  TRLR-CERT-NO-PRIME  PIC X(10).                   EL1501
00179              20  TRLR-CERT-NO-SUFX   PIC X.                          CL*28
00180          16  TRLR-SEQ-NO             PIC S9(4)   COMP.               CL*28
00181      12  ELACTQ-KEY.                                              EL1501
00182          16  ACTQ-COMP-CD            PIC X.                          CL*28
00183          16  ACTQ-CARRIER            PIC X.                          CL*28
00184          16  ACTQ-CLAIM-NO           PIC X(7).                       CL*28
00185          16  ACTQ-CERT-NO.                                        EL1501
00186              20  ACTQ-CERT-NO-PRIME  PIC X(10).                   EL1501
00187              20  ACTQ-CERT-NO-SUFX   PIC X.                          CL*28
00188      12  ELCHKQ-KEY.                                              EL1501
00189          16  CHKQ-COMP-CD            PIC X.                          CL*28
00190          16  CHKQ-CONTROL            PIC S9(8)   COMP.               CL*28
00191          16  CHKQ-SEQ-NO             PIC S9(4)   COMP.               CL*28
00192      12  ELRCON-KEY.                                                 CL*18
00193          16  RCON-COMPANY-CD         PIC X.                          CL*28
00194          16  RCON-CHECK-NO           PIC X(7).                       CL*28
00195          16  RCON-CHECK-ORIGIN       PIC X.                          CL*28
00196          16  RCON-GL-ACCOUNT-NO      PIC X(10).                      CL*18
00197      12  EMPLCY-KEY.                                                 CL*19
00198          16  PLCY-COMPANY-CD         PIC X.                          CL*28
00199          16  PLCY-CARRIER            PIC X.                          CL*28
00200          16  PLCY-GROUPING           PIC X(6).                       CL*28
00201          16  PLCY-STATE              PIC XX.                         CL*28
00202          16  PLCY-PRODUCER           PIC X(10).                      CL*19
00203          16  PLCY-EFF-DT             PIC XX.                         CL*28
00204          16  PLCY-REFERENCE-NO       PIC X(20).                      CL*19
00205      12  W-NOTE-KEY.                                                 CL*21
00206          16  W-NOTE-COMP-CD      PIC X.                              CL*21
00207          16  W-NOTE-CERT-KEY.                                        CL*21
00208              20  W-NOTE-CARRIER  PIC X.                              CL*21
00209              20  W-NOTE-GROUPING PIC X(6).                           CL*21
00210              20  W-NOTE-STATE    PIC X(2).                           CL*21
00211              20  W-NOTE-ACCOUNT  PIC X(10).                          CL*21
00212              20  W-NOTE-EFF-DT   PIC XX.                             CL*21
00213              20  W-NOTE-CERT-NO  PIC X(11).                          CL*21
00214                                                                   EL1501
00215      EJECT                                                        EL1501
00216  01  ERROR-MESSAGES.                                              EL1501
00217      12  ER-0000                     PIC X(4)    VALUE '0000'.       CL*28
00218      12  ER-0004                     PIC X(4)    VALUE '0004'.       CL*28
00219      12  ER-0008                     PIC X(4)    VALUE '0008'.       CL*28
00220      12  ER-0029                     PIC X(4)    VALUE '0029'.       CL*28
00221      12  ER-0033                     PIC X(4)    VALUE '0033'.       CL*28
00222      12  ER-0042                     PIC X(4)    VALUE '0042'.       CL*28
00223      12  ER-0068                     PIC X(4)    VALUE '0068'.       CL*28
00224      12  ER-0070                     PIC X(4)    VALUE '0070'.       CL*28
00225      12  ER-0130                     PIC X(4)    VALUE '0130'.       CL*28
00226      12  ER-0154                     PIC X(4)    VALUE '0154'.       CL*28
00227      12  ER-0169                     PIC X(4)    VALUE '0169'.       CL*28
00228      12  ER-0172                     PIC X(4)    VALUE '0172'.       CL*28
00229      12  ER-0190                     PIC X(4)    VALUE '0190'.       CL*28
00230      12  ER-0204                     PIC X(4)    VALUE '0204'.       CL*28
00231      12  ER-0206                     PIC X(4)    VALUE '0206'.       CL*28
00232      12  ER-0282                     PIC X(4)    VALUE '0282'.       CL*28
00233      12  ER-0303                     PIC X(4)    VALUE '0303'.       CL*28
00234      12  ER-0334                     PIC X(4)    VALUE '0334'.       CL*28
00235      12  ER-0335                     PIC X(4)    VALUE '0335'.       CL*28
00236      12  ER-0336                     PIC X(4)    VALUE '0336'.       CL*28
00237      12  ER-0337                     PIC X(4)    VALUE '0337'.       CL*28
00238      12  ER-0338                     PIC X(4)    VALUE '0338'.       CL*28
00239      12  ER-0376                     PIC X(4)    VALUE '0376'.       CL*28
00240      12  ER-0412                     PIC X(4)    VALUE '0412'.       CL*28
00241      12  ER-0413                     PIC X(4)    VALUE '0413'.       CL*28
00242      12  ER-0660                     PIC X(4)    VALUE '0660'.       CL*28
00243      12  ER-0661                     PIC X(4)    VALUE '0661'.       CL*28
00244      12  ER-0662                     PIC X(4)    VALUE '0662'.       CL*28
00245      12  ER-0663                     PIC X(4)    VALUE '0663'.       CL*28
00246      12  ER-0664                     PIC X(4)    VALUE '0664'.       CL*28
00247      12  ER-0665                     PIC X(4)    VALUE '0665'.       CL*28
00248      12  ER-0666                     PIC X(4)    VALUE '0666'.       CL*28
00249      12  ER-0667                     PIC X(4)    VALUE '0667'.       CL*28
00250      12  ER-0672                     PIC X(4)    VALUE '0672'.       CL*29
00251      12  ER-0776                     PIC X(4)    VALUE '0776'.       CL*28
00252      12  ER-0800                     PIC X(4)    VALUE '0800'.       CL*28
00253      12  ER-0801                     PIC X(4)    VALUE '0801'.       CL*28
00254      12  ER-0816                     PIC X(4)    VALUE '0816'.       CL*28
00255      12  ER-0823                     PIC X(4)    VALUE '0823'.       CL*28
00256      12  ER-0833                     PIC X(4)    VALUE '0833'.       CL*28
00257      12  ER-0835                     PIC X(4)    VALUE '0835'.       CL*28
00258      12  ER-0849                     PIC X(4)    VALUE '0849'.       CL*28
00259      12  ER-0919                     PIC X(4)    VALUE '0919'.       CL*28
00260      12  ER-0920                     PIC X(4)    VALUE '0920'.       CL*28
00261      12  ER-0921                     PIC X(4)    VALUE '0921'.       CL*28
00262      12  ER-0922                     PIC X(4)    VALUE '0922'.       CL*28
00263      12  ER-0923                     PIC X(4)    VALUE '0923'.       CL*28
00264      12  ER-0925                     PIC X(4)    VALUE '0925'.       CL*28
00265      12  ER-0939                     PIC X(4)    VALUE '0939'.       CL*28
00266      12  ER-0940                     PIC X(4)    VALUE '0940'.       CL*28
00267      12  ER-0941                     PIC X(4)    VALUE '0941'.       CL*28
00268      12  ER-0942                     PIC X(4)    VALUE '0942'.       CL*28
00269      12  ER-0946                     PIC X(4)    VALUE '0946'.       CL*28
00270      12  ER-0947                     PIC X(4)    VALUE '0947'.       CL*28
00271      12  ER-0948                     PIC X(4)    VALUE '0948'.       CL*28
00272      12  ER-0949                     PIC X(4)    VALUE '0949'.       CL*28
00273      12  ER-0950                     PIC X(4)    VALUE '0950'.       CL*28
00274      12  ER-0951                     PIC X(4)    VALUE '0951'.       CL*28
00275      12  ER-0954                     PIC X(4)    VALUE '0954'.       CL*28
00276      12  ER-0974                     PIC X(4)    VALUE '0974'.       CL*28
00277      12  ER-0975                     PIC X(4)    VALUE '0975'.       CL*28
00278      12  ER-2378                     PIC X(4)    VALUE '2378'.       CL*28
00279      12  ER-2379                     PIC X(4)    VALUE '2379'.       CL*28
040913     12  ER-2893                     PIC X(4)    VALUE '2893'.
00280      12  ER-7999                     PIC X(4)    VALUE '7999'.       CL*28
062602     12  ER-8003                     PIC X(4)    VALUE '8003'.       CL*28
00281      12  ER-8051                     PIC X(4)    VALUE '8051'.       CL*28
00282      12  ER-8052                     PIC X(4)    VALUE '8052'.       CL*28
00283      12  ER-8053                     PIC X(4)    VALUE '8053'.       CL*28
00284      12  ER-8054                     PIC X(4)    VALUE '8054'.       CL*28
00285      12  ER-8055                     PIC X(4)    VALUE '8055'.       CL*28
00286      12  ER-8056                     PIC X(4)    VALUE '8056'.       CL*28
00287      12  ER-8057                     PIC X(4)    VALUE '8057'.       CL*28
00288      12  ER-8058                     PIC X(4)    VALUE '8058'.       CL*28
00289      12  ER-8059                     PIC X(4)    VALUE '8059'.       CL*28
00290      12  ER-8060                     PIC X(4)    VALUE '8060'.       CL*28
00291      12  ER-8061                     PIC X(4)    VALUE '8061'.       CL*28
00292      12  ER-8062                     PIC X(4)    VALUE '8062'.       CL*28
00293      12  ER-8063                     PIC X(4)    VALUE '8063'.       CL*28
00294      12  ER-8064                     PIC X(4)    VALUE '8064'.       CL*28
00295      12  ER-8065                     PIC X(4)    VALUE '8065'.       CL*28
00296      12  ER-8066                     PIC X(4)    VALUE '8066'.       CL*28
00297      12  ER-8152                     PIC X(4)    VALUE '8152'.       CL*28
00298      12  ER-8153                     PIC X(4)    VALUE '8153'.       CL*28
00299      12  ER-8154                     PIC X(4)    VALUE '8154'.       CL*28
00300      12  ER-8155                     PIC X(4)    VALUE '8155'.
013017     12  er-8162                     pic x(4)    value '8162'.
00301      12  ER-9211                     PIC X(4)    VALUE '9211'.       CL*28
00302      12  ER-9883                     PIC X(4)    VALUE '9883'.       CL*28
00303                                                                   EL1501
00304  EJECT                                                            EL1501
00305  01  TEXT-WORK-AREAS.                                             EL1501
00306      12  PAYMENT-LINE-1.                                          EL1501
00307          16  PMT-HDG1.                                            EL1501
00308              20  PMT-LINE-NO         PIC X.                          CL*28
00309              20  FILLER              PIC X.                          CL*28
00310              20  PMT-HDG1-LIT        PIC X(8).                       CL*28
00311          16  PMT-TEXT-1.                                          EL1501
00312              20  PMT-TYPE-LIT        PIC X(6).                       CL*28
00313              20  PMT-TYPE            PIC X(7).                       CL*28
00314              20  PMT-CHECK-NO-LIT    PIC X(11).                   EL1501
00315              20  PMT-CHECK-NO        PIC X(7).                       CL*28
00316              20  PMT-DT-WRITTEN-LIT  PIC X(14).                      CL*16
00317              20  PMT-DT-WRITTEN      PIC X(8).                       CL*28
00318              20  PMT-AMT-PD-LIT      PIC X(5).                       CL*28
00319              20  PMT-AMT-PAID        PIC Z(6).99-.                   CL*19
00320      12  PAYMENT-LINE-2.                                          EL1501
00321          16  PMT-HDG2.                                            EL1501
00322              20  FILLER              PIC X(6).                       CL*28
00323              20  PMT-HDG2-LIT        PIC X(4).                       CL*28
00324          16  PMT-TEXT-2.                                          EL1501
00325              20  PMT-FROM-LIT        PIC X(6).                       CL*28
00326              20  PMT-PAID-FROM       PIC X(8).                       CL*28
00327              20  PMT-THRU-LIT        PIC X(10).                   EL1501
00328              20  PMT-PAID-THRU       PIC X(8).                       CL*28
00329              20  PMT-VOID-LIT        PIC X(13).                      CL*16
00330              20  PMT-VOID-DT         PIC X(8).                       CL*28
00331              20  PMT-PAYEE-LIT       PIC X(8).                       CL*28
00332              20  PMT-PAYEE           PIC X(7).                       CL*28
00333      12  AUTO-PMT-LINE-1.                                         EL1501
00334          16  AUTO-PMT-HDG1.                                       EL1501
00335              20  AUTO-LINE-NO        PIC X.                          CL*28
00336              20  FILLER              PIC X.                          CL*28
00337              20  AUTO-HDG1-LIT       PIC X(8).                       CL*28
00338          16  AUTO-PMT-TEXT1.                                      EL1501
00339              20  AUTO-EFF-DT-LIT     PIC X(6).                       CL*28
00340              20  AUTO-EFF-DT         PIC X(8).                       CL*28
00341              20  AUTO-1ST-AMT-LIT    PIC X(10).                   EL1501
00342              20  AUTO-1ST-AMT        PIC Z(5).99.                    CL*28
00343              20  AUTO-REG-AMT-LIT    PIC X(14).                   EL1501
00344              20  AUTO-REG-AMT        PIC Z(5).99.                    CL*28
00345              20  AUTO-PMT-STATUS-LIT PIC X(8).                       CL*28
00346              20  AUTO-PMT-STATUS     PIC X(6).                       CL*28
00347      12  AUTO-PMT-LINE-2.                                         EL1501
00348          16  AUTO-PMT-HDG2.                                       EL1501
00349              20  FILLER              PIC X(6).                       CL*28
00350              20  AUTO-HDG2-LIT       PIC X(4).                       CL*28
00351          16  AUTO-PMT-TEXT2.                                      EL1501
00352              20  AUTO-PAYEE-LIT      PIC X(6).                       CL*28
00353              20  AUTO-PAYEE          PIC X(8).                       CL*28
00354              20  AUTO-1ST-PMT-LIT    PIC X(10).                   EL1501
00355              20  AUTO-1ST-PMT-DT     PIC X(8).                       CL*28
00356              20  AUTO-LST-PMT-LIT    PIC X(14).                   EL1501
00357              20  AUTO-LST-PMT-DT     PIC X(8).                       CL*28
00358              20  FILLER              PIC X(4).                       CL*28
00359      12  CORRESPONDENCE-LINE-1.                                   EL1501
00360          16  CORR-HDG1.                                           EL1501
00361              20  CORR-LINE-NO        PIC X.                          CL*28
00362              20  FILLER              PIC X.                          CL*28
00363              20  CORR-HDG1-LIT       PIC X(8).                       CL*28
00364          16  CORR-TEXT-1.                                         EL1501
00365              20  CORR-FORM-LIT       PIC X(6).                       CL*28
00366              20  CORR-FORM-TYPE      PIC X(4).                       CL*28
071210*             20  FILLER              PIC X(4).                       CL*28
00368              20  CORR-DT-SENT-LIT    PIC X(10).                   EL1501
00369              20  CORR-DT-SENT        PIC X(8).                       CL*28
071210             20  CORR-INIT-PRT-LIT   PIC X(11).                   EL1501
00371              20  CORR-INIT-PRT-DT    PIC X(8).                       CL*28
071210             20  CORR-ADDR-LIT       PIC X(05).                   EL1501
00373              20  CORR-ADDR-TYPE      PIC XX.                         CL*28
071210             20  CORR-LET-TO-BEN     PIC X(14).
00374      12  CORRESPONDENCE-LINE-2.                                   EL1501
00375          16  CORR-HDG2.                                           EL1501
00376              20  FILLER              PIC X(6).                       CL*28
00377              20  CORR-HDG2-LIT       PIC X(4).                       CL*28
00378          16  CORR-TEXT-2.                                         EL1501
00379              20  CORR-RESEND-LIT     PIC X(6).                       CL*28
00380              20  CORR-RESEND-DT      PIC X(8).                       CL*28
00381              20  CORR-RECVD-LIT      PIC X(10).                   EL1501
00382              20  CORR-RECVD-DT       PIC X(8).                       CL*28
00383              20  CORR-FOLLOW-UP-LIT  PIC X(14).                   EL1501
00384              20  CORR-FOLLOW-UP-DT   PIC X(8).                       CL*28
00385              20  CORR-ARCH-LIT       PIC X(6).                       CL*28
00386              20  CORR-ARCH-NO        PIC 9(8).                       CL*28
00387 *            20  FILLER              PIC X(14).                      CL*18
00388      12  FORM-LINE-1.                                             EL1501
00389          16  FORM-HDG1.                                           EL1501
00390              20  FORM-LINE-NO        PIC X.                          CL*28
00391              20  FILLER              PIC X.                          CL*28
00392              20  FORM-HDG1-LIT       PIC X(8).                       CL*28
00393          16  FORM-TEXT-1.                                         EL1501
00394              20  FORM-TYPE-LIT       PIC X(6).                       CL*28
00395              20  FORM-TYPE           PIC X(4).                       CL*28
00396              20  FILLER              PIC X(4).                       CL*28
00397              20  FORM-SEND-ON-LIT    PIC X(10).                   EL1501
00398              20  FORM-SEND-ON-DT     PIC X(8).                       CL*28
00399              20  FORM-RESEND-LIT     PIC X(14).                   EL1501
00400              20  FORM-RESEND-DT      PIC X(8).                       CL*28
00401              20  FORM-FOLLOW-UP-LIT  PIC X(6).                       CL*28
00402              20  FORM-FOLLOW-UP-DT   PIC X(8).                       CL*28
00403      12  FORM-LINE-2.                                             EL1501
00404          16  FORM-HDG2.                                           EL1501
00405              20  FILLER              PIC X(6).                       CL*28
00406              20  FORM-HDG2-LIT       PIC X(4).                       CL*28
00407          16  FORM-TEXT-2.                                         EL1501
00408              20  FORM-REC-INS-LIT    PIC X(6).                       CL*28
00409              20  FORM-REC-INS-DT     PIC X(8).                       CL*28
00410              20  FORM-REC-PHY-LIT    PIC X(10).                   EL1501
00411              20  FORM-REC-PHY-DT     PIC X(8).                       CL*28
00412              20  FORM-REC-EMP-LIT    PIC X(14).                   EL1501
00413              20  FORM-REC-EMP-DT     PIC X(8).                       CL*28
00414              20  FILLER              PIC X(12).                   EL1501
00415      12  INCUR-CHG-LINE-1.                                        EL1501
00416          16  INCUR-CHG-HDG1.                                      EL1501
00417              20  INCUR-LINE-NO       PIC X.                          CL*28
00418              20  FILLER              PIC X.                          CL*28
00419              20  INCUR-HDG1-LIT      PIC X(8).                       CL*28
00420          16  INCUR-TEXT-1.                                        EL1501
00421              20  INCUR-INCUR-LIT     PIC X(6).                       CL*28
00422              20  INCUR-INCUR-DT      PIC X(8).                       CL*28
00423              20  INCUR-REPORT-LIT    PIC X(10).                   EL1501
00424              20  INCUR-REPORT-DT     PIC X(8).                       CL*28
00425              20  INCUR-ESTAB-LIT     PIC X(14).                   EL1501
00426              20  INCUR-ESTAB-DT      PIC X(8).                       CL*28
00427              20  INCUR-NO-PMTS-LIT   PIC X(11).                   EL1501
00428              20  INCUR-NO-PMTS       PIC ZZ9.                     EL1501
00429      12  INCUR-CHG-LINE-2.                                        EL1501
00430          16  INCUR-CHG-HDG2.                                      EL1501
00431              20  FILLER              PIC X(6).                       CL*28
00432              20  INCUR-HDG2-LIT      PIC X(4).                       CL*28
00433          16  INCUR-TEXT-2.                                        EL1501
00434              20  INCUR-PD-THRU-LIT   PIC X(6).                       CL*28
00435              20  INCUR-PD-THRU-DT    PIC X(8).                       CL*28
00436              20  INCUR-TOT-PD-LIT    PIC X(10).                   EL1501
00437              20  INCUR-TOT-PD        PIC Z(5).99.                    CL*28
00438              20  INCUR-TOT-DAYS-LIT  PIC X(14).                   EL1501
00439              20  INCUR-TOT-DAYS-PD   PIC ZZ9.                     EL1501
00440              20  FILLER              PIC X(19).                   EL1501
00441      12  DENIAL-LINE-1.                                           EL1501
00442          16  DENIAL-HDG1.                                         EL1501
00443              20  DENIAL-LINE-NO      PIC X.                          CL*28
00444              20  FILLER              PIC X.                          CL*28
00445              20  DENIAL-HDG1-LIT     PIC X(8).                       CL*28
00446          16  DENIAL-TEXT-1.                                       EL1501
00447              20  DENIAL-LN1-LIT      PIC X(6).                       CL*28
00448              20  DENIAL-LN1          PIC X(62).                   EL1501
00449      12  DENIAL-LINE-2.                                           EL1501
00450          16  DENIAL-HDG2.                                         EL1501
00451              20  FILLER              PIC X(10).                   EL1501
00452          16  DENIAL-TEXT-2.                                       EL1501
00453              20  DENIAL-LN2-LIT      PIC X(6).                       CL*28
00454              20  DENIAL-LN2          PIC X(62).                   EL1501
00455      12  GEN-INFO-LINE-1.                                         EL1501
00456          16  GEN-INFO-HDG1.                                       EL1501
00457              20  GI-LINE-NO          PIC X.                          CL*28
00458              20  FILLER              PIC X.                          CL*28
00459              20  GI-HDG1-LIT         PIC X(8).                       CL*28
00460          16  GEN-INFO-TEXT-1.                                     EL1501
00461              20  GEN-INFO-MSG-1-LIT  PIC X(6).                       CL*28
00462              20  GEN-INFO-MSG-1      PIC X(62).                   EL1501
00463      12  GEN-INFO-LINE-2.                                         EL1501
00464          16  GEN-INFO-HDG2.                                       EL1501
00465              20  FILLER              PIC XX.                         CL*28
00466              20  GI-HDG2-LIT         PIC X(8).                       CL*28
00467          16  GEN-INFO-TEXT-2.                                     EL1501
00468              20  GEN-INFO-MSG-2-LIT  PIC X(6).                       CL*28
00469              20  GEN-INFO-MSG-2      PIC X(62).                   EL1501
00470      12  REMINDER-LINE-1.                                         EL1501
00471          16  REMINDER-HDG1.                                       EL1501
00472              20  REM-LINE-NO         PIC X.                          CL*28
00473              20  FILLER              PIC X.                          CL*28
00474              20  REM-HDG1-LIT        PIC X(8).                       CL*28
00475          16  REMINDER-TEXT-1.                                     EL1501
00476              20  REM-LINE-1-LIT      PIC X(6).                       CL*28
00477              20  REM-LINE-1          PIC X(62).                   EL1501
00478      12  REMINDER-LINE-2.                                         EL1501
00479          16  REMINDER-HDG2.                                       EL1501
00480              20  FILLER              PIC X(10).                   EL1501
00481          16  REMINDER-TEXT-2.                                     EL1501
00482              20  REM-LINE-2-LIT      PIC X(6).                       CL*28
00483              20  REM-LINE-2          PIC X(62).                   EL1501
00484                                                                   EL1501
00485  01  PAYMENT-DESCRIPTION-TABLE.                                   EL1501
00486      12  FILLER                      PIC X(8)    VALUE 'PARTIAL '.   CL*28
00487      12  FILLER                      PIC X(8)    VALUE 'FINAL   '.   CL*28
00488      12  FILLER                      PIC X(8)    VALUE 'LUMP SUM'.   CL*28
00489      12  FILLER                      PIC X(8)    VALUE 'ADDITION'.   CL*28
00490      12  FILLER                      PIC X(8)    VALUE 'CHRG EXP'.   CL*28
00491      12  FILLER                      PIC X(8)    VALUE 'NON-CHG '.   CL*28
00492      12  FILLER                      PIC X(8)    VALUE 'LF PRM  '.   CL*28
00493      12  FILLER                      PIC X(8)    VALUE 'A/H PRM '.   CL*28
00494      12  FILLER                      PIC X(8)    VALUE 'ENT CORR'.   CL*28
00495  01  PAYMENT-DESC-R   REDEFINES PAYMENT-DESCRIPTION-TABLE.        EL1501
00496      12  PAY-DESC                    PIC X(8)    OCCURS 2.           CL*28
00497                                                                   EL1501
00498  01  CV-PAYMENT-DESCRIPTION-TABLE.                                   CL*19
00499      12  FILLER                      PIC X(7)    VALUE 'FUL DTH'.    CL*28
00500      12  FILLER                      PIC X(7)    VALUE 'HLF DTH'.    CL*28
00501      12  FILLER                      PIC X(7)    VALUE 'FUL ADD'.    CL*28
00502      12  FILLER                      PIC X(7)    VALUE 'HLF ADD'.    CL*28
00503      12  FILLER                      PIC X(7)    VALUE 'FUL RID'.    CL*28
00504      12  FILLER                      PIC X(7)    VALUE 'HLF RID'.    CL*28
00505      12  FILLER                      PIC X(7)    VALUE 'NON-CHG'.    CL*28
00506      12  FILLER                      PIC X(7)    VALUE 'ADDL   '.    CL*28
00507  01  CV-PAYMENT-DESC-R REDEFINES CV-PAYMENT-DESCRIPTION-TABLE.       CL*19
00508      12  CV-PAY-DESC                 PIC X(7)    OCCURS 2.           CL*28
00509                                                                      CL*19
00510      EJECT                                                           CL*21
00511                                      COPY ELCDATE.                   CL*13
00512      EJECT                                                        EL1501
00513                                      COPY ELCLOGOF.                  CL*13
00514      EJECT                                                        EL1501
00515                                      COPY ELCATTR.                   CL*13
00516      EJECT                                                        EL1501
121802*                                    COPY ELCDCTB.                   CL*21
121802*    EJECT                                                           CL*21
121802*                                    COPY ELCDMO.                    CL*21
121802*    EJECT                                                           CL*21
00521                                      COPY ELCEMIB.                   CL*13
00522      EJECT                                                           CL*21
121802*                                    COPY ELCNWA.                    CL*21
121802*    EJECT                                                           CL*19
121802*                                    COPY MPCPOLUP.                  CL*19
121802*    EJECT                                                        EL1501
00527                              COPY ELCINTF.                           CL*13
00528      12  PI-REDEF    REDEFINES PI-PROGRAM-WORK-AREA.              EL1501
062602         16  FILLER                  PIC x(2).                       CL*28
062602         16  pi-el142-priority       pic x.
062602         16  filler                  pic x.
00530          16  PI-MAP-NAME             PIC X(8).                       CL*28
00531          16  PI-QUALIFICATION-SWITCHES  COMP-3.                   EL1501
00532              20  PI-REMINDERS-SW     PIC S9.                         CL*28
00533              20  PI-LETTERS-SW       PIC S9.                         CL*28
00534              20  PI-PAYMENTS-SW      PIC S9.                         CL*28
00535              20  PI-AUTO-PAY-SW      PIC S9.                         CL*28
00536              20  PI-NOTES-SW         PIC S9.                         CL*28
00537              20  PI-RES-EXP-SW       PIC S9.                         CL*28
00538              20  PI-DENIALS-SW       PIC S9.                         CL*28
00539              20  PI-INCURRED-DATE-SW PIC S9.                         CL*28
00540              20  PI-FORMS-SW         PIC S9.                         CL*28
00541          16  FILLER                  PIC X(8).                       CL*28
00542          16  PI-ACTIVITY-TRAILERS-KEY.                            EL1501
00543              20  PI-ATK-COMPANY-CODE PIC X.                          CL*28
00544              20  PI-ATK-CARRIER      PIC X.                          CL*28
00545              20  PI-ATK-CLAIM-NO     PIC X(7).                       CL*28
00546              20  PI-ATK-CERT-NO      PIC X(11).                   EL1501
00547              20  PI-ATK-SEQ-NO       PIC S9(4)     COMP.             CL*28
00548          16  PI-PREV-ACTIVITY-TRAILERS-KEY.                       EL1501
00549              20  PI-PREV-ATK-COMPANY-CODE PIC X.                     CL*28
00550              20  PI-PREV-ATK-CARRIER      PIC X.                     CL*28
00551              20  PI-PREV-ATK-CLAIM-NO     PIC X(7).                  CL*28
00552              20  PI-PREV-ATK-CERT-NO      PIC X(11).              EL1501
00553              20  PI-PREV-ATK-SEQ-NO       PIC S9(4)     COMP.        CL*28
00554          16  PI-SAVE-KEY.                                         EL1501
00555              20  PI-SAVE-ATK-COMPANY-CODE PIC X.                     CL*28
00556              20  PI-SAVE-ATK-CARRIER      PIC X.                     CL*28
00557              20  PI-SAVE-ATK-CLAIM-NO     PIC X(7).                  CL*28
00558              20  PI-SAVE-ATK-CERT-NO      PIC X(11).              EL1501
00559              20  PI-SAVE-ATK-SEQ-NO       PIC S9(4)     COMP.        CL*28
00560          16  PI-PREV-AID                  PIC X.                     CL*28
00561          16  PI-RECORD-COUNT              PIC S9        COMP-3.      CL*28
00562          16  PI-END-OF-FILE               PIC S9        COMP-3.      CL*28
00563          16  PI-SAVE-CURSOR               PIC S9(4)     COMP.        CL*28
00564          16  PI-PURGED-SW                 PIC X.                     CL*28
00565              88  CLAIM-IS-PURGED                 VALUE 'Y'.       EL1501
00566          16  PI-LINE-NO                   PIC 9.                     CL*28
00567          16  PI-PREV-SEQ-NO               PIC S9(4)   COMP.          CL*28
00568          16  PI-FIRST-TIME-SW             PIC X.                     CL*28
00569              88  FIRST-TIME                      VALUE 'Y'.       EL1501
00570          16  PI-PREV-DIRECTION            PIC X.                     CL*28
00571          16  PI-SEQ-NUMBERS.                                      EL1501
010719             20  PI-SEQ-NO-TABLE OCCURS 9 TIMES.                  EL1501
00573                  24  PI-TRLR-LN-NO        PIC 9.                     CL*28
00574                  24  PI-TRLR-SEQ-NO       PIC S9(4)   COMP.          CL*28
00575                  24  PI-TRLR-TYPE         PIC X.                     CL*28
00576          16  PI-PAY-TYPE                  PIC X.
061013         16  pi-save-type                 pic x.
00577          16  PI-FULL-SCREEN-IND           PIC X.                     CL*28
00578              88  PI-FULL-SCREEN-SHOWN            VALUE 'Y'.
013017         16  pi-approval-level            pic x.
010719         16  FILLER                       PIC X(494).
00580                                                                   EL1501
00581      EJECT                                                        EL1501
00582                                      COPY ELCAID.                    CL*13
00583  01  FILLER    REDEFINES DFHAID.                                  EL1501
00584      12  FILLER                      PIC X(8).                    EL1501
00585      12  PF-VALUES                   PIC X       OCCURS 24 TIMES. EL1501
00586      EJECT                                                        EL1501
00587                                      COPY EL1501S.                   CL*13
00588  01  EL150BI-R REDEFINES EL150BI.                                 EL1501
00589      12  FILLER                      PIC X(62).                   EL1501
00590      12  EL150BI-OCCURS OCCURS 16.                                EL1501
00591          16  MAP-HDG-LENGTH          PIC S9(4)   COMP.               CL*28
00592          16  MAP-HDG-ATTRB           PIC X.                       EL1501
00593          16  MAP-HDG.                                             EL1501
00594              20  MAP-LINE-NO         PIC X.                          CL*28
00595              20  FILLER              PIC X.                          CL*28
00596              20  MAP-HDG-LIT         PIC X(8).                       CL*28
00597          16  MAP-TEXT-LENGTH         PIC S9(4)   COMP.               CL*28
00598          16  MAP-TEXT-ATTRB          PIC X.                          CL*28
00599          16  MAP-TEXT                PIC X(68).                   EL1501
00600                                                                   EL1501
00601      EJECT                                                        EL1501
00602  LINKAGE SECTION.                                                 EL1501
00603  01  DFHCOMMAREA                     PIC X(1024).                 EL1501
00604                                                                   EL1501
00605                                      COPY ELCMSTR.                   CL*13
00606      EJECT                                                        EL1501
00607                                      COPY ELCCNTL.                   CL*13
00608      EJECT                                                        EL1501
00609                                      COPY ELCCERT.                   CL*13
00610      EJECT                                                        EL1501
00611                                      COPY ELCTRLR.                   CL*13
00612      EJECT                                                        EL1501
00613                                      COPY ELCACTQ.                   CL*13
00614      EJECT                                                        EL1501
00615                                      COPY ELCCHKQ.                   CL*13
00616      EJECT                                                           CL*18
00617                                      COPY ELCRCON.                   CL*18
00618      EJECT                                                           CL*19
121802*                                    COPY ERCDMDNT.                  CL*21
121802*    EJECT                                                           CL*21
00619                                      COPY ELCDAR.
061013                                     copy ELCCRTT.
00620      EJECT                                                           CL*21
121802*                                    COPY MPCPLCY.                   CL*19
00622      EJECT                                                        EL1501
00623  PROCEDURE DIVISION.                                              EL1501
00624                                                                   EL1501
00625      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL1501
00626      MOVE '5'                    TO  DC-OPTION-CODE.              EL1501
00627      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1501
00628      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL1501
00629      MOVE DC-GREG-DATE-1-YMD     TO  SAVE-DATE-YMD.                  CL*21
00630      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL1501
00631                                                                      CL*21
00632      IF SAVE-DATE-YY > 70                                            CL*29
00633          MOVE 19                 TO  SAVE-DATE-CC                    CL*21
00634      ELSE                                                            CL*21
00635          MOVE 20                 TO  SAVE-DATE-CC.                   CL*21
00636                                                                   EL1501
00637      IF EIBCALEN = 0                                              EL1501
00638          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL1501
00639                                                                   EL1501
00640      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL1501
00641                                                                   EL1501
00642      MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.         EL1501
00643      MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.           EL1501
00644                                                                   EL1501
00645      MOVE EIBTRMID               TO QID-TERM.                     EL1501
00646                                                                   EL1501
00647      EXEC CICS HANDLE CONDITION                                   EL1501
00648          QIDERR   (1000-SHOW-CLAIM-HISTORY)                       EL1501
00649          MAPFAIL  (0100-FIRST-TIME-IN)                            EL1501
00650          NOTOPEN  (8500-FILE-NOTOPEN)                             EL1501
00651          PGMIDERR (9600-PGMID-ERROR)                              EL1501
00652          ERROR    (9990-ABEND)                                    EL1501
00653      END-EXEC.                                                       CL*28
00654                                                                   EL1501
00655      IF PI-RETURN-TO-PROGRAM = THIS-PGM                              CL*24
00656          MOVE PI-CALLING-PROGRAM       TO RETURNED-FROM.          EL1501
00657                                                                   EL1501
00658      IF PI-CALLING-PROGRAM NOT = THIS-PGM                            CL*24
00659          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                      CL*24
00660              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL1501
00661              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL1501
00662              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL1501
00663              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL1501
00664              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL1501
00665              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL1501
00666              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL1501
00667              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL1501
00668          ELSE                                                     EL1501
00669              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL1501
00670              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL1501
00671              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL1501
00672              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL1501
00673              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL1501
00674              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL1501
00675              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL1501
00676              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL1501
00677                                                                   EL1501
00678      IF RETURNED-FROM NOT = SPACES                                   CL*24
00679          GO TO 0600-RECOVER-TEMP-STORAGE.                         EL1501
00680                                                                   EL1501
00681      IF EIBAID = DFHCLEAR                                            CL*24
00682          GO TO 9400-CLEAR.                                        EL1501
00683                                                                   EL1501
00684      IF PI-PROCESSOR-ID = 'LGXX'                                     CL*24
00685          NEXT SENTENCE                                            EL1501
00686      ELSE                                                         EL1501
00687          EXEC CICS READQ TS                                       EL1501
00688              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL1501
00689              INTO    (SECURITY-CONTROL)                           EL1501
00690              LENGTH  (SC-COMM-LENGTH)                             EL1501
00691              ITEM    (SC-ITEM)                                    EL1501
00692          END-EXEC                                                 EL1501
00693              MOVE SC-CLAIMS-DISPLAY (16)  TO  PI-DISPLAY-CAP      EL1501
00694              MOVE SC-CLAIMS-UPDATE  (16)  TO  PI-MODIFY-CAP       EL1501
00695              IF NOT DISPLAY-CAP                                   EL1501
00696                  MOVE 'READ'              TO  SM-READ             EL1501
00697                  PERFORM 9995-SECURITY-VIOLATION                  EL1501
00698                  MOVE ER-0070             TO  EMI-ERROR           EL1501
00699                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL1501
00700                  GO TO 8100-SEND-INITIAL-MAP.                     EL1501
00701                                                                   EL1501
00702      IF EIBTRNID = TRANS-ID                                          CL*24
00703          GO TO 0200-RECEIVE.                                      EL1501
00704                                                                   EL1501
00705  EJECT                                                            EL1501
00706  0100-FIRST-TIME-IN.                                              EL1501
00707      MOVE LOW-VALUES             TO  EL150BO                      EL1501
00708                                      PI-PROGRAM-WORK-AREA.        EL1501
00709                                                                   EL1501
00710      MOVE 'Y'                    TO  PI-FIRST-TIME-SW             EL1501
00711                                      WS-RECORDS-READ-SW.          EL1501
00712      MOVE 1                      TO  PI-LINE-NO.                  EL1501
00713      MOVE ZERO                   TO  PI-PREV-SEQ-NO                  CL*28
00714                                      PI-REMINDERS-SW                 CL*28
00715                                      PI-LETTERS-SW                EL1501
00716                                      PI-PAYMENTS-SW               EL1501
00717                                      PI-AUTO-PAY-SW               EL1501
00718                                      PI-NOTES-SW                  EL1501
00719                                      PI-RES-EXP-SW                EL1501
00720                                      PI-DENIALS-SW                EL1501
00721                                      PI-INCURRED-DATE-SW          EL1501
00722                                      PI-FORMS-SW.                 EL1501

013017     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
013017     MOVE '2'                    TO CNTL-REC-TYPE
013017     MOVE pi-processor-id        TO CNTL-ACCESS
013017     MOVE +0                     TO CNTL-SEQ-NO
013017     MOVE 'CNTL'                 TO FILE-SWITCH

013017     PERFORM 7900-READ-CONTROL-FILE
                                       THRU 7900-EXIT
013017     MOVE CF-APPROVAL-LEVEL      TO PI-APPROVAL-LEVEL

00724      EXEC CICS DELETEQ TS                                         EL1501
00725          QUEUE(QID)                                               EL1501
00726      END-EXEC.                                                       CL*28
00727                                                                   EL1501
00728      GO TO 1000-SHOW-CLAIM-HISTORY.                               EL1501
00729                                                                   EL1501
00730      EJECT                                                        EL1501
00731  0200-RECEIVE.                                                    EL1501
00732      MOVE 'B'                    TO  PASS-SWITCH.                 EL1501
00733      MOVE LOW-VALUES             TO  EL150BI.                     EL1501
00734                                                                   EL1501
00735      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                          CL*24
00736          MOVE ER-0008            TO  EMI-ERROR                    EL1501
00737          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1501
00738          MOVE -1                 TO  ENTERPFL                     EL1501
00739          GO TO 8200-SEND-DATAONLY.                                EL1501
00740                                                                   EL1501
00741      EXEC CICS RECEIVE                                            EL1501
00742          MAP      (MAP-NAME)                                      EL1501
00743          MAPSET   (MAPSET-NAME)                                   EL1501
00744          INTO     (EL150BI)                                       EL1501
00745      END-EXEC.                                                       CL*28
00746                                                                   EL1501
00747      IF ENTERPFL = 0                                                 CL*24
00748          GO TO 0300-CHECK-PFKEYS.                                 EL1501
00749                                                                   EL1501
00750      IF EIBAID = DFHENTER                                            CL*24
00751          MOVE ER-0004            TO  EMI-ERROR                    EL1501
00752          GO TO 0320-INPUT-ERROR.                                  EL1501
00753                                                                   EL1501
00754      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)               CL*29
00755          MOVE PF-VALUES (ENTERPFI)   TO  EIBAID                   EL1501
00756      ELSE                                                         EL1501
00757          MOVE ER-0029                TO  EMI-ERROR                EL1501
00758          GO TO 0320-INPUT-ERROR.                                  EL1501
00759                                                                   EL1501
00760  0300-CHECK-PFKEYS.                                               EL1501
00761      IF EIBAID = DFHPF23                                             CL*24
00762          GO TO 8810-PF23.                                         EL1501
00763                                                                   EL1501
00764      IF EIBAID = DFHPF24                                             CL*24
00765          GO TO 9200-RETURN-MAIN-MENU.                             EL1501
00766                                                                   EL1501
00767      IF EIBAID = DFHPF12                                             CL*24
00768          GO TO 9500-PF12.                                         EL1501
00769                                                                   EL1501
00770      IF EIBAID = DFHPF3                                              CL*24
00771         IF LINENOL > +0                                              CL*29
00772             GO TO 0500-CREATE-TEMP-STORAGE                           CL*29
00773         ELSE                                                         CL*29
00774             MOVE ER-0672        TO  EMI-ERROR                        CL*29
00775             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 CL*29
00776             MOVE -1             TO  LINENOL                          CL*29
00777             GO TO 8200-SEND-DATAONLY.                                CL*29
00778                                                                   EL1501
00779      IF EIBAID = DFHPF4                                              CL*24
00780         IF LINENOL > +0                                              CL*29
062602           IF (PI-EL142-PRIORITY = '8')
062602              AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
062602                   AND 'AMWA')
062602              MOVE ER-8003        TO  EMI-ERROR                   EL1501
062602              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               CL*18
062602              MOVE -1             TO  LINENOL                        CL*18
062602              GO TO 8200-SEND-DATAONLY                               CL*18
062602           ELSE
00781               MOVE 'V'            TO  WS-VOID-CODE
00782               GO TO 5000-VOID-PAYMENT
062602           END-IF
00783         ELSE                                                      EL1501
00784            MOVE ER-0663        TO  EMI-ERROR                      EL1501
00785            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  CL*18
00786            MOVE -1             TO  LINENOL                           CL*18
00787            GO TO 8200-SEND-DATAONLY                                  CL*18
062602        END-IF
062602     END-IF
00788                                                                      CL*18
00789      IF EIBAID = DFHPF5                                              CL*24
00790         IF LINENOL > +0                                              CL*29
062602           IF (PI-EL142-PRIORITY = '8')
062602              AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
062602                   AND 'AMWA')
062602              MOVE ER-8003        TO  EMI-ERROR                   EL1501
062602              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               CL*18
062602              MOVE -1             TO  LINENOL                        CL*18
062602              GO TO 8200-SEND-DATAONLY                               CL*18
062602           ELSE
00791               MOVE 'S'           TO  WS-VOID-CODE                    CL*18
00792               GO TO 5000-VOID-PAYMENT                                CL*18
062602           END-IF
00793         ELSE                                                         CL*18
00794            MOVE ER-0835        TO  EMI-ERROR                         CL*18
00795            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL1501
00796            MOVE -1             TO  LINENOL                        EL1501
00797            GO TO 8200-SEND-DATAONLY                               EL1501
062602        END-IF
062602     END-IF
00798                                                                      CL*21
121802*    IF EIBAID = DFHPF6                                              CL*28
121802*        IF PI-COMPANY-ID = 'DMD'                                    CL*28
121802*            IF LINENOL NOT = ZEROS                                  CL*28
121802*                MOVE LINENOI        TO SUB-1                        CL*28
121802*                IF PI-TRLR-TYPE (SUB-1) = '2'                       CL*28
121802*                    PERFORM 0500-CREATE-TEMP-STORAGE                CL*21
121802*                    MOVE 'EL402DMD' TO PGM-NAME                     CL*27
121802*                    GO TO 9300-XCTL                                 CL*21
121802*                ELSE                                                CL*21
121802*                    MOVE ER-0940    TO EMI-ERROR                    CL*27
121802*                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        CL*21
121802*                    MOVE -1         TO LINENOL                      CL*28
121802*                    GO TO 8200-SEND-DATAONLY                        CL*21
121802*            ELSE                                                    CL*21
121802*                MOVE ER-0939    TO EMI-ERROR                        CL*21
121802*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*21
121802*                MOVE -1         TO LINENOL                          CL*21
121802*                GO TO 8200-SEND-DATAONLY.                           CL*21
00817                                                                   EL1501
00818      MOVE SPACES                 TO  ERRMSG1O.                    EL1501
00819                                                                   EL1501
00820      IF EIBAID = DFHPF1                                              CL*24
00821          MOVE 'F'                TO  DIRECTION-SWITCH             EL1501
00822          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL1501
00823          GO TO 1000-SHOW-CLAIM-HISTORY.                           EL1501
00824                                                                   EL1501
00825      IF EIBAID = DFHPF2                                              CL*24
00826          MOVE 'B'                TO  DIRECTION-SWITCH             EL1501
00827          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL1501
00828          MOVE PI-TRLR-SEQ-NO (1) TO  PI-PREV-SEQ-NO               EL1501
00829          GO TO 1000-SHOW-CLAIM-HISTORY.                           EL1501
00830                                                                   EL1501
00831      IF EIBAID = DFHENTER                                            CL*24
00832          GO TO 0330-EDIT-DATA.                                    EL1501
00833                                                                   EL1501
00834      MOVE ER-0029                TO EMI-ERROR.                    EL1501
00835                                                                   EL1501
00836  0320-INPUT-ERROR.                                                EL1501
00837      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1501
00838                                                                   EL1501
00839      IF ENTERPFL = 0                                                 CL*24
00840          MOVE -1                 TO ENTERPFL                      EL1501
00841      ELSE                                                         EL1501
00842          MOVE AL-UNBON           TO ENTERPFA                      EL1501
00843          MOVE -1                 TO ENTERPFL.                     EL1501
00844                                                                   EL1501
00845      GO TO 8200-SEND-DATAONLY.                                    EL1501
00846                                                                   EL1501
00847  0330-EDIT-DATA.                                                  EL1501
00848      IF NOT MODIFY-CAP                                            EL1501
00849          MOVE 'UPDATE'           TO  SM-READ                      EL1501
00850          PERFORM 9995-SECURITY-VIOLATION                          EL1501
00851          MOVE ER-0070            TO  EMI-ERROR                    EL1501
00852          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1501
00853          GO TO 8100-SEND-INITIAL-MAP.                             EL1501
00854                                                                   EL1501
00855      IF LINENOL  > 0 AND                                             CL*29
00856         RECVDTL  > 0 AND                                             CL*29
00857         RECVTYPL > 0                                                 CL*29
00858            GO TO 4000-RECEIVE-FORMS.                                 CL*28
00859                                                                   EL1501
00860      IF LINENOL > 0 AND                                              CL*29
00861         RECVDTL > 0                                                  CL*29
00862            GO TO 3000-RECEIVE-LETTERS.                               CL*28
00863                                                                   EL1501
00864      IF LINENOL > 0                                                  CL*29
00865          MOVE LINENOI            TO  SUB-1                        EL1501
00866          IF PI-TRLR-TYPE (SUB-1) = 'A'                               CL*24
00867              MOVE ER-0665        TO  EMI-ERROR                    EL1501
00868              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL1501
00869              MOVE -1             TO  LINENOL                      EL1501
00870              GO TO 8200-SEND-DATAONLY                             EL1501
00871          ELSE                                                     EL1501
00872              IF PI-TRLR-TYPE (SUB-1) = '4'                           CL*24
00873                  MOVE ER-0666        TO  EMI-ERROR                EL1501
00874                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL1501
00875                  MOVE -1             TO  LINENOL                  EL1501
00876                  GO TO 8200-SEND-DATAONLY                         EL1501
00877              ELSE                                                 EL1501
00878                  IF PI-TRLR-TYPE (SUB-1) = '2'                       CL*24
00879                      MOVE ER-0667    TO  EMI-ERROR                EL1501
00880                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL1501
00881                      MOVE -1         TO  LINENOL                  EL1501
00882                      GO TO 8200-SEND-DATAONLY.                    EL1501
00883                                                                   EL1501
00884      MOVE 'F'                    TO  DIRECTION-SWITCH.            EL1501
00885      MOVE 'Y'                    TO  PI-FIRST-TIME-SW.            EL1501
00886      GO TO 1000-SHOW-CLAIM-HISTORY.                               EL1501
00887                                                                   EL1501
00888      EJECT                                                        EL1501
00889  0500-CREATE-TEMP-STORAGE.                                        EL1501
00890      MOVE EIBCPOSN               TO PI-SAVE-CURSOR.                  CL*28
00891      MOVE SPACES                 TO PI-FULL-SCREEN-IND.              CL*21
00892                                                                   EL1501
00893      EXEC CICS WRITEQ TS                                          EL1501
00894          QUEUE    (QID)                                           EL1501
00895          FROM     (PROGRAM-INTERFACE-BLOCK)                       EL1501
00896          LENGTH   (PI-COMM-LENGTH)                                EL1501
00897      END-EXEC.                                                       CL*28
00898                                                                   EL1501
00899      IF LINENOL > +0                                                 CL*29
00900          MOVE LINENOI            TO  SUB-1                        EL1501
00901          IF PI-TRLR-TYPE (SUB-1) = '2'                               CL*29
00902             MOVE +1                      TO  PI-PAYMENTS-SW       EL1501
00903             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO        EL1501
00904                                              PI-SAVE-ATK-SEQ-NO   EL1501
00905          ELSE                                                     EL1501
00906          IF PI-TRLR-TYPE (SUB-1) = '3'                               CL*29
00907             MOVE +1                      TO  PI-AUTO-PAY-SW          CL*28
00908             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*28
00909                                              PI-SAVE-ATK-SEQ-NO   EL1501
00910          ELSE                                                        CL*28
00911          IF PI-TRLR-TYPE (SUB-1) = '4'                               CL*29
00912             MOVE +1                      TO  PI-LETTERS-SW           CL*28
00913             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*28
00914                                              PI-SAVE-ATK-SEQ-NO   EL1501
00915          ELSE                                                        CL*28
00916          IF PI-TRLR-TYPE (SUB-1) = '6'                               CL*29
00917             MOVE +1                      TO  PI-NOTES-SW             CL*28
00918             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*28
00919                                              PI-SAVE-ATK-SEQ-NO   EL1501
00920          ELSE                                                        CL*28
00921          IF PI-TRLR-TYPE (SUB-1) = '7'                               CL*29
00922             MOVE +1                      TO  PI-REMINDERS-SW         CL*28
00923             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*28
00924                                              PI-SAVE-ATK-SEQ-NO   EL1501
00925          ELSE                                                        CL*28
00926          IF PI-TRLR-TYPE (SUB-1) = '8'                               CL*29
00927             MOVE +1                      TO  PI-DENIALS-SW           CL*28
00928             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*28
00929                                              PI-SAVE-ATK-SEQ-NO   EL1501
00930          ELSE                                                        CL*28
00931          IF PI-TRLR-TYPE (SUB-1) = '9'                               CL*29
00932             MOVE +1                      TO  PI-INCURRED-DATE-SW     CL*28
00933             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*28
00934                                              PI-SAVE-ATK-SEQ-NO   EL1501
00935          ELSE                                                        CL*28
00936             MOVE +1                      TO  PI-FORMS-SW             CL*28
00937             MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO           CL*28
00938                                              PI-SAVE-ATK-SEQ-NO   EL1501
00939      ELSE                                                         EL1501
00940          MOVE +1                         TO  PI-REMINDERS-SW      EL1501
00941                                              PI-LETTERS-SW        EL1501
00942                                              PI-PAYMENTS-SW       EL1501
00943                                              PI-AUTO-PAY-SW       EL1501
00944                                              PI-NOTES-SW          EL1501
00945                                              PI-RES-EXP-SW        EL1501
00946                                              PI-DENIALS-SW        EL1501
00947                                              PI-INCURRED-DATE-SW  EL1501
00948                                              PI-FORMS-SW          EL1501
00949          MOVE +0                         TO  PI-ATK-SEQ-NO.       EL1501
00950                                                                      CL*21
00951      IF EIBAID = DFHPF6                                              CL*24
00952          MOVE PI-COMPANY-CD      TO  PI-SAVE-ATK-COMPANY-CODE        CL*21
00953                                      PI-ATK-COMPANY-CODE             CL*21
00954          MOVE PI-CARRIER         TO  PI-SAVE-ATK-CARRIER             CL*21
00955                                      PI-ATK-CARRIER                  CL*21
00956          MOVE PI-CLAIM-NO        TO  PI-SAVE-ATK-CLAIM-NO            CL*21
00957                                      PI-ATK-CLAIM-NO                 CL*21
00958          MOVE PI-CERT-NO         TO  PI-SAVE-ATK-CERT-NO             CL*21
00959                                      PI-ATK-CERT-NO                  CL*21
00960          MOVE 'Y'                TO  PI-FIRST-TIME-SW.               CL*21
00961                                                                   EL1501
00962      IF EIBAID = DFHPF3                                              CL*24
00963          MOVE XCTL-142           TO  PGM-NAME                     EL1501
00964          MOVE 'EL142A'           TO  PI-MAP-NAME                  EL1501
00965          MOVE PI-COMPANY-CD      TO  PI-SAVE-ATK-COMPANY-CODE     EL1501
00966                                      PI-ATK-COMPANY-CODE          EL1501
00967          MOVE PI-CARRIER         TO  PI-SAVE-ATK-CARRIER          EL1501
00968                                      PI-ATK-CARRIER               EL1501
00969          MOVE PI-CLAIM-NO        TO  PI-SAVE-ATK-CLAIM-NO         EL1501
00970                                      PI-ATK-CLAIM-NO              EL1501
00971          MOVE PI-CERT-NO         TO  PI-SAVE-ATK-CERT-NO          EL1501
00972                                      PI-ATK-CERT-NO               EL1501
00973          MOVE 'Y'                TO  PI-FIRST-TIME-SW             EL1501
00974          GO TO 9300-XCTL.                                         EL1501
00975                                                                   EL1501
00976      EJECT                                                        EL1501
00977  0600-RECOVER-TEMP-STORAGE.                                       EL1501
00978      MOVE PI-CONTROL-IN-PROGRESS TO SAVE-CONTROL.                 EL1501
00979                                                                   EL1501
00980      EXEC CICS HANDLE CONDITION                                   EL1501
00981          QIDERR   (0690-QIDERR)                                   EL1501
00982      END-EXEC.                                                       CL*28
00983                                                                   EL1501
00984      EXEC CICS READQ TS                                           EL1501
00985          QUEUE    (QID)                                           EL1501
00986          INTO     (PROGRAM-INTERFACE-BLOCK)                       EL1501
00987          LENGTH   (PI-COMM-LENGTH)                                EL1501
00988      END-EXEC.                                                       CL*28
00989                                                                   EL1501
00990      EXEC CICS DELETEQ TS                                         EL1501
00991          QUEUE   (QID)                                            EL1501
00992      END-EXEC.                                                       CL*28
00993                                                                   EL1501
00994      MOVE SAVE-CONTROL           TO  PI-CONTROL-IN-PROGRESS.      EL1501
00995      MOVE LOW-VALUES             TO  EL150BO.                     EL1501
00996      MOVE ZEROS                  TO  PI-PREV-SEQ-NO.              EL1501
00997      MOVE 'F'                    TO  DIRECTION-SWITCH.            EL1501
00998      MOVE 'Y'                    TO  PI-FIRST-TIME-SW.            EL1501
00999                                                                   EL1501
01000      GO TO 1000-SHOW-CLAIM-HISTORY.                               EL1501
01001                                                                   EL1501
01002  0690-QIDERR.                                                     EL1501
01003      MOVE ER-0033                TO  EMI-ERROR.                   EL1501
01004      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1501
01005      MOVE LOW-VALUES             TO  EL150BO.                     EL1501
01006      MOVE -1                     TO  ENTERPFL.                    EL1501
01007      GO TO 8100-SEND-INITIAL-MAP.                                 EL1501
01008                                                                   EL1501
01009      EJECT                                                        EL1501
01010  1000-SHOW-CLAIM-HISTORY.                                         EL1501
01011                                                                   EL1501
01012      PERFORM 2000-BUILD-TRAILER-DISPLAY THRU 2999-EXIT.           EL1501
01013                                                                   EL1501
01014      MOVE PI-CARRIER             TO  CARRO.                       EL1501
01015      MOVE PI-CLAIM-NO            TO  CLMNOO.                      EL1501
01016      MOVE PI-CERT-PRIME          TO  CERTNOO.                     EL1501
01017      MOVE PI-CERT-SFX            TO  SUFXO.                       EL1501
01018                                                                   EL1501
01019      MOVE -1                     TO  LINENOL.                     EL1501
01020                                                                   EL1501
01021      IF RECORDS-READ                                              EL1501
01022          MOVE 'N'                TO  WS-RECORDS-READ-SW           EL1501
01023          GO TO 8100-SEND-INITIAL-MAP                              EL1501
01024      ELSE                                                         EL1501
01025          GO TO 8200-SEND-DATAONLY.                                EL1501
01026                                                                   EL1501
01027      EJECT                                                        EL1501
01028  2000-BUILD-TRAILER-DISPLAY.                                      EL1501
01029                                                                   EL1501
01030      MOVE PI-COMPANY-CD          TO  TRLR-COMP-CD.                EL1501
01031      MOVE PI-CARRIER             TO  TRLR-CARRIER.                EL1501
01032      MOVE PI-CLAIM-NO            TO  TRLR-CLAIM-NO.               EL1501
01033      MOVE PI-CERT-NO             TO  TRLR-CERT-NO.                EL1501
01034      MOVE 'TRLR'                 TO  FILE-SWITCH.                 EL1501
01035      MOVE PI-PREV-SEQ-NO         TO  TRLR-SEQ-NO.                 EL1501
01036                                                                   EL1501
01037      IF PI-PREV-DIRECTION = 'B'                                      CL*24
01038          IF DIRECTION-SWITCH = 'F'                                   CL*24
01039              MOVE PI-TRLR-SEQ-NO (8) TO  TRLR-SEQ-NO              EL1501
01040                                          PI-PREV-SEQ-NO.          EL1501
01041                                                                   EL1501
01042      IF DIRECTION-SWITCH = 'B'                                       CL*24
01043         MOVE 'B'                 TO  PI-PREV-DIRECTION            EL1501
01044         MOVE +15                 TO  DISPLAY-CNT                  EL1501
01045         GO TO 2050-START-BROWSE.                                  EL1501
01046                                                                   EL1501
01047      MOVE +1                     TO  DISPLAY-CNT.                 EL1501
01048      MOVE 'F'                    TO  DIRECTION-SWITCH             EL1501
01049                                      PI-PREV-DIRECTION.           EL1501
01050  2010-START-BROWSE.                                               EL1501
01051      EXEC CICS HANDLE CONDITION                                   EL1501
01052          ENDFILE   (2950-NO-MORE-TRAILERS)                        EL1501
01053          NOTFND    (2950-NO-MORE-TRAILERS)                        EL1501
01054      END-EXEC.                                                       CL*28
01055                                                                   EL1501
01056      EXEC CICS STARTBR                                            EL1501
01057          DATASET   ('ELTRLR')                                     EL1501
01058          RIDFLD    (ELTRLR-KEY)                                   EL1501
01059          GTEQ                                                     EL1501
01060      END-EXEC.                                                       CL*28
01061                                                                   EL1501
01062      MOVE 1                      TO  PI-LINE-NO                   EL1501
01063                                      SUB-1.                       EL1501
01064                                                                   EL1501
01065  2020-BROWSE-FORWARD.                                             EL1501
01066                                                                   EL1501
01067      EXEC CICS HANDLE CONDITION                                   EL1501
01068          NOTFND    (2020-BROWSE-FORWARD)                          EL1501
01069      END-EXEC.                                                       CL*28
01070                                                                   EL1501
01071      EXEC CICS READNEXT                                           EL1501
01072          DATASET   ('ELTRLR')                                     EL1501
01073          SET       (ADDRESS OF ACTIVITY-TRAILERS)                    CL*21
01074          RIDFLD    (ELTRLR-KEY)                                   EL1501
01075      END-EXEC.                                                       CL*28
01076                                                                   EL1501
01077      IF (PI-COMPANY-CD NOT = TRLR-COMP-CD)  OR                       CL*28
01078         (PI-CARRIER    NOT = TRLR-CARRIER)  OR                       CL*28
01079         (PI-CLAIM-NO   NOT = TRLR-CLAIM-NO) OR                       CL*28
01080         (PI-CERT-NO    NOT = TRLR-CERT-NO)                           CL*28
01081             GO TO 2950-NO-MORE-TRAILERS.                          EL1501
01082                                                                   EL1501
01083      IF TRLR-SEQ-NO = 90                                             CL*24
01084          GO TO 2020-BROWSE-FORWARD.                               EL1501
01085                                                                   EL1501
01086      IF RESERVE-EXPENSE-TR OR ADDRESS-TR                          EL1501
01087          GO TO 2020-BROWSE-FORWARD.                               EL1501
01088                                                                   EL1501
01089      IF TRLR-SEQ-NO = PI-PREV-SEQ-NO                                 CL*24
01090          MOVE 'N'                TO  WS-RECORDS-READ-SW           EL1501
01091          GO TO 2020-BROWSE-FORWARD                                EL1501
01092      ELSE                                                         EL1501
01093          IF FIRST-TIME                                            EL1501
01094              PERFORM 2070-INITIALIZE-SCREEN-AREA THRU 2070-EXIT   EL1501
01095              MOVE 'N'            TO  PI-FIRST-TIME-SW             EL1501
01096              MOVE 'Y'            TO  WS-RECORDS-READ-SW           EL1501
01097          ELSE                                                     EL1501
01098              MOVE 'Y'            TO  WS-RECORDS-READ-SW.          EL1501
01099                                                                      CL*28
01100      MOVE TRLR-SEQ-NO            TO  PI-PREV-SEQ-NO.              EL1501
01101      GO TO 2090-DISPLAY-TRAILER.                                  EL1501
01102                                                                   EL1501
01103  2050-START-BROWSE.                                               EL1501
01104      EXEC CICS HANDLE CONDITION                                   EL1501
01105          ENDFILE   (2950-NO-MORE-TRAILERS)                        EL1501
01106          NOTFND    (2950-NO-MORE-TRAILERS)                        EL1501
01107      END-EXEC.                                                       CL*28
01108                                                                   EL1501
01109      EXEC CICS STARTBR                                            EL1501
01110          DATASET   ('ELTRLR')                                     EL1501
01111          RIDFLD    (ELTRLR-KEY)                                   EL1501
01112          GTEQ                                                     EL1501
01113      END-EXEC.                                                       CL*28
01114                                                                   EL1501
01115      MOVE 8                      TO  PI-LINE-NO                   EL1501
01116                                      SUB-1.                       EL1501
01117                                                                   EL1501
01118  2060-BROWSE-BACKWARD.                                            EL1501
01119                                                                   EL1501
01120      EXEC CICS READPREV                                           EL1501
01121          DATASET   ('ELTRLR')                                     EL1501
01122          SET       (ADDRESS OF ACTIVITY-TRAILERS)                    CL*21
01123          RIDFLD    (ELTRLR-KEY)                                   EL1501
01124      END-EXEC.                                                       CL*28
01125                                                                   EL1501
01126      IF (PI-COMPANY-CD NOT = TRLR-COMP-CD)  OR                       CL*28
01127         (PI-CARRIER    NOT = TRLR-CARRIER)  OR                       CL*28
01128         (PI-CLAIM-NO   NOT = TRLR-CLAIM-NO) OR                       CL*28
01129         (PI-CERT-NO    NOT = TRLR-CERT-NO)                           CL*28
01130             GO TO 2950-NO-MORE-TRAILERS.                          EL1501
01131                                                                   EL1501
01132      IF TRLR-SEQ-NO = 90                                             CL*24
01133          GO TO 2060-BROWSE-BACKWARD.                              EL1501
01134                                                                   EL1501
01135      IF RESERVE-EXPENSE-TR OR ADDRESS-TR                          EL1501
01136          GO TO 2060-BROWSE-BACKWARD.                              EL1501
01137                                                                   EL1501
01138      IF TRLR-SEQ-NO = PI-PREV-SEQ-NO                                 CL*24
01139          MOVE 'N'                TO  WS-RECORDS-READ-SW           EL1501
01140          GO TO 2060-BROWSE-BACKWARD                               EL1501
01141      ELSE                                                         EL1501
01142          IF FIRST-TIME                                            EL1501
01143              PERFORM 2070-INITIALIZE-SCREEN-AREA THRU 2070-EXIT   EL1501
01144              MOVE 'N'            TO  PI-FIRST-TIME-SW             EL1501
01145              MOVE 'Y'            TO  WS-RECORDS-READ-SW           EL1501
01146          ELSE                                                     EL1501
01147              MOVE 'Y'            TO  WS-RECORDS-READ-SW.          EL1501
01148                                                                      CL*27
01149      MOVE TRLR-SEQ-NO            TO  PI-PREV-SEQ-NO.              EL1501
01150      GO TO 2090-DISPLAY-TRAILER.                                  EL1501
01151                                                                   EL1501
01152  EJECT                                                            EL1501
01153  2070-INITIALIZE-SCREEN-AREA.                                     EL1501
01154                                                                   EL1501
01155      MOVE SPACES                 TO  TEXT-WORK-AREAS              EL1501
01156                                      MAP-HDG (1)   MAP-TEXT (1)   EL1501
01157                                      MAP-HDG (2)   MAP-TEXT (2)   EL1501
01158                                      MAP-HDG (3)   MAP-TEXT (3)   EL1501
01159                                      MAP-HDG (4)   MAP-TEXT (4)   EL1501
01160                                      MAP-HDG (5)   MAP-TEXT (5)   EL1501
01161                                      MAP-HDG (6)   MAP-TEXT (6)   EL1501
01162                                      MAP-HDG (7)   MAP-TEXT (7)   EL1501
01163                                      MAP-HDG (8)   MAP-TEXT (8)   EL1501
01164                                      MAP-HDG (9)   MAP-TEXT (9)   EL1501
01165                                      MAP-HDG (10)  MAP-TEXT (10)  EL1501
01166                                      MAP-HDG (11)  MAP-TEXT (11)  EL1501
01167                                      MAP-HDG (12)  MAP-TEXT (12)  EL1501
01168                                      MAP-HDG (13)  MAP-TEXT (13)  EL1501
01169                                      MAP-HDG (14)  MAP-TEXT (14)  EL1501
01170                                      MAP-HDG (15)  MAP-TEXT (15)  EL1501
01171                                      MAP-HDG (16)  MAP-TEXT (16). EL1501
01172                                                                   EL1501
01173      MOVE ZEROS                  TO  PMT-AMT-PAID                 EL1501
01174                                      AUTO-1ST-AMT                 EL1501
01175                                      AUTO-REG-AMT                 EL1501
01176                                      INCUR-TOT-PD.                EL1501
01177                                                                   EL1501
01178  2070-EXIT.                                                       EL1501
01179      EXIT.                                                        EL1501
01180                                                                   EL1501
01181  EJECT                                                            EL1501
01182  2090-DISPLAY-TRAILER.                                            EL1501
01183                                                                   EL1501
01184      IF PAYMENT-TR                                                EL1501
01185          GO TO 2100-PAYMENT-TRAILER.                              EL1501
01186                                                                   EL1501
01187      IF AUTO-PAY-TR                                               EL1501
01188         GO TO 2200-AUTO-PAYMENT-TRAILER.                          EL1501
01189                                                                   EL1501
01190      IF CORRESPONDENCE-TR                                         EL1501
01191          GO TO 2300-CORRESPONDENCE-TRAILER.                       EL1501
01192                                                                   EL1501
01193      IF GENERAL-INFO-TR                                           EL1501
01194          GO TO 2400-GENERAL-INFO-TRAILER.                         EL1501
01195                                                                   EL1501
01196      IF AUTO-PROMPT-TR                                            EL1501
01197          GO TO 2500-AUTO-PROMPT-TRAILER.                          EL1501
01198                                                                   EL1501
01199      IF DENIAL-TR                                                 EL1501
01200          GO TO 2600-DENIAL-TRAILER.                               EL1501
01201                                                                   EL1501
01202      IF INCURRED-CHG-TR                                           EL1501
01203          GO TO 2700-INCURRED-CHANGE-TRAILER.                      EL1501
01204                                                                   EL1501
01205      IF FORM-CONTROL-TR                                           EL1501
01206          GO TO 2710-FORM-CONTROL-TRAILER.                         EL1501
01207                                                                   EL1501
01208      IF DIRECTION-SWITCH = 'B'                                       CL*24
01209          GO TO 2060-BROWSE-BACKWARD                               EL1501
01210      ELSE                                                         EL1501
01211          GO TO 2020-BROWSE-FORWARD.                               EL1501
01212                                                                   EL1501
01213      EJECT                                                        EL1501
01214  2100-PAYMENT-TRAILER.                                            EL1501
01215      MOVE ZEROS                  TO  PMT-AMT-PAID.                EL1501
01216      MOVE PI-LINE-NO             TO  PMT-LINE-NO.                 EL1501
01217      MOVE 'PAYMENT '             TO  PMT-HDG1-LIT.                EL1501
01218      MOVE 'TYPE: '               TO  PMT-TYPE-LIT.                EL1501
01219                                                                      CL*19
022106     IF TRANSFER
022106        MOVE 'TRANSFR'           TO PMT-TYPE
022106        GO TO 2100-CONT
022106     ELSE
022106        IF AT-PAYMENT-TYPE = 'I'
022106           MOVE 'INTEREST'       TO PMT-TYPE
022106           GO TO 2100-CONT
022106        END-IF
022106     END-IF

01224      IF AT-CV-PMT-CODE = ' '                                         CL*24
01225          MOVE AT-PAYMENT-TYPE            TO  WS-SUB                  CL*19
01226          IF  WS-SUB < 1 OR > 6                                       CL*29
01227              MOVE 2                      TO  WS-SUB                  CL*19
01228              MOVE PAY-DESC (WS-SUB)      TO  PMT-TYPE                CL*19
01229          ELSE                                                        CL*19
01230              MOVE PAY-DESC (WS-SUB)      TO  PMT-TYPE                CL*19
01231      ELSE                                                            CL*19
01232          MOVE AT-CV-PMT-CODE             TO  WS-SUB                  CL*19
01233          IF WS-SUB < 1 OR > 8                                        CL*29
01234              MOVE 1                      TO  WS-SUB                  CL*19
01235              MOVE CV-PAY-DESC (WS-SUB)   TO  PMT-TYPE                CL*19
01236          ELSE                                                        CL*19
01237              MOVE CV-PAY-DESC (WS-SUB)   TO  PMT-TYPE.               CL*19
01238                                                                      CL*21
01239  2100-CONT.                                                          CL*21
01240                                                                   EL1501
013017     if at-ach-payment = 'Y'
013017        move ' ACH PAYMT '       to pmt-check-no-lit
              move 'ACH PMNT'          to PMT-HDG1-LIT
013017     else
013017        MOVE ' CHECK NO: '       TO  PMT-CHECK-NO-LIT
013017     end-if

01242      MOVE AT-CHECK-NO            TO  PMT-CHECK-NO.                EL1501
01243                                                                   EL1501
01244      IF AT-CHECK-WRITTEN-DT = LOW-VALUES                             CL*24
01245          IF AT-TO-BE-WRITTEN-DT = LOW-VALUES                         CL*24
01246              MOVE ' DT WRITTEN : '         TO  PMT-DT-WRITTEN-LIT    CL*16
01247              MOVE SPACES                   TO  PMT-DT-WRITTEN     EL1501
01248          ELSE                                                     EL1501
01249              MOVE ' HOLD UNTIL : '         TO  PMT-DT-WRITTEN-LIT    CL*16
01250              MOVE AT-TO-BE-WRITTEN-DT      TO  DC-BIN-DATE-1      EL1501
01251              MOVE ' '                      TO  DC-OPTION-CODE     EL1501
01252              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1501
01253              IF NO-CONVERSION-ERROR                               EL1501
01254                  MOVE DC-GREG-DATE-1-EDIT  TO  PMT-DT-WRITTEN     EL1501
01255              ELSE                                                 EL1501
01256                  MOVE SPACES               TO  PMT-DT-WRITTEN     EL1501
01257      ELSE                                                         EL1501
01258          MOVE ' DT WRITTEN : '             TO  PMT-DT-WRITTEN-LIT    CL*16
01259          MOVE AT-CHECK-WRITTEN-DT          TO  DC-BIN-DATE-1      EL1501
01260          MOVE ' '                          TO  DC-OPTION-CODE     EL1501
01261          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01262          IF NO-CONVERSION-ERROR                                   EL1501
01263              MOVE DC-GREG-DATE-1-EDIT      TO  PMT-DT-WRITTEN     EL1501
01264          ELSE                                                     EL1501
01265              MOVE SPACES                   TO  PMT-DT-WRITTEN.    EL1501
01266                                                                   EL1501
01267      MOVE ' AMT:'                TO  PMT-AMT-PD-LIT.                 CL*19
01268      MOVE AT-AMOUNT-PAID         TO  PMT-AMT-PAID.                EL1501
01269                                                                   EL1501
01270      MOVE 'PAID'                 TO  PMT-HDG2-LIT.                EL1501
01271      MOVE 'FROM: '               TO  PMT-FROM-LIT.                EL1501
01272                                                                   EL1501
01273      IF AT-PAID-FROM-DT = LOW-VALUES                                 CL*24
01274          MOVE SPACES                     TO  PMT-PAID-FROM        EL1501
01275      ELSE                                                         EL1501
01276          MOVE AT-PAID-FROM-DT            TO  DC-BIN-DATE-1        EL1501
01277          MOVE ' '                        TO  DC-OPTION-CODE       EL1501
01278          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01279          IF NO-CONVERSION-ERROR                                   EL1501
01280              MOVE DC-GREG-DATE-1-EDIT    TO PMT-PAID-FROM         EL1501
01281          ELSE                                                     EL1501
01282              MOVE SPACES                 TO  PMT-PAID-FROM.       EL1501
01283                                                                   EL1501
01284      IF PI-USES-PAID-TO                                              CL**3
01285          MOVE ' PAID TO: '               TO  PMT-THRU-LIT            CL**3
01286      ELSE                                                            CL**3
01287          MOVE ' PD THRU: '               TO  PMT-THRU-LIT.           CL**3
01288                                                                      CL**3
01289      IF AT-PAID-THRU-DT = LOW-VALUES                                 CL*24
01290          MOVE SPACES                     TO  PMT-PAID-THRU        EL1501
01291      ELSE                                                         EL1501
01292          IF PI-USES-PAID-TO                                          CL**7
01293              MOVE AT-PAID-THRU-DT        TO  DC-BIN-DATE-1           CL**7
01294              MOVE '6'                    TO  DC-OPTION-CODE          CL**7
01295              MOVE +1                     TO  DC-ELAPSED-DAYS         CL**7
01296              MOVE +0                     TO  DC-ELAPSED-MONTHS       CL**7
01297              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL**7
01298              IF NO-CONVERSION-ERROR                                  CL**7
01299                  MOVE DC-GREG-DATE-1-EDIT TO PMT-PAID-THRU           CL**7
01300              ELSE                                                    CL**7
01301                  MOVE SPACES             TO  PMT-PAID-THRU           CL**7
01302          ELSE                                                     EL1501
01303              MOVE AT-PAID-THRU-DT        TO  DC-BIN-DATE-1           CL**7
01304              MOVE ' '                    TO  DC-OPTION-CODE          CL**7
01305              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL**7
01306              IF NO-CONVERSION-ERROR                                  CL**7
01307                  MOVE DC-GREG-DATE-1-EDIT TO  PMT-PAID-THRU          CL**7
01308              ELSE                                                    CL**7
01309                  MOVE SPACES             TO  PMT-PAID-THRU.          CL**7
01310                                                                   EL1501
01311      MOVE ' PAYEE: '             TO  PMT-PAYEE-LIT.               EL1501
01312      IF INSURED-PAID                                              EL1501
01313          MOVE 'INSURED'          TO  PMT-PAYEE.                      CL*16
01314      IF BENEFICIARY-PAID                                          EL1501
01315          MOVE 'BENEF '           TO  PMT-PAYEE.                   EL1501
01316      IF ACCOUNT-PAID                                              EL1501
01317          MOVE 'ACCOUNT'          TO  PMT-PAYEE.                      CL*16
01318      IF OTHER-1-PAID                                              EL1501
01319          MOVE 'OTHER 1'          TO  PMT-PAYEE.                      CL*16
01320      IF OTHER-2-PAID                                              EL1501
01321          MOVE 'OTHER 2'          TO  PMT-PAYEE.                      CL*16
01322      IF DOCTOR-PAID                                               EL1501
01323          MOVE 'DOCTOR'           TO  PMT-PAYEE.                   EL1501
01324      IF EMPLOYER-PAID                                                CL*18
01325          MOVE 'EMPLOY'           TO  PMT-PAYEE.                      CL*18
01326                                                                   EL1501
01327      IF AT-VOID-DT = LOW-VALUES OR SPACES                            CL*28
01328          MOVE SPACES                     TO  PMT-VOID-LIT            CL*18
01329      ELSE                                                            CL*18
01330          IF AT-VOID-TYPE = 'S'                                       CL*24
01331              MOVE ' STOP DATE : '        TO  PMT-VOID-LIT            CL*18
01332          ELSE                                                        CL*18
01333              MOVE ' VOID DATE : '        TO  PMT-VOID-LIT.           CL*18
01334                                                                      CL*18
01335      IF AT-VOID-DT = LOW-VALUES                                      CL*24
01336          MOVE SPACES                     TO  PMT-VOID-DT          EL1501
01337      ELSE                                                         EL1501
01338          MOVE AT-VOID-DT                 TO  DC-BIN-DATE-1        EL1501
01339          MOVE ' '                        TO  DC-OPTION-CODE       EL1501
01340          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01341          IF NO-CONVERSION-ERROR                                   EL1501
01342              MOVE DC-GREG-DATE-1-EDIT    TO  PMT-VOID-DT          EL1501
01343          ELSE                                                     EL1501
01344              MOVE SPACES                 TO  PMT-VOID-DT.         EL1501
01345                                                                   EL1501
01346      MOVE PMT-HDG1               TO  MAP-HDG        (DISPLAY-CNT).EL1501
01347      MOVE PMT-TEXT-1             TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01348      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1501
01349      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01350      ADD +1 TO DISPLAY-CNT.                                          CL*29
01351      MOVE PMT-HDG2               TO  MAP-HDG        (DISPLAY-CNT).EL1501
01352      MOVE PMT-TEXT-2             TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01353      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1501
01354                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01355      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1501
01356      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1501
01357      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1501
01358                                                                   EL1501
01359      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1501
01360                                                                   EL1501
01361      EJECT                                                        EL1501
01362  2200-AUTO-PAYMENT-TRAILER.                                       EL1501
01363      MOVE PI-LINE-NO             TO  AUTO-LINE-NO.                EL1501
01364      MOVE 'AUTO PMT'             TO  AUTO-HDG1-LIT.               EL1501
01365                                                                   EL1501
01366      MOVE 'EFF : '                       TO  AUTO-EFF-DT-LIT.     EL1501
01367                                                                      CL*28
01368      IF AT-SCHEDULE-START-DT = LOW-VALUES                            CL*24
01369          MOVE SPACES                     TO  AUTO-EFF-DT          EL1501
01370      ELSE                                                         EL1501
01371          MOVE AT-SCHEDULE-START-DT   TO  DC-BIN-DATE-1               CL*11
01372          MOVE ' '                    TO  DC-OPTION-CODE              CL*11
01373          MOVE +0                     TO  DC-ELAPSED-DAYS             CL*11
01374                                          DC-ELAPSED-MONTHS           CL*11
01375          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01376          IF NO-CONVERSION-ERROR                                   EL1501
01377              MOVE DC-GREG-DATE-1-EDIT TO AUTO-EFF-DT                 CL*11
01378          ELSE                                                     EL1501
01379              MOVE SPACES             TO  AUTO-EFF-DT.                CL*11
01380                                                                   EL1501
01381      MOVE ' 1ST AMT: '           TO  AUTO-1ST-AMT-LIT.            EL1501
01382      MOVE ' REG AMOUNT : '       TO  AUTO-REG-AMT-LIT.            EL1501
01383      MOVE AT-FIRST-PMT-AMT       TO  AUTO-1ST-AMT.                EL1501
01384      MOVE AT-REGULAR-PMT-AMT     TO  AUTO-REG-AMT.                EL1501
01385                                                                   EL1501
01386      MOVE ' STAT : '             TO  AUTO-PMT-STATUS-LIT.         EL1501
01387                                                                      CL*29
01388      IF AT-TERMINATED-DT NOT = LOW-VALUES                            CL*24
01389          MOVE 'TERM'             TO  AUTO-PMT-STATUS              EL1501
01390      ELSE                                                         EL1501
01391          MOVE 'ACTIVE'           TO  AUTO-PMT-STATUS.             EL1501
01392                                                                   EL1501
01393      MOVE 'PAYE'                 TO  AUTO-HDG2-LIT.               EL1501
01394      MOVE '    : '               TO  AUTO-PAYEE-LIT.              EL1501
01395                                                                      CL*29
01396      IF INSURED-PAID-AUTO                                         EL1501
01397          MOVE 'INSURED '         TO  AUTO-PAYEE.                     CL*11
01398      IF BENEFICIARY-PAID-AUTO                                     EL1501
01399          MOVE 'BENEF   '         TO  AUTO-PAYEE.                     CL*11
01400      IF ACCOUNT-PAID-AUTO                                         EL1501
01401          MOVE 'ACCOUNT '         TO  AUTO-PAYEE.                     CL*11
01402      IF OTHER-1-PAID-AUTO                                         EL1501
01403          MOVE 'OTHER 1 '         TO  AUTO-PAYEE.                     CL*11
01404      IF OTHER-2-PAID                                              EL1501
01405          MOVE 'OTHER 2 '         TO  AUTO-PAYEE.                     CL*11
01406      IF DOCTOR-PAID                                               EL1501
01407          MOVE 'DOCTOR  '         TO  AUTO-PAYEE.                     CL*11
01408                                                                   EL1501
01409      MOVE ' 1ST PMT: '                   TO  AUTO-1ST-PMT-LIT.    EL1501
01410                                                                      CL*29
01411      IF AT-1ST-PAY-THRU-DT = LOW-VALUES                              CL*24
01412          MOVE SPACES                     TO  AUTO-1ST-PMT-DT      EL1501
01413      ELSE                                                         EL1501
01414          IF PI-USES-PAID-TO                                          CL*11
01415              MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1           CL*11
01416              MOVE '6'                    TO  DC-OPTION-CODE          CL*11
01417              MOVE +1                     TO  DC-ELAPSED-DAYS         CL*11
01418              MOVE +0                     TO  DC-ELAPSED-MONTHS       CL*11
01419              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL*11
01420              IF NO-CONVERSION-ERROR                                  CL*11
01421                  MOVE DC-GREG-DATE-1-EDIT TO AUTO-1ST-PMT-DT         CL*11
01422              ELSE                                                    CL*11
01423                  MOVE SPACES             TO  AUTO-1ST-PMT-DT         CL*11
01424          ELSE                                                     EL1501
01425              MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1           CL*11
01426              MOVE ' '                    TO  DC-OPTION-CODE          CL*11
01427              MOVE +0                     TO  DC-ELAPSED-DAYS         CL*11
01428                                              DC-ELAPSED-MONTHS       CL*11
01429              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT           CL*11
01430              IF NO-CONVERSION-ERROR                                  CL*11
01431                  MOVE DC-GREG-DATE-1-EDIT TO AUTO-1ST-PMT-DT         CL*11
01432              ELSE                                                    CL*11
01433                  MOVE SPACES             TO  AUTO-1ST-PMT-DT.        CL*11
01434                                                                   EL1501
01435      IF AT-TERMINATED-DT = LOW-VALUES                                CL*24
01436          IF AT-SCHEDULE-END-DT = LOW-VALUES                          CL*24
01437              MOVE SPACES                     TO  AUTO-LST-PMT-DT  EL1501
01438          ELSE                                                     EL1501
01439              MOVE ' LST PMT    : '           TO  AUTO-LST-PMT-LIT EL1501
01440              IF PI-USES-PAID-TO                                      CL*11
01441                  MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1       CL*11
01442                  MOVE '6'                    TO  DC-OPTION-CODE      CL*11
01443                  MOVE +1                     TO  DC-ELAPSED-DAYS     CL*11
01444                  MOVE +0                     TO  DC-ELAPSED-MONTHS   CL*11
01445                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT       CL*11
01446                  IF NO-CONVERSION-ERROR                              CL*11
01447                      MOVE DC-GREG-DATE-1-EDIT TO AUTO-LST-PMT-DT     CL*11
01448                  ELSE                                                CL*11
01449                      MOVE SPACES             TO  AUTO-LST-PMT-DT     CL*11
01450              ELSE                                                 EL1501
01451                  MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1       CL*11
01452                  MOVE ' '                    TO  DC-OPTION-CODE      CL*11
01453                  MOVE +0                     TO  DC-ELAPSED-DAYS     CL*11
01454                                                  DC-ELAPSED-MONTHS   CL*11
01455                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT       CL*11
01456                  IF NO-CONVERSION-ERROR                              CL*11
01457                      MOVE DC-GREG-DATE-1-EDIT TO AUTO-LST-PMT-DT     CL*11
01458                  ELSE                                                CL*11
01459                      MOVE SPACES             TO  AUTO-LST-PMT-DT     CL*11
01460      ELSE                                                         EL1501
01461          MOVE ' TERM DATE  : '               TO  AUTO-LST-PMT-LIT EL1501
01462          MOVE AT-TERMINATED-DT               TO  DC-BIN-DATE-1    EL1501
01463          MOVE ' '                            TO  DC-OPTION-CODE   EL1501
01464          MOVE +0                             TO  DC-ELAPSED-DAYS     CL*11
01465                                                  DC-ELAPSED-MONTHS   CL*11
01466          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01467          IF NO-CONVERSION-ERROR                                   EL1501
01468              MOVE DC-GREG-DATE-1-EDIT        TO  AUTO-LST-PMT-DT  EL1501
01469          ELSE                                                     EL1501
01470              MOVE SPACES                     TO  AUTO-LST-PMT-DT. EL1501
01471                                                                   EL1501
01472      MOVE AUTO-PMT-HDG1          TO  MAP-HDG        (DISPLAY-CNT).EL1501
01473      MOVE AUTO-PMT-TEXT1         TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01474      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1501
01475      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01476      ADD +1 TO DISPLAY-CNT.                                          CL*29
01477      MOVE AUTO-PMT-HDG2          TO  MAP-HDG        (DISPLAY-CNT).EL1501
01478      MOVE AUTO-PMT-TEXT2         TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01479      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1501
01480                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01481      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1501
01482      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1501
01483      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1501
01484                                                                   EL1501
01485      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1501
01486                                                                   EL1501
01487      EJECT                                                        EL1501
01488  2300-CORRESPONDENCE-TRAILER.                                     EL1501
01489      MOVE PI-LINE-NO             TO  CORR-LINE-NO.                EL1501
01490      MOVE 'LETTER'               TO  CORR-HDG1-LIT.               EL1501
01491      MOVE 'FORM: '               TO  CORR-FORM-LIT.               EL1501
01492      MOVE AT-STD-LETTER-FORM     TO  CORR-FORM-TYPE.              EL1501
01493                                                                   EL1501
01494      MOVE ' DT SENT: '                   TO  CORR-DT-SENT-LIT.    EL1501
01495                                                                      CL*29
01496      IF AT-LETTER-SENT-DT = LOW-VALUES                               CL*24
01497          MOVE SPACES                     TO  CORR-DT-SENT         EL1501
01498      ELSE                                                         EL1501
01499          MOVE AT-LETTER-SENT-DT          TO  DC-BIN-DATE-1        EL1501
01500          MOVE ' '                        TO  DC-OPTION-CODE       EL1501
01501          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01502          IF NO-CONVERSION-ERROR                                   EL1501
01503              MOVE DC-GREG-DATE-1-EDIT    TO  CORR-DT-SENT         EL1501
01504          ELSE                                                     EL1501
01505              MOVE SPACES                 TO  CORR-DT-SENT.        EL1501
01506                                                                   EL1501
071210        MOVE ' INIT PRT: '               TO  CORR-INIT-PRT-LIT.   EL1501
01508                                                                      CL*29
01509      IF AT-INITIAL-PRINT-DATE = LOW-VALUES                           CL*24
01510          MOVE SPACES                     TO  CORR-INIT-PRT-DT     EL1501
01511      ELSE                                                         EL1501
01512          MOVE AT-INITIAL-PRINT-DATE      TO  DC-BIN-DATE-1        EL1501
01513          MOVE ' '                        TO  DC-OPTION-CODE       EL1501
01514          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01515          IF NO-CONVERSION-ERROR                                   EL1501
01516              MOVE DC-GREG-DATE-1-EDIT    TO  CORR-INIT-PRT-DT     EL1501
01517          ELSE                                                     EL1501
01518              MOVE SPACES                 TO  CORR-INIT-PRT-DT.    EL1501
01519                                                                   EL1501
071210     MOVE ' TO: '                TO  CORR-ADDR-LIT.               EL1501
01521      MOVE AT-ADDRESEE-TYPE       TO  CORR-ADDR-TYPE.              EL1501
01522                                                                   EL1501
071210     IF AT-LETTER-TO-BENE EQUAL 'Y'
071210        MOVE 'LETTER TO BENE'    TO CORR-LET-TO-BEN
071210     ELSE
062217        IF AT-AUTH-RCVD >  SPACES
062217           MOVE ' AUTH RCVD: ' TO CORR-LET-TO-BEN
062217           MOVE AT-AUTH-RCVD TO CORR-LET-TO-BEN(13:1)
062217        ELSE
071210           MOVE SPACES              TO CORR-LET-TO-BEN
062217        END-IF
071210     END-IF.
071210
01523      MOVE 'DATE: '                          TO  CORR-RESEND-LIT.  EL1501
01524                                                                      CL*29
01525      IF AT-RESEND-PRINT-DATE = LOW-VALUES                            CL*24
01526          IF AT-AUTO-RE-SEND-DT = LOW-VALUES                          CL*24
01527              MOVE SPACES                    TO  CORR-RESEND-DT    EL1501
01528          ELSE                                                     EL1501
01529              MOVE 'RSND'                    TO  CORR-HDG2-LIT     EL1501
01530              MOVE AT-AUTO-RE-SEND-DT        TO  DC-BIN-DATE-1     EL1501
01531              MOVE ' '                       TO  DC-OPTION-CODE    EL1501
01532              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1501
01533              IF NO-CONVERSION-ERROR                               EL1501
01534                  MOVE DC-GREG-DATE-1-EDIT   TO  CORR-RESEND-DT    EL1501
01535              ELSE                                                 EL1501
01536                  MOVE SPACES                TO  CORR-RESEND-DT    EL1501
01537      ELSE                                                         EL1501
01538          MOVE 'RSNT'                        TO  CORR-HDG2-LIT     EL1501
01539          MOVE AT-RESEND-PRINT-DATE          TO  DC-BIN-DATE-1     EL1501
01540          MOVE ' '                           TO  DC-OPTION-CODE    EL1501
01541          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01542          IF NO-CONVERSION-ERROR                                   EL1501
01543              MOVE DC-GREG-DATE-1-EDIT       TO  CORR-RESEND-DT    EL1501
01544          ELSE                                                     EL1501
01545              MOVE SPACES                    TO  CORR-RESEND-DT.   EL1501
01546                                                                   EL1501
113010     IF AT-STOP-LETTER-DT NOT = LOW-VALUES AND SPACES
113010        AND (AT-LETTER-ANSWERED-DT = LOW-VALUES  OR
113010             AT-LETTER-ANSWERED-DT > AT-STOP-LETTER-DT)
113010           MOVE '    STOP:'             TO  CORR-RECVD-LIT
113010           MOVE AT-STOP-LETTER-DT       TO  DC-BIN-DATE-1
113010           MOVE ' '                     TO  DC-OPTION-CODE
113010           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
113010           IF NO-CONVERSION-ERROR
113010             MOVE DC-GREG-DATE-1-EDIT   TO  CORR-RECVD-DT
113010           ELSE
113010             MOVE SPACES                TO  CORR-RECVD-DT
113010           END-IF
113010     ELSE
041613        IF AT-LETTER-SENT-DT = LOW-VALUES OR SPACES
041613            MOVE 'MAIL RCVD'    TO CORR-HDG1-LIT
041613        END-IF
113010        MOVE ' RECVD  : '               TO  CORR-RECVD-LIT
113010        IF AT-LETTER-ANSWERED-DT = LOW-VALUES
113010            MOVE SPACES                 TO  CORR-RECVD-DT
113010        ELSE
113010            MOVE AT-LETTER-ANSWERED-DT  TO  DC-BIN-DATE-1
113010            MOVE ' '                    TO  DC-OPTION-CODE
113010            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
113010            IF NO-CONVERSION-ERROR
113010                MOVE DC-GREG-DATE-1-EDIT TO CORR-RECVD-DT
113010            ELSE
113010                MOVE SPACES             TO  CORR-RECVD-DT
113010            END-IF
113010        END-IF
113010     END-IF.
01559                                                                   EL1501
01560      MOVE ' FOLLOW UP  : '              TO  CORR-FOLLOW-UP-LIT.   EL1501
01561                                                                      CL*29
01562      IF AT-RECEIPT-FOLLOW-UP = LOW-VALUES                            CL*24
01563          MOVE SPACES                    TO  CORR-FOLLOW-UP-DT     EL1501
01564      ELSE                                                         EL1501
01565          MOVE AT-RECEIPT-FOLLOW-UP      TO  DC-BIN-DATE-1         EL1501
01566          MOVE ' '                       TO  DC-OPTION-CODE        EL1501
01567          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01568          IF NO-CONVERSION-ERROR                                   EL1501
01569              MOVE DC-GREG-DATE-1-EDIT   TO  CORR-FOLLOW-UP-DT     EL1501
01570          ELSE                                                     EL1501
01571              MOVE SPACES                TO  CORR-FOLLOW-UP-DT.    EL1501
01572                                                                      CL*16
01573      MOVE ' ARC: '               TO  CORR-ARCH-LIT.                  CL*16
01574      MOVE AT-LETTER-ARCHIVE-NO   TO  CORR-ARCH-NO.                   CL*16
01575                                                                   EL1501
01576      MOVE CORR-HDG1              TO  MAP-HDG        (DISPLAY-CNT).EL1501
01577      MOVE CORR-TEXT-1            TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01578      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1501
01579      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01580      ADD +1 TO DISPLAY-CNT.                                          CL*29
01581      MOVE CORR-HDG2              TO  MAP-HDG        (DISPLAY-CNT).EL1501
01582      MOVE CORR-TEXT-2            TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01583      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1501
01584                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01585      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1501
01586      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1501
01587      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1501
01588                                                                   EL1501
01589      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1501
01590                                                                   EL1501
01591      EJECT                                                        EL1501
01592  2400-GENERAL-INFO-TRAILER.                                       EL1501
01593                                                                      CL*21
01594      MOVE SPACES                 TO GEN-INFO-LINE-1                  CL*21
01595                                     GEN-INFO-LINE-2.                 CL*21
01596                                                                      CL*21
01597      IF AT-PAYMENT-NOTE                                           EL1501
01598          IF DIRECTION-SWITCH = 'B'                                   CL*24
01599              GO TO 2060-BROWSE-BACKWARD                           EL1501
01600          ELSE                                                     EL1501
01601              GO TO 2020-BROWSE-FORWARD.                           EL1501
01602                                                                   EL1501
01603      MOVE PI-LINE-NO             TO  GI-LINE-NO.                  EL1501

061013     evaluate true
061013        when at-maint-note
061013           MOVE 'MAINT'          TO GI-HDG1-LIT
061013        when at-call-note
061013           MOVE 'CALL'           TO GI-HDG1-LIT
061013           IF AT-PHONE-CALL-IN                    
061013               MOVE '     IN'    TO GI-HDG2-LIT
061013           ELSE
061013               MOVE '     OUT'   TO GI-HDG2-LIT
061013           END-IF
061013        when at-cert-change
061013           MOVE 'CERT CHG'       TO GI-HDG1-LIT
061013        when at-errors-note
061013           move  'NOTE'          TO  GI-HDG1-LIT
061013           perform 7520-read-claim-mstr
061013                                 thru 7520-exit
061013           if resp-normal
061013              move cl-claim-type to pi-save-type
061013           end-if
061013           move error-message-interface-block
061013                                 to ws-save-error-interface-block
061013           perform varying s1 from +1 by +1 until
061013              at-note-error-no (s1) = spaces
061013              move at-note-error-no (s1)
061013                                 to emi-error
061013              PERFORM 9900-ERROR-FORMAT
061013                                 THRU 9900-EXIT
061013              if at-note-error-no (s1) = '1653'
061013                 evaluate true
061013                    when pi-save-type = 'L'
061013                       move '    LF    '
061013                                 to emi-text-variable (1)
061013                    when pi-save-type = 'I'
061013                       move '    IU    '
061013                                 to emi-text-variable (1)
052614                    when pi-save-type = 'F'
052614                       move '    FL    '
052614                                 to emi-text-variable (1)
080322                    when pi-save-type = 'B'
080322                       move ' BR  '
080322                                 to emi-text-variable (1)
080322                    when pi-save-type = 'H'
080322                       move ' HS '
080322                                 to emi-text-variable (1)
100518                    when pi-save-type = 'O'
100518                       move '    OT    '
100518                                 to emi-text-variable (1)
061013                    when other
061013                       move '    AH    '
061013                                 to emi-text-variable (1)
061013                 end-evaluate
061013              end-if
061013              move 'MSG : '      to gen-info-msg-1-lit
061013              move emi-line1 (8:64)
061013                                 to gen-info-msg-1
061013              move gen-info-hdg1 to map-hdg (display-cnt)
061013              move gen-info-text-1
061013                                 to map-text (display-cnt)
061013              move al-sabon      to map-hdg-attrb (display-cnt)
061013                                    map-text-attrb (display-cnt)
061013              add +1 to display-cnt pi-line-no
061013           end-perform
061013           if s1 > +1
061013              subtract +1 from display-cnt
061013              subtract +1 from pi-line-no
061013           end-if
061013           move ws-save-error-interface-block
061013                                 to error-message-interface-block
061013           MOVE TRLR-SEQ-NO      TO PI-TRLR-SEQ-NO (SUB-1)
061013           MOVE PI-LINE-NO       TO PI-TRLR-LN-NO  (SUB-1)
061013           MOVE AT-TRAILER-TYPE  TO PI-TRLR-TYPE   (SUB-1)
061013           GO TO 2800-INCR-DISPLAY-CNT
061013        when other
061013           move  'NOTE'          TO  GI-HDG1-LIT
061013     end-evaluate
01619                                                                      CL*21
01620      MOVE 'MSG : '               TO  GEN-INFO-MSG-1-LIT              CL*28
01621                                      GEN-INFO-MSG-2-LIT.             CL*28
01622                                                                   EL1501
01623      MOVE AT-INFO-LINE-1         TO  GEN-INFO-MSG-1.              EL1501
01624      MOVE AT-INFO-LINE-2         TO  GEN-INFO-MSG-2.              EL1501
01625                                                                   EL1501
01626      MOVE GEN-INFO-HDG1          TO  MAP-HDG        (DISPLAY-CNT).EL1501
01627      MOVE GEN-INFO-TEXT-1        TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01628      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1501
01629      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01630      ADD +1 TO DISPLAY-CNT.                                          CL*29
01631      MOVE GEN-INFO-HDG2          TO  MAP-HDG        (DISPLAY-CNT).EL1501
01632      MOVE GEN-INFO-TEXT-2        TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01633      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1501
01634                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01635      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1501
01636      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1501
01637      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1501
01638                                                                   EL1501
01639      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1501
01640                                                                   EL1501
01641      EJECT                                                        EL1501
01642  2500-AUTO-PROMPT-TRAILER.                                        EL1501
01643      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL1501
01644      MOVE '5'                    TO  DC-OPTION-CODE.              EL1501
01645      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL1501
01646                                                                   EL1501
01647      IF DC-BIN-DATE-1 > AT-PROMPT-END-DT                             CL*29
01648          IF DIRECTION-SWITCH = 'B'                                   CL*24
01649              GO TO 2060-BROWSE-BACKWARD                           EL1501
01650          ELSE                                                     EL1501
01651              GO TO 2020-BROWSE-FORWARD.                           EL1501
01652                                                                   EL1501
01653      MOVE PI-LINE-NO             TO  REM-LINE-NO.                 EL1501
01654      MOVE 'REMINDER'             TO  REM-HDG1-LIT.                EL1501
01655      MOVE 'LN 1: '               TO  REM-LINE-1-LIT.              EL1501
01656      MOVE 'LN 2: '               TO  REM-LINE-2-LIT.              EL1501
01657                                                                   EL1501
01658      MOVE AT-PROMPT-LINE-1       TO  REM-LINE-1.                  EL1501
01659      MOVE AT-PROMPT-LINE-2       TO  REM-LINE-2.                  EL1501
01660                                                                   EL1501
01661      MOVE REMINDER-HDG1          TO  MAP-HDG        (DISPLAY-CNT).EL1501
01662      MOVE REMINDER-TEXT-1        TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01663      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1501
01664      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01665      ADD +1 TO DISPLAY-CNT.                                          CL*29
01666      MOVE REMINDER-HDG2          TO  MAP-HDG        (DISPLAY-CNT).EL1501
01667      MOVE REMINDER-TEXT-2        TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01668      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1501
01669                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01670      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1501
01671      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1501
01672      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1501
01673                                                                   EL1501
01674      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1501
01675                                                                   EL1501
01676      EJECT                                                        EL1501
01677  2600-DENIAL-TRAILER.                                             EL1501
01678      MOVE PI-LINE-NO             TO  DENIAL-LINE-NO.              EL1501
01679      MOVE 'DENIAL'               TO  DENIAL-HDG1-LIT.             EL1501
01680      MOVE 'LN 1: '               TO  DENIAL-LN1-LIT.              EL1501
01681      MOVE 'LN 2: '               TO  DENIAL-LN2-LIT.              EL1501
01682                                                                   EL1501
01683      MOVE AT-DENIAL-INFO-1       TO  DENIAL-LN1.                  EL1501
01684      MOVE AT-DENIAL-INFO-2       TO  DENIAL-LN2.                  EL1501
01685                                                                   EL1501
01686      MOVE DENIAL-HDG1            TO  MAP-HDG        (DISPLAY-CNT).EL1501
01687      MOVE DENIAL-TEXT-1          TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01688      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1501
01689      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01690      ADD +1 TO DISPLAY-CNT.                                          CL*29
01691      MOVE DENIAL-HDG2            TO  MAP-HDG        (DISPLAY-CNT).EL1501
01692      MOVE DENIAL-TEXT-2          TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01693      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1501
01694                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01695      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1501
01696      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1501
01697      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1501
01698                                                                   EL1501
01699      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1501
01700                                                                   EL1501
01701      EJECT                                                        EL1501
01702  2700-INCURRED-CHANGE-TRAILER.                                    EL1501
01703      MOVE PI-LINE-NO             TO  INCUR-LINE-NO.               EL1501
01704      MOVE 'INCUR CG'             TO  INCUR-HDG1-LIT.              EL1501
01705                                                                   EL1501
01706      MOVE 'INC : '                       TO  INCUR-INCUR-LIT.     EL1501
01707      IF AT-OLD-INCURRED-DT = LOW-VALUES                              CL*24
01708          MOVE SPACES                     TO  INCUR-INCUR-DT       EL1501
01709      ELSE                                                         EL1501
01710          MOVE AT-OLD-INCURRED-DT         TO  DC-BIN-DATE-1        EL1501
01711          MOVE ' '                        TO  DC-OPTION-CODE       EL1501
01712          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01713          IF NO-CONVERSION-ERROR                                   EL1501
01714              MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-INCUR-DT       EL1501
01715          ELSE                                                     EL1501
01716              MOVE SPACES                 TO  INCUR-INCUR-DT.      EL1501
01717                                                                   EL1501
01718      MOVE ' REPORT : '                   TO  INCUR-REPORT-LIT.    EL1501
01719      IF AT-OLD-REPORTED-DT = LOW-VALUES                              CL*24
01720          MOVE SPACES                     TO  INCUR-REPORT-DT      EL1501
01721      ELSE                                                         EL1501
01722          MOVE AT-OLD-REPORTED-DT         TO  DC-BIN-DATE-1        EL1501
01723          MOVE ' '                        TO  DC-OPTION-CODE       EL1501
01724          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01725          IF NO-CONVERSION-ERROR                                   EL1501
01726              MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-REPORT-DT      EL1501
01727          ELSE                                                     EL1501
01728              MOVE SPACES                 TO  INCUR-REPORT-DT.     EL1501
01729                                                                   EL1501
01730      MOVE ' ESTABLISH  : '               TO  INCUR-ESTAB-LIT.     EL1501
01731      IF AT-OLD-ESTABLISHED-DT = LOW-VALUES                           CL*24
01732          MOVE SPACES                     TO  INCUR-ESTAB-DT       EL1501
01733      ELSE                                                         EL1501
01734          MOVE AT-OLD-ESTABLISHED-DT      TO  DC-BIN-DATE-1        EL1501
01735          MOVE ' '                        TO  DC-OPTION-CODE       EL1501
01736          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01737          IF NO-CONVERSION-ERROR                                   EL1501
01738              MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-ESTAB-DT       EL1501
01739          ELSE                                                     EL1501
01740              MOVE SPACES                 TO  INCUR-ESTAB-DT.      EL1501
01741                                                                   EL1501
01742      MOVE 'PAID'                         TO  INCUR-HDG2-LIT.      EL1501
01743      IF PI-USES-PAID-TO                                              CL**5
01744          MOVE '  TO: '                   TO  INCUR-PD-THRU-LIT       CL**5
01745      ELSE                                                            CL**5
01746          MOVE 'THRU: '                   TO  INCUR-PD-THRU-LIT.      CL**5
01747                                                                      CL**5
01748      IF AT-OLD-PAID-THRU-DT = LOW-VALUES                             CL*24
01749          MOVE SPACES                     TO  INCUR-PD-THRU-DT     EL1501
01750      ELSE                                                         EL1501
01751          MOVE AT-OLD-PAID-THRU-DT        TO  DC-BIN-DATE-1        EL1501
01752          MOVE ' '                        TO  DC-OPTION-CODE       EL1501
01753          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01754          IF NO-CONVERSION-ERROR                                   EL1501
01755              MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-PD-THRU-DT     EL1501
01756          ELSE                                                     EL1501
01757              MOVE SPACES                 TO  INCUR-PD-THRU-DT.    EL1501
01758                                                                   EL1501
01759      MOVE ' TOT AMT: '           TO  INCUR-TOT-PD-LIT.            EL1501
01760      MOVE ' TOT DAYS PD: '       TO  INCUR-TOT-DAYS-LIT.          EL1501
01761      MOVE ' NO PMTS : '          TO  INCUR-NO-PMTS-LIT.           EL1501
01762      MOVE AT-OLD-TOTAL-PAID      TO  INCUR-TOT-PD.                EL1501
01763      MOVE AT-OLD-DAYS-PAID       TO  INCUR-TOT-DAYS-PD.           EL1501
01764      MOVE AT-OLD-NO-OF-PMTS      TO  INCUR-NO-PMTS.               EL1501
01765                                                                   EL1501
01766      MOVE INCUR-CHG-HDG1         TO  MAP-HDG        (DISPLAY-CNT).EL1501
01767      MOVE INCUR-TEXT-1           TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01768      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1501
01769      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01770      ADD +1 TO DISPLAY-CNT.                                          CL*29
01771      MOVE INCUR-CHG-HDG2         TO  MAP-HDG        (DISPLAY-CNT).EL1501
01772      MOVE INCUR-TEXT-2           TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01773      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1501
01774                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01775      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1501
01776      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1501
01777      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1501
01778                                                                   EL1501
01779      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1501
01780                                                                   EL1501
01781      EJECT                                                        EL1501
01782  2710-FORM-CONTROL-TRAILER.                                       EL1501
01783      MOVE PI-LINE-NO             TO  FORM-LINE-NO.                EL1501
01784      MOVE 'FORM    '             TO  FORM-HDG1-LIT.               EL1501
01785                                                                   EL1501
01786      MOVE 'FORM: '               TO  FORM-TYPE-LIT.               EL1501
01787                                                                      CL*29
01788      IF INITIAL-FORM                                              EL1501
01789         MOVE 'INIT'              TO  FORM-TYPE                    EL1501
01790      ELSE                                                         EL1501
01791         MOVE 'PROG'              TO  FORM-TYPE.                   EL1501
01792                                                                   EL1501
01793      IF AT-FORM-PRINTED-DT = LOW-VALUES                              CL*24
01794          MOVE ' SEND ON: '                   TO  FORM-SEND-ON-LIT EL1501
01795          IF AT-FORM-SEND-ON-DT = LOW-VALUES                          CL*24
01796              MOVE SPACES                     TO  FORM-SEND-ON-DT  EL1501
01797          ELSE                                                     EL1501
01798              MOVE AT-FORM-SEND-ON-DT         TO  DC-BIN-DATE-1    EL1501
01799              MOVE ' '                        TO  DC-OPTION-CODE   EL1501
01800              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1501
01801              IF NO-CONVERSION-ERROR                               EL1501
01802                  MOVE DC-GREG-DATE-1-EDIT    TO  FORM-SEND-ON-DT  EL1501
01803              ELSE                                                 EL1501
01804                  MOVE SPACES                 TO  FORM-SEND-ON-DT  EL1501
01805      ELSE                                                         EL1501
01806          MOVE ' SENT ON: '                   TO  FORM-SEND-ON-LIT EL1501
01807          MOVE AT-FORM-PRINTED-DT             TO  DC-BIN-DATE-1    EL1501
01808          MOVE ' '                            TO  DC-OPTION-CODE   EL1501
01809          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01810          IF NO-CONVERSION-ERROR                                   EL1501
01811              MOVE DC-GREG-DATE-1-EDIT        TO  FORM-SEND-ON-DT  EL1501
01812          ELSE                                                     EL1501
01813              MOVE SPACES                     TO  FORM-SEND-ON-DT. EL1501
01814                                                                   EL1501
01815      IF AT-FORM-REPRINT-DT = LOW-VALUES                              CL*24
01816          MOVE ' RESEND     : '               TO  FORM-RESEND-LIT  EL1501
01817          IF AT-FORM-RE-SEND-DT = LOW-VALUES                          CL*24
01818              MOVE SPACES                     TO  FORM-RESEND-DT   EL1501
01819          ELSE                                                     EL1501
01820              MOVE AT-FORM-RE-SEND-DT         TO  DC-BIN-DATE-1    EL1501
01821              MOVE ' '                        TO  DC-OPTION-CODE   EL1501
01822              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1501
01823              IF NO-CONVERSION-ERROR                               EL1501
01824                  MOVE DC-GREG-DATE-1-EDIT    TO  FORM-RESEND-DT   EL1501
01825              ELSE                                                 EL1501
01826                  MOVE SPACES                 TO  FORM-RESEND-DT   EL1501
01827      ELSE                                                         EL1501
01828          MOVE ' RESENT     : '               TO  FORM-RESEND-LIT  EL1501
01829          MOVE AT-FORM-REPRINT-DT             TO  DC-BIN-DATE-1    EL1501
01830          MOVE ' '                            TO  DC-OPTION-CODE   EL1501
01831          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01832          IF NO-CONVERSION-ERROR                                   EL1501
01833              MOVE DC-GREG-DATE-1-EDIT        TO  FORM-RESEND-DT   EL1501
01834          ELSE                                                     EL1501
01835              MOVE SPACES                     TO  FORM-RESEND-DT.  EL1501
01836                                                                   EL1501
01837      MOVE ' FOL: '                       TO  FORM-FOLLOW-UP-LIT.  EL1501
01838                                                                      CL*29
01839      IF AT-FORM-FOLLOW-UP-DT = LOW-VALUES                            CL*24
01840          MOVE SPACES                     TO  FORM-FOLLOW-UP-DT    EL1501
01841      ELSE                                                         EL1501
01842          MOVE AT-FORM-FOLLOW-UP-DT       TO  DC-BIN-DATE-1        EL1501
01843          MOVE ' '                        TO  DC-OPTION-CODE       EL1501
01844          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01845          IF NO-CONVERSION-ERROR                                   EL1501
01846              MOVE DC-GREG-DATE-1-EDIT    TO  FORM-FOLLOW-UP-DT    EL1501
01847          ELSE                                                     EL1501
01848              MOVE SPACES                 TO  FORM-FOLLOW-UP-DT.   EL1501
01849                                                                   EL1501
01850      MOVE 'REC '                      TO  FORM-HDG2-LIT.          EL1501
01851      MOVE 'INS : '                    TO  FORM-REC-INS-LIT.       EL1501
01852                                                                      CL*29
01853      IF AT-FORM-ANSWERED-DT = LOW-VALUES                             CL*29
01854          MOVE SPACES                  TO  FORM-REC-INS-DT         EL1501
01855      ELSE                                                         EL1501
01856          MOVE AT-FORM-ANSWERED-DT     TO  DC-BIN-DATE-1           EL1501
01857          MOVE ' '                     TO  DC-OPTION-CODE          EL1501
01858          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01859          IF NO-CONVERSION-ERROR                                   EL1501
01860              MOVE DC-GREG-DATE-1-EDIT TO  FORM-REC-INS-DT         EL1501
01861          ELSE                                                     EL1501
01862              MOVE SPACES              TO  FORM-REC-INS-DT.        EL1501
01863                                                                   EL1501
01864      MOVE ' REC PHY: '                 TO  FORM-REC-PHY-LIT.      EL1501
01865                                                                      CL*29
01866      IF AT-PHY-FORM-ANSWERED-DT = LOW-VALUES                         CL*24
01867          MOVE SPACES                   TO  FORM-REC-PHY-DT        EL1501
01868      ELSE                                                         EL1501
01869          MOVE AT-PHY-FORM-ANSWERED-DT  TO  DC-BIN-DATE-1          EL1501
01870          MOVE ' '                      TO  DC-OPTION-CODE         EL1501
01871          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01872          IF NO-CONVERSION-ERROR                                   EL1501
01873              MOVE DC-GREG-DATE-1-EDIT  TO  FORM-REC-PHY-DT        EL1501
01874          ELSE                                                     EL1501
01875              MOVE SPACES               TO  FORM-REC-PHY-DT.       EL1501
01876                                                                   EL1501
01877      MOVE ' REC EMP    : '             TO  FORM-REC-EMP-LIT.      EL1501
01878                                                                      CL*29
01879      IF AT-EMP-FORM-ANSWERED-DT = LOW-VALUES                         CL*24
01880          MOVE SPACES                   TO  FORM-REC-EMP-DT        EL1501
01881      ELSE                                                         EL1501
01882          MOVE AT-EMP-FORM-ANSWERED-DT  TO  DC-BIN-DATE-1          EL1501
01883          MOVE ' '                      TO  DC-OPTION-CODE         EL1501
01884          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL1501
01885          IF NO-CONVERSION-ERROR                                   EL1501
01886              MOVE DC-GREG-DATE-1-EDIT  TO  FORM-REC-EMP-DT        EL1501
01887          ELSE                                                     EL1501
01888              MOVE SPACES               TO  FORM-REC-EMP-DT.       EL1501
01889                                                                   EL1501
01890      MOVE FORM-HDG1              TO  MAP-HDG        (DISPLAY-CNT).EL1501
01891      MOVE FORM-TEXT-1            TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01892      MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).EL1501
01893      MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01894      ADD +1 TO DISPLAY-CNT.                                          CL*29
01895      MOVE FORM-HDG2              TO  MAP-HDG        (DISPLAY-CNT).EL1501
01896      MOVE FORM-TEXT-2            TO  MAP-TEXT       (DISPLAY-CNT).EL1501
01897      MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT) EL1501
01898                                      MAP-TEXT-ATTRB (DISPLAY-CNT).EL1501
01899      MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).      EL1501
01900      MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).      EL1501
01901      MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).      EL1501
01902                                                                   EL1501
01903      GO TO 2800-INCR-DISPLAY-CNT.                                 EL1501
01904                                                                   EL1501
01905  2800-INCR-DISPLAY-CNT.                                           EL1501
01906                                                                   EL1501
01907      IF DIRECTION-SWITCH = 'F'                                    EL1501
01908          ADD +1 TO DISPLAY-CNT                                       CL*29
01909                    PI-LINE-NO                                        CL*29
01910                    SUB-1                                             CL*29
01911          IF DISPLAY-CNT > +16                                        CL*29
01912              GO TO 2999-EXIT                                      EL1501
01913          ELSE                                                     EL1501
01914              NEXT SENTENCE                                        EL1501
01915      ELSE                                                         EL1501
01916          SUBTRACT +3 FROM DISPLAY-CNT                             EL1501
01917          SUBTRACT +1 FROM PI-LINE-NO                              EL1501
01918          SUBTRACT +1 FROM SUB-1                                   EL1501
01919          IF DISPLAY-CNT < +1                                         CL*29
01920              GO TO 2999-EXIT.                                     EL1501
01921                                                                   EL1501
01922      IF DIRECTION-SWITCH = 'B'                                       CL*24
01923          GO TO 2060-BROWSE-BACKWARD                               EL1501
01924      ELSE                                                         EL1501
01925          GO TO 2020-BROWSE-FORWARD.                               EL1501
01926                                                                   EL1501
01927  2950-NO-MORE-TRAILERS.                                           EL1501
01928      MOVE ER-0303                TO  EMI-ERROR.                   EL1501
01929      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1501
01930                                                                   EL1501
01931  2999-EXIT.                                                       EL1501
01932      EXIT.                                                        EL1501
01933                                                                   EL1501
01934      EJECT                                                        EL1501
01935  3000-RECEIVE-LETTERS.                                            EL1501
01936                                                                   EL1501
01937      MOVE LINENOI                TO  SUB-1.                       EL1501
01938                                                                   EL1501
01939      IF PI-TRLR-TYPE (SUB-1) = '4'                                   CL*24
01940          NEXT SENTENCE                                            EL1501
01941      ELSE                                                         EL1501
01942      IF PI-TRLR-TYPE (SUB-1) = '2'                                   CL*29
01943          MOVE ER-0667            TO  EMI-ERROR                       CL*29
01944          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*29
01945          MOVE -1                 TO  LINENOL                         CL*29
01946          GO TO 8200-SEND-DATAONLY                                    CL*29
01947      ELSE                                                            CL*29
01948      IF PI-TRLR-TYPE (SUB-1) = 'A'                                   CL*29
01949          MOVE ER-0665            TO  EMI-ERROR                       CL*29
01950          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*29
01951          MOVE -1                 TO  LINENOL                         CL*29
01952          GO TO 8200-SEND-DATAONLY                                    CL*29
01953      ELSE                                                            CL*29
01954          MOVE ER-0660            TO  EMI-ERROR                       CL*29
01955          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*29
01956          MOVE -1                 TO  LINENOL                         CL*29
01957          GO TO 8200-SEND-DATAONLY.                                   CL*29
01958                                                                   EL1501
01959      IF RECVDTI = SPACES                                             CL*24
01960          MOVE LOW-VALUES                   TO  WS-RECEIVED-DATE   EL1501
01961      ELSE                                                         EL1501
01962          MOVE RECVDTI                      TO  WS-DEEDIT-FIELD    EL1501
01963          PERFORM 9800-DEEDIT THRU 9800-EXIT                       EL1501
01964          IF WS-DEEDIT-FIELD-V0 NUMERIC                               CL*29
01965              MOVE '4'                      TO  DC-OPTION-CODE     EL1501
01966              MOVE WS-DEEDIT-FIELD-V0       TO  DC-GREG-DATE-1-MDY EL1501
01967              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1501
01968              IF NO-CONVERSION-ERROR                               EL1501
01969                  MOVE DC-BIN-DATE-1        TO  WS-RECEIVED-DATE   EL1501
01970                  MOVE DC-GREG-DATE-1-EDIT  TO  RECVDTO            EL1501
01971                  MOVE AL-UANON             TO  RECVDTA            EL1501
01972              ELSE                                                 EL1501
01973                  MOVE LOW-VALUES           TO  WS-RECEIVED-DATE.  EL1501
01974                                                                   EL1501
01975      MOVE PI-TRLR-SEQ-NO (SUB-1) TO  TRLR-SEQ-NO.                 EL1501
01976      PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.                EL1501
01977                                                                   EL1501
01978      MOVE WS-RECEIVED-DATE       TO  AT-LETTER-ANSWERED-DT.       EL1501
01979      MOVE PI-PROCESSOR-ID        TO  AT-CORR-LAST-UPDATED-BY.     EL1501
01980      MOVE SAVE-BIN-DATE          TO  AT-CORR-LAST-MAINT-DT.       EL1501
01981                                                                   EL1501
01982      PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.                    EL1501
01983                                                                   EL1501
01984      PERFORM 7500-READ-CLAIM-MSTR-UPDATE THRU 7500-EXIT.          EL1501
01985                                                                      CL*21
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*29
121802*        MOVE 11                 TO CL-ACTIVITY-CODE                 CL*21
121802*        MOVE SAVE-BIN-DATE      TO CL-ACTIVITY-MAINT-DT             CL*21
121802*        MOVE 'CORR'             TO CL-ACTIVITY-MAINT-TYPE           CL*21
121802*        MOVE PI-PROCESSOR-ID    TO CL-PROCESSOR-ID.                 CL*21
01991                                                                      CL*21
01992      PERFORM 7600-REWRITE-CLAIM-MSTR THRU 7600-EXIT.              EL1501
01993                                                                   EL1501
01994      MOVE ER-0000                TO  EMI-ERROR.                   EL1501
01995      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1501
01996      MOVE -1                     TO  LINENOL.                     EL1501
01997      MOVE AL-UANOF               TO  LINENOA  RECVDTA             EL1501
01998                                      RECVTYPA.                    EL1501
01999      MOVE LOW-VALUES             TO  EL150BO.                     EL1501
02000      MOVE 'F'                    TO  DIRECTION-SWITCH             EL1501
02001                                      PI-PREV-DIRECTION.           EL1501
02002      MOVE 'Y'                    TO  PI-FIRST-TIME-SW.            EL1501
02003      MOVE PI-TRLR-SEQ-NO (1)     TO  PI-PREV-SEQ-NO.              EL1501
02004      SUBTRACT +1 FROM PI-PREV-SEQ-NO.                             EL1501
02005      GO TO 1000-SHOW-CLAIM-HISTORY.                               EL1501
02006                                                                   EL1501
02007      EJECT                                                        EL1501
02008  4000-RECEIVE-FORMS.                                              EL1501
02009                                                                   EL1501
02010      MOVE LINENOI                TO  SUB-1.                       EL1501
02011                                                                   EL1501
02012      IF PI-TRLR-TYPE (SUB-1) = 'A'                                   CL*24
02013          NEXT SENTENCE                                            EL1501
02014      ELSE                                                         EL1501
02015      IF PI-TRLR-TYPE (SUB-1) = '4'                                   CL*29
02016          MOVE ER-0666            TO  EMI-ERROR                       CL*29
02017          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*29
02018          MOVE -1                 TO  LINENOL                         CL*29
02019          GO TO 8200-SEND-DATAONLY                                    CL*29
02020      ELSE                                                            CL*29
02021      IF PI-TRLR-TYPE (SUB-1) = '2'                                   CL*29
02022          MOVE ER-0667            TO  EMI-ERROR                       CL*29
02023          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*29
02024          MOVE -1                 TO  LINENOL                         CL*29
02025          GO TO 8200-SEND-DATAONLY                                    CL*29
02026      ELSE                                                            CL*29
02027          MOVE ER-0661            TO  EMI-ERROR                       CL*29
02028          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*29
02029          MOVE -1                 TO  LINENOL                         CL*29
02030          GO TO 8200-SEND-DATAONLY.                                   CL*29
02031                                                                   EL1501
02032      IF RECVTYPI = 'I' OR 'P' OR 'E'                                 CL*24
02033          NEXT SENTENCE                                            EL1501
02034      ELSE                                                         EL1501
02035          MOVE ER-0662            TO  EMI-ERROR                    EL1501
02036          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL1501
02037          MOVE -1                 TO  RECVTYPL                     EL1501
02038          GO TO 8200-SEND-DATAONLY.                                EL1501
02039                                                                   EL1501
02040      IF RECVDTI = SPACES                                             CL*24
02041          MOVE LOW-VALUES                   TO  WS-RECEIVED-DATE   EL1501
02042      ELSE                                                         EL1501
02043          MOVE RECVDTI                      TO  WS-DEEDIT-FIELD    EL1501
02044          PERFORM 9800-DEEDIT THRU 9800-EXIT                       EL1501
02045          IF WS-DEEDIT-FIELD-V0 NUMERIC                               CL*29
02046              MOVE '4'                      TO  DC-OPTION-CODE     EL1501
02047              MOVE WS-DEEDIT-FIELD-V0       TO  DC-GREG-DATE-1-MDY EL1501
02048              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL1501
02049              IF NO-CONVERSION-ERROR                               EL1501
02050                  MOVE DC-BIN-DATE-1        TO  WS-RECEIVED-DATE   EL1501
02051                  MOVE DC-GREG-DATE-1-EDIT  TO  RECVDTO            EL1501
02052                  MOVE AL-UANON             TO  RECVDTA            EL1501
02053              ELSE                                                 EL1501
02054                  MOVE LOW-VALUES           TO  WS-RECEIVED-DATE.  EL1501
02055                                                                   EL1501
02056      MOVE PI-TRLR-SEQ-NO (SUB-1)     TO  TRLR-SEQ-NO.             EL1501
02057      PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.                EL1501
02058                                                                   EL1501
02059      IF RECVTYPI = 'I'                                               CL*24
02060          MOVE WS-RECEIVED-DATE       TO  AT-FORM-ANSWERED-DT      EL1501
02061      ELSE                                                         EL1501
02062      IF RECVTYPI = 'P'                                               CL*29
02063          MOVE WS-RECEIVED-DATE       TO  AT-PHY-FORM-ANSWERED-DT     CL*29
02064      ELSE                                                            CL*29
02065          MOVE WS-RECEIVED-DATE       TO  AT-EMP-FORM-ANSWERED-DT.    CL*29
02066                                                                   EL1501
02067      MOVE PI-PROCESSOR-ID            TO  AT-FORM-LAST-UPDATED-BY. EL1501
02068      MOVE SAVE-BIN-DATE              TO  AT-FORM-LAST-MAINT-DT.   EL1501
02069                                                                   EL1501
02070      PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.                    EL1501
02071                                                                   EL1501
02072      MOVE ER-0000                TO  EMI-ERROR.                   EL1501
02073      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1501
02074      MOVE -1                     TO  LINENOL.                     EL1501
02075      MOVE AL-UANOF               TO  LINENOA  RECVDTA             EL1501
02076                                      RECVTYPA.                    EL1501
02077      MOVE LOW-VALUES             TO  EL150BO.                     EL1501
02078      MOVE 'F'                    TO  DIRECTION-SWITCH             EL1501
02079                                      PI-PREV-DIRECTION.           EL1501
02080      MOVE 'Y'                    TO  PI-FIRST-TIME-SW.            EL1501
02081      MOVE PI-TRLR-SEQ-NO (1)     TO  PI-PREV-SEQ-NO.              EL1501
02082      SUBTRACT +1 FROM PI-PREV-SEQ-NO.                             EL1501
02083      GO TO 1000-SHOW-CLAIM-HISTORY.                               EL1501
02084                                                                   EL1501
02085      EJECT                                                        EL1501
02086                                                                   EL1501
02087  5000-VOID-PAYMENT.                                               EL1501
02088                                                                   EL1501
02089      MOVE LINENOI                TO  SUB-1.                       EL1501
02090                                                                   EL1501
02091      IF PI-TRLR-TYPE (SUB-1) = '2'                                   CL*24
02092          NEXT SENTENCE                                            EL1501
02093      ELSE                                                         EL1501
02094      IF PI-TRLR-TYPE (SUB-1) = '4'                                   CL*29
02095          MOVE ER-0666            TO  EMI-ERROR                       CL*29
02096          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*29
02097          MOVE -1                 TO  LINENOL                         CL*29
02098          GO TO 8200-SEND-DATAONLY                                    CL*29
02099      ELSE                                                            CL*29
02100      IF PI-TRLR-TYPE (SUB-1) = 'A'                                   CL*29
02101          MOVE ER-0665            TO  EMI-ERROR                       CL*29
02102          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*29
02103          MOVE -1                 TO  LINENOL                         CL*29
02104          GO TO 8200-SEND-DATAONLY                                    CL*29
02105      ELSE                                                            CL*29
02106          MOVE ER-0664            TO  EMI-ERROR                       CL*29
02107          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*29
02108          MOVE -1                 TO  LINENOL                         CL*29
02109          GO TO 8200-SEND-DATAONLY.                                   CL*29
02110                                                                   EL1501
02111      PERFORM 7500-READ-CLAIM-MSTR-UPDATE THRU 7500-EXIT.          EL1501
02112                                                                   EL1501
02113      MOVE PI-TRLR-SEQ-NO (SUB-1) TO  TRLR-SEQ-NO.                 EL1501
02114      PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.                EL1501
02115                                                                   EL1501
040913     IF AT-CHECK-WRITTEN-DT > SAVE-BIN-DATE
040913         MOVE ER-2893            TO EMI-ERROR
040913         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040913         MOVE -1                 TO LINENOL
040913         PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
040913         PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT
040913         GO TO 8200-SEND-DATAONLY
040913     END-IF.
040913
02116      IF AT-VOID-DT = LOW-VALUES OR SPACES                            CL*28
02117          NEXT SENTENCE                                               CL*18
02118      ELSE                                                            CL*18
02119          MOVE ER-0800            TO  EMI-ERROR                       CL*18
02120          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02121          MOVE -1                 TO  LINENOL                         CL*18
02122          PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT               CL*18
02123          PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT               CL*28
02124          GO TO 8200-SEND-DATAONLY.                                   CL*18

013017     if at-ach-payment = 'Y'
013017        if pi-approval-level = '4' or '5'
                 perform 5320-build-email
                                       thru 5320-exit
013017        else
013017           MOVE ER-8162          TO  EMI-ERROR
013017           PERFORM 9900-ERROR-FORMAT
013017                                 THRU 9900-EXIT
013017           MOVE -1               TO  LINENOL
013017           PERFORM 7510-UNLOCK-CLAIM-MSTR
013017                                 THRU 7510-EXIT
013017           PERFORM 7010-UNLOCK-TRLR
013017                                 THRU 7010-EXIT
013017           GO TO 8200-SEND-DATAONLY
013017        end-if
013017     end-if

121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*28
121802*        IF AT-CASH-PAYMENT = 'N'                                    CL*24
121802*            IF AT-CHECK-WRITTEN-DT = SPACES OR LOW-VALUES           CL*29
121802*                MOVE ER-0833    TO  EMI-ERROR                       CL*18
121802*                MOVE -1         TO  LINENOL                         CL*18
121802*                PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT       CL*28
121802*                PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT       CL*18
121802*                PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT       CL*28
121802*                GO TO 8200-SEND-DATAONLY.                           CL*18
121802*                                                                    CL*18
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*28
121802*      IF AT-CASH-PAYMENT = 'N'                                      CL*24
121802*        IF AT-RECORDED-DT = SAVE-BIN-DATE                           CL*24
121802*          NEXT SENTENCE                                             CL*18
121802*        ELSE                                                        CL*18
121802*          IF PI-PROCESSOR-USER-ALMIGHTY = 'Y'                       CL*24
121802*            NEXT SENTENCE                                           CL*18
121802*          ELSE                                                      CL*18
121802*            MOVE ER-0816        TO  EMI-ERROR                       CL*18
121802*            MOVE -1             TO  LINENOL                         CL*18
121802*            PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT           CL*28
121802*            PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT           CL*18
121802*            PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT           CL*28
121802*            GO TO 8200-SEND-DATAONLY.                               CL*18
121802*                                                                    CL*18
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*28
121802*        IF CLAIM-IS-CLOSED                                          CL*28
121802*            IF SETUP-ERRORS                                         CL*28
121802*                MOVE ER-0941    TO EMI-ERROR                        CL*21
121802*                MOVE -1         TO LINENOL                          CL*21
121802*                PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT       CL*28
121802*                PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT       CL*28
121802*                PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT       CL*28
121802*                GO TO 8200-SEND-DATAONLY                            CL*21
121802*            ELSE                                                    CL*21
121802*                IF BENEFITS-CHANGED                                 CL*28
121802*                    IF SYSTEM-MODIFY-CAP                            CL*28
121802*                        NEXT SENTENCE                               CL*21
121802*                    ELSE                                            CL*21
121802*                        MOVE ER-0942    TO EMI-ERROR                CL*28
121802*                        MOVE -1         TO LINENOL                  CL*28
121802*                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    CL*21
121802*                        PERFORM 7510-UNLOCK-CLAIM-MSTR              CL*21
121802*                                                  THRU 7510-EXIT    CL*28
121802*                        PERFORM 7010-UNLOCK-TRLR  THRU 7010-EXIT    CL*28
121802*                        GO TO 8200-SEND-DATAONLY.                   CL*21
121802*                                                                    CL*21
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*28
121802*        IF AT-CASH-PAYMENT = 'N'                                    CL*28
121802*            IF SYSTEM-MODIFY-CAP                                    CL*28
121802*                NEXT SENTENCE                                       CL*21
121802*            ELSE                                                    CL*21
121802*                IF AT-RECORDED-DT = SAVE-BIN-DATE                   CL*29
121802*                    NEXT SENTENCE                                   CL*21
121802*                ELSE                                                CL*21
121802*                    MOVE ER-0920  TO EMI-ERROR                      CL*28
121802*                    MOVE -1       TO LINENOL                        CL*28
121802*                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        CL*21
121802*                    PERFORM 7510-UNLOCK-CLAIM-MSTR                  CL*21
121802*                                             THRU 7510-EXIT         CL*28
121802*                    PERFORM 7010-UNLOCK-TRLR THRU 7010-EXIT         CL*21
121802*                    GO TO 8200-SEND-DATAONLY.                       CL*21
121802*                                                                    CL*21
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*28
121802*       IF AT-PAYMENT-TYPE NOT = '4' AND '5' AND '6'                 CL*29
121802*          MOVE 'O'              TO  CL-CLAIM-STATUS                 CL*29
121802*       END-IF                                                       CL*29
121802*       PERFORM 5300-CREATE-DMO THRU 5300-EXIT.                      CL*29
02194                                                                      CL*24
061013*    if pi-company-id = 'DCC' or 'VPP'
061013     if at-payment-type not = '5' and '6' and 'I'
061013        perform 5300-upd-cert-trlr thru 5300-exit
061013     end-if
02195      MOVE AT-CHECK-WRITTEN-DT    TO  WS-CHECK-WRITTEN-DT.         EL1501
02196      MOVE AT-PAYMENT-APPROVAL-SW TO  WS-PMT-APPROVAL-SW.          EL1501
02197      MOVE AT-AMOUNT-PAID         TO  WS-AMOUNT-PAID.              EL1501
02198      MOVE AT-PAYMENT-ORIGIN      TO  WS-PAYMENT-ORIGIN.              CL**2
02199      MOVE AT-CV-PMT-CODE         TO  WS-CV-PMT-CODE.                 CL*19
02200                                                                   EL1501
022718     IF AT-TO-BE-WRITTEN-DT > ZERO
022718       AND AT-PAID-THRU-DT > CL-PAID-THRU-DT
022718        CONTINUE
022718     ELSE
022106     IF AT-PAYMENT-TYPE NOT = '5' AND '6' AND 'I'                    CL*28
02202          SUBTRACT AT-AMOUNT-PAID    FROM CL-TOTAL-PAID-AMT           CL*28
02203          SUBTRACT AT-DAYS-IN-PERIOD FROM CL-NO-OF-DAYS-PAID       EL1501
02204          IF AT-PAYMENT-TYPE NOT = '4'                                CL*28
02205              SUBTRACT +1 FROM CL-NO-OF-PMTS-MADE                  EL1501
02206              IF (AT-PAID-THRU-DT NOT = CL-PAID-THRU-DT) OR           CL*29
02207                 (AT-RECORDED-BY = 'ZZZZ')                            CL*29
02208                  NEXT SENTENCE                                    EL1501
02209              ELSE                                                 EL1501
02210                  MOVE AT-PREV-LAST-PMT-DT    TO  CL-LAST-PMT-DT   EL1501
02211                  MOVE AT-PREV-PAID-THRU-DT   TO  CL-PAID-THRU-DT  EL1501
02212                  MOVE AT-PREV-LAST-PMT-AMT   TO  CL-LAST-PMT-AMT. EL1501
031214
031214     IF AT-PAYMENT-TYPE = 'I'
031214        IF CL-TOTAL-INT-PAID NUMERIC AND 
031214           CL-TOTAL-INT-PAID NOT LESS THAN AT-AMOUNT-PAID
031214              SUBTRACT AT-AMOUNT-PAID FROM CL-TOTAL-INT-PAID
031214        END-IF
031214     END-IF.
02213                                                                   EL1501
02214      IF CL-NO-OF-DAYS-PAID < ZERO                                    CL*29
02215          MOVE +0                 TO  CL-NO-OF-DAYS-PAID.          EL1501
02216                                                                   EL1501
02217      IF CL-NO-OF-PMTS-MADE < ZERO                                    CL*29
02218          MOVE +0                 TO  CL-NO-OF-PMTS-MADE.          EL1501
02219                                                                   EL1501
02220      MOVE SAVE-BIN-DATE          TO  AT-VOID-DT                   EL1501
02221                                      CL-LAST-REOPEN-DT.           EL1501
02222                                                                   EL1501
02223      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL1501
02224      MOVE '1'                    TO  CNTL-REC-TYPE.               EL1501
02225      MOVE SPACES                 TO  CNTL-ACCESS.                 EL1501
02226      MOVE +0                     TO  CNTL-SEQ-NO.                 EL1501
02227      MOVE 'CNTL'                 TO  FILE-SWITCH.                    CL**8
02228                                                                   EL1501
02229      PERFORM 7900-READ-CONTROL-FILE THRU 7900-EXIT.               EL1501
02230                                                                   EL1501
02231      MOVE CF-PAYMENT-APPROVAL-SW TO  WS-CF-PMT-APPROVAL-SW.          CL*17
02232                                                                      CL*17
02233      IF CF-PMT-APPROVAL-USED                                         CL*13
02234          MOVE 'V'                TO  AT-PAYMENT-APPROVAL-SW.      EL1501
CIDMOD                                                                  EL1501
121802*    IF PI-COMPANY-ID = 'CID'                                          000
CIDMOD         PERFORM 9870-OUTPUT-ACTIVITY-RECORD THRU                      000
CIDMOD                 9870-EXIT.                                            000
CIDMOD         IF ERROR-ON-OUTPUT                                            000
CIDMOD             MOVE -1             TO ENTERPFL                           000
CIDMOD             MOVE AL-UANON       TO ENTERPFA                           000
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU                            000
CIDMOD                     9900-EXIT                                         000
CIDMOD             GO TO 8200-SEND-DATAONLY                                  000
CIDMOD         END-IF.                                                       000
121802*    END-IF.                                                           000
CIDMOD                                                                       000
02236 ******************************************************************   CL*18
02237 **  1.  BYPASS READING THE RECON RECORD FOR THE FOLLOWING       **   CL*18
02238 **      REASONS:                                                **   CL*18
02239 **      A.  NON-CASH PAYMENT                                    **   CL*18
02240 **      B.  CHECK HAS NOT BEEN PRINTED                          **   CL*18
02241 **      C.  USER IS NOT A RECON USER                            **   CL*18
02242 **      D.  PAYMENT IS A MANUAL (OFFLINE) PAYMENT               **   CL*19
02243 ******************************************************************   CL*18
02244                                                                      CL*18
02245      IF AT-CASH-PAYMENT = 'N'                                        CL*24
02246          GO TO 5005-CONT-VOID.                                       CL*18
02247                                                                      CL*18
02248      IF AT-CHECK-NO  = SPACES OR LOW-VALUES                          CL*28
02249          GO TO 5005-CONT-VOID.                                       CL*18
02250                                                                      CL*18
02251      IF CF-CLAIMS-CHECK-RECON-USER NOT = 'Y'                         CL*24
02252          GO TO 5005-CONT-VOID.                                       CL*19
02253                                                                      CL*19
02254      IF OFFLINE-PMT                                                  CL*19
02255          GO TO 5005-CONT-VOID.                                       CL*18
02256                                                                      CL*18
02257 ******************************************************************   CL*18
02258 **  1.  RECON SW VALUES = :                                     **   CL*18
02259 **      A.  R = CHECK HAS BEEN REDEEMED - CANNOT BE VOIDED      **   CL*18
02260 **      B.  X = RECON RECORD NOT FOUND                          **   CL*18
02261 ******************************************************************   CL*18
02262                                                                      CL*18
02263      MOVE 'RCON'                     TO  FILE-SWITCH.                CL*18
02264      PERFORM 5700-UPDATE-RECON THRU 5700-EXIT.                       CL*18
02265      IF WS-RECON-SW = 'R'                                            CL*24
02266           PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT              CL*18
02267           PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT              CL*28
02268           GO TO 8200-SEND-DATAONLY.                                  CL*18
02269                                                                      CL*18
121802*    IF WS-RECON-SW = 'X'                                            CL*24
121802*        IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CIG' OR 'CUK'         CL*28
121802*            PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT           CL*18
121802*            PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT           CL*28
121802*            GO TO 8200-SEND-DATAONLY.                               CL*18
02275                                                                      CL*18
02276  5005-CONT-VOID.                                                     CL*18
02277      IF AT-RECORDED-BY = 'ZZZZ'                                      CL*24
02278          GO TO 5010-BYPASS.                                       EL1501
02279                                                                   EL1501
02280      MOVE '7'                    TO  PI-PAY-TYPE.                    CL*15
02281      MOVE AT-PAYMENT-TYPE        TO  WS-PAY-TYPE.                 EL1501
02282                                                                      CL**8
121802*    IF PI-COMPANY-ID = 'DMD'                                        CL*30
121802*        MOVE 88888888           TO WS-CK-Q-CONTROL                  CL*30
121802*     ELSE                                                           CL*30
02286          MOVE 99999999           TO WS-CK-Q-CONTROL.                 CL*30
02287                                                                      CL*30
02288      IF AT-CHECK-QUE-CONTROL > ZEROS AND < WS-CK-Q-CONTROL           CL*30
02289          PERFORM 5200-UPDATE-CHECK-QUE THRU 5299-EXIT                CL*28
02290      ELSE                                                            CL**8
02291          IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES AND SPACES          CL*29
02292              MOVE 'Y'                TO  WS-PRINTED-SW               CL*10
02293                                          WS-RELEASED-SW              CL*10
02294          ELSE                                                        CL*10
02295              MOVE 'N'                TO  WS-PRINTED-SW               CL**8
02296                                          WS-RELEASED-SW.             CL**8
02297                                                                   EL1501
02298  5010-BYPASS.                                                     EL1501
02299                                                                      CL**8
02300      IF PAYMENT-HAS-BEEN-PRINTED OR                                  CL**9
02301         OFFLINE-PMT                                                  CL**9
02302          MOVE CF-CURRENT-MONTH-END   TO  AT-VOID-SELECT-DT           CL**8
02303      ELSE                                                            CL**8
02304          MOVE LOW-VALUES             TO  AT-PMT-SELECT-DT            CL**8
02305                                          AT-VOID-SELECT-DT.          CL**8
02306                                                                   EL1501
02307      MOVE WS-VOID-CODE           TO  AT-VOID-TYPE.                   CL*18
02308                                                                      CL*20
02309      MOVE PI-PROCESSOR-ID        TO  AT-PAYMENT-LAST-UPDATED-BY.     CL*20
02310      MOVE SAVE-BIN-DATE          TO  AT-PAYMENT-LAST-MAINT-DT.       CL*20
02311                                                                      CL*18
02312      PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.                       CL*14

071510     IF WS-PAY-TYPE NOT = 'I'
071510        PERFORM 5400-UPDATE-ZERO-TRAILER
071510                                 THRU 5400-EXIT
071510     END-IF

121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'                                  CL*24
121802*        PERFORM 5500-UPDATE-POLICY-MASTER THRU 5500-EXIT            CL*19
121802*    ELSE                                                            CL*19
02318          PERFORM 5600-UPDATE-CERT THRU 5600-EXIT.                    CL*19
02319                                                                   EL1501
02320      MOVE CL-CONTROL-PRIMARY     TO  ELACTQ-KEY.                  EL1501
02321                                                                      CL*15
071510     IF WS-PAY-TYPE = '4' OR '5' OR '6' OR 'I'
02323          GO TO 5020-CONTINUE-VOID.                                   CL*19
02324                                                                      CL*19
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'                                  CL*24
121802*        IF CL-NO-OF-PMTS-MADE > +0                                  CL*29
121802*            GO TO 5020-CONTINUE-VOID.                               CL*19
02328                                                                      CL*19
02329      MOVE 'O'                    TO  CL-CLAIM-STATUS.                CL*19
02330                                                                      CL*19
02331  5020-CONTINUE-VOID.                                                 CL*19
02332                                                                   EL1501
02333      PERFORM 7600-REWRITE-CLAIM-MSTR THRU 7600-EXIT.              EL1501
02334                                                                   EL1501
02335      IF WS-PAYMENT-ORIGIN = '3'                                      CL*24
02336          GO TO 5100-CONTINUE.                                     EL1501
02337                                                                   EL1501
02338      IF PAYMENT-HAS-BEEN-PRINTED AND                                 CL**8
02339         NOT WS-CF-PMT-APPROVAL-USED                                  CL*17
02340          GO TO 5100-CONTINUE.                                        CL**8
02341                                                                   EL1501
02342      MOVE 'ACTQ'                 TO  FILE-SWITCH.                    CL*18
02343      PERFORM 7700-READ-ELACTQ THRU 7799-EXIT.                        CL**8
02344                                                                   EL1501
02345  5100-CONTINUE.                                                   EL1501
02346                                                                   EL1501
02347      MOVE ER-0000                TO  EMI-ERROR.                   EL1501
02348      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1501
02349      MOVE -1                     TO  LINENOL.                     EL1501
02350      MOVE AL-UANOF               TO  LINENOA  RECVDTA             EL1501
02351                                      RECVTYPA.                    EL1501
02352      MOVE LOW-VALUES             TO  EL150BO.                     EL1501
02353      MOVE 'F'                    TO  DIRECTION-SWITCH             EL1501
02354                                      PI-PREV-DIRECTION.           EL1501
02355      MOVE 'Y'                    TO  PI-FIRST-TIME-SW.            EL1501
02356      MOVE PI-TRLR-SEQ-NO (1)     TO  PI-PREV-SEQ-NO.              EL1501
02357      SUBTRACT +1 FROM PI-PREV-SEQ-NO.                             EL1501
02358      GO TO 1000-SHOW-CLAIM-HISTORY.                               EL1501
02359                                                                   EL1501
02360  EJECT                                                            EL1501
02361  5200-UPDATE-CHECK-QUE.                                           EL1501
02362                                                                   EL1501
02363      MOVE PI-COMPANY-CD          TO  CHKQ-COMP-CD.                EL1501
02364      MOVE AT-CHECK-QUE-CONTROL   TO  CHKQ-CONTROL.                EL1501
02365      MOVE AT-CHECK-QUE-SEQUENCE  TO  CHKQ-SEQ-NO.                 EL1501
02366                                                                   EL1501
02367      EXEC CICS HANDLE CONDITION                                   EL1501
02368          NOTFND   (5290-NOTFND)                                      CL**8
02369      END-EXEC.                                                    EL1501
02370                                                                   EL1501
02371      EXEC CICS READ                                               EL1501
02372          DATASET   ('ELCHKQ')                                     EL1501
02373          RIDFLD    (ELCHKQ-KEY)                                   EL1501
02374          SET       (ADDRESS OF CHECK-QUE)                            CL*21
02375          UPDATE                                                   EL1501
02376      END-EXEC.                                                    EL1501
02377                                                                   EL1501
02378      MOVE 'Y'                    TO  WS-RELEASED-SW                  CL**8
02379                                                                      CL**8
02380      IF CQ-TIMES-PRINTED = +0                                        CL*24
02381          MOVE 'N'                TO  WS-PRINTED-SW                   CL**8
02382          GO TO 5210-DELETE-CHECK-QUE                                 CL**8
02383      ELSE                                                            CL**8
02384          MOVE 'Y'                TO  WS-PRINTED-SW.                  CL**8
02385                                                                      CL**8
02386      MOVE WS-VOID-CODE           TO  CQ-VOID-INDICATOR.              CL*18
02387                                                                   EL1501
02388      EXEC CICS REWRITE                                            EL1501
02389          DATASET   ('ELCHKQ')                                     EL1501
02390          FROM      (CHECK-QUE)                                    EL1501
02391      END-EXEC.                                                    EL1501
02392                                                                   EL1501
02393      GO TO 5299-EXIT.                                                CL**8
02394                                                                      CL**8
02395  5210-DELETE-CHECK-QUE.                                              CL**8
02396                                                                      CL**8
02397      EXEC CICS DELETE                                                CL**8
02398          DATASET  ('ELCHKQ')                                         CL**8
02399      END-EXEC.                                                       CL**8
02400                                                                      CL**8
02401      MOVE +0                     TO  AT-CHECK-QUE-CONTROL            CL**8
02402                                      AT-CHECK-QUE-SEQUENCE.          CL**8
02403                                                                      CL**8
02404      GO TO 5299-EXIT.                                                CL**8
02405                                                                      CL**8
02406  5290-NOTFND.                                                        CL**8
02407      MOVE 'N'                    TO  WS-PRINTED-SW                   CL**8
02408                                      WS-RELEASED-SW.                 CL**8
02409  5299-EXIT.                                                          CL**8
02410      EXIT.                                                        EL1501
02411                                  EJECT                               CL*21
061013 5300-UPD-CERT-TRLR.
061013
061013*    if cl-insured-claim = 'S'
061013*       move +2 to s1
061013*    else
061013*       MOVE +1 to s1
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
061013     MOVE CL-COMPANY-CD          TO CTRLR-COMP-CD
061013     MOVE CL-CERT-KEY-DATA       TO ELCRTT-KEY (2:21)
061013     MOVE CL-CERT-NO             TO CTRLR-CERT-NO
061013     MOVE 'B'                    TO CTRLR-REC-TYPE
061013
061013     EXEC CICS READ
061013        UPDATE
061013        DATASET   ('ELCRTT')
061013        RIDFLD    (ELCRTT-KEY)
061013        set       (address of CERTIFICATE-TRAILERS)
061013        RESP      (WS-RESPONSE)
061013     END-EXEC
061013
061013     IF RESP-NORMAL
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or (cl-claim-no = cs-claim-no (s1))
              end-perform

           if s1 < +25
061013        subtract at-amount-paid from cs-total-paid (s1)
061013        if cs-total-paid (s1) < zeros
061013           move zeros            to cs-total-paid (s1)
061013        end-if
061013        subtract at-days-in-period from cs-days-paid (s1)
061013        if cs-days-paid (s1) < zeros
061013           move zeros            to cs-days-paid (s1)
061013        end-if
100518        if cl-claim-type not = 'L' and 'P' AND 'O'
                 perform 5310-calc-rem-bens
                                       thru 5310-exit
              end-if
061013        exec cics rewrite
061013           dataset    ('ELCRTT')
061013           from       (certificate-trailers)
061013           resp       (ws-response)
061013        end-exec
061013     end-if              

061013     .
061013 5300-EXIT.
061013     EXIT.

       5310-calc-rem-bens.

           move cm-ah-orig-term        to ws-max-bens
           if cl-critical-period not = zeros and spaces
              move cl-critical-period  to ws-max-bens
           end-if

           move zeros to ws-tot-days-paid ws-tot-amt-paid
           perform varying s2 from +1 by +1 until
              (s2 > +24)
              or (cs-claim-no (s2) = spaces)
              if (cs-benefit-period (s2) = cl-benefit-period)
                 and (cs-insured-type (s2) = cl-insured-type)
                 and (cs-claim-type (s2) = cl-claim-type)
                 compute ws-tot-days-paid =
                    ws-tot-days-paid + cs-days-paid (s2)
                 compute ws-tot-amt-paid =
                    ws-tot-amt-paid + cs-total-paid (s2)
              end-if
           end-perform
           compute cs-remaining-bens (s1) =
              ws-max-bens / cm-ah-benefit-amt
           if cs-remaining-bens (s1) < zeros
              move zeros            to cs-remaining-bens (s1)
           end-if

           .
       5310-exit.
           exit.

       5320-build-email.

      *    move at-payees-name         to ws-email-payee
      *    move at-amount-paid         to ws-email-check-amt
      *    move pi-company-id          to ws-email-client-id
      *    move at-check-no            to ws-email-check-no
      *    move at-claim-no            to ws-email-claim-no
      *    display ' email string **' ws-email-string '**'
      *
      *    call "SYSTEM" using ws-email-string
      *       returning ws-email-return-cd
      *
      *    display ' email return code ' ws-email-return-cd

           .
       5320-exit.
           exit.

121802*5300-CREATE-DMO. Remove as dead code                                CL*21
121802*5300-CONT. Remove as dead code                                      CL*22
121802*5300-NOTE-NOT-FOUND. Remove as dead code                            CL*21
121802*5300-EXIT. Remove as dead code                                      CL*21
121802*5350-FORMAT-LAST-NAME-1ST. Remove as dead code                      CL*21
121802*5350-EXIT. Remove as dead code                                      CL*21
121802*5360-MOVE-NAME. Remove as dead code                                 CL*21
121802*5360-MOVE-NAME-CYCLE. Remove as dead code                           CL*21
121802*5360-EXIT. Remove as dead code                                      CL*21
02832                                  EJECT                               CL*21
02833  5400-UPDATE-ZERO-TRAILER.                                        EL1501
02834                                                                   EL1501
02835      MOVE ELMSTR-KEY             TO  ELTRLR-KEY.                  EL1501
02836                                                                   EL1501
02837      MOVE ZEROS                  TO  TRLR-SEQ-NO.                 EL1501
02838      PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.                EL1501
02839                                                                   EL1501
02840      IF WS-PAY-TYPE = '5'                                            CL*24
02841          SUBTRACT WS-AMOUNT-PAID FROM AT-ITD-CHARGEABLE-EXPENSE.  EL1501
02842                                                                   EL1501
02843      IF WS-PAY-TYPE = '6'                                            CL*24
02844          SUBTRACT WS-AMOUNT-PAID FROM AT-ITD-PAID-EXPENSES.       EL1501
02845                                                                   EL1501
02846      IF AT-INITIAL-MANUAL-RESERVE NOT = ZEROS                        CL*24
02847          ADD WS-AMOUNT-PAID      TO  AT-CURRENT-MANUAL-RESERVE.   EL1501
02848                                                                   EL1501
02849  5410-CHECK-OPEN-CLOSE.                                           EL1501
02850                                                                   EL1501
02851      IF PI-PAY-TYPE = '5' OR '6'                                     CL*28
02852          PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.                   CL*15
02853                                                                      CL*15
02854      IF PI-PAY-TYPE = '1' OR '4' OR '7'                              CL*29
02855         IF CLAIM-IS-OPEN                                             CL*29
02856            PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT                  CL*28
02857            GO TO 5400-EXIT.                                          CL*28
02858                                                                   EL1501
02859      IF PI-PAY-TYPE = '2' OR '3'                                     CL*29
02860         IF CLAIM-IS-CLOSED                                           CL*29
02861             PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT                 CL*28
02862             GO TO 5400-EXIT.                                         CL*28
02863                                                                   EL1501
02864      MOVE 1                      TO  SUB.                         EL1501
02865                                                                   EL1501
02866  5420-LOOP.                                                       EL1501
02867                                                                   EL1501
02868      IF AT-OPEN-CLOSE-TYPE (SUB) = SPACES                            CL*24
02869          MOVE SAVE-BIN-DATE      TO  AT-OPEN-CLOSE-DATE (SUB)     EL1501
02870          MOVE 'O'                TO  AT-OPEN-CLOSE-TYPE (SUB)     EL1501
02871          MOVE 'FORCE'            TO  AT-OPEN-CLOSE-REASON (SUB)   EL1501
02872          PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT                 EL1501
02873          GO TO 5400-EXIT.                                         EL1501
02874                                                                   EL1501
02875      IF SUB = 6                                                      CL*24
02876       MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1) EL1501
02877       MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2) EL1501
02878       MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3) EL1501
02879       MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4) EL1501
02880       MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5) EL1501
02881       MOVE SPACES                    TO AT-OPEN-CLOSE-HISTORY (6) EL1501
02882       GO TO 5420-LOOP.                                            EL1501
02883                                                                   EL1501
02884      ADD 1                       TO  SUB.                         EL1501
02885      GO TO 5420-LOOP.                                             EL1501
02886                                                                   EL1501
02887  5400-EXIT.                                                       EL1501
02888      EXIT.                                                        EL1501
02889                                                                   EL1501
02890      EJECT                                                        EL1501
121802*5500-UPDATE-POLICY-MASTER.                                          CL*19
121802*                                                                    CL*19
121802*    MOVE PI-COMPANY-CD          TO  PLCY-COMPANY-CD.                CL*19
121802*    MOVE CL-CERT-CARRIER        TO  PLCY-CARRIER.                   CL*19
121802*    MOVE CL-CERT-GROUPING       TO  PLCY-GROUPING.                  CL*19
121802*    MOVE CL-CERT-STATE          TO  PLCY-STATE.                     CL*19
121802*    MOVE CL-CERT-ACCOUNT        TO  PLCY-PRODUCER.                  CL*19
121802*    MOVE CL-CERT-EFF-DT         TO  PLCY-EFF-DT.                    CL*19
121802*    MOVE CL-CV-REFERENCE-NO     TO  PLCY-REFERENCE-NO.              CL*19
121802*                                                                    CL*19
121802*    EXEC CICS READ                                                  CL*19
121802*        DATASET   ('MPPLCY')                                        CL*19
121802*        RIDFLD    (EMPLCY-KEY)                                      CL*19
121802*        SET       (ADDRESS OF POLICY-MASTER)                        CL*21
121802*    END-EXEC.                                                       CL*19
121802*                                                                    CL*19
121802*    MOVE LOW-VALUES             TO WS-POLICY-UPDATE-WORKING-GRPS.   CL*26
121802*    MOVE +0                     TO  WS-CLAIM-PAYMENT-CNT.           CL*20
121802*                                                                    CL*20
121802*    MOVE PM-COMPANY-CD          TO  WS-COMPANY-CD.                  CL*19
121802*    MOVE PM-CARRIER             TO  WS-CARRIER.                     CL*19
121802*    MOVE PM-GROUPING            TO  WS-GROUPING.                    CL*19
121802*    MOVE PM-STATE               TO  WS-STATE.                       CL*19
121802*    MOVE PM-PRODUCER            TO  WS-PRODUCER.                    CL*19
121802*    MOVE PM-POLICY-EFF-DT       TO  WS-POLICY-EFF-DT.               CL*19
121802*    MOVE PM-REFERENCE-NUMBER    TO  WS-REFERENCE-NUMBER.            CL*19
121802*                                                                    CL*19
121802*    MOVE 'RW'                   TO  WS-EMPLCY-FUNCTION.             CL*19
121802*    MOVE PI-PROCESSOR-ID        TO  WS-LAST-CHANGE-PROCESSOR.       CL*19
121802*    MOVE SAVE-BIN-DATE          TO  WS-LAST-CHANGE-DT.              CL*19
121802*    MOVE EIBTIME                TO  WS-LAST-CHANGE-TIME.            CL*19
121802*                                                                    CL*19
121802*    IF CL-CLAIM-TYPE = 'A' 
121802*        GO TO 5500-UPDATE-AH-POLICY-DATA.                           CL*19
121802*                                                                    CL*19
121802*5500-UPDATE-LF-POLICY-DATA.                                         CL*19
121802*                                                                    CL*19
121802*    IF WS-PAY-TYPE = '2'                                            CL*24
02929 *** FROM AT-PAYMENT-TYPE - TYPES 1-6 IN CONVENIENCE VALUES           CL*26
121802*      IF WS-CV-PMT-CODE = '1' OR '2' OR '3' OR '4'                  CL*28
02931 *** LIFE / HALF LIFE / ADD / HALF ADD - CONV VALUES                  CL*26
121802*        COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -     CL*19
121802*                                        WS-AMOUNT-PAID              CL*19
121802*        COMPUTE WS-CLAIM-LIFE-ITD = PM-CLAIM-LIFE-ITD -             CL*19
121802*                                    WS-AMOUNT-PAID                  CL*19
121802*        COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.    CL*19
121802*                                                                    CL*19
121802*    IF WS-PAY-TYPE = '2'                                            CL*24
121802*      IF WS-CV-PMT-CODE = '5' OR '6'                                CL*28
02940 *** RIDER AND HALF RIDER - CONV VALUES                               CL*26
121802*        COMPUTE WS-CLAIM-RIDER-ITD = PM-CLAIM-RIDER-ITD -           CL*19
121802*                                        WS-AMOUNT-PAID.             CL*19
121802*                                                                    CL*19
121802*    IF WS-PAY-TYPE = '4'                                            CL*24
02945 *** ADDITIONAL PAYMENT - TYPE 8 CONV VALUES                          CL*26
121802*        COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -     CL*19
121802*                                        WS-AMOUNT-PAID              CL*19
121802*        COMPUTE WS-CLAIM-LIFE-ITD = PM-CLAIM-LIFE-ITD -             CL*19
121802*                                    WS-AMOUNT-PAID                  CL*19
121802*        COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.    CL*19
121802*                                                                    CL*19
121802*    IF WS-PAY-TYPE = '6'                                            CL*24
02953 *** NON CHARGEABLE EXPENSE - TYPE 7 IN CONVENIENCE                   CL*26
121802*        COMPUTE WS-CLAIM-EXPENSES-ITD = PM-CLAIM-EXPENSES-ITD -     CL*19
121802*                                        WS-AMOUNT-PAID.             CL*19
121802*                                                                    CL*26
121802*    IF PM-CLAIM-SETTLEMENT                                          CL*28
121802*        IF WS-CV-PMT-CODE = '1' OR '2' OR '3' OR '4'                CL*28
121802*            MOVE '6'             TO WS-CURRENT-STATUS               CL*28
121802*            IF PM-EXIT-DT > LOW-VALUES                              CL*29
121802*                MOVE HIGH-VALUES TO WS-EXIT-DT.                     CL*28
121802*                                                                    CL*19
121802*    GO TO 5500-UPDATE-CLAIM-HISTORY.                                CL*19
121802*                                                                    CL*19
121802*5500-UPDATE-AH-POLICY-DATA.                                         CL*19
121802*                                                                    CL*19
121802*    IF WS-PAY-TYPE = '1' OR '4'                                     CL*28
121802*        COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -     CL*19
121802*                                        WS-AMOUNT-PAID              CL*19
121802*        COMPUTE WS-CLAIM-AH-ITD = PM-CLAIM-AH-ITD -                 CL*19
121802*                                  WS-AMOUNT-PAID                    CL*19
121802*        COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.    CL*19
121802*                                                                    CL*19
121802*    IF WS-PAY-TYPE = '6'                                            CL*24
121802*        COMPUTE WS-CLAIM-EXPENSES-ITD = PM-CLAIM-EXPENSES-ITD -     CL*19
121802*                                        WS-AMOUNT-PAID.             CL*19
121802*                                                                    CL*26
121802*    IF PM-CLAIM-SETTLEMENT                                          CL*28
121802*        IF WS-CV-PMT-CODE = '2'                                     CL*28
121802*            MOVE '6'             TO WS-CURRENT-STATUS               CL*28
121802*            IF PM-EXIT-DT > LOW-VALUES                              CL*29
121802*                MOVE HIGH-VALUES TO WS-EXIT-DT                      CL*28
121802*            END-IF                                                  CL*26
121802*        ELSE                                                        CL*26
121802*            IF WS-CV-PMT-CODE = '1'                                 CL*28
121802*                    AND                                             CL*26
121802*               WS-CLAIM-PAYMENTS-ITD < PM-INS-TOTAL-BENEFIT         CL*29
121802*                MOVE '6'             TO WS-CURRENT-STATUS           CL*28
121802*                IF PM-EXIT-DT > LOW-VALUES                          CL*29
121802*                    MOVE HIGH-VALUES TO WS-EXIT-DT.                 CL*28
121802*                                                                    CL*19
121802*5500-UPDATE-CLAIM-HISTORY.                                          CL*19
121802*                                                                    CL*19
121802*    IF PM-CLAIM-ATTACH-CNT = +1 OR                                  CL*24
121802*       PM-CLAIM-INCURRED-DT = CL-INCURRED-DT                        CL*24
121802*        NEXT SENTENCE                                               CL*19
121802*    ELSE                                                            CL*19
121802*        GO TO 5500-FINISH-POLICY-UPDATE.                            CL*19
121802*                                                                    CL*19
121802*    IF (PM-CLAIM-ATTACH-CNT = +1  AND                               CL*29
121802*        CL-NO-OF-PMTS-MADE = +0)                                    CL*29
121802*               OR                                                   CL*28
121802*       (PM-CLAIM-PAYMENT-CNT = +0)                                  CL*24
121802*           MOVE +0              TO  WS-CLAIM-LAST-PAYMENT-AMT       CL*28
121802*           MOVE '1'             TO  WS-CLAIM-INTERFACE-SW           CL*28
121802*           MOVE HIGH-VALUES     TO  WS-CLAIM-INCURRED-DT            CL*28
121802*                                    WS-CLAIM-PAID-TO-DT             CL*19
121802*       ELSE                                                         CL*28
121802*           MOVE CL-PAID-THRU-DT TO  WS-CLAIM-PAID-TO-DT             CL*28
121802*           MOVE CL-LAST-PMT-AMT TO  WS-CLAIM-LAST-PAYMENT-AMT.      CL*28
121802*                                                                    CL*19
121802*5500-FINISH-POLICY-UPDATE.                                          CL*19
121802*                                                                    CL*19
121802*    IF WS-CLAIM-PAYMENT-CNT NEGATIVE                                CL*29
121802*        MOVE +0                 TO  WS-CLAIM-PAYMENT-CNT.           CL*19
121802*                                                                    CL*19
121802*    EXEC CICS LINK                                                  CL*19
121802*        PROGRAM     ('EMPLCY')                                      CL*19
121802*        COMMAREA    (WS-POLICY-MASTER-UPDATE-AREA)                  CL*19
121802*        LENGTH      (WS-PM-COMM-LNGTH)                              CL*19
121802*    END-EXEC.                                                       CL*19
121802*                                                                    CL*19
121802*    IF WS-EMPLCY-RETURN-CODE = LOW-VALUES                           CL*24
121802*        NEXT SENTENCE                                               CL*19
121802*    ELSE                                                            CL*19
121802*        MOVE ER-9211            TO  EMI-ERROR                       CL*19
121802*        MOVE -1                 TO  LINENOL                         CL*19
121802*        MOVE AL-UABON           TO  LINENOA                         CL*19
121802*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*19
121802*        EXEC CICS SYNCPOINT                                         CL*19
121802*            ROLLBACK                                                CL*19
121802*        END-EXEC                                                    CL*19
121802*        GO TO 8200-SEND-DATAONLY.                                   CL*19
121802*                                                                    CL*19
121802*5500-EXIT.                                                          CL*19
121802*    EXIT.                                                           CL*19
03037                                                                      CL*19
03038      EJECT                                                           CL*19
03039  5600-UPDATE-CERT.                                                EL1501
03040                                                                   EL1501
03041      MOVE PI-COMPANY-CD          TO  CERT-COMP-CD.                EL1501
03042      MOVE CL-CERT-CARRIER        TO  CERT-CARRIER.                EL1501
03043      MOVE CL-CERT-GROUPING       TO  CERT-GROUPING.               EL1501
03044      MOVE CL-CERT-STATE          TO  CERT-STATE.                  EL1501
03045      MOVE CL-CERT-ACCOUNT        TO  CERT-ACCOUNT.                EL1501
03046      MOVE CL-CERT-EFF-DT         TO  CERT-EFF-DT.                 EL1501
03047      MOVE CL-CERT-NO             TO  CERT-CERT-NO.                EL1501
03048      MOVE 'CERT'                 TO  FILE-SWITCH.                 EL1501
03049                                                                   EL1501
03050      PERFORM 7800-READ-CERT-UPDATE.                               EL1501
03051                                                                   EL1501
100518     IF CL-CLAIM-TYPE NOT = PI-LIFE-OVERRIDE-L1 AND 'O'              CL*24
03053          GO TO 5610-AH-VOID.                                      EL1501
03054                                                                   EL1501
03055      MOVE CM-LF-BENEFIT-CD       TO  WS-BEN-CD.                      CL**6
03056      MOVE WS-ACCESS              TO  CNTL-ACCESS.                    CL**6
03057      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                   CL**6
03058      MOVE '4'                    TO  CNTL-REC-TYPE.                  CL**6
03059      MOVE ZEROS                  TO  CNTL-SEQ-NO.                    CL**6
03060      MOVE 'BENE'                 TO  FILE-SWITCH.                    CL**6
03061      MOVE +0                     TO  SUB.                            CL**6
03062      PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.                       CL**6
03063                                                                      CL*28
03064      IF NO-BENEFIT-FOUND                                             CL**6
03065          GO TO 8400-NOT-FOUND.                                       CL**6
03066                                                                      CL*28
03067      MOVE CF-LF-COVERAGE-TYPE (SUB)  TO  WS-LF-COVERAGE-TYPE.        CL**6
03068                                                                      CL**6
03069      IF PI-LIFE-OVERRIDE-L1 = 'P' OR                                 CL*24
03070         WS-LF-COVERAGE-TYPE = 'P'                                    CL*24
03071          IF WS-PAY-TYPE = '4'                                        CL*24
03072              SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT        CL**6
03073              IF CM-LF-CURRENT-STATUS = '1' OR '2'                    CL*24
03074                  PERFORM 7810-REWRITE-CERT THRU 7810-EXIT            CL*19
03075                  GO TO 5600-EXIT                                     CL**6
03076              ELSE                                                    CL**6
03077                  MOVE CM-LF-STATUS-AT-DEATH  TO                      CL**6
03078                                              CM-LF-CURRENT-STATUS    CL**6
03079                  MOVE SPACES                 TO                      CL**6
03080                                              CM-LF-STATUS-AT-DEATH   CL**6
03081                  MOVE LOW-VALUES             TO                      CL**6
03082                                              CM-LF-DEATH-EXIT-DT     CL**6
03083                                              CM-LF-DEATH-DT          CL**6
03084                  PERFORM 7810-REWRITE-CERT THRU 7810-EXIT            CL*19
03085                  GO TO 5600-EXIT.                                    CL**6
03086                                                                      CL**6
03087      IF WS-PAY-TYPE = '4'                                            CL*24
03088          SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT         EL1501
03089          PERFORM 7810-REWRITE-CERT THRU 7810-EXIT                    CL*19
03090          GO TO 5600-EXIT.                                         EL1501
03091                                                                   EL1501
03092      IF WS-PAY-TYPE = '2'                                            CL*24
100518       OR (CL-CLAIM-TYPE = 'O' AND WS-PAY-TYPE = '3')
03093          IF CM-LF-CURRENT-STATUS = '1' OR '2'                        CL*24
03094              SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT        CL**4
03095              PERFORM 7810-REWRITE-CERT THRU 7810-EXIT                CL*19
03096              GO TO 5600-EXIT                                         CL**4
03097          ELSE                                                        CL**4
100518           IF (CL-CLAIM-TYPE = 'O' AND WS-PAY-TYPE = '3')
100518             MOVE CM-LF-STATUS-AT-CANCEL TO  CM-LF-CURRENT-STATUS    CL**4
100518             MOVE SPACES                TO CM-LF-STATUS-AT-CANCEL    CL**4
100518             SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT        CL**4
100518             MOVE LOW-VALUES            TO  CM-LF-CANCEL-EXIT-DT     CL**4
100518                                            CM-LF-CANCEL-DT          CL**4
100518           ELSE
03098              MOVE CM-LF-STATUS-AT-DEATH TO  CM-LF-CURRENT-STATUS     CL**4
03099              MOVE SPACES                TO  CM-LF-STATUS-AT-DEATH    CL**4
03100              SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT        CL**4
03101              MOVE LOW-VALUES            TO  CM-LF-DEATH-EXIT-DT      CL**4
03102                                             CM-LF-DEATH-DT           CL**4
                 END-IF
03103              PERFORM 7810-REWRITE-CERT THRU 7810-EXIT                CL*19
03104              GO TO 5600-EXIT                                         CL**4
03105      ELSE                                                         EL1501
03106          GO TO 5620-UNLOCK-CERT.                                  EL1501
03107                                                                   EL1501
03108  5610-AH-VOID.                                                    EL1501
03109                                                                   EL1501
03110      IF WS-PAY-TYPE = '4'                                            CL*24
03111          SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT          EL1501
03112          PERFORM 7810-REWRITE-CERT THRU 7810-EXIT                    CL*19
03113          GO TO 5600-EXIT.                                         EL1501
03114                                                                   EL1501
03115      IF WS-PAY-TYPE = '3'                                            CL*24
03116          MOVE CM-AH-STATUS-AT-SETTLEMENT                          EL1501
03117                                  TO  CM-AH-CURRENT-STATUS         EL1501
03118          MOVE SPACES             TO  CM-AH-STATUS-AT-SETTLEMENT   EL1501
03119          SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT          EL1501
03120          MOVE LOW-VALUES         TO  CM-AH-SETTLEMENT-EXIT-DT     EL1501
03121                                      CM-AH-SETTLEMENT-DT          EL1501
03122          PERFORM 7810-REWRITE-CERT THRU 7810-EXIT                    CL*19
03123          GO TO 5600-EXIT                                          EL1501
03124      ELSE                                                         EL1501
03125          GO TO 5620-UNLOCK-CERT.                                  EL1501
03126                                                                   EL1501
03127  5620-UNLOCK-CERT.                                                EL1501
03128                                                                   EL1501
03129      EXEC CICS UNLOCK                                             EL1501
03130          DATASET   ('ELCERT')                                     EL1501
03131      END-EXEC.                                                    EL1501
03132                                                                   EL1501
03133  5600-EXIT.                                                       EL1501
03134      EXIT.                                                        EL1501
03135                                                                   EL1501
03136      EJECT                                                        EL1501
03137  5700-UPDATE-RECON.                                                  CL*18
03138                                                                      CL*18
03139      EXEC CICS HANDLE CONDITION                                      CL*18
03140          NOTFND   (5700-NOT-FOUND)                                   CL*18
03141          NOTOPEN  (8500-FILE-NOTOPEN)                                CL*18
03142      END-EXEC.                                                       CL*18
03143                                                                      CL*18
03144      MOVE PI-COMPANY-CD              TO  RCON-COMPANY-CD.            CL*18
03145      MOVE AT-CHECK-NO                TO  RCON-CHECK-NO.              CL*18
03146      MOVE 'C'                        TO  RCON-CHECK-ORIGIN.          CL*18
03147      MOVE SPACES                     TO  RCON-GL-ACCOUNT-NO.         CL*18
03148                                                                      CL*18
03149      EXEC CICS READ                                                  CL*18
03150          DATASET   ('ELRCON')                                        CL*18
03151          RIDFLD    (ELRCON-KEY)                                      CL*18
03152          SET       (ADDRESS OF CHECK-RECONCILIATION)                 CL*21
03153          UPDATE                                                      CL*18
03154      END-EXEC.                                                       CL*18
03155                                                                      CL*18
03156 ******************************************************************   CL*18
03157 *           IF THE CHECK HAS BEEN REDEEMED - DO NOT VOID         *   CL*18
03158 ******************************************************************   CL*18
03159                                                                      CL*18
03160      IF RC-STATUS = 'R'                                              CL*24
03161          MOVE 'R'                    TO  WS-RECON-SW                 CL*18
03162          MOVE ER-0823                TO  EMI-ERROR                   CL*18
03163          MOVE -1                     TO  LINENOL                     CL*18
03164          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
03165          EXEC CICS UNLOCK                                            CL*18
03166              DATASET   ('ELRCON')                                    CL*18
03167          END-EXEC                                                    CL*18
03168          GO TO 5700-EXIT                                             CL*18
03169      ELSE                                                            CL*18
03170          MOVE ' '                    TO  WS-RECON-SW.                CL*18
03171                                                                      CL*18
03172      MOVE WS-VOID-CODE               TO  RC-STATUS.                  CL*18
03173                                                                      CL*18
03174      MOVE EIBDATE                    TO  DC-JULIAN-YYDDD.            CL*18
03175      MOVE '5'                        TO  DC-OPTION-CODE.             CL*18
03176      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL*18
03177      MOVE DC-GREG-DATE-1-MDY         TO  WS-WORK-DATE.               CL*18
03178      IF WS-WORK-YY > 50                                              CL*29
03179          MOVE '19'                   TO  WS-RCON-YY-1                CL*18
03180          MOVE WS-WORK-YY             TO  WS-RCON-YY-2                CL*18
03181          MOVE WS-WORK-MM             TO  WS-RCON-MM                  CL*18
03182          MOVE WS-WORK-DD             TO  WS-RCON-DD                  CL*18
03183      ELSE                                                            CL*18
03184          MOVE '20'                   TO  WS-RCON-YY-1                CL*18
03185          MOVE WS-WORK-YY             TO  WS-RCON-YY-2                CL*18
03186          MOVE WS-WORK-MM             TO  WS-RCON-MM                  CL*18
03187          MOVE WS-WORK-DD             TO  WS-RCON-DD.                 CL*18
03188                                                                      CL*18
03189      MOVE WS-RCON-DATE               TO  RC-STATUS-DATE.             CL*18
03190                                                                      CL*18
03191      MOVE PI-PROCESSOR-ID            TO  RC-LAST-MAINT-BY.           CL*18
03192      MOVE SAVE-BIN-DATE              TO  RC-LAST-MAINT-DT.           CL*18
03193      MOVE EIBTIME                    TO  RC-LAST-MAINT-HHMMSS.       CL*18
03194                                                                      CL*18
03195      EXEC CICS REWRITE                                               CL*18
03196          DATASET   ('ELRCON')                                        CL*18
03197          FROM      (CHECK-RECONCILIATION)                            CL*18
03198      END-EXEC.                                                       CL*18
03199                                                                      CL*18
03200      GO TO 5700-EXIT.                                                CL*18
03201                                                                      CL*18
03202  5700-NOT-FOUND.                                                     CL*18
03203                                                                      CL*18
03204      MOVE 'X'                        TO  WS-RECON-SW.                CL*18
03205      MOVE -1                         TO  LINENOL.                    CL*18
03206      MOVE ER-0801                    TO  EMI-ERROR.                  CL*18
03207      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*18
03208                                                                      CL*18
03209  5700-EXIT.                                                          CL*18
03210      EXIT.                                                           CL*18
03211                                                                      CL*19
03212      EJECT                                                           CL*18
03213  7000-READ-TRLR-UPDATE.                                           EL1501
03214                                                                   EL1501
03215      MOVE PI-COMPANY-CD          TO  TRLR-COMP-CD.                EL1501
03216      MOVE PI-CARRIER             TO  TRLR-CARRIER.                EL1501
03217      MOVE PI-CLAIM-NO            TO  TRLR-CLAIM-NO.               EL1501
03218      MOVE PI-CERT-NO             TO  TRLR-CERT-NO.                EL1501
03219                                                                   EL1501
03220      EXEC CICS READ                                               EL1501
03221          DATASET   ('ELTRLR')                                     EL1501
03222          RIDFLD    (ELTRLR-KEY)                                   EL1501
03223          SET       (ADDRESS OF ACTIVITY-TRAILERS)                    CL*21
03224          UPDATE                                                   EL1501
03225      END-EXEC.                                                    EL1501
03226                                                                   EL1501
03227  7000-EXIT.                                                       EL1501
03228      EXIT.                                                           CL*18
03229                                                                      CL*18
03230  7010-UNLOCK-TRLR.                                                   CL*18
03231                                                                      CL*18
03232      EXEC CICS UNLOCK                                                CL*18
03233          DATASET   ('ELTRLR')                                        CL*18
03234      END-EXEC.                                                       CL*18
03235                                                                      CL*18
03236  7010-EXIT.                                                          CL*18
03237      EXIT.                                                        EL1501
03238                                                                   EL1501
03239  7100-REWRITE-TRLR.                                               EL1501
03240                                                                   EL1501
03241      MOVE PI-PROCESSOR-ID        TO  PI-UPDATE-BY.                EL1501
03242      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS         EL1501
03243                                      PI-UPDATE-HHMMSS.            EL1501
03244                                                                   EL1501
03245      EXEC CICS REWRITE                                            EL1501
03246          DATASET   ('ELTRLR')                                     EL1501
03247          FROM      (ACTIVITY-TRAILERS)                            EL1501
03248      END-EXEC.                                                    EL1501
03249                                                                   EL1501
03250  7100-EXIT.                                                       EL1501
03251      EXIT.                                                        EL1501
03252                                                                   EL1501
03253      EJECT                                                           CL**6
03254  7200-FIND-BENEFIT.                                                  CL**6
03255                                                                      CL**6
03256      MOVE 'N'                    TO  WS-BEN-SEARCH-SW.               CL**6
03257                                                                      CL**6
03258      EXEC CICS HANDLE CONDITION                                      CL**6
03259          ENDFILE   (7200-EXIT)                                       CL**6
03260          NOTFND    (7200-EXIT)                                       CL**6
03261      END-EXEC.                                                       CL**6
03262                                                                      CL**6
03263      EXEC CICS READ                                                  CL**6
03264          DATASET   ('ELCNTL')                                        CL**6
03265          RIDFLD    (ELCNTL-KEY)                                      CL**6
03266          SET       (ADDRESS OF CONTROL-FILE)                         CL*21
03267          GTEQ                                                        CL**6
03268      END-EXEC.                                                       CL**6
03269                                                                      CL**6
03270      IF CNTL-COMP-ID  NOT = CF-COMPANY-ID OR                         CL*28
03271         CNTL-REC-TYPE NOT = CF-RECORD-TYPE                           CL*24
03272          GO TO 7200-EXIT.                                            CL**6
03273                                                                      CL**6
03274      PERFORM 7200-BENEFIT-DUMMY THRU 7200-DUMMY-EXIT                 CL**6
03275          VARYING SUB FROM 1 BY 1 UNTIL                               CL**6
03276          ((SUB > 8) OR                                               CL*29
03277          (CF-BENEFIT-CODE (SUB) = WS-BEN-CD)).                       CL*24
03278                                                                      CL**6
03279      IF SUB NOT = 9                                                  CL*24
03280          MOVE 'Y'             TO  WS-BEN-SEARCH-SW.                  CL**6
03281                                                                      CL**6
03282      GO TO 7200-EXIT.                                                CL**6
03283                                                                      CL**6
03284  7200-BENEFIT-DUMMY.                                                 CL**6
03285                                                                      CL**6
03286  7200-DUMMY-EXIT.                                                    CL**6
03287      EXIT.                                                           CL**6
03288                                                                      CL**6
03289  7200-EXIT.                                                          CL**6
03290      EXIT.                                                           CL**6
03291      EJECT                                                           CL**6
03292  7500-READ-CLAIM-MSTR-UPDATE.                                     EL1501
03293                                                                   EL1501
03294      MOVE PI-COMPANY-CD          TO  MSTR-COMP-CD.                EL1501
03295      MOVE PI-CARRIER             TO  MSTR-CARRIER.                EL1501
03296      MOVE PI-CLAIM-NO            TO  MSTR-CLAIM-NO.               EL1501
03297      MOVE PI-CERT-NO             TO  MSTR-CERT-NO.                EL1501
03298                                                                   EL1501
03299      EXEC CICS READ                                               EL1501
03300          DATASET   ('ELMSTR')                                     EL1501
03301          RIDFLD    (ELMSTR-KEY)                                   EL1501
03302          SET       (ADDRESS OF CLAIM-MASTER)                         CL*21
03303          UPDATE                                                   EL1501
03304      END-EXEC.                                                    EL1501
03305                                                                   EL1501
03306  7500-EXIT.                                                       EL1501
03307      EXIT.                                                           CL*18
03308                                                                      CL*18
03309  7510-UNLOCK-CLAIM-MSTR.                                             CL*18
03310                                                                      CL*18
03311      EXEC CICS UNLOCK                                                CL*18
03312          DATASET   ('ELMSTR')                                        CL*18
03313      END-EXEC.                                                       CL*18
03314                                                                      CL*18
03315  7510-EXIT.                                                          CL*18
03316      EXIT.                                                        EL1501
03317                                                                   EL1501
061013 7520-READ-CLAIM-MSTR.
061013
061013     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD
061013     MOVE PI-CARRIER             TO MSTR-CARRIER
061013     MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO
061013     MOVE PI-CERT-NO             TO MSTR-CERT-NO
061013
061013     EXEC CICS READ
061013         DATASET   ('ELMSTR')
061013         RIDFLD    (ELMSTR-KEY)
061013         SET       (ADDRESS OF CLAIM-MASTER)
061013         RESP      (WS-RESPONSE)
061013     END-EXEC
061013
061013     .
061013 7520-EXIT.
061013     EXIT.

03318  7600-REWRITE-CLAIM-MSTR.                                         EL1501
03319                                                                   EL1501
03320      MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT.            EL1501
03321      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.        EL1501
03322      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.          EL1501
03323      MOVE '3'                    TO  CL-LAST-MAINT-TYPE.          EL1501
03324                                                                   EL1501
03325      EXEC CICS REWRITE                                            EL1501
03326          DATASET   ('ELMSTR')                                     EL1501
03327          FROM      (CLAIM-MASTER)                                 EL1501
03328      END-EXEC.                                                    EL1501
03329                                                                   EL1501
03330  7600-EXIT.                                                       EL1501
03331      EXIT.                                                        EL1501
03332                                                                   EL1501
03333      EJECT                                                        EL1501
03334  7700-READ-ELACTQ.                                                EL1501
03335                                                                   EL1501
03336      EXEC CICS HANDLE CONDITION                                   EL1501
03337          NOTFND   (7799-EXIT)                                        CL**8
03338      END-EXEC.                                                    EL1501
03339                                                                   EL1501
03340      EXEC CICS READ                                               EL1501
03341          DATASET   ('ELACTQ')                                     EL1501
03342          RIDFLD    (ELACTQ-KEY)                                   EL1501
03343          SET       (ADDRESS OF ACTIVITY-QUE)                         CL*21
03344          UPDATE                                                   EL1501
03345      END-EXEC.                                                    EL1501
03346                                                                   EL1501
03347      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC                          CL*29
03348          MOVE ZEROS              TO  AQ-PMT-UNAPPROVED-COUNT.        CL**8
03349                                                                   EL1501
03350      IF AQ-PAYMENT-COUNTER NOT NUMERIC                               CL*29
03351          MOVE +0                 TO  AQ-PAYMENT-COUNTER.             CL**8
03352                                                                      CL**8
03353      IF WS-CF-PMT-APPROVAL-USED                                      CL*17
03354          IF AQ-PMT-UNAPPROVED-COUNT > +0 AND                         CL*29
03355             WS-CHECK-WRITTEN-DT = LOW-VALUES AND                     CL*24
03356             WS-PMT-APPROVAL-SW = 'U'                                 CL*24
03357              SUBTRACT +1 FROM AQ-PMT-UNAPPROVED-COUNT                CL**8
03358              MOVE 'Y'            TO  WS-UPDATE-SW.                   CL**8
03359                                                                      CL**8
03360      IF PAYMENT-NOT-PRINTED OR                                       CL**8
03361         PAYMENT-NOT-RELEASED                                         CL**8
03362          IF AQ-PAYMENT-COUNTER > +0                                  CL*29
03363              SUBTRACT +1 FROM AQ-PAYMENT-COUNTER                     CL**8
03364              MOVE 'Y'            TO  WS-UPDATE-SW.                   CL**8
03365                                                                      CL**8
03366      IF AQ-PAYMENT-COUNTER = +0                                      CL*24
03367          MOVE ' '                TO  AQ-PENDING-PAYMENT-FLAG.        CL**8
03368                                                                      CL**8
03369      IF WS-UPDATE-SW = 'Y'                                           CL*24
03370          IF AQ-PENDING-ACTIVITY-FLAGS = SPACES                       CL*24
03371              MOVE 'N'            TO  WS-UPDATE-SW                    CL**8
03372              GO TO 7720-DELETE-ACTIVITY-QUE                          CL**8
03373          ELSE                                                        CL**8
03374              MOVE 'N'            TO  WS-UPDATE-SW                    CL**8
03375      ELSE                                                            CL**8
03376          GO TO 7750-UNLOCK-ACTIVITY-QUE.                             CL**8
03377                                                                      CL**8
03378  7710-REWRITE-ACTIVITY-QUE.                                          CL**8
03379                                                                   EL1501
03380      EXEC CICS REWRITE                                            EL1501
03381          DATASET   ('ELACTQ')                                     EL1501
03382          FROM      (ACTIVITY-QUE)                                 EL1501
03383      END-EXEC.                                                    EL1501
03384                                                                   EL1501
03385      GO TO 7799-EXIT.                                                CL**8
03386                                                                      CL**8
03387  7720-DELETE-ACTIVITY-QUE.                                           CL**8
03388                                                                      CL**8
03389      EXEC CICS DELETE                                                CL**8
03390          DATASET   ('ELACTQ')                                        CL**8
03391      END-EXEC.                                                       CL**8
03392                                                                      CL**8
03393      GO TO 7799-EXIT.                                                CL**8
03394                                                                      CL**8
03395  7750-UNLOCK-ACTIVITY-QUE.                                           CL**8
03396                                                                      CL**8
03397      EXEC CICS UNLOCK                                                CL**8
03398          DATASET   ('ELACTQ')                                        CL**8
03399      END-EXEC.                                                       CL**8
03400                                                                      CL**8
03401      MOVE 'N'                    TO  WS-UPDATE-SW.                   CL**8
03402                                                                      CL**8
03403  7799-EXIT.                                                          CL**8
03404      EXIT.                                                        EL1501
03405      EJECT                                                        EL1501
03406  7800-READ-CERT-UPDATE.                                           EL1501
03407                                                                   EL1501
03408      EXEC CICS READ                                               EL1501
03409          DATASET   ('ELCERT')                                     EL1501
03410          RIDFLD    (ELCERT-KEY)                                   EL1501
03411          SET       (ADDRESS OF CERTIFICATE-MASTER)                   CL*21
03412          UPDATE                                                   EL1501
03413      END-EXEC.                                                    EL1501
03414                                                                   EL1501
03415  7800-EXIT.                                                       EL1501
03416      EXIT.                                                        EL1501
03417                                                                   EL1501
03418  7810-REWRITE-CERT.                                                  CL*19
03419                                                                   EL1501
03420      EXEC CICS REWRITE                                            EL1501
03421          DATASET   ('ELCERT')                                     EL1501
03422          FROM      (CERTIFICATE-MASTER)                           EL1501
03423      END-EXEC.                                                    EL1501
03424                                                                   EL1501
03425  7810-EXIT.                                                          CL*19
03426      EXIT.                                                        EL1501
03427                                                                   EL1501
03428      EJECT                                                        EL1501
03429  7900-READ-CONTROL-FILE.                                          EL1501
03430                                                                   EL1501
03431      EXEC CICS READ                                               EL1501
03432          DATASET   ('ELCNTL')                                     EL1501
03433          RIDFLD    (ELCNTL-KEY)                                   EL1501
03434          SET       (ADDRESS OF CONTROL-FILE)                         CL*21
03435      END-EXEC.                                                    EL1501
03436                                                                   EL1501
03437  7900-EXIT.                                                       EL1501
03438      EXIT.                                                        EL1501
03439                                                                   EL1501
03440      EJECT                                                        EL1501
03441  8000-LOAD-ERROR-MESSAGES.                                        EL1501
03442      IF EMI-NO-ERRORS                                             EL1501
03443          GO TO 8000-EXIT.                                         EL1501
03444                                                                   EL1501
03445      IF EMI-NUMBER-OF-LINES = 1                                   EL1501
03446          MOVE EMI-LINE1          TO  ERRMSG1O                     EL1501
03447          GO TO 8000-EXIT.                                         EL1501
03448                                                                   EL1501
03449      MOVE EMI-LINE1              TO  ERRMSG1O.                    EL1501
03450                                                                   EL1501
03451  8000-EXIT.                                                       EL1501
03452      EXIT.                                                        EL1501
03453                                                                   EL1501
03454  8100-SEND-INITIAL-MAP.                                           EL1501
03455                                                                      CL*21
03456      IF PI-FULL-SCREEN-SHOWN                                         CL*28
03457          GO TO 8200-SEND-DATAONLY.                                   CL*21
03458                                                                      CL*21
03459      MOVE 'Y'                    TO PI-FULL-SCREEN-IND.              CL*21
03460                                                                      CL*21
03461      MOVE SAVE-DATE              TO  RUNDTEO.                     EL1501
03462      MOVE EIBTIME                TO  TIME-IN.                     EL1501
03463      MOVE TIME-OUT               TO  RUNTIMEO.                    EL1501
03464      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.             EL1501
03465                                                                      CL*21
121802*    IF PI-COMPANY-ID NOT = 'DMD'                                    CL*28
03467          MOVE SPACES             TO PF6O.                            CL*21
03468          MOVE AL-SADOF           TO PF6A.                            CL*21
03469                                                                   EL1501
03470      EXEC CICS SEND                                               EL1501
03471          MAP      (MAP-NAME)                                      EL1501
03472          MAPSET   (MAPSET-NAME)                                   EL1501
03473          FROM     (EL150BO)                                       EL1501
03474          ERASE                                                    EL1501
03475          CURSOR                                                   EL1501
03476      END-EXEC.                                                       CL*28
03477                                                                   EL1501
03478      GO TO 9100-RETURN-TRAN.                                      EL1501
03479                                                                   EL1501
03480  8200-SEND-DATAONLY.                                              EL1501
03481                                                                      CL*21
03482      IF NOT PI-FULL-SCREEN-SHOWN                                     CL*28
03483          GO TO 8100-SEND-INITIAL-MAP.                                CL*21
03484                                                                      CL*21
03485      MOVE SAVE-DATE              TO  RUNDTEO.                     EL1501
03486      MOVE EIBTIME                TO  TIME-IN.                     EL1501
03487      MOVE TIME-OUT               TO  RUNTIMEO.                    EL1501
03488      PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.             EL1501
03489                                                                   EL1501
03490      MOVE PI-CARRIER             TO  CARRO.                       EL1501
03491      MOVE PI-CLAIM-NO            TO  CLMNOO.                      EL1501
03492      MOVE PI-CERT-PRIME          TO  CERTNOO.                     EL1501
03493      MOVE PI-CERT-SFX            TO  SUFXO.                       EL1501
03494                                                                      CL*21
121802*    IF PI-COMPANY-ID NOT = 'DMD'                                    CL*28
03496          MOVE SPACES             TO PF6O.                            CL*21
03497          MOVE AL-SADOF           TO PF6A.                            CL*21
03498                                                                   EL1501
03499      EXEC CICS SEND                                               EL1501
03500          MAP      (MAP-NAME)                                      EL1501
03501          MAPSET   (MAPSET-NAME)                                   EL1501
03502          FROM     (EL150BO)                                       EL1501
03503          DATAONLY                                                 EL1501
03504          CURSOR                                                   EL1501
03505      END-EXEC.                                                       CL*28
03506                                                                   EL1501
03507      GO TO 9100-RETURN-TRAN.                                      EL1501
03508                                                                   EL1501
03509  8300-SEND-TEXT.                                                  EL1501
03510      EXEC CICS SEND TEXT                                          EL1501
03511          FROM     (LOGOFF-TEXT)                                   EL1501
03512          LENGTH   (LOGOFF-LENGTH)                                 EL1501
03513          ERASE                                                    EL1501
03514          FREEKB                                                   EL1501
03515      END-EXEC.                                                       CL*28
03516                                                                   EL1501
03517      EXEC CICS RETURN                                             EL1501
03518          END-EXEC.                                                EL1501
03519                                                                      CL**6
03520  8400-NOT-FOUND.                                                     CL**6
03521      IF FILE-SWITCH = 'BENE'                                         CL*24
03522          MOVE ER-0282            TO  EMI-ERROR.                      CL**6
03523                                                                      CL**6
03524      MOVE -1                     TO  LINENOL.                        CL**6
03525      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**6
03526                                                                      CL**6
03527      IF PASS-SWITCH = 'A'                                            CL*24
03528          GO TO 8100-SEND-INITIAL-MAP                                 CL**6
03529      ELSE                                                            CL**6
03530          GO TO 8200-SEND-DATAONLY.                                   CL**6
03531                                                                   EL1501
03532  8500-FILE-NOTOPEN.                                               EL1501
03533                                                                   EL1501
03534      IF FILE-SWITCH = 'TRLR'                                         CL*24
03535          MOVE ER-0172            TO  EMI-ERROR.                   EL1501
03536                                                                   EL1501
03537      IF FILE-SWITCH = 'CERT'                                         CL*24
03538          MOVE ER-0169            TO  EMI-ERROR.                   EL1501
03539                                                                   EL1501
03540      IF FILE-SWITCH = 'CNTL'                                         CL*24
03541          MOVE ER-0042            TO  EMI-ERROR.                   EL1501
03542                                                                   EL1501
03543      IF FILE-SWITCH = 'ACTQ'                                         CL*24
03544          MOVE ER-0338            TO  EMI-ERROR.                   EL1501
03545                                                                   EL1501
03546      IF FILE-SWITCH = 'MSTR'                                         CL*24
03547          MOVE ER-0154            TO  EMI-ERROR.                   EL1501
03548                                                                      CL*18
03549      IF FILE-SWITCH = 'RCON'                                         CL*24
03550          MOVE ER-0776            TO  EMI-ERROR.                      CL*18
03551                                                                      CL*19
03552      IF FILE-SWITCH = 'PLCY'                                         CL*24
03553          MOVE ER-9883            TO  EMI-ERROR.                      CL*19
03554                                                                   EL1501
03555      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL1501
03556                                                                   EL1501
03557      MOVE -1                     TO  LINENOL.                     EL1501
03558                                                                   EL1501
03559      IF PASS-SWITCH = 'A'                                         EL1501
03560          GO TO 8100-SEND-INITIAL-MAP                              EL1501
03561      ELSE                                                         EL1501
03562          GO TO 8200-SEND-DATAONLY.                                EL1501
03563                                                                   EL1501
03564  8800-UNAUTHORIZED-ACCESS.                                        EL1501
03565      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL1501
03566      GO TO 8300-SEND-TEXT.                                        EL1501
03567                                                                   EL1501
03568  8810-PF23.                                                       EL1501
03569      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL1501
03570      MOVE XCTL-005               TO  PGM-NAME.                    EL1501
03571      GO TO 9300-XCTL.                                             EL1501
03572                                                                   EL1501
03573  9100-RETURN-TRAN.                                                EL1501
03574      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL1501
03575      MOVE '150B'                 TO  PI-CURRENT-SCREEN-NO.        EL1501
03576                                                                   EL1501
03577      EXEC CICS RETURN                                             EL1501
03578          TRANSID    (TRANS-ID)                                    EL1501
03579          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL1501
03580          LENGTH     (PI-COMM-LENGTH)                              EL1501
03581      END-EXEC.                                                       CL*28
03582                                                                   EL1501
03583  9200-RETURN-MAIN-MENU.                                           EL1501
03584      MOVE XCTL-126               TO PGM-NAME.                     EL1501
03585      GO TO 9300-XCTL.                                             EL1501
03586                                                                   EL1501
03587  9300-XCTL.                                                       EL1501
03588      EXEC CICS HANDLE CONDITION                                      CL*21
03589          PGMIDERR   (9350-NOT-FOUND)                                 CL*21
03590      END-EXEC.                                                       CL*28
03591                                                                      CL*21
03592      EXEC CICS XCTL                                               EL1501
03593          PROGRAM    (PGM-NAME)                                    EL1501
03594          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL1501
03595          LENGTH     (PI-COMM-LENGTH)                              EL1501
03596      END-EXEC.                                                       CL*28
03597                                                                      CL*21
03598  9350-NOT-FOUND.                                                     CL*21
03599      MOVE ER-0923                TO EMI-ERROR.                       CL*21
03600      MOVE -1                     TO LINENOL.                         CL*21
03601      PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT.                  CL*28
03602      PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT.                  CL*28
03603      PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT.                  CL*28
03604      GO TO 8200-SEND-DATAONLY.                                       CL*21
03605                                                                   EL1501
03606  9400-CLEAR.                                                      EL1501
03607      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL1501
03608      GO TO 9300-XCTL.                                             EL1501
03609                                                                   EL1501
03610  9500-PF12.                                                       EL1501
03611      MOVE XCTL-010               TO  PGM-NAME.                    EL1501
03612      GO TO 9300-XCTL.                                             EL1501
03613                                                                   EL1501
03614  9600-PGMID-ERROR.                                                EL1501
03615      EXEC CICS HANDLE CONDITION                                   EL1501
03616          PGMIDERR   (8300-SEND-TEXT)                              EL1501
03617      END-EXEC.                                                       CL*28
03618                                                                   EL1501
03619      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL1501
03620      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL1501
03621      MOVE XCTL-005               TO  PGM-NAME.                    EL1501
03622      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL1501
03623      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL1501
03624      GO TO 9300-XCTL.                                             EL1501
03625                                                                   EL1501
03626  9700-LINK-DATE-CONVERT.                                          EL1501
03627      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL1501
03628                                                                   EL1501
03629      EXEC CICS LINK                                               EL1501
03630          PROGRAM    (PGM-NAME)                                    EL1501
03631          COMMAREA   (DATE-CONVERSION-DATA)                        EL1501
03632          LENGTH     (DC-COMM-LENGTH)                              EL1501
03633      END-EXEC.                                                       CL*28
03634                                                                      CL*28
03635  9700-EXIT.                                                       EL1501
03636      EXIT.                                                        EL1501
03637                                                                   EL1501
03638  9800-DEEDIT.                                                        CL*21
03639                                                                   EL1501
03640      EXEC CICS BIF DEEDIT                                         EL1501
03641          FIELD   (WS-DEEDIT-FIELD)                                EL1501
03642          LENGTH  (WS-DEEDIT-LENGTH)                               EL1501
03643      END-EXEC.                                                    EL1501
03644                                                                   EL1501
03645  9800-EXIT.                                                       EL1501
03646      EXIT.                                                        EL1501
CIDMOD
CIDMOD                                                                       000
CIDMOD 9870-OUTPUT-ACTIVITY-RECORD.                                          000
CIDMOD                                                                       000
CIDMOD     EXEC CICS GETMAIN                                                 000
CIDMOD         SET(ADDRESS OF DAILY-ACTIVITY-RECORD)                         000
CIDMOD         LENGTH(25)                                                    000
CIDMOD         INITIMG(WS-BLANK)                                             000
CIDMOD     END-EXEC.                                                         000
CIDMOD                                                                       000
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.             000
CIDMOD     MOVE ELMSTR-KEY             TO DA-KEY.                            000
CIDMOD     MOVE CL-TRAILER-SEQ-CNT     TO DA-TRAILER-SEQ-NO.                 000
CIDMOD
CIDMOD     IF PI-PAY-TYPE EQUAL '7'                                          000
CIDMOD         MOVE 'V'                TO DA-RECORD-TYPE                     000
CIDMOD     ELSE                                                              000
CIDMOD         MOVE 'P'                TO DA-RECORD-TYPE.                    000
CIDMOD                                                                       000
CIDMOD     EXEC CICS HANDLE CONDITION                                        000
CIDMOD         NOTOPEN (9870-NOTOPEN)                                        000
CIDMOD         DUPREC (9870-EXIT)                                            000
CIDMOD     END-EXEC.                                                         000
CIDMOD                                                                       000
CIDMOD     EXEC CICS WRITE                                                   000
CIDMOD         DATASET ('DLYACTV')                                           000
CIDMOD         RIDFLD (DA-KEY)                                               000
CIDMOD         FROM (DAILY-ACTIVITY-RECORD)                                  000
CIDMOD         LENGTH (25)                                                   000
CIDMOD     END-EXEC.                                                         000
CIDMOD                                                                       000
CIDMOD     MOVE 'N'                    TO ERROR-ON-OUTPUT-SW.                000
CIDMOD     GO TO 9870-EXIT.                                                  000
CIDMOD                                                                       000
CIDMOD 9870-NOTOPEN.                                                         000
CIDMOD                                                                       000
CIDMOD     MOVE '2955'                 TO EMI-ERROR.                         000
CIDMOD     MOVE 'Y'                    TO ERROR-ON-OUTPUT-SW.                000
CIDMOD                                                                       000
CIDMOD 9870-EXIT.                                                            000
CIDMOD                                                                  EL1501
03648  9900-ERROR-FORMAT.                                               EL1501
03649      IF NOT EMI-ERRORS-COMPLETE                                   EL1501
03650          MOVE LINK-001           TO PGM-NAME                      EL1501
03651          EXEC CICS LINK                                           EL1501
03652              PROGRAM    (PGM-NAME)                                EL1501
03653              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL1501
03654              LENGTH     (EMI-COMM-LENGTH)                         EL1501
03655          END-EXEC.                                                   CL*28
03656                                                                   EL1501
03657  9900-EXIT.                                                       EL1501
03658      EXIT.                                                        EL1501
03659                                                                   EL1501
03660  9990-ABEND.                                                      EL1501
03661      MOVE -1                     TO  ENTERPFL.                    EL1501
03662      MOVE LINK-004               TO  PGM-NAME.                    EL1501
03663                                                                   EL1501
03664      MOVE DFHEIBLK               TO  EMI-LINE1                    EL1501
03665      EXEC CICS LINK                                               EL1501
03666          PROGRAM   (PGM-NAME)                                     EL1501
03667          COMMAREA  (EMI-LINE1)                                    EL1501
03668          LENGTH    (72)                                           EL1501
03669      END-EXEC.                                                    EL1501
03670                                                                   EL1501
03671      MOVE EMI-LINE1              TO  ERRMSG1O.                    EL1501
03672      GO TO 8200-SEND-DATAONLY.                                    EL1501
03673                                                                   EL1501
03674  EJECT                                                            EL1501
03675  9995-SECURITY-VIOLATION.                                         EL1501
03676                              COPY ELCSCTP.                        EL1501
03677                                                                   EL1501
