      *((program: EL1501.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL1501.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 05/01/95 15:33:51.
000007*                            VMOD=2.030.
000008*
000009*AUTHOR.     LOGIC,INC.
000010*            DALLAS, TEXAS.
000011
000012*REMARKS.    TRANSACTION - EXH9 - CLAIM HISTORY
000013******************************************************************
000014*                   C H A N G E   L O G
000015*
000016* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000017*-----------------------------------------------------------------
000018*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000019* EFFECTIVE    NUMBER
000020*-----------------------------------------------------------------
000021* 062602    2002030700006  PEMA  Add note type of 'S'
000022*                                  (special review)
000023* 121802    2001061800003  SMVA  REMOVE OBSOLETE CODE
000024* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST CALC
000025* 071210    2009122800001  AJRA  ADD 'LETTER TO BENE' ON LETTER
000026* 071510    2010070600001  PEMA  INT PMT VOID SHOULD NOT OPEN CLM
000027* 113010    2009122800001  AJRA  DISPLAY STOP DATE
000028* 041613  CR2013031200002  AJRA  ADD MAIL RECEIVED ACTION TYPE
000029* 061013    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
000030* 031214    2014031200001  AJRA  UPDATE TOT INT ON VOID
000031* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000032* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000033* 013017  CR2016053100001  PEMA  ACH PROCESSING
000034* 062217  CR2017050300002  TANA  ADD AUTH RCVD INDICATOR
000035* 022718  CR2017100200004  TANA  DONT UPDT ON HOLD UNTIL PMT
000036* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000037* 010719  IR2019010400001  PEMA  FIX VOID FOR ACH PAYMENT
000038* 080322  CR2021100800003  TANA  Add B and H claim types
000039*****************************************************************
000040
000041 ENVIRONMENT DIVISION.
000042
000043     EJECT
000044 DATA DIVISION.
000045 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000046 77  FILLER  PIC X(32)  VALUE '********************************'.
000047 77  FILLER  PIC X(32)  VALUE '*   EL1501 WORKING STORAGE     *'.
000048 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.030 *********'.
000049 77  S1                          PIC S999 COMP-3 VALUE +0.
000050 77  S2                          PIC S999 COMP-3 VALUE +0.
000051 77  ws-max-bens                 pic s999 comp-3 value +0.
000052 77  ws-prev-days-paid           pic s9(5) comp-3 value +0.
000053 77  ws-prev-amt-paid            pic s9(9)v99 comp-3 value +0.
000054 77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
000055 77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
000056 77  ws-pd-bens                  pic s9(5) comp-3 value +0.
000057
000058*                                    COPY ELCSCTM.
      *>>((file: ELCSCTM))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCSCTM                             *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
000008*                                                                *
000009******************************************************************
000010 01  SECURITY-MESSAGE.
000011     12  FILLER                          PIC X(30)
000012            VALUE '** LOGIC SECURITY VIOLATION -'.
000013     12  SM-READ                         PIC X(6).
000014     12  FILLER                          PIC X(5)
000015            VALUE ' PGM='.
000016     12  SM-PGM                          PIC X(6).
000017     12  FILLER                          PIC X(5)
000018            VALUE ' OPR='.
000019     12  SM-PROCESSOR-ID                 PIC X(4).
000020     12  FILLER                          PIC X(6)
000021            VALUE ' TERM='.
000022     12  SM-TERMID                       PIC X(4).
000023     12  FILLER                          PIC XX   VALUE SPACE.
000024     12  SM-JUL-DATE                     PIC 9(5).
000025     12  FILLER                          PIC X    VALUE SPACE.
000026     12  SM-TIME                         PIC 99.99.
000027
      *<<((file: ELCSCTM))
000059
000060*                                    COPY ELCSCRTY.
      *>>((file: ELCSCRTY))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCSCRTY                            *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
000008*        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
000009*        SAVED IN PI-SECURITY-ADDRESS.                           *
000010*                                                                *
000011******************************************************************
000012 01  SECURITY-CONTROL.
000013     12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
000014     12  FILLER                       PIC XX    VALUE 'SC'.
000015     12  SC-CREDIT-CODES.
000016         16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
000017             20  SC-CREDIT-DISPLAY    PIC X.
000018             20  SC-CREDIT-UPDATE     PIC X.
000019     12  SC-CLAIMS-CODES.
000020         16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
000021             20  SC-CLAIMS-DISPLAY    PIC X.
000022             20  SC-CLAIMS-UPDATE     PIC X.
      *<<((file: ELCSCRTY))
000061
000062 EJECT
000063 01  WS-DATE-AREA.
000064     12  SAVE-DATE                   PIC X(8)    VALUE SPACES.
000065     12  SAVE-DATE-CCYYMMDD.
000066         16  SAVE-DATE-CC            PIC XX      VALUE SPACES.
000067         16  SAVE-DATE-YMD.
000068             20  SAVE-DATE-YY        PIC XX      VALUE SPACES.
000069             20  FILLER              PIC X(4)    VALUE SPACES.
000070     12  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
000071
000072 01  ws-save-error-interface-block pic x(400) value low-values.
000073
000074 01  STANDARD-AREAS.
000075     12  WS-RESPONSE         PIC S9(8)   COMP.
000076         88  RESP-NORMAL              VALUE +00.
000077         88  RESP-ERROR               VALUE +01.
000078         88  RESP-NOTFND              VALUE +13.
000079         88  RESP-DUPREC              VALUE +14.
000080         88  RESP-ENDFILE             VALUE +20.
000081
000082     12  GETMAIN-SPACE               PIC X       VALUE SPACE.
000083     12  MAP-NAME                    PIC X(8)    VALUE 'EL150B'.
000084     12  MAPSET-NAME                 PIC X(8)    VALUE 'EL1501S'.
000085     12  TRANS-ID                    PIC X(4)    VALUE 'EXH9'.
000086     12  THIS-PGM                    PIC X(8)    VALUE 'EL1501'.
000087     12  PGM-NAME                    PIC X(8).
000088     12  TIME-IN                     PIC S9(7).
000089     12  TIME-OUT-R  REDEFINES TIME-IN.
000090         16  FILLER                  PIC X.
000091         16  TIME-OUT                PIC 99V99.
000092         16  FILLER                  PIC XX.
000093     12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
000094     12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
000095     12  XCTL-126                    PIC X(8)    VALUE 'EL126'.
000096     12  XCTL-142                    PIC X(8)    VALUE 'EL142'.
000097     12  LINK-001                    PIC X(8)    VALUE 'EL001'.
000098     12  LINK-004                    PIC X(8)    VALUE 'EL004'.
000099     12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
000100     12  FILE-ID                     PIC X(8).
000101     12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.
000102     12  WS-BLANK                    PIC X       VALUE ' '.
000103
000104 01  MISC-WORK-AREAS.
000105     12  ws-email-return-cd      pic s9(8) comp-5 value +0.
000106     12  ws-email-string.
000107         16  f                   pic x(28) value
000108          'smtp -f slunikix -t pema -s '.
000109         16  f                   pic x(29) value
000110          '''ACH Void Alert for Company '.
000111         16  ws-email-client-id  pic xxx value spaces.
000112         16  f                   pic x(7) value ''' -ml '.
000113         16  f                   pic x(53) value
000114          '''A void on an ACH payment has just been performed.  '.
000115         16  f                   pic x(14) value
000116          ' Claim Number '.
000117         16  ws-email-claim-no   pic x(10).
000118         16  f                   pic x(14) value
000119          ' Check Number '.
000120         16  ws-email-check-no   pic x(10).
000121         16  f                   pic x(7) value
000122          ' Payee '.
000123         16  ws-email-payee      pic x(30).
000124         16  f                   pic x(14) value
000125          ' Check Amount '.
000126         16  ws-email-check-amt  pic zzz,zz9.99.
000127         16  f                   pic xxx  value ''''.
000128         16  f                   pic x value low-values.
000129
000130     12  ws-curl-return-cd       pic s9(8) comp-5 value +0.
000131     12  ws-curl-string.
000132         16  f                   pic x(19) value
000133          'curl --data "USZip='.
000134         16  ws-curl-zip         pic x(5) value spaces.
000135         16  f                   pic xx value '" '.
000136         16  f                   pic x(46) value
000137          'http://webservicex.net/uszip.asmx/GetInfoByZIP'.
000138
000139     12  WS-CK-Q-CONTROL             PIC S9(8) COMP.
000140     12  W-CALLED-NAME               PIC X(8).
000141     12  QID.
000142         16  QID-TERM                PIC X(4).
000143         16  FILLER                  PIC X(4)    VALUE '150B'.
000144     12  MAP-LENGTH                  PIC S9(4)   VALUE +1920 COMP.
000145     12  PASS-SWITCH                 PIC X       VALUE 'A'.
000146     12  DISPLAY-CNT                 PIC S9(4)   VALUE +1    COMP.
000147     12  FILE-SWITCH                 PIC X(4)    VALUE SPACES.
000148     12  WS-SUB                      PIC 9       VALUE 0.
000149     12  SUB                         PIC 9       VALUE 1.
000150     12  SUB-1                       PIC 9       VALUE 1.
000151     12  RETURNED-FROM               PIC X(8)    VALUE SPACES.
000152     12  DIRECTION-SWITCH            PIC X       VALUE 'N'.
000153     12  WS-RECORDS-READ-SW          PIC X       VALUE 'N'.
000154         88  RECORDS-READ                        VALUE 'Y'.
000155     12  SAVE-CONTROL                PIC X(39).
000156     12  WS-RECEIVED-DATE            PIC XX      VALUE LOW-VALUES.
000157     12  WS-CHECK-WRITTEN-DT         PIC XX      VALUE LOW-VALUES.
000158     12  WS-PMT-APPROVAL-SW          PIC X       VALUE SPACE.
000159     12  WS-CF-PMT-APPROVAL-SW       PIC X       VALUE SPACE.
000160         88  WS-CF-PMT-APPROVAL-USED             VALUE 'Y' 'G'.
000161         88  WS-CF-NO-APPROVAL                   VALUE ' ' 'N'.
000162         88  WS-CF-ALL-APPROVED                  VALUE 'Y'.
000163         88  WS-CF-GRADUATED-APPROVAL            VALUE 'G'.
000164
000165     12  WS-CV-PMT-CODE              PIC X       VALUE SPACE.
000166     12  WS-PAY-TYPE                 PIC X       VALUE SPACE.
000167     12  WS-AMOUNT-PAID              PIC S9(7)V99 VALUE ZEROS.
000168     12  WS-PAYMENT-ORIGIN           PIC X       VALUE SPACE.
000169     12  WS-RECON-SW                 PIC X       VALUE ' '.
000170         88  RECON-RCD-REDEEMED                  VALUE 'R'.
000171         88  RECON-RCD-NOT-FOUND                 VALUE 'X'.
000172
000173     12  WS-DEEDIT-LENGTH            PIC S9(4)   VALUE +16   COMP.
000174     12  WS-DEEDIT-FIELD             PIC X(16)   VALUE ZEROS.
000175     12  WS-DEEDIT-FIELD-V0 REDEFINES WS-DEEDIT-FIELD
000176                                     PIC S9(16).
000177
000178     12  WS-LF-COVERAGE-TYPE         PIC X       VALUE SPACE.
000179     12  WS-BEN-SEARCH-SW            PIC X       VALUE 'N'.
000180         88  BENEFIT-FOUND                       VALUE 'Y'.
000181         88  NO-BENEFIT-FOUND                    VALUE 'N'.
000182     12  WS-ACCESS.
000183         16  FILLER                  PIC XX      VALUE SPACES.
000184         16  WS-BEN-CD               PIC XX      VALUE SPACES.
000185
000186     12  WS-PRINTED-SW               PIC X       VALUE 'N'.
000187         88  PAYMENT-HAS-BEEN-PRINTED            VALUE 'Y'.
000188         88  PAYMENT-NOT-PRINTED                 VALUE 'N'.
000189
000190     12  WS-RELEASED-SW              PIC X       VALUE 'N'.
000191         88  PAYMENT-NOT-RELEASED                VALUE 'N'.
000192
000193     12  WS-UPDATE-SW                PIC X       VALUE 'N'.
000194     12  WS-VOID-CODE                PIC X       VALUE ' '.
000195
000196     12  WS-WORK-DATE.
000197         16  WS-WORK-MM              PIC 99      VALUE ZEROS.
000198         16  WS-WORK-DD              PIC 99      VALUE ZEROS.
000199         16  WS-WORK-YY              PIC 99      VALUE ZEROS.
000200
000201     12  WS-RCON-DATE.
000202         16  WS-RCON-YEAR.
000203             20  WS-RCON-YY-1        PIC 99.
000204             20  WS-RCON-YY-2        PIC 99.
000205         16  WS-RCON-MM              PIC 99.
000206         16  WS-RCON-DD              PIC 99.
000207     12  W-NAME-LAST             PIC  X(15).
000208     12  W-NAME-FIRST            PIC  X(15).
000209     12  W-NAME-MIDDLE.
000210         16  FILLER              PIC  X.
000211         16  W-NAME-MIDDLE-2     PIC  X.
000212         16  FILLER              PIC  X(13).
000213
000214     12  WS-DMO-LENGTH           PIC S9(4)   VALUE +108 COMP.
000215     12  WS-DCT-LENGTH           PIC S9(4)   VALUE +53 COMP.
000216 01  CSO-WORK-FIELDS.
000217     05  ERROR-ON-OUTPUT-SW          PIC X       VALUE 'N'.
000218       88  ERROR-ON-OUTPUT                       VALUE 'Y'.
000219 EJECT
000220 01  ELCRTT-KEY.
000221     05  CTRLR-COMP-CD       PIC X.
000222     05  CTRLR-CARRIER       PIC X.
000223     05  CTRLR-GROUPING      PIC X(6).
000224     05  CTRLR-STATE         PIC X(2).
000225     05  CTRLR-ACCOUNT       PIC X(10).
000226     05  CTRLR-EFF-DT        PIC XX.
000227     05  CTRLR-CERT-NO       PIC X(11).
000228     05  CTRLR-REC-TYPE      PIC X.
000229
000230 01  ACCESS-KEYS.
000231     12  ELMSTR-KEY.
000232         16  MSTR-COMP-CD            PIC X.
000233         16  MSTR-CARRIER            PIC X.
000234         16  MSTR-CLAIM-NO           PIC X(7).
000235         16  MSTR-CERT-NO.
000236             20  MSTR-CERT-NO-PRIME  PIC X(10).
000237             20  MSTR-CERT-NO-SUFX   PIC X.
000238     12  ELCNTL-KEY.
000239         16  CNTL-COMP-ID            PIC X(3).
000240         16  CNTL-REC-TYPE           PIC X.
000241         16  CNTL-ACCESS             PIC X(4).
000242         16  CNTL-SEQ-NO             PIC S9(4)     COMP.
000243     12  ELCERT-KEY.
000244         16  CERT-COMP-CD            PIC X.
000245         16  CERT-CARRIER            PIC X.
000246         16  CERT-GROUPING           PIC X(6).
000247         16  CERT-STATE              PIC XX.
000248         16  CERT-ACCOUNT            PIC X(10).
000249         16  CERT-EFF-DT             PIC XX.
000250         16  CERT-CERT-NO.
000251             20  CERT-CERT-NO-PRIME  PIC X(10).
000252             20  CERT-CERT-NO-SUFX   PIC X.
000253     12  ELTRLR-KEY.
000254         16  TRLR-COMP-CD            PIC X.
000255         16  TRLR-CARRIER            PIC X.
000256         16  TRLR-CLAIM-NO           PIC X(7).
000257         16  TRLR-CERT-NO.
000258             20  TRLR-CERT-NO-PRIME  PIC X(10).
000259             20  TRLR-CERT-NO-SUFX   PIC X.
000260         16  TRLR-SEQ-NO             PIC S9(4)   COMP.
000261     12  ELACTQ-KEY.
000262         16  ACTQ-COMP-CD            PIC X.
000263         16  ACTQ-CARRIER            PIC X.
000264         16  ACTQ-CLAIM-NO           PIC X(7).
000265         16  ACTQ-CERT-NO.
000266             20  ACTQ-CERT-NO-PRIME  PIC X(10).
000267             20  ACTQ-CERT-NO-SUFX   PIC X.
000268     12  ELCHKQ-KEY.
000269         16  CHKQ-COMP-CD            PIC X.
000270         16  CHKQ-CONTROL            PIC S9(8)   COMP.
000271         16  CHKQ-SEQ-NO             PIC S9(4)   COMP.
000272     12  ELRCON-KEY.
000273         16  RCON-COMPANY-CD         PIC X.
000274         16  RCON-CHECK-NO           PIC X(7).
000275         16  RCON-CHECK-ORIGIN       PIC X.
000276         16  RCON-GL-ACCOUNT-NO      PIC X(10).
000277     12  EMPLCY-KEY.
000278         16  PLCY-COMPANY-CD         PIC X.
000279         16  PLCY-CARRIER            PIC X.
000280         16  PLCY-GROUPING           PIC X(6).
000281         16  PLCY-STATE              PIC XX.
000282         16  PLCY-PRODUCER           PIC X(10).
000283         16  PLCY-EFF-DT             PIC XX.
000284         16  PLCY-REFERENCE-NO       PIC X(20).
000285     12  W-NOTE-KEY.
000286         16  W-NOTE-COMP-CD      PIC X.
000287         16  W-NOTE-CERT-KEY.
000288             20  W-NOTE-CARRIER  PIC X.
000289             20  W-NOTE-GROUPING PIC X(6).
000290             20  W-NOTE-STATE    PIC X(2).
000291             20  W-NOTE-ACCOUNT  PIC X(10).
000292             20  W-NOTE-EFF-DT   PIC XX.
000293             20  W-NOTE-CERT-NO  PIC X(11).
000294
000295     EJECT
000296 01  ERROR-MESSAGES.
000297     12  ER-0000                     PIC X(4)    VALUE '0000'.
000298     12  ER-0004                     PIC X(4)    VALUE '0004'.
000299     12  ER-0008                     PIC X(4)    VALUE '0008'.
000300     12  ER-0029                     PIC X(4)    VALUE '0029'.
000301     12  ER-0033                     PIC X(4)    VALUE '0033'.
000302     12  ER-0042                     PIC X(4)    VALUE '0042'.
000303     12  ER-0068                     PIC X(4)    VALUE '0068'.
000304     12  ER-0070                     PIC X(4)    VALUE '0070'.
000305     12  ER-0130                     PIC X(4)    VALUE '0130'.
000306     12  ER-0154                     PIC X(4)    VALUE '0154'.
000307     12  ER-0169                     PIC X(4)    VALUE '0169'.
000308     12  ER-0172                     PIC X(4)    VALUE '0172'.
000309     12  ER-0190                     PIC X(4)    VALUE '0190'.
000310     12  ER-0204                     PIC X(4)    VALUE '0204'.
000311     12  ER-0206                     PIC X(4)    VALUE '0206'.
000312     12  ER-0282                     PIC X(4)    VALUE '0282'.
000313     12  ER-0303                     PIC X(4)    VALUE '0303'.
000314     12  ER-0334                     PIC X(4)    VALUE '0334'.
000315     12  ER-0335                     PIC X(4)    VALUE '0335'.
000316     12  ER-0336                     PIC X(4)    VALUE '0336'.
000317     12  ER-0337                     PIC X(4)    VALUE '0337'.
000318     12  ER-0338                     PIC X(4)    VALUE '0338'.
000319     12  ER-0376                     PIC X(4)    VALUE '0376'.
000320     12  ER-0412                     PIC X(4)    VALUE '0412'.
000321     12  ER-0413                     PIC X(4)    VALUE '0413'.
000322     12  ER-0660                     PIC X(4)    VALUE '0660'.
000323     12  ER-0661                     PIC X(4)    VALUE '0661'.
000324     12  ER-0662                     PIC X(4)    VALUE '0662'.
000325     12  ER-0663                     PIC X(4)    VALUE '0663'.
000326     12  ER-0664                     PIC X(4)    VALUE '0664'.
000327     12  ER-0665                     PIC X(4)    VALUE '0665'.
000328     12  ER-0666                     PIC X(4)    VALUE '0666'.
000329     12  ER-0667                     PIC X(4)    VALUE '0667'.
000330     12  ER-0672                     PIC X(4)    VALUE '0672'.
000331     12  ER-0776                     PIC X(4)    VALUE '0776'.
000332     12  ER-0800                     PIC X(4)    VALUE '0800'.
000333     12  ER-0801                     PIC X(4)    VALUE '0801'.
000334     12  ER-0816                     PIC X(4)    VALUE '0816'.
000335     12  ER-0823                     PIC X(4)    VALUE '0823'.
000336     12  ER-0833                     PIC X(4)    VALUE '0833'.
000337     12  ER-0835                     PIC X(4)    VALUE '0835'.
000338     12  ER-0849                     PIC X(4)    VALUE '0849'.
000339     12  ER-0919                     PIC X(4)    VALUE '0919'.
000340     12  ER-0920                     PIC X(4)    VALUE '0920'.
000341     12  ER-0921                     PIC X(4)    VALUE '0921'.
000342     12  ER-0922                     PIC X(4)    VALUE '0922'.
000343     12  ER-0923                     PIC X(4)    VALUE '0923'.
000344     12  ER-0925                     PIC X(4)    VALUE '0925'.
000345     12  ER-0939                     PIC X(4)    VALUE '0939'.
000346     12  ER-0940                     PIC X(4)    VALUE '0940'.
000347     12  ER-0941                     PIC X(4)    VALUE '0941'.
000348     12  ER-0942                     PIC X(4)    VALUE '0942'.
000349     12  ER-0946                     PIC X(4)    VALUE '0946'.
000350     12  ER-0947                     PIC X(4)    VALUE '0947'.
000351     12  ER-0948                     PIC X(4)    VALUE '0948'.
000352     12  ER-0949                     PIC X(4)    VALUE '0949'.
000353     12  ER-0950                     PIC X(4)    VALUE '0950'.
000354     12  ER-0951                     PIC X(4)    VALUE '0951'.
000355     12  ER-0954                     PIC X(4)    VALUE '0954'.
000356     12  ER-0974                     PIC X(4)    VALUE '0974'.
000357     12  ER-0975                     PIC X(4)    VALUE '0975'.
000358     12  ER-2378                     PIC X(4)    VALUE '2378'.
000359     12  ER-2379                     PIC X(4)    VALUE '2379'.
000360     12  ER-2893                     PIC X(4)    VALUE '2893'.
000361     12  ER-7999                     PIC X(4)    VALUE '7999'.
000362     12  ER-8003                     PIC X(4)    VALUE '8003'.
000363     12  ER-8051                     PIC X(4)    VALUE '8051'.
000364     12  ER-8052                     PIC X(4)    VALUE '8052'.
000365     12  ER-8053                     PIC X(4)    VALUE '8053'.
000366     12  ER-8054                     PIC X(4)    VALUE '8054'.
000367     12  ER-8055                     PIC X(4)    VALUE '8055'.
000368     12  ER-8056                     PIC X(4)    VALUE '8056'.
000369     12  ER-8057                     PIC X(4)    VALUE '8057'.
000370     12  ER-8058                     PIC X(4)    VALUE '8058'.
000371     12  ER-8059                     PIC X(4)    VALUE '8059'.
000372     12  ER-8060                     PIC X(4)    VALUE '8060'.
000373     12  ER-8061                     PIC X(4)    VALUE '8061'.
000374     12  ER-8062                     PIC X(4)    VALUE '8062'.
000375     12  ER-8063                     PIC X(4)    VALUE '8063'.
000376     12  ER-8064                     PIC X(4)    VALUE '8064'.
000377     12  ER-8065                     PIC X(4)    VALUE '8065'.
000378     12  ER-8066                     PIC X(4)    VALUE '8066'.
000379     12  ER-8152                     PIC X(4)    VALUE '8152'.
000380     12  ER-8153                     PIC X(4)    VALUE '8153'.
000381     12  ER-8154                     PIC X(4)    VALUE '8154'.
000382     12  ER-8155                     PIC X(4)    VALUE '8155'.
000383     12  er-8162                     pic x(4)    value '8162'.
000384     12  ER-9211                     PIC X(4)    VALUE '9211'.
000385     12  ER-9883                     PIC X(4)    VALUE '9883'.
000386
000387 EJECT
000388 01  TEXT-WORK-AREAS.
000389     12  PAYMENT-LINE-1.
000390         16  PMT-HDG1.
000391             20  PMT-LINE-NO         PIC X.
000392             20  FILLER              PIC X.
000393             20  PMT-HDG1-LIT        PIC X(8).
000394         16  PMT-TEXT-1.
000395             20  PMT-TYPE-LIT        PIC X(6).
000396             20  PMT-TYPE            PIC X(7).
000397             20  PMT-CHECK-NO-LIT    PIC X(11).
000398             20  PMT-CHECK-NO        PIC X(7).
000399             20  PMT-DT-WRITTEN-LIT  PIC X(14).
000400             20  PMT-DT-WRITTEN      PIC X(8).
000401             20  PMT-AMT-PD-LIT      PIC X(5).
000402             20  PMT-AMT-PAID        PIC Z(6).99-.
000403     12  PAYMENT-LINE-2.
000404         16  PMT-HDG2.
000405             20  FILLER              PIC X(6).
000406             20  PMT-HDG2-LIT        PIC X(4).
000407         16  PMT-TEXT-2.
000408             20  PMT-FROM-LIT        PIC X(6).
000409             20  PMT-PAID-FROM       PIC X(8).
000410             20  PMT-THRU-LIT        PIC X(10).
000411             20  PMT-PAID-THRU       PIC X(8).
000412             20  PMT-VOID-LIT        PIC X(13).
000413             20  PMT-VOID-DT         PIC X(8).
000414             20  PMT-PAYEE-LIT       PIC X(8).
000415             20  PMT-PAYEE           PIC X(7).
000416     12  AUTO-PMT-LINE-1.
000417         16  AUTO-PMT-HDG1.
000418             20  AUTO-LINE-NO        PIC X.
000419             20  FILLER              PIC X.
000420             20  AUTO-HDG1-LIT       PIC X(8).
000421         16  AUTO-PMT-TEXT1.
000422             20  AUTO-EFF-DT-LIT     PIC X(6).
000423             20  AUTO-EFF-DT         PIC X(8).
000424             20  AUTO-1ST-AMT-LIT    PIC X(10).
000425             20  AUTO-1ST-AMT        PIC Z(5).99.
000426             20  AUTO-REG-AMT-LIT    PIC X(14).
000427             20  AUTO-REG-AMT        PIC Z(5).99.
000428             20  AUTO-PMT-STATUS-LIT PIC X(8).
000429             20  AUTO-PMT-STATUS     PIC X(6).
000430     12  AUTO-PMT-LINE-2.
000431         16  AUTO-PMT-HDG2.
000432             20  FILLER              PIC X(6).
000433             20  AUTO-HDG2-LIT       PIC X(4).
000434         16  AUTO-PMT-TEXT2.
000435             20  AUTO-PAYEE-LIT      PIC X(6).
000436             20  AUTO-PAYEE          PIC X(8).
000437             20  AUTO-1ST-PMT-LIT    PIC X(10).
000438             20  AUTO-1ST-PMT-DT     PIC X(8).
000439             20  AUTO-LST-PMT-LIT    PIC X(14).
000440             20  AUTO-LST-PMT-DT     PIC X(8).
000441             20  FILLER              PIC X(4).
000442     12  CORRESPONDENCE-LINE-1.
000443         16  CORR-HDG1.
000444             20  CORR-LINE-NO        PIC X.
000445             20  FILLER              PIC X.
000446             20  CORR-HDG1-LIT       PIC X(8).
000447         16  CORR-TEXT-1.
000448             20  CORR-FORM-LIT       PIC X(6).
000449             20  CORR-FORM-TYPE      PIC X(4).
000450*             20  FILLER              PIC X(4).
000451             20  CORR-DT-SENT-LIT    PIC X(10).
000452             20  CORR-DT-SENT        PIC X(8).
000453             20  CORR-INIT-PRT-LIT   PIC X(11).
000454             20  CORR-INIT-PRT-DT    PIC X(8).
000455             20  CORR-ADDR-LIT       PIC X(05).
000456             20  CORR-ADDR-TYPE      PIC XX.
000457             20  CORR-LET-TO-BEN     PIC X(14).
000458     12  CORRESPONDENCE-LINE-2.
000459         16  CORR-HDG2.
000460             20  FILLER              PIC X(6).
000461             20  CORR-HDG2-LIT       PIC X(4).
000462         16  CORR-TEXT-2.
000463             20  CORR-RESEND-LIT     PIC X(6).
000464             20  CORR-RESEND-DT      PIC X(8).
000465             20  CORR-RECVD-LIT      PIC X(10).
000466             20  CORR-RECVD-DT       PIC X(8).
000467             20  CORR-FOLLOW-UP-LIT  PIC X(14).
000468             20  CORR-FOLLOW-UP-DT   PIC X(8).
000469             20  CORR-ARCH-LIT       PIC X(6).
000470             20  CORR-ARCH-NO        PIC 9(8).
000471*            20  FILLER              PIC X(14).
000472     12  FORM-LINE-1.
000473         16  FORM-HDG1.
000474             20  FORM-LINE-NO        PIC X.
000475             20  FILLER              PIC X.
000476             20  FORM-HDG1-LIT       PIC X(8).
000477         16  FORM-TEXT-1.
000478             20  FORM-TYPE-LIT       PIC X(6).
000479             20  FORM-TYPE           PIC X(4).
000480             20  FILLER              PIC X(4).
000481             20  FORM-SEND-ON-LIT    PIC X(10).
000482             20  FORM-SEND-ON-DT     PIC X(8).
000483             20  FORM-RESEND-LIT     PIC X(14).
000484             20  FORM-RESEND-DT      PIC X(8).
000485             20  FORM-FOLLOW-UP-LIT  PIC X(6).
000486             20  FORM-FOLLOW-UP-DT   PIC X(8).
000487     12  FORM-LINE-2.
000488         16  FORM-HDG2.
000489             20  FILLER              PIC X(6).
000490             20  FORM-HDG2-LIT       PIC X(4).
000491         16  FORM-TEXT-2.
000492             20  FORM-REC-INS-LIT    PIC X(6).
000493             20  FORM-REC-INS-DT     PIC X(8).
000494             20  FORM-REC-PHY-LIT    PIC X(10).
000495             20  FORM-REC-PHY-DT     PIC X(8).
000496             20  FORM-REC-EMP-LIT    PIC X(14).
000497             20  FORM-REC-EMP-DT     PIC X(8).
000498             20  FILLER              PIC X(12).
000499     12  INCUR-CHG-LINE-1.
000500         16  INCUR-CHG-HDG1.
000501             20  INCUR-LINE-NO       PIC X.
000502             20  FILLER              PIC X.
000503             20  INCUR-HDG1-LIT      PIC X(8).
000504         16  INCUR-TEXT-1.
000505             20  INCUR-INCUR-LIT     PIC X(6).
000506             20  INCUR-INCUR-DT      PIC X(8).
000507             20  INCUR-REPORT-LIT    PIC X(10).
000508             20  INCUR-REPORT-DT     PIC X(8).
000509             20  INCUR-ESTAB-LIT     PIC X(14).
000510             20  INCUR-ESTAB-DT      PIC X(8).
000511             20  INCUR-NO-PMTS-LIT   PIC X(11).
000512             20  INCUR-NO-PMTS       PIC ZZ9.
000513     12  INCUR-CHG-LINE-2.
000514         16  INCUR-CHG-HDG2.
000515             20  FILLER              PIC X(6).
000516             20  INCUR-HDG2-LIT      PIC X(4).
000517         16  INCUR-TEXT-2.
000518             20  INCUR-PD-THRU-LIT   PIC X(6).
000519             20  INCUR-PD-THRU-DT    PIC X(8).
000520             20  INCUR-TOT-PD-LIT    PIC X(10).
000521             20  INCUR-TOT-PD        PIC Z(5).99.
000522             20  INCUR-TOT-DAYS-LIT  PIC X(14).
000523             20  INCUR-TOT-DAYS-PD   PIC ZZ9.
000524             20  FILLER              PIC X(19).
000525     12  DENIAL-LINE-1.
000526         16  DENIAL-HDG1.
000527             20  DENIAL-LINE-NO      PIC X.
000528             20  FILLER              PIC X.
000529             20  DENIAL-HDG1-LIT     PIC X(8).
000530         16  DENIAL-TEXT-1.
000531             20  DENIAL-LN1-LIT      PIC X(6).
000532             20  DENIAL-LN1          PIC X(62).
000533     12  DENIAL-LINE-2.
000534         16  DENIAL-HDG2.
000535             20  FILLER              PIC X(10).
000536         16  DENIAL-TEXT-2.
000537             20  DENIAL-LN2-LIT      PIC X(6).
000538             20  DENIAL-LN2          PIC X(62).
000539     12  GEN-INFO-LINE-1.
000540         16  GEN-INFO-HDG1.
000541             20  GI-LINE-NO          PIC X.
000542             20  FILLER              PIC X.
000543             20  GI-HDG1-LIT         PIC X(8).
000544         16  GEN-INFO-TEXT-1.
000545             20  GEN-INFO-MSG-1-LIT  PIC X(6).
000546             20  GEN-INFO-MSG-1      PIC X(62).
000547     12  GEN-INFO-LINE-2.
000548         16  GEN-INFO-HDG2.
000549             20  FILLER              PIC XX.
000550             20  GI-HDG2-LIT         PIC X(8).
000551         16  GEN-INFO-TEXT-2.
000552             20  GEN-INFO-MSG-2-LIT  PIC X(6).
000553             20  GEN-INFO-MSG-2      PIC X(62).
000554     12  REMINDER-LINE-1.
000555         16  REMINDER-HDG1.
000556             20  REM-LINE-NO         PIC X.
000557             20  FILLER              PIC X.
000558             20  REM-HDG1-LIT        PIC X(8).
000559         16  REMINDER-TEXT-1.
000560             20  REM-LINE-1-LIT      PIC X(6).
000561             20  REM-LINE-1          PIC X(62).
000562     12  REMINDER-LINE-2.
000563         16  REMINDER-HDG2.
000564             20  FILLER              PIC X(10).
000565         16  REMINDER-TEXT-2.
000566             20  REM-LINE-2-LIT      PIC X(6).
000567             20  REM-LINE-2          PIC X(62).
000568
000569 01  PAYMENT-DESCRIPTION-TABLE.
000570     12  FILLER                      PIC X(8)    VALUE 'PARTIAL '.
000571     12  FILLER                      PIC X(8)    VALUE 'FINAL   '.
000572     12  FILLER                      PIC X(8)    VALUE 'LUMP SUM'.
000573     12  FILLER                      PIC X(8)    VALUE 'ADDITION'.
000574     12  FILLER                      PIC X(8)    VALUE 'CHRG EXP'.
000575     12  FILLER                      PIC X(8)    VALUE 'NON-CHG '.
000576     12  FILLER                      PIC X(8)    VALUE 'LF PRM  '.
000577     12  FILLER                      PIC X(8)    VALUE 'A/H PRM '.
000578     12  FILLER                      PIC X(8)    VALUE 'ENT CORR'.
000579 01  PAYMENT-DESC-R   REDEFINES PAYMENT-DESCRIPTION-TABLE.
000580     12  PAY-DESC                    PIC X(8)    OCCURS 2.
000581
000582 01  CV-PAYMENT-DESCRIPTION-TABLE.
000583     12  FILLER                      PIC X(7)    VALUE 'FUL DTH'.
000584     12  FILLER                      PIC X(7)    VALUE 'HLF DTH'.
000585     12  FILLER                      PIC X(7)    VALUE 'FUL ADD'.
000586     12  FILLER                      PIC X(7)    VALUE 'HLF ADD'.
000587     12  FILLER                      PIC X(7)    VALUE 'FUL RID'.
000588     12  FILLER                      PIC X(7)    VALUE 'HLF RID'.
000589     12  FILLER                      PIC X(7)    VALUE 'NON-CHG'.
000590     12  FILLER                      PIC X(7)    VALUE 'ADDL   '.
000591 01  CV-PAYMENT-DESC-R REDEFINES CV-PAYMENT-DESCRIPTION-TABLE.
000592     12  CV-PAY-DESC                 PIC X(7)    OCCURS 2.
000593
000594     EJECT
000595*                                    COPY ELCDATE.
      *>>((file: ELCDATE))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCDATE.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003
000007*                                                                *
000008*                                                                *
000009*   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
000010*                 LENGTH = 200                                   *
000011******************************************************************
000012
000013 01  DATE-CONVERSION-DATA.
000014     12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
000015     12  DC-OPTION-CODE                PIC X.
000016         88  BIN-TO-GREG                VALUE ' '.
000017         88  ELAPSED-BETWEEN-BIN        VALUE '1'.
000018         88  EDIT-GREG-TO-BIN           VALUE '2'.
000019         88  YMD-GREG-TO-BIN            VALUE '3'.
000020         88  MDY-GREG-TO-BIN            VALUE '4'.
000021         88  JULIAN-TO-BIN              VALUE '5'.
000022         88  BIN-PLUS-ELAPSED           VALUE '6'.
000023         88  FIND-CENTURY               VALUE '7'.
000024         88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
000025         88  EDIT-GREG-TO-BIN-3         VALUE '9'.
000026         88  YMD-GREG-TO-BIN-3          VALUE 'A'.
000027         88  MDY-GREG-TO-BIN-3          VALUE 'B'.
000028         88  JULIAN-TO-BIN-3            VALUE 'C'.
000029         88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
000030         88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
000031         88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
000032         88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
000033         88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
000034         88  CHECK-LEAP-YEAR            VALUE 'H'.
000035         88  BIN-3-TO-GREG              VALUE 'I'.
000036         88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
000037         88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
000038         88  CYMD-GREG-TO-BIN           VALUE 'L'.
000039         88  MDCY-GREG-TO-BIN           VALUE 'M'.
000040         88  MDY-GREG-TO-JULIAN         VALUE 'N'.
000041         88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
000042         88  YMD-GREG-TO-JULIAN         VALUE 'P'.
000043         88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
000044         88  THREE-CHARACTER-BIN
000045                  VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
000046         88  GREGORIAN-TO-BIN
000047                  VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
000048         88  BIN-TO-GREGORIAN
000049                  VALUES ' ' '1' 'I' '8' 'G'.
000050         88  JULIAN-TO-BINARY
000051                  VALUES '5' 'C' 'E' 'F'.
000052     12  DC-ERROR-CODE                 PIC X.
000053         88  NO-CONVERSION-ERROR        VALUE ' '.
000054         88  DATE-CONVERSION-ERROR
000055                  VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
000056         88  DATE-IS-ZERO               VALUE '1'.
000057         88  DATE-IS-NON-NUMERIC        VALUE '2'.
000058         88  DATE-IS-INVALID            VALUE '3'.
000059         88  DATE1-GREATER-DATE2        VALUE '4'.
000060         88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
000061         88  DATE-INVALID-OPTION        VALUE '9'.
000062         88  INVALID-CENTURY            VALUE 'A'.
000063         88  ONLY-CENTURY               VALUE 'B'.
000064         88  ONLY-LEAP-YEAR             VALUE 'C'.
000065         88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
000066     12  DC-END-OF-MONTH               PIC X.
000067         88  CALCULATE-END-OF-MONTH     VALUE '1'.
000068     12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
000069         88  USE-NORMAL-PROCESS         VALUE ' '.
000070         88  ADJUST-DOWN-100-YRS        VALUE '1'.
000071         88  ADJUST-UP-100-YRS          VALUE '2'.
000072     12  FILLER                        PIC X.
000073     12  DC-CONVERSION-DATES.
000074         16  DC-BIN-DATE-1             PIC XX.
000075         16  DC-BIN-DATE-2             PIC XX.
000076         16  DC-GREG-DATE-1-EDIT       PIC X(08).
000077         16  DC-GREG-DATE-1-EDIT-R REDEFINES
000078                       DC-GREG-DATE-1-EDIT.
000079             20  DC-EDIT1-MONTH        PIC 99.
000080             20  SLASH1-1              PIC X.
000081             20  DC-EDIT1-DAY          PIC 99.
000082             20  SLASH1-2              PIC X.
000083             20  DC-EDIT1-YEAR         PIC 99.
000084         16  DC-GREG-DATE-2-EDIT       PIC X(08).
000085         16  DC-GREG-DATE-2-EDIT-R REDEFINES
000086                     DC-GREG-DATE-2-EDIT.
000087             20  DC-EDIT2-MONTH        PIC 99.
000088             20  SLASH2-1              PIC X.
000089             20  DC-EDIT2-DAY          PIC 99.
000090             20  SLASH2-2              PIC X.
000091             20  DC-EDIT2-YEAR         PIC 99.
000092         16  DC-GREG-DATE-1-YMD        PIC 9(06).
000093         16  DC-GREG-DATE-1-YMD-R  REDEFINES
000094                     DC-GREG-DATE-1-YMD.
000095             20  DC-YMD-YEAR           PIC 99.
000096             20  DC-YMD-MONTH          PIC 99.
000097             20  DC-YMD-DAY            PIC 99.
000098         16  DC-GREG-DATE-1-MDY        PIC 9(06).
000099         16  DC-GREG-DATE-1-MDY-R REDEFINES
000100                      DC-GREG-DATE-1-MDY.
000101             20  DC-MDY-MONTH          PIC 99.
000102             20  DC-MDY-DAY            PIC 99.
000103             20  DC-MDY-YEAR           PIC 99.
000104         16  DC-GREG-DATE-1-ALPHA.
000105             20  DC-ALPHA-MONTH        PIC X(10).
000106             20  DC-ALPHA-DAY          PIC 99.
000107             20  FILLER                PIC XX.
000108             20  DC-ALPHA-CENTURY.
000109                 24 DC-ALPHA-CEN-N     PIC 99.
000110             20  DC-ALPHA-YEAR         PIC 99.
000111         16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
000112         16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
000113         16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
000114         16  DC-JULIAN-DATE            PIC 9(05).
000115         16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
000116                                       PIC 9(05).
000117         16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
000118             20  DC-JULIAN-YEAR        PIC 99.
000119             20  DC-JULIAN-DAYS        PIC 999.
000120         16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
000121         16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
000122         16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
000123     12  DATE-CONVERSION-VARIBLES.
000124         16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
000125         16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
000126             20  FILLER                PIC 9(3).
000127             20  HOLD-CEN-1-CCYY.
000128                 24  HOLD-CEN-1-CC     PIC 99.
000129                 24  HOLD-CEN-1-YY     PIC 99.
000130             20  HOLD-CEN-1-MO         PIC 99.
000131             20  HOLD-CEN-1-DA         PIC 99.
000132         16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
000133             20  HOLD-CEN-1-R-MO       PIC 99.
000134             20  HOLD-CEN-1-R-DA       PIC 99.
000135             20  HOLD-CEN-1-R-CCYY.
000136                 24  HOLD-CEN-1-R-CC   PIC 99.
000137                 24  HOLD-CEN-1-R-YY   PIC 99.
000138             20  FILLER                PIC 9(3).
000139         16  HOLD-CENTURY-1-X.
000140             20  FILLER                PIC X(3)  VALUE SPACES.
000141             20  HOLD-CEN-1-X-CCYY.
000142                 24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
000143                 24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
000144             20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
000145             20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
000146         16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
000147             20  HOLD-CEN-1-R-X-MO     PIC XX.
000148             20  HOLD-CEN-1-R-X-DA     PIC XX.
000149             20  HOLD-CEN-1-R-X-CCYY.
000150                 24  HOLD-CEN-1-R-X-CC PIC XX.
000151                 24  HOLD-CEN-1-R-X-YY PIC XX.
000152             20  FILLER                PIC XXX.
000153         16  DC-BIN-DATE-EXPAND-1      PIC XXX.
000154         16  DC-BIN-DATE-EXPAND-2      PIC XXX.
000155         16  DC-JULIAN-DATE-1          PIC 9(07).
000156         16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
000157             20  DC-JULIAN-1-CCYY.
000158                 24  DC-JULIAN-1-CC    PIC 99.
000159                 24  DC-JULIAN-1-YR    PIC 99.
000160             20  DC-JULIAN-DA-1        PIC 999.
000161         16  DC-JULIAN-DATE-2          PIC 9(07).
000162         16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
000163             20  DC-JULIAN-2-CCYY.
000164                 24  DC-JULIAN-2-CC    PIC 99.
000165                 24  DC-JULIAN-2-YR    PIC 99.
000166             20  DC-JULIAN-DA-2        PIC 999.
000167         16  DC-GREG-DATE-A-EDIT.
000168             20  DC-EDITA-MONTH        PIC 99.
000169             20  SLASHA-1              PIC X VALUE '/'.
000170             20  DC-EDITA-DAY          PIC 99.
000171             20  SLASHA-2              PIC X VALUE '/'.
000172             20  DC-EDITA-CCYY.
000173                 24  DC-EDITA-CENT     PIC 99.
000174                 24  DC-EDITA-YEAR     PIC 99.
000175         16  DC-GREG-DATE-B-EDIT.
000176             20  DC-EDITB-MONTH        PIC 99.
000177             20  SLASHB-1              PIC X VALUE '/'.
000178             20  DC-EDITB-DAY          PIC 99.
000179             20  SLASHB-2              PIC X VALUE '/'.
000180             20  DC-EDITB-CCYY.
000181                 24  DC-EDITB-CENT     PIC 99.
000182                 24  DC-EDITB-YEAR     PIC 99.
000183         16  DC-GREG-DATE-CYMD         PIC 9(08).
000184         16  DC-GREG-DATE-CYMD-R REDEFINES
000185                              DC-GREG-DATE-CYMD.
000186             20  DC-CYMD-CEN           PIC 99.
000187             20  DC-CYMD-YEAR          PIC 99.
000188             20  DC-CYMD-MONTH         PIC 99.
000189             20  DC-CYMD-DAY           PIC 99.
000190         16  DC-GREG-DATE-MDCY         PIC 9(08).
000191         16  DC-GREG-DATE-MDCY-R REDEFINES
000192                              DC-GREG-DATE-MDCY.
000193             20  DC-MDCY-MONTH         PIC 99.
000194             20  DC-MDCY-DAY           PIC 99.
000195             20  DC-MDCY-CEN           PIC 99.
000196             20  DC-MDCY-YEAR          PIC 99.
000197    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
000198        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
000199    12  DC-EL310-DATE                  PIC X(21).
000200    12  FILLER                         PIC X(28).
      *<<((file: ELCDATE))
000596     EJECT
000597*                                    COPY ELCLOGOF.
      *>>((file: ELCLOGOF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCLOGOF.                           *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
000008*                                                                *
000009******************************************************************
000010 01  CLASIC-LOGOFF.
000011     12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
000012     12  LOGOFF-TEXT.
000013         16  FILLER          PIC X(5)    VALUE SPACES.
000014         16  LOGOFF-MSG.
000015             20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
000016             20  FILLER      PIC X       VALUE SPACES.
000017             20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
000018         16  FILLER          PIC X(80)
000019           VALUE '* YOU ARE NOW LOGGED OFF'.
000020         16  FILLER          PIC X(7)    VALUE '* LOGIC'.
000021         16  FILLER          PIC X       VALUE QUOTE.
000022         16  LOGOFF-SYS-MSG  PIC X(17)
000023           VALUE 'S CLAS-IC SYSTEM '.
000024     12  TEXT-MESSAGES.
000025         16  UNACCESS-MSG    PIC X(29)
000026             VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
000027         16  PGMIDERR-MSG    PIC X(17)
000028             VALUE 'PROGRAM NOT FOUND'.
      *<<((file: ELCLOGOF))
000598     EJECT
000599*                                    COPY ELCATTR.
      *>>((file: ELCATTR))
000001******************************************************************
000002*                                                                *
000003*                            ELCATTR.                            *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*             LIST OF STANDARD ATTRIBUTE VALUES                  *
000007*                                                                *
000008*   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
000009*                                                                *
000010*                   POS 1   P=PROTECTED                          *
000011*                           U=UNPROTECTED                        *
000012*                           S=ASKIP                              *
000013*                   POS 2   A=ALPHA/NUMERIC                      *
000014*                           N=NUMERIC                            *
000015*                   POS 3   N=NORMAL                             *
000016*                           B=BRIGHT                             *
000017*                           D=DARK                               *
000018*                   POS 4-5 ON=MODIFIED DATA TAG ON              *
000019*                           OF=MODIFIED DATA TAG OFF             *
000020*                                                                *
000021*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
000022******************************************************************
000023 01  ATTRIBUTE-LIST.
000024     12  AL-PABOF            PIC X       VALUE 'Y'.
000025     12  AL-PABON            PIC X       VALUE 'Z'.
000026     12  AL-PADOF            PIC X       VALUE '%'.
000027     12  AL-PADON            PIC X       VALUE '_'.
000028     12  AL-PANOF            PIC X       VALUE '-'.
000029     12  AL-PANON            PIC X       VALUE '/'.
000030     12  AL-SABOF            PIC X       VALUE '8'.
000031     12  AL-SABON            PIC X       VALUE '9'.
000032     12  AL-SADOF            PIC X       VALUE '@'.
000033     12  AL-SADON            PIC X       VALUE QUOTE.
000034     12  AL-SANOF            PIC X       VALUE '0'.
000035     12  AL-SANON            PIC X       VALUE '1'.
000036     12  AL-UABOF            PIC X       VALUE 'H'.
000037     12  AL-UABON            PIC X       VALUE 'I'.
000038     12  AL-UADOF            PIC X       VALUE '<'.
000039     12  AL-UADON            PIC X       VALUE '('.
000040     12  AL-UANOF            PIC X       VALUE ' '.
000041     12  AL-UANON            PIC X       VALUE 'A'.
000042     12  AL-UNBOF            PIC X       VALUE 'Q'.
000043     12  AL-UNBON            PIC X       VALUE 'R'.
000044     12  AL-UNDOF            PIC X       VALUE '*'.
000045     12  AL-UNDON            PIC X       VALUE ')'.
000046     12  AL-UNNOF            PIC X       VALUE '&'.
000047     12  AL-UNNON            PIC X       VALUE 'J'.
      *<<((file: ELCATTR))
000600     EJECT
000601*                                    COPY ELCDCTB.
000602*    EJECT
000603*                                    COPY ELCDMO.
000604*    EJECT
000605*                                    COPY ELCEMIB.
      *>>((file: ELCEMIB))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCEMIB.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.005                          *
000007*                                                                *
000008*    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
000009*                                                                *
000010******************************************************************
000011 01  ERROR-MESSAGE-INTERFACE-BLOCK.
000012     12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
000013     12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
000014     12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
000015     12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
000016     12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
000017     12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
000018     12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
000019     12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
000020     12  EMI-SWITCH1             PIC X        VALUE '1'.
000021         88  EMI-NO-ERRORS                    VALUE '1'.
000022         88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
000023         88  EMI-ERRORS-COMPLETE              VALUE '3'.
000024     12  EMI-SWITCH2             PIC X        VALUE '1'.
000025         88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
000026     12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
000027         88  EMI-AREA1-EMPTY                  VALUE '1'.
000028         88  EMI-AREA1-FULL                   VALUE '2'.
000029     12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
000030         88  EMI-AREA2-EMPTY                  VALUE '1'.
000031         88  EMI-AREA2-FULL                   VALUE '2'.
000032     12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
000033         88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
000034         88  EMI-BYPASS-NOTES                 VALUE 'N'.
000035         88  EMI-BYPASS-WARNINGS              VALUE 'W'.
000036         88  EMI-BYPASS-FORCABLES             VALUE 'F'.
000037         88  EMI-BYPASS-FATALS                VALUE 'X'.
000038     12  EMI-ERROR-LINES.
000039         16  EMI-LINE1           PIC X(72)   VALUE SPACES.
000040         16  EMI-LINE2           PIC X(72)   VALUE SPACES.
000041         16  EMI-LINE3           PIC X(72)   VALUE SPACES.
000042         16  EMI-CODE-LINE REDEFINES EMI-LINE3.
000043             20  EMI-ERR-CODES OCCURS 10 TIMES.
000044                 24  EMI-ERR-NUM         PIC X(4).
000045                 24  EMI-FILLER          PIC X.
000046                 24  EMI-SEV             PIC X.
000047                 24  FILLER              PIC X.
000048             20  FILLER                  PIC X(02).
000049     12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
000050         16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
000051             20  EMI-ERROR-NUMBER    PIC X(4).
000052             20  EMI-FILL            PIC X.
000053             20  EMI-SEVERITY        PIC X.
000054             20  FILLER              PIC X.
000055             20  EMI-ERROR-TEXT.
000056                 24  EMI-TEXT-VARIABLE   PIC X(10).
000057                 24  FILLER          PIC X(55).
000058     12  EMI-SEVERITY-SAVE           PIC X.
000059         88  EMI-NOTE                    VALUE 'N'.
000060         88  EMI-WARNING                 VALUE 'W'.
000061         88  EMI-FORCABLE                VALUE 'F'.
000062         88  EMI-FATAL                   VALUE 'X'.
000063     12  EMI-MESSAGE-FLAG            PIC X.
000064         88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
000065         88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
000066     12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
000067     12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
000068         88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
000069         88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
000070         88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
000071     12  emi-claim-no                pic x(7).
000072     12  emi-claim-type              pic x(6).
000073     12  FILLER                      PIC X(124)  VALUE SPACES.
000074     12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
000075     12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
000076     12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
000077     12  EMI-AH-OVERRIDE-L6          PIC X(6).
      *<<((file: ELCEMIB))
000606     EJECT
000607*                                    COPY ELCNWA.
000608*    EJECT
000609*                                    COPY MPCPOLUP.
000610*    EJECT
000611*                            COPY ELCINTF.
      *>>((file: ELCINTF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCINTF.                            *
000005*                            VMOD=2.017                          *
000006*                                                                *
000007*   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
000008*                                                                *
000009*       LENGTH = 1024                                            *
000010*                                                                *
000011******************************************************************
000012*                   C H A N G E   L O G
000013*
000014* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000015*-----------------------------------------------------------------
000016*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000017* EFFECTIVE    NUMBER
000018*-----------------------------------------------------------------
000019* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
000020******************************************************************
000021 01  PROGRAM-INTERFACE-BLOCK.
000022     12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
000023     12  PI-CALLING-PROGRAM              PIC X(8).
000024     12  PI-SAVED-PROGRAM-1              PIC X(8).
000025     12  PI-SAVED-PROGRAM-2              PIC X(8).
000026     12  PI-SAVED-PROGRAM-3              PIC X(8).
000027     12  PI-SAVED-PROGRAM-4              PIC X(8).
000028     12  PI-SAVED-PROGRAM-5              PIC X(8).
000029     12  PI-SAVED-PROGRAM-6              PIC X(8).
000030     12  PI-RETURN-TO-PROGRAM            PIC X(8).
000031     12  PI-COMPANY-ID                   PIC XXX.
000032     12  PI-COMPANY-CD                   PIC X.
000033
000034     12  PI-COMPANY-PASSWORD             PIC X(8).
000035
000036     12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
000037
000038     12  PI-CONTROL-IN-PROGRESS.
000039         16  PI-CARRIER                  PIC X.
000040         16  PI-GROUPING                 PIC X(6).
000041         16  PI-STATE                    PIC XX.
000042         16  PI-ACCOUNT                  PIC X(10).
000043         16  PI-PRODUCER REDEFINES PI-ACCOUNT
000044                                         PIC X(10).
000045         16  PI-CLAIM-CERT-GRP.
000046             20  PI-CLAIM-NO             PIC X(7).
000047             20  PI-CERT-NO.
000048                 25  PI-CERT-PRIME       PIC X(10).
000049                 25  PI-CERT-SFX         PIC X.
000050             20  PI-CERT-EFF-DT          PIC XX.
000051         16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
000052             20  PI-PLAN-CODE            PIC X(2).
000053             20  PI-REVISION-NUMBER      PIC X(3).
000054             20  PI-PLAN-EFF-DT          PIC X(2).
000055             20  PI-PLAN-EXP-DT          PIC X(2).
000056             20  FILLER                  PIC X(11).
000057         16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
000058             20  PI-OE-REFERENCE-1.
000059                 25  PI-OE-REF-1-PRIME   PIC X(18).
000060                 25  PI-OE-REF-1-SUFF    PIC XX.
000061
000062     12  PI-SESSION-IN-PROGRESS          PIC X.
000063         88  CLAIM-SESSION                   VALUE '1'.
000064         88  CREDIT-SESSION                  VALUE '2'.
000065         88  WARRANTY-SESSION                VALUE '3'.
000066         88  MORTGAGE-SESSION                VALUE '4'.
000067         88  GENERAL-LEDGER-SESSION          VALUE '5'.
000068
000069
000070*THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
000071
000072     12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
000073     12  PI-ORIGINAL-COMPANY-CD          PIC X.
000074
000075     12  PI-CREDIT-USER                  PIC X.
000076         88  PI-NOT-CREDIT-USER              VALUE 'N'.
000077         88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
000078
000079     12  PI-CLAIM-USER                   PIC X.
000080         88  PI-NOT-CLAIM-USER               VALUE 'N'.
000081         88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
000082
000083     12  PI-PROCESSOR-SYS-ACCESS         PIC X.
000084         88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
000085         88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
000086         88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
000087         88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
000088         88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
000089
000090     12  PI-PROCESSOR-ID                 PIC X(4).
000091
000092     12  PI-PROCESSOR-PASSWORD           PIC X(11).
000093
000094     12  PI-MEMBER-CAPTION               PIC X(10).
000095
000096     12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
000097         88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
000098
000099     12  PI-LIFE-OVERRIDE-L1             PIC X.
000100     12  PI-LIFE-OVERRIDE-L2             PIC XX.
000101     12  PI-LIFE-OVERRIDE-L6             PIC X(6).
000102     12  PI-LIFE-OVERRIDE-L12            PIC X(12).
000103
000104     12  PI-AH-OVERRIDE-L1               PIC X.
000105     12  PI-AH-OVERRIDE-L2               PIC XX.
000106     12  PI-AH-OVERRIDE-L6               PIC X(6).
000107     12  PI-AH-OVERRIDE-L12              PIC X(12).
000108
000109     12  PI-NEW-SYSTEM                   PIC X(2).
000110
000111     12  PI-PRIMARY-CERT-NO              PIC X(11).
000112     12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
000113         88  PI-USES-PAID-TO                 VALUE '1'.
000114     12  PI-CRDTCRD-SYSTEM.
000115         16  PI-CRDTCRD-USER             PIC X.
000116             88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
000117             88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
000118         16  PI-CC-MONTH-END-DT          PIC XX.
000119     12  PI-PROCESSOR-PRINTER            PIC X(4).
000120
000121     12  PI-OE-REFERENCE-2.
000122         16  PI-OE-REF-2-PRIME           PIC X(10).
000123         16  PI-OE-REF-2-SUFF            PIC X.
000124
000125     12  PI-REM-TRM-CALC-OPTION          PIC X.
000126
000127     12  PI-LANGUAGE-TYPE                PIC X.
000128             88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
000129             88  PI-LANGUAGE-IS-FR           VALUE 'F'.
000130             88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
000131
000132     12  PI-POLICY-LINKAGE-IND           PIC X.
000133         88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
000134         88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
000135                                                   LOW-VALUES.
000136
000137     12  PI-ALT-DMD-PRT-ID               PIC X(4).
000138     12  PI-CLAIM-PW-SESSION             PIC X(1).
000139         88  PI-CLAIM-CREDIT                 VALUE '1'.
000140         88  PI-CLAIM-CONVEN                 VALUE '2'.
000141
000142     12  PI-PROCESSOR-CSR-IND            PIC X.
000143         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
000144         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
000145
000146     12  FILLER                          PIC X(3).
000147
000148     12  PI-SYSTEM-LEVEL                 PIC X(145).
000149
000150     12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
000151         PI-SYSTEM-LEVEL.
000152
000153         16  PI-ENTRY-CODES.
000154             20  PI-ENTRY-CD-1           PIC X.
000155             20  PI-ENTRY-CD-2           PIC X.
000156
000157         16  PI-RETURN-CODES.
000158             20  PI-RETURN-CD-1          PIC X.
000159             20  PI-RETURN-CD-2          PIC X.
000160
000161         16  PI-UPDATE-STATUS-SAVE.
000162             20  PI-UPDATE-BY            PIC X(4).
000163             20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
000164
000165         16  PI-LOWER-CASE-LETTERS       PIC X.
000166             88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
000167
000168*        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
000169*            88  CLAIM-NO-UNIQUE             VALUE '1'.
000170*            88  CARRIER-CLM-CNTL            VALUE '2'.
000171
000172         16  PI-CERT-ACCESS-CONTROL      PIC X.
000173             88  ST-ACCNT-CNTL               VALUE ' '.
000174             88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
000175             88  CARR-ST-ACCNT-CNTL          VALUE '2'.
000176             88  ACCNT-CNTL                  VALUE '3'.
000177             88  CARR-ACCNT-CNTL             VALUE '4'.
000178
000179         16  PI-PROCESSOR-CAP-LIST.
000180             20  PI-SYSTEM-CONTROLS.
000181                24 PI-SYSTEM-DISPLAY     PIC X.
000182                 88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
000183                24 PI-SYSTEM-MODIFY      PIC X.
000184                 88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
000185             20  FILLER                  PIC XX.
000186             20  PI-DISPLAY-CAP          PIC X.
000187                 88  DISPLAY-CAP             VALUE 'Y'.
000188             20  PI-MODIFY-CAP           PIC X.
000189                 88  MODIFY-CAP              VALUE 'Y'.
000190             20  PI-MSG-AT-LOGON-CAP     PIC X.
000191                 88  MSG-AT-LOGON-CAP        VALUE 'Y'.
000192             20  PI-FORCE-CAP            PIC X.
000193                 88  FORCE-CAP               VALUE 'Y'.
000194
000195         16  PI-PROGRAM-CONTROLS.
000196             20  PI-PGM-PRINT-OPT        PIC X.
000197             20  PI-PGM-FORMAT-OPT       PIC X.
000198             20  PI-PGM-PROCESS-OPT      PIC X.
000199             20  PI-PGM-TOTALS-OPT       PIC X.
000200
000201         16  PI-HELP-INTERFACE.
000202             20  PI-LAST-ERROR-NO        PIC X(4).
000203             20  PI-CURRENT-SCREEN-NO    PIC X(4).
000204
000205         16  PI-CARRIER-CONTROL-LEVEL    PIC X.
000206             88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
000207
000208         16  PI-CR-CONTROL-IN-PROGRESS.
000209             20  PI-CR-CARRIER           PIC X.
000210             20  PI-CR-GROUPING          PIC X(6).
000211             20  PI-CR-STATE             PIC XX.
000212             20  PI-CR-ACCOUNT           PIC X(10).
000213             20  PI-CR-FIN-RESP          PIC X(10).
000214             20  PI-CR-TYPE              PIC X.
000215
000216         16  PI-CR-BATCH-NUMBER          PIC X(6).
000217
000218         16  PI-CR-MONTH-END-DT          PIC XX.
000219
000220         16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
000221             88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
000222             88  PI-ZERO-CARRIER             VALUE '1'.
000223             88  PI-ZERO-GROUPING            VALUE '2'.
000224             88  PI-ZERO-CAR-GROUP           VALUE '3'.
000225
000226         16  PI-CARRIER-SECURITY         PIC X.
000227             88  PI-NO-CARRIER-SECURITY      VALUE ' '.
000228
000229         16  PI-ACCOUNT-SECURITY         PIC X(10).
000230             88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
000231             88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
000232
000233         16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
000234             20  PI-ACCESS-CODE          OCCURS 10 TIMES
000235                                         INDEXED BY PI-ACCESS-NDX
000236                                         PIC X.
000237
000238         16  PI-GA-BILLING-CONTROL       PIC X.
000239             88  PI-GA-BILLING               VALUE '1'.
000240
000241         16  PI-MAIL-PROCESSING          PIC X.
000242             88  PI-MAIL-YES                 VALUE 'Y'.
000243
000244         16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
000245
000246         16  PI-AR-SYSTEM.
000247             20  PI-AR-PROCESSING-CNTL   PIC X.
000248                 88  PI-AR-PROCESSING        VALUE 'Y'.
000249             20  PI-AR-SUMMARY-CODE      PIC X(6).
000250             20  PI-AR-MONTH-END-DT      PIC XX.
000251
000252         16  PI-MP-SYSTEM.
000253             20  PI-MORTGAGE-USER            PIC X.
000254                 88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
000255                 88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
000256             20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
000257                 88  PI-MP-ST-PROD-CNTL              VALUE ' '.
000258                 88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
000259                 88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
000260                 88  PI-MP-PROD-CNTL                 VALUE '3'.
000261                 88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
000262             20  PI-MP-MONTH-END-DT          PIC XX.
000263             20  PI-MP-REFERENCE-NO.
000264                 24  PI-MP-REFERENCE-PRIME   PIC X(18).
000265                 24  PI-MP-REFERENCE-SFX     PIC XX.
000266
000267         16  PI-LABEL-CONTROL            PIC X(01).
000268             88  PI-CREATE-LABELS                    VALUE 'Y'.
000269             88  PI-BYPASS-LABELS                    VALUE 'N'.
000270
000271         16  PI-BILL-GROUPING-CODE       PIC X(01).
000272             88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
000273
000274         16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
000275             88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
000276             88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
000277
000278         16  FILLER                      PIC X(14).
000279
000280     12  PI-PROGRAM-WORK-AREA            PIC X(640).
000281******************************************************************
      *<<((file: ELCINTF))
000612     12  PI-REDEF    REDEFINES PI-PROGRAM-WORK-AREA.
000613         16  FILLER                  PIC x(2).
000614         16  pi-el142-priority       pic x.
000615         16  filler                  pic x.
000616         16  PI-MAP-NAME             PIC X(8).
000617         16  PI-QUALIFICATION-SWITCHES  COMP-3.
000618             20  PI-REMINDERS-SW     PIC S9.
000619             20  PI-LETTERS-SW       PIC S9.
000620             20  PI-PAYMENTS-SW      PIC S9.
000621             20  PI-AUTO-PAY-SW      PIC S9.
000622             20  PI-NOTES-SW         PIC S9.
000623             20  PI-RES-EXP-SW       PIC S9.
000624             20  PI-DENIALS-SW       PIC S9.
000625             20  PI-INCURRED-DATE-SW PIC S9.
000626             20  PI-FORMS-SW         PIC S9.
000627         16  FILLER                  PIC X(8).
000628         16  PI-ACTIVITY-TRAILERS-KEY.
000629             20  PI-ATK-COMPANY-CODE PIC X.
000630             20  PI-ATK-CARRIER      PIC X.
000631             20  PI-ATK-CLAIM-NO     PIC X(7).
000632             20  PI-ATK-CERT-NO      PIC X(11).
000633             20  PI-ATK-SEQ-NO       PIC S9(4)     COMP.
000634         16  PI-PREV-ACTIVITY-TRAILERS-KEY.
000635             20  PI-PREV-ATK-COMPANY-CODE PIC X.
000636             20  PI-PREV-ATK-CARRIER      PIC X.
000637             20  PI-PREV-ATK-CLAIM-NO     PIC X(7).
000638             20  PI-PREV-ATK-CERT-NO      PIC X(11).
000639             20  PI-PREV-ATK-SEQ-NO       PIC S9(4)     COMP.
000640         16  PI-SAVE-KEY.
000641             20  PI-SAVE-ATK-COMPANY-CODE PIC X.
000642             20  PI-SAVE-ATK-CARRIER      PIC X.
000643             20  PI-SAVE-ATK-CLAIM-NO     PIC X(7).
000644             20  PI-SAVE-ATK-CERT-NO      PIC X(11).
000645             20  PI-SAVE-ATK-SEQ-NO       PIC S9(4)     COMP.
000646         16  PI-PREV-AID                  PIC X.
000647         16  PI-RECORD-COUNT              PIC S9        COMP-3.
000648         16  PI-END-OF-FILE               PIC S9        COMP-3.
000649         16  PI-SAVE-CURSOR               PIC S9(4)     COMP.
000650         16  PI-PURGED-SW                 PIC X.
000651             88  CLAIM-IS-PURGED                 VALUE 'Y'.
000652         16  PI-LINE-NO                   PIC 9.
000653         16  PI-PREV-SEQ-NO               PIC S9(4)   COMP.
000654         16  PI-FIRST-TIME-SW             PIC X.
000655             88  FIRST-TIME                      VALUE 'Y'.
000656         16  PI-PREV-DIRECTION            PIC X.
000657         16  PI-SEQ-NUMBERS.
000658             20  PI-SEQ-NO-TABLE OCCURS 9 TIMES.
000659                 24  PI-TRLR-LN-NO        PIC 9.
000660                 24  PI-TRLR-SEQ-NO       PIC S9(4)   COMP.
000661                 24  PI-TRLR-TYPE         PIC X.
000662         16  PI-PAY-TYPE                  PIC X.
000663         16  pi-save-type                 pic x.
000664         16  PI-FULL-SCREEN-IND           PIC X.
000665             88  PI-FULL-SCREEN-SHOWN            VALUE 'Y'.
000666         16  pi-approval-level            pic x.
000667         16  FILLER                       PIC X(494).
000668
000669     EJECT
000670*                                    COPY ELCAID.
      *>>((file: ELCAID))
000001******************************************************************
000002*                                                                *
000003*                            ELCAID.                             *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
000007*                                                                *
000008*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
000009*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
000010******************************************************************
000011
000012 01  DFHAID.
000013   02  DFHNULL   PIC  X  VALUE  ' '.
000014   02  DFHENTER  PIC  X  VALUE  QUOTE.
000015   02  DFHCLEAR  PIC  X  VALUE  '_'.
000016   02  DFHPEN    PIC  X  VALUE  '='.
000017   02  DFHOPID   PIC  X  VALUE  'W'.
000018   02  DFHPA1    PIC  X  VALUE  '%'.
000019   02  DFHPA2    PIC  X  VALUE  '>'.
000020   02  DFHPA3    PIC  X  VALUE  ','.
000021   02  DFHPF1    PIC  X  VALUE  '1'.
000022   02  DFHPF2    PIC  X  VALUE  '2'.
000023   02  DFHPF3    PIC  X  VALUE  '3'.
000024   02  DFHPF4    PIC  X  VALUE  '4'.
000025   02  DFHPF5    PIC  X  VALUE  '5'.
000026   02  DFHPF6    PIC  X  VALUE  '6'.
000027   02  DFHPF7    PIC  X  VALUE  '7'.
000028   02  DFHPF8    PIC  X  VALUE  '8'.
000029   02  DFHPF9    PIC  X  VALUE  '9'.
000030   02  DFHPF10   PIC  X  VALUE  ':'.
000031   02  DFHPF11   PIC  X  VALUE  '#'.
000032   02  DFHPF12   PIC  X  VALUE  '@'.
000033   02  DFHPF13   PIC  X  VALUE  'A'.
000034   02  DFHPF14   PIC  X  VALUE  'B'.
000035   02  DFHPF15   PIC  X  VALUE  'C'.
000036   02  DFHPF16   PIC  X  VALUE  'D'.
000037   02  DFHPF17   PIC  X  VALUE  'E'.
000038   02  DFHPF18   PIC  X  VALUE  'F'.
000039   02  DFHPF19   PIC  X  VALUE  'G'.
000040   02  DFHPF20   PIC  X  VALUE  'H'.
000041   02  DFHPF21   PIC  X  VALUE  'I'.
000042*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
000043   02  DFHPF22   PIC  X  VALUE  '['.
000044   02  DFHPF23   PIC  X  VALUE  '.'.
000045   02  DFHPF24   PIC  X  VALUE  '<'.
000046   02  DFHMSRE   PIC  X  VALUE  'X'.
000047   02  DFHSTRF   PIC  X  VALUE  'h'.
000048   02  DFHTRIG   PIC  X  VALUE  '"'.
      *<<((file: ELCAID))
000671 01  FILLER    REDEFINES DFHAID.
000672     12  FILLER                      PIC X(8).
000673     12  PF-VALUES                   PIC X       OCCURS 24 TIMES.
000674     EJECT
000675*                                    COPY EL1501S.
      *>>((file: EL1501S))
000001 01  EL150BI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  RUNDTEL PIC S9(0004) COMP.
000005     05  RUNDTEF PIC  X(0001).
000006     05  FILLER REDEFINES RUNDTEF.
000007         10  RUNDTEA PIC  X(0001).
000008     05  RUNDTEI PIC  X(0008).
000009*    -------------------------------
000010     05  RUNTIMEL PIC S9(0004) COMP.
000011     05  RUNTIMEF PIC  X(0001).
000012     05  FILLER REDEFINES RUNTIMEF.
000013         10  RUNTIMEA PIC  X(0001).
000014     05  RUNTIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  CLMNOL PIC S9(0004) COMP.
000017     05  CLMNOF PIC  X(0001).
000018     05  FILLER REDEFINES CLMNOF.
000019         10  CLMNOA PIC  X(0001).
000020     05  CLMNOI PIC  X(0007).
000021*    -------------------------------
000022     05  CARRL PIC S9(0004) COMP.
000023     05  CARRF PIC  X(0001).
000024     05  FILLER REDEFINES CARRF.
000025         10  CARRA PIC  X(0001).
000026     05  CARRI PIC  X(0001).
000027*    -------------------------------
000028     05  CERTNOL PIC S9(0004) COMP.
000029     05  CERTNOF PIC  X(0001).
000030     05  FILLER REDEFINES CERTNOF.
000031         10  CERTNOA PIC  X(0001).
000032     05  CERTNOI PIC  X(0010).
000033*    -------------------------------
000034     05  SUFXL PIC S9(0004) COMP.
000035     05  SUFXF PIC  X(0001).
000036     05  FILLER REDEFINES SUFXF.
000037         10  SUFXA PIC  X(0001).
000038     05  SUFXI PIC  X(0001).
000039*    -------------------------------
000040     05  LN1HDGL PIC S9(0004) COMP.
000041     05  LN1HDGF PIC  X(0001).
000042     05  FILLER REDEFINES LN1HDGF.
000043         10  LN1HDGA PIC  X(0001).
000044     05  LN1HDGI PIC  X(0010).
000045*    -------------------------------
000046     05  LN1TXTL PIC S9(0004) COMP.
000047     05  LN1TXTF PIC  X(0001).
000048     05  FILLER REDEFINES LN1TXTF.
000049         10  LN1TXTA PIC  X(0001).
000050     05  LN1TXTI PIC  X(0068).
000051*    -------------------------------
000052     05  LN2HDGL PIC S9(0004) COMP.
000053     05  LN2HDGF PIC  X(0001).
000054     05  FILLER REDEFINES LN2HDGF.
000055         10  LN2HDGA PIC  X(0001).
000056     05  LN2HDGI PIC  X(0010).
000057*    -------------------------------
000058     05  LN2TXTL PIC S9(0004) COMP.
000059     05  LN2TXTF PIC  X(0001).
000060     05  FILLER REDEFINES LN2TXTF.
000061         10  LN2TXTA PIC  X(0001).
000062     05  LN2TXTI PIC  X(0068).
000063*    -------------------------------
000064     05  LN3HDGL PIC S9(0004) COMP.
000065     05  LN3HDGF PIC  X(0001).
000066     05  FILLER REDEFINES LN3HDGF.
000067         10  LN3HDGA PIC  X(0001).
000068     05  LN3HDGI PIC  X(0010).
000069*    -------------------------------
000070     05  LN3TXTL PIC S9(0004) COMP.
000071     05  LN3TXTF PIC  X(0001).
000072     05  FILLER REDEFINES LN3TXTF.
000073         10  LN3TXTA PIC  X(0001).
000074     05  LN3TXTI PIC  X(0068).
000075*    -------------------------------
000076     05  LN4HDGL PIC S9(0004) COMP.
000077     05  LN4HDGF PIC  X(0001).
000078     05  FILLER REDEFINES LN4HDGF.
000079         10  LN4HDGA PIC  X(0001).
000080     05  LN4HDGI PIC  X(0010).
000081*    -------------------------------
000082     05  LN4TXTL PIC S9(0004) COMP.
000083     05  LN4TXTF PIC  X(0001).
000084     05  FILLER REDEFINES LN4TXTF.
000085         10  LN4TXTA PIC  X(0001).
000086     05  LN4TXTI PIC  X(0068).
000087*    -------------------------------
000088     05  LN5HDGL PIC S9(0004) COMP.
000089     05  LN5HDGF PIC  X(0001).
000090     05  FILLER REDEFINES LN5HDGF.
000091         10  LN5HDGA PIC  X(0001).
000092     05  LN5HDGI PIC  X(0010).
000093*    -------------------------------
000094     05  LN5TXTL PIC S9(0004) COMP.
000095     05  LN5TXTF PIC  X(0001).
000096     05  FILLER REDEFINES LN5TXTF.
000097         10  LN5TXTA PIC  X(0001).
000098     05  LN5TXTI PIC  X(0068).
000099*    -------------------------------
000100     05  LN6HDGL PIC S9(0004) COMP.
000101     05  LN6HDGF PIC  X(0001).
000102     05  FILLER REDEFINES LN6HDGF.
000103         10  LN6HDGA PIC  X(0001).
000104     05  LN6HDGI PIC  X(0010).
000105*    -------------------------------
000106     05  LN6TXTL PIC S9(0004) COMP.
000107     05  LN6TXTF PIC  X(0001).
000108     05  FILLER REDEFINES LN6TXTF.
000109         10  LN6TXTA PIC  X(0001).
000110     05  LN6TXTI PIC  X(0068).
000111*    -------------------------------
000112     05  LN7HDGL PIC S9(0004) COMP.
000113     05  LN7HDGF PIC  X(0001).
000114     05  FILLER REDEFINES LN7HDGF.
000115         10  LN7HDGA PIC  X(0001).
000116     05  LN7HDGI PIC  X(0010).
000117*    -------------------------------
000118     05  LN7TXTL PIC S9(0004) COMP.
000119     05  LN7TXTF PIC  X(0001).
000120     05  FILLER REDEFINES LN7TXTF.
000121         10  LN7TXTA PIC  X(0001).
000122     05  LN7TXTI PIC  X(0068).
000123*    -------------------------------
000124     05  LN8HDGL PIC S9(0004) COMP.
000125     05  LN8HDGF PIC  X(0001).
000126     05  FILLER REDEFINES LN8HDGF.
000127         10  LN8HDGA PIC  X(0001).
000128     05  LN8HDGI PIC  X(0010).
000129*    -------------------------------
000130     05  LN8TXTL PIC S9(0004) COMP.
000131     05  LN8TXTF PIC  X(0001).
000132     05  FILLER REDEFINES LN8TXTF.
000133         10  LN8TXTA PIC  X(0001).
000134     05  LN8TXTI PIC  X(0068).
000135*    -------------------------------
000136     05  LN9HDGL PIC S9(0004) COMP.
000137     05  LN9HDGF PIC  X(0001).
000138     05  FILLER REDEFINES LN9HDGF.
000139         10  LN9HDGA PIC  X(0001).
000140     05  LN9HDGI PIC  X(0010).
000141*    -------------------------------
000142     05  LN9TXTL PIC S9(0004) COMP.
000143     05  LN9TXTF PIC  X(0001).
000144     05  FILLER REDEFINES LN9TXTF.
000145         10  LN9TXTA PIC  X(0001).
000146     05  LN9TXTI PIC  X(0068).
000147*    -------------------------------
000148     05  LN10HDGL PIC S9(0004) COMP.
000149     05  LN10HDGF PIC  X(0001).
000150     05  FILLER REDEFINES LN10HDGF.
000151         10  LN10HDGA PIC  X(0001).
000152     05  LN10HDGI PIC  X(0010).
000153*    -------------------------------
000154     05  LN10TXTL PIC S9(0004) COMP.
000155     05  LN10TXTF PIC  X(0001).
000156     05  FILLER REDEFINES LN10TXTF.
000157         10  LN10TXTA PIC  X(0001).
000158     05  LN10TXTI PIC  X(0068).
000159*    -------------------------------
000160     05  LN11HDGL PIC S9(0004) COMP.
000161     05  LN11HDGF PIC  X(0001).
000162     05  FILLER REDEFINES LN11HDGF.
000163         10  LN11HDGA PIC  X(0001).
000164     05  LN11HDGI PIC  X(0010).
000165*    -------------------------------
000166     05  LN11TXTL PIC S9(0004) COMP.
000167     05  LN11TXTF PIC  X(0001).
000168     05  FILLER REDEFINES LN11TXTF.
000169         10  LN11TXTA PIC  X(0001).
000170     05  LN11TXTI PIC  X(0068).
000171*    -------------------------------
000172     05  LN12HDGL PIC S9(0004) COMP.
000173     05  LN12HDGF PIC  X(0001).
000174     05  FILLER REDEFINES LN12HDGF.
000175         10  LN12HDGA PIC  X(0001).
000176     05  LN12HDGI PIC  X(0010).
000177*    -------------------------------
000178     05  LN12TXTL PIC S9(0004) COMP.
000179     05  LN12TXTF PIC  X(0001).
000180     05  FILLER REDEFINES LN12TXTF.
000181         10  LN12TXTA PIC  X(0001).
000182     05  LN12TXTI PIC  X(0068).
000183*    -------------------------------
000184     05  LN13HDGL PIC S9(0004) COMP.
000185     05  LN13HDGF PIC  X(0001).
000186     05  FILLER REDEFINES LN13HDGF.
000187         10  LN13HDGA PIC  X(0001).
000188     05  LN13HDGI PIC  X(0010).
000189*    -------------------------------
000190     05  LN13TXTL PIC S9(0004) COMP.
000191     05  LN13TXTF PIC  X(0001).
000192     05  FILLER REDEFINES LN13TXTF.
000193         10  LN13TXTA PIC  X(0001).
000194     05  LN13TXTI PIC  X(0068).
000195*    -------------------------------
000196     05  LN14HDGL PIC S9(0004) COMP.
000197     05  LN14HDGF PIC  X(0001).
000198     05  FILLER REDEFINES LN14HDGF.
000199         10  LN14HDGA PIC  X(0001).
000200     05  LN14HDGI PIC  X(0010).
000201*    -------------------------------
000202     05  LN14TXTL PIC S9(0004) COMP.
000203     05  LN14TXTF PIC  X(0001).
000204     05  FILLER REDEFINES LN14TXTF.
000205         10  LN14TXTA PIC  X(0001).
000206     05  LN14TXTI PIC  X(0068).
000207*    -------------------------------
000208     05  LN15HDGL PIC S9(0004) COMP.
000209     05  LN15HDGF PIC  X(0001).
000210     05  FILLER REDEFINES LN15HDGF.
000211         10  LN15HDGA PIC  X(0001).
000212     05  LN15HDGI PIC  X(0010).
000213*    -------------------------------
000214     05  LN15TXTL PIC S9(0004) COMP.
000215     05  LN15TXTF PIC  X(0001).
000216     05  FILLER REDEFINES LN15TXTF.
000217         10  LN15TXTA PIC  X(0001).
000218     05  LN15TXTI PIC  X(0068).
000219*    -------------------------------
000220     05  LN16HDGL PIC S9(0004) COMP.
000221     05  LN16HDGF PIC  X(0001).
000222     05  FILLER REDEFINES LN16HDGF.
000223         10  LN16HDGA PIC  X(0001).
000224     05  LN16HDGI PIC  X(0010).
000225*    -------------------------------
000226     05  TRLR8BL PIC S9(0004) COMP.
000227     05  TRLR8BF PIC  X(0001).
000228     05  FILLER REDEFINES TRLR8BF.
000229         10  TRLR8BA PIC  X(0001).
000230     05  TRLR8BI PIC  X(0068).
000231*    -------------------------------
000232     05  ERRMSG1L PIC S9(0004) COMP.
000233     05  ERRMSG1F PIC  X(0001).
000234     05  FILLER REDEFINES ERRMSG1F.
000235         10  ERRMSG1A PIC  X(0001).
000236     05  ERRMSG1I PIC  X(0079).
000237*    -------------------------------
000238     05  LINENOL PIC S9(0004) COMP.
000239     05  LINENOF PIC  X(0001).
000240     05  FILLER REDEFINES LINENOF.
000241         10  LINENOA PIC  X(0001).
000242     05  LINENOI PIC  X(0001).
000243*    -------------------------------
000244     05  RECVDTL PIC S9(0004) COMP.
000245     05  RECVDTF PIC  X(0001).
000246     05  FILLER REDEFINES RECVDTF.
000247         10  RECVDTA PIC  X(0001).
000248     05  RECVDTI PIC  X(0008).
000249*    -------------------------------
000250     05  RECVTYPL PIC S9(0004) COMP.
000251     05  RECVTYPF PIC  X(0001).
000252     05  FILLER REDEFINES RECVTYPF.
000253         10  RECVTYPA PIC  X(0001).
000254     05  RECVTYPI PIC  X(0001).
000255*    -------------------------------
000256     05  ENTERPFL PIC S9(0004) COMP.
000257     05  ENTERPFF PIC  X(0001).
000258     05  FILLER REDEFINES ENTERPFF.
000259         10  ENTERPFA PIC  X(0001).
000260     05  ENTERPFI PIC  99.
000261*    -------------------------------
000262     05  PF5L PIC S9(0004) COMP.
000263     05  PF5F PIC  X(0001).
000264     05  FILLER REDEFINES PF5F.
000265         10  PF5A PIC  X(0001).
000266     05  PF5I PIC  X(0016).
000267*    -------------------------------
000268     05  PF6L PIC S9(0004) COMP.
000269     05  PF6F PIC  X(0001).
000270     05  FILLER REDEFINES PF6F.
000271         10  PF6A PIC  X(0001).
000272     05  PF6I PIC  X(0016).
000273 01  EL150BO REDEFINES EL150BI.
000274     05  FILLER            PIC  X(0012).
000275*    -------------------------------
000276     05  FILLER            PIC  X(0003).
000277     05  RUNDTEO PIC  X(0008).
000278*    -------------------------------
000279     05  FILLER            PIC  X(0003).
000280     05  RUNTIMEO PIC  99.99.
000281*    -------------------------------
000282     05  FILLER            PIC  X(0003).
000283     05  CLMNOO PIC  X(0007).
000284*    -------------------------------
000285     05  FILLER            PIC  X(0003).
000286     05  CARRO PIC  X(0001).
000287*    -------------------------------
000288     05  FILLER            PIC  X(0003).
000289     05  CERTNOO PIC  X(0010).
000290*    -------------------------------
000291     05  FILLER            PIC  X(0003).
000292     05  SUFXO PIC  X(0001).
000293*    -------------------------------
000294     05  FILLER            PIC  X(0003).
000295     05  LN1HDGO PIC  X(0010).
000296*    -------------------------------
000297     05  FILLER            PIC  X(0003).
000298     05  LN1TXTO PIC  X(0068).
000299*    -------------------------------
000300     05  FILLER            PIC  X(0003).
000301     05  LN2HDGO PIC  X(0010).
000302*    -------------------------------
000303     05  FILLER            PIC  X(0003).
000304     05  LN2TXTO PIC  X(0068).
000305*    -------------------------------
000306     05  FILLER            PIC  X(0003).
000307     05  LN3HDGO PIC  X(0010).
000308*    -------------------------------
000309     05  FILLER            PIC  X(0003).
000310     05  LN3TXTO PIC  X(0068).
000311*    -------------------------------
000312     05  FILLER            PIC  X(0003).
000313     05  LN4HDGO PIC  X(0010).
000314*    -------------------------------
000315     05  FILLER            PIC  X(0003).
000316     05  LN4TXTO PIC  X(0068).
000317*    -------------------------------
000318     05  FILLER            PIC  X(0003).
000319     05  LN5HDGO PIC  X(0010).
000320*    -------------------------------
000321     05  FILLER            PIC  X(0003).
000322     05  LN5TXTO PIC  X(0068).
000323*    -------------------------------
000324     05  FILLER            PIC  X(0003).
000325     05  LN6HDGO PIC  X(0010).
000326*    -------------------------------
000327     05  FILLER            PIC  X(0003).
000328     05  LN6TXTO PIC  X(0068).
000329*    -------------------------------
000330     05  FILLER            PIC  X(0003).
000331     05  LN7HDGO PIC  X(0010).
000332*    -------------------------------
000333     05  FILLER            PIC  X(0003).
000334     05  LN7TXTO PIC  X(0068).
000335*    -------------------------------
000336     05  FILLER            PIC  X(0003).
000337     05  LN8HDGO PIC  X(0010).
000338*    -------------------------------
000339     05  FILLER            PIC  X(0003).
000340     05  LN8TXTO PIC  X(0068).
000341*    -------------------------------
000342     05  FILLER            PIC  X(0003).
000343     05  LN9HDGO PIC  X(0010).
000344*    -------------------------------
000345     05  FILLER            PIC  X(0003).
000346     05  LN9TXTO PIC  X(0068).
000347*    -------------------------------
000348     05  FILLER            PIC  X(0003).
000349     05  LN10HDGO PIC  X(0010).
000350*    -------------------------------
000351     05  FILLER            PIC  X(0003).
000352     05  LN10TXTO PIC  X(0068).
000353*    -------------------------------
000354     05  FILLER            PIC  X(0003).
000355     05  LN11HDGO PIC  X(0010).
000356*    -------------------------------
000357     05  FILLER            PIC  X(0003).
000358     05  LN11TXTO PIC  X(0068).
000359*    -------------------------------
000360     05  FILLER            PIC  X(0003).
000361     05  LN12HDGO PIC  X(0010).
000362*    -------------------------------
000363     05  FILLER            PIC  X(0003).
000364     05  LN12TXTO PIC  X(0068).
000365*    -------------------------------
000366     05  FILLER            PIC  X(0003).
000367     05  LN13HDGO PIC  X(0010).
000368*    -------------------------------
000369     05  FILLER            PIC  X(0003).
000370     05  LN13TXTO PIC  X(0068).
000371*    -------------------------------
000372     05  FILLER            PIC  X(0003).
000373     05  LN14HDGO PIC  X(0010).
000374*    -------------------------------
000375     05  FILLER            PIC  X(0003).
000376     05  LN14TXTO PIC  X(0068).
000377*    -------------------------------
000378     05  FILLER            PIC  X(0003).
000379     05  LN15HDGO PIC  X(0010).
000380*    -------------------------------
000381     05  FILLER            PIC  X(0003).
000382     05  LN15TXTO PIC  X(0068).
000383*    -------------------------------
000384     05  FILLER            PIC  X(0003).
000385     05  LN16HDGO PIC  X(0010).
000386*    -------------------------------
000387     05  FILLER            PIC  X(0003).
000388     05  TRLR8BO PIC  X(0068).
000389*    -------------------------------
000390     05  FILLER            PIC  X(0003).
000391     05  ERRMSG1O PIC  X(0079).
000392*    -------------------------------
000393     05  FILLER            PIC  X(0003).
000394     05  LINENOO PIC  X(0001).
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  RECVDTO PIC  X(0008).
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  RECVTYPO PIC  X(0001).
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  ENTERPFO PIC  X(0002).
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  PF5O PIC  X(0016).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  PF6O PIC  X(0016).
000410*    -------------------------------
      *<<((file: EL1501S))
000676 01  EL150BI-R REDEFINES EL150BI.
000677     12  FILLER                      PIC X(62).
000678     12  EL150BI-OCCURS OCCURS 16.
000679         16  MAP-HDG-LENGTH          PIC S9(4)   COMP.
000680         16  MAP-HDG-ATTRB           PIC X.
000681         16  MAP-HDG.
000682             20  MAP-LINE-NO         PIC X.
000683             20  FILLER              PIC X.
000684             20  MAP-HDG-LIT         PIC X(8).
000685         16  MAP-TEXT-LENGTH         PIC S9(4)   COMP.
000686         16  MAP-TEXT-ATTRB          PIC X.
000687         16  MAP-TEXT                PIC X(68).
000688
000689     EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        
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
      * Copyright (c) 2016-2020 NTT DATA, Inc.                        *
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
000691 01  DFHCOMMAREA                     PIC X(1024).
000692
000693*                                    COPY ELCMSTR.
      *>>((file: ELCMSTR))
000001******************************************************************
000002*                                                                *
000003*                            ELCMSTR.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.012                          *
000006*                                                                *
000007*   FILE DESCRIPTION = CLAIM MASTER FILE                         *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 350  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
000013*       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
000014*       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
000015*       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
000016*       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
000017*                                                 RKP=75,LEN=21  *
000018*                                                                *
000019*   **** NOTE ****                                               *
000020*             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
000021*             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
000022*                                                                *
000023*   LOG = YES                                                    *
000024*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000025******************************************************************
000026*                   C H A N G E   L O G
000027*
000028* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000029*-----------------------------------------------------------------
000030*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000031* EFFECTIVE    NUMBER
000032*-----------------------------------------------------------------
000033* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
000034* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
000035* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
000036* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000037* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000038* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
000039* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000040******************************************************************
000041 01  CLAIM-MASTER.
000042     12  CL-RECORD-ID                PIC XX.
000043         88  VALID-CL-ID         VALUE 'CL'.
000044
000045     12  CL-CONTROL-PRIMARY.
000046         16  CL-COMPANY-CD           PIC X.
000047         16  CL-CARRIER              PIC X.
000048         16  CL-CLAIM-NO             PIC X(7).
000049         16  CL-CERT-NO.
000050             20  CL-CERT-PRIME       PIC X(10).
000051             20  CL-CERT-SFX         PIC X.
000052
000053     12  CL-CONTROL-BY-NAME.
000054         16  CL-COMPANY-CD-A1        PIC X.
000055         16  CL-INSURED-LAST-NAME    PIC X(15).
000056         16  CL-INSURED-NAME.
000057             20  CL-INSURED-1ST-NAME PIC X(12).
000058             20  CL-INSURED-MID-INIT PIC X.
000059
000060     12  CL-CONTROL-BY-SSN.
000061         16  CL-COMPANY-CD-A2        PIC X.
000062         16  CL-SOC-SEC-NO.
000063             20  CL-SSN-STATE        PIC XX.
000064             20  CL-SSN-ACCOUNT      PIC X(6).
000065             20  CL-SSN-LN3          PIC X(3).
000066
000067     12  CL-CONTROL-BY-CERT-NO.
000068         16  CL-COMPANY-CD-A4        PIC X.
000069         16  CL-CERT-NO-A4.
000070             20  CL-CERT-A4-PRIME    PIC X(10).
000071             20  CL-CERT-A4-SFX      PIC X.
000072
000073     12  CL-CONTROL-BY-CCN.
000074         16  CL-COMPANY-CD-A5        PIC X.
000075         16  CL-CCN-A5.
000076             20  CL-CCN.
000077                 24  CL-CCN-PREFIX-A5 PIC X(4).
000078                 24  CL-CCN-PRIME-A5 PIC X(12).
000079             20  CL-CCN-FILLER-A5    PIC X(4).
000080
000081     12  CL-INSURED-PROFILE-DATA.
000082         16  CL-INSURED-BIRTH-DT     PIC XX.
000083         16  CL-INSURED-SEX-CD       PIC X.
000084             88  INSURED-IS-MALE        VALUE 'M'.
000085             88  INSURED-IS-FEMALE      VALUE 'F'.
000086             88  INSURED-SEX-UNKNOWN    VALUE ' '.
000087         16  CL-INSURED-OCC-CD       PIC X(6).
000088         16  FILLER                  PIC X(5).
000089
000090     12  CL-PROCESSING-INFO.
000091         16  CL-PROCESSOR-ID         PIC X(4).
000092         16  CL-CLAIM-STATUS         PIC X.
000093             88  CLAIM-IS-OPEN          VALUE 'O'.
000094             88  CLAIM-IS-CLOSED        VALUE 'C'.
000095         16  CL-CLAIM-TYPE           PIC X.
000096*            88  AH-CLAIM               VALUE 'A'.
000097*            88  LIFE-CLAIM             VALUE 'L'.
000098*            88  PROPERTY-CLAIM         VALUE 'P'.
000099*            88  IUI-CLAIM              VALUE 'I'.
000100*            88  GAP-CLAIM              VALUE 'G'.
000101*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
000102*            88  OTHER-CLAIM            VALUE 'O'.
000103         16  CL-CLAIM-PREM-TYPE      PIC X.
000104             88  SINGLE-PREMIUM         VALUE '1'.
000105             88  O-B-COVERAGE           VALUE '2'.
000106             88  OPEN-END-COVERAGE      VALUE '3'.
000107         16  CL-INCURRED-DT          PIC XX.
000108         16  CL-REPORTED-DT          PIC XX.
000109         16  CL-FILE-ESTABLISH-DT    PIC XX.
000110         16  CL-EST-END-OF-DISAB-DT  PIC XX.
000111         16  CL-LAST-PMT-DT          PIC XX.
000112         16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
000113         16  CL-PAID-THRU-DT         PIC XX.
000114         16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
000115         16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
000116         16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
000117         16  CL-PMT-CALC-METHOD      PIC X.
000118             88  CL-360-DAY-YR          VALUE '1'.
000119             88  CL-365-DAY-YR          VALUE '2'.
000120             88  CL-FULL-MONTHS         VALUE '3'.
000121         16  CL-CAUSE-CD             PIC X(6).
000122
000123         16  CL-PRIME-CERT-NO.
000124             20  CL-PRIME-CERT-PRIME PIC X(10).
000125             20  CL-PRIME-CERT-SFX   PIC X.
000126
000127         16  CL-SYSTEM-IDENTIFIER    PIC XX.
000128             88  CL-CREDIT-CLAIM        VALUE 'CR'.
000129             88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
000130
000131         16  CL-MICROFILM-NO         PIC X(10).
000132         16  FILLER REDEFINES CL-MICROFILM-NO.
000133             20  CL-BENEFIT-PERIOD   PIC 99.
000134             20  FILLER              PIC X(8).
000135         16  CL-PROG-FORM-TYPE       PIC X.
000136         16  CL-LAST-ADD-ON-DT       PIC XX.
000137
000138         16  CL-LAST-REOPEN-DT       PIC XX.
000139         16  CL-LAST-CLOSE-DT        PIC XX.
000140         16  CL-LAST-CLOSE-REASON    PIC X(01).
000141             88  FINAL-PAID             VALUE '1'.
000142             88  CLAIM-DENIED           VALUE '2'.
000143             88  AUTO-CLOSE             VALUE '3'.
000144             88  MANUAL-CLOSE           VALUE '4'.
000145             88  BENEFITS-CHANGED       VALUE 'C'.
000146             88  SETUP-ERRORS           VALUE 'E'.
000147         16  CL-ASSOC-CERT-SEQU      PIC S99.
000148         16  CL-ASSOC-CERT-TOTAL     PIC S99.
000149         16  CL-CLAIM-PAYMENT-STATUS PIC 9.
000150             88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
000151         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
000152         16  FILLER                  PIC X.
000153
000154     12  CL-CERTIFICATE-DATA.
000155         16  CL-CERT-ORIGIN          PIC X.
000156             88  CERT-WAS-ONLINE        VALUE '1'.
000157             88  CERT-WAS-CREATED       VALUE '2'.
000158             88  COVERAGE-WAS-ADDED     VALUE '3'.
000159         16  CL-CERT-KEY-DATA.
000160             20  CL-CERT-CARRIER     PIC X.
000161             20  CL-CERT-GROUPING    PIC X(6).
000162             20  CL-CERT-STATE       PIC XX.
000163             20  CL-CERT-ACCOUNT.
000164                 24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
000165                 24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
000166             20  CL-CERT-EFF-DT      PIC XX.
000167
000168     12  CL-STATUS-CONTROLS.
000169         16  CL-PRIORITY-CD          PIC X.
000170             88  CONFIDENTIAL-DATA      VALUE '8'.
000171             88  HIGHEST-PRIORITY       VALUE '9'.
000172         16  CL-SUPV-ATTN-CD         PIC X.
000173             88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
000174             88  SUPV-IS-REQUIRED       VALUE 'Y'.
000175         16  CL-PURGED-DT            PIC XX.
000176         16  CL-RESTORED-DT          PIC XX.
000177         16  CL-NEXT-AUTO-PAY-DT     PIC XX.
000178         16  CL-NEXT-RESEND-DT       PIC XX.
000179         16  CL-NEXT-FOLLOWUP-DT     PIC XX.
000180         16  CL-CRITICAL-PERIOD      PIC 99.
000181*        16  FILLER                  PIC XX.
000182         16  CL-LAST-MAINT-DT        PIC XX.
000183         16  CL-LAST-MAINT-USER      PIC X(4).
000184         16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
000185         16  CL-LAST-MAINT-TYPE      PIC X.
000186             88  CLAIM-SET-UP           VALUE ' '.
000187             88  PAYMENT-MADE           VALUE '1'.
000188             88  LETTER-SENT            VALUE '2'.
000189             88  MASTER-WAS-ALTERED     VALUE '3'.
000190             88  MASTER-WAS-RESTORED    VALUE '4'.
000191             88  INCURRED-DATE-CHANGED  VALUE '5'.
000192             88  FILE-CONVERTED         VALUE '6'.
000193             88  CHANGE-OF-BENEFITS     VALUE 'C'.
000194             88  ERROR-CORRECTION       VALUE 'E'.
000195         16  CL-RELATED-CLAIM-NO     PIC X(7).
000196         16  CL-HISTORY-ARCHIVE-DT   PIC XX.
000197         16  CL-BENEFICIARY          PIC X(10).
000198         16  CL-FILE-ESTABLISHED-BY  PIC X(4).
000199         16  CL-DENIAL-TYPE          PIC X.
000200             88  CL-TYPE-DENIAL          VALUE '1'.
000201             88  CL-TYPE-RESCISSION      VALUE '2'.
000202             88  CL-TYPE-REFORMATION     VALUE '3'.
000203             88  CL-TYPE-REF-TO-RES      VALUE '4'.
000204             88  CL-TYPE-RECONSIDERED    VALUE '5'.
000205         16  CL-NO-OF-EXTENSIONS     PIC 99.
000206
000207         16  filler                  pic x(3).
000208*        16  CL-CRIT-PER-RECURRENT   PIC X.
000209*        16  CL-CRIT-PER-RTW-MOS     PIC 99.
000210*        16  CL-RTW-DT               PIC XX.
000211
000212     12  CL-TRAILER-CONTROLS.
000213         16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
000214             88  CL-1ST-TRL-AVAIL       VALUE +4095.
000215             88  CL-LAST-TRL-AVAIL      VALUE +100.
000216             88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
000217         16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
000218         16  FILLER                  PIC XX.
000219         16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
000220         16  CL-ADDRESS-TRAILER-CNT.
000221             20  CL-INSURED-ADDR-CNT  PIC S9(1).
000222                 88  NO-INSURED-AVAILABLE    VALUE ZERO.
000223             20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
000224                 88  ACCOUNT-IS-ONLINE       VALUE ZERO.
000225             20  CL-BENIF-ADDR-CNT    PIC S9(1).
000226                 88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
000227             20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
000228                 88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
000229             20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
000230                 88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
000231             20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
000232                 88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
000233             20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
000234                 88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
000235
000236     12  CL-CV-REFERENCE-NO.
000237         16  CL-CV-REFNO-PRIME       PIC X(18).
000238         16  CL-CV-REFNO-SFX         PIC XX.
000239
000240     12  CL-FILE-LOCATION            PIC X(4).
000241
000242     12  CL-PROCESS-ERRORS.
000243         16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
000244             88  NO-FATAL-ERRORS        VALUE ZERO.
000245         16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
000246             88  NO-FORCABLE-ERRORS     VALUE ZERO.
000247
000248     12  CL-PRODUCT-CD               PIC X.
000249
000250     12  CL-CURRENT-KEY-DATA.
000251         16  CL-CURRENT-CARRIER      PIC X.
000252         16  CL-CURRENT-GROUPING     PIC X(6).
000253         16  CL-CURRENT-STATE        PIC XX.
000254         16  CL-CURRENT-ACCOUNT      PIC X(10).
000255
000256     12  CL-ASSOCIATES               PIC X.
000257         88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
000258         88  CL-ASSOC-INTERFACE         VALUE 'I'.
000259         88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
000260         88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
000261
000262     12  CL-ACTIVITY-CODE            PIC 99.
000263     12  CL-ACTIVITY-MAINT-DT        PIC XX.
000264     12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
000265
000266     12  CL-LAPSE-REPORT-CODE        PIC 9.
000267     12  CL-LAG-REPORT-CODE          PIC 9.
000268     12  CL-LOAN-TYPE                PIC XX.
000269     12  CL-LEGAL-STATE              PIC XX.
000270
000271     12  CL-YESNOSW                  PIC X.
000272     12  CL-ACCIDENT-CLAIM-SW        PIC X.
000273         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
000274         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
000275         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
000276     12  cl-insured-type             pic x.
000277         88  cl-claim-on-primary         value 'P'.
000278         88  cl-claim-on-co-borrower     value 'C'.
000279     12  cl-benefit-expiration-dt    PIC XX.
      *<<((file: ELCMSTR))
000694     EJECT
000695*                                    COPY ELCCNTL.
      *>>((file: ELCCNTL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCCNTL.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.059                          *
000007*                                                                *
000008*   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 750  RECFORM = FIXED                           *
000012*                                                                *
000013*   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
000014*       ALTERNATE INDEX = NONE                                   *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019*                   C H A N G E   L O G
000020*
000021* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000022*-----------------------------------------------------------------
000023*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000024* EFFECTIVE    NUMBER
000025*-----------------------------------------------------------------
000026* 082503                   PEMA  ADD BENEFIT GROUP
000027* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000028* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
000029* 092705    2005050300006  PEMA  ADD SPP LEASES
000030* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
000031* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
000032* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
000033* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
000034* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
000035* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
000036* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
000037* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
000038* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
000039* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000040* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
000041******************************************************************
000042*
000043 01  CONTROL-FILE.
000044     12  CF-RECORD-ID                       PIC XX.
000045         88  VALID-CF-ID                        VALUE 'CF'.
000046
000047     12  CF-CONTROL-PRIMARY.
000048         16  CF-COMPANY-ID                  PIC XXX.
000049         16  CF-RECORD-TYPE                 PIC X.
000050             88  CF-COMPANY-MASTER              VALUE '1'.
000051             88  CF-PROCESSOR-MASTER            VALUE '2'.
000052             88  CF-STATE-MASTER                VALUE '3'.
000053             88  CF-LF-BENEFIT-MASTER           VALUE '4'.
000054             88  CF-AH-BENEFIT-MASTER           VALUE '5'.
000055             88  CF-CARRIER-MASTER              VALUE '6'.
000056             88  CF-MORTALITY-MASTER            VALUE '7'.
000057             88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
000058             88  CF-TERMINAL-MASTER             VALUE '9'.
000059             88  CF-AH-EDIT-MASTER              VALUE 'A'.
000060             88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
000061             88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
000062             88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
000063             88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
000064             88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
000065             88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
000066             88  CF-REMINDERS-MASTER            VALUE 'R'.
000067             88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
000068         16  CF-ACCESS-CD-GENL              PIC X(4).
000069         16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
000070             20  CF-PROCESSOR               PIC X(4).
000071         16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
000072             20  CF-STATE-CODE              PIC XX.
000073             20  FILLER                     PIC XX.
000074         16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
000075             20  FILLER                     PIC XX.
000076             20  CF-HI-BEN-IN-REC           PIC XX.
000077         16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
000078             20  FILLER                     PIC XXX.
000079             20  CF-CARRIER-CNTL            PIC X.
000080         16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
000081             20  FILLER                     PIC XX.
000082             20  CF-HI-TYPE-IN-REC          PIC 99.
000083         16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
000084             20  CF-CRDB-TABLE-INDICATOR    PIC X.
000085                 88  CF-CRDB-NAIC-TABLE         VALUE '9'.
000086             20  CF-CRDB-BENEFIT-TYPE       PIC X.
000087             20  CF-CRDB-WAITING-PERIOD     PIC XX.
000088         16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
000089             20  FILLER                     PIC X.
000090             20  CF-CUSTOM-REPORT-NO        PIC 999.
000091         16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
000092             20  FILLER                     PIC XX.
000093             20  CF-MORTGAGE-PLAN           PIC XX.
000094         16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
000095
000096     12  CF-LAST-MAINT-DT                   PIC XX.
000097     12  CF-LAST-MAINT-BY                   PIC X(4).
000098     12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
000099
000100     12  CF-RECORD-BODY                     PIC X(728).
000101
000102
000103****************************************************************
000104*             COMPANY MASTER RECORD                            *
000105****************************************************************
000106
000107     12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000108         16  CF-COMPANY-ADDRESS.
000109             20  CF-CL-MAIL-TO-NAME         PIC X(30).
000110             20  CF-CL-IN-CARE-OF           PIC X(30).
000111             20  CF-CL-ADDR-LINE-1          PIC X(30).
000112             20  CF-CL-ADDR-LINE-2          PIC X(30).
000113             20  CF-CL-CITY-STATE           PIC X(30).
000114             20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
000115             20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
000116         16  CF-COMPANY-CD                  PIC X.
000117         16  CF-COMPANY-PASSWORD            PIC X(8).
000118         16  CF-SECURITY-OPTION             PIC X.
000119             88  ALL-SECURITY                   VALUE '1'.
000120             88  COMPANY-VERIFY                 VALUE '2'.
000121             88  PROCESSOR-VERIFY               VALUE '3'.
000122             88  NO-SECURITY                    VALUE '4'.
000123             88  ALL-BUT-TERM                   VALUE '5'.
000124         16  CF-CARRIER-CONTROL-LEVEL       PIC X.
000125             88  USE-ACTUAL-CARRIER             VALUE SPACE.
000126         16  CF-LGX-INTERFACE-CNTL          PIC X.
000127             88  LGX-TIME-SHR-COMPANY           VALUE '1'.
000128         16  CF-INFORCE-LOCATION            PIC X.
000129             88  CERTS-ARE-ONLINE               VALUE '1'.
000130             88  CERTS-ARE-OFFLINE              VALUE '2'.
000131             88  NO-CERTS-AVAILABLE             VALUE '3'.
000132         16  CF-LOWER-CASE-LETTERS          PIC X.
000133         16  CF-CERT-ACCESS-CONTROL         PIC X.
000134             88  CF-ST-ACCNT-CNTL               VALUE ' '.
000135             88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
000136             88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
000137             88  CF-ACCNT-CNTL                  VALUE '3'.
000138             88  CF-CARR-ACCNT-CNTL             VALUE '4'.
000139
000140         16  CF-FORMS-PRINTER-ID            PIC X(4).
000141         16  CF-CHECK-PRINTER-ID            PIC X(4).
000142
000143         16  CF-LGX-CREDIT-USER             PIC X.
000144             88  CO-IS-NOT-USER                 VALUE 'N'.
000145             88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
000146
000147         16 CF-CREDIT-CALC-CODES.
000148             20  CF-CR-REM-TERM-CALC PIC X.
000149               88  CR-EARN-AFTER-15TH           VALUE '1'.
000150               88  CR-EARN-ON-HALF-MO           VALUE '2'.
000151               88  CR-EARN-ON-1ST-DAY           VALUE '3'.
000152               88  CR-EARN-ON-FULL-MO           VALUE '4'.
000153               88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
000154               88  CR-EARN-AFTER-14TH           VALUE '6'.
000155               88  CR-EARN-AFTER-16TH           VALUE '7'.
000156             20  CF-CR-R78-METHOD           PIC X.
000157               88  USE-TERM-PLUS-ONE            VALUE SPACE.
000158               88  DONT-USE-PLUS-ONE            VALUE '1'.
000159
000160         16  CF-CLAIM-CONTROL-COUNTS.
000161             20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
000162                 88  CO-CLM-COUNT-RESET         VALUE +99999.
000163
000164             20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
000165                 88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
000166
000167             20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
000168                 88  CO-CHECK-COUNT-RESET       VALUE +9999999.
000169
000170             20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
000171                 88  CO-QUE-COUNT-RESET         VALUE +9999999.
000172
000173         16  CF-CURRENT-MONTH-END           PIC XX.
000174
000175         16  CF-CO-CALC-QUOTE-TOLERANCE.
000176             20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
000177             20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
000178             20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
000179             20  CF-CO-CLAIM-REJECT-SW      PIC X.
000180                 88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
000181                 88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
000182             20  CF-CO-PREM-REJECT-SW       PIC X.
000183                 88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
000184                 88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
000185             20  CF-CO-REF-REJECT-SW        PIC X.
000186                 88 CO-WARN-IF-REF-OUT          VALUE SPACE.
000187                 88 CO-FORCE-IF-REF-OUT         VALUE '1'.
000188
000189         16  CF-CO-REPORTING-DT             PIC XX.
000190         16  CF-CO-REPORTING-MONTH-DT       PIC XX.
000191         16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
000192           88  CF-CO-NOT-MONTH-END              VALUE SPACES.
000193           88  CF-CO-MONTH-END                  VALUE '1'.
000194
000195         16  CF-LGX-CLAIM-USER              PIC X.
000196             88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
000197             88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
000198
000199         16  CF-CREDIT-EDIT-CONTROLS.
000200             20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
000201             20  CF-MIN-AGE                 PIC 99.
000202             20  CF-DEFAULT-AGE             PIC 99.
000203             20  CF-MIN-TERM                PIC S999      COMP-3.
000204             20  CF-MAX-TERM                PIC S999      COMP-3.
000205             20  CF-DEFAULT-SEX             PIC X.
000206             20  CF-JOINT-AGE-INPUT         PIC X.
000207                 88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
000208             20  CF-BIRTH-DATE-INPUT        PIC X.
000209                 88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
000210             20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
000211                 88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
000212                 88  CF-ZERO-CARRIER            VALUE '1'.
000213                 88  CF-ZERO-GROUPING           VALUE '2'.
000214                 88  CF-ZERO-CAR-GROUP          VALUE '3'.
000215             20  CF-EDIT-SW                 PIC X.
000216                 88  CF-START-EDIT-TONIGHT      VALUE '1'.
000217             20  CF-EDIT-RESTART-BATCH      PIC X(6).
000218             20  CF-CR-PR-METHOD            PIC X.
000219               88  USE-NORMAL-PR-METHOD         VALUE SPACE.
000220               88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
000221             20  FILLER                     PIC X.
000222
000223         16  CF-CREDIT-MISC-CONTROLS.
000224             20  CF-REIN-TABLE-SW           PIC X.
000225                 88 REIN-TABLES-ARE-USED        VALUE '1'.
000226             20  CF-COMP-TABLE-SW           PIC X.
000227                 88 COMP-TABLES-ARE-USED        VALUE '1'.
000228             20  CF-EXPERIENCE-RETENTION-AGE
000229                                            PIC S9        COMP-3.
000230             20  CF-CONVERSION-DT           PIC XX.
000231             20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
000232             20  CF-RUN-FREQUENCY-SW        PIC X.
000233                 88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
000234                 88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
000235
000236             20  CF-CR-CHECK-NO-CONTROL.
000237                 24  CF-CR-CHECK-NO-METHOD    PIC X.
000238                     88  CR-CHECK-NO-MANUAL       VALUE '1'.
000239                     88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
000240                     88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
000241                 24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
000242                     88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
000243
000244                 24  CF-CR-CHECK-COUNT       REDEFINES
000245                     CF-CR-CHECK-COUNTER      PIC X(4).
000246
000247                 24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
000248                     88  CR-QUE-COUNT-RESET      VALUE +9999999.
000249
000250                 24  CF-CR-CHECK-QUE-COUNT   REDEFINES
000251                     CF-CR-CHECK-QUE-COUNTER  PIC X(4).
000252                 24  CF-MAIL-PROCESSING       PIC X.
000253                     88  MAIL-PROCESSING          VALUE 'Y'.
000254
000255         16  CF-MISC-SYSTEM-CONTROL.
000256             20  CF-SYSTEM-C                 PIC X.
000257                 88  CONFIRMATION-SYS-USED       VALUE '1'.
000258             20  CF-SYSTEM-D                 PIC X.
000259                 88  DAILY-BILL-SYS-USED         VALUE '1'.
000260             20  CF-SOC-SEC-NO-SW            PIC X.
000261                 88  SOC-SEC-NO-USED             VALUE '1'.
000262             20  CF-MEMBER-NO-SW             PIC X.
000263                 88  MEMBER-NO-USED              VALUE '1'.
000264             20  CF-TAX-ID-NUMBER            PIC X(11).
000265             20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
000266             20  CF-PAYMENT-APPROVAL-SW      PIC X.
000267                 88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
000268                 88  CF-NO-APPROVAL              VALUE ' ' 'N'.
000269                 88  CF-ALL-APPROVED             VALUE 'Y'.
000270                 88  CF-GRADUATED-APPROVAL       VALUE 'G'.
000271             20  CF-SYSTEM-E                 PIC X.
000272                 88  CF-AR-SYSTEM-USED           VALUE 'Y'.
000273
000274         16  CF-LGX-LIFE-USER               PIC X.
000275             88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
000276             88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
000277
000278         16  CF-CR-MONTH-END-DT             PIC XX.
000279
000280         16  CF-FILE-MAINT-DATES.
000281             20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
000282                 88  CF-LAST-BATCH-RESET        VALUE +999999.
000283             20  CF-LAST-BATCH       REDEFINES
000284                 CF-LAST-BATCH-NO               PIC X(4).
000285             20  CF-RATES-FILE-MAINT-DT         PIC XX.
000286             20  CF-RATES-FILE-CREATE-DT        PIC XX.
000287             20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
000288             20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
000289             20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
000290             20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
000291             20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
000292             20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
000293             20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
000294             20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
000295
000296         16  CF-NEXT-COMPANY-ID             PIC XXX.
000297         16  FILLER                         PIC X.
000298
000299         16  CF-ALT-MORT-CODE               PIC X(4).
000300         16  CF-MEMBER-CAPTION              PIC X(10).
000301
000302         16  CF-LIFE-ACCESS-CONTROL         PIC X.
000303             88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
000304             88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
000305             88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
000306             88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
000307             88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
000308
000309         16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
000310
000311         16  CF-LIFE-OVERRIDE-L1            PIC X.
000312         16  CF-LIFE-OVERRIDE-L2            PIC XX.
000313         16  CF-LIFE-OVERRIDE-L6            PIC X(6).
000314         16  CF-LIFE-OVERRIDE-L12           PIC X(12).
000315
000316         16  CF-AH-OVERRIDE-L1              PIC X.
000317         16  CF-AH-OVERRIDE-L2              PIC XX.
000318         16  CF-AH-OVERRIDE-L6              PIC X(6).
000319         16  CF-AH-OVERRIDE-L12             PIC X(12).
000320
000321         16  CF-REPORT-CD1-CAPTION          PIC X(10).
000322         16  CF-REPORT-CD2-CAPTION          PIC X(10).
000323
000324         16  CF-CLAIM-CUTOFF-DATE           PIC XX.
000325         16  CF-AR-LAST-EL860-DT            PIC XX.
000326         16  CF-MP-MONTH-END-DT             PIC XX.
000327
000328         16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
000329         16  CF-CLAIM-PAID-THRU-TO          PIC X.
000330             88  CF-CLAIM-PAID-TO               VALUE '1'.
000331
000332         16  CF-AR-MONTH-END-DT             PIC XX.
000333
000334         16  CF-CRDTCRD-USER                PIC X.
000335             88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
000336             88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
000337
000338         16  CF-CC-MONTH-END-DT             PIC XX.
000339
000340         16  CF-PRINT-ADDRESS-LABELS        PIC X.
000341
000342         16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
000343             88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
000344             88  CF-USE-ALL-AGE-LAST            VALUE '2'.
000345             88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
000346         16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
000347         16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
000348         16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
000349         16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
000350             88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
000351             88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
000352         16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
000353         16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
000354         16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
000355             88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
000356         16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
000357
000358         16  CF-CL-ZIP-CODE.
000359             20  CF-CL-ZIP-PRIME.
000360                 24  CF-CL-ZIP-1ST          PIC X.
000361                     88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
000362                 24  FILLER                 PIC X(4).
000363             20  CF-CL-ZIP-PLUS4            PIC X(4).
000364         16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
000365             20  CF-CL-CAN-POSTAL-1         PIC XXX.
000366             20  CF-CL-CAN-POSTAL-2         PIC XXX.
000367             20  FILLER                     PIC XXX.
000368
000369         16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
000370         16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
000371         16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
000372         16  CF-CO-OPTION-START-DATE        PIC XX.
000373         16  CF-REM-TRM-CALC-OPTION         PIC X.
000374           88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
000375                                                      '3' '4'.
000376           88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
000377           88  CF-30-DAY-MONTH                  VALUE '1' '3'.
000378           88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
000379           88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
000380           88  CF-EXT-30-DAY-MONTH              VALUE '3'.
000381           88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
000382
000383         16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
000384
000385         16  CF-PAYMENT-APPROVAL-LEVELS.
000386             20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
000387             20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
000388             20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
000389             20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
000390             20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
000391             20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
000392
000393         16  CF-END-USER-REPORTING-USER     PIC X.
000394             88  CO-NO-END-USER-REPORTING       VALUE 'N'.
000395             88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
000396
000397         16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
000398             88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
000399             88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
000400
000401         16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
000402
000403         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
000404         16  FILLER                         PIC X.
000405
000406         16  CF-CREDIT-ARCHIVE-CNTL.
000407             20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
000408             20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
000409             20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
000410
000411         16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
000412
000413         16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
000414             88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
000415             88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
000416
000417         16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
000418             88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
000419             88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
000420
000421         16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
000422
000423         16  CF-CO-ACH-ID-CODE              PIC  X.
000424             88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
000425             88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
000426             88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
000427         16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
000428         16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
000429         16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
000430         16  CF-CO-ACH-ADMIN-NO             PIC X(09).
000431         16  CF-CO-ACH-RECV-NAME            PIC X(23).
000432         16  CF-CO-ACH-RECV-NO              PIC X(08).
000433         16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
000434         16  CF-CO-ACH-COMPANY-ID           PIC X(09).
000435         16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
000436                 88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
000437         16  CF-CO-ACH-TRACE-SPACE REDEFINES
000438                 CF-CO-ACH-TRACE-NO         PIC X(4).
000439
000440         16  CF-CO-OVER-SHORT.
000441             20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
000442             20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
000443
000444*         16  FILLER                         PIC X(102).
000445         16  CF-PAYMENT-APPROVAL-LEVELS-2.
000446             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
000447             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
000448
000449         16  CF-AH-APPROVAL-DAYS.
000450             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
000451             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
000452             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
000453             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
000454
000455         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
000456
000457         16  CF-APPROV-LEV-5.
000458             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
000459             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
000460             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
000461
000462         16  FILLER                         PIC X(68).
000463****************************************************************
000464*             PROCESSOR/USER RECORD                            *
000465****************************************************************
000466
000467     12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000468         16  CF-PROCESSOR-NAME              PIC X(30).
000469         16  CF-PROCESSOR-PASSWORD          PIC X(11).
000470         16  CF-PROCESSOR-TITLE             PIC X(26).
000471         16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
000472                 88  MESSAGE-YES                VALUE 'Y'.
000473                 88  MESSAGE-NO                 VALUE ' ' 'N'.
000474
000475*****************************************************
000476****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
000477****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
000478****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
000479****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
000480*****************************************************
000481
000482         16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
000483             20  CF-ADMINISTRATION-CONTROLS PIC XX.
000484             20  CF-APPLICATION-FORCE       PIC X.
000485             20  CF-INDIVIDUAL-APP.
000486                 24  CF-APP-SWITCHES  OCCURS  44 TIMES.
000487                     28  CF-BROWSE-APP      PIC X.
000488                     28  CF-UPDATE-APP      PIC X.
000489
000490         16  CF-CURRENT-TERM-ON             PIC X(4).
000491         16  CF-PROCESSOR-LIMITS-CLAIMS.
000492             20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
000493             20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
000494             20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
000495             20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
000496             20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
000497             20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
000498             20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
000499         16  CF-PROCESSOR-CARRIER           PIC X.
000500             88  NO-CARRIER-SECURITY            VALUE ' '.
000501         16  CF-PROCESSOR-ACCOUNT           PIC X(10).
000502             88  NO-ACCOUNT-SECURITY            VALUE SPACES.
000503         16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
000504             88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
000505         16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
000506             88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
000507
000508         16  CF-PROC-SYS-ACCESS-SW.
000509             20  CF-PROC-CREDIT-CLAIMS-SW.
000510                 24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
000511                     88  ACCESS-TO-CREDIT           VALUE 'Y'.
000512                 24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
000513                     88  ACCESS-TO-CLAIMS           VALUE 'Y'.
000514             20  CF-PROC-CREDIT-CLAIMS   REDEFINES
000515                 CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
000516                 88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
000517             20  CF-PROC-LIFE-GNRLDGR-SW.
000518                 24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
000519                     88  ACCESS-TO-LIFE             VALUE 'Y'.
000520                 24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
000521                     88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
000522             20  CF-PROC-LIFE-GNRLDGR    REDEFINES
000523                 CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
000524                 88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
000525         16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
000526             CF-PROC-SYS-ACCESS-SW              PIC X(4).
000527             88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
000528         16  CF-PROCESSOR-PRINTER               PIC X(4).
000529
000530         16  CF-APPROVAL-LEVEL                  PIC X.
000531             88  APPROVAL-LEVEL-1                   VALUE '1'.
000532             88  APPROVAL-LEVEL-2                   VALUE '2'.
000533             88  APPROVAL-LEVEL-3                   VALUE '3'.
000534             88  APPROVAL-LEVEL-4                   VALUE '4'.
000535             88  APPROVAL-LEVEL-5                   VALUE '5'.
000536
000537         16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
000538
000539         16  CF-LANGUAGE-TYPE                   PIC X.
000540             88  CF-LANG-IS-ENG                     VALUE 'E'.
000541             88  CF-LANG-IS-FR                      VALUE 'F'.
000542
000543         16  CF-CSR-IND                         PIC X.
000544         16  FILLER                             PIC X(239).
000545
000546****************************************************************
000547*             PROCESSOR/REMINDERS RECORD                       *
000548****************************************************************
000549
000550     12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
000551         16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
000552             20  CF-START-REMIND-DT         PIC XX.
000553             20  CF-END-REMIND-DT           PIC XX.
000554             20  CF-REMINDER-TEXT           PIC X(50).
000555         16  FILLER                         PIC X(296).
000556
000557
000558****************************************************************
000559*             STATE MASTER RECORD                              *
000560****************************************************************
000561
000562     12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000563         16  CF-STATE-ABBREVIATION          PIC XX.
000564         16  CF-STATE-NAME                  PIC X(25).
000565         16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
000566         16  CF-ST-CALC-QUOTE-TOLERANCE.
000567             20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
000568             20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
000569             20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
000570             20  CF-ST-CLAIM-REJECT-SW      PIC X.
000571                 88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
000572                 88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
000573             20  CF-ST-PREM-REJECT-SW       PIC X.
000574                 88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
000575                 88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
000576             20  CF-ST-REF-REJECT-SW        PIC X.
000577                 88 ST-WARN-IF-REF-OUT          VALUE SPACE.
000578                 88 ST-FORCE-IF-REF-OUT         VALUE '1'.
000579         16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
000580         16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
000581         16  CF-ST-REFUND-RULES.
000582             20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
000583             20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
000584             20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
000585         16  CF-ST-FST-PMT-EXTENSION.
000586             20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
000587             20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
000588                 88  CF-ST-EXT-NO-CHG           VALUE ' '.
000589                 88  CF-ST-EXT-CHG-LF           VALUE '1'.
000590                 88  CF-ST-EXT-CHG-AH           VALUE '2'.
000591                 88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
000592         16  CF-ST-STATE-CALL.
000593             20  CF-ST-CALL-UNEARNED        PIC X.
000594             20  CF-ST-CALL-RPT-CNTL        PIC X.
000595             20  CF-ST-CALL-RATE-DEV        PIC XXX.
000596         16  CF-REPLACEMENT-LAW-SW          PIC X.
000597             88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
000598             88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
000599         16  CF-REPLACEMENT-LETTER          PIC X(4).
000600         16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
000601         16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
000602         16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
000603         16  CF-ST-SPLIT-PAYMENT            PIC X.
000604         16  FILLER                         PIC X.
000605         16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
000606             20  CF-ST-BENEFIT-CD           PIC XX.
000607             20  CF-ST-BENEFIT-KIND         PIC X.
000608                 88  CF-ST-LIFE-KIND            VALUE 'L'.
000609                 88  CF-ST-AH-KIND              VALUE 'A'.
000610             20  CF-ST-REM-TERM-CALC        PIC X.
000611                 88  ST-REM-TERM-NOT-USED       VALUE SPACE.
000612                 88  ST-EARN-AFTER-15TH         VALUE '1'.
000613                 88  ST-EARN-ON-HALF-MO         VALUE '2'.
000614                 88  ST-EARN-ON-1ST-DAY         VALUE '3'.
000615                 88  ST-EARN-ON-FULL-MO         VALUE '4'.
000616                 88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
000617                 88  ST-EARN-AFTER-14TH         VALUE '6'.
000618                 88  ST-EARN-AFTER-16TH         VALUE '7'.
000619
000620             20  CF-ST-REFUND-CALC          PIC X.
000621                 88  ST-REFUND-NOT-USED         VALUE SPACE.
000622                 88  ST-REFD-BY-R78             VALUE '1'.
000623                 88  ST-REFD-BY-PRO-RATA        VALUE '2'.
000624                 88  ST-REFD-AS-CALIF           VALUE '3'.
000625                 88  ST-REFD-AS-TEXAS           VALUE '4'.
000626                 88  ST-REFD-IS-NET-PAY         VALUE '5'.
000627                 88  ST-REFD-ANTICIPATION       VALUE '6'.
000628                 88  ST-REFD-UTAH               VALUE '7'.
000629                 88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
000630                 88  ST-REFD-REG-BALLOON        VALUE 'B'.
000631                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
000632
000633             20  CF-ST-EARNING-CALC         PIC X.
000634                 88  ST-EARNING-NOT-USED        VALUE SPACE.
000635                 88  ST-EARN-BY-R78             VALUE '1'.
000636                 88  ST-EARN-BY-PRO-RATA        VALUE '2'.
000637                 88  ST-EARN-AS-CALIF           VALUE '3'.
000638                 88  ST-EARN-AS-TEXAS           VALUE '4'.
000639                 88  ST-EARN-IS-NET-PAY         VALUE '5'.
000640                 88  ST-EARN-ANTICIPATION       VALUE '6'.
000641                 88  ST-EARN-MEAN               VALUE '8'.
000642                 88  ST-EARN-REG-BALLOON        VALUE 'B'.
000643
000644             20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
000645                 88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
000646                 88  ST-OVRD-BY-R78             VALUE '1'.
000647                 88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
000648                 88  ST-OVRD-AS-CALIF           VALUE '3'.
000649                 88  ST-OVRD-AS-TEXAS           VALUE '4'.
000650                 88  ST-OVRD-IS-NET-PAY         VALUE '5'.
000651                 88  ST-OVRD-ANTICIPATION       VALUE '6'.
000652                 88  ST-OVRD-MEAN               VALUE '8'.
000653                 88  ST-OVRD-REG-BALLOON        VALUE 'B'.
000654             20  cf-st-extra-periods        pic 9.
000655*            20  FILLER                     PIC X.
000656
000657         16  CF-ST-COMMISSION-CAPS.
000658             20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
000659             20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
000660             20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
000661             20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
000662         16  CF-COMM-CAP-LIMIT-TO           PIC X.
000663                 88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
000664                 88  ST-LIMIT-TO-GA             VALUE 'G'.
000665                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
000666
000667         16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
000668
000669         16  CF-ST-STATUTORY-INTEREST.
000670             20  CF-ST-STAT-DATE-FROM       PIC X.
000671                 88  ST-STAT-FROM-INCURRED      VALUE 'I'.
000672                 88  ST-STAT-FROM-REPORTED      VALUE 'R'.
000673             20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
000674             20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
000675             20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
000676             20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
000677             20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
000678
000679         16  CF-ST-OVER-SHORT.
000680             20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
000681             20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
000682
000683         16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
000684
000685         16  CF-ST-RT-CALC                  PIC X.
000686
000687         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
000688         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
000689         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
000690         16  CF-ST-RF-LR-CALC               PIC X.
000691         16  CF-ST-RF-LL-CALC               PIC X.
000692         16  CF-ST-RF-LN-CALC               PIC X.
000693         16  CF-ST-RF-AH-CALC               PIC X.
000694         16  CF-ST-RF-CP-CALC               PIC X.
000695*        16  FILLER                         PIC X(206).
000696*CIDMOD         16  FILLER                         PIC X(192).
000697         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
000698             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
000699         16  CF-ST-REF-AH-DEATH-IND         PIC X.
000700         16  CF-ST-VFY-2ND-BENE             PIC X.
000701         16  CF-ST-CAUSAL-STATE             PIC X.
000702         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
000703         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
000704         16  CF-ST-AGENT-SIG-EDIT           PIC X.
000705             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
000706         16  CF-ST-NET-ONLY-STATE           PIC X.
000707             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
000708         16  cf-commission-cap-required     pic x.
000709         16  CF-ST-GA-COMMISSION-CAPS.
000710             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
000711             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
000712             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
000713             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
000714         16  CF-ST-TOT-COMMISSION-CAPS.
000715             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
000716             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
000717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
000718             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
000719         16  FILLER                         PIC X(156).
000720
000721****************************************************************
000722*             BENEFIT MASTER RECORD                            *
000723****************************************************************
000724
000725     12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000726         16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
000727             20  CF-BENEFIT-CODE            PIC XX.
000728             20  CF-BENEFIT-NUMERIC  REDEFINES
000729                 CF-BENEFIT-CODE            PIC XX.
000730             20  CF-BENEFIT-ALPHA           PIC XXX.
000731             20  CF-BENEFIT-DESCRIP         PIC X(10).
000732             20  CF-BENEFIT-COMMENT         PIC X(10).
000733
000734             20  CF-LF-COVERAGE-TYPE        PIC X.
000735                 88  CF-REDUCING                VALUE 'R'.
000736                 88  CF-LEVEL                   VALUE 'L' 'P'.
000737
000738             20  CF-SPECIAL-CALC-CD         PIC X.
000739                 88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
000740                 88  CF-NP-0-MO-INT             VALUE 'A'.
000741                 88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
000742                 88  CF-CRITICAL-PERIOD         VALUE 'C'.
000743                 88  CF-TERM-IN-DAYS            VALUE 'D'.
000744                 88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
000745                 88  CF-FARM-PLAN               VALUE 'F'.
000746                 88  CF-RATE-AS-STANDARD        VALUE 'G'.
000747                 88  CF-2-MTH-INTEREST          VALUE 'I'.
000748                 88  CF-3-MTH-INTEREST          VALUE 'J'.
000749                 88  CF-4-MTH-INTEREST          VALUE 'K'.
000750                 88  CF-BALLOON-LAST-PMT        VALUE 'L'.
000751                 88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
000752                 88  CF-PRUDENTIAL              VALUE 'P'.
000753                 88  CF-OUTSTANDING-BAL         VALUE 'O'.
000754                 88  CF-TRUNCATED-LIFE          VALUE 'T'.
000755                 88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
000756                 88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
000757                 88  CF-NET-PAY-SIMPLE          VALUE 'S'.
000758                 88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
000759
000760             20  CF-JOINT-INDICATOR         PIC X.
000761                 88  CF-JOINT-COVERAGE          VALUE 'J'.
000762
000763*            20  FILLER                     PIC X(12).
000764             20  cf-maximum-benefits        pic s999 comp-3.
000765             20  FILLER                     PIC X(09).
000766             20  CF-BENEFIT-CATEGORY        PIC X.
000767             20  CF-LOAN-TYPE               PIC X(8).
000768
000769             20  CF-CO-REM-TERM-CALC        PIC X.
000770                 88  CO-EARN-AFTER-15TH         VALUE '1'.
000771                 88  CO-EARN-ON-HALF-MO         VALUE '2'.
000772                 88  CO-EARN-ON-1ST-DAY         VALUE '3'.
000773                 88  CO-EARN-ON-FULL-MO         VALUE '4'.
000774                 88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
000775
000776             20  CF-CO-EARNINGS-CALC        PIC X.
000777                 88  CO-EARN-BY-R78             VALUE '1'.
000778                 88  CO-EARN-BY-PRO-RATA        VALUE '2'.
000779                 88  CO-EARN-AS-CALIF           VALUE '3'.
000780                 88  CO-EARN-AS-TEXAS           VALUE '4'.
000781                 88  CO-EARN-IS-NET-PAY         VALUE '5'.
000782                 88  CO-EARN-ANTICIPATION       VALUE '6'.
000783                 88  CO-EARN-AS-MEAN            VALUE '8'.
000784                 88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
000785
000786             20  CF-CO-REFUND-CALC          PIC X.
000787                 88  CO-REFUND-NOT-USED         VALUE SPACE.
000788                 88  CO-REFD-BY-R78             VALUE '1'.
000789                 88  CO-REFD-BY-PRO-RATA        VALUE '2'.
000790                 88  CO-REFD-AS-CALIF           VALUE '3'.
000791                 88  CO-REFD-AS-TEXAS           VALUE '4'.
000792                 88  CO-REFD-IS-NET-PAY         VALUE '5'.
000793                 88  CO-REFD-ANTICIPATION       VALUE '6'.
000794                 88  CO-REFD-MEAN               VALUE '8'.
000795                 88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
000796                 88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
000797                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
000798
000799             20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
000800                 88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
000801                 88  CO-OVRD-BY-R78             VALUE '1'.
000802                 88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
000803                 88  CO-OVRD-AS-CALIF           VALUE '3'.
000804                 88  CO-OVRD-AS-TEXAS           VALUE '4'.
000805                 88  CO-OVRD-IS-NET-PAY         VALUE '5'.
000806                 88  CO-OVRD-ANTICIPATION       VALUE '6'.
000807                 88  CO-OVRD-MEAN               VALUE '8'.
000808                 88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
000809
000810             20  CF-CO-BEN-I-G-CD           PIC X.
000811                 88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
000812                 88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
000813                 88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
000814
000815         16  FILLER                         PIC X(304).
000816
000817
000818****************************************************************
000819*             CARRIER MASTER RECORD                            *
000820****************************************************************
000821
000822     12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
000823         16  CF-ADDRESS-DATA.
000824             20  CF-MAIL-TO-NAME            PIC X(30).
000825             20  CF-IN-CARE-OF              PIC X(30).
000826             20  CF-ADDRESS-LINE-1          PIC X(30).
000827             20  CF-ADDRESS-LINE-2          PIC X(30).
000828             20  CF-CITY-STATE              PIC X(30).
000829             20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
000830             20  CF-PHONE-NO                PIC 9(11)     COMP-3.
000831
000832         16  CF-CLAIM-NO-CONTROL.
000833             20  CF-CLAIM-NO-METHOD         PIC X.
000834                 88  CLAIM-NO-MANUAL            VALUE '1'.
000835                 88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
000836                 88  CLAIM-NO-SEQ               VALUE '3'.
000837                 88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
000838             20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
000839                 88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
000840                 88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
000841                 88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
000842
000843         16  CF-CHECK-NO-CONTROL.
000844             20  CF-CHECK-NO-METHOD         PIC X.
000845                 88  CHECK-NO-MANUAL            VALUE '1'.
000846                 88  CHECK-NO-AUTO-SEQ          VALUE '2'.
000847                 88  CHECK-NO-CARR-SEQ          VALUE '3'.
000848                 88  CHECK-NO-AT-PRINT          VALUE '4'.
000849             20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
000850                 88  CHECK-CNT-RESET-VALUE      VALUE +999999.
000851
000852         16  CF-DOMICILE-STATE              PIC XX.
000853
000854         16  CF-EXPENSE-CONTROLS.
000855             20  CF-EXPENSE-METHOD          PIC X.
000856                 88  EXPENSE-CALC-MANUAL        VALUE '1'.
000857                 88  DOLLARS-PER-PMT            VALUE '2'.
000858                 88  PERCENT-OF-PAYMENT         VALUE '3'.
000859                 88  DOLLARS-PER-MONTH          VALUE '4'.
000860             20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
000861             20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
000862
000863         16  CF-CORRESPONDENCE-CONTROL.
000864             20  CF-LETTER-RESEND-OPT       PIC X.
000865                 88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
000866                 88  LETTERS-ARE-ARCHIVED       VALUE '1'.
000867             20  FILLER                     PIC X(4).
000868
000869         16  CF-RESERVE-CONTROLS.
000870             20  CF-MANUAL-SW               PIC X.
000871                 88  CF-MANUAL-RESERVES-USED    VALUE '1'.
000872             20  CF-FUTURE-SW               PIC X.
000873                 88  CF-FUTURE-RESERVES-USED    VALUE '1'.
000874             20  CF-PTC-SW                  PIC X.
000875                 88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
000876             20  CF-IBNR-SW                 PIC X.
000877                 88  CF-IBNR-RESERVES-USED      VALUE '1'.
000878             20  CF-PTC-LF-SW               PIC X.
000879                 88  CF-LF-PTC-USED             VALUE '1'.
000880             20  CF-CDT-ACCESS-METHOD       PIC X.
000881                 88  CF-CDT-ROUND-NEAR          VALUE '1'.
000882                 88  CF-CDT-ROUND-HIGH          VALUE '2'.
000883                 88  CF-CDT-INTERPOLATED        VALUE '3'.
000884             20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
000885
000886         16  CF-CLAIM-CALC-METHOD           PIC X.
000887             88  360-PLUS-MONTHS                VALUE '1'.
000888             88  365-PLUS-MONTHS                VALUE '2'.
000889             88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
000890             88  360-DAILY                      VALUE '4'.
000891             88  365-DAILY                      VALUE '5'.
000892
000893         16  CF-LAST-ALPHA-CHARACTER        PIC X.
000894         16  FILLER                         PIC X(11).
000895
000896         16  CF-LIMIT-AMOUNTS.
000897             20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
000898             20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
000899             20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
000900             20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
000901             20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
000902             20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
000903             20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
000904             20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
000905             20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
000906             20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
000907
000908         16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
000909         16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
000910         16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
000911
000912         16  CF-ZIP-CODE.
000913             20  CF-ZIP-PRIME.
000914                 24  CF-ZIP-1ST             PIC X.
000915                     88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000916                 24  FILLER                 PIC X(4).
000917             20  CF-ZIP-PLUS4               PIC X(4).
000918         16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
000919             20  CF-CAN-POSTAL-1            PIC XXX.
000920             20  CF-CAN-POSTAL-2            PIC XXX.
000921             20  FILLER                     PIC XXX.
000922
000923         16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
000924         16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
000925         16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
000926
000927         16  CF-RATING-SWITCH               PIC X.
000928             88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
000929             88  CF-NO-RATING                   VALUE 'N'.
000930
000931         16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
000932
000933         16  CF-CARRIER-OVER-SHORT.
000934             20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
000935             20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
000936
000937         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
000938         16  CF-SECPAY-SWITCH               PIC X.
000939             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
000940             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
000941         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
000942         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
000943         16  FILLER                         PIC X(444).
000944*        16  FILLER                         PIC X(452).
000945
000946
000947****************************************************************
000948*             MORTALITY MASTER RECORD                          *
000949****************************************************************
000950
000951     12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
000952         16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
000953                                INDEXED BY CF-MORT-NDX.
000954             20  CF-MORT-TABLE              PIC X(5).
000955             20  CF-MORT-TABLE-TYPE         PIC X.
000956                 88  CF-MORT-JOINT              VALUE 'J'.
000957                 88  CF-MORT-SINGLE             VALUE 'S'.
000958                 88  CF-MORT-COMBINED           VALUE 'C'.
000959                 88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
000960                 88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
000961             20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
000962             20  CF-MORT-AGE-METHOD         PIC XX.
000963                 88  CF-AGE-LAST                VALUE 'AL'.
000964                 88  CF-AGE-NEAR                VALUE 'AN'.
000965             20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
000966             20  CF-MORT-ADJUSTMENT-DIRECTION
000967                                            PIC X.
000968                 88  CF-MINUS                   VALUE '-'.
000969                 88  CF-PLUS                    VALUE '+'.
000970             20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
000971             20  CF-MORT-JOINT-CODE         PIC X.
000972                 88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
000973             20  CF-MORT-PC-Q               PIC X.
000974                 88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
000975             20  CF-MORT-TABLE-CODE         PIC X(4).
000976             20  CF-MORT-COMMENTS           PIC X(15).
000977             20  FILLER                     PIC X(14).
000978
000979         16  FILLER                         PIC X(251).
000980
000981
000982****************************************************************
000983*             BUSSINESS TYPE MASTER RECORD                     *
000984****************************************************************
000985
000986     12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
000987* FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
000988* RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
000989* AND RECORD 05 IS TYPES 81-99
000990         16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
000991             20  CF-BUSINESS-TITLE          PIC  X(19).
000992             20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
000993                                            PIC S9V9(4) COMP-3.
000994             20  CF-BUS-EXCL-ST-CALL        PIC  X.
000995             20  FILLER                     PIC  X.
000996         16  FILLER                         PIC  X(248).
000997
000998
000999****************************************************************
001000*             TERMINAL MASTER RECORD                           *
001001****************************************************************
001002
001003     12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
001004
001005         16  CF-COMPANY-TERMINALS.
001006             20  CF-TERMINAL-ID  OCCURS 120 TIMES
001007                                  PIC X(4).
001008         16  FILLER               PIC X(248).
001009
001010
001011****************************************************************
001012*             LIFE EDIT MASTER RECORD                          *
001013****************************************************************
001014
001015     12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
001016         16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
001017             20  CF-LIFE-CODE-IN            PIC XX.
001018             20  CF-LIFE-CODE-OUT           PIC XX.
001019         16  FILLER                         PIC X(248).
001020
001021
001022****************************************************************
001023*             AH EDIT MASTER RECORD                            *
001024****************************************************************
001025
001026     12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
001027         16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
001028             20  CF-AH-CODE-IN              PIC XXX.
001029             20  CF-AH-CODE-OUT             PIC XX.
001030         16  FILLER                         PIC X(248).
001031
001032
001033****************************************************************
001034*             CREDIBILITY TABLES                               *
001035****************************************************************
001036
001037     12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
001038         16  CF-CRDB-ENTRY   OCCURS 36 TIMES
001039                             INDEXED BY CF-CRDB-NDX.
001040             20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
001041             20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
001042             20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
001043         16  FILLER                         PIC  X(332).
001044
001045
001046****************************************************************
001047*             REPORT CUSTOMIZATION RECORD                      *
001048****************************************************************
001049
001050     12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
001051         16  CF-ACCOUNT-MASTER-STATUS       PIC X.
001052             88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
001053             88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
001054             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
001055**** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
001056****       A T-TRANSFER.                                   ****
001057             88  CF-ALL-ACCOUNTS                VALUE 'B'.
001058
001059         16  FILLER                         PIC XX.
001060
001061         16  CF-CARRIER-CNTL-OPT.
001062             20  CF-CARRIER-OPT-SEQ         PIC 9.
001063                 88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
001064                 88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
001065             20  CF-CARRIER-SELECT OCCURS 3 TIMES
001066                                            PIC X.
001067         16  CF-GROUP-CNTL-OPT.
001068             20  CF-GROUP-OPT-SEQ           PIC 9.
001069                 88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
001070                 88  CF-GROUP-OPT-NOT-USED      VALUE 0.
001071             20  CF-GROUP-SELECT OCCURS 3 TIMES
001072                                            PIC X(6).
001073         16  CF-STATE-CNTL-OPT.
001074             20  CF-STATE-OPT-SEQ           PIC 9.
001075                 88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
001076                 88  CF-STATE-OPT-NOT-USED      VALUE 0.
001077             20  CF-STATE-SELECT OCCURS 3 TIMES
001078                                            PIC XX.
001079         16  CF-ACCOUNT-CNTL-OPT.
001080             20  CF-ACCOUNT-OPT-SEQ         PIC 9.
001081                 88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
001082                 88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
001083             20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
001084                                            PIC X(10).
001085         16  CF-BUS-TYP-CNTL-OPT.
001086             20  CF-BUS-TYP-OPT-SEQ         PIC 9.
001087                 88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
001088                 88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
001089             20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
001090                                            PIC XX.
001091         16  CF-LF-TYP-CNTL-OPT.
001092             20  CF-LF-TYP-OPT-SEQ          PIC 9.
001093                 88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
001094                 88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
001095             20  CF-BUS-LF-SELECT OCCURS 3 TIMES
001096                                            PIC XX.
001097         16  CF-AH-TYP-CNTL-OPT.
001098             20  CF-AH-TYP-OPT-SEQ          PIC 9.
001099                 88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
001100                 88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
001101             20  CF-BUS-AH-SELECT OCCURS 3 TIMES
001102                                            PIC XX.
001103         16  CF-REPTCD1-CNTL-OPT.
001104             20  CF-REPTCD1-OPT-SEQ         PIC 9.
001105                 88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
001106                 88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
001107             20  CF-REPTCD1-SELECT OCCURS 3 TIMES
001108                                            PIC X(10).
001109         16  CF-REPTCD2-CNTL-OPT.
001110             20  CF-REPTCD2-OPT-SEQ         PIC 9.
001111                 88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
001112                 88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
001113             20  CF-REPTCD2-SELECT OCCURS 3 TIMES
001114                                            PIC X(10).
001115         16  CF-USER1-CNTL-OPT.
001116             20  CF-USER1-OPT-SEQ           PIC 9.
001117                 88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
001118                 88  CF-USER1-OPT-NOT-USED      VALUE 0.
001119             20  CF-USER1-SELECT OCCURS 3 TIMES
001120                                            PIC X(10).
001121         16  CF-USER2-CNTL-OPT.
001122             20  CF-USER2-OPT-SEQ           PIC 9.
001123                 88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
001124                 88  CF-USER2-OPT-NOT-USED      VALUE 0.
001125             20  CF-USER2-SELECT OCCURS 3 TIMES
001126                                            PIC X(10).
001127         16  CF-USER3-CNTL-OPT.
001128             20  CF-USER3-OPT-SEQ           PIC 9.
001129                 88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
001130                 88  CF-USER3-OPT-NOT-USED      VALUE 0.
001131             20  CF-USER3-SELECT OCCURS 3 TIMES
001132                                            PIC X(10).
001133         16  CF-USER4-CNTL-OPT.
001134             20  CF-USER4-OPT-SEQ           PIC 9.
001135                 88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
001136                 88  CF-USER4-OPT-NOT-USED      VALUE 0.
001137             20  CF-USER4-SELECT OCCURS 3 TIMES
001138                                            PIC X(10).
001139         16  CF-USER5-CNTL-OPT.
001140             20  CF-USER5-OPT-SEQ           PIC 9.
001141                 88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
001142                 88  CF-USER5-OPT-NOT-USED      VALUE 0.
001143             20  CF-USER5-SELECT OCCURS 3 TIMES
001144                                            PIC X(10).
001145         16  CF-REINS-CNTL-OPT.
001146             20  CF-REINS-OPT-SEQ           PIC 9.
001147                 88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
001148                 88  CF-REINS-OPT-NOT-USED      VALUE 0.
001149             20  CF-REINS-SELECT OCCURS 3 TIMES.
001150                 24  CF-REINS-PRIME         PIC XXX.
001151                 24  CF-REINS-SUB           PIC XXX.
001152
001153         16  CF-AGENT-CNTL-OPT.
001154             20  CF-AGENT-OPT-SEQ           PIC 9.
001155                 88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
001156                 88  CF-AGENT-OPT-NOT-USED      VALUE 0.
001157             20  CF-AGENT-SELECT OCCURS 3 TIMES
001158                                            PIC X(10).
001159
001160         16  FILLER                         PIC X(43).
001161
001162         16  CF-LOSS-RATIO-SELECT.
001163             20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
001164             20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
001165         16  CF-ENTRY-DATE-SELECT.
001166             20  CF-SEL-LO-ENTRY-DATE       PIC XX.
001167             20  CF-SEL-HI-ENTRY-DATE       PIC XX.
001168         16  CF-EFFECTIVE-DATE-SELECT.
001169             20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
001170             20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
001171
001172         16  CF-EXCEPTION-LIST-IND          PIC X.
001173             88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
001174
001175         16  FILLER                         PIC X(318).
001176
001177****************************************************************
001178*                  EXCEPTION REPORTING RECORD                  *
001179****************************************************************
001180
001181     12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
001182         16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
001183             88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
001184
001185         16  CF-COMBINED-LIFE-AH-OPT.
001186             20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
001187             20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
001188             20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
001189             20  CF-CANCELLATION-RATIO      PIC S9(02).
001190
001191         16  CF-LIFE-OPT.
001192             20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
001193             20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
001194             20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
001195             20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
001196             20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
001197             20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
001198             20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
001199             20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
001200             20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
001201             20  CF-LF-AVG-AGE-MAX          PIC S9(02).
001202
001203         16  CF-AH-OPT.
001204             20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
001205             20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
001206             20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
001207             20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
001208             20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
001209             20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
001210             20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
001211             20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
001212             20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
001213             20  CF-AH-AVG-AGE-MAX          PIC S9(02).
001214
001215         16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
001216             88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
001217             88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
001218             88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
001219
001220         16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
001221
001222         16  FILLER                         PIC X(673).
001223
001224
001225****************************************************************
001226*             MORTGAGE SYSTEM PLAN RECORD                      *
001227****************************************************************
001228
001229     12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
001230         16  CF-PLAN-TYPE                   PIC X.
001231             88  CF-LIFE-MORT-PLAN             VALUE 'L'.
001232             88  CF-DISAB-MORT-PLAN            VALUE 'D'.
001233             88  CF-AD-D-MORT-PLAN             VALUE 'A'.
001234         16  CF-PLAN-ABBREV                 PIC XXX.
001235         16  CF-PLAN-DESCRIPT               PIC X(10).
001236         16  CF-PLAN-NOTES                  PIC X(20).
001237         16  CF-PLAN-ESTABLISH-DATE         PIC XX.
001238         16  CF-PLAN-UNDERWRITING.
001239             20  CF-PLAN-TERM-DATA.
001240                 24  CF-MINIMUM-TERM        PIC S999      COMP-3.
001241                 24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
001242             20  CF-PLAN-AGE-DATA.
001243                 24  CF-MINIMUM-AGE         PIC S999      COMP-3.
001244                 24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
001245                 24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
001246             20  CF-PLAN-BENEFIT-DATA.
001247                 24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
001248                 24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
001249                 24  CF-MAXIMUM-MONTHLY-BENEFIT
001250                                            PIC S9(7)V99  COMP-3.
001251         16  CF-PLAN-POLICY-FORMS.
001252             20  CF-POLICY-FORM             PIC X(12).
001253             20  CF-MASTER-APPLICATION      PIC X(12).
001254             20  CF-MASTER-POLICY           PIC X(12).
001255         16  CF-PLAN-RATING.
001256             20  CF-RATE-CODE               PIC X(5).
001257             20  CF-SEX-RATING              PIC X.
001258                 88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
001259                 88  CF-PLAN-SEX-RATED         VALUE '2'.
001260             20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
001261             20  CF-SUB-STD-TYPE            PIC X.
001262                 88  CF-PCT-OF-PREM            VALUE '1'.
001263                 88  CF-PCT-OF-BENE            VALUE '2'.
001264         16  CF-PLAN-PREM-TOLERANCES.
001265             20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
001266             20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
001267         16  CF-PLAN-PYMT-TOLERANCES.
001268             20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
001269             20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
001270         16  CF-PLAN-MISC-DATA.
001271             20  FILLER                     PIC X.
001272             20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
001273             20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
001274         16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
001275         16  CF-PLAN-IND-GRP                PIC X.
001276             88  CF-MORT-INDIV-PLAN            VALUE 'I'
001277                                                     '1'.
001278             88  CF-MORT-GROUP-PLAN            VALUE 'G'
001279                                                     '2'.
001280         16  CF-MIB-SEARCH-SW               PIC X.
001281             88  CF-MIB-SEARCH-ALL             VALUE '1'.
001282             88  CF-MIB-SEARCH-NONE            VALUE '2'.
001283             88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
001284             88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
001285         16  CF-ALPHA-SEARCH-SW             PIC X.
001286             88  CF-MIB-ALPHA-ALL              VALUE '1'.
001287             88  CF-MIB-ALPHA-NONE             VALUE '2'.
001288             88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
001289             88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
001290             88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
001291             88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
001292             88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
001293             88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
001294             88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
001295             88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
001296                                                     'A' 'B' 'C'
001297                                                     'X' 'Y' 'Z'.
001298         16  CF-EFF-DT-RULE-SW              PIC X.
001299             88  CF-EFF-DT-ENTER               VALUE 'E'.
001300             88  CF-EFF-DT-MONTH               VALUE 'M'.
001301             88  CF-EFF-DT-QTR                 VALUE 'Q'.
001302             88  CF-EFF-DT-SEMI                VALUE 'S'.
001303             88  CF-EFF-DT-ANN                 VALUE 'A'.
001304         16  FILLER                         PIC X(4).
001305         16  CF-HEALTH-QUESTIONS            PIC X.
001306             88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
001307         16  CF-GRACE-PERIOD                PIC S999      COMP-3.
001308         16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
001309         16  CF-PLAN-SNGL-JNT               PIC X.
001310             88  CF-COMBINED-PLAN              VALUE 'C'.
001311             88  CF-JNT-PLAN                   VALUE 'J'.
001312             88  CF-SNGL-PLAN                  VALUE 'S'.
001313         16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
001314         16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
001315         16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
001316         16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
001317         16  CF-RERATE-CNTL                 PIC  X.
001318             88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
001319             88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
001320             88  CF-DO-NOT-RERATE               VALUE '3' ' '.
001321             88  CF-AUTO-RECALC                 VALUE '4'.
001322         16  CF-BENEFIT-TYPE                PIC  X.
001323             88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
001324             88  CF-BENEFIT-REDUCES             VALUE '2'.
001325         16  CF-POLICY-FEE                  PIC S999V99
001326                                                    COMP-3.
001327         16  CF-1ST-NOTICE-FORM             PIC  X(04).
001328         16  CF-2ND-NOTICE-FORM             PIC  X(04).
001329         16  CF-3RD-NOTICE-FORM             PIC  X(04).
001330         16  CF-4TH-NOTICE-FORM             PIC  X(04).
001331         16  FILLER                         PIC  X(32).
001332         16  CF-TERMINATION-FORM            PIC  X(04).
001333         16  FILLER                         PIC  X(08).
001334         16  CF-CLAIM-CAP                   PIC S9(7)V99
001335                                                       COMP-3.
001336         16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
001337         16  CF-ISSUE-LETTER                PIC  X(4).
001338         16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
001339         16  CF-DEPENDENT-COVERAGE          PIC  X.
001340             88  CF-YES-DEP-COV                 VALUE 'Y'.
001341             88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
001342         16  CF-MP-REFUND-CALC              PIC X.
001343             88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
001344             88  CF-MP-REFD-BY-R78              VALUE '1'.
001345             88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
001346             88  CF-MP-REFD-AS-CALIF            VALUE '3'.
001347             88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
001348             88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
001349             88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
001350             88  CF-MP-REFD-MEAN                VALUE '8'.
001351         16  CF-ALT-RATE-CODE               PIC  X(5).
001352
001353
001354         16  FILLER                         PIC X(498).
001355****************************************************************
001356*             MORTGAGE COMPANY MASTER RECORD                   *
001357****************************************************************
001358
001359     12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
001360         16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
001361         16  CF-MORTG-ACCESS-CONTROL        PIC X.
001362             88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
001363             88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
001364             88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
001365             88  CF-MORT-PROD-CNTL                   VALUE '3'.
001366             88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
001367
001368         16  CF-MORTG-CONVERSION-DATE       PIC XX.
001369         16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
001370         16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
001371         16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
001372         16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
001373
001374         16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
001375             88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
001376         16  CF-MP-RECON-USE-IND            PIC X(1).
001377             88  CF-MP-USE-RECON             VALUE 'Y'.
001378         16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
001379             88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
001380         16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
001381             88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
001382             88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
001383         16  FILLER                         PIC X(1).
001384         16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
001385             88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
001386         16  CF-MORTG-MIB-VERSION           PIC X.
001387             88  CF-MORTG-MIB-BATCH         VALUE '1'.
001388             88  CF-MORTG-MIB-ONLINE        VALUE '2'.
001389             88  CF-MORTG-MIB-BOTH          VALUE '3'.
001390         16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
001391             20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
001392                 88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
001393             20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
001394                 88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
001395             20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
001396                 88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
001397             20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
001398                 88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
001399             20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
001400                 88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
001401             20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
001402                 88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
001403         16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
001404         16  FILLER                         PIC X(7).
001405         16  CF-MORTG-DESTINATION-SYMBOL.
001406             20  CF-MORTG-MIB-COMM          PIC X(5).
001407             20  CF-MORTG-MIB-TERM          PIC X(5).
001408         16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
001409             88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
001410         16  FILLER                         PIC X(03).
001411         16  CF-MP-CHECK-NO-CONTROL.
001412             20  CF-MP-CHECK-NO-METHOD      PIC X(01).
001413                 88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
001414                 88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
001415                                                ' ' LOW-VALUES.
001416                 88  CF-MP-CHECK-NO-PRE-PRINTED
001417                                               VALUE '3'.
001418         16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
001419         16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
001420         16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
001421             20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
001422                 88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
001423             20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
001424                 88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
001425             20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
001426                 88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
001427             20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
001428                 88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
001429             20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
001430                 88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
001431             20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
001432                 88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
001433         16  CF-MORTG-BILLING-AREA.
001434             20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
001435                                            PIC X.
001436         16  CF-MORTG-MONTH-END-DT          PIC XX.
001437         16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
001438         16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
001439         16  CF-MORTG-MIB-DEST-SW           PIC X.
001440             88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
001441             88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
001442         16  FILLER                         PIC X.
001443         16  CF-MORTG-LABEL-CONTROL         PIC X.
001444             88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
001445             88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
001446         16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
001447         16  FILLER                         PIC X(8).
001448         16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
001449         16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
001450         16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
001451         16  CF-ACH-COMPANY-ID.
001452             20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
001453                 88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
001454                 88  CF-ACH-ICD-DUNS                VALUE '3'.
001455                 88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
001456             20  CF-ACH-COMPANY-ID-NO       PIC X(9).
001457         16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
001458             88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
001459         16  CF-RATE-DEV-AUTHORIZATION      PIC X.
001460             88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
001461             88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
001462         16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
001463         16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
001464         16  FILLER                         PIC X(536).
001465
001466****************************************************************
001467*             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
001468****************************************************************
001469
001470     12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
001471         16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
001472             20  CF-FEMALE-HEIGHT.
001473                 24  CF-FEMALE-FT           PIC 99.
001474                 24  CF-FEMALE-IN           PIC 99.
001475             20  CF-FEMALE-MIN-WT           PIC 999.
001476             20  CF-FEMALE-MAX-WT           PIC 999.
001477         16  FILLER                         PIC X(428).
001478
001479     12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
001480         16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
001481             20  CF-MALE-HEIGHT.
001482                 24  CF-MALE-FT             PIC 99.
001483                 24  CF-MALE-IN             PIC 99.
001484             20  CF-MALE-MIN-WT             PIC 999.
001485             20  CF-MALE-MAX-WT             PIC 999.
001486         16  FILLER                         PIC X(428).
001487******************************************************************
001488*             AUTOMATIC ACTIVITY RECORD                          *
001489******************************************************************
001490     12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
001491         16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
001492             20  CF-SYS-ACTIVE-SW           PIC X(01).
001493             20  CF-SYS-LETTER-ID           PIC X(04).
001494             20  CF-SYS-RESEND-DAYS         PIC 9(03).
001495             20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
001496             20  CF-SYS-RESET-SW            PIC X(01).
001497             20  CF-SYS-REPORT-DAYS         PIC 9(03).
001498             20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
001499
001500         16  FILLER                         PIC X(50).
001501
001502         16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
001503             20  CF-USER-ACTIVE-SW          PIC X(01).
001504             20  CF-USER-LETTER-ID          PIC X(04).
001505             20  CF-USER-RESEND-DAYS        PIC 9(03).
001506             20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
001507             20  CF-USER-RESET-SW           PIC X(01).
001508             20  CF-USER-REPORT-DAYS        PIC 9(03).
001509             20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
001510             20  CF-USER-ACTIVITY-DESC      PIC X(20).
001511
001512         16  FILLER                         PIC X(246).
      *<<((file: ELCCNTL))
000696     EJECT
000697*                                    COPY ELCCERT.
      *>>((file: ELCCERT))
000001******************************************************************
000002*                                                                *
000003*                            ELCCERT.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.013                          *
000006*                                                                *
000007*   FILE DESCRIPTION = CERTIFICATE MASTER                        *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 450  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
000013*       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
000014*       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
000015*       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
000016*       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
000017*                                                                *
000018*   LOG = YES                                                    *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020******************************************************************
000021*                   C H A N G E   L O G
000022*
000023* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000024*-----------------------------------------------------------------
000025*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000026* EFFECTIVE    NUMBER
000027*-----------------------------------------------------------------
000028* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
000029* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
000030* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
000031* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
000032* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
000033* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
000034* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000035* 032612  CR2011110200001  PEMA  AHL CHANGES
000036* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
000037* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000038* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
000039******************************************************************
000040
000041 01  CERTIFICATE-MASTER.
000042     12  CM-RECORD-ID                      PIC XX.
000043         88  VALID-CM-ID                      VALUE 'CM'.
000044
000045     12  CM-CONTROL-PRIMARY.
000046         16  CM-COMPANY-CD                 PIC X.
000047         16  CM-CARRIER                    PIC X.
000048         16  CM-GROUPING.
000049             20  CM-GROUPING-PREFIX        PIC X(3).
000050             20  CM-GROUPING-PRIME         PIC X(3).
000051         16  CM-STATE                      PIC XX.
000052         16  CM-ACCOUNT.
000053             20  CM-ACCOUNT-PREFIX         PIC X(4).
000054             20  CM-ACCOUNT-PRIME          PIC X(6).
000055         16  CM-CERT-EFF-DT                PIC XX.
000056         16  CM-CERT-NO.
000057             20  CM-CERT-PRIME             PIC X(10).
000058             20  CM-CERT-SFX               PIC X.
000059
000060     12  CM-CONTROL-BY-NAME.
000061         16  CM-COMPANY-CD-A1              PIC X.
000062         16  CM-INSURED-LAST-NAME          PIC X(15).
000063         16  CM-INSURED-INITIALS.
000064             20  CM-INSURED-INITIAL1       PIC X.
000065             20  CM-INSURED-INITIAL2       PIC X.
000066
000067     12  CM-CONTROL-BY-SSN.
000068         16  CM-COMPANY-CD-A2              PIC X.
000069         16  CM-SOC-SEC-NO.
000070             20  CM-SSN-STATE              PIC XX.
000071             20  CM-SSN-ACCOUNT            PIC X(6).
000072             20  CM-SSN-LN3.
000073                 25  CM-INSURED-INITIALS-A2.
000074                     30 CM-INSURED-INITIAL1-A2   PIC X.
000075                     30 CM-INSURED-INITIAL2-A2   PIC X.
000076                 25 CM-PART-LAST-NAME-A2         PIC X.
000077
000078     12  CM-CONTROL-BY-CERT-NO.
000079         16  CM-COMPANY-CD-A4              PIC X.
000080         16  CM-CERT-NO-A4                 PIC X(11).
000081
000082     12  CM-CONTROL-BY-MEMB.
000083         16  CM-COMPANY-CD-A5              PIC X.
000084         16  CM-MEMBER-NO.
000085             20  CM-MEMB-STATE             PIC XX.
000086             20  CM-MEMB-ACCOUNT           PIC X(6).
000087             20  CM-MEMB-LN4.
000088                 25  CM-INSURED-INITIALS-A5.
000089                     30 CM-INSURED-INITIAL1-A5   PIC X.
000090                     30 CM-INSURED-INITIAL2-A5   PIC X.
000091                 25 CM-PART-LAST-NAME-A5         PIC XX.
000092
000093     12  CM-INSURED-PROFILE-DATA.
000094         16  CM-INSURED-FIRST-NAME.
000095             20  CM-INSURED-1ST-INIT       PIC X.
000096             20  FILLER                    PIC X(9).
000097         16  CM-INSURED-ISSUE-AGE          PIC 99.
000098         16  CM-INSURED-SEX                PIC X.
000099             88  CM-SEX-MALE                  VALUE 'M'.
000100             88  CM-SEX-FEMAL                 VALUE 'F'.
000101         16  CM-INSURED-JOINT-AGE          PIC 99.
000102         16  CM-JOINT-INSURED-NAME.
000103             20  CM-JT-LAST-NAME           PIC X(15).
000104             20  CM-JT-FIRST-NAME.
000105                 24  CM-JT-1ST-INIT        PIC X.
000106                 24  FILLER                PIC X(9).
000107             20  CM-JT-INITIAL             PIC X.
000108
000109     12  CM-LIFE-DATA.
000110         16  CM-LF-BENEFIT-CD              PIC XX.
000111         16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
000112         16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
000113         16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
000114         16  CM-LF-DEV-CODE                PIC XXX.
000115         16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
000116         16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
000117         16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
000118         16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
000119         16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
000120         16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
000121         16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
000122         16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
000123         16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
000124         16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
000125         16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
000126         16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
000127         16  cm-temp-epiq                  pic xx.
000128             88  EPIQ-CLASS                  value 'EQ'.
000129*        16  FILLER                        PIC XX.
000130
000131     12  CM-AH-DATA.
000132         16  CM-AH-BENEFIT-CD              PIC XX.
000133         16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
000134         16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
000135         16  CM-AH-DEV-CODE                PIC XXX.
000136         16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
000137         16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
000138         16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
000139         16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
000140         16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
000141         16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
000142         16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
000143         16  CM-AH-PAID-THRU-DT            PIC XX.
000144             88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
000145         16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
000146         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
000147         16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
000148         16  FILLER                        PIC X.
000149
000150     12  CM-LOAN-INFORMATION.
000151         16  CM-LIVES                      PIC S9(7)     COMP-3.
000152         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
000153                                           PIC S9(5)V99  COMP-3.
000154         16  CM-BILLED                     PIC S9(7)     COMP-3.
000155         16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
000156         16  CM-PAY-FREQUENCY              PIC S99.
000157         16  CM-LOAN-TERM                  PIC S999      COMP-3.
000158         16  CM-RATE-CLASS                 PIC XX.
000159         16  CM-BENEFICIARY                PIC X(25).
000160         16  CM-POLICY-FORM-NO             PIC X(12).
000161         16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
000162         16  CM-LAST-ADD-ON-DT             PIC XX.
000163         16  CM-DEDUCTIBLE-AMOUNTS.
000164             20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
000165             20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
000166         16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
000167             20  CM-RESIDENT-STATE         PIC XX.
000168             20  CM-RATE-CODE              PIC X(4).
000169             20  FILLER                    PIC XX.
000170         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
000171             20  CM-LOAN-OFFICER           PIC X(5).
000172             20  FILLER                    PIC XXX.
000173         16  CM-CSR-CODE                   PIC XXX.
000174         16  CM-UNDERWRITING-CODE          PIC X.
000175             88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
000176         16  CM-POST-CARD-IND              PIC X.
000177         16  CM-REF-INTERFACE-SW           PIC X.
000178         16  CM-PREMIUM-TYPE               PIC X.
000179             88  CM-SING-PRM                  VALUE '1'.
000180             88  CM-O-B-COVERAGE              VALUE '2'.
000181             88  CM-OPEN-END                  VALUE '3'.
000182         16  CM-IND-GRP-TYPE               PIC X.
000183             88  CM-INDIVIDUAL                VALUE 'I'.
000184             88  CM-GROUP                     VALUE 'G'.
000185         16  CM-SKIP-CODE                  PIC X.
000186             88  NO-MONTHS-SKIPPED            VALUE SPACE.
000187             88  SKIP-JULY                    VALUE '1'.
000188             88  SKIP-AUGUST                  VALUE '2'.
000189             88  SKIP-SEPTEMBER               VALUE '3'.
000190             88  SKIP-JULY-AUG                VALUE '4'.
000191             88  SKIP-AUG-SEPT                VALUE '5'.
000192             88  SKIP-JULY-AUG-SEPT           VALUE '6'.
000193             88  SKIP-JUNE-JULY-AUG           VALUE '7'.
000194             88  SKIP-JUNE                    VALUE '8'.
000195             88  SKIP-JUNE-JULY               VALUE '9'.
000196             88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
000197             88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
000198         16  CM-PAYMENT-MODE               PIC X.
000199             88  PAY-MONTHLY                  VALUE SPACE.
000200             88  PAY-WEEKLY                   VALUE '1'.
000201             88  PAY-SEMI-MONTHLY             VALUE '2'.
000202             88  PAY-BI-WEEKLY                VALUE '3'.
000203             88  PAY-SEMI-ANUALLY             VALUE '4'.
000204         16  CM-LOAN-NUMBER                PIC X(8).
000205         16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
000206         16  CM-OLD-LOF                    PIC XXX.
000207*        16  CM-LOAN-OFFICER               PIC XXX.
000208         16  CM-REIN-TABLE                 PIC XXX.
000209         16  CM-SPECIAL-REIN-CODE          PIC X.
000210         16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
000211         16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
000212         16  CM-LOAN-1ST-PMT-DT            PIC XX.
000213
000214     12  CM-STATUS-DATA.
000215         16  CM-ENTRY-STATUS               PIC X.
000216         16  CM-ENTRY-DT                   PIC XX.
000217
000218         16  CM-LF-STATUS-AT-CANCEL        PIC X.
000219         16  CM-LF-CANCEL-DT               PIC XX.
000220         16  CM-LF-CANCEL-EXIT-DT          PIC XX.
000221
000222         16  CM-LF-STATUS-AT-DEATH         PIC X.
000223         16  CM-LF-DEATH-DT                PIC XX.
000224         16  CM-LF-DEATH-EXIT-DT           PIC XX.
000225
000226         16  CM-LF-CURRENT-STATUS          PIC X.
000227             88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
000228                                                'M' '4' '5' '9'.
000229             88  CM-LF-NORMAL-ENTRY           VALUE '1'.
000230             88  CM-LF-POLICY-PENDING         VALUE '2'.
000231             88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
000232             88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
000233             88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
000234             88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
000235             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
000236             88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
000237             88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
000238             88  CM-LF-CANCEL-APPLIED         VALUE '8'.
000239             88  CM-LF-IS-REIN-ONLY           VALUE '9'.
000240             88  CM-LF-DECLINED               VALUE 'D'.
000241             88  CM-LF-VOIDED                 VALUE 'V'.
000242
000243         16  CM-AH-STATUS-AT-CANCEL        PIC X.
000244         16  CM-AH-CANCEL-DT               PIC XX.
000245         16  CM-AH-CANCEL-EXIT-DT          PIC XX.
000246
000247         16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
000248         16  CM-AH-SETTLEMENT-DT           PIC XX.
000249         16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
000250
000251         16  CM-AH-CURRENT-STATUS          PIC X.
000252             88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
000253                                                'M' '4' '5' '9'.
000254             88  CM-AH-NORMAL-ENTRY           VALUE '1'.
000255             88  CM-AH-POLICY-PENDING         VALUE '2'.
000256             88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
000257             88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
000258             88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
000259             88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
000260             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
000261             88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
000262             88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
000263             88  CM-AH-CANCEL-APPLIED         VALUE '8'.
000264             88  CM-AH-IS-REIN-ONLY           VALUE '9'.
000265             88  CM-AH-DECLINED               VALUE 'D'.
000266             88  CM-AH-VOIDED                 VALUE 'V'.
000267
000268         16  CM-CLAIM-INTERFACE-SW         PIC X.
000269             88  NO-CLAIM-ATTACHED            VALUE SPACE.
000270             88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
000271             88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
000272         16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
000273
000274         16  CM-ENTRY-BATCH                PIC X(6).
000275         16  CM-LF-EXIT-BATCH              PIC X(6).
000276         16  CM-AH-EXIT-BATCH              PIC X(6).
000277         16  CM-LAST-MONTH-END             PIC XX.
000278
000279     12  CM-NOTE-SW                        PIC X.
000280         88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
000281         88  CERT-NOTES-PRESENT               VALUE '1'.
000282         88  BILLING-NOTES-PRESENT            VALUE '2'.
000283         88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
000284         88  CLAIM-NOTES-PRESENT              VALUE '4'.
000285         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
000286         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
000287         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
000288     12  CM-COMP-EXCP-SW                   PIC X.
000289         88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
000290         88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
000291     12  CM-INSURED-ADDRESS-SW             PIC X.
000292         88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
000293         88  INSURED-ADDR-PRESENT             VALUE '1'.
000294
000295*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
000296     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
000297     12  FILLER                            PIC X.
000298
000299*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
000300     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
000301     12  FILLER                            PIC X.
000302*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
000303     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
000304
000305     12  CM-CREDIT-INTERFACE-SW-1          PIC X.
000306         88  CERT-ADDED-BATCH                 VALUE ' '.
000307         88  CERT-ADDED-ONLINE                VALUE '1'.
000308         88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
000309         88  CERT-PURGED-OFFLINE              VALUE '3'.
000310         88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
000311     12  CM-CREDIT-INTERFACE-SW-2          PIC X.
000312         88  CERT-AS-LOADED                   VALUE ' '.
000313         88  CERT-CANCELLED-ONLINE            VALUE '1'.
000314         88  CERT-CLAIM-ONLINE                VALUE '2'.
000315         88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
000316         88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
000317         88  CERT-PEND-CANCEL-VOID            VALUE '5'.
000318         88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
000319         88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
000320
000321     12  CM-ACCOUNT-COMM-PCTS.
000322         16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
000323         16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
000324
000325     12  CM-USER-FIELD                     PIC X.
000326     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
000327     12  CM-CLP-STATE                      PIC XX.
000328     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
000329     12  CM-USER-RESERVED                  PIC XXX.
000330     12  FILLER REDEFINES CM-USER-RESERVED.
000331         16  CM-AH-CLASS-CD                PIC XX.
000332         16  F                             PIC X.
000333******************************************************************
      *<<((file: ELCCERT))
000698     EJECT
000699*                                    COPY ELCTRLR.
      *>>((file: ELCTRLR))
000001******************************************************************
000002*                                                                *
000003*                            ELCTRLR.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.014                          *
000006*                                                                *
000007*   FILE DESCRIPTION = ACTIVITY TRAILER FILE                     *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 200    RECFORM = FIXED                         *
000011*                                                                *
000012*   BASE CLUSTER NAME = ELTRLR             RKP=2,LEN=22          *
000013*       ALTERNATE INDEX = NONE                                   *
000014*                                                                *
000015*   LOG = YES                                                    *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017******************************************************************
000018*                   C H A N G E   L O G
000019*
000020* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000021*-----------------------------------------------------------------
000022*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000023* EFFECTIVE    NUMBER
000024*-----------------------------------------------------------------
000025* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
000026* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
000027* 050506    2006030600001  AJRA  ADD DENIAL PROOF DATE
000028* 062806    2006030600001  AJRA  ADD PAYMENT PROOF DATE
000029* 080106    2006052500001  AJRA  ADD N AND R NOTE TYPES
000030* 041807    2006032200004  AJRA  ADD APPROVED BY TO PAYMENT
000031* 082807    2007032100001  PEMA  ADD INT RATE TO PMT TRLR
000032* 101807  IR2007100100007  PEMA  EXPAND SIZE OF CLM RESERVE FLDS
000033* 070909    2009060400001  AJRA  ADD AUTO PAY END LETTER
000034* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
000035* 071910  CR2009122800001  PEMA  ADD EOB SWITCHES
000036* 102610    2009122800001  AJRA  ADD STOP DATE TO LETTER
000037* 061511    2011042000002  AJRA  ADD VFY 2ND BENE TO ADDRESS TRAIL
000038* 020413    2012071700001  AJRA  PRINT SURVEY AND PRINT CLM FORM I
000039* 021213    2012092400007  AJRA  CAUSAL STATE SEQUENCE NO
000040* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
000041* 102413  CR2013100800001  AJRA  ADD SPECIAL RELEASE IND
000042* 022614    2013050100003  AJRA  ADD CERT CANCELLED NOTE TYPE - T
000043* 040814    2014030500002  AJRA  ADD ICD CODES
000044* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000045* 013017  CR2016053100001  PEMA  ACH PROCESSING
000046* 062217  CR2017050300002  TANA  ADD AUTH RCVD
000047* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000048* 102418  CR2018083000001  TANA  ADD ADD NEW CALL TYPE
000049******************************************************************
000050 01  ACTIVITY-TRAILERS.
000051     12  AT-RECORD-ID                    PIC XX.
000052         88  VALID-AT-ID                       VALUE 'AT'.
000053
000054     12  AT-CONTROL-PRIMARY.
000055         16  AT-COMPANY-CD               PIC X.
000056         16  AT-CARRIER                  PIC X.
000057         16  AT-CLAIM-NO                 PIC X(7).
000058         16  AT-CERT-NO.
000059             20  AT-CERT-PRIME           PIC X(10).
000060             20  AT-CERT-SFX             PIC X.
000061         16  AT-SEQUENCE-NO              PIC S9(4)     COMP.
000062             88  AT-1ST-TRL-AVAIL             VALUE +4095.
000063             88  AT-LAST-TRL-AVAIL            VALUE +100.
000064             88  AT-RESV-EXP-HIST-TRL         VALUE +0.
000065             88  AT-INSURED-ADDR-TRL          VALUE +1 THRU +9.
000066             88  AT-BENEFICIARY-ADDR-TRL      VALUE +11 THRU +19.
000067             88  AT-ACCOUNT-ADDR-TRL          VALUE +21 THRU +29.
000068             88  AT-PHYSICIAN-ADDR-TRL        VALUE +31 THRU +39.
000069             88  AT-EMPLOYERS-ADDR-TRL        VALUE +41 THRU +49.
000070             88  AT-OTHER-1-ADDR-TRL          VALUE +51 THRU +59.
000071             88  AT-OTHER-2-ADDR-TRL          VALUE +61 THRU +69.
000072             88  AT-DIAGNOSIS-TRL             VALUE +90.
000073             88  AT-BENEFICIARY-TRL           VALUE +91.
000074             88  AT-SPECIAL-REVIEW-TRL        VALUE +92.
000075             88  AT-VFY-2ND-BENE-NOTE-TRL     VALUE +93.
000076             88  AT-VFY-CAUSAL-STATE          VALUE +94.
000077             88  AT-ERROR-MSGS-TRL            VALUE +95.
000078
000079     12  AT-TRAILER-TYPE                 PIC X.
000080         88  RESERVE-EXPENSE-TR               VALUE '1'.
000081         88  PAYMENT-TR                       VALUE '2'.
000082         88  AUTO-PAY-TR                      VALUE '3'.
000083         88  CORRESPONDENCE-TR                VALUE '4'.
000084         88  ADDRESS-TR                       VALUE '5'.
000085         88  GENERAL-INFO-TR                  VALUE '6'.
000086         88  AUTO-PROMPT-TR                   VALUE '7'.
000087         88  DENIAL-TR                        VALUE '8'.
000088         88  INCURRED-CHG-TR                  VALUE '9'.
000089         88  FORM-CONTROL-TR                  VALUE 'A'.
000090
000091     12  AT-RECORDED-DT                  PIC XX.
000092     12  AT-RECORDED-BY                  PIC X(4).
000093     12  AT-LAST-MAINT-HHMMSS            PIC S9(6)     COMP-3.
000094
000095     12  AT-TRAILER-BODY                 PIC X(165).
000096
000097     12  AT-RESERVE-EXPENSE-TR  REDEFINES  AT-TRAILER-BODY.
000098         16  AT-RESERVE-CONTROLS.
000099             20  AT-MANUAL-SW            PIC X.
000100                 88  AT-MANUAL-RESERVES-USED VALUE '1'.
000101             20  AT-FUTURE-SW            PIC X.
000102                 88  AT-FUTURE-RESERVES-USED VALUE '1'.
000103             20  AT-PTC-SW               PIC X.
000104                 88  AT-PAY-TO-CURRENT-USED  VALUE '1'.
000105             20  AT-IBNR-SW              PIC X.
000106                 88  AT-IBNR-RESERVES-USED   VALUE '1'.
000107             20  AT-PTC-LF-SW            PIC X.
000108                 88  AT-LF-PTC-USED          VALUE '1'.
000109             20  AT-CDT-ACCESS-METHOD    PIC X.
000110                 88  AT-CDT-ROUND-NEAR       VALUE '1'.
000111                 88  AT-CDT-ROUND-HIGH       VALUE '2'.
000112                 88  AT-CDT-INTERPOLATED     VALUE '3'.
000113             20  AT-PERCENT-OF-CDT       PIC S9(3)V99    COMP-3.
000114         16  AT-LAST-COMPUTED-DT         PIC XX.
000115         16  AT-FUTURE-RESERVE           PIC S9(7)V99    COMP-3.
000116         16  AT-PAY-CURRENT-RESERVE      PIC S9(7)V99    COMP-3.
000117         16  AT-IBNR-RESERVE             PIC S9(7)V99    COMP-3.
000118         16  AT-INITIAL-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
000119         16  AT-CURRENT-MANUAL-RESERVE   PIC S9(7)V99    COMP-3.
000120         16  AT-ITD-ADDITIONAL-RESERVE   PIC S9(7)V99    COMP-3.
000121         16  AT-EXPENSE-CONTROLS.
000122             20  AT-EXPENSE-METHOD       PIC X.
000123                 88  NO-EXPENSE-CALCULATED    VALUE '1'.
000124                 88  FLAT-DOLLAR-PER-PMT      VALUE '2'.
000125                 88  PERCENT-OF-PMT           VALUE '3'.
000126                 88  DOLLAR-PER-OPEN-MONTH    VALUE '4'.
000127             20  AT-EXPENSE-PERCENT      PIC S9(3)V99    COMP-3.
000128             20  AT-EXPENSE-DOLLAR       PIC S9(3)V99    COMP-3.
000129         16  AT-ITD-PAID-EXPENSES        PIC S9(5)V99    COMP-3.
000130         16  AT-ITD-CHARGEABLE-EXPENSE   PIC S9(5)V99    COMP-3.
000131
000132         16  AT-ITD-LIFE-REFUNDS         PIC S9(5)V99    COMP-3.
000133         16  AT-ITD-AH-REFUNDS           PIC S9(5)V99    COMP-3.
000134
000135*        16  FILLER                      PIC X(53).
000136         16  FILLER                      PIC X(47).
000137
000138         16  AT-RESERVES-LAST-MAINT-DT   PIC XX.
000139         16  AT-RESERVES-LAST-UPDATED-BY PIC X(4).
000140
000141         16  AT-OPEN-CLOSE-HISTORY OCCURS 6 TIMES.
000142             20  AT-OPEN-CLOSE-DATE      PIC XX.
000143             20  AT-OPEN-CLOSE-TYPE      PIC X.
000144*                    C = CLOSED
000145*                    O = OPEN
000146             20  AT-OPEN-CLOSE-REASON    PIC X(5).
000147*                   REASONS = ALTER, AUTO, FINAL, NEW, FORCE
000148
000149     12  AT-PAYMENT-TR  REDEFINES  AT-TRAILER-BODY.
000150         16  AT-PAYMENT-TYPE             PIC X.
000151             88  PARTIAL-PAYMENT                VALUE '1'.
000152             88  FINAL-PAYMENT                  VALUE '2'.
000153             88  LUMP-SUM-PAYMENT               VALUE '3'.
000154             88  ADDITIONAL-PAYMENT             VALUE '4'.
000155             88  CHARGEABLE-EXPENSE             VALUE '5'.
000156             88  NON-CHARGEABLE-EXPENSE         VALUE '6'.
000157             88  VOIDED-PAYMENT                 VALUE '9'.
000158             88  TRANSFER                       VALUE 'T'.
000159             88  LIFE-INTEREST                  VALUE 'I'.
000160
000161         16  AT-CLAIM-TYPE               PIC X.
000162             88  PAID-FOR-AH                    VALUE 'A'.
000163             88  PAID-FOR-LIFE                  VALUE 'L'.
000164             88  PAID-FOR-IUI                   VALUE 'I'.
000165             88  PAID-FOR-GAP                   VALUE 'G'.
000166             88  PAID-FOR-FAM                   VALUE 'F'.
000167             88  PAID-FOR-OTH                   VALUE 'O'.
000168         16  AT-CLAIM-PREM-TYPE          PIC X.
000169             88  AT-SINGLE-PREMIUM              VALUE '1'.
000170             88  AT-O-B-COVERAGE                VALUE '2'.
000171             88  AT-OPEN-END-COVERAGE           VALUE '3'.
000172         16  AT-AMOUNT-PAID              PIC S9(7)V99  COMP-3.
000173         16  AT-CHECK-NO                 PIC X(7).
000174         16  AT-PAID-FROM-DT             PIC XX.
000175         16  AT-PAID-THRU-DT             PIC XX.
000176         16  AT-DAYS-IN-PERIOD           PIC S9(4)     COMP.
000177         16  AT-ACH-PAYMENT              PIC X.
000178*        16  FILLER                      PIC X.
000179         16  AT-PAYEES-NAME              PIC X(30).
000180         16  AT-PAYMENT-ORIGIN           PIC X.
000181             88  ONLINE-MANUAL-PMT              VALUE '1'.
000182             88  ONLINE-AUTO-PMT                VALUE '2'.
000183             88  OFFLINE-PMT                    VALUE '3'.
000184         16  AT-CHECK-WRITTEN-DT         PIC XX.
000185         16  AT-TO-BE-WRITTEN-DT         PIC XX.
000186         16  AT-VOID-DATA.
000187             20  AT-VOID-DT              PIC XX.
000188*00144       20  AT-VOID-REASON          PIC X(30).
000189             20  AT-VOID-REASON          PIC X(26).
000190         16  AT-PMT-APPROVED-BY          PIC X(04).
000191         16  AT-ADDL-RESERVE             PIC S9(5)V99  COMP-3.
000192         16  AT-EXPENSE-PER-PMT          PIC S9(5)V99  COMP-3.
000193         16  AT-INT-RATE REDEFINES AT-EXPENSE-PER-PMT
000194                                         PIC S99V9(5)  COMP-3.
000195         16  AT-CREDIT-INTERFACE.
000196             20  AT-PMT-SELECT-DT        PIC XX.
000197                 88  PAYMENT-NOT-SELECTED  VALUE LOW-VALUE.
000198             20  AT-PMT-ACCEPT-DT        PIC XX.
000199                 88  PAYMENT-NOT-ACCEPTED  VALUE LOW-VALUE.
000200             20  AT-VOID-SELECT-DT       PIC XX.
000201                 88  VOID-NOT-SELECTED     VALUE LOW-VALUE.
000202             20  AT-VOID-ACCEPT-DT       PIC XX.
000203                 88  VOID-NOT-ACCEPTED     VALUE LOW-VALUE.
000204
000205         16  AT-CHECK-QUE-CONTROL        PIC S9(8)     COMP.
000206                 88  PAYMENT-NOT-QUEUED           VALUE ZERO.
000207                 88  CONVERSION-PAYMENT           VALUE +99999999.
000208         16  AT-CHECK-QUE-SEQUENCE       PIC S9(4)     COMP.
000209
000210         16  AT-FORCE-CONTROL            PIC X.
000211             88  PAYMENT-WAS-FORCED           VALUE '1'.
000212         16  AT-PREV-LAST-PMT-DT         PIC XX.
000213         16  AT-PREV-PAID-THRU-DT        PIC XX.
000214         16  AT-PREV-LAST-PMT-AMT        PIC S9(7)V99  COMP-3.
000215         16  AT-ELIMINATION-DAYS         PIC S999      COMP-3.
000216         16  AT-DAILY-RATE               PIC S9(3)V99  COMP-3.
000217         16  AT-BENEFIT-TYPE             PIC X.
000218
000219         16  AT-EXPENSE-TYPE             PIC X.
000220         16  AT-PAYMENT-APPROVAL-SW      PIC X.
000221
000222         16  AT-PAYEE-TYPE-CD.
000223             20  AT-PAYEE-TYPE           PIC X.
000224                 88  INSURED-PAID           VALUE 'I'.
000225                 88  BENEFICIARY-PAID       VALUE 'B'.
000226                 88  ACCOUNT-PAID           VALUE 'A'.
000227                 88  OTHER-1-PAID           VALUE 'O'.
000228                 88  OTHER-2-PAID           VALUE 'Q'.
000229                 88  DOCTOR-PAID            VALUE 'P'.
000230                 88  EMPLOYER-PAID          VALUE 'E'.
000231             20  AT-PAYEE-SEQ            PIC X.
000232
000233         16  AT-CASH-PAYMENT             PIC X.
000234         16  AT-GROUPED-PAYMENT          PIC X.
000235         16  AT-PAYMENT-NOTE-SEQ-NO      PIC S9(4)       COMP.
000236         16  AT-APPROVAL-LEVEL-REQD      PIC X.
000237         16  AT-APPROVED-LEVEL           PIC X.
000238         16  AT-VOID-TYPE                PIC X.
000239             88  AT-PAYMENT-WAS-STOPPED     VALUE 'S'.
000240             88  AT-PAYMENT-WAS-VOIDED      VALUE 'V'.
000241         16  AT-AIG-UNEMP-IND            PIC X.
000242             88  AT-AIG-UNEMPLOYMENT-PMT    VALUE 'U'.
000243         16  AT-ASSOCIATES               PIC X.
000244             88  AT-AIG-INTERFACE           VALUE 'I' 'N'.
000245             88  AT-AIG-NON-INTERFACE       VALUE 'A' 'M'.
000246
000247         16  AT-FORM-CTL-SEQ-NO          PIC S9(4)       COMP.
000248         16  AT-CV-PMT-CODE              PIC X.
000249             88  FULL-DEATH-PAYMENT         VALUE '1'.
000250             88  HALF-DEATH-PAYMENT         VALUE '2'.
000251             88  FULL-ADD-PAYMENT           VALUE '3'.
000252             88  HALF-ADD-PAYMENT           VALUE '4'.
000253             88  FULL-RIDER-PAYMENT         VALUE '5'.
000254             88  HALF-RIDER-PAYMENT         VALUE '6'.
000255             88  NON-CHG-EXP-PAYMENT        VALUE '7'.
000256             88  ADDL-PAYMENT               VALUE '8'.
000257
000258         16  AT-EOB-CODE1                PIC XXX.
000259         16  AT-EOB-CODE2                PIC XXX.
000260         16  AT-EOB-CODE3                PIC XXX.
000261         16  FILLER REDEFINES AT-EOB-CODE3.
000262             20  AT-PRINT-CLM-FORM       PIC X.
000263             20  AT-PRINT-SURVEY         PIC X.
000264             20  AT-SPECIAL-RELEASE      PIC X.
000265         16  AT-EOB-CODE4                PIC XXX.
000266         16  FILLER REDEFINES AT-EOB-CODE4.
000267             20  AT-INT-PMT-SELECT-DT    PIC XX.
000268             20  FILLER                  PIC X.
000269         16  AT-EOB-CODE5                PIC XXX.
000270         16  FILLER REDEFINES AT-EOB-CODE5.
000271             20  AT-PMT-PROOF-DT         PIC XX.
000272             20  FILLER                  PIC X.
000273
000274         16  AT-PRINT-EOB-WITH-CHECK     PIC X.
000275             88  AT-PRINT-EOB            VALUE 'Y'.
000276
000277         16  AT-PAYMENT-LAST-MAINT-DT    PIC XX.
000278         16  AT-PAYMENT-LAST-UPDATED-BY  PIC X(4).
000279
000280     12  AT-AUTO-PAY-TR  REDEFINES  AT-TRAILER-BODY.
000281         16  AT-SCHEDULE-START-DT        PIC XX.
000282         16  AT-SCHEDULE-END-DT          PIC XX.
000283         16  AT-TERMINATED-DT            PIC XX.
000284         16  AT-LAST-PMT-TYPE            PIC X.
000285             88  LAST-PMT-IS-FINAL              VALUE 'F'.
000286             88  LAST-PMT-IS-PARTIAL            VALUE 'P'.
000287         16  AT-FIRST-PMT-DATA.
000288             20  AT-FIRST-PMT-AMT        PIC S9(7)V99  COMP-3.
000289             20  AT-DAYS-IN-1ST-PMT      PIC S9(4)     COMP.
000290             20  AT-1ST-PAY-THRU-DT      PIC XX.
000291         16  AT-REGULAR-PMT-DATA.
000292             20  AT-REGULAR-PMT-AMT      PIC S9(7)V99  COMP-3.
000293             20  AT-DAYS-IN-REG-PMT      PIC S9(4)     COMP.
000294             20  AT-INTERVAL-MONTHS      PIC S9(4)     COMP.
000295         16  AT-AUTO-PAYEE-CD.
000296             20  AT-AUTO-PAYEE-TYPE      PIC X.
000297                 88  INSURED-PAID-AUTO      VALUE 'I'.
000298                 88  BENEFICIARY-PAID-AUTO  VALUE 'B'.
000299                 88  ACCOUNT-PAID-AUTO      VALUE 'A'.
000300                 88  OTHER-1-PAID-AUTO      VALUE 'O'.
000301                 88  OTHER-2-PAID-AUTO      VALUE 'Q'.
000302             20  AT-AUTO-PAYEE-SEQ       PIC X.
000303         16  AT-AUTO-PAY-DAY             PIC 99.
000304         16  AT-AUTO-CASH                PIC X.
000305             88  AT-CASH                      VALUE 'Y'.
000306             88  AT-NON-CASH                  VALUE 'N'.
000307*        16  FILLER                      PIC X(129).
000308         16  AT-AUTO-END-LETTER          PIC X(4).
000309         16  FILLER                      PIC X(125).
000310
000311         16  AT-AUTO-PAY-LAST-MAINT-DT   PIC XX.
000312         16  AT-AUTO-PAY-LAST-UPDATED-BY PIC X(4).
000313
000314     12  AT-CORRESPONDENCE-TR  REDEFINES  AT-TRAILER-BODY.
000315         16  AT-LETTER-SENT-DT           PIC XX.
000316         16  AT-RECEIPT-FOLLOW-UP        PIC XX.
000317         16  AT-AUTO-RE-SEND-DT          PIC XX.
000318         16  AT-LETTER-ANSWERED-DT       PIC XX.
000319         16  AT-LETTER-ARCHIVE-NO        PIC S9(8)     COMP.
000320         16  AT-LETTER-ORIGIN            PIC X.
000321             88  ONLINE-CREATION              VALUE '1' '3'.
000322             88  OFFLINE-CREATION             VALUE '2' '4'.
000323             88  NAPER-ONLINE-CREATION        VALUE '3'.
000324             88  NAPER-OFFLINE-CREATION       VALUE '4'.
000325         16  AT-STD-LETTER-FORM          PIC X(4).
000326         16  AT-REASON-TEXT              PIC X(70).
000327         16  AT-ADDRESS-REC-SEQ-NO       PIC S9(4)     COMP.
000328         16  AT-ADDRESEE-TYPE            PIC X.
000329              88  INSURED-ADDRESEE            VALUE 'I'.
000330              88  BENEFICIARY-ADDRESEE        VALUE 'B'.
000331              88  ACCOUNT-ADDRESEE            VALUE 'A'.
000332              88  PHYSICIAN-ADDRESEE          VALUE 'P'.
000333              88  EMPLOYER-ADDRESEE           VALUE 'E'.
000334              88  OTHER-ADDRESEE-1            VALUE 'O'.
000335              88  OTHER-ADDRESEE-2            VALUE 'Q'.
000336         16  AT-ADDRESSEE-NAME           PIC X(30).
000337         16  AT-INITIAL-PRINT-DATE       PIC XX.
000338         16  AT-RESEND-PRINT-DATE        PIC XX.
000339         16  AT-CORR-SOL-UNSOL           PIC X.
000340         16  AT-LETTER-PURGED-DT         PIC XX.
000341*
000342*FOLLOWING CID CHGS REENTERED AS DMD CHGS OVERLAID THEM.
000343*
000344         16  AT-CSO-REDEFINITION.
000345             20  AT-RESEND-LETTER-FORM   PIC X(4).
000346             20  AT-AUTO-CLOSE-IND       PIC X(1).
000347             20  AT-LETTER-TO-BENE       PIC X(1).
000348             20  AT-STOP-LETTER-DT       PIC X(2).
000349             20  AT-AUTH-RCVD            PIC X(1).
000350             20  FILLER                  PIC X(18).
000351*             20  FILLER                  PIC X(27).
000352             20  AT-CSO-LETTER-STATUS    PIC X.
000353                 88  AT-CSO-LETTER-ONLINE    VALUE '1'.
000354                 88  AT-CSO-LETTER-PURGED    VALUE '2'.
000355                 88  AT-CSO-LETTER-RELOADED  VALUE '3'.
000356             20  AT-CSO-LETTER-PURGE-DATE   PIC XX.
000357             20  AT-CSO-LETTER-RELOAD-DATE  PIC XX.
000358*
000359*FOLLOWING DMD CHGS COMMENTED OUT AS THEY OVERLAY CID MODS NEEDED
000360*
000361*        16  FILLER                      PIC X(26).
000362*
000363*        16  AT-DMD-BSR-CODE             PIC X.
000364*            88  AT-AUTOMATED-BSR              VALUE 'A'.
000365*            88  AT-NON-AUTOMATED-BSR          VALUE 'B' ' '.
000366*
000367*        16  AT-DMD-LETTER-STATUS        PIC X.
000368*            88  AT-DMD-LETTER-ONLINE          VALUE '1'.
000369*            88  AT-DMD-LETTER-PURGED          VALUE '2'.
000370*            88  AT-DMD-LETTER-RELOADED        VALUE '3'.
000371*        16  AT-DMD-LETTER-PURGE-DT      PIC XX.
000372*        16  AT-DMD-LETTER-RELOAD-DT     PIC XX.
000373
000374         16  AT-CORR-LAST-MAINT-DT       PIC XX.
000375         16  AT-CORR-LAST-UPDATED-BY     PIC X(4).
000376
000377     12  AT-ADDRESS-TR  REDEFINES  AT-TRAILER-BODY.
000378         16  AT-ADDRESS-TYPE             PIC X.
000379             88  INSURED-ADDRESS               VALUE 'I'.
000380             88  BENEFICIARY-ADDRESS           VALUE 'B'.
000381             88  ACCOUNT-ADDRESS               VALUE 'A'.
000382             88  PHYSICIAN-ADDRESS             VALUE 'P'.
000383             88  EMPLOYER-ADDRESS              VALUE 'E'.
000384             88  OTHER-ADDRESS-1               VALUE 'O'.
000385             88  OTHER-ADDRESS-2               VALUE 'Q'.
000386         16  AT-MAIL-TO-NAME             PIC X(30).
000387         16  AT-ADDRESS-LINE-1           PIC X(30).
000388         16  AT-ADDRESS-LINE-2           PIC X(30).
000389         16  AT-CITY-STATE.
000390             20  AT-CITY                 PIC X(28).
000391             20  AT-STATE                PIC XX.
000392         16  AT-ZIP.
000393             20  AT-ZIP-CODE.
000394                 24  AT-ZIP-1ST          PIC X.
000395                     88  AT-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000396                 24  FILLER              PIC X(4).
000397             20  AT-ZIP-PLUS4            PIC X(4).
000398         16  AT-CANADIAN-POSTAL-CODE  REDEFINES  AT-ZIP.
000399             20  AT-CAN-POSTAL-1         PIC XXX.
000400             20  AT-CAN-POSTAL-2         PIC XXX.
000401             20  FILLER                  PIC XXX.
000402         16  AT-PHONE-NO                 PIC 9(11)     COMP-3.
000403*         16  FILLER                      PIC X(23).
000404         16  AT-VFY-2ND-BENE-SSN         PIC X(9).
000405         16  AT-VFY-2ND-BENE-VERIFIED    PIC X.
000406         16  FILLER                      PIC X(13).
000407         16  AT-ADDRESS-LAST-MAINT-DT    PIC XX.
000408         16  AT-ADDRESS-LAST-UPDATED-BY  PIC X(4).
000409
000410     12  AT-GENERAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
000411         16  AT-INFO-LINE-1              PIC X(60).
000412         16  FILLER REDEFINES AT-INFO-LINE-1.
000413             20  AT-NOTE-ERROR-NO OCCURS 15
000414                                         PIC X(4).
000415         16  AT-INFO-LINE-2              PIC X(60).
000416         16  FILLER REDEFINES AT-INFO-LINE-2.
000417             20  AT-ICD-CODE-1           PIC X(8).
000418             20  AT-ICD-CODE-2           PIC X(8).
000419             20  FILLER                  PIC X(44).
000420         16  AT-INFO-TRAILER-TYPE        PIC X.
000421             88  AT-ERRORS-NOTE          VALUE 'E'.
000422             88  AT-PAYMENT-NOTE         VALUE 'P'.
000423             88  AT-CALL-NOTE            VALUE 'C'.
000424             88  AT-MAINT-NOTE           VALUE 'M'.
000425             88  AT-CERT-CHANGE          VALUE 'X'.
000426             88  AT-APPROVAL-NOTE        VALUE 'R'.
000427             88  AT-NOTE-FILE-NOTE       VALUE 'N'.
000428             88  AT-CERT-CANCELLED       VALUE 'T'.
000429         16  AT-CALL-TYPE                PIC X.
000430             88  AT-PHONE-CALL-IN        VALUE 'I'.
000431             88  AT-PHONE-CALL-NEW       VALUE 'N'.
000432             88  AT-PHONE-CALL-OUT       VALUE 'O'.
000433         16  AT-NOTE-CONTINUATION        PIC X.
000434             88  AT-CONTINUED-NOTE       VALUE 'X'.
000435         16  AT-EOB-CODES-EXIST          PIC X.
000436             88  AT-EOB-CODES-PRESENT    VALUE 'Y'.
000437         16  FILLER                      PIC X(35).
000438         16  AT-GEN-INFO-LAST-MAINT-DT   PIC XX.
000439         16  AT-GEN-INFO-LAST-UPDATED-BY PIC X(4).
000440
000441     12  AT-AUTO-PROMPT-TR  REDEFINES  AT-TRAILER-BODY.
000442         16  AT-PROMPT-LINE-1            PIC X(60).
000443         16  AT-PROMPT-LINE-2            PIC X(60).
000444         16  AT-PROMPT-START-DT          PIC XX.
000445         16  AT-PROMPT-END-DT            PIC XX.
000446         16  FILLER                      PIC X(35).
000447         16  AT-PROMPT-LAST-MAINT-DT     PIC XX.
000448         16  AT-PROMPT-LAST-UPDATED-BY   PIC X(4).
000449
000450     12  AT-DENIAL-INFO-TR  REDEFINES  AT-TRAILER-BODY.
000451         16  AT-DENIAL-INFO-1            PIC X(60).
000452         16  AT-DENIAL-INFO-2            PIC X(60).
000453         16  AT-DENIAL-DT                PIC XX.
000454         16  AT-RETRACTION-DT            PIC XX.
000455         16  AT-DENIAL-REASON-CODE       PIC X(4).
000456*         16  FILLER                      PIC X(31).
000457         16  AT-DENIAL-PROOF-DT          PIC XX.
000458         16  FILLER                      PIC X(29).
000459         16  AT-DENIAL-LAST-MAINT-DT     PIC XX.
000460         16  AT-DENIAL-LAST-UPDATED-BY   PIC X(4).
000461
000462     12  AT-INCURRED-CHG-TR  REDEFINES  AT-TRAILER-BODY.
000463         16  AT-OLD-INCURRED-DT          PIC XX.
000464         16  AT-OLD-REPORTED-DT          PIC XX.
000465         16  AT-OLD-ESTABLISHED-DT       PIC XX.
000466         16  AT-OLD-TOTAL-PAID           PIC S9(7)V99     COMP-3.
000467         16  AT-OLD-DAYS-PAID            PIC S9(4)        COMP.
000468         16  AT-OLD-NO-OF-PMTS           PIC S9(3)        COMP-3.
000469         16  AT-OLD-PAID-THRU-DT         PIC XX.
000470         16  AT-LAST-PMT-MADE-DT         PIC XX.
000471         16  FILLER                      PIC X(26).
000472         16  AT-OLD-DIAG-CODE            PIC X(6).
000473         16  AT-TRAILER-CNT-AT-CHG       PIC S9(4)        COMP.
000474         16  AT-OLD-ITD-PAID-EXPENSE     PIC S9(5)V99     COMP-3.
000475         16  AT-OLD-CHARGABLE-EXPENSE    PIC S9(5)V99     COMP-3.
000476         16  AT-OLD-INIT-MAN-RESV        PIC S9(7)V99     COMP-3.
000477         16  AT-OLD-CURRENT-MAN-RESV     PIC S9(7)V99     COMP-3.
000478         16  AT-OLD-ADDL-MAN-RESV        PIC S9(7)V99     COMP-3.
000479         16  AT-OLD-DIAG-DESCRIP         PIC X(60).
000480         16  AT-OLD-ICD-CODE-1           PIC X(8).
000481         16  AT-OLD-ICD-CODE-2           PIC X(8).
000482         16  FILLER                      PIC X(9).
000483         16  AT-INCURRED-LAST-UPDATED-BY PIC X(4).
000484
000485     12  AT-FORM-CONTROL-TR  REDEFINES  AT-TRAILER-BODY.
000486         16  AT-FORM-SEND-ON-DT          PIC XX.
000487         16  AT-FORM-FOLLOW-UP-DT        PIC XX.
000488         16  AT-FORM-RE-SEND-DT          PIC XX.
000489         16  AT-FORM-ANSWERED-DT         PIC XX.
000490         16  AT-FORM-PRINTED-DT          PIC XX.
000491         16  AT-FORM-REPRINT-DT          PIC XX.
000492         16  AT-FORM-TYPE                PIC X.
000493             88  INITIAL-FORM                  VALUE '1'.
000494             88  PROGRESS-FORM                 VALUE '2'.
000495         16  AT-INSTRUCT-LN-1            PIC X(28).
000496         16  AT-INSTRUCT-LN-2            PIC X(28).
000497         16  AT-INSTRUCT-LN-3            PIC X(28).
000498         16  AT-FORM-ADDR-SEQ-NO         PIC S9(4)      COMP.
000499         16  AT-FORM-ADDRESS             PIC X.
000500             88  FORM-TO-INSURED              VALUE 'I'.
000501             88  FORM-TO-ACCOUNT              VALUE 'A'.
000502             88  FORM-TO-OTHER-1              VALUE 'O'.
000503             88  FORM-TO-OTHER-2              VALUE 'Q'.
000504         16  AT-RELATED-1.
000505             20 AT-REL-CARR-1            PIC X.
000506             20 AT-REL-CLAIM-1           PIC X(7).
000507             20 AT-REL-CERT-1            PIC X(11).
000508         16  AT-RELATED-2.
000509             20 AT-REL-CARR-2            PIC X.
000510             20 AT-REL-CLAIM-2           PIC X(7).
000511             20 AT-REL-CERT-2            PIC X(11).
000512         16  AT-EMP-FORM-SEND-ON-DT      PIC XX.
000513         16  AT-PHY-FORM-SEND-ON-DT      PIC XX.
000514         16  AT-EMP-FORM-ANSWERED-DT     PIC XX.
000515         16  AT-PHY-FORM-ANSWERED-DT     PIC XX.
000516         16  AT-FORM-REM-PRINT-DT        PIC XX.
000517         16  AT-STOP-FORM-DT             PIC X(2).
000518
000519         16  FILLER                      PIC X(09).
000520         16  AT-FORM-LAST-MAINT-DT       PIC XX.
000521         16  AT-FORM-LAST-UPDATED-BY     PIC X(4).
000522******************************************************************
      *<<((file: ELCTRLR))
000700     EJECT
000701*                                    COPY ELCACTQ.
      *>>((file: ELCACTQ))
000001******************************************************************
000002*                                                                *
000003*                            ELCACTQ.                            *
000004*           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.004                          *
000006*                                                                *
000007*   FILE DESCRIPTION = ACTIVITY QUE FILE                         *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 60     RECFORM = FIXED                         *
000011*                                                                *
000012*   BASE CLUSTER NAME = ELACTQ             RKP=2,LEN=20          *
000013*       ALTERNATE INDEX = NONE                                   *
000014*                                                                *
000015*   LOG = YES                                                    *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017*                                                                *
000018*  NO  CID  MODS  IN  COPYBOOK  ELCACTQ                          *
000019******************************************************************
000020
000021 01  ACTIVITY-QUE.
000022     12  AQ-RECORD-ID                PIC XX.
000023         88  VALID-AQ-ID                VALUE 'AQ'.
000024
000025     12  AQ-CONTROL-PRIMARY.
000026         16  AQ-COMPANY-CD           PIC X.
000027         16  AQ-CARRIER              PIC X.
000028         16  AQ-CLAIM-NO             PIC X(7).
000029         16  AQ-CERT-NO.
000030             20  AQ-CERT-PRIME       PIC X(10).
000031             20  AQ-CERT-SFX         PIC X.
000032
000033     12  AQ-PENDING-ACTIVITY-FLAGS.
000034         88  NO-PENDING-ACTIVITY        VALUE SPACES.
000035         16  AQ-PENDING-PAYMENT-FLAG PIC X.
000036             88  PENDING-PAYMENTS       VALUE '1'.
000037         16  AQ-PENDING-STATUS-FLAG  PIC X.
000038             88  PENDING-FULL-PRINT     VALUE '1'.
000039             88  PENDING-PART-PRINT     VALUE '2'.
000040         16  AQ-PENDING-LETTER-FLAG  PIC X.
000041             88  PENDING-LETTERS        VALUE '1'.
000042         16  AQ-PENDING-CLAIM-RESTORE PIC X.
000043             88  PENDING-RESTORE        VALUE 'C'.
000044             88  PENDING-RESTORE-LETTER VALUE 'L'.
000045
000046     12  FILLER                      PIC X(20).
000047
000048     12  AQ-RESEND-DATE              PIC XX.
000049     12  AQ-FOLLOWUP-DATE            PIC XX.
000050     12  AQ-PAYMENT-COUNTER          PIC S9        COMP-3.
000051     12  AQ-PMT-UNAPPROVED-COUNT     PIC S9        COMP-3.
000052     12  AQ-AUTO-LETTER              PIC X(4).
000053     12  FILLER                      PIC XX.
000054     12  AQ-LAST-UPDATED-BY          PIC S9(4)     COMP.
000055*****************************************************************
      *<<((file: ELCACTQ))
000702     EJECT
000703*                                    COPY ELCCHKQ.
      *>>((file: ELCCHKQ))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCCHKQ.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.007                          *
000007*                                                                *
000008*   FILE DESCRIPTION = CHECK QUE FILE                            *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 100  RECFORM = FIXED                           *
000012*                                                                *
000013*   BASE CLUSTER = ELCHKQ                         RKP=2,LEN=7    *
000014*       ALTERNATE PATH1 = ELCHKQ2 (BY PAYEE)      RKP=9,LEN=26   *
000015*                                                                *
000016*   LOG = YES                                                    *
000017*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000018******************************************************************
000019 01  CHECK-QUE.
000020     12  CQ-RECORD-ID                PIC XX.
000021         88  VALID-CQ-ID         VALUE 'CQ'.
000022
000023     12  CQ-CONTROL-PRIMARY.
000024         16  CQ-COMPANY-CD           PIC X.
000025         16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
000026         16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
000027
000028     12  CQ-CONTROL-BY-PAYEE.
000029         16  CQ-CONTROL-BY-NUMBER.
000030             20  CQ-COMPANY-CD-A1     PIC X.
000031             20  CQ-CONTROL-NUMBER-A1 PIC S9(8)      COMP.
000032         16  CQ-PAYEE-CARRIER        PIC X.
000033         16  CQ-PAYEE-GROUPING       PIC X(6).
000034         16  CQ-PAYEE-STATE          PIC XX.
000035         16  CQ-PAYEE-BENE-ACCT      PIC X(10).
000036         16  CQ-SEQUENCE-NUMBER-A1   PIC S9(4)       COMP.
000037
000038     12  CQ-DMD-CONTROL  REDEFINES  CQ-CONTROL-BY-PAYEE.
000039         16  CQ-DMD-COMPANY-CD-A2    PIC X.
000040         16  CQ-DMD-PAYEE-TYPE-A2    PIC X.
000041         16  CQ-DMD-BENE-CODE-A2     PIC X(10).
000042         16  CQ-DMD-CLAIM-NO-A2      PIC X(7).
000043         16  CQ-DMD-TIME-SEQ-A2      PIC S9(7)       COMP.
000044         16  FILLER                  PIC X(3).
000045
000046     12  CQ-ENTRY-TYPE               PIC X.
000047             88  CHECK-ON-QUE           VALUE 'Q'.
000048             88  ALIGNMENT-CHECK        VALUE 'A'.
000049             88  SPOILED-CHECK          VALUE 'S'.
000050             88  PAYMENT-ABORTED        VALUE 'X'.
000051
000052     12  CQ-CLAIM-MAST-CNTL.
000053         16  CQ-CARRIER              PIC X.
000054         16  CQ-CLAIM-NO             PIC X(7).
000055         16  CQ-CERT-NO.
000056             20  CQ-CERT-PRIME       PIC X(10).
000057             20  CQ-CERT-SFX         PIC X.
000058         16  CQ-CLAIM-TYPE           PIC X.
000059             88  CQ-LIFE-CLAIM          VALUE 'L'.
000060             88  CQ-AH-CLAIM            VALUE 'A'.
000061         16  CQ-CLAIM-SUB-TYPE       PIC X.
000062             88  CQ-FIXED-COVERAGE      VALUE '1'.
000063             88  CQ-O-B-COVERAGE        VALUE '2'.
000064             88  CQ-OPEN-END-COVERAGE   VALUE '3'.
000065
000066     12  CQ-PMT-TRLR-SEQUENCE        PIC S9(4)       COMP.
000067     12  CQ-CHECK-NUMBER             PIC X(7).
000068     12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
000069     12  CQ-PAYMENT-TYPE             PIC X.
000070             88  CQ-PARTIAL-PAYMENT        VALUE '1'.
000071             88  CQ-FINAL-PAYMENT          VALUE '2'.
000072             88  CQ-LUMP-SUM-PAYMENT       VALUE '3'.
000073             88  CQ-ADDITIONAL-PAYMENT     VALUE '4'.
000074             88  CQ-CHARGEABLE-EXPENSE     VALUE '5'.
000075             88  CQ-NON-CHARGEABLE-EXPENSE VALUE '6'.
000076             88  CQ-LIFE-PREMIUM-REFUND    VALUE '7'.
000077             88  CQ-AH-PREMIUM-REFUND      VALUE '8'.
000078     12  CQ-VOID-INDICATOR           PIC X.
000079             88  CHECK-IS-STOPPED          VALUE 'S'.
000080             88  CHECK-IS-VOID             VALUE 'V'.
000081     12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
000082     12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
000083     12  CQ-CHECK-BY-USER            PIC X(4).
000084     12  CQ-PRE-NUMBERING-SW         PIC X.
000085       88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
000086       88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
000087
000088     12  CQ-CHECK-WRITTEN-DT         PIC XX.
000089     12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
000090     12  CQ-LEDGER-FLAG              PIC X(01).
000091     12  CQ-VOID-AFTER-LEDGER        PIC X(01).
000092     12  CQ-LAST-UPDATED-DT          PIC XX.
000093     12  CQ-LAST-UPDATED-HHMMSS      PIC S9(6)       COMP-3.
000094     12  CQ-APPLIED-TO-RCON-DT       PIC XX.
000095
000096     12  FILLER                      PIC X(04).
000097
000098******************************************************************
      *<<((file: ELCCHKQ))
000704     EJECT
000705*                                    COPY ELCRCON.
      *>>((file: ELCRCON))
000001******************************************************************
000002*                                                                *
000003*
000004*   THIS COPYBOOK IS NOT BEING USED IN LOGIC
000005*
000006*
000007*                                                                *
000008*                            ELCRCON.                            *
000009*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000010*                            VMOD=2.003                          *
000011*                                                                *
000012*   FILE DESCRIPTION = CHECK RECONCILIATION FILE                 *
000013*                                                                *
000014*   FILE TYPE = VSAM,KSDS                                        *
000015*   RECORD SIZE = 194  RECFORM = FIXED                           *
000016*                                                                *
000017*   BASE CLUSTER = ELRCON                         RKP=2,LEN=19   *
000018*                                                                *
000019*   LOG = YES                                                    *
000020*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000021******************************************************************
000022
000023 01  CHECK-RECONCILIATION.
000024     12  RC-RECORD-ID                          PIC XX.
000025         88  VALID-RC-ID                    VALUE 'RC'.
000026     12  RC-CONTROL-PRIMARY.
000027         16  RC-COMPANY-CD                     PIC X.
000028         16  RC-CHECK-NO                       PIC X(7).
000029         16  RC-CHECK-ORIGIN                   PIC X.
000030         16  RC-GL-ACCOUNT-NO                  PIC X(10).
000031
000032     12  RC-CHECK-DATA.
000033         16  RC-ISSUE-DATE.
000034             20  RC-ISSUE-YYYY.
000035               22  RC-ISSUE-CC                 PIC XX.
000036               22  RC-ISSUE-YY                 PIC XX.
000037             20  RC-ISSUE-MM                   PIC XX.
000038             20  RC-ISSUE-DD                   PIC XX.
000039         16  RC-CHECK-AMOUNT                   PIC 9(7)V99.
000040         16  RC-CARRIER                        PIC X.
000041         16  RC-CLAIM-NO                       PIC X(7).
000042         16  RC-REFERENCE-NO                   PIC X(20).
000043         16  RC-MORTGAGE-REF   REDEFINES  RC-REFERENCE-NO.
000044             20  RC-MORT-REF-1-18              PIC X(18).
000045             20  RC-MORT-REF-SUF-19-20         PIC XX.
000046         16  FILLER  REDEFINES  RC-MORTGAGE-REF.
000047             20  FILLER                        PIC X(13).
000048             20  RC-CLAIM-REF.
000049               22  RC-CLAIM-PREFIX             PIC X.
000050               22  RC-CLAIM-NO-REF             PIC X(6).
000051         16  RC-COVERAGE-TYPE                  PIC X.
000052         16  RC-BENEFIT-CODE                   PIC XX.
000053         16  RC-BENEFICIARY                    PIC X(10).
000054         16  RC-PAYMENT-TYPE                   PIC X.
000055         16  RC-STATUS                         PIC X.
000056           88  RC-STATUS-ABANDONED                VALUE 'A'.
000057           88  RC-STATUS-DESTROYED                VALUE 'D'.
000058           88  RC-STATUS-OUTSTANDING              VALUE 'O'.
000059           88  RC-STATUS-REDEEMED                 VALUE 'R'.
000060           88  RC-STATUS-STOP-PAY                 VALUE 'S'.
000061           88  RC-STATUS-UNREDEEMED               VALUE 'U'.
000062           88  RC-STATUS-VOIDED                   VALUE 'V'.
000063         16  RC-STATUS-DATE.
000064             20  RC-STATUS-YYYY.
000065               22  RC-STATUS-CC                PIC XX.
000066               22  RC-STATUS-YY                PIC XX.
000067             20  RC-STATUS-MM                  PIC XX.
000068             20  RC-STATUS-DD                  PIC XX.
000069
000070     12  RC-MAINT-AREA.
000071         16  RC-LAST-MAINT-BY                  PIC X(4).
000072         16  RC-LAST-MAINT-DT                  PIC XX.
000073         16  RC-LAST-MAINT-HHMMSS    COMP-3    PIC S9(7).
000074
000075     12  RC-CHECK-MAINT-AREA.
000076         16  RC-LAST-CHECK-BY                  PIC X(4).
000077         16  RC-LAST-CHECK-DT                  PIC XX.
000078         16  RC-LAST-CHECK-HHMMSS    COMP-3    PIC S9(7).
000079
000080     12  RC-CASHED-AMOUNT                      PIC 9(7)V99.
000081
000082     12  RC-CHECK-NOTE                         PIC X(67).
000083     12  FILLER                                PIC X(11).
000084
      *<<((file: ELCRCON))
000706     EJECT
000707*                                    COPY ERCDMDNT.
000708*    EJECT
000709*                                    COPY ELCDAR.
      *>>((file: ELCDAR))
000001******************************************************************
000002*                                                                *
000003*   FILE DESC. = DAILY ACTIVITY FILE, FOR PROCESSING NITELY      *
000004*   FILE TYPE = VSAM,KSDS                                        *
000005*   RECORD SIZE = 25   RECFORM = FIXED                           *
000006*   BASE CLUSTER = DLYACTV                                       *
000007*   LOG = YES                                                    *
000008*   NARRATIVE - FILE IS BUILT DURING DAYTIME CICS PROCESSING AND *
000009*               IS THEN PROCESSED BY CYCLE PROCESSING AT NIGHT.  *
000010*               THIS IS USED TO BUILD THE LOGIC "F" EXTRACT      *
000011*               RECORDS FOR THOSE CLAIMS WHICH HAVE HAD ACTIVITY *
000012*               DURING THE DAY. THE EXTRACTS THEN GET READ IN    *
000013*               BY PROGRAM "LGINFCE".                            *
000014*                                                                *
000015******************************************************************
000016 01  DAILY-ACTIVITY-RECORD.
000017     05  DA-KEY.
000018         10  DA-COMP-CD          PIC X.
000019         10  DA-CARRIER          PIC X.
000020         10  DA-CLAIM-NO         PIC X(7).
000021         10  DA-CERT-NO.
000022             15  DA-CERT-PRIME   PIC X(10).
000023             15  DA-CERT-SFX     PIC X.
000024     05  DA-TRAILER-SEQ-NO       PIC S9(4)  COMP.
000025     05  DA-RECORD-TYPE          PIC X.
000026     05  FILLER                  PIC X(2).
000027******************************************************************
      *<<((file: ELCDAR))
000710*                                    copy ELCCRTT.
      *>>((file: ELCCRTT))
000001******************************************************************
000002*                                                                *
000003*                            ELCCRTT.                            *
000004*                                                                *
000005*   FILE DESCRIPTION = CERTIFICATE TRAILERS                      *
000006*                                                                *
000007*   FILE TYPE = VSAM,KSDS                                        *
000008*   RECORD SIZE = 552  RECFORM = FIXED                           *
000009*                                                                *
000010*   BASE CLUSTER = ELCRTT                         RKP=2,LEN=34   *
000011*                                                                *
000012*   LOG = YES                                                    *
000013*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000014******************************************************************
000015*                   C H A N G E   L O G
000016*
000017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000018*-----------------------------------------------------------------
000019*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000020* EFFECTIVE    NUMBER
000021*-----------------------------------------------------------------
000022* 111204                   PEMA  NEW FILE TO SPLIT BANK COMM
000023* 040109  2009031600001    AJRA  ADD NEW TRAILER TYPE AND REDEFINE
000024* 012010  2009061500002    AJRA  ADD FLAG FOR REFUND WITH OPEN CLA
000025* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
000026* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
000027* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000028* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000029* 022715  CR2015010800003  PEMA  AGENT SIGNATURE
000030* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
000031* 012918  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
000032* 091318  CR2018073000001  PEMA  ADD Refund methods
000033* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000034******************************************************************
000035
000036 01  CERTIFICATE-TRAILERS.
000037     12  CS-RECORD-ID                      PIC XX.
000038         88  VALID-CS-ID                      VALUE 'CS'.
000039
000040     12  CS-CONTROL-PRIMARY.
000041         16  CS-COMPANY-CD                 PIC X.
000042         16  CS-CARRIER                    PIC X.
000043         16  CS-GROUPING                   PIC X(6).
000044         16  CS-STATE                      PIC XX.
000045         16  CS-ACCOUNT                    PIC X(10).
000046         16  CS-CERT-EFF-DT                PIC XX.
000047         16  CS-CERT-NO.
000048             20  CS-CERT-PRIME             PIC X(10).
000049             20  CS-CERT-SFX               PIC X.
000050         16  CS-TRAILER-TYPE               PIC X.
000051             88  COMM-TRLR           VALUE 'A'.
000052             88  CLAIM-HISTORY-TRLR  VALUE 'B'.
000053             88  CERT-DATA-TRLR      VALUE 'C'.
000054
000055     12  CS-DATA-AREA                      PIC X(516).
000056
000057     12  CS-BANK-COMMISSIONS REDEFINES CS-DATA-AREA.
000058         16  CS-BANK-COMMISSION-AREA.
000059             20  CS-BANK-COMMS       OCCURS 10.
000060                 24  CS-AGT                PIC X(10).
000061                 24  CS-COM-TYP            PIC X.
000062                 24  CS-SPP-FEES           PIC S9(5)V99   COMP-3.
000063                 24  CS-RECALC-LV-INDIC    PIC X.
000064                 24  FILLER                PIC X(10).
000065
000066         16  FILLER                        PIC X(256).
000067
000068     12  CS-CLAIM-HISTORY-TRAILER REDEFINES CS-DATA-AREA.
000069****  TO CALC NO OF BENEFITS PAID = (CS-DAYS-PAID / 30)
000070
000071         16  CS-MB-CLAIM-DATA OCCURS 24.
000072             20  CS-CLAIM-NO               PIC X(7).
000073             20  CS-CLAIM-TYPE             PIC X.
000074                 88  CS-AH-CLM               VALUE 'A'.
000075                 88  CS-IU-CLM               VALUE 'I'.
000076                 88  CS-GP-CLM               VALUE 'G'.
000077                 88  CS-LF-CLM               VALUE 'L'.
000078                 88  CS-PR-CLM               VALUE 'P'.
000079                 88  CS-FL-CLM               VALUE 'F'.
000080                 88  CS-OT-CLM               VALUE 'O'.
000081             20  CS-INSURED-TYPE           PIC X.
000082                 88  CS-PRIM-INSURED          VALUE 'P'.
000083                 88  CS-CO-BORROWER           VALUE 'C'.
000084             20  CS-BENEFIT-PERIOD         PIC 99.
000085             20  CS-DAYS-PAID              PIC S9(5) COMP-3.
000086             20  CS-TOTAL-PAID             PIC S9(7)V99 COMP-3.
000087             20  CS-REMAINING-BENS         PIC S999 COMP-3.
000088         16  FILLER                        PIC X(12).
000089
000090     12  CS-CERT-DATA REDEFINES CS-DATA-AREA.
000091         16  CS-VIN-NUMBER                 PIC X(17).
000092         16  CS-REFUND-CLAIM-FLAG          PIC X(01).
000093         16  CS-INS-AGE-DEFAULT-FLAG       PIC X(01).
000094         16  CS-JNT-AGE-DEFAULT-FLAG       PIC X(01).
000095         16  cs-agent-name.
000096             20  cs-agent-fname            pic x(20).
000097             20  cs-agent-mi               pic x.
000098             20  cs-agent-lname            pic x(25).
000099         16  cs-license-no                 pic x(15).
000100         16  cs-npn-number                 pic x(10).
000101         16  cs-agent-edit-status          pic x.
000102             88  cs-ae-refer-to-manager      value 'M'.
000103             88  cs-ae-cover-sheet           value 'C'.
000104             88  cs-ae-sig-form              value 'S'.
000105             88  cs-ae-verified              value 'V'.
000106             88  cs-unidentified-signature   value 'U'.
000107             88  cs-cert-returned            value 'R'.
000108             88  cs-accept-no-commission     value 'N'.
000109         16  cs-year                       pic 9999.
000110         16  cs-make                       pic x(20).
000111         16  cs-model                      pic x(20).
000112         16  cs-future                     pic x(20).
000113         16  cs-vehicle-odometer           pic s9(7) comp-3.
000114         16  cs-claim-verification-status  pic x.
000115             88  cs-clm-ver-eligible         value 'A'.
000116             88  cs-clm-ver-partial-elig     value 'B'.
000117             88  cs-clm-ver-not-eligible     value 'C'.
000118             88  cs-clm-ver-not-elig-opn-clm value 'D'.
000119             88  cs-clm-ver-not-part-elig-rw value 'E'.
000120             88  cs-clm-ver-ND-CERT          value 'F'.
000121             88  cs-clm-ver-spec-other       value 'G'.
000122             88  cs-clam-ver-pratial-corrected
000123                                             value 'H'.
000124             88  cs-clm-ver-no-matches       value 'I'.
000125             88  cs-clm-ver-not-elig-corrected
000126                                             value 'J'.
000127             88  cs-clm-ver-needs-review     value 'R'.
000128             88  cs-clm-ver-sent-to-claims   value 'W'.
000129         16  CS-LF-REFUND-METHOD           PIC X.
000130         16  CS-AH-REFUND-METHOD           PIC X.
000131         16  FILLER                        PIC X(353). *> was 420
000132*        16  FILLER                        PIC X(496).
      *<<((file: ELCCRTT))
000711     EJECT
000712*                                    COPY MPCPLCY.
000713     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL1501' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000714 VCOBOL-DUMMY-PROCEDURE.
000715
000716     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
000717     MOVE '5'                    TO  DC-OPTION-CODE.
000718     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
000719     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000720     MOVE DC-GREG-DATE-1-YMD     TO  SAVE-DATE-YMD.
000721     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000722
000723     IF SAVE-DATE-YY > 70
000724         MOVE 19                 TO  SAVE-DATE-CC
000725     ELSE
000726         MOVE 20                 TO  SAVE-DATE-CC.
000727
000728     IF EIBCALEN = 0
000729         GO TO 8800-UNAUTHORIZED-ACCESS.
000730
000731     MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
000732
000733     MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.
000734     MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.
000735
000736     MOVE EIBTRMID               TO QID-TERM.
000737
000738     
      * EXEC CICS HANDLE CONDITION
000739*        QIDERR   (1000-SHOW-CLAIM-HISTORY)
000740*        MAPFAIL  (0100-FIRST-TIME-IN)
000741*        NOTOPEN  (8500-FILE-NOTOPEN)
000742*        PGMIDERR (9600-PGMID-ERROR)
000743*        ERROR    (9990-ABEND)
000744*    END-EXEC.
      *    MOVE '"$N?JL.               ! " #00005056' TO DFHEIV0
           MOVE X'22244E3F4A4C2E2020202020' &
                X'202020202020202020202120' &
                X'2220233030303035303536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000745
000746     IF PI-RETURN-TO-PROGRAM = THIS-PGM
000747         MOVE PI-CALLING-PROGRAM       TO RETURNED-FROM.
000748
000749     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000750         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000751             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000752             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000753             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000754             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000755             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000756             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000757             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000758             MOVE THIS-PGM             TO PI-CALLING-PROGRAM
000759         ELSE
000760             MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
000761             MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
000762             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
000763             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
000764             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
000765             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
000766             MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
000767             MOVE SPACES               TO PI-SAVED-PROGRAM-6.
000768
000769     IF RETURNED-FROM NOT = SPACES
000770         GO TO 0600-RECOVER-TEMP-STORAGE.
000771
000772     IF EIBAID = DFHCLEAR
000773         GO TO 9400-CLEAR.
000774
000775     IF PI-PROCESSOR-ID = 'LGXX'
000776         NEXT SENTENCE
000777     ELSE
000778         
      * EXEC CICS READQ TS
000779*            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
000780*            INTO    (SECURITY-CONTROL)
000781*            LENGTH  (SC-COMM-LENGTH)
000782*            ITEM    (SC-ITEM)
000783*        END-EXEC
      *    MOVE '*$II   L              ''   #00005096' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035303936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000784             MOVE SC-CLAIMS-DISPLAY (16)  TO  PI-DISPLAY-CAP
000785             MOVE SC-CLAIMS-UPDATE  (16)  TO  PI-MODIFY-CAP
000786             IF NOT DISPLAY-CAP
000787                 MOVE 'READ'              TO  SM-READ
000788                 PERFORM 9995-SECURITY-VIOLATION
000789                 MOVE ER-0070             TO  EMI-ERROR
000790                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000791                 GO TO 8100-SEND-INITIAL-MAP.
000792
000793     IF EIBTRNID = TRANS-ID
000794         GO TO 0200-RECEIVE.
000795
000796 EJECT
000797 0100-FIRST-TIME-IN.
000798     MOVE LOW-VALUES             TO  EL150BO
000799                                     PI-PROGRAM-WORK-AREA.
000800
000801     MOVE 'Y'                    TO  PI-FIRST-TIME-SW
000802                                     WS-RECORDS-READ-SW.
000803     MOVE 1                      TO  PI-LINE-NO.
000804     MOVE ZERO                   TO  PI-PREV-SEQ-NO
000805                                     PI-REMINDERS-SW
000806                                     PI-LETTERS-SW
000807                                     PI-PAYMENTS-SW
000808                                     PI-AUTO-PAY-SW
000809                                     PI-NOTES-SW
000810                                     PI-RES-EXP-SW
000811                                     PI-DENIALS-SW
000812                                     PI-INCURRED-DATE-SW
000813                                     PI-FORMS-SW.
000814
000815     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
000816     MOVE '2'                    TO CNTL-REC-TYPE
000817     MOVE pi-processor-id        TO CNTL-ACCESS
000818     MOVE +0                     TO CNTL-SEQ-NO
000819     MOVE 'CNTL'                 TO FILE-SWITCH
000820
000821     PERFORM 7900-READ-CONTROL-FILE
000822                                 THRU 7900-EXIT
000823     MOVE CF-APPROVAL-LEVEL      TO PI-APPROVAL-LEVEL
000824
000825     
      * EXEC CICS DELETEQ TS
000826*        QUEUE(QID)
000827*    END-EXEC.
      *    MOVE '*&                    #   #00005143' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035313433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000828
000829     GO TO 1000-SHOW-CLAIM-HISTORY.
000830
000831     EJECT
000832 0200-RECEIVE.
000833     MOVE 'B'                    TO  PASS-SWITCH.
000834     MOVE LOW-VALUES             TO  EL150BI.
000835
000836     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000837         MOVE ER-0008            TO  EMI-ERROR
000838         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000839         MOVE -1                 TO  ENTERPFL
000840         GO TO 8200-SEND-DATAONLY.
000841
000842     
      * EXEC CICS RECEIVE
000843*        MAP      (MAP-NAME)
000844*        MAPSET   (MAPSET-NAME)
000845*        INTO     (EL150BI)
000846*    END-EXEC.
           MOVE LENGTH OF
            EL150BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005160' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035313630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150BI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000847
000848     IF ENTERPFL = 0
000849         GO TO 0300-CHECK-PFKEYS.
000850
000851     IF EIBAID = DFHENTER
000852         MOVE ER-0004            TO  EMI-ERROR
000853         GO TO 0320-INPUT-ERROR.
000854
000855     IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)
000856         MOVE PF-VALUES (ENTERPFI)   TO  EIBAID
000857     ELSE
000858         MOVE ER-0029                TO  EMI-ERROR
000859         GO TO 0320-INPUT-ERROR.
000860
000861 0300-CHECK-PFKEYS.
000862     IF EIBAID = DFHPF23
000863         GO TO 8810-PF23.
000864
000865     IF EIBAID = DFHPF24
000866         GO TO 9200-RETURN-MAIN-MENU.
000867
000868     IF EIBAID = DFHPF12
000869         GO TO 9500-PF12.
000870
000871     IF EIBAID = DFHPF3
000872        IF LINENOL > +0
000873            GO TO 0500-CREATE-TEMP-STORAGE
000874        ELSE
000875            MOVE ER-0672        TO  EMI-ERROR
000876            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000877            MOVE -1             TO  LINENOL
000878            GO TO 8200-SEND-DATAONLY.
000879
000880     IF EIBAID = DFHPF4
000881        IF LINENOL > +0
000882           IF (PI-EL142-PRIORITY = '8')
000883              AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
000884                   AND 'AMWA')
000885              MOVE ER-8003        TO  EMI-ERROR
000886              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000887              MOVE -1             TO  LINENOL
000888              GO TO 8200-SEND-DATAONLY
000889           ELSE
000890              MOVE 'V'            TO  WS-VOID-CODE
000891              GO TO 5000-VOID-PAYMENT
000892           END-IF
000893        ELSE
000894           MOVE ER-0663        TO  EMI-ERROR
000895           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000896           MOVE -1             TO  LINENOL
000897           GO TO 8200-SEND-DATAONLY
000898        END-IF
000899     END-IF
000900
000901     IF EIBAID = DFHPF5
000902        IF LINENOL > +0
000903           IF (PI-EL142-PRIORITY = '8')
000904              AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
000905                   AND 'AMWA')
000906              MOVE ER-8003        TO  EMI-ERROR
000907              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000908              MOVE -1             TO  LINENOL
000909              GO TO 8200-SEND-DATAONLY
000910           ELSE
000911              MOVE 'S'           TO  WS-VOID-CODE
000912              GO TO 5000-VOID-PAYMENT
000913           END-IF
000914        ELSE
000915           MOVE ER-0835        TO  EMI-ERROR
000916           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000917           MOVE -1             TO  LINENOL
000918           GO TO 8200-SEND-DATAONLY
000919        END-IF
000920     END-IF
000921
000922*    IF EIBAID = DFHPF6
000923*        IF PI-COMPANY-ID = 'DMD'
000924*            IF LINENOL NOT = ZEROS
000925*                MOVE LINENOI        TO SUB-1
000926*                IF PI-TRLR-TYPE (SUB-1) = '2'
000927*                    PERFORM 0500-CREATE-TEMP-STORAGE
000928*                    MOVE 'EL402DMD' TO PGM-NAME
000929*                    GO TO 9300-XCTL
000930*                ELSE
000931*                    MOVE ER-0940    TO EMI-ERROR
000932*                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000933*                    MOVE -1         TO LINENOL
000934*                    GO TO 8200-SEND-DATAONLY
000935*            ELSE
000936*                MOVE ER-0939    TO EMI-ERROR
000937*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000938*                MOVE -1         TO LINENOL
000939*                GO TO 8200-SEND-DATAONLY.
000940
000941     MOVE SPACES                 TO  ERRMSG1O.
000942
000943     IF EIBAID = DFHPF1
000944         MOVE 'F'                TO  DIRECTION-SWITCH
000945         MOVE 'Y'                TO  PI-FIRST-TIME-SW
000946         GO TO 1000-SHOW-CLAIM-HISTORY.
000947
000948     IF EIBAID = DFHPF2
000949         MOVE 'B'                TO  DIRECTION-SWITCH
000950         MOVE 'Y'                TO  PI-FIRST-TIME-SW
000951         MOVE PI-TRLR-SEQ-NO (1) TO  PI-PREV-SEQ-NO
000952         GO TO 1000-SHOW-CLAIM-HISTORY.
000953
000954     IF EIBAID = DFHENTER
000955         GO TO 0330-EDIT-DATA.
000956
000957     MOVE ER-0029                TO EMI-ERROR.
000958
000959 0320-INPUT-ERROR.
000960     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000961
000962     IF ENTERPFL = 0
000963         MOVE -1                 TO ENTERPFL
000964     ELSE
000965         MOVE AL-UNBON           TO ENTERPFA
000966         MOVE -1                 TO ENTERPFL.
000967
000968     GO TO 8200-SEND-DATAONLY.
000969
000970 0330-EDIT-DATA.
000971     IF NOT MODIFY-CAP
000972         MOVE 'UPDATE'           TO  SM-READ
000973         PERFORM 9995-SECURITY-VIOLATION
000974         MOVE ER-0070            TO  EMI-ERROR
000975         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000976         GO TO 8100-SEND-INITIAL-MAP.
000977
000978     IF LINENOL  > 0 AND
000979        RECVDTL  > 0 AND
000980        RECVTYPL > 0
000981           GO TO 4000-RECEIVE-FORMS.
000982
000983     IF LINENOL > 0 AND
000984        RECVDTL > 0
000985           GO TO 3000-RECEIVE-LETTERS.
000986
000987     IF LINENOL > 0
000988         MOVE LINENOI            TO  SUB-1
000989         IF PI-TRLR-TYPE (SUB-1) = 'A'
000990             MOVE ER-0665        TO  EMI-ERROR
000991             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000992             MOVE -1             TO  LINENOL
000993             GO TO 8200-SEND-DATAONLY
000994         ELSE
000995             IF PI-TRLR-TYPE (SUB-1) = '4'
000996                 MOVE ER-0666        TO  EMI-ERROR
000997                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000998                 MOVE -1             TO  LINENOL
000999                 GO TO 8200-SEND-DATAONLY
001000             ELSE
001001                 IF PI-TRLR-TYPE (SUB-1) = '2'
001002                     MOVE ER-0667    TO  EMI-ERROR
001003                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001004                     MOVE -1         TO  LINENOL
001005                     GO TO 8200-SEND-DATAONLY.
001006
001007     MOVE 'F'                    TO  DIRECTION-SWITCH.
001008     MOVE 'Y'                    TO  PI-FIRST-TIME-SW.
001009     GO TO 1000-SHOW-CLAIM-HISTORY.
001010
001011     EJECT
001012 0500-CREATE-TEMP-STORAGE.
001013     MOVE EIBCPOSN               TO PI-SAVE-CURSOR.
001014     MOVE SPACES                 TO PI-FULL-SCREEN-IND.
001015
001016     
      * EXEC CICS WRITEQ TS
001017*        QUEUE    (QID)
001018*        FROM     (PROGRAM-INTERFACE-BLOCK)
001019*        LENGTH   (PI-COMM-LENGTH)
001020*    END-EXEC.
      *    MOVE '*"     L              ''   #00005334' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035333334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001021
001022     IF LINENOL > +0
001023         MOVE LINENOI            TO  SUB-1
001024         IF PI-TRLR-TYPE (SUB-1) = '2'
001025            MOVE +1                      TO  PI-PAYMENTS-SW
001026            MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
001027                                             PI-SAVE-ATK-SEQ-NO
001028         ELSE
001029         IF PI-TRLR-TYPE (SUB-1) = '3'
001030            MOVE +1                      TO  PI-AUTO-PAY-SW
001031            MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
001032                                             PI-SAVE-ATK-SEQ-NO
001033         ELSE
001034         IF PI-TRLR-TYPE (SUB-1) = '4'
001035            MOVE +1                      TO  PI-LETTERS-SW
001036            MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
001037                                             PI-SAVE-ATK-SEQ-NO
001038         ELSE
001039         IF PI-TRLR-TYPE (SUB-1) = '6'
001040            MOVE +1                      TO  PI-NOTES-SW
001041            MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
001042                                             PI-SAVE-ATK-SEQ-NO
001043         ELSE
001044         IF PI-TRLR-TYPE (SUB-1) = '7'
001045            MOVE +1                      TO  PI-REMINDERS-SW
001046            MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
001047                                             PI-SAVE-ATK-SEQ-NO
001048         ELSE
001049         IF PI-TRLR-TYPE (SUB-1) = '8'
001050            MOVE +1                      TO  PI-DENIALS-SW
001051            MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
001052                                             PI-SAVE-ATK-SEQ-NO
001053         ELSE
001054         IF PI-TRLR-TYPE (SUB-1) = '9'
001055            MOVE +1                      TO  PI-INCURRED-DATE-SW
001056            MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
001057                                             PI-SAVE-ATK-SEQ-NO
001058         ELSE
001059            MOVE +1                      TO  PI-FORMS-SW
001060            MOVE PI-TRLR-SEQ-NO (SUB-1)  TO  PI-ATK-SEQ-NO
001061                                             PI-SAVE-ATK-SEQ-NO
001062     ELSE
001063         MOVE +1                         TO  PI-REMINDERS-SW
001064                                             PI-LETTERS-SW
001065                                             PI-PAYMENTS-SW
001066                                             PI-AUTO-PAY-SW
001067                                             PI-NOTES-SW
001068                                             PI-RES-EXP-SW
001069                                             PI-DENIALS-SW
001070                                             PI-INCURRED-DATE-SW
001071                                             PI-FORMS-SW
001072         MOVE +0                         TO  PI-ATK-SEQ-NO.
001073
001074     IF EIBAID = DFHPF6
001075         MOVE PI-COMPANY-CD      TO  PI-SAVE-ATK-COMPANY-CODE
001076                                     PI-ATK-COMPANY-CODE
001077         MOVE PI-CARRIER         TO  PI-SAVE-ATK-CARRIER
001078                                     PI-ATK-CARRIER
001079         MOVE PI-CLAIM-NO        TO  PI-SAVE-ATK-CLAIM-NO
001080                                     PI-ATK-CLAIM-NO
001081         MOVE PI-CERT-NO         TO  PI-SAVE-ATK-CERT-NO
001082                                     PI-ATK-CERT-NO
001083         MOVE 'Y'                TO  PI-FIRST-TIME-SW.
001084
001085     IF EIBAID = DFHPF3
001086         MOVE XCTL-142           TO  PGM-NAME
001087         MOVE 'EL142A'           TO  PI-MAP-NAME
001088         MOVE PI-COMPANY-CD      TO  PI-SAVE-ATK-COMPANY-CODE
001089                                     PI-ATK-COMPANY-CODE
001090         MOVE PI-CARRIER         TO  PI-SAVE-ATK-CARRIER
001091                                     PI-ATK-CARRIER
001092         MOVE PI-CLAIM-NO        TO  PI-SAVE-ATK-CLAIM-NO
001093                                     PI-ATK-CLAIM-NO
001094         MOVE PI-CERT-NO         TO  PI-SAVE-ATK-CERT-NO
001095                                     PI-ATK-CERT-NO
001096         MOVE 'Y'                TO  PI-FIRST-TIME-SW
001097         GO TO 9300-XCTL.
001098
001099     EJECT
001100 0600-RECOVER-TEMP-STORAGE.
001101     MOVE PI-CONTROL-IN-PROGRESS TO SAVE-CONTROL.
001102
001103     
      * EXEC CICS HANDLE CONDITION
001104*        QIDERR   (0690-QIDERR)
001105*    END-EXEC.
      *    MOVE '"$N                   ! # #00005421' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303035343231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001106
001107     
      * EXEC CICS READQ TS
001108*        QUEUE    (QID)
001109*        INTO     (PROGRAM-INTERFACE-BLOCK)
001110*        LENGTH   (PI-COMM-LENGTH)
001111*    END-EXEC.
      *    MOVE '*$I    L              ''   #00005425' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035343235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001112
001113     
      * EXEC CICS DELETEQ TS
001114*        QUEUE   (QID)
001115*    END-EXEC.
      *    MOVE '*&                    #   #00005431' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035343331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001116
001117     MOVE SAVE-CONTROL           TO  PI-CONTROL-IN-PROGRESS.
001118     MOVE LOW-VALUES             TO  EL150BO.
001119     MOVE ZEROS                  TO  PI-PREV-SEQ-NO.
001120     MOVE 'F'                    TO  DIRECTION-SWITCH.
001121     MOVE 'Y'                    TO  PI-FIRST-TIME-SW.
001122
001123     GO TO 1000-SHOW-CLAIM-HISTORY.
001124
001125 0690-QIDERR.
001126     MOVE ER-0033                TO  EMI-ERROR.
001127     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001128     MOVE LOW-VALUES             TO  EL150BO.
001129     MOVE -1                     TO  ENTERPFL.
001130     GO TO 8100-SEND-INITIAL-MAP.
001131
001132     EJECT
001133 1000-SHOW-CLAIM-HISTORY.
001134
001135     PERFORM 2000-BUILD-TRAILER-DISPLAY THRU 2999-EXIT.
001136
001137     MOVE PI-CARRIER             TO  CARRO.
001138     MOVE PI-CLAIM-NO            TO  CLMNOO.
001139     MOVE PI-CERT-PRIME          TO  CERTNOO.
001140     MOVE PI-CERT-SFX            TO  SUFXO.
001141
001142     MOVE -1                     TO  LINENOL.
001143
001144     IF RECORDS-READ
001145         MOVE 'N'                TO  WS-RECORDS-READ-SW
001146         GO TO 8100-SEND-INITIAL-MAP
001147     ELSE
001148         GO TO 8200-SEND-DATAONLY.
001149
001150     EJECT
001151 2000-BUILD-TRAILER-DISPLAY.
001152
001153     MOVE PI-COMPANY-CD          TO  TRLR-COMP-CD.
001154     MOVE PI-CARRIER             TO  TRLR-CARRIER.
001155     MOVE PI-CLAIM-NO            TO  TRLR-CLAIM-NO.
001156     MOVE PI-CERT-NO             TO  TRLR-CERT-NO.
001157     MOVE 'TRLR'                 TO  FILE-SWITCH.
001158     MOVE PI-PREV-SEQ-NO         TO  TRLR-SEQ-NO.
001159
001160     IF PI-PREV-DIRECTION = 'B'
001161         IF DIRECTION-SWITCH = 'F'
001162             MOVE PI-TRLR-SEQ-NO (8) TO  TRLR-SEQ-NO
001163                                         PI-PREV-SEQ-NO.
001164
001165     IF DIRECTION-SWITCH = 'B'
001166        MOVE 'B'                 TO  PI-PREV-DIRECTION
001167        MOVE +15                 TO  DISPLAY-CNT
001168        GO TO 2050-START-BROWSE.
001169
001170     MOVE +1                     TO  DISPLAY-CNT.
001171     MOVE 'F'                    TO  DIRECTION-SWITCH
001172                                     PI-PREV-DIRECTION.
001173 2010-START-BROWSE.
001174     
      * EXEC CICS HANDLE CONDITION
001175*        ENDFILE   (2950-NO-MORE-TRAILERS)
001176*        NOTFND    (2950-NO-MORE-TRAILERS)
001177*    END-EXEC.
      *    MOVE '"$''I                  ! $ #00005492' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303035343932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001178
001179     
      * EXEC CICS STARTBR
001180*        DATASET   ('ELTRLR')
001181*        RIDFLD    (ELTRLR-KEY)
001182*        GTEQ
001183*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005497' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303035343937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001184
001185     MOVE 1                      TO  PI-LINE-NO
001186                                     SUB-1.
001187
001188 2020-BROWSE-FORWARD.
001189
001190     
      * EXEC CICS HANDLE CONDITION
001191*        NOTFND    (2020-BROWSE-FORWARD)
001192*    END-EXEC.
      *    MOVE '"$I                   ! % #00005508' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303035353038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001193
001194     
      * EXEC CICS READNEXT
001195*        DATASET   ('ELTRLR')
001196*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
001197*        RIDFLD    (ELTRLR-KEY)
001198*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005512' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303035353132' TO DFHEIV0
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
           
001199
001200     IF (PI-COMPANY-CD NOT = TRLR-COMP-CD)  OR
001201        (PI-CARRIER    NOT = TRLR-CARRIER)  OR
001202        (PI-CLAIM-NO   NOT = TRLR-CLAIM-NO) OR
001203        (PI-CERT-NO    NOT = TRLR-CERT-NO)
001204            GO TO 2950-NO-MORE-TRAILERS.
001205
001206     IF TRLR-SEQ-NO = 90
001207         GO TO 2020-BROWSE-FORWARD.
001208
001209     IF RESERVE-EXPENSE-TR OR ADDRESS-TR
001210         GO TO 2020-BROWSE-FORWARD.
001211
001212     IF TRLR-SEQ-NO = PI-PREV-SEQ-NO
001213         MOVE 'N'                TO  WS-RECORDS-READ-SW
001214         GO TO 2020-BROWSE-FORWARD
001215     ELSE
001216         IF FIRST-TIME
001217             PERFORM 2070-INITIALIZE-SCREEN-AREA THRU 2070-EXIT
001218             MOVE 'N'            TO  PI-FIRST-TIME-SW
001219             MOVE 'Y'            TO  WS-RECORDS-READ-SW
001220         ELSE
001221             MOVE 'Y'            TO  WS-RECORDS-READ-SW.
001222
001223     MOVE TRLR-SEQ-NO            TO  PI-PREV-SEQ-NO.
001224     GO TO 2090-DISPLAY-TRAILER.
001225
001226 2050-START-BROWSE.
001227     
      * EXEC CICS HANDLE CONDITION
001228*        ENDFILE   (2950-NO-MORE-TRAILERS)
001229*        NOTFND    (2950-NO-MORE-TRAILERS)
001230*    END-EXEC.
      *    MOVE '"$''I                  ! & #00005545' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303035353435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001231
001232     
      * EXEC CICS STARTBR
001233*        DATASET   ('ELTRLR')
001234*        RIDFLD    (ELTRLR-KEY)
001235*        GTEQ
001236*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005550' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303035353530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001237
001238     MOVE 8                      TO  PI-LINE-NO
001239                                     SUB-1.
001240
001241 2060-BROWSE-BACKWARD.
001242
001243     
      * EXEC CICS READPREV
001244*        DATASET   ('ELTRLR')
001245*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
001246*        RIDFLD    (ELTRLR-KEY)
001247*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00005561' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303035353631' TO DFHEIV0
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
           
001248
001249     IF (PI-COMPANY-CD NOT = TRLR-COMP-CD)  OR
001250        (PI-CARRIER    NOT = TRLR-CARRIER)  OR
001251        (PI-CLAIM-NO   NOT = TRLR-CLAIM-NO) OR
001252        (PI-CERT-NO    NOT = TRLR-CERT-NO)
001253            GO TO 2950-NO-MORE-TRAILERS.
001254
001255     IF TRLR-SEQ-NO = 90
001256         GO TO 2060-BROWSE-BACKWARD.
001257
001258     IF RESERVE-EXPENSE-TR OR ADDRESS-TR
001259         GO TO 2060-BROWSE-BACKWARD.
001260
001261     IF TRLR-SEQ-NO = PI-PREV-SEQ-NO
001262         MOVE 'N'                TO  WS-RECORDS-READ-SW
001263         GO TO 2060-BROWSE-BACKWARD
001264     ELSE
001265         IF FIRST-TIME
001266             PERFORM 2070-INITIALIZE-SCREEN-AREA THRU 2070-EXIT
001267             MOVE 'N'            TO  PI-FIRST-TIME-SW
001268             MOVE 'Y'            TO  WS-RECORDS-READ-SW
001269         ELSE
001270             MOVE 'Y'            TO  WS-RECORDS-READ-SW.
001271
001272     MOVE TRLR-SEQ-NO            TO  PI-PREV-SEQ-NO.
001273     GO TO 2090-DISPLAY-TRAILER.
001274
001275 EJECT
001276 2070-INITIALIZE-SCREEN-AREA.
001277
001278     MOVE SPACES                 TO  TEXT-WORK-AREAS
001279                                     MAP-HDG (1)   MAP-TEXT (1)
001280                                     MAP-HDG (2)   MAP-TEXT (2)
001281                                     MAP-HDG (3)   MAP-TEXT (3)
001282                                     MAP-HDG (4)   MAP-TEXT (4)
001283                                     MAP-HDG (5)   MAP-TEXT (5)
001284                                     MAP-HDG (6)   MAP-TEXT (6)
001285                                     MAP-HDG (7)   MAP-TEXT (7)
001286                                     MAP-HDG (8)   MAP-TEXT (8)
001287                                     MAP-HDG (9)   MAP-TEXT (9)
001288                                     MAP-HDG (10)  MAP-TEXT (10)
001289                                     MAP-HDG (11)  MAP-TEXT (11)
001290                                     MAP-HDG (12)  MAP-TEXT (12)
001291                                     MAP-HDG (13)  MAP-TEXT (13)
001292                                     MAP-HDG (14)  MAP-TEXT (14)
001293                                     MAP-HDG (15)  MAP-TEXT (15)
001294                                     MAP-HDG (16)  MAP-TEXT (16).
001295
001296     MOVE ZEROS                  TO  PMT-AMT-PAID
001297                                     AUTO-1ST-AMT
001298                                     AUTO-REG-AMT
001299                                     INCUR-TOT-PD.
001300
001301 2070-EXIT.
001302     EXIT.
001303
001304 EJECT
001305 2090-DISPLAY-TRAILER.
001306
001307     IF PAYMENT-TR
001308         GO TO 2100-PAYMENT-TRAILER.
001309
001310     IF AUTO-PAY-TR
001311        GO TO 2200-AUTO-PAYMENT-TRAILER.
001312
001313     IF CORRESPONDENCE-TR
001314         GO TO 2300-CORRESPONDENCE-TRAILER.
001315
001316     IF GENERAL-INFO-TR
001317         GO TO 2400-GENERAL-INFO-TRAILER.
001318
001319     IF AUTO-PROMPT-TR
001320         GO TO 2500-AUTO-PROMPT-TRAILER.
001321
001322     IF DENIAL-TR
001323         GO TO 2600-DENIAL-TRAILER.
001324
001325     IF INCURRED-CHG-TR
001326         GO TO 2700-INCURRED-CHANGE-TRAILER.
001327
001328     IF FORM-CONTROL-TR
001329         GO TO 2710-FORM-CONTROL-TRAILER.
001330
001331     IF DIRECTION-SWITCH = 'B'
001332         GO TO 2060-BROWSE-BACKWARD
001333     ELSE
001334         GO TO 2020-BROWSE-FORWARD.
001335
001336     EJECT
001337 2100-PAYMENT-TRAILER.
001338     MOVE ZEROS                  TO  PMT-AMT-PAID.
001339     MOVE PI-LINE-NO             TO  PMT-LINE-NO.
001340     MOVE 'PAYMENT '             TO  PMT-HDG1-LIT.
001341     MOVE 'TYPE: '               TO  PMT-TYPE-LIT.
001342
001343     IF TRANSFER
001344        MOVE 'TRANSFR'           TO PMT-TYPE
001345        GO TO 2100-CONT
001346     ELSE
001347        IF AT-PAYMENT-TYPE = 'I'
001348           MOVE 'INTEREST'       TO PMT-TYPE
001349           GO TO 2100-CONT
001350        END-IF
001351     END-IF
001352
001353     IF AT-CV-PMT-CODE = ' '
001354         MOVE AT-PAYMENT-TYPE            TO  WS-SUB
001355         IF  WS-SUB < 1 OR > 6
001356             MOVE 2                      TO  WS-SUB
001357             MOVE PAY-DESC (WS-SUB)      TO  PMT-TYPE
001358         ELSE
001359             MOVE PAY-DESC (WS-SUB)      TO  PMT-TYPE
001360     ELSE
001361         MOVE AT-CV-PMT-CODE             TO  WS-SUB
001362         IF WS-SUB < 1 OR > 8
001363             MOVE 1                      TO  WS-SUB
001364             MOVE CV-PAY-DESC (WS-SUB)   TO  PMT-TYPE
001365         ELSE
001366             MOVE CV-PAY-DESC (WS-SUB)   TO  PMT-TYPE.
001367
001368 2100-CONT.
001369
001370     if at-ach-payment = 'Y'
001371        move ' ACH PAYMT '       to pmt-check-no-lit
001372        move 'ACH PMNT'          to PMT-HDG1-LIT
001373     else
001374        MOVE ' CHECK NO: '       TO  PMT-CHECK-NO-LIT
001375     end-if
001376
001377     MOVE AT-CHECK-NO            TO  PMT-CHECK-NO.
001378
001379     IF AT-CHECK-WRITTEN-DT = LOW-VALUES
001380         IF AT-TO-BE-WRITTEN-DT = LOW-VALUES
001381             MOVE ' DT WRITTEN : '         TO  PMT-DT-WRITTEN-LIT
001382             MOVE SPACES                   TO  PMT-DT-WRITTEN
001383         ELSE
001384             MOVE ' HOLD UNTIL : '         TO  PMT-DT-WRITTEN-LIT
001385             MOVE AT-TO-BE-WRITTEN-DT      TO  DC-BIN-DATE-1
001386             MOVE ' '                      TO  DC-OPTION-CODE
001387             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001388             IF NO-CONVERSION-ERROR
001389                 MOVE DC-GREG-DATE-1-EDIT  TO  PMT-DT-WRITTEN
001390             ELSE
001391                 MOVE SPACES               TO  PMT-DT-WRITTEN
001392     ELSE
001393         MOVE ' DT WRITTEN : '             TO  PMT-DT-WRITTEN-LIT
001394         MOVE AT-CHECK-WRITTEN-DT          TO  DC-BIN-DATE-1
001395         MOVE ' '                          TO  DC-OPTION-CODE
001396         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001397         IF NO-CONVERSION-ERROR
001398             MOVE DC-GREG-DATE-1-EDIT      TO  PMT-DT-WRITTEN
001399         ELSE
001400             MOVE SPACES                   TO  PMT-DT-WRITTEN.
001401
001402     MOVE ' AMT:'                TO  PMT-AMT-PD-LIT.
001403     MOVE AT-AMOUNT-PAID         TO  PMT-AMT-PAID.
001404
001405     MOVE 'PAID'                 TO  PMT-HDG2-LIT.
001406     MOVE 'FROM: '               TO  PMT-FROM-LIT.
001407
001408     IF AT-PAID-FROM-DT = LOW-VALUES
001409         MOVE SPACES                     TO  PMT-PAID-FROM
001410     ELSE
001411         MOVE AT-PAID-FROM-DT            TO  DC-BIN-DATE-1
001412         MOVE ' '                        TO  DC-OPTION-CODE
001413         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001414         IF NO-CONVERSION-ERROR
001415             MOVE DC-GREG-DATE-1-EDIT    TO PMT-PAID-FROM
001416         ELSE
001417             MOVE SPACES                 TO  PMT-PAID-FROM.
001418
001419     IF PI-USES-PAID-TO
001420         MOVE ' PAID TO: '               TO  PMT-THRU-LIT
001421     ELSE
001422         MOVE ' PD THRU: '               TO  PMT-THRU-LIT.
001423
001424     IF AT-PAID-THRU-DT = LOW-VALUES
001425         MOVE SPACES                     TO  PMT-PAID-THRU
001426     ELSE
001427         IF PI-USES-PAID-TO
001428             MOVE AT-PAID-THRU-DT        TO  DC-BIN-DATE-1
001429             MOVE '6'                    TO  DC-OPTION-CODE
001430             MOVE +1                     TO  DC-ELAPSED-DAYS
001431             MOVE +0                     TO  DC-ELAPSED-MONTHS
001432             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001433             IF NO-CONVERSION-ERROR
001434                 MOVE DC-GREG-DATE-1-EDIT TO PMT-PAID-THRU
001435             ELSE
001436                 MOVE SPACES             TO  PMT-PAID-THRU
001437         ELSE
001438             MOVE AT-PAID-THRU-DT        TO  DC-BIN-DATE-1
001439             MOVE ' '                    TO  DC-OPTION-CODE
001440             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001441             IF NO-CONVERSION-ERROR
001442                 MOVE DC-GREG-DATE-1-EDIT TO  PMT-PAID-THRU
001443             ELSE
001444                 MOVE SPACES             TO  PMT-PAID-THRU.
001445
001446     MOVE ' PAYEE: '             TO  PMT-PAYEE-LIT.
001447     IF INSURED-PAID
001448         MOVE 'INSURED'          TO  PMT-PAYEE.
001449     IF BENEFICIARY-PAID
001450         MOVE 'BENEF '           TO  PMT-PAYEE.
001451     IF ACCOUNT-PAID
001452         MOVE 'ACCOUNT'          TO  PMT-PAYEE.
001453     IF OTHER-1-PAID
001454         MOVE 'OTHER 1'          TO  PMT-PAYEE.
001455     IF OTHER-2-PAID
001456         MOVE 'OTHER 2'          TO  PMT-PAYEE.
001457     IF DOCTOR-PAID
001458         MOVE 'DOCTOR'           TO  PMT-PAYEE.
001459     IF EMPLOYER-PAID
001460         MOVE 'EMPLOY'           TO  PMT-PAYEE.
001461
001462     IF AT-VOID-DT = LOW-VALUES OR SPACES
001463         MOVE SPACES                     TO  PMT-VOID-LIT
001464     ELSE
001465         IF AT-VOID-TYPE = 'S'
001466             MOVE ' STOP DATE : '        TO  PMT-VOID-LIT
001467         ELSE
001468             MOVE ' VOID DATE : '        TO  PMT-VOID-LIT.
001469
001470     IF AT-VOID-DT = LOW-VALUES
001471         MOVE SPACES                     TO  PMT-VOID-DT
001472     ELSE
001473         MOVE AT-VOID-DT                 TO  DC-BIN-DATE-1
001474         MOVE ' '                        TO  DC-OPTION-CODE
001475         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001476         IF NO-CONVERSION-ERROR
001477             MOVE DC-GREG-DATE-1-EDIT    TO  PMT-VOID-DT
001478         ELSE
001479             MOVE SPACES                 TO  PMT-VOID-DT.
001480
001481     MOVE PMT-HDG1               TO  MAP-HDG        (DISPLAY-CNT).
001482     MOVE PMT-TEXT-1             TO  MAP-TEXT       (DISPLAY-CNT).
001483     MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
001484     MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
001485     ADD +1 TO DISPLAY-CNT.
001486     MOVE PMT-HDG2               TO  MAP-HDG        (DISPLAY-CNT).
001487     MOVE PMT-TEXT-2             TO  MAP-TEXT       (DISPLAY-CNT).
001488     MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
001489                                     MAP-TEXT-ATTRB (DISPLAY-CNT).
001490     MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
001491     MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
001492     MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
001493
001494     GO TO 2800-INCR-DISPLAY-CNT.
001495
001496     EJECT
001497 2200-AUTO-PAYMENT-TRAILER.
001498     MOVE PI-LINE-NO             TO  AUTO-LINE-NO.
001499     MOVE 'AUTO PMT'             TO  AUTO-HDG1-LIT.
001500
001501     MOVE 'EFF : '                       TO  AUTO-EFF-DT-LIT.
001502
001503     IF AT-SCHEDULE-START-DT = LOW-VALUES
001504         MOVE SPACES                     TO  AUTO-EFF-DT
001505     ELSE
001506         MOVE AT-SCHEDULE-START-DT   TO  DC-BIN-DATE-1
001507         MOVE ' '                    TO  DC-OPTION-CODE
001508         MOVE +0                     TO  DC-ELAPSED-DAYS
001509                                         DC-ELAPSED-MONTHS
001510         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001511         IF NO-CONVERSION-ERROR
001512             MOVE DC-GREG-DATE-1-EDIT TO AUTO-EFF-DT
001513         ELSE
001514             MOVE SPACES             TO  AUTO-EFF-DT.
001515
001516     MOVE ' 1ST AMT: '           TO  AUTO-1ST-AMT-LIT.
001517     MOVE ' REG AMOUNT : '       TO  AUTO-REG-AMT-LIT.
001518     MOVE AT-FIRST-PMT-AMT       TO  AUTO-1ST-AMT.
001519     MOVE AT-REGULAR-PMT-AMT     TO  AUTO-REG-AMT.
001520
001521     MOVE ' STAT : '             TO  AUTO-PMT-STATUS-LIT.
001522
001523     IF AT-TERMINATED-DT NOT = LOW-VALUES
001524         MOVE 'TERM'             TO  AUTO-PMT-STATUS
001525     ELSE
001526         MOVE 'ACTIVE'           TO  AUTO-PMT-STATUS.
001527
001528     MOVE 'PAYE'                 TO  AUTO-HDG2-LIT.
001529     MOVE '    : '               TO  AUTO-PAYEE-LIT.
001530
001531     IF INSURED-PAID-AUTO
001532         MOVE 'INSURED '         TO  AUTO-PAYEE.
001533     IF BENEFICIARY-PAID-AUTO
001534         MOVE 'BENEF   '         TO  AUTO-PAYEE.
001535     IF ACCOUNT-PAID-AUTO
001536         MOVE 'ACCOUNT '         TO  AUTO-PAYEE.
001537     IF OTHER-1-PAID-AUTO
001538         MOVE 'OTHER 1 '         TO  AUTO-PAYEE.
001539     IF OTHER-2-PAID
001540         MOVE 'OTHER 2 '         TO  AUTO-PAYEE.
001541     IF DOCTOR-PAID
001542         MOVE 'DOCTOR  '         TO  AUTO-PAYEE.
001543
001544     MOVE ' 1ST PMT: '                   TO  AUTO-1ST-PMT-LIT.
001545
001546     IF AT-1ST-PAY-THRU-DT = LOW-VALUES
001547         MOVE SPACES                     TO  AUTO-1ST-PMT-DT
001548     ELSE
001549         IF PI-USES-PAID-TO
001550             MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1
001551             MOVE '6'                    TO  DC-OPTION-CODE
001552             MOVE +1                     TO  DC-ELAPSED-DAYS
001553             MOVE +0                     TO  DC-ELAPSED-MONTHS
001554             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001555             IF NO-CONVERSION-ERROR
001556                 MOVE DC-GREG-DATE-1-EDIT TO AUTO-1ST-PMT-DT
001557             ELSE
001558                 MOVE SPACES             TO  AUTO-1ST-PMT-DT
001559         ELSE
001560             MOVE AT-1ST-PAY-THRU-DT     TO  DC-BIN-DATE-1
001561             MOVE ' '                    TO  DC-OPTION-CODE
001562             MOVE +0                     TO  DC-ELAPSED-DAYS
001563                                             DC-ELAPSED-MONTHS
001564             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001565             IF NO-CONVERSION-ERROR
001566                 MOVE DC-GREG-DATE-1-EDIT TO AUTO-1ST-PMT-DT
001567             ELSE
001568                 MOVE SPACES             TO  AUTO-1ST-PMT-DT.
001569
001570     IF AT-TERMINATED-DT = LOW-VALUES
001571         IF AT-SCHEDULE-END-DT = LOW-VALUES
001572             MOVE SPACES                     TO  AUTO-LST-PMT-DT
001573         ELSE
001574             MOVE ' LST PMT    : '           TO  AUTO-LST-PMT-LIT
001575             IF PI-USES-PAID-TO
001576                 MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1
001577                 MOVE '6'                    TO  DC-OPTION-CODE
001578                 MOVE +1                     TO  DC-ELAPSED-DAYS
001579                 MOVE +0                     TO  DC-ELAPSED-MONTHS
001580                 PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001581                 IF NO-CONVERSION-ERROR
001582                     MOVE DC-GREG-DATE-1-EDIT TO AUTO-LST-PMT-DT
001583                 ELSE
001584                     MOVE SPACES             TO  AUTO-LST-PMT-DT
001585             ELSE
001586                 MOVE AT-SCHEDULE-END-DT     TO  DC-BIN-DATE-1
001587                 MOVE ' '                    TO  DC-OPTION-CODE
001588                 MOVE +0                     TO  DC-ELAPSED-DAYS
001589                                                 DC-ELAPSED-MONTHS
001590                 PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001591                 IF NO-CONVERSION-ERROR
001592                     MOVE DC-GREG-DATE-1-EDIT TO AUTO-LST-PMT-DT
001593                 ELSE
001594                     MOVE SPACES             TO  AUTO-LST-PMT-DT
001595     ELSE
001596         MOVE ' TERM DATE  : '               TO  AUTO-LST-PMT-LIT
001597         MOVE AT-TERMINATED-DT               TO  DC-BIN-DATE-1
001598         MOVE ' '                            TO  DC-OPTION-CODE
001599         MOVE +0                             TO  DC-ELAPSED-DAYS
001600                                                 DC-ELAPSED-MONTHS
001601         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001602         IF NO-CONVERSION-ERROR
001603             MOVE DC-GREG-DATE-1-EDIT        TO  AUTO-LST-PMT-DT
001604         ELSE
001605             MOVE SPACES                     TO  AUTO-LST-PMT-DT.
001606
001607     MOVE AUTO-PMT-HDG1          TO  MAP-HDG        (DISPLAY-CNT).
001608     MOVE AUTO-PMT-TEXT1         TO  MAP-TEXT       (DISPLAY-CNT).
001609     MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
001610     MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
001611     ADD +1 TO DISPLAY-CNT.
001612     MOVE AUTO-PMT-HDG2          TO  MAP-HDG        (DISPLAY-CNT).
001613     MOVE AUTO-PMT-TEXT2         TO  MAP-TEXT       (DISPLAY-CNT).
001614     MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
001615                                     MAP-TEXT-ATTRB (DISPLAY-CNT).
001616     MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
001617     MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
001618     MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
001619
001620     GO TO 2800-INCR-DISPLAY-CNT.
001621
001622     EJECT
001623 2300-CORRESPONDENCE-TRAILER.
001624     MOVE PI-LINE-NO             TO  CORR-LINE-NO.
001625     MOVE 'LETTER'               TO  CORR-HDG1-LIT.
001626     MOVE 'FORM: '               TO  CORR-FORM-LIT.
001627     MOVE AT-STD-LETTER-FORM     TO  CORR-FORM-TYPE.
001628
001629     MOVE ' DT SENT: '                   TO  CORR-DT-SENT-LIT.
001630
001631     IF AT-LETTER-SENT-DT = LOW-VALUES
001632         MOVE SPACES                     TO  CORR-DT-SENT
001633     ELSE
001634         MOVE AT-LETTER-SENT-DT          TO  DC-BIN-DATE-1
001635         MOVE ' '                        TO  DC-OPTION-CODE
001636         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001637         IF NO-CONVERSION-ERROR
001638             MOVE DC-GREG-DATE-1-EDIT    TO  CORR-DT-SENT
001639         ELSE
001640             MOVE SPACES                 TO  CORR-DT-SENT.
001641
001642        MOVE ' INIT PRT: '               TO  CORR-INIT-PRT-LIT.
001643
001644     IF AT-INITIAL-PRINT-DATE = LOW-VALUES
001645         MOVE SPACES                     TO  CORR-INIT-PRT-DT
001646     ELSE
001647         MOVE AT-INITIAL-PRINT-DATE      TO  DC-BIN-DATE-1
001648         MOVE ' '                        TO  DC-OPTION-CODE
001649         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001650         IF NO-CONVERSION-ERROR
001651             MOVE DC-GREG-DATE-1-EDIT    TO  CORR-INIT-PRT-DT
001652         ELSE
001653             MOVE SPACES                 TO  CORR-INIT-PRT-DT.
001654
001655     MOVE ' TO: '                TO  CORR-ADDR-LIT.
001656     MOVE AT-ADDRESEE-TYPE       TO  CORR-ADDR-TYPE.
001657
001658     IF AT-LETTER-TO-BENE EQUAL 'Y'
001659        MOVE 'LETTER TO BENE'    TO CORR-LET-TO-BEN
001660     ELSE
001661        IF AT-AUTH-RCVD >  SPACES
001662           MOVE ' AUTH RCVD: ' TO CORR-LET-TO-BEN
001663           MOVE AT-AUTH-RCVD TO CORR-LET-TO-BEN(13:1)
001664        ELSE
001665           MOVE SPACES              TO CORR-LET-TO-BEN
001666        END-IF
001667     END-IF.
001668
001669     MOVE 'DATE: '                          TO  CORR-RESEND-LIT.
001670
001671     IF AT-RESEND-PRINT-DATE = LOW-VALUES
001672         IF AT-AUTO-RE-SEND-DT = LOW-VALUES
001673             MOVE SPACES                    TO  CORR-RESEND-DT
001674         ELSE
001675             MOVE 'RSND'                    TO  CORR-HDG2-LIT
001676             MOVE AT-AUTO-RE-SEND-DT        TO  DC-BIN-DATE-1
001677             MOVE ' '                       TO  DC-OPTION-CODE
001678             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001679             IF NO-CONVERSION-ERROR
001680                 MOVE DC-GREG-DATE-1-EDIT   TO  CORR-RESEND-DT
001681             ELSE
001682                 MOVE SPACES                TO  CORR-RESEND-DT
001683     ELSE
001684         MOVE 'RSNT'                        TO  CORR-HDG2-LIT
001685         MOVE AT-RESEND-PRINT-DATE          TO  DC-BIN-DATE-1
001686         MOVE ' '                           TO  DC-OPTION-CODE
001687         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001688         IF NO-CONVERSION-ERROR
001689             MOVE DC-GREG-DATE-1-EDIT       TO  CORR-RESEND-DT
001690         ELSE
001691             MOVE SPACES                    TO  CORR-RESEND-DT.
001692
001693     IF AT-STOP-LETTER-DT NOT = LOW-VALUES AND SPACES
001694        AND (AT-LETTER-ANSWERED-DT = LOW-VALUES  OR
001695             AT-LETTER-ANSWERED-DT > AT-STOP-LETTER-DT)
001696           MOVE '    STOP:'             TO  CORR-RECVD-LIT
001697           MOVE AT-STOP-LETTER-DT       TO  DC-BIN-DATE-1
001698           MOVE ' '                     TO  DC-OPTION-CODE
001699           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001700           IF NO-CONVERSION-ERROR
001701             MOVE DC-GREG-DATE-1-EDIT   TO  CORR-RECVD-DT
001702           ELSE
001703             MOVE SPACES                TO  CORR-RECVD-DT
001704           END-IF
001705     ELSE
001706        IF AT-LETTER-SENT-DT = LOW-VALUES OR SPACES
001707            MOVE 'MAIL RCVD'    TO CORR-HDG1-LIT
001708        END-IF
001709        MOVE ' RECVD  : '               TO  CORR-RECVD-LIT
001710        IF AT-LETTER-ANSWERED-DT = LOW-VALUES
001711            MOVE SPACES                 TO  CORR-RECVD-DT
001712        ELSE
001713            MOVE AT-LETTER-ANSWERED-DT  TO  DC-BIN-DATE-1
001714            MOVE ' '                    TO  DC-OPTION-CODE
001715            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001716            IF NO-CONVERSION-ERROR
001717                MOVE DC-GREG-DATE-1-EDIT TO CORR-RECVD-DT
001718            ELSE
001719                MOVE SPACES             TO  CORR-RECVD-DT
001720            END-IF
001721        END-IF
001722     END-IF.
001723
001724     MOVE ' FOLLOW UP  : '              TO  CORR-FOLLOW-UP-LIT.
001725
001726     IF AT-RECEIPT-FOLLOW-UP = LOW-VALUES
001727         MOVE SPACES                    TO  CORR-FOLLOW-UP-DT
001728     ELSE
001729         MOVE AT-RECEIPT-FOLLOW-UP      TO  DC-BIN-DATE-1
001730         MOVE ' '                       TO  DC-OPTION-CODE
001731         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001732         IF NO-CONVERSION-ERROR
001733             MOVE DC-GREG-DATE-1-EDIT   TO  CORR-FOLLOW-UP-DT
001734         ELSE
001735             MOVE SPACES                TO  CORR-FOLLOW-UP-DT.
001736
001737     MOVE ' ARC: '               TO  CORR-ARCH-LIT.
001738     MOVE AT-LETTER-ARCHIVE-NO   TO  CORR-ARCH-NO.
001739
001740     MOVE CORR-HDG1              TO  MAP-HDG        (DISPLAY-CNT).
001741     MOVE CORR-TEXT-1            TO  MAP-TEXT       (DISPLAY-CNT).
001742     MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
001743     MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
001744     ADD +1 TO DISPLAY-CNT.
001745     MOVE CORR-HDG2              TO  MAP-HDG        (DISPLAY-CNT).
001746     MOVE CORR-TEXT-2            TO  MAP-TEXT       (DISPLAY-CNT).
001747     MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
001748                                     MAP-TEXT-ATTRB (DISPLAY-CNT).
001749     MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
001750     MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
001751     MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
001752
001753     GO TO 2800-INCR-DISPLAY-CNT.
001754
001755     EJECT
001756 2400-GENERAL-INFO-TRAILER.
001757
001758     MOVE SPACES                 TO GEN-INFO-LINE-1
001759                                    GEN-INFO-LINE-2.
001760
001761     IF AT-PAYMENT-NOTE
001762         IF DIRECTION-SWITCH = 'B'
001763             GO TO 2060-BROWSE-BACKWARD
001764         ELSE
001765             GO TO 2020-BROWSE-FORWARD.
001766
001767     MOVE PI-LINE-NO             TO  GI-LINE-NO.
001768
001769     evaluate true
001770        when at-maint-note
001771           MOVE 'MAINT'          TO GI-HDG1-LIT
001772        when at-call-note
001773           MOVE 'CALL'           TO GI-HDG1-LIT
001774           IF AT-PHONE-CALL-IN
001775               MOVE '     IN'    TO GI-HDG2-LIT
001776           ELSE
001777               MOVE '     OUT'   TO GI-HDG2-LIT
001778           END-IF
001779        when at-cert-change
001780           MOVE 'CERT CHG'       TO GI-HDG1-LIT
001781        when at-errors-note
001782           move  'NOTE'          TO  GI-HDG1-LIT
001783           perform 7520-read-claim-mstr
001784                                 thru 7520-exit
001785           if resp-normal
001786              move cl-claim-type to pi-save-type
001787           end-if
001788           move error-message-interface-block
001789                                 to ws-save-error-interface-block
001790           perform varying s1 from +1 by +1 until
001791              at-note-error-no (s1) = spaces
001792              move at-note-error-no (s1)
001793                                 to emi-error
001794              PERFORM 9900-ERROR-FORMAT
001795                                 THRU 9900-EXIT
001796              if at-note-error-no (s1) = '1653'
001797                 evaluate true
001798                    when pi-save-type = 'L'
001799                       move '    LF    '
001800                                 to emi-text-variable (1)
001801                    when pi-save-type = 'I'
001802                       move '    IU    '
001803                                 to emi-text-variable (1)
001804                    when pi-save-type = 'F'
001805                       move '    FL    '
001806                                 to emi-text-variable (1)
001807                    when pi-save-type = 'B'
001808                       move ' BR  '
001809                                 to emi-text-variable (1)
001810                    when pi-save-type = 'H'
001811                       move ' HS '
001812                                 to emi-text-variable (1)
001813                    when pi-save-type = 'O'
001814                       move '    OT    '
001815                                 to emi-text-variable (1)
001816                    when other
001817                       move '    AH    '
001818                                 to emi-text-variable (1)
001819                 end-evaluate
001820              end-if
001821              move 'MSG : '      to gen-info-msg-1-lit
001822              move emi-line1 (8:64)
001823                                 to gen-info-msg-1
001824              move gen-info-hdg1 to map-hdg (display-cnt)
001825              move gen-info-text-1
001826                                 to map-text (display-cnt)
001827              move al-sabon      to map-hdg-attrb (display-cnt)
001828                                    map-text-attrb (display-cnt)
001829              add +1 to display-cnt pi-line-no
001830           end-perform
001831           if s1 > +1
001832              subtract +1 from display-cnt
001833              subtract +1 from pi-line-no
001834           end-if
001835           move ws-save-error-interface-block
001836                                 to error-message-interface-block
001837           MOVE TRLR-SEQ-NO      TO PI-TRLR-SEQ-NO (SUB-1)
001838           MOVE PI-LINE-NO       TO PI-TRLR-LN-NO  (SUB-1)
001839           MOVE AT-TRAILER-TYPE  TO PI-TRLR-TYPE   (SUB-1)
001840           GO TO 2800-INCR-DISPLAY-CNT
001841        when other
001842           move  'NOTE'          TO  GI-HDG1-LIT
001843     end-evaluate
001844
001845     MOVE 'MSG : '               TO  GEN-INFO-MSG-1-LIT
001846                                     GEN-INFO-MSG-2-LIT.
001847
001848     MOVE AT-INFO-LINE-1         TO  GEN-INFO-MSG-1.
001849     MOVE AT-INFO-LINE-2         TO  GEN-INFO-MSG-2.
001850
001851     MOVE GEN-INFO-HDG1          TO  MAP-HDG        (DISPLAY-CNT).
001852     MOVE GEN-INFO-TEXT-1        TO  MAP-TEXT       (DISPLAY-CNT).
001853     MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
001854     MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
001855     ADD +1 TO DISPLAY-CNT.
001856     MOVE GEN-INFO-HDG2          TO  MAP-HDG        (DISPLAY-CNT).
001857     MOVE GEN-INFO-TEXT-2        TO  MAP-TEXT       (DISPLAY-CNT).
001858     MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
001859                                     MAP-TEXT-ATTRB (DISPLAY-CNT).
001860     MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
001861     MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
001862     MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
001863
001864     GO TO 2800-INCR-DISPLAY-CNT.
001865
001866     EJECT
001867 2500-AUTO-PROMPT-TRAILER.
001868     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
001869     MOVE '5'                    TO  DC-OPTION-CODE.
001870     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
001871
001872     IF DC-BIN-DATE-1 > AT-PROMPT-END-DT
001873         IF DIRECTION-SWITCH = 'B'
001874             GO TO 2060-BROWSE-BACKWARD
001875         ELSE
001876             GO TO 2020-BROWSE-FORWARD.
001877
001878     MOVE PI-LINE-NO             TO  REM-LINE-NO.
001879     MOVE 'REMINDER'             TO  REM-HDG1-LIT.
001880     MOVE 'LN 1: '               TO  REM-LINE-1-LIT.
001881     MOVE 'LN 2: '               TO  REM-LINE-2-LIT.
001882
001883     MOVE AT-PROMPT-LINE-1       TO  REM-LINE-1.
001884     MOVE AT-PROMPT-LINE-2       TO  REM-LINE-2.
001885
001886     MOVE REMINDER-HDG1          TO  MAP-HDG        (DISPLAY-CNT).
001887     MOVE REMINDER-TEXT-1        TO  MAP-TEXT       (DISPLAY-CNT).
001888     MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
001889     MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
001890     ADD +1 TO DISPLAY-CNT.
001891     MOVE REMINDER-HDG2          TO  MAP-HDG        (DISPLAY-CNT).
001892     MOVE REMINDER-TEXT-2        TO  MAP-TEXT       (DISPLAY-CNT).
001893     MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
001894                                     MAP-TEXT-ATTRB (DISPLAY-CNT).
001895     MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
001896     MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
001897     MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
001898
001899     GO TO 2800-INCR-DISPLAY-CNT.
001900
001901     EJECT
001902 2600-DENIAL-TRAILER.
001903     MOVE PI-LINE-NO             TO  DENIAL-LINE-NO.
001904     MOVE 'DENIAL'               TO  DENIAL-HDG1-LIT.
001905     MOVE 'LN 1: '               TO  DENIAL-LN1-LIT.
001906     MOVE 'LN 2: '               TO  DENIAL-LN2-LIT.
001907
001908     MOVE AT-DENIAL-INFO-1       TO  DENIAL-LN1.
001909     MOVE AT-DENIAL-INFO-2       TO  DENIAL-LN2.
001910
001911     MOVE DENIAL-HDG1            TO  MAP-HDG        (DISPLAY-CNT).
001912     MOVE DENIAL-TEXT-1          TO  MAP-TEXT       (DISPLAY-CNT).
001913     MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
001914     MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
001915     ADD +1 TO DISPLAY-CNT.
001916     MOVE DENIAL-HDG2            TO  MAP-HDG        (DISPLAY-CNT).
001917     MOVE DENIAL-TEXT-2          TO  MAP-TEXT       (DISPLAY-CNT).
001918     MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
001919                                     MAP-TEXT-ATTRB (DISPLAY-CNT).
001920     MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
001921     MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
001922     MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
001923
001924     GO TO 2800-INCR-DISPLAY-CNT.
001925
001926     EJECT
001927 2700-INCURRED-CHANGE-TRAILER.
001928     MOVE PI-LINE-NO             TO  INCUR-LINE-NO.
001929     MOVE 'INCUR CG'             TO  INCUR-HDG1-LIT.
001930
001931     MOVE 'INC : '                       TO  INCUR-INCUR-LIT.
001932     IF AT-OLD-INCURRED-DT = LOW-VALUES
001933         MOVE SPACES                     TO  INCUR-INCUR-DT
001934     ELSE
001935         MOVE AT-OLD-INCURRED-DT         TO  DC-BIN-DATE-1
001936         MOVE ' '                        TO  DC-OPTION-CODE
001937         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001938         IF NO-CONVERSION-ERROR
001939             MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-INCUR-DT
001940         ELSE
001941             MOVE SPACES                 TO  INCUR-INCUR-DT.
001942
001943     MOVE ' REPORT : '                   TO  INCUR-REPORT-LIT.
001944     IF AT-OLD-REPORTED-DT = LOW-VALUES
001945         MOVE SPACES                     TO  INCUR-REPORT-DT
001946     ELSE
001947         MOVE AT-OLD-REPORTED-DT         TO  DC-BIN-DATE-1
001948         MOVE ' '                        TO  DC-OPTION-CODE
001949         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001950         IF NO-CONVERSION-ERROR
001951             MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-REPORT-DT
001952         ELSE
001953             MOVE SPACES                 TO  INCUR-REPORT-DT.
001954
001955     MOVE ' ESTABLISH  : '               TO  INCUR-ESTAB-LIT.
001956     IF AT-OLD-ESTABLISHED-DT = LOW-VALUES
001957         MOVE SPACES                     TO  INCUR-ESTAB-DT
001958     ELSE
001959         MOVE AT-OLD-ESTABLISHED-DT      TO  DC-BIN-DATE-1
001960         MOVE ' '                        TO  DC-OPTION-CODE
001961         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001962         IF NO-CONVERSION-ERROR
001963             MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-ESTAB-DT
001964         ELSE
001965             MOVE SPACES                 TO  INCUR-ESTAB-DT.
001966
001967     MOVE 'PAID'                         TO  INCUR-HDG2-LIT.
001968     IF PI-USES-PAID-TO
001969         MOVE '  TO: '                   TO  INCUR-PD-THRU-LIT
001970     ELSE
001971         MOVE 'THRU: '                   TO  INCUR-PD-THRU-LIT.
001972
001973     IF AT-OLD-PAID-THRU-DT = LOW-VALUES
001974         MOVE SPACES                     TO  INCUR-PD-THRU-DT
001975     ELSE
001976         MOVE AT-OLD-PAID-THRU-DT        TO  DC-BIN-DATE-1
001977         MOVE ' '                        TO  DC-OPTION-CODE
001978         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001979         IF NO-CONVERSION-ERROR
001980             MOVE DC-GREG-DATE-1-EDIT    TO  INCUR-PD-THRU-DT
001981         ELSE
001982             MOVE SPACES                 TO  INCUR-PD-THRU-DT.
001983
001984     MOVE ' TOT AMT: '           TO  INCUR-TOT-PD-LIT.
001985     MOVE ' TOT DAYS PD: '       TO  INCUR-TOT-DAYS-LIT.
001986     MOVE ' NO PMTS : '          TO  INCUR-NO-PMTS-LIT.
001987     MOVE AT-OLD-TOTAL-PAID      TO  INCUR-TOT-PD.
001988     MOVE AT-OLD-DAYS-PAID       TO  INCUR-TOT-DAYS-PD.
001989     MOVE AT-OLD-NO-OF-PMTS      TO  INCUR-NO-PMTS.
001990
001991     MOVE INCUR-CHG-HDG1         TO  MAP-HDG        (DISPLAY-CNT).
001992     MOVE INCUR-TEXT-1           TO  MAP-TEXT       (DISPLAY-CNT).
001993     MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
001994     MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
001995     ADD +1 TO DISPLAY-CNT.
001996     MOVE INCUR-CHG-HDG2         TO  MAP-HDG        (DISPLAY-CNT).
001997     MOVE INCUR-TEXT-2           TO  MAP-TEXT       (DISPLAY-CNT).
001998     MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
001999                                     MAP-TEXT-ATTRB (DISPLAY-CNT).
002000     MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
002001     MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
002002     MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
002003
002004     GO TO 2800-INCR-DISPLAY-CNT.
002005
002006     EJECT
002007 2710-FORM-CONTROL-TRAILER.
002008     MOVE PI-LINE-NO             TO  FORM-LINE-NO.
002009     MOVE 'FORM    '             TO  FORM-HDG1-LIT.
002010
002011     MOVE 'FORM: '               TO  FORM-TYPE-LIT.
002012
002013     IF INITIAL-FORM
002014        MOVE 'INIT'              TO  FORM-TYPE
002015     ELSE
002016        MOVE 'PROG'              TO  FORM-TYPE.
002017
002018     IF AT-FORM-PRINTED-DT = LOW-VALUES
002019         MOVE ' SEND ON: '                   TO  FORM-SEND-ON-LIT
002020         IF AT-FORM-SEND-ON-DT = LOW-VALUES
002021             MOVE SPACES                     TO  FORM-SEND-ON-DT
002022         ELSE
002023             MOVE AT-FORM-SEND-ON-DT         TO  DC-BIN-DATE-1
002024             MOVE ' '                        TO  DC-OPTION-CODE
002025             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002026             IF NO-CONVERSION-ERROR
002027                 MOVE DC-GREG-DATE-1-EDIT    TO  FORM-SEND-ON-DT
002028             ELSE
002029                 MOVE SPACES                 TO  FORM-SEND-ON-DT
002030     ELSE
002031         MOVE ' SENT ON: '                   TO  FORM-SEND-ON-LIT
002032         MOVE AT-FORM-PRINTED-DT             TO  DC-BIN-DATE-1
002033         MOVE ' '                            TO  DC-OPTION-CODE
002034         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002035         IF NO-CONVERSION-ERROR
002036             MOVE DC-GREG-DATE-1-EDIT        TO  FORM-SEND-ON-DT
002037         ELSE
002038             MOVE SPACES                     TO  FORM-SEND-ON-DT.
002039
002040     IF AT-FORM-REPRINT-DT = LOW-VALUES
002041         MOVE ' RESEND     : '               TO  FORM-RESEND-LIT
002042         IF AT-FORM-RE-SEND-DT = LOW-VALUES
002043             MOVE SPACES                     TO  FORM-RESEND-DT
002044         ELSE
002045             MOVE AT-FORM-RE-SEND-DT         TO  DC-BIN-DATE-1
002046             MOVE ' '                        TO  DC-OPTION-CODE
002047             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002048             IF NO-CONVERSION-ERROR
002049                 MOVE DC-GREG-DATE-1-EDIT    TO  FORM-RESEND-DT
002050             ELSE
002051                 MOVE SPACES                 TO  FORM-RESEND-DT
002052     ELSE
002053         MOVE ' RESENT     : '               TO  FORM-RESEND-LIT
002054         MOVE AT-FORM-REPRINT-DT             TO  DC-BIN-DATE-1
002055         MOVE ' '                            TO  DC-OPTION-CODE
002056         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002057         IF NO-CONVERSION-ERROR
002058             MOVE DC-GREG-DATE-1-EDIT        TO  FORM-RESEND-DT
002059         ELSE
002060             MOVE SPACES                     TO  FORM-RESEND-DT.
002061
002062     MOVE ' FOL: '                       TO  FORM-FOLLOW-UP-LIT.
002063
002064     IF AT-FORM-FOLLOW-UP-DT = LOW-VALUES
002065         MOVE SPACES                     TO  FORM-FOLLOW-UP-DT
002066     ELSE
002067         MOVE AT-FORM-FOLLOW-UP-DT       TO  DC-BIN-DATE-1
002068         MOVE ' '                        TO  DC-OPTION-CODE
002069         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002070         IF NO-CONVERSION-ERROR
002071             MOVE DC-GREG-DATE-1-EDIT    TO  FORM-FOLLOW-UP-DT
002072         ELSE
002073             MOVE SPACES                 TO  FORM-FOLLOW-UP-DT.
002074
002075     MOVE 'REC '                      TO  FORM-HDG2-LIT.
002076     MOVE 'INS : '                    TO  FORM-REC-INS-LIT.
002077
002078     IF AT-FORM-ANSWERED-DT = LOW-VALUES
002079         MOVE SPACES                  TO  FORM-REC-INS-DT
002080     ELSE
002081         MOVE AT-FORM-ANSWERED-DT     TO  DC-BIN-DATE-1
002082         MOVE ' '                     TO  DC-OPTION-CODE
002083         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002084         IF NO-CONVERSION-ERROR
002085             MOVE DC-GREG-DATE-1-EDIT TO  FORM-REC-INS-DT
002086         ELSE
002087             MOVE SPACES              TO  FORM-REC-INS-DT.
002088
002089     MOVE ' REC PHY: '                 TO  FORM-REC-PHY-LIT.
002090
002091     IF AT-PHY-FORM-ANSWERED-DT = LOW-VALUES
002092         MOVE SPACES                   TO  FORM-REC-PHY-DT
002093     ELSE
002094         MOVE AT-PHY-FORM-ANSWERED-DT  TO  DC-BIN-DATE-1
002095         MOVE ' '                      TO  DC-OPTION-CODE
002096         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002097         IF NO-CONVERSION-ERROR
002098             MOVE DC-GREG-DATE-1-EDIT  TO  FORM-REC-PHY-DT
002099         ELSE
002100             MOVE SPACES               TO  FORM-REC-PHY-DT.
002101
002102     MOVE ' REC EMP    : '             TO  FORM-REC-EMP-LIT.
002103
002104     IF AT-EMP-FORM-ANSWERED-DT = LOW-VALUES
002105         MOVE SPACES                   TO  FORM-REC-EMP-DT
002106     ELSE
002107         MOVE AT-EMP-FORM-ANSWERED-DT  TO  DC-BIN-DATE-1
002108         MOVE ' '                      TO  DC-OPTION-CODE
002109         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002110         IF NO-CONVERSION-ERROR
002111             MOVE DC-GREG-DATE-1-EDIT  TO  FORM-REC-EMP-DT
002112         ELSE
002113             MOVE SPACES               TO  FORM-REC-EMP-DT.
002114
002115     MOVE FORM-HDG1              TO  MAP-HDG        (DISPLAY-CNT).
002116     MOVE FORM-TEXT-1            TO  MAP-TEXT       (DISPLAY-CNT).
002117     MOVE AL-SABON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT).
002118     MOVE AL-SANON               TO  MAP-TEXT-ATTRB (DISPLAY-CNT).
002119     ADD +1 TO DISPLAY-CNT.
002120     MOVE FORM-HDG2              TO  MAP-HDG        (DISPLAY-CNT).
002121     MOVE FORM-TEXT-2            TO  MAP-TEXT       (DISPLAY-CNT).
002122     MOVE AL-SANON               TO  MAP-HDG-ATTRB  (DISPLAY-CNT)
002123                                     MAP-TEXT-ATTRB (DISPLAY-CNT).
002124     MOVE TRLR-SEQ-NO            TO  PI-TRLR-SEQ-NO (SUB-1).
002125     MOVE PI-LINE-NO             TO  PI-TRLR-LN-NO  (SUB-1).
002126     MOVE AT-TRAILER-TYPE        TO  PI-TRLR-TYPE   (SUB-1).
002127
002128     GO TO 2800-INCR-DISPLAY-CNT.
002129
002130 2800-INCR-DISPLAY-CNT.
002131
002132     IF DIRECTION-SWITCH = 'F'
002133         ADD +1 TO DISPLAY-CNT
002134                   PI-LINE-NO
002135                   SUB-1
002136         IF DISPLAY-CNT > +16
002137             GO TO 2999-EXIT
002138         ELSE
002139             NEXT SENTENCE
002140     ELSE
002141         SUBTRACT +3 FROM DISPLAY-CNT
002142         SUBTRACT +1 FROM PI-LINE-NO
002143         SUBTRACT +1 FROM SUB-1
002144         IF DISPLAY-CNT < +1
002145             GO TO 2999-EXIT.
002146
002147     IF DIRECTION-SWITCH = 'B'
002148         GO TO 2060-BROWSE-BACKWARD
002149     ELSE
002150         GO TO 2020-BROWSE-FORWARD.
002151
002152 2950-NO-MORE-TRAILERS.
002153     MOVE ER-0303                TO  EMI-ERROR.
002154     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002155
002156 2999-EXIT.
002157     EXIT.
002158
002159     EJECT
002160 3000-RECEIVE-LETTERS.
002161
002162     MOVE LINENOI                TO  SUB-1.
002163
002164     IF PI-TRLR-TYPE (SUB-1) = '4'
002165         NEXT SENTENCE
002166     ELSE
002167     IF PI-TRLR-TYPE (SUB-1) = '2'
002168         MOVE ER-0667            TO  EMI-ERROR
002169         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002170         MOVE -1                 TO  LINENOL
002171         GO TO 8200-SEND-DATAONLY
002172     ELSE
002173     IF PI-TRLR-TYPE (SUB-1) = 'A'
002174         MOVE ER-0665            TO  EMI-ERROR
002175         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002176         MOVE -1                 TO  LINENOL
002177         GO TO 8200-SEND-DATAONLY
002178     ELSE
002179         MOVE ER-0660            TO  EMI-ERROR
002180         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002181         MOVE -1                 TO  LINENOL
002182         GO TO 8200-SEND-DATAONLY.
002183
002184     IF RECVDTI = SPACES
002185         MOVE LOW-VALUES                   TO  WS-RECEIVED-DATE
002186     ELSE
002187         MOVE RECVDTI                      TO  WS-DEEDIT-FIELD
002188         PERFORM 9800-DEEDIT THRU 9800-EXIT
002189         IF WS-DEEDIT-FIELD-V0 NUMERIC
002190             MOVE '4'                      TO  DC-OPTION-CODE
002191             MOVE WS-DEEDIT-FIELD-V0       TO  DC-GREG-DATE-1-MDY
002192             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002193             IF NO-CONVERSION-ERROR
002194                 MOVE DC-BIN-DATE-1        TO  WS-RECEIVED-DATE
002195                 MOVE DC-GREG-DATE-1-EDIT  TO  RECVDTO
002196                 MOVE AL-UANON             TO  RECVDTA
002197             ELSE
002198                 MOVE LOW-VALUES           TO  WS-RECEIVED-DATE.
002199
002200     MOVE PI-TRLR-SEQ-NO (SUB-1) TO  TRLR-SEQ-NO.
002201     PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.
002202
002203     MOVE WS-RECEIVED-DATE       TO  AT-LETTER-ANSWERED-DT.
002204     MOVE PI-PROCESSOR-ID        TO  AT-CORR-LAST-UPDATED-BY.
002205     MOVE SAVE-BIN-DATE          TO  AT-CORR-LAST-MAINT-DT.
002206
002207     PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.
002208
002209     PERFORM 7500-READ-CLAIM-MSTR-UPDATE THRU 7500-EXIT.
002210
002211*    IF PI-COMPANY-ID = 'DMD'
002212*        MOVE 11                 TO CL-ACTIVITY-CODE
002213*        MOVE SAVE-BIN-DATE      TO CL-ACTIVITY-MAINT-DT
002214*        MOVE 'CORR'             TO CL-ACTIVITY-MAINT-TYPE
002215*        MOVE PI-PROCESSOR-ID    TO CL-PROCESSOR-ID.
002216
002217     PERFORM 7600-REWRITE-CLAIM-MSTR THRU 7600-EXIT.
002218
002219     MOVE ER-0000                TO  EMI-ERROR.
002220     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002221     MOVE -1                     TO  LINENOL.
002222     MOVE AL-UANOF               TO  LINENOA  RECVDTA
002223                                     RECVTYPA.
002224     MOVE LOW-VALUES             TO  EL150BO.
002225     MOVE 'F'                    TO  DIRECTION-SWITCH
002226                                     PI-PREV-DIRECTION.
002227     MOVE 'Y'                    TO  PI-FIRST-TIME-SW.
002228     MOVE PI-TRLR-SEQ-NO (1)     TO  PI-PREV-SEQ-NO.
002229     SUBTRACT +1 FROM PI-PREV-SEQ-NO.
002230     GO TO 1000-SHOW-CLAIM-HISTORY.
002231
002232     EJECT
002233 4000-RECEIVE-FORMS.
002234
002235     MOVE LINENOI                TO  SUB-1.
002236
002237     IF PI-TRLR-TYPE (SUB-1) = 'A'
002238         NEXT SENTENCE
002239     ELSE
002240     IF PI-TRLR-TYPE (SUB-1) = '4'
002241         MOVE ER-0666            TO  EMI-ERROR
002242         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002243         MOVE -1                 TO  LINENOL
002244         GO TO 8200-SEND-DATAONLY
002245     ELSE
002246     IF PI-TRLR-TYPE (SUB-1) = '2'
002247         MOVE ER-0667            TO  EMI-ERROR
002248         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002249         MOVE -1                 TO  LINENOL
002250         GO TO 8200-SEND-DATAONLY
002251     ELSE
002252         MOVE ER-0661            TO  EMI-ERROR
002253         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002254         MOVE -1                 TO  LINENOL
002255         GO TO 8200-SEND-DATAONLY.
002256
002257     IF RECVTYPI = 'I' OR 'P' OR 'E'
002258         NEXT SENTENCE
002259     ELSE
002260         MOVE ER-0662            TO  EMI-ERROR
002261         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002262         MOVE -1                 TO  RECVTYPL
002263         GO TO 8200-SEND-DATAONLY.
002264
002265     IF RECVDTI = SPACES
002266         MOVE LOW-VALUES                   TO  WS-RECEIVED-DATE
002267     ELSE
002268         MOVE RECVDTI                      TO  WS-DEEDIT-FIELD
002269         PERFORM 9800-DEEDIT THRU 9800-EXIT
002270         IF WS-DEEDIT-FIELD-V0 NUMERIC
002271             MOVE '4'                      TO  DC-OPTION-CODE
002272             MOVE WS-DEEDIT-FIELD-V0       TO  DC-GREG-DATE-1-MDY
002273             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002274             IF NO-CONVERSION-ERROR
002275                 MOVE DC-BIN-DATE-1        TO  WS-RECEIVED-DATE
002276                 MOVE DC-GREG-DATE-1-EDIT  TO  RECVDTO
002277                 MOVE AL-UANON             TO  RECVDTA
002278             ELSE
002279                 MOVE LOW-VALUES           TO  WS-RECEIVED-DATE.
002280
002281     MOVE PI-TRLR-SEQ-NO (SUB-1)     TO  TRLR-SEQ-NO.
002282     PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.
002283
002284     IF RECVTYPI = 'I'
002285         MOVE WS-RECEIVED-DATE       TO  AT-FORM-ANSWERED-DT
002286     ELSE
002287     IF RECVTYPI = 'P'
002288         MOVE WS-RECEIVED-DATE       TO  AT-PHY-FORM-ANSWERED-DT
002289     ELSE
002290         MOVE WS-RECEIVED-DATE       TO  AT-EMP-FORM-ANSWERED-DT.
002291
002292     MOVE PI-PROCESSOR-ID            TO  AT-FORM-LAST-UPDATED-BY.
002293     MOVE SAVE-BIN-DATE              TO  AT-FORM-LAST-MAINT-DT.
002294
002295     PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.
002296
002297     MOVE ER-0000                TO  EMI-ERROR.
002298     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002299     MOVE -1                     TO  LINENOL.
002300     MOVE AL-UANOF               TO  LINENOA  RECVDTA
002301                                     RECVTYPA.
002302     MOVE LOW-VALUES             TO  EL150BO.
002303     MOVE 'F'                    TO  DIRECTION-SWITCH
002304                                     PI-PREV-DIRECTION.
002305     MOVE 'Y'                    TO  PI-FIRST-TIME-SW.
002306     MOVE PI-TRLR-SEQ-NO (1)     TO  PI-PREV-SEQ-NO.
002307     SUBTRACT +1 FROM PI-PREV-SEQ-NO.
002308     GO TO 1000-SHOW-CLAIM-HISTORY.
002309
002310     EJECT
002311
002312 5000-VOID-PAYMENT.
002313
002314     MOVE LINENOI                TO  SUB-1.
002315
002316     IF PI-TRLR-TYPE (SUB-1) = '2'
002317         NEXT SENTENCE
002318     ELSE
002319     IF PI-TRLR-TYPE (SUB-1) = '4'
002320         MOVE ER-0666            TO  EMI-ERROR
002321         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002322         MOVE -1                 TO  LINENOL
002323         GO TO 8200-SEND-DATAONLY
002324     ELSE
002325     IF PI-TRLR-TYPE (SUB-1) = 'A'
002326         MOVE ER-0665            TO  EMI-ERROR
002327         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002328         MOVE -1                 TO  LINENOL
002329         GO TO 8200-SEND-DATAONLY
002330     ELSE
002331         MOVE ER-0664            TO  EMI-ERROR
002332         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002333         MOVE -1                 TO  LINENOL
002334         GO TO 8200-SEND-DATAONLY.
002335
002336     PERFORM 7500-READ-CLAIM-MSTR-UPDATE THRU 7500-EXIT.
002337
002338     MOVE PI-TRLR-SEQ-NO (SUB-1) TO  TRLR-SEQ-NO.
002339     PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.
002340
002341     IF AT-CHECK-WRITTEN-DT > SAVE-BIN-DATE
002342         MOVE ER-2893            TO EMI-ERROR
002343         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002344         MOVE -1                 TO LINENOL
002345         PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
002346         PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT
002347         GO TO 8200-SEND-DATAONLY
002348     END-IF.
002349
002350     IF AT-VOID-DT = LOW-VALUES OR SPACES
002351         NEXT SENTENCE
002352     ELSE
002353         MOVE ER-0800            TO  EMI-ERROR
002354         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002355         MOVE -1                 TO  LINENOL
002356         PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
002357         PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT
002358         GO TO 8200-SEND-DATAONLY.
002359
002360     if at-ach-payment = 'Y'
002361        if pi-approval-level = '4' or '5'
002362           perform 5320-build-email
002363                                 thru 5320-exit
002364        else
002365           MOVE ER-8162          TO  EMI-ERROR
002366           PERFORM 9900-ERROR-FORMAT
002367                                 THRU 9900-EXIT
002368           MOVE -1               TO  LINENOL
002369           PERFORM 7510-UNLOCK-CLAIM-MSTR
002370                                 THRU 7510-EXIT
002371           PERFORM 7010-UNLOCK-TRLR
002372                                 THRU 7010-EXIT
002373           GO TO 8200-SEND-DATAONLY
002374        end-if
002375     end-if
002376
002377*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002378*        IF AT-CASH-PAYMENT = 'N'
002379*            IF AT-CHECK-WRITTEN-DT = SPACES OR LOW-VALUES
002380*                MOVE ER-0833    TO  EMI-ERROR
002381*                MOVE -1         TO  LINENOL
002382*                PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT
002383*                PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
002384*                PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT
002385*                GO TO 8200-SEND-DATAONLY.
002386*
002387*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002388*      IF AT-CASH-PAYMENT = 'N'
002389*        IF AT-RECORDED-DT = SAVE-BIN-DATE
002390*          NEXT SENTENCE
002391*        ELSE
002392*          IF PI-PROCESSOR-USER-ALMIGHTY = 'Y'
002393*            NEXT SENTENCE
002394*          ELSE
002395*            MOVE ER-0816        TO  EMI-ERROR
002396*            MOVE -1             TO  LINENOL
002397*            PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT
002398*            PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
002399*            PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT
002400*            GO TO 8200-SEND-DATAONLY.
002401*
002402*    IF PI-COMPANY-ID = 'DMD'
002403*        IF CLAIM-IS-CLOSED
002404*            IF SETUP-ERRORS
002405*                MOVE ER-0941    TO EMI-ERROR
002406*                MOVE -1         TO LINENOL
002407*                PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT
002408*                PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
002409*                PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT
002410*                GO TO 8200-SEND-DATAONLY
002411*            ELSE
002412*                IF BENEFITS-CHANGED
002413*                    IF SYSTEM-MODIFY-CAP
002414*                        NEXT SENTENCE
002415*                    ELSE
002416*                        MOVE ER-0942    TO EMI-ERROR
002417*                        MOVE -1         TO LINENOL
002418*                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002419*                        PERFORM 7510-UNLOCK-CLAIM-MSTR
002420*                                                  THRU 7510-EXIT
002421*                        PERFORM 7010-UNLOCK-TRLR  THRU 7010-EXIT
002422*                        GO TO 8200-SEND-DATAONLY.
002423*
002424*    IF PI-COMPANY-ID = 'DMD'
002425*        IF AT-CASH-PAYMENT = 'N'
002426*            IF SYSTEM-MODIFY-CAP
002427*                NEXT SENTENCE
002428*            ELSE
002429*                IF AT-RECORDED-DT = SAVE-BIN-DATE
002430*                    NEXT SENTENCE
002431*                ELSE
002432*                    MOVE ER-0920  TO EMI-ERROR
002433*                    MOVE -1       TO LINENOL
002434*                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002435*                    PERFORM 7510-UNLOCK-CLAIM-MSTR
002436*                                             THRU 7510-EXIT
002437*                    PERFORM 7010-UNLOCK-TRLR THRU 7010-EXIT
002438*                    GO TO 8200-SEND-DATAONLY.
002439*
002440*    IF PI-COMPANY-ID = 'DMD'
002441*       IF AT-PAYMENT-TYPE NOT = '4' AND '5' AND '6'
002442*          MOVE 'O'              TO  CL-CLAIM-STATUS
002443*       END-IF
002444*       PERFORM 5300-CREATE-DMO THRU 5300-EXIT.
002445
002446*    if pi-company-id = 'DCC' or 'VPP'
002447     if at-payment-type not = '5' and '6' and 'I'
002448        perform 5300-upd-cert-trlr thru 5300-exit
002449     end-if
002450     MOVE AT-CHECK-WRITTEN-DT    TO  WS-CHECK-WRITTEN-DT.
002451     MOVE AT-PAYMENT-APPROVAL-SW TO  WS-PMT-APPROVAL-SW.
002452     MOVE AT-AMOUNT-PAID         TO  WS-AMOUNT-PAID.
002453     MOVE AT-PAYMENT-ORIGIN      TO  WS-PAYMENT-ORIGIN.
002454     MOVE AT-CV-PMT-CODE         TO  WS-CV-PMT-CODE.
002455
002456     IF AT-TO-BE-WRITTEN-DT > ZERO
002457       AND AT-PAID-THRU-DT > CL-PAID-THRU-DT
002458        CONTINUE
002459     ELSE
002460     IF AT-PAYMENT-TYPE NOT = '5' AND '6' AND 'I'
002461         SUBTRACT AT-AMOUNT-PAID    FROM CL-TOTAL-PAID-AMT
002462         SUBTRACT AT-DAYS-IN-PERIOD FROM CL-NO-OF-DAYS-PAID
002463         IF AT-PAYMENT-TYPE NOT = '4'
002464             SUBTRACT +1 FROM CL-NO-OF-PMTS-MADE
002465             IF (AT-PAID-THRU-DT NOT = CL-PAID-THRU-DT) OR
002466                (AT-RECORDED-BY = 'ZZZZ')
002467                 NEXT SENTENCE
002468             ELSE
002469                 MOVE AT-PREV-LAST-PMT-DT    TO  CL-LAST-PMT-DT
002470                 MOVE AT-PREV-PAID-THRU-DT   TO  CL-PAID-THRU-DT
002471                 MOVE AT-PREV-LAST-PMT-AMT   TO  CL-LAST-PMT-AMT.
002472
002473     IF AT-PAYMENT-TYPE = 'I'
002474        IF CL-TOTAL-INT-PAID NUMERIC AND
002475           CL-TOTAL-INT-PAID NOT LESS THAN AT-AMOUNT-PAID
002476              SUBTRACT AT-AMOUNT-PAID FROM CL-TOTAL-INT-PAID
002477        END-IF
002478     END-IF.
002479
002480     IF CL-NO-OF-DAYS-PAID < ZERO
002481         MOVE +0                 TO  CL-NO-OF-DAYS-PAID.
002482
002483     IF CL-NO-OF-PMTS-MADE < ZERO
002484         MOVE +0                 TO  CL-NO-OF-PMTS-MADE.
002485
002486     MOVE SAVE-BIN-DATE          TO  AT-VOID-DT
002487                                     CL-LAST-REOPEN-DT.
002488
002489     MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
002490     MOVE '1'                    TO  CNTL-REC-TYPE.
002491     MOVE SPACES                 TO  CNTL-ACCESS.
002492     MOVE +0                     TO  CNTL-SEQ-NO.
002493     MOVE 'CNTL'                 TO  FILE-SWITCH.
002494
002495     PERFORM 7900-READ-CONTROL-FILE THRU 7900-EXIT.
002496
002497     MOVE CF-PAYMENT-APPROVAL-SW TO  WS-CF-PMT-APPROVAL-SW.
002498
002499     IF CF-PMT-APPROVAL-USED
002500         MOVE 'V'                TO  AT-PAYMENT-APPROVAL-SW.
002501
002502*    IF PI-COMPANY-ID = 'CID'
002503         PERFORM 9870-OUTPUT-ACTIVITY-RECORD THRU
002504                 9870-EXIT.
002505         IF ERROR-ON-OUTPUT
002506             MOVE -1             TO ENTERPFL
002507             MOVE AL-UANON       TO ENTERPFA
002508             PERFORM 9900-ERROR-FORMAT THRU
002509                     9900-EXIT
002510             GO TO 8200-SEND-DATAONLY
002511         END-IF.
002512*    END-IF.
002513
002514******************************************************************
002515**  1.  BYPASS READING THE RECON RECORD FOR THE FOLLOWING       **
002516**      REASONS:                                                **
002517**      A.  NON-CASH PAYMENT                                    **
002518**      B.  CHECK HAS NOT BEEN PRINTED                          **
002519**      C.  USER IS NOT A RECON USER                            **
002520**      D.  PAYMENT IS A MANUAL (OFFLINE) PAYMENT               **
002521******************************************************************
002522
002523     IF AT-CASH-PAYMENT = 'N'
002524         GO TO 5005-CONT-VOID.
002525
002526     IF AT-CHECK-NO  = SPACES OR LOW-VALUES
002527         GO TO 5005-CONT-VOID.
002528
002529     IF CF-CLAIMS-CHECK-RECON-USER NOT = 'Y'
002530         GO TO 5005-CONT-VOID.
002531
002532     IF OFFLINE-PMT
002533         GO TO 5005-CONT-VOID.
002534
002535******************************************************************
002536**  1.  RECON SW VALUES = :                                     **
002537**      A.  R = CHECK HAS BEEN REDEEMED - CANNOT BE VOIDED      **
002538**      B.  X = RECON RECORD NOT FOUND                          **
002539******************************************************************
002540
002541     MOVE 'RCON'                     TO  FILE-SWITCH.
002542     PERFORM 5700-UPDATE-RECON THRU 5700-EXIT.
002543     IF WS-RECON-SW = 'R'
002544          PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
002545          PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT
002546          GO TO 8200-SEND-DATAONLY.
002547
002548*    IF WS-RECON-SW = 'X'
002549*        IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CIG' OR 'CUK'
002550*            PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT
002551*            PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT
002552*            GO TO 8200-SEND-DATAONLY.
002553
002554 5005-CONT-VOID.
002555     IF AT-RECORDED-BY = 'ZZZZ'
002556         GO TO 5010-BYPASS.
002557
002558     MOVE '7'                    TO  PI-PAY-TYPE.
002559     MOVE AT-PAYMENT-TYPE        TO  WS-PAY-TYPE.
002560
002561*    IF PI-COMPANY-ID = 'DMD'
002562*        MOVE 88888888           TO WS-CK-Q-CONTROL
002563*     ELSE
002564         MOVE 99999999           TO WS-CK-Q-CONTROL.
002565
002566     IF AT-CHECK-QUE-CONTROL > ZEROS AND < WS-CK-Q-CONTROL
002567         PERFORM 5200-UPDATE-CHECK-QUE THRU 5299-EXIT
002568     ELSE
002569         IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES AND SPACES
002570             MOVE 'Y'                TO  WS-PRINTED-SW
002571                                         WS-RELEASED-SW
002572         ELSE
002573             MOVE 'N'                TO  WS-PRINTED-SW
002574                                         WS-RELEASED-SW.
002575
002576 5010-BYPASS.
002577
002578     IF PAYMENT-HAS-BEEN-PRINTED OR
002579        OFFLINE-PMT
002580         MOVE CF-CURRENT-MONTH-END   TO  AT-VOID-SELECT-DT
002581     ELSE
002582         MOVE LOW-VALUES             TO  AT-PMT-SELECT-DT
002583                                         AT-VOID-SELECT-DT.
002584
002585     MOVE WS-VOID-CODE           TO  AT-VOID-TYPE.
002586
002587     MOVE PI-PROCESSOR-ID        TO  AT-PAYMENT-LAST-UPDATED-BY.
002588     MOVE SAVE-BIN-DATE          TO  AT-PAYMENT-LAST-MAINT-DT.
002589
002590     PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.
002591
002592     IF WS-PAY-TYPE NOT = 'I'
002593        PERFORM 5400-UPDATE-ZERO-TRAILER
002594                                 THRU 5400-EXIT
002595     END-IF
002596
002597*    IF CL-SYSTEM-IDENTIFIER = 'CV'
002598*        PERFORM 5500-UPDATE-POLICY-MASTER THRU 5500-EXIT
002599*    ELSE
002600         PERFORM 5600-UPDATE-CERT THRU 5600-EXIT.
002601
002602     MOVE CL-CONTROL-PRIMARY     TO  ELACTQ-KEY.
002603
002604     IF WS-PAY-TYPE = '4' OR '5' OR '6' OR 'I'
002605         GO TO 5020-CONTINUE-VOID.
002606
002607*    IF CL-SYSTEM-IDENTIFIER = 'CV'
002608*        IF CL-NO-OF-PMTS-MADE > +0
002609*            GO TO 5020-CONTINUE-VOID.
002610
002611     MOVE 'O'                    TO  CL-CLAIM-STATUS.
002612
002613 5020-CONTINUE-VOID.
002614
002615     PERFORM 7600-REWRITE-CLAIM-MSTR THRU 7600-EXIT.
002616
002617     IF WS-PAYMENT-ORIGIN = '3'
002618         GO TO 5100-CONTINUE.
002619
002620     IF PAYMENT-HAS-BEEN-PRINTED AND
002621        NOT WS-CF-PMT-APPROVAL-USED
002622         GO TO 5100-CONTINUE.
002623
002624     MOVE 'ACTQ'                 TO  FILE-SWITCH.
002625     PERFORM 7700-READ-ELACTQ THRU 7799-EXIT.
002626
002627 5100-CONTINUE.
002628
002629     MOVE ER-0000                TO  EMI-ERROR.
002630     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002631     MOVE -1                     TO  LINENOL.
002632     MOVE AL-UANOF               TO  LINENOA  RECVDTA
002633                                     RECVTYPA.
002634     MOVE LOW-VALUES             TO  EL150BO.
002635     MOVE 'F'                    TO  DIRECTION-SWITCH
002636                                     PI-PREV-DIRECTION.
002637     MOVE 'Y'                    TO  PI-FIRST-TIME-SW.
002638     MOVE PI-TRLR-SEQ-NO (1)     TO  PI-PREV-SEQ-NO.
002639     SUBTRACT +1 FROM PI-PREV-SEQ-NO.
002640     GO TO 1000-SHOW-CLAIM-HISTORY.
002641
002642 EJECT
002643 5200-UPDATE-CHECK-QUE.
002644
002645     MOVE PI-COMPANY-CD          TO  CHKQ-COMP-CD.
002646     MOVE AT-CHECK-QUE-CONTROL   TO  CHKQ-CONTROL.
002647     MOVE AT-CHECK-QUE-SEQUENCE  TO  CHKQ-SEQ-NO.
002648
002649     
      * EXEC CICS HANDLE CONDITION
002650*        NOTFND   (5290-NOTFND)
002651*    END-EXEC.
      *    MOVE '"$I                   ! '' #00006967' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303036393637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002652
002653     
      * EXEC CICS READ
002654*        DATASET   ('ELCHKQ')
002655*        RIDFLD    (ELCHKQ-KEY)
002656*        SET       (ADDRESS OF CHECK-QUE)
002657*        UPDATE
002658*    END-EXEC.
           MOVE 'ELCHKQ' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00006971' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303036393731' TO DFHEIV0
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
           
002659
002660     MOVE 'Y'                    TO  WS-RELEASED-SW
002661
002662     IF CQ-TIMES-PRINTED = +0
002663         MOVE 'N'                TO  WS-PRINTED-SW
002664         GO TO 5210-DELETE-CHECK-QUE
002665     ELSE
002666         MOVE 'Y'                TO  WS-PRINTED-SW.
002667
002668     MOVE WS-VOID-CODE           TO  CQ-VOID-INDICATOR.
002669
002670     
      * EXEC CICS REWRITE
002671*        DATASET   ('ELCHKQ')
002672*        FROM      (CHECK-QUE)
002673*    END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
           MOVE 'ELCHKQ' TO DFHEIV1
      *    MOVE '&& L                  %   #00006988' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303036393838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002674
002675     GO TO 5299-EXIT.
002676
002677 5210-DELETE-CHECK-QUE.
002678
002679     
      * EXEC CICS DELETE
002680*        DATASET  ('ELCHKQ')
002681*    END-EXEC.
           MOVE 'ELCHKQ' TO DFHEIV1
      *    MOVE '&(                    &   #00006997' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303036393937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELCHKQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002682
002683     MOVE +0                     TO  AT-CHECK-QUE-CONTROL
002684                                     AT-CHECK-QUE-SEQUENCE.
002685
002686     GO TO 5299-EXIT.
002687
002688 5290-NOTFND.
002689     MOVE 'N'                    TO  WS-PRINTED-SW
002690                                     WS-RELEASED-SW.
002691 5299-EXIT.
002692     EXIT.
002693                                 EJECT
002694 5300-UPD-CERT-TRLR.
002695
002696*    if cl-insured-claim = 'S'
002697*       move +2 to s1
002698*    else
002699*       MOVE +1 to s1
002700*    end-if
002701
002702*    move +1 to s1
002703*    evaluate cl-claim-type
002704*       when 'A'
002705*          move +1 to s2
002706*       when 'I'
002707*          move +2 to s2
002708*       when 'G'
002709*          move +3 to s2
002710*       when 'L'
002711*          move +4 to s2
002712*       when 'P'
002713*          move +5 to s2
002714*    end-evaluate
002715
002716     MOVE CL-COMPANY-CD          TO CTRLR-COMP-CD
002717     MOVE CL-CERT-KEY-DATA       TO ELCRTT-KEY (2:21)
002718     MOVE CL-CERT-NO             TO CTRLR-CERT-NO
002719     MOVE 'B'                    TO CTRLR-REC-TYPE
002720
002721     
      * EXEC CICS READ
002722*       UPDATE
002723*       DATASET   ('ELCRTT')
002724*       RIDFLD    (ELCRTT-KEY)
002725*       set       (address of CERTIFICATE-TRAILERS)
002726*       RESP      (WS-RESPONSE)
002727*    END-EXEC
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00007039' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303037303339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002728
002729     IF RESP-NORMAL
002730        perform varying s1 from +1 by +1 until
002731           (s1 > +24)
002732           or (cl-claim-no = cs-claim-no (s1))
002733        end-perform
002734
002735     if s1 < +25
002736        subtract at-amount-paid from cs-total-paid (s1)
002737        if cs-total-paid (s1) < zeros
002738           move zeros            to cs-total-paid (s1)
002739        end-if
002740        subtract at-days-in-period from cs-days-paid (s1)
002741        if cs-days-paid (s1) < zeros
002742           move zeros            to cs-days-paid (s1)
002743        end-if
002744        if cl-claim-type not = 'L' and 'P' AND 'O'
002745           perform 5310-calc-rem-bens
002746                                 thru 5310-exit
002747        end-if
002748        
      * exec cics rewrite
002749*          dataset    ('ELCRTT')
002750*          from       (certificate-trailers)
002751*          resp       (ws-response)
002752*       end-exec
           MOVE LENGTH OF
            certificate-trailers
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&& L                  %  N#00007066' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303037303636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-trailers, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002753     end-if
002754
002755     .
002756 5300-EXIT.
002757     EXIT.
002758
002759 5310-calc-rem-bens.
002760
002761     move cm-ah-orig-term        to ws-max-bens
002762     if cl-critical-period not = zeros and spaces
002763        move cl-critical-period  to ws-max-bens
002764     end-if
002765
002766     move zeros to ws-tot-days-paid ws-tot-amt-paid
002767     perform varying s2 from +1 by +1 until
002768        (s2 > +24)
002769        or (cs-claim-no (s2) = spaces)
002770        if (cs-benefit-period (s2) = cl-benefit-period)
002771           and (cs-insured-type (s2) = cl-insured-type)
002772           and (cs-claim-type (s2) = cl-claim-type)
002773           compute ws-tot-days-paid =
002774              ws-tot-days-paid + cs-days-paid (s2)
002775           compute ws-tot-amt-paid =
002776              ws-tot-amt-paid + cs-total-paid (s2)
002777        end-if
002778     end-perform
002779     compute cs-remaining-bens (s1) =
002780        ws-max-bens / cm-ah-benefit-amt
002781     if cs-remaining-bens (s1) < zeros
002782        move zeros            to cs-remaining-bens (s1)
002783     end-if
002784
002785     .
002786 5310-exit.
002787     exit.
002788
002789 5320-build-email.
002790
002791*    move at-payees-name         to ws-email-payee
002792*    move at-amount-paid         to ws-email-check-amt
002793*    move pi-company-id          to ws-email-client-id
002794*    move at-check-no            to ws-email-check-no
002795*    move at-claim-no            to ws-email-claim-no
002796*    display ' email string **' ws-email-string '**'
002797*
002798*    call "SYSTEM" using ws-email-string
002799*       returning ws-email-return-cd
002800*
002801*    display ' email return code ' ws-email-return-cd
002802
002803     .
002804 5320-exit.
002805     exit.
002806
002807*5300-CREATE-DMO. Remove as dead code
002808*5300-CONT. Remove as dead code
002809*5300-NOTE-NOT-FOUND. Remove as dead code
002810*5300-EXIT. Remove as dead code
002811*5350-FORMAT-LAST-NAME-1ST. Remove as dead code
002812*5350-EXIT. Remove as dead code
002813*5360-MOVE-NAME. Remove as dead code
002814*5360-MOVE-NAME-CYCLE. Remove as dead code
002815*5360-EXIT. Remove as dead code
002816                                 EJECT
002817 5400-UPDATE-ZERO-TRAILER.
002818
002819     MOVE ELMSTR-KEY             TO  ELTRLR-KEY.
002820
002821     MOVE ZEROS                  TO  TRLR-SEQ-NO.
002822     PERFORM 7000-READ-TRLR-UPDATE THRU 7000-EXIT.
002823
002824     IF WS-PAY-TYPE = '5'
002825         SUBTRACT WS-AMOUNT-PAID FROM AT-ITD-CHARGEABLE-EXPENSE.
002826
002827     IF WS-PAY-TYPE = '6'
002828         SUBTRACT WS-AMOUNT-PAID FROM AT-ITD-PAID-EXPENSES.
002829
002830     IF AT-INITIAL-MANUAL-RESERVE NOT = ZEROS
002831         ADD WS-AMOUNT-PAID      TO  AT-CURRENT-MANUAL-RESERVE.
002832
002833 5410-CHECK-OPEN-CLOSE.
002834
002835     IF PI-PAY-TYPE = '5' OR '6'
002836         PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT.
002837
002838     IF PI-PAY-TYPE = '1' OR '4' OR '7'
002839        IF CLAIM-IS-OPEN
002840           PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT
002841           GO TO 5400-EXIT.
002842
002843     IF PI-PAY-TYPE = '2' OR '3'
002844        IF CLAIM-IS-CLOSED
002845            PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT
002846            GO TO 5400-EXIT.
002847
002848     MOVE 1                      TO  SUB.
002849
002850 5420-LOOP.
002851
002852     IF AT-OPEN-CLOSE-TYPE (SUB) = SPACES
002853         MOVE SAVE-BIN-DATE      TO  AT-OPEN-CLOSE-DATE (SUB)
002854         MOVE 'O'                TO  AT-OPEN-CLOSE-TYPE (SUB)
002855         MOVE 'FORCE'            TO  AT-OPEN-CLOSE-REASON (SUB)
002856         PERFORM 7100-REWRITE-TRLR THRU 7100-EXIT
002857         GO TO 5400-EXIT.
002858
002859     IF SUB = 6
002860      MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1)
002861      MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2)
002862      MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3)
002863      MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4)
002864      MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5)
002865      MOVE SPACES                    TO AT-OPEN-CLOSE-HISTORY (6)
002866      GO TO 5420-LOOP.
002867
002868     ADD 1                       TO  SUB.
002869     GO TO 5420-LOOP.
002870
002871 5400-EXIT.
002872     EXIT.
002873
002874     EJECT
002875*5500-UPDATE-POLICY-MASTER.
002876*
002877*    MOVE PI-COMPANY-CD          TO  PLCY-COMPANY-CD.
002878*    MOVE CL-CERT-CARRIER        TO  PLCY-CARRIER.
002879*    MOVE CL-CERT-GROUPING       TO  PLCY-GROUPING.
002880*    MOVE CL-CERT-STATE          TO  PLCY-STATE.
002881*    MOVE CL-CERT-ACCOUNT        TO  PLCY-PRODUCER.
002882*    MOVE CL-CERT-EFF-DT         TO  PLCY-EFF-DT.
002883*    MOVE CL-CV-REFERENCE-NO     TO  PLCY-REFERENCE-NO.
002884*
002885*    EXEC CICS READ
002886*        DATASET   ('MPPLCY')
002887*        RIDFLD    (EMPLCY-KEY)
002888*        SET       (ADDRESS OF POLICY-MASTER)
002889*    END-EXEC.
002890*
002891*    MOVE LOW-VALUES             TO WS-POLICY-UPDATE-WORKING-GRPS.
002892*    MOVE +0                     TO  WS-CLAIM-PAYMENT-CNT.
002893*
002894*    MOVE PM-COMPANY-CD          TO  WS-COMPANY-CD.
002895*    MOVE PM-CARRIER             TO  WS-CARRIER.
002896*    MOVE PM-GROUPING            TO  WS-GROUPING.
002897*    MOVE PM-STATE               TO  WS-STATE.
002898*    MOVE PM-PRODUCER            TO  WS-PRODUCER.
002899*    MOVE PM-POLICY-EFF-DT       TO  WS-POLICY-EFF-DT.
002900*    MOVE PM-REFERENCE-NUMBER    TO  WS-REFERENCE-NUMBER.
002901*
002902*    MOVE 'RW'                   TO  WS-EMPLCY-FUNCTION.
002903*    MOVE PI-PROCESSOR-ID        TO  WS-LAST-CHANGE-PROCESSOR.
002904*    MOVE SAVE-BIN-DATE          TO  WS-LAST-CHANGE-DT.
002905*    MOVE EIBTIME                TO  WS-LAST-CHANGE-TIME.
002906*
002907*    IF CL-CLAIM-TYPE = 'A'
002908*        GO TO 5500-UPDATE-AH-POLICY-DATA.
002909*
002910*5500-UPDATE-LF-POLICY-DATA.
002911*
002912*    IF WS-PAY-TYPE = '2'
002913*** FROM AT-PAYMENT-TYPE - TYPES 1-6 IN CONVENIENCE VALUES
002914*      IF WS-CV-PMT-CODE = '1' OR '2' OR '3' OR '4'
002915*** LIFE / HALF LIFE / ADD / HALF ADD - CONV VALUES
002916*        COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -
002917*                                        WS-AMOUNT-PAID
002918*        COMPUTE WS-CLAIM-LIFE-ITD = PM-CLAIM-LIFE-ITD -
002919*                                    WS-AMOUNT-PAID
002920*        COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.
002921*
002922*    IF WS-PAY-TYPE = '2'
002923*      IF WS-CV-PMT-CODE = '5' OR '6'
002924*** RIDER AND HALF RIDER - CONV VALUES
002925*        COMPUTE WS-CLAIM-RIDER-ITD = PM-CLAIM-RIDER-ITD -
002926*                                        WS-AMOUNT-PAID.
002927*
002928*    IF WS-PAY-TYPE = '4'
002929*** ADDITIONAL PAYMENT - TYPE 8 CONV VALUES
002930*        COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -
002931*                                        WS-AMOUNT-PAID
002932*        COMPUTE WS-CLAIM-LIFE-ITD = PM-CLAIM-LIFE-ITD -
002933*                                    WS-AMOUNT-PAID
002934*        COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.
002935*
002936*    IF WS-PAY-TYPE = '6'
002937*** NON CHARGEABLE EXPENSE - TYPE 7 IN CONVENIENCE
002938*        COMPUTE WS-CLAIM-EXPENSES-ITD = PM-CLAIM-EXPENSES-ITD -
002939*                                        WS-AMOUNT-PAID.
002940*
002941*    IF PM-CLAIM-SETTLEMENT
002942*        IF WS-CV-PMT-CODE = '1' OR '2' OR '3' OR '4'
002943*            MOVE '6'             TO WS-CURRENT-STATUS
002944*            IF PM-EXIT-DT > LOW-VALUES
002945*                MOVE HIGH-VALUES TO WS-EXIT-DT.
002946*
002947*    GO TO 5500-UPDATE-CLAIM-HISTORY.
002948*
002949*5500-UPDATE-AH-POLICY-DATA.
002950*
002951*    IF WS-PAY-TYPE = '1' OR '4'
002952*        COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -
002953*                                        WS-AMOUNT-PAID
002954*        COMPUTE WS-CLAIM-AH-ITD = PM-CLAIM-AH-ITD -
002955*                                  WS-AMOUNT-PAID
002956*        COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.
002957*
002958*    IF WS-PAY-TYPE = '6'
002959*        COMPUTE WS-CLAIM-EXPENSES-ITD = PM-CLAIM-EXPENSES-ITD -
002960*                                        WS-AMOUNT-PAID.
002961*
002962*    IF PM-CLAIM-SETTLEMENT
002963*        IF WS-CV-PMT-CODE = '2'
002964*            MOVE '6'             TO WS-CURRENT-STATUS
002965*            IF PM-EXIT-DT > LOW-VALUES
002966*                MOVE HIGH-VALUES TO WS-EXIT-DT
002967*            END-IF
002968*        ELSE
002969*            IF WS-CV-PMT-CODE = '1'
002970*                    AND
002971*               WS-CLAIM-PAYMENTS-ITD < PM-INS-TOTAL-BENEFIT
002972*                MOVE '6'             TO WS-CURRENT-STATUS
002973*                IF PM-EXIT-DT > LOW-VALUES
002974*                    MOVE HIGH-VALUES TO WS-EXIT-DT.
002975*
002976*5500-UPDATE-CLAIM-HISTORY.
002977*
002978*    IF PM-CLAIM-ATTACH-CNT = +1 OR
002979*       PM-CLAIM-INCURRED-DT = CL-INCURRED-DT
002980*        NEXT SENTENCE
002981*    ELSE
002982*        GO TO 5500-FINISH-POLICY-UPDATE.
002983*
002984*    IF (PM-CLAIM-ATTACH-CNT = +1  AND
002985*        CL-NO-OF-PMTS-MADE = +0)
002986*               OR
002987*       (PM-CLAIM-PAYMENT-CNT = +0)
002988*           MOVE +0              TO  WS-CLAIM-LAST-PAYMENT-AMT
002989*           MOVE '1'             TO  WS-CLAIM-INTERFACE-SW
002990*           MOVE HIGH-VALUES     TO  WS-CLAIM-INCURRED-DT
002991*                                    WS-CLAIM-PAID-TO-DT
002992*       ELSE
002993*           MOVE CL-PAID-THRU-DT TO  WS-CLAIM-PAID-TO-DT
002994*           MOVE CL-LAST-PMT-AMT TO  WS-CLAIM-LAST-PAYMENT-AMT.
002995*
002996*5500-FINISH-POLICY-UPDATE.
002997*
002998*    IF WS-CLAIM-PAYMENT-CNT NEGATIVE
002999*        MOVE +0                 TO  WS-CLAIM-PAYMENT-CNT.
003000*
003001*    EXEC CICS LINK
003002*        PROGRAM     ('EMPLCY')
003003*        COMMAREA    (WS-POLICY-MASTER-UPDATE-AREA)
003004*        LENGTH      (WS-PM-COMM-LNGTH)
003005*    END-EXEC.
003006*
003007*    IF WS-EMPLCY-RETURN-CODE = LOW-VALUES
003008*        NEXT SENTENCE
003009*    ELSE
003010*        MOVE ER-9211            TO  EMI-ERROR
003011*        MOVE -1                 TO  LINENOL
003012*        MOVE AL-UABON           TO  LINENOA
003013*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003014*        EXEC CICS SYNCPOINT
003015*            ROLLBACK
003016*        END-EXEC
003017*        GO TO 8200-SEND-DATAONLY.
003018*
003019*5500-EXIT.
003020*    EXIT.
003021
003022     EJECT
003023 5600-UPDATE-CERT.
003024
003025     MOVE PI-COMPANY-CD          TO  CERT-COMP-CD.
003026     MOVE CL-CERT-CARRIER        TO  CERT-CARRIER.
003027     MOVE CL-CERT-GROUPING       TO  CERT-GROUPING.
003028     MOVE CL-CERT-STATE          TO  CERT-STATE.
003029     MOVE CL-CERT-ACCOUNT        TO  CERT-ACCOUNT.
003030     MOVE CL-CERT-EFF-DT         TO  CERT-EFF-DT.
003031     MOVE CL-CERT-NO             TO  CERT-CERT-NO.
003032     MOVE 'CERT'                 TO  FILE-SWITCH.
003033
003034     PERFORM 7800-READ-CERT-UPDATE.
003035
003036     IF CL-CLAIM-TYPE NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
003037         GO TO 5610-AH-VOID.
003038
003039     MOVE CM-LF-BENEFIT-CD       TO  WS-BEN-CD.
003040     MOVE WS-ACCESS              TO  CNTL-ACCESS.
003041     MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
003042     MOVE '4'                    TO  CNTL-REC-TYPE.
003043     MOVE ZEROS                  TO  CNTL-SEQ-NO.
003044     MOVE 'BENE'                 TO  FILE-SWITCH.
003045     MOVE +0                     TO  SUB.
003046     PERFORM 7200-FIND-BENEFIT THRU 7200-EXIT.
003047
003048     IF NO-BENEFIT-FOUND
003049         GO TO 8400-NOT-FOUND.
003050
003051     MOVE CF-LF-COVERAGE-TYPE (SUB)  TO  WS-LF-COVERAGE-TYPE.
003052
003053     IF PI-LIFE-OVERRIDE-L1 = 'P' OR
003054        WS-LF-COVERAGE-TYPE = 'P'
003055         IF WS-PAY-TYPE = '4'
003056             SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
003057             IF CM-LF-CURRENT-STATUS = '1' OR '2'
003058                 PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
003059                 GO TO 5600-EXIT
003060             ELSE
003061                 MOVE CM-LF-STATUS-AT-DEATH  TO
003062                                             CM-LF-CURRENT-STATUS
003063                 MOVE SPACES                 TO
003064                                             CM-LF-STATUS-AT-DEATH
003065                 MOVE LOW-VALUES             TO
003066                                             CM-LF-DEATH-EXIT-DT
003067                                             CM-LF-DEATH-DT
003068                 PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
003069                 GO TO 5600-EXIT.
003070
003071     IF WS-PAY-TYPE = '4'
003072         SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
003073         PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
003074         GO TO 5600-EXIT.
003075
003076     IF WS-PAY-TYPE = '2'
003077       OR (CL-CLAIM-TYPE = 'O' AND WS-PAY-TYPE = '3')
003078         IF CM-LF-CURRENT-STATUS = '1' OR '2'
003079             SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
003080             PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
003081             GO TO 5600-EXIT
003082         ELSE
003083           IF (CL-CLAIM-TYPE = 'O' AND WS-PAY-TYPE = '3')
003084             MOVE CM-LF-STATUS-AT-CANCEL TO  CM-LF-CURRENT-STATUS
003085             MOVE SPACES                TO CM-LF-STATUS-AT-CANCEL
003086             SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
003087             MOVE LOW-VALUES            TO  CM-LF-CANCEL-EXIT-DT
003088                                            CM-LF-CANCEL-DT
003089           ELSE
003090             MOVE CM-LF-STATUS-AT-DEATH TO  CM-LF-CURRENT-STATUS
003091             MOVE SPACES                TO  CM-LF-STATUS-AT-DEATH
003092             SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
003093             MOVE LOW-VALUES            TO  CM-LF-DEATH-EXIT-DT
003094                                            CM-LF-DEATH-DT
003095           END-IF
003096             PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
003097             GO TO 5600-EXIT
003098     ELSE
003099         GO TO 5620-UNLOCK-CERT.
003100
003101 5610-AH-VOID.
003102
003103     IF WS-PAY-TYPE = '4'
003104         SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT
003105         PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
003106         GO TO 5600-EXIT.
003107
003108     IF WS-PAY-TYPE = '3'
003109         MOVE CM-AH-STATUS-AT-SETTLEMENT
003110                                 TO  CM-AH-CURRENT-STATUS
003111         MOVE SPACES             TO  CM-AH-STATUS-AT-SETTLEMENT
003112         SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT
003113         MOVE LOW-VALUES         TO  CM-AH-SETTLEMENT-EXIT-DT
003114                                     CM-AH-SETTLEMENT-DT
003115         PERFORM 7810-REWRITE-CERT THRU 7810-EXIT
003116         GO TO 5600-EXIT
003117     ELSE
003118         GO TO 5620-UNLOCK-CERT.
003119
003120 5620-UNLOCK-CERT.
003121
003122     
      * EXEC CICS UNLOCK
003123*        DATASET   ('ELCERT')
003124*    END-EXEC.
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&*                    #   #00007440' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037343430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003125
003126 5600-EXIT.
003127     EXIT.
003128
003129     EJECT
003130 5700-UPDATE-RECON.
003131
003132     
      * EXEC CICS HANDLE CONDITION
003133*        NOTFND   (5700-NOT-FOUND)
003134*        NOTOPEN  (8500-FILE-NOTOPEN)
003135*    END-EXEC.
      *    MOVE '"$IJ                  ! ( #00007450' TO DFHEIV0
           MOVE X'2224494A2020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303037343530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003136
003137     MOVE PI-COMPANY-CD              TO  RCON-COMPANY-CD.
003138     MOVE AT-CHECK-NO                TO  RCON-CHECK-NO.
003139     MOVE 'C'                        TO  RCON-CHECK-ORIGIN.
003140     MOVE SPACES                     TO  RCON-GL-ACCOUNT-NO.
003141
003142     
      * EXEC CICS READ
003143*        DATASET   ('ELRCON')
003144*        RIDFLD    (ELRCON-KEY)
003145*        SET       (ADDRESS OF CHECK-RECONCILIATION)
003146*        UPDATE
003147*    END-EXEC.
           MOVE 'ELRCON' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00007460' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303037343630' TO DFHEIV0
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
           
003148
003149******************************************************************
003150*           IF THE CHECK HAS BEEN REDEEMED - DO NOT VOID         *
003151******************************************************************
003152
003153     IF RC-STATUS = 'R'
003154         MOVE 'R'                    TO  WS-RECON-SW
003155         MOVE ER-0823                TO  EMI-ERROR
003156         MOVE -1                     TO  LINENOL
003157         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003158         
      * EXEC CICS UNLOCK
003159*            DATASET   ('ELRCON')
003160*        END-EXEC
           MOVE 'ELRCON' TO DFHEIV1
      *    MOVE '&*                    #   #00007476' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037343736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003161         GO TO 5700-EXIT
003162     ELSE
003163         MOVE ' '                    TO  WS-RECON-SW.
003164
003165     MOVE WS-VOID-CODE               TO  RC-STATUS.
003166
003167     MOVE EIBDATE                    TO  DC-JULIAN-YYDDD.
003168     MOVE '5'                        TO  DC-OPTION-CODE.
003169     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003170     MOVE DC-GREG-DATE-1-MDY         TO  WS-WORK-DATE.
003171     IF WS-WORK-YY > 50
003172         MOVE '19'                   TO  WS-RCON-YY-1
003173         MOVE WS-WORK-YY             TO  WS-RCON-YY-2
003174         MOVE WS-WORK-MM             TO  WS-RCON-MM
003175         MOVE WS-WORK-DD             TO  WS-RCON-DD
003176     ELSE
003177         MOVE '20'                   TO  WS-RCON-YY-1
003178         MOVE WS-WORK-YY             TO  WS-RCON-YY-2
003179         MOVE WS-WORK-MM             TO  WS-RCON-MM
003180         MOVE WS-WORK-DD             TO  WS-RCON-DD.
003181
003182     MOVE WS-RCON-DATE               TO  RC-STATUS-DATE.
003183
003184     MOVE PI-PROCESSOR-ID            TO  RC-LAST-MAINT-BY.
003185     MOVE SAVE-BIN-DATE              TO  RC-LAST-MAINT-DT.
003186     MOVE EIBTIME                    TO  RC-LAST-MAINT-HHMMSS.
003187
003188     
      * EXEC CICS REWRITE
003189*        DATASET   ('ELRCON')
003190*        FROM      (CHECK-RECONCILIATION)
003191*    END-EXEC.
           MOVE LENGTH OF
            CHECK-RECONCILIATION
             TO DFHEIV11
           MOVE 'ELRCON' TO DFHEIV1
      *    MOVE '&& L                  %   #00007506' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037353036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CHECK-RECONCILIATION, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003192
003193     GO TO 5700-EXIT.
003194
003195 5700-NOT-FOUND.
003196
003197     MOVE 'X'                        TO  WS-RECON-SW.
003198     MOVE -1                         TO  LINENOL.
003199     MOVE ER-0801                    TO  EMI-ERROR.
003200     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003201
003202 5700-EXIT.
003203     EXIT.
003204
003205     EJECT
003206 7000-READ-TRLR-UPDATE.
003207
003208     MOVE PI-COMPANY-CD          TO  TRLR-COMP-CD.
003209     MOVE PI-CARRIER             TO  TRLR-CARRIER.
003210     MOVE PI-CLAIM-NO            TO  TRLR-CLAIM-NO.
003211     MOVE PI-CERT-NO             TO  TRLR-CERT-NO.
003212
003213     
      * EXEC CICS READ
003214*        DATASET   ('ELTRLR')
003215*        RIDFLD    (ELTRLR-KEY)
003216*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
003217*        UPDATE
003218*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00007531' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303037353331' TO DFHEIV0
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
           
003219
003220 7000-EXIT.
003221     EXIT.
003222
003223 7010-UNLOCK-TRLR.
003224
003225     
      * EXEC CICS UNLOCK
003226*        DATASET   ('ELTRLR')
003227*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&*                    #   #00007543' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037353433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003228
003229 7010-EXIT.
003230     EXIT.
003231
003232 7100-REWRITE-TRLR.
003233
003234     MOVE PI-PROCESSOR-ID        TO  PI-UPDATE-BY.
003235     MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS
003236                                     PI-UPDATE-HHMMSS.
003237
003238     
      * EXEC CICS REWRITE
003239*        DATASET   ('ELTRLR')
003240*        FROM      (ACTIVITY-TRAILERS)
003241*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00007556' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037353536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003242
003243 7100-EXIT.
003244     EXIT.
003245
003246     EJECT
003247 7200-FIND-BENEFIT.
003248
003249     MOVE 'N'                    TO  WS-BEN-SEARCH-SW.
003250
003251     
      * EXEC CICS HANDLE CONDITION
003252*        ENDFILE   (7200-EXIT)
003253*        NOTFND    (7200-EXIT)
003254*    END-EXEC.
      *    MOVE '"$''I                  ! ) #00007569' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303037353639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003255
003256     
      * EXEC CICS READ
003257*        DATASET   ('ELCNTL')
003258*        RIDFLD    (ELCNTL-KEY)
003259*        SET       (ADDRESS OF CONTROL-FILE)
003260*        GTEQ
003261*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00007574' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303037353734' TO DFHEIV0
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
           
003262
003263     IF CNTL-COMP-ID  NOT = CF-COMPANY-ID OR
003264        CNTL-REC-TYPE NOT = CF-RECORD-TYPE
003265         GO TO 7200-EXIT.
003266
003267     PERFORM 7200-BENEFIT-DUMMY THRU 7200-DUMMY-EXIT
003268         VARYING SUB FROM 1 BY 1 UNTIL
003269         ((SUB > 8) OR
003270         (CF-BENEFIT-CODE (SUB) = WS-BEN-CD)).
003271
003272     IF SUB NOT = 9
003273         MOVE 'Y'             TO  WS-BEN-SEARCH-SW.
003274
003275     GO TO 7200-EXIT.
003276
003277 7200-BENEFIT-DUMMY.
003278
003279 7200-DUMMY-EXIT.
003280     EXIT.
003281
003282 7200-EXIT.
003283     EXIT.
003284     EJECT
003285 7500-READ-CLAIM-MSTR-UPDATE.
003286
003287     MOVE PI-COMPANY-CD          TO  MSTR-COMP-CD.
003288     MOVE PI-CARRIER             TO  MSTR-CARRIER.
003289     MOVE PI-CLAIM-NO            TO  MSTR-CLAIM-NO.
003290     MOVE PI-CERT-NO             TO  MSTR-CERT-NO.
003291
003292     
      * EXEC CICS READ
003293*        DATASET   ('ELMSTR')
003294*        RIDFLD    (ELMSTR-KEY)
003295*        SET       (ADDRESS OF CLAIM-MASTER)
003296*        UPDATE
003297*    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00007610' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303037363130' TO DFHEIV0
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
           
003298
003299 7500-EXIT.
003300     EXIT.
003301
003302 7510-UNLOCK-CLAIM-MSTR.
003303
003304     
      * EXEC CICS UNLOCK
003305*        DATASET   ('ELMSTR')
003306*    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&*                    #   #00007622' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037363232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003307
003308 7510-EXIT.
003309     EXIT.
003310
003311 7520-READ-CLAIM-MSTR.
003312
003313     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD
003314     MOVE PI-CARRIER             TO MSTR-CARRIER
003315     MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO
003316     MOVE PI-CERT-NO             TO MSTR-CERT-NO
003317
003318     
      * EXEC CICS READ
003319*        DATASET   ('ELMSTR')
003320*        RIDFLD    (ELMSTR-KEY)
003321*        SET       (ADDRESS OF CLAIM-MASTER)
003322*        RESP      (WS-RESPONSE)
003323*    END-EXEC
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00007636' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037363336' TO DFHEIV0
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
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003324
003325     .
003326 7520-EXIT.
003327     EXIT.
003328
003329 7600-REWRITE-CLAIM-MSTR.
003330
003331     MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT.
003332     MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.
003333     MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.
003334     MOVE '3'                    TO  CL-LAST-MAINT-TYPE.
003335
003336     
      * EXEC CICS REWRITE
003337*        DATASET   ('ELMSTR')
003338*        FROM      (CLAIM-MASTER)
003339*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&& L                  %   #00007654' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037363534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003340
003341 7600-EXIT.
003342     EXIT.
003343
003344     EJECT
003345 7700-READ-ELACTQ.
003346
003347     
      * EXEC CICS HANDLE CONDITION
003348*        NOTFND   (7799-EXIT)
003349*    END-EXEC.
      *    MOVE '"$I                   ! * #00007665' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303037363635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003350
003351     
      * EXEC CICS READ
003352*        DATASET   ('ELACTQ')
003353*        RIDFLD    (ELACTQ-KEY)
003354*        SET       (ADDRESS OF ACTIVITY-QUE)
003355*        UPDATE
003356*    END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00007669' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303037363639' TO DFHEIV0
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
           
003357
003358     IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC
003359         MOVE ZEROS              TO  AQ-PMT-UNAPPROVED-COUNT.
003360
003361     IF AQ-PAYMENT-COUNTER NOT NUMERIC
003362         MOVE +0                 TO  AQ-PAYMENT-COUNTER.
003363
003364     IF WS-CF-PMT-APPROVAL-USED
003365         IF AQ-PMT-UNAPPROVED-COUNT > +0 AND
003366            WS-CHECK-WRITTEN-DT = LOW-VALUES AND
003367            WS-PMT-APPROVAL-SW = 'U'
003368             SUBTRACT +1 FROM AQ-PMT-UNAPPROVED-COUNT
003369             MOVE 'Y'            TO  WS-UPDATE-SW.
003370
003371     IF PAYMENT-NOT-PRINTED OR
003372        PAYMENT-NOT-RELEASED
003373         IF AQ-PAYMENT-COUNTER > +0
003374             SUBTRACT +1 FROM AQ-PAYMENT-COUNTER
003375             MOVE 'Y'            TO  WS-UPDATE-SW.
003376
003377     IF AQ-PAYMENT-COUNTER = +0
003378         MOVE ' '                TO  AQ-PENDING-PAYMENT-FLAG.
003379
003380     IF WS-UPDATE-SW = 'Y'
003381         IF AQ-PENDING-ACTIVITY-FLAGS = SPACES
003382             MOVE 'N'            TO  WS-UPDATE-SW
003383             GO TO 7720-DELETE-ACTIVITY-QUE
003384         ELSE
003385             MOVE 'N'            TO  WS-UPDATE-SW
003386     ELSE
003387         GO TO 7750-UNLOCK-ACTIVITY-QUE.
003388
003389 7710-REWRITE-ACTIVITY-QUE.
003390
003391     
      * EXEC CICS REWRITE
003392*        DATASET   ('ELACTQ')
003393*        FROM      (ACTIVITY-QUE)
003394*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&& L                  %   #00007709' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037373039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003395
003396     GO TO 7799-EXIT.
003397
003398 7720-DELETE-ACTIVITY-QUE.
003399
003400     
      * EXEC CICS DELETE
003401*        DATASET   ('ELACTQ')
003402*    END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&(                    &   #00007718' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303037373138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003403
003404     GO TO 7799-EXIT.
003405
003406 7750-UNLOCK-ACTIVITY-QUE.
003407
003408     
      * EXEC CICS UNLOCK
003409*        DATASET   ('ELACTQ')
003410*    END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&*                    #   #00007726' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037373236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003411
003412     MOVE 'N'                    TO  WS-UPDATE-SW.
003413
003414 7799-EXIT.
003415     EXIT.
003416     EJECT
003417 7800-READ-CERT-UPDATE.
003418
003419     
      * EXEC CICS READ
003420*        DATASET   ('ELCERT')
003421*        RIDFLD    (ELCERT-KEY)
003422*        SET       (ADDRESS OF CERTIFICATE-MASTER)
003423*        UPDATE
003424*    END-EXEC.
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00007737' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303037373337' TO DFHEIV0
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
           
003425
003426 7800-EXIT.
003427     EXIT.
003428
003429 7810-REWRITE-CERT.
003430
003431     
      * EXEC CICS REWRITE
003432*        DATASET   ('ELCERT')
003433*        FROM      (CERTIFICATE-MASTER)
003434*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&& L                  %   #00007749' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037373439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003435
003436 7810-EXIT.
003437     EXIT.
003438
003439     EJECT
003440 7900-READ-CONTROL-FILE.
003441
003442     
      * EXEC CICS READ
003443*        DATASET   ('ELCNTL')
003444*        RIDFLD    (ELCNTL-KEY)
003445*        SET       (ADDRESS OF CONTROL-FILE)
003446*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00007760' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037373630' TO DFHEIV0
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
           
003447
003448 7900-EXIT.
003449     EXIT.
003450
003451     EJECT
003452 8000-LOAD-ERROR-MESSAGES.
003453     IF EMI-NO-ERRORS
003454         GO TO 8000-EXIT.
003455
003456     IF EMI-NUMBER-OF-LINES = 1
003457         MOVE EMI-LINE1          TO  ERRMSG1O
003458         GO TO 8000-EXIT.
003459
003460     MOVE EMI-LINE1              TO  ERRMSG1O.
003461
003462 8000-EXIT.
003463     EXIT.
003464
003465 8100-SEND-INITIAL-MAP.
003466
003467     IF PI-FULL-SCREEN-SHOWN
003468         GO TO 8200-SEND-DATAONLY.
003469
003470     MOVE 'Y'                    TO PI-FULL-SCREEN-IND.
003471
003472     MOVE SAVE-DATE              TO  RUNDTEO.
003473     MOVE EIBTIME                TO  TIME-IN.
003474     MOVE TIME-OUT               TO  RUNTIMEO.
003475     PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
003476
003477*    IF PI-COMPANY-ID NOT = 'DMD'
003478         MOVE SPACES             TO PF6O.
003479         MOVE AL-SADOF           TO PF6A.
003480
003481     
      * EXEC CICS SEND
003482*        MAP      (MAP-NAME)
003483*        MAPSET   (MAPSET-NAME)
003484*        FROM     (EL150BO)
003485*        ERASE
003486*        CURSOR
003487*    END-EXEC.
           MOVE LENGTH OF
            EL150BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00007799' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303037373939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150BO, 
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
           
003488
003489     GO TO 9100-RETURN-TRAN.
003490
003491 8200-SEND-DATAONLY.
003492
003493     IF NOT PI-FULL-SCREEN-SHOWN
003494         GO TO 8100-SEND-INITIAL-MAP.
003495
003496     MOVE SAVE-DATE              TO  RUNDTEO.
003497     MOVE EIBTIME                TO  TIME-IN.
003498     MOVE TIME-OUT               TO  RUNTIMEO.
003499     PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
003500
003501     MOVE PI-CARRIER             TO  CARRO.
003502     MOVE PI-CLAIM-NO            TO  CLMNOO.
003503     MOVE PI-CERT-PRIME          TO  CERTNOO.
003504     MOVE PI-CERT-SFX            TO  SUFXO.
003505
003506*    IF PI-COMPANY-ID NOT = 'DMD'
003507         MOVE SPACES             TO PF6O.
003508         MOVE AL-SADOF           TO PF6A.
003509
003510     
      * EXEC CICS SEND
003511*        MAP      (MAP-NAME)
003512*        MAPSET   (MAPSET-NAME)
003513*        FROM     (EL150BO)
003514*        DATAONLY
003515*        CURSOR
003516*    END-EXEC.
           MOVE LENGTH OF
            EL150BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00007828' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303037383238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150BO, 
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
           
003517
003518     GO TO 9100-RETURN-TRAN.
003519
003520 8300-SEND-TEXT.
003521     
      * EXEC CICS SEND TEXT
003522*        FROM     (LOGOFF-TEXT)
003523*        LENGTH   (LOGOFF-LENGTH)
003524*        ERASE
003525*        FREEKB
003526*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00007839' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303037383339' TO DFHEIV0
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
           
003527
003528     
      * EXEC CICS RETURN
003529*        END-EXEC.
      *    MOVE '.(                    ''   #00007846' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037383436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003530
003531 8400-NOT-FOUND.
003532     IF FILE-SWITCH = 'BENE'
003533         MOVE ER-0282            TO  EMI-ERROR.
003534
003535     MOVE -1                     TO  LINENOL.
003536     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003537
003538     IF PASS-SWITCH = 'A'
003539         GO TO 8100-SEND-INITIAL-MAP
003540     ELSE
003541         GO TO 8200-SEND-DATAONLY.
003542
003543 8500-FILE-NOTOPEN.
003544
003545     IF FILE-SWITCH = 'TRLR'
003546         MOVE ER-0172            TO  EMI-ERROR.
003547
003548     IF FILE-SWITCH = 'CERT'
003549         MOVE ER-0169            TO  EMI-ERROR.
003550
003551     IF FILE-SWITCH = 'CNTL'
003552         MOVE ER-0042            TO  EMI-ERROR.
003553
003554     IF FILE-SWITCH = 'ACTQ'
003555         MOVE ER-0338            TO  EMI-ERROR.
003556
003557     IF FILE-SWITCH = 'MSTR'
003558         MOVE ER-0154            TO  EMI-ERROR.
003559
003560     IF FILE-SWITCH = 'RCON'
003561         MOVE ER-0776            TO  EMI-ERROR.
003562
003563     IF FILE-SWITCH = 'PLCY'
003564         MOVE ER-9883            TO  EMI-ERROR.
003565
003566     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003567
003568     MOVE -1                     TO  LINENOL.
003569
003570     IF PASS-SWITCH = 'A'
003571         GO TO 8100-SEND-INITIAL-MAP
003572     ELSE
003573         GO TO 8200-SEND-DATAONLY.
003574
003575 8800-UNAUTHORIZED-ACCESS.
003576     MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
003577     GO TO 8300-SEND-TEXT.
003578
003579 8810-PF23.
003580     MOVE EIBAID                 TO  PI-ENTRY-CD-1.
003581     MOVE XCTL-005               TO  PGM-NAME.
003582     GO TO 9300-XCTL.
003583
003584 9100-RETURN-TRAN.
003585     MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
003586     MOVE '150B'                 TO  PI-CURRENT-SCREEN-NO.
003587
003588     
      * EXEC CICS RETURN
003589*        TRANSID    (TRANS-ID)
003590*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
003591*        LENGTH     (PI-COMM-LENGTH)
003592*    END-EXEC.
      *    MOVE '.(CT                  ''   #00007906' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037393036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003593
003594 9200-RETURN-MAIN-MENU.
003595     MOVE XCTL-126               TO PGM-NAME.
003596     GO TO 9300-XCTL.
003597
003598 9300-XCTL.
003599     
      * EXEC CICS HANDLE CONDITION
003600*        PGMIDERR   (9350-NOT-FOUND)
003601*    END-EXEC.
      *    MOVE '"$L                   ! + #00007917' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303037393137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003602
003603     
      * EXEC CICS XCTL
003604*        PROGRAM    (PGM-NAME)
003605*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
003606*        LENGTH     (PI-COMM-LENGTH)
003607*    END-EXEC.
      *    MOVE '.$C                   %   #00007921' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037393231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003608
003609 9350-NOT-FOUND.
003610     MOVE ER-0923                TO EMI-ERROR.
003611     MOVE -1                     TO LINENOL.
003612     PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT.
003613     PERFORM 7510-UNLOCK-CLAIM-MSTR THRU 7510-EXIT.
003614     PERFORM 7010-UNLOCK-TRLR       THRU 7010-EXIT.
003615     GO TO 8200-SEND-DATAONLY.
003616
003617 9400-CLEAR.
003618     MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
003619     GO TO 9300-XCTL.
003620
003621 9500-PF12.
003622     MOVE XCTL-010               TO  PGM-NAME.
003623     GO TO 9300-XCTL.
003624
003625 9600-PGMID-ERROR.
003626     
      * EXEC CICS HANDLE CONDITION
003627*        PGMIDERR   (8300-SEND-TEXT)
003628*    END-EXEC.
      *    MOVE '"$L                   ! , #00007944' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303037393434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003629
003630     MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
003631     MOVE ' '                    TO  PI-ENTRY-CD-1.
003632     MOVE XCTL-005               TO  PGM-NAME.
003633     MOVE PGM-NAME               TO  LOGOFF-PGM.
003634     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
003635     GO TO 9300-XCTL.
003636
003637 9700-LINK-DATE-CONVERT.
003638     MOVE LINK-ELDATCV           TO PGM-NAME.
003639
003640     
      * EXEC CICS LINK
003641*        PROGRAM    (PGM-NAME)
003642*        COMMAREA   (DATE-CONVERSION-DATA)
003643*        LENGTH     (DC-COMM-LENGTH)
003644*    END-EXEC.
      *    MOVE '."C                   (   #00007958' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303037393538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003645
003646 9700-EXIT.
003647     EXIT.
003648
003649 9800-DEEDIT.
003650
003651     
      * EXEC CICS BIF DEEDIT
003652*        FIELD   (WS-DEEDIT-FIELD)
003653*        LENGTH  (WS-DEEDIT-LENGTH)
003654*    END-EXEC.
      *    MOVE '@"L                   #   #00007969' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037393639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 WS-DEEDIT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003655
003656 9800-EXIT.
003657     EXIT.
003658
003659
003660 9870-OUTPUT-ACTIVITY-RECORD.
003661
003662     
      * EXEC CICS GETMAIN
003663*        SET(ADDRESS OF DAILY-ACTIVITY-RECORD)
003664*        LENGTH(25)
003665*        INITIMG(WS-BLANK)
003666*    END-EXEC.
           MOVE 25
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007980' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037393830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-BLANK
           SET ADDRESS OF DAILY-ACTIVITY-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003667
003668     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.
003669     MOVE ELMSTR-KEY             TO DA-KEY.
003670     MOVE CL-TRAILER-SEQ-CNT     TO DA-TRAILER-SEQ-NO.
003671
003672     IF PI-PAY-TYPE EQUAL '7'
003673         MOVE 'V'                TO DA-RECORD-TYPE
003674     ELSE
003675         MOVE 'P'                TO DA-RECORD-TYPE.
003676
003677     
      * EXEC CICS HANDLE CONDITION
003678*        NOTOPEN (9870-NOTOPEN)
003679*        DUPREC (9870-EXIT)
003680*    END-EXEC.
      *    MOVE '"$J%                  ! - #00007995' TO DFHEIV0
           MOVE X'22244A252020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303037393935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003681
003682     
      * EXEC CICS WRITE
003683*        DATASET ('DLYACTV')
003684*        RIDFLD (DA-KEY)
003685*        FROM (DAILY-ACTIVITY-RECORD)
003686*        LENGTH (25)
003687*    END-EXEC.
           MOVE 'DLYACTV' TO DFHEIV1
           MOVE 25
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008000' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303038303030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DAILY-ACTIVITY-RECORD, 
                 DFHEIV11, 
                 DA-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003688
003689     MOVE 'N'                    TO ERROR-ON-OUTPUT-SW.
003690     GO TO 9870-EXIT.
003691
003692 9870-NOTOPEN.
003693
003694     MOVE '2955'                 TO EMI-ERROR.
003695     MOVE 'Y'                    TO ERROR-ON-OUTPUT-SW.
003696
003697 9870-EXIT.
003698
003699 9900-ERROR-FORMAT.
003700     IF NOT EMI-ERRORS-COMPLETE
003701         MOVE LINK-001           TO PGM-NAME
003702         
      * EXEC CICS LINK
003703*            PROGRAM    (PGM-NAME)
003704*            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
003705*            LENGTH     (EMI-COMM-LENGTH)
003706*        END-EXEC.
      *    MOVE '."C                   (   #00008020' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303038303230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003707
003708 9900-EXIT.
003709     EXIT.
003710
003711 9990-ABEND.
003712     MOVE -1                     TO  ENTERPFL.
003713     MOVE LINK-004               TO  PGM-NAME.
003714
003715     MOVE DFHEIBLK               TO  EMI-LINE1
003716     
      * EXEC CICS LINK
003717*        PROGRAM   (PGM-NAME)
003718*        COMMAREA  (EMI-LINE1)
003719*        LENGTH    (72)
003720*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00008034' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303038303334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003721
003722     MOVE EMI-LINE1              TO  ERRMSG1O.
003723     GO TO 8200-SEND-DATAONLY.
003724
003725 EJECT
003726 9995-SECURITY-VIOLATION.
003727*                            COPY ELCSCTP.
      *>>((file: ELCSCTP))
000001******************************************************************
000002*                                                                *
000003*                            ELCSCTP                             *
000004*                            VMOD=2.001                          *
000005*                                                                *
000006*   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
000007******************************************************************
000008
000009
000010     MOVE EIBDATE          TO SM-JUL-DATE.
000011     MOVE EIBTRMID         TO SM-TERMID.
000012     MOVE THIS-PGM         TO SM-PGM.
000013     MOVE EIBTIME          TO TIME-IN.
000014     MOVE TIME-OUT         TO SM-TIME.
000015     MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
000016
000017     
      * EXEC CICS LINK
000018*         PROGRAM  ('EL003')
000019*         COMMAREA (SECURITY-MESSAGE)
000020*         LENGTH   (80)
000021*    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00008063' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303038303633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000022
000023******************************************************************
000024
      *<<((file: ELCSCTP))
003728

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1501' TO DFHEIV1
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
               GO TO 0690-QIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 2950-NO-MORE-TRAILERS,
                     2950-NO-MORE-TRAILERS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 2020-BROWSE-FORWARD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 2950-NO-MORE-TRAILERS,
                     2950-NO-MORE-TRAILERS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 5290-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 5700-NOT-FOUND,
                     8500-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 7200-EXIT,
                     7200-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 7799-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 9350-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 9870-NOTOPEN,
                     9870-EXIT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1501' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
