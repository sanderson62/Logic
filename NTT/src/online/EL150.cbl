      *((program: EL150.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL150 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 05/09/95 16:19:31.
000007*                            VMOD=2.053
000008*
000009*AUTHOR.     LOGIC,INC.
000010*            DALLAS, TEXAS.
000011
000012*REMARKS.    TRANSACTION - EX23 - STATUS DISPLAY AND DISPOSITION
000013*
000014******************************************************************
000015*                   C H A N G E   L O G
000016*
000017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000018*-----------------------------------------------------------------
000019*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000020* EFFECTIVE    NUMBER
000021*-----------------------------------------------------------------
000022* 101501    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
000023*                              ADJUST REDEFINES EL150AI FILLER
000024* 062602    2002030700006  PEMA  Add note type of 'S'
000025*                                  (special review)
000026* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
000027* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST CALC
000028* 050107    2007041300002  AJRA  PREVENT CHECK FOR NON COVERED CLA
000029* 080106    2006052500001  AJRA  ADD NOTE TYPE N
000030* 041807    2006032200004  AJRA  ADD NOTE TYPE R
000031* 082707    2007032100001  PEMA  ADDITIONAL INTEREST CHANGES
000032* 102809    2008100900003  AJRA  PF19 TO NEW CERT NOTES SCREEN
000033* 042110  CR2008100900001  PEMA  ADD DENIAL TYPE
000034* 113010    2009122800001  AJRA  DISPLAY STOP DATE
000035* 061511    2011042000002  AJRA  VERIFY 2ND BENEFICIARY SSN
000036* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000037* 020613  CR2012092400007  AJRA  VERIFY CAUSAL STATE
000038* 041613  CR2013031200002  AJRA  ADD MAIL RECEIVED ACTION TYPE
000039* 061013    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
000040* 031714    2014031200001  AJRA  ALLOW LEVEL 4 & 5 TO UPDATE TOT I
000041* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000042* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000043* 100314  CR2014061900001  PEMA  Add pct of benefit funcionality
000044* 021615  CR2014062700002  PEMA  ADD XCTL TO EL1504
000045* 010816  IR2015092900001  PEMA  USE CLP STATE WHERE NEEDED
000046* 040416  CR2016021500002  TANA  ADD PF21 XCTL TO EL1284
000047* 062217  CR2017050300002  TANA  ADD AUTH RCVD
000048* 082218  CR2018051400001  TANA  Hold and Pay
000049* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000050* 102418  CR2018083000001  TANA  ADD NEW CALL TYPE
000051* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000052* 080322  CR2021100800003  TANA  Add B and H claim types
000053******************************************************************
000054
000055
000056 ENVIRONMENT DIVISION.
000057
000058     EJECT
000059 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000061 77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.
000062 77  FILLER  PIC X(32)  VALUE '********************************'.
000063 77  FILLER  PIC X(32)  VALUE '*    EL150 WORKING STORAGE     *'.
000064 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.053 *********'.
000065 77  a1                          pic s999 comp-3 value +0.
000066 77  p1                          pic s999 comp-3 value +0.
000067 77  ws-work-ben-pct             pic s9v999 comp-3 value +0.
000068 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
000069 77  WS-EDIT-AGE                 PIC S999       COMP-3 VALUE ZERO.
000070 77  ws-MAX-TOT-BEN              pic s9(7)v99 comp-3 value +0.
000071
000072*                            COPY ELCSCTM.
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
000073
000074*                            COPY ELCSCRTY.
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
000075
000076 01  TS-AREA.
000077     05  TS-PI-AREA          PIC X(1024) VALUE LOW-VALUES.
000078     05  TS-PI-SAVE-CLOAN    PIC X(25)   VALUE LOW-VALUES.
000079 01  WS-DATE-AREA.
000080     05  SAVE-DATE           PIC X(8)    VALUE SPACES.
000081     05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
000082
000083 01  ws-save-error-interface-block pic x(400) value low-values.
000084
000085 01  STANDARD-AREAS.
000086     12  TS-LENGTH           PIC S9(4)  COMP VALUE +1049.
000087     12  WS-EARNING-METHOD   PIC X      VALUE SPACES.
000088     12  WS-INDEX            PIC S9(04) COMP VALUE +0.
000089     12  GETMAIN-SPACE       PIC X       VALUE SPACE.
000090     12  crtt-map            pic x(8)    value 'EL150T'.
000091     12  SAD-MAP             PIC X(8)    VALUE 'EL150A'.
000092     12  MAP-NAME            PIC X(8)    VALUE 'EL150A'.
000093     12  MAPSET-NAME         PIC X(8)    VALUE 'EL150S'.
000094     12  TRANS-ID            PIC X(4)    VALUE 'EX23'.
000095     12  THIS-PGM            PIC X(8)    VALUE 'EL150'.
000096     12  START-TRANS-ID      PIC X(4)    VALUE 'EX58'.
000097     12  PGM-NAME            PIC X(8).
000098     12  TIME-IN             PIC S9(7).
000099     12  TIME-OUT-R  REDEFINES TIME-IN.
000100         16  FILLER          PIC X.
000101         16  TIME-OUT        PIC 99V99.
000102         16  FILLER          PIC XX.
000103     12  XCTL-005            PIC X(8)    VALUE 'EL005'.
000104     12  XCTL-010            PIC X(8)    VALUE 'EL010'.
000105     12  XCTL-126            PIC X(8)    VALUE 'EL126'.
000106     12  XCTL-1273           PIC X(8)    VALUE 'EL1273'.
000107     12  XCTL-EM1273         PIC X(8)    VALUE 'EM1273'.
000108     12  XCTL-131            PIC X(8)    VALUE 'EL131'.
000109     12  XCTL-132            PIC X(8)    VALUE 'EL132'.
000110     12  XCTL-EM131          PIC X(8)    VALUE 'EM131'.
000111     12  XCTL-141            PIC X(8)    VALUE 'EL141'.
000112     12  XCTL-142            PIC X(8)    VALUE 'EL142'.
000113     12  XCTL-1501           PIC X(8)    VALUE 'EL1501'.
000114     12  XCTL-1502           PIC X(8)    VALUE 'EL1502'.
000115     12  XCTL-151            PIC X(8)    VALUE 'EL151'.
000116     12  XCTL-152            PIC X(8)    VALUE 'EL152'.
000117     12  XCTL-153            PIC X(8)    VALUE 'EL153'.
000118     12  XCTL-154            PIC X(8)    VALUE 'EL154'.
000119     12  XCTL-156            PIC X(8)    VALUE 'EL156'.
000120     12  XCTL-EM156          PIC X(8)    VALUE 'EM156'.
000121     12  XCTL-EM1561         PIC X(8)    VALUE 'EM1561'.
000122     12  XCTL-1503           PIC X(8)    VALUE 'EL1503'.
000123     12  XCTL-1504           PIC X(8)    VALUE 'EL1504'.
000124     12  XCTL-155            PIC X(8)    VALUE 'EL155'.
000125     12  XCTL-157            PIC X(8)    VALUE 'EL157'.
000126     12  XCTL-158            PIC X(8)    VALUE 'EL158'.
000127     12  XCTL-162            PIC X(8)    VALUE 'EL162'.
000128     12  XCTL-1276           PIC X(8)    VALUE 'EL1276'.
000129     12  XCTL-EM1276         PIC X(8)    VALUE 'EM1276'.
000130     12  XCTL-1279           PIC X(8)    VALUE 'EL1279'.
000131     12  XCTL-1284           PIC X(8)    VALUE 'EL1284'.
000132     12  LINK-001            PIC X(8)    VALUE 'EL001'.
000133     12  LINK-004            PIC X(8)    VALUE 'EL004'.
000134     12  LINK-1523           PIC X(8)    VALUE 'EL1523'.
000135     12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
000136     12  LINK-ELRTRM         PIC X(8)    VALUE 'ELRTRM'.
000137     12  FILE-ID             PIC X(8).
000138     12  EMPHST-FILE-ID      PIC X(8)    VALUE 'MPPHST'.
000139     12  SC-ITEM             PIC S9(4)   VALUE +0001   COMP.
000140
000141 01  WS-HISTORY-AREA                 PIC X(50).
000142 01  WS-FLD1            REDEFINES WS-HISTORY-AREA.
000143     12  FILLER                 PIC X(38).
000144     12  WS-NUMERIC-FLD1        PIC S9(10)V99.
000145 01  WS-FLD2            REDEFINES WS-HISTORY-AREA.
000146     12  FILLER                 PIC X(45).
000147     12  WS-NUMERIC-FLD2        PIC S99V999.
000148 01  WS-FLD3            REDEFINES WS-HISTORY-AREA.
000149     12  FILLER                 PIC X(35).
000150     12  WS-NUMERIC-FLD3        PIC S9(15).
000151 01  WS-FLD4            REDEFINES WS-HISTORY-AREA.
000152     12  FILLER                 PIC X(36).
000153     12  WS-NUMERIC-FLD4        PIC S9(10)V9999.
000154
000155 01  MISC-WORK-AREAS.
000156     12  QID.
000157         16  QID-TERM        PIC X(4).
000158         16  FILLER          PIC X(4)    VALUE '150A'.
000159     12  MAP-LENGTH          PIC S9(4)   VALUE +1920   COMP.
000160     12  PASS-SWITCH         PIC X       VALUE 'A'.
000161     12  SV-LAST-BY          PIC X(4)    VALUE SPACES.
000162     12  SV-LAST-HHMMSS      PIC S9(6)   VALUE +0      COMP-3.
000163     12  SUB-1               PIC S9(4)   VALUE +0    COMP.
000164     12  DISPLAY-CNT         PIC S9(4)   VALUE +1    COMP.
000165     12  FILE-SWITCH         PIC X(4)    VALUE SPACES.
000166     12  WS-SUB              PIC 9       VALUE 0.
000167     12  RETURNED-FROM       PIC X(8)    VALUE SPACES.
000168     12  DIRECTION-SWITCH    PIC X       VALUE 'N'.
000169     12  WS-STATUS           PIC X.
000170     12  WS-LF-COVERAGE-TYPE PIC X       VALUE SPACE.
000171     12  WS-REMAINING-AMT    PIC S9(9)V99 VALUE +0.
000172     12  W-REMAINDER         PIC S9(3)    VALUE +0.
000173     12  WS-TERMS.
000174         16  WST-ORIG        PIC ZZ9.
000175         16  WST-ORIG-DAYS-GRP.
000176             20  WST-SLASH1  PIC X       VALUE '/'.
000177             20  WST-EXT-DAYS
000178                             PIC Z9.
000179         16  FILLER          PIC X       VALUE '/'.
000180         16  WST-REM         PIC ZZ9.
000181         16  WST-REM-DAYS-GRP.
000182             20  WST-SLASH2  PIC X       VALUE '/'.
000183             20  WST-REM-DAYS
000184                             PIC Z9.
000185     12  W-DAYS-PAID-X       PIC ZZZZ9.
000186     12  W-TOTAL-PAID-AMT-X  PIC ZZZZ,ZZZ.99.
000187     12  W-PAID-MONTHS       PIC S9(03)  COMP-3.
000188     12  W-PAID-DAYS         PIC S9(03)  COMP-3.
000189
000190     12  W-REM               PIC S999    COMP-3.
000191     12  W-REM-DAYS          PIC S999    COMP-3.
000192     12  W-TERM-IN-DAYS      PIC S9(4)   COMP-3.
000193     12  W-REM-TERM-IN-DAYS  PIC S9(4)   COMP-3.
000194
000195     12  W-PAYMENTS.
000196         16  W-PYMTS         PIC ZZ9.
000197         16  FILLER          PIC X       VALUE '/'.
000198         16  W-ADD-DAYS      PIC ZZ9.
000199     12  WS-ACCESS.
000200         16  FILLER          PIC XX      VALUE SPACES.
000201         16  WS-BEN-CD       PIC XX.
000202     12  WS-STATE-ACCESS.
000203         16  WS-ST-ACCESS    PIC XX.
000204         16  FILLER          PIC XX      VALUE SPACES.
000205     12  WS-CLAIM-SEQUENCE.
000206         16  FILLER          PIC X       VALUE '('.
000207         16  WS-CUR-SEQU     PIC Z9      VALUE ZEROS.
000208         16  FILLER          PIC X(04)   VALUE ' OF '.
000209         16  WS-OF-SEQU      PIC Z9      VALUE ZEROS.
000210         16  FILLER          PIC X       VALUE ')'.
000211
000212     12  WS-PURGED-MESSAGE.
000213         16  WS-PURGED-MSG       PIC X(18)    VALUE SPACES.
000214         16  WS-PURGED-DATE      PIC X(08)    VALUE SPACES.
000215
000216     12  SAVE-CONTROL        PIC X(39).
000217     12  WS-ERACCT-SAVE-KEY      PIC X(20) VALUE SPACES.
000218     12  WS-ERACCT-HOLD-RECORD   PIC X(2000) VALUE SPACES.
000219
000220     12  SUB                 PIC 99      VALUE ZEROS.
000221
000222     12  WS-ACTIVITY-CODE    PIC 99      VALUE ZEROS.
000223         88  VALID-ACTIVITY-CODE         VALUE 00, 10 THRU 17.
000224
000225     12  WS-ACT-REC-FOUND-SW PIC X       VALUE 'N'.
000226     12  WS-LETTER-SW        PIC X       VALUE 'N'.
000227     12  WS-BROWSE-SW        PIC X       VALUE 'N'.
000228     12  WS-BROWSE-START-SW  PIC X       VALUE ' '.
000229     12  WS-UPDATE-SW        PIC X       VALUE 'N'.
000230
000231     12  WS-ACT-USER-DESC.
000232         16  WS-DESC-1-3     PIC X(03)   VALUE SPACES.
000233         16  FILLER          PIC X(17)   VALUE SPACES.
000234
000235     12  WS-CERT-READ-SW     PIC X       VALUE 'N'.
000236     12  WS-LOAN-EXPIRE-DT   PIC X(08)   VALUE SPACES.
000237     12  W-CLAIM-MASTER-SAVE PIC X(350)  VALUE SPACES.
000238
000239     12  WS-PMT-AMTD.
000240         16  WS-PMT-AMT      PIC Z(4).99-.
000241
000242     12  WS-RESPONSE         PIC S9(8)   COMP.
000243         88  WS-RESP-NORMAL              VALUE +00.
000244         88  WS-RESP-NOTFND              VALUE +13.
000245
000246 01  WS-PDEF-RECORD-SW           PIC X  VALUE ' '.
000247     88  PDEF-FOUND                   VALUE 'Y'.
000248 01  ERPDEF-KEY-SAVE             PIC X(18).
000249 01  ERPDEF-KEY.
000250     12  ERPDEF-COMPANY-CD       PIC X.
000251     12  ERPDEF-STATE            PIC XX.
000252     12  ERPDEF-PROD-CD          PIC XXX.
000253     12  F                       PIC X(7).
000254     12  ERPDEF-BEN-TYPE         PIC X.
000255     12  ERPDEF-BEN-CODE         PIC XX.
000256     12  ERPDEF-EXP-DT           PIC XX.
000257
000258 01  ELCRTT-KEY.
000259     05  CTRLR-COMP-CD       PIC X.
000260     05  CTRLR-CARRIER       PIC X.
000261     05  CTRLR-GROUPING      PIC X(6).
000262     05  CTRLR-STATE         PIC X(2).
000263     05  CTRLR-ACCOUNT       PIC X(10).
000264     05  CTRLR-EFF-DT        PIC XX.
000265     05  CTRLR-CERT-NO       PIC X(11).
000266     05  CTRLR-REC-TYPE      PIC X.
000267
000268 01  ACCESS-KEYS.
000269     12  ERACCT-KEY.
000270         16  ERACCT-PARTIAL-KEY.
000271             20  ACCT-COMP-CD    PIC X.
000272             20  ACCT-CARRIER    PIC X.
000273             20  ACCT-GROUPING   PIC X(6).
000274             20  ACCT-STATE      PIC XX.
000275             20  ACCT-ACCOUNT    PIC X(10).
000276         16  ACCT-EXP-DT         PIC XX.
000277         16  FILLER              PIC X(04).
000278     12  ELMSTR-KEY.
000279         16  MSTR-COMP-CD    PIC X.
000280         16  MSTR-CARRIER    PIC X.
000281         16  MSTR-CLAIM-NO   PIC X(7).
000282         16  MSTR-CERT-NO.
000283             20  MSTR-CERT-NO-PRIME  PIC X(10).
000284             20  MSTR-CERT-NO-SUFX   PIC X.
000285     12  W-ELRETR-KEY.
000286         16  W-RET-COMP-CD       PIC  X.
000287         16  W-RET-CARRIER       PIC  X.
000288         16  W-RET-CLAIM-NO      PIC  X(07).
000289         16  W-RET-CERT-NO.
000290             20  W-RET-CERT-PRIME
000291                                 PIC  X(10).
000292             20  W-RET-CERT-SUFX PIC  X.
000293     12  ELCNTL-KEY.
000294         16  CNTL-COMP-ID    PIC X(3).
000295         16  CNTL-REC-TYPE   PIC X.
000296         16  CNTL-ACCESS.
000297             20  FILLER      PIC XX.
000298             20  CNTL-BEN-NO PIC XX.
000299         16  CNTL-SEQ-NO     PIC S9(4)    COMP.
000300     12  ELCERT-KEY.
000301         16  CERT-COMP-CD    PIC X.
000302         16  CERT-CARRIER    PIC X.
000303         16  CERT-GROUPING   PIC X(6).
000304         16  CERT-STATE      PIC XX.
000305         16  CERT-ACCOUNT    PIC X(10).
000306         16  CERT-EFF-DT     PIC XX.
000307         16  CERT-CERT-NO.
000308             20  CERT-CERT-NO-PRIME PIC X(10).
000309             20  CERT-CERT-NO-SUFX  PIC X.
000310     12  ELTRLR-KEY.
000311         16  TRLR-COMP-CD    PIC X.
000312         16  TRLR-CARRIER    PIC X.
000313         16  TRLR-CLAIM-NO   PIC X(7).
000314         16  TRLR-CERT-NO.
000315             20  TRLR-CERT-NO-PRIME PIC X(10).
000316             20  TRLR-CERT-NO-SUFX  PIC X.
000317         16  TRLR-SEQ-NO     PIC S9(4)   COMP.
000318     12  ws-ELTRLR-KEY.
000319         16  ws-TRLR-COMP-CD    PIC X.
000320         16  ws-TRLR-CARRIER    PIC X.
000321         16  ws-TRLR-CLAIM-NO   PIC X(7).
000322         16  ws-TRLR-CERT-NO.
000323             20  ws-TRLR-CERT-NO-PRIME PIC X(10).
000324             20  ws-TRLR-CERT-NO-SUFX  PIC X.
000325         16  ws-TRLR-SEQ-NO     PIC S9(4)   COMP.
000326     12  ELACTQ-KEY.
000327         16  ACTQ-COMP-CD    PIC X.
000328         16  ACTQ-CARRIER    PIC X.
000329         16  ACTQ-CLAIM-NO   PIC X(7).
000330         16  ACTQ-CERT-NO.
000331             20  ACTQ-CERT-NO-PRIME PIC X(10).
000332             20  ACTQ-CERT-NO-SUFX  PIC X.
000333     12  ELARCH-KEY.
000334         16  ARCH-COMP-CD        PIC X.
000335         16  ARCH-ARCHIVE-NO     PIC S9(08)  COMP.
000336         16  ARCH-RECORD-TYPE    PIC X.
000337         16  ARCH-SEQ-NO         PIC S9(04)  COMP.
000338
000339     12  EMPLCY-KEY.
000340         16  PLCY-COMP-CD            PIC X.
000341         16  PLCY-CARRIER            PIC X.
000342         16  PLCY-GROUPING           PIC X(06).
000343         16  PLCY-STATE              PIC XX.
000344         16  PLCY-PRODUCER           PIC X(10).
000345         16  PLCY-EFF-DT             PIC XX.
000346         16  PLCY-REFERENCE-NO.
000347             20  PLCY-REFNO-PRIME    PIC X(18).
000348             20  PLCY-REFNO-SFX      PIC XX.
000349
000350     12  EMPHST-KEY.
000351         16  EMPHST-COMPANY-CD       PIC X.
000352         16  EMPHST-CARRIER          PIC X.
000353         16  EMPHST-GROUPING         PIC X(06).
000354         16  EMPHST-STATE            PIC XX.
000355         16  EMPHST-PRODUCER         PIC X(10).
000356         16  EMPHST-POLICY-EFF-DT    PIC XX.
000357         16  EMPHST-REFERENCE-NUMBER.
000358             20  EMPHST-REFNO-PRIME  PIC X(18).
000359             20  EMPHST-REFNO-SFX    PIC XX.
000360         16  EMPHST-RECORD-TYPE      PIC XX.
000361         16  EMPHST-FIELD-TYPE       PIC XX.
000362         16  EMPHST-SEQUENCE-NO      PIC S9(04) COMP.
000363
000364     12  EMPLAN-KEY.
000365         16  PLAN-COMP-CD            PIC X.
000366         16  PLAN-CARRIER            PIC X.
000367         16  PLAN-GROUPING           PIC X(06).
000368         16  PLAN-STATE              PIC XX.
000369         16  PLAN-PRODUCER           PIC X(10).
000370         16  PLAN-PLAN-CODE          PIC XX.
000371         16  PLAN-REV-NO             PIC 9(03).
000372
000373     12  EMPROD-KEY.
000374         16  EMPROD-PARTIAL-KEY.
000375             20  PROD-COMP-CD        PIC X.
000376             20  PROD-CARRIER        PIC X.
000377             20  PROD-GROUPING       PIC X(06).
000378             20  PROD-STATE          PIC XX.
000379             20  PROD-PRODUCER       PIC X(10).
000380         16  PROD-EXP-DT             PIC XX.
000381
000382 01  TRAILER-DISPLAY-WORK-AREA.
000383     12  DISPLAY-ACTION.
000384         16  DISP-ACT-A      PIC X(10).
000385         16  DISP-ACT-B      PIC X(4).
000386     12  FILLER              PIC X.
000387     12  DISPLAY-BY          PIC X(4).
000388     12  FILLER              PIC X.
000389     12  DISPLAY-DATE        PIC X(8).
000390     12  FILLER              PIC X.
000391     12  DISPLAY-SEQ         PIC Z(4).
000392     12  FILLER              PIC X.
000393     12  DISPLAY-TEXT        PIC X(45).
000394     EJECT
000395 01  ERROR-MESSAGES.
000396     05  ER-0000                 PIC X(4) VALUE '0000'.
000397     05  ER-0004                 PIC X(4) VALUE '0004'.
000398     05  ER-0008                 PIC X(4) VALUE '0008'.
000399     05  ER-0029                 PIC X(4) VALUE '0029'.
000400     05  ER-0033                 PIC X(4) VALUE '0033'.
000401     05  ER-0042                 PIC X(4) VALUE '0042'.
000402     05  ER-0068                 PIC X(4) VALUE '0068'.
000403     05  ER-0070                 PIC X(4) VALUE '0070'.
000404     05  ER-0130                 PIC X(4) VALUE '0130'.
000405     05  ER-0154                 PIC X(4) VALUE '0154'.
000406     05  ER-0168                 PIC X(4) VALUE '0168'.
000407     05  ER-0169                 PIC X(4) VALUE '0169'.
000408     05  ER-0172                 PIC X(4) VALUE '0172'.
000409     05  ER-0190                 PIC X(4) VALUE '0190'.
000410     05  ER-0204                 PIC X(4) VALUE '0204'.
000411     05  ER-0206                 PIC X(4) VALUE '0206'.
000412     05  ER-0303                 PIC X(4) VALUE '0303'.
000413     05  ER-0334                 PIC X(4) VALUE '0334'.
000414     05  ER-0335                 PIC X(4) VALUE '0335'.
000415     05  ER-0336                 PIC X(4) VALUE '0336'.
000416     05  ER-0337                 PIC X(4) VALUE '0337'.
000417     05  ER-0338                 PIC X(4) VALUE '0338'.
000418     05  ER-0376                 PIC X(4) VALUE '0376'.
000419     05  ER-0412                 PIC X(4) VALUE '0412'.
000420     05  ER-0413                 PIC X(4) VALUE '0413'.
000421     05  ER-0433                 PIC X(4) VALUE '0433'.
000422     05  ER-0500                 PIC X(4) VALUE '0500'.
000423     05  ER-0802                 PIC X(4) VALUE '0802'.
000424     05  ER-0803                 PIC X(4) VALUE '0803'.
000425     05  ER-0804                 PIC X(4) VALUE '0804'.
000426     05  ER-0926                 PIC X(4) VALUE '0926'.
000427     05  ER-0927                 PIC X(4) VALUE '0927'.
000428     05  ER-0928                 PIC X(4) VALUE '0928'.
000429     05  ER-0929                 PIC X(4) VALUE '0929'.
000430     05  ER-0931                 PIC X(4) VALUE '0931'.
000431     05  ER-0932                 PIC X(4) VALUE '0932'.
000432     05  ER-0933                 PIC X(4) VALUE '0933'.
000433     05  ER-0934                 PIC X(4) VALUE '0934'.
000434     05  ER-0935                 PIC X(4) VALUE '0935'.
000435     05  ER-0936                 PIC X(4) VALUE '0936'.
000436     05  ER-0937                 PIC X(4) VALUE '0937'.
000437     05  ER-2378                 PIC X(4) VALUE '2378'.
000438     05  ER-2379                 PIC X(4) VALUE '2379'.
000439     05  ER-2848                 PIC X(4) VALUE '2848'.
000440     05  ER-3516                 PIC X(4) VALUE '3516'.
000441     05  ER-3526                 PIC X(4) VALUE '3526'.
000442     05  ER-3545                 PIC X(4) VALUE '3545'.
000443     05  ER-3550                 PIC X(4) VALUE '3550'.
000444     05  ER-9483                 PIC X(4) VALUE '9483'.
000445     05  ER-9808                 PIC X(4) VALUE '9808'.
000446     05  ER-9883                 PIC X(4) VALUE '9883'.
000447     05  ER-9886                 PIC X(4) VALUE '9886'.
000448
000449 01  TEXT-WORK-AREAS.
000450     12  PAYMENT-TEXT.
000451         16  PMT-VAR         PIC X(8).
000452         16  PMT-PD-THRU     PIC X(8).
000453         16  pmt-check-head  pic x(7)    value spaces.
000454*        16  FILLER          PIC X(7)    VALUE ' CHECK='.
000455         16  PMT-CHECK-NO    PIC X(7).
000456         16  FILLER          PIC X(5)    VALUE ' AMT='.
000457         16  PMT-AMOUNT      PIC Z(6).99-.
000458
000459     12  AUTO-PMT-TEXT.
000460         16  FILLER          PIC X(6)    VALUE 'START='.
000461         16  AUTO-START      PIC X(8).
000462         16  FILLER          PIC X(5)    VALUE ' END='.
000463         16  AUTO-END        PIC X(8).
000464         16  FILLER          PIC X(8)    VALUE ' AMOUNT='.
000465         16  AUTO-AMOUNT     PIC Z(6).99-.
000466
000467     12  AUTO-TERM-TEXT.
000468         16  FILLER          PIC X(11)   VALUE 'TERMINATED='.
000469         16  AUTO-TERMINATED PIC X(8).
000470         16  FILLER          PIC X(8)    VALUE SPACES.
000471         16  FILLER          PIC X(8)    VALUE ' AMOUNT='.
000472         16  AUTO-TERM-AMT   PIC Z(6).99-.
000473
000474     12  CORRESPONDENCE-TEXT.
000475         16  FILLER          PIC X(3)    VALUE 'TO='.
000476         16  CORR-TO         PIC X(8).
000477         16  CORR-RECVD-LIT  PIC X(8)    VALUE '  RECVD='.
000478         16  CORR-RECVD      PIC X(8).
000479         16  CORR-VAR        PIC X(9).
000480         16  CORR-RESENT     PIC X(8).
000481         16  CORR-ARCH-NO REDEFINES CORR-RESENT PIC 9(8).
000482
000483     12  PROMPT-TEXT.
000484         16  FILLER          PIC X(4)    VALUE 'END='.
000485         16  PROMPT-END      PIC X(8).
000486         16  FILLER          PIC XX      VALUE SPACES.
000487         16  PROMPT-MSG      PIC X(31).
000488
000489     12  DENIAL-TEXT.
000490         16  FILLER          PIC X(5)    VALUE 'DATE='.
000491         16  DENIAL-DATE     PIC X(8).
000492         16  FILLER          PIC X(6)    VALUE ' CODE='.
000493         16  DENIAL-CODE     PIC X(4).
000494         16  FILLER          PIC XX      VALUE SPACES.
000495         16  DENIAL-MSG      PIC X(20).
000496
000497     12  RECONSIDERED-TEXT.
000498         16  FILLER          PIC X(7)    VALUE 'DENIED '.
000499         16  DENIED-DATE     PIC X(8).
000500         16  FILLER          PIC X(15)   VALUE '  RECONSIDERED '.
000501         16  RECONSIDERED-DATE  PIC X(8).
000502
000503     12  INCUR-TEXT.
000504         16  FILLER          PIC X(6)    VALUE 'INCUR='.
000505         16  INCUR-DT        PIC X(8).
000506         16  PDTHRU-HEAD     PIC X(9)    VALUE ' PD THRU='.
000507         16  INCUR-PDTHRU    PIC X(8).
000508         16  FILLER          PIC X(4)    VALUE ' PD='.
000509         16  INCUR-PAID      PIC Z(6).99-.
000510
000511     12  LOAN-TEXT.
000512         16  FILLER          PIC X(5)    VALUE 'LOAN='.
000513         16  LOAN-NUMBER     PIC X(8).
000514         16  FILLER          PIC X(5)    VALUE ' BAL='.
000515         16  LOAN-BALANCE    PIC Z(7).99-.
000516         16  FILLER          PIC X(4)    VALUE ' ME='.
000517         16  LOAN-MEMBER     PIC X(12).
000518
000519     12  ASSOC-LOAN-TEXT.
000520         16  FILLER          PIC X(11)   VALUE
000521                 'ORIG/CUR : '.
000522         16  ASSOC-ORIG-LOAN PIC X(08)   VALUE SPACES.
000523         16  FILLER          PIC X       VALUE SPACES.
000524         16  ASSOC-CUR-LOAN  PIC X(12)   VALUE SPACES.
000525         16  FILLER          PIC XX      VALUE SPACES.
000526         16  ASSOC-LOAN-TYPE PIC XX      VALUE SPACES.
000527         16  FILLER          PIC X(09)   VALUE SPACES.
000528
000529     12  ASSOC-CV-LOAN-TEXT.
000530         16  FILLER          PIC X(11)   VALUE
000531                 'LOAN NO. : '.
000532         16  ASSOC-CV-LOAN   PIC X(20)   VALUE SPACES.
000533         16  FILLER          PIC X(14)   VALUE SPACES.
000534
000535 01  PAYMENT-DESCRIPTION-TABLE.
000536     12  FILLER              PIC X(14)   VALUE 'PARTIAL PMT   '.
000537     12  FILLER              PIC X(14)   VALUE 'FINAL PMT     '.
000538     12  FILLER              PIC X(14)   VALUE 'LUMP SUM PMT  '.
000539     12  FILLER              PIC X(14)   VALUE 'ADDITIONAL PMT'.
000540     12  FILLER              PIC X(14)   VALUE 'CHARGEABLE EXP'.
000541     12  FILLER              PIC X(14)   VALUE 'NON-CHG EXP   '.
000542     12  FILLER              PIC X(14)   VALUE 'LIFE PRM RFND '.
000543     12  FILLER              PIC X(14)   VALUE 'A/H PRM RFND  '.
000544     12  FILLER              PIC X(14)   VALUE 'ENTRY CORRECT '.
000545 01  PAYMENT-DESC-R   REDEFINES PAYMENT-DESCRIPTION-TABLE.
000546     12  PAY-DESC            PIC X(14)   OCCURS 9.
000547
000548 01  CV-PAYMENT-DESCRIPTION-TABLE.
000549     12  FILLER              PIC X(14)   VALUE 'FULL DEATH    '.
000550     12  FILLER              PIC X(14)   VALUE 'HALF DEATH    '.
000551     12  FILLER              PIC X(14)   VALUE 'FULL AD&D     '.
000552     12  FILLER              PIC X(14)   VALUE 'HALF AD&D     '.
000553     12  FILLER              PIC X(14)   VALUE 'FULL RIDER    '.
000554     12  FILLER              PIC X(14)   VALUE 'HALF RIDER    '.
000555     12  FILLER              PIC X(14)   VALUE 'NON-CHG EXP   '.
000556     12  FILLER              PIC X(14)   VALUE 'ADDITIONAL    '.
000557 01  CV-PAYMENT-DESC-R REDEFINES CV-PAYMENT-DESCRIPTION-TABLE.
000558     12  CV-PAY-DESC         PIC X(14)   OCCURS 2.
000559
000560 01  CORRESPONDENCE-TO-DESCRIPTIONS.
000561     12  FILLER              PIC X(8)    VALUE 'INSURED '.
000562     12  FILLER              PIC X(8)    VALUE 'BENEF   '.
000563     12  FILLER              PIC X(8)    VALUE 'ACCOUNT '.
000564     12  FILLER              PIC X(8)    VALUE 'DOCTOR  '.
000565     12  FILLER              PIC X(8)    VALUE 'EMPLOYER'.
000566     12  FILLER              PIC X(8)    VALUE 'OTHER1  '.
000567     12  FILLER              PIC X(8)    VALUE 'OTHER2  '.
000568 01  CORR-TO-DESC   REDEFINES CORRESPONDENCE-TO-DESCRIPTIONS.
000569     12  CORR-DESC           PIC X(8)    OCCURS 2.
000570
000571 01  W-CONFIDENTIAL-TEXT.
000572     12  W-CONF-LENGTH       PIC S9(4)   VALUE +185   COMP.
000573     12  W-CONF-TEXT.
000574         16  FILLER.
000575             20  W-CONF-PGM  PIC X(8)    VALUE 'EL150'.
000576             20  FILLER      PIC X       VALUE SPACES.
000577             20  FILLER      PIC X(71)   VALUE
000578     '******THIS IS A CONFIDENTIAL CLAIM!!!!!*****'.
000579         16  FILLER          PIC X(8)    VALUE SPACES.
000580         16  FILLER          PIC X(71)   VALUE
000581      '******PRESS ENTER TO RETURN TO SCREEN.******'.
000582         16  FILLER          PIC X(26)   VALUE SPACES.
000583*        16  FILLER          PIC X(7)    VALUE '* LOGIC'.
000584*        16  FILLER          PIC X       VALUE QUOTE.
000585*        16  W-CONF-SYS-MSG  PIC X(17)
000586*          VALUE 'S CLAS-IC SYSTEM '.
000587
000588     EJECT
000589*                            copy ERCPDEF.
      *>>((file: ERCPDEF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCPDEF.                            *
000005*                                                                *
000006*    FILE DESCRIPTION = PRODUCT DEFINITION MASTER                *
000007*                                                                *
000008*    FILE TYPE = VSAM,KSDS                                       *
000009*    RECORD SIZE = 1319 RECFORM = FIXED                          *
000010*                                                                *
000011*    BASE CLUSTER = ERPDEF                      RKP=02,LEN=18    *
000012*                                                                *
000013*    LOG = YES                                                   *
000014*    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000015******************************************************************
000016*                   C H A N G E   L O G
000017*
000018* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000019*-----------------------------------------------------------------
000020*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000021* EFFECTIVE    NUMBER
000022*-----------------------------------------------------------------
000023* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000024* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000025* 100314  CR2014061900001  PEMA  ADD PCT OF BENEFIT
000026* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000027******************************************************************
000028 01  PRODUCT-MASTER.
000029    12  PD-RECORD-ID                 PIC X(02).
000030        88  VALID-PD-ID                  VALUE 'PD'.
000031
000032    12  PD-CONTROL-PRIMARY.
000033        16  PD-COMPANY-CD            PIC X.
000034        16  PD-STATE                 PIC XX.
000035        16  PD-PRODUCT-CD            PIC XXX.
000036        16  PD-FILLER                PIC X(7).
000037        16  PD-BEN-TYPE              PIC X.
000038        16  PD-BEN-CODE              PIC XX.
000039        16  PD-PROD-EXP-DT           PIC XX.
000040
000041    12  FILLER                       PIC X(50).
000042
000043    12  PD-PRODUCT-DATA OCCURS 8.
000044        16  PD-PROD-CODE             PIC X.
000045            88  PD-PROD-LIFE           VALUE 'L'.
000046            88  PD-PROD-PROP           VALUE 'P'.
000047            88  PD-PROD-AH             VALUE 'A'.
000048            88  PD-PROD-IU             VALUE 'I'.
000049            88  PD-PROD-GAP            VALUE 'G'.
000050            88  PD-PROD-FAML           VALUE 'F'.
000051            88  PD-PROD-OTH            VALUE 'O'.
000052        16  PD-MAX-ATT-AGE           PIC S999        COMP-3.
000053        16  PD-MIN-ISSUE-AGE         PIC S999        COMP-3.
000054        16  PD-MAX-ISSUE-AGE         PIC S999        COMP-3.
000055        16  PD-MAX-TERM              PIC S999        COMP-3.
000056        16  PD-MAX-AMT               PIC S9(07)      COMP-3.
000057        16  FILLER                   PIC X.
000058        16  PD-PRE-EXIST-EXCL-TYPE   PIC 99.
000059        16  PD-EXCLUSION-PERIOD-DAYS PIC S999        COMP-3.
000060        16  PD-COVERAGE-ENDS-MOS     PIC S999        COMP-3.
000061        16  PD-ACCIDENT-ONLY-MOS     PIC S999        COMP-3.
000062        16  PD-CRIT-PERIOD           PIC S999        COMP-3.
000063        16  PD-REC-CRIT-PERIOD       PIC 99.
000064        16  PD-REC-CP-ALPHA  REDEFINES PD-REC-CRIT-PERIOD.
000065            20  PD-RECURRING-YN      PIC X.
000066            20  FILLER               PIC X.
000067        16  PD-RTW-MOS               PIC 99.
000068        16  PD-MAX-EXTENSION         PIC 99.
000069        16  pd-ben-pct               pic sv999 comp-3.
000070*       16  FILLER                   PIC XX.
000071
000072    12  PD-1ST-YR-ADMIN-ALLOW        PIC S9(3)V99    COMP-3.
000073
000074    12  PD-TERM-LIMITS OCCURS 15.
000075        16  PD-LOW-TERM              PIC S999        COMP-3.
000076        16  PD-HI-TERM               PIC S999        COMP-3.
000077
000078*  THE LOAN AMT LIMITS CORRESPOND TO THE TERM LIMITS ABOVE
000079    12  PD-LOAN-AMT-LIMITS OCCURS 15.
000080        16  PD-LOW-AMT               PIC S9(5)       COMP-3.
000081        16  PD-HI-AMT                PIC S9(7)       COMP-3.
000082
000083    12  PD-EARN-FACTORS.
000084        16  FILLER OCCURS 15.
000085            20  FILLER OCCURS 15.
000086                24  PD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
000087
000088    12  PD-PRODUCT-DESC              PIC X(80).
000089    12  PD-TRUNCATED                 PIC X.
000090    12  FILLER                       PIC X(59).
000091
000092    12  PD-MAINT-INFORMATION.
000093        16  PD-LAST-MAINT-DT         PIC X(02).
000094        16  PD-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
000095        16  PD-LAST-MAINT-BY         PIC X(04).
      *<<((file: ERCPDEF))
000590*                            COPY ELCNWA.
      *>>((file: ELCNWA))
000001*****************************************************************
000002*                                                               *
000003*                                                               *
000004*                            ELCNWA.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                         *
000007*                                                               *
000008*            M O V E   N A M E   W O R K   A R E A.             *
000009*                                                               *
000010*****************************************************************.
000011
000012 01  WS-NAME-WORK-AREA.
000013     05  WS-INSURED-LAST-NAME        PIC X(15).
000014     05  WS-INSURED-1ST-NAME         PIC X(12).
000015     05  WS-INSURED-MID-INIT         PIC X.
000016
000017     05  WS-NAME-WORK.
000018         10  WS-NW                   PIC X
000019             OCCURS 30 TIMES INDEXED BY NWA-INDEX.
000020
000021     05  WS-NAME-WORK2.
000022         10  WS-NW2                  PIC X
000023             OCCURS 20 TIMES INDEXED BY NWA-INDEX2 NWA-INDEX3
000024                                        NWA-INDEX0.
000025
000026     05  WS-NAME-SW                  PIC S9          VALUE ZERO
000027                                     COMP-3.
000028
      *<<((file: ELCNWA))
000591     EJECT
000592*                            COPY ELCDATE.
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
000593     EJECT
000594*                            COPY ELCCALC.
      *>>((file: ELCCALC))
000001******************************************************************
000002*                                                                *
000003*                           ELCCALC.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.025                          *
000006*                                                                *
000007*   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
000008*                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
000009*                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
000010*                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
000011*                                                                *
000012*  PASSED TO ELRTRM                                              *
000013*  -----------------                                             *
000014*  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
000015*  ORIGINAL TERM                                                 *
000016*  BEGINNING DATE                                                *
000017*  ENDING DATE                                                   *
000018*  COMPANY I.D.                                                  *
000019*  ACCOUNT MASTER USER FIELD                                     *
000020*  PROCESS SWITCH (CANCEL, CLAIM)                                *
000021*  FREE LOOK DAYS                                                *
000022*                                                                *
000023*  RETURNED FROM ELRTRM                                          *
000024*  ---------------------                                         *
000025*  REMAINING TERM 1 - USED FOR EARNINGS                          *
000026*  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
000027*  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
000028*  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
000029*----------------------------------------------------------------*
000030*  PASSED TO ELRAMT                                              *
000031*  ----------------                                              *
000032*  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
000033*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000034*  ORIGINAL AMOUNT                                               *
000035*  ALTERNATE BENEFIT (BALLON)                                    *
000036*  A.P.R. - NET PAY ONLY                                         *
000037*  METHOD
000038*  PAYMENT FREQUENCY - FOR FARM PLAN                             *
000039*  COMPANY I.D.                                                  *
000040*  BENEFIT TYPE                                                  *
000041*                                                                *
000042*  RETURNED FROM ELRAMT                                          *
000043*  --------------------                                          *
000044*  REMAINING AMOUNT 1 - CURRENT                                  *
000045*  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
000046*  REMAINING AMOUNT FACTOR
000047*----------------------------------------------------------------*
000048*  PASSED TO ELRESV                                              *
000049*  -----------------                                             *
000050*  CERTIFICATE EFFECTIVE DATE                                    *
000051*  VALUATION DATE                                                *
000052*  PAID THRU DATE                                                *
000053*  BENEFIT                                                       *
000054*  INCURRED DATE                                                 *
000055*  REPORTED DATE                                                 *
000056*  ISSUE AGE                                                     *
000057*  TERM                                                          *
000058*  CDT PERCENT                                                   *
000059*  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
000060* *CLAIM TYPE (LIFE, A/H)                                        *
000061* *REMAINING BENEFIT (FROM ELRAMT)                               *
000062* *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
000063*                                                                *
000064*  RETURNED FROM ELRESV                                          *
000065*  --------------------                                          *
000066*  CDT TABLE USED                                                *
000067*  CDT FACTOR USED                                               *
000068*  PAY TO CURRENT RESERVE                                        *
000069*  I.B.N.R. - A/H ONLY                                           *
000070*  FUTURE (ACCRUED) AH ONLY                                      *
000071*----------------------------------------------------------------*
000072*  PASSED TO ELRATE                                              *
000073*  ----------------                                              *
000074*  CERT ISSUE DATE                                               *
000075*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000076*  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
000077*  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
000078*  STATE CODE (CLIENT DEFINED)                                   *
000079*  STATE CODE (STANDARD P.O. ABBRV)                              *
000080*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000081*  DEVIATION CODE                                                *
000082*  ISSUE AGE                                                     *
000083*  ORIGINAL BENEFIT AMOUNT                                       *
000084*  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
000085*  PROCESS TYPE (ISSUE OR CANCEL)                                *
000086*  BENEFIT KIND (LIFE OR A/H)                                    *
000087*  A.P.R.                                                        *
000088*  METHOD
000089*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000090*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000091*  COMPANY I.D. (3 CHARACTER)                                    *
000092*  BENEFIT CODE                                                  *
000093*  BENEFIT OVERRIDE CODE                                         *
000094*  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
000095*  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
000096*  JOINT INDICATOR (CSL ONLY)                                    *
000097*  FIRST PAYMENT DATE (CSL ONLY)                                 *
000098*  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
000099*                                                                *
000100*  RETURNED FROM ELRATE                                          *
000101*  --------------------                                          *
000102*  CALCULATED PREMIUM                                            *
000103*  PREMIUM RATE                                                  *
000104*  MORTALITY CODE                                                *
000105*  MAX ATTAINED AGE                                              *
000106*  MAX AGE                                                       *
000107*  MAX TERM                                                      *
000108*  MAX MONTHLY BENEFIT                                           *
000109*  MAX TOTAL BENIFIT                                             *
000110*  COMPOSITE RATE (OPEN-END ONLY)                                *
000111*----------------------------------------------------------------*
000112*  PASSED TO ELRFND                                              *
000113*  ----------------                                              *
000114*  CERT ISSUE DATE                                               *
000115*  REFUND DATE                                                   *
000116*  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
000117*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000118*  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
000119*  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
000120*  STATE CODE (CLIENT DEFINED)                                   *
000121*  STATE CODE (STANDARD P.O. ABBRV)                              *
000122*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000123*  DEVIATION CODE                                                *
000124*  ISSUE AGE                                                     *
000125*  ORIGINAL BENEFIT AMOUNT                                       *
000126*  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
000127*  PROCESS TYPE (CANCEL)                                         *
000128*  BENEFIT KIND (LIFE OR A/H)                                    *
000129*  A.P.R.                                                        *
000130*  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
000131*  RATING METHOD -  (CODE FROM BENEFIT)                          *
000132*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000133*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000134*  COMPANY I.D. (3 CHARACTER)                                    *
000135*  BENEFIT CODE                                                  *
000136*  BENEFIT OVERRIDE CODE                                         *
000137*                                                                *
000138*  RETURNED FROM ELRFND                                          *
000139*  --------------------                                          *
000140*  CALCULATED REFUND                                             *
000141*----------------------------------------------------------------*
000142*  PASSED TO ELEARN                                              *
000143*  ----------------                                              *
000144*  CERT ISSUE DATE                                               *
000145*  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
000146*  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
000147*  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
000148*  STATE CODE (CLIENT DEFINED)                                   *
000149*  STATE CODE (STANDARD P.O. ABBRV)                              *
000150*  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
000151*  DEVIATION CODE                                                *
000152*  ISSUE AGE                                                     *
000153*  ORIGINAL BENEFIT AMOUNT                                       *
000154*  BENEFIT KIND (LIFE OR A/H)                                    *
000155*  A.P.R.                                                        *
000156*  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
000157*  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
000158*  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
000159*  COMPANY I.D. (3 CHARACTER)                                    *
000160*  BENEFIT CODE                                                  *
000161*  BENEFIT OVERRIDE CODE                                         *
000162*                                                                *
000163*  RETURNED FROM ELEARN                                          *
000164*  --------------------                                          *
000165*  INDICATED  EARNINGS                                           *
000166*----------------------------------------------------------------*
000167*                 LENGTH = 450                                   *
000168*                                                                *
000169******************************************************************
000170******************************************************************
000171*                   C H A N G E   L O G
000172*
000173* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000174*-----------------------------------------------------------------
000175*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000176* EFFECTIVE    NUMBER
000177*-----------------------------------------------------------------
000178* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
000179* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
000180* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
000181* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
000182* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
000183* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
000184* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
000185* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
000186* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000187* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS
000188* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
000189* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
000190******************************************************************
000191
000192 01  CALCULATION-PASS-AREA.
000193     12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
000194                                     COMP.
000195
000196     12  CP-RETURN-CODE            PIC X             VALUE ZERO.
000197       88  NO-CP-ERROR                             VALUE ZERO.
000198       88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
000199                                  '9' 'A' 'B' 'C' 'D' 'E' 'H' 'I'.
000200       88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
000201       88  CP-ERROR-IN-DATES                       VALUE '2'.
000202       88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
000203       88  CP-ERROR-IN-TERMS                       VALUE '4'.
000204       88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
000205       88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
000206       88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
000207       88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
000208       88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
000209       88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
000210       88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
000211       88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
000212       88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
000213       88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
000214       88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
000215       88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
000216       88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
000217       88  CP-ERROR-TERM-BELOW-MINIMUM             VALUE 'I'.
000218
000219     12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
000220       88  NO-CP-ERROR-2                           VALUE ZERO.
000221***********************  INPUT AREAS ****************************
000222
000223     12  CP-CALCULATION-AREA.
000224         16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
000225         16  CP-CERT-EFF-DT        PIC XX.
000226         16  CP-VALUATION-DT       PIC XX.
000227         16  CP-PAID-THRU-DT       PIC XX.
000228         16  CP-BENEFIT-TYPE       PIC X.
000229           88  CP-AH                               VALUE 'A' 'D'
000230                                                   'I' 'U'.
000231           88  CP-REDUCING-LIFE                    VALUE 'R'.
000232           88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
000233         16  CP-INCURRED-DT        PIC XX.
000234         16  CP-REPORTED-DT        PIC XX.
000235         16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
000236         16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
000237         16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
000238                                     COMP-3.
000239         16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
000240                                     COMP-3.
000241         16  CP-CDT-METHOD         PIC X.
000242           88  CP-CDT-ROUND-NEAR                   VALUE '1'.
000243           88  CP-CDT-ROUND-HIGH                   VALUE '2'.
000244           88  CP-CDT-INTERPOLATED                 VALUE '3'.
000245         16  CP-CLAIM-TYPE         PIC X.
000246           88  CP-AH-CLAIM                         VALUE 'A'.
000247           88  CP-LIFE-CLAIM                       VALUE 'L'.
000248         16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
000249                                     COMP-3.
000250         16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
000251                                     COMP-3.
000252         16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
000253                                     COMP-3.
000254         16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
000255                                     COMP-3.
000256         16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
000257                                     COMP-3.
000258         16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
000259                                     COMP-3.
000260         16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
000261                                     COMP-3.
000262         16  CP-REM-TERM-METHOD    PIC X.
000263           88  CP-EARN-AFTER-15TH                  VALUE '1'.
000264           88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
000265           88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
000266           88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
000267           88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
000268           88  CP-EARN-AFTER-14TH                  VALUE '6'.
000269           88  CP-EARN-AFTER-16TH                  VALUE '7'.
000270         16  CP-EARNING-METHOD     PIC X.
000271           88  CP-EARN-BY-R78                      VALUE '1' 'R'.
000272           88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
000273           88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
000274           88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
000275           88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
000276           88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
000277           88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
000278           88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
000279           88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
000280           88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
000281           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
000282           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
000283           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
000284           88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
000285         16  CP-PROCESS-TYPE       PIC X.
000286           88  CP-CLAIM                            VALUE '1'.
000287           88  CP-CANCEL                           VALUE '2'.
000288           88  CP-ISSUE                            VALUE '3'.
000289         16  CP-SPECIAL-CALC-CD    PIC X.
000290           88  CP-OUTSTANDING-BAL              VALUE 'O'.
000291           88  CP-1-MTH-INTEREST               VALUE ' '.
000292           88  CP-0-MTH-INTEREST               VALUE 'A'.
000293           88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
000294           88  CP-CRITICAL-PERIOD              VALUE 'C'.
000295           88  CP-TERM-IS-DAYS                 VALUE 'D'.
000296           88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
000297           88  CP-FARM-PLAN                    VALUE 'F'.
000298           88  CP-RATE-AS-STANDARD             VALUE 'G'.
000299           88  CP-2-MTH-INTEREST               VALUE 'I'.
000300           88  CP-3-MTH-INTEREST               VALUE 'J'.
000301           88  CP-4-MTH-INTEREST               VALUE 'K'.
000302           88  CP-BALLOON-LAST-PMT             VALUE 'L'.
000303           88  CP-MORTGAGE-REC                 VALUE 'M'.
000304           88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
000305           88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
000306           88  CP-NET-PAY-SIMPLE               VALUE 'S'.
000307           88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
000308                                                     'W' 'X'.
000309           88  CP-TRUNCATE-0-MTH               VALUE 'T'.
000310           88  CP-TRUNCATE-1-MTH               VALUE 'U'.
000311           88  CP-TRUNCATE-2-MTH               VALUE 'V'.
000312           88  CP-TRUNCATE-3-MTH               VALUE 'W'.
000313           88  CP-TRUNCATE-4-MTH               VALUE 'X'.
000314           88  CP-SUMMARY-REC                  VALUE 'Z'.
000315           88  CP-PROPERTY-BENEFIT             VALUE '2'.
000316           88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
000317           88  CP-AD-D-BENEFIT                 VALUE '4'.
000318           88  CP-CSL-METH-1                   VALUE '5'.
000319           88  CP-CSL-METH-2                   VALUE '6'.
000320           88  CP-CSL-METH-3                   VALUE '7'.
000321           88  CP-CSL-METH-4                   VALUE '8'.
000322
000323         16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
000324                                     COMP-3.
000325         16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
000326         16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
000327         16  CP-STATE              PIC XX          VALUE SPACE.
000328         16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
000329         16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
000330           88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
000331               '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
000332         16  CP-R78-OPTION         PIC X.
000333           88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
000334           88  CP-TERM-TIMES-TERM                  VALUE '1'.
000335
000336         16  CP-COMPANY-CD         PIC X             VALUE SPACE.
000337         16  CP-IBNR-RESERVE-SW    PIC X.
000338         16  CP-CLAIM-STATUS       PIC X.
000339         16  CP-RATE-FILE          PIC X.
000340         16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
000341                                     COMP-3.
000342
000343         16  CP-LIFE-OVERRIDE-CODE PIC X.
000344         16  CP-AH-OVERRIDE-CODE   PIC X.
000345
000346         16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
000347                                     COMP-3.
000348         16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
000349                                   PIC S9(5)V99 COMP-3.
000350         16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
000351                                     COMP-3.
000352         16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
000353                                     COMP-3.
000354         16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
000355                                     COMP-3.
000356         16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
000357                                  PIC S9(7)V99 COMP-3.
000358
000359         16  CP-PAID-FROM-DATE     PIC X(02).
000360         16  CP-CLAIM-CALC-METHOD  PIC X(01).
000361         16  CP-EXT-DAYS-CALC      PIC X.
000362           88  CP-EXT-NO-CHG                   VALUE ' '.
000363           88  CP-EXT-CHG-LF                   VALUE '1'.
000364           88  CP-EXT-CHG-AH                   VALUE '2'.
000365           88  CP-EXT-CHG-LF-AH                VALUE '3'.
000366         16  CP-DOMICILE-STATE     PIC XX.
000367         16  CP-CARRIER            PIC X.
000368         16  CP-REIN-FLAG          PIC X.
000369         16  CP-REM-TRM-CALC-OPTION PIC X.
000370           88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
000371                      '2' '3' '4' '5'.
000372           88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
000373           88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
000374           88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
000375           88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
000376           88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
000377           88  CP-EXT-30-DAY-MONTH          VALUE '3'.
000378           88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
000379           88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
000380         16  CP-SIG-SWITCH         PIC X.
000381         16  CP-RATING-METHOD      PIC X.
000382           88  CP-RATE-AS-R78                      VALUE '1' 'R'.
000383           88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
000384           88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
000385           88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
000386           88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
000387           88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
000388           88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
000389           88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
000390           88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
000391         16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
000392                                     COMP-3.
000393         16  CP-BEN-CATEGORY       PIC X.
000394         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
000395         16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
000396                                   PIC S99V9(5) COMP-3.
000397         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
000398         16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
000399                                   PIC S99V9(5) COMP-3.
000400         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
000401         16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
000402         16  CP-EXPIRE-DT          PIC XX.
000403         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
000404         16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
000405         16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
000406         16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
000407         16  CP-DDF-SPEC-CALC      PIC X.
000408             88  CP-CALC-GROSS-FEE        VALUE 'G'.
000409             88  CP-CALC-CLP              VALUE 'C'.
000410         16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
000411         16  CP-CANCEL-REASON      PIC X.
000412         16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
000413         16  CP-PMT-MODE           PIC X.
000414         16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
000415         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
000416         16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
000417         16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
000418         16  FILLER                PIC X.
000419
000420***************    OUTPUT FROM ELRESV   ************************
000421
000422         16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
000423
000424         16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
000425                                     COMP-3.
000426         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
000427                                     COMP-3.
000428         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
000429                                     COMP-3.
000430         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
000431                                     COMP-3.
000432         16  FILLER                PIC X(09).
000433***************    OUTPUT FROM ELRTRM   *************************
000434
000435         16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
000436                                     COMP-3.
000437         16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
000438                                     COMP-3.
000439         16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
000440                                     COMP-3.
000441         16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
000442                                     COMP-3.
000443         16  FILLER                PIC X(12).
000444
000445***************    OUTPUT FROM ELRAMT   *************************
000446
000447         16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
000448                                     COMP-3.
000449         16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
000450                                     COMP-3.
000451         16  FILLER                PIC X(12).
000452
000453***************    OUTPUT FROM ELRATE   *************************
000454
000455         16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
000456                                     COMP-3.
000457         16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
000458                                     COMP-3.
000459         16  CP-MORTALITY-CODE     PIC X(4).
000460         16  CP-RATE-EDIT-FLAG     PIC X.
000461             88  CP-RATES-NEED-APR                  VALUE '1'.
000462         16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
000463                                     COMP-3.
000464         16  CP-CANCEL-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
000465         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
000466         16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
000467                                   PIC S9(7)V99 COMP-3.
000468         16  FILLER                PIC X(07).
000469
000470***************    OUTPUT FROM ELRFND   *************************
000471
000472         16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
000473                                     COMP-3.
000474         16  CP-REFUND-TYPE-USED   PIC X.
000475           88  CP-R-AS-R78                         VALUE '1'.
000476           88  CP-R-AS-PRORATA                     VALUE '2'.
000477           88  CP-R-AS-CALIF                       VALUE '3'.
000478           88  CP-R-AS-TEXAS                       VALUE '4'.
000479           88  CP-R-AS-FARM-PLAN                   VALUE '4'.
000480           88  CP-R-AS-NET-PAY                     VALUE '5'.
000481           88  CP-R-AS-ANTICIPATION                VALUE '6'.
000482           88  CP-R-AS-MEAN                        VALUE '8'.
000483           88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
000484           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
000485           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
000486           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
000487           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
000488           88  CP-R-AS-REPOSSESSION                VALUE 'R'.
000489         16  FILLER                PIC X(12).
000490
000491***************    OUTPUT FROM ELEARN   *************************
000492
000493         16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
000494                                     COMP-3.
000495         16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
000496                                     COMP-3.
000497         16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
000498                                     COMP-3.
000499         16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
000500                                     COMP-3.
000501         16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
000502                                     COMP-3.
000503         16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
000504                                     COMP-3.
000505         16  CP-EARNING-TYPE-USED  PIC X.
000506           88  CP-E-AS-SPECIAL                     VALUE 'S'.
000507           88  CP-E-AS-R78                         VALUE '1'.
000508           88  CP-E-AS-PRORATA                     VALUE '2'.
000509           88  CP-E-AS-TEXAS                       VALUE '4'.
000510           88  CP-E-AS-FARM-PLAN                   VALUE '4'.
000511           88  CP-E-AS-NET-PAY                     VALUE '5'.
000512           88  CP-E-AS-ANTICIPATION                VALUE '6'.
000513           88  CP-E-AS-MEAN                        VALUE '8'.
000514           88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
000515         16  FILLER                PIC X(12).
000516
000517***************    OUTPUT FROM ELPMNT   *************************
000518
000519         16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
000520                                     COMP-3.
000521         16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
000522                                     COMP-3.
000523         16  FILLER                PIC X(12).
000524
000525***************   MISC WORK AREAS    *****************************
000526         16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
000527                                     COMP-3.
000528         16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
000529                                     COMP-3.
000530         16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
000531                                     COMP-3.
000532         16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
000533                                     COMP-3.
000534         16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
000535                                     COMP-3.
000536         16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
000537                                     COMP-3.
000538         16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
000539             88  OPEN-RATE-FILE                   VALUE 'O'.
000540             88  CLOSE-RATE-FILE                  VALUE 'C'.
000541             88  IO-ERROR                         VALUE 'E'.
000542
000543         16  CP-FIRST-PAY-DATE     PIC XX.
000544
000545         16  CP-JOINT-INDICATOR    PIC X.
000546
000547         16  CP-RESERVE-REMAINING-TERM
000548                                   PIC S9(4)V9    VALUE ZERO
000549                                     COMP-3.
000550
000551         16  CP-INSURED-BIRTH-DT   PIC XX.
000552
000553         16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
000554                                     COMP-3.
000555
000556         16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
000557                                     COMP-3.
000558
000559         16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
000560                                     COMP-3.
000561
000562         16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
000563                                     COMP-3.
000564
000565         16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
000566                                     COMP-3.
000567
000568         16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
000569                                     COMP-3.
000570
000571         16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
000572                                     COMP-3.
000573
000574         16  CP-ROA-REFUND         PIC X          VALUE 'N'.
000575             88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
000576
000577         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
000578                                     COMP-3.
000579         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
000580                                     COMP-3.
000581         16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
000582         16  cp-extra-periods      pic 9 value zeros.
000583         16  cp-net-only-state     pic x value spaces.
000584         16  FILLER                PIC X(13).
000585******************************************************************
      *<<((file: ELCCALC))
000595     EJECT
000596*                            COPY ELCLOGOF.
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
000597     EJECT
000598*                            COPY ELCATTR.
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
000599     EJECT
000600*                            COPY ELCMSTR.
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
000601     EJECT
000602*                            COPY ELCEMIB.
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
000603     EJECT
000604*                            COPY ELCLNKLT.
      *>>((file: ELCLNKLT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                           ELCLNKLT                             *
000005*                            VMOD=2.003                          *
000006*                                                                *
000007*   THIS COPY BOOK IS USED TO PASS DATA TO THE LETTER GENERATOR  *
000008*   PROGRAM (EL1523). THE AREA SHOULD BE INITIALIZED TO LOW-     *
000009*   VALUES BEFORE ENTERING YOUR INFORMATION.                     *
000010*                                                                *
000011*   MOVE THE PROGRAM-INTERFACE-BLOCK TO W-1523-LINK-DATA BEFORE  *
000012*   COMPLETING THE REQUIRED FIELDS.                              *
000013*                                                                *
000014*   THE CALLING PROGRAM SHOULD CHECK:                            *
000015*   1. W-1523-ERROR-CODE WHERE 0000 INDICATES NO ERROR DETECTED. *
000016*                              9999 INDICATES AN UNKNOWN ABEND   *
000017*                                   WAS DETECTED.                *
000018*                              ALL OTHER VALUES ARE NORMAL ERROR *
000019*                              CODES FOUND IN THE ERROR FILE.    *
000020******************************************************************
000021 01  W-1523-LINKDATA.
000022     12  W-1523-COMMON-PI-DATA.
000023         16  W-1523-COMM-LENGTH  PIC S9(04) COMP   VALUE +1024.
000024         16  FILLER              PIC  X(67).
000025         16  W-1523-COMPANY-CD   PIC  X(01).
000026         16  FILLER              PIC  X(10).
000027         16  W-1523-CONTROL-IN-PROGRESS.
000028             20  W-1523-CARRIER  PIC  X(01).
000029             20  W-1523-GROUPING PIC  X(06).
000030             20  W-1523-STATE    PIC  X(02).
000031             20  W-1523-ACCOUNT  PIC  X(10).
000032             20  W-1523-CLAIM-CERT-GRP.
000033                 24  W-1523-CLAIM-NO
000034                                 PIC  X(07).
000035                 24  W-1523-CERT-NO.
000036                     28  W-1523-CERT-PRIME
000037                                 PIC  X(10).
000038                     28  W-1523-CERT-SFX
000039                                 PIC  X(01).
000040                 24  W-1523-CERT-EFF-DT
000041                                 PIC  X(02).
000042         16  FILLER              PIC X(265).
000043
000044     12  W-1523-WORK-AREA.
000045         16  W-1523-FORM-NUMBER  PIC  X(04).
000046         16  W-1523-NUMBER-COPIES
000047                                 PIC  9(01).
000048         16  W-1523-ADDR-TYPE    PIC  X(02).
000049         16  W-1523-FOLLOW-UP-DATE
000050                                 PIC  X(02).
000051         16  W-1523-RESEND-DATE  PIC  X(02).
000052         16  W-1523-ERROR-CODE   PIC  9(04).
000053             88  W-1523-NO-ERRORS-DETECTED VALUE 0000.
000054             88  W-1523-FATAL-ERROR
000055                                    VALUES  0006 0013 0042
000056                                            0154 0168 0169
000057                                            0176 9999 0172
000058                                            0179 0186 0281
000059                                            0332 2055 3697
000060                                            3698 3699 3770
000061                                            3771 3772 7675
000062                                            9106 9808 9883
000063                                            9887.
000064             88  W-1523-STOP-ERRORS
000065                                    VALUES  0013 0042 0154
000066                                            0168 0169 0172
000067                                            0281 0332 2055
000068                                            3698 3699 7675
000069                                            9106 9808 9883
000070                                            9999.
000071         16  W-1523-REASON       PIC  X(70).
000072         16  W-1523-ARCHIVE-NUMBER
000073                                 PIC  9(08).
000074     12  W-1523-POINTER-AREA.
000075         16  W-1523-ACCT-POINTER PIC S9(08) COMP.
000076         16  W-1523-ACTY-POINTER PIC S9(08) COMP.
000077         16  W-1523-ARCH-POINTER PIC S9(08) COMP.
000078         16  W-1523-VAR-POINTER  PIC S9(08) COMP.
000079         16  W-1523-PROD-POINTER PIC S9(08) COMP.
      *<<((file: ELCLNKLT))
000605     EJECT
000606*                            COPY ELCINTF.
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
000607     12  PI-REDEF    REDEFINES PI-PROGRAM-WORK-AREA.
000608         16  PI-PASS-AREA        PIC  S9(4) COMP.
000609         16  PI-EL142-PRIORITY   PIC  X.
000610         16  pi-save-map         pic x(8).
000611         16  pi-max-benefit      pic s9(7)v99 comp-3.
000612         16  PI-APPROVAL-LEVEL   PIC X.
000613         16  f                   pic x(197).
000614*        16  FILLER              PIC  X(211).
000615         16  PI-CURRENT-FILE     PIC  X(08).
000616         16  PI-ORIGINAL-ASS-CERT
000617                                 PIC  X(20).
000618         16  PI-TOTAL-PAID       PIC S9(07)V99 COMP-3.
000619         16  PI-DAYS-PAID        PIC S9(05) COMP-3.
000620         16  PI-LAST-SEQ-NO      PIC S9(03) COMP-3.
000621         16  PI-LAST-PF-KEY-IND  PIC  X.
000622             88  PI-PF15-LAST            VALUE 'F'.
000623             88  PI-PF16-LAST            VALUE 'S'.
000624         16  PI-ASSOCIATED-PROCESS-IND
000625                                 PIC  X.
000626             88  PI-ASS-PROCESSING       VALUE 'Y'.
000627             88  PI-NOT-ASS-PROCESSING   VALUE SPACES LOW-VALUES.
000628         16  PI-ASS-NDX          PIC S9(04) COMP.
000629         16  PI-ASSOCIATED-CERTS-TABLE.
000630             20  PI-ASS-CERTS OCCURS 24 TIMES
000631                                 PIC  X(11).
000632*00545          16  FILLER              PIC  X(10).
000633         16  FILLER              PIC  X(05).
000634         16  PI-FORCE-COUNT      PIC  99.
000635         16  PI-PFKEY-USED       PIC  X.
000636         16  PI-JOINT-COV-IND    PIC  X.
000637             88  PI-JOINT-COVERAGE       VALUE 'J'.
000638             88  PI-SINGLE-COVERAGE      VALUE ' '.
000639         16  PI-JOINT-INSURED-IND PIC X.
000640             88  PI-JOINT-INSURED        VALUE 'Y'.
000641             88  PI-PRIMARY-INSURED      VALUE 'N'.
000642         16  PI-PRIORITY-CD      PIC  X.
000643             88  PI-PRIORITY-9           VALUE '9'.
000644         16  PI-STATUS-IND       PIC  X.
000645             88  PI-CLOSED               VALUE 'C'.
000646             88  PI-OPEN                 VALUE 'O'.
000647         16  PI-LAST-CLOSE-REASON-IND
000648                                 PIC  X.
000649             88  PI-BENEFIT-CHANGE       VALUE 'C'.
000650             88  PI-ERROR-CORRECTION     VALUE 'E'.
000651         16  PI-PRIORITY-IND     PIC  X.
000652             88  PI-SUPERVISOR-ONLY      VALUE 'S'.
000653             88  PI-CONFIDENTIAL-UP      VALUE 'C'.
000654         16  PI-RETRIEVED-DATA-IND
000655                                 PIC  X.
000656             88  PI-RETRIEVED-DATA       VALUE 'Y'.
000657*        16  PI-SAVE             PIC  X(227).
000658         16  PI-SAVE-TYPE        PIC  X.
000659         16  PI-SAVE-FILETO      PIC  X(4).
000660         16  PI-SAVE-PRTOPT      PIC  X.
000661         16  PI-SAVE-FORMAT      PIC  X.
000662         16  PI-SAVE-FORM        PIC  X(12).
000663         16  PI-PREV-DISP        PIC  X.
000664         16  PI-PREV-DIR         PIC  X.
000665         16  PI-SAVE-CURSOR      PIC S9(04) COMP.
000666         16  PI-SAVE-CNT         PIC S9(04) COMP.
000667         16  PI-SAVE-LOW         PIC S9(04) COMP.
000668         16  PI-SAVE-HIGH        PIC S9(04) COMP.
000669         16  PI-PREV-CLAIM.
000670             20  PI-PREV-COMP-CD PIC X.
000671             20  PI-PREV-CARRIER PIC X.
000672             20  PI-PREV-CLAIM-NO PIC X(7).
000673             20  PI-PREV-CERT-NO.
000674                 24  PI-PREV-CERT-NO-PRIME
000675                                 PIC X(10).
000676                 24  PI-CERT-NO-SUFX   PIC X.
000677         16  PI-PREV-PRIME-CERT-NO
000678                                 PIC X(11).
000679         16  PI-PURGED-SW        PIC  X.
000680             88  CLAIM-IS-PURGED         VALUE 'Y'.
000681         16  PI-PREV-CLMNO       PIC  X(07).
000682         16  PI-SAVE-MCRFLM      PIC  X(10).
000683         16  PI-PREV-TRLR-KEY    PIC  X(22).
000684         16  PI-SAVE-ACT-CD      PIC  XX.
000685         16  PI-SAVE-ASSOC       PIC  X.
000686         16  PI-SAVE-SYS-ID      PIC  XX.
000687*        16  PI-SAVE-CLOAN       PIC  X(25).
000688     EJECT
000689*                            COPY ELCJPFX.
      *>>((file: ELCJPFX))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCJPFX.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                          *
000007*                                                                *
000008*    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
000009*                                                                *
000010*     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
000011*     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
000012*        ELCNTL - CONTROL FILE                                   *
000013*        ELMSTR - CLAIM MASTERS                                  *
000014*        ELTRLR - ACTIVITY TRAILERS                              *
000015*        ELCHKQ - CHECK QUE                                      *
000016******************************************************************
000017 01  JOURNAL-RECORD.
000018     12  jp-date                     pic s9(5) comp-3.
000019     12  jp-time                     pic s9(7) comp-3.
000020     12  JP-USER-ID                  PIC X(4).
000021     12  JP-FILE-ID                  PIC X(8).
000022     12  JP-PROGRAM-ID               PIC X(8).
000023     12  JP-RECORD-TYPE              PIC X.
000024         88 JP-ADD              VALUE 'A'.
000025         88 JP-BEFORE-CHANGE    VALUE 'B'.
000026         88 JP-AFTER-CHANGE     VALUE 'C'.
000027         88 JP-DELETE           VALUE 'D'.
000028         88 JP-GENERIC-DELETE   VALUE 'G'.
000029         88 JP-KEY-CHG-DELETE   VALUE 'K'.
000030         88 JP-KEY-CHG-ADD      VALUE 'N'.
000031     12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
000032     12  JP-RECORD-AREA
000033
000034
      *<<((file: ELCJPFX))
000690                             PIC X(750).
000691     EJECT
000692*                            COPY ELCAID.
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
000693 01  FILLER    REDEFINES DFHAID.
000694     12  FILLER              PIC X(8).
000695     12  PF-VALUES           PIC X       OCCURS 24 TIMES.
000696     EJECT
000697*                            COPY EL150S.
      *>>((file: EL150S))
000001 01  EL150AI.
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
000016     05  SEQUL PIC S9(0004) COMP.
000017     05  SEQUF PIC  X(0001).
000018     05  FILLER REDEFINES SEQUF.
000019         10  SEQUA PIC  X(0001).
000020     05  SEQUI PIC  X(0010).
000021*    -------------------------------
000022     05  CMPNYIDL PIC S9(0004) COMP.
000023     05  CMPNYIDF PIC  X(0001).
000024     05  FILLER REDEFINES CMPNYIDF.
000025         10  CMPNYIDA PIC  X(0001).
000026     05  CMPNYIDI PIC  X(0003).
000027*    -------------------------------
000028     05  USERIDL PIC S9(0004) COMP.
000029     05  USERIDF PIC  X(0001).
000030     05  FILLER REDEFINES USERIDF.
000031         10  USERIDA PIC  X(0001).
000032     05  USERIDI PIC  X(0004).
000033*    -------------------------------
000034     05  RETMSTL PIC S9(0004) COMP.
000035     05  RETMSTF PIC  X(0001).
000036     05  FILLER REDEFINES RETMSTF.
000037         10  RETMSTA PIC  X(0001).
000038     05  RETMSTI PIC  X(0015).
000039*    -------------------------------
000040     05  CRDCARDL PIC S9(0004) COMP.
000041     05  CRDCARDF PIC  X(0001).
000042     05  FILLER REDEFINES CRDCARDF.
000043         10  CRDCARDA PIC  X(0001).
000044     05  CRDCARDI PIC  X(0016).
000045*    -------------------------------
000046     05  CLMNOL PIC S9(0004) COMP.
000047     05  CLMNOF PIC  X(0001).
000048     05  FILLER REDEFINES CLMNOF.
000049         10  CLMNOA PIC  X(0001).
000050     05  CLMNOI PIC  X(0007).
000051*    -------------------------------
000052     05  CARRL PIC S9(0004) COMP.
000053     05  CARRF PIC  X(0001).
000054     05  FILLER REDEFINES CARRF.
000055         10  CARRA PIC  X(0001).
000056     05  CARRI PIC  X(0001).
000057*    -------------------------------
000058     05  CERTNOL PIC S9(0004) COMP.
000059     05  CERTNOF PIC  X(0001).
000060     05  FILLER REDEFINES CERTNOF.
000061         10  CERTNOA PIC  X(0001).
000062     05  CERTNOI PIC  X(0010).
000063*    -------------------------------
000064     05  SUFXL PIC S9(0004) COMP.
000065     05  SUFXF PIC  X(0001).
000066     05  FILLER REDEFINES SUFXF.
000067         10  SUFXA PIC  X(0001).
000068     05  SUFXI PIC  X(0001).
000069*    -------------------------------
000070     05  CLMTYPL PIC S9(0004) COMP.
000071     05  CLMTYPF PIC  X(0001).
000072     05  FILLER REDEFINES CLMTYPF.
000073         10  CLMTYPA PIC  X(0001).
000074     05  CLMTYPI PIC  X(0006).
000075*    -------------------------------
000076     05  CLMSTATL PIC S9(0004) COMP.
000077     05  CLMSTATF PIC  X(0001).
000078     05  FILLER REDEFINES CLMSTATF.
000079         10  CLMSTATA PIC  X(0001).
000080     05  CLMSTATI PIC  X(0006).
000081*    -------------------------------
000082     05  LSTNMEL PIC S9(0004) COMP.
000083     05  LSTNMEF PIC  X(0001).
000084     05  FILLER REDEFINES LSTNMEF.
000085         10  LSTNMEA PIC  X(0001).
000086     05  LSTNMEI PIC  X(0015).
000087*    -------------------------------
000088     05  FSTNMEL PIC S9(0004) COMP.
000089     05  FSTNMEF PIC  X(0001).
000090     05  FILLER REDEFINES FSTNMEF.
000091         10  FSTNMEA PIC  X(0001).
000092     05  FSTNMEI PIC  X(0012).
000093*    -------------------------------
000094     05  MINITL PIC S9(0004) COMP.
000095     05  MINITF PIC  X(0001).
000096     05  FILLER REDEFINES MINITF.
000097         10  MINITA PIC  X(0001).
000098     05  MINITI PIC  X(0001).
000099*    -------------------------------
000100     05  PRIMHDGL PIC S9(0004) COMP.
000101     05  PRIMHDGF PIC  X(0001).
000102     05  FILLER REDEFINES PRIMHDGF.
000103         10  PRIMHDGA PIC  X(0001).
000104     05  PRIMHDGI PIC  X(0012).
000105*    -------------------------------
000106     05  PRIMCRTL PIC S9(0004) COMP.
000107     05  PRIMCRTF PIC  X(0001).
000108     05  FILLER REDEFINES PRIMCRTF.
000109         10  PRIMCRTA PIC  X(0001).
000110     05  PRIMCRTI PIC  X(0010).
000111*    -------------------------------
000112     05  PRIMSFXL PIC S9(0004) COMP.
000113     05  PRIMSFXF PIC  X(0001).
000114     05  FILLER REDEFINES PRIMSFXF.
000115         10  PRIMSFXA PIC  X(0001).
000116     05  PRIMSFXI PIC  X(0001).
000117*    -------------------------------
000118     05  NOTIFYL PIC S9(0004) COMP.
000119     05  NOTIFYF PIC  X(0001).
000120     05  FILLER REDEFINES NOTIFYF.
000121         10  NOTIFYA PIC  X(0001).
000122     05  NOTIFYI PIC  X(0013).
000123*    -------------------------------
000124     05  CLMTOTHL PIC S9(0004) COMP.
000125     05  CLMTOTHF PIC  X(0001).
000126     05  FILLER REDEFINES CLMTOTHF.
000127         10  CLMTOTHA PIC  X(0001).
000128     05  CLMTOTHI PIC  X(0027).
000129*    -------------------------------
000130     05  AMTPDL PIC S9(0004) COMP.
000131     05  AMTPDF PIC  X(0001).
000132     05  FILLER REDEFINES AMTPDF.
000133         10  AMTPDA PIC  X(0001).
000134     05  AMTPDI PIC  X(0011).
000135*    -------------------------------
000136     05  DAYSPDHL PIC S9(0004) COMP.
000137     05  DAYSPDHF PIC  X(0001).
000138     05  FILLER REDEFINES DAYSPDHF.
000139         10  DAYSPDHA PIC  X(0001).
000140     05  DAYSPDHI PIC  X(0010).
000141*    -------------------------------
000142     05  DAYSPDL PIC S9(0004) COMP.
000143     05  DAYSPDF PIC  X(0001).
000144     05  FILLER REDEFINES DAYSPDF.
000145         10  DAYSPDA PIC  X(0001).
000146     05  DAYSPDI PIC  X(0005).
000147*    -------------------------------
000148     05  YESNOSWL PIC S9(0004) COMP.
000149     05  YESNOSWF PIC  X(0001).
000150     05  FILLER REDEFINES YESNOSWF.
000151         10  YESNOSWA PIC  X(0001).
000152     05  YESNOSWI PIC  X(0001).
000153*    -------------------------------
000154     05  PDTHRHDL PIC S9(0004) COMP.
000155     05  PDTHRHDF PIC  X(0001).
000156     05  FILLER REDEFINES PDTHRHDF.
000157         10  PDTHRHDA PIC  X(0001).
000158     05  PDTHRHDI PIC  X(0014).
000159*    -------------------------------
000160     05  PDTHRUL PIC S9(0004) COMP.
000161     05  PDTHRUF PIC  X(0001).
000162     05  FILLER REDEFINES PDTHRUF.
000163         10  PDTHRUA PIC  X(0001).
000164     05  PDTHRUI PIC  X(0008).
000165*    -------------------------------
000166     05  TOTPAIDL PIC S9(0004) COMP.
000167     05  TOTPAIDF PIC  X(0001).
000168     05  FILLER REDEFINES TOTPAIDF.
000169         10  TOTPAIDA PIC  X(0001).
000170     05  TOTPAIDI PIC  X(0010).
000171*    -------------------------------
000172     05  COVERDL PIC S9(0004) COMP.
000173     05  COVERDF PIC  X(0001).
000174     05  FILLER REDEFINES COVERDF.
000175         10  COVERDA PIC  X(0001).
000176     05  COVERDI PIC  X(0012).
000177*    -------------------------------
000178     05  COVERL PIC S9(0004) COMP.
000179     05  COVERF PIC  X(0001).
000180     05  FILLER REDEFINES COVERF.
000181         10  COVERA PIC  X(0001).
000182     05  COVERI PIC  X(0010).
000183*    -------------------------------
000184     05  INCURL PIC S9(0004) COMP.
000185     05  INCURF PIC  X(0001).
000186     05  FILLER REDEFINES INCURF.
000187         10  INCURA PIC  X(0001).
000188     05  INCURI PIC  X(0008).
000189*    -------------------------------
000190     05  NOPMTSL PIC S9(0004) COMP.
000191     05  NOPMTSF PIC  X(0001).
000192     05  FILLER REDEFINES NOPMTSF.
000193         10  NOPMTSA PIC  X(0001).
000194     05  NOPMTSI PIC  X(0007).
000195*    -------------------------------
000196     05  PRMTYPDL PIC S9(0004) COMP.
000197     05  PRMTYPDF PIC  X(0001).
000198     05  FILLER REDEFINES PRMTYPDF.
000199         10  PRMTYPDA PIC  X(0001).
000200     05  PRMTYPDI PIC  X(0012).
000201*    -------------------------------
000202     05  PRMTYPEL PIC S9(0004) COMP.
000203     05  PRMTYPEF PIC  X(0001).
000204     05  FILLER REDEFINES PRMTYPEF.
000205         10  PRMTYPEA PIC  X(0001).
000206     05  PRMTYPEI PIC  X(0008).
000207*    -------------------------------
000208     05  NXTAUTOL PIC S9(0004) COMP.
000209     05  NXTAUTOF PIC  X(0001).
000210     05  FILLER REDEFINES NXTAUTOF.
000211         10  NXTAUTOA PIC  X(0001).
000212     05  NXTAUTOI PIC  X(0008).
000213*    -------------------------------
000214     05  ESTABDTL PIC S9(0004) COMP.
000215     05  ESTABDTF PIC  X(0001).
000216     05  FILLER REDEFINES ESTABDTF.
000217         10  ESTABDTA PIC  X(0001).
000218     05  ESTABDTI PIC  X(0008).
000219*    -------------------------------
000220     05  CRTSTADL PIC S9(0004) COMP.
000221     05  CRTSTADF PIC  X(0001).
000222     05  FILLER REDEFINES CRTSTADF.
000223         10  CRTSTADA PIC  X(0001).
000224     05  CRTSTADI PIC  X(0012).
000225*    -------------------------------
000226     05  CRTSTATL PIC S9(0004) COMP.
000227     05  CRTSTATF PIC  X(0001).
000228     05  FILLER REDEFINES CRTSTATF.
000229         10  CRTSTATA PIC  X(0001).
000230     05  CRTSTATI PIC  X(0010).
000231*    -------------------------------
000232     05  EFFECTL PIC S9(0004) COMP.
000233     05  EFFECTF PIC  X(0001).
000234     05  FILLER REDEFINES EFFECTF.
000235         10  EFFECTA PIC  X(0001).
000236     05  EFFECTI PIC  X(0008).
000237*    -------------------------------
000238     05  TOTINTL PIC S9(0004) COMP.
000239     05  TOTINTF PIC  X(0001).
000240     05  FILLER REDEFINES TOTINTF.
000241         10  TOTINTA PIC  X(0001).
000242     05  TOTINTI PIC  S9(7)V99.
000243*    -------------------------------
000244     05  STATEL PIC S9(0004) COMP.
000245     05  STATEF PIC  X(0001).
000246     05  FILLER REDEFINES STATEF.
000247         10  STATEA PIC  X(0001).
000248     05  STATEI PIC  X(0003).
000249*    -------------------------------
000250     05  ACCTL PIC S9(0004) COMP.
000251     05  ACCTF PIC  X(0001).
000252     05  FILLER REDEFINES ACCTF.
000253         10  ACCTA PIC  X(0001).
000254     05  ACCTI PIC  X(0010).
000255*    -------------------------------
000256     05  EXPIREL PIC S9(0004) COMP.
000257     05  EXPIREF PIC  X(0001).
000258     05  FILLER REDEFINES EXPIREF.
000259         10  EXPIREA PIC  X(0001).
000260     05  EXPIREI PIC  X(0008).
000261*    -------------------------------
000262     05  BENECAPL PIC S9(0004) COMP.
000263     05  BENECAPF PIC  X(0001).
000264     05  FILLER REDEFINES BENECAPF.
000265         10  BENECAPA PIC  X(0001).
000266     05  BENECAPI PIC  X(0013).
000267*    -------------------------------
000268     05  BENEL PIC S9(0004) COMP.
000269     05  BENEF PIC  X(0001).
000270     05  FILLER REDEFINES BENEF.
000271         10  BENEA PIC  X(0001).
000272     05  BENEI PIC  X(0009).
000273*    -------------------------------
000274     05  TERMSL PIC S9(0004) COMP.
000275     05  TERMSF PIC  X(0001).
000276     05  FILLER REDEFINES TERMSF.
000277         10  TERMSA PIC  X(0001).
000278     05  TERMSI PIC  X(0013).
000279*    -------------------------------
000280     05  DIAGL PIC S9(0004) COMP.
000281     05  DIAGF PIC  X(0001).
000282     05  FILLER REDEFINES DIAGF.
000283         10  DIAGA PIC  X(0001).
000284     05  DIAGI PIC  X(0056).
000285*    -------------------------------
000286     05  FILETOL PIC S9(0004) COMP.
000287     05  FILETOF PIC  X(0001).
000288     05  FILLER REDEFINES FILETOF.
000289         10  FILETOA PIC  X(0001).
000290     05  FILETOI PIC  X(0004).
000291*    -------------------------------
000292     05  PRTOPTL PIC S9(0004) COMP.
000293     05  PRTOPTF PIC  X(0001).
000294     05  FILLER REDEFINES PRTOPTF.
000295         10  PRTOPTA PIC  X(0001).
000296     05  PRTOPTI PIC  X(0001).
000297*    -------------------------------
000298     05  FORMATL PIC S9(0004) COMP.
000299     05  FORMATF PIC  X(0001).
000300     05  FILLER REDEFINES FORMATF.
000301         10  FORMATA PIC  X(0001).
000302     05  FORMATI PIC  X(0001).
000303*    -------------------------------
000304     05  ALTPRTL PIC S9(0004) COMP.
000305     05  ALTPRTF PIC  X(0001).
000306     05  FILLER REDEFINES ALTPRTF.
000307         10  ALTPRTA PIC  X(0001).
000308     05  ALTPRTI PIC  X(0004).
000309*    -------------------------------
000310     05  CLOANL PIC S9(0004) COMP.
000311     05  CLOANF PIC  X(0001).
000312     05  FILLER REDEFINES CLOANF.
000313         10  CLOANA PIC  X(0001).
000314     05  CLOANI PIC  X(0025).
000315*    -------------------------------
000316     05  DENTYPL PIC S9(0004) COMP.
000317     05  DENTYPF PIC  X(0001).
000318     05  FILLER REDEFINES DENTYPF.
000319         10  DENTYPA PIC  X(0001).
000320     05  DENTYPI PIC  X(0029).
000321*    -------------------------------
000322     05  ACTCDL PIC S9(0004) COMP.
000323     05  ACTCDF PIC  X(0001).
000324     05  FILLER REDEFINES ACTCDF.
000325         10  ACTCDA PIC  X(0001).
000326     05  ACTCDI PIC  99.
000327*    -------------------------------
000328     05  ACTDTL PIC S9(0004) COMP.
000329     05  ACTDTF PIC  X(0001).
000330     05  FILLER REDEFINES ACTDTF.
000331         10  ACTDTA PIC  X(0001).
000332     05  ACTDTI PIC  X(0008).
000333*    -------------------------------
000334     05  ACTTYPL PIC S9(0004) COMP.
000335     05  ACTTYPF PIC  X(0001).
000336     05  FILLER REDEFINES ACTTYPF.
000337         10  ACTTYPA PIC  X(0001).
000338     05  ACTTYPI PIC  X(0004).
000339*    -------------------------------
000340     05  ASSHDGL PIC S9(0004) COMP.
000341     05  ASSHDGF PIC  X(0001).
000342     05  FILLER REDEFINES ASSHDGF.
000343         10  ASSHDGA PIC  X(0001).
000344     05  ASSHDGI PIC  X(0006).
000345*    -------------------------------
000346     05  ASSOCL PIC S9(0004) COMP.
000347     05  ASSOCF PIC  X(0001).
000348     05  FILLER REDEFINES ASSOCF.
000349         10  ASSOCA PIC  X(0001).
000350     05  ASSOCI PIC  X(0001).
000351*    -------------------------------
000352     05  TRLR1L PIC S9(0004) COMP.
000353     05  TRLR1F PIC  X(0001).
000354     05  FILLER REDEFINES TRLR1F.
000355         10  TRLR1A PIC  X(0001).
000356     05  TRLR1I PIC  X(0079).
000357*    -------------------------------
000358     05  TRLR2L PIC S9(0004) COMP.
000359     05  TRLR2F PIC  X(0001).
000360     05  FILLER REDEFINES TRLR2F.
000361         10  TRLR2A PIC  X(0001).
000362     05  TRLR2I PIC  X(0079).
000363*    -------------------------------
000364     05  TRLR3L PIC S9(0004) COMP.
000365     05  TRLR3F PIC  X(0001).
000366     05  FILLER REDEFINES TRLR3F.
000367         10  TRLR3A PIC  X(0001).
000368     05  TRLR3I PIC  X(0079).
000369*    -------------------------------
000370     05  TRLR4L PIC S9(0004) COMP.
000371     05  TRLR4F PIC  X(0001).
000372     05  FILLER REDEFINES TRLR4F.
000373         10  TRLR4A PIC  X(0001).
000374     05  TRLR4I PIC  X(0079).
000375*    -------------------------------
000376     05  ERRMSG1L PIC S9(0004) COMP.
000377     05  ERRMSG1F PIC  X(0001).
000378     05  FILLER REDEFINES ERRMSG1F.
000379         10  ERRMSG1A PIC  X(0001).
000380     05  ERRMSG1I PIC  X(0079).
000381*    -------------------------------
000382     05  ENTERPFL PIC S9(0004) COMP.
000383     05  ENTERPFF PIC  X(0001).
000384     05  FILLER REDEFINES ENTERPFF.
000385         10  ENTERPFA PIC  X(0001).
000386     05  ENTERPFI PIC  99.
000387*    -------------------------------
000388     05  PFKEY11L PIC S9(0004) COMP.
000389     05  PFKEY11F PIC  X(0001).
000390     05  FILLER REDEFINES PFKEY11F.
000391         10  PFKEY11A PIC  X(0001).
000392     05  PFKEY11I PIC  X(0017).
000393 01  EL150AO REDEFINES EL150AI.
000394     05  FILLER            PIC  X(0012).
000395*    -------------------------------
000396     05  FILLER            PIC  X(0003).
000397     05  RUNDTEO PIC  X(0008).
000398*    -------------------------------
000399     05  FILLER            PIC  X(0003).
000400     05  RUNTIMEO PIC  99.99.
000401*    -------------------------------
000402     05  FILLER            PIC  X(0003).
000403     05  SEQUO PIC  X(0010).
000404*    -------------------------------
000405     05  FILLER            PIC  X(0003).
000406     05  CMPNYIDO PIC  X(0003).
000407*    -------------------------------
000408     05  FILLER            PIC  X(0003).
000409     05  USERIDO PIC  X(0004).
000410*    -------------------------------
000411     05  FILLER            PIC  X(0003).
000412     05  RETMSTO PIC  X(0015).
000413*    -------------------------------
000414     05  FILLER            PIC  X(0003).
000415     05  CRDCARDO PIC  X(0016).
000416*    -------------------------------
000417     05  FILLER            PIC  X(0003).
000418     05  CLMNOO PIC  X(0007).
000419*    -------------------------------
000420     05  FILLER            PIC  X(0003).
000421     05  CARRO PIC  X(0001).
000422*    -------------------------------
000423     05  FILLER            PIC  X(0003).
000424     05  CERTNOO PIC  X(0010).
000425*    -------------------------------
000426     05  FILLER            PIC  X(0003).
000427     05  SUFXO PIC  X(0001).
000428*    -------------------------------
000429     05  FILLER            PIC  X(0003).
000430     05  CLMTYPO PIC  X(0006).
000431*    -------------------------------
000432     05  FILLER            PIC  X(0003).
000433     05  CLMSTATO PIC  X(0006).
000434*    -------------------------------
000435     05  FILLER            PIC  X(0003).
000436     05  LSTNMEO PIC  X(0015).
000437*    -------------------------------
000438     05  FILLER            PIC  X(0003).
000439     05  FSTNMEO PIC  X(0012).
000440*    -------------------------------
000441     05  FILLER            PIC  X(0003).
000442     05  MINITO PIC  X(0001).
000443*    -------------------------------
000444     05  FILLER            PIC  X(0003).
000445     05  PRIMHDGO PIC  X(0012).
000446*    -------------------------------
000447     05  FILLER            PIC  X(0003).
000448     05  PRIMCRTO PIC  X(0010).
000449*    -------------------------------
000450     05  FILLER            PIC  X(0003).
000451     05  PRIMSFXO PIC  X(0001).
000452*    -------------------------------
000453     05  FILLER            PIC  X(0003).
000454     05  NOTIFYO PIC  X(0013).
000455*    -------------------------------
000456     05  FILLER            PIC  X(0003).
000457     05  CLMTOTHO PIC  X(0027).
000458*    -------------------------------
000459     05  FILLER            PIC  X(0003).
000460     05  AMTPDO PIC  X(0011).
000461*    -------------------------------
000462     05  FILLER            PIC  X(0003).
000463     05  DAYSPDHO PIC  X(0010).
000464*    -------------------------------
000465     05  FILLER            PIC  X(0003).
000466     05  DAYSPDO PIC  X(0005).
000467*    -------------------------------
000468     05  FILLER            PIC  X(0003).
000469     05  YESNOSWO PIC  X(0001).
000470*    -------------------------------
000471     05  FILLER            PIC  X(0003).
000472     05  PDTHRHDO PIC  X(0014).
000473*    -------------------------------
000474     05  FILLER            PIC  X(0003).
000475     05  PDTHRUO PIC  X(0008).
000476*    -------------------------------
000477     05  FILLER            PIC  X(0003).
000478     05  TOTPAIDO PIC  Z(6).99-.
000479*    -------------------------------
000480     05  FILLER            PIC  X(0003).
000481     05  COVERDO PIC  X(0012).
000482*    -------------------------------
000483     05  FILLER            PIC  X(0003).
000484     05  COVERO PIC  X(0010).
000485*    -------------------------------
000486     05  FILLER            PIC  X(0003).
000487     05  INCURO PIC  X(0008).
000488*    -------------------------------
000489     05  FILLER            PIC  X(0003).
000490     05  NOPMTSO PIC  X(0007).
000491*    -------------------------------
000492     05  FILLER            PIC  X(0003).
000493     05  PRMTYPDO PIC  X(0012).
000494*    -------------------------------
000495     05  FILLER            PIC  X(0003).
000496     05  PRMTYPEO PIC  X(0008).
000497*    -------------------------------
000498     05  FILLER            PIC  X(0003).
000499     05  NXTAUTOO PIC  X(0008).
000500*    -------------------------------
000501     05  FILLER            PIC  X(0003).
000502     05  ESTABDTO PIC  X(0008).
000503*    -------------------------------
000504     05  FILLER            PIC  X(0003).
000505     05  CRTSTADO PIC  X(0012).
000506*    -------------------------------
000507     05  FILLER            PIC  X(0003).
000508     05  CRTSTATO PIC  X(0010).
000509*    -------------------------------
000510     05  FILLER            PIC  X(0003).
000511     05  EFFECTO PIC  X(0008).
000512*    -------------------------------
000513     05  FILLER            PIC  X(0003).
000514     05  TOTINTO PIC  -(6).99.
000515*    -------------------------------
000516     05  FILLER            PIC  X(0003).
000517     05  STATEO PIC  X(0003).
000518*    -------------------------------
000519     05  FILLER            PIC  X(0003).
000520     05  ACCTO PIC  X(0010).
000521*    -------------------------------
000522     05  FILLER            PIC  X(0003).
000523     05  EXPIREO PIC  X(0008).
000524*    -------------------------------
000525     05  FILLER            PIC  X(0003).
000526     05  BENECAPO PIC  X(0013).
000527*    -------------------------------
000528     05  FILLER            PIC  X(0003).
000529     05  BENEO PIC  Z(6).99.
000530*    -------------------------------
000531     05  FILLER            PIC  X(0003).
000532     05  TERMSO PIC  X(0013).
000533*    -------------------------------
000534     05  FILLER            PIC  X(0003).
000535     05  DIAGO PIC  X(0056).
000536*    -------------------------------
000537     05  FILLER            PIC  X(0003).
000538     05  FILETOO PIC  X(0004).
000539*    -------------------------------
000540     05  FILLER            PIC  X(0003).
000541     05  PRTOPTO PIC  X(0001).
000542*    -------------------------------
000543     05  FILLER            PIC  X(0003).
000544     05  FORMATO PIC  X(0001).
000545*    -------------------------------
000546     05  FILLER            PIC  X(0003).
000547     05  ALTPRTO PIC  X(0004).
000548*    -------------------------------
000549     05  FILLER            PIC  X(0003).
000550     05  CLOANO PIC  X(0025).
000551*    -------------------------------
000552     05  FILLER            PIC  X(0003).
000553     05  DENTYPO PIC  X(0029).
000554*    -------------------------------
000555     05  FILLER            PIC  X(0003).
000556     05  ACTCDO PIC  X(0002).
000557*    -------------------------------
000558     05  FILLER            PIC  X(0003).
000559     05  ACTDTO PIC  X(0008).
000560*    -------------------------------
000561     05  FILLER            PIC  X(0003).
000562     05  ACTTYPO PIC  X(0004).
000563*    -------------------------------
000564     05  FILLER            PIC  X(0003).
000565     05  ASSHDGO PIC  X(0006).
000566*    -------------------------------
000567     05  FILLER            PIC  X(0003).
000568     05  ASSOCO PIC  X(0001).
000569*    -------------------------------
000570     05  FILLER            PIC  X(0003).
000571     05  TRLR1O PIC  X(0079).
000572*    -------------------------------
000573     05  FILLER            PIC  X(0003).
000574     05  TRLR2O PIC  X(0079).
000575*    -------------------------------
000576     05  FILLER            PIC  X(0003).
000577     05  TRLR3O PIC  X(0079).
000578*    -------------------------------
000579     05  FILLER            PIC  X(0003).
000580     05  TRLR4O PIC  X(0079).
000581*    -------------------------------
000582     05  FILLER            PIC  X(0003).
000583     05  ERRMSG1O PIC  X(0079).
000584*    -------------------------------
000585     05  FILLER            PIC  X(0003).
000586     05  ENTERPFO PIC  X(0002).
000587*    -------------------------------
000588     05  FILLER            PIC  X(0003).
000589     05  PFKEY11O PIC  X(0017).
000590*    -------------------------------
000591 01  EL150TI REDEFINES EL150AI.
000592     05  FILLER            PIC  X(0012).
000593*    -------------------------------
000594     05  RUNDTL PIC S9(0004) COMP.
000595     05  RUNDTF PIC  X(0001).
000596     05  FILLER REDEFINES RUNDTF.
000597         10  RUNDTA PIC  X(0001).
000598     05  RUNDTI PIC  X(0008).
000599*    -------------------------------
000600     05  RUNTIML PIC S9(0004) COMP.
000601     05  RUNTIMF PIC  X(0001).
000602     05  FILLER REDEFINES RUNTIMF.
000603         10  RUNTIMA PIC  X(0001).
000604     05  RUNTIMI PIC  X(0005).
000605*    -------------------------------
000606     05  CLMTYP1L PIC S9(0004) COMP.
000607     05  CLMTYP1F PIC  X(0001).
000608     05  FILLER REDEFINES CLMTYP1F.
000609         10  CLMTYP1A PIC  X(0001).
000610     05  CLMTYP1I PIC  X(0002).
000611*    -------------------------------
000612     05  BANK1L PIC S9(0004) COMP.
000613     05  BANK1F PIC  X(0001).
000614     05  FILLER REDEFINES BANK1F.
000615         10  BANK1A PIC  X(0001).
000616     05  BANK1I PIC  X(0003).
000617*    -------------------------------
000618     05  NOCLMS1L PIC S9(0004) COMP.
000619     05  NOCLMS1F PIC  X(0001).
000620     05  FILLER REDEFINES NOCLMS1F.
000621         10  NOCLMS1A PIC  X(0001).
000622     05  NOCLMS1I PIC  X(0003).
000623*    -------------------------------
000624     05  NODAYS1L PIC S9(0004) COMP.
000625     05  NODAYS1F PIC  X(0001).
000626     05  FILLER REDEFINES NODAYS1F.
000627         10  NODAYS1A PIC  X(0001).
000628     05  NODAYS1I PIC  X(0005).
000629*    -------------------------------
000630     05  TAMT1L PIC S9(0004) COMP.
000631     05  TAMT1F PIC  X(0001).
000632     05  FILLER REDEFINES TAMT1F.
000633         10  TAMT1A PIC  X(0001).
000634     05  TAMT1I PIC  X(0012).
000635*    -------------------------------
000636     05  CLMTYP2L PIC S9(0004) COMP.
000637     05  CLMTYP2F PIC  X(0001).
000638     05  FILLER REDEFINES CLMTYP2F.
000639         10  CLMTYP2A PIC  X(0001).
000640     05  CLMTYP2I PIC  X(0002).
000641*    -------------------------------
000642     05  BANK2L PIC S9(0004) COMP.
000643     05  BANK2F PIC  X(0001).
000644     05  FILLER REDEFINES BANK2F.
000645         10  BANK2A PIC  X(0001).
000646     05  BANK2I PIC  X(0003).
000647*    -------------------------------
000648     05  NOCLMS2L PIC S9(0004) COMP.
000649     05  NOCLMS2F PIC  X(0001).
000650     05  FILLER REDEFINES NOCLMS2F.
000651         10  NOCLMS2A PIC  X(0001).
000652     05  NOCLMS2I PIC  X(0003).
000653*    -------------------------------
000654     05  NODAYS2L PIC S9(0004) COMP.
000655     05  NODAYS2F PIC  X(0001).
000656     05  FILLER REDEFINES NODAYS2F.
000657         10  NODAYS2A PIC  X(0001).
000658     05  NODAYS2I PIC  X(0005).
000659*    -------------------------------
000660     05  TAMT2L PIC S9(0004) COMP.
000661     05  TAMT2F PIC  X(0001).
000662     05  FILLER REDEFINES TAMT2F.
000663         10  TAMT2A PIC  X(0001).
000664     05  TAMT2I PIC  X(0012).
000665*    -------------------------------
000666     05  CLMTYP3L PIC S9(0004) COMP.
000667     05  CLMTYP3F PIC  X(0001).
000668     05  FILLER REDEFINES CLMTYP3F.
000669         10  CLMTYP3A PIC  X(0001).
000670     05  CLMTYP3I PIC  X(0002).
000671*    -------------------------------
000672     05  BANK3L PIC S9(0004) COMP.
000673     05  BANK3F PIC  X(0001).
000674     05  FILLER REDEFINES BANK3F.
000675         10  BANK3A PIC  X(0001).
000676     05  BANK3I PIC  X(0003).
000677*    -------------------------------
000678     05  NOCLMS3L PIC S9(0004) COMP.
000679     05  NOCLMS3F PIC  X(0001).
000680     05  FILLER REDEFINES NOCLMS3F.
000681         10  NOCLMS3A PIC  X(0001).
000682     05  NOCLMS3I PIC  X(0003).
000683*    -------------------------------
000684     05  NODAYS3L PIC S9(0004) COMP.
000685     05  NODAYS3F PIC  X(0001).
000686     05  FILLER REDEFINES NODAYS3F.
000687         10  NODAYS3A PIC  X(0001).
000688     05  NODAYS3I PIC  X(0005).
000689*    -------------------------------
000690     05  TAMT3L PIC S9(0004) COMP.
000691     05  TAMT3F PIC  X(0001).
000692     05  FILLER REDEFINES TAMT3F.
000693         10  TAMT3A PIC  X(0001).
000694     05  TAMT3I PIC  X(0012).
000695*    -------------------------------
000696     05  CLMTYP4L PIC S9(0004) COMP.
000697     05  CLMTYP4F PIC  X(0001).
000698     05  FILLER REDEFINES CLMTYP4F.
000699         10  CLMTYP4A PIC  X(0001).
000700     05  CLMTYP4I PIC  X(0002).
000701*    -------------------------------
000702     05  BANK4L PIC S9(0004) COMP.
000703     05  BANK4F PIC  X(0001).
000704     05  FILLER REDEFINES BANK4F.
000705         10  BANK4A PIC  X(0001).
000706     05  BANK4I PIC  X(0003).
000707*    -------------------------------
000708     05  NOCLMS4L PIC S9(0004) COMP.
000709     05  NOCLMS4F PIC  X(0001).
000710     05  FILLER REDEFINES NOCLMS4F.
000711         10  NOCLMS4A PIC  X(0001).
000712     05  NOCLMS4I PIC  X(0003).
000713*    -------------------------------
000714     05  NODAYS4L PIC S9(0004) COMP.
000715     05  NODAYS4F PIC  X(0001).
000716     05  FILLER REDEFINES NODAYS4F.
000717         10  NODAYS4A PIC  X(0001).
000718     05  NODAYS4I PIC  X(0005).
000719*    -------------------------------
000720     05  TAMT4L PIC S9(0004) COMP.
000721     05  TAMT4F PIC  X(0001).
000722     05  FILLER REDEFINES TAMT4F.
000723         10  TAMT4A PIC  X(0001).
000724     05  TAMT4I PIC  X(0012).
000725*    -------------------------------
000726     05  CLMTYP5L PIC S9(0004) COMP.
000727     05  CLMTYP5F PIC  X(0001).
000728     05  FILLER REDEFINES CLMTYP5F.
000729         10  CLMTYP5A PIC  X(0001).
000730     05  CLMTYP5I PIC  X(0002).
000731*    -------------------------------
000732     05  BANK5L PIC S9(0004) COMP.
000733     05  BANK5F PIC  X(0001).
000734     05  FILLER REDEFINES BANK5F.
000735         10  BANK5A PIC  X(0001).
000736     05  BANK5I PIC  X(0003).
000737*    -------------------------------
000738     05  NOCLMS5L PIC S9(0004) COMP.
000739     05  NOCLMS5F PIC  X(0001).
000740     05  FILLER REDEFINES NOCLMS5F.
000741         10  NOCLMS5A PIC  X(0001).
000742     05  NOCLMS5I PIC  X(0003).
000743*    -------------------------------
000744     05  NODAYS5L PIC S9(0004) COMP.
000745     05  NODAYS5F PIC  X(0001).
000746     05  FILLER REDEFINES NODAYS5F.
000747         10  NODAYS5A PIC  X(0001).
000748     05  NODAYS5I PIC  X(0005).
000749*    -------------------------------
000750     05  TAMT5L PIC S9(0004) COMP.
000751     05  TAMT5F PIC  X(0001).
000752     05  FILLER REDEFINES TAMT5F.
000753         10  TAMT5A PIC  X(0001).
000754     05  TAMT5I PIC  X(0012).
000755 01  EL150TO REDEFINES EL150AI.
000756     05  FILLER            PIC  X(0012).
000757*    -------------------------------
000758     05  FILLER            PIC  X(0003).
000759     05  RUNDTO PIC  X(0008).
000760*    -------------------------------
000761     05  FILLER            PIC  X(0003).
000762     05  RUNTIMO PIC  99.99.
000763*    -------------------------------
000764     05  FILLER            PIC  X(0003).
000765     05  CLMTYP1O PIC  X(0002).
000766*    -------------------------------
000767     05  FILLER            PIC  X(0003).
000768     05  BANK1O PIC  999.
000769*    -------------------------------
000770     05  FILLER            PIC  X(0003).
000771     05  NOCLMS1O PIC  999.
000772*    -------------------------------
000773     05  FILLER            PIC  X(0003).
000774     05  NODAYS1O PIC  99999.
000775*    -------------------------------
000776     05  FILLER            PIC  X(0003).
000777     05  TAMT1O PIC  9,999,999.99.
000778*    -------------------------------
000779     05  FILLER            PIC  X(0003).
000780     05  CLMTYP2O PIC  X(0002).
000781*    -------------------------------
000782     05  FILLER            PIC  X(0003).
000783     05  BANK2O PIC  999.
000784*    -------------------------------
000785     05  FILLER            PIC  X(0003).
000786     05  NOCLMS2O PIC  999.
000787*    -------------------------------
000788     05  FILLER            PIC  X(0003).
000789     05  NODAYS2O PIC  99999.
000790*    -------------------------------
000791     05  FILLER            PIC  X(0003).
000792     05  TAMT2O PIC  9,999,999.99.
000793*    -------------------------------
000794     05  FILLER            PIC  X(0003).
000795     05  CLMTYP3O PIC  X(0002).
000796*    -------------------------------
000797     05  FILLER            PIC  X(0003).
000798     05  BANK3O PIC  999.
000799*    -------------------------------
000800     05  FILLER            PIC  X(0003).
000801     05  NOCLMS3O PIC  999.
000802*    -------------------------------
000803     05  FILLER            PIC  X(0003).
000804     05  NODAYS3O PIC  99999.
000805*    -------------------------------
000806     05  FILLER            PIC  X(0003).
000807     05  TAMT3O PIC  9,999,999.99.
000808*    -------------------------------
000809     05  FILLER            PIC  X(0003).
000810     05  CLMTYP4O PIC  X(0002).
000811*    -------------------------------
000812     05  FILLER            PIC  X(0003).
000813     05  BANK4O PIC  999.
000814*    -------------------------------
000815     05  FILLER            PIC  X(0003).
000816     05  NOCLMS4O PIC  999.
000817*    -------------------------------
000818     05  FILLER            PIC  X(0003).
000819     05  NODAYS4O PIC  99999.
000820*    -------------------------------
000821     05  FILLER            PIC  X(0003).
000822     05  TAMT4O PIC  9,999,999.99.
000823*    -------------------------------
000824     05  FILLER            PIC  X(0003).
000825     05  CLMTYP5O PIC  X(0002).
000826*    -------------------------------
000827     05  FILLER            PIC  X(0003).
000828     05  BANK5O PIC  999.
000829*    -------------------------------
000830     05  FILLER            PIC  X(0003).
000831     05  NOCLMS5O PIC  999.
000832*    -------------------------------
000833     05  FILLER            PIC  X(0003).
000834     05  NODAYS5O PIC  99999.
000835*    -------------------------------
000836     05  FILLER            PIC  X(0003).
000837     05  TAMT5O PIC  9,999,999.99.
000838*    -------------------------------
      *<<((file: EL150S))
000698 01  EL150AI-R REDEFINES EL150AI.
000699*    12  FILLER              PIC X(665).
000700*    12  FILLER              PIC X(706).
000701     12  FILLER              PIC X(738).
000702     12  EL150AI-OCCURS         OCCURS 4 TIMES.
000703         16  FILLER          PIC X(3).
000704         16  MAP-DISPLAY     PIC X(79).
000705     12  FILLER              PIC X(40).
000706
000707     EJECT
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
000709 01  DFHCOMMAREA             PIC X(1024).
000710
000711 01  CLAIM-MASTER-L          PIC X(350).
000712     EJECT
000713*                            COPY ELCCNTL.
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
000714     EJECT
000715*                            COPY ELCCERT.
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
000716     EJECT
000717*                            COPY ELCTRLR.
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
000718     EJECT
000719*                            COPY ELCACTQ.
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
000720     EJECT
000721*                            COPY ERCACCT.
      *>>((file: ERCACCT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCACCT                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.031                          *
000007*                                                                *
000008*   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
000009*                                                                *
000010*   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
000011*   VSAM ACCOUNT MASTER FILES.                                   *
000012*                                                                *
000013*   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
000014*                                                                *
000015*   FILE TYPE = VSAM,KSDS                                        *
000016*   RECORD SIZE = 2000  RECFORM = FIX                            *
000017*                                                                *
000018*   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
000019*       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
000020*                                                                *
000021*   LOG = NO                                                     *
000022*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000023*                                                                *
000024*                                                                *
000025******************************************************************
000026*                   C H A N G E   L O G
000027*
000028* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000029*-----------------------------------------------------------------
000030*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000031* EFFECTIVE    NUMBER
000032*-----------------------------------------------------------------
000033* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
000034* 092705    2005050300006  PEMA  ADD SPP LEASES
000035* 022808    2007083100002  PEMA  ADD FREEZE STATUS
000036* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000037* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
000038* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
000039* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
000040* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
000041******************************************************************
000042
000043 01  ACCOUNT-MASTER.
000044     12  AM-RECORD-ID                      PIC XX.
000045         88  VALID-AM-ID                      VALUE 'AM'.
000046
000047     12  AM-CONTROL-PRIMARY.
000048         16  AM-COMPANY-CD                 PIC X.
000049         16  AM-MSTR-CNTRL.
000050             20  AM-CONTROL-A.
000051                 24  AM-CARRIER            PIC X.
000052                 24  AM-GROUPING.
000053                     28 AM-GROUPING-PREFIX PIC XXX.
000054                     28 AM-GROUPING-PRIME  PIC XXX.
000055                 24  AM-STATE              PIC XX.
000056                 24  AM-ACCOUNT.
000057                     28  AM-ACCOUNT-PREFIX PIC X(4).
000058                     28  AM-ACCOUNT-PRIME  PIC X(6).
000059             20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
000060                                           PIC X(19).
000061             20  AM-CNTRL-B.
000062                 24  AM-EXPIRATION-DT      PIC XX.
000063                 24  FILLER                PIC X(4).
000064             20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
000065                 24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
000066
000067     12  AM-CONTROL-BY-VAR-GRP.
000068         16  AM-COMPANY-CD-A1              PIC X.
000069         16  AM-VG-CARRIER                 PIC X.
000070         16  AM-VG-GROUPING                PIC X(6).
000071         16  AM-VG-STATE                   PIC XX.
000072         16  AM-VG-ACCOUNT                 PIC X(10).
000073         16  AM-VG-DATE.
000074             20  AM-VG-EXPIRATION-DT       PIC XX.
000075             20  FILLER                    PIC X(4).
000076         16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
000077                                           PIC 9(11)      COMP-3.
000078     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
000079         16  FILLER                        PIC X(10).
000080         16  AM-VG-KEY3.
000081             20  AM-VG3-ACCOUNT            PIC X(10).
000082             20  AM-VG3-EXP-DT             PIC XX.
000083         16  FILLER                        PIC X(4).
000084     12  AM-MAINT-INFORMATION.
000085         16  AM-LAST-MAINT-DT              PIC XX.
000086         16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000087         16  AM-LAST-MAINT-USER            PIC X(4).
000088         16  FILLER                        PIC XX.
000089
000090     12  AM-EFFECTIVE-DT                   PIC XX.
000091     12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
000092
000093     12  AM-PREV-DATES  COMP-3.
000094         16  AM-PREV-EXP-DT                PIC 9(11).
000095         16  AM-PREV-EFF-DT                PIC 9(11).
000096
000097     12  AM-REPORT-CODE-1                  PIC X(10).
000098     12  AM-REPORT-CODE-2                  PIC X(10).
000099
000100     12  AM-CITY-CODE                      PIC X(4).
000101     12  AM-COUNTY-PARISH                  PIC X(6).
000102
000103     12  AM-NAME                           PIC X(30).
000104     12  AM-PERSON                         PIC X(30).
000105     12  AM-ADDRS                          PIC X(30).
000106     12  AM-CITY.
000107         16  AM-ADDR-CITY                  PIC X(28).
000108         16  AM-ADDR-STATE                 PIC XX.
000109     12  AM-ZIP.
000110         16  AM-ZIP-PRIME.
000111             20  AM-ZIP-PRI-1ST            PIC X.
000112                 88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
000113             20  FILLER                    PIC X(4).
000114         16  AM-ZIP-PLUS4                  PIC X(4).
000115     12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
000116         16  AM-CAN-POSTAL-1               PIC XXX.
000117         16  AM-CAN-POSTAL-2               PIC XXX.
000118         16  FILLER                        PIC XXX.
000119     12  AM-TEL-NO.
000120         16  AM-AREA-CODE                  PIC 999.
000121         16  AM-TEL-PRE                    PIC 999.
000122         16  AM-TEL-NBR                    PIC 9(4).
000123     12  AM-TEL-LOC                        PIC X.
000124         88  AM-TEL-AT-HOME                   VALUE 'H'.
000125         88  AM-TEL-AT-BUSINESS               VALUE 'B'.
000126
000127     12  AM-COMM-STRUCTURE.
000128         16  AM-DEFN-1.
000129             20  AM-AGT-COMMS       OCCURS 10 TIMES.
000130                 24  AM-AGT.
000131                     28  AM-AGT-PREFIX     PIC X(4).
000132                     28  AM-AGT-PRIME      PIC X(6).
000133                 24  AM-COM-TYP            PIC X.
000134                 24  AM-L-COM              PIC SV9(5)     COMP-3.
000135                 24  AM-J-COM              PIC SV9(5)     COMP-3.
000136                 24  AM-A-COM              PIC SV9(5)     COMP-3.
000137                 24  AM-RECALC-LV-INDIC    PIC X.
000138                 24  AM-RETRO-LV-INDIC     PIC X.
000139                 24  AM-GL-CODES           PIC X.
000140                 24  AM-COMM-CHARGEBACK    PIC 9(02).
000141                 24  FILLER                PIC X(01).
000142         16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
000143             20  AM-COM-TBLS        OCCURS 10 TIMES.
000144                 24  FILLER                PIC X(11).
000145                 24  AM-L-COMA             PIC XXX.
000146                 24  AM-J-COMA             PIC XXX.
000147                 24  AM-A-COMA             PIC XXX.
000148                 24  FILLER                PIC X(6).
000149
000150     12  AM-COMM-CHANGE-STATUS             PIC X.
000151         88  AM-COMMISSIONS-CHANGED           VALUE '*'.
000152
000153     12  AM-CSR-CODE                       PIC X(4).
000154
000155     12  AM-BILLING-STATUS                 PIC X.
000156         88  AM-ACCOUNT-BILLED                VALUE 'B'.
000157         88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
000158     12  AM-AUTO-REFUND-SW                 PIC X.
000159         88  AUTO-REFUNDS-USED                VALUE 'Y'.
000160         88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
000161     12  AM-GPCD                           PIC 99.
000162     12  AM-IG                             PIC X.
000163         88  AM-HAS-INDIVIDUAL                VALUE '1'.
000164         88  AM-HAS-GROUP                     VALUE '2'.
000165     12  AM-STATUS                         PIC X.
000166         88  AM-ACCOUNT-ACTIVE                VALUE '0'.
000167         88  AM-ACCOUNT-INACTIVE              VALUE '1'.
000168         88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
000169         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
000170         88  AM-ACCOUNT-FROZEN                VALUE '4'.
000171         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
000172         88  AM-ACCOUNT-DROPPED               VALUE '6'.
000173         88  AM-ACCOUNT-LAPSED                VALUE '7'.
000174         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
000175         88  AM-ACCOUNT-PENDING               VALUE '9'.
000176     12  AM-REMIT-TO                       PIC 99.
000177     12  AM-ID-NO                          PIC X(11).
000178
000179     12  AM-CAL-TABLE                      PIC XX.
000180     12  AM-LF-DEVIATION                   PIC XXX.
000181     12  AM-AH-DEVIATION                   PIC XXX.
000182     12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
000183     12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
000184     12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
000185     12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
000186     12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
000187     12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
000188
000189     12  AM-USER-FIELDS.
000190         16  AM-FLD-1                      PIC XX.
000191         16  AM-FLD-2                      PIC XX.
000192         16  AM-FLD-3                      PIC XX.
000193         16  AM-FLD-4                      PIC XX.
000194         16  AM-FLD-5                      PIC XX.
000195
000196     12  AM-1ST-PROD-DATE.
000197         16  AM-1ST-PROD-YR                PIC XX.
000198         16  AM-1ST-PROD-MO                PIC XX.
000199         16  AM-1ST-PROD-DA                PIC XX.
000200     12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
000201     12  AM-CERTS-PURGED-DATE.
000202         16  AM-PUR-YR                     PIC XX.
000203         16  AM-PUR-MO                     PIC XX.
000204         16  AM-PUR-DA                     PIC XX.
000205     12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
000206     12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
000207     12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
000208     12  AM-INACTIVE-DATE.
000209         16  AM-INA-MO                     PIC 99.
000210         16  AM-INA-DA                     PIC 99.
000211         16  AM-INA-YR                     PIC 99.
000212     12  AM-AR-HI-CERT-DATE                PIC XX.
000213
000214     12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
000215     12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
000216
000217     12  AM-OB-PAYMENT-MODE                PIC X.
000218         88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
000219         88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
000220         88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
000221         88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
000222
000223     12  AM-AH-ONLY-INDICATOR              PIC X.
000224         88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
000225         88  AM-NO-AH-ONLY                    VALUE 'N'.
000226
000227     12  AM-EDIT-LOAN-OFC                  PIC X(01).
000228
000229     12  AM-OVER-SHORT.
000230         16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
000231         16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
000232
000233     12  AM-DCC-PRODUCT-CODE               PIC XXX.
000234     12  AM-DCC-CLP-STATE                  PIC XX.
000235
000236     12  AM-RECALC-COMM                    PIC X.
000237     12  AM-RECALC-REIN                    PIC X.
000238
000239     12  AM-REI-TABLE                      PIC XXX.
000240     12  AM-REI-ET-LF                      PIC X.
000241     12  AM-REI-ET-AH                      PIC X.
000242     12  AM-REI-PE-LF                      PIC X.
000243     12  AM-REI-PE-AH                      PIC X.
000244     12  AM-REI-PRT-ST                     PIC X.
000245     12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
000246     12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
000247     12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
000248     12  AM-REI-GROUP-A                    PIC X(6).
000249     12  AM-REI-MORT                       PIC X(4).
000250     12  AM-REI-PRT-OW                     PIC X.
000251     12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
000252     12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
000253     12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
000254     12  AM-REI-GROUP-B                    PIC X(6).
000255
000256     12  AM-TRUST-TYPE                     PIC X(2).
000257
000258     12  AM-EMPLOYER-STMT-USED             PIC X.
000259     12  AM-GROUPED-CHECKS-Y-N             PIC X.
000260
000261     12  AM-STD-AH-TYPE                    PIC XX.
000262     12  AM-EARN-METHODS.
000263         16  AM-EARN-METHOD-R              PIC X.
000264             88 AM-REF-RL-R78                 VALUE 'R'.
000265             88 AM-REF-RL-PR                  VALUE 'P'.
000266             88 AM-REF-RL-MEAN                VALUE 'M'.
000267             88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
000268         16  AM-EARN-METHOD-L              PIC X.
000269             88 AM-REF-LL-R78                 VALUE 'R'.
000270             88 AM-REF-LL-PR                  VALUE 'P'.
000271             88 AM-REF-LL-MEAN                VALUE 'M'.
000272             88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
000273         16  AM-EARN-METHOD-A              PIC X.
000274             88 AM-REF-AH-R78                 VALUE 'R'.
000275             88 AM-REF-AH-PR                  VALUE 'P'.
000276             88 AM-REF-AH-MEAN                VALUE 'M'.
000277             88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
000278             88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
000279             88 AM-REF-AH-NET                 VALUE 'N'.
000280
000281     12  AM-TOL-PREM                       PIC S999V99    COMP-3.
000282     12  AM-TOL-REF                        PIC S999V99    COMP-3.
000283     12  AM-TOL-CLM                        PIC S999V99    COMP-3.
000284
000285     12  AM-RET-Y-N                        PIC X.
000286     12  AM-RET-P-E                        PIC X.
000287     12  AM-LF-RET                         PIC S9V9999    COMP-3.
000288     12  AM-AH-RET                         PIC S9V9999    COMP-3.
000289     12  AM-RET-GRP                        PIC X(6).
000290     12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
000291         16  AM-POOL-PRIME                 PIC XXX.
000292         16  AM-POOL-SUB                   PIC XXX.
000293     12  AM-RETRO-EARNINGS.
000294         16  AM-RET-EARN-R                 PIC X.
000295         16  AM-RET-EARN-L                 PIC X.
000296         16  AM-RET-EARN-A                 PIC X.
000297     12  AM-RET-ST-TAX-USE                 PIC X.
000298         88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
000299         88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
000300     12  AM-RETRO-BEG-EARNINGS.
000301         16  AM-RET-BEG-EARN-R             PIC X.
000302         16  AM-RET-BEG-EARN-L             PIC X.
000303         16  AM-RET-BEG-EARN-A             PIC X.
000304     12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
000305     12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
000306
000307     12  AM-USER-SELECT-OPTIONS.
000308         16  AM-USER-SELECT-1              PIC X(10).
000309         16  AM-USER-SELECT-2              PIC X(10).
000310         16  AM-USER-SELECT-3              PIC X(10).
000311         16  AM-USER-SELECT-4              PIC X(10).
000312         16  AM-USER-SELECT-5              PIC X(10).
000313
000314     12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
000315
000316     12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
000317
000318     12  AM-RPT045A-SWITCH                 PIC X.
000319         88  RPT045A-OFF                   VALUE 'N'.
000320
000321     12  AM-INSURANCE-LIMITS.
000322         16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
000323         16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
000324
000325     12  AM-PROFILE-CHANGE-SWITCH          PIC X.
000326         88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
000327
000328     12  AM-DISMBR-COVERAGE-SW             PIC X.
000329         88  AM-DISMBR-COVERAGE               VALUE 'Y'.
000330         88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
000331
000332     12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
000333
000334     12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
000335     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
000336     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
000337     12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
000338     12  AM-DCC-UEF-STATE                  PIC XX.
000339     12  FILLER                            PIC XXX.
000340     12  AM-REPORT-CODE-3                  PIC X(10).
000341*    12  FILLER                            PIC X(22).
000342
000343     12  AM-RESERVE-DATE.
000344         16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
000345         16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
000346         16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
000347
000348     12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
000349     12  AM-NOTIFICATION-TYPES.
000350         16  AM-NOTIF-OF-LETTERS           PIC X.
000351         16  AM-NOTIF-OF-PAYMENTS          PIC X.
000352         16  AM-NOTIF-OF-REPORTS           PIC X.
000353         16  AM-NOTIF-OF-STATUS            PIC X.
000354
000355     12  AM-BENEFIT-TABLE-USAGE            PIC X.
000356         88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
000357         88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
000358         88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
000359         88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
000360
000361     12  AM-BENEFIT-CONTROLS.
000362         16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
000363             20  AM-BENEFIT-CODE           PIC XX.
000364             20  AM-BENEFIT-TYPE           PIC X.
000365             20  AM-BENEFIT-REVISION       PIC XXX.
000366             20  AM-BENEFIT-REM-TERM       PIC X.
000367             20  AM-BENEFIT-RETRO-Y-N      PIC X.
000368             20  FILLER                    PIC XX.
000369         16  FILLER                        PIC X(80).
000370
000371     12  AM-TRANSFER-DATA.
000372         16  AM-TRANSFERRED-FROM.
000373             20  AM-TRNFROM-CARRIER        PIC X.
000374             20  AM-TRNFROM-GROUPING.
000375                 24  AM-TRNFROM-GRP-PREFIX PIC XXX.
000376                 24  AM-TRNFROM-GRP-PRIME  PIC XXX.
000377             20  AM-TRNFROM-STATE          PIC XX.
000378             20  AM-TRNFROM-ACCOUNT.
000379                 24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
000380                 24  AM-TRNFROM-ACCT-PRIME PIC X(6).
000381             20  AM-TRNFROM-DTE            PIC XX.
000382         16  AM-TRANSFERRED-TO.
000383             20  AM-TRNTO-CARRIER          PIC X.
000384             20  AM-TRNTO-GROUPING.
000385                 24  AM-TRNTO-GRP-PREFIX   PIC XXX.
000386                 24  AM-TRNTO-GRP-PRIME    PIC XXX.
000387             20  AM-TRNTO-STATE            PIC XX.
000388             20  AM-TRNTO-ACCOUNT.
000389                 24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
000390                 24  AM-TRNTO-ACCT-PRIME   PIC X(6).
000391             20  AM-TRNTO-DTE              PIC XX.
000392         16  FILLER                        PIC X(10).
000393
000394     12  AM-SAVED-REMIT-TO                 PIC 99.
000395
000396     12  AM-COMM-STRUCTURE-SAVED.
000397         16  AM-DEFN-1-SAVED.
000398             20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
000399                 24  AM-AGT-SV             PIC X(10).
000400                 24  AM-COM-TYP-SV         PIC X.
000401                 24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
000402                 24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
000403                 24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
000404                 24  AM-RECALC-LV-INDIC-SV PIC X.
000405                 24  FILLER                PIC X.
000406                 24  AM-GL-CODES-SV        PIC X.
000407                 24  AM-COM-CHARGEBACK-SV  PIC 99.
000408                 24  FILLER                PIC X.
000409         16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
000410             20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
000411                 24  FILLER                PIC X(11).
000412                 24  AM-L-COMA-SV          PIC XXX.
000413                 24  AM-J-COMA-SV          PIC XXX.
000414                 24  AM-A-COMA-SV          PIC XXX.
000415                 24  FILLER                PIC X(6).
000416
000417     12  AM-FLC-NET-PREMIUM-ALLOWANCE.
000418         16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
000419            20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
000420            20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
000421            20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
000422
000423     12  AM-ORIG-DEALER-NO                 PIC X(10).
000424     12  FILLER                            PIC X(120).
000425
000426     12  AM-ACCOUNT-EXECUTIVE-DATA.
000427         16  AM-CONTROL-NAME               PIC X(30).
000428         16  AM-EXECUTIVE-ONE.
000429             20  AM-EXEC1-NAME             PIC X(15).
000430             20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
000431                                                          COMP-3.
000432             20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
000433                                                          COMP-3.
000434         16  AM-EXECUTIVE-TWO.
000435             20  AM-EXEC2-NAME             PIC X(15).
000436             20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
000437                                                          COMP-3.
000438             20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
000439                                                          COMP-3.
000440
000441     12  AM-RETRO-ADDITIONAL-DATA.
000442         16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
000443         16  AM-RETRO-PREM-P-E             PIC X.
000444         16  AM-RETRO-CLMS-P-I             PIC X.
000445         16  AM-RETRO-RET-BRACKET-LF.
000446             20  AM-RETRO-RET-METHOD-LF    PIC X.
000447                 88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
000448                 88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
000449             20  AM-RETRO-RET-BASIS-LF     PIC X.
000450                 88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
000451                 88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
000452             20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
000453                 24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
000454                 24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
000455         16  AM-RETRO-RET-BRACKET-AH.
000456             20  AM-RETRO-RET-METHOD-AH    PIC X.
000457                 88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
000458                 88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
000459                 88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
000460             20  AM-RETRO-RET-BASIS-AH     PIC X.
000461                 88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
000462                 88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
000463             20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
000464                 24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
000465                 24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
000466
000467     12  AM-COMMENTS.
000468         16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
000469
000470     12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
000471         16  AM-FLI-RETRO-SHARE-CODE       PIC X.
000472         16  AM-FLI-BILLING-CODE           PIC X.
000473         16  AM-FLI-ALT-STATE-CODE         PIC XX.
000474         16  AM-FLI-UNITED-IDENT           PIC X.
000475         16  AM-FLI-INTEREST-LOST-DATA.
000476             20  AM-FLI-BANK-NO            PIC X(5).
000477             20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
000478             20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
000479             20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
000480         16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
000481             20  AM-FLI-AGT                PIC X(9).
000482             20  AM-FLI-AGT-COMM-ACC       PIC X.
000483             20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
000484         16  FILLER                        PIC X(102).
000485
000486     12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
000487         16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
000488             20  AM-BENEFIT-DMD-CODE         PIC XX.
000489             20  AM-BENEFIT-DMD-TYPE         PIC X.
000490             20  AM-BENEFIT-DMD-REVISION     PIC XXX.
000491             20  AM-BENEFIT-DMD-REM-TERM     PIC X.
000492             20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
000493         16  FILLER                          PIC X(10).
000494******************************************************************
      *<<((file: ERCACCT))
000722     EJECT
000723*                            COPY ELCARCH.
      *>>((file: ELCARCH))
000001******************************************************************
000002*                                                                *
000003*                            ELCARCH.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.007                          *
000006*                                                                *
000007*   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 090  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELARCH                        RKP=2,LEN=8     *
000013*       ALTERNATE PATH1 = ELARCH2 (RECORD TYPE)  RKP=10,LEN=8    *
000014*                                                                *
000015*   LOG = NO                                                     *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017*                                                                *
000018*  THERE ARE CID MODS IN COPYBOOK ELCARCH                        *
000019******************************************************************
000020 01  LETTER-ARCHIVE.
000021     12  LA-RECORD-ID                PIC XX.
000022         88  VALID-LA-ID                VALUE 'LA'.
000023
000024     12  LA-CONTROL-PRIMARY.
000025         16  LA-COMPANY-CD           PIC X.
000026         16  LA-ARCHIVE-NO           PIC S9(8)     COMP.
000027         16  LA-RECORD-TYPE          PIC X.
000028             88  LA-HEADER-DATA         VALUE '1'.
000029             88  LA-ADDRESS-DATA        VALUE '2'.
000030             88  LA-TEXT-DATA           VALUE '3'.
000031             88  LA-FORM-CONTROL-HDR    VALUE '4'.
000032         16  LA-LINE-SEQ-NO          PIC S9(4)     COMP.
000033
000034     12  LA-CONTROL-BY-TYPE.
000035         16  LA-COMPANY-CD-A1        PIC X.
000036         16  LA-RECORD-TYPE-A1       PIC X.
000037         16  LA-ARCHIVE-NO-A1        PIC S9(8)     COMP.
000038         16  LA-LINE-SEQ-NO-A1       PIC S9(4)     COMP.
000039
000040     12  LA-TEXT-RECORD.
000041         16  LA-SKIP-CONTROL         PIC XX.
000042             88  NO-LINES-SKIPPED       VALUE SPACES.
000043             88  SKIP-TO-NEXT-PAGE      VALUE '99'.
000044         16  LA-TEXT-LINE            PIC X(70).
000045
000046     12  LA-ADDRESS-RECORD  REDEFINES  LA-TEXT-RECORD.
000047         16  FILLER                  PIC XX.
000048         16  LA-ADDRESS-LINE         PIC X(30).
000049         16  FILLER                  PIC X(40).
000050
000051     12  LA-HEADER-RECORD  REDEFINES  LA-TEXT-RECORD.
000052         16  FILLER                  PIC XX.
000053         16  LA-CARRIER              PIC X.
000054         16  LA-CLAIM-NO             PIC X(7).
000055         16  LA-CERT-NO.
000056             20  LA-CERT-PRIME       PIC X(10).
000057             20  LA-CERT-SFX         PIC X.
000058         16  LA-NO-OF-COPIES         PIC S9.
000059         16  LA-RESEND-DATE          PIC XX.
000060         16  LA-PROCESSOR-CD         PIC X(4).
000061         16  LA-CREATION-DT          PIC XX.
000062         16  LA-INITIAL-PRINT-DATE   PIC XX.
000063         16  LA-RESEND-PRINT-DATE    PIC XX.
000064         16  LA-CORR-TRLR-SEQ        PIC S9(4)    COMP.
000065         16  LA-1ST-RESEND-PRINT-DT  PIC XX.
000066*
000067* -----  16  LA-DMD-ADDITIONAL-FIELDS.
000068*   I        20  LA-DMD-LETTER-FORM      PIC X(4).
000069*   I        20  LA-DMD-PROD-CODE        PIC XX.
000070*   I        20  LA-DMD-RES-ST           PIC XX.
000071*   I        20  LA-DMD-CORR-TRLR-SEQ    PIC S9(4)    COMP.
000072*   I        20  LA-DMD-LETTER-STATUS    PIC X.
000073*  NEW           88  LA-DMD-LETTER-ONLINE   VALUE '1'.
000074*  DMD           88  LA-DMD-LETTER-PURGED   VALUE '2'.
000075*  CHGS          88  LA-DMD-LETTER-RELOADED VALUE '3'.
000076*   I        20  LA-DMD-LETTER-PURGE-DT  PIC XX.
000077*   I        20  LA-DMD-LETTER-RELOAD-DT PIC XX.
000078*   I        20  LA-DMD-UND-CODE         PIC XX.
000079*   I        20  LA-DMD-BEN-CODE         PIC XX.
000080*   V    16  FILLER                  PIC X(15).
000081* -----
000082*
000083* REINSERTED  CSO  MODS
000084*
000085         16  FILLER.
000086             20  FILLER                  PIC X(29).
000087             20  LA-CSO-LETTER-STATUS    PIC X.
000088                 88  LA-CSO-LETTER-ONLINE   VALUE '1'.
000089                 88  LA-CSO-LETTER-PURGED   VALUE '2'.
000090                 88  LA-CSO-LETTER-RELOADED VALUE '3'.
000091             20  LA-CSO-LETTER-PURGE-DT  PIC XX.
000092             20  LA-CSO-LETTER-RELOAD-DT PIC XX.
000093*
000094
000095     12  LA-FORM-CONTROL-HEADER REDEFINES  LA-TEXT-RECORD.
000096         16  FILLER                  PIC XX.
000097         16  LA4-CARRIER             PIC X.
000098         16  LA4-CLAIM-NO            PIC X(7).
000099         16  LA4-CERT-NO.
000100             20  LA4-CERT-PRIME      PIC X(10).
000101             20  LA4-CERT-SFX        PIC X.
000102         16  LA4-NO-OF-COPIES        PIC S9.
000103         16  LA4-RESEND-DATE         PIC XX.
000104         16  LA4-PROCESSOR-CD        PIC X(4).
000105         16  LA4-CREATION-DT         PIC XX.
000106         16  LA4-INITIAL-PRINT-DATE  PIC XX.
000107         16  LA4-RESEND-PRINT-DATE   PIC XX.
000108         16  LA4-FORM-TRLR-SEQ       PIC S9(4)    COMP.
000109         16  LA4-FORM-TYPE           PIC X.
000110             88  LA4-INITIAL-FORM    VALUE '1'.
000111             88  LA4-PROGRESS-FORM   VALUE '2'.
000112         16  LA4-FORM-REM-PRINT-DT   PIC X(02).
000113         16  LA4-STATE               PIC X(02).
000114         16  FILLER                  PIC X(31).
000115******************************************************************
      *<<((file: ELCARCH))
000724*                            copy ELCCRTT.
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
000725     EJECT
000726*                COPY MTCPLCY REPLACING ==:TAG:== BY ==PM==.
      *>>((file: MTCPLCY))
000001******************************************************************
000002*                                                                *
000003*                           MTCPLCY                              *
000004*                            VMOD=1.003                          *
000005*                                                                *
000006*   FILE DESCRIPTION = POLICY MASTER                             *
000007*                                                                *
000008*   FILE TYPE = VSAM,KSDS                                        *
000009*   RECORD SIZE = 1200 RECFORM = FIXED                           *
000010*                                                                *
000011*   BASE CLUSTER = MPPLCY                         RKP=2,LEN=42   *
000012*       ALTERNATE PATH2 = ** NOT USED **                         *
000013*       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
000014*       ALTERNATE PATH4 = MPPLCY4 (BY REF. NO.)   RKP=60,LEN=25  *
000015*       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT )   RKP=85,LEN=27  *
000016*       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT )   RKP=112,LEN=15 *
000017*       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO.)   RKP=127,LEN=27 *
000018*                                                                *
000019*   LOG = YES                                                    *
000020*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000021******************************************************************
000022**WARNING*********************************************************
000023**ANY CHANGES TO THIS COPY BOOK MAY NEED CORRESPONDING CHANGES****
000024**TO THE FOLLOWING COPY BOOKS: MPCPOLUP                          *
000025**                             MPCPHSTD                          *
000026**                             MPCPHSTC                          *
000027**                             MPCPHSTT                          *
000028**                                                               *
000029******************************************************************
000030
000031 01  POLICY-MASTER.
000032     12  PM-RECORD-ID                   PIC XX.
000033         88  VALID-PM-ID                      VALUE 'PM'.
000034
000035******************************************************************
000036*   BASE CLUSTER = MPPLCY         (BASE KEY)      RKP=2,LEN=42   *
000037******************************************************************
000038
000039     12  PM-CONTROL-PRIMARY.
000040         16  PM-PRODUCER-PRIMARY.
000041             20  PM-PROD-PRIMARY.
000042                 24  PM-COMPANY-CD      PIC X.
000043                 24  PM-CGSP-KEY.
000044                     28  PM-CARRIER     PIC X.
000045                     28  PM-GROUPING.
000046                         32  PM-GROUPING-PREFIX
000047                                           PIC X(3).
000048                         32  PM-GROUPING-PRIME
000049                                           PIC X(3).
000050                     28  PM-STATE       PIC XX.
000051                     28  PM-PRODUCER.
000052                         32  PM-PRODUCER-PREFIX
000053                                           PIC X(4).
000054                         32  PM-PRODUCER-PRIME
000055                                           PIC X(6).
000056             20  PM-POLICY-EFF-DT       PIC XX.
000057         16  PM-REFERENCE-NUMBER.
000058             20  PM-REFNO-PRIME         PIC X(18).
000059             20  PM-REFNO-SFX           PIC XX.
000060
000061******************************************************************
000062*       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
000063******************************************************************
000064
000065     12  PM-CONTROL-BY-SSN.
000066         16  PM-COMPANY-CD-A3           PIC X.
000067         16  PM-SOC-SEC-NO.
000068             20  PM-SSN-STATE           PIC XX.
000069             20  PM-SSN-PRODUCER        PIC X(6).
000070             20  PM-SSN-LN3.
000071                 25  PM-INSURED-INITIALS-A3.
000072                     30 PM-INSURED-INITIAL1-A3 PIC X.
000073                     30 PM-INSURED-INITIAL2-A3 PIC X.
000074                 25 PM-PART-LAST-NAME-A3      PIC X.
000075         16  PM-DATE-A3                  PIC XX.
000076         16  PM-TIME-A3                  PIC S9(04)   COMP.
000077
000078******************************************************************
000079*       ALTERNATE PATH4 = MPPLCY4 (BY REFRENCE)   RKP=60,LEN=25  *
000080******************************************************************
000081
000082     12  PM-CONTROL-BY-POLICY-NO.
000083         16  PM-COMPANY-CD-A4           PIC X.
000084         16  PM-POLICY-NO-A4.
000085             20  PM-POLICY-PRIME-A4     PIC X(18).
000086             20  PM-POLICY-SFX-A4       PIC XX.
000087         16  PM-DATE-A4                 PIC XX.
000088         16  PM-TIME-A4                 PIC S9(04)   COMP.
000089
000090******************************************************************
000091*       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT NO) RKP=85,LEN=27  *
000092******************************************************************
000093
000094     12  PM-CONTROL-BY-ACCOUNT.
000095         16  PM-COMPANY-CD-A5           PIC X.
000096         16  PM-BANK-ACCOUNT-NUMBER     PIC X(20).
000097         16  PM-DATE-A5                 PIC XX.
000098         16  PM-TIME-A5                 PIC S9(07)   COMP.
000099
000100******************************************************************
000101*       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT NO) RKP=112,LEN=15 *
000102******************************************************************
000103
000104     12  PM-CONTROL-BY-TRANSIT.
000105         16  PM-COMPANY-CD-A6           PIC X.
000106         16  PM-BANK-TRANSIT-NUMBER.
000107             20  PM-FEDERAL-NUMBER      PIC X(4).
000108             20  PM-BANK-NUMBER         PIC X(4).
000109         16  PM-DATE-A6                 PIC XX.
000110         16  PM-TIME-A6                 PIC S9(07)   COMP.
000111
000112******************************************************************
000113*       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO)    RKP=127,LEN=27 *
000114******************************************************************
000115
000116     12  PM-CONTROL-BY-LOAN-NO.
000117         16  PM-COMPANY-CD-A7           PIC X.
000118         16  PM-LOAN-NUMBER             PIC X(20).
000119         16  PM-DATE-A7                 PIC XX.
000120         16  PM-TIME-A7                 PIC S9(07)   COMP.
000121
000122******************************************************************
000123*                 FILE SYNCHRONIZATION DATA                      *
000124******************************************************************
000125
000126     12  FILLER                            PIC X(05).
000127     12  PM-FILE-SYNCH-DATA.
000128         16  PM-LAST-CHANGE-DT          PIC XX.
000129         16  PM-LAST-CHANGE-TIME        PIC S9(7)    COMP.
000130         16  PM-LAST-CHANGE-PROCESSOR   PIC X(4).
000131     12  FILLER                            PIC X(05).
000132
000133******************************************************************
000134*                    INSUREDS PROFILE DATA                       *
000135******************************************************************
000136
000137     12  PM-INSURED-PROFILE-DATA.
000138         16  PM-INSURED-NAME.
000139             20  PM-INSURED-LAST-NAME  PIC X(15).
000140             20  PM-INSURED-FIRST-NAME.
000141                 24  PM-INSURED-1ST-INIT PIC X.
000142                 24  FILLER               PIC X(9).
000143             20  PM-INSURED-MIDDLE-INIT PIC X.
000144         16  PM-INSURED-ADDRESS.
000145             20  PM-ADDRESS-LINE-1      PIC X(30).
000146             20  PM-ADDRESS-LINE-2      PIC X(30).
000147             20  PM-CITY                PIC X(25).
000148             20  PM-RESIDENT-STATE      PIC XX.
000149             20  PM-ZIP-CD.
000150                 24  PM-ZIP-FIRST-FIVE  PIC X(5).
000151                 24  PM-ZIP-PLUS-FOUR   PIC X(4).
000152         16  PM-INSURED-PERSONAL.
000153             20  PM-INSURED-OCC-CLASS   PIC X.
000154                 88  PM-PREFERRED         VALUE '1'.
000155                 88  PM-STANDARD          VALUE '2'.
000156                 88  PM-HAZARDOUS         VALUE '3'.
000157                 88  PM-VERY-HAZARDOUS    VALUE '4'.
000158                 88  PM-EXTREME-HAZARDOUS VALUE '5'.
000159                 88  PM-NOT-OCC           VALUE '6'.
000160                 88  PM-OCC-UNKNOWN       VALUE '9'.
000161             20  PM-INSURED-OCC-CD      PIC X(3).
000162             20  PM-INSURED-OCC-CD-NUM REDEFINES
000163                 PM-INSURED-OCC-CD      PIC 9(3).
000164             20  PM-INSURED-SEX         PIC X.
000165                 88  PM-INSURED-SEX-MALE   VALUE 'M'.
000166                 88  PM-INSURED-SEX-FEMALE VALUE 'F'.
000167             20  PM-INSURED-BIRTH-DT    PIC XX.
000168             20  PM-INSURED-ISSUE-AGE   PIC S9(3)     COMP-3.
000169             20  PM-INSURED-HEIGHT-FT   PIC S9(3)     COMP-3.
000170             20  PM-INSURED-HEIGHT-IN   PIC S9(3)     COMP-3.
000171             20  PM-INSURED-WEIGHT      PIC S9(3)     COMP-3.
000172             20  PM-INSURED-BIRTH-STATE PIC XX.
000173             20  PM-INSURED-PHONE-NO    PIC X(13).
000174             20  PM-INSURED-RATED-AGE   PIC S9(3)     COMP-3.
000175         16  PM-INS-LANGUAGE-IND        PIC X(01).
000176             88  PM-ENGLISH                        VALUE 'E'.
000177             88  PM-FRENCH                         VALUE 'F'.
000178             88  PM-SPANISH                        VALUE 'S'.
000179         16  PM-INSURED-TOT-BENEFIT     PIC S9(7)V99  COMP-3.
000180
000181         16  PM-INSURED-AGE-IND         PIC X(01).
000182             88  PM-INSURED-AGE-75-REACHED         VALUE 'Y'.
000183     12  FILLER                            PIC X(13).
000184
000185******************************************************************
000186*                JOINT INSUREDS PROFILE DATA                     *
000187******************************************************************
000188
000189     12  PM-JOINT-PROFILE-DATA.
000190         16  PM-JOINT-NAME.
000191             20  PM-JOINT-LAST-NAME     PIC X(15).
000192             20  PM-JOINT-FIRST-NAME.
000193                 24  PM-JOINT-1ST-INIT  PIC X.
000194                 24  FILLER                PIC X(9).
000195             20  PM-JOINT-MIDDLE-INIT   PIC X.
000196         16  PM-JOINT-SOC-SEC-NO.
000197             20  PM-JT-SSN-STATE        PIC XX.
000198             20  PM-JT-SSN-PRODUCER     PIC X(6).
000199             20  PM-JT-SSN-LN3.
000200                 25  PM-JT-INSURED-INITIALS-A3.
000201                     30 PM-JT-INSURED-INITIAL1-A3 PIC X.
000202                     30 PM-JT-INSURED-INITIAL2-A3 PIC X.
000203                 25 PM-JT-PART-LAST-NAME-A3     PIC X.
000204         16  PM-JOINT-PERSONAL.
000205             20  PM-JOINT-OCC-CLASS     PIC X.
000206                 88 PM-JNT-PREFERRED       VALUE '1'.
000207                 88 PM-JNT-STANDARD        VALUE '2'.
000208                 88 PM-JNT-HAZARDOUS       VALUE '3'.
000209                 88 PM-JNT-VERY-HAZARDOUS  VALUE '4'.
000210                 88 PM-JNT-EXTREME-HAZARDOUS VALUE '5'.
000211                 88 PM-JNT-NOT-OCC         VALUE '6'.
000212                 88 PM-JNT-OCC-UNKNOWN     VALUE '9'.
000213             20  PM-JOINT-OCC-CD        PIC X(3).
000214             20  PM-JOINT-SEX           PIC X.
000215                 88  PM-JOINT-SEX-MALE     VALUE 'M'.
000216                 88  PM-JOINT-SEX-FEMALE   VALUE 'F'.
000217             20  PM-JOINT-BIRTH-DT      PIC XX.
000218             20  PM-JOINT-ISSUE-AGE     PIC S9(3)     COMP-3.
000219             20  PM-JOINT-HEIGHT-FT     PIC S9(3)     COMP-3.
000220             20  PM-JOINT-HEIGHT-IN     PIC S9(3)     COMP-3.
000221             20  PM-JOINT-WEIGHT        PIC S9(3)     COMP-3.
000222             20  PM-JOINT-BIRTH-STATE   PIC XX.
000223             20  PM-JOINT-RATED-AGE     PIC S9(3)     COMP-3.
000224         16  PM-JOINT-TOT-BENEFIT       PIC S9(7)V99  COMP-3.
000225         16  PM-JOINT-AGE-IND           PIC X(01).
000226             88  PM-JOINT-AGE-75-REACHED           VALUE 'Y'.
000227
000228     12  FILLER                            PIC X(12).
000229
000230******************************************************************
000231*                  INSURANCE COVERAGE DATA                       *
000232******************************************************************
000233
000234     12  PM-INS-COVERAGE-DATA.
000235         16  PM-FREE-PERIOD             PIC S9(03)    COMP-3.
000236         16  PM-LOAN-TERM               PIC S9(3)     COMP-3.
000237         16  PM-LOAN-APR                PIC S9V9999   COMP-3.
000238         16  PM-LOAN-DT                 PIC XX.
000239         16  PM-LOAN-PYMT               PIC S9(5)V99  COMP-3.
000240         16  PM-LOAN-BALC               PIC S9(7)V99  COMP-3.
000241         16  PM-INS-BENEFIT-MONTHS      PIC S9(3)     COMP-3.
000242         16  PM-INS-MONTH-BENEFIT       PIC S9(7)V99  COMP-3.
000243         16  PM-INS-TOTAL-BENEFIT       PIC S9(7)V99  COMP-3.
000244         16  PM-INS-PLAN-TYPE           PIC X.
000245             88  PM-AH-MORT-PLAN           VALUE 'A'.
000246             88  PM-AD-D-MORT-PLAN         VALUE 'E'.
000247             88  PM-DISMEM-MORT-PLAN       VALUE 'D'.
000248             88  PM-LIFE-MORT-PLAN         VALUE 'L'.
000249         16  PM-INS-PLAN-CD             PIC XX.
000250         16  PM-INS-PLAN-REVISION       PIC X(3).
000251         16  PM-INS-POLICY-FORM         PIC X(12).
000252         16  PM-INS-MSTR-POLICY.
000253             20  PM-FREE-TYPE           PIC X(04).
000254             20  FILLER                    PIC X(08).
000255         16  PM-INS-MSTR-APP.
000256             20  FILLER                    PIC X(11).
000257             20  PM-INS-B-C-TYPE        PIC X(01).
000258         16  PM-INS-RATE-CD             PIC X(5).
000259         16  PM-INS-SEX-RATING          PIC X.
000260             88  PM-NOT-SEX-RATED           VALUE '1'.
000261             88  PM-SEX-RATED               VALUE '2'.
000262         16  PM-INS-SUBSTANDARD-PCT     PIC S9V9999   COMP-3.
000263         16  PM-INS-SUBSTANDARD-TYPE    PIC X.
000264         16  PM-INS-TERMINATION-DT      PIC XX.
000265         16  PM-INS-MONTH-PREMIUM   PIC S9(5)V999999  COMP-3.
000266         16  PM-INS-CALC-MO-PREM    PIC S9(5)V999999  COMP-3.
000267         16  PM-REINSURANCE-TABLE       PIC X(3).
000268         16  PM-MORTALITY-CD            PIC X(4).
000269         16  PM-INS-TYPE                PIC X.
000270             88  PM-INDIVIDUAL             VALUES ARE '1' 'I'.
000271             88  PM-GROUP                  VALUES ARE '2' 'G'.
000272         16  PM-LOAN-OFFICER            PIC X(5).
000273         16  PM-POLICY-FEE              PIC S9(3)V99 COMP-3.
000274         16  PM-DEPENDENT-COUNT         PIC S99      COMP-3.
000275         16  PM-CWA-AMOUNT              PIC S9(5)V99  COMP-3.
000276         16  PM-LAST-AUTO-RERATE-DT     PIC XX.
000277         16  PM-PREM-FINANCED-SW        PIC X.
000278             88  PM-PREM-FINANCED           VALUE 'Y'.
000279             88  PM-PREM-NOT-FINANCED       VALUE 'N'.
000280
000281         16  PM-INS-TERM-LETTER-IND     PIC X.
000282             88  PM-TERM-INITIALIZED        VALUE 'Y'.
000283         16  PM-INS-UNDERWRITER-MAX-BEN PIC S9(7)V99  COMP-3.
000284     12  FILLER                            PIC X(11).
000285
000286******************************************************************
000287*                    POLICY BILLING DATA                         *
000288******************************************************************
000289
000290     12  PM-BILLING-DATA.
000291         16  PM-BILLING-MODE            PIC X(1).
000292             88  PM-ANNUAL                 VALUE '1'.
000293             88  PM-SEMI-ANNUAL            VALUE '2'.
000294             88  PM-QUARTERLY              VALUE '3'.
000295             88  PM-MONTHLY                VALUE '4'.
000296             88  PM-BI-MONTHLY             VALUE '5'.
000297             88  PM-SINGLE-PREM            VALUE '6'.
000298         16  PM-BILLING-SCHEDULE        PIC X(1).
000299         16  PM-BILLING-SW              PIC X(1).
000300             88  PM-FIRST-BILLING          VALUE 'Y'.
000301             88  PM-PAID-IN-ADVANCE        VALUE 'A'.
000302             88  PM-POLICY-FEE-REFUNDED    VALUE 'F'.
000303         16  PM-BILLING-TYPE            PIC X(1).
000304             88  PM-LIST-BILL              VALUE '1'.
000305             88  PM-TAPE-BILL              VALUE '2'.
000306             88  PM-TAPE-LIST-BILL         VALUE '3'.
000307             88  PM-GROUP-BILL       VALUE ARE '1' '2' '3'.
000308             88  PM-DIRECT-BILL            VALUE '4'.
000309             88  PM-PAC-BILL         VALUE ARE '5' 'C' 'S'.
000310             88  PM-CHARGE-CARD-BILL       VALUE '6'.
000311             88  PM-INDIV-BILL
000312                                  VALUE ARE '4' '5' '6' 'C' 'S'.
000313             88  PM-GRP-PLCY-BILL          VALUE '7'.
000314             88  PM-GRP-PLCY-PAC           VALUE '8'.
000315             88  PM-GRP-PLCY-CR-CRD        VALUE '9'.
000316             88  PM-GRP-PLCY         VALUE ARE '7' '8' '9'.
000317             88  PM-GRP-PROD               VALUE 'A'.
000318             88  PM-EFT-CHECKING           VALUE 'C'.
000319             88  PM-EFT-SAVINGS            VALUE 'S'.
000320         16  PM-PAYMENT-AMT             PIC S9(5)V99  COMP-3.
000321         16  PM-OVER-SHORT-AMT          PIC S9(5)V99  COMP-3.
000322         16  PM-LAST-BILL-DT            PIC XX.
000323         16  PM-LAST-BILL-AMT           PIC S9(5)V99  COMP-3.
000324         16  PM-BILL-TO-DT              PIC XX.
000325         16  PM-LAST-PYMT-DT            PIC XX.
000326         16  PM-PAID-TO-DT              PIC XX.
000327         16  PM-PYMT-INVOICE-NUMBER     PIC X(6).
000328         16  PM-MONTHS-PAID             PIC S9(3)     COMP-3.
000329         16  PM-TOTAL-PREM-RECVD        PIC S9(7)V99  COMP-3.
000330         16  PM-BILLING-GROUPING-CODE   PIC X(6).
000331         16  PM-CHARGE-CARD-EXP-DT      PIC X(2).
000332         16  PM-CHARGE-CARD-TYPE        PIC X(2).
000333             88  PM-VISA                   VALUE 'VI'.
000334             88  PM-MSTR-CARD              VALUE 'MC'.
000335             88  PM-DINERS-CLUB            VALUE 'DN'.
000336             88  PM-DISCOVER               VALUE 'DS'.
000337             88  PM-CARTE-BLANCHE          VALUE 'CB'.
000338             88  PM-AMERICAN-EXPRESS       VALUE 'AE'.
000339         16  PM-BILL-INVOICE-NUMBER     PIC X(6).
000340         16  PM-BILL-DAY                PIC S99       COMP-3.
000341         16  PM-RES-PREM-TAX        PIC S9(3)V999999  COMP-3.
000342     12  FILLER                            PIC X(15).
000343
000344******************************************************************
000345*                     CLAIM PAYMENT DATA                         *
000346******************************************************************
000347
000348     12  PM-CLAIM-PAYMENT-DATA.
000349         16  PM-CLAIM-BENEFICIARY-NAME  PIC X(25).
000350         16  PM-CLAIM-INTERFACE-SW      PIC X.
000351             88  PM-NO-CLAIM-ATTACHED      VALUE SPACE.
000352             88  PM-POLICY-AND-CLAIM-ONLINE VALUE '1'.
000353             88  PM-POLICY-CREATED-FOR-CLAIM VALUE '2'.
000354             88  PM-CLAIM-CLOSED           VALUE '3'.
000355             88  PM-ACTIVE-CLAIM           VALUE '1' '2'.
000356             88  PM-CLAIM-ATTACHED         VALUE '1' '2' '3'.
000357         16  PM-CLAIM-INCURRED-DT       PIC XX.
000358         16  PM-CLAIM-PAID-TO-DT        PIC XX.
000359         16  PM-CLAIM-PAYMENT-CNT       PIC S9(3)     COMP-3.
000360         16  PM-CLAIM-LAST-PAYMENT-AMT  PIC S9(7)V99  COMP-3.
000361         16  PM-CLAIM-EXPENSES-ITD      PIC S9(7)V99  COMP-3.
000362         16  PM-CLAIM-PAYMENTS-ITD      PIC S9(7)V99  COMP-3.
000363         16  PM-CLAIM-ACCUMULATOR       PIC S9(7)V99  COMP-3.
000364         16  PM-CLAIM-ATTACH-CNT        PIC S9(3)     COMP-3.
000365         16  PM-CLAIM-LIFE-ITD          PIC S9(7)V99  COMP-3.
000366         16  PM-CLAIM-AH-ITD            PIC S9(7)V99  COMP-3.
000367         16  PM-CLAIM-RIDER-ITD         PIC S9(7)V99  COMP-3.
000368
000369     12  FILLER                            PIC X(03).
000370
000371******************************************************************
000372*                POLICY STATUS AND DISPOSITION                   *
000373******************************************************************
000374
000375     12  PM-STATUS-DISPOSITION-DATA.
000376         16  PM-ISSUE-EOM-DT            PIC XX.
000377         16  PM-REPLACEMENT-SWITCH      PIC X.
000378         16  PM-APPL-SIGN-DT            PIC XX.
000379         16  PM-UNDERWRITER             PIC X(3).
000380         16  PM-ENTRY-PROCESSOR         PIC X(4).
000381         16  PM-ENTRY-STATUS            PIC X.
000382             88  PM-NORMAL                 VALUE '1'.
000383             88  PM-TAKE-OVER              VALUE '2'.
000384             88  PM-CONVERSION             VALUE '4'.
000385             88  PM-RE-ISSUE               VALUE '5'.
000386             88  PM-REINSURANCE-ONLY       VALUE '9'.
000387         16  PM-ENTRY-DT                PIC XX.
000388         16  PM-ENTRY-TIME              PIC S9(7) COMP-3.
000389         16  PM-EXIT-DT                 PIC XX.
000390         16  PM-CURRENT-STATUS          PIC X.
000391             88  PM-LAPSE                  VALUE '0'.
000392             88  PM-ACTIVE                 VALUE '1'.
000393             88  PM-PENDING-ISSUE          VALUE '2'.
000394             88  PM-DECLINED               VALUE '3'.
000395             88  PM-PENDING-CANCEL         VALUE '4'.
000396             88  PM-PENDING-ISSUE-ERROR    VALUE '5'.
000397             88  PM-CLAIM-APPLIED          VALUE '6'.
000398             88  PM-CANCEL                 VALUE '7'.
000399             88  PM-PENDING-UNWTR-REVW     VALUE '8'.
000400             88  PM-PENDING-CANCEL-ERROR   VALUE '9'.
000401             88  PM-CANCEL-TRANSFER        VALUE 'C'.
000402             88  PM-CLAIM-SETTLEMENT       VALUE 'F'.
000403             88  PM-TERMINATE              VALUE 'T'.
000404** NOTE TYPE 1 IS ANYTHING THAT IS OR HAS BEEN ACTIVE.  TYPE 2 IS
000405** EVERYTHING ELSE.  IF YOU ADD A STATUS ADD THE VALUE TO ONE OF
000406** THESE GROUPS.
000407             88  PM-TYPE-STAT-1
000408                     VALUES ARE '0' '1' '4' '6' '7' '9'
000409                                'C' 'F' 'T'.
000410             88  PM-TYPE-STAT-2
000411                     VALUES ARE '2' '3' '5' '8'.
000412             88  PM-BILLABLE-STATUS VALUES ARE '0' '1' '6'.
000413             88  PM-PENDING-STATUS
000414                                VALUES ARE '2' '4' '5' '8' '9'.
000415             88  PM-PENDING-ISSUE-STATUS
000416                                VALUES ARE '2' '5' '8'.
000417             88  PM-CANCEL-STATUS
000418                                VALUES ARE '4' '7' '9' 'C'.
000419         16  PM-CANCEL-CAUSE-CD         PIC X(3).
000420         16  PM-CANCEL-DT               PIC XX.
000421         16  PM-REFUND-AMT              PIC S9(5)V99  COMP-3.
000422         16  PM-CALC-REFUND-AMT         PIC S9(5)V99  COMP-3.
000423         16  PM-DECLINE-CD              PIC X(3).
000424         16  PM-DECLINE-DT              PIC XX.
000425         16  PM-LAST-LAPSE-DT           PIC XX.
000426         16  PM-LAST-REINSTATE-DT       PIC XX.
000427         16  PM-SECURITY-ACCESS-CODE    PIC X.
000428         16  PM-PREV-CONTROL-PRIMARY.
000429             20  PM-PREV-COMPANY-CD          PIC X.
000430             20  PM-PREV-CARRIER             PIC X.
000431             20  PM-PREV-GROUPING.
000432                 24  PM-PREV-GROUPING-PREFIX PIC X(3).
000433                 24  PM-PREV-GROUPING-PRIME  PIC X(3).
000434             20  PM-PREV-STATE               PIC XX.
000435             20  PM-PREV-PRODUCER.
000436                 24  PM-PREV-PRODUCER-PREFIX PIC X(4).
000437                 24  PM-PREV-PRODUCER-PRIME  PIC X(6).
000438             20  PM-PREV-POLICY-EFF-DT       PIC XX.
000439             20  PM-PREV-REFERENCE-NUMBER.
000440                 24  PM-PREV-REFNO-PRIME     PIC X(18).
000441                 24  PM-PREV-REFNO-SFX       PIC XX.
000442         16  PM-ACTION-DT               PIC XX.
000443         16  PM-ACTION-CODE             PIC X(3).
000444         16  PM-ACTION-DT-2             PIC XX.
000445         16  PM-ACTION-CODE-2           PIC X(3).
000446         16  PM-ACTION-DT-3             PIC XX.
000447         16  PM-ACTION-CODE-3           PIC X(3).
000448         16  PM-ACTION-DT-4             PIC XX.
000449         16  PM-ACTION-CODE-4           PIC X(3).
000450         16  PM-ACTION-DT-5             PIC XX.
000451         16  PM-ACTION-CODE-5           PIC X(3).
000452
000453         16  PM-KEY-CHANGE              PIC X.
000454                 88  PM-NO-KEY-CHG   VALUES ARE ' ' 'N'.
000455                 88  PM-KEY-CHG           VALUE 'Y'.
000456         16  PM-KEY-CHANGE-DT           PIC XX.
000457
000458         16  PM-RTI-INDICATOR           PIC X.
000459         16  PM-REASON-CODE             PIC X(3).
000460         16  PM-IN-OUT-PROCESSING-IND   PIC X(1).
000461             88  PM-IN-OUT-PROCESSING   VALUE 'Y'.
000462             88  PM-NOT-IN-OUT-PROCESSING
000463                                           VALUE SPACES.
000464
000465     12  FILLER                            PIC X(12).
000466
000467******************************************************************
000468*                 AGENT AND COMMISSION DATA                      *
000469******************************************************************
000470
000471     12  PM-COMMISSION-DATA.
000472         16  PM-REMIT-TO                PIC S9(3) COMP-3.
000473         16  PM-COMM-CHANGE-SW          PIC X.
000474                 88  PM-COMMISSION-CHANGE  VALUE 'Y'.
000475         16  PM-AGENT-INFORMATION OCCURS  5 TIMES.
000476             20  PM-AGENT-NUMBER        PIC X(10).
000477             20  PM-AGENT-TYPE          PIC X.
000478                 88  PM-PRODUCER-LEVEL-AGENT
000479                                              VALUES ARE 'C' 'D'.
000480                 88  PM-AGENT-GROSS        VALUE 'C'.
000481                 88  PM-AGENT-REINS        VALUE 'R'.
000482                 88  PM-AGENT-GROSS-REINS  VALUE 'D'.
000483                 88  PM-OVERWRITE-GROSS    VALUE 'O'.
000484                 88  PM-OVERWRITE-GROSS-REINS VALUE 'P'.
000485                 88  PM-OVERWRITE-REINS    VALUE 'T'.
000486                 88  PM-REINS-ONLY         VALUE 'W'.
000487             20  PM-COMMISSION-BILL-PAID PIC X(1).
000488                 88  PM-GENERATE-BILL      VALUE 'B'.
000489                 88  PM-GENERATE-PAID      VALUE 'P'.
000490             20  PM-AGENT-COMP-1ST-YEAR PIC S99V999.
000491             20  PM-COMP-1ST-YEAR-TYPE  PIC X(1).
000492                 88  PM-COMP-1ST-YEAR-PERCENT VALUE '1'.
000493                 88  PM-COMP-1ST-YEAR-DOLLARS VALUE '2'.
000494                 88  PM-COMP-1ST-YEAR-NOT-USED VALUE '3'.
000495             20  PM-RENEWAL-DATA.
000496                 24  PM-AGENT-RENEWAL-DATA OCCURS 6 TIMES.
000497                     28  PM-RENEW-MONTHS  PIC S999    COMP-3.
000498                     28  PM-RENEW-COMMISSION
000499                                             PIC S99V999 COMP-3.
000500                     28  PM-RENEW-TYPE    PIC X(1).
000501                         88  PM-COMP-RENEW-PERCENT   VALUE '1'.
000502                         88  PM-COMP-RENEW-DOLLARS   VALUE '2'.
000503                         88  PM-COMP-RENEW-NOT-USED  VALUE '3'.
000504             20  PM-COMP-RECALC-FLAG    PIC X(1).
000505                 88  PM-BYPASS-RECALC      VALUE 'N'.
000506     12  FILLER                            PIC X(20).
000507******************************************************************
000508*             CUSTOMER DATA                                      *
000509******************************************************************
000510     12  PM-CUSTOMER-ID                 PIC X(20).
000511******************************************************************
000512     12  FILLER                            PIC X(43).
000513******************************************************************
      *<<((file: MTCPLCY))
000727     EJECT
000728*                COPY MTCPLAN REPLACING ==:TAG:== BY ==PP==.
      *>>((file: MTCPLAN))
000001******************************************************************
000002*                                                                *
000003*                            MTCPLAN                             *
000004*                            VMOD=1.001                          *
000005*                                                                *
000006*   MORTGAGE SYSTEM PRODUCER PLAN MASTER FILE.                   *
000007*                                                                *
000008*   THIS COPYBOOK IS USED FOR THE ONLINE                         *
000009*   PLAN CODE MASTER FILE.                                       *
000010*                                                                *
000011*   FILE DESCRIPTION = PRODUCER PLAN MASTER                      *
000012*                                                                *
000013*   FILE TYPE = VSAM,KSDS                                        *
000014*   RECORD SIZE = 450  RECFORM = FIX                             *
000015*                                                                *
000016*   BASE CLUSTER NAME = MPPLAN                    RKP=2,LEN=25   *
000017*       ALTERNATE PATH1 = MPPLAN2 (ALT GROUPING) RKP=47,LEN=25   *
000018*                                                                *
000019*   LOG = NO                                                     *
000020*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000021*                                                                *
000022*                                                                *
000023******************************************************************
000024
000025 01  PRODUCER-PLANS.
000026     12  PP-RECORD-ID                   PIC  X(02).
000027         88  VALID-PP-ID                      VALUE 'PP'.
000028
000029******************************************************************
000030*   BASE CLUSTER NAME = MPPLAN                    RKP=2,LEN=25   *
000031******************************************************************
000032
000033     12  PP-CONTROL-PRIMARY.
000034         16  PP-PROD-PRIMARY.
000035             20  PP-COMPANY-CD          PIC  X(01).
000036             20  PP-CONTROL-A.
000037                 24  PP-CARRIER         PIC  X(01).
000038                 24  PP-GROUPING.
000039                     28  PP-GROUPING-PREFIX
000040                                           PIC  X(03).
000041                     28  PP-GROUPING-PRIME PIC X(03).
000042                 24  PP-STATE           PIC  X(02).
000043                 24  PP-PRODUCER.
000044                     28  PP-PRODUCER-PREFIX
000045                                           PIC  X(04).
000046                     28  PP-PRODUCER-PRIME PIC X(06).
000047         16  PP-PRODUCER-PLAN.
000048             20  PP-PLAN-CODE           PIC  X(02).
000049             20  PP-PLAN-REVISION       PIC  9(03).
000050     12  FILLER                            PIC  X(20).
000051
000052******************************************************************
000053*      ALTERNATE PATH1 = MPPLAN2 (ALT GROUPING) RKP=47,LEN=25    *
000054******************************************************************
000055
000056     12  PP-CONTROL-BY-VAR-GRP.
000057         16  PP-COMPANY-CD-A1           PIC  X(01).
000058         16  PP-VG-CARRIER              PIC  X(01).
000059         16  PP-VG-GROUPING             PIC  X(06).
000060         16  PP-VG-STATE                PIC  X(02).
000061         16  PP-VG-PRODUCER             PIC  X(10).
000062         16  PP-VG-PLAN-CODE            PIC  X(02).
000063         16  PP-VG-PLAN-REVISION        PIC  X(03).
000064     12  FILLER                            PIC  X(20).
000065
000066******************************************************************
000067*                PRODUCER SECURITY DATA                          *
000068******************************************************************
000069
000070     12  PP-SECURITY-ACCESS-CODE        PIC  X(01).
000071     12  PP-POLICY-CNT                  PIC S9(07)    COMP-3.
000072
000073******************************************************************
000074*                FILE SYNCHRONIZATION DATA                       *
000075******************************************************************
000076
000077     12  PP-MAINT-INFORMATION.
000078         16  PP-LAST-MAINT-DATE         PIC  X(02).
000079         16  PP-LAST-MAINT-HHMMSS       PIC S9(07)    COMP-3.
000080         16  PP-LAST-MAINT-USER         PIC  X(04).
000081     12  FILLER                            PIC  X(10).
000082
000083******************************************************************
000084*                   CRITICAL FILE DATES                          *
000085******************************************************************
000086
000087     12  PP-PLAN-DATES.
000088         16  PP-PLAN-EFFECT-DATE        PIC  X(02).
000089         16  PP-PLAN-EXPIRE-DATE        PIC  X(02).
000090
000091     12  FILLER                            PIC  X(10).
000092
000093******************************************************************
000094*                GENERAL INFORMATION                             *
000095******************************************************************
000096
000097     12  PP-GENERAL-INFORMATION.
000098         16  PP-ALPHA-SEARCH-SW         PIC  X(01).
000099             88  PP-MIB-ALPHA-ALL           VALUE '1'.
000100             88  PP-MIB-ALPHA-NONE          VALUE '2'.
000101             88  PP-MIB-ALPHA-EXCEEDED      VALUE '3'.
000102             88  PP-CLIENT-ALPHA-ALL        VALUE 'A'.
000103             88  PP-CLIENT-ALPHA-NONE       VALUE 'B'.
000104             88  PP-CLIENT-ALPHA-EXCEEDED   VALUE 'C'.
000105             88  PP-BOTH-ALPHA-ALL          VALUE 'X'.
000106             88  PP-BOTH-ALPHA-NONE         VALUE 'Y'.
000107             88  PP-BOTH-ALPHA-EXCEEDED     VALUE 'Z'.
000108             88  PP-ALPHA-SEARCH-VALID VALUES ARE '1' '2' '3'
000109                                                     'A' 'B' 'C'
000110                                                     'X' 'Y' 'Z'.
000111         16  PP-BENEFIT-TYPE            PIC  X(01).
000112             88  PP-BENEFIT-IS-LEVEL         VALUE '1'.
000113             88  PP-BENEFIT-REDUCES          VALUE '2'.
000114         16  PP-DAYS-TO-1ST-NOTICE      PIC  9(02).
000115         16  PP-DAYS-TO-2ND-NOTICE      PIC  9(02).
000116         16  PP-DAYS-TO-3RD-NOTICE      PIC  9(02).
000117         16  PP-DAYS-TO-4TH-NOTICE      PIC  9(02).
000118         16  PP-EFF-DT-RULE-SW          PIC  X(01).
000119             88  PP-EFF-DT-ENTER            VALUE 'E'.
000120             88  PP-EFF-DT-MONTH            VALUE 'M'.
000121             88  PP-EFF-DT-QTR              VALUE 'Q'.
000122             88  PP-EFF-DT-SEMI             VALUE 'S'.
000123             88  PP-EFF-DT-ANN              VALUE 'A'.
000124         16  PP-FREE-EXAM-DAYS          PIC S9(03)   COMP-3.
000125         16  PP-GRACE-PERIOD            PIC S9(03)   COMP-3.
000126         16  PP-HEALTH-QUESTIONS        PIC  9(01).
000127         16  PP-NUMBER-LAPSE-NOTICES    PIC S9(03)   COMP-3.
000128         16  PP-MIB-SEARCH-SW           PIC  X(01).
000129             88  PP-MIB-SEARCH-ALL          VALUE '1'.
000130             88  PP-MIB-SEARCH-NONE         VALUE '2'.
000131             88  PP-MIB-SEARCH-EXCEEDED     VALUE '3'.
000132             88  PP-MIB-SEARCH-VALID   VALUES ARE '1' '2' '3'.
000133         16  PP-PLAN-ABBREV             PIC  X(03).
000134         16  PP-PLAN-AGES.
000135             20  PP-MINIMUM-AGE         PIC S9(03)   COMP-3.
000136             20  PP-MAXIMUM-AGE         PIC S9(03)   COMP-3.
000137             20  PP-MAXIMUM-ATTAIN-AGE  PIC S9(03)   COMP-3.
000138         16  PP-PLAN-BENEFITS.
000139             20  PP-CLAIM-CAP           PIC S9(07)V99 COMP-3.
000140             20  PP-MINIMUM-BENEFIT     PIC S9(07)V99 COMP-3.
000141             20  PP-MAXIMUM-BENEFIT     PIC S9(07)V99 COMP-3.
000142             20  PP-MAXIMUM-MONTHLY-BENEFIT
000143                                           PIC S9(07)V99 COMP-3.
000144         16  PP-PLAN-DESCRIPTION        PIC  X(10).
000145         16  PP-POLICY-FEE              PIC S9(03)V9(02)
000146                                                        COMP-3.
000147         16  PP-PLAN-IND-GRP            PIC  X(01).
000148         16  PP-PLAN-SNGL-JNT           PIC  X(01).
000149             88  PP-COMBINED-PLAN          VALUE 'C'.
000150             88  PP-JNT-PLAN               VALUE 'J'.
000151             88  PP-SNGL-PLAN              VALUE 'S'.
000152         16  PP-PLAN-TERMS.
000153             20  PP-MINIMUM-TERM        PIC S9(03)   COMP-3.
000154             20  PP-MAXIMUM-TERM        PIC S9(03)   COMP-3.
000155         16  PP-PLAN-TYPE               PIC  X(01).
000156             88  PP-AH-MORT-PLAN           VALUE 'A'.
000157             88  PP-AD-D-MORT-PLAN         VALUE 'E'.
000158             88  PP-DISMEM-MORT-PLAN       VALUE 'D'.
000159             88  PP-LIFE-MORT-PLAN         VALUE 'L'.
000160         16  PP-PREMIUM-TOLERANCES.
000161             20  PP-PREM-TOLERANCE      PIC S9(03)   COMP-3.
000162             20  PP-PREM-TOLERANCE-PCT  PIC SV9(03)  COMP-3.
000163         16  PP-RATE-CODE               PIC  X(05).
000164         16  PP-REOCCURRING-DISABILITY-PRD PIC S9(03) COMP-3.
000165         16  PP-REPLACEMENT-LAW-SW      PIC  X(01).
000166             88  PP-NO-REPLACE             VALUE '1'.
000167             88  PP-REPLACE-APPLIES        VALUE '2'.
000168             88  PP-VALID-REPLACEMENT-LAW  VALUE '1' '2'.
000169         16  PP-RETRO-RETENTION         PIC S9V9(04) COMP-3.
000170         16  PP-RERATE-CNTL             PIC  X(01).
000171             88  PP-RERATE-WITH-ISSUE-AGE    VALUE '1'.
000172             88  PP-RERATE-WITH-CURRENT-AGE  VALUE '2'.
000173             88  PP-DO-NOT-RERATE            VALUE '3' ' '.
000174             88  PP-AUTO-RECALC              VALUE '4'.
000175         16  PP-SEX-RATING              PIC  X(01).
000176             88  PP-NOT-SEX-RATED          VALUE '1'.
000177             88  PP-SEX-RATED              VALUE '2'.
000178         16  PP-SUBSTANDARD-DATA.
000179             20  PP-SUBSTANDARD-PERCENT PIC S9(01)V9(04).
000180             20  PP-SUBSTANDARD-TYPE    PIC  X(01).
000181                 88  PP-PCT-OF-BENEFIT     VALUE '1'.
000182                 88  PP-PCT-OF-PREMIUM     VALUE '2'.
000183                 88  PP-NOT-APPLICABLE     VALUE '3'.
000184         16  PP-YEARS-TO-NEXT-RERATE    PIC  9(02).
000185         16  PP-DEPENDANT-COVERAGE      PIC  X(01).
000186             88  PP-DEP-COVERED            VALUE 'Y'.
000187             88  PP-DEP-NOT-COVERED        VALUE 'N' ' '.
000188         16  PP-REFUND-CALC             PIC  X(01).
000189             88  PP-RFND-MP-REFUND  VALUES ARE ' ' LOW-VALUES.
000190             88  PP-RFND-BY-R78            VALUE '1'.
000191             88  PP-RFND-BY-PRO-RATA       VALUE '2'.
000192             88  PP-RFND-AS-CALIF          VALUE '3'.
000193             88  PP-RFND-AS-TEXAS          VALUE '4'.
000194             88  PP-RFND-IS-NET-PAY        VALUE '5'.
000195             88  PP-RFND-ANTICIPATION      VALUE '6'.
000196             88  PP-RFND-MEAN              VALUE '8'.
000197             88  PP-VALID-REFUND    VALUES ARE ' ' '1' '2' '3'
000198                                                  '4' '5' '6' '8'
000199                                                  LOW-VALUES.
000200         16  PP-ALT-RATE-CODE           PIC  X(05).
000201
000202     12  FILLER                            PIC  X(39).
000203
000204******************************************************************
000205*                     PLAN FORMS AND LETTERS                     *
000206******************************************************************
000207
000208     12  PP-PLAN-MASTER-FORMS.
000209         16  PP-POLICY-FORM             PIC  X(12).
000210         16  PP-MASTER-APPLICATION      PIC  X(12).
000211         16  PP-MASTER-POLICY           PIC  X(12).
000212     12  PP-DELINQUENCY-NOTICE-FORMS.
000213         16  PP-1ST-NOTICE-FORM         PIC  X(04).
000214         16  PP-2ND-NOTICE-FORM         PIC  X(04).
000215         16  PP-3RD-NOTICE-FORM         PIC  X(04).
000216         16  PP-4TH-NOTICE-FORM         PIC  X(04).
000217     12  FILLER                            PIC  X(32).
000218     12  PP-TERMINATION-FORM            PIC  X(04).
000219     12  FILLER                            PIC  X(08).
000220     12  PP-ISSUE-LETTER                PIC  X(04).
000221
000222     12  FILLER                            PIC  X(80).
000223******************************************************************
      *<<((file: MTCPLAN))
000729     EJECT
000730*                COPY MTCPROD REPLACING ==:TAG:== BY ==PDM==.
      *>>((file: MTCPROD))
000001******************************************************************
000002*                                                                *
000003*                            MTCPROD                             *
000004*                            VMOD=1.001                          *
000005*                                                                *
000006*   MORTGAGE SYSTEM PRODUCER MASTER FILE                         *
000007*                                                                *
000008*   THIS COPYBOOK IS USED FOR THE ONLINE                         *
000009*   VSAM PRODUCER MASTER FILE.                                   *
000010*                                                                *
000011*   FILE DESCRIPTION = PRODUCER MASTER FILE                      *
000012*                                                                *
000013*   FILE TYPE = VSAM,KSDS                                        *
000014*   RECORD SIZE = 2000 RECFORM = FIXED                           *
000015*                                                                *
000016*   BASE CLUSTER NAME = MPPROD                    RKP=02,LEN=22  *
000017*       ALTERNATE PATH1 = MPPROD2 (ALT GROUPING)  RKP=48,LEN=22  *
000018*       ALTERNATE PATH2 = MPPROD3 (PRODUCER NAME) RKP=90,LEN=56  *
000019*                                                                *
000020*   LOG = NO                                                     *
000021*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000022*                                                                *
000023*                                                                *
000024******************************************************************
000025
000026 01  PRODUCER-MASTER.
000027     12  PDM-RECORD-ID              PIC  X(02).
000028         88  PDM-VALID-ID                VALUE 'PD'.
000029
000030******************************************************************
000031*   BASE CLUSTER NAME = MPPROD                    RKP=2,LEN=22   *
000032******************************************************************
000033
000034     12  PDM-CONTROL-PRIMARY-BATCH.
000035         16  FILLER                   PIC  X(20).
000036         16  PDM-EXPIRE-DT.
000037             20  PDM-EXPIRE-DT-YY   PIC  9(02).
000038             20  PDM-EXPIRE-DT-MM   PIC  9(02).
000039             20  PDM-EXPIRE-DT-DD   PIC  9(02).
000040     12  FILLER REDEFINES PDM-CONTROL-PRIMARY-BATCH.
000041         16  PDM-CONTROL-PRIMARY.
000042             20  PDM-COMPANY-CD     PIC  X(01).
000043             20  PDM-MSTR-CNTRL.
000044                 24  PDM-CONTROL-A.
000045                     28  PDM-CARRIER PIC X(01).
000046                     28  PDM-GROUPING.
000047                         32 PDM-GROUPING-PREFIX
000048                                      PIC  X(03).
000049                         32 PDM-GROUPING-PRIME
000050                                      PIC  X(03).
000051                     28  PDM-STATE  PIC  X(02).
000052                     28  PDM-PRODUCER.
000053                         32  PDM-PRODUCER-PREFIX
000054                                      PIC  X(04).
000055                         32  PDM-PRODUCER-PRIME
000056                                      PIC  X(06).
000057                 24  PDM-CNTRL-B.
000058                     28  PDM-EXPIRE-DATE
000059                                      PIC  X(02).
000060         16  FILLER REDEFINES PDM-CONTROL-PRIMARY.
000061             20  FILLER               PIC  X(01).
000062             20  PDM-CGSPE-KEY      PIC  X(21).
000063         16  FILLER                   PIC  X(04).
000064     12  FILLER                       PIC  X(20).
000065
000066******************************************************************
000067*      ALTERNATE PATH1 = MPPROD2 (ALT GROUPING) RKP=48,LEN=22    *
000068******************************************************************
000069
000070     12  PDM-CONTROL-BY-VAR-GRP.
000071         16  PDM-VG-CCGSP-KEYLET.
000072             20  PDM-COMPANY-CD-A1  PIC  X(01).
000073             20  PDM-VG-CARRIER     PIC  X(01).
000074             20  PDM-VG-GROUPING    PIC  X(06).
000075             20  PDM-VG-STATE       PIC  X(02).
000076             20  PDM-VG-PRODUCER    PIC  X(10).
000077         16  PDM-VG-DATE.
000078             24  PDM-VG-EXPIRE-DATE PIC  X(02).
000079     12  FILLER                       PIC  X(20).
000080
000081
000082******************************************************************
000083*      ALTERNATE PATH2 = MPPROD3 (NAME)         RKP=90,LEN=56    *
000084******************************************************************
000085
000086     12  PDM-CONTROL-BY-NAME.
000087         16  PDM-COMPANY-CD-A2      PIC  X(01).
000088         16  PDM-NAME-A2            PIC  X(30).
000089         16  PDM-CGSPE-KEY-A2.
000090             20  PDM-CARRIER-A2     PIC  X(01).
000091             20  PDM-GROUPING-A2    PIC  X(06).
000092             20  PDM-STATE-A2       PIC  X(02).
000093             20  PDM-PRODUCER-A2    PIC  X(10).
000094             20  PDM-EXPIRE-DATE-A2 PIC  X(02).
000095         16  PDM-CURRENT-DATE-BIN-A2 PIC X(02).
000096         16  PDM-CURRENT-TIME-BIN-A2 PIC S9(04) COMP.
000097     12  FILLER                       PIC  X(20).
000098
000099******************************************************************
000100*                FILE SYNCHRONIZATION DATA                       *
000101******************************************************************
000102
000103     12  PDM-MAINT-INFORMATION.
000104         16  PDM-LAST-MAINT-DATE    PIC  X(02).
000105         16  PDM-LAST-MAINT-HHMMSS  PIC S9(07) COMP-3.
000106         16  PDM-LAST-MAINT-USER    PIC  X(04).
000107
000108******************************************************************
000109*                PRODUCER SECURITY DATA                          *
000110******************************************************************
000111
000112     12  PDM-SECURITY-ACCESS-CODE   PIC  X(01).
000113
000114******************************************************************
000115*                DATES                                           *
000116******************************************************************
000117
000118     12  PDM-ANNIVERSARY-DATE       PIC  X(02).
000119
000120     12  PDM-AR-HI-DATE.
000121         16  PDM-AR-HI-POLICY-DATE  PIC  X(02).
000122         16  FILLER                   PIC  X(04).
000123     12  PDM-AR-HI-POLICY-DT REDEFINES PDM-AR-HI-DATE.
000124         16  PDM-AR-HI-POLICY-DT-YY PIC  9(02).
000125         16  PDM-AR-HI-POLICY-DT-MM PIC  9(02).
000126         16  PDM-AR-HI-POLICY-DT-DD PIC  9(02).
000127
000128     12  PDM-ENTRY-DATE             PIC  X(02).
000129
000130     12  PDM-EFFECT-DTE.
000131         16  PDM-EFFECT-DATE        PIC  X(02).
000132         16  FILLER                   PIC  X(04).
000133     12  PDM-EFFECT-DT REDEFINES PDM-EFFECT-DTE.
000134         16  PDM-EFFECT-DT-YY       PIC  9(02).
000135         16  PDM-EFFECT-DT-MM       PIC  9(02).
000136         16  PDM-EFFECT-DT-DD       PIC  9(02).
000137
000138     12  PDM-HI-DATE.
000139         16  PDM-HI-POLICY-DATE     PIC  X(02).
000140         16  FILLER                   PIC  X(04).
000141     12  PDM-HI-POLICY-DT REDEFINES PDM-HI-DATE.
000142         16  PDM-HI-POLICY-DT-YY    PIC  9(02).
000143         16  PDM-HI-POLICY-DT-MM    PIC  9(02).
000144         16  PDM-HI-POLICY-DT-DD    PIC  9(02).
000145
000146     12  PDM-INACTIVE-DATE          PIC  X(02).
000147
000148     12  PDM-LO-DATE.
000149         16  PDM-LO-POLICY-DATE     PIC  X(02).
000150         16  FILLER                   PIC  X(04).
000151     12  PDM-LO-POLICY-DT REDEFINES PDM-LO-DATE.
000152         16  PDM-LO-POLICY-DT-YY    PIC  9(02).
000153         16  PDM-LO-POLICY-DT-MM    PIC  9(02).
000154         16  PDM-LO-POLICY-DT-DD    PIC  9(02).
000155
000156     12  PDM-POLICIES-PURGED-DATE   PIC  X(02).
000157
000158     12  PDM-PREV-DATES.
000159         16  PDM-PREV-EFF-DATE      PIC  X(02).
000160         16  FILLER                   PIC  X(04).
000161         16  PDM-PREV-EXP-DATE      PIC  X(02).
000162         16  FILLER                   PIC  X(04).
000163     12  PDM-PREV-DTS REDEFINES PDM-PREV-DATES.
000164         16  PDM-PREV-EFF-DT.
000165             20  PDM-PREV-EFF-DT-YY PIC  9(02).
000166             20  PDM-PREV-EFF-DT-MM PIC  9(02).
000167             20  PDM-PREV-EFF-DT-DD PIC  9(02).
000168         16  PDM-PREV-EXP-DT.
000169             20  PDM-PREV-EXP-DT-YY PIC  9(02).
000170             20  PDM-PREV-EXP-DT-MM PIC  9(02).
000171             20  PDM-PREV-EXP-DT-DD PIC  9(02).
000172
000173     12  PDM-1ST-PROD-DATE          PIC  X(02).
000174
000175     12  FILLER                       PIC  X(20).
000176
000177******************************************************************
000178*                MORTGAGE BILLING DATA                           *
000179******************************************************************
000180
000181     12  PDM-CONTACT                PIC  X(30).
000182     12  PDM-BILLING-MONTHS.
000183         16  PDM-BILLING-MONTH-ANNUAL PIC 9(02).
000184         16  PDM-BILLING-MONTH-SEMIANN PIC 9(02).
000185     12  PDM-BILLING-ADVANCE-ARREARS PIC X(01).
000186         88  PDM-BILL-ADVANCE           VALUE '1'.
000187         88  PDM-BILL-ARREARS           VALUE '2'.
000188     12  PDM-BILLING-MODE           PIC  X(01).
000189         88  PDM-ANNUAL-BILL            VALUE '1'.
000190         88  PDM-SEMI-ANNUAL-BILL       VALUE '2'.
000191         88  PDM-QUARTERLY-BILL         VALUE '3'.
000192         88  PDM-MONTHLY-BILL           VALUE '4'.
000193         88  PDM-BI-MONTHLY-BILL        VALUE '5'.
000194         88  PDM-SINGLE-PREM-BILL       VALUE '6'.
000195     12  PDM-BILLING-GROUPING-CODE  PIC  X(06).
000196     12  PDM-BILLING-SCHEDULE       PIC  X(01).
000197         88  PDM-BILL-1ST-WEEK          VALUE '1'.
000198         88  PDM-BILL-2ND-WEEK          VALUE '2'.
000199         88  PDM-BILL-3RD-WEEK          VALUE '3'.
000200         88  PDM-BILL-4TH-WEEK          VALUE '4'.
000201         88  PDM-BILL-5TH-WEEK          VALUE '5'.
000202         88  PDM-HOLD-BILL              VALUE '6'.
000203         88  PDM-NO-BILL                VALUE '7'.
000204     12  PDM-BILLING-SEQUENCE       PIC  X(01).
000205         88  PDM-BILL-NAME-SEQU         VALUE '1'.
000206         88  PDM-BILL-LOAN-SEQU         VALUE '2'.
000207         88  PDM-BILL-PLCY-SEQU         VALUE '3'.
000208     12  PDM-BILLING-TYPE           PIC  X(01).
000209         88  PDM-LIST-BILL              VALUE '1'.
000210         88  PDM-TAPE-BILL              VALUE '2'.
000211         88  PDM-TAPE-LIST-BILL         VALUE '3'.
000212         88  PDM-GROUP-BILL         VALUES ARE '1' '2' '3'.
000213         88  PDM-DIRECT-BILL            VALUE '4'.
000214         88  PDM-PAC                VALUES ARE '5' 'C' 'S'.
000215         88  PDM-CREDIT-CARD            VALUE '6'.
000216         88  PDM-INDIV-BILL
000217                              VALUES ARE '4' '5' '6' 'C' 'S'.
000218         88  PDM-GROUP-BY-POLICY        VALUE '7'.
000219         88  PDM-GROUP-BY-POLICY-PAC    VALUE '8'.
000220         88  PDM-GROUP-BY-POLICY-CRDC   VALUE '9'.
000221         88  PDM-GROUP-BY-BILL          VALUE '7' '8' '9'.
000222         88  PDM-GROUP-BY-PROD          VALUE 'A'.
000223         88  PDM-EFT-CHECKING           VALUE 'C'.
000224         88  PDM-EFT-SAVINGS            VALUE 'S'.
000225     12  PDM-DATE-PAID              PIC  X(02).
000226     12  PDM-LAST-BILLING-DATE      PIC  X(02).
000227     12  PDM-LAST-BILL-TO-DATE      PIC  X(02).
000228     12  PDM-MAX-MONTHS-BILL        PIC S9(03)  COMP-3.
000229     12  PDM-PAID-TO-DATE           PIC  X(02).
000230     12  PDM-PREV-BILLING-DATE      PIC  X(02).
000231     12  PDM-PREV-BILL-TO-DATE      PIC  X(02).
000232
000233     12  FILLER                       PIC  X(20).
000234
000235******************************************************************
000236*                PERSONAL DATA                                   *
000237******************************************************************
000238
000239     12  PDM-ADDRS                  PIC  X(30).
000240     12  PDM-CITY                   PIC  X(30).
000241     12  PDM-CITY-CODE              PIC  X(04).
000242     12  PDM-COUNTY-CODE            PIC  X(03).
000243     12  PDM-NAME                   PIC  X(30).
000244     12  PDM-PARRISH-CODE           PIC  X(03).
000245     12  PDM-PERSON                 PIC  X(30).
000246     12  PDM-TEL-NO.
000247         16  PDM-AREA-CODE          PIC  9(03).
000248         16  PDM-TEL-PRE            PIC  9(03).
000249         16  PDM-TEL-NBR            PIC  9(04).
000250     12  PDM-ZIP.
000251         16  PDM-ZIP-PRIME          PIC  X(05).
000252         16  PDM-ZIP-PLUS4          PIC  X(04).
000253     12  PDM-LANGUAGE-IND           PIC  X(01).
000254         88  PDM-ENGLISH                       VALUE 'E'.
000255         88  PDM-FRENCH                        VALUE 'F'.
000256         88  PDM-SPANISH                       VALUE 'S'.
000257
000258     12  FILLER                       PIC  X(19).
000259
000260******************************************************************
000261*                REINSURANCE DATA                                *
000262******************************************************************
000263
000264     12  PDM-REINS-TBL-CODE         PIC  X(03).
000265     12  PDM-REIN-RECALC            PIC  X(01).
000266
000267     12  PDM-REI-AH-FEE             PIC S9(01)V9(04) COMP-3.
000268     12  PDM-REI-AH-PE              PIC  X(01).
000269     12  PDM-REI-AH-TAX             PIC S9(01)V9(04) COMP-3.
000270
000271     12  PDM-REI-GROUP-A            PIC  X(06).
000272     12  PDM-REI-GROUP-B            PIC  X(06).
000273
000274     12  PDM-REI-LF-FEE             PIC S9(01)V9(04) COMP-3.
000275     12  PDM-REI-LF-PE              PIC  X(01).
000276     12  PDM-REI-LF-TAX             PIC S9(01)V9(04) COMP-3.
000277
000278     12  PDM-REI-MORT               PIC  X(04).
000279     12  PDM-REI-PRT-OW             PIC  X(01).
000280     12  PDM-REI-PRT-ST             PIC  X(01).
000281
000282     12  PDM-REI-ADD-FEE            PIC S9(01)V9(04) COMP-3.
000283     12  PDM-REI-ADD-PE             PIC  X(01).
000284     12  PDM-REI-ADD-TAX            PIC S9(01)V9(04) COMP-3.
000285
000286     12  PDM-REI-DIS-FEE            PIC S9(01)V9(04) COMP-3.
000287     12  PDM-REI-DIS-PE             PIC  X(01).
000288     12  PDM-REI-DIS-TAX            PIC S9(01)V9(04) COMP-3.
000289
000290     12  FILLER                       PIC  X(10).
000291******************************************************************
000292*                RETRO DATA                                      *
000293******************************************************************
000294
000295     12  PDM-RET-AH                 PIC S9(01)V9(04) COMP-3.
000296     12  PDM-RET-GRP                PIC  X(06).
000297     12  PDM-RET-LF                 PIC S9(01)V9(04) COMP-3.
000298     12  PDM-RET-MIN-LOSS-A         PIC SV9(03)      COMP-3.
000299     12  PDM-RET-MIN-LOSS-L         PIC SV9(03)      COMP-3.
000300     12  PDM-RET-P-E                PIC  X(01).
000301     12  PDM-RET-ST-TAX-USE         PIC  X(01).
000302         88  PDM-CHARGE-ST-TAXES-ON-RETRO   VALUE 'Y' 'E' 'P'.
000303         88  PDM-TAXES-NOT-IN-RETRO         VALUE 'N' ' '.
000304     12  PDM-RET-Y-N                PIC  X(01).
000305     12  PDM-RET-ADD                PIC S9(01)V9(04) COMP-3.
000306     12  PDM-RET-MIN-LOSS-ADD       PIC SV9(03)      COMP-3.
000307     12  PDM-RET-DIS                PIC S9(01)V9(04) COMP-3.
000308     12  PDM-RET-MIN-LOSS-DIS       PIC SV9(03)      COMP-3.
000309
000310     12  FILLER                       PIC  X(10).
000311
000312******************************************************************
000313*                     MANAGEMENT OPTIONS                         *
000314******************************************************************
000315
000316     12  PDM-DEFAULT-UNWTR-CODE     PIC  X(03).
000317     12  PDM-LAPSE-NOTICE-CNTL      PIC  X(01).
000318     12  PDM-CORRESPONDENCE-CNTL    PIC  X(01).
000319     12  PDM-RETAIN-BILLING-DATA-MTHS PIC S9(03) COMP-3.
000320     12  PDM-RETAIN-CLAIM-DATA-MTHS PIC S9(03)  COMP-3.
000321     12  PDM-RETAIN-COMMISSION-MTHS PIC S9(03)  COMP-3.
000322     12  PDM-RETAIN-DELINQUENCY-MTHS PIC S9(03) COMP-3.
000323     12  PDM-RETAIN-INSD-PROFILE-MTHS PIC S9(03) COMP-3.
000324     12  PDM-RETAIN-INS-COVERAGE-MTHS PIC S9(03) COMP-3.
000325     12  PDM-RETAIN-STATUS-DISP-MTHS PIC S9(03) COMP-3.
000326     12  PDM-NUM-BILLING-CYCLES-RETAINED
000327                                      PIC S9(03)  COMP-3.
000328     12  PDM-RETAIN-UNDERWRITER-HST-MTHS
000329                                      PIC S9(03)  COMP-3.
000330
000331     12  FILLER                       PIC X(098).
000332
000333
000334******************************************************************
000335*                MISCELLANEOUS DATA                              *
000336******************************************************************
000337
000338     12  PDM-AH-RPT021-EXP-PCT      PIC S9(03)V9(04) COMP-3.
000339     12  PDM-AUTO-REFUND-SW         PIC  X(01).
000340         88  PDM-AUTO-REFUNDS-USED          VALUE 'Y'.
000341         88  PDM-AUTO-REFUNDS-NOT-USED      VALUE 'N' ' '.
000342     12  PDM-BUSINESS-TYPE          PIC  9(02).
000343     12  PDM-CAL-TABLE              PIC  X(02).
000344     12  PDM-COMMENTS.
000345         16  PDM-COMMENT-LINE       PIC  X(50)
000346                                           OCCURS 5 TIMES.
000347     12  PDM-EMPLOYER-STMT-USED     PIC  X(01).
000348     12  PDM-GROUPED-CHECKS-Y-N     PIC  X(01).
000349     12  PDM-IG                     PIC  X(01).
000350         88  PDM-HAS-INDIVIDUAL             VALUE 'I'
000351                                                    '1'.
000352         88  PDM-HAS-GROUP                  VALUE 'G'
000353                                                    '2'.
000354     12  PDM-LF-RPT021-EXP-PCT      PIC S9(03)V9(04) COMP-3.
000355     12  PDM-REPORT-CODE-1          PIC  X(10).
000356     12  PDM-REPORT-CODE-2          PIC  X(10).
000357     12  PDM-RPT045A-SWITCH         PIC  X(01).
000358         88  PDM-RPT045A-OFF             VALUE 'N'.
000359     12  PDM-SPECIAL-BILLING-FREQ   PIC  X(01).
000360         88  PDM-HAS-SPECIAL-BILL-FREQ      VALUE 'Y'.
000361         88  PDM-NO-SPECIAL-BILL-FREQ       VALUE 'N' ' '.
000362     12  PDM-STATUS                 PIC  X(01).
000363         88  PDM-STATUS-ACTIVE              VALUE '0'.
000364         88  PDM-STATUS-INACTIVE            VALUE '1'.
000365     12  PDM-STD-AH-TYPE            PIC  X(02).
000366     12  PDM-TAX-NUMBER             PIC  X(11).
000367     12  PDM-TOL-CLM                PIC S9(03)V9(02) COMP-3.
000368     12  PDM-USER-FIELDS.
000369         16  PDM-USER-FLD-1         PIC  X(02).
000370         16  PDM-USER-FLD-2         PIC  X(02).
000371         16  PDM-USER-FLD-3         PIC  X(02).
000372         16  PDM-USER-FLD-4         PIC  X(02).
000373         16  PDM-USER-FLD-5         PIC  X(02).
000374     12  PDM-USER-SELECT-OPTIONS.
000375         16  PDM-USER-SELECT-1      PIC  X(10).
000376         16  PDM-USER-SELECT-2      PIC  X(10).
000377         16  PDM-USER-SELECT-3      PIC  X(10).
000378         16  PDM-USER-SELECT-4      PIC  X(10).
000379         16  PDM-USER-SELECT-5      PIC  X(10).
000380     12  PDM-DIS-RPT021-EXP-PCT     PIC S9(03)V9(04) COMP-3.
000381     12  PDM-ADD-RPT021-EXP-PCT     PIC S9(03)V9(04) COMP-3.
000382     12  FILLER                       PIC  X(20).
000383
000384******************************************************************
000385*                CLIENT USE AREAS                                *
000386******************************************************************
000387
000388     12  PDM-CLIENT-USE-AREA-1      PIC  X(30).
000389     12  PDM-CLIENT-USE-AREA-2      PIC  X(30).
000390     12  PDM-CLIENT-USE-AREA-3      PIC  X(11).
000391     12  PDM-CLIENT-USE-AREA-4      PIC  X(30).
000392     12  PDM-CLIENT-USE-AREA-5      PIC  X(30).
000393     12  PDM-CLIENT-USE-AREA-6      PIC  X(11).
000394     12  PDM-CLIENT-USE-AREA-7      PIC  X(30).
000395     12  PDM-CLIENT-USE-AREA-8      PIC  X(30).
000396     12  PDM-CLIENT-USE-AREA-9      PIC  X(11).
000397
000398******************************************************************
000399*                TRANSFER DATA                                   *
000400******************************************************************
000401     12  PDM-TRANSFERRED-FROM.
000402         16  PDM-TRNFROM-CARRIER    PIC  X(01).
000403         16  PDM-TRNFROM-GROUPING.
000404             20  PDM-TRNFROM-GRP-PREFIX
000405                                      PIC  X(03).
000406             20  PDM-TRNFROM-GRP-PRIME PIC X(03).
000407         16  PDM-TRNFROM-STATE      PIC  X(02).
000408         16  PDM-TRNFROM-PRODUCER.
000409             20  PDM-TRNFROM-PROD-PREFIX
000410                                      PIC  X(04).
000411             20  PDM-TRNFROM-PROD-PRIME
000412                                      PIC  X(06).
000413         16  PDM-TRNFROM-DATE       PIC  X(02).
000414     12  PDM-TRANSFERRED-TO.
000415         16  PDM-TRNTO-CARRIER      PIC  X(01).
000416         16  PDM-TRNTO-GROUPING.
000417             20  PDM-TRNTO-GRP-PREFIX PIC X(03).
000418             20  PDM-TRNTO-GRP-PRIME PIC X(03).
000419         16  PDM-TRNTO-STATE        PIC  X(02).
000420         16  PDM-TRNTO-PRODUCER.
000421             20  PDM-TRNTO-PROD-PREFIX PIC X(04).
000422             20  PDM-TRNTO-PROD-PRIME PIC X(06).
000423         16  PDM-TRNTO-DATE         PIC  X(02).
000424     12  FILLER                       PIC  X(20).
000425
000426******************************************************************
000427*                MORTGAGE PLANS SOLD                             *
000428******************************************************************
000429
000430     12  PDM-PLANS-SOLD.
000431         16  PDM-PRODUCER-PLANS OCCURS 40 TIMES
000432                                INDEXED BY PDM-PLAN-NDX
000433                                           PDM-PLAN-NDX2.
000434             20  PDM-INDIVIDUAL-PLAN.
000435                 24  PDM-PLAN-CODE  PIC  X(02).
000436                 24  PDM-PLAN-REVISION PIC X(03).
000437             20  PDM-IBNR-PERCENT   PIC S9(01)V9(04) COMP-3.
000438     12  FILLER                       PIC  X(54).
000439
000440******************************************************************
000441*                 AGENT AND COMMISSION DATA                      *
000442******************************************************************
000443
000444     12  PDM-COMMISSION-INFORMATION.
000445         16  PDM-REMIT-TO           PIC S9(03)   COMP-3.
000446         16  PDM-RECALCULATION-SW   PIC  X(01).
000447             88  PDM-RECALC-DETAIL          VALUE 'Y'.
000448             88  PDM-RECALC-NO-DETAIL       VALUE 'I'.
000449             88  PDM-IGNORE-RECALC          VALUE 'N'.
000450             88  PDM-VALID-RECALCULATION-SW VALUE 'Y' 'I' 'N'.
000451         16  PDM-AGENT-DATA.
000452             20  PDM-AGENT-ENTRY    OCCURS 5 TIMES
000453                                    INDEXED BY PDM-AGENT-NDX
000454                                               PDM-AGENT-NDX2.
000455                 24  PDM-AGENT-NUMBER PIC X(10).
000456                 24  PDM-AGENT-TYPE PIC  X(01).
000457                     88  PDM-AGENT-TYPE-A   VALUE 'C' 'D'.
000458                     88  PDM-AGENT-TYPE-G   VALUE 'O' 'R'
000459                                                    'P' 'T'
000460                                                    'W'.
000461                     88  PDM-AGENT-GROSS    VALUE 'C'.
000462                     88  PDM-AGENT-REINS    VALUE 'R'.
000463                     88  PDM-AGENT-GROSS-REINS VALUE 'D'.
000464                     88  PDM-OVERWRITE-GROSS VALUE 'O'.
000465                     88  PDM-OVERWRITE-GROSS-REINS
000466                                          VALUE 'P'.
000467                     88  PDM-OVERWRITE-REINS VALUE 'T'.
000468                     88  PDM-REINS-ONLY     VALUE 'W'.
000469                     88  PDM-VALID-AGENT-TYPE VALUE 'C' 'R'
000470                                                'D' 'O' 'P'
000471                                                'T' 'W'.
000472                 24  PDM-COMMISSION-BILLED-PAID
000473                                      PIC  X(01).
000474                     88  PDM-AGENT-BILLED   VALUE 'B'.
000475                     88  PDM-AGENT-PAID     VALUE 'P'.
000476                 24  PDM-COMP-RECALC-FLAG
000477                                      PIC  X(01).
000478                     88  PDM-BYPASS-RECALC  VALUE 'N'.
000479                     88  PDM-VALID-RECALC-FLAG VALUE ' ' 'N'.
000480     12  FILLER                       PIC  X(55).
000481
000482******************************************************************
000483*                BANK DATA                                       *
000484******************************************************************
000485
000486     12  PDM-BANK-ACCOUNT-NUMBER    PIC  X(20).
000487     12  PDM-BANK-TRANSIT-NUMBER.
000488         16  PDM-FEDERAL-NUMBER     PIC  X(04).
000489         16  PDM-BANK-NUMBER        PIC  X(04).
000490     12  PDM-CHARGE-CARD-EXP-DT     PIC  X(02).
000491     12  PDM-CHARGE-CARD-TYPE       PIC  X(02).
000492         88  PDM-AMERICAN-EXPRESS              VALUE 'AE'.
000493         88  PDM-CARTE-BLANCHE                 VALUE 'CB'.
000494         88  PDM-DINERS-CLUB                   VALUE 'DN'.
000495         88  PDM-DISCOVER                      VALUE 'DS'.
000496         88  PDM-MASTER-CARD                   VALUE 'MC'.
000497         88  PDM-VISA                          VALUE 'VI'.
000498     12  PDM-SIGNATURE-NAME         PIC  X(25).
000499     12  PDM-AUTHORIZATION-SW       PIC  X(01).
000500******************************************************************
000501*                GENERIC FILLER                                  *
000502******************************************************************
000503
000504     12  FILLER                       PIC  X(66).
000505
000506******************************************************************
      *<<((file: MTCPROD))
000731     EJECT
000732*                            COPY ELCRETR.
      *>>((file: ELCRETR))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCRETR.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                          *
000007*                                                                *
000008*   FILE DESCRIPTION = CLAIM MASTER RETRIEVE FILE                *
000009*                                                                *
000010*   **** NOTE -- THIS FILE IS IDENTICAL TO CLAIM MASTER (ELMSTR) *
000011*   ****      ANY CHANGES TO THIS COPYBOOK OR ELCMSTR MUST BE    *
000012*   ****      DUPLICATED IN THE OTHER.                           *
000013*                                                                *
000014*   FILE TYPE = VSAM,KSDS                                        *
000015*   RECORD SIZE = 350  RECFORM = FIXED                           *
000016*                                                                *
000017*   BASE CLUSTER = ELRETR                         RKP=2,LEN=20   *
000018*       ALTERNATE PATH1 = ELRETR2 (BY NAME)       RKP=22,LEN=29  *
000019*       ALTERNATE PATH2 = ELRETR3 (BY SOC SEC NO) RKP=51,LEN=12  *
000020*       ALTERNATE PATH3 = ELRETR5 (BY CERT NO)    RKP=63,LEN=12  *
000021*       ALTERNATE PATH4 = ELRETR6 (BY CREDIT CARD NO)
000022*                                                 RKP=75,LEN=21  *
000023*                                                                *
000024*   LOG = YES                                                    *
000025*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000026******************************************************************
000027 01  RETRIEVE-MASTER.
000028     12  RL-RECORD-ID                PIC XX.
000029         88  VALID-RL-ID         VALUE 'RL'.
000030
000031     12  RL-CONTROL-PRIMARY.
000032         16  RL-COMPANY-CD           PIC X.
000033         16  RL-CARRIER              PIC X.
000034         16  RL-CLAIM-NO             PIC X(7).
000035         16  RL-CERT-NO.
000036             20  RL-CERT-PRIME       PIC X(10).
000037             20  RL-CERT-SFX         PIC X.
000038
000039     12  RL-CONTROL-BY-NAME.
000040         16  RL-COMPANY-CD-A1        PIC X.
000041         16  RL-INSURED-LAST-NAME    PIC X(15).
000042         16  RL-INSURED-NAME.
000043             20  RL-INSURED-1ST-NAME PIC X(12).
000044             20  RL-INSURED-MID-INIT PIC X.
000045
000046     12  RL-CONTROL-BY-SSN.
000047         16  RL-COMPANY-CD-A2        PIC X.
000048         16  RL-SOC-SEC-NO.
000049             20  RL-SSN-STATE        PIC XX.
000050             20  RL-SSN-ACCOUNT      PIC X(6).
000051             20  RL-SSN-LN3          PIC X(3).
000052
000053     12  RL-CONTROL-BY-CERT-NO.
000054         16  RL-COMPANY-CD-A4        PIC X.
000055         16  RL-CERT-NO-A4.
000056             20  RL-CERT-A4-PRIME    PIC X(10).
000057             20  RL-CERT-A4-SFX      PIC X.
000058
000059     12  RL-CONTROL-BY-CCN.
000060         16  RL-COMPANY-CD-A5        PIC X.
000061         16  RL-CCN-A5.
000062             20  RL-CCN-NO.
000063                 24  RL-CCN-PREFIX-A5 PIC X(4).
000064                 24  RL-CCN-PRIME-A5 PIC X(12).
000065             20  RL-CCN-FILLER-A5    PIC X(4).
000066
000067     12  RL-INSURED-PROFILE-DATA.
000068         16  RL-INSURED-BIRTH-DT     PIC XX.
000069         16  RL-INSURED-SEX-CD       PIC X.
000070             88  RL-INSURED-IS-MALE     VALUE 'M'.
000071             88  RL-INSURED-IS-FEMALE   VALUE 'F'.
000072             88  RL-INSURED-SEX-UNKNOWN VALUE ' '.
000073         16  RL-INSURED-OCC-CD       PIC X(6).
000074         16  FILLER                  PIC X(5).
000075
000076     12  RL-PROCESSING-INFO.
000077         16  RL-PROCESSOR-ID         PIC X(4).
000078         16  RL-CLAIM-STATUS         PIC X.
000079             88  RL-CLAIM-IS-OPEN       VALUE 'O'.
000080             88  RL-CLAIM-IS-CLOSED     VALUE 'C'.
000081         16  RL-CLAIM-TYPE           PIC X.
000082*            88  RL-AH-CLAIM            VALUE 'A'.
000083*            88  RL-LIFE-CLAIM          VALUE 'L'.
000084*            88  RL-PROPERTY-CLAIM      VALUE 'P'.
000085*            88  RL-UNEMPLOYMENT-CLAIM  VALUE 'U'.
000086         16  RL-CLAIM-PREM-TYPE      PIC X.
000087             88  RL-SINGLE-PREMIUM         VALUE '1'.
000088             88  RL-O-B-COVERAGE           VALUE '2'.
000089             88  RL-OPEN-END-COVERAGE      VALUE '3'.
000090         16  RL-INCURRED-DT          PIC XX.
000091         16  RL-REPORTED-DT          PIC XX.
000092         16  RL-FILE-ESTABLISH-DT    PIC XX.
000093         16  RL-EST-END-OF-DISAB-DT  PIC XX.
000094         16  RL-LAST-PMT-DT          PIC XX.
000095         16  RL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
000096         16  RL-PAID-THRU-DT         PIC XX.
000097         16  RL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
000098         16  RL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
000099         16  RL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
000100         16  RL-PMT-CALC-METHOD      PIC X.
000101             88  RL-360-DAY-YR          VALUE '1'.
000102             88  RL-365-DAY-YR          VALUE '2'.
000103             88  RL-FULL-MONTHS         VALUE '3'.
000104         16  RL-CAUSE-CD             PIC X(6).
000105
000106         16  RL-PRIME-CERT-NO.
000107             20  RL-PRIME-CERT-PRIME PIC X(10).
000108             20  RL-PRIME-CERT-SFX   PIC X.
000109
000110         16  RL-SYSTEM-IDENTIFIER    PIC XX.
000111             88  RL-CREDIT-CLAIM        VALUE 'CR'.
000112             88  RL-CONVENIENCE-CLAIM   VALUE 'CV'.
000113
000114         16  RL-MICROFILM-NO         PIC X(10).
000115         16  RL-PROG-FORM-TYPE       PIC X.
000116         16  RL-LAST-ADD-ON-DT       PIC XX.
000117
000118         16  RL-LAST-REOPEN-DT       PIC XX.
000119         16  RL-LAST-CLOSE-DT        PIC XX.
000120         16  RL-LAST-CLOSE-REASON    PIC X.
000121             88  RL-FINAL-PAID          VALUE '1'.
000122             88  RL-CLAIM-DENIED        VALUE '2'.
000123             88  RL-AUTO-CLOSE          VALUE '3'.
000124             88  RL-MANUAL-CLOSE        VALUE '4'.
000125         16  RL-ASSOC-CERT-SEQU      PIC S99.
000126         16  RL-ASSOC-CERT-TOTAL     PIC S99.
000127         16  RL-CLAIM-PAYMENT-STATUS PIC 9.
000128             88  RL-PAYMENT-IN-PREP     VALUE 1 THRU 9.
000129         16  FILLER                  PIC X(5).
000130
000131     12  RL-CERTIFICATE-DATA.
000132         16  RL-CERT-ORIGIN          PIC X.
000133             88  RL-CERT-WAS-ONLINE     VALUE '1'.
000134             88  RL-CERT-WAS-CREATED    VALUE '2'.
000135             88  RL-COVERAGE-WAS-ADDED  VALUE '3'.
000136         16  RL-CERT-KEY-DATA.
000137             20  RL-CERT-CARRIER     PIC X.
000138             20  RL-CERT-GROUPING    PIC X(6).
000139             20  RL-CERT-STATE       PIC XX.
000140             20  RL-CERT-ACCOUNT.
000141                 24  RL-CERT-ACCOUNT-PREFIX PIC X(4).
000142                 24  RL-CERT-ACCOUNT-PRIME  PIC X(6).
000143             20  RL-CERT-EFF-DT      PIC XX.
000144
000145     12  RL-STATUS-CONTROLS.
000146         16  RL-PRIORITY-CD          PIC X.
000147             88  RL-HIGHEST-PRIORITY    VALUE '9'.
000148         16  RL-SUPV-ATTN-CD         PIC X.
000149             88  RL-SUPV-NOT-REQUIRED   VALUE ' ' 'N'.
000150             88  RL-SUPV-IS-REQUIRED    VALUE 'Y'.
000151         16  RL-PURGED-DT            PIC XX.
000152         16  RL-RESTORED-DT          PIC XX.
000153         16  RL-NEXT-AUTO-PAY-DT     PIC XX.
000154         16  RL-NEXT-RESEND-DT       PIC XX.
000155         16  RL-NEXT-FOLLOWUP-DT     PIC XX.
000156         16  FILLER                  PIC XX.
000157         16  RL-LAST-MAINT-DT        PIC XX.
000158         16  RL-LAST-MAINT-USER      PIC X(4).
000159         16  RL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
000160         16  RL-LAST-MAINT-TYPE      PIC X.
000161             88  RL-CLAIM-SET-UP           VALUE ' '.
000162             88  RL-PAYMENT-MADE           VALUE '1'.
000163             88  RL-LETTER-SENT            VALUE '2'.
000164             88  RL-MASTER-WAS-ALTERED     VALUE '3'.
000165             88  RL-MASTER-WAS-RESTORED    VALUE '4'.
000166             88  RL-INCURRED-DATE-CHANGED  VALUE '5'.
000167             88  RL-FILE-CONVERTED         VALUE '6'.
000168         16  RL-RELATED-CLAIM-NO     PIC X(7).
000169         16  RL-HISTORY-ARCHIVE-DT   PIC XX.
000170         16  RL-BENEFICIARY          PIC X(10).
000171         16  RL-FILE-ESTABLISHED-BY  PIC X(4).
000172         16  FILLER                  PIC X(6).
000173
000174     12  RL-TRAILER-CONTROLS.
000175         16  RL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
000176             88  RL-1ST-TRL-AVAIL       VALUE +4095.
000177             88  RL-LAST-TRL-AVAIL      VALUE +100.
000178             88  RL-RESV-EXP-HIST-TRLR  VALUE +0.
000179         16  RL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
000180         16  FILLER                  PIC XX.
000181         16  RL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
000182         16  RL-ADDRESS-TRAILER-CNT.
000183             20  RL-INSURED-ADDR-CNT  PIC S9.
000184                 88  RL-NO-INSURED-AVAILABLE VALUE ZERO.
000185             20  RL-ACCOUNT-ADDR-CNT  PIC S9.
000186                 88  RL-ACCOUNT-IS-ONLINE    VALUE ZERO.
000187             20  RL-BENIF-ADDR-CNT    PIC S9.
000188                 88  RL-BENEFICIARY-IS-ONLINE VALUE ZERO.
000189             20  RL-EMPLOYER-ADDR-CNT PIC S9.
000190                 88  RL-NO-EMPLOY-AVAILABLE   VALUE ZERO.
000191             20  RL-DOCTOR-ADDR-CNT   PIC S9.
000192                 88  RL-NO-DOCTOR-AVAILABLE   VALUE ZERO.
000193             20  RL-OTHER-1-ADDR-CNT  PIC S9.
000194                 88  RL-NO-OTHER-1-ADDRESSES  VALUE ZERO.
000195             20  RL-OTHER-2-ADDR-CNT  PIC S9.
000196                 88  RL-NO-OTHER-2-ADDRESSES  VALUE ZERO.
000197
000198     12  RL-CV-REFERENCE-NO.
000199         16  RL-CV-REFNO-PRIME       PIC X(18).
000200         16  RL-CV-REFNO-SFX         PIC XX.
000201
000202     12  RL-FILE-LOCATION            PIC X(4).
000203
000204     12  RL-PROCESS-ERRORS.
000205         16  RL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
000206             88  RL-NO-FATAL-ERRORS     VALUE ZERO.
000207         16  RL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
000208             88  RL-NO-FORCABLE-ERRORS  VALUE ZERO.
000209
000210     12  RL-PRODUCT-CD               PIC X.
000211
000212     12  RL-CURRENT-KEY-DATA.
000213         16  RL-CURRENT-CARRIER      PIC X.
000214         16  RL-CURRENT-GROUPING     PIC X(6).
000215         16  RL-CURRENT-STATE        PIC XX.
000216         16  RL-CURRENT-ACCOUNT      PIC X(10).
000217
000218     12  RL-ASSOCIATES               PIC X.
000219         88  RL-ASSOC-NO-INTERFACE      VALUE 'A'.
000220         88  RL-ASSOC-INTERFACE         VALUE 'I'.
000221         88  RL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
000222         88  RL-NON-ASSOC-INTERFACE     VALUE 'M'.
000223
000224     12  RL-ACTIVITY-CODE            PIC 99.
000225     12  RL-ACTIVITY-MAINT-DT        PIC XX.
000226     12  RL-ACTIVITY-MAINT-TYPE      PIC X(4).
000227
000228     12  RL-LAPSE-REPORT-CODE        PIC 9.
000229     12  RL-LAG-REPORT-CODE          PIC 9.
000230     12  RL-LOAN-TYPE                PIC XX.
000231     12  RL-LEGAL-STATE              PIC XX.
000232
000233     12  FILLER                      PIC X(5).
      *<<((file: ELCRETR))
000733     EJECT
000734*                COPY MTCPHST REPLACING ==:TAG:== BY ==PH==.
      *>>((file: MTCPHST))
000001******************************************************************
000002*                                                                *
000003*                           MTCPHST                              *
000004*                            VMOD=1.001                          *
000005*                                                                *
000006*   FILE DESCRIPTION = POLICY HISTORY FILE                       *
000007*                                                                *
000008*   FILE TYPE = VSAM,KSDS                                        *
000009*   RECORD SIZE = 132  RECFORM = FIXED                           *
000010*                                                                *
000011*   BASE CLUSTER = MPPHST                         RKP=2,LEN=46   *
000012*       ALTERNATE (NONE)                                         *
000013*                                                                *
000014*   LOG = YES                                                    *
000015*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000016******************************************************************
000017
000018 01  POLICY-HISTORY.
000019     12 PH-RECORD-ID                    PIC XX.
000020         88  VALID-PH-ID                      VALUE 'PH'.
000021
000022******************************************************************
000023*   BASE CLUSTER = MPPHST         (BASE KEY)      RKP=2,LEN=46   *
000024******************************************************************
000025
000026     12 PH-CONTROL-PRIMARY.
000027         16 PH-COMPANY-CD               PIC X.
000028         16 PH-CARRIER                  PIC X.
000029         16 PH-GROUPING.
000030             20 PH-GROUPING-PREFIX      PIC X(3).
000031             20 PH-GROUPING-PRIME       PIC X(3).
000032         16 PH-STATE                    PIC XX.
000033         16 PH-PRODUCER.
000034             20 PH-PRODUCER-PREFIX      PIC X(4).
000035             20 PH-PRODUCER-PRIME       PIC X(6).
000036         16 PH-POLICY-EFF-DT            PIC XX.
000037         16 PH-POLICY-NO.
000038             20 PH-POLICY-PRIME         PIC X(18).
000039             20 PH-POLICY-SFX           PIC XX.
000040         16 PH-RECORD-TYPE              PIC X(2).
000041         16 PH-FIELD-TYPE               PIC X(2).
000042         16 PH-SEQUENCE-NO              PIC S9(4)    COMP.
000043             88 PH-1ST-HISTORY-REC         VALUE +4095.
000044     12  FILLER                            PIC X(16).
000045
000046******************************************************************
000047*                 FILE SYNCHRONIZATION DATA                      *
000048******************************************************************
000049
000050     12 PH-FILE-SYNCH-DATA.
000051         16 PH-TRANSACTION-ID           PIC X(4).
000052         16 PH-CHANGE-DT                PIC XX.
000053         16 PH-CHANGE-TIME              PIC S9(7)  COMP-3.
000054         16 PH-CHANGE-PROCESSOR         PIC X(4).
000055         16 PH-MONTH-END-DT             PIC XX.
000056
000057******************************************************************
000058*                 POLICY HISTORY RECORD BODY                     *
000059******************************************************************
000060
000061     12 PH-RECORD-BODY                  PIC X(50).
000062
000063******************************************************************
000064*                       NUMERIC DATA                             *
000065******************************************************************
000066     12  FILLER           REDEFINES PH-RECORD-BODY.
000067         20 PH-NUMERIC-FLD1             PIC S9(10)V99.
000068         20  FILLER                        PIC X(38).
000069
000070     12  FILLER           REDEFINES PH-RECORD-BODY.
000071         20 PH-NUMERIC-FLD2             PIC S9(2)V999.
000072         20  FILLER                        PIC X(45).
000073
000074     12  FILLER           REDEFINES PH-RECORD-BODY.
000075         20 PH-NUMERIC-FLD3             PIC S9(15).
000076         20  FILLER                        PIC X(35).
000077
000078******************************************************************
      *<<((file: MTCPHST))
000735     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL150' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000736 VCOBOL-DUMMY-PROCEDURE.
000737
000738     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
000739     MOVE '5'                    TO DC-OPTION-CODE.
000740     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
000741     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000742     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000743
000744     IF EIBCALEN = 0
000745         GO TO 8800-UNAUTHORIZED-ACCESS.
000746
000747     MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
000748
000749     IF PI-COMPANY-ID NOT = 'DMD'
000750         MOVE SPACES             TO WST-REM-DAYS-GRP
000751                                    WST-ORIG-DAYS-GRP.
000752
000753     MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.
000754     MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.
000755
000756     MOVE EIBTRMID               TO QID-TERM.
000757
000758     
      * EXEC CICS HANDLE CONDITION
000759*        QIDERR   (1000-SHOW-CLAIM)
000760*        MAPFAIL  (0100-FIRST-TIME-IN)
000761*        NOTOPEN  (8500-FILE-NOTOPEN)
000762*        PGMIDERR (9600-PGMID-ERROR)
000763*        ERROR    (9990-ABEND)
000764*    END-EXEC.
      *    MOVE '"$N?JL.               ! " #00008296' TO DFHEIV0
           MOVE X'22244E3F4A4C2E2020202020' &
                X'202020202020202020202120' &
                X'2220233030303038323936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000765
000766     IF PI-RETURN-TO-PROGRAM = THIS-PGM
000767         MOVE PI-CALLING-PROGRAM TO RETURNED-FROM.
000768
000769     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000770         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000771             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000772             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000773             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000774             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000775             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000776             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000777             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000778             MOVE THIS-PGM             TO PI-CALLING-PROGRAM
000779         ELSE
000780             MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
000781             MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
000782             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
000783             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
000784             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
000785             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
000786             MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
000787             MOVE SPACES               TO PI-SAVED-PROGRAM-6.
000788
000789     IF (RETURNED-FROM NOT = SPACES)
000790        and (pi-save-map not = crtt-map)
000791         GO TO 0600-RECOVER-TEMP-STORAGE.
000792
000793     IF EIBAID = DFHCLEAR
000794         GO TO 9400-CLEAR.
000795
000796     IF PI-PROCESSOR-ID = 'LGXX'
000797         NEXT SENTENCE
000798     ELSE
000799         
      * EXEC CICS READQ TS
000800*            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
000801*            INTO    (SECURITY-CONTROL)
000802*            LENGTH  (SC-COMM-LENGTH)
000803*            ITEM    (SC-ITEM)
000804*        END-EXEC
      *    MOVE '*$II   L              ''   #00008337' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303038333337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000805         MOVE SC-CLAIMS-DISPLAY (16)  TO  PI-DISPLAY-CAP
000806         MOVE SC-CLAIMS-UPDATE  (16)  TO  PI-MODIFY-CAP
000807         IF NOT DISPLAY-CAP
000808             MOVE 'READ'              TO  SM-READ
000809             PERFORM 9995-SECURITY-VIOLATION
000810             MOVE ER-0070             TO  EMI-ERROR
000811             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000812             GO TO 8100-SEND-INITIAL-MAP.
000813
000814     IF EIBTRNID = TRANS-ID
000815         GO TO 0200-RECEIVE.
000816
000817 0100-FIRST-TIME-IN.
000818
000819     MOVE LOW-VALUES             TO EL150AO.
000820
000821     IF NOT PI-CONFIDENTIAL-UP
000822         MOVE LOW-VALUES         TO PI-REDEF.
000823
000824     MOVE 'ELMSTR'               TO PI-CURRENT-FILE.
000825     MOVE ZEROS                  TO PI-TOTAL-PAID
000826                                    PI-DAYS-PAID
000827                                    PI-LAST-SEQ-NO.
000828
000829     MOVE 0                      TO PI-FORCE-COUNT.
000830
000831     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
000832     MOVE '2'                    TO CNTL-REC-TYPE.
000833     MOVE +0                     TO CNTL-SEQ-NO.
000834     MOVE PI-PROCESSOR-ID        TO CNTL-ACCESS.
000835     MOVE 'CNTL'                 TO FILE-SWITCH.
000836
000837     
      * EXEC CICS READ
000838*         DATASET  ('ELCNTL')
000839*         SET      (ADDRESS OF CONTROL-FILE)
000840*         RIDFLD   (ELCNTL-KEY)
000841*         RESP     (WS-RESPONSE)
000842*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00008375' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303038333735' TO DFHEIV0
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
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000843
000844     IF WS-RESP-NORMAL
000845        MOVE CF-APPROVAL-LEVEL TO PI-APPROVAL-LEVEL
000846     END-IF.
000847
000848
000849     
      * EXEC CICS DELETEQ TS
000850*        QUEUE(QID)
000851*    END-EXEC.
      *    MOVE '*&                    #   #00008387' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038333837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000852
000853     GO TO 1000-SHOW-CLAIM.
000854
000855     EJECT
000856 0200-RECEIVE.
000857     MOVE 'B'                    TO PASS-SWITCH.
000858
000859     MOVE LOW-VALUES             TO EL150AI.
000860
000861     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000862         MOVE ER-0008            TO EMI-ERROR
000863         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000864         MOVE -1                 TO FILETOL
000865         GO TO 8200-SEND-DATAONLY.
000866
000867     if pi-save-map = crtt-map
000868        move pi-save-map to map-name
000869     end-if
000870
000871     
      * EXEC CICS RECEIVE
000872*        MAP      (MAP-NAME)
000873*        MAPSET   (MAPSET-NAME)
000874*        INTO     (EL150AI)
000875*    END-EXEC.
           MOVE LENGTH OF
            EL150AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00008409' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303038343039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000876
000877     if pi-save-map = crtt-map
000878        move spaces              to pi-save-map
000879        move sad-map             to map-name
000880        go to 0100-first-time-in
000881     end-if
000882
000883     IF ENTERPFL = 0
000884         GO TO 0300-CHECK-PFKEYS.
000885
000886     IF EIBAID NOT = DFHENTER
000887         MOVE ER-0004            TO EMI-ERROR
000888         GO TO 0320-INPUT-ERROR.
000889
000890     IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)
000891         MOVE PF-VALUES (ENTERPFI) TO EIBAID
000892     ELSE
000893         MOVE ER-0029            TO EMI-ERROR
000894         GO TO 0320-INPUT-ERROR.
000895
000896 0300-CHECK-PFKEYS.
000897     IF EIBAID = DFHPF23
000898         GO TO 8810-PF23.
000899
000900     IF EIBAID = DFHPF24
000901         GO TO 9200-RETURN-MAIN-MENU.
000902
000903     IF EIBAID = DFHPF12
000904         GO TO 9500-PF12.
000905
000906     IF (EIBAID = DFHPF22) AND (NOT FORCE-CAP)
000907         MOVE ER-0433            TO EMI-ERROR
000908         GO TO 0320-INPUT-ERROR.
000909
000910     IF (PI-FORCE-COUNT NOT = 0) AND (EIBAID NOT = DFHPF22)
000911         MOVE ER-0500            TO EMI-ERROR
000912         GO TO 0320-INPUT-ERROR.
000913
000914     IF PI-RETRIEVED-DATA
000915         IF EIBAID = DFHPF1  OR DFHPF2 OR DFHPF15 OR
000916                     DFHPF16 OR DFHPF21
000917             NEXT SENTENCE
000918         ELSE
000919             MOVE ER-0926        TO EMI-ERROR
000920             GO TO 0320-INPUT-ERROR.
000921
000922*    IF EIBAID = DFHPF21
000923*        IF NOT PI-RETRIEVED-DATA
000924*            PERFORM 7650-CREATE-ACTQ THRU 7650-EXIT
000925*            MOVE ER-0927        TO EMI-ERROR
000926*            GO TO 0320-INPUT-ERROR
000927*        ELSE
000928*            GO TO 7700-CREATE-CLAIM.
000929
000930     IF CLAIM-IS-PURGED
000931         IF EIBAID = DFHPF1  OR DFHPF2  OR DFHPF3  OR DFHPF4  OR
000932                     DFHPF6  OR DFHPF7  OR DFHPF8  OR DFHPF9  OR
000933                     DFHPF10 OR DFHPF11 OR DFHPF13 OR DFHPF14 OR
000934                     DFHPF17 OR DFHPF18 OR DFHPF20
000935                       MOVE -1            TO  ENTERPFL
000936                       MOVE 'CLAIM IS PURGED, PFKEYS ARE INVALID'
000937                                          TO  MAP-DISPLAY (1)
000938                         GO TO 8200-SEND-DATAONLY.
000939
000940     IF (PI-SUPERVISOR-ONLY)
000941        AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
000942             AND 'AMWA')
000943*       NOT SYSTEM-MODIFY-CAP
000944*        IF EIBAID = DFHPF3  OR DFHPF6  OR DFHPF7  OR DFHPF8  OR
000945         IF EIBAID = DFHPF4  OR DFHPF6  OR DFHPF7  OR DFHPF8  OR
000946                     DFHPF9  or
000947                     DFHPF11 OR DFHPF13 OR DFHPF14 OR DFHPF17 OR
000948                     DFHPF21 OR DFHENTER
000949             MOVE -1             TO ENTERPFL
000950             MOVE ER-0931        TO EMI-ERROR
000951             GO TO 0320-INPUT-ERROR.
000952
000953     IF EIBAID = DFHPF13 OR DFHPF19 OR DFHPF20
000954         IF PI-COMPANY-ID = 'AIG' OR 'AUK'
000955             MOVE ER-0029            TO  EMI-ERROR
000956             GO TO 0320-INPUT-ERROR.
000957
000958     IF EIBAID = DFHPF5 OR DFHPF18 OR DFHPF19 OR DFHPF20
000959         IF PI-COMPANY-ID = 'DMD'
000960             AND
000961            NOT SYSTEM-MODIFY-CAP
000962            IF PI-PRIORITY-9
000963               MOVE ER-0931            TO  EMI-ERROR
000964               GO TO 0320-INPUT-ERROR.
000965
000966     IF EIBAID = DFHPF7 OR DFHPF6
000967         IF PI-JOINT-INSURED AND NOT PI-JOINT-COVERAGE
000968            AND PI-TOTAL-PAID NOT GREATER THAN +0
000969             MOVE EIBAID             TO PI-PFKEY-USED
000970             MOVE ER-3550            TO  EMI-ERROR
000971             GO TO 0320-INPUT-ERROR.
000972
000973     IF EIBAID = DFHPF22 AND PI-PFKEY-USED = DFHPF7
000974                 GO TO 0500-CREATE-TEMP-STORAGE.
000975     IF EIBAID = DFHPF22 AND PI-PFKEY-USED = DFHPF6
000976         IF PI-SAVE-TYPE = 'L'
000977             MOVE ER-0376        TO EMI-ERROR
000978             GO TO 0320-INPUT-ERROR
000979         ELSE
000980             GO TO 0500-CREATE-TEMP-STORAGE.
000981
000982     IF EIBAID = DFHPF3  OR DFHPF4  OR DFHPF5  OR DFHPF7  OR
000983                 DFHPF8  OR DFHPF9  OR DFHPF10 OR DFHPF13 OR
000984                 DFHPF14 OR DFHPF17 OR DFHPF18 OR DFHPF19 OR
000985                 DFHPF20 or dfhpf11 OR DFHPF21
000986                 GO TO 0500-CREATE-TEMP-STORAGE.
000987
000988     IF EIBAID = DFHPF6
000989         IF PI-SAVE-TYPE = 'L'
000990             MOVE ER-0376        TO EMI-ERROR
000991             GO TO 0320-INPUT-ERROR
000992         ELSE
000993             GO TO 0500-CREATE-TEMP-STORAGE.
000994
000995     MOVE SPACES                 TO ERRMSG1O.
000996
000997     IF EIBAID = DFHPF15
000998         GO TO 7000-START-BROWSE.
000999
001000     IF EIBAID = DFHPF16
001001         GO TO 7500-START-BROWSE.
001002
001003     IF EIBAID = DFHPF1
001004         MOVE 'F'                TO DIRECTION-SWITCH
001005         GO TO 1600-ROLL-TRAILERS.
001006
001007     IF EIBAID = DFHPF2
001008         MOVE 'B'                TO DIRECTION-SWITCH
001009         GO TO 1600-ROLL-TRAILERS.
001010
001011     IF EIBAID = DFHENTER
001012         GO TO 0330-EDIT-DATA.
001013
001014*      string "EIBAID = " eibaid into map-display (1)
001015*      GO TO 8200-SEND-DATAONLY.
001016
001017     MOVE ER-0029                TO EMI-ERROR.
001018
001019 0320-INPUT-ERROR.
001020     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001021
001022     MOVE EMI-FORCABLE-CTR       TO  PI-FORCE-COUNT.
001023
001024     IF ENTERPFL = 0
001025         MOVE -1                 TO FILETOL
001026     ELSE
001027         MOVE AL-UNBON           TO ENTERPFA
001028         MOVE -1                 TO ENTERPFL.
001029
001030     GO TO 8200-SEND-DATAONLY.
001031
001032 0330-EDIT-DATA.
001033
001034     MOVE 0                      TO PI-FORCE-COUNT.
001035
001036     IF NOT MODIFY-CAP
001037         MOVE 'UPDATE'           TO  SM-READ
001038         PERFORM 9995-SECURITY-VIOLATION
001039         MOVE ER-0070            TO  EMI-ERROR
001040         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001041         GO TO 8100-SEND-INITIAL-MAP.
001042
001043     IF CLMNOL  > 0 OR
001044        CARRL   > 0 OR
001045        CERTNOL > 0 OR
001046        SUFXL   > 0
001047            GO TO 1000-SHOW-CLAIM.
001048
001049     IF YESNOSWL > 0
001050         MOVE AL-UANON           TO YESNOSWA.
001051
001052*    IF MCRFLML > 0
001053*        MOVE AL-UANON           TO MCRFLMA.
001054
001055     IF FILETOL > 0
001056         MOVE AL-UANON           TO FILETOA.
001057
001058     IF CLOANL > 0
001059        MOVE AL-UANON            TO CLOANA
001060     END-IF
001061
001062     IF TOTINTL > 0
001063         MOVE AL-UNNON           TO TOTINTA
001064     END-IF
001065
001066     IF PRTOPTL > 0
001067        IF PRTOPTI NOT = 'N' AND 'L'
001068           MOVE AL-UABON         TO PRTOPTA
001069           MOVE -1               TO PRTOPTL
001070           MOVE ER-0334 TO EMI-ERROR
001071           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001072        ELSE
001073           MOVE AL-UANON         TO PRTOPTA.
001074
001075     IF PRTOPTL > 0
001076         IF (NOT PI-NO-CARRIER-SECURITY OR
001077             NOT PI-NO-ACCOUNT-SECURITY)
001078             IF PRTOPTI NOT = 'N'
001079                 MOVE AL-UABON   TO PRTOPTA
001080                 MOVE -1         TO PRTOPTL
001081                 MOVE ER-2378    TO EMI-ERROR
001082                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001083
001084     IF FORMATL > 0
001085         IF FORMATI NOT = 'F' AND 'P'
001086             MOVE AL-UABON       TO FORMATA
001087             MOVE -1             TO FORMATL
001088             MOVE ER-0335        TO EMI-ERROR
001089             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001090         ELSE
001091             MOVE AL-UANON       TO FORMATA.
001092
001093     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
001094         IF ASSOCL > +0
001095             IF ASSOCI = 'A' OR 'I' OR 'N' OR 'M'
001096                 MOVE AL-UANON   TO  ASSOCA
001097             ELSE
001098                 MOVE AL-UABON   TO  ASSOCA
001099                 MOVE -1         TO  ASSOCL
001100                 MOVE ER-0804    TO  EMI-ERROR
001101                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001102
001103 0335-EDIT-ACTIVITY-CODE.
001104******************************************************************
001105*    THIS PARAGRAPH DETERMINES IF THE COMPANY IS CURRENTLY USING *
001106*    THE AUTOMATIC ACTIVITY FUNCTIONS OF THE SYSTEM AND, IF SO,  *
001107*    WHAT FUNCTIONS ARE TO BE PERFORMED BASED ON THE CODES SET   *
001108*    IN THE AUTOMATIC ACTIVITY RECORD.                           *
001109******************************************************************
001110
001111     IF ACTCDL = +0
001112         GO TO 0340-EDIT-DATA.
001113
001114     IF ACTCDI IS NOT NUMERIC
001115         MOVE ZEROS              TO  ACTCDI
001116         MOVE AL-UABON           TO  ACTCDA
001117         MOVE -1                 TO  ACTCDL
001118         MOVE ER-3526            TO  EMI-ERROR
001119         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001120         GO TO 0340-EDIT-DATA.
001121
001122     MOVE ACTCDI                 TO  WS-ACTIVITY-CODE.
001123     IF NOT VALID-ACTIVITY-CODE
001124         MOVE AL-UABON           TO  ACTCDA
001125         MOVE -1                 TO  ACTCDL
001126         MOVE ER-3526            TO  EMI-ERROR
001127         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001128         GO TO 0340-EDIT-DATA.
001129
001130     MOVE 'CNTL'                 TO  FILE-SWITCH.
001131     PERFORM 6000-CHECK-AUTO-ACTIVITY THRU 6000-EXIT.
001132
001133     IF WS-ACT-REC-FOUND-SW = 'N'
001134          MOVE +0                TO  ACTCDL
001135          GO TO 0340-EDIT-DATA.
001136
001137     IF ACTCDI = ZEROS
001138         NEXT SENTENCE
001139     ELSE
001140         MOVE ACTCDI             TO  SUB
001141         SUBTRACT +9 FROM SUB
001142         IF CF-USER-ACTIVE-SW (SUB) = ' ' OR 'N'
001143             MOVE AL-UABON       TO  ACTCDA
001144             MOVE -1             TO  ACTCDL
001145             MOVE ER-3545        TO  EMI-ERROR
001146             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001147             GO TO 0340-EDIT-DATA.
001148
001149     IF ACTCDI = ZEROS
001150         IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CIG' OR 'CUK'
001151             PERFORM 6100-RESET-AUTO-ACTIVITY THRU 6100-EXIT
001152             MOVE AL-UANOF       TO  ACTCDA
001153             GO TO 0340-EDIT-DATA.
001154
001155     MOVE 'Y'                    TO  WS-LETTER-SW.
001156     MOVE AL-UANOF               TO  ACTCDA.
001157
001158 0340-EDIT-DATA.
001159
001160     IF EMI-NO-ERRORS
001161         IF PRTOPTL NOT = FORMATL
001162             MOVE AL-UABON       TO PRTOPTA  FORMATA
001163             MOVE -1             TO PRTOPTL
001164             MOVE ER-0336 TO EMI-ERROR
001165             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001166
001167     IF PRTOPTL > 0
001168        IF (NOT PI-NO-CARRIER-SECURITY OR
001169            NOT PI-NO-ACCOUNT-SECURITY)
001170           IF ALTPRTL NOT > 0
001171              MOVE AL-UABON      TO ALTPRTA
001172              MOVE -1            TO ALTPRTL
001173              MOVE ER-2379       TO EMI-ERROR
001174              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001175
001176     IF NOT EMI-NO-ERRORS
001177         IF EMI-FORCABLE-CTR = ZEROS AND
001178            EMI-FATAL-CTR = ZEROS
001179             NEXT SENTENCE
001180         ELSE
001181             GO TO 8200-SEND-DATAONLY.
001182
001183     IF FILETOL NOT > 0        AND
001184*       MCRFLML NOT > 0        AND
001185        ACTCDL  NOT > 0        AND
001186        ASSOCL  NOT > 0        AND
001187        CLOANL  NOT > 0        AND
001188        TOTINTL NOT > 0        AND
001189        YESNOSWL NOT > 0
001190         GO TO 0350-PROCESS-OPTIONS.
001191
001192     
      * EXEC CICS HANDLE CONDITION
001193*        NOTFND   (0450-NOT-FOUND)
001194*    END-EXEC.
      *    MOVE '"$I                   ! # #00008730' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303038373330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001195
001196     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
001197     MOVE PI-CARRIER             TO MSTR-CARRIER.
001198     MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.
001199     MOVE PI-CERT-NO             TO MSTR-CERT-NO.
001200     MOVE 'MSTR'                 TO FILE-SWITCH.
001201
001202     
      * EXEC CICS READ
001203*        UPDATE
001204*        DATASET   ('ELMSTR')
001205*        RIDFLD    (ELMSTR-KEY)
001206*        INTO      (CLAIM-MASTER)
001207*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"IL       EU         (   #00008740' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303038373430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001208
001209     IF ACTCDL > +0
001210         IF CL-LAST-MAINT-USER NOT = PI-UPDATE-BY
001211             MOVE ER-0068        TO  EMI-ERROR
001212             MOVE -1             TO  ACTCDL
001213             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001214             GO TO 8200-SEND-DATAONLY
001215         ELSE
001216             NEXT SENTENCE
001217     ELSE
001218         IF (CL-LAST-MAINT-USER NOT = PI-UPDATE-BY) OR
001219            (CL-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS)
001220               MOVE ER-0068      TO EMI-ERROR
001221               MOVE -1           TO FILETOL
001222               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001223               GO TO 8200-SEND-DATAONLY.
001224
001225     MOVE 'ELMSTR  '             TO FILE-ID.
001226
001227*    IF MCRFLML > 0
001228*        MOVE MCRFLMI            TO CL-MICROFILM-NO.
001229
001230     IF FILETOL > 0
001231         IF FILETOI  = CL-FILE-LOCATION
001232             MOVE ZEROS          TO FILETOL
001233         ELSE
001234             MOVE FILETOI        TO CL-FILE-LOCATION.
001235
001236     IF TOTINTL > 0
001237         
      * EXEC CICS BIF DEEDIT
001238*            FIELD(TOTINTI)
001239*            LENGTH(9)
001240*        END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008775' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038373735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TOTINTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001241         IF TOTINTI  = CL-TOTAL-INT-PAID
001242             MOVE ZEROS          TO TOTINTL
001243         ELSE
001244             MOVE TOTINTI        TO CL-TOTAL-INT-PAID
001245     END-IF.
001246
001247     IF CLOANL > 0
001248        IF CLOANI NOT = SPACES
001249           MOVE CL-CONTROL-PRIMARY
001250                                 TO ELTRLR-KEY
001251           MOVE +91              TO TRLR-SEQ-NO
001252           MOVE 'TRLR'           TO FILE-SWITCH
001253           
      * EXEC CICS READ
001254*             UPDATE
001255*             DATASET   ('ELTRLR')
001256*             RIDFLD    (ELTRLR-KEY)
001257*             SET       (ADDRESS OF ACTIVITY-TRAILERS)
001258*             RESP      (WS-RESPONSE)
001259*          END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00008791' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303038373931' TO DFHEIV0
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
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001260           IF WS-RESP-NORMAL
001261              MOVE CLOANI        TO AT-INFO-LINE-1
001262              MOVE PI-PROCESSOR-ID
001263                                 TO AT-GEN-INFO-LAST-UPDATED-BY
001264              MOVE SAVE-BIN-DATE TO AT-GEN-INFO-LAST-MAINT-DT
001265              MOVE EIBTIME       TO AT-LAST-MAINT-HHMMSS
001266              
      * EXEC CICS REWRITE
001267*                DATASET   ('ELTRLR')
001268*                FROM      (ACTIVITY-TRAILERS)
001269*             END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00008804' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303038383034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001270           ELSE
001271              IF WS-RESP-NOTFND
001272                 
      * EXEC CICS GETMAIN
001273*                   SET       (ADDRESS OF ACTIVITY-TRAILERS)
001274*                   LENGTH    (200)
001275*                   INITIMG   (GETMAIN-SPACE)
001276*                END-EXEC
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00008810' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038383130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001277                 MOVE 'AT'       TO AT-RECORD-ID
001278                 MOVE CL-CONTROL-PRIMARY
001279                                 TO AT-CONTROL-PRIMARY
001280                 MOVE +91        TO AT-SEQUENCE-NO
001281                 MOVE CLOANI     TO AT-INFO-LINE-1
001282                 MOVE '6'        TO AT-TRAILER-TYPE
001283                 MOVE PI-PROCESSOR-ID
001284                                 TO AT-RECORDED-BY
001285                                    AT-GEN-INFO-LAST-UPDATED-BY
001286                 MOVE SAVE-BIN-DATE
001287                                 TO AT-RECORDED-DT
001288                                    AT-GEN-INFO-LAST-MAINT-DT
001289                 MOVE EIBTIME    TO AT-LAST-MAINT-HHMMSS
001290                 
      * EXEC CICS WRITE
001291*                   DATASET   ('ELTRLR')
001292*                   FROM      (ACTIVITY-TRAILERS)
001293*                   RIDFLD    (AT-CONTROL-PRIMARY)
001294*                END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00008828' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303038383238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001295              END-IF
001296           END-IF
001297        END-IF
001298     END-IF
001299
001300     IF YESNOSWI EQUAL TO CL-YESNOSW
001301         MOVE ZEROS    TO YESNOSWL
001302     ELSE
001303         MOVE YESNOSWI TO CL-YESNOSW
001304     END-IF.
001305
001306     IF WS-ACT-REC-FOUND-SW = 'Y'
001307         IF ACTCDL > +0
001308             MOVE ACTCDI             TO  CL-ACTIVITY-CODE
001309             MOVE SAVE-BIN-DATE      TO  CL-ACTIVITY-MAINT-DT
001310                                         DC-BIN-DATE-1
001311             MOVE ' '                TO  DC-OPTION-CODE
001312             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001313             MOVE DC-GREG-DATE-1-EDIT    TO  ACTDTO.
001314
001315     IF WS-ACT-REC-FOUND-SW = 'Y'
001316         IF ACTCDL > +0
001317             IF ACTCDI = ZEROS
001318                 MOVE SPACES         TO  CL-ACTIVITY-MAINT-TYPE
001319                                         ACTTYPO
001320             ELSE
001321                 MOVE WS-ACTIVITY-CODE   TO  SUB
001322                 SUBTRACT 9 FROM SUB
001323                 MOVE CF-USER-ACTIVITY-DESC (SUB)
001324                                     TO  WS-ACT-USER-DESC
001325                 MOVE WS-DESC-1-3    TO  CL-ACTIVITY-MAINT-TYPE
001326                                         ACTTYPO.
001327
001328     IF WS-ACT-REC-FOUND-SW = 'Y'
001329         IF ACTCDL > +0
001330             IF WS-UPDATE-SW = 'Y'
001331                 MOVE LOW-VALUES     TO  CL-NEXT-RESEND-DT
001332                                         CL-NEXT-FOLLOWUP-DT.
001333
001334     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
001335         IF ASSOCL > +0
001336             MOVE ASSOCI         TO  CL-ASSOCIATES.
001337
001338     IF PI-COMPANY-ID = 'FLA'
001339         NEXT SENTENCE
001340     ELSE
001341         MOVE '3'                TO  CL-LAST-MAINT-TYPE
001342         MOVE PI-PROCESSOR-ID    TO  CL-LAST-MAINT-USER
001343                                     SV-LAST-BY
001344         MOVE EIBTIME            TO  CL-LAST-MAINT-HHMMSS
001345                                     SV-LAST-HHMMSS
001346         MOVE SAVE-BIN-DATE      TO  CL-LAST-MAINT-DT.
001347
001348 0350-PROCESS-OPTIONS.
001349     IF PRTOPTL NOT > 0
001350         GO TO 0400-WRITE-CLAIM.
001351
001352     IF PRTOPTI = 'N'
001353         GO TO 0380-START-PRINTER.
001354
001355     
      * EXEC CICS HANDLE CONDITION
001356*        NOTFND(0360-CREATE-NEW-ACTQ)
001357*    END-EXEC.
      *    MOVE '"$I                   ! $ #00008893' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303038383933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001358
001359     MOVE PI-COMPANY-CD          TO ACTQ-COMP-CD.
001360     MOVE PI-CARRIER             TO ACTQ-CARRIER.
001361     MOVE PI-CLAIM-NO            TO ACTQ-CLAIM-NO.
001362     MOVE PI-CERT-NO             TO ACTQ-CERT-NO.
001363
001364     MOVE 'ACTQ'                 TO FILE-SWITCH.
001365     MOVE 'ELACTQ  '             TO FILE-ID.
001366
001367     
      * EXEC CICS READ
001368*        UPDATE
001369*        DATASET   ('ELACTQ')
001370*        SET       (ADDRESS OF ACTIVITY-QUE)
001371*        RIDFLD    (ELACTQ-KEY)
001372*    END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00008905' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303038393035' TO DFHEIV0
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
           
001373
001374     IF FORMATI = 'F'
001375         MOVE '1'                TO AQ-PENDING-STATUS-FLAG
001376     ELSE
001377         MOVE '2'                TO AQ-PENDING-STATUS-FLAG.
001378
001379     MOVE +150                   TO AQ-LAST-UPDATED-BY.
001380
001381     MOVE 'ELACTQ  '             TO FILE-ID.
001382
001383     
      * EXEC CICS REWRITE
001384*        DATASET   ('ELACTQ')
001385*        FROM      (ACTIVITY-QUE)
001386*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&& L                  %   #00008921' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303038393231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001387
001388     GO TO 0400-WRITE-CLAIM.
001389
001390 0360-CREATE-NEW-ACTQ.
001391     
      * EXEC CICS GETMAIN
001392*        SET       (ADDRESS OF ACTIVITY-QUE)
001393*        LENGTH    (60)
001394*        INITIMG   (GETMAIN-SPACE)
001395*    END-EXEC.
           MOVE 60
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00008929' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038393239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001396
001397     MOVE 'AQ'                   TO AQ-RECORD-ID.
001398     MOVE PI-COMPANY-CD          TO AQ-COMPANY-CD.
001399     MOVE PI-CARRIER             TO AQ-CARRIER.
001400     MOVE PI-CLAIM-NO            TO AQ-CLAIM-NO.
001401     MOVE PI-CERT-NO             TO AQ-CERT-NO.
001402
001403     IF FORMATI = 'F'
001404         MOVE '1'                TO AQ-PENDING-STATUS-FLAG
001405     ELSE
001406         MOVE '2'                TO AQ-PENDING-STATUS-FLAG.
001407
001408     MOVE +0                     TO AQ-PAYMENT-COUNTER
001409                                    AQ-PMT-UNAPPROVED-COUNT.
001410
001411     MOVE LOW-VALUES             TO AQ-RESEND-DATE
001412                                    AQ-FOLLOWUP-DATE.
001413
001414     MOVE +150                   TO AQ-LAST-UPDATED-BY.
001415
001416     MOVE 'ACTQ'                 TO FILE-SWITCH.
001417
001418     MOVE 'ELACTQ  '             TO FILE-ID.
001419
001420     
      * EXEC CICS WRITE
001421*        DATASET   ('ELACTQ')
001422*        FROM      (ACTIVITY-QUE)
001423*        RIDFLD    (AQ-CONTROL-PRIMARY)
001424*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00008958' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303038393538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001425
001426     GO TO 0400-WRITE-CLAIM.
001427
001428 0380-START-PRINTER.
001429     
      * EXEC CICS HANDLE CONDITION
001430*         TERMIDERR    (0385-TERMID-ERROR)
001431*         TRANSIDERR   (0390-TRANS-ERROR)
001432*    END-EXEC.
      *    MOVE '"$[\                  ! % #00008967' TO DFHEIV0
           MOVE X'22245B5C2020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303038393637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001433
001434     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
001435     MOVE '1'                    TO CNTL-REC-TYPE.
001436     MOVE SPACES                 TO CNTL-ACCESS.
001437     MOVE +0                     TO CNTL-SEQ-NO.
001438     MOVE 'CNTL'                 TO FILE-SWITCH.
001439
001440     
      * EXEC CICS READ
001441*        DATASET   ('ELCNTL')
001442*        SET       (ADDRESS OF CONTROL-FILE)
001443*        RIDFLD    (ELCNTL-KEY)
001444*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008978' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038393738' TO DFHEIV0
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
           
001445
001446     IF CF-FORMS-PRINTER-ID = SPACES
001447         MOVE ER-0337            TO EMI-ERROR
001448         MOVE -1                 TO FILETOL
001449         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001450         GO TO 8200-SEND-DATAONLY.
001451
001452     IF FORMATI = 'F'
001453         MOVE '2'                TO PI-ENTRY-CD-1
001454     ELSE
001455         MOVE '1'                TO PI-ENTRY-CD-1.
001456
001457     IF PI-PROCESSOR-PRINTER NOT = SPACES
001458         MOVE PI-PROCESSOR-PRINTER  TO  CF-FORMS-PRINTER-ID.
001459
001460     MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.
001461
001462     IF ALTPRTL > 0
001463        MOVE ALTPRTI             TO CF-FORMS-PRINTER-ID
001464                                    PI-ALT-DMD-PRT-ID.
001465
001466     IF PI-COMPANY-ID = 'DMD' OR 'XXX'
001467*       MOVE EIBTRMID            TO CF-FORMS-PRINTER-ID
001468         
      * EXEC CICS START
001469*            TRANSID   (START-TRANS-ID)
001470*            FROM      (PROGRAM-INTERFACE-BLOCK)
001471*            LENGTH    (PI-COMM-LENGTH)
001472*            TERMID    ('A155')
001473*            TERMID    (CF-FORMS-PRINTER-ID)
001474*        END-EXEC
      *    MOVE '0( LF                 1   #00009006' TO DFHEIV0
           MOVE X'3028204C4620202020202020' &
                X'202020202020202020203120' &
                X'2020233030303039303036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 START-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001475     ELSE
001476         
      * EXEC CICS START
001477*            TRANSID   (START-TRANS-ID)
001478*            TERMID    (CF-FORMS-PRINTER-ID)
001479*            TERMID    ('PRN1')
001480*            FROM      (PROGRAM-INTERFACE-BLOCK)
001481*            LENGTH    (PI-COMM-LENGTH)
001482*        END-EXEC.
      *    MOVE '0( LFT                1   #00009014' TO DFHEIV0
           MOVE X'3028204C4654202020202020' &
                X'202020202020202020203120' &
                X'2020233030303039303134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 START-TRANS-ID, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 CF-FORMS-PRINTER-ID, 
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
           
001483
001484      GO TO 0400-WRITE-CLAIM.
001485
001486 0385-TERMID-ERROR.
001487     MOVE ER-0412                TO EMI-ERROR.
001488     MOVE -1                     TO ALTPRTL.
001489     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001490     GO TO 8200-SEND-DATAONLY.
001491
001492 0390-TRANS-ERROR.
001493     MOVE ER-0413                TO EMI-ERROR.
001494     MOVE -1                     TO FILETOL.
001495     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001496     GO TO 8200-SEND-DATAONLY.
001497
001498 0400-WRITE-CLAIM.
001499     IF FILETOL = 0  AND
001500*       MCRFLML = 0  AND
001501        ACTCDL  = 0  AND
001502        ASSOCL  = 0  AND
001503        CLOANL  = 0  AND
001504        TOTINTL = 0  AND
001505        YESNOSWL = 0
001506         GO TO 0440-UPDATE-DONE.
001507
001508     
      * EXEC CICS HANDLE CONDITION
001509*        NOTFND (0450-NOT-FOUND)
001510*        DUPKEY (0440-UPDATE-DONE)
001511*    END-EXEC.
      *    MOVE '"$I$                  ! & #00009046' TO DFHEIV0
           MOVE X'222449242020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303039303436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001512
001513     MOVE 'ELMSTR  '             TO FILE-ID.
001514
001515     
      * EXEC CICS REWRITE
001516*        DATASET   ('ELMSTR')
001517*        FROM      (CLAIM-MASTER)
001518*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&& L                  %   #00009053' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039303533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001519
001520 0440-UPDATE-DONE.
001521     IF FILETOL  > 0 OR
001522*       MCRFLML  > 0 OR
001523        PRTOPTL  > 0 OR
001524        FORMATL  > 0 OR
001525        YESNOSWL > 0 OR
001526        CLOANL   > 0 OR
001527        TOTINTL  > 0 OR
001528        ACTCDL   > 0
001529            MOVE SV-LAST-BY             TO PI-UPDATE-BY
001530            MOVE SV-LAST-HHMMSS         TO PI-UPDATE-HHMMSS.
001531
001532     PERFORM 6050-AUTO-LETTER-WRITER THRU 6050-EXIT.
001533
001534     MOVE ER-0000                TO EMI-ERROR.
001535     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001536
001537     IF CLAIM-IS-PURGED
001538        CONTINUE
001539*        MOVE -1                 TO  MCRFLML
001540     ELSE
001541         MOVE -1                 TO  FILETOL
001542     END-IF
001543
001544     GO TO 8200-SEND-DATAONLY.
001545
001546 0450-NOT-FOUND.
001547     IF FILE-SWITCH = 'MSTR'
001548         MOVE ER-0204            TO EMI-ERROR
001549     ELSE
001550         MOVE ER-0190            TO EMI-ERROR.
001551
001552     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001553
001554     MOVE -1                     TO FILETOL.
001555     GO TO 8200-SEND-DATAONLY.
001556
001557     EJECT
001558 0500-CREATE-TEMP-STORAGE.
001559
001560     IF PI-SAVE-SYS-ID = 'CR'
001561        IF PI-CLOSED
001562           IF PI-BENEFIT-CHANGE OR PI-ERROR-CORRECTION
001563              IF EIBAID = DFHPF7
001564                 MOVE ER-0934            TO EMI-ERROR
001565                 MOVE -1                 TO FILETOL
001566                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001567                 GO TO 8200-SEND-DATAONLY
001568              ELSE
001569              IF EIBAID = DFHPF8
001570                 MOVE ER-0935            TO EMI-ERROR
001571                 MOVE -1                 TO FILETOL
001572                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001573                 GO TO 8200-SEND-DATAONLY.
001574
001575     MOVE EIBCPOSN               TO PI-SAVE-CURSOR.
001576
001577     MOVE LOW-VALUES TO PI-SAVE-FILETO  PI-SAVE-PRTOPT
001578                        PI-SAVE-FORMAT  PI-SAVE-MCRFLM
001579                        PI-SAVE-ACT-CD  PI-SAVE-ASSOC
001580                        TS-PI-SAVE-CLOAN.
001581
001582     MOVE FILETOI                TO PI-SAVE-FILETO.
001583*    MOVE MCRFLMI                TO PI-SAVE-MCRFLM.
001584     MOVE PRTOPTI                TO PI-SAVE-PRTOPT.
001585     MOVE FORMATI                TO PI-SAVE-FORMAT.
001586     MOVE ACTCDI                 TO PI-SAVE-ACT-CD.
001587     MOVE CLOANI                 TO TS-PI-SAVE-CLOAN
001588
001589     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
001590         MOVE ASSOCI             TO PI-SAVE-ASSOC.
001591
001592     MOVE PROGRAM-INTERFACE-BLOCK TO TS-PI-AREA
001593
001594     
      * EXEC CICS WRITEQ TS
001595*        QUEUE    (QID)
001596*        FROM     (PROGRAM-INTERFACE-BLOCK)
001597*        FROM     (TS-AREA)
001598*        LENGTH   (PI-COMM-LENGTH)
001599*        LENGTH   (TS-LENGTH)
001600*    END-EXEC.
      *    MOVE '*"     L              ''   #00009132' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303039313332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 TS-AREA, 
                 TS-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001601
001602 0500-PF-CHECKS.
001603
001604     IF EIBAID = DFHPF3
001605         IF PI-SAVE-SYS-ID = 'CV'
001606             MOVE XCTL-EM131     TO PGM-NAME
001607             GO TO 9300-XCTL
001608         ELSE
001609             MOVE XCTL-131       TO PGM-NAME
001610             GO TO 9300-XCTL.
001611
001612     IF EIBAID = DFHPF4
001613         MOVE XCTL-153           TO PGM-NAME
001614         GO TO 9300-XCTL.
001615
001616     IF EIBAID = DFHPF5
001617         IF PI-SAVE-SYS-ID = 'CV'
001618             MOVE THIS-PGM       TO PI-RETURN-TO-PROGRAM
001619             MOVE XCTL-EM1273    TO PGM-NAME
001620             GO TO 9300-XCTL
001621         ELSE
001622             MOVE XCTL-1273      TO PGM-NAME
001623             GO TO 9300-XCTL.
001624
001625     IF EIBAID = DFHPF6
001626       OR (EIBAID = DFHPF22 AND PI-PFKEY-USED = DFHPF6)
001627         MOVE XCTL-154           TO PGM-NAME
001628         GO TO 9300-XCTL.
001629
001630     IF EIBAID = DFHPF7
001631       OR (EIBAID = DFHPF22 AND PI-PFKEY-USED = DFHPF7)
001632         IF PI-SAVE-SYS-ID = 'CV'
001633             IF PI-SAVE-TYPE = PI-LIFE-OVERRIDE-L1
001634                 MOVE XCTL-EM156    TO  PGM-NAME
001635                 GO TO 9300-XCTL
001636             ELSE
001637                 MOVE XCTL-EM1561   TO  PGM-NAME
001638                 GO TO 9300-XCTL
001639         ELSE
001640             MOVE XCTL-156          TO PGM-NAME
001641             GO TO 9300-XCTL.
001642
001643     IF EIBAID = DFHPF8
001644         MOVE XCTL-151           TO PGM-NAME
001645         GO TO 9300-XCTL.
001646
001647     IF EIBAID = DFHPF9
001648         MOVE XCTL-152           TO PGM-NAME
001649         GO TO 9300-XCTL.
001650
001651     IF EIBAID = DFHPF10
001652         MOVE PI-SAVE-LOW        TO PI-PASS-AREA
001653         MOVE PI-PRIORITY-CD     TO PI-EL142-PRIORITY
001654         MOVE XCTL-142           TO PGM-NAME
001655         GO TO 9300-XCTL.
001656
001657     IF EIBAID = DFHPF11
001658        IF PI-COMPANY-ID = 'DMD'
001659           MOVE XCTL-155           TO PGM-NAME
001660           GO TO 9300-XCTL
001661         ELSE
001662           MOVE XCTL-1503          TO PGM-NAME
001663           GO TO 9300-XCTL.
001664
001665     IF EIBAID = DFHPF13
001666         MOVE PI-SAVE-FORM       TO PI-PASS-AREA
001667         MOVE XCTL-1504          TO PGM-NAME
001668         GO TO 9300-XCTL.
001669
001670     IF EIBAID = DFHPF14
001671         MOVE XCTL-141           TO PGM-NAME
001672         GO TO 9300-XCTL.
001673
001674     IF EIBAID = DFHPF17
001675         MOVE XCTL-162           TO PGM-NAME
001676         GO TO 9300-XCTL.
001677
001678     IF EIBAID = DFHPF18
001679         MOVE PI-PRIORITY-CD     TO PI-EL142-PRIORITY
001680         MOVE XCTL-1501          TO PGM-NAME
001681         GO TO 9300-XCTL.
001682
001683     IF EIBAID = DFHPF19
001684         IF PI-SAVE-SYS-ID = 'CV'
001685             MOVE THIS-PGM       TO PI-RETURN-TO-PROGRAM
001686             MOVE XCTL-EM1276    TO PGM-NAME
001687             GO TO 9300-XCTL
001688         ELSE
001689         IF PI-COMPANY-ID = 'DMD'
001690             MOVE 'EL401DMD'     TO PGM-NAME
001691             GO TO 9300-XCTL
001692          ELSE
001693             MOVE XCTL-1279      TO PGM-NAME
001694             GO TO 9300-XCTL.
001695
001696     IF EIBAID = DFHPF20
001697         MOVE PI-PRIORITY-CD     TO PI-EL142-PRIORITY
001698         MOVE XCTL-1502          TO PGM-NAME
001699         GO TO 9300-XCTL.
001700
001701     IF EIBAID = DFHPF21
001702         MOVE XCTL-1284          TO PGM-NAME
001703         GO TO 9300-XCTL.
001704
001705     EJECT
001706 0600-RECOVER-TEMP-STORAGE.
001707
001708     MOVE PI-CONTROL-IN-PROGRESS TO SAVE-CONTROL.
001709
001710     
      * EXEC CICS HANDLE CONDITION
001711*        QIDERR   (0690-QIDERR)
001712*    END-EXEC
      *    MOVE '"$N                   ! '' #00009248' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303039323438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001713
001714     
      * EXEC CICS READQ TS
001715*        QUEUE    (QID)
001716*        INTO     (PROGRAM-INTERFACE-BLOCK)
001717*        INTO     (TS-AREA)
001718*        LENGTH   (PI-COMM-LENGTH)
001719*        LENGTH   (TS-LENGTH)
001720*    END-EXEC
      *    MOVE '*$I    L              ''   #00009252' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303039323532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 TS-AREA, 
                 TS-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001721
001722     MOVE TS-PI-AREA        TO PROGRAM-INTERFACE-BLOCK
001723
001724     
      * EXEC CICS DELETEQ TS
001725*        QUEUE   (QID)
001726*    END-EXEC.
      *    MOVE '*&                    #   #00009262' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303039323632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001727
001728     IF RETURNED-FROM NOT = XCTL-EM1273
001729         MOVE SAVE-CONTROL       TO PI-CONTROL-IN-PROGRESS.
001730
001731     MOVE LOW-VALUES             TO EL150AO.
001732
001733*    IF PI-SAVE-MCRFLM NOT = LOW-VALUES
001734*        MOVE PI-SAVE-MCRFLM     TO MCRFLMO.
001735
001736     IF PI-SAVE-FILETO NOT = LOW-VALUES
001737         MOVE PI-SAVE-FILETO     TO FILETOO.
001738
001739     IF TS-PI-SAVE-CLOAN NOT = LOW-VALUES
001740        MOVE TS-PI-SAVE-CLOAN   TO CLOANO
001741     END-IF
001742
001743     IF PI-SAVE-PRTOPT NOT = LOW-VALUES
001744         MOVE PI-SAVE-PRTOPT     TO PRTOPTO
001745         MOVE AL-UANON           TO PRTOPTA.
001746
001747     IF PI-SAVE-FORMAT NOT = LOW-VALUES
001748         MOVE PI-SAVE-FORMAT     TO FORMATO
001749         MOVE AL-UANON           TO FORMATA.
001750
001751     IF PI-SAVE-ACT-CD NOT = LOW-VALUES
001752         MOVE PI-SAVE-ACT-CD     TO ACTCDO.
001753
001754     IF PI-SAVE-ASSOC  NOT = LOW-VALUES
001755         MOVE PI-SAVE-ASSOC      TO ASSOCO.
001756
001757     MOVE 'N'                    TO DIRECTION-SWITCH.
001758
001759     MOVE SPACES TO PI-PFKEY-USED.
001760     MOVE 0      TO PI-FORCE-COUNT.
001761
001762     GO TO 1000-SHOW-CLAIM.
001763
001764 0690-QIDERR.
001765     MOVE ER-0033                TO EMI-ERROR.
001766     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001767     MOVE LOW-VALUES             TO EL150AO.
001768     MOVE -1                     TO FILETOL.
001769     GO TO 8100-SEND-INITIAL-MAP.
001770
001771     EJECT
001772 1000-SHOW-CLAIM.
001773
001774     IF CLMNOL > 0
001775         MOVE CLMNOI             TO PI-CLAIM-NO.
001776
001777     IF CARRL > 0
001778         MOVE CARRI              TO PI-CARRIER.
001779
001780     IF CERTNOL > 0
001781         MOVE CERTNOI            TO PI-CERT-PRIME.
001782
001783     IF SUFXL > 0
001784         MOVE SUFXI              TO PI-CERT-SFX.
001785
001786     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
001787     MOVE PI-CARRIER             TO MSTR-CARRIER.
001788     MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.
001789     MOVE PI-CERT-NO             TO MSTR-CERT-NO.
001790     MOVE 'MSTR'                 TO FILE-SWITCH.
001791
001792 1000-READ-CLAIM.
001793
001794     
      * EXEC CICS HANDLE CONDITION
001795*        NOTFND   (1100-SHOW-RECORD-NOT-FOUND)
001796*    END-EXEC.
      *    MOVE '"$I                   ! ( #00009332' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303039333332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001797
001798     
      * EXEC CICS READ
001799*        DATASET   (PI-CURRENT-FILE)
001800*        RIDFLD    (ELMSTR-KEY)
001801*        INTO      (CLAIM-MASTER)
001802*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00009336' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039333336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001803
001804 1000-CONTINUE-WITH-RETRIEVED.
001805
001806     if (pi-company-id = 'CID' OR 'DCC' OR 'AHL' or 'CAP'
001807           OR 'FNL')
001808        and (cl-priority-cd = '8')
001809        move 'S'                 to pi-priority-ind
001810     end-if
001811
001812     IF PI-COMPANY-ID = 'DMD'
001813         IF HIGHEST-PRIORITY
001814             MOVE 'S'            TO PI-PRIORITY-IND
001815             MOVE ER-0933        TO EMI-ERROR
001816             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001817         ELSE
001818             IF NOT CONFIDENTIAL-DATA
001819                 MOVE LOW-VALUES TO PI-PRIORITY-IND.
001820
001821     IF PI-COMPANY-ID = 'DMD'
001822             AND
001823        PI-ASS-PROCESSING
001824             AND
001825        PI-PREV-PRIME-CERT-NO = CL-PRIME-CERT-NO
001826         NEXT SENTENCE
001827     ELSE
001828         MOVE ELMSTR-KEY         TO PI-PREV-CLAIM
001829         MOVE CL-PRIME-CERT-NO   TO PI-PREV-PRIME-CERT-NO.
001830
001831     MOVE CL-LAST-MAINT-USER     TO PI-UPDATE-BY.
001832     MOVE CL-PRIORITY-CD         TO PI-PRIORITY-CD.
001833
001834     IF CL-LAST-MAINT-HHMMSS IS NOT NUMERIC
001835         MOVE EIBTIME            TO PI-UPDATE-HHMMSS
001836     ELSE
001837         MOVE CL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
001838
001839     MOVE CL-TRAILER-SEQ-CNT     TO PI-SAVE-CNT.
001840
001841     IF CL-YESNOSW = 'Y' OR 'N'
001842         MOVE CL-YESNOSW         TO YESNOSWO
001843     ELSE
001844         MOVE 'Y'                TO YESNOSWO
001845     END-IF.
001846
001847     MOVE CL-CLAIM-NO            TO CLMNOO
001848                                    PI-CLAIM-NO.
001849     MOVE CL-CARRIER             TO CARRO
001850                                    PI-CARRIER.
001851     MOVE CL-CERT-PRIME          TO CERTNOO
001852                                    PI-CERT-PRIME.
001853     MOVE CL-CERT-SFX            TO SUFXO
001854                                    PI-CERT-SFX.
001855     MOVE CL-CCN                 TO CRDCARDO
001856
001857     MOVE CL-SYSTEM-IDENTIFIER   TO PI-SAVE-SYS-ID.
001858
001859     IF CL-SYSTEM-IDENTIFIER = 'CV'
001860         MOVE CL-CV-REFERENCE-NO TO PI-MP-REFERENCE-NO.
001861
001862     IF CL-ASSOC-CERT-TOTAL = +0 OR +1
001863         MOVE SPACES                 TO PRIMCRTO
001864                                        PRIMSFXO
001865                                        PRIMHDGO
001866                                        SEQUO
001867         MOVE AL-SADOF               TO PRIMCRTA
001868                                        PRIMSFXA
001869                                        PRIMHDGA
001870                                        SEQUA
001871     ELSE
001872         MOVE CL-ASSOC-CERT-SEQU     TO WS-CUR-SEQU
001873         MOVE CL-ASSOC-CERT-TOTAL    TO WS-OF-SEQU
001874         MOVE WS-CLAIM-SEQUENCE      TO SEQUO
001875         MOVE CL-PRIME-CERT-PRIME    TO PRIMCRTO
001876         MOVE CL-PRIME-CERT-SFX      TO PRIMSFXO
001877         MOVE 'PRIME CERT :'         TO PRIMHDGO
001878         MOVE AL-SABON               TO PRIMCRTA
001879                                        PRIMSFXA
001880                                        PRIMHDGA
001881                                        SEQUA.
001882
001883     MOVE CL-CLAIM-TYPE          TO PI-SAVE-TYPE.
001884     MOVE CL-CERT-GROUPING       TO PI-GROUPING.
001885     MOVE CL-CERT-STATE          TO PI-STATE.
001886
001887     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
001888         MOVE CL-CERT-ACCOUNT    TO  PI-ACCOUNT
001889         MOVE CL-CURRENT-ACCOUNT TO  ACCTO
001890     ELSE
001891         MOVE CL-CERT-ACCOUNT    TO  PI-ACCOUNT
001892                                     ACCTO.
001893
001894     MOVE CL-CERT-EFF-DT         TO PI-CERT-EFF-DT.
001895
001896     EVALUATE TRUE
001897
001898        WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
001899           MOVE PI-AH-OVERRIDE-L6
001900                                 TO CLMTYPO
001901
001902        WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
001903           MOVE PI-LIFE-OVERRIDE-L6
001904                                 TO CLMTYPO
001905
001906        WHEN CL-CLAIM-TYPE = 'I'
001907           MOVE '  IU  '         TO CLMTYPO
001908
001909        WHEN CL-CLAIM-TYPE = 'G'
001910           MOVE ' GAP  '         TO CLMTYPO
001911
001912        WHEN CL-CLAIM-TYPE = 'F'
001913           MOVE ' FAM  '         TO CLMTYPO
001914
001915        WHEN CL-CLAIM-TYPE = 'B'
001916           MOVE ' BRV  '         TO CLMTYPO
001917        WHEN CL-CLAIM-TYPE = 'H'
001918           MOVE ' HSP '          TO CLMTYPO
001919
001920        WHEN CL-CLAIM-TYPE = 'O'
001921           MOVE ' OTH  '         TO CLMTYPO
001922
001923     END-EVALUATE
001924
001925     IF CLAIM-IS-OPEN
001926         MOVE ' OPEN'            TO CLMSTATO
001927     ELSE
001928         MOVE 'CLOSED'           TO CLMSTATO.
001929
001930*    IF PI-COMPANY-ID = 'AIG' OR 'AUK' OR 'CIG' OR 'CUK'
001931*        IF (CLAIM-IS-CLOSED)  AND
001932*           (CL-LAST-CLOSE-REASON = '2')
001933*            MOVE 'DENIED'       TO CLMSTATO.
001934
001935     MOVE CL-INSURED-LAST-NAME   TO LSTNMEO.
001936     MOVE CL-INSURED-1ST-NAME    TO FSTNMEO.
001937     MOVE CL-INSURED-MID-INIT    TO MINITO.
001938
001939     IF CL-NO-OF-PMTS-MADE = +0 AND
001940        CL-TOTAL-PAID-AMT = +0
001941         NEXT SENTENCE
001942     ELSE
001943         IF CL-PAID-THRU-DT NOT = LOW-VALUES
001944             MOVE CL-PAID-THRU-DT        TO  DC-BIN-DATE-1
001945             MOVE ' '                    TO  DC-OPTION-CODE
001946             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001947             MOVE DC-GREG-DATE-1-EDIT    TO  PDTHRUO
001948             IF PI-USES-PAID-TO
001949                 MOVE CL-PAID-THRU-DT    TO  DC-BIN-DATE-1
001950                 MOVE '6'                TO  DC-OPTION-CODE
001951                 MOVE +1                 TO  DC-ELAPSED-DAYS
001952                 MOVE +0                 TO  DC-ELAPSED-MONTHS
001953                 PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001954                 MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO.
001955
001956     IF CL-INCURRED-DT NOT = LOW-VALUES
001957         MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1
001958         MOVE ' '                TO DC-OPTION-CODE
001959         MOVE +0                 TO DC-ELAPSED-DAYS
001960                                    DC-ELAPSED-MONTHS
001961         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001962         MOVE DC-GREG-DATE-1-EDIT TO INCURO.
001963
001964     IF CL-DENIAL-TYPE NOT = SPACES AND LOW-VALUES
001965        EVALUATE CL-DENIAL-TYPE
001966           WHEN '1'
001967              MOVE 'DENIAL STATUS - DENIAL       '
001968                                 TO DENTYPO
001969           WHEN '2'
001970              MOVE 'DENIAL STATUS - RESCISSION   '
001971                                 TO DENTYPO
001972           WHEN '3'
001973              MOVE 'DENIAL STATUS - REFORMATION  '
001974                                 TO DENTYPO
001975           WHEN '4'
001976              MOVE 'DENIAL STATUS - REF TO RESC  '
001977                                 TO DENTYPO
001978           WHEN '5'
001979              MOVE 'DENIAL STATUS - RECONSIDERED '
001980                                 TO DENTYPO
001981        END-EVALUATE
001982        MOVE AL-SANOF            TO DENTYPA
001983     END-IF
001984
001985     IF CL-NEXT-AUTO-PAY-DT NOT = LOW-VALUES
001986         MOVE CL-NEXT-AUTO-PAY-DT TO DC-BIN-DATE-1
001987         MOVE ' '                TO DC-OPTION-CODE
001988         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001989         MOVE DC-GREG-DATE-1-EDIT TO NXTAUTOO
001990         IF PI-USES-PAID-TO
001991             MOVE CL-NEXT-AUTO-PAY-DT   TO  DC-BIN-DATE-1
001992             MOVE '6'                   TO  DC-OPTION-CODE
001993             MOVE +1                    TO  DC-ELAPSED-DAYS
001994             MOVE +0                    TO  DC-ELAPSED-MONTHS
001995             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
001996             IF NO-CONVERSION-ERROR
001997                 MOVE DC-GREG-DATE-1-EDIT   TO  NXTAUTOO
001998             ELSE
001999                 MOVE LOW-VALUES            TO  NXTAUTOO.
002000
002001     IF CL-FILE-ESTABLISH-DT NOT = LOW-VALUES
002002         MOVE CL-FILE-ESTABLISH-DT      TO  DC-BIN-DATE-1
002003         MOVE ' '                       TO  DC-OPTION-CODE
002004         MOVE +0                        TO  DC-ELAPSED-DAYS
002005                                            DC-ELAPSED-MONTHS
002006         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002007         IF NO-CONVERSION-ERROR
002008             MOVE DC-GREG-DATE-1-EDIT   TO  ESTABDTO
002009         ELSE
002010             MOVE SPACES                TO  ESTABDTO.
002011
002012*    MOVE CL-MICROFILM-NO        TO MCRFLMO.
002013     MOVE CL-FILE-LOCATION       TO FILETOO.
002014
002015     IF CL-CERT-EFF-DT NOT = LOW-VALUES
002016         MOVE CL-CERT-EFF-DT     TO DC-BIN-DATE-1
002017         MOVE ' '                TO DC-OPTION-CODE
002018         MOVE +0                 TO DC-ELAPSED-DAYS
002019                                    DC-ELAPSED-MONTHS
002020         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002021         MOVE DC-GREG-DATE-1-EDIT TO EFFECTO.
002022
002023     IF CL-TOTAL-INT-PAID NOT NUMERIC
002024        MOVE +0                  TO CL-TOTAL-INT-PAID
002025     END-IF
002026     MOVE CL-TOTAL-INT-PAID      TO TOTINTO
002027     MOVE CL-TOTAL-PAID-AMT      TO TOTPAIDO.
002028     MOVE CL-TOTAL-PAID-AMT      TO PI-TOTAL-PAID.
002029
002030     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002031         MOVE CL-CURRENT-STATE   TO  STATEO
002032     ELSE
002033     IF PI-COMPANY-ID = 'DMD'
002034         MOVE PI-COMPANY-CD          TO CERT-COMP-CD
002035         MOVE CL-CERT-CARRIER        TO CERT-CARRIER
002036         MOVE CL-CERT-GROUPING       TO CERT-GROUPING
002037         MOVE CL-CERT-STATE          TO CERT-STATE
002038         MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT
002039         MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT
002040         MOVE CL-CERT-NO             TO CERT-CERT-NO
002041         MOVE 'CERT'                 TO FILE-SWITCH
002042         
      * EXEC CICS READ
002043*            DATASET   ('ELCERT')
002044*            SET       (ADDRESS OF CERTIFICATE-MASTER)
002045*            RIDFLD    (ELCERT-KEY)
002046*        END-EXEC
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        E          (   #00009580' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039353830' TO DFHEIV0
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002047         MOVE CM-RESIDENT-STATE  TO  STATEO
002048     ELSE
002049         MOVE CL-CERT-STATE      TO  STATEO.
002050
002051     MOVE CL-ACTIVITY-CODE       TO  ACTCDO.
002052     MOVE CL-ACTIVITY-MAINT-TYPE TO  ACTTYPO.
002053
002054     IF CL-ACTIVITY-MAINT-DT = SPACES OR LOW-VALUES
002055         MOVE SPACES                     TO  ACTDTO
002056     ELSE
002057         MOVE CL-ACTIVITY-MAINT-DT       TO  DC-BIN-DATE-1
002058         MOVE ' '                        TO  DC-OPTION-CODE
002059         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002060         IF NO-CONVERSION-ERROR
002061             MOVE DC-GREG-DATE-1-EDIT    TO  ACTDTO
002062         ELSE
002063             MOVE SPACES                 TO  ACTDTO.
002064
002065     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002066         MOVE 'ASSOC:'               TO  ASSHDGO
002067         MOVE AL-SABON               TO  ASSHDGA
002068         MOVE CL-ASSOCIATES          TO  ASSOCO
002069     ELSE
002070         MOVE SPACES                 TO  ASSHDGO
002071         MOVE AL-SADOF               TO  ASSOCA.
002072
002073     MOVE CL-CLAIM-STATUS        TO PI-STATUS-IND.
002074     MOVE CL-LAST-CLOSE-REASON   TO PI-LAST-CLOSE-REASON-IND.
002075
002076     IF PI-COMPANY-ID = 'DMD'
002077             AND
002078         ((PI-ASS-PROCESSING  AND
002079           PI-PREV-PRIME-CERT-NO = CL-PRIME-CERT-NO)
002080                 OR
002081         ((PI-CLOSED AND
002082          (PI-BENEFIT-CHANGE OR PI-ERROR-CORRECTION))
002083                 OR
002084           CL-CERT-NO NOT = CL-PRIME-CERT-NO))
002085         PERFORM 1200-GET-ASS-TOTALS THRU 1200-EXIT
002086     ELSE
002087         MOVE LOW-VALUES             TO PI-LAST-PF-KEY-IND
002088                                        PI-ASSOCIATED-CERTS-TABLE
002089                                        PI-ASSOCIATED-PROCESS-IND
002090         MOVE SPACES                 TO AMTPDO
002091                                        DAYSPDO
002092                                        CLMTOTHO
002093                                        DAYSPDHO
002094*01745          MOVE ZEROS                  TO PI-TOTAL-PAID
002095         MOVE ZEROS                  TO
002096                                        PI-DAYS-PAID
002097                                        PI-LAST-SEQ-NO
002098                                        PI-ASS-NDX.
002099
002100     
      * EXEC CICS HANDLE CONDITION
002101*        NOTFND    (1005-END-BUILD-DIAGNOSIS)
002102*    END-EXEC.
      *    MOVE '"$I                   ! ) #00009638' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303039363338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002103
002104     MOVE CL-CONTROL-PRIMARY  TO ELTRLR-KEY.
002105     MOVE +90                 TO TRLR-SEQ-NO.
002106     MOVE 'TRLR'              TO FILE-SWITCH.
002107
002108     
      * EXEC CICS READ
002109*        DATASET   ('ELTRLR')
002110*        RIDFLD    (ELTRLR-KEY)
002111*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
002112*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (   #00009646' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039363436' TO DFHEIV0
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
           
002113
002114     MOVE AT-INFO-LINE-1         TO DIAGO.
002115
002116 1005-END-BUILD-DIAGNOSIS.
002117
002118     MOVE CL-CONTROL-PRIMARY  TO ELTRLR-KEY.
002119     MOVE +91                 TO TRLR-SEQ-NO.
002120     MOVE 'TRLR'              TO FILE-SWITCH.
002121
002122     
      * EXEC CICS READ
002123*        DATASET   ('ELTRLR')
002124*        RIDFLD    (ELTRLR-KEY)
002125*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
002126*        RESP      (WS-RESPONSE)
002127*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00009660' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039363630' TO DFHEIV0
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
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002128
002129     IF WS-RESP-NORMAL
002130        MOVE AT-INFO-LINE-1      TO CLOANO
002131     ELSE
002132        MOVE SPACES              TO CLOANO
002133     END-IF
002134
002135     IF CL-SYSTEM-IDENTIFIER = 'CV'
002136         PERFORM 1500-READ-CONVENIENCE-FILES THRU 1500-EXIT
002137         GO TO 1050-SHOW-CONTINUE.
002138
002139     
      * EXEC CICS HANDLE CONDITION
002140*        NOTFND   (1100-SHOW-RECORD-NOT-FOUND)
002141*    END-EXEC.
      *    MOVE '"$I                   ! * #00009677' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303039363737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002142
002143     MOVE PI-COMPANY-CD          TO CERT-COMP-CD.
002144     MOVE CL-CERT-CARRIER        TO CERT-CARRIER.
002145     MOVE CL-CERT-GROUPING       TO CERT-GROUPING.
002146     MOVE CL-CERT-STATE          TO CERT-STATE.
002147     MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
002148     MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.
002149     MOVE CL-CERT-NO             TO CERT-CERT-NO.
002150     MOVE 'CERT'                 TO FILE-SWITCH.
002151
002152     
      * EXEC CICS READ
002153*        DATASET   ('ELCERT')
002154*        SET       (ADDRESS OF CERTIFICATE-MASTER)
002155*        RIDFLD    (ELCERT-KEY)
002156*    END-EXEC.
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        E          (   #00009690' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039363930' TO DFHEIV0
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
           
002157
002158     MOVE 'Y'                    TO WS-CERT-READ-SW.
002159     MOVE LOW-VALUES             TO ERACCT-KEY.
002160     MOVE PI-COMPANY-CD          TO ACCT-COMP-CD.
002161     MOVE CL-CERT-CARRIER        TO ACCT-CARRIER.
002162     MOVE CL-CERT-GROUPING       TO ACCT-GROUPING.
002163     MOVE CL-CERT-STATE          TO ACCT-STATE.
002164     MOVE CL-CERT-ACCOUNT        TO ACCT-ACCOUNT.
002165     MOVE CL-CERT-EFF-DT         TO ACCT-EXP-DT.
002166     MOVE 'ACCT'                 TO FILE-SWITCH.
002167
002168     MOVE ERACCT-PARTIAL-KEY     TO WS-ERACCT-SAVE-KEY.
002169     MOVE SPACES                 TO WS-ERACCT-HOLD-RECORD.
002170
002171     
      * EXEC CICS HANDLE CONDITION
002172*         NOTFND   (1005-NO-ERACCT)
002173*         ENDFILE  (1005-NO-ERACCT)
002174*    END-EXEC.
      *    MOVE '"$I''                  ! + #00009709' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303039373039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002175
002176     
      * EXEC CICS STARTBR
002177*         DATASET   ('ERACCT')
002178*         RIDFLD    (ERACCT-KEY)
002179*         GTEQ
002180*    END-EXEC.
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009714' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303039373134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002181
002182 1005-READ-NEXT-ACCOUNT.
002183
002184     
      * EXEC CICS READNEXT
002185*         DATASET   ('ERACCT')
002186*         SET       (ADDRESS OF ACCOUNT-MASTER)
002187*         RIDFLD    (ERACCT-KEY)
002188*    END-EXEC.
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009722' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303039373232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002189
002190     IF WS-ERACCT-SAVE-KEY NOT = ERACCT-PARTIAL-KEY
002191        IF WS-ERACCT-HOLD-RECORD = SPACES
002192           GO TO 1005-NO-ERACCT
002193        ELSE
002194           MOVE WS-ERACCT-HOLD-RECORD TO ACCOUNT-MASTER
002195           GO TO 1005-CONTINUE.
002196
002197     IF ACCT-EXP-DT = HIGH-VALUES
002198        NEXT SENTENCE
002199     ELSE
002200        MOVE ACCOUNT-MASTER      TO WS-ERACCT-HOLD-RECORD
002201        GO TO 1005-READ-NEXT-ACCOUNT.
002202
002203 1005-CONTINUE.
002204
002205     IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
002206        MOVE ZEROS               TO AM-3RD-PARTY-NOTIF-LEVEL.
002207
002208     IF AM-3RD-PARTY-NOTIF-LEVEL > ZERO
002209        MOVE 'THIRD PARTY'       TO NOTIFYO.
002210
002211 1005-NO-ERACCT.
002212
002213     IF CM-INSURED-LAST-NAME EQUAL CL-INSURED-LAST-NAME  AND
002214        CM-INSURED-FIRST-NAME EQUAL CL-INSURED-1ST-NAME  AND
002215        CM-INSURED-INITIAL2 EQUAL CL-INSURED-MID-INIT
002216            MOVE 'N' TO PI-JOINT-INSURED-IND
002217     ELSE
002218            MOVE 'Y' TO PI-JOINT-INSURED-IND
002219     END-IF.
002220
002221     IF CM-SING-PRM
002222         MOVE 'SING PRM'         TO PRMTYPEO
002223     ELSE
002224         IF CM-O-B-COVERAGE
002225             MOVE 'OUT BAL'      TO PRMTYPEO
002226         ELSE
002227             MOVE 'OPEN END'     TO PRMTYPEO.
002228
002229     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
002230        GO TO 1006-CALC-LIFE.
002231
002232     MOVE 'MO. BENEFIT :'       TO BENECAPO.
002233     MOVE CM-CERT-EFF-DT        TO CP-CERT-EFF-DT.
002234     MOVE CM-LOAN-1ST-PMT-DT    TO CP-FIRST-PAY-DATE.
002235
002236     IF PI-COMPANY-ID = 'DMD'
002237         PERFORM 9830-DMD-REMAINING-TERM THRU 9830-EXIT
002238         MOVE W-PAYMENTS         TO NOPMTSO
002239         GO TO 1005-RTRM-CONT
002240     ELSE
002241         MOVE SAVE-BIN-DATE      TO CP-VALUATION-DT
002242         MOVE SPACES             TO W-PAYMENTS
002243         MOVE CL-NO-OF-PMTS-MADE TO W-PYMTS
002244         MOVE W-PAYMENTS         TO NOPMTSO.
002245
002246     IF CM-AH-ORIG-TERM = +0
002247        MOVE +1                 TO CP-ORIGINAL-TERM
002248     ELSE
002249        MOVE CM-AH-ORIG-TERM    TO CP-ORIGINAL-TERM.
002250
002251     MOVE PI-REM-TRM-CALC-OPTION
002252                                TO CP-REM-TRM-CALC-OPTION.
002253     MOVE '4'                   TO CP-REM-TERM-METHOD.
002254     MOVE PI-COMPANY-ID         TO CP-COMPANY-ID
002255     PERFORM 8900-GET-FREE-LOOK THRU 8900-EXIT.
002256     PERFORM 9800-LINK-REM-TERM.
002257     MOVE CP-REMAINING-TERM-3   TO WST-REM.
002258
002259 1005-RTRM-CONT.
002260
002261     move zeros                  to pi-max-benefit
002262     if pi-company-id = 'DCC' or 'CAP'
002263        and am-dcc-product-code not = '   '
002264        move ' '                 to ws-pdef-record-sw
002265        perform 3997-get-erpdef  thru 3997-exit
002266        if pdef-found
002267           move cl-insured-birth-dt
002268                                 to dc-bin-date-1
002269           move cl-incurred-dt   to dc-bin-date-2
002270           move '1'              to dc-option-code
002271           PERFORM 9700-LINK-DATE-CONVERT
002272                                 THRU 9700-EXIT
002273           compute ws-att-age =
002274              dc-elapsed-months / 12
002275           move zeros to dc-elapsed-months dc-elapsed-days
002276
002277           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
002278              (P1 > +11)
002279              OR (PD-PROD-CODE (P1) = cl-claim-type
002280                   AND PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
002281           END-PERFORM
002282           IF P1 < +12
002283              if pd-ben-pct (p1) not numeric
002284                 move zeros      to pd-ben-pct (p1)
002285              end-if
002286              if pd-ben-pct (p1) = zeros
002287                 move +1         to ws-work-ben-pct
002288              else
002289                 move pd-ben-pct (p1)
002290                                 to ws-work-ben-pct
002291              end-if
002292              compute cm-ah-benefit-amt =
002293                 cm-ah-benefit-amt * ws-work-ben-pct
002294              MOVE PD-MAX-AMT (P1)
002295                                 TO pi-max-benefit
002296           END-IF
002297        end-if
002298     end-if
002299     if (pi-max-benefit not = zeros)
002300        and (cm-ah-benefit-amt > pi-max-benefit)
002301        move pi-max-benefit      to beneo
002302     else
002303        MOVE CM-AH-BENEFIT-AMT   TO BENEO
002304     end-if
002305
002306     MOVE CM-POLICY-FORM-NO     TO PI-SAVE-FORM.
002307     MOVE CM-STATE              TO PI-CR-STATE.
002308     MOVE WS-TERMS              TO TERMSO.
002309     MOVE CM-AH-ORIG-TERM       TO WST-ORIG
002310                                   DC-ELAPSED-MONTHS
002311     MOVE WS-TERMS              TO TERMSO.
002312     MOVE CM-AH-CURRENT-STATUS TO WS-STATUS.
002313
002314     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002315         MOVE CM-LOAN-1ST-PMT-DT     TO  DC-BIN-DATE-1
002316         MOVE '6'                    TO  DC-OPTION-CODE
002317         MOVE '1'                    TO  DC-END-OF-MONTH
002318         MOVE +0                     TO  DC-ELAPSED-DAYS
002319                                         DC-ODD-DAYS-OVER
002320         COMPUTE DC-ELAPSED-MONTHS = CM-AH-ORIG-TERM - +1
002321         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002322         IF NO-CONVERSION-ERROR
002323             MOVE DC-GREG-DATE-1-EDIT    TO  EXPIREO
002324         ELSE
002325             MOVE SPACES                 TO  EXPIREO
002326     ELSE
002327         MOVE CM-AH-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
002328         MOVE ' '                    TO  DC-OPTION-CODE
002329         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002330         IF NO-CONVERSION-ERROR
002331             MOVE DC-GREG-DATE-1-EDIT    TO  EXPIREO
002332         ELSE
002333             MOVE SPACES                 TO  EXPIREO.
002334
002335     GO TO 1009-CONTINUE-BUILD.
002336
002337 1006-CALC-LIFE.
002338
002339     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
002340     MOVE '4'                    TO CNTL-REC-TYPE.
002341     MOVE SPACES                 TO CNTL-ACCESS.
002342     MOVE CM-LF-BENEFIT-CD       TO CNTL-BEN-NO.
002343     MOVE +0                     TO CNTL-SEQ-NO.
002344     MOVE 'CNTL'                 TO FILE-SWITCH.
002345
002346     
      * EXEC CICS HANDLE CONDITION
002347*         NOTFND    (1008-NOTFND)
002348*         ENDFILE   (1008-NOTFND)
002349*    END-EXEC.
      *    MOVE '"$I''                  ! , #00009884' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303039383834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002350
002351     
      * EXEC CICS READ
002352*        DATASET   ('ELCNTL')
002353*        SET       (ADDRESS OF CONTROL-FILE)
002354*        RIDFLD    (ELCNTL-KEY)
002355*        GTEQ
002356*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00009889' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303039383839' TO DFHEIV0
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
           
002357
002358     IF PI-COMPANY-ID NOT = CF-COMPANY-ID OR
002359        CF-RECORD-TYPE NOT = '4'
002360          GO TO 1008-NOTFND.
002361
002362     MOVE +1 TO WS-INDEX.
002363
002364 1007-TABLE-LOOKUP.
002365
002366     IF CM-LF-BENEFIT-CD = CF-BENEFIT-CODE (WS-INDEX)
002367        MOVE CF-LF-COVERAGE-TYPE (WS-INDEX) TO CP-BENEFIT-TYPE
002368                                               WS-LF-COVERAGE-TYPE
002369        MOVE CF-CO-EARNINGS-CALC (WS-INDEX) TO WS-EARNING-METHOD
002370        MOVE CF-SPECIAL-CALC-CD (WS-INDEX)  TO CP-SPECIAL-CALC-CD
002371        GO TO 1008-CONTINUE.
002372
002373     IF CF-BENEFIT-CODE (WS-INDEX) NOT < CF-HI-BEN-IN-REC
002374        GO TO 1008-NOTFND.
002375
002376     IF WS-INDEX < +8
002377        ADD +1 TO WS-INDEX
002378        GO TO 1007-TABLE-LOOKUP.
002379
002380 1008-NOTFND.
002381
002382     MOVE ZEROS TO BENEO.
002383     GO TO 1009-CONTINUE-BUILD.
002384
002385 1008-CONTINUE.
002386
002387     MOVE CM-CERT-EFF-DT        TO CP-CERT-EFF-DT.
002388     MOVE CM-LOAN-1ST-PMT-DT    TO CP-FIRST-PAY-DATE.
002389     MOVE CL-INCURRED-DT        TO CP-VALUATION-DT.
002390
002391     IF CM-LF-ORIG-TERM = +0
002392        MOVE +1                TO CP-ORIGINAL-TERM
002393     ELSE
002394        MOVE CM-LF-ORIG-TERM   TO CP-ORIGINAL-TERM.
002395
002396     MOVE CM-LF-ORIG-TERM      TO WST-ORIG
002397                                  DC-ELAPSED-MONTHS.
002398     MOVE CM-LF-CURRENT-STATUS TO WS-STATUS.
002399
002400     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002401         MOVE CM-LOAN-1ST-PMT-DT     TO  DC-BIN-DATE-1
002402         MOVE '6'                    TO  DC-OPTION-CODE
002403         MOVE '1'                    TO  DC-END-OF-MONTH
002404         MOVE +0                     TO  DC-ELAPSED-DAYS
002405                                         DC-ODD-DAYS-OVER
002406         COMPUTE DC-ELAPSED-MONTHS = CM-LF-ORIG-TERM - +1
002407         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002408         IF NO-CONVERSION-ERROR
002409             MOVE DC-GREG-DATE-1-EDIT    TO  EXPIREO
002410         ELSE
002411             MOVE SPACES                 TO  EXPIREO
002412     ELSE
002413         MOVE CM-LF-LOAN-EXPIRE-DT   TO  DC-BIN-DATE-1
002414         MOVE ' '                    TO  DC-OPTION-CODE
002415         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002416         IF NO-CONVERSION-ERROR
002417             MOVE DC-GREG-DATE-1-EDIT    TO  EXPIREO
002418         ELSE
002419             MOVE SPACES                 TO  EXPIREO.
002420
002421     MOVE PI-REM-TRM-CALC-OPTION
002422                                TO CP-REM-TRM-CALC-OPTION.
002423     MOVE '4'                   TO CP-REM-TERM-METHOD.
002424     MOVE WS-EARNING-METHOD     TO CP-EARNING-METHOD.
002425     MOVE PI-COMPANY-ID         TO CP-COMPANY-ID.
002426     PERFORM 8900-GET-FREE-LOOK THRU 8900-EXIT.
002427     PERFORM 9800-LINK-REM-TERM.
002428
002429     MOVE CP-REMAINING-TERM-3    TO WST-REM.
002430     MOVE SPACES                 TO WST-REM-DAYS-GRP
002431                                    WST-ORIG-DAYS-GRP.
002432     MOVE WS-TERMS               TO TERMSO.
002433
002434     MOVE SPACES                 TO W-PAYMENTS
002435     MOVE CL-NO-OF-PMTS-MADE     TO W-PYMTS
002436     MOVE W-PAYMENTS             TO NOPMTSO.
002437
002438 1009-CALC-REM-AMT.
002439
002440     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
002441     MOVE PI-STATE               TO WS-ST-ACCESS.
002442     MOVE WS-STATE-ACCESS        TO CNTL-ACCESS.
002443     MOVE '3'                    TO CNTL-REC-TYPE.
002444     MOVE +0                     TO CNTL-SEQ-NO.
002445     MOVE 'CNTL'                 TO  FILE-SWITCH.
002446
002447     
      * EXEC CICS HANDLE CONDITION
002448*        NOTFND   (1009-CONTINUE-BUILD)
002449*        ENDFILE  (1009-CONTINUE-BUILD)
002450*    END-EXEC.
      *    MOVE '"$I''                  ! - #00009985' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303039393835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002451
002452     
      * EXEC CICS READ
002453*        DATASET   ('ELCNTL')
002454*        SET       (ADDRESS OF CONTROL-FILE)
002455*        RIDFLD    (ELCNTL-KEY)
002456*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00009990' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039393930' TO DFHEIV0
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
           
002457
002458     MOVE CF-STATE-ABBREVIATION  TO CP-STATE-STD-ABBRV.
002459     MOVE CM-RATE-CLASS          TO CP-CLASS-CODE
002460
002461     move zeros                  to ws-max-tot-ben
002462                                    ws-work-ben-pct
002463
002464     IF CM-LF-CURRENT-STATUS = '6' OR '7' OR '8'
002465        MOVE ZEROS                 TO BENEO
002466     ELSE
002467        MOVE CP-REMAINING-TERM-3   TO CP-REMAINING-TERM
002468        MOVE CM-LF-BENEFIT-AMT     TO CP-ORIGINAL-BENEFIT
002469        MOVE CM-LF-ALT-BENEFIT-AMT TO CP-ALTERNATE-BENEFIT
002470        MOVE CM-LOAN-APR           TO CP-LOAN-APR
002471        MOVE CM-LOAN-TERM          TO CP-LOAN-TERM
002472        MOVE CM-PAY-FREQUENCY      TO CP-PAY-FREQUENCY
002473        if pi-company-id = 'DCC' or 'CAP'
002474           and am-dcc-product-code not = '   '
002475           move ' '              to ws-pdef-record-sw
002476           perform 3997-get-erpdef
002477                                 thru 3997-exit
002478           if pdef-found
002479              move cl-insured-birth-dt
002480                                 to dc-bin-date-1
002481              move cl-incurred-dt
002482                                 to dc-bin-date-2
002483              move '1'           to dc-option-code
002484              PERFORM 9700-LINK-DATE-CONVERT
002485                                 THRU 9700-EXIT
002486              compute ws-att-age =
002487                dc-elapsed-months / 12
002488              move zeros to dc-elapsed-months dc-elapsed-days
002489
002490              PERFORM VARYING P1 FROM +1 BY +1 UNTIL
002491                 (P1 > +11)
002492                 OR (PD-PROD-CODE (P1) = 'L'
002493                   AND PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE )
002494              END-PERFORM
002495              IF P1 < +12
002496                 MOVE PD-MAX-AMT (P1)
002497                                 TO ws-MAX-TOT-BEN
002498                 if pd-ben-pct (p1) not numeric
002499                    move zeros   to pd-ben-pct (p1)
002500                 end-if
002501                 if pd-ben-pct (p1) = zeros
002502                    move +1      to ws-work-ben-pct
002503                 else
002504                    move pd-ben-pct (p1)
002505                                 to ws-work-ben-pct
002506                 end-if
002507              END-IF
002508           end-if
002509        end-if
002510        PERFORM 9850-LINK-REM-AMT THRU 9850-EXIT
002511        MOVE CP-REMAINING-AMT      TO BENEO
002512     end-if
002513
002514     if ws-work-ben-pct <> zeros
002515        compute cp-remaining-amt =
002516           cp-remaining-amt * ws-work-ben-pct
002517     end-if
002518     if ws-max-tot-ben <> zeros
002519        if cp-remaining-amt > ws-max-tot-ben
002520           move ws-max-tot-ben   to cp-remaining-amt
002521        end-if
002522     end-if
002523     MOVE CP-REMAINING-AMT      TO BENEO
002524     IF PI-LIFE-OVERRIDE-L1 = 'P' OR
002525        WS-LF-COVERAGE-TYPE = 'P'
002526         COMPUTE WS-REMAINING-AMT = CM-LF-BENEFIT-AMT -
002527                                    CM-LF-ITD-DEATH-AMT
002528         MOVE WS-REMAINING-AMT    TO  BENEO.
002529
002530 1009-CONTINUE-BUILD.
002531
002532     MOVE CM-POLICY-FORM-NO  TO PI-SAVE-FORM.
002533
002534     IF WS-STATUS = '6' OR '7' OR '8'
002535        MOVE AL-SABOF              TO CRTSTATA.
002536
002537     IF WS-STATUS = '1' OR '4'
002538        IF CP-REMAINING-TERM-3 = ZEROS
002539           MOVE 'EXPIRED'        TO CRTSTATO
002540        ELSE
002541           MOVE 'ACTIVE  '       TO CRTSTATO.
002542
002543     IF WS-STATUS = '2'
002544         MOVE 'PEND    '         TO CRTSTATO.
002545
002546     IF WS-STATUS = '3'
002547         MOVE 'RESTORE '         TO CRTSTATO.
002548
002549     IF WS-STATUS = '5'
002550         MOVE 'REISSUE '         TO CRTSTATO.
002551
002552     IF WS-STATUS = '6'
002553        IF CM-LF-BENEFIT-CD = ZEROS  OR  SPACES
002554           MOVE 'LMP DIS'        TO CRTSTATO
002555        ELSE
002556           MOVE 'LMP BEN'        TO CRTSTATO.
002557
002558     IF WS-STATUS = '7'
002559         MOVE 'DEATH'            TO CRTSTATO.
002560
002561     IF WS-STATUS = '8'
002562         MOVE 'CANCEL'           TO CRTSTATO.
002563
002564     IF WS-STATUS = '9'
002565         MOVE 'RE-ONLY '         TO CRTSTATO.
002566
002567     IF WS-STATUS = 'D'
002568         MOVE 'DECLINE'          TO CRTSTATO.
002569
002570     IF WS-STATUS = 'V'
002571         MOVE 'VOID'             TO CRTSTATO.
002572
002573     IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
002574                                          OR 'B' OR 'H'
002575        MOVE CM-AH-BENEFIT-CD    TO WS-BEN-CD
002576        MOVE '5'                 TO CNTL-REC-TYPE
002577     END-IF
002578
002579     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
002580         MOVE CM-LF-BENEFIT-CD   TO WS-BEN-CD
002581         MOVE '4'                TO CNTL-REC-TYPE.
002582
002583     MOVE '** NONE **'           TO COVERO.
002584
002585     IF WS-BEN-CD = ZERO
002586         MOVE AL-SABOF           TO COVERA
002587         MOVE SPACES             TO CRTSTATO
002588         GO TO 1050-SHOW-CONTINUE.
002589
002590 1010-SHOW-CONTINUE.
002591     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
002592     MOVE WS-ACCESS              TO CNTL-ACCESS.
002593     MOVE +0                     TO CNTL-SEQ-NO.
002594     MOVE 'CNTL'                 TO FILE-SWITCH.
002595
002596     
      * EXEC CICS HANDLE CONDITION
002597*        ENDFILE   (1050-SHOW-CONTINUE)
002598*        NOTFND    (1050-SHOW-CONTINUE)
002599*    END-EXEC.
      *    MOVE '"$''I                  ! . #00010134' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303130313334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002600
002601     
      * EXEC CICS READ
002602*        DATASET   ('ELCNTL')
002603*        SET       (ADDRESS OF CONTROL-FILE)
002604*        RIDFLD    (ELCNTL-KEY)
002605*        GTEQ
002606*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00010139' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303130313339' TO DFHEIV0
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
           
002607
002608     IF (CNTL-COMP-ID  NOT = CF-COMPANY-ID) OR
002609        (CNTL-REC-TYPE NOT = CF-RECORD-TYPE)
002610           GO TO 1050-SHOW-CONTINUE.
002611
002612     PERFORM 1030-BENEFIT-DUMMY THRU 1030-EXIT
002613         VARYING SUB-1 FROM 1 BY 1 UNTIL
002614            ((SUB-1 > 8) OR
002615            (CF-BENEFIT-CODE (SUB-1) = WS-BEN-CD)).
002616
002617     IF SUB-1 NOT = 9
002618         MOVE CF-BENEFIT-DESCRIP (SUB-1) TO COVERO
002619         MOVE CF-JOINT-INDICATOR (SUB-1) TO PI-JOINT-COV-IND
002620       ELSE
002621         IF CRTSTATO = 'ACTIVE'
002622             MOVE SPACES         TO  CRTSTATO.
002623
002624     GO TO 1050-SHOW-CONTINUE.
002625
002626 1030-BENEFIT-DUMMY.
002627 1030-EXIT.
002628     EXIT.
002629
002630 1050-SHOW-CONTINUE.
002631
002632     IF CL-PURGED-DT NOT = LOW-VALUES
002633         MOVE 'Y'                        TO  PI-PURGED-SW
002634         MOVE AL-SANOF                   TO  FILETOA    PRTOPTA
002635                                             FORMATA    ALTPRTA
002636                                             ACTCDA     ASSOCA
002637                                             CLOANA
002638         MOVE 'CLAIM IS PURGED - '       TO  WS-PURGED-MSG
002639         MOVE CL-PURGED-DT               TO  DC-BIN-DATE-1
002640         MOVE ' '                        TO  DC-OPTION-CODE
002641         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002642         IF NO-CONVERSION-ERROR
002643             MOVE DC-GREG-DATE-1-EDIT    TO  WS-PURGED-DATE
002644             MOVE WS-PURGED-MESSAGE      TO  MAP-DISPLAY (1)
002645         ELSE
002646             MOVE SPACES                 TO  WS-PURGED-DATE
002647             MOVE WS-PURGED-MESSAGE      TO  MAP-DISPLAY (1)
002648     ELSE
002649         MOVE 'N'                        TO  PI-PURGED-SW
002650         PERFORM 2000-BUILD-TRAILER-DISPLAY THRU 2999-EXIT.
002651
002652     IF PI-COMPANY-ID = 'DMD'
002653        MOVE COVERO                  TO  CRTSTATO
002654        IF CL-LAST-PMT-AMT NOT = ZERO
002655           MOVE CL-LAST-PMT-DT       TO  DC-BIN-DATE-1
002656           MOVE ' '                  TO  DC-OPTION-CODE
002657           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
002658           MOVE DC-GREG-DATE-1-EDIT  TO  PRMTYPEO
002659           MOVE CL-LAST-PMT-AMT      TO  WS-PMT-AMT
002660           MOVE WS-PMT-AMTD          TO  COVERO
002661         ELSE
002662           MOVE SPACES               TO  COVERO
002663                                         PRMTYPEO.
002664
002665     IF RETURNED-FROM NOT = SPACES
002666         GO TO 8250-SEND-WITH-CURSOR.
002667
002668     IF CLAIM-IS-PURGED
002669        CONTINUE
002670*        MOVE -1                 TO  MCRFLML
002671     ELSE
002672         MOVE -1                 TO  FILETOL
002673     END-IF
002674
002675     IF PI-COMPANY-ID = 'DMD'
002676         IF CONFIDENTIAL-DATA
002677             IF PI-CONFIDENTIAL-UP
002678                 MOVE LOW-VALUE  TO PI-PRIORITY-IND
002679                 MOVE ER-0932    TO EMI-ERROR
002680                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002681             ELSE
002682                 MOVE 'C'        TO PI-PRIORITY-IND
002683                 
      * EXEC CICS SEND TEXT
002684*                    FROM     (W-CONF-TEXT)
002685*                    LENGTH   (W-CONF-LENGTH)
002686*                    ERASE
002687*                    FREEKB
002688*                END-EXEC
      *    MOVE '8&      T  E F  H   F -   #00010221' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303130323231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CONF-TEXT, 
                 W-CONF-LENGTH, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002689                 GO TO 9100-RETURN-TRAN.
002690
002691     GO TO 8100-SEND-INITIAL-MAP.
002692
002693 1100-SHOW-RECORD-NOT-FOUND.
002694
002695     IF FILE-SWITCH = 'MSTR'
002696         IF PI-CURRENT-FILE = 'ELRETR'
002697             MOVE ER-0204        TO EMI-ERROR
002698         ELSE
002699             MOVE 'Y'            TO PI-RETRIEVED-DATA-IND
002700             MOVE 'ELRETR'       TO PI-CURRENT-FILE
002701             GO TO 1000-READ-CLAIM
002702     ELSE
002703         MOVE ER-0206            TO EMI-ERROR.
002704
002705     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002706
002707     IF FILE-SWITCH = 'CERT' AND
002708      (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
002709        GO TO 1050-SHOW-CONTINUE.
002710
002711     MOVE -1                     TO CLMNOL.
002712
002713     MOVE PI-CARRIER             TO CARRO.
002714     MOVE PI-CLAIM-NO            TO CLMNOO.
002715     MOVE PI-CERT-PRIME          TO CERTNOO.
002716     MOVE PI-CERT-SFX            TO SUFXO.
002717
002718     MOVE AL-UABON               TO CLMNOA   CARRA
002719                                    YESNOSWA
002720                                    CERTNOA  SUFXA.
002721
002722     GO TO 8100-SEND-INITIAL-MAP.
002723     EJECT
002724 1200-GET-ASS-TOTALS.
002725
002726     MOVE CL-ASSOC-CERT-SEQU     TO PI-ASS-NDX.
002727
002728     IF RETURNED-FROM NOT = 'EL156' AND 'EL1501' AND 'EL1502'
002729         IF PI-ASS-PROCESSING
002730                 AND
002731            PI-ASS-CERTS (PI-ASS-NDX) = CL-CERT-NO
002732                 AND
002733            PI-PREV-PRIME-CERT-NO = CL-PRIME-CERT-NO
002734             GO TO 1200-SET-UP-FIELDS.
002735
002736     MOVE CL-ASSOC-CERT-SEQU      TO PI-LAST-SEQ-NO.
002737
002738     IF EIBAID = DFHPF15 OR DFHPF16
002739         
      * EXEC CICS ENDBR
002740*            DATASET   (PI-CURRENT-FILE)
002741*        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00010277' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130323737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002742
002743     MOVE CLAIM-MASTER           TO W-CLAIM-MASTER-SAVE.
002744     MOVE ZEROS                  TO PI-DAYS-PAID
002745                                    PI-TOTAL-PAID.
002746     MOVE LOW-VALUES             TO PI-ASSOCIATED-CERTS-TABLE
002747                                    PI-LAST-PF-KEY-IND
002748                                    PI-ASSOCIATED-PROCESS-IND
002749     MOVE SPACES                 TO MSTR-CERT-NO.
002750
002751     IF RETURNED-FROM = 'EL156'
002752             AND
002753        PI-PREV-PRIME-CERT-NO = CL-PRIME-CERT-NO
002754         MOVE SPACES             TO RETURNED-FROM
002755     ELSE
002756         MOVE SPACES             TO RETURNED-FROM
002757         MOVE CL-CONTROL-PRIMARY TO PI-ORIGINAL-ASS-CERT.
002758
002759     
      * EXEC CICS HANDLE CONDITION
002760*        NOTFND   (1220-NO-CLAIM-RECORD-FND)
002761*        ENDFILE  (1200-ENDBR)
002762*    END-EXEC.
      *    MOVE '"$I''                  ! / #00010297' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303130323937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002763
002764     
      * EXEC CICS STARTBR
002765*         DATASET   (PI-CURRENT-FILE)
002766*         RIDFLD    (ELMSTR-KEY)
002767*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00010302' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303130333032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002768
002769 1200-GET-NEXT-RECORD.
002770
002771     
      * EXEC CICS READNEXT
002772*         DATASET   (PI-CURRENT-FILE)
002773*         RIDFLD    (ELMSTR-KEY)
002774*         INTO      (CLAIM-MASTER)
002775*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00010309' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303130333039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 CLAIM-MASTER, 
                 DFHEIV12, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002776
002777     IF PI-PREV-COMP-CD  NOT = CL-COMPANY-CD  OR
002778        PI-PREV-CARRIER  NOT = CL-CARRIER     OR
002779        PI-PREV-CLAIM-NO NOT = CL-CLAIM-NO
002780         GO TO 1200-ENDBR.
002781
002782     IF CL-PRIME-CERT-NO NOT = PI-PREV-PRIME-CERT-NO
002783         GO TO 1200-GET-NEXT-RECORD.
002784
002785     ADD CL-TOTAL-PAID-AMT       TO PI-TOTAL-PAID.
002786     ADD CL-NO-OF-DAYS-PAID      TO PI-DAYS-PAID.
002787
002788     MOVE CL-ASSOC-CERT-SEQU     TO PI-ASS-NDX.
002789     MOVE CL-CERT-NO             TO PI-ASS-CERTS (PI-ASS-NDX).
002790
002791     GO TO 1200-GET-NEXT-RECORD.
002792
002793 1200-ENDBR.
002794
002795     
      * EXEC CICS ENDBR
002796*        DATASET   (PI-CURRENT-FILE)
002797*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00010333' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130333333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002798
002799     MOVE W-CLAIM-MASTER-SAVE    TO CLAIM-MASTER.
002800     MOVE PI-LAST-SEQ-NO         TO PI-ASS-NDX.
002801
002802 1200-SET-UP-FIELDS.
002803
002804     MOVE 'Y'                    TO PI-ASSOCIATED-PROCESS-IND.
002805     MOVE PI-DAYS-PAID           TO W-DAYS-PAID-X.
002806     MOVE PI-TOTAL-PAID          TO W-TOTAL-PAID-AMT-X.
002807     MOVE W-TOTAL-PAID-AMT-X     TO AMTPDO.
002808     MOVE W-DAYS-PAID-X          TO DAYSPDO.
002809     MOVE 'CLAIM TOTALS...AMOUNT PD:'
002810                                 TO CLMTOTHO.
002811     MOVE 'DAYS PAID:'           TO DAYSPDHO.
002812
002813 1200-EXIT.
002814     EXIT.
002815
002816 1220-NO-CLAIM-RECORD-FND.
002817
002818     MOVE ER-0204                TO EMI-ERROR.
002819
002820     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002821
002822     IF FILE-SWITCH = 'CERT' AND
002823       (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
002824         GO TO 1050-SHOW-CONTINUE.
002825
002826     MOVE -1                     TO CLMNOL.
002827
002828     MOVE PI-CARRIER             TO CARRO.
002829     MOVE PI-CLAIM-NO            TO CLMNOO.
002830     MOVE PI-CERT-PRIME          TO CERTNOO.
002831     MOVE PI-CERT-SFX            TO SUFXO.
002832
002833     MOVE AL-UABON               TO CLMNOA   CARRA
002834                                    CERTNOA  SUFXA.
002835
002836     GO TO 8100-SEND-INITIAL-MAP.
002837     EJECT
002838 1500-READ-CONVENIENCE-FILES.
002839
002840     
      * EXEC CICS HANDLE CONDITION
002841*        NOTFND   (1500-SHOW-RECORD-NOT-FOUND)
002842*    END-EXEC.
      *    MOVE '"$I                   ! 0 #00010378' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3020233030303130333738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002843
002844     MOVE PI-COMPANY-CD              TO  PLCY-COMP-CD.
002845     MOVE CL-CERT-CARRIER            TO  PLCY-CARRIER.
002846     MOVE CL-CERT-GROUPING           TO  PLCY-GROUPING.
002847     MOVE CL-CERT-STATE              TO  PLCY-STATE.
002848     MOVE CL-CERT-ACCOUNT            TO  PLCY-PRODUCER.
002849     MOVE CL-CERT-EFF-DT             TO  PLCY-EFF-DT.
002850     MOVE CL-CV-REFERENCE-NO         TO  PLCY-REFERENCE-NO.
002851     MOVE 'PLCY'                     TO  FILE-SWITCH.
002852
002853     
      * EXEC CICS READ
002854*        DATASET   ('MPPLCY')
002855*        SET       (ADDRESS OF POLICY-MASTER)
002856*        RIDFLD    (EMPLCY-KEY)
002857*    END-EXEC.
           MOVE 'MPPLCY' TO DFHEIV1
      *    MOVE '&"S        E          (   #00010391' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130333931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPLCY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002858
002859     MOVE 'Y'                        TO  WS-CERT-READ-SW.
002860
002861     PERFORM 1700-READ-EMPHST THRU 1700-EXIT.
002862
002863 1500-READ-EMPLAN.
002864
002865     MOVE PM-COMPANY-CD              TO  PLAN-COMP-CD.
002866     MOVE PM-CARRIER                 TO  PLAN-CARRIER.
002867     MOVE PM-GROUPING                TO  PLAN-GROUPING.
002868     MOVE PM-STATE                   TO  PLAN-STATE.
002869     MOVE PM-PRODUCER                TO  PLAN-PRODUCER.
002870     MOVE PM-INS-PLAN-CD             TO  PLAN-PLAN-CODE.
002871     MOVE PM-INS-PLAN-REVISION       TO  PLAN-REV-NO.
002872     MOVE 'PLAN'                     TO  FILE-SWITCH.
002873
002874     
      * EXEC CICS HANDLE CONDITION
002875*        NOTFND   (1500-NO-PLAN-RECORD)
002876*    END-EXEC.
      *    MOVE '"$I                   ! 1 #00010412' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3120233030303130343132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002877
002878     
      * EXEC CICS READ
002879*        DATASET   ('MPPLAN')
002880*        RIDFLD    (EMPLAN-KEY)
002881*        SET       (ADDRESS OF PRODUCER-PLANS)
002882*    END-EXEC.
           MOVE 'MPPLAN' TO DFHEIV1
      *    MOVE '&"S        E          (   #00010416' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130343136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-PLANS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002883
002884     MOVE PP-PLAN-ABBREV             TO  COVERO.
002885     MOVE PP-REFUND-CALC             TO  CP-EARNING-METHOD
002886                                         CP-RATING-METHOD.
002887     IF PP-BENEFIT-IS-LEVEL
002888         MOVE 'L'                    TO  CP-BENEFIT-TYPE
002889     ELSE
002890         MOVE 'R'                    TO  CP-BENEFIT-TYPE.
002891
002892     MOVE PM-LOAN-TERM               TO  CP-ORIGINAL-TERM
002893                                         CP-LOAN-TERM.
002894     MOVE SAVE-BIN-DATE              TO  CP-VALUATION-DT.
002895     MOVE PM-POLICY-EFF-DT           TO  CP-CERT-EFF-DT.
002896     MOVE PM-STATE                   TO  CP-STATE.
002897     MOVE 'A'                        TO  CP-SPECIAL-CALC-CD.
002898     MOVE '2'                        TO  CP-PROCESS-TYPE.
002899     MOVE PI-COMPANY-ID              TO  CP-COMPANY-ID.
002900     MOVE PI-COMPANY-CD              TO  CP-COMPANY-CD.
002901     MOVE '1'                        TO  CP-REM-TRM-CALC-OPTION.
002902
002903     IF PM-AH-MORT-PLAN
002904         MOVE '3'                    TO  CP-REM-TERM-METHOD
002905         MOVE PM-LOAN-DT             TO  CP-FIRST-PAY-DATE
002906     ELSE
002907         MOVE '2'                    TO  CP-REM-TERM-METHOD
002908         MOVE PM-POLICY-EFF-DT       TO  CP-FIRST-PAY-DATE.
002909
002910     PERFORM 8900-GET-FREE-LOOK THRU 8900-EXIT.
002911     PERFORM 9800-LINK-REM-TERM.
002912
002913     IF PM-AH-MORT-PLAN
002914       MOVE CP-REMAINING-TERM-1    TO  WST-REM
002915     ELSE
002916       IF PI-COMPANY-ID = 'CIG' OR 'CUK'
002917         COMPUTE CP-REMAINING-TERM-3 = CP-REMAINING-TERM-3 + 1
002918         MOVE CP-REMAINING-TERM-3    TO  WST-REM
002919       ELSE
002920         MOVE CP-REMAINING-TERM-3    TO  WST-REM.
002921
002922     GO TO 1500-SHOW-CONTINUE.
002923
002924 1500-NO-PLAN-RECORD.
002925
002926     MOVE ZEROS                      TO  WST-REM.
002927     MOVE '** NONE **'               TO  COVERO.
002928
002929 1500-SHOW-CONTINUE.
002930
002931     MOVE PM-LOAN-TERM               TO  WST-ORIG.
002932     MOVE WS-TERMS                   TO  TERMSO.
002933
002934     IF PM-AH-MORT-PLAN
002935         MOVE 'MO. BENEFIT :'        TO  BENECAPO
002936         MOVE PM-INS-MONTH-BENEFIT   TO  BENEO
002937     ELSE
002938         MOVE PM-INS-TOTAL-BENEFIT   TO  BENEO.
002939
002940     IF PM-ANNUAL
002941         MOVE 'ANNUAL'              TO  PRMTYPEO
002942       ELSE
002943     IF PM-SEMI-ANNUAL
002944         MOVE 'SEMI ANL'            TO  PRMTYPEO
002945       ELSE
002946     IF PM-QUARTERLY
002947         MOVE 'QTRLY'               TO  PRMTYPEO
002948       ELSE
002949     IF PM-BI-MONTHLY
002950         MOVE 'BI MONTH'            TO  PRMTYPEO
002951       ELSE
002952     IF PM-SINGLE-PREM
002953         MOVE 'SING PRM'            TO  PRMTYPEO.
002954
002955     MOVE PM-POLICY-EFF-DT           TO  DC-BIN-DATE-1.
002956     MOVE '6'                        TO  DC-OPTION-CODE.
002957     MOVE PM-LOAN-TERM               TO  DC-ELAPSED-MONTHS.
002958     MOVE +0                         TO  DC-ELAPSED-DAYS.
002959     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
002960     IF NO-CONVERSION-ERROR
002961         MOVE DC-GREG-DATE-1-EDIT    TO  EXPIREO
002962     ELSE
002963         MOVE SPACES                 TO  EXPIREO.
002964
002965     IF PM-CURRENT-STATUS = '0'
002966         MOVE 'LAPSED'               TO  CRTSTATO.
002967     IF PM-CURRENT-STATUS = '1'
002968         MOVE 'ACTIVE'               TO  CRTSTATO.
002969     IF PM-CURRENT-STATUS = '2'
002970         MOVE 'PENDING'              TO  CRTSTATO.
002971     IF PM-CURRENT-STATUS = '3'
002972         MOVE 'DECLINE'              TO  CRTSTATO.
002973     IF PM-CURRENT-STATUS = '4' OR '9'
002974         MOVE 'PND CANC'             TO  CRTSTATO.
002975     IF PM-CURRENT-STATUS = '5'
002976         MOVE 'PND ISS'              TO  CRTSTATO.
002977     IF PM-CURRENT-STATUS = '6'
002978         MOVE 'CLAIM'                TO  CRTSTATO.
002979     IF PM-CURRENT-STATUS = '7'
002980         MOVE 'CANCEL'               TO  CRTSTATO.
002981     IF PM-CURRENT-STATUS = '8'
002982         MOVE 'PND UNDW'             TO  CRTSTATO.
002983     IF PM-CURRENT-STATUS = 'C'
002984         MOVE 'TRANSFER'             TO  CRTSTATO.
002985     IF PM-CURRENT-STATUS = 'F'
002986         MOVE 'SETTLE'               TO  CRTSTATO.
002987     IF PM-CURRENT-STATUS = 'T'
002988         MOVE 'TRMNAT'               TO  CRTSTATO.
002989
002990     GO TO 1500-EXIT.
002991
002992 1500-SHOW-RECORD-NOT-FOUND.
002993
002994     IF FILE-SWITCH = 'PLCY'
002995         MOVE ER-9483            TO  EMI-ERROR.
002996
002997     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002998     MOVE -1                     TO  CLMNOL.
002999     MOVE PI-CARRIER             TO  CARRO.
003000     MOVE PI-CLAIM-NO            TO  CLMNOL.
003001     MOVE PI-CERT-PRIME          TO  CERTNOO.
003002     MOVE PI-CERT-SFX            TO  SUFXO.
003003
003004     MOVE AL-UABON               TO  CLMNOA    CARRA
003005                                     CERTNOA   SUFXA.
003006     GO TO 8100-SEND-INITIAL-MAP.
003007
003008 1500-EXIT.
003009     EXIT.
003010
003011     EJECT
003012 1600-ROLL-TRAILERS.
003013     MOVE SPACES                 TO MAP-DISPLAY (1)
003014                                    MAP-DISPLAY (2)
003015                                    MAP-DISPLAY (3)
003016                                    MAP-DISPLAY (4).
003017
003018     PERFORM 2000-BUILD-TRAILER-DISPLAY THRU 2999-EXIT.
003019
003020     MOVE -1                     TO FILETOL.
003021     GO TO 8200-SEND-DATAONLY.
003022
003023     EJECT
003024***************************************************************
003025*    I/O REQUESTS AGAINST THE CONVENIENCE POLICY HISTORY FILE *
003026***************************************************************
003027 1700-READ-EMPHST.
003028
003029     
      * EXEC CICS HANDLE CONDITION
003030*        ENDFILE  (1700-ENDBROWSE)
003031*        NOTFND   (1700-ENDBROWSE)
003032*    END-EXEC.
      *    MOVE '"$''I                  ! 2 #00010567' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'3220233030303130353637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003033
003034     MOVE PM-COMPANY-CD          TO  EMPHST-COMPANY-CD.
003035     MOVE PM-CARRIER             TO  EMPHST-CARRIER.
003036     MOVE PM-GROUPING            TO  EMPHST-GROUPING.
003037     MOVE PM-STATE               TO  EMPHST-STATE.
003038     MOVE PM-PRODUCER            TO  EMPHST-PRODUCER.
003039     MOVE PM-POLICY-EFF-DT       TO  EMPHST-POLICY-EFF-DT.
003040     MOVE PM-REFERENCE-NUMBER    TO  EMPHST-REFERENCE-NUMBER.
003041     MOVE ZEROS                  TO  EMPHST-SEQUENCE-NO.
003042
003043     IF PM-LIFE-MORT-PLAN
003044         MOVE '02'                   TO  EMPHST-RECORD-TYPE
003045         MOVE '21'                   TO  EMPHST-FIELD-TYPE
003046     ELSE
003047         MOVE '04'                   TO  EMPHST-RECORD-TYPE
003048         MOVE '07'                   TO  EMPHST-FIELD-TYPE.
003049
003050     
      * EXEC CICS STARTBR
003051*        DATASET   (EMPHST-FILE-ID)
003052*        RIDFLD    (EMPHST-KEY)
003053*        GTEQ
003054*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00010588' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303130353838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EMPHST-FILE-ID, 
                 EMPHST-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003055
003056     MOVE 'Y'                    TO  WS-BROWSE-START-SW.
003057
003058 1700-READ-NEXT.
003059
003060     
      * EXEC CICS READNEXT
003061*        DATASET   (EMPHST-FILE-ID)
003062*        SET       (ADDRESS OF POLICY-HISTORY)
003063*        RIDFLD    (EMPHST-KEY)
003064*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00010598' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303130353938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EMPHST-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPHST-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-HISTORY TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003065
003066     IF PM-COMPANY-CD       = PH-COMPANY-CD         AND
003067        PM-CARRIER          = PH-CARRIER            AND
003068        PM-GROUPING         = EMPHST-GROUPING       AND
003069        PM-STATE            = EMPHST-STATE          AND
003070        PM-PRODUCER         = EMPHST-PRODUCER       AND
003071        PM-POLICY-EFF-DT    = EMPHST-POLICY-EFF-DT  AND
003072        PM-REFERENCE-NUMBER = EMPHST-REFERENCE-NUMBER
003073         NEXT SENTENCE
003074     ELSE
003075         GO TO 1700-ENDBROWSE.
003076
003077     IF PM-LIFE-MORT-PLAN
003078         IF PH-RECORD-TYPE = '02' AND
003079            PH-FIELD-TYPE  = '21'
003080             NEXT SENTENCE
003081         ELSE
003082             GO TO 1700-ENDBROWSE.
003083
003084     IF NOT PM-LIFE-MORT-PLAN
003085         IF PH-RECORD-TYPE = '04' AND
003086            PH-FIELD-TYPE  = '07'
003087             NEXT SENTENCE
003088         ELSE
003089             GO TO 1700-ENDBROWSE.
003090
003091     IF PH-CHANGE-DT > CL-INCURRED-DT
003092         GO TO 1700-READ-NEXT.
003093
003094     MOVE PH-RECORD-BODY         TO  WS-HISTORY-AREA.
003095
003096     
      * EXEC CICS BIF
003097*        DEEDIT
003098*        FIELD (WS-HISTORY-AREA)
003099*        LENGTH ('50')
003100*    END-EXEC.
           MOVE '50'
             TO DFHEIV11
      *    MOVE '@"L                   #   #00010634' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303130363334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-HISTORY-AREA, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003101
003102     IF PM-LIFE-MORT-PLAN
003103         MOVE WS-NUMERIC-FLD1    TO  PM-INSURED-TOT-BENEFIT
003104     ELSE
003105         MOVE WS-NUMERIC-FLD1    TO  PM-INS-MONTH-BENEFIT.
003106
003107 1700-ENDBROWSE.
003108
003109     
      * EXEC CICS ENDBR
003110*        DATASET   (EMPHST-FILE-ID)
003111*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00010647' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130363437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EMPHST-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003112
003113 1700-EXIT.
003114      EXIT.
003115
003116     EJECT
003117 2000-BUILD-TRAILER-DISPLAY.
003118
003119     IF DIRECTION-SWITCH = 'F'
003120        IF PI-PREV-DIR = 'B' AND PI-PREV-DISP = 'N'
003121           MOVE PI-SAVE-LOW    TO TRLR-SEQ-NO
003122        ELSE
003123           MOVE PI-SAVE-HIGH   TO TRLR-SEQ-NO
003124           ADD +1              TO TRLR-SEQ-NO
003125     ELSE
003126        IF DIRECTION-SWITCH = 'B'
003127           IF PI-PREV-DIR = 'F' AND PI-PREV-DISP = 'N'
003128              MOVE PI-SAVE-HIGH TO TRLR-SEQ-NO
003129           ELSE
003130              MOVE PI-SAVE-LOW TO TRLR-SEQ-NO
003131        ELSE
003132           IF DIRECTION-SWITCH = 'R'
003133              MOVE PI-SAVE-LOW TO TRLR-SEQ-NO
003134              MOVE 'F'        TO DIRECTION-SWITCH
003135           ELSE
003136              IF PI-COMPANY-ID = 'AIG' OR 'AUK'
003137                  MOVE +95     TO  TRLR-SEQ-NO
003138                  MOVE +0      TO  PI-SAVE-LOW
003139                                   PI-SAVE-HIGH
003140                  MOVE 'N'     TO  PI-PREV-DISP
003141                  MOVE 'F'     TO  DIRECTION-SWITCH
003142              ELSE
003143                  MOVE +93     TO  TRLR-SEQ-NO
003144                  MOVE +0      TO  PI-SAVE-LOW
003145                                   PI-SAVE-HIGH
003146                  MOVE 'N'     TO  PI-PREV-DISP
003147                  MOVE 'F'     TO  DIRECTION-SWITCH.
003148
003149     IF DIRECTION-SWITCH = 'F'
003150         MOVE +1                 TO DISPLAY-CNT
003151     ELSE
003152         MOVE +4                 TO DISPLAY-CNT.
003153
003154     MOVE PI-COMPANY-CD          TO TRLR-COMP-CD.
003155     MOVE PI-CARRIER             TO TRLR-CARRIER.
003156     MOVE PI-CLAIM-NO            TO TRLR-CLAIM-NO.
003157     MOVE PI-CERT-NO             TO TRLR-CERT-NO.
003158     MOVE 'TRLR'                 TO FILE-SWITCH.
003159
003160     IF PI-SAVE-SYS-ID = 'CV'
003161         NEXT SENTENCE
003162     ELSE
003163         GO TO 2005-BUILD-CR-LOAN-DATA.
003164
003165     IF WS-CERT-READ-SW = 'Y'
003166       IF PI-COMPANY-ID = 'CIG' OR 'CUK'
003167         IF CL-SYSTEM-IDENTIFIER = 'CV'
003168           MOVE PM-INS-TERMINATION-DT    TO  DC-BIN-DATE-1
003169           MOVE ' '                      TO  DC-OPTION-CODE
003170           MOVE +0                       TO  DC-ELAPSED-MONTHS
003171                                             DC-ELAPSED-DAYS
003172           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003173           IF NO-CONVERSION-ERROR
003174               MOVE DC-GREG-DATE-1-EDIT  TO  WS-LOAN-EXPIRE-DT
003175           ELSE
003176               MOVE SPACES               TO  WS-LOAN-EXPIRE-DT.
003177
003178     IF WS-CERT-READ-SW = 'Y'
003179       IF PI-COMPANY-ID = 'CIG' OR 'CUK'
003180         MOVE SPACES                 TO  TRAILER-DISPLAY-WORK-AREA
003181         MOVE 'LOAN INFO'            TO  DISPLAY-ACTION
003182         MOVE ZEROS                  TO  DISPLAY-SEQ
003183         MOVE WS-LOAN-EXPIRE-DT      TO  DISPLAY-DATE
003184         MOVE PM-LOAN-NUMBER         TO  ASSOC-CV-LOAN
003185         MOVE ASSOC-CV-LOAN-TEXT     TO  DISPLAY-TEXT
003186         MOVE TRAILER-DISPLAY-WORK-AREA
003187                                     TO  MAP-DISPLAY (DISPLAY-CNT)
003188         ADD +1                      TO  DISPLAY-CNT.
003189
003190     GO TO 2010-CONTINUE-TRAILER-DISPLAY.
003191
003192 2005-BUILD-CR-LOAN-DATA.
003193
003194     IF WS-CERT-READ-SW = 'Y'
003195        IF PI-COMPANY-ID = 'AIG' OR 'AUK'
003196          IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
003197            MOVE CM-LF-LOAN-EXPIRE-DT    TO  DC-BIN-DATE-1
003198            MOVE ' '                     TO  DC-OPTION-CODE
003199            MOVE +0                      TO  DC-ELAPSED-MONTHS
003200                                             DC-ELAPSED-DAYS
003201            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003202            IF NO-CONVERSION-ERROR
003203              MOVE DC-GREG-DATE-1-EDIT   TO  WS-LOAN-EXPIRE-DT
003204            ELSE
003205              MOVE SPACES                TO  WS-LOAN-EXPIRE-DT
003206          ELSE
003207            MOVE CM-AH-LOAN-EXPIRE-DT    TO  DC-BIN-DATE-1
003208            MOVE ' '                     TO  DC-OPTION-CODE
003209            MOVE +0                      TO  DC-ELAPSED-MONTHS
003210                                             DC-ELAPSED-DAYS
003211            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003212            IF NO-CONVERSION-ERROR
003213              MOVE DC-GREG-DATE-1-EDIT   TO  WS-LOAN-EXPIRE-DT
003214            ELSE
003215              MOVE SPACES                TO  WS-LOAN-EXPIRE-DT.
003216
003217     IF WS-CERT-READ-SW = 'Y'
003218       IF PI-COMPANY-ID = 'AIG' OR 'AUK'
003219         MOVE SPACES             TO  TRAILER-DISPLAY-WORK-AREA
003220         MOVE ZEROS              TO  DISPLAY-SEQ
003221         MOVE 'LOAN DATA'        TO  DISPLAY-ACTION
003222         MOVE WS-LOAN-EXPIRE-DT  TO  DISPLAY-DATE
003223         MOVE CM-LOAN-NUMBER     TO  ASSOC-ORIG-LOAN
003224         MOVE CM-MEMBER-NO       TO  ASSOC-CUR-LOAN
003225         MOVE CL-LOAN-TYPE       TO  ASSOC-LOAN-TYPE
003226         MOVE ASSOC-LOAN-TEXT    TO  DISPLAY-TEXT
003227         MOVE TRAILER-DISPLAY-WORK-AREA
003228                                 TO  MAP-DISPLAY (DISPLAY-CNT)
003229         ADD +1                  TO  DISPLAY-CNT
003230       ELSE
003231         IF CM-OPEN-END
003232           MOVE SPACES           TO TRAILER-DISPLAY-WORK-AREA
003233           MOVE 'LOAN INFO'      TO DISPLAY-ACTION
003234           MOVE CM-LOAN-NUMBER   TO LOAN-NUMBER
003235           MOVE CM-LOAN-BALANCE  TO LOAN-BALANCE
003236           MOVE CM-MEMBER-NO     TO LOAN-MEMBER
003237           MOVE LOAN-TEXT        TO DISPLAY-TEXT
003238           MOVE TRAILER-DISPLAY-WORK-AREA
003239                                 TO MAP-DISPLAY (DISPLAY-CNT)
003240           ADD +1                TO DISPLAY-CNT.
003241
003242     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'CAP'
003243           OR 'FNL'
003244        PERFORM 6150-READ-TRLR92 THRU 6150-EXIT
003245        if ws-resp-normal
003246           MOVE SPACES           TO TRAILER-DISPLAY-WORK-AREA
003247           MOVE 'SPECIAL REVIEW' TO DISPLAY-ACTION
003248           move at-recorded-by   to display-by
003249           move at-recorded-dt   to dc-bin-date-1
003250           MOVE ' '              TO DC-OPTION-CODE
003251           PERFORM 9700-LINK-DATE-CONVERT
003252                                 THRU 9700-EXIT
003253           MOVE DC-GREG-DATE-1-EDIT
003254                                 TO DISPLAY-DATE
003255           MOVE ws-TRLR-SEQ-NO   TO DISPLAY-SEQ
003256           MOVE AT-INFO-LINE-1   TO DISPLAY-TEXT
003257           MOVE TRAILER-DISPLAY-WORK-AREA
003258                                 TO MAP-DISPLAY (1)
003259           if direction-switch = 'F'
003260              ADD +1             TO  DISPLAY-CNT
003261           end-if
003262        end-if
003263        PERFORM 6160-READ-TRLR93 THRU 6160-EXIT
003264        if ws-resp-normal
003265           MOVE SPACES           TO TRAILER-DISPLAY-WORK-AREA
003266           MOVE 'VERIFY SSN    ' TO DISPLAY-ACTION
003267           move at-recorded-by   to display-by
003268           move at-recorded-dt   to dc-bin-date-1
003269           MOVE ' '              TO DC-OPTION-CODE
003270           PERFORM 9700-LINK-DATE-CONVERT
003271                                 THRU 9700-EXIT
003272           MOVE DC-GREG-DATE-1-EDIT
003273                                 TO DISPLAY-DATE
003274           MOVE ws-TRLR-SEQ-NO   TO DISPLAY-SEQ
003275           MOVE AT-INFO-LINE-1   TO DISPLAY-TEXT
003276           IF MAP-DISPLAY (1) (1:14) = 'SPECIAL REVIEW'
003277               MOVE TRAILER-DISPLAY-WORK-AREA
003278                                 TO MAP-DISPLAY (2)
003279           ELSE
003280               MOVE TRAILER-DISPLAY-WORK-AREA
003281                                 TO MAP-DISPLAY (1)
003282           END-IF
003283           if direction-switch = 'F'
003284              ADD +1             TO  DISPLAY-CNT
003285           end-if
003286        end-if
003287        PERFORM 6170-READ-TRLR94 THRU 6170-EXIT
003288        if ws-resp-normal
003289           MOVE SPACES           TO TRAILER-DISPLAY-WORK-AREA
003290           MOVE 'CAUSAL STATE  ' TO DISPLAY-ACTION
003291           MOVE AT-RECORDED-BY   TO DISPLAY-BY
003292           MOVE AT-RECORDED-DT   TO DC-BIN-DATE-1
003293           MOVE ' '              TO DC-OPTION-CODE
003294           PERFORM 9700-LINK-DATE-CONVERT
003295                                 THRU 9700-EXIT
003296           MOVE DC-GREG-DATE-1-EDIT
003297                                 TO DISPLAY-DATE
003298           MOVE WS-TRLR-SEQ-NO   TO DISPLAY-SEQ
003299           MOVE AT-INFO-LINE-1   TO DISPLAY-TEXT
003300           IF MAP-DISPLAY (1) (1:14) = 'SPECIAL REVIEW'
003301            OR 'VERIFY SSN    '
003302               IF MAP-DISPLAY (2) (1:14) = 'VERIFY SSN    '
003303                   MOVE TRAILER-DISPLAY-WORK-AREA
003304                                 TO MAP-DISPLAY (3)
003305               ELSE
003306                  MOVE TRAILER-DISPLAY-WORK-AREA
003307                                 TO MAP-DISPLAY (2)
003308               END-IF
003309           ELSE
003310               MOVE TRAILER-DISPLAY-WORK-AREA
003311                                 TO MAP-DISPLAY (1)
003312           END-IF
003313           IF DIRECTION-SWITCH = 'F'
003314              ADD +1             TO  DISPLAY-CNT
003315           END-IF
003316        END-IF
003317     end-if
003318
003319     IF PI-COMPANY-ID = 'DCC' or 'CAP'
003320        PERFORM 6180-READ-TRLR95 THRU 6180-EXIT
003321        if ws-resp-normal
003322           move error-message-interface-block
003323                                 to ws-save-error-interface-block
003324           MOVE SPACES           TO TRAILER-DISPLAY-WORK-AREA
003325           MOVE 'NOTE'           TO DISPLAY-ACTION
003326           move at-recorded-by   to display-by
003327           move at-recorded-dt   to dc-bin-date-1
003328           MOVE ' '              TO DC-OPTION-CODE
003329           PERFORM 9700-LINK-DATE-CONVERT
003330                                 THRU 9700-EXIT
003331           MOVE DC-GREG-DATE-1-EDIT
003332                                 TO DISPLAY-DATE
003333           MOVE ws-TRLR-SEQ-NO   TO DISPLAY-SEQ
003334           perform varying a1 from +1 by +1 until
003335              at-note-error-no (a1) = spaces
003336              or display-cnt > +4
003337              move at-note-error-no (a1)
003338                                 to emi-error
003339              if at-note-error-no (a1) = '1653'
003340                 evaluate true
003341                    when pi-save-type = 'L'
003342                       move '  LF  '
003343                              to emi-claim-type
003344                    when pi-save-type = 'I'
003345                       move '  IU  '
003346                              to emi-claim-type
003347                    when pi-save-type = 'F'
003348                       move '  FL  '
003349                              to emi-claim-type
003350                    WHEN pi-save-type = 'B'
003351                       MOVE ' BR  '         TO emi-claim-type
003352                    WHEN pi-save-type = 'H'
003353                       MOVE ' HS '          TO emi-claim-type
003354
003355                    when pi-save-type = 'O'
003356                       move '  OT  '
003357                              to emi-claim-type
003358                    when other
003359                       move '  AH  '
003360                              to emi-claim-type
003361                 end-evaluate
003362              end-if
003363              PERFORM 9900-ERROR-FORMAT
003364                                 THRU 9900-EXIT
003365              move emi-line1 (8:64)
003366                             to TRAILER-DISPLAY-WORK-AREA (16:64)
003367              move trailer-display-work-area
003368                                 to map-display (display-cnt)
003369              if direction-switch = 'F'
003370                 ADD +1          TO  DISPLAY-CNT
003371              end-if
003372              move ws-save-error-interface-block
003373                                 to error-message-interface-block
003374           end-perform
003375        end-if
003376     end-if
003377
003378     .
003379 2010-CONTINUE-TRAILER-DISPLAY.
003380
003381     
      * EXEC CICS HANDLE CONDITION
003382*        ENDFILE   (2950-NO-MORE-TRAILERS)
003383*        NOTFND    (2950-NO-MORE-TRAILERS)
003384*    END-EXEC.
      *    MOVE '"$''I                  ! 3 #00010919' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'3320233030303130393139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003385
003386     
      * EXEC CICS STARTBR
003387*        DATASET   ('ELTRLR')
003388*        GTEQ
003389*        RIDFLD    (ELTRLR-KEY)
003390*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00010924' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303130393234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003391
003392     IF DIRECTION-SWITCH = 'B'
003393         IF (PI-PREV-DISP = 'Y') OR (PI-PREV-DIR = 'B')
003394             
      * EXEC CICS READPREV
003395*                DATASET ('ELTRLR')
003396*                RIDFLD  (ELTRLR-KEY)
003397*                SET     (ADDRESS OF ACTIVITY-TRAILERS)
003398*            END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00010932' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303130393332' TO DFHEIV0
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
           
003399
003400     MOVE 'N'                    TO PI-PREV-DISP.
003401
003402     MOVE DIRECTION-SWITCH       TO PI-PREV-DIR.
003403
003404 2020-READ-TRAILER-LOOP.
003405     IF DIRECTION-SWITCH = 'F'
003406         
      * EXEC CICS READNEXT
003407*            DATASET   ('ELTRLR')
003408*            SET       (ADDRESS OF ACTIVITY-TRAILERS)
003409*            RIDFLD    (ELTRLR-KEY)
003410*        END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00010944' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303130393434' TO DFHEIV0
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003411     ELSE
003412         
      * EXEC CICS READPREV
003413*            DATASET   ('ELTRLR')
003414*            SET       (ADDRESS OF ACTIVITY-TRAILERS)
003415*            RIDFLD    (ELTRLR-KEY)
003416*        END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00010950' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303130393530' TO DFHEIV0
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
           
003417
003418     IF (PI-COMPANY-CD NOT = TRLR-COMP-CD)  OR
003419        (PI-CARRIER    NOT = TRLR-CARRIER)  OR
003420        (PI-CLAIM-NO   NOT = TRLR-CLAIM-NO) OR
003421        (PI-CERT-NO    NOT = TRLR-CERT-NO)
003422            GO TO 2950-NO-MORE-TRAILERS.
003423
003424*    IF TRLR-SEQ-NO = 90
003425     IF TRLR-SEQ-NO = 90 or 91 or 92 or 93
003426       OR 94 or 95
003427         GO TO 2020-READ-TRAILER-LOOP.
003428
003429     IF PI-COMPANY-ID = 'LAP'
003430         IF TRLR-SEQ-NO > +100 AND < +301
003431             GO TO 2020-READ-TRAILER-LOOP.
003432
003433     IF RESERVE-EXPENSE-TR OR ADDRESS-TR
003434         GO TO 2020-READ-TRAILER-LOOP.
003435
003436     IF PAYMENT-TR
003437         GO TO 2100-PAYMENT-TRAILER.
003438
003439     IF AUTO-PAY-TR
003440        GO TO 2200-AUTO-PAYMENT-TRAILER.
003441
003442     IF CORRESPONDENCE-TR
003443         GO TO 2300-CORRESPONDENCE-TRAILER.
003444
003445     IF GENERAL-INFO-TR
003446         GO TO 2400-GENERAL-INFO-TRAILER.
003447
003448     IF AUTO-PROMPT-TR
003449         GO TO 2500-AUTO-PROMPT-TRAILER.
003450
003451     IF DENIAL-TR
003452         GO TO 2600-DENIAL-TRAILER.
003453
003454     IF INCURRED-CHG-TR
003455         GO TO 2700-INCURRED-CHANGE-TRAILER.
003456
003457     IF FORM-CONTROL-TR
003458         GO TO 2710-FORM-CONTROL-TRAILER.
003459
003460     GO TO 2020-READ-TRAILER-LOOP.
003461
003462     EJECT
003463 2100-PAYMENT-TRAILER.
003464
003465     IF AT-PAYMENT-NOTE
003466         GO TO 2020-READ-TRAILER-LOOP.
003467
003468     MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
003469
003470     IF AT-PAYMENT-TYPE = 'T'
003471         MOVE 'TRANSFER PMT'     TO DISPLAY-ACTION
003472         GO TO 2100-CONT.
003473
003474     IF PI-SAVE-SYS-ID = 'CV'
003475         IF AT-CLAIM-TYPE = 'L'
003476             MOVE AT-CV-PMT-CODE             TO  WS-SUB
003477             IF WS-SUB < 1 OR > 8
003478                 MOVE 1                      TO  WS-SUB
003479                 MOVE CV-PAY-DESC (WS-SUB)   TO  DISPLAY-ACTION
003480             ELSE
003481                 MOVE CV-PAY-DESC (WS-SUB)   TO  DISPLAY-ACTION.
003482
003483     IF AT-TO-BE-WRITTEN-DT > LOW-VALUES
003484       AND AT-CHECK-WRITTEN-DT = LOW-VALUES
003485       AND AT-VOID-DT = LOW-VALUES
003486        MOVE 'HOLD & PAY'        TO  DISPLAY-ACTION
003487     ELSE
003488     IF AT-PAYMENT-TYPE = 'I'
003489        MOVE 'INTEREST PMT'      TO DISPLAY-ACTION
003490     ELSE
003491     IF (PI-SAVE-SYS-ID = 'CV' AND AT-CLAIM-TYPE = 'A')
003492                  OR
003493         PI-SAVE-SYS-ID NOT = 'CV'
003494         MOVE AT-PAYMENT-TYPE           TO  WS-SUB
003495         IF WS-SUB < 1 OR > 6
003496             MOVE 2                     TO  WS-SUB
003497             MOVE PAY-DESC (WS-SUB)     TO  DISPLAY-ACTION
003498         ELSE
003499             MOVE PAY-DESC (WS-SUB)     TO  DISPLAY-ACTION.
003500
003501 2100-CONT.
003502
003503     MOVE AT-RECORDED-BY         TO DISPLAY-BY.
003504     MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
003505     MOVE ' '                    TO DC-OPTION-CODE.
003506     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003507     MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
003508     MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
003509
003510
003511     if at-ach-payment = 'Y'
003512        move '  ACH  '           to pmt-check-head
003513     else
003514        move ' CHECK='           to pmt-check-head
003515     end-if
003516
003517     IF AT-VOID-DT NOT = LOW-VALUES
003518        MOVE AT-VOID-DT         TO DC-BIN-DATE-1
003519        MOVE ' '                TO DC-OPTION-CODE
003520        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003521        MOVE DC-GREG-DATE-1-EDIT TO PMT-PD-THRU
003522        IF AT-VOID-TYPE = 'S'
003523            MOVE 'STOP DT='     TO PMT-VAR
003524        ELSE
003525            MOVE 'VOIDED ='     TO PMT-VAR
003526     ELSE
003527        IF AT-PAID-THRU-DT NOT = LOW-VALUES
003528           MOVE AT-PAID-THRU-DT TO DC-BIN-DATE-1
003529           MOVE ' '             TO DC-OPTION-CODE
003530           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003531           MOVE DC-GREG-DATE-1-EDIT TO PMT-PD-THRU
003532           MOVE 'PD THRU='     TO PMT-VAR
003533           IF PI-USES-PAID-TO
003534              MOVE AT-PAID-THRU-DT TO DC-BIN-DATE-1
003535              MOVE '6'             TO DC-OPTION-CODE
003536              MOVE +1              TO DC-ELAPSED-DAYS
003537              MOVE +0              TO DC-ELAPSED-MONTHS
003538              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003539              MOVE DC-GREG-DATE-1-EDIT TO PMT-PD-THRU
003540              MOVE 'PAID TO='      TO PMT-VAR
003541           ELSE
003542              NEXT SENTENCE
003543        ELSE
003544           IF CHARGEABLE-EXPENSE OR NON-CHARGEABLE-EXPENSE
003545              MOVE 'EXP TYP='       TO PMT-VAR
003546              MOVE AT-EXPENSE-TYPE  TO PMT-PD-THRU
003547           ELSE
003548              MOVE SPACES      TO PMT-VAR  PMT-PD-THRU.
003549
003550     MOVE AT-CHECK-NO               TO PMT-CHECK-NO.
003551     MOVE AT-AMOUNT-PAID            TO PMT-AMOUNT.
003552     MOVE PAYMENT-TEXT              TO DISPLAY-TEXT.
003553     MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
003554
003555     GO TO 2800-INCR-DISPLAY-CNT.
003556
003557     EJECT
003558 2200-AUTO-PAYMENT-TRAILER.
003559     MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
003560     MOVE 'AUTO PMT SETUP'       TO DISPLAY-ACTION.
003561     MOVE AT-RECORDED-BY         TO DISPLAY-BY.
003562
003563     MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
003564     MOVE ' '                    TO DC-OPTION-CODE.
003565     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003566     MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
003567
003568     MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
003569
003570     IF PI-COMPANY-ID = 'DMD'
003571        IF AT-TERMINATED-DT NOT = SPACES AND ZEROS
003572                                         AND LOW-VALUES
003573           MOVE AT-TERMINATED-DT          TO DC-BIN-DATE-1
003574           MOVE ' '                       TO DC-OPTION-CODE
003575           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003576           MOVE DC-GREG-DATE-1-EDIT       TO AUTO-TERMINATED
003577           MOVE AT-REGULAR-PMT-AMT        TO AUTO-TERM-AMT
003578           MOVE AUTO-TERM-TEXT            TO DISPLAY-TEXT
003579           MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY
003580                                            (DISPLAY-CNT)
003581           GO TO 2800-INCR-DISPLAY-CNT.
003582
003583     MOVE AT-SCHEDULE-START-DT   TO DC-BIN-DATE-1.
003584     MOVE ' '                    TO DC-OPTION-CODE.
003585     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003586     MOVE DC-GREG-DATE-1-EDIT    TO AUTO-START.
003587
003588     IF PI-USES-PAID-TO
003589         MOVE AT-SCHEDULE-END-DT        TO  DC-BIN-DATE-1
003590         MOVE '6'                       TO  DC-OPTION-CODE
003591         MOVE +1                        TO  DC-ELAPSED-DAYS
003592         MOVE +0                        TO  DC-ELAPSED-MONTHS
003593         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003594         IF NO-CONVERSION-ERROR
003595             MOVE DC-GREG-DATE-1-EDIT   TO  AUTO-END
003596         ELSE
003597             MOVE SPACES                TO  AUTO-END
003598     ELSE
003599         MOVE AT-SCHEDULE-END-DT        TO  DC-BIN-DATE-1
003600         MOVE ' '                       TO  DC-OPTION-CODE
003601         MOVE +0                        TO  DC-ELAPSED-DAYS
003602                                            DC-ELAPSED-MONTHS
003603         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003604         IF NO-CONVERSION-ERROR
003605             MOVE DC-GREG-DATE-1-EDIT   TO  AUTO-END
003606         ELSE
003607             MOVE SPACES                TO  AUTO-END.
003608
003609     MOVE AT-REGULAR-PMT-AMT        TO AUTO-AMOUNT.
003610     MOVE AUTO-PMT-TEXT             TO DISPLAY-TEXT.
003611     MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
003612
003613     GO TO 2800-INCR-DISPLAY-CNT.
003614
003615     EJECT
003616 2300-CORRESPONDENCE-TRAILER.
003617     MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
003618     MOVE 'LETTER'               TO DISP-ACT-A.
003619     MOVE AT-STD-LETTER-FORM     TO DISP-ACT-B.
003620     MOVE AT-RECORDED-BY         TO DISPLAY-BY.
003621
003622     MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
003623     MOVE ' '                    TO DC-OPTION-CODE.
003624     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003625     MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
003626
003627     MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
003628
003629     IF AT-ADDRESEE-TYPE = 'I'
003630         MOVE 1                  TO WS-SUB
003631      ELSE
003632     IF AT-ADDRESEE-TYPE = 'B'
003633         MOVE 2                  TO WS-SUB
003634      ELSE
003635     IF AT-ADDRESEE-TYPE = 'A'
003636         MOVE 3                  TO WS-SUB
003637      ELSE
003638     IF AT-ADDRESEE-TYPE = 'P'
003639         MOVE 4                  TO WS-SUB
003640      ELSE
003641     IF AT-ADDRESEE-TYPE = 'E'
003642         MOVE 5                  TO WS-SUB
003643      ELSE
003644     IF AT-ADDRESEE-TYPE = 'O'
003645         MOVE 6                  TO WS-SUB
003646      ELSE
003647     IF AT-ADDRESEE-TYPE = 'Q'
003648         MOVE 7                  TO WS-SUB
003649      ELSE
003650         MOVE 0                  TO WS-SUB.
003651
003652     IF WS-SUB = 0
003653         MOVE SPACES             TO  CORR-TO
003654     ELSE
003655         MOVE CORR-DESC (WS-SUB) TO  CORR-TO.
003656
003657     IF AT-STOP-LETTER-DT NOT = LOW-VALUES AND SPACES
003658        AND (AT-LETTER-ANSWERED-DT = LOW-VALUES  OR
003659             AT-LETTER-ANSWERED-DT > AT-STOP-LETTER-DT)
003660          MOVE '   STOP='         TO CORR-RECVD-LIT
003661          MOVE AT-STOP-LETTER-DT  TO DC-BIN-DATE-1
003662          MOVE ' '                TO DC-OPTION-CODE
003663          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003664          MOVE DC-GREG-DATE-1-EDIT TO CORR-RECVD
003665     ELSE
003666        IF AT-LETTER-SENT-DT = LOW-VALUES OR SPACES
003667            MOVE 'MAIL RCVD'    TO DISP-ACT-A
003668        END-IF
003669        MOVE '  RECVD='         TO CORR-RECVD-LIT
003670        IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES
003671            MOVE AT-LETTER-ANSWERED-DT TO DC-BIN-DATE-1
003672            MOVE ' '                TO DC-OPTION-CODE
003673            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003674            MOVE DC-GREG-DATE-1-EDIT TO CORR-RECVD
003675        ELSE
003676            MOVE SPACES             TO CORR-RECVD
003677        END-IF
003678     END-IF.
003679
003680     IF AT-RESEND-PRINT-DATE NOT = LOW-VALUES
003681         MOVE '  RESENT='          TO CORR-VAR
003682         MOVE AT-RESEND-PRINT-DATE TO DC-BIN-DATE-1
003683         MOVE ' '                  TO DC-OPTION-CODE
003684         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003685         MOVE DC-GREG-DATE-1-EDIT  TO CORR-RESENT
003686     ELSE
003687         IF AT-AUTO-RE-SEND-DT NOT = LOW-VALUES
003688             MOVE '  RESEND='         TO CORR-VAR
003689             MOVE AT-AUTO-RE-SEND-DT  TO DC-BIN-DATE-1
003690             MOVE ' '                 TO DC-OPTION-CODE
003691             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003692             MOVE DC-GREG-DATE-1-EDIT TO CORR-RESENT
003693         ELSE
003694            IF AT-LETTER-ARCHIVE-NO NOT = ZEROS
003695               MOVE 'ARCH. NO='          TO CORR-VAR
003696               MOVE AT-LETTER-ARCHIVE-NO TO CORR-ARCH-NO
003697            ELSE
003698                IF AT-AUTH-RCVD >  SPACES
003699                   MOVE ' AUTH RCV'  TO CORR-VAR
003700                   MOVE SPACES       TO CORR-RESENT
003701                   MOVE AT-AUTH-RCVD TO CORR-RESENT(2:1)
003702                ELSE
003703                   MOVE SPACES       TO CORR-RESENT  CORR-VAR.
003704
003705*    IF PI-COMPANY-ID = 'DMD'
003706*        IF AT-DMD-LETTER-PURGE-DT NOT = SPACES AND LOW-VALUES
003707*            MOVE '  PURGED='             TO CORR-VAR
003708*            MOVE AT-DMD-LETTER-PURGE-DT  TO DC-BIN-DATE-1
003709*            MOVE ' '                     TO DC-OPTION-CODE
003710*            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003711*            MOVE DC-GREG-DATE-1-EDIT     TO CORR-RESENT.
003712
003713     MOVE CORRESPONDENCE-TEXT       TO DISPLAY-TEXT.
003714     MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
003715
003716     GO TO 2800-INCR-DISPLAY-CNT.
003717
003718     EJECT
003719 2400-GENERAL-INFO-TRAILER.
003720
003721     IF AT-PAYMENT-NOTE
003722         GO TO 2020-READ-TRAILER-LOOP.
003723
003724     IF AT-CONTINUED-NOTE
003725         GO TO 2020-READ-TRAILER-LOOP.
003726
003727     MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
003728
003729     IF AT-MAINT-NOTE
003730         MOVE 'MAINT NOTES'      TO DISPLAY-ACTION
003731     ELSE
003732         IF AT-CALL-NOTE
003733             MOVE 'CALL'         TO DISP-ACT-A
003734             EVALUATE TRUE
003735               WHEN AT-PHONE-CALL-IN
003736                 MOVE 'IN'       TO DISP-ACT-B
003737               WHEN AT-PHONE-CALL-OUT
003738                 MOVE 'OUT'      TO DISP-ACT-B
003739               WHEN OTHER
003740                 MOVE 'NEW'      TO DISP-ACT-B
003741             END-EVALUATE
003742         ELSE
003743             IF AT-CERT-CHANGE
003744                 MOVE 'CERT CHANGE'
003745                                 TO DISPLAY-ACTION
003746             ELSE IF AT-APPROVAL-NOTE
003747                 MOVE 'APPROVL REVIEW' TO DISPLAY-ACTION
003748             ELSE IF AT-NOTE-FILE-NOTE
003749                 MOVE 'NOTE & FILE' TO DISPLAY-ACTION
003750             ELSE
003751                 MOVE 'NOTE'     TO DISPLAY-ACTION.
003752
003753     MOVE AT-RECORDED-BY         TO DISPLAY-BY.
003754     MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
003755     MOVE ' '                    TO DC-OPTION-CODE.
003756     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003757     MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
003758
003759     MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
003760     MOVE AT-INFO-LINE-1         TO DISPLAY-TEXT.
003761     MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
003762     GO TO 2800-INCR-DISPLAY-CNT.
003763
003764     EJECT
003765 2500-AUTO-PROMPT-TRAILER.
003766     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
003767     MOVE '5'                    TO DC-OPTION-CODE.
003768     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003769
003770     IF DC-BIN-DATE-1 > AT-PROMPT-END-DT
003771         GO TO 2020-READ-TRAILER-LOOP.
003772
003773     MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
003774     MOVE 'REMINDER'             TO DISPLAY-ACTION.
003775     MOVE AT-RECORDED-BY         TO DISPLAY-BY.
003776
003777     MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
003778     MOVE ' '                    TO DC-OPTION-CODE.
003779     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003780     MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
003781
003782     MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
003783
003784     MOVE AT-PROMPT-END-DT       TO DC-BIN-DATE-1.
003785     MOVE ' '                    TO DC-OPTION-CODE.
003786     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003787     MOVE DC-GREG-DATE-1-EDIT    TO PROMPT-END.
003788
003789     MOVE AT-PROMPT-LINE-1       TO PROMPT-MSG.
003790     MOVE PROMPT-TEXT            TO DISPLAY-TEXT.
003791     MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
003792     GO TO 2800-INCR-DISPLAY-CNT.
003793
003794     EJECT
003795 2600-DENIAL-TRAILER.
003796     MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
003797     MOVE 'DENIAL'               TO DISPLAY-ACTION.
003798     MOVE AT-RECORDED-BY         TO DISPLAY-BY.
003799
003800     MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
003801     MOVE ' '                    TO DC-OPTION-CODE.
003802     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003803     MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
003804
003805     MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
003806
003807     IF AT-RETRACTION-DT NOT = LOW-VALUES
003808         MOVE AT-DENIAL-DT       TO DC-BIN-DATE-1
003809         MOVE ' '                TO DC-OPTION-CODE
003810         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003811         MOVE DC-GREG-DATE-1-EDIT TO DENIED-DATE
003812         MOVE AT-RETRACTION-DT   TO DC-BIN-DATE-1
003813         MOVE ' '                TO DC-OPTION-CODE
003814         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003815         MOVE DC-GREG-DATE-1-EDIT TO RECONSIDERED-DATE
003816         MOVE RECONSIDERED-TEXT   TO DISPLAY-TEXT
003817         MOVE TRAILER-DISPLAY-WORK-AREA
003818                                 TO MAP-DISPLAY (DISPLAY-CNT)
003819         GO TO 2800-INCR-DISPLAY-CNT.
003820
003821     MOVE AT-DENIAL-DT           TO DC-BIN-DATE-1.
003822     MOVE ' '                    TO DC-OPTION-CODE.
003823     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003824     MOVE DC-GREG-DATE-1-EDIT    TO DENIAL-DATE.
003825
003826     MOVE AT-DENIAL-REASON-CODE  TO DENIAL-CODE.
003827     MOVE AT-DENIAL-INFO-1       TO DENIAL-MSG.
003828
003829     IF CL-DENIAL-TYPE NOT = SPACES AND LOW-VALUES
003830        EVALUATE CL-DENIAL-TYPE
003831           WHEN '1'
003832              MOVE 'DENIAL STATUS - DENIAL       '
003833                                 TO DENTYPO
003834           WHEN '2'
003835              MOVE 'DENIAL STATUS - RESCISSION   '
003836                                 TO DENTYPO
003837           WHEN '3'
003838              MOVE 'DENIAL STATUS - REFORMATION  '
003839                                 TO DENTYPO
003840           WHEN '4'
003841              MOVE 'DENIAL STATUS - REF TO RESC  '
003842                                 TO DENTYPO
003843           WHEN '5'
003844              MOVE 'DENIAL STATUS - RECONSIDERED '
003845                                 TO DENTYPO
003846        END-EVALUATE
003847        MOVE AL-SANOF            TO DENTYPA
003848     END-IF
003849
003850     MOVE DENIAL-TEXT            TO DISPLAY-TEXT.
003851     MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
003852     GO TO 2800-INCR-DISPLAY-CNT.
003853
003854     EJECT
003855 2700-INCURRED-CHANGE-TRAILER.
003856     MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
003857     MOVE 'INCUR DATE CHG'       TO DISPLAY-ACTION.
003858     MOVE AT-RECORDED-BY         TO DISPLAY-BY.
003859
003860     MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
003861     MOVE ' '                    TO DC-OPTION-CODE.
003862     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003863     MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
003864
003865     MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
003866
003867     IF AT-OLD-INCURRED-DT NOT = LOW-VALUES
003868        MOVE AT-OLD-INCURRED-DT  TO DC-BIN-DATE-1
003869        MOVE ' '                 TO DC-OPTION-CODE
003870        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003871        IF DATE-CONVERSION-ERROR
003872           MOVE SPACES           TO INCUR-DT
003873        ELSE
003874           MOVE DC-GREG-DATE-1-EDIT  TO INCUR-DT
003875     ELSE
003876        MOVE SPACES              TO INCUR-DT.
003877
003878     IF PI-USES-PAID-TO
003879        MOVE ' PD  TO ='         TO PDTHRU-HEAD.
003880
003881     IF AT-OLD-PAID-THRU-DT NOT = LOW-VALUES
003882        MOVE AT-OLD-PAID-THRU-DT TO DC-BIN-DATE-1
003883        MOVE ' '                 TO DC-OPTION-CODE
003884        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003885        IF DATE-CONVERSION-ERROR
003886           MOVE SPACES           TO INCUR-PDTHRU
003887        ELSE
003888           MOVE DC-GREG-DATE-1-EDIT  TO INCUR-PDTHRU
003889           IF PI-USES-PAID-TO
003890              MOVE AT-OLD-PAID-THRU-DT TO DC-BIN-DATE-1
003891              MOVE '6'                 TO DC-OPTION-CODE
003892              MOVE +1                  TO DC-ELAPSED-DAYS
003893              MOVE +0                  TO DC-ELAPSED-MONTHS
003894              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003895              IF NO-CONVERSION-ERROR
003896                 MOVE DC-GREG-DATE-1-EDIT  TO INCUR-PDTHRU
003897              ELSE
003898                 NEXT SENTENCE
003899           ELSE
003900              NEXT SENTENCE
003901     ELSE
003902        MOVE SPACES              TO INCUR-PDTHRU.
003903
003904     MOVE AT-OLD-TOTAL-PAID      TO INCUR-PAID.
003905     MOVE INCUR-TEXT             TO DISPLAY-TEXT.
003906     MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
003907     GO TO 2800-INCR-DISPLAY-CNT.
003908
003909     EJECT
003910 2710-FORM-CONTROL-TRAILER.
003911     MOVE SPACES                 TO TRAILER-DISPLAY-WORK-AREA.
003912     MOVE 'FORM CTL'             TO DISP-ACT-A.
003913     IF INITIAL-FORM
003914        MOVE 'INIT'              TO DISP-ACT-B
003915     ELSE
003916        MOVE 'PROG'              TO DISP-ACT-B.
003917
003918     MOVE AT-RECORDED-BY         TO DISPLAY-BY.
003919
003920     MOVE AT-RECORDED-DT         TO DC-BIN-DATE-1.
003921     MOVE ' '                    TO DC-OPTION-CODE.
003922     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003923     MOVE DC-GREG-DATE-1-EDIT    TO DISPLAY-DATE.
003924
003925     MOVE TRLR-SEQ-NO            TO DISPLAY-SEQ.
003926
003927     IF AT-FORM-ADDRESS = 'I'
003928         MOVE 1                  TO WS-SUB
003929      ELSE
003930     IF AT-FORM-ADDRESS = 'A'
003931         MOVE 3                  TO WS-SUB
003932      ELSE
003933     IF AT-FORM-ADDRESS = 'O'
003934         MOVE 6                  TO WS-SUB
003935      ELSE
003936     IF AT-FORM-ADDRESS = 'Q'
003937         MOVE 7                  TO WS-SUB
003938      ELSE
003939         MOVE 0                  TO WS-SUB.
003940
003941     IF WS-SUB = 0
003942         MOVE SPACES             TO  CORR-TO
003943     ELSE
003944         MOVE CORR-DESC (WS-SUB) TO  CORR-TO.
003945
003946     IF AT-FORM-ANSWERED-DT NOT = LOW-VALUES
003947         MOVE AT-FORM-ANSWERED-DT TO DC-BIN-DATE-1
003948         MOVE ' '                 TO DC-OPTION-CODE
003949         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003950         MOVE DC-GREG-DATE-1-EDIT TO CORR-RECVD
003951     ELSE
003952         MOVE SPACES              TO CORR-RECVD.
003953
003954     IF AT-FORM-PRINTED-DT  = LOW-VALUES
003955         MOVE '    SEND='         TO CORR-VAR
003956         MOVE AT-FORM-SEND-ON-DT  TO DC-BIN-DATE-1
003957         MOVE ' '                 TO DC-OPTION-CODE
003958         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003959         MOVE DC-GREG-DATE-1-EDIT TO CORR-RESENT
003960     ELSE
003961         MOVE '    SENT='        TO CORR-VAR
003962         IF AT-FORM-REPRINT-DT  = LOW-VALUES
003963            MOVE AT-FORM-PRINTED-DT TO DC-BIN-DATE-1
003964            MOVE ' '                TO DC-OPTION-CODE
003965            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003966            MOVE DC-GREG-DATE-1-EDIT TO CORR-RESENT
003967         ELSE
003968            MOVE AT-FORM-REPRINT-DT TO DC-BIN-DATE-1
003969            MOVE ' '                TO DC-OPTION-CODE
003970            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003971            MOVE DC-GREG-DATE-1-EDIT    TO CORR-RESENT.
003972
003973     MOVE CORRESPONDENCE-TEXT       TO DISPLAY-TEXT.
003974     MOVE TRAILER-DISPLAY-WORK-AREA TO MAP-DISPLAY (DISPLAY-CNT).
003975
003976     GO TO 2800-INCR-DISPLAY-CNT.
003977
003978 2800-INCR-DISPLAY-CNT.
003979     IF LCP-ONCTR-01 =  0
003980         ADD 1 TO LCP-ONCTR-01
003981         MOVE 'Y'                TO PI-PREV-DISP
003982         MOVE TRLR-SEQ-NO        TO PI-SAVE-LOW  PI-SAVE-HIGH.
003983
003984     IF DIRECTION-SWITCH = 'F'
003985         MOVE TRLR-SEQ-NO        TO PI-SAVE-HIGH
003986         ADD +1                  TO DISPLAY-CNT
003987         IF DISPLAY-CNT > +4
003988             GO TO 2999-EXIT
003989         ELSE
003990             NEXT SENTENCE
003991     ELSE
003992         MOVE TRLR-SEQ-NO        TO PI-SAVE-LOW
003993         SUBTRACT +1 FROM DISPLAY-CNT
003994         if map-display (1) (1:14) = 'SPECIAL REVIEW'
003995           OR MAP-DISPLAY (1) (1:14) = 'VERIFY SSN    '
003996           OR MAP-DISPLAY (1) (1:14) = 'CAUSAL STATE  '
003997            IF MAP-DISPLAY (2) (1:14) = 'VERIFY SSN    '
003998              OR MAP-DISPLAY (2) (1:14) = 'CAUSAL STATE  '
003999                 IF MAP-DISPLAY (3) (1:14) = 'CAUSAL STATE  '
004000                     IF DISPLAY-CNT < +4
004001                        GO TO 2999-EXIT
004002                     END-IF
004003                 END-IF
004004                 IF DISPLAY-CNT < +3
004005                   GO TO 2999-EXIT
004006                 END-IF
004007            END-IF
004008            IF DISPLAY-CNT < +2
004009               GO TO 2999-EXIT
004010            END-IF
004011         ELSE
004012            IF DISPLAY-CNT < +1
004013               GO TO 2999-EXIT.
004014
004015     GO TO 2020-READ-TRAILER-LOOP.
004016
004017 2950-NO-MORE-TRAILERS.
004018     MOVE ER-0303                TO EMI-ERROR.
004019     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004020
004021 2999-EXIT.
004022     EXIT.
004023
004024     EJECT
004025
004026 3997-GET-ERPDEF.
004027
004028     MOVE PI-COMPANY-CD          TO ERPDEF-KEY
004029     MOVE CM-STATE               TO ERPDEF-STATE
004030     if cm-clp-state not = cm-state and spaces and zeros
004031        move cm-clp-state        to erpdef-state
004032     end-if
004033     MOVE am-dcc-product-code    TO ERPDEF-PROD-CD
004034     MOVE 'A'                    TO ERPDEF-BEN-TYPE
004035     MOVE CM-AH-BENEFIT-CD       TO ERPDEF-BEN-CODE
004036     MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
004037     MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
004038
004039     
      * EXEC CICS STARTBR
004040*        DATASET  ('ERPDEF')
004041*        RIDFLD   (ERPDEF-KEY)
004042*        GTEQ
004043*        RESP     (WS-RESPONSE)
004044*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00011577' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303131353737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004045
004046     IF NOT WS-RESP-NORMAL
004047        GO TO 3997-EXIT
004048     END-IF
004049
004050     IF WS-RESP-NORMAL
004051        
      * EXEC CICS READNEXT
004052*          DATASET  ('ERPDEF')
004053*          INTO     (PRODUCT-MASTER)
004054*          RIDFLD   (ERPDEF-KEY)
004055*          RESP     (WS-RESPONSE)
004056*       END-EXEC
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV12
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00011589' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303131353839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 PRODUCT-MASTER, 
                 DFHEIV12, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004057        IF WS-RESP-NORMAL
004058           IF (ERPDEF-KEY-SAVE (1:16) =
004059              PD-CONTROL-PRIMARY (1:16))
004060              AND (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
004061              PERFORM VARYING A1 FROM +1 BY +1 UNTIL
004062                 (A1 > +11)
004063                 OR (PD-PROD-CODE (A1) = cl-claim-type)
004064              END-PERFORM
004065              IF A1 < +12
004066                 SET PDEF-FOUND TO TRUE
004067              end-if
004068           END-IF
004069        END-IF
004070     END-IF
004071
004072     .
004073 3997-ENDBR.
004074
004075     
      * EXEC CICS ENDBR
004076*       DATASET  ('ERPDEF')
004077*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011613' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131363133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004078
004079     .
004080 3997-EXIT.
004081     EXIT.
004082
004083 6000-CHECK-AUTO-ACTIVITY.
004084******************************************************************
004085*    READ THE CONTROL FILE TO VERIFY THAT AN AUTOMATIC           *
004086*    ACTIVITY RECORD EXISTS.                                     *
004087******************************************************************
004088
004089     
      * EXEC CICS HANDLE CONDITION
004090*        NOTFND   (6000-NOT-FOUND)
004091*    END-EXEC.
      *    MOVE '"$I                   ! 4 #00011627' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3420233030303131363237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004092
004093     MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID
004094     MOVE 'T'                    TO  CNTL-REC-TYPE
004095     MOVE SPACES                 TO  CNTL-ACCESS
004096     MOVE +0                     TO  CNTL-SEQ-NO
004097     MOVE 'CNTL'                 TO  FILE-SWITCH.
004098
004099     
      * EXEC CICS READ
004100*        DATASET   ('ELCNTL')
004101*        RIDFLD    (ELCNTL-KEY)
004102*        SET       (ADDRESS OF CONTROL-FILE)
004103*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00011637' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303131363337' TO DFHEIV0
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
           
004104
004105     MOVE 'Y'                    TO  WS-ACT-REC-FOUND-SW.
004106     GO TO 6000-EXIT.
004107
004108 6000-NOT-FOUND.
004109     MOVE 'N'                    TO  WS-ACT-REC-FOUND-SW.
004110     MOVE AL-UABON               TO  ACTCDA.
004111     MOVE -1                     TO  ACTCDL.
004112     MOVE ER-3516                TO  EMI-ERROR
004113     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004114
004115 6000-EXIT.
004116     EXIT.
004117
004118     EJECT
004119 6050-AUTO-LETTER-WRITER.
004120******************************************************************
004121*    THE FOLLOWING ACTIVITIES WILL BE PERFORMED BASED ON THE     *
004122*    CODES ENTERED IN THE AUTOMATIC ACTIVITY RECORD.             *
004123*       1.  IF SPECIFIED, THE AUTOMATIC LETTER WRITER PROGRAM    *
004124*           WILL BE LINKED TO AND A LETTER WILL BE GENERATED.    *
004125*           A.  ANY RESEND OR FOLLOW UP DATES SPECIFIED IN THE   *
004126*               ACTIVITY RECORD WILL BE STORED IN THE LETTER     *
004127******************************************************************
004128
004129     IF WS-ACTIVITY-CODE = ZERO
004130         GO TO 6050-EXIT.
004131
004132     MOVE WS-ACTIVITY-CODE       TO  SUB.
004133     SUBTRACT 9 FROM SUB.
004134
004135 6050-EDIT-USER-ACTIVITY-LOOP.
004136
004137     IF (CF-USER-ACTIVE-SW (SUB) = ' ' OR 'N') OR
004138        (CF-USER-LETTER-ID (SUB) = SPACES OR LOW-VALUES)
004139             GO TO 6050-EXIT.
004140
004141 6050-START-AUTO-LETTER-WRITER.
004142
004143     MOVE LOW-VALUES                 TO  W-1523-LINKDATA.
004144     MOVE PROGRAM-INTERFACE-BLOCK    TO  W-1523-COMMON-PI-DATA.
004145
004146     MOVE CF-USER-LETTER-ID (SUB)    TO W-1523-FORM-NUMBER.
004147
004148     IF CF-USER-RESEND-DAYS (SUB) NOT = ZEROS
004149         MOVE SAVE-BIN-DATE          TO  DC-BIN-DATE-1
004150         MOVE '6'                    TO  DC-OPTION-CODE
004151         MOVE CF-USER-RESEND-DAYS (SUB)  TO  DC-ELAPSED-DAYS
004152         MOVE +0                     TO  DC-ELAPSED-MONTHS
004153         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
004154         IF NO-CONVERSION-ERROR
004155             MOVE DC-BIN-DATE-2      TO  W-1523-RESEND-DATE
004156         ELSE
004157             MOVE LOW-VALUES         TO  W-1523-RESEND-DATE.
004158
004159     IF CF-USER-FOLLOW-UP-DAYS (SUB) NOT = ZEROS
004160         MOVE SAVE-BIN-DATE          TO  DC-BIN-DATE-1
004161         MOVE '6'                    TO  DC-OPTION-CODE
004162         MOVE CF-USER-FOLLOW-UP-DAYS (SUB)   TO  DC-ELAPSED-DAYS
004163         MOVE +0                     TO  DC-ELAPSED-MONTHS
004164         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
004165         IF NO-CONVERSION-ERROR
004166             MOVE DC-BIN-DATE-2      TO  W-1523-FOLLOW-UP-DATE
004167         ELSE
004168             MOVE LOW-VALUES         TO  W-1523-FOLLOW-UP-DATE.
004169
004170******************************************************************
004171*    LINK TO THE AUTOMATIC LETTER WRITER PROGRAM.                *
004172******************************************************************
004173
004174     
      * EXEC CICS LINK
004175*        PROGRAM    (LINK-1523)
004176*        COMMAREA   (W-1523-LINKDATA)
004177*        LENGTH     (W-1523-COMM-LENGTH)
004178*    END-EXEC.
      *    MOVE '."C                   (   #00011712' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303131373132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-1523, 
                 W-1523-LINKDATA, 
                 W-1523-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004179
004180******************************************************************
004181*    CHECK THE ERROR CODES FROM THE AUTOMATIC LETTER WRITER      *
004182*    PROGRAM.                                                    *
004183*        1.  IF FATAL ERRORS ARE DETECTED BY THE AUTO LETTER     *
004184*            WRITER PROGRAM, THE LETTER IS NOT CREATED AND AN    *
004185*            ERROR MESSAGE (#0802) IS DISPLAYED INDICATING THAT  *
004186*            THE LETTER WAS NOT CREATED.                         *
004187*        2.  IF THE AUTO LETTER WRITER PROGRAM IS UNABLE TO      *
004188*            RESOLVE A VARIABLE, THE LETTER IS CREATED AND AN    *
004189*            ERROR MESSAGE (#0803) INDICATING THAT THE LETTER    *
004190*            WAS CREATED WITH ERRORS IS DISPLAYED.               *
004191******************************************************************
004192
004193     IF W-1523-ERROR-CODE = ZEROS
004194         GO TO 6050-EXIT.
004195
004196     IF W-1523-FATAL-ERROR
004197         MOVE ER-0802                TO  EMI-ERROR
004198         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004199
004200     IF W-1523-ERROR-CODE = 0191
004201         MOVE ER-0803                TO  EMI-ERROR
004202         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004203
004204 6050-EXIT.
004205     EXIT.
004206
004207     EJECT
004208 6100-RESET-AUTO-ACTIVITY.
004209******************************************************************
004210*    RESET ALL LETTER ACTIVITY AWAITING FURTHER ACTION (RESEND,  *
004211*    FOLLOW-UP, ETC) IF THE AUTOMATIC ACTIVITY CODE IS SET       *
004212*    TO ZEROS.                                                   *
004213******************************************************************
004214
004215     MOVE PI-COMPANY-CD          TO  TRLR-COMP-CD.
004216     MOVE PI-CARRIER             TO  TRLR-CARRIER.
004217     MOVE PI-CLAIM-NO            TO  TRLR-CLAIM-NO.
004218     MOVE PI-CERT-NO             TO  TRLR-CERT-NO.
004219     MOVE +100                   TO  TRLR-SEQ-NO.
004220     MOVE 'TRLR'                 TO  FILE-SWITCH.
004221
004222 6100-STARTBR-TRLR.
004223
004224     
      * EXEC CICS HANDLE CONDITION
004225*        ENDFILE  (6100-END-RESET)
004226*        NOTFND   (6100-END-RESET)
004227*    END-EXEC.
      *    MOVE '"$''I                  ! 5 #00011762' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'3520233030303131373632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004228
004229     
      * EXEC CICS STARTBR
004230*        DATASET   ('ELTRLR')
004231*        RIDFLD    (ELTRLR-KEY)
004232*        GTEQ
004233*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00011767' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303131373637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004234
004235     MOVE 'Y'                        TO  WS-BROWSE-SW.
004236
004237 6100-READ-NEXT.
004238
004239     
      * EXEC CICS READNEXT
004240*        DATASET   ('ELTRLR')
004241*        RIDFLD    (ELTRLR-KEY)
004242*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
004243*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00011777' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303131373737' TO DFHEIV0
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
           
004244
004245     IF (PI-COMPANY-CD NOT = TRLR-COMP-CD)   OR
004246        (PI-CARRIER    NOT = TRLR-CARRIER)   OR
004247        (PI-CLAIM-NO   NOT = TRLR-CLAIM-NO)  OR
004248        (PI-CERT-NO    NOT = TRLR-CERT-NO)
004249         GO TO 6100-END-RESET.
004250
004251     IF ELTRLR-KEY = PI-PREV-TRLR-KEY
004252         GO TO 6100-READ-NEXT.
004253
004254     IF AT-TRAILER-TYPE NOT = '4'
004255         GO TO 6100-READ-NEXT.
004256
004257     IF AT-LETTER-ANSWERED-DT NOT = LOW-VALUES AND SPACES
004258         GO TO 6100-READ-NEXT.
004259
004260     IF (AT-AUTO-RE-SEND-DT = LOW-VALUES OR SPACES)
004261       AND
004262        (AT-RECEIPT-FOLLOW-UP = LOW-VALUES OR SPACES)
004263         GO TO 6100-READ-NEXT.
004264
004265     IF AT-RECEIPT-FOLLOW-UP < SAVE-BIN-DATE AND
004266        AT-AUTO-RE-SEND-DT   < SAVE-BIN-DATE AND
004267        AT-RESEND-PRINT-DATE < SAVE-BIN-DATE
004268         GO TO 6100-READ-NEXT.
004269
004270     IF (AT-AUTO-RE-SEND-DT NOT = LOW-VALUES AND SPACES)
004271       AND
004272        (AT-RESEND-PRINT-DATE NOT = LOW-VALUES AND SPACES)
004273       AND
004274        (AT-RECEIPT-FOLLOW-UP = LOW-VALUES)
004275         GO TO 6100-READ-NEXT.
004276
004277 6100-END-BROWSE.
004278
004279     MOVE ELTRLR-KEY                 TO  PI-PREV-TRLR-KEY.
004280
004281     
      * EXEC CICS ENDBR
004282*        DATASET   ('ELTRLR')
004283*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011819' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131383139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004284
004285     MOVE 'N'                        TO  WS-BROWSE-SW.
004286
004287 6100-READ-TRLR-UPDATE.
004288
004289     
      * EXEC CICS READ
004290*        DATASET   ('ELTRLR')
004291*        RIDFLD    (ELTRLR-KEY)
004292*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
004293*        UPDATE
004294*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00011827' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131383237' TO DFHEIV0
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
           
004295
004296     MOVE 'Y'                        TO  WS-UPDATE-SW.
004297
004298     IF AT-AUTO-RE-SEND-DT IS NOT < SAVE-BIN-DATE
004299         MOVE LOW-VALUES             TO  AT-AUTO-RE-SEND-DT
004300         MOVE PI-COMPANY-CD          TO  ARCH-COMP-CD
004301         MOVE AT-LETTER-ARCHIVE-NO   TO  ARCH-ARCHIVE-NO
004302         MOVE '1'                    TO  ARCH-RECORD-TYPE
004303         MOVE +0                     TO  ARCH-SEQ-NO
004304         PERFORM 6200-READ-ARCH-UPDATE THRU 6200-EXIT.
004305
004306     IF AT-RECEIPT-FOLLOW-UP NOT < SAVE-BIN-DATE
004307         MOVE LOW-VALUES             TO  AT-RECEIPT-FOLLOW-UP.
004308
004309 6100-REWRITE-TRLR.
004310
004311     
      * EXEC CICS REWRITE
004312*        DATASET   ('ELTRLR')
004313*        FROM      (ACTIVITY-TRAILERS)
004314*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00011849' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131383439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004315
004316     GO TO 6100-STARTBR-TRLR.
004317
004318 6100-END-RESET.
004319
004320     IF WS-BROWSE-SW = 'Y'
004321         MOVE 'N'                    TO  WS-BROWSE-SW
004322         
      * EXEC CICS ENDBR
004323*            DATASET   ('ELTRLR')
004324*        END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011860' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131383630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004325
004326 6100-EXIT.
004327     EXIT.
004328
004329     EJECT
004330 6150-READ-TRLR92.
004331
004332     MOVE PI-COMPANY-CD          TO  WS-TRLR-COMP-CD.
004333     MOVE PI-CARRIER             TO  WS-TRLR-CARRIER.
004334     MOVE PI-CLAIM-NO            TO  WS-TRLR-CLAIM-NO.
004335     MOVE PI-CERT-NO             TO  WS-TRLR-CERT-NO.
004336     MOVE +092                   TO  WS-TRLR-SEQ-NO.
004337     MOVE 'TRLR'                 TO  FILE-SWITCH.
004338
004339     
      * EXEC CICS READ
004340*        DATASET   ('ELTRLR')
004341*        RIDFLD    (WS-ELTRLR-KEY)
004342*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
004343*        RESP      (WS-RESPONSE)
004344*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00011877' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303131383737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004345
004346     .
004347 6150-EXIT.
004348     EXIT.
004349
004350 6160-READ-TRLR93.
004351
004352     MOVE PI-COMPANY-CD          TO  WS-TRLR-COMP-CD.
004353     MOVE PI-CARRIER             TO  WS-TRLR-CARRIER.
004354     MOVE PI-CLAIM-NO            TO  WS-TRLR-CLAIM-NO.
004355     MOVE PI-CERT-NO             TO  WS-TRLR-CERT-NO.
004356     MOVE +093                   TO  WS-TRLR-SEQ-NO.
004357     MOVE 'TRLR'                 TO  FILE-SWITCH.
004358
004359     
      * EXEC CICS READ
004360*        DATASET   ('ELTRLR')
004361*        RIDFLD    (WS-ELTRLR-KEY)
004362*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
004363*        RESP      (WS-RESPONSE)
004364*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00011897' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303131383937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004365
004366     .
004367 6160-EXIT.
004368     EXIT.
004369
004370 6170-READ-TRLR94.
004371
004372     MOVE PI-COMPANY-CD          TO  WS-TRLR-COMP-CD.
004373     MOVE PI-CARRIER             TO  WS-TRLR-CARRIER.
004374     MOVE PI-CLAIM-NO            TO  WS-TRLR-CLAIM-NO.
004375     MOVE PI-CERT-NO             TO  WS-TRLR-CERT-NO.
004376     MOVE +094                   TO  WS-TRLR-SEQ-NO.
004377     MOVE 'TRLR'                 TO  FILE-SWITCH.
004378
004379     
      * EXEC CICS READ
004380*        DATASET   ('ELTRLR')
004381*        RIDFLD    (WS-ELTRLR-KEY)
004382*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
004383*        RESP      (WS-RESPONSE)
004384*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00011917' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303131393137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004385
004386     .
004387 6170-EXIT.
004388     EXIT.
004389
004390 6180-READ-TRLR95.
004391
004392     MOVE PI-COMPANY-CD          TO  WS-TRLR-COMP-CD.
004393     MOVE PI-CARRIER             TO  WS-TRLR-CARRIER.
004394     MOVE PI-CLAIM-NO            TO  WS-TRLR-CLAIM-NO.
004395     MOVE PI-CERT-NO             TO  WS-TRLR-CERT-NO.
004396     MOVE +095                   TO  WS-TRLR-SEQ-NO.
004397     MOVE 'TRLR'                 TO  FILE-SWITCH.
004398
004399     
      * EXEC CICS READ
004400*        DATASET   ('ELTRLR')
004401*        RIDFLD    (WS-ELTRLR-KEY)
004402*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
004403*        RESP      (WS-RESPONSE)
004404*    END-EXEC.
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00011937' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303131393337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004405
004406     .
004407 6180-EXIT.
004408     EXIT.
004409
004410 6200-READ-ARCH-UPDATE.
004411******************************************************************
004412*  READ AND UPDATE THE RESEND DATE ON THE LETTER ARCHIVE RECORD  *
004413******************************************************************
004414
004415     
      * EXEC CICS HANDLE CONDITION
004416*        NOTFND   (6200-EXIT)
004417*    END-EXEC.
      *    MOVE '"$I                   ! 6 #00011953' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3620233030303131393533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004418
004419     
      * EXEC CICS READ
004420*        DATASET   ('ELARCH')
004421*        RIDFLD    (ELARCH-KEY)
004422*        SET       (ADDRESS OF LETTER-ARCHIVE)
004423*        UPDATE
004424*    END-EXEC.
           MOVE 'ELARCH' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00011957' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131393537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004425
004426     MOVE LOW-VALUES                 TO  LA-RESEND-DATE.
004427
004428 6200-REWRITE-ARCH.
004429     
      * EXEC CICS REWRITE
004430*        DATASET   ('ELARCH')
004431*        FROM      (LETTER-ARCHIVE)
004432*    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
           MOVE 'ELARCH' TO DFHEIV1
      *    MOVE '&& L                  %   #00011967' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131393637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004433
004434 6200-EXIT.
004435     EXIT.
004436
004437     EJECT
004438 7000-START-BROWSE.
004439
004440     IF PI-COMPANY-ID = 'DMD'
004441             AND
004442        PI-ASS-PROCESSING
004443         GO TO 7200-BROWSING-ASS-FWRD.
004444
004445     MOVE LOW-VALUES             TO  ELMSTR-KEY.
004446     MOVE PI-COMPANY-CD          TO  MSTR-COMP-CD.
004447
004448     IF CLMNOL > 0
004449         MOVE CLMNOI             TO  MSTR-CLAIM-NO.
004450     IF CARRL > 0
004451         MOVE CARRI              TO  MSTR-CARRIER.
004452     IF CERTNOL > 0
004453         MOVE CERTNOI            TO  MSTR-CERT-NO-PRIME.
004454     IF SUFXL > 0
004455         MOVE SUFXI              TO  MSTR-CERT-NO-SUFX.
004456
004457     IF PI-PREV-CLAIM = LOW-VALUES
004458         NEXT SENTENCE
004459     ELSE
004460         IF CLMNOL  = 0 AND
004461            CARRL   = 0 AND
004462            CERTNOL = 0 AND
004463            SUFXL   = 0
004464                 MOVE PI-PREV-CLAIM  TO  ELMSTR-KEY.
004465
004466 7000-START-READING.
004467
004468     
      * EXEC CICS HANDLE CONDITION
004469*        ENDFILE  (8700-END-FILE)
004470*        NOTFND   (8600-NOT-FOUND)
004471*    END-EXEC.
      *    MOVE '"$''I                  ! 7 #00012006' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'3720233030303132303036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004472
004473     IF YESNOSWL > 0
004474         MOVE YESNOSWI TO CL-YESNOSW
004475     END-IF.
004476
004477     MOVE 'MSTR'                 TO FILE-SWITCH.
004478
004479     
      * EXEC CICS STARTBR
004480*        DATASET   (PI-CURRENT-FILE)
004481*        RIDFLD    (ELMSTR-KEY)
004482*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012017' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303132303137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004483
004484 7000-READ-NEXT.
004485
004486     
      * EXEC CICS READNEXT
004487*        DATASET   (PI-CURRENT-FILE)
004488*        RIDFLD    (ELMSTR-KEY)
004489*        INTO      (CLAIM-MASTER)
004490*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00012024' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303132303234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 CLAIM-MASTER, 
                 DFHEIV12, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004491
004492     IF ELMSTR-KEY = PI-PREV-CLAIM
004493         MOVE CL-CLAIM-NO        TO  PI-PREV-CLMNO
004494         GO TO 7000-READ-NEXT.
004495
004496     IF PI-COMPANY-CD NOT = CL-COMPANY-CD
004497         GO TO 8700-END-FILE.
004498
004499 7000-PICK-UP-LOGIC.
004500
004501     IF PI-COMPANY-ID  =  'CRI'
004502         IF CL-CLAIM-NO NOT = PI-PREV-CLMNO
004503             GO TO 8700-END-FILE.
004504
004505     IF PI-CARRIER-SECURITY > SPACES
004506         IF CL-CARRIER = PI-CARRIER-SECURITY
004507             NEXT SENTENCE
004508         ELSE
004509             GO TO 7000-READ-NEXT.
004510
004511     IF PI-ACCOUNT-SECURITY > SPACES
004512         IF CL-CERT-ACCOUNT = PI-ACCOUNT-SECURITY
004513             NEXT SENTENCE
004514         ELSE
004515             GO TO 7000-READ-NEXT.
004516
004517     GO TO 1000-READ-CLAIM.
004518                                 EJECT
004519 7200-BROWSING-ASS-FWRD.
004520
004521     COMPUTE PI-ASS-NDX = PI-ASS-NDX + 1.
004522
004523     IF PI-ASS-NDX > +24
004524             OR
004525        PI-ASS-CERTS (PI-ASS-NDX) NOT > LOW-VALUE
004526         IF PI-PF15-LAST
004527             MOVE PI-ORIGINAL-ASS-CERT
004528                                 TO ELMSTR-KEY
004529             MOVE ZEROS          TO PI-DAYS-PAID
004530                                    PI-TOTAL-PAID
004531             MOVE LOW-VALUES     TO PI-ASSOCIATED-CERTS-TABLE
004532                                    PI-LAST-PF-KEY-IND
004533                                    PI-ASSOCIATED-PROCESS-IND
004534             GO TO 7000-START-READING
004535         ELSE
004536             COMPUTE PI-ASS-NDX = PI-ASS-NDX - 1
004537             MOVE 'F'            TO PI-LAST-PF-KEY-IND
004538             MOVE -1             TO ENTERPFL
004539             MOVE ER-0936        TO EMI-ERROR
004540             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004541             GO TO 8200-SEND-DATAONLY.
004542
004543     MOVE PI-PREV-CLAIM          TO ELMSTR-KEY.
004544     MOVE PI-ASS-CERTS (PI-ASS-NDX)
004545                                 TO MSTR-CERT-NO.
004546     MOVE 'MSTR'                 TO FILE-SWITCH.
004547
004548     
      * EXEC CICS HANDLE CONDITION
004549*        NOTFND   (0450-NOT-FOUND)
004550*    END-EXEC.
      *    MOVE '"$I                   ! 8 #00012086' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3820233030303132303836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004551
004552     
      * EXEC CICS READ
004553*        DATASET   (PI-CURRENT-FILE)
004554*        RIDFLD    (ELMSTR-KEY)
004555*        INTO      (CLAIM-MASTER)
004556*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00012090' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132303930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004557
004558     GO TO 7000-PICK-UP-LOGIC.
004559
004560 7200-EXIT.
004561     EXIT.
004562
004563     EJECT
004564 7500-START-BROWSE.
004565
004566     IF PI-COMPANY-ID = 'DMD'
004567             AND
004568        PI-ASS-PROCESSING
004569         GO TO 7600-BROWSING-ASS-BWRD.
004570
004571     MOVE LOW-VALUES             TO  ELMSTR-KEY.
004572     MOVE PI-COMPANY-CD          TO  MSTR-COMP-CD.
004573
004574     IF CLMNOL > 0
004575         MOVE CLMNOI             TO  MSTR-CLAIM-NO.
004576     IF CARRL > 0
004577         MOVE CARRI              TO  MSTR-CARRIER.
004578     IF CERTNOL > 0
004579         MOVE CERTNOI            TO  MSTR-CERT-NO-PRIME.
004580     IF SUFXL > 0
004581         MOVE SUFXI              TO  MSTR-CERT-NO-SUFX.
004582
004583     IF PI-PREV-CLAIM = LOW-VALUES
004584         NEXT SENTENCE
004585     ELSE
004586         IF CLMNOL  = 0 AND
004587            CARRL   = 0 AND
004588            CERTNOL = 0 AND
004589            SUFXL   = 0
004590                 MOVE PI-PREV-CLAIM  TO  ELMSTR-KEY.
004591
004592 7500-START-READING.
004593
004594     
      * EXEC CICS HANDLE CONDITION
004595*        ENDFILE  (8700-END-FILE)
004596*        NOTFND   (8600-NOT-FOUND)
004597*    END-EXEC.
      *    MOVE '"$''I                  ! 9 #00012132' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'3920233030303132313332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004598
004599     MOVE 'MSTR'                 TO  FILE-SWITCH.
004600
004601     
      * EXEC CICS STARTBR
004602*        DATASET   (PI-CURRENT-FILE)
004603*        RIDFLD    (ELMSTR-KEY)
004604*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012139' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303132313339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004605
004606 7500-READ-PREV.
004607
004608     
      * EXEC CICS READPREV
004609*        DATASET   (PI-CURRENT-FILE)
004610*        RIDFLD    (ELMSTR-KEY)
004611*        INTO      (CLAIM-MASTER)
004612*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0IL                  )   #00012146' TO DFHEIV0
           MOVE X'2630494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303132313436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 CLAIM-MASTER, 
                 DFHEIV12, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004613
004614     IF ELMSTR-KEY = PI-PREV-CLAIM
004615         MOVE CL-CLAIM-NO        TO  PI-PREV-CLMNO
004616         GO TO 7500-READ-PREV.
004617
004618     IF PI-COMPANY-CD NOT = CL-COMPANY-CD
004619         GO TO 8700-END-FILE.
004620
004621 7500-PICK-UP-LOGIC.
004622
004623     IF PI-COMPANY-ID  =  'CRI'
004624         IF CL-CLAIM-NO NOT = PI-PREV-CLMNO
004625             GO TO 8700-END-FILE.
004626
004627     IF PI-CARRIER-SECURITY > SPACES
004628         IF CL-CARRIER = PI-CARRIER-SECURITY
004629             NEXT SENTENCE
004630         ELSE
004631             GO TO 7500-READ-PREV.
004632
004633     IF PI-ACCOUNT-SECURITY > SPACES
004634         IF CL-CERT-ACCOUNT = PI-ACCOUNT-SECURITY
004635             NEXT SENTENCE
004636         ELSE
004637             GO TO 7500-READ-PREV.
004638
004639     GO TO 1000-READ-CLAIM.
004640                                 EJECT
004641 7600-BROWSING-ASS-BWRD.
004642
004643     COMPUTE PI-ASS-NDX = PI-ASS-NDX - 1.
004644
004645     IF PI-ASS-NDX < +1
004646         IF PI-PF16-LAST
004647             MOVE PI-ORIGINAL-ASS-CERT
004648                                 TO ELMSTR-KEY
004649             MOVE ZEROS          TO PI-DAYS-PAID
004650                                    PI-TOTAL-PAID
004651             MOVE LOW-VALUES     TO PI-ASSOCIATED-CERTS-TABLE
004652                                    PI-LAST-PF-KEY-IND
004653                                    PI-ASSOCIATED-PROCESS-IND
004654             GO TO 7500-START-READING
004655         ELSE
004656             COMPUTE PI-ASS-NDX = PI-ASS-NDX + 1
004657             MOVE 'S'            TO PI-LAST-PF-KEY-IND
004658             MOVE -1             TO ENTERPFL
004659             MOVE ER-0937        TO EMI-ERROR
004660             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
004661             GO TO 8200-SEND-DATAONLY.
004662
004663     MOVE PI-PREV-CLAIM          TO ELMSTR-KEY.
004664     MOVE PI-ASS-CERTS (PI-ASS-NDX)
004665                                 TO MSTR-CERT-NO.
004666     MOVE 'MSTR'                 TO FILE-SWITCH.
004667
004668     
      * EXEC CICS HANDLE CONDITION
004669*        NOTFND   (0450-NOT-FOUND)
004670*    END-EXEC.
      *    MOVE '"$I                   ! : #00012206' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3A20233030303132323036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004671
004672     
      * EXEC CICS READ
004673*        DATASET   (PI-CURRENT-FILE)
004674*        RIDFLD    (ELMSTR-KEY)
004675*        INTO      (CLAIM-MASTER)
004676*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00012210' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132323130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-CURRENT-FILE, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004677
004678     GO TO 7500-PICK-UP-LOGIC.
004679
004680 7600-EXIT.
004681     EXIT.
004682                                 EJECT
004683 7650-CREATE-ACTQ.
004684
004685     
      * EXEC CICS HANDLE CONDITION
004686*        NOTFND(7650-CREATE-NEW-ACTQ)
004687*    END-EXEC.
      *    MOVE '"$I                   ! ; #00012223' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3B20233030303132323233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004688
004689     MOVE PI-COMPANY-CD          TO ACTQ-COMP-CD.
004690     MOVE PI-CARRIER             TO ACTQ-CARRIER.
004691     MOVE PI-CLAIM-NO            TO ACTQ-CLAIM-NO.
004692     MOVE PI-CERT-NO             TO ACTQ-CERT-NO.
004693
004694     MOVE 'ACTQ'                 TO FILE-SWITCH.
004695     MOVE 'ELACTQ  '             TO FILE-ID.
004696
004697     
      * EXEC CICS READ
004698*        UPDATE
004699*        DATASET   ('ELACTQ')
004700*        SET       (ADDRESS OF ACTIVITY-QUE)
004701*        RIDFLD    (ELACTQ-KEY)
004702*    END-EXEC.
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00012235' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303132323335' TO DFHEIV0
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
           
004703
004704     MOVE 'L'                    TO AQ-PENDING-CLAIM-RESTORE.
004705
004706     MOVE +150                   TO AQ-LAST-UPDATED-BY.
004707
004708     MOVE 'ELACTQ  '             TO FILE-ID.
004709
004710     
      * EXEC CICS REWRITE
004711*        DATASET   ('ELACTQ')
004712*        FROM      (ACTIVITY-QUE)
004713*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&& L                  %   #00012248' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303132323438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004714
004715     GO TO 7650-EXIT.
004716
004717 7650-CREATE-NEW-ACTQ.
004718
004719     
      * EXEC CICS GETMAIN
004720*        SET       (ADDRESS OF ACTIVITY-QUE)
004721*        LENGTH    (60)
004722*        INITIMG   (GETMAIN-SPACE)
004723*    END-EXEC.
           MOVE 60
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00012257' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303132323537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004724
004725     MOVE 'AQ'                   TO AQ-RECORD-ID.
004726     MOVE PI-COMPANY-CD          TO AQ-COMPANY-CD.
004727     MOVE PI-CARRIER             TO AQ-CARRIER.
004728     MOVE PI-CLAIM-NO            TO AQ-CLAIM-NO.
004729     MOVE PI-CERT-NO             TO AQ-CERT-NO.
004730
004731     MOVE 'L'                    TO AQ-PENDING-CLAIM-RESTORE.
004732
004733     MOVE +0                     TO AQ-PAYMENT-COUNTER
004734                                    AQ-PMT-UNAPPROVED-COUNT.
004735
004736     MOVE LOW-VALUES             TO AQ-RESEND-DATE
004737                                    AQ-FOLLOWUP-DATE.
004738
004739     MOVE +150                   TO AQ-LAST-UPDATED-BY.
004740
004741     MOVE 'ACTQ'                 TO FILE-SWITCH.
004742
004743     MOVE 'ELACTQ  '             TO FILE-ID.
004744
004745     
      * EXEC CICS WRITE
004746*        DATASET   ('ELACTQ')
004747*        FROM      (ACTIVITY-QUE)
004748*        RIDFLD    (AQ-CONTROL-PRIMARY)
004749*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
           MOVE 'ELACTQ' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00012283' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303132323833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004750
004751 7650-EXIT.
004752     EXIT.
004753                                  EJECT
004754 7700-CREATE-CLAIM.
004755
004756     
      * EXEC CICS HANDLE CONDITION
004757*        NOTFND   (7700-RETR-RECORD-NOT-FOUND)
004758*        DUPREC   (7700-DUPLICATE-CLAIM)
004759*    END-EXEC.
      *    MOVE '"$I%                  ! < #00012294' TO DFHEIV0
           MOVE X'222449252020202020202020' &
                X'202020202020202020202120' &
                X'3C20233030303132323934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004760
004761     IF CLMNOL > 0
004762         MOVE CLMNOI             TO PI-CLAIM-NO.
004763
004764     IF CARRL > 0
004765         MOVE CARRI              TO PI-CARRIER.
004766
004767     IF CERTNOL > 0
004768         MOVE CERTNOI            TO PI-CERT-PRIME.
004769
004770     IF SUFXL > 0
004771         MOVE SUFXI              TO PI-CERT-SFX.
004772
004773     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
004774     MOVE PI-CARRIER             TO MSTR-CARRIER.
004775     MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO.
004776     MOVE PI-CERT-NO             TO MSTR-CERT-NO.
004777     MOVE +1                     TO PI-ASS-NDX.
004778
004779 7700-CONT-MOVE.
004780
004781     IF PI-ASS-PROCESSING
004782         MOVE PI-ASS-CERTS (PI-ASS-NDX)
004783                                 TO MSTR-CERT-NO.
004784
004785     MOVE ELMSTR-KEY             TO W-ELRETR-KEY.
004786
004787     
      * EXEC CICS READ
004788*        DATASET   ('ELRETR')
004789*        SET       (ADDRESS OF RETRIEVE-MASTER)
004790*        RIDFLD    (W-ELRETR-KEY)
004791*    END-EXEC.
           MOVE 'ELRETR' TO DFHEIV1
      *    MOVE '&"S        E          (   #00012325' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132333235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ELRETR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RETRIEVE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004792
004793     
      * EXEC CICS GETMAIN
004794*        SET       (ADDRESS OF CLAIM-MASTER-L)
004795*        LENGTH    (350)
004796*        INITIMG   (GETMAIN-SPACE)
004797*    END-EXEC.
           MOVE 350
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00012331' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303132333331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF CLAIM-MASTER-L TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004798
004799     MOVE RETRIEVE-MASTER        TO CLAIM-MASTER.
004800     MOVE 'CL'                   TO CL-RECORD-ID.
004801     MOVE SAVE-BIN-DATE          TO CL-RESTORED-DT.
004802
004803     
      * EXEC CICS WRITE
004804*        DATASET   ('ELMSTR')
004805*        FROM      (CLAIM-MASTER)
004806*        RIDFLD    (CL-CONTROL-PRIMARY)
004807*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00012341' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303132333431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 CL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004808
004809     
      * EXEC CICS DELETE
004810*        FILE      ('ELRETR')
004811*        RIDFLD    (RL-CONTROL-PRIMARY)
004812*    END-EXEC.
           MOVE 'ELRETR' TO DFHEIV1
      *    MOVE '&(  R                 &   #00012347' TO DFHEIV0
           MOVE X'262820205220202020202020' &
                X'202020202020202020202620' &
                X'2020233030303132333437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 RL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004813
004814     IF PI-ASS-PROCESSING
004815         ADD +1                  TO PI-ASS-NDX
004816         IF PI-ASS-NDX < +25
004817                 AND
004818            PI-ASS-CERTS (PI-ASS-NDX) > SPACES
004819             GO TO 7700-CONT-MOVE.
004820
004821     MOVE 'MSTR'                 TO FILE-SWITCH.
004822     MOVE 'ELMSTR'               TO PI-CURRENT-FILE.
004823     MOVE LOW-VALUES             TO PI-RETRIEVED-DATA-IND.
004824     MOVE XCTL-126               TO PI-CALLING-PROGRAM.
004825     MOVE XCTL-132               TO PGM-NAME.
004826     GO TO 9300-XCTL.
004827
004828 7700-RETR-RECORD-NOT-FOUND.
004829
004830     MOVE ER-0929                TO EMI-ERROR.
004831     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004832     MOVE -1                     TO CLMNOL.
004833     MOVE PI-CARRIER             TO CARRO.
004834     MOVE PI-CLAIM-NO            TO CLMNOO.
004835     MOVE PI-CERT-PRIME          TO CERTNOO.
004836     MOVE PI-CERT-SFX            TO SUFXO.
004837     MOVE AL-UABON               TO CLMNOA   CARRA
004838                                    CERTNOA  SUFXA.
004839     GO TO 8100-SEND-INITIAL-MAP.
004840
004841 7700-DUPLICATE-CLAIM.
004842
004843     MOVE ER-0928                TO EMI-ERROR.
004844     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004845     MOVE -1                     TO CLMNOL.
004846     MOVE PI-CARRIER             TO CARRO.
004847     MOVE PI-CLAIM-NO            TO CLMNOO.
004848     MOVE PI-CERT-PRIME          TO CERTNOO.
004849     MOVE PI-CERT-SFX            TO SUFXO.
004850     MOVE AL-UABON               TO CLMNOA   CARRA
004851                                    CERTNOA  SUFXA.
004852     GO TO 8100-SEND-INITIAL-MAP.
004853
004854 7700-EXIT.
004855     EXIT.
004856                                  EJECT
004857 8000-LOAD-ERROR-MESSAGES.
004858     IF EMI-NO-ERRORS
004859         GO TO 8000-EXIT.
004860
004861     IF EMI-NUMBER-OF-LINES = 1
004862         MOVE EMI-LINE1          TO ERRMSG1O
004863         GO TO 8000-EXIT.
004864
004865     MOVE EMI-LINE1              TO ERRMSG1O.
004866
004867 8000-EXIT.
004868     EXIT.
004869
004870 8100-SEND-INITIAL-MAP.
004871
004872     MOVE SAVE-DATE              TO RUNDTEO.
004873     MOVE EIBTIME                TO TIME-IN.
004874     MOVE TIME-OUT               TO RUNTIMEO.
004875     MOVE PI-COMPANY-ID          TO CMPNYIDO.
004876     MOVE PI-PROCESSOR-ID        TO USERIDO.
004877     PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
004878
004879     IF PI-USES-PAID-TO
004880        MOVE 'PAID  TO  DT :'     TO PDTHRHDO.
004881
004882     IF PI-RETRIEVED-DATA
004883         MOVE 'RETRIEVE MASTER'  TO RETMSTO.
004884
004885     IF PI-COMPANY-ID = 'DMD'
004886         MOVE 'PF11=FORM REF'    TO PFKEY11O
004887         MOVE 'LST PMT AMT:'     TO COVERDO
004888         MOVE 'LST PMT DT :'     TO PRMTYPDO
004889         MOVE 'COVERAGE   :'     TO CRTSTADO.
004890
004891     IF PI-APPROVAL-LEVEL = '4' OR '5'
004892         MOVE AL-UNNOF         TO TOTINTA
004893     END-IF
004894
004895     
      * EXEC CICS SEND
004896*        MAP      (MAP-NAME)
004897*        MAPSET   (MAPSET-NAME)
004898*        FROM     (EL150AO)
004899*        ERASE
004900*        CURSOR
004901*    END-EXEC.
           MOVE LENGTH OF
            EL150AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00012433' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303132343333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150AO, 
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
           
004902
004903     GO TO 9100-RETURN-TRAN.
004904
004905     .
004906 8200-SEND-DATAONLY.
004907     MOVE SAVE-DATE              TO RUNDTEO.
004908     MOVE EIBTIME                TO TIME-IN.
004909     MOVE TIME-OUT               TO RUNTIMEO.
004910     MOVE PI-COMPANY-ID          TO CMPNYIDO.
004911     MOVE PI-PROCESSOR-ID        TO USERIDO.
004912     PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
004913
004914     MOVE PI-CARRIER             TO CARRO.
004915     MOVE PI-CLAIM-NO            TO CLMNOO.
004916     MOVE PI-CERT-PRIME          TO CERTNOO.
004917     MOVE PI-CERT-SFX            TO SUFXO.
004918
004919     IF PI-USES-PAID-TO
004920        MOVE 'PAID  TO  DT :'     TO PDTHRHDO.
004921
004922     IF PI-RETRIEVED-DATA
004923         MOVE 'RETRIEVE MASTER'  TO RETMSTO.
004924
004925     IF PI-COMPANY-ID = 'DMD'
004926         MOVE 'PF11=FORM REF'    TO PFKEY11O
004927         MOVE 'LST PMT AMT:'     TO COVERDO
004928         MOVE 'LST PMT DT :'     TO PRMTYPDO
004929         MOVE 'COVERAGE   :'     TO CRTSTADO.
004930
004931     IF PI-APPROVAL-LEVEL = '4' OR '5'
004932         MOVE AL-UNNOF         TO TOTINTA
004933     END-IF
004934
004935     
      * EXEC CICS SEND
004936*        MAP      (MAP-NAME)
004937*        MAPSET   (MAPSET-NAME)
004938*        FROM     (EL150AO)
004939*        DATAONLY
004940*        CURSOR
004941*    END-EXEC.
           MOVE LENGTH OF
            EL150AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00012473' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303132343733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150AO, 
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
           
004942
004943     GO TO 9100-RETURN-TRAN.
004944
004945 8250-SEND-WITH-CURSOR.
004946     MOVE SAVE-DATE              TO RUNDTEO.
004947     MOVE EIBTIME                TO TIME-IN.
004948     MOVE TIME-OUT               TO RUNTIMEO.
004949     MOVE PI-COMPANY-ID          TO CMPNYIDO.
004950     MOVE PI-PROCESSOR-ID        TO USERIDO.
004951     PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
004952
004953     IF PI-USES-PAID-TO
004954        MOVE 'PAID  TO  DT :'     TO PDTHRHDO.
004955
004956     IF PI-COMPANY-ID = 'DMD'
004957         MOVE 'PF11=FORM REF'    TO PFKEY11O
004958         MOVE 'LST PMT AMT:'     TO COVERDO
004959         MOVE 'LST PMT DT :'     TO PRMTYPDO
004960         MOVE 'COVERAGE   :'     TO CRTSTADO.
004961
004962     IF PI-APPROVAL-LEVEL = '4' OR '5'
004963         MOVE AL-UNNOF         TO TOTINTA
004964     END-IF
004965
004966     
      * EXEC CICS SEND
004967*        MAP      (MAP-NAME)
004968*        MAPSET   (MAPSET-NAME)
004969*        FROM     (EL150AO)
004970*        ERASE
004971*        CURSOR   (PI-SAVE-CURSOR)
004972*    END-EXEC.
           MOVE LENGTH OF
            EL150AO
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00012504' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303132353034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL150AO, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 PI-SAVE-CURSOR, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004973
004974     GO TO 9100-RETURN-TRAN.
004975
004976 8300-SEND-TEXT.
004977     
      * EXEC CICS SEND TEXT
004978*        FROM     (LOGOFF-TEXT)
004979*        LENGTH   (LOGOFF-LENGTH)
004980*        ERASE
004981*        FREEKB
004982*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00012515' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303132353135' TO DFHEIV0
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
           
004983
004984     
      * EXEC CICS RETURN
004985*    END-EXEC.
      *    MOVE '.(                    ''   #00012522' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303132353232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004986
004987 8500-FILE-NOTOPEN.
004988     IF FILE-SWITCH = 'MSTR'
004989         MOVE ER-0154            TO EMI-ERROR.
004990
004991     IF FILE-SWITCH = 'TRLR'
004992         MOVE ER-0172            TO EMI-ERROR.
004993
004994     IF FILE-SWITCH = 'CERT'
004995         MOVE ER-0169            TO EMI-ERROR.
004996
004997     IF FILE-SWITCH = 'CNTL'
004998         MOVE ER-0042            TO EMI-ERROR.
004999
005000     IF FILE-SWITCH = 'ACTQ'
005001         MOVE ER-0338            TO EMI-ERROR.
005002
005003     IF FILE-SWITCH = 'ACCT'
005004         MOVE ER-0168            TO EMI-ERROR.
005005
005006     IF FILE-SWITCH = 'PLCY'
005007         MOVE ER-9883            TO EMI-ERROR.
005008
005009     IF FILE-SWITCH = 'PLAN'
005010         MOVE ER-9808            TO EMI-ERROR.
005011
005012     IF FILE-SWITCH = 'PROD'
005013         MOVE ER-9886            TO EMI-ERROR.
005014
005015     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
005016
005017     IF FILE-SWITCH = 'CERT' AND
005018       (PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
005019        GO TO 1050-SHOW-CONTINUE.
005020
005021     MOVE -1                     TO FILETOL.
005022
005023     IF PASS-SWITCH = 'A'
005024         GO TO 8100-SEND-INITIAL-MAP
005025     ELSE
005026         GO TO 8200-SEND-DATAONLY.
005027
005028 8600-NOT-FOUND.
005029     MOVE AL-UABON               TO CLMNOA    CARRA
005030                                    CERTNOA   SUFXA.
005031     MOVE -1                     TO ENTERPFL.
005032     MOVE ER-0204                TO EMI-ERROR.
005033     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
005034     GO TO 8200-SEND-DATAONLY.
005035
005036 8700-END-FILE.
005037
005038     MOVE -1                     TO ENTERPFL.
005039     MOVE ER-0130                TO EMI-ERROR.
005040     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005041     GO TO 8200-SEND-DATAONLY.
005042
005043 8800-UNAUTHORIZED-ACCESS.
005044     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
005045     GO TO 8300-SEND-TEXT.
005046
005047 8810-PF23.
005048     MOVE EIBAID                 TO PI-ENTRY-CD-1.
005049     MOVE XCTL-005               TO PGM-NAME.
005050     GO TO 9300-XCTL.
005051
005052 8900-GET-FREE-LOOK.
005053     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
005054     MOVE SPACES                 TO CNTL-ACCESS.
005055     MOVE PI-STATE               TO CNTL-ACCESS.
005056     MOVE '3'                    TO CNTL-REC-TYPE.
005057     MOVE +0                     TO CNTL-SEQ-NO.
005058
005059     
      * EXEC CICS READ
005060*        DATASET   ('ELCNTL')
005061*        SET       (ADDRESS OF CONTROL-FILE)
005062*        RIDFLD    (ELCNTL-KEY)
005063*        RESP      (WS-RESPONSE)
005064*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00012597' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303132353937' TO DFHEIV0
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
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005065
005066     IF WS-RESP-NOTFND
005067        MOVE ER-2848             TO EMI-ERROR
005068        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005069        GO TO 8100-SEND-INITIAL-MAP
005070     ELSE
005071        MOVE CF-ST-FREE-LOOK-PERIOD
005072                                 TO CP-FREE-LOOK.
005073
005074 8900-EXIT.
005075     EXIT.
005076
005077 9100-RETURN-TRAN.
005078     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
005079     MOVE '150A'                 TO PI-CURRENT-SCREEN-NO.
005080
005081     
      * EXEC CICS RETURN
005082*        TRANSID    (TRANS-ID)
005083*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
005084*        LENGTH     (PI-COMM-LENGTH)
005085*    END-EXEC.
      *    MOVE '.(CT                  ''   #00012619' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303132363139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005086
005087 9200-RETURN-MAIN-MENU.
005088     MOVE XCTL-126               TO PGM-NAME.
005089     GO TO 9300-XCTL.
005090
005091 9300-XCTL.
005092     
      * EXEC CICS XCTL
005093*        PROGRAM    (PGM-NAME)
005094*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
005095*        LENGTH     (PI-COMM-LENGTH)
005096*    END-EXEC.
      *    MOVE '.$C                   %   #00012630' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303132363330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005097
005098 9400-CLEAR.
005099     MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
005100     GO TO 9300-XCTL.
005101
005102 9500-PF12.
005103     MOVE XCTL-010               TO PGM-NAME.
005104     GO TO 9300-XCTL.
005105
005106 9600-PGMID-ERROR.
005107     
      * EXEC CICS HANDLE CONDITION
005108*        PGMIDERR   (8300-SEND-TEXT)
005109*    END-EXEC.
      *    MOVE '"$L                   ! = #00012645' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'3D20233030303132363435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005110
005111     MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
005112     MOVE ' '                    TO PI-ENTRY-CD-1.
005113     MOVE XCTL-005               TO PGM-NAME.
005114     MOVE PGM-NAME               TO LOGOFF-PGM.
005115     MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
005116     GO TO 9300-XCTL.
005117
005118 9700-LINK-DATE-CONVERT.
005119     MOVE LINK-ELDATCV           TO PGM-NAME.
005120
005121     
      * EXEC CICS LINK
005122*        PROGRAM    (PGM-NAME)
005123*        COMMAREA   (DATE-CONVERSION-DATA)
005124*        LENGTH     (DC-COMM-LENGTH)
005125*    END-EXEC.
      *    MOVE '."C                   (   #00012659' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303132363539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005126
005127 9700-EXIT.
005128     EXIT.
005129
005130 9800-LINK-REM-TERM.
005131     MOVE LINK-ELRTRM            TO PGM-NAME.
005132
005133     
      * EXEC CICS LINK
005134*        PROGRAM    (PGM-NAME)
005135*        COMMAREA   (CALCULATION-PASS-AREA)
005136*        LENGTH     (CP-COMM-LENGTH)
005137*    END-EXEC.
      *    MOVE '."C                   (   #00012671' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303132363731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005138
005139 9800-EXIT.
005140     EXIT.
005141                                 EJECT
005142 9830-DMD-REMAINING-TERM.
005143
005144     DIVIDE CL-NO-OF-DAYS-PAID BY +30
005145         GIVING W-PAID-MONTHS REMAINDER W-PAID-DAYS.
005146
005147     MOVE W-PAID-MONTHS          TO W-PYMTS.
005148     MOVE W-PAID-DAYS            TO W-ADD-DAYS.
005149
005150     COMPUTE W-TERM-IN-DAYS = CM-AH-ORIG-TERM * 30
005151                            + CM-PMT-EXTENSION-DAYS.
005152
005153     COMPUTE W-REM-TERM-IN-DAYS = W-TERM-IN-DAYS
005154                                - CL-NO-OF-DAYS-PAID.
005155
005156     DIVIDE W-REM-TERM-IN-DAYS BY +30
005157         GIVING W-REM REMAINDER W-REM-DAYS.
005158
005159     MOVE W-REM                  TO WST-REM.
005160
005161     IF CM-PMT-EXTENSION-DAYS > ZEROS
005162         MOVE '/'                TO WST-SLASH1
005163         MOVE CM-PMT-EXTENSION-DAYS TO WST-EXT-DAYS
005164     ELSE
005165         MOVE SPACES             TO WST-ORIG-DAYS-GRP.
005166
005167     IF W-REM-DAYS > ZEROS
005168         MOVE '/'                TO WST-SLASH2
005169         MOVE W-REM-DAYS         TO WST-REM-DAYS
005170     ELSE
005171         MOVE SPACES             TO WST-REM-DAYS-GRP.
005172
005173 9830-EXIT.
005174     EXIT.
005175
005176 9850-LINK-REM-AMT.
005177     MOVE 'ELRAMT  '             TO PGM-NAME.
005178
005179     
      * EXEC CICS LINK
005180*        PROGRAM    (PGM-NAME)
005181*        COMMAREA   (CALCULATION-PASS-AREA)
005182*        LENGTH     (CP-COMM-LENGTH)
005183*    END-EXEC.
      *    MOVE '."C                   (   #00012717' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303132373137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005184
005185 9850-EXIT.
005186     EXIT.
005187
005188 9900-ERROR-FORMAT.
005189     IF NOT EMI-ERRORS-COMPLETE
005190         MOVE LINK-001           TO PGM-NAME
005191         
      * EXEC CICS LINK
005192*            PROGRAM    (PGM-NAME)
005193*            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
005194*            LENGTH     (EMI-COMM-LENGTH)
005195*        END-EXEC.
      *    MOVE '."C                   (   #00012729' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303132373239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005196
005197 9900-EXIT.
005198     EXIT.
005199
005200 9990-ABEND.
005201     MOVE -1 TO FILETOL.
005202     MOVE LINK-004 TO PGM-NAME.
005203
005204     MOVE DFHEIBLK               TO EMI-LINE1
005205     
      * EXEC CICS LINK
005206*        PROGRAM   (PGM-NAME)
005207*        COMMAREA  (EMI-LINE1)
005208*        LENGTH    (72)
005209*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00012743' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303132373433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005210
005211     MOVE EMI-LINE1              TO ERRMSG1O.
005212     GO TO 8200-SEND-DATAONLY.
005213
005214 EJECT
005215 9995-SECURITY-VIOLATION.
005216*                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00012772' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303132373732' TO DFHEIV0
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
005217

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL150' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 1000-SHOW-CLAIM,
                     0100-FIRST-TIME-IN,
                     8500-FILE-NOTOPEN,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0450-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0360-CREATE-NEW-ACTQ
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0385-TERMID-ERROR,
                     0390-TRANS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0450-NOT-FOUND,
                     0440-UPDATE-DONE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 0690-QIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1100-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1005-END-BUILD-DIAGNOSIS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 1100-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 1005-NO-ERACCT,
                     1005-NO-ERACCT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 1008-NOTFND,
                     1008-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 1009-CONTINUE-BUILD,
                     1009-CONTINUE-BUILD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 1050-SHOW-CONTINUE,
                     1050-SHOW-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 1220-NO-CLAIM-RECORD-FND,
                     1200-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 1500-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 1500-NO-PLAN-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 1700-ENDBROWSE,
                     1700-ENDBROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 2950-NO-MORE-TRAILERS,
                     2950-NO-MORE-TRAILERS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 6000-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 6100-END-RESET,
                     6100-END-RESET
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 6200-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 8700-END-FILE,
                     8600-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 0450-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 8700-END-FILE,
                     8600-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 0450-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 7650-CREATE-NEW-ACTQ
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 7700-RETR-RECORD-NOT-FOUND,
                     7700-DUPLICATE-CLAIM
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL150' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
