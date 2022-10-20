      *((program: EL131.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL131 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 04/03/95 18:43:40.
000007*                            VMOD=2.046.
000008*
000009*AUTHOR.        LOGIC, INC.
000010*               DALLAS, TEXAS.
000011*REMARKS. TRANSACTION EX24 - CLAIM MAINTENANCE.
000012*
000013******************************************************************
000014*                   C H A N G E   L O G
000015*
000016* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000017*-----------------------------------------------------------------
000018*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000019* EFFECTIVE    NUMBER
000020*-----------------------------------------------------------------
000021* 101501    2001100100006  SMVA  ADD USERID, POPULATE COMPO
000022* 062602    2002030700006  PEMA  Add note type of 'S'
000023*                                  (special review)
000024* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
000025* 071508    2008071000003  AJRA  ADD EDIT FOR SOC SEC NUMBER
000026* 012009    2007042600001  PEMA  RESTRICT CLAIM TYPE FOR CID
000027* 032612    2011110200001  PEMA  AHL CHANGES
000028* 040412    2012040400003  AJRA  DO NOT DELETE CERT ON AHL
000029* 032613    2013032500002  AJRA  PROTECT CERT#,CARR,GRP,ST,ACCT,EF
000030* 052113    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
000031* 060413    2013060300001  AJRA  CHECK FOR AUTO PAY WHEN BENE REMO
000032* 071613    2013071200001  PEMA  REMOVE INC & RPT DATE EDITS.
000033* 080613    2013071600001  PEMA  ALLOW INC DTE CHANGE
000034* 082013    2013040900001  AJRA  ADD PF15 BENEFICIARY MAINT
000035* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
000036* 111113    2013110600002  AJRA  CHG LEVEL 5 RESTRICTIONS TO LEVEL
000037* 040814    2014030500002  AJRA  ADD ICD CODES
000038* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000039* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000040* 070714    2014052800001  PEMA  correct read on erpdef for DCC
000041* 031715  CR2015022700002  PEMA  CHECK CHG IN DIAGNOSIS
000042* 031815    2015021700001  TANA  BENE EDIT IF A PAYMENT PENDING
000043* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
000044* 040617  CR2017022200003  TANA  INCREASE CHG INC DT TO 180 DAYS
000045* 062217  CR2017050300002  TANA  ADD AUTH RCVD FIELD
000046* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
000047* 101917  CR2017083000003  TANA  ADD CONTRACT CONTESTABLE MSG
000048* 021418  CR2017110200001  TANA  EDIT NAME CHANGE AGST INSRD TYPE
000049* 052918  CR2018031500002  TANA  Add Message for filing time limit
000050* 061418  IR2018053000003  TANA  Fix Causal state / filing limit
000051* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000052* 011020  IR2019121100004  PEMA  ADD last name to compare.
000053* 071720  CR2019112600001  TANA  Remove filing time limit error
000054* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000055* 090821  CR2021081200003  PEMA  Add error if inc > act cnc dt
000056* 080322  CR2021100800003  TANA  Add B and H claim types
000057******************************************************************
000058
000059
000060     EJECT
000061 ENVIRONMENT DIVISION.
000062
000063 DATA DIVISION.
000064
000065 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000066
000067 77  FILLER  PIC X(32)  VALUE '********************************'.
000068 77  FILLER  PIC X(32)  VALUE '*   EL131  WORKING STORAGE     *'.
000069 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.046 *********'.
000070 77  s1                          pic s999 comp-3 value+0.
000071 77  s2                          pic s999 comp-3 value+0.
000072 77  a1                          pic s999 comp-3 value+0.
000073 77  WS-CRITICAL-PERIOD          PIC 99    VALUE ZEROS.
000074 77  WS-CRIT-PER-RTW-MOS         PIC 99    VALUE ZEROS.
000075 77  WS-EXCL-PERIOD              PIC S999 COMP-3 VALUE +0.
000076 77  ws-pre-exsist               pic s999 comp-3 value +0.
000077 77  WS-COV-ENDS                 PIC S999 COMP-3 VALUE +0.
000078 77  WS-ACC-PERIOD               PIC S999 COMP-3 VALUE +0.
000079 77  ws-max-extension            pic s999 comp-3 value +0.
000080 77  ws-max-moben                pic s9(7)v99 comp-3 value +0.
000081 77  WS-MONTHS-BETWEEN           PIC S999 COMP-3 VALUE +0.
000082 77  WS-ERPDEF-SW                PIC X     VALUE ' '.
000083     88  ERPDEF-FOUND                 VALUE 'Y'.
000084 77  WS-TRLR-FILE-EOF            PIC X     VALUE ' '.
000085     88  TRLR-FILE-EOF                VALUE 'Y'.
000086 77  WS-PAYMENT-PENDING          PIC X     VALUE ' '.
000087     88  PAYMENT-PENDING              VALUE 'Y'.
000088 77  WS-AUTH-RCVD                PIC X     VALUE ' '.
000089     88  AUTH-RCVD                    VALUE 'Y'.
000090     88  NO-AUTH-RCVD                 VALUE 'N'.
000091
000092 77  ws-eracct-sw                pic x  value ' '.
000093     88  acct-found                value 'Y'.
000094 77  ws-benper-sw                pic x   value ' '.
000095     88  good-benper               value 'Y'.
000096 77  ws-claim-type               pic x value ' '.
000097 77  ws-ben-per                  pic 99 value 00.
000098 77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
000099 77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
000100 77  ws-pd-bens                  pic s9(5) comp-3 value +0.
000101 77  ws-max-bens                 pic s9(5) comp-3 value +0.
000102 77  sub                         pic s999 comp-3 value +0.
000103 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
000104
000105 01  LCP-TIME-OF-DAY-XX.
000106     05  LCP-TIME-OF-DAY-68        PIC 9(6).
000107     05  FILLER                    PIC 99.
000108 01  LCP-CICS-TIME                 PIC 9(15).
000109*                            COPY ELCSCTM.
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
000110
000111*                            COPY ELCSCRTY.
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
000112
000113*                            COPY ELCNWA.
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
000114
000115 01  ws-dcc-error-line.
000116     05  filler occurs 15.
000117         10  ws-error-no        pic x(4).
000118 01  WS-DATE-AREA.
000119     12  SAVE-DATE              PIC X(8)    VALUE SPACES.
000120     12  SAVE-BIN-DATE          PIC XX      VALUE SPACES.
000121     12  SAVE-DATE-CCYYMMDD.
000122         16  SAVE-DATE-CC       PIC XX      VALUE SPACES.
000123         16  SAVE-DATE-YMD.
000124             20  SAVE-DATE-YY   PIC XX      VALUE SPACES.
000125             20  FILLER         PIC X(4)    VALUE SPACES.
000126
000127 01  WS-BLANK                   PIC X       VALUE ' '.
000128
000129 01  CSO-WORK-FIELDS.
000130     05  WS-HOLD-CLAIM-STATUS    PIC X.
000131     05  WS-CRIT-PER-RECURRENT   PIC 99    VALUE 00.
000132     05  WS-CRIT-PER-ALPHA REDEFINES WS-CRIT-PER-RECURRENT.
000133         10  WS-RECURRENT-YN     PIC X.
000134         10  FILLER              PIC X.
000135
000136 01  LITERALS-NUMBERS.
000137     12  SC-ITEM                 PIC S9(4)   VALUE +1    COMP.
000138     12  NINETY                  PIC S9(4)   VALUE +90   COMP.
000139     12  ELMSTR-GENERIC-LENGTH   PIC S9(4)   VALUE +9    COMP.
000140     12  GETMAIN-SPACE           PIC X       VALUE SPACE.
000141     12  THIS-PGM                PIC X(8)    VALUE 'EL131'.
000142     12  TRAN-ID                 PIC X(4)    VALUE 'EX24'.
000143     12  MAP-ID                  PIC X(4)    VALUE '131A'.
000144     12  MAP-LENGTH              PIC S9(4)   VALUE +1060  COMP.
000145
000146     12  XCTL-EL001              PIC X(8)    VALUE 'EL001'.
000147     12  XCTL-EL004              PIC X(8)    VALUE 'EL004'.
000148     12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.
000149     12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.
000150     12  XCTL-EL114              PIC X(8)    VALUE 'EL114'.
000151     12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.
000152     12  XCTL-EL1273             PIC X(8)    VALUE 'EL1273'.
000153     12  XCTL-EL141              PIC X(8)    VALUE 'EL141'.
000154     12  XCTL-EL142              PIC X(8)    VALUE 'EL142'.
000155     12  XCTL-EL400DMD           PIC X(8)    VALUE 'EL400DMD'.
000156
000157     12  LINK-1523               PIC X(8)    VALUE 'EL1523'.
000158
000159     12  DATE-CONV               PIC X(8)    VALUE 'ELDATCV'.
000160
000161     12  RTRM-FILE-ID            PIC X(8)    VALUE 'ELRTRM'.
000162     12  CLMS-FILE-ID            PIC X(8)    VALUE 'ELMSTR'.
000163     12  NOTE-FILE-ID            PIC X(8)    VALUE 'ERNOTE'.
000164     12  CERT-FILE-ID            PIC X(8)    VALUE 'ELCERT'.
000165     12  TRLR-FILE-ID            PIC X(8)    VALUE 'ELTRLR'.
000166     12  CNTL-FILE-ID            PIC X(8)    VALUE 'ELCNTL'.
000167     12  ACTQ-FILE-ID            PIC X(8)    VALUE 'ELACTQ'.
000168     12  CHKQ-FILE-ID            PIC X(8)    VALUE 'ELCHKQ'.
000169     12  ACCT-FILE-ID            PIC X(8)    VALUE 'ERACCT'.
000170     12  ARCH-FILE-ID            PIC X(8)    VALUE 'ELARCH'.
000171     12  ALPH-FILE-ID            PIC X(8)    VALUE 'ELALPH'.
000172     12  WS-CONTEST-NOTE      PIC X(44)
000173         VALUE 'CONTRACT NOW CONTESTABLE - INC/RPT DT CHGD'.
000174     12  WS-FILE-LIM-NOTE     PIC X(44)
000175         VALUE 'FILING TIME LIMIT OK - INCRD DT CHGD'.
000176
000177     12  WS-FILING-NOTE     PIC X(30)
000178         VALUE 'FILING TIME LIMIT EXCEEDED'.
000179
000180
000181     12  MISC-SUB                PIC S9(3)   COMP-3  VALUE +0.
000182     12  ONE-OR-MIN1             PIC S9      COMP-3  VALUE +1.
000183     12  WS-READNEXT-SWITCH      PIC S99     VALUE +1.
000184     12  WS-ASSOC-CERT-TOTAL     PIC S99             VALUE ZEROS.
000185         88  NO-ASSOCIATED-CERTS                     VALUE ZEROS.
000186     12  WS-ASSOC-CERT-SEQU      PIC S99             VALUE ZEROS.
000187     12  WS-ASSOCIATED-CERTS     PIC S9      COMP-3  VALUE +0.
000188     12  WS-DIAGNOSIS            PIC X(60)   VALUE SPACES.
000189     12  WS-ICD-CODE-1           PIC X(8).
000190     12  WS-ICD-CODE-2           PIC X(8).
000191     12  WS-CLAIM-SEQU.
000192         16  FILLER              PIC X VALUE '('.
000193         16  WS-CURRENT-SEQU     PIC Z9.
000194         16  FILLER              PIC X(04) VALUE ' OF '.
000195         16  WS-OF-SEQU          PIC Z9.
000196         16  FILLER              PIC X VALUE ')'.
000197     12  WS-EDIT-BEN-CODE        PIC XX.
000198         88  INVALID-BENEFIT-CODE   VALUE '  ' '00'
000199                                          '90' THRU '99'.
000200
000201     12  WS-EDIT-AMT             PIC ZZZ,ZZZ.99.
000202     12  WS-EDIT-NUMBER          PIC ZZZ9.
000203
000204     12  WS-RESPONSE             PIC S9(8)   COMP.
000205         88  WS-RESP-NORMAL              VALUE +00.
000206         88  WS-RESP-ERROR               VALUE +01.
000207         88  WS-RESP-NOTFND              VALUE +13.
000208         88  WS-RESP-DUPKEY              VALUE +15.
000209         88  WS-RESP-NOTOPEN             VALUE +19.
000210         88  WS-RESP-ENDFILE             VALUE +20.
000211
000212     12  WS-MOS-PAID             PIC 999.
000213     12  WS-ODD-DAYS             PIC 99.
000214     12  WS-REM-MOS              PIC 999.
000215     12  WS-REM-DAYS             PIC 99.
000216     12  WS-TERM-IN-DAYS         PIC 9(4).
000217     12  WS-REM-TERM-IN-DAYS     PIC 9(4).
000218
000219     12  WS-BROWSE-SW            PIC X       VALUE 'N'.
000220     12  WS-UPDATE-SW            PIC X       VALUE 'N'.
000221     12  WS-OPEN-CLOSE-SW        PIC X       VALUE ' '.
000222     12  WS-RESET-SW             PIC X       VALUE 'N'.
000223     12  WS-REC-FOUND-SW         PIC X       VALUE 'N'.
000224     12  WS-DMO-LENGTH           PIC S9(4)   VALUE +108 COMP.
000225     12  WS-DCT-LENGTH           PIC S9(4)   VALUE +53 COMP.
000226
000227 01  filler.
000228     05  ws-prev-inc-dt          pic xx value low-values.
000229     05  ws-mob-cert-ind         pic x value ' '.
000230         88  mob-cert        value 'M'.
000231     05  ws-eracct-startbr-ind   pic x  value spaces.
000232         88  eracct-browse-started  value 'Y'.
000233     05  ws-lo-acct-dt           pic xx value low-values.
000234     05  ws-hi-acct-dt           pic xx value low-values.
000235     05  ws-acct-status          pic x value spaces.
000236         88  acct-cancelled          value '3'.
000237     05  WS-I-SAY-STOP-IND       PIC X  VALUE ' '.
000238         88  i-say-STOP            value 'S'.
000239     05  er-1679-text        pic x(60) value
000240       '1679-N CONTRACT IS NOT CONTESTABLE'.
000241     05  er-1682-text        pic x(60) value
000242       '1682-N INC DATE > MOB ACCOUNT CANCEL DATE'.
000243     05  er-1683-text        pic x(60) value
000244       '1683-N INC DATE < CERTIFICATE EFF DATE'.
000245     05  ws-rec-type             pic x.
000246     05  ws-ben-hold             pic xx.
000247     05  ws-special-calc-cd      pic x.
000248
000249 01  DMD-DATE-YYYYMMDD.
000250     12  DMD-DECADE          PIC XX      VALUE SPACES.
000251     12  DMD-YYMMDD.
000252         16  DMD-YY          PIC XX      VALUE SPACES.
000253         16  DMD-MM          PIC XX      VALUE SPACES.
000254         16  DMD-DD          PIC XX      VALUE SPACES.
000255
000256 01  DMD-DATE-MMDDYYYY.
000257     12  DMD-MDY-MM          PIC XX      VALUE SPACES.
000258     12  DMD-MDY-SLASH1      PIC X       VALUE '/'.
000259     12  DMD-MDY-DD          PIC XX      VALUE SPACES.
000260     12  DMD-MDY-SLASH2      PIC X       VALUE '/'.
000261     12  DMD-MDY-DECADE      PIC XX      VALUE SPACES.
000262     12  DMD-MDY-YY          PIC XX      VALUE SPACES.
000263
000264 01  WS-MAIL-CODE.
000265     12  FILLER              PIC X.
000266     12  DMD-MAIL-CODE.
000267         16  WS-MAIL-4       PIC X(4).
000268         16  WS-MAIL-5       PIC X.
000269     12  FILLER              PIC X(4).
000270
000271     12  W-NAME-LAST             PIC  X(15).
000272     12  W-NAME-FIRST            PIC  X(15).
000273     12  W-NAME-MIDDLE.
000274         16  FILLER              PIC  X.
000275         16  W-NAME-MIDDLE-2     PIC  X.
000276         16  FILLER              PIC  X(13).
000277
000278*    COPY ELCDCTB.
      *>>((file: ELCDCTB))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCDCTB.                            *
000005*                            VMOD=2.002                          *
000006*                                                                *
000007*   DESCRIPTION = DISTRIBUTION CONTROL TABLE MAINTENANCE PROGRAM *
000008*       COMMUNICATIONS AREA                                      *
000009*                                                                *
000010******************************************************************
000011 01  DCT-COMMUNICATION-AREA.
000012     12  DCT-BILLING-BANK-ID      PIC  X(05).
000013     12  DCT-LOGIC-BENEFICIARY-ID PIC  X(10).
000014     12  DCT-CREDIT-CARD-NUMBER   PIC  X(16).
000015     12  DCT-PRODUCT-CODE         PIC  X(02).
000016     12  DCT-COLUMN-ID-REQUESTED  PIC  X(02).
000017     12  DCT-RETURN-CODE          PIC  X(02).
000018     12  DCT-MAIL-CODE            PIC  X(05).
000019     12  DCT-DISTRIBUTION-CODE    PIC  X(04).
000020     12  DCT-MSA-ACCT-NO          PIC  X(07).
      *<<((file: ELCDCTB))
000279     EJECT
000280 01  ERROR-SWITCHES.
000281     12  ERROR-SWITCH            PIC X.
000282         88  SCREEN-ERROR                    VALUE 'X'.
000283     12  MSTR-KEY-SWITCH         PIC X.
000284         88  MSTR-KEY-CHANGED                VALUE 'X'.
000285     12  CERT-KEY-SWITCH         PIC X.
000286*********SET TO X IF ALTERNATE INDEXES WILL CHANGE
000287         88  CERT-ALT-KEY-CHANGED            VALUE 'X'.
000288*********SET TO Y IF PRIMARY KEY WILL CHANGE
000289         88  CERT-KEY-CHANGED                VALUE 'Y'.
000290     12  MSTR-SWITCH             PIC X.
000291         88  MSTR-UPDATES                    VALUE 'X'.
000292     12  crtt-switch             pic x value spaces.
000293         88  crtt-update             value 'X'.
000294     12  CERT-SWITCH             PIC X.
000295         88  CERT-UPDATES                    VALUE 'X'.
000296     12  UPDATE-SWITCH           PIC X.
000297         88  UPDATES-PRESENT                 VALUE 'X'.
000298     12  TRLR-SWITCH             PIC X.
000299         88  TRLR-UPDATE-REQUIRED            VALUE 'X'.
000300         88  UPDATE-MADE                     VALUE 'Y'.
000301     12  NINETY-TRLR-SWITCH      PIC X.
000302         88  NINETY-TRLR-UPDATED             VALUE 'X'.
000303     12  NINETY5-TRLR-SWITCH      PIC X  VALUE ' '.
000304         88  NINETY5-TRLR-UPDATED             VALUE 'X'.
000305     12  EARNINGS-CALC-SWITCH    PIC X.
000306         88  TEX-REG                         VALUE '4'.
000307         88  NET-PAY                         VALUE '5'.
000308     12  DLYACTV-SW              PIC X       VALUE 'N'.
000309         88  DLYACTV-RECORD-NEEDED           VALUE 'Y'.
000310     EJECT
000311 01  EDIT-WORK-AREA.
000312     12  COUNT-2                 PIC 9.
000313     12  CALL-PGM                PIC X(8).
000314     12  TRANS-ID                PIC X(4).
000315     12  CHECK-PFKEYS            PIC 99.
000316     12  CHECK-MAINT             PIC X.
000317         88  ADD-ALPHA-RECORD                VALUE 'A'.
000318         88  CHANGE-CLAIM                    VALUE 'C'.
000319         88  DELETE-CLAIM                    VALUE 'D'.
000320         88  CHANGE-NAME                     VALUE 'N'.
000321         88  VALID-OPTIONS                   VALUE 'A' 'C'
000322                                                   'D' 'N'.
000323
000324     12  HOLD-BENEFIT            PIC XX.
000325     12  HOLD-FREQ               PIC XX.
000326     12  HOLD-REPORTED           PIC XX  VALUE LOW-VALUES.
000327     12  HOLD-INCUR              PIC XX  VALUE LOW-VALUES.
000328     12  HOLD-PDTHRU             PIC XX.
000329     12  HOLD-ADDON              PIC XX.
000330     12  HOLD-NODAYS             PIC 9(4).
000331     12  HOLD-NOPMTS             PIC 9(3).
000332     12  HOLD-PDAMT              PIC 9(7)V99    COMP-3.
000333     12  HOLD-LOANBAL            PIC 9(7)V99    COMP-3  VALUE 0.
000334     12  HOLD-APR                PIC 9(3)V9(4)  COMP-3  VALUE 0.
000335     12  HOLD-BIRTH              PIC XX.
000336     12  HOLD-END                PIC XX.
000337     12  HOLD-EFF                PIC XX.
000338     12  HOLD-ENTRY              PIC XX.
000339     12  HOLD-LF-CV-BEN          PIC S9(9)V99   COMP-3  VALUE +0.
000340     12  HOLD-LF-RATE            PIC S9(4)V99   COMP-3  VALUE +0.
000341     12  HOLD-AH-CV-BEN          PIC S9(9)V99   COMP-3  VALUE +0.
000342     12  HOLD-AH-RATE            PIC S9(4)V99   COMP-3  VALUE +0.
000343     12  HOLD-LF-CV-CAN          PIC XX.
000344     12  HOLD-AH-CV-CAN          PIC XX.
000345     12  DIVIDE-QUOT             PIC 9(3).
000346     12  DIVIDE-REM              PIC 9(3).
000347
000348     12  WS-ALPHA-DATE.
000349         16  WS-ALPHA-YEAR.
000350             20  WS-ALPHA-YY-1   PIC 99.
000351             20  WS-ALPHA-YY-2   PIC 99.
000352         16  WS-ALPHA-MM         PIC 99.
000353         16  WS-ALPHA-DD         PIC 99.
000354
000355     12  BUILD-VALID-DATE.
000356         16  BUILD-MONTH         PIC 99.
000357         16  BUILD-DAY           PIC 99.
000358         16  BUILD-YEAR          PIC 99.
000359     12  DEEDIT-MMYY-INPUT.
000360         16  FILLER              PIC X.
000361         16  DEEDIT-MONTH        PIC XX.
000362         16  DEEDIT-YEAR         PIC XX.
000363     12  DEEDIT-BEN              PIC 9(9)V99.
000364     12  DEEDIT-DATE-INPUT.
000365         16  FILLER              PIC XX.
000366         16  DEEDIT-DATE         PIC X(6).
000367     12  WORK-DATE-MDY.
000368         16  MONTH-WORK          PIC XX.
000369         16  FILLER              PIC X(4).
000370         16  YEAR-WORK           PIC XX.
000371     12  WORK-DATE-MY.
000372         16  WORK-MONTH          PIC XX.
000373         16  WORK-SLASH          PIC X       VALUE '/'.
000374         16  WORK-YEAR           PIC XX.
000375     12  HOLD-DATE.
000376         16  HOLD-MONTH          PIC 99.
000377         16  FILLER              PIC X(4).
000378         16  HOLD-YEAR           PIC 99.
000379     12  SPLIT-INFO-LINE-1.
000380         16  SPLIT-INFO-DESC     PIC X(15).
000381         16  FILLER              PIC X(5)    VALUE ' OLD='.
000382         16  SPLIT-INFO-OLD      PIC X(40).
000383     12  EFF-TERM-MO             PIC 9(4).
000384     12  CURR-MO                 PIC 9(4).
000385     12  WS-BIN-CURRENT-DT       PIC XX.
000386     12  WS-CALC-METHOD          PIC X.
000387     12  WS-INITIALS.
000388         16  WS-INIT-1           PIC X.
000389         16  WS-INIT-2           PIC X.
000390     12  WS-REIN-TABLE.
000391         16  WS-REIN-1           PIC X.
000392         16  WS-REIN-2           PIC X.
000393         16  WS-REIN-3           PIC X.
000394     12  PI-KEY.
000395         16  PI-TERM             PIC X(4).
000396         16  PI-QUAL             PIC X(4).
000397     12  CHECK-KEY               PIC X(20).
000398     12  SAVE-ACCT-KEY           PIC X(20).
000399     12  SKIP-ATTRIBUTE          PIC X      VALUE SPACES.
000400     12  WS-SOC-SEC-NUMBER.
000401         16  WS-SOC-SEC-NO       PIC 9(9).
000402         16  WS-SOC-SEC-BLANK    PIC X(2).
000403     12  WS-SOC-SEC-REDEF REDEFINES WS-SOC-SEC-NUMBER.
000404         16  WS-SSN-1-3          PIC 9(3).
000405         16  WS-SSN-DASH1        PIC X(1).
000406         16  WS-SSN-4-5          PIC 9(2).
000407         16  WS-SSN-DASH2        PIC X(1).
000408         16  WS-SSN-6-9          PIC 9(4).
000409     12  AUTO-PAY-TO-BENE        PIC X     VALUE 'N'.
000410     12  WS-RETURNED-FROM        PIC X(8)  VALUE SPACES.
000411
000412
000413 01  WS-SAVE-FIELDS.
000414     12  WS-SAVE-BENEFICIARY     PIC X(10) VALUE LOW-VALUES.
000415     12  WS-SAVE-MAINTI          PIC X(1) VALUE LOW-VALUES.
000416     12  WS-SAVE-MAINTL          PIC S9(4) COMP VALUE +0.
000417     12  WS-SAVE-STATUSI         PIC X(6) VALUE LOW-VALUES.
000418     12  WS-SAVE-STATUSL         PIC S9(4) COMP VALUE +0.
000419     12  WS-SAVE-REPI            PIC X(8) VALUE LOW-VALUES.
000420     12  WS-SAVE-REPL            PIC S9(4) COMP VALUE +0.
000421     12  WS-SAVE-CAUSEI          PIC X(6) VALUE LOW-VALUES.
000422     12  WS-SAVE-CAUSEL          PIC S9(4) COMP VALUE +0.
000423     12  WS-SAVE-ENDI            PIC X(8) VALUE LOW-VALUES.
000424     12  WS-SAVE-ENDL            PIC S9(4) COMP VALUE +0.
000425     12  WS-SAVE-DIAGI           PIC X(60) VALUE LOW-VALUES.
000426     12  WS-SAVE-DIAGL           PIC S9(4) COMP VALUE +0.
000427     12  WS-SAVE-ICD1I           PIC X(8)  VALUE LOW-VALUES.
000428     12  WS-SAVE-ICD1L           PIC S9(4) COMP VALUE +0.
000429     12  WS-SAVE-ICD2I           PIC X(8)  VALUE LOW-VALUES.
000430     12  WS-SAVE-ICD2L           PIC S9(4) COMP VALUE +0.
000431     12  WS-SAVE-BENEI           PIC X(10) VALUE LOW-VALUES.
000432     12  WS-SAVE-BENEL           PIC S9(4) COMP VALUE +0.
000433     12  WS-SAVE-BIRTHI          PIC X(8) VALUE LOW-VALUES.
000434     12  WS-SAVE-BIRTHL          PIC S9(4) COMP VALUE +0.
000435     12  WS-SAVE-SOCIALI         PIC X(11) VALUE LOW-VALUES.
000436     12  WS-SAVE-SOCIALL         PIC S9(4) COMP VALUE +0.
000437     12  WS-SAVE-SEXI            PIC X(1) VALUE LOW-VALUES.
000438     12  WS-SAVE-SEXL            PIC S9(4) COMP VALUE +0.
000439     12  WS-SAVE-MLNAMEI         PIC X(15) VALUE LOW-VALUES.
000440     12  WS-SAVE-MLNAMEL         PIC S9(4) COMP VALUE +0.
000441     12  WS-SAVE-MFNAMEI         PIC X(12) VALUE LOW-VALUES.
000442     12  WS-SAVE-MFNAMEL         PIC S9(4) COMP VALUE +0.
000443     12  WS-SAVE-MMINITI         PIC X(1) VALUE LOW-VALUES.
000444     12  WS-SAVE-MMINITL         PIC S9(4) COMP VALUE +0.
000445     12  WS-SAVE-LOANNOI         PIC X(8) VALUE LOW-VALUES.
000446     12  WS-SAVE-LOANNOL         PIC S9(4) COMP VALUE +0.
000447     12  WS-SAVE-LOANBALI        PIC 9(7)V99 VALUE ZEROS.
000448     12  WS-SAVE-LOANBALL        PIC S9(4) COMP VALUE +0.
000449     12  WS-SAVE-PROCI           PIC X(4) VALUE LOW-VALUES.
000450     12  WS-SAVE-PROCL           PIC S9(4) COMP VALUE +0.
000451     12  WS-SAVE-SUPVI           PIC X(1) VALUE LOW-VALUES.
000452     12  WS-SAVE-SUPVL           PIC S9(4) COMP VALUE +0.
000453     12  WS-SAVE-PRICDI          PIC X(1) VALUE LOW-VALUES.
000454     12  WS-SAVE-PRICDL          PIC S9(4) COMP VALUE +0.
000455     12  WS-SAVE-FILETOI         PIC X(4) VALUE LOW-VALUES.
000456     12  WS-SAVE-FILETOL         PIC S9(4) COMP VALUE +0.
000457     12  WS-SAVE-PDTHRUI         PIC X(8) VALUE LOW-VALUES.
000458     12  WS-SAVE-PDTHRUL         PIC S9(4) COMP VALUE +0.
000459     12  WS-SAVE-PDAMTI          PIC 9(7)V99 VALUE ZEROS.
000460     12  WS-SAVE-PDAMTL          PIC S9(4) COMP VALUE +0.
000461     12  WS-SAVE-NODAYSI         PIC 9(5) VALUE ZEROS.
000462     12  WS-SAVE-NODAYSL         PIC S9(4) COMP VALUE +0.
000463     12  WS-SAVE-NOPMTSI         PIC 9(4) VALUE ZEROS.
000464     12  WS-SAVE-NOPMTSL         PIC S9(4) COMP VALUE +0.
000465     12  WS-SAVE-FORMTYPI        PIC X(1) VALUE LOW-VALUES.
000466     12  WS-SAVE-FORMTYPL        PIC S9(4) COMP VALUE +0.
000467     12  WS-SAVE-OCCI            PIC X(6) VALUE LOW-VALUES.
000468     12  WS-SAVE-OCCL            PIC S9(4) COMP VALUE +0.
000469
000470 01  TERM-CALCULATION-WORK-AREA     COMP-3.
000471     12  M                   PIC S9(7)V99           VALUE ZEROS.
000472     12  L                   PIC S9(7)V99           VALUE ZEROS.
000473     12  N                   PIC S9(3)              VALUE ZEROS.
000474     12  N-STORE             PIC S9(3)              VALUE ZEROS.
000475     12  NV-STORE            PIC S9(3)              VALUE ZEROS.
000476     12  I                   PIC S99V9(5)           VALUE ZEROS.
000477     12  A-N                 PIC S9(7)V9(8)         VALUE ZEROS.
000478     12  IA-N                PIC S9(7)V9(8)         VALUE ZEROS.
000479     12  V                   PIC S9(3)V9(14)        VALUE ZEROS.
000480     12  R                   PIC S9(3)              VALUE ZEROS.
000481     12  M1                  PIC S9(7)V99           VALUE ZEROS.
000482     12  V-EX-N              PIC S9(3)V9(14)        VALUE ZEROS.
000483     12  TERM1               PIC S9(8)V9(9)         VALUE ZEROS.
000484     12  TERM2               PIC S9(8)V9(9)         VALUE ZEROS.
000485     12  TERM3               PIC S9(8)V9(9)         VALUE ZEROS.
000486     12  TERM4               PIC S9(3)V9(14)        VALUE ZEROS.
000487     12  LEFT-TOT-1          PIC S9(9)V9(8)         VALUE ZEROS.
000488     12  RIGHT-TOT-1         PIC S9(9)V9(8)         VALUE ZEROS.
000489     12  RIGHT-TOT-2         PIC S9(9)V9(8)         VALUE ZEROS.
000490
000491 01  TERM-CALC-WORK-AREA.
000492     12  WS-AH-RATE          PIC S999V9(5)          VALUE ZEROS.
000493     12  WS-LF-RATE          PIC S999V9(5)          VALUE ZEROS.
000494     12  WS-TERM             PIC S9(3)              VALUE ZEROS.
000495     12  WS-TERM-REM         PIC S9(3)V99           VALUE ZEROS.
000496     12  WS-REMAIN           PIC S99                VALUE ZEROS.
000497     12  V-EXPONENTS.
000498        14  V-EXPONENT       PIC S9(3)V9(14) COMP-3 OCCURS 250.
000499     12  V-EX-ONETIME        PIC 9                  VALUE 1.
000500
000501 01  TIME-IN.
000502     12  UN-HOURS                PIC XX.
000503     12  UN-MINUTES              PIC XX.
000504     12  FILLER                  PIC XX.
000505
000506 01  TIME-OUT.
000507     12  FOR-HOURS               PIC XX.
000508     12  FILLER                  PIC X       VALUE '.'.
000509     12  FOR-MINUTES             PIC XX.
000510
000511 01  ERROR-NUMBERS.
000512     12  ER-0000                 PIC X(4)    VALUE '0000'.
000513     12  ER-0004                 PIC X(4)    VALUE '0004'.
000514     12  ER-0008                 PIC X(4)    VALUE '0008'.
000515     12  ER-0023                 PIC X(4)    VALUE '0023'.
000516     12  ER-0029                 PIC X(4)    VALUE '0029'.
000517     12  ER-0068                 PIC X(4)    VALUE '0068'.
000518     12  ER-0070                 PIC X(4)    VALUE '0070'.
000519     12  ER-0149                 PIC X(4)    VALUE '0149'.
000520     12  ER-0154                 PIC X(4)    VALUE '0154'.
000521     12  ER-0169                 PIC X(4)    VALUE '0169'.
000522     12  ER-0192                 PIC X(4)    VALUE '0192'.
000523     12  ER-0199                 PIC X(4)    VALUE '0199'.
000524     12  ER-0203                 PIC X(4)    VALUE '0203'.
000525     12  ER-0204                 PIC X(4)    VALUE '0204'.
000526     12  ER-0205                 PIC X(4)    VALUE '0205'.
000527     12  ER-0206                 PIC X(4)    VALUE '0206'.
000528     12  ER-0208                 PIC X(4)    VALUE '0208'.
000529     12  ER-0210                 PIC X(4)    VALUE '0210'.
000530     12  ER-0215                 PIC X(4)    VALUE '0215'.
000531     12  ER-0219                 PIC X(4)    VALUE '0219'.
000532     12  ER-0220                 PIC X(4)    VALUE '0220'.
000533     12  ER-0222                 PIC X(4)    VALUE '0222'.
000534     12  ER-0223                 PIC X(4)    VALUE '0223'.
000535     12  ER-0224                 PIC X(4)    VALUE '0224'.
000536     12  ER-0227                 PIC X(4)    VALUE '0227'.
000537     12  ER-0230                 PIC X(4)    VALUE '0230'.
000538     12  ER-0232                 PIC X(4)    VALUE '0232'.
000539     12  ER-0233                 PIC X(4)    VALUE '0233'.
000540     12  ER-0236                 PIC X(4)    VALUE '0236'.
000541     12  ER-0237                 PIC X(4)    VALUE '0237'.
000542     12  ER-0238                 PIC X(4)    VALUE '0238'.
000543     12  ER-0240                 PIC X(4)    VALUE '0240'.
000544     12  ER-0241                 PIC X(4)    VALUE '0241'.
000545     12  ER-0243                 PIC X(4)    VALUE '0243'.
000546     12  ER-0244                 PIC X(4)    VALUE '0244'.
000547     12  ER-0246                 PIC X(4)    VALUE '0246'.
000548     12  ER-0247                 PIC X(4)    VALUE '0247'.
000549     12  ER-0248                 PIC X(4)    VALUE '0248'.
000550     12  ER-0250                 PIC X(4)    VALUE '0250'.
000551     12  ER-0251                 PIC X(4)    VALUE '0251'.
000552     12  ER-0253                 PIC X(4)    VALUE '0253'.
000553     12  ER-0256                 PIC X(4)    VALUE '0256'.
000554     12  ER-0257                 PIC X(4)    VALUE '0257'.
000555     12  ER-0258                 PIC X(4)    VALUE '0258'.
000556     12  ER-0259                 PIC X(4)    VALUE '0259'.
000557     12  ER-0260                 PIC X(4)    VALUE '0260'.
000558     12  ER-0261                 PIC X(4)    VALUE '0261'.
000559     12  ER-0262                 PIC X(4)    VALUE '0262'.
000560     12  ER-0263                 PIC X(4)    VALUE '0263'.
000561     12  ER-0273                 PIC X(4)    VALUE '0273'.
000562     12  ER-0274                 PIC X(4)    VALUE '0274'.
000563     12  ER-0276                 PIC X(4)    VALUE '0276'.
000564     12  ER-0282                 PIC X(4)    VALUE '0282'.
000565     12  ER-0283                 PIC X(4)    VALUE '0283'.
000566     12  ER-0309                 PIC X(4)    VALUE '0309'.
000567     12  ER-0310                 PIC X(4)    VALUE '0310'.
000568     12  ER-0311                 PIC X(4)    VALUE '0311'.
000569     12  ER-0333                 PIC X(4)    VALUE '0333'.
000570     12  ER-0360                 PIC X(4)    VALUE '0360'.
000571     12  ER-0377                 PIC X(4)    VALUE '0377'.
000572     12  ER-0402                 PIC X(4)    VALUE '0402'.
000573     12  ER-0416                 PIC X(4)    VALUE '0416'.
000574     12  ER-0426                 PIC X(4)    VALUE '0426'.
000575     12  ER-0427                 PIC X(4)    VALUE '0427'.
000576     12  ER-0428                 PIC X(4)    VALUE '0428'.
000577     12  ER-0429                 PIC X(4)    VALUE '0429'.
000578     12  ER-0430                 PIC X(4)    VALUE '0430'.
000579     12  ER-0458                 PIC X(4)    VALUE '0458'.
000580     12  ER-0475                 PIC X(4)    VALUE '0475'.
000581     12  ER-0491                 PIC X(4)    VALUE '0491'.
000582     12  ER-0509                 PIC X(4)    VALUE '0509'.
000583     12  ER-0510                 PIC X(4)    VALUE '0510'.
000584     12  ER-0511                 PIC X(4)    VALUE '0511'.
000585     12  ER-0512                 PIC X(4)    VALUE '0512'.
000586     12  ER-0519                 PIC X(4)    VALUE '0519'.
000587     12  ER-0520                 PIC X(4)    VALUE '0520'.
000588     12  ER-0521                 PIC X(4)    VALUE '0521'.
000589     12  ER-0547                 PIC X(4)    VALUE '0547'.
000590     12  ER-0548                 PIC X(4)    VALUE '0548'.
000591     12  ER-0549                 PIC X(4)    VALUE '0549'.
000592     12  ER-0565                 PIC X(4)    VALUE '0565'.
000593     12  ER-0598                 PIC X(4)    VALUE '0598'.
000594     12  ER-0639                 PIC X(4)    VALUE '0639'.
000595     12  ER-0797                 PIC X(4)    VALUE '0797'.
000596     12  ER-0802                 PIC X(4)    VALUE '0802'.
000597     12  ER-0803                 PIC X(4)    VALUE '0803'.
000598     12  ER-0849                 PIC X(4)    VALUE '0849'.
000599     12  ER-0885                 PIC X(4)    VALUE '0885'.
000600     12  ER-0887                 PIC X(4)    VALUE '0887'.
000601     12  ER-0919                 PIC X(4)    VALUE '0919'.
000602     12  ER-0921                 PIC X(4)    VALUE '0921'.
000603     12  ER-0938                 PIC X(4)    VALUE '0938'.
000604     12  ER-0946                 PIC X(4)    VALUE '0946'.
000605     12  ER-0947                 PIC X(4)    VALUE '0947'.
000606     12  ER-0948                 PIC X(4)    VALUE '0948'.
000607     12  ER-0949                 PIC X(4)    VALUE '0949'.
000608     12  ER-0950                 PIC X(4)    VALUE '0950'.
000609     12  ER-0951                 PIC X(4)    VALUE '0951'.
000610     12  ER-0954                 PIC X(4)    VALUE '0954'.
000611     12  ER-0974                 PIC X(4)    VALUE '0974'.
000612     12  ER-0975                 PIC X(4)    VALUE '0975'.
000613     12  ER-0992                 PIC X(4)    VALUE '0992'.
000614     12  er-1581                 pic x(4)    value '1581'.
000615     12  ER-1651                 PIC X(4)    VALUE '1651'.
000616     12  ER-1652                 PIC X(4)    VALUE '1652'.
000617     12  ER-1653                 PIC X(4)    VALUE '1653'.
000618     12  ER-1654                 PIC X(4)    VALUE '1654'.
000619     12  ER-1655                 PIC X(4)    VALUE '1655'.
000620     12  er-1656                 pic x(4)    value '1656'.
000621     12  ER-1661                 PIC X(4)    VALUE '1661'.
000622     12  ER-1662                 PIC X(4)    VALUE '1662'.
000623     12  ER-1663                 PIC X(4)    VALUE '1663'.
000624     12  er-1664                 pic x(4)    value '1664'.
000625     12  er-1665                 pic x(4)    value '1665'.
000626     12  er-1666                 pic x(4)    value '1666'.
000627     12  er-1668                 pic x(4)    value '1668'.
000628     12  er-1669                 pic x(4)    value '1669'.
000629     12  er-1675                 pic x(4)    value '1675'.
000630     12  er-1676                 pic x(4)    value '1676'.
000631     12  er-1677                 pic x(4)    value '1677'.
000632     12  ER-1678                 PIC X(4)    VALUE '1678'.
000633     12  ER-1679                 PIC X(4)    VALUE '1679'.
000634     12  er-1682                 pic x(4)    value '1682'.
000635     12  er-1683                 pic x(4)    value '1683'.
000636     12  ER-1772                 PIC X(4)    VALUE '1772'.
000637     12  ER-1773                 PIC X(4)    VALUE '1773'.
000638     12  ER-1778                 PIC X(4)    VALUE '1778'.
000639     12  ER-2280                 PIC X(4)    VALUE '2280'.
000640     12  ER-2878                 PIC X(4)    VALUE '2878'.
000641     12  ER-7572                 PIC X(4)    VALUE '7572'.
000642     12  ER-7641                 PIC X(4)    VALUE '7641'.
000643     12  ER-7642                 PIC X(4)    VALUE '7642'.
000644     12  ER-7650                 PIC X(4)    VALUE '7650'.
000645     12  ER-7651                 PIC X(4)    VALUE '7651'.
000646     12  ER-7687                 PIC X(4)    VALUE '7687'.
000647     12  ER-7689                 PIC X(4)    VALUE '7689'.
000648     12  ER-7690                 PIC X(4)    VALUE '7690'.
000649     12  ER-7691                 PIC X(4)    VALUE '7691'.
000650     12  ER-8003                 PIC X(4)    VALUE '8003'.
000651     12  ER-8051                 PIC X(4)    VALUE '8051'.
000652     12  ER-8052                 PIC X(4)    VALUE '8052'.
000653     12  ER-8053                 PIC X(4)    VALUE '8053'.
000654     12  ER-8054                 PIC X(4)    VALUE '8054'.
000655     12  ER-8055                 PIC X(4)    VALUE '8055'.
000656     12  ER-8056                 PIC X(4)    VALUE '8056'.
000657     12  ER-8057                 PIC X(4)    VALUE '8057'.
000658     12  ER-8058                 PIC X(4)    VALUE '8058'.
000659     12  ER-8059                 PIC X(4)    VALUE '8059'.
000660     12  ER-8060                 PIC X(4)    VALUE '8060'.
000661     12  ER-8061                 PIC X(4)    VALUE '8061'.
000662     12  ER-8062                 PIC X(4)    VALUE '8062'.
000663     12  ER-8063                 PIC X(4)    VALUE '8063'.
000664     12  ER-8064                 PIC X(4)    VALUE '8064'.
000665     12  ER-8065                 PIC X(4)    VALUE '8065'.
000666     12  ER-8066                 PIC X(4)    VALUE '8066'.
000667     12  ER-8152                 PIC X(4)    VALUE '8152'.
000668     12  ER-8153                 PIC X(4)    VALUE '8153'.
000669     12  ER-8154                 PIC X(4)    VALUE '8154'.
000670     12  ER-8155                 PIC X(4)    VALUE '8155'.
000671
000672 01  ERPDEF-KEY-SAVE             PIC X(18).
000673 01  ERPDEF-KEY.
000674     12  ERPDEF-COMPANY-CD       PIC X.
000675     12  ERPDEF-STATE            PIC XX.
000676     12  ERPDEF-PROD-CD          PIC XXX.
000677     12  F                       PIC X(7).
000678     12  ERPDEF-BEN-TYPE         PIC X.
000679     12  ERPDEF-BEN-CODE         PIC XX.
000680     12  ERPDEF-EXP-DT           PIC XX.
000681
000682 01  MSTR-KEY.
000683     12  COMPANY-CODE            PIC X.
000684     12  CARRIER-CODE            PIC X.
000685     12  CLAIM-NO                PIC X(7).
000686     12  CERT-NO.
000687         16  CERT-NO-PRIME       PIC X(10).
000688         16  CERT-NO-SUFX        PIC X.
000689
000690 01  WS-SAVE-CLAIM-KEY.
000691     12  WS-SAVE-COMPANY-CD      PIC X.
000692     12  WS-SAVE-CARRIER         PIC X.
000693     12  WS-SAVE-CLAIM-NO        PIC X(7).
000694     12  WS-SAVE-CERT-NO.
000695         16  WS-CERT-NO-PRIME    PIC X(10).
000696         16  WS-CERT-NO-SUFX     PIC X.
000697
000698 01  CERT-KEY.
000699     12  CERT-COMPANY-CODE       PIC X.
000700     12  CERT-CARRIER            PIC X.
000701     12  CERT-GROUP              PIC X(6).
000702     12  CERT-STATE              PIC XX.
000703     12  CERT-ACCOUNT            PIC X(10).
000704     12  CERT-DATE               PIC XX.
000705     12  CERT-CERT.
000706         16  CERT-CERT-PRIME     PIC X(10).
000707         16  CERT-CERT-SUFX      PIC X.
000708
000709 01  ELCRTT-KEY.
000710     05  CTRLR-COMP-CD       PIC X.
000711     05  CTRLR-CARRIER       PIC X.
000712     05  CTRLR-GROUPING      PIC X(6).
000713     05  CTRLR-STATE         PIC X(2).
000714     05  CTRLR-ACCOUNT       PIC X(10).
000715     05  CTRLR-EFF-DT        PIC XX.
000716     05  CTRLR-CERT-NO       PIC X(11).
000717     05  CTRLR-REC-TYPE      PIC X.
000718
000719 01  NOTE-KEY.
000720     12  NOTE-COMP-CD            PIC X.
000721     12  NOTE-CERT-KEY.
000722         16  NOTE-CARRIER        PIC X.
000723         16  NOTE-GROUP          PIC X(6).
000724         16  NOTE-STATE          PIC XX.
000725         16  NOTE-ACCOUNT        PIC X(10).
000726         16  NOTE-DATE           PIC XX.
000727         16  NOTE-CERT-NO        PIC X(11).
000728
000729 01  TRLR-KEY.
000730     12  TRLR-MAIN-KEY.
000731         16  TRLR-COMPANY-CD     PIC X.
000732         16  TRLR-CARRIER        PIC X.
000733         16  TRLR-CLAIM-NO       PIC X(07).
000734         16  TRLR-CERT-NO        PIC X(11).
000735     12  TRLR-SEQ-NO             PIC S9(4)   COMP.
000736
000737 01  BENEFIT-KEY.
000738     12  BEN-CO-ID               PIC X(3).
000739     12  BEN-REC-TYPE            PIC X.
000740     12  FILLER                  PIC XX.
000741     12  BEN-ACC-CD              PIC XX.
000742     12  BEN-SEQ-NO              PIC S9(4)   COMP.
000743
000744 01  CNTL-KEY.
000745     12  CNTL-CO-ID              PIC X(3).
000746     12  CNTL-REC-TYPE           PIC X.
000747     12  CNTL-PROC-ID            PIC X(4).
000748     12  CNTL-STATE-ACCESS REDEFINES CNTL-PROC-ID.
000749         16  CNTL-STATE-NUMBER   PIC XX.
000750         16  FILLER              PIC XX.
000751     12  CNTL-CARRIER-ACCESS REDEFINES CNTL-PROC-ID.
000752         16  FILLER              PIC X(3).
000753         16  CNTL-CARRIER        PIC X.
000754     12  CNTL-bene-ACCESS REDEFINES CNTL-PROC-ID.
000755         16  filler              PIC XX.
000756         16  cntl-benefit        PIC XX.
000757     12  CNTL-SEQ-NO             PIC S9(4)   COMP.
000758
000759 01  ACTQ-KEY                    PIC X(20).
000760
000761 01  WS-LAST-TRLR-KEY            PIC X(22)  VALUE SPACES.
000762
000763 01  CHKQ-KEY.
000764     12  CHKQ-COMPANY-CODE       PIC X.
000765     12  CHKQ-CONTROL-NO         PIC S9(8)  COMP.
000766     12  CHKQ-SEQ-NO             PIC S9(4)  COMP.
000767
000768 01  ARCH-KEY.
000769     12  ARCH-COMPANY-CODE       PIC X.
000770     12  ARCH-ARCHIVE-NO         PIC S9(8)  COMP.
000771     12  ARCH-RECORD-TYPE        PIC X.
000772     12  ARCH-SEQ-NO             PIC S9(4)  COMP.
000773
000774 01  ACCT-KEY.
000775     05  ACCT-PARTIAL-KEY.
000776         12  ACCT-COMPANY-CODE   PIC X.
000777         12  ACCT-CARRIER        PIC X.
000778         12  ACCT-GROUP          PIC X(6).
000779         12  ACCT-STATE          PIC XX.
000780         12  ACCT-ACCOUNT        PIC X(10).
000781     05  ACCT-DATE               PIC XX.
000782
000783 01  WS-ELBENE-KEY.
000784     12  WS-ELBENE-COMPANY-CD    PIC X.
000785     12  WS-ELBENE-RECORD-TYPE   PIC X.
000786     12  WS-ELBENE-ID            PIC X(10).
000787
000788 01  ELALPH-KEY.
000789     12  ELALPH-COMPANY-CD       PIC X.
000790     12  ELALPH-SOURCE           PIC X.
000791     12  ELALPH-NAME.
000792         16  ELALPH-LAST-NAME    PIC X(15).
000793         16  ELALPH-FIRST-NAME.
000794             20  ELALPH-FIRST-INIT   PIC X.
000795             20  FILLER              PIC X(11).
000796         16  ELALPH-MIDDLE-INIT  PIC X.
000797     12  ELALPH-DATE             PIC X(08).
000798     12  ELALPH-TIME             PIC S9(04)    COMP.
000799
000800 01  COMP-LENGTHS.
000801     12  CNTL-GENERIC-LENGTH     PIC S9(4)   COMP VALUE +8.
000802     12  JOURNAL-LENGTH          PIC S9(4)   COMP VALUE +0.
000803     12  DATE-LENGTH             PIC S9(4)   COMP VALUE +8.
000804     12  MO-YR-LENGTH            PIC S9(4)   COMP VALUE +5.
000805     12  TERM-LENGTH             PIC S9(4)   COMP VALUE +3.
000806     12  BEN-LENGTH              PIC S9(4)   COMP VALUE +11.
000807     12  FREQ-LENGTH             PIC S9(4)   COMP VALUE +2.
000808     12  MSTR-LENGTH             PIC S9(4)   COMP VALUE +350.
000809     12  CERT-LENGTH             PIC S9(4)   COMP VALUE +450.
000810     12  TRLR-LENGTH             PIC S9(4)   COMP VALUE +200.
000811     12  ACTQ-LENGTH             PIC S9(4)   COMP VALUE +60.
000812     12  CHKQ-LENGTH             PIC S9(4)   COMP VALUE +100.
000813     12  ARCH-LENGTH             PIC S9(4)   COMP VALUE +90.
000814     12  ALPH-LENGTH             PIC S9(4)   COMP VALUE +128.
000815     EJECT
000816*                                COPY ELCLNKLT.
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
000817     EJECT
000818*                                COPY ELCINTF.
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
000819     12  EL131-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
000820         16  PI-NO-PMTS                    PIC 9(4).
000821         16  PI-CERT-SWITCH                PIC X.
000822             88  CREATED-CERT              VALUE '2'.
000823         16  PI-PREM-TYPE                  PIC X.
000824         16  PI-SAVE-CERT-NO.
000825             20  PI-SAVE-CERT-NO-PRIME     PIC X(10).
000826             20  PI-SAVE-CERT-NO-SUFX      PIC X.
000827         16  FILLER                        PIC X(46).
000828         16  PI-CLAIM-DELETED-SWITCH       PIC X(1).
000829         16  PI-PAYMENT-AMT                PIC S9(7)V99   COMP-3.
000830         16  PI-REC-FOUND-SW               PIC X.
000831         16  PI-LETTER-SW                  PIC X.
000832         16  PI-PREV-TRLR-KEY              PIC X(22).
000833         16  pi-claim-type                 pic x.
000834         16  PI-AUTO-PAY-SEQ               PIC S9(4)      COMP.
000835         16  pi-approval-level             pic x.
000836         16  FILLER                        PIC X(543).
000837
000838 01  SAVE-RECORD                 PIC X(450).
000839
000840     EJECT
000841*                                COPY ELCLOGOF SUPPRESS.
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
000842
000843*                                COPY ELCATTR SUPPRESS.
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
000844
000845*                                COPY ELCAID SUPPRESS.
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
000846 01  FILLER REDEFINES DFHAID.
000847     12  FILLER                  PIC X(8).
000848     12  AID-KEYS OCCURS 24 TIMES.
000849         16  FILLER              PIC X.
000850
000851*                                COPY EL131S.
      *>>((file: EL131S))
000001 01  EL131AI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  DATEL PIC S9(0004) COMP.
000005     05  DATEF PIC  X(0001).
000006     05  FILLER REDEFINES DATEF.
000007         10  DATEA PIC  X(0001).
000008     05  DATEI PIC  X(0008).
000009*    -------------------------------
000010     05  TIMEL PIC S9(0004) COMP.
000011     05  TIMEF PIC  X(0001).
000012     05  FILLER REDEFINES TIMEF.
000013         10  TIMEA PIC  X(0001).
000014     05  TIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  SEQUL PIC S9(0004) COMP.
000017     05  SEQUF PIC  X(0001).
000018     05  FILLER REDEFINES SEQUF.
000019         10  SEQUA PIC  X(0001).
000020     05  SEQUI PIC  X(0010).
000021*    -------------------------------
000022     05  COMPL PIC S9(0004) COMP.
000023     05  COMPF PIC  X(0001).
000024     05  FILLER REDEFINES COMPF.
000025         10  COMPA PIC  X(0001).
000026     05  COMPI PIC  X(0003).
000027*    -------------------------------
000028     05  USERIDL PIC S9(0004) COMP.
000029     05  USERIDF PIC  X(0001).
000030     05  FILLER REDEFINES USERIDF.
000031         10  USERIDA PIC  X(0001).
000032     05  USERIDI PIC  X(0004).
000033*    -------------------------------
000034     05  LABEL1L PIC S9(0004) COMP.
000035     05  LABEL1F PIC  X(0001).
000036     05  FILLER REDEFINES LABEL1F.
000037         10  LABEL1A PIC  X(0001).
000038     05  LABEL1I PIC  X(0011).
000039*    -------------------------------
000040     05  PCERTNOL PIC S9(0004) COMP.
000041     05  PCERTNOF PIC  X(0001).
000042     05  FILLER REDEFINES PCERTNOF.
000043         10  PCERTNOA PIC  X(0001).
000044     05  PCERTNOI PIC  X(0010).
000045*    -------------------------------
000046     05  PSUFXL PIC S9(0004) COMP.
000047     05  PSUFXF PIC  X(0001).
000048     05  FILLER REDEFINES PSUFXF.
000049         10  PSUFXA PIC  X(0001).
000050     05  PSUFXI PIC  X(0001).
000051*    -------------------------------
000052     05  CCNOL PIC S9(0004) COMP.
000053     05  CCNOF PIC  X(0001).
000054     05  FILLER REDEFINES CCNOF.
000055         10  CCNOA PIC  X(0001).
000056     05  CCNOI PIC  X(0016).
000057*    -------------------------------
000058     05  MAINTL PIC S9(0004) COMP.
000059     05  MAINTF PIC  X(0001).
000060     05  FILLER REDEFINES MAINTF.
000061         10  MAINTA PIC  X(0001).
000062     05  MAINTI PIC  X(0001).
000063*    -------------------------------
000064     05  CLAIML PIC S9(0004) COMP.
000065     05  CLAIMF PIC  X(0001).
000066     05  FILLER REDEFINES CLAIMF.
000067         10  CLAIMA PIC  X(0001).
000068     05  CLAIMI PIC  X(0007).
000069*    -------------------------------
000070     05  CARRL PIC S9(0004) COMP.
000071     05  CARRF PIC  X(0001).
000072     05  FILLER REDEFINES CARRF.
000073         10  CARRA PIC  X(0001).
000074     05  CARRI PIC  X(0001).
000075*    -------------------------------
000076     05  CERTL PIC S9(0004) COMP.
000077     05  CERTF PIC  X(0001).
000078     05  FILLER REDEFINES CERTF.
000079         10  CERTA PIC  X(0001).
000080     05  CERTI PIC  X(0010).
000081*    -------------------------------
000082     05  SUFXL PIC S9(0004) COMP.
000083     05  SUFXF PIC  X(0001).
000084     05  FILLER REDEFINES SUFXF.
000085         10  SUFXA PIC  X(0001).
000086     05  SUFXI PIC  X(0001).
000087*    -------------------------------
000088     05  TYPEL PIC S9(0004) COMP.
000089     05  TYPEF PIC  X(0001).
000090     05  FILLER REDEFINES TYPEF.
000091         10  TYPEA PIC  X(0001).
000092     05  TYPEI PIC  X(0001).
000093*    -------------------------------
000094     05  BENPERL PIC S9(0004) COMP.
000095     05  BENPERF PIC  X(0001).
000096     05  FILLER REDEFINES BENPERF.
000097         10  BENPERA PIC  X(0001).
000098     05  BENPERI PIC  99.
000099*    -------------------------------
000100     05  INSTYPEL PIC S9(0004) COMP.
000101     05  INSTYPEF PIC  X(0001).
000102     05  FILLER REDEFINES INSTYPEF.
000103         10  INSTYPEA PIC  X(0001).
000104     05  INSTYPEI PIC  X(0001).
000105*    -------------------------------
000106     05  STATUSL PIC S9(0004) COMP.
000107     05  STATUSF PIC  X(0001).
000108     05  FILLER REDEFINES STATUSF.
000109         10  STATUSA PIC  X(0001).
000110     05  STATUSI PIC  X(0006).
000111*    -------------------------------
000112     05  INCL PIC S9(0004) COMP.
000113     05  INCF PIC  X(0001).
000114     05  FILLER REDEFINES INCF.
000115         10  INCA PIC  X(0001).
000116     05  INCI PIC  X(0008).
000117*    -------------------------------
000118     05  REPL PIC S9(0004) COMP.
000119     05  REPF PIC  X(0001).
000120     05  FILLER REDEFINES REPF.
000121         10  REPA PIC  X(0001).
000122     05  REPI PIC  X(0008).
000123*    -------------------------------
000124     05  ICD1L PIC S9(0004) COMP.
000125     05  ICD1F PIC  X(0001).
000126     05  FILLER REDEFINES ICD1F.
000127         10  ICD1A PIC  X(0001).
000128     05  ICD1I PIC  X(0008).
000129*    -------------------------------
000130     05  ICD2L PIC S9(0004) COMP.
000131     05  ICD2F PIC  X(0001).
000132     05  FILLER REDEFINES ICD2F.
000133         10  ICD2A PIC  X(0001).
000134     05  ICD2I PIC  X(0008).
000135*    -------------------------------
000136     05  DIAGL PIC S9(0004) COMP.
000137     05  DIAGF PIC  X(0001).
000138     05  FILLER REDEFINES DIAGF.
000139         10  DIAGA PIC  X(0001).
000140     05  DIAGI PIC  X(0060).
000141*    -------------------------------
000142     05  ACCSWL PIC S9(0004) COMP.
000143     05  ACCSWF PIC  X(0001).
000144     05  FILLER REDEFINES ACCSWF.
000145         10  ACCSWA PIC  X(0001).
000146     05  ACCSWI PIC  X(0001).
000147*    -------------------------------
000148     05  BENEL PIC S9(0004) COMP.
000149     05  BENEF PIC  X(0001).
000150     05  FILLER REDEFINES BENEF.
000151         10  BENEA PIC  X(0001).
000152     05  BENEI PIC  X(0010).
000153*    -------------------------------
000154     05  BIRTHL PIC S9(0004) COMP.
000155     05  BIRTHF PIC  X(0001).
000156     05  FILLER REDEFINES BIRTHF.
000157         10  BIRTHA PIC  X(0001).
000158     05  BIRTHI PIC  X(0008).
000159*    -------------------------------
000160     05  SOCIALL PIC S9(0004) COMP.
000161     05  SOCIALF PIC  X(0001).
000162     05  FILLER REDEFINES SOCIALF.
000163         10  SOCIALA PIC  X(0001).
000164     05  SOCIALI PIC  X(0011).
000165*    -------------------------------
000166     05  SEXL PIC S9(0004) COMP.
000167     05  SEXF PIC  X(0001).
000168     05  FILLER REDEFINES SEXF.
000169         10  SEXA PIC  X(0001).
000170     05  SEXI PIC  X(0001).
000171*    -------------------------------
000172     05  MLNAMEL PIC S9(0004) COMP.
000173     05  MLNAMEF PIC  X(0001).
000174     05  FILLER REDEFINES MLNAMEF.
000175         10  MLNAMEA PIC  X(0001).
000176     05  MLNAMEI PIC  X(0015).
000177*    -------------------------------
000178     05  MFNAMEL PIC S9(0004) COMP.
000179     05  MFNAMEF PIC  X(0001).
000180     05  FILLER REDEFINES MFNAMEF.
000181         10  MFNAMEA PIC  X(0001).
000182     05  MFNAMEI PIC  X(0012).
000183*    -------------------------------
000184     05  MMINITL PIC S9(0004) COMP.
000185     05  MMINITF PIC  X(0001).
000186     05  FILLER REDEFINES MMINITF.
000187         10  MMINITA PIC  X(0001).
000188     05  MMINITI PIC  X(0001).
000189*    -------------------------------
000190     05  AUTHRCVL PIC S9(0004) COMP.
000191     05  AUTHRCVF PIC  X(0001).
000192     05  FILLER REDEFINES AUTHRCVF.
000193         10  AUTHRCVA PIC  X(0001).
000194     05  AUTHRCVI PIC  X(0001).
000195*    -------------------------------
000196     05  CRTLNMEL PIC S9(0004) COMP.
000197     05  CRTLNMEF PIC  X(0001).
000198     05  FILLER REDEFINES CRTLNMEF.
000199         10  CRTLNMEA PIC  X(0001).
000200     05  CRTLNMEI PIC  X(0015).
000201*    -------------------------------
000202     05  CRTFNMEL PIC S9(0004) COMP.
000203     05  CRTFNMEF PIC  X(0001).
000204     05  FILLER REDEFINES CRTFNMEF.
000205         10  CRTFNMEA PIC  X(0001).
000206     05  CRTFNMEI PIC  X(0012).
000207*    -------------------------------
000208     05  CRTINITL PIC S9(0004) COMP.
000209     05  CRTINITF PIC  X(0001).
000210     05  FILLER REDEFINES CRTINITF.
000211         10  CRTINITA PIC  X(0001).
000212     05  CRTINITI PIC  X(0001).
000213*    -------------------------------
000214     05  LOANBALL PIC S9(0004) COMP.
000215     05  LOANBALF PIC  X(0001).
000216     05  FILLER REDEFINES LOANBALF.
000217         10  LOANBALA PIC  X(0001).
000218     05  LOANBALI PIC  9(7)V99.
000219*    -------------------------------
000220     05  PROCL PIC S9(0004) COMP.
000221     05  PROCF PIC  X(0001).
000222     05  FILLER REDEFINES PROCF.
000223         10  PROCA PIC  X(0001).
000224     05  PROCI PIC  X(0004).
000225*    -------------------------------
000226     05  SUPVL PIC S9(0004) COMP.
000227     05  SUPVF PIC  X(0001).
000228     05  FILLER REDEFINES SUPVF.
000229         10  SUPVA PIC  X(0001).
000230     05  SUPVI PIC  X(0001).
000231*    -------------------------------
000232     05  PRICDL PIC S9(0004) COMP.
000233     05  PRICDF PIC  X(0001).
000234     05  FILLER REDEFINES PRICDF.
000235         10  PRICDA PIC  X(0001).
000236     05  PRICDI PIC  X(0001).
000237*    -------------------------------
000238     05  CRITPL PIC S9(0004) COMP.
000239     05  CRITPF PIC  X(0001).
000240     05  FILLER REDEFINES CRITPF.
000241         10  CRITPA PIC  X(0001).
000242     05  CRITPI PIC  X(0002).
000243*    -------------------------------
000244     05  EXTENSL PIC S9(0004) COMP.
000245     05  EXTENSF PIC  X(0001).
000246     05  FILLER REDEFINES EXTENSF.
000247         10  EXTENSA PIC  X(0001).
000248     05  EXTENSI PIC  99.
000249*    -------------------------------
000250     05  FILETOL PIC S9(0004) COMP.
000251     05  FILETOF PIC  X(0001).
000252     05  FILLER REDEFINES FILETOF.
000253         10  FILETOA PIC  X(0001).
000254     05  FILETOI PIC  X(0004).
000255*    -------------------------------
000256     05  PTHRHDGL PIC S9(0004) COMP.
000257     05  PTHRHDGF PIC  X(0001).
000258     05  FILLER REDEFINES PTHRHDGF.
000259         10  PTHRHDGA PIC  X(0001).
000260     05  PTHRHDGI PIC  X(0010).
000261*    -------------------------------
000262     05  PDTHRUL PIC S9(0004) COMP.
000263     05  PDTHRUF PIC  X(0001).
000264     05  FILLER REDEFINES PDTHRUF.
000265         10  PDTHRUA PIC  X(0001).
000266     05  PDTHRUI PIC  X(0008).
000267*    -------------------------------
000268     05  PDAMTL PIC S9(0004) COMP.
000269     05  PDAMTF PIC  X(0001).
000270     05  FILLER REDEFINES PDAMTF.
000271         10  PDAMTA PIC  X(0001).
000272     05  PDAMTI PIC  9(7)V99.
000273*    -------------------------------
000274     05  NODAYSL PIC S9(0004) COMP.
000275     05  NODAYSF PIC  X(0001).
000276     05  FILLER REDEFINES NODAYSF.
000277         10  NODAYSA PIC  X(0001).
000278     05  NODAYSI PIC  9(5).
000279*    -------------------------------
000280     05  NOPMTSL PIC S9(0004) COMP.
000281     05  NOPMTSF PIC  X(0001).
000282     05  FILLER REDEFINES NOPMTSF.
000283         10  NOPMTSA PIC  X(0001).
000284     05  NOPMTSI PIC  9(4).
000285*    -------------------------------
000286     05  FORMTYPL PIC S9(0004) COMP.
000287     05  FORMTYPF PIC  X(0001).
000288     05  FILLER REDEFINES FORMTYPF.
000289         10  FORMTYPA PIC  X(0001).
000290     05  FORMTYPI PIC  X(0001).
000291*    -------------------------------
000292     05  ESTL PIC S9(0004) COMP.
000293     05  ESTF PIC  X(0001).
000294     05  FILLER REDEFINES ESTF.
000295         10  ESTA PIC  X(0001).
000296     05  ESTI PIC  X(0008).
000297*    -------------------------------
000298     05  AUIDL PIC S9(0004) COMP.
000299     05  AUIDF PIC  X(0001).
000300     05  FILLER REDEFINES AUIDF.
000301         10  AUIDA PIC  X(0001).
000302     05  AUIDI PIC  X(0004).
000303*    -------------------------------
000304     05  OCCL PIC S9(0004) COMP.
000305     05  OCCF PIC  X(0001).
000306     05  FILLER REDEFINES OCCF.
000307         10  OCCA PIC  X(0001).
000308     05  OCCI PIC  X(0006).
000309*    -------------------------------
000310     05  MNTDTL PIC S9(0004) COMP.
000311     05  MNTDTF PIC  X(0001).
000312     05  FILLER REDEFINES MNTDTF.
000313         10  MNTDTA PIC  X(0001).
000314     05  MNTDTI PIC  X(0008).
000315*    -------------------------------
000316     05  MNTTYPEL PIC S9(0004) COMP.
000317     05  MNTTYPEF PIC  X(0001).
000318     05  FILLER REDEFINES MNTTYPEF.
000319         10  MNTTYPEA PIC  X(0001).
000320     05  MNTTYPEI PIC  X(0006).
000321*    -------------------------------
000322     05  CERTCARL PIC S9(0004) COMP.
000323     05  CERTCARF PIC  X(0001).
000324     05  FILLER REDEFINES CERTCARF.
000325         10  CERTCARA PIC  X(0001).
000326     05  CERTCARI PIC  X(0001).
000327*    -------------------------------
000328     05  CERTGRPL PIC S9(0004) COMP.
000329     05  CERTGRPF PIC  X(0001).
000330     05  FILLER REDEFINES CERTGRPF.
000331         10  CERTGRPA PIC  X(0001).
000332     05  CERTGRPI PIC  X(0006).
000333*    -------------------------------
000334     05  CERTSTL PIC S9(0004) COMP.
000335     05  CERTSTF PIC  X(0001).
000336     05  FILLER REDEFINES CERTSTF.
000337         10  CERTSTA PIC  X(0001).
000338     05  CERTSTI PIC  X(0002).
000339*    -------------------------------
000340     05  CERTACTL PIC S9(0004) COMP.
000341     05  CERTACTF PIC  X(0001).
000342     05  FILLER REDEFINES CERTACTF.
000343         10  CERTACTA PIC  X(0001).
000344     05  CERTACTI PIC  X(0010).
000345*    -------------------------------
000346     05  CERTEFFL PIC S9(0004) COMP.
000347     05  CERTEFFF PIC  X(0001).
000348     05  FILLER REDEFINES CERTEFFF.
000349         10  CERTEFFA PIC  X(0001).
000350     05  CERTEFFI PIC  X(0008).
000351*    -------------------------------
000352     05  ISSAGEL PIC S9(0004) COMP.
000353     05  ISSAGEF PIC  X(0001).
000354     05  FILLER REDEFINES ISSAGEF.
000355         10  ISSAGEA PIC  X(0001).
000356     05  ISSAGEI PIC  X(0002).
000357*    -------------------------------
000358     05  APRL PIC S9(0004) COMP.
000359     05  APRF PIC  X(0001).
000360     05  FILLER REDEFINES APRF.
000361         10  APRA PIC  X(0001).
000362     05  APRI PIC  9(4)V9(4).
000363*    -------------------------------
000364     05  PMTFREQL PIC S9(0004) COMP.
000365     05  PMTFREQF PIC  X(0001).
000366     05  FILLER REDEFINES PMTFREQF.
000367         10  PMTFREQA PIC  X(0001).
000368     05  PMTFREQI PIC  99.
000369*    -------------------------------
000370     05  INDGRPL PIC S9(0004) COMP.
000371     05  INDGRPF PIC  X(0001).
000372     05  FILLER REDEFINES INDGRPF.
000373         10  INDGRPA PIC  X(0001).
000374     05  INDGRPI PIC  X(0001).
000375*    -------------------------------
000376     05  PREMTYPL PIC S9(0004) COMP.
000377     05  PREMTYPF PIC  X(0001).
000378     05  FILLER REDEFINES PREMTYPF.
000379         10  PREMTYPA PIC  X(0001).
000380     05  PREMTYPI PIC  X(0001).
000381*    -------------------------------
000382     05  REINCDL PIC S9(0004) COMP.
000383     05  REINCDF PIC  X(0001).
000384     05  FILLER REDEFINES REINCDF.
000385         10  REINCDA PIC  X(0001).
000386     05  REINCDI PIC  X(0001).
000387*    -------------------------------
000388     05  JNTLNMEL PIC S9(0004) COMP.
000389     05  JNTLNMEF PIC  X(0001).
000390     05  FILLER REDEFINES JNTLNMEF.
000391         10  JNTLNMEA PIC  X(0001).
000392     05  JNTLNMEI PIC  X(0015).
000393*    -------------------------------
000394     05  JNTFNMEL PIC S9(0004) COMP.
000395     05  JNTFNMEF PIC  X(0001).
000396     05  FILLER REDEFINES JNTFNMEF.
000397         10  JNTFNMEA PIC  X(0001).
000398     05  JNTFNMEI PIC  X(0012).
000399*    -------------------------------
000400     05  JNTINITL PIC S9(0004) COMP.
000401     05  JNTINITF PIC  X(0001).
000402     05  FILLER REDEFINES JNTINITF.
000403         10  JNTINITA PIC  X(0001).
000404     05  JNTINITI PIC  X(0001).
000405*    -------------------------------
000406     05  JNTAGEL PIC S9(0004) COMP.
000407     05  JNTAGEF PIC  X(0001).
000408     05  FILLER REDEFINES JNTAGEF.
000409         10  JNTAGEA PIC  X(0001).
000410     05  JNTAGEI PIC  X(0002).
000411*    -------------------------------
000412     05  ADDONDTL PIC S9(0004) COMP.
000413     05  ADDONDTF PIC  X(0001).
000414     05  FILLER REDEFINES ADDONDTF.
000415         10  ADDONDTA PIC  X(0001).
000416     05  ADDONDTI PIC  X(0008).
000417*    -------------------------------
000418     05  LCVDSCRL PIC S9(0004) COMP.
000419     05  LCVDSCRF PIC  X(0001).
000420     05  FILLER REDEFINES LCVDSCRF.
000421         10  LCVDSCRA PIC  X(0001).
000422     05  LCVDSCRI PIC  X(0006).
000423*    -------------------------------
000424     05  LCVKINDL PIC S9(0004) COMP.
000425     05  LCVKINDF PIC  X(0001).
000426     05  FILLER REDEFINES LCVKINDF.
000427         10  LCVKINDA PIC  X(0001).
000428     05  LCVKINDI PIC  X(0003).
000429*    -------------------------------
000430     05  LCVCDL PIC S9(0004) COMP.
000431     05  LCVCDF PIC  X(0001).
000432     05  FILLER REDEFINES LCVCDF.
000433         10  LCVCDA PIC  X(0001).
000434     05  LCVCDI PIC  X(0002).
000435*    -------------------------------
000436     05  LCVOTRML PIC S9(0004) COMP.
000437     05  LCVOTRMF PIC  X(0001).
000438     05  FILLER REDEFINES LCVOTRMF.
000439         10  LCVOTRMA PIC  X(0001).
000440     05  LCVOTRMI PIC  999.
000441*    -------------------------------
000442     05  LCVRTRML PIC S9(0004) COMP.
000443     05  LCVRTRMF PIC  X(0001).
000444     05  FILLER REDEFINES LCVRTRMF.
000445         10  LCVRTRMA PIC  X(0001).
000446     05  LCVRTRMI PIC  X(0003).
000447*    -------------------------------
000448     05  LCVRATEL PIC S9(0004) COMP.
000449     05  LCVRATEF PIC  X(0001).
000450     05  FILLER REDEFINES LCVRATEF.
000451         10  LCVRATEA PIC  X(0001).
000452     05  LCVRATEI PIC  9999V99.
000453*    -------------------------------
000454     05  LCVBENEL PIC S9(0004) COMP.
000455     05  LCVBENEF PIC  X(0001).
000456     05  FILLER REDEFINES LCVBENEF.
000457         10  LCVBENEA PIC  X(0001).
000458     05  LCVBENEI PIC  9(9)V99.
000459*    -------------------------------
000460     05  LCVFORML PIC S9(0004) COMP.
000461     05  LCVFORMF PIC  X(0001).
000462     05  FILLER REDEFINES LCVFORMF.
000463         10  LCVFORMA PIC  X(0001).
000464     05  LCVFORMI PIC  X(0012).
000465*    -------------------------------
000466     05  LCVCNDTL PIC S9(0004) COMP.
000467     05  LCVCNDTF PIC  X(0001).
000468     05  FILLER REDEFINES LCVCNDTF.
000469         10  LCVCNDTA PIC  X(0001).
000470     05  LCVCNDTI PIC  X(0008).
000471*    -------------------------------
000472     05  LCVEXITL PIC S9(0004) COMP.
000473     05  LCVEXITF PIC  X(0001).
000474     05  FILLER REDEFINES LCVEXITF.
000475         10  LCVEXITA PIC  X(0001).
000476     05  LCVEXITI PIC  X(0008).
000477*    -------------------------------
000478     05  LCVSTATL PIC S9(0004) COMP.
000479     05  LCVSTATF PIC  X(0001).
000480     05  FILLER REDEFINES LCVSTATF.
000481         10  LCVSTATA PIC  X(0001).
000482     05  LCVSTATI PIC  X(0006).
000483*    -------------------------------
000484     05  ACVDSCRL PIC S9(0004) COMP.
000485     05  ACVDSCRF PIC  X(0001).
000486     05  FILLER REDEFINES ACVDSCRF.
000487         10  ACVDSCRA PIC  X(0001).
000488     05  ACVDSCRI PIC  X(0006).
000489*    -------------------------------
000490     05  ACVKINDL PIC S9(0004) COMP.
000491     05  ACVKINDF PIC  X(0001).
000492     05  FILLER REDEFINES ACVKINDF.
000493         10  ACVKINDA PIC  X(0001).
000494     05  ACVKINDI PIC  X(0003).
000495*    -------------------------------
000496     05  ACVCDL PIC S9(0004) COMP.
000497     05  ACVCDF PIC  X(0001).
000498     05  FILLER REDEFINES ACVCDF.
000499         10  ACVCDA PIC  X(0001).
000500     05  ACVCDI PIC  X(0002).
000501*    -------------------------------
000502     05  ACVOTRML PIC S9(0004) COMP.
000503     05  ACVOTRMF PIC  X(0001).
000504     05  FILLER REDEFINES ACVOTRMF.
000505         10  ACVOTRMA PIC  X(0001).
000506     05  ACVOTRMI PIC  999.
000507*    -------------------------------
000508     05  ACVRTRML PIC S9(0004) COMP.
000509     05  ACVRTRMF PIC  X(0001).
000510     05  FILLER REDEFINES ACVRTRMF.
000511         10  ACVRTRMA PIC  X(0001).
000512     05  ACVRTRMI PIC  X(0003).
000513*    -------------------------------
000514     05  ACVRATEL PIC S9(0004) COMP.
000515     05  ACVRATEF PIC  X(0001).
000516     05  FILLER REDEFINES ACVRATEF.
000517         10  ACVRATEA PIC  X(0001).
000518     05  ACVRATEI PIC  9999V99.
000519*    -------------------------------
000520     05  ACVBENEL PIC S9(0004) COMP.
000521     05  ACVBENEF PIC  X(0001).
000522     05  FILLER REDEFINES ACVBENEF.
000523         10  ACVBENEA PIC  X(0001).
000524     05  ACVBENEI PIC  9(9)V99.
000525*    -------------------------------
000526     05  ACVFORML PIC S9(0004) COMP.
000527     05  ACVFORMF PIC  X(0001).
000528     05  FILLER REDEFINES ACVFORMF.
000529         10  ACVFORMA PIC  X(0001).
000530     05  ACVFORMI PIC  X(0012).
000531*    -------------------------------
000532     05  ACVCNDTL PIC S9(0004) COMP.
000533     05  ACVCNDTF PIC  X(0001).
000534     05  FILLER REDEFINES ACVCNDTF.
000535         10  ACVCNDTA PIC  X(0001).
000536     05  ACVCNDTI PIC  X(0008).
000537*    -------------------------------
000538     05  ACVEXITL PIC S9(0004) COMP.
000539     05  ACVEXITF PIC  X(0001).
000540     05  FILLER REDEFINES ACVEXITF.
000541         10  ACVEXITA PIC  X(0001).
000542     05  ACVEXITI PIC  X(0008).
000543*    -------------------------------
000544     05  ACVSTATL PIC S9(0004) COMP.
000545     05  ACVSTATF PIC  X(0001).
000546     05  FILLER REDEFINES ACVSTATF.
000547         10  ACVSTATA PIC  X(0001).
000548     05  ACVSTATI PIC  X(0006).
000549*    -------------------------------
000550     05  LABEL2L PIC S9(0004) COMP.
000551     05  LABEL2F PIC  X(0001).
000552     05  FILLER REDEFINES LABEL2F.
000553         10  LABEL2A PIC  X(0001).
000554     05  LABEL2I PIC  X(0014).
000555*    -------------------------------
000556     05  BCERT1L PIC S9(0004) COMP.
000557     05  BCERT1F PIC  X(0001).
000558     05  FILLER REDEFINES BCERT1F.
000559         10  BCERT1A PIC  X(0001).
000560     05  BCERT1I PIC  X(0010).
000561*    -------------------------------
000562     05  BSUFX1L PIC S9(0004) COMP.
000563     05  BSUFX1F PIC  X(0001).
000564     05  FILLER REDEFINES BSUFX1F.
000565         10  BSUFX1A PIC  X(0001).
000566     05  BSUFX1I PIC  X(0001).
000567*    -------------------------------
000568     05  BCERT2L PIC S9(0004) COMP.
000569     05  BCERT2F PIC  X(0001).
000570     05  FILLER REDEFINES BCERT2F.
000571         10  BCERT2A PIC  X(0001).
000572     05  BCERT2I PIC  X(0010).
000573*    -------------------------------
000574     05  BSUFX2L PIC S9(0004) COMP.
000575     05  BSUFX2F PIC  X(0001).
000576     05  FILLER REDEFINES BSUFX2F.
000577         10  BSUFX2A PIC  X(0001).
000578     05  BSUFX2I PIC  X(0001).
000579*    -------------------------------
000580     05  BCERT3L PIC S9(0004) COMP.
000581     05  BCERT3F PIC  X(0001).
000582     05  FILLER REDEFINES BCERT3F.
000583         10  BCERT3A PIC  X(0001).
000584     05  BCERT3I PIC  X(0010).
000585*    -------------------------------
000586     05  BSUFX3L PIC S9(0004) COMP.
000587     05  BSUFX3F PIC  X(0001).
000588     05  FILLER REDEFINES BSUFX3F.
000589         10  BSUFX3A PIC  X(0001).
000590     05  BSUFX3I PIC  X(0001).
000591*    -------------------------------
000592     05  BCERT4L PIC S9(0004) COMP.
000593     05  BCERT4F PIC  X(0001).
000594     05  FILLER REDEFINES BCERT4F.
000595         10  BCERT4A PIC  X(0001).
000596     05  BCERT4I PIC  X(0010).
000597*    -------------------------------
000598     05  BSUFX4L PIC S9(0004) COMP.
000599     05  BSUFX4F PIC  X(0001).
000600     05  FILLER REDEFINES BSUFX4F.
000601         10  BSUFX4A PIC  X(0001).
000602     05  BSUFX4I PIC  X(0001).
000603*    -------------------------------
000604     05  BCERT5L PIC S9(0004) COMP.
000605     05  BCERT5F PIC  X(0001).
000606     05  FILLER REDEFINES BCERT5F.
000607         10  BCERT5A PIC  X(0001).
000608     05  BCERT5I PIC  X(0010).
000609*    -------------------------------
000610     05  BSUFX5L PIC S9(0004) COMP.
000611     05  BSUFX5F PIC  X(0001).
000612     05  FILLER REDEFINES BSUFX5F.
000613         10  BSUFX5A PIC  X(0001).
000614     05  BSUFX5I PIC  X(0001).
000615*    -------------------------------
000616     05  MSG1L PIC S9(0004) COMP.
000617     05  MSG1F PIC  X(0001).
000618     05  FILLER REDEFINES MSG1F.
000619         10  MSG1A PIC  X(0001).
000620     05  MSG1I PIC  X(0072).
000621*    -------------------------------
000622     05  MSG2L PIC S9(0004) COMP.
000623     05  MSG2F PIC  X(0001).
000624     05  FILLER REDEFINES MSG2F.
000625         10  MSG2A PIC  X(0001).
000626     05  MSG2I PIC  X(0072).
000627*    -------------------------------
000628     05  PFKEYL PIC S9(0004) COMP.
000629     05  PFKEYF PIC  X(0001).
000630     05  FILLER REDEFINES PFKEYF.
000631         10  PFKEYA PIC  X(0001).
000632     05  PFKEYI PIC  99.
000633*    -------------------------------
000634     05  PFKEY6L PIC S9(0004) COMP.
000635     05  PFKEY6F PIC  X(0001).
000636     05  FILLER REDEFINES PFKEY6F.
000637         10  PFKEY6A PIC  X(0001).
000638     05  PFKEY6I PIC  X(0016).
000639 01  EL131AO REDEFINES EL131AI.
000640     05  FILLER            PIC  X(0012).
000641*    -------------------------------
000642     05  FILLER            PIC  X(0003).
000643     05  DATEO PIC  X(0008).
000644*    -------------------------------
000645     05  FILLER            PIC  X(0003).
000646     05  TIMEO PIC  99.99.
000647*    -------------------------------
000648     05  FILLER            PIC  X(0003).
000649     05  SEQUO PIC  X(0010).
000650*    -------------------------------
000651     05  FILLER            PIC  X(0003).
000652     05  COMPO PIC  X(0003).
000653*    -------------------------------
000654     05  FILLER            PIC  X(0003).
000655     05  USERIDO PIC  X(0004).
000656*    -------------------------------
000657     05  FILLER            PIC  X(0003).
000658     05  LABEL1O PIC  X(0011).
000659*    -------------------------------
000660     05  FILLER            PIC  X(0003).
000661     05  PCERTNOO PIC  X(0010).
000662*    -------------------------------
000663     05  FILLER            PIC  X(0003).
000664     05  PSUFXO PIC  X(0001).
000665*    -------------------------------
000666     05  FILLER            PIC  X(0003).
000667     05  CCNOO PIC  X(0016).
000668*    -------------------------------
000669     05  FILLER            PIC  X(0003).
000670     05  MAINTO PIC  X(0001).
000671*    -------------------------------
000672     05  FILLER            PIC  X(0003).
000673     05  CLAIMO PIC  X(0007).
000674*    -------------------------------
000675     05  FILLER            PIC  X(0003).
000676     05  CARRO PIC  X(0001).
000677*    -------------------------------
000678     05  FILLER            PIC  X(0003).
000679     05  CERTO PIC  X(0010).
000680*    -------------------------------
000681     05  FILLER            PIC  X(0003).
000682     05  SUFXO PIC  X(0001).
000683*    -------------------------------
000684     05  FILLER            PIC  X(0003).
000685     05  TYPEO PIC  X(0001).
000686*    -------------------------------
000687     05  FILLER            PIC  X(0003).
000688     05  BENPERO PIC  X(0002).
000689*    -------------------------------
000690     05  FILLER            PIC  X(0003).
000691     05  INSTYPEO PIC  X(0001).
000692*    -------------------------------
000693     05  FILLER            PIC  X(0003).
000694     05  STATUSO PIC  X(0006).
000695*    -------------------------------
000696     05  FILLER            PIC  X(0003).
000697     05  INCO PIC  X(0008).
000698*    -------------------------------
000699     05  FILLER            PIC  X(0003).
000700     05  REPO PIC  X(0008).
000701*    -------------------------------
000702     05  FILLER            PIC  X(0003).
000703     05  ICD1O PIC  X(0008).
000704*    -------------------------------
000705     05  FILLER            PIC  X(0003).
000706     05  ICD2O PIC  X(0008).
000707*    -------------------------------
000708     05  FILLER            PIC  X(0003).
000709     05  DIAGO PIC  X(0060).
000710*    -------------------------------
000711     05  FILLER            PIC  X(0003).
000712     05  ACCSWO PIC  X(0001).
000713*    -------------------------------
000714     05  FILLER            PIC  X(0003).
000715     05  BENEO PIC  X(0010).
000716*    -------------------------------
000717     05  FILLER            PIC  X(0003).
000718     05  BIRTHO PIC  X(0008).
000719*    -------------------------------
000720     05  FILLER            PIC  X(0003).
000721     05  SOCIALO PIC  X(0011).
000722*    -------------------------------
000723     05  FILLER            PIC  X(0003).
000724     05  SEXO PIC  X(0001).
000725*    -------------------------------
000726     05  FILLER            PIC  X(0003).
000727     05  MLNAMEO PIC  X(0015).
000728*    -------------------------------
000729     05  FILLER            PIC  X(0003).
000730     05  MFNAMEO PIC  X(0012).
000731*    -------------------------------
000732     05  FILLER            PIC  X(0003).
000733     05  MMINITO PIC  X(0001).
000734*    -------------------------------
000735     05  FILLER            PIC  X(0003).
000736     05  AUTHRCVO PIC  X(0001).
000737*    -------------------------------
000738     05  FILLER            PIC  X(0003).
000739     05  CRTLNMEO PIC  X(0015).
000740*    -------------------------------
000741     05  FILLER            PIC  X(0003).
000742     05  CRTFNMEO PIC  X(0012).
000743*    -------------------------------
000744     05  FILLER            PIC  X(0003).
000745     05  CRTINITO PIC  X(0001).
000746*    -------------------------------
000747     05  FILLER            PIC  X(0003).
000748     05  LOANBALO PIC  ZZZZZ9.99.
000749*    -------------------------------
000750     05  FILLER            PIC  X(0003).
000751     05  PROCO PIC  X(0004).
000752*    -------------------------------
000753     05  FILLER            PIC  X(0003).
000754     05  SUPVO PIC  X(0001).
000755*    -------------------------------
000756     05  FILLER            PIC  X(0003).
000757     05  PRICDO PIC  X(0001).
000758*    -------------------------------
000759     05  FILLER            PIC  X(0003).
000760     05  CRITPO PIC  X(0002).
000761*    -------------------------------
000762     05  FILLER            PIC  X(0003).
000763     05  EXTENSO PIC  X(0002).
000764*    -------------------------------
000765     05  FILLER            PIC  X(0003).
000766     05  FILETOO PIC  X(0004).
000767*    -------------------------------
000768     05  FILLER            PIC  X(0003).
000769     05  PTHRHDGO PIC  X(0010).
000770*    -------------------------------
000771     05  FILLER            PIC  X(0003).
000772     05  PDTHRUO PIC  X(0008).
000773*    -------------------------------
000774     05  FILLER            PIC  X(0003).
000775     05  PDAMTO PIC  Z(06).99.
000776*    -------------------------------
000777     05  FILLER            PIC  X(0003).
000778     05  NODAYSO PIC  ZZZ99.
000779*    -------------------------------
000780     05  FILLER            PIC  X(0003).
000781     05  NOPMTSO PIC  ZZ99.
000782*    -------------------------------
000783     05  FILLER            PIC  X(0003).
000784     05  FORMTYPO PIC  X(0001).
000785*    -------------------------------
000786     05  FILLER            PIC  X(0003).
000787     05  ESTO PIC  X(0008).
000788*    -------------------------------
000789     05  FILLER            PIC  X(0003).
000790     05  AUIDO PIC  X(0004).
000791*    -------------------------------
000792     05  FILLER            PIC  X(0003).
000793     05  OCCO PIC  X(0006).
000794*    -------------------------------
000795     05  FILLER            PIC  X(0003).
000796     05  MNTDTO PIC  X(0008).
000797*    -------------------------------
000798     05  FILLER            PIC  X(0003).
000799     05  MNTTYPEO PIC  X(0006).
000800*    -------------------------------
000801     05  FILLER            PIC  X(0003).
000802     05  CERTCARO PIC  X(0001).
000803*    -------------------------------
000804     05  FILLER            PIC  X(0003).
000805     05  CERTGRPO PIC  X(0006).
000806*    -------------------------------
000807     05  FILLER            PIC  X(0003).
000808     05  CERTSTO PIC  X(0002).
000809*    -------------------------------
000810     05  FILLER            PIC  X(0003).
000811     05  CERTACTO PIC  X(0010).
000812*    -------------------------------
000813     05  FILLER            PIC  X(0003).
000814     05  CERTEFFO PIC  X(0008).
000815*    -------------------------------
000816     05  FILLER            PIC  X(0003).
000817     05  ISSAGEO PIC  X(0002).
000818*    -------------------------------
000819     05  FILLER            PIC  X(0003).
000820     05  APRO PIC  9(3).9(4).
000821*    -------------------------------
000822     05  FILLER            PIC  X(0003).
000823     05  PMTFREQO PIC  99.
000824*    -------------------------------
000825     05  FILLER            PIC  X(0003).
000826     05  INDGRPO PIC  X(0001).
000827*    -------------------------------
000828     05  FILLER            PIC  X(0003).
000829     05  PREMTYPO PIC  X(0001).
000830*    -------------------------------
000831     05  FILLER            PIC  X(0003).
000832     05  REINCDO PIC  X(0001).
000833*    -------------------------------
000834     05  FILLER            PIC  X(0003).
000835     05  JNTLNMEO PIC  X(0015).
000836*    -------------------------------
000837     05  FILLER            PIC  X(0003).
000838     05  JNTFNMEO PIC  X(0012).
000839*    -------------------------------
000840     05  FILLER            PIC  X(0003).
000841     05  JNTINITO PIC  X(0001).
000842*    -------------------------------
000843     05  FILLER            PIC  X(0003).
000844     05  JNTAGEO PIC  X(0002).
000845*    -------------------------------
000846     05  FILLER            PIC  X(0003).
000847     05  ADDONDTO PIC  X(0008).
000848*    -------------------------------
000849     05  FILLER            PIC  X(0003).
000850     05  LCVDSCRO PIC  X(0006).
000851*    -------------------------------
000852     05  FILLER            PIC  X(0003).
000853     05  LCVKINDO PIC  X(0003).
000854*    -------------------------------
000855     05  FILLER            PIC  X(0003).
000856     05  LCVCDO PIC  X(0002).
000857*    -------------------------------
000858     05  FILLER            PIC  X(0003).
000859     05  LCVOTRMO PIC  999.
000860*    -------------------------------
000861     05  FILLER            PIC  X(0003).
000862     05  LCVRTRMO PIC  999.
000863*    -------------------------------
000864     05  FILLER            PIC  X(0003).
000865     05  LCVRATEO PIC  ZZZ.ZZ.
000866*    -------------------------------
000867     05  FILLER            PIC  X(0003).
000868     05  LCVBENEO PIC  ZZZZZZZZ.ZZ.
000869*    -------------------------------
000870     05  FILLER            PIC  X(0003).
000871     05  LCVFORMO PIC  X(0012).
000872*    -------------------------------
000873     05  FILLER            PIC  X(0003).
000874     05  LCVCNDTO PIC  X(0008).
000875*    -------------------------------
000876     05  FILLER            PIC  X(0003).
000877     05  LCVEXITO PIC  X(0008).
000878*    -------------------------------
000879     05  FILLER            PIC  X(0003).
000880     05  LCVSTATO PIC  X(0006).
000881*    -------------------------------
000882     05  FILLER            PIC  X(0003).
000883     05  ACVDSCRO PIC  X(0006).
000884*    -------------------------------
000885     05  FILLER            PIC  X(0003).
000886     05  ACVKINDO PIC  X(0003).
000887*    -------------------------------
000888     05  FILLER            PIC  X(0003).
000889     05  ACVCDO PIC  X(0002).
000890*    -------------------------------
000891     05  FILLER            PIC  X(0003).
000892     05  ACVOTRMO PIC  999.
000893*    -------------------------------
000894     05  FILLER            PIC  X(0003).
000895     05  ACVRTRMO PIC  999.
000896*    -------------------------------
000897     05  FILLER            PIC  X(0003).
000898     05  ACVRATEO PIC  ZZZ.ZZ.
000899*    -------------------------------
000900     05  FILLER            PIC  X(0003).
000901     05  ACVBENEO PIC  ZZZZZZZZ.ZZ.
000902*    -------------------------------
000903     05  FILLER            PIC  X(0003).
000904     05  ACVFORMO PIC  X(0012).
000905*    -------------------------------
000906     05  FILLER            PIC  X(0003).
000907     05  ACVCNDTO PIC  X(0008).
000908*    -------------------------------
000909     05  FILLER            PIC  X(0003).
000910     05  ACVEXITO PIC  X(0008).
000911*    -------------------------------
000912     05  FILLER            PIC  X(0003).
000913     05  ACVSTATO PIC  X(0006).
000914*    -------------------------------
000915     05  FILLER            PIC  X(0003).
000916     05  LABEL2O PIC  X(0014).
000917*    -------------------------------
000918     05  FILLER            PIC  X(0003).
000919     05  BCERT1O PIC  X(0010).
000920*    -------------------------------
000921     05  FILLER            PIC  X(0003).
000922     05  BSUFX1O PIC  X(0001).
000923*    -------------------------------
000924     05  FILLER            PIC  X(0003).
000925     05  BCERT2O PIC  X(0010).
000926*    -------------------------------
000927     05  FILLER            PIC  X(0003).
000928     05  BSUFX2O PIC  X(0001).
000929*    -------------------------------
000930     05  FILLER            PIC  X(0003).
000931     05  BCERT3O PIC  X(0010).
000932*    -------------------------------
000933     05  FILLER            PIC  X(0003).
000934     05  BSUFX3O PIC  X(0001).
000935*    -------------------------------
000936     05  FILLER            PIC  X(0003).
000937     05  BCERT4O PIC  X(0010).
000938*    -------------------------------
000939     05  FILLER            PIC  X(0003).
000940     05  BSUFX4O PIC  X(0001).
000941*    -------------------------------
000942     05  FILLER            PIC  X(0003).
000943     05  BCERT5O PIC  X(0010).
000944*    -------------------------------
000945     05  FILLER            PIC  X(0003).
000946     05  BSUFX5O PIC  X(0001).
000947*    -------------------------------
000948     05  FILLER            PIC  X(0003).
000949     05  MSG1O PIC  X(0072).
000950*    -------------------------------
000951     05  FILLER            PIC  X(0003).
000952     05  MSG2O PIC  X(0072).
000953*    -------------------------------
000954     05  FILLER            PIC  X(0003).
000955     05  PFKEYO PIC  X(0002).
000956*    -------------------------------
000957     05  FILLER            PIC  X(0003).
000958     05  PFKEY6O PIC  X(0016).
000959*    -------------------------------
      *<<((file: EL131S))
000852
000853*                                COPY ELCEMIB SUPPRESS.
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
000854
000855*                                COPY ELCJPFX SUPPRESS.
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
000856                                 PIC X(750).
000857
000858*                                COPY ELCCALC.
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
000859
000860*                                COPY ELCDATE.
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
000861
000862*                                COPY ELCDMO.
      *>>((file: ELCDMO))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ELCDMO.                             *
000005*                            VMOD=2.004                          *
000006*                                                                *
000007*   FILE DESCRIPTION = DLO025 (DMO FILE MAINTENANCE PGRM)        *
000008*        COMMUNICATION AREA                                      *
000009*   FILE TYPE = NA                                               *
000010*   RECORD SIZE = 110    RECFORM = FIXED                         *
000011*                                                                *
000012******************************************************************
000013 01  DMO-COMMUNICATION-AREA.
000014     12  DM-RECORD-TYPE                  PIC  X(02).
000015             88  DM-ISSUE-TRAN                VALUE 'CC'.
000016             88  DM-CLAIM-STATUS-CHANGE       VALUE 'CS'.
000017             88  DM-CLAIM-PAYMENT             VALUE 'DR'.
000018     12  DM-DIST-CODE                    PIC  X(04).
000019     12  DM-MAIL-CODE                    PIC  X(05).
000020     12  DM-CREDIT-CARD-NUMBER           PIC  X(16).
000021     12  DM-INSURED-NAME                 PIC  X(30).
000022     12  DM-CLAIM-NO                     PIC  X(07).
000023     12  DM-CLAIM-TYPE                   PIC  X.
000024
000025     12  DM-STATUS-DATA-AREA.
000026         16  DM-CLAIM-STATUS             PIC  X.
000027             88  DM-OPEN-NO-PAYMENTS              VALUE '1'.
000028             88  DM-OPEN-WITH-PAYMENTS            VALUE '2'.
000029             88  DM-CLOSED                        VALUE '3'.
000030             88  DM-CLOSE-SETTLE-FINAL            VALUE '4'.
000031             88  DM-DEFAULT                       VALUE '9'.
000032         16  DM-STATUS-DATE              PIC  X(08).
000033******YYYYMMDD
000034         16  DM-STAT-CHANGE-TYPE         PIC  X.
000035             88  DM-MANUAL-CLOSE                  VALUE 'C'.
000036             88  DM-CLAIM-DENIED                  VALUE 'D'.
000037             88  DM-FINAL-PAYMENT                 VALUE 'F'.
000038             88  DM-INITIAL-PAYMENT               VALUE 'I'.
000039             88  DM-AUTO-CLOSE                    VALUE 'Q'.
000040             88  DM-RE-OPENED                     VALUE 'R'.
000041             88  DM-NEW-CLAIM-SETUP               VALUE 'S'.
000042             88  DM-VOIDED-PAYMENT                VALUE 'V'.
000043             88  DM-CLAIM-DELETED                 VALUE 'X'.
000044         16  DM-STAT-CARRIER             PIC X.
000045
000046     12  DM-DRAFT-DATA-AREA.
000047         16  DM-PAYMENT-TYPE             PIC  X.
000048             88  DM-VALID-CLAIM-TYPES VALUES 'L' 'D' 'U' 'A'.
000049         16  DM-PAYMENT-AMT              PIC  9(05)V9(02).
000050         16  DM-PAYMENT-DATE             PIC  X(08).
000051******YYYYMMDD
000052         16  DM-CERT-NO                  PIC  X(11).
000053         16  DM-TRLR-SEQ-NO              PIC  9(04).
000054         16  DM-CARRIER                  PIC  X.
000055
000056     12  DM-RETURN-CODE                  PIC  XX.
      *<<((file: ELCDMO))
000863*                                COPY ERCPDEF.
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
000864*                                COPY ELCCRTT.
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
000865
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
000867 01  DFHCOMMAREA                 PIC X(1024).
000868
000869*                                COPY ELCMSTR.
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
000870
000871*                                COPY ELCCERT.
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
000872
000873*                                COPY ELCTRLR.
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
000874
000875*                                COPY ELCCNTL.
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
000876
000877*                                COPY ELCACTQ.
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
000878
000879*                                COPY ERCACCT.
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
000880
000881*                                COPY ELCCHKQ.
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
000882
000883*                                COPY ELCARCH.
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
000884
000885*                                COPY ELCBENE.
      *>>((file: ELCBENE))
000001******************************************************************
000002*                                                                *
000003*                            ELCBENE.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.006                          *
000006*                                                                *
000007*   FILE DESCRIPTION = BENEFICIARY FILE                          *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 500   RECFORM = FIX                            *
000011*                                                                *
000012*   BASE CLUSTER NAME = ELBENE                   RKP=2,LEN=12    *
000013*     ALTERNATE PATH1 = ELBENE2 (ALT BY NAME)    RKP=14,LEN=42   *
000014*                                                                *
000015*   LOG = YES                                                    *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017*                                                                *
000018*  NO  CID  MODS  TO  COPYBOOK  ELCBENE                          *
000019******************************************************************
000020*                   C H A N G E   L O G
000021*
000022* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000023*-----------------------------------------------------------------
000024*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000025* EFFECTIVE    NUMBER
000026*-----------------------------------------------------------------
000027* 013017  CR2016053100001  PEMA  ACH PROCESSING
000028* 082317  CR2017082100003  PEMA  Add sub type
000029* 032019  CR2019011400002  PEMA  Add email address for ach report
000030******************************************************************
000031
000032 01  BENEFICIARY-MASTER.
000033     12  BE-RECORD-ID                PIC XX.
000034         88  VALID-BE-ID                VALUE 'BE'.
000035
000036     12  BE-CONTROL-PRIMARY.
000037         16  BE-COMPANY-CD           PIC X.
000038         16  BE-RECORD-TYPE          PIC X.
000039             88  BENEFICIARY-RECORD  VALUE 'B'.
000040             88  ADJUSTOR-RECORD     VALUE 'A'.
000041         16  BE-BENEFICIARY          PIC X(10).
000042     12  BE-CONTROL-BY-NAME.
000043         16  BE-COMPANY-CD-A1        PIC X.
000044         16  BE-RECORD-TYPE-A1       PIC X.
000045         16  BE-MAIL-TO-NAME-A1      PIC X(30).
000046         16  BE-ALTERNATE-PRIME-A1   PIC X(10).
000047
000048     12  BE-LAST-MAINT-DT            PIC XX.
000049     12  BE-LAST-MAINT-BY            PIC X(4).
000050     12  BE-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
000051
000052     12  BE-ADDRESS-DATA.
000053         16  BE-MAIL-TO-NAME         PIC X(30).
000054         16  BE-ADDRESS-LINE-1       PIC X(30).
000055         16  BE-ADDRESS-LINE-2       PIC X(30).
000056         16  BE-ADDRESS-LINE-3       PIC X(30).
000057         16  BE-CITY-STATE.
000058             20  BE-CITY             PIC X(28).
000059             20  BE-STATE            PIC XX.
000060         16  BE-ZIP-CODE.
000061             20  BE-ZIP-PRIME.
000062                 24  BE-ZIP-1ST      PIC X.
000063                     88  BE-CANADIAN-POST-CODE
000064                                         VALUE 'A' THRU 'Z'.
000065                 24  FILLER          PIC X(4).
000066             20  BE-ZIP-PLUS4        PIC X(4).
000067         16  BE-CANADIAN-POSTAL-CODE  REDEFINES  BE-ZIP-CODE.
000068             20  BE-CAN-POSTAL-1     PIC XXX.
000069             20  BE-CAN-POSTAL-2     PIC XXX.
000070             20  FILLER              PIC XXX.
000071         16  BE-PHONE-NO             PIC 9(11)     COMP-3.
000072         16  BE-GROUP-CHECKS-Y-N     PIC X.
000073
000074******************************************************************
000075*    THE BE-CARRIER FIELD IS USED BY 'AIG' TO DETERMINE HOW TO   *
000076*    SET THE CARRIER CODE IN THE PENDING CLAIM FILE.             *
000077******************************************************************
000078     12  BE-CARRIER                  PIC X.
000079
000080     12  BE-ADDRESS-DATA2.
000081         16  BE-MAIL-TO-NAME2        PIC X(30).
000082         16  BE-ADDRESS-LINE-12      PIC X(30).
000083         16  BE-ADDRESS-LINE-22      PIC X(30).
000084         16  BE-ADDRESS-LINE-32      PIC X(30).
000085         16  BE-CITY-STATE2.
000086             20  BE-CITY2            PIC X(28).
000087             20  BE-STATE2           PIC XX.
000088         16  BE-ZIP-CODE2.
000089             20  BE-ZIP-PRIME2.
000090                 24  BE-ZIP-1ST2     PIC X.
000091                     88  BE-CANADIAN-POST-CODE2
000092                                         VALUE 'A' THRU 'Z'.
000093                 24  FILLER          PIC X(4).
000094             20  BE-ZIP-PLUS42       PIC X(4).
000095         16  BE-CANADIAN-POSTAL-CODE2 REDEFINES  BE-ZIP-CODE2.
000096             20  BE-CAN-POSTAL-12    PIC XXX.
000097             20  BE-CAN-POSTAL-22    PIC XXX.
000098             20  FILLER              PIC XXX.
000099         16  BE-PHONE-NO2            PIC 9(11)     COMP-3.
000100         16  BE-ACH-DATA.
000101             20  BE-ACH-YES-OR-NO    PIC X.
000102                 88  BE-ON-ACH       VALUE 'Y'.
000103                 88  BE-NOT-ON-ACH   VALUE 'N' ' '.
000104             20  BE-ACH-ABA-ROUTING-NUMBER
000105                                     PIC X(15).
000106             20  BE-ACH-BANK-ACCOUNT-NUMBER
000107                                     PIC X(20).
000108             20  BE-ACH-SUB-TYPE     PIC XX.
000109             20  BE-ACH-EMAIL-YN     PIC X.
000110                 88  BE-EMAIL-ACH-RPT  VALUE 'Y'.
000111             20  be-ach-email-addr   PIC X(40).
000112         16  BE-BILLING-STMT-DATA.
000113*            20  BE-BSR-PHONE-NUM    PIC 9(11)     COMP-3.
000114             20  BE-BSR-FAX-NUM      PIC 9(11)     COMP-3.
000115             20  BE-OUTPUT-TYPE      PIC X.
000116                 88  BE-FAX-OUTPUT         VALUE 'F'.
000117                 88  BE-PRINT-OUTPUT       VALUE 'P' ' '.
000118
000119     12  filler                      PIC X(16).
000120******************************************************************
      *<<((file: ELCBENE))
000886
000887*                                COPY ELCDAR.
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
000888
000889*                                COPY ELCALPH.
      *>>((file: ELCALPH))
000001******************************************************************
000002*                                                                *
000003*                            ELCALPH.                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.003                          *
000006*                                                                *
000007*   FILE DESCRIPTION = ALPHA CROSS REFERENCE FILE                *
000008*                                                                *
000009*   FILE TYPE = VSAM,KSDS                                        *
000010*   RECORD SIZE = 128  RECFORM = FIXED                           *
000011*                                                                *
000012*   BASE CLUSTER = ELALPH                         RKP=2,LEN=42   *
000013*       ALTERNATE PATH1 = ELALPH2 (FULL CONTROL)  RKP=44,LEN=57  *
000014*                                                                *
000015*   LOG = YES                                                    *
000016*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000017*                                                                *
000018*  NO  CID  MODS  IN  COPYBOOK  ELCALPH                          *
000019******************************************************************
000020
000021 01  ALPHA-INDEX.
000022     12  AI-RECORD-ID                         PIC XX.
000023         88  VALID-AI-ID                  VALUE 'AI'.
000024     12  AI-CONTROL-PRIMARY.
000025         16  AI-COMPANY-CD                     PIC X.
000026         16  AI-SOURCE                         PIC X.
000027             88  AI-CLAIM-SYSTEM          VALUE 'C'.
000028             88  AI-ADMIN-SYSTEM          VALUE 'A'.
000029         16  AI-NAME.
000030             20  AI-LAST-NAME                  PIC X(15).
000031             20  AI-FIRST-NAME.
000032                 24  AI-FIRST-INITIAL          PIC X.
000033                 24  FILLER                    PIC X(11).
000034             20  AI-MIDDLE-INIT                PIC X.
000035         16  AI-DATE                           PIC X(8).
000036         16  AI-TIME                           PIC S9(07) COMP-3.
000037
000038     12  AI-CONTROL-BY-ADMIN-KEY.
000039         16  AI-CM-COMPANY-CD                  PIC X.
000040         16  AI-CM-SOURCE                      PIC X.
000041         16  AI-CM-CARRIER                     PIC X.
000042         16  AI-CM-GROUPING.
000043             20  AI-CM-GROUPING-PREFIX         PIC X(3).
000044             20  AI-CM-GROUPING-PRIME          PIC X(3).
000045         16  AI-CM-STATE                       PIC XX.
000046         16  AI-CM-PRODUCER.
000047             20  AI-CM-PRODUCER-PREFIX         PIC X(4).
000048             20  AI-CM-PRODUCER-PRIME          PIC X(6).
000049         16  AI-CM-CERT-EFF-DT                 PIC XX.
000050         16  AI-CM-CERTIFICATE-NUMBER.
000051             20  AI-CM-CERT-PRIME              PIC X(10).
000052             20  AI-CM-CERT-SFX                PIC X.
000053         16  AI-CM-DATE                        PIC X(8).
000054         16  AI-CM-TIME                        PIC S9(7) COMP-3.
000055         16  FILLER                            PIC X(11).
000056
000057     12  AI-CONTROL-BY-CLAIM-KEY REDEFINES
000058         AI-CONTROL-BY-ADMIN-KEY.
000059         16  AI-CL-COMPANY-CD                  PIC X.
000060         16  AI-CL-SOURCE                      PIC X.
000061         16  AI-CL-CARRIER                     PIC X.
000062         16  AI-CL-CLAIM-NUMBER                PIC X(7).
000063         16  AI-CL-CERTIFICATE-NUMBER.
000064             20  AI-CL-CERT-PRIME              PIC X(10).
000065             20  AI-CL-CERT-SFX                PIC X.
000066         16  AI-CL-DATE                        PIC X(8).
000067         16  AI-CL-INCURRED-DATE               PIC XX.
000068         16  AI-CL-CLOSE-DATE                  PIC XX.
000069         16  AI-CL-TIME                        PIC S9(7)  COMP-3.
000070         16  AI-CREDIT-CARD-NUMBER.
000071             20  AI-CCN.
000072                 24  AI-CCN-PREFIX             PIC X(4).
000073                 24  AI-CCN-PRIME              PIC X(12).
000074             20  AI-CCN-FILLER                 PIC X(4).
000075
000076     12  AI-MAINT-INFO.
000077         16  AI-LAST-MAINT-BY                  PIC X(4).
000078         16  AI-LAST-MAINT-DT                  PIC XX.
000079         16  AI-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
000080
000081     12  AI-CLAIM-PAID-THRU-DT                 PIC XX.
000082
000083     12  FILLER                                PIC X(15).
000084
      *<<((file: ELCALPH))
000890
000891*                                COPY ERCDMDNT.
      *>>((file: ERCDMDNT))
000001******************************************************************
000002*                                                                *
000003*                            ERCDMDNT                            *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.005                          *
000006*                                                                *
000007*        FILE DESCRIPTION = DMD CERTIFICATE NOTES                *
000008*                                                                *
000009*        THIS COPYBOOK IS A REDEFINES OF ERCNOTE -               *
000010*                                                                *
000011*        FILE TYPE= VSAM,KSDS                                    *
000012*        RECORD SIZE = 825    RECFORM = FIXED                    *
000013*                                                                *
000014*        BASE CLUSTER = ERNOTE        RKP=2,LEN=33               *
000015*                                                                *
000016*        LOG = YES                                               *
000017*        SERVREQ = DELETE,UPDATE,NEWREC                          *
000018*                                                                *
000019******************************************************************
000020
000021 01  CERTIFICATE-NOTE.
000022     12  CN-RECORD-ID                     PIC  XX.
000023         88  VALID-CN-ID                      VALUE 'CN'.
000024
000025     12  CN-CONTROL-PRIMARY.
000026         16  CN-COMPANY-CD                PIC X.
000027         16  CN-CERT-KEY.
000028             20  CN-CARRIER               PIC X.
000029             20  CN-GROUPING.
000030                 24  CN-GROUPING-PREFIX   PIC XXX.
000031                 24  CN-GROUPING-PRIME    PIC XXX.
000032             20  CN-STATE                 PIC XX.
000033             20  CN-ACCOUNT.
000034                 24  CN-ACCOUNT-PREFIX    PIC X(4).
000035                 24  CN-ACCOUNT-PRIME     PIC X(6).
000036             20  CN-CERT-EFF-DT           PIC XX.
000037             20  CN-CERT-NO.
000038                 24  CN-CERT-PRIME        PIC X(10).
000039                 24  CN-CERT-SFX          PIC X.
000040
000041     12  CN-BILLING-START-LINE-NO         PIC 99.
000042     12  CN-BILLING-END-LINE-NO           PIC 99.
000043
000044     12  CN-LINES.
000045         16  CN-LINE                      PIC X(77)  OCCURS 10.
000046
000047     12  CN-CSI-NOTES REDEFINES CN-LINES.
000048         16  CN-CSI-TEXT-NOTES            PIC X(77)  OCCURS 6.
000049         16  CN-CSI-GENERAL-DATA-AREA.
000050             20  CN-CSI-GENERAL-DATA      PIC X(77)  OCCURS 2.
000051
000052         16  CN-CSI-GENERAL-DATA-R REDEFINES
000053             CN-CSI-GENERAL-DATA-AREA.
000054             20  CN-CSI-GEN-NOC-KEY           PIC X(11).
000055             20  CN-CSI-GEN-PRI-INSD-1ST-NAME PIC X(15).
000056             20  CN-CSI-GEN-SEC-INSD-1ST-NAME PIC X(15).
000057             20  CN-CSI-GEN-INSD-WORK-PHONE   PIC X(10).
000058             20  CN-CSI-GEN-INFRM-1ST-NAME    PIC X(15).
000059             20  CN-CSI-GEN-INFRM-LAST-NAME   PIC X(20).
000060             20  CN-CSI-GEN-INFRM-MI          PIC X.
000061             20  CN-CSI-GEN-INFRM-PHONE       PIC X(10).
000062             20  CN-CSI-GEN-INFRM-REL         PIC X(15).
000063             20  FILLER                       PIC XX.
000064             20  CN-CSI-GEN-DATA-SOURCE       PIC XX.
000065             20  FILLER                       PIC X(38).
000066
000067         16  CN-CSI-PRODUCT-DATA-AREA.
000068             20  CN-CSI-PRODUCT-DATA      PIC X(77)  OCCURS 2.
000069
000070         16  CN-CSI-CREDIT-CARD-DATA REDEFINES
000071             CN-CSI-PRODUCT-DATA-AREA.
000072             20  CN-CSI-CC-BILL-BANK-ID   PIC X(6).
000073             20  CN-CSI-CC-CANCEL-CD      PIC XX.
000074             20  CN-CSI-CC-CANCEL-DT      PIC X(8).
000075             20  CN-CSI-CC-CARD-TYPE      PIC XX.
000076             20  CN-CSI-CC-CHANGE-AGE     PIC 999.
000077             20  CN-CSI-CC-DIAGNOSIS-CD   PIC X(6).
000078             20  FILLER                   PIC XX.
000079             20  CN-CSI-CC-INSURED-BAL    PIC S9(5)V99  COMP-3.
000080             20  CN-CSI-CC-INTEREST-AMT   PIC S9(5)V99  COMP-3.
000081             20  CN-CSI-CC-INTEREST-PAID  PIC X.
000082             20  CN-CSI-CC-ISSUE-ST       PIC XX.
000083             20  CN-CSI-CC-MAX-BEN-LIMIT  PIC S9(5)V99  COMP-3.
000084             20  CN-CSI-CC-MAX-BENEFITS   PIC S9(5)V99  COMP-3.
000085             20  CN-CSI-CC-MIN-PAY-AMT    PIC S9(5)V99  COMP-3.
000086             20  CN-CSI-CC-MIN-PAY-PCT    PIC SV9(6)    COMP-3.
000087             20  CN-CSI-CC-OLD-ACCT-NO    PIC X(20).
000088             20  CN-CSI-CC-POLICY-TYPE    PIC XXX.
000089             20  CN-CSI-CC-PREMIUM-AMT    PIC S999V99   COMP-3.
000090             20  CN-CSI-CC-PREMIUM-RT     PIC S999V999  COMP-3.
000091             20  CN-CSI-CC-PREV-CLAIM-NO  PIC X(7).
000092             20  CN-CSI-CC-SIGNED-DT      PIC X(8).
000093             20  CN-CSI-CC-SPECIAL-TERM   PIC S999      COMP-3.
000094             20  CN-CSI-CC-STMNT-DT       PIC X(8).
000095             20  CN-CSI-CC-TERM-AGE       PIC 999.
000096             20  CN-CSI-CC-TOL-BALANCE    PIC S9(5)V99  COMP-3.
000097             20  CN-CSI-CC-WAIV-PREM-FLAG PIC X.
000098             20  CN-CSI-CC-ISSUE-DT       PIC X(8).
000099             20  CN-CSI-CC-BEN-CALC-SW    PIC X.
000100             20  CN-CSI-CC-TERM-ROUND-SW  PIC X.
000101             20  FILLER                   PIC X(25).
000102
000103         16  CN-CSI-FAMILY-LEAVE-DATA REDEFINES
000104             CN-CSI-CREDIT-CARD-DATA.
000105             20  CN-CSI-FL-BILL-BANK-ID   PIC X(6).
000106             20  CN-CSI-FL-CANCEL-CD      PIC XX.
000107             20  CN-CSI-FL-CANCEL-DT      PIC X(8).
000108             20  CN-CSI-FL-CARD-TYPE      PIC XX.
000109             20  CN-CSI-FL-CHANGE-AGE     PIC 999.
000110             20  CN-CSI-FL-DIAGNOSIS-CD   PIC X(6).
000111             20  FILLER                   PIC XX.
000112             20  CN-CSI-FL-INSURED-BAL    PIC S9(5)V99  COMP-3.
000113             20  CN-CSI-FL-INTEREST-AMT   PIC S9(5)V99  COMP-3.
000114             20  CN-CSI-FL-INTEREST-PAID  PIC X.
000115             20  CN-CSI-FL-ISSUE-ST       PIC XX.
000116             20  CN-CSI-FL-MAX-BEN-LIMIT  PIC S9(5)V99  COMP-3.
000117             20  CN-CSI-FL-MAX-BENEFITS   PIC S9(5)V99  COMP-3.
000118             20  CN-CSI-FL-MIN-PAY-AMT    PIC S9(5)V99  COMP-3.
000119             20  CN-CSI-FL-MIN-PAY-PCT    PIC SV9(6)    COMP-3.
000120             20  CN-CSI-FL-OLD-ACCT-NO    PIC X(20).
000121             20  CN-CSI-FL-POLICY-TYPE    PIC XXX.
000122             20  CN-CSI-FL-PREMIUM-AMT    PIC S999V99   COMP-3.
000123             20  CN-CSI-FL-PREMIUM-RT     PIC S999V999  COMP-3.
000124             20  CN-CSI-FL-PREV-CLAIM-NO  PIC X(7).
000125             20  CN-CSI-FL-SIGNED-DT      PIC X(8).
000126             20  CN-CSI-FL-SPECIAL-TERM   PIC S999      COMP-3.
000127             20  CN-CSI-FL-STMNT-DT       PIC X(8).
000128             20  CN-CSI-FL-TERM-AGE       PIC 999.
000129             20  CN-CSI-FL-TOL-BALANCE    PIC S9(5)V99  COMP-3.
000130             20  CN-CSI-FL-WAIV-PREM-FLAG PIC X.
000131             20  CN-CSI-FL-ISSUE-DT       PIC X(8).
000132             20  CN-CSI-FL-BEN-CALC-SW    PIC X.
000133             20  CN-CSI-FL-TERM-ROUND-SW  PIC X.
000134             20  CN-CSI-FL-LAST-DAY-WRKED PIC X(8).
000135             20  FILLER                   PIC X(17).
000136
000137         16  CN-CSI-SENIOR-LIFE-DATA REDEFINES
000138             CN-CSI-FAMILY-LEAVE-DATA.
000139             20  CN-CSI-SL-BENE-DOB       PIC X(8).
000140             20  CN-CSI-SL-BENE-NAME      PIC X(27).
000141             20  CN-CSI-SL-BENE-REL       PIC X(8).
000142             20  CN-CSI-SL-BENE-SSN       PIC S9(9)     COMP-3.
000143             20  CN-CSI-SL-BILL-BANK-ID   PIC X(6).
000144             20  CN-CSI-SL-CANCEL-DT      PIC X(8).
000145             20  CN-CSI-SL-DIAGNOSIS-CD   PIC X(6).
000146             20  CN-CSI-SL-INT-CHECK-DT   PIC X(8).
000147             20  CN-CSI-SL-INT-CHECK-NO   PIC S9(7)     COMP-3.
000148             20  CN-CSI-SL-INT-ON-PROCEEDS
000149                                          PIC S9(5)V99  COMP-3.
000150             20  CN-CSI-SL-ISSUE-DT       PIC X(8).
000151             20  CN-CSI-SL-ISSUE-ST       PIC XX.
000152             20  CN-CSI-SL-LIFE-PROCEEDS  PIC S9(5)V99  COMP-3.
000153             20  CN-CSI-SL-LOAN-INT-DUE   PIC S9(5)V99  COMP-3.
000154             20  CN-CSI-SL-POLICY-BENEFITS
000155                                          PIC S9(5)V99  COMP-3.
000156             20  CN-CSI-SL-POLICY-TYPE    PIC XXX.
000157             20  CN-CSI-SL-PREM-AMT       PIC S9(5)V99  COMP-3.
000158             20  CN-CSI-SL-PREM-CHECK-DT  PIC X(8).
000159             20  CN-CSI-SL-PREM-CHECK-NO  PIC S9(7)     COMP-3.
000160             20  CN-CSI-SL-PREM-DUE       PIC S9(5)V99  COMP-3.
000161             20  CN-CSI-SL-PREM-MODE      PIC 99.
000162             20  CN-CSI-SL-PREM-REFUND    PIC S9(5)V99  COMP-3.
000163             20  CN-CSI-SL-PREM-SUSP-DT   PIC X(8).
000164             20  CN-CSI-SL-SIGNED-DT      PIC X(8).
000165             20  CN-CSI-SL-STATE-NOT      PIC X.
000166             20  FILLER                   PIC XX.
000167
000168         16  CN-CSI-PURCH-PROP-DATA REDEFINES
000169             CN-CSI-SENIOR-LIFE-DATA.
000170             20  CN-CSI-PP-CARD-TYPE      PIC XX.
000171             20  CN-CSI-PP-CHANGE-AGE     PIC 999.
000172             20  CN-CSI-PP-BEN-PAID-TO-DATE
000173                                          PIC S9(5)V99  COMP-3.
000174             20  CN-CSI-PP-BILL-BANK-ID   PIC X(6).
000175             20  CN-CSI-PP-CANCEL-CD      PIC XX.
000176             20  CN-CSI-PP-CANCEL-DT      PIC X(8).
000177             20  CN-CSI-PP-DIAGNOSIS-CD   PIC X(6).
000178             20  CN-CSI-PP-ISSUE-DT       PIC X(8).
000179             20  CN-CSI-PP-ISSUE-ST       PIC XX.
000180             20  CN-CSI-PP-MANUFACTURER   PIC X(17).
000181             20  CN-CSI-PP-MODEL-NO       PIC X(8).
000182             20  CN-CSI-PP-OLD-ACCT-NO    PIC X(20).
000183             20  CN-CSI-PP-POLICY-TYPE    PIC XXX.
000184             20  CN-CSI-PP-PREMIUM-RT     PIC S999V999  COMP-3.
000185             20  CN-CSI-PP-PREV-CLAIM-NO  PIC X(7).
000186             20  CN-CSI-PP-PURCHASE-DT    PIC X(8).
000187             20  CN-CSI-PP-PURCHASE-PRICE PIC S9(5)V99  COMP-3.
000188             20  CN-CSI-PP-REPAIR         PIC X.
000189             20  CN-CSI-PP-REPLACE        PIC X.
000190             20  CN-CSI-PP-SERIAL-NO      PIC X(16).
000191             20  CN-CSI-PP-SIGNED-DT      PIC X(8).
000192             20  CN-CSI-PP-STMNT-DT       PIC X(8).
000193             20  CN-CSI-PP-TERM-AGE       PIC 999.
000194             20  FILLER                   PIC X(5).
000195
000196         16  CN-CSI-EXT-WARR-DATA REDEFINES
000197             CN-CSI-PURCH-PROP-DATA.
000198             20  CN-CSI-EW-CARD-TYPE      PIC XX.
000199             20  CN-CSI-EW-CHANGE-AGE     PIC 999.
000200             20  CN-CSI-EW-BILL-BANK-ID   PIC X(6).
000201             20  CN-CSI-EW-CANCEL-CD      PIC XX.
000202             20  CN-CSI-EW-CANCEL-DT      PIC X(8).
000203             20  CN-CSI-EW-DIAGNOSIS-CD   PIC X(6).
000204             20  CN-CSI-EW-ISSUE-DT       PIC X(8).
000205             20  CN-CSI-EW-ISSUE-ST       PIC XX.
000206             20  CN-CSI-EW-MANUFACTURER   PIC X(17).
000207             20  CN-CSI-EW-MODEL-NO       PIC X(8).
000208             20  CN-CSI-EW-OLD-ACCT-NO    PIC X(20).
000209             20  CN-CSI-EW-POLICY-TYPE    PIC XXX.
000210             20  CN-CSI-EW-PREMIUM-RT     PIC S999V999  COMP-3.
000211             20  CN-CSI-EW-PREV-CLAIM-NO  PIC X(7).
000212             20  CN-CSI-EW-PURCHASE-DT    PIC X(8).
000213             20  CN-CSI-EW-PURCHASE-PRICE PIC S9(5)V99  COMP-3.
000214             20  CN-CSI-EW-REPAIR-COST    PIC S9(5)V99  COMP-3.
000215             20  CN-CSI-EW-REPLACE        PIC X.
000216             20  CN-CSI-EW-SERIAL-NO      PIC X(16).
000217             20  CN-CSI-EW-SIGNED-DT      PIC X(8).
000218             20  CN-CSI-EW-STMNT-DT       PIC X(8).
000219             20  CN-CSI-EW-TERM-AGE       PIC 999.
000220             20  CN-CSI-EW-WARRANTY-NO    PIC 99.
000221             20  FILLER                   PIC X(4).
000222
000223     12  CN-LAST-MAINT-DT                 PIC XX.
000224     12  CN-LAST-MAINT-HHMMSS             PIC S9(7)     COMP-3.
000225     12  CN-LAST-MAINT-USER               PIC X(4).
000226     12  FILLER                           PIC X(6).
000227
000228******************************************************************
      *<<((file: ERCDMDNT))
000892
000893     EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL131' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000894 VCOBOL-DUMMY-PROCEDURE.
000895
000896     IF EIBCALEN = ZERO
000897         GO TO 8800-UNAUTHORIZED-ACCESS.
000898
000899     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
000900     MOVE '5'                    TO DC-OPTION-CODE.
000901     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
000902     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000903     MOVE DC-GREG-DATE-1-YMD     TO  SAVE-DATE-YMD.
000904     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000905
000906     IF SAVE-DATE-YY GREATER 70
000907         MOVE 19                 TO SAVE-DATE-CC
000908      ELSE
000909         MOVE 20                 TO SAVE-DATE-CC.
000910
000911     
      * EXEC CICS HANDLE CONDITION
000912*        PGMIDERR (8820-XCTL-ERROR)
000913*        NOTFND   (8150-ENTERED-CLAIM-NOTFOUND)
000914*        ERROR    (9990-ABEND)
000915*    END-EXEC.
      *    MOVE '"$LI.                 ! " #00007654' TO DFHEIV0
           MOVE X'22244C492E20202020202020' &
                X'202020202020202020202120' &
                X'2220233030303037363534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000916
000917     MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
000918     MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.
000919     MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.
000920
000921     INITIALIZE    EDIT-WORK-AREA.
000922     MOVE '/'                    TO WORK-SLASH.
000923     MOVE SPACES                 TO ERROR-SWITCHES.
000924
000925     MOVE TRAN-ID                TO TRANS-ID.
000926     MOVE EIBTRMID               TO PI-TERM.
000927     MOVE MAP-ID                 TO PI-QUAL.
000928
000929     MOVE 2                      TO EMI-NUMBER-OF-LINES.
000930
000931     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000932         MOVE LOW-VALUES         TO EL131AO
000933         MOVE ER-0008            TO EMI-ERROR
000934         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000935         MOVE -1                 TO MAINTL
000936         GO TO 8110-SEND-DATA.
000937
000938     IF THIS-PGM NOT = PI-CALLING-PROGRAM
000939         MOVE LOW-VALUES         TO EL131AO
000940         MOVE PI-CALLING-PROGRAM TO WS-RETURNED-FROM
000941         PERFORM 0100-UPDATE-PI THRU 0120-UPDATE-PI-EXIT
000942         MOVE 'X'                TO PI-RETURN-CD-1
000943         PERFORM 5000-BUILD-MAP THRU 5000-EXIT
000944         MOVE -1                 TO MAINTL
000945         IF WS-RETURNED-FROM = XCTL-EL114
000946             PERFORM 4200-LOAD-CHANGES THRU 4200-EXIT
000947         END-IF
000948         GO TO 8100-SEND-MAP.
000949
000950     IF EIBAID = DFHCLEAR
000951         GO TO 8200-RETURN-PRIOR.
000952
000953     IF PI-PROCESSOR-ID = 'LGXX'
000954         NEXT SENTENCE
000955     ELSE
000956         
      * EXEC CICS READQ TS
000957*            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
000958*            INTO    (SECURITY-CONTROL)
000959*            LENGTH  (SC-COMM-LENGTH)
000960*            ITEM    (SC-ITEM)
000961*        END-EXEC
      *    MOVE '*$II   L              ''   #00007699' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037363939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000962         MOVE SC-CLAIMS-DISPLAY (5)    TO  PI-DISPLAY-CAP
000963         MOVE SC-CLAIMS-UPDATE  (5)    TO  PI-MODIFY-CAP
000964         IF NOT DISPLAY-CAP
000965             MOVE 'READ'               TO  SM-READ
000966             PERFORM 9995-SECURITY-VIOLATION
000967             MOVE ER-0070              TO  EMI-ERROR
000968             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000969             GO TO 8100-SEND-MAP.
000970
000971     
      * EXEC CICS RECEIVE
000972*        MAP    ('EL131A')
000973*        MAPSET ('EL131S')
000974*    END-EXEC.
           MOVE 'EL131A' TO DFHEIV1
           MOVE 'EL131S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00007714' TO DFHEIV0
           MOVE X'382254202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037373134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL131AI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000975
000976     IF PFKEYL GREATER THAN ZERO
000977         PERFORM 0200-TRANS-PF THRU 0210-EXIT.
000978
000979     IF SCREEN-ERROR
000980         MOVE ER-0004 TO EMI-ERROR
000981         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000982         MOVE -1 TO MAINTL
000983         GO TO 8110-SEND-DATA.
000984
000985     IF EIBAID = DFHPF3
000986         GO TO 8500-TRLR-MNT.
000987
000988     IF EIBAID = DFHPF4
000989         GO TO 8600-ADDR-MNT.
000990
000991     IF EIBAID = DFHPF5
000992         GO TO 8700-CERT-MNT.
000993
000994     IF PI-COMPANY-ID = 'DMD'
000995       IF EIBAID = DFHPF6
000996         GO TO 8750-DMD-CLM-FIX.
000997
000998     IF EIBAID = DFHPF7
000999         GO TO 4000-FORCE-ERRORS.
001000
001001     IF EIBAID = DFHPF12
001002         GO TO 8300-GET-HELP.
001003
001004     IF EIBAID = DFHPF15
001005         GO TO 8725-BENEFICIARY-MNT
001006     END-IF.
001007
001008     IF EIBAID = DFHPF23
001009         GO TO 8810-PF23-ENTERED.
001010
001011     IF EIBAID = DFHPF24
001012         GO TO 8400-RETURN-MASTER.
001013
001014     IF EIBAID NOT = DFHENTER
001015         MOVE ER-0029            TO EMI-ERROR
001016         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001017         MOVE -1                 TO MAINTL
001018         GO TO 8110-SEND-DATA.
001019
001020     IF PI-RETURN-CD-1 =  'X'
001021         MOVE ER-0311            TO EMI-ERROR
001022         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001023         MOVE -1                 TO MAINTL
001024         GO TO 8110-SEND-DATA.
001025
001026     IF MAINTI = 'A'
001027         GO TO 0500-ADD-ALPHA-ONLY.
001028
001029     IF MAINTI = 'N'
001030         GO TO 0600-CHANGE-NAME.
001031
001032     PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.
001033
001034     PERFORM 1030-EDIT-NAME THRU 1030-EXIT.
001035
001036     IF SCREEN-ERROR
001037         GO TO 8110-SEND-DATA.
001038
001039     IF NOT UPDATES-PRESENT AND NOT DELETE-CLAIM
001040         MOVE ER-0276            TO EMI-ERROR
001041         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001042         MOVE -1                 TO MAINTL
001043         GO TO 8110-SEND-DATA.
001044
001045     PERFORM 2000-UPDATE-CLAIM THRU 2000-EXIT.
001046
001047     IF SCREEN-ERROR
001048         GO TO 8110-SEND-DATA.
001049
001050     MOVE 'W'                    TO EMI-ACTION-SWITCH.
001051
001052     PERFORM 5000-BUILD-MAP THRU 5000-EXIT.
001053
001054     IF EMI-FORCABLE-CTR GREATER THAN ZERO
001055         GO TO 8110-SEND-DATA.
001056
001057     IF EMI-FATAL-CTR GREATER THAN ZERO
001058         GO TO 8110-SEND-DATA.
001059
001060     MOVE SPACE                  TO EMI-ACTION-SWITCH.
001061
001062     IF WS-OPEN-CLOSE-SW = 'Y'
001063         IF PI-LETTER-SW = 'Y'
001064             MOVE PI-COMPANY-ID  TO  CNTL-CO-ID
001065             MOVE 'T'            TO  CNTL-REC-TYPE
001066             MOVE SPACES         TO  CNTL-PROC-ID
001067             MOVE +0             TO  CNTL-SEQ-NO
001068             
      * EXEC CICS READ
001069*                DATASET   (CNTL-FILE-ID)
001070*                RIDFLD    (CNTL-KEY)
001071*                SET       (ADDRESS OF CONTROL-FILE)
001072*            END-EXEC
      *    MOVE '&"S        E          (   #00007811' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037383131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001073             PERFORM 7850-AUTO-LETTER-WRITER THRU 7850-EXIT.
001074
001075     MOVE ER-0000                TO EMI-ERROR.
001076     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001077
001078     MOVE SPACE                  TO MAINTO.
001079     MOVE -1                     TO MAINTL.
001080     GO TO 8100-SEND-MAP.
001081
001082     EJECT
001083 0100-UPDATE-PI.
001084
001085     MOVE LOW-VALUES             TO WS-SAVE-BENEFICIARY.
001086     IF WS-RETURNED-FROM = XCTL-EL114
001087***sometimes there is crap in positions 7-10 when no code was sele
001088        IF PI-PROGRAM-WORK-AREA (1:5) > SPACES
001089          MOVE PI-PROGRAM-WORK-AREA (1:10) TO WS-SAVE-BENEFICIARY
001090        END-IF
001091     END-IF.
001092
001093     IF PI-RETURN-TO-PROGRAM = THIS-PGM
001094         GO TO 0110-UPDATE-UP.
001095
001096     MOVE PI-SAVED-PROGRAM-5     TO PI-SAVED-PROGRAM-6.
001097     MOVE PI-SAVED-PROGRAM-4     TO PI-SAVED-PROGRAM-5.
001098     MOVE PI-SAVED-PROGRAM-3     TO PI-SAVED-PROGRAM-4.
001099     MOVE PI-SAVED-PROGRAM-2     TO PI-SAVED-PROGRAM-3.
001100     MOVE PI-SAVED-PROGRAM-1     TO PI-SAVED-PROGRAM-2.
001101     MOVE PI-RETURN-TO-PROGRAM   TO PI-SAVED-PROGRAM-1.
001102     MOVE PI-CALLING-PROGRAM     TO PI-RETURN-TO-PROGRAM.
001103     MOVE THIS-PGM               TO PI-CALLING-PROGRAM.
001104     GO TO 0120-UPDATE-PI-EXIT.
001105
001106 0110-UPDATE-UP.
001107     MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM.
001108     MOVE PI-SAVED-PROGRAM-1 TO PI-RETURN-TO-PROGRAM.
001109     MOVE PI-SAVED-PROGRAM-2 TO PI-SAVED-PROGRAM-1.
001110     MOVE PI-SAVED-PROGRAM-3 TO PI-SAVED-PROGRAM-2.
001111     MOVE PI-SAVED-PROGRAM-4 TO PI-SAVED-PROGRAM-3.
001112     MOVE PI-SAVED-PROGRAM-5 TO PI-SAVED-PROGRAM-4.
001113     MOVE PI-SAVED-PROGRAM-6 TO PI-SAVED-PROGRAM-5.
001114     MOVE SPACES             TO PI-SAVED-PROGRAM-6.
001115
001116     
      * EXEC CICS HANDLE CONDITION
001117*         QIDERR    (0130-TS-ERROR)
001118*         ITEMERR   (0130-TS-ERROR)
001119*    END-EXEC.
      *    MOVE '"$N<                  ! # #00007859' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303037383539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001120
001121     IF WS-RETURNED-FROM = XCTL-EL114
001122        
      * EXEC CICS READQ TS
001123*            QUEUE    (PI-KEY)
001124*            INTO     (EL131AI)
001125*            LENGTH   (MAP-LENGTH)
001126*       END-EXEC
      *    MOVE '*$I    L              ''   #00007865' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037383635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 EL131AI, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001127
001128        IF WS-SAVE-BENEFICIARY > SPACES
001129            MOVE WS-SAVE-BENEFICIARY TO BENEI
001130            MOVE +10            TO BENEL
001131            MOVE LOW-VALUES TO WS-SAVE-BENEFICIARY
001132        END-IF
001133        PERFORM 4100-SAVE-CHANGES THRU 4100-EXIT
001134     END-IF.
001135
001136     
      * EXEC CICS READQ TS
001137*         QUEUE    (PI-KEY)
001138*         INTO     (PROGRAM-INTERFACE-BLOCK)
001139*         LENGTH   (PI-COMM-LENGTH)
001140*    END-EXEC.
      *    MOVE '*$I    L              ''   #00007879' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037383739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001141
001142     
      * EXEC CICS DELETEQ TS
001143*         QUEUE    (PI-KEY)
001144*    END-EXEC.
      *    MOVE '*&                    #   #00007885' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037383835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001145
001146 0120-UPDATE-PI-EXIT.
001147     EXIT.
001148
001149 0130-TS-ERROR.
001150     MOVE LOW-VALUES             TO EL131AO.
001151     MOVE ER-0192                TO EMI-ERROR.
001152
001153     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001154
001155     GO TO 8100-SEND-MAP.
001156
001157 0200-TRANS-PF.
001158     IF EIBAID NOT = DFHENTER
001159         MOVE 'X'                TO ERROR-SWITCH
001160         GO TO 0210-EXIT.
001161
001162     IF PFKEYI NOT NUMERIC
001163         MOVE 'X'                TO ERROR-SWITCH
001164         GO TO 0210-EXIT.
001165
001166     MOVE PFKEYI                 TO CHECK-PFKEYS.
001167
001168     IF CHECK-PFKEYS LESS 1 OR GREATER 24
001169         MOVE 'X' TO ERROR-SWITCH
001170         GO TO 0210-EXIT.
001171
001172     MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
001173
001174 0210-EXIT.
001175     EXIT.
001176
001177     EJECT
001178 0500-ADD-ALPHA-ONLY.
001179******************************************************************
001180*    ADD NAME TO ALPHA FILE.  NO CHANGES ARE MADE TO THE CLAIM   *
001181*    RECORD.                                                     *
001182******************************************************************
001183
001184     IF MLNAMEL = +0 AND
001185        MFNAMEL = +0 AND
001186        MMINITL = +0
001187         MOVE ER-0797                TO  EMI-ERROR
001188         MOVE -1                     TO  MAINTL
001189         MOVE AL-UABON               TO  MAINTA
001190         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001191         GO TO 8110-SEND-DATA.
001192
001193     MOVE PI-COMPANY-CD              TO  COMPANY-CODE.
001194     MOVE PI-CARRIER                 TO  CARRIER-CODE.
001195     MOVE PI-CLAIM-NO                TO  CLAIM-NO.
001196     MOVE PI-CERT-NO                 TO  CERT-NO.
001197
001198     
      * EXEC CICS READ
001199*        DATASET   (CLMS-FILE-ID)
001200*        RIDFLD    (MSTR-KEY)
001201*        SET       (ADDRESS OF CLAIM-MASTER)
001202*    END-EXEC.
      *    MOVE '&"S        E          (   #00007941' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037393431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001203
001204     
      * EXEC CICS GETMAIN
001205*        SET       (ADDRESS OF ALPHA-INDEX)
001206*        LENGTH    (ALPH-LENGTH)
001207*        INITIMG   (GETMAIN-SPACE)
001208*    END-EXEC.
      *    MOVE ',"IL                  $   #00007947' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037393437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ALPH-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ALPHA-INDEX TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001209
001210     MOVE 'AI'                       TO  AI-RECORD-ID.
001211     MOVE CL-COMPANY-CD              TO  AI-COMPANY-CD
001212                                         AI-CL-COMPANY-CD.
001213     MOVE 'C'                        TO  AI-SOURCE
001214                                         AI-CL-SOURCE.
001215
001216     IF MLNAMEL IS GREATER THAN +0
001217         MOVE MLNAMEI                TO  AI-LAST-NAME
001218     ELSE
001219         MOVE CL-INSURED-LAST-NAME   TO  AI-LAST-NAME.
001220
001221     IF MFNAMEL IS GREATER THAN +0
001222         MOVE MFNAMEI                TO  AI-FIRST-NAME
001223     ELSE
001224         MOVE CL-INSURED-1ST-NAME    TO  AI-FIRST-NAME.
001225
001226     IF MMINITL IS GREATER THAN +0
001227         MOVE MMINITI                TO  AI-MIDDLE-INIT
001228     ELSE
001229         MOVE CL-INSURED-MID-INIT    TO  AI-MIDDLE-INIT.
001230
001231     MOVE EIBDATE                    TO  DC-JULIAN-YYDDD.
001232     MOVE '5'                        TO  DC-OPTION-CODE.
001233     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
001234     MOVE DC-GREG-DATE-1-MDY         TO  BUILD-VALID-DATE.
001235     IF BUILD-YEAR IS GREATER THAN 50
001236         MOVE '19'                   TO  WS-ALPHA-YY-1
001237         MOVE BUILD-YEAR             TO  WS-ALPHA-YY-2
001238         MOVE BUILD-MONTH            TO  WS-ALPHA-MM
001239         MOVE BUILD-DAY              TO  WS-ALPHA-DD
001240     ELSE
001241         MOVE '20'                   TO  WS-ALPHA-YY-1
001242         MOVE BUILD-YEAR             TO  WS-ALPHA-YY-2
001243         MOVE BUILD-MONTH            TO  WS-ALPHA-MM
001244         MOVE BUILD-DAY              TO  WS-ALPHA-DD.
001245
001246     MOVE WS-ALPHA-DATE              TO  AI-DATE
001247                                         AI-CL-DATE.
001248
001249     MOVE EIBTIME                    TO  AI-TIME
001250                                         AI-CL-TIME.
001251
001252     MOVE CL-COMPANY-CD              TO  AI-CL-COMPANY-CD.
001253     MOVE 'C'                        TO  AI-CL-SOURCE.
001254     MOVE CL-CARRIER                 TO  AI-CL-CARRIER.
001255     MOVE CL-CLAIM-NO                TO  AI-CL-CLAIM-NUMBER.
001256     MOVE CL-CERT-NO                 TO  AI-CL-CERTIFICATE-NUMBER.
001257     MOVE CL-INCURRED-DT             TO  AI-CL-INCURRED-DATE.
001258     MOVE LOW-VALUES                 TO  AI-CL-CLOSE-DATE.
001259
001260     MOVE PI-PROCESSOR-ID            TO  AI-LAST-MAINT-BY.
001261     MOVE SAVE-BIN-DATE              TO  AI-LAST-MAINT-DT.
001262     MOVE EIBTIME                    TO  AI-LAST-MAINT-HHMMSS.
001263
001264     
      * EXEC CICS WRITE
001265*        DATASET   (ALPH-FILE-ID)
001266*        RIDFLD    (AI-CONTROL-PRIMARY)
001267*        FROM      (ALPHA-INDEX)
001268*    END-EXEC.
           MOVE LENGTH OF
            ALPHA-INDEX
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008007' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303038303037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALPH-FILE-ID, 
                 ALPHA-INDEX, 
                 DFHEIV11, 
                 AI-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001269
001270     MOVE LOW-VALUES                 TO  EL131AO.
001271     PERFORM 5000-BUILD-MAP THRU 5000-EXIT.
001272     MOVE ER-0000                    TO  EMI-ERROR.
001273     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001274     MOVE -1                         TO  MAINTL.
001275     GO TO 8100-SEND-MAP.
001276
001277     EJECT
001278 0600-CHANGE-NAME.
001279******************************************************************
001280*    MOVE NAME IN THE CLAIM RECORD TO THE ALPHA FILE AND REPLACE *
001281*    IT WITH THE NAME THAT IS KEYED ON THE SCREEN.               *
001282******************************************************************
001283
001284     IF MLNAMEL = +0 AND
001285        MFNAMEL = +0 AND
001286        MMINITL = +0
001287         MOVE ER-0276                TO  EMI-ERROR
001288         MOVE -1                     TO  MAINTL
001289         MOVE AL-UABON               TO  MAINTA
001290         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001291         GO TO 8110-SEND-DATA.
001292
001293     MOVE PI-COMPANY-CD              TO  COMPANY-CODE.
001294     MOVE PI-CARRIER                 TO  CARRIER-CODE.
001295     MOVE PI-CLAIM-NO                TO  CLAIM-NO.
001296     MOVE PI-CERT-NO                 TO  CERT-NO.
001297
001298     
      * EXEC CICS READ
001299*        DATASET   (CLMS-FILE-ID)
001300*        RIDFLD    (MSTR-KEY)
001301*        SET       (ADDRESS OF CLAIM-MASTER)
001302*    END-EXEC.
      *    MOVE '&"S        E          (   #00008041' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038303431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001303
001304     
      * EXEC CICS GETMAIN
001305*        SET       (ADDRESS OF ALPHA-INDEX)
001306*        LENGTH    (ALPH-LENGTH)
001307*        INITIMG   (GETMAIN-SPACE)
001308*    END-EXEC.
      *    MOVE ',"IL                  $   #00008047' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038303437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ALPH-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ALPHA-INDEX TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001309
001310     MOVE CL-COMPANY-CD          TO CERT-COMPANY-CODE
001311     MOVE CL-CERT-CARRIER        TO CERT-CARRIER
001312     MOVE CL-CERT-GROUPING       TO CERT-GROUP
001313     MOVE CL-CERT-STATE          TO CERT-STATE
001314     MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT
001315     MOVE CL-CERT-EFF-DT         TO CERT-DATE
001316     MOVE CL-CERT-NO             TO CERT-CERT
001317
001318     
      * EXEC CICS READ
001319*       DATASET   (CERT-FILE-ID)
001320*       RIDFLD    (CERT-KEY)
001321*       SET       (ADDRESS OF CERTIFICATE-MASTER)
001322*       RESP      (WS-RESPONSE)
001323*    END-EXEC
      *    MOVE '&"S        E          (  N#00008061' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303038303631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001324
001325     IF INSTYPEL > ZEROS
001326        MOVE INSTYPEI TO CL-INSURED-TYPE
001327     END-IF
001328
001329     IF MFNAMEL > +0
001330        IF CL-INSURED-TYPE = 'C'
001331          AND (CM-INSURED-FIRST-NAME = MFNAMEI(1:10))
001332           MOVE ER-1675             TO EMI-ERROR
001333           MOVE -1                  TO MFNAMEL
001334           MOVE AL-UABON            TO MFNAMEA
001335           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001336           GO TO 8110-SEND-DATA
001337        ELSE
001338          IF CL-INSURED-TYPE = 'P'
001339            AND (CM-INSURED-FIRST-NAME <> MFNAMEI(1:10))
001340             MOVE ER-1676             TO EMI-ERROR
001341             MOVE -1                  TO MFNAMEL
001342             MOVE AL-UABON            TO MFNAMEA
001343             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001344             GO TO 8110-SEND-DATA
001345          END-IF
001346        END-IF
001347     END-IF
001348
001349     MOVE 'AI'                       TO  AI-RECORD-ID.
001350     MOVE CL-COMPANY-CD              TO  AI-COMPANY-CD
001351                                         AI-CL-COMPANY-CD.
001352     MOVE 'C'                        TO  AI-SOURCE
001353                                         AI-CL-SOURCE.
001354
001355     MOVE CL-INSURED-LAST-NAME       TO  AI-LAST-NAME.
001356     MOVE CL-INSURED-1ST-NAME        TO  AI-FIRST-NAME.
001357     MOVE CL-INSURED-MID-INIT        TO  AI-MIDDLE-INIT.
001358
001359     MOVE EIBDATE                    TO  DC-JULIAN-YYDDD.
001360     MOVE '5'                        TO  DC-OPTION-CODE.
001361     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
001362     MOVE DC-GREG-DATE-1-MDY         TO  BUILD-VALID-DATE.
001363     IF BUILD-YEAR IS GREATER THAN 50
001364         MOVE '19'                   TO  WS-ALPHA-YY-1
001365         MOVE BUILD-YEAR             TO  WS-ALPHA-YY-2
001366         MOVE BUILD-MONTH            TO  WS-ALPHA-MM
001367         MOVE BUILD-DAY              TO  WS-ALPHA-DD
001368     ELSE
001369         MOVE '20'                   TO  WS-ALPHA-YY-1
001370         MOVE BUILD-YEAR             TO  WS-ALPHA-YY-2
001371         MOVE BUILD-MONTH            TO  WS-ALPHA-MM
001372         MOVE BUILD-DAY              TO  WS-ALPHA-DD.
001373
001374     MOVE WS-ALPHA-DATE              TO  AI-DATE
001375                                         AI-CL-DATE.
001376
001377     MOVE EIBTIME                    TO  AI-TIME
001378                                         AI-CL-TIME.
001379
001380     MOVE CL-COMPANY-CD              TO  AI-CL-COMPANY-CD.
001381     MOVE 'C'                        TO  AI-CL-SOURCE.
001382     MOVE CL-CARRIER                 TO  AI-CL-CARRIER.
001383     MOVE CL-CLAIM-NO                TO  AI-CL-CLAIM-NUMBER.
001384     MOVE CL-CERT-NO                 TO  AI-CL-CERTIFICATE-NUMBER.
001385     MOVE CL-INCURRED-DT             TO  AI-CL-INCURRED-DATE.
001386     MOVE LOW-VALUES                 TO  AI-CL-CLOSE-DATE.
001387
001388     MOVE PI-PROCESSOR-ID            TO  AI-LAST-MAINT-BY.
001389     MOVE SAVE-BIN-DATE              TO  AI-LAST-MAINT-DT.
001390     MOVE EIBTIME                    TO  AI-LAST-MAINT-HHMMSS.
001391
001392     IF PI-COMPANY-ID = 'CID' or 'AHL' or 'FNL'
001393         MOVE 'Y'                TO CL-YESNOSW
001394     END-IF.
001395
001396     
      * EXEC CICS WRITE
001397*        DATASET   (ALPH-FILE-ID)
001398*        RIDFLD    (AI-CONTROL-PRIMARY)
001399*        FROM      (ALPHA-INDEX)
001400*    END-EXEC.
           MOVE LENGTH OF
            ALPHA-INDEX
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008139' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303038313339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALPH-FILE-ID, 
                 ALPHA-INDEX, 
                 DFHEIV11, 
                 AI-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001401
001402     
      * EXEC CICS READ
001403*        DATASET   (CLMS-FILE-ID)
001404*        RIDFLD    (MSTR-KEY)
001405*        SET       (ADDRESS OF CLAIM-MASTER)
001406*        UPDATE
001407*    END-EXEC.
      *    MOVE '&"S        EU         (   #00008145' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303038313435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001408
001409     if (cl-priority-cd = '8')
001410        and (pi-processor-id not = 'PEMA'and 'JMS '
001411             AND 'AMWA' AND 'KMSB')
001412        MOVE ER-8003             TO EMI-ERROR
001413        PERFORM 9900-ERROR-FORMAT
001414                                 THRU 9900-EXIT
001415        MOVE -1                  TO MAINTL
001416        GO TO 8110-SEND-DATA
001417     end-if
001418
001419     IF MLNAMEL IS GREATER THAN +0
001420         MOVE MLNAMEI                TO  CL-INSURED-LAST-NAME.
001421
001422     IF MFNAMEL IS GREATER THAN +0
001423         MOVE MFNAMEI                TO  CL-INSURED-1ST-NAME.
001424
001425     IF MMINITL IS GREATER THAN +0
001426         MOVE MMINITI                TO  CL-INSURED-MID-INIT.
001427
001428     MOVE PI-PROCESSOR-ID            TO  CL-LAST-MAINT-USER.
001429     MOVE SAVE-BIN-DATE              TO  CL-LAST-MAINT-DT.
001430     MOVE EIBTIME                    TO  CL-LAST-MAINT-HHMMSS.
001431     MOVE '3'                        TO  CL-LAST-MAINT-TYPE.
001432
001433     IF PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL'
001434         MOVE 'Y'                TO CL-YESNOSW
001435     END-IF.
001436
001437     
      * EXEC CICS HANDLE CONDITION
001438*        DUPKEY   (0600-CONTINUE-NAME-UPDATE)
001439*    END-EXEC.
      *    MOVE '"$$                   ! $ #00008180' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303038313830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001440
001441     
      * EXEC CICS REWRITE
001442*        DATASET   (CLMS-FILE-ID)
001443*        FROM      (CLAIM-MASTER)
001444*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008184' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303038313834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001445
001446 0600-CONTINUE-NAME-UPDATE.
001447
001448     MOVE LOW-VALUES                 TO  EL131AO.
001449     PERFORM 5000-BUILD-MAP THRU 5000-EXIT.
001450     MOVE ER-0000                    TO  EMI-ERROR.
001451     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001452     MOVE -1                         TO  MAINTL.
001453     GO TO 8100-SEND-MAP.
001454
001455     EJECT
001456 1000-EDIT-SCREEN.
001457
001458     MOVE MAINTI                 TO CHECK-MAINT.
001459
001460     IF NOT VALID-OPTIONS
001461         MOVE 'X'                TO ERROR-SWITCH
001462         MOVE -1                 TO MAINTL
001463         MOVE AL-UABON           TO MAINTA
001464         MOVE ER-0023            TO EMI-ERROR
001465         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001466         GO TO 1010-EXIT.
001467
001468     IF NOT MODIFY-CAP
001469         MOVE 'UPDATE'           TO  SM-READ
001470         PERFORM 9995-SECURITY-VIOLATION
001471         MOVE ER-0070               TO  EMI-ERROR
001472         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001473         GO TO 8100-SEND-MAP.
001474
001475     IF DELETE-CLAIM
001476         MOVE AL-UANON TO MAINTA
001477         GO TO 1010-EXIT.
001478
001479     MOVE AL-UANON               TO MAINTA.
001480
001481     IF CCNOI GREATER THAN LOW-VALUES
001482         MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH.
001483
001484     IF PI-COMPANY-ID = 'CSL'
001485        CONTINUE
001486     ELSE
001487        IF TYPEL > ZERO
001488           IF ((PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL')
001489              AND (TYPEI = PI-AH-OVERRIDE-L1 OR
001490                    PI-LIFE-OVERRIDE-L1 OR 'O'))
001491                       OR
001492              ((PI-COMPANY-ID = 'DCC' OR 'VPP')
001493              AND (TYPEI = PI-AH-OVERRIDE-L1 OR
001494                PI-LIFE-OVERRIDE-L1 OR 'I' OR 'G' OR 'F' OR 'O'
001495                                    OR 'B' OR 'H' ))
001496              MOVE AL-UANON      TO TYPEA
001497              MOVE 'X'           TO UPDATE-SWITCH MSTR-SWITCH
001498           ELSE
001499              MOVE ER-0199       TO EMI-ERROR
001500              PERFORM 9900-ERROR-FORMAT
001501                                 THRU 9900-EXIT
001502              MOVE AL-UABON      TO TYPEA
001503              MOVE -1            TO TYPEL
001504              MOVE 'X'           TO ERROR-SWITCH
001505           END-IF
001506        ELSE
001507           MOVE AL-UANOF         TO TYPEA
001508        END-IF
001509     END-IF
001510
001511     IF STATUSI GREATER THAN LOW-VALUES
001512         IF STATUSI = 'OPEN' OR 'CLOSED' OR 'O' OR 'C'
001513             MOVE 'Y'            TO WS-OPEN-CLOSE-SW
001514             MOVE AL-UANON       TO STATUSA
001515             MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH TRLR-SWITCH
001516             MOVE 'Y'            TO DLYACTV-SW
001517         ELSE
001518             MOVE ER-0333        TO EMI-ERROR
001519             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001520             MOVE AL-UABON       TO STATUSA
001521             MOVE -1             TO STATUSL
001522             MOVE 'X'            TO ERROR-SWITCH
001523         END-IF
001524     ELSE
001525         MOVE SPACE              TO TRLR-SWITCH
001526         MOVE 'N'                TO DLYACTV-SW
001527         MOVE AL-UANOF           TO STATUSA.
001528
001529     IF PROCI GREATER THAN LOW-VALUES
001530         PERFORM 1120-EDIT-PROC THRU 1120-EXIT
001531     ELSE
001532         MOVE AL-UANOF TO PROCA.
001533
001534     IF SEXI GREATER THAN LOW-VALUES
001535         IF SEXI = 'F' OR 'M'
001536             MOVE AL-UANON       TO SEXA
001537             MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
001538           ELSE
001539             MOVE ER-0219        TO EMI-ERROR
001540             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001541             MOVE AL-UABON       TO SEXA
001542             MOVE -1             TO SEXL
001543             MOVE 'X'            TO ERROR-SWITCH
001544       ELSE
001545         MOVE AL-UANOF           TO SEXA.
001546
001547     IF BIRTHL = +0
001548         MOVE LOW-VALUES         TO  HOLD-BIRTH
001549         MOVE AL-UANOF           TO  BIRTHA
001550         GO TO 1000-CONTINUE-EDITS.
001551
001552     IF BIRTHI = SPACES
001553         MOVE LOW-VALUES         TO  HOLD-BIRTH
001554         MOVE 'X'                TO  UPDATE-SWITCH
001555                                     MSTR-SWITCH
001556         GO TO 1000-CONTINUE-EDITS.
001557
001558     MOVE BIRTHI                 TO  DEEDIT-DATE-INPUT.
001559     PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT.
001560     MOVE DEEDIT-DATE            TO  DC-GREG-DATE-1-MDY.
001561     MOVE '4'                    TO  DC-OPTION-CODE.
001562     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
001563     IF DATE-CONVERSION-ERROR
001564         MOVE ER-0220            TO  EMI-ERROR
001565         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001566         MOVE AL-UABON           TO  BIRTHA
001567         MOVE -1                 TO  BIRTHL
001568         MOVE 'X'                TO  ERROR-SWITCH
001569         GO TO 1000-CONTINUE-EDITS.
001570
001571******************************************************************
001572**   IF CALCULATED BIRTH DATE IS GREATER THAN TODAYS DATE       **
001573**   USE THE CENTURY ADJUSTMENT SWITCH IN THE DATE ROUTINE      **
001574**   TO SUBTRACT 100 YEARS TO OBTAIN THE CORRECT BIRTH DATE.    **
001575******************************************************************
001576     IF DC-BIN-DATE-1 IS GREATER THAN SAVE-BIN-DATE
001577         MOVE BIRTHI             TO  DEEDIT-DATE-INPUT
001578         PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT
001579         MOVE DEEDIT-DATE        TO  DC-GREG-DATE-1-MDY
001580         MOVE '4'                TO  DC-OPTION-CODE
001581         MOVE '1'                TO  DC-CENTURY-ADJUSTMENT
001582         MOVE +0                 TO  DC-ELAPSED-MONTHS
001583                                     DC-ELAPSED-DAYS
001584         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001585         IF DATE-CONVERSION-ERROR
001586             MOVE ER-0220        TO  EMI-ERROR
001587             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001588             MOVE AL-UABON       TO  BIRTHA
001589             MOVE -1             TO  BIRTHL
001590             MOVE 'X'            TO  ERROR-SWITCH
001591             GO TO 1000-CONTINUE-EDITS.
001592
001593     MOVE AL-UANON               TO  BIRTHA.
001594     MOVE 'X'                    TO  UPDATE-SWITCH
001595                                     MSTR-SWITCH.
001596     MOVE DC-GREG-DATE-1-EDIT    TO  BIRTHO.
001597     MOVE DC-BIN-DATE-1          TO  HOLD-BIRTH.
001598     MOVE ' '                    TO  DC-CENTURY-ADJUSTMENT.
001599
001600 1000-CONTINUE-EDITS.
001601
001602     IF SOCIALI GREATER THAN LOW-VALUES
001603         MOVE SOCIALI TO WS-SOC-SEC-NUMBER
001604         IF WS-SOC-SEC-NO NUMERIC AND
001605           (WS-SOC-SEC-BLANK = SPACES OR LOW-VALUES)
001606             NEXT SENTENCE
001607         ELSE
001608            IF WS-SSN-1-3 NUMERIC AND WS-SSN-4-5 NUMERIC AND
001609               WS-SSN-6-9 NUMERIC AND WS-SSN-DASH1 = '-' AND
001610               WS-SSN-DASH2 = '-'
001611                 NEXT SENTENCE
001612            ELSE
001613                 MOVE ER-0887        TO  EMI-ERROR
001614                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001615                 MOVE AL-UABON       TO  SOCIALA
001616                 MOVE -1             TO  SOCIALL
001617                 MOVE 'X'            TO  ERROR-SWITCH
001618            END-IF
001619        END-IF
001620     END-IF.
001621
001622     if critpl > zeros
001623        and critpi numeric
001624        move al-uanon            to critpa
001625        move 'X'                 to update-switch
001626     end-if
001627
001628     IF SOCIALI GREATER THAN LOW-VALUES
001629         MOVE AL-UANON           TO SOCIALA
001630         MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH MSTR-KEY-SWITCH
001631     ELSE
001632         MOVE AL-UANOF           TO SOCIALA.
001633
001634     IF OCCI GREATER THAN LOW-VALUES
001635         MOVE AL-UANON           TO OCCA
001636         MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH
001637     ELSE
001638         MOVE AL-UANOF           TO OCCA.
001639
001640     IF BENEL NOT GREATER THAN ZERO
001641            GO TO 1005-CONTINUE-EDITS.
001642
001643     IF BENEI = SPACES
001644        IF PI-AUTO-PAY-SEQ NOT = ZEROS
001645           MOVE 'N'             TO AUTO-PAY-TO-BENE
001646           PERFORM 2800-CHECK-AUTO-PAY THRU 2800-EXIT
001647           IF AUTO-PAY-TO-BENE = 'Y'
001648               MOVE ER-1773      TO  EMI-ERROR
001649               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001650               MOVE 'X'          TO ERROR-SWITCH
001651               MOVE AL-UABON     TO BENEA
001652               MOVE -1           TO BENEL
001653           ELSE
001654               MOVE 'X'          TO UPDATE-SWITCH MSTR-SWITCH
001655           END-IF
001656        ELSE
001657           PERFORM 1050-EDIT-BENE THRU 1050-EXIT
001658           IF PAYMENT-PENDING
001659               MOVE ER-1772      TO  EMI-ERROR
001660               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001661               MOVE 'X'          TO ERROR-SWITCH
001662               MOVE AL-UABON     TO BENEA
001663               MOVE -1           TO BENEL
001664           END-IF
001665           MOVE 'X'              TO UPDATE-SWITCH MSTR-SWITCH
001666        END-IF
001667        GO TO 1005-CONTINUE-EDITS.
001668
001669     
      * EXEC CICS HANDLE CONDITION
001670*        NOTFND (1004-BENE-NOT-FOUND)
001671*    END-EXEC.
      *    MOVE '"$I                   ! % #00008412' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303038343132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001672
001673     MOVE PI-COMPANY-CD          TO  WS-ELBENE-COMPANY-CD.
001674     MOVE 'B'                    TO  WS-ELBENE-RECORD-TYPE.
001675     MOVE BENEI                  TO  WS-ELBENE-ID.
001676
001677     
      * EXEC CICS READ
001678*        DATASET ('ELBENE')
001679*        RIDFLD  (WS-ELBENE-KEY)
001680*        SET     (ADDRESS OF BENEFICIARY-MASTER)
001681*    END-EXEC.
           MOVE 'ELBENE' TO DFHEIV1
      *    MOVE '&"S        E          (   #00008420' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303038343230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001682
001683     MOVE AL-UANON               TO  BENEA.
001684     MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH.
001685
001686     GO TO 1005-CONTINUE-EDITS.
001687
001688 1004-BENE-NOT-FOUND.
001689     MOVE ER-0565                TO  EMI-ERROR.
001690
001691     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001692     MOVE 'X'                    TO ERROR-SWITCH.
001693     MOVE AL-UABON               TO BENEA.
001694     MOVE -1                     TO BENEL.
001695
001696 1005-CONTINUE-EDITS.
001697
001698     IF DIAGI GREATER THAN LOW-VALUES
001699         MOVE AL-UANON           TO DIAGA
001700         MOVE 'X'                TO UPDATE-SWITCH
001701                                    NINETY-TRLR-SWITCH
001702     ELSE
001703         MOVE AL-UANOF           TO DIAGA.
001704
001705     IF ICD1I GREATER THAN LOW-VALUES
001706         IF (ICD1I GREATER THAN SPACES) AND
001707            (ICD1I(4:1) NOT = '.')
001708             IF ICD1I(4:1) = ' '
001709                 MOVE '.' TO ICD1I(4:1)
001710             ELSE
001711                 IF ICD1I(8:1) = ' '
001712                     MOVE ICD1I(7:1) TO ICD1I(8:1)
001713                     MOVE ICD1I(6:1) TO ICD1I(7:1)
001714                     MOVE ICD1I(5:1) TO ICD1I(6:1)
001715                     MOVE ICD1I(4:1) TO ICD1I(5:1)
001716                     MOVE '.'        TO ICD1I(4:1)
001717                 END-IF
001718             END-IF
001719         END-IF
001720         IF (ICD1I GREATER THAN SPACES) AND
001721            (ICD1I(1:1) NOT > ' ' OR
001722             ICD1I(2:1) NOT > ' ' OR
001723             ICD1I(3:1) NOT > ' ' OR
001724             ICD1I(4:1) NOT = '.')
001725              MOVE ER-0992        TO  EMI-ERROR
001726              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001727              MOVE 'X'            TO ERROR-SWITCH
001728              MOVE AL-UABON       TO ICD1A
001729              MOVE -1             TO ICD1L
001730         ELSE
001731             MOVE AL-UANON       TO ICD1A
001732             MOVE 'X'            TO UPDATE-SWITCH
001733                                    NINETY-TRLR-SWITCH
001734         END-IF
001735     ELSE
001736         MOVE AL-UANOF           TO ICD1A
001737     END-IF.
001738
001739     IF ICD2I GREATER THAN LOW-VALUES
001740         IF (ICD2I GREATER THAN SPACES) AND
001741            (ICD2I(4:1) NOT = '.')
001742             IF ICD2I(4:1) = ' '
001743                 MOVE '.' TO ICD2I(4:1)
001744             ELSE
001745                 IF ICD2I(8:1) = ' '
001746                     MOVE ICD2I(7:1) TO ICD2I(8:1)
001747                     MOVE ICD2I(6:1) TO ICD2I(7:1)
001748                     MOVE ICD2I(5:1) TO ICD2I(6:1)
001749                     MOVE ICD2I(4:1) TO ICD2I(5:1)
001750                     MOVE '.'        TO ICD2I(4:1)
001751                 END-IF
001752             END-IF
001753         END-IF
001754         IF (ICD2I GREATER THAN SPACES) AND
001755            (ICD2I(1:1) NOT > ' ' OR
001756             ICD2I(2:1) NOT > ' ' OR
001757             ICD2I(3:1) NOT > ' ' OR
001758             ICD2I(4:1) NOT = '.')
001759              MOVE ER-0992        TO EMI-ERROR
001760              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001761              MOVE 'X'            TO ERROR-SWITCH
001762              MOVE AL-UABON       TO ICD2A
001763              MOVE -1             TO ICD2L
001764         ELSE
001765             MOVE AL-UANON       TO ICD2A
001766             MOVE 'X'            TO UPDATE-SWITCH
001767                                    NINETY-TRLR-SWITCH
001768         END-IF
001769     ELSE
001770         MOVE AL-UANOF           TO ICD2A
001771     END-IF.
001772
001773*    IF CAUSEI GREATER THAN LOW-VALUES
001774*        MOVE AL-UANON           TO CAUSEA
001775*        MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH
001776*    ELSE
001777*        MOVE AL-UANOF           TO CAUSEA.
001778
001779*    IF ENDL GREATER THAN ZERO
001780*        MOVE LOW-VALUES         TO HOLD-END
001781*        IF ENDI = SPACES
001782*            MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
001783*        ELSE
001784*            MOVE ENDI           TO DEEDIT-DATE-INPUT
001785*            EXEC CICS BIF DEEDIT
001786*                FIELD    (DEEDIT-DATE-INPUT)
001787*                LENGTH   (DATE-LENGTH)
001788*            END-EXEC
001789*            MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
001790*            MOVE '4'            TO DC-OPTION-CODE
001791*            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001792*            IF NOT DATE-CONVERSION-ERROR
001793*                MOVE DC-BIN-DATE-1      TO HOLD-END
001794*                MOVE AL-UANON   TO ENDA
001795*                MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
001796*                MOVE DC-GREG-DATE-1-EDIT TO ENDO
001797*            ELSE
001798*                MOVE ER-0224    TO EMI-ERROR
001799*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001800*                MOVE AL-UABON   TO ENDA
001801*                MOVE -1         TO ENDL
001802*                MOVE 'X'        TO ERROR-SWITCH
001803*      ELSE
001804*          MOVE AL-UANOF         TO ENDA.
001805
001806     IF INCL GREATER THAN ZERO
001807         MOVE LOW-VALUES         TO HOLD-INCUR
001808         IF INCI = SPACES
001809             MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
001810         ELSE
001811             MOVE INCI           TO DEEDIT-DATE-INPUT
001812             
      * EXEC CICS BIF DEEDIT
001813*                FIELD    (DEEDIT-DATE-INPUT)
001814*                LENGTH   (DATE-LENGTH)
001815*            END-EXEC
      *    MOVE '@"L                   #   #00008555' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038353535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-DATE-INPUT, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001816             MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
001817             MOVE '4'            TO DC-OPTION-CODE
001818             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001819             IF NOT DATE-CONVERSION-ERROR
001820                 MOVE DC-BIN-DATE-1      TO HOLD-INCUR
001821                 MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
001822                 MOVE AL-UANON           TO INCA
001823                 MOVE DC-GREG-DATE-1-EDIT TO INCO
001824             ELSE
001825                 MOVE ER-0222    TO EMI-ERROR
001826                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001827                 MOVE AL-UABON   TO INCA
001828                 MOVE -1         TO INCL
001829                 MOVE 'X'        TO ERROR-SWITCH
001830             end-if
001831         end-if
001832     ELSE
001833        IF PI-NO-PMTS = ZEROS
001834           MOVE AL-UANOF     TO INCA
001835        end-if
001836     end-if
001837
001838     IF REPL GREATER THAN ZERO
001839         MOVE LOW-VALUES         TO HOLD-REPORTED
001840         IF REPI = SPACES
001841             MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
001842         ELSE
001843             MOVE REPI           TO DEEDIT-DATE-INPUT
001844             
      * EXEC CICS BIF DEEDIT
001845*                FIELD    (DEEDIT-DATE-INPUT)
001846*                LENGTH   (DATE-LENGTH)
001847*            END-EXEC
      *    MOVE '@"L                   #   #00008587' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038353837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-DATE-INPUT, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001848             MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
001849             MOVE '4'            TO DC-OPTION-CODE
001850             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001851             IF NOT DATE-CONVERSION-ERROR
001852                 MOVE DC-BIN-DATE-1      TO HOLD-REPORTED
001853                 MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
001854                 MOVE AL-UANON   TO REPA
001855                 MOVE DC-GREG-DATE-1-EDIT TO REPO
001856             ELSE
001857                 MOVE ER-0223    TO EMI-ERROR
001858                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001859                 MOVE AL-UABON   TO REPA
001860                 MOVE -1         TO REPL
001861                 MOVE 'X'        TO ERROR-SWITCH
001862       ELSE
001863         MOVE AL-UANOF TO REPA.
001864
001865     IF PDTHRUI GREATER THAN LOW-VALUES
001866        MOVE LOW-VALUES         TO HOLD-PDTHRU
001867        IF PDTHRUI = SPACES
001868           MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
001869        ELSE
001870           MOVE PDTHRUI        TO DEEDIT-DATE-INPUT
001871           
      * EXEC CICS BIF DEEDIT
001872*               FIELD    (DEEDIT-DATE-INPUT)
001873*               LENGTH   (DATE-LENGTH)
001874*          END-EXEC
      *    MOVE '@"L                   #   #00008614' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038363134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-DATE-INPUT, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001875           MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
001876           MOVE '4'            TO DC-OPTION-CODE
001877           PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001878           IF NOT DATE-CONVERSION-ERROR
001879              MOVE DC-BIN-DATE-1      TO HOLD-PDTHRU
001880              MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
001881              MOVE AL-UANON   TO PDTHRUA
001882              MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO
001883              IF PI-USES-PAID-TO
001884                 MOVE '6'            TO DC-OPTION-CODE
001885                 MOVE -1             TO DC-ELAPSED-DAYS
001886                 MOVE +0             TO DC-ELAPSED-MONTHS
001887                 PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
001888                 IF NO-CONVERSION-ERROR
001889                    MOVE DC-BIN-DATE-2 TO HOLD-PDTHRU
001890                 ELSE
001891                    NEXT SENTENCE
001892              ELSE
001893                 NEXT SENTENCE
001894           ELSE
001895              MOVE ER-0475    TO EMI-ERROR
001896              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001897              MOVE AL-UABON   TO PDTHRUA
001898              MOVE -1         TO PDTHRUL
001899              MOVE 'X'        TO ERROR-SWITCH
001900     ELSE
001901        IF MODIFY-CAP
001902           MOVE AL-UANOF        TO PDTHRUA.
001903
001904     IF PDAMTL GREATER THAN +0
001905         MOVE ZEROS              TO HOLD-PDAMT
001906         
      * EXEC CICS BIF DEEDIT
001907*            FIELD    (PDAMTI)
001908*            LENGTH   (9)
001909*        END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008649' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038363439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PDAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001910         IF PDAMTI NUMERIC
001911             MOVE PDAMTI         TO HOLD-PDAMT PDAMTO
001912             MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
001913             MOVE AL-UNNON       TO PDAMTA
001914         ELSE
001915             MOVE ER-0547        TO EMI-ERROR
001916             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001917             MOVE AL-UNBON       TO PDAMTA
001918             MOVE -1             TO PDAMTL
001919             MOVE 'X'            TO ERROR-SWITCH
001920     ELSE
001921         IF MODIFY-CAP
001922            MOVE AL-UNNOF        TO PDAMTA.
001923
001924     IF NODAYSI GREATER THAN LOW-VALUES
001925         MOVE ZEROS              TO HOLD-NODAYS
001926         
      * EXEC CICS BIF DEEDIT
001927*            FIELD (NODAYSI)
001928*            LENGTH (5)
001929*        END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008669' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038363639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NODAYSI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001930         IF NODAYSI NUMERIC
001931             MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
001932             MOVE AL-UNNON       TO NODAYSA
001933             MOVE NODAYSI        TO  HOLD-NODAYS  NODAYSO
001934         ELSE
001935             MOVE ER-0491        TO EMI-ERROR
001936             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001937             MOVE AL-UNBON       TO NODAYSA
001938             MOVE -1             TO NODAYSL
001939             MOVE 'X'            TO ERROR-SWITCH
001940     ELSE
001941         IF MODIFY-CAP
001942            MOVE AL-UNNOF        TO NODAYSA.
001943
001944     IF NOPMTSI GREATER THAN LOW-VALUES
001945         MOVE ZEROS              TO HOLD-NOPMTS
001946         
      * EXEC CICS BIF DEEDIT
001947*            FIELD (NOPMTSI)
001948*            LENGTH (4)
001949*        END-EXEC
           MOVE 4
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008689' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038363839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NOPMTSI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001950         IF NOPMTSI NUMERIC
001951             MOVE NOPMTSI        TO HOLD-NOPMTS  NOPMTSO
001952             MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
001953             MOVE AL-UNNON       TO NOPMTSA
001954           ELSE
001955             MOVE ER-0548        TO EMI-ERROR
001956             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001957             MOVE AL-UNBON       TO NOPMTSA
001958             MOVE -1             TO NOPMTSL
001959             MOVE 'X'            TO ERROR-SWITCH
001960       ELSE
001961         IF MODIFY-CAP
001962            MOVE AL-UNNOF        TO NOPMTSA.
001963
001964     IF FORMTYPI GREATER THAN LOW-VALUES
001965         IF FORMTYPI = ' ' OR 'L' OR 'S'
001966             MOVE AL-UANON       TO FORMTYPA
001967             MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
001968           ELSE
001969             MOVE ER-7650        TO EMI-ERROR
001970             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001971             MOVE AL-UABON       TO FORMTYPA
001972             MOVE -1             TO FORMTYPL
001973             MOVE 'X'            TO ERROR-SWITCH
001974       ELSE
001975         MOVE AL-UANOF           TO FORMTYPA.
001976
001977     IF PRICDL GREATER THAN ZERO
001978         IF (PRICDI = ' ') OR
001979            (PRICDI GREATER THAN ZERO   AND
001980             PRICDI NOT GREATER THAN '9')
001981             MOVE AL-UANON       TO PRICDA
001982             MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
001983         ELSE
001984             MOVE ER-0274        TO EMI-ERROR
001985             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001986             MOVE AL-UABON       TO PRICDA
001987             MOVE -1             TO PRICDL
001988             MOVE 'X'            TO ERROR-SWITCH
001989     ELSE
001990         MOVE AL-UANOF TO PRICDA.
001991
001992     IF PI-COMPANY-ID = 'DMD'
001993     IF PRICDL GREATER THAN ZERO
001994       IF SYSTEM-MODIFY-CAP  OR
001995          PI-PROCESSOR-ID = 'LGXX'
001996             NEXT SENTENCE
001997           ELSE
001998            IF CL-PRIORITY-CD = '9'  OR
001999               PRICDI         = '9'
002000                  MOVE ER-0938        TO EMI-ERROR
002001                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002002                  MOVE AL-UABON       TO PRICDA
002003                  MOVE -1             TO PRICDL
002004                  MOVE 'X'            TO ERROR-SWITCH.
002005
002006     IF FILETOL GREATER THAN ZERO
002007         MOVE AL-UANON           TO FILETOA
002008         MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH
002009     ELSE
002010         MOVE AL-UANOF TO FILETOA.
002011
002012     IF SUPVL GREATER THAN ZERO
002013         IF SUPVI = 'Y' OR 'N' OR SPACE
002014             MOVE AL-UANON       TO SUPVA
002015             MOVE 'X'            TO UPDATE-SWITCH MSTR-SWITCH
002016         ELSE
002017             MOVE ER-0230        TO EMI-ERROR
002018             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002019             MOVE AL-UABON       TO SUPVA
002020             MOVE -1             TO SUPVL
002021             MOVE 'X'            TO ERROR-SWITCH
002022     ELSE
002023         MOVE AL-UANOF           TO SUPVA.
002024
002025     IF MLNAMEL GREATER THAN ZERO
002026         IF MLNAMEI GREATER THAN SPACES
002027            MOVE AL-UANON        TO MLNAMEA
002028            MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH
002029                                     MSTR-KEY-SWITCH
002030         ELSE
002031            MOVE ER-0236         TO EMI-ERROR
002032            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002033            MOVE AL-UABON        TO MLNAMEA
002034            MOVE -1              TO MLNAMEL
002035            MOVE 'X'             TO ERROR-SWITCH
002036     ELSE
002037         MOVE AL-UANOF           TO MLNAMEA.
002038
002039     IF MFNAMEI GREATER THAN LOW-VALUES
002040         MOVE AL-UANON           TO MFNAMEA
002041         MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH
002042     ELSE
002043         MOVE AL-UANOF           TO MFNAMEA.
002044
002045     IF MMINITI GREATER THAN LOW-VALUES
002046         MOVE AL-UANON           TO MMINITA
002047         MOVE 'X'                TO UPDATE-SWITCH MSTR-SWITCH
002048     ELSE
002049         MOVE AL-UANOF           TO MMINITA.
002050
002051     IF CRTLNMEI GREATER THAN LOW-VALUES
002052         IF CRTLNMEI NOT = SPACES
002053            MOVE AL-UANON        TO CRTLNMEA
002054            MOVE 'X'             TO  UPDATE-SWITCH CERT-SWITCH
002055                                     CERT-KEY-SWITCH
002056        ELSE
002057            MOVE ER-0236         TO EMI-ERROR
002058            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002059            MOVE AL-UABON        TO CRTLNMEA
002060            MOVE -1              TO CRTLNMEL
002061            MOVE 'X'             TO ERROR-SWITCH
002062     ELSE
002063         MOVE AL-UANOF           TO CRTLNMEA.
002064
002065     IF CRTFNMEI GREATER THAN LOW-VALUES
002066         MOVE AL-UANON           TO CRTFNMEA
002067         MOVE 'X'                TO UPDATE-SWITCH CERT-SWITCH
002068     ELSE
002069         MOVE AL-UANOF           TO CRTFNMEA.
002070
002071     IF CRTINITI GREATER THAN LOW-VALUES
002072         MOVE AL-UANON           TO CRTINITA
002073         MOVE 'X' TO UPDATE-SWITCH CERT-SWITCH CERT-KEY-SWITCH
002074     ELSE
002075         MOVE AL-UANOF           TO CRTINITA.
002076
002077     IF ISSAGEL GREATER THAN ZERO
002078         IF ISSAGEI NUMERIC
002079            MOVE AL-UANON        TO ISSAGEA
002080            MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
002081         ELSE
002082            MOVE ER-0237         TO EMI-ERROR
002083            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002084            MOVE AL-UABON        TO ISSAGEA
002085            MOVE -1              TO ISSAGEL
002086            MOVE 'X'             TO ERROR-SWITCH
002087     ELSE
002088         MOVE AL-UANOF           TO ISSAGEA.
002089
002090     IF JNTFNMEI GREATER THAN LOW-VALUES
002091         MOVE AL-UANON           TO JNTFNMEA
002092         MOVE 'X'                TO UPDATE-SWITCH CERT-SWITCH
002093     ELSE
002094         MOVE AL-UANOF           TO JNTFNMEA.
002095
002096     IF JNTLNMEI GREATER THAN LOW-VALUES
002097         MOVE AL-UANON           TO JNTLNMEA
002098         MOVE 'X'                TO UPDATE-SWITCH CERT-SWITCH
002099     ELSE
002100         MOVE AL-UANOF           TO JNTLNMEA.
002101
002102     IF JNTINITI GREATER THAN LOW-VALUES
002103         MOVE AL-UANON           TO JNTINITA
002104         MOVE 'X'                TO UPDATE-SWITCH CERT-SWITCH
002105     ELSE
002106         MOVE AL-UANOF           TO JNTINITA.
002107
002108     IF JNTAGEL GREATER THAN ZERO
002109         IF JNTAGEI NUMERIC
002110            MOVE AL-UANON        TO JNTAGEA
002111            MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
002112         ELSE
002113            MOVE ER-0238         TO EMI-ERROR
002114            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002115            MOVE AL-UABON        TO JNTAGEA
002116            MOVE -1              TO JNTAGEL
002117            MOVE 'X'             TO ERROR-SWITCH
002118     ELSE
002119         MOVE AL-UANOF           TO JNTAGEA.
002120
002121     IF ADDONDTI GREATER THAN LOW-VALUES
002122         MOVE LOW-VALUES         TO HOLD-ADDON
002123         IF ADDONDTI = SPACES
002124             MOVE 'X'  TO UPDATE-SWITCH MSTR-SWITCH CERT-SWITCH
002125         ELSE
002126             MOVE ADDONDTI       TO DEEDIT-DATE-INPUT
002127             
      * EXEC CICS BIF DEEDIT
002128*                FIELD    (DEEDIT-DATE-INPUT)
002129*                LENGTH   (DATE-LENGTH)
002130*            END-EXEC
      *    MOVE '@"L                   #   #00008870' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038383730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-DATE-INPUT, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002131             MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
002132             MOVE '4'            TO DC-OPTION-CODE
002133             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
002134             IF NOT DATE-CONVERSION-ERROR
002135                 MOVE DC-BIN-DATE-1      TO HOLD-ADDON
002136                 MOVE 'X'        TO UPDATE-SWITCH MSTR-SWITCH
002137                                                  CERT-SWITCH
002138                 MOVE AL-UANON   TO ADDONDTA
002139                 MOVE DC-GREG-DATE-1-EDIT TO ADDONDTO
002140             ELSE
002141                 MOVE ER-7651    TO EMI-ERROR
002142                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002143                 MOVE AL-UABON   TO ADDONDTA
002144                 MOVE -1         TO ADDONDTL
002145                 MOVE 'X'        TO ERROR-SWITCH
002146       ELSE
002147         MOVE AL-UANOF TO ADDONDTA.
002148
002149     IF LCVCDI GREATER THAN LOW-VALUES
002150         MOVE LCVCDI             TO WS-EDIT-BEN-CODE
002151         PERFORM 1240-EDIT-LIFE-CODE THRU 1240-EXIT
002152     ELSE
002153         MOVE AL-UANOF           TO LCVCDA.
002154
002155     IF ACVCDI GREATER THAN LOW-VALUES
002156         MOVE ACVCDI             TO WS-EDIT-BEN-CODE
002157         PERFORM 1310-EDIT-AH-CODE THRU 1310-EXIT
002158     ELSE
002159         MOVE AL-UANOF           TO ACVCDA.
002160
002161     IF LCVOTRMI GREATER THAN LOW-VALUES
002162         
      * EXEC CICS BIF DEEDIT
002163*            FIELD (LCVOTRMI)
002164*            LENGTH(3)
002165*        END-EXEC
           MOVE 3
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008905' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038393035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCVOTRMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002166         IF LCVOTRMI IS NUMERIC
002167             MOVE LCVOTRMI       TO LCVOTRMO
002168             MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
002169             MOVE AL-UANON       TO LCVOTRMA
002170         ELSE
002171             MOVE ER-0241        TO EMI-ERROR
002172             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002173             MOVE AL-UNBON       TO LCVOTRMA
002174             MOVE 'X'            TO ERROR-SWITCH
002175             MOVE -1             TO LCVOTRML
002176     ELSE
002177         MOVE AL-UANOF           TO LCVOTRMA.
002178
002179     IF ACVOTRMI GREATER THAN LOW-VALUES
002180         
      * EXEC CICS BIF DEEDIT
002181*            FIELD (ACVOTRMI)
002182*            LENGTH(3)
002183*        END-EXEC
           MOVE 3
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008923' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038393233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACVOTRMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002184         IF ACVOTRMI IS NUMERIC
002185             MOVE ACVOTRMI       TO ACVOTRMO
002186             MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
002187             MOVE AL-UANON       TO ACVOTRMA
002188         ELSE
002189             MOVE ER-0241        TO EMI-ERROR
002190             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002191             MOVE AL-UNBON       TO ACVOTRMA
002192             MOVE 'X'            TO ERROR-SWITCH
002193             MOVE -1             TO ACVOTRML
002194     ELSE
002195         MOVE AL-UANOF           TO ACVOTRMA.
002196
002197     IF LCVRATEL GREATER THAN +0
002198         
      * EXEC CICS BIF DEEDIT
002199*            FIELD (LCVRATEI)
002200*            LENGTH(6)
002201*        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008941' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038393431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCVRATEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002202         IF LCVRATEI IS NUMERIC
002203             MOVE LCVRATEI       TO HOLD-LF-RATE  LCVRATEO
002204             MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
002205             MOVE AL-UNNON       TO LCVRATEA
002206         ELSE
002207             MOVE ER-2280        TO EMI-ERROR
002208             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002209             MOVE AL-UNBON       TO LCVRATEA
002210             MOVE 'X'            TO ERROR-SWITCH
002211             MOVE -1             TO LCVRATEL
002212     ELSE
002213         MOVE AL-UNNOF           TO LCVRATEA.
002214
002215     IF ACVRATEL GREATER THAN +0
002216         
      * EXEC CICS BIF DEEDIT
002217*            FIELD (ACVRATEI)
002218*            LENGTH(6)
002219*        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008959' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038393539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACVRATEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002220         IF ACVRATEI IS NUMERIC
002221             MOVE ACVRATEI       TO HOLD-AH-RATE  ACVRATEO
002222             MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
002223             MOVE AL-UNNON       TO ACVRATEA
002224         ELSE
002225             MOVE ER-2280        TO EMI-ERROR
002226             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002227             MOVE AL-UNBON       TO ACVRATEA
002228             MOVE 'X'            TO ERROR-SWITCH
002229             MOVE -1             TO ACVRATEL
002230     ELSE
002231         MOVE AL-UNNOF           TO ACVRATEA.
002232
002233     IF LCVBENEL GREATER THAN ZERO
002234         
      * EXEC CICS BIF DEEDIT
002235*            FIELD (LCVBENEI)
002236*            LENGTH (11)
002237*        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008977' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038393737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCVBENEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002238         IF LCVBENEI IS NUMERIC
002239            MOVE LCVBENEI        TO HOLD-LF-CV-BEN  LCVBENEO
002240            MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
002241            MOVE AL-UNNON        TO LCVBENEA
002242         ELSE
002243            MOVE ER-0243         TO EMI-ERROR
002244            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002245            MOVE AL-UNBON        TO LCVBENEA
002246            MOVE -1              TO LCVBENEL
002247            MOVE 'X'             TO ERROR-SWITCH
002248     ELSE
002249         MOVE AL-UANOF           TO LCVBENEA.
002250
002251     IF ACVBENEL GREATER THAN ZERO
002252         
      * EXEC CICS BIF DEEDIT
002253*            FIELD (ACVBENEI)
002254*            LENGTH (11)
002255*        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00008995' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038393935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACVBENEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002256         IF ACVBENEI IS NUMERIC
002257            MOVE ACVBENEI        TO HOLD-AH-CV-BEN  ACVBENEO
002258            MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
002259            MOVE AL-UNNON        TO ACVBENEA
002260         ELSE
002261            MOVE ER-0243         TO EMI-ERROR
002262            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002263            MOVE AL-UNBON        TO ACVBENEA
002264            MOVE -1              TO ACVBENEL
002265            MOVE 'X'             TO ERROR-SWITCH
002266     ELSE
002267         MOVE AL-UANOF           TO ACVBENEA.
002268
002269     IF LCVFORMI GREATER THAN LOW-VALUES
002270        MOVE 'X'                 TO UPDATE-SWITCH CERT-SWITCH
002271        MOVE AL-UANON            TO LCVFORMA
002272     ELSE
002273        MOVE AL-UANOF            TO LCVFORMA.
002274
002275     IF ACVFORMI GREATER THAN LOW-VALUES
002276        MOVE 'X'                 TO UPDATE-SWITCH CERT-SWITCH
002277        MOVE AL-UANON            TO ACVFORMA
002278     ELSE
002279        MOVE AL-UANOF            TO ACVFORMA.
002280
002281     IF LCVCNDTL GREATER THAN ZERO
002282         MOVE LOW-VALUES         TO HOLD-LF-CV-CAN
002283         IF LCVCNDTI = SPACES
002284             MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
002285         ELSE
002286             MOVE LCVCNDTI       TO DEEDIT-DATE-INPUT
002287             PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT
002288             MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
002289             MOVE '4'            TO DC-OPTION-CODE
002290             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
002291             IF NOT DATE-CONVERSION-ERROR
002292                 MOVE DC-BIN-DATE-1      TO HOLD-LF-CV-CAN
002293                 MOVE AL-UANON   TO LCVCNDTA
002294                 MOVE 'X'        TO UPDATE-SWITCH CERT-SWITCH
002295                 MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO
002296             ELSE
002297                 MOVE ER-0246    TO EMI-ERROR
002298                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002299                 MOVE AL-UABON   TO LCVCNDTA
002300                 MOVE -1         TO LCVCNDTL
002301                 MOVE 'X'        TO ERROR-SWITCH
002302     ELSE
002303         MOVE LOW-VALUES         TO HOLD-LF-CV-CAN
002304         MOVE AL-UANOF           TO LCVCNDTA.
002305
002306     IF ACVCNDTL GREATER THAN ZERO
002307         MOVE LOW-VALUES         TO HOLD-AH-CV-CAN
002308         IF ACVCNDTI = SPACES
002309             MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
002310         ELSE
002311             MOVE ACVCNDTI       TO DEEDIT-DATE-INPUT
002312             PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT
002313             MOVE DEEDIT-DATE    TO DC-GREG-DATE-1-MDY
002314             MOVE '4'            TO DC-OPTION-CODE
002315             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
002316             IF NOT DATE-CONVERSION-ERROR
002317                 MOVE DC-BIN-DATE-1      TO HOLD-AH-CV-CAN
002318                 MOVE AL-UANON   TO ACVCNDTA
002319                 MOVE 'X'        TO UPDATE-SWITCH CERT-SWITCH
002320                 MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO
002321             ELSE
002322                 MOVE ER-0246    TO EMI-ERROR
002323                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002324                 MOVE AL-UABON   TO ACVCNDTA
002325                 MOVE -1         TO ACVCNDTL
002326                 MOVE 'X'        TO ERROR-SWITCH
002327     ELSE
002328         MOVE LOW-VALUES         TO HOLD-AH-CV-CAN
002329         MOVE AL-UANOF           TO ACVCNDTA.
002330
002331     IF APRL GREATER THAN +0
002332         
      * EXEC CICS BIF DEEDIT
002333*            FIELD    (APRI)
002334*            LENGTH   (8)
002335*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009075' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303039303735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APRI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002336         IF APRI NUMERIC
002337            MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
002338            MOVE APRI            TO HOLD-APR APRO
002339            MOVE AL-UANON        TO APRA
002340          ELSE
002341            MOVE ER-0259         TO EMI-ERROR
002342            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002343            MOVE AL-UABON        TO APRA
002344            MOVE -1              TO APRL
002345            MOVE 'X'             TO ERROR-SWITCH
002346     ELSE
002347         MOVE AL-UANOF           TO APRA.
002348
002349     IF PMTFREQL GREATER THAN ZERO
002350         
      * EXEC CICS BIF DEEDIT
002351*            FIELD    (PMTFREQI)
002352*            LENGTH   (2)
002353*        END-EXEC
           MOVE 2
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009093' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303039303933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PMTFREQI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002354         IF PMTFREQI NUMERIC
002355            MOVE PMTFREQI        TO  HOLD-FREQ PMTFREQO
002356            MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
002357            MOVE AL-UANON        TO PMTFREQA
002358          ELSE
002359            MOVE ER-0427         TO EMI-ERROR
002360            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002361            MOVE AL-UABON        TO PMTFREQA
002362            MOVE -1              TO PMTFREQL
002363            MOVE 'X'             TO ERROR-SWITCH
002364     ELSE
002365         MOVE AL-UANOF           TO PMTFREQA.
002366
002367     IF INDGRPL GREATER THAN ZERO
002368         IF INDGRPI = 'I' OR 'G' OR SPACE
002369             MOVE 'X'            TO UPDATE-SWITCH CERT-SWITCH
002370             MOVE AL-UANON       TO INDGRPA
002371          ELSE
002372             MOVE ER-0260        TO EMI-ERROR
002373             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002374             MOVE AL-UABON       TO INDGRPA
002375             MOVE -1             TO INDGRPL
002376             MOVE 'X'            TO ERROR-SWITCH
002377     ELSE
002378         MOVE AL-UANOF           TO INDGRPA.
002379
002380     IF PREMTYPL GREATER THAN ZERO
002381         IF PREMTYPI = '1' OR '2' OR '3'
002382            MOVE PREMTYPI        TO PI-PREM-TYPE
002383            MOVE AL-UNNON        TO PREMTYPA
002384            MOVE 'X'             TO UPDATE-SWITCH CERT-SWITCH
002385          ELSE
002386            MOVE ER-0227         TO EMI-ERROR
002387            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002388            MOVE AL-UNBON        TO PREMTYPA
002389            MOVE -1              TO PREMTYPL
002390            MOVE 'X'             TO ERROR-SWITCH
002391     ELSE
002392         MOVE AL-UANOF           TO PREMTYPA.
002393
002394     IF LOANBALL GREATER THAN ZERO
002395         
      * EXEC CICS BIF DEEDIT
002396*             FIELD    (LOANBALI)
002397*             LENGTH   (9)
002398*        END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009138' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303039313338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOANBALI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002399         IF LOANBALI NUMERIC
002400             MOVE LOANBALI       TO  HOLD-LOANBAL LOANBALO
002401             MOVE 'X' TO UPDATE-SWITCH CERT-SWITCH MSTR-SWITCH
002402             MOVE AL-UNNON TO LOANBALA
002403         ELSE
002404             MOVE ER-0639        TO EMI-ERROR
002405             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002406             MOVE AL-UNBON       TO LOANBALA
002407             MOVE -1             TO LOANBALL
002408             MOVE 'X'            TO ERROR-SWITCH
002409     ELSE
002410         MOVE AL-UNNOF           TO LOANBALA.
002411
002412*    IF LOANNOL GREATER THAN ZERO
002413*          MOVE 'X'                 TO UPDATE-SWITCH CERT-SWITCH
002414*          MOVE AL-UANON            TO LOANNOA
002415*       ELSE
002416*          MOVE AL-UANOF            TO LOANNOA.
002417
002418     IF REINCDI GREATER THAN LOW-VALUES
002419        MOVE 'X'                 TO UPDATE-SWITCH CERT-SWITCH
002420        MOVE AL-UANON            TO REINCDA
002421     ELSE
002422        MOVE AL-UANOF            TO REINCDA.
002423
002424     IF CERTI GREATER THAN LOW-VALUES
002425         PERFORM 1460-EDIT-CERT-NO THRU 1460-EXIT
002426     ELSE
002427         MOVE AL-UANOF           TO CERTA.
002428
002429     IF CERTEFFI GREATER THAN LOW-VALUES
002430         PERFORM 1470-EDIT-EFF THRU 1470-EXIT
002431     ELSE
002432         MOVE AL-UANOF           TO CERTEFFA.
002433
002434     IF CERTACTI GREATER THAN LOW-VALUES
002435         PERFORM 1480-EDIT-ACCT THRU 1480-EXIT
002436     ELSE
002437         MOVE AL-UANOF           TO CERTACTA.
002438
002439     IF CERTSTI GREATER THAN LOW-VALUES
002440         PERFORM 1490-EDIT-STATE THRU 1490-EXIT
002441     ELSE
002442         MOVE AL-UANOF           TO CERTSTA.
002443
002444     IF CERTCARI GREATER THAN LOW-VALUES
002445         PERFORM 1500-EDIT-CARR THRU 1500-EXIT
002446     ELSE
002447         MOVE AL-UANOF           TO CERTCARA.
002448
002449     IF DLYACTV-RECORD-NEEDED
002450         PERFORM 1150-OUTPUT-ACTIVITY-RECORD THRU
002451                 1150-EXIT
002452     END-IF.
002453
002454     IF CERTEFFI GREATER LOW-VALUES OR
002455        CERTACTI GREATER LOW-VALUES OR
002456        CERTSTI  GREATER LOW-VALUES OR
002457        CERTCARI GREATER LOW-VALUES OR
002458        CERTGRPI GREATER LOW-VALUES
002459        PERFORM 1620-VERIFY-ACCT THRU 1620-EXIT.
002460
002461*    IF PI-COMPANY-ID = 'CRI' OR 'PEM' OR 'NCL'
002462*      IF PI-PREM-TYPE NOT = '1'  AND
002463*         PI-NO-PMTS = ZEROS
002464*          IF LOANBALL GREATER +0 OR
002465*             ACVBENEL GREATER +0
002466*               PERFORM 1620-VERIFY-ACCT THRU 1620-EXIT.
002467
002468     if not acct-found
002469        perform 1620-VERIFY-ACCT THRU 1620-EXIT
002470     end-if
002471
002472
002473     MOVE PI-COMPANY-CD          TO COMPANY-CODE
002474     MOVE PI-CARRIER             TO CARRIER-CODE
002475     MOVE PI-CLAIM-NO            TO CLAIM-NO
002476     MOVE PI-CERT-NO             TO CERT-NO
002477
002478     
      * EXEC CICS READ
002479*        DATASET   (CLMS-FILE-ID)
002480*        RIDFLD    (MSTR-KEY)
002481*        SET       (ADDRESS OF CLAIM-MASTER)
002482*        RESP      (WS-RESPONSE)
002483*    END-EXEC
      *    MOVE '&"S        E          (  N#00009221' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039323231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002484
002485     MOVE CL-COMPANY-CD          TO CERT-COMPANY-CODE
002486     MOVE CL-CERT-CARRIER        TO CERT-CARRIER
002487     MOVE CL-CERT-GROUPING       TO CERT-GROUP
002488     MOVE CL-CERT-STATE          TO CERT-STATE
002489     MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT
002490     MOVE CL-CERT-EFF-DT         TO CERT-DATE
002491     MOVE CL-CERT-NO             TO CERT-CERT
002492
002493     
      * EXEC CICS READ
002494*       DATASET   (CERT-FILE-ID)
002495*       RIDFLD    (CERT-KEY)
002496*       SET       (ADDRESS OF CERTIFICATE-MASTER)
002497*       RESP      (WS-RESPONSE)
002498*    END-EXEC
      *    MOVE '&"S        E          (  N#00009236' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039323336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002499
002500     if mfnamei > low-values
002501        move mfnamei to w-name-first
002502     else
002503        move cl-insured-1st-name to w-name-first
002504     end-if
002505
002506     if mlnamei > low-values
002507        move mlnamei to w-name-last
002508     else
002509        move cl-insured-last-name to w-name-last
002510     end-if
002511
002512     if instypel > zeros
002513        evaluate true
002514           when cl-no-of-days-paid > zeros
002515              move er-1668       to emi-error
002516              perform 9900-error-format
002517                                 thru 9900-exit
002518              move al-uabon      to instypea
002519              move -1            to instypel
002520              move 'X'           to error-switch
002521           when instypei = 'C'
002522              if (w-name-first (1:10) = cm-insured-first-name)
002523                 and (w-name-last = cm-insured-last-name)
002524                 move er-1666    to emi-error
002525                 PERFORM 9900-ERROR-FORMAT
002526                                 THRU 9900-EXIT
002527                 move al-uabon   to instypea
002528                 move -1         to instypel
002529                 move 'X'        to error-switch
002530              end-if
002531           when instypei = 'P'
002532              if (w-name-first (1:10) <> cm-insured-first-name)
002533                 or (w-name-last <> cm-insured-last-name)
002534                 move er-1676    to emi-error
002535                 PERFORM 9900-ERROR-FORMAT
002536                                 THRU 9900-EXIT
002537                 move al-uabon   to instypea
002538                 move -1         to instypel
002539                 move 'X'        to error-switch
002540              end-if
002541        end-evaluate
002542     end-if
002543
002544*    if (instypel > zeros)
002545*       and (cl-no-of-days-paid > zero)
002546*       move er-1668             to emi-error
002547*       perform 9900-error-format thru 9900-exit
002548*       move al-uabon            to instypea
002549*       move -1                  to instypel
002550*       move 'X'                 to error-switch
002551*    end-if
002552*
002553*    if instypel > zero
002554*       if instypei = 'C'
002555*          if w-name-first (1:10) = cm-insured-first-name
002556*             move er-1666          to emi-error
002557*             PERFORM 9900-ERROR-FORMAT
002558*                                   THRU 9900-EXIT
002559*             move al-uabon         to instypea
002560*             move -1               to instypel
002561*             move 'X'              to error-switch
002562*          end-if
002563*       end-if
002564*    end-if
002565
002566     if (pi-company-id = 'DCC' OR 'VPP')
002567        and (acct-found)
002568        display ' about to perform 2120 '
002569        perform 2120-check-pdef  thru 2120-exit
002570        display ' back from 2120- ' ws-pre-exsist
002571        IF EXTENSL > ZERO
002572           IF EXTENSI NUMERIC
002573              IF EXTENSI >  WS-MAX-EXTENSION
002574                AND ERPDEF-FOUND
002575                 MOVE ER-1678          TO EMI-ERROR
002576                 PERFORM 9900-ERROR-FORMAT
002577                                       THRU 9900-EXIT
002578                 MOVE AL-UABON         TO EXTENSA
002579                 MOVE -1               TO EXTENSL
002580                 MOVE 'X'              TO ERROR-SWITCH
002581              ELSE
002582                 MOVE 'X'              TO UPDATE-SWITCH
002583              END-IF
002584           ELSE
002585              MOVE ER-1778          TO EMI-ERROR
002586              PERFORM 9900-ERROR-FORMAT
002587                                    THRU 9900-EXIT
002588              MOVE AL-UABON         TO EXTENSA
002589              MOVE -1               TO EXTENSL
002590              MOVE 'X'              TO ERROR-SWITCH
002591           END-IF
002592        END-IF
002593     end-if
002594
002595     if benperl > zeros
002596        move al-uanon            to benpera
002597     end-if
002598     if benperl > zeros
002599        if benperi numeric
002600           perform 1015-edit-benefit-period
002601                                 thru 1015-exit
002602        else
002603           move er-1669          to emi-error
002604           PERFORM 9900-ERROR-FORMAT
002605                                 THRU 9900-EXIT
002606           move al-uabon         to benpera
002607           move -1               to benperl
002608           move 'X'              to error-switch
002609        end-if
002610    end-if
002611
002612     if instypel > zeros
002613        if instypei = 'P' OR 'C'
002614           move 'X'              to update-switch
002615                                    mstr-switch
002616           move al-uanon         to instypea
002617        else
002618            move er-1654          to emi-error
002619           PERFORM 9900-ERROR-FORMAT
002620                                 THRU 9900-EXIT
002621           move al-uabon         to instypea
002622           move -1               to instypel
002623           move 'X'              to error-switch
002624        end-if
002625    end-if
002626
002627     if accswl > zeros
002628        if accswi = 'Y' OR 'N'
002629           MOVE 'X'              TO UPDATE-SWITCH
002630                                    MSTR-SWITCH
002631           MOVE AL-UANON         TO accswa
002632*       else
002633*          move er-1656          to emi-error
002634*          PERFORM 9900-ERROR-FORMAT
002635*                                THRU 9900-EXIT
002636*          move al-uabon         to accswa
002637*          move -1               to accswl
002638*          move 'X'              to error-switch
002639*          move al-uanof         to accswa
002640        end-if
002641     end-if
002642
002643     IF CERTL = ZEROS AND
002644        SUFXL = ZEROS
002645          GO TO 1010-EXIT.
002646
002647     IF CERTL GREATER THAN ZEROS
002648         IF CERTI NOT = PI-SAVE-CERT-NO-PRIME
002649             MOVE CERTI          TO PI-SAVE-CERT-NO-PRIME
002650             MOVE 'Y'            TO SKIP-ATTRIBUTE
002651             MOVE 'X'            TO ERROR-SWITCH
002652             PERFORM 7000-RESET-ATTRIBUTE THRU 7000-EXIT.
002653
002654     IF SUFXL GREATER THAN ZEROS
002655         IF SUFXI NOT = PI-SAVE-CERT-NO-SUFX
002656             MOVE SUFXI          TO PI-SAVE-CERT-NO-SUFX
002657             MOVE 'Y'            TO SKIP-ATTRIBUTE
002658             MOVE 'X'            TO ERROR-SWITCH
002659             PERFORM 7000-RESET-ATTRIBUTE THRU 7000-EXIT.
002660
002661 1010-EXIT.
002662     EXIT.
002663
002664 1015-edit-benefit-period.
002665
002666     move ' '                    to ws-benper-sw
002667     evaluate true
002668        when not erpdef-found
002669           set good-benper       to true
002670        when pd-recurring-yn (a1) = 'Y'
002671           set good-benper       to true
002672        when pd-recurring-yn (a1) = 'N'
002673           if benperi < 02
002674              set good-benper    to true
002675           end-if
002676        when pd-rec-crit-period (a1) numeric
002677           if benperi <= pd-rec-crit-period (a1)
002678              set good-benper    to true
002679           end-if
002680     end-evaluate
002681
002682     MOVE CERT-KEY               TO ELCRTT-KEY
002683     MOVE 'B'                    TO CTRLR-REC-TYPE
002684     
      * EXEC CICS READ
002685*       DATASET   ('ELCRTT')
002686*       RIDFLD    (ELCRTT-KEY)
002687*       INTO      (CERTIFICATE-TRAILERS)
002688*       RESP      (WS-RESPONSE)
002689*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00009427' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303039343237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002690     IF WS-RESP-NORMAL
002691        perform varying s1 from +1 by +1 until
002692           (s1 > +24)
002693           or ((cs-claim-type (s1) = cl-claim-type)
002694           and (cs-insured-type (s1) = cl-insured-type)
002695           and (cs-benefit-period (s1) > benperi))
002696        end-perform
002697        if s1 < +25
002698           move er-1665          to emi-error
002699           PERFORM 9900-ERROR-FORMAT
002700                                 THRU 9900-EXIT
002701           move al-uabon         to benpera
002702           move -1               to benperl
002703           move 'X'              to error-switch
002704           go to 1015-exit
002705*       else
002706*          set good-benper       to true
002707        end-if
002708     end-if
002709
002710     if good-benper
002711        move 'X'                 to update-switch
002712                                    mstr-switch
002713        move al-uanon            to benpera
002714     else
002715        move er-1669             to emi-error
002716        PERFORM 9900-ERROR-FORMAT
002717                                 THRU 9900-EXIT
002718        move al-uabon            to benpera
002719        move -1                  to benperl
002720        move 'X'                 to error-switch
002721     end-if
002722
002723     .
002724 1015-exit.
002725     exit.
002726 1030-EDIT-NAME.
002727     IF INSTYPEL > ZEROS
002728        MOVE INSTYPEI TO CL-INSURED-TYPE
002729     END-IF
002730     IF MFNAMEL > +0
002731        IF CL-INSURED-TYPE = 'C'
002732          AND (CM-INSURED-FIRST-NAME = MFNAMEI(1:10))
002733           MOVE ER-1675             TO EMI-ERROR
002734           MOVE -1                  TO MFNAMEL
002735           MOVE AL-UABON            TO MFNAMEA
002736           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002737           MOVE 'X'                 TO ERROR-SWITCH
002738        ELSE
002739          IF CL-INSURED-TYPE = 'P'
002740            AND (CM-INSURED-FIRST-NAME <> MFNAMEI(1:10))
002741             MOVE ER-1676             TO EMI-ERROR
002742             MOVE -1                  TO MFNAMEL
002743             MOVE AL-UABON            TO MFNAMEA
002744             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002745             MOVE 'X'                 TO ERROR-SWITCH
002746          END-IF
002747        END-IF
002748     END-IF.
002749
002750 1030-EXIT.
002751     EXIT.
002752
002753 1050-EDIT-BENE.
002754     MOVE PI-COMPANY-CD      TO TRLR-COMPANY-CD.
002755     MOVE PI-CARRIER         TO TRLR-CARRIER.
002756     MOVE PI-CLAIM-NO        TO TRLR-CLAIM-NO.
002757     MOVE PI-CERT-NO         TO TRLR-CERT-NO.
002758     MOVE 1000               TO TRLR-SEQ-NO.
002759
002760     
      * EXEC CICS HANDLE CONDITION
002761*         NOTFND    (1052-DONE)
002762*         ENDFILE   (1052-DONE)
002763*    END-EXEC.
      *    MOVE '"$I''                  ! & #00009503' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303039353033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002764
002765     
      * EXEC CICS STARTBR
002766*        DATASET (TRLR-FILE-ID)
002767*        RIDFLD  (TRLR-KEY)
002768*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009508' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303039353038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002769
002770
002771     MOVE SPACES TO WS-TRLR-FILE-EOF
002772                    WS-PAYMENT-PENDING
002773     PERFORM 1052-READ-TRLR THRU 1052-EXIT
002774       UNTIL  TRLR-FILE-EOF
002775         OR PAYMENT-PENDING
002776
002777     
      * EXEC CICS ENDBR
002778*        DATASET (TRLR-FILE-ID)
002779*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009520' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039353230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002780 1050-EXIT.
002781     EXIT.
002782
002783 1052-READ-TRLR.
002784     
      * EXEC CICS READNEXT
002785*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
002786*         DATASET  (TRLR-FILE-ID)
002787*         RIDFLD   (TRLR-KEY)
002788*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009527' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303039353237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002789
002790     IF PI-COMPANY-CD   = TRLR-COMPANY-CD
002791       AND PI-CARRIER  = TRLR-CARRIER
002792       AND PI-CLAIM-NO = TRLR-CLAIM-NO
002793       AND PI-CERT-NO  = TRLR-CERT-NO
002794        CONTINUE
002795     ELSE
002796        GO TO 1052-DONE
002797     END-IF.
002798
002799     IF PAYMENT-TR
002800       AND BENEFICIARY-PAID
002801       AND AT-VOID-DT = LOW-VALUES
002802       AND AT-CHECK-WRITTEN-DT NOT > SPACES
002803         SET PAYMENT-PENDING TO TRUE
002804     END-IF.
002805
002806     IF CORRESPONDENCE-TR
002807        IF AT-AUTH-RCVD = 'Y'
002808           SET AUTH-RCVD TO TRUE
002809        ELSE
002810        IF AT-AUTH-RCVD = 'N'
002811           SET NO-AUTH-RCVD TO TRUE
002812        END-IF
002813        END-IF
002814     END-IF.
002815
002816     GO TO 1052-EXIT.
002817
002818 1052-DONE.
002819     SET TRLR-FILE-EOF TO TRUE.
002820 1052-EXIT.
002821     EXIT.
002822     EJECT
002823
002824
002825 1120-EDIT-PROC.
002826     MOVE PI-COMPANY-ID          TO CNTL-CO-ID.
002827     MOVE '2'                    TO CNTL-REC-TYPE.
002828     MOVE PROCI                  TO CNTL-PROC-ID.
002829     MOVE ZERO                   TO CNTL-SEQ-NO.
002830
002831     
      * EXEC CICS HANDLE CONDITION
002832*         NOTFND    (1120-CNTL-NOTFND)
002833*    END-EXEC.
      *    MOVE '"$I                   ! '' #00009574' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303039353734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002834
002835     
      * EXEC CICS READ
002836*         SET      (ADDRESS OF CONTROL-FILE)
002837*         DATASET  (CNTL-FILE-ID)
002838*         RIDFLD   (CNTL-KEY)
002839*    END-EXEC.
      *    MOVE '&"S        E          (   #00009578' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039353738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002840
002841     MOVE AL-UANON               TO PROCA.
002842     MOVE 'X' TO UPDATE-SWITCH MSTR-SWITCH MSTR-KEY-SWITCH.
002843     GO TO 1120-EXIT.
002844
002845 1120-CNTL-NOTFND.
002846     MOVE ER-0273                TO EMI-ERROR.
002847     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002848     MOVE AL-UABON               TO PROCA.
002849     MOVE -1                     TO PROCL.
002850     MOVE 'X'                    TO ERROR-SWITCH.
002851
002852 1120-EXIT.
002853     EXIT.
002854
002855*************************************************************
002856*****    BEGIN PROCESSING FOR OUTPUTTING DLYACTV RECORD    **
002857*************************************************************
002858
002859 1150-OUTPUT-ACTIVITY-RECORD.
002860
002861     
      * EXEC CICS GETMAIN
002862*        SET (ADDRESS OF DAILY-ACTIVITY-RECORD)
002863*        LENGTH (25)
002864*        INITIMG (WS-BLANK)
002865*    END-EXEC.
           MOVE 25
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00009604' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039363034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-BLANK
           SET ADDRESS OF DAILY-ACTIVITY-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002866
002867     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.
002868     MOVE PI-COMPANY-CD          TO DA-COMP-CD.
002869     MOVE PI-CARRIER             TO DA-CARRIER.
002870     MOVE PI-CLAIM-NO            TO DA-CLAIM-NO.
002871     MOVE PI-CERT-NO             TO DA-CERT-NO.
002872     MOVE ZEROS                  TO DA-TRAILER-SEQ-NO.
002873     MOVE WS-HOLD-CLAIM-STATUS   TO DA-RECORD-TYPE.
002874
002875     
      * EXEC CICS HANDLE CONDITION
002876*        NOTOPEN (1150-NOTOPEN-ERROR)
002877*        DUPREC (1150-EXIT)
002878*    END-EXEC.
      *    MOVE '"$J%                  ! ( #00009618' TO DFHEIV0
           MOVE X'22244A252020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303039363138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002879     MOVE DAILY-ACTIVITY-RECORD  TO JP-RECORD-AREA.
002880     MOVE 'DLYACTV'              TO JP-FILE-ID.
002881     MOVE 'A'                    TO JP-RECORD-TYPE.
002882     MOVE +48                    TO JOURNAL-LENGTH.
002883
002884     
      * EXEC CICS WRITE
002885*        DATASET ('DLYACTV')
002886*        RIDFLD (DA-KEY)
002887*        FROM (DAILY-ACTIVITY-RECORD)
002888*        LENGTH (25)
002889*    END-EXEC.
           MOVE 'DLYACTV' TO DFHEIV1
           MOVE 25
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009627' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039363237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DAILY-ACTIVITY-RECORD, 
                 DFHEIV11, 
                 DA-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002890
002891     GO TO 1150-EXIT.
002892
002893 1150-NOTOPEN-ERROR.
002894
002895     MOVE '2955'                 TO EMI-ERROR.
002896     MOVE -1                     TO MAINTL.
002897     MOVE AL-UANON               TO MAINTA.
002898     MOVE 'X'                    TO ERROR-SWITCH.
002899     PERFORM 9900-ERROR-FORMAT THRU
002900             9900-EXIT.
002901
002902 1150-EXIT.
002903     EXIT.
002904
002905*************************************************************
002906*****    END OF PROCESSING FOR OUTPUTTING DLYACTV RECORD    *
002907*************************************************************
002908
002909 1240-EDIT-LIFE-CODE.
002910
002911     IF INVALID-BENEFIT-CODE
002912         GO TO 1240-SET-ERROR.
002913
002914     MOVE SPACES                 TO BENEFIT-KEY.
002915     MOVE PI-COMPANY-ID          TO BEN-CO-ID.
002916     MOVE '4'                    TO BEN-REC-TYPE.
002917     MOVE LCVCDI                 TO BEN-ACC-CD HOLD-BENEFIT.
002918     MOVE ZERO                   TO BEN-SEQ-NO.
002919
002920     
      * EXEC CICS READ GTEQ
002921*         DATASET  (CNTL-FILE-ID)
002922*         RIDFLD   (BENEFIT-KEY)
002923*         SET      (ADDRESS OF CONTROL-FILE)
002924*    END-EXEC.
      *    MOVE '&"S        G          (   #00009663' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303039363633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENEFIT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002925
002926     IF CF-COMPANY-ID  NOT = PI-COMPANY-ID OR
002927        CF-RECORD-TYPE NOT = '4'
002928         GO TO 1240-SET-ERROR.
002929
002930     MOVE ZERO                   TO COUNT-2.
002931     PERFORM 5040-FIND-BENEFIT THRU 5060-EXIT.
002932
002933     IF SCREEN-ERROR
002934         GO TO 1240-SET-ERROR.
002935
002936 1240-SET-CODE-SWITCHES.
002937     MOVE 'X'                    TO UPDATE-SWITCH CERT-SWITCH.
002938     MOVE AL-UANON               TO LCVCDA.
002939     GO TO 1240-EXIT.
002940
002941 1240-SET-ERROR.
002942     MOVE ER-0240                TO EMI-ERROR.
002943     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002944     MOVE 'X'                    TO ERROR-SWITCH.
002945     MOVE AL-UABON               TO LCVCDA.
002946     MOVE -1                     TO LCVCDL.
002947
002948 1240-EXIT.
002949     EXIT.
002950
002951 1310-EDIT-AH-CODE.
002952     IF INVALID-BENEFIT-CODE
002953         GO TO 1310-SET-ERROR.
002954
002955     MOVE SPACES                 TO BENEFIT-KEY.
002956     MOVE PI-COMPANY-ID          TO BEN-CO-ID.
002957     MOVE '5'                    TO BEN-REC-TYPE.
002958     MOVE ACVCDI                 TO BEN-ACC-CD HOLD-BENEFIT.
002959     MOVE ZERO                   TO BEN-SEQ-NO.
002960
002961     
      * EXEC CICS READ GTEQ
002962*         DATASET  (CNTL-FILE-ID)
002963*         RIDFLD   (BENEFIT-KEY)
002964*         SET      (ADDRESS OF CONTROL-FILE)
002965*    END-EXEC.
      *    MOVE '&"S        G          (   #00009704' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303039373034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENEFIT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002966
002967     IF CF-COMPANY-ID  NOT = PI-COMPANY-ID OR
002968        CF-RECORD-TYPE NOT = '5'
002969         GO TO 1310-SET-ERROR.
002970
002971     MOVE ZERO                   TO COUNT-2.
002972     PERFORM 5040-FIND-BENEFIT THRU 5060-EXIT.
002973     IF SCREEN-ERROR
002974         GO TO 1310-SET-ERROR.
002975
002976 1310-SET-CODE-SWITCHES.
002977     MOVE 'X'                    TO UPDATE-SWITCH CERT-SWITCH.
002978     MOVE AL-UANON               TO ACVCDA.
002979     GO TO 1310-EXIT.
002980
002981 1310-SET-ERROR.
002982     MOVE ER-0250                TO EMI-ERROR.
002983     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002984     MOVE AL-UABON               TO ACVCDA.
002985     MOVE 'X'                    TO ERROR-SWITCH.
002986     MOVE -1                     TO ACVCDL.
002987
002988 1310-EXIT.
002989     EXIT.
002990
002991     EJECT
002992 1460-EDIT-CERT-NO.
002993     MOVE CERTI                  TO WS-CERT-NO-PRIME.
002994     MOVE SUFXI                  TO WS-CERT-NO-SUFX.
002995
002996     IF WS-SAVE-CERT-NO = SPACES
002997         MOVE ER-0203            TO EMI-ERROR
002998         GO TO 1460-CERT-NO-ERROR.
002999
003000     IF WS-CERT-NO-PRIME = SPACES AND
003001        WS-CERT-NO-SUFX GREATER THAN SPACE
003002            MOVE ER-0210         TO EMI-ERROR
003003            GO TO 1460-CERT-NO-ERROR.
003004
003005     MOVE 'X'                    TO UPDATE-SWITCH.
003006     MOVE 'Y'                    TO CERT-KEY-SWITCH.
003007     MOVE AL-UANON               TO CERTA.
003008
003009     IF SUFXL IS GREATER THAN 0
003010         MOVE AL-UANON           TO SUFXA.
003011
003012     GO TO 1460-EXIT.
003013
003014 1460-CERT-NO-ERROR.
003015     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003016     MOVE AL-UABON               TO SUFXA.
003017     MOVE AL-UABON               TO CERTA.
003018     MOVE -1                     TO CERTL.
003019     MOVE 'X'                    TO ERROR-SWITCH.
003020
003021 1460-EXIT.
003022     EXIT.
003023
003024 1470-EDIT-EFF.
003025     IF CERTEFFI = SPACES
003026         GO TO 1470-DATE-ERROR.
003027
003028     MOVE CERTEFFI               TO DEEDIT-DATE-INPUT.
003029     PERFORM 1510-DEEDIT-DATE THRU 1510-EXIT.
003030     IF DEEDIT-DATE = ZERO
003031         GO TO 1470-DATE-ERROR.
003032
003033     MOVE DEEDIT-DATE            TO DC-GREG-DATE-1-MDY.
003034     MOVE '4'                    TO DC-OPTION-CODE.
003035     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
003036     IF NOT DATE-CONVERSION-ERROR
003037         MOVE DC-BIN-DATE-1      TO HOLD-EFF
003038         MOVE AL-UANON           TO CERTEFFA
003039         MOVE 'X'                TO UPDATE-SWITCH
003040         MOVE 'Y'                TO CERT-KEY-SWITCH
003041         MOVE DC-GREG-DATE-1-EDIT TO CERTEFFO
003042         GO TO 1470-EXIT.
003043
003044 1470-DATE-ERROR.
003045     MOVE ER-0215                TO EMI-ERROR.
003046     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003047     MOVE AL-UABON               TO CERTEFFA.
003048     MOVE -1                     TO CERTEFFL.
003049     MOVE 'X'                    TO ERROR-SWITCH.
003050
003051 1470-EXIT.
003052     EXIT.
003053
003054     EJECT
003055 1480-EDIT-ACCT.
003056     IF CERTACTI = SPACES
003057         MOVE ER-0232            TO EMI-ERROR
003058         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003059         MOVE AL-UABON           TO CERTACTA
003060         MOVE -1                 TO CERTACTL
003061         MOVE 'X'                TO ERROR-SWITCH
003062     ELSE
003063         MOVE 'X'                TO UPDATE-SWITCH
003064         MOVE 'Y'                TO CERT-KEY-SWITCH.
003065
003066 1480-EXIT.
003067     EXIT.
003068
003069 1490-EDIT-STATE.
003070     IF CERTSTI = SPACES
003071         MOVE ER-0233            TO EMI-ERROR
003072         GO TO 1490-STATE-ERROR.
003073
003074     MOVE PI-COMPANY-ID          TO CNTL-CO-ID.
003075     MOVE '3'                    TO CNTL-REC-TYPE.
003076     MOVE SPACES                 TO CNTL-STATE-ACCESS.
003077     MOVE CERTSTI                TO CNTL-STATE-NUMBER.
003078     MOVE ZERO                   TO CNTL-SEQ-NO.
003079
003080     
      * EXEC CICS HANDLE CONDITION
003081*         NOTFND (1490-STATE-NOTFND)
003082*    END-EXEC.
      *    MOVE '"$I                   ! ) #00009823' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303039383233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003083
003084     
      * EXEC CICS READ
003085*         SET        (ADDRESS OF CONTROL-FILE)
003086*         DATASET    ('ELCNTL')
003087*         RIDFLD     (CNTL-KEY)
003088*         KEYLENGTH  (CNTL-GENERIC-LENGTH)
003089*         GENERIC
003090*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S  KG    E          (   #00009827' TO DFHEIV0
           MOVE X'26225320204B472020202045' &
                X'202020202020202020202820' &
                X'2020233030303039383237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 CNTL-GENERIC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003091
003092     MOVE AL-UANON               TO CERTSTA.
003093     MOVE 'X'                    TO UPDATE-SWITCH.
003094     MOVE 'Y'                    TO CERT-KEY-SWITCH.
003095     GO TO 1490-EXIT.
003096
003097 1490-STATE-NOTFND.
003098     MOVE ER-0149                TO EMI-ERROR.
003099
003100 1490-STATE-ERROR.
003101     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003102     MOVE AL-UABON               TO CERTSTA.
003103     MOVE -1                     TO CERTSTL.
003104     MOVE 'X'                    TO ERROR-SWITCH.
003105
003106 1490-EXIT.
003107     EXIT.
003108     EJECT
003109 1500-EDIT-CARR.
003110     IF CERTCARI = SPACES
003111         GO TO 1500-CARR-NOTFND.
003112
003113     MOVE PI-COMPANY-ID          TO CNTL-CO-ID.
003114     MOVE '6'                    TO CNTL-REC-TYPE.
003115     MOVE SPACES                 TO CNTL-CARRIER-ACCESS.
003116     MOVE CERTCARI               TO CNTL-CARRIER.
003117     MOVE ZERO                   TO CNTL-SEQ-NO.
003118
003119     
      * EXEC CICS HANDLE CONDITION
003120*         NOTFND   (1500-CARR-NOTFND)
003121*    END-EXEC.
      *    MOVE '"$I                   ! * #00009862' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303039383632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003122
003123     
      * EXEC CICS READ
003124*         SET        (ADDRESS OF CONTROL-FILE)
003125*         DATASET    ('ELCNTL')
003126*         RIDFLD     (CNTL-KEY)
003127*         KEYLENGTH  (CNTL-GENERIC-LENGTH)
003128*         GENERIC
003129*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S  KG    E          (   #00009866' TO DFHEIV0
           MOVE X'26225320204B472020202045' &
                X'202020202020202020202820' &
                X'2020233030303039383636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 CNTL-GENERIC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003130
003131     MOVE AL-UANON               TO CERTCARA.
003132     MOVE 'X'                    TO UPDATE-SWITCH.
003133     MOVE 'Y'                    TO CERT-KEY-SWITCH.
003134     GO TO 1500-EXIT.
003135
003136 1500-CARR-NOTFND.
003137     MOVE ER-0360                TO EMI-ERROR.
003138     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003139     MOVE AL-UABON               TO CERTCARA.
003140     MOVE -1                     TO CERTCARL.
003141     MOVE 'X'                    TO ERROR-SWITCH.
003142
003143 1500-EXIT.
003144     EXIT.
003145     EJECT
003146 1510-DEEDIT-DATE.
003147     
      * EXEC CICS BIF DEEDIT
003148*         FIELD    (DEEDIT-DATE-INPUT)
003149*         LENGTH   (DATE-LENGTH)
003150*    END-EXEC.
      *    MOVE '@"L                   #   #00009890' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303039383930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-DATE-INPUT, 
                 DATE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003151
003152 1510-EXIT.
003153     EXIT.
003154
003155     EJECT
003156 1620-VERIFY-ACCT.
003157
003158     move ' '                    to ws-eracct-sw
003159     MOVE PI-COMPANY-CD          TO ACCT-COMPANY-CODE.
003160     MOVE PI-CARRIER             TO ACCT-CARRIER.
003161     MOVE PI-GROUPING            TO ACCT-GROUP.
003162     MOVE PI-STATE               TO ACCT-STATE.
003163     MOVE PI-ACCOUNT             TO ACCT-ACCOUNT.
003164
003165     IF CERTEFFI GREATER THAN LOW-VALUES
003166         MOVE HOLD-EFF           TO ACCT-DATE
003167     ELSE
003168         MOVE PI-CERT-EFF-DT     TO HOLD-EFF ACCT-DATE.
003169
003170     IF CERTACTI GREATER THAN LOW-VALUES
003171         MOVE CERTACTI           TO  ACCT-ACCOUNT.
003172     IF CERTSTI GREATER THAN LOW-VALUES
003173         MOVE CERTSTI            TO  ACCT-STATE.
003174     IF CERTCARI GREATER THAN LOW-VALUES
003175         MOVE CERTCARI           TO  ACCT-CARRIER.
003176     IF CERTGRPI GREATER THAN LOW-VALUES
003177         MOVE CERTGRPI           TO ACCT-GROUP.
003178
003179     MOVE ACCT-PARTIAL-KEY       TO SAVE-ACCT-KEY.
003180
003181     
      * EXEC CICS HANDLE CONDITION
003182*         NOTFND  (1620-ACCT-NOTFND)
003183*    END-EXEC.
      *    MOVE '"$I                   ! + #00009924' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303039393234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003184
003185     
      * EXEC CICS STARTBR
003186*         DATASET   (ACCT-FILE-ID)
003187*         RIDFLD    (ACCT-KEY)
003188*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009928' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303039393238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003189
003190 1620-ACCT-LOOP.
003191
003192     
      * EXEC CICS READNEXT
003193*         DATASET  (ACCT-FILE-ID)
003194*         RIDFLD   (ACCT-KEY)
003195*         SET      (ADDRESS OF ACCOUNT-MASTER)
003196*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009935' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303039393335' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003197
003198     IF SAVE-ACCT-KEY NOT = ACCT-PARTIAL-KEY
003199         GO TO 1620-ACCT-NOTFND.
003200
003201     IF HOLD-EFF NOT LESS AM-EFFECTIVE-DT  AND
003202        HOLD-EFF     LESS AM-EXPIRATION-DT
003203         NEXT SENTENCE
003204     ELSE
003205         GO TO 1620-ACCT-LOOP.
003206
003207     set acct-found to true
003208
003209     IF PI-COMPANY-ID NOT = 'CRI' AND 'PEM' AND 'NCL'
003210         GO TO 1620-ACCT-END-BROWSE.
003211
003212     IF LOANBALL GREATER THAN +0
003213         IF AM-MAX-TOT-BEN NUMERIC  AND
003214            AM-MAX-TOT-BEN GREATER THAN ZERO
003215             IF HOLD-LOANBAL GREATER THAN AM-MAX-TOT-BEN
003216                 MOVE AL-UNBON     TO LOANBALA
003217                 MOVE -1           TO LOANBALL
003218                 MOVE 'X'          TO ERROR-SWITCH
003219                 MOVE ER-7641      TO EMI-ERROR
003220                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003221
003222     IF ACVBENEL GREATER THAN +0
003223         IF AM-MAX-MON-BEN NUMERIC  AND
003224            AM-MAX-MON-BEN GREATER THAN ZERO
003225             IF HOLD-AH-CV-BEN GREATER THAN AM-MAX-MON-BEN
003226                 MOVE AL-UNBON     TO ACVBENEA
003227                 MOVE -1           TO ACVBENEL
003228                 MOVE 'X'          TO ERROR-SWITCH
003229                 MOVE ER-7642      TO EMI-ERROR
003230                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003231
003232 1620-ACCT-END-BROWSE.
003233
003234     
      * EXEC CICS ENDBR
003235*         DATASET (ACCT-FILE-ID)
003236*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009977' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039393737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003237
003238     GO TO 1620-EXIT.
003239
003240 1620-ACCT-NOTFND.
003241
003242     MOVE -1                     TO MAINTL.
003243     MOVE 'X'                    TO ERROR-SWITCH.
003244     MOVE ER-0426                TO EMI-ERROR.
003245     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003246
003247 1620-EXIT.
003248     EXIT.
003249
003250     EJECT
003251 2000-UPDATE-CLAIM.
003252
003253     MOVE SPACES                 TO ERROR-SWITCH.
003254     MOVE PI-COMPANY-CD          TO COMPANY-CODE.
003255     MOVE PI-CARRIER             TO CARRIER-CODE.
003256     MOVE PI-CLAIM-NO            TO CLAIM-NO.
003257     MOVE PI-CERT-NO             TO CERT-NO.
003258
003259     IF DELETE-CLAIM
003260         GO TO 3000-DELETE-CLAIM.
003261
003262     IF  NOT MODIFY-CAP
003263         MOVE ER-0070            TO EMI-ERROR
003264         MOVE 'X'                TO ERROR-SWITCH
003265         MOVE -1                 TO MAINTL
003266         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003267         GO TO 2000-EXIT.
003268
003269     MOVE MSTR-KEY               TO TRLR-MAIN-KEY ACTQ-KEY.
003270     MOVE ZERO                   TO TRLR-SEQ-NO.
003271
003272     
      * EXEC CICS READ UPDATE
003273*         SET       (ADDRESS OF CLAIM-MASTER)
003274*         DATASET   (CLMS-FILE-ID)
003275*         RIDFLD    (MSTR-KEY)
003276*    END-EXEC.
      *    MOVE '&"S        EU         (   #00010015' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303130303135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003277
003278     if (incl > zeros)
003279        and (cl-no-of-pmts-made > 0)
003280        and (hold-incur > low-values)
003281        and (hold-incur not = cl-incurred-dt)
003282        if hold-incur > cl-incurred-dt
003283           move cl-incurred-dt   to dc-bin-date-1
003284           move hold-incur       to dc-bin-date-2
003285        else
003286           move hold-incur       to dc-bin-date-1
003287           move cl-incurred-dt   to dc-bin-date-2
003288        end-if
003289        move '1'                 to dc-option-code
003290        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
003291        if dc-elapsed-days > +180
003292           MOVE ER-1663             TO EMI-ERROR
003293           PERFORM 9900-ERROR-FORMAT
003294                                 THRU 9900-EXIT
003295           MOVE -1                  TO MAINTL
003296           GO TO 8110-SEND-DATA
003297        end-if
003298     end-if
003299
003300     IF (CL-PRIORITY-CD = '8')
003301        AND (PI-PROCESSOR-ID NOT = 'PEMA' AND 'JMS '
003302             AND 'AMWA' AND 'KMSB')
003303        MOVE ER-8003             TO EMI-ERROR
003304        PERFORM 9900-ERROR-FORMAT
003305                                 THRU 9900-EXIT
003306        MOVE -1                  TO MAINTL
003307        GO TO 8110-SEND-DATA
003308     END-IF
003309
003310     IF PI-UPDATE-BY NOT = CL-LAST-MAINT-USER
003311         MOVE ER-0068            TO EMI-ERROR
003312         PERFORM 2010-RELEASE-REC THRU 2010-EXIT
003313         GO TO 2000-EXIT.
003314
003315     IF PI-UPDATE-HHMMSS NOT = CL-LAST-MAINT-HHMMSS
003316         MOVE ER-0068            TO EMI-ERROR
003317         PERFORM 2010-RELEASE-REC THRU 2010-EXIT
003318         GO TO 2000-EXIT.
003319
003320     IF CL-CLAIM-PAYMENT-STATUS NOT NUMERIC
003321         MOVE ZEROS              TO CL-CLAIM-PAYMENT-STATUS.
003322
003323     IF CL-PURGED-DT NOT = LOW-VALUES
003324         MOVE ER-7691            TO EMI-ERROR
003325         PERFORM 2010-RELEASE-REC THRU 2010-EXIT
003326         GO TO 2000-EXIT.
003327
003328     IF CERT-KEY-CHANGED
003329         PERFORM 2060-CHANGE-CERT THRU 2060-EXIT.
003330
003331     IF SCREEN-ERROR
003332         MOVE ER-0068            TO EMI-ERROR
003333         PERFORM 2010-RELEASE-REC THRU 2010-EXIT
003334         GO TO 2000-EXIT.
003335
003336     IF NINETY-TRLR-UPDATED
003337         PERFORM 2425-UPDATE-NINETY-TRLR THRU 2425-EXIT.
003338
003339     IF MSTR-KEY-CHANGED
003340         PERFORM 2030-KEY-UPDATE THRU 2030-EXIT
003341        ELSE
003342         PERFORM 2020-NORMAL-UPDATE THRU 2020-EXIT.
003343
003344     IF TRLR-UPDATE-REQUIRED
003345         PERFORM 2300-UPDATE-TRLR THRU 2300-EXIT.
003346
003347 2000-CHECK-CERT.
003348     IF NOT CERT-UPDATES
003349         GO TO 2000-EXIT.
003350
003351     MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.
003352     MOVE PI-CARRIER             TO CERT-CARRIER.
003353     MOVE PI-GROUPING            TO CERT-GROUP.
003354     MOVE PI-STATE               TO CERT-STATE.
003355     MOVE PI-ACCOUNT             TO CERT-ACCOUNT.
003356     MOVE PI-CERT-EFF-DT         TO CERT-DATE.
003357     MOVE PI-CERT-NO             TO CERT-CERT.
003358
003359     IF CERT-ALT-KEY-CHANGED
003360         PERFORM 2050-KEY-UPDATE THRU 2050-EXIT
003361     ELSE
003362         PERFORM 2040-NORMAL-UPDATE THRU 2040-EXIT.
003363
003364 2000-EXIT.
003365     EXIT.
003366     EJECT
003367 2010-RELEASE-REC.
003368     
      * EXEC CICS UNLOCK
003369*         DATASET  (CLMS-FILE-ID)
003370*    END-EXEC
      *    MOVE '&*                    #   #00010111' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303130313131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003371
003372     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003373     MOVE 'X'                    TO ERROR-SWITCH.
003374
003375 2010-EXIT.
003376     EXIT.
003377
003378 2020-NORMAL-UPDATE.
003379
003380     PERFORM 2600-CREATE-MAINT-NOTE THRU 2600-EXIT.
003381
003382     PERFORM 2100-MOVE-MSTR THRU 2100-EXIT.
003383
003384     IF WS-OPEN-CLOSE-SW = 'Y'
003385       PERFORM  7800-CHECK-AUTO-ACTIVITY THRU 7800-EXIT
003386       IF PI-REC-FOUND-SW = 'Y'
003387         IF CL-ACTIVITY-CODE = 09
003388           NEXT SENTENCE
003389         ELSE
003390           IF WS-RESET-SW = 'Y'
003391             IF CL-CLAIM-STATUS = 'C'
003392               MOVE 07               TO  CL-ACTIVITY-CODE
003393               MOVE SAVE-BIN-DATE    TO  CL-ACTIVITY-MAINT-DT
003394               MOVE 'CLOS'           TO  CL-ACTIVITY-MAINT-TYPE
003395             ELSE
003396               MOVE 08               TO  CL-ACTIVITY-CODE
003397               MOVE SAVE-BIN-DATE    TO  CL-ACTIVITY-MAINT-DT
003398               MOVE 'OPEN'           TO  CL-ACTIVITY-MAINT-TYPE.
003399
003400     IF PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL'
003401         MOVE 'Y'                TO CL-YESNOSW
003402     END-IF.
003403
003404     if pi-company-id = 'DCC' OR 'VPP'
003405        perform 1620-VERIFY-ACCT THRU 1620-EXIT
003406        perform 2120-check-pdef  thru 2120-exit
003407     end-if
003408
003409     if ((benperl <> zeros)
003410        and (benperi numeric))
003411                  or
003412            (instypel <> zeros)
003413        perform 2140-update-elcrtt thru 2140-exit
003414     end-if
003415
003416     
      * EXEC CICS HANDLE CONDITION
003417*         DUPKEY   (2020-EXIT)
003418*    END-EXEC.
      *    MOVE '"$$                   ! , #00010159' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303130313539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003419
003420     
      * EXEC CICS REWRITE
003421*         DATASET   (CLMS-FILE-ID)
003422*         FROM      (CLAIM-MASTER)
003423*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010163' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130313633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003424
003425 2020-EXIT.
003426     EXIT.
003427     EJECT
003428 2030-KEY-UPDATE.
003429     MOVE CLAIM-MASTER           TO JP-RECORD-AREA.
003430
003431     
      * EXEC CICS DELETE
003432*        DATASET  (CLMS-FILE-ID)
003433*    END-EXEC.
      *    MOVE '&(                    &   #00010174' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303130313734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003434
003435     
      * EXEC CICS GETMAIN
003436*         SET      (ADDRESS OF CLAIM-MASTER)
003437*         LENGTH   (MSTR-LENGTH)
003438*         INITIMG  (GETMAIN-SPACE)
003439*    END-EXEC.
      *    MOVE ',"IL                  $   #00010178' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130313738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 MSTR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003440
003441     MOVE JP-RECORD-AREA         TO CLAIM-MASTER.
003442
003443     PERFORM 2600-CREATE-MAINT-NOTE THRU 2600-EXIT.
003444
003445     PERFORM 2100-MOVE-MSTR THRU 2100-EXIT.
003446
003447     IF WS-OPEN-CLOSE-SW = 'Y'
003448       PERFORM 7800-CHECK-AUTO-ACTIVITY THRU 7800-EXIT
003449         IF PI-REC-FOUND-SW = 'Y'
003450           IF CL-ACTIVITY-CODE = 09
003451             NEXT SENTENCE
003452           ELSE
003453             IF WS-RESET-SW = 'Y'
003454               IF CL-CLAIM-STATUS = 'C'
003455                 MOVE 07             TO  CL-ACTIVITY-CODE
003456                 MOVE SAVE-BIN-DATE  TO  CL-ACTIVITY-MAINT-DT
003457                 MOVE 'CLOS'         TO  CL-ACTIVITY-MAINT-TYPE
003458               ELSE
003459                 MOVE 08             TO  CL-ACTIVITY-CODE
003460                 MOVE SAVE-BIN-DATE  TO  CL-ACTIVITY-MAINT-DT
003461                 MOVE 'OPEN'         TO  CL-ACTIVITY-MAINT-TYPE.
003462
003463     
      * EXEC CICS HANDLE CONDITION
003464*         DUPKEY    (2030-EXIT)
003465*    END-EXEC.
      *    MOVE '"$$                   ! - #00010206' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303130323036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003466
003467** POPULATE THE CREDIT-CARD NO WITH THE CERT NO.
003468*    MOVE CL-CERT-NO             TO CL-CCN-A5.
003469
003470     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
003471         MOVE 'Y'                TO CL-YESNOSW
003472     END-IF.
003473
003474     
      * EXEC CICS WRITE
003475*         DATASET   (CLMS-FILE-ID)
003476*         RIDFLD    (MSTR-KEY)
003477*         FROM      (CLAIM-MASTER)
003478*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010217' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130323137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003479
003480 2030-EXIT.
003481     EXIT.
003482
003483     EJECT
003484 2040-NORMAL-UPDATE.
003485     
      * EXEC CICS READ UPDATE
003486*         DATASET  (CERT-FILE-ID)
003487*         RIDFLD   (CERT-KEY)
003488*         SET      (ADDRESS OF CERTIFICATE-MASTER)
003489*    END-EXEC.
      *    MOVE '&"S        EU         (   #00010228' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303130323238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003490
003491     PERFORM 2200-MOVE-CERT THRU 2200-EXIT.
003492
003493     
      * EXEC CICS HANDLE CONDITION
003494*         DUPKEY   (2040-EXIT)
003495*    END-EXEC.
      *    MOVE '"$$                   ! . #00010236' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303130323336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003496
003497     
      * EXEC CICS REWRITE
003498*         DATASET  (CERT-FILE-ID)
003499*         FROM     (CERTIFICATE-MASTER)
003500*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010240' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130323430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003501
003502 2040-EXIT.
003503     EXIT.
003504
003505 2050-KEY-UPDATE.
003506     
      * EXEC CICS READ UPDATE
003507*         DATASET  (CERT-FILE-ID)
003508*         RIDFLD   (CERT-KEY)
003509*         SET      (ADDRESS OF CERTIFICATE-MASTER)
003510*    END-EXEC.
      *    MOVE '&"S        EU         (   #00010249' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303130323439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003511
003512     MOVE CERTIFICATE-MASTER     TO JP-RECORD-AREA.
003513
003514     
      * EXEC CICS DELETE
003515*         DATASET   (CERT-FILE-ID)
003516*    END-EXEC.
      *    MOVE '&(                    &   #00010257' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303130323537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003517
003518     
      * EXEC CICS GETMAIN
003519*         SET       (ADDRESS OF CERTIFICATE-MASTER)
003520*         LENGTH    (CERT-LENGTH)
003521*         INITIMG   (GETMAIN-SPACE)
003522*    END-EXEC.
      *    MOVE ',"IL                  $   #00010261' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130323631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 CERT-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003523
003524     MOVE JP-RECORD-AREA         TO CERTIFICATE-MASTER.
003525
003526     PERFORM 2200-MOVE-CERT THRU 2200-EXIT.
003527
003528     
      * EXEC CICS HANDLE CONDITION
003529*         DUPKEY (2050-EXIT)
003530*    END-EXEC.
      *    MOVE '"$$                   ! / #00010271' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303130323731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003531
003532     
      * EXEC CICS WRITE
003533*         DATASET  (CERT-FILE-ID)
003534*         RIDFLD   (CERT-KEY)
003535*         FROM     (CERTIFICATE-MASTER)
003536*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010275' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130323735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003537
003538 2050-EXIT.
003539     EXIT.
003540     EJECT
003541 2060-CHANGE-CERT.
003542     MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.
003543     MOVE PI-CARRIER             TO CERT-CARRIER.
003544     MOVE PI-GROUPING            TO CERT-GROUP.
003545     MOVE PI-STATE               TO CERT-STATE.
003546     MOVE PI-ACCOUNT             TO CERT-ACCOUNT.
003547     MOVE PI-CERT-EFF-DT         TO CERT-DATE.
003548     MOVE PI-CERT-NO             TO CERT-CERT.
003549     IF CERTEFFI GREATER THAN LOW-VALUES
003550         MOVE HOLD-EFF           TO CERT-DATE.
003551     IF CERTACTI GREATER THAN LOW-VALUES
003552         MOVE CERTACTI           TO CERT-ACCOUNT.
003553     IF CERTSTI GREATER THAN LOW-VALUES
003554         MOVE CERTSTI            TO CERT-STATE.
003555     IF CERTCARI GREATER THAN LOW-VALUES
003556         MOVE CERTCARI           TO CERT-CARRIER.
003557     IF CERTGRPI GREATER THAN LOW-VALUES
003558         MOVE CERTGRPI           TO CERT-GROUP.
003559     IF CERTI GREATER THAN LOW-VALUES
003560         MOVE CERTI              TO CERT-CERT-PRIME.
003561     IF SUFXI GREATER THAN LOW-VALUES
003562         MOVE SUFXI              TO CERT-CERT-SUFX.
003563
003564     
      * EXEC CICS HANDLE CONDITION
003565*         NOTFND   (2060-CERT-NOTFND)
003566*    END-EXEC.
      *    MOVE '"$I                   ! 0 #00010307' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3020233030303130333037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003567
003568     
      * EXEC CICS READ UPDATE
003569*         DATASET  (CERT-FILE-ID)
003570*         RIDFLD   (CERT-KEY)
003571*         SET      (ADDRESS OF CERTIFICATE-MASTER)
003572*    END-EXEC.
      *    MOVE '&"S        EU         (   #00010311' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303130333131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003573
003574     ADD 1                       TO CM-CLAIM-ATTACHED-COUNT.
003575
003576     IF CM-CLAIM-INTERFACE-SW = SPACES
003577        MOVE '1'   TO CM-CLAIM-INTERFACE-SW PI-CERT-SWITCH.
003578
003579     
      * EXEC CICS HANDLE CONDITION
003580*         DUPKEY   (2060-DUPKEY)
003581*    END-EXEC.
      *    MOVE '"$$                   ! 1 #00010322' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'3120233030303130333232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003582
003583     
      * EXEC CICS REWRITE
003584*         DATASET  (CERT-FILE-ID)
003585*         FROM     (CERTIFICATE-MASTER)
003586*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010326' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130333236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003587
003588 2060-DUPKEY.
003589
003590     PERFORM 2070-CHECK-OLD THRU 2070-EXIT.
003591
003592 2060-CHANGE-TRLR-ACTQ.
003593     IF SCREEN-ERROR
003594         GO TO 2060-EXIT.
003595
003596     IF CERTI    GREATER LOW-VALUES OR
003597        SUFXI    GREATER LOW-VALUES OR
003598        CERTCARI GREATER LOW-VALUES
003599         PERFORM 2400-UPDATE-TRLR THRU 2400-EXIT
003600         PERFORM 2430-UPDATE-ACTQ THRU 2430-EXIT.
003601
003602     MOVE SPACES                 TO CERT-KEY-SWITCH
003603                                    CERT-SWITCH
003604                                    TRLR-SWITCH
003605                                    NINETY-TRLR-SWITCH.
003606     MOVE 'X' TO MSTR-KEY-SWITCH MSTR-SWITCH.
003607     GO TO 2060-EXIT.
003608
003609 2060-CERT-NOTFND.
003610     PERFORM 2070-CHECK-OLD THRU 2070-EXIT.
003611
003612     IF WS-REC-FOUND-SW = 'N'
003613         MOVE ER-0206            TO  EMI-ERROR
003614         MOVE -1                 TO  MAINTL
003615         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003616         
      * EXEC CICS UNLOCK
003617*            DATASET   (CLMS-FILE-ID)
003618*        END-EXEC
      *    MOVE '&*                    #   #00010359' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303130333539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003619         GO TO 8110-SEND-DATA.
003620
003621     PERFORM 2080-CREATE-CERT THRU 2080-EXIT.
003622
003623     GO TO 2060-CHANGE-TRLR-ACTQ.
003624
003625 2060-EXIT.
003626     EXIT.
003627     EJECT
003628 2070-CHECK-OLD.
003629
003630     
      * EXEC CICS HANDLE CONDITION
003631*        NOTFND   (2070-NOT-FOUND)
003632*    END-EXEC.
      *    MOVE '"$I                   ! 2 #00010373' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3220233030303130333733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003633
003634     MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.
003635     MOVE PI-CARRIER             TO CERT-CARRIER.
003636     MOVE PI-GROUPING            TO CERT-GROUP.
003637     MOVE PI-STATE               TO CERT-STATE.
003638     MOVE PI-ACCOUNT             TO CERT-ACCOUNT.
003639     MOVE PI-CERT-EFF-DT         TO CERT-DATE.
003640     MOVE PI-CERT-NO             TO CERT-CERT.
003641
003642     
      * EXEC CICS READ UPDATE
003643*         DATASET  (CERT-FILE-ID)
003644*         RIDFLD   (CERT-KEY)
003645*         SET      (ADDRESS OF CERTIFICATE-MASTER)
003646*    END-EXEC.
      *    MOVE '&"S        EU         (   #00010385' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303130333835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003647
003648     MOVE 'Y'                    TO WS-REC-FOUND-SW.
003649     MOVE CERTIFICATE-MASTER     TO SAVE-RECORD.
003650     SUBTRACT 1                  FROM CM-CLAIM-ATTACHED-COUNT.
003651
003652     IF CM-CLAIM-ATTACHED-COUNT LESS THAN ZERO
003653         MOVE ZERO               TO CM-CLAIM-ATTACHED-COUNT.
003654
003655     IF CM-CLAIM-ATTACHED-COUNT = ZERO
003656       AND
003657        CERT-WAS-CREATED-FOR-CLAIM
003658         MOVE CERTIFICATE-MASTER TO JP-RECORD-AREA
003659         
      * EXEC CICS DELETE
003660*             DATASET   (CERT-FILE-ID)
003661*        END-EXEC
      *    MOVE '&(                    &   #00010402' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303130343032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003662         GO TO 2070-EXIT.
003663
003664     IF CM-CLAIM-ATTACHED-COUNT = ZERO
003665         MOVE SPACE TO CM-CLAIM-INTERFACE-SW PI-CERT-SWITCH.
003666
003667     
      * EXEC CICS HANDLE CONDITION
003668*         DUPKEY   (2070-EXIT)
003669*    END-EXEC.
      *    MOVE '"$$                   ! 3 #00010410' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'3320233030303130343130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003670
003671     
      * EXEC CICS REWRITE
003672*         DATASET  (CERT-FILE-ID)
003673*         FROM     (CERTIFICATE-MASTER)
003674*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010414' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130343134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003675
003676     GO TO 2070-EXIT.
003677
003678 2070-NOT-FOUND.
003679     MOVE 'N'                        TO  WS-REC-FOUND-SW.
003680
003681 2070-EXIT.
003682     EXIT.
003683     EJECT
003684 2080-CREATE-CERT.
003685     
      * EXEC CICS GETMAIN
003686*         SET       (ADDRESS OF CERTIFICATE-MASTER)
003687*         LENGTH    (CERT-LENGTH)
003688*         INITIMG   (GETMAIN-SPACE)
003689*    END-EXEC.
      *    MOVE ',"IL                  $   #00010428' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130343238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 CERT-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003690
003691     MOVE SAVE-RECORD            TO CERTIFICATE-MASTER.
003692
003693     IF CERTEFFI GREATER THAN LOW-VALUES
003694         MOVE HOLD-EFF           TO CERT-DATE CM-CERT-EFF-DT
003695                                    DC-BIN-DATE-1
003696         MOVE '6'                TO DC-OPTION-CODE
003697         MOVE +1                 TO DC-ELAPSED-MONTHS
003698         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
003699         IF NO-CONVERSION-ERROR
003700             MOVE DC-BIN-DATE-2  TO CM-LOAN-1ST-PMT-DT.
003701
003702     IF CERTACTI GREATER THAN LOW-VALUES
003703         MOVE CERTACTI           TO CERT-ACCOUNT CM-ACCOUNT.
003704     IF CERTSTI GREATER THAN LOW-VALUES
003705         MOVE CERTSTI            TO CERT-STATE CM-STATE.
003706     IF CERTCARI GREATER THAN LOW-VALUES
003707         MOVE CERTCARI           TO CERT-CARRIER CM-CARRIER.
003708     IF CERTGRPI GREATER THAN LOW-VALUES
003709         MOVE CERTGRPI           TO CERT-GROUP CM-GROUPING.
003710     IF CERTI GREATER THAN LOW-VALUES
003711         MOVE CERTI              TO CERT-CERT-PRIME
003712                                    CM-CERT-PRIME
003713         MOVE CM-CERT-NO         TO CM-CERT-NO-A4.
003714
003715     IF SUFXI GREATER THAN LOW-VALUES
003716         MOVE SUFXI              TO CERT-CERT-SUFX
003717                                    CM-CERT-SFX
003718         MOVE CM-CERT-NO         TO CM-CERT-NO-A4.
003719
003720     PERFORM 2200-MOVE-CERT THRU 2200-EXIT.
003721
003722     MOVE LOW-VALUES             TO CM-LAST-MONTH-END.
003723
003724     MOVE SPACES                 TO CM-ENTRY-BATCH
003725                                    CM-LF-EXIT-BATCH
003726                                    CM-AH-EXIT-BATCH
003727                                    CM-CREDIT-INTERFACE-SW-1
003728                                    CM-CREDIT-INTERFACE-SW-2.
003729
003730     IF CM-LF-CURRENT-STATUS = '2' OR '3' OR '4' OR '5' OR
003731                               '9' OR 'D' OR 'V'
003732         MOVE '1'                TO CM-LF-CURRENT-STATUS.
003733
003734     IF CM-AH-CURRENT-STATUS = '2' OR '3' OR '4' OR '5' OR
003735                               '9' OR 'D' OR 'V'
003736         MOVE '1'                TO CM-AH-CURRENT-STATUS.
003737
003738     MOVE '2'                    TO CM-CLAIM-INTERFACE-SW
003739                                    CL-CERT-ORIGIN PI-CERT-SWITCH.
003740     MOVE 1                      TO CM-CLAIM-ATTACHED-COUNT.
003741
003742     
      * EXEC CICS HANDLE CONDITION
003743*         DUPKEY (2080-EXIT)
003744*    END-EXEC.
      *    MOVE '"$$                   ! 4 #00010485' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'3420233030303130343835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003745
003746     
      * EXEC CICS WRITE
003747*         DATASET  (CERT-FILE-ID)
003748*         RIDFLD   (CERT-KEY)
003749*         FROM     (CERTIFICATE-MASTER)
003750*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010489' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130343839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003751
003752 2080-EXIT.
003753     EXIT.
003754     EJECT
003755 2100-MOVE-MSTR.
003756     IF TYPEI GREATER THAN LOW-VALUES
003757         MOVE TYPEI              TO CL-CLAIM-TYPE.
003758
003759     IF STATUSI = LOW-VALUES
003760         GO TO 2100-NO-STATUS-CHANGE.
003761
003762     IF STATUSI = 'OPEN' OR 'O'
003763         IF CL-CLAIM-STATUS = 'O'
003764             MOVE SPACES         TO TRLR-SWITCH
003765                 GO TO 2100-NO-STATUS-CHANGE.
003766
003767     IF STATUSI = 'CLOSED' OR 'C'
003768         IF CL-CLAIM-STATUS = 'C'
003769             MOVE SPACES         TO TRLR-SWITCH
003770                 GO TO 2100-NO-STATUS-CHANGE.
003771
003772     IF PI-COMPANY-ID = 'DMD'
003773        IF STATUSI = 'OPEN' OR 'O'
003774           IF CL-LAST-CLOSE-REASON = 'C' OR 'E'
003775              MOVE ZERO       TO STATUSL
003776              MOVE LOW-VALUES TO STATUSI
003777              GO TO 2100-NO-STATUS-CHANGE.
003778
003779     IF PI-COMPANY-ID = 'DMD'
003780         PERFORM 8000-CREATE-DMO-REC THRU 8000-EXIT.
003781
003782     MOVE STATUSI                TO CL-CLAIM-STATUS.
003783
003784 2100-NO-STATUS-CHANGE.
003785
003786     IF CCNOI GREATER THAN LOW-VALUES
003787         MOVE CCNOI              TO CL-CCN.
003788
003789     IF PROCI GREATER THAN LOW-VALUES
003790         MOVE PROCI              TO CL-PROCESSOR-ID.
003791
003792     if (accswi > low-values)
003793        and (accswi = 'Y' OR 'N')
003794        move accswi              to cl-accident-claim-sw
003795     end-if
003796
003797     if benperi > low-values
003798        and benperi numeric
003799        move benperi             to cl-benefit-period
003800     end-if
003801
003802     if instypei > low-values
003803        move instypei            to cl-insured-type
003804     end-if
003805
003806     IF SEXI GREATER THAN LOW-VALUES
003807         MOVE SEXI               TO CL-INSURED-SEX-CD.
003808
003809     IF BIRTHI GREATER THAN LOW-VALUES
003810         MOVE HOLD-BIRTH         TO CL-INSURED-BIRTH-DT.
003811
003812     IF SOCIALI GREATER THAN LOW-VALUES
003813       AND
003814        SOCIALI = SPACES
003815         MOVE CL-CERT-STATE      TO CL-SSN-STATE
003816         MOVE CL-CERT-ACCOUNT-PRIME   TO CL-SSN-ACCOUNT
003817         MOVE CL-INSURED-LAST-NAME   TO CL-SSN-LN3
003818     ELSE
003819         IF SOCIALI GREATER THAN LOW-VALUES
003820             MOVE SOCIALI        TO CL-SOC-SEC-NO.
003821
003822     IF OCCI GREATER THAN LOW-VALUES
003823         MOVE OCCI               TO CL-INSURED-OCC-CD.
003824
003825     IF BENEL GREATER THAN ZERO
003826         MOVE BENEI              TO CL-BENEFICIARY.
003827
003828     IF DIAGI GREATER THAN LOW-VALUES
003829         MOVE DIAGI              TO WS-DIAGNOSIS.
003830
003831     IF ICD1I GREATER THAN LOW-VALUES
003832         MOVE ICD1I              TO WS-ICD-CODE-1
003833     END-IF.
003834
003835     IF ICD2I GREATER THAN LOW-VALUES
003836         MOVE ICD2I              TO WS-ICD-CODE-2
003837     END-IF.
003838
003839     IF PREMTYPI GREATER THAN LOW-VALUES
003840         MOVE PREMTYPI           TO CL-CLAIM-PREM-TYPE.
003841
003842*    IF CAUSEI GREATER THAN LOW-VALUES
003843*        MOVE CAUSEI             TO CL-CAUSE-CD.
003844
003845*    IF ENDI GREATER THAN LOW-VALUES
003846*        MOVE HOLD-END           TO CL-EST-END-OF-DISAB-DT.
003847
003848     IF PDTHRUI GREATER THAN LOW-VALUES
003849        MOVE HOLD-PDTHRU         TO CL-PAID-THRU-DT.
003850
003851     IF PDAMTL GREATER THAN +0
003852        MOVE HOLD-PDAMT          TO CL-TOTAL-PAID-AMT.
003853
003854     IF NODAYSI GREATER THAN LOW-VALUES
003855        MOVE HOLD-NODAYS         TO CL-NO-OF-DAYS-PAID.
003856
003857     IF NOPMTSI GREATER THAN LOW-VALUES
003858        MOVE HOLD-NOPMTS         TO CL-NO-OF-PMTS-MADE.
003859
003860     IF FORMTYPI GREATER THAN LOW-VALUES
003861        MOVE FORMTYPI            TO CL-PROG-FORM-TYPE.
003862
003863     IF INCI GREATER THAN LOW-VALUES
003864        MOVE HOLD-INCUR          TO CL-INCURRED-DT.
003865
003866     IF REPI GREATER THAN LOW-VALUES
003867        MOVE HOLD-REPORTED       TO CL-REPORTED-DT.
003868
003869     IF ADDONDTI GREATER THAN LOW-VALUES
003870        MOVE HOLD-ADDON          TO CL-LAST-ADD-ON-DT.
003871
003872     IF PRICDI GREATER THAN LOW-VALUES
003873         MOVE PRICDI             TO CL-PRIORITY-CD.
003874
003875     IF CRITPL  > ZEROS
003876        MOVE CRITPI              TO CL-CRITICAL-PERIOD
003877     END-IF
003878
003879     IF EXTENSL  > ZEROS
003880        MOVE EXTENSI             TO CL-NO-OF-EXTENSIONS
003881     END-IF
003882
003883*    IF CRITPTL > ZEROS
003884*       MOVE CRITPTI             TO CL-CRIT-PER-RECURRENT
003885*    END-IF
003886
003887*    IF RTWMOSL > ZEROS
003888*       MOVE RTWMOSI             TO CL-CRIT-PER-RTW-MOS
003889*    END-IF
003890
003891     IF FILETOI GREATER THAN LOW-VALUES
003892         MOVE FILETOI            TO CL-FILE-LOCATION.
003893
003894     IF SUPVI GREATER THAN LOW-VALUES
003895         MOVE SUPVI              TO CL-SUPV-ATTN-CD.
003896
003897     IF MLNAMEI GREATER THAN LOW-VALUES
003898         MOVE MLNAMEI            TO CL-INSURED-LAST-NAME.
003899
003900     IF MFNAMEI GREATER THAN LOW-VALUES
003901         MOVE MFNAMEI            TO CL-INSURED-1ST-NAME.
003902
003903     IF MMINITI GREATER THAN LOW-VALUES
003904         MOVE MMINITI            TO CL-INSURED-MID-INIT.
003905
003906     IF CERTEFFI GREATER THAN LOW-VALUES
003907         MOVE HOLD-EFF           TO CL-CERT-EFF-DT.
003908
003909     IF CERTACTI GREATER THAN LOW-VALUES
003910         MOVE CERTACTI           TO CL-CERT-ACCOUNT.
003911
003912     IF CERTSTI GREATER THAN LOW-VALUES
003913         MOVE CERTSTI            TO CL-CERT-STATE.
003914
003915     IF CERTCARI GREATER THAN LOW-VALUES
003916         MOVE CERTCARI           TO CL-CERT-CARRIER CARRIER-CODE
003917                                    CL-CARRIER.
003918
003919     IF CERTGRPI GREATER THAN LOW-VALUES
003920         MOVE CERTGRPI           TO CL-CERT-GROUPING.
003921
003922     IF CERTI GREATER THAN LOW-VALUES
003923         MOVE CERTI              TO CERT-NO-PRIME.
003924
003925     IF SUFXI GREATER THAN LOW-VALUES
003926         MOVE SUFXI              TO CERT-NO-SUFX.
003927
003928     MOVE CERT-NO                TO CL-CERT-NO
003929                                    CL-CERT-NO-A4.
003930
003931     IF PI-COMPANY-ID = 'FLA'
003932          NEXT SENTENCE
003933     ELSE
003934         MOVE SAVE-BIN-DATE      TO CL-LAST-MAINT-DT
003935         MOVE PI-PROCESSOR-ID    TO CL-LAST-MAINT-USER
003936         MOVE EIBTIME            TO CL-LAST-MAINT-HHMMSS
003937         MOVE '3'                TO CL-LAST-MAINT-TYPE.
003938
003939     IF TRLR-UPDATE-REQUIRED
003940        MOVE CL-CONTROL-PRIMARY TO TRLR-KEY
003941        MOVE ZEROS              TO TRLR-SEQ-NO
003942        
      * EXEC CICS READ
003943*            DATASET   (TRLR-FILE-ID)
003944*            RIDFLD    (TRLR-KEY)
003945*            SET       (ADDRESS OF ACTIVITY-TRAILERS)
003946*       END-EXEC
      *    MOVE '&"S        E          (   #00010685' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303130363835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003947        IF AT-TRAILER-TYPE = '1'
003948           MOVE +1 TO MISC-SUB
003949           PERFORM 2100-BUMP-OPEN-CLOSE-HIST UNTIL
003950           MISC-SUB GREATER THAN +6 OR
003951           AT-OPEN-CLOSE-TYPE (MISC-SUB) = SPACES
003952           IF MISC-SUB GREATER THAN +1
003953              SUBTRACT +1 FROM MISC-SUB
003954              IF AT-OPEN-CLOSE-TYPE (MISC-SUB) = STATUSI
003955                 GO TO 2100-BYPASS-LAST-UPDATE.
003956
003957     IF TRLR-UPDATE-REQUIRED AND
003958        CLAIM-IS-OPEN
003959         MOVE SAVE-BIN-DATE      TO CL-LAST-REOPEN-DT
003960     ELSE
003961         IF TRLR-UPDATE-REQUIRED AND
003962            CLAIM-IS-CLOSED
003963             MOVE SAVE-BIN-DATE  TO CL-LAST-CLOSE-DT
003964             MOVE '4'            TO CL-LAST-CLOSE-REASON.
003965
003966 2100-BYPASS-LAST-UPDATE.
003967     MOVE EMI-FORCABLE-CTR       TO CL-FORCEABLE-ERROR-CNT.
003968     MOVE EMI-FATAL-CTR          TO CL-FATAL-ERROR-CNT.
003969
003970     GO TO 2100-EXIT.
003971
003972 2100-BUMP-OPEN-CLOSE-HIST.
003973     ADD +1                      TO MISC-SUB.
003974
003975 2100-EXIT.
003976     EXIT.
003977     EJECT
003978
003979 2120-check-pdef.
003980
003981     move +0                     to s1
003982     MOVE ZEROS                  TO WS-MONTHS-BETWEEN
003983     move spaces                 to ws-dcc-error-line
003984     PERFORM 3997-GET-ERPDEF  THRU 3997-EXIT
003985     IF ERPDEF-FOUND
003986        perform 2130-check-trlr  thru 2130-exit
003987        move +1                  to s1
003988*       if cl-accident-claim-sw = ' '
003989*          move er-1655             to ws-error-no (s1)
003990*          add +1 to s1
003991*       end-if
003992
003993        MOVE CL-CERT-EFF-DT      TO DC-BIN-DATE-1
003994
003995        MOVE cl-incurred-dt      TO DC-BIN-DATE-2
003996        if (hold-incur > low-values)
003997           and (incl > zeros)
003998           and (hold-incur not = cl-incurred-dt)
003999           move hold-incur       to dc-bin-date-2
004000        end-if
004001        MOVE '1'                 TO DC-OPTION-CODE
004002        MOVE +0                  TO DC-ELAPSED-MONTHS
004003                                    DC-ELAPSED-DAYS
004004        PERFORM 9800-CONVERT-DATE
004005                                 THRU 9800-EXIT
004006        IF NO-CONVERSION-ERROR
004007           MOVE DC-ELAPSED-MONTHS
004008                                 TO WS-MONTHS-BETWEEN
004009           IF DC-ELAPSED-DAYS > 1
004010              ADD 1 TO WS-MONTHS-BETWEEN
004011           END-IF
004012        ELSE
004013           display ' dte conv error ' dc-error-code
004014           MOVE ZEROS            TO WS-MONTHS-BETWEEN
004015        END-IF
004016        IF (WS-EXCL-PERIOD = ZEROS)
004017           OR (WS-MONTHS-BETWEEN = ZEROS)
004018           CONTINUE
004019        ELSE
004020           IF WS-MONTHS-BETWEEN  <= WS-EXCL-PERIOD
004021              MOVE ER-1651       TO ws-error-no (s1)
004022              add +1             to s1
004023           END-IF
004024        END-IF
004025
004026        display ' mos diff ' ws-months-between ' '
004027           ws-pre-exsist
004028        IF (WS-pre-exsist = ZEROS)
004029           OR (WS-MONTHS-BETWEEN = ZEROS)
004030           CONTINUE
004031        ELSE
004032           IF WS-months-between <= WS-pre-exsist
004033              MOVE ER-1677       TO ws-error-no (s1)
004034              add +1             to s1
004035           END-IF
004036        END-IF
004037
004038
004039
004040        IF (WS-COV-ENDS = ZEROS)
004041           OR (WS-MONTHS-BETWEEN = ZEROS)
004042           CONTINUE
004043        ELSE
004044           IF WS-MONTHS-BETWEEN > WS-COV-ENDS
004045              MOVE -1            TO MAINTL
004046              MOVE ER-1653       TO ws-error-no (s1)
004047              add +1 to s1
004048           END-IF
004049        END-IF
004050        IF (WS-ACC-PERIOD = ZEROS)
004051           OR (WS-MONTHS-BETWEEN = ZEROS)
004052           CONTINUE
004053        ELSE
004054           IF (WS-MONTHS-BETWEEN <= WS-ACC-PERIOD)
004055              and (accswi = spaces or low-values)
004056*             and (cl-accident-claim-sw = ' ')
004057              move er-1655       to ws-error-no (s1)
004058              add +1             to s1
004059           end-if
004060           IF WS-MONTHS-BETWEEN <= WS-ACC-PERIOD
004061*             if (cl-accident-claim-sw not = 'Y')
004062              if (accswi not = 'Y')
004063                 MOVE ER-1652    TO ws-error-no (s1)
004064              else
004065                 move er-1662    to ws-error-no (s1)
004066              end-if
004067              MOVE -1            TO MAINTL
004068              add +1             to s1
004069           END-IF
004070        END-IF
004071        if cl-claim-type  = 'I'
004072           move er-1661          to ws-error-no (s1)
004073           add +1                to s1
004074        end-if
004075        perform 2130-check-trlr  thru 2130-exit
004076     END-IF
004077
004078     .
004079 2120-exit.
004080     exit.
004081
004082 2130-check-trlr.
004083
004084     evaluate true
004085        when s1 = +0
004086           move cl-control-primary
004087                                 to trlr-key
004088           MOVE +95              TO TRLR-SEQ-NO
004089           
      * EXEC CICS READ
004090*             update
004091*             DATASET  ('ELTRLR')
004092*             SET      (ADDRESS OF ACTIVITY-TRAILERS)
004093*             RIDFLD   (TRLR-KEY)
004094*             RESP     (WS-RESPONSE)
004095*          END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00010832' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303130383332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004096           if ws-resp-normal
004097              move spaces        to at-info-line-1
004098           end-if
004099        when s1 = +1
004100           if ws-resp-normal
004101              
      * exec cics rewrite
004102*                dataset   ('ELTRLR')
004103*                from      (activity-trailers)
004104*             end-exec
           MOVE LENGTH OF
            activity-trailers
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00010844' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130383434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 activity-trailers, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004105           end-if
004106        when s1 > +1
004107           if ws-resp-normal
004108              move ws-dcc-error-line
004109                                 to at-info-line-1
004110              
      * exec cics rewrite
004111*                dataset   ('ELTRLR')
004112*                from      (activity-trailers)
004113*             end-exec
           MOVE LENGTH OF
            activity-trailers
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00010853' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130383533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 activity-trailers, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004114           else
004115              
      * EXEC CICS GETMAIN
004116*                SET      (ADDRESS OF ACTIVITY-TRAILERS)
004117*                LENGTH   (TRLR-LENGTH)
004118*                INITIMG  (GETMAIN-SPACE)
004119*             END-EXEC
      *    MOVE ',"IL                  $   #00010858' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303130383538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004120              move 'AT'          to at-record-id
004121              move cl-control-primary
004122                                 to at-control-primary
004123              move +95           to at-sequence-no
004124              move '6'           to at-trailer-type
004125              move ws-dcc-error-line
004126                                 to at-info-line-1
004127              move 'E'           to at-info-trailer-type
004128              move save-bin-date to at-recorded-dt
004129                                    at-gen-info-last-maint-dt
004130              move pi-processor-id
004131                                 to at-recorded-by
004132                                    at-gen-info-last-updated-by
004133              move eibtime       to at-last-maint-hhmmss
004134              
      * exec cics write
004135*                dataset   ('ELTRLR')
004136*                from      (activity-trailers)
004137*                ridfld    (trlr-key)
004138*             end-exec
           MOVE LENGTH OF
            activity-trailers
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00010877' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130383737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 activity-trailers, 
                 DFHEIV11, 
                 trlr-key, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004139              perform varying s1 from +1 by +1 until
004140                 ws-error-no (s1) = spaces
004141                 move ws-error-no (s1)
004142                                 to emi-error
004143                 PERFORM 9900-ERROR-FORMAT
004144                                 THRU 9900-EXIT
004145              end-perform
004146           end-if
004147     end-evaluate
004148
004149     .
004150 2130-exit.
004151     exit.
004152
004153 2140-update-elcrtt.
004154
004155     move ' '                    to crtt-switch
004156     MOVE CERT-KEY               TO ELCRTT-KEY
004157     MOVE 'B'                    TO CTRLR-REC-TYPE
004158     
      * EXEC CICS READ
004159*       UPDATE
004160*       DATASET   ('ELCRTT')
004161*       RIDFLD    (ELCRTT-KEY)
004162*       INTO      (CERTIFICATE-TRAILERS)
004163*       RESP      (WS-RESPONSE)
004164*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00010901' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303130393031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004165     IF WS-RESP-NORMAL
004166        perform varying s1 from +1 by +1 until
004167           (s1 > +24)
004168           or (cs-claim-no (s1) = cl-claim-no)
004169        end-perform
004170        if (s1 < +25)
004171           if (benperl <> zeros)
004172              and (cs-benefit-period (s1) not = benperi)
004173              move benperi       to cs-benefit-period (s1)
004174              set crtt-update    to true
004175          end-if
004176          if (instypel <> zeros)
004177             and (instypei not = cs-insured-type (s1))
004178             move instypei      to cs-insured-type (s1)
004179             set crtt-update    to true
004180          end-if
004181          move cs-claim-type (s1) to ws-claim-type
004182          move cs-benefit-period (s1) to ws-ben-per
004183        end-if
004184        if crtt-update
004185           perform 2150-accum    thru 2150-exit
004186           
      * EXEC CICS REWRITE
004187*             DATASET   ('ELCRTT')
004188*             FROM      (CERTIFICATE-TRAILERS)
004189*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&& L                  %   #00010929' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130393239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004190        else
004191           
      * exec cics unlock
004192*             dataset   ('ELCRTT')
004193*          end-exec
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&*                    #   #00010934' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303130393334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004194        end-if
004195     end-if
004196
004197     .
004198 2140-exit.
004199     exit.
004200 2150-accum.
004201
004202     if not erpdef-found
004203        perform 3997-get-erpdef  thru 3997-exit
004204     end-if
004205
004206     if (erpdef-found)
004207        and (ws-max-moben not = zeros)
004208        and (ws-max-moben < cm-ah-benefit-amt)
004209        continue
004210     else
004211        move cm-ah-benefit-amt   to ws-max-moben
004212     end-if
004213
004214     move cm-ah-orig-term        to ws-max-bens
004215     if cl-critical-period not = zeros and spaces
004216        move cl-critical-period  to ws-max-bens
004217     end-if
004218
004219     move zeros to ws-tot-days-paid ws-tot-amt-paid
004220     perform varying s2 from +1 by +1 until
004221        (s2 > +24)
004222        or (cs-claim-no (s2) = spaces)
004223        if (cs-benefit-period (s2) = ws-ben-per)
004224           and (cs-claim-type (s2) = ws-claim-type)
004225           and (cs-insured-type (s2) = cl-insured-type)
004226*          compute ws-tot-days-paid =
004227*             ws-tot-days-paid + cs-days-paid (s2)
004228           compute ws-tot-amt-paid =
004229              ws-tot-amt-paid + cs-total-paid (s2)
004230        end-if
004231     end-perform
004232     if s2 < +25
004233        compute ws-pd-bens rounded =
004234           ws-tot-amt-paid / ws-max-moben
004235        compute cs-remaining-bens (s1) =
004236           ws-max-bens - ws-pd-bens
004237        if cs-remaining-bens (s1) < zeros
004238           move zeros            to cs-remaining-bens (s1)
004239        end-if
004240     end-if
004241
004242     .
004243 2150-exit.
004244     exit.
004245
004246 2200-MOVE-CERT.
004247     IF CRTLNMEI GREATER THAN LOW-VALUES
004248         MOVE CRTLNMEI           TO CM-INSURED-LAST-NAME.
004249
004250     IF CRTFNMEI GREATER THAN LOW-VALUES
004251         MOVE CRTFNMEI           TO CM-INSURED-FIRST-NAME
004252         MOVE CM-INSURED-1ST-INIT TO CM-INSURED-INITIAL1.
004253
004254     IF CRTINITI GREATER THAN LOW-VALUES
004255         MOVE CRTINITI           TO CM-INSURED-INITIAL2.
004256
004257     IF CM-INSURED-INITIALS = SPACES
004258         MOVE '**'               TO CM-INSURED-INITIALS.
004259
004260     IF ISSAGEI GREATER THAN LOW-VALUES
004261         MOVE ISSAGEI            TO CM-INSURED-ISSUE-AGE.
004262
004263     IF JNTFNMEI GREATER THAN LOW-VALUES
004264         MOVE JNTFNMEI           TO CM-JT-FIRST-NAME.
004265
004266     IF JNTLNMEI GREATER THAN LOW-VALUES
004267         MOVE JNTLNMEI           TO CM-JT-LAST-NAME.
004268
004269     IF JNTINITI GREATER THAN LOW-VALUES
004270         MOVE JNTINITI           TO CM-JT-INITIAL.
004271
004272     IF JNTAGEI GREATER THAN LOW-VALUES
004273         MOVE JNTAGEI            TO CM-INSURED-JOINT-AGE.
004274
004275     IF ADDONDTI GREATER THAN LOW-VALUES
004276        MOVE HOLD-ADDON          TO CM-LAST-ADD-ON-DT.
004277
004278     IF LCVCDI GREATER THAN LOW-VALUES
004279        IF LCVCDI = SPACES OR ZEROS
004280           MOVE ZEROS            TO CM-LF-BENEFIT-CD
004281        ELSE
004282           MOVE LCVCDI           TO CM-LF-BENEFIT-CD.
004283
004284     IF LCVOTRMI GREATER THAN LOW-VALUES
004285         MOVE LCVOTRMI           TO CM-LF-ORIG-TERM.
004286
004287     IF LCVRATEL GREATER THAN +0
004288         MOVE HOLD-LF-RATE       TO CM-LF-PREMIUM-RATE.
004289
004290     IF LCVBENEL GREATER THAN +0
004291         MOVE HOLD-LF-CV-BEN     TO CM-LF-BENEFIT-AMT.
004292
004293     IF LCVFORMI GREATER THAN LOW-VALUES
004294         MOVE LCVFORMI           TO CM-POLICY-FORM-NO.
004295
004296     IF LCVCNDTI = LOW-VALUES
004297        GO TO 2210-SET-STATUS.
004298
004299     IF HOLD-LF-CV-CAN NOT = LOW-VALUES
004300        MOVE '8'                 TO CM-LF-CURRENT-STATUS
004301        MOVE HOLD-LF-CV-CAN      TO CM-LF-CANCEL-DT
004302        GO TO 2210-SET-STATUS.
004303
004304     IF CM-LF-CURRENT-STATUS = '7'
004305        MOVE CM-LF-STATUS-AT-DEATH   TO CM-LF-CURRENT-STATUS
004306        MOVE SPACES              TO CM-LF-STATUS-AT-DEATH
004307        MOVE LOW-VALUES          TO CM-LF-DEATH-EXIT-DT
004308                                    CM-LF-DEATH-DT
004309        GO TO 2210-SET-STATUS.
004310
004311     IF CM-LF-CURRENT-STATUS = '8'
004312        MOVE '1'                 TO CM-LF-CURRENT-STATUS
004313        MOVE LOW-VALUES          TO CM-LF-DEATH-EXIT-DT
004314                                    CM-LF-CANCEL-DT.
004315
004316 2210-SET-STATUS.
004317
004318     IF CM-LF-CURRENT-STATUS = SPACES
004319         MOVE '1'                TO CM-LF-CURRENT-STATUS.
004320
004321     IF CM-LF-ORIG-TERM GREATER THAN ZERO
004322         MOVE '6'                TO DC-OPTION-CODE
004323         MOVE CM-LF-ORIG-TERM    TO DC-ELAPSED-MONTHS
004324         MOVE CM-CERT-EFF-DT     TO DC-BIN-DATE-1
004325         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
004326         IF NO-CONVERSION-ERROR
004327             MOVE DC-BIN-DATE-2  TO CM-LF-LOAN-EXPIRE-DT
004328         ELSE
004329             MOVE LOW-VALUES     TO CM-LF-LOAN-EXPIRE-DT
004330     ELSE
004331         MOVE LOW-VALUES         TO CM-LF-LOAN-EXPIRE-DT.
004332
004333 2215-PROCESS-AH-SIDE.
004334
004335     IF ACVCDI GREATER THAN LOW-VALUES
004336        IF ACVCDI = SPACES OR ZEROS
004337           MOVE ZEROS            TO CM-AH-BENEFIT-CD
004338        ELSE
004339           MOVE ACVCDI           TO CM-AH-BENEFIT-CD.
004340
004341     IF ACVOTRMI GREATER THAN LOW-VALUES
004342         MOVE ACVOTRMI           TO CM-AH-ORIG-TERM.
004343
004344     IF ACVRATEL GREATER THAN +0
004345         MOVE HOLD-AH-RATE       TO CM-AH-PREMIUM-RATE.
004346
004347     IF ACVBENEL GREATER THAN +0
004348         MOVE HOLD-AH-CV-BEN     TO CM-AH-BENEFIT-AMT
004349                                    PI-PAYMENT-AMT.
004350
004351     IF ACVFORMI GREATER THAN LOW-VALUES
004352         MOVE ACVFORMI           TO CM-POLICY-FORM-NO.
004353
004354     IF ACVCNDTI = LOW-VALUES
004355        GO TO 2220-SET-STATUS.
004356
004357     IF HOLD-AH-CV-CAN NOT = LOW-VALUES
004358        MOVE '8'                 TO CM-AH-CURRENT-STATUS
004359        MOVE HOLD-AH-CV-CAN      TO CM-AH-CANCEL-DT
004360        GO TO 2220-SET-STATUS.
004361
004362     IF CM-AH-CURRENT-STATUS = '6'
004363        MOVE CM-AH-STATUS-AT-SETTLEMENT
004364                                 TO CM-AH-CURRENT-STATUS
004365        MOVE SPACES              TO CM-AH-STATUS-AT-SETTLEMENT
004366        MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-EXIT-DT
004367                                    CM-AH-SETTLEMENT-DT
004368        GO TO 2220-SET-STATUS.
004369
004370     IF CM-AH-CURRENT-STATUS = '8'
004371        MOVE '1'                 TO CM-AH-CURRENT-STATUS
004372        MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-EXIT-DT
004373                                    CM-AH-CANCEL-DT.
004374
004375 2220-SET-STATUS.
004376     IF CM-AH-CURRENT-STATUS = SPACES
004377         MOVE '1'                TO CM-AH-CURRENT-STATUS.
004378
004379     IF CM-AH-ORIG-TERM GREATER THAN ZERO
004380         MOVE '6'                TO DC-OPTION-CODE
004381         MOVE CM-AH-ORIG-TERM    TO DC-ELAPSED-MONTHS
004382         MOVE CM-CERT-EFF-DT     TO DC-BIN-DATE-1
004383         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
004384         IF NO-CONVERSION-ERROR
004385             MOVE DC-BIN-DATE-2  TO CM-AH-LOAN-EXPIRE-DT
004386         ELSE
004387             MOVE LOW-VALUES     TO CM-AH-LOAN-EXPIRE-DT
004388     ELSE
004389         MOVE LOW-VALUES         TO CM-AH-LOAN-EXPIRE-DT.
004390
004391 2225-FINISH-CERT.
004392     IF APRL GREATER THAN +0
004393         MOVE HOLD-APR           TO CM-LOAN-APR.
004394
004395     IF PMTFREQI GREATER THAN LOW-VALUES
004396         MOVE PMTFREQI           TO CM-PAY-FREQUENCY.
004397
004398     IF INDGRPI GREATER THAN LOW-VALUES
004399         MOVE INDGRPI            TO CM-IND-GRP-TYPE.
004400
004401*    IF LOANNOI GREATER THAN LOW-VALUES
004402*       MOVE LOANNOI             TO CM-LOAN-NUMBER.
004403
004404     IF LOANBALL GREATER THAN +0
004405        MOVE HOLD-LOANBAL        TO CM-LOAN-BALANCE.
004406
004407     IF PREMTYPI GREATER THAN LOW-VALUES
004408         MOVE PREMTYPI           TO CM-PREMIUM-TYPE.
004409
004410     IF REINCDI GREATER THAN LOW-VALUES
004411         MOVE REINCDI            TO CM-SPECIAL-REIN-CODE
004412                                    WS-REIN-1
004413                                    WS-REIN-2
004414                                    WS-REIN-3
004415         MOVE WS-REIN-TABLE      TO CM-REIN-TABLE.
004416
004417     MOVE LOW-VALUES             TO CM-AH-PAID-THRU-DT.
004418
004419     MOVE ZEROS                  TO CM-LOAN-TERM
004420                                    CM-LIFE-COMM-PCT
004421                                    CM-AH-COMM-PCT.
004422
004423*    IF PI-COMPANY-ID = 'CRI' OR 'PEM' OR 'NCL'
004424*      IF CM-PREMIUM-TYPE NOT = '1'  AND
004425*         PI-NO-PMTS = ZEROS
004426*          IF LOANBALL GREATER THAN +0          OR
004427*             ACVBENEL GREATER THAN +0          OR
004428*             APRL     GREATER THAN +0          OR
004429*             LCVRATEL GREATER THAN +0          OR
004430*             ACVRATEL GREATER THAN +0
004431*               PERFORM 6000-CALCULATE-CERT-TERM THRU 6000-EXIT.
004432
004433 2200-EXIT.
004434     EXIT.
004435     EJECT
004436 2300-UPDATE-TRLR.
004437
004438     MOVE ZERO                   TO TRLR-SEQ-NO.
004439     
      * EXEC CICS READ UPDATE
004440*         DATASET  (TRLR-FILE-ID)
004441*         RIDFLD   (TRLR-KEY)
004442*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
004443*    END-EXEC.
      *    MOVE '&"S        EU         (   #00011182' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131313832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004444
004445     MOVE ZERO                   TO COUNT-2.
004446
004447     PERFORM 2310-INS-STATUS THRU 2310-EXIT
004448         UNTIL UPDATE-MADE OR COUNT-2 GREATER THAN 6.
004449
004450     IF NOT UPDATE-MADE
004451         PERFORM 2320-SHIFT-STATUS THRU 2320-EXIT.
004452
004453     
      * EXEC CICS REWRITE
004454*         DATASET  (TRLR-FILE-ID)
004455*         FROM     (ACTIVITY-TRAILERS)
004456*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011196' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131313936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004457
004458 2300-EXIT.
004459     EXIT.
004460
004461 2310-INS-STATUS.
004462     ADD 1                       TO COUNT-2.
004463     IF AT-OPEN-CLOSE-TYPE (COUNT-2) = SPACE
004464         PERFORM 2330-ADD-UPDATE THRU 2330-EXIT
004465         MOVE 'Y'                TO TRLR-SWITCH.
004466
004467 2310-EXIT.
004468     EXIT.
004469
004470 2320-SHIFT-STATUS.
004471     IF AT-OPEN-CLOSE-TYPE (6) = STATUSI
004472        MOVE 6                   TO COUNT-2
004473        GO TO 2320-EXIT.
004474
004475     MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1).
004476     MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2).
004477     MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3).
004478     MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4).
004479     MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5).
004480     MOVE 6       TO COUNT-2.
004481     PERFORM 2330-ADD-UPDATE THRU 2330-EXIT.
004482
004483 2320-EXIT.
004484     EXIT.
004485
004486 2330-ADD-UPDATE.
004487
004488     IF COUNT-2 GREATER THAN 1
004489        COMPUTE MISC-SUB = (COUNT-2 - 1)
004490        IF AT-OPEN-CLOSE-TYPE (MISC-SUB) = STATUSI
004491           GO TO 2330-EXIT.
004492
004493     MOVE SAVE-BIN-DATE         TO AT-OPEN-CLOSE-DATE (COUNT-2)
004494                                   AT-RESERVES-LAST-MAINT-DT
004495
004496     MOVE PI-PROCESSOR-ID       TO AT-RESERVES-LAST-UPDATED-BY
004497     MOVE EIBTIME               TO AT-LAST-MAINT-HHMMSS
004498     MOVE STATUSI               TO AT-OPEN-CLOSE-TYPE (COUNT-2)
004499     MOVE 'ALTER'               TO AT-OPEN-CLOSE-REASON (COUNT-2).
004500
004501 2330-EXIT.
004502     EXIT.
004503
004504     EJECT
004505 2400-UPDATE-TRLR.
004506     
      * EXEC CICS HANDLE CONDITION
004507*         NOTFND    (2400-TRLR-NOTFND)
004508*         ENDFILE   (2410-EXIT)
004509*    END-EXEC.
      *    MOVE '"$I''                  ! 5 #00011249' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'3520233030303131323439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004510
004511     
      * EXEC CICS STARTBR
004512*        DATASET (TRLR-FILE-ID)
004513*        RIDFLD  (TRLR-KEY)
004514*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00011254' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303131323534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004515
004516     
      * EXEC CICS GETMAIN
004517*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
004518*         LENGTH    (TRLR-LENGTH)
004519*         INITIMG   (GETMAIN-SPACE)
004520*    END-EXEC.
      *    MOVE ',"IL                  $   #00011259' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131323539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004521
004522     PERFORM 2410-CHANGE-TRLR THRU 2410-EXIT.
004523
004524     
      * EXEC CICS ENDBR
004525*        DATASET (TRLR-FILE-ID)
004526*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011267' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131323637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004527
004528     GO TO 2400-EXIT.
004529
004530 2400-TRLR-NOTFND.
004531     MOVE ER-0205                TO EMI-ERROR.
004532     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004533     MOVE 'X'                    TO ERROR-SWITCH.
004534
004535 2400-EXIT.
004536     EXIT.
004537
004538     EJECT
004539 2410-CHANGE-TRLR.
004540     
      * EXEC CICS READNEXT
004541*        DATASET (TRLR-FILE-ID)
004542*        RIDFLD  (TRLR-KEY)
004543*        INTO    (ACTIVITY-TRAILERS)
004544*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00011283' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303131323833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV12, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004545
004546     IF AT-CONTROL-PRIMARY = WS-LAST-TRLR-KEY
004547        GO TO 2410-CHANGE-TRLR.
004548
004549     MOVE AT-CONTROL-PRIMARY     TO WS-LAST-TRLR-KEY
004550                                    CHECK-KEY.
004551
004552     IF CHECK-KEY NOT = MSTR-KEY
004553         GO TO 2410-EXIT.
004554
004555     
      * EXEC CICS ENDBR
004556*        DATASET (TRLR-FILE-ID)
004557*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011298' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131323938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004558
004559     
      * EXEC CICS READ UPDATE
004560*        DATASET (TRLR-FILE-ID)
004561*        RIDFLD  (TRLR-KEY)
004562*        INTO    (ACTIVITY-TRAILERS)
004563*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&"IL       EU         (   #00011302' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131333032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004564
004565     
      * EXEC CICS DELETE
004566*        DATASET (TRLR-FILE-ID)
004567*    END-EXEC.
      *    MOVE '&(                    &   #00011308' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303131333038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004568
004569     IF AT-SEQUENCE-NO = ZERO AND
004570        TRLR-UPDATE-REQUIRED
004571         PERFORM 2420-UPDATE-TRLR THRU 2420-EXIT.
004572
004573     IF CERTI GREATER THAN LOW-VALUES
004574        MOVE CERTI               TO AT-CERT-PRIME.
004575
004576     IF SUFXI GREATER THAN LOW-VALUES
004577        MOVE SUFXI               TO AT-CERT-SFX.
004578
004579     IF CERTCARI GREATER THAN LOW-VALUES
004580        MOVE CERTCARI            TO AT-CARRIER.
004581
004582     IF PAYMENT-TR
004583        IF AT-CHECK-QUE-CONTROL NOT = ZEROS
004584           AND AT-CHECK-QUE-CONTROL NOT = 99999999
004585              PERFORM 2460-UPDATE-CHECKQ THRU 2460-EXIT.
004586
004587     IF CORRESPONDENCE-TR
004588        AND AT-LETTER-ARCHIVE-NO NOT = ZEROS
004589           PERFORM 2475-UPDATE-LETTER THRU 2475-EXIT.
004590
004591     
      * EXEC CICS WRITE
004592*         DATASET  (TRLR-FILE-ID)
004593*         RIDFLD   (AT-CONTROL-PRIMARY)
004594*         FROM     (ACTIVITY-TRAILERS)
004595*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00011334' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303131333334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004596
004597     
      * EXEC CICS STARTBR
004598*         DATASET  (TRLR-FILE-ID)
004599*         RIDFLD   (TRLR-KEY)
004600*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00011340' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303131333430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004601
004602     GO TO 2410-CHANGE-TRLR.
004603
004604 2410-EXIT.
004605     EXIT.
004606
004607 2420-UPDATE-TRLR.
004608     MOVE ZERO TO COUNT-2.
004609     PERFORM 2310-INS-STATUS THRU 2310-EXIT
004610         UNTIL UPDATE-MADE OR COUNT-2 GREATER THAN 6.
004611
004612     IF NOT UPDATE-MADE
004613         PERFORM 2320-SHIFT-STATUS THRU 2320-EXIT.
004614
004615 2420-EXIT.
004616     EXIT.
004617
004618     EJECT
004619 2425-UPDATE-NINETY-TRLR.
004620
004621     MOVE +90                    TO TRLR-SEQ-NO.
004622
004623     
      * EXEC CICS HANDLE CONDITION
004624*         NOTFND    (2425-NINETY-NOTFND)
004625*         ENDFILE   (2425-NINETY-NOTFND)
004626*    END-EXEC.
      *    MOVE '"$I''                  ! 6 #00011366' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'3620233030303131333636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004627
004628     
      * EXEC CICS READ UPDATE
004629*        DATASET (TRLR-FILE-ID)
004630*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
004631*        RIDFLD  (TRLR-KEY)
004632*    END-EXEC.
      *    MOVE '&"S        EU         (   #00011371' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131333731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004633
004634     if (diagi > low-values)
004635        and (diagi not = at-info-line-1)
004636        and (cl-total-paid-amt > zeros)
004637        move cl-cert-eff-dt      to dc-bin-date-1
004638        move cl-paid-thru-dt     to dc-bin-date-2
004639        move '1'                 to dc-option-code
004640        perform 9800-convert-date
004641                                 thru 9800-exit
004642        if no-conversion-error
004643           and dc-elapsed-months < +24
004644           and (not emi-bypass-forcables)
004645           move er-1581          to emi-error
004646           perform 9900-error-format
004647                                thru 9900-exit
004648           move -1              to diagl
004649           go to 8110-send-data
004650        end-if
004651     end-if
004652
004653     MOVE AT-INFO-LINE-1        TO WS-DIAGNOSIS.
004654     MOVE AT-ICD-CODE-1         TO WS-ICD-CODE-1.
004655     MOVE AT-ICD-CODE-2         TO WS-ICD-CODE-2.
004656
004657     IF DIAGI GREATER THAN LOW-VALUES
004658         MOVE DIAGI             TO AT-INFO-LINE-1
004659     END-IF.
004660
004661     IF ICD1I GREATER THAN LOW-VALUES
004662         MOVE ICD1I             TO AT-ICD-CODE-1
004663     END-IF.
004664
004665     IF ICD2I GREATER THAN LOW-VALUES
004666         MOVE ICD2I             TO AT-ICD-CODE-2
004667     END-IF.
004668
004669     
      * EXEC CICS REWRITE
004670*        DATASET (TRLR-FILE-ID)
004671*        FROM    (ACTIVITY-TRAILERS)
004672*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011412' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131343132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004673
004674     IF DIAGI GREATER THAN LOW-VALUES
004675       AND DIAGI NOT = WS-DIAGNOSIS
004676         MOVE 'DIAGNOSIS'          TO SPLIT-INFO-DESC
004677         MOVE WS-DIAGNOSIS         TO SPLIT-INFO-OLD
004678         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004679
004680     IF ICD1I GREATER THAN LOW-VALUES
004681       AND ICD1I NOT = WS-ICD-CODE-1
004682         MOVE 'ICD CODE 1'         TO SPLIT-INFO-DESC
004683         MOVE WS-ICD-CODE-1        TO SPLIT-INFO-OLD
004684         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT
004685     END-IF.
004686
004687     IF ICD2I GREATER THAN LOW-VALUES
004688       AND ICD2I NOT = WS-ICD-CODE-2
004689         MOVE 'ICD CODE 2'         TO SPLIT-INFO-DESC
004690         MOVE WS-ICD-CODE-2        TO SPLIT-INFO-OLD
004691         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT
004692     END-IF.
004693
004694     GO TO 2425-EXIT.
004695
004696 2425-NINETY-NOTFND.
004697
004698     
      * EXEC CICS GETMAIN
004699*         SET       (ADDRESS OF ACTIVITY-TRAILERS)
004700*         LENGTH    (TRLR-LENGTH)
004701*         INITIMG   (GETMAIN-SPACE)
004702*    END-EXEC.
      *    MOVE ',"IL                  $   #00011441' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131343431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004703
004704     MOVE +90                  TO TRLR-SEQ-NO.
004705     MOVE TRLR-KEY             TO AT-CONTROL-PRIMARY.
004706
004707     MOVE 'AT'                 TO AT-RECORD-ID
004708     MOVE '6'                  TO AT-TRAILER-TYPE
004709     MOVE SAVE-BIN-DATE        TO AT-RECORDED-DT
004710                                  AT-GEN-INFO-LAST-MAINT-DT
004711     MOVE PI-PROCESSOR-ID      TO AT-RECORDED-BY
004712                                  AT-GEN-INFO-LAST-UPDATED-BY
004713     MOVE EIBTIME              TO AT-LAST-MAINT-HHMMSS
004714     MOVE DIAGI                TO AT-INFO-LINE-1.
004715     MOVE ICD1I                TO AT-ICD-CODE-1.
004716     MOVE ICD2I                TO AT-ICD-CODE-2.
004717
004718     
      * EXEC CICS WRITE
004719*        DATASET (TRLR-FILE-ID)
004720*        FROM    (ACTIVITY-TRAILERS)
004721*        RIDFLD  (TRLR-KEY)
004722*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00011461' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303131343631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004723
004724 2425-EXIT.
004725     EXIT.
004726
004727     EJECT
004728 2430-UPDATE-ACTQ.
004729     
      * EXEC CICS HANDLE CONDITION
004730*         NOTFND    (2430-EXIT)
004731*         ENDFILE   (2440-EXIT)
004732*    END-EXEC.
      *    MOVE '"$I''                  ! 7 #00011472' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'3720233030303131343732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004733
004734     
      * EXEC CICS STARTBR
004735*         DATASET  (ACTQ-FILE-ID)
004736*         RIDFLD   (ACTQ-KEY)
004737*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00011477' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303131343737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004738
004739     
      * EXEC CICS GETMAIN
004740*         SET      (ADDRESS OF ACTIVITY-QUE)
004741*         LENGTH   (ACTQ-LENGTH)
004742*         INITIMG  (GETMAIN-SPACE)
004743*    END-EXEC.
      *    MOVE ',"IL                  $   #00011482' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131343832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACTQ-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004744
004745     PERFORM 2440-CHANGE-ACTQ THRU 2440-EXIT.
004746
004747 2430-EXIT.
004748     EXIT.
004749
004750 2440-CHANGE-ACTQ.
004751
004752     
      * EXEC CICS READNEXT
004753*         DATASET  (ACTQ-FILE-ID)
004754*         RIDFLD   (ACTQ-KEY)
004755*         INTO     (ACTIVITY-QUE)
004756*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00011495' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303131343935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTIVITY-QUE, 
                 DFHEIV12, 
                 ACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004757
004758     
      * EXEC CICS ENDBR
004759*         DATASET (ACTQ-FILE-ID)
004760*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011501' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131353031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004761
004762     IF ACTQ-KEY NOT = MSTR-KEY
004763         GO TO 2440-EXIT.
004764
004765     
      * EXEC CICS READ UPDATE
004766*         DATASET  (ACTQ-FILE-ID)
004767*         RIDFLD   (AQ-CONTROL-PRIMARY)
004768*         SET      (ADDRESS OF ACTIVITY-QUE)
004769*    END-EXEC.
      *    MOVE '&"S        EU         (   #00011508' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131353038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004770
004771     MOVE ACTIVITY-QUE           TO JP-RECORD-AREA
004772
004773     
      * EXEC CICS DELETE
004774*         DATASET (ACTQ-FILE-ID)
004775*    END-EXEC.
      *    MOVE '&(                    &   #00011516' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303131353136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004776
004777     MOVE JP-RECORD-AREA         TO ACTIVITY-QUE.
004778     IF CERTI GREATER THAN LOW-VALUES
004779        MOVE CERTI               TO AQ-CERT-PRIME.
004780     IF SUFXI GREATER THAN LOW-VALUES
004781        MOVE SUFXI               TO AQ-CERT-SFX.
004782     IF CERTCARI GREATER THAN LOW-VALUES
004783        MOVE CERTCARI            TO AQ-CARRIER.
004784
004785     
      * EXEC CICS HANDLE CONDITION
004786*         DUPREC   (2440-EXIT)
004787*    END-EXEC.
      *    MOVE '"$%                   ! 8 #00011528' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'3820233030303131353238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004788
004789     
      * EXEC CICS WRITE
004790*         DATASET  (ACTQ-FILE-ID)
004791*         RIDFLD   (AQ-CONTROL-PRIMARY)
004792*         FROM     (ACTIVITY-QUE)
004793*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00011532' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303131353332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 AQ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004794
004795 2440-EXIT.
004796     EXIT.
004797
004798     EJECT
004799 2460-UPDATE-CHECKQ.
004800     
      * EXEC CICS HANDLE CONDITION
004801*         NOTFND   (2460-EXIT)
004802*    END-EXEC.
      *    MOVE '"$I                   ! 9 #00011543' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3920233030303131353433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004803
004804     MOVE AT-CHECK-QUE-CONTROL   TO CHKQ-CONTROL-NO.
004805     MOVE AT-CHECK-QUE-SEQUENCE  TO CHKQ-SEQ-NO.
004806     MOVE PI-COMPANY-CD          TO CHKQ-COMPANY-CODE.
004807
004808     
      * EXEC CICS READ UPDATE
004809*         DATASET   (CHKQ-FILE-ID)
004810*         RIDFLD    (CHKQ-KEY)
004811*         SET       (ADDRESS OF CHECK-QUE)
004812*    END-EXEC.
      *    MOVE '&"S        EU         (   #00011551' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131353531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CHKQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CHKQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004813
004814     IF CERTI GREATER THAN LOW-VALUES
004815        MOVE CERTI               TO CQ-CERT-PRIME.
004816     IF SUFXI GREATER THAN LOW-VALUES
004817        MOVE SUFXI               TO CQ-CERT-SFX.
004818     IF CERTCARI GREATER THAN LOW-VALUES
004819        MOVE CERTCARI            TO CQ-CARRIER.
004820
004821     
      * EXEC CICS REWRITE
004822*         DATASET  (CHKQ-FILE-ID)
004823*         FROM     (CHECK-QUE)
004824*    END-EXEC.
           MOVE LENGTH OF
            CHECK-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011564' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131353634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CHKQ-FILE-ID, 
                 CHECK-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004825
004826 2460-EXIT.
004827     EXIT.
004828
004829     EJECT
004830 2475-UPDATE-LETTER.
004831     
      * EXEC CICS HANDLE CONDITION
004832*         NOTFND   (2475-EXIT)
004833*    END-EXEC.
      *    MOVE '"$I                   ! : #00011574' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3A20233030303131353734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004834
004835     MOVE AT-LETTER-ARCHIVE-NO   TO ARCH-ARCHIVE-NO.
004836     MOVE '1'                    TO ARCH-RECORD-TYPE.
004837     MOVE ZEROS                  TO ARCH-SEQ-NO.
004838     MOVE PI-COMPANY-CD          TO ARCH-COMPANY-CODE.
004839
004840     
      * EXEC CICS READ UPDATE
004841*         DATASET   (ARCH-FILE-ID)
004842*         RIDFLD    (ARCH-KEY)
004843*         SET       (ADDRESS OF LETTER-ARCHIVE)
004844*    END-EXEC.
      *    MOVE '&"S        EU         (   #00011583' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131353833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004845
004846* COMPARE LETTER HEADER INFORMATION TO ASSURE THAT IT BELONGS
004847* TO THIS MASTER-KEY INFORMATION.
004848
004849     IF LA-CARRIER  = CARRIER-CODE AND
004850        LA-CLAIM-NO = CLAIM-NO     AND
004851        LA-CERT-NO  = CERT-NO
004852        NEXT SENTENCE
004853     ELSE
004854        GO TO 2475-UNLOCK.
004855
004856     IF CERTI GREATER THAN LOW-VALUES
004857        MOVE CERTI               TO LA-CERT-PRIME.
004858     IF SUFXI GREATER THAN LOW-VALUES
004859        MOVE SUFXI               TO LA-CERT-SFX.
004860     IF CERTCARI GREATER THAN LOW-VALUES
004861        MOVE CERTCARI            TO LA-CARRIER.
004862
004863     
      * EXEC CICS REWRITE
004864*         DATASET  (ARCH-FILE-ID)
004865*         FROM     (LETTER-ARCHIVE)
004866*    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011606' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131363036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004867
004868     GO TO 2475-EXIT.
004869
004870 2475-UNLOCK.
004871     
      * EXEC CICS UNLOCK
004872*         DATASET  (ARCH-FILE-ID)
004873*    END-EXEC.
      *    MOVE '&*                    #   #00011614' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131363134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004874
004875 2475-EXIT.
004876     EXIT.
004877     EJECT
004878 2600-CREATE-MAINT-NOTE.
004879     MOVE PI-COMPANY-ID          TO CNTL-CO-ID.
004880     MOVE '1'                    TO CNTL-REC-TYPE.
004881     MOVE SPACES                 TO CNTL-PROC-ID.
004882     MOVE ZERO                   TO CNTL-SEQ-NO.
004883
004884     
      * EXEC CICS READ
004885*         SET        (ADDRESS OF CONTROL-FILE)
004886*         DATASET    ('ELCNTL')
004887*         RIDFLD     (CNTL-KEY)
004888*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00011627' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303131363237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004889
004890     IF CO-NO-USE-AUDIT-CHANGES
004891         GO TO 2600-EXIT.
004892
004893     IF PROCL GREATER ZERO
004894     IF PROCI NOT = LOW-VALUES
004895     IF PROCI NOT = CL-PROCESSOR-ID
004896         MOVE 'PROCESSOR'          TO SPLIT-INFO-DESC
004897         MOVE CL-PROCESSOR-ID      TO SPLIT-INFO-OLD
004898         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004899
004900     IF SEXL GREATER ZERO
004901     IF SEXI NOT = LOW-VALUES
004902     IF SEXI NOT = CL-INSURED-SEX-CD
004903         MOVE 'SEX'                TO SPLIT-INFO-DESC
004904         MOVE CL-INSURED-SEX-CD    TO SPLIT-INFO-OLD
004905         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004906
004907     IF BIRTHL GREATER ZERO
004908     IF HOLD-BIRTH NOT = LOW-VALUES
004909     IF HOLD-BIRTH NOT = CL-INSURED-BIRTH-DT
004910         MOVE 'BIRTH'              TO SPLIT-INFO-DESC
004911         MOVE CL-INSURED-BIRTH-DT  TO DC-BIN-DATE-1
004912         MOVE SPACES               TO DC-OPTION-CODE
004913         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
004914         MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD
004915         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004916
004917     IF SOCIALL GREATER ZERO
004918     IF SOCIALI NOT = LOW-VALUES
004919     IF SOCIALI NOT = CL-SOC-SEC-NO
004920         MOVE 'SOC SEC NO'         TO SPLIT-INFO-DESC
004921         MOVE CL-SOC-SEC-NO        TO SPLIT-INFO-OLD
004922         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004923
004924     IF OCCL GREATER ZERO
004925     IF OCCI NOT = LOW-VALUES
004926     IF OCCI NOT = CL-INSURED-OCC-CD
004927         MOVE 'OCCUPATION'         TO SPLIT-INFO-DESC
004928         MOVE CL-INSURED-OCC-CD    TO SPLIT-INFO-OLD
004929         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004930
004931     IF BENEL GREATER ZERO
004932     IF BENEI NOT = LOW-VALUES
004933     IF BENEI NOT = CL-BENEFICIARY
004934         MOVE 'BENEFICIARY'        TO SPLIT-INFO-DESC
004935         MOVE CL-BENEFICIARY      TO SPLIT-INFO-OLD
004936         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004937
004938     IF PREMTYPL GREATER ZERO
004939     IF PREMTYPI NOT = LOW-VALUES
004940     IF PREMTYPI NOT = CL-CLAIM-PREM-TYPE
004941         MOVE 'PREM TYPE'          TO SPLIT-INFO-DESC
004942         MOVE CL-CLAIM-PREM-TYPE   TO SPLIT-INFO-OLD
004943         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004944
004945*    IF CAUSEL GREATER ZERO
004946*    IF CAUSEI NOT = LOW-VALUES
004947*    IF CAUSEI NOT = CL-CAUSE-CD
004948*        MOVE 'CAUSE CD'           TO SPLIT-INFO-DESC
004949*        MOVE CL-CAUSE-CD          TO SPLIT-INFO-OLD
004950*        PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004951
004952*    IF ENDL GREATER ZERO
004953*    IF HOLD-END NOT = LOW-VALUES
004954*    IF HOLD-END NOT = CL-EST-END-OF-DISAB-DT
004955*        MOVE 'END DT'             TO SPLIT-INFO-DESC
004956*        MOVE CL-EST-END-OF-DISAB-DT
004957*                                  TO DC-BIN-DATE-1
004958*        MOVE SPACES               TO DC-OPTION-CODE
004959*        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
004960*        MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD
004961*        PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004962
004963     IF PDTHRUL GREATER ZERO
004964     IF HOLD-PDTHRU NOT = LOW-VALUES
004965     IF HOLD-PDTHRU NOT = CL-PAID-THRU-DT
004966         MOVE 'PAID THRU'          TO SPLIT-INFO-DESC
004967         MOVE CL-PAID-THRU-DT      TO DC-BIN-DATE-1
004968         MOVE SPACES               TO DC-OPTION-CODE
004969         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
004970         MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD
004971         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004972
004973     IF PDAMTL GREATER ZERO
004974     IF HOLD-PDAMT NOT = ZEROS
004975     IF HOLD-PDAMT NOT = CL-TOTAL-PAID-AMT
004976         MOVE 'PAID AMOUNT'        TO SPLIT-INFO-DESC
004977         MOVE CL-TOTAL-PAID-AMT    TO WS-EDIT-AMT
004978         MOVE WS-EDIT-AMT          TO SPLIT-INFO-OLD
004979         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004980
004981     IF NODAYSL GREATER ZERO
004982     IF HOLD-NODAYS NOT = ZEROS
004983     IF HOLD-NODAYS NOT = CL-NO-OF-DAYS-PAID
004984         MOVE 'DAYS PAID'          TO SPLIT-INFO-DESC
004985         MOVE CL-NO-OF-DAYS-PAID   TO WS-EDIT-NUMBER
004986         MOVE WS-EDIT-NUMBER       TO SPLIT-INFO-OLD
004987         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004988
004989     IF NOPMTSL GREATER ZERO
004990     IF HOLD-NOPMTS NOT = ZEROS
004991     IF HOLD-NOPMTS NOT = CL-NO-OF-PMTS-MADE
004992         MOVE 'PMTS MADE'          TO SPLIT-INFO-DESC
004993         MOVE CL-NO-OF-PMTS-MADE   TO WS-EDIT-NUMBER
004994         MOVE WS-EDIT-NUMBER       TO SPLIT-INFO-OLD
004995         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
004996
004997     IF FORMTYPL GREATER ZERO
004998     IF FORMTYPI NOT = LOW-VALUES
004999     IF FORMTYPI NOT = CL-PROG-FORM-TYPE
005000         MOVE 'FORM TYPE'          TO SPLIT-INFO-DESC
005001         MOVE CL-PROG-FORM-TYPE    TO SPLIT-INFO-OLD
005002         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005003
005004     IF REPL GREATER ZERO
005005     IF HOLD-REPORTED NOT = LOW-VALUES
005006     IF HOLD-REPORTED NOT = CL-REPORTED-DT
005007         MOVE 'REPORTED DT'        TO SPLIT-INFO-DESC
005008         MOVE CL-REPORTED-DT       TO DC-BIN-DATE-1
005009         MOVE SPACES               TO DC-OPTION-CODE
005010         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
005011         MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD
005012         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005013
005014     IF ADDONDTL GREATER ZERO
005015     IF HOLD-ADDON NOT = LOW-VALUES
005016     IF HOLD-ADDON NOT = CL-LAST-ADD-ON-DT
005017         MOVE 'ADD ON DT'          TO SPLIT-INFO-DESC
005018         MOVE CL-LAST-ADD-ON-DT    TO DC-BIN-DATE-1
005019         MOVE SPACES               TO DC-OPTION-CODE
005020         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
005021         MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD
005022         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005023
005024     IF PRICDL GREATER ZERO
005025     IF PRICDI NOT = LOW-VALUES
005026     IF PRICDI NOT = CL-PRIORITY-CD
005027         MOVE 'PRIORITY CODE'      TO SPLIT-INFO-DESC
005028         MOVE CL-PRIORITY-CD       TO SPLIT-INFO-OLD
005029         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005030
005031     IF FILETOL GREATER ZERO
005032     IF FILETOI NOT = LOW-VALUES
005033     IF FILETOI NOT = CL-FILE-LOCATION
005034         MOVE 'FILE TO'            TO SPLIT-INFO-DESC
005035         MOVE CL-FILE-LOCATION     TO SPLIT-INFO-OLD
005036         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005037
005038     IF SUPVL GREATER ZERO
005039     IF SUPVI NOT = LOW-VALUES
005040     IF SUPVI NOT = CL-SUPV-ATTN-CD
005041         MOVE 'SUPV (Y/N)'         TO SPLIT-INFO-DESC
005042         MOVE CL-SUPV-ATTN-CD      TO SPLIT-INFO-OLD
005043         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005044
005045*    IF (CRITPTL > ZERO)
005046*       AND (CRITPTI NOT = LOW-VALUES)
005047*       AND (CRITPTI NOT = CL-CRIT-PER-RECURRENT)
005048*       MOVE 'CRIT PER RECURR'   TO SPLIT-INFO-DESC
005049*       MOVE CL-CRIT-PER-RECURRENT
005050*                                TO SPLIT-INFO-OLD
005051*       PERFORM 2650-WRITE-MAINT-NOTE
005052*                                THRU 2650-EXIT
005053*    END-IF
005054
005055*    IF (RTWMOSL > ZERO)
005056*       AND (RTWMOSI NOT = LOW-VALUES)
005057*       AND (RTWMOSI NOT = CL-CRIT-PER-RTW-MOS)
005058*       MOVE 'CRIT PER RTW MO'   TO SPLIT-INFO-DESC
005059*       MOVE CL-CRIT-PER-RTW-MOS TO SPLIT-INFO-OLD
005060*       PERFORM 2650-WRITE-MAINT-NOTE
005061*                                THRU 2650-EXIT
005062*    END-IF
005063
005064     IF MLNAMEL GREATER ZERO
005065     IF MLNAMEI NOT = LOW-VALUES
005066     IF MLNAMEI NOT = CL-INSURED-LAST-NAME
005067         MOVE 'INS LAST NAME'      TO SPLIT-INFO-DESC
005068         MOVE CL-INSURED-LAST-NAME TO SPLIT-INFO-OLD
005069         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005070
005071     IF MFNAMEL GREATER ZERO
005072     IF MFNAMEI NOT = LOW-VALUES
005073     IF MFNAMEI NOT = CL-INSURED-1ST-NAME
005074         MOVE 'INS 1ST NAME'       TO SPLIT-INFO-DESC
005075         MOVE CL-INSURED-1ST-NAME  TO SPLIT-INFO-OLD
005076         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005077
005078     IF MMINITL GREATER ZERO
005079     IF MMINITI NOT = LOW-VALUES
005080     IF MMINITI NOT = CL-INSURED-MID-INIT
005081         MOVE 'INS MID INITL'      TO SPLIT-INFO-DESC
005082         MOVE CL-INSURED-MID-INIT  TO SPLIT-INFO-OLD
005083         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005084
005085     IF CERTEFFL GREATER ZERO
005086     IF HOLD-EFF NOT = LOW-VALUES
005087     IF HOLD-EFF NOT = CL-CERT-EFF-DT
005088         MOVE 'CERT EFF DT'        TO SPLIT-INFO-DESC
005089         MOVE CL-CERT-EFF-DT       TO DC-BIN-DATE-1
005090         MOVE SPACES               TO DC-OPTION-CODE
005091         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
005092         MOVE DC-GREG-DATE-1-EDIT  TO SPLIT-INFO-OLD
005093         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005094
005095     IF CERTACTL GREATER ZERO
005096     IF CERTACTI NOT = LOW-VALUES
005097     IF CERTACTI NOT = CL-CERT-ACCOUNT
005098         MOVE 'CERT ACCOUNT'       TO SPLIT-INFO-DESC
005099         MOVE CL-CERT-ACCOUNT      TO SPLIT-INFO-OLD
005100         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005101
005102     IF CERTSTL GREATER ZERO
005103     IF CERTSTI NOT = LOW-VALUES
005104     IF CERTSTI NOT = CL-CERT-STATE
005105         MOVE 'CERT STATE'         TO SPLIT-INFO-DESC
005106         MOVE CL-CERT-STATE        TO SPLIT-INFO-OLD
005107         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005108
005109     IF CERTCARL GREATER ZERO
005110     IF CERTCARI NOT = LOW-VALUES
005111     IF CERTCARI NOT = CL-CERT-CARRIER
005112         MOVE 'CERT CARRIER'       TO SPLIT-INFO-DESC
005113         MOVE CL-CERT-CARRIER      TO SPLIT-INFO-OLD
005114         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005115
005116     IF CERTGRPL GREATER ZERO
005117     IF CERTGRPI NOT = LOW-VALUES
005118     IF CERTGRPI NOT = CL-CERT-GROUPING
005119         MOVE 'CERT GROUPING'      TO SPLIT-INFO-DESC
005120         MOVE CL-CERT-GROUPING     TO SPLIT-INFO-OLD
005121         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005122
005123     IF CERTL GREATER ZERO OR
005124        SUFXL GREATER ZERO
005125     IF (CERTI NOT = LOW-VALUES AND
005126                     CL-CERT-PRIME)
005127                OR
005128        (SUFXI NOT = LOW-VALUES AND
005129                     CL-CERT-SFX)
005130         MOVE 'CERT NO'            TO SPLIT-INFO-DESC
005131         MOVE CL-CERT-NO           TO SPLIT-INFO-OLD
005132         PERFORM 2650-WRITE-MAINT-NOTE THRU 2650-EXIT.
005133
005134     IF incl > zeros
005135        IF (HOLD-INCUR NOT = LOW-VALUES)
005136           AND (HOLD-INCUR NOT = CL-INCURRED-DT)
005137           MOVE 'INC DTE'        TO SPLIT-INFO-DESC
005138           MOVE CL-INCURRED-DT   TO DC-BIN-DATE-1
005139           MOVE SPACES           TO DC-OPTION-CODE
005140           PERFORM 9800-CONVERT-DATE
005141                                 THRU 9800-EXIT
005142           MOVE DC-GREG-DATE-1-EDIT
005143                                 TO SPLIT-INFO-OLD
005144           PERFORM 2650-WRITE-MAINT-NOTE
005145                                 THRU 2650-EXIT
005146        END-IF
005147     END-IF
005148
005149     IF PI-STATE = 'VA' OR 'PA' OR 'GA'
005150       IF (REPL GREATER ZERO
005151         AND HOLD-REPORTED NOT = LOW-VALUES
005152         AND HOLD-REPORTED NOT = CL-REPORTED-DT)
005153        OR (incl > zeros
005154          AND (HOLD-INCUR NOT = LOW-VALUES)
005155             AND (HOLD-INCUR NOT = CL-INCURRED-DT))
005156          PERFORM 2610-CHECK-2-YEAR-CONTESTABLE THRU 2610-EXIT
005157       END-IF
005158     END-IF
005159
005160     if incl > zeros
005161        move cl-incurred-dt      to ws-prev-inc-dt
005162        perform 7990-get-lo-hi-acct-dates
005163                                 thru 7990-exit
005164        if (hold-incur >= ws-hi-acct-dt)
005165           and (acct-cancelled)
005166           and (mob-cert)
005167           MOVE er-1682          TO EMI-ERROR
005168           MOVE -1               TO INCL
005169           MOVE AL-UABON         TO INCA
005170           PERFORM 9900-ERROR-FORMAT
005171                                 THRU 9900-EXIT
005172           PERFORM 2620-CHECK-TRLR
005173                                 THRU 2620-EXIT
005174        end-if
005175        if hold-incur < cm-cert-eff-dt
005176           MOVE er-1683          TO EMI-ERROR
005177           MOVE -1               TO INCL
005178           MOVE AL-UABON         TO INCA
005179           PERFORM 9900-ERROR-FORMAT
005180                                 THRU 9900-EXIT
005181           PERFORM 2620-CHECK-TRLR
005182                                 THRU 2620-EXIT
005183        end-if
005184     end-if
005185
005186*052918IF incl > zeros
005187*052918  AND PI-COMPANY-ID = 'CID'
005188*052918   IF (HOLD-INCUR NOT = LOW-VALUES)
005189*052918      AND (HOLD-INCUR NOT = CL-INCURRED-DT)
005190*052918      IF TYPEI = PI-AH-OVERRIDE-L1
005191*052918         SET ELAPSED-BETWEEN-BIN TO TRUE
005192*052918         MOVE ZERO               TO DC-ELAPSED-MONTHS
005193*052918                                    DC-ELAPSED-DAYS
005194*052918
005195*052918         MOVE HOLD-INCUR  TO DC-BIN-DATE-1
005196*052918         MOVE SAVE-BIN-DATE TO DC-BIN-DATE-2
005197*052918         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
005198*052918         IF DC-ODD-DAYS-OVER > ZERO
005199*052918            ADD 1 TO DC-ELAPSED-MONTHS
005200*052918         END-IF
005201*052918
005202*052918         IF PI-STATE = 'HI'
005203*052918           AND DC-ELAPSED-MONTHS <= 18
005204*052918            CONTINUE
005205*052918         ELSE
005206*052918            IF DC-ELAPSED-MONTHS > 15
005207*052918               MOVE ER-7572            TO EMI-ERROR
005208*052918               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005209*052918            END-IF
005210*052918         END-IF
005211*052918         PERFORM 2620-CHECK-TRLR THRU 2620-EXIT
005212*052918      END-IF
005213*052918   END-IF
005214*052918END-IF
005215
005216     .
005217 2600-EXIT.
005218      EXIT.
005219 2610-CHECK-2-YEAR-CONTESTABLE.
005220     SET ELAPSED-BETWEEN-BIN TO TRUE
005221     MOVE ZERO               TO DC-ELAPSED-MONTHS
005222                                DC-ELAPSED-DAYS
005223
005224     MOVE HOLD-EFF TO DC-BIN-DATE-1.
005225     IF HOLD-INCUR > SPACES
005226        MOVE HOLD-INCUR TO DC-BIN-DATE-2
005227     ELSE
005228        MOVE CL-INCURRED-DT TO DC-BIN-DATE-2
005229     END-IF
005230     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
005231     IF DC-ODD-DAYS-OVER > ZERO
005232        ADD 1 TO DC-ELAPSED-MONTHS
005233     END-IF
005234
005235     IF DC-ELAPSED-MONTHS <= 24
005236        IF HOLD-REPORTED > SPACES
005237           MOVE HOLD-REPORTED TO DC-BIN-DATE-2
005238        ELSE
005239           MOVE CL-REPORTED-DT TO DC-BIN-DATE-2
005240        END-IF
005241        MOVE ZERO           TO DC-ELAPSED-MONTHS
005242                               DC-ELAPSED-DAYS
005243        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
005244        IF DC-ODD-DAYS-OVER > ZERO
005245           ADD 1 TO DC-ELAPSED-MONTHS
005246        END-IF
005247        IF DC-ELAPSED-MONTHS > 24
005248           MOVE ER-1679            TO EMI-ERROR
005249           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005250        ELSE
005251           MOVE 1                  TO EMI-ERROR
005252        END-IF
005253     END-IF.
005254
005255     PERFORM 2615-CHECK-TRLR THRU 2615-EXIT.
005256
005257 2610-EXIT.
005258     EXIT.
005259 2615-CHECK-TRLR.
005260
005261     MOVE CL-CONTROL-PRIMARY   TO TRLR-KEY
005262     MOVE +96              TO TRLR-SEQ-NO
005263     
      * EXEC CICS READ
005264*       UPDATE
005265*       DATASET  ('ELTRLR')
005266*       SET      (ADDRESS OF ACTIVITY-TRAILERS)
005267*       RIDFLD   (TRLR-KEY)
005268*       RESP     (WS-RESPONSE)
005269*    END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00012006' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303132303036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005270     IF WS-RESP-NORMAL
005271        IF EMI-ERROR =  ER-1679
005272           IF EMI-ERROR-NUMBER(1) = ER-1679
005273              MOVE EMI-LINE1 TO AT-INFO-LINE-1
005274           ELSE
005275           IF EMI-ERROR-NUMBER(2) = ER-1679
005276              MOVE EMI-LINE2 TO AT-INFO-LINE-1
005277           ELSE
005278           IF EMI-ERROR-NUMBER(3) = ER-1679
005279              MOVE EMI-LINE3 TO AT-INFO-LINE-1
005280           END-IF
005281           END-IF
005282           END-IF
005283           
      * EXEC CICS REWRITE
005284*             DATASET   ('ELTRLR')
005285*             FROM      (ACTIVITY-TRAILERS)
005286*          END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00012026' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303132303236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005287        ELSE
005288           IF EMI-ERROR = 1
005289              MOVE WS-CONTEST-NOTE TO AT-INFO-LINE-1
005290              MOVE ZERO TO EMI-ERROR
005291              
      * EXEC CICS REWRITE
005292*                DATASET   ('ELTRLR')
005293*                FROM      (ACTIVITY-TRAILERS)
005294*             END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&& L                  %   #00012034' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303132303334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005295           ELSE
005296              
      * EXEC CICS DELETE
005297*                DATASET   ('ELTRLR')
005298*             END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&(                    &   #00012039' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303132303339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005299           END-IF
005300        END-IF
005301     ELSE
005302     IF EMI-ERROR =  ER-1679
005303        
      * EXEC CICS GETMAIN
005304*          SET      (ADDRESS OF ACTIVITY-TRAILERS)
005305*          LENGTH   (TRLR-LENGTH)
005306*          INITIMG  (GETMAIN-SPACE)
005307*       END-EXEC
      *    MOVE ',"IL                  $   #00012046' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303132303436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005308        MOVE 'AT'          TO AT-RECORD-ID
005309        MOVE CL-CONTROL-PRIMARY
005310                           TO AT-CONTROL-PRIMARY
005311        MOVE +96           TO AT-SEQUENCE-NO
005312        MOVE '6'           TO AT-TRAILER-TYPE
005313        MOVE SPACES             TO AT-GENERAL-INFO-TR
005314        INITIALIZE AT-GENERAL-INFO-TR
005315        IF EMI-ERROR-NUMBER(1) = ER-1679
005316           MOVE EMI-LINE1 TO AT-INFO-LINE-1
005317        ELSE
005318        IF EMI-ERROR-NUMBER(2) = ER-1679
005319           MOVE EMI-LINE2 TO AT-INFO-LINE-1
005320        ELSE
005321        IF EMI-ERROR-NUMBER(3) = ER-1679
005322           MOVE EMI-LINE3 TO AT-INFO-LINE-1
005323        END-IF
005324        END-IF
005325        END-IF
005326        MOVE SPACE           TO AT-INFO-TRAILER-TYPE
005327        MOVE SAVE-BIN-DATE TO AT-RECORDED-DT
005328                              AT-GEN-INFO-LAST-MAINT-DT
005329        MOVE PI-PROCESSOR-ID
005330                           TO AT-RECORDED-BY
005331                              AT-GEN-INFO-LAST-UPDATED-BY
005332        MOVE EIBTIME       TO AT-LAST-MAINT-HHMMSS
005333        
      * EXEC CICS WRITE
005334*          DATASET   ('ELTRLR')
005335*          FROM      (ACTIVITY-TRAILERS)
005336*          RIDFLD    (TRLR-KEY)
005337*       END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00012076' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303132303736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005338     END-IF
005339     END-IF
005340     .
005341 2615-EXIT.
005342     EXIT.
005343
005344 2620-CHECK-TRLR.
005345
005346     MOVE SPACES                 TO AT-GENERAL-INFO-TR
005347     INITIALIZE AT-GENERAL-INFO-TR
005348     MOVE MSTR-KEY               TO AT-CONTROL-PRIMARY
005349     MOVE 'AT'                   TO AT-RECORD-ID
005350     evaluate true
005351        when emi-error = er-1679
005352           move er-1679-text     to at-info-line-1
005353        when emi-error = er-1682
005354           move er-1682-text     to at-info-line-1
005355        when emi-error = er-1683
005356           move er-1683-text     to at-info-line-1
005357     end-evaluate
005358     move +97                    to at-sequence-no
005359     MOVE '6'                    TO AT-TRAILER-TYPE
005360     MOVE save-bin-date          TO AT-RECORDED-DT
005361                                    AT-GEN-INFO-LAST-MAINT-DT
005362     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
005363                                    AT-GEN-INFO-LAST-UPDATED-BY
005364     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
005365
005366     MOVE SPACES                 TO AT-INFO-LINE-2
005367
005368     .
005369 2620-WRITE.
005370
005371     
      * EXEC CICS HANDLE CONDITION
005372*        DUPREC    (2620-DUPREC)
005373*    END-EXEC.
      *    MOVE '"$%                   ! ; #00012114' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'3B20233030303132313134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005374
005375     
      * EXEC CICS WRITE
005376*         DATASET     ('ELTRLR')
005377*         FROM        (ACTIVITY-TRAILERS)
005378*         RIDFLD      (AT-CONTROL-PRIMARY)
005379*     END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00012118' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303132313138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005380
005381     GO TO 2620-EXIT
005382
005383     .
005384 2620-DUPREC.
005385
005386     SUBTRACT +1 FROM AT-SEQUENCE-NO
005387     GO TO 2620-WRITE
005388
005389     .
005390 2620-EXIT.
005391     EXIT.
005392
005393
005394*0529182620-CHECK-TRLR.
005395*052918
005396*052918MOVE CL-CONTROL-PRIMARY   TO TRLR-KEY
005397*052918MOVE +97                  TO TRLR-SEQ-NO
005398*052918EXEC CICS READ
005399*052918   UPDATE
005400*052918   DATASET  ('ELTRLR')
005401*052918   SET      (ADDRESS OF ACTIVITY-TRAILERS)
005402*052918   RIDFLD   (TRLR-KEY)
005403*052918   RESP     (WS-RESPONSE)
005404*052918END-EXEC
005405*052918IF WS-RESP-NORMAL
005406*052918   IF EMI-ERROR =  ER-7572
005407*052918      MOVE WS-FILING-NOTE TO AT-INFO-LINE-1
005408*052918      EXEC CICS REWRITE
005409*052918         DATASET   ('ELTRLR')
005410*052918         FROM      (ACTIVITY-TRAILERS)
005411*052918      END-EXEC
005412*052918   ELSE
005413*052918      MOVE WS-FILE-LIM-NOTE TO AT-INFO-LINE-1
005414*052918         EXEC CICS REWRITE
005415*052918            DATASET   ('ELTRLR')
005416*052918            FROM      (ACTIVITY-TRAILERS)
005417*052918         END-EXEC
005418*052918   END-IF
005419*052918ELSE
005420*052918   IF EMI-ERROR =  ER-7572
005421*052918      EXEC CICS GETMAIN
005422*052918         SET      (ADDRESS OF ACTIVITY-TRAILERS)
005423*052918         LENGTH   (TRLR-LENGTH)
005424*052918         INITIMG  (GETMAIN-SPACE)
005425*052918      END-EXEC
005426*052918      MOVE 'AT'          TO AT-RECORD-ID
005427*052918      MOVE CL-CONTROL-PRIMARY
005428*052918                         TO AT-CONTROL-PRIMARY
005429*052918      MOVE +97           TO AT-SEQUENCE-NO
005430*052918      MOVE '6'           TO AT-TRAILER-TYPE
005431*052918      MOVE SPACES             TO AT-GENERAL-INFO-TR
005432*052918      INITIALIZE AT-GENERAL-INFO-TR
005433*052918      MOVE WS-FILING-NOTE TO AT-INFO-LINE-1
005434*052918      MOVE SPACE           TO AT-INFO-TRAILER-TYPE
005435*052918      MOVE SAVE-BIN-DATE TO AT-RECORDED-DT
005436*052918                            AT-GEN-INFO-LAST-MAINT-DT
005437*052918      MOVE PI-PROCESSOR-ID
005438*052918                         TO AT-RECORDED-BY
005439*052918                            AT-GEN-INFO-LAST-UPDATED-BY
005440*052918      MOVE EIBTIME       TO AT-LAST-MAINT-HHMMSS
005441*052918      EXEC CICS WRITE
005442*052918         DATASET   ('ELTRLR')
005443*052918         FROM      (ACTIVITY-TRAILERS)
005444*052918         RIDFLD    (TRLR-KEY)
005445*052918      END-EXEC
005446*052918   END-IF
005447*052918END-IF
005448*052918.
005449*0529182620-EXIT.
005450*052918EXIT.
005451
005452
005453 2650-WRITE-MAINT-NOTE.
005454     
      * EXEC CICS GETMAIN
005455*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
005456*         LENGTH   (TRLR-LENGTH)
005457*         INITIMG  (GETMAIN-SPACE)
005458*    END-EXEC.
      *    MOVE ',"IL                  $   #00012197' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303132313937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005459
005460     
      * EXEC CICS HANDLE CONDITION
005461*         DUPREC   (2650-EXIT)
005462*    END-EXEC.
      *    MOVE '"$%                   ! < #00012203' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'3C20233030303132323033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005463
005464     MOVE SPACES             TO ACTIVITY-TRAILERS.
005465
005466     IF CERTL GREATER ZERO
005467         MOVE CERTI          TO CL-CERT-PRIME.
005468
005469     IF SUFXL GREATER ZERO
005470         MOVE SUFXI          TO CL-CERT-SFX.
005471
005472     MOVE CL-CONTROL-PRIMARY TO AT-CONTROL-PRIMARY.
005473
005474     SUBTRACT 1 FROM  CL-TRAILER-SEQ-CNT.
005475     MOVE CL-TRAILER-SEQ-CNT TO AT-SEQUENCE-NO.
005476
005477     MOVE 'AT'               TO AT-RECORD-ID.
005478
005479     MOVE '6'                TO AT-TRAILER-TYPE.
005480     MOVE SAVE-BIN-DATE      TO AT-RECORDED-DT
005481                                AT-GEN-INFO-LAST-MAINT-DT.
005482     MOVE PI-PROCESSOR-ID    TO AT-RECORDED-BY
005483                                AT-GEN-INFO-LAST-UPDATED-BY.
005484     MOVE EIBTIME            TO AT-LAST-MAINT-HHMMSS.
005485     MOVE SPLIT-INFO-LINE-1  TO AT-INFO-LINE-1.
005486     MOVE 'M'                TO AT-INFO-TRAILER-TYPE.
005487
005488     
      * EXEC CICS WRITE
005489*         DATASET  (TRLR-FILE-ID)
005490*         RIDFLD   (AT-CONTROL-PRIMARY)
005491*         FROM     (ACTIVITY-TRAILERS)
005492*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00012231' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303132323331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005493
005494 2650-EXIT.
005495      EXIT.
005496
005497 2800-CHECK-AUTO-PAY.
005498
005499     MOVE PI-COMPANY-CD      TO TRLR-COMPANY-CD.
005500     MOVE PI-CARRIER         TO TRLR-CARRIER.
005501     MOVE PI-CLAIM-NO        TO TRLR-CLAIM-NO.
005502     MOVE PI-CERT-NO         TO TRLR-CERT-NO.
005503     MOVE PI-AUTO-PAY-SEQ    TO TRLR-SEQ-NO.
005504
005505     
      * EXEC CICS HANDLE CONDITION
005506*         NOTFND    (2800-EXIT)
005507*         ENDFILE   (2800-EXIT)
005508*    END-EXEC.
      *    MOVE '"$I''                  ! = #00012248' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'3D20233030303132323438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005509
005510     
      * EXEC CICS READ
005511*        DATASET (TRLR-FILE-ID)
005512*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
005513*        RIDFLD  (TRLR-KEY)
005514*    END-EXEC.
      *    MOVE '&"S        E          (   #00012253' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132323533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005515
005516     IF BENEFICIARY-PAID-AUTO
005517         MOVE 'Y'              TO AUTO-PAY-TO-BENE
005518     END-IF.
005519
005520 2800-EXIT.
005521      EXIT.
005522
005523     EJECT
005524 3000-DELETE-CLAIM.
005525     IF  NOT MODIFY-CAP
005526         MOVE ER-0070            TO EMI-ERROR
005527         MOVE -1                 TO MAINTL
005528         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005529         GO TO 8110-SEND-DATA.
005530
005531     
      * EXEC CICS READ
005532*         SET      (ADDRESS OF CLAIM-MASTER)
005533*         DATASET  (CLMS-FILE-ID)
005534*         RIDFLD   (MSTR-KEY)
005535*    END-EXEC.
      *    MOVE '&"S        E          (   #00012274' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132323734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005536
005537     IF CL-1ST-TRL-AVAIL
005538         PERFORM 3010-DEL-MSTR-TRLR THRU 3010-EXIT
005539     ELSE
005540        MOVE MSTR-KEY            TO TRLR-MAIN-KEY
005541        MOVE 0                   TO TRLR-SEQ-NO
005542         
      * EXEC CICS STARTBR
005543*            DATASET (TRLR-FILE-ID)
005544*            RIDFLD  (TRLR-KEY)
005545*        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012285' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303132323835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005546         PERFORM 3060-READ-TRLR THRU 3060-EXIT
005547         
      * EXEC CICS ENDBR
005548*            DATASET (TRLR-FILE-ID)
005549*        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00012290' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303132323930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005550        IF SCREEN-ERROR
005551            MOVE ER-0208         TO EMI-ERROR
005552            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005553            MOVE -1              TO MAINTL
005554            GO TO 8110-SEND-DATA
005555        ELSE
005556            PERFORM 3010-DEL-MSTR-TRLR THRU 3010-EXIT.
005557
005558     IF SCREEN-ERROR
005559         MOVE -1                 TO MAINTL
005560         GO TO 8110-SEND-DATA.
005561
005562     MOVE -1                     TO ONE-OR-MIN1.
005563     PERFORM 7700-CHECK-SEQUENCE THRU 7799-EXIT.
005564
005565     IF WS-ASSOC-CERT-TOTAL NOT = ZERO
005566         MOVE CL-CONTROL-PRIMARY TO MSTR-KEY
005567                                    WS-SAVE-CLAIM-KEY
005568         MOVE +1                 TO ONE-OR-MIN1
005569         PERFORM 7710-RESEQUENCE-CLAIMS THRU 7799-EXIT.
005570
005571     MOVE 'D'                    TO PI-CLAIM-DELETED-SWITCH.
005572
005573     MOVE LOW-VALUES             TO EL131AO.
005574     MOVE ER-0000                TO EMI-ERROR.
005575     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
005576     MOVE 'X'                    TO PI-RETURN-CD-1.
005577     MOVE -1                     TO MAINTL.
005578     GO TO 8100-SEND-MAP.
005579
005580 3010-DEL-MSTR-TRLR.
005581     
      * EXEC CICS READ UPDATE
005582*         SET       (ADDRESS OF CLAIM-MASTER)
005583*         DATASET   (CLMS-FILE-ID)
005584*         RIDFLD    (MSTR-KEY)
005585*    END-EXEC.
      *    MOVE '&"S        EU         (   #00012324' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303132333234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005586
005587     if (cl-priority-cd = '8')
005588        and (pi-processor-id not = 'PEMA' and 'JMS '
005589             AND 'AMWA')
005590        MOVE ER-8003             TO EMI-ERROR
005591        PERFORM 9900-ERROR-FORMAT
005592                                 THRU 9900-EXIT
005593        MOVE -1                  TO MAINTL
005594        GO TO 8110-SEND-DATA
005595     end-if
005596
005597     MOVE MSTR-KEY               TO TRLR-MAIN-KEY ACTQ-KEY.
005598     MOVE ZERO                   TO TRLR-SEQ-NO.
005599
005600     
      * EXEC CICS HANDLE CONDITION
005601*         NOTFND    (3010-DEL-ACTQ)
005602*         ENDFILE   (3040-EXIT)
005603*    END-EXEC.
      *    MOVE '"$I''                  ! > #00012343' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'3E20233030303132333433' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005604
005605     PERFORM 3040-DELETE-TRLR THRU 3040-EXIT.
005606
005607 3010-DEL-ACTQ.
005608
005609     
      * EXEC CICS HANDLE CONDITION
005610*         NOTFND    (3010-NO-ACTIVITY)
005611*         ENDFILE   (3050-EXIT)
005612*    END-EXEC.
      *    MOVE '"$I''                  ! ? #00012352' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'3F20233030303132333532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005613
005614     
      * EXEC CICS STARTBR
005615*         DATASET  (ACTQ-FILE-ID)
005616*         RIDFLD   (ACTQ-KEY)
005617*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012357' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303132333537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005618
005619     
      * EXEC CICS GETMAIN
005620*         SET      (ADDRESS OF ACTIVITY-QUE)
005621*         LENGTH   (ACTQ-LENGTH)
005622*         INITIMG  (GETMAIN-SPACE)
005623*    END-EXEC.
      *    MOVE ',"IL                  $   #00012362' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303132333632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACTQ-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005624
005625     PERFORM 3050-DELETE-ACTQ THRU 3050-EXIT.
005626
005627     
      * EXEC CICS ENDBR
005628*         DATASET (ACTQ-FILE-ID)
005629*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00012370' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303132333730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005630
005631 3010-NO-ACTIVITY.
005632
005633     
      * EXEC CICS HANDLE CONDITION
005634*        NOTFND   (3010-CERT-NOT-FOUND)
005635*    END-EXEC.
      *    MOVE '"$I                   ! @ #00012376' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'4020233030303132333736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005636
005637     MOVE CL-COMPANY-CD          TO CERT-COMPANY-CODE.
005638     MOVE CL-CERT-CARRIER        TO CERT-CARRIER.
005639     MOVE CL-CERT-GROUPING       TO CERT-GROUP.
005640     MOVE CL-CERT-STATE          TO CERT-STATE.
005641     MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
005642     MOVE CL-CERT-EFF-DT         TO CERT-DATE.
005643     MOVE CL-CERT-NO             TO CERT-CERT.
005644
005645     
      * EXEC CICS READ
005646*        DATASET   (CERT-FILE-ID)
005647*        RIDFLD    (CERT-KEY)
005648*        SET       (ADDRESS OF CERTIFICATE-MASTER)
005649*    END-EXEC.
      *    MOVE '&"S        E          (   #00012388' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132333838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005650
005651     IF CERT-WAS-CREATED AND CM-CLAIM-ATTACHED-COUNT = 1
005652       AND PI-COMPANY-ID NOT EQUAL 'AHL' AND 'FNL'
005653         PERFORM 3030-DELETE-CERT THRU 3030-EXIT
005654     ELSE
005655         PERFORM 3020-UPDATE-CERT THRU 3020-EXIT.
005656
005657 3010-UPDATE-CERT-TRLR.
005658
005659     MOVE CERT-KEY               TO ELCRTT-KEY
005660     MOVE 'B'                    TO CTRLR-REC-TYPE
005661     
      * EXEC CICS READ
005662*       UPDATE
005663*       DATASET   ('ELCRTT')
005664*       RIDFLD    (ELCRTT-KEY)
005665*       INTO      (CERTIFICATE-TRAILERS)
005666*       RESP      (WS-RESPONSE)
005667*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00012404' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303132343034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005668     IF WS-RESP-NORMAL
005669        perform varying s1 from +1 by +1 until
005670           (s1 > +24)
005671           or (cs-claim-no (s1) = cl-claim-no)
005672        end-perform
005673        if s1 < +25
005674           move spaces           to cs-claim-no (s1)
005675                                    cs-claim-type (s1)
005676                                    cs-insured-type (s1)
005677           move zeros            to cs-days-paid (s1)
005678                                    cs-total-paid (s1)
005679                                    cs-benefit-period (s1)
005680           if s1 not = +24
005681              compute s2 = s1 + +1
005682              perform 3015-bump  thru 3015-exit
005683           end-if
005684           
      * EXEC CICS REWRITE
005685*             DATASET   ('ELCRTT')
005686*             FROM      (CERTIFICATE-TRAILERS)
005687*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&& L                  %   #00012427' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303132343237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005688        end-if
005689     end-if
005690
005691     .
005692 3010-CERT-NOT-FOUND.
005693
005694     
      * EXEC CICS DELETE
005695*        DATASET  (CLMS-FILE-ID)
005696*    END-EXEC.
      *    MOVE '&(                    &   #00012437' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303132343337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005697
005698 3010-EXIT.
005699      EXIT.
005700
005701 3015-bump.
005702
005703     perform varying s2 from s2 by +1 until s2 > +24
005704        move cs-mb-claim-data (s2)
005705                                 to cs-mb-claim-data (s1)
005706        move spaces              to cs-mb-claim-data (s2)
005707        add +1 to s1
005708     end-perform
005709
005710     .
005711 3015-exit.
005712     exit.
005713
005714 3020-UPDATE-CERT.
005715     
      * EXEC CICS READ UPDATE
005716*         DATASET  (CERT-FILE-ID)
005717*         RIDFLD   (CERT-KEY)
005718*         SET      (ADDRESS OF CERTIFICATE-MASTER)
005719*    END-EXEC.
      *    MOVE '&"S        EU         (   #00012458' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303132343538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005720
005721     SUBTRACT 1       FROM CM-CLAIM-ATTACHED-COUNT.
005722
005723     
      * EXEC CICS HANDLE CONDITION
005724*         DUPKEY   (3020-EXIT)
005725*    END-EXEC.
      *    MOVE '"$$                   ! A #00012466' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'4120233030303132343636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005726
005727     
      * EXEC CICS REWRITE
005728*         DATASET  (CERT-FILE-ID)
005729*         FROM     (CERTIFICATE-MASTER)
005730*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00012470' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303132343730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005731
005732 3020-EXIT.
005733     EXIT.
005734
005735 3030-DELETE-CERT.
005736     
      * EXEC CICS READ UPDATE
005737*         DATASET  (CERT-FILE-ID)
005738*         RIDFLD   (CERT-KEY)
005739*         SET      (ADDRESS OF CERTIFICATE-MASTER)
005740*    END-EXEC.
      *    MOVE '&"S        EU         (   #00012479' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303132343739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005741
005742     
      * EXEC CICS DELETE
005743*         DATASET   (CERT-FILE-ID)
005744*    END-EXEC.
      *    MOVE '&(                    &   #00012485' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303132343835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005745
005746 3030-EXIT.
005747     EXIT.
005748
005749 3040-DELETE-TRLR.
005750     
      * EXEC CICS READ
005751*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
005752*         DATASET  (TRLR-FILE-ID)
005753*         RIDFLD   (TRLR-KEY)
005754*         GTEQ
005755*    END-EXEC.
      *    MOVE '&"S        G          (   #00012493' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303132343933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005756
005757     IF PI-COMPANY-CD = AT-COMPANY-CD
005758        MOVE AT-CONTROL-PRIMARY  TO TRLR-KEY
005759        IF TRLR-MAIN-KEY GREATER THAN CL-CONTROL-PRIMARY
005760           GO TO 3040-EXIT
005761        ELSE
005762           NEXT SENTENCE
005763     ELSE
005764        GO TO 3040-EXIT.
005765
005766     
      * EXEC CICS DELETE
005767*         DATASET  (TRLR-FILE-ID)
005768*         RIDFLD   (TRLR-KEY)
005769*    END-EXEC.
      *    MOVE '&(  R                 &   #00012509' TO DFHEIV0
           MOVE X'262820205220202020202020' &
                X'202020202020202020202620' &
                X'2020233030303132353039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005770
005771     GO TO 3040-DELETE-TRLR.
005772
005773 3040-EXIT.
005774     EXIT.
005775
005776 3050-DELETE-ACTQ.
005777     
      * EXEC CICS READNEXT
005778*         DATASET  (ACTQ-FILE-ID)
005779*         RIDFLD   (ACTQ-KEY)
005780*         INTO     (ACTIVITY-QUE)
005781*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00012520' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303132353230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTIVITY-QUE, 
                 DFHEIV12, 
                 ACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005782
005783     IF ACTQ-KEY GREATER THAN CL-CONTROL-PRIMARY
005784         GO TO 3050-EXIT.
005785
005786     
      * EXEC CICS ENDBR
005787*         DATASET  (ACTQ-FILE-ID)
005788*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00012529' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303132353239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005789
005790     
      * EXEC CICS DELETE
005791*         DATASET   (ACTQ-FILE-ID)
005792*         RIDFLD    (ACTQ-KEY)
005793*    END-EXEC.
      *    MOVE '&(  R                 &   #00012533' TO DFHEIV0
           MOVE X'262820205220202020202020' &
                X'202020202020202020202620' &
                X'2020233030303132353333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005794
005795     
      * EXEC CICS STARTBR
005796*         DATASET   (ACTQ-FILE-ID)
005797*         RIDFLD    (ACTQ-KEY)
005798*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00012538' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303132353338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTQ-FILE-ID, 
                 ACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005799
005800     GO TO 3050-DELETE-ACTQ.
005801
005802 3050-EXIT.
005803     EXIT.
005804     EJECT
005805 3060-READ-TRLR.
005806     
      * EXEC CICS HANDLE CONDITION
005807*         NOTFND    (3060-EXIT)
005808*         ENDFILE   (3060-EXIT)
005809*    END-EXEC.
      *    MOVE '"$I''                  ! B #00012549' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'4220233030303132353439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005810
005811 3060-LOOP.
005812     
      * EXEC CICS READNEXT
005813*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
005814*         DATASET  (TRLR-FILE-ID)
005815*         RIDFLD   (TRLR-KEY)
005816*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00012555' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303132353535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005817
005818     IF TRLR-MAIN-KEY GREATER THAN CL-CONTROL-PRIMARY
005819         GO TO 3060-EXIT.
005820
005821     IF FORM-CONTROL-TR
005822         IF AT-RECORDED-DT = SAVE-BIN-DATE
005823             NEXT SENTENCE
005824         ELSE
005825             MOVE 'X'            TO ERROR-SWITCH
005826             GO TO 3060-EXIT
005827     ELSE
005828         IF PAYMENT-TR OR CORRESPONDENCE-TR
005829            MOVE 'X'                 TO ERROR-SWITCH
005830            GO TO 3060-EXIT.
005831
005832     GO TO 3060-LOOP.
005833
005834 3060-EXIT.
005835     EXIT.
005836     EJECT
005837
005838 3997-GET-ERPDEF.
005839
005840     move cl-company-cd          to cert-company-code
005841     move cl-cert-key-data       to cert-key (2:21)
005842     move cl-cert-no             to cert-cert
005843
005844     
      * EXEC CICS READ
005845*        DATASET   (CERT-FILE-ID)
005846*        RIDFLD    (CERT-KEY)
005847*        SET       (ADDRESS OF CERTIFICATE-MASTER)
005848*        resp      (ws-response)
005849*    END-EXEC
      *    MOVE '&"S        E          (  N#00012587' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303132353837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005850
005851     MOVE SPACES                 TO WS-CRIT-PER-ALPHA
005852                                    WS-ERPDEF-SW
005853     MOVE ZEROS                  TO WS-CRITICAL-PERIOD
005854                                    WS-CRIT-PER-RTW-MOS
005855
005856     if cm-clp-state = spaces or low-values or zeros
005857        move cm-state            to cm-clp-state
005858     end-if
005859     MOVE PI-COMPANY-CD          TO ERPDEF-KEY
005860     MOVE CM-clp-state           TO ERPDEF-STATE
005861     MOVE am-dcc-product-code    TO ERPDEF-PROD-CD
005862
005863     evaluate true
005864        when (cl-claim-type = 'L' or 'P' OR 'O')
005865           and (cm-lf-benefit-cd not = '00' and '  ' and 'DD'
005866             and 'CU')
005867           move 'L'              to erpdef-ben-type
005868           move cm-lf-benefit-cd to erpdef-ben-code
005869        when (cl-claim-type not = 'L' and 'P' AND 'O')
005870           and (cm-ah-benefit-cd not = '00' and '  ')
005871           move 'A'              to erpdef-ben-type
005872           move cm-ah-benefit-cd to erpdef-ben-code
005873        when (cl-claim-type not = 'L' and 'P' AND 'O')
005874           and (cm-ah-benefit-cd = '00' or '  ')
005875           move 'L'              to erpdef-ben-type
005876           move cm-lf-benefit-cd to erpdef-ben-code
005877        when (cl-claim-type = 'L' or 'P' OR 'O')
005878           and (cm-lf-benefit-cd = '00' or '  ' or 'DD' or 'CU')
005879           move 'A'              to erpdef-ben-type
005880           move cm-ah-benefit-cd to erpdef-ben-code
005881        when other
005882           move 'A'              to erpdef-ben-type
005883           move cm-ah-benefit-cd to erpdef-ben-code
005884     end-evaluate
005885
005886*    if (cl-claim-type = 'L' or 'P')
005887*       and (cm-lf-benefit-cd not = '00' and '  ' and 'DD'
005888*          and 'CU')
005889*       move 'L'                 to erpdef-ben-type
005890*       move cm-lf-benefit-cd    to erpdef-ben-code
005891*    else
005892*       MOVE 'A'                 TO ERPDEF-BEN-TYPE
005893*       MOVE CM-AH-BENEFIT-CD    TO ERPDEF-BEN-CODE
005894*    end-if
005895
005896     move cl-insured-birth-dt    to dc-bin-date-1
005897     move cl-incurred-dt         to dc-bin-date-2
005898     move '1'                    to dc-option-code
005899     PERFORM 9800-CONVERT-DATE   THRU 9800-EXIT
005900     compute ws-att-age =
005901        dc-elapsed-months / 12
005902
005903     MOVE '6'                    TO DC-OPTION-CODE
005904     move zeros                  to dc-elapsed-months
005905                                    dc-elapsed-days
005906     move low-values to dc-bin-date-1 dc-bin-date-2
005907
005908     MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
005909     MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
005910
005911     
      * EXEC CICS STARTBR
005912*        DATASET  ('ERPDEF')
005913*        RIDFLD   (ERPDEF-KEY)
005914*        GTEQ
005915*        RESP     (WS-RESPONSE)
005916*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00012654' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303132363534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005917
005918     IF WS-RESP-NORMAL
005919        
      * EXEC CICS READNEXT
005920*          DATASET  ('ERPDEF')
005921*          INTO     (PRODUCT-MASTER)
005922*          RIDFLD   (ERPDEF-KEY)
005923*          RESP     (WS-RESPONSE)
005924*       END-EXEC
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV12
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00012662' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303132363632' TO DFHEIV0
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
005925
005926        IF WS-RESP-NORMAL
005927           IF (ERPDEF-KEY-SAVE (1:16) =
005928              PD-CONTROL-PRIMARY (1:16))
005929              AND (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
005930
005931              PERFORM VARYING A1 FROM +1 BY +1 UNTIL
005932                 (A1 > +11)
005933                 OR ((PD-PROD-CODE (A1) = cl-claim-type)
005934                  AND (PD-MAX-ATT-AGE (a1) >= WS-ATT-AGE))
005935              END-PERFORM
005936              IF A1 < +12
005937
005938                 SET ERPDEF-FOUND TO TRUE
005939                 MOVE PD-CRIT-PERIOD (A1)
005940                              TO WS-CRITICAL-PERIOD
005941                 MOVE PD-REC-CRIT-PERIOD (A1)
005942                              TO WS-CRIT-PER-RECURRENT
005943                 IF PD-RTW-MOS (A1) NUMERIC
005944                    MOVE PD-RTW-MOS (A1)
005945                              TO WS-CRIT-PER-RTW-MOS
005946                 ELSE
005947                    MOVE 0    TO WS-CRIT-PER-RTW-MOS
005948                 END-IF
005949                 IF PD-EXCLUSION-PERIOD-DAYS (A1) NUMERIC
005950                    MOVE PD-EXCLUSION-PERIOD-DAYS (A1)
005951                                 TO WS-EXCL-PERIOD
005952                 END-IF
005953                 IF PD-COVERAGE-ENDS-MOS (A1) NUMERIC
005954                    MOVE PD-COVERAGE-ENDS-MOS (A1)
005955                                 TO WS-COV-ENDS
005956                 END-IF
005957                 IF PD-ACCIDENT-ONLY-MOS (A1) NUMERIC
005958                    MOVE PD-ACCIDENT-ONLY-MOS (A1)
005959                                 TO WS-ACC-PERIOD
005960                 END-IF
005961                 if pd-max-extension (a1) numeric
005962                    move pd-max-extension (a1)
005963                                 to ws-max-extension
005964                 end-if
005965                 if pd-max-amt (a1) numeric
005966                    move pd-max-amt (a1)
005967                                 to ws-max-moben
005968                 end-if
005969                 if pd-pre-exist-excl-type(a1) numeric
005970                    move pd-pre-exist-excl-type(a1)
005971                                 to ws-pre-exsist
005972                 end-if
005973              END-IF
005974           END-IF
005975        END-IF
005976     END-IF
005977
005978     .
005979 3997-EXIT.
005980     EXIT.
005981
005982 4000-FORCE-ERRORS.
005983
005984     IF PI-RETURN-CD-1 = 'X'
005985         MOVE ER-0311            TO EMI-ERROR
005986         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005987         MOVE -1                 TO MAINTL
005988         GO TO 8110-SEND-DATA.
005989
005990     IF NOT FORCE-CAP
005991         MOVE ER-0416            TO EMI-ERROR
005992         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005993         MOVE -1                 TO MAINTL
005994         GO TO 8110-SEND-DATA.
005995
005996     MOVE 'F'                    TO EMI-ACTION-SWITCH.
005997     PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.
005998     IF EMI-FATAL-CTR GREATER THAN ZERO
005999         GO TO 4000-FATAL-ERRORS.
006000
006001     PERFORM 2000-UPDATE-CLAIM THRU 2000-EXIT.
006002
006003     IF EMI-FATAL-CTR GREATER THAN ZERO
006004         GO TO 4000-FATAL-ERRORS.
006005
006006     PERFORM 5000-BUILD-MAP THRU 5000-EXIT.
006007
006008     IF EMI-FATAL-CTR GREATER THAN ZERO
006009         GO TO 4000-FATAL-ERRORS.
006010
006011     MOVE SPACE                  TO EMI-ACTION-SWITCH MAINTO.
006012     MOVE AL-UANOF               TO CERTA.
006013     MOVE ER-0000 TO EMI-ERROR.
006014     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006015
006016 4000-FATAL-ERRORS.
006017     MOVE -1                     TO MAINTL.
006018     GO TO 8110-SEND-DATA.
006019     EJECT
006020
006021 4100-SAVE-CHANGES.
006022     IF MAINTL > 0
006023         MOVE MAINTI  TO WS-SAVE-MAINTI
006024         MOVE MAINTL  TO WS-SAVE-MAINTL
006025     END-IF
006026     IF STATUSL > 0
006027         MOVE STATUSI  TO WS-SAVE-STATUSI
006028         MOVE STATUSL  TO WS-SAVE-STATUSL
006029     END-IF
006030     IF REPL > 0
006031         MOVE REPI  TO WS-SAVE-REPI
006032         MOVE REPL  TO WS-SAVE-REPL
006033     END-IF
006034*    IF CAUSEL > 0
006035*        MOVE CAUSEI  TO WS-SAVE-CAUSEI
006036*        MOVE CAUSEL  TO WS-SAVE-CAUSEL
006037*    END-IF
006038*    IF ENDL > 0
006039*        MOVE ENDI  TO WS-SAVE-ENDI
006040*        MOVE ENDL  TO WS-SAVE-ENDL
006041*    END-IF
006042     IF DIAGL > 0
006043         MOVE DIAGI  TO WS-SAVE-DIAGI
006044         MOVE DIAGL  TO WS-SAVE-DIAGL
006045     END-IF
006046     IF ICD1L > 0
006047         MOVE ICD1I  TO WS-SAVE-ICD1I
006048         MOVE ICD1L  TO WS-SAVE-ICD1L
006049     END-IF
006050     IF ICD2L > 0
006051         MOVE ICD2I  TO WS-SAVE-ICD2I
006052         MOVE ICD2L  TO WS-SAVE-ICD2L
006053     END-IF
006054     IF BENEL > 0
006055         MOVE BENEI  TO WS-SAVE-BENEI
006056         MOVE BENEL  TO WS-SAVE-BENEL
006057     END-IF
006058     IF BIRTHL > 0
006059         MOVE BIRTHI  TO WS-SAVE-BIRTHI
006060         MOVE BIRTHL  TO WS-SAVE-BIRTHL
006061     END-IF
006062     IF SOCIALL > 0
006063         MOVE SOCIALI  TO WS-SAVE-SOCIALI
006064         MOVE SOCIALL  TO WS-SAVE-SOCIALL
006065     END-IF
006066     IF SEXL > 0
006067         MOVE SEXI  TO WS-SAVE-SEXI
006068         MOVE SEXL  TO WS-SAVE-SEXL
006069     END-IF
006070     IF MLNAMEL > 0
006071         MOVE MLNAMEI  TO WS-SAVE-MLNAMEI
006072         MOVE MLNAMEL  TO WS-SAVE-MLNAMEL
006073     END-IF
006074     IF MFNAMEL > 0
006075         MOVE MFNAMEI  TO WS-SAVE-MFNAMEI
006076         MOVE MFNAMEL  TO WS-SAVE-MFNAMEL
006077     END-IF
006078     IF MMINITL > 0
006079         MOVE MMINITI  TO WS-SAVE-MMINITI
006080         MOVE MMINITL  TO WS-SAVE-MMINITL
006081     END-IF
006082*    IF LOANNOL > 0
006083*        MOVE LOANNOI  TO WS-SAVE-LOANNOI
006084*        MOVE LOANNOL  TO WS-SAVE-LOANNOL
006085*    END-IF
006086     IF LOANBALL > 0
006087         MOVE LOANBALI  TO WS-SAVE-LOANBALI
006088         MOVE LOANBALL  TO WS-SAVE-LOANBALL
006089     END-IF
006090     IF PROCL > 0
006091         MOVE PROCI  TO WS-SAVE-PROCI
006092         MOVE PROCL  TO WS-SAVE-PROCL
006093     END-IF
006094     IF SUPVL > 0
006095         MOVE SUPVI  TO WS-SAVE-SUPVI
006096         MOVE SUPVL  TO WS-SAVE-SUPVL
006097     END-IF
006098     IF PRICDL > 0
006099         MOVE PRICDI  TO WS-SAVE-PRICDI
006100         MOVE PRICDL  TO WS-SAVE-PRICDL
006101     END-IF
006102     IF FILETOL > 0
006103         MOVE FILETOI  TO WS-SAVE-FILETOI
006104         MOVE FILETOL  TO WS-SAVE-FILETOL
006105     END-IF
006106     IF PDTHRUL > 0
006107         MOVE PDTHRUI  TO WS-SAVE-PDTHRUI
006108         MOVE PDTHRUL  TO WS-SAVE-PDTHRUL
006109     END-IF
006110     IF PDAMTL > 0
006111         MOVE PDAMTI  TO WS-SAVE-PDAMTI
006112         MOVE PDAMTL  TO WS-SAVE-PDAMTL
006113     END-IF
006114     IF NODAYSL > 0
006115         MOVE NODAYSI  TO WS-SAVE-NODAYSI
006116         MOVE NODAYSL  TO WS-SAVE-NODAYSL
006117     END-IF
006118     IF NOPMTSL > 0
006119         MOVE NOPMTSI  TO WS-SAVE-NOPMTSI
006120         MOVE NOPMTSL  TO WS-SAVE-NOPMTSL
006121     END-IF
006122     IF FORMTYPL > 0
006123         MOVE FORMTYPI  TO WS-SAVE-FORMTYPI
006124         MOVE FORMTYPL  TO WS-SAVE-FORMTYPL
006125     END-IF
006126     IF OCCL > 0
006127         MOVE OCCI  TO WS-SAVE-OCCI
006128         MOVE OCCL  TO WS-SAVE-OCCL
006129     END-IF.
006130 4100-EXIT.
006131      EXIT.
006132
006133 4200-LOAD-CHANGES.
006134     IF WS-SAVE-MAINTL > 0
006135         MOVE WS-SAVE-MAINTI  TO MAINTI
006136         MOVE WS-SAVE-MAINTL  TO MAINTL
006137         MOVE AL-UANON        TO MAINTA
006138     END-IF
006139     IF WS-SAVE-STATUSL > 0
006140         MOVE WS-SAVE-STATUSI  TO STATUSI
006141         MOVE WS-SAVE-STATUSL  TO STATUSL
006142         MOVE AL-UANON         TO STATUSA
006143     END-IF
006144     IF WS-SAVE-REPL > 0
006145         MOVE WS-SAVE-REPI  TO REPI
006146         MOVE WS-SAVE-REPL  TO REPL
006147         MOVE AL-UANON      TO REPA
006148     END-IF
006149*    IF WS-SAVE-CAUSEL > 0
006150*        MOVE WS-SAVE-CAUSEI  TO CAUSEI
006151*        MOVE WS-SAVE-CAUSEL  TO CAUSEL
006152*        MOVE AL-UANON        TO CAUSEA
006153*    END-IF
006154*    IF WS-SAVE-ENDL > 0
006155*        MOVE WS-SAVE-ENDI  TO ENDI
006156*        MOVE WS-SAVE-ENDL  TO ENDL
006157*        MOVE AL-UANON      TO ENDA
006158*    END-IF
006159     IF WS-SAVE-DIAGL > 0
006160         MOVE WS-SAVE-DIAGI  TO DIAGI
006161         MOVE WS-SAVE-DIAGL  TO DIAGL
006162         MOVE AL-UANON       TO DIAGA
006163     END-IF
006164     IF WS-SAVE-ICD1L > 0
006165         MOVE WS-SAVE-ICD1I  TO ICD1I
006166         MOVE WS-SAVE-ICD1L  TO ICD1L
006167         MOVE AL-UANON       TO ICD1A
006168     END-IF
006169     IF WS-SAVE-ICD2L > 0
006170         MOVE WS-SAVE-ICD2I  TO ICD2I
006171         MOVE WS-SAVE-ICD2L  TO ICD2L
006172         MOVE AL-UANON       TO ICD2A
006173     END-IF
006174     IF WS-SAVE-BENEL > 0
006175         MOVE WS-SAVE-BENEI  TO BENEI
006176         MOVE WS-SAVE-BENEL  TO BENEL
006177         MOVE AL-UANON       TO BENEA
006178     END-IF
006179     IF WS-SAVE-BIRTHL > 0
006180         MOVE WS-SAVE-BIRTHI  TO BIRTHI
006181         MOVE WS-SAVE-BIRTHL  TO BIRTHL
006182         MOVE AL-UANON        TO BIRTHA
006183     END-IF
006184     IF WS-SAVE-SOCIALL > 0
006185         MOVE WS-SAVE-SOCIALI  TO SOCIALI
006186         MOVE WS-SAVE-SOCIALL  TO SOCIALL
006187         MOVE AL-UANON         TO SOCIALA
006188     END-IF
006189     IF WS-SAVE-SEXL > 0
006190         MOVE WS-SAVE-SEXI  TO SEXI
006191         MOVE WS-SAVE-SEXL  TO SEXL
006192         MOVE AL-UANON      TO SEXA
006193     END-IF
006194     IF WS-SAVE-MLNAMEL > 0
006195         MOVE WS-SAVE-MLNAMEI  TO MLNAMEI
006196         MOVE WS-SAVE-MLNAMEL  TO MLNAMEL
006197         MOVE AL-UANON         TO MLNAMEA
006198     END-IF
006199     IF WS-SAVE-MFNAMEL > 0
006200         MOVE WS-SAVE-MFNAMEI  TO MFNAMEI
006201         MOVE WS-SAVE-MFNAMEL  TO MFNAMEL
006202         MOVE AL-UANON         TO MFNAMEA
006203     END-IF
006204     IF WS-SAVE-MMINITL > 0
006205         MOVE WS-SAVE-MMINITI  TO MMINITI
006206         MOVE WS-SAVE-MMINITL  TO MMINITL
006207         MOVE AL-UANON         TO MMINITA
006208     END-IF
006209*    IF WS-SAVE-LOANNOL > 0
006210*        MOVE WS-SAVE-LOANNOI  TO LOANNOI
006211*        MOVE WS-SAVE-LOANNOL  TO LOANNOL
006212*        MOVE AL-UANON         TO LOANNOA
006213*    END-IF
006214     IF WS-SAVE-LOANBALL > 0
006215         MOVE WS-SAVE-LOANBALI  TO LOANBALI
006216         MOVE WS-SAVE-LOANBALL  TO LOANBALL
006217         MOVE AL-UANON          TO LOANBALA
006218     END-IF
006219     IF WS-SAVE-PROCL > 0
006220         MOVE WS-SAVE-PROCI  TO PROCI
006221         MOVE WS-SAVE-PROCL  TO PROCL
006222         MOVE AL-UANON       TO PROCA
006223     END-IF
006224     IF WS-SAVE-SUPVL > 0
006225         MOVE WS-SAVE-SUPVI  TO SUPVI
006226         MOVE WS-SAVE-SUPVL  TO SUPVL
006227         MOVE AL-UANON       TO SUPVA
006228     END-IF
006229     IF WS-SAVE-PRICDL > 0
006230         MOVE WS-SAVE-PRICDI  TO PRICDI
006231         MOVE WS-SAVE-PRICDL  TO PRICDL
006232         MOVE AL-UANON        TO PRICDA
006233     END-IF
006234     IF WS-SAVE-FILETOL > 0
006235         MOVE WS-SAVE-FILETOI  TO FILETOI
006236         MOVE WS-SAVE-FILETOL  TO FILETOL
006237         MOVE AL-UANON         TO FILETOA
006238     END-IF
006239     IF WS-SAVE-PDTHRUL > 0
006240         MOVE WS-SAVE-PDTHRUI  TO PDTHRUI
006241         MOVE WS-SAVE-PDTHRUL  TO PDTHRUL
006242         MOVE AL-UANON         TO PDTHRUA
006243     END-IF
006244     IF WS-SAVE-PDAMTL > 0
006245         MOVE WS-SAVE-PDAMTI  TO PDAMTI
006246         MOVE WS-SAVE-PDAMTL  TO PDAMTL
006247         MOVE AL-UANON        TO PDAMTA
006248     END-IF
006249     IF WS-SAVE-NODAYSL > 0
006250         MOVE WS-SAVE-NODAYSI  TO NODAYSI
006251         MOVE WS-SAVE-NODAYSL  TO NODAYSL
006252         MOVE AL-UANON         TO NODAYSA
006253     END-IF
006254     IF WS-SAVE-NOPMTSL > 0
006255         MOVE WS-SAVE-NOPMTSI  TO NOPMTSI
006256         MOVE WS-SAVE-NOPMTSL  TO NOPMTSL
006257         MOVE AL-UANON         TO NOPMTSA
006258     END-IF
006259     IF WS-SAVE-FORMTYPL > 0
006260         MOVE WS-SAVE-FORMTYPI  TO FORMTYPI
006261         MOVE WS-SAVE-FORMTYPL  TO FORMTYPL
006262         MOVE AL-UANON          TO FORMTYPA
006263     END-IF
006264     IF WS-SAVE-OCCL > 0
006265         MOVE WS-SAVE-OCCI  TO OCCI
006266         MOVE WS-SAVE-OCCL  TO OCCL
006267         MOVE AL-UANON      TO OCCA
006268     END-IF.
006269 4200-EXIT.
006270      EXIT.
006271
006272 5000-BUILD-MAP.
006273     IF PI-RETURN-CD-1 = SPACE
006274         PERFORM 5010-RESET-PI-AREA THRU 5010-EXIT.
006275
006276     MOVE SPACE                  TO PI-RETURN-CD-1.
006277
006278     MOVE LOW-VALUES             TO EL131AI.
006279
006280     MOVE PI-COMPANY-CD          TO COMPANY-CODE.
006281     MOVE PI-CARRIER             TO CARRIER-CODE.
006282     MOVE PI-CLAIM-NO            TO CLAIM-NO.
006283     MOVE PI-CERT-NO             TO CERT-NO.
006284
006285 5000-READ-CLAIM-MASTER.
006286
006287     
      * EXEC CICS READ
006288*         SET      (ADDRESS OF CLAIM-MASTER)
006289*         DATASET  (CLMS-FILE-ID)
006290*         RIDFLD   (MSTR-KEY)
006291*    END-EXEC.
      *    MOVE '&"S        E          (   #00013030' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303133303330' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006292
006293     PERFORM 5020-MOVE-MSTR THRU 5020-EXIT.
006294
006295 5000-READ-DIAGNOSIS-TRAILER.
006296
006297     
      * EXEC CICS HANDLE CONDITION
006298*         NOTFND (5000-TRLR-NOT-FOUND)
006299*    END-EXEC.
      *    MOVE '"$I                   ! C #00013040' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'4320233030303133303430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006300
006301     MOVE MSTR-KEY               TO TRLR-KEY.
006302     MOVE NINETY                 TO TRLR-SEQ-NO.
006303
006304     
      * EXEC CICS READ
006305*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
006306*         DATASET  (TRLR-FILE-ID)
006307*         RIDFLD   (TRLR-KEY)
006308*    END-EXEC.
      *    MOVE '&"S        E          (   #00013047' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303133303437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006309
006310     MOVE AT-INFO-LINE-1         TO DIAGO.
006311     MOVE AT-ICD-CODE-1          TO ICD1O.
006312     MOVE AT-ICD-CODE-2          TO ICD2O.
006313
006314 5000-READ-CERTIFICATE-MASTER.
006315
006316     MOVE PI-COMPANY-CD          TO CERT-COMPANY-CODE.
006317     MOVE CL-CERT-CARRIER        TO CERT-CARRIER
006318     MOVE CL-CERT-GROUPING       TO CERT-GROUP PI-GROUPING.
006319     MOVE CL-CERT-STATE          TO CERT-STATE PI-STATE.
006320     MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT PI-ACCOUNT.
006321     MOVE CL-CERT-NO             TO CERT-CERT PI-CERT-NO.
006322     MOVE CL-CERT-EFF-DT         TO CERT-DATE PI-CERT-EFF-DT.
006323
006324     
      * EXEC CICS HANDLE CONDITION
006325*         NOTFND (5000-CERT-NOT-FOUND)
006326*    END-EXEC.
      *    MOVE '"$I                   ! D #00013067' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'4420233030303133303637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006327
006328     
      * EXEC CICS READ
006329*         SET      (ADDRESS OF CERTIFICATE-MASTER)
006330*         DATASET  (CERT-FILE-ID)
006331*         RIDFLD   (CERT-KEY)
006332*    END-EXEC.
      *    MOVE '&"S        E          (   #00013071' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303133303731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006333
006334     PERFORM 5030-MOVE-CERT THRU 5030-EXIT.
006335
006336     PERFORM 5100-LOAD-CORR-TRLR THRU 5100-EXIT.
006337
006338     PERFORM 7600-BROWSE-CLAIM THRU 7699-EXIT.
006339
006340     GO TO 5000-EXIT.
006341
006342 5000-TRLR-NOT-FOUND.
006343     MOVE ER-7687                TO EMI-ERROR.
006344     MOVE -1                     TO DIAGL.
006345     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006346     GO TO 5000-EXIT.
006347
006348 5000-CERT-NOT-FOUND.
006349     MOVE ER-0206                TO EMI-ERROR.
006350     MOVE -1                     TO CERTL.
006351     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006352
006353 5000-EXIT.
006354     EXIT.
006355     EJECT
006356 5010-RESET-PI-AREA.
006357     IF CERTI GREATER THAN LOW-VALUES
006358         MOVE CERTI              TO PI-CERT-PRIME.
006359
006360     IF SUFXI GREATER THAN LOW-VALUES
006361         MOVE SUFXI              TO PI-CERT-SFX.
006362
006363     IF CERTEFFI GREATER THAN LOW-VALUES
006364         MOVE HOLD-EFF           TO PI-CERT-EFF-DT.
006365
006366     IF CERTACTI GREATER THAN LOW-VALUES
006367         MOVE CERTACTI           TO PI-ACCOUNT.
006368
006369     IF CERTSTI GREATER THAN LOW-VALUES
006370         MOVE CERTSTI            TO PI-STATE.
006371
006372     IF CERTCARI GREATER THAN LOW-VALUES
006373         MOVE CERTCARI           TO PI-CARRIER.
006374
006375     IF CERTGRPI GREATER THAN LOW-VALUES
006376         MOVE CERTGRPI           TO PI-GROUPING.
006377
006378 5010-EXIT.
006379     EXIT.
006380
006381     EJECT
006382 5020-MOVE-MSTR.
006383
006384     MOVE CL-ASSOC-CERT-SEQU     TO WS-CURRENT-SEQU.
006385     MOVE CL-ASSOC-CERT-TOTAL    TO WS-OF-SEQU.
006386     MOVE WS-CLAIM-SEQU          TO SEQUO.
006387     MOVE AL-SABON               TO SEQUA.
006388     MOVE CL-CLAIM-NO            TO CLAIMO PI-CLAIM-NO.
006389     MOVE CL-CLAIM-TYPE          TO TYPEO
006390                                    pi-claim-type
006391
006392     IF CL-TOTAL-PAID-AMT  = +0 AND
006393        CL-LAST-PMT-AMT    = +0 AND
006394        CL-NO-OF-PMTS-MADE = +0
006395          MOVE AL-UANON          TO TYPEA.
006396
006397     MOVE CL-PRIME-CERT-PRIME    TO PCERTNOO.
006398     MOVE CL-PRIME-CERT-SFX      TO PSUFXO.
006399     MOVE CL-CERT-PRIME          TO CERTO.
006400     MOVE CL-CERT-SFX            TO SUFXO.
006401     MOVE CL-CERT-CARRIER        TO CARRO.
006402
006403     IF CL-CLAIM-STATUS = 'C'
006404         MOVE 'CLOSED'           TO STATUSO
006405     ELSE
006406         MOVE 'OPEN  '           TO STATUSO.
006407
006408     move cl-accident-claim-sw   to accswo
006409     if cl-benefit-period not numeric
006410        move zeros               to cl-benefit-period
006411     end-if
006412
006413     move cl-benefit-period      to benpero
006414     move cl-insured-type        to instypeo
006415
006416     MOVE CL-PROCESSOR-ID        TO PROCO.
006417
006418     MOVE CL-FILE-ESTABLISHED-BY TO AUIDO.
006419     MOVE CL-CCN                 TO CCNOO.
006420
006421     MOVE CL-INSURED-LAST-NAME   TO MLNAMEO.
006422     MOVE CL-INSURED-1ST-NAME    TO MFNAMEO.
006423     MOVE CL-INSURED-MID-INIT    TO MMINITO.
006424     MOVE CL-INSURED-SEX-CD      TO SEXO.
006425
006426     IF CL-INSURED-BIRTH-DT GREATER THAN LOW-VALUES
006427         MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1
006428         MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE
006429         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006430         MOVE DC-GREG-DATE-1-EDIT TO BIRTHO
006431     ELSE
006432         MOVE SPACES             TO BIRTHO.
006433
006434     IF CL-SSN-STATE   = CL-CERT-STATE  AND
006435        CL-SSN-ACCOUNT = CL-CERT-ACCOUNT-PRIME
006436         NEXT SENTENCE
006437       ELSE
006438         MOVE CL-SOC-SEC-NO      TO SOCIALO.
006439
006440     IF PI-COMPANY-ID = 'DMD'
006441       IF CL-CERT-PRIME (4:1) = 'U' OR 'F'
006442          IF SOCIALO = SPACES OR LOW-VALUES
006443            MOVE ER-0885         TO EMI-ERROR
006444            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006445
006446     MOVE CL-INSURED-OCC-CD      TO OCCO.
006447*    MOVE CL-CAUSE-CD            TO CAUSEO.
006448
006449*    IF CL-EST-END-OF-DISAB-DT GREATER THAN LOW-VALUES
006450*        MOVE CL-EST-END-OF-DISAB-DT TO DC-BIN-DATE-1
006451*        MOVE SPACES                 TO DC-OPTION-CODE
006452*        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006453*        MOVE DC-GREG-DATE-1-EDIT TO ENDO
006454*    ELSE
006455*        MOVE SPACES             TO ENDO.
006456
006457     IF CL-PAID-THRU-DT GREATER THAN LOW-VALUES
006458        MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1
006459        MOVE SPACES             TO DC-OPTION-CODE
006460        PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006461        MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO
006462        IF PI-USES-PAID-TO
006463           MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1
006464           MOVE '6'                TO DC-OPTION-CODE
006465           MOVE +1                 TO DC-ELAPSED-DAYS
006466           MOVE +0                 TO DC-ELAPSED-MONTHS
006467           PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006468           MOVE DC-GREG-DATE-1-EDIT TO PDTHRUO
006469        ELSE
006470           NEXT SENTENCE
006471     ELSE
006472        MOVE SPACES             TO PDTHRUO.
006473
006474     MOVE CL-TOTAL-PAID-AMT      TO PDAMTO.
006475     MOVE CL-NO-OF-DAYS-PAID     TO NODAYSO.
006476     MOVE CL-NO-OF-PMTS-MADE     TO NOPMTSO.
006477     MOVE CL-PROG-FORM-TYPE      TO FORMTYPO.
006478
006479     IF MODIFY-CAP
006480        MOVE AL-UNNOF            TO PDAMTA
006481                                    NODAYSA
006482                                    NOPMTSA
006483        MOVE AL-UANOF            TO PDTHRUA.
006484
006485     MOVE PI-COMPANY-ID          TO CNTL-CO-ID
006486     MOVE '2'                    TO CNTL-REC-TYPE
006487     MOVE +0                     TO CNTL-SEQ-NO
006488     MOVE PI-PROCESSOR-ID        TO CNTL-PROC-ID
006489
006490     
      * EXEC CICS READ
006491*         DATASET  (CNTL-FILE-ID)
006492*         SET      (ADDRESS OF CONTROL-FILE)
006493*         RIDFLD   (CNTL-KEY)
006494*         RESP     (WS-RESPONSE)
006495*    END-EXEC
      *    MOVE '&"S        E          (  N#00013233' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303133323333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006496
006497     IF WS-RESP-NORMAL
006498        MOVE CF-APPROVAL-LEVEL   TO PI-APPROVAL-LEVEL
006499     end-if
006500
006501     IF CL-INCURRED-DT GREATER THAN LOW-VALUES
006502         MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1
006503         MOVE SPACES             TO DC-OPTION-CODE
006504         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006505         MOVE DC-GREG-DATE-1-EDIT TO INCO
006506     ELSE
006507         MOVE SPACES             TO INCO.
006508
006509     IF (CL-NO-OF-PMTS-MADE = ZEROS)
006510        OR (PI-APPROVAL-LEVEL = '4' OR '5')
006511        MOVE AL-UANOF            TO INCA
006512     end-if
006513
006514     MOVE CL-NO-OF-PMTS-MADE     TO PI-NO-PMTS.
006515
006516     IF CL-REPORTED-DT GREATER THAN LOW-VALUES
006517         MOVE CL-REPORTED-DT     TO DC-BIN-DATE-1
006518         MOVE SPACES             TO DC-OPTION-CODE
006519         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006520         MOVE DC-GREG-DATE-1-EDIT TO REPO
006521     ELSE
006522         MOVE SPACES             TO REPO.
006523
006524     IF CL-FILE-ESTABLISH-DT GREATER THAN LOW-VALUES
006525         MOVE CL-FILE-ESTABLISH-DT TO DC-BIN-DATE-1
006526         MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE
006527         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006528         MOVE DC-GREG-DATE-1-EDIT TO ESTO
006529     ELSE
006530         MOVE SPACES             TO ESTO.
006531
006532     IF CL-LAST-MAINT-DT GREATER THAN LOW-VALUES
006533         MOVE CL-LAST-MAINT-DT   TO DC-BIN-DATE-1
006534         MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE
006535         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006536         MOVE DC-GREG-DATE-1-EDIT TO MNTDTO
006537     ELSE
006538         MOVE SPACES             TO MNTDTO.
006539
006540
006541     IF CL-LAST-MAINT-TYPE = SPACE
006542         MOVE 'SET-UP'           TO MNTTYPEO
006543     ELSE
006544     IF CL-LAST-MAINT-TYPE = '1'
006545         MOVE 'PAYMNT'           TO MNTTYPEO
006546     ELSE
006547     IF CL-LAST-MAINT-TYPE = '2'
006548         MOVE 'LETTER'           TO MNTTYPEO
006549     ELSE
006550     IF CL-LAST-MAINT-TYPE = '3'
006551         MOVE 'UPDATE'           TO MNTTYPEO
006552     ELSE
006553     IF CL-LAST-MAINT-TYPE = '4'
006554         MOVE 'RESTOR'           TO MNTTYPEO
006555     ELSE
006556     IF CL-LAST-MAINT-TYPE = '5'
006557         MOVE 'INC DT'           TO MNTTYPEO
006558     ELSE
006559     IF CL-LAST-MAINT-TYPE = '6'
006560         MOVE ' CONV'            TO MNTTYPEO
006561     ELSE
006562     IF CL-LAST-MAINT-TYPE = 'C'
006563         MOVE 'CHGBEN'           TO MNTTYPEO
006564     ELSE
006565     IF CL-LAST-MAINT-TYPE = 'E'
006566         MOVE 'ERRCOR'           TO MNTTYPEO
006567     ELSE
006568         MOVE SPACES             TO MNTTYPEO.
006569
006570     MOVE CL-BENEFICIARY         TO BENEO.
006571     MOVE CL-FILE-LOCATION       TO FILETOO.
006572     IF CL-CRITICAL-PERIOD NOT NUMERIC
006573        MOVE ZEROS               TO CL-CRITICAL-PERIOD
006574     END-IF
006575     MOVE CL-CRITICAL-PERIOD     TO CRITPO
006576     IF CL-NO-OF-EXTENSIONS NOT NUMERIC
006577        MOVE ZEROS               TO CL-NO-OF-EXTENSIONS
006578     END-IF
006579     MOVE CL-NO-OF-EXTENSIONS    TO EXTENSO
006580*    MOVE CL-CRIT-PER-RECURRENT
006581*                                TO CRITPTO
006582*    IF CL-CRIT-PER-RTW-MOS NOT NUMERIC
006583*       MOVE ZEROS               TO CL-CRIT-PER-RTW-MOS
006584*    END-IF
006585*    MOVE CL-CRIT-PER-RTW-MOS    TO RTWMOSO
006586     MOVE CL-PRIORITY-CD         TO PRICDO.
006587     MOVE CL-SUPV-ATTN-CD        TO SUPVO.
006588     MOVE CL-LAST-MAINT-USER     TO PI-UPDATE-BY.
006589     MOVE CL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
006590     MOVE CL-AUTO-PAY-SEQ        TO PI-AUTO-PAY-SEQ.
006591
006592 5020-EXIT.
006593     EXIT.
006594     EJECT
006595 5030-MOVE-CERT.
006596
006597     IF CM-CERT-EFF-DT GREATER THAN LOW-VALUES
006598         MOVE CM-CERT-EFF-DT     TO DC-BIN-DATE-1
006599         MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE
006600         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006601         MOVE DC-GREG-DATE-1-EDIT    TO CERTEFFO HOLD-DATE
006602       ELSE
006603         MOVE SPACES             TO CERTEFFO
006604         MOVE ZERO               TO HOLD-DATE.
006605
006606     MOVE CM-ACCOUNT             TO CERTACTO.
006607     MOVE CM-STATE               TO CERTSTO PI-STATE.
006608     MOVE CM-GROUPING            TO CERTGRPO.
006609     MOVE CM-CARRIER             TO CERTCARO.
006610     MOVE CM-INSURED-LAST-NAME   TO CRTLNMEO.
006611     MOVE CM-INSURED-INITIAL2    TO CRTINITO.
006612     MOVE CM-INSURED-FIRST-NAME  TO CRTFNMEO.
006613     MOVE CM-INSURED-ISSUE-AGE   TO ISSAGEO.
006614     MOVE CM-JT-LAST-NAME        TO JNTLNMEO.
006615     MOVE CM-JT-FIRST-NAME       TO JNTFNMEO.
006616     MOVE CM-JT-INITIAL          TO JNTINITO.
006617     MOVE CM-INSURED-JOINT-AGE   TO JNTAGEO.
006618
006619*** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
006620     MOVE PI-COMPANY-ID          TO CNTL-CO-ID.
006621     MOVE '3'                    TO CNTL-REC-TYPE.
006622     MOVE SPACES                 TO CNTL-STATE-ACCESS.
006623     MOVE CM-STATE               TO CNTL-STATE-NUMBER.
006624     MOVE ZERO                   TO CNTL-SEQ-NO.
006625
006626     
      * EXEC CICS READ
006627*         SET        (ADDRESS OF CONTROL-FILE)
006628*         DATASET    ('ELCNTL')
006629*         RIDFLD     (CNTL-KEY)
006630*         RESP       (WS-RESPONSE)
006631*    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00013369' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303133333639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006632
006633     IF WS-RESP-NOTFND
006634        MOVE ZERO                TO CP-FREE-LOOK
006635     ELSE
006636        MOVE CF-ST-FREE-LOOK-PERIOD
006637                                 TO CP-FREE-LOOK.
006638
006639     IF CM-LF-BENEFIT-CD = '00'
006640        GO TO 5030-MOVE-CERT-AH.
006641
006642     EJECT
006643     MOVE EIBDATE                TO DC-JULIAN-YYDDD
006644     MOVE '5'                    TO DC-OPTION-CODE
006645     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006646     MOVE DC-BIN-DATE-1          TO WS-BIN-CURRENT-DT.
006647     MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.
006648     MOVE CM-LF-BENEFIT-CD       TO LCVCDO HOLD-BENEFIT.
006649     MOVE CM-LF-ORIG-TERM        TO LCVOTRMO  CP-ORIGINAL-TERM.
006650     MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT
006651     MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
006652     MOVE CL-INCURRED-DT         TO CP-VALUATION-DT
006653     MOVE '4'                    TO CP-REM-TERM-METHOD
006654     MOVE PI-COMPANY-ID          TO CP-COMPANY-ID
006655     MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
006656     PERFORM 9700-LINK-RTRM-FILE-ID THRU 9700-EXIT.
006657
006658     MOVE CP-REMAINING-TERM-3    TO LCVRTRMO.
006659     IF CM-LF-PREMIUM-RATE NUMERIC
006660         MOVE CM-LF-PREMIUM-RATE TO LCVRATEO
006661     ELSE
006662         MOVE ZEROS              TO LCVRATEO.
006663
006664     IF CM-LF-ALT-BENEFIT-AMT NOT NUMERIC
006665         MOVE ZEROS              TO CM-LF-ALT-BENEFIT-AMT.
006666
006667     COMPUTE LCVBENEO = CM-LF-BENEFIT-AMT + CM-LF-ALT-BENEFIT-AMT.
006668
006669     MOVE CM-POLICY-FORM-NO      TO LCVFORMO.
006670
006671     IF CM-LF-CURRENT-STATUS = '8'
006672        IF CM-LF-CANCEL-DT NOT = LOW-VALUES
006673            MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1
006674            MOVE ' '             TO DC-OPTION-CODE
006675            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006676            IF NOT DATE-CONVERSION-ERROR
006677                MOVE DC-GREG-DATE-1-EDIT     TO LCVCNDTO.
006678
006679     IF CM-LF-CURRENT-STATUS = '7'
006680        IF CM-LF-DEATH-DT NOT = LOW-VALUES
006681            MOVE CM-LF-DEATH-DT  TO DC-BIN-DATE-1
006682            MOVE ' '             TO DC-OPTION-CODE
006683            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006684            IF NOT DATE-CONVERSION-ERROR
006685                MOVE DC-GREG-DATE-1-EDIT     TO LCVCNDTO.
006686
006687     IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES
006688         MOVE ' '                TO DC-OPTION-CODE
006689         MOVE CM-LF-DEATH-EXIT-DT TO DC-BIN-DATE-1
006690         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006691         IF NOT DATE-CONVERSION-ERROR
006692             MOVE DC-GREG-DATE-1-EDIT TO HOLD-DATE
006693             MOVE HOLD-MONTH     TO WORK-MONTH
006694             MOVE HOLD-YEAR      TO WORK-YEAR
006695             MOVE WORK-DATE-MY   TO LCVEXITO.
006696
006697     IF CM-LF-CURRENT-STATUS = '1' OR = '4'
006698        IF CP-REMAINING-TERM-3 = ZEROS
006699           MOVE 'EXPIRED'        TO LCVSTATO
006700        ELSE
006701           MOVE 'ACTIVE'         TO LCVSTATO.
006702
006703     IF CM-LF-CURRENT-STATUS = '2'
006704        MOVE 'PEND   '           TO LCVSTATO.
006705     IF CM-LF-CURRENT-STATUS = '3'
006706        MOVE 'RESTORE'           TO LCVSTATO.
006707     IF CM-LF-CURRENT-STATUS = '5'
006708        MOVE 'REISSUE'           TO LCVSTATO.
006709     IF CM-LF-CURRENT-STATUS = '6'
006710        MOVE 'LMPBEN'            TO LCVSTATO.
006711     IF CM-LF-CURRENT-STATUS = '7'
006712        MOVE 'DEATH  '           TO LCVSTATO.
006713     IF CM-LF-CURRENT-STATUS = '8'
006714        MOVE 'CANCEL '           TO LCVSTATO.
006715     IF CM-LF-CURRENT-STATUS = '9'
006716        MOVE 'RE-ONLY'           TO LCVSTATO.
006717     IF CM-LF-CURRENT-STATUS = 'V'
006718        MOVE 'VOID   '           TO LCVSTATO.
006719     IF CM-LF-CURRENT-STATUS = 'D'
006720        MOVE 'DECLINE'           TO LCVSTATO.
006721
006722     MOVE SPACES                 TO BENEFIT-KEY ERROR-SWITCH.
006723     MOVE PI-COMPANY-ID          TO BEN-CO-ID.
006724     MOVE '4'                    TO BEN-REC-TYPE.
006725     MOVE CM-LF-BENEFIT-CD       TO BEN-ACC-CD.
006726     MOVE ZERO                   TO BEN-SEQ-NO.
006727
006728     
      * EXEC CICS READ GTEQ
006729*         DATASET  (CNTL-FILE-ID)
006730*         RIDFLD   (BENEFIT-KEY)
006731*         SET      (ADDRESS OF CONTROL-FILE)
006732*    END-EXEC.
      *    MOVE '&"S        G          (   #00013471' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303133343731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENEFIT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006733
006734     IF CF-COMPANY-ID NOT = PI-COMPANY-ID OR
006735        CF-RECORD-TYPE NOT = '4'
006736         MOVE ER-0240            TO EMI-ERROR
006737         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006738         GO TO 5030-MOVE-CERT-FINISH.
006739
006740     MOVE ZERO                   TO COUNT-2.
006741     PERFORM 5040-FIND-BENEFIT THRU 5060-EXIT.
006742
006743     IF SCREEN-ERROR
006744         MOVE ER-0282            TO EMI-ERROR
006745         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006746        ELSE
006747         MOVE CF-BENEFIT-ALPHA (COUNT-2) TO LCVKINDO.
006748
006749 5030-MOVE-CERT-AH.
006750
006751     IF CM-LF-BENEFIT-CD = '00'
006752         IF CM-AH-BENEFIT-CD = '00'
006753             GO TO 5030-MOVE-CERT-FINISH.
006754
006755     IF CM-AH-BENEFIT-CD = '00'
006756        MOVE SPACES             TO ACVKINDO
006757        COMPUTE PI-PAYMENT-AMT =
006758                    CM-LF-BENEFIT-AMT / CM-LF-ORIG-TERM
006759        GO TO 5030-MOVE-CERT-FINISH.
006760
006761     MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.
006762     MOVE CM-AH-BENEFIT-CD       TO ACVCDO HOLD-BENEFIT.
006763     MOVE DC-GREG-DATE-1-EDIT    TO HOLD-DATE.
006764     MOVE CM-AH-ORIG-TERM        TO ACVOTRMO CP-ORIGINAL-TERM.
006765     MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT
006766     MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
006767     MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
006768     MOVE SAVE-BIN-DATE          TO CP-VALUATION-DT.
006769
006770**DMD **********************************************
006771     IF PI-COMPANY-ID = 'DMD'
006772         COMPUTE WS-TERM-IN-DAYS = CM-AH-ORIG-TERM * 30 +
006773                                   CM-PMT-EXTENSION-DAYS
006774         COMPUTE WS-REM-TERM-IN-DAYS = WS-TERM-IN-DAYS -
006775                                      CL-NO-OF-DAYS-PAID
006776         DIVIDE WS-REM-TERM-IN-DAYS BY +30
006777             GIVING WS-REM-MOS REMAINDER WS-REM-DAYS
006778         MOVE WS-REM-MOS         TO ACVRTRMO
006779                                    CP-REMAINING-TERM-3
006780*        MOVE WS-REM-DAYS        TO DAYSPO
006781         GO TO 5030-SKIP-RTRM.
006782**DMD **********************************************
006783
006784     MOVE '4'                    TO CP-REM-TERM-METHOD.
006785     MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
006786     PERFORM 9700-LINK-RTRM-FILE-ID THRU 9700-EXIT.
006787     MOVE CP-REMAINING-TERM-3    TO ACVRTRMO.
006788
006789 5030-SKIP-RTRM.
006790     IF CM-AH-PREMIUM-RATE NUMERIC
006791         MOVE CM-AH-PREMIUM-RATE TO ACVRATEO
006792     ELSE
006793         MOVE ZEROS              TO ACVRATEO.
006794
006795     MOVE CM-AH-BENEFIT-AMT      TO ACVBENEO  PI-PAYMENT-AMT.
006796     MOVE CM-POLICY-FORM-NO      TO ACVFORMO.
006797
006798     IF CM-AH-CURRENT-STATUS = '8'
006799        IF CM-AH-CANCEL-DT NOT = LOW-VALUES
006800            MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1
006801            MOVE ' '             TO DC-OPTION-CODE
006802            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006803            IF NOT DATE-CONVERSION-ERROR
006804                MOVE DC-GREG-DATE-1-EDIT     TO ACVCNDTO.
006805
006806     IF CM-AH-CURRENT-STATUS = '6' OR '7'
006807        IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
006808            MOVE CM-AH-SETTLEMENT-DT         TO DC-BIN-DATE-1
006809            MOVE ' '             TO DC-OPTION-CODE
006810            PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006811            IF NOT DATE-CONVERSION-ERROR
006812                MOVE DC-GREG-DATE-1-EDIT     TO ACVCNDTO.
006813
006814     IF CM-AH-CURRENT-STATUS = '1' OR = '4'
006815        IF CP-REMAINING-TERM-3 = ZEROS
006816           MOVE 'EXPIRED'        TO ACVSTATO
006817        ELSE
006818           MOVE 'ACTIVE'         TO ACVSTATO.
006819
006820     IF CM-AH-CURRENT-STATUS = '2'
006821        MOVE 'PEND   '           TO ACVSTATO.
006822     IF CM-AH-CURRENT-STATUS = '3'
006823        MOVE 'RESTORE'           TO ACVSTATO.
006824     IF CM-AH-CURRENT-STATUS = '5'
006825        MOVE 'REISSUE'           TO ACVSTATO.
006826     IF CM-AH-CURRENT-STATUS = '6'
006827        MOVE 'LMP DIS'           TO ACVSTATO.
006828     IF CM-AH-CURRENT-STATUS = '7'
006829        MOVE 'DEATH  '           TO ACVSTATO.
006830     IF CM-AH-CURRENT-STATUS = '8'
006831        MOVE 'CANCEL '           TO ACVSTATO.
006832     IF CM-AH-CURRENT-STATUS = '9'
006833        MOVE 'RE-ONLY'           TO ACVSTATO.
006834     IF CM-AH-CURRENT-STATUS = 'V'
006835        MOVE 'VOID   '           TO ACVSTATO.
006836     IF CM-AH-CURRENT-STATUS = '9'
006837        MOVE 'DECLINE'           TO ACVSTATO.
006838
006839     IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES
006840         MOVE ' '                TO DC-OPTION-CODE
006841         MOVE CM-AH-SETTLEMENT-EXIT-DT TO DC-BIN-DATE-1
006842         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006843         IF NOT DATE-CONVERSION-ERROR
006844             MOVE DC-GREG-DATE-1-EDIT     TO HOLD-DATE
006845             MOVE HOLD-MONTH     TO WORK-MONTH
006846             MOVE HOLD-YEAR      TO WORK-YEAR
006847             MOVE WORK-DATE-MY   TO ACVEXITO.
006848
006849     MOVE SPACES                 TO BENEFIT-KEY ERROR-SWITCH.
006850
006851     MOVE PI-COMPANY-ID          TO BEN-CO-ID.
006852     MOVE '5'                    TO BEN-REC-TYPE.
006853     MOVE CM-AH-BENEFIT-CD       TO BEN-ACC-CD.
006854     MOVE ZERO                   TO BEN-SEQ-NO.
006855
006856     
      * EXEC CICS READ GTEQ
006857*         DATASET  (CNTL-FILE-ID)
006858*         RIDFLD   (BENEFIT-KEY)
006859*         SET      (ADDRESS OF CONTROL-FILE)
006860*    END-EXEC.
      *    MOVE '&"S        G          (   #00013599' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303133353939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENEFIT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006861
006862     IF CF-COMPANY-ID NOT = PI-COMPANY-ID OR
006863        CF-RECORD-TYPE NOT = '5'
006864         MOVE ER-0250            TO EMI-ERROR
006865         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006866         GO TO 5030-MOVE-CERT-FINISH.
006867
006868     MOVE ZERO                   TO COUNT-2.
006869     PERFORM 5040-FIND-BENEFIT THRU 5060-EXIT.
006870
006871     IF SCREEN-ERROR
006872         MOVE ER-0283            TO EMI-ERROR
006873         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006874       ELSE
006875         MOVE CF-BENEFIT-ALPHA (COUNT-2) TO ACVKINDO.
006876
006877 5030-MOVE-CERT-FINISH.
006878
006879     MOVE CM-LOAN-APR            TO APRO.
006880     MOVE CM-PAY-FREQUENCY       TO PMTFREQO.
006881     MOVE CM-IND-GRP-TYPE        TO INDGRPO.
006882     MOVE CM-PREMIUM-TYPE        TO PREMTYPO  PI-PREM-TYPE.
006883*    MOVE CM-LOAN-NUMBER         TO LOANNOO
006884     MOVE CM-LOAN-BALANCE        TO LOANBALO.
006885     MOVE CM-SPECIAL-REIN-CODE   TO REINCDO.
006886
006887     IF CM-LAST-ADD-ON-DT NOT = SPACES  AND  ZEROS
006888         MOVE CM-LAST-ADD-ON-DT  TO DC-BIN-DATE-1
006889         MOVE SPACES             TO DC-OPTION-CODE DC-ERROR-CODE
006890         PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
006891         MOVE DC-GREG-DATE-1-EDIT  TO ADDONDTO
006892     ELSE
006893         MOVE SPACES               TO ADDONDTO.
006894
006895     MOVE CM-CLAIM-INTERFACE-SW  TO PI-CERT-SWITCH.
006896
006897 5030-EXIT.
006898     EXIT.
006899     EJECT
006900 5040-FIND-BENEFIT.
006901     ADD 1                       TO COUNT-2.
006902     IF COUNT-2 GREATER THAN 8
006903         GO TO 5050-BENEFIT-NOTFND.
006904
006905     IF CF-BENEFIT-CODE (COUNT-2) = HOLD-BENEFIT
006906         GO TO 5060-EXIT.
006907
006908     IF CF-BENEFIT-CODE (COUNT-2) GREATER THAN HOLD-BENEFIT
006909         GO TO 5050-BENEFIT-NOTFND.
006910
006911     GO TO 5040-FIND-BENEFIT.
006912
006913 5050-BENEFIT-NOTFND.
006914     MOVE 'X'                    TO ERROR-SWITCH.
006915
006916 5060-EXIT.
006917     EXIT.
006918 5100-LOAD-CORR-TRLR.
006919     MOVE PI-COMPANY-CD      TO TRLR-COMPANY-CD.
006920     MOVE PI-CARRIER         TO TRLR-CARRIER.
006921     MOVE PI-CLAIM-NO        TO TRLR-CLAIM-NO.
006922     MOVE PI-CERT-NO         TO TRLR-CERT-NO.
006923     MOVE 1000               TO TRLR-SEQ-NO.
006924
006925     
      * EXEC CICS HANDLE CONDITION
006926*         NOTFND    (1052-DONE)
006927*         ENDFILE   (1052-DONE)
006928*    END-EXEC.
      *    MOVE '"$I''                  ! E #00013668' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'4520233030303133363638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006929
006930     
      * EXEC CICS STARTBR
006931*        DATASET (TRLR-FILE-ID)
006932*        RIDFLD  (TRLR-KEY)
006933*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00013673' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303133363733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006934
006935
006936     MOVE SPACES TO WS-TRLR-FILE-EOF
006937                    WS-AUTH-RCVD
006938     PERFORM 1052-READ-TRLR THRU 1052-EXIT
006939       UNTIL  TRLR-FILE-EOF
006940         OR WS-AUTH-RCVD > SPACES.
006941
006942     
      * EXEC CICS ENDBR
006943*        DATASET (TRLR-FILE-ID)
006944*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013685' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303133363835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006945
006946     IF AUTH-RCVD
006947        MOVE 'Y' TO AUTHRCVO
006948     ELSE
006949        MOVE 'N' TO AUTHRCVO
006950     END-IF.
006951
006952
006953 5100-EXIT.
006954     EXIT.
006955
006956
006957     EJECT
006958 6000-CALCULATE-CERT-TERM.
006959
006960     IF CM-LF-PREMIUM-RATE NOT NUMERIC
006961         MOVE ZEROS              TO CM-LF-PREMIUM-RATE.
006962     IF CM-AH-PREMIUM-RATE NOT NUMERIC
006963         MOVE ZEROS              TO CM-AH-PREMIUM-RATE.
006964     IF CM-LOAN-APR NOT NUMERIC
006965         MOVE ZEROS              TO CM-LOAN-APR.
006966
006967*    IF WS-CALC-METHOD  = 'G'
006968         PERFORM 6100-CALC-GROSS-TERM THRU 6200-EXIT
006969*     ELSE
006970*        PERFORM 6500-CALC-NET-TERM   THRU 6700-EXIT.
006971
006972     IF N GREATER 120
006973         MOVE 120                TO N.
006974
006975     MOVE '6'                    TO DC-OPTION-CODE.
006976     MOVE N                      TO DC-ELAPSED-MONTHS.
006977     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
006978     PERFORM 9800-CONVERT-DATE THRU 9800-EXIT.
006979
006980     IF CM-LF-BENEFIT-CD NOT = ZERO
006981         MOVE N                  TO CM-LF-ORIG-TERM
006982         COMPUTE CM-LF-BENEFIT-AMT = PI-PAYMENT-AMT * N
006983         IF NO-CONVERSION-ERROR
006984             MOVE DC-BIN-DATE-2  TO CM-LF-LOAN-EXPIRE-DT.
006985
006986     IF CM-AH-BENEFIT-CD NOT = ZERO
006987         MOVE N                  TO CM-AH-ORIG-TERM
006988         IF NO-CONVERSION-ERROR
006989             MOVE DC-BIN-DATE-2  TO CM-AH-LOAN-EXPIRE-DT.
006990
006991 6000-EXIT.
006992      EXIT.
006993
006994 6100-CALC-GROSS-TERM.
006995     MOVE CM-LOAN-BALANCE         TO L.
006996     MOVE PI-PAYMENT-AMT          TO M.
006997
006998     COMPUTE N = L / M.
006999     IF N LESS 1
007000         MOVE 1  TO N.
007001
007002     IF CM-LF-BENEFIT-CD NOT = ZERO
007003         COMPUTE WS-LF-RATE = CM-LF-PREMIUM-RATE / +1000
007004     ELSE
007005         MOVE ZEROS               TO WS-LF-RATE.
007006
007007     IF CM-AH-BENEFIT-CD NOT = ZERO
007008         COMPUTE WS-AH-RATE = CM-AH-PREMIUM-RATE / +1000
007009     ELSE
007010         MOVE ZEROS               TO WS-AH-RATE.
007011
007012     COMPUTE I = CM-LOAN-APR / +1200.
007013
007014 6105-LOOP.
007015     IF N GREATER 240
007016         GO TO 6200-EXIT.
007017     PERFORM 6210-CALC-LEFT-RIGHTONE THRU 6220-EXIT.
007018     IF LEFT-TOT-1 GREATER RIGHT-TOT-1
007019         ADD 1 TO N
007020         GO TO 6105-LOOP.
007021
007022 6200-EXIT.
007023      EXIT.
007024
007025 6210-CALC-LEFT-RIGHTONE.
007026      MOVE L         TO LEFT-TOT-1.
007027      SUBTRACT 1 FROM N.
007028      PERFORM 6300-CALC-A-N THRU 6300-EXIT.
007029      PERFORM 6350-CALC-IA-N THRU 6400-EXIT.
007030      ADD 1 TO N.
007031 6211-CALC-TERM1.
007032      MOVE N         TO TERM1.
007033      MULTIPLY M BY TERM1.
007034 6212-LOOP.
007035      COMPUTE TERM1 = (WS-AH-RATE + WS-LF-RATE) * TERM1.
007036      ADD 1 TO A-N.
007037      MULTIPLY A-N BY TERM1.
007038      SUBTRACT 1 FROM A-N.
007039      ADD TERM1 TO LEFT-TOT-1.
007040 6213-CALC-TERM2.
007041      MOVE M         TO TERM2.
007042      COMPUTE TERM2 = (WS-AH-RATE + WS-LF-RATE) * TERM2.
007043      MULTIPLY IA-N BY TERM2.
007044      SUBTRACT TERM2 FROM LEFT-TOT-1.
007045 6215-CALC-RIGHTONE.
007046      MOVE M         TO RIGHT-TOT-1.
007047      PERFORM 6300-CALC-A-N THRU 6300-EXIT.
007048      MULTIPLY A-N BY RIGHT-TOT-1.
007049 6220-EXIT.
007050      EXIT.
007051
007052 6300-CALC-A-N.
007053     IF N LESS 1
007054         MOVE 0     TO A-N
007055         GO TO 6300-EXIT.
007056     IF I = 0
007057         MOVE .00001 TO I.
007058     ADD 1 TO I.
007059     DIVIDE I INTO 1 GIVING V.
007060     SUBTRACT 1 FROM I.
007061     PERFORM 6450-CALC-V-EX-N THRU 6490-EXIT.
007062     SUBTRACT V-EX-N FROM 1 GIVING TERM3.
007063     DIVIDE I INTO TERM3 GIVING A-N.
007064 6300-EXIT.
007065      EXIT.
007066
007067 6350-CALC-IA-N.
007068     IF N LESS 1
007069         MOVE 0      TO IA-N
007070         GO TO 6400-EXIT.
007071     ADD 1 TO N.
007072     PERFORM 6450-CALC-V-EX-N THRU 6490-EXIT.
007073     SUBTRACT 1 FROM N.
007074     MULTIPLY N BY V-EX-N GIVING TERM3.
007075     SUBTRACT TERM3 FROM A-N GIVING TERM3.
007076     SUBTRACT V FROM 1 GIVING TERM4.
007077     DIVIDE TERM4 INTO TERM3 GIVING IA-N.
007078 6400-EXIT.
007079      EXIT.
007080
007081 6450-CALC-V-EX-N.
007082     IF N LESS 1
007083         MOVE 1    TO V-EX-N
007084         GO TO 6490-EXIT.
007085     MOVE N        TO NV-STORE.
007086     IF V-EX-ONETIME = 1  OR
007087        V NOT = V-EXPONENT (1)
007088          PERFORM 6470-BUILD-V-EX-TABLE THRU 6480-EXIT.
007089 6460-LOOP.
007090     IF N GREATER 248
007091         MOVE 248 TO N.
007092     IF N = 1
007093         MOVE V               TO V-EX-N
007094      ELSE
007095         MOVE V-EXPONENT (N)  TO V-EX-N.
007096     GO TO 6490-EXIT.
007097
007098 6470-BUILD-V-EX-TABLE.
007099     MOVE 2      TO N.
007100     MOVE V      TO V-EXPONENT (1)
007101                    V-EX-N.
007102 6471-LOOP.
007103     MULTIPLY V BY V-EX-N.
007104     MOVE V-EX-N   TO V-EXPONENT (N).
007105     ADD 1 TO N.
007106     IF N LESS 248
007107         GO TO 6471-LOOP.
007108     MOVE NV-STORE     TO N.
007109     MOVE 0            TO V-EX-ONETIME.
007110 6480-EXIT.
007111      EXIT.
007112
007113 6490-EXIT.
007114      EXIT.
007115
007116 6500-CALC-NET-TERM.
007117     MOVE CM-LOAN-BALANCE         TO L.
007118     MOVE PI-PAYMENT-AMT          TO M.
007119
007120     DIVIDE L BY M GIVING N REMAINDER WS-REMAIN.
007121     IF WS-REMAIN GREATER ZERO
007122         ADD 1 TO N.
007123     IF N = 0
007124         MOVE 1 TO N.
007125
007126 6700-EXIT.
007127      EXIT.
007128
007129 6999-CALC-TERM-EXIT.
007130     EXIT.
007131
007132     EJECT
007133 7000-RESET-ATTRIBUTE.
007134
007135     MOVE AL-UANON               TO CERTEFFA  CERTACTA
007136                                    CERTSTA   CERTCARA
007137                                    CERTGRPA.
007138     MOVE -1                     TO CERTCARL.
007139
007140 7000-EXIT.
007141     EXIT.
007142
007143 7600-BROWSE-CLAIM.
007144
007145     
      * EXEC CICS READ
007146*        DATASET(CLMS-FILE-ID)
007147*        SET    (ADDRESS OF CLAIM-MASTER)
007148*        RIDFLD (MSTR-KEY)
007149*        GENERIC
007150*        EQUAL
007151*        KEYLENGTH(ELMSTR-GENERIC-LENGTH)
007152*    END-EXEC.
      *    MOVE '&"S  KG    E          (   #00013888' TO DFHEIV0
           MOVE X'26225320204B472020202045' &
                X'202020202020202020202820' &
                X'2020233030303133383838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 ELMSTR-GENERIC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007153
007154     MOVE CL-CONTROL-PRIMARY     TO MSTR-KEY.
007155
007156     if cl-assoc-cert-total = 0
007157        move 1 to cl-assoc-cert-total
007158     end-if
007159     IF CL-ASSOC-CERT-SEQU = 0
007160        MOVE 1 TO CL-ASSOC-CERT-SEQU
007161     END-IF
007162
007163     IF CL-ASSOC-CERT-TOTAL = 1
007164         MOVE AL-SADOF           TO BCERT1A
007165                                    BSUFX1A
007166                                    BCERT2A
007167                                    BSUFX2A
007168                                    BCERT3A
007169                                    BSUFX3A
007170                                    BCERT4A
007171                                    BSUFX4A
007172                                    BCERT5A
007173                                    BSUFX5A
007174                                    PCERTNOA
007175                                    PSUFXA
007176                                    LABEL1A
007177                                    LABEL2A
007178         GO TO 7699-EXIT.
007179
007180 7610-BROWSE-CLAIM-LOOP.
007181
007182     MOVE LOW-VALUES             TO BCERT1O
007183                                    BSUFX1O
007184                                    BCERT2O
007185                                    BSUFX2O
007186                                    BCERT3O
007187                                    BSUFX3O
007188                                    BCERT4O
007189                                    BSUFX4O
007190                                    BCERT5O
007191                                    BSUFX5O.
007192
007193     
      * EXEC CICS HANDLE CONDITION
007194*        ENDFILE  (7630-END-BROWSE)
007195*    END-EXEC.
      *    MOVE '"$''                   ! F #00013936' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'4620233030303133393336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007196
007197     
      * EXEC CICS STARTBR
007198*        DATASET    (CLMS-FILE-ID)
007199*        RIDFLD     (MSTR-KEY)
007200*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00013940' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303133393430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007201
007202     MOVE +1                     TO WS-ASSOCIATED-CERTS.
007203
007204 7620-READ-CLAIM-LOOP.
007205
007206     
      * EXEC CICS READNEXT
007207*        DATASET   (CLMS-FILE-ID)
007208*        SET       (ADDRESS OF CLAIM-MASTER)
007209*        RIDFLD    (MSTR-KEY)
007210*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013949' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303133393439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007211
007212     IF CL-COMPANY-CD  NOT = PI-COMPANY-CD   OR
007213        CL-CARRIER     NOT = CARRI           OR
007214        CL-CLAIM-NO    NOT = CLAIMI
007215        GO TO 7630-END-BROWSE.
007216
007217     IF WS-ASSOCIATED-CERTS = 1
007218         MOVE CL-CERT-PRIME      TO BCERT1O
007219         MOVE CL-CERT-SFX        TO BSUFX1O.
007220
007221     IF WS-ASSOCIATED-CERTS = 2
007222         MOVE CL-CERT-PRIME      TO BCERT2O
007223         MOVE CL-CERT-SFX        TO BSUFX2O.
007224
007225     IF WS-ASSOCIATED-CERTS = 3
007226         MOVE CL-CERT-PRIME      TO BCERT3O
007227         MOVE CL-CERT-SFX        TO BSUFX3O.
007228
007229     IF WS-ASSOCIATED-CERTS = 4
007230         MOVE CL-CERT-PRIME      TO BCERT4O
007231         MOVE CL-CERT-SFX        TO BSUFX4O.
007232
007233     IF WS-ASSOCIATED-CERTS = 5
007234         MOVE CL-CERT-PRIME      TO BCERT5O
007235         MOVE CL-CERT-SFX        TO BSUFX5O
007236     ELSE
007237         ADD +1                  TO WS-ASSOCIATED-CERTS
007238         GO TO 7620-READ-CLAIM-LOOP.
007239
007240 7630-END-BROWSE.
007241
007242     
      * EXEC CICS ENDBR
007243*        DATASET   (CLMS-FILE-ID)
007244*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013985' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303133393835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007245
007246 7640-HIGHLIGHT-CERT-DISPLAYED.
007247
007248     MOVE AL-SANON               TO BCERT1A
007249                                    BSUFX1A
007250                                    BCERT2A
007251                                    BSUFX2A
007252                                    BCERT3A
007253                                    BSUFX3A
007254                                    BCERT4A
007255                                    BSUFX4A
007256                                    BCERT5A
007257                                    BSUFX5A.
007258
007259     IF BCERT1O = CERTI AND
007260        BSUFX1O = SUFXI
007261            MOVE AL-SABON        TO BCERT1A
007262                                    BSUFX1A.
007263
007264     IF BCERT2O = CERTI AND
007265        BSUFX2O = SUFXI
007266            MOVE AL-SABON        TO BCERT2A
007267                                    BSUFX2A.
007268
007269     IF BCERT3O = CERTI AND
007270        BSUFX3O = SUFXI
007271            MOVE AL-SABON        TO BCERT3A
007272                                    BSUFX3A.
007273
007274     IF BCERT4O = CERTI AND
007275        BSUFX4O = SUFXI
007276            MOVE AL-SABON        TO BCERT4A
007277                                    BSUFX4A.
007278
007279     IF BCERT5O = CERTI AND
007280        BSUFX5O = SUFXI
007281            MOVE AL-SABON        TO BCERT5A
007282                                    BSUFX5A.
007283
007284 7699-EXIT.
007285     EXIT.
007286
007287     EJECT
007288 7700-CHECK-SEQUENCE.
007289
007290     
      * EXEC CICS HANDLE CONDITION
007291*        ENDFILE  (7799-EXIT)
007292*        NOTFND   (7799-EXIT)
007293*    END-EXEC.
      *    MOVE '"$''I                  ! G #00014033' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'4720233030303134303333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007294
007295     
      * EXEC CICS READ
007296*        DATASET(CLMS-FILE-ID)
007297*        SET    (ADDRESS OF CLAIM-MASTER)
007298*        RIDFLD (MSTR-KEY)
007299*        GENERIC
007300*        KEYLENGTH(ELMSTR-GENERIC-LENGTH)
007301*    END-EXEC.
      *    MOVE '&"S  KG    E          (   #00014038' TO DFHEIV0
           MOVE X'26225320204B472020202045' &
                X'202020202020202020202820' &
                X'2020233030303134303338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 ELMSTR-GENERIC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007302
007303     COMPUTE WS-ASSOC-CERT-TOTAL =
007304             CL-ASSOC-CERT-TOTAL + ONE-OR-MIN1.
007305
007306     GO TO 7799-EXIT.
007307
007308 7710-RESEQUENCE-CLAIMS.
007309
007310     
      * EXEC CICS HANDLE CONDITION
007311*        ENDFILE  (7799-EXIT)
007312*    END-EXEC.
      *    MOVE '"$''                   ! H #00014053' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'4820233030303134303533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007313
007314     
      * EXEC CICS STARTBR
007315*        DATASET(CLMS-FILE-ID)
007316*        RIDFLD (MSTR-KEY)
007317*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00014057' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303134303537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007318
007319     COMPUTE WS-ASSOC-CERT-SEQU =
007320             WS-ASSOC-CERT-SEQU + 1.
007321
007322     COMPUTE WS-READNEXT-SWITCH =
007323             WS-READNEXT-SWITCH + 1.
007324
007325 7720-READ-CLAIM-LOOP.
007326
007327     
      * EXEC CICS READNEXT
007328*        DATASET(CLMS-FILE-ID)
007329*        SET    (ADDRESS OF CLAIM-MASTER)
007330*        RIDFLD (MSTR-KEY)
007331*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00014070' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303134303730' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007332
007333     IF WS-READNEXT-SWITCH = 1
007334         ADD 1                   TO WS-READNEXT-SWITCH
007335         GO TO 7720-READ-CLAIM-LOOP.
007336
007337 7730-END-BROWSE.
007338
007339     
      * EXEC CICS ENDBR
007340*        DATASET(CLMS-FILE-ID)
007341*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014082' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303134303832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007342
007343 7740-READ-CLAIM-UPDATE.
007344
007345     IF CL-COMPANY-CD  NOT = WS-SAVE-COMPANY-CD  OR
007346        CL-CARRIER     NOT = WS-SAVE-CARRIER     OR
007347        CL-CLAIM-NO    NOT = WS-SAVE-CLAIM-NO
007348        GO TO 7799-EXIT
007349     ELSE
007350        MOVE ZERO                TO WS-READNEXT-SWITCH.
007351
007352     
      * EXEC CICS READ
007353*        DATASET(CLMS-FILE-ID)
007354*        SET    (ADDRESS OF CLAIM-MASTER)
007355*        RIDFLD (MSTR-KEY)
007356*        UPDATE
007357*    END-EXEC.
      *    MOVE '&"S        EU         (   #00014095' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303134303935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007358
007359     if (cl-priority-cd = '8')
007360        and (pi-processor-id not = 'PEMA' and 'JMS '
007361             AND 'AMWA')
007362        MOVE ER-8003             TO EMI-ERROR
007363        PERFORM 9900-ERROR-FORMAT
007364                                 THRU 9900-EXIT
007365        MOVE -1                  TO MAINTL
007366        GO TO 8110-SEND-DATA
007367     end-if
007368
007369     IF PI-COMPANY-ID = 'CID' or 'AHL' OR 'FNL'
007370         MOVE 'Y'                TO CL-YESNOSW
007371     END-IF.
007372
007373     MOVE WS-ASSOC-CERT-TOTAL    TO CL-ASSOC-CERT-TOTAL.
007374     MOVE WS-ASSOC-CERT-SEQU     TO CL-ASSOC-CERT-SEQU.
007375
007376     
      * EXEC CICS REWRITE
007377*         DATASET(CLMS-FILE-ID)
007378*         FROM   (CLAIM-MASTER)
007379*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00014119' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303134313139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007380
007381     GO TO 7710-RESEQUENCE-CLAIMS.
007382
007383 7799-EXIT.
007384     EXIT.
007385
007386     EJECT
007387 7800-CHECK-AUTO-ACTIVITY.
007388
007389     
      * EXEC CICS HANDLE CONDITION
007390*        NOTFND   (7800-NOT-FOUND)
007391*    END-EXEC.
      *    MOVE '"$I                   ! I #00014132' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'4920233030303134313332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007392
007393     MOVE PI-COMPANY-ID              TO  CNTL-CO-ID.
007394     MOVE 'T'                        TO  CNTL-REC-TYPE.
007395     MOVE SPACES                     TO  CNTL-PROC-ID.
007396     MOVE +0                         TO  CNTL-SEQ-NO.
007397
007398     
      * EXEC CICS READ
007399*        DATASET   (CNTL-FILE-ID)
007400*        RIDFLD    (CNTL-KEY)
007401*        SET       (ADDRESS OF CONTROL-FILE)
007402*    END-EXEC.
      *    MOVE '&"S        E          (   #00014141' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134313431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007403
007404     IF CL-ACTIVITY-CODE IS NOT NUMERIC
007405         MOVE ZEROS                  TO  CL-ACTIVITY-CODE.
007406
007407     IF CL-ACTIVITY-CODE NOT = ZEROS
007408         MOVE CL-ACTIVITY-CODE                 TO  MISC-SUB
007409         IF MISC-SUB IS GREATER THAN +9
007410             SUBTRACT +9 FROM MISC-SUB
007411             MOVE CF-USER-RESET-SW (MISC-SUB)  TO  WS-RESET-SW
007412         ELSE
007413             MOVE CF-SYS-RESET-SW  (MISC-SUB)  TO  WS-RESET-SW
007414     ELSE
007415         MOVE 'Y'                              TO  WS-RESET-SW.
007416
007417     IF STATUSI = 'C' OR 'CLOSED'
007418         IF CF-SYS-ACTIVE-SW (7) = ' ' OR 'N'
007419             MOVE 'N'                TO  PI-REC-FOUND-SW
007420                                         PI-LETTER-SW
007421             GO TO 7800-EXIT.
007422
007423     IF STATUSI = 'C' OR 'CLOSED'
007424         IF CF-SYS-LETTER-ID (7) = SPACES OR LOW-VALUES
007425             MOVE 'N'                TO  PI-LETTER-SW
007426         ELSE
007427             MOVE 'Y'                TO  PI-LETTER-SW.
007428
007429     IF STATUSI = 'O' OR 'OPEN'
007430         IF CF-SYS-ACTIVE-SW (8) = ' ' OR 'N'
007431             MOVE 'N'                TO  PI-REC-FOUND-SW
007432                                         PI-LETTER-SW
007433             GO TO 7800-EXIT.
007434
007435     IF STATUSI = 'O' OR 'OPEN'
007436         IF CF-SYS-LETTER-ID (8) = SPACES OR LOW-VALUES
007437             MOVE 'N'                TO  PI-LETTER-SW
007438         ELSE
007439             MOVE 'Y'                TO  PI-LETTER-SW.
007440
007441     MOVE 'Y'                        TO  PI-REC-FOUND-SW.
007442     GO TO 7800-EXIT.
007443
007444 7800-NOT-FOUND.
007445
007446     MOVE 'N'                        TO  PI-REC-FOUND-SW
007447                                         PI-LETTER-SW.
007448
007449 7800-EXIT.
007450     EXIT.
007451
007452 7850-AUTO-LETTER-WRITER.
007453
007454     MOVE LOW-VALUES                 TO  W-1523-LINKDATA.
007455     MOVE PROGRAM-INTERFACE-BLOCK    TO  W-1523-COMMON-PI-DATA.
007456
007457     IF STATUSI = 'C' OR 'CLOSED'
007458         MOVE CF-SYS-LETTER-ID (7)   TO  W-1523-FORM-NUMBER
007459     ELSE
007460         MOVE CF-SYS-LETTER-ID (8)   TO  W-1523-FORM-NUMBER.
007461
007462     IF STATUSI = 'C' OR 'CLOSED'
007463         IF CF-SYS-RESEND-DAYS (7) NOT = ZEROS
007464             MOVE SAVE-BIN-DATE      TO  DC-BIN-DATE-1
007465             MOVE '6'                TO  DC-OPTION-CODE
007466             MOVE CF-SYS-RESEND-DAYS (7) TO  DC-ELAPSED-DAYS
007467             MOVE +0                 TO  DC-ELAPSED-MONTHS
007468             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
007469             IF NO-CONVERSION-ERROR
007470                 MOVE DC-BIN-DATE-2  TO  W-1523-RESEND-DATE
007471             ELSE
007472                 MOVE LOW-VALUES     TO  W-1523-RESEND-DATE.
007473
007474     IF STATUSI = 'C' OR 'CLOSED'
007475         IF CF-SYS-FOLLOW-UP-DAYS (7) NOT = ZEROS
007476             MOVE SAVE-BIN-DATE      TO  DC-BIN-DATE-1
007477             MOVE '6'                TO  DC-OPTION-CODE
007478             MOVE CF-SYS-FOLLOW-UP-DAYS (7) TO  DC-ELAPSED-DAYS
007479             MOVE +0                 TO  DC-ELAPSED-MONTHS
007480             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
007481             IF NO-CONVERSION-ERROR
007482                 MOVE DC-BIN-DATE-2  TO  W-1523-FOLLOW-UP-DATE
007483             ELSE
007484                 MOVE LOW-VALUES     TO  W-1523-FOLLOW-UP-DATE.
007485
007486     IF STATUSI = 'O' OR 'OPEN'
007487         IF CF-SYS-RESEND-DAYS (8) NOT = ZEROS
007488             MOVE SAVE-BIN-DATE      TO  DC-BIN-DATE-1
007489             MOVE '6'                TO  DC-OPTION-CODE
007490             MOVE CF-SYS-RESEND-DAYS (8) TO  DC-ELAPSED-DAYS
007491             MOVE +0                 TO  DC-ELAPSED-MONTHS
007492             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
007493             IF NO-CONVERSION-ERROR
007494                 MOVE DC-BIN-DATE-2  TO  W-1523-RESEND-DATE
007495             ELSE
007496                 MOVE LOW-VALUES     TO  W-1523-RESEND-DATE.
007497
007498     IF STATUSI = 'O' OR 'OPEN'
007499         IF CF-SYS-FOLLOW-UP-DAYS (8) NOT = ZEROS
007500             MOVE SAVE-BIN-DATE      TO  DC-BIN-DATE-1
007501             MOVE '6'                TO  DC-OPTION-CODE
007502             MOVE CF-SYS-FOLLOW-UP-DAYS (8) TO  DC-ELAPSED-DAYS
007503             MOVE +0                 TO  DC-ELAPSED-MONTHS
007504             PERFORM 9800-CONVERT-DATE THRU 9800-EXIT
007505             IF NO-CONVERSION-ERROR
007506                 MOVE DC-BIN-DATE-2  TO  W-1523-FOLLOW-UP-DATE
007507             ELSE
007508                 MOVE LOW-VALUES     TO  W-1523-FOLLOW-UP-DATE.
007509
007510     
      * EXEC CICS LINK
007511*        PROGRAM    (LINK-1523)
007512*        COMMAREA   (W-1523-LINKDATA)
007513*        LENGTH     (W-1523-COMM-LENGTH)
007514*    END-EXEC.
      *    MOVE '."C                   (   #00014253' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303134323533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-1523, 
                 W-1523-LINKDATA, 
                 W-1523-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007515
007516     IF W-1523-ERROR-CODE = ZEROS
007517         GO TO 7850-EXIT.
007518
007519     IF W-1523-FATAL-ERROR
007520         MOVE ER-0802                TO  EMI-ERROR
007521         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
007522
007523     IF W-1523-ERROR-CODE = 0191
007524         MOVE ER-0803                TO  EMI-ERROR
007525         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
007526
007527 7850-EXIT.
007528     EXIT.
007529
007530
007531 7900-READ-BENEFIT.
007532
007533     move spaces                 to cntl-key
007534     move pi-company-id          to cntl-co-id
007535     if cl-claim-type = 'L' or 'O' or 'P'
007536        move cm-lf-benefit-cd    to cntl-benefit
007537                                    ws-ben-hold
007538        move '4'                 to cntl-rec-type
007539                                    ws-rec-type
007540     else
007541        move cm-ah-benefit-cd    to cntl-benefit
007542                                    ws-ben-hold
007543        move '5'                 to cntl-rec-type
007544                                    ws-rec-type
007545     end-if
007546     MOVE ZEROS                  TO CNTL-SEQ-NO
007547
007548     .
007549 7900-READ-FILE.
007550
007551     PERFORM 7975-READ-CNTL-GTEQ THRU 7975-EXIT
007552
007553     if not WS-RESP-NORMAL
007554        go to 7900-not-found
007555     end-if
007556
007557     IF (PI-COMPANY-ID NOT = CF-COMPANY-ID)  OR
007558        (WS-REC-TYPE   NOT = CF-RECORD-TYPE)
007559        GO TO 7900-NOT-FOUND
007560     END-IF
007561
007562     MOVE 1                      TO SUB
007563
007564     .
007565 7900-LOOP.
007566     IF SUB = 9
007567         GO TO 7900-NOT-FOUND
007568     END-IF
007569
007570     IF WS-BEN-HOLD <> CF-BENEFIT-CODE (SUB)
007571        ADD 1                    TO SUB
007572        GO TO 7900-LOOP
007573     END-IF
007574
007575     MOVE CF-SPECIAL-CALC-CD (SUB)  TO WS-SPECIAL-CALC-CD.
007576     if (ws-special-calc-cd = 'O')
007577                OR
007578        (pi-company-id = 'DCC' and cl-carrier = '7')
007579        set mob-cert to true
007580     end-if
007581     GO TO 7900-EXIT
007582
007583     .
007584 7900-NOT-FOUND.
007585     MOVE SPACES                 TO WS-special-calc-cd
007586
007587     .
007588 7900-EXIT.
007589      EXIT.
007590
007591 7975-READ-CNTL-GTEQ.
007592
007593     
      * EXEC CICS READ
007594*         DATASET   ('ELCNTL')
007595*         SET       (ADDRESS OF CONTROL-FILE)
007596*         RIDFLD    (CNTL-KEY)
007597*         resp      (ws-response)
007598*         GTEQ
007599*     END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (  N#00014336' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303134333336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007600
007601 7975-EXIT.
007602      EXIT.
007603
007604 7990-get-lo-hi-acct-dates.
007605
007606     MOVE PI-COMPANY-CD       TO ACCT-company-code
007607     MOVE PI-CARRIER          TO ACCT-CARRIER
007608     MOVE PI-GROUPING         TO ACCT-GROUP
007609     MOVE PI-STATE            TO ACCT-STATE
007610     MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
007611     MOVE low-values          TO ACCT-date
007612     MOVE ACCT-KEY            TO SAVE-ACCT-KEY
007613
007614     move spaces              to ws-i-say-stop-ind
007615                                 ws-eracct-startbr-ind
007616                                 ws-acct-status
007617
007618     
      * EXEC CICS STARTBR
007619*         DATASET    ('ERACCT')
007620*         RIDFLD     (ACCT-KEY)
007621*         GTEQ
007622*         resp       (ws-response)
007623*    END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00014361' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303134333631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
007624
007625     if ws-resp-normal
007626        set eracct-browse-started to true
007627     end-if
007628
007629     perform until i-say-stop
007630        
      * EXEC CICS READNEXT
007631*          DATASET ('ERACCT')
007632*          RIDFLD  (ACCT-KEY)
007633*          SET     (ADDRESS OF ACCOUNT-MASTER)
007634*          resp    (WS-RESPONSE)
007635*       END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00014373' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303134333733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
007636
007637        IF WS-RESP-NORMAL
007638           AND save-acct-key(1:20) =
007639                       AM-CONTROL-PRIMARY (1:20)
007640           if ws-lo-acct-dt = low-values
007641              move am-effective-dt
007642                                 to ws-lo-acct-dt
007643           end-if
007644           if am-expiration-dt > ws-hi-acct-dt
007645              move am-expiration-dt
007646                                 to ws-hi-acct-dt
007647           end-if
007648           move am-status        to ws-acct-status
007649        else
007650           set i-say-stop to true
007651        end-if
007652     end-perform
007653
007654     if eracct-browse-started
007655        
      * exec cics endbr
007656*          dataset('ERACCT')
007657*       end-exec
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014398' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303134333938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
007658     end-if
007659
007660     perform 7900-READ-BENEFIT   thru 7900-exit
007661
007662     .
007663 7990-exit.
007664     exit.
007665
007666
007667 8000-CREATE-DMO-REC.
007668     MOVE PI-COMPANY-CD          TO NOTE-COMP-CD.
007669     MOVE CL-CERT-KEY-DATA       TO NOTE-CERT-KEY.
007670     MOVE CL-CERT-NO             TO NOTE-CERT-NO.
007671
007672     
      * EXEC CICS HANDLE CONDITION
007673*         NOTFND   (8000-NOTE-NOT-FOUND)
007674*    END-EXEC.
      *    MOVE '"$I                   ! J #00014415' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'4A20233030303134343135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007675
007676     
      * EXEC CICS READ
007677*         DATASET(NOTE-FILE-ID)
007678*         SET    (ADDRESS OF CERTIFICATE-NOTE)
007679*         RIDFLD (NOTE-KEY)
007680*    END-EXEC.
      *    MOVE '&"S        E          (   #00014419' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134343139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NOTE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 NOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-NOTE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007681
007682     MOVE SPACES                 TO DCT-COMMUNICATION-AREA.
007683     MOVE CL-BENEFICIARY         TO DCT-LOGIC-BENEFICIARY-ID.
007684     MOVE CL-CCN                 TO DCT-CREDIT-CARD-NUMBER.
007685
007686     IF CL-CERT-GROUPING (5:2) = SPACES OR ZEROS
007687         MOVE 'CC'               TO DCT-PRODUCT-CODE
007688     ELSE
007689         MOVE CL-CERT-GROUPING (5:2) TO DCT-PRODUCT-CODE.
007690
007691     MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.
007692     MOVE '02'                   TO DCT-COLUMN-ID-REQUESTED.
007693
007694     
      * EXEC CICS LINK
007695*        PROGRAM    ('DLO006')
007696*        COMMAREA   (DCT-COMMUNICATION-AREA)
007697*        LENGTH     (WS-DCT-LENGTH)
007698*    END-EXEC.
           MOVE 'DLO006' TO DFHEIV1
      *    MOVE '."C                   (   #00014437' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303134343337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DCT-COMMUNICATION-AREA, 
                 WS-DCT-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007699
007700     IF  DCT-RETURN-CODE = 'OK'
007701         GO TO 8000-CONT.
007702
007703     IF DCT-RETURN-CODE = '01' OR '02'
007704         GO TO 8000-EXIT.
007705
007706     IF DCT-RETURN-CODE = '03'
007707         MOVE ER-0951            TO EMI-ERROR
007708         MOVE -1                 TO MAINTL
007709         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007710         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007711         GO TO 8110-SEND-DATA.
007712
007713     IF DCT-RETURN-CODE = '04'
007714         MOVE ER-0946            TO EMI-ERROR
007715         MOVE -1                 TO MAINTL
007716         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007717         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007718         GO TO 8110-SEND-DATA.
007719
007720     IF DCT-RETURN-CODE = '05'
007721         MOVE ER-0947            TO EMI-ERROR
007722         MOVE -1                 TO MAINTL
007723         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007724         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007725         GO TO 8110-SEND-DATA.
007726
007727     IF DCT-RETURN-CODE = '06'
007728         MOVE ER-0921            TO EMI-ERROR
007729         MOVE -1                 TO MAINTL
007730         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007731         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007732         GO TO 8110-SEND-DATA.
007733
007734     IF DCT-RETURN-CODE = '07'
007735         MOVE ER-0919            TO EMI-ERROR
007736         MOVE -1                 TO MAINTL
007737         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007738         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007739         GO TO 8110-SEND-DATA.
007740
007741     IF DCT-RETURN-CODE = '08'
007742         MOVE ER-0948            TO EMI-ERROR
007743         MOVE -1                 TO MAINTL
007744         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007745         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007746         GO TO 8110-SEND-DATA.
007747
007748     IF DCT-RETURN-CODE = 'N1'
007749         MOVE ER-0950            TO EMI-ERROR
007750         MOVE -1                 TO MAINTL
007751         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007752         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007753         GO TO 8110-SEND-DATA.
007754
007755     IF DCT-RETURN-CODE = 'E1'
007756         MOVE ER-0974            TO EMI-ERROR
007757         MOVE -1                 TO MAINTL
007758         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007759         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007760         GO TO 8110-SEND-DATA.
007761
007762     IF DCT-RETURN-CODE = 'E2'
007763         MOVE ER-0975            TO EMI-ERROR
007764         MOVE -1                 TO MAINTL
007765         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007766         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007767         GO TO 8110-SEND-DATA.
007768
007769     IF DCT-RETURN-CODE NOT = 'OK'
007770         MOVE ER-0949            TO EMI-ERROR
007771         MOVE -1                 TO MAINTL
007772         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007773         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007774         GO TO 8110-SEND-DATA.
007775
007776 8000-CONT.
007777
007778     MOVE SPACES                 TO DMO-COMMUNICATION-AREA.
007779     MOVE 'CS'                   TO DM-RECORD-TYPE.
007780     MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.
007781     MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.
007782     MOVE CL-CLAIM-NO            TO DM-CLAIM-NO.
007783     MOVE CL-CERT-NO (4:1)       TO DM-CLAIM-TYPE.
007784     MOVE CL-CCN                 TO DM-CREDIT-CARD-NUMBER.
007785     MOVE SAVE-DATE-CCYYMMDD     TO DM-STATUS-DATE.
007786
007787     MOVE CL-INSURED-LAST-NAME   TO W-NAME-LAST.
007788     MOVE CL-INSURED-1ST-NAME    TO W-NAME-FIRST.
007789     MOVE CL-INSURED-MID-INIT    TO W-NAME-MIDDLE.
007790     PERFORM 8050-FORMAT-LAST-NAME-1ST THRU 8050-EXIT.
007791     MOVE WS-NAME-WORK           TO DM-INSURED-NAME.
007792
007793     IF STATUSI = 'OPEN' OR 'O'
007794         MOVE 'R'                TO DM-STAT-CHANGE-TYPE
007795      ELSE
007796         MOVE 'C'                TO DM-STAT-CHANGE-TYPE.
007797
007798     IF STATUSI = 'OPEN' OR 'O'
007799        IF CL-NO-OF-PMTS-MADE = 0
007800         MOVE '1'                TO DM-CLAIM-STATUS
007801      ELSE
007802         MOVE '2'                TO DM-CLAIM-STATUS.
007803
007804     IF STATUSI = 'CLOSED' OR 'C'
007805        IF NOT FINAL-PAID
007806         MOVE '3'                TO DM-CLAIM-STATUS
007807      ELSE
007808         MOVE '4'                TO DM-CLAIM-STATUS.
007809
007810     MOVE CL-CARRIER             TO DM-STAT-CARRIER.
007811
007812     
      * EXEC CICS LINK
007813*        PROGRAM    ('DLO025')
007814*        COMMAREA   (DMO-COMMUNICATION-AREA)
007815*        LENGTH     (WS-DMO-LENGTH)
007816*    END-EXEC.
           MOVE 'DLO025' TO DFHEIV1
      *    MOVE '."C                   (   #00014555' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303134353535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DMO-COMMUNICATION-AREA, 
                 WS-DMO-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007817
007818     IF  DM-RETURN-CODE = 'OK'
007819         GO TO 8000-EXIT.
007820
007821     IF DM-RETURN-CODE = '01'
007822         MOVE ER-8051            TO EMI-ERROR
007823         MOVE -1                 TO MAINTL
007824         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007825         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007826         GO TO 8110-SEND-DATA.
007827
007828     IF DM-RETURN-CODE = '02'
007829         MOVE ER-8052            TO EMI-ERROR
007830         MOVE -1                 TO MAINTL
007831         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007832         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007833         GO TO 8110-SEND-DATA.
007834
007835     IF DM-RETURN-CODE = '03'
007836         MOVE ER-8053            TO EMI-ERROR
007837         MOVE -1                 TO MAINTL
007838         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007839         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007840         GO TO 8110-SEND-DATA.
007841
007842     IF DM-RETURN-CODE = '04'
007843         MOVE ER-8054            TO EMI-ERROR
007844         MOVE -1                 TO MAINTL
007845         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007846         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007847         GO TO 8110-SEND-DATA.
007848
007849     IF DM-RETURN-CODE = '05'
007850         MOVE ER-8055            TO EMI-ERROR
007851         MOVE -1                 TO MAINTL
007852         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007853         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007854         GO TO 8110-SEND-DATA.
007855
007856     IF DM-RETURN-CODE = '06'
007857         MOVE ER-8056            TO EMI-ERROR
007858         MOVE -1                 TO MAINTL
007859         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007860         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007861         GO TO 8110-SEND-DATA.
007862
007863     IF DM-RETURN-CODE = '07'
007864         MOVE ER-8057            TO EMI-ERROR
007865         MOVE -1                 TO MAINTL
007866         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007867         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007868         GO TO 8110-SEND-DATA.
007869
007870     IF DM-RETURN-CODE = '08'
007871         MOVE ER-8058            TO EMI-ERROR
007872         MOVE -1                 TO MAINTL
007873         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007874         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007875         GO TO 8110-SEND-DATA.
007876
007877     IF DM-RETURN-CODE = '09'
007878         MOVE ER-8059            TO EMI-ERROR
007879         MOVE -1                 TO MAINTL
007880         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007881         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007882         GO TO 8110-SEND-DATA.
007883
007884     IF DM-RETURN-CODE = '10'
007885         MOVE ER-8060            TO EMI-ERROR
007886         MOVE -1                 TO MAINTL
007887         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007888         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007889         GO TO 8110-SEND-DATA.
007890
007891     IF DM-RETURN-CODE = '11'
007892         MOVE ER-8061            TO EMI-ERROR
007893         MOVE -1                 TO MAINTL
007894         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007895         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007896         GO TO 8110-SEND-DATA.
007897
007898     IF DM-RETURN-CODE = '12'
007899         MOVE ER-8062            TO EMI-ERROR
007900         MOVE -1                 TO MAINTL
007901         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007902         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007903         GO TO 8110-SEND-DATA.
007904
007905     IF DM-RETURN-CODE = '13'
007906         MOVE ER-8063            TO EMI-ERROR
007907         MOVE -1                 TO MAINTL
007908         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007909         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007910         GO TO 8110-SEND-DATA.
007911
007912     IF DM-RETURN-CODE = '14'
007913         MOVE ER-8064            TO EMI-ERROR
007914         MOVE -1                 TO MAINTL
007915         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007916         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007917         GO TO 8110-SEND-DATA.
007918
007919     IF DM-RETURN-CODE = '15'
007920         MOVE ER-8065            TO EMI-ERROR
007921         MOVE -1                 TO MAINTL
007922         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007923         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007924         GO TO 8110-SEND-DATA.
007925
007926     IF DM-RETURN-CODE = '16'
007927         MOVE ER-8154            TO EMI-ERROR
007928         MOVE -1                 TO MAINTL
007929         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007930         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007931         GO TO 8110-SEND-DATA.
007932
007933     IF DM-RETURN-CODE = '17'
007934         MOVE ER-8155            TO EMI-ERROR
007935         MOVE -1                 TO MAINTL
007936         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007937         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007938         GO TO 8110-SEND-DATA.
007939
007940     IF DM-RETURN-CODE = 'N1'
007941         MOVE ER-8152            TO EMI-ERROR
007942         MOVE -1                 TO MAINTL
007943         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007944         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007945         GO TO 8110-SEND-DATA.
007946
007947     IF DM-RETURN-CODE = 'E1'
007948         MOVE ER-8153            TO EMI-ERROR
007949         MOVE -1                 TO MAINTL
007950         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007951         PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT
007952         GO TO 8110-SEND-DATA.
007953
007954     MOVE ER-8066                TO EMI-ERROR.
007955     MOVE -1                     TO MAINTL.
007956     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
007957     PERFORM 8070-UNLOCK-CLAIM-MSTR THRU 8070-EXIT.
007958     GO TO 8110-SEND-DATA.
007959
007960 8000-NOTE-NOT-FOUND.
007961     
      * EXEC CICS SYNCPOINT ROLLBACK END-EXEC.
      *    MOVE '6"R                   !   #00014704' TO DFHEIV0
           MOVE X'362252202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303134373034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007962     MOVE ER-0954                TO EMI-ERROR.
007963     MOVE -1                     TO MAINTL.
007964     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
007965     GO TO 8110-SEND-DATA.
007966
007967 8000-EXIT.
007968     EXIT.
007969 EJECT
007970 8050-FORMAT-LAST-NAME-1ST.
007971*****************************************************************
007972*             M O V E   N A M E   R O U T I N E                 *
007973*                                                               *
007974*     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
007975*     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
007976*     FIELDS IN THE FOLLOWING WORKING-STORAGE FIELDS.           *
007977*                                                               *
007978*                  FIELD               VALUE                    *
007979*                  -----               -----                    *
007980*           W-NAME-LAST    (CL15)      SMITH                    *
007981*           W-NAME-FIRST   (CL15)      JOHN                     *
007982*           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
007983*                                                               *
007984*     AFTER NAME HAS BEEN MOVED WS-NAME-WORK WILL CONTAIN       *
007985*                SMITH, JOHN ALLEN                              *
007986*                     OR                                        *
007987*                SMITH, JOHN A.                                 *
007988*                                                               *
007989*     TO USE THIS ROUTINE YOU NEED THE WORKING-STORAGE          *
007990*     COPYBOOK, ELCNWA.                                         *
007991*****************************************************************.
007992
007993     MOVE SPACES                 TO WS-NAME-WORK-AREA.
007994     MOVE ZERO                   TO WS-NAME-SW.
007995     SET NWA-INDEX               TO +1.
007996
007997     IF W-NAME-LAST   = SPACES  AND
007998        W-NAME-MIDDLE = SPACES
007999          MOVE +1                TO WS-NAME-SW.
008000
008001     MOVE W-NAME-LAST            TO WS-NAME-WORK2.
008002     PERFORM 8060-MOVE-NAME THRU 8060-EXIT.
008003
008004     MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
008005     PERFORM 8060-MOVE-NAME THRU 8060-EXIT.
008006
008007     SET NWA-INDEX UP BY +1.
008008
008009     IF W-NAME-MIDDLE NOT = SPACES
008010         IF W-NAME-MIDDLE-2 = SPACES
008011             MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
008012             SET NWA-INDEX UP BY +1
008013             MOVE '.'            TO WS-NW (NWA-INDEX)
008014             SET NWA-INDEX UP BY +2
008015         ELSE
008016             MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
008017             PERFORM 8060-MOVE-NAME THRU 8060-EXIT.
008018
008019 8050-EXIT.
008020     EXIT.
008021
008022 EJECT
008023 8060-MOVE-NAME.
008024     IF WS-NAME-SW GREATER THAN +1
008025         GO TO 8060-EXIT.
008026
008027     IF WS-NAME-WORK2 = SPACES
008028         GO TO 8060-EXIT.
008029
008030     SET NWA-INDEX2            TO +1.
008031     SET NWA-INDEX3            TO +2.
008032
008033 8060-MOVE-NAME-CYCLE.
008034     MOVE WS-NW2 (NWA-INDEX2)  TO WS-NW (NWA-INDEX).
008035
008036     IF NWA-INDEX LESS THAN +30
008037         SET NWA-INDEX UP BY +1
008038     ELSE
008039         ADD +2 TO  WS-NAME-SW
008040         GO TO 8060-EXIT.
008041
008042     IF NWA-INDEX2 LESS THAN +20
008043         SET NWA-INDEX3 UP BY +1
008044         SET NWA-INDEX2 UP BY +1.
008045
008046     IF WS-NW2 (NWA-INDEX2) = SPACES  AND
008047        WS-NW2 (NWA-INDEX3) = SPACES
008048         IF WS-NAME-SW = ZERO
008049             MOVE ','            TO WS-NW (NWA-INDEX)
008050             SET NWA-INDEX UP BY +2
008051             MOVE +1             TO WS-NAME-SW
008052             GO TO 8060-EXIT
008053         ELSE
008054             GO TO 8060-EXIT.
008055
008056     GO TO 8060-MOVE-NAME-CYCLE.
008057
008058 8060-EXIT.
008059     EXIT.
008060
008061     EJECT
008062
008063 8070-UNLOCK-CLAIM-MSTR.
008064     
      * EXEC CICS UNLOCK
008065*        DATASET   (CLMS-FILE-ID)
008066*    END-EXEC.
      *    MOVE '&*                    #   #00014807' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303134383037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLMS-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008067
008068 8070-EXIT.
008069      EXIT.
008070
008071     EJECT
008072 8100-SEND-MAP.
008073     PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.
008074
008075     move al-uanon               to accswa
008076     IF NOT PI-NO-CARRIER-SECURITY
008077        MOVE AL-SANOF            TO CERTCARA.
008078     IF NOT PI-NO-ACCOUNT-SECURITY
008079        MOVE AL-SANOF            TO CERTACTA.
008080
008081     IF PI-PROCESSOR-ID = 'LGXX'
008082        MOVE AL-UANON            TO CCNOA.
008083
008084     IF PI-COMPANY-ID = 'DMD'
008085        NEXT SENTENCE
008086     ELSE
008087        MOVE AL-SADOF            TO PFKEY6A.
008088
008089     IF NOT (PI-COMPANY-ID = 'DCC' OR 'VPP')
008090        MOVE AL-SANOF TO EXTENSA
008091     END-IF
008092
008093
008094     IF PI-USES-PAID-TO
008095        MOVE 'PAID TO  :' TO PTHRHDGO.
008096
008097     
      * EXEC CICS SEND
008098*         MAP     ('EL131A')
008099*         MAPSET  ('EL131S')
008100*         ERASE
008101*         FREEKB
008102*         CURSOR
008103*    END-EXEC.
           MOVE 'EL131A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL131S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00014840' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'204620204820202020202C20' &
                X'2020233030303134383430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL131AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008104
008105     GO TO 9000-RETURN-TRANS.
008106
008107 8110-SEND-DATA.
008108     PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.
008109
008110     IF PI-USES-PAID-TO
008111        MOVE 'PAID TO  :' TO PTHRHDGO.
008112
008113     IF PI-PROCESSOR-ID = 'LGXX'
008114        MOVE AL-UANON            TO CCNOA.
008115
008116     IF PI-COMPANY-ID = 'DMD'
008117        NEXT SENTENCE
008118     ELSE
008119        MOVE AL-SADOF            TO PFKEY6A.
008120
008121     
      * EXEC CICS SEND
008122*         MAP      ('EL131A')
008123*         MAPSET   ('EL131S')
008124*         DATAONLY
008125*         FREEKB
008126*         CURSOR
008127*    END-EXEC.
           MOVE 'EL131A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL131S' TO DFHEIV2
      *    MOVE '8$D    CT    F  H     ,   #00014864' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'204620204820202020202C20' &
                X'2020233030303134383634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL131AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008128
008129     GO TO 9000-RETURN-TRANS.
008130
008131 8120-FORMAT-TIME-DATE.
008132
008133     perform 8140-check-for-prev-errors
008134                                 thru 8140-exit
008135
008136     IF SKIP-ATTRIBUTE = 'Y'
008137        MOVE SPACES              TO SKIP-ATTRIBUTE
008138        MOVE ER-0598             TO EMI-ERROR
008139        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008140        GO TO 8125-SKIP-ATTRIBUTE.
008141
008142*     IF PI-COMPANY-ID = 'DMD'
008143      IF PI-PROCESSOR-ID <> 'EMER'
008144         MOVE AL-PANOF           TO CERTEFFA  CERTACTA  CERTSTA
008145                                    CERTCARA  CERTGRPA CERTA SUFXA
008146
008147     MOVE AL-PANOF               TO CRTLNMEA  CRTFNMEA  CRTINITA
008148                                    ISSAGEA   JNTLNMEA  JNTFNMEA
008149                                    JNTINITA  JNTAGEA   APRA
008150                                    LCVCDA    LCVOTRMA  LCVRTRMA
008151                                    LCVBENEA  LCVFORMA  LCVCNDTA
008152                                    LCVEXITA  LCVSTATA  LCVKINDA
008153                                    ACVCDA    ACVOTRMA  ACVRTRMA
008154                                    ACVBENEA  ACVFORMA  ACVCNDTA
008155                                    ACVEXITA  ACVSTATA  ACVKINDA
008156                                    PMTFREQA  INDGRPA   PREMTYPA
008157                                    REINCDA   LCVRATEA  ACVRATEA
008158                                    ADDONDTA.
008159
008160*    IF PI-PROCESSOR-ID NOT = 'LGXX'
008161*       IF PI-COMPANY-ID = 'DMD'
008162*          MOVE AL-SANOF         TO TYPEA     CERTA     CERTSTA
008163*                                   SUFXA
008164*                                   PDAMTA    NODAYSA   NOPMTSA
008165*                                   INCA      OCCA
008166*          IF NOT SYSTEM-MODIFY-CAP
008167*              MOVE AL-SANOF         TO PDTHRUA.
008168
008169     IF PI-COMPANY-ID NOT = 'CRI' AND 'PEM' AND 'NCL'
008170         MOVE AL-SANOF           TO LCVRATEA  ACVRATEA.
008171
008172 8125-SKIP-ATTRIBUTE.
008173
008174     MOVE SAVE-DATE              TO DATEO.
008175     
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
008176*    END-EXEC
      *    MOVE '0"A                   "   #00014918' TO DFHEIV0
           MOVE X'302241202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303134393138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
008177     
      * EXEC CICS FORMATTIME
008178*              ABSTIME(LCP-CICS-TIME)
008179*              TIME(LCP-TIME-OF-DAY-XX)
008180*    END-EXEC
      *    MOVE 'j$(     (             #   #00014920' TO DFHEIV0
           MOVE X'6A2428202020202028202020' &
                X'202020202020202020202320' &
                X'2020233030303134393230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
008181     MOVE  LCP-TIME-OF-DAY-68 TO TIME-IN.
008182     MOVE UN-HOURS               TO FOR-HOURS.
008183     MOVE UN-MINUTES             TO FOR-MINUTES.
008184     MOVE TIME-OUT               TO TIMEO.
008185     MOVE PI-COMPANY-ID          TO COMPO.
008186     MOVE PI-PROCESSOR-ID        TO USERIDO.
008187     MOVE MAP-ID                 TO PI-CURRENT-SCREEN-NO.
008188     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
008189     MOVE EMI-MESSAGE-AREA (1)   TO MSG1O.
008190     MOVE EMI-MESSAGE-AREA (2)   TO MSG2O.
008191
008192 8130-EXIT.
008193     EXIT.
008194
008195 8140-check-for-prev-errors.
008196
008197     MOVE PI-COMPANY-CD          TO trlr-company-cd
008198     MOVE PI-CARRIER             TO trlr-carrier
008199     MOVE PI-CLAIM-NO            TO trlr-claim-no
008200     MOVE PI-CERT-NO             TO trlr-cert-no
008201     MOVE +95                    TO TRLR-SEQ-NO
008202
008203     
      * EXEC CICS READ
008204*       DATASET  ('ELTRLR')
008205*       SET      (ADDRESS OF ACTIVITY-TRAILERS)
008206*       RIDFLD   (TRLR-KEY)
008207*       RESP     (WS-RESPONSE)
008208*    END-EXEC
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00014946' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303134393436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
008209
008210     if ws-resp-normal
008211        perform varying s1 from +1 by +1 until
008212           at-note-error-no (s1) = spaces
008213           if at-note-error-no (s1) = '1652'
008214              continue
008215           else
008216              move at-note-error-no (s1)
008217                                 to emi-error
008218              move -1 to maintl
008219              if at-note-error-no (s1) = '1653'
008220                 evaluate true
008221                    when pi-claim-type = 'L'
008222                       move '  LF  '
008223                        to emi-claim-type
008224                    when pi-claim-type = 'I'
008225                       move '  IU  '
008226                        to emi-claim-type
008227                    WHEN PI-CLAIM-TYPE = 'F'
008228                       MOVE '  FL  '
008229                        TO EMI-CLAIM-TYPE
008230                    WHEN PI-CLAIM-TYPE = 'B'
008231                       MOVE '  BR  '
008232                        TO EMI-CLAIM-TYPE
008233                    WHEN PI-CLAIM-TYPE = 'H'
008234                       MOVE '  HS  '
008235                        TO EMI-CLAIM-TYPE
008236                    WHEN PI-CLAIM-TYPE = 'O'
008237                       MOVE '  OT  '
008238                        TO EMI-CLAIM-TYPE
008239                    when other
008240                       move '  AH  '
008241                        to emi-claim-type
008242                 end-evaluate
008243              end-if
008244              PERFORM 9900-ERROR-FORMAT
008245                                 THRU 9900-EXIT
008246           end-if
008247*          if at-note-error-no (s1) = '1653'
008248*             perform varying emi-sub from 1 by 1 until
008249*                emi-sub > 3
008250*                if emi-error-number (emi-sub) = '1653'
008251*                   evaluate true
008252*                      when pi-claim-type = 'L'
008253*                         move '  LF  '
008254*                          to emi-claim-type
008255*                      when pi-claim-type = 'I'
008256*                         move '  IU  '
008257*                          to emi-claim-type
008258*                      when other
008259*                         move '  AH  '
008260*                          to emi-claim-type
008261*                   end-evaluate
008262*                end-if
008263*             end-perform
008264*          end-if
008265        end-perform
008266     else
008267        display ' resp not normal ' ws-response ' '
008268        trlr-key (2:19)
008269     end-if
008270
008271     .
008272 8140-exit.
008273     exit.
008274
008275 8150-ENTERED-CLAIM-NOTFOUND.
008276
008277     MOVE ER-0204                TO EMI-ERROR.
008278     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
008279     MOVE -1                     TO MAINTL.
008280     GO TO 8100-SEND-MAP.
008281
008282 8200-RETURN-PRIOR.
008283     MOVE SPACE                  TO PI-RETURN-CD-1.
008284     MOVE PI-RETURN-TO-PROGRAM   TO CALL-PGM.
008285     GO TO 9200-XCTL.
008286
008287 8300-GET-HELP.
008288     MOVE XCTL-EL010             TO CALL-PGM.
008289     GO TO 9200-XCTL.
008290
008291 8400-RETURN-MASTER.
008292     MOVE SPACE                  TO PI-RETURN-CD-1.
008293     MOVE XCTL-EL126             TO CALL-PGM.
008294     GO TO 9200-XCTL.
008295
008296 8500-TRLR-MNT.
008297     IF PI-RETURN-CD-1 = 'X'
008298         MOVE ER-0311            TO EMI-ERROR
008299         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008300         MOVE -1                 TO MAINTL
008301         GO TO 8110-SEND-DATA.
008302
008303     
      * EXEC CICS WRITEQ TS
008304*         QUEUE     (PI-KEY)
008305*         FROM      (PROGRAM-INTERFACE-BLOCK)
008306*         LENGTH    (PI-COMM-LENGTH)
008307*    END-EXEC.
      *    MOVE '*"     L              ''   #00015046' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303135303436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008308
008309     MOVE XCTL-EL142             TO CALL-PGM.
008310     GO TO 9200-XCTL.
008311
008312 8600-ADDR-MNT.
008313     IF PI-RETURN-CD-1 = 'X'
008314         MOVE ER-0311            TO EMI-ERROR
008315         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008316         MOVE -1                 TO MAINTL
008317         GO TO 8110-SEND-DATA.
008318
008319     
      * EXEC CICS WRITEQ TS
008320*         QUEUE     (PI-KEY)
008321*         FROM      (PROGRAM-INTERFACE-BLOCK)
008322*         LENGTH    (PI-COMM-LENGTH)
008323*    END-EXEC.
      *    MOVE '*"     L              ''   #00015062' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303135303632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008324
008325     MOVE XCTL-EL141             TO CALL-PGM.
008326     GO TO 9200-XCTL.
008327
008328 8700-CERT-MNT.
008329     IF PI-RETURN-CD-1 = 'X'
008330         MOVE ER-7690            TO EMI-ERROR
008331         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008332         MOVE -1                 TO MAINTL
008333         GO TO 8110-SEND-DATA.
008334
008335     
      * EXEC CICS WRITEQ TS
008336*         QUEUE     (PI-KEY)
008337*         FROM      (PROGRAM-INTERFACE-BLOCK)
008338*         LENGTH    (PI-COMM-LENGTH)
008339*    END-EXEC.
      *    MOVE '*"     L              ''   #00015078' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303135303738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008340
008341     MOVE XCTL-EL1273            TO CALL-PGM.
008342     GO TO 9200-XCTL.
008343
008344 8725-BENEFICIARY-MNT.
008345     IF PI-RETURN-CD-1 = 'X'
008346         MOVE ER-0311            TO EMI-ERROR
008347         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008348         MOVE -1                 TO MAINTL
008349         GO TO 8110-SEND-DATA.
008350
008351     
      * EXEC CICS WRITEQ TS
008352*         QUEUE     (PI-KEY)
008353*         FROM      (EL131AI)
008354*         LENGTH    (MAP-LENGTH)
008355*    END-EXEC.
      *    MOVE '*"     L              ''   #00015094' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303135303934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 EL131AI, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008356
008357     
      * EXEC CICS WRITEQ TS
008358*         QUEUE     (PI-KEY)
008359*         FROM      (PROGRAM-INTERFACE-BLOCK)
008360*         LENGTH    (PI-COMM-LENGTH)
008361*    END-EXEC.
      *    MOVE '*"     L              ''   #00015100' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303135313030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008362
008363     MOVE XCTL-EL114             TO CALL-PGM.
008364     GO TO 9200-XCTL.
008365
008366 8750-DMD-CLM-FIX.
008367     
      * EXEC CICS WRITEQ TS
008368*         QUEUE     (PI-KEY)
008369*         FROM      (PROGRAM-INTERFACE-BLOCK)
008370*         LENGTH    (PI-COMM-LENGTH)
008371*    END-EXEC.
      *    MOVE '*"     L              ''   #00015110' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303135313130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-KEY, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008372
008373     MOVE XCTL-EL400DMD          TO CALL-PGM.
008374     GO TO 9200-XCTL.
008375
008376 8800-UNAUTHORIZED-ACCESS.
008377     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
008378     GO TO 8990-SEND-TEXT.
008379
008380 8810-PF23-ENTERED.
008381     MOVE EIBAID                 TO PI-ENTRY-CD-1.
008382     MOVE XCTL-EL005             TO CALL-PGM.
008383     GO TO 9200-XCTL.
008384
008385 8820-XCTL-ERROR.
008386     
      * EXEC CICS HANDLE CONDITION
008387*        PGMIDERR (8990-SEND-TEXT)
008388*    END-EXEC.
      *    MOVE '"$L                   ! K #00015129' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'4B20233030303135313239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008389
008390     MOVE SPACE                 TO PI-ENTRY-CD-1.
008391     MOVE CALL-PGM              TO PI-CALLING-PROGRAM  LOGOFF-PGM
008392     MOVE PGMIDERR-MSG          TO LOGOFF-FILL.
008393     MOVE XCTL-EL005            TO CALL-PGM.
008394     GO TO 9200-XCTL.
008395
008396 8990-SEND-TEXT.
008397     
      * EXEC CICS SEND TEXT
008398*         FROM      (LOGOFF-TEXT)
008399*         LENGTH    (LOGOFF-LENGTH)
008400*         ERASE
008401*         FREEKB
008402*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00015140' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303135313430' TO DFHEIV0
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
           
008403
008404     GO TO 9100-RETURN-CICS.
008405
008406     EJECT
008407 9000-RETURN-TRANS.
008408     
      * EXEC CICS RETURN
008409*         TRANSID   (TRANS-ID)
008410*         COMMAREA  (PROGRAM-INTERFACE-BLOCK)
008411*         LENGTH    (PI-COMM-LENGTH)
008412*    END-EXEC.
      *    MOVE '.(CT                  ''   #00015151' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303135313531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008413
008414     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL131' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
008415
008416 9100-RETURN-CICS.
008417     
      * EXEC CICS RETURN
008418*    END-EXEC.
      *    MOVE '.(                    ''   #00015160' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303135313630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008419
008420     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL131' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
008421
008422 9200-XCTL.
008423     
      * EXEC CICS XCTL
008424*         PROGRAM    (CALL-PGM)
008425*         COMMAREA   (PROGRAM-INTERFACE-BLOCK)
008426*         LENGTH     (PI-COMM-LENGTH)
008427*    END-EXEC.
      *    MOVE '.$C                   %   #00015166' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303135313636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008428
008429 9700-LINK-RTRM-FILE-ID.
008430     
      * EXEC CICS LINK
008431*        PROGRAM  (RTRM-FILE-ID)
008432*        COMMAREA (CALCULATION-PASS-AREA)
008433*        LENGTH   (CP-COMM-LENGTH)
008434*    END-EXEC.
      *    MOVE '."C                   (   #00015173' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135313733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RTRM-FILE-ID, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008435
008436 9700-EXIT.
008437      EXIT.
008438
008439 9800-CONVERT-DATE.
008440     MOVE SPACES TO DC-ERROR-CODE.
008441     
      * EXEC CICS LINK
008442*         PROGRAM    (DATE-CONV)
008443*         COMMAREA   (DATE-CONVERSION-DATA)
008444*         LENGTH     (DC-COMM-LENGTH)
008445*    END-EXEC.
      *    MOVE '."C                   (   #00015184' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135313834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DATE-CONV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008446
008447 9800-EXIT.
008448     EXIT.
008449
008450 9900-ERROR-FORMAT.
008451     
      * EXEC CICS LINK
008452*         PROGRAM    (XCTL-EL001)
008453*         COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
008454*         LENGTH     (EMI-COMM-LENGTH)
008455*    END-EXEC.
      *    MOVE '."C                   (   #00015194' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135313934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 XCTL-EL001, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008456
008457 9900-EXIT.
008458     EXIT.
008459
008460 9990-ABEND.
008461     MOVE DFHEIBLK               TO EMI-LINE1.
008462     
      * EXEC CICS LINK
008463*         PROGRAM   (XCTL-EL004)
008464*         COMMAREA  (EMI-LINE1)
008465*         LENGTH    (72)
008466*     END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00015205' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135323035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 XCTL-EL004, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008467
008468     MOVE -1                     TO  MAINTL
008469
008470     GO TO 8110-SEND-DATA.
008471
008472 9995-SECURITY-VIOLATION.
008473*           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00015234' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135323334' TO DFHEIV0
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
008474

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL131' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8820-XCTL-ERROR,
                     8150-ENTERED-CLAIM-NOTFOUND,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0130-TS-ERROR,
                     0130-TS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0600-CONTINUE-NAME-UPDATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1004-BENE-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1052-DONE,
                     1052-DONE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1120-CNTL-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1150-NOTOPEN-ERROR,
                     1150-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1490-STATE-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 1500-CARR-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 1620-ACCT-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 2020-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 2030-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 2040-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 2050-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 2060-CERT-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 2060-DUPKEY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 2070-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 2070-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 2080-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 2400-TRLR-NOTFND,
                     2410-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 2425-NINETY-NOTFND,
                     2425-NINETY-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 2430-EXIT,
                     2440-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 2440-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 2460-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 2475-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 2620-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 2650-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 2800-EXIT,
                     2800-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 3010-DEL-ACTQ,
                     3040-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 31
               GO TO 3010-NO-ACTIVITY,
                     3050-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 32
               GO TO 3010-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 33
               GO TO 3020-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 34
               GO TO 3060-EXIT,
                     3060-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 35
               GO TO 5000-TRLR-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 36
               GO TO 5000-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 37
               GO TO 1052-DONE,
                     1052-DONE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 38
               GO TO 7630-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 39
               GO TO 7799-EXIT,
                     7799-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 40
               GO TO 7799-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 41
               GO TO 7800-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 42
               GO TO 8000-NOTE-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 43
               GO TO 8990-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL131' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
